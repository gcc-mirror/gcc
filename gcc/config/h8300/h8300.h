/* Definitions of target machine for GNU compiler.
   Renesas H8/300 (generic)
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1996, 1997, 1998, 1999,
   2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
   Contributed by Steve Chamberlain (sac@cygnus.com),
   Jim Wilson (wilson@cygnus.com), and Doug Evans (dje@cygnus.com).

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef GCC_H8300_H
#define GCC_H8300_H

/* Which CPU to compile for.
   We use int for CPU_TYPE to avoid lots of casts.  */
#if 0 /* defined in insn-attr.h, here for documentation */
enum attr_cpu { CPU_H8300, CPU_H8300H };
#endif
extern int cpu_type;

/* Various globals defined in h8300.c.  */

extern const char *h8_push_op, *h8_pop_op, *h8_mov_op;
extern const char * const *h8_reg_names;

/* Target CPU builtins.  */
#define TARGET_CPU_CPP_BUILTINS()			\
  do							\
    {							\
      if (TARGET_H8300H)				\
	{						\
	  builtin_define ("__H8300H__");		\
	  builtin_assert ("cpu=h8300h");		\
	  builtin_assert ("machine=h8300h");		\
	  if (TARGET_NORMAL_MODE)			\
	    {						\
	      builtin_define ("__NORMAL_MODE__");	\
	    }						\
	}						\
      else if (TARGET_H8300S)				\
	{						\
	  builtin_define ("__H8300S__");		\
	  builtin_assert ("cpu=h8300s");		\
	  builtin_assert ("machine=h8300s");		\
	  if (TARGET_NORMAL_MODE)			\
	    {						\
	      builtin_define ("__NORMAL_MODE__");	\
	    }						\
	}						\
      else						\
	{						\
	  builtin_define ("__H8300__");			\
	  builtin_assert ("cpu=h8300");			\
	  builtin_assert ("machine=h8300");		\
	}						\
    }							\
  while (0)

#define LINK_SPEC "%{mh:%{mn:-m h8300hn}} %{mh:%{!mn:-m h8300h}} %{ms:%{mn:-m h8300sn}} %{ms:%{!mn:-m h8300s}}"

#define LIB_SPEC "%{mrelax:-relax} %{g:-lg} %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p}"

#define OPTIMIZATION_OPTIONS(LEVEL, SIZE)				 \
  do									 \
    {									 \
      /* Basic block reordering is only beneficial on targets with cache \
	 and/or variable-cycle branches where (cycle count taken !=	 \
	 cycle count not taken).  */					 \
      flag_reorder_blocks = 0;						 \
    }									 \
  while (0)

/* Print subsidiary information on the compiler version in use.  */

#define TARGET_VERSION fprintf (stderr, " (Renesas H8/300)");

/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;

/* Masks for the -m switches.  */
#define MASK_H8300S		0x00000001
#define MASK_MAC		0x00000002
#define MASK_INT32		0x00000008
#define MASK_ADDRESSES		0x00000040
#define MASK_QUICKCALL		0x00000080
#define MASK_SLOWBYTE		0x00000100
#define MASK_NORMAL_MODE 	0x00000200
#define MASK_RELAX		0x00000400
#define MASK_H8300H		0x00001000
#define MASK_ALIGN_300		0x00002000

/* Macros used in the machine description to test the flags.  */

/* Make int's 32 bits.  */
#define TARGET_INT32 (target_flags & MASK_INT32)

/* Dump recorded insn lengths into the output file.  This helps debug the
   md file.  */
#define TARGET_ADDRESSES (target_flags & MASK_ADDRESSES)

/* Pass the first few arguments in registers.  */
#define TARGET_QUICKCALL (target_flags & MASK_QUICKCALL)

/* Pretend byte accesses are slow.  */
#define TARGET_SLOWBYTE (target_flags & MASK_SLOWBYTE)

/* Select between the H8/300 and H8/300H CPUs.  */
#define TARGET_H8300	(! TARGET_H8300H && ! TARGET_H8300S)
#define TARGET_H8300H	(target_flags & MASK_H8300H)
#define TARGET_H8300S	(target_flags & MASK_H8300S)
#define TARGET_NORMAL_MODE (target_flags & MASK_NORMAL_MODE)

/* mac register and relevant instructions are available.  */
#define TARGET_MAC    (target_flags & MASK_MAC)

/* Align all values on the H8/300H the same way as the H8/300.  Specifically,
   32 bit and larger values are aligned on 16 bit boundaries.
   This is all the hardware requires, but the default is 32 bits for the H8/300H.
   ??? Now watch someone add hardware floating point requiring 32 bit
   alignment.  */
#define TARGET_ALIGN_300 (target_flags & MASK_ALIGN_300)

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES							    \
{ {"s",			 MASK_H8300S, N_("Generate H8S code")},		    \
  {"no-s",		-MASK_H8300S, N_("Do not generate H8S code")},	    \
  {"s2600",		 MASK_MAC, N_("Generate H8S/2600 code")},	    \
  {"no-s2600",		-MASK_MAC, N_("Do not generate H8S/2600 code")},    \
  {"int32",		 MASK_INT32, N_("Make integers 32 bits wide")},	    \
  {"addresses",		 MASK_ADDRESSES, NULL},				    \
  {"quickcall",		 MASK_QUICKCALL,				    \
   N_("Use registers for argument passing")},				    \
  {"no-quickcall",	-MASK_QUICKCALL,				    \
   N_("Do not use registers for argument passing")},			    \
  {"slowbyte",		 MASK_SLOWBYTE,					    \
   N_("Consider access to byte sized memory slow")},			    \
  {"relax",		 MASK_RELAX, N_("Enable linker relaxing")},	    \
  {"h",			 MASK_H8300H, N_("Generate H8/300H code")},	    \
  {"n",			 MASK_NORMAL_MODE, N_("Enable the normal mode")},   \
  {"no-h",		-MASK_H8300H, N_("Do not generate H8/300H code")},  \
  {"align-300",		 MASK_ALIGN_300, N_("Use H8/300 alignment rules")}, \
  { "",			 TARGET_DEFAULT, NULL}}

#ifdef IN_LIBGCC2
#undef TARGET_H8300H
#undef TARGET_H8300S
#undef TARGET_NORMAL_MODE
/* If compiling libgcc2, make these compile time constants based on what
   flags are we actually compiling with.  */
#ifdef __H8300H__
#define TARGET_H8300H	1
#else
#define TARGET_H8300H	0
#endif
#ifdef __H8300S__
#define TARGET_H8300S	1
#else
#define TARGET_H8300S	0
#endif
#ifdef __NORMAL_MODE__
#define TARGET_NORMAL_MODE 1
#else
#define TARGET_NORMAL_MODE 0
#endif
#endif /* !IN_LIBGCC2 */

/* Do things that must be done once at start up.  */

#define OVERRIDE_OPTIONS			\
  do						\
    {						\
      h8300_init_once ();			\
    }						\
  while (0)

/* Default target_flags if no switches specified.  */

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_QUICKCALL)
#endif

/* Show we can debug even without a frame pointer.  */
/* #define CAN_DEBUG_WITHOUT_FP */

/* Define this if addresses of constant functions
   shouldn't be put through pseudo regs where they can be cse'd.
   Desirable on machines where ordinary constants are expensive
   but a CALL with constant address is cheap.

   Calls through a register are cheaper than calls to named
   functions; however, the register pressure this causes makes
   CSEing of function addresses generally a lose.  */
#define NO_FUNCTION_CSE

/* Target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.
   This is not true on the H8/300.  */
#define BITS_BIG_ENDIAN 0

/* Define this if most significant byte of a word is the lowest numbered.  */
/* That is true on the H8/300.  */
#define BYTES_BIG_ENDIAN 1

/* Define this if most significant word of a multiword number is lowest
   numbered.  */
#define WORDS_BIG_ENDIAN 1

#define MAX_BITS_PER_WORD	32

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD		(TARGET_H8300H || TARGET_H8300S ? 4 : 2)
#define MIN_UNITS_PER_WORD	2

#define SHORT_TYPE_SIZE	16
#define INT_TYPE_SIZE		(TARGET_INT32 ? 32 : 16)
#define LONG_TYPE_SIZE		32
#define LONG_LONG_TYPE_SIZE	64
#define FLOAT_TYPE_SIZE	32
#define DOUBLE_TYPE_SIZE	32
#define LONG_DOUBLE_TYPE_SIZE	DOUBLE_TYPE_SIZE

#define MAX_FIXED_MODE_SIZE	32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY (TARGET_H8300H || TARGET_H8300S ? 32 : 16)

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 16

/* Alignment of field after `int : 0' in a structure.  */
/* One can argue this should be 32 for -mint32, but since 32 bit ints only
   need 16 bit alignment, this is left as is so that -mint32 doesn't change
   structure layouts.  */
#define EMPTY_FIELD_BOUNDARY 16

/* No data type wants to be aligned rounder than this.
   32 bit values are aligned as such on the H8/300H and H8S for speed.  */
#define BIGGEST_ALIGNMENT \
(((TARGET_H8300H || TARGET_H8300S) && ! TARGET_ALIGN_300) ? 32 : 16)

/* The stack goes in 16/32 bit lumps.  */
#define STACK_BOUNDARY (TARGET_H8300 ? 16 : 32)

/* Define this if move instructions will actually fail to work
   when given unaligned data.  */
/* On the H8/300, longs can be aligned on halfword boundaries, but not
   byte boundaries.  */
#define STRICT_ALIGNMENT 1

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.

   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   Reg 9 does not correspond to any hardware register, but instead
   appears in the RTL as an argument pointer prior to reload, and is
   eliminated during reloading in favor of either the stack or frame
   pointer.  */

#define FIRST_PSEUDO_REGISTER 11

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.  */

#define FIXED_REGISTERS \
  { 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you
   like.

   H8 destroys r0,r1,r2,r3.  */

#define CALL_USED_REGISTERS \
  { 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1 }

#define REG_ALLOC_ORDER \
  { 2, 3, 0, 1, 4, 5, 6, 8, 7, 9, 10}

#define CONDITIONAL_REGISTER_USAGE			\
{							\
  if (!TARGET_MAC)					\
    fixed_regs[MAC_REG] = call_used_regs[MAC_REG] = 1;	\
}

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.

   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   We pretend the MAC register is 32bits -- we don't have any data
   types on the H8 series to handle more than 32bits.  */

#define HARD_REGNO_NREGS(REGNO, MODE)   \
   ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Value is 1 if hard register REGNO can hold a value of machine-mode
   MODE.

   H8/300: If an even reg, then anything goes. Otherwise the mode must be QI
           or HI.
   H8/300H: Anything goes.  */

#define HARD_REGNO_MODE_OK(REGNO, MODE)					\
  (TARGET_H8300								\
   ? ((((REGNO) & 1) == 0) || ((MODE) == HImode) || ((MODE) == QImode))	\
   : (REGNO) == MAC_REG ? (MODE) == SImode : 1)

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2)					  \
  ((MODE1) == (MODE2)							  \
   || (((MODE1) == QImode || (MODE1) == HImode				  \
	|| ((TARGET_H8300H || TARGET_H8300S) && (MODE1) == SImode))	  \
       &&  ((MODE2) == QImode || (MODE2) == HImode			  \
	    || ((TARGET_H8300H || TARGET_H8300S) && (MODE2) == SImode))))

/* A C expression that is nonzero if hard register NEW_REG can be
   considered for use as a rename register for OLD_REG register */

#define HARD_REGNO_RENAME_OK(OLD_REG, NEW_REG)		\
   h8300_hard_regno_rename_ok (OLD_REG, NEW_REG)

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* H8/300 pc is not overloaded on a register.  */

/*#define PC_REGNUM 15*/

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM SP_REG

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM FP_REG

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
#define FRAME_POINTER_REQUIRED 0

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM AP_REG

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM SC_REG

/* Fake register that holds the address on the stack of the
   current function's return address.  */
#define RETURN_ADDRESS_POINTER_REGNUM RAP_REG

/* A C expression whose value is RTL representing the value of the return
   address for the frame COUNT steps up from the current frame.
   FRAMEADDR is already the frame pointer of the COUNT frame, assuming
   a stack layout with the frame pointer as the first saved register.  */
#define RETURN_ADDR_RTX(COUNT, FRAME) h8300_return_addr_rtx ((COUNT), (FRAME))

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

enum reg_class {
  NO_REGS, GENERAL_REGS, MAC_REGS, ALL_REGS, LIM_REG_CLASSES
};

#define N_REG_CLASSES ((int) LIM_REG_CLASSES)

/* Give names of register classes as strings for dump file.  */

#define REG_CLASS_NAMES \
{ "NO_REGS", "GENERAL_REGS", "MAC_REGS", "ALL_REGS", "LIM_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS			\
{      {0},		/* No regs      */	\
   {0x6ff},		/* GENERAL_REGS */	\
   {0x100},		/* MAC_REGS */	\
   {0x7ff},		/* ALL_REGS	*/	\
}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO) (REGNO != MAC_REG ? GENERAL_REGS : MAC_REGS)

/* The class value for index registers, and the one for base regs.  */

#define INDEX_REG_CLASS NO_REGS
#define BASE_REG_CLASS  GENERAL_REGS

/* Get reg_class from a letter such as appears in the machine description.

   'a' is the MAC register.  */

#define REG_CLASS_FROM_LETTER(C) ((C) == 'a' ? MAC_REGS : NO_REGS)

/* The letters I, J, K, L, M, N, O, P in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.  */

#define CONST_OK_FOR_I(VALUE) ((VALUE) == 0)
#define CONST_OK_FOR_J(VALUE) (((VALUE) & 0xff) == 0)
#define CONST_OK_FOR_L(VALUE)				\
  (TARGET_H8300H || TARGET_H8300S			\
   ? (VALUE) == 1 || (VALUE) == 2 || (VALUE) == 4	\
   : (VALUE) == 1 || (VALUE) == 2)
#define CONST_OK_FOR_M(VALUE)				\
  ((VALUE) == 1 || (VALUE) == 2)
#define CONST_OK_FOR_N(VALUE)				\
  (TARGET_H8300H || TARGET_H8300S			\
   ? (VALUE) == -1 || (VALUE) == -2 || (VALUE) == -4	\
   : (VALUE) == -1 || (VALUE) == -2)
#define CONST_OK_FOR_O(VALUE)				\
  ((VALUE) == -1 || (VALUE) == -2)

#define CONST_OK_FOR_LETTER_P(VALUE, C)		\
  ((C) == 'I' ? CONST_OK_FOR_I (VALUE) :	\
   (C) == 'J' ? CONST_OK_FOR_J (VALUE) :	\
   (C) == 'L' ? CONST_OK_FOR_L (VALUE) :	\
   (C) == 'M' ? CONST_OK_FOR_M (VALUE) :	\
   (C) == 'N' ? CONST_OK_FOR_N (VALUE) :	\
   (C) == 'O' ? CONST_OK_FOR_O (VALUE) :	\
   0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.

  `G' is a floating-point zero.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)	\
  ((C) == 'G' ? (VALUE) == CONST0_RTX (SFmode)	\
   : 0)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */

#define PREFERRED_RELOAD_CLASS(X, CLASS)  (CLASS)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */

/* On the H8, this is the size of MODE in words.  */

#define CLASS_MAX_NREGS(CLASS, MODE)	\
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Any SI register-to-register move may need to be reloaded,
   so define REGISTER_MOVE_COST to be > 2 so that reload never
   shortcuts.  */

#define REGISTER_MOVE_COST(MODE, CLASS1, CLASS2)  \
  (CLASS1 == MAC_REGS || CLASS2 == MAC_REGS ? 6 : 3)

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
   the mov.b rx,@-sp and turn it into a mov.w rx,@-sp on output.

   On the H8/300H, we simplify TARGET_QUICKCALL by setting this to 4
   and doing a similar thing.  */

#define PUSH_ROUNDING(BYTES) \
  (((BYTES) + PARM_BOUNDARY / 8 - 1) & -PARM_BOUNDARY / 8)

/* Offset of first parameter from the argument pointer register value.  */
/* Is equal to the size of the saved fp + pc, even if an fp isn't
   saved since the value is used before we know.  */

#define FIRST_PARM_OFFSET(FNDECL) 0

/* Value is the number of bytes of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.

   On the H8 the return does not pop anything.  */

#define RETURN_POPS_ARGS(FUNDECL, FUNTYPE, SIZE) 0

/* Definitions for register eliminations.

   This is an array of structures.  Each structure initializes one pair
   of eliminable registers.  The "from" register number is given first,
   followed by "to".  Eliminations of the same "from" register are listed
   in order of preference.

   We have three registers that can be eliminated on the h8300.
   First, the frame pointer register can often be eliminated in favor
   of the stack pointer register.  Secondly, the argument pointer
   register and the return address pointer register are always
   eliminated; they are replaced with either the stack or frame
   pointer.  */

#define ELIMINABLE_REGS					\
{{ ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},		\
 { ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM},		\
 { RETURN_ADDRESS_POINTER_REGNUM, STACK_POINTER_REGNUM},\
 { RETURN_ADDRESS_POINTER_REGNUM, FRAME_POINTER_REGNUM},\
 { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}}

/* Given FROM and TO register numbers, say whether this elimination is allowed.
   Frame pointer elimination is automatically handled.

   For the h8300, if frame pointer elimination is being done, we would like to
   convert ap and rp into sp, not fp.

   All other eliminations are valid.  */

#define CAN_ELIMINATE(FROM, TO)					\
  ((TO) == STACK_POINTER_REGNUM ? ! frame_pointer_needed : 1)

/* Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)		\
  ((OFFSET) = h8300_initial_elimination_offset ((FROM), (TO)))

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.

   On the H8 the return value is in R0/R1.  */

#define FUNCTION_VALUE(VALTYPE, FUNC) \
  gen_rtx_REG (TYPE_MODE (VALTYPE), R0_REG)

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

/* On the H8 the return value is in R0/R1.  */

#define LIBCALL_VALUE(MODE) \
  gen_rtx_REG (MODE, R0_REG)

/* 1 if N is a possible register number for a function value.
   On the H8, R0 is the only register thus used.  */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == R0_REG)

/* Define this if PCC uses the nonreentrant convention for returning
   structure and union values.  */

/*#define PCC_STATIC_STRUCT_RETURN*/

/* 1 if N is a possible register number for function argument passing.
   On the H8, no registers are used in this way.  */

#define FUNCTION_ARG_REGNO_P(N) (TARGET_QUICKCALL ? N < 3 : 0)

/* When defined, the compiler allows registers explicitly used in the
   rtl to be used as spill registers but prevents the compiler from
   extending the lifetime of these registers.  */

#define SMALL_REGISTER_CLASSES 1

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On the H8/300, this is a two item struct, the first is the number
   of bytes scanned so far and the second is the rtx of the called
   library function if any.  */

#define CUMULATIVE_ARGS struct cum_arg
struct cum_arg
{
  int nbytes;
  struct rtx_def *libcall;
};

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On the H8/300, the offset starts at 0.  */

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
 ((CUM).nbytes = 0, (CUM).libcall = LIBNAME)

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)			\
 ((CUM).nbytes += ((MODE) != BLKmode					\
  ? (GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) & -UNITS_PER_WORD	\
  : (int_size_in_bytes (TYPE) + UNITS_PER_WORD - 1) & -UNITS_PER_WORD))

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

/* On the H8/300 all normal args are pushed, unless -mquickcall in which
   case the first 3 arguments are passed in registers.
   See function `function_arg'.  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
  function_arg (&CUM, MODE, TYPE, NAMED)

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)  \
  fprintf (FILE, "\t%s\t#LP%d,%s\n\tjsr @mcount\n", \
	   h8_mov_op, (LABELNO), h8_reg_names[0]);

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 0

/* We emit the entire trampoline with INITIALIZE_TRAMPOLINE.
   Depending on the pointer size, we use a different trampoline.

   Pmode == HImode
	      vvvv context
   1 0000 7903xxxx		mov.w	#0x1234,r3
   2 0004 5A00xxxx		jmp	@0x1234
	      ^^^^ function

   Pmode == SImode
	      vvvvvvvv context
   2 0000 7A03xxxxxxxx		mov.l	#0x12345678,er3
   3 0006 5Axxxxxx		jmp	@0x123456
	    ^^^^^^ function
*/

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE ((Pmode == HImode) ? 8 : 12)

/* Emit RTL insns to build a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)			    \
  do									    \
    {									    \
      if (Pmode == HImode)						    \
	{								    \
	  emit_move_insn (gen_rtx_MEM (HImode, (TRAMP)), GEN_INT (0x7903)); \
	  emit_move_insn (gen_rtx_MEM (Pmode, plus_constant ((TRAMP), 2)),  \
			  (CXT));					    \
	  emit_move_insn (gen_rtx_MEM (Pmode, plus_constant ((TRAMP), 4)),  \
			  GEN_INT (0x5a00));				    \
	  emit_move_insn (gen_rtx_MEM (Pmode, plus_constant ((TRAMP), 6)),  \
			  (FNADDR));					    \
	}								    \
      else								    \
	{								    \
	  rtx tem = gen_reg_rtx (Pmode);				    \
									    \
	  emit_move_insn (gen_rtx_MEM (HImode, (TRAMP)), GEN_INT (0x7a03)); \
	  emit_move_insn (gen_rtx_MEM (Pmode, plus_constant ((TRAMP), 2)),  \
			  (CXT));					    \
	  emit_move_insn (tem, (FNADDR));				    \
	  emit_insn (gen_andsi3 (tem, tem, GEN_INT (0x00ffffff)));	    \
	  emit_insn (gen_iorsi3 (tem, tem, GEN_INT (0x5a000000)));	    \
	  emit_move_insn (gen_rtx_MEM (Pmode, plus_constant ((TRAMP), 6)),  \
			  tem);						    \
	}								    \
    }									    \
  while (0)

/* Addressing modes, and classification of registers for them.  */

#define HAVE_POST_INCREMENT 1
#define HAVE_PRE_DECREMENT 1

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */

#define REGNO_OK_FOR_INDEX_P(regno) 0

#define REGNO_OK_FOR_BASE_P(regno)				\
  (((regno) < FIRST_PSEUDO_REGISTER && regno != MAC_REG)	\
   || reg_renumber[regno] >= 0)

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 1

/* 1 if X is an rtx for a constant that is a valid address.  */

#define CONSTANT_ADDRESS_P(X)					\
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF	\
   || (GET_CODE (X) == CONST_INT				\
       /* We handle signed and unsigned offsets here.  */	\
       && INTVAL (X) > (TARGET_H8300 ? -0x10000 : -0x1000000)	\
       && INTVAL (X) < (TARGET_H8300 ? 0x10000 : 0x1000000))	\
   || (GET_CODE (X) == HIGH || GET_CODE (X) == CONST))

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
#define REG_OK_FOR_INDEX_P(X) 0
/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
/* Don't use REGNO_OK_FOR_BASE_P here because it uses reg_renumber.  */
#define REG_OK_FOR_BASE_P(X) \
	(REGNO (X) >= FIRST_PSEUDO_REGISTER || REGNO (X) != 8)
#define REG_OK_FOR_INDEX_P_STRICT(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
#define REG_OK_FOR_BASE_P_STRICT(X) REGNO_OK_FOR_BASE_P (REGNO (X))
#define STRICT 0

#else

/* Nonzero if X is a hard reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
/* Nonzero if X is a hard reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))
#define STRICT 1

#endif

/* Extra constraints.  */

#define OK_FOR_R(OP)					\
  (GET_CODE (OP) == CONST_INT				\
   ? !h8300_shift_needs_scratch_p (INTVAL (OP), QImode)	\
   : 0)

#define OK_FOR_S(OP)					\
  (GET_CODE (OP) == CONST_INT				\
   ? !h8300_shift_needs_scratch_p (INTVAL (OP), HImode)	\
   : 0)

#define OK_FOR_T(OP)					\
  (GET_CODE (OP) == CONST_INT				\
   ? !h8300_shift_needs_scratch_p (INTVAL (OP), SImode)	\
   : 0)

/* 'U' if valid for a bset destination;
   i.e. a register, register indirect, or the eightbit memory region
   (a SYMBOL_REF with an SYMBOL_REF_FLAG set).

   On the H8S 'U' can also be a 16bit or 32bit absolute.  */
#define OK_FOR_U(OP)							\
  ((GET_CODE (OP) == REG && REG_OK_FOR_BASE_P (OP))			\
   || (GET_CODE (OP) == MEM && GET_CODE (XEXP (OP, 0)) == REG		\
       && REG_OK_FOR_BASE_P (XEXP (OP, 0)))				\
   || (GET_CODE (OP) == MEM && GET_CODE (XEXP (OP, 0)) == SYMBOL_REF	\
       && TARGET_H8300S)						\
   || (GET_CODE (OP) == MEM && GET_CODE (XEXP (OP, 0)) == CONST		\
       && GET_CODE (XEXP (XEXP (OP, 0), 0)) == PLUS			\
       && GET_CODE (XEXP (XEXP (XEXP (OP, 0), 0), 0)) == SYMBOL_REF	\
       && GET_CODE (XEXP (XEXP (XEXP (OP, 0), 0), 1)) == CONST_INT	\
       && (TARGET_H8300S						\
	   || SYMBOL_REF_FLAG (XEXP (XEXP (XEXP (OP, 0), 0), 0))))	\
   || (GET_CODE (OP) == MEM						\
       && h8300_eightbit_constant_address_p (XEXP (OP, 0)))		\
   || (GET_CODE (OP) == MEM && TARGET_H8300S				\
       && GET_CODE (XEXP (OP, 0)) == CONST_INT))

#define EXTRA_CONSTRAINT(OP, C)			\
  ((C) == 'R' ? OK_FOR_R (OP) :			\
   (C) == 'S' ? OK_FOR_S (OP) :			\
   (C) == 'T' ? OK_FOR_T (OP) :			\
   (C) == 'U' ? OK_FOR_U (OP) :			\
   0)

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

#define RTX_OK_FOR_BASE_P(X)				\
  ((REG_P (X) && REG_OK_FOR_BASE_P (X))			\
   || (GET_CODE (X) == SUBREG && REG_P (SUBREG_REG (X))	\
       && REG_OK_FOR_BASE_P (SUBREG_REG (X))))

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)		\
  if (RTX_OK_FOR_BASE_P (X)) goto ADDR;			\
  if (CONSTANT_ADDRESS_P (X)) goto ADDR;		\
  if (GET_CODE (X) == PLUS				\
      && CONSTANT_ADDRESS_P (XEXP (X, 1))		\
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

#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN)  {}

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.

   On the H8/300, the predecrement and postincrement address depend thus
   (the amount of decrement or increment being the length of the operand).  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR, LABEL) \
  if (GET_CODE (ADDR) == POST_INC || GET_CODE (ADDR) == PRE_DEC) goto LABEL;

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE Pmode

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses.  */
/*#define CASE_VECTOR_PC_RELATIVE 1 */

/* Define this as 1 if `char' should by default be signed; else as 0.

   On the H8/300, sign extension is expensive, so we'll say that chars
   are unsigned.  */
#define DEFAULT_SIGNED_CHAR 0

/* This flag, if defined, says the same insns that convert to a signed fixnum
   also convert validly to an unsigned one.  */
#define FIXUNS_TRUNC_LIKE_FIX_TRUNC

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX	(TARGET_H8300H || TARGET_H8300S ? 4 : 2)
#define MAX_MOVE_MAX	4

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
#define Pmode								      \
  ((TARGET_H8300H || TARGET_H8300S) && !TARGET_NORMAL_MODE ? SImode : HImode)

/* ANSI C types.
   We use longs for the H8/300H and the H8S because ints can be 16 or 32.
   GCC requires SIZE_TYPE to be the same size as pointers.  */
#define SIZE_TYPE								\
  (TARGET_H8300 || TARGET_NORMAL_MODE ? "unsigned int" : "long unsigned int")
#define PTRDIFF_TYPE						\
  (TARGET_H8300 || TARGET_NORMAL_MODE ? "int" : "long int")

#define POINTER_SIZE							\
  ((TARGET_H8300H || TARGET_H8300S) && !TARGET_NORMAL_MODE ? 32 : 16)

#define WCHAR_TYPE "short unsigned int"
#define WCHAR_TYPE_SIZE 16
#define MAX_WCHAR_TYPE_SIZE 16

/* A function address in a call instruction
   is a byte address (for indexing purposes)
   so give the MEM rtx a byte's mode.  */
#define FUNCTION_MODE QImode

#define BRANCH_COST 0

/* Tell final.c how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  No extra ones are needed for the h8300.  */

/* Store in cc_status the expressions
   that the condition codes will describe
   after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

#define NOTICE_UPDATE_CC(EXP, INSN) notice_update_cc (EXP, INSN)

/* The add insns don't set overflow in a usable way.  */
#define CC_OVERFLOW_UNUSABLE 01000
/* The mov,and,or,xor insns don't set carry.  That's OK though as the
   Z bit is all we need when doing unsigned comparisons on the result of
   these insns (since they're always with 0).  However, conditions.h has
   CC_NO_OVERFLOW defined for this purpose.  Rename it to something more
   understandable.  */
#define CC_NO_CARRY CC_NO_OVERFLOW

/* Control the assembler format that we output.  */

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON "; #APP\n"

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF "; #NO_APP\n"

#define FILE_ASM_OP "\t.file\n"

/* The assembler op to get a word, 2 bytes for the H8/300, 4 for H8/300H.  */
#define ASM_WORD_OP							\
  (TARGET_H8300 || TARGET_NORMAL_MODE ? "\t.word\t" : "\t.long\t")

#define TEXT_SECTION_ASM_OP "\t.section .text"
#define DATA_SECTION_ASM_OP "\t.section .data"
#define BSS_SECTION_ASM_OP "\t.section .bss"

#undef DO_GLOBAL_CTORS_BODY
#define DO_GLOBAL_CTORS_BODY			\
{						\
  extern func_ptr __ctors[];			\
  extern func_ptr __ctors_end[];		\
  func_ptr *p;					\
  for (p = __ctors_end; p > __ctors; )		\
    {						\
      (*--p)();					\
    }						\
}

#undef DO_GLOBAL_DTORS_BODY
#define DO_GLOBAL_DTORS_BODY			\
{						\
  extern func_ptr __dtors[];			\
  extern func_ptr __dtors_end[];		\
  func_ptr *p;					\
  for (p = __dtors; p < __dtors_end; p++)	\
    {						\
      (*p)();					\
    }						\
}

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES \
{ "r0", "r1", "r2", "r3", "r4", "r5", "r6", "sp", "mac", "ap", "rap" }

#define ADDITIONAL_REGISTER_NAMES \
{ {"er0", 0}, {"er1", 1}, {"er2", 2}, {"er3", 3}, {"er4", 4}, \
  {"er5", 5}, {"er6", 6}, {"er7", 7}, {"r7", 7} }

#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME)

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.global "

#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL) \
   ASM_OUTPUT_LABEL (FILE, NAME)

/* The prefix to add to user-visible assembler symbols.  */

#define USER_LABEL_PREFIX "_"

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.

   N.B.: The h8300.md branch_true and branch_false patterns also know
   how to generate internal labels.  */
#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM)	\
  sprintf (LABEL, "*.%s%lu", PREFIX, (unsigned long)(NUM))

/* This is how to output an insn to push a register on the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_PUSH(FILE, REGNO) \
  fprintf (FILE, "\t%s\t%s\n", h8_push_op, h8_reg_names[REGNO])

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_POP(FILE, REGNO) \
  fprintf (FILE, "\t%s\t%s\n", h8_pop_op, h8_reg_names[REGNO])

/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE) \
  fprintf (FILE, "%s.L%d\n", ASM_WORD_OP, VALUE)

/* This is how to output an element of a case-vector that is relative.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL) \
  fprintf (FILE, "%s.L%d-.L%d\n", ASM_WORD_OP, VALUE, REL)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE, LOG)		\
  if ((LOG) != 0)				\
    fprintf (FILE, "\t.align %d\n", (LOG))

#define ASM_OUTPUT_SKIP(FILE, SIZE) \
  fprintf (FILE, "\t.space %d\n", (int)(SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)	\
( fputs ("\t.comm ", (FILE)),				\
  assemble_name ((FILE), (NAME)),			\
  fprintf ((FILE), ",%lu\n", (unsigned long)(SIZE)))

/* This says how to output the assembler to define a global
   uninitialized but not common symbol.
   Try to use asm_output_bss to implement this macro.  */

#define ASM_OUTPUT_BSS(FILE, DECL, NAME, SIZE, ROUNDED)		\
  asm_output_bss ((FILE), (DECL), (NAME), (SIZE), (ROUNDED))

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)	\
( fputs ("\t.lcomm ", (FILE)),				\
  assemble_name ((FILE), (NAME)),			\
  fprintf ((FILE), ",%d\n", (int)(SIZE)))

#define ASM_PN_FORMAT "%s___%lu"

/* Print an instruction operand X on file FILE.
   Look in h8300.c for details.  */

#define PRINT_OPERAND_PUNCT_VALID_P(CODE) \
  ((CODE) == '#')

#define PRINT_OPERAND(FILE, X, CODE) print_operand (FILE, X, CODE)

/* Print a memory operand whose address is X, on file FILE.
   This uses a function in h8300.c.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR) print_operand_address (FILE, ADDR)

/* H8300 specific pragmas.  */
#define REGISTER_TARGET_PRAGMAS()				\
  do								\
    {								\
      c_register_pragma (0, "saveall", h8300_pr_saveall);	\
      c_register_pragma (0, "interrupt", h8300_pr_interrupt);	\
    }								\
  while (0)

#define FINAL_PRESCAN_INSN(insn, operand, nop)	\
  final_prescan_insn (insn, operand, nop)

/* Define this macro if GCC should generate calls to the System V
   (and ANSI C) library functions `memcpy' and `memset' rather than
   the BSD functions `bcopy' and `bzero'.  */

#define TARGET_MEM_FUNCTIONS

#define MOVE_RATIO 3

/* Define the codes that are matched by predicates in h8300.c.  */

#define PREDICATE_CODES							\
  {"general_operand_src", {CONST_INT, CONST_DOUBLE, CONST, SYMBOL_REF,	\
			   LABEL_REF, SUBREG, REG, MEM, ADDRESSOF}},	\
  {"general_operand_dst", {CONST_INT, CONST_DOUBLE, CONST, SYMBOL_REF,	\
			   LABEL_REF, SUBREG, REG, MEM, ADDRESSOF}},	\
  {"single_one_operand", {CONST_INT}},					\
  {"single_zero_operand", {CONST_INT}},					\
  {"call_insn_operand", {MEM}},						\
  {"small_call_insn_operand", {MEM}},					\
  {"jump_address_operand", {REG, MEM}},					\
  {"two_insn_adds_subs_operand", {CONST_INT}},				\
  {"bit_operand", {REG, SUBREG, MEM}},					\
  {"bit_memory_operand", {MEM}},					\
  {"stack_pointer_operand", {REG}},					\
  {"const_int_gt_2_operand", {CONST_INT}},				\
  {"const_int_ge_8_operand", {CONST_INT}},				\
  {"const_int_qi_operand", {CONST_INT}},				\
  {"const_int_hi_operand", {CONST_INT}},				\
  {"incdec_operand", {CONST_INT}},					\
  {"bit_operator", {XOR, AND, IOR}},					\
  {"nshift_operator", {ASHIFTRT, LSHIFTRT, ASHIFT}},			\
  {"eqne_operator", {EQ, NE}},						\
  {"gtle_operator", {GT, LE, GTU, LEU}},				\
  {"gtuleu_operator", {GTU, LEU}},					\
  {"iorxor_operator", {IOR, XOR}},

#endif /* ! GCC_H8300_H */
