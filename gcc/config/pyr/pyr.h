/* Definitions of target machine parameters for GNU compiler,
   for Pyramid 90x, 9000, and MIServer Series.
   Copyright (C) 1989, 1995, 1996, 1997 Free Software Foundation, Inc.

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

/*
 * If you're going to change this, and you haven't already,
 * you should get and read
 * 	``OSx Operating System Porting Guide'',
 *	  publication number 4100-0066-A
 *	  Revision A
 *	  Pyramid Technology Corporation.
 *
 * or whatever the most recent version is.  In any case, page and
 * section number references given herein refer to this document.
 *
 *  The instruction table for gdb lists the available insns and
 *  the valid addressing modes.
 *
 *  Any other information on the Pyramid architecture is proprietary
 *  and hard to get. (Pyramid cc -S and adb are also useful.)
 *
 */

/*** Run-time compilation parameters selecting different hardware subsets. ***/

/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Dpyr -Dunix -Asystem(unix) -Acpu(pyr) -Amachine(pyr)"

/* Print subsidiary information on the compiler version in use.  */

#define TARGET_VERSION fprintf (stderr, " (pyr)");

extern int target_flags;

/* Nonzero if compiling code that Unix assembler can assemble.  */
#define TARGET_UNIX_ASM (target_flags & 1)

/* Implement stdarg in the same fashion used on all other machines.  */
#define TARGET_GNU_STDARG   (target_flags & 2)

/* Compile using RETD to pop off the args.
   This will not work unless you use prototypes at least
   for all functions that can take varying numbers of args.
   This contravenes the Pyramid calling convention, so we don't
   do it yet.  */

#define TARGET_RETD (target_flags & 4)

/* Macros used in the machine description to test the flags.  */

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.

   -mgnu will be useful if we ever have GAS on a pyramid.  */

#define TARGET_SWITCHES  \
  { {"unix", 1},  		\
    {"gnu", -1},  		\
    {"gnu-stdarg", 2},		\
    {"nognu-stdarg", -2},	\
    {"retd", 4},		\
    {"no-retd", -4},		\
    { "", TARGET_DEFAULT}}

/* Default target_flags if no switches specified.

   (equivalent to "-munix -mindex -mgnu-stdarg")  */

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT (1 + 2)
#endif

/* Make GCC agree with types.h.  */
#ifdef	SIZE_TYPE
#undef	SIZE_TYPE
#endif
#define	SIZE_TYPE	"unsigned int"

/* Assembler does not permit $ in labels */

#define	NO_DOLLAR_IN_LABEL

/* Maybe it doesn't permit dot either.  */
#define NO_DOT_IN_LABEL

/* Never allow $ in identifiers */

#define DOLLARS_IN_IDENTIFIERS 0

/*** Target machine storage layout ***/

/* Define this to non-zero if most significant bit is lowest
   numbered in instructions that operate on numbered bit-fields.
   This is not true on the pyramid.  */
#define BITS_BIG_ENDIAN 0

/* Define this to non-zero if most significant byte of a word is
   the lowest numbered.  */
#define BYTES_BIG_ENDIAN 1

/* Define this to non-zero if most significant word of a multiword
   number is the lowest numbered.  */
#define WORDS_BIG_ENDIAN 1

/* Number of bits in an addressable storage unit */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
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
#define FUNCTION_BOUNDARY 32

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 32

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 32

/* Specified types of bitfields affect alignment of those fields
   and of the structure as a whole.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* Make strings word-aligned so strcpy from constants will be faster. 
   Pyramid documentation says the best alignment is to align
   on the size of a cache line, which is 32 bytes.
   Newer pyrs have single insns that do strcmp() and strcpy(), so this
   may not actually win anything.   */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  (TREE_CODE (EXP) == STRING_CST	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/*** Standard register usage.  ***/

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.  */

/* Nota Bene:
   Pyramids have 64 addressable 32-bit registers, arranged as four
   groups of sixteen registers each. Pyramid names the groups
   global, parameter, local, and temporary.

   The sixteen global registers are fairly conventional; the last
   four are overloaded with a PSW, frame pointer, stack pointer, and pc.
   The non-dedicated global registers used to be reserved for Pyramid
   operating systems, and still have cryptic and undocumented uses for
   certain library calls.  We do not use global registers gr0 through
   gr11.

   The parameter, local, and temporary registers provide register
   windowing.  Each procedure call has its own set of these 48
   registers, which constitute its call frame. (These frames are
   not allocated on the conventional stack, but contiguously
   on a separate stack called the control stack.)
   Register windowing is a facility whereby the temporary registers
   of frame n become the parameter registers of frame n+1, viz.:

                                      0         15 0         15 0         15
                                     +------------+------------+------------+
frame n+1                            |            |            |            |
                                     +------------+------------+------------+
                                        Parameter     Local       Temporary

                                          ^
                                          | These 16 regs are the same.
                                          v

            0         15 0         15 0         15
           +------------+------------+------------+
frame n    |            |            |            |
           +------------+------------+------------+
             Parameter     Local       Temporary

   New frames are automatically allocated on the control stack by the
   call instruction and de-allocated by the return insns "ret" and
   "retd".  The control-stack grows contiguously upward from a
   well-known address in memory; programs are free to allocate
   a variable sized, conventional frame on the data stack, which
   grows downwards in memory from just below the control stack.

   Temporary registers are used for parameter passing, and are not
   preserved across calls.  TR0 through TR11 correspond to
   gcc's ``input'' registers; PR0 through TR11 the ``output''
   registers. The call insn stores the PC and PSW in PR14 and PR15 of
   the frame it creates; the return insns restore these into the PC
   and PSW. The same is true for interrupts; TR14 and TR15 of the
   current frame are reserved and should never be used, since an
   interrupt may occur at any time and clobber them.

   An interesting quirk is the ability to take the address of a
   variable in a windowed register.  This done by adding the memory
   address of the base of the current window frame, to the offset
   within the frame of the desired register.  The resulting address
   can be treated just like any other pointer; if a quantity is stored
   into that address, the appropriate register also changes.
   GCC does not, and according to RMS will not, support this feature,
   even though some programs rely on this (mis)feature.
 */

#define PYR_GREG(n) (n)
#define PYR_PREG(n) (16+(n))
#define PYR_LREG(n) (32+(n))
#define PYR_TREG(n) (48+(n))

/* Define this macro if the target machine has "register windows".  This
   C expression returns the register number as seen by the called function
   corresponding to register number OUT as seen by the calling function.
   Return OUT if register number OUT is not an outbound register.  */

#define INCOMING_REGNO(OUT) \
 (((OUT) < 48 || (OUT) > 63) ? (OUT) : (OUT) - 32)

/* Define this macro if the target machine has "register windows".  This
   C expression returns the register number as seen by the calling function
   corresponding to register number IN as seen by the called function.
   Return IN if register number IN is not an inbound register.  */

#define OUTGOING_REGNO(IN) \
 (((IN) < 15 || (IN) > 31) ? (IN) : (IN) + 32)

#define FIRST_PSEUDO_REGISTER 64

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.

   On the pyramid, these are LOGPSW, SP, and PC.  */

#define FIXED_REGISTERS \
  {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	\
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,	\
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */
#define CALL_USED_REGISTERS \
  {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	\
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,	\
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 	\
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}

/* #define DEFAULT_CALLER_SAVES */

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.
   On the pyramid, all registers are one word long.  */
#define HARD_REGNO_NREGS(REGNO, MODE)   \
 ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   On the pyramid, all registers can hold all modes.  */

/* -->FIXME: this is not the case for 64-bit quantities in tr11/12 through
   --> TR14/15.  This should be fixed,  but to do it correctly, we also
   --> need to fix MODES_TIEABLE_P. Yuk.  We ignore this, since GCC should
   --> do the "right" thing due to FIXED_REGISTERS. */
#define HARD_REGNO_MODE_OK(REGNO, MODE) 1

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2) 1

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* Pyramid pc is overloaded on global register 15.  */
#define PC_REGNUM PYR_GREG(15)

/* Register to use for pushing function arguments.
   --> on Pyramids, the data stack pointer. */
#define STACK_POINTER_REGNUM PYR_GREG(14)

/* Base register for access to local variables of the function.
   Pyramid uses CFP (GR13) as both frame pointer and argument pointer. */
#define FRAME_POINTER_REGNUM 13 /* pyr cpp fails on PYR_GREG(13) */

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.

   Setting this to 1 can't break anything.  Since the Pyramid has
   register windows, I don't know if defining this to be zero can
   win anything.  It could changed later, if it wins. */
#define FRAME_POINTER_REQUIRED 1

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 13 /* PYR_GREG(13) */

/* Register in which static-chain is passed to a function.  */
/* If needed, Pyramid says to use temporary register 12. */
#define STATIC_CHAIN_REGNUM PYR_TREG(12)

/* If register windows are used, STATIC_CHAIN_INCOMING_REGNUM
   is the register number as seen by the called function, while
   STATIC_CHAIN_REGNUM is the register number as seen by the calling
   function. */
#define STATIC_CHAIN_INCOMING_REGNUM PYR_PREG(12)

/* Register in which address to store a structure value
   is passed to a function.
   On a Pyramid, this is temporary register 0 (TR0).   */

#define STRUCT_VALUE_REGNUM PYR_TREG(0)
#define STRUCT_VALUE_INCOMING_REGNUM PYR_PREG(0)

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

/* The pyramid has only one kind of registers, so NO_REGS and ALL_REGS
   are the only classes.  */

enum reg_class { NO_REGS, ALL_REGS, LIM_REG_CLASSES };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Since GENERAL_REGS is the same class as ALL_REGS,
   don't give it a different class number; just make it an alias.  */

#define GENERAL_REGS ALL_REGS

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES \
 {"NO_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS {{0,0}, {0xffffffff,0xffffffff}}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO) ALL_REGS

/* The class value for index registers, and the one for base regs.  */

#define BASE_REG_CLASS ALL_REGS
#define INDEX_REG_CLASS ALL_REGS

/* Get reg_class from a letter such as appears in the machine description.  */

#define REG_CLASS_FROM_LETTER(C) NO_REGS

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */

#define PREFERRED_RELOAD_CLASS(X,CLASS)  (CLASS)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
/* On the pyramid, this is always the size of MODE in words,
   since all registers are the same size.  */
#define CLASS_MAX_NREGS(CLASS, MODE)	\
 ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* The letters I, J, K, L and M in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.

   --> For the Pyramid, 'I' can be used for the 6-bit signed integers
   --> (-32 to 31) allowed as immediate short operands in many
   --> instructions. 'J' cane be used for any value that doesn't fit
   --> in 6 bits.  */

#define CONST_OK_FOR_LETTER_P(VALUE, C)  \
  ((C) == 'I' ? (VALUE) >= -32 && (VALUE) < 32 : \
   (C) == 'J' ? (VALUE) < -32 || (VALUE) >= 32 : \
   (C) == 'K' ? (VALUE) == 0xff || (VALUE) == 0xffff : 0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C) 0


/*** Stack layout; function entry, exit and calling.  ***/

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
/* FIXME: this used to work when defined as 0.  But that makes gnu
   stdargs clobber the first arg.  What gives?? */
#define STARTING_FRAME_OFFSET 0

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET(FNDECL) 0

/* Value is the number of bytes of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.

   The Pyramid OSx Porting Guide says we are never to do this;
   using RETD in this way violates the Pyramid calling convention.
   We may nevertheless provide this as an option.   */

#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE)   \
  ((TARGET_RETD && (!(FUNDECL) || TREE_CODE (FUNDECL) != IDENTIFIER_NODE)	\
    && (TYPE_ARG_TYPES (FUNTYPE) == 0				\
	|| (TREE_VALUE (tree_last (TYPE_ARG_TYPES (FUNTYPE)))	\
	    == void_type_node)))				\
   ? (SIZE) : 0)

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

/* --> Pyramid has register windows.
   --> The caller sees the return value is in TR0(/TR1) regardless of
   --> its type.   */

#define FUNCTION_VALUE(VALTYPE, FUNC)  \
  gen_rtx (REG, TYPE_MODE (VALTYPE), PYR_TREG(0))

/* --> but the callee has to leave it in PR0(/PR1) */

#define FUNCTION_OUTGOING_VALUE(VALTYPE, FUNC)	\
  gen_rtx (REG, TYPE_MODE (VALTYPE), PYR_PREG(0))

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

/* --> On Pyramid the return value is in TR0/TR1 regardless.  */

#define LIBCALL_VALUE(MODE)  gen_rtx (REG, MODE, PYR_TREG(0))

/* Define this if PCC uses the nonreentrant convention for returning
   structure and union values.  */

#define PCC_STATIC_STRUCT_RETURN

/* 1 if N is a possible register number for a function value
   as seen by the caller.

  On the Pyramid, TR0 is the only register thus used.   */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == PYR_TREG(0))

/* 1 if N is a possible register number for function argument passing.
   On the Pyramid, the first twelve temporary registers are available.  */

/* FIXME FIXME FIXME
   it's not clear whether this macro should be defined from the point
   of view of the caller or the callee.  Since it's never actually used
   in GNU CC, the point is somewhat moot :-).

   This definition is consistent with register usage in the md's for
   other register-window architectures (sparc and spur).
 */
#define FUNCTION_ARG_REGNO_P(N) ((PYR_TREG(0) <= (N)) && ((N) <= PYR_TREG(11)))

/*** Parameter passing: FUNCTION_ARG and FUNCTION_INCOMING_ARG ***/

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On Pyramids, each parameter is passed either completely on the stack
   or completely in registers.  No parameter larger than a double may
   be passed in a register.  Also, no struct or union may be passed in
   a register, even if it would fit.

    So parameters are not necessarily passed "consecutively".
    Thus we need a vector data type: one element to record how many
    parameters have been passed in registers and on the stack,
    respectively.

    ((These constraints seem like a gross waste of registers. But if we
    ignore the constraint about structs & unions, we won`t be able to
    freely mix gcc-compiled code and pyr cc-compiled code.  It looks
    like better argument passing conventions, and a machine-dependent
    flag to enable them, might be a win.))   */


#define CUMULATIVE_ARGS int

/* Define the number of registers that can hold parameters.
   This macro is used only in other macro definitions below.   */
#define NPARM_REGS 12

/* Decide whether or not a parameter can be put in a register.
   (We may still have problems with libcalls. GCC doesn't seem
   to know about anything more than the machine mode.  I trust
   structures are never passed to a libcall...

   If compiling with -mgnu-stdarg, this definition should make
   functions using the gcc-supplied stdarg, and calls to such
   functions (declared with an arglist ending in"..."),  work.
   But such fns won't be able to call pyr cc-compiled
   varargs fns (eg, printf(), _doprnt.)

   If compiling with -mnognu-stdarg, this definition should make
   calls to pyr cc-compiled functions work.  Functions using
   the gcc-supplied stdarg will be utterly broken.
   There will be no better solution until RMS can be persuaded that
   one is needed.

   This macro is used only in other macro definitions below.
   (well, it may be used in pyr.c, because the damn pyramid cc
   can't handle the macro definition of PARAM_SAFE_FOR_REG_P !   */


#define INNER_PARAM_SAFE_HELPER(TYPE) \
 ((TARGET_GNU_STDARG ? (! TREE_ADDRESSABLE ((tree)TYPE)): 1)	\
   && (TREE_CODE ((tree)TYPE) != RECORD_TYPE)			\
   && (TREE_CODE ((tree)TYPE) != UNION_TYPE))

#ifdef __GNUC__
#define PARAM_SAFE_HELPER(TYPE) \
  INNER_PARAM_SAFE_HELPER((TYPE))
#else
extern int inner_param_safe_helper();
#define PARAM_SAFE_HELPER(TYPE) \
  inner_param_safe_helper((tree)(TYPE))
#endif

/* Be careful with the expression (long) (TYPE) == 0.
   Writing it in more obvious/correct forms makes the Pyr cc
   dump core!   */
#define PARAM_SAFE_FOR_REG_P(MODE, TYPE, NAMED) \
  (((MODE) != BLKmode)				\
   && ((TARGET_GNU_STDARG) ? (NAMED) : 1)	\
   && ((((long)(TYPE))==0) || PARAM_SAFE_HELPER((TYPE))))

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.   */

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,INDIRECT)	\
  ((CUM) = (FNTYPE && !flag_pcc_struct_return		\
	    && aggregate_value_p (TREE_TYPE (FNTYPE))))

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
    (otherwise it is an extra parameter matching an ellipsis). */

#define FUNCTION_ARG_HELPER(CUM, MODE, TYPE, NAMED) \
(PARAM_SAFE_FOR_REG_P(MODE,TYPE,NAMED)				\
 ? (NPARM_REGS >= ((CUM)					\
		   + ((MODE) == BLKmode				\
		      ? (int_size_in_bytes (TYPE) + 3) / 4	\
		      : (GET_MODE_SIZE (MODE) + 3) / 4))	\
    ? gen_rtx (REG, (MODE), PYR_TREG(CUM))			\
    : 0)							\
 : 0)
#ifdef __GNUC__
#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
	FUNCTION_ARG_HELPER(CUM, MODE, TYPE, NAMED)
#else
/*****************  Avoid bug in Pyramid OSx compiler... ******************/
#define FUNCTION_ARG  (rtx) pyr_function_arg
extern void* pyr_function_arg ();
#endif

/* Define where a function finds its arguments.
   This is different from FUNCTION_ARG because of register windows.  */

#define FUNCTION_INCOMING_ARG(CUM, MODE, TYPE, NAMED) \
(PARAM_SAFE_FOR_REG_P(MODE,TYPE,NAMED)			\
 ? (NPARM_REGS >= ((CUM)				\
	   + ((MODE) == BLKmode				\
	      ? (int_size_in_bytes (TYPE) + 3) / 4	\
	      : (GET_MODE_SIZE (MODE) + 3) / 4))	\
    ? gen_rtx (REG, (MODE), PYR_PREG(CUM))		\
    : 0)						\
 : 0)

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

#define FUNCTION_ARG_ADVANCE(CUM,MODE,TYPE,NAMED)  \
((CUM)	+=  (PARAM_SAFE_FOR_REG_P(MODE,TYPE,NAMED)	\
	     ? ((MODE) != BLKmode			\
		? (GET_MODE_SIZE (MODE) + 3) / 4	\
		: (int_size_in_bytes (TYPE) + 3) / 4)	\
	     : 0))

/* This macro generates the assembly code for function entry.
   FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.
   Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This macro is responsible for
   knowing which registers should not be saved even if used.  */

#if FRAME_POINTER_REQUIRED

/* We always have frame pointers */

/* Don't set up a frame pointer if it's not referenced.  */

#define FUNCTION_PROLOGUE(FILE, SIZE) \
{									\
  int _size = (SIZE) + current_function_pretend_args_size;		\
  if (_size + current_function_args_size != 0				\
      || current_function_calls_alloca)					\
    {									\
      fprintf (FILE, "\tadsf $%d\n", _size);				\
      if (current_function_pretend_args_size > 0)			\
      fprintf (FILE, "\tsubw $%d,cfp\n",				\
	  current_function_pretend_args_size);				\
    }									\
}

#else /* !FRAME_POINTER_REQUIRED */

/* Don't set up a frame pointer if `frame_pointer_needed' tells us
   there is no need.  Also, don't set up a frame pointer if it's not
   referenced.  */

/* The definition used to be broken.  Write a new one.  */

#endif /* !FRAME_POINTER_REQUIRED */

/* the trampoline stuff was taken from convex.h - S.P. */

/* A C statement to output, on the stream FILE, assembler code for a
   block of data that contains the constant parts of a trampoline.  This
   code should not include a label - the label is taken care of
   automatically.
	We use TR12/PR12 for the static chain.
	movew $<STATIC>,pr12	# I2R
	jump $<func>		# S2R
 */
#define TRAMPOLINE_TEMPLATE(FILE) \
{ ASM_OUTPUT_INT (FILE, GEN_INT (0x2100001C));	\
  ASM_OUTPUT_INT (FILE, GEN_INT (0x00000000));	\
  ASM_OUTPUT_INT (FILE, GEN_INT (0x40000000));	\
  ASM_OUTPUT_INT (FILE, GEN_INT (0x00000000)); }

#define TRAMPOLINE_SIZE		16
#define TRAMPOLINE_ALIGNMENT	32

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT) \
{ emit_move_insn (gen_rtx (MEM, Pmode, plus_constant (TRAMP, 4)), CXT);	\
  emit_move_insn (gen_rtx (MEM, Pmode, plus_constant (TRAMP, 12)), FNADDR); \
  emit_call_insn (gen_call (gen_rtx (MEM, QImode,			\
				     gen_rtx (SYMBOL_REF, Pmode,	\
					      "__enable_execute_stack")), \
			    const0_rtx));				\
}

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */
#define FUNCTION_PROFILER(FILE, LABELNO)  \
   fprintf (FILE, "\tmova LP%d,tr0\n\tcall mcount\n", (LABELNO));

/* Output assembler code to FILE to initialize this source file's
   basic block profiling info, if that has not already been done.
   Don't know if this works on Pyrs. */

#if 0 /* don't do basic_block profiling yet */
#define FUNCTION_BLOCK_PROFILER(FILE, LABELNO)  \
  fprintf (FILE, \
           "\tmtstw LPBX0,tr0\n\tbne LPI%d\n\tmova LP%d,TR0\n\tcall __bb_init_func\nLPI%d:\n", \
           LABELNO, LABELNO);

/* Output assembler code to increment the count associated with
   the basic block number BLOCKNO.  Not sure how to do this on pyrs. */
#define BLOCK_PROFILER(FILE, BLOCKNO)  \
    fprintf (FILE, "\taddw", 4 * BLOCKNO)
#endif /* don't do basic_block profiling yet */

/* When returning from a function, the stack pointer does not matter
   (as long as there is a frame pointer).  */

/* This should return non-zero when we really set up a frame pointer.
   Otherwise, GCC is directed to preserve sp by returning zero.  */
extern int current_function_pretend_args_size;
extern int current_function_args_size;
extern int current_function_calls_alloca;
#define EXIT_IGNORE_STACK \
  (get_frame_size () + current_function_pretend_args_size		\
   + current_function_args_size != 0					\
   || current_function_calls_alloca)					\

/* Store in the variable DEPTH the initial difference between the
   frame pointer reg contents and the stack pointer reg contents,
   as of the start of the function body.  This depends on the layout
   of the fixed parts of the stack frame and on how registers are saved.

   On the Pyramid, FRAME_POINTER_REQUIRED is always 1, so the definition
   of this macro doesn't matter.  But it must be defined.  */

#define INITIAL_FRAME_POINTER_OFFSET(DEPTH) (DEPTH) = 0;

/*** Addressing modes, and classification of registers for them.  ***/

/* #define HAVE_POST_INCREMENT 0 */	/* pyramid has none of these */
/* #define HAVE_POST_DECREMENT 0 */

/* #define HAVE_PRE_DECREMENT 0 */
/* #define HAVE_PRE_INCREMENT 0 */

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */

/* All registers except gr0 OK as index or base registers.  */

#define REGNO_OK_FOR_BASE_P(regno) \
((regno) < FIRST_PSEUDO_REGISTER || reg_renumber[regno] >= 0)

#define REGNO_OK_FOR_INDEX_P(regno)  \
((unsigned) (regno) - 1 < FIRST_PSEUDO_REGISTER - 1 \
 || reg_renumber[regno] > 0)

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 2     /* check MAX_REGS_PER_ADDRESS */

/* 1 if X is an rtx for a constant that is a valid address.  */

#define CONSTANT_ADDRESS_P(X)   \
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
   || GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST		\
   || GET_CODE (X) == HIGH)

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
#define REG_OK_FOR_INDEX_P(X) (REGNO (X) > 0)
/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) 1

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
   except for CONSTANT_ADDRESS_P which is actually machine-independent.  */


/* Go to ADDR if X is indexable -- i.e., neither indexed nor offset.  */
#define GO_IF_INDEXABLE_ADDRESS(X, ADDR)  \
{ register rtx xfoob = (X);						\
  if ((CONSTANT_ADDRESS_P (xfoob))					\
      || (GET_CODE (xfoob) == REG && (REG_OK_FOR_BASE_P (xfoob))))	\
	  goto ADDR;							\
 }


/* Go to label ADDR if X is a valid address that doesn't use indexing.
   This is so if X is either a simple address, or the contents of a register
   plus an offset.
   This macro also gets used in output-pyramid.h in the function that
   recognizes non-indexed operands.  */

#define GO_IF_NONINDEXED_ADDRESS(X, ADDR)  \
{									\
  if (GET_CODE (X) == REG)						\
      goto ADDR;							\
  GO_IF_INDEXABLE_ADDRESS (X, ADDR);					\
  if (GET_CODE (X) == PLUS)						\
    { /* Handle offset(reg) represented with offset on left */		\
      if (CONSTANT_ADDRESS_P (XEXP (X, 0)))				\
	{ if (GET_CODE (XEXP (X, 1)) == REG				\
	      && REG_OK_FOR_BASE_P (XEXP (X, 1)))			\
	    goto ADDR;							\
	 }								\
      /* Handle offset(reg) represented with offset on right */		\
      if (CONSTANT_ADDRESS_P (XEXP (X, 1)))				\
	{ if (GET_CODE (XEXP (X, 0)) == REG				\
	      && REG_OK_FOR_BASE_P (XEXP (X, 0)))			\
	    goto ADDR;							\
	 }								\
     }									\
}

/* 1 if PROD is either a reg or a reg times a valid offset multiplier
   (ie, 2, 4, or 8).
   This macro's expansion uses the temporary variables xfoo0 and xfoo1
   that must be declared in the surrounding context.  */
#define INDEX_TERM_P(PROD, MODE)   \
((GET_CODE (PROD) == REG && REG_OK_FOR_BASE_P (PROD))			\
  || (GET_CODE (PROD) == MULT						\
      &&								\
      (xfoo0 = XEXP (PROD, 0), xfoo1 = XEXP (PROD, 1),			\
       ((GET_CODE (xfoo0) == CONST_INT					\
         && (INTVAL (xfoo0) == 1					\
	     || INTVAL (xfoo0) == 2					\
	     || INTVAL (xfoo0) == 4					\
	     || INTVAL (xfoo0) == 8)					\
         && GET_CODE (xfoo1) == REG					\
         && REG_OK_FOR_INDEX_P (xfoo1))					\
        ||								\
        (GET_CODE (xfoo1) == CONST_INT					\
	 && (INTVAL (xfoo1) == 1					\
	     || INTVAL (xfoo1) == 2					\
	     || INTVAL (xfoo1) == 4					\
	     || INTVAL (xfoo1) == 8)					\
        && GET_CODE (xfoo0) == REG					\
        && REG_OK_FOR_INDEX_P (xfoo0))))))


#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)  \
{ register rtx xone, xtwo, xfoo0, xfoo1;				\
  GO_IF_NONINDEXED_ADDRESS (X, ADDR);					\
  if (GET_CODE (X) == PLUS)						\
    {									\
      /* Handle <address>[index] represented with index-sum outermost */\
      xone = XEXP (X, 0);						\
      xtwo = XEXP (X, 1);						\
      if (INDEX_TERM_P (xone, MODE))					\
	{ GO_IF_INDEXABLE_ADDRESS (xtwo, ADDR); }			\
      /* Handle <address>[index] represented with index-sum innermost */\
      if (INDEX_TERM_P (xtwo, MODE))					\
	{ GO_IF_INDEXABLE_ADDRESS (xone, ADDR); }			\
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

   --> FIXME: We haven't yet figured out what optimizations are useful
   --> on Pyramids.   */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)  {}

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.
   There don't seem to be any such modes on pyramids. */
#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)

/*** Miscellaneous Parameters ***/

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE SImode

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses. */
/*#define CASE_VECTOR_PC_RELATIVE 1 */

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.
   It's just a guess. I have no idea of insn cost on pyrs. */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 1

/* This flag, if defined, says the same insns that convert to a signed fixnum
   also convert validly to an unsigned one.  */
/* This is untrue for pyramid.  The cvtdw instruction generates a trap
   for input operands that are out-of-range for a signed int.  */
/* #define FIXUNS_TRUNC_LIKE_FIX_TRUNC */

/* Define this macro if the preprocessor should silently ignore
  '#sccs' directives. */
/* #define SCCS_DIRECTIVE */

/* Define this macro if the preprocessor should silently ignore
  '#ident' directives. */
/* #define IDENT_DIRECTIVE */

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 8

/* Define this if zero-extension is slow (more than one real instruction).  */
/* #define SLOW_ZERO_EXTEND */

/* number of bits in an 'int' on target machine */
#define INT_TYPE_SIZE 32

/* 1 if byte access requires more than one instruction */
#define SLOW_BYTE_ACCESS 0

/* Define this to be nonzero if shift instructions ignore all but the low-order
   few bits. */
#define SHIFT_COUNT_TRUNCATED 1

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* Define this macro if it is as good or better to call a constant
   function address than to call an address kept in a register.  */
/* #define NO_FUNCTION_CSE */

/* When a prototype says `char' or `short', really pass an `int'.  */
#define PROMOTE_PROTOTYPES

/* There are no flag store insns on a pyr. */
/* #define STORE_FLAG_VALUE */

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode SImode

/* A function address in a call instruction
   is a byte address (for indexing purposes)
   so give the MEM rtx a byte's mode.  */
#define FUNCTION_MODE QImode

/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch.  */

#define CONST_COSTS(RTX,CODE,OUTER_CODE) \
  case CONST_INT:						\
    if (CONST_OK_FOR_LETTER_P (INTVAL (RTX),'I')) return 0;	\
  case CONST:							\
  case LABEL_REF:						\
  case SYMBOL_REF:						\
    return 4;							\
  case CONST_DOUBLE:						\
    return 6;

/* A flag which says to swap the operands of certain insns
   when they are output.  */
extern int swap_operands;

/*** Condition Code Information ***/

/* Tell final.c how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  No extra ones are needed for the pyr.  */

/* Store in cc_status the expressions
   that the condition codes will describe
   after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

/* This is a very simple definition of NOTICE_UPDATE_CC.
   Many cases can be optimized, to improve condition code usage.
   Maybe we should handle this entirely in the md, since it complicated
   to describe the way pyr sets cc.  */

#define TRULY_UNSIGNED_COMPARE_P(X) \
  (X == GEU || X == GTU || X == LEU || X == LTU)
#define CC_VALID_FOR_UNSIGNED 2

#define CC_STATUS_MDEP_INIT cc_status.mdep = 0

#define NOTICE_UPDATE_CC(EXP, INSN) \
  notice_update_cc(EXP, INSN)

/*** Output of Assembler Code ***/

/* Output at beginning of assembler file.  */

#define ASM_FILE_START(FILE) \
  fprintf (FILE, ((TARGET_UNIX_ASM)? "" : "#NO_APP\n"));

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON ((TARGET_UNIX_ASM) ? "" : "#APP\n")

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF ((TARGET_UNIX_ASM) ? "" : "#NO_APP\n")

/* Output before read-only data.  */

#define TEXT_SECTION_ASM_OP ".text"

/* Output before writable data.  */

#define DATA_SECTION_ASM_OP ".data"

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES \
{"gr0", "gr1", "gr2", "gr3", "gr4", "gr5", "gr6", "gr7", "gr8", \
 "gr9", "gr10", "gr11", "logpsw", "cfp", "sp", "pc", \
 "pr0", "pr1", "pr2", "pr3", "pr4", "pr5", "pr6", "pr7", \
 "pr8", "pr9", "pr10", "pr11", "pr12", "pr13", "pr14", "pr15", \
 "lr0", "lr1", "lr2", "lr3", "lr4", "lr5", "lr6", "lr7", \
 "lr8", "lr9", "lr10", "lr11", "lr12", "lr13", "lr14", "lr15", \
 "tr0", "tr1", "tr2", "tr3", "tr4", "tr5", "tr6", "tr7", \
 "tr8", "tr9", "tr10", "tr11", "tr12", "tr13", "tr14", "tr15"}

/* How to renumber registers for dbx and gdb.  */

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* Our preference is for dbx rather than sdb.
   Yours may be different. */
#define DBX_DEBUGGING_INFO
/* #define SDB_DEBUGGING_INFO */

/* Don't use the `xsfoo;' construct in DBX output; this system
   doesn't support it.  */

#define DBX_NO_XREFS 1

/* Do not break .stabs pseudos into continuations.  */

#define DBX_CONTIN_LENGTH 0

/* This is the char to use for continuation (in case we need to turn
   continuation back on).  */

#define DBX_CONTIN_CHAR '?'

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#define ASM_OUTPUT_LABEL(FILE,NAME)	\
  do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0)

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(FILE,NAME)	\
  do { fputs (".globl ", FILE); assemble_name (FILE, NAME); fputs ("\n", FILE);} while (0)

/* The prefix to add to user-visible assembler symbols. */

#define USER_LABEL_PREFIX "_"

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, "%s%d:\n", PREFIX, NUM)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*%s%d", PREFIX, NUM)

/* This is how to output an assembler line defining a `double' constant.  */

#define ASM_OUTPUT_DOUBLE(FILE,VALUE)  \
  fprintf (FILE, "\t.double 0d%.20e\n", (VALUE))

/* This is how to output an assembler line defining a `float' constant.  */

#define ASM_OUTPUT_FLOAT(FILE,VALUE)  \
  fprintf (FILE, "\t.float 0f%.20e\n", (VALUE))

/* This is how to output an assembler line defining an `int' constant.  */

#define ASM_OUTPUT_INT(FILE,VALUE)  \
( fprintf (FILE, "\t.word "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* Likewise for `char' and `short' constants.  */

#define ASM_OUTPUT_SHORT(FILE,VALUE)  \
( fprintf (FILE, "\t.half "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

#define ASM_OUTPUT_CHAR(FILE,VALUE)  \
( fprintf (FILE, "\t.byte "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* This is how to output an assembler line for a numeric constant byte.  */

#define ASM_OUTPUT_BYTE(FILE,VALUE)  \
  fprintf (FILE, "\t.byte 0x%x\n", (VALUE))

/* This is how to output an insn to push a register on the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  \
  fprintf (FILE, "\tsubw $4,sp\n\tmovw %s,(sp)\n", reg_names[REGNO])

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_POP(FILE,REGNO)  \
  fprintf (FILE, "\tmovw (sp),%s\n\taddw $4,sp\n", reg_names[REGNO])

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),	\
  sprintf ((OUTPUT), "%s.%d", (NAME), (LABELNO)))

/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  fprintf (FILE, "\t.word L%d\n", VALUE)

/* This is how to output an element of a case-vector that is relative.  */


#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL)  \
  fprintf (FILE, "\t.word L%d-L%d\n", VALUE, REL)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.

   On Pyramids, the text segment must always be word aligned.
   On Pyramids, .align takes only args between 2 and 5.
  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)  \
  fprintf (FILE, "\t.align %d\n", (LOG) < 2 ? 2 : (LOG))

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

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.
   On the Pyr, we support the conventional CODE characters:

   'f' for float insn (print a CONST_DOUBLE as a float rather than in hex)
   which are never used. */

/* FIXME : should be more robust with CONST_DOUBLE. */

#define PRINT_OPERAND(FILE, X, CODE)  \
{ if (GET_CODE (X) == REG)						\
    fprintf (FILE, "%s", reg_names [REGNO (X) + ((CODE) == 'R')]);	\
									\
  else if (GET_CODE (X) == MEM)						\
    output_address (XEXP (X, 0));					\
									\
  else if (GET_CODE (X) == CONST_DOUBLE && GET_MODE (X) == SFmode)	\
    { union { double d; int i[2]; } u;					\
      union { float f; int i; } u1;					\
      u.i[0] = CONST_DOUBLE_LOW (X); u.i[1] = CONST_DOUBLE_HIGH (X);	\
      u1.f = u.d;							\
      if (CODE == 'f')							\
        fprintf (FILE, "$0f%.0e", u1.f);				\
      else								\
        fprintf (FILE, "$0x%x", u1.i); }				\
									\
  else if (GET_CODE (X) == CONST_DOUBLE && GET_MODE (X) != VOIDmode)	\
    { union { double d; int i[2]; } u;					\
      u.i[0] = CONST_DOUBLE_LOW (X); u.i[1] = CONST_DOUBLE_HIGH (X);	\
      fprintf (FILE, "$0d%.20e", u.d); }				\
									\
  else if (CODE == 'N')							\
    switch (GET_CODE (X))						\
      {									\
      case EQ:	fputs ("eq", FILE);	break;				\
      case NE:	fputs ("ne", FILE);	break;				\
      case GT:								\
      case GTU:	fputs ("gt", FILE);	break;				\
      case LT:								\
      case LTU:	fputs ("lt", FILE);	break;				\
      case GE:								\
      case GEU:	fputs ("ge", FILE);	break;				\
      case LE:								\
      case LEU:	fputs ("le", FILE);	break;				\
      }									\
									\
  else if (CODE == 'C')							\
    switch (GET_CODE (X))						\
      {									\
      case EQ:	fputs ("ne", FILE);	break;				\
      case NE:	fputs ("eq", FILE);	break;				\
      case GT:								\
      case GTU:	fputs ("le", FILE);	break;				\
      case LT:								\
      case LTU:	fputs ("ge", FILE);	break;				\
      case GE:								\
      case GEU:	fputs ("lt", FILE);	break;				\
      case LE:								\
      case LEU:	fputs ("gt", FILE);	break;				\
      }									\
									\
  else if (CODE == 'R')							\
    switch (GET_CODE (X))						\
      {									\
      case EQ:	fputs ("eq", FILE);	break;				\
      case NE:	fputs ("ne", FILE);	break;				\
      case GT:								\
      case GTU:	fputs ("lt", FILE);	break;				\
      case LT:								\
      case LTU:	fputs ("gt", FILE);	break;				\
      case GE:								\
      case GEU:	fputs ("le", FILE);	break;				\
      case LE:								\
      case LEU:	fputs ("ge", FILE);	break;				\
      }									\
									\
  else { putc ('$', FILE); output_addr_const (FILE, X); }		\
}

/* Print a memory operand whose address is ADDR, on file FILE.  */
/* This is horrendously complicated.  */
#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  \
{									\
  register rtx reg1, reg2, breg, ireg;					\
  register rtx addr = ADDR;						\
  rtx offset, scale;							\
 retry:									\
  switch (GET_CODE (addr))						\
    {									\
    case MEM:								\
      fprintf (stderr, "bad Mem "); debug_rtx (addr);			\
      addr = XEXP (addr, 0);						\
      abort ();								\
    case REG:								\
      fprintf (FILE, "(%s)", reg_names [REGNO (addr)]);			\
      break;								\
    case PLUS:								\
      reg1 = 0;	reg2 = 0;						\
      ireg = 0;	breg = 0;						\
      offset = 0;							\
      if (CONSTANT_ADDRESS_P (XEXP (addr, 0))				\
	  || GET_CODE (XEXP (addr, 0)) == MEM)				\
	{								\
	  offset = XEXP (addr, 0);					\
	  addr = XEXP (addr, 1);					\
	}								\
      else if (CONSTANT_ADDRESS_P (XEXP (addr, 1))			\
	       || GET_CODE (XEXP (addr, 1)) == MEM)			\
	{								\
	  offset = XEXP (addr, 1);					\
	  addr = XEXP (addr, 0);					\
	}								\
      if (GET_CODE (addr) != PLUS) ;					\
      else if (GET_CODE (XEXP (addr, 0)) == MULT)			\
	{								\
	  reg1 = XEXP (addr, 0);					\
	  addr = XEXP (addr, 1);					\
	}								\
      else if (GET_CODE (XEXP (addr, 1)) == MULT)			\
	{								\
	  reg1 = XEXP (addr, 1);					\
	  addr = XEXP (addr, 0);					\
	}								\
      else if (GET_CODE (XEXP (addr, 0)) == REG)			\
	{								\
	  reg1 = XEXP (addr, 0);					\
	  addr = XEXP (addr, 1);					\
	}								\
      else if (GET_CODE (XEXP (addr, 1)) == REG)			\
	{								\
	  reg1 = XEXP (addr, 1);					\
	  addr = XEXP (addr, 0);					\
	}								\
      if (GET_CODE (addr) == REG || GET_CODE (addr) == MULT)		\
	{								\
	  if (reg1 == 0)						\
	    reg1 = addr;						\
          else								\
	    reg2 = addr;						\
	  addr = 0;							\
	}								\
      if (offset != 0) 							\
	{								\
	  if (addr != 0) {						\
	    fprintf (stderr, "\nBad addr "); debug_rtx (addr);		\
	    abort ();}							\
	  addr = offset;						\
	}								\
      if (reg1 != 0 && GET_CODE (reg1) == MULT)				\
	{ breg = reg2; ireg = reg1; }					\
      else if (reg2 != 0 && GET_CODE (reg2) == MULT)			\
	{ breg = reg1; ireg = reg2; }					\
      else if (reg2 != 0 || GET_CODE (addr) == MEM)			\
	{ breg = reg2; ireg = reg1; }					\
      else								\
	{ breg = reg1; ireg = reg2; }					\
      if (addr != 0)							\
	output_address (offset);					\
      if (breg != 0)							\
	{ if (GET_CODE (breg) != REG)					\
	    {								\
	      fprintf (stderr, "bad Breg"); debug_rtx (addr);		\
	      abort ();							\
	    }								\
	  fprintf (FILE, "(%s)", reg_names[REGNO (breg)]); }		\
      if (ireg != 0)							\
	{								\
	  if (GET_CODE (ireg) == MULT)					\
	    {								\
	      scale = XEXP (ireg, 1);					\
	      ireg = XEXP (ireg, 0);					\
	      if (GET_CODE (ireg) != REG)				\
	        { register rtx tem;					\
		  tem = ireg; ireg = scale; scale = tem;		\
	        }							\
 	      if (GET_CODE (ireg) != REG) {				\
		      fprintf (stderr, "bad idx "); debug_rtx (addr);	\
		abort (); }						\
	      if ((GET_CODE (scale) == CONST_INT) && (INTVAL(scale) >= 1))\
		fprintf (FILE, "[%s*0x%x]", reg_names[REGNO (ireg)],	\
			 INTVAL(scale));				\
	      else							\
		fprintf (FILE, "[%s*1]", reg_names[REGNO (ireg)]);	\
 	    } 								\
	  else if (GET_CODE (ireg) == REG)				\
	      fprintf (FILE, "[%s*1]", reg_names[REGNO (ireg)]);	\
	  else								\
	    {								\
	      fprintf (stderr, "Not indexed at all!"); debug_rtx (addr);\
	      abort ();							\
	    }								\
	 }								\
       break;								\
    default:								\
      output_addr_const (FILE, addr);					\
   }									\
}
