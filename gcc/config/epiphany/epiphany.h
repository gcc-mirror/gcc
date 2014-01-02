/* Definitions of target machine for GNU compiler, Argonaut EPIPHANY cpu.
   Copyright (C) 1994-2014 Free Software Foundation, Inc.
   Contributed by Embecosm on behalf of Adapteva, Inc.

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

#ifndef GCC_EPIPHANY_H
#define GCC_EPIPHANY_H

#undef LINK_SPEC
#undef STARTFILE_SPEC
#undef ENDFILE_SPEC
#undef SIZE_TYPE
#undef PTRDIFF_TYPE
#undef WCHAR_TYPE
#undef WCHAR_TYPE_SIZE

/* Names to predefine in the preprocessor for this target machine.  */
#define TARGET_CPU_CPP_BUILTINS()		\
  do						\
    {						\
	builtin_define ("__epiphany__");	\
        builtin_define ("__little_endian__");	\
	builtin_define_with_int_value ("__EPIPHANY_STACK_OFFSET__", \
				       epiphany_stack_offset); \
	builtin_assert ("cpu=epiphany");	\
	builtin_assert ("machine=epiphany");	\
    } while (0)

/* Pick up the libgloss library. One day we may do this by linker script, but
   for now its static.
   libgloss might use errno/__errno, which might not have been needed when we
   saw libc the first time, so link with libc a second time.  */
#undef LIB_SPEC
#define LIB_SPEC "%{!shared:%{g*:-lg} %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p}} -lepiphany %{!shared:%{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p}}"

#define LINK_SPEC "%{v}"

#define STARTFILE_SPEC "%{!shared:crt0.o%s} crti.o%s " \
  "%{mfp-mode=int:crtint.o%s} %{mfp-mode=truncate:crtrunc.o%s} " \
  "%{m1reg-r43:crtm1reg-r43.o%s} %{m1reg-r63:crtm1reg-r63.o%s} " \
  "crtbegin.o%s"

#define ENDFILE_SPEC "crtend.o%s crtn.o%s"

#define EPIPHANY_LIBRARY_EXTRA_SPEC \
  "-ffixed-r40 -ffixed-r41 -ffixed-r42 -ffixed-r43"

/* In the "spec:" rule,, t-epiphany changes this to epiphany_library_stub_spec
   and epiphany_library_extra_spec, respectively.  */
#define EXTRA_SPECS \
  { "epiphany_library_extra_spec", "" }, \
  { "epiphany_library_build_spec", EPIPHANY_LIBRARY_EXTRA_SPEC }, \

#define DRIVER_SELF_SPECS " %(epiphany_library_extra_spec) "

#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX "_"

#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC) \
   asm (SECTION_OP "\n\
	mov r0,%low(" USER_LABEL_PREFIX #FUNC")\n\
	movt r0,%high(" USER_LABEL_PREFIX #FUNC")\n\
	jalr r0\n\
	.text");

#if 0 /* We would like to use Posix for profiling, but the simulator
	 interface still lacks mkdir.  */
#define TARGET_POSIX_IO
#endif

/* Target machine storage layout.  */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */
#define BITS_BIG_ENDIAN 0

/* Define this if most significant byte of a word is the lowest numbered.  */
#define BYTES_BIG_ENDIAN 0

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
#define WORDS_BIG_ENDIAN 0

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 4

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases,
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.  */
/* It is far faster to zero extend chars than to sign extend them */

#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)	\
  if (GET_MODE_CLASS (MODE) == MODE_INT		\
      && GET_MODE_SIZE (MODE) < 4)      	\
    {						\
      if (MODE == QImode)			\
	UNSIGNEDP = 1;				\
      else if (MODE == HImode)			\
	UNSIGNEDP = 1;				\
      (MODE) = SImode;				\
    }

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 32

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY 64

/* ALIGN FRAMES on word boundaries */
#define EPIPHANY_STACK_ALIGN(LOC) (((LOC)+7) & ~7)

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 32

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* A bit-field declared as `int' forces `int' alignment for the struct.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* No data type wants to be aligned rounder than this.  */
/* This is bigger than currently necessary for the EPIPHANY.  If 8 byte floats are
   ever added it's not clear whether they'll need such alignment or not.  For
   now we assume they will.  We can always relax it if necessary but the
   reverse isn't true.  */
#define BIGGEST_ALIGNMENT 64

/* The best alignment to use in cases where we have a choice.  */
#define FASTEST_ALIGNMENT 64

#define MALLOC_ABI_ALIGNMENT BIGGEST_ALIGNMENT

/* Make strings dword-aligned so strcpy from constants will be faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  ((TREE_CODE (EXP) == STRING_CST	\
    && (ALIGN) < FASTEST_ALIGNMENT)	\
   ? FASTEST_ALIGNMENT : (ALIGN))

/* Make arrays of chars dword-aligned for the same reasons.
   Also, align arrays of SImode items.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < FASTEST_ALIGNMENT		\
   ? FASTEST_ALIGNMENT				\
   : (TREE_CODE (TYPE) == ARRAY_TYPE		\
      && TYPE_MODE (TREE_TYPE (TYPE)) == SImode	\
      && (ALIGN) < FASTEST_ALIGNMENT)		\
   ? FASTEST_ALIGNMENT				\
   : (ALIGN))

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
/* On the EPIPHANY the lower address bits are masked to 0 as necessary.  The chip
   won't croak when given an unaligned address, but the insn will still fail
   to produce the correct result.  */
#define STRICT_ALIGNMENT 1

/* layout_type overrides our ADJUST_ALIGNMENT settings from epiphany-modes.def
   for vector modes, so we have to override it back.  */
#define ROUND_TYPE_ALIGN(TYPE, MANGLED_ALIGN, SPECIFIED_ALIGN) \
 (TREE_CODE (TYPE) == VECTOR_TYPE && !TYPE_USER_ALIGN (TYPE) \
  && SPECIFIED_ALIGN <= GET_MODE_ALIGNMENT (TYPE_MODE (TYPE)) \
  ? GET_MODE_ALIGNMENT (TYPE_MODE (TYPE)) \
  : ((TREE_CODE (TYPE) == RECORD_TYPE \
      || TREE_CODE (TYPE) == UNION_TYPE \
      || TREE_CODE (TYPE) == QUAL_UNION_TYPE) \
     && !TYPE_PACKED (TYPE)) \
  ? epiphany_special_round_type_align ((TYPE), (MANGLED_ALIGN), \
				       (SPECIFIED_ALIGN)) \
  : MAX ((MANGLED_ALIGN), (SPECIFIED_ALIGN)))

#define ADJUST_FIELD_ALIGN(FIELD, COMPUTED) \
  epiphany_adjust_field_align((FIELD), (COMPUTED))

/* Layout of source language data types.  */

#define SHORT_TYPE_SIZE		16
#define INT_TYPE_SIZE		32
#define LONG_TYPE_SIZE		32
#define LONG_LONG_TYPE_SIZE	64
#define FLOAT_TYPE_SIZE		32
#define DOUBLE_TYPE_SIZE	64
#define LONG_DOUBLE_TYPE_SIZE	64

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 0

#define SIZE_TYPE "long unsigned int"
#define PTRDIFF_TYPE "long int"
#define WCHAR_TYPE "unsigned int"
#define WCHAR_TYPE_SIZE BITS_PER_WORD

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.  */

#define FIRST_PSEUDO_REGISTER 78


/* General purpose registers.  */
#define GPR_FIRST       0                       /* First gpr */

#define PIC_REGNO       (GPR_FIRST + 28)        /* PIC register.  */
#define GPR_LAST        (GPR_FIRST + 63)        /* Last gpr */
#define CORE_CONTROL_FIRST CONFIG_REGNUM
#define CORE_CONTROL_LAST IRET_REGNUM

#define GPR_P(R)	IN_RANGE (R, GPR_FIRST, GPR_LAST)
#define GPR_OR_AP_P(R)	(GPR_P (R) || (R) == ARG_POINTER_REGNUM)

#define GPR_OR_PSEUDO_P(R)	(GPR_P (R) || (R) >= FIRST_PSEUDO_REGISTER)
#define GPR_AP_OR_PSEUDO_P(R)	(GPR_OR_AP_P (R) || (R) >= FIRST_PSEUDO_REGISTER)

#define FIXED_REGISTERS							\
{	/* Integer Registers */						\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 000-007, gr0  - gr7  */	\
	0, 0, 0, 0, 0, 1, 0, 0,		/* 008-015, gr8  - gr15 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 016-023, gr16 - gr23 */	\
	0, 0, 0, 0, 1, 1, 1, 1,		/* 024-031, gr24 - gr31 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 032-039, gr32 - gr39 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 040-047, gr40 - gr47 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 048-055, gr48 - gr55 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 056-063, gr56 - gr63 */	\
	/* Other registers */						\
	1,				/* 64 AP   - fake arg ptr */	\
	1,				/* soft frame pointer */	\
        1,				/* CC_REGNUM  - integer conditions */\
	1,				/* CCFP_REGNUM  - fp conditions */\
	1, 1, 1, 1, 1, 1,               /* Core Control Registers.  */  \
	1, 1, 1,			/* FP_{NEAREST,...}_REGNUM */\
	1,				/* UNKNOWN_REGNUM - placeholder.  */\
}

/* Like `FIXED_REGISTERS' but has 1 for each register that is clobbered (in
   general) by function calls as well as for fixed registers.  This macro
   therefore identifies the registers that are not available for general
   allocation of values that must live across function calls.

   If a register has 0 in `CALL_USED_REGISTERS', the compiler automatically
   saves it on function entry and restores it on function exit, if the register
   is used within the function.  */

#define CALL_USED_REGISTERS						\
{	/* Integer Registers */						\
	1, 1, 1, 1, 0, 0, 0, 0,	        /* 000-007, gr0  - gr7  */	\
	0, 0, 0, 0, 1, 1, 1, 0,		/* 008-015, gr8  - gr15 */	\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 016-023, gr16 - gr23 */	\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 024-031, gr24 - gr31 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 032-039, gr32 - gr38 */	\
	0, 0, 0, 0, 1, 1, 1, 1,		/* 040-047, gr40 - gr47 */	\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 048-055, gr48 - gr55 */	\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 056-063, gr56 - gr63 */	\
	1,				/* 64 AP   - fake arg ptr */	\
	1,				/* soft frame pointer */	\
	1,				/* 66 CC_REGNUM */   		\
	1,				/* 67 CCFP_REGNUM */   		\
	1, 1, 1, 1, 1, 1,               /* Core Control Registers.  */  \
	1, 1, 1,			/* FP_{NEAREST,...}_REGNUM */\
	1,				/* UNKNOWN_REGNUM - placeholder.  */\
}

#define REG_ALLOC_ORDER \
  { \
    0, 1, 2, 3, /* Caller-saved 'small' registers.  */ \
    12, /* Caller-saved unpaired register.  */ \
    /* Caller-saved registers.  */ \
    16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, \
    44, 45, 46, 47, \
    48, 49, 50, 51, 52, 53, 54, 55, \
    56, 57, 58, 59, 60, 61, 62, 63, \
    4, 5, 6, 7, /* Calle-saved 'small' registers.  */ \
    15, /* Calle-saved unpaired register.  */ \
    8, 9, 10, 11, /* Calle-saved registers.  */ \
    32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, \
    14, 13, /* Link register, stack pointer.  */ \
    /* Can't allocate, but must name these... */ \
    28, 29, 30, 31, \
    64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77 \
  }

#define HARD_REGNO_RENAME_OK(SRC, DST) epiphany_regno_rename_ok (SRC, DST)

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.  */
#define HARD_REGNO_NREGS(REGNO, MODE) \
((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.  */
extern const unsigned int epiphany_hard_regno_mode_ok[];
extern unsigned int epiphany_mode_class[];
#define HARD_REGNO_MODE_OK(REGNO, MODE) hard_regno_mode_ok((REGNO), (MODE))

/* A C expression that is nonzero if it is desirable to choose
   register allocation so as to avoid move instructions between a
   value of mode MODE1 and a value of mode MODE2.

   If `HARD_REGNO_MODE_OK (R, MODE1)' and `HARD_REGNO_MODE_OK (R,
   MODE2)' are ever different for any R, then `MODES_TIEABLE_P (MODE1,
   MODE2)' must be zero.  */

#define MODES_TIEABLE_P(MODE1, MODE2) 1

/* Register classes and constants.  */

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

   It is important that any condition codes have class NO_REGS.
   See `register_operand'.  */

enum reg_class {
  NO_REGS,
  LR_REGS,
  SHORT_INSN_REGS,
  SIBCALL_REGS,
  GENERAL_REGS,
  CORE_CONTROL_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define N_REG_CLASSES ((int) LIM_REG_CLASSES)

/* Give names of register classes as strings for dump file.  */
#define REG_CLASS_NAMES \
{			\
  "NO_REGS",		\
  "LR_REGS",		\
  "SHORT_INSN_REGS",	\
  "SIBCALL_REGS",	\
  "GENERAL_REGS",	\
  "CORE_CONTROL_REGS",	\
  "ALL_REGS"		\
}

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS						\
{  /* r0-r31    r32-r63  ap/sfp/cc1/cc2/iret/status */			\
  { 0x00000000,0x00000000,0x0},  /* NO_REGS  */				\
  { 0x00004000,0x00000000,0x0},  /* LR_REGS  */				\
  { 0x000000ff,0x00000000,0x0},  /* SHORT_INSN_REGS */			\
  { 0xffff100f,0xffffff00,0x0},  /* SIBCALL_REGS */			\
  { 0xffffffff,0xffffffff,0x0003}, /* GENERAL_REGS */			\
  { 0x00000000,0x00000000,0x03f0}, /* CORE_CONTROL_REGS */		\
  { 0xffffffff,0xffffffff,0x3fff}, /* ALL_REGS */				\
}


/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */
extern enum reg_class epiphany_regno_reg_class[FIRST_PSEUDO_REGISTER];
#define REGNO_REG_CLASS(REGNO) \
(epiphany_regno_reg_class[REGNO])

/* The class value for index registers, and the one for base regs.  */
#define BASE_REG_CLASS GENERAL_REGS
#define INDEX_REG_CLASS GENERAL_REGS

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in reginfo.c during register
   allocation.  */
#define REGNO_OK_FOR_BASE_P(REGNO) \
((REGNO) < FIRST_PSEUDO_REGISTER || (unsigned) reg_renumber[REGNO] < FIRST_PSEUDO_REGISTER)
#define REGNO_OK_FOR_INDEX_P(REGNO) \
((REGNO) < FIRST_PSEUDO_REGISTER || (unsigned) reg_renumber[REGNO] < FIRST_PSEUDO_REGISTER)



/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */
#define PREFERRED_RELOAD_CLASS(X,CLASS) \
(CLASS)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
#define CLASS_MAX_NREGS(CLASS, MODE) \
((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* The letters I, J, K, L, M, N, O, P in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.  */

/* 'I' is used for 16 bit unsigned.
   'Cal' is used for long immediates (32 bits)
   'K' is used for any constant up to 5 bits.
   'L' is used for any 11 bit signed.
*/

#define IMM16(X)     (IN_RANGE ((X), 0, 0xFFFF))
#define SIMM16(X)    (IN_RANGE ((X), -65536, 65535))
#define SIMM11(X)    (IN_RANGE ((X), -1024, 1023))
#define IMM5(X)      (IN_RANGE ((X), 0, 0x1F))

typedef struct GTY (()) machine_function
{
  unsigned args_parsed : 1;
  unsigned pretend_args_odd : 1;
  unsigned lr_clobbered : 1;
  unsigned control_use_inserted : 1;
  unsigned lr_slot_known : 1;
  unsigned sw_entities_processed : 6;
  long lr_slot_offset;
  rtx and_mask;
  rtx or_mask;
  unsigned unknown_mode_uses;
  unsigned unknown_mode_sets;
} machine_function_t;

#define MACHINE_FUNCTION(fun) (fun)->machine

#define INIT_EXPANDERS epiphany_init_expanders ()

/* Stack layout and stack pointer usage.  */

/* Define this macro if pushing a word onto the stack moves the stack
   pointer to a smaller address.  */
#define STACK_GROWS_DOWNWARD

/* Define this to nonzero if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */
#define FRAME_GROWS_DOWNWARD 1

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.  */
#define STARTING_FRAME_OFFSET epiphany_stack_offset

/* Offset from the stack pointer register to the first location at which
   outgoing arguments are placed.  */
#define STACK_POINTER_OFFSET epiphany_stack_offset

/* Offset of first parameter from the argument pointer register value.  */
/* 4 bytes for each of previous fp, return address, and previous gp.
   4 byte reserved area for future considerations.  */
#define FIRST_PARM_OFFSET(FNDECL) \
  (epiphany_stack_offset \
   + (MACHINE_FUNCTION (DECL_STRUCT_FUNCTION (FNDECL))->pretend_args_odd \
      ? 4 : 0))

#define INCOMING_FRAME_SP_OFFSET epiphany_stack_offset

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM GPR_SP

/* Base register for access to local variables of the function.  */
#define HARD_FRAME_POINTER_REGNUM GPR_FP

/* Register in which static-chain is passed to a function.  This must
   not be a register used by the prologue.  */
#define STATIC_CHAIN_REGNUM GPR_IP

/* Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */

#define ELIMINABLE_REGS						\
{{ FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},			\
 { FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM},		\
 { ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},                   \
 { ARG_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM},		\
}

/* Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  ((OFFSET) = epiphany_initial_elimination_offset ((FROM), (TO)))

/* Function argument passing.  */

/* If defined, the maximum amount of space required for outgoing
   arguments will be computed and placed into the variable
   `current_function_outgoing_args_size'.  No space will be pushed
   onto the stack for each call; instead, the function prologue should
   increase the stack frame size by this amount.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.  */
#define CUMULATIVE_ARGS int

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
((CUM) = 0)

/* The number of registers used for parameter passing.  Local to this file.  */
#define MAX_EPIPHANY_PARM_REGS 4

/* 1 if N is a possible register number for function argument passing.  */
#define FUNCTION_ARG_REGNO_P(N) \
((unsigned) (N) < MAX_EPIPHANY_PARM_REGS)

/* Return boolean indicating arg of type TYPE and mode MODE will be passed in
   a reg.  This includes arguments that have to be passed by reference as the
   pointer to them is passed in a reg if one is available (and that is what
   we're given).
   This macro is only used in this file.  */
/* We must use partial argument passing because of the chosen mode
   of varargs handling.  */
#define PASS_IN_REG_P(CUM, MODE, TYPE) \
  (ROUND_ADVANCE_CUM ((CUM), (MODE), (TYPE)) < MAX_EPIPHANY_PARM_REGS)

/* Tell GCC to use TARGET_RETURN_IN_MEMORY.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */
#define EXIT_IGNORE_STACK 1

#define EPILOGUE_USES(REGNO) epiphany_epilogue_uses (REGNO)

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */
#define FUNCTION_PROFILER(FILE, LABELNO)

/* Given an rtx for the frame pointer,
   return an rtx for the address of the frame.  */
#define FRAME_ADDR_RTX(frame) \
  ((frame) == hard_frame_pointer_rtx ? arg_pointer_rtx : NULL)

#define EPIPHANY_RETURN_REGNO \
  ((current_function_decl != NULL \
    && epiphany_is_interrupt_p (current_function_decl)) \
   ? IRET_REGNUM : GPR_LR)
/* This is not only for dwarf unwind info, but also for the benefit of
   df-scan.c to tell it that LR is live at the function start.  */
#define INCOMING_RETURN_ADDR_RTX gen_rtx_REG (Pmode, EPIPHANY_RETURN_REGNO)

/* However, we haven't implemented the rest needed for dwarf2 unwind info.  */
#define DWARF2_UNWIND_INFO 0

#define RETURN_ADDR_RTX(count, frame) \
  (count ? NULL_RTX \
   : gen_rtx_UNSPEC (SImode, gen_rtvec (1, const0_rtx), UNSPEC_RETURN_ADDR))

#define DWARF_FRAME_RETURN_COLUMN DWARF_FRAME_REGNUM (EPIPHANY_RETURN_REGNO)

/* Trampolines.
   An epiphany trampoline looks like this:
   mov r16,%low(fnaddr)
   movt r16,%high(fnaddr)
   mov ip,%low(cxt)
   movt ip,%high(cxt)
   jr r16  */

/* Length in units of the trampoline for entering a nested function.  */
#define TRAMPOLINE_SIZE 20

/* Addressing modes, and classification of registers for them.  */

/* Maximum number of registers that can appear in a valid memory address.  */
#define MAX_REGS_PER_ADDRESS 2

/* We have post_modify (load/store with update).  */
#define HAVE_POST_INCREMENT TARGET_POST_INC
#define HAVE_POST_DECREMENT TARGET_POST_INC
#define HAVE_POST_MODIFY_DISP TARGET_POST_MODIFY
#define HAVE_POST_MODIFY_REG TARGET_POST_MODIFY

/* Currently, the only users of the USE_*CREMENT macros are
   move_by_pieces / store_by_pieces_1 .  We don't want them to use
   POST_MODIFY modes, because we got ample addressing range for the
   reg+offset addressing mode; besides, there are short index+offset loads,
   but the only short post-modify load uses POST_MODIFY_REG.
   Moreover, using auto-increment in move_by_pieces from structure copying
   in the prologue causes confused debug output.
   If another pass starts using these macros where the use of these
   addressing modes would make more sense, we can try checking the
   current pass.  */
#define USE_LOAD_POST_INCREMENT(MODE) 0
#define USE_LOAD_POST_DECREMENT(MODE) 0
#define USE_STORE_POST_INCREMENT(MODE) 0
#define USE_STORE_POST_DECREMENT(MODE) 0

/* Recognize any constant value that is a valid address.  */
#define CONSTANT_ADDRESS_P(X) \
(GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF	\
 || GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST)

#define RTX_OK_FOR_OFFSET_P(MODE, X) \
  RTX_OK_FOR_OFFSET_1 (GET_MODE_CLASS (MODE) == MODE_VECTOR_INT \
		       && epiphany_vect_align == 4 ? SImode : (MODE), X)
#define RTX_OK_FOR_OFFSET_1(MODE, X) \
  (GET_CODE (X) == CONST_INT \
   && !(INTVAL (X) & (GET_MODE_SIZE (MODE) - 1)) \
   && INTVAL (X) >= -2047 * (int) GET_MODE_SIZE (MODE) \
   && INTVAL (X) <=  2047 * (int) GET_MODE_SIZE (MODE))

/* Frame offsets cannot be evaluated till the frame pointer is eliminated.  */
#define RTX_FRAME_OFFSET_P(X) \
  ((X) == frame_pointer_rtx \
   || (GET_CODE (X) == PLUS && XEXP ((X), 0) == frame_pointer_rtx \
       && CONST_INT_P (XEXP ((X), 1))))

/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
   return the mode to be used for the comparison.  */
#define SELECT_CC_MODE(OP, X, Y) \
  epiphany_select_cc_mode (OP, X, Y)

/* Return nonzero if SELECT_CC_MODE will never return MODE for a
   floating point inequality comparison.  */

#define REVERSE_CONDITION(CODE, MODE) \
  ((MODE) == CC_FPmode || (MODE) == CC_FP_EQmode || (MODE) == CC_FP_GTEmode \
   || (MODE) == CC_FP_ORDmode || (MODE) == CC_FP_UNEQmode \
   ? reverse_condition_maybe_unordered (CODE) \
   : (MODE) == CCmode ? reverse_condition (CODE) \
   : UNKNOWN)

/* We can reverse all CCmodes with REVERSE_CONDITION.  */
#define REVERSIBLE_CC_MODE(MODE) \
  ((MODE) == CCmode || (MODE) == CC_FPmode || (MODE) == CC_FP_EQmode \
   || (MODE) == CC_FP_GTEmode || (MODE) == CC_FP_ORDmode \
   || (MODE) == CC_FP_UNEQmode)

/* Costs.  */

/* The cost of a branch insn.  */
/* ??? What's the right value here?  Branches are certainly more
   expensive than reg->reg moves.  */
#define BRANCH_COST(speed_p, predictable_p) \
  (speed_p ? epiphany_branch_cost : 1)

/* Nonzero if access to memory by bytes is slow and undesirable.
   For RISC chips, it means that access to memory by bytes is no
   better than access by words when possible, so grab a whole word
   and maybe make use of that.  */
#define SLOW_BYTE_ACCESS 1

/* Define this macro if it is as good or better to call a constant
   function address than to call an address kept in a register.  */
/* On the EPIPHANY, calling through registers is slow.  */
#define NO_FUNCTION_CSE

/* Section selection.  */
/* WARNING: These section names also appear in dwarf2out.c.  */

#define TEXT_SECTION_ASM_OP	"\t.section .text"
#define DATA_SECTION_ASM_OP	"\t.section .data"

#undef  READONLY_DATA_SECTION_ASM_OP
#define READONLY_DATA_SECTION_ASM_OP	"\t.section .rodata"

#define BSS_SECTION_ASM_OP	"\t.section .bss"

/* Define this macro if jump tables (for tablejump insns) should be
   output in the text section, along with the assembler instructions.
   Otherwise, the readonly data section is used.
   This macro is irrelevant if there is no separate readonly data section.  */
#define JUMP_TABLES_IN_TEXT_SECTION (flag_pic)

/* PIC */

/* The register number of the register used to address a table of static
   data addresses in memory.  In some cases this register is defined by a
   processor's ``application binary interface'' (ABI).  When this macro
   is defined, RTL is generated for this register once, as with the stack
   pointer and frame pointer registers.  If this macro is not defined, it
   is up to the machine-dependent files to allocate such a register (if
   necessary).  */
#define PIC_OFFSET_TABLE_REGNUM  (flag_pic ? PIC_REGNO : INVALID_REGNUM)

/* Control the assembler format that we output.  */

/* A C string constant describing how to begin a comment in the target
   assembler language.  The compiler assumes that the comment will
   end at the end of the line.  */
#define ASM_COMMENT_START ";"

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */
#define ASM_APP_ON ""

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */
#define ASM_APP_OFF ""

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.global\t"

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES							\
{									\
  "r0",  "r1",  "r2",  "r3",  "r4",  "r5",  "r6",  "r7",	\
  "r8",  "r9",  "r10", "fp",  "ip",  "sp",  "lr",  "r15",	\
  "r16",  "r17","r18", "r19", "r20", "r21", "r22", "r23",	\
  "r24", "r25", "r26", "r27", "r28", "r29", "r30", "r31",	\
  "r32", "r33", "r34", "r35", "r36", "r37", "r38", "r39",	\
  "r40", "r41", "r42", "r43", "r44", "r45", "r46", "r47",	\
  "r48", "r49", "r50", "r51", "r52", "r53", "r54", "r55",	\
  "r56", "r57", "r58", "r59", "r60", "r61", "r62", "r63",	\
  "ap",  "sfp", "cc1", "cc2",					\
  "config", "status", "lc", "ls", "le", "iret",			\
  "fp_near", "fp_trunc", "fp_anyfp", "unknown"			\
}

#define FINAL_PRESCAN_INSN(INSN, OPVEC, NOPERANDS) \
  epiphany_final_prescan_insn (INSN, OPVEC, NOPERANDS)

#define LOCAL_LABEL_PREFIX  "."

/* A C expression which evaluates to true if CODE is a valid
   punctuation character for use in the `PRINT_OPERAND' macro.  */
extern char epiphany_punct_chars[256];
#define PRINT_OPERAND_PUNCT_VALID_P(CHAR) \
  epiphany_punct_chars[(unsigned char) (CHAR)]

/* This is how to output an element of a case-vector that is absolute.  */
#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
do { \
  if (CASE_VECTOR_MODE == Pmode) \
    asm_fprintf ((FILE), "\t.word %LL%d\n", (VALUE)); \
  else \
    asm_fprintf ((FILE), "\t.short %LL%d\n", (VALUE)); \
} while (0)

/* This is how to output an element of a case-vector that is relative.  */
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL) \
do {							\
  if (CASE_VECTOR_MODE == Pmode) \
    asm_fprintf ((FILE), "\t.word"); \
  else \
    asm_fprintf ((FILE), "\t.short"); \
  asm_fprintf ((FILE), " %LL%d-%LL%d\n", (VALUE), (REL)); \
} while (0)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */
#define ASM_OUTPUT_ALIGN(FILE, LOG) \
do { if ((LOG) != 0) fprintf (FILE, "\t.balign %d\n", 1 << (LOG)); } while (0)

/* Inside the text section, align with nops rather than zeros.  */
#define ASM_OUTPUT_ALIGN_WITH_NOP(FILE, LOG) \
do \
{ \
  if ((LOG) != 0) fprintf (FILE, "\t.balignw %d,0x01a2\n", 1 << (LOG)); \
} while (0)

/* This is how to declare the size of a function.  */
#undef ASM_DECLARE_FUNCTION_SIZE
#define ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)			\
  do									\
    {									\
      const char *__name = (FNAME);					\
      tree attrs = DECL_ATTRIBUTES ((DECL));				\
									\
      if (!flag_inhibit_size_directive)					\
	{								\
	  if (lookup_attribute ("forwarder_section", attrs))			\
	    {								\
	      const char *prefix = "__forwarder_dst_";			\
	      char *dst_name						\
		= (char *) alloca (strlen (prefix) + strlen (__name) + 1); \
									\
	      strcpy (dst_name, prefix);				\
	      strcat (dst_name, __name);				\
	      __name = dst_name;					\
	    }								\
	  ASM_OUTPUT_MEASURED_SIZE ((FILE), __name);			\
	}								\
    }									\
  while (0)

/* Debugging information.  */

/* Generate DBX and DWARF debugging information.  */
#define DBX_DEBUGGING_INFO 1

#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* Turn off splitting of long stabs.  */
#define DBX_CONTIN_LENGTH 0

/* Miscellaneous.  */

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE (TARGET_SMALL16 && optimize_size ? HImode : Pmode)

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, UNKNOWN if none.  */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 8

/* Define this to be nonzero if shift instructions ignore all but the low-order
   few bits.  */
#define SHIFT_COUNT_TRUNCATED 1

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */

#define Pmode SImode

/* A function address in a call instruction.  */
#define FUNCTION_MODE SImode

/* EPIPHANY function types.  */
enum epiphany_function_type
{
  EPIPHANY_FUNCTION_UNKNOWN, EPIPHANY_FUNCTION_NORMAL,
  EPIPHANY_FUNCTION_INTERRUPT
};

#define EPIPHANY_INTERRUPT_P(TYPE) ((TYPE) == EPIPHANY_FUNCTION_INTERRUPT)

/* Compute the type of a function from its DECL.  */

#define IMMEDIATE_PREFIX "#"

#define OPTIMIZE_MODE_SWITCHING(ENTITY) \
  (epiphany_optimize_mode_switching (ENTITY))

/* We have two fake entities for lazy code motion of the mask constants,
   one entity each for round-to-nearest / truncating
   with a different idea what FP_MODE_ROUND_UNKNOWN will be, and
   finally an entity that runs in a second mode switching pass to
   resolve FP_MODE_ROUND_UNKNOWN.  */
#define NUM_MODES_FOR_MODE_SWITCHING \
  { 2, 2, 2, \
    FP_MODE_NONE, FP_MODE_NONE, FP_MODE_NONE, FP_MODE_NONE, FP_MODE_NONE }

#define MODE_NEEDED(ENTITY, INSN) epiphany_mode_needed((ENTITY), (INSN))

#define MODE_PRIORITY_TO_MODE(ENTITY, N) \
  (epiphany_mode_priority_to_mode ((ENTITY), (N)))

#define EMIT_MODE_SET(ENTITY, MODE, HARD_REGS_LIVE) \
  emit_set_fp_mode ((ENTITY), (MODE), (HARD_REGS_LIVE))

#define MODE_ENTRY(ENTITY) (epiphany_mode_entry_exit ((ENTITY), false))
#define MODE_EXIT(ENTITY) (epiphany_mode_entry_exit ((ENTITY), true))
#define MODE_AFTER(ENTITY, LAST_MODE, INSN) \
  (epiphany_mode_after ((ENTITY), (LAST_MODE), (INSN)))

#define TARGET_INSERT_MODE_SWITCH_USE epiphany_insert_mode_switch_use

/* Mode switching entities.  */
enum
{
  EPIPHANY_MSW_ENTITY_AND,
  EPIPHANY_MSW_ENTITY_OR,
  EPIPHANY_MSW_ENTITY_CONFIG, /* 1 means config is known or saved.  */
  EPIPHANY_MSW_ENTITY_NEAREST,
  EPIPHANY_MSW_ENTITY_TRUNC,
  EPIPHANY_MSW_ENTITY_ROUND_UNKNOWN,
  EPIPHANY_MSW_ENTITY_ROUND_KNOWN,
  EPIPHANY_MSW_ENTITY_FPU_OMNIBUS,
  EPIPHANY_MSW_ENTITY_NUM
};

extern int epiphany_normal_fp_rounding;
#ifndef IN_LIBGCC2
extern rtl_opt_pass *make_pass_mode_switch_use (gcc::context *ctxt);
extern rtl_opt_pass *make_pass_resolve_sw_modes (gcc::context *ctxt);
#endif

/* This will need to be adjusted when FP_CONTRACT_ON is properly
   implemented.  */
#define TARGET_FUSED_MADD (flag_fp_contract_mode == FP_CONTRACT_FAST)

#undef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL) \
  epiphany_start_function ((FILE), (NAME), (DECL))

#endif /* !GCC_EPIPHANY_H */
