/* Definitions of target machine for GNU compiler. NEC V850 series
   Copyright (C) 1996, 1997, 1998, 1999 Free Software Foundation, Inc.
   Contributed by Jeff Law (law@cygnus.com).

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

#include "svr4.h"	/* Automatically does #undef CPP_PREDEFINES */

/* These are defiend in svr4.h but we want to override them.  */
#undef ASM_FINAL_SPEC
#undef LIB_SPEC
#undef ENDFILE_SPEC
#undef LINK_SPEC
#undef STARTFILE_SPEC
#undef ASM_SPEC


#define TARGET_CPU_generic 	1

#ifndef TARGET_CPU_DEFAULT
#define TARGET_CPU_DEFAULT	TARGET_CPU_generic
#endif

#define MASK_DEFAULT            MASK_V850
#define SUBTARGET_ASM_SPEC 	"%{!mv*:-mv850}"
#define SUBTARGET_CPP_SPEC 	"%{!mv*:-D__v850__}"
#define TARGET_VERSION 		fprintf (stderr, " (NEC V850)");


#define ASM_SPEC "%{mv*:-mv%*}"
#define CPP_SPEC		"%{mv850ea:-D__v850ea__} %{mv850e:-D__v850e__} %{mv850:-D__v850__} %(subtarget_cpp_spec)"

#define EXTRA_SPECS \
 { "subtarget_asm_spec", SUBTARGET_ASM_SPEC }, \
 { "subtarget_cpp_spec", SUBTARGET_CPP_SPEC } 

/* Names to predefine in the preprocessor for this target machine.  */
#define CPP_PREDEFINES "-D__v851__ -D__v850"

/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;

/* Target flags bits, see below for an explanation of the bits.  */
#define MASK_GHS		0x00000001
#define MASK_LONG_CALLS		0x00000002
#define MASK_EP			0x00000004
#define MASK_PROLOG_FUNCTION	0x00000008
#define MASK_DEBUG		0x40000000

#define MASK_CPU                0x00000030
#define MASK_V850               0x00000010

#define MASK_BIG_SWITCH		0x00000100

/* Macros used in the machine description to test the flags.  */

/* The GHS calling convention support doesn't really work,
   mostly due to a lack of documentation.  Outstanding issues:

     * How do varargs & stdarg really work.  How to they handle
     passing structures (if at all).

     * Doubles are normally 4 byte aligned, except in argument
     lists where they are 8 byte aligned.  Is the alignment
     in the argument list based on the first parameter,
     first stack parameter, etc etc.

     * Passing/returning of large structures probably isn't the same
     as GHS.  We don't have enough documentation on their conventions
     to be compatible.

     * Tests of SETUP_INCOMING_VARARGS need to be made runtime checks
     since it depends on TARGET_GHS.  */
#define TARGET_GHS (target_flags & MASK_GHS)
 
/* Don't do PC-relative calls, instead load the address of the target
   function into a register and perform a register indirect call.  */
#define TARGET_LONG_CALLS (target_flags & MASK_LONG_CALLS)

/* Whether to optimize space by using ep (r30) for pointers with small offsets
   in basic blocks.  */
#define TARGET_EP (target_flags & MASK_EP)

/* Whether to call out-of-line functions to save registers or not.  */
#define TARGET_PROLOG_FUNCTION (target_flags & MASK_PROLOG_FUNCTION)

#define TARGET_V850    		((target_flags & MASK_CPU) == MASK_V850)

/* Whether to emit 2 byte per entry or 4 byte per entry switch tables.  */
#define TARGET_BIG_SWITCH (target_flags & MASK_BIG_SWITCH)

/* General debug flag */
#define TARGET_DEBUG (target_flags & MASK_DEBUG)

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES							\
  {{ "ghs",			 MASK_GHS, "Support Green Hills ABI" },	\
   { "no-ghs",			-MASK_GHS, "" },			\
   { "long-calls",		 MASK_LONG_CALLS, 			\
       				"Prohibit PC relative function calls" },\
   { "no-long-calls",		-MASK_LONG_CALLS, "" },			\
   { "ep",			 MASK_EP,				\
                                "Reuse r30 on a per function basis" },  \
   { "no-ep",			-MASK_EP, "" },				\
   { "prolog-function",		 MASK_PROLOG_FUNCTION, 			\
       				"Use stubs for function prologues" },	\
   { "no-prolog-function",	-MASK_PROLOG_FUNCTION, "" },		\
   { "space",			 MASK_EP | MASK_PROLOG_FUNCTION, 	\
       				"Same as: -mep -mprolog-function" },	\
   { "debug",			 MASK_DEBUG, "Enable backend debugging" }, \
   { "v850",		 	 MASK_V850,				\
                                "Compile for the v850 processor" },	\
   { "v850",		 	 -(MASK_V850 ^ MASK_CPU), "" },		\
   { "big-switch",		 MASK_BIG_SWITCH, 			\
       				"Use 4 byte entries in switch tables" },\
   { "",			 MASK_DEFAULT, ""}}

/* Information about the various small memory areas.  */
struct small_memory_info {
  char *name;
  const char *value;
  long max;
  long physical_max;
};

enum small_memory_type {
  /* tiny data area, using EP as base register */
  SMALL_MEMORY_TDA = 0,
  /* small data area using dp as base register */
  SMALL_MEMORY_SDA,
  /* zero data area using r0 as base register */
  SMALL_MEMORY_ZDA,
  SMALL_MEMORY_max
};

extern struct small_memory_info small_memory[(int)SMALL_MEMORY_max];

/* This macro is similar to `TARGET_SWITCHES' but defines names of
   command options that have values.  Its definition is an
   initializer with a subgrouping for each command option.

   Each subgrouping contains a string constant, that defines the
   fixed part of the option name, and the address of a variable.  The
   variable, type `char *', is set to the variable part of the given
   option if the fixed part matches.  The actual option name is made
   by appending `-m' to the specified name.

   Here is an example which defines `-mshort-data-NUMBER'.  If the
   given option is `-mshort-data-512', the variable `m88k_short_data'
   will be set to the string `"512"'.

          extern char *m88k_short_data;
          #define TARGET_OPTIONS \
           { { "short-data-", &m88k_short_data } } */

#define TARGET_OPTIONS							\
{									\
  { "tda=",	&small_memory[ (int)SMALL_MEMORY_TDA ].value,		\
      "Set the max size of data eligible for the TDA area"  },		\
  { "tda-",	&small_memory[ (int)SMALL_MEMORY_TDA ].value, "" },	\
  { "sda=",	&small_memory[ (int)SMALL_MEMORY_SDA ].value, 		\
      "Set the max size of data eligible for the SDA area"  },		\
  { "sda-",	&small_memory[ (int)SMALL_MEMORY_SDA ].value, "" },	\
  { "zda=",	&small_memory[ (int)SMALL_MEMORY_ZDA ].value, 		\
      "Set the max size of data eligible for the ZDA area"  },		\
  { "zda-",	&small_memory[ (int)SMALL_MEMORY_ZDA ].value, "" },	\
}

/* Sometimes certain combinations of command options do not make
   sense on a particular target machine.  You can define a macro
   `OVERRIDE_OPTIONS' to take account of this.  This macro, if
   defined, is executed once just after all the command options have
   been parsed.

   Don't use this macro to turn on various extra optimizations for
   `-O'.  That is what `OPTIMIZATION_OPTIONS' is for.  */
#define OVERRIDE_OPTIONS override_options ()


/* Show we can debug even without a frame pointer.  */
#define CAN_DEBUG_WITHOUT_FP

/* Some machines may desire to change what optimizations are
   performed for various optimization levels.   This macro, if
   defined, is executed once just after the optimization level is
   determined and before the remainder of the command options have
   been parsed.  Values set in this macro are used as the default
   values for the other command line options.

   LEVEL is the optimization level specified; 2 if `-O2' is
   specified, 1 if `-O' is specified, and 0 if neither is specified.

   SIZE is non-zero if `-Os' is specified, 0 otherwise.  

   You should not use this macro to change options that are not
   machine-specific.  These should uniformly selected by the same
   optimization level on all supported machines.  Use this macro to
   enable machine-specific optimizations.

   *Do not examine `write_symbols' in this macro!* The debugging
   options are not supposed to alter the generated code. */

#define OPTIMIZATION_OPTIONS(LEVEL,SIZE)				\
{									\
  if (LEVEL)								\
    target_flags |= (MASK_EP | MASK_PROLOG_FUNCTION);			\
}


/* Target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.
   This is not true on the NEC V850.  */
#define BITS_BIG_ENDIAN 0

/* Define this if most significant byte of a word is the lowest numbered.  */
/* This is not true on the NEC V850.  */
#define BYTES_BIG_ENDIAN 0

/* Define this if most significant word of a multiword number is lowest
   numbered.
   This is not true on the NEC V850.  */
#define WORDS_BIG_ENDIAN 0

/* Number of bits in an addressable storage unit */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD		32

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD		4

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE 		32

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases,
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.

   Some simple experiments have shown that leaving UNSIGNEDP alone
   generates the best overall code.  */

#define PROMOTE_MODE(MODE,UNSIGNEDP,TYPE)  \
  if (GET_MODE_CLASS (MODE) == MODE_INT \
      && GET_MODE_SIZE (MODE) < 4)      \
    { (MODE) = SImode; }

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY		32

/* The stack goes in 32 bit lumps.  */
#define STACK_BOUNDARY 		32

/* Allocation boundary (in *bits*) for the code of a function.
   16 is the minimum boundary; 32 would give better performance.  */
#define FUNCTION_BOUNDARY 16

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT	32

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* No structure field wants to be aligned rounder than this.  */
#define BIGGEST_FIELD_ALIGNMENT 32

/* Define this if move instructions will actually fail to work
   when given unaligned data.  */
#ifndef STRICT_ALIGNMENT 
#define STRICT_ALIGNMENT TARGET_V850
#endif

/* Define this as 1 if `char' should by default be signed; else as 0.

   On the NEC V850, loads do sign extension, so make this default. */
#define DEFAULT_SIGNED_CHAR 1

/* Define results of standard character escape sequences.  */
#define TARGET_BELL 007
#define TARGET_BS 010
#define TARGET_TAB 011
#define TARGET_NEWLINE 012
#define TARGET_VT 013
#define TARGET_FF 014
#define TARGET_CR 015

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.

   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.  */

#define FIRST_PSEUDO_REGISTER 34

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.  */

#define FIXED_REGISTERS \
  { 1, 1, 0, 1, 1, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 1, 0, \
    1, 1}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you
   like.  */

#define CALL_USED_REGISTERS \
  { 1, 1, 0, 1, 1, 1, 1, 1, \
    1, 1, 1, 1, 1, 1, 1, 1, \
    1, 1, 1, 1, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 1, 1, \
    1, 1}

/* List the order in which to allocate registers.  Each register must be
   listed once, even those in FIXED_REGISTERS.

   On the 850, we make the return registers first, then all of the volatile
   registers, then the saved registers in reverse order to better save the
   registers with an out of line function, and finally the fixed
   registers.  */

#define REG_ALLOC_ORDER							\
{									\
  10, 11,				/* return registers */		\
  12, 13, 14, 15, 16, 17, 18, 19,	/* scratch registers */		\
   6,  7,  8,  9, 31,			/* argument registers */	\
  29, 28, 27, 26, 25, 24, 23, 22,	/* saved registers */		\
  21, 20,  2,								\
   0,  1,  3,  4,  5, 30, 32, 33	/* fixed registers */		\
}

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.

   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.  */

#define HARD_REGNO_NREGS(REGNO, MODE)   \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Value is 1 if hard register REGNO can hold a value of machine-mode
   MODE.  */

#define HARD_REGNO_MODE_OK(REGNO, MODE) \
 ((((REGNO) & 1) == 0) || (GET_MODE_SIZE (MODE) <= 4))

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2) \
  (MODE1 == MODE2 || (GET_MODE_SIZE (MODE1) <= 4 && GET_MODE_SIZE (MODE2) <= 4))


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
{
  NO_REGS, GENERAL_REGS, ALL_REGS, LIM_REG_CLASSES
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES \
{ "NO_REGS", "GENERAL_REGS", "ALL_REGS", "LIM_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS  		\
{					\
  { 0x00000000 }, /* NO_REGS      */	\
  { 0xffffffff }, /* GENERAL_REGS */   	\
  { 0xffffffff }, /* ALL_REGS 	*/	\
}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO)  GENERAL_REGS

/* The class value for index registers, and the one for base regs.  */

#define INDEX_REG_CLASS NO_REGS
#define BASE_REG_CLASS  GENERAL_REGS

/* Get reg_class from a letter such as appears in the machine description.  */

#define REG_CLASS_FROM_LETTER(C) (NO_REGS)

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */
 
#define REGNO_OK_FOR_BASE_P(regno) \
  ((regno) < FIRST_PSEUDO_REGISTER || reg_renumber[regno] >= 0)

#define REGNO_OK_FOR_INDEX_P(regno) 0

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */

#define PREFERRED_RELOAD_CLASS(X,CLASS)  (CLASS)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */

#define CLASS_MAX_NREGS(CLASS, MODE)	\
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* The letters I, J, K, L, M, N, O, P in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.  */

#define INT_7_BITS(VALUE) ((unsigned) (VALUE) + 0x40 < 0x80)
#define INT_8_BITS(VALUE) ((unsigned) (VALUE) + 0x80 < 0x100)
/* zero */
#define CONST_OK_FOR_I(VALUE) ((VALUE) == 0)
/* 5 bit signed immediate */
#define CONST_OK_FOR_J(VALUE) ((unsigned) (VALUE) + 0x10 < 0x20)
/* 16 bit signed immediate */
#define CONST_OK_FOR_K(VALUE) ((unsigned) (VALUE) + 0x8000 < 0x10000)
/* valid constant for movhi instruction.  */
#define CONST_OK_FOR_L(VALUE) \
  (((unsigned) ((int) (VALUE) >> 16) + 0x8000 < 0x10000) \
   && CONST_OK_FOR_I ((VALUE & 0xffff)))
/* 16 bit unsigned immediate */
#define CONST_OK_FOR_M(VALUE) ((unsigned)(VALUE) < 0x10000)
/* 5 bit unsigned immediate in shift instructions */
#define CONST_OK_FOR_N(VALUE) ((unsigned) (VALUE) <= 31)

#define CONST_OK_FOR_O(VALUE) 0
#define CONST_OK_FOR_P(VALUE) 0


#define CONST_OK_FOR_LETTER_P(VALUE, C)  \
  ((C) == 'I' ? CONST_OK_FOR_I (VALUE) : \
   (C) == 'J' ? CONST_OK_FOR_J (VALUE) : \
   (C) == 'K' ? CONST_OK_FOR_K (VALUE) : \
   (C) == 'L' ? CONST_OK_FOR_L (VALUE) : \
   (C) == 'M' ? CONST_OK_FOR_M (VALUE) : \
   (C) == 'N' ? CONST_OK_FOR_N (VALUE) : \
   (C) == 'O' ? CONST_OK_FOR_O (VALUE) : \
   (C) == 'P' ? CONST_OK_FOR_P (VALUE) : \
   0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself. 
     
  `G' is a zero of some form.  */

#define CONST_DOUBLE_OK_FOR_G(VALUE)					\
  ((GET_MODE_CLASS (GET_MODE (VALUE)) == MODE_FLOAT			\
    && (VALUE) == CONST0_RTX (GET_MODE (VALUE)))			\
   || (GET_MODE_CLASS (GET_MODE (VALUE)) == MODE_INT			\
       && CONST_DOUBLE_LOW (VALUE) == 0					\
       && CONST_DOUBLE_HIGH (VALUE) == 0))

#define CONST_DOUBLE_OK_FOR_H(VALUE) 0

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)				\
  ((C) == 'G'   ? CONST_DOUBLE_OK_FOR_G (VALUE)				\
   : (C) == 'H' ? CONST_DOUBLE_OK_FOR_H (VALUE)				\
   : 0)


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

/* Offset of first parameter from the argument pointer register value.  */
/* Is equal to the size of the saved fp + pc, even if an fp isn't
   saved since the value is used before we know.  */

#define FIRST_PARM_OFFSET(FNDECL) 0

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 3

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 32

/* Register containing return address from latest function call.  */
#define LINK_POINTER_REGNUM 31
     
/* On some machines the offset between the frame pointer and starting
   offset of the automatic variables is not known until after register
   allocation has been done (for example, because the saved registers
   are between these two locations).  On those machines, define
   `FRAME_POINTER_REGNUM' the number of a special, fixed register to
   be used internally until the offset is known, and define
   `HARD_FRAME_POINTER_REGNUM' to be actual the hard register number
   used for the frame pointer.

   You should define this macro only in the very rare circumstances
   when it is not possible to calculate the offset between the frame
   pointer and the automatic variables until after register
   allocation has been completed.  When this macro is defined, you
   must also indicate in your definition of `ELIMINABLE_REGS' how to
   eliminate `FRAME_POINTER_REGNUM' into either
   `HARD_FRAME_POINTER_REGNUM' or `STACK_POINTER_REGNUM'.

   Do not define this macro if it would be the same as
   `FRAME_POINTER_REGNUM'. */
#undef  HARD_FRAME_POINTER_REGNUM 
#define HARD_FRAME_POINTER_REGNUM 29

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 33

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM 20

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
#define FRAME_POINTER_REQUIRED 0

/* If defined, this macro specifies a table of register pairs used to
   eliminate unneeded registers that point into the stack frame.  If
   it is not defined, the only elimination attempted by the compiler
   is to replace references to the frame pointer with references to
   the stack pointer.

   The definition of this macro is a list of structure
   initializations, each of which specifies an original and
   replacement register.

   On some machines, the position of the argument pointer is not
   known until the compilation is completed.  In such a case, a
   separate hard register must be used for the argument pointer.
   This register can be eliminated by replacing it with either the
   frame pointer or the argument pointer, depending on whether or not
   the frame pointer has been eliminated.

   In this case, you might specify:
        #define ELIMINABLE_REGS  \
        {{ARG_POINTER_REGNUM, STACK_POINTER_REGNUM}, \
         {ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM}, \
         {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}}

   Note that the elimination of the argument pointer with the stack
   pointer is specified first since that is the preferred elimination. */

#define ELIMINABLE_REGS							\
{{ FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM },			\
 { FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM },			\
 { ARG_POINTER_REGNUM,	 STACK_POINTER_REGNUM },			\
 { ARG_POINTER_REGNUM,   HARD_FRAME_POINTER_REGNUM }}			\

/* A C expression that returns non-zero if the compiler is allowed to
   try to replace register number FROM-REG with register number
   TO-REG.  This macro need only be defined if `ELIMINABLE_REGS' is
   defined, and will usually be the constant 1, since most of the
   cases preventing register elimination are things that the compiler
   already knows about. */

#define CAN_ELIMINATE(FROM, TO) \
 ((TO) == STACK_POINTER_REGNUM ? ! frame_pointer_needed : 1)

/* This macro is similar to `INITIAL_FRAME_POINTER_OFFSET'.  It
   specifies the initial difference between the specified pair of
   registers.  This macro must be defined if `ELIMINABLE_REGS' is
   defined. */

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
{									\
  if ((FROM) == FRAME_POINTER_REGNUM)					\
    (OFFSET) = get_frame_size () + current_function_outgoing_args_size;	\
  else if ((FROM) == ARG_POINTER_REGNUM)				\
   (OFFSET) = compute_frame_size (get_frame_size (), (long *)0);	\
  else									\
    abort ();								\
}

/* A guess for the V850.  */
#define PROMOTE_PROTOTYPES 1

/* Keep the stack pointer constant throughout the function.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* Value is the number of bytes of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.  */

#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE) 0


/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.  */

#define CUMULATIVE_ARGS struct cum_arg
struct cum_arg { int nbytes; };

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
  function_arg (&CUM, MODE, TYPE, NAMED)

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) \
  function_arg_partial_nregs (&CUM, MODE, TYPE, NAMED)

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,INDIRECT)	\
 ((CUM).nbytes = 0)

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)	\
 ((CUM).nbytes += ((MODE) != BLKmode			\
  ? (GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) & -UNITS_PER_WORD	\
  : (int_size_in_bytes (TYPE) + UNITS_PER_WORD - 1) & -UNITS_PER_WORD))

/* When a parameter is passed in a register, stack space is still
   allocated for it.  */
#define REG_PARM_STACK_SPACE(DECL) (!TARGET_GHS ? 16 : 0)

/* Define this if the above stack space is to be considered part of the
   space allocated by the caller.  */
#define OUTGOING_REG_PARM_STACK_SPACE

extern int current_function_anonymous_args;
/* Do any setup necessary for varargs/stdargs functions.  */
#define SETUP_INCOMING_VARARGS(CUM, MODE, TYPE, PAS, SECOND) \
  current_function_anonymous_args = (!TARGET_GHS ? 1 : 0);

/* Implement `va_arg'.  */
#define EXPAND_BUILTIN_VA_ARG(valist, type) \
  v850_va_arg (valist, type)

#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED)		\
  ((TYPE) && int_size_in_bytes (TYPE) > 8)
 
#define FUNCTION_ARG_CALLEE_COPIES(CUM, MODE, TYPE, NAMED) \
  ((TYPE) && int_size_in_bytes (TYPE) > 8)

/* 1 if N is a possible register number for function argument passing.  */

#define FUNCTION_ARG_REGNO_P(N) (N >= 6 && N <= 9)

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.   */
   
#define FUNCTION_VALUE(VALTYPE, FUNC) \
  gen_rtx_REG (TYPE_MODE (VALTYPE), 10)

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

#define LIBCALL_VALUE(MODE) \
  gen_rtx_REG (MODE, 10)

/* 1 if N is a possible register number for a function value.  */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == 10)

/* Return values > 8 bytes in length in memory.  */
#define DEFAULT_PCC_STRUCT_RETURN 0
#define RETURN_IN_MEMORY(TYPE)  \
  (int_size_in_bytes (TYPE) > 8 || TYPE_MODE (TYPE) == BLKmode)

/* Register in which address to store a structure value
   is passed to a function.  On the V850 it's passed as
   the first parameter.  */

#define STRUCT_VALUE 0

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 1

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO) ;

#define TRAMPOLINE_TEMPLATE(FILE)			\
  do {							\
    fprintf (FILE, "\tjarl .+4,r12\n");			\
    fprintf (FILE, "\tld.w 12[r12],r5\n");		\
    fprintf (FILE, "\tld.w 16[r12],r12\n");		\
    fprintf (FILE, "\tjmp [r12]\n");			\
    fprintf (FILE, "\tnop\n");				\
    fprintf (FILE, "\t.long 0\n");			\
    fprintf (FILE, "\t.long 0\n");			\
  } while (0)

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE 24

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)			\
{									\
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant ((TRAMP), 16)),	\
 		 (CXT));						\
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant ((TRAMP), 20)),	\
		 (FNADDR));						\
}

/* Addressing modes, and classification of registers for them.  */


/* 1 if X is an rtx for a constant that is a valid address.  */

/* ??? This seems too exclusive.  May get better code by accepting more
   possibilities here, in particular, should accept ZDA_NAME SYMBOL_REFs.  */

#define CONSTANT_ADDRESS_P(X)   \
  (GET_CODE (X) == CONST_INT				\
   && CONST_OK_FOR_K (INTVAL (X)))

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
#define REG_OK_FOR_INDEX_P(X) 0
/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) 1
#define REG_OK_FOR_INDEX_P_STRICT(X) 0
#define REG_OK_FOR_BASE_P_STRICT(X) REGNO_OK_FOR_BASE_P (REGNO (X))
#define STRICT 0

#else

/* Nonzero if X is a hard reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P(X) 0
/* Nonzero if X is a hard reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))
#define STRICT 1

#endif

/* A C expression that defines the optional machine-dependent
   constraint letters that can be used to segregate specific types of
   operands, usually memory references, for the target machine.
   Normally this macro will not be defined.  If it is required for a
   particular target machine, it should return 1 if VALUE corresponds
   to the operand type represented by the constraint letter C.  If C
   is not defined as an extra constraint, the value returned should
   be 0 regardless of VALUE.

   For example, on the ROMP, load instructions cannot have their
   output in r0 if the memory reference contains a symbolic address.
   Constraint letter `Q' is defined as representing a memory address
   that does *not* contain a symbolic address.  An alternative is
   specified with a `Q' constraint on the input and `r' on the
   output.  The next alternative specifies `m' on the input and a
   register class that does not include r0 on the output.  */

#define EXTRA_CONSTRAINT(OP, C)						\
 ((C) == 'Q'   ? ep_memory_operand (OP, GET_MODE (OP), 0)			\
  : (C) == 'R' ? special_symbolref_operand (OP, VOIDmode)		\
  : (C) == 'S' ? (GET_CODE (OP) == SYMBOL_REF && ! ZDA_NAME_P (XSTR (OP, 0))) \
  : (C) == 'T' ? 0							\
  : (C) == 'U' ? ((GET_CODE (OP) == SYMBOL_REF && ZDA_NAME_P (XSTR (OP, 0))) \
		  || (GET_CODE (OP) == CONST				\
		      && GET_CODE (XEXP (OP, 0)) == PLUS		\
		      && GET_CODE (XEXP (XEXP (OP, 0), 0)) == SYMBOL_REF \
		      && ZDA_NAME_P (XSTR (XEXP (XEXP (OP, 0), 0), 0)))) \
  : 0)

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.

   The other macros defined here are used only in GO_IF_LEGITIMATE_ADDRESS,
   except for CONSTANT_ADDRESS_P which is actually
   machine-independent.  */

/* Accept either REG or SUBREG where a register is valid.  */
  
#define RTX_OK_FOR_BASE_P(X)						\
  ((REG_P (X) && REG_OK_FOR_BASE_P (X))					\
   || (GET_CODE (X) == SUBREG && REG_P (SUBREG_REG (X))			\
       && REG_OK_FOR_BASE_P (SUBREG_REG (X))))

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)				\
do {									\
  if (RTX_OK_FOR_BASE_P (X)) goto ADDR;					\
  if (CONSTANT_ADDRESS_P (X)						\
      && (MODE == QImode || INTVAL (X) % 2 == 0)			\
      && (GET_MODE_SIZE (MODE) <= 4 || INTVAL (X) % 4 == 0))		\
    goto ADDR;								\
  if (GET_CODE (X) == LO_SUM						\
      && GET_CODE (XEXP (X, 0)) == REG					\
      && REG_OK_FOR_BASE_P (XEXP (X, 0))				\
      && CONSTANT_P (XEXP (X, 1))					\
      && (GET_CODE (XEXP (X, 1)) != CONST_INT				\
	  || ((MODE == QImode || INTVAL (XEXP (X, 1)) % 2 == 0)		\
	      && CONST_OK_FOR_K (INTVAL (XEXP (X, 1)))))		\
      && GET_MODE_SIZE (MODE) <= GET_MODE_SIZE (word_mode))		\
    goto ADDR;								\
  if (special_symbolref_operand (X, MODE)				\
      && (GET_MODE_SIZE (MODE) <= GET_MODE_SIZE (word_mode)))		\
     goto ADDR;								\
  if (GET_CODE (X) == PLUS						\
      && CONSTANT_ADDRESS_P (XEXP (X, 1))				\
      && (MODE == QImode || INTVAL (XEXP (X, 1)) % 2 == 0)		\
      && RTX_OK_FOR_BASE_P (XEXP (X, 0))) goto ADDR;			\
} while (0)


/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.   */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)  {}

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)  {}

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */

#define LEGITIMATE_CONSTANT_P(X)					\
  (GET_CODE (X) == CONST_DOUBLE						\
   || !(GET_CODE (X) == CONST						\
	&& GET_CODE (XEXP (X, 0)) == PLUS				\
	&& GET_CODE (XEXP (XEXP (X, 0), 0)) == SYMBOL_REF		\
	&& GET_CODE (XEXP (XEXP (X, 0), 1)) == CONST_INT		\
	&& ! CONST_OK_FOR_K (INTVAL (XEXP (XEXP (X, 0), 1)))))

/* In rare cases, correct code generation requires extra machine
   dependent processing between the second jump optimization pass and
   delayed branch scheduling.  On those machines, define this macro
   as a C statement to act on the code starting at INSN.  */

#define MACHINE_DEPENDENT_REORG(INSN) v850_reorg (INSN)


/* Tell final.c how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  No extra ones are needed for the vax.  */

/* Store in cc_status the expressions
   that the condition codes will describe
   after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

#define CC_OVERFLOW_UNUSABLE 0x200
#define CC_NO_CARRY CC_NO_OVERFLOW
#define NOTICE_UPDATE_CC(EXP, INSN) notice_update_cc(EXP, INSN)

/* A part of a C `switch' statement that describes the relative costs
   of constant RTL expressions.  It must contain `case' labels for
   expression codes `const_int', `const', `symbol_ref', `label_ref'
   and `const_double'.  Each case must ultimately reach a `return'
   statement to return the relative cost of the use of that kind of
   constant value in an expression.  The cost may depend on the
   precise value of the constant, which is available for examination
   in X, and the rtx code of the expression in which it is contained,
   found in OUTER_CODE.

   CODE is the expression code--redundant, since it can be obtained
   with `GET_CODE (X)'. */

#define CONST_COSTS(RTX,CODE,OUTER_CODE)				\
  case CONST_INT:							\
  case CONST_DOUBLE:							\
  case CONST:								\
  case SYMBOL_REF:							\
  case LABEL_REF:							\
    {									\
      int _zxy = const_costs(RTX, CODE);				\
      return (_zxy) ? COSTS_N_INSNS (_zxy) : 0;				\
    }

/* A crude cut at RTX_COSTS for the V850.  */

/* Provide the costs of a rtl expression.  This is in the body of a
   switch on CODE. 

   There aren't DImode MOD, DIV or MULT operations, so call them
   very expensive.  Everything else is pretty much a constant cost.  */

#define RTX_COSTS(RTX,CODE,OUTER_CODE)					\
  case MOD:								\
  case DIV:								\
    return 60;								\
  case MULT:								\
    return 20;

/* All addressing modes have the same cost on the V850 series.  */
#define ADDRESS_COST(ADDR) 1

/* Nonzero if access to memory by bytes or half words is no faster
   than accessing full words.  */
#define SLOW_BYTE_ACCESS 1

/* Define this if zero-extension is slow (more than one real instruction).  */
#define SLOW_ZERO_EXTEND 

/* According expr.c, a value of around 6 should minimize code size, and
   for the V850 series, that's our primary concern.  */
#define MOVE_RATIO 6

/* Indirect calls are expensive, never turn a direct call
   into an indirect call.  */
#define NO_FUNCTION_CSE

/* The four different data regions on the v850.  */
typedef enum 
{
  DATA_AREA_NORMAL,
  DATA_AREA_SDA,
  DATA_AREA_TDA,
  DATA_AREA_ZDA
} v850_data_area;

/* A list of names for sections other than the standard two, which are
   `in_text' and `in_data'.  You need not define this macro on a
   system with no other sections (that GCC needs to use).  */
#undef	EXTRA_SECTIONS
#define EXTRA_SECTIONS in_tdata, in_sdata, in_zdata, in_const, in_ctors, \
in_dtors, in_rozdata, in_rosdata, in_sbss, in_zbss, in_zcommon, in_scommon

/* One or more functions to be defined in `varasm.c'.  These
   functions should do jobs analogous to those of `text_section' and
   `data_section', for your additional sections.  Do not define this
   macro if you do not define `EXTRA_SECTIONS'. */
#undef	EXTRA_SECTION_FUNCTIONS

/* This could be done a lot more cleanly using ANSI C ... */
#define EXTRA_SECTION_FUNCTIONS						\
CONST_SECTION_FUNCTION							\
CTORS_SECTION_FUNCTION							\
DTORS_SECTION_FUNCTION							\
									\
void									\
sdata_section ()							\
{									\
  if (in_section != in_sdata)						\
    {									\
      fprintf (asm_out_file, "%s\n", SDATA_SECTION_ASM_OP);		\
      in_section = in_sdata;						\
    }									\
}									\
									\
void									\
rosdata_section ()							\
{									\
  if (in_section != in_rosdata)						\
    {									\
      fprintf (asm_out_file, "%s\n", ROSDATA_SECTION_ASM_OP);		\
      in_section = in_sdata;						\
    }									\
}									\
									\
void									\
sbss_section ()								\
{									\
  if (in_section != in_sbss)						\
    {									\
      fprintf (asm_out_file, "%s\n", SBSS_SECTION_ASM_OP);		\
      in_section = in_sbss;						\
    }									\
}									\
									\
void									\
tdata_section ()							\
{									\
  if (in_section != in_tdata)						\
    {									\
      fprintf (asm_out_file, "%s\n", TDATA_SECTION_ASM_OP);		\
      in_section = in_tdata;						\
    }									\
}									\
									\
void									\
zdata_section ()							\
{									\
  if (in_section != in_zdata)						\
    {									\
      fprintf (asm_out_file, "%s\n", ZDATA_SECTION_ASM_OP);		\
      in_section = in_zdata;						\
    }									\
}									\
									\
void									\
rozdata_section ()							\
{									\
  if (in_section != in_rozdata)						\
    {									\
      fprintf (asm_out_file, "%s\n", ROZDATA_SECTION_ASM_OP);		\
      in_section = in_rozdata;						\
    }									\
}									\
									\
void									\
zbss_section ()								\
{									\
  if (in_section != in_zbss)						\
    {									\
      fprintf (asm_out_file, "%s\n", ZBSS_SECTION_ASM_OP);		\
      in_section = in_zbss;						\
    }									\
}

#define TEXT_SECTION_ASM_OP  "\t.section .text"
#define DATA_SECTION_ASM_OP  "\t.section .data"
#define BSS_SECTION_ASM_OP   "\t.section .bss"
#define SDATA_SECTION_ASM_OP "\t.section .sdata,\"aw\""
#define SBSS_SECTION_ASM_OP  "\t.section .sbss,\"aw\""
#define ZDATA_SECTION_ASM_OP "\t.section .zdata,\"aw\""
#define ZBSS_SECTION_ASM_OP  "\t.section .zbss,\"aw\""
#define TDATA_SECTION_ASM_OP "\t.section .tdata,\"aw\""
#define ROSDATA_SECTION_ASM_OP "\t.section .rosdata,\"a\""
#define ROZDATA_SECTION_ASM_OP "\t.section .rozdata,\"a\""

#define SCOMMON_ASM_OP 	       ".scomm"
#define ZCOMMON_ASM_OP 	       ".zcomm"
#define TCOMMON_ASM_OP 	       ".tcomm"

/* A C statement or statements to switch to the appropriate section
   for output of EXP.  You can assume that EXP is either a `VAR_DECL'
   node or a constant of some sort.  RELOC indicates whether the
   initial value of EXP requires link-time relocations.  Select the
   section by calling `text_section' or one of the alternatives for
   other sections.

   Do not define this macro if you put all read-only variables and
   constants in the read-only data section (usually the text section).  */
#undef  SELECT_SECTION
#define SELECT_SECTION(EXP, RELOC)					\
do {									\
  if (TREE_CODE (EXP) == VAR_DECL)					\
    {									\
      int is_const;							\
      if (!TREE_READONLY (EXP)						\
	  || TREE_SIDE_EFFECTS (EXP)					\
	  || !DECL_INITIAL (EXP)					\
	  || (DECL_INITIAL (EXP) != error_mark_node			\
	      && !TREE_CONSTANT (DECL_INITIAL (EXP))))			\
        is_const = FALSE;						\
      else								\
        is_const = TRUE;						\
									\
      switch (v850_get_data_area (EXP))					\
        {								\
        case DATA_AREA_ZDA:						\
	  if (is_const)	        					\
	    rozdata_section ();						\
	  else								\
	    zdata_section ();						\
	  break;							\
									\
        case DATA_AREA_TDA:						\
	  tdata_section ();						\
	  break;							\
									\
        case DATA_AREA_SDA:						\
	  if (is_const)		                        		\
	    rosdata_section ();						\
	  else								\
	    sdata_section ();						\
	  break;							\
									\
        default:							\
          if (is_const)							\
	    const_section ();						\
	  else								\
	    data_section ();						\
	  break;							\
        }								\
    }									\
  else if (TREE_CODE (EXP) == STRING_CST)				\
    {									\
      if (! flag_writable_strings)					\
	const_section ();						\
      else								\
	data_section ();						\
    }									\
									\
  else									\
    const_section ();							\
									\
} while (0)

/* A C statement or statements to switch to the appropriate section
   for output of RTX in mode MODE.  You can assume that RTX is some
   kind of constant in RTL.  The argument MODE is redundant except in
   the case of a `const_int' rtx.  Select the section by calling
   `text_section' or one of the alternatives for other sections.

   Do not define this macro if you put all constants in the read-only
   data section.  */
/* #define SELECT_RTX_SECTION(MODE, RTX) */

/* Output at beginning/end of assembler file.  */
#undef ASM_FILE_START
#define ASM_FILE_START(FILE) asm_file_start(FILE)

#define ASM_COMMENT_START "#"

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON "#APP\n"

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF "#NO_APP\n"

/* This is how to output an assembler line defining a `double' constant.
   It is .double or .float, depending.  */

#define ASM_OUTPUT_DOUBLE(FILE, VALUE)			\
do { char dstr[30];					\
     REAL_VALUE_TO_DECIMAL ((VALUE), "%.20e", dstr);	\
     fprintf (FILE, "\t.double %s\n", dstr);		\
   } while (0)


/* This is how to output an assembler line defining a `float' constant.  */
#define ASM_OUTPUT_FLOAT(FILE, VALUE)			\
do { char dstr[30];					\
     REAL_VALUE_TO_DECIMAL ((VALUE), "%.20e", dstr);	\
     fprintf (FILE, "\t.float %s\n", dstr);		\
   } while (0)

/* This is how to output an assembler line defining an `int' constant.  */

#define ASM_OUTPUT_INT(FILE, VALUE)		\
( fprintf (FILE, "\t.long "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* Likewise for `char' and `short' constants.  */

#define ASM_OUTPUT_SHORT(FILE, VALUE)		\
( fprintf (FILE, "\t.hword "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

#define ASM_OUTPUT_CHAR(FILE, VALUE)		\
( fprintf (FILE, "\t.byte "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* This is how to output an assembler line for a numeric constant byte.  */
#define ASM_OUTPUT_BYTE(FILE, VALUE)  \
  fprintf (FILE, "\t.byte 0x%x\n", (VALUE))

/* Define the parentheses used to group arithmetic operations
   in assembler code.  */

#define ASM_OPEN_PAREN "("
#define ASM_CLOSE_PAREN ")"

/* This says how to output the assembler to define a global
   uninitialized but not common symbol.  */

#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss ((FILE), (DECL), (NAME), (SIZE), (ALIGN))

#undef  ASM_OUTPUT_ALIGNED_BSS 
#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  v850_output_aligned_bss (FILE, DECL, NAME, SIZE, ALIGN)

/* This says how to output the assembler to define a global
   uninitialized, common symbol. */
#undef  ASM_OUTPUT_ALIGNED_COMMON
#undef  ASM_OUTPUT_COMMON
#define ASM_OUTPUT_ALIGNED_DECL_COMMON(FILE, DECL, NAME, SIZE, ALIGN) \
     v850_output_common (FILE, DECL, NAME, SIZE, ALIGN)

/* This says how to output the assembler to define a local
   uninitialized symbol. */
#undef  ASM_OUTPUT_ALIGNED_LOCAL
#undef  ASM_OUTPUT_LOCAL
#define ASM_OUTPUT_ALIGNED_DECL_LOCAL(FILE, DECL, NAME, SIZE, ALIGN) \
     v850_output_local (FILE, DECL, NAME, SIZE, ALIGN)
     
/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#define ASM_OUTPUT_LABEL(FILE, NAME)	\
  do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0)

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(FILE, NAME)	\
  do { fputs ("\t.global ", FILE); assemble_name (FILE, NAME); fputs ("\n", FILE);} while (0)

/* This is how to output a reference to a user-level label named NAME.
   `assemble_name' uses this.  */

#undef ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(FILE, NAME)	          \
  do {                                            \
  const char * real_name;                         \
  STRIP_NAME_ENCODING (real_name, (NAME));        \
  fprintf (FILE, "_%s", real_name);               \
  } while (0)           

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),	\
  sprintf ((OUTPUT), "%s___%d", (NAME), (LABELNO)))

/* This is how we tell the assembler that two symbols have the same value.  */

#define ASM_OUTPUT_DEF(FILE,NAME1,NAME2) \
  do { assemble_name(FILE, NAME1); 	 \
       fputs(" = ", FILE);		 \
       assemble_name(FILE, NAME2);	 \
       fputc('\n', FILE); } while (0)


/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES							\
{  "r0",  "r1",  "r2",  "sp",  "gp",  "r5",  "r6" , "r7",		\
   "r8",  "r9", "r10", "r11", "r12", "r13", "r14", "r15",		\
  "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23",		\
  "r24", "r25", "r26", "r27", "r28", "r29",  "ep", "r31",		\
  ".fp", ".ap"}

#define ADDITIONAL_REGISTER_NAMES					\
{ { "zero",	0 },							\
  { "hp",	2 },							\
  { "r3",	3 },							\
  { "r4",	4 },							\
  { "tp",	5 },							\
  { "fp",	29 },							\
  { "r30",	30 },							\
  { "lp",	31} }

/* Print an instruction operand X on file FILE.
   look in v850.c for details */

#define PRINT_OPERAND(FILE, X, CODE)  print_operand (FILE, X, CODE)

#define PRINT_OPERAND_PUNCT_VALID_P(CODE) \
  ((CODE) == '.')

/* Print a memory operand whose address is X, on file FILE.
   This uses a function in output-vax.c.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR) print_operand_address (FILE, ADDR)

#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)
#define ASM_OUTPUT_REG_POP(FILE,REGNO)

/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE) \
  asm_fprintf (FILE, "\t%s .L%d\n",					\
	       (TARGET_BIG_SWITCH ? ".long" : ".short"), VALUE)

/* This is how to output an element of a case-vector that is relative.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL) \
  fprintf (FILE, "\t%s .L%d-.L%d\n",					\
	   (TARGET_BIG_SWITCH ? ".long" : ".short"),			\
	   VALUE, REL)

#define ASM_OUTPUT_ALIGN(FILE, LOG)	\
  if ((LOG) != 0)			\
    fprintf (FILE, "\t.align %d\n", (LOG))

/* We don't have to worry about dbx compatibility for the v850.  */
#define DEFAULT_GDB_EXTENSIONS 1

/* Use stabs debugging info by default.  */
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

#define DBX_REGISTER_NUMBER(REGNO) REGNO

/* Define to use software floating point emulator for REAL_ARITHMETIC and
   decimal <-> binary conversion. */
#define REAL_ARITHMETIC

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE (TARGET_BIG_SWITCH ? SImode : HImode)

/* Define this if the case instruction drops through after the table
   when the index is out of range.  Don't define it if the case insn
   jumps to the default label instead.  */
/* #define CASE_DROPS_THROUGH */

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses. */
#define CASE_VECTOR_PC_RELATIVE 1

/* The switch instruction requires that the jump table immediately follow
   it. */
#define JUMP_TABLES_IN_TEXT_SECTION 1

/* svr4.h defines this assuming that 4 byte alignment is required.  */
#undef ASM_OUTPUT_BEFORE_CASE_LABEL
#define ASM_OUTPUT_BEFORE_CASE_LABEL(FILE,PREFIX,NUM,TABLE) \
  ASM_OUTPUT_ALIGN ((FILE), (TARGET_BIG_SWITCH ? 2 : 1));

#define WORD_REGISTER_OPERATIONS

/* Byte and short loads sign extend the value to a word.  */
#define LOAD_EXTEND_OP(MODE) SIGN_EXTEND

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This flag, if defined, says the same insns that convert to a signed fixnum
   also convert validly to an unsigned one.  */
#define FIXUNS_TRUNC_LIKE_FIX_TRUNC

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX	4

/* Define if shifts truncate the shift count
   which implies one can omit a sign-extension or zero-extension
   of a shift count.  */
#define SHIFT_COUNT_TRUNCATED 1

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

#define STORE_FLAG_VALUE 1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode SImode

/* A function address in a call instruction
   is a byte address (for indexing purposes)
   so give the MEM rtx a byte's mode.  */
#define FUNCTION_MODE QImode

/* A C expression whose value is nonzero if IDENTIFIER with arguments ARGS
   is a valid machine specific attribute for DECL.
   The attributes in ATTRIBUTES have previously been assigned to DECL.  */
#define VALID_MACHINE_DECL_ATTRIBUTE(DECL, ATTRIBUTES, IDENTIFIER, ARGS) \
  v850_valid_machine_decl_attribute (DECL, IDENTIFIER, ARGS)

/* A C statement that assigns default attributes to a newly created DECL.  */
#define SET_DEFAULT_DECL_ATTRIBUTES(decl, attr) \
     v850_set_default_decl_attr (decl)

/* Tell compiler we want to support GHS pragmas */
#define HANDLE_PRAGMA(get, unget, name) v850_handle_pragma (get, unget, name)

enum v850_pragma_state
{
  V850_PS_START,
  V850_PS_SHOULD_BE_DONE,
  V850_PS_BAD,
  V850_PS_MAYBE_SECTION_NAME,
  V850_PS_EXPECTING_EQUALS,
  V850_PS_EXPECTING_SECTION_ALIAS,
  V850_PS_MAYBE_COMMA
};

enum v850_pragma_type
{
  V850_PT_UNKNOWN,
  V850_PT_INTERRUPT,
  V850_PT_SECTION,
  V850_PT_START_SECTION,
  V850_PT_END_SECTION
};

/* enum GHS_SECTION_KIND is an enumeration of the kinds of sections that
   can appear in the "ghs section" pragma.  These names are used to index
   into the GHS_default_section_names[] and GHS_current_section_names[]
   that are defined in v850.c, and so the ordering of each must remain
   consistant. 

   These arrays give the default and current names for each kind of 
   section defined by the GHS pragmas.  The current names can be changed
   by the "ghs section" pragma.  If the current names are null, use 
   the default names.  Note that the two arrays have different types.

   For the *normal* section kinds (like .data, .text, etc.) we do not
   want to explicitly force the name of these sections, but would rather
   let the linker (or at least the back end) choose the name of the 
   section, UNLESS the user has force a specific name for these section
   kinds.  To accomplish this set the name in ghs_default_section_names
   to null.  */

enum GHS_section_kind
{ 
  GHS_SECTION_KIND_DEFAULT,

  GHS_SECTION_KIND_TEXT,
  GHS_SECTION_KIND_DATA, 
  GHS_SECTION_KIND_RODATA,
  GHS_SECTION_KIND_BSS,
  GHS_SECTION_KIND_SDATA,
  GHS_SECTION_KIND_ROSDATA,
  GHS_SECTION_KIND_TDATA,
  GHS_SECTION_KIND_ZDATA,
  GHS_SECTION_KIND_ROZDATA,

  COUNT_OF_GHS_SECTION_KINDS  /* must be last */
};

/* The assembler op to start the file.  */

#define FILE_ASM_OP "\t.file\n"

/* Enable the register move pass to improve code.  */
#define ENABLE_REGMOVE_PASS


/* Implement ZDA, TDA, and SDA */

#define EP_REGNUM 30	/* ep register number */

#define ENCODE_SECTION_INFO(DECL)			\
  do							\
    {							\
      if ((TREE_STATIC (DECL) || DECL_EXTERNAL (DECL))	\
	  && TREE_CODE (DECL) == VAR_DECL)		\
	v850_encode_data_area (DECL);			\
    }							\
  while (0)

#define ZDA_NAME_FLAG_CHAR '@'
#define TDA_NAME_FLAG_CHAR '%'
#define SDA_NAME_FLAG_CHAR '&'

#define ZDA_NAME_P(NAME) (*(NAME) == ZDA_NAME_FLAG_CHAR)
#define TDA_NAME_P(NAME) (*(NAME) == TDA_NAME_FLAG_CHAR)
#define SDA_NAME_P(NAME) (*(NAME) == SDA_NAME_FLAG_CHAR)

#define ENCODED_NAME_P(SYMBOL_NAME)    \
  (ZDA_NAME_P (SYMBOL_NAME)            \
   || TDA_NAME_P (SYMBOL_NAME)         \
   || SDA_NAME_P (SYMBOL_NAME))

#define STRIP_NAME_ENCODING(VAR,SYMBOL_NAME) \
     (VAR) = (SYMBOL_NAME) + (ENCODED_NAME_P (SYMBOL_NAME) || *(SYMBOL_NAME) == '*')

/* Define this if you have defined special-purpose predicates in the
   file `MACHINE.c'.  This macro is called within an initializer of an
   array of structures.  The first field in the structure is the name
   of a predicate and the second field is an array of rtl codes.  For
   each predicate, list all rtl codes that can be in expressions
   matched by the predicate.  The list should have a trailing comma.  */

#define PREDICATE_CODES							\
{ "ep_memory_operand",		{ MEM }},				\
{ "reg_or_0_operand",		{ REG, SUBREG, CONST_INT, CONST_DOUBLE }}, \
{ "reg_or_int5_operand",	{ REG, SUBREG, CONST_INT }},		\
{ "call_address_operand",	{ REG, SYMBOL_REF }},			\
{ "movsi_source_operand",	{ LABEL_REF, SYMBOL_REF, CONST_INT,	\
				  CONST_DOUBLE, CONST, HIGH, MEM,	\
				  REG, SUBREG }},			\
{ "special_symbolref_operand",	{ SYMBOL_REF }},			\
{ "power_of_two_operand",	{ CONST_INT }},				\
{ "pattern_is_ok_for_prologue",	{ PARALLEL }},				\
{ "pattern_is_ok_for_epilogue",	{ PARALLEL }},				\
{ "register_is_ok_for_epilogue",{ REG }},				\
{ "not_power_of_two_operand",	{ CONST_INT }},
  
