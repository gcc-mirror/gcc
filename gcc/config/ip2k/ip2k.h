/* Definitions of target machine for GNU compiler,
   For Ubicom IP2022 Communications Controller

   Copyright (C) 2000, 2001, 2002, 2003 Free Software Foundation, Inc.
   Contributed by Red Hat, Inc and Ubicom, Inc.

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


/* Set up System V.4 (aka ELF) defaults.  */

#include "elfos.h"
#undef ASM_SPEC			/* But we have a GAS assembler.  */

#define CPP_PREDEFINES \
  "-DIP2K -D_DOUBLE_IS_32BITS -D__BUFSIZ__=512 -D__FILENAME_MAX__=128"
/* Define this to be a string constant containing `-D' options to
   define the predefined macros that identify this machine and system.
   These macros will be predefined unless the `-ansi' option is
   specified.

   In addition, a parallel set of macros are predefined, whose names
   are made by appending `__' at the beginning and at the end.  These
   `__' macros are permitted by the ANSI standard, so they are
   predefined regardless of whether `-ansi' is specified.

   For example, on the Sun, one can use the following value:

   "-Dmc68000 -Dsun -Dunix"

   The result is to define the macros `__mc68000__', `__sun__' and
   `__unix__' unconditionally, and the macros `mc68000', `sun' and
   `unix' provided `-ansi' is not specified.  */


/* This declaration should be present.  */
extern int target_flags;

/* `TARGET_...'
   This series of macros is to allow compiler command arguments to
   enable or disable the use of optional features of the target
   machine.  For example, one machine description serves both the
   68000 and the 68020; a command argument tells the compiler whether
   it should use 68020-only instructions or not.  This command
   argument works by means of a macro `TARGET_68020' that tests a bit
   in `target_flags'.

   Define a macro `TARGET_FEATURENAME' for each such option.  Its
   definition should test a bit in `target_flags'; for example:

   #define TARGET_68020 (target_flags & 1)

   One place where these macros are used is in the
   condition-expressions of instruction patterns.  Note how
   `TARGET_68020' appears frequently in the 68000 machine description
   file, `m68k.md'.  Another place they are used is in the
   definitions of the other macros in the `MACHINE.h' file.  */



#define TARGET_SWITCHES {{"",0, NULL}}
/* This macro defines names of command options to set and clear bits
   in `target_flags'.  Its definition is an initializer with a
   subgrouping for each command option.

   Each subgrouping contains a string constant, that defines the
   option name, and a number, which contains the bits to set in
   `target_flags'.  A negative number says to clear bits instead; the
   negative of the number is which bits to clear.  The actual option
   name is made by appending `-m' to the specified name.

   One of the subgroupings should have a null string.  The number in
   this grouping is the default value for `target_flags'.  Any target
   options act starting with that value.

   Here is an example which defines `-m68000' and `-m68020' with
   opposite meanings, and picks the latter as the default:

   #define TARGET_SWITCHES \
   { { "68020", 1},      \
   { "68000", -1},     \
   { "", 1}}  */


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
   { { "short-data-", &m88k_short_data } }  */

#define TARGET_VERSION fprintf (stderr, " (ip2k, GNU assembler syntax)")
/* This macro is a C statement to print on `stderr' a string
   describing the particular machine description choice.  Every
   machine description should define `TARGET_VERSION'. For example:

   #ifdef MOTOROLA
   #define TARGET_VERSION \
   fprintf (stderr, " (68k, Motorola syntax)")
   #else
   #define TARGET_VERSION \
   fprintf (stderr, " (68k, MIT syntax)")
   #endif  */

/* Caller-saves is not a win for the IP2K.  Pretty much anywhere that
   a register is permitted allows SP-relative addresses too.

   This machine doesn't have PIC addressing modes, so disable that also.  */

#define OVERRIDE_OPTIONS	\
    do {			\
	flag_caller_saves = 0;	\
	flag_pic = 0;		\
    } while (0)

/* `OVERRIDE_OPTIONS'
   Sometimes certain combinations of command options do not make
   sense on a particular target machine.  You can define a macro
   `OVERRIDE_OPTIONS' to take account of this.  This macro, if
   defined, is executed once just after all the command options have
   been parsed.

   Don't use this macro to turn on various extra optimizations for
   `-O'.  That is what `OPTIMIZATION_OPTIONS' is for.  */

/* Put each function in its own section so that PAGE-instruction
   relaxation can do its best.  */
#define OPTIMIZATION_OPTIONS(LEVEL, SIZEFLAG)	\
    do {					\
	if ((LEVEL) || (SIZEFLAG))		\
	    flag_function_sections = 1;	\
    } while (0)

/* Define this if most significant byte of a word is the lowest numbered.  */
#define BITS_BIG_ENDIAN 0

/* Define this if most significant byte of a word is the lowest numbered.  */
#define BYTES_BIG_ENDIAN 1

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
#define WORDS_BIG_ENDIAN 1

/* Number of bits in an addressable storage unit.  */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';  */
#define BITS_PER_WORD 8

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD (BITS_PER_WORD / BITS_PER_UNIT)

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE 16

/* Maximum sized of reasonable data type DImode or Dfmode ...  */
#define MAX_FIXED_MODE_SIZE 64

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 8

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 16

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 8

/* No data type wants to be aligned rounder than this.  */

#define BIGGEST_ALIGNMENT 8

#define STRICT_ALIGNMENT 0

#define PCC_BITFIELD_TYPE_MATTERS 1

/* A C expression for the size in bits of the type `int' on the
     target machine.  If you don't define this, the default is one word.  */
#undef INT_TYPE_SIZE
#define INT_TYPE_SIZE 16


/* A C expression for the size in bits of the type `short' on the
   target machine.  If you don't define this, the default is half a
   word.  (If this would be less than one storage unit, it is rounded
   up to one unit.)  */
#undef SHORT_TYPE_SIZE
#define SHORT_TYPE_SIZE 16

/* A C expression for the size in bits of the type `long' on the
   target machine.  If you don't define this, the default is one word.  */
#undef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE 32


/* Maximum number for the size in bits of the type `long' on the
   target machine.  If this is undefined, the default is
   `LONG_TYPE_SIZE'.  Otherwise, it is the constant value that is the
   largest value that `LONG_TYPE_SIZE' can have at run-time.  This is
   used in `cpp'.  */
#define MAX_LONG_TYPE_SIZE 32

/* A C expression for the size in bits of the type `long long' on the
   target machine.  If you don't define this, the default is two
   words.  If you want to support GNU Ada on your machine, the value
   of macro must be at least 64.  */
#undef LONG_LONG_TYPE_SIZE
#define LONG_LONG_TYPE_SIZE	64

#undef CHAR_TYPE_SIZE
#define  CHAR_TYPE_SIZE 8
/* A C expression for the size in bits of the type `char' on the
   target machine.  If you don't define this, the default is one
   quarter of a word.  (If this would be less than one storage unit,
   it is rounded up to one unit.)  */

#undef FLOAT_TYPE_SIZE
#define FLOAT_TYPE_SIZE 32
/* A C expression for the size in bits of the type `float' on the
   target machine.  If you don't define this, the default is one word.  */

#undef DOUBLE_TYPE_SIZE
#define DOUBLE_TYPE_SIZE 32
/* A C expression for the size in bits of the type `double' on the
   target machine.  If you don't define this, the default is two
   words.  */


/* A C expression for the size in bits of the type `long double' on
   the target machine.  If you don't define this, the default is two
   words.  */
#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE	32

#define DEFAULT_SIGNED_CHAR 1
/* An expression whose value is 1 or 0, according to whether the type
   `char' should be signed or unsigned by default.  The user can
   always override this default with the options `-fsigned-char' and
   `-funsigned-char'.  */

/* #define DEFAULT_SHORT_ENUMS	1
   This was the default for the IP2k but gcc has a bug (as of 17th May
   2001) in the way that library calls to the memory checker functions
   are issues that screws things up if an enum is not equivalent to
   an int.  */
/* `DEFAULT_SHORT_ENUMS'
   A C expression to determine whether to give an `enum' type only as
   many bytes as it takes to represent the range of possible values
   of that type.  A nonzero value means to do that; a zero value
   means all `enum' types should be allocated like `int'.

   If you don't define the macro, the default is 0.  */

#define SIZE_TYPE "unsigned int"
/* A C expression for a string describing the name of the data type
   to use for size values.  The typedef name `size_t' is defined
   using the contents of the string.
   
   The string can contain more than one keyword.  If so, separate
   them with spaces, and write first any length keyword, then
   `unsigned' if appropriate, and finally `int'.  The string must
   exactly match one of the data type names defined in the function
   `init_decl_processing' in the file `c-decl.c'.  You may not omit
   `int' or change the order--that would cause the compiler to crash
   on startup.
   
   If you don't define this macro, the default is `"long unsigned
   int"'.  */

#define PTRDIFF_TYPE "int"
/* A C expression for a string describing the name of the data type
   to use for the result of subtracting two pointers.  The typedef
   name `ptrdiff_t' is defined using the contents of the string.  See
   `SIZE_TYPE' above for more information.
   
   If you don't define this macro, the default is `"long int"'.  */

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"
#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE	16
/* A C expression for the size in bits of the data type for wide
   characters.  This is used in `cpp', which cannot make use of
   `WCHAR_TYPE'.  */

#define HARD_REG_SIZE           (UNITS_PER_WORD)
/* Standard register usage.

   for the IP2K, we are going to have a LOT of registers, but only some of them
   are named.  */
 
#define FIRST_PSEUDO_REGISTER (0x104) /* Skip over physical regs, VFP, AP.  */

/* Number of hardware registers known to the compiler.  They receive
   numbers 0 through `FIRST_PSEUDO_REGISTER-1'; thus, the first
   pseudo register's number really is assigned the number
   `FIRST_PSEUDO_REGISTER'.  */

#define REG_IP		0x4
#define REG_IPH		REG_IP
#define REG_IPL		0x5

#define REG_SP		0x6
#define REG_SPH		REG_SP
#define REG_SPL		0x7

#define REG_PCH		0x8
#define REG_PCL		0x9

#define REG_W		0xa
#define REG_STATUS	0xb

#define REG_DP		0xc
#define REG_DPH		REG_DP
#define REG_DPL		0xd

#define REG_MULH	0xf

#define REG_CALLH	0x7e		/* Call-stack readout.  */
#define REG_CALLL	0x7f


#define REG_RESULT	0x80	/* Result register (upto 8 bytes).  */
#define REG_FP		0xfd	/* 2 bytes for FRAME chain  */

#define REG_ZERO	0xff	/* Initialized to zero by runtime.  */

#define REG_VFP		0x100	/* Virtual frame pointer.  */
#define REG_AP		0x102	/* Virtual arg pointer.  */

/* Status register bits.  */
#define Z_FLAG	0x2	 
#define DC_FLAG	0x1
#define C_FLAG	0x0

#define FIXED_REGISTERS {\
1,1,1,1,0,0,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/*  r0.. r31*/\
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/* r32.. r63*/\
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/* r64.. r95*/\
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/* r96..r127*/\
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,/*r128..r159*/\
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/*r160..r191*/\
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/*r192..r223*/\
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/*r224..r255*/\
1,1,1,1}

/* An initializer that says which registers are used for fixed
   purposes all throughout the compiled code and are therefore not
   available for general allocation.  These would include the stack
   pointer, the frame pointer (except on machines where that can be
   used as a general register when no frame pointer is needed), the
   program counter on machines where that is considered one of the
   addressable registers, and any other numbered register with a
   standard use.

   This information is expressed as a sequence of numbers, separated
   by commas and surrounded by braces.  The Nth number is 1 if
   register N is fixed, 0 otherwise.

   The table initialized from this macro, and the table initialized by
   the following one, may be overridden at run time either
   automatically, by the actions of the macro
   `CONDITIONAL_REGISTER_USAGE', or by the user with the command
   options `-ffixed-REG', `-fcall-used-REG' and `-fcall-saved-REG'.  */

#define CALL_USED_REGISTERS {			\
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/*  r0.. r31*/\
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/* r32.. r63*/\
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/* r64.. r95*/\
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/* r96..r127*/\
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/*r128..r159*/\
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/*r160..r191*/\
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/*r192..r223*/\
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/*r224..r255*/\
1,1,1,1}

/* Like `FIXED_REGISTERS' but has 1 for each register that is
   clobbered (in general) by function calls as well as for fixed
   registers.  This macro therefore identifies the registers that are
   not available for general allocation of values that must live
   across function calls.

   If a register has 0 in `CALL_USED_REGISTERS', the compiler
   automatically saves it on function entry and restores it on
   function exit, if the register is used within the function.  */

#define NON_SAVING_SETJMP 0
/* If this macro is defined and has a nonzero value, it means that
   `setjmp' and related functions fail to save the registers, or that
   `longjmp' fails to restore them.  To compensate, the compiler
   avoids putting variables in registers in functions that use
   `setjmp'.  */

#define REG_ALLOC_ORDER {			\
    0x88,0x89,0x8a,0x8b,0x8c,0x8d,0x8e,0x8f,	\
    0x90,0x91,0x92,0x93,0x94,0x95,0x96,0x97,	\
    0x98,0x99,0x9a,0x9b,0x9c,0x9d,0x9e,0x9f,	\
    0x80,0x81,0x82,0x83,0x84,0x85,0x86,0x87,	\
    0xa0,0xa1,0xa2,0xa3,0xa4,0xa5,0xa6,0xa7,	\
    0xa8,0xa9,0xaa,0xab,0xac,0xad,0xae,0xaf,	\
    0xb0,0xb1,0xb2,0xb3,0xb4,0xb5,0xb6,0xb7,	\
    0xb8,0xb9,0xba,0xbb,0xbc,0xbd,0xbe,0xbf,	\
    0xc0,0xc1,0xc2,0xc3,0xc4,0xc5,0xc6,0xc7,	\
    0xc8,0xc9,0xca,0xcb,0xcc,0xcd,0xce,0xcf,	\
    0xd0,0xd1,0xd2,0xd3,0xd4,0xd5,0xd6,0xd7,	\
    0xd8,0xd9,0xda,0xdb,0xdc,0xdd,0xde,0xdf,	\
    0xe0,0xe1,0xe2,0xe3,0xe4,0xe5,0xe6,0xe7,	\
    0xe8,0xe9,0xea,0xeb,0xec,0xed,0xee,0xef,	\
    0xf0,0xf1,0xf2,0xf3,0xf4,0xf5,0xf6,0xf7,	\
    0xf8,0xf9,0xfa,0xfb,0xfc,0xfd,0xfe,0xff,	\
    0x00,0x01,0x02,0x03,0x0c,0x0d,0x06,0x07,	\
    0x08,0x09,0x0a,0x0b,0x04,0x05,0x0e,0x0f,	\
    0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,	\
    0x18,0x19,0x1a,0x1b,0x1c,0x1d,0x1e,0x1f,	\
    0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,	\
    0x28,0x29,0x2a,0x2b,0x2c,0x2d,0x2e,0x2f,	\
    0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,	\
    0x38,0x39,0x3a,0x3b,0x3c,0x3d,0x3e,0x3f,	\
    0x40,0x41,0x42,0x43,0x44,0x45,0x46,0x47,	\
    0x48,0x49,0x4a,0x4b,0x4c,0x4d,0x4e,0x4f,	\
    0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,	\
    0x58,0x59,0x5a,0x5b,0x5c,0x5d,0x5e,0x5f,	\
    0x60,0x61,0x62,0x63,0x64,0x65,0x66,0x67,	\
    0x68,0x69,0x6a,0x6b,0x6c,0x6d,0x6e,0x6f,	\
    0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,	\
    0x78,0x79,0x7a,0x7b,0x7c,0x7d,0x7e,0x7f,	\
    0x100,0x101,0x102,0x103}

/* If defined, an initializer for a vector of integers, containing the
   numbers of hard registers in the order in which GNU CC should
   prefer to use them (from most preferred to least).
   
   If this macro is not defined, registers are used lowest numbered
   first (all else being equal).
   
   One use of this macro is on machines where the highest numbered
   registers must always be saved and the save-multiple-registers
   instruction supports only sequences of consecutive registers.  On
   such machines, define `REG_ALLOC_ORDER' to be an initializer that
   lists the highest numbered allocatable register first.  */

#define ORDER_REGS_FOR_LOCAL_ALLOC ip2k_init_local_alloc (reg_alloc_order)
/* A C statement (sans semicolon) to choose the order in which to
   allocate hard registers for pseudo-registers local to a basic
   block.

   Store the desired register order in the array `reg_alloc_order'.
   Element 0 should be the register to allocate first; element 1, the
   next register; and so on.

   The macro body should not assume anything about the contents of
   `reg_alloc_order' before execution of the macro.

   On most machines, it is not necessary to define this macro.  */

/* Are we allowed to rename registers?  For some reason, regrename was
   changing DP to IP (when it appeared in addresses like (plus:HI
   (reg: DP) (const_int 37)) - and that's bad because IP doesn't
   permit offsets!  */

#define HARD_REGNO_RENAME_OK(REG, NREG)				\
  (((REG) == REG_DPH) ? 0					\
    : ((REG) == REG_IPH) ? ((NREG) == REG_DPH)			\
    : (((NREG) == REG_IPL) || ((NREG) == REG_DPL)) ? 0 : 1)

#define HARD_REGNO_NREGS(REGNO, MODE) \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* A C expression for the number of consecutive hard registers,
   starting at register number REGNO, required to hold a value of mode
   MODE.

   On a machine where all registers are exactly one word, a suitable
   definition of this macro is

   #define HARD_REGNO_NREGS(REGNO, MODE)            \
   ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1)  \
   / UNITS_PER_WORD))  */

#define HARD_REGNO_MODE_OK(REGNO, MODE) 1
/* A C expression that is nonzero if it is permissible to store a
   value of mode MODE in hard register number REGNO (or in several
   registers starting with that one).  For a machine where all
   registers are equivalent, a suitable definition is

   #define HARD_REGNO_MODE_OK(REGNO, MODE) 1

   It is not necessary for this macro to check for the numbers of
   fixed registers, because the allocation mechanism considers them
   to be always occupied.

   On some machines, double-precision values must be kept in even/odd
   register pairs.  The way to implement that is to define this macro
   to reject odd register numbers for such modes.

   The minimum requirement for a mode to be OK in a register is that
   the `movMODE' instruction pattern support moves between the
   register and any other hard register for which the mode is OK; and
   that moving a value into the register and back out not alter it.

   Since the same instruction used to move `SImode' will work for all
   narrower integer modes, it is not necessary on any machine for
   `HARD_REGNO_MODE_OK' to distinguish between these modes, provided
   you define patterns `movhi', etc., to take advantage of this.  This
   is useful because of the interaction between `HARD_REGNO_MODE_OK'
   and `MODES_TIEABLE_P'; it is very desirable for all integer modes
   to be tieable.

   Many machines have special registers for floating point arithmetic.
   Often people assume that floating point machine modes are allowed
   only in floating point registers.  This is not true.  Any
   registers that can hold integers can safely *hold* a floating
   point machine mode, whether or not floating arithmetic can be done
   on it in those registers.  Integer move instructions can be used
   to move the values.

   On some machines, though, the converse is true: fixed-point machine
   modes may not go in floating registers.  This is true if the
   floating registers normalize any value stored in them, because
   storing a non-floating value there would garble it.  In this case,
   `HARD_REGNO_MODE_OK' should reject fixed-point machine modes in
   floating registers.  But if the floating registers do not
   automatically normalize, if you can store any bit pattern in one
   and retrieve it unchanged without a trap, then any machine mode
   may go in a floating register, so you can define this macro to say
   so.

   The primary significance of special floating registers is rather
   that they are the registers acceptable in floating point arithmetic
   instructions.  However, this is of no concern to
   `HARD_REGNO_MODE_OK'.  You handle it by writing the proper
   constraints for those instructions.

   On some machines, the floating registers are especially slow to
   access, so that it is better to store a value in a stack frame
   than in such a register if floating point arithmetic is not being
   done.  As long as the floating registers are not in class
   `GENERAL_REGS', they will not be used unless some pattern's
   constraint asks for one.  */

#define MODES_TIEABLE_P(MODE1, MODE2)		\
   (((MODE1) == QImode && (MODE2) == HImode)	\
    || ((MODE2) == QImode && (MODE1) == HImode))
/* We originally had this as follows - this isn't a win on the IP2k
   though as registers just get in our way!
   
   #define MODES_TIEABLE_P(MODE1, MODE2) \
    (((MODE1) > HImode && (MODE2) == HImode)
     || ((MODE1) == HImode && (MODE2) > HImode))  */

/* A C expression that is nonzero if it is desirable to choose
   register allocation so as to avoid move instructions between a
   value of mode MODE1 and a value of mode MODE2.

   If `HARD_REGNO_MODE_OK (R, MODE1)' and `HARD_REGNO_MODE_OK (R,
   MODE2)' are ever different for any R, then `MODES_TIEABLE_P (MODE1,
   MODE2)' must be zero.  */

enum reg_class {
  NO_REGS,
  DPH_REGS,
  DPL_REGS,
  DP_REGS,
  SP_REGS,
  IPH_REGS,
  IPL_REGS,
  IP_REGS,
  DP_SP_REGS,
  PTR_REGS,
  NONPTR_REGS,
  NONSP_REGS,
  GENERAL_REGS,
  ALL_REGS = GENERAL_REGS,
  LIM_REG_CLASSES
};

/* An enumeral type that must be defined with all the register class
   names as enumeral values.  `NO_REGS' must be first.  `ALL_REGS'
   must be the last register class, followed by one more enumeral
   value, `LIM_REG_CLASSES', which is not a register class but rather
   tells how many classes there are.

   Each register class has a number, which is the value of casting
   the class name to type `int'.  The number serves as an index in
   many of the tables described below.  */


#define N_REG_CLASSES (int)LIM_REG_CLASSES
/* The number of distinct register classes, defined as follows:

   #define N_REG_CLASSES (int) LIM_REG_CLASSES  */

#define REG_CLASS_NAMES {			\
		"NO_REGS",			\
		"DPH_REGS",			\
		"DPL_REGS",			\
		"DP_REGS",			\
		"SP_REGS",			\
		"IPH_REGS",			\
		"IPL_REGS",			\
		"IP_REGS",			\
		"DP_SP_REGS",			\
		"PTR_REGS",			\
	        "NONPTR_REGS",			\
		"NONSP_REGS",			\
		"GENERAL_REGS"			\
		}
/* An initializer containing the names of the register classes as C
   string constants.  These names are used in writing some of the
   debugging dumps.  */


#define REG_CLASS_CONTENTS {			 	\
{0x00000000, 0, 0, 0, 0, 0, 0, 0, 0}, /* NO_REGS */	\
{0x00001000, 0, 0, 0, 0, 0, 0, 0, 0}, /* DPH_REGS */	\
{0x00002000, 0, 0, 0, 0, 0, 0, 0, 0}, /* DPL_REGS */	\
{0x00003000, 0, 0, 0, 0, 0, 0, 0, 0}, /* DP_REGS */	\
{0x000000c0, 0, 0, 0, 0, 0, 0, 0, 0}, /* SP_REGS */	\
{0x00000010, 0, 0, 0, 0, 0, 0, 0, 0}, /* IPH_REGS */	\
{0x00000020, 0, 0, 0, 0, 0, 0, 0, 0}, /* IPL_REGS */	\
{0x00000030, 0, 0, 0, 0, 0, 0, 0, 0}, /* IP_REGS */	\
{0x000030c0, 0, 0, 0, 0, 0, 0, 0, 0}, /* DP_SP_REGS */	\
{0x000030f0, 0, 0, 0, 0, 0, 0, 0, 0}, /* PTR_REGS */	\
{0xffffcf0f,-1,-1,-1,-1,-1,-1,-1, 0}, /* NONPTR_REGS */	\
{0xffffff3f,-1,-1,-1,-1,-1,-1,-1, 0}, /* NONSP_REGS */	\
{0xffffffff,-1,-1,-1,-1,-1,-1,-1,15}  /* GENERAL_REGS */ \
}

/* An initializer containing the contents of the register classes, as
   integers which are bit masks.  The Nth integer specifies the
   contents of class N.  The way the integer MASK is interpreted is
   that register R is in the class if `MASK & (1 << R)' is 1.

   When the machine has more than 32 registers, an integer does not
   suffice.  Then the integers are replaced by sub-initializers,
   braced groupings containing several integers.  Each
   sub-initializer must be suitable as an initializer for the type
   `HARD_REG_SET' which is defined in `hard-reg-set.h'.  */

#define REGNO_REG_CLASS(R)	\
  ( (R) == REG_IPH ? IPH_REGS	\
  : (R) == REG_IPL ? IPL_REGS	\
  : (R) == REG_DPH ? DPH_REGS	\
  : (R) == REG_DPL ? DPL_REGS	\
  : (R) == REG_SPH ? SP_REGS	\
  : (R) == REG_SPL ? SP_REGS	\
  : NONPTR_REGS)

/* A C expression whose value is a register class containing hard
   register REGNO.  In general there is more than one such class;
   choose a class which is "minimal", meaning that no smaller class
   also contains the register.  */

#define MODE_BASE_REG_CLASS(MODE) ((MODE) == QImode ? PTR_REGS : DP_SP_REGS)
/* This is a variation of the BASE_REG_CLASS macro which allows
   the selection of a base register in a mode depenedent manner.
   If MODE is VOIDmode then it should return the same value as
   BASE_REG_CLASS.  */

#define BASE_REG_CLASS PTR_REGS
/* A macro whose definition is the name of the class to which a valid
   base register must belong.  A base register is one used in an
   address which is the register value plus a displacement.  */

#define INDEX_REG_CLASS NO_REGS
/* A macro whose definition is the name of the class to which a valid
   index register must belong.  An index register is one used in an
   address where its value is either multiplied by a scale factor or
   added to another register (as well as added to a displacement).  */


#define REG_CLASS_FROM_LETTER(C)	\
  ( (C) == 'j' ? IPH_REGS		\
  : (C) == 'k' ? IPL_REGS		\
  : (C) == 'f' ? IP_REGS		\
  : (C) == 'y' ? DPH_REGS		\
  : (C) == 'z' ? DPL_REGS		\
  : (C) == 'b' ? DP_REGS		\
  : (C) == 'u' ? NONSP_REGS		\
  : (C) == 'q' ? SP_REGS		\
  : (C) == 'c' ? DP_SP_REGS		\
  : (C) == 'a' ? PTR_REGS		\
  : (C) == 'd' ? NONPTR_REGS 		\
  : NO_REGS)

/* A C expression which defines the machine-dependent operand
   constraint letters for register classes.  If CHAR is such a
   letter, the value should be the register class corresponding to
   it.  Otherwise, the value should be `NO_REGS'.  The register
   letter `r', corresponding to class `GENERAL_REGS', will not be
   passed to this macro; you do not need to handle it.  */


#define REGNO_OK_FOR_BASE_P(R) \
  ((R) == REG_DP || (R) == REG_IP || (R) == REG_SP)
/* A C expression which is nonzero if register number R is suitable
   for use as a base register in operand addresses.  It may be either
   a suitable hard register or a pseudo register that has been
   allocated such a hard register.  */

#define REGNO_MODE_OK_FOR_BASE_P(R,M) 		\
  ((R) == REG_DP || (R) == REG_SP		\
   || ((R) == REG_IP && GET_MODE_SIZE (M) <= 1))
/* A C expression that is just like `REGNO_OK_FOR_BASE_P', except that
   that expression may examine the mode of the memory reference in
   MODE.  You should define this macro if the mode of the memory
   reference affects whether a register may be used as a base
   register.  If you define this macro, the compiler will use it
   instead of `REGNO_OK_FOR_BASE_P'.  */

#define REGNO_OK_FOR_INDEX_P(NUM) 0
/* A C expression which is nonzero if register number NUM is suitable
   for use as an index register in operand addresses.  It may be
   either a suitable hard register or a pseudo register that has been
   allocated such a hard register.

   The difference between an index register and a base register is
   that the index register may be scaled.  If an address involves the
   sum of two registers, neither one of them scaled, then either one
   may be labeled the "base" and the other the "index"; but whichever
   labeling is used must fit the machine's constraints of which
   registers may serve in each capacity.  The compiler will try both
   labelings, looking for one that is valid, and will reload one or
   both registers only if neither labeling works.  */

#define PREFERRED_RELOAD_CLASS(X, CLASS) (CLASS)
/* A C expression that places additional restrictions on the register
   class to use when it is necessary to copy value X into a register
   in class CLASS.  The value is a register class; perhaps CLASS, or
   perhaps another, smaller class.  On many machines, the following
   definition is safe:

   #define PREFERRED_RELOAD_CLASS(X,CLASS) (CLASS)

   Sometimes returning a more restrictive class makes better code.
   For example, on the 68000, when X is an integer constant that is
   in range for a `moveq' instruction, the value of this macro is
   always `DATA_REGS' as long as CLASS includes the data registers.
   Requiring a data register guarantees that a `moveq' will be used.

   If X is a `const_double', by returning `NO_REGS' you can force X
   into a memory constant.  This is useful on certain machines where
   immediate floating values cannot be loaded into certain kinds of
   registers.  */

/* `PREFERRED_OUTPUT_RELOAD_CLASS (X, CLASS)'
   Like `PREFERRED_RELOAD_CLASS', but for output reloads instead of
   input reloads.  If you don't define this macro, the default is to
   use CLASS, unchanged.  */

/* `LIMIT_RELOAD_CLASS (MODE, CLASS)'
   A C expression that places additional restrictions on the register
   class to use when it is necessary to be able to hold a value of
   mode MODE in a reload register for which class CLASS would
   ordinarily be used.

   Unlike `PREFERRED_RELOAD_CLASS', this macro should be used when
   there are certain modes that simply can't go in certain reload
   classes.

   The value is a register class; perhaps CLASS, or perhaps another,
   smaller class.

   Don't define this macro unless the target machine has limitations
   which require the macro to do something nontrivial.  */

/* SECONDARY_INPUT_RELOAD_CLASS(CLASS, MODE, X)
   `SECONDARY_RELOAD_CLASS (CLASS, MODE, X)'
   `SECONDARY_OUTPUT_RELOAD_CLASS (CLASS, MODE, X)'
   Many machines have some registers that cannot be copied directly
   to or from memory or even from other types of registers.  An
   example is the `MQ' register, which on most machines, can only be
   copied to or from general registers, but not memory.  Some
   machines allow copying all registers to and from memory, but
   require a scratch register for stores to some memory locations
   (e.g., those with symbolic address on the RT, and those with
   certain symbolic address on the SPARC when compiling PIC).  In
   some cases, both an intermediate and a scratch register are
   required.

   You should define these macros to indicate to the reload phase
   that it may need to allocate at least one register for a reload in
   addition to the register to contain the data.  Specifically, if
   copying X to a register CLASS in MODE requires an intermediate
   register, you should define `SECONDARY_INPUT_RELOAD_CLASS' to
   return the largest register class all of whose registers can be
   used as intermediate registers or scratch registers.

   If copying a register CLASS in MODE to X requires an intermediate
   or scratch register, `SECONDARY_OUTPUT_RELOAD_CLASS' should be
   defined to return the largest register class required.  If the
   requirements for input and output reloads are the same, the macro
   `SECONDARY_RELOAD_CLASS' should be used instead of defining both
   macros identically.

   The values returned by these macros are often `GENERAL_REGS'.
   Return `NO_REGS' if no spare register is needed; i.e., if X can be
   directly copied to or from a register of CLASS in MODE without
   requiring a scratch register.  Do not define this macro if it
   would always return `NO_REGS'.

   If a scratch register is required (either with or without an
   intermediate register), you should define patterns for
   `reload_inM' or `reload_outM', as required (*note Standard
   Names::..  These patterns, which will normally be implemented with
   a `define_expand', should be similar to the `movM' patterns,
   except that operand 2 is the scratch register.

   Define constraints for the reload register and scratch register
   that contain a single register class.  If the original reload
   register (whose class is CLASS) can meet the constraint given in
   the pattern, the value returned by these macros is used for the
   class of the scratch register.  Otherwise, two additional reload
   registers are required.  Their classes are obtained from the
   constraints in the insn pattern.

   X might be a pseudo-register or a `subreg' of a pseudo-register,
   which could either be in a hard register or in memory.  Use
   `true_regnum' to find out; it will return -1 if the pseudo is in
   memory and the hard register number if it is in a register.

   These macros should not be used in the case where a particular
   class of registers can only be copied to memory and not to another
   class of registers.  In that case, secondary reload registers are
   not needed and would not be helpful.  Instead, a stack location
   must be used to perform the copy and the `movM' pattern should use
   memory as an intermediate storage.  This case often occurs between
   floating-point and general registers.  */

/* `SECONDARY_MEMORY_NEEDED (CLASS1, CLASS2, M)'
   Certain machines have the property that some registers cannot be
   copied to some other registers without using memory.  Define this
   macro on those machines to be a C expression that is nonzero if
   objects of mode M in registers of CLASS1 can only be copied to
   registers of class CLASS2 by storing a register of CLASS1 into
   memory and loading that memory location into a register of CLASS2.

   Do not define this macro if its value would always be zero.

   `SECONDARY_MEMORY_NEEDED_RTX (MODE)'
   Normally when `SECONDARY_MEMORY_NEEDED' is defined, the compiler
   allocates a stack slot for a memory location needed for register
   copies.  If this macro is defined, the compiler instead uses the
   memory location defined by this macro.

   Do not define this macro if you do not define
   `SECONDARY_MEMORY_NEEDED'.  */

#define SMALL_REGISTER_CLASSES 1
/* Normally the compiler avoids choosing registers that have been
   explicitly mentioned in the rtl as spill registers (these
   registers are normally those used to pass parameters and return
   values).  However, some machines have so few registers of certain
   classes that there would not be enough registers to use as spill
   registers if this were done.

   Define `SMALL_REGISTER_CLASSES' to be an expression with a nonzero
   value on these machines.  When this macro has a nonzero value, the
   compiler allows registers explicitly used in the rtl to be used as
   spill registers but avoids extending the lifetime of these
   registers.

   It is always safe to define this macro with a nonzero value, but
   if you unnecessarily define it, you will reduce the amount of
   optimizations that can be performed in some cases.  If you do not
   define this macro with a nonzero value when it is required, the
   compiler will run out of spill registers and print a fatal error
   message.  For most machines, you should not define this macro at
   all.  */

#define CLASS_LIKELY_SPILLED_P(CLASS)  class_likely_spilled_p(CLASS)
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

#define CLASS_MAX_NREGS(CLASS, MODE)   GET_MODE_SIZE (MODE)
/* A C expression for the maximum number of consecutive registers of
   class CLASS needed to hold a value of mode MODE.

   This is closely related to the macro `HARD_REGNO_NREGS'.  In fact,
   the value of the macro `CLASS_MAX_NREGS (CLASS, MODE)' should be
   the maximum value of `HARD_REGNO_NREGS (REGNO, MODE)' for all
   REGNO values in the class CLASS.

   This macro helps control the handling of multiple-word values in
   the reload pass.  */

#define CONST_OK_FOR_LETTER_P(VALUE, C)				\
  ((C) == 'I' ? (VALUE) >= -255 && (VALUE) <= -1 :		\
   (C) == 'J' ? (VALUE) >= 0 && (VALUE) <= 7 :			\
   (C) == 'K' ? (VALUE) >= 0 && (VALUE) <= 127 :		\
   (C) == 'L' ? (VALUE) > 0 && (VALUE) < 128:			\
   (C) == 'M' ? (VALUE) == -1:					\
   (C) == 'N' ? (VALUE) == 1:					\
   (C) == 'O' ? (VALUE) == 0:					\
   (C) == 'P' ? (VALUE) >= 0 && (VALUE) <= 255:			\
   0)

/* A C expression that defines the machine-dependent operand
   constraint letters (`I', `J', `K', ... `P') that specify
   particular ranges of integer values.  If C is one of those
   letters, the expression should check that VALUE, an integer, is in
   the appropriate range and return 1 if so, 0 otherwise.  If C is
   not one of those letters, the value should be 0 regardless of
   VALUE.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C) 0

/* `CONST_DOUBLE_OK_FOR_LETTER_P (VALUE, C)'
   A C expression that defines the machine-dependent operand
   constraint letters that specify particular ranges of
   `const_double' values (`G' or `H').

   If C is one of those letters, the expression should check that
   VALUE, an RTX of code `const_double', is in the appropriate range
   and return 1 if so, 0 otherwise.  If C is not one of those
   letters, the value should be 0 regardless of VALUE.

   `const_double' is used for all floating-point constants and for
   `DImode' fixed-point constants.  A given letter can accept either
   or both kinds of values.  It can use `GET_MODE' to distinguish
   between these kinds.  */

#define EXTRA_CONSTRAINT(X, C) ip2k_extra_constraint (X, C)

/* A C expression that defines the optional machine-dependent
   constraint letters (``Q', `R', `S', `T', `U') that can'
   be used to segregate specific types of operands, usually memory
   references, for the target machine.  Normally this macro will not
   be defined.  If it is required for a particular target machine, it
   should return 1 if VALUE corresponds to the operand type
   represented by the constraint letter C.  If C is not defined as an
   extra constraint, the value returned should be 0 regardless of
   VALUE.

   For example, on the ROMP, load instructions cannot have their
   output in r0 if the memory reference contains a symbolic address.
   Constraint letter `Q' is defined as representing a memory address
   that does *not* contain a symbolic address.  An alternative is
   specified with a `Q' constraint on the input and `r' on the
   output.  The next alternative specifies `m' on the input and a
   register class that does not include r0 on the output.  */

/* This is an undocumented variable which describes
   how GCC will pop a data.  */
#define STACK_POP_CODE PRE_INC

#define STACK_PUSH_CODE POST_DEC
/* This macro defines the operation used when something is pushed on
   the stack.  In RTL, a push operation will be `(set (mem
   (STACK_PUSH_CODE (reg sp))) ...)'
  
   The choices are `PRE_DEC', `POST_DEC', `PRE_INC', and `POST_INC'.
   Which of these is correct depends on the stack direction and on
   whether the stack pointer points to the last item on the stack or
   whether it points to the space for the next item on the stack.
  
   The default is `PRE_DEC' when `STACK_GROWS_DOWNWARD' is defined,
   which is almost always right, and `PRE_INC' otherwise, which is
   often wrong.  */


#define STACK_CHECK_BUILTIN	1
/* Prologue code will do stack checking as necessary.  */
  
#define STARTING_FRAME_OFFSET (0)	
/* Offset from the frame pointer to the first local variable slot to
   be allocated.

   If `FRAME_GROWS_DOWNWARD', find the next slot's offset by
   subtracting the first slot's length from `STARTING_FRAME_OFFSET'.
   Otherwise, it is found by adding the length of the first slot to
   the value `STARTING_FRAME_OFFSET'.  */

#define FRAME_GROWS_DOWNWARD	1
#define STACK_GROWS_DOWNWARD	1

/* On IP2K arg pointer is virtual and resolves to either SP or FP
   after we've resolved what registers are saved (fp chain, return
   pc, etc.  */

#define FIRST_PARM_OFFSET(FUNDECL) 0
/* Offset from the argument pointer register to the first argument's
   address.  On some machines it may depend on the data type of the
   function.

   If `ARGS_GROW_DOWNWARD', this is the offset to the location above
   the first argument's address.  */

/* `STACK_DYNAMIC_OFFSET (FUNDECL)'
   Offset from the stack pointer register to an item dynamically
   allocated on the stack, e.g., by `alloca'.

   The default value for this macro is `STACK_POINTER_OFFSET' plus the
   length of the outgoing arguments.  The default is correct for most
   machines.  See `function.c' for details.  */

#define STACK_POINTER_OFFSET 1
/* IP2K stack is post-decremented, so 0(sp) is address of open space
   and 1(sp) is offset to the location avobe the forst location at which
   outgoing arguments are placed.  */

#define STACK_BOUNDARY 8
/* Define this macro if there is a guaranteed alignment for the stack
   pointer on this machine.  The definition is a C expression for the
   desired alignment (measured in bits).  This value is used as a
   default if PREFERRED_STACK_BOUNDARY is not defined.  */

#define STACK_POINTER_REGNUM REG_SP
/* The register number of the stack pointer register, which must also
   be a fixed register according to `FIXED_REGISTERS'.  On most
   machines, the hardware determines which register this is.  */

#define FRAME_POINTER_REGNUM REG_VFP
/* The register number of the frame pointer register, which is used to
   access automatic variables in the stack frame.  On some machines,
   the hardware determines which register this is.  On other
   machines, you can choose any register you wish for this purpose.  */

#define HARD_FRAME_POINTER_REGNUM REG_FP

#define ARG_POINTER_REGNUM  REG_AP
/* The register number of the arg pointer register, which is used to
   access the function's argument list.  On some machines, this is
   the same as the frame pointer register.  On some machines, the
   hardware determines which register this is.  On other machines,
   you can choose any register you wish for this purpose.  If this is
   not the same register as the frame pointer register, then you must
   mark it as a fixed register according to `FIXED_REGISTERS', or
   arrange to be able to eliminate it (*note Elimination::.).  */

/* We don't really want to support nested functions.  But we'll crash
   in various testsuite tests if we don't at least define the register
   to contain the static chain. The return value register is about as
   bad a place as any for this.  */

#define STATIC_CHAIN_REGNUM	REG_RESULT
/* Register numbers used for passing a function's static chain
   pointer.  If register windows are used, the register number as
   seen by the called function is `STATIC_CHAIN_INCOMING_REGNUM',
   while the register number as seen by the calling function is
   `STATIC_CHAIN_REGNUM'.  If these registers are the same,
   `STATIC_CHAIN_INCOMING_REGNUM' need not be defined.

   The static chain register need not be a fixed register.

   If the static chain is passed in memory, these macros should not be
   defined; instead, the next two macros should be defined.  */

#define FRAME_POINTER_REQUIRED (!flag_omit_frame_pointer)
/* A C expression which is nonzero if a function must have and use a
   frame pointer.  This expression is evaluated  in the reload pass.
   If its value is nonzero the function will have a frame pointer.

   The expression can in principle examine the current function and
   decide according to the facts, but on most machines the constant 0
   or the constant 1 suffices.  Use 0 when the machine allows code to
   be generated with no frame pointer, and doing so saves some time
   or space.  Use 1 when there is no possible advantage to avoiding a
   frame pointer.

   In certain cases, the compiler does not know how to produce valid
   code without a frame pointer.  The compiler recognizes those cases
   and automatically gives the function a frame pointer regardless of
   what `FRAME_POINTER_REQUIRED' says.  You don't need to worry about
   them.

   In a function that does not require a frame pointer, the frame
   pointer register can be allocated for ordinary usage, unless you
   mark it as a fixed register.  See `FIXED_REGISTERS' for more
   information.  */

#define ELIMINABLE_REGS	{ 					\
        {ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},		\
	{ARG_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM},	\
	{FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},		\
	{FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM},	\
	{HARD_FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},	\
}
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
   pointer is specified first since that is the preferred elimination.  */


#define CAN_ELIMINATE(FROM, TO) 				\
  ((FROM) == HARD_FRAME_POINTER_REGNUM				\
   ? (flag_omit_frame_pointer && !frame_pointer_needed) : 1)
/* Don't eliminate FP unless we EXPLICITLY_ASKED  */

/* A C expression that returns nonzero if the compiler is allowed to
   try to replace register number FROM-REG with register number
   TO-REG.  This macro need only be defined if `ELIMINABLE_REGS' is
   defined, and will usually be the constant 1, since most of the
   cases preventing register elimination are things that the compiler
   already knows about.  */

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  ((OFFSET) = ip2k_init_elim_offset ((FROM), (TO)))

/* This macro is similar to `INITIAL_FRAME_POINTER_OFFSET'.  It
   specifies the initial difference between the specified pair of
   registers.  This macro must be defined if `ELIMINABLE_REGS' is
   defined.  */

#define RETURN_ADDR_RTX(COUNT, X) \
  (((COUNT) == 0) ? gen_rtx_REG (HImode, REG_CALLH) : NULL_RTX)
/*   A C expression whose value is RTL representing the value of the
     return address for the frame COUNT steps up from the current
     frame, after the prologue.  FRAMEADDR is the frame pointer of the
     COUNT frame, or the frame pointer of the COUNT - 1 frame if
     `RETURN_ADDR_IN_PREVIOUS_FRAME' is defined.

     The value of the expression must always be the correct address when
     COUNT is zero, but may be `NULL_RTX' if there is not way to
     determine the return address of other frames.  */

#define PUSH_ROUNDING(NPUSHED) (NPUSHED)
/* A C expression that is the number of bytes actually pushed onto the
   stack when an instruction attempts to push NPUSHED bytes.

   If the target machine does not have a push instruction, do not
   define this macro.  That directs GNU CC to use an alternate
   strategy: to allocate the entire argument block and then store the
   arguments into it.

   On some machines, the definition

   #define PUSH_ROUNDING(BYTES) (BYTES)

   will suffice.  But on other machines, instructions that appear to
   push one byte actually push two bytes in an attempt to maintain
   alignment.  Then the definition should be

   #define PUSH_ROUNDING(BYTES) (((BYTES) + 1) & ~1)  */

#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE) \
  ip2k_return_pops_args ((FUNDECL), (FUNTYPE), (SIZE))
/* A C expression that should indicate the number of bytes of its own
   arguments that a function pops on returning, or 0 if the function
   pops no arguments and the caller must therefore pop them all after
   the function returns.

   FUNDECL is a C variable whose value is a tree node that describes
   the function in question.  Normally it is a node of type
   `FUNCTION_DECL' that describes the declaration of the function.
   From this you can obtain the DECL_MACHINE_ATTRIBUTES of the
   function.

   FUNTYPE is a C variable whose value is a tree node that describes
   the function in question.  Normally it is a node of type
   `FUNCTION_TYPE' that describes the data type of the function.
   From this it is possible to obtain the data types of the value and
   arguments (if known).

   When a call to a library function is being considered, FUNDECL
   will contain an identifier node for the library function.  Thus, if
   you need to distinguish among various library functions, you can
   do so by their names.  Note that "library function" in this
   context means a function used to perform arithmetic, whose name is
   known specially in the compiler and was not mentioned in the C
   code being compiled.

   STACK-SIZE is the number of bytes of arguments passed on the
   stack.  If a variable number of bytes is passed, it is zero, and
   argument popping will always be the responsibility of the calling
   function.

   On the VAX, all functions always pop their arguments, so the
   definition of this macro is STACK-SIZE.  On the 68000, using the
   standard calling convention, no functions pop their arguments, so
   the value of the macro is always 0 in this case.  But an
   alternative calling convention is available in which functions
   that take a fixed number of arguments pop them but other functions
   (such as `printf') pop nothing (the caller pops all).  When this
   convention is in use, FUNTYPE is examined to determine whether a
   function takes a fixed number of arguments.  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) 0
/* A C expression that controls whether a function argument is passed
   in a register, and which register.

   The arguments are CUM, which summarizes all the previous
   arguments; MODE, the machine mode of the argument; TYPE, the data
   type of the argument as a tree node or 0 if that is not known
   (which happens for C support library functions); and NAMED, which
   is 1 for an ordinary argument and 0 for nameless arguments that
   correspond to `...' in the called function's prototype.

   The value of the expression is usually either a `reg' RTX for the
   hard register in which to pass the argument, or zero to pass the
   argument on the stack.

   For machines like the VAX and 68000, where normally all arguments
   are pushed, zero suffices as a definition.

   The value of the expression can also be a `parallel' RTX.  This is
   used when an argument is passed in multiple locations.  The mode
   of the of the `parallel' should be the mode of the entire
   argument.  The `parallel' holds any number of `expr_list' pairs;
   each one describes where part of the argument is passed.  In each
   `expr_list', the first operand can be either a `reg' RTX for the
   hard register in which to pass this part of the argument, or zero
   to pass the argument on the stack.  If this operand is a `reg',
   then the mode indicates how large this part of the argument is.
   The second operand of the `expr_list' is a `const_int' which gives
   the offset in bytes into the entire argument where this part
   starts.

   The usual way to make the ANSI library `stdarg.h' work on a machine
   where some arguments are usually passed in registers, is to 	cause
   nameless arguments to be passed on the stack instead.  This is done
   by making `FUNCTION_ARG' return 0 whenever NAMED is 0.

   You may use the macro `MUST_PASS_IN_STACK (MODE, TYPE)' in the
   definition of this macro to determine if this argument is of a
   type that must be passed in the stack.  If `REG_PARM_STACK_SPACE'
   is not defined and `FUNCTION_ARG' returns nonzero for such an
   argument, the compiler will abort.  If `REG_PARM_STACK_SPACE' is
   defined, the argument will be computed in the stack and then
   loaded into a register.  */

#define CUMULATIVE_ARGS	int

/* A C type for declaring a variable that is used as the first
   argument of `FUNCTION_ARG' and other related values.  For some
   target machines, the type `int' suffices and can hold the number
   of bytes of argument so far.

   There is no need to record in `CUMULATIVE_ARGS' anything about the
   arguments that have been passed on the stack.  The compiler has
   other variables to keep track of that.  For target machines on
   which all arguments are passed on the stack, there is no need to
   store anything in `CUMULATIVE_ARGS'; however, the data structure
   must exist and should not be empty, so use `int'.  */

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT) \
  ((CUM) = 0)

/* A C statement (sans semicolon) for initializing the variable CUM
   for the state at the beginning of the argument list.  The variable
   has type `CUMULATIVE_ARGS'.  The value of FNTYPE is the tree node
   for the data type of the function which will receive the args, or 0
   if the args are to a compiler support library function.  The value
   of INDIRECT is nonzero when processing an indirect call, for
   example a call through a function pointer.  The value of INDIRECT
   is zero for a call to an explicitly named function, a library
   function call, or when `INIT_CUMULATIVE_ARGS' is used to find
   arguments for the function being compiled.
   
   When processing a call to a compiler support library function,
   LIBNAME identifies which one.  It is a `symbol_ref' rtx which
   contains the name of the function, as a string.  LIBNAME is 0 when
   an ordinary C function call is being processed.  Thus, each time
   this macro is called, either LIBNAME or FNTYPE is nonzero, but
   never both of them at once.  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)

/* All arguments are passed on stack - do nothing here.  */

/* A C statement (sans semicolon) to update the summarizer variable
   CUM to advance past an argument in the argument list.  The values
   MODE, TYPE and NAMED describe that argument.  Once this is done,
   the variable CUM is suitable for analyzing the *following*
   argument with `FUNCTION_ARG', etc.
   
   This macro need not do anything if the argument in question was
   passed on the stack.  The compiler knows how to track the amount
   of stack space used for arguments without any special help.  */

#define FUNCTION_ARG_REGNO_P(R) 0
/* A C expression that is nonzero if REGNO is the number of a hard
   register in which function arguments are sometimes passed.  This
   does *not* include implicit arguments such as the static chain and
   the structure-value address.  On many machines, no registers can be
   used for this purpose since all function arguments are pushed on
   the stack.  */

#define FUNCTION_VALUE(VALTYPE, FUNC) 				\
   ((TYPE_MODE (VALTYPE) == QImode)				\
    ? gen_rtx_REG (TYPE_MODE (VALTYPE), REG_RESULT + 1)	\
    : gen_rtx_REG (TYPE_MODE (VALTYPE), REG_RESULT))

/* Because functions returning 'char' actually widen to 'int', we have to
   use $81 as the return location if we think we only have a 'char'.  */

/* A C expression to create an RTX representing the place where a
   function returns a value of data type VALTYPE.  VALTYPE is a tree
   node representing a data type.  Write `TYPE_MODE (VALTYPE)' to get
   the machine mode used to represent that type.  On many machines,
   only the mode is relevant.  (Actually, on most machines, scalar
   values are returned in the same place regardless of mode).

   The value of the expression is usually a `reg' RTX for the hard
   register where the return value is stored.  The value can also be a
   `parallel' RTX, if the return value is in multiple places.  See
   `FUNCTION_ARG' for an explanation of the `parallel' form.

   If `PROMOTE_FUNCTION_RETURN' is defined, you must apply the same
   promotion rules specified in `PROMOTE_MODE' if VALTYPE is a scalar
   type.

   If the precise function being called is known, FUNC is a tree node
   (`FUNCTION_DECL') for it; otherwise, FUNC is a null pointer.  This
   makes it possible to use a different value-returning convention
   for specific functions when all their calls are known.

   `FUNCTION_VALUE' is not used for return vales with aggregate data
   types, because these are returned in another way.  See
   `STRUCT_VALUE_REGNUM' and related macros, below.  */

#define LIBCALL_VALUE(MODE)  gen_rtx_REG ((MODE), REG_RESULT)
/* A C expression to create an RTX representing the place where a
   library function returns a value of mode MODE.  If the precise
   function being called is known, FUNC is a tree node
   (`FUNCTION_DECL') for it; otherwise, FUNC is a null pointer.  This
   makes it possible to use a different value-returning convention
   for specific functions when all their calls are known.

   Note that "library function" in this context means a compiler
   support routine, used to perform arithmetic, whose name is known
   specially by the compiler and was not mentioned in the C code being
   compiled.

   The definition of `LIBRARY_VALUE' need not be concerned aggregate
   data types, because none of the library functions returns such
   types.  */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == REG_RESULT)
/* A C expression that is nonzero if REGNO is the number of a hard
   register in which the values of called function may come back.

   A register whose use for returning values is limited to serving as
   the second of a pair (for a value of type `double', say) need not
   be recognized by this macro.  So for most machines, this definition
   suffices:

   #define FUNCTION_VALUE_REGNO_P(N) ((N) == 0)

   If the machine has register windows, so that the caller and the
   called function use different registers for the return value, this
   macro should recognize only the caller's register numbers.  */

#define RETURN_IN_MEMORY(TYPE) \
  ((TYPE_MODE (TYPE) == BLKmode) ? int_size_in_bytes (TYPE) > 8 : 0)
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

/* Indicate that large structures are passed by reference.  */
#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM,MODE,TYPE,NAMED)	0


#define DEFAULT_PCC_STRUCT_RETURN 0
/* Define this macro to be 1 if all structure and union return values
   must be in memory.  Since this results in slower code, this should
   be defined only if needed for compatibility with other compilers
   or with an ABI.  If you define this macro to be 0, then the
   conventions used for structure and union return values are decided
   by the `RETURN_IN_MEMORY' macro.

   If not defined, this defaults to the value 1.  */

#define STRUCT_VALUE 0
/* If the structure value address is not passed in a register, define
   `STRUCT_VALUE' as an expression returning an RTX for the place
   where the address is passed.  If it returns 0, the address is
   passed as an "invisible" first argument.  */

#define STRUCT_VALUE_INCOMING 0
/* If the incoming location is not a register, then you should define
   `STRUCT_VALUE_INCOMING' as an expression for an RTX for where the
   called function should find the value.  If it should find the
   value on the stack, define this to create a `mem' which refers to
   the frame pointer.  A definition of 0 means that the address is
   passed as an "invisible" first argument.  */

#define EPILOGUE_USES(REGNO) 0
/* Define this macro as a C expression that is nonzero for registers
   are used by the epilogue or the `return' pattern.  The stack and
   frame pointer registers are already be assumed to be used as
   needed.  */

#define SETUP_INCOMING_VARARGS(ARGS_SO_FAR,MODE,TYPE,		\
			       PRETEND_ARGS_SIZE,SECOND_TIME)	\
  ((PRETEND_ARGS_SIZE) = (0))


/*  Hmmm.  We don't actually like constants as addresses - they always need
    to be loaded to a register, except for function calls which take an
    address by immediate value.  But changing this to zero had negative
    effects, causing the compiler to get very confused....  */

#define CONSTANT_ADDRESS_P(X) CONSTANT_P (X)

/* A C expression that is 1 if the RTX X is a constant which is a
   valid address.  On most machines, this can be defined as
   `CONSTANT_P (X)', but a few machines are more restrictive in which
   constant addresses are supported.

   `CONSTANT_P' accepts integer-values expressions whose values are
   not explicitly known, such as `symbol_ref', `label_ref', and
   `high' expressions and `const' arithmetic expressions, in addition
   to `const_int' and `const_double' expressions.  */

#define MAX_REGS_PER_ADDRESS 1
/* A number, the maximum number of registers that can appear in a
   valid memory address.  Note that it is up to you to specify a
   value equal to the maximum number that `GO_IF_LEGITIMATE_ADDRESS'
   would ever accept.  */

#ifdef REG_OK_STRICT
#  define GO_IF_LEGITIMATE_ADDRESS(MODE, OPERAND, ADDR)	\
{							\
  if (legitimate_address_p ((MODE), (OPERAND), 1))	\
    goto ADDR;						\
}
#else
#  define GO_IF_LEGITIMATE_ADDRESS(MODE, OPERAND, ADDR)	\
{							\
  if (legitimate_address_p ((MODE), (OPERAND), 0))	\
    goto ADDR;						\
}
#endif
/* A C compound statement with a conditional `goto LABEL;' executed
   if X (an RTX) is a legitimate memory address on the target machine
   for a memory operand of mode MODE.

   It usually pays to define several simpler macros to serve as
   subroutines for this one.  Otherwise it may be too complicated to
   understand.

   This macro must exist in two variants: a strict variant and a
   non-strict one.  The strict variant is used in the reload pass.  It
   must be defined so that any pseudo-register that has not been
   allocated a hard register is considered a memory reference.  In
   contexts where some kind of register is required, a pseudo-register
   with no hard register must be rejected.

   The non-strict variant is used in other passes.  It must be
   defined to accept all pseudo-registers in every context where some
   kind of register is required.

   Compiler source files that want to use the strict variant of this
   macro define the macro `REG_OK_STRICT'.  You should use an `#ifdef
   REG_OK_STRICT' conditional to define the strict variant in that
   case and the non-strict variant otherwise.

   Subroutines to check for acceptable registers for various purposes
   (one for base registers, one for index registers, and so on) are
   typically among the subroutines used to define
   `GO_IF_LEGITIMATE_ADDRESS'.  Then only these subroutine macros
   need have two variants; the higher levels of macros may be the
   same whether strict or not.

   Normally, constant addresses which are the sum of a `symbol_ref'
   and an integer are stored inside a `const' RTX to mark them as
   constant.  Therefore, there is no need to recognize such sums
   specifically as legitimate addresses.  Normally you would simply
   recognize any `const' as legitimate.

   Usually `PRINT_OPERAND_ADDRESS' is not prepared to handle constant
   sums that are not marked with  `const'.  It assumes that a naked
   `plus' indicates indexing.  If so, then you *must* reject such
   naked constant sums as illegitimate addresses, so that none of
   them will be given to `PRINT_OPERAND_ADDRESS'.

   On some machines, whether a symbolic address is legitimate depends
   on the section that the address refers to.  On these machines,
   define the macro `ENCODE_SECTION_INFO' to store the information
   into the `symbol_ref', and then check for it here.  When you see a
   `const', you will have to look inside it to find the `symbol_ref'
   in order to determine the section.  *Note Assembler Format::.

   The best way to modify the name string is by adding text to the
   beginning, with suitable punctuation to prevent any ambiguity.
   Allocate the new name in `saveable_obstack'.  You will have to
   modify `ASM_OUTPUT_LABELREF' to remove and decode the added text
   and output the name accordingly, and define `STRIP_NAME_ENCODING'
   to access the original name string.

   You can check the information stored here into the `symbol_ref' in
   the definitions of the macros `GO_IF_LEGITIMATE_ADDRESS' and
   `PRINT_OPERAND_ADDRESS'.  */

/* A C expression that is nonzero if X (assumed to be a `reg' RTX) is
   valid for use as a base register.  For hard registers, it should
   always accept those which the hardware permits and reject the
   others.  Whether the macro accepts or rejects pseudo registers
   must be controlled by `REG_OK_STRICT' as described above.  This
   usually requires two variant definitions, of which `REG_OK_STRICT'
   controls the one actually used.  */

#define REG_OK_FOR_BASE_STRICT_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))

#define REG_OK_FOR_BASE_NOSTRICT_P(X) 	\
  (REGNO (X) >= FIRST_PSEUDO_REGISTER 	\
   || (REGNO (X) == REG_FP)		\
   || (REGNO (X) == REG_VFP)		\
   || (REGNO (X) == REG_AP)		\
   || REG_OK_FOR_BASE_STRICT_P(X))

#ifdef REG_OK_STRICT
#  define REG_OK_FOR_BASE_P(X) REG_OK_FOR_BASE_STRICT_P (X)
#else
#  define REG_OK_FOR_BASE_P(X) REG_OK_FOR_BASE_NOSTRICT_P (X)
#endif

#define REG_OK_FOR_INDEX_P(X) 0
/* A C expression that is nonzero if X (assumed to be a `reg' RTX) is
   valid for use as an index register.

   The difference between an index register and a base register is
   that the index register may be scaled.  If an address involves the
   sum of two registers, neither one of them scaled, then either one
   may be labeled the "base" and the other the "index"; but whichever
   labeling is used must fit the machine's constraints of which
   registers may serve in each capacity.  The compiler will try both
   labelings, looking for one that is valid, and will reload one or
   both registers only if neither labeling works.  */


/* A C compound statement that attempts to replace X with a valid
   memory address for an operand of mode MODE.  WIN will be a C
   statement label elsewhere in the code; the macro definition may use

   GO_IF_LEGITIMATE_ADDRESS (MODE, X, WIN);

   to avoid further processing if the address has become legitimate.

   X will always be the result of a call to `break_out_memory_refs',
   and OLDX will be the operand that was given to that function to
   produce X.

   The code generated by this macro should not alter the substructure
   of X.  If it transforms X into a more legitimate form, it should
   assign X (which will always be a C variable) a new value.

   It is not necessary for this macro to come up with a legitimate
   address.  The compiler has standard ways of doing so in all cases.
   In fact, it is safe for this macro to do nothing.  But often a
   machine-dependent strategy can generate better code.  */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)		\
do { rtx orig_x = (X);					\
  (X) = legitimize_address ((X), (OLDX), (MODE), 0);	\
  if ((X) != orig_x && memory_address_p ((MODE), (X)))	\
    goto WIN;						\
} while (0)

/* Is X a legitimate register to reload, or is it a pseudo stack-temp
   that is problematic for push_reload() ?  */

#define LRA_REG(X)						\
  (! (reg_equiv_memory_loc[REGNO (X)]				\
      && (reg_equiv_address[REGNO (X)]				\
	  || num_not_at_initial_offset)))

/* Given a register X that failed the LRA_REG test, replace X
   by its memory equivalent, find the reloads needed for THAT memory
   location and substitute that back for the higher-level reload
   that we're conducting...  */

/* WARNING: we reference 'ind_levels' and 'insn' which are local variables
   in find_reloads_address (), where the LEGITIMIZE_RELOAD_ADDRESS macro
   expands.  */

#define FRA_REG(X,MODE,OPNUM,TYPE)					\
do {									\
  rtx tem = make_memloc ((X), REGNO (X));				\
									\
  if (! strict_memory_address_p (GET_MODE (tem), XEXP (tem, 0)))	\
    {									\
      /* Note that we're doing address in address - cf. ADDR_TYPE  */	\
      find_reloads_address (GET_MODE (tem), &tem, XEXP (tem, 0),	\
 			    &XEXP (tem, 0), (OPNUM),			\
			    ADDR_TYPE (TYPE), ind_levels, insn);	\
    }									\
  (X) = tem;								\
} while (0)


/* For the IP2K, we want to be clever about picking IP vs DP for a
   base pointer since IP only directly supports a zero displacement.
   (Note that we have modified all the HI patterns to correctly handle
   IP references by manipulating iph:ipl as we fetch the pieces).  */
#define LEGITIMIZE_RELOAD_ADDRESS(X,MODE,OPNUM,TYPE,IND,WIN)		     \
{									     \
  if (GET_CODE (X) == PLUS						     \
      && REG_P (XEXP (X, 0))						     \
      && GET_CODE (XEXP (X, 1)) == CONST_INT)				     \
    {									     \
      int disp = INTVAL (XEXP (X, 1));					     \
      int fit = (disp >= 0 && disp <= (127 - 2 * GET_MODE_SIZE (MODE)));     \
      rtx reg = XEXP (X, 0);						     \
      if (!fit)								     \
	{								     \
          push_reload ((X), NULL_RTX, &(X),				     \
		       NULL, MODE_BASE_REG_CLASS (MODE), GET_MODE (X),	     \
		       VOIDmode, 0, 0, OPNUM, TYPE);			     \
	  goto WIN;							     \
	}								     \
      if (reg_equiv_memory_loc[REGNO (reg)]				     \
          && (reg_equiv_address[REGNO (reg)] || num_not_at_initial_offset))  \
        {								     \
	  rtx mem = make_memloc (reg, REGNO (reg));			     \
	  if (! strict_memory_address_p (GET_MODE (mem), XEXP (mem, 0)))     \
	    {								     \
	      /* Note that we're doing address in address - cf. ADDR_TYPE  */\
               find_reloads_address (GET_MODE (mem), &mem, XEXP (mem, 0),    \
 			            &XEXP (mem, 0), (OPNUM),		     \
			            ADDR_TYPE (TYPE), (IND), insn);	     \
	    }								     \
          push_reload (mem, NULL, &XEXP (X, 0), NULL,			     \
		       GENERAL_REGS, Pmode, VOIDmode, 0, 0,		     \
		       OPNUM, TYPE);					     \
          push_reload (X, NULL, &X, NULL,				     \
		       MODE_BASE_REG_CLASS (MODE), GET_MODE (X), VOIDmode,   \
		       0, 0, OPNUM, TYPE);				     \
	  goto WIN;							     \
	}								     \
   }									     \
}
/* A C compound statement that attempts to replace X, which is an
   address that needs reloading, with a valid memory address for an
   operand of mode MODE.  WIN will be a C statement label elsewhere
   in the code.  It is not necessary to define this macro, but it 
   might be useful for performance reasons.

   For example, on the i386, it is sometimes possible to use a single
   reload register instead of two by reloading a sum of two pseudo
   registers into a register.  On the other hand, for number of RISC
   processors offsets are limited so that often an intermediate
   address needs to be generated in order to address a stack slot.
   By defining LEGITIMIZE_RELOAD_ADDRESS appropriately, the
   intermediate addresses generated for adjacent some stack slots can
   be made identical, and thus be shared.

   *Note*: This macro should be used with caution.  It is necessary
   to know something of how reload works in order to effectively use
   this, and it is quite easy to produce macros that build in too
   much knowledge of reload internals.

   *Note*: This macro must be able to reload an address created by a
   previous invocation of this macro.  If it fails to handle such
   addresses then the compiler may generate incorrect code or abort.

   The macro definition should use `push_reload' to indicate parts
   that need reloading; OPNUM, TYPE and IND_LEVELS are usually
   suitable to be passed unaltered to `push_reload'.

   The code generated by this macro must not alter the substructure of
   X.  If it transforms X into a more legitimate form, it should
   assign X (which will always be a C variable) a new value.  This
   also applies to parts that you change indirectly by calling
   `push_reload'.

   The macro definition may use `strict_memory_address_p' to test if
   the address has become legitimate.

   If you want to change only a part of X, one standard way of doing
   this is to use `copy_rtx'.  Note, however, that is unshares only a
   single level of rtl.  Thus, if the part to be changed is not at the
   top level, you'll need to replace first the top leve It is not
   necessary for this macro to come up with a legitimate address;
   but often a machine-dependent strategy can generate better code.  */
	
#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)		\
    do {							\
	if (ip2k_mode_dependent_address (ADDR)) goto LABEL;	\
    } while (0)

/* A C statement or compound statement with a conditional `goto
   LABEL;' executed if memory address X (an RTX) can have different
   meanings depending on the machine mode of the memory reference it
   is used for or if the address is valid for some modes but not
   others.

   Autoincrement and autodecrement addresses typically have
   mode-dependent effects because the amount of the increment or
   decrement is the size of the operand being addressed.  Some
   machines have other mode-dependent addresses.  Many RISC machines
   have no mode-dependent addresses.

   You may assume that ADDR is a valid address for the machine.  */

#define LEGITIMATE_CONSTANT_P(X) 1
/* A C expression that is nonzero if X is a legitimate constant for
   an immediate operand on the target machine.  You can assume that X
   satisfies `CONSTANT_P', so you need not check this.  In fact, `1'
   is a suitable definition for this macro on machines where anything
   `CONSTANT_P' is valid.  */

#define CONST_COSTS(RTX,CODE,OUTER_CODE) \
  case CONST_INT:			 \
    return 0;				 \
  case CONST:				 \
    return 8;                            \
  case LABEL_REF:			 \
    return 0;				 \
  case SYMBOL_REF:			 \
    return 8;				 \
  case CONST_DOUBLE:			 \
    return 0;

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
   with `GET_CODE (X)'.  */

#define DEFAULT_RTX_COSTS(X, CODE, OUTER_CODE)			\
  return default_rtx_costs ((X), (CODE), (OUTER_CODE))

/* Like `CONST_COSTS' but applies to nonconstant RTL expressions.
   This can be used, for example, to indicate how costly a multiply
   instruction is.  In writing this macro, you can use the construct
   `COSTS_N_INSNS (N)' to specify a cost equal to N fast
   instructions.  OUTER_CODE is the code of the expression in which X
   is contained.

   This macro is optional; do not define it if the default cost
   assumptions are adequate for the target machine.  */

#define ADDRESS_COST(ADDRESS) ip2k_address_cost (ADDRESS)

/* An expression giving the cost of an addressing mode that contains
   ADDRESS.  If not defined, the cost is computed from the ADDRESS
   expression and the `CONST_COSTS' values.

   For most CISC machines, the default cost is a good approximation
   of the true cost of the addressing mode.  However, on RISC
   machines, all instructions normally have the same length and
   execution time.  Hence all addresses will have equal costs.

   In cases where more than one form of an address is known, the form
   with the lowest cost will be used.  If multiple forms have the
   same, lowest, cost, the one that is the most complex will be used.

   For example, suppose an address that is equal to the sum of a
   register and a constant is used twice in the same basic block.
   When this macro is not defined, the address will be computed in a
   register and memory references will be indirect through that
   register.  On machines where the cost of the addressing mode
   containing the sum is no higher than that of a simple indirect
   reference, this will produce an additional instruction and
   possibly require an additional register.  Proper specification of
   this macro eliminates this overhead for such machines.

   Similar use of this macro is made in strength reduction of loops.

   ADDRESS need not be valid as an address.  In such a case, the cost
   is not relevant and can be any value; invalid addresses need not be
   assigned a different cost.

   On machines where an address involving more than one register is as
   cheap as an address computation involving only one register,
   defining `ADDRESS_COST' to reflect this can cause two registers to
   be live over a region of code where only one would have been if
   `ADDRESS_COST' were not defined in that manner.  This effect should
   be considered in the definition of this macro.  Equivalent costs
   should probably only be given to addresses with different numbers
   of registers on machines with lots of registers.

   This macro will normally either not be defined or be defined as a
   constant.  */

#define REGISTER_MOVE_COST(MODE, CLASS1, CLASS2) 7
/* A C expression for the cost of moving data from a register in class
   FROM to one in class TO.  The classes are expressed using the
   enumeration values such as `GENERAL_REGS'.  A value of 2 is the
   default; other values are interpreted relative to that.

   It is not required that the cost always equal 2 when FROM is the
   same as TO; on some machines it is expensive to move between
   registers if they are not general registers.

   If reload sees an insn consisting of a single `set' between two
   hard registers, and if `REGISTER_MOVE_COST' applied to their
   classes returns a value of 2, reload does not check to ensure that
   the constraints of the insn are met.  Setting a cost of other than
   2 will allow reload to verify that the constraints are met.  You
   should do this if the `movM' pattern's constraints do not allow
   such copying.  */

#define MEMORY_MOVE_COST(MODE,CLASS,IN) 6
/* A C expression for the cost of moving data of mode M between a
   register and memory.  A value of 4 is the default; this cost is
   relative to those in `REGISTER_MOVE_COST'.

   If moving between registers and memory is more expensive than
   between two registers, you should define this macro to express the
   relative cost.  */

#define SLOW_BYTE_ACCESS 0
/* Define this macro as a C expression which is nonzero if accessing
   less than a word of memory (i.e. a `char' or a `short') is no
   faster than accessing a word of memory, i.e., if such access
   require more than one instruction or if there is no difference in
   cost between byte and (aligned) word loads.

   When this macro is not defined, the compiler will access a field by
   finding the smallest containing object; when it is defined, a
   fullword load will be used if alignment permits.  Unless bytes
   accesses are faster than word accesses, using word accesses is
   preferable since it may eliminate subsequent memory access if
   subsequent accesses occur to other fields in the same word of the
   structure, but to different bytes.

   `SLOW_ZERO_EXTEND'
   Define this macro if zero-extension (of a `char' or `short' to an
   `int') can be done faster if the destination is a register that is
   known to be zero.

   If you define this macro, you must have instruction patterns that
   recognize RTL structures like this:

   (set (strict_low_part (subreg:QI (reg:SI ...) 0)) ...)

   and likewise for `HImode'.

   `SLOW_UNALIGNED_ACCESS'
   Define this macro to be the value 1 if unaligned accesses have a
   cost many times greater than aligned accesses, for example if they
   are emulated in a trap handler.

   When this macro is nonzero, the compiler will act as if
   `STRICT_ALIGNMENT' were nonzero when generating code for block
   moves.  This can cause significantly more instructions to be
   produced.  Therefore, do not set this macro nonzero if unaligned
   accesses only add a cycle or two to the time for a memory access.

   If the value of this macro is always zero, it need not be defined.

   `DONT_REDUCE_ADDR'
   Define this macro to inhibit strength reduction of memory
   addresses.  (On some machines, such strength reduction seems to do
   harm rather than good.)

   `MOVE_RATIO'
   The number of scalar move insns which should be generated instead
   of a string move insn or a library call.  Increasing the value
   will always make code faster, but eventually incurs high cost in
   increased code size.

   If you don't define this, a reasonable default is used.  */

#define NO_FUNCTION_CSE
/* Define this macro if it is as good or better to call a constant
   function address than to call an address kept in a register.  */

#define NO_RECURSIVE_FUNCTION_CSE
/* Define this macro if it is as good or better for a function to call
   itself with an explicit address than to call an address kept in a
   register.

   `ADJUST_COST (INSN, LINK, DEP_INSN, COST)'
   A C statement (sans semicolon) to update the integer variable COST
   based on the relationship between INSN that is dependent on
   DEP_INSN through the dependence LINK.  The default is to make no
   adjustment to COST.  This can be used for example to specify to
   the scheduler that an output- or anti-dependence does not incur
   the same cost as a data-dependence.

   `ADJUST_PRIORITY (INSN)'
   A C statement (sans semicolon) to update the integer scheduling
   priority `INSN_PRIORITY(INSN)'.  Reduce the priority to execute
   the INSN earlier, increase the priority to execute INSN later.
   Do not define this macro if you do not need to adjust the
   scheduling priorities of insns.  */

#define TEXT_SECTION_ASM_OP ".text"
/* A C expression whose value is a string containing the assembler
   operation that should precede instructions and read-only data.
   Normally `".text"' is right.  */

#define DATA_SECTION_ASM_OP ".data"
/* A C expression whose value is a string containing the assembler
   operation to identify the following data as writable initialized
   data.  Normally `".data"' is right.  */

#define JUMP_TABLES_IN_TEXT_SECTION 1
/* Define this macro if jump tables (for `tablejump' insns) should be
   output in the text section, along with the assembler instructions.
   Otherwise, the readonly data section is used.

   This macro is irrelevant if there is no separate readonly data
   section.  */

#define ASM_COMMENT_START " ; "
/* A C string constant describing how to begin a comment in the target
   assembler language.  The compiler assumes that the comment will
   end at the end of the line.  */

#define ASM_APP_ON "/* #APP */\n"
/* A C string constant for text to be output before each `asm'
   statement or group of consecutive ones.  Normally this is
   `"#APP"', which is a comment that has no effect on most assemblers
   but tells the GNU assembler that it must check the lines that
   follow for all valid assembler constructs.  */

#define ASM_APP_OFF "/* #NOAPP */\n"
/* A C string constant for text to be output after each `asm'
   statement or group of consecutive ones.  Normally this is
   `"#NO_APP"', which tells the GNU assembler to resume making the
   time-saving assumptions that are valid for ordinary compiler
   output.  */


#define OBJC_PROLOGUE {}
/* A C statement to output any assembler statements which are
   required to precede any Objective-C object definitions or message
   sending.  The statement is executed only when compiling an
   Objective-C program.  */

#define ASM_OUTPUT_DOUBLE(STREAM, VALUE) \
  fprintf ((STREAM), ".double %.20e\n", (VALUE))
#define ASM_OUTPUT_FLOAT(STREAM, VALUE) \
  asm_output_float ((STREAM), (VALUE))

/* `ASM_OUTPUT_LONG_DOUBLE (STREAM, VALUE)'
   `ASM_OUTPUT_THREE_QUARTER_FLOAT (STREAM, VALUE)'
   `ASM_OUTPUT_SHORT_FLOAT (STREAM, VALUE)'
   `ASM_OUTPUT_BYTE_FLOAT (STREAM, VALUE)'
   A C statement to output to the stdio stream STREAM an assembler
   instruction to assemble a floating-point constant of `TFmode',
   `DFmode', `SFmode', `TQFmode', `HFmode', or `QFmode',
   respectively, whose value is VALUE.  VALUE will be a C expression
   of type `REAL_VALUE_TYPE'.  Macros such as
   `REAL_VALUE_TO_TARGET_DOUBLE' are useful for writing these
   definitions.  */

#define ASM_OUTPUT_INT(FILE, VALUE)			\
 ( fprintf ((FILE), "\t.long "),			\
   output_addr_const ((FILE), (VALUE)),			\
   fputs ("\n", (FILE)))

 /* Likewise for `short' and `char' constants.  */

#define ASM_OUTPUT_SHORT(FILE,VALUE) \
  asm_output_short ((FILE), (VALUE))
#define ASM_OUTPUT_CHAR(FILE,VALUE) \
  asm_output_char ((FILE), (VALUE))

/* `ASM_OUTPUT_QUADRUPLE_INT (STREAM, EXP)'
   A C statement to output to the stdio stream STREAM an assembler
   instruction to assemble an integer of 16, 8, 4, 2 or 1 bytes,
   respectively, whose value is VALUE.  The argument EXP will be an
   RTL expression which represents a constant value.  Use
   `output_addr_const (STREAM, EXP)' to output this value as an
   assembler expression.

   For sizes larger than `UNITS_PER_WORD', if the action of a macro
   would be identical to repeatedly calling the macro corresponding to
   a size of `UNITS_PER_WORD', once for each word, you need not define
   the macro.  */

#define ASM_OUTPUT_BYTE(FILE,VALUE) \
  asm_output_byte ((FILE), (VALUE))
/* A C statement to output to the stdio stream STREAM an assembler
   instruction to assemble a single byte containing the number VALUE.  */

#define IS_ASM_LOGICAL_LINE_SEPARATOR(C) \
  ((C) == '\n' || ((C) == '$'))
/* Define this macro as a C expression which is nonzero if C is used
   as a logical line separator by the assembler.

   If you do not define this macro, the default is that only the
   character `;' is treated as a logical line separator.  */

#define ASM_OUTPUT_COMMON(STREAM, NAME, SIZE, ROUNDED)	\
do {							\
     fputs ("\t.comm ", (STREAM));			\
     assemble_name ((STREAM), (NAME));			\
     fprintf ((STREAM), ",%d\n", (SIZE));		\
} while (0)
/* A C statement (sans semicolon) to output to the stdio stream
   STREAM the assembler definition of a common-label named NAME whose
   size is SIZE bytes.  The variable ROUNDED is the size rounded up
   to whatever alignment the caller wants.

   Use the expression `assemble_name (STREAM, NAME)' to output the
   name itself; before and after that, output the additional
   assembler syntax for defining the name, and a newline.

   This macro controls how the assembler definitions of uninitialized
   common global variables are output.  */

#define ASM_OUTPUT_LOCAL(STREAM, NAME, SIZE, ROUNDED)	\
do {							\
     fputs ("\t.lcomm ", (STREAM));			\
     assemble_name ((STREAM), (NAME));			\
     fprintf ((STREAM), ",%d\n", (SIZE));		\
} while (0)
/* A C statement (sans semicolon) to output to the stdio stream
   STREAM the assembler definition of a local-common-label named NAME
   whose size is SIZE bytes.  The variable ROUNDED is the size
   rounded up to whatever alignment the caller wants.

   Use the expression `assemble_name (STREAM, NAME)' to output the
   name itself; before and after that, output the additional
   assembler syntax for defining the name, and a newline.

   This macro controls how the assembler definitions of uninitialized
   static variables are output.  */

#undef WEAK_ASM_OP
#define WEAK_ASM_OP	".weak"

#undef ASM_DECLARE_FUNCTION_SIZE
#define ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)		\
  do {								\
    if (!flag_inhibit_size_directive)				\
      ASM_OUTPUT_MEASURED_SIZE (FILE, FNAME);			\
  } while (0)
/* A C statement (sans semicolon) to output to the stdio stream
   STREAM any text necessary for declaring the size of a function
   which is being defined.  The argument NAME is the name of the
   function.  The argument DECL is the `FUNCTION_DECL' tree node
   representing the function.

   If this macro is not defined, then the function size is not
   defined.  */

#define ESCAPES \
"\1\1\1\1\1\1\1\1btn\1fr\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\
\0\0\"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\
\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\\\0\0\0\
\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\1\
\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\
\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\
\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\
\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"
/* A table of bytes codes used by the ASM_OUTPUT_ASCII and
   ASM_OUTPUT_LIMITED_STRING macros.  Each byte in the table
   corresponds to a particular byte value [0..255].  For any
   given byte value, if the value in the corresponding table
   position is zero, the given character can be output directly.
   If the table value is 1, the byte must be output as a \ooo
   octal escape.  If the tables value is anything else, then the
   byte value should be output as a \ followed by the value
   in the table.  Note that we can use standard UN*X escape
   sequences for many control characters, but we don't use
   \a to represent BEL because some svr4 assemblers (e.g. on
   the i386) don't know about that.  Also, we don't use \v
   since some versions of gas, such as 2.2 did not accept it.  */

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP ".global\t"

#undef ASM_FORMAT_PRIVATE_NAME
#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),	\
  sprintf ((OUTPUT), "%s.%d", (NAME), (LABELNO)))

/* A C expression to assign to OUTVAR (which is a variable of type
   `char *') a newly allocated string made from the string NAME and
   the number NUMBER, with some suitable punctuation added.  Use
   `alloca' to get space for the string.

   The string will be used as an argument to `ASM_OUTPUT_LABELREF' to
   produce an assembler label for an internal static variable whose
   name is NAME.  Therefore, the string must be such as to result in
   valid assembler code.  The argument NUMBER is different each time
   this macro is executed; it prevents conflicts between
   similarly-named internal static variables in different scopes.

   Ideally this string should not be a valid C identifier, to prevent
   any conflict with the user's own symbols.  Most assemblers allow
   periods or percent signs in assembler symbols; putting at least
   one of these between the name and the number will suffice.  */

#define REGISTER_NAMES	{					\
  "$00","$01","$02","$03","iph","ipl","sph","spl",		\
  "pch","pcl","wreg","status","dph","dpl","$0e","mulh",		\
  "$10","$11","$12","$13","$14","$15","$16","$17",		\
  "$18","$19","$1a","$1b","$1c","$1d","$1e","$1f",		\
  "$20","$21","$22","$23","$24","$25","$26","$27",		\
  "$28","$29","$2a","$2b","$2c","$2d","$2e","$2f",		\
  "$30","$31","$32","$33","$34","$35","$36","$37",		\
  "$38","$39","$3a","$3b","$3c","$3d","$3e","$3f",		\
  "$40","$41","$42","$43","$44","$45","$46","$47",		\
  "$48","$49","$4a","$4b","$4c","$4d","$4e","$4f",		\
  "$50","$51","$52","$53","$54","$55","$56","$57",		\
  "$58","$59","$5a","$5b","$5c","$5d","$5e","$5f",		\
  "$60","$61","$62","$63","$64","$65","$66","$67",		\
  "$68","$69","$6a","$6b","$6c","$6d","$6e","$6f",		\
  "$70","$71","$72","$73","$74","$75","$76","$77",		\
  "$78","$79","$7a","$7b","$7c","$7d","callh","calll",		\
  "$80","$81","$82","$83","$84","$85","$86","$87",		\
  "$88","$89","$8a","$8b","$8c","$8d","$8e","$8f",		\
  "$90","$91","$92","$93","$94","$95","$96","$97",		\
  "$98","$99","$9a","$9b","$9c","$9d","$9e","$9f",		\
  "$a0","$a1","$a2","$a3","$a4","$a5","$a6","$a7",		\
  "$a8","$a9","$aa","$ab","$ac","$ad","$ae","$af",		\
  "$b0","$b1","$b2","$b3","$b4","$b5","$b6","$b7",		\
  "$b8","$b9","$ba","$bb","$bc","$bd","$be","$bf",		\
  "$c0","$c1","$c2","$c3","$c4","$c5","$c6","$c7",		\
  "$c8","$c9","$ca","$cb","$cc","$cd","$ce","$cf",		\
  "$d0","$d1","$d2","$d3","$d4","$d5","$d6","$d7",		\
  "$d8","$d9","$da","$db","$dc","$dd","$de","$df",		\
  "$e0","$e1","$e2","$e3","$e4","$e5","$e6","$e7",		\
  "$e8","$e9","$ea","$eb","$ec","$ed","$ee","$ef",		\
  "$f0","$f1","$f2","$f3","$f4","$f5","$f6","$f7",		\
  "$f8","$f9","$fa","$fb","$fc","$fd","$fe","$ff",		\
  "vfph","vfpl","vaph","vapl"}

/* A C initializer containing the assembler's names for the machine
   registers, each one as a C string constant.  This is what
   translates register numbers in the compiler into assembler
   language.  */

#define PRINT_OPERAND(STREAM, X, CODE) \
  print_operand ((STREAM), (X), (CODE))
/* A C compound statement to output to stdio stream STREAM the
   assembler syntax for an instruction operand X.  X is an RTL
   expression.

   CODE is a value that can be used to specify one of several ways of
   printing the operand.  It is used when identical operands must be
   printed differently depending on the context.  CODE comes from the
   `%' specification that was used to request printing of the
   operand.  If the specification was just `%DIGIT' then CODE is 0;
   if the specification was `%LTR DIGIT' then CODE is the ASCII code
   for LTR.

   If X is a register, this macro should print the register's name.
   The names can be found in an array `reg_names' whose type is `char
   *[]'.  `reg_names' is initialized from `REGISTER_NAMES'.

   When the machine description has a specification `%PUNCT' (a `%'
   followed by a punctuation character), this macro is called with a
   null pointer for X and the punctuation character for CODE.  */

#define PRINT_OPERAND_PUNCT_VALID_P(CODE) \
  ((CODE) == '<' || (CODE) == '>')

/* A C expression which evaluates to true if CODE is a valid
   punctuation character for use in the `PRINT_OPERAND' macro.  If
   `PRINT_OPERAND_PUNCT_VALID_P' is not defined, it means that no
   punctuation characters (except for the standard one, `%') are used
   in this way.  */

#define PRINT_OPERAND_ADDRESS(STREAM, X) print_operand_address(STREAM, X)
/* A C compound statement to output to stdio stream STREAM the
   assembler syntax for an instruction operand that is a memory
   reference whose address is X.  X is an RTL expression.

   On some machines, the syntax for a symbolic address depends on the
   section that the address refers to.  On these machines, define the
   macro `ENCODE_SECTION_INFO' to store the information into the
   `symbol_ref', and then check for it here.  *Note Assembler
   Format::.  */

/* Since register names don't have a prefix, we must preface all
   user identifiers with the '_' to prevent confusion.  */

#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX "_"
#define LOCAL_LABEL_PREFIX ".L"
/* `LOCAL_LABEL_PREFIX'
   `REGISTER_PREFIX'
   `IMMEDIATE_PREFIX'
   If defined, C string expressions to be used for the `%R', `%L',
   `%U', and `%I' options of `asm_fprintf' (see `final.c').  These
   are useful when a single `md' file must support multiple assembler
   formats.  In that case, the various `tm.h' files can define these
   macros differently.  */


#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL)		\
  asm_fprintf ((STREAM), "\tpage\t%L%d\n\tjmp\t%L%d\n", (VALUE), (VALUE))

/* elfos.h presumes that we will want switch/case dispatch tables aligned.
   This is not so for the ip2k.  */
#undef ASM_OUTPUT_CASE_LABEL

#undef ASM_OUTPUT_ADDR_VEC_ELT
#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE)				\
  asm_fprintf ((STREAM), "\tpage\t%L%d\n\tjmp\t%L%d\n", (VALUE), (VALUE))

/* This macro should be provided on machines where the addresses in a
   dispatch table are absolute.

   The definition should be a C statement to output to the stdio
   stream STREAM an assembler pseudo-instruction to generate a
   reference to a label.  VALUE is the number of an internal label
   whose definition is output using `ASM_OUTPUT_INTERNAL_LABEL'.  For
   example,

   fprintf ((STREAM), "\t.word L%d\n", (VALUE))  */

#define ASM_OUTPUT_ALIGN(STREAM, POWER) \
  fprintf ((STREAM), "\t.align %d\n", (POWER))
/* A C statement to output to the stdio stream STREAM an assembler
   command to advance the location counter to a multiple of 2 to the
   POWER bytes.  POWER will be a C expression of type `int'.  */

/* Since instructions are 16 bit word addresses, we should lie and claim that
   the dispatch vectors are in QImode.  Otherwise the offset into the jump
   table will be scaled by the MODE_SIZE.  */

#define CASE_VECTOR_MODE QImode
/* An alias for a machine mode name.  This is the machine mode that
   elements of a jump-table should have.  */


/* `CASE_VALUES_THRESHOLD'
   Define this to be the smallest number of different values for
   which it is best to use a jump-table instead of a tree of
   conditional branches.  The default is four for machines with a
   `casesi' instruction and five otherwise.  This is best for most
   machines.  */

#undef WORD_REGISTER_OPERATIONS
/* Define this macro if operations between registers with integral
   mode smaller than a word are always performed on the entire
   register.  Most RISC machines have this property and most CISC
   machines do not.  */

#define MOVE_MAX 1
/* The maximum number of bytes that a single instruction can move
   quickly between memory and registers or between two memory
   locations.  */

#define MOVE_RATIO		3
/* MOVE_RATIO is the number of move instructions that is better than a
   block move.  Make this small on the IP2k, since the code size grows very
   large with each move.  */

#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1
/* A C expression which is nonzero if on this machine it is safe to
   "convert" an integer of INPREC bits to one of OUTPREC bits (where
   OUTPREC is smaller than INPREC) by merely operating on it as if it
   had only OUTPREC bits.

   On many machines, this expression can be 1.

   When `TRULY_NOOP_TRUNCATION' returns 1 for a pair of sizes for
   modes for which `MODES_TIEABLE_P' is 0, suboptimal code can result.
   If this is the case, making `TRULY_NOOP_TRUNCATION' return 0 in
   such cases may improve things.  */

#define Pmode HImode
/* An alias for the machine mode for pointers.  On most machines,
   define this to be the integer mode corresponding to the width of a
   hardware pointer; `SImode' on 32-bit machine or `DImode' on 64-bit
   machines.  On some machines you must define this to be one of the
   partial integer modes, such as `PSImode'.

   The width of `Pmode' must be at least as large as the value of
   `POINTER_SIZE'.  If it is not equal, you must define the macro
   `POINTERS_EXTEND_UNSIGNED' to specify how pointers are extended to
   `Pmode'.  */

#define FUNCTION_MODE HImode
/* An alias for the machine mode used for memory references to
   functions being called, in `call' RTL expressions.  On most
   machines this should be `QImode'.  */

#define INTEGRATE_THRESHOLD(DECL) \
  (1 + (3 * list_length (DECL_ARGUMENTS (DECL)) / 2))
/* A C expression for the maximum number of instructions above which
   the function DECL should not be inlined.  DECL is a
   `FUNCTION_DECL' node.

   The default definition of this macro is 64 plus 8 times the number
   of arguments that the function accepts.  Some people think a larger
   threshold should be used on RISC machines.  */

#define DOLLARS_IN_IDENTIFIERS 0
/* Define this macro to control use of the character `$' in identifier
   names.  0 means `$' is not allowed by default; 1 means it is
   allowed.  1 is the default; there is no need to define this macro
   in that case.  This macro controls the compiler proper; it does
   not affect the preprocessor.  */

#define MACHINE_DEPENDENT_REORG(INSN) machine_dependent_reorg (INSN)
/* In rare cases, correct code generation requires extra machine
   dependent processing between the second jump optimization pass and
   delayed branch scheduling.  On those machines, define this macro
   as a C statement to act on the code starting at INSN.  */

extern int ip2k_reorg_in_progress;
/* Flag if we're in the middle of IP2k-specific reorganization.  */

extern int ip2k_reorg_completed;
/* Flag if we've completed our IP2k-specific reorganization.  If we have
   then we allow quite a few more tricks than before.  */

extern int ip2k_reorg_split_dimode;
extern int ip2k_reorg_split_simode;
extern int ip2k_reorg_split_qimode;
extern int ip2k_reorg_split_himode;
/* Flags for various split operations that we run in sequence.  */

extern int ip2k_reorg_merge_qimode;
/* Flag to indicate that it's safe to merge QImode operands.  */

#define GIV_SORT_CRITERION(X, Y)			\
  do {							\
    if (GET_CODE ((X)->add_val) == CONST_INT		\
        && GET_CODE ((Y)->add_val) == CONST_INT)	\
      return INTVAL ((X)->add_val) - INTVAL ((Y)->add_val); \
  } while (0)

/* In some cases, the strength reduction optimization pass can
   produce better code if this is defined.  This macro controls the
   order that induction variables are combined.  This macro is
   particularly useful if the target has limited addressing modes.
   For instance, the SH target has only positive offsets in
   addresses.  Thus sorting to put the smallest address first allows
   the most combinations to be found.  */

#define TRAMPOLINE_TEMPLATE(FILE) abort ()

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE 4

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)			\
{									\
  emit_move_insn (gen_rtx_MEM (HImode, plus_constant ((TRAMP), 2)),	\
		   	   CXT);    					\
  emit_move_insn (gen_rtx_MEM (HImode, plus_constant ((TRAMP), 6)),	\
			   FNADDR);					\
}
/* Store in cc_status the expressions
   that the condition codes will describe
   after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

#define NOTICE_UPDATE_CC(EXP, INSN) (void)(0)

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)  \
  fprintf ((FILE), "/* profiler %d */", (LABELNO))

#define TARGET_MEM_FUNCTIONS
/* Define this macro if GNU CC should generate calls to the System V
   (and ANSI C) library functions `memcpy' and `memset' rather than
   the BSD functions `bcopy' and `bzero'.  */


#undef ENDFILE_SPEC
#undef LINK_SPEC
#undef STARTFILE_SPEC

/* Another C string constant used much like `LINK_SPEC'.  The
   difference between the two is that `ENDFILE_SPEC' is used at the
   very end of the command given to the linker.

   Do not define this macro if it does not need to do anything.  */

#if defined(__STDC__) || defined(ALMOST_STDC)
#define AS2(a,b,c) #a "\t" #b "," #c
#define AS1(a,b) #a "\t" #b
#else
#define AS1(a,b) "a	b"
#define AS2(a,b,c) "a	b,c"
#endif
#define OUT_AS1(a,b) output_asm_insn (AS1 (a,b), operands)
#define OUT_AS2(a,b,c) output_asm_insn (AS2 (a,b,c), operands)
#define CR_TAB "\n\t"

/* Define this macro as a C statement that declares additional library
   routines renames existing ones. `init_optabs' calls this macro
   after initializing all the normal library routines.  */

#define INIT_TARGET_OPTABS				\
{							\
  smul_optab->handlers[(int) SImode].libfunc		\
    = gen_rtx_SYMBOL_REF (Pmode, "_mulsi3");		\
							\
  smul_optab->handlers[(int) DImode].libfunc		\
    = gen_rtx_SYMBOL_REF (Pmode, "_muldi3");		\
							\
  cmp_optab->handlers[(int) HImode].libfunc		\
    = gen_rtx_SYMBOL_REF (Pmode, "_cmphi2");		\
							\
  cmp_optab->handlers[(int) SImode].libfunc		\
    = gen_rtx_SYMBOL_REF (Pmode, "_cmpsi2");		\
}

#define PREDICATE_CODES					\
  {"ip2k_ip_operand", {MEM}},				\
  {"ip2k_short_operand", {MEM}},			\
  {"ip2k_gen_operand", {MEM, REG, SUBREG}},		\
  {"ip2k_nonptr_operand", {REG, SUBREG}},		\
  {"ip2k_ptr_operand", {REG, SUBREG}},			\
  {"ip2k_split_dest_operand", {REG, SUBREG, MEM}}, 	\
  {"ip2k_sp_operand", {REG}},				\
  {"ip2k_nonsp_reg_operand", {REG, SUBREG}}, 		\
  {"ip2k_symbol_ref_operand", {SYMBOL_REF}}, 		\
  {"ip2k_binary_operator", {PLUS, MINUS, MULT, DIV,	\
			    UDIV, MOD, UMOD, AND, IOR,	\
			    XOR, COMPARE, ASHIFT,	\
			    ASHIFTRT, LSHIFTRT}},	\
  {"ip2k_unary_operator", {NEG, NOT, SIGN_EXTEND,	\
			   ZERO_EXTEND}},		\
  {"ip2k_unsigned_comparison_operator", {LTU, GTU, NE,	\
					 EQ, LEU, GEU}},\
  {"ip2k_signed_comparison_operator", {LT, GT, LE, GE}},

#define DWARF2_DEBUGGING_INFO 1

#define DWARF2_ASM_LINE_DEBUG_INFO	1

#define DBX_REGISTER_NUMBER(REGNO)	(REGNO)

/* Miscellaneous macros to describe machine specifics. */

#define STORE_FLAG_VALUE	1

#define IS_PSEUDO_P(R)	(REGNO (R) >= FIRST_PSEUDO_REGISTER)

/* Default calculations would cause DWARF address sizes to be 2 bytes,
   but the Harvard architecture of the IP2k and the word-addressed 64k
   of instruction memory causes us to want a 32-bit "address" field.  */
#undef DWARF2_ADDR_SIZE
#define DWARF2_ADDR_SIZE	4

