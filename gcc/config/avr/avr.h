/* Definitions of target machine for GNU compiler,
   for ATMEL AVR at90s8515, ATmega103/103L, ATmega603/603L microcontrollers.

   Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.
   Contributed by Denis Chertykov (denisc@overta.ru)

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

/* Names to predefine in the preprocessor for this target machine. */

#define CPP_PREDEFINES "-DAVR"
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


/* This declaration should be present. */
extern int target_flags;

#define TARGET_ORDER_1		(target_flags &  0x1000)
#define TARGET_ORDER_2		(target_flags &  0x4000)
#define TARGET_INT8  		(target_flags & 0x10000)
#define TARGET_NO_INTERRUPTS	(target_flags & 0x20000)
#define TARGET_INSN_SIZE_DUMP	(target_flags &  0x2000)
#define TARGET_CALL_PROLOGUES	(target_flags & 0x40000)

/* Dump each assembler insn's rtl into the output file.
   This is for debugging the compiler itself.  */

#define TARGET_RTL_DUMP		(target_flags &   0x010)
#define TARGET_ALL_DEBUG 	(target_flags &   0xfe0)

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



#define TARGET_SWITCHES {						\
  {"order1",0x1000, NULL},						\
  {"order2",0x4000, NULL},						\
  {"int8",0x10000,"Assume int to be 8 bit integer"},			\
  {"no-interrupts",0x20000,"Don't output interrupt compatible code"},	\
  {"call-prologues",0x40000,						\
   "Use subroutines for functions prologeu/epilogue"},			\
  {"rtl",0x10, NULL},							\
  {"size",0x2000,"Output instruction size's to the asm file"},		\
  {"deb",0xfe0, NULL},							\
  {"",0, NULL}}
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

extern const char *avr_ram_end;
extern const char *avr_mcu_name;

struct mcu_type_s {
  char * name;
  int stack;
  int mega;
 };

extern struct mcu_type_s *avr_mcu_type;
#define AVR_MEGA (avr_mcu_type->mega)

#define TARGET_OPTIONS {						      \
 {"init-stack=",&avr_ram_end,"Specify the initial stack address" },	      \
 {"mcu=", &avr_mcu_name,						      \
  "Specify the MCU name (at90s23xx,attiny22,at90s44xx,at90s85xx,atmega603,atmega103)"}}
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

#define TARGET_VERSION fprintf (stderr, " (GNU assembler syntax)");
/* This macro is a C statement to print on `stderr' a string
   describing the particular machine description choice.  Every
   machine description should define `TARGET_VERSION'.  For example:

   #ifdef MOTOROLA
   #define TARGET_VERSION \
   fprintf (stderr, " (68k, Motorola syntax)");
   #else
   #define TARGET_VERSION \
   fprintf (stderr, " (68k, MIT syntax)");
   #endif  */

#define OVERRIDE_OPTIONS avr_override_options()
/* `OVERRIDE_OPTIONS'
   Sometimes certain combinations of command options do not make
   sense on a particular target machine.  You can define a macro
   `OVERRIDE_OPTIONS' to take account of this.  This macro, if
   defined, is executed once just after all the command options have
   been parsed.

   Don't use this macro to turn on various extra optimizations for
   `-O'.  That is what `OPTIMIZATION_OPTIONS' is for.  */

#define CAN_DEBUG_WITHOUT_FP
/* Define this macro if debugging can be performed even without a
   frame pointer.  If this macro is defined, GNU CC will turn on the
   `-fomit-frame-pointer' option whenever `-O' is specified.  */

/* Define this if most significant byte of a word is the lowest numbered. */
#define BITS_BIG_ENDIAN 0

/* Define this if most significant byte of a word is the lowest numbered. */
#define BYTES_BIG_ENDIAN 0

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
#define WORDS_BIG_ENDIAN 0

/* number of bits in an addressable storage unit */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';  */
#define BITS_PER_WORD 8

/* Width of a word, in units (bytes). */
#define UNITS_PER_WORD 1

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE 16


/* Maximum sized of reasonable data type
   DImode or Dfmode ...  */
#define MAX_FIXED_MODE_SIZE 32

/* Allocation boundary (in *bits*) for storing arguments in argument list. */
#define PARM_BOUNDARY 8

/* Allocation boundary (in *bits*) for the code of a function. */
#define FUNCTION_BOUNDARY 8

/* Alignment of field after `int : 0' in a structure. */
#define EMPTY_FIELD_BOUNDARY 8

/* No data type wants to be aligned rounder than this. */
#define BIGGEST_ALIGNMENT 8


/* Define this if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 0

/* A C expression for the size in bits of the type `int' on the
     target machine.  If you don't define this, the default is one word.  */
#define INT_TYPE_SIZE (TARGET_INT8 ? 8 : 16)


/* A C expression for the size in bits of the type `short' on the
   target machine.  If you don't define this, the default is half a
   word.  (If this would be less than one storage unit, it is rounded
   up to one unit.)  */
#define SHORT_TYPE_SIZE (INT_TYPE_SIZE == 8 ? INT_TYPE_SIZE : 16)

/* A C expression for the size in bits of the type `long' on the
   target machine.  If you don't define this, the default is one word.  */
#define LONG_TYPE_SIZE (INT_TYPE_SIZE == 8 ? 16 : 32)

#define MAX_LONG_TYPE_SIZE 32
/* Maximum number for the size in bits of the type `long' on the
   target machine.  If this is undefined, the default is
   `LONG_TYPE_SIZE'.  Otherwise, it is the constant value that is the
   largest value that `LONG_TYPE_SIZE' can have at run-time.  This is
   used in `cpp'.  */


#define LONG_LONG_TYPE_SIZE 64
/* A C expression for the size in bits of the type `long long' on the
   target machine.  If you don't define this, the default is two
   words.  If you want to support GNU Ada on your machine, the value
   of macro must be at least 64.  */


#define  CHAR_TYPE_SIZE 8
/* A C expression for the size in bits of the type `char' on the
   target machine.  If you don't define this, the default is one
   quarter of a word.  (If this would be less than one storage unit,
   it is rounded up to one unit.)  */

#define FLOAT_TYPE_SIZE 32
/* A C expression for the size in bits of the type `float' on the
   target machine.  If you don't define this, the default is one word.  */

#define DOUBLE_TYPE_SIZE 32
/* A C expression for the size in bits of the type `double' on the
   target machine.  If you don't define this, the default is two
   words. */


#define LONG_DOUBLE_TYPE_SIZE 32
/* A C expression for the size in bits of the type `long double' on
   the target machine.  If you don't define this, the default is two
   words.  */

#define DEFAULT_SIGNED_CHAR 1
/* An expression whose value is 1 or 0, according to whether the type
   `char' should be signed or unsigned by default.  The user can
   always override this default with the options `-fsigned-char' and
   `-funsigned-char'.  */

/* `DEFAULT_SHORT_ENUMS'
   A C expression to determine whether to give an `enum' type only as
   many bytes as it takes to represent the range of possible values
   of that type.  A nonzero value means to do that; a zero value
   means all `enum' types should be allocated like `int'.

   If you don't define the macro, the default is 0.  */

#define SIZE_TYPE (INT_TYPE_SIZE == 8 ? "long unsigned int" : "unsigned int")
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

#define PTRDIFF_TYPE (INT_TYPE_SIZE == 8 ? "long unsigned int" :"unsigned int")
/* A C expression for a string describing the name of the data type
   to use for the result of subtracting two pointers.  The typedef
   name `ptrdiff_t' is defined using the contents of the string.  See
   `SIZE_TYPE' above for more information.
   
   If you don't define this macro, the default is `"long int"'.  */


#define WCHAR_TYPE_SIZE 16
/* A C expression for the size in bits of the data type for wide
   characters.  This is used in `cpp', which cannot make use of
   `WCHAR_TYPE'.  */

#define FIRST_PSEUDO_REGISTER 36
/* Number of hardware registers known to the compiler.  They receive
   numbers 0 through `FIRST_PSEUDO_REGISTER-1'; thus, the first
   pseudo register's number really is assigned the number
   `FIRST_PSEUDO_REGISTER'.  */

#define FIXED_REGISTERS {\
  1,1,/* r0 r1 */\
  0,0,/* r2 r3 */\
  0,0,/* r4 r5 */\
  0,0,/* r6 r7 */\
  0,0,/* r8 r9 */\
  0,0,/* r10 r11 */\
  0,0,/* r12 r13 */\
  0,0,/* r14 r15 */\
  0,0,/* r16 r17 */\
  0,0,/* r18 r19 */\
  0,0,/* r20 r21 */\
  0,0,/* r22 r23 */\
  0,0,/* r24 r25 */\
  0,0,/* r26 r27 */\
  0,0,/* r28 r29 */\
  0,0,/* r30 r31 */\
  1,1,/*  STACK */\
  1,1 /* arg pointer */  }
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
  1,1,/* r0 r1 */				\
    0,0,/* r2 r3 */				\
    0,0,/* r4 r5 */				\
    0,0,/* r6 r7 */				\
    0,0,/* r8 r9 */				\
    0,0,/* r10 r11 */				\
    0,0,/* r12 r13 */				\
    0,0,/* r14 r15 */				\
    0,0,/* r16 r17 */				\
    1,1,/* r18 r19 */				\
    1,1,/* r20 r21 */				\
    1,1,/* r22 r23 */				\
    1,1,/* r24 r25 */				\
    1,1,/* r26 r27 */				\
    0,0,/* r28 r29 */				\
    1,1,/* r30 r31 */				\
    1,1,/*  STACK */				\
    1,1 /* arg pointer */  }
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
    24,25,					\
    18,19,					\
    20,21,					\
    22,23,					\
    30,31,					\
    26,27,					\
    28,29,					\
    17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,	\
    0,1,					\
    32,33,34,35					\
    }
/* If defined, an initializer for a vector of integers, containing the
   numbers of hard registers in the order in which GNU CC should
   prefer to use them (from most preferred to least).
   
   If this macro is not defined, registers are used lowest numbered
   first (all else being equal).
   
   One use of this macro is on machines where the highest numbered
   registers must always be saved and the save-multiple-registers
   instruction supports only sequences of consetionve registers.  On
   such machines, define `REG_ALLOC_ORDER' to be an initializer that
   lists the highest numbered allocatable register first. */

#define ORDER_REGS_FOR_LOCAL_ALLOC order_regs_for_local_alloc ()
/* ORDER_REGS_FOR_LOCAL_ALLOC'
   A C statement (sans semicolon) to choose the order in which to
   allocate hard registers for pseudo-registers local to a basic
   block.

   Store the desired register order in the array `reg_alloc_order'.
   Element 0 should be the register to allocate first; element 1, the
   next register; and so on.

   The macro body should not assume anything about the contents of
   `reg_alloc_order' before execution of the macro.

   On most machines, it is not necessary to define this macro.  */


#define HARD_REGNO_NREGS(REGNO, MODE) ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* A C expression for the number of consecutive hard registers,
   starting at register number REGNO, required to hold a value of mode
   MODE.

   On a machine where all registers are exactly one word, a suitable
   definition of this macro is

   #define HARD_REGNO_NREGS(REGNO, MODE)            \
   ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1)  \
   / UNITS_PER_WORD))  */

#define HARD_REGNO_MODE_OK(REGNO, MODE) (((REGNO) >= 24 && (MODE) != QImode) \
					 ? ! ((REGNO) & 1)		     \
					 : 1)
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

#define MODES_TIEABLE_P(MODE1, MODE2) 0
/* A C expression that is nonzero if it is desirable to choose
   register allocation so as to avoid move instructions between a
   value of mode MODE1 and a value of mode MODE2.

   If `HARD_REGNO_MODE_OK (R, MODE1)' and `HARD_REGNO_MODE_OK (R,
   MODE2)' are ever different for any R, then `MODES_TIEABLE_P (MODE1,
   MODE2)' must be zero.  */

enum reg_class {
  NO_REGS,
  R0_REG,			/* r0 */
  POINTER_X_REGS,		/* r26 - r27 */
  POINTER_Y_REGS,		/* r28 - r29 */
  POINTER_Z_REGS,		/* r30 - r31 */
  STACK_REG,			/* STACK */
  BASE_POINTER_REGS,		/* r28 - r31 */
  POINTER_REGS,			/* r26 - r31 */
  ADDW_REGS,			/* r24 - r31 */
  SIMPLE_LD_REGS,		/* r16 - r23 */
  LD_REGS,			/* r16 - r31 */
  NO_LD_REGS,			/* r0 - r15 */
  GENERAL_REGS,			/* r0 - r31 */
  ALL_REGS, LIM_REG_CLASSES
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

#define REG_CLASS_NAMES {					\
		 "NO_REGS",					\
		   "R0_REG",	/* r0 */                        \
		   "POINTER_X_REGS", /* r26 - r27 */		\
		   "POINTER_Y_REGS", /* r28 - r29 */		\
		   "POINTER_Z_REGS", /* r30 - r31 */		\
		   "STACK_REG",	/* STACK */			\
		   "BASE_POINTER_REGS",	/* r28 - r31 */		\
		   "POINTER_REGS", /* r26 - r31 */		\
		   "ADDW_REGS",	/* r24 - r31 */			\
                   "SIMPLE_LD_REGS", /* r16 - r23 */            \
		   "LD_REGS",	/* r16 - r31 */			\
                   "NO_LD_REGS", /* r0 - r15 */                 \
		   "GENERAL_REGS", /* r0 - r31 */		\
		   "ALL_REGS" }
/* An initializer containing the names of the register classes as C
   string constants.  These names are used in writing some of the
   debugging dumps.  */

#define REG_X 26
#define REG_Y 28
#define REG_Z 30
#define REG_W 24

#define REG_CLASS_CONTENTS {						\
  {0x00000000,0x00000000},	/* NO_REGS */				\
  {0x00000001,0x00000000},	/* R0_REG */                            \
  {3 << REG_X,0x00000000},      /* POINTER_X_REGS, r26 - r27 */		\
  {3 << REG_Y,0x00000000},      /* POINTER_Y_REGS, r28 - r29 */		\
  {3 << REG_Z,0x00000000},      /* POINTER_Z_REGS, r30 - r31 */		\
  {0x00000000,0x00000003},	/* STACK_REG, STACK */			\
  {(3 << REG_Y) | (3 << REG_Z),						\
     0x00000000},		/* BASE_POINTER_REGS, r28 - r31 */	\
  {(3 << REG_X) | (3 << REG_Y) | (3 << REG_Z),				\
     0x00000000},		/* POINTER_REGS, r26 - r31 */		\
  {(3 << REG_X) | (3 << REG_Y) | (3 << REG_Z) | (3 << REG_W),		\
     0x00000000},		/* ADDW_REGS, r24 - r31 */		\
  {0x00ff0000,0x00000000}, 	/* SIMPLE_LD_REGS r16 - r23 */          \
  {(3 << REG_X)|(3 << REG_Y)|(3 << REG_Z)|(3 << REG_W)|(0xff << 16),	\
     0x00000000},	/* LD_REGS, r16 - r31 */			\
  {0x0000ffff,0x00000000}, 	/* NO_LD_REGS  r0 - r15 */              \
  {0xffffffffu,0x00000000},	/* GENERAL_REGS, r0 - r31 */		\
  {0xffffffffu,0x00000003}	/* ALL_REGS */				\
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

#define REGNO_REG_CLASS(R) avr_regno_reg_class(R)
/* A C expression whose value is a register class containing hard
   register REGNO.  In general there is more than one such class;
   choose a class which is "minimal", meaning that no smaller class
   also contains the register.  */

#define BASE_REG_CLASS POINTER_REGS
/* A macro whose definition is the name of the class to which a valid
   base register must belong.  A base register is one used in an
   address which is the register value plus a displacement.  */

#define INDEX_REG_CLASS NO_REGS
/* A macro whose definition is the name of the class to which a valid
   index register must belong.  An index register is one used in an
   address where its value is either multiplied by a scale factor or
   added to another register (as well as added to a displacement).  */

#define REG_CLASS_FROM_LETTER(C) avr_reg_class_from_letter(C)
/* A C expression which defines the machine-dependent operand
   constraint letters for register classes.  If CHAR is such a
   letter, the value should be the register class corresponding to
   it.  Otherwise, the value should be `NO_REGS'.  The register
   letter `r', corresponding to class `GENERAL_REGS', will not be
   passed to this macro; you do not need to handle it.  */

#define REGNO_OK_FOR_BASE_P(r) (((r) < FIRST_PSEUDO_REGISTER		\
				 && ((r) == REG_X			\
				     || (r) == REG_Y			\
				     || (r) == REG_Z			\
				     || (r) == ARG_POINTER_REGNUM))	\
				|| (reg_renumber			\
				    && (reg_renumber[r] == REG_X	\
					|| reg_renumber[r] == REG_Y	\
					|| reg_renumber[r] == REG_Z	\
					|| (reg_renumber[r]		\
					    == ARG_POINTER_REGNUM))))
/* A C expression which is nonzero if register number NUM is suitable
   for use as a base register in operand addresses.  It may be either
   a suitable hard register or a pseudo register that has been
   allocated such a hard register.  */

/* #define REGNO_MODE_OK_FOR_BASE_P(r, m) regno_mode_ok_for_base_p(r, m)
   A C expression that is just like `REGNO_OK_FOR_BASE_P', except that
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

#define PREFERRED_RELOAD_CLASS(X, CLASS) preferred_reload_class(X,CLASS)
/* A C expression that places additional restrictions on the register
   class to use when it is necessary to copy value X into a register
   in class CLASS.  The value is a register class; perhaps CLASS, or
   perhaps another, smaller class.  On many machines, the following
   definition is safe:

   #define PREFERRED_RELOAD_CLASS(X,CLASS) CLASS

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
   certain symbolic address on the Sparc when compiling PIC).  In
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
   memory as a intermediate storage.  This case often occurs between
   floating-point and general registers.  */

/* `SECONDARY_MEMORY_NEEDED (CLASS1, CLASS2, M)'
   Certain machines have the property that some registers cannot be
   copied to some other registers without using memory.  Define this
   macro on those machines to be a C expression that is non-zero if
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

   Define `SMALL_REGISTER_CLASSES' to be an expression with a non-zero
   value on these machines.  When this macro has a non-zero value, the
   compiler allows registers explicitly used in the rtl to be used as
   spill registers but avoids extending the lifetime of these
   registers.

   It is always safe to define this macro with a non-zero value, but
   if you unnecessarily define it, you will reduce the amount of
   optimizations that can be performed in some cases.  If you do not
   define this macro with a non-zero value when it is required, the
   compiler will run out of spill registers and print a fatal error
   message.  For most machines, you should not define this macro at
   all.  */

#define CLASS_LIKELY_SPILLED_P(c) class_likely_spilled_p(c)
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

#define CLASS_MAX_NREGS(CLASS, MODE)   class_max_nregs (CLASS, MODE)
/* A C expression for the maximum number of consecutive registers of
   class CLASS needed to hold a value of mode MODE.

   This is closely related to the macro `HARD_REGNO_NREGS'.  In fact,
   the value of the macro `CLASS_MAX_NREGS (CLASS, MODE)' should be
   the maximum value of `HARD_REGNO_NREGS (REGNO, MODE)' for all
   REGNO values in the class CLASS.

   This macro helps control the handling of multiple-word values in
   the reload pass.  */

#undef CLASS_CANNOT_CHANGE_SIZE
/* `CLASS_CANNOT_CHANGE_SIZE'
   If defined, a C expression for a class that contains registers
   which the compiler must always access in a mode that is the same
   size as the mode in which it loaded the register.

   For the example, loading 32-bit integer or floating-point objects
   into floating-point registers on the Alpha extends them to 64-bits.
   Therefore loading a 64-bit object and then storing it as a 32-bit
   object does not store the low-order 32-bits, as would be the case
   for a normal register.  Therefore, `alpha.h' defines this macro as
   `FLOAT_REGS'.

   Three other special macros describe which operands fit which
   constraint letters.  */

#define CONST_OK_FOR_LETTER_P(VALUE, C)				\
  ((C) == 'I' ? (VALUE) >= 0 && (VALUE) <= 63 :			\
   (C) == 'J' ? (VALUE) <= 0 && (VALUE) >= -63:			\
   (C) == 'K' ? (VALUE) == 2 :					\
   (C) == 'L' ? (VALUE) == 0 :					\
   (C) == 'M' ? (VALUE) >= 0 && (VALUE) <= 0xff :		\
   (C) == 'N' ? (VALUE) == -1:					\
   (C) == 'O' ? (VALUE) == 8 || (VALUE) == 16 || (VALUE) == 24:	\
   (C) == 'P' ? (VALUE) == 1 :					\
   0)

/* A C expression that defines the machine-dependent operand
   constraint letters (`I', `J', `K', ... `P') that specify
   particular ranges of integer values.  If C is one of those
   letters, the expression should check that VALUE, an integer, is in
   the appropriate range and return 1 if so, 0 otherwise.  If C is
   not one of those letters, the value should be 0 regardless of
   VALUE.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C) \
  ((C) == 'G' ? (VALUE) == CONST0_RTX (SFmode)	\
   : 0)
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

#define EXTRA_CONSTRAINT(x, c) extra_constraint(x, c)
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

/*  This is an undocumented variable which describes
    how GCC will push a data */
#define STACK_PUSH_CODE POST_DEC

#define STACK_GROWS_DOWNWARD
/* Define this macro if pushing a word onto the stack moves the stack
   pointer to a smaller address.

   When we say, "define this macro if ...," it means that the
   compiler checks this macro only with `#ifdef' so the precise
   definition used does not matter.  */

#define STARTING_FRAME_OFFSET 1
/* Offset from the frame pointer to the first local variable slot to
   be allocated.

   If `FRAME_GROWS_DOWNWARD', find the next slot's offset by
   subtracting the first slot's length from `STARTING_FRAME_OFFSET'.
   Otherwise, it is found by adding the length of the first slot to
   the value `STARTING_FRAME_OFFSET'.  */

#define STACK_POINTER_OFFSET 1
/* Offset from the stack pointer register to the first location at
   which outgoing arguments are placed.  If not specified, the
   default value of zero is used.  This is the proper value for most
   machines.

   If `ARGS_GROW_DOWNWARD', this is the offset to the location above
   the first location at which outgoing arguments are placed.  */

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

#define STACK_BOUNDARY 8
/* Define this macro if there is a guaranteed alignment for the stack
   pointer on this machine.  The definition is a C expression for the
   desired alignment (measured in bits).  This value is used as a
   default if PREFERRED_STACK_BOUNDARY is not defined.  */

#define STACK_POINTER_REGNUM 32
/* The register number of the stack pointer register, which must also
   be a fixed register according to `FIXED_REGISTERS'.  On most
   machines, the hardware determines which register this is.  */

#define FRAME_POINTER_REGNUM REG_Y
/* The register number of the frame pointer register, which is used to
   access automatic variables in the stack frame.  On some machines,
   the hardware determines which register this is.  On other
   machines, you can choose any register you wish for this purpose.  */

#define ARG_POINTER_REGNUM 34
/* The register number of the arg pointer register, which is used to
   access the function's argument list.  On some machines, this is
   the same as the frame pointer register.  On some machines, the
   hardware determines which register this is.  On other machines,
   you can choose any register you wish for this purpose.  If this is
   not the same register as the frame pointer register, then you must
   mark it as a fixed register according to `FIXED_REGISTERS', or
   arrange to be able to eliminate it (*note Elimination::.).  */

#define STATIC_CHAIN_REGNUM 2
/* Register numbers used for passing a function's static chain
   pointer.  If register windows are used, the register number as
   seen by the called function is `STATIC_CHAIN_INCOMING_REGNUM',
   while the register number as seen by the calling function is
   `STATIC_CHAIN_REGNUM'.  If these registers are the same,
   `STATIC_CHAIN_INCOMING_REGNUM' need not be defined.

   The static chain register need not be a fixed register.

   If the static chain is passed in memory, these macros should not be
   defined; instead, the next two macros should be defined.  */

#define FRAME_POINTER_REQUIRED frame_pointer_required_p()
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

#define ELIMINABLE_REGS {					\
      {ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM},		\
	{FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}		\
       ,{FRAME_POINTER_REGNUM+1,STACK_POINTER_REGNUM+1}}
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

#define CAN_ELIMINATE(FROM, TO) (((FROM) == ARG_POINTER_REGNUM		   \
				  && (TO) == FRAME_POINTER_REGNUM)	   \
				 || (((FROM) == FRAME_POINTER_REGNUM	   \
				      || (FROM) == FRAME_POINTER_REGNUM+1) \
				     && ! FRAME_POINTER_REQUIRED	   \
				     ))
/* A C expression that returns non-zero if the compiler is allowed to
   try to replace register number FROM-REG with register number
   TO-REG.  This macro need only be defined if `ELIMINABLE_REGS' is
   defined, and will usually be the constant 1, since most of the
   cases preventing register elimination are things that the compiler
   already knows about.  */

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
     OFFSET = initial_elimination_offset (FROM, TO)
/* This macro is similar to `INITIAL_FRAME_POINTER_OFFSET'.  It
   specifies the initial difference between the specified pair of
   registers.  This macro must be defined if `ELIMINABLE_REGS' is
   defined.  */

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

#define RETURN_POPS_ARGS(FUNDECL, FUNTYPE, STACK_SIZE) 0
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

   On the Vax, all functions always pop their arguments, so the
   definition of this macro is STACK-SIZE.  On the 68000, using the
   standard calling convention, no functions pop their arguments, so
   the value of the macro is always 0 in this case.  But an
   alternative calling convention is available in which functions
   that take a fixed number of arguments pop them but other functions
   (such as `printf') pop nothing (the caller pops all).  When this
   convention is in use, FUNTYPE is examined to determine whether a
   function takes a fixed number of arguments.  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) (function_arg (&(CUM), MODE, TYPE, NAMED))
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

   For machines like the Vax and 68000, where normally all arguments
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
   where some arguments are usually passed in registers, is to cause
   nameless arguments to be passed on the stack instead.  This is done
   by making `FUNCTION_ARG' return 0 whenever NAMED is 0.

   You may use the macro `MUST_PASS_IN_STACK (MODE, TYPE)' in the
   definition of this macro to determine if this argument is of a
   type that must be passed in the stack.  If `REG_PARM_STACK_SPACE'
   is not defined and `FUNCTION_ARG' returns non-zero for such an
   argument, the compiler will abort.  If `REG_PARM_STACK_SPACE' is
   defined, the argument will be computed in the stack and then
   loaded into a register.  */

typedef struct avr_args {
  int nregs;			/* # registers available for passing */
  int regno;			/* next available register number */
} CUMULATIVE_ARGS;
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

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT) init_cumulative_args (&(CUM), FNTYPE, LIBNAME, INDIRECT)

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
   never both of them at once.   */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)	\
  (function_arg_advance (&CUM, MODE, TYPE, NAMED))

/* A C statement (sans semicolon) to update the summarizer variable
   CUM to advance past an argument in the argument list.  The values
   MODE, TYPE and NAMED describe that argument.  Once this is done,
   the variable CUM is suitable for analyzing the *following*
   argument with `FUNCTION_ARG', etc.
   
   This macro need not do anything if the argument in question was
   passed on the stack.  The compiler knows how to track the amount
   of stack space used for arguments without any special help. */

#define FUNCTION_ARG_REGNO_P(r) function_arg_regno_p(r)
/* A C expression that is nonzero if REGNO is the number of a hard
   register in which function arguments are sometimes passed.  This
   does *not* include implicit arguments such as the static chain and
   the structure-value address.  On many machines, no registers can be
   used for this purpose since all function arguments are pushed on
   the stack.  */

extern int avr_reg_order[];

#define RET_REGISTER avr_ret_register ()

#define FUNCTION_VALUE(VALTYPE, FUNC) avr_function_value (VALTYPE, FUNC)
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

#define LIBCALL_VALUE(MODE)  avr_libcall_value (MODE)
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

#define FUNCTION_VALUE_REGNO_P(N) ((N) == RET_REGISTER)
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

#define RETURN_IN_MEMORY(TYPE) ((TYPE_MODE (TYPE) == BLKmode)	\
				? int_size_in_bytes (TYPE) > 8	\
				: 0)
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

#define FUNCTION_PROLOGUE(FILE, SIZE) function_prologue (FILE, SIZE)
/* A C compound statement that outputs the assembler code for entry
   to a function.  The prologue is responsible for setting up the
   stack frame, initializing the frame pointer register, saving
   registers that must be saved, and allocating SIZE additional bytes
   of storage for the local variables.  SIZE is an integer.  FILE is
   a stdio stream to which the assembler code should be output.

   The label for the beginning of the function need not be output by
   this macro.  That has already been done when the macro is run.

   To determine which registers to save, the macro can refer to the
   array `regs_ever_live': element R is nonzero if hard register R is
   used anywhere within the function.  This implies the function
   prologue should save register R, provided it is not one of the
   call-used registers.  (`FUNCTION_EPILOGUE' must likewise use
   `regs_ever_live'.)

   On machines that have "register windows", the function entry code
   does not save on the stack the registers that are in the windows,
   even if they are supposed to be preserved by function calls;
   instead it takes appropriate steps to "push" the register stack,
   if any non-call-used registers are used in the function.

   On machines where functions may or may not have frame-pointers, the
   function entry code must vary accordingly; it must set up the frame
   pointer if one is wanted, and not otherwise.  To determine whether
   a frame pointer is in wanted, the macro can refer to the variable
   `frame_pointer_needed'.  The variable's value will be 1 at run
   time in a function that needs a frame pointer.  *Note
   Elimination::.

   The function entry code is responsible for allocating any stack
   space required for the function.  This stack space consists of the
   regions listed below.  In most cases, these regions are allocated
   in the order listed, with the last listed region closest to the
   top of the stack (the lowest address if `STACK_GROWS_DOWNWARD' is
   defined, and the highest address if it is not defined).  You can
   use a different order for a machine if doing so is more convenient
   or required for compatibility reasons.  Except in cases where
   required by standard or by a debugger, there is no reason why the
   stack layout used by GCC need agree with that used by other
   compilers for a machine.

   * A region of `current_function_pretend_args_size' bytes of
   uninitialized space just underneath the first argument
   arriving on the stack.  (This may not be at the very start of
   the allocated stack region if the calling sequence has pushed
   anything else since pushing the stack arguments.  But
   usually, on such machines, nothing else has been pushed yet,
   because the function prologue itself does all the pushing.)
   This region is used on machines where an argument may be
   passed partly in registers and partly in memory, and, in some
   cases to support the features in `varargs.h' and `stdargs.h'.

   * An area of memory used to save certain registers used by the
   function.  The size of this area, which may also include
   space for such things as the return address and pointers to
   previous stack frames, is machine-specific and usually
   depends on which registers have been used in the function.
   Machines with register windows often do not require a save
   area.

   * A region of at least SIZE bytes, possibly rounded up to an
   allocation boundary, to contain the local variables of the
   function.  On some machines, this region and the save area
   may occur in the opposite order, with the save area closer to
   the top of the stack.

   * Optionally, when `ACCUMULATE_OUTGOING_ARGS' is defined, a
   region of `current_function_outgoing_args_size' bytes to be
   used for outgoing argument lists of the function.  *Note
   Stack Arguments::.

   Normally, it is necessary for the macros `FUNCTION_PROLOGUE' and
   `FUNCTION_EPILOGE' to treat leaf functions specially.  The C
   variable `leaf_function' is nonzero for such a function.  */

#define EPILOGUE_USES(REGNO) 0
/* Define this macro as a C expression that is nonzero for registers
   are used by the epilogue or the `return' pattern.  The stack and
   frame pointer registers are already be assumed to be used as
   needed.  */

#define FUNCTION_EPILOGUE(FILE, SIZE) function_epilogue (FILE, SIZE)
/* A C compound statement that outputs the assembler code for exit
   from a function.  The epilogue is responsible for restoring the
   saved registers and stack pointer to their values when the
   function was called, and returning control to the caller.  This
   macro takes the same arguments as the macro `FUNCTION_PROLOGUE',
   and the registers to restore are determined from `regs_ever_live'
   and `CALL_USED_REGISTERS' in the same way.

   On some machines, there is a single instruction that does all the
   work of returning from the function.  On these machines, give that
   instruction the name `return' and do not define the macro
   `FUNCTION_EPILOGUE' at all.

   Do not define a pattern named `return' if you want the
   `FUNCTION_EPILOGUE' to be used.  If you want the target switches
   to control whether return instructions or epilogues are used,
   define a `return' pattern with a validity condition that tests the
   target switches appropriately.  If the `return' pattern's validity
   condition is false, epilogues will be used.

   On machines where functions may or may not have frame-pointers, the
   function exit code must vary accordingly.  Sometimes the code for
   these two cases is completely different.  To determine whether a
   frame pointer is wanted, the macro can refer to the variable
   `frame_pointer_needed'.  The variable's value will be 1 when
   compiling a function that needs a frame pointer.

   Normally, `FUNCTION_PROLOGUE' and `FUNCTION_EPILOGUE' must treat
   leaf functions specially.  The C variable `leaf_function' is
   nonzero for such a function.  *Note Leaf Functions::.

   On some machines, some functions pop their arguments on exit while
   others leave that for the caller to do.  For example, the 68020
   when given `-mrtd' pops arguments in functions that take a fixed
   number of arguments.

   Your definition of the macro `RETURN_POPS_ARGS' decides which
   functions pop their own arguments.  `FUNCTION_EPILOGUE' needs to
   know what was decided.  The variable that is called
   `current_function_pops_args' is the number of bytes of its
   arguments that a function should pop.  *Note Scalar Return::.  */

#define STRICT_ARGUMENT_NAMING 1
/* Define this macro if the location where a function argument is
   passed depends on whether or not it is a named argument.

   This macro controls how the NAMED argument to `FUNCTION_ARG' is
   set for varargs and stdarg functions.  With this macro defined,
   the NAMED argument is always true for named arguments, and false
   for unnamed arguments.  If this is not defined, but
   `SETUP_INCOMING_VARARGS' is defined, then all arguments are
   treated as named.  Otherwise, all named arguments except the last
   are treated as named.  */


#define HAVE_POST_INCREMENT 1
/* Define this macro if the machine supports post-increment
   addressing.  */

#define HAVE_PRE_DECREMENT 1
/* #define HAVE_PRE_INCREMENT
   #define HAVE_POST_DECREMENT  */
/* Similar for other kinds of addressing.  */

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
#  define GO_IF_LEGITIMATE_ADDRESS(mode, operand, ADDR)	\
{							\
  if (legitimate_address_p (mode, operand, 1))		\
    goto ADDR;						\
}
#  else
#  define GO_IF_LEGITIMATE_ADDRESS(mode, operand, ADDR)	\
{							\
  if (legitimate_address_p (mode, operand, 0))		\
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
   `PRINT_OPERAND_ADDRESS'. */

/* `REG_OK_FOR_BASE_P (X)'
   A C expression that is nonzero if X (assumed to be a `reg' RTX) is
   valid for use as a base register.  For hard registers, it should
   always accept those which the hardware permits and reject the
   others.  Whether the macro accepts or rejects pseudo registers
   must be controlled by `REG_OK_STRICT' as described above.  This
   usually requires two variant definitions, of which `REG_OK_STRICT'
   controls the one actually used.  */

#define REG_OK_FOR_BASE_NOSTRICT_P(X) \
  (REGNO (X) >= FIRST_PSEUDO_REGISTER || REG_OK_FOR_BASE_STRICT_P(X))

#define REG_OK_FOR_BASE_STRICT_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))

#ifdef REG_OK_STRICT
#  define REG_OK_FOR_BASE_P(X) REG_OK_FOR_BASE_STRICT_P (X)
#else
#  define REG_OK_FOR_BASE_P(X) REG_OK_FOR_BASE_NOSTRICT_P (X)
#endif

/* A C expression that is just like `REG_OK_FOR_BASE_P', except that
   that expression may examine the mode of the memory reference in
   MODE.  You should define this macro if the mode of the memory
   reference affects whether a register may be used as a base
   register.  If you define this macro, the compiler will use it
   instead of `REG_OK_FOR_BASE_P'.  */
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

#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN)				\
{									\
  (X) = legitimize_address (X, OLDX, MODE);				\
  if (memory_address_p (MODE, X))					\
    goto WIN;								\
}
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

#define XEXP_(X,Y) (X)
#define LEGITIMIZE_RELOAD_ADDRESS(X, MODE, OPNUM, TYPE, IND_LEVELS, WIN)    \
do {									    \
  if (1&&(GET_CODE (X) == POST_INC || GET_CODE (X) == PRE_DEC))	    \
    {									    \
      push_reload (XEXP (X,0), XEXP (X,0), &XEXP (X,0), &XEXP (X,0),	    \
	           POINTER_REGS, GET_MODE (X),GET_MODE (X) , 0, 0,	    \
		   OPNUM, RELOAD_OTHER);				    \
      goto WIN;								    \
    }									    \
  if (GET_CODE (X) == PLUS						    \
      && REG_P (XEXP (X, 0))						    \
      && GET_CODE (XEXP (X, 1)) == CONST_INT				    \
      && INTVAL (XEXP (X, 1)) >= 1)					    \
    {									    \
      int fit = INTVAL (XEXP (X, 1)) <= (64 - GET_MODE_SIZE (MODE));	    \
      if (fit)								    \
	{								    \
          if (reg_equiv_address[REGNO (XEXP (X, 0))] != 0)		    \
	    {								    \
	      int regno = REGNO (XEXP (X, 0));				    \
	      rtx mem = make_memloc (X, regno);				    \
	      push_reload (XEXP (mem,0), NULL_PTR, &XEXP (mem,0), NULL_PTR, \
		           POINTER_REGS, Pmode, VOIDmode, 0, 0,		    \
		           1, ADDR_TYPE (TYPE));			    \
	      push_reload (mem, NULL_RTX, &XEXP (X, 0), NULL_PTR,	    \
		           BASE_POINTER_REGS, GET_MODE (X), VOIDmode, 0, 0, \
		           OPNUM, TYPE);				    \
	      goto WIN;							    \
	    }								    \
	  push_reload (XEXP (X, 0), NULL_RTX, &XEXP (X, 0), NULL_PTR,	    \
		       BASE_POINTER_REGS, GET_MODE (X), VOIDmode, 0, 0,	    \
		       OPNUM, TYPE);					    \
          goto WIN;							    \
	}								    \
      else if (! (frame_pointer_needed && XEXP (X,0) == frame_pointer_rtx)) \
	{								    \
	  push_reload (X, NULL_RTX, &X, NULL_PTR,			    \
		       POINTER_REGS, GET_MODE (X), VOIDmode, 0, 0,	    \
		       OPNUM, TYPE);					    \
          goto WIN;							    \
	}								    \
    }									    \
} while(0)
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
	
#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)			\
      if (GET_CODE (ADDR) == POST_INC || GET_CODE (ADDR) == PRE_DEC)	\
        goto LABEL
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

#define CONST_COSTS(x,CODE,OUTER_CODE)		\
    case CONST_INT:				\
      if (OUTER_CODE == PLUS			\
	  || OUTER_CODE == IOR			\
	  || OUTER_CODE == AND			\
	  || OUTER_CODE == MINUS		\
	  || OUTER_CODE == SET			\
	  || INTVAL (x) == 0)			\
        return 2;				\
      if (OUTER_CODE == COMPARE			\
	  && INTVAL (x) >= 0			\
	  && INTVAL (x) <= 255)			\
        return 2;				\
    case CONST:					\
    case LABEL_REF:				\
    case SYMBOL_REF:				\
      return 4;					\
    case CONST_DOUBLE:				\
      return 4;

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

#define DEFAULT_RTX_COSTS(x, code, outer_code)		\
{							\
  int cst = default_rtx_costs (x, code, outer_code);	\
  if (cst>0)						\
    return cst; 			                \
  else if (cst<0)					\
    total += -cst; 			                \
  break;						\
}

/* Like `CONST_COSTS' but applies to nonconstant RTL expressions.
   This can be used, for example, to indicate how costly a multiply
   instruction is.  In writing this macro, you can use the construct
   `COSTS_N_INSNS (N)' to specify a cost equal to N fast
   instructions.  OUTER_CODE is the code of the expression in which X
   is contained.

   This macro is optional; do not define it if the default cost
   assumptions are adequate for the target machine.  */

#define ADDRESS_COST(ADDRESS) avr_address_cost (ADDRESS)

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

#define REGISTER_MOVE_COST(FROM, TO) ((FROM) == STACK_REG ? 6 : \
				      (TO) == STACK_REG ? 12    \
				      : 2)
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

#define MEMORY_MOVE_COST(MODE,CLASS,IN) ((MODE)==QImode ? 2 :	\
					 (MODE)==HImode ? 4 :	\
					 (MODE)==SImode ? 8 :	\
					 (MODE)==SFmode ? 8 : 16)
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

   When this macro is non-zero, the compiler will act as if
   `STRICT_ALIGNMENT' were non-zero when generating code for block
   moves.  This can cause significantly more instructions to be
   produced.  Therefore, do not set this macro non-zero if unaligned
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

#define EXTRA_SECTIONS in_progmem
/* A list of names for sections other than the standard two, which are
   `in_text' and `in_data'.  You need not define this macro on a
   system with no other sections (that GCC needs to use).  */

#define EXTRA_SECTION_FUNCTIONS						      \
									      \
void									      \
progmem_section (void)							      \
{									      \
  if (in_section != in_progmem)						      \
    {									      \
      fprintf (asm_out_file, ".section .progmem.gcc_sw_table\n");	      \
      in_section = in_progmem;						      \
    }									      \
}
/* `EXTRA_SECTION_FUNCTIONS'
   One or more functions to be defined in `varasm.c'.  These
   functions should do jobs analogous to those of `text_section' and
   `data_section', for your additional sections.  Do not define this
   macro if you do not define `EXTRA_SECTIONS'.  */

#define READONLY_DATA_SECTION data_section
/* On most machines, read-only variables, constants, and jump tables
   are placed in the text section.  If this is not the case on your
   machine, this macro should be defined to be the name of a function
   (either `data_section' or a function defined in `EXTRA_SECTIONS')
   that switches to the section to be used for read-only items.

   If these items should be placed in the text section, this macro
   should not be defined.  */

/* `SELECT_SECTION (EXP, RELOC)'
   A C statement or statements to switch to the appropriate section
   for output of EXP.  You can assume that EXP is either a `VAR_DECL'
   node or a constant of some sort.  RELOC indicates whether the
   initial value of EXP requires link-time relocations.  Select the
   section by calling `text_section' or one of the alternatives for
   other sections.

   Do not define this macro if you put all read-only variables and
   constants in the read-only data section (usually the text section).  */

/* `SELECT_RTX_SECTION (MODE, RTX)'
   A C statement or statements to switch to the appropriate section
   for output of RTX in mode MODE.  You can assume that RTX is some
   kind of constant in RTL.  The argument MODE is redundant except in
   the case of a `const_int' rtx.  Select the section by calling
   `text_section' or one of the alternatives for other sections.

   Do not define this macro if you put all constants in the read-only
   data section.  */

#define JUMP_TABLES_IN_TEXT_SECTION 1
/* Define this macro if jump tables (for `tablejump' insns) should be
   output in the text section, along with the assembler instructions.
   Otherwise, the readonly data section is used.

   This macro is irrelevant if there is no separate readonly data
   section.  */

#define ENCODE_SECTION_INFO(DECL)  encode_section_info(DECL)
/* Define this macro if references to a symbol must be treated
   differently depending on something about the variable or function
   named by the symbol (such as what section it is in).

   The macro definition, if any, is executed immediately after the
   rtl for DECL has been created and stored in `DECL_RTL (DECL)'.
   The value of the rtl will be a `mem' whose address is a
   `symbol_ref'.

   The usual thing for this macro to do is to record a flag in the
   `symbol_ref' (such as `SYMBOL_REF_FLAG') or to store a modified
   name string in the `symbol_ref' (if one bit is not enough
   information).  */

#define STRIP_NAME_ENCODING(VAR,SYMBOL_NAME) \
  (VAR) = (SYMBOL_NAME) + ((SYMBOL_NAME)[0] == '*' || (SYMBOL_NAME)[0] == '@');
/* `STRIP_NAME_ENCODING (VAR, SYM_NAME)'
   Decode SYM_NAME and store the real name part in VAR, sans the
   characters that encode section info.  Define this macro if
   `ENCODE_SECTION_INFO' alters the symbol's name string.  */
/* `UNIQUE_SECTION_P (DECL)'
   A C expression which evaluates to true if DECL should be placed
   into a unique section for some target-specific reason.  If you do
   not define this macro, the default is `0'.  Note that the flag
   `-ffunction-sections' will also cause functions to be placed into
   unique sections.  */

#define UNIQUE_SECTION(DECL, RELOC) unique_section (DECL, RELOC)
/* `UNIQUE_SECTION (DECL, RELOC)'
   A C statement to build up a unique section name, expressed as a
   STRING_CST node, and assign it to `DECL_SECTION_NAME (DECL)'.
   RELOC indicates whether the initial value of EXP requires
   link-time relocations.  If you do not define this macro, GNU CC
   will use the symbol name prefixed by `.' as the section name.  */


#define ASM_FILE_START(STREAM) asm_file_start (STREAM)
/* A C expression which outputs to the stdio stream STREAM some
   appropriate text to go at the start of an assembler file.

   Normally this macro is defined to output a line containing
   `#NO_APP', which is a comment that has no effect on most
   assemblers but tells the GNU assembler that it can save time by not
   checking for certain assembler constructs.

   On systems that use SDB, it is necessary to output certain
   commands; see `attasm.h'.  */

#define ASM_FILE_END(STREAM) asm_file_end (STREAM)
/* A C expression which outputs to the stdio stream STREAM some
   appropriate text to go at the end of an assembler file.

   If this macro is not defined, the default is to output nothing
   special at the end of the file.  Most systems don't require any
   definition.

   On systems that use SDB, it is necessary to output certain
   commands; see `attasm.h'.  */

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

#define ASM_OUTPUT_SOURCE_LINE(STREAM, LINE) fprintf (STREAM,"/* line: %d */\n",LINE)
/* A C statement to output DBX or SDB debugging information before
   code for line number LINE of the current source file to the stdio
   stream STREAM.

   This macro need not be defined if the standard form of debugging
   information for the debugger in use is appropriate.  */

#define ASM_OUTPUT_SECTION_NAME(FILE, DECL, NAME, RELOC) \
  asm_output_section_name(FILE, DECL, NAME, RELOC)

/* `ASM_OUTPUT_SECTION_NAME (STREAM, DECL, NAME, RELOC)'
   A C statement to output something to the assembler file to switch
   to section NAME for object DECL which is either a `FUNCTION_DECL',
   a `VAR_DECL' or `NULL_TREE'.  RELOC indicates whether the initial
   value of EXP requires link-time relocations.  Some target formats
   do not support arbitrary sections.  Do not define this macro in
   such cases.

   At present this macro is only used to support section attributes.
   When this macro is undefined, section attributes are disabled.  */

#define OBJC_PROLOGUE {}
/* A C statement to output any assembler statements which are
   required to precede any Objective C object definitions or message
   sending.  The statement is executed only when compiling an
   Objective C program.  */



#define ASM_OUTPUT_DOUBLE(STREAM, VALUE) fprintf (STREAM, "no double float %.20e\n", VALUE)
#define ASM_OUTPUT_FLOAT(STREAM, VALUE) asm_output_float (STREAM, VALUE)
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
 ( fprintf (FILE, "\t.long "),				\
   output_addr_const (FILE, (VALUE)),			\
   fputs ("\n", FILE))

 /* Likewise for `short' and `char' constants.   */

#define ASM_OUTPUT_SHORT(FILE,VALUE) asm_output_short(FILE,VALUE)
#define ASM_OUTPUT_CHAR(FILE,VALUE) asm_output_char(FILE,VALUE)

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


#define ASM_OUTPUT_BYTE(FILE,VALUE) asm_output_byte (FILE,VALUE)
/* A C statement to output to the stdio stream STREAM an assembler
   instruction to assemble a single byte containing the number VALUE.  */

#define ASM_BYTE_OP ".byte "
/* A C string constant giving the pseudo-op to use for a sequence of
   single-byte constants.  If this macro is not defined, the default
   is `"byte"'.  */

#define ASM_OUTPUT_ASCII(FILE, P, SIZE)	 gas_output_ascii (FILE,P,SIZE)
/* `ASM_OUTPUT_ASCII (STREAM, PTR, LEN)'
   output_ascii (FILE, P, SIZE)
   A C statement to output to the stdio stream STREAM an assembler
   instruction to assemble a string constant containing the LEN bytes
   at PTR.  PTR will be a C expression of type `char *' and LEN a C
   expression of type `int'.

   If the assembler has a `.ascii' pseudo-op as found in the Berkeley
   Unix assembler, do not define the macro `ASM_OUTPUT_ASCII'.  */

#define IS_ASM_LOGICAL_LINE_SEPARATOR(C) ((C) == '\n'			 \
					  || ((C) == '$'))
/* Define this macro as a C expression which is nonzero if C is used
   as a logical line separator by the assembler.

   If you do not define this macro, the default is that only the
   character `;' is treated as a logical line separator.  */

#define ASM_OPEN_PAREN "("
#define ASM_CLOSE_PAREN ")"
/* These macros are defined as C string constant, describing the
   syntax in the assembler for grouping arithmetic expressions.  The
   following definitions are correct for most assemblers:

   #define ASM_OPEN_PAREN "("
   #define ASM_CLOSE_PAREN ")"

   These macros are provided by `real.h' for writing the definitions of
   `ASM_OUTPUT_DOUBLE' and the like:  */

#define ASM_OUTPUT_COMMON(STREAM, NAME, SIZE, ROUNDED)			   \
do {									   \
     fputs ("\t.comm ", (STREAM));					   \
     assemble_name ((STREAM), (NAME));					   \
     fprintf ((STREAM), ",%d\n", (SIZE));				   \
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

#define ASM_OUTPUT_LOCAL(STREAM, NAME, SIZE, ROUNDED)			\
do {									\
     fputs ("\t.lcomm ", (STREAM));					\
     assemble_name ((STREAM), (NAME));					\
     fprintf ((STREAM), ",%d\n", (SIZE));				\
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

#define ASM_OUTPUT_LABEL(STREAM, NAME)		\
{						\
  assemble_name (STREAM, NAME);			\
  fprintf (STREAM, ":\n");			\
}
/* A C statement (sans semicolon) to output to the stdio stream
   STREAM the assembler definition of a label named NAME.  Use the
   expression `assemble_name (STREAM, NAME)' to output the name
   itself; before and after that, output the additional assembler
   syntax for defining the name, and a newline.  */

#undef TYPE_ASM_OP
#undef SIZE_ASM_OP
#undef WEAK_ASM_OP
#define TYPE_ASM_OP	".type"
#define SIZE_ASM_OP	".size"
#define WEAK_ASM_OP	".weak"
/* Define the strings used for the special svr4 .type and .size directives.
   These strings generally do not vary from one system running svr4 to
   another, but if a given system (e.g. m88k running svr) needs to use
   different pseudo-op names for these, they may be overridden in the
   file which includes this one.  */


#undef TYPE_OPERAND_FMT
#define TYPE_OPERAND_FMT	"@%s"
/* The following macro defines the format used to output the second
   operand of the .type assembler directive.  Different svr4 assemblers
   expect various different forms for this operand.  The one given here
   is just a default.  You may need to override it in your machine-
   specific tm.h file (depending upon the particulars of your assembler).  */


#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)	\
do {						   	\
     fprintf (FILE, "\t%s\t ", TYPE_ASM_OP);	   	\
     assemble_name (FILE, NAME);		   	\
     putc (',', FILE);				   	\
     fprintf (FILE, TYPE_OPERAND_FMT, "function");	\
     putc ('\n', FILE);				   	\
     ASM_OUTPUT_LABEL (FILE, NAME);		   	\
} while (0)
/* A C statement (sans semicolon) to output to the stdio stream
   STREAM any text necessary for declaring the name NAME of a
   function which is being defined.  This macro is responsible for
   outputting the label definition (perhaps using
   `ASM_OUTPUT_LABEL').  The argument DECL is the `FUNCTION_DECL'
   tree node representing the function.

   If this macro is not defined, then the function name is defined in
   the usual manner as a label (by means of `ASM_OUTPUT_LABEL').  */

#define ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)			\
  do {									\
    if (!flag_inhibit_size_directive)					\
      {									\
        char label[256];						\
	static int labelno;						\
	labelno++;							\
	ASM_GENERATE_INTERNAL_LABEL (label, "Lfe", labelno);		\
	ASM_OUTPUT_INTERNAL_LABEL (FILE, "Lfe", labelno);		\
	fprintf (FILE, "\t%s\t ", SIZE_ASM_OP);				\
	assemble_name (FILE, (FNAME));					\
        fprintf (FILE, ",");						\
	assemble_name (FILE, label);					\
        fprintf (FILE, "-");						\
	assemble_name (FILE, (FNAME));					\
	putc ('\n', FILE);						\
      }									\
  } while (0)
/* A C statement (sans semicolon) to output to the stdio stream
   STREAM any text necessary for declaring the size of a function
   which is being defined.  The argument NAME is the name of the
   function.  The argument DECL is the `FUNCTION_DECL' tree node
   representing the function.

   If this macro is not defined, then the function size is not
   defined.  */

#define ASM_DECLARE_OBJECT_NAME(FILE, NAME, DECL)			  \
do {									  \
      fprintf (FILE, "\t%s\t ", TYPE_ASM_OP);				  \
      assemble_name (FILE, NAME);					  \
      putc (',', FILE);							  \
      fprintf (FILE, TYPE_OPERAND_FMT, "object");			  \
      putc ('\n', FILE);						  \
      size_directive_output = 0;					  \
      if (!flag_inhibit_size_directive && DECL_SIZE (DECL))		  \
	{								  \
	  size_directive_output = 1;					  \
	  fprintf (FILE, "\t%s\t ", SIZE_ASM_OP);			  \
	  assemble_name (FILE, NAME);					  \
	  fprintf (FILE, ",%d\n",  int_size_in_bytes (TREE_TYPE (DECL))); \
    }									  \
  ASM_OUTPUT_LABEL(FILE, NAME);						  \
} while (0)
/* A C statement (sans semicolon) to output to the stdio stream
   STREAM any text necessary for declaring the name NAME of an
   initialized variable which is being defined.  This macro must
   output the label definition (perhaps using `ASM_OUTPUT_LABEL').
   The argument DECL is the `VAR_DECL' tree node representing the
   variable.

   If this macro is not defined, then the variable name is defined in
   the usual manner as a label (by means of `ASM_OUTPUT_LABEL').  */

#define ASM_FINISH_DECLARE_OBJECT(FILE, DECL, TOP_LEVEL, AT_END)	 \
do {									 \
     const char *name = XSTR (XEXP (DECL_RTL (DECL), 0), 0);		 \
     if (!flag_inhibit_size_directive && DECL_SIZE (DECL)		 \
         && ! AT_END && TOP_LEVEL					 \
	 && DECL_INITIAL (DECL) == error_mark_node			 \
	 && !size_directive_output)					 \
       {								 \
	 size_directive_output = 1;					 \
	 fprintf (FILE, "\t%s\t ", SIZE_ASM_OP);			 \
	 assemble_name (FILE, name);					 \
	 fprintf (FILE, ",%d\n",  int_size_in_bytes (TREE_TYPE (DECL))); \
       }								 \
   } while (0)
/* A C statement (sans semicolon) to finish up declaring a variable
   name once the compiler has processed its initializer fully and
   thus has had a chance to determine the size of an array when
   controlled by an initializer.  This is used on systems where it's
   necessary to declare something about the size of the object.

   If you don't define this macro, that is equivalent to defining it
   to do nothing.  */


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

#define STRING_LIMIT	((unsigned) 64)
#define STRING_ASM_OP	".string"
/* Some svr4 assemblers have a limit on the number of characters which
   can appear in the operand of a .string directive.  If your assembler
   has such a limitation, you should define STRING_LIMIT to reflect that
   limit.  Note that at least some svr4 assemblers have a limit on the
   actual number of bytes in the double-quoted string, and that they
   count each character in an escape sequence as one byte.  Thus, an
   escape sequence like \377 would count as four bytes.

   If your target assembler doesn't support the .string directive, you
   should define this to zero.  */

#define ASM_GLOBALIZE_LABEL(STREAM, NAME)	\
do {						\
  fprintf (STREAM, ".global\t");		\
  assemble_name (STREAM, NAME);			\
  fprintf (STREAM, "\n");			\
}						\
while (0)
     
/* A C statement (sans semicolon) to output to the stdio stream
   STREAM some commands that will make the label NAME global; that
   is, available for reference from other files.  Use the expression
   `assemble_name (STREAM, NAME)' to output the name itself; before
   and after that, output the additional assembler syntax for making
   that name global, and a newline.  */

/* `ASM_WEAKEN_LABEL'
   A C statement (sans semicolon) to output to the stdio stream
   STREAM some commands that will make the label NAME weak; that is,
   available for reference from other files but only used if no other
   definition is available.  Use the expression `assemble_name
   (STREAM, NAME)' to output the name itself; before and after that,
   output the additional assembler syntax for making that name weak,
   and a newline.

   If you don't define this macro, GNU CC will not support weak
   symbols and you should not define the `SUPPORTS_WEAK' macro.

   `SUPPORTS_WEAK'
   A C expression which evaluates to true if the target supports weak
   symbols.

   If you don't define this macro, `defaults.h' provides a default
   definition.  If `ASM_WEAKEN_LABEL' is defined, the default
   definition is `1'; otherwise, it is `0'.  Define this macro if you
   want to control weak symbol support with a compiler flag such as
   `-melf'.

   `MAKE_DECL_ONE_ONLY'
   A C statement (sans semicolon) to mark DECL to be emitted as a
   public symbol such that extra copies in multiple translation units
   will be discarded by the linker.  Define this macro if your object
   file format provides support for this concept, such as the `COMDAT'
   section flags in the Microsoft Windows PE/COFF format, and this
   support requires changes to DECL, such as putting it in a separate
   section.

   `SUPPORTS_WEAK'
   A C expression which evaluates to true if the target supports
   one-only semantics.

   If you don't define this macro, `varasm.c' provides a default
   definition.  If `MAKE_DECL_ONE_ONLY' is defined, the default
   definition is `1'; otherwise, it is `0'.  Define this macro if you
   want to control weak symbol support with a compiler flag, or if
   setting the `DECL_ONE_ONLY' flag is enough to mark a declaration to
   be emitted as one-only.  */

#define ASM_OUTPUT_INTERNAL_LABEL(STREAM, PREFIX, NUM)	\
fprintf(STREAM, ".%s%d:\n", PREFIX, NUM)
/* A C statement to output to the stdio stream STREAM a label whose
   name is made from the string PREFIX and the number NUM.

   It is absolutely essential that these labels be distinct from the
   labels used for user-level functions and variables.  Otherwise,
   certain programs will have name conflicts with internal labels.

   It is desirable to exclude internal labels from the symbol table
   of the object file.  Most assemblers have a naming convention for
   labels that should be excluded; on many systems, the letter `L' at
   the beginning of a label has this effect.  You should find out what
   convention your system uses, and follow it.

   The usual definition of this macro is as follows:

   fprintf (STREAM, "L%s%d:\n", PREFIX, NUM)  */

#define ASM_GENERATE_INTERNAL_LABEL(STRING, PREFIX, NUM)	\
sprintf (STRING, "*.%s%d", PREFIX, NUM)
/* A C statement to store into the string STRING a label whose name
   is made from the string PREFIX and the number NUM.

   This string, when output subsequently by `assemble_name', should
   produce the output that `ASM_OUTPUT_INTERNAL_LABEL' would produce
   with the same PREFIX and NUM.

   If the string begins with `*', then `assemble_name' will output
   the rest of the string unchanged.  It is often convenient for
   `ASM_GENERATE_INTERNAL_LABEL' to use `*' in this way.  If the
   string doesn't start with `*', then `ASM_OUTPUT_LABELREF' gets to
   output the string, and may change it.  (Of course,
   `ASM_OUTPUT_LABELREF' is also part of your machine description, so
   you should know what it does on your machine.)  */

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

/* `ASM_OUTPUT_WEAK_ALIAS (STREAM, NAME, VALUE)'
   A C statement to output to the stdio stream STREAM assembler code
   which defines (equates) the weak symbol NAME to have the value
   VALUE.

   Define this macro if the target only supports weak aliases; define
   ASM_OUTPUT_DEF instead if possible.  */

#define HAS_INIT_SECTION 1
/* If defined, `main' will not call `__main' as described above.
   This macro should be defined for systems that control the contents
   of the init section on a symbol-by-symbol basis, such as OSF/1,
   and should not be defined explicitly for systems that support
   `INIT_SECTION_ASM_OP'.  */

#define REGISTER_NAMES {				\
  "r0","r1","r2","r3","r4","r5","r6","r7",		\
    "r8","r9","r10","r11","r12","r13","r14","r15",	\
    "r16","r17","r18","r19","r20","r21","r22","r23",	\
    "r24","r25","r26","r27","r28","r29","r30","r31",	\
    "__SPL__","__SPH__","argL","argH"}
/* A C initializer containing the assembler's names for the machine
   registers, each one as a C string constant.  This is what
   translates register numbers in the compiler into assembler
   language.  */

#define FINAL_PRESCAN_INSN(insn, operand, nop) final_prescan_insn (insn, operand,nop)
/* If defined, a C statement to be executed just prior to the output
   of assembler code for INSN, to modify the extracted operands so
   they will be output differently.

   Here the argument OPVEC is the vector containing the operands
   extracted from INSN, and NOPERANDS is the number of elements of
   the vector which contain meaningful data for this insn.  The
   contents of this vector are what will be used to convert the insn
   template into assembler code, so you can change the assembler
   output by changing the contents of the vector.

   This macro is useful when various assembler syntaxes share a single
   file of instruction patterns; by defining this macro differently,
   you can cause a large class of instructions to be output
   differently (such as with rearranged operands).  Naturally,
   variations in assembler syntax affecting individual insn patterns
   ought to be handled by writing conditional output routines in
   those patterns.

   If this macro is not defined, it is equivalent to a null statement.  */

#define PRINT_OPERAND(STREAM, X, CODE) print_operand (STREAM, X, CODE)
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

#define PRINT_OPERAND_PUNCT_VALID_P(CODE) ((CODE) == '~')
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

#define USER_LABEL_PREFIX ""
/* `LOCAL_LABEL_PREFIX'
   `REGISTER_PREFIX'
   `IMMEDIATE_PREFIX'
   If defined, C string expressions to be used for the `%R', `%L',
   `%U', and `%I' options of `asm_fprintf' (see `final.c').  These
   are useful when a single `md' file must support multiple assembler
   formats.  In that case, the various `tm.h' files can define these
   macros differently.  */

#define ASM_OUTPUT_REG_PUSH(STREAM, REGNO)	\
{						\
  if (REGNO > 31)				\
    fatal("regno error in push");		\
  fprintf (STREAM, "\tpush\tr%d", REGNO);	\
}
/* A C expression to output to STREAM some assembler code which will
   push hard register number REGNO onto the stack.  The code need not
   be optimal, since this macro is used only when profiling.  */

#define ASM_OUTPUT_REG_POP(STREAM, REGNO)	\
{						\
  if (REGNO > 31)				\
    fatal("regno error in pop");		\
  fprintf (STREAM, "\tpop\tr%d", REGNO);	\
}
/* A C expression to output to STREAM some assembler code which will
   pop hard register number REGNO off of the stack.  The code need
   not be optimal, since this macro is used only when profiling.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE)				      \
  fprintf (STREAM, "\t.word pm(.L%d)\n", VALUE);
/* This macro should be provided on machines where the addresses in a
   dispatch table are absolute.

   The definition should be a C statement to output to the stdio
   stream STREAM an assembler pseudo-instruction to generate a
   reference to a label.  VALUE is the number of an internal label
   whose definition is output using `ASM_OUTPUT_INTERNAL_LABEL'.  For
   example,

   fprintf (STREAM, "\t.word L%d\n", VALUE)  */

#define ASM_OUTPUT_CASE_LABEL(STREAM, PREFIX, NUM, TABLE) \
  progmem_section (), ASM_OUTPUT_INTERNAL_LABEL (STREAM, PREFIX, NUM)

/* `ASM_OUTPUT_CASE_LABEL (STREAM, PREFIX, NUM, TABLE)'
   Define this if the label before a jump-table needs to be output
   specially.  The first three arguments are the same as for
   `ASM_OUTPUT_INTERNAL_LABEL'; the fourth argument is the jump-table
   which follows (a `jump_insn' containing an `addr_vec' or
   `addr_diff_vec').

   This feature is used on system V to output a `swbeg' statement for
   the table.

   If this macro is not defined, these labels are output with
   `ASM_OUTPUT_INTERNAL_LABEL'.  */

/* `ASM_OUTPUT_CASE_END (STREAM, NUM, TABLE)'
   Define this if something special must be output at the end of a
   jump-table.  The definition should be a C statement to be executed
   after the assembler code for the table is written.  It should write
   the appropriate code to stdio stream STREAM.  The argument TABLE
   is the jump-table insn, and NUM is the label-number of the
   preceding label.

   If this macro is not defined, nothing special is output at the end
   of the jump-table.  */

#define ASM_OUTPUT_SKIP(STREAM, n)		\
fprintf (STREAM, "\t.skip %d,0\n", n)
/* A C statement to output to the stdio stream STREAM an assembler
   instruction to advance the location counter by NBYTES bytes.
   Those bytes should be zero when loaded.  NBYTES will be a C
   expression of type `int'.  */

#define ASM_OUTPUT_ALIGN(STREAM, POWER)
/* A C statement to output to the stdio stream STREAM an assembler
   command to advance the location counter to a multiple of 2 to the
   POWER bytes.  POWER will be a C expression of type `int'.  */

#define CASE_VECTOR_MODE HImode
/* An alias for a machine mode name.  This is the machine mode that
   elements of a jump-table should have.  */

#define CASE_VALUES_THRESHOLD 17
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

#define EASY_DIV_EXPR TRUNC_DIV_EXPR
/* An alias for a tree code that is the easiest kind of division to
   compile code for in the general case.  It may be `TRUNC_DIV_EXPR',
   `FLOOR_DIV_EXPR', `CEIL_DIV_EXPR' or `ROUND_DIV_EXPR'.  These four
   division operators differ in how they round the result to an
   integer.  `EASY_DIV_EXPR' is used when it is permissible to use
   any of those kinds of division and the choice should be made on
   the basis of efficiency.  */

#define MOVE_MAX 4
/* The maximum number of bytes that a single instruction can move
   quickly between memory and registers or between two memory
   locations.  */

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
     /*                            1        3 */
#define INTEGRATE_THRESHOLD(DECL) (1 + (3 * list_length (DECL_ARGUMENTS (DECL)) / 2))

/* A C expression for the maximum number of instructions above which
   the function DECL should not be inlined.  DECL is a
   `FUNCTION_DECL' node.

   The default definition of this macro is 64 plus 8 times the number
   of arguments that the function accepts.  Some people think a larger
   threshold should be used on RISC machines.  */

#define VALID_MACHINE_DECL_ATTRIBUTE(DECL, ATTRIBUTES, IDENTIFIER, ARGS) \
valid_machine_decl_attribute (DECL, ATTRIBUTES, IDENTIFIER, ARGS)
/* `VALID_MACHINE_DECL_ATTRIBUTE (DECL, ATTRIBUTES, IDENTIFIER, ARGS)'
   If defined, a C expression whose value is nonzero if IDENTIFIER
   with arguments ARGS is a valid machine specific attribute for DECL.
   The attributes in ATTRIBUTES have previously been assigned to DECL.  */

#define VALID_MACHINE_TYPE_ATTRIBUTE(TYPE, ATTRIBUTES, IDENTIFIER, ARGS) \
     valid_machine_type_attribute(TYPE, ATTRIBUTES, IDENTIFIER, ARGS)
/* `VALID_MACHINE_TYPE_ATTRIBUTE (TYPE, ATTRIBUTES, IDENTIFIER, ARGS)'
   If defined, a C expression whose value is nonzero if IDENTIFIER
   with arguments ARGS is a valid machine specific attribute for TYPE.
   The attributes in ATTRIBUTES have previously been assigned to TYPE.  */

#define DOLLARS_IN_IDENTIFIERS 0
/* Define this macro to control use of the character `$' in identifier
   names.  0 means `$' is not allowed by default; 1 means it is
   allowed.  1 is the default; there is no need to define this macro
   in that case.  This macro controls the compiler proper; it does
   not affect the preprocessor.  */

#define NO_DOLLAR_IN_LABEL 1
/* Define this macro if the assembler does not accept the character
   `$' in label names.  By default constructors and destructors in
   G++ have `$' in the identifiers.  If this macro is defined, `.' is
   used instead.  */

#define MACHINE_DEPENDENT_REORG(INSN) machine_dependent_reorg (INSN)
/* In rare cases, correct code generation requires extra machine
   dependent processing between the second jump optimization pass and
   delayed branch scheduling.  On those machines, define this macro
   as a C statement to act on the code starting at INSN.  */

#define GIV_SORT_CRITERION(X, Y)	\
  if (GET_CODE ((X)->add_val) == CONST_INT		\
      && GET_CODE ((Y)->add_val) == CONST_INT)		\
    return INTVAL ((X)->add_val) - INTVAL ((Y)->add_val);

/* `GIV_SORT_CRITERION(GIV1, GIV2)'
   In some cases, the strength reduction optimization pass can
   produce better code if this is defined.  This macro controls the
   order that induction variables are combined.  This macro is
   particularly useful if the target has limited addressing modes.
   For instance, the SH target has only positive offsets in
   addresses.  Thus sorting to put the smallest address first allows
   the most combinations to be found.  */

/* Define results of standard character escape sequences.  */
#define TARGET_BELL 007
#define TARGET_BS 010
#define TARGET_TAB 011
#define TARGET_NEWLINE 012
#define TARGET_VT 013
#define TARGET_FF 014
#define TARGET_CR 015



#define TRAMPOLINE_TEMPLATE(FILE) fatal ("Trampolines not supported\n")

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE 4

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)			      \
{									      \
  emit_move_insn (gen_rtx (MEM, HImode, plus_constant ((TRAMP), 2)), CXT);    \
  emit_move_insn (gen_rtx (MEM, HImode, plus_constant ((TRAMP), 6)), FNADDR); \
}
/* Store in cc_status the expressions
   that the condition codes will describe
   after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

#define NOTICE_UPDATE_CC(EXP, INSN) notice_update_cc(EXP, INSN)

/* The add insns don't set overflow in a usable way.  */
#define CC_OVERFLOW_UNUSABLE 01000
/* The mov,and,or,xor insns don't set carry.  That's ok though as the
   Z bit is all we need when doing unsigned comparisons on the result of
   these insns (since they're always with 0).  However, conditions.h has
   CC_NO_OVERFLOW defined for this purpose.  Rename it to something more
   understandable.  */
#define CC_NO_CARRY CC_NO_OVERFLOW


/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)  \
  fprintf (FILE, "/* profiler %d */", (LABELNO))

/* `FIRST_INSN_ADDRESS'
   When the `length' insn attribute is used, this macro specifies the
   value to be assigned to the address of the first insn in a
   function.  If not specified, 0 is used.  */

#define ADJUST_INSN_LENGTH(INSN, LENGTH) (LENGTH =\
					  adjust_insn_length (INSN, LENGTH))
/* If defined, modifies the length assigned to instruction INSN as a
   function of the context in which it is used.  LENGTH is an lvalue
   that contains the initially computed length of the insn and should
   be updated with the correct length of the insn.  If updating is
   required, INSN must not be a varying-length insn.

   This macro will normally not be required.  A case in which it is
   required is the ROMP.  On this machine, the size of an `addr_vec'
   insn must be increased by two to compensate for the fact that
   alignment may be required.  */

#define TARGET_MEM_FUNCTIONS
/* Define this macro if GNU CC should generate calls to the System V
   (and ANSI C) library functions `memcpy' and `memset' rather than
   the BSD functions `bcopy' and `bzero'.  */

#define CPP_SPEC "\
%{!mmcu=*:-DAVR_AT90S8515} \
%{mmcu=at90s2313:-DAVR_AT90S2313} \
%{mmcu=at90s2323:-DAVR_AT90S2323} \
%{mmcu=at90s2333:-DAVR_AT90S2333} \
%{mmcu=at90s2343:-DAVR_AT90S2343} \
%{mmcu=attiny22:-DAVR_ATtiny22} \
%{mmcu=at90s4433:-DAVR_AT90S4433} \
%{mmcu=at90s4414:-DAVR_AT90S4414} \
%{mmcu=at90s4434:-DAVR_AT90S4434} \
%{mmcu=at90s8515:-DAVR_AT90S8515} \
%{mmcu=at90s8535:-DAVR_AT90S8535} \
%{mmcu=atmega603:-DAVR_ATmega603} \
%{mmcu=atmega103:-DAVR_ATmega103} \
%{mint8:-D__SIZE_TYPE__=long\\ unsigned\\ int -D__PTRDIFF_TYPE__=long -D__INT_MAX__=127} \
%{!mint*:-D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int -D__INT_MAX__=32767} \
%{posix:-D_POSIX_SOURCE}"
/* A C string constant that tells the GNU CC driver program options to
   pass to CPP.  It can also specify how to translate options you
   give to GNU CC into options for GNU CC to pass to the CPP.

   Do not define this macro if it does not need to do anything.  */

#define NO_BUILTIN_SIZE_TYPE
/* If this macro is defined, the preprocessor will not define the
   builtin macro `__SIZE_TYPE__'.  The macro `__SIZE_TYPE__' must
   then be defined by `CPP_SPEC' instead.

   This should be defined if `SIZE_TYPE' depends on target dependent
   flags which are not accessible to the preprocessor.  Otherwise, it
   should not be defined.  */

#define NO_BUILTIN_PTRDIFF_TYPE
/* If this macro is defined, the preprocessor will not define the
   builtin macro `__PTRDIFF_TYPE__'.  The macro `__PTRDIFF_TYPE__'
   must then be defined by `CPP_SPEC' instead.

   This should be defined if `PTRDIFF_TYPE' depends on target
   dependent flags which are not accessible to the preprocessor.
   Otherwise, it should not be defined.

   `SIGNED_CHAR_SPEC'
   A C string constant that tells the GNU CC driver program options to
   pass to CPP.  By default, this macro is defined to pass the option
   `-D__CHAR_UNSIGNED__' to CPP if `char' will be treated as
   `unsigned char' by `cc1'.

   Do not define this macro unless you need to override the default
   definition.  */

#define CC1_SPEC "%{!mmcu*:-mmcu=at90s8515} %{profile:-p}"
/* A C string constant that tells the GNU CC driver program options to
   pass to `cc1'.  It can also specify how to translate options you
   give to GNU CC into options for GNU CC to pass to the `cc1'.

   Do not define this macro if it does not need to do anything.  */

#define ASM_SPEC ""
/* A C string constant that tells the GNU CC driver program options to
   pass to the assembler.  It can also specify how to translate
   options you give to GNU CC into options for GNU CC to pass to the
   assembler.  See the file `sun3.h' for an example of this.

   Do not define this macro if it does not need to do anything.  */

#define ASM_FINAL_SPEC ""
/* A C string constant that tells the GNU CC driver program how to
   run any programs which cleanup after the normal assembler.
   Normally, this is not needed.  See the file `mips.h' for an
   example of this.

   Do not define this macro if it does not need to do anything.  */

#define LINK_SPEC "\
%{!mmcu*:-m avr85xx} \
%{mmcu=atmega603:-m avrmega603} \
%{mmcu=atmega103:-m avrmega103} \
%{mmcu=at90s2313:-m avr23xx} \
%{mmcu=at90s2323:-m avr23xx} \
%{mmcu=attiny22:-m avr23xx} \
%{mmcu=at90s2333:-m avr23xx} \
%{mmcu=at90s2343:-m avr23xx} \
%{mmcu=at90s4433:-m avr4433} \
%{mmcu=at90s4414:-m avr44x4} \
%{mmcu=at90s4434:-m avr44x4} \
%{mmcu=at90s8535:-m avr85xx} \
%{mmcu=at90s8515:-m avr85xx}"

/* A C string constant that tells the GNU CC driver program options to
   pass to the linker.  It can also specify how to translate options
   you give to GNU CC into options for GNU CC to pass to the linker.

   Do not define this macro if it does not need to do anything.  */

#define LIB_SPEC "\
%{!mmcu*|mmcu=at90s*|mmcu=attiny22: -lc} \
%{mmcu=atmega*: -lc-mega}"
/* Another C string constant used much like `LINK_SPEC'.  The
   difference between the two is that `LIB_SPEC' is used at the end
   of the command given to the linker.

   If this macro is not defined, a default is provided that loads the
   standard C library from the usual place.  See `gcc.c'.  */

#define LIBGCC_SPEC "\
%{mmcu=atmega*:-lgcc} \
%{!mmcu*|mmcu=at90s*|mmcu=attiny22:-lgcc}"
/* Another C string constant that tells the GNU CC driver program how
   and when to place a reference to `libgcc.a' into the linker
   command line.  This constant is placed both before and after the
   value of `LIB_SPEC'.

   If this macro is not defined, the GNU CC driver provides a default
   that passes the string `-lgcc' to the linker unless the `-shared'
   option is specified.  */

#define STARTFILE_SPEC "%(crt_binutils)"
/* Another C string constant used much like `LINK_SPEC'.  The
   difference between the two is that `STARTFILE_SPEC' is used at the
   very beginning of the command given to the linker.

   If this macro is not defined, a default is provided that loads the
   standard C startup file from the usual place.  See `gcc.c'.  */

#define ENDFILE_SPEC ""
/* Another C string constant used much like `LINK_SPEC'.  The
   difference between the two is that `ENDFILE_SPEC' is used at the
   very end of the command given to the linker.

   Do not define this macro if it does not need to do anything.  */

#define CRT_BINUTILS_SPECS "\
%{!mmcu*:gcrt1-8515.o%s} \
%{mmcu=atmega603:gcrt1-mega603.o%s} \
%{mmcu=atmega103:gcrt1-mega103.o%s} \
%{mmcu=at90s2313:gcrt1-2313.o%s} \
%{mmcu=at90s2323:gcrt1-2323.o%s} \
%{mmcu=attiny22:gcrt1-tiny22.o%s} \
%{mmcu=at90s2333:gcrt1-2333.o%s} \
%{mmcu=at90s2343:gcrt1-2343.o%s} \
%{mmcu=at90s4433:gcrt1-4433.o%s} \
%{mmcu=at90s4414:gcrt1-4414.o%s} \
%{mmcu=at90s4434:gcrt1-4434.o%s} \
%{mmcu=at90s8535:gcrt1-8535.o%s} \
%{mmcu=at90s8515:gcrt1-8515.o%s}"

#define EXTRA_SPECS				\
{"crt_binutils", CRT_BINUTILS_SPECS},
/* Define this macro to provide additional specifications to put in
   the `specs' file that can be used in various specifications like
   `CC1_SPEC'.

   The definition should be an initializer for an array of structures,
   containing a string constant, that defines the specification name,
   and a string constant that provides the specification.

   Do not define this macro if it does not need to do anything.

   `EXTRA_SPECS' is useful when an architecture contains several
   related targets, which have various `..._SPECS' which are similar
   to each other, and the maintainer would like one central place to
   keep these definitions.

   For example, the PowerPC System V.4 targets use `EXTRA_SPECS' to
   define either `_CALL_SYSV' when the System V calling sequence is
   used or `_CALL_AIX' when the older AIX-based calling sequence is
   used.

   The `config/rs6000/rs6000.h' target file defines:

   #define EXTRA_SPECS \
   { "cpp_sysv_default", CPP_SYSV_DEFAULT },

   #define CPP_SYS_DEFAULT ""

   The `config/rs6000/sysv.h' target file defines:
   #undef CPP_SPEC
   #define CPP_SPEC \
   "%{posix: -D_POSIX_SOURCE } \
   %{mcall-sysv: -D_CALL_SYSV } %{mcall-aix: -D_CALL_AIX } \
   %{!mcall-sysv: %{!mcall-aix: %(cpp_sysv_default) }} \
   %{msoft-float: -D_SOFT_FLOAT} %{mcpu=403: -D_SOFT_FLOAT}"

   #undef CPP_SYSV_DEFAULT
   #define CPP_SYSV_DEFAULT "-D_CALL_SYSV"

   while the `config/rs6000/eabiaix.h' target file defines
   `CPP_SYSV_DEFAULT' as:

   #undef CPP_SYSV_DEFAULT
   #define CPP_SYSV_DEFAULT "-D_CALL_AIX"  */

/* This is undefined macro for collect2 disabling */
#define LINKER_NAME "ld"

#define TEST_HARD_REG_CLASS(CLASS, REGNO) \
  TEST_HARD_REG_BIT (reg_class_contents[ (int) (CLASS)], REGNO)

/* Note that the other files fail to use these
   in some of the places where they should.  */

#if defined(__STDC__) || defined(ALMOST_STDC)
#define AS2(a,b,c) #a " " #b "," #c
#define AS2C(b,c) " " #b "," #c
#define AS3(a,b,c,d) #a " " #b "," #c "," #d
#define AS1(a,b) #a " " #b
#else
#define AS1(a,b) "a	b"
#define AS2(a,b,c) "a	b,c"
#define AS2C(b,c) " b,c"
#define AS3(a,b,c,d) "a	b,c,d"
#endif
#define OUT_AS1(a,b) output_asm_insn (AS1(a,b), operands)
#define OUT_AS2(a,b,c) output_asm_insn (AS2(a,b,c), operands)
#define CR_TAB "\n\t"

/* Define this macro as a C statement that declares additional library
   routines renames existing ones. `init_optabs' calls this macro
   after initializing all the normal library routines.  */

#define INIT_TARGET_OPTABS				\
{							\
  smul_optab->handlers[(int) QImode].libfunc		\
    = gen_rtx (SYMBOL_REF, Pmode, "_mulqi3");		\
							\
  sdiv_optab->handlers[(int) QImode].libfunc		\
    = gen_rtx (SYMBOL_REF, Pmode, "_divqi3");		\
							\
  smod_optab->handlers[(int) QImode].libfunc		\
    = gen_rtx (SYMBOL_REF, Pmode, "_modqi3");		\
							\
  udiv_optab->handlers[(int) QImode].libfunc		\
    = gen_rtx (SYMBOL_REF, Pmode, "_udivqi3");		\
							\
  umod_optab->handlers[(int) QImode].libfunc		\
    = gen_rtx (SYMBOL_REF, Pmode, "_umodqi3");		\
							\
  smul_optab->handlers[(int) HImode].libfunc		\
    = gen_rtx (SYMBOL_REF, Pmode, "_mulhi3");		\
							\
  sdiv_optab->handlers[(int) HImode].libfunc		\
    = gen_rtx (SYMBOL_REF, Pmode, "_divhi3");		\
							\
  smod_optab->handlers[(int) HImode].libfunc		\
    = gen_rtx (SYMBOL_REF, Pmode, "_modhi3");		\
							\
  udiv_optab->handlers[(int) HImode].libfunc		\
    = gen_rtx (SYMBOL_REF, Pmode, "_udivhi3");		\
							\
  umod_optab->handlers[(int) HImode].libfunc		\
    = gen_rtx (SYMBOL_REF, Pmode, "_umodhi3");		\
							\
  smul_optab->handlers[(int) SImode].libfunc		\
    = gen_rtx (SYMBOL_REF, Pmode, "_mulsi3");		\
							\
  sdiv_optab->handlers[(int) SImode].libfunc		\
    = gen_rtx (SYMBOL_REF, Pmode, "_divsi3");		\
							\
  smod_optab->handlers[(int) SImode].libfunc		\
    = gen_rtx (SYMBOL_REF, Pmode, "_modsi3");		\
							\
  udiv_optab->handlers[(int) SImode].libfunc		\
    = gen_rtx (SYMBOL_REF, Pmode, "_udivsi3");		\
							\
  umod_optab->handlers[(int) SImode].libfunc		\
    = gen_rtx (SYMBOL_REF, Pmode, "_umodsi3");		\
  avr_init_once();					\
}

/* Temporary register r0 */
#define TMP_REGNO 0

/* zero register r1 */
#define ZERO_REGNO 1

extern struct rtx_def *tmp_reg_rtx;
extern struct rtx_def *zero_reg_rtx;

#define TARGET_FLOAT_FORMAT IEEE_FLOAT_FORMAT

/* Define to use software floating point emulator for REAL_ARITHMETIC and
   decimal <-> binary conversion. */
#define REAL_ARITHMETIC

#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

#define DBX_REGISTER_NUMBER(r) (r)

/* Get the standard ELF stabs definitions.  */
#include "dbxelf.h"

#undef ASM_IDENTIFY_GCC
#define ASM_IDENTIFY_GCC(FILE)				\
do							\
  {							\
    if (write_symbols != DBX_DEBUG)			\
      fputs ("gcc2_compiled.:\n", FILE);		\
  }							\
while (0)
