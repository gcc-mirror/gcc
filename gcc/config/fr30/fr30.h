/*{{{  Comment.  */ 

/* Definitions of FR30 target. 
   Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.
   Contributed by Cygnus Solutions.

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

/*}}}*/ 
/*{{{  Includes.  */ 

/* Set up System V.4 (aka ELF) defaults.  */
#include "svr4.h"

/* Include prototyping macros */
#include "gansidecl.h"

/*}}}*/ 
/*{{{  Driver configuration.  */ 

/* A C expression which determines whether the option `-CHAR' takes arguments.
   The value should be the number of arguments that option takes-zero, for many
   options.

   By default, this macro is defined to handle the standard options properly.
   You need not define it unless you wish to add additional options which take
   arguments.

   Defined in svr4.h.  */
#undef SWITCH_TAKES_ARG

/* A C expression which determines whether the option `-NAME' takes arguments.
   The value should be the number of arguments that option takes-zero, for many
   options.  This macro rather than `SWITCH_TAKES_ARG' is used for
   multi-character option names.

   By default, this macro is defined as `DEFAULT_WORD_SWITCH_TAKES_ARG', which
   handles the standard options properly.  You need not define
   `WORD_SWITCH_TAKES_ARG' unless you wish to add additional options which take
   arguments.  Any redefinition should call `DEFAULT_WORD_SWITCH_TAKES_ARG' and
   then check for additional options.

   Defined in svr4.h.  */
#undef WORD_SWITCH_TAKES_ARG

/*}}}*/ 
/*{{{  Run-time target specifications.  */ 

#undef  ASM_SPEC
#define ASM_SPEC "%{v}"

/* Define this to be a string constant containing `-D' options to define the
   predefined macros that identify this machine and system.  These macros will
   be predefined unless the `-ansi' option is specified. */

#define CPP_PREDEFINES "-Dfr30 -D__fr30__ -Amachine(fr30)"

/* Use LDI:20 instead of LDI:32 to load addresses.  */
#define TARGET_SMALL_MODEL_MASK	(1 << 0)
#define TARGET_SMALL_MODEL	(target_flags & TARGET_SMALL_MODEL_MASK)

#define TARGET_DEFAULT		0

/* This declaration should be present.  */
extern int target_flags;

#define TARGET_SWITCHES \
{	\
  { "small-model",      TARGET_SMALL_MODEL_MASK, "Assume small address space" }, \
  { "no-small-model", - TARGET_SMALL_MODEL_MASK, "" },			 	 \
  { "no-lsim",          0, "" },					 	 \
  { "",                 TARGET_DEFAULT }					 \
}

#define TARGET_VERSION fprintf (stderr, " (fr30)");

/* Define this macro if debugging can be performed even without a frame
   pointer.  If this macro is defined, GNU CC will turn on the
   `-fomit-frame-pointer' option whenever `-O' is specified.  */
#define CAN_DEBUG_WITHOUT_FP

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "crt0.o%s crti.o%s crtbegin.o%s"

/* Include the OS stub library, so that the code can be simulated.
   This is not the right way to do this.  Ideally this kind of thing
   should be done in the linker script - but I have not worked out how
   to specify the location of a linker script in a gcc command line yet... */
#undef  ENDFILE_SPEC
#define ENDFILE_SPEC  "%{!mno-lsim:-lsim} crtend.o%s crtn.o%s"

/*}}}*/ 
/*{{{  Storage Layout.  */ 

/* Define this macro to have the value 1 if the most significant bit in a byte
   has the lowest number; otherwise define it to have the value zero.  This
   means that bit-field instructions count from the most significant bit.  If
   the machine has no bit-field instructions, then this must still be defined,
   but it doesn't matter which value it is defined to.  This macro need not be
   a constant.

   This macro does not affect the way structure fields are packed into bytes or
   words; that is controlled by `BYTES_BIG_ENDIAN'.  */
#define BITS_BIG_ENDIAN 1

/* Define this macro to have the value 1 if the most significant byte in a word
   has the lowest number.  This macro need not be a constant.  */
#define BYTES_BIG_ENDIAN 1

/* Define this macro to have the value 1 if, in a multiword object, the most
   significant word has the lowest number.  This applies to both memory
   locations and registers; GNU CC fundamentally assumes that the order of
   words in memory is the same as the order in registers.  This macro need not
   be a constant.  */
#define WORDS_BIG_ENDIAN 1

/* Define this macro to be the number of bits in an addressable storage unit
   (byte); normally 8.  */
#define BITS_PER_UNIT 	8

/* Number of bits in a word; normally 32.  */
#define BITS_PER_WORD 	32

/* Number of storage units in a word; normally 4.  */
#define UNITS_PER_WORD 	4

/* Width of a pointer, in bits.  You must specify a value no wider than the
   width of `Pmode'.  If it is not equal to the width of `Pmode', you must
   define `POINTERS_EXTEND_UNSIGNED'.  */
#define POINTER_SIZE 	32

/* A macro to update MODE and UNSIGNEDP when an object whose type is TYPE and
   which has the specified mode and signedness is to be stored in a register.
   This macro is only called when TYPE is a scalar type.

   On most RISC machines, which only have operations that operate on a full
   register, define this macro to set M to `word_mode' if M is an integer mode
   narrower than `BITS_PER_WORD'.  In most cases, only integer modes should be
   widened because wider-precision floating-point operations are usually more
   expensive than their narrower counterparts.

   For most machines, the macro definition does not change UNSIGNEDP.  However,
   some machines, have instructions that preferentially handle either signed or
   unsigned quantities of certain modes.  For example, on the DEC Alpha, 32-bit
   loads from memory and 32-bit add instructions sign-extend the result to 64
   bits.  On such machines, set UNSIGNEDP according to which kind of extension
   is more efficient.

   Do not define this macro if it would never modify MODE.  */
#define PROMOTE_MODE(MODE,UNSIGNEDP,TYPE)	\
  do						\
    {						\
      if (GET_MODE_CLASS (MODE) == MODE_INT	\
	  && GET_MODE_SIZE (MODE) < 4)		\
	(MODE) = SImode;			\
    }						\
  while (0)

/* Normal alignment required for function parameters on the stack, in bits.
   All stack parameters receive at least this much alignment regardless of data
   type.  On most machines, this is the same as the size of an integer.  */
#define PARM_BOUNDARY 32

/* Define this macro if you wish to preserve a certain alignment for the stack
   pointer.  The definition is a C expression for the desired alignment
   (measured in bits).

   If `PUSH_ROUNDING' is not defined, the stack will always be aligned to the
   specified boundary.  If `PUSH_ROUNDING' is defined and specifies a less
   strict alignment than `STACK_BOUNDARY', the stack may be momentarily
   unaligned while pushing arguments.  */
#define STACK_BOUNDARY 32

/* Alignment required for a function entry point, in bits.  */
#define FUNCTION_BOUNDARY 32

/* Biggest alignment that any data type can require on this machine,
   in bits.  */
#define BIGGEST_ALIGNMENT 32

/* If defined, a C expression to compute the alignment for a static variable.
   TYPE is the data type, and ALIGN is the alignment that the object
   would ordinarily have.  The value of this macro is used instead of that
   alignment to align the object.

   If this macro is not defined, then ALIGN is used.

   One use of this macro is to increase alignment of medium-size data to make
   it all fit in fewer cache lines.  Another is to cause character arrays to be
   word-aligned so that `strcpy' calls that copy constants to character arrays
   can be done inline.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

/* If defined, a C expression to compute the alignment given to a constant that
   is being placed in memory.  CONSTANT is the constant and ALIGN is the
   alignment that the object would ordinarily have.  The value of this macro is
   used instead of that alignment to align the object.

   If this macro is not defined, then ALIGN is used.

   The typical use of this macro is to increase alignment for string constants
   to be word aligned so that `strcpy' calls that copy constants can be done
   inline.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  (TREE_CODE (EXP) == STRING_CST	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

/* Define this macro to be the value 1 if instructions will fail to work if
   given data not on the nominal alignment.  If instructions will merely go
   slower in that case, define this macro as 0.  */
#define STRICT_ALIGNMENT 1

/* Define this if you wish to imitate the way many other C compilers handle
   alignment of bitfields and the structures that contain them.

   The behavior is that the type written for a bitfield (`int', `short', or
   other integer type) imposes an alignment for the entire structure, as if the
   structure really did contain an ordinary field of that type.  In addition,
   the bitfield is placed within the structure so that it would fit within such
   a field, not crossing a boundary for it.

   Thus, on most machines, a bitfield whose type is written as `int' would not
   cross a four-byte boundary, and would force four-byte alignment for the
   whole structure.  (The alignment used may not be four bytes; it is
   controlled by the other alignment parameters.)

   If the macro is defined, its definition should be a C expression; a nonzero
   value for the expression enables this behavior.

   Note that if this macro is not defined, or its value is zero, some bitfields
   may cross more than one alignment boundary.  The compiler can support such
   references if there are `insv', `extv', and `extzv' insns that can directly
   reference memory.

   The other known way of making bitfields work is to define
   `STRUCTURE_SIZE_BOUNDARY' as large as `BIGGEST_ALIGNMENT'.  Then every
   structure can be accessed with fullwords.

   Unless the machine has bitfield instructions or you define
   `STRUCTURE_SIZE_BOUNDARY' that way, you must define
   `PCC_BITFIELD_TYPE_MATTERS' to have a nonzero value.

   If your aim is to make GNU CC use the same conventions for laying out
   bitfields as are used by another compiler, here is how to investigate what
   the other compiler does.  Compile and run this program:

        struct foo1
        {
          char x;
          char :0;
          char y;
        };

        struct foo2
        {
          char x;
          int :0;
          char y;
        };

        main ()
        {
          printf ("Size of foo1 is %d\n",
                  sizeof (struct foo1));
          printf ("Size of foo2 is %d\n",
                  sizeof (struct foo2));
          exit (0);
        }

   If this prints 2 and 5, then the compiler's behavior is what you would get
   from `PCC_BITFIELD_TYPE_MATTERS'.

   Defined in svr4.h.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* A code distinguishing the floating point format of the target machine.
   There are three defined values:

   IEEE_FLOAT_FORMAT'
        This code indicates IEEE floating point.  It is the default;
        there is no need to define this macro when the format is IEEE.

   VAX_FLOAT_FORMAT'
        This code indicates the peculiar format used on the Vax.

   UNKNOWN_FLOAT_FORMAT'
        This code indicates any other format.

   The value of this macro is compared with `HOST_FLOAT_FORMAT'
   to determine whether the target machine has the same format as
   the host machine.  If any other formats are actually in use on supported
   machines, new codes should be defined for them.

   The ordering of the component words of floating point values stored in
   memory is controlled by `FLOAT_WORDS_BIG_ENDIAN' for the target machine and
   `HOST_FLOAT_WORDS_BIG_ENDIAN' for the host.  */
#define TARGET_FLOAT_FORMAT IEEE_FLOAT_FORMAT

/* GNU CC supports two ways of implementing C++ vtables: traditional or with
   so-called "thunks".  The flag `-fvtable-thunk' chooses between them.  Define
   this macro to be a C expression for the default value of that flag.  If
   `DEFAULT_VTABLE_THUNKS' is 0, GNU CC uses the traditional implementation by
   default.  The "thunk" implementation is more efficient (especially if you
   have provided an implementation of `ASM_OUTPUT_MI_THUNK', but is not binary
   compatible with code compiled using the traditional implementation.  If you
   are writing a new ports, define `DEFAULT_VTABLE_THUNKS' to 1.

   If you do not define this macro, the default for `-fvtable-thunk' is 0.  */
#define DEFAULT_VTABLE_THUNKS 1

/*}}}*/ 
/*{{{  Layout of Source Language Data Types.  */ 

#define CHAR_TYPE_SIZE 		 8
#define SHORT_TYPE_SIZE 	16
#define INT_TYPE_SIZE 		32
#define LONG_TYPE_SIZE 		32
#define LONG_LONG_TYPE_SIZE 	64
#define FLOAT_TYPE_SIZE 	32
#define DOUBLE_TYPE_SIZE 	64
#define LONG_DOUBLE_TYPE_SIZE 	64

/* An expression whose value is 1 or 0, according to whether the type `char'
   should be signed or unsigned by default.  The user can always override this
   default with the options `-fsigned-char' and `-funsigned-char'.  */
#define DEFAULT_SIGNED_CHAR 1

#define TARGET_BELL     0x7	/*  '\a'  */
#define TARGET_BS	0x8	/*  '\b'  */
#define TARGET_TAB	0x9	/*  '\t'  */
#define TARGET_NEWLINE	0xa	/*  '\n'  */
#define TARGET_VT	0xb	/*  '\v'  */
#define TARGET_FF	0xc	/*  '\f'  */
#define TARGET_CR	0xd	/*  '\r'  */

/*}}}*/ 
/*{{{  REGISTER BASICS.  */ 

/* Number of hardware registers known to the compiler.  They receive numbers 0
   through `FIRST_PSEUDO_REGISTER-1'; thus, the first pseudo register's number
   really is assigned the number `FIRST_PSEUDO_REGISTER'.  */
#define FIRST_PSEUDO_REGISTER	21

/* Fixed register assignments: */

/* Here we do a BAD THING - reserve a register for use by the machine
   description file.  There are too many places in compiler where it
   assumes that it can issue a branch or jump instruction without
   providing a scratch register for it, and reload just cannot cope, so
   we keep a register back for these situations.  */
#define COMPILER_SCRATCH_REGISTER 0

/* The register that contains the result of a function call.  */
#define RETURN_VALUE_REGNUM	 4

/* The first register that can contain the arguments to a function.  */
#define FIRST_ARG_REGNUM	 4

/* A call-used register that can be used during the function prologue.  */
#define PROLOGUE_TMP_REGNUM	 COMPILER_SCRATCH_REGISTER
     
/* Register numbers used for passing a function's static chain pointer.  If
   register windows are used, the register number as seen by the called
   function is `STATIC_CHAIN_INCOMING_REGNUM', while the register number as
   seen by the calling function is `STATIC_CHAIN_REGNUM'.  If these registers
   are the same, `STATIC_CHAIN_INCOMING_REGNUM' need not be defined.

   The static chain register need not be a fixed register.

   If the static chain is passed in memory, these macros should not be defined;
   instead, the next two macros should be defined.  */
#define STATIC_CHAIN_REGNUM 	12
/* #define STATIC_CHAIN_INCOMING_REGNUM */

/* An FR30 specific hardware register.  */
#define ACCUMULATOR_REGNUM	13

/* The register number of the frame pointer register, which is used to access
   automatic variables in the stack frame.  On some machines, the hardware
   determines which register this is.  On other machines, you can choose any
   register you wish for this purpose.  */
#define FRAME_POINTER_REGNUM	14
     
/* The register number of the stack pointer register, which must also be a
   fixed register according to `FIXED_REGISTERS'.  On most machines, the
   hardware determines which register this is.  */
#define STACK_POINTER_REGNUM	15

/* The following a fake hard registers that describe some of the dedicated
   registers on the FR30.  */
#define CONDITION_CODE_REGNUM	16
#define RETURN_POINTER_REGNUM	17
#define MD_HIGH_REGNUM		18
#define MD_LOW_REGNUM		19

/* An initializer that says which registers are used for fixed purposes all
   throughout the compiled code and are therefore not available for general
   allocation.  These would include the stack pointer, the frame pointer
   (except on machines where that can be used as a general register when no
   frame pointer is needed), the program counter on machines where that is
   considered one of the addressable registers, and any other numbered register
   with a standard use.

   This information is expressed as a sequence of numbers, separated by commas
   and surrounded by braces.  The Nth number is 1 if register N is fixed, 0
   otherwise.

   The table initialized from this macro, and the table initialized by the
   following one, may be overridden at run time either automatically, by the
   actions of the macro `CONDITIONAL_REGISTER_USAGE', or by the user with the
   command options `-ffixed-REG', `-fcall-used-REG' and `-fcall-saved-REG'.  */
#define FIXED_REGISTERS 			\
  { 1, 0, 0, 0, 0, 0, 0, 0, 	/*  0 -  7 */ 	\
    0, 0, 0, 0, 0, 0, 0, 1,	/*  8 - 15 */ 	\
    1, 1, 1, 1, 1 }		/* 16 - 20 */

/* XXX - MDL and MDH set as fixed for now - this is until I can get the
   mul patterns working.  */

/* Like `FIXED_REGISTERS' but has 1 for each register that is clobbered (in
   general) by function calls as well as for fixed registers.  This macro
   therefore identifies the registers that are not available for general
   allocation of values that must live across function calls.

   If a register has 0 in `CALL_USED_REGISTERS', the compiler automatically
   saves it on function entry and restores it on function exit, if the register
   is used within the function.  */
#define CALL_USED_REGISTERS 			\
  { 1, 1, 1, 1, 1, 1, 1, 1,	/*  0 -  7 */ 	\
    0, 0, 0, 0, 1, 1, 0, 1,	/*  8 - 15 */ 	\
    1, 1, 1, 1, 1 }		/* 16 - 20 */

/* A C initializer containing the assembler's names for the machine registers,
   each one as a C string constant.  This is what translates register numbers
   in the compiler into assembler language.  */
#define REGISTER_NAMES 						\
{   "r0", "r1", "r2",  "r3",  "r4",  "r5", "r6", "r7",	\
    "r8", "r9", "r10", "r11", "r12", "ac", "fp", "sp",	\
    "cc", "rp", "mdh", "mdl", "ap"			\
}

/* If defined, a C initializer for an array of structures containing a name and
   a register number.  This macro defines additional names for hard registers,
   thus allowing the `asm' option in declarations to refer to registers using
   alternate names.  */
#define ADDITIONAL_REGISTER_NAMES 				\
{								\
  {"r13", 13}, {"r14", 14}, {"r15", 15}, {"usp", 15}, {"ps", 16}\
}

/*}}}*/ 
/*{{{  How Values Fit in Registers.  */ 

/* A C expression for the number of consecutive hard registers, starting at
   register number REGNO, required to hold a value of mode MODE.  */

#define HARD_REGNO_NREGS(REGNO, MODE) 			\
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* A C expression that is nonzero if it is permissible to store a value of mode
   MODE in hard register number REGNO (or in several registers starting with
   that one).  */

#define HARD_REGNO_MODE_OK(REGNO, MODE) 1

/* A C expression that is nonzero if it is desirable to choose register
   allocation so as to avoid move instructions between a value of mode MODE1
   and a value of mode MODE2.

   If `HARD_REGNO_MODE_OK (R, MODE1)' and `HARD_REGNO_MODE_OK (R, MODE2)' are
   ever different for any R, then `MODES_TIEABLE_P (MODE1, MODE2)' must be
   zero.  */
#define MODES_TIEABLE_P(MODE1, MODE2) 1

/*}}}*/ 
/*{{{  Register Classes.  */ 

/* An enumeral type that must be defined with all the register class names as
   enumeral values.  `NO_REGS' must be first.  `ALL_REGS' must be the last
   register class, followed by one more enumeral value, `LIM_REG_CLASSES',
   which is not a register class but rather tells how many classes there are.

   Each register class has a number, which is the value of casting the class
   name to type `int'.  The number serves as an index in many of the tables
   described below.  */
enum reg_class
{
  NO_REGS,
  MULTIPLY_32_REG,	/* the MDL register as used by the MULH, MULUH insns */
  MULTIPLY_64_REG,	/* the MDH,MDL register pair as used by MUL and MULU */
  LOW_REGS,		/* registers 0 through 7 */
  HIGH_REGS,		/* registers 8 through 15 */
  REAL_REGS,		/* ie all the general hardware registers on the FR30 */
  ALL_REGS,
  LIM_REG_CLASSES
};

#define GENERAL_REGS 	REAL_REGS
#define N_REG_CLASSES 	((int) LIM_REG_CLASSES)

/* An initializer containing the names of the register classes as C string
   constants.  These names are used in writing some of the debugging dumps.  */
#define REG_CLASS_NAMES \
{			\
  "NO_REGS",		\
  "MULTIPLY_32_REG",	\
  "MULTIPLY_64_REG",	\
  "LOW_REGS", 		\
  "HIGH_REGS", 		\
  "REAL_REGS",		\
  "ALL_REGS"		\
 }

/* An initializer containing the contents of the register classes, as integers
   which are bit masks.  The Nth integer specifies the contents of class N.
   The way the integer MASK is interpreted is that register R is in the class
   if `MASK & (1 << R)' is 1.

   When the machine has more than 32 registers, an integer does not suffice.
   Then the integers are replaced by sub-initializers, braced groupings
   containing several integers.  Each sub-initializer must be suitable as an
   initializer for the type `HARD_REG_SET' which is defined in
   `hard-reg-set.h'.  */
#define REG_CLASS_CONTENTS 				\
{ 							\
  { 0 },						\
  { 1 << MD_LOW_REGNUM },				\
  { (1 << MD_LOW_REGNUM) | (1 << MD_HIGH_REGNUM) },	\
  { (1 << 8) - 1 },					\
  { ((1 << 8) - 1) << 8 },				\
  { (1 << CONDITION_CODE_REGNUM) - 1 },			\
  { (1 << FIRST_PSEUDO_REGISTER) - 1 }			\
}

/* A C expression whose value is a register class containing hard register
   REGNO.  In general there is more than one such class; choose a class which
   is "minimal", meaning that no smaller class also contains the register.  */
#define REGNO_REG_CLASS(REGNO) 			\
  ( (REGNO) < 8 ? LOW_REGS			\
  : (REGNO) < CONDITION_CODE_REGNUM ? HIGH_REGS	\
  : (REGNO) == MD_LOW_REGNUM ? MULTIPLY_32_REG	\
  : (REGNO) == MD_HIGH_REGNUM ? MULTIPLY_64_REG	\
  : ALL_REGS)

/* A macro whose definition is the name of the class to which a valid base
   register must belong.  A base register is one used in an address which is
   the register value plus a displacement.  */
#define BASE_REG_CLASS 	REAL_REGS

/* A macro whose definition is the name of the class to which a valid index
   register must belong.  An index register is one used in an address where its
   value is either multiplied by a scale factor or added to another register
   (as well as added to a displacement).  */
#define INDEX_REG_CLASS REAL_REGS

/* A C expression which defines the machine-dependent operand constraint
   letters for register classes.  If CHAR is such a letter, the value should be
   the register class corresponding to it.  Otherwise, the value should be
   `NO_REGS'.  The register letter `r', corresponding to class `GENERAL_REGS',
   will not be passed to this macro; you do not need to handle it.

   The following letters are unavailable, due to being used as
   constraints:
	'0'..'9'
	'<', '>'
	'E', 'F', 'G', 'H'
	'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P'
	'Q', 'R', 'S', 'T', 'U'
	'V', 'X'
	'g', 'i', 'm', 'n', 'o', 'p', 'r', 's' */

#define REG_CLASS_FROM_LETTER(CHAR) 	\
     (  (CHAR) == 'd' ? MULTIPLY_64_REG	\
      : (CHAR) == 'e' ? MULTIPLY_32_REG	\
      : (CHAR) == 'h' ? HIGH_REGS	\
      : (CHAR) == 'l' ? LOW_REGS	\
      : (CHAR) == 'a' ? ALL_REGS	\
      : NO_REGS)

/* A C expression which is nonzero if register number NUM is suitable for use
   as a base register in operand addresses.  It may be either a suitable hard
   register or a pseudo register that has been allocated such a hard register.  */
#define REGNO_OK_FOR_BASE_P(NUM) 1

/* A C expression which is nonzero if register number NUM is suitable for use
   as an index register in operand addresses.  It may be either a suitable hard
   register or a pseudo register that has been allocated such a hard register.

   The difference between an index register and a base register is that the
   index register may be scaled.  If an address involves the sum of two
   registers, neither one of them scaled, then either one may be labeled the
   "base" and the other the "index"; but whichever labeling is used must fit
   the machine's constraints of which registers may serve in each capacity.
   The compiler will try both labelings, looking for one that is valid, and
   will reload one or both registers only if neither labeling works.  */
#define REGNO_OK_FOR_INDEX_P(NUM) 1

/* A C expression that places additional restrictions on the register class to
   use when it is necessary to copy value X into a register in class CLASS.
   The value is a register class; perhaps CLASS, or perhaps another, smaller
   class.  On many machines, the following definition is safe:

        #define PREFERRED_RELOAD_CLASS(X,CLASS) CLASS

   Sometimes returning a more restrictive class makes better code.  For
   example, on the 68000, when X is an integer constant that is in range for a
   `moveq' instruction, the value of this macro is always `DATA_REGS' as long
   as CLASS includes the data registers.  Requiring a data register guarantees
   that a `moveq' will be used.

   If X is a `const_double', by returning `NO_REGS' you can force X into a
   memory constant.  This is useful on certain machines where immediate
   floating values cannot be loaded into certain kinds of registers.  */
#define PREFERRED_RELOAD_CLASS(X, CLASS) CLASS

/* A C expression for the maximum number of consecutive registers of
   class CLASS needed to hold a value of mode MODE.

   This is closely related to the macro `HARD_REGNO_NREGS'.  In fact, the value
   of the macro `CLASS_MAX_NREGS (CLASS, MODE)' should be the maximum value of
   `HARD_REGNO_NREGS (REGNO, MODE)' for all REGNO values in the class CLASS.

   This macro helps control the handling of multiple-word values in
   the reload pass.  */
#define CLASS_MAX_NREGS(CLASS, MODE) HARD_REGNO_NREGS (0, MODE)

/*}}}*/ 
/*{{{  CONSTANTS.  */ 

/* Return true if a value is inside a range */
#define IN_RANGE(VALUE, LOW, HIGH)					\
  (   ((unsigned HOST_WIDE_INT)((VALUE) - (LOW)))			\
   <= ((unsigned HOST_WIDE_INT)( (HIGH) - (LOW))))

/* A C expression that defines the machine-dependent operand constraint letters
   (`I', `J', `K', .. 'P') that specify particular ranges of integer values.
   If C is one of those letters, the expression should check that VALUE, an
   integer, is in the appropriate range and return 1 if so, 0 otherwise.  If C
   is not one of those letters, the value should be 0 regardless of VALUE.  */
#define CONST_OK_FOR_LETTER_P(VALUE, C) 			\
 (  (C) == 'I' ? IN_RANGE (VALUE,    0,       15)		\
  : (C) == 'J' ? IN_RANGE (VALUE,  -16,       -1)		\
  : (C) == 'K' ? IN_RANGE (VALUE,   16,       31)		\
  : (C) == 'L' ? IN_RANGE (VALUE,    0,       (1 <<  8) - 1)	\
  : (C) == 'M' ? IN_RANGE (VALUE,    0,       (1 << 20) - 1)	\
  : (C) == 'P' ? IN_RANGE (VALUE,  -(1 << 8), (1 <<  8) - 1)	\
  : 0)
     
/* A C expression that defines the machine-dependent operand constraint letters
   (`G', `H') that specify particular ranges of `const_double' values.

   If C is one of those letters, the expression should check that VALUE, an RTX
   of code `const_double', is in the appropriate range and return 1 if so, 0
   otherwise.  If C is not one of those letters, the value should be 0
   regardless of VALUE.

   `const_double' is used for all floating-point constants and for `DImode'
   fixed-point constants.  A given letter can accept either or both kinds of
   values.  It can use `GET_MODE' to distinguish between these kinds.  */
#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C) 0

/* A C expression that defines the optional machine-dependent constraint
   letters (`Q', `R', `S', `T', `U') that can be used to segregate specific
   types of operands, usually memory references, for the target machine.
   Normally this macro will not be defined.  If it is required for a particular
   target machine, it should return 1 if VALUE corresponds to the operand type
   represented by the constraint letter C.  If C is not defined as an extra
   constraint, the value returned should be 0 regardless of VALUE.

   For example, on the ROMP, load instructions cannot have their output in r0
   if the memory reference contains a symbolic address.  Constraint letter `Q'
   is defined as representing a memory address that does *not* contain a
   symbolic address.  An alternative is specified with a `Q' constraint on the
   input and `r' on the output.  The next alternative specifies `m' on the
   input and a register class that does not include r0 on the output.  */
#define EXTRA_CONSTRAINT(VALUE, C) \
   ((C) == 'Q' ? (GET_CODE (VALUE) == MEM && GET_CODE (XEXP (VALUE, 0)) == SYMBOL_REF) : 0)

/*}}}*/ 
/*{{{  Basic Stack Layout.  */ 

/* Define this macro if pushing a word onto the stack moves the stack pointer
   to a smaller address.  */
#define STACK_GROWS_DOWNWARD 1

/* Define this macro if the addresses of local variable slots are at negative
   offsets from the frame pointer.  */
#define FRAME_GROWS_DOWNWARD 1

/* Offset from the frame pointer to the first local variable slot to be
   allocated.

   If `FRAME_GROWS_DOWNWARD', find the next slot's offset by subtracting the
   first slot's length from `STARTING_FRAME_OFFSET'.  Otherwise, it is found by
   adding the length of the first slot to the value `STARTING_FRAME_OFFSET'.  */
/* #define STARTING_FRAME_OFFSET -4 */
#define STARTING_FRAME_OFFSET 0

/* Offset from the stack pointer register to the first location at which
   outgoing arguments are placed.  If not specified, the default value of zero
   is used.  This is the proper value for most machines.

   If `ARGS_GROW_DOWNWARD', this is the offset to the location above the first
   location at which outgoing arguments are placed.  */
#define STACK_POINTER_OFFSET 0

/* Offset from the argument pointer register to the first argument's address.
   On some machines it may depend on the data type of the function.

   If `ARGS_GROW_DOWNWARD', this is the offset to the location above the first
   argument's address.  */
#define FIRST_PARM_OFFSET(FUNDECL) 0

/* A C expression whose value is RTL representing the location of the incoming
   return address at the beginning of any function, before the prologue.  This
   RTL is either a `REG', indicating that the return value is saved in `REG',
   or a `MEM' representing a location in the stack.

   You only need to define this macro if you want to support call frame
   debugging information like that provided by DWARF 2.  */
#define INCOMING_RETURN_ADDR_RTX gen_rtx_REG (SImode, RETURN_POINTER_REGNUM)

/*}}}*/ 
/*{{{  Register That Address the Stack Frame.  */ 

/* The register number of the arg pointer register, which is used to access the
   function's argument list.  On some machines, this is the same as the frame
   pointer register.  On some machines, the hardware determines which register
   this is.  On other machines, you can choose any register you wish for this
   purpose.  If this is not the same register as the frame pointer register,
   then you must mark it as a fixed register according to `FIXED_REGISTERS', or
   arrange to be able to eliminate it.  */
#define ARG_POINTER_REGNUM 20

/*}}}*/ 
/*{{{  Eliminating the Frame Pointer and the Arg Pointer.  */ 

/* A C expression which is nonzero if a function must have and use a frame
   pointer.  This expression is evaluated in the reload pass.  If its value is
   nonzero the function will have a frame pointer.

   The expression can in principle examine the current function and decide
   according to the facts, but on most machines the constant 0 or the constant
   1 suffices.  Use 0 when the machine allows code to be generated with no
   frame pointer, and doing so saves some time or space.  Use 1 when there is
   no possible advantage to avoiding a frame pointer.

   In certain cases, the compiler does not know how to produce valid code
   without a frame pointer.  The compiler recognizes those cases and
   automatically gives the function a frame pointer regardless of what
   `FRAME_POINTER_REQUIRED' says.  You don't need to worry about them.

   In a function that does not require a frame pointer, the frame pointer
   register can be allocated for ordinary usage, unless you mark it as a fixed
   register.  See `FIXED_REGISTERS' for more information.  */
/* #define FRAME_POINTER_REQUIRED 0 */
#define FRAME_POINTER_REQUIRED \
     (flag_omit_frame_pointer == 0 || current_function_pretend_args_size > 0)

/* If defined, this macro specifies a table of register pairs used to eliminate
   unneeded registers that point into the stack frame.  If it is not defined,
   the only elimination attempted by the compiler is to replace references to
   the frame pointer with references to the stack pointer.

   The definition of this macro is a list of structure initializations, each of
   which specifies an original and replacement register.

   On some machines, the position of the argument pointer is not known until
   the compilation is completed.  In such a case, a separate hard register must
   be used for the argument pointer.  This register can be eliminated by
   replacing it with either the frame pointer or the argument pointer,
   depending on whether or not the frame pointer has been eliminated.

   In this case, you might specify:
        #define ELIMINABLE_REGS  \
        {{ARG_POINTER_REGNUM, STACK_POINTER_REGNUM}, \
         {ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM}, \
         {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}}

   Note that the elimination of the argument pointer with the stack pointer is
   specified first since that is the preferred elimination.  */

#define ELIMINABLE_REGS				\
{						\
  {ARG_POINTER_REGNUM,	 STACK_POINTER_REGNUM},	\
  {ARG_POINTER_REGNUM,	 FRAME_POINTER_REGNUM},	\
  {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}	\
}

/* A C expression that returns non-zero if the compiler is allowed to try to
   replace register number FROM with register number TO.  This macro
   need only be defined if `ELIMINABLE_REGS' is defined, and will usually be
   the constant 1, since most of the cases preventing register elimination are
   things that the compiler already knows about.  */

#define CAN_ELIMINATE(FROM, TO)						\
 ((TO) == FRAME_POINTER_REGNUM || ! frame_pointer_needed)

/* This macro is similar to `INITIAL_FRAME_POINTER_OFFSET'.  It specifies the
   initial difference between the specified pair of registers.  This macro must
   be defined if `ELIMINABLE_REGS' is defined.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
     (OFFSET) = fr30_compute_frame_size (FROM, TO)

/*}}}*/ 
/*{{{  Passing Function Arguments on the Stack.  */ 

/* Define this macro if an argument declared in a prototype as an integral type
   smaller than `int' should actually be passed as an `int'.  In addition to
   avoiding errors in certain cases of mismatch, it also makes for better code
   on certain machines.  */
#define PROMOTE_PROTOTYPES 1

/* If defined, the maximum amount of space required for outgoing arguments will
   be computed and placed into the variable
   `current_function_outgoing_args_size'.  No space will be pushed onto the
   stack for each call; instead, the function prologue should increase the
   stack frame size by this amount.

   Defining both `PUSH_ROUNDING' and `ACCUMULATE_OUTGOING_ARGS' is not
   proper.  */
#define ACCUMULATE_OUTGOING_ARGS

/* A C expression that should indicate the number of bytes of its own arguments
   that a function pops on returning, or 0 if the function pops no arguments
   and the caller must therefore pop them all after the function returns.

   FUNDECL is a C variable whose value is a tree node that describes the
   function in question.  Normally it is a node of type `FUNCTION_DECL' that
   describes the declaration of the function.  From this it is possible to
   obtain the DECL_MACHINE_ATTRIBUTES of the function.

   FUNTYPE is a C variable whose value is a tree node that describes the
   function in question.  Normally it is a node of type `FUNCTION_TYPE' that
   describes the data type of the function.  From this it is possible to obtain
   the data types of the value and arguments (if known).

   When a call to a library function is being considered, FUNTYPE will contain
   an identifier node for the library function.  Thus, if you need to
   distinguish among various library functions, you can do so by their names.
   Note that "library function" in this context means a function used to
   perform arithmetic, whose name is known specially in the compiler and was
   not mentioned in the C code being compiled.

   STACK-SIZE is the number of bytes of arguments passed on the stack.  If a
   variable number of bytes is passed, it is zero, and argument popping will
   always be the responsibility of the calling function.

   On the Vax, all functions always pop their arguments, so the definition of
   this macro is STACK-SIZE.  On the 68000, using the standard calling
   convention, no functions pop their arguments, so the value of the macro is
   always 0 in this case.  But an alternative calling convention is available
   in which functions that take a fixed number of arguments pop them but other
   functions (such as `printf') pop nothing (the caller pops all).  When this
   convention is in use, FUNTYPE is examined to determine whether a function
   takes a fixed number of arguments.  */
#define RETURN_POPS_ARGS(FUNDECL, FUNTYPE, STACK_SIZE) 0

/* Implement `va_arg'.  */
#define EXPAND_BUILTIN_VA_ARG(valist, type) \
  fr30_va_arg (valist, type)

/*}}}*/ 
/*{{{  Function Arguments in Registers.  */ 

/* Nonzero if we do not know how to pass TYPE solely in registers.
   We cannot do so in the following cases:

   - if the type has variable size
   - if the type is marked as addressable (it is required to be constructed
     into the stack)
   - if the type is a structure or union. */

#define MUST_PASS_IN_STACK(MODE,TYPE)				\
   (((MODE) == BLKmode)						\
    || ((TYPE) != 0						\
         && (TREE_CODE (TYPE_SIZE (TYPE)) != INTEGER_CST	\
	     || TREE_CODE (TYPE) == RECORD_TYPE			\
	     || TREE_CODE (TYPE) == UNION_TYPE			\
	     || TREE_CODE (TYPE) == QUAL_UNION_TYPE		\
             || TREE_ADDRESSABLE (TYPE))))

/* The number of register assigned to holding function arguments.  */
     
#define FR30_NUM_ARG_REGS	 4

/* A C expression that controls whether a function argument is passed in a
   register, and which register.

   The usual way to make the ANSI library `stdarg.h' work on a machine where
   some arguments are usually passed in registers, is to cause nameless
   arguments to be passed on the stack instead.  This is done by making
   `FUNCTION_ARG' return 0 whenever NAMED is 0.

   You may use the macro `MUST_PASS_IN_STACK (MODE, TYPE)' in the definition of
   this macro to determine if this argument is of a type that must be passed in
   the stack.  If `REG_PARM_STACK_SPACE' is not defined and `FUNCTION_ARG'
   returns non-zero for such an argument, the compiler will abort.  If
   `REG_PARM_STACK_SPACE' is defined, the argument will be computed in the
   stack and then loaded into a register.  */
     
#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED)			\
  (  (NAMED) == 0                    ? NULL_RTX			\
   : MUST_PASS_IN_STACK (MODE, TYPE) ? NULL_RTX			\
   : (CUM) >= FR30_NUM_ARG_REGS      ? NULL_RTX			\
   : gen_rtx (REG, MODE, CUM + FIRST_ARG_REGNUM))

/* A C type for declaring a variable that is used as the first argument of
   `FUNCTION_ARG' and other related values.  For some target machines, the type
   `int' suffices and can hold the number of bytes of argument so far.

   There is no need to record in `CUMULATIVE_ARGS' anything about the arguments
   that have been passed on the stack.  The compiler has other variables to
   keep track of that.  For target machines on which all arguments are passed
   on the stack, there is no need to store anything in `CUMULATIVE_ARGS';
   however, the data structure must exist and should not be empty, so use
   `int'.  */
/* On the FR30 this value is an accumulating count of the number of argument
   registers that have been filled with argument values, as opposed to say,
   the number of bytes of argument accumulated so far.  */
typedef int CUMULATIVE_ARGS;

/* A C expression for the number of words, at the beginning of an argument,
   must be put in registers.  The value must be zero for arguments that are
   passed entirely in registers or that are entirely pushed on the stack.

   On some machines, certain arguments must be passed partially in registers
   and partially in memory.  On these machines, typically the first N words of
   arguments are passed in registers, and the rest on the stack.  If a
   multi-word argument (a `double' or a structure) crosses that boundary, its
   first few words must be passed in registers and the rest must be pushed.
   This macro tells the compiler when this occurs, and how many of the words
   should go in registers.

   `FUNCTION_ARG' for these arguments should return the first register to be
   used by the caller for this argument; likewise `FUNCTION_INCOMING_ARG', for
   the called function.  */
#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) 	\
  fr30_function_arg_partial_nregs (CUM, MODE, TYPE, NAMED)

/* A C expression that indicates when an argument must be passed by reference.
   If nonzero for an argument, a copy of that argument is made in memory and a
   pointer to the argument is passed instead of the argument itself.  The
   pointer is passed in whatever way is appropriate for passing a pointer to
   that type.

   On machines where `REG_PARM_STACK_SPACE' is not defined, a suitable
   definition of this macro might be:
        #define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED)  \
          MUST_PASS_IN_STACK (MODE, TYPE)  */
#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED) \
  MUST_PASS_IN_STACK (MODE, TYPE)

/* A C statement (sans semicolon) for initializing the variable CUM for the
   state at the beginning of the argument list.  The variable has type
   `CUMULATIVE_ARGS'.  The value of FNTYPE is the tree node for the data type
   of the function which will receive the args, or 0 if the args are to a
   compiler support library function.  The value of INDIRECT is nonzero when
   processing an indirect call, for example a call through a function pointer.
   The value of INDIRECT is zero for a call to an explicitly named function, a
   library function call, or when `INIT_CUMULATIVE_ARGS' is used to find
   arguments for the function being compiled.

   When processing a call to a compiler support library function, LIBNAME
   identifies which one.  It is a `symbol_ref' rtx which contains the name of
   the function, as a string.  LIBNAME is 0 when an ordinary C function call is
   being processed.  Thus, each time this macro is called, either LIBNAME or
   FNTYPE is nonzero, but never both of them at once.  */
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT) (CUM) = 0

/* A C statement (sans semicolon) to update the summarizer variable CUM to
   advance past an argument in the argument list.  The values MODE, TYPE and
   NAMED describe that argument.  Once this is done, the variable CUM is
   suitable for analyzing the *following* argument with `FUNCTION_ARG', etc.

   This macro need not do anything if the argument in question was passed on
   the stack.  The compiler knows how to track the amount of stack space used
   for arguments without any special help.  */
#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)			\
  (CUM) += (NAMED) * fr30_num_arg_regs (MODE, TYPE)

/* A C expression that is nonzero if REGNO is the number of a hard register in
   which function arguments are sometimes passed.  This does *not* include
   implicit arguments such as the static chain and the structure-value address.
   On many machines, no registers can be used for this purpose since all
   function arguments are pushed on the stack.  */
#define FUNCTION_ARG_REGNO_P(REGNO) \
  ((REGNO) >= FIRST_ARG_REGNUM && ((REGNO) < FIRST_ARG_REGNUM + FR30_NUM_ARG_REGS))

/*}}}*/ 
/*{{{  How Scalar Function Values are Returned.  */ 

/* A C expression to create an RTX representing the place where a function
   returns a value of data type VALTYPE.  VALTYPE is a tree node representing a
   data type.  Write `TYPE_MODE (VALTYPE)' to get the machine mode used to
   represent that type.  On many machines, only the mode is relevant.
   (Actually, on most machines, scalar values are returned in the same place
   regardless of mode).

   If `PROMOTE_FUNCTION_RETURN' is defined, you must apply the same promotion
   rules specified in `PROMOTE_MODE' if VALTYPE is a scalar type.

   If the precise function being called is known, FUNC is a tree node
   (`FUNCTION_DECL') for it; otherwise, FUNC is a null pointer.  This makes it
   possible to use a different value-returning convention for specific
   functions when all their calls are known.

   `FUNCTION_VALUE' is not used for return vales with aggregate data types,
   because these are returned in another way.  See `STRUCT_VALUE_REGNUM' and
   related macros, below.  */
#define FUNCTION_VALUE(VALTYPE, FUNC) \
     gen_rtx_REG (TYPE_MODE (VALTYPE), RETURN_VALUE_REGNUM)

/* A C expression to create an RTX representing the place where a library
   function returns a value of mode MODE.  If the precise function being called
   is known, FUNC is a tree node (`FUNCTION_DECL') for it; otherwise, FUNC is a
   null pointer.  This makes it possible to use a different value-returning
   convention for specific functions when all their calls are known.

   Note that "library function" in this context means a compiler support
   routine, used to perform arithmetic, whose name is known specially by the
   compiler and was not mentioned in the C code being compiled.

   The definition of `LIBRARY_VALUE' need not be concerned aggregate data
   types, because none of the library functions returns such types.  */
#define LIBCALL_VALUE(MODE) gen_rtx (REG, MODE, RETURN_VALUE_REGNUM)

/* A C expression that is nonzero if REGNO is the number of a hard register in
   which the values of called function may come back. */

#define FUNCTION_VALUE_REGNO_P(REGNO) ((REGNO) == RETURN_VALUE_REGNUM)

/*}}}*/ 
/*{{{  How Large Values are Returned.  */ 

/* Define this macro to be 1 if all structure and union return values must be
   in memory.  Since this results in slower code, this should be defined only
   if needed for compatibility with other compilers or with an ABI.  If you
   define this macro to be 0, then the conventions used for structure and union
   return values are decided by the `RETURN_IN_MEMORY' macro.

   If not defined, this defaults to the value 1.  */
#define DEFAULT_PCC_STRUCT_RETURN 1

/* If the structure value address is not passed in a register, define
   `STRUCT_VALUE' as an expression returning an RTX for the place where the
   address is passed.  If it returns 0, the address is passed as an "invisible"
   first argument.  */
#define STRUCT_VALUE 0

/*}}}*/ 
/*{{{  Generating Code for Profiling.  */ 

/* A C statement or compound statement to output to FILE some assembler code to
   call the profiling subroutine `mcount'.  Before calling, the assembler code
   must load the address of a counter variable into a register where `mcount'
   expects to find the address.  The name of this variable is `LP' followed by
   the number LABELNO, so you would generate the name using `LP%d' in a
   `fprintf'.

   The details of how the address should be passed to `mcount' are determined
   by your operating system environment, not by GNU CC.  To figure them out,
   compile a small program for profiling using the system's installed C
   compiler and look at the assembler code that results.  */
#define FUNCTION_PROFILER(FILE, LABELNO)	\
{						\
  fprintf (FILE, "\t mov rp, r1\n" );		\
  fprintf (FILE, "\t ldi:32 mcount, r0\n" );	\
  fprintf (FILE, "\t call @r0\n" );		\
  fprintf (FILE, ".word\tLP%d\n", LABELNO);	\
}

/*}}}*/ 
/*{{{  Implementing the VARARGS Macros.  */ 

/* This macro offers an alternative to using `__builtin_saveregs' and defining
   the macro `EXPAND_BUILTIN_SAVEREGS'.  Use it to store the anonymous register
   arguments into the stack so that all the arguments appear to have been
   passed consecutively on the stack.  Once this is done, you can use the
   standard implementation of varargs that works for machines that pass all
   their arguments on the stack.

   The argument ARGS_SO_FAR is the `CUMULATIVE_ARGS' data structure, containing
   the values that obtain after processing of the named arguments.  The
   arguments MODE and TYPE describe the last named argument--its machine mode
   and its data type as a tree node.

   The macro implementation should do two things: first, push onto the stack
   all the argument registers *not* used for the named arguments, and second,
   store the size of the data thus pushed into the `int'-valued variable whose
   name is supplied as the argument PRETEND_ARGS_SIZE.  The value that you
   store here will serve as additional offset for setting up the stack frame.

   Because you must generate code to push the anonymous arguments at compile
   time without knowing their data types, `SETUP_INCOMING_VARARGS' is only
   useful on machines that have just a single category of argument register and
   use it uniformly for all data types.

   If the argument SECOND_TIME is nonzero, it means that the arguments of the
   function are being analyzed for the second time.  This happens for an inline
   function, which is not actually compiled until the end of the source file.
   The macro `SETUP_INCOMING_VARARGS' should not generate any instructions in
   this case.  */
#define SETUP_INCOMING_VARARGS(ARGS_SO_FAR, MODE, TYPE, PRETEND_ARGS_SIZE, SECOND_TIME) \
  if (! SECOND_TIME) \
    fr30_setup_incoming_varargs (ARGS_SO_FAR, MODE, TYPE, & PRETEND_ARGS_SIZE)

/* Define this macro if the location where a function argument is passed
   depends on whether or not it is a named argument.

   This macro controls how the NAMED argument to `FUNCTION_ARG' is set for
   varargs and stdarg functions.  With this macro defined, the NAMED argument
   is always true for named arguments, and false for unnamed arguments.  If
   this is not defined, but `SETUP_INCOMING_VARARGS' is defined, then all
   arguments are treated as named.  Otherwise, all named arguments except the
   last are treated as named.  */
#define STRICT_ARGUMENT_NAMING 0

/*}}}*/ 
/*{{{  Trampolines for Nested Functions.  */ 

/* On the FR30, the trampoline is:

   nop
   ldi:32 STATIC, r12
   nop
   ldi:32 FUNCTION, r0
   jmp    @r0

   The no-ops are to guarantee that the the static chain and final
   target are 32 bit ailgned within the trampoline.  That allows us to
   initialize those locations with simple SImode stores.   The alternative
   would be to use HImode stores.  */
   
/* A C statement to output, on the stream FILE, assembler code for a block of
   data that contains the constant parts of a trampoline.  This code should not
   include a label--the label is taken care of automatically.  */
#define TRAMPOLINE_TEMPLATE(FILE)						\
{										\
  fprintf (FILE, "\tnop\n");							\
  fprintf (FILE, "\tldi:32\t#0, %s\n", reg_names [STATIC_CHAIN_REGNUM]);	\
  fprintf (FILE, "\tnop\n");							\
  fprintf (FILE, "\tldi:32\t#0, %s\n", reg_names [COMPILER_SCRATCH_REGISTER]);	\
  fprintf (FILE, "\tjmp\t@%s\n", reg_names [COMPILER_SCRATCH_REGISTER]);	\
}

/* A C expression for the size in bytes of the trampoline, as an integer.  */
#define TRAMPOLINE_SIZE 18

/* We want the trampoline to be aligned on a 32bit boundary so that we can
   make sure the location of the static chain & target function within
   the trampoline is also aligned on a 32bit boundary.  */
#define TRAMPOLINE_ALIGNMENT 32

/* A C statement to initialize the variable parts of a trampoline.  ADDR is an
   RTX for the address of the trampoline; FNADDR is an RTX for the address of
   the nested function; STATIC_CHAIN is an RTX for the static chain value that
   should be passed to the function when it is called.  */
#define INITIALIZE_TRAMPOLINE(ADDR, FNADDR, STATIC_CHAIN)			\
do										\
{										\
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant (ADDR, 4)), STATIC_CHAIN);\
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant (ADDR, 12)), FNADDR);	\
} while (0);

/*}}}*/ 
/*{{{  Addressing Modes.  */ 

/* A C expression that is 1 if the RTX X is a constant which is a valid
   address.  On most machines, this can be defined as `CONSTANT_P (X)', but a
   few machines are more restrictive in which constant addresses are supported.

   `CONSTANT_P' accepts integer-values expressions whose values are not
   explicitly known, such as `symbol_ref', `label_ref', and `high' expressions
   and `const' arithmetic expressions, in addition to `const_int' and
   `const_double' expressions.  */
#define CONSTANT_ADDRESS_P(X) CONSTANT_P (X)

/* A number, the maximum number of registers that can appear in a valid memory
   address.  Note that it is up to you to specify a value equal to the maximum
   number that `GO_IF_LEGITIMATE_ADDRESS' would ever accept.  */
#define MAX_REGS_PER_ADDRESS 1

/* A C compound statement with a conditional `goto LABEL;' executed if X (an
   RTX) is a legitimate memory address on the target machine for a memory
   operand of mode MODE.

   It usually pays to define several simpler macros to serve as subroutines for
   this one.  Otherwise it may be too complicated to understand.

   This macro must exist in two variants: a strict variant and a non-strict
   one.  The strict variant is used in the reload pass.  It must be defined so
   that any pseudo-register that has not been allocated a hard register is
   considered a memory reference.  In contexts where some kind of register is
   required, a pseudo-register with no hard register must be rejected.

   The non-strict variant is used in other passes.  It must be defined to
   accept all pseudo-registers in every context where some kind of register is
   required.

   Compiler source files that want to use the strict variant of this macro
   define the macro `REG_OK_STRICT'.  You should use an `#ifdef REG_OK_STRICT'
   conditional to define the strict variant in that case and the non-strict
   variant otherwise.

   Subroutines to check for acceptable registers for various purposes (one for
   base registers, one for index registers, and so on) are typically among the
   subroutines used to define `GO_IF_LEGITIMATE_ADDRESS'.  Then only these
   subroutine macros need have two variants; the higher levels of macros may be
   the same whether strict or not.

   Normally, constant addresses which are the sum of a `symbol_ref' and an
   integer are stored inside a `const' RTX to mark them as constant.
   Therefore, there is no need to recognize such sums specifically as
   legitimate addresses.  Normally you would simply recognize any `const' as
   legitimate.

   Usually `PRINT_OPERAND_ADDRESS' is not prepared to handle constant sums that
   are not marked with `const'.  It assumes that a naked `plus' indicates
   indexing.  If so, then you *must* reject such naked constant sums as
   illegitimate addresses, so that none of them will be given to
   `PRINT_OPERAND_ADDRESS'.

   On some machines, whether a symbolic address is legitimate depends on the
   section that the address refers to.  On these machines, define the macro
   `ENCODE_SECTION_INFO' to store the information into the `symbol_ref', and
   then check for it here.  When you see a `const', you will have to look
   inside it to find the `symbol_ref' in order to determine the section.

   The best way to modify the name string is by adding text to the beginning,
   with suitable punctuation to prevent any ambiguity.  Allocate the new name
   in `saveable_obstack'.  You will have to modify `ASM_OUTPUT_LABELREF' to
   remove and decode the added text and output the name accordingly, and define
   `STRIP_NAME_ENCODING' to access the original name string.

   You can check the information stored here into the `symbol_ref' in the
   definitions of the macros `GO_IF_LEGITIMATE_ADDRESS' and
   `PRINT_OPERAND_ADDRESS'.

   Used in explow.c, recog.c, reload.c.  */

/* On the FR30 we only have one real addressing mode - an address in a
   register.  There are three special cases however:
   
   * indexed addressing using small positive offsets from the stack pointer
   
   * indexed addressing using small signed offsets from the frame pointer

   * register plus register addresing using R13 as the base register.

   At the moment we only support the first two of these special cases.  */
   
#ifdef REG_OK_STRICT
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, LABEL)			\
  do									\
    {									\
      if (GET_CODE (X) == REG && REG_OK_FOR_BASE_P (X))			\
        goto LABEL;							\
      if (GET_CODE (X) == PLUS						\
	  && ((MODE) == SImode || (MODE) == SFmode)			\
	  && XEXP (X, 0) == stack_pointer_rtx				\
	  && GET_CODE (XEXP (X, 1)) == CONST_INT			\
	  && IN_RANGE (INTVAL (XEXP (X, 1)), 0, (1 <<  6) - 4))		\
	goto LABEL;							\
      if (GET_CODE (X) == PLUS						\
	  && ((MODE) == SImode || (MODE) == SFmode)			\
	  && XEXP (X, 0) == frame_pointer_rtx				\
	  && GET_CODE (XEXP (X, 1)) == CONST_INT			\
	  && IN_RANGE (INTVAL (XEXP (X, 1)), -(1 << 9), (1 <<  9) - 4))	\
        goto LABEL;							\
    }									\
  while (0)
#else
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, LABEL)			\
  do									\
    {									\
      if (GET_CODE (X) == REG && REG_OK_FOR_BASE_P (X))			\
        goto LABEL;							\
      if (GET_CODE (X) == PLUS						\
	  && ((MODE) == SImode || (MODE) == SFmode)			\
	  && XEXP (X, 0) == stack_pointer_rtx				\
	  && GET_CODE (XEXP (X, 1)) == CONST_INT			\
	  && IN_RANGE (INTVAL (XEXP (X, 1)), 0, (1 <<  6) - 4))		\
	goto LABEL;							\
      if (GET_CODE (X) == PLUS						\
	  && ((MODE) == SImode || (MODE) == SFmode)			\
	  && (XEXP (X, 0) == frame_pointer_rtx				\
	      || XEXP(X,0) == arg_pointer_rtx)				\
	  && GET_CODE (XEXP (X, 1)) == CONST_INT			\
	  && IN_RANGE (INTVAL (XEXP (X, 1)), -(1 << 9), (1 <<  9) - 4))	\
        goto LABEL;							\
    }									\
  while (0)
#endif

/* A C expression that is nonzero if X (assumed to be a `reg' RTX) is valid for
   use as a base register.  For hard registers, it should always accept those
   which the hardware permits and reject the others.  Whether the macro accepts
   or rejects pseudo registers must be controlled by `REG_OK_STRICT' as
   described above.  This usually requires two variant definitions, of which
   `REG_OK_STRICT' controls the one actually used.  */
#ifdef REG_OK_STRICT
#define REG_OK_FOR_BASE_P(X) (((unsigned) REGNO (X)) <= STACK_POINTER_REGNUM)
#else
#define REG_OK_FOR_BASE_P(X) 1
#endif

/* A C expression that is nonzero if X (assumed to be a `reg' RTX) is valid for
   use as an index register.

   The difference between an index register and a base register is that the
   index register may be scaled.  If an address involves the sum of two
   registers, neither one of them scaled, then either one may be labeled the
   "base" and the other the "index"; but whichever labeling is used must fit
   the machine's constraints of which registers may serve in each capacity.
   The compiler will try both labelings, looking for one that is valid, and
   will reload one or both registers only if neither labeling works.  */
#define REG_OK_FOR_INDEX_P(X) REG_OK_FOR_BASE_P (X)

/* A C compound statement that attempts to replace X with a valid memory
   address for an operand of mode MODE.  WIN will be a C statement label
   elsewhere in the code; the macro definition may use

        GO_IF_LEGITIMATE_ADDRESS (MODE, X, WIN);

   to avoid further processing if the address has become legitimate.

   X will always be the result of a call to `break_out_memory_refs', and OLDX
   will be the operand that was given to that function to produce X.

   The code generated by this macro should not alter the substructure of X.  If
   it transforms X into a more legitimate form, it should assign X (which will
   always be a C variable) a new value.

   It is not necessary for this macro to come up with a legitimate address.
   The compiler has standard ways of doing so in all cases.  In fact, it is
   safe for this macro to do nothing.  But often a machine-dependent strategy
   can generate better code.  */
#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN)

/* A C statement or compound statement with a conditional `goto LABEL;'
   executed if memory address X (an RTX) can have different meanings depending
   on the machine mode of the memory reference it is used for or if the address
   is valid for some modes but not others.

   Autoincrement and autodecrement addresses typically have mode-dependent
   effects because the amount of the increment or decrement is the size of the
   operand being addressed.  Some machines have other mode-dependent addresses.
   Many RISC machines have no mode-dependent addresses.

   You may assume that ADDR is a valid address for the machine.  */
#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR, LABEL)

/* A C expression that is nonzero if X is a legitimate constant for an
   immediate operand on the target machine.  You can assume that X satisfies
   `CONSTANT_P', so you need not check this.  In fact, `1' is a suitable
   definition for this macro on machines where anything `CONSTANT_P' is valid.  */
#define LEGITIMATE_CONSTANT_P(X) 1

/*}}}*/ 
/*{{{  Describing Relative Costs of Operations */ 

/* Define this macro as a C expression which is nonzero if accessing less than
   a word of memory (i.e. a `char' or a `short') is no faster than accessing a
   word of memory, i.e., if such access require more than one instruction or if
   there is no difference in cost between byte and (aligned) word loads.

   When this macro is not defined, the compiler will access a field by finding
   the smallest containing object; when it is defined, a fullword load will be
   used if alignment permits.  Unless bytes accesses are faster than word
   accesses, using word accesses is preferable since it may eliminate
   subsequent memory access if subsequent accesses occur to other fields in the
   same word of the structure, but to different bytes.  */
#define SLOW_BYTE_ACCESS 1

/* Define this macro if zero-extension (of a `char' or `short' to an `int') can
   be done faster if the destination is a register that is known to be zero.

   If you define this macro, you must have instruction patterns that recognize
   RTL structures like this:

        (set (strict_low_part (subreg:QI (reg:SI ...) 0)) ...)

   and likewise for `HImode'.  */
#define SLOW_ZERO_EXTEND 0

/*}}}*/ 
/*{{{  Dividing the output into sections.  */ 

/* A C expression whose value is a string containing the assembler operation
   that should precede instructions and read-only data.  Normally `".text"' is
   right.  */
#define TEXT_SECTION_ASM_OP ".text"

/* A C expression whose value is a string containing the assembler operation to
   identify the following data as writable initialized data.  Normally
   `".data"' is right.  */
#define DATA_SECTION_ASM_OP ".data"

/* If defined, a C expression whose value is a string containing the
   assembler operation to identify the following data as
   uninitialized global data.  If not defined, and neither
   `ASM_OUTPUT_BSS' nor `ASM_OUTPUT_ALIGNED_BSS' are defined,
   uninitialized global data will be output in the data section if
   `-fno-common' is passed, otherwise `ASM_OUTPUT_COMMON' will be
   used.  */
#define BSS_SECTION_ASM_OP ".bss"

/*}}}*/ 
/*{{{  The Overall Framework of an Assembler File.  */

/* A C string constant describing how to begin a comment in the target
   assembler language.  The compiler assumes that the comment will end at the
   end of the line.  */
#define ASM_COMMENT_START ";"

/* A C string constant for text to be output before each `asm' statement or
   group of consecutive ones.  Normally this is `"#APP"', which is a comment
   that has no effect on most assemblers but tells the GNU assembler that it
   must check the lines that follow for all valid assembler constructs.  */
#define ASM_APP_ON "#APP\n"

/* A C string constant for text to be output after each `asm' statement or
   group of consecutive ones.  Normally this is `"#NO_APP"', which tells the
   GNU assembler to resume making the time-saving assumptions that are valid
   for ordinary compiler output.  */
#define ASM_APP_OFF "#NO_APP\n"

/*}}}*/ 
/*{{{  Output of Data.  */

/* This is how to output an assembler line defining a `float' constant.  */
#define ASM_OUTPUT_FLOAT(FILE, VALUE)			\
  do							\
    {							\
      long t;						\
      char str[30];					\
      							\
      REAL_VALUE_TO_TARGET_SINGLE ((VALUE), t);		\
      REAL_VALUE_TO_DECIMAL ((VALUE), "%.20e", str);	\
      							\
      fprintf (FILE, "\t.word\t0x%lx %s %s\n",		\
	       t, ASM_COMMENT_START, str);		\
    }							\
  while (0)

/* This is how to output an assembler line defining a `double' constant.  */
#define ASM_OUTPUT_DOUBLE(FILE, VALUE)				\
  do								\
    {								\
      long t[2];						\
      char str[30];						\
      								\
      REAL_VALUE_TO_TARGET_DOUBLE ((VALUE), t);			\
      REAL_VALUE_TO_DECIMAL ((VALUE), "%.20e", str);		\
      								\
      fprintf (FILE, "\t.word\t0x%lx %s %s\n\t.word\t0x%lx\n",	\
	       t[0], ASM_COMMENT_START, str, t[1]);		\
    }								\
  while (0)

/* This is how to output an assembler line defining a `char' constant.  */
#define ASM_OUTPUT_CHAR(FILE, VALUE)		\
  do						\
    {						\
      fprintf (FILE, "\t.byte\t");		\
      output_addr_const (FILE, (VALUE));	\
      fprintf (FILE, "\n");			\
    }						\
  while (0)

/* This is how to output an assembler line defining a `short' constant.  */
#define ASM_OUTPUT_SHORT(FILE, VALUE)		\
  do						\
    {						\
      fprintf (FILE, "\t.hword\t");		\
      output_addr_const (FILE, (VALUE));	\
      fprintf (FILE, "\n");			\
    }						\
  while (0)

/* This is how to output an assembler line defining an `int' constant.
   We also handle symbol output here.  */
#define ASM_OUTPUT_INT(FILE, VALUE)		\
  do						\
    {						\
      fprintf (FILE, "\t.word\t");		\
      output_addr_const (FILE, (VALUE));	\
      fprintf (FILE, "\n");			\
    }						\
  while (0)

/* A C statement to output to the stdio stream STREAM an assembler instruction
   to assemble a single byte containing the number VALUE.  */
#define ASM_OUTPUT_BYTE(STREAM, VALUE) \
  fprintf (STREAM, "\t%s\t0x%x\n", ASM_BYTE_OP, (VALUE))

/* These macros are defined as C string constant, describing the syntax in the
   assembler for grouping arithmetic expressions.  The following definitions
   are correct for most assemblers:

        #define ASM_OPEN_PAREN "("
        #define ASM_CLOSE_PAREN ")"  */
#define ASM_OPEN_PAREN "("
#define ASM_CLOSE_PAREN ")"

/*}}}*/ 
/*{{{  Output and Generation of Labels.  */ 

/* A C statement (sans semicolon) to output to the stdio stream STREAM the
   assembler definition of a label named NAME.  Use the expression
   `assemble_name (STREAM, NAME)' to output the name itself; before and after
   that, output the additional assembler syntax for defining the name, and a
   newline.  */
#define ASM_OUTPUT_LABEL(STREAM, NAME)	\
  do					\
    {					\
      assemble_name (STREAM, NAME);	\
      fputs (":\n", STREAM);		\
    }					\
  while (0)

/* A C statement (sans semicolon) to output to the stdio stream STREAM some
   commands that will make the label NAME global; that is, available for
   reference from other files.  Use the expression `assemble_name (STREAM,
   NAME)' to output the name itself; before and after that, output the
   additional assembler syntax for making that name global, and a newline.  */
#define ASM_GLOBALIZE_LABEL(STREAM,NAME)	\
  do						\
    {						\
      fputs ("\t.globl ", STREAM);		\
      assemble_name (STREAM, NAME);		\
      fputs ("\n", STREAM);			\
    }						\
  while (0)

/* A C expression to assign to OUTVAR (which is a variable of type `char *') a
   newly allocated string made from the string NAME and the number NUMBER, with
   some suitable punctuation added.  Use `alloca' to get space for the string.

   The string will be used as an argument to `ASM_OUTPUT_LABELREF' to produce
   an assembler label for an internal static variable whose name is NAME.
   Therefore, the string must be such as to result in valid assembler code.
   The argument NUMBER is different each time this macro is executed; it
   prevents conflicts between similarly-named internal static variables in
   different scopes.

   Ideally this string should not be a valid C identifier, to prevent any
   conflict with the user's own symbols.  Most assemblers allow periods or
   percent signs in assembler symbols; putting at least one of these between
   the name and the number will suffice.  */
#define ASM_FORMAT_PRIVATE_NAME(OUTVAR, NAME, NUMBER)		\
  do								\
    {								\
      (OUTVAR) = (char *) alloca (strlen ((NAME)) + 12);	\
      sprintf ((OUTVAR), "%s.%ld", (NAME), (long)(NUMBER));	\
    }								\
  while (0)

/*}}}*/ 
/*{{{  Output of Assembler Instructions.  */ 

/* A C compound statement to output to stdio stream STREAM the assembler syntax
   for an instruction operand X.  X is an RTL expression.

   CODE is a value that can be used to specify one of several ways of printing
   the operand.  It is used when identical operands must be printed differently
   depending on the context.  CODE comes from the `%' specification that was
   used to request printing of the operand.  If the specification was just
   `%DIGIT' then CODE is 0; if the specification was `%LTR DIGIT' then CODE is
   the ASCII code for LTR.

   If X is a register, this macro should print the register's name.  The names
   can be found in an array `reg_names' whose type is `char *[]'.  `reg_names'
   is initialized from `REGISTER_NAMES'.

   When the machine description has a specification `%PUNCT' (a `%' followed by
   a punctuation character), this macro is called with a null pointer for X and
   the punctuation character for CODE.  */
#define PRINT_OPERAND(STREAM, X, CODE)	fr30_print_operand (STREAM, X, CODE)

/* A C expression which evaluates to true if CODE is a valid punctuation
   character for use in the `PRINT_OPERAND' macro.  If
   `PRINT_OPERAND_PUNCT_VALID_P' is not defined, it means that no punctuation
   characters (except for the standard one, `%') are used in this way.  */
#define PRINT_OPERAND_PUNCT_VALID_P(CODE) (CODE == '#')

/* A C compound statement to output to stdio stream STREAM the assembler syntax
   for an instruction operand that is a memory reference whose address is X.  X
   is an RTL expression.

   On some machines, the syntax for a symbolic address depends on the section
   that the address refers to.  On these machines, define the macro
   `ENCODE_SECTION_INFO' to store the information into the `symbol_ref', and
   then check for it here.  *Note Assembler Format::.  */
#define PRINT_OPERAND_ADDRESS(STREAM, X) fr30_print_operand_address (STREAM, X)

/* If defined, C string expressions to be used for the `%R', `%L', `%U', and
   `%I' options of `asm_fprintf' (see `final.c').  These are useful when a
   single `md' file must support multiple assembler formats.  In that case, the
   various `tm.h' files can define these macros differently.

   USER_LABEL_PREFIX is defined in svr4.h.  */
#define REGISTER_PREFIX "%"
#define LOCAL_LABEL_PREFIX "."
#define USER_LABEL_PREFIX ""
#define IMMEDIATE_PREFIX ""

/*}}}*/ 
/*{{{  Output of Dispatch Tables.  */ 

/* This macro should be provided on machines where the addresses in a dispatch
   table are relative to the table's own address.

   The definition should be a C statement to output to the stdio stream STREAM
   an assembler pseudo-instruction to generate a difference between two labels.
   VALUE and REL are the numbers of two internal labels.  The definitions of
   these labels are output using `ASM_OUTPUT_INTERNAL_LABEL', and they must be
   printed in the same way here.  For example,

        fprintf (STREAM, "\t.word L%d-L%d\n", VALUE, REL)  */
#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL) \
fprintf (STREAM, "\t.word .L%d-.L%d\n", VALUE, REL)

/* This macro should be provided on machines where the addresses in a dispatch
   table are absolute.

   The definition should be a C statement to output to the stdio stream STREAM
   an assembler pseudo-instruction to generate a reference to a label.  VALUE
   is the number of an internal label whose definition is output using
   `ASM_OUTPUT_INTERNAL_LABEL'.  For example,

        fprintf (STREAM, "\t.word L%d\n", VALUE)  */
#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE) \
fprintf (STREAM, "\t.word .L%d\n", VALUE)

/*}}}*/ 
/*{{{  Assembler Commands for Alignment.  */ 

/* A C statement to output to the stdio stream STREAM an assembler command to
   advance the location counter to a multiple of 2 to the POWER bytes.  POWER
   will be a C expression of type `int'.  */
#define ASM_OUTPUT_ALIGN(STREAM, POWER) \
  fprintf ((STREAM), "\t.p2align %d\n", (POWER))

/*}}}*/ 
/*{{{  Macros Affecting all Debug Formats.  */ 

/* A C expression that returns the DBX register number for the compiler
   register number REGNO.  In simple cases, the value of this expression may be
   REGNO itself.  But sometimes there are some registers that the compiler
   knows about and DBX does not, or vice versa.  In such cases, some register
   may need to have one number in the compiler and another for DBX.

   If two registers have consecutive numbers inside GNU CC, and they can be
   used as a pair to hold a multiword value, then they *must* have consecutive
   numbers after renumbering with `DBX_REGISTER_NUMBER'.  Otherwise, debuggers
   will be unable to access such a pair, because they expect register pairs to
   be consecutive in their own numbering scheme.

   If you find yourself defining `DBX_REGISTER_NUMBER' in way that does not
   preserve register pairs, then what you must do instead is redefine the
   actual register numbering scheme.  */
#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/*}}}*/ 
/*{{{  Macros for SDB and Dwarf Output.  */ 

/* Define this macro to allow references to structure, union, or enumeration
   tags that have not yet been seen to be handled.  Some assemblers choke if
   forward tags are used, while some require it.  */
/* #define SDB_ALLOW_FORWARD_REFERENCES */

#define DWARF_LINE_MIN_INSTR_LENGTH 2
     
/*}}}*/ 
/*{{{  Miscellaneous Parameters.  */ 

/* An alias for a machine mode name.  This is the machine mode that elements of
   a jump-table should have.  */
#define CASE_VECTOR_MODE SImode

/* An alias for a tree code that is the easiest kind of division to compile
   code for in the general case.  It may be `TRUNC_DIV_EXPR', `FLOOR_DIV_EXPR',
   `CEIL_DIV_EXPR' or `ROUND_DIV_EXPR'.  These four division operators differ
   in how they round the result to an integer.  `EASY_DIV_EXPR' is used when it
   is permissible to use any of those kinds of division and the choice should
   be made on the basis of efficiency.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* The maximum number of bytes that a single instruction can move quickly from
   memory to memory.  */
#define MOVE_MAX 8

/* A C expression which is nonzero if on this machine it is safe to "convert"
   an integer of INPREC bits to one of OUTPREC bits (where OUTPREC is smaller
   than INPREC) by merely operating on it as if it had only OUTPREC bits.

   On many machines, this expression can be 1.

   When `TRULY_NOOP_TRUNCATION' returns 1 for a pair of sizes for modes for
   which `MODES_TIEABLE_P' is 0, suboptimal code can result.  If this is the
   case, making `TRULY_NOOP_TRUNCATION' return 0 in such cases may improve
   things.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* An alias for the machine mode for pointers.  On most machines, define this
   to be the integer mode corresponding to the width of a hardware pointer;
   `SImode' on 32-bit machine or `DImode' on 64-bit machines.  On some machines
   you must define this to be one of the partial integer modes, such as
   `PSImode'.

   The width of `Pmode' must be at least as large as the value of
   `POINTER_SIZE'.  If it is not equal, you must define the macro
   `POINTERS_EXTEND_UNSIGNED' to specify how pointers are extended to `Pmode'.  */
#define Pmode SImode

/* An alias for the machine mode used for memory references to functions being
   called, in `call' RTL expressions.  On most machines this should be
   `QImode'.  */
#define FUNCTION_MODE QImode

/* If cross-compiling, don't require stdio.h etc to build libgcc.a.  */
#if defined CROSS_COMPILE && ! defined inhibit_libc
#define inhibit_libc
#endif

/*}}}*/ 
/*{{{  Exported variables */ 

/* Define the information needed to generate branch and scc insns.  This is
   stored from the compare operation.  Note that we can't use "rtx" here
   since it hasn't been defined!  */

extern struct rtx_def * fr30_compare_op0;
extern struct rtx_def * fr30_compare_op1;

/*}}}*/ 
/*{{{  PERDICATE_CODES.  */ 

#define PREDICATE_CODES					\
  { "stack_add_operand",	{ CONST_INT }},		\
  { "high_register_operand",	{ REG }},		\
  { "low_register_operand",	{ REG }},		\
  { "call_operand",		{ MEM }},		\
  { "fp_displacement_operand",	{ CONST_INT }},		\
  { "sp_displacement_operand",	{ CONST_INT }},		\
  { "add_immediate_operand",	{ REG, CONST_INT }},

/*}}}*/ 

/* Local Variables: */
/* folded-file: t   */
/* End:		    */
