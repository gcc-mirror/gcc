/* Xstormy16 cpu description.
   Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002
   Free Software Foundation, Inc.
   Contributed by Red Hat, Inc.

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


/* Driver configuration */

/* Defined in svr4.h.  */
/* #define SWITCH_TAKES_ARG(CHAR) */

/* Defined in svr4.h.  */
/* #define WORD_SWITCH_TAKES_ARG(NAME) */

/* Defined in svr4.h.  */
#undef ASM_SPEC
#define ASM_SPEC ""

/* Defined in svr4.h.  */
/* #define ASM_FINAL_SPEC "" */

/* Defined in svr4.h.  */
/* #define LINK_SPEC "" */

/* For xstormy16:
   - If -msim is specified, everything is built and linked as for the sim.
   - If -T is specified, that linker script is used, and it should provide
     appropriate libraries.
   - If neither is specified, everything is built as for the sim, but no
     I/O support is assumed.

*/
#undef LIB_SPEC
#define LIB_SPEC "-( -lc %{msim:-lsim}%{!msim:%{!T*:-lnosys}} -)"

/* Defined in svr4.h.  */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC "crt0.o%s crti.o%s crtbegin.o%s"

/* Defined in svr4.h.  */
#undef ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s crtn.o%s"

/* Defined in svr4.h for host compilers.  */
/* #define MD_EXEC_PREFIX "" */

/* Defined in svr4.h for host compilers.  */
/* #define MD_STARTFILE_PREFIX "" */


/* Run-time target specifications */

#define CPP_PREDEFINES "-Dxstormy16 -Amachine=xstormy16 -D__INT_MAX__=32767"

/* This declaration should be present.  */
extern int target_flags;

#define TARGET_SWITCHES					\
  {{ "sim", 0, "Provide libraries for the simulator" },	\
   { "", 0, "" }}

#define TARGET_VERSION fprintf (stderr, " (xstormy16 cpu core)");

#define CAN_DEBUG_WITHOUT_FP


/* Storage Layout */

#define BITS_BIG_ENDIAN 1

#define BYTES_BIG_ENDIAN 0

#define WORDS_BIG_ENDIAN 0

#define BITS_PER_UNIT 8

#define BITS_PER_WORD 16

#define UNITS_PER_WORD 2

#define POINTER_SIZE 16

#define PROMOTE_MODE(MODE,UNSIGNEDP,TYPE)				\
do {									\
  if (GET_MODE_CLASS (MODE) == MODE_INT					\
      && GET_MODE_SIZE (MODE) < 2)					\
    (MODE) = HImode;							\
} while (0)

#define PROMOTE_FUNCTION_ARGS 1

#define PROMOTE_FUNCTION_RETURN 1

#define PARM_BOUNDARY 16

#define STACK_BOUNDARY 16

#define FUNCTION_BOUNDARY 16

#define BIGGEST_ALIGNMENT 16

/* Defined in svr4.h.  */
/* #define MAX_OFILE_ALIGNMENT */

#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  (TREE_CODE (EXP) == STRING_CST	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

#define STRICT_ALIGNMENT 1

/* Defined in svr4.h.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

#define TARGET_FLOAT_FORMAT IEEE_FLOAT_FORMAT


/* Layout of Source Language Data Types */

#define INT_TYPE_SIZE 16

#define SHORT_TYPE_SIZE 16

#define LONG_TYPE_SIZE 32

#define LONG_LONG_TYPE_SIZE 64

#define CHAR_TYPE_SIZE 8

#define FLOAT_TYPE_SIZE 32

#define DOUBLE_TYPE_SIZE 64

#define LONG_DOUBLE_TYPE_SIZE 64

#define DEFAULT_SIGNED_CHAR 0

/* Defined in svr4.h.  */
#define SIZE_TYPE "unsigned int"

/* Defined in svr4.h.  */
#define PTRDIFF_TYPE "int"

/* Defined in svr4.h, to "long int".  */
/* #define WCHAR_TYPE "long int" */

/* Defined in svr4.h.  */
#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

/* Define this macro if the type of Objective C selectors should be `int'.

   If this macro is not defined, then selectors should have the type `struct
   objc_selector *'.  */
/* #define OBJC_INT_SELECTORS */


/* Register Basics */

/* Number of hardware registers known to the compiler.  They receive numbers 0
   through `FIRST_PSEUDO_REGISTER-1'; thus, the first pseudo register's number
   really is assigned the number `FIRST_PSEUDO_REGISTER'.  */
#define FIRST_PSEUDO_REGISTER 19

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
#define FIXED_REGISTERS \
  { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1 }

/* Like `FIXED_REGISTERS' but has 1 for each register that is clobbered (in
   general) by function calls as well as for fixed registers.  This macro
   therefore identifies the registers that are not available for general
   allocation of values that must live across function calls.

   If a register has 0 in `CALL_USED_REGISTERS', the compiler automatically
   saves it on function entry and restores it on function exit, if the register
   is used within the function.  */
#define CALL_USED_REGISTERS \
  { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1 }

/* Zero or more C statements that may conditionally modify two variables
   `fixed_regs' and `call_used_regs' (both of type `char []') after they have
   been initialized from the two preceding macros.

   This is necessary in case the fixed or call-clobbered registers depend on
   target flags.

   You need not define this macro if it has no work to do.

   If the usage of an entire class of registers depends on the target flags,
   you may indicate this to GCC by using this macro to modify `fixed_regs' and
   `call_used_regs' to 1 for each of the registers in the classes which should
   not be used by GCC.  Also define the macro `REG_CLASS_FROM_LETTER' to return
   `NO_REGS' if it is called with a letter for a class that shouldn't be used.

   (However, if this class is not included in `GENERAL_REGS' and all of the
   insn patterns whose constraints permit this class are controlled by target
   switches, then GCC will automatically avoid using these registers when the
   target switches are opposed to them.)  */
/* #define CONDITIONAL_REGISTER_USAGE */

/* If this macro is defined and has a nonzero value, it means that `setjmp' and
   related functions fail to save the registers, or that `longjmp' fails to
   restore them.  To compensate, the compiler avoids putting variables in
   registers in functions that use `setjmp'.  */
/* #define NON_SAVING_SETJMP */

/* Define this macro if the target machine has register windows.  This C
   expression returns the register number as seen by the called function
   corresponding to the register number OUT as seen by the calling function.
   Return OUT if register number OUT is not an outbound register.  */
/* #define INCOMING_REGNO(OUT) */

/* Define this macro if the target machine has register windows.  This C
   expression returns the register number as seen by the calling function
   corresponding to the register number IN as seen by the called function.
   Return IN if register number IN is not an inbound register.  */
/* #define OUTGOING_REGNO(IN) */


/* Order of allocation of registers */

/* If defined, an initializer for a vector of integers, containing the numbers
   of hard registers in the order in which GNU CC should prefer to use them
   (from most preferred to least).

   If this macro is not defined, registers are used lowest numbered first (all
   else being equal).

   One use of this macro is on machines where the highest numbered registers
   must always be saved and the save-multiple-registers instruction supports
   only sequences of consecutive registers.  On such machines, define
   `REG_ALLOC_ORDER' to be an initializer that lists the highest numbered
   allocatable register first.  */
#define REG_ALLOC_ORDER { 7, 6, 5, 4, 3, 2, 1, 0, 9, 8, 10, 11, 12, 13, 14, 15, 16 }

/* A C statement (sans semicolon) to choose the order in which to allocate hard
   registers for pseudo-registers local to a basic block.

   Store the desired register order in the array `reg_alloc_order'.  Element 0
   should be the register to allocate first; element 1, the next register; and
   so on.

   The macro body should not assume anything about the contents of
   `reg_alloc_order' before execution of the macro.

   On most machines, it is not necessary to define this macro.  */
/* #define ORDER_REGS_FOR_LOCAL_ALLOC */


/* How Values Fit in Registers */

/* A C expression for the number of consecutive hard registers, starting at
   register number REGNO, required to hold a value of mode MODE.

   On a machine where all registers are exactly one word, a suitable definition
   of this macro is

        #define HARD_REGNO_NREGS(REGNO, MODE)            \
           ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1)  \
            / UNITS_PER_WORD))  */
#define HARD_REGNO_NREGS(REGNO, MODE) 				\
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* A C expression that is nonzero if it is permissible to store a value of mode
   MODE in hard register number REGNO (or in several registers starting with
   that one).  For a machine where all registers are equivalent, a suitable
   definition is

        #define HARD_REGNO_MODE_OK(REGNO, MODE) 1

   It is not necessary for this macro to check for the numbers of fixed
   registers, because the allocation mechanism considers them to be always
   occupied.

   On some machines, double-precision values must be kept in even/odd register
   pairs.  The way to implement that is to define this macro to reject odd
   register numbers for such modes.

   The minimum requirement for a mode to be OK in a register is that the
   `movMODE' instruction pattern support moves between the register and any
   other hard register for which the mode is OK; and that moving a value into
   the register and back out not alter it.

   Since the same instruction used to move `SImode' will work for all narrower
   integer modes, it is not necessary on any machine for `HARD_REGNO_MODE_OK'
   to distinguish between these modes, provided you define patterns `movhi',
   etc., to take advantage of this.  This is useful because of the interaction
   between `HARD_REGNO_MODE_OK' and `MODES_TIEABLE_P'; it is very desirable for
   all integer modes to be tieable.

   Many machines have special registers for floating point arithmetic.  Often
   people assume that floating point machine modes are allowed only in floating
   point registers.  This is not true.  Any registers that can hold integers
   can safely *hold* a floating point machine mode, whether or not floating
   arithmetic can be done on it in those registers.  Integer move instructions
   can be used to move the values.

   On some machines, though, the converse is true: fixed-point machine modes
   may not go in floating registers.  This is true if the floating registers
   normalize any value stored in them, because storing a non-floating value
   there would garble it.  In this case, `HARD_REGNO_MODE_OK' should reject
   fixed-point machine modes in floating registers.  But if the floating
   registers do not automatically normalize, if you can store any bit pattern
   in one and retrieve it unchanged without a trap, then any machine mode may
   go in a floating register, so you can define this macro to say so.

   The primary significance of special floating registers is rather that they
   are the registers acceptable in floating point arithmetic instructions.
   However, this is of no concern to `HARD_REGNO_MODE_OK'.  You handle it by
   writing the proper constraints for those instructions.

   On some machines, the floating registers are especially slow to access, so
   that it is better to store a value in a stack frame than in such a register
   if floating point arithmetic is not being done.  As long as the floating
   registers are not in class `GENERAL_REGS', they will not be used unless some
   pattern's constraint asks for one.  */
#define HARD_REGNO_MODE_OK(REGNO, MODE) ((REGNO) != 16 || (MODE) == BImode)

/* A C expression that is nonzero if it is desirable to choose register
   allocation so as to avoid move instructions between a value of mode MODE1
   and a value of mode MODE2.

   If `HARD_REGNO_MODE_OK (R, MODE1)' and `HARD_REGNO_MODE_OK (R, MODE2)' are
   ever different for any R, then `MODES_TIEABLE_P (MODE1, MODE2)' must be
   zero.  */
#define MODES_TIEABLE_P(MODE1, MODE2) ((MODE1) != BImode && (MODE2) != BImode)

/* Define this macro if the compiler should avoid copies to/from CCmode
   registers.  You should only define this macro if support fo copying to/from
   CCmode is incomplete.  */
/* #define AVOID_CCMODE_COPIES */


/* Handling Leaf Functions */

/* A C initializer for a vector, indexed by hard register number, which
   contains 1 for a register that is allowable in a candidate for leaf function
   treatment.

   If leaf function treatment involves renumbering the registers, then the
   registers marked here should be the ones before renumbering--those that GNU
   CC would ordinarily allocate.  The registers which will actually be used in
   the assembler code, after renumbering, should not be marked with 1 in this
   vector.

   Define this macro only if the target machine offers a way to optimize the
   treatment of leaf functions.  */
/* #define LEAF_REGISTERS */

/* A C expression whose value is the register number to which REGNO should be
   renumbered, when a function is treated as a leaf function.

   If REGNO is a register number which should not appear in a leaf function
   before renumbering, then the expression should yield -1, which will cause
   the compiler to abort.

   Define this macro only if the target machine offers a way to optimize the
   treatment of leaf functions, and registers need to be renumbered to do this.  */
/* #define LEAF_REG_REMAP(REGNO) */


/* Registers That Form a Stack.  */

/* Define this if the machine has any stack-like registers.  */
/* #define STACK_REGS */

/* The number of the first stack-like register.  This one is the top
   of the stack.  */
/* #define FIRST_STACK_REG */

/* The number of the last stack-like register.  This one is the
   bottom of the stack.  */
/* #define LAST_STACK_REG */


/* Register Classes */

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
  R0_REGS,
  R1_REGS,
  TWO_REGS,
  R2_REGS,
  EIGHT_REGS,
  R8_REGS,
  ICALL_REGS,
  GENERAL_REGS,
  CARRY_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

/* The number of distinct register classes, defined as follows:

        #define N_REG_CLASSES (int) LIM_REG_CLASSES  */
#define N_REG_CLASSES ((int) LIM_REG_CLASSES)

/* An initializer containing the names of the register classes as C string
   constants.  These names are used in writing some of the debugging dumps.  */
#define REG_CLASS_NAMES				\
{						\
  "NO_REGS", 					\
  "R0_REGS", 					\
  "R1_REGS",					\
  "TWO_REGS",					\
  "R2_REGS",					\
  "EIGHT_REGS",					\
  "R8_REGS",					\
  "ICALL_REGS",					\
  "GENERAL_REGS",				\
  "CARRY_REGS",					\
  "ALL_REGS"					\
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
#define REG_CLASS_CONTENTS			\
{						\
  { 0x00000 },					\
  { 0x00001 },					\
  { 0x00002 },					\
  { 0x00003 },					\
  { 0x00004 },					\
  { 0x000FF },					\
  { 0x00100 },					\
  { 0x00300 },					\
  { 0x6FFFF },					\
  { 0x10000 },					\
  { (1 << FIRST_PSEUDO_REGISTER) - 1 }		\
}

/* A C expression whose value is a register class containing hard register
   REGNO.  In general there is more than one such class; choose a class which
   is "minimal", meaning that no smaller class also contains the register.  */
#define REGNO_REG_CLASS(REGNO) 			\
  ((REGNO) == 0   ? R0_REGS			\
   : (REGNO) == 1 ? R1_REGS			\
   : (REGNO) == 2 ? R2_REGS			\
   : (REGNO) < 8  ? EIGHT_REGS			\
   : (REGNO) == 8 ? R8_REGS			\
   : (REGNO) == 16 ? CARRY_REGS			\
   : (REGNO) <= 18 ? GENERAL_REGS		\
   : ALL_REGS)

/* A macro whose definition is the name of the class to which a valid base
   register must belong.  A base register is one used in an address which is
   the register value plus a displacement.  */
#define BASE_REG_CLASS GENERAL_REGS

/* A macro whose definition is the name of the class to which a valid index
   register must belong.  An index register is one used in an address where its
   value is either multiplied by a scale factor or added to another register
   (as well as added to a displacement).  */
#define INDEX_REG_CLASS GENERAL_REGS

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

#define REG_CLASS_FROM_LETTER(CHAR)		\
 (  (CHAR) == 'a' ? R0_REGS			\
  : (CHAR) == 'b' ? R1_REGS			\
  : (CHAR) == 'c' ? R2_REGS			\
  : (CHAR) == 'd' ? R8_REGS			\
  : (CHAR) == 'e' ? EIGHT_REGS			\
  : (CHAR) == 't' ? TWO_REGS			\
  : (CHAR) == 'y' ? CARRY_REGS			\
  : (CHAR) == 'z' ? ICALL_REGS			\
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
#define REGNO_OK_FOR_INDEX_P(NUM) REGNO_OK_FOR_BASE_P (NUM)

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
   floating values cannot be loaded into certain kinds of registers.

   This declaration must be present.  */
#define PREFERRED_RELOAD_CLASS(X, CLASS) \
  xstormy16_preferred_reload_class (X, CLASS)

/* Like `PREFERRED_RELOAD_CLASS', but for output reloads instead of input
   reloads.  If you don't define this macro, the default is to use CLASS,
   unchanged.  */
#define PREFERRED_OUTPUT_RELOAD_CLASS(X, CLASS) \
  xstormy16_preferred_reload_class (X, CLASS)

/* A C expression that places additional restrictions on the register class to
   use when it is necessary to be able to hold a value of mode MODE in a reload
   register for which class CLASS would ordinarily be used.

   Unlike `PREFERRED_RELOAD_CLASS', this macro should be used when there are
   certain modes that simply can't go in certain reload classes.

   The value is a register class; perhaps CLASS, or perhaps another, smaller
   class.

   Don't define this macro unless the target machine has limitations which
   require the macro to do something nontrivial.  */
/* #define LIMIT_RELOAD_CLASS(MODE, CLASS) */

/* Many machines have some registers that cannot be copied directly to or from
   memory or even from other types of registers.  An example is the `MQ'
   register, which on most machines, can only be copied to or from general
   registers, but not memory.  Some machines allow copying all registers to and
   from memory, but require a scratch register for stores to some memory
   locations (e.g., those with symbolic address on the RT, and those with
   certain symbolic address on the Sparc when compiling PIC).  In some cases,
   both an intermediate and a scratch register are required.

   You should define these macros to indicate to the reload phase that it may
   need to allocate at least one register for a reload in addition to the
   register to contain the data.  Specifically, if copying X to a register
   CLASS in MODE requires an intermediate register, you should define
   `SECONDARY_INPUT_RELOAD_CLASS' to return the largest register class all of
   whose registers can be used as intermediate registers or scratch registers.

   If copying a register CLASS in MODE to X requires an intermediate or scratch
   register, `SECONDARY_OUTPUT_RELOAD_CLASS' should be defined to return the
   largest register class required.  If the requirements for input and output
   reloads are the same, the macro `SECONDARY_RELOAD_CLASS' should be used
   instead of defining both macros identically.

   The values returned by these macros are often `GENERAL_REGS'.  Return
   `NO_REGS' if no spare register is needed; i.e., if X can be directly copied
   to or from a register of CLASS in MODE without requiring a scratch register.
   Do not define this macro if it would always return `NO_REGS'.

   If a scratch register is required (either with or without an intermediate
   register), you should define patterns for `reload_inM' or `reload_outM', as
   required..  These patterns, which will normally be implemented with a
   `define_expand', should be similar to the `movM' patterns, except that
   operand 2 is the scratch register.

   Define constraints for the reload register and scratch register that contain
   a single register class.  If the original reload register (whose class is
   CLASS) can meet the constraint given in the pattern, the value returned by
   these macros is used for the class of the scratch register.  Otherwise, two
   additional reload registers are required.  Their classes are obtained from
   the constraints in the insn pattern.

   X might be a pseudo-register or a `subreg' of a pseudo-register, which could
   either be in a hard register or in memory.  Use `true_regnum' to find out;
   it will return -1 if the pseudo is in memory and the hard register number if
   it is in a register.

   These macros should not be used in the case where a particular class of
   registers can only be copied to memory and not to another class of
   registers.  In that case, secondary reload registers are not needed and
   would not be helpful.  Instead, a stack location must be used to perform the
   copy and the `movM' pattern should use memory as an intermediate storage.
   This case often occurs between floating-point and general registers.  */

/* This chip has the interesting property that only the first eight
   registers can be moved to/from memory.  */
#define SECONDARY_RELOAD_CLASS(CLASS, MODE, X)			\
  xstormy16_secondary_reload_class (CLASS, MODE, X)

/* #define SECONDARY_INPUT_RELOAD_CLASS(CLASS, MODE, X) */
/* #define SECONDARY_OUTPUT_RELOAD_CLASS(CLASS, MODE, X) */

/* Certain machines have the property that some registers cannot be copied to
   some other registers without using memory.  Define this macro on those
   machines to be a C expression that is non-zero if objects of mode M in
   registers of CLASS1 can only be copied to registers of class CLASS2 by
   storing a register of CLASS1 into memory and loading that memory location
   into a register of CLASS2.

   Do not define this macro if its value would always be zero.  */
/* #define SECONDARY_MEMORY_NEEDED(CLASS1, CLASS2, M) */

/* Normally when `SECONDARY_MEMORY_NEEDED' is defined, the compiler allocates a
   stack slot for a memory location needed for register copies.  If this macro
   is defined, the compiler instead uses the memory location defined by this
   macro.

   Do not define this macro if you do not define
   `SECONDARY_MEMORY_NEEDED'.  */
/* #define SECONDARY_MEMORY_NEEDED_RTX(MODE) */

/* When the compiler needs a secondary memory location to copy between two
   registers of mode MODE, it normally allocates sufficient memory to hold a
   quantity of `BITS_PER_WORD' bits and performs the store and load operations
   in a mode that many bits wide and whose class is the same as that of MODE.

   This is right thing to do on most machines because it ensures that all bits
   of the register are copied and prevents accesses to the registers in a
   narrower mode, which some machines prohibit for floating-point registers.

   However, this default behavior is not correct on some machines, such as the
   DEC Alpha, that store short integers in floating-point registers differently
   than in integer registers.  On those machines, the default widening will not
   work correctly and you must define this macro to suppress that widening in
   some cases.  See the file `alpha.h' for details.

   Do not define this macro if you do not define `SECONDARY_MEMORY_NEEDED' or
   if widening MODE to a mode that is `BITS_PER_WORD' bits wide is correct for
   your machine.  */
/* #define SECONDARY_MEMORY_NEEDED_MODE(MODE) */

/* Normally the compiler avoids choosing registers that have been explicitly
   mentioned in the rtl as spill registers (these registers are normally those
   used to pass parameters and return values).  However, some machines have so
   few registers of certain classes that there would not be enough registers to
   use as spill registers if this were done.

   Define `SMALL_REGISTER_CLASSES' to be an expression with a non-zero value on
   these machines.  When this macro has a non-zero value, the compiler allows
   registers explicitly used in the rtl to be used as spill registers but
   avoids extending the lifetime of these registers.

   It is always safe to define this macro with a non-zero value, but if you
   unnecessarily define it, you will reduce the amount of optimizations that
   can be performed in some cases.  If you do not define this macro with a
   non-zero value when it is required, the compiler will run out of spill
   registers and print a fatal error message.  For most machines, you should
   not define this macro at all.  */
/* #define SMALL_REGISTER_CLASSES */

/* A C expression whose value is nonzero if pseudos that have been assigned to
   registers of class CLASS would likely be spilled because registers of CLASS
   are needed for spill registers.

   The default value of this macro returns 1 if CLASS has exactly one register
   and zero otherwise.  On most machines, this default should be used.  Only
   define this macro to some other expression if pseudo allocated by
   `local-alloc.c' end up in memory because their hard registers were needed
   for spill registers.  If this macro returns nonzero for those classes, those
   pseudos will only be allocated by `global.c', which knows how to reallocate
   the pseudo to another register.  If there would not be another register
   available for reallocation, you should not change the definition of this
   macro since the only effect of such a definition would be to slow down
   register allocation.  */
/* #define CLASS_LIKELY_SPILLED_P(CLASS) */

/* A C expression for the maximum number of consecutive registers of
   class CLASS needed to hold a value of mode MODE.

   This is closely related to the macro `HARD_REGNO_NREGS'.  In fact, the value
   of the macro `CLASS_MAX_NREGS (CLASS, MODE)' should be the maximum value of
   `HARD_REGNO_NREGS (REGNO, MODE)' for all REGNO values in the class CLASS.

   This macro helps control the handling of multiple-word values in
   the reload pass.

   This declaration is required.  */
#define CLASS_MAX_NREGS(CLASS, MODE) \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* If defined, a C expression for a class that contains registers which the
   compiler must always access in a mode that is the same size as the mode in
   which it loaded the register.

   For the example, loading 32-bit integer or floating-point objects into
   floating-point registers on the Alpha extends them to 64-bits.  Therefore
   loading a 64-bit object and then storing it as a 32-bit object does not
   store the low-order 32-bits, as would be the case for a normal register.
   Therefore, `alpha.h' defines this macro as `FLOAT_REGS'.  */
/* #define CLASS_CANNOT_CHANGE_SIZE */

/* A C expression that defines the machine-dependent operand constraint letters
   (`I', `J', `K', .. 'P') that specify particular ranges of integer values.
   If C is one of those letters, the expression should check that VALUE, an
   integer, is in the appropriate range and return 1 if so, 0 otherwise.  If C
   is not one of those letters, the value should be 0 regardless of VALUE.  */
#define CONST_OK_FOR_LETTER_P(VALUE, C)			\
  (  (C) == 'I' ? (VALUE) >= 0 && (VALUE) <= 3		\
   : (C) == 'J' ? exact_log2 (VALUE) != -1		\
   : (C) == 'K' ? exact_log2 (~(VALUE)) != -1		\
   : (C) == 'L' ? (VALUE) >= 0 && (VALUE) <= 255	\
   : (C) == 'M' ? (VALUE) >= -255 && (VALUE) <= 0	\
   : (C) == 'N' ? (VALUE) >= -3 && (VALUE) <= 0		\
   : (C) == 'O' ? (VALUE) >= 1 && (VALUE) <= 4		\
   : (C) == 'P' ? (VALUE) >= -4 && (VALUE) <= -1	\
   : 0 )

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
  xstormy16_extra_constraint_p (VALUE, C)


/* Basic Stack Layout */

/* Define this macro if pushing a word onto the stack moves the stack pointer
   to a smaller address.

   When we say, "define this macro if ...," it means that the compiler checks
   this macro only with `#ifdef' so the precise definition used does not
   matter.  */
/* #define STACK_GROWS_DOWNWARD */

/* We want to use post-increment instructions to push things on the stack,
   because we don't have any pre-increment ones.  */
#define STACK_PUSH_CODE POST_INC

/* Define this macro if the addresses of local variable slots are at negative
   offsets from the frame pointer.  */
/* #define FRAME_GROWS_DOWNWARD */

/* Define this macro if successive arguments to a function occupy decreasing
   addresses on the stack.  */
#define ARGS_GROW_DOWNWARD 1

/* Offset from the frame pointer to the first local variable slot to be
   allocated.

   If `FRAME_GROWS_DOWNWARD', find the next slot's offset by
   subtracting the first slot's length from `STARTING_FRAME_OFFSET'.
   Otherwise, it is found by adding the length of the first slot to
   the value `STARTING_FRAME_OFFSET'.  */
#define STARTING_FRAME_OFFSET 0

/* Offset from the stack pointer register to the first location at which
   outgoing arguments are placed.  If not specified, the default value of zero
   is used.  This is the proper value for most machines.

   If `ARGS_GROW_DOWNWARD', this is the offset to the location above the first
   location at which outgoing arguments are placed.  */
/* #define STACK_POINTER_OFFSET */

/* Offset from the argument pointer register to the first argument's address.
   On some machines it may depend on the data type of the function.

   If `ARGS_GROW_DOWNWARD', this is the offset to the location above the first
   argument's address.  */
#define FIRST_PARM_OFFSET(FUNDECL) 0

/* Offset from the stack pointer register to an item dynamically allocated on
   the stack, e.g., by `alloca'.

   The default value for this macro is `STACK_POINTER_OFFSET' plus the length
   of the outgoing arguments.  The default is correct for most machines.  See
   `function.c' for details.  */
/* #define STACK_DYNAMIC_OFFSET(FUNDECL) */

/* A C expression whose value is RTL representing the address in a stack frame
   where the pointer to the caller's frame is stored.  Assume that FRAMEADDR is
   an RTL expression for the address of the stack frame itself.

   If you don't define this macro, the default is to return the value of
   FRAMEADDR--that is, the stack frame address is also the address of the stack
   word that points to the previous frame.  */
/* #define DYNAMIC_CHAIN_ADDRESS(FRAMEADDR) */

/* If defined, a C expression that produces the machine-specific code to setup
   the stack so that arbitrary frames can be accessed.  For example, on the
   Sparc, we must flush all of the register windows to the stack before we can
   access arbitrary stack frames.  This macro will seldom need to be defined.  */
/* #define SETUP_FRAME_ADDRESSES() */

/* A C expression whose value is RTL representing the value of the return
   address for the frame COUNT steps up from the current frame, after the
   prologue.  FRAMEADDR is the frame pointer of the COUNT frame, or the frame
   pointer of the COUNT - 1 frame if `RETURN_ADDR_IN_PREVIOUS_FRAME' is
   defined.

   The value of the expression must always be the correct address when COUNT is
   zero, but may be `NULL_RTX' if there is not way to determine the return
   address of other frames.  */
#define RETURN_ADDR_RTX(COUNT, FRAMEADDR)	\
  ((COUNT) == 0					\
   ? gen_rtx_MEM (Pmode, arg_pointer_rtx)	\
   : NULL_RTX)

/* Define this if the return address of a particular stack frame is
   accessed from the frame pointer of the previous stack frame.  */
/* #define RETURN_ADDR_IN_PREVIOUS_FRAME */

/* A C expression whose value is RTL representing the location of the incoming
   return address at the beginning of any function, before the prologue.  This
   RTL is either a `REG', indicating that the return value is saved in `REG',
   or a `MEM' representing a location in the stack.

   You only need to define this macro if you want to support call frame
   debugging information like that provided by DWARF 2.  */
#define INCOMING_RETURN_ADDR_RTX  \
   gen_rtx_MEM (SImode, gen_rtx_PLUS (Pmode, stack_pointer_rtx, GEN_INT (-4)))

/* A C expression whose value is an integer giving the offset, in bytes, from
   the value of the stack pointer register to the top of the stack frame at the
   beginning of any function, before the prologue.  The top of the frame is
   defined to be the value of the stack pointer in the previous frame, just
   before the call instruction.

   You only need to define this macro if you want to support call frame
   debugging information like that provided by DWARF 2.  */
#define INCOMING_FRAME_SP_OFFSET (xstormy16_interrupt_function_p () ? 6 : 4)


/* Stack Checking.  */

/* A nonzero value if stack checking is done by the configuration files in a
   machine-dependent manner.  You should define this macro if stack checking is
   require by the ABI of your machine or if you would like to have to stack
   checking in some more efficient way than GNU CC's portable approach.  The
   default value of this macro is zero.  */
/* #define STACK_CHECK_BUILTIN */

/* An integer representing the interval at which GNU CC must generate stack
   probe instructions.  You will normally define this macro to be no larger
   than the size of the "guard pages" at the end of a stack area.  The default
   value of 4096 is suitable for most systems.  */
/* #define STACK_CHECK_PROBE_INTERVAL */

/* A integer which is nonzero if GNU CC should perform the stack probe as a
   load instruction and zero if GNU CC should use a store instruction.  The
   default is zero, which is the most efficient choice on most systems.  */
/* #define STACK_CHECK_PROBE_LOAD */

/* The number of bytes of stack needed to recover from a stack overflow, for
   languages where such a recovery is supported.  The default value of 75 words
   should be adequate for most machines.  */
/* #define STACK_CHECK_PROTECT */

/* The maximum size of a stack frame, in bytes.  GNU CC will generate probe
   instructions in non-leaf functions to ensure at least this many bytes of
   stack are available.  If a stack frame is larger than this size, stack
   checking will not be reliable and GNU CC will issue a warning.  The default
   is chosen so that GNU CC only generates one instruction on most systems.
   You should normally not change the default value of this macro.  */
/* #define STACK_CHECK_MAX_FRAME_SIZE */

/* GNU CC uses this value to generate the above warning message.  It represents
   the amount of fixed frame used by a function, not including space for any
   callee-saved registers, temporaries and user variables.  You need only
   specify an upper bound for this amount and will normally use the default of
   four words.  */
/* #define STACK_CHECK_FIXED_FRAME_SIZE */

/* The maximum size, in bytes, of an object that GNU CC will place in the fixed
   area of the stack frame when the user specifies `-fstack-check'.  GNU CC
   computed the default from the values of the above macros and you will
   normally not need to override that default.  */
/* #define STACK_CHECK_MAX_VAR_SIZE */


/* Register That Address the Stack Frame.  */

/* The register number of the stack pointer register, which must also be a
   fixed register according to `FIXED_REGISTERS'.  On most machines, the
   hardware determines which register this is.  */
#define STACK_POINTER_REGNUM 15

/* The register number of the frame pointer register, which is used to access
   automatic variables in the stack frame.  On some machines, the hardware
   determines which register this is.  On other machines, you can choose any
   register you wish for this purpose.  */
#define FRAME_POINTER_REGNUM 17

/* On some machines the offset between the frame pointer and starting offset of
   the automatic variables is not known until after register allocation has
   been done (for example, because the saved registers are between these two
   locations).  On those machines, define `FRAME_POINTER_REGNUM' the number of
   a special, fixed register to be used internally until the offset is known,
   and define `HARD_FRAME_POINTER_REGNUM' to be actual the hard register number
   used for the frame pointer.

   You should define this macro only in the very rare circumstances when it is
   not possible to calculate the offset between the frame pointer and the
   automatic variables until after register allocation has been completed.
   When this macro is defined, you must also indicate in your definition of
   `ELIMINABLE_REGS' how to eliminate `FRAME_POINTER_REGNUM' into either
   `HARD_FRAME_POINTER_REGNUM' or `STACK_POINTER_REGNUM'.

   Do not define this macro if it would be the same as `FRAME_POINTER_REGNUM'.  */
#define HARD_FRAME_POINTER_REGNUM 13

/* The register number of the arg pointer register, which is used to access the
   function's argument list.  On some machines, this is the same as the frame
   pointer register.  On some machines, the hardware determines which register
   this is.  On other machines, you can choose any register you wish for this
   purpose.  If this is not the same register as the frame pointer register,
   then you must mark it as a fixed register according to `FIXED_REGISTERS', or
   arrange to be able to eliminate it.  */
#define ARG_POINTER_REGNUM 18

/* The register number of the return address pointer register, which is used to
   access the current function's return address from the stack.  On some
   machines, the return address is not at a fixed offset from the frame pointer
   or stack pointer or argument pointer.  This register can be defined to point
   to the return address on the stack, and then be converted by
   `ELIMINABLE_REGS' into either the frame pointer or stack pointer.

   Do not define this macro unless there is no other way to get the return
   address from the stack.  */
/* #define RETURN_ADDRESS_POINTER_REGNUM */

/* Register numbers used for passing a function's static chain pointer.  If
   register windows are used, the register number as seen by the called
   function is `STATIC_CHAIN_INCOMING_REGNUM', while the register number as
   seen by the calling function is `STATIC_CHAIN_REGNUM'.  If these registers
   are the same, `STATIC_CHAIN_INCOMING_REGNUM' need not be defined.

   The static chain register need not be a fixed register.

   If the static chain is passed in memory, these macros should not be defined;
   instead, the next two macros should be defined.  */
#define STATIC_CHAIN_REGNUM 1
/* #define STATIC_CHAIN_INCOMING_REGNUM */

/* If the static chain is passed in memory, these macros provide rtx giving
   `mem' expressions that denote where they are stored.  `STATIC_CHAIN' and
   `STATIC_CHAIN_INCOMING' give the locations as seen by the calling and called
   functions, respectively.  Often the former will be at an offset from the
   stack pointer and the latter at an offset from the frame pointer.

   The variables `stack_pointer_rtx', `frame_pointer_rtx', and
   `arg_pointer_rtx' will have been initialized prior to the use of these
   macros and should be used to refer to those items.

   If the static chain is passed in a register, the two previous
   macros should be defined instead.  */
/* #define STATIC_CHAIN */
/* #define STATIC_CHAIN_INCOMING */


/* Eliminating the Frame Pointer and the Arg Pointer */

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
#define FRAME_POINTER_REQUIRED 0

/* A C statement to store in the variable DEPTH_VAR the difference between the
   frame pointer and the stack pointer values immediately after the function
   prologue.  The value would be computed from information such as the result
   of `get_frame_size ()' and the tables of registers `regs_ever_live' and
   `call_used_regs'.

   If `ELIMINABLE_REGS' is defined, this macro will be not be used and need not
   be defined.  Otherwise, it must be defined even if `FRAME_POINTER_REQUIRED'
   is defined to always be true; in that case, you may set DEPTH_VAR to
   anything.  */
/* #define INITIAL_FRAME_POINTER_OFFSET(DEPTH_VAR) */

/* If defined, this macro specifies a table of register pairs used to eliminate
   unneeded registers that point into the stack frame.  If it is not defined,
   the only elimination attempted by the compiler is to replace references to
   the frame pointer with references to the stack pointer.

   The definition of this macro is a list of structure initializations, each of
   which specifies an original and replacement register.
*/

#define ELIMINABLE_REGS					\
{							\
  {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},		\
  {FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM},	\
  {ARG_POINTER_REGNUM,	 STACK_POINTER_REGNUM},		\
  {ARG_POINTER_REGNUM,	 HARD_FRAME_POINTER_REGNUM},	\
}

/* A C expression that returns non-zero if the compiler is allowed to try to
   replace register number FROM with register number TO.  This macro need only
   be defined if `ELIMINABLE_REGS' is defined, and will usually be the constant
   1, since most of the cases preventing register elimination are things that
   the compiler already knows about.  */

#define CAN_ELIMINATE(FROM, TO)						\
 ((FROM) == ARG_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM		\
  ? ! frame_pointer_needed						\
  : 1)

/* This macro is similar to `INITIAL_FRAME_POINTER_OFFSET'.  It specifies the
   initial difference between the specified pair of registers.  This macro must
   be defined if `ELIMINABLE_REGS' is defined.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  (OFFSET) = xstormy16_initial_elimination_offset (FROM, TO)


/* Passing Function Arguments on the Stack */

/* Define this macro if an argument declared in a prototype as an integral type
   smaller than `int' should actually be passed as an `int'.  In addition to
   avoiding errors in certain cases of mismatch, it also makes for better code
   on certain machines.  */
#define PROMOTE_PROTOTYPES 1

/* A C expression that is the number of bytes actually pushed onto the stack
   when an instruction attempts to push NPUSHED bytes.

   If the target machine does not have a push instruction, do not define this
   macro.  That directs GNU CC to use an alternate strategy: to allocate the
   entire argument block and then store the arguments into it.

   On some machines, the definition

        #define PUSH_ROUNDING(BYTES) (BYTES)

   will suffice.  But on other machines, instructions that appear to push one
   byte actually push two bytes in an attempt to maintain alignment.  Then the
   definition should be

        #define PUSH_ROUNDING(BYTES) (((BYTES) + 1) & ~1)  */
#define PUSH_ROUNDING(BYTES) (((BYTES) + 1) & ~1)

/* If defined, the maximum amount of space required for outgoing arguments will
   be computed and placed into the variable
   `current_function_outgoing_args_size'.  No space will be pushed onto the
   stack for each call; instead, the function prologue should increase the
   stack frame size by this amount.

   Defining both `PUSH_ROUNDING' and `ACCUMULATE_OUTGOING_ARGS' is not
   proper.  */
/* #define ACCUMULATE_OUTGOING_ARGS */

/* Define this macro if functions should assume that stack space has been
   allocated for arguments even when their values are passed in registers.

   The value of this macro is the size, in bytes, of the area reserved for
   arguments passed in registers for the function represented by FNDECL.

   This space can be allocated by the caller, or be a part of the
   machine-dependent stack frame: `OUTGOING_REG_PARM_STACK_SPACE' says
   which.  */
/* #define REG_PARM_STACK_SPACE(FNDECL) */

/* Define these macros in addition to the one above if functions might allocate
   stack space for arguments even when their values are passed in registers.
   These should be used when the stack space allocated for arguments in
   registers is not a simple constant independent of the function declaration.

   The value of the first macro is the size, in bytes, of the area that we
   should initially assume would be reserved for arguments passed in registers.

   The value of the second macro is the actual size, in bytes, of the area that
   will be reserved for arguments passed in registers.  This takes two
   arguments: an integer representing the number of bytes of fixed sized
   arguments on the stack, and a tree representing the number of bytes of
   variable sized arguments on the stack.

   When these macros are defined, `REG_PARM_STACK_SPACE' will only be called
   for libcall functions, the current function, or for a function being called
   when it is known that such stack space must be allocated.  In each case this
   value can be easily computed.

   When deciding whether a called function needs such stack space, and how much
   space to reserve, GNU CC uses these two macros instead of
   `REG_PARM_STACK_SPACE'.  */
/* #define MAYBE_REG_PARM_STACK_SPACE */
/* #define FINAL_REG_PARM_STACK_SPACE(CONST_SIZE, VAR_SIZE) */

/* Define this if it is the responsibility of the caller to allocate the area
   reserved for arguments passed in registers.

   If `ACCUMULATE_OUTGOING_ARGS' is defined, this macro controls whether the
   space for these arguments counts in the value of
   `current_function_outgoing_args_size'.  */
/* #define OUTGOING_REG_PARM_STACK_SPACE */

/* Define this macro if `REG_PARM_STACK_SPACE' is defined, but the stack
   parameters don't skip the area specified by it.

   Normally, when a parameter is not passed in registers, it is placed on the
   stack beyond the `REG_PARM_STACK_SPACE' area.  Defining this macro
   suppresses this behavior and causes the parameter to be passed on the stack
   in its natural location.  */
/* #define STACK_PARMS_IN_REG_PARM_AREA */

/* A C expression that should indicate the number of bytes of its own arguments
   that a function pops on returning, or 0 if the function pops no arguments
   and the caller must therefore pop them all after the function returns.

   FUNDECL is a C variable whose value is a tree node that describes the
   function in question.  Normally it is a node of type `FUNCTION_DECL' that
   describes the declaration of the function.  From this it is possible to
   obtain the DECL_ATTRIBUTES of the function.

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


/* Function Arguments in Registers */

#define NUM_ARGUMENT_REGISTERS 6
#define FIRST_ARGUMENT_REGISTER 2

#define XSTORMY16_WORD_SIZE(TYPE, MODE)				\
  ((((TYPE) ? int_size_in_bytes (TYPE) : GET_MODE_SIZE (MODE))	\
    + 1) 							\
   / 2)

/* A C expression that controls whether a function argument is passed in a
   register, and which register.

   The arguments are CUM, of type CUMULATIVE_ARGS, which summarizes
   (in a way defined by INIT_CUMULATIVE_ARGS and FUNCTION_ARG_ADVANCE)
   all of the previous arguments so far passed in registers; MODE, the
   machine mode of the argument; TYPE, the data type of the argument
   as a tree node or 0 if that is not known (which happens for C
   support library functions); and NAMED, which is 1 for an ordinary
   argument and 0 for nameless arguments that correspond to `...' in
   the called function's prototype.

   The value of the expression should either be a `reg' RTX for the hard
   register in which to pass the argument, or zero to pass the argument on the
   stack.

   For machines like the Vax and 68000, where normally all arguments are
   pushed, zero suffices as a definition.

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
#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED)				  \
  ((MODE) == VOIDmode ? const0_rtx					  \
   : (CUM) + XSTORMY16_WORD_SIZE (TYPE, MODE) > NUM_ARGUMENT_REGISTERS ? 0 \
   : gen_rtx_REG (MODE, (CUM) + 2))

/* Define this macro if the target machine has "register windows", so that the
   register in which a function sees an arguments is not necessarily the same
   as the one in which the caller passed the argument.

   For such machines, `FUNCTION_ARG' computes the register in which the caller
   passes the value, and `FUNCTION_INCOMING_ARG' should be defined in a similar
   fashion to tell the function being called where the arguments will arrive.

   If `FUNCTION_INCOMING_ARG' is not defined, `FUNCTION_ARG' serves both
   purposes.  */
/* #define FUNCTION_INCOMING_ARG(CUM, MODE, TYPE, NAMED) */

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
#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) 0

/* A C expression that indicates when an argument must be passed by reference.
   If nonzero for an argument, a copy of that argument is made in memory and a
   pointer to the argument is passed instead of the argument itself.  The
   pointer is passed in whatever way is appropriate for passing a pointer to
   that type.

   On machines where `REG_PARM_STACK_SPACE' is not defined, a suitable
   definition of this macro might be
        #define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED)  \
          MUST_PASS_IN_STACK (MODE, TYPE)  */
#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED) 0

/* If defined, a C expression that indicates when it is more
   desirable to keep an argument passed by invisible reference as a
   reference, rather than copying it to a pseudo register.  */
/* #define FUNCTION_ARG_KEEP_AS_REFERENCE(CUM, MODE, TYPE, NAMED) */

/* If defined, a C expression that indicates when it is the called function's
   responsibility to make a copy of arguments passed by invisible reference.
   Normally, the caller makes a copy and passes the address of the copy to the
   routine being called.  When FUNCTION_ARG_CALLEE_COPIES is defined and is
   nonzero, the caller does not make a copy.  Instead, it passes a pointer to
   the "live" value.  The called function must not modify this value.  If it
   can be determined that the value won't be modified, it need not make a copy;
   otherwise a copy must be made.  */
/* #define FUNCTION_ARG_CALLEE_COPIES(CUM, MODE, TYPE, NAMED) */

/* A C type for declaring a variable that is used as the first argument of
   `FUNCTION_ARG' and other related values.  For some target machines, the type
   `int' suffices and can hold the number of bytes of argument so far.

   There is no need to record in `CUMULATIVE_ARGS' anything about the arguments
   that have been passed on the stack.  The compiler has other variables to
   keep track of that.  For target machines on which all arguments are passed
   on the stack, there is no need to store anything in `CUMULATIVE_ARGS';
   however, the data structure must exist and should not be empty, so use
   `int'.  

   For this platform, the value of CUMULATIVE_ARGS is the number of words
   of arguments that have been passed in registers so far.  */
typedef int CUMULATIVE_ARGS;

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

/* Like `INIT_CUMULATIVE_ARGS' but overrides it for the purposes of finding the
   arguments for the function being compiled.  If this macro is undefined,
   `INIT_CUMULATIVE_ARGS' is used instead.

   The value passed for LIBNAME is always 0, since library routines with
   special calling conventions are never compiled with GNU CC.  The argument
   LIBNAME exists for symmetry with `INIT_CUMULATIVE_ARGS'.  */
/* #define INIT_CUMULATIVE_INCOMING_ARGS(CUM, FNTYPE, LIBNAME) */

/* A C statement (sans semicolon) to update the summarizer variable CUM to
   advance past an argument in the argument list.  The values MODE, TYPE and
   NAMED describe that argument.  Once this is done, the variable CUM is
   suitable for analyzing the *following* argument with `FUNCTION_ARG', etc.

   This macro need not do anything if the argument in question was passed on
   the stack.  The compiler knows how to track the amount of stack space used
   for arguments without any special help.  */
#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)			\
  ((CUM) = xstormy16_function_arg_advance (CUM, MODE, TYPE, NAMED))

/* If defined, a C expression which determines whether, and in which direction,
   to pad out an argument with extra space.  The value should be of type `enum
   direction': either `upward' to pad above the argument, `downward' to pad
   below, or `none' to inhibit padding.

   The *amount* of padding is always just enough to reach the next multiple of
   `FUNCTION_ARG_BOUNDARY'; this macro does not control it.

   This macro has a default definition which is right for most systems.  For
   little-endian machines, the default is to pad upward.  For big-endian
   machines, the default is to pad downward for an argument of constant size
   shorter than an `int', and upward otherwise.  */
/* #define FUNCTION_ARG_PADDING(MODE, TYPE) */

/* If defined, a C expression that gives the alignment boundary, in bits, of an
   argument with the specified mode and type.  If it is not defined,
   `PARM_BOUNDARY' is used for all arguments.  */
/* #define FUNCTION_ARG_BOUNDARY(MODE, TYPE) */

/* A C expression that is nonzero if REGNO is the number of a hard register in
   which function arguments are sometimes passed.  This does *not* include
   implicit arguments such as the static chain and the structure-value address.
   On many machines, no registers can be used for this purpose since all
   function arguments are pushed on the stack.  */
#define FUNCTION_ARG_REGNO_P(REGNO)					\
  ((REGNO) >= FIRST_ARGUMENT_REGISTER 					\
   && (REGNO) < FIRST_ARGUMENT_REGISTER + NUM_ARGUMENT_REGISTERS)


/* How Scalar Function Values are Returned */

/* The number of the hard register that is used to return a scalar value from a
   function call.  */
#define RETURN_VALUE_REGNUM	FIRST_ARGUMENT_REGISTER
     
/* Define this macro if `-traditional' should not cause functions declared to
   return `float' to convert the value to `double'.  */
/* #define TRADITIONAL_RETURN_FLOAT */

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
  xstormy16_function_value (VALTYPE, FUNC)


/* Define this macro if the target machine has "register windows" so that the
   register in which a function returns its value is not the same as the one in
   which the caller sees the value.

   For such machines, `FUNCTION_VALUE' computes the register in which the
   caller will see the value.  `FUNCTION_OUTGOING_VALUE' should be defined in a
   similar fashion to tell the function where to put the value.

   If `FUNCTION_OUTGOING_VALUE' is not defined, `FUNCTION_VALUE' serves both
   purposes.

   `FUNCTION_OUTGOING_VALUE' is not used for return vales with aggregate data
   types, because these are returned in another way.  See `STRUCT_VALUE_REGNUM'
   and related macros, below.  */
/* #define FUNCTION_OUTGOING_VALUE(VALTYPE, FUNC) */

/* A C expression to create an RTX representing the place where a library
   function returns a value of mode MODE.

   Note that "library function" in this context means a compiler support
   routine, used to perform arithmetic, whose name is known specially by the
   compiler and was not mentioned in the C code being compiled.

   The definition of `LIBRARY_VALUE' need not be concerned aggregate data
   types, because none of the library functions returns such types.  */
#define LIBCALL_VALUE(MODE) gen_rtx_REG (MODE, RETURN_VALUE_REGNUM)

/* A C expression that is nonzero if REGNO is the number of a hard register in
   which the values of called function may come back.

   A register whose use for returning values is limited to serving as the
   second of a pair (for a value of type `double', say) need not be recognized
   by this macro.  So for most machines, this definition suffices:

        #define FUNCTION_VALUE_REGNO_P(N) ((N) == RETURN)

   If the machine has register windows, so that the caller and the called
   function use different registers for the return value, this macro should
   recognize only the caller's register numbers.  */
#define FUNCTION_VALUE_REGNO_P(REGNO) ((REGNO) == RETURN_VALUE_REGNUM)

/* Define this macro if `untyped_call' and `untyped_return' need more space
   than is implied by `FUNCTION_VALUE_REGNO_P' for saving and restoring an
   arbitrary return value.  */
/* #define APPLY_RESULT_SIZE */


/* How Large Values are Returned */

/* A C expression which can inhibit the returning of certain function values in
   registers, based on the type of value.  A nonzero value says to return the
   function value in memory, just as large structures are always returned.
   Here TYPE will be a C expression of type `tree', representing the data type
   of the value.

   Note that values of mode `BLKmode' must be explicitly handled by this macro.
   Also, the option `-fpcc-struct-return' takes effect regardless of this
   macro.  On most systems, it is possible to leave the macro undefined; this
   causes a default definition to be used, whose value is the constant 1 for
   `BLKmode' values, and 0 otherwise.

   Do not use this macro to indicate that structures and unions should always
   be returned in memory.  You should instead use `DEFAULT_PCC_STRUCT_RETURN'
   to indicate this.  */
#define RETURN_IN_MEMORY(TYPE) \
  (int_size_in_bytes (TYPE) > UNITS_PER_WORD * NUM_ARGUMENT_REGISTERS)

/* Define this macro to be 1 if all structure and union return values must be
   in memory.  Since this results in slower code, this should be defined only
   if needed for compatibility with other compilers or with an ABI.  If you
   define this macro to be 0, then the conventions used for structure and union
   return values are decided by the `RETURN_IN_MEMORY' macro.

   If not defined, this defaults to the value 1.  */
/* #define DEFAULT_PCC_STRUCT_RETURN 0 */

/* If the structure value address is passed in a register, then
   `STRUCT_VALUE_REGNUM' should be the number of that register.  */
/* #define STRUCT_VALUE_REGNUM */

/* If the structure value address is not passed in a register, define
   `STRUCT_VALUE' as an expression returning an RTX for the place where the
   address is passed.  If it returns 0, the address is passed as an "invisible"
   first argument.  */
#define STRUCT_VALUE 0

/* On some architectures the place where the structure value address is found
   by the called function is not the same place that the caller put it.  This
   can be due to register windows, or it could be because the function prologue
   moves it to a different place.

   If the incoming location of the structure value address is in a register,
   define this macro as the register number.  */
/* #define STRUCT_VALUE_INCOMING_REGNUM */

/* If the incoming location is not a register, then you should define
   `STRUCT_VALUE_INCOMING' as an expression for an RTX for where the called
   function should find the value.  If it should find the value on the stack,
   define this to create a `mem' which refers to the frame pointer.  A
   definition of 0 means that the address is passed as an "invisible" first
   argument.  */
/* #define STRUCT_VALUE_INCOMING */

/* Define this macro if the usual system convention on the target machine for
   returning structures and unions is for the called function to return the
   address of a static variable containing the value.

   Do not define this if the usual system convention is for the caller to pass
   an address to the subroutine.

   This macro has effect in `-fpcc-struct-return' mode, but it does nothing
   when you use `-freg-struct-return' mode.  */
/* #define PCC_STATIC_STRUCT_RETURN */


/* Caller-Saves Register Allocation */

/* Define this macro if function calls on the target machine do not preserve
   any registers; in other words, if `CALL_USED_REGISTERS' has 1 for all
   registers.  This macro enables `-fcaller-saves' by default.  Eventually that
   option will be enabled by default on all machines and both the option and
   this macro will be eliminated.  */
/* #define DEFAULT_CALLER_SAVES */

/* A C expression to determine whether it is worthwhile to consider placing a
   pseudo-register in a call-clobbered hard register and saving and restoring
   it around each function call.  The expression should be 1 when this is worth
   doing, and 0 otherwise.

   If you don't define this macro, a default is used which is good on most
   machines: `4 * CALLS < REFS'.  */
/* #define CALLER_SAVE_PROFITABLE(REFS, CALLS) */


/* Function Entry and Exit */

/* Define this macro as a C expression that is nonzero if the return
   instruction or the function epilogue ignores the value of the stack pointer;
   in other words, if it is safe to delete an instruction to adjust the stack
   pointer before a return from the function.

   Note that this macro's value is relevant only for functions for which frame
   pointers are maintained.  It is never safe to delete a final stack
   adjustment in a function that has no frame pointer, and the compiler knows
   this regardless of `EXIT_IGNORE_STACK'.  */
/* #define EXIT_IGNORE_STACK */

/* Define this macro as a C expression that is nonzero for registers
   are used by the epilogue or the `return' pattern.  The stack and
   frame pointer registers are already be assumed to be used as
   needed.  */
#define EPILOGUE_USES(REGNO) \
  xstormy16_epilogue_uses (REGNO)

/* Define this macro if the function epilogue contains delay slots to which
   instructions from the rest of the function can be "moved".  The definition
   should be a C expression whose value is an integer representing the number
   of delay slots there.  */
/* #define DELAY_SLOTS_FOR_EPILOGUE */

/* A C expression that returns 1 if INSN can be placed in delay slot number N
   of the epilogue.

   The argument N is an integer which identifies the delay slot now being
   considered (since different slots may have different rules of eligibility).
   It is never negative and is always less than the number of epilogue delay
   slots (what `DELAY_SLOTS_FOR_EPILOGUE' returns).  If you reject a particular
   insn for a given delay slot, in principle, it may be reconsidered for a
   subsequent delay slot.  Also, other insns may (at least in principle) be
   considered for the so far unfilled delay slot.

   The insns accepted to fill the epilogue delay slots are put in an
   RTL list made with `insn_list' objects, stored in the variable
   `current_function_epilogue_delay_list'.  The insn for the first
   delay slot comes first in the list.  Your definition of the macro
   `FUNCTION_EPILOGUE' should fill the delay slots by outputting the
   insns in this list, usually by calling `final_scan_insn'.

   You need not define this macro if you did not define
   `DELAY_SLOTS_FOR_EPILOGUE'.  */
/* #define ELIGIBLE_FOR_EPILOGUE_DELAY(INSN, N) */

/* A C compound statement that outputs the assembler code for a thunk function,
   used to implement C++ virtual function calls with multiple inheritance.  The
   thunk acts as a wrapper around a virtual function, adjusting the implicit
   object parameter before handing control off to the real function.

   First, emit code to add the integer DELTA to the location that contains the
   incoming first argument.  Assume that this argument contains a pointer, and
   is the one used to pass the `this' pointer in C++.  This is the incoming
   argument *before* the function prologue, e.g. `%o0' on a sparc.  The
   addition must preserve the values of all other incoming arguments.

   After the addition, emit code to jump to FUNCTION, which is a
   `FUNCTION_DECL'.  This is a direct pure jump, not a call, and does not touch
   the return address.  Hence returning from FUNCTION will return to whoever
   called the current `thunk'.

   The effect must be as if @var{function} had been called directly
   with the adjusted first argument.  This macro is responsible for
   emitting all of the code for a thunk function;
   TARGET_ASM_FUNCTION_PROLOGUE and TARGET_ASM_FUNCTION_EPILOGUE are
   not invoked.

   The THUNK_FNDECL is redundant.  (DELTA and FUNCTION have already been
   extracted from it.)  It might possibly be useful on some targets, but
   probably not.

   If you do not define this macro, the target-independent code in the C++
   frontend will generate a less efficient heavyweight thunk that calls
   FUNCTION instead of jumping to it.  The generic approach does not support
   varargs.  */
#define ASM_OUTPUT_MI_THUNK(FILE, THUNK_FNDECL, DELTA, FUNCTION)	\
  xstormy16_asm_output_mi_thunk (FILE, THUNK_FNDECL, DELTA, FUNCTION)


/* Generating Code for Profiling.  */

/* A C statement or compound statement to output to FILE some assembler code to
   call the profiling subroutine `mcount'.  Before calling, the assembler code
   must load the address of a counter variable into a register where `mcount'
   expects to find the address.  The name of this variable is `LP' followed by
   the number LABELNO, so you would generate the name using `LP%d' in a
   `fprintf'.

   The details of how the address should be passed to `mcount' are determined
   by your operating system environment, not by GNU CC.  To figure them out,
   compile a small program for profiling using the system's installed C
   compiler and look at the assembler code that results.

   This declaration must be present, but it can be an abort if profiling is
   not implemented.  */
     
#define FUNCTION_PROFILER(FILE, LABELNO) abort ()

/* Define this macro if the code for function profiling should come before the
   function prologue.  Normally, the profiling code comes after.  */
/* #define PROFILE_BEFORE_PROLOGUE */


/* If the target has particular reasons why a function cannot be inlined,
   it may define the TARGET_CANNOT_INLINE_P.  This macro takes one argument,
   the DECL describing the function.  The function should NULL if the function
   *can* be inlined.  Otherwise it should return a pointer to a string containing
   a message describing why the function could not be inlined.  The message will
   displayed if the '-Winline' command line switch has been given.  If the message
   contains a '%s' sequence, this will be replaced by the name of the function.  */
/* #define TARGET_CANNOT_INLINE_P(FN_DECL) xstormy16_cannot_inline_p (FN_DECL) */

/* Implementing the Varargs Macros.  */

/* If defined, is a C expression that produces the machine-specific code for a
   call to `__builtin_saveregs'.  This code will be moved to the very beginning
   of the function, before any parameter access are made.  The return value of
   this function should be an RTX that contains the value to use as the return
   of `__builtin_saveregs'.

   If this macro is not defined, the compiler will output an ordinary call to
   the library function `__builtin_saveregs'.  */
/* #define EXPAND_BUILTIN_SAVEREGS() */

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
    xstormy16_setup_incoming_varargs (ARGS_SO_FAR, MODE, TYPE, & PRETEND_ARGS_SIZE)

/* Define this macro if the location where a function argument is passed
   depends on whether or not it is a named argument.

   This macro controls how the NAMED argument to `FUNCTION_ARG' is set for
   varargs and stdarg functions.  With this macro defined, the NAMED argument
   is always true for named arguments, and false for unnamed arguments.  If
   this is not defined, but `SETUP_INCOMING_VARARGS' is defined, then all
   arguments are treated as named.  Otherwise, all named arguments except the
   last are treated as named.  */
/* #define STRICT_ARGUMENT_NAMING  1 */

/* Build up the stdarg/varargs va_list type tree, assinging it to NODE.  If not
   defined, it is assumed that va_list is a void * pointer.  */
#define BUILD_VA_LIST_TYPE(NODE) \
  ((NODE) = xstormy16_build_va_list ())

/* Implement the stdarg/varargs va_start macro.  STDARG_P is non-zero if this
   is stdarg.h instead of varargs.h.  VALIST is the tree of the va_list
   variable to initialize.  NEXTARG is the machine independent notion of the
   'next' argument after the variable arguments.  If not defined, a standard
   implementation will be defined that works for arguments passed on the stack.  */
#define EXPAND_BUILTIN_VA_START(STDARG_P, VALIST, NEXTARG) \
  xstormy16_expand_builtin_va_start (STDARG_P, VALIST, NEXTARG)

/* Implement the stdarg/varargs va_arg macro.  VALIST is the variable of type
   va_list as a tree, TYPE is the type passed to va_arg.  */
#define EXPAND_BUILTIN_VA_ARG(VALIST, TYPE) \
  xstormy16_expand_builtin_va_arg (VALIST, TYPE)

/* Implement the stdarg/varargs va_end macro.  VALIST is the variable of type
   va_list as a tree.  */
/* #define EXPAND_BUILTIN_VA_END(VALIST) */


/* Trampolines for Nested Functions.  */

/* A C statement to output, on the stream FILE, assembler code for a block of
   data that contains the constant parts of a trampoline.  This code should not
   include a label--the label is taken care of automatically.  */
/* #define TRAMPOLINE_TEMPLATE(FILE) */

/* The name of a subroutine to switch to the section in which the trampoline
   template is to be placed.  The default is a value of `readonly_data_section',
   which places the trampoline in the section containing read-only data.  */
/* #define TRAMPOLINE_SECTION */

/* A C expression for the size in bytes of the trampoline, as an integer.  */
#define TRAMPOLINE_SIZE 8

/* Alignment required for trampolines, in bits.

   If you don't define this macro, the value of `BIGGEST_ALIGNMENT' is used for
   aligning trampolines.  */
#define TRAMPOLINE_ALIGNMENT 16

/* A C statement to initialize the variable parts of a trampoline.  ADDR is an
   RTX for the address of the trampoline; FNADDR is an RTX for the address of
   the nested function; STATIC_CHAIN is an RTX for the static chain value that
   should be passed to the function when it is called.  */
#define INITIALIZE_TRAMPOLINE(ADDR, FNADDR, STATIC_CHAIN) \
  xstormy16_initialize_trampoline (ADDR, FNADDR, STATIC_CHAIN)

/* A C expression to allocate run-time space for a trampoline.  The expression
   value should be an RTX representing a memory reference to the space for the
   trampoline.

   If this macro is not defined, by default the trampoline is allocated as a
   stack slot.  This default is right for most machines.  The exceptions are
   machines where it is impossible to execute instructions in the stack area.
   On such machines, you may have to implement a separate stack, using this
   macro in conjunction with `TARGET_ASM_FUNCTION_PROLOGUE' and
   `TARGET_ASM_FUNCTION_EPILOGUE'.

   FP points to a data structure, a `struct function', which describes the
   compilation status of the immediate containing function of the function
   which the trampoline is for.  Normally (when `ALLOCATE_TRAMPOLINE' is not
   defined), the stack slot for the trampoline is in the stack frame of this
   containing function.  Other allocation strategies probably must do something
   analogous with this information.  */
/* #define ALLOCATE_TRAMPOLINE(FP) */

/* Implementing trampolines is difficult on many machines because they have
   separate instruction and data caches.  Writing into a stack location fails
   to clear the memory in the instruction cache, so when the program jumps to
   that location, it executes the old contents.

   Here are two possible solutions.  One is to clear the relevant parts of the
   instruction cache whenever a trampoline is set up.  The other is to make all
   trampolines identical, by having them jump to a standard subroutine.  The
   former technique makes trampoline execution faster; the latter makes
   initialization faster.

   To clear the instruction cache when a trampoline is initialized, define the
   following macros which describe the shape of the cache.  */

/* The total size in bytes of the cache.  */
/* #define INSN_CACHE_SIZE */

/* The length in bytes of each cache line.  The cache is divided into cache
   lines which are disjoint slots, each holding a contiguous chunk of data
   fetched from memory.  Each time data is brought into the cache, an entire
   line is read at once.  The data loaded into a cache line is always aligned
   on a boundary equal to the line size.  */
/* #define INSN_CACHE_LINE_WIDTH */

/* The number of alternative cache lines that can hold any particular memory
   location.  */
/* #define INSN_CACHE_DEPTH */

/* Alternatively, if the machine has system calls or instructions to clear the
   instruction cache directly, you can define the following macro.  */

/* If defined, expands to a C expression clearing the *instruction cache* in
   the specified interval.  If it is not defined, and the macro INSN_CACHE_SIZE
   is defined, some generic code is generated to clear the cache.  The
   definition of this macro would typically be a series of `asm' statements.
   Both BEG and END are both pointer expressions.  */
/* #define CLEAR_INSN_CACHE (BEG, END) */

/* To use a standard subroutine, define the following macro.  In addition, you
   must make sure that the instructions in a trampoline fill an entire cache
   line with identical instructions, or else ensure that the beginning of the
   trampoline code is always aligned at the same point in its cache line.  Look
   in `m68k.h' as a guide.  */

/* Define this macro if trampolines need a special subroutine to do their work.
   The macro should expand to a series of `asm' statements which will be
   compiled with GNU CC.  They go in a library function named
   `__transfer_from_trampoline'.

   If you need to avoid executing the ordinary prologue code of a compiled C
   function when you jump to the subroutine, you can do so by placing a special
   label of your own in the assembler code.  Use one `asm' statement to
   generate an assembler label, and another to make the label global.  Then
   trampolines can use that label to jump directly to your special assembler
   code.  */
/* #define TRANSFER_FROM_TRAMPOLINE */


/* Implicit Calls to Library Routines */

/* A C string constant giving the name of the function to call for
   multiplication of one signed full-word by another.  If you do not define
   this macro, the default name is used, which is `__mulsi3', a function
   defined in `libgcc.a'.  */
/* #define MULSI3_LIBCALL */

/* A C string constant giving the name of the function to call for division of
   one signed full-word by another.  If you do not define this macro, the
   default name is used, which is `__divsi3', a function defined in `libgcc.a'.  */
/* #define DIVSI3_LIBCALL */

/* A C string constant giving the name of the function to call for division of
   one unsigned full-word by another.  If you do not define this macro, the
   default name is used, which is `__udivsi3', a function defined in
   `libgcc.a'.  */
/* #define UDIVSI3_LIBCALL */

/* A C string constant giving the name of the function to call for the
   remainder in division of one signed full-word by another.  If you do not
   define this macro, the default name is used, which is `__modsi3', a function
   defined in `libgcc.a'.  */
/* #define MODSI3_LIBCALL */

/* A C string constant giving the name of the function to call for the
   remainder in division of one unsigned full-word by another.  If you do not
   define this macro, the default name is used, which is `__umodsi3', a
   function defined in `libgcc.a'.  */
/* #define UMODSI3_LIBCALL */

/* A C string constant giving the name of the function to call for
   multiplication of one signed double-word by another.  If you do not define
   this macro, the default name is used, which is `__muldi3', a function
   defined in `libgcc.a'.  */
/* #define MULDI3_LIBCALL */

/* A C string constant giving the name of the function to call for division of
   one signed double-word by another.  If you do not define this macro, the
   default name is used, which is `__divdi3', a function defined in `libgcc.a'.  */
/* #define DIVDI3_LIBCALL */

/* A C string constant giving the name of the function to call for division of
   one unsigned full-word by another.  If you do not define this macro, the
   default name is used, which is `__udivdi3', a function defined in
   `libgcc.a'.  */
/* #define UDIVDI3_LIBCALL */

/* A C string constant giving the name of the function to call for the
   remainder in division of one signed double-word by another.  If you do not
   define this macro, the default name is used, which is `__moddi3', a function
   defined in `libgcc.a'.  */
/* #define MODDI3_LIBCALL */

/* A C string constant giving the name of the function to call for the
   remainder in division of one unsigned full-word by another.  If you do not
   define this macro, the default name is used, which is `__umoddi3', a
   function defined in `libgcc.a'.  */
/* #define UMODDI3_LIBCALL */

/* Define this macro as a C statement that declares additional library routines
   renames existing ones. `init_optabs' calls this macro after initializing all
   the normal library routines.  */
/* #define INIT_TARGET_OPTABS */

/* The value of `EDOM' on the target machine, as a C integer constant
   expression.  If you don't define this macro, GNU CC does not attempt to
   deposit the value of `EDOM' into `errno' directly.  Look in
   `/usr/include/errno.h' to find the value of `EDOM' on your system.

   If you do not define `TARGET_EDOM', then compiled code reports domain errors
   by calling the library function and letting it report the error.  If
   mathematical functions on your system use `matherr' when there is an error,
   then you should leave `TARGET_EDOM' undefined so that `matherr' is used
   normally.  */
/* #define TARGET_EDOM */

/* Define this macro as a C expression to create an rtl expression that refers
   to the global "variable" `errno'.  (On certain systems, `errno' may not
   actually be a variable.)  If you don't define this macro, a reasonable
   default is used.  */
/* #define GEN_ERRNO_RTX */

/* Define this macro if GNU CC should generate calls to the System V (and ANSI
   C) library functions `memcpy' and `memset' rather than the BSD functions
   `bcopy' and `bzero'.

   Defined in svr4.h.  */
#define TARGET_MEM_FUNCTIONS

/* Define this macro if only `float' arguments cannot be passed to library
   routines (so they must be converted to `double').  This macro affects both
   how library calls are generated and how the library routines in `libgcc1.c'
   accept their arguments.  It is useful on machines where floating and fixed
   point arguments are passed differently, such as the i860.  */
/* #define LIBGCC_NEEDS_DOUBLE */

/* Define this macro to override the type used by the library routines to pick
   up arguments of type `float'.  (By default, they use a union of `float' and
   `int'.)

   The obvious choice would be `float'--but that won't work with traditional C
   compilers that expect all arguments declared as `float' to arrive as
   `double'.  To avoid this conversion, the library routines ask for the value
   as some other type and then treat it as a `float'.

   On some systems, no other type will work for this.  For these systems, you
   must use `LIBGCC_NEEDS_DOUBLE' instead, to force conversion of the values
   `double' before they are passed.  */
/* #define FLOAT_ARG_TYPE */

/* Define this macro to override the way library routines redesignate a `float'
   argument as a `float' instead of the type it was passed as.  The default is
   an expression which takes the `float' field of the union.  */
/* #define FLOATIFY(PASSED_VALUE) */

/* Define this macro to override the type used by the library routines to
   return values that ought to have type `float'.  (By default, they use
   `int'.)

   The obvious choice would be `float'--but that won't work with traditional C
   compilers gratuitously convert values declared as `float' into `double'.  */
/* #define FLOAT_VALUE_TYPE */

/* Define this macro to override the way the value of a `float'-returning
   library routine should be packaged in order to return it.  These functions
   are actually declared to return type `FLOAT_VALUE_TYPE' (normally `int').

   These values can't be returned as type `float' because traditional C
   compilers would gratuitously convert the value to a `double'.

   A local variable named `intify' is always available when the macro `INTIFY'
   is used.  It is a union of a `float' field named `f' and a field named `i'
   whose type is `FLOAT_VALUE_TYPE' or `int'.

   If you don't define this macro, the default definition works by copying the
   value through that union.  */
/* #define INTIFY(FLOAT_VALUE) */

/* Define this macro as the name of the data type corresponding to `SImode' in
   the system's own C compiler.

   You need not define this macro if that type is `long int', as it usually is.  */
/* #define nongcc_SI_type */

/* Define this macro as the name of the data type corresponding to the
   word_mode in the system's own C compiler.

   You need not define this macro if that type is `long int', as it usually is.  */
/* #define nongcc_word_type */

/* Define these macros to supply explicit C statements to carry out various
   arithmetic operations on types `float' and `double' in the library routines
   in `libgcc1.c'.  See that file for a full list of these macros and their
   arguments.

   On most machines, you don't need to define any of these macros, because the
   C compiler that comes with the system takes care of doing them.  */
/* #define perform_...  */

/* Define this macro to generate code for Objective C message sending using the
   calling convention of the NeXT system.  This calling convention involves
   passing the object, the selector and the method arguments all at once to the
   method-lookup library function.

   The default calling convention passes just the object and the selector to
   the lookup function, which returns a pointer to the method.  */
/* #define NEXT_OBJC_RUNTIME */


/* Addressing Modes */

/* Define this macro if the machine supports post-increment addressing.  */
#define HAVE_POST_INCREMENT 1

/* Similar for other kinds of addressing.  */
/* #define HAVE_PRE_INCREMENT 1 */
/* #define HAVE_POST_DECREMENT 1 */
#define HAVE_PRE_DECREMENT 1

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
   `PRINT_OPERAND_ADDRESS'.  */
#ifdef REG_OK_STRICT
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, LABEL)	\
do {							\
  if (xstormy16_legitimate_address_p (MODE, X, 1))	\
    goto LABEL;						\
} while (0)
#else
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, LABEL)	\
do {							\
  if (xstormy16_legitimate_address_p (MODE, X, 0))	\
    goto LABEL;						\
} while (0)
#endif
/* A C expression that is nonzero if X (assumed to be a `reg' RTX) is valid for
   use as a base register.  For hard registers, it should always accept those
   which the hardware permits and reject the others.  Whether the macro accepts
   or rejects pseudo registers must be controlled by `REG_OK_STRICT' as
   described above.  This usually requires two variant definitions, of which
   `REG_OK_STRICT' controls the one actually used.  */
#ifdef REG_OK_STRICT
#define REG_OK_FOR_BASE_P(X) 						   \
  (REGNO_OK_FOR_BASE_P (REGNO (X)) && (REGNO (X) < FIRST_PSEUDO_REGISTER))
#else
#define REG_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))
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

   You may assume that ADDR is a valid address for the machine.  
   
   On this chip, this is true if the address is valid with an offset
   of 0 but not of 6, because in that case it cannot be used as an
   address for DImode or DFmode, or if the address is a post-increment
   or pre-decrement address.
*/
#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)			\
  if (xstormy16_mode_dependent_address_p (ADDR))				\
    goto LABEL

/* A C expression that is nonzero if X is a legitimate constant for an
   immediate operand on the target machine.  You can assume that X satisfies
   `CONSTANT_P', so you need not check this.  In fact, `1' is a suitable
   definition for this macro on machines where anything `CONSTANT_P' is valid.  */
#define LEGITIMATE_CONSTANT_P(X) 1


/* Condition Code Status */

/* C code for a data type which is used for declaring the `mdep' component of
   `cc_status'.  It defaults to `int'.

   This macro is not used on machines that do not use `cc0'.  */
/* #define CC_STATUS_MDEP */

/* A C expression to initialize the `mdep' field to "empty".  The default
   definition does nothing, since most machines don't use the field anyway.  If
   you want to use the field, you should probably define this macro to
   initialize it.

   This macro is not used on machines that do not use `cc0'.  */
/* #define CC_STATUS_MDEP_INIT */

/* A C compound statement to set the components of `cc_status' appropriately
   for an insn INSN whose body is EXP.  It is this macro's responsibility to
   recognize insns that set the condition code as a byproduct of other activity
   as well as those that explicitly set `(cc0)'.

   This macro is not used on machines that do not use `cc0'.

   If there are insns that do not set the condition code but do alter other
   machine registers, this macro must check to see whether they invalidate the
   expressions that the condition code is recorded as reflecting.  For example,
   on the 68000, insns that store in address registers do not set the condition
   code, which means that usually `NOTICE_UPDATE_CC' can leave `cc_status'
   unaltered for such insns.  But suppose that the previous insn set the
   condition code based on location `a4@(102)' and the current insn stores a
   new value in `a4'.  Although the condition code is not changed by this, it
   will no longer be true that it reflects the contents of `a4@(102)'.
   Therefore, `NOTICE_UPDATE_CC' must alter `cc_status' in this case to say
   that nothing is known about the condition code value.

   The definition of `NOTICE_UPDATE_CC' must be prepared to deal with the
   results of peephole optimization: insns whose patterns are `parallel' RTXs
   containing various `reg', `mem' or constants which are just the operands.
   The RTL structure of these insns is not sufficient to indicate what the
   insns actually do.  What `NOTICE_UPDATE_CC' should do when it sees one is
   just to run `CC_STATUS_INIT'.

   A possible definition of `NOTICE_UPDATE_CC' is to call a function that looks
   at an attribute named, for example, `cc'.  This avoids having detailed
   information about patterns in two places, the `md' file and in
   `NOTICE_UPDATE_CC'.  */
/* #define NOTICE_UPDATE_CC(EXP, INSN) */

/* A list of names to be used for additional modes for condition code values in
   registers.  These names are added to `enum machine_mode' and all have class
   `MODE_CC'.  By convention, they should start with `CC' and end with `mode'.

   You should only define this macro if your machine does not use `cc0' and
   only if additional modes are required.  */
/* #define EXTRA_CC_MODES */

/* Returns a mode from class `MODE_CC' to be used when comparison operation
   code OP is applied to rtx X and Y.  For example, on the Sparc,
   `SELECT_CC_MODE' is defined as (see *note Jump Patterns::.  for a
   description of the reason for this definition)

        #define SELECT_CC_MODE(OP,X,Y) \
          (GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT          \
           ? ((OP == EQ || OP == NE) ? CCFPmode : CCFPEmode)    \
           : ((GET_CODE (X) == PLUS || GET_CODE (X) == MINUS    \
               || GET_CODE (X) == NEG) \
              ? CC_NOOVmode : CCmode))

   You need not define this macro if `EXTRA_CC_MODES' is not defined.  */
/* #define SELECT_CC_MODE(OP, X, Y) */

/* One some machines not all possible comparisons are defined, but you can
   convert an invalid comparison into a valid one.  For example, the Alpha does
   not have a `GT' comparison, but you can use an `LT' comparison instead and
   swap the order of the operands.

   On such machines, define this macro to be a C statement to do any required
   conversions.  CODE is the initial comparison code and OP0 and OP1 are the
   left and right operands of the comparison, respectively.  You should modify
   CODE, OP0, and OP1 as required.

   GNU CC will not assume that the comparison resulting from this macro is
   valid but will see if the resulting insn matches a pattern in the `md' file.

   You need not define this macro if it would never change the comparison code
   or operands.  */
/* #define CANONICALIZE_COMPARISON(CODE, OP0, OP1) */

/* A C expression whose value is one if it is always safe to reverse a
   comparison whose mode is MODE.  If `SELECT_CC_MODE' can ever return MODE for
   a floating-point inequality comparison, then `REVERSIBLE_CC_MODE (MODE)'
   must be zero.

   You need not define this macro if it would always returns zero or if the
   floating-point format is anything other than `IEEE_FLOAT_FORMAT'.  For
   example, here is the definition used on the Sparc, where floating-point
   inequality comparisons are always given `CCFPEmode':

        #define REVERSIBLE_CC_MODE(MODE)  ((MODE) != CCFPEmode)  */
/* #define REVERSIBLE_CC_MODE(MODE) */


/* Describing Relative Costs of Operations */

/* A part of a C `switch' statement that describes the relative costs of
   constant RTL expressions.  It must contain `case' labels for expression
   codes `const_int', `const', `symbol_ref', `label_ref' and `const_double'.
   Each case must ultimately reach a `return' statement to return the relative
   cost of the use of that kind of constant value in an expression.  The cost
   may depend on the precise value of the constant, which is available for
   examination in X, and the rtx code of the expression in which it is
   contained, found in OUTER_CODE.

   CODE is the expression code--redundant, since it can be obtained with
   `GET_CODE (X)'.  */
#define CONST_COSTS(X, CODE, OUTER_CODE)	\
  case CONST_INT:				\
    if (INTVAL (X) < 16 && INTVAL (X) >= 0)	\
      return COSTS_N_INSNS (1)/2;		\
    if (INTVAL (X) < 256 && INTVAL (X) >= 0)	\
      return COSTS_N_INSNS (1);			\
  case CONST_DOUBLE:				\
  case CONST:					\
  case SYMBOL_REF:				\
  case LABEL_REF:				\
     return COSTS_N_INSNS(2);

/* Like `CONST_COSTS' but applies to nonconstant RTL expressions.  This can be
   used, for example, to indicate how costly a multiply instruction is.  In
   writing this macro, you can use the construct `COSTS_N_INSNS (N)' to specify
   a cost equal to N fast instructions.  OUTER_CODE is the code of the
   expression in which X is contained.

   This macro is optional; do not define it if the default cost assumptions are
   adequate for the target machine.  */
#define RTX_COSTS(X, CODE, OUTER_CODE)		\
  case MULT:					\
    return COSTS_N_INSNS (35 + 6);		\
  case DIV:					\
    return COSTS_N_INSNS (51 - 6);

/* An expression giving the cost of an addressing mode that contains ADDRESS.
   If not defined, the cost is computed from the ADDRESS expression and the
   `CONST_COSTS' values.

   For most CISC machines, the default cost is a good approximation of the true
   cost of the addressing mode.  However, on RISC machines, all instructions
   normally have the same length and execution time.  Hence all addresses will
   have equal costs.

   In cases where more than one form of an address is known, the form with the
   lowest cost will be used.  If multiple forms have the same, lowest, cost,
   the one that is the most complex will be used.

   For example, suppose an address that is equal to the sum of a register and a
   constant is used twice in the same basic block.  When this macro is not
   defined, the address will be computed in a register and memory references
   will be indirect through that register.  On machines where the cost of the
   addressing mode containing the sum is no higher than that of a simple
   indirect reference, this will produce an additional instruction and possibly
   require an additional register.  Proper specification of this macro
   eliminates this overhead for such machines.

   Similar use of this macro is made in strength reduction of loops.

   ADDRESS need not be valid as an address.  In such a case, the cost is not
   relevant and can be any value; invalid addresses need not be assigned a
   different cost.

   On machines where an address involving more than one register is as cheap as
   an address computation involving only one register, defining `ADDRESS_COST'
   to reflect this can cause two registers to be live over a region of code
   where only one would have been if `ADDRESS_COST' were not defined in that
   manner.  This effect should be considered in the definition of this macro.
   Equivalent costs should probably only be given to addresses with different
   numbers of registers on machines with lots of registers.

   This macro will normally either not be defined or be defined as a
   constant.  */
#define ADDRESS_COST(ADDRESS)			\
  (GET_CODE (ADDRESS) == CONST_INT ? 2		\
   : GET_CODE (ADDRESS) == PLUS ? 7		\
   : 5)

/* A C expression for the cost of moving data of mode MODE from a
   register in class FROM to one in class TO.  The classes are
   expressed using the enumeration values such as `GENERAL_REGS'.  A
   value of 4 is the default; other values are interpreted relative to
   that.

   It is not required that the cost always equal 2 when FROM is the same as TO;
   on some machines it is expensive to move between registers if they are not
   general registers.

   If reload sees an insn consisting of a single `set' between two hard
   registers, and if `REGISTER_MOVE_COST' applied to their classes returns a
   value of 2, reload does not check to ensure that the constraints of the insn
   are met.  Setting a cost of other than 2 will allow reload to verify that
   the constraints are met.  You should do this if the `movM' pattern's
   constraints do not allow such copying.  */
#define REGISTER_MOVE_COST(MODE, FROM, TO) 2

/* A C expression for the cost of moving data of mode M between a register and
   memory.  A value of 2 is the default; this cost is relative to those in
   `REGISTER_MOVE_COST'.

   If moving between registers and memory is more expensive than between two
   registers, you should define this macro to express the relative cost.  */
#define MEMORY_MOVE_COST(M,C,I) (5 + memory_move_secondary_cost (M, C, I))

/* A C expression for the cost of a branch instruction.  A value of 1 is the
   default; other values are interpreted relative to that.  */

#define BRANCH_COST 5

/* Here are additional macros which do not specify precise relative costs, but
   only that certain actions are more expensive than GNU CC would ordinarily
   expect.  */

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
#define SLOW_BYTE_ACCESS 0

/* Define this macro to be the value 1 if unaligned accesses have a cost many
   times greater than aligned accesses, for example if they are emulated in a
   trap handler.

   When this macro is non-zero, the compiler will act as if `STRICT_ALIGNMENT'
   were non-zero when generating code for block moves.  This can cause
   significantly more instructions to be produced.  Therefore, do not set this
   macro non-zero if unaligned accesses only add a cycle or two to the time for
   a memory access.

   If the value of this macro is always zero, it need not be defined.  */
/* #define SLOW_UNALIGNED_ACCESS */

/* Define this macro to inhibit strength reduction of memory addresses.  (On
   some machines, such strength reduction seems to do harm rather than good.)  */
/* #define DONT_REDUCE_ADDR */

/* The number of scalar move insns which should be generated instead of a
   string move insn or a library call.  Increasing the value will always make
   code faster, but eventually incurs high cost in increased code size.

   If you don't define this, a reasonable default is used.  */
/* #define MOVE_RATIO */

/* Define this macro if it is as good or better to call a constant function
   address than to call an address kept in a register.  */
#define NO_FUNCTION_CSE

/* Define this macro if it is as good or better for a function to call itself
   with an explicit address than to call an address kept in a register.  */
#define NO_RECURSIVE_FUNCTION_CSE

/* A C statement (sans semicolon) to update the integer variable COST based on
   the relationship between INSN that is dependent on DEP_INSN through the
   dependence LINK.  The default is to make no adjustment to COST.  This can be
   used for example to specify to the scheduler that an output- or
   anti-dependence does not incur the same cost as a data-dependence.  */
/* #define ADJUST_COST(INSN, LINK, DEP_INSN, COST) */

/* A C statement (sans semicolon) to update the integer scheduling
   priority `INSN_PRIORITY(INSN)'.  Reduce the priority to execute
   the INSN earlier, increase the priority to execute INSN later.
   Do not define this macro if you do not need to adjust the
   scheduling priorities of insns.  */
/* #define ADJUST_PRIORITY (INSN) */


/* Dividing the output into sections.  */

/* A C expression whose value is a string containing the assembler operation
   that should precede instructions and read-only data.  Normally `".text"' is
   right.  */
#define TEXT_SECTION_ASM_OP ".text"

/* A C expression whose value is a string containing the assembler operation to
   identify the following data as writable initialized data.  Normally
   `".data"' is right.  */
#define DATA_SECTION_ASM_OP ".data"

/* if defined, a C expression whose value is a string containing the assembler
   operation to identify the following data as shared data.  If not defined,
   `DATA_SECTION_ASM_OP' will be used.  */
/* #define SHARED_SECTION_ASM_OP */

/* If defined, a C expression whose value is a string containing the
   assembler operation to identify the following data as
   uninitialized global data.  If not defined, and neither
   `ASM_OUTPUT_BSS' nor `ASM_OUTPUT_ALIGNED_BSS' are defined,
   uninitialized global data will be output in the data section if
   `-fno-common' is passed, otherwise `ASM_OUTPUT_COMMON' will be
   used.  */
#define BSS_SECTION_ASM_OP ".bss"

/* If defined, a C expression whose value is a string containing the
   assembler operation to identify the following data as
   uninitialized global shared data.  If not defined, and
   `BSS_SECTION_ASM_OP' is, the latter will be used.  */
/* #define SHARED_BSS_SECTION_ASM_OP */

/* Define the pseudo-ops used to switch to the .ctors and .dtors sections.
   There are no shared libraries on this target so these sections need
   not be writable.

   Defined in elfos.h.  */

#undef CTORS_SECTION_ASM_OP
#undef DTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP	"\t.section\t.ctors,\"a\""
#define DTORS_SECTION_ASM_OP	"\t.section\t.dtors,\"a\""

/* A list of names for sections other than the standard two, which are
   `in_text' and `in_data'.  You need not define this macro on a system with no
   other sections (that GCC needs to use).

   Defined in svr4.h.  */
/* #define EXTRA_SECTIONS */

/* One or more functions to be defined in `varasm.c'.  These functions should
   do jobs analogous to those of `text_section' and `data_section', for your
   additional sections.  Do not define this macro if you do not define
   `EXTRA_SECTIONS'.

   Defined in svr4.h.  */
/* #define EXTRA_SECTION_FUNCTIONS */

/* On most machines, read-only variables, constants, and jump tables are placed
   in the text section.  If this is not the case on your machine, this macro
   should be defined to be the name of a function (either `data_section' or a
   function defined in `EXTRA_SECTIONS') that switches to the section to be
   used for read-only items.

   If these items should be placed in the text section, this macro should not
   be defined.  */
/* #define READONLY_DATA_SECTION */

/* A C statement or statements to switch to the appropriate section for output
   of EXP.  You can assume that EXP is either a `VAR_DECL' node or a constant
   of some sort.  RELOC indicates whether the initial value of EXP requires
   link-time relocations.  Select the section by calling `text_section' or one
   of the alternatives for other sections.

   Do not define this macro if you put all read-only variables and constants in
   the read-only data section (usually the text section).

   Defined in svr4.h.  */
/* #define SELECT_SECTION(EXP, RELOC, ALIGN) */

/* A C statement or statements to switch to the appropriate section for output
   of RTX in mode MODE.  You can assume that RTX is some kind of constant in
   RTL.  The argument MODE is redundant except in the case of a `const_int'
   rtx.  Select the section by calling `text_section' or one of the
   alternatives for other sections.

   Do not define this macro if you put all constants in the read-only data
   section.

   Defined in svr4.h.  */
/* #define SELECT_RTX_SECTION(MODE, RTX, ALIGN) */

/* Define this macro if jump tables (for `tablejump' insns) should be output in
   the text section, along with the assembler instructions.  Otherwise, the
   readonly data section is used.

   This macro is irrelevant if there is no separate readonly data section.  */
#define JUMP_TABLES_IN_TEXT_SECTION 1

/* Define this macro if references to a symbol must be treated differently
   depending on something about the variable or function named by the symbol
   (such as what section it is in).

   The macro definition, if any, is executed immediately after the rtl for DECL
   has been created and stored in `DECL_RTL (DECL)'.  The value of the rtl will
   be a `mem' whose address is a `symbol_ref'.

   The usual thing for this macro to do is to record a flag in the `symbol_ref'
   (such as `SYMBOL_REF_FLAG') or to store a modified name string in the
   `symbol_ref' (if one bit is not enough information).  */
#define ENCODE_SECTION_INFO(DECL)  xstormy16_encode_section_info(DECL)

/* Decode SYM_NAME and store the real name part in VAR, sans the characters
   that encode section info.  Define this macro if `ENCODE_SECTION_INFO' alters
   the symbol's name string.  */
/* #define STRIP_NAME_ENCODING(VAR, SYM_NAME) */

/* A C statement to build up a unique section name, expressed as a
   STRING_CST node, and assign it to `DECL_SECTION_NAME (DECL)'.
   RELOC indicates whether the initial value of EXP requires
   link-time relocations.  If you do not define this macro, GNU CC
   will use the symbol name prefixed by `.' as the section name.

   Defined in svr4.h.  */
/* #define UNIQUE_SECTION(DECL, RELOC) */


/* Position Independent Code.  */

/* The register number of the register used to address a table of static data
   addresses in memory.  In some cases this register is defined by a
   processor's "application binary interface" (ABI).  When this macro is
   defined, RTL is generated for this register once, as with the stack pointer
   and frame pointer registers.  If this macro is not defined, it is up to the
   machine-dependent files to allocate such a register (if necessary).  */
/* #define PIC_OFFSET_TABLE_REGNUM */

/* Define this macro if the register defined by `PIC_OFFSET_TABLE_REGNUM' is
   clobbered by calls.  Do not define this macro if `PPIC_OFFSET_TABLE_REGNUM'
   is not defined.  */
/* #define PIC_OFFSET_TABLE_REG_CALL_CLOBBERED */

/* By generating position-independent code, when two different programs (A and
   B) share a common library (libC.a), the text of the library can be shared
   whether or not the library is linked at the same address for both programs.
   In some of these environments, position-independent code requires not only
   the use of different addressing modes, but also special code to enable the
   use of these addressing modes.

   The `FINALIZE_PIC' macro serves as a hook to emit these special codes once
   the function is being compiled into assembly code, but not before.  (It is
   not done before, because in the case of compiling an inline function, it
   would lead to multiple PIC prologues being included in functions which used
   inline functions and were compiled to assembly language.)  */
/* #define FINALIZE_PIC */

/* A C expression that is nonzero if X is a legitimate immediate operand on the
   target machine when generating position independent code.  You can assume
   that X satisfies `CONSTANT_P', so you need not check this.  You can also
   assume FLAG_PIC is true, so you need not check it either.  You need not
   define this macro if all constants (including `SYMBOL_REF') can be immediate
   operands when generating position independent code.  */
/* #define LEGITIMATE_PIC_OPERAND_P(X) */


/* The Overall Framework of an Assembler File.  */

/* A C expression which outputs to the stdio stream STREAM some appropriate
   text to go at the start of an assembler file.

   Normally this macro is defined to output a line containing `#NO_APP', which
   is a comment that has no effect on most assemblers but tells the GNU
   assembler that it can save time by not checking for certain assembler
   constructs.

   On systems that use SDB, it is necessary to output certain commands; see
   `attasm.h'.

   Defined in svr4.h.  */
/* #define ASM_FILE_START(STREAM) */

/* A C expression which outputs to the stdio stream STREAM some appropriate
   text to go at the end of an assembler file.

   If this macro is not defined, the default is to output nothing special at
   the end of the file.  Most systems don't require any definition.

   On systems that use SDB, it is necessary to output certain commands; see
   `attasm.h'.

   Defined in svr4.h.  */
/* #define ASM_FILE_END(STREAM) */

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

/* A C statement to output COFF information or DWARF debugging information
   which indicates that filename NAME is the current source file to the stdio
   stream STREAM.

   This macro need not be defined if the standard form of output for the file
   format in use is appropriate.  */
/* #define ASM_OUTPUT_SOURCE_FILENAME(STREAM, NAME) */

/* A C statement to output DBX or SDB debugging information before code for
   line number LINE of the current source file to the stdio stream STREAM.

   This macro need not be defined if the standard form of debugging information
   for the debugger in use is appropriate.

   Defined in svr4.h.  */
/* #define ASM_OUTPUT_SOURCE_LINE(STREAM, LINE) */

/* A C statement to output something to the assembler file to handle a `#ident'
   directive containing the text STRING.  If this macro is not defined, nothing
   is output for a `#ident' directive.

   Defined in svr4.h.  */
/* #define ASM_OUTPUT_IDENT(STREAM, STRING) */

/* A C statement to output something to the assembler file to switch to section
   NAME for object DECL which is either a `FUNCTION_DECL', a `VAR_DECL' or
   `NULL_TREE'.  Some target formats do not support arbitrary sections.  Do not
   define this macro in such cases.

   At present this macro is only used to support section attributes.  When this
   macro is undefined, section attributes are disabled.

   Defined in svr4.h.  */
/* #define ASM_OUTPUT_SECTION_NAME(STREAM, DECL, NAME) */

/* A C statement to output any assembler statements which are required to
   precede any Objective C object definitions or message sending.  The
   statement is executed only when compiling an Objective C program.  */
/* #define OBJC_PROLOGUE */


/* Output of Data.  */

/* A C statement to output to the stdio stream STREAM an assembler instruction
   to assemble a string constant containing the LEN bytes at PTR.  PTR will be
   a C expression of type `char *' and LEN a C expression of type `int'.

   If the assembler has a `.ascii' pseudo-op as found in the Berkeley Unix
   assembler, do not define the macro `ASM_OUTPUT_ASCII'.

   Defined in svr4.h.  */
/* #define ASM_OUTPUT_ASCII(STREAM, PTR, LEN) */

/* You may define this macro as a C expression.  You should define the
   expression to have a non-zero value if GNU CC should output the
   constant pool for a function before the code for the function, or
   a zero value if GNU CC should output the constant pool after the
   function.  If you do not define this macro, the usual case, GNU CC
   will output the constant pool before the function.  */
/* #define CONSTANT_POOL_BEFORE_FUNCTION */

/* A C statement to output assembler commands to define the start of the
   constant pool for a function.  FUNNAME is a string giving the name of the
   function.  Should the return type of the function be required, it can be
   obtained via FUNDECL.  SIZE is the size, in bytes, of the constant pool that
   will be written immediately after this call.

   If no constant-pool prefix is required, the usual case, this macro need not
   be defined.  */
/* #define ASM_OUTPUT_POOL_PROLOGUE(FILE FUNNAME FUNDECL SIZE) */

/* A C statement (with or without semicolon) to output a constant in the
   constant pool, if it needs special treatment.  (This macro need not do
   anything for RTL expressions that can be output normally.)

   The argument FILE is the standard I/O stream to output the assembler code
   on.  X is the RTL expression for the constant to output, and MODE is the
   machine mode (in case X is a `const_int').  ALIGN is the required alignment
   for the value X; you should output an assembler directive to force this much
   alignment.

   The argument LABELNO is a number to use in an internal label for the address
   of this pool entry.  The definition of this macro is responsible for
   outputting the label definition at the proper place.  Here is how to do
   this:

        ASM_OUTPUT_INTERNAL_LABEL (FILE, "LC", LABELNO);

   When you output a pool entry specially, you should end with a `goto' to the
   label JUMPTO.  This will prevent the same pool entry from being output a
   second time in the usual manner.

   You need not define this macro if it would do nothing.  */
/* #define ASM_OUTPUT_SPECIAL_POOL_ENTRY(FILE, X, MODE, ALIGN, LABELNO, JUMPTO) */

/* Define this macro as a C expression which is nonzero if the constant EXP, of
   type `tree', should be output after the code for a function.  The compiler
   will normally output all constants before the function; you need not define
   this macro if this is OK.  */
/* #define CONSTANT_AFTER_FUNCTION_P(EXP) */

/* A C statement to output assembler commands to at the end of the constant
   pool for a function.  FUNNAME is a string giving the name of the function.
   Should the return type of the function be required, you can obtain it via
   FUNDECL.  SIZE is the size, in bytes, of the constant pool that GNU CC wrote
   immediately before this call.

   If no constant-pool epilogue is required, the usual case, you need not
   define this macro.  */
/* #define ASM_OUTPUT_POOL_EPILOGUE (FILE FUNNAME FUNDECL SIZE) */

/* Define this macro as a C expression which is nonzero if C is used as a
   logical line separator by the assembler.

   If you do not define this macro, the default is that only the character `;'
   is treated as a logical line separator.  */
#define IS_ASM_LOGICAL_LINE_SEPARATOR(C) ((C) == '|')

/* These macros are provided by `real.h' for writing the definitions of
   `ASM_OUTPUT_DOUBLE' and the like: */

/* These translate X, of type `REAL_VALUE_TYPE', to the target's floating point
   representation, and store its bit pattern in the array of `long int' whose
   address is L.  The number of elements in the output array is determined by
   the size of the desired target floating point data type: 32 bits of it go in
   each `long int' array element.  Each array element holds 32 bits of the
   result, even if `long int' is wider than 32 bits on the host machine.

   The array element values are designed so that you can print them out using
   `fprintf' in the order they should appear in the target machine's memory.  */
/* #define REAL_VALUE_TO_TARGET_SINGLE(X, L) */
/* #define REAL_VALUE_TO_TARGET_DOUBLE(X, L) */
/* #define REAL_VALUE_TO_TARGET_LONG_DOUBLE(X, L) */

/* This macro converts X, of type `REAL_VALUE_TYPE', to a decimal number and
   stores it as a string into STRING.  You must pass, as STRING, the address of
   a long enough block of space to hold the result.

   The argument FORMAT is a `printf'-specification that serves as a suggestion
   for how to format the output string.  */
/* #define REAL_VALUE_TO_DECIMAL(X, FORMAT, STRING) */


/* Output of Uninitialized Variables.  */

/* A C statement (sans semicolon) to output to the stdio stream STREAM the
   assembler definition of a common-label named NAME whose size is SIZE bytes.
   The variable ROUNDED is the size rounded up to whatever alignment the caller
   wants.

   Use the expression `assemble_name (STREAM, NAME)' to output the name itself;
   before and after that, output the additional assembler syntax for defining
   the name, and a newline.

   This macro controls how the assembler definitions of uninitialized global
   variables are output.  */
/* #define ASM_OUTPUT_COMMON(STREAM, NAME, SIZE, ROUNDED) */

/* Like `ASM_OUTPUT_COMMON' except takes the required alignment as a separate,
   explicit argument.  If you define this macro, it is used in place of
   `ASM_OUTPUT_COMMON', and gives you more flexibility in handling the required
   alignment of the variable.  The alignment is specified as the number of
   bits.

   Defined in svr4.h.  */
/* #define ASM_OUTPUT_ALIGNED_COMMON(STREAM, NAME, SIZE, ALIGNMENT) */

/* Like ASM_OUTPUT_ALIGNED_COMMON except that it takes an additional argument -
   the DECL of the variable to be output, if there is one.  This macro can be
   called with DECL == NULL_TREE.  If you define this macro, it is used in
   place of both ASM_OUTPUT_COMMON and ASM_OUTPUT_ALIGNED_COMMON, and gives you
   more flexibility in handling the destination of the variable.  */
/* #define ASM_OUTPUT_DECL_COMMON (STREAM, DECL, NAME, SIZE, ALIGNMENT) */

/* If defined, it is similar to `ASM_OUTPUT_COMMON', except that it is used
   when NAME is shared.  If not defined, `ASM_OUTPUT_COMMON' will be used.  */
/* #define ASM_OUTPUT_SHARED_COMMON(STREAM, NAME, SIZE, ROUNDED) */

/* A C statement (sans semicolon) to output to the stdio stream STREAM the
   assembler definition of uninitialized global DECL named NAME whose size is
   SIZE bytes.  The variable ROUNDED is the size rounded up to whatever
   alignment the caller wants.

   Try to use function `asm_output_bss' defined in `varasm.c' when defining
   this macro.  If unable, use the expression `assemble_name (STREAM, NAME)' to
   output the name itself; before and after that, output the additional
   assembler syntax for defining the name, and a newline.

   This macro controls how the assembler definitions of uninitialized global
   variables are output.  This macro exists to properly support languages like
   `c++' which do not have `common' data.  However, this macro currently is not
   defined for all targets.  If this macro and `ASM_OUTPUT_ALIGNED_BSS' are not
   defined then `ASM_OUTPUT_COMMON' or `ASM_OUTPUT_ALIGNED_COMMON' or
   `ASM_OUTPUT_DECL_COMMON' is used.  */
/* #define ASM_OUTPUT_BSS(STREAM, DECL, NAME, SIZE, ROUNDED) */

/* Like `ASM_OUTPUT_BSS' except takes the required alignment as a separate,
   explicit argument.  If you define this macro, it is used in place of
   `ASM_OUTPUT_BSS', and gives you more flexibility in handling the required
   alignment of the variable.  The alignment is specified as the number of
   bits.

   Try to use function `asm_output_aligned_bss' defined in file `varasm.c' when
   defining this macro.  */
/* #define ASM_OUTPUT_ALIGNED_BSS(STREAM, DECL, NAME, SIZE, ALIGNMENT) */

/* If defined, it is similar to `ASM_OUTPUT_BSS', except that it is used when
   NAME is shared.  If not defined, `ASM_OUTPUT_BSS' will be used.  */
/* #define ASM_OUTPUT_SHARED_BSS(STREAM, DECL, NAME, SIZE, ROUNDED) */

/* A C statement (sans semicolon) to output to the stdio stream STREAM the
   assembler definition of a local-common-label named NAME whose size is SIZE
   bytes.  The variable ROUNDED is the size rounded up to whatever alignment
   the caller wants.

   Use the expression `assemble_name (STREAM, NAME)' to output the name itself;
   before and after that, output the additional assembler syntax for defining
   the name, and a newline.

   This macro controls how the assembler definitions of uninitialized static
   variables are output.  */
/* #define ASM_OUTPUT_LOCAL(STREAM, NAME, SIZE, ROUNDED) */

/* Like `ASM_OUTPUT_LOCAL' except takes the required alignment as a separate,
   explicit argument.  If you define this macro, it is used in place of
   `ASM_OUTPUT_LOCAL', and gives you more flexibility in handling the required
   alignment of the variable.  The alignment is specified as the number of
   bits.

   Defined in svr4.h.  */
/* #define ASM_OUTPUT_ALIGNED_LOCAL(STREAM, NAME, SIZE, ALIGNMENT) */

/* Like `ASM_OUTPUT_ALIGNED_LOCAL' except that it takes an additional
   parameter - the DECL of variable to be output, if there is one.
   This macro can be called with DECL == NULL_TREE.  If you define
   this macro, it is used in place of `ASM_OUTPUT_LOCAL' and
   `ASM_OUTPUT_ALIGNED_LOCAL', and gives you more flexibility in
   handling the destination of the variable.  */
/* #define ASM_OUTPUT_DECL_LOCAL(STREAM, DECL, NAME, SIZE, ALIGNMENT) */

/* If defined, it is similar to `ASM_OUTPUT_LOCAL', except that it is used when
   NAME is shared.  If not defined, `ASM_OUTPUT_LOCAL' will be used.  */
/* #define ASM_OUTPUT_SHARED_LOCAL (STREAM, NAME, SIZE, ROUNDED) */


/* Output and Generation of Labels.  */

/* A C statement (sans semicolon) to output to the stdio stream STREAM the
   assembler definition of a label named NAME.  Use the expression
   `assemble_name (STREAM, NAME)' to output the name itself; before and after
   that, output the additional assembler syntax for defining the name, and a
   newline.  */
#define ASM_OUTPUT_LABEL(STREAM, NAME)					\
do {									\
  assemble_name (STREAM, NAME);						\
  fputs (":\n", STREAM);						\
} while (0)

/* A C statement to output to the stdio stream STREAM the assembler
   definition of a symbol named SYMBOL.  */
#define ASM_OUTPUT_SYMBOL_REF(STREAM, SYMBOL)				\
  do {									\
    if (SYMBOL_REF_FLAG (SYMBOL))					\
      {									\
        fputs ("@fptr(", STREAM);					\
	assemble_name (STREAM, XSTR (SYMBOL, 0));			\
	fputc (')', STREAM);						\
      }									\
    else								\
      assemble_name (STREAM, XSTR (SYMBOL, 0));				\
  } while (0)

/* A C statement to output to the stdio stream STREAM the assembler
   definition of a label, the textual form is in 'BUF'.  Not used
   for %l.  */
#define ASM_OUTPUT_LABEL_REF(STREAM, NAME)	\
do  {						\
  fputs ("@fptr(", STREAM);			\
  assemble_name (STREAM, NAME);			\
  fputc (')', STREAM);				\
} while (0)

/* A C statement (sans semicolon) to output to the stdio stream STREAM any text
   necessary for declaring the name NAME of a function which is being defined.
   This macro is responsible for outputting the label definition (perhaps using
   `ASM_OUTPUT_LABEL').  The argument DECL is the `FUNCTION_DECL' tree node
   representing the function.

   If this macro is not defined, then the function name is defined in the usual
   manner as a label (by means of `ASM_OUTPUT_LABEL').

   Defined in svr4.h.  */
/* #define ASM_DECLARE_FUNCTION_NAME(STREAM, NAME, DECL) */

/* A C statement (sans semicolon) to output to the stdio stream STREAM any text
   necessary for declaring the size of a function which is being defined.  The
   argument NAME is the name of the function.  The argument DECL is the
   `FUNCTION_DECL' tree node representing the function.

   If this macro is not defined, then the function size is not defined.

   Defined in svr4.h.  */
/* #define ASM_DECLARE_FUNCTION_SIZE(STREAM, NAME, DECL) */

/* A C statement (sans semicolon) to output to the stdio stream STREAM any text
   necessary for declaring the name NAME of an initialized variable which is
   being defined.  This macro must output the label definition (perhaps using
   `ASM_OUTPUT_LABEL').  The argument DECL is the `VAR_DECL' tree node
   representing the variable.

   If this macro is not defined, then the variable name is defined in the usual
   manner as a label (by means of `ASM_OUTPUT_LABEL').

   Defined in svr4.h.  */
/* #define ASM_DECLARE_OBJECT_NAME(STREAM, NAME, DECL) */

/* A C statement (sans semicolon) to finish up declaring a variable name once
   the compiler has processed its initializer fully and thus has had a chance
   to determine the size of an array when controlled by an initializer.  This
   is used on systems where it's necessary to declare something about the size
   of the object.

   If you don't define this macro, that is equivalent to defining it to do
   nothing.

   Defined in svr4.h.  */
/* #define ASM_FINISH_DECLARE_OBJECT(STREAM, DECL, TOPLEVEL, ATEND) */

/* A C statement (sans semicolon) to output to the stdio stream STREAM some
   commands that will make the label NAME global; that is, available for
   reference from other files.  Use the expression `assemble_name (STREAM,
   NAME)' to output the name itself; before and after that, output the
   additional assembler syntax for making that name global, and a newline.  */
#define ASM_GLOBALIZE_LABEL(STREAM,NAME)				\
do {									\
  fputs ("\t.globl ", STREAM);						\
  assemble_name (STREAM, NAME);						\
  fputs ("\n", STREAM);							\
} while (0)

/* A C statement (sans semicolon) to output to the stdio stream STREAM some
   commands that will make the label NAME weak; that is, available for
   reference from other files but only used if no other definition is
   available.  Use the expression `assemble_name (STREAM, NAME)' to output the
   name itself; before and after that, output the additional assembler syntax
   for making that name weak, and a newline.

   If you don't define this macro, GNU CC will not support weak symbols and you
   should not define the `SUPPORTS_WEAK' macro.

   Defined in svr4.h.  */
/* #define ASM_WEAKEN_LABEL */

/* A C expression which evaluates to true if the target supports weak symbols.

   If you don't define this macro, `defaults.h' provides a default definition.
   If `ASM_WEAKEN_LABEL' is defined, the default definition is `1'; otherwise,
   it is `0'.  Define this macro if you want to control weak symbol support
   with a compiler flag such as `-melf'.  */
/* #define SUPPORTS_WEAK */

/* A C statement (sans semicolon) to mark DECL to be emitted as a
   public symbol such that extra copies in multiple translation units
   will be discarded by the linker.  Define this macro if your object
   file format provides support for this concept, such as the `COMDAT'
   section flags in the Microsoft Windows PE/COFF format, and this
   support requires changes to DECL, such as putting it in a separate
   section.

   Defined in svr4.h.  */
/* #define MAKE_DECL_ONE_ONLY */

/* A C expression which evaluates to true if the target supports one-only
   semantics.

   If you don't define this macro, `varasm.c' provides a default definition.
   If `MAKE_DECL_ONE_ONLY' is defined, the default definition is `1';
   otherwise, it is `0'.  Define this macro if you want to control one-only
   symbol support with a compiler flag, or if setting the `DECL_ONE_ONLY' flag
   is enough to mark a declaration to be emitted as one-only.  */
/* #define SUPPORTS_ONE_ONLY */

/* A C statement (sans semicolon) to output to the stdio stream STREAM any text
   necessary for declaring the name of an external symbol named NAME which is
   referenced in this compilation but not defined.  The value of DECL is the
   tree node for the declaration.

   This macro need not be defined if it does not need to output anything.  The
   GNU assembler and most Unix assemblers don't require anything.  */
/* #define ASM_OUTPUT_EXTERNAL(STREAM, DECL, NAME) */

/* A C statement (sans semicolon) to output on STREAM an assembler pseudo-op to
   declare a library function name external.  The name of the library function
   is given by SYMREF, which has type `rtx' and is a `symbol_ref'.

   This macro need not be defined if it does not need to output anything.  The
   GNU assembler and most Unix assemblers don't require anything.

   Defined in svr4.h.  */
/* #define ASM_OUTPUT_EXTERNAL_LIBCALL(STREAM, SYMREF) */

/* A C statement (sans semicolon) to output to the stdio stream STREAM a
   reference in assembler syntax to a label named NAME.  This should add `_' to
   the front of the name, if that is customary on your operating system, as it
   is in most Berkeley Unix systems.  This macro is used in `assemble_name'.  */
/* #define ASM_OUTPUT_LABELREF(STREAM, NAME) */

/* A C statement to output to the stdio stream STREAM a label whose name is
   made from the string PREFIX and the number NUM.

   It is absolutely essential that these labels be distinct from the labels
   used for user-level functions and variables.  Otherwise, certain programs
   will have name conflicts with internal labels.

   It is desirable to exclude internal labels from the symbol table of the
   object file.  Most assemblers have a naming convention for labels that
   should be excluded; on many systems, the letter `L' at the beginning of a
   label has this effect.  You should find out what convention your system
   uses, and follow it.

   The usual definition of this macro is as follows:

        fprintf (STREAM, "L%s%d:\n", PREFIX, NUM)

   Defined in svr4.h.  */
/* #define ASM_OUTPUT_INTERNAL_LABEL(STREAM, PREFIX, NUM) */

/* A C statement to store into the string STRING a label whose name is made
   from the string PREFIX and the number NUM.

   This string, when output subsequently by `assemble_name', should produce the
   output that `ASM_OUTPUT_INTERNAL_LABEL' would produce with the same PREFIX
   and NUM.

   If the string begins with `*', then `assemble_name' will output the rest of
   the string unchanged.  It is often convenient for
   `ASM_GENERATE_INTERNAL_LABEL' to use `*' in this way.  If the string doesn't
   start with `*', then `ASM_OUTPUT_LABELREF' gets to output the string, and
   may change it.  (Of course, `ASM_OUTPUT_LABELREF' is also part of your
   machine description, so you should know what it does on your machine.)

   Defined in svr4.h.  */
/* #define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM) */

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
#define ASM_FORMAT_PRIVATE_NAME(OUTVAR, NAME, NUMBER)			\
do {									\
  (OUTVAR) = (char *) alloca (strlen ((NAME)) + 12);			\
  sprintf ((OUTVAR), "%s.%ld", (NAME), (long)(NUMBER));			\
} while (0)

/* A C statement to output to the stdio stream STREAM assembler code which
   defines (equates) the symbol NAME to have the value VALUE.

   If SET_ASM_OP is defined, a default definition is provided which is correct
   for most systems.

   Defined in svr4.h.  */
/* #define ASM_OUTPUT_DEF(STREAM, NAME, VALUE) */

/* A C statement to output to the stdio stream STREAM assembler code which
   defines (equates) the weak symbol NAME to have the value VALUE.

   Define this macro if the target only supports weak aliases; define
   ASM_OUTPUT_DEF instead if possible.  */
/* #define ASM_OUTPUT_WEAK_ALIAS (STREAM, NAME, VALUE) */

/* Define this macro to override the default assembler names used for Objective
   C methods.

   The default name is a unique method number followed by the name of the class
   (e.g. `_1_Foo').  For methods in categories, the name of the category is
   also included in the assembler name (e.g.  `_1_Foo_Bar').

   These names are safe on most systems, but make debugging difficult since the
   method's selector is not present in the name.  Therefore, particular systems
   define other ways of computing names.

   BUF is an expression of type `char *' which gives you a buffer in which to
   store the name; its length is as long as CLASS_NAME, CAT_NAME and SEL_NAME
   put together, plus 50 characters extra.

   The argument IS_INST specifies whether the method is an instance method or a
   class method; CLASS_NAME is the name of the class; CAT_NAME is the name of
   the category (or NULL if the method is not in a category); and SEL_NAME is
   the name of the selector.

   On systems where the assembler can handle quoted names, you can use this
   macro to provide more human-readable names.  */
/* #define OBJC_GEN_METHOD_LABEL(BUF, IS_INST, CLASS_NAME, CAT_NAME, SEL_NAME) */


/* Macros Controlling Initialization Routines.  */

/* If defined, a C string constant for the assembler operation to identify the
   following data as initialization code.  If not defined, GNU CC will assume
   such a section does not exist.  When you are using special sections for
   initialization and termination functions, this macro also controls how
   `crtstuff.c' and `libgcc2.c' arrange to run the initialization functions.

   Defined in svr4.h.  */
/* #define INIT_SECTION_ASM_OP */

/* If defined, `main' will not call `__main' as described above.  This macro
   should be defined for systems that control the contents of the init section
   on a symbol-by-symbol basis, such as OSF/1, and should not be defined
   explicitly for systems that support `INIT_SECTION_ASM_OP'.  */
/* #define HAS_INIT_SECTION */

/* If defined, a C string constant for a switch that tells the linker that the
   following symbol is an initialization routine.  */
/* #define LD_INIT_SWITCH */

/* If defined, a C string constant for a switch that tells the linker that the
   following symbol is a finalization routine.  */
/* #define LD_FINI_SWITCH */

/* If defined, `main' will call `__main' despite the presence of
   `INIT_SECTION_ASM_OP'.  This macro should be defined for systems where the
   init section is not actually run automatically, but is still useful for
   collecting the lists of constructors and destructors.  */
/* #define INVOKE__main */

/* Define this macro as a C statement to output on the stream STREAM the
   assembler code to arrange to call the function named NAME at initialization
   time.

   Assume that NAME is the name of a C function generated automatically by the
   compiler.  This function takes no arguments.  Use the function
   `assemble_name' to output the name NAME; this performs any system-specific
   syntactic transformations such as adding an underscore.

   If you don't define this macro, nothing special is output to arrange to call
   the function.  This is correct when the function will be called in some
   other manner--for example, by means of the `collect2' program, which looks
   through the symbol table to find these functions by their names.

   Defined in svr4.h.  */
/* #define ASM_OUTPUT_CONSTRUCTOR(STREAM, NAME) */

/* This is like `ASM_OUTPUT_CONSTRUCTOR' but used for termination functions
   rather than initialization functions.

   Defined in svr4.h.  */
/* #define ASM_OUTPUT_DESTRUCTOR(STREAM, NAME) */

/* If your system uses `collect2' as the means of processing constructors, then
   that program normally uses `nm' to scan an object file for constructor
   functions to be called.  On certain kinds of systems, you can define these
   macros to make `collect2' work faster (and, in some cases, make it work at
   all): */

/* Define this macro if the system uses COFF (Common Object File Format) object
   files, so that `collect2' can assume this format and scan object files
   directly for dynamic constructor/destructor functions.  */
/* #define OBJECT_FORMAT_COFF */

/* Define this macro if the system uses ROSE format object files, so that
   `collect2' can assume this format and scan object files directly for dynamic
   constructor/destructor functions.

   These macros are effective only in a native compiler; `collect2' as
   part of a cross compiler always uses `nm' for the target machine.  */
/* #define OBJECT_FORMAT_ROSE */

/* Define this macro if the system uses ELF format object files.

   Defined in svr4.h.  */
/* #define OBJECT_FORMAT_ELF */

/* Define this macro as a C string constant containing the file name to use to
   execute `nm'.  The default is to search the path normally for `nm'.

   If your system supports shared libraries and has a program to list the
   dynamic dependencies of a given library or executable, you can define these
   macros to enable support for running initialization and termination
   functions in shared libraries: */
/* #define REAL_NM_FILE_NAME */

/* Define this macro to a C string constant containing the name of the program
   which lists dynamic dependencies, like `"ldd"' under SunOS 4.  */
/* #define LDD_SUFFIX */

/* Define this macro to be C code that extracts filenames from the output of
   the program denoted by `LDD_SUFFIX'.  PTR is a variable of type `char *'
   that points to the beginning of a line of output from `LDD_SUFFIX'.  If the
   line lists a dynamic dependency, the code must advance PTR to the beginning
   of the filename on that line.  Otherwise, it must set PTR to `NULL'.  */
/* #define PARSE_LDD_OUTPUT (PTR) */


/* Output of Assembler Instructions.  */

/* A C initializer containing the assembler's names for the machine registers,
   each one as a C string constant.  This is what translates register numbers
   in the compiler into assembler language.  */
#define REGISTER_NAMES							\
{ "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9", "r10",	\
  "r11", "r12", "r13", "psw", "sp", "carry", "fp", "ap" }

/* If defined, a C initializer for an array of structures containing a name and
   a register number.  This macro defines additional names for hard registers,
   thus allowing the `asm' option in declarations to refer to registers using
   alternate names.  */
#define ADDITIONAL_REGISTER_NAMES		\
  { { "r14", 14 },				\
    { "r15", 15 } }

/* Define this macro if you are using an unusual assembler that requires
   different names for the machine instructions.

   The definition is a C statement or statements which output an assembler
   instruction opcode to the stdio stream STREAM.  The macro-operand PTR is a
   variable of type `char *' which points to the opcode name in its "internal"
   form--the form that is written in the machine description.  The definition
   should output the opcode name to STREAM, performing any translation you
   desire, and increment the variable PTR to point at the end of the opcode so
   that it will not be output twice.

   In fact, your macro definition may process less than the entire opcode name,
   or more than the opcode name; but if you want to process text that includes
   `%'-sequences to substitute operands, you must take care of the substitution
   yourself.  Just be sure to increment PTR over whatever text should not be
   output normally.

   If you need to look at the operand values, they can be found as the elements
   of `recog_data.operand'.

   If the macro definition does nothing, the instruction is output in the usual
   way.  */
/* #define ASM_OUTPUT_OPCODE(STREAM, PTR) */

/* If defined, a C statement to be executed just prior to the output of
   assembler code for INSN, to modify the extracted operands so they will be
   output differently.

   Here the argument OPVEC is the vector containing the operands extracted from
   INSN, and NOPERANDS is the number of elements of the vector which contain
   meaningful data for this insn.  The contents of this vector are what will be
   used to convert the insn template into assembler code, so you can change the
   assembler output by changing the contents of the vector.

   This macro is useful when various assembler syntaxes share a single file of
   instruction patterns; by defining this macro differently, you can cause a
   large class of instructions to be output differently (such as with
   rearranged operands).  Naturally, variations in assembler syntax affecting
   individual insn patterns ought to be handled by writing conditional output
   routines in those patterns.

   If this macro is not defined, it is equivalent to a null statement.  */
/* #define FINAL_PRESCAN_INSN(INSN, OPVEC, NOPERANDS) */

/* If defined, `FINAL_PRESCAN_INSN' will be called on each
   `CODE_LABEL'.  In that case, OPVEC will be a null pointer and
   NOPERANDS will be zero.  */
/* #define FINAL_PRESCAN_LABEL */

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
#define PRINT_OPERAND(STREAM, X, CODE) xstormy16_print_operand (STREAM, X, CODE)

/* A C expression which evaluates to true if CODE is a valid punctuation
   character for use in the `PRINT_OPERAND' macro.  If
   `PRINT_OPERAND_PUNCT_VALID_P' is not defined, it means that no punctuation
   characters (except for the standard one, `%') are used in this way.  */
/* #define PRINT_OPERAND_PUNCT_VALID_P(CODE) */

/* A C compound statement to output to stdio stream STREAM the assembler syntax
   for an instruction operand that is a memory reference whose address is X.  X
   is an RTL expression.

   On some machines, the syntax for a symbolic address depends on the section
   that the address refers to.  On these machines, define the macro
   `ENCODE_SECTION_INFO' to store the information into the `symbol_ref', and
   then check for it here.

   This declaration must be present.  */
#define PRINT_OPERAND_ADDRESS(STREAM, X) xstormy16_print_operand_address (STREAM, X)

/* A C statement, to be executed after all slot-filler instructions have been
   output.  If necessary, call `dbr_sequence_length' to determine the number of
   slots filled in a sequence (zero if not currently outputting a sequence), to
   decide how many no-ops to output, or whatever.

   Don't define this macro if it has nothing to do, but it is helpful in
   reading assembly output if the extent of the delay sequence is made explicit
   (e.g. with white space).

   Note that output routines for instructions with delay slots must be prepared
   to deal with not being output as part of a sequence (i.e.  when the
   scheduling pass is not run, or when no slot fillers could be found.)  The
   variable `final_sequence' is null when not processing a sequence, otherwise
   it contains the `sequence' rtx being output.  */
/* #define DBR_OUTPUT_SEQEND(FILE) */

/* If defined, C string expressions to be used for the `%R', `%L', `%U', and
   `%I' options of `asm_fprintf' (see `final.c').  These are useful when a
   single `md' file must support multiple assembler formats.  In that case, the
   various `tm.h' files can define these macros differently.

   USER_LABEL_PREFIX is defined in svr4.h.  */
#define REGISTER_PREFIX ""
#define LOCAL_LABEL_PREFIX "."
#define USER_LABEL_PREFIX ""
#define IMMEDIATE_PREFIX "#"

/* If your target supports multiple dialects of assembler language (such as
   different opcodes), define this macro as a C expression that gives the
   numeric index of the assembler language dialect to use, with zero as the
   first variant.

   If this macro is defined, you may use `{option0|option1|option2...}'
   constructs in the output templates of patterns or in the first argument of
   `asm_fprintf'.  This construct outputs `option0', `option1' or `option2',
   etc., if the value of `ASSEMBLER_DIALECT' is zero, one or two, etc.  Any
   special characters within these strings retain their usual meaning.

   If you do not define this macro, the characters `{', `|' and `}' do not have
   any special meaning when used in templates or operands to `asm_fprintf'.

   Define the macros `REGISTER_PREFIX', `LOCAL_LABEL_PREFIX',
   `USER_LABEL_PREFIX' and `IMMEDIATE_PREFIX' if you can express the variations
   in assemble language syntax with that mechanism.  Define `ASSEMBLER_DIALECT'
   and use the `{option0|option1}' syntax if the syntax variant are larger and
   involve such things as different opcodes or operand order.  */
/* #define ASSEMBLER_DIALECT */

/* A C expression to output to STREAM some assembler code which will push hard
   register number REGNO onto the stack.  The code need not be optimal, since
   this macro is used only when profiling.  */
#define ASM_OUTPUT_REG_PUSH(STREAM, REGNO) \
  fprintf (STREAM, "\tpush %d\n", REGNO)

/* A C expression to output to STREAM some assembler code which will pop hard
   register number REGNO off of the stack.  The code need not be optimal, since
   this macro is used only when profiling.  */
#define ASM_OUTPUT_REG_POP(STREAM, REGNO) \
  fprintf (STREAM, "\tpop %d\n", REGNO)


/* Output of dispatch tables.  */

/* This port does not use the ASM_OUTPUT_ADDR_VEC_ELT macro, because
   this could cause label alignment to appear between the 'br' and the table,
   which would be bad.  Instead, it controls the output of the table
   itself.  */
#define ASM_OUTPUT_ADDR_VEC(LABEL, BODY) \
  xstormy16_output_addr_vec (file, LABEL, BODY)

/* Alignment for ADDR_VECs is the same as for code.  */
#define ADDR_VEC_ALIGN(ADDR_VEC) 1


/* Assembler Commands for Exception Regions.  */

/* An rtx used to mask the return address found via RETURN_ADDR_RTX, so that it
   does not contain any extraneous set bits in it.  */
/* #define MASK_RETURN_ADDR */

/* Define this macro to 0 if your target supports DWARF 2 frame unwind
   information, but it does not yet work with exception handling.  Otherwise,
   if your target supports this information (if it defines
   `INCOMING_RETURN_ADDR_RTX'), GCC will provide a default definition of 1.

   If this macro is defined to 1, the DWARF 2 unwinder will be the default
   exception handling mechanism; otherwise, setjmp/longjmp will be used by
   default.

   If this macro is defined to anything, the DWARF 2 unwinder will be used
   instead of inline unwinders and __unwind_function in the non-setjmp case.  */
#define DWARF2_UNWIND_INFO 0

/* Don't use __builtin_setjmp for unwinding, since it's tricky to get
   at the high 16 bits of an address.  */
#define DONT_USE_BUILTIN_SETJMP
#define JMP_BUF_SIZE  8

/* Assembler Commands for Alignment.  */

/* The alignment (log base 2) to put in front of LABEL, which follows
   a BARRIER.

   This macro need not be defined if you don't want any special alignment to be
   done at such a time.  Most machine descriptions do not currently define the
   macro.  */
/* #define LABEL_ALIGN_AFTER_BARRIER(LABEL) */

/* The desired alignment for the location counter at the beginning
   of a loop.

   This macro need not be defined if you don't want any special alignment to be
   done at such a time.  Most machine descriptions do not currently define the
   macro.  */
/* #define LOOP_ALIGN(LABEL) */

/* A C statement to output to the stdio stream STREAM an assembler instruction
   to advance the location counter by NBYTES bytes.  Those bytes should be zero
   when loaded.  NBYTES will be a C expression of type `int'.

   Defined in elfos.h.  */
/* #define ASM_OUTPUT_SKIP(STREAM, NBYTES) */

/* Define this macro if `ASM_OUTPUT_SKIP' should not be used in the text
   section because it fails put zeros in the bytes that are skipped.  This is
   true on many Unix systems, where the pseudo-op to skip bytes produces no-op
   instructions rather than zeros when used in the text section.  */
/* #define ASM_NO_SKIP_IN_TEXT */

/* A C statement to output to the stdio stream STREAM an assembler command to
   advance the location counter to a multiple of 2 to the POWER bytes.  POWER
   will be a C expression of type `int'.  */
#define ASM_OUTPUT_ALIGN(STREAM, POWER) \
  fprintf ((STREAM), "\t.p2align %d\n", (POWER))


/* Macros Affecting all Debug Formats.  */

/* A C expression that returns the integer offset value for an automatic
   variable having address X (an RTL expression).  The default computation
   assumes that X is based on the frame-pointer and gives the offset from the
   frame-pointer.  This is required for targets that produce debugging output
   for DBX or COFF-style debugging output for SDB and allow the frame-pointer
   to be eliminated when the `-g' options is used.  */
/* #define DEBUGGER_AUTO_OFFSET(X) */

/* A C expression that returns the integer offset value for an argument having
   address X (an RTL expression).  The nominal offset is OFFSET.  */
/* #define DEBUGGER_ARG_OFFSET(OFFSET, X) */

/* A C expression that returns the type of debugging output GNU CC produces
   when the user specifies `-g' or `-ggdb'.  Define this if you have arranged
   for GNU CC to support more than one format of debugging output.  Currently,
   the allowable values are `DBX_DEBUG', `SDB_DEBUG', `DWARF_DEBUG',
   `DWARF2_DEBUG', and `XCOFF_DEBUG'.

   The value of this macro only affects the default debugging output; the user
   can always get a specific type of output by using `-gstabs', `-gcoff',
   `-gdwarf-1', `-gdwarf-2', or `-gxcoff'.

   Defined in svr4.h.  */
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG


/* Specific Options for DBX Output.  */

/* Define this macro if GNU CC should produce debugging output for DBX in
   response to the `-g' option.

   Defined in svr4.h.  */
/* #define DBX_DEBUGGING_INFO */

/* Define this macro if GNU CC should produce XCOFF format debugging output in
   response to the `-g' option.  This is a variant of DBX format.  */
/* #define XCOFF_DEBUGGING_INFO */

/* Define this macro to control whether GNU CC should by default generate GDB's
   extended version of DBX debugging information (assuming DBX-format debugging
   information is enabled at all).  If you don't define the macro, the default
   is 1: always generate the extended information if there is any occasion to.  */
/* #define DEFAULT_GDB_EXTENSIONS */

/* Define this macro if all `.stabs' commands should be output while in the
   text section.  */
/* #define DEBUG_SYMS_TEXT */

/* A C string constant naming the assembler pseudo op to use instead of
   `.stabs' to define an ordinary debugging symbol.  If you don't define this
   macro, `.stabs' is used.  This macro applies only to DBX debugging
   information format.  */
/* #define ASM_STABS_OP */

/* A C string constant naming the assembler pseudo op to use instead of
   `.stabd' to define a debugging symbol whose value is the current location.
   If you don't define this macro, `.stabd' is used.  This macro applies only
   to DBX debugging information format.  */
/* #define ASM_STABD_OP */

/* A C string constant naming the assembler pseudo op to use instead of
   `.stabn' to define a debugging symbol with no name.  If you don't define
   this macro, `.stabn' is used.  This macro applies only to DBX debugging
   information format.  */
/* #define ASM_STABN_OP */

/* Define this macro if DBX on your system does not support the construct
   `xsTAGNAME'.  On some systems, this construct is used to describe a forward
   reference to a structure named TAGNAME.  On other systems, this construct is
   not supported at all.  */
/* #define DBX_NO_XREFS */

/* A symbol name in DBX-format debugging information is normally continued
   (split into two separate `.stabs' directives) when it exceeds a certain
   length (by default, 80 characters).  On some operating systems, DBX requires
   this splitting; on others, splitting must not be done.  You can inhibit
   splitting by defining this macro with the value zero.  You can override the
   default splitting-length by defining this macro as an expression for the
   length you desire.  */
/* #define DBX_CONTIN_LENGTH */

/* Normally continuation is indicated by adding a `\' character to the end of a
   `.stabs' string when a continuation follows.  To use a different character
   instead, define this macro as a character constant for the character you
   want to use.  Do not define this macro if backslash is correct for your
   system.  */
/* #define DBX_CONTIN_CHAR */

/* Define this macro if it is necessary to go to the data section before
   outputting the `.stabs' pseudo-op for a non-global static variable.  */
/* #define DBX_STATIC_STAB_DATA_SECTION */

/* The value to use in the "code" field of the `.stabs' directive for a
   typedef.  The default is `N_LSYM'.  */
/* #define DBX_TYPE_DECL_STABS_CODE */

/* The value to use in the "code" field of the `.stabs' directive for a static
   variable located in the text section.  DBX format does not provide any
   "right" way to do this.  The default is `N_FUN'.  */
/* #define DBX_STATIC_CONST_VAR_CODE */

/* The value to use in the "code" field of the `.stabs' directive for a
   parameter passed in registers.  DBX format does not provide any "right" way
   to do this.  The default is `N_RSYM'.  */
/* #define DBX_REGPARM_STABS_CODE */

/* The letter to use in DBX symbol data to identify a symbol as a parameter
   passed in registers.  DBX format does not customarily provide any way to do
   this.  The default is `'P''.  */
/* #define DBX_REGPARM_STABS_LETTER */

/* The letter to use in DBX symbol data to identify a symbol as a stack
   parameter.  The default is `'p''.  */
/* #define DBX_MEMPARM_STABS_LETTER */

/* Define this macro if the DBX information for a function and its arguments
   should precede the assembler code for the function.  Normally, in DBX
   format, the debugging information entirely follows the assembler code.

   Defined in svr4.h.  */
/* #define DBX_FUNCTION_FIRST */

/* Define this macro if the `N_LBRAC' symbol for a block should precede the
   debugging information for variables and functions defined in that block.
   Normally, in DBX format, the `N_LBRAC' symbol comes first.  */
/* #define DBX_LBRAC_FIRST */

/* Define this macro if the value of a symbol describing the scope of a block
   (`N_LBRAC' or `N_RBRAC') should be relative to the start of the enclosing
   function.  Normally, GNU C uses an absolute address.

   Defined in svr4.h.  */
/* #define DBX_BLOCKS_FUNCTION_RELATIVE */

/* Define this macro if GNU C should generate `N_BINCL' and `N_EINCL'
   stabs for included header files, as on Sun systems.  This macro
   also directs GNU C to output a type number as a pair of a file
   number and a type number within the file.  Normally, GNU C does not
   generate `N_BINCL' or `N_EINCL' stabs, and it outputs a single
   number for a type number.  */
/* #define DBX_USE_BINCL */


/* Open ended Hooks for DBX Output.  */

/* Define this macro to say how to output to STREAM the debugging information
   for the start of a scope level for variable names.  The argument NAME is the
   name of an assembler symbol (for use with `assemble_name') whose value is
   the address where the scope begins.  */
/* #define DBX_OUTPUT_LBRAC(STREAM, NAME) */

/* Like `DBX_OUTPUT_LBRAC', but for the end of a scope level.  */
/* #define DBX_OUTPUT_RBRAC(STREAM, NAME) */

/* Define this macro if the target machine requires special handling to output
   an enumeration type.  The definition should be a C statement (sans
   semicolon) to output the appropriate information to STREAM for the type
   TYPE.  */
/* #define DBX_OUTPUT_ENUM(STREAM, TYPE) */

/* Define this macro if the target machine requires special output at the end
   of the debugging information for a function.  The definition should be a C
   statement (sans semicolon) to output the appropriate information to STREAM.
   FUNCTION is the `FUNCTION_DECL' node for the function.  */
/* #define DBX_OUTPUT_FUNCTION_END(STREAM, FUNCTION) */

/* Define this macro if you need to control the order of output of the standard
   data types at the beginning of compilation.  The argument SYMS is a `tree'
   which is a chain of all the predefined global symbols, including names of
   data types.

   Normally, DBX output starts with definitions of the types for integers and
   characters, followed by all the other predefined types of the particular
   language in no particular order.

   On some machines, it is necessary to output different particular types
   first.  To do this, define `DBX_OUTPUT_STANDARD_TYPES' to output those
   symbols in the necessary order.  Any predefined types that you don't
   explicitly output will be output afterward in no particular order.

   Be careful not to define this macro so that it works only for C.  There are
   no global variables to access most of the built-in types, because another
   language may have another set of types.  The way to output a particular type
   is to look through SYMS to see if you can find it.  Here is an example:

        {
          tree decl;
          for (decl = syms; decl; decl = TREE_CHAIN (decl))
            if (!strcmp (IDENTIFIER_POINTER (DECL_NAME (decl)),
                         "long int"))
              dbxout_symbol (decl);
          ...
        }

   This does nothing if the expected type does not exist.

   See the function `init_decl_processing' in `c-decl.c' to find the names to
   use for all the built-in C types.  */
/* #define DBX_OUTPUT_STANDARD_TYPES(SYMS) */

/* Some stabs encapsulation formats (in particular ECOFF), cannot
   handle the `.stabs "",N_FUN,,0,0,Lscope-function-1' gdb dbx
   extension construct.  On those machines, define this macro to turn
   this feature off without disturbing the rest of the gdb extensions.  */
/* #define NO_DBX_FUNCTION_END */


/* File names in DBX format.  */

/* Define this if DBX wants to have the current directory recorded in each
   object file.

   Note that the working directory is always recorded if GDB extensions are
   enabled.  */
/* #define DBX_WORKING_DIRECTORY */

/* A C statement to output DBX debugging information to the stdio stream STREAM
   which indicates that file NAME is the main source file--the file specified
   as the input file for compilation.  This macro is called only once, at the
   beginning of compilation.

   This macro need not be defined if the standard form of output for DBX
   debugging information is appropriate.

   Defined in svr4.h.  */
/* #define DBX_OUTPUT_MAIN_SOURCE_FILENAME(STREAM, NAME) */

/* A C statement to output DBX debugging information to the stdio stream STREAM
   which indicates that the current directory during compilation is named NAME.

   This macro need not be defined if the standard form of output for DBX
   debugging information is appropriate.  */
/* #define DBX_OUTPUT_MAIN_SOURCE_DIRECTORY(STREAM, NAME) */

/* A C statement to output DBX debugging information at the end of compilation
   of the main source file NAME.

   If you don't define this macro, nothing special is output at the end of
   compilation, which is correct for most machines.  */
/* #define DBX_OUTPUT_MAIN_SOURCE_FILE_END(STREAM, NAME) */

/* A C statement to output DBX debugging information to the stdio stream STREAM
   which indicates that file NAME is the current source file.  This output is
   generated each time input shifts to a different source file as a result of
   `#include', the end of an included file, or a `#line' command.

   This macro need not be defined if the standard form of output for DBX
   debugging information is appropriate.  */
/* #define DBX_OUTPUT_SOURCE_FILENAME(STREAM, NAME) */


/* Macros for SDB and Dwarf Output.  */

/* Define this macro if GNU CC should produce COFF-style debugging output for
   SDB in response to the `-g' option.  */
/* #define SDB_DEBUGGING_INFO */

/* Define this macro if GNU CC should produce dwarf format debugging output in
   response to the `-g' option.

   Defined in svr4.h.  */
/* #define DWARF_DEBUGGING_INFO */

/* Define this macro if GNU CC should produce dwarf version 2 format debugging
   output in response to the `-g' option.

   To support optional call frame debugging information, you must also define
   `INCOMING_RETURN_ADDR_RTX' and either set `RTX_FRAME_RELATED_P' on the
   prologue insns if you use RTL for the prologue, or call `dwarf2out_def_cfa'
   and `dwarf2out_reg_save' as appropriate from `TARGET_ASM_FUNCTION_PROLOGUE'
   if you don't.

   Defined in svr4.h.  */
/* #define DWARF2_DEBUGGING_INFO */

/* Define this macro if GNU CC should produce dwarf version 2-style
   line numbers.  This usually requires extending the assembler to
   support them, and #defining DWARF2_LINE_MIN_INSN_LENGTH in the
   assembler configuration header files.  */
/* #define DWARF2_ASM_LINE_DEBUG_INFO 1 */

/* Define this macro if addresses in Dwarf 2 debugging info should not
   be the same size as pointers on the target architecture.  The
   macro's value should be the size, in bytes, to use for addresses in
   the debugging info.

   Some architectures use word addresses to refer to code locations,
   but Dwarf 2 info always uses byte addresses.  On such machines,
   Dwarf 2 addresses need to be larger than the architecture's
   pointers.  */
#define DWARF2_ADDR_SIZE 4

/* Define these macros to override the assembler syntax for the special SDB
   assembler directives.  See `sdbout.c' for a list of these macros and their
   arguments.  If the standard syntax is used, you need not define them
   yourself.  */
/* #define PUT_SDB_...  */

/* Some assemblers do not support a semicolon as a delimiter, even between SDB
   assembler directives.  In that case, define this macro to be the delimiter
   to use (usually `\n').  It is not necessary to define a new set of
   `PUT_SDB_OP' macros if this is the only change required.  */
/* #define SDB_DELIM */

/* Define this macro to override the usual method of constructing a dummy name
   for anonymous structure and union types.  See `sdbout.c' for more
   information.  */
/* #define SDB_GENERATE_FAKE */

/* Define this macro to allow references to unknown structure, union, or
   enumeration tags to be emitted.  Standard COFF does not allow handling of
   unknown references, MIPS ECOFF has support for it.  */
/* #define SDB_ALLOW_UNKNOWN_REFERENCES */

/* Define this macro to allow references to structure, union, or enumeration
   tags that have not yet been seen to be handled.  Some assemblers choke if
   forward tags are used, while some require it.  */
/* #define SDB_ALLOW_FORWARD_REFERENCES */


/* Miscellaneous Parameters.  */

/* Define REAL_ARITHMETIC to use a software emulator for the target floating
   point mode.  Otherwise the host floating point mode is used.  */
#define REAL_ARITHMETIC

/* Define this if you have defined special-purpose predicates in the file
   `MACHINE.c'.  This macro is called within an initializer of an array of
   structures.  The first field in the structure is the name of a predicate and
   the second field is an array of rtl codes.  For each predicate, list all rtl
   codes that can be in expressions matched by the predicate.  The list should
   have a trailing comma.  Here is an example of two entries in the list for a
   typical RISC machine:

        #define PREDICATE_CODES \
          {"gen_reg_rtx_operand", {SUBREG, REG}},  \
          {"reg_or_short_cint_operand", {SUBREG, REG, CONST_INT}},

   Defining this macro does not affect the generated code (however, incorrect
   definitions that omit an rtl code that may be matched by the predicate can
   cause the compiler to malfunction).  Instead, it allows the table built by
   `genrecog' to be more compact and efficient, thus speeding up the compiler.
   The most important predicates to include in the list specified by this macro
   are thoses used in the most insn patterns.  */
#define PREDICATE_CODES					\
  {"shift_operator", {ASHIFT, ASHIFTRT, LSHIFTRT }},	\
  {"equality_operator", {EQ, NE }},			\
  {"inequality_operator", {GE, GT, LE, LT, GEU, GTU, LEU, LTU }}, \
  {"xstormy16_ineqsi_operator", {LT, GE, LTU, GEU }}, \
  {"nonimmediate_nonstack_operand", {REG, MEM}},
/* An alias for a machine mode name.  This is the machine mode that elements of
   a jump-table should have.  */
#define CASE_VECTOR_MODE SImode

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses.  */
/* #define CASE_VECTOR_PC_RELATIVE 1 */

/* Define this if control falls through a `case' insn when the index value is
   out of range.  This means the specified default-label is actually ignored by
   the `case' insn proper.  */
/* #define CASE_DROPS_THROUGH */

/* Define this to be the smallest number of different values for which it is
   best to use a jump-table instead of a tree of conditional branches.  The
   default is four for machines with a `casesi' instruction and five otherwise.
   This is best for most machines.  */
/* #define CASE_VALUES_THRESHOLD */

/* Define this macro if operations between registers with integral mode smaller
   than a word are always performed on the entire register.  Most RISC machines
   have this property and most CISC machines do not.  */
#define WORD_REGISTER_OPERATIONS

/* Define this macro to be a C expression indicating when insns that read
   memory in MODE, an integral mode narrower than a word, set the bits outside
   of MODE to be either the sign-extension or the zero-extension of the data
   read.  Return `SIGN_EXTEND' for values of MODE for which the insn
   sign-extends, `ZERO_EXTEND' for which it zero-extends, and `NIL' for other
   modes.

   This macro is not called with MODE non-integral or with a width greater than
   or equal to `BITS_PER_WORD', so you may return any value in this case.  Do
   not define this macro if it would always return `NIL'.  On machines where
   this macro is defined, you will normally define it as the constant
   `SIGN_EXTEND' or `ZERO_EXTEND'.  */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* Define if loading short immediate values into registers sign extends.  */
/* #define SHORT_IMMEDIATES_SIGN_EXTEND */

/* Define this macro if the same instructions that convert a floating point
   number to a signed fixed point number also convert validly to an unsigned
   one.  */
/* #define FIXUNS_TRUNC_LIKE_FIX_TRUNC */

/* The maximum number of bytes that a single instruction can move quickly from
   memory to memory.  */
#define MOVE_MAX 2

/* The maximum number of bytes that a single instruction can move quickly from
   memory to memory.  If this is undefined, the default is `MOVE_MAX'.
   Otherwise, it is the constant value that is the largest value that
   `MOVE_MAX' can have at run-time.  */
/* #define MAX_MOVE_MAX */

/* A C expression that is nonzero if on this machine the number of bits
   actually used for the count of a shift operation is equal to the number of
   bits needed to represent the size of the object being shifted.  When this
   macro is non-zero, the compiler will assume that it is safe to omit a
   sign-extend, zero-extend, and certain bitwise `and' instructions that
   truncates the count of a shift operation.  On machines that have
   instructions that act on bitfields at variable positions, which may include
   `bit test' instructions, a nonzero `SHIFT_COUNT_TRUNCATED' also enables
   deletion of truncations of the values that serve as arguments to bitfield
   instructions.

   If both types of instructions truncate the count (for shifts) and position
   (for bitfield operations), or if no variable-position bitfield instructions
   exist, you should define this macro.

   However, on some machines, such as the 80386 and the 680x0, truncation only
   applies to shift operations and not the (real or pretended) bitfield
   operations.  Define `SHIFT_COUNT_TRUNCATED' to be zero on such machines.
   Instead, add patterns to the `md' file that include the implied truncation
   of the shift instructions.

   You need not define this macro if it would always have the value of zero.  */
#define SHIFT_COUNT_TRUNCATED 1

/* A C expression which is nonzero if on this machine it is safe to "convert"
   an integer of INPREC bits to one of OUTPREC bits (where OUTPREC is smaller
   than INPREC) by merely operating on it as if it had only OUTPREC bits.

   On many machines, this expression can be 1.

   When `TRULY_NOOP_TRUNCATION' returns 1 for a pair of sizes for modes for
   which `MODES_TIEABLE_P' is 0, suboptimal code can result.  If this is the
   case, making `TRULY_NOOP_TRUNCATION' return 0 in such cases may improve
   things.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* A C expression describing the value returned by a comparison operator with
   an integral mode and stored by a store-flag instruction (`sCOND') when the
   condition is true.  This description must apply to *all* the `sCOND'
   patterns and all the comparison operators whose results have a `MODE_INT'
   mode.

   A value of 1 or -1 means that the instruction implementing the comparison
   operator returns exactly 1 or -1 when the comparison is true and 0 when the
   comparison is false.  Otherwise, the value indicates which bits of the
   result are guaranteed to be 1 when the comparison is true.  This value is
   interpreted in the mode of the comparison operation, which is given by the
   mode of the first operand in the `sCOND' pattern.  Either the low bit or the
   sign bit of `STORE_FLAG_VALUE' be on.  Presently, only those bits are used
   by the compiler.

   If `STORE_FLAG_VALUE' is neither 1 or -1, the compiler will generate code
   that depends only on the specified bits.  It can also replace comparison
   operators with equivalent operations if they cause the required bits to be
   set, even if the remaining bits are undefined.  For example, on a machine
   whose comparison operators return an `SImode' value and where
   `STORE_FLAG_VALUE' is defined as `0x80000000', saying that just the sign bit
   is relevant, the expression

        (ne:SI (and:SI X (const_int POWER-OF-2)) (const_int 0))

   can be converted to

        (ashift:SI X (const_int N))

   where N is the appropriate shift count to move the bit being tested into the
   sign bit.

   There is no way to describe a machine that always sets the low-order bit for
   a true value, but does not guarantee the value of any other bits, but we do
   not know of any machine that has such an instruction.  If you are trying to
   port GNU CC to such a machine, include an instruction to perform a
   logical-and of the result with 1 in the pattern for the comparison operators
   and let us know.

   Often, a machine will have multiple instructions that obtain a value from a
   comparison (or the condition codes).  Here are rules to guide the choice of
   value for `STORE_FLAG_VALUE', and hence the instructions to be used:

      * Use the shortest sequence that yields a valid definition for
        `STORE_FLAG_VALUE'.  It is more efficient for the compiler to
        "normalize" the value (convert it to, e.g., 1 or 0) than for
        the comparison operators to do so because there may be
        opportunities to combine the normalization with other
        operations.

      * For equal-length sequences, use a value of 1 or -1, with -1
        being slightly preferred on machines with expensive jumps and
        1 preferred on other machines.

      * As a second choice, choose a value of `0x80000001' if
        instructions exist that set both the sign and low-order bits
        but do not define the others.

      * Otherwise, use a value of `0x80000000'.

   Many machines can produce both the value chosen for `STORE_FLAG_VALUE' and
   its negation in the same number of instructions.  On those machines, you
   should also define a pattern for those cases, e.g., one matching

        (set A (neg:M (ne:M B C)))

   Some machines can also perform `and' or `plus' operations on condition code
   values with less instructions than the corresponding `sCOND' insn followed
   by `and' or `plus'.  On those machines, define the appropriate patterns.
   Use the names `incscc' and `decscc', respectively, for the the patterns
   which perform `plus' or `minus' operations on condition code values.  See
   `rs6000.md' for some examples.  The GNU Superoptizer can be used to find
   such instruction sequences on other machines.

   You need not define `STORE_FLAG_VALUE' if the machine has no store-flag
   instructions.  */
/* #define STORE_FLAG_VALUE */

/* A C expression that gives a non-zero floating point value that is returned
   when comparison operators with floating-point results are true.  Define this
   macro on machine that have comparison operations that return floating-point
   values.  If there are no such operations, do not define this macro.  */
/* #define FLOAT_STORE_FLAG_VALUE */

/* An alias for the machine mode for pointers.  On most machines, define this
   to be the integer mode corresponding to the width of a hardware pointer;
   `SImode' on 32-bit machine or `DImode' on 64-bit machines.  On some machines
   you must define this to be one of the partial integer modes, such as
   `PSImode'.

   The width of `Pmode' must be at least as large as the value of
   `POINTER_SIZE'.  If it is not equal, you must define the macro
   `POINTERS_EXTEND_UNSIGNED' to specify how pointers are extended to `Pmode'.  */
#define Pmode HImode

/* An alias for the machine mode used for memory references to functions being
   called, in `call' RTL expressions.  On most machines this should be
   `QImode'.  */
#define FUNCTION_MODE HImode

/* A C expression for the maximum number of instructions above which the
   function DECL should not be inlined.  DECL is a `FUNCTION_DECL' node.

   The default definition of this macro is 64 plus 8 times the number of
   arguments that the function accepts.  Some people think a larger threshold
   should be used on RISC machines.  */
/* #define INTEGRATE_THRESHOLD(DECL) */

/* Define this if the preprocessor should ignore `#sccs' directives and print
   no error message.

   Defined in svr4.h.  */
/* #define SCCS_DIRECTIVE */

/* Define this macro if the system header files support C++ as well as C.  This
   macro inhibits the usual method of using system header files in C++, which
   is to pretend that the file's contents are enclosed in `extern "C" {...}'.  */
#define NO_IMPLICIT_EXTERN_C

/* Define this macro if you want to implement any pragmas.  If defined, it
   should be a C expression to be executed when #pragma is seen.  The
   argument GETC is a function which will return the next character in the
   input stream, or EOF if no characters are left.  The argument UNGETC is
   a function which will push a character back into the input stream.  The
   argument NAME is the word following #pragma in the input stream.  The input
   stream pointer will be pointing just beyond the end of this word.  The
   expression should return true if it handled the pragma, false otherwise.
   The input stream should be left undistrubed if false is returned, otherwise
   it should be pointing at the next character after the end of the pragma.
   Any characters left between the end of the pragma and the end of the line will
   be ignored.
   
   It is generally a bad idea to implement new uses of `#pragma'.  The only
   reason to define this macro is for compatibility with other compilers that
   do support `#pragma' for the sake of any user programs which already use it.  */
/* #define HANDLE_PRAGMA(GETC, UNGETC, NAME) handle_pragma (GETC, UNGETC, NAME) */

/* Define this macro to handle System V style pragmas: #pragma pack and
   #pragma weak.  Note, #pragma weak will only be supported if SUPPORT_WEAK is
   defined.

   Defined in svr4.h.  */
#define HANDLE_SYSV_PRAGMA

/* Define this macro if you want to support the Win32 style pragmas
   #pragma pack(push,<n>) and #pragma pack(pop).  */
/* HANDLE_PRAGMA_PACK_PUSH_POP 1 */
   
/* Define this macro to control use of the character `$' in identifier names.
   The value should be 0, 1, or 2.  0 means `$' is not allowed by default; 1
   means it is allowed by default if `-traditional' is used; 2 means it is
   allowed by default provided `-ansi' is not used.  1 is the default; there is
   no need to define this macro in that case.  */
/* #define DOLLARS_IN_IDENTIFIERS */

/* Define this macro if the assembler does not accept the character `$' in
   label names.  By default constructors and destructors in G++ have `$' in the
   identifiers.  If this macro is defined, `.' is used instead.

   Defined in svr4.h.  */
/* #define NO_DOLLAR_IN_LABEL */

/* Define this macro if the assembler does not accept the character `.' in
   label names.  By default constructors and destructors in G++ have names that
   use `.'.  If this macro is defined, these names are rewritten to avoid `.'.  */
/* #define NO_DOT_IN_LABEL */

/* Define this macro if the target system expects every program's `main'
   function to return a standard "success" value by default (if no other value
   is explicitly returned).

   The definition should be a C statement (sans semicolon) to generate the
   appropriate rtl instructions.  It is used only when compiling the end of
   `main'.  */
/* #define DEFAULT_MAIN_RETURN */

/* Define this if the target system supports the function `atexit' from the
   ANSI C standard.  If this is not defined, and `INIT_SECTION_ASM_OP' is not
   defined, a default `exit' function will be provided to support C++.

   Defined by svr4.h */
/* #define HAVE_ATEXIT */

/* Define this if your `exit' function needs to do something besides calling an
   external function `_cleanup' before terminating with `_exit'.  The
   `EXIT_BODY' macro is only needed if netiher `HAVE_ATEXIT' nor
   `INIT_SECTION_ASM_OP' are defined.  */
/* #define EXIT_BODY */

/* Define this macro as a C expression that is nonzero if it is safe for the
   delay slot scheduler to place instructions in the delay slot of INSN, even
   if they appear to use a resource set or clobbered in INSN.  INSN is always a
   `jump_insn' or an `insn'; GNU CC knows that every `call_insn' has this
   behavior.  On machines where some `insn' or `jump_insn' is really a function
   call and hence has this behavior, you should define this macro.

   You need not define this macro if it would always return zero.  */
/* #define INSN_SETS_ARE_DELAYED(INSN) */

/* Define this macro as a C expression that is nonzero if it is safe for the
   delay slot scheduler to place instructions in the delay slot of INSN, even
   if they appear to set or clobber a resource referenced in INSN.  INSN is
   always a `jump_insn' or an `insn'.  On machines where some `insn' or
   `jump_insn' is really a function call and its operands are registers whose
   use is actually in the subroutine it calls, you should define this macro.
   Doing so allows the delay slot scheduler to move instructions which copy
   arguments into the argument registers into the delay slot of INSN.

   You need not define this macro if it would always return zero.  */
/* #define INSN_REFERENCES_ARE_DELAYED(INSN) */

/* In rare cases, correct code generation requires extra machine dependent
   processing between the second jump optimization pass and delayed branch
   scheduling.  On those machines, define this macro as a C statement to act on
   the code starting at INSN.  */
/* #define MACHINE_DEPENDENT_REORG(INSN) */

/* Define this macro if in some cases global symbols from one translation unit
   may not be bound to undefined symbols in another translation unit without
   user intervention.  For instance, under Microsoft Windows symbols must be
   explicitly imported from shared libraries (DLLs).  */
/* #define MULTIPLE_SYMBOL_SPACES */

/* A C expression for the maximum number of instructions to execute via
   conditional execution instructions instead of a branch.  A value of
   BRANCH_COST+1 is the default if the machine does not use
   cc0, and 1 if it does use cc0.  */
/* #define MAX_CONDITIONAL_EXECUTE */

/* A C statement that adds to tree CLOBBERS a set of STRING_CST trees for any
   hard regs the port wishes to automatically clobber for all asms.  */
/* #define MD_ASM_CLOBBERS(CLOBBERS) */

/* Indicate how many instructions can be issued at the same time.  */
/* #define ISSUE_RATE */

/* A C statement which is executed by the Haifa scheduler at the beginning of
   each block of instructions that are to be scheduled.  FILE is either a null
   pointer, or a stdio stream to write any debug output to.  VERBOSE is the
   verbose level provided by -fsched-verbose-<n>.  */
/* #define MD_SCHED_INIT (FILE, VERBOSE) */

/* A C statement which is executed by the Haifa scheduler after it has scheduled
   the ready list to allow the machine description to reorder it (for example to
   combine two small instructions together on VLIW machines).  FILE is either a
   null pointer, or a stdio stream to write any debug output to.  VERBOSE is the
   verbose level provided by -fsched-verbose-=<n>.  READY is a pointer to the
   ready list of instructions that are ready to be scheduled.  N_READY is the
   number of elements in the ready list.  The scheduler reads the ready list in
   reverse order, starting with READY[N_READY-1] and going to READY[0].  CLOCK
   is the timer tick of the scheduler.  CAN_ISSUE_MORE is an output parameter that
   is set to the number of insns that can issue this clock; normally this is just
   'issue_rate'  */
/* #define  MD_SCHED_REORDER (FILE, VERBOSE, READY, N_READY, CLOCK, CAN_ISSUE_MORE) */

/* A C statement which is executed by the Haifa scheduler after it has scheduled
   an insn from the ready list.  FILE is either a null pointer, or a stdio stream
   to write any debug output to.  VERBOSE is the verbose level provided by
   -fsched-verbose-<n>.  INSN is the instruction that was scheduled.  MORE is the
   number of instructions that can be issued in the current cycle.  This macro
   is responsible for updating the value of MORE (typically by (MORE)--).  */
/* #define MD_SCHED_VARIABLE_ISSUE (FILE, VERBOSE, INSN, MORE) */

/* Define this to the largest integer machine mode which can be used for
   operations other than load, store and copy operations.  You need only define
   this macro if the target holds values larger than word_mode in general purpose
   registers.  Most targets should not define this macro.  */
/* #define MAX_INTEGER_COMPUTATION_MODE */

/* Define this macro as a C string constant for the linker argument to link in the
   system math library, or "" if the target does not have a separate math library.
   You need only define this macro if the default of "-lm" is wrong.  */
/* #define  MATH_LIBRARY */

/* Define the information needed to generate branch and scc insns.  This is
   stored from the compare operation.  Note that we can't use "rtx" here
   since it hasn't been defined!  */

extern struct rtx_def *xstormy16_compare_op0, *xstormy16_compare_op1;

/* End of xstormy16.h */
