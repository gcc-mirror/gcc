/* Definitions of target machine for Visium.
   Copyright (C) 2002-2023 Free Software Foundation, Inc.
   Contributed by C.Nettleton, J.P.Parkes and P.Garbett.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */


/* Controlling the Compilation Driver, `gcc'  */

/* Pass -mtune=* options to the assembler */
#undef ASM_SPEC
#define ASM_SPEC "%{mcpu=gr6:-mtune=gr6; :-mtune=mcm}"

/* Define symbols for the preprocessor.  */
#define CPP_SPEC "%{mcpu=gr6:-D__gr6__; :-D__gr5__}"

/* Targets of a link */
#define LIB_SPEC \
  "--start-group -lc %{msim:-lsim; mdebug:-ldebug; :-lserial} --end-group"

#define ENDFILE_SPEC "crtend.o%s crtn.o%s"
#define STARTFILE_SPEC "crti.o%s crtbegin.o%s crt0.o%s"

/* Run-time Target Specification */

/* TARGET_CPU_CPP_BUILTINS() This function-like macro expands to a
   block of code that defines built-in preprocessor macros and
   assertions for the target cpu, using the functions builtin_define,
   builtin_define_std and builtin_assert. When the front end calls
   this macro it provides a trailing semicolon, and since it has
   finished command line option processing your code can use those
   results freely.  builtin_assert takes a string in the form you pass
   to the command-line option -A, such as cpu=mips, and creates the
   assertion. builtin_define takes a string in the form accepted by
   option -D and unconditionally defines the macro.

   builtin_define_std takes a string representing the name of an
   object-like macro. If it doesn't lie in the user's namespace,
   builtin_define_std defines it unconditionally. Otherwise, it
   defines a version with two leading underscores, and another version
   with two leading and trailing underscores, and defines the original
   only if an ISO standard was not requested on the command line. For
   example, passing unix defines __unix, __unix__ and possibly unix;
   passing _mips defines __mips, __mips__ and possibly _mips, and
   passing _ABI64 defines only _ABI64.

   You can also test for the C dialect being compiled. The variable
   c_language is set to one of clk_c, clk_cplusplus or
   clk_objective_c. Note that if we are preprocessing assembler, this
   variable will be clk_c but the function-like macro
   preprocessing_asm_p() will return true, so you might want to check
   for that first.  If you need to check for strict ANSI, the variable
   flag_iso can be used. The function-like macro
   preprocessing_trad_p() can be used to check for traditional
   preprocessing.  */
#define TARGET_CPU_CPP_BUILTINS()			\
  do							\
    {							\
      builtin_define ("__VISIUM__");			\
      if (TARGET_MCM)					\
	builtin_define ("__VISIUM_ARCH_MCM__");		\
      if (TARGET_BMI)					\
	builtin_define ("__VISIUM_ARCH_BMI__");		\
      if (TARGET_FPU_IEEE)				\
	builtin_define ("__VISIUM_ARCH_FPU_IEEE__");	\
    }							\
  while (0)

/* Recast the cpu class to be the cpu attribute.
   Every file includes us, but not every file includes insn-attr.h.  */
#define visium_cpu_attr ((enum attr_cpu) visium_cpu)

/* Defining data structures for per-function information.

   If the target needs to store information on a per-function basis,
   GCC provides a macro and a couple of variables to allow this. Note,
   just using statics to store the information is a bad idea, since
   GCC supports nested functions, so you can be halfway through
   encoding one function when another one comes along.

   GCC defines a data structure called struct function which contains
   all of the data specific to an individual function. This structure
   contains a field called machine whose type is struct
   machine_function *, which can be used by targets to point to their
   own specific data.

   If a target needs per-function specific data it should define the
   type struct machine_function and also the macro
   INIT_EXPANDERS. This macro should be used to initialize the
   function pointer init_machine_status.  This pointer is explained
   below.

   One typical use of per-function, target specific data is to create
   an RTX to hold the register containing the function's return
   address.  This RTX can then be used to implement the
   __builtin_return_address function, for level 0.

   Note--earlier implementations of GCC used a single data area to
   hold all of the per-function information. Thus when processing of a
   nested function began the old per-function data had to be pushed
   onto a stack, and when the processing was finished, it had to be
   popped off the stack.  GCC used to provide function pointers called
   save_machine_status and restore_machine_status to handle the saving
   and restoring of the target specific information. Since the single
   data area approach is no longer used, these pointers are no longer
   supported.

   The macro and function pointers are described below. 

   INIT_EXPANDERS:

   Macro called to initialize any target specific information. This
   macro is called once per function, before generation of any RTL has
   begun.  The intention of this macro is to allow the initialization
   of the function pointers below.

   init_machine_status:
   This is a void (*)(struct function *) function pointer. If this
   pointer is non-NULL it will be called once per function, before
   function compilation starts, in order to allow the target to
   perform any target specific initialization of the struct function
   structure. It is intended that this would be used to initialize the
   machine of that structure.  struct machine_function structures are
   expected to be freed by GC.  Generally, any memory that they
   reference must be allocated by using ggc_alloc, including the
   structure itself. */

#define INIT_EXPANDERS visium_init_expanders ()

/* Storage Layout

   Note that the definitions of the macros in this table which are
   sizes or alignments measured in bits do not need to be constant.
   They can be C expressions that refer to static variables, such as
   the `target_flags'.

   `BITS_BIG_ENDIAN'

   Define this macro to have the value 1 if the most significant bit
   in a byte has the lowest number; otherwise define it to have the
   value zero.  This means that bit-field instructions count from the
   most significant bit.  If the machine has no bit-field
   instructions, then this must still be defined, but it doesn't
   matter which value it is defined to.  This macro need not be a
   constant.

   This macro does not affect the way structure fields are packed into
   bytes or words; that is controlled by `BYTES_BIG_ENDIAN'. */
#define BITS_BIG_ENDIAN 1

/* `BYTES_BIG_ENDIAN'

   Define this macro to have the value 1 if the most significant byte
   in a word has the lowest number.  This macro need not be a
   constant.*/
#define BYTES_BIG_ENDIAN 1

/* `WORDS_BIG_ENDIAN'

   Define this macro to have the value 1 if, in a multiword object,
   the most significant word has the lowest number.  This applies to
   both memory locations and registers; GNU CC fundamentally assumes
   that the order of words in memory is the same as the order in
   registers.  This macro need not be a constant.  */
#define WORDS_BIG_ENDIAN 1

/* `BITS_PER_WORD'

   Number of bits in a word; normally 32. */
#define BITS_PER_WORD 32

/* `UNITS_PER_WORD'

   Number of storage units in a word; normally 4. */
#define UNITS_PER_WORD 4

/* `POINTER_SIZE'

   Width of a pointer, in bits.  You must specify a value no wider
   than the width of `Pmode'.  If it is not equal to the width of
   `Pmode', you must define `POINTERS_EXTEND_UNSIGNED'.  */
#define POINTER_SIZE 32

/* `PARM_BOUNDARY'

   Normal alignment required for function parameters on the stack, in
   bits.  All stack parameters receive at least this much alignment
   regardless of data type.  On most machines, this is the same as the
   size of an integer. */
#define PARM_BOUNDARY 32

/* `STACK_BOUNDARY'

   Define this macro if you wish to preserve a certain alignment for
   the stack pointer.  The definition is a C expression for the
   desired alignment (measured in bits).

   If `PUSH_ROUNDING' is not defined, the stack will always be aligned
   to the specified boundary.  If `PUSH_ROUNDING' is defined and
   specifies a less strict alignment than `STACK_BOUNDARY', the stack
   may be momentarily unaligned while pushing arguments. */
#define STACK_BOUNDARY 32

#define VISIUM_STACK_ALIGN(LOC) (((LOC) + 3) & ~3)

/* `FUNCTION_BOUNDARY'

   Alignment required for a function entry point, in bits. */
#define FUNCTION_BOUNDARY 32

/* `BIGGEST_ALIGNMENT'

   Biggest alignment that any data type can require on this machine,
   in bits. */
#define BIGGEST_ALIGNMENT 32

/* `DATA_ALIGNMENT (TYPE, BASIC-ALIGN)`

   If defined, a C expression to compute the alignment for a variable
   in the static store.  TYPE is the data type, and BASIC-ALIGN is
   the alignment that the object would ordinarily have.  The value of
   this macro is used instead of that alignment to align the object. */
#define DATA_ALIGNMENT(TYPE,ALIGN) visium_data_alignment (TYPE, ALIGN)

/* `LOCAL_ALIGNMENT (TYPE, BASIC-ALIGN)`

   If defined, a C expression to compute the alignment for a variable
   in the local store.  TYPE is the data type, and BASIC-ALIGN is the
   alignment that the object would ordinarily have.  The value of this
   macro is used instead of that alignment to align the object. */
#define LOCAL_ALIGNMENT(TYPE,ALIGN) visium_data_alignment (TYPE, ALIGN)

/* `EMPTY_FIELD_BOUNDARY'

   Alignment in bits to be given to a structure bit field that follows
   an empty field such as `int : 0;'.

   Note that `PCC_BITFIELD_TYPE_MATTERS' also affects the alignment
   that results from an empty field. */
#define EMPTY_FIELD_BOUNDARY 32

/* `STRICT_ALIGNMENT'

   Define this macro to be the value 1 if instructions will fail to
   work if given data not on the nominal alignment.  If instructions
   will merely go slower in that case, define this macro as 0. */
#define STRICT_ALIGNMENT 1

/* `TARGET_FLOAT_FORMAT'

   A code distinguishing the floating point format of the target
   machine.  There are three defined values:

   `IEEE_FLOAT_FORMAT'
          This code indicates IEEE floating point.  It is the default;
          there is no need to define this macro when the format is IEEE.

    `VAX_FLOAT_FORMAT'
          This code indicates the peculiar format used on the Vax.

    `UNKNOWN_FLOAT_FORMAT'
          This code indicates any other format.

    The value of this macro is compared with `HOST_FLOAT_FORMAT' to
    determine whether the target machine has the same format as the
    host machine.  If any other formats are actually in use on
    supported machines, new codes should be defined for them.

    The ordering of the component words of floating point values
    stored in memory is controlled by `FLOAT_WORDS_BIG_ENDIAN' for the
    target machine and `HOST_FLOAT_WORDS_BIG_ENDIAN' for the host. */
#define TARGET_FLOAT_FORMAT IEEE_FLOAT_FORMAT
#define UNITS_PER_HWFPVALUE 4

/* Layout of Source Language Data Types

   These macros define the sizes and other characteristics of the
   standard basic data types used in programs being compiled.  Unlike
   the macros in the previous section, these apply to specific
   features of C and related languages, rather than to fundamental
   aspects of storage layout. */

/* `INT_TYPE_SIZE'

   A C expression for the size in bits of the type `int' on the target
   machine.  If you don't define this, the default is one word. */
#define INT_TYPE_SIZE  32

/* `SHORT_TYPE_SIZE'

   A C expression for the size in bits of the type `short' on the
   target machine.  If you don't define this, the default is half a
   word.  (If this would be less than one storage unit, it is rounded
   up to one unit.) */
#define SHORT_TYPE_SIZE 16

/* `LONG_TYPE_SIZE'

   A C expression for the size in bits of the type `long' on the
   target machine.  If you don't define this, the default is one word. */
#define LONG_TYPE_SIZE  32

/* `LONG_LONG_TYPE_SIZE'

   A C expression for the size in bits of the type `long long' on the
   target machine.  If you don't define this, the default is two
   words.  If you want to support GNU Ada on your machine, the value
   of macro must be at least 64. */
#define LONG_LONG_TYPE_SIZE  64

/* `CHAR_TYPE_SIZE'

   A C expression for the size in bits of the type `char' on the
   target machine.  If you don't define this, the default is one
   quarter of a word.  (If this would be less than one storage unit,
   it is rounded up to one unit.) */
#define CHAR_TYPE_SIZE  8

/* `FLOAT_TYPE_SIZE'

   A C expression for the size in bits of the type `float' on the
   target machine.  If you don't define this, the default is one word. */
#define FLOAT_TYPE_SIZE  32

/* `DOUBLE_TYPE_SIZE'

   A C expression for the size in bits of the type `double' on the
   target machine.  If you don't define this, the default is two
   words. */
#define DOUBLE_TYPE_SIZE  64

/* `LONG_DOUBLE_TYPE_SIZE'

   A C expression for the size in bits of the type `long double' on
   the target machine.  If you don't define this, the default is two
   words. */
#define LONG_DOUBLE_TYPE_SIZE   DOUBLE_TYPE_SIZE

/* `WIDEST_HARDWARE_FP_SIZE'

   A C expression for the size in bits of the widest floating-point
   format supported by the hardware.  If you define this macro, you
   must specify a value less than or equal to the value of
   `LONG_DOUBLE_TYPE_SIZE'.  If you do not define this macro, the
   value of `LONG_DOUBLE_TYPE_SIZE' is the default. */

/* `DEFAULT_SIGNED_CHAR'

   An expression whose value is 1 or 0, according to whether the type
   `char' should be signed or unsigned by default.  The user can
   always override this default with the options `-fsigned-char' and
   `-funsigned-char'. */
#define DEFAULT_SIGNED_CHAR 0

/* `SIZE_TYPE'

   A C expression for a string describing the name of the data type to
   use for size values.  The typedef name `size_t' is defined using
   the contents of the string.

   The string can contain more than one keyword.  If so, separate them
   with spaces, and write first any length keyword, then `unsigned' if
   appropriate, and finally `int'.  The string must exactly match one
   of the data type names defined in the function
   `init_decl_processing' in the file `c-decl.cc'.  You may not omit
   `int' or change the order--that would cause the compiler to crash
   on startup.

   If you don't define this macro, the default is `"long unsigned
   int"'. */
#define SIZE_TYPE "unsigned int"

/* `PTRDIFF_TYPE'

   A C expression for a string describing the name of the data type to
   use for the result of subtracting two pointers.  The typedef name
   `ptrdiff_t' is defined using the contents of the string.  See
   `SIZE_TYPE' above for more information.

   If you don't define this macro, the default is `"long int"'. */
#define PTRDIFF_TYPE "long int"

/* Newlib uses the unsigned type corresponding to ptrdiff_t for
   uintptr_t; this is the same as size_t for most newlib-using
   targets, but not for us.  */
#define UINTPTR_TYPE "long unsigned int"

/* `WCHAR_TYPE'

   A C expression for a string describing the name of the data type to
   use for wide characters.  The typedef name `wchar_t' is defined
   using the contents of the string.  See `SIZE_TYPE' above for more
   information.

   If you don't define this macro, the default is `"int"'. */
#define WCHAR_TYPE "short int"

/* `WCHAR_TYPE_SIZE'

   A C expression for the size in bits of the data type for wide
   characters.  This is used in `cpp', which cannot make use of
   `WCHAR_TYPE'. */
#define WCHAR_TYPE_SIZE 16

/* Register Usage

   This section explains how to describe what registers the target
   machine has, and how (in general) they can be used.  */

/* `FIRST_PSEUDO_REGISTER'

   Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   Register 51 is used as the argument pointer register.
   Register 52 is used as the soft frame pointer register.  */
#define FIRST_PSEUDO_REGISTER 53

#define RETURN_REGNUM        1
#define PROLOGUE_TMP_REGNUM  9
#define LINK_REGNUM         21
#define GP_LAST_REGNUM      31
#define GP_REGISTER_P(REGNO) \
  (((unsigned) (REGNO)) <= GP_LAST_REGNUM)

#define MDB_REGNUM          32
#define MDC_REGNUM          33

#define FP_FIRST_REGNUM     34
#define FP_LAST_REGNUM      49
#define FP_RETURN_REGNUM    (FP_FIRST_REGNUM + 1)
#define FP_REGISTER_P(REGNO) \
  (FP_FIRST_REGNUM <= (REGNO) && (REGNO) <= FP_LAST_REGNUM)

#define FLAGS_REGNUM        50

/* `FIXED_REGISTERS'

   An initializer that says which registers are used for fixed
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
   options `-ffixed-REG', `-fcall-used-REG' and `-fcall-saved-REG'.

   r0 and f0 are immutable registers hardwired to 0.
   r21 is the link register used for procedure linkage.
   r23 is the stack pointer register.
   r29 and r30 hold the interrupt context.
   mdc is a read-only register because the writemdc instruction
   terminates all the operations of the EAM on the GR6.  */
#define FIXED_REGISTERS  \
 { 1, 0, 0, 0, 0, 0, 0, 0, /* r0 .. r7 */      \
   0, 0, 0, 0, 0, 0, 0, 0, /* r8 .. r15 */     \
   0, 0, 0, 0, 0, 1, 0, 1, /* r16 .. r23 */    \
   0, 0, 0, 0, 0, 1, 1, 0, /* r24 .. r31 */    \
   0, 1,                   /* mdb, mdc */      \
   1, 0, 0, 0, 0, 0, 0, 0, /* f0 .. f7 */      \
   0, 0, 0, 0, 0, 0, 0, 0, /* f8 .. f15 */     \
   1, 1, 1 }               /* flags, arg, frame */

/* Like `CALL_USED_REGISTERS' except this macro doesn't require that
   the entire set of `FIXED_REGISTERS' be included.
   (`CALL_USED_REGISTERS' must be a superset of `FIXED_REGISTERS').
   This macro is optional.  If not specified, it defaults to the value
   of `CALL_USED_REGISTERS'.  */
#define CALL_REALLY_USED_REGISTERS  \
 { 0, 1, 1, 1, 1, 1, 1, 1, /* r0 .. r7 */      \
   1, 1, 1, 0, 0, 0, 0, 0, /* r8 .. r15 */     \
   0, 0, 0, 0, 1, 0, 0, 0, /* r16 .. r23 */    \
   1, 1, 1, 1, 1, 0, 0, 1, /* r24 .. r31 */    \
   1, 1,                   /* mdb, mdc */      \
   1, 1, 1, 1, 1, 1, 1, 1, /* f0 .. f7 */      \
   1, 0, 0, 0, 0, 0, 0, 0, /* f8 .. f15 */     \
   1, 0, 0 }               /* flags, arg, frame */

/* `REG_ALLOC_ORDER'

   If defined, an initializer for a vector of integers, containing the
   numbers of hard registers in the order in which GCC should prefer
   to use them (from most preferred to least).

   If this macro is not defined, registers are used lowest numbered
   first (all else being equal).  */
#define REG_ALLOC_ORDER \
 { 10, 9, 8, 7, 6, 5, 4, 3, 2, 1,          /* r10 .. r1 */   \
   11, 12, 13, 14, 15, 16, 17, 18, 19, 20, /* r11 .. r20 */  \
   22,                                     /* fp */          \
   24, 25, 26, 27, 28,                     /* r24 .. r28 */  \
   31,                                     /* r31 */         \
   32, 33,                                 /* mdb, mdc */    \
   42, 41, 40, 39, 38, 37, 36, 35,         /* f8 .. f1 */    \
   43, 44, 45, 46, 47, 48, 49,             /* f9 .. f15 */   \
   21, 23,                                 /* lr, sp */      \
   29, 30,                                 /* r29, r30 */    \
   50, 51, 52,                             /* flags, arg, frame */ \
   0, 34 }                                 /* r0, f0 */

/* `HARD_REGNO_RENAME_OK (OLD_REG, NEW_REG)'

   A C expression which is nonzero if hard register NEW_REG can be
   considered for use as a rename register for hard register OLD_REG. */
#define HARD_REGNO_RENAME_OK(OLD_REG, NEW_REG) \
  visium_hard_regno_rename_ok (OLD_REG, NEW_REG)

/* Register Classes

   On many machines, the numbered registers are not all equivalent.
   For example, certain registers may not be allowed for indexed
   addressing; certain registers may not be allowed in some
   instructions.  These machine restrictions are described to the
   compiler using "register classes".

   `enum reg_class'

   An enumeral type that must be defined with all the register class
   names as enumeral values.  `NO_REGS' must be first.  `ALL_REGS'
   must be the last register class, followed by one more enumeral
   value, `LIM_REG_CLASSES', which is not a register class but rather
   tells how many classes there are.

   Each register class has a number, which is the value of casting the
   class name to type `int'.  The number serves as an index in many of
   the tables described below. */

enum reg_class
{
  NO_REGS,
  MDB,
  MDC,
  FP_REGS,
  FLAGS,
  R1,
  R2,
  R3,
  SIBCALL_REGS,
  LOW_REGS,
  GENERAL_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

/* `N_REG_CLASSES'

   The number of distinct register classes, defined as follows.  */
#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* `REG_CLASS_NAMES'

   An initializer containing the names of the register classes as C
   string constants.  These names are used in writing some of the
   debugging dumps. */
#define REG_CLASS_NAMES \
 {"NO_REGS", "MDB", "MDC", "FP_REGS", "FLAGS", "R1", "R2", "R3", \
  "SIBCALL_REGS", "LOW_REGS", "GENERAL_REGS", "ALL_REGS"}

/* `REG_CLASS_CONTENTS'

   An initializer containing the contents of the register classes, as
   integers which are bit masks.  The Nth integer specifies the
   contents of class N.  The way the integer MASK is interpreted is
   that register R is in the class if `MASK & (1 << R)' is 1.

   When the machine has more than 32 registers, an integer does not
   suffice.  Then the integers are replaced by sub-initializers,
   braced groupings containing several integers.  Each sub-initializer
   must be suitable as an initializer for the type `HARD_REG_SET'
   which is defined in `hard-reg-set.h'. */
#define REG_CLASS_CONTENTS {                     \
    {0x00000000, 0x00000000}, /* NO_REGS */      \
    {0x00000000, 0x00000001}, /* MDB */          \
    {0x00000000, 0x00000002}, /* MDC */          \
    {0x00000000, 0x0003fffc}, /* FP_REGS */      \
    {0x00000000, 0x00040000}, /* FLAGS */        \
    {0x00000002, 0x00000000}, /* R1 */           \
    {0x00000004, 0x00000000}, /* R2 */           \
    {0x00000008, 0x00000000}, /* R3 */           \
    {0x000005ff, 0x00000000}, /* SIBCALL_REGS */ \
    {0x1fffffff, 0x00000000}, /* LOW_REGS */     \
    {0xffffffff, 0x00180000}, /* GENERAL_REGS */ \
    {0xffffffff, 0x001fffff}} /* ALL_REGS */

/* `REGNO_REG_CLASS (REGNO)'

   A C expression whose value is a register class containing hard
   register REGNO.  In general there is more than one such class;
   choose a class which is "minimal", meaning that no smaller class
   also contains the register. */
#define REGNO_REG_CLASS(REGNO)                    \
  ((REGNO) == MDB_REGNUM ? MDB :                  \
   (REGNO) == MDC_REGNUM ? MDC :                  \
   FP_REGISTER_P (REGNO) ? FP_REGS :              \
   (REGNO) == FLAGS_REGNUM ? FLAGS :              \
   (REGNO) == 1 ? R1 :                            \
   (REGNO) == 2 ? R2 :                            \
   (REGNO) == 3 ? R3 :                            \
   (REGNO) <= 8 || (REGNO) == 10 ? SIBCALL_REGS : \
   (REGNO) <= 28 ? LOW_REGS :                     \
   GENERAL_REGS)

/* `BASE_REG_CLASS'

   A macro whose definition is the name of the class to which a valid
   base register must belong.  A base register is one used in an
   address which is the register value plus a displacement. */
#define BASE_REG_CLASS GENERAL_REGS

#define BASE_REGISTER_P(REGNO)        \
  (GP_REGISTER_P (REGNO)              \
   || (REGNO) == ARG_POINTER_REGNUM   \
   || (REGNO) == FRAME_POINTER_REGNUM)

/* `INDEX_REG_CLASS'

   A macro whose definition is the name of the class to which a valid
   index register must belong.  An index register is one used in an
   address where its value is either multiplied by a scale factor or
   added to another register (as well as added to a displacement). */
#define INDEX_REG_CLASS NO_REGS

/* `REGNO_OK_FOR_BASE_P (NUM)'

   A C expression which is nonzero if register number NUM is suitable
   for use as a base register in operand addresses.  It may be either
   a suitable hard register or a pseudo register that has been
   allocated such a hard register. */
#define REGNO_OK_FOR_BASE_P(REGNO) \
  (BASE_REGISTER_P (REGNO) || BASE_REGISTER_P ((unsigned)reg_renumber[REGNO]))

/* `REGNO_OK_FOR_INDEX_P (NUM)'

   A C expression which is nonzero if register number NUM is suitable
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
   both registers only if neither labeling works. */
#define REGNO_OK_FOR_INDEX_P(REGNO) 0

/* `PREFERRED_RELOAD_CLASS (X, CLASS)'

   A C expression that places additional restrictions on the register
   class to use when it is necessary to copy value X into a register
   in class CLASS.  The value is a register class; perhaps CLASS, or
   perhaps another, smaller class.

   Sometimes returning a more restrictive class makes better code.
   For example, on the 68000, when X is an integer constant that is in
   range for a `moveq' instruction, the value of this macro is always
   `DATA_REGS' as long as CLASS includes the data registers.
   Requiring a data register guarantees that a `moveq' will be used.

   If X is a `const_double', by returning `NO_REGS' you can force X
   into a memory constant.  This is useful on certain machines where
   immediate floating values cannot be loaded into certain kinds of
   registers. */
#define PREFERRED_RELOAD_CLASS(X,CLASS) CLASS

#define CLASS_MAX_NREGS(CLASS, MODE)    \
  ((CLASS) == MDB ?                     \
  ((GET_MODE_SIZE (MODE) + 2 * UNITS_PER_WORD - 1) / (2 * UNITS_PER_WORD)) \
  : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* Stack Layout and Calling Conventions

   Basic Stack Layout

   `STACK_GROWS_DOWNWARD'
   Define this macro if pushing a word onto the stack moves the stack
   pointer to a smaller address.  */
#define STACK_GROWS_DOWNWARD 1

/* `FIRST_PARM_OFFSET (FUNDECL)'

   Offset from the argument pointer register to the first argument's
   address.  On some machines it may depend on the data type of the
   function.

   If `ARGS_GROW_DOWNWARD', this is the offset to the location above
   the first argument's address. */
#define FIRST_PARM_OFFSET(FNDECL) 0

/* `DYNAMIC_CHAIN_ADDRESS (FRAMEADDR)'

   A C expression whose value is RTL representing the address in a
   stack frame where the pointer to the caller's frame is stored.
   Assume that FRAMEADDR is an RTL expression for the address of the
   stack frame itself.

   If you don't define this macro, the default is to return the value
   of FRAMEADDR--that is, the stack frame address is also the address
   of the stack word that points to the previous frame. */
#define DYNAMIC_CHAIN_ADDRESS(FRAMEADDR) \
  visium_dynamic_chain_address (FRAMEADDR)

/* `RETURN_ADDR_RTX (COUNT, FRAMEADDR)'

   A C expression whose value is RTL representing the value of the
   return address for the frame COUNT steps up from the current frame,
   after the prologue.  FRAMEADDR is the frame pointer of the COUNT
   frame, or the frame pointer of the COUNT - 1 frame if
   `RETURN_ADDR_IN_PREVIOUS_FRAME' is defined.

   The value of the expression must always be the correct address when
   COUNT is zero, but may be `NULL_RTX' if there is not way to
   determine the return address of other frames.  */
#define RETURN_ADDR_RTX(COUNT,FRAMEADDR) \
  visium_return_addr_rtx (COUNT, FRAMEADDR)

/* Exception Handling

   `EH_RETURN_DATA_REGNO'

   A C expression whose value is the Nth register number used for data
   by exception handlers or INVALID_REGNUM if fewer than N registers
   are available.

   The exception handling library routines communicate with the
   exception handlers via a set of agreed upon registers. */
#define EH_RETURN_DATA_REGNO(N) ((N) < 4 ? (N) + 11 : INVALID_REGNUM)
#define EH_RETURN_STACKADJ_RTX gen_rtx_REG (SImode, 8)
#define EH_RETURN_HANDLER_RTX visium_eh_return_handler_rtx ()

/* Registers That Address the Stack Frame

   This discusses registers that address the stack frame.

   `STACK_POINTER_REGNUM'

   The register number of the stack pointer register, which must also
   be a fixed register according to `FIXED_REGISTERS'.  On most
   machines, the hardware determines which register this is. */
#define STACK_POINTER_REGNUM 23

/* `FRAME_POINTER_REGNUM'

   The register number of the frame pointer register, which is used to
   access automatic variables in the stack frame.  On some machines,
   the hardware determines which register this is.  On other machines,
   you can choose any register you wish for this purpose. */
#define FRAME_POINTER_REGNUM 52

/* `HARD_FRAME_POINTER_REGNUM'

   On some machines the offset between the frame pointer and starting
   offset of the automatic variables is not known until after register
   allocation has been done (for example, because the saved registers
   are between these two locations).  On those machines, define
   `FRAME_POINTER_REGNUM' the number of a special, fixed register to
   be used internally until the offset is known, and define
   `HARD_FRAME_POINTER_REGNUM' to be the actual hard register number
   used for the frame pointer.  */
#define HARD_FRAME_POINTER_REGNUM 22

/* `ARG_POINTER_REGNUM'

   The register number of the arg pointer register, which is used to
   access the function's argument list.  On some machines, this is the
   same as the frame pointer register.  On some machines, the hardware
   determines which register this is.  On other machines, you can
   choose any register you wish for this purpose.  If this is not the
   same register as the frame pointer register, then you must mark it
   as a fixed register according to `FIXED_REGISTERS', or arrange to
   be able to eliminate it (*note Elimination::.).  */
#define ARG_POINTER_REGNUM 51

/* `STATIC_CHAIN_REGNUM'
   `STATIC_CHAIN_INCOMING_REGNUM'

   Register numbers used for passing a function's static chain
   pointer.  If register windows are used, the register number as seen
   by the called function is `STATIC_CHAIN_INCOMING_REGNUM', while the
   register number as seen by the calling function is
   `STATIC_CHAIN_REGNUM'.  If these registers are the same,
   `STATIC_CHAIN_INCOMING_REGNUM' need not be defined.

   The static chain register need not be a fixed register.

   If the static chain is passed in memory, these macros should not be
   defined; instead, the next two macros should be defined. */
#define STATIC_CHAIN_REGNUM 20

/* `ELIMINABLE_REGS'

   If defined, this macro specifies a table of register pairs used to
   eliminate unneeded registers that point into the stack frame.  If
   it is not defined, the only elimination attempted by the compiler
   is to replace references to the frame pointer with references to
   the stack pointer.

   The definition of this macro is a list of structure
   initializations, each of which specifies an original and
   replacement register.

   On some machines, the position of the argument pointer is not known
   until the compilation is completed.  In such a case, a separate
   hard register must be used for the argument pointer.  This register
   can be eliminated by replacing it with either the frame pointer or
   the argument pointer, depending on whether or not the frame pointer
   has been eliminated.

   Note that the elimination of the argument pointer with the stack
   pointer is specified first since that is the preferred elimination.  */
#define ELIMINABLE_REGS				     \
{{ ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},	     \
 { ARG_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM},   \
 { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},	     \
 { FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM}}

/* `INITIAL_ELIMINATION_OFFSET (FROM-REG, TO-REG, OFFSET-VAR)'

   This macro returns the initial difference between the specified pair
   of registers.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  (OFFSET = visium_initial_elimination_offset (FROM, TO))

/* Passing Function Arguments on the Stack

   The macros in this section control how arguments are passed on the
   stack.  See the following section for other macros that control
   passing certain arguments in registers.

   Passing Arguments in Registers

   This section describes the macros which let you control how various
   types of arguments are passed in registers or how they are arranged
   in the stack.

   Define the general purpose, and floating point registers used for
   passing arguments */
#define MAX_ARGS_IN_GP_REGISTERS 8
#define GP_ARG_FIRST 1
#define GP_ARG_LAST (GP_ARG_FIRST + MAX_ARGS_IN_GP_REGISTERS - 1)
#define MAX_ARGS_IN_FP_REGISTERS 8
#define FP_ARG_FIRST (FP_FIRST_REGNUM + 1)
#define FP_ARG_LAST (FP_ARG_FIRST + MAX_ARGS_IN_FP_REGISTERS - 1)

/* Define a data type for recording info about an argument list during the
processing of that argument list. */

struct visium_args
{
  /* The count of general registers used */
  int grcount;
  /* The count of floating registers used */
  int frcount;
  /* The number of stack words used by named arguments */
  int stack_words;
};

/* `CUMULATIVE_ARGS'

   A C type for declaring a variable that is used as the first
   argument of `FUNCTION_ARG' and other related values.  For some
   target machines, the type `int' suffices and can hold the number of
   bytes of argument so far.

   There is no need to record in `CUMULATIVE_ARGS' anything about the
   arguments that have been passed on the stack.  The compiler has
   other variables to keep track of that.  For target machines on
   which all arguments are passed on the stack, there is no need to
   store anything in `CUMULATIVE_ARGS'; however, the data structure
   must exist and should not be empty, so use `int'. */
#define CUMULATIVE_ARGS struct visium_args

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,FNDECL,N_NAMED_ARGS) \
  do {                        \
       (CUM).grcount = 0;     \
       (CUM).frcount = 0;     \
       (CUM).stack_words = 0; \
     } while (0)

/* `FUNCTION_ARG_REGNO_P (REGNO)'

   A C expression that is nonzero if REGNO is the number of a hard
   register in which function arguments are sometimes passed.  This
   does *not* include implicit arguments such as the static chain and
   the structure-value address.  On many machines, no registers can be
   used for this purpose since all function arguments are pushed on
   the stack. */
#define FUNCTION_ARG_REGNO_P(N)               	\
  ((GP_ARG_FIRST <= (N) && (N) <= GP_ARG_LAST)	\
   || (TARGET_FPU && FP_ARG_FIRST <= (N) && (N) <= FP_ARG_LAST))

/* `FUNCTION_VALUE_REGNO_P (REGNO)'

   A C expression that is nonzero if REGNO is the number of a hard
   register in which the values of called function may come back.

   A register whose use for returning values is limited to serving as
   the second of a pair (for a value of type `double', say) need not
   be recognized by this macro. If the machine has register windows,
   so that the caller and the called function use different registers
   for the return value, this macro should recognize only the caller's
   register numbers. */
#define FUNCTION_VALUE_REGNO_P(N) \
  ((N) == RETURN_REGNUM || (TARGET_FPU && (N) == FP_RETURN_REGNUM))

/* How Large Values Are Returned

   When a function value's mode is `BLKmode' (and in some other
   cases), the value is not returned according to `FUNCTION_VALUE'.
   Instead, the caller passes the address of a block of memory in
   which the value should be stored.  This address is called the
   "structure value address".

   This section describes how to control returning structure values in
   memory.

   `DEFAULT_PCC_STRUCT_RETURN'

   Define this macro to be 1 if all structure and union return values
   must be in memory.  Since this results in slower code, this should
   be defined only if needed for compatibility with other compilers or
   with an ABI.  If you define this macro to be 0, then the
   conventions used for structure and union return values are decided
   by the `RETURN_IN_MEMORY' macro.

   If not defined, this defaults to the value 1. */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Caller-Saves Register Allocation

   If you enable it, GNU CC can save registers around function calls.
   This makes it possible to use call-clobbered registers to hold
   variables that must live across calls.

   Function Entry and Exit

   This section describes the macros that output function entry
   ("prologue") and exit ("epilogue") code.

   `EXIT_IGNORE_STACK'

   Define this macro as a C expression that is nonzero if the return
   instruction or the function epilogue ignores the value of the stack
   pointer; in other words, if it is safe to delete an instruction to
   adjust the stack pointer before a return from the function.

   Note that this macro's value is relevant only for functions for
   which frame pointers are maintained.  It is never safe to delete a
   final stack adjustment in a function that has no frame pointer, and
   the compiler knows this regardless of `EXIT_IGNORE_STACK'. */
#define EXIT_IGNORE_STACK 1

/* `EPILOGUE_USES (REGNO)'

   Define this macro as a C expression that is nonzero for registers
   are used by the epilogue or the `return' pattern.  The stack and
   frame pointer registers are already be assumed to be used as
   needed. */
#define EPILOGUE_USES(REGNO) visium_epilogue_uses (REGNO)

/* Generating Code for Profiling

   These macros will help you generate code for profiling. */

#define PROFILE_HOOK(LABEL) visium_profile_hook ()
#define FUNCTION_PROFILER(FILE, LABELNO) do {} while (0)
#define NO_PROFILE_COUNTERS 1

/* Trampolines for Nested Functions

   A trampoline is a small piece of code that is created at run time
   when the address of a nested function is taken. It normally resides
   on the stack, in the stack frame of the containing function. These
   macros tell GCC how to generate code to allocate and initialize a
   trampoline.

   The instructions in the trampoline must do two things: load a
   constant address into the static chain register, and jump to the
   real address of the nested function. On CISC machines such as the
   m68k, this requires two instructions, a move immediate and a
   jump. Then the two addresses exist in the trampoline as word-long
   immediate operands. On RISC machines, it is often necessary to load
   each address into a register in two parts. Then pieces of each
   address form separate immediate operands.

   The code generated to initialize the trampoline must store the
   variable parts--the static chain value and the function
   address--into the immediate operands of the instructions. On a CISC
   machine, this is simply a matter of copying each address to a
   memory reference at the proper offset from the start of the
   trampoline. On a RISC machine, it may be necessary to take out
   pieces of the address and store them separately.

   On the Visium, the trampoline is

	moviu	r9,%u FUNCTION
	movil	r9,%l FUNCTION
	[nop]
	moviu	r20,%u STATIC
	bra	tr,r9,r0
	 movil	r20,%l STATIC

    A difficulty is setting the correct instruction parity at run time.


    TRAMPOLINE_SIZE 
    A C expression for the size in bytes of the trampoline, as an integer. */
#define TRAMPOLINE_SIZE (visium_cpu == PROCESSOR_GR6 ? 24 : 20)

/* Alignment required for trampolines, in bits.  */
#define TRAMPOLINE_ALIGNMENT (visium_cpu == PROCESSOR_GR6 ? 64 : 32)

/* Implicit calls to library routines

   Avoid calling library routines (sqrtf) just to set `errno' to EDOM */
#define TARGET_EDOM 33

/* Addressing Modes

   `MAX_REGS_PER_ADDRESS'

   A number, the maximum number of registers that can appear in a
   valid memory address.  Note that it is up to you to specify a value
   equal to the maximum number that `TARGET_LEGITIMATE_ADDRESS_P' would
   ever accept.  */
#define MAX_REGS_PER_ADDRESS 1

/* `LEGITIMIZE_RELOAD_ADDRESS (X, MODE, OPNUM, TYPE, IND_LEVELS, WIN)'

   A C compound statement that attempts to replace X, which is an
   address that needs reloading, with a valid memory address for an
   operand of mode MODE.  WIN will be a C statement label elsewhere
   in the code.  It is not necessary to define this macro, but it
   might be useful for performance reasons.  */
#define LEGITIMIZE_RELOAD_ADDRESS(AD, MODE, OPNUM, TYPE, IND, WIN) 	\
do									\
{									\
  rtx new_x = visium_legitimize_reload_address ((AD), (MODE), (OPNUM),	\
					(int) (TYPE), (IND));		\
  if (new_x)								\
    {									\
      (AD) = new_x;							\
      goto WIN;								\
    }									\
} while (0)

/* Given a comparison code (EQ, NE, etc.) and the operands of a COMPARE,
   return the mode to be used for the comparison.  */
#define SELECT_CC_MODE(OP,X,Y) visium_select_cc_mode ((OP), (X), (Y))

/* Return nonzero if MODE implies a floating point inequality can be
   reversed.  For Visium this is always true because we have a full
   compliment of ordered and unordered comparisons, but until generic
   code knows how to reverse it correctly we keep the old definition.  */
#define REVERSIBLE_CC_MODE(MODE) ((MODE) != CCFPEmode && (MODE) != CCFPmode)

/* `BRANCH_COST'

   A C expression for the cost of a branch instruction.  A value of 1
   is the default; other values are interpreted relative to that.  */
#define BRANCH_COST(A,B)  10

/* Override BRANCH_COST heuristics for complex logical ops.  */
#define LOGICAL_OP_NON_SHORT_CIRCUIT 0

/* `SLOW_BYTE_ACCESS'

   Define this macro as a C expression which is nonzero if accessing
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
   structure, but to different bytes. */
#define SLOW_BYTE_ACCESS 0

/* `MOVE_RATIO (SPEED)`

   The threshold of number of scalar memory-to-memory move insns,
   _below_ which a sequence of insns should be generated instead of a
   string move insn or a library call.  Increasing the value will
   always make code faster, but eventually incurs high cost in
   increased code size.

   Since we have a cpymemsi pattern, the default MOVE_RATIO is 2, which
   is too low given that cpymemsi will invoke a libcall.  */
#define MOVE_RATIO(speed) ((speed) ? 9 : 3)

/* `CLEAR_RATIO (SPEED)`

   The threshold of number of scalar move insns, _below_ which a
   sequence of insns should be generated to clear memory instead of a
   string clear insn or a library call.  Increasing the value will
   always make code faster, but eventually incurs high cost in
   increased code size.

   Since we have a setmemsi pattern, the default CLEAR_RATIO is 2, which
   is too low given that setmemsi will invoke a libcall.  */
#define CLEAR_RATIO(speed) ((speed) ? 13 : 5)

/* `MOVE_MAX'

   The maximum number of bytes that a single instruction can move
   quickly between memory and registers or between two memory
   locations. */
#define MOVE_MAX 4

/* `MAX_MOVE_MAX'

   The maximum number of bytes that a single instruction can move
   quickly between memory and registers or between two memory
   locations.  If this is undefined, the default is `MOVE_MAX'.
   Otherwise, it is the constant value that is the largest value that
   `MOVE_MAX' can have at run-time. */
#define MAX_MOVE_MAX 4

/* `SHIFT_COUNT_TRUNCATED'

   A C expression that is nonzero if on this machine the number of
   bits actually used for the count of a shift operation is equal to
   the number of bits needed to represent the size of the object being
   shifted.  When this macro is non-zero, the compiler will assume
   that it is safe to omit a sign-extend, zero-extend, and certain
   bitwise `and' instructions that truncates the count of a shift
   operation.  On machines that have instructions that act on
   bitfields at variable positions, which may include `bit test'
   instructions, a nonzero `SHIFT_COUNT_TRUNCATED' also enables
   deletion of truncations of the values that serve as arguments to
   bitfield instructions. */
#define SHIFT_COUNT_TRUNCATED 0

/* `STORE_FLAG_VALUE'

   A C expression describing the value returned by a comparison
   operator with an integral mode and stored by a store-flag
   instruction (`sCOND') when the condition is true.  This description
   must apply to *all* the `sCOND' patterns and all the comparison
   operators whose results have a `MODE_INT' mode. */
#define STORE_FLAG_VALUE 1

/* `Pmode'

   An alias for the machine mode for pointers.  On most machines,
   define this to be the integer mode corresponding to the width of a
   hardware pointer; `SImode' on 32-bit machine or `DImode' on 64-bit
   machines.  On some machines you must define this to be one of the
   partial integer modes, such as `PSImode'.

   The width of `Pmode' must be at least as large as the value of
   `POINTER_SIZE'.  If it is not equal, you must define the macro
   `POINTERS_EXTEND_UNSIGNED' to specify how pointers are extended to
   `Pmode'. */
#define Pmode SImode

/* `FUNCTION_MODE'

   An alias for the machine mode used for memory references to
   functions being called, in `call' RTL expressions.  On most
   machines this should be `QImode'. */
#define FUNCTION_MODE SImode

/* Dividing the Output into Sections (Texts, Data, ...)

   An object file is divided into sections containing different types
   of data.  In the most common case, there are three sections: the
   "text section", which holds instructions and read-only data; the
   "data section", which holds initialized writable data; and the "bss
   section", which holds uninitialized data.  Some systems have other
   kinds of sections.

   `TEXT_SECTION_ASM_OP'

   A C expression whose value is a string containing the assembler
   operation that should precede instructions and read-only data.
   Normally `".text"' is right. */
#define TEXT_SECTION_ASM_OP "\t.text"

/* `DATA_SECTION_ASM_OP'

   A C expression whose value is a string containing the assembler
   operation to identify the following data as writable initialized
   data.  Normally `".data"' is right. */
#define DATA_SECTION_ASM_OP "\t.data"

/* `BSS_SECTION_ASM_OP'

   If defined, a C expression whose value is a string containing the
   assembler operation to identify the following data as uninitialized
   global data.  If not defined, and neither `ASM_OUTPUT_BSS' nor
   `ASM_OUTPUT_ALIGNED_BSS' are defined, uninitialized global data
   will be output in the data section if `-fno-common' is passed,
   otherwise `ASM_OUTPUT_COMMON' will be used.

   `EXTRA_SECTIONS'

   A list of names for sections other than the standard two, which are
   `in_text' and `in_data'.  You need not define this macro on a
   system with no other sections (that GCC needs to use).

   `EXTRA_SECTION_FUNCTIONS'

   One or more functions to be defined in `varasm.cc'.  These functions
   should do jobs analogous to those of `text_section' and
   `data_section', for your additional sections.  Do not define this
   macro if you do not define `EXTRA_SECTIONS'.

   `JUMP_TABLES_IN_TEXT_SECTION' Define this macro if jump tables (for
   `tablejump' insns) should be output in the text section, along with
   the assembler instructions.  Otherwise, the readonly data section
   is used.

   This macro is irrelevant if there is no separate readonly data
   section. */
#undef JUMP_TABLES_IN_TEXT_SECTION


/* The Overall Framework of an Assembler File

   This describes the overall framework of an assembler file.

   `ASM_COMMENT_START'

   A C string constant describing how to begin a comment in the target
   assembler language.  The compiler assumes that the comment will end
   at the end of the line. */
#define ASM_COMMENT_START ";"

/* `ASM_APP_ON'

   A C string constant for text to be output before each `asm'
   statement or group of consecutive ones.  Normally this is `"#APP"',
   which is a comment that has no effect on most assemblers but tells
   the GNU assembler that it must check the lines that follow for all
   valid assembler constructs. */
#define ASM_APP_ON "#APP\n"

/* `ASM_APP_OFF'

   A C string constant for text to be output after each `asm'
   statement or group of consecutive ones.  Normally this is
   `"#NO_APP"', which tells the GNU assembler to resume making the
   time-saving assumptions that are valid for ordinary compiler
   output. */
#define ASM_APP_OFF "#NO_APP\n"

/* Output of Data

   This describes data output.

   Output and Generation of Labels

   This is about outputting labels.

   `ASM_OUTPUT_LABEL (STREAM, NAME)'

   A C statement (sans semicolon) to output to the stdio stream STREAM
   the assembler definition of a label named NAME.  Use the expression
   `assemble_name (STREAM, NAME)' to output the name itself; before
   and after that, output the additional assembler syntax for defining
   the name, and a newline. */
#define ASM_OUTPUT_LABEL(STREAM,NAME)     \
  do { assemble_name (STREAM, NAME); fputs (":\n", STREAM); } while (0)

/* Globalizing directive for a label */
#define GLOBAL_ASM_OP "\t.global "

/* `ASM_OUTPUT_LABELREF (STREAM, NAME)'

   A C statement (sans semicolon) to output to the stdio stream STREAM
   a reference in assembler syntax to a label named NAME.  This should
   add `_' to the front of the name, if that is customary on your
   operating system, as it is in most Berkeley Unix systems.  This
   macro is used in `assemble_name'. */
#define ASM_OUTPUT_LABELREF(STREAM,NAME)  \
  asm_fprintf (STREAM, "%U%s", NAME)

/* Output of Assembler Instructions

   This describes assembler instruction output.

   `REGISTER_NAMES'

   A C initializer containing the assembler's names for the machine
   registers, each one as a C string constant.  This is what
   translates register numbers in the compiler into assembler
   language. */
#define REGISTER_NAMES \
 {"r0",  "r1",  "r2",  "r3",  "r4",  "r5",  "r6",  "r7",  \
  "r8",  "r9",  "r10", "r11", "r12", "r13", "r14", "r15", \
  "r16", "r17", "r18", "r19", "r20", "r21", "fp",  "sp",  \
  "r24", "r25", "r26", "r27", "r28", "r29", "r30", "r31", \
  "mdb", "mdc",                                           \
  "f0",  "f1",  "f2",  "f3",  "f4",  "f5",  "f6",  "f7",  \
  "f8",  "f9",  "f10", "f11", "f12", "f13", "f14", "f15", \
  "flags","argp","sfp" }

/* `ADDITIONAL_REGISTER_NAMES`

   If defined, a C initializer for an array of structures containing
   a name and a register number.  This macro defines additional names
   for hard registers, thus allowing the `asm' option in declarations
   to refer to registers using alternate names.  */
#define ADDITIONAL_REGISTER_NAMES \
  {{"r22", HARD_FRAME_POINTER_REGNUM}, {"r23", STACK_POINTER_REGNUM}}

/* `REGISTER_PREFIX'
   `LOCAL_LABEL_PREFIX'
   `USER_LABEL_PREFIX'
   `IMMEDIATE_PREFIX'

   If defined, C string expressions to be used for the `%R', `%L',
   `%U', and `%I' options of `asm_fprintf' (see `final.cc').  These are
   useful when a single `md' file must support multiple assembler
   formats.  In that case, the various `tm.h' files can define these
   macros differently. */
#define REGISTER_PREFIX ""
#define LOCAL_LABEL_PREFIX "."
#define IMMEDIATE_PREFIX "#"

/* `ASM_OUTPUT_REG_PUSH (STREAM, REGNO)'

   A C expression to output to STREAM some assembler code which will
   push hard register number REGNO onto the stack.  The code need not
   be optimal, since this macro is used only when profiling. */
#define ASM_OUTPUT_REG_PUSH(STREAM,REGNO)  \
  asm_fprintf (STREAM, "\tsubi    sp,4\n\twrite.l (sp),%s\n", \
               reg_names[REGNO])

/* `ASM_OUTPUT_REG_POP (STREAM, REGNO)'

   A C expression to output to STREAM some assembler code which will
   pop hard register number REGNO off of the stack.  The code need not
   be optimal, since this macro is used only when profiling. */
#define ASM_OUTPUT_REG_POP(STREAM,REGNO)  \
  asm_fprintf (STREAM, "\tread.l  %s,(sp)\n\taddi    sp,4\n", \
               reg_names[REGNO])


/* Output of Dispatch Tables

   This concerns dispatch tables.

   `ASM_OUTPUT_ADDR_DIFF_ELT (STREAM, VALUE, REL)'

   A C statement to output to the stdio stream STREAM an assembler
   pseudo-instruction to generate a difference between two labels.
   VALUE and REL are the numbers of two internal labels.  The
   definitions of these labels are output using
   `ASM_OUTPUT_INTERNAL_LABEL', and they must be printed in the same
   way here.

   You must provide this macro on machines where the addresses in a
   dispatch table are relative to the table's own address.  If
   defined, GNU CC will also use this macro on all machines when
   producing PIC. */
#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM,BODY,VALUE,REL)  		\
  switch (GET_MODE (BODY))						\
    {									\
    case E_SImode:							\
      asm_fprintf ((STREAM), "\t.long\t%LL%d-%LL%d\n", (VALUE),(REL));	\
      break;								\
    case E_HImode:							\
      asm_fprintf ((STREAM), "\t.word\t%LL%d-%LL%d\n", (VALUE),(REL));	\
      break;								\
    case E_QImode:							\
      asm_fprintf ((STREAM), "\t.byte\t%LL%d-%LL%d\n", (VALUE),(REL));	\
      break;								\
    default:								\
      break;								\
    }

/* `ASM_OUTPUT_ADDR_VEC_ELT (STREAM, VALUE)'

   This macro should be provided on machines where the addresses in a
   dispatch table are absolute.

   The definition should be a C statement to output to the stdio
   stream STREAM an assembler pseudo-instruction to generate a
   reference to a label.  VALUE is the number of an internal label
   whose definition is output using `ASM_OUTPUT_INTERNAL_LABEL'. */
#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE)  \
  asm_fprintf (STREAM, "\t.long   %LL%d\n", VALUE)

/* `ASM_OUTPUT_CASE_END (STREAM, NUM, TABLE)'

   Define this if something special must be output at the end of a
   jump-table. The definition should be a C statement to be executed
   after the assembler code for the table is written. It should write
   the appropriate code to stdio stream STREAM. The argument TABLE is
   the jump-table insn, and NUM is the label-number of the preceding
   label.

   If this macro is not defined, nothing special is output at the end
   of a jump table.

   Here we output a word of zero so that jump-tables can be seperated
   in reverse assembly. */
#define ASM_OUTPUT_CASE_END(STREAM, NUM, TABLE) \
  asm_fprintf (STREAM, "\t.long   0\n")

/* Support subalignment values.  */

#define SUBALIGN_LOG 3

/* Assembler Commands for Alignment

   This describes commands for alignment.

   `ASM_OUTPUT_ALIGN_CODE (STREAM)'

   A C expression to output text to align the location counter in the
   way that is desirable at a point in the code that is reached only
   by jumping.

   This macro need not be defined if you don't want any special
   alignment to be done at such a time.  Most machine descriptions do
   not currently define the macro. */
#undef ASM_OUTPUT_ALIGN_CODE

/* `ASM_OUTPUT_LOOP_ALIGN (STREAM)'

   A C expression to output text to align the location counter in the
   way that is desirable at the beginning of a loop.

   This macro need not be defined if you don't want any special
   alignment to be done at such a time.  Most machine descriptions do
   not currently define the macro. */
#undef ASM_OUTPUT_LOOP_ALIGN

/* `ASM_OUTPUT_ALIGN (STREAM, POWER)'

   A C statement to output to the stdio stream STREAM an assembler
   command to advance the location counter to a multiple of 2 to the
   POWER bytes.  POWER will be a C expression of type `int'. */
#define ASM_OUTPUT_ALIGN(STREAM,LOG)      \
  if ((LOG) != 0)                       \
    fprintf (STREAM, "\t.align  %d\n", (1 << (LOG)))

/* `ASM_OUTPUT_MAX_SKIP_ALIGN (STREAM, POWER, MAX_SKIP)`

   A C statement to output to the stdio stream STREAM an assembler
   command to advance the location counter to a multiple of 2 to the
   POWER bytes, but only if MAX_SKIP or fewer bytes are needed to
   satisfy the alignment request.  POWER and MAX_SKIP will be a C
   expression of type `int'. */
#define ASM_OUTPUT_MAX_SKIP_ALIGN(STREAM,LOG,MAX_SKIP)			\
  if ((LOG) != 0) {							\
    if ((MAX_SKIP) == 0 || (MAX_SKIP) >= (1 << (LOG)) - 1)		\
      fprintf ((STREAM), "\t.p2align %d\n", (LOG));			\
    else								\
      fprintf ((STREAM), "\t.p2align %d,,%d\n", (LOG), (MAX_SKIP));	\
  }

/* Controlling Debugging Information Format

   This describes how to specify debugging information.

    mda is known to GDB, but not to GCC. */
#define DEBUGGER_REGNO(REGNO) \
  ((REGNO) > MDB_REGNUM ? (REGNO) + 1 : (REGNO))

/* `DEBUGGER_AUTO_OFFSET (X)'

   A C expression that returns the integer offset value for an
   automatic variable having address X (an RTL expression).  The
   default computation assumes that X is based on the frame-pointer
   and gives the offset from the frame-pointer.  This is required for
   targets that produce debugging output for debugger and allow the frame-pointer
   to be eliminated when the `-g' options is used. */
#define DEBUGGER_AUTO_OFFSET(X) \
  (GET_CODE (X) == PLUS ? INTVAL (XEXP (X, 1)) : 0)

/* Miscellaneous Parameters

   `CASE_VECTOR_MODE'

   An alias for a machine mode name.  This is the machine mode that
   elements of a jump-table should have. */
#define CASE_VECTOR_MODE SImode

/* `CASE_VECTOR_PC_RELATIVE'
   Define this macro if jump-tables should contain relative addresses. */
#undef CASE_VECTOR_PC_RELATIVE

/* This says how to output assembler code to declare an
   unitialised external linkage data object. */
#define ASM_OUTPUT_COMMON(STREAM, NAME, SIZE, ROUNDED)      \
( fputs ("\n\t.comm  ", (STREAM)),                        \
  assemble_name ((STREAM), (NAME)),                         \
  fprintf ((STREAM), "," HOST_WIDE_INT_PRINT_UNSIGNED"\n", ROUNDED))

/* This says how to output assembler code to declare an
   unitialised internal linkage data object. */
#define ASM_OUTPUT_LOCAL(STREAM, NAME, SIZE, ROUNDED)     \
( fputs ("\n\t.lcomm ", (STREAM)),                      \
  assemble_name ((STREAM), (NAME)),                     \
  fprintf ((STREAM), "," HOST_WIDE_INT_PRINT_UNSIGNED"\n", ROUNDED))

/* Prettify the assembly.  */
extern int visium_indent_opcode;

#define ASM_OUTPUT_OPCODE(FILE, PTR)	\
  do {					\
    if (visium_indent_opcode)		\
      {					\
	putc (' ', FILE);		\
	visium_indent_opcode = 0;	\
      }					\
  } while (0)

/* Configure-time default values for common options.  */
#define OPTION_DEFAULT_SPECS { "cpu", "%{!mcpu=*:-mcpu=%(VALUE)}" }

/* Values of TARGET_CPU_DEFAULT specified via --with-cpu.  */
#define TARGET_CPU_gr5	0
#define TARGET_CPU_gr6	1

/* Default -mcpu multilib for above values.  */
#if TARGET_CPU_DEFAULT == TARGET_CPU_gr5
#define MULTILIB_DEFAULTS { "mcpu=gr5" }
#elif TARGET_CPU_DEFAULT == TARGET_CPU_gr6
#define MULTILIB_DEFAULTS { "mcpu=gr6" }
#else
#error Unrecognized value in TARGET_CPU_DEFAULT
#endif
