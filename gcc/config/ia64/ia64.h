/* Definitions of target machine GNU compiler.  IA64 version.
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by James E. Wilson <wilson@cygnus.com> and
   		  David Mosberger <davidm@hpl.hp.com>.

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

/* ??? Use of the upper 32 FP registers for integer values will make context
   switching slower, because the kernel only saves any registers past f32 if
   it has to.  */

/* ??? Look at ABI group documents for list of preprocessor macros and
   other features required for ABI compliance.  */

/* ??? Functions containing a non-local goto target save many registers.  Why?
   See for instance execute/920428-2.c.  */

/* ??? Get CAN_DEBUG_WITHOUT_FP working so that -fomit-frame-pointer is not
   needed.  */

/* ??? Add support for short data/bss sections.  */


/* Run-time target specifications */

/* Define this to be a string constant containing `-D' options to define the
   predefined macros that identify this machine and system.  These macros will
   be predefined unless the `-ansi' option is specified.  */
/* ??? This is undefed in svr4.h.  */
#define CPP_PREDEFINES "-Dia64 -Amachine(ia64)"

/* This declaration should be present.  */
extern int target_flags;

/* This series of macros is to allow compiler command arguments to enable or
   disable the use of optional features of the target machine.  */

#define MASK_BIG_ENDIAN	0x00000001	/* Generate big endian code.  */

#define MASK_GNU_AS	0x00000002	/* Generate code for GNU as.  */

#define MASK_GNU_LD	0x00000004	/* Generate code for GNU ld.  */

#define MASK_NO_PIC	0x00000008	/* Generate code without GP reg.  */

#define MASK_VOL_ASM_STOP 0x00000010	/* Emit stop bits for vol ext asm.  */

#define MASK_A_STEP	0x00000020	/* Emit code for Itanium A step.  */

#define MASK_REG_NAMES	0x00000040	/* Use in/loc/out register names.  */

#define MASK_NO_SDATA   0x00000080	/* Disable sdata/scommon/sbss.  */

#define MASK_DWARF2_ASM 0x40000000	/* test dwarf2 line info via gas.  */

#define TARGET_BIG_ENDIAN	(target_flags & MASK_BIG_ENDIAN)

#define TARGET_GNU_AS		(target_flags & MASK_GNU_AS)

#define TARGET_GNU_LD		(target_flags & MASK_GNU_LD)

#define TARGET_NO_PIC		(target_flags & MASK_NO_PIC)

#define TARGET_VOL_ASM_STOP	(target_flags & MASK_VOL_ASM_STOP)

#define TARGET_A_STEP		(target_flags & MASK_A_STEP)

#define TARGET_REG_NAMES	(target_flags & MASK_REG_NAMES)

#define TARGET_NO_SDATA		(target_flags & MASK_NO_SDATA)

#define TARGET_DWARF2_ASM	(target_flags & MASK_DWARF2_ASM)

/* This macro defines names of command options to set and clear bits in
   `target_flags'.  Its definition is an initializer with a subgrouping for
   each command option.  */

#define TARGET_SWITCHES \
{									\
  { "big-endian",	MASK_BIG_ENDIAN,				\
      "Generate big endian code" },					\
  { "little-endian",	-MASK_BIG_ENDIAN,				\
      "Generate little endian code" },					\
  { "gnu-as",		MASK_GNU_AS,					\
      "Generate code for GNU as" },					\
  { "no-gnu-as",	-MASK_GNU_AS,					\
      "Generate code for Intel as" },					\
  { "gnu-ld",		MASK_GNU_LD,					\
      "Generate code for GNU ld" },					\
  { "no-gnu-ld",	-MASK_GNU_LD,					\
      "Generate code for Intel ld" },					\
  { "no-pic",		MASK_NO_PIC,					\
      "Generate code without GP reg" },					\
  { "volatile-asm-stop", MASK_VOL_ASM_STOP,				\
      "Emit stop bits before and after volatile extended asms" },	\
  { "no-volatile-asm-stop", -MASK_VOL_ASM_STOP,				\
      "Don't emit stop bits before and after volatile extended asms" },	\
  { "a-step",		MASK_A_STEP,					\
      "Emit code for Itanium (TM) processor A step"},			\
  { "register-names",	MASK_REG_NAMES,					\
      "Use in/loc/out register names"},					\
  { "no-sdata",		MASK_NO_SDATA,					\
      "Disable use of sdata/scommon/sbss"},				\
  { "sdata",		-MASK_NO_SDATA,					\
      "Enable use of sdata/scommon/sbss"},				\
  { "dwarf2-asm", 	MASK_DWARF2_ASM,				\
      "Enable Dwarf 2 line debug info via GNU as"},			\
  { "no-dwarf2-asm", 	-MASK_DWARF2_ASM,				\
      "Disable Dwarf 2 line debug info via GNU as"},			\
  { "",			TARGET_DEFAULT | TARGET_CPU_DEFAULT,		\
      NULL }								\
}

/* Default target_flags if no switches are specified  */

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT MASK_DWARF2_ASM
#endif

#ifndef TARGET_CPU_DEFAULT
#define TARGET_CPU_DEFAULT 0
#endif

/* This macro is similar to `TARGET_SWITCHES' but defines names of command
   options that have values.  Its definition is an initializer with a
   subgrouping for each command option.  */

extern const char *ia64_fixed_range_string;
#define TARGET_OPTIONS \
{									\
  { "fixed-range=", 	&ia64_fixed_range_string,			\
      "Specify range of registers to make fixed."},			\
}

/* This macro is a C statement to print on `stderr' a string describing the
   particular machine description choice.  */

#define TARGET_VERSION fprintf (stderr, " (IA-64)");

/* Sometimes certain combinations of command options do not make sense on a
   particular target machine.  You can define a macro `OVERRIDE_OPTIONS' to
   take account of this.  This macro, if defined, is executed once just after
   all the command options have been parsed.  */

#define OVERRIDE_OPTIONS ia64_override_options ()

/* Some machines may desire to change what optimizations are performed for
   various optimization levels.  This macro, if defined, is executed once just
   after the optimization level is determined and before the remainder of the
   command options have been parsed.  Values set in this macro are used as the
   default values for the other command line options.  */

/* #define OPTIMIZATION_OPTIONS(LEVEL,SIZE) */

/* Define this macro if debugging can be performed even without a frame
   pointer.  If this macro is defined, GNU CC will turn on the
   `-fomit-frame-pointer' option whenever `-O' is specified.  */
/* ??? Need to define this.  */
/* #define CAN_DEBUG_WITHOUT_FP */


/* Driver configuration */

/* A C string constant that tells the GNU CC driver program options to pass to
   CPP.  It can also specify how to translate options you give to GNU CC into
   options for GNU CC to pass to the CPP.  */

/* ??? __LONG_MAX__ depends on LP64/ILP32 switch.  */
/* ??? An alternative is to modify glimits.h to check for __LP64__ instead
   of checked for CPU specific defines.  We could also get rid of all LONG_MAX
   defines in other tm.h files.  */
#define CPP_SPEC \
  "%{mcpu=itanium:-D__itanium__} %{mbig-endian:-D__BIG_ENDIAN__}	\
   -D__LONG_MAX__=9223372036854775807L"

/* If this macro is defined, the preprocessor will not define the builtin macro
   `__SIZE_TYPE__'.  The macro `__SIZE_TYPE__' must then be defined by
   `CPP_SPEC' instead.

   This should be defined if `SIZE_TYPE' depends on target dependent flags
   which are not accessible to the preprocessor.  Otherwise, it should not be
   defined.  */
/* ??? Needs to be defined for P64 code.  */
/* #define NO_BUILTIN_SIZE_TYPE */

/* If this macro is defined, the preprocessor will not define the builtin macro
   `__PTRDIFF_TYPE__'.  The macro `__PTRDIFF_TYPE__' must then be defined by
   `CPP_SPEC' instead.

   This should be defined if `PTRDIFF_TYPE' depends on target dependent flags
   which are not accessible to the preprocessor.  Otherwise, it should not be
   defined.  */
/* ??? Needs to be defined for P64 code.  */
/* #define NO_BUILTIN_PTRDIFF_TYPE */

/* A C string constant that tells the GNU CC driver program options to pass to
   `cc1'.  It can also specify how to translate options you give to GNU CC into
   options for GNU CC to pass to the `cc1'.  */

/* #define CC1_SPEC "" */

/* A C string constant that tells the GNU CC driver program options to pass to
   `cc1plus'.  It can also specify how to translate options you give to GNU CC
   into options for GNU CC to pass to the `cc1plus'.  */

/* #define CC1PLUS_SPEC "" */

/* A C string constant that tells the GNU CC driver program options to pass to
   the assembler.  It can also specify how to translate options you give to GNU
   CC into options for GNU CC to pass to the assembler.  */

#if ((TARGET_CPU_DEFAULT | TARGET_DEFAULT) & MASK_GNU_AS) != 0
/* GNU AS.  */
#define ASM_SPEC "%{mno-gnu-as:-N so}"
#else
/* Intel ias.  */
#define ASM_SPEC "%{!mgnu-as:-N so}"
#endif

/* A C string constant that tells the GNU CC driver program options to pass to
   the linker.  It can also specify how to translate options you give to GNU CC
   into options for GNU CC to pass to the linker.  */

/* The Intel linker does not support dynamic linking, so we need -dn.
   The Intel linker gives annoying messages unless -N so is used.  */
#if ((TARGET_CPU_DEFAULT | TARGET_DEFAULT) & MASK_GNU_LD) != 0
/* GNU LD.  */
#define LINK_SPEC "%{mno-gnu-ld:-dn -N so}"
#else
/* Intel ild.  */
#define LINK_SPEC "%{!mgnu-ld:-dn -N so}"
#endif


/* Storage Layout */

/* Define this macro to have the value 1 if the most significant bit in a byte
   has the lowest number; otherwise define it to have the value zero.  */

#define BITS_BIG_ENDIAN 0

/* Define this macro to have the value 1 if the most significant byte in a word
   has the lowest number.  This macro need not be a constant.  */

#define BYTES_BIG_ENDIAN (TARGET_BIG_ENDIAN != 0)

/* Define this macro to have the value 1 if, in a multiword object, the most
   significant word has the lowest number.  */

#define WORDS_BIG_ENDIAN (TARGET_BIG_ENDIAN != 0)

/* Define this macro if WORDS_BIG_ENDIAN is not constant.  This must be a
   constant value with the same meaning as WORDS_BIG_ENDIAN, which will be used
   only when compiling libgcc2.c.  Typically the value will be set based on
   preprocessor defines.  */
#if defined(__BIG_ENDIAN__)
#define LIBGCC2_WORDS_BIG_ENDIAN 1
#else
#define LIBGCC2_WORDS_BIG_ENDIAN 0
#endif

/* Define this macro to be the number of bits in an addressable storage unit
   (byte); normally 8.  */
#define BITS_PER_UNIT 8

/* Number of bits in a word; normally 32.  */
#define BITS_PER_WORD 64

/* Number of storage units in a word; normally 4.  */
#define UNITS_PER_WORD 8

/* Width of a pointer, in bits.  You must specify a value no wider than the
   width of `Pmode'.  If it is not equal to the width of `Pmode', you must
   define `POINTERS_EXTEND_UNSIGNED'.  */
/* ??? Implement optional 32 bit pointer size later?  */
#define POINTER_SIZE 64

/* A C expression whose value is nonzero if pointers that need to be extended
   from being `POINTER_SIZE' bits wide to `Pmode' are sign-extended and zero if
   they are zero-extended.

   You need not define this macro if the `POINTER_SIZE' is equal to the width
   of `Pmode'.  */
/* ??? May need this for 32 bit pointers.  */
/* #define POINTERS_EXTEND_UNSIGNED */

/* A macro to update MODE and UNSIGNEDP when an object whose type is TYPE and
   which has the specified mode and signedness is to be stored in a register.
   This macro is only called when TYPE is a scalar type.  */

/* ??? Maybe sign-extend 32 bit values like the alpha?  Or maybe zero-extend
   because we only have zero-extending loads? */
#define PROMOTE_MODE(MODE,UNSIGNEDP,TYPE)				\
do									\
  {									\
    if (GET_MODE_CLASS (MODE) == MODE_INT				\
	&& GET_MODE_SIZE (MODE) < UNITS_PER_WORD)			\
      (MODE) = DImode;							\
  }									\
while (0)

/* Define this macro if the promotion described by `PROMOTE_MODE' should also
   be done for outgoing function arguments.  */
/* ??? ABI doesn't allow us to define this.  */
/* #define PROMOTE_FUNCTION_ARGS */

/* Define this macro if the promotion described by `PROMOTE_MODE' should also
   be done for the return value of functions.

   If this macro is defined, `FUNCTION_VALUE' must perform the same promotions
   done by `PROMOTE_MODE'.  */
/* ??? ABI doesn't allow us to define this.  */
/* #define PROMOTE_FUNCTION_RETURN */

/* Normal alignment required for function parameters on the stack, in bits.
   All stack parameters receive at least this much alignment regardless of data
   type.  On most machines, this is the same as the size of an integer.  */
#define PARM_BOUNDARY 64

/* Define this macro if you wish to preserve a certain alignment for the stack
   pointer.  The definition is a C expression for the desired alignment
   (measured in bits).  */

#define STACK_BOUNDARY 128

/* Align frames on double word boundaries */
#ifndef IA64_STACK_ALIGN
#define IA64_STACK_ALIGN(LOC) (((LOC) + 15) & ~15)
#endif

/* Alignment required for a function entry point, in bits.  */
#define FUNCTION_BOUNDARY 128

/* Biggest alignment that any data type can require on this machine,
   in bits.  */
/* Optional x86 80-bit float, quad-precision 128-bit float, and quad-word
   128 bit integers all require 128 bit alignment.  */
#define BIGGEST_ALIGNMENT 128

/* If defined, a C expression to compute the alignment for a static variable.
   TYPE is the data type, and ALIGN is the alignment that the object
   would ordinarily have.  The value of this macro is used instead of that
   alignment to align the object.  */

#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

/* If defined, a C expression to compute the alignment given to a constant that
   is being placed in memory.  CONSTANT is the constant and ALIGN is the
   alignment that the object would ordinarily have.  The value of this macro is
   used instead of that alignment to align the object.  */

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
   a field, not crossing a boundary for it.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* Define this macro as an expression for the overall size of a structure
   (given by STRUCT as a tree node) when the size computed from the fields is
   SIZE and the alignment is ALIGN.

   The default is to round SIZE up to a multiple of ALIGN.  */
/* ??? Might need this for 80-bit double-extended floats.  */
/* #define ROUND_TYPE_SIZE(STRUCT, SIZE, ALIGN) */

/* Define this macro as an expression for the alignment of a structure (given
   by STRUCT as a tree node) if the alignment computed in the usual way is
   COMPUTED and the alignment explicitly specified was SPECIFIED.

   The default is to use SPECIFIED if it is larger; otherwise, use the smaller
   of COMPUTED and `BIGGEST_ALIGNMENT' */
/* ??? Might need this for 80-bit double-extended floats.  */
/* #define ROUND_TYPE_ALIGN(STRUCT, COMPUTED, SPECIFIED) */

/* An integer expression for the size in bits of the largest integer machine
   mode that should actually be used.  */

/* Allow pairs of registers to be used, which is the intent of the default.  */
#define MAX_FIXED_MODE_SIZE GET_MODE_BITSIZE (TImode)

/* A code distinguishing the floating point format of the target machine.  */
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


/* Layout of Source Language Data Types */

/* A C expression for the size in bits of the type `int' on the target machine.
   If you don't define this, the default is one word.  */
#define INT_TYPE_SIZE 32

/* A C expression for the size in bits of the type `short' on the target
   machine.  If you don't define this, the default is half a word.  (If this
   would be less than one storage unit, it is rounded up to one unit.)  */
#define SHORT_TYPE_SIZE 16

/* A C expression for the size in bits of the type `long' on the target
   machine.  If you don't define this, the default is one word.  */
/* ??? Should be 32 for ILP32 code.  */
#define LONG_TYPE_SIZE 64

/* Maximum number for the size in bits of the type `long' on the target
   machine.  If this is undefined, the default is `LONG_TYPE_SIZE'.  Otherwise,
   it is the constant value that is the largest value that `LONG_TYPE_SIZE' can
   have at run-time.  This is used in `cpp'.  */
/* ??? Should be 64 for ILP32 code.  */
/* #define MAX_LONG_TYPE_SIZE */

/* A C expression for the size in bits of the type `long long' on the target
   machine.  If you don't define this, the default is two words.  If you want
   to support GNU Ada on your machine, the value of macro must be at least 64.  */
#define LONG_LONG_TYPE_SIZE 64

/* A C expression for the size in bits of the type `char' on the target
   machine.  If you don't define this, the default is one quarter of a word.
   (If this would be less than one storage unit, it is rounded up to one unit.)  */
#define CHAR_TYPE_SIZE 8

/* A C expression for the size in bits of the type `float' on the target
   machine.  If you don't define this, the default is one word.  */
#define FLOAT_TYPE_SIZE 32

/* A C expression for the size in bits of the type `double' on the target
   machine.  If you don't define this, the default is two words.  */
#define DOUBLE_TYPE_SIZE 64

/* A C expression for the size in bits of the type `long double' on the target
   machine.  If you don't define this, the default is two words.  */
/* ??? We have an 80 bit extended double format.  */
#define LONG_DOUBLE_TYPE_SIZE 64

/* An expression whose value is 1 or 0, according to whether the type `char'
   should be signed or unsigned by default.  The user can always override this
   default with the options `-fsigned-char' and `-funsigned-char'.  */
#define DEFAULT_SIGNED_CHAR 1

/* A C expression for a string describing the name of the data type to use for
   size values.  The typedef name `size_t' is defined using the contents of the
   string.  */
/* ??? Needs to be defined for P64 code.  */
/* #define SIZE_TYPE */

/* A C expression for a string describing the name of the data type to use for
   the result of subtracting two pointers.  The typedef name `ptrdiff_t' is
   defined using the contents of the string.  See `SIZE_TYPE' above for more
   information.  */
/* ??? Needs to be defined for P64 code.  */
/* #define PTRDIFF_TYPE */

/* A C expression for a string describing the name of the data type to use for
   wide characters.  The typedef name `wchar_t' is defined using the contents
   of the string.  See `SIZE_TYPE' above for more information.  */
/* #define WCHAR_TYPE */

/* A C expression for the size in bits of the data type for wide characters.
   This is used in `cpp', which cannot make use of `WCHAR_TYPE'.  */
/* #define WCHAR_TYPE_SIZE */

/* Maximum number for the size in bits of the data type for wide characters.
   If this is undefined, the default is `WCHAR_TYPE_SIZE'.  Otherwise, it is
   the constant value that is the largest value that `WCHAR_TYPE_SIZE' can have
   at run-time.  This is used in `cpp'.  */
/* #define MAX_WCHAR_TYPE_SIZE */

/* A C constant expression for the integer value for escape sequence
   `\a'.  */
#define TARGET_BELL 0x7

/* C constant expressions for the integer values for escape sequences
   `\b', `\t' and `\n'.  */
#define TARGET_BS	0x8
#define TARGET_TAB	0x9
#define TARGET_NEWLINE	0xa

/* C constant expressions for the integer values for escape sequences
   `\v', `\f' and `\r'.  */
#define TARGET_VT	0xb
#define TARGET_FF	0xc
#define TARGET_CR	0xd


/* Register Basics */

/* Number of hardware registers known to the compiler.  
   We have 128 general registers, 128 floating point registers, 64 predicate
   registers, 8 branch registers, and one frame pointer register.  */

/* ??? Should add ar.lc, ar.ec and probably also ar.pfs.  */

#define FIRST_PSEUDO_REGISTER 330

/* Ranges for the various kinds of registers.  */
#define ADDL_REGNO_P(REGNO) ((REGNO) >= 0 && (REGNO) <= 3)
#define GR_REGNO_P(REGNO) ((REGNO) >= 0 && (REGNO) <= 127)
#define FR_FP_REGNO_P(REGNO) \
  (((REGNO) >= 128 && (REGNO) <= 143) || ((REGNO) >= 152 && (REGNO) <= 223))
#define FR_INT_REGNO_P(REGNO) \
  (((REGNO) >= 144 && (REGNO) <= 151) || ((REGNO) >= 224 && (REGNO) <= 255))
#define FR_REGNO_P(REGNO) ((REGNO) >= 128 && (REGNO) <= 255)
#define PR_REGNO_P(REGNO) ((REGNO) >= 256 && (REGNO) <= 319)
#define BR_REGNO_P(REGNO) ((REGNO) >= 320 && (REGNO) <= 327)
#define GENERAL_REGNO_P(REGNO) \
  (GR_REGNO_P (REGNO)							\
   || (REGNO) == FRAME_POINTER_REGNUM					\
   || (REGNO) == RETURN_ADDRESS_REGNUM)

#define GR_REG(REGNO) ((REGNO) + 0)
#define FR_REG(REGNO) ((REGNO) + 128)
#define PR_REG(REGNO) ((REGNO) + 256)
#define BR_REG(REGNO) ((REGNO) + 320)
#define OUT_REG(REGNO) ((REGNO) + 120)
#define IN_REG(REGNO) ((REGNO) + 112)
#define LOC_REG(REGNO) ((REGNO) + 32)

#define IN_REGNO_P(REGNO) ((REGNO) >= IN_REG (0) && (REGNO) <= IN_REG (7))
#define LOC_REGNO_P(REGNO) ((REGNO) >= LOC_REG (0) && (REGNO) <= LOC_REG (79))
#define OUT_REGNO_P(REGNO) ((REGNO) >= OUT_REG (0) && (REGNO) <= OUT_REG (7))

/* ??? Don't really need two sets of macros.  I like this one better because
   it is less typing.  */
#define R_GR(REGNO) GR_REG (REGNO)
#define R_FR(REGNO) FR_REG (REGNO)
#define R_PR(REGNO) PR_REG (REGNO)
#define R_BR(REGNO) BR_REG (REGNO)

/* An initializer that says which registers are used for fixed purposes all
   throughout the compiled code and are therefore not available for general
   allocation.

   r0: constant 0
   r1: global pointer (gp)
   r12: stack pointer (sp)
   r13: thread pointer (tp)
   f0: constant 0.0
   f1: constant 1.0
   p0: constant true
   fp: eliminable frame pointer */   

/* The last 16 stacked regs are fixed, because they are reserved for the 8
   input and 8 output registers.  */

/* ??? Must mark the next 3 stacked regs as fixed, because ia64_expand_prologue
   assumes that three locals are available for fp, b0, and ar.pfs.  */

/* ??? Should mark b0 as fixed?  */

/* ??? input and output registers do not have to be marked as fixed.  */

#define FIXED_REGISTERS \
{ /* General registers.  */				\
  1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	\
  /* Floating-point registers.  */			\
  1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
  /* Predicate registers.  */				\
  1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
  /* Branch registers.  */				\
  0, 0, 0, 0, 0, 0, 0, 0,				\
  /*FP RA*/						\
  1, 1,							\
 }

/* Like `FIXED_REGISTERS' but has 1 for each register that is clobbered (in
   general) by function calls as well as for fixed registers.  This macro
   therefore identifies the registers that are not available for general
   allocation of values that must live across function calls.  */

/* ??? If inputs are not marked as fixed, then they are not call clobbered.  */

#define CALL_USED_REGISTERS \
{ /* General registers.  */				\
  1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	\
  /* Floating-point registers.  */			\
  1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	\
  /* Predicate registers.  */				\
  1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
  /* Branch registers.  */				\
  1, 0, 0, 0, 0, 0, 1, 1,				\
  /*FP RA*/						\
  1, 1,							\
}

/* Define this macro if the target machine has register windows.  This C
   expression returns the register number as seen by the called function
   corresponding to the register number OUT as seen by the calling function.
   Return OUT if register number OUT is not an outbound register.  */

#define INCOMING_REGNO(OUT) \
  ((unsigned) ((OUT) - OUT_REG (0)) < 8 ? IN_REG ((OUT) - OUT_REG (0)) : (OUT))

/* Define this macro if the target machine has register windows.  This C
   expression returns the register number as seen by the calling function
   corresponding to the register number IN as seen by the called function.
   Return IN if register number IN is not an inbound register.  */

#define OUTGOING_REGNO(IN) \
  ((unsigned) ((IN) - IN_REG (0)) < 8 ? OUT_REG ((IN) - IN_REG (0)) : (IN))


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

/* ??? Should the GR return value registers come before or after the rest
   of the caller-save GRs?  */

/* ??? Output registers are cheap, because they will be not be saved
   by the register engine.  They probably should be early in the list.
   We need to make them not fixed first though.  Similarly, input registers
   are callee-saved (RSE) like the stacked locals.  */

#define REG_ALLOC_ORDER \
{									   \
  /* Caller-saved general registers.  */				   \
  R_GR (14), R_GR (15), R_GR (16), R_GR (17), 				   \
  R_GR (18), R_GR (19), R_GR (20), R_GR (21), R_GR (22), R_GR (23), 	   \
  R_GR (24), R_GR (25), R_GR (26), R_GR (27), R_GR (28), R_GR (29), 	   \
  R_GR (30), R_GR (31),							   \
  /* Caller-saved general registers, also used for return values.  */	   \
  R_GR (8), R_GR (9), R_GR (10), R_GR (11), 				   \
  /* addl caller-saved general registers.  */				   \
  R_GR (2), R_GR (3),							   \
  /* Caller-saved FP registers.  */					   \
  R_FR (6), R_FR (7),							   \
  /* Caller-saved FP registers, used for parameters and return values.  */ \
  R_FR (8), R_FR (9), R_FR (10), R_FR (11), 				   \
  R_FR (12), R_FR (13), R_FR (14), R_FR (15), 				   \
  /* Rotating caller-saved FP registers.  */				   \
  R_FR (32), R_FR (33), R_FR (34), R_FR (35), 				   \
  R_FR (36), R_FR (37), R_FR (38), R_FR (39), R_FR (40), R_FR (41), 	   \
  R_FR (42), R_FR (43), R_FR (44), R_FR (45), R_FR (46), R_FR (47), 	   \
  R_FR (48), R_FR (49), R_FR (50), R_FR (51), R_FR (52), R_FR (53), 	   \
  R_FR (54), R_FR (55), R_FR (56), R_FR (57), R_FR (58), R_FR (59), 	   \
  R_FR (60), R_FR (61), R_FR (62), R_FR (63), R_FR (64), R_FR (65), 	   \
  R_FR (66), R_FR (67), R_FR (68), R_FR (69), R_FR (70), R_FR (71), 	   \
  R_FR (72), R_FR (73), R_FR (74), R_FR (75), R_FR (76), R_FR (77), 	   \
  R_FR (78), R_FR (79), R_FR (80), R_FR (81), R_FR (82), R_FR (83), 	   \
  R_FR (84), R_FR (85), R_FR (86), R_FR (87), R_FR (88), R_FR (89), 	   \
  R_FR (90), R_FR (91), R_FR (92), R_FR (93), R_FR (94), R_FR (95), 	   \
  R_FR (96), R_FR (97), R_FR (98), R_FR (99), R_FR (100), R_FR (101), 	   \
  R_FR (102), R_FR (103), R_FR (104), R_FR (105), R_FR (106), R_FR (107),  \
  R_FR (108), R_FR (109), R_FR (110), R_FR (111), R_FR (112), R_FR (113),  \
  R_FR (114), R_FR (115), R_FR (116), R_FR (117), R_FR (118), R_FR (119),  \
  R_FR (120), R_FR (121), R_FR (122), R_FR (123), R_FR (124), R_FR (125),  \
  R_FR (126), R_FR (127), 						   \
  /* Caller-saved predicate registers.  */				   \
  R_PR (6), R_PR (7), R_PR (8), R_PR (9), R_PR (10), R_PR (11), 	   \
  R_PR (12), R_PR (13), R_PR (14), R_PR (15),				   \
  /* Rotating caller-saved predicate registers.  */			   \
  R_PR (16), R_PR (17), 						   \
  R_PR (18), R_PR (19), R_PR (20), R_PR (21), R_PR (22), R_PR (23), 	   \
  R_PR (24), R_PR (25), R_PR (26), R_PR (27), R_PR (28), R_PR (29), 	   \
  R_PR (30), R_PR (31), R_PR (32), R_PR (33), R_PR (34), R_PR (35), 	   \
  R_PR (36), R_PR (37), R_PR (38), R_PR (39), R_PR (40), R_PR (41), 	   \
  R_PR (42), R_PR (43), R_PR (44), R_PR (45), R_PR (46), R_PR (47), 	   \
  R_PR (48), R_PR (49), R_PR (50), R_PR (51), R_PR (52), R_PR (53), 	   \
  R_PR (54), R_PR (55), R_PR (56), R_PR (57), R_PR (58), R_PR (59), 	   \
  R_PR (60), R_PR (61), R_PR (62), R_PR (63), 				   \
  /* Caller-saved branch registers.  */					   \
  R_BR (6), R_BR (7),							   \
									   \
  /* Stacked callee-saved general registers.  */			   \
  R_GR (32), R_GR (33), R_GR (34), R_GR (35), 				   \
  R_GR (36), R_GR (37), R_GR (38), R_GR (39), R_GR (40), R_GR (41), 	   \
  R_GR (42), R_GR (43), R_GR (44), R_GR (45), R_GR (46), R_GR (47), 	   \
  R_GR (48), R_GR (49), R_GR (50), R_GR (51), R_GR (52), R_GR (53), 	   \
  R_GR (54), R_GR (55), R_GR (56), R_GR (57), R_GR (58), R_GR (59), 	   \
  R_GR (60), R_GR (61), R_GR (62), R_GR (63), R_GR (64), R_GR (65), 	   \
  R_GR (66), R_GR (67), R_GR (68), R_GR (69), R_GR (70), R_GR (71), 	   \
  R_GR (72), R_GR (73), R_GR (74), R_GR (75), R_GR (76), R_GR (77), 	   \
  R_GR (78), R_GR (79), R_GR (80), R_GR (81), R_GR (82), R_GR (83), 	   \
  R_GR (84), R_GR (85), R_GR (86), R_GR (87), R_GR (88), R_GR (89), 	   \
  R_GR (90), R_GR (91), R_GR (92), R_GR (93), R_GR (94), R_GR (95), 	   \
  R_GR (96), R_GR (97), R_GR (98), R_GR (99), R_GR (100), R_GR (101), 	   \
  R_GR (102), R_GR (103), R_GR (104), R_GR (105), R_GR (106), R_GR (107),  \
  R_GR (108),								   \
  /* Callee-saved general registers.  */				   \
  R_GR (4), R_GR (5), R_GR (6), R_GR (7),				   \
  /* Callee-saved FP registers.  */					   \
  R_FR (2), R_FR (3), R_FR (4), R_FR (5), R_FR (16), R_FR (17), 	   \
  R_FR (18), R_FR (19), R_FR (20), R_FR (21), R_FR (22), R_FR (23), 	   \
  R_FR (24), R_FR (25), R_FR (26), R_FR (27), R_FR (28), R_FR (29), 	   \
  R_FR (30), R_FR (31),							   \
  /* Callee-saved predicate registers.  */				   \
  R_PR (1), R_PR (2), R_PR (3), R_PR (4), R_PR (5), 			   \
  /* Callee-saved branch registers.  */					   \
  R_BR (1), R_BR (2), R_BR (3), R_BR (4), R_BR (5),			   \
									   \
  /* ??? Stacked registers reserved for fp, rp, and ar.pfs.  */		   \
  R_GR (109), R_GR (110), R_GR (111),					   \
  /* Input registers.  */						   \
  R_GR (112), R_GR (113), R_GR (114), R_GR (115), R_GR (116), R_GR (117),  \
  R_GR (118), R_GR (119),						   \
  /* Output registers.  */						   \
  R_GR (120), R_GR (121), R_GR (122), R_GR (123), R_GR (124), R_GR (125),  \
  R_GR (126), R_GR (127), 						   \
									   \
  /* Special general registers.  */					   \
  R_GR (0), R_GR (1), R_GR (12), R_GR (13), 				   \
  /* Special FP registers.  */						   \
  R_FR (0), R_FR (1),							   \
  /* Special predicate registers.  */					   \
  R_PR (0),								   \
  /* Special branch registers.  */					   \
  R_BR (0),								   \
  /* Frame pointer.  Return address.  */				   \
  FRAME_POINTER_REGNUM, RETURN_ADDRESS_REGNUM,				   \
}


/* How Values Fit in Registers */

/* A C expression for the number of consecutive hard registers, starting at
   register number REGNO, required to hold a value of mode MODE.  */

/* ??? x86 80-bit FP values only require 1 register.  */
/* ??? We say that CCmode values require two registers.  This allows us to
   easily store the normal and inverted values.  If we want single register
   predicates, we can use EXTRA_CC_MODES to give them a different mode.  */

#define HARD_REGNO_NREGS(REGNO, MODE) \
  ((MODE) == CCmode && PR_REGNO_P (REGNO) ? 2				\
   : FR_REGNO_P (REGNO) && (MODE) == XFmode ? 1				\
   : (GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* A C expression that is nonzero if it is permissible to store a value of mode
   MODE in hard register number REGNO (or in several registers starting with
   that one).  */

#define HARD_REGNO_MODE_OK(REGNO, MODE) \
  (FR_FP_REGNO_P (REGNO) ? ! INTEGRAL_MODE_P (MODE)			\
   : FR_INT_REGNO_P (REGNO) ? ! FLOAT_MODE_P (MODE)			\
   : PR_REGNO_P (REGNO) ? (MODE) == CCmode				\
   : 1)

/* A C expression that is nonzero if it is desirable to choose register
   allocation so as to avoid move instructions between a value of mode MODE1
   and a value of mode MODE2.

   If `HARD_REGNO_MODE_OK (R, MODE1)' and `HARD_REGNO_MODE_OK (R, MODE2)' are
   ever different for any R, then `MODES_TIEABLE_P (MODE1, MODE2)' must be
   zero.  */
/* ??? If the comments are true, then this must be zero if one mode is CCmode,
   INTEGRAL_MODE_P or FLOAT_MODE_P and the other is not.  Otherwise, it is
   true.  */
#define MODES_TIEABLE_P(MODE1, MODE2) 1

/* Define this macro if the compiler should avoid copies to/from CCmode
   registers.  You should only define this macro if support fo copying to/from
   CCmode is incomplete.  */
/* ??? CCmode copies are very expensive, so we might want this defined.  */
/* #define AVOID_CCMODE_COPIES */


/* Handling Leaf Functions */

/* A C initializer for a vector, indexed by hard register number, which
   contains 1 for a register that is allowable in a candidate for leaf function
   treatment.  */
/* ??? This might be useful.  */
/* #define LEAF_REGISTERS */

/* A C expression whose value is the register number to which REGNO should be
   renumbered, when a function is treated as a leaf function.  */
/* ??? This might be useful.  */
/* #define LEAF_REG_REMAP(REGNO) */


/* Register Classes */

/* An enumeral type that must be defined with all the register class names as
   enumeral values.  `NO_REGS' must be first.  `ALL_REGS' must be the last
   register class, followed by one more enumeral value, `LIM_REG_CLASSES',
   which is not a register class but rather tells how many classes there
   are.  */
/* ??? FP registers hold INT and FP values in different representations, so
   we can't just use a subreg to convert between the two.  We get around this
   problem by segmenting the FP register set into two parts.  One part (FR_INT)
   only holds integer values, and one part (FR_FP) only hold FP values.  Thus
   we always know which representation is being used.  */
/* ??? When compiling without optimization, it is possible for the only use of
   a pseudo to be a parameter load from the stack with a REG_EQUIV note.
   Regclass handles this case specially and does not assign any costs to the
   pseudo.  The pseudo then ends up using the last class before ALL_REGS.
   Thus we must not let either PR_REGS or BR_REGS be the last class.  The
   testcase for this is gcc.c-torture/execute/va-arg-7.c.  */
enum reg_class
{
  NO_REGS,
  PR_REGS,
  BR_REGS,
  ADDL_REGS,
  GR_REGS,
  FR_INT_REGS,
  FR_FP_REGS,
  FR_REGS,
  GR_AND_FR_INT_REGS,
  GR_AND_FR_FP_REGS,
  GR_AND_FR_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define GENERAL_REGS GR_REGS

/* The number of distinct register classes.  */
#define N_REG_CLASSES ((int) LIM_REG_CLASSES)

/* An initializer containing the names of the register classes as C string
   constants.  These names are used in writing some of the debugging dumps.  */
#define REG_CLASS_NAMES \
{ "NO_REGS", "PR_REGS", "BR_REGS", "ADDL_REGS", "GR_REGS", "FR_INT_REGS", \
  "FR_FP_REGS", "FR_REGS", "GR_AND_FR_INT_REGS", "GR_AND_FR_FP_REGS",	  \
  "GR_AND_FR_REGS", "ALL_REGS" }

/* An initializer containing the contents of the register classes, as integers
   which are bit masks.  The Nth integer specifies the contents of class N.
   The way the integer MASK is interpreted is that register R is in the class
   if `MASK & (1 << R)' is 1.  */
#define REG_CLASS_CONTENTS \
{ 							\
  /* NO_REGS.  */					\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000,	\
    0x00000000, 0x00000000, 0x00000000, 0x00000000,	\
    0x00000000, 0x00000000, 0x000 },			\
  /* PR_REGS.  */					\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000,	\
    0x00000000, 0x00000000, 0x00000000, 0x00000000,	\
    0xFFFFFFFF, 0xFFFFFFFF, 0x000 },			\
  /* BR_REGS.  */					\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000,	\
    0x00000000, 0x00000000, 0x00000000, 0x00000000,	\
    0x00000000, 0x00000000, 0x0FF },			\
  /* ADDL_REGS.  */					\
  { 0x0000000F, 0x00000000, 0x00000000, 0x00000000,	\
    0x00000000, 0x00000000, 0x00000000, 0x00000000,	\
    0x00000000, 0x00000000, 0x000 },			\
  /* GR_REGS.  */					\
  { 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,	\
    0x00000000, 0x00000000, 0x00000000, 0x00000000,	\
    0x00000000, 0x00000000, 0x300 },			\
  /* FR_INT_REGS.  */					\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000,	\
    0x00FF0000, 0x00000000, 0x00000000, 0xFFFFFFFF,	\
    0x00000000, 0x00000000, 0x000 },			\
  /* FR_FP_REGS.  */					\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000,	\
    0xFF00FFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0x00000000,	\
    0x00000000, 0x00000000, 0x000 },			\
  /* FR_REGS.  */					\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000,	\
    0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,	\
    0x00000000, 0x00000000, 0x000 },			\
  /* GR_AND_FR_INT_REGS.  */				\
  { 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,	\
    0x00FF0000, 0x00000000, 0x00000000, 0xFFFFFFFF,	\
    0x00000000, 0x00000000, 0x300 },			\
  /* GR_AND_FR_FP_REGS.  */				\
  { 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,	\
    0xFF00FFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0x00000000,	\
    0x00000000, 0x00000000, 0x300 },			\
  /* GR_AND_FR_REGS.  */				\
  { 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,	\
    0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,	\
    0x00000000, 0x00000000, 0x300 },			\
  /* ALL_REGS.  */					\
  { 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,	\
    0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,	\
    0xFFFFFFFF, 0xFFFFFFFF, 0x3FF },			\
}

/* A C expression whose value is a register class containing hard register
   REGNO.  In general there is more than one such class; choose a class which
   is "minimal", meaning that no smaller class also contains the register.  */
/* The NO_REGS case is primarily for the benefit of rws_access_reg, which
   may call here with private (invalid) register numbers, such as
   REG_VOLATILE.  */
#define REGNO_REG_CLASS(REGNO) \
(ADDL_REGNO_P (REGNO) ? ADDL_REGS	\
 : GENERAL_REGNO_P (REGNO) ? GR_REGS	\
 : FR_FP_REGNO_P (REGNO) ? FR_FP_REGS	\
 : FR_INT_REGNO_P (REGNO) ? FR_INT_REGS	\
 : PR_REGNO_P (REGNO) ? PR_REGS		\
 : BR_REGNO_P (REGNO) ? BR_REGS		\
 : NO_REGS)

/* A macro whose definition is the name of the class to which a valid base
   register must belong.  A base register is one used in an address which is
   the register value plus a displacement.  */
#define BASE_REG_CLASS GENERAL_REGS

/* A macro whose definition is the name of the class to which a valid index
   register must belong.  An index register is one used in an address where its
   value is either multiplied by a scale factor or added to another register
   (as well as added to a displacement).  */
#define INDEX_REG_CLASS NO_REGS

/* A C expression which defines the machine-dependent operand constraint
   letters for register classes.  If CHAR is such a letter, the value should be
   the register class corresponding to it.  Otherwise, the value should be
   `NO_REGS'.  The register letter `r', corresponding to class `GENERAL_REGS',
   will not be passed to this macro; you do not need to handle it.  */

#define REG_CLASS_FROM_LETTER(CHAR) \
((CHAR) == 'f' ? FR_FP_REGS		\
 : (CHAR) == 'e' ? FR_INT_REGS		\
 : (CHAR) == 'a' ? ADDL_REGS		\
 : (CHAR) == 'b' ? BR_REGS		\
 : (CHAR) == 'c' ? PR_REGS		\
 : NO_REGS)

/* A C expression which is nonzero if register number NUM is suitable for use
   as a base register in operand addresses.  It may be either a suitable hard
   register or a pseudo register that has been allocated such a hard reg.  */
#define REGNO_OK_FOR_BASE_P(REGNO) \
  (GENERAL_REGNO_P (REGNO) || GENERAL_REGNO_P (reg_renumber[REGNO]))

/* A C expression which is nonzero if register number NUM is suitable for use
   as an index register in operand addresses.  It may be either a suitable hard
   register or a pseudo register that has been allocated such a hard reg.  */
#define REGNO_OK_FOR_INDEX_P(NUM) 0

/* A C expression that places additional restrictions on the register class to
   use when it is necessary to copy value X into a register in class CLASS.
   The value is a register class; perhaps CLASS, or perhaps another, smaller
   class.  */

#define PREFERRED_RELOAD_CLASS(X, CLASS) CLASS

/* You should define this macro to indicate to the reload phase that it may
   need to allocate at least one register for a reload in addition to the
   register to contain the data.  Specifically, if copying X to a register
   CLASS in MODE requires an intermediate register, you should define this
   to return the largest register class all of whose registers can be used
   as intermediate registers or scratch registers.  */

#define SECONDARY_RELOAD_CLASS(CLASS, MODE, X) \
 ia64_secondary_reload_class (CLASS, MODE, X)

/* Certain machines have the property that some registers cannot be copied to
   some other registers without using memory.  Define this macro on those
   machines to be a C expression that is non-zero if objects of mode M in
   registers of CLASS1 can only be copied to registers of class CLASS2 by
   storing a register of CLASS1 into memory and loading that memory location
   into a register of CLASS2.  */
/* ??? We may need this for XFmode moves between FR and GR regs.  Using
   getf.sig/getf.exp almost works, but the result in the GR regs is not
   properly formatted and has two extra bits.  */
/* #define SECONDARY_MEMORY_NEEDED(CLASS1, CLASS2, M) */

/* A C expression for the maximum number of consecutive registers of
   class CLASS needed to hold a value of mode MODE.
   This is closely related to the macro `HARD_REGNO_NREGS'.  */

#define CLASS_MAX_NREGS(CLASS, MODE) \
  ((MODE) == CCmode && (CLASS) == PR_REGS ? 2				\
   : (((CLASS) == FR_REGS || (CLASS) == FR_FP_REGS			\
       || (CLASS) == FR_INT_REGS) && (MODE) == XFmode) ? 1		\
   : (GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* A C expression that defines the machine-dependent operand constraint letters
   (`I', `J', `K', .. 'P') that specify particular ranges of integer values.  */

/* 14 bit signed immediate for arithmetic instructions.  */
#define CONST_OK_FOR_I(VALUE) \
  ((unsigned HOST_WIDE_INT)(VALUE) + 0x2000 < 0x4000)
/* 22 bit signed immediate for arith instructions with r0/r1/r2/r3 source.  */
#define CONST_OK_FOR_J(VALUE) \
  ((unsigned HOST_WIDE_INT)(VALUE) + 0x200000 < 0x400000)
/* 8 bit signed immediate for logical instructions.  */
#define CONST_OK_FOR_K(VALUE) ((unsigned HOST_WIDE_INT)(VALUE) + 0x80 < 0x100)
/* 8 bit adjusted signed immediate for compare pseudo-ops.  */
#define CONST_OK_FOR_L(VALUE) ((unsigned HOST_WIDE_INT)(VALUE) + 0x7F < 0x100)
/* 6 bit unsigned immediate for shift counts.  */
#define CONST_OK_FOR_M(VALUE) ((unsigned HOST_WIDE_INT)(VALUE) < 0x40)
/* 9 bit signed immediate for load/store post-increments.  */
/* ??? N is currently not used.  */
#define CONST_OK_FOR_N(VALUE) ((unsigned HOST_WIDE_INT)(VALUE) + 0x100 < 0x200)
/* 0 for r0.  Used by Linux kernel, do not change.  */
#define CONST_OK_FOR_O(VALUE) ((VALUE) == 0)
/* 0 or -1 for dep instruction.  */
#define CONST_OK_FOR_P(VALUE) ((VALUE) == 0 || (VALUE) == -1)

#define CONST_OK_FOR_LETTER_P(VALUE, C) \
((C) == 'I' ? CONST_OK_FOR_I (VALUE)		\
 : (C) == 'J' ? CONST_OK_FOR_J (VALUE)		\
 : (C) == 'K' ? CONST_OK_FOR_K (VALUE)		\
 : (C) == 'L' ? CONST_OK_FOR_L (VALUE)		\
 : (C) == 'M' ? CONST_OK_FOR_M (VALUE)		\
 : (C) == 'N' ? CONST_OK_FOR_N (VALUE)		\
 : (C) == 'O' ? CONST_OK_FOR_O (VALUE)		\
 : (C) == 'P' ? CONST_OK_FOR_P (VALUE)		\
 : 0)

/* A C expression that defines the machine-dependent operand constraint letters
   (`G', `H') that specify particular ranges of `const_double' values.  */

/* 0.0 and 1.0 for fr0 and fr1.  */
#define CONST_DOUBLE_OK_FOR_G(VALUE) \
  ((VALUE) == CONST0_RTX (GET_MODE (VALUE))	\
   || (VALUE) == CONST1_RTX (GET_MODE (VALUE)))

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C) \
  ((C) == 'G' ? CONST_DOUBLE_OK_FOR_G (VALUE) : 0)

/* A C expression that defines the optional machine-dependent constraint
   letters (`Q', `R', `S', `T', `U') that can be used to segregate specific
   types of operands, usually memory references, for the target machine.  */
/* ??? This might be useful considering that we have already used all of the
   integer constant contraint letters.  */
/* #define EXTRA_CONSTRAINT(VALUE, C) */

/* Basic Stack Layout */

/* Define this macro if pushing a word onto the stack moves the stack pointer
   to a smaller address.  */
#define STACK_GROWS_DOWNWARD 1

/* Define this macro if the addresses of local variable slots are at negative
   offsets from the frame pointer.  */
#define FRAME_GROWS_DOWNWARD

/* Offset from the frame pointer to the first local variable slot to be
   allocated.  */
/* ??? This leaves 16 bytes unused normally, but it looks funny to store locals
   into the 16-byte reserved area.  */
/* ??? This isn't very efficient use of the frame pointer.  Better would be
   to move it down a ways, so that we have positive and negative offsets.  */
#define STARTING_FRAME_OFFSET \
  (current_function_pretend_args_size					\
   ? 16 - current_function_pretend_args_size				\
   : 0)

/* Offset from the stack pointer register to the first location at which
   outgoing arguments are placed.  If not specified, the default value of zero
   is used.  This is the proper value for most machines.  */
/* IA64 has a 16 byte scratch area that is at the bottom of the stack.  */
#define STACK_POINTER_OFFSET 16

/* Offset from the argument pointer register to the first argument's address.
   On some machines it may depend on the data type of the function.  */
#define FIRST_PARM_OFFSET(FUNDECL) 0

/* A C expression whose value is RTL representing the value of the return
   address for the frame COUNT steps up from the current frame, after the
   prologue.  */

/* ??? Frames other than zero would likely require interpreting the frame
   unwind info, so we don't try to support them.  We would also need to define
   DYNAMIC_CHAIN_ADDRESS and SETUP_FRAME_ADDRESS (for the reg stack flush).  */

/* ??? This only works for non-leaf functions.  In a leaf function, the return
   address would be in b0 (rp).  */

#define RETURN_ADDR_RTX(COUNT, FRAMEADDR) \
  ((count == 0)								\
   ? gen_rtx_REG (Pmode, RETURN_ADDRESS_REGNUM)				\
   : (rtx) 0)

/* A C expression whose value is RTL representing the location of the incoming
   return address at the beginning of any function, before the prologue.  This
   RTL is either a `REG', indicating that the return value is saved in `REG',
   or a `MEM' representing a location in the stack.  This enables DWARF2
   unwind info for C++ EH.  */
#define INCOMING_RETURN_ADDR_RTX gen_rtx_REG (VOIDmode, BR_REG (0))
/* ??? This is not defined because of three problems.
   1) dwarf2out.c assumes that DWARF_FRAME_RETURN_COLUMN fits in one byte.
   The default value is FIRST_PSEUDO_REGISTER which doesn't.  This can be
   worked around by setting PC_REGNUM to FR_REG (0) which is an otherwise
   unused register number.
   2) dwarf2out_frame_debug core dumps while processing prologue insns.  We
   need to refine which insns have RTX_FRAME_RELATED_P set and which don't.
   3) It isn't possible to turn off EH frame info by defining DWARF2_UNIND_INFO
   to zero, despite what the documentation implies, because it is tested in
   a few places with #ifdef instead of #if.  */
#undef INCOMING_RETURN_ADDR_RTX

/* A C expression whose value is an integer giving the offset, in bytes, from
   the value of the stack pointer register to the top of the stack frame at the
   beginning of any function, before the prologue.  The top of the frame is
   defined to be the value of the stack pointer in the previous frame, just
   before the call instruction.  */
#define INCOMING_FRAME_SP_OFFSET 0


/* Register That Address the Stack Frame.  */

/* The register number of the stack pointer register, which must also be a
   fixed register according to `FIXED_REGISTERS'.  On most machines, the
   hardware determines which register this is.  */

#define STACK_POINTER_REGNUM 12

/* The register number of the frame pointer register, which is used to access
   automatic variables in the stack frame.  On some machines, the hardware
   determines which register this is.  On other machines, you can choose any
   register you wish for this purpose.  */

#define FRAME_POINTER_REGNUM 328

/* Register number where frame pointer was saved in the prologue, or zero
   if it was not saved.  */

extern int ia64_fp_regno;

/* Number of input and local registers used.  This is needed for the .regstk
   directive, and also for debugging info.  */

extern int ia64_input_regs;
extern int ia64_local_regs;

/* The register number of the arg pointer register, which is used to access the
   function's argument list.  */
/* r0 won't otherwise be used, so put the always eliminated argument pointer
   in it.  */
#define ARG_POINTER_REGNUM R_GR(0)

/* The register number for the return address register.  This is modified by
   ia64_expand_prologue to point to the real return address save register.  */

#define RETURN_ADDRESS_REGNUM 329

/* Register numbers used for passing a function's static chain pointer.  */

#define STATIC_CHAIN_REGNUM 15


/* Eliminating the Frame Pointer and the Arg Pointer */

/* A C expression which is nonzero if a function must have and use a frame
   pointer.  This expression is evaluated in the reload pass.  If its value is
   nonzero the function will have a frame pointer.  */

#define FRAME_POINTER_REQUIRED 0

/* If defined, this macro specifies a table of register pairs used to eliminate
   unneeded registers that point into the stack frame.  */

#define ELIMINABLE_REGS							\
{									\
  {ARG_POINTER_REGNUM,	 STACK_POINTER_REGNUM},				\
  {ARG_POINTER_REGNUM,	 FRAME_POINTER_REGNUM},				\
  {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}				\
}

/* A C expression that returns non-zero if the compiler is allowed to try to
   replace register number FROM with register number TO.  There are no ia64
   specific restrictions.  */

#define CAN_ELIMINATE(FROM, TO) 1

/* This macro is similar to `INITIAL_FRAME_POINTER_OFFSET'.  It specifies the
   initial difference between the specified pair of registers.  This macro must
   be defined if `ELIMINABLE_REGS' is defined.  */
/* ??? I need to decide whether the frame pointer is the old frame SP
   or the new frame SP before dynamic allocs.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
{									\
  unsigned int size = ia64_compute_frame_size (get_frame_size ());	\
									\
  if ((FROM) == FRAME_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM)	\
    (OFFSET) = size;							\
  else if ((FROM) == ARG_POINTER_REGNUM)				\
    {									\
      switch (TO)							\
	{								\
	case FRAME_POINTER_REGNUM:					\
	  /* Arguments start above the 16 byte save area, unless stdarg	\
	     in which case we store through the 16 byte save area.  */	\
	  (OFFSET) = 16 - current_function_pretend_args_size;		\
	  break;							\
	case STACK_POINTER_REGNUM:					\
	  (OFFSET) = size + 16 - current_function_pretend_args_size;	\
	  break;							\
	default:							\
	  abort ();							\
	}								\
    }									\
  else									\
    abort ();								\
}


/* Passing Function Arguments on the Stack */

/* Define this macro if an argument declared in a prototype as an integral type
   smaller than `int' should actually be passed as an `int'.  In addition to
   avoiding errors in certain cases of mismatch, it also makes for better code
   on certain machines.  */
/* ??? Investigate.  */
/* #define PROMOTE_PROTOTYPES */

/* If defined, the maximum amount of space required for outgoing arguments will
   be computed and placed into the variable
   `current_function_outgoing_args_size'.  */

#define ACCUMULATE_OUTGOING_ARGS

/* A C expression that should indicate the number of bytes of its own arguments
   that a function pops on returning, or 0 if the function pops no arguments
   and the caller must therefore pop them all after the function returns.  */

#define RETURN_POPS_ARGS(FUNDECL, FUNTYPE, STACK_SIZE) 0


/* Function Arguments in Registers */

#define MAX_ARGUMENT_SLOTS 8
#define MAX_INT_RETURN_SLOTS 4
#define GR_ARG_FIRST IN_REG (0)
#define GR_RET_FIRST GR_REG (8)
#define GR_RET_LAST  GR_REG (11)
#define FR_ARG_FIRST FR_REG (8)
#define FR_RET_FIRST FR_REG (8)
#define FR_RET_LAST  FR_REG (15)
#define AR_ARG_FIRST OUT_REG (0)

/* A C expression that controls whether a function argument is passed in a
   register, and which register.  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
  ia64_function_arg (&CUM, MODE, TYPE, NAMED, 0)

/* Define this macro if the target machine has "register windows", so that the
   register in which a function sees an arguments is not necessarily the same
   as the one in which the caller passed the argument.  */

#define FUNCTION_INCOMING_ARG(CUM, MODE, TYPE, NAMED) \
  ia64_function_arg (&CUM, MODE, TYPE, NAMED, 1)

/* A C expression for the number of words, at the beginning of an argument,
   must be put in registers.  The value must be zero for arguments that are
   passed entirely in registers or that are entirely pushed on the stack.  */

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) \
 ia64_function_arg_partial_nregs (&CUM, MODE, TYPE, NAMED)

/* A C expression that indicates when an argument must be passed by reference.
   If nonzero for an argument, a copy of that argument is made in memory and a
   pointer to the argument is passed instead of the argument itself.  The
   pointer is passed in whatever way is appropriate for passing a pointer to
   that type.  */

#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED) 0

/* A C type for declaring a variable that is used as the first argument of
   `FUNCTION_ARG' and other related values.  For some target machines, the type
   `int' suffices and can hold the number of bytes of argument so far.  */

typedef struct ia64_args
{
  int words;			/* # words of arguments so far  */
  int fp_regs;			/* # FR registers used so far  */
  int prototype;		/* whether function prototyped  */
} CUMULATIVE_ARGS;

/* A C statement (sans semicolon) for initializing the variable CUM for the
   state at the beginning of the argument list.  */

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT) \
do {									\
  (CUM).words = 0;							\
  (CUM).fp_regs = 0;							\
  (CUM).prototype = ((FNTYPE) && TYPE_ARG_TYPES (FNTYPE)) || (LIBNAME);	\
} while (0)

/* Like `INIT_CUMULATIVE_ARGS' but overrides it for the purposes of finding the
   arguments for the function being compiled.  If this macro is undefined,
   `INIT_CUMULATIVE_ARGS' is used instead.  */

/* We set prototype to true so that we never try to return a PARALLEL from
   function_arg.  */
#define INIT_CUMULATIVE_INCOMING_ARGS(CUM, FNTYPE, LIBNAME) \
do {									\
  (CUM).words = 0;							\
  (CUM).fp_regs = 0;							\
  (CUM).prototype = 1;							\
} while (0)

/* A C statement (sans semicolon) to update the summarizer variable CUM to
   advance past an argument in the argument list.  The values MODE, TYPE and
   NAMED describe that argument.  Once this is done, the variable CUM is
   suitable for analyzing the *following* argument with `FUNCTION_ARG'.  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED) \
 ia64_function_arg_advance (&CUM, MODE, TYPE, NAMED)

/* If defined, a C expression that gives the alignment boundary, in bits, of an
   argument with the specified mode and type.  */

/* Arguments larger than 64 bits require 128 bit alignment.  */

#define FUNCTION_ARG_BOUNDARY(MODE, TYPE) \
  (((((MODE) == BLKmode ? int_size_in_bytes (TYPE) : GET_MODE_SIZE (MODE)) \
     + UNITS_PER_WORD - 1) / UNITS_PER_WORD) > 1 ? 128 : PARM_BOUNDARY)

/* A C expression that is nonzero if REGNO is the number of a hard register in
   which function arguments are sometimes passed.  This does *not* include
   implicit arguments such as the static chain and the structure-value address.
   On many machines, no registers can be used for this purpose since all
   function arguments are pushed on the stack.  */
#define FUNCTION_ARG_REGNO_P(REGNO) \
(((REGNO) >= GR_ARG_FIRST && (REGNO) < (GR_ARG_FIRST + MAX_ARGUMENT_SLOTS)) \
 || ((REGNO) >= FR_ARG_FIRST && (REGNO) < (FR_ARG_FIRST + MAX_ARGUMENT_SLOTS)))

/* Implement `va_start' for varargs and stdarg.  */
#define EXPAND_BUILTIN_VA_START(stdarg, valist, nextarg) \
  ia64_va_start (stdarg, valist, nextarg)

/* Implement `va_arg'.  */
#define EXPAND_BUILTIN_VA_ARG(valist, type) \
  ia64_va_arg (valist, type)

/* How Scalar Function Values are Returned */

/* A C expression to create an RTX representing the place where a function
   returns a value of data type VALTYPE.  */

#define FUNCTION_VALUE(VALTYPE, FUNC) \
  ia64_function_value (VALTYPE, FUNC)

/* A C expression to create an RTX representing the place where a library
   function returns a value of mode MODE.  */

#define LIBCALL_VALUE(MODE) \
  gen_rtx_REG (MODE,							\
	       ((GET_MODE_CLASS (MODE) == MODE_FLOAT			\
		 || GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT)	\
		? FR_RET_FIRST : GR_RET_FIRST))

/* A C expression that is nonzero if REGNO is the number of a hard register in
   which the values of called function may come back.  */

#define FUNCTION_VALUE_REGNO_P(REGNO)				\
  (((REGNO) >= GR_RET_FIRST && (REGNO) <= GR_RET_LAST)		\
   || ((REGNO) >= FR_RET_FIRST && (REGNO) <= FR_RET_LAST)) 


/* How Large Values are Returned */

/* A nonzero value says to return the function value in memory, just as large
   structures are always returned.  */

#define RETURN_IN_MEMORY(TYPE) \
  ia64_return_in_memory (TYPE)

/* If you define this macro to be 0, then the conventions used for structure
   and union return values are decided by the `RETURN_IN_MEMORY' macro.  */

#define DEFAULT_PCC_STRUCT_RETURN 0

/* If the structure value address is passed in a register, then
   `STRUCT_VALUE_REGNUM' should be the number of that register.  */

#define STRUCT_VALUE_REGNUM GR_REG (8)


/* Caller-Saves Register Allocation */

/* A C expression to determine whether it is worthwhile to consider placing a
   pseudo-register in a call-clobbered hard register and saving and restoring
   it around each function call.  The expression should be 1 when this is worth
   doing, and 0 otherwise.

   If you don't define this macro, a default is used which is good on most
   machines: `4 * CALLS < REFS'.  */
/* ??? Investigate.  */
/* #define CALLER_SAVE_PROFITABLE(REFS, CALLS) */


/* Function Entry and Exit */

/* A C compound statement that outputs the assembler code for entry to a
   function.  */

#define FUNCTION_PROLOGUE(FILE, SIZE) \
  ia64_function_prologue (FILE, SIZE)

/* Define this macro as a C expression that is nonzero if the return
   instruction or the function epilogue ignores the value of the stack pointer;
   in other words, if it is safe to delete an instruction to adjust the stack
   pointer before a return from the function.  */

#define EXIT_IGNORE_STACK 1

/* Define this macro as a C expression that is nonzero for registers
   used by the epilogue or the `return' pattern.  */

#define EPILOGUE_USES(REGNO) ia64_epilogue_uses (REGNO)

/* A C compound statement that outputs the assembler code for exit from a
   function.  */

#define FUNCTION_EPILOGUE(FILE, SIZE) \
  ia64_function_epilogue (FILE, SIZE)

/* A C compound statement that outputs the assembler code for a thunk function,
   used to implement C++ virtual function calls with multiple inheritance.  */

/* ??? This only supports deltas up to 14 bits.  If we need more, then we
   must load the delta into a register first.  */

#define ASM_OUTPUT_MI_THUNK(FILE, THUNK_FNDECL, DELTA, FUNCTION) \
do {									\
  fprintf (FILE, "\tadd r32 = %d, r32\n", (DELTA));			\
  fprintf (FILE, "\tbr ");						\
  assemble_name (FILE, XSTR (XEXP (DECL_RTL (FUNCTION), 0), 0));	\
  fprintf (FILE, "\n");							\
} while (0)


/* Generating Code for Profiling.  */

/* A C statement or compound statement to output to FILE some assembler code to
   call the profiling subroutine `mcount'.  */

/* ??? Unclear if this will actually work.  No way to test this currently.  */

#define FUNCTION_PROFILER(FILE, LABELNO) \
do {									\
  char buf[20];								\
  ASM_GENERATE_INTERNAL_LABEL (buf, "LP", LABELNO);			\
  fputs ("\taddl r16 = @ltoff(", FILE);					\
  assemble_name (FILE, buf);						\
  fputs ("), gp\n", FILE);						\
  fputs ("\tmov r17 = r1;;\n", FILE);					\
  fputs ("\tld8 out0 = [r16]\n", FILE);					\
  fputs ("\tmov r18 = b0\n", FILE);					\
  fputs ("\tbr.call.sptk.many rp = mcount;;\n", FILE);			\
  fputs ("\tmov b0 = r18\n", FILE);					\
  fputs ("\tmov r1 = r17;;\n", FILE);					\
} while (0)

/* A C statement or compound statement to output to FILE some assembler code to
   initialize basic-block profiling for the current object module.  */

/* ??? Unclear if this will actually work.  No way to test this currently.  */

#define FUNCTION_BLOCK_PROFILER(FILE, LABELNO) \
do {									\
  int labelno = LABELNO;						\
  switch (profile_block_flag)						\
    {									\
    case 2:								\
      fputs ("\taddl r16 = @ltoff(LPBX0), gp\n", FILE);			\
      fprintf (FILE, "\tmov out1 = %d;;\n", labelno);			\
      fputs ("\tld8 out0 = [r16]\n", FILE);				\
      fputs ("\tmov r17 = r1\n", FILE);					\
      fputs ("\tmov r18 = b0\n", FILE);					\
      fputs ("\tbr.call.sptk.many rp = __bb_init_trace_func;;\n", FILE);\
      fputs ("\tmov r1 = r17\n", FILE);					\
      fputs ("\tmov b0 = r18;;\n", FILE);				\
      break;								\
    default:								\
      fputs ("\taddl r16 = @ltoff(LPBX0), gp;;\n", FILE);		\
      fputs ("\tld8 out0 = [r16];;\n", FILE);				\
      fputs ("\tld8 r17 = [out0];;\n", FILE);				\
      fputs ("\tcmp.eq p6, p0 = r0, r17;;\n", FILE);			\
      fputs ("(p6)\tmov r16 = r1\n", FILE);				\
      fputs ("(p6)\tmov r17 = b0\n", FILE);				\
      fputs ("(p6)\tbr.call.sptk.many rp = __bb_init_func;;\n", FILE);	\
      fputs ("(p6)\tmov r1 = r16\n", FILE);				\
      fputs ("(p6)\tmov b0 = r17;;\n", FILE);				\
      break;								\
    }									\
} while (0)

/* A C statement or compound statement to output to FILE some assembler code to
   increment the count associated with the basic block number BLOCKNO.  */

/* ??? This can't work unless we mark some registers as fixed, so that we
   can use them as temporaries in this macro.  We need two registers for -a
   profiling and 4 registers for -ax profiling.  */

#define BLOCK_PROFILER(FILE, BLOCKNO) \
do {									\
  int blockn = BLOCKNO;							\
  switch (profile_block_flag)						\
    {									\
    case 2:								\
      fputs ("\taddl r2 = @ltoff(__bb), gp\n", FILE);			\
      fputs ("\taddl r3 = @ltoff(LPBX0), gp;;\n", FILE);		\
      fprintf (FILE, "\tmov r9 = %d\n", blockn);			\
      fputs ("\tld8 r2 = [r2]\n", FILE);				\
      fputs ("\tld8 r3 = [r3];;\n", FILE);				\
      fputs ("\tadd r8 = 8, r2\n", FILE);				\
      fputs ("\tst8 [r2] = r9;;\n", FILE);				\
      fputs ("\tst8 [r8] = r3\n", FILE);				\
      fputs ("\tbr.call.sptk.many rp = __bb_trace_func\n", FILE);	\
      break;								\
									\
    default:								\
      fputs ("\taddl r2 = @ltoff(LPBX2), gp;;\n", FILE);		\
      fputs ("\tld8 r2 = [r2];;\n", FILE);				\
      fprintf (FILE, "\taddl r2 = %d, r2;;\n", 8 * blockn);		\
      fputs ("\tld8 r3 = [r2];;\n", FILE);				\
      fputs ("\tadd r3 = 1, r3;;\n", FILE);				\
      fputs ("\tst8 [r2] = r3;;\n", FILE);				\
      break;								\
    }									\
} while(0)

/* A C statement or compound statement to output to FILE assembler
   code to call function `__bb_trace_ret'.  */

/* ??? Unclear if this will actually work.  No way to test this currently.  */

/* ??? This needs to be emitted into the epilogue.  Perhaps rewrite to emit
   rtl and call from ia64_expand_epilogue?  */

#define FUNCTION_BLOCK_PROFILER_EXIT(FILE) \
  fputs ("\tbr.call.sptk.many rp = __bb_trace_ret\n", FILE);
#undef FUNCTION_BLOCK_PROFILER_EXIT

/* A C statement or compound statement to save all registers, which may be
   clobbered by a function call, including condition codes.  */

/* ??? We would have to save 20 GRs, 106 FRs, 10 PRs, 2 BRs, and possibly
   other things.  This is not practical.  Perhaps leave this feature (-ax)
   unsupported by undefining above macros?  */

/* #define MACHINE_STATE_SAVE(ID) */

/* A C statement or compound statement to restore all registers, including
   condition codes, saved by `MACHINE_STATE_SAVE'.  */

/* ??? We would have to restore 20 GRs, 106 FRs, 10 PRs, 2 BRs, and possibly
   other things.  This is not practical.  Perhaps leave this feature (-ax)
   unsupported by undefining above macros?  */

/* #define MACHINE_STATE_RESTORE(ID) */


/* Implementing the Varargs Macros.  */

/* Define this macro to store the anonymous register arguments into the stack
   so that all the arguments appear to have been passed consecutively on the
   stack.  */

#define SETUP_INCOMING_VARARGS(ARGS_SO_FAR, MODE, TYPE, PRETEND_ARGS_SIZE, SECOND_TIME) \
    ia64_setup_incoming_varargs (ARGS_SO_FAR, MODE, TYPE, & PRETEND_ARGS_SIZE, SECOND_TIME)

/* Define this macro if the location where a function argument is passed
   depends on whether or not it is a named argument.  */

#define STRICT_ARGUMENT_NAMING  1


/* Trampolines for Nested Functions.  */

/* We need 32 bytes, so we can save the sp, ar.rnat, ar.bsp, and ar.pfs of
   the function containing a non-local goto target.  */

#define STACK_SAVEAREA_MODE(LEVEL) \
  ((LEVEL) == SAVE_NONLOCAL ? OImode : Pmode)

/* Output assembler code for a block containing the constant parts of
   a trampoline, leaving space for the variable parts.

   The trampoline should set the static chain pointer to value placed
   into the trampoline and should branch to the specified routine.  The
   gp doesn't have to be set since that is already done by the caller
   of the trampoline.  To make the normal indirect-subroutine calling
   convention work, the trampoline must look like a function descriptor.
   That is, the first word must be the target address, the second
   word must be the target's global pointer.  The complete trampoline
   has the following form:

		+----------------+ \
	TRAMP:	| TRAMP+32     	 | |
		+----------------+  > fake function descriptor
		|    gp		 | |
		+----------------+ /
		| target addr	 |
		+----------------+
		| static link	 |
		+----------------+
		| mov r2=ip	 |
		+		 +
		| ;;		 |
		+----------------+
		| adds r4=-16,r2 |
		+ adds r15=-8,r2 +
		| ;;		 |
		+----------------+
		| ld8 r4=[r4];;	 |
		+ ld8 r15=[r15]	 +
		| mov b6=r4;;	 |
		+----------------+
		| br b6		 |
		+----------------+
*/

/* ??? Need a version of this and INITIALIZE_TRAMPOLINE for -mno-pic.  */

#define TRAMPOLINE_TEMPLATE(FILE)					\
{									\
  fprintf (FILE,							\
	   "\tdata8 0,0,0,0\n"						\
	   "\t{ mov r2=ip }\n"						\
	   "\t;;\n"							\
	   "\t{ adds r4=-16,r2; adds r%d=-8,r2 }\n"			\
	   "\t;;\n"							\
	   "\t{ ld8 r4=[r4];; ld8 r%d=[r%d]; mov b6=r4 }\n"		\
	   "\t;;\n"							\
	   "\t{ br b6 }\n"						\
	   "\t;;\n",							\
	   STATIC_CHAIN_REGNUM, STATIC_CHAIN_REGNUM,			\
	   STATIC_CHAIN_REGNUM);					\
}

/* The name of a subroutine to switch to the section in which the trampoline
   template is to be placed.

   On ia64, instructions may only be placed in a text segment.  */

#define TRAMPOLINE_SECTION	text_section

/* A C expression for the size in bytes of the trampoline, as an integer.  */

#define TRAMPOLINE_SIZE		96

/* Alignment required for trampolines, in bits.  */

#define TRAMPOLINE_ALIGNMENT	256

/* A C statement to initialize the variable parts of a trampoline.  */

#define INITIALIZE_TRAMPOLINE(ADDR, FNADDR, STATIC_CHAIN) \
{									\
  rtx addr, addr2, addr_reg, fdesc_addr;				\
									\
  /* Load function descriptor address into a pseudo.  */		\
  fdesc_addr = gen_reg_rtx (DImode);					\
  emit_move_insn (fdesc_addr, FNADDR);				     	\
									\
  /* Read target address from function descriptor and store in		\
     trampoline.  */							\
  addr = memory_address (Pmode, plus_constant (ADDR, 16));		\
  emit_move_insn (gen_rtx_MEM (Pmode, addr),				\
		  gen_rtx_MEM (Pmode, fdesc_addr));			\
  /* Store static chain in trampoline.  */				\
  addr = memory_address (Pmode, plus_constant (ADDR, 24));		\
  emit_move_insn (gen_rtx_MEM (Pmode, addr), STATIC_CHAIN);		\
									\
  /* Load GP value from function descriptor and store in trampoline.  */\
  addr = memory_address (Pmode, plus_constant (ADDR, 8));		\
  addr2 = memory_address (Pmode, plus_constant (fdesc_addr, 8));	\
  emit_move_insn (gen_rtx_MEM (Pmode, addr),				\
		  gen_rtx_MEM (Pmode, addr2));				\
									\
  /* Store trampoline entry address in trampoline.  */			\
  addr = memory_address (Pmode, ADDR);					\
  addr2 = memory_address (Pmode, plus_constant (ADDR, 32));		\
  emit_move_insn (gen_rtx_MEM (Pmode, addr), addr2);			\
									\
  /* Flush the relevant 64 bytes from the i-cache.  */			\
  addr_reg = force_reg (DImode, plus_constant (ADDR, 0));		\
  emit_insn (gen_rtx_UNSPEC_VOLATILE (VOIDmode,				\
				      gen_rtvec (1, addr_reg), 3));	\
}


/* Implicit Calls to Library Routines */

/* ??? The ia64 linux kernel requires that we use the standard names for
   divide and modulo routines.  However, if we aren't careful, lib1funcs.asm
   will be overridden by libgcc2.c.  We avoid this by using different names
   for lib1funcs.asm modules, e.g. __divdi3 vs _divdi3.  Since lib1funcs.asm
   goes into libgcc.a first, the linker will find it first.  */

/* Define this macro as a C statement that declares additional library routines
   renames existing ones.  */

/* ??? Disable the SImode divide routines for now.  */
#define INIT_TARGET_OPTABS \
do {									\
  sdiv_optab->handlers[(int) SImode].libfunc = 0;			\
  udiv_optab->handlers[(int) SImode].libfunc = 0;			\
  smod_optab->handlers[(int) SImode].libfunc = 0;			\
  umod_optab->handlers[(int) SImode].libfunc = 0;			\
} while (0)

/* Define this macro if GNU CC should generate calls to the System V (and ANSI
   C) library functions `memcpy' and `memset' rather than the BSD functions
   `bcopy' and `bzero'.  */

#define TARGET_MEM_FUNCTIONS


/* Addressing Modes */

/* Define this macro if the machine supports post-increment addressing.  */

#define HAVE_POST_INCREMENT 1
#define HAVE_POST_DECREMENT 1

/* A C expression that is 1 if the RTX X is a constant which is a valid
   address.  */

#define CONSTANT_ADDRESS_P(X) 0

/* The max number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 1

/* A C compound statement with a conditional `goto LABEL;' executed if X (an
   RTX) is a legitimate memory address on the target machine for a memory
   operand of mode MODE.  */

/* ??? IA64 post increment addressing mode is much more powerful than this.  */

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, LABEL) 			\
do {									\
  if (GET_CODE (X) == REG && REG_OK_FOR_BASE_P (X))			\
    goto LABEL;								\
  else if (GET_CODE (X) == SUBREG && GET_CODE (XEXP (X, 0)) == REG	\
	   && REG_OK_FOR_BASE_P (XEXP (X, 0)))				\
    goto LABEL;								\
  else if (GET_CODE (X) == POST_INC || GET_CODE (X) == POST_DEC)	\
    {									\
      if (GET_CODE (XEXP (X, 0)) == REG					\
	  && REG_OK_FOR_BASE_P (XEXP (X, 0)))				\
	goto LABEL;							\
      else if (GET_CODE (XEXP (X, 0)) == SUBREG				\
	       && GET_CODE (XEXP (XEXP (X, 0), 0)) == REG		\
	       && REG_OK_FOR_BASE_P (XEXP (XEXP (X, 0), 0)))		\
	goto LABEL;							\
    }									\
} while (0)

/* A C expression that is nonzero if X (assumed to be a `reg' RTX) is valid for
   use as a base register.  */

#ifdef REG_OK_STRICT
#define REG_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))
#else
#define REG_OK_FOR_BASE_P(X) \
  (GENERAL_REGNO_P (REGNO (X)) || (REGNO (X) >= FIRST_PSEUDO_REGISTER))
#endif

/* A C expression that is nonzero if X (assumed to be a `reg' RTX) is valid for
   use as an index register.  */

#define REG_OK_FOR_INDEX_P(X) 0

/* A C compound statement that attempts to replace X with a valid memory
   address for an operand of mode MODE.

   This must be present, but there is nothing useful to be done here.  */

#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN)

/* A C statement or compound statement with a conditional `goto LABEL;'
   executed if memory address X (an RTX) can have different meanings depending
   on the machine mode of the memory reference it is used for or if the address
   is valid for some modes but not others.  */

/* ??? Strictly speaking this isn't true, because we can use any increment with
   any mode.  Unfortunately, the RTL implies that the increment depends on the
   mode, so we need this for now.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR, LABEL) \
  if (GET_CODE (ADDR) == POST_DEC || GET_CODE (ADDR) == POST_INC)	\
    goto LABEL;

/* A C expression that is nonzero if X is a legitimate constant for an
   immediate operand on the target machine.  */

#define LEGITIMATE_CONSTANT_P(X) \
  (GET_CODE (X) != CONST_DOUBLE || GET_MODE (X) == VOIDmode	\
   || GET_MODE (X) == DImode || CONST_DOUBLE_OK_FOR_G (X))	\


/* Condition Code Status */

/* One some machines not all possible comparisons are defined, but you can
   convert an invalid comparison into a valid one.  */
/* ??? Investigate.  See the alpha definition.  */
/* #define CANONICALIZE_COMPARISON(CODE, OP0, OP1) */


/* Describing Relative Costs of Operations */

/* A part of a C `switch' statement that describes the relative costs of
   constant RTL expressions.  */

/* ??? This is incomplete.  */

#define CONST_COSTS(X, CODE, OUTER_CODE) \
  case CONST_INT:							\
    if ((X) == const0_rtx)						\
      return 0;								\
  case CONST_DOUBLE:							\
  case CONST:								\
  case SYMBOL_REF:							\
  case LABEL_REF:							\
    return COSTS_N_INSNS (1);

/* Like `CONST_COSTS' but applies to nonconstant RTL expressions.  */

/* ??? Should define this to get better optimized code.  */

/* We make divide expensive, so that divide-by-constant will be optimized to
   a multiply.  */

#define RTX_COSTS(X, CODE, OUTER_CODE) \
  case DIV:								\
  case UDIV:								\
  case MOD:								\
  case UMOD:								\
    return COSTS_N_INSNS (20);

/* An expression giving the cost of an addressing mode that contains ADDRESS.
   If not defined, the cost is computed from the ADDRESS expression and the
   `CONST_COSTS' values.  */

#define ADDRESS_COST(ADDRESS) 0

/* A C expression for the cost of moving data from a register in class FROM to
   one in class TO.  */

#define REGISTER_MOVE_COST(FROM, TO) \
((FROM) == BR_REGS && (TO) == BR_REGS ? 8				\
 : (((FROM) == BR_REGS && (TO) != GENERAL_REGS)				\
    || ((TO) == BR_REGS && (FROM) != GENERAL_REGS)) ? 6			\
 : (((FROM) == FR_FP_REGS && (TO) == FR_INT_REGS)			\
    || ((FROM) == FR_INT_REGS && (TO) == FR_FP_REGS)) ? 4		\
 : 2)

/* A C expression for the cost of moving data of mode M between a register and
   memory.  */
/* ??? Investigate.  Might get better code by defining this.  */
/* #define MEMORY_MOVE_COST(M,C,I) */

/* A C expression for the cost of a branch instruction.  A value of 1 is the
   default; other values are interpreted relative to that.  */
/* ??? Investigate.  Might get better code by defining this.  */
/* #define BRANCH_COST */

/* Define this macro as a C expression which is nonzero if accessing less than
   a word of memory (i.e. a `char' or a `short') is no faster than accessing a
   word of memory.  */

#define SLOW_BYTE_ACCESS 1

/* Define this macro if it is as good or better to call a constant function
   address than to call an address kept in a register.

   Indirect function calls are more expensive that direct function calls, so
   don't cse function addresses.  */

#define NO_FUNCTION_CSE

/* A C statement (sans semicolon) to update the integer variable COST based on
   the relationship between INSN that is dependent on DEP_INSN through the
   dependence LINK.  */

/* ??? Investigate.  */
/* #define ADJUST_COST(INSN, LINK, DEP_INSN, COST) */

/* A C statement (sans semicolon) to update the integer scheduling
   priority `INSN_PRIORITY(INSN)'.  */

/* ??? Investigate.  */
/* #define ADJUST_PRIORITY (INSN) */


/* Dividing the output into sections.  */

/* A C expression whose value is a string containing the assembler operation
   that should precede instructions and read-only data.  */

#define TEXT_SECTION_ASM_OP ".text"

/* A C expression whose value is a string containing the assembler operation to
   identify the following data as writable initialized data.  */

#define DATA_SECTION_ASM_OP ".data"

/* If defined, a C expression whose value is a string containing the assembler
   operation to identify the following data as uninitialized global data.  */

#define BSS_SECTION_ASM_OP ".bss"

/* Define this macro if jump tables (for `tablejump' insns) should be output in
   the text section, along with the assembler instructions.  */

/* ??? It is probably better for the jump tables to be in the rodata section,
   which is where they go by default.  Unfortunately, that currently does not
   work, because of some problem with pcrelative relocations not getting
   resolved correctly.  */
/* ??? FIXME ??? rth says that we should use @gprel to solve this problem.  */
/* ??? If jump tables are in the text section, then we can use 4 byte
   entries instead of 8 byte entries.  */

#define JUMP_TABLES_IN_TEXT_SECTION 1

/* Define this macro if references to a symbol must be treated differently
   depending on something about the variable or function named by the symbol
   (such as what section it is in).  */

#define ENCODE_SECTION_INFO(DECL) ia64_encode_section_info (DECL)

#define SDATA_NAME_FLAG_CHAR '@'

#define IA64_DEFAULT_GVALUE 8

/* Decode SYM_NAME and store the real name part in VAR, sans the characters
   that encode section info.  */

#define STRIP_NAME_ENCODING(VAR, SYMBOL_NAME) \
  (VAR) = (SYMBOL_NAME) + ((SYMBOL_NAME)[0] == SDATA_NAME_FLAG_CHAR)


/* Position Independent Code.  */

/* The register number of the register used to address a table of static data
   addresses in memory.  */

/* ??? Should modify ia64.md to use pic_offset_table_rtx instead of
   gen_rtx_REG (DImode, 1).  */

/* ??? Should we set flag_pic?  Probably need to define
   LEGITIMIZE_PIC_OPERAND_P to make that work.  */

#define PIC_OFFSET_TABLE_REGNUM GR_REG (1)

/* Define this macro if the register defined by `PIC_OFFSET_TABLE_REGNUM' is
   clobbered by calls.  */

#define PIC_OFFSET_TABLE_REG_CALL_CLOBBERED


/* The Overall Framework of an Assembler File.  */

/* A C string constant describing how to begin a comment in the target
   assembler language.  The compiler assumes that the comment will end at the
   end of the line.  */

#define ASM_COMMENT_START "//"

/* A C string constant for text to be output before each `asm' statement or
   group of consecutive ones.  */

/* ??? This won't work with the Intel assembler, because it does not accept
   # as a comment start character.  However, //APP does not work in gas, so we
   can't use that either.  Same problem for ASM_APP_OFF below.  */

#define ASM_APP_ON "#APP\n"

/* A C string constant for text to be output after each `asm' statement or
   group of consecutive ones.  */

#define ASM_APP_OFF "#NO_APP\n"


/* Output of Data.  */

/* A C statement to output to the stdio stream STREAM an assembler instruction
   to assemble a floating-point constant of `XFmode', `DFmode', `SFmode',
   respectively, whose value is VALUE.  */

/* ??? This has not been tested.  Long doubles are really 10 bytes not 12
   bytes on ia64.  */

/* ??? Must reverse the word order for big-endian code?  */

#define ASM_OUTPUT_LONG_DOUBLE(FILE, VALUE) \
do {									\
  long t[3];								\
  REAL_VALUE_TO_TARGET_LONG_DOUBLE (VALUE, t);				\
  fprintf (FILE, "\tdata8 0x%08lx, 0x%08lx, 0x%08lx\n",			\
	   t[0] & 0xffffffff, t[1] & 0xffffffff, t[2] & 0xffffffff);	\
} while (0)

/* ??? Must reverse the word order for big-endian code?  */

#define ASM_OUTPUT_DOUBLE(FILE,VALUE)				\
do {								\
  long t[2];							\
  REAL_VALUE_TO_TARGET_DOUBLE (VALUE, t);			\
  fprintf (FILE, "\tdata8 0x%08lx%08lx\n",			\
	   t[1] & 0xffffffff, t[0] & 0xffffffff);		\
} while (0)

#define ASM_OUTPUT_FLOAT(FILE,VALUE)				\
  do {								\
    long t;							\
    REAL_VALUE_TO_TARGET_SINGLE (VALUE, t);			\
    fprintf (FILE, "\tdata4 0x%lx\n", t & 0xffffffff);		\
} while (0)
  
/* A C statement to output to the stdio stream STREAM an assembler instruction
   to assemble an integer of 1, 2, 4, or 8 bytes, respectively, whose value
   is VALUE.  */

/* This is how to output an assembler line defining a `char' constant.  */

#define ASM_OUTPUT_CHAR(FILE, VALUE)					\
do {									\
  fprintf (FILE, "\t%s\t", ASM_BYTE_OP);				\
  output_addr_const (FILE, (VALUE));					\
  fprintf (FILE, "\n");							\
} while (0)

/* This is how to output an assembler line defining a `short' constant.  */

#define ASM_OUTPUT_SHORT(FILE, VALUE)					\
do {									\
  fprintf (FILE, "\tdata2\t");						\
  output_addr_const (FILE, (VALUE));					\
  fprintf (FILE, "\n");							\
} while (0)

/* This is how to output an assembler line defining an `int' constant.
   We also handle symbol output here.  */

/* ??? For ILP32, also need to handle function addresses here.  */

#define ASM_OUTPUT_INT(FILE, VALUE)					\
do {									\
  fprintf (FILE, "\tdata4\t");						\
  output_addr_const (FILE, (VALUE));					\
  fprintf (FILE, "\n");							\
} while (0)

/* This is how to output an assembler line defining a `long' constant.
   We also handle symbol output here.  */

#define ASM_OUTPUT_DOUBLE_INT(FILE, VALUE)				\
do {									\
  fprintf (FILE, "\tdata8\t");						\
  if (SYMBOL_REF_FLAG (VALUE))						\
    fprintf (FILE, "@fptr(");						\
  output_addr_const (FILE, (VALUE));					\
  if (SYMBOL_REF_FLAG (VALUE))						\
    fprintf (FILE, ")");						\
  fprintf (FILE, "\n");							\
} while (0)

/* A C statement to output to the stdio stream STREAM an assembler instruction
   to assemble a single byte containing the number VALUE.  */

#define ASM_OUTPUT_BYTE(STREAM, VALUE) \
  fprintf (STREAM, "\t%s\t0x%x\n", ASM_BYTE_OP, (VALUE))

/* These macros are defined as C string constant, describing the syntax in the
   assembler for grouping arithmetic expressions.  */

#define ASM_OPEN_PAREN "("
#define ASM_CLOSE_PAREN ")"


/* Output of Uninitialized Variables.  */

/* This is all handled by svr4.h.  */


/* Output and Generation of Labels.  */

/* A C statement (sans semicolon) to output to the stdio stream STREAM the
   assembler definition of a label named NAME.  */

/* See the ASM_OUTPUT_LABELREF definition in sysv4.h for an explanation of
   why ia64_asm_output_label exists.  */

extern int ia64_asm_output_label;
#define ASM_OUTPUT_LABEL(STREAM, NAME)					\
do {									\
  ia64_asm_output_label = 1;						\
  assemble_name (STREAM, NAME);						\
  fputs (":\n", STREAM);						\
  ia64_asm_output_label = 0;						\
} while (0)

/* A C statement (sans semicolon) to output to the stdio stream STREAM some
   commands that will make the label NAME global; that is, available for
   reference from other files.  */

#define ASM_GLOBALIZE_LABEL(STREAM,NAME)				\
do {									\
  fputs ("\t.global ", STREAM);						\
  assemble_name (STREAM, NAME);						\
  fputs ("\n", STREAM);							\
} while (0)

/* A C statement (sans semicolon) to output to the stdio stream STREAM any text
   necessary for declaring the name of an external symbol named NAME which is
   referenced in this compilation but not defined.  */

#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME) \
  ia64_asm_output_external (FILE, DECL, NAME)

/* A C statement to store into the string STRING a label whose name is made
   from the string PREFIX and the number NUM.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM) \
do {									\
  sprintf (LABEL, "*.%s%d", PREFIX, NUM);				\
} while (0)

/* A C expression to assign to OUTVAR (which is a variable of type `char *') a
   newly allocated string made from the string NAME and the number NUMBER, with
   some suitable punctuation added.  */

/* ??? Not sure if using a ? in the name for Intel as is safe.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTVAR, NAME, NUMBER)			\
do {									\
  (OUTVAR) = (char *) alloca (strlen (NAME) + 12);			\
  sprintf (OUTVAR, "%s%c%ld", (NAME), (TARGET_GNU_AS ? '.' : '?'),	\
	   (long)(NUMBER));						\
} while (0)

/* A C statement to output to the stdio stream STREAM assembler code which
   defines (equates) the symbol NAME to have the value VALUE.  */

#define ASM_OUTPUT_DEF(STREAM, NAME, VALUE) \
do {									\
  assemble_name (STREAM, NAME);						\
  fputs (" = ", STREAM);						\
  assemble_name (STREAM, VALUE);					\
  fputc ('\n', STREAM);							\
} while (0)


/* Macros Controlling Initialization Routines.  */

/* This is handled by svr4.h and sysv4.h.  */


/* Output of Assembler Instructions.  */

/* A C initializer containing the assembler's names for the machine registers,
   each one as a C string constant.  */

#define REGISTER_NAMES \
{									\
  /* General registers.  */						\
  "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9",		\
  "r10", "r11", "r12", "r13", "r14", "r15", "r16", "r17", "r18", "r19",	\
  "r20", "r21", "r22", "r23", "r24", "r25", "r26", "r27", "r28", "r29",	\
  "r30", "r31",								\
  /* Local registers.  */						\
  "loc0", "loc1", "loc2", "loc3", "loc4", "loc5", "loc6", "loc7",	\
  "loc8", "loc9", "loc10","loc11","loc12","loc13","loc14","loc15",	\
  "loc16","loc17","loc18","loc19","loc20","loc21","loc22","loc23",	\
  "loc24","loc25","loc26","loc27","loc28","loc29","loc30","loc31",	\
  "loc32","loc33","loc34","loc35","loc36","loc37","loc38","loc39",	\
  "loc40","loc41","loc42","loc43","loc44","loc45","loc46","loc47",	\
  "loc48","loc49","loc50","loc51","loc52","loc53","loc54","loc55",	\
  "loc56","loc57","loc58","loc59","loc60","loc61","loc62","loc63",	\
  "loc64","loc65","loc66","loc67","loc68","loc69","loc70","loc71",	\
  "loc72","loc73","loc74","loc75","loc76","loc77","loc78","loc79",	\
  /* Input registers.  */						\
  "in0",  "in1",  "in2",  "in3",  "in4",  "in5",  "in6",  "in7",	\
  /* Output registers.  */						\
  "out0", "out1", "out2", "out3", "out4", "out5", "out6", "out7",	\
  /* Floating-point registers.  */					\
  "f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9",		\
  "f10", "f11", "f12", "f13", "f14", "f15", "f16", "f17", "f18", "f19",	\
  "f20", "f21", "f22", "f23", "f24", "f25", "f26", "f27", "f28", "f29",	\
  "f30", "f31", "f32", "f33", "f34", "f35", "f36", "f37", "f38", "f39",	\
  "f40", "f41", "f42", "f43", "f44", "f45", "f46", "f47", "f48", "f49",	\
  "f50", "f51", "f52", "f53", "f54", "f55", "f56", "f57", "f58", "f59",	\
  "f60", "f61", "f62", "f63", "f64", "f65", "f66", "f67", "f68", "f69",	\
  "f70", "f71", "f72", "f73", "f74", "f75", "f76", "f77", "f78", "f79",	\
  "f80", "f81", "f82", "f83", "f84", "f85", "f86", "f87", "f88", "f89",	\
  "f90", "f91", "f92", "f93", "f94", "f95", "f96", "f97", "f98", "f99",	\
  "f100","f101","f102","f103","f104","f105","f106","f107","f108","f109",\
  "f110","f111","f112","f113","f114","f115","f116","f117","f118","f119",\
  "f120","f121","f122","f123","f124","f125","f126","f127",		\
  /* Predicate registers.  */						\
  "p0", "p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9",		\
  "p10", "p11", "p12", "p13", "p14", "p15", "p16", "p17", "p18", "p19",	\
  "p20", "p21", "p22", "p23", "p24", "p25", "p26", "p27", "p28", "p29",	\
  "p30", "p31", "p32", "p33", "p34", "p35", "p36", "p37", "p38", "p39",	\
  "p40", "p41", "p42", "p43", "p44", "p45", "p46", "p47", "p48", "p49",	\
  "p50", "p51", "p52", "p53", "p54", "p55", "p56", "p57", "p58", "p59",	\
  "p60", "p61", "p62", "p63",						\
  /* Branch registers.  */						\
  "b0", "b1", "b2", "b3", "b4", "b5", "b6", "b7",			\
  /* Frame pointer.  Return address.  */				\
  "fp", "ra"								\
}

/* If defined, a C initializer for an array of structures containing a name and
   a register number.  This macro defines additional names for hard registers,
   thus allowing the `asm' option in declarations to refer to registers using
   alternate names.  */

#define ADDITIONAL_REGISTER_NAMES \
{									\
  { "gp", R_GR (1) },							\
  { "sp", R_GR (12) },							\
  { "in0", IN_REG (0) },						\
  { "in1", IN_REG (1) },						\
  { "in2", IN_REG (2) },						\
  { "in3", IN_REG (3) },						\
  { "in4", IN_REG (4) },						\
  { "in5", IN_REG (5) },						\
  { "in6", IN_REG (6) },						\
  { "in7", IN_REG (7) },						\
  { "out0", OUT_REG (0) },						\
  { "out1", OUT_REG (1) },						\
  { "out2", OUT_REG (2) },						\
  { "out3", OUT_REG (3) },						\
  { "out4", OUT_REG (4) },						\
  { "out5", OUT_REG (5) },						\
  { "out6", OUT_REG (6) },						\
  { "out7", OUT_REG (7) },						\
  { "loc0", LOC_REG (0) },						\
  { "loc1", LOC_REG (1) },						\
  { "loc2", LOC_REG (2) },						\
  { "loc3", LOC_REG (3) },						\
  { "loc4", LOC_REG (4) },						\
  { "loc5", LOC_REG (5) },						\
  { "loc6", LOC_REG (6) },						\
  { "loc7", LOC_REG (7) },						\
  { "loc8", LOC_REG (8) }, 						\
  { "loc9", LOC_REG (9) }, 						\
  { "loc10", LOC_REG (10) }, 						\
  { "loc11", LOC_REG (11) }, 						\
  { "loc12", LOC_REG (12) }, 						\
  { "loc13", LOC_REG (13) }, 						\
  { "loc14", LOC_REG (14) }, 						\
  { "loc15", LOC_REG (15) }, 						\
  { "loc16", LOC_REG (16) }, 						\
  { "loc17", LOC_REG (17) }, 						\
  { "loc18", LOC_REG (18) }, 						\
  { "loc19", LOC_REG (19) }, 						\
  { "loc20", LOC_REG (20) }, 						\
  { "loc21", LOC_REG (21) }, 						\
  { "loc22", LOC_REG (22) }, 						\
  { "loc23", LOC_REG (23) }, 						\
  { "loc24", LOC_REG (24) }, 						\
  { "loc25", LOC_REG (25) }, 						\
  { "loc26", LOC_REG (26) }, 						\
  { "loc27", LOC_REG (27) }, 						\
  { "loc28", LOC_REG (28) }, 						\
  { "loc29", LOC_REG (29) }, 						\
  { "loc30", LOC_REG (30) }, 						\
  { "loc31", LOC_REG (31) }, 						\
  { "loc32", LOC_REG (32) }, 						\
  { "loc33", LOC_REG (33) }, 						\
  { "loc34", LOC_REG (34) }, 						\
  { "loc35", LOC_REG (35) }, 						\
  { "loc36", LOC_REG (36) }, 						\
  { "loc37", LOC_REG (37) }, 						\
  { "loc38", LOC_REG (38) }, 						\
  { "loc39", LOC_REG (39) }, 						\
  { "loc40", LOC_REG (40) }, 						\
  { "loc41", LOC_REG (41) }, 						\
  { "loc42", LOC_REG (42) }, 						\
  { "loc43", LOC_REG (43) }, 						\
  { "loc44", LOC_REG (44) }, 						\
  { "loc45", LOC_REG (45) }, 						\
  { "loc46", LOC_REG (46) }, 						\
  { "loc47", LOC_REG (47) }, 						\
  { "loc48", LOC_REG (48) }, 						\
  { "loc49", LOC_REG (49) }, 						\
  { "loc50", LOC_REG (50) }, 						\
  { "loc51", LOC_REG (51) }, 						\
  { "loc52", LOC_REG (52) }, 						\
  { "loc53", LOC_REG (53) }, 						\
  { "loc54", LOC_REG (54) }, 						\
  { "loc55", LOC_REG (55) }, 						\
  { "loc56", LOC_REG (56) }, 						\
  { "loc57", LOC_REG (57) }, 						\
  { "loc58", LOC_REG (58) }, 						\
  { "loc59", LOC_REG (59) }, 						\
  { "loc60", LOC_REG (60) }, 						\
  { "loc61", LOC_REG (61) }, 						\
  { "loc62", LOC_REG (62) }, 						\
  { "loc63", LOC_REG (63) }, 						\
  { "loc64", LOC_REG (64) }, 						\
  { "loc65", LOC_REG (65) }, 						\
  { "loc66", LOC_REG (66) }, 						\
  { "loc67", LOC_REG (67) }, 						\
  { "loc68", LOC_REG (68) }, 						\
  { "loc69", LOC_REG (69) }, 						\
  { "loc70", LOC_REG (70) }, 						\
  { "loc71", LOC_REG (71) }, 						\
  { "loc72", LOC_REG (72) }, 						\
  { "loc73", LOC_REG (73) }, 						\
  { "loc74", LOC_REG (74) }, 						\
  { "loc75", LOC_REG (75) }, 						\
  { "loc76", LOC_REG (76) }, 						\
  { "loc77", LOC_REG (77) }, 						\
  { "loc78", LOC_REG (78) }, 						\
  { "loc79", LOC_REG (79) }, 						\
}

/* A C compound statement to output to stdio stream STREAM the assembler syntax
   for an instruction operand X.  X is an RTL expression.  */

#define PRINT_OPERAND(STREAM, X, CODE) \
  ia64_print_operand (STREAM, X, CODE)

/* A C expression which evaluates to true if CODE is a valid punctuation
   character for use in the `PRINT_OPERAND' macro.  */

/* ??? Keep this around for now, as we might need it later.  */

/* #define PRINT_OPERAND_PUNCT_VALID_P(CODE) */

/* A C compound statement to output to stdio stream STREAM the assembler syntax
   for an instruction operand that is a memory reference whose address is X.  X
   is an RTL expression.  */

#define PRINT_OPERAND_ADDRESS(STREAM, X) \
  ia64_print_operand_address (STREAM, X)

/* If defined, C string expressions to be used for the `%R', `%L', `%U', and
   `%I' options of `asm_fprintf' (see `final.c').  */

#define REGISTER_PREFIX ""
#define LOCAL_LABEL_PREFIX "."
#define USER_LABEL_PREFIX ""
#define IMMEDIATE_PREFIX ""


/* Output of dispatch tables.  */

/* This macro should be provided on machines where the addresses in a dispatch
   table are relative to the table's own address.  */

/* ??? Depends on the pointer size.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL) \
  fprintf (STREAM, "\tdata8 .L%d-.L%d\n", VALUE, REL)

/* This is how to output an element of a case-vector that is absolute.
   (Ia64 does not use such vectors, but we must define this macro anyway.)  */

#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE) abort ()

/* Define this if something special must be output at the end of a jump-table.
   We need to align back to a 16 byte boundary because offsets are smaller than
   instructions.  */

#define ASM_OUTPUT_CASE_END(STREAM, NUM, TABLE) ASM_OUTPUT_ALIGN (STREAM, 4)

/* Jump tables only need 8 byte alignment.  */

#define ADDR_VEC_ALIGN(ADDR_VEC) 3


/* Assembler Commands for Exception Regions.  */

/* ??? This entire section of ia64.h needs to be implemented and then cleaned
   up.  */

/* A C expression to output text to mark the start of an exception region.

   This macro need not be defined on most platforms.  */
/* #define ASM_OUTPUT_EH_REGION_BEG() */

/* A C expression to output text to mark the end of an exception region.

   This macro need not be defined on most platforms.  */
/* #define ASM_OUTPUT_EH_REGION_END() */

/* A C expression to switch to the section in which the main exception table is
   to be placed.  The default is a section named `.gcc_except_table' on machines
   that support named sections via `ASM_OUTPUT_SECTION_NAME', otherwise if `-fpic'
   or `-fPIC' is in effect, the `data_section', otherwise the
   `readonly_data_section'.  */
/* #define EXCEPTION_SECTION() */

/* If defined, a C string constant for the assembler operation to switch to the
   section for exception handling frame unwind information.  If not defined,
   GNU CC will provide a default definition if the target supports named
   sections.  `crtstuff.c' uses this macro to switch to the appropriate
   section.

   You should define this symbol if your target supports DWARF 2 frame unwind
   information and the default definition does not work.  */
/* #define EH_FRAME_SECTION_ASM_OP */

/* A C expression that is nonzero if the normal exception table output should
   be omitted.

   This macro need not be defined on most platforms.  */
/* #define OMIT_EH_TABLE() */

/* Alternate runtime support for looking up an exception at runtime and finding
   the associated handler, if the default method won't work.

   This macro need not be defined on most platforms.  */
/* #define EH_TABLE_LOOKUP() */

/* A C expression that decides whether or not the current function needs to
   have a function unwinder generated for it.  See the file `except.c' for
   details on when to define this, and how.  */
/* #define DOESNT_NEED_UNWINDER */

/* An rtx used to mask the return address found via RETURN_ADDR_RTX, so that it
   does not contain any extraneous set bits in it.  */
/* #define MASK_RETURN_ADDR */

/* Define this macro to 0 if your target supports DWARF 2 frame unwind
   information, but it does not yet work with exception handling.  Otherwise,
   if your target supports this information (if it defines
   `INCOMING_RETURN_ADDR_RTX' and either `UNALIGNED_INT_ASM_OP' or
   `OBJECT_FORMAT_ELF'), GCC will provide a default definition of 1.

   If this macro is defined to 1, the DWARF 2 unwinder will be the default
   exception handling mechanism; otherwise, setjmp/longjmp will be used by
   default.

   If this macro is defined to anything, the DWARF 2 unwinder will be used
   instead of inline unwinders and __unwind_function in the non-setjmp case.  */
/* #define DWARF2_UNWIND_INFO */


/* Assembler Commands for Alignment.  */

/* The alignment (log base 2) to put in front of LABEL, which follows
   a BARRIER.  */

/* ??? Investigate.  */

/* ??? Emitting align directives increases the size of the line number debug
   info, because each .align forces use of an extended opcode.  Perhaps try
   to fix this in the assembler?  */

/* #define LABEL_ALIGN_AFTER_BARRIER(LABEL) */

/* The desired alignment for the location counter at the beginning
   of a loop.  */

/* ??? Investigate.  */
/* #define LOOP_ALIGN(LABEL) */

/* Define this macro if `ASM_OUTPUT_SKIP' should not be used in the text
   section because it fails put zeros in the bytes that are skipped.  */

#define ASM_NO_SKIP_IN_TEXT 1

/* A C statement to output to the stdio stream STREAM an assembler command to
   advance the location counter to a multiple of 2 to the POWER bytes.  */

#define ASM_OUTPUT_ALIGN(STREAM, POWER) \
  fprintf (STREAM, "\t.align %d\n", 1<<(POWER))


/* Macros Affecting all Debug Formats.  */

/* This is handled in svr4.h and sysv4.h.  */


/* Specific Options for DBX Output.  */

/* This is handled by dbxelf.h which is included by svr4.h.  */


/* Open ended Hooks for DBX Output.  */

/* Likewise.  */


/* File names in DBX format.  */

/* Likewise.  */


/* Macros for SDB and Dwarf Output.  */

/* Define this macro if GNU CC should produce dwarf version 2 format debugging
   output in response to the `-g' option.  */

#define DWARF2_DEBUGGING_INFO

/* Section names for DWARF2 debug info.  */

#define DEBUG_INFO_SECTION	".debug_info, \"\", \"progbits\""
#define ABBREV_SECTION		".debug_abbrev, \"\", \"progbits\""
#define ARANGES_SECTION		".debug_aranges, \"\", \"progbits\""
#define DEBUG_LINE_SECTION	".debug_line, \"\", \"progbits\""
#define PUBNAMES_SECTION	".debug_pubnames, \"\", \"progbits\""

/* C string constants giving the pseudo-op to use for a sequence of
   2, 4, and 8 byte unaligned constants.  dwarf2out.c needs these.  */

#define UNALIGNED_SHORT_ASM_OP		"data2.ua"
#define UNALIGNED_INT_ASM_OP		"data4.ua"
#define UNALIGNED_DOUBLE_INT_ASM_OP	"data8.ua"

/* We need to override the default definition for this in dwarf2out.c so that
   we can emit the necessary # postfix.  */
#define ASM_NAME_TO_STRING(STR, NAME)			\
  do {							\
      if ((NAME)[0] == '*')				\
	dyn_string_append (STR, NAME + 1);		\
      else						\
	{						\
	  char *newstr;					\
	  STRIP_NAME_ENCODING (newstr, NAME);		\
	  dyn_string_append (STR, user_label_prefix);	\
	  dyn_string_append (STR, newstr);		\
	  dyn_string_append (STR, "#");			\
	}						\
  }							\
  while (0)

#define DWARF2_ASM_LINE_DEBUG_INFO (TARGET_DWARF2_ASM)


/* Cross Compilation and Floating Point.  */

/* Define to enable software floating point emulation. */
#define REAL_ARITHMETIC


/* Miscellaneous Parameters.  */

/* Define this if you have defined special-purpose predicates in the file
   `MACHINE.c'.  For each predicate, list all rtl codes that can be in
   expressions matched by the predicate.  */

#define PREDICATE_CODES \
{ "call_operand", {SUBREG, REG, SYMBOL_REF}},				\
{ "sdata_symbolic_operand", {SYMBOL_REF, CONST}},			\
{ "symbolic_operand", {SYMBOL_REF, CONST, LABEL_REF}},			\
{ "function_operand", {SYMBOL_REF}},					\
{ "setjmp_operand", {SYMBOL_REF}},					\
{ "move_operand", {SUBREG, REG, MEM, CONST_INT, CONST_DOUBLE,		\
		     CONSTANT_P_RTX, SYMBOL_REF, CONST, LABEL_REF}},	\
{ "reg_or_0_operand", {SUBREG, REG, CONST_INT}},			\
{ "reg_or_6bit_operand", {SUBREG, REG, CONST_INT, CONSTANT_P_RTX}},	\
{ "reg_or_8bit_operand", {SUBREG, REG, CONST_INT, CONSTANT_P_RTX}},	\
{ "reg_or_8bit_adjusted_operand", {SUBREG, REG, CONST_INT,		\
				     CONSTANT_P_RTX}},			\
{ "reg_or_8bit_and_adjusted_operand", {SUBREG, REG, CONST_INT,		\
					 CONSTANT_P_RTX}},		\
{ "reg_or_14bit_operand", {SUBREG, REG, CONST_INT, CONSTANT_P_RTX}},	\
{ "reg_or_22bit_operand", {SUBREG, REG, CONST_INT, CONSTANT_P_RTX}},	\
{ "shift_count_operand", {SUBREG, REG, CONST_INT, CONSTANT_P_RTX}},	\
{ "shift_32bit_count_operand", {SUBREG, REG, CONST_INT,			\
				  CONSTANT_P_RTX}},			\
{ "shladd_operand", {CONST_INT}},					\
{ "fetchadd_operand", {CONST_INT}},					\
{ "reg_or_fp01_operand", {SUBREG, REG, CONST_DOUBLE, CONSTANT_P_RTX}},	\
{ "normal_comparison_operator", {EQ, NE, GT, LE, GTU, LEU}},		\
{ "adjusted_comparison_operator", {LT, GE, LTU, GEU}},			\
{ "call_multiple_values_operation", {PARALLEL}},

/* An alias for a machine mode name.  This is the machine mode that elements of
   a jump-table should have.  */

#define CASE_VECTOR_MODE Pmode

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.  */

#define CASE_VECTOR_PC_RELATIVE 1

/* Define this macro if operations between registers with integral mode smaller
   than a word are always performed on the entire register.  */

#define WORD_REGISTER_OPERATIONS

/* Define this macro to be a C expression indicating when insns that read
   memory in MODE, an integral mode narrower than a word, set the bits outside
   of MODE to be either the sign-extension or the zero-extension of the data
   read.  */

#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* An alias for a tree code that should be used by default for conversion of
   floating point values to fixed point.  */

/* ??? Looks like this macro is obsolete and should be deleted everywhere.  */

#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* An alias for a tree code that is the easiest kind of division to compile
   code for in the general case.  */

#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* The maximum number of bytes that a single instruction can move quickly from
   memory to memory.  */
#define MOVE_MAX 8

/* A C expression which is nonzero if on this machine it is safe to "convert"
   an integer of INPREC bits to one of OUTPREC bits (where OUTPREC is smaller
   than INPREC) by merely operating on it as if it had only OUTPREC bits.  */

#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* A C expression describing the value returned by a comparison operator with
   an integral mode and stored by a store-flag instruction (`sCOND') when the
   condition is true.  */

/* ??? Investigate using -1 instead of 1.  */

#define STORE_FLAG_VALUE 1

/* An alias for the machine mode for pointers.  */

/* ??? This would change if we had ILP32 support.  */

#define Pmode DImode

/* An alias for the machine mode used for memory references to functions being
   called, in `call' RTL expressions.  */

#define FUNCTION_MODE Pmode

/* Define this macro to handle System V style pragmas: #pragma pack and
   #pragma weak.  Note, #pragma weak will only be supported if SUPPORT_WEAK is
   defined.  */

#define HANDLE_SYSV_PRAGMA

/* If defined, a C expression whose value is nonzero if IDENTIFIER with
   arguments ARGS is a valid machine specific attribute for TYPE.  The
   attributes in ATTRIBUTES have previously been assigned to TYPE.  */

#define VALID_MACHINE_TYPE_ATTRIBUTE(TYPE, ATTRIBUTES, IDENTIFIER, ARGS) \
  ia64_valid_type_attribute (TYPE, ATTRIBUTES, IDENTIFIER, ARGS)

/* In rare cases, correct code generation requires extra machine dependent
   processing between the second jump optimization pass and delayed branch
   scheduling.  On those machines, define this macro as a C statement to act on
   the code starting at INSN.  */

#define MACHINE_DEPENDENT_REORG(INSN) ia64_reorg (INSN)

/* A C expression for the maximum number of instructions to execute via
   conditional execution instructions instead of a branch.  A value of
   BRANCH_COST+1 is the default if the machine does not use
   cc0, and 1 if it does use cc0.  */
/* ??? Investigate.  */
/* #define MAX_CONDITIONAL_EXECUTE */

/* Indicate how many instructions can be issued at the same time.  */

/* ??? For now, we just schedule to fill bundles.  */

#define ISSUE_RATE 3

enum ia64_builtins
{
  IA64_BUILTIN_SYNCHRONIZE,

  IA64_BUILTIN_FETCH_AND_ADD_SI,
  IA64_BUILTIN_FETCH_AND_SUB_SI,
  IA64_BUILTIN_FETCH_AND_OR_SI,
  IA64_BUILTIN_FETCH_AND_AND_SI,
  IA64_BUILTIN_FETCH_AND_XOR_SI,
  IA64_BUILTIN_FETCH_AND_NAND_SI,

  IA64_BUILTIN_ADD_AND_FETCH_SI,
  IA64_BUILTIN_SUB_AND_FETCH_SI,
  IA64_BUILTIN_OR_AND_FETCH_SI,
  IA64_BUILTIN_AND_AND_FETCH_SI,
  IA64_BUILTIN_XOR_AND_FETCH_SI,
  IA64_BUILTIN_NAND_AND_FETCH_SI,

  IA64_BUILTIN_BOOL_COMPARE_AND_SWAP_SI,
  IA64_BUILTIN_VAL_COMPARE_AND_SWAP_SI,

  IA64_BUILTIN_SYNCHRONIZE_SI,

  IA64_BUILTIN_LOCK_TEST_AND_SET_SI,

  IA64_BUILTIN_LOCK_RELEASE_SI,

  IA64_BUILTIN_FETCH_AND_ADD_DI,
  IA64_BUILTIN_FETCH_AND_SUB_DI,
  IA64_BUILTIN_FETCH_AND_OR_DI,
  IA64_BUILTIN_FETCH_AND_AND_DI,
  IA64_BUILTIN_FETCH_AND_XOR_DI,
  IA64_BUILTIN_FETCH_AND_NAND_DI,

  IA64_BUILTIN_ADD_AND_FETCH_DI,
  IA64_BUILTIN_SUB_AND_FETCH_DI,
  IA64_BUILTIN_OR_AND_FETCH_DI,
  IA64_BUILTIN_AND_AND_FETCH_DI,
  IA64_BUILTIN_XOR_AND_FETCH_DI,
  IA64_BUILTIN_NAND_AND_FETCH_DI,

  IA64_BUILTIN_BOOL_COMPARE_AND_SWAP_DI,
  IA64_BUILTIN_VAL_COMPARE_AND_SWAP_DI,

  IA64_BUILTIN_SYNCHRONIZE_DI,

  IA64_BUILTIN_LOCK_TEST_AND_SET_DI,

  IA64_BUILTIN_LOCK_RELEASE_DI
};

/* Codes for expand_compare_and_swap and expand_swap_and_compare. */
enum fetchop_code {
  IA64_ADD_OP, IA64_SUB_OP, IA64_OR_OP, IA64_AND_OP, IA64_XOR_OP, IA64_NAND_OP
};

#define MD_INIT_BUILTINS do { \
    ia64_init_builtins (); \
  } while (0)

#define MD_EXPAND_BUILTIN(EXP, TARGET, SUBTARGET, MODE, IGNORE) \
    ia64_expand_builtin ((EXP), (TARGET), (SUBTARGET), (MODE), (IGNORE))

/* End of ia64.h */
