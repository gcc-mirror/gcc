/* Definitions of target machine for GNU compiler, for Sun SPARC.
   Copyright (C) 1987, 88, 89, 92, 94, 1995 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com).
   64 bit SPARC V9 support by Michael Tiemann, Jim Wilson, and Doug Evans,
   at Cygnus Support.

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

/* Note that some other tm.h files include this one and then override
   many of the definitions that relate to assembler syntax.  */

/* Sparc64 support has been added by trying to allow for a day when one
   compiler can handle both v8 and v9.  There are a few cases where this
   isn't doable, but keep them to a minimum!  Two macros are used to help out:
   TARGET_V9 is used to select (at runtime) !v9-ness or v9-ness.
   SPARCV9 is defined when compiling for sparc64 only.
   In places where it is possible to choose between the two at runtime, use
   TARGET_V9.  In places where it is currently not possible to select
   between the two at runtime use SPARCV9.  Again, keep uses of SPARCV9 to a
   minimum.  No attempt is made to support both v8 and v9 in the v9 compiler.

   If a combination v8/v9 compiler is too slow, it should always be possible
   to #define TARGET_V9 as 0 (and potentially other v9-only options), and
   #undef SPARCV9.  */

/* What architecture we're compiling for.  This must coincide with the
   `arch_type' attribute in the .md file.  The names were chosen to avoid
   potential misunderstandings with the various 32 bit flavors (v7, v8, etc.):
   if we used ARCH_V9 then we'd want to use something like ARCH_V8 but that
   could be misleading and ARCH_NOTV9 sounds klunky.  */
enum arch_type { ARCH_32BIT, ARCH_64BIT };
extern enum arch_type sparc_arch_type;

/* Names to predefine in the preprocessor for this target machine.  */

/* ??? The GCC_NEW_VARARGS macro is now obsolete, because gcc always uses
   the right varags.h file when bootstrapping.  */

#ifdef SPARCV9
#define CPP_PREDEFINES \
  "-Dsparc -Dsun -Dunix -D__sparc_v9__ \
   -Asystem(unix) -Asystem(bsd) -Acpu(sparc64) -Amachine(sparc64)"
#else
#define CPP_PREDEFINES \
  "-Dsparc -Dsun -Dunix -D__GCC_NEW_VARARGS__ \
   -Asystem(unix) -Asystem(bsd) -Acpu(sparc) -Amachine(sparc)"
#endif

#define LIB_SPEC "%{!shared:%{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p} %{g:-lg}}"

/* Provide required defaults for linker -e and -d switches.  */

#define LINK_SPEC \
 "%{!shared:%{!nostdlib:%{!r*:%{!e*:-e start}}} -dc -dp} %{static:-Bstatic} \
  %{assert*} %{shared:-assert pure-text}"

/* Special flags to the Sun-4 assembler when using pipe for input.  */

#define ASM_SPEC \
  " %| %{R} %{!pg:%{!p:%{fpic:-k} %{fPIC:-k}}} %{keep-local-as-symbols:-L}"

/* Define macros to distinguish architectures.  */

#ifdef SPARCV9
#define CPP_SPEC "\
%{mint64:-D__INT_MAX__=9223372036854775807LL -D__LONG_MAX__=9223372036854775807LL} \
%{mlong64:-D__LONG_MAX__=9223372036854775807LL} \
"
#else
#define CPP_SPEC "\
%{msparclite:-D__sparclite__} \
%{mf930:-D__sparclite__} %{mf934:-D__sparclite__} \
%{mv8:-D__sparc_v8__} \
%{msupersparc:-D__supersparc__ -D__sparc_v8__}	\
"
#endif

/* Prevent error on `-sun4' and `-target sun4' options.  */
/* This used to translate -dalign to -malign, but that is no good
   because it can't turn off the usual meaning of making debugging dumps.  */

#define CC1_SPEC "%{sun4:} %{target:}"

#ifdef SPARCV9
#define PTRDIFF_TYPE "long long int"
#define SIZE_TYPE "long long unsigned int"
#else
#define PTRDIFF_TYPE "int"
/* In 2.4 it should work to delete this.
   #define SIZE_TYPE "int"  */
#endif

/* ??? This should be 32 bits for v9 but what can we do?  */
#define WCHAR_TYPE "short unsigned int"
#define WCHAR_TYPE_SIZE 16
#define MAX_WCHAR_TYPE_SIZE 16

/* Show we can debug even without a frame pointer.  */
#define CAN_DEBUG_WITHOUT_FP

/* To make profiling work with -f{pic,PIC}, we need to emit the profiling
   code into the rtl.  Also, if we are profiling, we cannot eliminate
   the frame pointer (because the return address will get smashed).  */

void sparc_override_options ();

#define OVERRIDE_OPTIONS \
  do {									\
    if (profile_flag || profile_block_flag)				\
      {									\
	if (flag_pic)							\
	  {								\
	    char *pic_string = (flag_pic == 1) ? "-fpic" : "-fPIC";	\
	    warning ("%s and profiling conflict: disabling %s",		\
		     pic_string, pic_string);				\
	    flag_pic = 0;						\
	  }								\
	flag_omit_frame_pointer = 0;					\
      }									\
    SUBTARGET_OVERRIDE_OPTIONS;						\
    sparc_override_options ();						\
  } while (0)

/* This is meant to be redefined in the host dependent files.  */
#define SUBTARGET_OVERRIDE_OPTIONS

/* These compiler options take an argument.  We ignore -target for now.  */

#define WORD_SWITCH_TAKES_ARG(STR)				\
 (DEFAULT_WORD_SWITCH_TAKES_ARG (STR)				\
  || !strcmp (STR, "target") || !strcmp (STR, "assert"))

/* Print subsidiary information on the compiler version in use.  */

#define TARGET_VERSION fprintf (stderr, " (sparc)");

/* Generate DBX debugging information.  */

#define DBX_DEBUGGING_INFO

/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;

/* Nonzero if we should generate code to use the fpu.  */
#define MASK_FPU 1
#define TARGET_FPU (target_flags & MASK_FPU)

/* Nonzero if we should use FUNCTION_EPILOGUE.  Otherwise, we
   use fast return insns, but lose some generality.  */
#define MASK_EPILOGUE 2
#define TARGET_EPILOGUE (target_flags & MASK_EPILOGUE)

/* Nonzero if we should assume that double pointers might be unaligned.
   This can happen when linking gcc compiled code with other compilers,
   because the ABI only guarantees 4 byte alignment.  */
#define MASK_UNALIGNED_DOUBLES 4
#define TARGET_UNALIGNED_DOUBLES (target_flags & MASK_UNALIGNED_DOUBLES)

/* ??? Bits 0x18 are currently unused.  */

/* Nonzero means we should schedule code for the TMS390Z55 SuperSparc chip.  */
#define MASK_SUPERSPARC 0x20
#define TARGET_SUPERSPARC (target_flags & MASK_SUPERSPARC)

/* Nonzero means that we should generate code for a v8 sparc.  */
#define MASK_V8 0x40
#define TARGET_V8 (target_flags & MASK_V8)

/* Nonzero means that we should generate code for a sparclite.
   This enables the sparclite specific instructions, but does not affect
   whether FPU instructions are emitted.  */
#define MASK_SPARCLITE 0x80
#define TARGET_SPARCLITE (target_flags & MASK_SPARCLITE)

/* Nonzero means that we should generate code using a flat register window
   model, i.e. no save/restore instructions are generated, in the most
   efficient manner.  This code is not compatible with normal sparc code.  */
/* This is not a user selectable option yet, because it requires changes
   that are not yet switchable via command line arguments.  */
/* ??? This flag is deprecated and may disappear at some point.  */
#define MASK_FRW 0x100
#define TARGET_FRW (target_flags & MASK_FRW)

/* Nonzero means that we should generate code using a flat register window
   model, i.e. no save/restore instructions are generated, but which is
   compatible with normal sparc code.   This is the same as above, except
   that the frame pointer is %i7 instead of %fp.  */
/* ??? This use to be named TARGET_FRW_COMPAT.  At some point TARGET_FRW will
   go away, but until that time only use this one when necessary.
   -mflat sets both.  */
#define MASK_FLAT 0x200
#define TARGET_FLAT (target_flags & MASK_FLAT)

/* Nonzero means use the registers that the Sparc ABI reserves for
   application software.  This is the default for v8, but not v9.  */
#define MASK_APP_REGS 0x400
#define TARGET_APP_REGS (target_flags & MASK_APP_REGS)

/*  Option to select how quad word floating point is implemented.
    When TARGET_HARD_QUAD is true, we use the hardware quad instructions.
    Otherwise, we use the SPARC ABI quad library functions.  */
#define MASK_HARD_QUAD 0x800
#define TARGET_HARD_QUAD (target_flags & MASK_HARD_QUAD)

/* Nonzero if we're compiling for 64 bit sparc.  */
#define MASK_V9 0x1000
#define TARGET_V9 (target_flags & MASK_V9)

/* Nonzero if ints are 64 bits.
   This automatically implies longs are 64 bits too.
   This option is for v9 only.  */
#define MASK_INT64 0x2000
#define TARGET_INT64 (target_flags & MASK_INT64)

/* Nonzero if longs are 64 bits.
   This option is for v9 only.  */
#define MASK_LONG64 0x4000
#define TARGET_LONG64 (target_flags & MASK_LONG64)

/* Nonzero if pointers are 64 bits.
   This is not a user selectable option, though it may be one day -
   so it is used to determine pointer size instead of an architecture flag.  */
#define MASK_PTR64 0x8000
#define TARGET_PTR64 (target_flags & MASK_PTR64)

/* Nonzero if we are generating code to be tested in a 32 bit environment.
   Hence, we assume the upper 32 bits of symbolic addresses are zero, and
   avoid generating %uhi and %ulo terms.
   Pointers are still 64 bits though!  This option is for v9 only.  */
/* ??? This option is deprecated.  Try to use -mmedlow.  */
#define MASK_ENV32 0x10000
#define TARGET_ENV32 (target_flags & MASK_ENV32)

/* Memory models.
   Two memory models are supported:
   TARGET_MEDLOW: 32 bit address space, top 32 bits = 0
                  (pointers still 64 bits)
   TARGET_MEDANY: 32 bit address space, data segment loaded anywhere
                  (use %g4 as offset).
   TARGET_FULLANY: not supported yet.
   These options are for v9 only.  All mask values are nonzero so the v8
   compiler can assume this stuff won't interfere.  */
#define MASK_MEDLOW 0x20000
#define MASK_MEDANY 0x40000
#define MASK_FULLANY 0x60000
#define MASK_CODE_MODEL (MASK_MEDLOW + MASK_MEDANY)
#define TARGET_MEDLOW ((target_flags & MASK_CODE_MODEL) == MASK_MEDLOW)
#define TARGET_MEDANY ((target_flags & MASK_CODE_MODEL) == MASK_MEDANY)
#define TARGET_FULLANY ((target_flags & MASK_CODE_MODEL) == MASK_FULLANY)

/* ??? There are hardcoded references to this reg in the .md file.  */
#define MEDANY_BASE_REG "%g4"

/* Non-zero means use a stack bias of 2047.  Stack offsets are obtained by
   adding 2047 to %sp.  This option is for v9 only and is the default.  */
#define MASK_STACK_BIAS 0x80000
#define TARGET_STACK_BIAS (target_flags & MASK_STACK_BIAS)

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

/* The Fujitsu MB86930 is the original sparclite chip, with no fpu.
   The Fujitsu MB86934 is the recent sparclite chip, with an fpu.
   We use -mf930 and -mf934 options to choose which.
   ??? These should perhaps be -mcpu= options.  */

#define TARGET_SWITCHES  \
  { {"fpu", MASK_FPU},			\
    {"no-fpu", -MASK_FPU},		\
    {"hard-float", MASK_FPU},		\
    {"soft-float", -MASK_FPU},		\
    {"epilogue", MASK_EPILOGUE},	\
    {"no-epilogue", -MASK_EPILOGUE},	\
    {"unaligned-doubles", MASK_UNALIGNED_DOUBLES}, \
    {"no-unaligned-doubles", -MASK_UNALIGNED_DOUBLES}, \
    {"supersparc", MASK_SUPERSPARC+MASK_V8},	\
    {"cypress", -MASK_SUPERSPARC-MASK_V8},	\
    {"v8", MASK_V8},			\
    {"no-v8", -MASK_V8},		\
    {"sparclite", MASK_SPARCLITE},	\
    {"no-sparclite", -MASK_SPARCLITE},	\
    {"f930", MASK_SPARCLITE},		\
    {"f930", -MASK_FPU},		\
    {"f934", MASK_SPARCLITE},		\
    {"flat", MASK_FRW+MASK_FLAT},	\
    {"no-flat", -(MASK_FRW+MASK_FLAT)},	\
    {"app-regs", MASK_APP_REGS},	\
    {"no-app-regs", -MASK_APP_REGS},	\
    {"hard-quad-float", MASK_HARD_QUAD}, \
    {"soft-quad-float", -MASK_HARD_QUAD}, \
    SUBTARGET_SWITCHES			\
    V9_SWITCHES				\
    { "", TARGET_DEFAULT}}

#define TARGET_DEFAULT (MASK_APP_REGS + MASK_EPILOGUE + MASK_FPU)

/* This is meant to be redefined in the host dependent files */
#define SUBTARGET_SWITCHES

/* ??? Until we support a combination v8/v9 compiler, the v9 specific options
   are only defined for the v9 compiler.  */
#ifdef SPARCV9
#define V9_SWITCHES \
/*  {"v9", MASK_V9}, */			\
    {"int64", MASK_INT64+MASK_LONG64},	\
    {"int32", -MASK_INT64},		\
    {"int32", MASK_LONG64},		\
    {"long64", -MASK_INT64},		\
    {"long64", MASK_LONG64},		\
    {"long32", -(MASK_INT64+MASK_LONG64)}, \
/*  {"ptr64", MASK_PTR64}, */		\
/*  {"ptr32", -MASK_PTR64}, */		\
    {"stack-bias", MASK_STACK_BIAS},	\
    {"no-stack-bias", -MASK_STACK_BIAS}, \
    {"medlow", -MASK_CODE_MODEL},	\
    {"medlow", MASK_MEDLOW},		\
    {"medany", -MASK_CODE_MODEL},	\
    {"medany", MASK_MEDANY},
#else
#define V9_SWITCHES
#endif

/* target machine storage layout */

/* Define for cross-compilation to a sparc target with no TFmode from a host
   with a different float format (e.g. VAX).  */
#define REAL_ARITHMETIC

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */
#define BITS_BIG_ENDIAN 1

/* Define this if most significant byte of a word is the lowest numbered.  */
/* This is true on the SPARC.  */
#define BYTES_BIG_ENDIAN 1

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
/* Doubles are stored in memory with the high order word first.  This
   matters when cross-compiling.  */
#define WORDS_BIG_ENDIAN 1

/* number of bits in an addressable storage unit */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD		(TARGET_V9 ? 64 : 32)
#define MAX_BITS_PER_WORD	64

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD		(TARGET_V9 ? 8 : 4)
#define MIN_UNITS_PER_WORD	4

/* Now define the sizes of the C data types.  */

#define SHORT_TYPE_SIZE		16
#define INT_TYPE_SIZE		(TARGET_INT64 ? 64 : 32)
#define LONG_TYPE_SIZE		(TARGET_LONG64 ? 64 : 32)
#define LONG_LONG_TYPE_SIZE	64
#define FLOAT_TYPE_SIZE		32
#define DOUBLE_TYPE_SIZE	64

#define MAX_INT_TYPE_SIZE	64
#define MAX_LONG_TYPE_SIZE	64

#ifdef SPARCV9
/* ??? This does not work in SunOS 4.x, so it is not enabled here.
   Instead, it is enabled in sol2.h, because it does work under Solaris.  */
/* Define for support of TFmode long double and REAL_ARITHMETIC.
   Sparc ABI says that long double is 4 words.  */
#define LONG_DOUBLE_TYPE_SIZE 128
#endif

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE (TARGET_PTR64 ? 64 : 32)

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY (TARGET_V9 ? 64 : 32)

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY (TARGET_V9 ? 128 : 64)

/* ALIGN FRAMES on double word boundaries */

#define SPARC_STACK_ALIGN(LOC) \
  (TARGET_V9 ? (((LOC)+15) & ~15) : (((LOC)+7) & ~7))

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 32

/* Alignment of field after `int : 0' in a structure.  */
/* ??? Should this be based on TARGET_INT64?  */
#define EMPTY_FIELD_BOUNDARY (TARGET_V9 ? 64 : 32)

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* A bitfield declared as `int' forces `int' alignment for the struct.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT (TARGET_V9 ? 128 : 64)

/* The best alignment to use in cases where we have a choice.  */
#define FASTEST_ALIGNMENT 64

/* Make strings word-aligned so strcpy from constants will be faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  ((TREE_CODE (EXP) == STRING_CST	\
    && (ALIGN) < FASTEST_ALIGNMENT)	\
   ? FASTEST_ALIGNMENT : (ALIGN))

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN))

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* Things that must be doubleword aligned cannot go in the text section,
   because the linker fails to align the text section enough!
   Put them in the data section.  This macro is only used in this file.  */
#define MAX_TEXT_ALIGN 32

/* This forces all variables and constants to the data section when PIC.
   This is because the SunOS 4 shared library scheme thinks everything in
   text is a function, and patches the address to point to a loader stub.  */
/* This is defined to zero for every system which doesn't use the a.out object
   file format.  */
#ifndef SUNOS4_SHARED_LIBRARIES
#define SUNOS4_SHARED_LIBRARIES 0
#endif

/* This is defined differently for v9 in a cover file.  */
#define SELECT_SECTION(T,RELOC)						\
{									\
  if (TREE_CODE (T) == VAR_DECL)					\
    {									\
      if (TREE_READONLY (T) && ! TREE_SIDE_EFFECTS (T)			\
	  && DECL_INITIAL (T)						\
	  && (DECL_INITIAL (T) == error_mark_node			\
	      || TREE_CONSTANT (DECL_INITIAL (T)))			\
	  && DECL_ALIGN (T) <= MAX_TEXT_ALIGN				\
	  && ! (flag_pic && ((RELOC) || SUNOS4_SHARED_LIBRARIES)))	\
	text_section ();						\
      else								\
	data_section ();						\
    }									\
  else if (TREE_CODE (T) == CONSTRUCTOR)				\
    {									\
      if (flag_pic && ((RELOC) || SUNOS4_SHARED_LIBRARIES))		\
	data_section ();						\
    }									\
  else if (TREE_CODE_CLASS (TREE_CODE (T)) == 'c')			\
    {									\
      if ((TREE_CODE (T) == STRING_CST && flag_writable_strings)	\
	  || TYPE_ALIGN (TREE_TYPE (T)) > MAX_TEXT_ALIGN		\
	  || (flag_pic && ((RELOC) || SUNOS4_SHARED_LIBRARIES)))	\
	data_section ();						\
      else								\
	text_section ();						\
    }									\
}

/* Use text section for a constant
   unless we need more alignment than that offers.  */
/* This is defined differently for v9 in a cover file.  */
#define SELECT_RTX_SECTION(MODE, X)		\
{						\
  if (GET_MODE_BITSIZE (MODE) <= MAX_TEXT_ALIGN \
      && ! (flag_pic && (symbolic_operand (X) || SUNOS4_SHARED_LIBRARIES)))  \
    text_section ();				\
  else						\
    data_section ();				\
}

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   SPARC has 32 integer registers and 32 floating point registers.
   64 bit SPARC has 32 additional fp regs, but the odd numbered ones are not
   accessible.  We still account for them to simplify register computations
   (eg: in CLASS_MAX_NREGS).  There are also 4 fp condition code registers, so
   32+32+32+4 == 100.
   Register 0 is used as the integer condition code register.  */

#ifdef SPARCV9
#define FIRST_PSEUDO_REGISTER 100
#else
#define FIRST_PSEUDO_REGISTER 64
#endif

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.
   g0 is used for the condition code and not to represent %g0, which is
   hardwired to 0, so reg 0 is *not* fixed.
   On non-v9 systems:
   g1 is free to use as temporary.
   g2-g4 are reserved for applications.  Gcc normally uses them as
   temporaries, but this can be disabled via the -mno-app-regs option.
   g5 through g7 are reserved for the operating system.
   On v9 systems:
   g1 and g5 are free to use as temporaries.
   g2-g4 are reserved for applications (the compiler will not normally use
   them, but they can be used as temporaries with -mapp-regs).
   g6-g7 are reserved for the operating system.
   ??? Register 1 is used as a temporary by the 64 bit sethi pattern, so must
   currently be a fixed register until this pattern is rewritten.
   Register 1 is also used when restoring call-preserved registers in large
   stack frames.  */

#ifdef SPARCV9
#define FIXED_REGISTERS  \
 {0, 1, 1, 1, 1, 0, 1, 1,	\
  0, 0, 0, 0, 0, 0, 1, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 1, 1,	\
				\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
				\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
				\
  0, 0, 0, 0}
#else
#define FIXED_REGISTERS  \
 {0, 0, 0, 0, 0, 1, 1, 1,	\
  0, 0, 0, 0, 0, 0, 1, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 1, 1,	\
				\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0}
#endif

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */

#ifdef SPARCV9
#define CALL_USED_REGISTERS  \
 {1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 1, 1,	\
				\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
				\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
				\
  1, 1, 1, 1}
#else
#define CALL_USED_REGISTERS  \
 {1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 1, 1,	\
				\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1}
#endif

/* If !TARGET_FPU, then make the fp registers fixed so that they won't
   be allocated.  On v9, also make the fp cc regs fixed.  */

#define CONDITIONAL_REGISTER_USAGE				\
do								\
  {								\
    if (! TARGET_FPU)						\
      {								\
	int regno;						\
	for (regno = 32; regno < FIRST_PSEUDO_REGISTER; regno++) \
	  fixed_regs[regno] = 1;				\
      }								\
    if (! TARGET_APP_REGS)					\
      {								\
	fixed_regs[2] = 1;					\
	fixed_regs[3] = 1;					\
	fixed_regs[4] = 1;					\
      }								\
    else							\
      {								\
	fixed_regs[2] = 0;					\
	fixed_regs[3] = 0;					\
	fixed_regs[4] = TARGET_MEDANY != 0;			\
      }								\
    if (TARGET_FLAT)						\
      {								\
	/* Let the compiler believe the frame pointer is still	\
	   %fp, but output it as %i7.  */			\
	fixed_regs[31] = 1;					\
	reg_names[FRAME_POINTER_REGNUM] = "%i7";		\
	/* ??? This is a hack to disable leaf functions.  */	\
	global_regs[7] = 1;					\
      }								\
    if (profile_block_flag)					\
      {								\
	/* %g1 and %g2 must be fixed, because BLOCK_PROFILER	\
	    uses them.  */					\
	fixed_regs[1] = 1;					\
	fixed_regs[2] = 1;					\
      }								\
  }								\
while (0)

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   On SPARC, ordinary registers hold 32 bits worth;
   this means both integer and floating point registers.
   On v9, integer regs hold 64 bits worth; floating point regs hold
   32 bits worth (this includes the new fp regs as even the odd ones are
   included in the hard register count).  */

#define HARD_REGNO_NREGS(REGNO, MODE) \
  (TARGET_V9								\
   ?  ((REGNO) < 32							\
       ? (GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD	\
       : (GET_MODE_SIZE (MODE) + 3) / 4)				\
   : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   See sparc.c for how we initialize this.  */
extern int *hard_regno_mode_classes;
extern int sparc_mode_class[];
#define HARD_REGNO_MODE_OK(REGNO, MODE) \
  ((hard_regno_mode_classes[REGNO] & sparc_mode_class[MODE]) != 0)

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.

   For V9: SFmode can't be combined with other float modes, because they can't
   be allocated to the %d registers.  Also, DFmode won't fit in odd %f
   registers, but SFmode will.  */
#define MODES_TIEABLE_P(MODE1, MODE2) \
  ((MODE1) == (MODE2)						\
   || (GET_MODE_CLASS (MODE1) == GET_MODE_CLASS (MODE2)		\
       && (! TARGET_V9						\
	   || (GET_MODE_CLASS (MODE1) != MODE_FLOAT		\
	       || (MODE1 != SFmode && MODE2 != SFmode)))))

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* SPARC pc isn't overloaded on a register that the compiler knows about.  */
/* #define PC_REGNUM  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 14

/* Actual top-of-stack address is 92/136 greater than the contents of the
   stack pointer register for !v9/v9.  That is:
   - !v9: 64 bytes for the in and local registers, 4 bytes for structure return
     address, and 24 bytes for the 6 register parameters.
   - v9: 128 bytes for the in and local registers + 8 bytes reserved.  */
#define STACK_POINTER_OFFSET FIRST_PARM_OFFSET(0)

/* The stack bias (amount by which the hardware register is offset by).  */
#define SPARC_STACK_BIAS (TARGET_STACK_BIAS ? 2047 : 0)

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 30

#if 0
/* Register that is used for the return address.  */
#define RETURN_ADDR_REGNUM 15
#endif

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.
   Used in flow.c, global.c, and reload1.c.

   Being a non-leaf function does not mean a frame pointer is needed in the
   flat window model.  However, the debugger won't be able to backtrace through
   us with out it.  */
#define FRAME_POINTER_REQUIRED \
  (TARGET_FRW ? (current_function_calls_alloca || current_function_varargs \
		 || !leaf_function_p ()) \
   : ! (leaf_function_p () && only_leaf_regs_used ()))

/* C statement to store the difference between the frame pointer
   and the stack pointer values immediately after the function prologue.

   Note, we always pretend that this is a leaf function because if
   it's not, there's no point in trying to eliminate the
   frame pointer.  If it is a leaf function, we guessed right!  */
#define INITIAL_FRAME_POINTER_OFFSET(VAR) \
  ((VAR) = (TARGET_FRW ? sparc_flat_compute_frame_size (get_frame_size ()) \
	    : compute_frame_size (get_frame_size (), 1)))

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM FRAME_POINTER_REGNUM

/* Register in which static-chain is passed to a function.  This must
   not be a register used by the prologue.  */
#define STATIC_CHAIN_REGNUM (TARGET_V9 ? 5 : 2)

/* Register which holds offset table for position-independent
   data references.  */

#define PIC_OFFSET_TABLE_REGNUM 23

#define INITIALIZE_PIC initialize_pic ()
#define FINALIZE_PIC finalize_pic ()

/* Sparc ABI says that quad-precision floats and all structures are returned
   in memory.
   For v9, all aggregates are returned in memory.  */
#define RETURN_IN_MEMORY(TYPE)				\
  (TYPE_MODE (TYPE) == BLKmode				\
   || (! TARGET_V9 && (TYPE_MODE (TYPE) == TFmode	\
		       || TYPE_MODE (TYPE) == TCmode)))

/* Functions which return large structures get the address
   to place the wanted value at offset 64 from the frame.
   Must reserve 64 bytes for the in and local registers.
   v9: Functions which return large structures get the address to place the
   wanted value from an invisible first argument.  */
/* Used only in other #defines in this file.  */
#define STRUCT_VALUE_OFFSET 64

#define STRUCT_VALUE \
  (TARGET_V9						\
   ? 0							\
   : gen_rtx (MEM, Pmode,				\
	      gen_rtx (PLUS, Pmode, stack_pointer_rtx,	\
		       gen_rtx (CONST_INT, VOIDmode, STRUCT_VALUE_OFFSET))))
#define STRUCT_VALUE_INCOMING \
  (TARGET_V9						\
   ? 0							\
   : gen_rtx (MEM, Pmode,				\
	      gen_rtx (PLUS, Pmode, frame_pointer_rtx,	\
		       gen_rtx (CONST_INT, VOIDmode, STRUCT_VALUE_OFFSET))))

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

/* The SPARC has two kinds of registers, general and floating point.

   For v9 we must distinguish between the upper and lower floating point
   registers because the upper ones can't hold SFmode values.
   HARD_REGNO_MODE_OK won't help here because reload assumes that register(s)
   satisfying a group need for a class will also satisfy a single need for
   that class.  EXTRA_FP_REGS is a bit of a misnomer as it covers all 64 fp
   regs.

   It is important that one class contains all the general and all the standard
   fp regs.  Otherwise find_reg() won't properly allocate int regs for moves,
   because reg_class_record() will bias the selection in favor of fp regs,
   because reg_class_subunion[GENERAL_REGS][FP_REGS] will yield FP_REGS,
   because FP_REGS > GENERAL_REGS.

   It is also important that one class contain all the general and all the
   fp regs.  Otherwise when spilling a DFmode reg, it may be from EXTRA_FP_REGS
   but find_reloads() may use class GENERAL_OR_FP_REGS. This will cause
   allocate_reload_reg() to bypass it causing an abort because the compiler
   thinks it doesn't have a spill reg when in fact it does.

   v9 also has 4 floating point condition code registers.  Since we don't
   have a class that is the union of FPCC_REGS with either of the others,
   it is important that it appear first.  Otherwise the compiler will die
   trying to compile _fixunsdfsi because fix_truncdfsi2 won't match its
   constraints.  */

#ifdef SPARCV9
enum reg_class { NO_REGS, FPCC_REGS, GENERAL_REGS, FP_REGS, EXTRA_FP_REGS,
		 GENERAL_OR_FP_REGS, GENERAL_OR_EXTRA_FP_REGS,
		 ALL_REGS, LIM_REG_CLASSES };
#else
enum reg_class { NO_REGS, GENERAL_REGS, FP_REGS, ALL_REGS, LIM_REG_CLASSES };
#endif

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.   */

#ifdef SPARCV9
#define REG_CLASS_NAMES \
  { "NO_REGS", "FPCC_REGS", "GENERAL_REGS", "FP_REGS", "EXTRA_FP_REGS", \
    "GENERAL_OR_FP_REGS", "GENERAL_OR_EXTRA_FP_REGS", "ALL_REGS" }
#else
#define REG_CLASS_NAMES \
  { "NO_REGS", "GENERAL_REGS", "FP_REGS", "ALL_REGS" }
#endif

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#ifdef SPARCV9
#define REG_CLASS_CONTENTS \
  {{0, 0, 0, 0}, {0, 0, 0, 0xf}, {-2, 0, 0, 0}, \
   {0, -1, 0, 0}, {0, -1, -1, 0}, {-2, -1, 0, 0}, {-2, -1, -1, 0}, \
   {-2, -1, -1, 0xf}}
#else
#if 0 && defined (__GNUC__)
#define REG_CLASS_CONTENTS {0LL, 0xfffffffeLL, 0xffffffff00000000LL, 0xfffffffffffffffeLL}
#else
#define REG_CLASS_CONTENTS {{0, 0}, {-2, 0}, {0, -1}, {-2, -1}}
#endif
#endif

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#ifdef SPARCV9
#define REGNO_REG_CLASS(REGNO) \
  ((REGNO) == 0 ? NO_REGS		\
   : (REGNO) < 32 ? GENERAL_REGS	\
   : (REGNO) < 64 ? FP_REGS		\
   : (REGNO) < 96 ? EXTRA_FP_REGS	\
   : FPCC_REGS)
#else
#define REGNO_REG_CLASS(REGNO) \
  ((REGNO) >= 32 ? FP_REGS : (REGNO) == 0 ? NO_REGS : GENERAL_REGS)
#endif

/* This is the order in which to allocate registers normally.  
   
   We put %f0/%f1 last among the float registers, so as to make it more
   likely that a pseudo-register which dies in the float return register
   will get allocated to the float return register, thus saving a move
   instruction at the end of the function.

   On v9, the float registers are ordered a little "funny" because some
   of them (%f16-%f47) are call-preserved.  */
#ifdef SPARCV9
#define REG_ALLOC_ORDER \
{ 8, 9, 10, 11, 12, 13,			\
  15, 16, 17, 18, 19, 20, 21, 22,	\
  23, 24, 25, 26, 27, 28, 29, 31,	\
  34, 35, 36, 37, 38, 39,		/* %f2-%f7 */   \
  40, 41, 42, 43, 44, 45, 46, 47,	/* %f8-%f15 */  \
  80, 81, 82, 83, 84, 85, 86, 87,	/* %f48-%f55 */ \
  88, 89, 90, 91, 92, 93, 94, 95,	/* %f56-%f63 */ \
  48, 49, 50, 51, 52, 53, 54, 55,	/* %f16-%f23 */ \
  56, 57, 58, 59, 60, 61, 62, 63,	/* %f24-%f31 */ \
  64, 65, 66, 67, 68, 69, 70, 71,	/* %f32-%f39 */ \
  72, 73, 74, 75, 76, 77, 78, 79,	/* %f40-%f47 */ \
  32, 33,				/* %f0,%f1 */   \
  96, 97, 98, 99,			/* %fcc0-3 */   \
  1, 5, 2, 3, 4, 6, 7, 0, 14, 30}
#else
#define REG_ALLOC_ORDER \
{ 8, 9, 10, 11, 12, 13, 2, 3, 		\
  15, 16, 17, 18, 19, 20, 21, 22, 	\
  23, 24, 25, 26, 27, 28, 29, 31,	\
  34, 35, 36, 37, 38, 39,		\
  40, 41, 42, 43, 44, 45, 46, 47,	\
  48, 49, 50, 51, 52, 53, 54, 55,	\
  56, 57, 58, 59, 60, 61, 62, 63,	\
  32, 33,				\
  1, 4, 5, 6, 7, 0, 14, 30}
#endif

/* This is the order in which to allocate registers for
   leaf functions.  If all registers can fit in the "i" registers,
   then we have the possibility of having a leaf function.
   v9: The floating point registers are ordered a little "funny" because some
   of them (%f16-%f47) are call-preserved.   */
#ifdef SPARCV9
#define REG_LEAF_ALLOC_ORDER \
{ 24, 25, 26, 27, 28, 29,		\
  15, 8, 9, 10, 11, 12, 13,		\
  16, 17, 18, 19, 20, 21, 22, 23,	\
  34, 35, 36, 37, 38, 39,		\
  40, 41, 42, 43, 44, 45, 46, 47,	\
  80, 81, 82, 83, 84, 85, 86, 87,	\
  88, 89, 90, 91, 92, 93, 94, 95,	\
  48, 49, 50, 51, 52, 53, 54, 55,	\
  56, 57, 58, 59, 60, 61, 62, 63,	\
  64, 65, 66, 67, 68, 69, 70, 71,	\
  72, 73, 74, 75, 76, 77, 78, 79,	\
  32, 33,				\
  96, 97, 98, 99,			\
  1, 5, 2, 3, 4, 6, 7, 0, 14, 30, 31}
#else
#define REG_LEAF_ALLOC_ORDER \
{ 2, 3, 24, 25, 26, 27, 28, 29,		\
  15, 8, 9, 10, 11, 12, 13,		\
  16, 17, 18, 19, 20, 21, 22, 23,	\
  34, 35, 36, 37, 38, 39,		\
  40, 41, 42, 43, 44, 45, 46, 47,	\
  48, 49, 50, 51, 52, 53, 54, 55,	\
  56, 57, 58, 59, 60, 61, 62, 63,	\
  32, 33,				\
  1, 4, 5, 6, 7, 0, 14, 30, 31}
#endif

#define ORDER_REGS_FOR_LOCAL_ALLOC order_regs_for_local_alloc ()

/* ??? %g7 is not a leaf register to effectively #undef LEAF_REGISTERS when
   -mflat is used.  Function only_leaf_regs_used will return 0 if a global
   register is used and is not permitted in a leaf function.  We make %g7
   a global reg if -mflat and voila.  Since %g7 is a system register and is
   fixed it won't be used by gcc anyway.  */
#ifdef SPARCV9
#define LEAF_REGISTERS \
{ 1, 1, 1, 1, 1, 1, 1, 0,	\
  0, 0, 0, 0, 0, 0, 1, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  1, 1, 1, 1, 1, 1, 0, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1}
#else
#define LEAF_REGISTERS \
{ 1, 1, 1, 1, 1, 1, 1, 0,	\
  0, 0, 0, 0, 0, 0, 1, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  1, 1, 1, 1, 1, 1, 0, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1}
#endif

extern char leaf_reg_remap[];
#define LEAF_REG_REMAP(REGNO) (leaf_reg_remap[REGNO])

/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS GENERAL_REGS
#define BASE_REG_CLASS GENERAL_REGS

/* Local macro to handle the two v9 classes of FP regs.  */
#ifdef SPARCV9
#define FP_REG_CLASS_P(CLASS) ((CLASS) == FP_REGS || (CLASS) == EXTRA_FP_REGS)
#else
#define FP_REG_CLASS_P(CLASS) ((CLASS) == FP_REGS)
#endif

/* Get reg_class from a letter such as appears in the machine description.  */

#ifdef SPARCV9
#define REG_CLASS_FROM_LETTER(C) \
  ((C) == 'f' ? FP_REGS		\
   : (C) == 'e' ? EXTRA_FP_REGS	\
   : (C) == 'c' ? FPCC_REGS	\
   : NO_REGS)
#else
/* Coerce v9's 'e' class to 'f', so we can use 'e' in the .md file for
   v8 and v9.  */
#define REG_CLASS_FROM_LETTER(C) \
  ((C) == 'f' ? FP_REGS : (C) == 'e' ? FP_REGS : NO_REGS)
#endif

/* The letters I, J, K, L and M in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.

   For SPARC, `I' is used for the range of constants an insn
   can actually contain.
   `J' is used for the range which is just zero (since that is R0).
   `K' is used for constants which can be loaded with a single sethi insn.  */

#define SMALL_INT(X) ((unsigned) (INTVAL (X) + 0x1000) < 0x2000)

#define CONST_OK_FOR_LETTER_P(VALUE, C)  \
  ((C) == 'I' ? (unsigned) ((VALUE) + 0x1000) < 0x2000	\
   : (C) == 'J' ? (VALUE) == 0				\
   : (C) == 'K' ? ((VALUE) & 0x3ff) == 0		\
   : 0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)	\
  ((C) == 'G' ? fp_zero_operand (VALUE)			\
   : (C) == 'H' ? arith_double_operand (VALUE, DImode)	\
   : 0)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */
/* We can't load constants into FP registers.  We can't load any FP constant
   if an 'E' constraint fails to match it.  */
#define PREFERRED_RELOAD_CLASS(X,CLASS)			\
  (CONSTANT_P (X)					\
   && (FP_REG_CLASS_P (CLASS)				\
       || (GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT	\
	   && (HOST_FLOAT_FORMAT != IEEE_FLOAT_FORMAT	\
	       || HOST_BITS_PER_INT != BITS_PER_WORD)))	\
   ? NO_REGS : (CLASS))

/* Return the register class of a scratch register needed to load IN into
   a register of class CLASS in MODE.

   On the SPARC, when PIC, we need a temporary when loading some addresses
   into a register.

   Also, we need a temporary when loading/storing a HImode/QImode value
   between memory and the FPU registers.  This can happen when combine puts
   a paradoxical subreg in a float/fix conversion insn.  */

#define SECONDARY_INPUT_RELOAD_CLASS(CLASS, MODE, IN)		\
  ((FP_REG_CLASS_P (CLASS) && ((MODE) == HImode || (MODE) == QImode) \
    && (GET_CODE (IN) == MEM					\
	|| ((GET_CODE (IN) == REG || GET_CODE (IN) == SUBREG)	\
	    && true_regnum (IN) == -1))) ? GENERAL_REGS : NO_REGS)

#define SECONDARY_OUTPUT_RELOAD_CLASS(CLASS, MODE, IN)		\
  ((FP_REG_CLASS_P (CLASS) && ((MODE) == HImode || (MODE) == QImode) \
    && (GET_CODE (IN) == MEM					\
	|| ((GET_CODE (IN) == REG || GET_CODE (IN) == SUBREG)	\
	    && true_regnum (IN) == -1))) ? GENERAL_REGS : NO_REGS)

/* On SPARC it is not possible to directly move data between 
   GENERAL_REGS and FP_REGS.  */
#define SECONDARY_MEMORY_NEEDED(CLASS1, CLASS2, MODE) \
  (FP_REG_CLASS_P (CLASS1) != FP_REG_CLASS_P (CLASS2))

/* Return the stack location to use for secondary memory needed reloads.
   We want to use the reserved location just below the frame pointer.
   However, we must ensure that there is a frame, so use assign_stack_local
   if the frame size is zero.  */
#define SECONDARY_MEMORY_NEEDED_RTX(MODE) \
  (get_frame_size () == 0						\
   ? assign_stack_local (MODE, GET_MODE_SIZE (MODE), 0)			\
   : gen_rtx (MEM, MODE, gen_rtx (PLUS, Pmode, frame_pointer_rtx,	\
				  GEN_INT (STARTING_FRAME_OFFSET))))

/* Get_secondary_mem widens it's argument to BITS_PER_WORD which loses on v9
   because the movsi and movsf patterns don't handle r/f moves.
   For v8 we copy the default definition.  */
#define SECONDARY_MEMORY_NEEDED_MODE(MODE) \
  (TARGET_V9							\
   ? (GET_MODE_BITSIZE (MODE) < 32				\
      ? mode_for_size (32, GET_MODE_CLASS (MODE), 0)		\
      : MODE)							\
   : (GET_MODE_BITSIZE (MODE) < BITS_PER_WORD			\
      ? mode_for_size (BITS_PER_WORD, GET_MODE_CLASS (MODE), 0)	\
      : MODE))

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
/* On SPARC, this is the size of MODE in words.  */
#define CLASS_MAX_NREGS(CLASS, MODE)	\
  (FP_REG_CLASS_P (CLASS) ? (GET_MODE_SIZE (MODE) + 3) / 4 \
   : (GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Stack layout; function entry, exit and calling.  */

/* Define the number of register that can hold parameters.
   These two macros are used only in other macro definitions below.
   MODE is the mode of the argument.
   !v9: All args are passed in %o0-%o5.
   v9: Non-float args are passed in %o0-5 and float args are passed in
   %f0-%f15.  */
#define NPARM_REGS(MODE) \
  (TARGET_V9 ? (GET_MODE_CLASS (MODE) == MODE_FLOAT ? 16 : 6) : 6)

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
/* This allows space for one TFmode floating point value.  */
#define STARTING_FRAME_OFFSET \
  (TARGET_V9 ? (SPARC_STACK_BIAS - 16) \
   : (-SPARC_STACK_ALIGN (LONG_DOUBLE_TYPE_SIZE / BITS_PER_UNIT)))

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.
   On SPARC, don't define this because there are no push insns.  */
/*  #define PUSH_ROUNDING(BYTES) */

/* Offset of first parameter from the argument pointer register value.
   !v9: This is 64 for the ins and locals, plus 4 for the struct-return reg
   even if this function isn't going to use it.
   v9: This is 128 for the ins and locals, plus a reserved space of 8.  */
#define FIRST_PARM_OFFSET(FNDECL) \
  (TARGET_V9 ? (SPARC_STACK_BIAS + 136) \
   : (STRUCT_VALUE_OFFSET + UNITS_PER_WORD))

/* When a parameter is passed in a register, stack space is still
   allocated for it.  */
#ifndef SPARCV9
#define REG_PARM_STACK_SPACE(DECL) (NPARM_REGS (SImode) * UNITS_PER_WORD)
#endif

/* Keep the stack pointer constant throughout the function.
   This is both an optimization and a necessity: longjmp
   doesn't behave itself when the stack pointer moves within
   the function!  */
#define ACCUMULATE_OUTGOING_ARGS

/* Value is the number of bytes of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.  */

#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE) 0

/* Some subroutine macros specific to this machine.
   When !TARGET_FPU, put float return values in the general registers,
   since we don't have any fp registers.  */
#define BASE_RETURN_VALUE_REG(MODE) \
  (TARGET_V9 ? (TARGET_FPU && GET_MODE_CLASS (MODE) == MODE_FLOAT ? 32 : 8) \
   : (((MODE) == SFmode || (MODE) == DFmode) && TARGET_FPU ? 32 : 8))
#define BASE_OUTGOING_VALUE_REG(MODE) \
  (TARGET_V9 ? (TARGET_FPU && GET_MODE_CLASS (MODE) == MODE_FLOAT ? 32 \
		: TARGET_FRW ? 8 : 24) \
   : (((MODE) == SFmode || (MODE) == DFmode) && TARGET_FPU ? 32	\
      : (TARGET_FRW ? 8 : 24)))
#define BASE_PASSING_ARG_REG(MODE) \
  (TARGET_V9 ? (TARGET_FPU && GET_MODE_CLASS (MODE) == MODE_FLOAT ? 32 : 8) \
   : (8))
#define BASE_INCOMING_ARG_REG(MODE) \
  (TARGET_V9 ? (TARGET_FPU && GET_MODE_CLASS (MODE) == MODE_FLOAT ? 32 \
		: TARGET_FRW ? 8 : 24) \
   : (TARGET_FRW ? 8 : 24))

/* Define this macro if the target machine has "register windows".  This
   C expression returns the register number as seen by the called function
   corresponding to register number OUT as seen by the calling function.
   Return OUT if register number OUT is not an outbound register.  */

#define INCOMING_REGNO(OUT) \
 ((TARGET_FRW || (OUT) < 8 || (OUT) > 15) ? (OUT) : (OUT) + 16)

/* Define this macro if the target machine has "register windows".  This
   C expression returns the register number as seen by the calling function
   corresponding to register number IN as seen by the called function.
   Return IN if register number IN is not an inbound register.  */

#define OUTGOING_REGNO(IN) \
 ((TARGET_FRW || (IN) < 24 || (IN) > 31) ? (IN) : (IN) - 16)

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

/* On SPARC the value is found in the first "output" register.  */

#define FUNCTION_VALUE(VALTYPE, FUNC)  \
  gen_rtx (REG, TYPE_MODE (VALTYPE), BASE_RETURN_VALUE_REG (TYPE_MODE (VALTYPE)))

/* But the called function leaves it in the first "input" register.  */

#define FUNCTION_OUTGOING_VALUE(VALTYPE, FUNC)  \
  gen_rtx (REG, TYPE_MODE (VALTYPE), BASE_OUTGOING_VALUE_REG (TYPE_MODE (VALTYPE)))

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

#define LIBCALL_VALUE(MODE)	\
  gen_rtx (REG, MODE, BASE_RETURN_VALUE_REG (MODE))

/* 1 if N is a possible register number for a function value
   as seen by the caller.
   On SPARC, the first "output" reg is used for integer values,
   and the first floating point register is used for floating point values.  */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == 8 || (N) == 32)

/* Define the size of space to allocate for the return value of an
   untyped_call.  */

#define APPLY_RESULT_SIZE 16

/* 1 if N is a possible register number for function argument passing.
   On SPARC, these are the "output" registers.  v9 also uses %f0-%f15.  */

#define FUNCTION_ARG_REGNO_P(N) \
  (TARGET_V9 ? (((N) < 14 && (N) > 7) || (N) > 31 && (N) < 48) \
   : ((N) < 14 && (N) > 7))

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On SPARC (!v9), this is a single integer, which is a number of words
   of arguments scanned so far (including the invisible argument,
   if any, which holds the structure-value-address).
   Thus 7 or more means all following args should go on the stack.

   For v9, we record how many of each type has been passed.  Different
   types get passed differently.

	- Float args are passed in %f0-15, after which they go to the stack
	  where floats and doubles are passed 8 byte aligned and long doubles
	  are passed 16 byte aligned.
	- All aggregates are passed by reference.  The callee copies
	  the structure if necessary, except if stdarg/varargs and the struct
	  matches the ellipse in which case the caller makes a copy.
	- Any non-float argument might be split between memory and reg %o5.
	  ??? I don't think this can ever happen now that structs are no
	  longer passed in regs.

   For v9 return values:

	- For all aggregates, the caller allocates space for the return value,
          and passes the pointer as an implicit first argument, which is
          allocated like all other arguments.
	- The unimp instruction stuff for structure returns is gone.  */

#ifdef SPARCV9
enum sparc_arg_class { SPARC_ARG_INT = 0, SPARC_ARG_FLOAT = 1 };
struct sparc_args {
    int arg_count[2];	/* must be int! (for __builtin_args_info) */
};
#define CUMULATIVE_ARGS struct sparc_args

/* Return index into CUMULATIVE_ARGS.  */

#define GET_SPARC_ARG_CLASS(MODE) \
  (GET_MODE_CLASS (MODE) == MODE_FLOAT ? SPARC_ARG_FLOAT : SPARC_ARG_INT)

/* Round a register number up to a proper boundary for an arg of mode MODE.
   This macro is only used in this file.

   The "& (0x10000 - ...)" is used to round up to the next appropriate reg.  */

#define ROUND_REG(CUM, MODE)				\
  (GET_MODE_CLASS (MODE) != MODE_FLOAT			\
   ? (CUM).arg_count[(int) GET_SPARC_ARG_CLASS (MODE)]	\
   : ((CUM).arg_count[(int) GET_SPARC_ARG_CLASS (MODE)]	\
      + GET_MODE_UNIT_SIZE (MODE) / 4 - 1)		\
     & (0x10000 - GET_MODE_UNIT_SIZE (MODE) / 4))

#define ROUND_ADVANCE(SIZE)	\
  (((SIZE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

#else /* ! SPARCV9 */
#define CUMULATIVE_ARGS int

#define ROUND_REG(CUM, MODE) (CUM)

#define ROUND_ADVANCE(SIZE)	\
  ((SIZE + UNITS_PER_WORD - 1) / UNITS_PER_WORD)
#endif /* ! SPARCV9 */

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On SPARC, the offset always starts at 0: the first parm reg is always
   the same reg.  */

#ifdef SPARCV9
extern int sparc_arg_count,sparc_n_named_args;
#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME)		\
  do {								\
    (CUM).arg_count[(int) SPARC_ARG_INT] = 0;			\
    (CUM).arg_count[(int) SPARC_ARG_FLOAT] = 0;			\
    sparc_arg_count = 0;					\
    sparc_n_named_args =					\
      ((FNTYPE) && TYPE_ARG_TYPES (FNTYPE)			\
       ? (list_length (TYPE_ARG_TYPES (FNTYPE))			\
	  + (TREE_CODE (TREE_TYPE (FNTYPE)) == RECORD_TYPE	\
	     || TREE_CODE (TREE_TYPE (FNTYPE)) == UNION_TYPE))	\
       /* Can't tell, treat 'em all as named.  */		\
       : 10000);						\
  } while (0)
#else
#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME) ((CUM) = 0)
#endif

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

#ifdef SPARCV9
#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)		\
  do {								\
    (CUM).arg_count[(int) GET_SPARC_ARG_CLASS (MODE)] =		\
      ROUND_REG ((CUM), (MODE))					\
	+ (GET_MODE_CLASS (MODE) == MODE_FLOAT			\
	   ? GET_MODE_SIZE (MODE) / 4				\
	   : ROUND_ADVANCE ((MODE) == BLKmode			\
			    ? GET_MODE_SIZE (Pmode)		\
			    : GET_MODE_SIZE (MODE)));		\
    sparc_arg_count++;						\
  } while (0)
#else
#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)	\
  ((CUM) += ((MODE) != BLKmode				\
	     ? ROUND_ADVANCE (GET_MODE_SIZE (MODE))	\
	     : ROUND_ADVANCE (int_size_in_bytes (TYPE))))
#endif

/* Return boolean indicating arg of mode MODE will be passed in a reg.
   This macro is only used in this file.  */

#ifdef SPARCV9
#define PASS_IN_REG_P(CUM, MODE, TYPE)				\
  (ROUND_REG ((CUM), (MODE)) < NPARM_REGS (MODE)		\
   && ((TYPE)==0 || ! TREE_ADDRESSABLE ((tree)(TYPE)))		\
   && ((TYPE)==0 || (MODE) != BLKmode))
#else
#define PASS_IN_REG_P(CUM, MODE, TYPE)				\
  ((CUM) < NPARM_REGS (SImode)					\
   && ((TYPE)==0 || ! TREE_ADDRESSABLE ((tree)(TYPE)))		\
   && ((TYPE)==0 || (MODE) != BLKmode				\
       || (TYPE_ALIGN (TYPE) % PARM_BOUNDARY == 0)))
#endif

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
    (otherwise it is an extra parameter matching an ellipsis).  */

/* On SPARC the first six args are normally in registers
   and the rest are pushed.  Any arg that starts within the first 6 words
   is at least partially passed in a register unless its data type forbids.
   For v9, the first 6 int args are passed in regs and the first N
   float args are passed in regs (where N is such that %f0-15 are filled).
   The rest are pushed.  Any arg that starts within the first 6 words
   is at least partially passed in a register unless its data type forbids.  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED)				\
  (PASS_IN_REG_P ((CUM), (MODE), (TYPE))				\
   ? gen_rtx (REG, (MODE),						\
	      (BASE_PASSING_ARG_REG (MODE) + ROUND_REG ((CUM), (MODE))))\
   : 0)

/* Define where a function finds its arguments.
   This is different from FUNCTION_ARG because of register windows.  */

#define FUNCTION_INCOMING_ARG(CUM, MODE, TYPE, NAMED)			\
  (PASS_IN_REG_P ((CUM), (MODE), (TYPE))				\
   ? gen_rtx (REG, (MODE),						\
	      (BASE_INCOMING_ARG_REG (MODE) + ROUND_REG ((CUM), (MODE))))\
   : 0)

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.
   Any arg that starts in the first 6 regs but won't entirely fit in them
   needs partial registers on the Sparc (!v9).  On v9, there are no arguments
   that are passed partially in registers (??? complex values?).  */

#ifndef SPARCV9
#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) 		\
  (PASS_IN_REG_P ((CUM), (MODE), (TYPE))				\
   && ((CUM) + ((MODE) == BLKmode					\
		? ROUND_ADVANCE (int_size_in_bytes (TYPE))		\
		: ROUND_ADVANCE (GET_MODE_SIZE (MODE))) - NPARM_REGS (SImode) > 0)\
   ? (NPARM_REGS (SImode) - (CUM))					\
   : 0)
#endif

/* The SPARC ABI stipulates passing struct arguments (of any size) and
   (!v9) quad-precision floats by invisible reference.
   For Pascal, also pass arrays by reference.  */
#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED)		\
  ((TYPE && (TREE_CODE (TYPE) == RECORD_TYPE				\
	    || TREE_CODE (TYPE) == UNION_TYPE				\
    	    || TREE_CODE (TYPE) == ARRAY_TYPE))				\
   || (!TARGET_V9 && MODE == TFmode))

/* A C expression that indicates when it is the called function's
   responsibility to make copies of arguments passed by reference.
   If the callee can determine that the argument won't be modified, it can
   avoid the copy.  */
/* ??? We'd love to be able to use NAMED here.  Unfortunately, it doesn't
   include the last named argument so we keep track of the args ourselves.  */

#ifdef SPARCV9
#define FUNCTION_ARG_CALLEE_COPIES(CUM, MODE, TYPE, NAMED) \
  (sparc_arg_count < sparc_n_named_args)
#endif

/* Initialize data used by insn expanders.  This is called from
   init_emit, once for each function, before code is generated.
   For v9, clear the temp slot used by float/int DImode conversions.
   ??? There is the 16 bytes at [%fp-16], however we'd like to delete this
   space at some point.
   ??? Use assign_stack_temp?  */

extern void sparc64_init_expanders ();
extern struct rtx_def *sparc64_fpconv_stack_temp ();
#ifdef SPARCV9
#define INIT_EXPANDERS sparc64_init_expanders ()
#endif

/* Define the information needed to generate branch and scc insns.  This is
   stored from the compare operation.  Note that we can't use "rtx" here
   since it hasn't been defined!  */

extern struct rtx_def *sparc_compare_op0, *sparc_compare_op1;

/* Define the function that build the compare insn for scc and bcc.  */

extern struct rtx_def *gen_compare_reg ();

/* This function handles all v9 scc insns */

extern int gen_v9_scc ();

/* Generate the special assembly code needed to tell the assembler whatever
   it might need to know about the return value of a function.

   For Sparc assemblers, we need to output a .proc pseudo-op which conveys
   information to the assembler relating to peephole optimization (done in
   the assembler).  */

#define ASM_DECLARE_RESULT(FILE, RESULT) \
  fprintf ((FILE), "\t.proc\t0%o\n", sparc_type_code (TREE_TYPE (RESULT)))

/* Output the label for a function definition.  */

#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)			\
do {									\
  ASM_DECLARE_RESULT (FILE, DECL_RESULT (DECL));			\
  ASM_OUTPUT_LABEL (FILE, NAME);					\
} while (0)

/* This macro generates the assembly code for function entry.
   FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.
   Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This macro is responsible for
   knowing which registers should not be saved even if used.  */

/* On SPARC, move-double insns between fpu and cpu need an 8-byte block
   of memory.  If any fpu reg is used in the function, we allocate
   such a block here, at the bottom of the frame, just in case it's needed.

   If this function is a leaf procedure, then we may choose not
   to do a "save" insn.  The decision about whether or not
   to do this is made in regclass.c.  */

extern int leaf_function;
#define FUNCTION_PROLOGUE(FILE, SIZE)				\
  (TARGET_FRW ? sparc_flat_output_function_prologue (FILE, SIZE) \
   : output_function_prologue (FILE, SIZE, leaf_function))

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)  			\
  do {								\
    fputs ("\tsethi %hi(", (FILE));				\
    ASM_OUTPUT_INTERNAL_LABELREF (FILE, "LP", LABELNO);		\
    fputs ("),%o0\n", (FILE));					\
    if (TARGET_MEDANY)						\
      fprintf (FILE, "\tadd %%o0,%s,%%o0\n",			\
	       MEDANY_BASE_REG);				\
    fputs ("\tcall mcount\n\tadd %lo(", (FILE));		\
    ASM_OUTPUT_INTERNAL_LABELREF (FILE, "LP", LABELNO);		\
    fputs ("),%o0,%o0\n", (FILE));				\
  } while (0)

/* Output assembler code to FILE to initialize this source file's
   basic block profiling info, if that has not already been done.  */

#define FUNCTION_BLOCK_PROFILER(FILE, LABELNO)  \
  do { \
    if (TARGET_MEDANY) \
      fprintf (FILE, "\tsethi %%hi(LPBX0),%%o0\n\tor %%0,%%lo(LPBX0),%%o0\n\tld [%s+%%o0],%%o1\n\ttst %%o1\n\tbne LPY%d\n\tadd %%o0,%s,%%o0\n\tcall ___bb_init_func\n\tnop\nLPY%d:\n", \
	       MEDANY_BASE_REG, (LABELNO), MEDANY_BASE_REG, (LABELNO)); \
    else \
      fprintf (FILE, "\tsethi %%hi(LPBX0),%%o0\n\tld [%%lo(LPBX0)+%%o0],%%o1\n\ttst %%o1\n\tbne LPY%d\n\tadd %%o0,%%lo(LPBX0),%%o0\n\tcall ___bb_init_func\n\tnop\nLPY%d:\n", \
	       (LABELNO), (LABELNO)); \
  } while (0)

/* Output assembler code to FILE to increment the entry-count for
   the BLOCKNO'th basic block in this source file.  */

#define BLOCK_PROFILER(FILE, BLOCKNO) \
{ \
  int blockn = (BLOCKNO); \
  if (TARGET_MEDANY) \
    fprintf (FILE, "\tsethi %%hi(LPBX2+%d),%%g1\n\tor %%g1,%%lo(LPBX2+%d),%%g1\n\tld [%%g1+%s],%%g2\n\tadd %%g2,1,%%g2\n\tst %%g2,[%%g1+%s]\n", \
	     4 * blockn, 4 * blockn, MEDANY_BASE_REG, MEDANY_BASE_REG); \
  else \
    fprintf (FILE, "\tsethi %%hi(LPBX2+%d),%%g1\n\tld [%%lo(LPBX2+%d)+%%g1],%%g2\n\
\tadd %%g2,1,%%g2\n\tst %%g2,[%%lo(LPBX2+%d)+%%g1]\n", \
	     4 * blockn, 4 * blockn, 4 * blockn); \
}

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

extern int current_function_calls_alloca;
extern int current_function_outgoing_args_size;

#define EXIT_IGNORE_STACK	\
 (get_frame_size () != 0	\
  || current_function_calls_alloca || current_function_outgoing_args_size)

/* This macro generates the assembly code for function exit,
   on machines that need it.  If FUNCTION_EPILOGUE is not defined
   then individual return instructions are generated for each
   return statement.  Args are same as for FUNCTION_PROLOGUE.

   The function epilogue should not depend on the current stack pointer!
   It should use the frame pointer only.  This is mandatory because
   of alloca; we also take advantage of it to omit stack adjustments
   before returning.  */

/* This declaration is needed due to traditional/ANSI
   incompatibilities which cannot be #ifdefed away
   because they occur inside of macros.  Sigh.  */
extern union tree_node *current_function_decl;

#define FUNCTION_EPILOGUE(FILE, SIZE)				\
  (TARGET_FRW ? sparc_flat_output_function_epilogue (FILE, SIZE) \
   : output_function_epilogue (FILE, SIZE, leaf_function))

#define DELAY_SLOTS_FOR_EPILOGUE	\
  (TARGET_FRW ? sparc_flat_epilogue_delay_slots () : 1)
#define ELIGIBLE_FOR_EPILOGUE_DELAY(trial, slots_filled)	\
  (TARGET_FRW ? sparc_flat_eligible_for_epilogue_delay (trial, slots_filled) \
   : eligible_for_epilogue_delay (trial, slots_filled))

/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.  */

/* On 32 bit sparcs, the trampoline contains five instructions:
     sethi #TOP_OF_FUNCTION,%g1
     or #BOTTOM_OF_FUNCTION,%g1,%g1
     sethi #TOP_OF_STATIC,%g2
     jmp g1
     or #BOTTOM_OF_STATIC,%g2,%g2

  On 64 bit sparcs, the trampoline contains 4 insns and two pseudo-immediate
  constants (plus some padding):
     rd %pc,%g1
     ldx[%g1+20],%g5
     ldx[%g1+28],%g1
     jmp %g1
     nop
     nop
     .xword context
     .xword function  */

#define TRAMPOLINE_TEMPLATE(FILE) \
do {									\
  if (TARGET_V9)							\
    {									\
      fprintf (FILE, "\trd %%pc,%%g1\n");				\
      fprintf (FILE, "\tldx [%%g1+24],%%g5\n");				\
      fprintf (FILE, "\tldx [%%g1+32],%%g1\n");				\
      fprintf (FILE, "\tjmp %%g1\n");					\
      fprintf (FILE, "\tnop\n");					\
      fprintf (FILE, "\tnop\n");					\
      /* -mmedlow shouldn't generate .xwords, so don't use them at all */ \
      fprintf (FILE, "\t.word 0,0,0,0\n");				\
    }									\
  else									\
    {									\
      ASM_OUTPUT_INT (FILE, gen_rtx (CONST_INT, VOIDmode, 0x00000000));	\
      ASM_OUTPUT_INT (FILE, gen_rtx (CONST_INT, VOIDmode, 0x00000000));	\
      ASM_OUTPUT_INT (FILE, gen_rtx (CONST_INT, VOIDmode, 0x00000000));	\
      ASM_OUTPUT_INT (FILE, gen_rtx (CONST_INT, VOIDmode, 0x81C04000));	\
      ASM_OUTPUT_INT (FILE, gen_rtx (CONST_INT, VOIDmode, 0x00000000));	\
    }									\
} while (0)

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE (TARGET_V9 ? 40 : 20)

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

void sparc_initialize_trampoline ();
void sparc64_initialize_trampoline ();
#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT) \
  do {								\
    if (TARGET_V9)						\
      sparc64_initialize_trampoline (TRAMP, FNADDR, CXT);	\
    else							\
      sparc_initialize_trampoline (TRAMP, FNADDR, CXT);		\
  } while (0)

/* Generate necessary RTL for __builtin_saveregs().
   ARGLIST is the argument list; see expr.c.  */
extern struct rtx_def *sparc_builtin_saveregs ();
#define EXPAND_BUILTIN_SAVEREGS(ARGLIST) sparc_builtin_saveregs (ARGLIST)

/* Generate RTL to flush the register windows so as to make arbitrary frames
   available.  */
#define SETUP_FRAME_ADDRESSES()		\
  emit_insn (gen_flush_register_windows ())

/* Given an rtx for the address of a frame,
   return an rtx for the address of the word in the frame
   that holds the dynamic chain--the previous frame's address.
   ??? -mflat support? */
#define DYNAMIC_CHAIN_ADDRESS(frame) \
  gen_rtx (PLUS, Pmode, frame, gen_rtx (CONST_INT, VOIDmode, 14 * UNITS_PER_WORD))

/* The return address isn't on the stack, it is in a register, so we can't
   access it from the current frame pointer.  We can access it from the
   previous frame pointer though by reading a value from the register window
   save area.  */
#define RETURN_ADDR_IN_PREVIOUS_FRAME

/* This is the offset of the return address to the true next instruction to be
   executed for normal void functions. */
#define NORMAL_RETURN_ADDR_OFFSET (8)

/* The current return address is in %i7.  The return address of anything
   farther back is in the register window save area at [%fp+60].  */
/* ??? This ignores the fact that the actual return address is +8 for normal
   returns, and +12 for structure returns.  */
#define RETURN_ADDR_RTX(count, frame)		\
  ((count == -1)				\
   ? gen_rtx (REG, Pmode, 31)			\
   : gen_rtx (MEM, Pmode,			\
	      memory_address (Pmode, plus_constant (frame, 15 * UNITS_PER_WORD))))

/* Addressing modes, and classification of registers for them.  */

/* #define HAVE_POST_INCREMENT */
/* #define HAVE_POST_DECREMENT */

/* #define HAVE_PRE_DECREMENT */
/* #define HAVE_PRE_INCREMENT */

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */

#define REGNO_OK_FOR_INDEX_P(REGNO) \
(((REGNO) < 32 || (unsigned) reg_renumber[REGNO] < 32) && (REGNO) != 0)
#define REGNO_OK_FOR_BASE_P(REGNO) \
(((REGNO) < 32 || (unsigned) reg_renumber[REGNO] < 32) && (REGNO) != 0)
#define REGNO_OK_FOR_FP_P(REGNO) \
  (((unsigned) (REGNO) - 32 < (TARGET_V9 ? 64 : 32)) \
   || ((unsigned) reg_renumber[REGNO] - 32 < (TARGET_V9 ? 64 : 32)))
#define REGNO_OK_FOR_CCFP_P(REGNO) \
 (TARGET_V9 \
  && ((unsigned) (REGNO) - 96 < 4) || ((unsigned) reg_renumber[REGNO] - 96 < 4))

/* Now macros that check whether X is a register and also,
   strictly, whether it is in a specified class.

   These macros are specific to the SPARC, and may be used only
   in code for printing assembler insns and in conditions for
   define_optimization.  */

/* 1 if X is an fp register.  */

#define FP_REG_P(X) (REG_P (X) && REGNO_OK_FOR_FP_P (REGNO (X)))

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 2

/* Recognize any constant value that is a valid address.
   When PIC, we do not accept an address that would require a scratch reg
   to load into a register.  */

#define CONSTANT_ADDRESS_P(X)   \
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
   || GET_CODE (X) == CONST_INT || GET_CODE (X) == HIGH			\
   || (GET_CODE (X) == CONST						\
       && ! (flag_pic && pic_address_needs_scratch (X))))

/* Define this, so that when PIC, reload won't try to reload invalid
   addresses which require two reload registers.  */

#define LEGITIMATE_PIC_OPERAND_P(X)  (! pic_address_needs_scratch (X))

/* Nonzero if the constant value X is a legitimate general operand.
   Anything can be made to work except floating point constants.  */

#define LEGITIMATE_CONSTANT_P(X) \
  (GET_CODE (X) != CONST_DOUBLE || GET_MODE (X) == VOIDmode)

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

/* Optional extra constraints for this machine.  Borrowed from romp.h.

   For the SPARC, `Q' means that this is a memory operand but not a
   symbolic memory operand.  Note that an unassigned pseudo register
   is such a memory operand.  Needed because reload will generate
   these things in insns and then not re-recognize the insns, causing
   constrain_operands to fail.

   `S' handles constraints for calls.  ??? So where is it?  */

#ifndef REG_OK_STRICT

/* Nonzero if X is a hard reg that can be used as an index
   or if it is a pseudo reg.  */
#define REG_OK_FOR_INDEX_P(X) \
  (((unsigned) REGNO (X)) - 32 >= (FIRST_PSEUDO_REGISTER - 32) && REGNO (X) != 0)
/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) \
  (((unsigned) REGNO (X)) - 32 >= (FIRST_PSEUDO_REGISTER - 32) && REGNO (X) != 0)

/* 'T', 'U' are for aligned memory loads which aren't needed for v9.  */

#define EXTRA_CONSTRAINT(OP, C)				\
  ((C) == 'Q'						\
   ? ((GET_CODE (OP) == MEM				\
       && memory_address_p (GET_MODE (OP), XEXP (OP, 0)) \
       && ! symbolic_memory_operand (OP, VOIDmode))	\
      || (reload_in_progress && GET_CODE (OP) == REG	\
	  && REGNO (OP) >= FIRST_PSEUDO_REGISTER))	\
   : ! TARGET_V9 && (C) == 'T'			\
   ? (mem_aligned_8 (OP))				\
   : ! TARGET_V9 && (C) == 'U'						\
   ? (register_ok_for_ldd (OP))				\
   : 0)
 
#else

/* Nonzero if X is a hard reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
/* Nonzero if X is a hard reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))

#define EXTRA_CONSTRAINT(OP, C)				\
  ((C) == 'Q'						\
   ? (GET_CODE (OP) == REG				\
      ? (REGNO (OP) >= FIRST_PSEUDO_REGISTER		\
	 && reg_renumber[REGNO (OP)] < 0)		\
      : GET_CODE (OP) == MEM)				\
   : ! TARGET_V9 && (C) == 'T'			\
   ? mem_aligned_8 (OP) && strict_memory_address_p (Pmode, XEXP (OP, 0)) \
   : ! TARGET_V9 && (C) == 'U'			\
   ? (GET_CODE (OP) == REG				\
      && (REGNO (OP) < FIRST_PSEUDO_REGISTER		\
	  || reg_renumber[REGNO (OP)] > 0)		\
      && register_ok_for_ldd (OP)) : 0)
#endif

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.

   On SPARC, the actual legitimate addresses must be REG+REG or REG+SMALLINT
   ordinarily.  This changes a bit when generating PIC.

   If you change this, execute "rm explow.o recog.o reload.o".  */

#define RTX_OK_FOR_BASE_P(X)						\
  ((GET_CODE (X) == REG && REG_OK_FOR_BASE_P (X))			\
  || (GET_CODE (X) == SUBREG						\
      && GET_CODE (SUBREG_REG (X)) == REG				\
      && REG_OK_FOR_BASE_P (SUBREG_REG (X))))

#define RTX_OK_FOR_INDEX_P(X)						\
  ((GET_CODE (X) == REG && REG_OK_FOR_INDEX_P (X))			\
  || (GET_CODE (X) == SUBREG						\
      && GET_CODE (SUBREG_REG (X)) == REG				\
      && REG_OK_FOR_INDEX_P (SUBREG_REG (X))))

#define RTX_OK_FOR_OFFSET_P(X)						\
  (GET_CODE (X) == CONST_INT && INTVAL (X) >= -0x1000 && INTVAL (X) < 0x1000)

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)		\
{ if (RTX_OK_FOR_BASE_P (X))				\
    goto ADDR;						\
  else if (GET_CODE (X) == PLUS)			\
    {							\
      register rtx op0 = XEXP (X, 0);			\
      register rtx op1 = XEXP (X, 1);			\
      if (flag_pic && op0 == pic_offset_table_rtx)	\
	{						\
	  if (RTX_OK_FOR_BASE_P (op1))			\
	    goto ADDR;					\
	  else if (flag_pic == 1			\
		   && GET_CODE (op1) != REG		\
		   && GET_CODE (op1) != LO_SUM		\
		   && GET_CODE (op1) != MEM		\
		   && (GET_CODE (op1) != CONST_INT	\
		       || SMALL_INT (op1)))		\
	    goto ADDR;					\
	}						\
      else if (RTX_OK_FOR_BASE_P (op0))			\
	{						\
	  if (RTX_OK_FOR_INDEX_P (op1)			\
	      || RTX_OK_FOR_OFFSET_P (op1))		\
	    goto ADDR;					\
	}						\
      else if (RTX_OK_FOR_BASE_P (op1))			\
	{						\
	  if (RTX_OK_FOR_INDEX_P (op0)			\
	      || RTX_OK_FOR_OFFSET_P (op0))		\
	    goto ADDR;					\
	}						\
    }							\
  else if (GET_CODE (X) == LO_SUM)			\
    {							\
      register rtx op0 = XEXP (X, 0);			\
      register rtx op1 = XEXP (X, 1);			\
      if (RTX_OK_FOR_BASE_P (op0)			\
	  && CONSTANT_P (op1)				\
	  /* We can't allow TFmode, because an offset	\
	     greater than or equal to the alignment (8)	\
	     may cause the LO_SUM to overflow.  */	\
	  && MODE != TFmode)				\
	goto ADDR;					\
    }							\
  else if (GET_CODE (X) == CONST_INT && SMALL_INT (X))	\
    goto ADDR;						\
}

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.  */

/* On SPARC, change REG+N into REG+REG, and REG+(X*Y) into REG+REG.  */
extern struct rtx_def *legitimize_pic_address ();
#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)	\
{ rtx sparc_x = (X);						\
  if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 0)) == MULT)	\
    (X) = gen_rtx (PLUS, Pmode, XEXP (X, 1),			\
		   force_operand (XEXP (X, 0), NULL_RTX));	\
  if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 1)) == MULT)	\
    (X) = gen_rtx (PLUS, Pmode, XEXP (X, 0),			\
		   force_operand (XEXP (X, 1), NULL_RTX));	\
  if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 0)) == PLUS)	\
    (X) = gen_rtx (PLUS, Pmode, force_operand (XEXP (X, 0), NULL_RTX),\
		   XEXP (X, 1));				\
  if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 1)) == PLUS)	\
    (X) = gen_rtx (PLUS, Pmode, XEXP (X, 0),			\
		   force_operand (XEXP (X, 1), NULL_RTX));	\
  if (sparc_x != (X) && memory_address_p (MODE, X))		\
    goto WIN;							\
  if (flag_pic) (X) = legitimize_pic_address (X, MODE, 0);	\
  else if (GET_CODE (X) == PLUS && CONSTANT_ADDRESS_P (XEXP (X, 1)))	\
    (X) = gen_rtx (PLUS, Pmode, XEXP (X, 0),			\
		   copy_to_mode_reg (Pmode, XEXP (X, 1)));	\
  else if (GET_CODE (X) == PLUS && CONSTANT_ADDRESS_P (XEXP (X, 0)))	\
    (X) = gen_rtx (PLUS, Pmode, XEXP (X, 1),			\
		   copy_to_mode_reg (Pmode, XEXP (X, 0)));	\
  else if (GET_CODE (X) == SYMBOL_REF || GET_CODE (X) == CONST	\
	   || GET_CODE (X) == LABEL_REF)			\
    (X) = gen_rtx (LO_SUM, Pmode,				\
		   copy_to_mode_reg (Pmode, gen_rtx (HIGH, Pmode, X)), X); \
  if (memory_address_p (MODE, X))				\
    goto WIN; }

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.
   On the SPARC this is never true.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)

/* If we are referencing a function make the SYMBOL_REF special.
   In the Medium/Anywhere code model, %g4 points to the data segment so we
   must not add it to function addresses.  */

#define ENCODE_SECTION_INFO(DECL) \
  do {							\
    if (TARGET_MEDANY && TREE_CODE (DECL) == FUNCTION_DECL) \
      SYMBOL_REF_FLAG (XEXP (DECL_RTL (DECL), 0)) = 1;	\
  } while (0)

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE Pmode

/* Define this if the tablejump instruction expects the table
   to contain offsets from the address of the table.
   Do not define this if the table should contain absolute addresses.  */
/* #define CASE_VECTOR_PC_RELATIVE */

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 1

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 8

#if 0 /* Sun 4 has matherr, so this is no good.  */
/* This is the value of the error code EDOM for this machine,
   used by the sqrt instruction.  */
#define TARGET_EDOM 33

/* This is how to refer to the variable errno.  */
#define GEN_ERRNO_RTX \
  gen_rtx (MEM, SImode, gen_rtx (SYMBOL_REF, Pmode, "errno"))
#endif /* 0 */

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, NIL if none.  */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* Nonzero if access to memory by bytes is slow and undesirable.
   For RISC chips, it means that access to memory by bytes is no
   better than access by words when possible, so grab a whole word
   and maybe make use of that.  */
#define SLOW_BYTE_ACCESS 1

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */

#define STORE_FLAG_VALUE 1

/* When a prototype says `char' or `short', really pass an `int'.  */
#define PROMOTE_PROTOTYPES

/* Define this to be nonzero if shift instructions ignore all but the low-order
   few bits. */
#define SHIFT_COUNT_TRUNCATED 1

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode (TARGET_PTR64 ? DImode : SImode)

/* Generate calls to memcpy, memcmp and memset.  */
#define TARGET_MEM_FUNCTIONS

/* Add any extra modes needed to represent the condition code.

   On the Sparc, we have a "no-overflow" mode which is used when an add or
   subtract insn is used to set the condition code.  Different branches are
   used in this case for some operations.

   We also have two modes to indicate that the relevant condition code is
   in the floating-point condition code register.  One for comparisons which
   will generate an exception if the result is unordered (CCFPEmode) and
   one for comparisons which will never trap (CCFPmode).  This really should
   be a separate register, but we don't want to go to 65 registers.

   CCXmode and CCX_NOOVmode are only used by v9.  */

#define EXTRA_CC_MODES CCXmode, CC_NOOVmode, CCX_NOOVmode, CCFPmode, CCFPEmode

/* Define the names for the modes specified above.  */

#define EXTRA_CC_NAMES "CCX", "CC_NOOV", "CCX_NOOV", "CCFP", "CCFPE"

/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
   return the mode to be used for the comparison.  For floating-point,
   CCFP[E]mode is used.  CC_NOOVmode should be used when the first operand is a
   PLUS, MINUS, NEG, or ASHIFT.  CCmode should be used when no special
   processing is needed.  */
#define SELECT_CC_MODE(OP,X,Y) \
  (GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT				\
   ? ((OP == EQ || OP == NE) ? CCFPmode : CCFPEmode)			\
   : ((GET_CODE (X) == PLUS || GET_CODE (X) == MINUS			\
       || GET_CODE (X) == NEG || GET_CODE (X) == ASHIFT)		\
      ? (TARGET_V9 && GET_MODE (X) == DImode ? CCX_NOOVmode : CC_NOOVmode) \
      : (TARGET_V9 && GET_MODE (X) == DImode ? CCXmode : CCmode)))

/* Return non-zero if SELECT_CC_MODE will never return MODE for a
   floating point inequality comparison.  */

#define REVERSIBLE_CC_MODE(MODE) ((MODE) != CCFPEmode)

/* A function address in a call instruction
   is a byte address (for indexing purposes)
   so give the MEM rtx a byte's mode.  */
#define FUNCTION_MODE SImode

/* Define this if addresses of constant functions
   shouldn't be put through pseudo regs where they can be cse'd.
   Desirable on machines where ordinary constants are expensive
   but a CALL with constant address is cheap.  */
#define NO_FUNCTION_CSE

/* alloca should avoid clobbering the old register save area.  */
#define SETJMP_VIA_SAVE_AREA

/* Define subroutines to call to handle multiply and divide.
   Use the subroutines that Sun's library provides.
   The `*' prevents an underscore from being prepended by the compiler.  */

#define DIVSI3_LIBCALL "*.div"
#define UDIVSI3_LIBCALL "*.udiv"
#define MODSI3_LIBCALL "*.rem"
#define UMODSI3_LIBCALL "*.urem"
/* .umul is a little faster than .mul.  */
#define MULSI3_LIBCALL "*.umul"

/* Define library calls for quad FP operations.  These are all part of the
   SPARC ABI.  */
#define ADDTF3_LIBCALL "_Q_add"
#define SUBTF3_LIBCALL "_Q_sub"
#define NEGTF2_LIBCALL "_Q_neg"
#define MULTF3_LIBCALL "_Q_mul"
#define DIVTF3_LIBCALL "_Q_div"
#define FLOATSITF2_LIBCALL "_Q_itoq"
#define FIX_TRUNCTFSI2_LIBCALL "_Q_qtoi"
#define FIXUNS_TRUNCTFSI2_LIBCALL "_Q_qtou"
#define EXTENDSFTF2_LIBCALL "_Q_stoq"
#define TRUNCTFSF2_LIBCALL "_Q_qtos"
#define EXTENDDFTF2_LIBCALL "_Q_dtoq"
#define TRUNCTFDF2_LIBCALL "_Q_qtod"
#define EQTF2_LIBCALL "_Q_feq"
#define NETF2_LIBCALL "_Q_fne"
#define GTTF2_LIBCALL "_Q_fgt"
#define GETF2_LIBCALL "_Q_fge"
#define LTTF2_LIBCALL "_Q_flt"
#define LETF2_LIBCALL "_Q_fle"

/* We can define the TFmode sqrt optab only if TARGET_FPU.  This is because
   with soft-float, the SFmode and DFmode sqrt instructions will be absent,
   and the compiler will notice and try to use the TFmode sqrt instruction
   for calls to the builtin function sqrt, but this fails.  */
#define INIT_TARGET_OPTABS						\
  do {									\
    add_optab->handlers[(int) TFmode].libfunc				\
      = gen_rtx (SYMBOL_REF, Pmode, ADDTF3_LIBCALL);			\
    sub_optab->handlers[(int) TFmode].libfunc				\
      = gen_rtx (SYMBOL_REF, Pmode, SUBTF3_LIBCALL);			\
    neg_optab->handlers[(int) TFmode].libfunc				\
      = gen_rtx (SYMBOL_REF, Pmode, NEGTF2_LIBCALL);			\
    smul_optab->handlers[(int) TFmode].libfunc				\
      = gen_rtx (SYMBOL_REF, Pmode, MULTF3_LIBCALL);			\
    flodiv_optab->handlers[(int) TFmode].libfunc			\
      = gen_rtx (SYMBOL_REF, Pmode, DIVTF3_LIBCALL);			\
    eqtf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, EQTF2_LIBCALL);		\
    netf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, NETF2_LIBCALL);		\
    gttf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, GTTF2_LIBCALL);		\
    getf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, GETF2_LIBCALL);		\
    lttf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, LTTF2_LIBCALL);		\
    letf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, LETF2_LIBCALL);		\
    trunctfsf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, TRUNCTFSF2_LIBCALL);   \
    trunctfdf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, TRUNCTFDF2_LIBCALL);   \
    extendsftf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, EXTENDSFTF2_LIBCALL); \
    extenddftf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, EXTENDDFTF2_LIBCALL); \
    floatsitf_libfunc = gen_rtx (SYMBOL_REF, Pmode, FLOATSITF2_LIBCALL);    \
    fixtfsi_libfunc = gen_rtx (SYMBOL_REF, Pmode, FIX_TRUNCTFSI2_LIBCALL);  \
    fixunstfsi_libfunc							\
      = gen_rtx (SYMBOL_REF, Pmode, FIXUNS_TRUNCTFSI2_LIBCALL);		\
    if (TARGET_FPU)							\
      sqrt_optab->handlers[(int) TFmode].libfunc			\
	= gen_rtx (SYMBOL_REF, Pmode, "_Q_sqrt");			\
    INIT_SUBTARGET_OPTABS;						\
  } while (0)

/* This is meant to be redefined in the host dependent files */
#define INIT_SUBTARGET_OPTABS

/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch.  */

#define CONST_COSTS(RTX,CODE,OUTER_CODE) \
  case CONST_INT:						\
    if (INTVAL (RTX) < 0x1000 && INTVAL (RTX) >= -0x1000)	\
      return 0;							\
  case HIGH:							\
    return 2;							\
  case CONST:							\
  case LABEL_REF:						\
  case SYMBOL_REF:						\
    return 4;							\
  case CONST_DOUBLE:						\
    if (GET_MODE (RTX) == DImode)				\
      if ((XINT (RTX, 3) == 0					\
	   && (unsigned) XINT (RTX, 2) < 0x1000)		\
	  || (XINT (RTX, 3) == -1				\
	      && XINT (RTX, 2) < 0				\
	      && XINT (RTX, 2) >= -0x1000))			\
	return 0;						\
    return 8;

/* Compute the cost of an address.  For the sparc, all valid addresses are
   the same cost.
   ??? Is this true for v9?  */

#define ADDRESS_COST(RTX)  1

/* Compute extra cost of moving data between one register class
   and another.
   ??? v9: We ignore FPCC_REGS on the assumption they'll never be seen.  */
#define REGISTER_MOVE_COST(CLASS1, CLASS2) \
  (((FP_REG_CLASS_P (CLASS1) && (CLASS2) == GENERAL_REGS) \
    || ((CLASS1) == GENERAL_REGS && FP_REG_CLASS_P (CLASS2))) ? 6 : 2)

/* Provide the costs of a rtl expression.  This is in the body of a
   switch on CODE.  The purpose for the cost of MULT is to encourage
   `synth_mult' to find a synthetic multiply when reasonable.

   If we need more than 12 insns to do a multiply, then go out-of-line,
   since the call overhead will be < 10% of the cost of the multiply.  */

#define RTX_COSTS(X,CODE,OUTER_CODE)			\
  case MULT:						\
    return (TARGET_V8 || TARGET_SPARCLITE || TARGET_V9) ? COSTS_N_INSNS (5) : COSTS_N_INSNS (25);	\
  case DIV:						\
  case UDIV:						\
  case MOD:						\
  case UMOD:						\
    return COSTS_N_INSNS (25);				\
  /* Make FLOAT and FIX more expensive than CONST_DOUBLE,\
     so that cse will favor the latter.  */		\
  case FLOAT:						\
  case FIX:						\
    return 19;

/* Adjust the cost of dependencies.  */
#define ADJUST_COST(INSN,LINK,DEP,COST) \
  if (TARGET_SUPERSPARC) \
  (COST) = supersparc_adjust_cost (INSN, LINK, DEP, COST)

/* Conditional branches with empty delay slots have a length of two.  */
#define ADJUST_INSN_LENGTH(INSN, LENGTH)	\
  if (GET_CODE (INSN) == CALL_INSN					\
      || (GET_CODE (INSN) == JUMP_INSN && ! simplejump_p (insn)))	\
    LENGTH += 1;

/* Control the assembler format that we output.  */

/* Output at beginning of assembler file.  */

#define ASM_FILE_START(file)

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON ""

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF ""

/* ??? Try to make the style consistent here (_OP?).  */

#define ASM_LONGLONG	".xword"
#define ASM_LONG	".word"
#define ASM_SHORT	".half"
#define ASM_BYTE_OP	".byte"
#define ASM_FLOAT	".single"
#define ASM_DOUBLE	".double"
#define ASM_LONGDOUBLE	".xxx"		/* ??? Not known (or used yet). */

/* Output before read-only data.  */

#define TEXT_SECTION_ASM_OP ".text"

/* Output before writable data.  */

#define DATA_SECTION_ASM_OP ".data"

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#ifdef SPARCV9
#define REGISTER_NAMES \
{"%g0", "%g1", "%g2", "%g3", "%g4", "%g5", "%g6", "%g7",		\
 "%o0", "%o1", "%o2", "%o3", "%o4", "%o5", "%sp", "%o7",		\
 "%l0", "%l1", "%l2", "%l3", "%l4", "%l5", "%l6", "%l7",		\
 "%i0", "%i1", "%i2", "%i3", "%i4", "%i5", "%fp", "%i7",		\
 "%f0", "%f1", "%f2", "%f3", "%f4", "%f5", "%f6", "%f7",		\
 "%f8", "%f9", "%f10", "%f11", "%f12", "%f13", "%f14", "%f15",		\
 "%f16", "%f17", "%f18", "%f19", "%f20", "%f21", "%f22", "%f23",	\
 "%f24", "%f25", "%f26", "%f27", "%f28", "%f29", "%f30", "%f31",	\
 "%f32", "%f33", "%f34", "%f35", "%f36", "%f37", "%f38", "%f39",	\
 "%f40", "%f41", "%f42", "%f43", "%f44", "%f45", "%f46", "%f47",	\
 "%f48", "%f49", "%f50", "%f51", "%f52", "%f53", "%f54", "%f55",	\
 "%f56", "%f57", "%f58", "%f59", "%f60", "%f61", "%f62", "%f63",	\
 "%fcc0", "%fcc1", "%fcc2", "%fcc3"}
#else
#define REGISTER_NAMES \
{"%g0", "%g1", "%g2", "%g3", "%g4", "%g5", "%g6", "%g7",		\
 "%o0", "%o1", "%o2", "%o3", "%o4", "%o5", "%sp", "%o7",		\
 "%l0", "%l1", "%l2", "%l3", "%l4", "%l5", "%l6", "%l7",		\
 "%i0", "%i1", "%i2", "%i3", "%i4", "%i5", "%fp", "%i7",		\
 "%f0", "%f1", "%f2", "%f3", "%f4", "%f5", "%f6", "%f7",		\
 "%f8", "%f9", "%f10", "%f11", "%f12", "%f13", "%f14", "%f15",		\
 "%f16", "%f17", "%f18", "%f19", "%f20", "%f21", "%f22", "%f23",	\
 "%f24", "%f25", "%f26", "%f27", "%f28", "%f29", "%f30", "%f31"}
#endif

/* Define additional names for use in asm clobbers and asm declarations.

   We define the fake Condition Code register as an alias for reg 0 (which
   is our `condition code' register), so that condition codes can easily
   be clobbered by an asm.  No such register actually exists.  Condition
   codes are partly stored in the PSR and partly in the FSR.  */

#define ADDITIONAL_REGISTER_NAMES	{"ccr", 0, "cc", 0}

/* How to renumber registers for dbx and gdb.  */

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* On Sun 4, this limit is 2048.  We use 1500 to be safe,
   since the length can run past this up to a continuation point.  */
#define DBX_CONTIN_LENGTH 1500

/* This is how to output a note to DBX telling it the line number
   to which the following sequence of instructions corresponds.

   This is needed for SunOS 4.0, and should not hurt for 3.2
   versions either.  */
#define ASM_OUTPUT_SOURCE_LINE(file, line)		\
  { static int sym_lineno = 1;				\
    fprintf (file, ".stabn 68,0,%d,LM%d\nLM%d:\n",	\
	     line, sym_lineno, sym_lineno);		\
    sym_lineno += 1; }

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#define ASM_OUTPUT_LABEL(FILE,NAME)	\
  do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0)

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(FILE,NAME)	\
  do { fputs ("\t.global ", FILE); assemble_name (FILE, NAME); fputs ("\n", FILE);} while (0)

/* This is how to output a reference to a user-level label named NAME.
   `assemble_name' uses this.  */

#define ASM_OUTPUT_LABELREF(FILE,NAME)	\
  fprintf (FILE, "_%s", NAME)

/* This is how to output a definition of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, "%s%d:\n", PREFIX, NUM)

/* This is how to output a reference to an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */
/* FIXME:  This should be used throughout gcc, and documented in the texinfo
   files.  There is no reason you should have to allocate a buffer and
   `sprintf' to reference an internal label (as opposed to defining it).  */

#define ASM_OUTPUT_INTERNAL_LABELREF(FILE,PREFIX,NUM)	\
  fprintf (FILE, "%s%d", PREFIX, NUM)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*%s%d", PREFIX, NUM)

/* This is how to output an assembler line defining a `double' constant.  */

#define ASM_OUTPUT_DOUBLE(FILE,VALUE)					\
  {									\
    long t[2];								\
    REAL_VALUE_TO_TARGET_DOUBLE ((VALUE), t);				\
    fprintf (FILE, "\t%s\t0x%lx\n\t%s\t0x%lx\n",			\
	     ASM_LONG, t[0], ASM_LONG, t[1]);				\
  }

/* This is how to output an assembler line defining a `float' constant.  */

#define ASM_OUTPUT_FLOAT(FILE,VALUE)					\
  {									\
    long t;								\
    REAL_VALUE_TO_TARGET_SINGLE ((VALUE), t);				\
    fprintf (FILE, "\t%s\t0x%lx\n", ASM_LONG, t);			\
  }									\

/* This is how to output an assembler line defining a `long double'
   constant.  */

#define ASM_OUTPUT_LONG_DOUBLE(FILE,VALUE)				\
  {									\
    long t[4];								\
    REAL_VALUE_TO_TARGET_LONG_DOUBLE ((VALUE), t);			\
    fprintf (FILE, "\t%s\t0x%lx\n\t%s\t0x%lx\n\t%s\t0x%lx\n\t%s\t0x%lx\n", \
      ASM_LONG, t[0], ASM_LONG, t[1], ASM_LONG, t[2], ASM_LONG, t[3]);	\
  }

/* This is how to output an assembler line defining an `int' constant.  */

#define ASM_OUTPUT_INT(FILE,VALUE)  \
( fprintf (FILE, "\t%s\t", ASM_LONG),		\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* This is how to output an assembler line defining a DImode constant.  */
#define ASM_OUTPUT_DOUBLE_INT(FILE,VALUE)  \
  output_double_int (FILE, VALUE)

/* Likewise for `char' and `short' constants.  */

#define ASM_OUTPUT_SHORT(FILE,VALUE)  \
( fprintf (FILE, "\t%s\t", ASM_SHORT),		\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

#define ASM_OUTPUT_CHAR(FILE,VALUE)  \
( fprintf (FILE, "\t%s\t", ASM_BYTE_OP),	\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* This is how to output an assembler line for a numeric constant byte.  */

#define ASM_OUTPUT_BYTE(FILE,VALUE)  \
  fprintf (FILE, "\t%s\t0x%x\n", ASM_BYTE_OP, (VALUE))

/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
do {									\
  char label[30];							\
  ASM_GENERATE_INTERNAL_LABEL (label, "L", VALUE);			\
  if (Pmode == SImode)							\
    fprintf (FILE, "\t.word\t");					\
  else if (TARGET_ENV32)						\
    fprintf (FILE, "\t.word\t0\n\t.word\t");				\
  else									\
    fprintf (FILE, "\t.xword\t");					\
  assemble_name (FILE, label);						\
  fprintf (FILE, "\n");							\
} while (0)

/* This is how to output an element of a case-vector that is relative.
   (SPARC uses such vectors only when generating PIC.)  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL)			\
do {									\
  char label[30];							\
  ASM_GENERATE_INTERNAL_LABEL (label, "L", VALUE);			\
  if (Pmode == SImode)							\
    fprintf (FILE, "\t.word\t");					\
  else if (TARGET_ENV32)						\
    fprintf (FILE, "\t.word\t0\n\t.word\t");				\
  else									\
    fprintf (FILE, "\t.xword\t");					\
  assemble_name (FILE, label);						\
  fprintf (FILE, "-1b\n");						\
} while (0)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) != 0)			\
    fprintf (FILE, "\t.align %d\n", (1<<(LOG)))

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.skip %u\n", (SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
( fputs ("\t.common ", (FILE)),		\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u,\"bss\"\n", (SIZE)))

/* This says how to output an assembler line to define a local common
   symbol.  */

#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGNED)		\
( fputs ("\t.reserve ", (FILE)),					\
  assemble_name ((FILE), (NAME)),					\
  fprintf ((FILE), ",%u,\"bss\",%u\n",					\
	   (SIZE), ((ALIGNED) / BITS_PER_UNIT)))

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),	\
  sprintf ((OUTPUT), "%s.%d", (NAME), (LABELNO)))

#define IDENT_ASM_OP ".ident"

/* Output #ident as a .ident.  */

#define ASM_OUTPUT_IDENT(FILE, NAME) \
  fprintf (FILE, "\t%s\t\"%s\"\n", IDENT_ASM_OP, NAME);

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

#define PRINT_OPERAND_PUNCT_VALID_P(CHAR) \
  ((CHAR) == '#' || (CHAR) == '*' || (CHAR) == '^' || (CHAR) == '(')

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */

#define PRINT_OPERAND(FILE, X, CODE) print_operand (FILE, X, CODE)

/* Print a memory address as an operand to reference that memory location.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  \
{ register rtx base, index = 0;					\
  int offset = 0;						\
  register rtx addr = ADDR;					\
  if (GET_CODE (addr) == REG)					\
    fputs (reg_names[REGNO (addr)], FILE);			\
  else if (GET_CODE (addr) == PLUS)				\
    {								\
      if (GET_CODE (XEXP (addr, 0)) == CONST_INT)		\
	offset = INTVAL (XEXP (addr, 0)), base = XEXP (addr, 1);\
      else if (GET_CODE (XEXP (addr, 1)) == CONST_INT)		\
	offset = INTVAL (XEXP (addr, 1)), base = XEXP (addr, 0);\
      else							\
	base = XEXP (addr, 0), index = XEXP (addr, 1);		\
      fputs (reg_names[REGNO (base)], FILE);			\
      if (index == 0)						\
	fprintf (FILE, "%+d", offset);				\
      else if (GET_CODE (index) == REG)				\
	fprintf (FILE, "+%s", reg_names[REGNO (index)]);	\
      else if (GET_CODE (index) == SYMBOL_REF			\
	       || GET_CODE (index) == CONST)			\
	fputc ('+', FILE), output_addr_const (FILE, index);	\
      else abort ();						\
    }								\
  else if (GET_CODE (addr) == MINUS				\
	   && GET_CODE (XEXP (addr, 1)) == LABEL_REF)		\
    {								\
      output_addr_const (FILE, XEXP (addr, 0));			\
      fputs ("-(", FILE);					\
      output_addr_const (FILE, XEXP (addr, 1));			\
      fputs ("-.)", FILE);					\
    }								\
  else if (GET_CODE (addr) == LO_SUM)				\
    {								\
      output_operand (XEXP (addr, 0), 0);			\
      fputs ("+%lo(", FILE);					\
      output_address (XEXP (addr, 1));				\
      fputc (')', FILE);					\
    }								\
  else if (flag_pic && GET_CODE (addr) == CONST			\
	   && GET_CODE (XEXP (addr, 0)) == MINUS		\
	   && GET_CODE (XEXP (XEXP (addr, 0), 1)) == CONST	\
	   && GET_CODE (XEXP (XEXP (XEXP (addr, 0), 1), 0)) == MINUS	\
	   && XEXP (XEXP (XEXP (XEXP (addr, 0), 1), 0), 1) == pc_rtx)	\
    {								\
      addr = XEXP (addr, 0);					\
      output_addr_const (FILE, XEXP (addr, 0));			\
      /* Group the args of the second CONST in parenthesis.  */	\
      fputs ("-(", FILE);					\
      /* Skip past the second CONST--it does nothing for us.  */\
      output_addr_const (FILE, XEXP (XEXP (addr, 1), 0));	\
      /* Close the parenthesis.  */				\
      fputc (')', FILE);					\
    }								\
  else								\
    {								\
      output_addr_const (FILE, addr);				\
    }								\
}

/* Declare functions defined in sparc.c and used in templates.  */

extern char *singlemove_string ();
extern char *output_move_double ();
extern char *output_move_quad ();
extern char *output_fp_move_double ();
extern char *output_fp_move_quad ();
extern char *output_block_move ();
extern char *output_scc_insn ();
extern char *output_cbranch ();
extern char *output_v9branch ();
extern char *output_return ();

/* Defined in flags.h, but insn-emit.c does not include flags.h.  */

extern int flag_pic;
