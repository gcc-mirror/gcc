/* Definitions of target machine for GNU compiler, for IBM RS/6000.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
   2000 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@vlsi1.ultra.nyu.edu)

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
   many of the definitions.  */

/* Definitions for the object file format.  These are set at
   compile-time.  */

#define OBJECT_XCOFF 1
#define OBJECT_ELF 2
#define OBJECT_PEF 3

#define TARGET_ELF (TARGET_OBJECT_FORMAT == OBJECT_ELF)
#define TARGET_AIX (TARGET_OBJECT_FORMAT == OBJECT_XCOFF)
#define TARGET_MACOS (TARGET_OBJECT_FORMAT == OBJECT_PEF)

/* Print subsidiary information on the compiler version in use.  */
#define TARGET_VERSION ;

/* Default string to use for cpu if not specified.  */
#ifndef TARGET_CPU_DEFAULT
#define TARGET_CPU_DEFAULT ((char *)0)
#endif

/* Common CPP definitions used by CPP_SPEC among the various targets
   for handling -mcpu=xxx switches.  */
#define CPP_CPU_SPEC \
"%{!mcpu*: \
  %{mpower: %{!mpower2: -D_ARCH_PWR}} \
  %{mpower2: -D_ARCH_PWR2} \
  %{mpowerpc*: -D_ARCH_PPC} \
  %{mno-power: %{!mpowerpc*: -D_ARCH_COM}} \
  %{!mno-power: %{!mpower2: %(cpp_default)}}} \
%{mcpu=common: -D_ARCH_COM} \
%{mcpu=power: -D_ARCH_PWR} \
%{mcpu=power2: -D_ARCH_PWR2} \
%{mcpu=powerpc: -D_ARCH_PPC} \
%{mcpu=rios: -D_ARCH_PWR} \
%{mcpu=rios1: -D_ARCH_PWR} \
%{mcpu=rios2: -D_ARCH_PWR2} \
%{mcpu=rsc: -D_ARCH_PWR} \
%{mcpu=rsc1: -D_ARCH_PWR} \
%{mcpu=401: -D_ARCH_PPC} \
%{mcpu=403: -D_ARCH_PPC} \
%{mcpu=505: -D_ARCH_PPC} \
%{mcpu=601: -D_ARCH_PPC -D_ARCH_PWR} \
%{mcpu=602: -D_ARCH_PPC} \
%{mcpu=603: -D_ARCH_PPC} \
%{mcpu=603e: -D_ARCH_PPC} \
%{mcpu=ec603e: -D_ARCH_PPC} \
%{mcpu=604: -D_ARCH_PPC} \
%{mcpu=604e: -D_ARCH_PPC} \
%{mcpu=620: -D_ARCH_PPC} \
%{mcpu=740: -D_ARCH_PPC} \
%{mcpu=750: -D_ARCH_PPC} \
%{mcpu=801: -D_ARCH_PPC} \
%{mcpu=821: -D_ARCH_PPC} \
%{mcpu=823: -D_ARCH_PPC} \
%{mcpu=860: -D_ARCH_PPC}"

#define CPP_DEFAULT_SPEC "-D_ARCH_PWR"

/* Common ASM definitions used by ASM_SPEC among the various targets
   for handling -mcpu=xxx switches.  */
#define ASM_CPU_SPEC \
"%{!mcpu*: \
  %{mpower: %{!mpower2: -mpwr}} \
  %{mpower2: -mpwrx} \
  %{mpowerpc*: -mppc} \
  %{mno-power: %{!mpowerpc*: -mcom}} \
  %{!mno-power: %{!mpower2: %(asm_default)}}} \
%{mcpu=common: -mcom} \
%{mcpu=power: -mpwr} \
%{mcpu=power2: -mpwrx} \
%{mcpu=powerpc: -mppc} \
%{mcpu=rios: -mpwr} \
%{mcpu=rios1: -mpwr} \
%{mcpu=rios2: -mpwrx} \
%{mcpu=rsc: -mpwr} \
%{mcpu=rsc1: -mpwr} \
%{mcpu=401: -mppc} \
%{mcpu=403: -mppc} \
%{mcpu=505: -mppc} \
%{mcpu=601: -m601} \
%{mcpu=602: -mppc} \
%{mcpu=603: -mppc} \
%{mcpu=603e: -mppc} \
%{mcpu=ec603e: -mppc} \
%{mcpu=604: -mppc} \
%{mcpu=604e: -mppc} \
%{mcpu=620: -mppc} \
%{mcpu=740: -mppc} \
%{mcpu=750: -mppc} \
%{mcpu=801: -mppc} \
%{mcpu=821: -mppc} \
%{mcpu=823: -mppc} \
%{mcpu=860: -mppc}"

#define ASM_DEFAULT_SPEC ""

/* This macro defines names of additional specifications to put in the specs
   that can be used in various specifications like CC1_SPEC.  Its definition
   is an initializer with a subgrouping for each command option.

   Each subgrouping contains a string constant, that defines the
   specification name, and a string constant that used by the GNU CC driver
   program.

   Do not define this macro if it does not need to do anything.  */

#define SUBTARGET_EXTRA_SPECS

#define EXTRA_SPECS							\
  { "cpp_cpu",			CPP_CPU_SPEC },				\
  { "cpp_default",		CPP_DEFAULT_SPEC },			\
  { "asm_cpu",			ASM_CPU_SPEC },				\
  { "asm_default",		ASM_DEFAULT_SPEC },			\
  SUBTARGET_EXTRA_SPECS

/* gcc must do the search itself to find libgcc.a, not use -l.  */
#define LIBGCC_SPEC "libgcc.a%s"

/* Architecture type.  */

extern int target_flags;

/* Use POWER architecture instructions and MQ register.  */
#define MASK_POWER		0x00000001

/* Use POWER2 extensions to POWER architecture.  */
#define MASK_POWER2		0x00000002

/* Use PowerPC architecture instructions.  */
#define MASK_POWERPC		0x00000004

/* Use PowerPC General Purpose group optional instructions, e.g. fsqrt.  */
#define MASK_PPC_GPOPT		0x00000008

/* Use PowerPC Graphics group optional instructions, e.g. fsel.  */
#define MASK_PPC_GFXOPT		0x00000010

/* Use PowerPC-64 architecture instructions.  */
#define MASK_POWERPC64		0x00000020

/* Use revised mnemonic names defined for PowerPC architecture.  */
#define MASK_NEW_MNEMONICS	0x00000040

/* Disable placing fp constants in the TOC; can be turned on when the
   TOC overflows.  */
#define MASK_NO_FP_IN_TOC	0x00000080

/* Disable placing symbol+offset constants in the TOC; can be turned on when
   the TOC overflows.  */
#define MASK_NO_SUM_IN_TOC	0x00000100

/* Output only one TOC entry per module.  Normally linking fails if
   there are more than 16K unique variables/constants in an executable.  With
   this option, linking fails only if there are more than 16K modules, or
   if there are more than 16K unique variables/constant in a single module.

   This is at the cost of having 2 extra loads and one extra store per
   function, and one less allocable register.  */
#define MASK_MINIMAL_TOC	0x00000200

/* Nonzero for the 64bit model: ints, longs, and pointers are 64 bits.  */
#define MASK_64BIT		0x00000400

/* Disable use of FPRs.  */
#define MASK_SOFT_FLOAT		0x00000800

/* Enable load/store multiple, even on powerpc */
#define	MASK_MULTIPLE		0x00001000
#define	MASK_MULTIPLE_SET	0x00002000

/* Use string instructions for block moves */
#define MASK_STRING		0x00004000
#define MASK_STRING_SET		0x00008000

/* Disable update form of load/store */
#define MASK_NO_UPDATE		0x00010000

/* Disable fused multiply/add operations */
#define MASK_NO_FUSED_MADD	0x00020000

/* Nonzero if we need to schedule the prolog and epilog.  */
#define MASK_SCHED_PROLOG	0x00040000

#define TARGET_POWER		(target_flags & MASK_POWER)
#define TARGET_POWER2		(target_flags & MASK_POWER2)
#define TARGET_POWERPC		(target_flags & MASK_POWERPC)
#define TARGET_PPC_GPOPT	(target_flags & MASK_PPC_GPOPT)
#define TARGET_PPC_GFXOPT	(target_flags & MASK_PPC_GFXOPT)
#define TARGET_NEW_MNEMONICS	(target_flags & MASK_NEW_MNEMONICS)
#define TARGET_NO_FP_IN_TOC	(target_flags & MASK_NO_FP_IN_TOC)
#define TARGET_NO_SUM_IN_TOC	(target_flags & MASK_NO_SUM_IN_TOC)
#define TARGET_MINIMAL_TOC	(target_flags & MASK_MINIMAL_TOC)
#define TARGET_64BIT		(target_flags & MASK_64BIT)
#define TARGET_SOFT_FLOAT	(target_flags & MASK_SOFT_FLOAT)
#define	TARGET_MULTIPLE		(target_flags & MASK_MULTIPLE)
#define	TARGET_MULTIPLE_SET	(target_flags & MASK_MULTIPLE_SET)
#define TARGET_STRING		(target_flags & MASK_STRING)
#define TARGET_STRING_SET	(target_flags & MASK_STRING_SET)
#define TARGET_NO_UPDATE	(target_flags & MASK_NO_UPDATE)
#define TARGET_NO_FUSED_MADD	(target_flags & MASK_NO_FUSED_MADD)
#define TARGET_SCHED_PROLOG	(target_flags & MASK_SCHED_PROLOG)

#define TARGET_32BIT		(! TARGET_64BIT)
#define TARGET_HARD_FLOAT	(! TARGET_SOFT_FLOAT)
#define TARGET_UPDATE		(! TARGET_NO_UPDATE)
#define TARGET_FUSED_MADD	(! TARGET_NO_FUSED_MADD)

#ifdef IN_LIBGCC2
/* For libgcc2 we make sure this is a compile time constant */
#ifdef __64BIT__
#define TARGET_POWERPC64	1
#else
#define TARGET_POWERPC64	0
#endif
#else
#define TARGET_POWERPC64	(target_flags & MASK_POWERPC64)
#endif

#define TARGET_XL_CALL 0

/* Run-time compilation parameters selecting different hardware subsets.

   Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES							\
 {{"power",		MASK_POWER  | MASK_MULTIPLE | MASK_STRING,	\
			"Use POWER instruction set"},			\
  {"power2",		(MASK_POWER | MASK_MULTIPLE | MASK_STRING	\
			 | MASK_POWER2),				\
			"Use POWER2 instruction set"},			\
  {"no-power2",		- MASK_POWER2,					\
			"Do not use POWER2 instruction set"},		\
  {"no-power",		- (MASK_POWER | MASK_POWER2 | MASK_MULTIPLE	\
			   | MASK_STRING),				\
			"Do not use POWER instruction set"},		\
  {"powerpc",		MASK_POWERPC,					\
			"Use PowerPC instruction set"},			\
  {"no-powerpc",	- (MASK_POWERPC | MASK_PPC_GPOPT		\
			   | MASK_PPC_GFXOPT | MASK_POWERPC64),		\
			"Do not use PowerPC instruction set"},		\
  {"powerpc-gpopt",	MASK_POWERPC | MASK_PPC_GPOPT,			\
			"Use PowerPC General Purpose group optional instructions"},\
  {"no-powerpc-gpopt",	- MASK_PPC_GPOPT,				\
			"Don't use PowerPC General Purpose group optional instructions"},\
  {"powerpc-gfxopt",	MASK_POWERPC | MASK_PPC_GFXOPT,			\
			"Use PowerPC Graphics group optional instructions"},\
  {"no-powerpc-gfxopt",	- MASK_PPC_GFXOPT,				\
			"Don't use PowerPC Graphics group optional instructions"},\
  {"powerpc64",		MASK_POWERPC64,					\
			"Use PowerPC-64 instruction set"},		\
  {"no-powerpc64",	- MASK_POWERPC64,				\
			"Don't use PowerPC-64 instruction set"},	\
  {"new-mnemonics",	MASK_NEW_MNEMONICS,				\
			"Use new mnemonics for PowerPC architecture"},	\
  {"old-mnemonics",	-MASK_NEW_MNEMONICS,				\
			"Use old mnemonics for PowerPC architecture"},	\
  {"full-toc",		- (MASK_NO_FP_IN_TOC | MASK_NO_SUM_IN_TOC	\
			   | MASK_MINIMAL_TOC),				\
			"Put everything in the regular TOC"},		\
  {"fp-in-toc",		- MASK_NO_FP_IN_TOC,				\
			"Place floating point constants in TOC"},	\
  {"no-fp-in-toc",	MASK_NO_FP_IN_TOC,				\
			"Don't place floating point constants in TOC"},	\
  {"sum-in-toc",	- MASK_NO_SUM_IN_TOC,				\
			"Place symbol+offset constants in TOC"},	\
  {"no-sum-in-toc",	MASK_NO_SUM_IN_TOC,				\
			"Don't place symbol+offset constants in TOC"},	\
  {"minimal-toc",	MASK_MINIMAL_TOC,				\
			"Use only one TOC entry per procedure"},	\
  {"minimal-toc",	- (MASK_NO_FP_IN_TOC | MASK_NO_SUM_IN_TOC),	\
			""},				\
  {"no-minimal-toc",	- MASK_MINIMAL_TOC,				\
			"Place variable addresses in the regular TOC"},	\
  {"hard-float",	- MASK_SOFT_FLOAT,				\
			"Use hardware fp"},				\
  {"soft-float",	MASK_SOFT_FLOAT,				\
			"Do not use hardware fp"},			\
  {"multiple",		MASK_MULTIPLE | MASK_MULTIPLE_SET,		\
			"Generate load/store multiple instructions"},	\
  {"no-multiple",	- MASK_MULTIPLE,				\
			"Do not generate load/store multiple instructions"},\
  {"no-multiple",	MASK_MULTIPLE_SET,				\
			""},\
  {"string",		MASK_STRING | MASK_STRING_SET,			\
			"Generate string instructions for block moves"},\
  {"no-string",		- MASK_STRING,					\
			"Do not generate string instructions for block moves"},\
  {"no-string",		MASK_STRING_SET,				\
			""},\
  {"update",		- MASK_NO_UPDATE,				\
			"Generate load/store with update instructions"},\
  {"no-update",		MASK_NO_UPDATE,					\
			"Do not generate load/store with update instructions"},\
  {"fused-madd",	- MASK_NO_FUSED_MADD,				\
			"Generate fused multiply/add instructions"},	\
  {"no-fused-madd",	MASK_NO_FUSED_MADD,				\
			"Don't generate fused multiply/add instructions"},\
  {"sched-prolog",      MASK_SCHED_PROLOG,                              \
			""},						\
  {"no-sched-prolog",   -MASK_SCHED_PROLOG,                             \
			"Don't schedule the start and end of the procedure"},\
  {"sched-epilog",      MASK_SCHED_PROLOG,                              \
			""},						\
  {"no-sched-epilog",   -MASK_SCHED_PROLOG,                             \
			""},						\
  SUBTARGET_SWITCHES							\
  {"",			TARGET_DEFAULT | MASK_SCHED_PROLOG,		\
			""}}

#define TARGET_DEFAULT (MASK_POWER | MASK_MULTIPLE | MASK_STRING)

/* This is meant to be redefined in the host dependent files */
#define SUBTARGET_SWITCHES

/* Processor type.  Order must match cpu attribute in MD file.  */
enum processor_type
 {
   PROCESSOR_RIOS1,
   PROCESSOR_RIOS2,
   PROCESSOR_RS64A,
   PROCESSOR_MPCCORE,
   PROCESSOR_PPC403,
   PROCESSOR_PPC601,
   PROCESSOR_PPC603,
   PROCESSOR_PPC604,
   PROCESSOR_PPC604e,
   PROCESSOR_PPC620,
   PROCESSOR_PPC630,
   PROCESSOR_PPC750
};

extern enum processor_type rs6000_cpu;

/* Recast the processor type to the cpu attribute.  */
#define rs6000_cpu_attr ((enum attr_cpu)rs6000_cpu)

/* Define generic processor types based upon current deployment.  */
#define PROCESSOR_COMMON    PROCESSOR_PPC601
#define PROCESSOR_POWER     PROCESSOR_RIOS1
#define PROCESSOR_POWERPC   PROCESSOR_PPC604
#define PROCESSOR_POWERPC64 PROCESSOR_RS64A

/* Define the default processor.  This is overridden by other tm.h files.  */
#define PROCESSOR_DEFAULT   PROCESSOR_RIOS1
#define PROCESSOR_DEFAULT64 PROCESSOR_RS64A

/* Specify the dialect of assembler to use.  New mnemonics is dialect one
   and the old mnemonics are dialect zero.  */
#define ASSEMBLER_DIALECT (TARGET_NEW_MNEMONICS ? 1 : 0)

/* This macro is similar to `TARGET_SWITCHES' but defines names of
   command options that have values.  Its definition is an
   initializer with a subgrouping for each command option.

   Each subgrouping contains a string constant, that defines the
   fixed part of the option name, and the address of a variable.
   The variable, type `char *', is set to the variable part of the
   given option if the fixed part matches.  The actual option name
   is made by appending `-m' to the specified name.

   Here is an example which defines `-mshort-data-NUMBER'.  If the
   given option is `-mshort-data-512', the variable `m88k_short_data'
   will be set to the string `"512"'.

	extern char *m88k_short_data;
	#define TARGET_OPTIONS { { "short-data-", &m88k_short_data } }  */

/* This is meant to be overridden in target specific files.  */
#define	SUBTARGET_OPTIONS

#define TARGET_OPTIONS							\
{									\
   {"cpu=",  &rs6000_select[1].string, "Use features of and schedule code for given CPU" },\
   {"tune=", &rs6000_select[2].string, "Schedule code for given CPU" },	\
   {"debug=", &rs6000_debug_name, "Enable debug output" },		\
   SUBTARGET_OPTIONS							\
}

/* rs6000_select[0] is reserved for the default cpu defined via --with-cpu */
struct rs6000_cpu_select
{
  const char *string;
  const char *name;
  int set_tune_p;
  int set_arch_p;
};

extern struct rs6000_cpu_select rs6000_select[];

/* Debug support */
extern const char *rs6000_debug_name;		/* Name for -mdebug-xxxx option */
extern int rs6000_debug_stack;		/* debug stack applications */
extern int rs6000_debug_arg;		/* debug argument handling */

#define	TARGET_DEBUG_STACK	rs6000_debug_stack
#define	TARGET_DEBUG_ARG	rs6000_debug_arg

/* Sometimes certain combinations of command options do not make sense
   on a particular target machine.  You can define a macro
   `OVERRIDE_OPTIONS' to take account of this.  This macro, if
   defined, is executed once just after all the command options have
   been parsed.

   Don't use this macro to turn on various extra optimizations for
   `-O'.  That is what `OPTIMIZATION_OPTIONS' is for.

   On the RS/6000 this is used to define the target cpu type.  */

#define OVERRIDE_OPTIONS rs6000_override_options (TARGET_CPU_DEFAULT)

/* Define this to change the optimizations performed by default.  */
#define OPTIMIZATION_OPTIONS(LEVEL,SIZE) optimization_options(LEVEL,SIZE)

/* Show we can debug even without a frame pointer.  */
#define CAN_DEBUG_WITHOUT_FP

/* target machine storage layout */

/* Define to support cross compilation to an RS6000 target.  */
#define REAL_ARITHMETIC

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases,
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.  */

#define PROMOTE_MODE(MODE,UNSIGNEDP,TYPE)	\
  if (GET_MODE_CLASS (MODE) == MODE_INT		\
      && GET_MODE_SIZE (MODE) < UNITS_PER_WORD) \
    (MODE) = word_mode;

/* Define this if function arguments should also be promoted using the above
   procedure.  */

#define PROMOTE_FUNCTION_ARGS

/* Likewise, if the function return value is promoted.  */

#define PROMOTE_FUNCTION_RETURN

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields. */
/* That is true on RS/6000. */
#define BITS_BIG_ENDIAN 1

/* Define this if most significant byte of a word is the lowest numbered.  */
/* That is true on RS/6000.  */
#define BYTES_BIG_ENDIAN 1

/* Define this if most significant word of a multiword number is lowest
   numbered.

   For RS/6000 we can decide arbitrarily since there are no machine
   instructions for them.  Might as well be consistent with bits and bytes. */
#define WORDS_BIG_ENDIAN 1

/* number of bits in an addressable storage unit */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD (! TARGET_POWERPC64 ? 32 : 64)
#define MAX_BITS_PER_WORD 64

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD (! TARGET_POWERPC64 ? 4 : 8)
#define MIN_UNITS_PER_WORD 4
#define UNITS_PER_FP_WORD 8

/* Type used for ptrdiff_t, as a string used in a declaration.  */
#define PTRDIFF_TYPE "int"

/* Type used for wchar_t, as a string used in a declaration.  */
#define WCHAR_TYPE "short unsigned int"

/* Width of wchar_t in bits.  */
#define WCHAR_TYPE_SIZE 16

/* A C expression for the size in bits of the type `short' on the
   target machine.  If you don't define this, the default is half a
   word.  (If this would be less than one storage unit, it is
   rounded up to one unit.)  */
#define SHORT_TYPE_SIZE 16

/* A C expression for the size in bits of the type `int' on the
   target machine.  If you don't define this, the default is one
   word.  */
#define INT_TYPE_SIZE 32

/* A C expression for the size in bits of the type `long' on the
   target machine.  If you don't define this, the default is one
   word.  */
#define LONG_TYPE_SIZE (TARGET_32BIT ? 32 : 64)
#define MAX_LONG_TYPE_SIZE 64

/* A C expression for the size in bits of the type `long long' on the
   target machine.  If you don't define this, the default is two
   words.  */
#define LONG_LONG_TYPE_SIZE 64

/* A C expression for the size in bits of the type `char' on the
   target machine.  If you don't define this, the default is one
   quarter of a word.  (If this would be less than one storage unit,
   it is rounded up to one unit.)  */
#define CHAR_TYPE_SIZE BITS_PER_UNIT

/* A C expression for the size in bits of the type `float' on the
   target machine.  If you don't define this, the default is one
   word.  */
#define FLOAT_TYPE_SIZE 32

/* A C expression for the size in bits of the type `double' on the
   target machine.  If you don't define this, the default is two
   words.  */
#define DOUBLE_TYPE_SIZE 64

/* A C expression for the size in bits of the type `long double' on
   the target machine.  If you don't define this, the default is two
   words.  */
#define LONG_DOUBLE_TYPE_SIZE 64

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE (TARGET_32BIT ? 32 : 64)

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY (TARGET_32BIT ? 32 : 64)

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY (TARGET_32BIT ? 64 : 128)

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 32

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 64

/* Handle #pragma pack.  */
#define HANDLE_PRAGMA_PACK 1

/* AIX word-aligns FP doubles but doubleword-aligns 64-bit ints.  */
#define ADJUST_FIELD_ALIGN(FIELD, COMPUTED) \
  (TYPE_MODE (TREE_CODE (TREE_TYPE (FIELD)) == ARRAY_TYPE \
	      ? get_inner_array_type (FIELD) \
	      : TREE_TYPE (FIELD)) == DFmode \
   ? MIN ((COMPUTED), 32) : (COMPUTED))

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* A bitfield declared as `int' forces `int' alignment for the struct.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* AIX increases natural record alignment to doubleword if the first
   field is an FP double while the FP fields remain word aligned.  */
#define ROUND_TYPE_ALIGN(STRUCT, COMPUTED, SPECIFIED)	\
  ((TREE_CODE (STRUCT) == RECORD_TYPE			\
    || TREE_CODE (STRUCT) == UNION_TYPE			\
    || TREE_CODE (STRUCT) == QUAL_UNION_TYPE)		\
   && TYPE_FIELDS (STRUCT) != 0				\
   && DECL_MODE (TYPE_FIELDS (STRUCT)) == DFmode	\
   ? MAX (MAX ((COMPUTED), (SPECIFIED)), BIGGEST_ALIGNMENT) \
   : MAX ((COMPUTED), (SPECIFIED)))

/* Make strings word-aligned so strcpy from constants will be faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  (TREE_CODE (EXP) == STRING_CST	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

/* Non-zero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 0

/* Define this macro to be the value 1 if unaligned accesses have a cost
   many times greater than aligned accesses, for example if they are
   emulated in a trap handler.  */
#define SLOW_UNALIGNED_ACCESS(MODE, ALIGN)			\
   ((STRICT_ALIGNMENT						\
     || (((MODE) == SFmode || (MODE) == DFmode || (MODE) == DImode) \
         && (ALIGN) < 32)) ? 1 : 0)

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   RS/6000 has 32 fixed-point registers, 32 floating-point registers,
   an MQ register, a count register, a link register, and 8 condition
   register fields, which we view here as separate registers.

   In addition, the difference between the frame and argument pointers is
   a function of the number of registers saved, so we need to have a
   register for AP that will later be eliminated in favor of SP or FP.
   This is a normal register, but it is fixed.

   We also create a pseudo register for float/int conversions, that will
   really represent the memory location used.  It is represented here as
   a register, in order to work around problems in allocating stack storage
   in inline functions.  */

#define FIRST_PSEUDO_REGISTER 77

/* This must not decrease, for backwards compatibility.  If
   FIRST_PSEUDO_REGISTER increases, this should as well.  */
#define DWARF_FRAME_REGISTERS 77

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.

   On RS/6000, r1 is used for the stack and r2 is used as the TOC pointer.

   cr5 is not supposed to be used.

   On System V implementations, r13 is fixed and not available for use.  */

#define FIXED_REGISTERS  \
  {0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, FIXED_R13, 0, 0, \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */

#define CALL_USED_REGISTERS  \
  {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, FIXED_R13, 0, 0, \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1}

#define MQ_REGNO     64
#define CR0_REGNO    68
#define CR1_REGNO    69
#define CR2_REGNO    70
#define CR3_REGNO    71
#define CR4_REGNO    72
#define MAX_CR_REGNO 75
#define XER_REGNO    76

/* List the order in which to allocate registers.  Each register must be
   listed once, even those in FIXED_REGISTERS.

   We allocate in the following order:
	fp0		(not saved or used for anything)
	fp13 - fp2	(not saved; incoming fp arg registers)
	fp1		(not saved; return value)
 	fp31 - fp14	(saved; order given to save least number)
	cr7, cr6	(not saved or special)
	cr1		(not saved, but used for FP operations)
	cr0		(not saved, but used for arithmetic operations)
	cr4, cr3, cr2	(saved)
        r0		(not saved; cannot be base reg)
	r9		(not saved; best for TImode)
	r11, r10, r8-r4	(not saved; highest used first to make less conflict)
	r3     		(not saved; return value register)
	r31 - r13	(saved; order given to save least number)
	r12		(not saved; if used for DImode or DFmode would use r13)
	mq		(not saved; best to use it if we can)
	ctr		(not saved; when we have the choice ctr is better)
	lr		(saved)
        cr5, r1, r2, ap, xer (fixed)  */

#define REG_ALLOC_ORDER					\
  {32, 							\
   45, 44, 43, 42, 41, 40, 39, 38, 37, 36, 35, 34,	\
   33,							\
   63, 62, 61, 60, 59, 58, 57, 56, 55, 54, 53, 52, 51,	\
   50, 49, 48, 47, 46, 					\
   75, 74, 69, 68, 72, 71, 70,				\
   0,							\
   9, 11, 10, 8, 7, 6, 5, 4,				\
   3,							\
   31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19,	\
   18, 17, 16, 15, 14, 13, 12,				\
   64, 66, 65, 						\
   73, 1, 2, 67, 76}

/* True if register is floating-point.  */
#define FP_REGNO_P(N) ((N) >= 32 && (N) <= 63)

/* True if register is a condition register.  */
#define CR_REGNO_P(N) ((N) >= 68 && (N) <= 75)

/* True if register is a condition register, but not cr0.  */
#define CR_REGNO_NOT_CR0_P(N) ((N) >= 69 && (N) <= 75)

/* True if register is an integer register.  */
#define INT_REGNO_P(N) ((N) <= 31 || (N) == ARG_POINTER_REGNUM)

/* True if register is the temporary memory location used for int/float
   conversion.  */
#define XER_REGNO_P(N) ((N) == XER_REGNO)

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   POWER and PowerPC GPRs hold 32 bits worth;
   PowerPC64 GPRs and FPRs point register holds 64 bits worth.  */

#define HARD_REGNO_NREGS(REGNO, MODE)					\
  (FP_REGNO_P (REGNO)							\
   ? ((GET_MODE_SIZE (MODE) + UNITS_PER_FP_WORD - 1) / UNITS_PER_FP_WORD) \
   : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   For POWER and PowerPC, the GPRs can hold any mode, but the float
   registers only can hold floating modes and DImode, and CR register only
   can hold CC modes.  We cannot put TImode anywhere except general
   register and it must be able to fit within the register set. */

#define HARD_REGNO_MODE_OK(REGNO, MODE)					\
  (FP_REGNO_P (REGNO) ?							\
   (GET_MODE_CLASS (MODE) == MODE_FLOAT					\
    || (GET_MODE_CLASS (MODE) == MODE_INT				\
	&& GET_MODE_SIZE (MODE) == UNITS_PER_FP_WORD))			\
   : CR_REGNO_P (REGNO) ? GET_MODE_CLASS (MODE) == MODE_CC		\
   : XER_REGNO_P (REGNO) ? (MODE) == PSImode				\
   : ! INT_REGNO_P (REGNO) ? (GET_MODE_CLASS (MODE) == MODE_INT		\
			      && GET_MODE_SIZE (MODE) <= UNITS_PER_WORD) \
   : 1)

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2) \
  (GET_MODE_CLASS (MODE1) == MODE_FLOAT		\
   ? GET_MODE_CLASS (MODE2) == MODE_FLOAT	\
   : GET_MODE_CLASS (MODE2) == MODE_FLOAT	\
   ? GET_MODE_CLASS (MODE1) == MODE_FLOAT	\
   : GET_MODE_CLASS (MODE1) == MODE_CC		\
   ? GET_MODE_CLASS (MODE2) == MODE_CC		\
   : GET_MODE_CLASS (MODE2) == MODE_CC		\
   ? GET_MODE_CLASS (MODE1) == MODE_CC		\
   : 1)

/* A C expression returning the cost of moving data from a register of class
   CLASS1 to one of CLASS2.

   On the RS/6000, copying between floating-point and fixed-point
   registers is expensive.  */

#define REGISTER_MOVE_COST(CLASS1, CLASS2)			\
  ((CLASS1) == FLOAT_REGS && (CLASS2) == FLOAT_REGS ? 2		\
   : (CLASS1) == FLOAT_REGS && (CLASS2) != FLOAT_REGS ? 10	\
   : (CLASS1) != FLOAT_REGS && (CLASS2) == FLOAT_REGS ? 10	\
   : (((CLASS1) == SPECIAL_REGS || (CLASS1) == MQ_REGS		\
       || (CLASS1) == LINK_REGS || (CLASS1) == CTR_REGS		\
       || (CLASS1) == LINK_OR_CTR_REGS)				\
      && ((CLASS2) == SPECIAL_REGS || (CLASS2) == MQ_REGS	\
	  || (CLASS2) == LINK_REGS || (CLASS2) == CTR_REGS	\
	  || (CLASS2) == LINK_OR_CTR_REGS)) ? 10		\
   : 2)

/* A C expressions returning the cost of moving data of MODE from a register to
   or from memory.

   On the RS/6000, bump this up a bit.  */

#define MEMORY_MOVE_COST(MODE, CLASS, IN)	\
  ((GET_MODE_CLASS (MODE) == MODE_FLOAT		\
    && (rs6000_cpu == PROCESSOR_RIOS1 || rs6000_cpu == PROCESSOR_PPC601) \
    ? 3 : 2) \
   + 4)

/* Specify the cost of a branch insn; roughly the number of extra insns that
   should be added to avoid a branch.

   Set this to 3 on the RS/6000 since that is roughly the average cost of an
   unscheduled conditional branch.  */

#define BRANCH_COST 3

/* A C statement (sans semicolon) to update the integer variable COST
   based on the relationship between INSN that is dependent on
   DEP_INSN through the dependence LINK.  The default is to make no
   adjustment to COST.  On the RS/6000, ignore the cost of anti- and
   output-dependencies.  In fact, output dependencies on the CR do have
   a cost, but it is probably not worthwhile to track it.  */

#define ADJUST_COST(INSN, LINK, DEP_INSN, COST)				\
  (COST) = rs6000_adjust_cost (INSN,LINK,DEP_INSN,COST)

/* A C statement (sans semicolon) to update the integer scheduling priority
   INSN_PRIORITY (INSN).  Reduce the priority to execute the INSN earlier,
   increase the priority to execute INSN later.  Do not define this macro if
   you do not need to adjust the scheduling priorities of insns.  */

#define ADJUST_PRIORITY(INSN)						\
  INSN_PRIORITY (INSN) = rs6000_adjust_priority (INSN, INSN_PRIORITY (INSN))

/* Define this macro to change register usage conditional on target flags.
   Set MQ register fixed (already call_used) if not POWER architecture
   (RIOS1, RIOS2, RSC, and PPC601) so that it will not be allocated.
   64-bit AIX reserves GPR13 for thread-private data.
   Conditionally disable FPRs.  */

#define CONDITIONAL_REGISTER_USAGE					\
{									\
  if (! TARGET_POWER)							\
    fixed_regs[64] = 1;							\
  if (TARGET_64BIT)							\
    fixed_regs[13] = call_used_regs[13] = 1; 				\
  if (TARGET_SOFT_FLOAT)						\
    for (i = 32; i < 64; i++)						\
      fixed_regs[i] = call_used_regs[i] = 1; 				\
  if ((DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_SOLARIS)		\
      && flag_pic == 1)							\
    fixed_regs[PIC_OFFSET_TABLE_REGNUM]					\
      = call_used_regs[PIC_OFFSET_TABLE_REGNUM] = 1;			\
}

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* RS/6000 pc isn't overloaded on a register that the compiler knows about.  */
/* #define PC_REGNUM  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 1

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 31

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
#define FRAME_POINTER_REQUIRED 0

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 67

/* Place to put static chain when calling a function that requires it.  */
#define STATIC_CHAIN_REGNUM 11

/* Link register number. */
#define LINK_REGISTER_REGNUM 65

/* Count register number. */
#define COUNT_REGISTER_REGNUM 66

/* Place that structure value return address is placed.

   On the RS/6000, it is passed as an extra parameter.  */
#define STRUCT_VALUE 0

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

/* The RS/6000 has three types of registers, fixed-point, floating-point,
   and condition registers, plus three special registers, MQ, CTR, and the
   link register.

   However, r0 is special in that it cannot be used as a base register.
   So make a class for registers valid as base registers.

   Also, cr0 is the only condition code register that can be used in
   arithmetic insns, so make a separate class for it.

   There is a special 'register' (76), which is not a register, but a
   placeholder for memory allocated to convert between floating point and
   integral types.  This works around a problem where if we allocate memory
   with allocate_stack_{local,temp} and the function is an inline function, the
   memory allocated will clobber memory in the caller.  So we use a special
   register, and if that is used, we allocate stack space for it.  */

enum reg_class
{
  NO_REGS,
  BASE_REGS,
  GENERAL_REGS,
  FLOAT_REGS,
  NON_SPECIAL_REGS,
  MQ_REGS,
  LINK_REGS,
  CTR_REGS,
  LINK_OR_CTR_REGS,
  SPECIAL_REGS,
  SPEC_OR_GEN_REGS,
  CR0_REGS,
  CR_REGS,
  NON_FLOAT_REGS,
  XER_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES							\
{									\
  "NO_REGS",								\
  "BASE_REGS",								\
  "GENERAL_REGS",							\
  "FLOAT_REGS",								\
  "NON_SPECIAL_REGS",							\
  "MQ_REGS",								\
  "LINK_REGS",								\
  "CTR_REGS",								\
  "LINK_OR_CTR_REGS",							\
  "SPECIAL_REGS",							\
  "SPEC_OR_GEN_REGS",							\
  "CR0_REGS",								\
  "CR_REGS",								\
  "NON_FLOAT_REGS",							\
  "XER_REGS",								\
  "ALL_REGS"								\
}

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS						\
{									\
  { 0x00000000, 0x00000000, 0x00000000 },	/* NO_REGS */		\
  { 0xfffffffe, 0x00000000, 0x00000008 },	/* BASE_REGS */		\
  { 0xffffffff, 0x00000000, 0x00000008 },	/* GENERAL_REGS */	\
  { 0x00000000, 0xffffffff, 0x00000000 },	/* FLOAT_REGS */	\
  { 0xffffffff, 0xffffffff, 0x00000008 },	/* NON_SPECIAL_REGS */	\
  { 0x00000000, 0x00000000, 0x00000001 },	/* MQ_REGS */		\
  { 0x00000000, 0x00000000, 0x00000002 },	/* LINK_REGS */		\
  { 0x00000000, 0x00000000, 0x00000004 },	/* CTR_REGS */		\
  { 0x00000000, 0x00000000, 0x00000006 },	/* LINK_OR_CTR_REGS */	\
  { 0x00000000, 0x00000000, 0x00000007 },	/* SPECIAL_REGS */	\
  { 0xffffffff, 0x00000000, 0x0000000f },	/* SPEC_OR_GEN_REGS */	\
  { 0x00000000, 0x00000000, 0x00000010 },	/* CR0_REGS */		\
  { 0x00000000, 0x00000000, 0x00000ff0 },	/* CR_REGS */		\
  { 0xffffffff, 0x00000000, 0x0000ffff },	/* NON_FLOAT_REGS */	\
  { 0x00000000, 0x00000000, 0x00010000 },	/* XER_REGS */		\
  { 0xffffffff, 0xffffffff, 0x0001ffff }	/* ALL_REGS */		\
}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO)		\
 ((REGNO) == 0 ? GENERAL_REGS		\
  : (REGNO) < 32 ? BASE_REGS		\
  : FP_REGNO_P (REGNO) ? FLOAT_REGS	\
  : (REGNO) == 68 ? CR0_REGS		\
  : CR_REGNO_P (REGNO) ? CR_REGS	\
  : (REGNO) == 64 ? MQ_REGS		\
  : (REGNO) == 65 ? LINK_REGS		\
  : (REGNO) == 66 ? CTR_REGS		\
  : (REGNO) == 67 ? BASE_REGS		\
  : (REGNO) == 76 ? XER_REGS		\
  : NO_REGS)

/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS GENERAL_REGS
#define BASE_REG_CLASS BASE_REGS

/* Get reg_class from a letter such as appears in the machine description.  */

#define REG_CLASS_FROM_LETTER(C) \
  ((C) == 'f' ? FLOAT_REGS	\
   : (C) == 'b' ? BASE_REGS	\
   : (C) == 'h' ? SPECIAL_REGS	\
   : (C) == 'q' ? MQ_REGS	\
   : (C) == 'c' ? CTR_REGS	\
   : (C) == 'l' ? LINK_REGS	\
   : (C) == 'x' ? CR0_REGS	\
   : (C) == 'y' ? CR_REGS	\
   : (C) == 'z' ? XER_REGS	\
   : NO_REGS)

/* The letters I, J, K, L, M, N, and P in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.

   `I' is a signed 16-bit constant
   `J' is a constant with only the high-order 16 bits non-zero
   `K' is a constant with only the low-order 16 bits non-zero
   `L' is a signed 16-bit constant shifted left 16 bits
   `M' is a constant that is greater than 31
   `N' is a constant that is an exact power of two
   `O' is the constant zero
   `P' is a constant whose negation is a signed 16-bit constant */

#define CONST_OK_FOR_LETTER_P(VALUE, C)					\
   ( (C) == 'I' ? (unsigned HOST_WIDE_INT) ((VALUE) + 0x8000) < 0x10000	\
   : (C) == 'J' ? ((VALUE) & (~ (HOST_WIDE_INT) 0xffff0000u)) == 0	\
   : (C) == 'K' ? ((VALUE) & (~ (HOST_WIDE_INT) 0xffff)) == 0		\
   : (C) == 'L' ? (((VALUE) & 0xffff) == 0				\
		   && ((VALUE) >> 31 == -1 || (VALUE) >> 31 == 0))	\
   : (C) == 'M' ? (VALUE) > 31						\
   : (C) == 'N' ? exact_log2 (VALUE) >= 0				\
   : (C) == 'O' ? (VALUE) == 0						\
   : (C) == 'P' ? (unsigned HOST_WIDE_INT) ((- (VALUE)) + 0x8000) < 0x10000 \
   : 0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.

   We flag for special constants when we can copy the constant into
   a general register in two insns for DF/DI and one insn for SF.

   'H' is used for DI/DF constants that take 3 insns.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)				\
  (  (C) == 'G' ? (num_insns_constant (VALUE, GET_MODE (VALUE))		\
		   == ((GET_MODE (VALUE) == SFmode) ? 1 : 2))		\
   : (C) == 'H' ? (num_insns_constant (VALUE, GET_MODE (VALUE)) == 3)	\
   : 0)

/* Optional extra constraints for this machine.

   'Q' means that is a memory operand that is just an offset from a reg.
   'R' is for AIX TOC entries.
   'S' is a constant that can be placed into a 64-bit mask operand
   'T' is a consatnt that can be placed into a 32-bit mask operand
   'U' is for V.4 small data references.  */

#define EXTRA_CONSTRAINT(OP, C)						\
  ((C) == 'Q' ? GET_CODE (OP) == MEM && GET_CODE (XEXP (OP, 0)) == REG	\
   : (C) == 'R' ? LEGITIMATE_CONSTANT_POOL_ADDRESS_P (OP)		\
   : (C) == 'S' ? mask64_operand (OP, VOIDmode)				\
   : (C) == 'T' ? mask_operand (OP, VOIDmode)				\
   : (C) == 'U' ? ((DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_SOLARIS) \
		   && small_data_operand (OP, GET_MODE (OP)))		\
   : 0)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.

   On the RS/6000, we have to return NO_REGS when we want to reload a
   floating-point CONST_DOUBLE to force it to be copied to memory.  */

#define PREFERRED_RELOAD_CLASS(X,CLASS)			\
  ((GET_CODE (X) == CONST_DOUBLE			\
    && GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT)	\
   ? NO_REGS : (CLASS))

/* Return the register class of a scratch register needed to copy IN into
   or out of a register in CLASS in MODE.  If it can be done directly,
   NO_REGS is returned.  */

#define SECONDARY_RELOAD_CLASS(CLASS,MODE,IN) \
  secondary_reload_class (CLASS, MODE, IN)

/* If we are copying between FP registers and anything else, we need a memory
   location.  */

#define SECONDARY_MEMORY_NEEDED(CLASS1,CLASS2,MODE) \
 ((CLASS1) != (CLASS2) && ((CLASS1) == FLOAT_REGS || (CLASS2) == FLOAT_REGS))

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.

   On RS/6000, this is the size of MODE in words,
   except in the FP regs, where a single reg is enough for two words.  */
#define CLASS_MAX_NREGS(CLASS, MODE)					\
 (((CLASS) == FLOAT_REGS) 						\
  ? ((GET_MODE_SIZE (MODE) + UNITS_PER_FP_WORD - 1) / UNITS_PER_FP_WORD) \
  : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* If defined, gives a class of registers that cannot be used as the
   operand of a SUBREG that changes the size of the object.  */

#define CLASS_CANNOT_CHANGE_SIZE      FLOAT_REGS

/* Stack layout; function entry, exit and calling.  */

/* Enumeration to give which calling sequence to use.  */
enum rs6000_abi {
  ABI_NONE,
  ABI_AIX,			/* IBM's AIX */
  ABI_AIX_NODESC,		/* AIX calling sequence minus function descriptors */
  ABI_V4,			/* System V.4/eabi */
  ABI_SOLARIS			/* Solaris */
};

extern enum rs6000_abi rs6000_current_abi;	/* available for use by subtarget */

/* Structure used to define the rs6000 stack */
typedef struct rs6000_stack {
  int first_gp_reg_save;	/* first callee saved GP register used */
  int first_fp_reg_save;	/* first callee saved FP register used */
  int lr_save_p;		/* true if the link reg needs to be saved */
  int cr_save_p;		/* true if the CR reg needs to be saved */
  int toc_save_p;		/* true if the TOC needs to be saved */
  int push_p;			/* true if we need to allocate stack space */
  int calls_p;			/* true if the function makes any calls */
  enum rs6000_abi abi;		/* which ABI to use */
  int gp_save_offset;		/* offset to save GP regs from initial SP */
  int fp_save_offset;		/* offset to save FP regs from initial SP */
  int lr_save_offset;		/* offset to save LR from initial SP */
  int cr_save_offset;		/* offset to save CR from initial SP */
  int toc_save_offset;		/* offset to save the TOC pointer */
  int varargs_save_offset;	/* offset to save the varargs registers */
  int reg_size;			/* register size (4 or 8) */
  int varargs_size;		/* size to hold V.4 args passed in regs */
  int vars_size;		/* variable save area size */
  int parm_size;		/* outgoing parameter size */
  int save_size;		/* save area size */
  int fixed_size;		/* fixed size of stack frame */
  int gp_size;			/* size of saved GP registers */
  int fp_size;			/* size of saved FP registers */
  int cr_size;			/* size to hold CR if not in save_size */
  int lr_size;			/* size to hold LR if not in save_size */
  int toc_size;			/* size to hold TOC if not in save_size */
  int total_size;		/* total bytes allocated for stack */
} rs6000_stack_t;

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD

/* Define this if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.

   On the RS/6000, we grow upwards, from the area after the outgoing
   arguments.  */
/* #define FRAME_GROWS_DOWNWARD */

/* Size of the outgoing register save area */
#define RS6000_REG_SAVE ((DEFAULT_ABI == ABI_AIX			\
			  || DEFAULT_ABI == ABI_AIX_NODESC)		\
			 ? (TARGET_64BIT ? 64 : 32)			\
			 : 0)

/* Size of the fixed area on the stack */
#define RS6000_SAVE_AREA \
  (((DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_AIX_NODESC) ? 24 : 8)	\
   << (TARGET_64BIT ? 1 : 0))

/* MEM representing address to save the TOC register */
#define RS6000_SAVE_TOC gen_rtx_MEM (Pmode, \
				     plus_constant (stack_pointer_rtx, \
						    (TARGET_32BIT ? 20 : 40)))

/* Size of the V.4 varargs area if needed */
#define RS6000_VARARGS_AREA 0

/* Whether a V.4 varargs area is needed */
extern int rs6000_sysv_varargs_p;

/* Align an address */
#define RS6000_ALIGN(n,a) (((n) + (a) - 1) & ~((a) - 1))

/* Initialize data used by insn expanders.  This is called from
   init_emit, once for each function, before code is generated. */
#define INIT_EXPANDERS rs6000_init_expanders ()

/* Size of V.4 varargs area in bytes */
#define RS6000_VARARGS_SIZE \
  ((GP_ARG_NUM_REG * (TARGET_32BIT ? 4 : 8)) + (FP_ARG_NUM_REG * 8) + 8)

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.

   On the RS/6000, the frame pointer is the same as the stack pointer,
   except for dynamic allocations.  So we start after the fixed area and
   outgoing parameter area.  */

#define STARTING_FRAME_OFFSET						\
  (RS6000_ALIGN (current_function_outgoing_args_size, 8)		\
   + RS6000_VARARGS_AREA						\
   + RS6000_SAVE_AREA)

/* Offset from the stack pointer register to an item dynamically
   allocated on the stack, e.g., by `alloca'.

   The default value for this macro is `STACK_POINTER_OFFSET' plus the
   length of the outgoing arguments.  The default is correct for most
   machines.  See `function.c' for details.  */
#define STACK_DYNAMIC_OFFSET(FUNDECL)					\
  (RS6000_ALIGN (current_function_outgoing_args_size, 8)		\
   + (STACK_POINTER_OFFSET))

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.
   On RS/6000, don't define this because there are no push insns.  */
/*  #define PUSH_ROUNDING(BYTES) */

/* Offset of first parameter from the argument pointer register value.
   On the RS/6000, we define the argument pointer to the start of the fixed
   area.  */
#define FIRST_PARM_OFFSET(FNDECL) RS6000_SAVE_AREA

/* Offset from the argument pointer register value to the top of
   stack.  This is different from FIRST_PARM_OFFSET because of the
   register save area.  */
#define ARG_POINTER_CFA_OFFSET(FNDECL) 0

/* Define this if stack space is still allocated for a parameter passed
   in a register.  The value is the number of bytes allocated to this
   area.  */
#define REG_PARM_STACK_SPACE(FNDECL)	RS6000_REG_SAVE

/* Define this if the above stack space is to be considered part of the
   space allocated by the caller.  */
#define OUTGOING_REG_PARM_STACK_SPACE

/* This is the difference between the logical top of stack and the actual sp.

   For the RS/6000, sp points past the fixed area. */
#define STACK_POINTER_OFFSET RS6000_SAVE_AREA

/* Define this if the maximum size of all the outgoing args is to be
   accumulated and pushed during the prologue.  The amount can be
   found in the variable current_function_outgoing_args_size.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* Value is the number of bytes of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.  */

#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE) 0

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.

   On RS/6000 an integer value is in r3 and a floating-point value is in
   fp1, unless -msoft-float.  */

#define FUNCTION_VALUE(VALTYPE, FUNC)				\
  gen_rtx_REG ((INTEGRAL_TYPE_P (VALTYPE)			\
		&& TYPE_PRECISION (VALTYPE) < BITS_PER_WORD)	\
	       || POINTER_TYPE_P (VALTYPE)			\
	       ? word_mode : TYPE_MODE (VALTYPE),		\
	       TREE_CODE (VALTYPE) == REAL_TYPE && TARGET_HARD_FLOAT ? 33 : 3)

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

#define LIBCALL_VALUE(MODE)		\
  gen_rtx_REG (MODE, (GET_MODE_CLASS (MODE) == MODE_FLOAT	 \
		      && TARGET_HARD_FLOAT ? 33 : 3))

/* The definition of this macro implies that there are cases where
   a scalar value cannot be returned in registers.

   For the RS/6000, any structure or union type is returned in memory, except for
   Solaris, which returns structures <= 8 bytes in registers.  */

#define RETURN_IN_MEMORY(TYPE)						\
  (TYPE_MODE (TYPE) == BLKmode						\
   && (DEFAULT_ABI != ABI_SOLARIS || int_size_in_bytes (TYPE) > 8))

/* Mode of stack savearea.
   FUNCTION is VOIDmode because calling convention maintains SP.
   BLOCK needs Pmode for SP.
   NONLOCAL needs twice Pmode to maintain both backchain and SP.  */
#define STACK_SAVEAREA_MODE(LEVEL)	\
  (LEVEL == SAVE_FUNCTION ? VOIDmode	\
  : LEVEL == SAVE_NONLOCAL ? (TARGET_32BIT ? DImode : TImode) : Pmode)

/* Minimum and maximum general purpose registers used to hold arguments.  */
#define GP_ARG_MIN_REG 3
#define GP_ARG_MAX_REG 10
#define GP_ARG_NUM_REG (GP_ARG_MAX_REG - GP_ARG_MIN_REG + 1)

/* Minimum and maximum floating point registers used to hold arguments.  */
#define FP_ARG_MIN_REG 33
#define	FP_ARG_AIX_MAX_REG 45
#define	FP_ARG_V4_MAX_REG  40
#define	FP_ARG_MAX_REG ((DEFAULT_ABI == ABI_AIX				\
			 || DEFAULT_ABI == ABI_AIX_NODESC)		\
			? FP_ARG_AIX_MAX_REG : FP_ARG_V4_MAX_REG)
#define FP_ARG_NUM_REG (FP_ARG_MAX_REG - FP_ARG_MIN_REG + 1)

/* Return registers */
#define GP_ARG_RETURN GP_ARG_MIN_REG
#define FP_ARG_RETURN FP_ARG_MIN_REG

/* Flags for the call/call_value rtl operations set up by function_arg */
#define CALL_NORMAL		0x00000000	/* no special processing */
/* Bits in 0x00000001 are unused.  */
#define CALL_V4_CLEAR_FP_ARGS	0x00000002	/* V.4, no FP args passed */
#define CALL_V4_SET_FP_ARGS	0x00000004	/* V.4, FP args were passed */
#define CALL_LONG		0x00000008	/* always call indirect */

/* 1 if N is a possible register number for a function value
   as seen by the caller.

   On RS/6000, this is r3 and fp1.  */
#define FUNCTION_VALUE_REGNO_P(N)  ((N) == GP_ARG_RETURN || ((N) == FP_ARG_RETURN))

/* 1 if N is a possible register number for function argument passing.
   On RS/6000, these are r3-r10 and fp1-fp13.  */
#define FUNCTION_ARG_REGNO_P(N)						\
  (((unsigned)((N) - GP_ARG_MIN_REG) < (unsigned)(GP_ARG_NUM_REG))	\
   || ((unsigned)((N) - FP_ARG_MIN_REG) < (unsigned)(FP_ARG_NUM_REG)))


/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On the RS/6000, this is a structure.  The first element is the number of
   total argument words, the second is used to store the next
   floating-point register number, and the third says how many more args we
   have prototype types for.

   For ABI_V4, we treat these slightly differently -- `sysv_gregno' is
   the next availible GP register, `fregno' is the next available FP
   register, and `words' is the number of words used on the stack.

   The varargs/stdarg support requires that this structure's size
   be a multiple of sizeof(int).  */

typedef struct rs6000_args
{
  int words;			/* # words used for passing GP registers */
  int fregno;			/* next available FP register */
  int nargs_prototype;		/* # args left in the current prototype */
  int orig_nargs;		/* Original value of nargs_prototype */
  int prototype;		/* Whether a prototype was defined */
  int call_cookie;		/* Do special things for this call */
  int sysv_gregno;		/* next available GP register */
} CUMULATIVE_ARGS;

/* Define intermediate macro to compute the size (in registers) of an argument
   for the RS/6000.  */

#define RS6000_ARG_SIZE(MODE, TYPE, NAMED)				\
(! (NAMED) ? 0								\
 : (MODE) != BLKmode							\
 ? (GET_MODE_SIZE (MODE) + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD 	\
 : (int_size_in_bytes (TYPE) + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD)

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,INDIRECT) \
  init_cumulative_args (&CUM, FNTYPE, LIBNAME, FALSE)

/* Similar, but when scanning the definition of a procedure.  We always
   set NARGS_PROTOTYPE large so we never return an EXPR_LIST.  */

#define INIT_CUMULATIVE_INCOMING_ARGS(CUM,FNTYPE,LIBNAME) \
  init_cumulative_args (&CUM, FNTYPE, LIBNAME, TRUE)

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)	\
  function_arg_advance (&CUM, MODE, TYPE, NAMED)

/* Non-zero if we can use a floating-point register to pass this arg.  */
#define USE_FP_FOR_ARG_P(CUM,MODE,TYPE) \
  (GET_MODE_CLASS (MODE) == MODE_FLOAT  \
   && (CUM).fregno <= FP_ARG_MAX_REG    \
   && TARGET_HARD_FLOAT)

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
    (otherwise it is an extra parameter matching an ellipsis).

   On RS/6000 the first eight words of non-FP are normally in registers
   and the rest are pushed.  The first 13 FP args are in registers.

   If this is floating-point and no prototype is specified, we use
   both an FP and integer register (or possibly FP reg and stack).  Library
   functions (when TYPE is zero) always have the proper types for args,
   so we can pass the FP value just in one register.  emit_library_function
   doesn't support EXPR_LIST anyway.  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
  function_arg (&CUM, MODE, TYPE, NAMED)

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.  */

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) \
  function_arg_partial_nregs (&CUM, MODE, TYPE, NAMED)

/* A C expression that indicates when an argument must be passed by
   reference.  If nonzero for an argument, a copy of that argument is
   made in memory and a pointer to the argument is passed instead of
   the argument itself.  The pointer is passed in whatever way is
   appropriate for passing a pointer to that type. */

#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED) \
  function_arg_pass_by_reference(&CUM, MODE, TYPE, NAMED)

/* If defined, a C expression which determines whether, and in which
   direction, to pad out an argument with extra space.  The value
   should be of type `enum direction': either `upward' to pad above
   the argument, `downward' to pad below, or `none' to inhibit
   padding.  */

#define FUNCTION_ARG_PADDING(MODE, TYPE) function_arg_padding (MODE, TYPE)

/* If defined, a C expression that gives the alignment boundary, in bits,
   of an argument with the specified mode and type.  If it is not defined,
   PARM_BOUNDARY is used for all arguments.  */

#define FUNCTION_ARG_BOUNDARY(MODE, TYPE) \
  function_arg_boundary (MODE, TYPE)

/* Perform any needed actions needed for a function that is receiving a
   variable number of arguments.

   CUM is as above.

   MODE and TYPE are the mode and type of the current parameter.

   PRETEND_SIZE is a variable that should be set to the amount of stack
   that must be pushed by the prolog to pretend that our caller pushed
   it.

   Normally, this macro will push all remaining incoming registers on the
   stack and set PRETEND_SIZE to the length of the registers pushed.  */

#define SETUP_INCOMING_VARARGS(CUM,MODE,TYPE,PRETEND_SIZE,NO_RTL) \
  setup_incoming_varargs (&CUM, MODE, TYPE, &PRETEND_SIZE, NO_RTL)

/* Define the `__builtin_va_list' type for the ABI.  */
#define BUILD_VA_LIST_TYPE(VALIST) \
  (VALIST) = rs6000_build_va_list ()

/* Implement `va_start' for varargs and stdarg.  */
#define EXPAND_BUILTIN_VA_START(stdarg, valist, nextarg) \
  rs6000_va_start (stdarg, valist, nextarg)

/* Implement `va_arg'.  */
#define EXPAND_BUILTIN_VA_ARG(valist, type) \
  rs6000_va_arg (valist, type)

/* This macro generates the assembly code for function entry.
   FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.
   Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This macro is responsible for
   knowing which registers should not be saved even if used.  */

#define FUNCTION_PROLOGUE(FILE, SIZE) output_prolog (FILE, SIZE)

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)	\
  output_function_profiler ((FILE), (LABELNO));

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter. No definition is equivalent to
   always zero.

   On the RS/6000, this is non-zero because we can restore the stack from
   its backpointer, which we maintain.  */
#define EXIT_IGNORE_STACK	1

/* This macro generates the assembly code for function exit,
   on machines that need it.  If FUNCTION_EPILOGUE is not defined
   then individual return instructions are generated for each
   return statement.  Args are same as for FUNCTION_PROLOGUE.

   The function epilogue should not depend on the current stack pointer!
   It should use the frame pointer only.  This is mandatory because
   of alloca; we also take advantage of it to omit stack adjustments
   before returning.  */

#define FUNCTION_EPILOGUE(FILE, SIZE) output_epilog (FILE, SIZE)

/* TRAMPOLINE_TEMPLATE deleted */

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE rs6000_trampoline_size ()

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#define INITIALIZE_TRAMPOLINE(ADDR, FNADDR, CXT)		\
  rs6000_initialize_trampoline (ADDR, FNADDR, CXT)

/* If defined, a C expression whose value is nonzero if IDENTIFIER
   with arguments ARGS is a valid machine specific attribute for DECL.
   The attributes in ATTRIBUTES have previously been assigned to DECL.  */

#define VALID_MACHINE_DECL_ATTRIBUTE(DECL, ATTRIBUTES, NAME, ARGS) \
  (rs6000_valid_decl_attribute_p (DECL, ATTRIBUTES, NAME, ARGS))

/* If defined, a C expression whose value is nonzero if IDENTIFIER
   with arguments ARGS is a valid machine specific attribute for TYPE.
   The attributes in ATTRIBUTES have previously been assigned to TYPE.  */

#define VALID_MACHINE_TYPE_ATTRIBUTE(TYPE, ATTRIBUTES, NAME, ARGS) \
  (rs6000_valid_type_attribute_p (TYPE, ATTRIBUTES, NAME, ARGS))

/* If defined, a C expression whose value is zero if the attributes on
   TYPE1 and TYPE2 are incompatible, one if they are compatible, and
   two if they are nearly compatible (which causes a warning to be
   generated).  */

#define COMP_TYPE_ATTRIBUTES(TYPE1, TYPE2) \
  (rs6000_comp_type_attributes (TYPE1, TYPE2))

/* If defined, a C statement that assigns default attributes to newly
   defined TYPE.  */

#define SET_DEFAULT_TYPE_ATTRIBUTES(TYPE) \
  (rs6000_set_default_type_attributes (TYPE))


/* Definitions for __builtin_return_address and __builtin_frame_address.
   __builtin_return_address (0) should give link register (65), enable
   this. */
/* This should be uncommented, so that the link register is used, but
   currently this would result in unmatched insns and spilling fixed
   registers so we'll leave it for another day.  When these problems are
   taken care of one additional fetch will be necessary in RETURN_ADDR_RTX.
   (mrs) */
/* #define RETURN_ADDR_IN_PREVIOUS_FRAME */

/* Number of bytes into the frame return addresses can be found.  See
   rs6000_stack_info in rs6000.c for more information on how the different
   abi's store the return address.  */
#define RETURN_ADDRESS_OFFSET						\
 ((DEFAULT_ABI == ABI_AIX						\
   || DEFAULT_ABI == ABI_AIX_NODESC)	? 8 :				\
  (DEFAULT_ABI == ABI_V4						\
   || DEFAULT_ABI == ABI_SOLARIS)	? (TARGET_32BIT ? 4 : 8) :	\
  (fatal ("RETURN_ADDRESS_OFFSET not supported"), 0))

/* The current return address is in link register (65).  The return address
   of anything farther back is accessed normally at an offset of 8 from the
   frame pointer.  */
#define RETURN_ADDR_RTX(count, frame)			\
  ((count == -1)					\
   ? gen_rtx_REG (Pmode, LINK_REGISTER_REGNUM)		\
   : gen_rtx_MEM (Pmode,				\
		  memory_address			\
		  (Pmode, 				\
		   plus_constant (copy_to_reg		\
				  (gen_rtx_MEM (Pmode,	\
						memory_address (Pmode, \
								frame))), \
				  RETURN_ADDRESS_OFFSET))))

/* Definitions for register eliminations.

   We have two registers that can be eliminated on the RS/6000.  First, the
   frame pointer register can often be eliminated in favor of the stack
   pointer register.  Secondly, the argument pointer register can always be
   eliminated; it is replaced with either the stack or frame pointer.

   In addition, we use the elimination mechanism to see if r30 is needed
   Initially we assume that it isn't.  If it is, we spill it.  This is done
   by making it an eliminable register.  We replace it with itself so that
   if it isn't needed, then existing uses won't be modified.  */

/* This is an array of structures.  Each structure initializes one pair
   of eliminable registers.  The "from" register number is given first,
   followed by "to".  Eliminations of the same "from" register are listed
   in order of preference.  */
#define ELIMINABLE_REGS				\
{{ FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},	\
 { ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},	\
 { ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM},	\
 { 30, 30} }

/* Given FROM and TO register numbers, say whether this elimination is allowed.
   Frame pointer elimination is automatically handled.

   For the RS/6000, if frame pointer elimination is being done, we would like
   to convert ap into fp, not sp.

   We need r30 if -mminimal-toc was specified, and there are constant pool
   references.  */

#define CAN_ELIMINATE(FROM, TO)					\
 ((FROM) == ARG_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM	\
  ? ! frame_pointer_needed					\
  : (FROM) == 30 ? ! TARGET_MINIMAL_TOC || TARGET_NO_TOC || get_pool_size () == 0 \
  : 1)

/* Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
{									\
  rs6000_stack_t *info = rs6000_stack_info ();				\
									\
 if ((FROM) == FRAME_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM)	\
   (OFFSET) = (info->push_p) ? 0 : - info->total_size;			\
 else if ((FROM) == ARG_POINTER_REGNUM && (TO) == FRAME_POINTER_REGNUM)	\
   (OFFSET) = info->total_size;						\
 else if ((FROM) == ARG_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM)	\
   (OFFSET) = (info->push_p) ? info->total_size : 0;			\
  else if ((FROM) == 30)						\
    (OFFSET) = 0;							\
  else									\
    abort ();								\
}

/* Addressing modes, and classification of registers for them.  */

/* #define HAVE_POST_INCREMENT 0 */
/* #define HAVE_POST_DECREMENT 0 */

#define HAVE_PRE_DECREMENT 1
#define HAVE_PRE_INCREMENT 1

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */

#define REGNO_OK_FOR_INDEX_P(REGNO)				\
((REGNO) < FIRST_PSEUDO_REGISTER				\
 ? (REGNO) <= 31 || (REGNO) == 67				\
 : (reg_renumber[REGNO] >= 0					\
    && (reg_renumber[REGNO] <= 31 || reg_renumber[REGNO] == 67)))

#define REGNO_OK_FOR_BASE_P(REGNO)				\
((REGNO) < FIRST_PSEUDO_REGISTER				\
 ? ((REGNO) > 0 && (REGNO) <= 31) || (REGNO) == 67		\
 : (reg_renumber[REGNO] > 0					\
    && (reg_renumber[REGNO] <= 31 || reg_renumber[REGNO] == 67)))

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 2

/* Recognize any constant value that is a valid address.  */

#define CONSTANT_ADDRESS_P(X)   \
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
   || GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST		\
   || GET_CODE (X) == HIGH)

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.

   On the RS/6000, all integer constants are acceptable, most won't be valid
   for particular insns, though.  Only easy FP constants are
   acceptable.  */

#define LEGITIMATE_CONSTANT_P(X)				\
  (GET_CODE (X) != CONST_DOUBLE || GET_MODE (X) == VOIDmode	\
   || (TARGET_POWERPC64 && GET_MODE (X) == DImode)		\
   || easy_fp_constant (X, GET_MODE (X)))

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
#define REG_OK_FOR_INDEX_P(X)			\
  (REGNO (X) <= 31 || REGNO (X) == 67 || REGNO (X) >= FIRST_PSEUDO_REGISTER)

/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X)					 \
  (REGNO (X) > 0 && REG_OK_FOR_INDEX_P (X))

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

   On the RS/6000, there are four valid address: a SYMBOL_REF that
   refers to a constant pool entry of an address (or the sum of it
   plus a constant), a short (16-bit signed) constant plus a register,
   the sum of two registers, or a register indirect, possibly with an
   auto-increment.  For DFmode and DImode with an constant plus register,
   we must ensure that both words are addressable or PowerPC64 with offset
   word aligned.

   For modes spanning multiple registers (DFmode in 32-bit GPRs,
   32-bit DImode, TImode), indexed addressing cannot be used because
   adjacent memory cells are accessed by adding word-sized offsets
   during assembly output.  */

#define CONSTANT_POOL_EXPR_P(X) (constant_pool_expr_p (X))

#define TOC_RELATIVE_EXPR_P(X) (toc_relative_expr_p (X))

#define LEGITIMATE_CONSTANT_POOL_ADDRESS_P(X)				\
  (TARGET_TOC								\
  && GET_CODE (X) == PLUS						\
  && GET_CODE (XEXP (X, 0)) == REG					\
  && (TARGET_MINIMAL_TOC || REGNO (XEXP (X, 0)) == TOC_REGISTER)	\
  && CONSTANT_POOL_EXPR_P (XEXP (X, 1)))

#define LEGITIMATE_SMALL_DATA_P(MODE, X)				\
  ((DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_SOLARIS)		\
   && !flag_pic && !TARGET_TOC						\
   && (GET_CODE (X) == SYMBOL_REF || GET_CODE (X) == CONST)		\
   && small_data_operand (X, MODE))

#define LEGITIMATE_ADDRESS_INTEGER_P(X,OFFSET)				\
 (GET_CODE (X) == CONST_INT						\
  && (unsigned HOST_WIDE_INT) (INTVAL (X) + (OFFSET) + 0x8000) < 0x10000)

#define LEGITIMATE_OFFSET_ADDRESS_P(MODE,X)		\
 (GET_CODE (X) == PLUS					\
  && GET_CODE (XEXP (X, 0)) == REG			\
  && REG_OK_FOR_BASE_P (XEXP (X, 0))			\
  && LEGITIMATE_ADDRESS_INTEGER_P (XEXP (X, 1), 0)	\
  && (((MODE) != DFmode && (MODE) != DImode)		\
      || (TARGET_32BIT					\
	  ? LEGITIMATE_ADDRESS_INTEGER_P (XEXP (X, 1), 4) \
	  : ! (INTVAL (XEXP (X, 1)) & 3)))		\
  && ((MODE) != TImode					\
      || (TARGET_32BIT					\
	  ? LEGITIMATE_ADDRESS_INTEGER_P (XEXP (X, 1), 12) \
	  : (LEGITIMATE_ADDRESS_INTEGER_P (XEXP (X, 1), 8) \
	     && ! (INTVAL (XEXP (X, 1)) & 3)))))

#define LEGITIMATE_INDEXED_ADDRESS_P(X)		\
 (GET_CODE (X) == PLUS				\
  && GET_CODE (XEXP (X, 0)) == REG		\
  && GET_CODE (XEXP (X, 1)) == REG		\
  && ((REG_OK_FOR_BASE_P (XEXP (X, 0))		\
       && REG_OK_FOR_INDEX_P (XEXP (X, 1)))	\
      || (REG_OK_FOR_BASE_P (XEXP (X, 1))	\
	  && REG_OK_FOR_INDEX_P (XEXP (X, 0)))))

#define LEGITIMATE_INDIRECT_ADDRESS_P(X)	\
  (GET_CODE (X) == REG && REG_OK_FOR_BASE_P (X))

#define LEGITIMATE_LO_SUM_ADDRESS_P(MODE, X)		\
  (TARGET_ELF						\
   && ! flag_pic && ! TARGET_TOC			\
   && (MODE) != DImode					\
   && (MODE) != TImode					\
   && (TARGET_HARD_FLOAT || (MODE) != DFmode)		\
   && GET_CODE (X) == LO_SUM				\
   && GET_CODE (XEXP (X, 0)) == REG			\
   && REG_OK_FOR_BASE_P (XEXP (X, 0))			\
   && CONSTANT_P (XEXP (X, 1)))

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)		\
{ if (LEGITIMATE_INDIRECT_ADDRESS_P (X))		\
    goto ADDR;						\
  if ((GET_CODE (X) == PRE_INC || GET_CODE (X) == PRE_DEC) \
      && TARGET_UPDATE					\
      && LEGITIMATE_INDIRECT_ADDRESS_P (XEXP (X, 0)))	\
    goto ADDR;						\
  if (LEGITIMATE_SMALL_DATA_P (MODE, X))		\
    goto ADDR;						\
  if (LEGITIMATE_CONSTANT_POOL_ADDRESS_P (X))		\
    goto ADDR;						\
  if (LEGITIMATE_OFFSET_ADDRESS_P (MODE, X))		\
    goto ADDR;						\
  if ((MODE) != TImode					\
      && (TARGET_HARD_FLOAT || TARGET_POWERPC64 || (MODE) != DFmode) \
      && (TARGET_POWERPC64 || (MODE) != DImode)		\
      && LEGITIMATE_INDEXED_ADDRESS_P (X))		\
    goto ADDR;						\
  if (LEGITIMATE_LO_SUM_ADDRESS_P (MODE, X))		\
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
   opportunities to optimize the output.

   On RS/6000, first check for the sum of a register with a constant
   integer that is out of range.  If so, generate code to add the
   constant with the low-order 16 bits masked to the register and force
   this result into another register (this can be done with `cau').
   Then generate an address of REG+(CONST&0xffff), allowing for the
   possibility of bit 16 being a one.

   Then check for the sum of a register and something not constant, try to
   load the other things into a register and return the sum.  */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)			\
{  rtx result = rs6000_legitimize_address (X, OLDX, MODE);	\
   if (result != NULL_RTX)					\
     {								\
       (X) = result;						\
       goto WIN;						\
     }								\
}

/* Try a machine-dependent way of reloading an illegitimate address
   operand.  If we find one, push the reload and jump to WIN.  This
   macro is used in only one place: `find_reloads_address' in reload.c.

   For RS/6000, we wish to handle large displacements off a base
   register by splitting the addend across an addiu/addis and the mem insn.
   This cuts number of extra insns needed from 3 to 1.  */
   
#define LEGITIMIZE_RELOAD_ADDRESS(X,MODE,OPNUM,TYPE,IND_LEVELS,WIN)     \
do {                                                                    \
  /* We must recognize output that we have already generated ourselves.  */ \
  if (GET_CODE (X) == PLUS						\
      && GET_CODE (XEXP (X, 0)) == PLUS					\
      && GET_CODE (XEXP (XEXP (X, 0), 0)) == REG			\
      && GET_CODE (XEXP (XEXP (X, 0), 1)) == CONST_INT			\
      && GET_CODE (XEXP (X, 1)) == CONST_INT)				\
    {									\
      push_reload (XEXP (X, 0), NULL_RTX, &XEXP (X, 0), NULL_PTR,       \
                   BASE_REG_CLASS, GET_MODE (X), VOIDmode, 0, 0,        \
                   OPNUM, TYPE);                                        \
      goto WIN;                                                         \
    }									\
  if (GET_CODE (X) == PLUS                                              \
      && GET_CODE (XEXP (X, 0)) == REG                                  \
      && REGNO (XEXP (X, 0)) < FIRST_PSEUDO_REGISTER                    \
      && REG_MODE_OK_FOR_BASE_P (XEXP (X, 0), MODE)                     \
      && GET_CODE (XEXP (X, 1)) == CONST_INT)                           \
    {                                                                   \
      HOST_WIDE_INT val = INTVAL (XEXP (X, 1));                         \
      HOST_WIDE_INT low = ((val & 0xffff) ^ 0x8000) - 0x8000;           \
      HOST_WIDE_INT high                                                \
        = (((val - low) & 0xffffffffu) ^ 0x80000000u) - 0x80000000u;       \
                                                                        \
      /* Check for 32-bit overflow.  */                                 \
      if (high + low != val)                                            \
        break;                                                          \
                                                                        \
      /* Reload the high part into a base reg; leave the low part       \
         in the mem directly.  */                                       \
                                                                        \
      X = gen_rtx_PLUS (GET_MODE (X),                                   \
                        gen_rtx_PLUS (GET_MODE (X), XEXP (X, 0),        \
                                      GEN_INT (high)),                  \
                        GEN_INT (low));                                 \
                                                                        \
      push_reload (XEXP (X, 0), NULL_RTX, &XEXP (X, 0), NULL_PTR,       \
                   BASE_REG_CLASS, GET_MODE (X), VOIDmode, 0, 0,        \
                   OPNUM, TYPE);                                        \
      goto WIN;                                                         \
    }                                                                   \
  else if (TARGET_TOC 							\
	   && CONSTANT_POOL_EXPR_P (X)					\
	   && ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (get_pool_constant (X)))	\
    {									\
      (X) = create_TOC_reference (X);					\
      goto WIN;								\
    }									\
} while (0)

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.

   On the RS/6000 this is true if the address is valid with a zero offset
   but not with an offset of four (this means it cannot be used as an
   address for DImode or DFmode) or is a pre-increment or decrement.  Since
   we know it is valid, we just check for an address that is not valid with
   an offset of four.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)		\
{ if (GET_CODE (ADDR) == PLUS					\
      && LEGITIMATE_ADDRESS_INTEGER_P (XEXP (ADDR, 1), 0)	\
      && ! LEGITIMATE_ADDRESS_INTEGER_P (XEXP (ADDR, 1),	\
					 (TARGET_32BIT ? 4 : 8))) \
    goto LABEL;							\
  if (TARGET_UPDATE && GET_CODE (ADDR) == PRE_INC)		\
    goto LABEL;							\
  if (TARGET_UPDATE && GET_CODE (ADDR) == PRE_DEC)		\
    goto LABEL;							\
  if (GET_CODE (ADDR) == LO_SUM)				\
    goto LABEL;							\
}

/* The register number of the register used to address a table of
   static data addresses in memory.  In some cases this register is
   defined by a processor's "application binary interface" (ABI).
   When this macro is defined, RTL is generated for this register
   once, as with the stack pointer and frame pointer registers.  If
   this macro is not defined, it is up to the machine-dependent files
   to allocate such a register (if necessary).  */

#define PIC_OFFSET_TABLE_REGNUM 30

#define TOC_REGISTER (TARGET_MINIMAL_TOC ? 30 : 2)

/* Define this macro if the register defined by
   `PIC_OFFSET_TABLE_REGNUM' is clobbered by calls.  Do not define
   this macro if `PPIC_OFFSET_TABLE_REGNUM' is not defined. */

/* #define PIC_OFFSET_TABLE_REG_CALL_CLOBBERED */

/* By generating position-independent code, when two different
   programs (A and B) share a common library (libC.a), the text of
   the library can be shared whether or not the library is linked at
   the same address for both programs.  In some of these
   environments, position-independent code requires not only the use
   of different addressing modes, but also special code to enable the
   use of these addressing modes.

   The `FINALIZE_PIC' macro serves as a hook to emit these special
   codes once the function is being compiled into assembly code, but
   not before.  (It is not done before, because in the case of
   compiling an inline function, it would lead to multiple PIC
   prologues being included in functions which used inline functions
   and were compiled to assembly language.)  */

/* #define FINALIZE_PIC */

/* A C expression that is nonzero if X is a legitimate immediate
   operand on the target machine when generating position independent
   code.  You can assume that X satisfies `CONSTANT_P', so you need
   not check this.  You can also assume FLAG_PIC is true, so you need
   not check it either.  You need not define this macro if all
   constants (including `SYMBOL_REF') can be immediate operands when
   generating position independent code.  */

/* #define LEGITIMATE_PIC_OPERAND_P (X) */

/* In rare cases, correct code generation requires extra machine
   dependent processing between the second jump optimization pass and
   delayed branch scheduling.  On those machines, define this macro
   as a C statement to act on the code starting at INSN.  */

/* #define MACHINE_DEPENDENT_REORG(INSN) */


/* Define this if some processing needs to be done immediately before
   emitting code for an insn.  */

/* #define FINAL_PRESCAN_INSN(INSN,OPERANDS,NOPERANDS) */

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE SImode

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses. */
#define CASE_VECTOR_PC_RELATIVE 1

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 0

/* This flag, if defined, says the same insns that convert to a signed fixnum
   also convert validly to an unsigned one.  */

/* #define FIXUNS_TRUNC_LIKE_FIX_TRUNC */

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX (! TARGET_POWERPC64 ? 4 : 8)
#define MAX_MOVE_MAX 8

/* Nonzero if access to memory by bytes is no faster than for words.
   Also non-zero if doing byte operations (specifically shifts) in registers
   is undesirable.  */
#define SLOW_BYTE_ACCESS 1

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, NIL if none.  */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* Define if loading short immediate values into registers sign extends.  */
#define SHORT_IMMEDIATES_SIGN_EXTEND

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode (TARGET_32BIT ? SImode : DImode)

/* Mode of a function address in a call instruction (for indexing purposes).
   Doesn't matter on RS/6000.  */
#define FUNCTION_MODE (TARGET_32BIT ? SImode : DImode)

/* Define this if addresses of constant functions
   shouldn't be put through pseudo regs where they can be cse'd.
   Desirable on machines where ordinary constants are expensive
   but a CALL with constant address is cheap.  */
#define NO_FUNCTION_CSE

/* Define this to be nonzero if shift instructions ignore all but the low-order
   few bits.

   The sle and sre instructions which allow SHIFT_COUNT_TRUNCATED
   have been dropped from the PowerPC architecture.  */

#define SHIFT_COUNT_TRUNCATED (TARGET_POWER ? 1 : 0)

/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch.

   On the RS/6000, if it is valid in the insn, it is free.  So this
   always returns 0.  */

#define CONST_COSTS(RTX,CODE,OUTER_CODE)			\
  case CONST_INT:						\
  case CONST:							\
  case LABEL_REF:						\
  case SYMBOL_REF:						\
  case CONST_DOUBLE:						\
  case HIGH:							\
    return 0;

/* Provide the costs of a rtl expression.  This is in the body of a
   switch on CODE.  */

#define RTX_COSTS(X,CODE,OUTER_CODE)					\
  case PLUS:								\
    return ((GET_CODE (XEXP (X, 1)) == CONST_INT			\
	     && ((unsigned HOST_WIDE_INT) (INTVAL (XEXP (X, 1))		\
					   + 0x8000) >= 0x10000)	\
	     && ((INTVAL (XEXP (X, 1)) & 0xffff) != 0))			\
	    ? COSTS_N_INSNS (2)						\
	    : COSTS_N_INSNS (1));					\
  case AND:								\
  case IOR:								\
  case XOR:								\
    return ((GET_CODE (XEXP (X, 1)) == CONST_INT			\
	     && (INTVAL (XEXP (X, 1)) & (~ (HOST_WIDE_INT) 0xffff)) != 0 \
	     && ((INTVAL (XEXP (X, 1)) & 0xffff) != 0))			\
	    ? COSTS_N_INSNS (2)						\
	    : COSTS_N_INSNS (1));					\
  case MULT:								\
    switch (rs6000_cpu)							\
      {									\
      case PROCESSOR_RIOS1:						\
        return (GET_CODE (XEXP (X, 1)) != CONST_INT			\
		? COSTS_N_INSNS (5)					\
		: INTVAL (XEXP (X, 1)) >= -256 && INTVAL (XEXP (X, 1)) <= 255 \
		? COSTS_N_INSNS (3) : COSTS_N_INSNS (4));		\
      case PROCESSOR_RS64A:						\
        return (GET_CODE (XEXP (X, 1)) != CONST_INT			\
		? GET_MODE (XEXP (X, 1)) != DImode			\
		? COSTS_N_INSNS (20) : COSTS_N_INSNS (34)		\
		: INTVAL (XEXP (X, 1)) >= -256 && INTVAL (XEXP (X, 1)) <= 255 \
		? COSTS_N_INSNS (12) : COSTS_N_INSNS (14));		\
      case PROCESSOR_RIOS2:						\
      case PROCESSOR_MPCCORE:						\
      case PROCESSOR_PPC604e:						\
        return COSTS_N_INSNS (2);					\
      case PROCESSOR_PPC601:						\
        return COSTS_N_INSNS (5);					\
      case PROCESSOR_PPC603:						\
      case PROCESSOR_PPC750:						\
        return (GET_CODE (XEXP (X, 1)) != CONST_INT			\
		? COSTS_N_INSNS (5)					\
		: INTVAL (XEXP (X, 1)) >= -256 && INTVAL (XEXP (X, 1)) <= 255 \
		? COSTS_N_INSNS (2) : COSTS_N_INSNS (3));		\
      case PROCESSOR_PPC403:						\
      case PROCESSOR_PPC604:						\
        return COSTS_N_INSNS (4);					\
      case PROCESSOR_PPC620:						\
      case PROCESSOR_PPC630:						\
        return (GET_CODE (XEXP (X, 1)) != CONST_INT			\
		? GET_MODE (XEXP (X, 1)) != DImode			\
		? COSTS_N_INSNS (4) : COSTS_N_INSNS (7)			\
		: INTVAL (XEXP (X, 1)) >= -256 && INTVAL (XEXP (X, 1)) <= 255 \
		? COSTS_N_INSNS (3) : COSTS_N_INSNS (4));		\
      }									\
  case DIV:								\
  case MOD:								\
    if (GET_CODE (XEXP (X, 1)) == CONST_INT				\
	&& exact_log2 (INTVAL (XEXP (X, 1))) >= 0)			\
      return COSTS_N_INSNS (2);						\
    /* otherwise fall through to normal divide.  */			\
  case UDIV:								\
  case UMOD:								\
    switch (rs6000_cpu)							\
      {									\
      case PROCESSOR_RIOS1:						\
	return COSTS_N_INSNS (19);					\
      case PROCESSOR_RIOS2:						\
	return COSTS_N_INSNS (13);					\
      case PROCESSOR_RS64A:						\
        return (GET_MODE (XEXP (X, 1)) != DImode			\
		? COSTS_N_INSNS (65)					\
		: COSTS_N_INSNS (67));					\
      case PROCESSOR_MPCCORE:						\
	return COSTS_N_INSNS (6);					\
      case PROCESSOR_PPC403:						\
	return COSTS_N_INSNS (33);					\
      case PROCESSOR_PPC601:						\
	return COSTS_N_INSNS (36);					\
      case PROCESSOR_PPC603:						\
	return COSTS_N_INSNS (37);					\
      case PROCESSOR_PPC604:						\
      case PROCESSOR_PPC604e:						\
	return COSTS_N_INSNS (20);					\
      case PROCESSOR_PPC620:						\
      case PROCESSOR_PPC630:						\
        return (GET_MODE (XEXP (X, 1)) != DImode			\
		? COSTS_N_INSNS (21)					\
		: COSTS_N_INSNS (37));					\
      case PROCESSOR_PPC750:						\
        return COSTS_N_INSNS (19);					\
      }									\
  case FFS:								\
    return COSTS_N_INSNS (4);						\
  case MEM:								\
    /* MEM should be slightly more expensive than (plus (reg) (const)) */ \
    return 5;

/* Compute the cost of an address.  This is meant to approximate the size
   and/or execution delay of an insn using that address.  If the cost is
   approximated by the RTL complexity, including CONST_COSTS above, as
   is usually the case for CISC machines, this macro should not be defined.
   For aggressively RISCy machines, only one insn format is allowed, so
   this macro should be a constant.  The value of this macro only matters
   for valid addresses.

   For the RS/6000, everything is cost 0.  */

#define ADDRESS_COST(RTX) 0

/* Adjust the length of an INSN.  LENGTH is the currently-computed length and
   should be adjusted to reflect any required changes.  This macro is used when
   there is some systematic length adjustment required that would be difficult
   to express in the length attribute.  */

/* #define ADJUST_INSN_LENGTH(X,LENGTH) */

/* Add any extra modes needed to represent the condition code.

   For the RS/6000, we need separate modes when unsigned (logical) comparisons
   are being done and we need a separate mode for floating-point.  We also
   use a mode for the case when we are comparing the results of two
   comparisons.  */

#define EXTRA_CC_MODES		\
    CC(CCUNSmode,  "CCUNS")	\
    CC(CCFPmode,   "CCFP")	\
    CC(CCEQmode,   "CCEQ")

/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
   return the mode to be used for the comparison.  For floating-point, CCFPmode
   should be used.  CCUNSmode should be used for unsigned comparisons.
   CCEQmode should be used when we are doing an inequality comparison on
   the result of a comparison. CCmode should be used in all other cases.  */

#define SELECT_CC_MODE(OP,X,Y) \
  (GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT ? CCFPmode	\
   : (OP) == GTU || (OP) == LTU || (OP) == GEU || (OP) == LEU ? CCUNSmode \
   : (((OP) == EQ || (OP) == NE) && GET_RTX_CLASS (GET_CODE (X)) == '<'   \
      ? CCEQmode : CCmode))

/* Define the information needed to generate branch and scc insns.  This is
   stored from the compare operation.  Note that we can't use "rtx" here
   since it hasn't been defined!  */

extern struct rtx_def *rs6000_compare_op0, *rs6000_compare_op1;
extern int rs6000_compare_fp_p;

/* Set to non-zero by "fix" operation to indicate that itrunc and
   uitrunc must be defined.  */

extern int rs6000_trunc_used;

/* Function names to call to do floating point truncation.  */

#define RS6000_ITRUNC "__itrunc"
#define RS6000_UITRUNC "__uitrunc"


/* Control the assembler format that we output.  */

/* A C string constant describing how to begin a comment in the target
   assembler language.  The compiler assumes that the comment will end at
   the end of the line.  */
#define ASM_COMMENT_START " #"

/* We define this to prevent the name mangler from putting dollar signs into
   function names.  */

#define NO_DOLLAR_IN_LABEL

/* We define this to 0 so that gcc will never accept a dollar sign in a
   variable name.  This is needed because the AIX assembler will not accept
   dollar signs.  */

#define DOLLARS_IN_IDENTIFIERS 0

/* Implicit library calls should use memcpy, not bcopy, etc.  */

#define TARGET_MEM_FUNCTIONS

/* Define the name of the section to use for the exception tables.
   TODO: test and see if we can use read_only_data_section, if so,
   remove this.  */

#define EXCEPTION_SECTION data_section

/* If we are referencing a function that is static or is known to be
   in this file, make the SYMBOL_REF special.  We can use this to indicate
   that we can branch to this function without emitting a no-op after the
   call.  Do not set this flag if the function is weakly defined. */

#define ENCODE_SECTION_INFO(DECL)			\
  if (TREE_CODE (DECL) == FUNCTION_DECL			\
      && (TREE_ASM_WRITTEN (DECL) || ! TREE_PUBLIC (DECL)) \
      && ! DECL_WEAK (DECL))				\
    SYMBOL_REF_FLAG (XEXP (DECL_RTL (DECL), 0)) = 1;

/* Flag to say the TOC is initialized */
extern int toc_initialized;

/* Return non-zero if this entry is to be written into the constant pool
   in a special way.  We do so if this is a SYMBOL_REF, LABEL_REF or a CONST
   containing one of them.  If -mfp-in-toc (the default), we also do
   this for floating-point constants.  We actually can only do this
   if the FP formats of the target and host machines are the same, but
   we can't check that since not every file that uses
   GO_IF_LEGITIMATE_ADDRESS_P includes real.h.  */

#define ASM_OUTPUT_SPECIAL_POOL_ENTRY_P(X)				\
  (TARGET_TOC								\
   && (GET_CODE (X) == SYMBOL_REF					\
       || (GET_CODE (X) == CONST && GET_CODE (XEXP (X, 0)) == PLUS	\
	   && GET_CODE (XEXP (XEXP (X, 0), 0)) == SYMBOL_REF)		\
       || GET_CODE (X) == LABEL_REF					\
       || (! (TARGET_NO_FP_IN_TOC && ! TARGET_MINIMAL_TOC)		\
	   && GET_CODE (X) == CONST_DOUBLE				\
	   && (GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT		\
	       || (TARGET_POWERPC64 && GET_MODE (X) == DImode)))))
#if 0
	   && BITS_PER_WORD == HOST_BITS_PER_INT)))
#endif

/* Macro to output a special constant pool entry.  Go to WIN if we output
   it.  Otherwise, it is written the usual way.

   On the RS/6000, toc entries are handled this way.  */

#define ASM_OUTPUT_SPECIAL_POOL_ENTRY(FILE, X, MODE, ALIGN, LABELNO, WIN)  \
{ if (ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (X))	\
    {						\
      output_toc (FILE, X, LABELNO);		\
      goto WIN;					\
    }						\
}

/* This is how we tell the assembler that two symbols have the same value.  */

#define SET_ASM_OP ".set"

/* This implementes the `alias' attribute.  */

#define ASM_OUTPUT_DEF_FROM_DECLS(FILE,decl,target)	\
do {							\
  const char * alias = XSTR (XEXP (DECL_RTL (decl), 0), 0); \
  char * name = IDENTIFIER_POINTER (target);		\
  if (TREE_CODE (decl) == FUNCTION_DECL			\
      && DEFAULT_ABI == ABI_AIX)			\
    {							\
      if (TREE_PUBLIC (decl))				\
	{						\
	  fputs ("\t.globl .", FILE);			\
	  assemble_name (FILE, alias);			\
	  putc ('\n', FILE);				\
	}						\
      else						\
	{						\
	  fputs ("\t.lglobl .", FILE);			\
	  assemble_name (FILE, alias);			\
	  putc ('\n', FILE);				\
	}						\
      fputs ("\t.set .", FILE);				\
      assemble_name (FILE, alias);			\
      fputs (",.", FILE);				\
      assemble_name (FILE, name);			\
      fputc ('\n', FILE);				\
    }							\
  ASM_OUTPUT_DEF (FILE, alias, name);			\
} while (0)

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON ""

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF ""

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

extern char rs6000_reg_names[][8];	/* register names (0 vs. %r0). */

#define REGISTER_NAMES							\
{									\
  &rs6000_reg_names[ 0][0],	/* r0   */				\
  &rs6000_reg_names[ 1][0],	/* r1	*/				\
  &rs6000_reg_names[ 2][0],     /* r2	*/				\
  &rs6000_reg_names[ 3][0],	/* r3	*/				\
  &rs6000_reg_names[ 4][0],	/* r4	*/				\
  &rs6000_reg_names[ 5][0],	/* r5	*/				\
  &rs6000_reg_names[ 6][0],	/* r6	*/				\
  &rs6000_reg_names[ 7][0],	/* r7	*/				\
  &rs6000_reg_names[ 8][0],	/* r8	*/				\
  &rs6000_reg_names[ 9][0],	/* r9	*/				\
  &rs6000_reg_names[10][0],	/* r10  */				\
  &rs6000_reg_names[11][0],	/* r11  */				\
  &rs6000_reg_names[12][0],	/* r12  */				\
  &rs6000_reg_names[13][0],	/* r13  */				\
  &rs6000_reg_names[14][0],	/* r14  */				\
  &rs6000_reg_names[15][0],	/* r15  */				\
  &rs6000_reg_names[16][0],	/* r16  */				\
  &rs6000_reg_names[17][0],	/* r17  */				\
  &rs6000_reg_names[18][0],	/* r18  */				\
  &rs6000_reg_names[19][0],	/* r19  */				\
  &rs6000_reg_names[20][0],	/* r20  */				\
  &rs6000_reg_names[21][0],	/* r21  */				\
  &rs6000_reg_names[22][0],	/* r22  */				\
  &rs6000_reg_names[23][0],	/* r23  */				\
  &rs6000_reg_names[24][0],	/* r24  */				\
  &rs6000_reg_names[25][0],	/* r25  */				\
  &rs6000_reg_names[26][0],	/* r26  */				\
  &rs6000_reg_names[27][0],	/* r27  */				\
  &rs6000_reg_names[28][0],	/* r28  */				\
  &rs6000_reg_names[29][0],	/* r29  */				\
  &rs6000_reg_names[30][0],	/* r30  */				\
  &rs6000_reg_names[31][0],	/* r31  */				\
									\
  &rs6000_reg_names[32][0],     /* fr0  */				\
  &rs6000_reg_names[33][0],	/* fr1  */				\
  &rs6000_reg_names[34][0],	/* fr2  */				\
  &rs6000_reg_names[35][0],	/* fr3  */				\
  &rs6000_reg_names[36][0],	/* fr4  */				\
  &rs6000_reg_names[37][0],	/* fr5  */				\
  &rs6000_reg_names[38][0],	/* fr6  */				\
  &rs6000_reg_names[39][0],	/* fr7  */				\
  &rs6000_reg_names[40][0],	/* fr8  */				\
  &rs6000_reg_names[41][0],	/* fr9  */				\
  &rs6000_reg_names[42][0],	/* fr10 */				\
  &rs6000_reg_names[43][0],	/* fr11 */				\
  &rs6000_reg_names[44][0],	/* fr12 */				\
  &rs6000_reg_names[45][0],	/* fr13 */				\
  &rs6000_reg_names[46][0],	/* fr14 */				\
  &rs6000_reg_names[47][0],	/* fr15 */				\
  &rs6000_reg_names[48][0],	/* fr16 */				\
  &rs6000_reg_names[49][0],	/* fr17 */				\
  &rs6000_reg_names[50][0],	/* fr18 */				\
  &rs6000_reg_names[51][0],	/* fr19 */				\
  &rs6000_reg_names[52][0],	/* fr20 */				\
  &rs6000_reg_names[53][0],	/* fr21 */				\
  &rs6000_reg_names[54][0],	/* fr22 */				\
  &rs6000_reg_names[55][0],	/* fr23 */				\
  &rs6000_reg_names[56][0],	/* fr24 */				\
  &rs6000_reg_names[57][0],	/* fr25 */				\
  &rs6000_reg_names[58][0],	/* fr26 */				\
  &rs6000_reg_names[59][0],	/* fr27 */				\
  &rs6000_reg_names[60][0],	/* fr28 */				\
  &rs6000_reg_names[61][0],	/* fr29 */				\
  &rs6000_reg_names[62][0],	/* fr30 */				\
  &rs6000_reg_names[63][0],	/* fr31 */				\
									\
  &rs6000_reg_names[64][0],     /* mq   */				\
  &rs6000_reg_names[65][0],	/* lr   */				\
  &rs6000_reg_names[66][0],	/* ctr  */				\
  &rs6000_reg_names[67][0],	/* ap   */				\
									\
  &rs6000_reg_names[68][0],	/* cr0  */				\
  &rs6000_reg_names[69][0],	/* cr1  */				\
  &rs6000_reg_names[70][0],	/* cr2  */				\
  &rs6000_reg_names[71][0],	/* cr3  */				\
  &rs6000_reg_names[72][0],	/* cr4  */				\
  &rs6000_reg_names[73][0],	/* cr5  */				\
  &rs6000_reg_names[74][0],	/* cr6  */				\
  &rs6000_reg_names[75][0],	/* cr7  */				\
									\
  &rs6000_reg_names[76][0],	/* xer  */				\
}

/* print-rtl can't handle the above REGISTER_NAMES, so define the
   following for it.  Switch to use the alternate names since
   they are more mnemonic.  */

#define DEBUG_REGISTER_NAMES						\
{									\
     "r0", "r1",   "r2",  "r3",  "r4",  "r5",  "r6",  "r7",		\
     "r8", "r9",  "r10", "r11", "r12", "r13", "r14", "r15",		\
    "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23",		\
    "r24", "r25", "r26", "r27", "r28", "r29", "r30", "r31",		\
     "f0",  "f1",  "f2",  "f3",  "f4",  "f5",  "f6",  "f7",		\
     "f8",  "f9", "f10", "f11", "f12", "f13", "f14", "f15",		\
    "f16", "f17", "f18", "f19", "f20", "f21", "f22", "f23",		\
    "f24", "f25", "f26", "f27", "f28", "f29", "f30", "f31",		\
     "mq",  "lr", "ctr",  "ap",						\
    "cr0", "cr1", "cr2", "cr3", "cr4", "cr5", "cr6", "cr7",		\
  "xer"									\
}

/* Table of additional register names to use in user input.  */

#define ADDITIONAL_REGISTER_NAMES \
 {{"r0",    0}, {"r1",    1}, {"r2",    2}, {"r3",    3},	\
  {"r4",    4}, {"r5",    5}, {"r6",    6}, {"r7",    7},	\
  {"r8",    8}, {"r9",    9}, {"r10",  10}, {"r11",  11},	\
  {"r12",  12}, {"r13",  13}, {"r14",  14}, {"r15",  15},	\
  {"r16",  16}, {"r17",  17}, {"r18",  18}, {"r19",  19},	\
  {"r20",  20}, {"r21",  21}, {"r22",  22}, {"r23",  23},	\
  {"r24",  24}, {"r25",  25}, {"r26",  26}, {"r27",  27},	\
  {"r28",  28}, {"r29",  29}, {"r30",  30}, {"r31",  31},	\
  {"fr0",  32}, {"fr1",  33}, {"fr2",  34}, {"fr3",  35},	\
  {"fr4",  36}, {"fr5",  37}, {"fr6",  38}, {"fr7",  39},	\
  {"fr8",  40}, {"fr9",  41}, {"fr10", 42}, {"fr11", 43},	\
  {"fr12", 44}, {"fr13", 45}, {"fr14", 46}, {"fr15", 47},	\
  {"fr16", 48}, {"fr17", 49}, {"fr18", 50}, {"fr19", 51},	\
  {"fr20", 52}, {"fr21", 53}, {"fr22", 54}, {"fr23", 55},	\
  {"fr24", 56}, {"fr25", 57}, {"fr26", 58}, {"fr27", 59},	\
  {"fr28", 60}, {"fr29", 61}, {"fr30", 62}, {"fr31", 63},	\
  /* no additional names for: mq, lr, ctr, ap */		\
  {"cr0",  68}, {"cr1",  69}, {"cr2",  70}, {"cr3",  71},	\
  {"cr4",  72}, {"cr5",  73}, {"cr6",  74}, {"cr7",  75},	\
  {"cc",   68}, {"sp",    1}, {"toc",   2} }

/* How to renumber registers for dbx and gdb.  */

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* Text to write out after a CALL that may be replaced by glue code by
   the loader.  This depends on the AIX version.  */
#define RS6000_CALL_GLUE "cror 31,31,31"

/* This is how to output an assembler line defining a `double' constant.  */

#define ASM_OUTPUT_DOUBLE(FILE, VALUE)			\
  {							\
    long t[2];						\
    REAL_VALUE_TO_TARGET_DOUBLE ((VALUE), t);		\
    fprintf (FILE, "\t.long 0x%lx\n\t.long 0x%lx\n",	\
	     t[0] & 0xffffffffu, t[1] & 0xffffffffu);	\
  }

/* This is how to output an assembler line defining a `float' constant.  */

#define ASM_OUTPUT_FLOAT(FILE, VALUE)			\
  {							\
    long t;						\
    REAL_VALUE_TO_TARGET_SINGLE ((VALUE), t);		\
    fprintf (FILE, "\t.long 0x%lx\n", t & 0xffffffffu);	\
  }

/* This is how to output an assembler line defining an `int' constant.  */

#define ASM_OUTPUT_DOUBLE_INT(FILE,VALUE)				\
do {									\
  if (TARGET_32BIT)							\
    {									\
      assemble_integer (operand_subword ((VALUE), 0, 0, DImode),	\
                        UNITS_PER_WORD, 1);				\
      assemble_integer (operand_subword ((VALUE), 1, 0, DImode),	\
                        UNITS_PER_WORD, 1);				\
    }									\
  else									\
    {									\
      fputs ("\t.llong ", FILE);					\
      output_addr_const (FILE, (VALUE));				\
      putc ('\n', FILE);						\
    }									\
} while (0)

#define ASM_OUTPUT_INT(FILE,VALUE)  \
( fputs ("\t.long ", FILE),			\
  output_addr_const (FILE, (VALUE)),		\
  putc ('\n', FILE))

/* Likewise for `char' and `short' constants.  */

#define ASM_OUTPUT_SHORT(FILE,VALUE)  \
( fputs ("\t.short ", FILE),			\
  output_addr_const (FILE, (VALUE)),		\
  putc ('\n', FILE))

#define ASM_OUTPUT_CHAR(FILE,VALUE)  \
( fputs ("\t.byte ", FILE),			\
  output_addr_const (FILE, (VALUE)),		\
  putc ('\n', FILE))

/* This is how to output an assembler line for a numeric constant byte.  */

#define ASM_OUTPUT_BYTE(FILE,VALUE)  \
  fprintf (FILE, "\t.byte 0x%x\n", (VALUE))

/* This is used by the definition of ASM_OUTPUT_ADDR_ELT in defaults.h.  */
#define ASM_LONG (TARGET_32BIT ? ".long" : ".quad")

/* This is how to output an element of a case-vector that is relative.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL) \
  do { char buf[100];					\
       fputs ("\t.long ", FILE);			\
       ASM_GENERATE_INTERNAL_LABEL (buf, "L", VALUE);	\
       assemble_name (FILE, buf);			\
       putc ('-', FILE);				\
       ASM_GENERATE_INTERNAL_LABEL (buf, "L", REL);	\
       assemble_name (FILE, buf);			\
       putc ('\n', FILE);				\
     } while (0)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) != 0)			\
    fprintf (FILE, "\t.align %d\n", (LOG))

/* This says how to output an assembler line
   to define a local common symbol.
   Alignment cannot be specified, but we can try to maintain
   alignment after preceding TOC section if it was aligned
   for 64-bit mode.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)	\
  do { fputs (".lcomm ", (FILE));			\
       RS6000_OUTPUT_BASENAME ((FILE), (NAME));		\
       fprintf ((FILE), ",%d,%s\n", (TARGET_32BIT ? (SIZE) : (ROUNDED)), \
		xcoff_bss_section_name);		\
     } while (0)

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),	\
  sprintf ((OUTPUT), "%s.%d", (NAME), (LABELNO)))

/* Define the parentheses used to group arithmetic operations
   in assembler code.  */

#define ASM_OPEN_PAREN "("
#define ASM_CLOSE_PAREN ")"

/* Pick up the return address upon entry to a procedure. Used for
   dwarf2 unwind information.  This also enables the table driven
   mechanism.  */

#define INCOMING_RETURN_ADDR_RTX   gen_rtx_REG (Pmode, LINK_REGISTER_REGNUM)
#define DWARF_FRAME_RETURN_COLUMN  DWARF_FRAME_REGNUM (LINK_REGISTER_REGNUM)

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
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */

#define PRINT_OPERAND(FILE, X, CODE)  print_operand (FILE, X, CODE)

/* Define which CODE values are valid.  */

#define PRINT_OPERAND_PUNCT_VALID_P(CODE)  \
  ((CODE) == '.' || (CODE) == '*' || (CODE) == '$')

/* Print a memory address as an operand to reference that memory location.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR) print_operand_address (FILE, ADDR)

/* Define the codes that are matched by predicates in rs6000.c.  */

#define PREDICATE_CODES						\
  {"short_cint_operand", {CONST_INT}},				\
  {"u_short_cint_operand", {CONST_INT}},			\
  {"non_short_cint_operand", {CONST_INT}},			\
  {"gpc_reg_operand", {SUBREG, REG}},				\
  {"cc_reg_operand", {SUBREG, REG}},				\
  {"cc_reg_not_cr0_operand", {SUBREG, REG}},			\
  {"reg_or_short_operand", {SUBREG, REG, CONST_INT}}, 		\
  {"reg_or_neg_short_operand", {SUBREG, REG, CONST_INT}},	\
  {"reg_or_u_short_operand", {SUBREG, REG, CONST_INT}}, 	\
  {"reg_or_cint_operand", {SUBREG, REG, CONST_INT}}, 		\
  {"reg_or_u_cint_operand", {SUBREG, REG, CONST_INT, CONST_DOUBLE}}, \
  {"got_operand", {SYMBOL_REF, CONST, LABEL_REF}},		\
  {"got_no_const_operand", {SYMBOL_REF, LABEL_REF}},		\
  {"easy_fp_constant", {CONST_DOUBLE}},				\
  {"reg_or_mem_operand", {SUBREG, MEM, REG}},			\
  {"lwa_operand", {SUBREG, MEM, REG}},				\
  {"volatile_mem_operand", {MEM}},				\
  {"offsettable_mem_operand", {MEM}},				\
  {"mem_or_easy_const_operand", {SUBREG, MEM, CONST_DOUBLE}},	\
  {"add_operand", {SUBREG, REG, CONST_INT}},			\
  {"non_add_cint_operand", {CONST_INT}},			\
  {"and_operand", {SUBREG, REG, CONST_INT}},			\
  {"and64_operand", {SUBREG, REG, CONST_INT, CONST_DOUBLE}},	\
  {"logical_operand", {SUBREG, REG, CONST_INT}}, 		\
  {"logical_u_operand", {SUBREG, REG, CONST_INT, CONST_DOUBLE}}, \
  {"non_logical_cint_operand", {CONST_INT}},			\
  {"non_logical_u_cint_operand", {CONST_INT, CONST_DOUBLE}},	\
  {"mask_operand", {CONST_INT}},				\
  {"mask64_operand", {CONST_INT, CONST_DOUBLE}},		\
  {"count_register_operand", {REG}},				\
  {"xer_operand", {REG}},					\
  {"call_operand", {SYMBOL_REF, REG}},				\
  {"current_file_function_operand", {SYMBOL_REF}},		\
  {"input_operand", {SUBREG, MEM, REG, CONST_INT, 		\
		     CONST_DOUBLE, SYMBOL_REF}}, 		\
  {"load_multiple_operation", {PARALLEL}},			\
  {"store_multiple_operation", {PARALLEL}},			\
  {"branch_comparison_operator", {EQ, NE, LE, LT, GE,		\
				  GT, LEU, LTU, GEU, GTU,	\
				  UNORDERED, ORDERED,		\
				  UNEQ, LTGT,			\
				  UNGE, UNGT, UNLE, UNLT}},	\
  {"scc_comparison_operator", {EQ, NE, LE, LT, GE,		\
			       GT, LEU, LTU, GEU, GTU}},	\
  {"trap_comparison_operator", {EQ, NE, LE, LT, GE,		\
				GT, LEU, LTU, GEU, GTU}},

/* uncomment for disabling the corresponding default options */
/* #define  MACHINE_no_sched_interblock */
/* #define  MACHINE_no_sched_speculative */
/* #define  MACHINE_no_sched_speculative_load */

/* indicate that issue rate is defined for this machine
   (no need to use the default) */
#define ISSUE_RATE get_issue_rate ()

/* General flags.  */
extern int flag_pic;
extern int optimize;
extern int flag_expensive_optimizations;
extern int frame_pointer_needed;
