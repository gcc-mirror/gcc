/* Definitions of target machine for GNU compiler, for Sun SPARC.
   Copyright (C) 1987-2020 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com).
   64-bit SPARC-V9 support by Michael Tiemann, Jim Wilson, and Doug Evans,
   at Cygnus Support.

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

#include "config/vxworks-dummy.h"

/* Note that some other tm.h files include this one and then override
   whatever definitions are necessary.  */

#define TARGET_CPU_CPP_BUILTINS() sparc_target_macros ()

/* Target CPU versions for D.  */
#define TARGET_D_CPU_VERSIONS sparc_d_target_versions

/* Specify this in a cover file to provide bi-architecture (32/64) support.  */
/* #define SPARC_BI_ARCH */

/* Macro used later in this file to determine default architecture.  */
#define DEFAULT_ARCH32_P ((TARGET_DEFAULT & MASK_64BIT) == 0)

/* TARGET_ARCH{32,64} are the main macros to decide which of the two
   architectures to compile for.  We allow targets to choose compile time or
   runtime selection.  */
#ifdef IN_LIBGCC2
#if defined(__sparcv9) || defined(__arch64__)
#define TARGET_ARCH32 0
#else
#define TARGET_ARCH32 1
#endif /* sparc64 */
#else
#ifdef SPARC_BI_ARCH
#define TARGET_ARCH32 (!TARGET_64BIT)
#else
#define TARGET_ARCH32 (DEFAULT_ARCH32_P)
#endif /* SPARC_BI_ARCH */
#endif /* IN_LIBGCC2 */
#define TARGET_ARCH64 (!TARGET_ARCH32)

/* Code model selection in 64-bit environment.

   The machine mode used for addresses is 32-bit wide:

   TARGET_CM_32:     32-bit address space.
                     It is the code model used when generating 32-bit code.

   The machine mode used for addresses is 64-bit wide:

   TARGET_CM_MEDLOW: 32-bit address space.
                     The executable must be in the low 32 bits of memory.
                     This avoids generating %uhi and %ulo terms.  Programs
                     can be statically or dynamically linked.

   TARGET_CM_MEDMID: 44-bit address space.
                     The executable must be in the low 44 bits of memory,
                     and the %[hml]44 terms are used.  The text and data
                     segments have a maximum size of 2GB (31-bit span).
                     The maximum offset from any instruction to the label
                     _GLOBAL_OFFSET_TABLE_ is 2GB (31-bit span).

   TARGET_CM_MEDANY: 64-bit address space.
                     The text and data segments have a maximum size of 2GB
                     (31-bit span) and may be located anywhere in memory.
                     The maximum offset from any instruction to the label
                     _GLOBAL_OFFSET_TABLE_ is 2GB (31-bit span).

   TARGET_CM_EMBMEDANY: 64-bit address space.
                     The text and data segments have a maximum size of 2GB
                     (31-bit span) and may be located anywhere in memory.
                     The global register %g4 contains the start address of
                     the data segment.  Programs are statically linked and
                     PIC is not supported.

   Different code models are not supported in 32-bit environment.  */

#define TARGET_CM_MEDLOW    (sparc_code_model == CM_MEDLOW)
#define TARGET_CM_MEDMID    (sparc_code_model == CM_MEDMID)
#define TARGET_CM_MEDANY    (sparc_code_model == CM_MEDANY)
#define TARGET_CM_EMBMEDANY (sparc_code_model == CM_EMBMEDANY)

/* Default code model to be overridden in 64-bit environment.  */
#define SPARC_DEFAULT_CMODEL CM_32

/* Do not use the .note.GNU-stack convention by default.  */
#define NEED_INDICATE_EXEC_STACK 0

/* This is call-clobbered in the normal ABI, but is reserved in the
   home grown (aka upward compatible) embedded ABI.  */
#define EMBMEDANY_BASE_REG "%g4"

/* Values of TARGET_CPU_DEFAULT, set via -D in the Makefile,
   and specified by the user via --with-cpu=foo.
   This specifies the cpu implementation, not the architecture size.  */
/* Note that TARGET_CPU_v9 is assumed to start the list of 64-bit
   capable cpu's.  */
#define TARGET_CPU_sparc	0
#define TARGET_CPU_v7		0	/* alias */
#define TARGET_CPU_cypress	0       /* alias */
#define TARGET_CPU_v8		1	/* generic v8 implementation */
#define TARGET_CPU_supersparc	2
#define TARGET_CPU_hypersparc	3
#define TARGET_CPU_leon		4
#define TARGET_CPU_leon3	5
#define TARGET_CPU_leon3v7	6
#define TARGET_CPU_sparclite	7
#define TARGET_CPU_f930		7       /* alias */
#define TARGET_CPU_f934		7       /* alias */
#define TARGET_CPU_sparclite86x	8
#define TARGET_CPU_sparclet	9
#define TARGET_CPU_tsc701	9       /* alias */
#define TARGET_CPU_v9		10	/* generic v9 implementation */
#define TARGET_CPU_sparcv9	10	/* alias */
#define TARGET_CPU_sparc64	10	/* alias */
#define TARGET_CPU_ultrasparc	11
#define TARGET_CPU_ultrasparc3	12
#define TARGET_CPU_niagara	13
#define TARGET_CPU_niagara2	14
#define TARGET_CPU_niagara3	15
#define TARGET_CPU_niagara4	16
#define TARGET_CPU_niagara7	19
#define TARGET_CPU_m8		20

#if TARGET_CPU_DEFAULT == TARGET_CPU_v9 \
 || TARGET_CPU_DEFAULT == TARGET_CPU_ultrasparc \
 || TARGET_CPU_DEFAULT == TARGET_CPU_ultrasparc3 \
 || TARGET_CPU_DEFAULT == TARGET_CPU_niagara \
 || TARGET_CPU_DEFAULT == TARGET_CPU_niagara2 \
 || TARGET_CPU_DEFAULT == TARGET_CPU_niagara3 \
 || TARGET_CPU_DEFAULT == TARGET_CPU_niagara4 \
 || TARGET_CPU_DEFAULT == TARGET_CPU_niagara7 \
 || TARGET_CPU_DEFAULT == TARGET_CPU_m8

#define CPP_CPU32_DEFAULT_SPEC ""
#define ASM_CPU32_DEFAULT_SPEC ""

#if TARGET_CPU_DEFAULT == TARGET_CPU_v9
/* ??? What does Sun's CC pass?  */
#define CPP_CPU64_DEFAULT_SPEC "-D__sparc_v9__"
/* ??? It's not clear how other assemblers will handle this, so by default
   use GAS.  Sun's Solaris assembler recognizes -xarch=v8plus, but this case
   is handled in sol2.h.  */
#define ASM_CPU64_DEFAULT_SPEC "-Av9"
#endif
#if TARGET_CPU_DEFAULT == TARGET_CPU_ultrasparc
#define CPP_CPU64_DEFAULT_SPEC "-D__sparc_v9__"
#define ASM_CPU64_DEFAULT_SPEC "-Av9a"
#endif
#if TARGET_CPU_DEFAULT == TARGET_CPU_ultrasparc3
#define CPP_CPU64_DEFAULT_SPEC "-D__sparc_v9__"
#define ASM_CPU64_DEFAULT_SPEC "-Av9b"
#endif
#if TARGET_CPU_DEFAULT == TARGET_CPU_niagara
#define CPP_CPU64_DEFAULT_SPEC "-D__sparc_v9__"
#define ASM_CPU64_DEFAULT_SPEC "-Av9b"
#endif
#if TARGET_CPU_DEFAULT == TARGET_CPU_niagara2
#define CPP_CPU64_DEFAULT_SPEC "-D__sparc_v9__"
#define ASM_CPU64_DEFAULT_SPEC "-Av9b"
#endif
#if TARGET_CPU_DEFAULT == TARGET_CPU_niagara3
#define CPP_CPU64_DEFAULT_SPEC "-D__sparc_v9__"
#define ASM_CPU64_DEFAULT_SPEC "-Av9" AS_NIAGARA3_FLAG
#endif
#if TARGET_CPU_DEFAULT == TARGET_CPU_niagara4
#define CPP_CPU64_DEFAULT_SPEC "-D__sparc_v9__"
#define ASM_CPU64_DEFAULT_SPEC AS_NIAGARA4_FLAG
#endif
#if TARGET_CPU_DEFAULT == TARGET_CPU_niagara7
#define CPP_CPU64_DEFAULT_SPEC "-D__sparc_v9__"
#define ASM_CPU64_DEFAULT_SPEC AS_NIAGARA7_FLAG
#endif
#if TARGET_CPU_DEFAULT == TARGET_CPU_m8
#define CPP_CPU64_DEFAULT_SPEC "-D__sparc_v9__"
#define ASM_CPU64_DEFAULT_SPEC AS_M8_FLAG
#endif

#else

#define CPP_CPU64_DEFAULT_SPEC ""
#define ASM_CPU64_DEFAULT_SPEC ""

#if TARGET_CPU_DEFAULT == TARGET_CPU_sparc \
 || TARGET_CPU_DEFAULT == TARGET_CPU_v8
#define CPP_CPU32_DEFAULT_SPEC ""
#define ASM_CPU32_DEFAULT_SPEC ""
#endif

#if TARGET_CPU_DEFAULT == TARGET_CPU_sparclet
#define CPP_CPU32_DEFAULT_SPEC "-D__sparclet__"
#define ASM_CPU32_DEFAULT_SPEC "-Asparclet"
#endif

#if TARGET_CPU_DEFAULT == TARGET_CPU_sparclite
#define CPP_CPU32_DEFAULT_SPEC "-D__sparclite__"
#define ASM_CPU32_DEFAULT_SPEC "-Asparclite"
#endif

#if TARGET_CPU_DEFAULT == TARGET_CPU_sparclite86x
#define CPP_CPU32_DEFAULT_SPEC "-D__sparclite86x__"
#define ASM_CPU32_DEFAULT_SPEC "-Asparclite"
#endif

#if TARGET_CPU_DEFAULT == TARGET_CPU_supersparc
#define CPP_CPU32_DEFAULT_SPEC "-D__supersparc__ -D__sparc_v8__"
#define ASM_CPU32_DEFAULT_SPEC ""
#endif

#if TARGET_CPU_DEFAULT == TARGET_CPU_hypersparc
#define CPP_CPU32_DEFAULT_SPEC "-D__hypersparc__ -D__sparc_v8__"
#define ASM_CPU32_DEFAULT_SPEC ""
#endif

#if TARGET_CPU_DEFAULT == TARGET_CPU_leon \
 || TARGET_CPU_DEFAULT == TARGET_CPU_leon3
#define CPP_CPU32_DEFAULT_SPEC "-D__leon__ -D__sparc_v8__"
#define ASM_CPU32_DEFAULT_SPEC AS_LEON_FLAG
#endif

#if TARGET_CPU_DEFAULT == TARGET_CPU_leon3v7
#define CPP_CPU32_DEFAULT_SPEC "-D__leon__"
#define ASM_CPU32_DEFAULT_SPEC AS_LEONV7_FLAG
#endif

#endif

#if !defined(CPP_CPU32_DEFAULT_SPEC) || !defined(CPP_CPU64_DEFAULT_SPEC)
 #error Unrecognized value in TARGET_CPU_DEFAULT.
#endif

#ifdef SPARC_BI_ARCH

#define CPP_CPU_DEFAULT_SPEC \
(DEFAULT_ARCH32_P ? "\
%{m64:" CPP_CPU64_DEFAULT_SPEC "} \
%{!m64:" CPP_CPU32_DEFAULT_SPEC "} \
" : "\
%{m32:" CPP_CPU32_DEFAULT_SPEC "} \
%{!m32:" CPP_CPU64_DEFAULT_SPEC "} \
")
#define ASM_CPU_DEFAULT_SPEC \
(DEFAULT_ARCH32_P ? "\
%{m64:" ASM_CPU64_DEFAULT_SPEC "} \
%{!m64:" ASM_CPU32_DEFAULT_SPEC "} \
" : "\
%{m32:" ASM_CPU32_DEFAULT_SPEC "} \
%{!m32:" ASM_CPU64_DEFAULT_SPEC "} \
")

#else /* !SPARC_BI_ARCH */

#define CPP_CPU_DEFAULT_SPEC (DEFAULT_ARCH32_P ? CPP_CPU32_DEFAULT_SPEC : CPP_CPU64_DEFAULT_SPEC)
#define ASM_CPU_DEFAULT_SPEC (DEFAULT_ARCH32_P ? ASM_CPU32_DEFAULT_SPEC : ASM_CPU64_DEFAULT_SPEC)

#endif /* !SPARC_BI_ARCH */

/* Define macros to distinguish architectures.  */

/* Common CPP definitions used by CPP_SPEC amongst the various targets
   for handling -mcpu=xxx switches.  */
#define CPP_CPU_SPEC "\
%{mcpu=sparclet:-D__sparclet__} %{mcpu=tsc701:-D__sparclet__} \
%{mcpu=sparclite:-D__sparclite__} \
%{mcpu=f930:-D__sparclite__} %{mcpu=f934:-D__sparclite__} \
%{mcpu=sparclite86x:-D__sparclite86x__} \
%{mcpu=v8:-D__sparc_v8__} \
%{mcpu=supersparc:-D__supersparc__ -D__sparc_v8__} \
%{mcpu=hypersparc:-D__hypersparc__ -D__sparc_v8__} \
%{mcpu=leon:-D__leon__ -D__sparc_v8__} \
%{mcpu=leon3:-D__leon__ -D__sparc_v8__} \
%{mcpu=leon3v7:-D__leon__} \
%{mcpu=v9:-D__sparc_v9__} \
%{mcpu=ultrasparc:-D__sparc_v9__} \
%{mcpu=ultrasparc3:-D__sparc_v9__} \
%{mcpu=niagara:-D__sparc_v9__} \
%{mcpu=niagara2:-D__sparc_v9__} \
%{mcpu=niagara3:-D__sparc_v9__} \
%{mcpu=niagara4:-D__sparc_v9__} \
%{mcpu=niagara7:-D__sparc_v9__} \
%{mcpu=m8:-D__sparc_v9__} \
%{!mcpu*:%(cpp_cpu_default)} \
"
#define CPP_ARCH32_SPEC ""
#define CPP_ARCH64_SPEC "-D__arch64__"

#define CPP_ARCH_DEFAULT_SPEC \
(DEFAULT_ARCH32_P ? CPP_ARCH32_SPEC : CPP_ARCH64_SPEC)

#define CPP_ARCH_SPEC "\
%{m32:%(cpp_arch32)} \
%{m64:%(cpp_arch64)} \
%{!m32:%{!m64:%(cpp_arch_default)}} \
"

/* Macros to distinguish the endianness, window model and FP support.  */
#define CPP_OTHER_SPEC "\
%{mflat:-D_FLAT} \
%{msoft-float:-D_SOFT_FLOAT} \
"

/* Macros to distinguish the particular subtarget.  */
#define CPP_SUBTARGET_SPEC ""

#define CPP_SPEC \
  "%(cpp_cpu) %(cpp_arch) %(cpp_endian) %(cpp_other) %(cpp_subtarget)"

/* This used to translate -dalign to -malign, but that is no good
   because it can't turn off the usual meaning of making debugging dumps.  */

#define CC1_SPEC ""

/* Override in target specific files.  */
#define ASM_CPU_SPEC "\
%{mcpu=sparclet:-Asparclet} %{mcpu=tsc701:-Asparclet} \
%{mcpu=sparclite:-Asparclite} \
%{mcpu=sparclite86x:-Asparclite} \
%{mcpu=f930:-Asparclite} %{mcpu=f934:-Asparclite} \
%{mcpu=v8:-Av8} \
%{mcpu=supersparc:-Av8} \
%{mcpu=hypersparc:-Av8} \
%{mcpu=leon:" AS_LEON_FLAG "} \
%{mcpu=leon3:" AS_LEON_FLAG "} \
%{mcpu=leon3v7:" AS_LEONV7_FLAG "} \
%{mv8plus:-Av8plus} \
%{mcpu=v9:-Av9} \
%{mcpu=ultrasparc:%{!mv8plus:-Av9a}} \
%{mcpu=ultrasparc3:%{!mv8plus:-Av9b}} \
%{mcpu=niagara:%{!mv8plus:-Av9b}} \
%{mcpu=niagara2:%{!mv8plus:-Av9b}} \
%{mcpu=niagara3:%{!mv8plus:-Av9" AS_NIAGARA3_FLAG "}} \
%{mcpu=niagara4:%{!mv8plus:" AS_NIAGARA4_FLAG "}} \
%{mcpu=niagara7:%{!mv8plus:" AS_NIAGARA7_FLAG "}} \
%{mcpu=m8:%{!mv8plus:" AS_M8_FLAG "}} \
%{!mcpu*:%(asm_cpu_default)} \
"

/* Word size selection, among other things.
   This is what GAS uses.  Add %(asm_arch) to ASM_SPEC to enable.  */

#define ASM_ARCH32_SPEC "-32"
#define ASM_ARCH64_SPEC "-64 -no-undeclared-regs"
#define ASM_ARCH_DEFAULT_SPEC \
(DEFAULT_ARCH32_P ? ASM_ARCH32_SPEC : ASM_ARCH64_SPEC)

#define ASM_ARCH_SPEC "\
%{m32:%(asm_arch32)} \
%{m64:%(asm_arch64)} \
%{!m32:%{!m64:%(asm_arch_default)}} \
"

#ifdef HAVE_AS_RELAX_OPTION
#define ASM_RELAX_SPEC "%{!mno-relax:-relax}"
#else
#define ASM_RELAX_SPEC ""
#endif

/* Special flags to the Sun-4 assembler when using pipe for input.  */

#define ASM_SPEC "\
%{!pg:%{!p:%{" FPIE_OR_FPIC_SPEC ":-k}}} %{keep-local-as-symbols:-L} \
%(asm_cpu) %(asm_relax)"

/* This macro defines names of additional specifications to put in the specs
   that can be used in various specifications like CC1_SPEC.  Its definition
   is an initializer with a subgrouping for each command option.

   Each subgrouping contains a string constant, that defines the
   specification name, and a string constant that used by the GCC driver
   program.

   Do not define this macro if it does not need to do anything.  */

#define EXTRA_SPECS \
  { "cpp_cpu",		CPP_CPU_SPEC },		\
  { "cpp_cpu_default",	CPP_CPU_DEFAULT_SPEC },	\
  { "cpp_arch32",	CPP_ARCH32_SPEC },	\
  { "cpp_arch64",	CPP_ARCH64_SPEC },	\
  { "cpp_arch_default",	CPP_ARCH_DEFAULT_SPEC },\
  { "cpp_arch",		CPP_ARCH_SPEC },	\
  { "cpp_other",	CPP_OTHER_SPEC },	\
  { "cpp_subtarget",	CPP_SUBTARGET_SPEC },	\
  { "asm_cpu",		ASM_CPU_SPEC },		\
  { "asm_cpu_default",	ASM_CPU_DEFAULT_SPEC },	\
  { "asm_arch32",	ASM_ARCH32_SPEC },	\
  { "asm_arch64",	ASM_ARCH64_SPEC },	\
  { "asm_relax",	ASM_RELAX_SPEC },	\
  { "asm_arch_default",	ASM_ARCH_DEFAULT_SPEC },\
  { "asm_arch",		ASM_ARCH_SPEC },	\
  SUBTARGET_EXTRA_SPECS

#define SUBTARGET_EXTRA_SPECS

/* Because libgcc can generate references back to libc (via .umul etc.) we have
   to list libc again after the second libgcc.  */
#define LINK_GCC_C_SEQUENCE_SPEC "%G %{!nolibc:%L} %G %{!nolibc:%L}"


#define PTRDIFF_TYPE (TARGET_ARCH64 ? "long int" : "int")
#define SIZE_TYPE (TARGET_ARCH64 ? "long unsigned int" : "unsigned int")

/* ??? This should be 32 bits for v9 but what can we do?  */
#define WCHAR_TYPE "short unsigned int"
#define WCHAR_TYPE_SIZE 16

/* Mask of all CPU selection flags.  */
#define MASK_ISA						\
  (MASK_SPARCLITE + MASK_SPARCLET + MASK_LEON + MASK_LEON3	\
   + MASK_V8 + MASK_V9 + MASK_DEPRECATED_V8_INSNS)

/* Mask of all CPU feature flags.  */
#define MASK_FEATURES						\
  (MASK_FPU + MASK_HARD_QUAD + MASK_VIS + MASK_VIS2 + MASK_VIS3	\
   + MASK_VIS4 + MASK_CBCOND + MASK_FMAF + MASK_FSMULD		\
   + MASK_POPC + MASK_SUBXC)
 
/* TARGET_HARD_MUL: Use 32-bit hardware multiply instructions but not %y.  */
#define TARGET_HARD_MUL				\
  (TARGET_SPARCLITE || TARGET_SPARCLET		\
   || TARGET_V8 || TARGET_DEPRECATED_V8_INSNS)

/* TARGET_HARD_MUL32: Use 32-bit hardware multiply instructions with %y
   to get high 32 bits.  False in 64-bit or V8+ because multiply stores
   a 64-bit result in a register.  */
#define TARGET_HARD_MUL32 \
  (TARGET_HARD_MUL && TARGET_ARCH32 && !TARGET_V8PLUS)

/* MASK_APP_REGS must always be the default because that's what
   FIXED_REGISTERS is set to and -ffixed- is processed before
   TARGET_CONDITIONAL_REGISTER_USAGE is called (where we process
   -mno-app-regs).  */
#define TARGET_DEFAULT (MASK_APP_REGS + MASK_FPU)

/* Recast the cpu class to be the cpu attribute.
   Every file includes us, but not every file includes insn-attr.h.  */
#define sparc_cpu_attr ((enum attr_cpu) sparc_cpu)

/* Support for a compile-time default CPU, et cetera.  The rules are:
   --with-cpu is ignored if -mcpu is specified.
   --with-tune is ignored if -mtune is specified.
   --with-float is ignored if -mhard-float, -msoft-float, -mfpu, or -mno-fpu
     are specified.  */
#define OPTION_DEFAULT_SPECS \
  {"cpu", "%{!mcpu=*:-mcpu=%(VALUE)}" }, \
  {"tune", "%{!mtune=*:-mtune=%(VALUE)}" }, \
  {"float", "%{!msoft-float:%{!mhard-float:%{!mfpu:%{!mno-fpu:-m%(VALUE)-float}}}}" }

/* target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */
#define BITS_BIG_ENDIAN 1

/* Define this if most significant byte of a word is the lowest numbered.  */
#define BYTES_BIG_ENDIAN 1

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
#define WORDS_BIG_ENDIAN 1

#define MAX_BITS_PER_WORD	64

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD		(TARGET_ARCH64 ? 8 : 4)
#ifdef IN_LIBGCC2
#define MIN_UNITS_PER_WORD	UNITS_PER_WORD
#else
#define MIN_UNITS_PER_WORD	4
#endif

/* Now define the sizes of the C data types.  */
#define SHORT_TYPE_SIZE		16
#define INT_TYPE_SIZE		32
#define LONG_TYPE_SIZE		(TARGET_ARCH64 ? 64 : 32)
#define LONG_LONG_TYPE_SIZE	64
#define FLOAT_TYPE_SIZE		32
#define DOUBLE_TYPE_SIZE	64

/* LONG_DOUBLE_TYPE_SIZE is defined per OS even though the
   SPARC ABI says that it is 128-bit wide.  */
/* #define LONG_DOUBLE_TYPE_SIZE	128 */

/* The widest floating-point format really supported by the hardware.  */
#define WIDEST_HARDWARE_FP_SIZE 64

/* Width in bits of a pointer.  This is the size of ptr_mode.  */
#define POINTER_SIZE (TARGET_PTR64 ? 64 : 32)

/* This is the machine mode used for addresses.  */
#define Pmode (TARGET_ARCH64 ? DImode : SImode)

/* If we have to extend pointers (only when TARGET_ARCH64 and not
   TARGET_PTR64), we want to do it unsigned.   This macro does nothing
   if ptr_mode and Pmode are the same.  */
#define POINTERS_EXTEND_UNSIGNED 1

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY (TARGET_ARCH64 ? 64 : 32)

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
/* FIXME, this is wrong when TARGET_ARCH64 and TARGET_STACK_BIAS, because
   then %sp+2047 is 128-bit aligned so %sp is really only byte-aligned.  */
#define STACK_BOUNDARY (TARGET_ARCH64 ? 128 : 64)

/* Temporary hack until the FIXME above is fixed.  */
#define SPARC_STACK_BOUNDARY_HACK (TARGET_ARCH64 && TARGET_STACK_BIAS)

/* ALIGN FRAMES on double word boundaries */
#define SPARC_STACK_ALIGN(LOC) ROUND_UP ((LOC), UNITS_PER_WORD * 2)

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 32

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY (TARGET_ARCH64 ? 64 : 32)

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* A bit-field declared as `int' forces `int' alignment for the struct.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT (TARGET_ARCH64 ? 128 : 64)

/* The best alignment to use in cases where we have a choice.  */
#define FASTEST_ALIGNMENT 64

/* Define this macro as an expression for the alignment of a structure
   (given by STRUCT as a tree node) if the alignment computed in the
   usual way is COMPUTED and the alignment explicitly specified was
   SPECIFIED.

   The default is to use SPECIFIED if it is larger; otherwise, use
   the smaller of COMPUTED and `BIGGEST_ALIGNMENT' */
#define ROUND_TYPE_ALIGN(STRUCT, COMPUTED, SPECIFIED)	\
 (TARGET_FASTER_STRUCTS ?				\
  ((TREE_CODE (STRUCT) == RECORD_TYPE			\
    || TREE_CODE (STRUCT) == UNION_TYPE                 \
    || TREE_CODE (STRUCT) == QUAL_UNION_TYPE)           \
   && TYPE_FIELDS (STRUCT) != 0                         \
     ? MAX (MAX ((COMPUTED), (SPECIFIED)), BIGGEST_ALIGNMENT) \
     : MAX ((COMPUTED), (SPECIFIED)))			\
   :  MAX ((COMPUTED), (SPECIFIED)))

/* An integer expression for the size in bits of the largest integer machine
   mode that should actually be used.  We allow pairs of registers.  */
#define MAX_FIXED_MODE_SIZE GET_MODE_BITSIZE (TARGET_ARCH64 ? TImode : DImode)

/* We need 2 words, so we can save the stack pointer and the return register
   of the function containing a non-local goto target.  */
#define STACK_SAVEAREA_MODE(LEVEL) \
  ((LEVEL) == SAVE_NONLOCAL ? (TARGET_ARCH64 ? TImode : DImode) : Pmode)

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN))

/* Make local arrays of chars word-aligned for the same reasons.  */
#define LOCAL_ALIGNMENT(TYPE, ALIGN) DATA_ALIGNMENT (TYPE, ALIGN)

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* Things that must be doubleword aligned cannot go in the text section,
   because the linker fails to align the text section enough!
   Put them in the data section.  This macro is only used in this file.  */
#define MAX_TEXT_ALIGN 32

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   SPARC has 32 integer registers and 32 floating point registers.
   64-bit SPARC has 32 additional fp regs, but the odd numbered ones are not
   accessible.  We still account for them to simplify register computations
   (e.g.: in CLASS_MAX_NREGS).  There are also 4 fp condition code registers, so
   32+32+32+4 == 100.
   Register 100 is used as the integer condition code register.
   Register 101 is used as the soft frame pointer register.
   Register 102 is used as the general status register by VIS instructions.  */

#define FIRST_PSEUDO_REGISTER 103

#define SPARC_FIRST_INT_REG     0
#define SPARC_LAST_INT_REG     31
#define SPARC_FIRST_FP_REG     32
/* Additional V9 fp regs.  */
#define SPARC_FIRST_V9_FP_REG  64
#define SPARC_LAST_V9_FP_REG   95
/* V9 %fcc[0123].  V8 uses (figuratively) %fcc0.  */
#define SPARC_FIRST_V9_FCC_REG 96
#define SPARC_LAST_V9_FCC_REG  99
/* V8 fcc reg.  */
#define SPARC_FCC_REG 96
/* Integer CC reg.  We don't distinguish %icc from %xcc.  */
#define SPARC_ICC_REG 100
#define SPARC_GSR_REG 102

/* Nonzero if REGNO is an fp reg.  */
#define SPARC_FP_REG_P(REGNO) \
((REGNO) >= SPARC_FIRST_FP_REG && (REGNO) <= SPARC_LAST_V9_FP_REG)

/* Nonzero if REGNO is an int reg.  */
#define SPARC_INT_REG_P(REGNO) \
(((unsigned) (REGNO)) <= SPARC_LAST_INT_REG)

/* Argument passing regs.  */
#define SPARC_OUTGOING_INT_ARG_FIRST 8
#define SPARC_INCOMING_INT_ARG_FIRST (TARGET_FLAT ? 8 : 24)
#define SPARC_FP_ARG_FIRST           32

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.

   On non-v9 systems:
   g1 is free to use as temporary.
   g2-g4 are reserved for applications.  Gcc normally uses them as
   temporaries, but this can be disabled via the -mno-app-regs option.
   g5 through g7 are reserved for the operating system.

   On v9 systems:
   g1,g5 are free to use as temporaries, and are free to use between calls
   if the call is to an external function via the PLT.
   g4 is free to use as a temporary in the non-embedded case.
   g4 is reserved in the embedded case.
   g2-g3 are reserved for applications.  Gcc normally uses them as
   temporaries, but this can be disabled via the -mno-app-regs option.
   g6-g7 are reserved for the operating system (or application in
   embedded case).
   ??? Register 1 is used as a temporary by the 64 bit sethi pattern, so must
   currently be a fixed register until this pattern is rewritten.
   Register 1 is also used when restoring call-preserved registers in large
   stack frames.

   Registers fixed in arch32 and not arch64 (or vice-versa) are marked in
   TARGET_CONDITIONAL_REGISTER_USAGE in order to properly handle -ffixed-.
*/

#define FIXED_REGISTERS  \
 {1, 0, 2, 2, 2, 2, 1, 1,	\
  0, 0, 0, 0, 0, 0, 1, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 1,	\
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
  0, 0, 0, 0, 1, 1, 1}

/* 1 for registers not available across function calls.
   Unlike the above, this need not include the FIXED_REGISTERS, but any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */

#define CALL_REALLY_USED_REGISTERS  \
 {1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
				\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
				\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
				\
  1, 1, 1, 1, 1, 1, 1}

/* Due to the ARCH64 discrepancy above we must override this next
   macro too.  */
#define REGMODE_NATURAL_SIZE(MODE) sparc_regmode_natural_size (MODE)

/* Value is 1 if it is OK to rename a hard register FROM to another hard
   register TO.  We cannot rename %g1 as it may be used before the save
   register window instruction in the prologue.  */
#define HARD_REGNO_RENAME_OK(FROM, TO) ((FROM) != 1)

/* Select a register mode required for caller save of hard regno REGNO.
   Contrary to what is documented, the default is not the smallest suitable
   mode but the largest suitable mode for the given (REGNO, NREGS) pair and
   it quickly creates paradoxical subregs that can be problematic.  */
#define HARD_REGNO_CALLER_SAVE_MODE(REGNO, NREGS, MODE) \
  ((MODE) == VOIDmode ? choose_hard_reg_mode (REGNO, NREGS, NULL) : (MODE))

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 14

/* The stack bias (amount by which the hardware register is offset by).  */
#define SPARC_STACK_BIAS ((TARGET_ARCH64 && TARGET_STACK_BIAS) ? 2047 : 0)

/* Actual top-of-stack address is 92/176 greater than the contents of the
   stack pointer register for !v9/v9.  That is:
   - !v9: 64 bytes for the in and local registers, 4 bytes for structure return
     address, and 6*4 bytes for the 6 register parameters.
   - v9: 128 bytes for the in and local registers + 6*8 bytes for the integer
     parameter regs.  */
#define STACK_POINTER_OFFSET (FIRST_PARM_OFFSET(0) + SPARC_STACK_BIAS)

/* Base register for access to local variables of the function.  */
#define HARD_FRAME_POINTER_REGNUM 30

/* The soft frame pointer does not have the stack bias applied.  */
#define FRAME_POINTER_REGNUM 101

#define INIT_EXPANDERS							 \
  do {									 \
    if (crtl->emit.regno_pointer_align)					 \
      {									 \
	/* The biased stack pointer is only aligned on BITS_PER_UNIT.  */\
	if (SPARC_STACK_BIAS)						 \
	  {								 \
	    REGNO_POINTER_ALIGN (STACK_POINTER_REGNUM)			 \
	      = BITS_PER_UNIT;	 					 \
	    REGNO_POINTER_ALIGN (HARD_FRAME_POINTER_REGNUM)		 \
	      = BITS_PER_UNIT;						 \
	  }								 \
									 \
	/* In 32-bit mode, not everything is double-word aligned.  */	 \
	if (TARGET_ARCH32)						 \
	  {								 \
	    REGNO_POINTER_ALIGN (VIRTUAL_INCOMING_ARGS_REGNUM)		 \
	      = BITS_PER_WORD;						 \
	    REGNO_POINTER_ALIGN (VIRTUAL_STACK_DYNAMIC_REGNUM)		 \
	      = BITS_PER_WORD;						 \
	    REGNO_POINTER_ALIGN (VIRTUAL_OUTGOING_ARGS_REGNUM)		 \
	      = BITS_PER_WORD;						 \
	  }								 \
      }									 \
  } while (0)

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM FRAME_POINTER_REGNUM

/* Register in which static-chain is passed to a function.  This must
   not be a register used by the prologue.  */
#define STATIC_CHAIN_REGNUM (TARGET_ARCH64 ? 5 : 2)

/* Register which holds the global offset table, if any.  */
#define GLOBAL_OFFSET_TABLE_REGNUM 23

/* Register which holds offset table for position-independent data references.
   The original SPARC ABI imposes no requirement on the choice of the register
   so we use a pseudo-register to make sure it is properly saved and restored
   around calls to setjmp.  Now the ABI of VxWorks RTP makes it live on entry
   to PLT entries so we use the canonical GOT register in this case.  */
#define PIC_OFFSET_TABLE_REGNUM \
  (TARGET_VXWORKS_RTP && flag_pic ? GLOBAL_OFFSET_TABLE_REGNUM : INVALID_REGNUM)

/* Pick a default value we can notice from override_options:
   !v9: Default is on.
   v9: Default is off.
   Originally it was -1, but later on the container of options changed to
   unsigned byte, so we decided to pick 127 as default value, which does
   reflect an undefined default value in case of 0/1.  */
#define DEFAULT_PCC_STRUCT_RETURN 127

/* Functions which return large structures get the address
   to place the wanted value at offset 64 from the frame.
   Must reserve 64 bytes for the in and local registers.
   v9: Functions which return large structures get the address to place the
   wanted value from an invisible first argument.  */
#define STRUCT_VALUE_OFFSET 64

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

/* The SPARC has various kinds of registers: general, floating point,
   and condition codes [well, it has others as well, but none that we
   care directly about].

   For v9 we must distinguish between the upper and lower floating point
   registers because the upper ones can't hold SFmode values.
   TARGET_HARD_REGNO_MODE_OK won't help here because reload assumes that
   register(s) satisfying a group need for a class will also satisfy a
   single need for that class.  EXTRA_FP_REGS is a bit of a misnomer as
   it covers all 64 fp regs.

   It is important that one class contains all the general and all the standard
   fp regs.  Otherwise find_reg() won't properly allocate int regs for moves,
   because reg_class_record() will bias the selection in favor of fp regs,
   because reg_class_subunion[GENERAL_REGS][FP_REGS] will yield FP_REGS,
   because FP_REGS > GENERAL_REGS.

   It is also important that one class contain all the general and all
   the fp regs.  Otherwise when spilling a DFmode reg, it may be from
   EXTRA_FP_REGS but find_reloads() may use class
   GENERAL_OR_FP_REGS. This will cause allocate_reload_reg() to die
   because the compiler thinks it doesn't have a spill reg when in
   fact it does.

   v9 also has 4 floating point condition code registers.  Since we don't
   have a class that is the union of FPCC_REGS with either of the others,
   it is important that it appear first.  Otherwise the compiler will die
   trying to compile _fixunsdfsi because fix_truncdfsi2 won't match its
   constraints.  */

enum reg_class { NO_REGS, FPCC_REGS, I64_REGS, GENERAL_REGS, FP_REGS,
		 EXTRA_FP_REGS, GENERAL_OR_FP_REGS, GENERAL_OR_EXTRA_FP_REGS,
		 ALL_REGS, LIM_REG_CLASSES };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.  */

#define REG_CLASS_NAMES \
  { "NO_REGS", "FPCC_REGS", "I64_REGS", "GENERAL_REGS", "FP_REGS",	\
     "EXTRA_FP_REGS", "GENERAL_OR_FP_REGS", "GENERAL_OR_EXTRA_FP_REGS",	\
     "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS				\
  {{0, 0, 0, 0},	/* NO_REGS */			\
   {0, 0, 0, 0xf},	/* FPCC_REGS */			\
   {0xffff, 0, 0, 0},	/* I64_REGS */			\
   {-1, 0, 0, 0x20},	/* GENERAL_REGS */		\
   {0, -1, 0, 0},	/* FP_REGS */			\
   {0, -1, -1, 0},	/* EXTRA_FP_REGS */		\
   {-1, -1, 0, 0x20},	/* GENERAL_OR_FP_REGS */	\
   {-1, -1, -1, 0x20},	/* GENERAL_OR_EXTRA_FP_REGS */	\
   {-1, -1, -1, 0x7f}}	/* ALL_REGS */

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

extern enum reg_class sparc_regno_reg_class[FIRST_PSEUDO_REGISTER];

#define REGNO_REG_CLASS(REGNO) sparc_regno_reg_class[(REGNO)]

/* This is the order in which to allocate registers normally.

   We put %f0-%f7 last among the float registers, so as to make it more
   likely that a pseudo-register which dies in the float return register
   area will get allocated to the float return register, thus saving a move
   instruction at the end of the function.

   Similarly for integer return value registers.

   We know in this case that we will not end up with a leaf function.

   The register allocator is given the global and out registers first
   because these registers are call clobbered and thus less useful to
   global register allocation.

   Next we list the local and in registers.  They are not call clobbered
   and thus very useful for global register allocation.  We list the input
   registers before the locals so that it is more likely the incoming
   arguments received in those registers can just stay there and not be
   reloaded.  */

#define REG_ALLOC_ORDER \
{ 1, 2, 3, 4, 5, 6, 7,			/* %g1-%g7 */	\
  13, 12, 11, 10, 9, 8, 		/* %o5-%o0 */	\
  15,					/* %o7 */	\
  16, 17, 18, 19, 20, 21, 22, 23,	/* %l0-%l7 */ 	\
  29, 28, 27, 26, 25, 24, 31,		/* %i5-%i0,%i7 */\
  40, 41, 42, 43, 44, 45, 46, 47,	/* %f8-%f15 */  \
  48, 49, 50, 51, 52, 53, 54, 55,	/* %f16-%f23 */ \
  56, 57, 58, 59, 60, 61, 62, 63,	/* %f24-%f31 */ \
  64, 65, 66, 67, 68, 69, 70, 71,	/* %f32-%f39 */ \
  72, 73, 74, 75, 76, 77, 78, 79,	/* %f40-%f47 */ \
  80, 81, 82, 83, 84, 85, 86, 87,	/* %f48-%f55 */ \
  88, 89, 90, 91, 92, 93, 94, 95,	/* %f56-%f63 */ \
  39, 38, 37, 36, 35, 34, 33, 32,	/* %f7-%f0 */   \
  96, 97, 98, 99,			/* %fcc0-3 */   \
  100, 0, 14, 30, 101, 102 }		/* %icc, %g0, %o6, %i6, %sfp, %gsr */

/* This is the order in which to allocate registers for
   leaf functions.  If all registers can fit in the global and
   output registers, then we have the possibility of having a leaf
   function.

   The macro actually mentioned the input registers first,
   because they get renumbered into the output registers once
   we know really do have a leaf function.

   To be more precise, this register allocation order is used
   when %o7 is found to not be clobbered right before register
   allocation.  Normally, the reason %o7 would be clobbered is
   due to a call which could not be transformed into a sibling
   call.

   As a consequence, it is possible to use the leaf register
   allocation order and not end up with a leaf function.  We will
   not get suboptimal register allocation in that case because by
   definition of being potentially leaf, there were no function
   calls.  Therefore, allocation order within the local register
   window is not critical like it is when we do have function calls.  */

#define REG_LEAF_ALLOC_ORDER \
{ 1, 2, 3, 4, 5, 6, 7, 			/* %g1-%g7 */	\
  29, 28, 27, 26, 25, 24,		/* %i5-%i0 */	\
  15,					/* %o7 */	\
  13, 12, 11, 10, 9, 8,			/* %o5-%o0 */	\
  16, 17, 18, 19, 20, 21, 22, 23,	/* %l0-%l7 */	\
  40, 41, 42, 43, 44, 45, 46, 47,	/* %f8-%f15 */	\
  48, 49, 50, 51, 52, 53, 54, 55,	/* %f16-%f23 */	\
  56, 57, 58, 59, 60, 61, 62, 63,	/* %f24-%f31 */	\
  64, 65, 66, 67, 68, 69, 70, 71,	/* %f32-%f39 */	\
  72, 73, 74, 75, 76, 77, 78, 79,	/* %f40-%f47 */	\
  80, 81, 82, 83, 84, 85, 86, 87,	/* %f48-%f55 */	\
  88, 89, 90, 91, 92, 93, 94, 95,	/* %f56-%f63 */	\
  39, 38, 37, 36, 35, 34, 33, 32,	/* %f7-%f0 */	\
  96, 97, 98, 99,			/* %fcc0-3 */	\
  100, 0, 14, 30, 31, 101, 102 }	/* %icc, %g0, %o6, %i6, %i7, %sfp, %gsr */

#define ADJUST_REG_ALLOC_ORDER order_regs_for_local_alloc ()

extern char sparc_leaf_regs[];
#define LEAF_REGISTERS sparc_leaf_regs

extern char leaf_reg_remap[];
#define LEAF_REG_REMAP(REGNO) (leaf_reg_remap[REGNO])

/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS GENERAL_REGS
#define BASE_REG_CLASS GENERAL_REGS

/* Local macro to handle the two v9 classes of FP regs.  */
#define FP_REG_CLASS_P(CLASS) ((CLASS) == FP_REGS || (CLASS) == EXTRA_FP_REGS)

/* Predicate for 2-bit and 5-bit unsigned constants.  */
#define SPARC_IMM2_P(X) (((unsigned HOST_WIDE_INT) (X) & ~0x3) == 0)
#define SPARC_IMM5_P(X) (((unsigned HOST_WIDE_INT) (X) & ~0x1F)	== 0)

/* Predicates for 5-bit, 10-bit, 11-bit and 13-bit signed constants.  */
#define SPARC_SIMM5_P(X)  ((unsigned HOST_WIDE_INT) (X) + 0x10 < 0x20)
#define SPARC_SIMM10_P(X) ((unsigned HOST_WIDE_INT) (X) + 0x200 < 0x400)
#define SPARC_SIMM11_P(X) ((unsigned HOST_WIDE_INT) (X) + 0x400 < 0x800)
#define SPARC_SIMM13_P(X) ((unsigned HOST_WIDE_INT) (X) + 0x1000 < 0x2000)

/* 10- and 11-bit immediates are only used for a few specific insns.
   SMALL_INT is used throughout the port so we continue to use it.  */
#define SMALL_INT(X) (SPARC_SIMM13_P (INTVAL (X)))

/* Predicate for constants that can be loaded with a sethi instruction.
   This is the general, 64-bit aware, bitwise version that ensures that
   only constants whose representation fits in the mask

     0x00000000fffffc00

   are accepted.  It will reject, for example, negative SImode constants
   on 64-bit hosts, so correct handling is to mask the value beforehand
   according to the mode of the instruction.  */
#define SPARC_SETHI_P(X) \
  (((unsigned HOST_WIDE_INT) (X) \
    & ((unsigned HOST_WIDE_INT) 0x3ff - GET_MODE_MASK (SImode) - 1)) == 0)

/* Version of the above predicate for SImode constants and below.  */
#define SPARC_SETHI32_P(X) \
  (SPARC_SETHI_P ((unsigned HOST_WIDE_INT) (X) & GET_MODE_MASK (SImode)))

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
/* On SPARC, this is the size of MODE in words.  */
#define CLASS_MAX_NREGS(CLASS, MODE)	\
  (FP_REG_CLASS_P (CLASS) ? (GET_MODE_SIZE (MODE) + 3) / 4 \
   : (GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD 1

/* Define this to nonzero if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */
#define FRAME_GROWS_DOWNWARD 1

/* Offset of first parameter from the argument pointer register value.
   !v9: This is 64 for the ins and locals, plus 4 for the struct-return reg
   even if this function isn't going to use it.
   v9: This is 128 for the ins and locals.  */
#define FIRST_PARM_OFFSET(FNDECL) \
  (TARGET_ARCH64 ? 16 * UNITS_PER_WORD : STRUCT_VALUE_OFFSET + UNITS_PER_WORD)

/* Offset from the argument pointer register value to the CFA.
   This is different from FIRST_PARM_OFFSET because the register window
   comes between the CFA and the arguments.  */
#define ARG_POINTER_CFA_OFFSET(FNDECL)  0

/* When a parameter is passed in a register, stack space is still
   allocated for it.
   !v9: All 6 possible integer registers have backing store allocated.
   v9: Only space for the arguments passed is allocated.  */
/* ??? Ideally, we'd use zero here (as the minimum), but zero has special
   meaning to the backend.  Further, we need to be able to detect if a
   varargs/unprototyped function is called, as they may want to spill more
   registers than we've provided space.  Ugly, ugly.  So for now we retain
   all 6 slots even for v9.  */
#define REG_PARM_STACK_SPACE(DECL) (6 * UNITS_PER_WORD)

/* Definitions for register elimination.  */

#define ELIMINABLE_REGS \
  {{ FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}, \
   { FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM} }

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) 		\
  do								\
    {								\
      (OFFSET) = sparc_initial_elimination_offset ((TO));	\
    }								\
  while (0)

/* Keep the stack pointer constant throughout the function.
   This is both an optimization and a necessity: longjmp
   doesn't behave itself when the stack pointer moves within
   the function!  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* Define this macro if the target machine has "register windows".  This
   C expression returns the register number as seen by the called function
   corresponding to register number OUT as seen by the calling function.
   Return OUT if register number OUT is not an outbound register.  */

#define INCOMING_REGNO(OUT) \
 ((TARGET_FLAT || (OUT) < 8 || (OUT) > 15) ? (OUT) : (OUT) + 16)

/* Define this macro if the target machine has "register windows".  This
   C expression returns the register number as seen by the calling function
   corresponding to register number IN as seen by the called function.
   Return IN if register number IN is not an inbound register.  */

#define OUTGOING_REGNO(IN) \
 ((TARGET_FLAT || (IN) < 24 || (IN) > 31) ? (IN) : (IN) - 16)

/* Define this macro if the target machine has register windows.  This
   C expression returns true if the register is call-saved but is in the
   register window.  */

#define LOCAL_REGNO(REGNO) \
  (!TARGET_FLAT && (REGNO) >= 16 && (REGNO) <= 31)

/* Define the size of space to allocate for the return value of an
   untyped_call.  */

#define APPLY_RESULT_SIZE (TARGET_ARCH64 ? 24 : 16)

/* 1 if N is a possible register number for function argument passing.
   On SPARC, these are the "output" registers.  v9 also uses %f0-%f31.  */

#define FUNCTION_ARG_REGNO_P(N) \
  (((N) >= 8 && (N) <= 13)	\
   || (TARGET_ARCH64 && TARGET_FPU && (N) >= 32 && (N) <= 63))

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On SPARC (!v9), this is a single integer, which is a number of words
   of arguments scanned so far (including the invisible argument,
   if any, which holds the structure-value-address).
   Thus 7 or more means all following args should go on the stack.

   For v9, we also need to know whether a prototype is present.  */

struct sparc_args {
  int words;       /* number of words passed so far */
  int prototype_p; /* nonzero if a prototype is present */
  int libcall_p;   /* nonzero if a library call */
};
#define CUMULATIVE_ARGS struct sparc_args

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, FNDECL, N_NAMED_ARGS) \
init_cumulative_args (& (CUM), (FNTYPE), (LIBNAME), (FNDECL));


/* Generate the special assembly code needed to tell the assembler whatever
   it might need to know about the return value of a function.

   For SPARC assemblers, we need to output a .proc pseudo-op which conveys
   information to the assembler relating to peephole optimization (done in
   the assembler).  */

#define ASM_DECLARE_RESULT(FILE, RESULT) \
  fprintf ((FILE), "\t.proc\t0%lo\n", sparc_type_code (TREE_TYPE (RESULT)))

/* Output the special assembly code needed to tell the assembler some
   register is used as global register variable.

   SPARC 64bit psABI declares registers %g2 and %g3 as application
   registers and %g6 and %g7 as OS registers.  Any object using them
   should declare (for %g2/%g3 has to, for %g6/%g7 can) that it uses them
   and how they are used (scratch or some global variable).
   Linker will then refuse to link together objects which use those
   registers incompatibly.

   Unless the registers are used for scratch, two different global
   registers cannot be declared to the same name, so in the unlikely
   case of a global register variable occupying more than one register
   we prefix the second and following registers with .gnu.part1. etc.  */

extern GTY(()) char sparc_hard_reg_printed[8];

#define ASM_DECLARE_REGISTER_GLOBAL(FILE, DECL, REGNO, NAME)		\
do {									\
  if (TARGET_ARCH64)							\
    {									\
      int end = end_hard_regno (DECL_MODE (decl), REGNO);		\
      int reg;								\
      for (reg = (REGNO); reg < 8 && reg < end; reg++)			\
	if ((reg & ~1) == 2 || (reg & ~1) == 6)				\
	  {								\
	    if (reg == (REGNO))						\
	      fprintf ((FILE), "\t.register\t%%g%d, %s\n", reg, (NAME)); \
	    else							\
	      fprintf ((FILE), "\t.register\t%%g%d, .gnu.part%d.%s\n",	\
		       reg, reg - (REGNO), (NAME));			\
	    sparc_hard_reg_printed[reg] = 1;				\
	  }								\
    }									\
} while (0)

/* Emit rtl for profiling.  */
#define PROFILE_HOOK(LABEL)   sparc_profile_hook (LABEL)

/* All the work done in PROFILE_HOOK, but still required.  */
#define FUNCTION_PROFILER(FILE, LABELNO) do { } while (0)

/* Set the name of the mcount function for the system.  */
#define MCOUNT_FUNCTION "*mcount"

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.  */
#define EXIT_IGNORE_STACK 1

/* Length in units of the trampoline for entering a nested function.  */
#define TRAMPOLINE_SIZE (TARGET_ARCH64 ? 32 : 16)

/* Alignment required for trampolines, in bits.  */
#define TRAMPOLINE_ALIGNMENT 128

/* Generate RTL to flush the register windows so as to make arbitrary frames
   available.  */
#define SETUP_FRAME_ADDRESSES()			\
  do {						\
    if (!TARGET_FLAT)				\
      emit_insn (gen_flush_register_windows ());\
  } while (0)

/* Given an rtx for the address of a frame,
   return an rtx for the address of the word in the frame
   that holds the dynamic chain--the previous frame's address.  */
#define DYNAMIC_CHAIN_ADDRESS(frame)	\
  plus_constant (Pmode, frame, 14 * UNITS_PER_WORD + SPARC_STACK_BIAS)

/* Given an rtx for the frame pointer,
   return an rtx for the address of the frame.  */
#define FRAME_ADDR_RTX(frame) plus_constant (Pmode, frame, SPARC_STACK_BIAS)

/* The return address isn't on the stack, it is in a register, so we can't
   access it from the current frame pointer.  We can access it from the
   previous frame pointer though by reading a value from the register window
   save area.  */
#define RETURN_ADDR_IN_PREVIOUS_FRAME 1

/* This is the offset of the return address to the true next instruction to be
   executed for the current function.  */
#define RETURN_ADDR_OFFSET \
  (8 + 4 * (! TARGET_ARCH64 && cfun->returns_struct))

/* The current return address is in %i7.  The return address of anything
   farther back is in the register window save area at [%fp+60].  */
/* ??? This ignores the fact that the actual return address is +8 for normal
   returns, and +12 for structure returns.  */
#define RETURN_ADDR_REGNUM 31
#define RETURN_ADDR_RTX(count, frame)		\
  ((count == -1)				\
   ? gen_rtx_REG (Pmode, RETURN_ADDR_REGNUM)			\
   : gen_rtx_MEM (Pmode,			\
		  memory_address (Pmode, plus_constant (Pmode, frame, \
							15 * UNITS_PER_WORD \
							+ SPARC_STACK_BIAS))))

/* Before the prologue, the return address is %o7 + 8.  OK, sometimes it's
   +12, but always using +8 is close enough for frame unwind purposes.
   Actually, just using %o7 is close enough for unwinding, but %o7+8
   is something you can return to.  */
#define INCOMING_RETURN_ADDR_REGNUM 15
#define INCOMING_RETURN_ADDR_RTX \
  plus_constant (word_mode, \
		 gen_rtx_REG (word_mode, INCOMING_RETURN_ADDR_REGNUM), 8)
#define DWARF_FRAME_RETURN_COLUMN \
  DWARF_FRAME_REGNUM (INCOMING_RETURN_ADDR_REGNUM)

/* The offset from the incoming value of %sp to the top of the stack frame
   for the current function.  On sparc64, we have to account for the stack
   bias if present.  */
#define INCOMING_FRAME_SP_OFFSET SPARC_STACK_BIAS

/* Describe how we implement __builtin_eh_return.  */
#define EH_RETURN_REGNUM 1
#define EH_RETURN_DATA_REGNO(N) ((N) < 4 ? (N) + 24 : INVALID_REGNUM)
#define EH_RETURN_STACKADJ_RTX	gen_rtx_REG (Pmode, EH_RETURN_REGNUM)

/* Define registers used by the epilogue and return instruction.  */
#define EPILOGUE_USES(REGNO)					\
  ((REGNO) == RETURN_ADDR_REGNUM				\
   || (TARGET_FLAT						\
       && epilogue_completed					\
       && (REGNO) == INCOMING_RETURN_ADDR_REGNUM)		\
   || (crtl->calls_eh_return && (REGNO) == EH_RETURN_REGNUM))

/* Select a format to encode pointers in exception handling data.  CODE
   is 0 for data, 1 for code labels, 2 for function pointers.  GLOBAL is
   true if the symbol may be affected by dynamic relocations.

   If assembler and linker properly support .uaword %r_disp32(foo),
   then use PC relative 32-bit relocations instead of absolute relocs
   for shared libraries.  On sparc64, use pc relative 32-bit relocs even
   for binaries, to save memory.

   binutils 2.12 would emit a R_SPARC_DISP32 dynamic relocation if the
   symbol %r_disp32() is against was not local, but .hidden.  In that
   case, we have to use DW_EH_PE_absptr for pic personality.  */
#ifdef HAVE_AS_SPARC_UA_PCREL
#ifdef HAVE_AS_SPARC_UA_PCREL_HIDDEN
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE,GLOBAL)			\
  (flag_pic								\
   ? (GLOBAL ? DW_EH_PE_indirect : 0) | DW_EH_PE_pcrel | DW_EH_PE_sdata4\
   : ((TARGET_ARCH64 && ! GLOBAL)					\
      ? (DW_EH_PE_pcrel | DW_EH_PE_sdata4)				\
      : DW_EH_PE_absptr))
#else
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE,GLOBAL)			\
  (flag_pic								\
   ? (GLOBAL ? DW_EH_PE_absptr : (DW_EH_PE_pcrel | DW_EH_PE_sdata4))	\
   : ((TARGET_ARCH64 && ! GLOBAL)					\
      ? (DW_EH_PE_pcrel | DW_EH_PE_sdata4)				\
      : DW_EH_PE_absptr))
#endif

/* Emit a PC-relative relocation.  */
#define ASM_OUTPUT_DWARF_PCREL(FILE, SIZE, LABEL)	\
  do {							\
    fputs (integer_asm_op (SIZE, FALSE), FILE);		\
    fprintf (FILE, "%%r_disp%d(", SIZE * 8);		\
    assemble_name (FILE, LABEL);			\
    fputc (')', FILE);					\
  } while (0)
#endif

/* Addressing modes, and classification of registers for them.  */

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in reginfo.c during register
   allocation.  */

#define REGNO_OK_FOR_INDEX_P(REGNO) \
(SPARC_INT_REG_P (REGNO) || SPARC_INT_REG_P (reg_renumber[REGNO]) \
 || (REGNO) == FRAME_POINTER_REGNUM				  \
 || reg_renumber[REGNO] == FRAME_POINTER_REGNUM)

#define REGNO_OK_FOR_BASE_P(REGNO)  REGNO_OK_FOR_INDEX_P (REGNO)

#define REGNO_OK_FOR_FP_P(REGNO) \
  (((unsigned) (REGNO) - 32 < (TARGET_V9 ? (unsigned)64 : (unsigned)32)) \
   || ((unsigned) reg_renumber[REGNO] - 32 < (TARGET_V9 ? (unsigned)64 : (unsigned)32)))

#define REGNO_OK_FOR_CCFP_P(REGNO) \
 (TARGET_V9 \
  && (((unsigned) (REGNO) - 96 < (unsigned)4) \
      || ((unsigned) reg_renumber[REGNO] - 96 < (unsigned)4)))

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 2

/* Recognize any constant value that is a valid address.
   When PIC, we do not accept an address that would require a scratch reg
   to load into a register.  */

#define CONSTANT_ADDRESS_P(X) constant_address_p (X)

/* Define this, so that when PIC, reload won't try to reload invalid
   addresses which require two reload registers.  */

#define LEGITIMATE_PIC_OPERAND_P(X) legitimate_pic_operand_p (X)

/* Should gcc use [%reg+%lo(xx)+offset] addresses?  */

#ifdef HAVE_AS_OFFSETABLE_LO10
#define USE_AS_OFFSETABLE_LO10 1
#else
#define USE_AS_OFFSETABLE_LO10 0
#endif

/* Try a machine-dependent way of reloading an illegitimate address
   operand.  If we find one, push the reload and jump to WIN.  This
   macro is used in only one place: `find_reloads_address' in reload.c.  */
#define LEGITIMIZE_RELOAD_ADDRESS(X,MODE,OPNUM,TYPE,IND_LEVELS,WIN)	   \
do {									   \
  int win;								   \
  (X) = sparc_legitimize_reload_address ((X), (MODE), (OPNUM),		   \
					 (int)(TYPE), (IND_LEVELS), &win); \
  if (win)								   \
    goto WIN;								   \
} while (0)

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
/* If we ever implement any of the full models (such as CM_FULLANY),
   this has to be DImode in that case */
#ifdef HAVE_GAS_SUBSECTION_ORDERING
#define CASE_VECTOR_MODE \
(! TARGET_PTR64 ? SImode : flag_pic ? SImode : TARGET_CM_MEDLOW ? SImode : DImode)
#else
/* If assembler does not have working .subsection -1, we use DImode for pic, as otherwise
   we have to sign extend which slows things down.  */
#define CASE_VECTOR_MODE \
(! TARGET_PTR64 ? SImode : flag_pic ? DImode : TARGET_CM_MEDLOW ? SImode : DImode)
#endif

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 1

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 8

/* If a memory-to-memory move would take MOVE_RATIO or more simple
   move-instruction pairs, we will do a cpymem or libcall instead.  */

#define MOVE_RATIO(speed) ((speed) ? 8 : 3)

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS 1

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, UNKNOWN if none.  */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* Nonzero if access to memory by bytes is slow and undesirable.
   For RISC chips, it means that access to memory by bytes is no
   better than access by words when possible, so grab a whole word
   and maybe make use of that.  */
#define SLOW_BYTE_ACCESS 1

/* Define this to be nonzero if shift instructions ignore all but the low-order
   few bits.  */
#define SHIFT_COUNT_TRUNCATED 1

/* For SImode, we make sure the top 32-bits of the register are clear and
   then we subtract 32 from the lzd instruction result.  */
#define CLZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE) \
  ((VALUE) = ((MODE) == SImode ? 32 : 64), 1)

/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
   return the mode to be used for the comparison.  For floating-point,
   CCFP[E]mode is used.  CCNZmode should be used when the first operand
   is a PLUS, MINUS, NEG, or ASHIFT.  CCmode should be used when no special
   processing is needed.  */
#define SELECT_CC_MODE(OP,X,Y)  select_cc_mode ((OP), (X), (Y))

/* Return nonzero if MODE implies a floating point inequality can be
   reversed.  For SPARC this is always true because we have a full
   compliment of ordered and unordered comparisons, but until generic
   code knows how to reverse it correctly we keep the old definition.  */
#define REVERSIBLE_CC_MODE(MODE) ((MODE) != CCFPEmode && (MODE) != CCFPmode)

/* A function address in a call instruction for indexing purposes.  */
#define FUNCTION_MODE Pmode

/* Define this if addresses of constant functions
   shouldn't be put through pseudo regs where they can be cse'd.
   Desirable on machines where ordinary constants are expensive
   but a CALL with constant address is cheap.  */
#define NO_FUNCTION_CSE 1

/* The _Q_* comparison libcalls return booleans.  */
#define FLOAT_LIB_COMPARE_RETURNS_BOOL(MODE, COMPARISON) ((MODE) == TFmode)

/* Assume by default that the _Qp_* 64-bit libcalls are implemented such
   that the inputs are fully consumed before the output memory is clobbered.  */

#define TARGET_BUGGY_QP_LIB	0

/* Assume by default that we do not have the Solaris-specific conversion
   routines nor 64-bit integer multiply and divide routines.  */

#define SUN_CONVERSION_LIBFUNCS 	0
#define DITF_CONVERSION_LIBFUNCS	0
#define SUN_INTEGER_MULTIPLY_64 	0

/* A C expression for the cost of a branch instruction.  A value of 1
   is the default; other values are interpreted relative to that.  */
#define BRANCH_COST(SPEED_P, PREDICTABLE_P) \
  (sparc_branch_cost (SPEED_P, PREDICTABLE_P))

/* Control the assembler format that we output.  */

/* A C string constant describing how to begin a comment in the target
   assembler language.  The compiler assumes that the comment will end at
   the end of the line.  */

#define ASM_COMMENT_START "!"

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON ""

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF ""

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

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
 "%fcc0", "%fcc1", "%fcc2", "%fcc3", "%icc", "%sfp", "%gsr" }

/* Define additional names for use in asm clobbers and asm declarations.  */

#define ADDITIONAL_REGISTER_NAMES \
{{"ccr", SPARC_ICC_REG}, {"cc", SPARC_ICC_REG}}

/* On Sun 4, this limit is 2048.  We use 1000 to be safe, since the length
   can run past this up to a continuation point.  Once we used 1500, but
   a single entry in C++ can run more than 500 bytes, due to the length of
   mangled symbol names.  dbxout.c should really be fixed to do
   continuations when they are actually needed instead of trying to
   guess...  */
#define DBX_CONTIN_LENGTH 1000

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.global "

/* The prefix to add to user-visible assembler symbols.  */

#define USER_LABEL_PREFIX "_"

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf ((LABEL), "*%s%ld", (PREFIX), (long)(NUM))

/* This is how we hook in and defer the case-vector until the end of
   the function.  */
#define ASM_OUTPUT_ADDR_VEC(LAB,VEC) \
  sparc_defer_case_vector ((LAB),(VEC), 0)

#define ASM_OUTPUT_ADDR_DIFF_VEC(LAB,VEC) \
  sparc_defer_case_vector ((LAB),(VEC), 1)

/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
do {									\
  char label[30];							\
  ASM_GENERATE_INTERNAL_LABEL (label, "L", VALUE);			\
  if (CASE_VECTOR_MODE == SImode)					\
    fprintf (FILE, "\t.word\t");					\
  else									\
    fprintf (FILE, "\t.xword\t");					\
  assemble_name (FILE, label);						\
  fputc ('\n', FILE);							\
} while (0)

/* This is how to output an element of a case-vector that is relative.
   (SPARC uses such vectors only when generating PIC.)  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL)		\
do {									\
  char label[30];							\
  ASM_GENERATE_INTERNAL_LABEL (label, "L", (VALUE));			\
  if (CASE_VECTOR_MODE == SImode)					\
    fprintf (FILE, "\t.word\t");					\
  else									\
    fprintf (FILE, "\t.xword\t");					\
  assemble_name (FILE, label);						\
  ASM_GENERATE_INTERNAL_LABEL (label, "L", (REL));			\
  fputc ('-', FILE);							\
  assemble_name (FILE, label);						\
  fputc ('\n', FILE);							\
} while (0)

/* This is what to output before and after case-vector (both
   relative and absolute).  If .subsection -1 works, we put case-vectors
   at the beginning of the current section.  */

#ifdef HAVE_GAS_SUBSECTION_ORDERING

#define ASM_OUTPUT_ADDR_VEC_START(FILE)					\
  fprintf(FILE, "\t.subsection\t-1\n")

#define ASM_OUTPUT_ADDR_VEC_END(FILE)					\
  fprintf(FILE, "\t.previous\n")

#endif

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) != 0)			\
    fprintf (FILE, "\t.align %d\n", (1 << (LOG)))

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.skip " HOST_WIDE_INT_PRINT_UNSIGNED"\n", (SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
( fputs ("\t.common ", (FILE)),		\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), "," HOST_WIDE_INT_PRINT_UNSIGNED",\"bss\"\n", (SIZE)))

/* This says how to output an assembler line to define a local common
   symbol.  */

#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGNED)		\
( fputs ("\t.reserve ", (FILE)),					\
  assemble_name ((FILE), (NAME)),					\
  fprintf ((FILE), "," HOST_WIDE_INT_PRINT_UNSIGNED",\"bss\",%u\n",	\
	   (SIZE), ((ALIGNED) / BITS_PER_UNIT)))

/* A C statement (sans semicolon) to output to the stdio stream
   FILE the assembler definition of uninitialized global DECL named
   NAME whose size is SIZE bytes and alignment is ALIGN bytes.
   Try to use asm_output_aligned_bss to implement this macro.  */

#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN)	\
  do {								\
    ASM_OUTPUT_ALIGNED_LOCAL (FILE, NAME, SIZE, ALIGN);		\
  } while (0)

/* Output #ident as a .ident.  */

#undef TARGET_ASM_OUTPUT_IDENT
#define TARGET_ASM_OUTPUT_IDENT default_asm_output_ident_directive

/* Prettify the assembly.  */

extern int sparc_indent_opcode;

#define ASM_OUTPUT_OPCODE(FILE, PTR)	\
  do {					\
    if (sparc_indent_opcode)		\
      {					\
	putc (' ', FILE);		\
	sparc_indent_opcode = 0;	\
      }					\
  } while (0)

/* TLS support defaulting to original Sun flavor.  GNU extensions
   must be activated in separate configuration files.  */
#ifdef HAVE_AS_TLS
#define TARGET_TLS 1
#else
#define TARGET_TLS 0
#endif

#define TARGET_SUN_TLS TARGET_TLS
#define TARGET_GNU_TLS 0

#ifdef HAVE_AS_FMAF_HPC_VIS3
#define AS_NIAGARA3_FLAG "d"
#else
#define AS_NIAGARA3_FLAG "b"
#endif

#ifdef HAVE_AS_SPARC4
#define AS_NIAGARA4_FLAG "-xarch=sparc4"
#else
#define AS_NIAGARA4_FLAG "-Av9" AS_NIAGARA3_FLAG
#endif

#ifdef HAVE_AS_SPARC5_VIS4
#define AS_NIAGARA7_FLAG "-xarch=sparc5"
#else
#define AS_NIAGARA7_FLAG AS_NIAGARA4_FLAG
#endif

#ifdef HAVE_AS_SPARC6
#define AS_M8_FLAG "-xarch=sparc6"
#else
#define AS_M8_FLAG AS_NIAGARA7_FLAG
#endif

#ifdef HAVE_AS_LEON
#define AS_LEON_FLAG "-Aleon"
#define AS_LEONV7_FLAG "-Aleon"
#else
#define AS_LEON_FLAG "-Av8"
#define AS_LEONV7_FLAG "-Av7"
#endif

/* We use gcc _mcount for profiling.  */
#define NO_PROFILE_COUNTERS 0

/* Debug support */
#define MASK_DEBUG_OPTIONS		0x01	/* debug option handling */
#define MASK_DEBUG_ALL			MASK_DEBUG_OPTIONS

#define TARGET_DEBUG_OPTIONS		(sparc_debug & MASK_DEBUG_OPTIONS)

/* By default, use the weakest memory model for the cpu.  */
#ifndef SUBTARGET_DEFAULT_MEMORY_MODEL
#define SUBTARGET_DEFAULT_MEMORY_MODEL	SMM_DEFAULT
#endif

/* Define this to 1 if the FE_EXCEPT values defined in fenv.h start at 1.  */
#define SPARC_LOW_FE_EXCEPT_VALUES 0

#define TARGET_SUPPORTS_WIDE_INT 1
