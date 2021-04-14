/* Definitions of target machine for GNU compiler, for IBM RS/6000.
   Copyright (C) 1992-2021 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@vlsi1.ultra.nyu.edu)

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* Note that some other tm.h files include this one and then override
   many of the definitions.  */

#ifndef RS6000_OPTS_H
#include "config/rs6000/rs6000-opts.h"
#endif

/* 128-bit floating point precision values.  */
#ifndef RS6000_MODES_H
#include "config/rs6000/rs6000-modes.h"
#endif

/* Definitions for the object file format.  These are set at
   compile-time.  */

#define OBJECT_XCOFF 1
#define OBJECT_ELF 2
#define OBJECT_MACHO 4

#define TARGET_ELF (TARGET_OBJECT_FORMAT == OBJECT_ELF)
#define TARGET_XCOFF (TARGET_OBJECT_FORMAT == OBJECT_XCOFF)
#define TARGET_MACHO (TARGET_OBJECT_FORMAT == OBJECT_MACHO)

#ifndef TARGET_AIX
#define TARGET_AIX 0
#endif

#ifndef TARGET_AIX_OS
#define TARGET_AIX_OS 0
#endif

/* Turn off TOC support if pc-relative addressing is used.  */
#define TARGET_TOC             (TARGET_HAS_TOC && !TARGET_PCREL)

/* On 32-bit systems without a TOC or pc-relative addressing, we need to use
   ADDIS/ADDI to load up the address of a symbol.  */
#define TARGET_NO_TOC_OR_PCREL (!TARGET_HAS_TOC && !TARGET_PCREL)

/* Control whether function entry points use a "dot" symbol when
   ABI_AIX.  */
#define DOT_SYMBOLS 1

/* Default string to use for cpu if not specified.  */
#ifndef TARGET_CPU_DEFAULT
#define TARGET_CPU_DEFAULT ((char *)0)
#endif

/* If configured for PPC405, support PPC405CR Erratum77.  */
#ifdef CONFIG_PPC405CR
#define PPC405_ERRATUM77 (rs6000_cpu == PROCESSOR_PPC405)
#else
#define PPC405_ERRATUM77 0
#endif

#ifndef SUBTARGET_DRIVER_SELF_SPECS
# define SUBTARGET_DRIVER_SELF_SPECS ""
#endif

/* Only for use in the testsuite: -mdejagnu-cpu= simply overrides -mcpu=.
   With older versions of Dejagnu the command line arguments you set in
   RUNTESTFLAGS override those set in the testcases; with this option,
   the testcase will always win.  Ditto for -mdejagnu-tune=.  */
#define DRIVER_SELF_SPECS \
  "%{mdejagnu-cpu=*: %<mcpu=* -mcpu=%*}", \
  "%{mdejagnu-tune=*: %<mtune=* -mtune=%*}", \
  "%{mdejagnu-*: %<mdejagnu-*}", \
   SUBTARGET_DRIVER_SELF_SPECS

#if CHECKING_P
#define ASM_OPT_ANY ""
#else
#define ASM_OPT_ANY " -many"
#endif

/* Common ASM definitions used by ASM_SPEC among the various targets for
   handling -mcpu=xxx switches.  There is a parallel list in driver-rs6000.c to
   provide the default assembler options if the user uses -mcpu=native, so if
   you make changes here, make them also there.  PR63177: Do not pass -mpower8
   to the assembler if -mpower9-vector was also used.  */
#define ASM_CPU_SPEC \
"%{mcpu=native: %(asm_cpu_native); \
  mcpu=power10: -mpower10; \
  mcpu=power9: -mpower9; \
  mcpu=power8|mcpu=powerpc64le: %{mpower9-vector: -mpower9;: -mpower8}; \
  mcpu=power7: -mpower7; \
  mcpu=power6x: -mpower6 %{!mvsx:%{!maltivec:-maltivec}}; \
  mcpu=power6: -mpower6 %{!mvsx:%{!maltivec:-maltivec}}; \
  mcpu=power5+: -mpower5; \
  mcpu=power5: -mpower5; \
  mcpu=power4: -mpower4; \
  mcpu=power3: -mppc64; \
  mcpu=powerpc: -mppc; \
  mcpu=powerpc64: -mppc64; \
  mcpu=a2: -ma2; \
  mcpu=cell: -mcell; \
  mcpu=rs64: -mppc64; \
  mcpu=401: -mppc; \
  mcpu=403: -m403; \
  mcpu=405: -m405; \
  mcpu=405fp: -m405; \
  mcpu=440: -m440; \
  mcpu=440fp: -m440; \
  mcpu=464: -m440; \
  mcpu=464fp: -m440; \
  mcpu=476: -m476; \
  mcpu=476fp: -m476; \
  mcpu=505: -mppc; \
  mcpu=601: -m601; \
  mcpu=602: -mppc; \
  mcpu=603: -mppc; \
  mcpu=603e: -mppc; \
  mcpu=ec603e: -mppc; \
  mcpu=604: -mppc; \
  mcpu=604e: -mppc; \
  mcpu=620: -mppc64; \
  mcpu=630: -mppc64; \
  mcpu=740: -mppc; \
  mcpu=750: -mppc; \
  mcpu=G3: -mppc; \
  mcpu=7400: -mppc %{!mvsx:%{!maltivec:-maltivec}}; \
  mcpu=7450: -mppc %{!mvsx:%{!maltivec:-maltivec}}; \
  mcpu=G4: -mppc %{!mvsx:%{!maltivec:-maltivec}}; \
  mcpu=801: -mppc; \
  mcpu=821: -mppc; \
  mcpu=823: -mppc; \
  mcpu=860: -mppc; \
  mcpu=970: -mpower4 %{!mvsx:%{!maltivec:-maltivec}}; \
  mcpu=G5: -mpower4 %{!mvsx:%{!maltivec:-maltivec}}; \
  mcpu=8540: -me500; \
  mcpu=8548: -me500; \
  mcpu=e300c2: -me300; \
  mcpu=e300c3: -me300; \
  mcpu=e500mc: -me500mc; \
  mcpu=e500mc64: -me500mc64; \
  mcpu=e5500: -me5500; \
  mcpu=e6500: -me6500; \
  mcpu=titan: -mtitan; \
  !mcpu*: %{mpower9-vector: -mpower9; \
	    mpower8-vector|mcrypto|mdirect-move|mhtm: -mpower8; \
	    mvsx: -mpower7; \
	    mpowerpc64: -mppc64;: %(asm_default)}; \
  :%eMissing -mcpu option in ASM_CPU_SPEC?\n} \
%{mvsx: -mvsx -maltivec; maltivec: -maltivec}" \
ASM_OPT_ANY

#define CPP_DEFAULT_SPEC ""

#define ASM_DEFAULT_SPEC ""
#define ASM_DEFAULT_EXTRA ""

/* This macro defines names of additional specifications to put in the specs
   that can be used in various specifications like CC1_SPEC.  Its definition
   is an initializer with a subgrouping for each command option.

   Each subgrouping contains a string constant, that defines the
   specification name, and a string constant that used by the GCC driver
   program.

   Do not define this macro if it does not need to do anything.  */

#define SUBTARGET_EXTRA_SPECS

#define EXTRA_SPECS							\
  { "cpp_default",		CPP_DEFAULT_SPEC },			\
  { "asm_cpu",			ASM_CPU_SPEC },				\
  { "asm_cpu_native",		ASM_CPU_NATIVE_SPEC },			\
  { "asm_default",		ASM_DEFAULT_SPEC ASM_DEFAULT_EXTRA },	\
  { "cc1_cpu",			CC1_CPU_SPEC },				\
  SUBTARGET_EXTRA_SPECS

/* -mcpu=native handling only makes sense with compiler running on
   an PowerPC chip.  If changing this condition, also change
   the condition in driver-rs6000.c.  */
#if defined(__powerpc__) || defined(__POWERPC__) || defined(_AIX)
/* In driver-rs6000.c.  */
extern const char *host_detect_local_cpu (int argc, const char **argv);
#define EXTRA_SPEC_FUNCTIONS \
  { "local_cpu_detect", host_detect_local_cpu },
#define HAVE_LOCAL_CPU_DETECT
#define ASM_CPU_NATIVE_SPEC "%:local_cpu_detect(asm)"

#else
#define ASM_CPU_NATIVE_SPEC "%(asm_default)"
#endif

#ifndef CC1_CPU_SPEC
#ifdef HAVE_LOCAL_CPU_DETECT
#define CC1_CPU_SPEC \
"%{mcpu=native:%<mcpu=native %:local_cpu_detect(cpu)} \
 %{mtune=native:%<mtune=native %:local_cpu_detect(tune)}"
#else
#define CC1_CPU_SPEC ""
#endif
#endif

/* Architecture type.  */

/* Define TARGET_MFCRF if the target assembler does not support the
   optional field operand for mfcr.  */

#ifndef HAVE_AS_MFCRF
#undef  TARGET_MFCRF
#define TARGET_MFCRF 0
#endif

#ifndef TARGET_SECURE_PLT
#define TARGET_SECURE_PLT 0
#endif

#ifndef TARGET_CMODEL
#define TARGET_CMODEL CMODEL_SMALL
#endif

#define TARGET_32BIT		(! TARGET_64BIT)

#ifndef HAVE_AS_TLS
#define HAVE_AS_TLS 0
#endif

#ifndef HAVE_AS_PLTSEQ
#define HAVE_AS_PLTSEQ 0
#endif

#ifndef TARGET_PLTSEQ
#define TARGET_PLTSEQ 0
#endif

#ifndef TARGET_LINK_STACK
#define TARGET_LINK_STACK 0
#endif

#ifndef SET_TARGET_LINK_STACK
#define SET_TARGET_LINK_STACK(X) do { } while (0)
#endif

#ifndef TARGET_FLOAT128_ENABLE_TYPE
#define TARGET_FLOAT128_ENABLE_TYPE 0
#endif

/* Return 1 for a symbol ref for a thread-local storage symbol.  */
#define RS6000_SYMBOL_REF_TLS_P(RTX) \
  (SYMBOL_REF_P (RTX) && SYMBOL_REF_TLS_MODEL (RTX) != 0)

#ifdef IN_LIBGCC2
/* For libgcc2 we make sure this is a compile time constant */
#if defined (__64BIT__) || defined (__powerpc64__) || defined (__ppc64__)
#undef TARGET_POWERPC64
#define TARGET_POWERPC64	1
#else
#undef TARGET_POWERPC64
#define TARGET_POWERPC64	0
#endif
#else
    /* The option machinery will define this.  */
#endif

#define TARGET_DEFAULT (MASK_MULTIPLE)

/* Define generic processor types based upon current deployment.  */
#define PROCESSOR_COMMON    PROCESSOR_PPC601
#define PROCESSOR_POWERPC   PROCESSOR_PPC604
#define PROCESSOR_POWERPC64 PROCESSOR_RS64A

/* Define the default processor.  This is overridden by other tm.h files.  */
#define PROCESSOR_DEFAULT   PROCESSOR_PPC603
#define PROCESSOR_DEFAULT64 PROCESSOR_RS64A

/* Specify the dialect of assembler to use.  Only new mnemonics are supported
   starting with GCC 4.8, i.e. just one dialect, but for backwards
   compatibility with older inline asm ASSEMBLER_DIALECT needs to be
   defined.  */
#define ASSEMBLER_DIALECT 1

/* Debug support */
#define MASK_DEBUG_STACK	0x01	/* debug stack applications */
#define	MASK_DEBUG_ARG		0x02	/* debug argument handling */
#define MASK_DEBUG_REG		0x04	/* debug register handling */
#define MASK_DEBUG_ADDR		0x08	/* debug memory addressing */
#define MASK_DEBUG_COST		0x10	/* debug rtx codes */
#define MASK_DEBUG_TARGET	0x20	/* debug target attribute/pragma */
#define MASK_DEBUG_BUILTIN	0x40	/* debug builtins */
#define MASK_DEBUG_ALL		(MASK_DEBUG_STACK \
				 | MASK_DEBUG_ARG \
				 | MASK_DEBUG_REG \
				 | MASK_DEBUG_ADDR \
				 | MASK_DEBUG_COST \
				 | MASK_DEBUG_TARGET \
				 | MASK_DEBUG_BUILTIN)

#define	TARGET_DEBUG_STACK	(rs6000_debug & MASK_DEBUG_STACK)
#define	TARGET_DEBUG_ARG	(rs6000_debug & MASK_DEBUG_ARG)
#define TARGET_DEBUG_REG	(rs6000_debug & MASK_DEBUG_REG)
#define TARGET_DEBUG_ADDR	(rs6000_debug & MASK_DEBUG_ADDR)
#define TARGET_DEBUG_COST	(rs6000_debug & MASK_DEBUG_COST)
#define TARGET_DEBUG_TARGET	(rs6000_debug & MASK_DEBUG_TARGET)
#define TARGET_DEBUG_BUILTIN	(rs6000_debug & MASK_DEBUG_BUILTIN)

/* Helper macros for TFmode.  Quad floating point (TFmode) can be either IBM
   long double format that uses a pair of doubles, or IEEE 128-bit floating
   point.  KFmode was added as a way to represent IEEE 128-bit floating point,
   even if the default for long double is the IBM long double format.
   Similarly IFmode is the IBM long double format even if the default is IEEE
   128-bit.  Don't allow IFmode if -msoft-float.  */
#define FLOAT128_IEEE_P(MODE)						\
  ((TARGET_IEEEQUAD && TARGET_LONG_DOUBLE_128				\
    && ((MODE) == TFmode || (MODE) == TCmode))				\
   || ((MODE) == KFmode) || ((MODE) == KCmode))

#define FLOAT128_IBM_P(MODE)						\
  ((!TARGET_IEEEQUAD && TARGET_LONG_DOUBLE_128				\
    && ((MODE) == TFmode || (MODE) == TCmode))				\
   || (TARGET_HARD_FLOAT && ((MODE) == IFmode || (MODE) == ICmode)))

/* Helper macros to say whether a 128-bit floating point type can go in a
   single vector register, or whether it needs paired scalar values.  */
#define FLOAT128_VECTOR_P(MODE) (TARGET_FLOAT128_TYPE && FLOAT128_IEEE_P (MODE))

#define FLOAT128_2REG_P(MODE)						\
  (FLOAT128_IBM_P (MODE)						\
   || ((MODE) == TDmode)						\
   || (!TARGET_FLOAT128_TYPE && FLOAT128_IEEE_P (MODE)))

/* Return true for floating point that does not use a vector register.  */
#define SCALAR_FLOAT_MODE_NOT_VECTOR_P(MODE)				\
  (SCALAR_FLOAT_MODE_P (MODE) && !FLOAT128_VECTOR_P (MODE))

/* Describe the vector unit used for arithmetic operations.  */
extern enum rs6000_vector rs6000_vector_unit[];

#define VECTOR_UNIT_NONE_P(MODE)			\
  (rs6000_vector_unit[(MODE)] == VECTOR_NONE)

#define VECTOR_UNIT_VSX_P(MODE)				\
  (rs6000_vector_unit[(MODE)] == VECTOR_VSX)

#define VECTOR_UNIT_P8_VECTOR_P(MODE)			\
  (rs6000_vector_unit[(MODE)] == VECTOR_P8_VECTOR)

#define VECTOR_UNIT_ALTIVEC_P(MODE)			\
  (rs6000_vector_unit[(MODE)] == VECTOR_ALTIVEC)

#define VECTOR_UNIT_VSX_OR_P8_VECTOR_P(MODE)		\
  (IN_RANGE ((int)rs6000_vector_unit[(MODE)],		\
	     (int)VECTOR_VSX,				\
	     (int)VECTOR_P8_VECTOR))

/* VECTOR_UNIT_ALTIVEC_OR_VSX_P is used in places where we are using either
   altivec (VMX) or VSX vector instructions.  P8 vector support is upwards
   compatible, so allow it as well, rather than changing all of the uses of the
   macro.  */
#define VECTOR_UNIT_ALTIVEC_OR_VSX_P(MODE)		\
  (IN_RANGE ((int)rs6000_vector_unit[(MODE)],		\
	     (int)VECTOR_ALTIVEC,			\
	     (int)VECTOR_P8_VECTOR))

/* Describe whether to use VSX loads or Altivec loads.  For now, just use the
   same unit as the vector unit we are using, but we may want to migrate to
   using VSX style loads even for types handled by altivec.  */
extern enum rs6000_vector rs6000_vector_mem[];

#define VECTOR_MEM_NONE_P(MODE)				\
  (rs6000_vector_mem[(MODE)] == VECTOR_NONE)

#define VECTOR_MEM_VSX_P(MODE)				\
  (rs6000_vector_mem[(MODE)] == VECTOR_VSX)

#define VECTOR_MEM_P8_VECTOR_P(MODE)			\
  (rs6000_vector_mem[(MODE)] == VECTOR_VSX)

#define VECTOR_MEM_ALTIVEC_P(MODE)			\
  (rs6000_vector_mem[(MODE)] == VECTOR_ALTIVEC)

#define VECTOR_MEM_VSX_OR_P8_VECTOR_P(MODE)		\
  (IN_RANGE ((int)rs6000_vector_mem[(MODE)],		\
	     (int)VECTOR_VSX,				\
	     (int)VECTOR_P8_VECTOR))

#define VECTOR_MEM_ALTIVEC_OR_VSX_P(MODE)		\
  (IN_RANGE ((int)rs6000_vector_mem[(MODE)],		\
	     (int)VECTOR_ALTIVEC,			\
	     (int)VECTOR_P8_VECTOR))

/* Return the alignment of a given vector type, which is set based on the
   vector unit use.  VSX for instance can load 32 or 64 bit aligned words
   without problems, while Altivec requires 128-bit aligned vectors.  */
extern int rs6000_vector_align[];

#define VECTOR_ALIGN(MODE)						\
  ((rs6000_vector_align[(MODE)] != 0)					\
   ? rs6000_vector_align[(MODE)]					\
   : (int)GET_MODE_BITSIZE ((MODE)))

/* Element number of the 64-bit value in a 128-bit vector that can be accessed
   with scalar instructions.  */
#define VECTOR_ELEMENT_SCALAR_64BIT	((BYTES_BIG_ENDIAN) ? 0 : 1)

/* Element number of the 64-bit value in a 128-bit vector that can be accessed
   with the ISA 3.0 MFVSRLD instructions.  */
#define VECTOR_ELEMENT_MFVSRLD_64BIT	((BYTES_BIG_ENDIAN) ? 1 : 0)

/* Alignment options for fields in structures for sub-targets following
   AIX-like ABI.
   ALIGN_POWER word-aligns FP doubles (default AIX ABI).
   ALIGN_NATURAL doubleword-aligns FP doubles (align to object size).

   Override the macro definitions when compiling libobjc to avoid undefined
   reference to rs6000_alignment_flags due to library's use of GCC alignment
   macros which use the macros below.  */

#ifndef IN_TARGET_LIBS
#define MASK_ALIGN_POWER   0x00000000
#define MASK_ALIGN_NATURAL 0x00000001
#define TARGET_ALIGN_NATURAL (rs6000_alignment_flags & MASK_ALIGN_NATURAL)
#else
#define TARGET_ALIGN_NATURAL 0
#endif

/* We use values 126..128 to pick the appropriate long double type (IFmode,
   KFmode, TFmode).  */
#define TARGET_LONG_DOUBLE_128 (rs6000_long_double_type_size > 64)
#define TARGET_IEEEQUAD rs6000_ieeequad
#define TARGET_ALTIVEC_ABI rs6000_altivec_abi
#define TARGET_LDBRX (TARGET_POPCNTD || rs6000_cpu == PROCESSOR_CELL)

/* ISA 2.01 allowed FCFID to be done in 32-bit, previously it was 64-bit only.
   Enable 32-bit fcfid's on any of the switches for newer ISA machines.  */
#define TARGET_FCFID	(TARGET_POWERPC64				\
			 || TARGET_PPC_GPOPT	/* 970/power4 */	\
			 || TARGET_POPCNTB	/* ISA 2.02 */		\
			 || TARGET_CMPB		/* ISA 2.05 */		\
			 || TARGET_POPCNTD)	/* ISA 2.06 */

#define TARGET_FCTIDZ	TARGET_FCFID
#define TARGET_STFIWX	TARGET_PPC_GFXOPT
#define TARGET_LFIWAX	TARGET_CMPB
#define TARGET_LFIWZX	TARGET_POPCNTD
#define TARGET_FCFIDS	TARGET_POPCNTD
#define TARGET_FCFIDU	TARGET_POPCNTD
#define TARGET_FCFIDUS	TARGET_POPCNTD
#define TARGET_FCTIDUZ	TARGET_POPCNTD
#define TARGET_FCTIWUZ	TARGET_POPCNTD
#define TARGET_CTZ	TARGET_MODULO
#define TARGET_EXTSWSLI	(TARGET_MODULO && TARGET_POWERPC64)
#define TARGET_MADDLD	TARGET_MODULO

#define TARGET_XSCVDPSPN	(TARGET_DIRECT_MOVE || TARGET_P8_VECTOR)
#define TARGET_XSCVSPDPN	(TARGET_DIRECT_MOVE || TARGET_P8_VECTOR)
#define TARGET_VADDUQM		(TARGET_P8_VECTOR && TARGET_POWERPC64)
#define TARGET_DIRECT_MOVE_128	(TARGET_P9_VECTOR && TARGET_DIRECT_MOVE \
				 && TARGET_POWERPC64)
#define TARGET_VEXTRACTUB	(TARGET_P9_VECTOR && TARGET_DIRECT_MOVE \
				 && TARGET_POWERPC64)

/* Whether we should avoid (SUBREG:SI (REG:SF) and (SUBREG:SF (REG:SI).  */
#define TARGET_NO_SF_SUBREG	TARGET_DIRECT_MOVE_64BIT
#define TARGET_ALLOW_SF_SUBREG	(!TARGET_DIRECT_MOVE_64BIT)

/* This wants to be set for p8 and newer.  On p7, overlapping unaligned
   loads are slow. */
#define TARGET_EFFICIENT_OVERLAPPING_UNALIGNED TARGET_EFFICIENT_UNALIGNED_VSX

/* Byte/char syncs were added as phased in for ISA 2.06B, but are not present
   in power7, so conditionalize them on p8 features.  TImode syncs need quad
   memory support.  */
#define TARGET_SYNC_HI_QI	(TARGET_QUAD_MEMORY			\
				 || TARGET_QUAD_MEMORY_ATOMIC		\
				 || TARGET_DIRECT_MOVE)

#define TARGET_SYNC_TI		TARGET_QUAD_MEMORY_ATOMIC

/* Power7 has both 32-bit load and store integer for the FPRs, so we don't need
   to allocate the SDmode stack slot to get the value into the proper location
   in the register.  */
#define TARGET_NO_SDMODE_STACK	(TARGET_LFIWZX && TARGET_STFIWX && TARGET_DFP)

/* ISA 3.0 has new min/max functions that don't need fast math that are being
   phased in.  Min/max using FSEL or XSMAXDP/XSMINDP do not return the correct
   answers if the arguments are not in the normal range.  */
#define TARGET_MINMAX	(TARGET_HARD_FLOAT && TARGET_PPC_GFXOPT		\
			 && (TARGET_P9_MINMAX || !flag_trapping_math))

/* In switching from using target_flags to using rs6000_isa_flags, the options
   machinery creates OPTION_MASK_<xxx> instead of MASK_<xxx>.  For now map
   OPTION_MASK_<xxx> back into MASK_<xxx>.  */
#define MASK_ALTIVEC			OPTION_MASK_ALTIVEC
#define MASK_CMPB			OPTION_MASK_CMPB
#define MASK_CRYPTO			OPTION_MASK_CRYPTO
#define MASK_DFP			OPTION_MASK_DFP
#define MASK_DIRECT_MOVE		OPTION_MASK_DIRECT_MOVE
#define MASK_DLMZB			OPTION_MASK_DLMZB
#define MASK_EABI			OPTION_MASK_EABI
#define MASK_FLOAT128_KEYWORD		OPTION_MASK_FLOAT128_KEYWORD
#define MASK_FLOAT128_HW		OPTION_MASK_FLOAT128_HW
#define MASK_FPRND			OPTION_MASK_FPRND
#define MASK_P8_FUSION			OPTION_MASK_P8_FUSION
#define MASK_HARD_FLOAT			OPTION_MASK_HARD_FLOAT
#define MASK_HTM			OPTION_MASK_HTM
#define MASK_ISEL			OPTION_MASK_ISEL
#define MASK_MFCRF			OPTION_MASK_MFCRF
#define MASK_MMA			OPTION_MASK_MMA
#define MASK_MULHW			OPTION_MASK_MULHW
#define MASK_MULTIPLE			OPTION_MASK_MULTIPLE
#define MASK_NO_UPDATE			OPTION_MASK_NO_UPDATE
#define MASK_P8_VECTOR			OPTION_MASK_P8_VECTOR
#define MASK_P9_VECTOR			OPTION_MASK_P9_VECTOR
#define MASK_P9_MISC			OPTION_MASK_P9_MISC
#define MASK_POPCNTB			OPTION_MASK_POPCNTB
#define MASK_POPCNTD			OPTION_MASK_POPCNTD
#define MASK_PPC_GFXOPT			OPTION_MASK_PPC_GFXOPT
#define MASK_PPC_GPOPT			OPTION_MASK_PPC_GPOPT
#define MASK_RECIP_PRECISION		OPTION_MASK_RECIP_PRECISION
#define MASK_SOFT_FLOAT			OPTION_MASK_SOFT_FLOAT
#define MASK_STRICT_ALIGN		OPTION_MASK_STRICT_ALIGN
#define MASK_UPDATE			OPTION_MASK_UPDATE
#define MASK_VSX			OPTION_MASK_VSX
#define MASK_POWER10			OPTION_MASK_POWER10
#define MASK_P10_FUSION			OPTION_MASK_P10_FUSION

#ifndef IN_LIBGCC2
#define MASK_POWERPC64			OPTION_MASK_POWERPC64
#endif

#ifdef TARGET_64BIT
#define MASK_64BIT			OPTION_MASK_64BIT
#endif

#ifdef TARGET_LITTLE_ENDIAN
#define MASK_LITTLE_ENDIAN		OPTION_MASK_LITTLE_ENDIAN
#endif

#ifdef TARGET_REGNAMES
#define MASK_REGNAMES			OPTION_MASK_REGNAMES
#endif

#ifdef TARGET_PROTOTYPE
#define MASK_PROTOTYPE			OPTION_MASK_PROTOTYPE
#endif

#ifdef TARGET_MODULO
#define RS6000_BTM_MODULO		OPTION_MASK_MODULO
#endif


/* For power systems, we want to enable Altivec and VSX builtins even if the
   user did not use -maltivec or -mvsx to allow the builtins to be used inside
   of #pragma GCC target or the target attribute to change the code level for a
   given system.  */

#define TARGET_EXTRA_BUILTINS	(TARGET_POWERPC64			 \
				 || TARGET_PPC_GPOPT /* 970/power4 */	 \
				 || TARGET_POPCNTB   /* ISA 2.02 */	 \
				 || TARGET_CMPB      /* ISA 2.05 */	 \
				 || TARGET_POPCNTD   /* ISA 2.06 */	 \
				 || TARGET_ALTIVEC			 \
				 || TARGET_VSX				 \
				 || TARGET_HARD_FLOAT)

/* E500 cores only support plain "sync", not lwsync.  */
#define TARGET_NO_LWSYNC (rs6000_cpu == PROCESSOR_PPC8540 \
			  || rs6000_cpu == PROCESSOR_PPC8548)


/* Which machine supports the various reciprocal estimate instructions.  */
#define TARGET_FRES	(TARGET_HARD_FLOAT && TARGET_PPC_GFXOPT)

#define TARGET_FRE	(TARGET_HARD_FLOAT \
			 && (TARGET_POPCNTB || VECTOR_UNIT_VSX_P (DFmode)))

#define TARGET_FRSQRTES	(TARGET_HARD_FLOAT && TARGET_POPCNTB \
			 && TARGET_PPC_GFXOPT)

#define TARGET_FRSQRTE	(TARGET_HARD_FLOAT \
			 && (TARGET_PPC_GFXOPT || VECTOR_UNIT_VSX_P (DFmode)))

/* Macro to say whether we can do optimizations where we need to do parts of
   the calculation in 64-bit GPRs and then is transfered to the vector
   registers.  */
#define TARGET_DIRECT_MOVE_64BIT	(TARGET_DIRECT_MOVE		\
					 && TARGET_P8_VECTOR		\
					 && TARGET_POWERPC64)

/* Whether the various reciprocal divide/square root estimate instructions
   exist, and whether we should automatically generate code for the instruction
   by default.  */
#define RS6000_RECIP_MASK_HAVE_RE	0x1	/* have RE instruction.  */
#define RS6000_RECIP_MASK_AUTO_RE	0x2	/* generate RE by default.  */
#define RS6000_RECIP_MASK_HAVE_RSQRTE	0x4	/* have RSQRTE instruction.  */
#define RS6000_RECIP_MASK_AUTO_RSQRTE	0x8	/* gen. RSQRTE by default.  */

extern unsigned char rs6000_recip_bits[];

#define RS6000_RECIP_HAVE_RE_P(MODE) \
  (rs6000_recip_bits[(int)(MODE)] & RS6000_RECIP_MASK_HAVE_RE)

#define RS6000_RECIP_AUTO_RE_P(MODE) \
  (rs6000_recip_bits[(int)(MODE)] & RS6000_RECIP_MASK_AUTO_RE)

#define RS6000_RECIP_HAVE_RSQRTE_P(MODE) \
  (rs6000_recip_bits[(int)(MODE)] & RS6000_RECIP_MASK_HAVE_RSQRTE)

#define RS6000_RECIP_AUTO_RSQRTE_P(MODE) \
  (rs6000_recip_bits[(int)(MODE)] & RS6000_RECIP_MASK_AUTO_RSQRTE)

/* The default CPU for TARGET_OPTION_OVERRIDE.  */
#define OPTION_TARGET_CPU_DEFAULT TARGET_CPU_DEFAULT

/* Target pragma.  */
#define REGISTER_TARGET_PRAGMAS() do {				\
  c_register_pragma (0, "longcall", rs6000_pragma_longcall);	\
  targetm.target_option.pragma_parse = rs6000_pragma_target_parse; \
  targetm.resolve_overloaded_builtin = altivec_resolve_overloaded_builtin; \
  rs6000_target_modify_macros_ptr = rs6000_target_modify_macros; \
} while (0)

/* Target #defines.  */
#define TARGET_CPU_CPP_BUILTINS() \
  rs6000_cpu_cpp_builtins (pfile)

/* Target hooks for D language.  */
#define TARGET_D_CPU_VERSIONS rs6000_d_target_versions
#define TARGET_D_REGISTER_CPU_TARGET_INFO rs6000_d_register_target_info

/* This is used by rs6000_cpu_cpp_builtins to indicate the byte order
   we're compiling for.  Some configurations may need to override it.  */
#define RS6000_CPU_CPP_ENDIAN_BUILTINS()	\
  do						\
    {						\
      if (BYTES_BIG_ENDIAN)			\
	{					\
	  builtin_define ("__BIG_ENDIAN__");	\
	  builtin_define ("_BIG_ENDIAN");	\
	  builtin_assert ("machine=bigendian");	\
	}					\
      else					\
	{					\
	  builtin_define ("__LITTLE_ENDIAN__");	\
	  builtin_define ("_LITTLE_ENDIAN");	\
	  builtin_assert ("machine=littleendian"); \
	}					\
    }						\
  while (0)

/* Target machine storage layout.  */

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases,
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.  */

#define PROMOTE_MODE(MODE,UNSIGNEDP,TYPE)	\
  if (GET_MODE_CLASS (MODE) == MODE_INT		\
      && GET_MODE_SIZE (MODE) < (TARGET_32BIT ? 4 : 8)) \
    (MODE) = TARGET_32BIT ? SImode : DImode;

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */
/* That is true on RS/6000.  */
#define BITS_BIG_ENDIAN 1

/* Define this if most significant byte of a word is the lowest numbered.  */
/* That is true on RS/6000.  */
#define BYTES_BIG_ENDIAN 1

/* Define this if most significant word of a multiword number is lowest
   numbered.

   For RS/6000 we can decide arbitrarily since there are no machine
   instructions for them.  Might as well be consistent with bits and bytes.  */
#define WORDS_BIG_ENDIAN 1

/* This says that for the IBM long double the larger magnitude double
   comes first.  It's really a two element double array, and arrays
   don't index differently between little- and big-endian.  */
#define LONG_DOUBLE_LARGE_FIRST 1

#define MAX_BITS_PER_WORD 64

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD (! TARGET_POWERPC64 ? 4 : 8)
#ifdef IN_LIBGCC2
#define MIN_UNITS_PER_WORD UNITS_PER_WORD
#else
#define MIN_UNITS_PER_WORD 4
#endif
#define UNITS_PER_FP_WORD 8
#define UNITS_PER_ALTIVEC_WORD 16
#define UNITS_PER_VSX_WORD 16

/* Type used for ptrdiff_t, as a string used in a declaration.  */
#define PTRDIFF_TYPE "int"

/* Type used for size_t, as a string used in a declaration.  */
#define SIZE_TYPE "long unsigned int"

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

/* A C expression for the size in bits of the type `long long' on the
   target machine.  If you don't define this, the default is two
   words.  */
#define LONG_LONG_TYPE_SIZE 64

/* A C expression for the size in bits of the type `float' on the
   target machine.  If you don't define this, the default is one
   word.  */
#define FLOAT_TYPE_SIZE 32

/* A C expression for the size in bits of the type `double' on the
   target machine.  If you don't define this, the default is two
   words.  */
#define DOUBLE_TYPE_SIZE 64

/* A C expression for the size in bits of the type `long double' on the target
   machine.  If you don't define this, the default is two words.  */
#define LONG_DOUBLE_TYPE_SIZE rs6000_long_double_type_size

/* Work around rs6000_long_double_type_size dependency in ada/targtyps.c.  */
#define WIDEST_HARDWARE_FP_SIZE 64

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
extern unsigned rs6000_pointer_size;
#define POINTER_SIZE rs6000_pointer_size

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY (TARGET_32BIT ? 32 : 64)

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY	\
  ((TARGET_32BIT && !TARGET_ALTIVEC && !TARGET_ALTIVEC_ABI && !TARGET_VSX) \
    ? 64 : 128)

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 32

/* No data type is required to be aligned rounder than this.  Warning, if
   BIGGEST_ALIGNMENT is changed, then this may be an ABI break.  An example
   of where this can break an ABI is in GLIBC's struct _Unwind_Exception.  */
#define BIGGEST_ALIGNMENT 128

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* A bit-field declared as `int' forces `int' alignment for the struct.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

enum data_align { align_abi, align_opt, align_both };

/* A C expression to compute the alignment for a variables in the
   local store.  TYPE is the data type, and ALIGN is the alignment
   that the object would ordinarily have.  */
#define LOCAL_ALIGNMENT(TYPE, ALIGN)				\
  rs6000_data_alignment (TYPE, ALIGN, align_both)

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN) \
  rs6000_data_alignment (TYPE, ALIGN, align_opt)

/* Align vectors to 128 bits.  */
#define DATA_ABI_ALIGNMENT(TYPE, ALIGN) \
  rs6000_data_alignment (TYPE, ALIGN, align_abi)

/* Nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 0

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   RS/6000 has 32 fixed-point registers, 32 floating-point registers,
   a count register, a link register, and 8 condition register fields,
   which we view here as separate registers.  AltiVec adds 32 vector
   registers and a VRsave register.

   In addition, the difference between the frame and argument pointers is
   a function of the number of registers saved, so we need to have a
   register for AP that will later be eliminated in favor of SP or FP.
   This is a normal register, but it is fixed.

   We also create a pseudo register for float/int conversions, that will
   really represent the memory location used.  It is represented here as
   a register, in order to work around problems in allocating stack storage
   in inline functions.

   Another pseudo (not included in DWARF_FRAME_REGISTERS) is soft frame
   pointer, which is eventually eliminated in favor of SP or FP.  */

#define FIRST_PSEUDO_REGISTER 111

/* Use standard DWARF numbering for DWARF debugging information.  */
#define DBX_REGISTER_NUMBER(REGNO) rs6000_dbx_register_number ((REGNO), 0)

/* Use gcc hard register numbering for eh_frame.  */
#define DWARF_FRAME_REGNUM(REGNO) (REGNO)

/* Map register numbers held in the call frame info that gcc has
   collected using DWARF_FRAME_REGNUM to those that should be output in
   .debug_frame and .eh_frame.  */
#define DWARF2_FRAME_REG_OUT(REGNO, FOR_EH) \
  rs6000_dbx_register_number ((REGNO), (FOR_EH) ? 2 : 1)

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.

   On RS/6000, r1 is used for the stack.  On Darwin, r2 is available
   as a local register; for all other OS's r2 is the TOC pointer.

   On System V implementations, r13 is fixed and not available for use.  */

#define FIXED_REGISTERS  \
  {/* GPRs */					   \
   0, 1, FIXED_R2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, FIXED_R13, 0, 0, \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   /* FPRs */					   \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   /* VRs */					   \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   /* lr ctr ca ap */				   \
   0, 0, 1, 1,					   \
   /* cr0..cr7 */				   \
   0, 0, 0, 0, 0, 0, 0, 0,			   \
   /* vrsave vscr sfp */			   \
   1, 1, 1					   \
}

/* Like `CALL_USED_REGISTERS' except this macro doesn't require that
   the entire set of `FIXED_REGISTERS' be included.
   (`CALL_USED_REGISTERS' must be a superset of `FIXED_REGISTERS').
   This macro is optional.  If not specified, it defaults to the value
   of `CALL_USED_REGISTERS'.  */

#define CALL_REALLY_USED_REGISTERS  \
  {/* GPRs */					   \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, FIXED_R13, 0, 0, \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   /* FPRs */					   \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   /* VRs */					   \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   /* lr ctr ca ap */				   \
   1, 1, 1, 1,					   \
   /* cr0..cr7 */				   \
   1, 1, 0, 0, 0, 1, 1, 1,			   \
   /* vrsave vscr sfp */			   \
   0, 0, 0					   \
}

#define TOTAL_ALTIVEC_REGS	(LAST_ALTIVEC_REGNO - FIRST_ALTIVEC_REGNO + 1)

#define FIRST_SAVED_ALTIVEC_REGNO (FIRST_ALTIVEC_REGNO+20)
#define FIRST_SAVED_FP_REGNO	  (14+32)
#define FIRST_SAVED_GP_REGNO	  (FIXED_R13 ? 14 : 13)

/* List the order in which to allocate registers.  Each register must be
   listed once, even those in FIXED_REGISTERS.

   We allocate in the following order:
	fp0		(not saved or used for anything)
	fp13 - fp2	(not saved; incoming fp arg registers)
	fp1		(not saved; return value)
	fp31 - fp14	(saved; order given to save least number)
	cr7, cr5	(not saved or special)
	cr6		(not saved, but used for vector operations)
	cr1		(not saved, but used for FP operations)
	cr0		(not saved, but used for arithmetic operations)
	cr4, cr3, cr2	(saved)
	r9		(not saved; best for TImode)
	r10, r8-r4	(not saved; highest first for less conflict with params)
	r3		(not saved; return value register)
	r11		(not saved; later alloc to help shrink-wrap)
	r0		(not saved; cannot be base reg)
	r31 - r13	(saved; order given to save least number)
	r12		(not saved; if used for DImode or DFmode would use r13)
	ctr		(not saved; when we have the choice ctr is better)
	lr		(saved)
	r1, r2, ap, ca	(fixed)
	v0 - v1		(not saved or used for anything)
	v13 - v3	(not saved; incoming vector arg registers)
	v2		(not saved; incoming vector arg reg; return value)
	v19 - v14	(not saved or used for anything)
	v31 - v20	(saved; order given to save least number)
	vrsave, vscr	(fixed)
	sfp		(fixed)
*/

#if FIXED_R2 == 1
#define MAYBE_R2_AVAILABLE
#define MAYBE_R2_FIXED 2,
#else
#define MAYBE_R2_AVAILABLE 2,
#define MAYBE_R2_FIXED
#endif

#if FIXED_R13 == 1
#define EARLY_R12 12,
#define LATE_R12
#else
#define EARLY_R12
#define LATE_R12 12,
#endif

#define REG_ALLOC_ORDER						\
  {32,								\
   /* move fr13 (ie 45) later, so if we need TFmode, it does */	\
   /* not use fr14 which is a saved register.  */		\
   44, 43, 42, 41, 40, 39, 38, 37, 36, 35, 34, 45,		\
   33,								\
   63, 62, 61, 60, 59, 58, 57, 56, 55, 54, 53, 52, 51,		\
   50, 49, 48, 47, 46,						\
   100, 107, 105, 106, 101, 104, 103, 102,			\
   MAYBE_R2_AVAILABLE						\
   9, 10, 8, 7, 6, 5, 4,					\
   3, EARLY_R12 11, 0,						\
   31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19,		\
   18, 17, 16, 15, 14, 13, LATE_R12				\
   97, 96,							\
   1, MAYBE_R2_FIXED 99, 98,					\
   /* AltiVec registers.  */					\
   64, 65,							\
   77, 76, 75, 74, 73, 72, 71, 70, 69, 68, 67,			\
   66,								\
   83, 82, 81, 80, 79, 78,					\
   95, 94, 93, 92, 91, 90, 89, 88, 87, 86, 85, 84,		\
   108, 109,							\
   110								\
}

/* True if register is floating-point.  */
#define FP_REGNO_P(N) ((N) >= 32 && (N) <= 63)

/* True if register is a condition register.  */
#define CR_REGNO_P(N) ((N) >= CR0_REGNO && (N) <= CR7_REGNO)

/* True if register is a condition register, but not cr0.  */
#define CR_REGNO_NOT_CR0_P(N) ((N) >= CR1_REGNO && (N) <= CR7_REGNO)

/* True if register is an integer register.  */
#define INT_REGNO_P(N) \
  ((N) <= 31 || (N) == ARG_POINTER_REGNUM || (N) == FRAME_POINTER_REGNUM)

/* True if register is the CA register.  */
#define CA_REGNO_P(N) ((N) == CA_REGNO)

/* True if register is an AltiVec register.  */
#define ALTIVEC_REGNO_P(N) ((N) >= FIRST_ALTIVEC_REGNO && (N) <= LAST_ALTIVEC_REGNO)

/* True if register is a VSX register.  */
#define VSX_REGNO_P(N) (FP_REGNO_P (N) || ALTIVEC_REGNO_P (N))

/* Alternate name for any vector register supporting floating point, no matter
   which instruction set(s) are available.  */
#define VFLOAT_REGNO_P(N) \
  (ALTIVEC_REGNO_P (N) || (TARGET_VSX && FP_REGNO_P (N)))

/* Alternate name for any vector register supporting integer, no matter which
   instruction set(s) are available.  */
#define VINT_REGNO_P(N) ALTIVEC_REGNO_P (N)

/* Alternate name for any vector register supporting logical operations, no
   matter which instruction set(s) are available.  Allow GPRs as well as the
   vector registers.  */
#define VLOGICAL_REGNO_P(N)						\
  (INT_REGNO_P (N) || ALTIVEC_REGNO_P (N)				\
   || (TARGET_VSX && FP_REGNO_P (N)))					\

/* When setting up caller-save slots (MODE == VOIDmode) ensure we allocate
   enough space to account for vectors in FP regs.  However, TFmode/TDmode
   should not use VSX instructions to do a caller save. */
#define HARD_REGNO_CALLER_SAVE_MODE(REGNO, NREGS, MODE)			\
  ((NREGS) <= rs6000_hard_regno_nregs[MODE][REGNO]			\
   ? (MODE)								\
   : TARGET_VSX								\
     && ((MODE) == VOIDmode || ALTIVEC_OR_VSX_VECTOR_MODE (MODE))	\
     && FP_REGNO_P (REGNO)						\
   ? V2DFmode								\
   : FLOAT128_IBM_P (MODE) && FP_REGNO_P (REGNO)			\
   ? DFmode								\
   : (MODE) == TDmode && FP_REGNO_P (REGNO)				\
   ? DImode								\
   : choose_hard_reg_mode ((REGNO), (NREGS), NULL))

#define VSX_VECTOR_MODE(MODE)		\
	 ((MODE) == V4SFmode		\
	  || (MODE) == V2DFmode)	\

/* Modes that are not vectors, but require vector alignment.  Treat these like
   vectors in terms of loads and stores.  */
#define VECTOR_ALIGNMENT_P(MODE)					\
  (FLOAT128_VECTOR_P (MODE) || (MODE) == OOmode || (MODE) == XOmode)

#define ALTIVEC_VECTOR_MODE(MODE)					\
  ((MODE) == V16QImode							\
   || (MODE) == V8HImode						\
   || (MODE) == V4SFmode						\
   || (MODE) == V4SImode						\
   || VECTOR_ALIGNMENT_P (MODE))

#define ALTIVEC_OR_VSX_VECTOR_MODE(MODE)				\
  (ALTIVEC_VECTOR_MODE (MODE) || VSX_VECTOR_MODE (MODE)			\
   || (MODE) == V2DImode || (MODE) == V1TImode)

/* Post-reload, we can't use any new AltiVec registers, as we already
   emitted the vrsave mask.  */

#define HARD_REGNO_RENAME_OK(SRC, DST) \
  (! ALTIVEC_REGNO_P (DST) || df_regs_ever_live_p (DST))

/* Specify the cost of a branch insn; roughly the number of extra insns that
   should be added to avoid a branch.

   Set this to 3 on the RS/6000 since that is roughly the average cost of an
   unscheduled conditional branch.  */

#define BRANCH_COST(speed_p, predictable_p) 3

/* Override BRANCH_COST heuristic which empirically produces worse
   performance for removing short circuiting from the logical ops.  */

#define LOGICAL_OP_NON_SHORT_CIRCUIT 0

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* RS/6000 pc isn't overloaded on a register that the compiler knows about.  */
/* #define PC_REGNUM  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 1

/* Base register for access to local variables of the function.  */
#define HARD_FRAME_POINTER_REGNUM 31

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 110

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 99

/* Place to put static chain when calling a function that requires it.  */
#define STATIC_CHAIN_REGNUM 11

/* Base register for access to thread local storage variables.  */
#define TLS_REGNUM ((TARGET_64BIT) ? 13 : 2)


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

/* The RS/6000 has three types of registers, fixed-point, floating-point, and
   condition registers, plus three special registers, CTR, and the link
   register.  AltiVec adds a vector register class.  VSX registers overlap the
   FPR registers and the Altivec registers.

   However, r0 is special in that it cannot be used as a base register.
   So make a class for registers valid as base registers.

   Also, cr0 is the only condition code register that can be used in
   arithmetic insns, so make a separate class for it.  */

enum reg_class
{
  NO_REGS,
  BASE_REGS,
  GENERAL_REGS,
  FLOAT_REGS,
  ALTIVEC_REGS,
  VSX_REGS,
  VRSAVE_REGS,
  VSCR_REGS,
  GEN_OR_FLOAT_REGS,
  GEN_OR_VSX_REGS,
  LINK_REGS,
  CTR_REGS,
  LINK_OR_CTR_REGS,
  SPECIAL_REGS,
  SPEC_OR_GEN_REGS,
  CR0_REGS,
  CR_REGS,
  NON_FLOAT_REGS,
  CA_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.  */

#define REG_CLASS_NAMES							\
{									\
  "NO_REGS",								\
  "BASE_REGS",								\
  "GENERAL_REGS",							\
  "FLOAT_REGS",								\
  "ALTIVEC_REGS",							\
  "VSX_REGS",								\
  "VRSAVE_REGS",							\
  "VSCR_REGS",								\
  "GEN_OR_FLOAT_REGS",							\
  "GEN_OR_VSX_REGS",							\
  "LINK_REGS",								\
  "CTR_REGS",								\
  "LINK_OR_CTR_REGS",							\
  "SPECIAL_REGS",							\
  "SPEC_OR_GEN_REGS",							\
  "CR0_REGS",								\
  "CR_REGS",								\
  "NON_FLOAT_REGS",							\
  "CA_REGS",								\
  "ALL_REGS"								\
}

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS						\
{									\
  /* NO_REGS.  */							\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000 },			\
  /* BASE_REGS.  */							\
  { 0xfffffffe, 0x00000000, 0x00000000, 0x00004008 },			\
  /* GENERAL_REGS.  */							\
  { 0xffffffff, 0x00000000, 0x00000000, 0x00004008 },			\
  /* FLOAT_REGS.  */							\
  { 0x00000000, 0xffffffff, 0x00000000, 0x00000000 },			\
  /* ALTIVEC_REGS.  */							\
  { 0x00000000, 0x00000000, 0xffffffff, 0x00000000 },			\
  /* VSX_REGS.  */							\
  { 0x00000000, 0xffffffff, 0xffffffff, 0x00000000 },			\
  /* VRSAVE_REGS.  */							\
  { 0x00000000, 0x00000000, 0x00000000, 0x00001000 },			\
  /* VSCR_REGS.  */							\
  { 0x00000000, 0x00000000, 0x00000000, 0x00002000 },			\
  /* GEN_OR_FLOAT_REGS.  */						\
  { 0xffffffff, 0xffffffff, 0x00000000, 0x00004008 },			\
  /* GEN_OR_VSX_REGS.  */						\
  { 0xffffffff, 0xffffffff, 0xffffffff, 0x00004008 },			\
  /* LINK_REGS.  */							\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000001 },			\
  /* CTR_REGS.  */							\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000002 },			\
  /* LINK_OR_CTR_REGS.  */						\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000003 },			\
  /* SPECIAL_REGS.  */							\
  { 0x00000000, 0x00000000, 0x00000000, 0x00001003 },			\
  /* SPEC_OR_GEN_REGS.  */						\
  { 0xffffffff, 0x00000000, 0x00000000, 0x0000500b },			\
  /* CR0_REGS.  */							\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000010 },			\
  /* CR_REGS.  */							\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000ff0 },			\
  /* NON_FLOAT_REGS.  */						\
  { 0xffffffff, 0x00000000, 0x00000000, 0x00004ffb },			\
  /* CA_REGS.  */							\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000004 },			\
  /* ALL_REGS.  */							\
  { 0xffffffff, 0xffffffff, 0xffffffff, 0x00007fff }			\
}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

extern enum reg_class rs6000_regno_regclass[FIRST_PSEUDO_REGISTER];

#define REGNO_REG_CLASS(REGNO) 						\
  (gcc_checking_assert (IN_RANGE ((REGNO), 0, FIRST_PSEUDO_REGISTER-1)),\
   rs6000_regno_regclass[(REGNO)])

/* Register classes for various constraints that are based on the target
   switches.  */
enum r6000_reg_class_enum {
  RS6000_CONSTRAINT_d,		/* fpr registers for double values */
  RS6000_CONSTRAINT_f,		/* fpr registers for single values */
  RS6000_CONSTRAINT_v,		/* Altivec registers */
  RS6000_CONSTRAINT_wa,		/* Any VSX register */
  RS6000_CONSTRAINT_we,		/* VSX register if ISA 3.0 vector. */
  RS6000_CONSTRAINT_wr,		/* GPR register if 64-bit  */
  RS6000_CONSTRAINT_wx,		/* FPR register for STFIWX */
  RS6000_CONSTRAINT_wA,		/* BASE_REGS if 64-bit.  */
  RS6000_CONSTRAINT_MAX
};

extern enum reg_class rs6000_constraints[RS6000_CONSTRAINT_MAX];

/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS GENERAL_REGS
#define BASE_REG_CLASS BASE_REGS

/* Return whether a given register class can hold VSX objects.  */
#define VSX_REG_CLASS_P(CLASS)			\
  ((CLASS) == VSX_REGS || (CLASS) == FLOAT_REGS || (CLASS) == ALTIVEC_REGS)

/* Return whether a given register class targets general purpose registers.  */
#define GPR_REG_CLASS_P(CLASS) ((CLASS) == GENERAL_REGS || (CLASS) == BASE_REGS)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.

   On the RS/6000, we have to return NO_REGS when we want to reload a
   floating-point CONST_DOUBLE to force it to be copied to memory.

   We also don't want to reload integer values into floating-point
   registers if we can at all help it.  In fact, this can
   cause reload to die, if it tries to generate a reload of CTR
   into a FP register and discovers it doesn't have the memory location
   required.

   ??? Would it be a good idea to have reload do the converse, that is
   try to reload floating modes into FP registers if possible?
 */

#define PREFERRED_RELOAD_CLASS(X,CLASS)			\
  rs6000_preferred_reload_class_ptr (X, CLASS)

/* Return the register class of a scratch register needed to copy IN into
   or out of a register in CLASS in MODE.  If it can be done directly,
   NO_REGS is returned.  */

#define SECONDARY_RELOAD_CLASS(CLASS,MODE,IN) \
  rs6000_secondary_reload_class_ptr (CLASS, MODE, IN)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.

   On RS/6000, this is the size of MODE in words, except in the FP regs, where
   a single reg is enough for two words, unless we have VSX, where the FP
   registers can hold 128 bits.  */
#define CLASS_MAX_NREGS(CLASS, MODE) rs6000_class_max_nregs[(MODE)][(CLASS)]

/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD 1

/* Offsets recorded in opcodes are a multiple of this alignment factor.  */
#define DWARF_CIE_DATA_ALIGNMENT (-((int) (TARGET_32BIT ? 4 : 8)))

/* Define this to nonzero if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.

   On the RS/6000, we grow upwards, from the area after the outgoing
   arguments.  */
#define FRAME_GROWS_DOWNWARD (flag_stack_protect != 0			\
			      || (flag_sanitize & SANITIZE_ADDRESS) != 0)

/* Size of the fixed area on the stack */
#define RS6000_SAVE_AREA \
  ((DEFAULT_ABI == ABI_V4 ? 8 : DEFAULT_ABI == ABI_ELFv2 ? 16 : 24)	\
   << (TARGET_64BIT ? 1 : 0))

/* Stack offset for toc save slot.  */
#define RS6000_TOC_SAVE_SLOT \
  ((DEFAULT_ABI == ABI_ELFv2 ? 12 : 20) << (TARGET_64BIT ? 1 : 0))

/* Align an address */
#define RS6000_ALIGN(n,a) ROUND_UP ((n), (a))

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.

   On the RS/6000, the frame pointer is the same as the stack pointer,
   except for dynamic allocations.  So we start after the fixed area and
   outgoing parameter area.

   If the function uses dynamic stack space (CALLS_ALLOCA is set), that
   space needs to be aligned to STACK_BOUNDARY, i.e. the sum of the
   sizes of the fixed area and the parameter area must be a multiple of
   STACK_BOUNDARY.  */

#define RS6000_STARTING_FRAME_OFFSET					\
  (cfun->calls_alloca							\
   ? (RS6000_ALIGN (crtl->outgoing_args_size + RS6000_SAVE_AREA,	\
		    (TARGET_ALTIVEC || TARGET_VSX) ? 16 : 8 ))		\
   : (RS6000_ALIGN (crtl->outgoing_args_size,				\
		    (TARGET_ALTIVEC || TARGET_VSX) ? 16 : 8)		\
      + RS6000_SAVE_AREA))

/* Offset from the stack pointer register to an item dynamically
   allocated on the stack, e.g., by `alloca'.

   The default value for this macro is `STACK_POINTER_OFFSET' plus the
   length of the outgoing arguments.  The default is correct for most
   machines.  See `function.c' for details.

   This value must be a multiple of STACK_BOUNDARY (hard coded in
   `emit-rtl.c').  */
#define STACK_DYNAMIC_OFFSET(FUNDECL)					\
  RS6000_ALIGN (crtl->outgoing_args_size.to_constant ()			\
		+ STACK_POINTER_OFFSET,					\
		(TARGET_ALTIVEC || TARGET_VSX) ? 16 : 8)

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
#define REG_PARM_STACK_SPACE(FNDECL) \
  rs6000_reg_parm_stack_space ((FNDECL), false)

/* Define this macro if space guaranteed when compiling a function body
   is different to space required when making a call, a situation that
   can arise with K&R style function definitions.  */
#define INCOMING_REG_PARM_STACK_SPACE(FNDECL) \
  rs6000_reg_parm_stack_space ((FNDECL), true)

/* Define this if the above stack space is to be considered part of the
   space allocated by the caller.  */
#define OUTGOING_REG_PARM_STACK_SPACE(FNTYPE) 1

/* This is the difference between the logical top of stack and the actual sp.

   For the RS/6000, sp points past the fixed area.  */
#define STACK_POINTER_OFFSET RS6000_SAVE_AREA

/* Define this if the maximum size of all the outgoing args is to be
   accumulated and pushed during the prologue.  The amount can be
   found in the variable crtl->outgoing_args_size.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

#define LIBCALL_VALUE(MODE) rs6000_libcall_value ((MODE))

/* DRAFT_V4_STRUCT_RET defaults off.  */
#define DRAFT_V4_STRUCT_RET 0

/* Let TARGET_RETURN_IN_MEMORY control what happens.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Mode of stack savearea.
   FUNCTION is VOIDmode because calling convention maintains SP.
   BLOCK needs Pmode for SP.
   NONLOCAL needs twice Pmode to maintain both backchain and SP.  */
#define STACK_SAVEAREA_MODE(LEVEL)	\
  (LEVEL == SAVE_FUNCTION ? VOIDmode	\
  : LEVEL == SAVE_NONLOCAL ? (TARGET_32BIT ? DImode : PTImode) : Pmode)

/* Minimum and maximum general purpose registers used to hold arguments.  */
#define GP_ARG_MIN_REG 3
#define GP_ARG_MAX_REG 10
#define GP_ARG_NUM_REG (GP_ARG_MAX_REG - GP_ARG_MIN_REG + 1)

/* Minimum and maximum floating point registers used to hold arguments.  */
#define FP_ARG_MIN_REG 33
#define	FP_ARG_AIX_MAX_REG 45
#define	FP_ARG_V4_MAX_REG  40
#define	FP_ARG_MAX_REG (DEFAULT_ABI == ABI_V4				\
			? FP_ARG_V4_MAX_REG : FP_ARG_AIX_MAX_REG)
#define FP_ARG_NUM_REG (FP_ARG_MAX_REG - FP_ARG_MIN_REG + 1)

/* Minimum and maximum AltiVec registers used to hold arguments.  */
#define ALTIVEC_ARG_MIN_REG (FIRST_ALTIVEC_REGNO + 2)
#define ALTIVEC_ARG_MAX_REG (ALTIVEC_ARG_MIN_REG + 11)
#define ALTIVEC_ARG_NUM_REG (ALTIVEC_ARG_MAX_REG - ALTIVEC_ARG_MIN_REG + 1)

/* Maximum number of registers per ELFv2 homogeneous aggregate argument.  */
#define AGGR_ARG_NUM_REG 8

/* Return registers */
#define GP_ARG_RETURN GP_ARG_MIN_REG
#define FP_ARG_RETURN FP_ARG_MIN_REG
#define ALTIVEC_ARG_RETURN (FIRST_ALTIVEC_REGNO + 2)
#define FP_ARG_MAX_RETURN (DEFAULT_ABI != ABI_ELFv2 ? FP_ARG_RETURN	\
			   : (FP_ARG_RETURN + AGGR_ARG_NUM_REG - 1))
#define ALTIVEC_ARG_MAX_RETURN (DEFAULT_ABI != ABI_ELFv2		\
				? (ALTIVEC_ARG_RETURN			\
				   + (TARGET_FLOAT128_TYPE ? 1 : 0))	\
			        : (ALTIVEC_ARG_RETURN + AGGR_ARG_NUM_REG - 1))

/* Flags for the call/call_value rtl operations set up by function_arg */
#define CALL_NORMAL		0x00000000	/* no special processing */
/* Bits in 0x00000001 are unused.  */
#define CALL_V4_CLEAR_FP_ARGS	0x00000002	/* V.4, no FP args passed */
#define CALL_V4_SET_FP_ARGS	0x00000004	/* V.4, FP args were passed */
#define CALL_LONG		0x00000008	/* always call indirect */
#define CALL_LIBCALL		0x00000010	/* libcall */

/* Identify PLT sequence for rs6000_pltseq_template.  */
enum rs6000_pltseq_enum {
  RS6000_PLTSEQ_TOCSAVE,
  RS6000_PLTSEQ_PLT16_HA,
  RS6000_PLTSEQ_PLT16_LO,
  RS6000_PLTSEQ_MTCTR,
  RS6000_PLTSEQ_PLT_PCREL34
};

#define IS_V4_FP_ARGS(OP) \
  ((INTVAL (OP) & (CALL_V4_CLEAR_FP_ARGS | CALL_V4_SET_FP_ARGS)) != 0)

/* We don't have prologue and epilogue functions to save/restore
   everything for most ABIs.  */
#define WORLD_SAVE_P(INFO) 0

/* 1 if N is a possible register number for a function value
   as seen by the caller.

   On RS/6000, this is r3, fp1, and v2 (for AltiVec).  */
#define FUNCTION_VALUE_REGNO_P(N)					\
  ((N) == GP_ARG_RETURN							\
   || (IN_RANGE ((N), FP_ARG_RETURN, FP_ARG_MAX_RETURN)			\
       && TARGET_HARD_FLOAT)						\
   || (IN_RANGE ((N), ALTIVEC_ARG_RETURN, ALTIVEC_ARG_MAX_RETURN)	\
       && TARGET_ALTIVEC && TARGET_ALTIVEC_ABI))

/* 1 if N is a possible register number for function argument passing.
   On RS/6000, these are r3-r10 and fp1-fp13.
   On AltiVec, v2 - v13 are used for passing vectors.  */
#define FUNCTION_ARG_REGNO_P(N)						\
  (IN_RANGE ((N), GP_ARG_MIN_REG, GP_ARG_MAX_REG)			\
   || (IN_RANGE ((N), ALTIVEC_ARG_MIN_REG, ALTIVEC_ARG_MAX_REG)		\
       && TARGET_ALTIVEC && TARGET_ALTIVEC_ABI)				\
   || (IN_RANGE ((N), FP_ARG_MIN_REG, FP_ARG_MAX_REG)			\
       && TARGET_HARD_FLOAT))

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
   the next available GP register, `fregno' is the next available FP
   register, and `words' is the number of words used on the stack.

   The varargs/stdarg support requires that this structure's size
   be a multiple of sizeof(int).  */

typedef struct rs6000_args
{
  int words;			/* # words used for passing GP registers */
  int fregno;			/* next available FP register */
  int vregno;			/* next available AltiVec register */
  int nargs_prototype;		/* # args left in the current prototype */
  int prototype;		/* Whether a prototype was defined */
  int stdarg;			/* Whether function is a stdarg function.  */
  int call_cookie;		/* Do special things for this call */
  int sysv_gregno;		/* next available GP register */
  int intoffset;		/* running offset in struct (darwin64) */
  int use_stack;		/* any part of struct on stack (darwin64) */
  int floats_in_gpr;		/* count of SFmode floats taking up
				   GPR space (darwin64) */
  int named;			/* false for varargs params */
  int escapes;			/* if function visible outside tu */
  int libcall;			/* If this is a compiler generated call.  */
} CUMULATIVE_ARGS;

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, FNDECL, N_NAMED_ARGS) \
  init_cumulative_args (&CUM, FNTYPE, LIBNAME, FALSE, FALSE, \
			N_NAMED_ARGS, FNDECL, VOIDmode)

/* Similar, but when scanning the definition of a procedure.  We always
   set NARGS_PROTOTYPE large so we never return an EXPR_LIST.  */

#define INIT_CUMULATIVE_INCOMING_ARGS(CUM, FNTYPE, LIBNAME) \
  init_cumulative_args (&CUM, FNTYPE, LIBNAME, TRUE, FALSE, \
			1000, current_function_decl, VOIDmode)

/* Like INIT_CUMULATIVE_ARGS' but only used for outgoing libcalls.  */

#define INIT_CUMULATIVE_LIBCALL_ARGS(CUM, MODE, LIBNAME) \
  init_cumulative_args (&CUM, NULL_TREE, LIBNAME, FALSE, TRUE, \
			0, NULL_TREE, MODE)

#define PAD_VARARGS_DOWN \
  (targetm.calls.function_arg_padding (TYPE_MODE (type), type) == PAD_DOWNWARD)

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)	\
  output_function_profiler ((FILE), (LABELNO));

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter. No definition is equivalent to
   always zero.

   On the RS/6000, this is nonzero because we can restore the stack from
   its backpointer, which we maintain.  */
#define EXIT_IGNORE_STACK	1

/* Define this macro as a C expression that is nonzero for registers
   that are used by the epilogue or the return' pattern.  The stack
   and frame pointer registers are already be assumed to be used as
   needed.  */

#define	EPILOGUE_USES(REGNO)					\
  ((reload_completed && (REGNO) == LR_REGNO)			\
   || (TARGET_ALTIVEC && (REGNO) == VRSAVE_REGNO)		\
   || (crtl->calls_eh_return					\
       && TARGET_AIX						\
       && (REGNO) == 2))


/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE rs6000_trampoline_size ()

/* Definitions for __builtin_return_address and __builtin_frame_address.
   __builtin_return_address (0) should give link register (LR_REGNO), enable
   this.  */
/* This should be uncommented, so that the link register is used, but
   currently this would result in unmatched insns and spilling fixed
   registers so we'll leave it for another day.  When these problems are
   taken care of one additional fetch will be necessary in RETURN_ADDR_RTX.
   (mrs) */
/* #define RETURN_ADDR_IN_PREVIOUS_FRAME */

/* Number of bytes into the frame return addresses can be found.  See
   rs6000_stack_info in rs6000.c for more information on how the different
   abi's store the return address.  */
#define RETURN_ADDRESS_OFFSET \
  ((DEFAULT_ABI == ABI_V4 ? 4 : 8) << (TARGET_64BIT ? 1 : 0))

/* The current return address is in the link register.  The return address
   of anything farther back is accessed normally at an offset of 8 from the
   frame pointer.  */
#define RETURN_ADDR_RTX(COUNT, FRAME)                 \
  (rs6000_return_addr (COUNT, FRAME))


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
#define ELIMINABLE_REGS					\
{{ HARD_FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},	\
 { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},		\
 { FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM},	\
 { ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},		\
 { ARG_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM},	\
 { RS6000_PIC_OFFSET_TABLE_REGNUM, RS6000_PIC_OFFSET_TABLE_REGNUM } }

/* Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  ((OFFSET) = rs6000_initial_elimination_offset(FROM, TO))

/* Addressing modes, and classification of registers for them.  */

#define HAVE_PRE_DECREMENT 1
#define HAVE_PRE_INCREMENT 1
#define HAVE_PRE_MODIFY_DISP 1
#define HAVE_PRE_MODIFY_REG 1

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in reginfo.c during register
   allocation.  */

#define REGNO_OK_FOR_INDEX_P(REGNO)				\
(HARD_REGISTER_NUM_P (REGNO)					\
 ? (REGNO) <= 31						\
   || (REGNO) == ARG_POINTER_REGNUM				\
   || (REGNO) == FRAME_POINTER_REGNUM				\
 : (reg_renumber[REGNO] >= 0					\
    && (reg_renumber[REGNO] <= 31				\
	|| reg_renumber[REGNO] == ARG_POINTER_REGNUM		\
	|| reg_renumber[REGNO] == FRAME_POINTER_REGNUM)))

#define REGNO_OK_FOR_BASE_P(REGNO)				\
(HARD_REGISTER_NUM_P (REGNO)					\
 ? ((REGNO) > 0 && (REGNO) <= 31)				\
   || (REGNO) == ARG_POINTER_REGNUM				\
   || (REGNO) == FRAME_POINTER_REGNUM				\
 : (reg_renumber[REGNO] > 0					\
    && (reg_renumber[REGNO] <= 31				\
	|| reg_renumber[REGNO] == ARG_POINTER_REGNUM		\
	|| reg_renumber[REGNO] == FRAME_POINTER_REGNUM)))

/* Nonzero if X is a hard reg that can be used as an index
   or if it is a pseudo reg in the non-strict case.  */
#define INT_REG_OK_FOR_INDEX_P(X, STRICT)			\
  ((!(STRICT) && !HARD_REGISTER_P (X))				\
   || REGNO_OK_FOR_INDEX_P (REGNO (X)))

/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg in the non-strict case.  */
#define INT_REG_OK_FOR_BASE_P(X, STRICT)			\
  ((!(STRICT) && !HARD_REGISTER_P (X))				\
   || REGNO_OK_FOR_BASE_P (REGNO (X)))


/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 2

/* Recognize any constant value that is a valid address.  */

#define CONSTANT_ADDRESS_P(X)   \
  (GET_CODE (X) == LABEL_REF || SYMBOL_REF_P (X)			\
   || CONST_INT_P (X) || GET_CODE (X) == CONST				\
   || GET_CODE (X) == HIGH)

#define EASY_VECTOR_15(n) ((n) >= -16 && (n) <= 15)
#define EASY_VECTOR_15_ADD_SELF(n) (!EASY_VECTOR_15((n))	\
				    && EASY_VECTOR_15((n) >> 1) \
				    && ((n) & 1) == 0)

#define EASY_VECTOR_MSB(n,mode)						\
  ((((unsigned HOST_WIDE_INT) (n)) & GET_MODE_MASK (mode)) ==		\
   ((((unsigned HOST_WIDE_INT)GET_MODE_MASK (mode)) + 1) >> 1))


#define FIND_BASE_TERM rs6000_find_base_term

/* The register number of the register used to address a table of
   static data addresses in memory.  In some cases this register is
   defined by a processor's "application binary interface" (ABI).
   When this macro is defined, RTL is generated for this register
   once, as with the stack pointer and frame pointer registers.  If
   this macro is not defined, it is up to the machine-dependent files
   to allocate such a register (if necessary).  */

#define RS6000_PIC_OFFSET_TABLE_REGNUM 30
#define PIC_OFFSET_TABLE_REGNUM \
  (TARGET_TOC ? TOC_REGISTER			\
   : flag_pic ? RS6000_PIC_OFFSET_TABLE_REGNUM	\
   : INVALID_REGNUM)

#define TOC_REGISTER (TARGET_MINIMAL_TOC ? RS6000_PIC_OFFSET_TABLE_REGNUM : 2)

/* Define this macro if the register defined by
   `PIC_OFFSET_TABLE_REGNUM' is clobbered by calls.  Do not define
   this macro if `PIC_OFFSET_TABLE_REGNUM' is not defined.  */

/* #define PIC_OFFSET_TABLE_REG_CALL_CLOBBERED */

/* A C expression that is nonzero if X is a legitimate immediate
   operand on the target machine when generating position independent
   code.  You can assume that X satisfies `CONSTANT_P', so you need
   not check this.  You can also assume FLAG_PIC is true, so you need
   not check it either.  You need not define this macro if all
   constants (including `SYMBOL_REF') can be immediate operands when
   generating position independent code.  */

/* #define LEGITIMATE_PIC_OPERAND_P (X) */

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses.  */
#define CASE_VECTOR_PC_RELATIVE rs6000_relative_jumptables

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE (rs6000_relative_jumptables ? SImode : Pmode)

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 0

/* An integer expression for the size in bits of the largest integer machine
   mode that should actually be used.  */

/* Allow pairs of registers to be used, which is the intent of the default.  */
#define MAX_FIXED_MODE_SIZE GET_MODE_BITSIZE (TARGET_POWERPC64 ? TImode : DImode)

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX (! TARGET_POWERPC64 ? 4 : 8)
#define MAX_MOVE_MAX 8

/* Nonzero if access to memory by bytes is no faster than for words.
   Also nonzero if doing byte operations (specifically shifts) in registers
   is undesirable.  */
#define SLOW_BYTE_ACCESS 1

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, UNKNOWN if none.  */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* Define if loading short immediate values into registers sign extends.  */
#define SHORT_IMMEDIATES_SIGN_EXTEND 1

/* The cntlzw and cntlzd instructions return 32 and 64 for input of zero.  */
#define CLZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE) \
  ((VALUE) = GET_MODE_BITSIZE (MODE), 2)

/* The CTZ patterns that are implemented in terms of CLZ return -1 for input of
   zero.  The hardware instructions added in Power9 and the sequences using
   popcount return 32 or 64.  */
#define CTZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE)				\
  (TARGET_CTZ || TARGET_POPCNTD						\
   ? ((VALUE) = GET_MODE_BITSIZE (MODE), 2)				\
   : ((VALUE) = -1, 2))

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
extern scalar_int_mode rs6000_pmode;
#define Pmode rs6000_pmode

/* Supply definition of STACK_SIZE_MODE for allocate_dynamic_stack_space.  */
#define STACK_SIZE_MODE (TARGET_32BIT ? SImode : DImode)

/* Mode of a function address in a call instruction (for indexing purposes).
   Doesn't matter on RS/6000.  */
#define FUNCTION_MODE SImode

/* Define this if addresses of constant functions
   shouldn't be put through pseudo regs where they can be cse'd.
   Desirable on machines where ordinary constants are expensive
   but a CALL with constant address is cheap.  */
#define NO_FUNCTION_CSE 1

/* Define this to be nonzero if shift instructions ignore all but the low-order
   few bits.

   The sle and sre instructions which allow SHIFT_COUNT_TRUNCATED
   have been dropped from the PowerPC architecture.  */
#define SHIFT_COUNT_TRUNCATED 0

/* Adjust the length of an INSN.  LENGTH is the currently-computed length and
   should be adjusted to reflect any required changes.  This macro is used when
   there is some systematic length adjustment required that would be difficult
   to express in the length attribute.

   In the PowerPC, we use this to adjust the length of an instruction if one or
   more prefixed instructions are generated, using the attribute
   num_prefixed_insns.  A prefixed instruction is 8 bytes instead of 4, but the
   hardware requires that a prefied instruciton does not cross a 64-byte
   boundary.  This means the compiler has to assume the length of the first
   prefixed instruction is 12 bytes instead of 8 bytes.  Since the length is
   already set for the non-prefixed instruction, we just need to udpate for the
   difference.  */

#define ADJUST_INSN_LENGTH(INSN,LENGTH)					\
  (LENGTH) = rs6000_adjust_insn_length ((INSN), (LENGTH))

/* Given a comparison code (EQ, NE, etc.) and the first operand of a
   COMPARE, return the mode to be used for the comparison.  For
   floating-point, CCFPmode should be used.  CCUNSmode should be used
   for unsigned comparisons.  CCEQmode should be used when we are
   doing an inequality comparison on the result of a
   comparison.  CCmode should be used in all other cases.  */

#define SELECT_CC_MODE(OP,X,Y) \
  (SCALAR_FLOAT_MODE_P (GET_MODE (X)) ? CCFPmode	\
   : (OP) == GTU || (OP) == LTU || (OP) == GEU || (OP) == LEU ? CCUNSmode \
   : (((OP) == EQ || (OP) == NE) && COMPARISON_P (X)			  \
      ? CCEQmode : CCmode))

/* Can the condition code MODE be safely reversed?  This is safe in
   all cases on this port, because at present it doesn't use the
   trapping FP comparisons (fcmpo).  */
#define REVERSIBLE_CC_MODE(MODE) 1

/* Given a condition code and a mode, return the inverse condition.  */
#define REVERSE_CONDITION(CODE, MODE) rs6000_reverse_condition (MODE, CODE)


/* Target cpu costs.  */

struct processor_costs {
  const int mulsi;	  /* cost of SImode multiplication.  */
  const int mulsi_const;  /* cost of SImode multiplication by constant.  */
  const int mulsi_const9; /* cost of SImode mult by short constant.  */
  const int muldi;	  /* cost of DImode multiplication.  */
  const int divsi;	  /* cost of SImode division.  */
  const int divdi;	  /* cost of DImode division.  */
  const int fp;		  /* cost of simple SFmode and DFmode insns.  */
  const int dmul;	  /* cost of DFmode multiplication (and fmadd).  */
  const int sdiv;	  /* cost of SFmode division (fdivs).  */
  const int ddiv;	  /* cost of DFmode division (fdiv).  */
  const int cache_line_size;    /* cache line size in bytes. */
  const int l1_cache_size;	/* size of l1 cache, in kilobytes.  */
  const int l2_cache_size;	/* size of l2 cache, in kilobytes.  */
  const int simultaneous_prefetches; /* number of parallel prefetch
					operations.  */
  const int sfdf_convert;	/* cost of SF->DF conversion.  */
};

extern const struct processor_costs *rs6000_cost;

/* Control the assembler format that we output.  */

/* A C string constant describing how to begin a comment in the target
   assembler language.  The compiler assumes that the comment will end at
   the end of the line.  */
#define ASM_COMMENT_START " #"

/* Flag to say the TOC is initialized */
extern int toc_initialized;

/* Macro to output a special constant pool entry.  Go to WIN if we output
   it.  Otherwise, it is written the usual way.

   On the RS/6000, toc entries are handled this way.  */

#define ASM_OUTPUT_SPECIAL_POOL_ENTRY(FILE, X, MODE, ALIGN, LABELNO, WIN) \
{ if (ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (X, MODE))			  \
    {									  \
      output_toc (FILE, X, LABELNO, MODE);				  \
      goto WIN;								  \
    }									  \
}

#ifdef HAVE_GAS_WEAK
#define RS6000_WEAK 1
#else
#define RS6000_WEAK 0
#endif

#if RS6000_WEAK
/* Used in lieu of ASM_WEAKEN_LABEL.  */
#define        ASM_WEAKEN_DECL(FILE, DECL, NAME, VAL) \
  rs6000_asm_weaken_decl ((FILE), (DECL), (NAME), (VAL))
#endif

#if HAVE_GAS_WEAKREF
#define ASM_OUTPUT_WEAKREF(FILE, DECL, NAME, VALUE)			\
  do									\
    {									\
      fputs ("\t.weakref\t", (FILE));					\
      RS6000_OUTPUT_BASENAME ((FILE), (NAME)); 				\
      fputs (", ", (FILE));						\
      RS6000_OUTPUT_BASENAME ((FILE), (VALUE));				\
      if ((DECL) && TREE_CODE (DECL) == FUNCTION_DECL			\
	  && DEFAULT_ABI == ABI_AIX && DOT_SYMBOLS)			\
	{								\
	  fputs ("\n\t.weakref\t.", (FILE));				\
	  RS6000_OUTPUT_BASENAME ((FILE), (NAME)); 			\
	  fputs (", .", (FILE));					\
	  RS6000_OUTPUT_BASENAME ((FILE), (VALUE));			\
	}								\
      fputc ('\n', (FILE));						\
    } while (0)
#endif

/* This implements the `alias' attribute.  */
#undef	ASM_OUTPUT_DEF_FROM_DECLS
#define	ASM_OUTPUT_DEF_FROM_DECLS(FILE, DECL, TARGET)			\
  do									\
    {									\
      const char *alias = XSTR (XEXP (DECL_RTL (DECL), 0), 0);		\
      const char *name = IDENTIFIER_POINTER (TARGET);			\
      if (TREE_CODE (DECL) == FUNCTION_DECL				\
	  && DEFAULT_ABI == ABI_AIX && DOT_SYMBOLS)			\
	{								\
	  if (TREE_PUBLIC (DECL))					\
	    {								\
	      if (!RS6000_WEAK || !DECL_WEAK (DECL))			\
		{							\
		  fputs ("\t.globl\t.", FILE);				\
		  RS6000_OUTPUT_BASENAME (FILE, alias);			\
		  putc ('\n', FILE);					\
		}							\
	    }								\
	  else if (TARGET_XCOFF)					\
	    {								\
	      if (!RS6000_WEAK || !DECL_WEAK (DECL))			\
		{							\
		  fputs ("\t.lglobl\t.", FILE);				\
		  RS6000_OUTPUT_BASENAME (FILE, alias);			\
		  putc ('\n', FILE);					\
		  fputs ("\t.lglobl\t", FILE);				\
		  RS6000_OUTPUT_BASENAME (FILE, alias);			\
		  putc ('\n', FILE);					\
		}							\
	    }								\
	  fputs ("\t.set\t.", FILE);					\
	  RS6000_OUTPUT_BASENAME (FILE, alias);				\
	  fputs (",.", FILE);						\
	  RS6000_OUTPUT_BASENAME (FILE, name);				\
	  fputc ('\n', FILE);						\
	}								\
      ASM_OUTPUT_DEF (FILE, alias, name);				\
    }									\
   while (0)

#define TARGET_ASM_FILE_START rs6000_file_start

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON ""

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF ""

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

extern char rs6000_reg_names[][8];	/* register names (0 vs. %r0).  */

#define REGISTER_NAMES							\
{									\
  &rs6000_reg_names[ 0][0],	/* r0   */				\
  &rs6000_reg_names[ 1][0],	/* r1	*/				\
  &rs6000_reg_names[ 2][0],	/* r2	*/				\
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
  &rs6000_reg_names[32][0],	/* fr0  */				\
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
  &rs6000_reg_names[64][0],	/* vr0  */				\
  &rs6000_reg_names[65][0],	/* vr1  */				\
  &rs6000_reg_names[66][0],	/* vr2  */				\
  &rs6000_reg_names[67][0],	/* vr3  */				\
  &rs6000_reg_names[68][0],	/* vr4  */				\
  &rs6000_reg_names[69][0],	/* vr5  */				\
  &rs6000_reg_names[70][0],	/* vr6  */				\
  &rs6000_reg_names[71][0],	/* vr7  */				\
  &rs6000_reg_names[72][0],	/* vr8  */				\
  &rs6000_reg_names[73][0],	/* vr9  */				\
  &rs6000_reg_names[74][0],	/* vr10 */				\
  &rs6000_reg_names[75][0],	/* vr11 */				\
  &rs6000_reg_names[76][0],	/* vr12 */				\
  &rs6000_reg_names[77][0],	/* vr13 */				\
  &rs6000_reg_names[78][0],	/* vr14 */				\
  &rs6000_reg_names[79][0],	/* vr15 */				\
  &rs6000_reg_names[80][0],	/* vr16 */				\
  &rs6000_reg_names[81][0],	/* vr17 */				\
  &rs6000_reg_names[82][0],	/* vr18 */				\
  &rs6000_reg_names[83][0],	/* vr19 */				\
  &rs6000_reg_names[84][0],	/* vr20 */				\
  &rs6000_reg_names[85][0],	/* vr21 */				\
  &rs6000_reg_names[86][0],	/* vr22 */				\
  &rs6000_reg_names[87][0],	/* vr23 */				\
  &rs6000_reg_names[88][0],	/* vr24 */				\
  &rs6000_reg_names[89][0],	/* vr25 */				\
  &rs6000_reg_names[90][0],	/* vr26 */				\
  &rs6000_reg_names[91][0],	/* vr27 */				\
  &rs6000_reg_names[92][0],	/* vr28 */				\
  &rs6000_reg_names[93][0],	/* vr29 */				\
  &rs6000_reg_names[94][0],	/* vr30 */				\
  &rs6000_reg_names[95][0],	/* vr31 */				\
									\
  &rs6000_reg_names[96][0],	/* lr   */				\
  &rs6000_reg_names[97][0],	/* ctr  */				\
  &rs6000_reg_names[98][0],	/* ca  */				\
  &rs6000_reg_names[99][0],	/* ap   */				\
									\
  &rs6000_reg_names[100][0],	/* cr0  */				\
  &rs6000_reg_names[101][0],	/* cr1  */				\
  &rs6000_reg_names[102][0],	/* cr2  */				\
  &rs6000_reg_names[103][0],	/* cr3  */				\
  &rs6000_reg_names[104][0],	/* cr4  */				\
  &rs6000_reg_names[105][0],	/* cr5  */				\
  &rs6000_reg_names[106][0],	/* cr6  */				\
  &rs6000_reg_names[107][0],	/* cr7  */				\
									\
  &rs6000_reg_names[108][0],	/* vrsave  */				\
  &rs6000_reg_names[109][0],	/* vscr  */				\
									\
  &rs6000_reg_names[110][0]	/* sfp  */				\
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
  {"v0",   64}, {"v1",   65}, {"v2",   66}, {"v3",   67},	\
  {"v4",   68}, {"v5",   69}, {"v6",   70}, {"v7",   71},	\
  {"v8",   72}, {"v9",   73}, {"v10",  74}, {"v11",  75},	\
  {"v12",  76}, {"v13",  77}, {"v14",  78}, {"v15",  79},	\
  {"v16",  80}, {"v17",  81}, {"v18",  82}, {"v19",  83},	\
  {"v20",  84}, {"v21",  85}, {"v22",  86}, {"v23",  87},	\
  {"v24",  88}, {"v25",  89}, {"v26",  90}, {"v27",  91},	\
  {"v28",  92}, {"v29",  93}, {"v30",  94}, {"v31",  95},	\
  {"vrsave", 108}, {"vscr", 109},				\
  /* no additional names for: lr, ctr, ap */			\
  {"cr0",  100},{"cr1",  101},{"cr2",  102},{"cr3",  103},	\
  {"cr4",  104},{"cr5",  105},{"cr6",  106},{"cr7",  107},	\
  {"cc",   100},{"sp",    1}, {"toc",   2},			\
  /* CA is only part of XER, but we do not model the other parts (yet).  */ \
  {"xer",  98},							\
  /* VSX registers overlaid on top of FR, Altivec registers */	\
  {"vs0",  32}, {"vs1",  33}, {"vs2",  34}, {"vs3",  35},	\
  {"vs4",  36}, {"vs5",  37}, {"vs6",  38}, {"vs7",  39},	\
  {"vs8",  40}, {"vs9",  41}, {"vs10", 42}, {"vs11", 43},	\
  {"vs12", 44}, {"vs13", 45}, {"vs14", 46}, {"vs15", 47},	\
  {"vs16", 48}, {"vs17", 49}, {"vs18", 50}, {"vs19", 51},	\
  {"vs20", 52}, {"vs21", 53}, {"vs22", 54}, {"vs23", 55},	\
  {"vs24", 56}, {"vs25", 57}, {"vs26", 58}, {"vs27", 59},	\
  {"vs28", 60}, {"vs29", 61}, {"vs30", 62}, {"vs31", 63},	\
  {"vs32", 64}, {"vs33", 65}, {"vs34", 66}, {"vs35", 67},	\
  {"vs36", 68}, {"vs37", 69}, {"vs38", 70}, {"vs39", 71},	\
  {"vs40", 72}, {"vs41", 73}, {"vs42", 74}, {"vs43", 75},	\
  {"vs44", 76}, {"vs45", 77}, {"vs46", 78}, {"vs47", 79},	\
  {"vs48", 80}, {"vs49", 81}, {"vs50", 82}, {"vs51", 83},	\
  {"vs52", 84}, {"vs53", 85}, {"vs54", 86}, {"vs55", 87},	\
  {"vs56", 88}, {"vs57", 89}, {"vs58", 90}, {"vs59", 91},	\
  {"vs60", 92}, {"vs61", 93}, {"vs62", 94}, {"vs63", 95},	\
}

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

/* This is how to output an element of a case-vector
   that is non-relative.  */
#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE) \
  rs6000_output_addr_vec_elt ((FILE), (VALUE))

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) != 0)			\
    fprintf (FILE, "\t.align %d\n", (LOG))

/* How to align the given loop. */
#define LOOP_ALIGN(LABEL)  rs6000_loop_align(LABEL)

/* Alignment guaranteed by __builtin_malloc.  */
/* FIXME:  128-bit alignment is guaranteed by glibc for TARGET_64BIT.
   However, specifying the stronger guarantee currently leads to
   a regression in SPEC CPU2006 437.leslie3d.  The stronger
   guarantee should be implemented here once that's fixed.  */
#define MALLOC_ABI_ALIGNMENT (64)

/* Pick up the return address upon entry to a procedure. Used for
   dwarf2 unwind information.  This also enables the table driven
   mechanism.  */

#define INCOMING_RETURN_ADDR_RTX   gen_rtx_REG (Pmode, LR_REGNO)
#define DWARF_FRAME_RETURN_COLUMN  DWARF_FRAME_REGNUM (LR_REGNO)

/* Describe how we implement __builtin_eh_return.  */
#define EH_RETURN_DATA_REGNO(N) ((N) < 4 ? (N) + 3 : INVALID_REGNUM)
#define EH_RETURN_STACKADJ_RTX  gen_rtx_REG (Pmode, 10)

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */

#define PRINT_OPERAND(FILE, X, CODE)  print_operand (FILE, X, CODE)

/* Define which CODE values are valid.  */

#define PRINT_OPERAND_PUNCT_VALID_P(CODE)  ((CODE) == '&')

/* Print a memory address as an operand to reference that memory location.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR) print_operand_address (FILE, ADDR)

/* For switching between functions with different target attributes.  */
#define SWITCHABLE_TARGET 1

/* uncomment for disabling the corresponding default options */
/* #define  MACHINE_no_sched_interblock */
/* #define  MACHINE_no_sched_speculative */
/* #define  MACHINE_no_sched_speculative_load */

/* General flags.  */
extern int frame_pointer_needed;

/* Classification of the builtin functions as to which switches enable the
   builtin, and what attributes it should have.  We used to use the target
   flags macros, but we've run out of bits, so we now map the options into new
   settings used here.  */

/* Builtin operand count.  */
#define RS6000_BTC_UNARY	0x00000001	/* normal unary function.  */
#define RS6000_BTC_BINARY	0x00000002	/* normal binary function.  */
#define RS6000_BTC_TERNARY	0x00000003	/* normal ternary function.  */
#define RS6000_BTC_QUATERNARY	0x00000004	/* normal quaternary
						   function. */
#define RS6000_BTC_QUINARY	0x00000005	/* normal quinary function.  */
#define RS6000_BTC_SENARY	0x00000006	/* normal senary function.  */
#define RS6000_BTC_OPND_MASK	0x00000007	/* Mask to isolate operands. */

/* Builtin attributes.  */
#define RS6000_BTC_SPECIAL	0x00000000	/* Special function.  */
#define RS6000_BTC_PREDICATE	0x00000008	/* predicate function.  */
#define RS6000_BTC_ABS		0x00000010	/* Altivec/VSX ABS
						   function.  */
#define RS6000_BTC_DST		0x00000020	/* Altivec DST function.  */

#define RS6000_BTC_TYPE_MASK	0x0000003f	/* Mask to isolate types */

#define RS6000_BTC_MISC		0x00000000	/* No special attributes.  */
#define RS6000_BTC_CONST	0x00000100	/* Neither uses, nor
						   modifies global state.  */
#define RS6000_BTC_PURE		0x00000200	/* reads global
						   state/mem and does
						   not modify global state.  */
#define RS6000_BTC_FP		0x00000400	/* depends on rounding mode.  */
#define RS6000_BTC_QUAD		0x00000800	/* Uses a register quad.  */
#define RS6000_BTC_PAIR		0x00001000	/* Uses a register pair.  */
#define RS6000_BTC_QUADPAIR	0x00001800	/* Uses a quad and a pair.  */
#define RS6000_BTC_ATTR_MASK	0x00001f00	/* Mask of the attributes.  */

/* Miscellaneous information.  */
#define RS6000_BTC_SPR		0x01000000	/* function references SPRs.  */
#define RS6000_BTC_VOID		0x02000000	/* function has no return value.  */
#define RS6000_BTC_CR		0x04000000	/* function references a CR.  */
#define RS6000_BTC_OVERLOADED	0x08000000	/* function is overloaded.  */
#define RS6000_BTC_GIMPLE	0x10000000	/* function should be expanded
						   into gimple.  */
#define RS6000_BTC_MISC_MASK	0x1f000000	/* Mask of the misc info.  */

/* Convenience macros to document the instruction type.  */
#define RS6000_BTC_MEM		RS6000_BTC_MISC	/* load/store touches mem.  */
#define RS6000_BTC_SAT		RS6000_BTC_MISC	/* saturate sets VSCR.  */

/* Builtin targets.  For now, we reuse the masks for those options that are in
   target flags, and pick a random bit for ldbl128, which isn't in
   target_flags.  */
#define RS6000_BTM_ALWAYS	0		/* Always enabled.  */
#define RS6000_BTM_ALTIVEC	MASK_ALTIVEC	/* VMX/altivec vectors.  */
#define RS6000_BTM_CMPB		MASK_CMPB	/* ISA 2.05: compare bytes.  */
#define RS6000_BTM_VSX		MASK_VSX	/* VSX (vector/scalar).  */
#define RS6000_BTM_P8_VECTOR	MASK_P8_VECTOR	/* ISA 2.07 vector.  */
#define RS6000_BTM_P9_VECTOR	MASK_P9_VECTOR	/* ISA 3.0 vector.  */
#define RS6000_BTM_P9_MISC	MASK_P9_MISC	/* ISA 3.0 misc. non-vector */
#define RS6000_BTM_CRYPTO	MASK_CRYPTO	/* crypto funcs.  */
#define RS6000_BTM_HTM		MASK_HTM	/* hardware TM funcs.  */
#define RS6000_BTM_FRE		MASK_POPCNTB	/* FRE instruction.  */
#define RS6000_BTM_FRES		MASK_PPC_GFXOPT	/* FRES instruction.  */
#define RS6000_BTM_FRSQRTE	MASK_PPC_GFXOPT	/* FRSQRTE instruction.  */
#define RS6000_BTM_FRSQRTES	MASK_POPCNTB	/* FRSQRTES instruction.  */
#define RS6000_BTM_POPCNTD	MASK_POPCNTD	/* Target supports ISA 2.06.  */
#define RS6000_BTM_CELL		MASK_FPRND	/* Target is cell powerpc.  */
#define RS6000_BTM_DFP		MASK_DFP	/* Decimal floating point.  */
#define RS6000_BTM_HARD_FLOAT	MASK_SOFT_FLOAT	/* Hardware floating point.  */
#define RS6000_BTM_LDBL128	MASK_MULTIPLE	/* 128-bit long double.  */
#define RS6000_BTM_64BIT	MASK_64BIT	/* 64-bit addressing.  */
#define RS6000_BTM_POWERPC64	MASK_POWERPC64	/* 64-bit registers.  */
#define RS6000_BTM_FLOAT128	MASK_FLOAT128_KEYWORD /* IEEE 128-bit float.  */
#define RS6000_BTM_FLOAT128_HW	MASK_FLOAT128_HW /* IEEE 128-bit float h/w.  */
#define RS6000_BTM_MMA		MASK_MMA	/* ISA 3.1 MMA.  */
#define RS6000_BTM_P10		MASK_POWER10


#define RS6000_BTM_COMMON	(RS6000_BTM_ALTIVEC			\
				 | RS6000_BTM_VSX			\
				 | RS6000_BTM_P8_VECTOR			\
				 | RS6000_BTM_P9_VECTOR			\
				 | RS6000_BTM_P9_MISC			\
				 | RS6000_BTM_MODULO                    \
				 | RS6000_BTM_CRYPTO			\
				 | RS6000_BTM_FRE			\
				 | RS6000_BTM_FRES			\
				 | RS6000_BTM_FRSQRTE			\
				 | RS6000_BTM_FRSQRTES			\
				 | RS6000_BTM_HTM			\
				 | RS6000_BTM_POPCNTD			\
				 | RS6000_BTM_CELL			\
				 | RS6000_BTM_DFP			\
				 | RS6000_BTM_HARD_FLOAT		\
				 | RS6000_BTM_LDBL128			\
				 | RS6000_BTM_POWERPC64			\
				 | RS6000_BTM_FLOAT128			\
				 | RS6000_BTM_FLOAT128_HW		\
				 | RS6000_BTM_MMA			\
				 | RS6000_BTM_P10)

/* Define builtin enum index.  */

#undef RS6000_BUILTIN_0
#undef RS6000_BUILTIN_1
#undef RS6000_BUILTIN_2
#undef RS6000_BUILTIN_3
#undef RS6000_BUILTIN_4
#undef RS6000_BUILTIN_A
#undef RS6000_BUILTIN_D
#undef RS6000_BUILTIN_H
#undef RS6000_BUILTIN_M
#undef RS6000_BUILTIN_P
#undef RS6000_BUILTIN_X

#define RS6000_BUILTIN_0(ENUM, NAME, MASK, ATTR, ICODE) ENUM,
#define RS6000_BUILTIN_1(ENUM, NAME, MASK, ATTR, ICODE) ENUM,
#define RS6000_BUILTIN_2(ENUM, NAME, MASK, ATTR, ICODE) ENUM,
#define RS6000_BUILTIN_3(ENUM, NAME, MASK, ATTR, ICODE) ENUM,
#define RS6000_BUILTIN_4(ENUM, NAME, MASK, ATTR, ICODE) ENUM,
#define RS6000_BUILTIN_A(ENUM, NAME, MASK, ATTR, ICODE) ENUM,
#define RS6000_BUILTIN_D(ENUM, NAME, MASK, ATTR, ICODE) ENUM,
#define RS6000_BUILTIN_H(ENUM, NAME, MASK, ATTR, ICODE) ENUM,
#define RS6000_BUILTIN_M(ENUM, NAME, MASK, ATTR, ICODE) ENUM,
#define RS6000_BUILTIN_P(ENUM, NAME, MASK, ATTR, ICODE) ENUM,
#define RS6000_BUILTIN_X(ENUM, NAME, MASK, ATTR, ICODE) ENUM,

enum rs6000_builtins
{
#include "rs6000-builtin.def"

  RS6000_BUILTIN_COUNT
};

#undef RS6000_BUILTIN_0
#undef RS6000_BUILTIN_1
#undef RS6000_BUILTIN_2
#undef RS6000_BUILTIN_3
#undef RS6000_BUILTIN_4
#undef RS6000_BUILTIN_A
#undef RS6000_BUILTIN_D
#undef RS6000_BUILTIN_H
#undef RS6000_BUILTIN_M
#undef RS6000_BUILTIN_P
#undef RS6000_BUILTIN_X

/* Mappings for overloaded builtins.  */
struct altivec_builtin_types
{
  enum rs6000_builtins code;
  enum rs6000_builtins overloaded_code;
  signed char ret_type;
  signed char op1;
  signed char op2;
  signed char op3;
};
extern const struct altivec_builtin_types altivec_overloaded_builtins[];

enum rs6000_builtin_type_index
{
  RS6000_BTI_NOT_OPAQUE,
  RS6000_BTI_opaque_V4SI,
  RS6000_BTI_V16QI,              /* __vector signed char */
  RS6000_BTI_V1TI,
  RS6000_BTI_V2DI,
  RS6000_BTI_V2DF,
  RS6000_BTI_V4HI,
  RS6000_BTI_V4SI,
  RS6000_BTI_V4SF,
  RS6000_BTI_V8HI,
  RS6000_BTI_unsigned_V16QI,     /* __vector unsigned char */
  RS6000_BTI_unsigned_V1TI,
  RS6000_BTI_unsigned_V8HI,
  RS6000_BTI_unsigned_V4SI,
  RS6000_BTI_unsigned_V2DI,
  RS6000_BTI_bool_char,          /* __bool char */
  RS6000_BTI_bool_short,         /* __bool short */
  RS6000_BTI_bool_int,           /* __bool int */
  RS6000_BTI_bool_long_long,     /* __bool long long */
  RS6000_BTI_pixel,              /* __pixel (16 bits arranged as 4
				    channels of 1, 5, 5, and 5 bits
				    respectively as packed with the
				    vpkpx insn.  __pixel is only
				    meaningful as a vector type.
				    There is no corresponding scalar
				    __pixel data type.)  */
  RS6000_BTI_bool_V16QI,         /* __vector __bool char */
  RS6000_BTI_bool_V8HI,          /* __vector __bool short */
  RS6000_BTI_bool_V4SI,          /* __vector __bool int */
  RS6000_BTI_bool_V2DI,          /* __vector __bool long */
  RS6000_BTI_pixel_V8HI,         /* __vector __pixel */
  RS6000_BTI_long,	         /* long_integer_type_node */
  RS6000_BTI_unsigned_long,      /* long_unsigned_type_node */
  RS6000_BTI_long_long,	         /* long_long_integer_type_node */
  RS6000_BTI_unsigned_long_long, /* long_long_unsigned_type_node */
  RS6000_BTI_INTQI,	         /* (signed) intQI_type_node */
  RS6000_BTI_UINTQI,		 /* unsigned_intQI_type_node */
  RS6000_BTI_INTHI,	         /* intHI_type_node */
  RS6000_BTI_UINTHI,		 /* unsigned_intHI_type_node */
  RS6000_BTI_INTSI,		 /* intSI_type_node (signed) */
  RS6000_BTI_UINTSI,		 /* unsigned_intSI_type_node */
  RS6000_BTI_INTDI,		 /* intDI_type_node */
  RS6000_BTI_UINTDI,		 /* unsigned_intDI_type_node */
  RS6000_BTI_INTTI,		 /* intTI_type_node */
  RS6000_BTI_UINTTI,		 /* unsigned_intTI_type_node */
  RS6000_BTI_float,	         /* float_type_node */
  RS6000_BTI_double,	         /* double_type_node */
  RS6000_BTI_long_double,        /* long_double_type_node */
  RS6000_BTI_dfloat64,		 /* dfloat64_type_node */
  RS6000_BTI_dfloat128,		 /* dfloat128_type_node */
  RS6000_BTI_void,	         /* void_type_node */
  RS6000_BTI_ieee128_float,	 /* ieee 128-bit floating point */
  RS6000_BTI_ibm128_float,	 /* IBM 128-bit floating point */
  RS6000_BTI_const_str,		 /* pointer to const char * */
  RS6000_BTI_vector_pair,	 /* unsigned 256-bit types (vector pair).  */
  RS6000_BTI_vector_quad,	 /* unsigned 512-bit types (vector quad).  */
  RS6000_BTI_MAX
};


#define opaque_V4SI_type_node         (rs6000_builtin_types[RS6000_BTI_opaque_V4SI])
#define V16QI_type_node               (rs6000_builtin_types[RS6000_BTI_V16QI])
#define V1TI_type_node                (rs6000_builtin_types[RS6000_BTI_V1TI])
#define V2DI_type_node                (rs6000_builtin_types[RS6000_BTI_V2DI])
#define V2DF_type_node                (rs6000_builtin_types[RS6000_BTI_V2DF])
#define V4HI_type_node                (rs6000_builtin_types[RS6000_BTI_V4HI])
#define V4SI_type_node                (rs6000_builtin_types[RS6000_BTI_V4SI])
#define V4SF_type_node                (rs6000_builtin_types[RS6000_BTI_V4SF])
#define V8HI_type_node                (rs6000_builtin_types[RS6000_BTI_V8HI])
#define unsigned_V16QI_type_node      (rs6000_builtin_types[RS6000_BTI_unsigned_V16QI])
#define unsigned_V1TI_type_node       (rs6000_builtin_types[RS6000_BTI_unsigned_V1TI])
#define unsigned_V8HI_type_node       (rs6000_builtin_types[RS6000_BTI_unsigned_V8HI])
#define unsigned_V4SI_type_node       (rs6000_builtin_types[RS6000_BTI_unsigned_V4SI])
#define unsigned_V2DI_type_node       (rs6000_builtin_types[RS6000_BTI_unsigned_V2DI])
#define bool_char_type_node           (rs6000_builtin_types[RS6000_BTI_bool_char])
#define bool_short_type_node          (rs6000_builtin_types[RS6000_BTI_bool_short])
#define bool_int_type_node            (rs6000_builtin_types[RS6000_BTI_bool_int])
#define bool_long_long_type_node      (rs6000_builtin_types[RS6000_BTI_bool_long_long])
#define pixel_type_node               (rs6000_builtin_types[RS6000_BTI_pixel])
#define bool_V16QI_type_node	      (rs6000_builtin_types[RS6000_BTI_bool_V16QI])
#define bool_V8HI_type_node	      (rs6000_builtin_types[RS6000_BTI_bool_V8HI])
#define bool_V4SI_type_node	      (rs6000_builtin_types[RS6000_BTI_bool_V4SI])
#define bool_V2DI_type_node	      (rs6000_builtin_types[RS6000_BTI_bool_V2DI])
#define pixel_V8HI_type_node	      (rs6000_builtin_types[RS6000_BTI_pixel_V8HI])

#define long_long_integer_type_internal_node  (rs6000_builtin_types[RS6000_BTI_long_long])
#define long_long_unsigned_type_internal_node (rs6000_builtin_types[RS6000_BTI_unsigned_long_long])
#define long_integer_type_internal_node  (rs6000_builtin_types[RS6000_BTI_long])
#define long_unsigned_type_internal_node (rs6000_builtin_types[RS6000_BTI_unsigned_long])
#define intQI_type_internal_node	 (rs6000_builtin_types[RS6000_BTI_INTQI])
#define uintQI_type_internal_node	 (rs6000_builtin_types[RS6000_BTI_UINTQI])
#define intHI_type_internal_node	 (rs6000_builtin_types[RS6000_BTI_INTHI])
#define uintHI_type_internal_node	 (rs6000_builtin_types[RS6000_BTI_UINTHI])
#define intSI_type_internal_node	 (rs6000_builtin_types[RS6000_BTI_INTSI])
#define uintSI_type_internal_node	 (rs6000_builtin_types[RS6000_BTI_UINTSI])
#define intDI_type_internal_node	 (rs6000_builtin_types[RS6000_BTI_INTDI])
#define uintDI_type_internal_node	 (rs6000_builtin_types[RS6000_BTI_UINTDI])
#define intTI_type_internal_node	 (rs6000_builtin_types[RS6000_BTI_INTTI])
#define uintTI_type_internal_node	 (rs6000_builtin_types[RS6000_BTI_UINTTI])
#define float_type_internal_node	 (rs6000_builtin_types[RS6000_BTI_float])
#define double_type_internal_node	 (rs6000_builtin_types[RS6000_BTI_double])
#define long_double_type_internal_node	 (rs6000_builtin_types[RS6000_BTI_long_double])
#define dfloat64_type_internal_node	 (rs6000_builtin_types[RS6000_BTI_dfloat64])
#define dfloat128_type_internal_node	 (rs6000_builtin_types[RS6000_BTI_dfloat128])
#define void_type_internal_node		 (rs6000_builtin_types[RS6000_BTI_void])
#define ieee128_float_type_node		 (rs6000_builtin_types[RS6000_BTI_ieee128_float])
#define ibm128_float_type_node		 (rs6000_builtin_types[RS6000_BTI_ibm128_float])
#define const_str_type_node		 (rs6000_builtin_types[RS6000_BTI_const_str])
#define vector_pair_type_node		 (rs6000_builtin_types[RS6000_BTI_vector_pair])
#define vector_quad_type_node		 (rs6000_builtin_types[RS6000_BTI_vector_quad])

extern GTY(()) tree rs6000_builtin_types[RS6000_BTI_MAX];
extern GTY(()) tree rs6000_builtin_decls[RS6000_BUILTIN_COUNT];

#ifndef USED_FOR_TARGET
extern GTY(()) tree builtin_mode_to_type[MAX_MACHINE_MODE][2];
extern GTY(()) tree altivec_builtin_mask_for_load;
extern GTY(()) section *toc_section;

/* A C structure for machine-specific, per-function data.
   This is added to the cfun structure.  */
typedef struct GTY(()) machine_function
{
  /* Flags if __builtin_return_address (n) with n >= 1 was used.  */
  int ra_needs_full_frame;
  /* Flags if __builtin_return_address (0) was used.  */
  int ra_need_lr;
  /* Cache lr_save_p after expansion of builtin_eh_return.  */
  int lr_save_state;
  /* Whether we need to save the TOC to the reserved stack location in the
     function prologue.  */
  bool save_toc_in_prologue;
  /* Offset from virtual_stack_vars_rtx to the start of the ABI_V4
     varargs save area.  */
  HOST_WIDE_INT varargs_save_offset;
  /* Alternative internal arg pointer for -fsplit-stack.  */
  rtx split_stack_arg_pointer;
  bool split_stack_argp_used;
  /* Flag if r2 setup is needed with ELFv2 ABI.  */
  bool r2_setup_needed;
  /* The number of components we use for separate shrink-wrapping.  */
  int n_components;
  /* The components already handled by separate shrink-wrapping, which should
     not be considered by the prologue and epilogue.  */
  bool gpr_is_wrapped_separately[32];
  bool fpr_is_wrapped_separately[32];
  bool lr_is_wrapped_separately;
  bool toc_is_wrapped_separately;
  bool mma_return_type_error;
} machine_function;
#endif


#define TARGET_SUPPORTS_WIDE_INT 1

#if (GCC_VERSION >= 3000)
#pragma GCC poison TARGET_FLOAT128 OPTION_MASK_FLOAT128 MASK_FLOAT128
#endif

/* Whether a given VALUE is a valid 16 or 34-bit signed integer.  */
#define SIGNED_INTEGER_NBIT_P(VALUE, N)					\
  IN_RANGE ((VALUE),							\
	    -(HOST_WIDE_INT_1 << ((N)-1)),				\
	    (HOST_WIDE_INT_1 << ((N)-1)) - 1)

#define SIGNED_INTEGER_16BIT_P(VALUE)	SIGNED_INTEGER_NBIT_P (VALUE, 16)
#define SIGNED_INTEGER_34BIT_P(VALUE)	SIGNED_INTEGER_NBIT_P (VALUE, 34)

/* Like SIGNED_INTEGER_16BIT_P and SIGNED_INTEGER_34BIT_P, but with an extra
   argument that gives a length to validate a range of addresses, to allow for
   splitting insns into several insns, each of which has an offsettable
   address.  */
#define SIGNED_16BIT_OFFSET_EXTRA_P(VALUE, EXTRA)			\
  IN_RANGE ((VALUE),							\
	    -(HOST_WIDE_INT_1 << 15),					\
	    (HOST_WIDE_INT_1 << 15) - 1 - (EXTRA))

#define SIGNED_34BIT_OFFSET_EXTRA_P(VALUE, EXTRA)			\
  IN_RANGE ((VALUE),							\
	    -(HOST_WIDE_INT_1 << 33),					\
	    (HOST_WIDE_INT_1 << 33) - 1 - (EXTRA))

/* Define this if some processing needs to be done before outputting the
   assembler code.  On the PowerPC, we remember if the current insn is a normal
   prefixed insn where we need to emit a 'p' before the insn.  */
#define FINAL_PRESCAN_INSN(INSN, OPERANDS, NOPERANDS)			\
do									\
  {									\
    if (TARGET_PREFIXED)						\
      rs6000_final_prescan_insn (INSN, OPERANDS, NOPERANDS);		\
  }									\
while (0)

/* Do anything special before emitting an opcode.  We use it to emit a 'p' for
   prefixed insns that is set in FINAL_PRESCAN_INSN.  */
#define ASM_OUTPUT_OPCODE(STREAM, OPCODE)				\
  do									\
    {									\
     if (TARGET_PREFIXED)						\
       rs6000_asm_output_opcode (STREAM);				\
    }									\
  while (0)
