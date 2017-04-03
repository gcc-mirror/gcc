/* Definitions of target machine for GNU compiler, for IBM RS/6000.
   Copyright (C) 1992-2017 Free Software Foundation, Inc.
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

/* Definitions for the object file format.  These are set at
   compile-time.  */

#define OBJECT_XCOFF 1
#define OBJECT_ELF 2
#define OBJECT_PEF 3
#define OBJECT_MACHO 4

#define TARGET_ELF (TARGET_OBJECT_FORMAT == OBJECT_ELF)
#define TARGET_XCOFF (TARGET_OBJECT_FORMAT == OBJECT_XCOFF)
#define TARGET_MACOS (TARGET_OBJECT_FORMAT == OBJECT_PEF)
#define TARGET_MACHO (TARGET_OBJECT_FORMAT == OBJECT_MACHO)

#ifndef TARGET_AIX
#define TARGET_AIX 0
#endif

#ifndef TARGET_AIX_OS
#define TARGET_AIX_OS 0
#endif

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

#ifndef TARGET_PAIRED_FLOAT
#define TARGET_PAIRED_FLOAT 0
#endif

#ifdef HAVE_AS_POPCNTB
#define ASM_CPU_POWER5_SPEC "-mpower5"
#else
#define ASM_CPU_POWER5_SPEC "-mpower4"
#endif

#ifdef HAVE_AS_DFP
#define ASM_CPU_POWER6_SPEC "-mpower6 -maltivec"
#else
#define ASM_CPU_POWER6_SPEC "-mpower4 -maltivec"
#endif

#ifdef HAVE_AS_POPCNTD
#define ASM_CPU_POWER7_SPEC "-mpower7"
#else
#define ASM_CPU_POWER7_SPEC "-mpower4 -maltivec"
#endif

#ifdef HAVE_AS_POWER8
#define ASM_CPU_POWER8_SPEC "-mpower8"
#else
#define ASM_CPU_POWER8_SPEC ASM_CPU_POWER7_SPEC
#endif

#ifdef HAVE_AS_POWER9
#define ASM_CPU_POWER9_SPEC "-mpower9"
#else
#define ASM_CPU_POWER9_SPEC ASM_CPU_POWER8_SPEC
#endif

#ifdef HAVE_AS_DCI
#define ASM_CPU_476_SPEC "-m476"
#else
#define ASM_CPU_476_SPEC "-mpower4"
#endif

/* Common ASM definitions used by ASM_SPEC among the various targets for
   handling -mcpu=xxx switches.  There is a parallel list in driver-rs6000.c to
   provide the default assembler options if the user uses -mcpu=native, so if
   you make changes here, make them also there.  */
#define ASM_CPU_SPEC \
"%{!mcpu*: \
  %{mpowerpc64*: -mppc64} \
  %{!mpowerpc64*: %(asm_default)}} \
%{mcpu=native: %(asm_cpu_native)} \
%{mcpu=cell: -mcell} \
%{mcpu=power3: -mppc64} \
%{mcpu=power4: -mpower4} \
%{mcpu=power5: %(asm_cpu_power5)} \
%{mcpu=power5+: %(asm_cpu_power5)} \
%{mcpu=power6: %(asm_cpu_power6) -maltivec} \
%{mcpu=power6x: %(asm_cpu_power6) -maltivec} \
%{mcpu=power7: %(asm_cpu_power7)} \
%{mcpu=power8: %(asm_cpu_power8)} \
%{mcpu=power9: %(asm_cpu_power9)} \
%{mcpu=a2: -ma2} \
%{mcpu=powerpc: -mppc} \
%{mcpu=powerpc64le: %(asm_cpu_power8)} \
%{mcpu=rs64a: -mppc64} \
%{mcpu=401: -mppc} \
%{mcpu=403: -m403} \
%{mcpu=405: -m405} \
%{mcpu=405fp: -m405} \
%{mcpu=440: -m440} \
%{mcpu=440fp: -m440} \
%{mcpu=464: -m440} \
%{mcpu=464fp: -m440} \
%{mcpu=476: %(asm_cpu_476)} \
%{mcpu=476fp: %(asm_cpu_476)} \
%{mcpu=505: -mppc} \
%{mcpu=601: -m601} \
%{mcpu=602: -mppc} \
%{mcpu=603: -mppc} \
%{mcpu=603e: -mppc} \
%{mcpu=ec603e: -mppc} \
%{mcpu=604: -mppc} \
%{mcpu=604e: -mppc} \
%{mcpu=620: -mppc64} \
%{mcpu=630: -mppc64} \
%{mcpu=740: -mppc} \
%{mcpu=750: -mppc} \
%{mcpu=G3: -mppc} \
%{mcpu=7400: -mppc -maltivec} \
%{mcpu=7450: -mppc -maltivec} \
%{mcpu=G4: -mppc -maltivec} \
%{mcpu=801: -mppc} \
%{mcpu=821: -mppc} \
%{mcpu=823: -mppc} \
%{mcpu=860: -mppc} \
%{mcpu=970: -mpower4 -maltivec} \
%{mcpu=G5: -mpower4 -maltivec} \
%{mcpu=8540: -me500} \
%{mcpu=8548: -me500} \
%{mcpu=e300c2: -me300} \
%{mcpu=e300c3: -me300} \
%{mcpu=e500mc: -me500mc} \
%{mcpu=e500mc64: -me500mc64} \
%{mcpu=e5500: -me5500} \
%{mcpu=e6500: -me6500} \
%{maltivec: -maltivec} \
%{mvsx: -mvsx %{!maltivec: -maltivec} %{!mcpu*: %(asm_cpu_power7)}} \
%{mpower8-vector|mcrypto|mdirect-move|mhtm: %{!mcpu*: %(asm_cpu_power8)}} \
-many"

#define CPP_DEFAULT_SPEC ""

#define ASM_DEFAULT_SPEC ""

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
  { "asm_default",		ASM_DEFAULT_SPEC },			\
  { "cc1_cpu",			CC1_CPU_SPEC },				\
  { "asm_cpu_power5",		ASM_CPU_POWER5_SPEC },			\
  { "asm_cpu_power6",		ASM_CPU_POWER6_SPEC },			\
  { "asm_cpu_power7",		ASM_CPU_POWER7_SPEC },			\
  { "asm_cpu_power8",		ASM_CPU_POWER8_SPEC },			\
  { "asm_cpu_power9",		ASM_CPU_POWER9_SPEC },			\
  { "asm_cpu_476",		ASM_CPU_476_SPEC },			\
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

/* Define TARGET_POPCNTB if the target assembler does not support the
   popcount byte instruction.  */

#ifndef HAVE_AS_POPCNTB
#undef  TARGET_POPCNTB
#define TARGET_POPCNTB 0
#endif

/* Define TARGET_FPRND if the target assembler does not support the
   fp rounding instructions.  */

#ifndef HAVE_AS_FPRND
#undef  TARGET_FPRND
#define TARGET_FPRND 0
#endif

/* Define TARGET_CMPB if the target assembler does not support the
   cmpb instruction.  */

#ifndef HAVE_AS_CMPB
#undef  TARGET_CMPB
#define TARGET_CMPB 0
#endif

/* Define TARGET_MFPGPR if the target assembler does not support the
   mffpr and mftgpr instructions. */

#ifndef HAVE_AS_MFPGPR
#undef  TARGET_MFPGPR
#define TARGET_MFPGPR 0
#endif

/* Define TARGET_DFP if the target assembler does not support decimal
   floating point instructions.  */
#ifndef HAVE_AS_DFP
#undef  TARGET_DFP
#define TARGET_DFP 0
#endif

/* Define TARGET_POPCNTD if the target assembler does not support the
   popcount word and double word instructions.  */

#ifndef HAVE_AS_POPCNTD
#undef  TARGET_POPCNTD
#define TARGET_POPCNTD 0
#endif

/* Define the ISA 2.07 flags as 0 if the target assembler does not support the
   waitasecond instruction.  Allow -mpower8-fusion, since it does not add new
   instructions.  */

#ifndef HAVE_AS_POWER8
#undef  TARGET_DIRECT_MOVE
#undef  TARGET_CRYPTO
#undef  TARGET_HTM
#undef  TARGET_P8_VECTOR
#define TARGET_DIRECT_MOVE 0
#define TARGET_CRYPTO 0
#define TARGET_HTM 0
#define TARGET_P8_VECTOR 0
#endif

/* Define the ISA 3.0 flags as 0 if the target assembler does not support
   Power9 instructions.  Allow -mpower9-fusion, since it does not add new
   instructions.  Allow -misel, since it predates ISA 3.0 and does
   not require any Power9 features.  */

#ifndef HAVE_AS_POWER9
#undef  TARGET_FLOAT128_HW
#undef  TARGET_MODULO
#undef  TARGET_P9_VECTOR
#undef  TARGET_P9_MINMAX
#undef  TARGET_P9_DFORM_SCALAR
#undef  TARGET_P9_DFORM_VECTOR
#undef  TARGET_P9_MISC
#define TARGET_FLOAT128_HW 0
#define TARGET_MODULO 0
#define TARGET_P9_VECTOR 0
#define TARGET_P9_MINMAX 0
#define TARGET_P9_DFORM_SCALAR 0
#define TARGET_P9_DFORM_VECTOR 0
#define TARGET_P9_MISC 0
#endif

/* Define TARGET_LWSYNC_INSTRUCTION if the assembler knows about lwsync.  If
   not, generate the lwsync code as an integer constant.  */
#ifdef HAVE_AS_LWSYNC
#define TARGET_LWSYNC_INSTRUCTION 1
#else
#define TARGET_LWSYNC_INSTRUCTION 0
#endif

/* Define TARGET_TLS_MARKERS if the target assembler does not support
   arg markers for __tls_get_addr calls.  */
#ifndef HAVE_AS_TLS_MARKERS
#undef  TARGET_TLS_MARKERS
#define TARGET_TLS_MARKERS 0
#else
#define TARGET_TLS_MARKERS tls_markers
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
  (GET_CODE (RTX) == SYMBOL_REF && SYMBOL_REF_TLS_MODEL (RTX) != 0)

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

#define TARGET_DEFAULT (MASK_MULTIPLE | MASK_STRING)

/* FPU operations supported. 
   Each use of TARGET_SINGLE_FLOAT or TARGET_DOUBLE_FLOAT must 
   also test TARGET_HARD_FLOAT.  */
#define TARGET_SINGLE_FLOAT 1
#define TARGET_DOUBLE_FLOAT 1
#define TARGET_SINGLE_FPU   0
#define TARGET_SIMPLE_FPU   0
#define TARGET_XILINX_FPU   0

/* Recast the processor type to the cpu attribute.  */
#define rs6000_cpu_attr ((enum attr_cpu)rs6000_cpu)

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
  ((TARGET_IEEEQUAD && ((MODE) == TFmode || (MODE) == TCmode))		\
   || ((MODE) == KFmode) || ((MODE) == KCmode))

#define FLOAT128_IBM_P(MODE)						\
  ((!TARGET_IEEEQUAD && ((MODE) == TFmode || (MODE) == TCmode))		\
   || (TARGET_HARD_FLOAT && TARGET_FPRS					\
       && ((MODE) == IFmode || (MODE) == ICmode)))

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

/* Determine the element order to use for vector instructions.  By
   default we use big-endian element order when targeting big-endian,
   and little-endian element order when targeting little-endian.  For
   programs being ported from BE Power to LE Power, it can sometimes
   be useful to use big-endian element order when targeting little-endian.
   This is set via -maltivec=be, for example.  */
#define VECTOR_ELT_ORDER_BIG                                  \
  (BYTES_BIG_ENDIAN || (rs6000_altivec_element_order == 2))

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

#define TARGET_LONG_DOUBLE_128 (rs6000_long_double_type_size == 128)
#define TARGET_IEEEQUAD rs6000_ieeequad
#define TARGET_ALTIVEC_ABI rs6000_altivec_abi
#define TARGET_LDBRX (TARGET_POPCNTD || rs6000_cpu == PROCESSOR_CELL)

#define TARGET_SPE_ABI 0
#define TARGET_SPE 0
#define TARGET_ISEL64 (TARGET_ISEL && TARGET_POWERPC64)
#define TARGET_FPRS 1
#define TARGET_E500_SINGLE 0
#define TARGET_E500_DOUBLE 0
#define CHECK_E500_OPTIONS do { } while (0)

/* ISA 2.01 allowed FCFID to be done in 32-bit, previously it was 64-bit only.
   Enable 32-bit fcfid's on any of the switches for newer ISA machines or
   XILINX.  */
#define TARGET_FCFID	(TARGET_POWERPC64				\
			 || TARGET_PPC_GPOPT	/* 970/power4 */	\
			 || TARGET_POPCNTB	/* ISA 2.02 */		\
			 || TARGET_CMPB		/* ISA 2.05 */		\
			 || TARGET_POPCNTD	/* ISA 2.06 */		\
			 || TARGET_XILINX_FPU)

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
#define TARGET_MADDLD	(TARGET_MODULO && TARGET_POWERPC64)

#define TARGET_XSCVDPSPN	(TARGET_DIRECT_MOVE || TARGET_P8_VECTOR)
#define TARGET_XSCVSPDPN	(TARGET_DIRECT_MOVE || TARGET_P8_VECTOR)
#define TARGET_VADDUQM		(TARGET_P8_VECTOR && TARGET_POWERPC64)
#define TARGET_DIRECT_MOVE_128	(TARGET_P9_VECTOR && TARGET_DIRECT_MOVE \
				 && TARGET_POWERPC64)
#define TARGET_VEXTRACTUB	(TARGET_P9_VECTOR && TARGET_DIRECT_MOVE \
				 && TARGET_UPPER_REGS_DI && TARGET_POWERPC64)


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
#define TARGET_MINMAX_SF	(TARGET_SF_FPR && TARGET_PPC_GFXOPT	\
				 && (TARGET_P9_MINMAX || !flag_trapping_math))

#define TARGET_MINMAX_DF	(TARGET_DF_FPR && TARGET_PPC_GFXOPT	\
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
#define MASK_FLOAT128_TYPE		OPTION_MASK_FLOAT128_TYPE
#define MASK_FPRND			OPTION_MASK_FPRND
#define MASK_P8_FUSION			OPTION_MASK_P8_FUSION
#define MASK_HARD_FLOAT			OPTION_MASK_HARD_FLOAT
#define MASK_HTM			OPTION_MASK_HTM
#define MASK_ISEL			OPTION_MASK_ISEL
#define MASK_MFCRF			OPTION_MASK_MFCRF
#define MASK_MFPGPR			OPTION_MASK_MFPGPR
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
#define MASK_STRING			OPTION_MASK_STRING
#define MASK_UPDATE			OPTION_MASK_UPDATE
#define MASK_VSX			OPTION_MASK_VSX
#define MASK_VSX_TIMODE			OPTION_MASK_VSX_TIMODE

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
   given system.  The SPE and Paired builtins are only enabled if you configure
   the compiler for those builtins, and those machines don't support altivec or
   VSX.  */

#define TARGET_EXTRA_BUILTINS	(!TARGET_SPE && !TARGET_PAIRED_FLOAT	 \
				 && ((TARGET_POWERPC64			 \
				      || TARGET_PPC_GPOPT /* 970/power4 */ \
				      || TARGET_POPCNTB	  /* ISA 2.02 */ \
				      || TARGET_CMPB	  /* ISA 2.05 */ \
				      || TARGET_POPCNTD	  /* ISA 2.06 */ \
				      || TARGET_ALTIVEC			 \
				      || TARGET_VSX			 \
				      || TARGET_HARD_FLOAT)))

/* E500 cores only support plain "sync", not lwsync.  */
#define TARGET_NO_LWSYNC (rs6000_cpu == PROCESSOR_PPC8540 \
			  || rs6000_cpu == PROCESSOR_PPC8548)


/* Whether SF/DF operations are supported on the E500.  */
#define TARGET_SF_SPE	(TARGET_HARD_FLOAT && TARGET_SINGLE_FLOAT	\
			 && !TARGET_FPRS)

#define TARGET_DF_SPE	(TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT	\
			 && !TARGET_FPRS && TARGET_E500_DOUBLE)

/* Whether SF/DF operations are supported by the normal floating point unit
   (or the vector/scalar unit).  */
#define TARGET_SF_FPR	(TARGET_HARD_FLOAT && TARGET_FPRS		\
			 && TARGET_SINGLE_FLOAT)

#define TARGET_DF_FPR	(TARGET_HARD_FLOAT && TARGET_FPRS		\
			 && TARGET_DOUBLE_FLOAT)

/* Whether SF/DF operations are supported by any hardware.  */
#define TARGET_SF_INSN	(TARGET_SF_FPR || TARGET_SF_SPE)
#define TARGET_DF_INSN	(TARGET_DF_FPR || TARGET_DF_SPE)

/* Which machine supports the various reciprocal estimate instructions.  */
#define TARGET_FRES	(TARGET_HARD_FLOAT && TARGET_PPC_GFXOPT \
			 && TARGET_FPRS && TARGET_SINGLE_FLOAT)

#define TARGET_FRE	(TARGET_HARD_FLOAT && TARGET_FPRS \
			 && TARGET_DOUBLE_FLOAT \
			 && (TARGET_POPCNTB || VECTOR_UNIT_VSX_P (DFmode)))

#define TARGET_FRSQRTES	(TARGET_HARD_FLOAT && TARGET_POPCNTB \
			 && TARGET_PPC_GFXOPT && TARGET_FPRS \
			 && TARGET_SINGLE_FLOAT)

#define TARGET_FRSQRTE	(TARGET_HARD_FLOAT && TARGET_FPRS \
			 && TARGET_DOUBLE_FLOAT \
			 && (TARGET_PPC_GFXOPT || VECTOR_UNIT_VSX_P (DFmode)))

/* Conditions to allow TOC fusion for loading/storing integers.  */
#define TARGET_TOC_FUSION_INT	(TARGET_P8_FUSION			\
				 && TARGET_TOC_FUSION			\
				 && (TARGET_CMODEL != CMODEL_SMALL)	\
				 && TARGET_POWERPC64)

/* Conditions to allow TOC fusion for loading/storing floating point.  */
#define TARGET_TOC_FUSION_FP	(TARGET_P9_FUSION			\
				 && TARGET_TOC_FUSION			\
				 && (TARGET_CMODEL != CMODEL_SMALL)	\
				 && TARGET_POWERPC64			\
				 && TARGET_HARD_FLOAT			\
				 && TARGET_FPRS				\
				 && TARGET_SINGLE_FLOAT			\
				 && TARGET_DOUBLE_FLOAT)

/* Macro to say whether we can do optimizations where we need to do parts of
   the calculation in 64-bit GPRs and then is transfered to the vector
   registers.  Do not allow -maltivec=be for these optimizations, because it
   adds to the complexity of the code.  */
#define TARGET_DIRECT_MOVE_64BIT	(TARGET_DIRECT_MOVE		\
					 && TARGET_P8_VECTOR		\
					 && TARGET_POWERPC64		\
					 && TARGET_UPPER_REGS_DI	\
					 && (rs6000_altivec_element_order != 2))

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
#define UNITS_PER_SPE_WORD 8
#define UNITS_PER_PAIRED_WORD 8

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

/* A C expression for the size in bits of the type `long double' on
   the target machine.  If you don't define this, the default is two
   words.  */
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

/* No data type wants to be aligned rounder than this.  */
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

/* Make strings word-aligned so strcpy from constants will be faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)                           \
  (TREE_CODE (EXP) == STRING_CST	                         \
   && (STRICT_ALIGNMENT || !optimize_size)                       \
   && (ALIGN) < BITS_PER_WORD                                    \
   ? BITS_PER_WORD                                               \
   : (ALIGN))

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN) \
  rs6000_data_alignment (TYPE, ALIGN, align_opt)

/* Align vectors to 128 bits.  Align SPE vectors and E500 v2 doubles to
   64 bits.  */
#define DATA_ABI_ALIGNMENT(TYPE, ALIGN) \
  rs6000_data_alignment (TYPE, ALIGN, align_abi)

/* Nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 0

/* Define this macro to be the value 1 if unaligned accesses have a cost
   many times greater than aligned accesses, for example if they are
   emulated in a trap handler.  */
/* Altivec vector memory instructions simply ignore the low bits; SPE vector
   memory instructions trap on unaligned accesses; VSX memory instructions are
   aligned to 4 or 8 bytes.  */
#define SLOW_UNALIGNED_ACCESS(MODE, ALIGN)				\
  (STRICT_ALIGNMENT							\
   || (!TARGET_EFFICIENT_UNALIGNED_VSX					\
       && ((SCALAR_FLOAT_MODE_NOT_VECTOR_P (MODE) && (ALIGN) < 32)	\
	   || ((VECTOR_MODE_P (MODE) || FLOAT128_VECTOR_P (MODE))	\
	       && (int) (ALIGN) < VECTOR_ALIGN (MODE)))))


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
   pointer, which is eventually eliminated in favor of SP or FP.

   The 3 HTM registers aren't also included in DWARF_FRAME_REGISTERS.  */

#define FIRST_PSEUDO_REGISTER 149

/* This must be included for pre gcc 3.0 glibc compatibility.  */
#define PRE_GCC3_DWARF_FRAME_REGISTERS 77

/* True if register is an SPE High register.  */
#define SPE_HIGH_REGNO_P(N) \
  ((N) >= FIRST_SPE_HIGH_REGNO && (N) <= LAST_SPE_HIGH_REGNO)

/* SPE high registers added as hard regs.
   The sfp register and 3 HTM registers
   aren't included in DWARF_FRAME_REGISTERS.  */
#define DWARF_FRAME_REGISTERS (FIRST_PSEUDO_REGISTER - 4)

/* The SPE has an additional 32 synthetic registers, with DWARF debug
   info numbering for these registers starting at 1200.  While eh_frame
   register numbering need not be the same as the debug info numbering,
   we choose to number these regs for eh_frame at 1200 too.

   We must map them here to avoid huge unwinder tables mostly consisting
   of unused space.  */
#define DWARF_REG_TO_UNWIND_COLUMN(r) \
  ((r) >= 1200 ? ((r) - 1200 + (DWARF_FRAME_REGISTERS - 32)) : (r))

/* Use standard DWARF numbering for DWARF debugging information.  */
#define DBX_REGISTER_NUMBER(REGNO) rs6000_dbx_register_number ((REGNO), 0)

/* Use gcc hard register numbering for eh_frame.  */
#define DWARF_FRAME_REGNUM(REGNO) (REGNO)

/* Map register numbers held in the call frame info that gcc has
   collected using DWARF_FRAME_REGNUM to those that should be output in
   .debug_frame and .eh_frame.  */
#define DWARF2_FRAME_REG_OUT(REGNO, FOR_EH) \
  rs6000_dbx_register_number ((REGNO), (FOR_EH)? 2 : 1)

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.

   On RS/6000, r1 is used for the stack.  On Darwin, r2 is available
   as a local register; for all other OS's r2 is the TOC pointer.

   On System V implementations, r13 is fixed and not available for use.  */

#define FIXED_REGISTERS  \
  {0, 1, FIXED_R2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, FIXED_R13, 0, 0, \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1,	   \
   /* AltiVec registers.  */			   \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   1, 1						   \
   , 1, 1, 1, 1, 1, 1,				   \
   /* SPE High registers.  */			   \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1  \
}

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
   1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1,	   \
   /* AltiVec registers.  */			   \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   1, 1						   \
   , 1, 1, 1, 1, 1, 1,				   \
   /* SPE High registers.  */			   \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1  \
}

/* Like `CALL_USED_REGISTERS' except this macro doesn't require that
   the entire set of `FIXED_REGISTERS' be included.
   (`CALL_USED_REGISTERS' must be a superset of `FIXED_REGISTERS').
   This macro is optional.  If not specified, it defaults to the value
   of `CALL_USED_REGISTERS'.  */

#define CALL_REALLY_USED_REGISTERS  \
  {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, FIXED_R13, 0, 0, \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1,	   \
   /* AltiVec registers.  */			   \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   0, 0						   \
   , 0, 0, 0, 0, 0, 0,				   \
   /* SPE High registers.  */			   \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0  \
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
	spe_acc, spefscr (fixed)
	sfp		(fixed)
	tfhar		(fixed)
	tfiar		(fixed)
	texasr		(fixed)
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
   75, 73, 74, 69, 68, 72, 71, 70,				\
   MAYBE_R2_AVAILABLE						\
   9, 10, 8, 7, 6, 5, 4,					\
   3, EARLY_R12 11, 0,						\
   31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19,		\
   18, 17, 16, 15, 14, 13, LATE_R12				\
   66, 65,							\
   1, MAYBE_R2_FIXED 67, 76,					\
   /* AltiVec registers.  */					\
   77, 78,							\
   90, 89, 88, 87, 86, 85, 84, 83, 82, 81, 80,			\
   79,								\
   96, 95, 94, 93, 92, 91,					\
   108, 107, 106, 105, 104, 103, 102, 101, 100, 99, 98, 97,	\
   109, 110,							\
   111, 112, 113, 114, 115, 116,				\
   117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,  \
   129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140,  \
   141, 142, 143, 144, 145, 146, 147, 148			\
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

/* SPE SIMD registers are just the GPRs.  */
#define SPE_SIMD_REGNO_P(N) ((N) <= 31)

/* PAIRED SIMD registers are just the FPRs.  */
#define PAIRED_SIMD_REGNO_P(N) ((N) >= 32 && (N) <= 63)

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

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.  */

#define HARD_REGNO_NREGS(REGNO, MODE) rs6000_hard_regno_nregs[(MODE)][(REGNO)]

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
   : TARGET_E500_DOUBLE && (MODE) == SImode				\
   ? SImode								\
   : TARGET_E500_DOUBLE && ((MODE) == VOIDmode || (MODE) == DFmode)	\
   ? DFmode								\
   : !TARGET_E500_DOUBLE && FLOAT128_IBM_P (MODE) && FP_REGNO_P (REGNO)	\
   ? DFmode								\
   : !TARGET_E500_DOUBLE && (MODE) == TDmode && FP_REGNO_P (REGNO)	\
   ? DImode								\
   : choose_hard_reg_mode ((REGNO), (NREGS), false))

#define HARD_REGNO_CALL_PART_CLOBBERED(REGNO, MODE)			\
  (((TARGET_32BIT && TARGET_POWERPC64					\
     && (GET_MODE_SIZE (MODE) > 4)					\
     && INT_REGNO_P (REGNO)) ? 1 : 0)					\
   || (TARGET_VSX && FP_REGNO_P (REGNO)					\
       && GET_MODE_SIZE (MODE) > 8 && !FLOAT128_2REG_P (MODE)))

#define VSX_VECTOR_MODE(MODE)		\
	 ((MODE) == V4SFmode		\
	  || (MODE) == V2DFmode)	\

/* Note KFmode and possibly TFmode (i.e. IEEE 128-bit floating point) are not
   really a vector, but we want to treat it as a vector for moves, and
   such.  */

#define ALTIVEC_VECTOR_MODE(MODE)					\
  ((MODE) == V16QImode							\
   || (MODE) == V8HImode						\
   || (MODE) == V4SFmode						\
   || (MODE) == V4SImode						\
   || FLOAT128_VECTOR_P (MODE))

#define ALTIVEC_OR_VSX_VECTOR_MODE(MODE)				\
  (ALTIVEC_VECTOR_MODE (MODE) || VSX_VECTOR_MODE (MODE)			\
   || (MODE) == V2DImode || (MODE) == V1TImode)

#define SPE_VECTOR_MODE(MODE)		\
	((MODE) == V4HImode          	\
         || (MODE) == V2SFmode          \
         || (MODE) == V1DImode          \
         || (MODE) == V2SImode)

#define PAIRED_VECTOR_MODE(MODE)        \
         ((MODE) == V2SFmode)            

/* Value is TRUE if hard register REGNO can hold a value of
   machine-mode MODE.  */
#define HARD_REGNO_MODE_OK(REGNO, MODE) \
  rs6000_hard_regno_mode_ok_p[(int)(MODE)][REGNO]

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.

   PTImode cannot tie with other modes because PTImode is restricted to even
   GPR registers, and TImode can go in any GPR as well as VSX registers (PR
   57744).

   Altivec/VSX vector tests were moved ahead of scalar float mode, so that IEEE
   128-bit floating point on VSX systems ties with other vectors.  */
#define MODES_TIEABLE_P(MODE1, MODE2)		\
  ((MODE1) == PTImode				\
   ? (MODE2) == PTImode				\
   : (MODE2) == PTImode				\
   ? 0						\
   : ALTIVEC_OR_VSX_VECTOR_MODE (MODE1)		\
   ? ALTIVEC_OR_VSX_VECTOR_MODE (MODE2)		\
   : ALTIVEC_OR_VSX_VECTOR_MODE (MODE2)		\
   ? 0						\
   : SCALAR_FLOAT_MODE_P (MODE1)		\
   ? SCALAR_FLOAT_MODE_P (MODE2)		\
   : SCALAR_FLOAT_MODE_P (MODE2)		\
   ? 0						\
   : GET_MODE_CLASS (MODE1) == MODE_CC		\
   ? GET_MODE_CLASS (MODE2) == MODE_CC		\
   : GET_MODE_CLASS (MODE2) == MODE_CC		\
   ? 0						\
   : SPE_VECTOR_MODE (MODE1)			\
   ? SPE_VECTOR_MODE (MODE2)			\
   : SPE_VECTOR_MODE (MODE2)			\
   ? 0						\
   : 1)

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

/* A fixed register used at epilogue generation to address SPE registers
   with negative offsets.  The 64-bit load/store instructions on the SPE
   only take positive offsets (and small ones at that), so we need to
   reserve a register for consing up negative offsets.  */

#define FIXED_SCRATCH 0

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* RS/6000 pc isn't overloaded on a register that the compiler knows about.  */
/* #define PC_REGNUM  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 1

/* Base register for access to local variables of the function.  */
#define HARD_FRAME_POINTER_REGNUM 31

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 113

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 67

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
  SPE_ACC_REGS,
  SPEFSCR_REGS,
  SPR_REGS,
  NON_SPECIAL_REGS,
  LINK_REGS,
  CTR_REGS,
  LINK_OR_CTR_REGS,
  SPECIAL_REGS,
  SPEC_OR_GEN_REGS,
  CR0_REGS,
  CR_REGS,
  NON_FLOAT_REGS,
  CA_REGS,
  SPE_HIGH_REGS,
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
  "SPE_ACC_REGS",                                                       \
  "SPEFSCR_REGS",                                                       \
  "SPR_REGS",								\
  "NON_SPECIAL_REGS",							\
  "LINK_REGS",								\
  "CTR_REGS",								\
  "LINK_OR_CTR_REGS",							\
  "SPECIAL_REGS",							\
  "SPEC_OR_GEN_REGS",							\
  "CR0_REGS",								\
  "CR_REGS",								\
  "NON_FLOAT_REGS",							\
  "CA_REGS",								\
  "SPE_HIGH_REGS",							\
  "ALL_REGS"								\
}

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS						\
{									\
  /* NO_REGS.  */							\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000 },	\
  /* BASE_REGS.  */							\
  { 0xfffffffe, 0x00000000, 0x00000008, 0x00020000, 0x00000000 },	\
  /* GENERAL_REGS.  */							\
  { 0xffffffff, 0x00000000, 0x00000008, 0x00020000, 0x00000000 },	\
  /* FLOAT_REGS.  */							\
  { 0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000 },	\
  /* ALTIVEC_REGS.  */							\
  { 0x00000000, 0x00000000, 0xffffe000, 0x00001fff, 0x00000000 },	\
  /* VSX_REGS.  */							\
  { 0x00000000, 0xffffffff, 0xffffe000, 0x00001fff, 0x00000000 },	\
  /* VRSAVE_REGS.  */							\
  { 0x00000000, 0x00000000, 0x00000000, 0x00002000, 0x00000000 },	\
  /* VSCR_REGS.  */							\
  { 0x00000000, 0x00000000, 0x00000000, 0x00004000, 0x00000000 },	\
  /* SPE_ACC_REGS.  */							\
  { 0x00000000, 0x00000000, 0x00000000, 0x00008000, 0x00000000 },	\
  /* SPEFSCR_REGS.  */							\
  { 0x00000000, 0x00000000, 0x00000000, 0x00010000, 0x00000000 },	\
  /* SPR_REGS.  */							\
  { 0x00000000, 0x00000000, 0x00000000, 0x00040000, 0x00000000 },	\
  /* NON_SPECIAL_REGS.  */						\
  { 0xffffffff, 0xffffffff, 0x00000008, 0x00020000, 0x00000000 },	\
  /* LINK_REGS.  */							\
  { 0x00000000, 0x00000000, 0x00000002, 0x00000000, 0x00000000 },	\
  /* CTR_REGS.  */							\
  { 0x00000000, 0x00000000, 0x00000004, 0x00000000, 0x00000000 },	\
  /* LINK_OR_CTR_REGS.  */						\
  { 0x00000000, 0x00000000, 0x00000006, 0x00000000, 0x00000000 },	\
  /* SPECIAL_REGS.  */							\
  { 0x00000000, 0x00000000, 0x00000006, 0x00002000, 0x00000000 },	\
  /* SPEC_OR_GEN_REGS.  */						\
  { 0xffffffff, 0x00000000, 0x0000000e, 0x00022000, 0x00000000 },	\
  /* CR0_REGS.  */							\
  { 0x00000000, 0x00000000, 0x00000010, 0x00000000, 0x00000000 },	\
  /* CR_REGS.  */							\
  { 0x00000000, 0x00000000, 0x00000ff0, 0x00000000, 0x00000000 },	\
  /* NON_FLOAT_REGS.  */						\
  { 0xffffffff, 0x00000000, 0x00000ffe, 0x00020000, 0x00000000 },	\
  /* CA_REGS.  */							\
  { 0x00000000, 0x00000000, 0x00001000, 0x00000000, 0x00000000 },	\
  /* SPE_HIGH_REGS.  */							\
  { 0x00000000, 0x00000000, 0x00000000, 0xffe00000, 0x001fffff },	\
  /* ALL_REGS.  */							\
  { 0xffffffff, 0xffffffff, 0xfffffffe, 0xffe7ffff, 0x001fffff }	\
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
  RS6000_CONSTRAINT_wb,		/* Altivec register if ISA 3.0 vector. */
  RS6000_CONSTRAINT_wd,		/* VSX register for V2DF */
  RS6000_CONSTRAINT_we,		/* VSX register if ISA 3.0 vector. */
  RS6000_CONSTRAINT_wf,		/* VSX register for V4SF */
  RS6000_CONSTRAINT_wg,		/* FPR register for -mmfpgpr */
  RS6000_CONSTRAINT_wh,		/* FPR register for direct moves.  */
  RS6000_CONSTRAINT_wi,		/* FPR/VSX register to hold DImode */
  RS6000_CONSTRAINT_wj,		/* FPR/VSX register for DImode direct moves. */
  RS6000_CONSTRAINT_wk,		/* FPR/VSX register for DFmode direct moves. */
  RS6000_CONSTRAINT_wl,		/* FPR register for LFIWAX */
  RS6000_CONSTRAINT_wm,		/* VSX register for direct move */
  RS6000_CONSTRAINT_wo,		/* VSX register for power9 vector.  */
  RS6000_CONSTRAINT_wp,		/* VSX reg for IEEE 128-bit fp TFmode. */
  RS6000_CONSTRAINT_wq,		/* VSX reg for IEEE 128-bit fp KFmode.  */
  RS6000_CONSTRAINT_wr,		/* GPR register if 64-bit  */
  RS6000_CONSTRAINT_ws,		/* VSX register for DF */
  RS6000_CONSTRAINT_wt,		/* VSX register for TImode */
  RS6000_CONSTRAINT_wu,		/* Altivec register for float load/stores.  */
  RS6000_CONSTRAINT_wv,		/* Altivec register for double load/stores.  */
  RS6000_CONSTRAINT_ww,		/* FP or VSX register for vsx float ops.  */
  RS6000_CONSTRAINT_wx,		/* FPR register for STFIWX */
  RS6000_CONSTRAINT_wy,		/* VSX register for SF */
  RS6000_CONSTRAINT_wz,		/* FPR register for LFIWZX */
  RS6000_CONSTRAINT_wA,		/* BASE_REGS if 64-bit.  */
  RS6000_CONSTRAINT_wH,		/* Altivec register for 32-bit integers.  */
  RS6000_CONSTRAINT_wI,		/* VSX register for 32-bit integers.  */
  RS6000_CONSTRAINT_wJ,		/* VSX register for 8/16-bit integers.  */
  RS6000_CONSTRAINT_wK,		/* Altivec register for 16/32-bit integers.  */
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

/* If we are copying between FP or AltiVec registers and anything
   else, we need a memory location.  The exception is when we are
   targeting ppc64 and the move to/from fpr to gpr instructions
   are available.*/

#define SECONDARY_MEMORY_NEEDED(CLASS1,CLASS2,MODE)			\
  rs6000_secondary_memory_needed_ptr (CLASS1, CLASS2, MODE)

/* For cpus that cannot load/store SDmode values from the 64-bit
   FP registers without using a full 64-bit load/store, we need
   to allocate a full 64-bit stack slot for them.  */

#define SECONDARY_MEMORY_NEEDED_RTX(MODE) \
  rs6000_secondary_memory_needed_rtx (MODE)

/* Specify the mode to be used for memory when a secondary memory
   location is needed.  For cpus that cannot load/store SDmode values
   from the 64-bit FP registers without using a full 64-bit
   load/store, we need a wider mode.  */
#define SECONDARY_MEMORY_NEEDED_MODE(MODE)		\
  rs6000_secondary_memory_needed_mode (MODE)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.

   On RS/6000, this is the size of MODE in words, except in the FP regs, where
   a single reg is enough for two words, unless we have VSX, where the FP
   registers can hold 128 bits.  */
#define CLASS_MAX_NREGS(CLASS, MODE) rs6000_class_max_nregs[(MODE)][(CLASS)]

/* Return nonzero if for CLASS a mode change from FROM to TO is invalid.  */

#define CANNOT_CHANGE_MODE_CLASS(FROM, TO, CLASS)			\
  rs6000_cannot_change_mode_class_ptr (FROM, TO, CLASS)

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

#define STARTING_FRAME_OFFSET						\
  (FRAME_GROWS_DOWNWARD							\
   ? 0									\
   : (cfun->calls_alloca						\
      ? (RS6000_ALIGN (crtl->outgoing_args_size + RS6000_SAVE_AREA,	\
		       (TARGET_ALTIVEC || TARGET_VSX) ? 16 : 8 ))	\
      : (RS6000_ALIGN (crtl->outgoing_args_size,			\
		       (TARGET_ALTIVEC || TARGET_VSX) ? 16 : 8)		\
	 + RS6000_SAVE_AREA)))

/* Offset from the stack pointer register to an item dynamically
   allocated on the stack, e.g., by `alloca'.

   The default value for this macro is `STACK_POINTER_OFFSET' plus the
   length of the outgoing arguments.  The default is correct for most
   machines.  See `function.c' for details.

   This value must be a multiple of STACK_BOUNDARY (hard coded in
   `emit-rtl.c').  */
#define STACK_DYNAMIC_OFFSET(FUNDECL)					\
  RS6000_ALIGN (crtl->outgoing_args_size + STACK_POINTER_OFFSET,	\
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

/* We don't have prologue and epilogue functions to save/restore
   everything for most ABIs.  */
#define WORLD_SAVE_P(INFO) 0

/* 1 if N is a possible register number for a function value
   as seen by the caller.

   On RS/6000, this is r3, fp1, and v2 (for AltiVec).  */
#define FUNCTION_VALUE_REGNO_P(N)					\
  ((N) == GP_ARG_RETURN							\
   || (IN_RANGE ((N), FP_ARG_RETURN, FP_ARG_MAX_RETURN)			\
       && TARGET_HARD_FLOAT && TARGET_FPRS)				\
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
       && TARGET_HARD_FLOAT && TARGET_FPRS))

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

/* If defined, a C expression which determines whether, and in which
   direction, to pad out an argument with extra space.  The value
   should be of type `enum direction': either `upward' to pad above
   the argument, `downward' to pad below, or `none' to inhibit
   padding.  */

#define FUNCTION_ARG_PADDING(MODE, TYPE) function_arg_padding (MODE, TYPE)

#define PAD_VARARGS_DOWN \
   (FUNCTION_ARG_PADDING (TYPE_MODE (type), type) == downward)

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

/* The current return address is in link register (65).  The return address
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
((REGNO) < FIRST_PSEUDO_REGISTER				\
 ? (REGNO) <= 31 || (REGNO) == 67				\
   || (REGNO) == FRAME_POINTER_REGNUM				\
 : (reg_renumber[REGNO] >= 0					\
    && (reg_renumber[REGNO] <= 31 || reg_renumber[REGNO] == 67	\
	|| reg_renumber[REGNO] == FRAME_POINTER_REGNUM)))

#define REGNO_OK_FOR_BASE_P(REGNO)				\
((REGNO) < FIRST_PSEUDO_REGISTER				\
 ? ((REGNO) > 0 && (REGNO) <= 31) || (REGNO) == 67		\
   || (REGNO) == FRAME_POINTER_REGNUM				\
 : (reg_renumber[REGNO] > 0					\
    && (reg_renumber[REGNO] <= 31 || reg_renumber[REGNO] == 67	\
	|| reg_renumber[REGNO] == FRAME_POINTER_REGNUM)))

/* Nonzero if X is a hard reg that can be used as an index
   or if it is a pseudo reg in the non-strict case.  */
#define INT_REG_OK_FOR_INDEX_P(X, STRICT)			\
  ((!(STRICT) && REGNO (X) >= FIRST_PSEUDO_REGISTER)		\
   || REGNO_OK_FOR_INDEX_P (REGNO (X)))

/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg in the non-strict case.  */
#define INT_REG_OK_FOR_BASE_P(X, STRICT)			\
  ((!(STRICT) && REGNO (X) >= FIRST_PSEUDO_REGISTER)		\
   || REGNO_OK_FOR_BASE_P (REGNO (X)))


/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 2

/* Recognize any constant value that is a valid address.  */

#define CONSTANT_ADDRESS_P(X)   \
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
   || GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST		\
   || GET_CODE (X) == HIGH)

#define EASY_VECTOR_15(n) ((n) >= -16 && (n) <= 15)
#define EASY_VECTOR_15_ADD_SELF(n) (!EASY_VECTOR_15((n))	\
				    && EASY_VECTOR_15((n) >> 1) \
				    && ((n) & 1) == 0)

#define EASY_VECTOR_MSB(n,mode)						\
  ((((unsigned HOST_WIDE_INT) (n)) & GET_MODE_MASK (mode)) ==		\
   ((((unsigned HOST_WIDE_INT)GET_MODE_MASK (mode)) + 1) >> 1))


/* Try a machine-dependent way of reloading an illegitimate address
   operand.  If we find one, push the reload and jump to WIN.  This
   macro is used in only one place: `find_reloads_address' in reload.c.

   Implemented on rs6000 by rs6000_legitimize_reload_address.
   Note that (X) is evaluated twice; this is safe in current usage.  */

#define LEGITIMIZE_RELOAD_ADDRESS(X,MODE,OPNUM,TYPE,IND_LEVELS,WIN)	     \
do {									     \
  int win;								     \
  (X) = rs6000_legitimize_reload_address_ptr ((X), (MODE), (OPNUM),	     \
			(int)(TYPE), (IND_LEVELS), &win);		     \
  if ( win )								     \
    goto WIN;								     \
} while (0)

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

/* Define this if some processing needs to be done immediately before
   emitting code for an insn.  */

#define FINAL_PRESCAN_INSN(INSN,OPERANDS,NOPERANDS) \
  rs6000_final_prescan_insn (INSN, OPERANDS, NOPERANDS)

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE SImode

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses.  */
#define CASE_VECTOR_PC_RELATIVE 1

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

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

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
extern unsigned rs6000_pmode;
#define Pmode ((machine_mode)rs6000_pmode)

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
   to express in the length attribute.  */

/* #define ADJUST_INSN_LENGTH(X,LENGTH) */

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
  &rs6000_reg_names[64][0],     /* was mq  */				\
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
  &rs6000_reg_names[76][0],	/* ca  */				\
									\
  &rs6000_reg_names[77][0],	/* v0  */				\
  &rs6000_reg_names[78][0],	/* v1  */				\
  &rs6000_reg_names[79][0],	/* v2  */				\
  &rs6000_reg_names[80][0],	/* v3  */				\
  &rs6000_reg_names[81][0],	/* v4  */				\
  &rs6000_reg_names[82][0],	/* v5  */				\
  &rs6000_reg_names[83][0],	/* v6  */				\
  &rs6000_reg_names[84][0],	/* v7  */				\
  &rs6000_reg_names[85][0],	/* v8  */				\
  &rs6000_reg_names[86][0],	/* v9  */				\
  &rs6000_reg_names[87][0],	/* v10  */				\
  &rs6000_reg_names[88][0],	/* v11  */				\
  &rs6000_reg_names[89][0],	/* v12  */				\
  &rs6000_reg_names[90][0],	/* v13  */				\
  &rs6000_reg_names[91][0],	/* v14  */				\
  &rs6000_reg_names[92][0],	/* v15  */				\
  &rs6000_reg_names[93][0],	/* v16  */				\
  &rs6000_reg_names[94][0],	/* v17  */				\
  &rs6000_reg_names[95][0],	/* v18  */				\
  &rs6000_reg_names[96][0],	/* v19  */				\
  &rs6000_reg_names[97][0],	/* v20  */				\
  &rs6000_reg_names[98][0],	/* v21  */				\
  &rs6000_reg_names[99][0],	/* v22  */				\
  &rs6000_reg_names[100][0],	/* v23  */				\
  &rs6000_reg_names[101][0],	/* v24  */				\
  &rs6000_reg_names[102][0],	/* v25  */				\
  &rs6000_reg_names[103][0],	/* v26  */				\
  &rs6000_reg_names[104][0],	/* v27  */				\
  &rs6000_reg_names[105][0],	/* v28  */				\
  &rs6000_reg_names[106][0],	/* v29  */				\
  &rs6000_reg_names[107][0],	/* v30  */				\
  &rs6000_reg_names[108][0],	/* v31  */				\
  &rs6000_reg_names[109][0],	/* vrsave  */				\
  &rs6000_reg_names[110][0],	/* vscr  */				\
  &rs6000_reg_names[111][0],	/* spe_acc */				\
  &rs6000_reg_names[112][0],	/* spefscr */				\
  &rs6000_reg_names[113][0],	/* sfp  */				\
  &rs6000_reg_names[114][0],	/* tfhar  */				\
  &rs6000_reg_names[115][0],	/* tfiar  */				\
  &rs6000_reg_names[116][0],	/* texasr  */				\
									\
  &rs6000_reg_names[117][0],	/* SPE rh0.  */				\
  &rs6000_reg_names[118][0],	/* SPE rh1.  */				\
  &rs6000_reg_names[119][0],	/* SPE rh2.  */				\
  &rs6000_reg_names[120][0],	/* SPE rh3.  */				\
  &rs6000_reg_names[121][0],	/* SPE rh4.  */				\
  &rs6000_reg_names[122][0],	/* SPE rh5.  */				\
  &rs6000_reg_names[123][0],	/* SPE rh6.  */				\
  &rs6000_reg_names[124][0],	/* SPE rh7.  */				\
  &rs6000_reg_names[125][0],	/* SPE rh8.  */				\
  &rs6000_reg_names[126][0],	/* SPE rh9.  */				\
  &rs6000_reg_names[127][0],	/* SPE rh10.  */			\
  &rs6000_reg_names[128][0],	/* SPE rh11.  */			\
  &rs6000_reg_names[129][0],	/* SPE rh12.  */			\
  &rs6000_reg_names[130][0],	/* SPE rh13.  */			\
  &rs6000_reg_names[131][0],	/* SPE rh14.  */			\
  &rs6000_reg_names[132][0],	/* SPE rh15.  */			\
  &rs6000_reg_names[133][0],	/* SPE rh16.  */			\
  &rs6000_reg_names[134][0],	/* SPE rh17.  */			\
  &rs6000_reg_names[135][0],	/* SPE rh18.  */			\
  &rs6000_reg_names[136][0],	/* SPE rh19.  */			\
  &rs6000_reg_names[137][0],	/* SPE rh20.  */			\
  &rs6000_reg_names[138][0],	/* SPE rh21.  */			\
  &rs6000_reg_names[139][0],	/* SPE rh22.  */			\
  &rs6000_reg_names[140][0],	/* SPE rh22.  */			\
  &rs6000_reg_names[141][0],	/* SPE rh24.  */			\
  &rs6000_reg_names[142][0],	/* SPE rh25.  */			\
  &rs6000_reg_names[143][0],	/* SPE rh26.  */			\
  &rs6000_reg_names[144][0],	/* SPE rh27.  */			\
  &rs6000_reg_names[145][0],	/* SPE rh28.  */			\
  &rs6000_reg_names[146][0],	/* SPE rh29.  */			\
  &rs6000_reg_names[147][0],	/* SPE rh30.  */			\
  &rs6000_reg_names[148][0],	/* SPE rh31.  */			\
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
  {"v0",   77}, {"v1",   78}, {"v2",   79}, {"v3",   80},       \
  {"v4",   81}, {"v5",   82}, {"v6",   83}, {"v7",   84},       \
  {"v8",   85}, {"v9",   86}, {"v10",  87}, {"v11",  88},       \
  {"v12",  89}, {"v13",  90}, {"v14",  91}, {"v15",  92},       \
  {"v16",  93}, {"v17",  94}, {"v18",  95}, {"v19",  96},       \
  {"v20",  97}, {"v21",  98}, {"v22",  99}, {"v23",  100},	\
  {"v24",  101},{"v25",  102},{"v26",  103},{"v27",  104},      \
  {"v28",  105},{"v29",  106},{"v30",  107},{"v31",  108},      \
  {"vrsave", 109}, {"vscr", 110},				\
  {"spe_acc", 111}, {"spefscr", 112},				\
  /* no additional names for: lr, ctr, ap */			\
  {"cr0",  68}, {"cr1",  69}, {"cr2",  70}, {"cr3",  71},	\
  {"cr4",  72}, {"cr5",  73}, {"cr6",  74}, {"cr7",  75},	\
  {"cc",   68}, {"sp",    1}, {"toc",   2},			\
  /* CA is only part of XER, but we do not model the other parts (yet).  */ \
  {"xer",  76},							\
  /* VSX registers overlaid on top of FR, Altivec registers */	\
  {"vs0",  32}, {"vs1",  33}, {"vs2",  34}, {"vs3",  35},	\
  {"vs4",  36}, {"vs5",  37}, {"vs6",  38}, {"vs7",  39},	\
  {"vs8",  40}, {"vs9",  41}, {"vs10", 42}, {"vs11", 43},	\
  {"vs12", 44}, {"vs13", 45}, {"vs14", 46}, {"vs15", 47},	\
  {"vs16", 48}, {"vs17", 49}, {"vs18", 50}, {"vs19", 51},	\
  {"vs20", 52}, {"vs21", 53}, {"vs22", 54}, {"vs23", 55},	\
  {"vs24", 56}, {"vs25", 57}, {"vs26", 58}, {"vs27", 59},	\
  {"vs28", 60}, {"vs29", 61}, {"vs30", 62}, {"vs31", 63},	\
  {"vs32", 77}, {"vs33", 78}, {"vs34", 79}, {"vs35", 80},       \
  {"vs36", 81}, {"vs37", 82}, {"vs38", 83}, {"vs39", 84},       \
  {"vs40", 85}, {"vs41", 86}, {"vs42", 87}, {"vs43", 88},       \
  {"vs44", 89}, {"vs45", 90}, {"vs46", 91}, {"vs47", 92},       \
  {"vs48", 93}, {"vs49", 94}, {"vs50", 95}, {"vs51", 96},       \
  {"vs52", 97}, {"vs53", 98}, {"vs54", 99}, {"vs55", 100},	\
  {"vs56", 101},{"vs57", 102},{"vs58", 103},{"vs59", 104},      \
  {"vs60", 105},{"vs61", 106},{"vs62", 107},{"vs63", 108},	\
  /* Transactional Memory Facility (HTM) Registers.  */		\
  {"tfhar",  114}, {"tfiar",  115}, {"texasr",  116},		\
  /* SPE high registers.  */					\
  {"rh0",  117}, {"rh1",  118}, {"rh2",  119}, {"rh3",  120},	\
  {"rh4",  121}, {"rh5",  122}, {"rh6",  123}, {"rh7",  124},	\
  {"rh8",  125}, {"rh9",  126}, {"rh10", 127}, {"rh11", 128},	\
  {"rh12", 129}, {"rh13", 130}, {"rh14", 131}, {"rh15", 132},	\
  {"rh16", 133}, {"rh17", 134}, {"rh18", 135}, {"rh19", 136},	\
  {"rh20", 137}, {"rh21", 138}, {"rh22", 139}, {"rh23", 140},	\
  {"rh24", 141}, {"rh25", 142}, {"rh26", 143}, {"rh27", 144},	\
  {"rh28", 145}, {"rh29", 146}, {"rh30", 147}, {"rh31", 148},	\
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

/* Builtin attributes.  */
#define RS6000_BTC_SPECIAL	0x00000000	/* Special function.  */
#define RS6000_BTC_UNARY	0x00000001	/* normal unary function.  */
#define RS6000_BTC_BINARY	0x00000002	/* normal binary function.  */
#define RS6000_BTC_TERNARY	0x00000003	/* normal ternary function.  */
#define RS6000_BTC_PREDICATE	0x00000004	/* predicate function.  */
#define RS6000_BTC_ABS		0x00000005	/* Altivec/VSX ABS function.  */
#define RS6000_BTC_EVSEL	0x00000006	/* SPE EVSEL function.  */
#define RS6000_BTC_DST		0x00000007	/* Altivec DST function.  */
#define RS6000_BTC_TYPE_MASK	0x0000000f	/* Mask to isolate types */

#define RS6000_BTC_MISC		0x00000000	/* No special attributes.  */
#define RS6000_BTC_CONST	0x00000100	/* Neither uses, nor
						   modifies global state.  */
#define RS6000_BTC_PURE		0x00000200	/* reads global
						   state/mem and does
						   not modify global state.  */
#define RS6000_BTC_FP		0x00000400	/* depends on rounding mode.  */
#define RS6000_BTC_ATTR_MASK	0x00000700	/* Mask of the attributes.  */

/* Miscellaneous information.  */
#define RS6000_BTC_SPR		0x01000000	/* function references SPRs.  */
#define RS6000_BTC_VOID		0x02000000	/* function has no return value.  */
#define RS6000_BTC_CR		0x04000000	/* function references a CR.  */
#define RS6000_BTC_OVERLOADED	0x08000000	/* function is overloaded.  */
#define RS6000_BTC_MISC_MASK	0x1f000000	/* Mask of the misc info.  */

/* Convenience macros to document the instruction type.  */
#define RS6000_BTC_MEM		RS6000_BTC_MISC	/* load/store touches mem.  */
#define RS6000_BTC_SAT		RS6000_BTC_MISC	/* saturate sets VSCR.  */

/* Builtin targets.  For now, we reuse the masks for those options that are in
   target flags, and pick three random bits for SPE, paired and ldbl128 which
   aren't in target_flags.  */
#define RS6000_BTM_ALWAYS	0		/* Always enabled.  */
#define RS6000_BTM_ALTIVEC	MASK_ALTIVEC	/* VMX/altivec vectors.  */
#define RS6000_BTM_VSX		MASK_VSX	/* VSX (vector/scalar).  */
#define RS6000_BTM_P8_VECTOR	MASK_P8_VECTOR	/* ISA 2.07 vector.  */
#define RS6000_BTM_P9_VECTOR	MASK_P9_VECTOR	/* ISA 3.0 vector.  */
#define RS6000_BTM_P9_MISC	MASK_P9_MISC	/* ISA 3.0 misc. non-vector */
#define RS6000_BTM_CRYPTO	MASK_CRYPTO	/* crypto funcs.  */
#define RS6000_BTM_HTM		MASK_HTM	/* hardware TM funcs.  */
#define RS6000_BTM_SPE		MASK_STRING	/* E500 */
#define RS6000_BTM_PAIRED	MASK_MULHW	/* 750CL paired insns.  */
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
#define RS6000_BTM_FLOAT128	MASK_FLOAT128_TYPE /* IEEE 128-bit float.  */

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
				 | RS6000_BTM_FLOAT128)

/* Define builtin enum index.  */

#undef RS6000_BUILTIN_0
#undef RS6000_BUILTIN_1
#undef RS6000_BUILTIN_2
#undef RS6000_BUILTIN_3
#undef RS6000_BUILTIN_A
#undef RS6000_BUILTIN_D
#undef RS6000_BUILTIN_E
#undef RS6000_BUILTIN_H
#undef RS6000_BUILTIN_P
#undef RS6000_BUILTIN_Q
#undef RS6000_BUILTIN_S
#undef RS6000_BUILTIN_X

#define RS6000_BUILTIN_0(ENUM, NAME, MASK, ATTR, ICODE) ENUM,
#define RS6000_BUILTIN_1(ENUM, NAME, MASK, ATTR, ICODE) ENUM,
#define RS6000_BUILTIN_2(ENUM, NAME, MASK, ATTR, ICODE) ENUM,
#define RS6000_BUILTIN_3(ENUM, NAME, MASK, ATTR, ICODE) ENUM,
#define RS6000_BUILTIN_A(ENUM, NAME, MASK, ATTR, ICODE) ENUM,
#define RS6000_BUILTIN_D(ENUM, NAME, MASK, ATTR, ICODE) ENUM,
#define RS6000_BUILTIN_E(ENUM, NAME, MASK, ATTR, ICODE) ENUM,
#define RS6000_BUILTIN_H(ENUM, NAME, MASK, ATTR, ICODE) ENUM,
#define RS6000_BUILTIN_P(ENUM, NAME, MASK, ATTR, ICODE) ENUM,
#define RS6000_BUILTIN_Q(ENUM, NAME, MASK, ATTR, ICODE) ENUM,
#define RS6000_BUILTIN_S(ENUM, NAME, MASK, ATTR, ICODE) ENUM,
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
#undef RS6000_BUILTIN_A
#undef RS6000_BUILTIN_D
#undef RS6000_BUILTIN_E
#undef RS6000_BUILTIN_H
#undef RS6000_BUILTIN_P
#undef RS6000_BUILTIN_Q
#undef RS6000_BUILTIN_S
#undef RS6000_BUILTIN_X

enum rs6000_builtin_type_index
{
  RS6000_BTI_NOT_OPAQUE,
  RS6000_BTI_opaque_V2SI,
  RS6000_BTI_opaque_V2SF,
  RS6000_BTI_opaque_p_V2SI,
  RS6000_BTI_opaque_V4SI,
  RS6000_BTI_V16QI,
  RS6000_BTI_V1TI,
  RS6000_BTI_V2SI,
  RS6000_BTI_V2SF,
  RS6000_BTI_V2DI,
  RS6000_BTI_V2DF,
  RS6000_BTI_V4HI,
  RS6000_BTI_V4SI,
  RS6000_BTI_V4SF,
  RS6000_BTI_V8HI,
  RS6000_BTI_unsigned_V16QI,
  RS6000_BTI_unsigned_V1TI,
  RS6000_BTI_unsigned_V8HI,
  RS6000_BTI_unsigned_V4SI,
  RS6000_BTI_unsigned_V2DI,
  RS6000_BTI_bool_char,          /* __bool char */
  RS6000_BTI_bool_short,         /* __bool short */
  RS6000_BTI_bool_int,           /* __bool int */
  RS6000_BTI_bool_long,		 /* __bool long */
  RS6000_BTI_pixel,              /* __pixel */
  RS6000_BTI_bool_V16QI,         /* __vector __bool char */
  RS6000_BTI_bool_V8HI,          /* __vector __bool short */
  RS6000_BTI_bool_V4SI,          /* __vector __bool int */
  RS6000_BTI_bool_V2DI,          /* __vector __bool long */
  RS6000_BTI_pixel_V8HI,         /* __vector __pixel */
  RS6000_BTI_long,	         /* long_integer_type_node */
  RS6000_BTI_unsigned_long,      /* long_unsigned_type_node */
  RS6000_BTI_long_long,	         /* long_long_integer_type_node */
  RS6000_BTI_unsigned_long_long, /* long_long_unsigned_type_node */
  RS6000_BTI_INTQI,	         /* intQI_type_node */
  RS6000_BTI_UINTQI,		 /* unsigned_intQI_type_node */
  RS6000_BTI_INTHI,	         /* intHI_type_node */
  RS6000_BTI_UINTHI,		 /* unsigned_intHI_type_node */
  RS6000_BTI_INTSI,		 /* intSI_type_node */
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
  RS6000_BTI_MAX
};


#define opaque_V2SI_type_node         (rs6000_builtin_types[RS6000_BTI_opaque_V2SI])
#define opaque_V2SF_type_node         (rs6000_builtin_types[RS6000_BTI_opaque_V2SF])
#define opaque_p_V2SI_type_node       (rs6000_builtin_types[RS6000_BTI_opaque_p_V2SI])
#define opaque_V4SI_type_node         (rs6000_builtin_types[RS6000_BTI_opaque_V4SI])
#define V16QI_type_node               (rs6000_builtin_types[RS6000_BTI_V16QI])
#define V1TI_type_node                (rs6000_builtin_types[RS6000_BTI_V1TI])
#define V2DI_type_node                (rs6000_builtin_types[RS6000_BTI_V2DI])
#define V2DF_type_node                (rs6000_builtin_types[RS6000_BTI_V2DF])
#define V2SI_type_node                (rs6000_builtin_types[RS6000_BTI_V2SI])
#define V2SF_type_node                (rs6000_builtin_types[RS6000_BTI_V2SF])
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
#define bool_long_type_node           (rs6000_builtin_types[RS6000_BTI_bool_long])
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

extern GTY(()) tree rs6000_builtin_types[RS6000_BTI_MAX];
extern GTY(()) tree rs6000_builtin_decls[RS6000_BUILTIN_COUNT];

#define TARGET_SUPPORTS_WIDE_INT 1

#if (GCC_VERSION >= 3000)
#pragma GCC poison TARGET_FLOAT128 OPTION_MASK_FLOAT128 MASK_FLOAT128
#endif
