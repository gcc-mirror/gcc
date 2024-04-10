/* Machine description for AArch64 architecture.
   Copyright (C) 2009-2024 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */


#ifndef GCC_AARCH64_H
#define GCC_AARCH64_H

/* Make these flags read-only so that all uses go via
   aarch64_set_asm_isa_flags.  */
#ifndef GENERATOR_FILE
#undef aarch64_asm_isa_flags
#define aarch64_asm_isa_flags \
  ((aarch64_feature_flags) global_options.x_aarch64_asm_isa_flags)
#undef aarch64_isa_flags
#define aarch64_isa_flags \
  ((aarch64_feature_flags) global_options.x_aarch64_isa_flags)
#endif

/* Target CPU builtins.  */
#define TARGET_CPU_CPP_BUILTINS()	\
  aarch64_cpu_cpp_builtins (pfile)



#define REGISTER_TARGET_PRAGMAS() aarch64_register_pragmas ()

/* Target machine storage layout.  */

#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)	\
  if (GET_MODE_CLASS (MODE) == MODE_INT		\
      && GET_MODE_SIZE (MODE) < 4)		\
    {						\
      if (MODE == QImode || MODE == HImode)	\
	{					\
	  MODE = SImode;			\
	}					\
    }

/* Bits are always numbered from the LSBit.  */
#define BITS_BIG_ENDIAN 0

/* Big/little-endian flavour.  */
#define BYTES_BIG_ENDIAN (TARGET_BIG_END != 0)
#define WORDS_BIG_ENDIAN (BYTES_BIG_ENDIAN)

/* AdvSIMD is supported in the default configuration, unless disabled by
   -mgeneral-regs-only or by the +nosimd extension.  The set of available
   instructions is then subdivided into:

   - the "base" set, available both in SME streaming mode and in
     non-streaming mode

   - the full set, available only in non-streaming mode.  */
#define TARGET_BASE_SIMD (AARCH64_ISA_SIMD)
#define TARGET_SIMD (AARCH64_ISA_SIMD && AARCH64_ISA_SM_OFF)
#define TARGET_FLOAT (AARCH64_ISA_FP)

#define UNITS_PER_WORD		8

#define UNITS_PER_VREG		16

#define PARM_BOUNDARY		64

#define STACK_BOUNDARY		128

#define FUNCTION_BOUNDARY	32

#define EMPTY_FIELD_BOUNDARY	32

#define BIGGEST_ALIGNMENT	128

#define SHORT_TYPE_SIZE		16

#define INT_TYPE_SIZE		32

#define LONG_TYPE_SIZE		(TARGET_ILP32 ? 32 : 64)

#define POINTER_SIZE		(TARGET_ILP32 ? 32 : 64)

#define LONG_LONG_TYPE_SIZE	64

#define FLOAT_TYPE_SIZE		32

#define DOUBLE_TYPE_SIZE	64

#define LONG_DOUBLE_TYPE_SIZE	128

/* This value is the amount of bytes a caller is allowed to drop the stack
   before probing has to be done for stack clash protection.  */
#define STACK_CLASH_CALLER_GUARD 1024

/* This value represents the minimum amount of bytes we expect the function's
   outgoing arguments to be when stack-clash is enabled.  */
#define STACK_CLASH_MIN_BYTES_OUTGOING_ARGS 8

/* This value controls how many pages we manually unroll the loop for when
   generating stack clash probes.  */
#define STACK_CLASH_MAX_UNROLL_PAGES 4

/* The architecture reserves all bits of the address for hardware use,
   so the vbit must go into the delta field of pointers to member
   functions.  This is the same config as that in the AArch32
   port.  */
#define TARGET_PTRMEMFUNC_VBIT_LOCATION ptrmemfunc_vbit_in_delta


/* Emit calls to libgcc helpers for atomic operations for runtime detection
   of LSE instructions.  */
#define TARGET_OUTLINE_ATOMICS (aarch64_flag_outline_atomics)

/* Align definitions of arrays, unions and structures so that
   initializations and copies can be made more efficient.  This is not
   ABI-changing, so it only affects places where we can see the
   definition.  Increasing the alignment tends to introduce padding,
   so don't do this when optimizing for size/conserving stack space.  */
#define AARCH64_EXPAND_ALIGNMENT(COND, EXP, ALIGN)			\
  (((COND) && ((ALIGN) < BITS_PER_WORD)					\
    && (TREE_CODE (EXP) == ARRAY_TYPE					\
	|| TREE_CODE (EXP) == UNION_TYPE				\
	|| TREE_CODE (EXP) == RECORD_TYPE)) ? BITS_PER_WORD : (ALIGN))

/* Align global data.  */
#define DATA_ALIGNMENT(EXP, ALIGN)			\
  AARCH64_EXPAND_ALIGNMENT (!optimize_size, EXP, ALIGN)

/* Similarly, make sure that objects on the stack are sensibly aligned.  */
#define LOCAL_ALIGNMENT(EXP, ALIGN)				\
  AARCH64_EXPAND_ALIGNMENT (!flag_conserve_stack, EXP, ALIGN)

#define STRUCTURE_SIZE_BOUNDARY		8

/* Heap alignment (same as BIGGEST_ALIGNMENT and STACK_BOUNDARY).  */
#define MALLOC_ABI_ALIGNMENT  128

/* Defined by the ABI */
#define WCHAR_TYPE "unsigned int"
#define WCHAR_TYPE_SIZE			32

/* Using long long breaks -ansi and -std=c90, so these will need to be
   made conditional for an LLP64 ABI.  */

#define SIZE_TYPE	"long unsigned int"

#define PTRDIFF_TYPE	"long int"

#define PCC_BITFIELD_TYPE_MATTERS	1

#ifndef USED_FOR_TARGET

/* Define an enum of all features (ISA modes, architectures and extensions).
   The ISA modes must come first.  */
enum class aarch64_feature : unsigned char {
#define DEF_AARCH64_ISA_MODE(IDENT) IDENT,
#define AARCH64_OPT_EXTENSION(A, IDENT, C, D, E, F) IDENT,
#define AARCH64_ARCH(A, B, IDENT, D, E) IDENT,
#include "aarch64-isa-modes.def"
#include "aarch64-option-extensions.def"
#include "aarch64-arches.def"
};

/* Define unique flags for each of the above.  */
#define HANDLE(IDENT) \
  constexpr auto AARCH64_FL_##IDENT \
    = aarch64_feature_flags (1) << int (aarch64_feature::IDENT);
#define DEF_AARCH64_ISA_MODE(IDENT) HANDLE (IDENT)
#define AARCH64_OPT_EXTENSION(A, IDENT, C, D, E, F) HANDLE (IDENT)
#define AARCH64_ARCH(A, B, IDENT, D, E) HANDLE (IDENT)
#include "aarch64-isa-modes.def"
#include "aarch64-option-extensions.def"
#include "aarch64-arches.def"
#undef HANDLE

constexpr auto AARCH64_FL_SM_STATE = AARCH64_FL_SM_ON | AARCH64_FL_SM_OFF;

constexpr unsigned int AARCH64_NUM_ISA_MODES = (0
#define DEF_AARCH64_ISA_MODE(IDENT) + 1
#include "aarch64-isa-modes.def"
);

/* The mask of all ISA modes.  */
constexpr auto AARCH64_FL_ISA_MODES
  = (aarch64_feature_flags (1) << AARCH64_NUM_ISA_MODES) - 1;

/* The default ISA mode, for functions with no attributes that specify
   something to the contrary.  */
constexpr auto AARCH64_FL_DEFAULT_ISA_MODE = AARCH64_FL_SM_OFF;

#endif

/* Macros to test ISA flags.

   There is intentionally no macro for AARCH64_FL_CRYPTO, since this flag bit
   is not always set when its constituent features are present.
   Check (TARGET_AES && TARGET_SHA2) instead.  */

#define AARCH64_ISA_SM_OFF         (aarch64_isa_flags & AARCH64_FL_SM_OFF)
#define AARCH64_ISA_SM_ON          (aarch64_isa_flags & AARCH64_FL_SM_ON)
#define AARCH64_ISA_ZA_ON          (aarch64_isa_flags & AARCH64_FL_ZA_ON)
#define AARCH64_ISA_MODE           (aarch64_isa_flags & AARCH64_FL_ISA_MODES)
#define AARCH64_ISA_V8A		   (aarch64_isa_flags & AARCH64_FL_V8A)
#define AARCH64_ISA_V8_1A	   (aarch64_isa_flags & AARCH64_FL_V8_1A)
#define AARCH64_ISA_CRC            (aarch64_isa_flags & AARCH64_FL_CRC)
#define AARCH64_ISA_FP             (aarch64_isa_flags & AARCH64_FL_FP)
#define AARCH64_ISA_SIMD           (aarch64_isa_flags & AARCH64_FL_SIMD)
#define AARCH64_ISA_LSE		   (aarch64_isa_flags & AARCH64_FL_LSE)
#define AARCH64_ISA_RDMA	   (aarch64_isa_flags & AARCH64_FL_RDMA)
#define AARCH64_ISA_V8_2A	   (aarch64_isa_flags & AARCH64_FL_V8_2A)
#define AARCH64_ISA_F16		   (aarch64_isa_flags & AARCH64_FL_F16)
#define AARCH64_ISA_SVE            (aarch64_isa_flags & AARCH64_FL_SVE)
#define AARCH64_ISA_SVE2	   (aarch64_isa_flags & AARCH64_FL_SVE2)
#define AARCH64_ISA_SVE2_AES	   (aarch64_isa_flags & AARCH64_FL_SVE2_AES)
#define AARCH64_ISA_SVE2_BITPERM  (aarch64_isa_flags & AARCH64_FL_SVE2_BITPERM)
#define AARCH64_ISA_SVE2_SHA3	   (aarch64_isa_flags & AARCH64_FL_SVE2_SHA3)
#define AARCH64_ISA_SVE2_SM4	   (aarch64_isa_flags & AARCH64_FL_SVE2_SM4)
#define AARCH64_ISA_SME		   (aarch64_isa_flags & AARCH64_FL_SME)
#define AARCH64_ISA_SME_I16I64	   (aarch64_isa_flags & AARCH64_FL_SME_I16I64)
#define AARCH64_ISA_SME_F64F64	   (aarch64_isa_flags & AARCH64_FL_SME_F64F64)
#define AARCH64_ISA_SME2	   (aarch64_isa_flags & AARCH64_FL_SME2)
#define AARCH64_ISA_V8_3A	   (aarch64_isa_flags & AARCH64_FL_V8_3A)
#define AARCH64_ISA_DOTPROD	   (aarch64_isa_flags & AARCH64_FL_DOTPROD)
#define AARCH64_ISA_AES	           (aarch64_isa_flags & AARCH64_FL_AES)
#define AARCH64_ISA_SHA2	   (aarch64_isa_flags & AARCH64_FL_SHA2)
#define AARCH64_ISA_V8_4A	   (aarch64_isa_flags & AARCH64_FL_V8_4A)
#define AARCH64_ISA_SM4	           (aarch64_isa_flags & AARCH64_FL_SM4)
#define AARCH64_ISA_SHA3	   (aarch64_isa_flags & AARCH64_FL_SHA3)
#define AARCH64_ISA_F16FML	   (aarch64_isa_flags & AARCH64_FL_F16FML)
#define AARCH64_ISA_RCPC	   (aarch64_isa_flags & AARCH64_FL_RCPC)
#define AARCH64_ISA_RCPC8_4	   (aarch64_isa_flags & AARCH64_FL_V8_4A)
#define AARCH64_ISA_RNG		   (aarch64_isa_flags & AARCH64_FL_RNG)
#define AARCH64_ISA_V8_5A	   (aarch64_isa_flags & AARCH64_FL_V8_5A)
#define AARCH64_ISA_TME		   (aarch64_isa_flags & AARCH64_FL_TME)
#define AARCH64_ISA_MEMTAG	   (aarch64_isa_flags & AARCH64_FL_MEMTAG)
#define AARCH64_ISA_V8_6A	   (aarch64_isa_flags & AARCH64_FL_V8_6A)
#define AARCH64_ISA_I8MM	   (aarch64_isa_flags & AARCH64_FL_I8MM)
#define AARCH64_ISA_F32MM	   (aarch64_isa_flags & AARCH64_FL_F32MM)
#define AARCH64_ISA_F64MM	   (aarch64_isa_flags & AARCH64_FL_F64MM)
#define AARCH64_ISA_BF16	   (aarch64_isa_flags & AARCH64_FL_BF16)
#define AARCH64_ISA_SB		   (aarch64_isa_flags & AARCH64_FL_SB)
#define AARCH64_ISA_RCPC3	   (aarch64_isa_flags & AARCH64_FL_RCPC3)
#define AARCH64_ISA_V8R		   (aarch64_isa_flags & AARCH64_FL_V8R)
#define AARCH64_ISA_PAUTH	   (aarch64_isa_flags & AARCH64_FL_PAUTH)
#define AARCH64_ISA_V8_7A	   (aarch64_isa_flags & AARCH64_FL_V8_7A)
#define AARCH64_ISA_V8_8A	   (aarch64_isa_flags & AARCH64_FL_V8_8A)
#define AARCH64_ISA_V8_9A	   (aarch64_isa_flags & AARCH64_FL_V8_9A)
#define AARCH64_ISA_V9A		   (aarch64_isa_flags & AARCH64_FL_V9A)
#define AARCH64_ISA_V9_1A          (aarch64_isa_flags & AARCH64_FL_V9_1A)
#define AARCH64_ISA_V9_2A          (aarch64_isa_flags & AARCH64_FL_V9_2A)
#define AARCH64_ISA_V9_3A          (aarch64_isa_flags & AARCH64_FL_V9_3A)
#define AARCH64_ISA_V9_4A	   (aarch64_isa_flags & AARCH64_FL_V9_4A)
#define AARCH64_ISA_MOPS	   (aarch64_isa_flags & AARCH64_FL_MOPS)
#define AARCH64_ISA_LS64	   (aarch64_isa_flags & AARCH64_FL_LS64)
#define AARCH64_ISA_CSSC	   (aarch64_isa_flags & AARCH64_FL_CSSC)
#define AARCH64_ISA_D128	   (aarch64_isa_flags & AARCH64_FL_D128)
#define AARCH64_ISA_THE		   (aarch64_isa_flags & AARCH64_FL_THE)
#define AARCH64_ISA_GCS		   (aarch64_isa_flags & AARCH64_FL_GCS)

/* The current function is a normal non-streaming function.  */
#define TARGET_NON_STREAMING (AARCH64_ISA_SM_OFF)

/* The current function has a streaming body.  */
#define TARGET_STREAMING (AARCH64_ISA_SM_ON)

/* The current function has a streaming-compatible body.  */
#define TARGET_STREAMING_COMPATIBLE \
  ((aarch64_isa_flags & AARCH64_FL_SM_STATE) == 0)

/* PSTATE.ZA is enabled in the current function body.  */
#define TARGET_ZA (AARCH64_ISA_ZA_ON)
/* AARCH64_FL options necessary for system register implementation.  */

/* Define AARCH64_FL aliases for architectural features which are protected
   by -march flags in binutils but which receive no special treatment by GCC.

   Such flags are inherited from the Binutils definition of system registers
   and are mapped to the architecture in which the feature is implemented.  */
#define AARCH64_FL_RAS		   AARCH64_FL_V8A
#define AARCH64_FL_LOR		   AARCH64_FL_V8_1A
#define AARCH64_FL_PAN		   AARCH64_FL_V8_1A
#define AARCH64_FL_AMU		   AARCH64_FL_V8_4A
#define AARCH64_FL_SCXTNUM	   AARCH64_FL_V8_5A
#define AARCH64_FL_ID_PFR2	   AARCH64_FL_V8_5A

/* Armv8.9-A extension feature bits defined in Binutils but absent from GCC,
   aliased to their base architecture.  */
#define AARCH64_FL_AIE		   AARCH64_FL_V8_9A
#define AARCH64_FL_DEBUGv8p9	   AARCH64_FL_V8_9A
#define AARCH64_FL_FGT2	   AARCH64_FL_V8_9A
#define AARCH64_FL_ITE		   AARCH64_FL_V8_9A
#define AARCH64_FL_PFAR	   AARCH64_FL_V8_9A
#define AARCH64_FL_PMUv3_ICNTR	   AARCH64_FL_V8_9A
#define AARCH64_FL_PMUv3_SS	   AARCH64_FL_V8_9A
#define AARCH64_FL_PMUv3p9	   AARCH64_FL_V8_9A
#define AARCH64_FL_RASv2	   AARCH64_FL_V8_9A
#define AARCH64_FL_S1PIE	   AARCH64_FL_V8_9A
#define AARCH64_FL_S1POE	   AARCH64_FL_V8_9A
#define AARCH64_FL_S2PIE	   AARCH64_FL_V8_9A
#define AARCH64_FL_S2POE	   AARCH64_FL_V8_9A
#define AARCH64_FL_SCTLR2	   AARCH64_FL_V8_9A
#define AARCH64_FL_SEBEP	   AARCH64_FL_V8_9A
#define AARCH64_FL_SPE_FDS	   AARCH64_FL_V8_9A
#define AARCH64_FL_TCR2	   AARCH64_FL_V8_9A

/* SHA2 is an optional extension to AdvSIMD.  */
#define TARGET_SHA2 (AARCH64_ISA_SHA2)

/* SHA3 is an optional extension to AdvSIMD.  */
#define TARGET_SHA3 (AARCH64_ISA_SHA3)

/* AES is an optional extension to AdvSIMD.  */
#define TARGET_AES (AARCH64_ISA_AES)

/* SM is an optional extension to AdvSIMD.  */
#define TARGET_SM4 (AARCH64_ISA_SM4)

/* FP16FML is an optional extension to AdvSIMD.  */
#define TARGET_F16FML (TARGET_SIMD && AARCH64_ISA_F16FML && TARGET_FP_F16INST)

/* CRC instructions that can be enabled through +crc arch extension.  */
#define TARGET_CRC32 (AARCH64_ISA_CRC)

/* Atomic instructions that can be enabled through the +lse extension.  */
#define TARGET_LSE (AARCH64_ISA_LSE)

/* ARMv8.2-A FP16 support that can be enabled through the +fp16 extension.  */
#define TARGET_FP_F16INST (AARCH64_ISA_F16)
#define TARGET_SIMD_F16INST (TARGET_SIMD && AARCH64_ISA_F16)

/* Dot Product is an optional extension to AdvSIMD enabled through +dotprod.  */
#define TARGET_DOTPROD (AARCH64_ISA_DOTPROD)

/* SVE instructions, enabled through +sve.  */
#define TARGET_SVE (AARCH64_ISA_SVE)

/* SVE2 instructions, enabled through +sve2.  */
#define TARGET_SVE2 (AARCH64_ISA_SVE2)

/* SVE2 AES instructions, enabled through +sve2-aes.  */
#define TARGET_SVE2_AES (AARCH64_ISA_SVE2_AES && TARGET_NON_STREAMING)

/* SVE2 BITPERM instructions, enabled through +sve2-bitperm.  */
#define TARGET_SVE2_BITPERM (AARCH64_ISA_SVE2_BITPERM && TARGET_NON_STREAMING)

/* SVE2 SHA3 instructions, enabled through +sve2-sha3.  */
#define TARGET_SVE2_SHA3 (AARCH64_ISA_SVE2_SHA3 && TARGET_NON_STREAMING)

/* SVE2 SM4 instructions, enabled through +sve2-sm4.  */
#define TARGET_SVE2_SM4 (AARCH64_ISA_SVE2_SM4 && TARGET_NON_STREAMING)

/* SME instructions, enabled through +sme.  Note that this does not
   imply anything about the state of PSTATE.SM.  */
#define TARGET_SME (AARCH64_ISA_SME)

/* Same with streaming mode enabled.  */
#define TARGET_STREAMING_SME (TARGET_STREAMING && TARGET_SME)

/* The FEAT_SME_I16I64 extension to SME, enabled through +sme-i16i64.  */
#define TARGET_SME_I16I64 (AARCH64_ISA_SME_I16I64)

/* The FEAT_SME_F64F64 extension to SME, enabled through +sme-f64f64.  */
#define TARGET_SME_F64F64 (AARCH64_ISA_SME_F64F64)

/* SME2 instructions, enabled through +sme2.  */
#define TARGET_SME2 (AARCH64_ISA_SME2)

/* Same with streaming mode enabled.  */
#define TARGET_STREAMING_SME2 (TARGET_STREAMING && TARGET_SME2)

/* ARMv8.3-A features.  */
#define TARGET_ARMV8_3	(AARCH64_ISA_V8_3A)

/* Javascript conversion instruction from Armv8.3-a.  */
#define TARGET_JSCVT	(TARGET_FLOAT && AARCH64_ISA_V8_3A)

/* Armv8.3-a Complex number extension to AdvSIMD extensions.  */
#define TARGET_COMPLEX (TARGET_SIMD && TARGET_ARMV8_3)

/* Floating-point rounding instructions from Armv8.5-a.  */
#define TARGET_FRINT (AARCH64_ISA_V8_5A && TARGET_FLOAT)

/* TME instructions are enabled.  */
#define TARGET_TME (AARCH64_ISA_TME)

/* Random number instructions from Armv8.5-a.  */
#define TARGET_RNG (AARCH64_ISA_RNG)

/* Memory Tagging instructions optional to Armv8.5 enabled through +memtag.  */
#define TARGET_MEMTAG (AARCH64_ISA_MEMTAG)

/* I8MM instructions are enabled through +i8mm.  */
#define TARGET_I8MM (AARCH64_ISA_I8MM)
#define TARGET_SVE_I8MM (TARGET_SVE && AARCH64_ISA_I8MM)

/* F32MM instructions are enabled through +f32mm.  */
#define TARGET_SVE_F32MM (AARCH64_ISA_F32MM)

/* F64MM instructions are enabled through +f64mm.  */
#define TARGET_SVE_F64MM (AARCH64_ISA_F64MM)

/* BF16 instructions are enabled through +bf16.  */
#define TARGET_BF16_FP (AARCH64_ISA_BF16)
#define TARGET_BF16_SIMD (AARCH64_ISA_BF16 && TARGET_SIMD)
#define TARGET_SVE_BF16 (TARGET_SVE && AARCH64_ISA_BF16)

/* PAUTH instructions are enabled through +pauth.  */
#define TARGET_PAUTH (AARCH64_ISA_PAUTH)

/* BTI instructions exist from Armv8.5-a onwards.  Their automatic use is
   enabled through -mbranch-protection by using NOP-space instructions,
   but this TARGET_ is used for defining BTI-related ACLE things.  */
#define TARGET_BTI (AARCH64_ISA_V8_5A)

/* MOPS instructions are enabled through +mops.  */
#define TARGET_MOPS (AARCH64_ISA_MOPS)

/* LS64 instructions are enabled through +ls64.  */
#define TARGET_LS64 (AARCH64_ISA_LS64)

/* CSSC instructions are enabled through +cssc.  */
#define TARGET_CSSC (AARCH64_ISA_CSSC)

/* Make sure this is always defined so we don't have to check for ifdefs
   but rather use normal ifs.  */
#ifndef TARGET_FIX_ERR_A53_835769_DEFAULT
#define TARGET_FIX_ERR_A53_835769_DEFAULT 0
#else
#undef TARGET_FIX_ERR_A53_835769_DEFAULT
#define TARGET_FIX_ERR_A53_835769_DEFAULT 1
#endif

/* SB instruction is enabled through +sb.  */
#define TARGET_SB (AARCH64_ISA_SB)

/* RCPC loads from Armv8.3-a.  */
#define TARGET_RCPC (AARCH64_ISA_RCPC)

/* The RCPC2 extensions from Armv8.4-a that allow immediate offsets to LDAPR
   and sign-extending versions.*/
#define TARGET_RCPC2 (AARCH64_ISA_RCPC8_4)

/* RCPC3 (Release Consistency) extensions, optional from Armv8.2-a.  */
#define TARGET_RCPC3 (AARCH64_ISA_RCPC3)

/* Apply the workaround for Cortex-A53 erratum 835769.  */
#define TARGET_FIX_ERR_A53_835769	\
  ((aarch64_fix_a53_err835769 == 2)	\
  ? TARGET_FIX_ERR_A53_835769_DEFAULT : aarch64_fix_a53_err835769)

/* Make sure this is always defined so we don't have to check for ifdefs
   but rather use normal ifs.  */
#ifndef TARGET_FIX_ERR_A53_843419_DEFAULT
#define TARGET_FIX_ERR_A53_843419_DEFAULT 0
#else
#undef TARGET_FIX_ERR_A53_843419_DEFAULT
#define TARGET_FIX_ERR_A53_843419_DEFAULT 1
#endif

/* Apply the workaround for Cortex-A53 erratum 843419.  */
#define TARGET_FIX_ERR_A53_843419	\
  ((aarch64_fix_a53_err843419 == 2)	\
  ? TARGET_FIX_ERR_A53_843419_DEFAULT : aarch64_fix_a53_err843419)

/* ARMv8.1-A Adv.SIMD support.  */
#define TARGET_SIMD_RDMA (TARGET_SIMD && AARCH64_ISA_RDMA)

/* Armv9.4-A features.  */
#define TARGET_ARMV9_4 (AARCH64_ISA_V9_4A)

/*  128-bit System Registers and Instructions from Armv9.4-a are enabled
    through +d128.  */
#define TARGET_D128 (AARCH64_ISA_D128)

/*  Armv8.9-A/9.4-A Translation Hardening Extension system registers are
    enabled through +the.  */
#define TARGET_THE (AARCH64_ISA_THE)

/*  Armv9.4-A Guarded Control Stack extension system registers are
    enabled through +gcs.  */
#define TARGET_GCS (AARCH64_ISA_GCS)


/* Standard register usage.  */

/* 31 64-bit general purpose registers R0-R30:
   R30		LR (link register)
   R29		FP (frame pointer)
   R19-R28	Callee-saved registers
   R18		The platform register; use as temporary register.
   R17		IP1 The second intra-procedure-call temporary register
		(can be used by call veneers and PLT code); otherwise use
		as a temporary register
   R16		IP0 The first intra-procedure-call temporary register (can
		be used by call veneers and PLT code); otherwise use as a
		temporary register
   R9-R15	Temporary registers
   R8		Structure value parameter / temporary register
   R0-R7	Parameter/result registers

   SP		stack pointer, encoded as X/R31 where permitted.
   ZR		zero register, encoded as X/R31 elsewhere

   32 x 128-bit floating-point/vector registers
   V16-V31	Caller-saved (temporary) registers
   V8-V15	Callee-saved registers
   V0-V7	Parameter/result registers

   The vector register V0 holds scalar B0, H0, S0 and D0 in its least
   significant bits.  Unlike AArch32 S1 is not packed into D0, etc.

   P0-P7        Predicate low registers: valid in all predicate contexts
   P8-P15       Predicate high registers: used as scratch space

   FFR		First Fault Register, a fixed-use SVE predicate register
   FFRT		FFR token: a fake register used for modelling dependencies

   VG           Pseudo "vector granules" register

   VG is the number of 64-bit elements in an SVE vector.  We define
   it as a hard register so that we can easily map it to the DWARF VG
   register.  GCC internally uses the poly_int variable aarch64_sve_vg
   instead.  */

#define FIXED_REGISTERS					\
  {							\
    0, 0, 0, 0,   0, 0, 0, 0,	/* R0 - R7 */		\
    0, 0, 0, 0,   0, 0, 0, 0,	/* R8 - R15 */		\
    0, 0, 0, 0,   0, 0, 0, 0,	/* R16 - R23 */		\
    0, 0, 0, 0,   0, 1, 0, 1,	/* R24 - R30, SP */	\
    0, 0, 0, 0,   0, 0, 0, 0,   /* V0 - V7 */           \
    0, 0, 0, 0,   0, 0, 0, 0,   /* V8 - V15 */		\
    0, 0, 0, 0,   0, 0, 0, 0,   /* V16 - V23 */         \
    0, 0, 0, 0,   0, 0, 0, 0,   /* V24 - V31 */         \
    1, 1, 1, 1,			/* SFP, AP, CC, VG */	\
    0, 0, 0, 0,   0, 0, 0, 0,   /* P0 - P7 */           \
    0, 0, 0, 0,   0, 0, 0, 0,   /* P8 - P15 */          \
    1, 1,			/* FFR and FFRT */	\
    1, 1, 1, 1, 1, 1, 1, 1	/* Fake registers */	\
  }

/* X30 is marked as caller-saved which is in line with regular function call
   behavior since the call instructions clobber it; AARCH64_EXPAND_CALL does
   that for regular function calls and avoids it for sibcalls.  X30 is
   considered live for sibcalls; EPILOGUE_USES helps achieve that by returning
   true but not until function epilogues have been generated.  This ensures
   that X30 is available for use in leaf functions if needed.  */

#define CALL_REALLY_USED_REGISTERS			\
  {							\
    1, 1, 1, 1,   1, 1, 1, 1,	/* R0 - R7 */		\
    1, 1, 1, 1,   1, 1, 1, 1,	/* R8 - R15 */		\
    1, 1, 1, 0,   0, 0, 0, 0,	/* R16 - R23 */		\
    0, 0, 0, 0,   0, 1, 1, 1,	/* R24 - R30, SP */	\
    1, 1, 1, 1,   1, 1, 1, 1,	/* V0 - V7 */		\
    0, 0, 0, 0,   0, 0, 0, 0,	/* V8 - V15 */		\
    1, 1, 1, 1,   1, 1, 1, 1,   /* V16 - V23 */         \
    1, 1, 1, 1,   1, 1, 1, 1,   /* V24 - V31 */         \
    1, 1, 1, 0,			/* SFP, AP, CC, VG */	\
    1, 1, 1, 1,   1, 1, 1, 1,	/* P0 - P7 */		\
    1, 1, 1, 1,   1, 1, 1, 1,	/* P8 - P15 */		\
    1, 1,			/* FFR and FFRT */	\
    0, 0, 0, 0, 0, 0, 0, 0	/* Fake registers */	\
  }

#define REGISTER_NAMES						\
  {								\
    "x0",  "x1",  "x2",  "x3",  "x4",  "x5",  "x6",  "x7",	\
    "x8",  "x9",  "x10", "x11", "x12", "x13", "x14", "x15",	\
    "x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23",	\
    "x24", "x25", "x26", "x27", "x28", "x29", "x30", "sp",	\
    "v0",  "v1",  "v2",  "v3",  "v4",  "v5",  "v6",  "v7",	\
    "v8",  "v9",  "v10", "v11", "v12", "v13", "v14", "v15",	\
    "v16", "v17", "v18", "v19", "v20", "v21", "v22", "v23",	\
    "v24", "v25", "v26", "v27", "v28", "v29", "v30", "v31",	\
    "sfp", "ap",  "cc",  "vg",					\
    "p0",  "p1",  "p2",  "p3",  "p4",  "p5",  "p6",  "p7",	\
    "p8",  "p9",  "p10", "p11", "p12", "p13", "p14", "p15",	\
    "ffr", "ffrt",						\
    "lowering", "tpidr2_block", "sme_state", "tpidr2_setup",	\
    "za_free", "za_saved", "za", "zt0"				\
  }

/* Generate the register aliases for core register N */
#define R_ALIASES(N) {"r" # N, R0_REGNUM + (N)}, \
                     {"w" # N, R0_REGNUM + (N)}

#define V_ALIASES(N) {"q" # N, V0_REGNUM + (N)}, \
                     {"d" # N, V0_REGNUM + (N)}, \
                     {"s" # N, V0_REGNUM + (N)}, \
                     {"h" # N, V0_REGNUM + (N)}, \
                     {"b" # N, V0_REGNUM + (N)}, \
                     {"z" # N, V0_REGNUM + (N)}

#define P_ALIASES(N) {"pn" # N, P0_REGNUM + (N)}

/* Provide aliases for all of the ISA defined register name forms.
   These aliases are convenient for use in the clobber lists of inline
   asm statements.  */

#define ADDITIONAL_REGISTER_NAMES \
  { R_ALIASES(0),  R_ALIASES(1),  R_ALIASES(2),  R_ALIASES(3),  \
    R_ALIASES(4),  R_ALIASES(5),  R_ALIASES(6),  R_ALIASES(7),  \
    R_ALIASES(8),  R_ALIASES(9),  R_ALIASES(10), R_ALIASES(11), \
    R_ALIASES(12), R_ALIASES(13), R_ALIASES(14), R_ALIASES(15), \
    R_ALIASES(16), R_ALIASES(17), R_ALIASES(18), R_ALIASES(19), \
    R_ALIASES(20), R_ALIASES(21), R_ALIASES(22), R_ALIASES(23), \
    R_ALIASES(24), R_ALIASES(25), R_ALIASES(26), R_ALIASES(27), \
    R_ALIASES(28), R_ALIASES(29), R_ALIASES(30), {"wsp", R0_REGNUM + 31}, \
    V_ALIASES(0),  V_ALIASES(1),  V_ALIASES(2),  V_ALIASES(3),  \
    V_ALIASES(4),  V_ALIASES(5),  V_ALIASES(6),  V_ALIASES(7),  \
    V_ALIASES(8),  V_ALIASES(9),  V_ALIASES(10), V_ALIASES(11), \
    V_ALIASES(12), V_ALIASES(13), V_ALIASES(14), V_ALIASES(15), \
    V_ALIASES(16), V_ALIASES(17), V_ALIASES(18), V_ALIASES(19), \
    V_ALIASES(20), V_ALIASES(21), V_ALIASES(22), V_ALIASES(23), \
    V_ALIASES(24), V_ALIASES(25), V_ALIASES(26), V_ALIASES(27), \
    V_ALIASES(28), V_ALIASES(29), V_ALIASES(30), V_ALIASES(31), \
    P_ALIASES(0),  P_ALIASES(1),  P_ALIASES(2),  P_ALIASES(3),  \
    P_ALIASES(4),  P_ALIASES(5),  P_ALIASES(6),  P_ALIASES(7),  \
    P_ALIASES(8),  P_ALIASES(9),  P_ALIASES(10), P_ALIASES(11), \
    P_ALIASES(12), P_ALIASES(13), P_ALIASES(14), P_ALIASES(15)  \
  }

#define EPILOGUE_USES(REGNO) (aarch64_epilogue_uses (REGNO))

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  This is only true if the function
   uses alloca.  */
#define EXIT_IGNORE_STACK	(cfun->calls_alloca)

#define STATIC_CHAIN_REGNUM		R18_REGNUM
#define HARD_FRAME_POINTER_REGNUM	R29_REGNUM
#define FRAME_POINTER_REGNUM		SFP_REGNUM
#define STACK_POINTER_REGNUM		SP_REGNUM
#define ARG_POINTER_REGNUM		AP_REGNUM
#define FIRST_PSEUDO_REGISTER		(LAST_FAKE_REGNUM + 1)

/* The number of argument registers available for each class.  */
#define NUM_ARG_REGS			8
#define NUM_FP_ARG_REGS			8
#define NUM_PR_ARG_REGS			4

/* A Homogeneous Floating-Point or Short-Vector Aggregate may have at most
   four members.  */
#define HA_MAX_NUM_FLDS		4

/* External dwarf register number scheme.  These number are used to
   identify registers in dwarf debug information, the values are
   defined by the AArch64 ABI.  The numbering scheme is independent of
   GCC's internal register numbering scheme.  */

#define AARCH64_DWARF_R0        0

/* The number of R registers, note 31! not 32.  */
#define AARCH64_DWARF_NUMBER_R 31

#define AARCH64_DWARF_SP       31
#define AARCH64_DWARF_VG       46
#define AARCH64_DWARF_P0       48
#define AARCH64_DWARF_V0       64

/* The number of V registers.  */
#define AARCH64_DWARF_NUMBER_V 32

/* For signal frames we need to use an alternative return column.  This
   value must not correspond to a hard register and must be out of the
   range of DWARF_FRAME_REGNUM().  */
#define DWARF_ALT_FRAME_RETURN_COLUMN   \
  (AARCH64_DWARF_V0 + AARCH64_DWARF_NUMBER_V)

/* We add 1 extra frame register for use as the
   DWARF_ALT_FRAME_RETURN_COLUMN.  */
#define DWARF_FRAME_REGISTERS           (DWARF_ALT_FRAME_RETURN_COLUMN + 1)


#define DEBUGGER_REGNO(REGNO)	aarch64_debugger_regno (REGNO)
/* Provide a definition of DWARF_FRAME_REGNUM here so that fallback unwinders
   can use DWARF_ALT_FRAME_RETURN_COLUMN defined below.  This is just the same
   as the default definition in dwarf2out.cc.  */
#undef DWARF_FRAME_REGNUM
#define DWARF_FRAME_REGNUM(REGNO)	DEBUGGER_REGNO (REGNO)

#define DWARF_FRAME_RETURN_COLUMN	DWARF_FRAME_REGNUM (LR_REGNUM)

#define DWARF2_UNWIND_INFO 1

/* Use R0 through R3 to pass exception handling information.  */
#define EH_RETURN_DATA_REGNO(N) \
  ((N) < 4 ? ((unsigned int) R0_REGNUM + (N)) : INVALID_REGNUM)

/* Select a format to encode pointers in exception handling data.  */
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE, GLOBAL) \
  aarch64_asm_preferred_eh_data_format ((CODE), (GLOBAL))

/* Output the assembly strings we want to add to a function definition.  */
#define ASM_DECLARE_FUNCTION_NAME(STR, NAME, DECL)	\
  aarch64_declare_function_name (STR, NAME, DECL)

/* Output assembly strings for alias definition.  */
#define ASM_OUTPUT_DEF_FROM_DECLS(STR, DECL, TARGET) \
  aarch64_asm_output_alias (STR, DECL, TARGET)

/* Output assembly strings for undefined extern symbols.  */
#undef ASM_OUTPUT_EXTERNAL
#define ASM_OUTPUT_EXTERNAL(STR, DECL, NAME) \
  aarch64_asm_output_external (STR, DECL, NAME)

/* Output assembly strings after .cfi_startproc is emitted.  */
#define ASM_POST_CFI_STARTPROC  aarch64_post_cfi_startproc

/* For EH returns X4 is a flag that is set in the EH return
   code paths and then X5 and X6 contain the stack adjustment
   and return address respectively.  */
#define EH_RETURN_TAKEN_RTX	gen_rtx_REG (Pmode, R4_REGNUM)
#define EH_RETURN_STACKADJ_RTX	gen_rtx_REG (Pmode, R5_REGNUM)
#define EH_RETURN_HANDLER_RTX	gen_rtx_REG (Pmode, R6_REGNUM)

#undef TARGET_COMPUTE_FRAME_LAYOUT
#define TARGET_COMPUTE_FRAME_LAYOUT aarch64_layout_frame

/* Register in which the structure value is to be returned.  */
#define AARCH64_STRUCT_VALUE_REGNUM R8_REGNUM

/* Non-zero if REGNO is part of the Core register set.

   The rather unusual way of expressing this check is to avoid
   warnings when building the compiler when R0_REGNUM is 0 and REGNO
   is unsigned.  */
#define GP_REGNUM_P(REGNO)						\
  (((unsigned) (REGNO - R0_REGNUM)) <= (R30_REGNUM - R0_REGNUM))

/* Registers known to be preserved over a BL instruction.  This consists of the
   GENERAL_REGS without x16, x17, and x30.  The x30 register is changed by the
   BL instruction itself, while the x16 and x17 registers may be used by
   veneers which can be inserted by the linker.  */
#define STUB_REGNUM_P(REGNO) \
  (GP_REGNUM_P (REGNO) \
   && (REGNO) != R16_REGNUM \
   && (REGNO) != R17_REGNUM \
   && (REGNO) != R30_REGNUM) \

#define W8_W11_REGNUM_P(REGNO) \
  IN_RANGE (REGNO, R8_REGNUM, R11_REGNUM)

#define W12_W15_REGNUM_P(REGNO) \
  IN_RANGE (REGNO, R12_REGNUM, R15_REGNUM)

#define FP_REGNUM_P(REGNO)			\
  (((unsigned) (REGNO - V0_REGNUM)) <= (V31_REGNUM - V0_REGNUM))

#define FP_LO_REGNUM_P(REGNO)            \
  (((unsigned) (REGNO - V0_REGNUM)) <= (V15_REGNUM - V0_REGNUM))

#define FP_LO8_REGNUM_P(REGNO)            \
  (((unsigned) (REGNO - V0_REGNUM)) <= (V7_REGNUM - V0_REGNUM))

#define PR_REGNUM_P(REGNO)\
  (((unsigned) (REGNO - P0_REGNUM)) <= (P15_REGNUM - P0_REGNUM))

#define PR_LO_REGNUM_P(REGNO)\
  (((unsigned) (REGNO - P0_REGNUM)) <= (P7_REGNUM - P0_REGNUM))

#define FP_SIMD_SAVED_REGNUM_P(REGNO)			\
  (((unsigned) (REGNO - V8_REGNUM)) <= (V23_REGNUM - V8_REGNUM))

#define FAKE_REGNUM_P(REGNO) \
  IN_RANGE (REGNO, FIRST_FAKE_REGNUM, LAST_FAKE_REGNUM)

/* Register and constant classes.  */

enum reg_class
{
  NO_REGS,
  W8_W11_REGS,
  W12_W15_REGS,
  TAILCALL_ADDR_REGS,
  STUB_REGS,
  GENERAL_REGS,
  STACK_REG,
  POINTER_REGS,
  FP_LO8_REGS,
  FP_LO_REGS,
  FP_REGS,
  POINTER_AND_FP_REGS,
  PR_LO_REGS,
  PR_HI_REGS,
  PR_REGS,
  FFR_REGS,
  PR_AND_FFR_REGS,
  FAKE_REGS,
  ALL_REGS,
  LIM_REG_CLASSES		/* Last */
};

#define N_REG_CLASSES	((int) LIM_REG_CLASSES)

#define REG_CLASS_NAMES				\
{						\
  "NO_REGS",					\
  "W8_W11_REGS",				\
  "W12_W15_REGS",				\
  "TAILCALL_ADDR_REGS",				\
  "STUB_REGS",					\
  "GENERAL_REGS",				\
  "STACK_REG",					\
  "POINTER_REGS",				\
  "FP_LO8_REGS",				\
  "FP_LO_REGS",					\
  "FP_REGS",					\
  "POINTER_AND_FP_REGS",			\
  "PR_LO_REGS",					\
  "PR_HI_REGS",					\
  "PR_REGS",					\
  "FFR_REGS",					\
  "PR_AND_FFR_REGS",				\
  "FAKE_REGS",					\
  "ALL_REGS"					\
}

#define REG_CLASS_CONTENTS						\
{									\
  { 0x00000000, 0x00000000, 0x00000000 },	/* NO_REGS */		\
  { 0x00000f00, 0x00000000, 0x00000000 },	/* W8_W11_REGS */	\
  { 0x0000f000, 0x00000000, 0x00000000 },	/* W12_W15_REGS */	\
  { 0x00030000, 0x00000000, 0x00000000 },	/* TAILCALL_ADDR_REGS */\
  { 0x3ffcffff, 0x00000000, 0x00000000 },	/* STUB_REGS */		\
  { 0x7fffffff, 0x00000000, 0x00000003 },	/* GENERAL_REGS */	\
  { 0x80000000, 0x00000000, 0x00000000 },	/* STACK_REG */		\
  { 0xffffffff, 0x00000000, 0x00000003 },	/* POINTER_REGS */	\
  { 0x00000000, 0x000000ff, 0x00000000 },       /* FP_LO8_REGS  */	\
  { 0x00000000, 0x0000ffff, 0x00000000 },       /* FP_LO_REGS  */	\
  { 0x00000000, 0xffffffff, 0x00000000 },       /* FP_REGS  */		\
  { 0xffffffff, 0xffffffff, 0x00000003 },	/* POINTER_AND_FP_REGS */\
  { 0x00000000, 0x00000000, 0x00000ff0 },	/* PR_LO_REGS */	\
  { 0x00000000, 0x00000000, 0x000ff000 },	/* PR_HI_REGS */	\
  { 0x00000000, 0x00000000, 0x000ffff0 },	/* PR_REGS */		\
  { 0x00000000, 0x00000000, 0x00300000 },	/* FFR_REGS */		\
  { 0x00000000, 0x00000000, 0x003ffff0 },	/* PR_AND_FFR_REGS */	\
  { 0x00000000, 0x00000000, 0x3fc00000 },	/* FAKE_REGS */		\
  { 0xffffffff, 0xffffffff, 0x000fffff }	/* ALL_REGS */		\
}

#define REGNO_REG_CLASS(REGNO)	aarch64_regno_regclass (REGNO)

#define INDEX_REG_CLASS	GENERAL_REGS
#define BASE_REG_CLASS  POINTER_REGS

/* Register pairs used to eliminate unneeded registers that point into
   the stack frame.  */
#define ELIMINABLE_REGS							\
{									\
  { ARG_POINTER_REGNUM,		STACK_POINTER_REGNUM		},	\
  { ARG_POINTER_REGNUM,		HARD_FRAME_POINTER_REGNUM	},	\
  { FRAME_POINTER_REGNUM,	STACK_POINTER_REGNUM		},	\
  { FRAME_POINTER_REGNUM,	HARD_FRAME_POINTER_REGNUM	},	\
}

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  (OFFSET) = aarch64_initial_elimination_offset (FROM, TO)

/* CPU/ARCH option handling.  */
#include "config/aarch64/aarch64-opts.h"

enum target_cpus
{
#define AARCH64_CORE(NAME, INTERNAL_IDENT, SCHED, ARCH, FLAGS, COSTS, IMP, PART, VARIANT) \
  TARGET_CPU_##INTERNAL_IDENT,
#include "aarch64-cores.def"
};

/* If there is no CPU defined at configure, use generic as default.  */
#ifndef TARGET_CPU_DEFAULT
# define TARGET_CPU_DEFAULT TARGET_CPU_generic_armv8_a
#endif

/* If inserting NOP before a mult-accumulate insn remember to adjust the
   length so that conditional branching code is updated appropriately.  */
#define ADJUST_INSN_LENGTH(insn, length)	\
  do						\
    {						\
       if (aarch64_madd_needs_nop (insn))	\
         length += 4;				\
    } while (0)

#define FINAL_PRESCAN_INSN(INSN, OPVEC, NOPERANDS)	\
    aarch64_final_prescan_insn (INSN);			\

/* The processor for which instructions should be scheduled.  */
extern enum aarch64_processor aarch64_tune;

/* RTL generation support.  */
#define INIT_EXPANDERS aarch64_init_expanders ()


/* Stack layout; function entry, exit and calling.  */
#define STACK_GROWS_DOWNWARD	1

#define FRAME_GROWS_DOWNWARD	1

#define ACCUMULATE_OUTGOING_ARGS	1

#define FIRST_PARM_OFFSET(FNDECL) 0

/* Fix for VFP */
#define LIBCALL_VALUE(MODE)  \
  gen_rtx_REG (MODE, FLOAT_MODE_P (MODE) ? V0_REGNUM : R0_REGNUM)

#define DEFAULT_PCC_STRUCT_RETURN 0

#if defined(HAVE_POLY_INT_H) && defined(GCC_VEC_H)
struct GTY (()) aarch64_frame
{
  /* The offset from the bottom of the static frame (the bottom of the
     outgoing arguments) of each register save slot, or -2 if no save is
     needed.  */
  poly_int64 reg_offset[LAST_SAVED_REGNUM + 1];

  /* The list of GPRs, FPRs and predicate registers that have nonnegative
     entries in reg_offset.  The registers are listed in order of
     increasing offset (rather than increasing register number).  */
  vec<unsigned, va_gc_atomic> *saved_gprs;
  vec<unsigned, va_gc_atomic> *saved_fprs;
  vec<unsigned, va_gc_atomic> *saved_prs;

  /* The offset from the base of the frame of a 64-bit slot whose low
     bit contains the incoming value of PSTATE.SM.  This slot must be
     within reach of the hard frame pointer.

     The offset is -1 if such a slot isn't needed.  */
  poly_int64 old_svcr_offset;

  /* The number of extra stack bytes taken up by register varargs.
     This area is allocated by the callee at the very top of the
     frame.  This value is rounded up to a multiple of
     STACK_BOUNDARY.  */
  HOST_WIDE_INT saved_varargs_size;

  /* The number of bytes between the bottom of the static frame (the bottom
     of the outgoing arguments) and the bottom of the register save area.
     This value is always a multiple of STACK_BOUNDARY.  */
  poly_int64 bytes_below_saved_regs;

  /* The number of bytes between the bottom of the static frame (the bottom
     of the outgoing arguments) and the hard frame pointer.  This value is
     always a multiple of STACK_BOUNDARY.  */
  poly_int64 bytes_below_hard_fp;

  /* The number of bytes between the top of the locals area and the top
     of the frame (the incomming SP).  This value is always a multiple of
     STACK_BOUNDARY.  */
  poly_int64 bytes_above_locals;

  /* The number of bytes between the hard_frame_pointer and the top of
     the frame (the incomming SP).  This value is always a multiple of
     STACK_BOUNDARY.  */
  poly_int64 bytes_above_hard_fp;

  /* The size of the frame, i.e. the number of bytes between the bottom
     of the outgoing arguments and the incoming SP.  This value is always
     a multiple of STACK_BOUNDARY.  */
  poly_int64 frame_size;

  /* The size of the initial stack adjustment before saving callee-saves.  */
  poly_int64 initial_adjust;

  /* The writeback value when pushing callee-save registers.
     It is zero when no push is used.  */
  HOST_WIDE_INT callee_adjust;

  /* The size of the stack adjustment before saving or after restoring
     SVE registers.  */
  poly_int64 sve_callee_adjust;

  /* The size of the stack adjustment after saving callee-saves.  */
  poly_int64 final_adjust;

  /* Store FP,LR and setup a frame pointer.  */
  bool emit_frame_chain;

  /* In each frame, we can associate up to two register saves with the
     initial stack allocation.  This happens in one of two ways:

     (1) Using an STR or STP with writeback to perform the initial
	 stack allocation.  When EMIT_FRAME_CHAIN, the registers will
	 be those needed to create a frame chain.

	 Indicated by CALLEE_ADJUST != 0.

     (2) Using a separate STP to set up the frame record, after the
	 initial stack allocation but before setting up the frame pointer.
	 This is used if the offset is too large to use writeback.

	 Indicated by CALLEE_ADJUST == 0 && EMIT_FRAME_CHAIN.

     These fields indicate which registers we've decided to handle using
     (1) or (2), or INVALID_REGNUM if none.

     In some cases we don't always need to pop all registers in the push
     candidates, pop candidates record which registers need to be popped
     eventually.  The initial value of a pop candidate is copied from its
     corresponding push candidate.

     Currently, different pop candidates are only used for shadow call
     stack.  When "-fsanitize=shadow-call-stack" is specified, we replace
     x30 in the pop candidate with INVALID_REGNUM to ensure that x30 is
     not popped twice.  */
  unsigned wb_push_candidate1;
  unsigned wb_push_candidate2;
  unsigned wb_pop_candidate1;
  unsigned wb_pop_candidate2;

  /* Big-endian SVE frames need a spare predicate register in order
     to save vector registers in the correct layout for unwinding.
     This is the register they should use.  */
  unsigned spare_pred_reg;

  /* An SVE register that is saved below the hard frame pointer and that acts
     as a probe for later allocations, or INVALID_REGNUM if none.  */
  unsigned sve_save_and_probe;

  /* A register that is saved at the hard frame pointer and that acts
     as a probe for later allocations, or INVALID_REGNUM if none.  */
  unsigned hard_fp_save_and_probe;

  bool laid_out;

  /* True if shadow call stack should be enabled for the current function.  */
  bool is_scs_enabled;
};

#ifdef hash_set_h
typedef struct GTY (()) machine_function
{
  struct aarch64_frame frame;
  /* One entry for each hard register.  */
  bool reg_is_wrapped_separately[LAST_SAVED_REGNUM];
  /* One entry for each general purpose register.  */
  rtx call_via[SP_REGNUM];

  /* A pseudo register that points to the function's TPIDR2 block, or null
     if the function doesn't have a TPIDR2 block.  */
  rtx tpidr2_block;

  /* A pseudo register that points to the function's ZA save buffer,
     or null if none.  */
  rtx za_save_buffer;

  /* A stack slot that stores the contents of the function's ZT0 state.  */
  rtx zt0_save_buffer;

  bool label_is_assembled;

  /* True if we've expanded at least one call to a function that changes
     PSTATE.SM.  This should only be used for saving compile time: false
     guarantees that no such mode switch exists.  */
  bool call_switches_pstate_sm;

  /* Used to generated unique identifiers for each update to ZA by an
     asm statement.  */
  unsigned int next_asm_update_za_id;

  /* A set of all decls that have been passed to a vld1 intrinsic in the
     current function.  This is used to help guide the vector cost model.  */
  hash_set<tree> *vector_load_decls;

  /* An instruction that was emitted at the start of the function to
     set an Advanced SIMD pseudo register to zero.  If the instruction
     still exists and still fulfils its original purpose. the same register
     can be reused by other code.  */
  rtx_insn *advsimd_zero_insn;
} machine_function;
#endif
#endif

/* Which ABI to use.  */
enum aarch64_abi_type
{
  AARCH64_ABI_LP64 = 0,
  AARCH64_ABI_ILP32 = 1
};

#ifndef AARCH64_ABI_DEFAULT
#define AARCH64_ABI_DEFAULT AARCH64_ABI_LP64
#endif

#define TARGET_ILP32	(aarch64_abi & AARCH64_ABI_ILP32)

enum arm_pcs
{
  ARM_PCS_AAPCS64,		/* Base standard AAPCS for 64 bit.  */
  ARM_PCS_SIMD,			/* For aarch64_vector_pcs functions.  */
  ARM_PCS_SVE,			/* For functions that pass or return
				   values in SVE registers.  */
  ARM_PCS_TLSDESC,		/* For targets of tlsdesc calls.  */
  ARM_PCS_UNKNOWN
};




/* We can't use machine_mode inside a generator file because it
   hasn't been created yet; we shouldn't be using any code that
   needs the real definition though, so this ought to be safe.  */
#ifdef GENERATOR_FILE
#define MACHMODE int
#else
#include "insn-modes.h"
#define MACHMODE machine_mode
#endif

#ifndef USED_FOR_TARGET
/* AAPCS related state tracking.  */
typedef struct
{
  enum arm_pcs pcs_variant;
  aarch64_feature_flags isa_mode;
  int aapcs_arg_processed;	/* No need to lay out this argument again.  */
  int aapcs_ncrn;		/* Next Core register number.  */
  int aapcs_nextncrn;		/* Next next core register number.  */
  int aapcs_nvrn;		/* Next Vector register number.  */
  int aapcs_nextnvrn;		/* Next Next Vector register number.  */
  int aapcs_nprn;		/* Next Predicate register number.  */
  int aapcs_nextnprn;		/* Next Next Predicate register number.  */
  rtx aapcs_reg;		/* Register assigned to this argument.  This
				   is NULL_RTX if this parameter goes on
				   the stack.  */
  MACHMODE aapcs_vfp_rmode;
  int aapcs_stack_words;	/* If the argument is passed on the stack, this
				   is the number of words needed, after rounding
				   up.  Only meaningful when
				   aapcs_reg == NULL_RTX.  */
  int aapcs_stack_size;		/* The total size (in words, per 8 byte) of the
				   stack arg area so far.  */
  bool silent_p;		/* True if we should act silently, rather than
				   raise an error for invalid calls.  */

  /* AARCH64_STATE_* flags that describe whether the function shares ZA
     and ZT0 with its callers.  */
  unsigned int shared_za_flags;
  unsigned int shared_zt0_flags;

  /* A list of registers that need to be saved and restored around a
     change to PSTATE.SM.  An auto_vec would be more convenient, but those
     can't be copied.  */
  unsigned int num_sme_mode_switch_args;
  rtx sme_mode_switch_args[12];
} CUMULATIVE_ARGS;
#endif

#define BLOCK_REG_PADDING(MODE, TYPE, FIRST) \
  (aarch64_pad_reg_upward (MODE, TYPE, FIRST) ? PAD_UPWARD : PAD_DOWNWARD)

#define PAD_VARARGS_DOWN	0

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, FNDECL, N_NAMED_ARGS) \
  aarch64_init_cumulative_args (&(CUM), FNTYPE, LIBNAME, FNDECL, N_NAMED_ARGS)

#define FUNCTION_ARG_REGNO_P(REGNO) \
  aarch64_function_arg_regno_p(REGNO)


/* ISA Features.  */

/* Addressing modes, etc.  */
#define HAVE_POST_INCREMENT	1
#define HAVE_PRE_INCREMENT	1
#define HAVE_POST_DECREMENT	1
#define HAVE_PRE_DECREMENT	1
#define HAVE_POST_MODIFY_DISP	1
#define HAVE_PRE_MODIFY_DISP	1

#define MAX_REGS_PER_ADDRESS	2

#define CONSTANT_ADDRESS_P(X)		aarch64_constant_address_p(X)

#define REGNO_OK_FOR_BASE_P(REGNO)	\
  aarch64_regno_ok_for_base_p (REGNO, true)

#define REGNO_OK_FOR_INDEX_P(REGNO) \
  aarch64_regno_ok_for_index_p (REGNO, true)

#define LEGITIMATE_PIC_OPERAND_P(X) \
  aarch64_legitimate_pic_operand_p (X)

#define CASE_VECTOR_MODE Pmode

#define DEFAULT_SIGNED_CHAR 0

/* An integer expression for the size in bits of the largest integer machine
   mode that should actually be used.  We allow pairs of registers.  */
#define MAX_FIXED_MODE_SIZE GET_MODE_BITSIZE (TImode)

/* Maximum bytes moved by a single instruction (load/store pair).  */
#define MOVE_MAX (UNITS_PER_WORD * 2)

/* The base cost overhead of a memcpy call, for MOVE_RATIO and friends.  */
#define AARCH64_CALL_RATIO 8

/* MOVE_RATIO dictates when we will use the move_by_pieces infrastructure.
   move_by_pieces will continually copy the largest safe chunks.  So a
   7-byte copy is a 4-byte + 2-byte + byte copy.  This proves inefficient
   for both size and speed of copy, so we will instead use the "cpymem"
   standard name to implement the copy.  This logic does not apply when
   targeting -mstrict-align or TARGET_MOPS, so keep a sensible default in
   that case.  */
#define MOVE_RATIO(speed) \
  ((!STRICT_ALIGNMENT || TARGET_MOPS) ? 2 : (((speed) ? 15 : AARCH64_CALL_RATIO) / 2))

/* Like MOVE_RATIO, without -mstrict-align, make decisions in "setmem" when
   we would use more than 3 scalar instructions.
   Otherwise follow a sensible default: when optimizing for size, give a better
   estimate of the length of a memset call, but use the default otherwise.  */
#define CLEAR_RATIO(speed) \
  (!STRICT_ALIGNMENT ? (TARGET_MOPS ? 0 : 4) : (speed) ? 15 : AARCH64_CALL_RATIO)

/* SET_RATIO is similar to CLEAR_RATIO, but for a non-zero constant.  Without
   -mstrict-align, make decisions in "setmem".  Otherwise follow a sensible
   default: when optimizing for size adjust the ratio to account for the
   overhead of loading the constant.  */
#define SET_RATIO(speed) \
  ((!STRICT_ALIGNMENT || TARGET_MOPS) ? 0 : (speed) ? 15 : AARCH64_CALL_RATIO - 2)

/* Disable auto-increment in move_by_pieces et al.  Use of auto-increment is
   rarely a good idea in straight-line code since it adds an extra address
   dependency between each instruction.  Better to use incrementing offsets.  */
#define USE_LOAD_POST_INCREMENT(MODE)   0
#define USE_LOAD_POST_DECREMENT(MODE)   0
#define USE_LOAD_PRE_INCREMENT(MODE)    0
#define USE_LOAD_PRE_DECREMENT(MODE)    0
#define USE_STORE_POST_INCREMENT(MODE)  0
#define USE_STORE_POST_DECREMENT(MODE)  0
#define USE_STORE_PRE_INCREMENT(MODE)   0
#define USE_STORE_PRE_DECREMENT(MODE)   0

/* WORD_REGISTER_OPERATIONS does not hold for AArch64.
   The assigned word_mode is DImode but operations narrower than SImode
   behave as 32-bit operations if using the W-form of the registers rather
   than as word_mode (64-bit) operations as WORD_REGISTER_OPERATIONS
   expects.  */
#define WORD_REGISTER_OPERATIONS 0

/* Define if loading from memory in MODE, an integral mode narrower than
   BITS_PER_WORD will either zero-extend or sign-extend.  The value of this
   macro should be the code that says which one of the two operations is
   implicitly done, or UNKNOWN if none.  */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* Define this macro to be non-zero if instructions will fail to work
   if given data not on the nominal alignment.  */
#define STRICT_ALIGNMENT		TARGET_STRICT_ALIGN

/* Enable wide bitfield accesses for more efficient bitfield code.  */
#define SLOW_BYTE_ACCESS 1

#define NO_FUNCTION_CSE	1

/* Specify the machine mode that the hardware addresses have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode		DImode

/* A C expression whose value is zero if pointers that need to be extended
   from being `POINTER_SIZE' bits wide to `Pmode' are sign-extended and
   greater then zero if they are zero-extended and less then zero if the
   ptr_extend instruction should be used.  */
#define POINTERS_EXTEND_UNSIGNED 1

/* Mode of a function address in a call instruction (for indexing purposes).  */
#define FUNCTION_MODE	Pmode

#define SELECT_CC_MODE(OP, X, Y)	aarch64_select_cc_mode (OP, X, Y)

/* Having an integer comparison mode guarantees that we can use
   reverse_condition, but the usual restrictions apply to floating-point
   comparisons.  */
#define REVERSIBLE_CC_MODE(MODE) ((MODE) != CCFPmode && (MODE) != CCFPEmode)

#define CLZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE) \
  ((VALUE) = GET_MODE_UNIT_BITSIZE (MODE), 2)
#define CTZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE) \
  ((VALUE) = GET_MODE_UNIT_BITSIZE (MODE), 2)

#define INCOMING_RETURN_ADDR_RTX gen_rtx_REG (Pmode, LR_REGNUM)

#define RETURN_ADDR_RTX aarch64_return_addr

/* BTI c + 3 insns
   + sls barrier of DSB + ISB.
   + 2 pointer-sized entries.  */
#define TRAMPOLINE_SIZE	(24 + (TARGET_ILP32 ? 8 : 16))

/* Trampolines contain dwords, so must be dword aligned.  */
#define TRAMPOLINE_ALIGNMENT 64

/* Put trampolines in the text section so that mapping symbols work
   correctly.  */
#define TRAMPOLINE_SECTION text_section

/* To start with.  */
#define BRANCH_COST(SPEED_P, PREDICTABLE_P) \
  (aarch64_branch_cost (SPEED_P, PREDICTABLE_P))


/* Assembly output.  */

/* For now we'll make all jump tables pc-relative.  */
#define CASE_VECTOR_PC_RELATIVE	1

#define CASE_VECTOR_SHORTEN_MODE(min, max, body)	\
  ((min < -0x1fff0 || max > 0x1fff0) ? SImode		\
   : (min < -0x1f0 || max > 0x1f0) ? HImode		\
   : QImode)

/* Jump table alignment is explicit in ASM_OUTPUT_CASE_LABEL.  */
#define ADDR_VEC_ALIGN(JUMPTABLE) 0

#define MCOUNT_NAME "_mcount"

#define NO_PROFILE_COUNTERS 1

/* Emit rtl for profiling.  Output assembler code to FILE
   to call "_mcount" for profiling a function entry.  */
#define PROFILE_HOOK(LABEL)						\
  {									\
    rtx fun, lr;							\
    lr = aarch64_return_addr_rtx ();					\
    fun = gen_rtx_SYMBOL_REF (Pmode, MCOUNT_NAME);			\
    emit_library_call (fun, LCT_NORMAL, VOIDmode, lr, Pmode);		\
  }

/* All the work done in PROFILE_HOOK, but still required.  */
#define FUNCTION_PROFILER(STREAM, LABELNO) do { } while (0)

/* For some reason, the Linux headers think they know how to define
   these macros.  They don't!!!  */
#undef ASM_APP_ON
#undef ASM_APP_OFF
#define ASM_APP_ON	"\t" ASM_COMMENT_START " Start of user assembly\n"
#define ASM_APP_OFF	"\t" ASM_COMMENT_START " End of user assembly\n"

#define CONSTANT_POOL_BEFORE_FUNCTION 0

/* This definition should be relocated to aarch64-elf-raw.h.  This macro
   should be undefined in aarch64-linux.h and a clear_cache pattern
   implmented to emit either the call to __aarch64_sync_cache_range()
   directly or preferably the appropriate sycall or cache clear
   instructions inline.  */
#define CLEAR_INSN_CACHE(beg, end)				\
  extern void  __aarch64_sync_cache_range (void *, void *);	\
  __aarch64_sync_cache_range (beg, end)

#define SHIFT_COUNT_TRUNCATED (!TARGET_SIMD)

/* Choose appropriate mode for caller saves, so we do the minimum
   required size of load/store.  */
#define HARD_REGNO_CALLER_SAVE_MODE(REGNO, NREGS, MODE) \
  aarch64_hard_regno_caller_save_mode ((REGNO), (NREGS), (MODE))

#undef SWITCHABLE_TARGET
#define SWITCHABLE_TARGET 1

/* Check TLS Descriptors mechanism is selected.  */
#define TARGET_TLS_DESC (aarch64_tls_dialect == TLS_DESCRIPTORS)

extern enum aarch64_code_model aarch64_cmodel;

/* When using the tiny addressing model conditional and unconditional branches
   can span the whole of the available address space (1MB).  */
#define HAS_LONG_COND_BRANCH				\
  (aarch64_cmodel == AARCH64_CMODEL_TINY		\
   || aarch64_cmodel == AARCH64_CMODEL_TINY_PIC)

#define HAS_LONG_UNCOND_BRANCH				\
  (aarch64_cmodel == AARCH64_CMODEL_TINY		\
   || aarch64_cmodel == AARCH64_CMODEL_TINY_PIC)

#define TARGET_HAS_FMV_TARGET_ATTRIBUTE 0

#define TARGET_SUPPORTS_WIDE_INT 1

/* Modes valid for AdvSIMD D registers, i.e. that fit in half a Q register.  */
#define AARCH64_VALID_SIMD_DREG_MODE(MODE) \
  ((MODE) == V2SImode || (MODE) == V4HImode || (MODE) == V8QImode \
   || (MODE) == V2SFmode || (MODE) == V4HFmode || (MODE) == DImode \
   || (MODE) == DFmode || (MODE) == V4BFmode)

/* Modes valid for AdvSIMD Q registers.  */
#define AARCH64_VALID_SIMD_QREG_MODE(MODE) \
  ((MODE) == V4SImode || (MODE) == V8HImode || (MODE) == V16QImode \
   || (MODE) == V4SFmode || (MODE) == V8HFmode || (MODE) == V2DImode \
   || (MODE) == V2DFmode || (MODE) == V8BFmode)

#define ENDIAN_LANE_N(NUNITS, N) \
  (BYTES_BIG_ENDIAN ? NUNITS - 1 - N : N)

/* Extra specs when building a native AArch64-hosted compiler.
   Option rewriting rules based on host system.  */
#if defined(__aarch64__)
extern const char *host_detect_local_cpu (int argc, const char **argv);
#define HAVE_LOCAL_CPU_DETECT
# define EXTRA_SPEC_FUNCTIONS                                           \
  { "local_cpu_detect", host_detect_local_cpu },                        \
  MCPU_TO_MARCH_SPEC_FUNCTIONS

/* Rewrite -m{arch,cpu,tune}=native based on the host system information.
   When rewriting -march=native convert it into an -mcpu option if no other
   -mcpu or -mtune was given.  */
# define MCPU_MTUNE_NATIVE_SPECS                                        \
   " %{march=native:%<march=native %:local_cpu_detect(%{mcpu=*|mtune=*:arch;:cpu})}"            \
   " %{mcpu=native:%<mcpu=native %:local_cpu_detect(cpu)}"              \
   " %{mtune=native:%<mtune=native %:local_cpu_detect(tune)}"
/* This will be used in OPTION_DEFAULT_SPECS below.
   When GCC is configured with --with-tune we don't want to materialize an
   implicit -mtune would prevent the rewriting of -march=native into
   -mcpu=native as per the above rules.  */
#define CONFIG_TUNE_SPEC						\
 { "tune", "%{!mcpu=*:%{!mtune=*:%{!march=native:-mtune=%(VALUE)}}}" },
#else
# define MCPU_MTUNE_NATIVE_SPECS ""
# define EXTRA_SPEC_FUNCTIONS MCPU_TO_MARCH_SPEC_FUNCTIONS
# define CONFIG_TUNE_SPEC                                                \
  {"tune", "%{!mcpu=*:%{!mtune=*:-mtune=%(VALUE)}}"},
#endif

/* Support for configure-time --with-arch, --with-cpu and --with-tune.
   --with-arch and --with-cpu are ignored if either -mcpu or -march is used.
   --with-tune is ignored if either -mtune or -mcpu is used (but is not
   affected by -march, except in the -march=native case as per the
   CONFIG_TUNE_SPEC above).  */
#define OPTION_DEFAULT_SPECS				\
  {"arch", "%{!march=*:%{!mcpu=*:-march=%(VALUE)}}" },	\
  {"cpu",  "%{!march=*:%{!mcpu=*:-mcpu=%(VALUE)}}" },   \
  CONFIG_TUNE_SPEC

#define MCPU_TO_MARCH_SPEC \
   " %{mcpu=*:-march=%:rewrite_mcpu(%{mcpu=*:%*})}"

extern const char *aarch64_rewrite_mcpu (int argc, const char **argv);
#define MCPU_TO_MARCH_SPEC_FUNCTIONS \
  { "rewrite_mcpu", aarch64_rewrite_mcpu },

#define ASM_CPU_SPEC \
   MCPU_TO_MARCH_SPEC

#define EXTRA_SPECS						\
  { "asm_cpu_spec",		ASM_CPU_SPEC }

#define ASM_OUTPUT_POOL_EPILOGUE  aarch64_asm_output_pool_epilogue

/* This type is the user-visible __fp16, and a pointer to that type.  We
   need it in many places in the backend.  Defined in aarch64-builtins.cc.  */
extern GTY(()) tree aarch64_fp16_type_node;
extern GTY(()) tree aarch64_fp16_ptr_type_node;

/* Pointer to the user-visible __bf16 type.  __bf16 itself is generic
   bfloat16_type_node.  Defined in aarch64-builtins.cc.  */
extern GTY(()) tree aarch64_bf16_ptr_type_node;

/* The generic unwind code in libgcc does not initialize the frame pointer.
   So in order to unwind a function using a frame pointer, the very first
   function that is unwound must save the frame pointer.  That way the frame
   pointer is restored and its value is now valid - otherwise _Unwind_GetGR
   crashes.  Libgcc can now be safely built with -fomit-frame-pointer.  */
#define LIBGCC2_UNWIND_ATTRIBUTE \
  __attribute__((optimize ("no-omit-frame-pointer")))

#ifndef USED_FOR_TARGET
extern poly_uint16 aarch64_sve_vg;

/* The number of bits and bytes in an SVE vector.  */
#define BITS_PER_SVE_VECTOR (poly_uint16 (aarch64_sve_vg * 64))
#define BYTES_PER_SVE_VECTOR (poly_uint16 (aarch64_sve_vg * 8))

/* The number of bits and bytes in an SVE predicate.  */
#define BITS_PER_SVE_PRED BYTES_PER_SVE_VECTOR
#define BYTES_PER_SVE_PRED aarch64_sve_vg

/* The SVE mode for a vector of bytes.  */
#define SVE_BYTE_MODE VNx16QImode

/* The maximum number of bytes in a fixed-size vector.  This is 256 bytes
   (for -msve-vector-bits=2048) multiplied by the maximum number of
   vectors in a structure mode (4).

   This limit must not be used for variable-size vectors, since
   VL-agnostic code must work with arbitary vector lengths.  */
#define MAX_COMPILE_TIME_VEC_BYTES (256 * 4)
#endif

#define REGMODE_NATURAL_SIZE(MODE) aarch64_regmode_natural_size (MODE)

/* Allocate a minimum of STACK_CLASH_MIN_BYTES_OUTGOING_ARGS bytes for the
   outgoing arguments if stack clash protection is enabled.  This is essential
   as the extra arg space allows us to skip a check in alloca.  */
#undef STACK_DYNAMIC_OFFSET
#define STACK_DYNAMIC_OFFSET(FUNDECL)			   \
   ((flag_stack_clash_protection			   \
     && cfun->calls_alloca				   \
     && known_lt (crtl->outgoing_args_size,		   \
		  STACK_CLASH_MIN_BYTES_OUTGOING_ARGS))    \
    ? ROUND_UP (STACK_CLASH_MIN_BYTES_OUTGOING_ARGS,       \
		STACK_BOUNDARY / BITS_PER_UNIT)		   \
    : (crtl->outgoing_args_size + STACK_POINTER_OFFSET))

/* Filled in by aarch64_adjust_reg_alloc_order, which is called before
   the first relevant use.  */
#define REG_ALLOC_ORDER {}
#define ADJUST_REG_ALLOC_ORDER aarch64_adjust_reg_alloc_order ()

#define AARCH64_VALID_SHRN_OP(T,S)			\
((T) == TRUNCATE					\
 || ((T) == US_TRUNCATE && (S) == LSHIFTRT)		\
 || ((T) == SS_TRUNCATE && (S) == ASHIFTRT))

#ifndef USED_FOR_TARGET

/* Enumerates the mode-switching "entities" for AArch64.  */
enum class aarch64_mode_entity : int
{
  /* An aarch64_tristate_mode that says whether we have created a local
     save buffer for the current function's ZA state.  The only transition
     is from NO to YES.  */
  HAVE_ZA_SAVE_BUFFER,

  /* An aarch64_local_sme_state that reflects the state of all data
     controlled by PSTATE.ZA.  */
  LOCAL_SME_STATE
};

/* Describes the state of all data controlled by PSTATE.ZA  */
enum class aarch64_local_sme_state : int
{
  /* ZA is in the off or dormant state.  If it is dormant, the contents
     of ZA belong to a caller.  */
  INACTIVE_CALLER,

  /* ZA is in the off state: PSTATE.ZA is 0 and TPIDR2_EL0 is null.  */
  OFF,

  /* ZA is in the off or dormant state.  If it is dormant, the contents
     of ZA belong to the current function.  */
  INACTIVE_LOCAL,

  /* ZA is in the off state and the current function's ZA contents are
     stored in the lazy save buffer.  This is the state on entry to
     exception handlers.  */
  SAVED_LOCAL,

  /* ZA is in the active state: PSTATE.ZA is 1 and TPIDR2_EL0 is null.
     The contents of ZA are live.  */
  ACTIVE_LIVE,

  /* ZA is in the active state: PSTATE.ZA is 1 and TPIDR2_EL0 is null.
     The contents of ZA are dead.  */
  ACTIVE_DEAD,

  /* ZA could be in multiple states.  */
  ANY
};

enum class aarch64_tristate_mode : int { NO, YES, MAYBE };

#define OPTIMIZE_MODE_SWITCHING(ENTITY) \
  aarch64_optimize_mode_switching (aarch64_mode_entity (ENTITY))

#define NUM_MODES_FOR_MODE_SWITCHING \
  { int (aarch64_tristate_mode::MAYBE), \
    int (aarch64_local_sme_state::ANY) }

#endif

#endif /* GCC_AARCH64_H */
