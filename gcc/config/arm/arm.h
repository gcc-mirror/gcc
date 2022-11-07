/* Definitions of target machine for GNU compiler, for ARM.
   Copyright (C) 1991-2022 Free Software Foundation, Inc.
   Contributed by Pieter `Tiggr' Schoenmakers (rcpieter@win.tue.nl)
   and Martin Simmons (@harleqn.co.uk).
   More major hacks by Richard Earnshaw (rearnsha@arm.com)
   Minor hacks by Nick Clifton (nickc@cygnus.com)

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

#ifndef GCC_ARM_H
#define GCC_ARM_H

/* We can't use machine_mode inside a generator file because it
   hasn't been created yet; we shouldn't be using any code that
   needs the real definition though, so this ought to be safe.  */
#ifdef GENERATOR_FILE
#define MACHMODE int
#else
#include "insn-modes.h"
#define MACHMODE machine_mode
#endif

#include "config/vxworks-dummy.h"

/* The architecture define.  */
extern char arm_arch_name[];

/* Target CPU builtins.  */
#define TARGET_CPU_CPP_BUILTINS() arm_cpu_cpp_builtins (pfile)

#include "config/arm/arm-opts.h"

/* The processor for which instructions should be scheduled.  */
extern enum processor_type arm_tune;

typedef enum arm_cond_code
{
  ARM_EQ = 0, ARM_NE, ARM_CS, ARM_CC, ARM_MI, ARM_PL, ARM_VS, ARM_VC,
  ARM_HI, ARM_LS, ARM_GE, ARM_LT, ARM_GT, ARM_LE, ARM_AL, ARM_NV
}
arm_cc;

extern arm_cc arm_current_cc;

#define ARM_INVERSE_CONDITION_CODE(X)  ((arm_cc) (((int)X) ^ 1))

/* The maximum number of instructions that is beneficial to
   conditionally execute. */
#undef MAX_CONDITIONAL_EXECUTE
#define MAX_CONDITIONAL_EXECUTE arm_max_conditional_execute ()

extern int arm_target_label;
extern int arm_ccfsm_state;
extern GTY(()) rtx arm_target_insn;
/* Callback to output language specific object attributes.  */
extern void (*arm_lang_output_object_attributes_hook)(void);

/* This type is the user-visible __fp16.  We need it in a few places in
   the backend.  Defined in arm-builtins.cc.  */
extern tree arm_fp16_type_node;

/* This type is the user-visible __bf16.  We need it in a few places in
   the backend.  Defined in arm-builtins.cc.  */
extern tree arm_bf16_type_node;
extern tree arm_bf16_ptr_type_node;


#undef  CPP_SPEC
#define CPP_SPEC "%(subtarget_cpp_spec)"

#ifndef CC1_SPEC
#define CC1_SPEC ""
#endif

/* This macro defines names of additional specifications to put in the specs
   that can be used in various specifications like CC1_SPEC.  Its definition
   is an initializer with a subgrouping for each command option.

   Each subgrouping contains a string constant, that defines the
   specification name, and a string constant that used by the GCC driver
   program.

   Do not define this macro if it does not need to do anything.  */
#define EXTRA_SPECS						\
  { "subtarget_cpp_spec",	SUBTARGET_CPP_SPEC },           \
  { "asm_cpu_spec",		ASM_CPU_SPEC },			\
  SUBTARGET_EXTRA_SPECS

#ifndef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS
#endif

#ifndef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC      ""
#endif

/* Tree Target Specification.  */
#define TARGET_ARM_P(flags)    (!TARGET_THUMB_P (flags))
#define TARGET_THUMB1_P(flags) (TARGET_THUMB_P (flags) && !arm_arch_thumb2)
#define TARGET_THUMB2_P(flags) (TARGET_THUMB_P (flags) && arm_arch_thumb2)
#define TARGET_32BIT_P(flags)  (TARGET_ARM_P (flags) || TARGET_THUMB2_P (flags))

/* Run-time Target Specification.  */
/* Use hardware floating point instructions. -mgeneral-regs-only prevents
the use of floating point instructions and registers but does not prevent
emission of floating point pcs attributes.  */
#define TARGET_HARD_FLOAT_SUB	(arm_float_abi != ARM_FLOAT_ABI_SOFT	\
				 && bitmap_bit_p (arm_active_target.isa, \
						  isa_bit_vfpv2) \
				 && TARGET_32BIT)

#define TARGET_HARD_FLOAT	(TARGET_HARD_FLOAT_SUB		\
				 && !TARGET_GENERAL_REGS_ONLY)

#define TARGET_SOFT_FLOAT	(!TARGET_HARD_FLOAT_SUB)
/* User has permitted use of FP instructions, if they exist for this
   target.  */
#define TARGET_MAYBE_HARD_FLOAT (arm_float_abi != ARM_FLOAT_ABI_SOFT)
/* Use hardware floating point calling convention.  */
#define TARGET_HARD_FLOAT_ABI		(arm_float_abi == ARM_FLOAT_ABI_HARD)
#define TARGET_IWMMXT			(arm_arch_iwmmxt)
#define TARGET_IWMMXT2			(arm_arch_iwmmxt2)
#define TARGET_REALLY_IWMMXT		(TARGET_IWMMXT && TARGET_32BIT \
					 && !TARGET_GENERAL_REGS_ONLY)
#define TARGET_REALLY_IWMMXT2		(TARGET_IWMMXT2 && TARGET_32BIT \
					 && !TARGET_GENERAL_REGS_ONLY)
#define TARGET_IWMMXT_ABI (TARGET_32BIT && arm_abi == ARM_ABI_IWMMXT)
#define TARGET_ARM                      (! TARGET_THUMB)
#define TARGET_EITHER			1 /* (TARGET_ARM | TARGET_THUMB) */
#define TARGET_BACKTRACE	        (crtl->is_leaf \
				         ? TARGET_TPCS_LEAF_FRAME \
				         : TARGET_TPCS_FRAME)
#define TARGET_AAPCS_BASED \
    (arm_abi != ARM_ABI_APCS && arm_abi != ARM_ABI_ATPCS)

#define TARGET_HARD_TP			(target_thread_pointer == TP_CP15)
#define TARGET_SOFT_TP			(target_thread_pointer == TP_SOFT)
#define TARGET_GNU2_TLS			(target_tls_dialect == TLS_GNU2)

/* Only 16-bit thumb code.  */
#define TARGET_THUMB1			(TARGET_THUMB && !arm_arch_thumb2)
/* Arm or Thumb-2 32-bit code.  */
#define TARGET_32BIT			(TARGET_ARM || arm_arch_thumb2)
/* 32-bit Thumb-2 code.  */
#define TARGET_THUMB2			(TARGET_THUMB && arm_arch_thumb2)
/* Thumb-1 only.  */
#define TARGET_THUMB1_ONLY		(TARGET_THUMB1 && !arm_arch_notm)

#define TARGET_LDRD			(arm_arch5te && ARM_DOUBLEWORD_ALIGN \
                                         && !TARGET_THUMB1)

#define TARGET_CRC32			(arm_arch_crc)

/* Thumb-2 but also has some conditional arithmetic instructions like csinc,
   csinv, etc. */
#define TARGET_COND_ARITH		(arm_arch8_1m_main)

/* The following two macros concern the ability to execute coprocessor
   instructions for VFPv3 or NEON.  TARGET_VFP3/TARGET_VFPD32 are currently
   only ever tested when we know we are generating for VFP hardware; we need
   to be more careful with TARGET_NEON as noted below.  */

/* FPU is has the full VFPv3/NEON register file of 32 D registers.  */
#define TARGET_VFPD32 (bitmap_bit_p (arm_active_target.isa, isa_bit_fp_d32))

/* FPU supports VFPv3 instructions.  */
#define TARGET_VFP3 (bitmap_bit_p (arm_active_target.isa, isa_bit_vfpv3))

/* FPU supports FPv5 instructions.  */
#define TARGET_VFP5 (bitmap_bit_p (arm_active_target.isa, isa_bit_fpv5))

/* FPU only supports VFP single-precision instructions.  */
#define TARGET_VFP_SINGLE (!TARGET_VFP_DOUBLE)

/* FPU supports VFP double-precision instructions.  */
#define TARGET_VFP_DOUBLE (bitmap_bit_p (arm_active_target.isa, isa_bit_fp_dbl))

/* FPU supports half-precision floating-point with NEON element load/store.  */
#define TARGET_NEON_FP16					\
  (bitmap_bit_p (arm_active_target.isa, isa_bit_neon)		\
   && bitmap_bit_p (arm_active_target.isa, isa_bit_fp16conv))

/* FPU supports VFP half-precision floating-point conversions.  */
#define TARGET_FP16 (bitmap_bit_p (arm_active_target.isa, isa_bit_fp16conv))

/* FPU supports converting between HFmode and DFmode in a single hardware
   step.  */
#define TARGET_FP16_TO_DOUBLE						\
  (TARGET_HARD_FLOAT && TARGET_FP16 && TARGET_VFP5 && TARGET_VFP_DOUBLE)

/* FPU supports fused-multiply-add operations.  */
#define TARGET_FMA (bitmap_bit_p (arm_active_target.isa, isa_bit_vfpv4))

/* FPU supports Crypto extensions.  */
#define TARGET_CRYPTO (bitmap_bit_p (arm_active_target.isa, isa_bit_crypto))

/* FPU supports Neon instructions.  The setting of this macro gets
   revealed via __ARM_NEON__ so we add extra guards upon TARGET_32BIT
   and TARGET_HARD_FLOAT to ensure that NEON instructions are
   available.  */
#define TARGET_NEON							\
  (TARGET_32BIT && TARGET_HARD_FLOAT					\
   && bitmap_bit_p (arm_active_target.isa, isa_bit_neon))

/* FPU supports ARMv8.1 Adv.SIMD extensions.  */
#define TARGET_NEON_RDMA (TARGET_NEON && arm_arch8_1)

/* Supports the Dot Product AdvSIMD extensions.  */
#define TARGET_DOTPROD (TARGET_NEON && TARGET_VFP5			\
			&& bitmap_bit_p (arm_active_target.isa,		\
					isa_bit_dotprod)		\
			&& arm_arch8_2)

/* Supports the Armv8.3-a Complex number AdvSIMD extensions.  */
#define TARGET_COMPLEX (TARGET_NEON && arm_arch8_3)

/* FPU supports the floating point FP16 instructions for ARMv8.2-A
   and later.  */
#define TARGET_VFP_FP16INST \
  (TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP5 && arm_fp16_inst)

/* Target supports the floating point FP16 instructions from ARMv8.2-A
   and later.  */
#define TARGET_FP16FML (TARGET_NEON					\
			&& bitmap_bit_p (arm_active_target.isa,	\
					isa_bit_fp16fml)		\
			&& arm_arch8_2)

/* FPU supports the AdvSIMD FP16 instructions for ARMv8.2 and later.  */
#define TARGET_NEON_FP16INST (TARGET_VFP_FP16INST && TARGET_NEON_RDMA)

/* FPU supports 8-bit Integer Matrix Multiply (I8MM) AdvSIMD extensions.  */
#define TARGET_I8MM (TARGET_NEON && arm_arch8_2 && arm_arch_i8mm)

/* FPU supports Brain half-precision floating-point (BFloat16) extension.  */
#define TARGET_BF16_FP (TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP5 \
			&& arm_arch8_2 && arm_arch_bf16)
#define TARGET_BF16_SIMD (TARGET_NEON && TARGET_VFP5 \
			  && arm_arch8_2 && arm_arch_bf16)

/* Q-bit is present.  */
#define TARGET_ARM_QBIT \
  (TARGET_32BIT && arm_arch5te && (arm_arch_notm || arm_arch7))
/* Saturation operation, e.g. SSAT.  */
#define TARGET_ARM_SAT \
  (TARGET_32BIT && arm_arch6 && (arm_arch_notm || arm_arch7))
/* "DSP" multiply instructions, eg. SMULxy.  */
#define TARGET_DSP_MULTIPLY \
  (TARGET_32BIT && arm_arch5te && (arm_arch_notm || arm_arch7em))
/* Integer SIMD instructions, and extend-accumulate instructions.  */
#define TARGET_INT_SIMD \
  (TARGET_32BIT && arm_arch6 && (arm_arch_notm || arm_arch7em))

/* Should MOVW/MOVT be used in preference to a constant pool.  */
#define TARGET_USE_MOVT \
  (TARGET_HAVE_MOVT \
   && (arm_disable_literal_pool \
       || (!optimize_size && !current_tune->prefer_constant_pool)))

/* Nonzero if this chip provides the DMB instruction.  */
#define TARGET_HAVE_DMB		(arm_arch6m || arm_arch7)

/* Nonzero if this chip implements a memory barrier via CP15.  */
#define TARGET_HAVE_DMB_MCR	(arm_arch6 && ! TARGET_HAVE_DMB \
				 && ! TARGET_THUMB1)

/* Nonzero if this chip implements a memory barrier instruction.  */
#define TARGET_HAVE_MEMORY_BARRIER (TARGET_HAVE_DMB || TARGET_HAVE_DMB_MCR)

/* Nonzero if this chip supports ldrex and strex */
#define TARGET_HAVE_LDREX        ((arm_arch6 && TARGET_ARM)	\
				  || arm_arch7			\
				  || (arm_arch8 && !arm_arch_notm))

/* Nonzero if this chip supports LPAE.  */
#define TARGET_HAVE_LPAE (arm_arch_lpae)

/* Nonzero if this chip supports ldrex{bh} and strex{bh}.  */
#define TARGET_HAVE_LDREXBH ((arm_arch6k && TARGET_ARM)		\
			     || arm_arch7			\
			     || (arm_arch8 && !arm_arch_notm))

/* Nonzero if this chip supports ldrexd and strexd.  */
#define TARGET_HAVE_LDREXD (((arm_arch6k && TARGET_ARM) \
			     || arm_arch7) && arm_arch_notm)

/* Nonzero if this chip supports load-acquire and store-release.  */
#define TARGET_HAVE_LDACQ	(TARGET_ARM_ARCH >= 8)

/* Nonzero if this chip supports LDAEXD and STLEXD.  */
#define TARGET_HAVE_LDACQEXD	(TARGET_ARM_ARCH >= 8	\
				 && TARGET_32BIT	\
				 && arm_arch_notm)

/* Nonzero if this chip provides the MOVW and MOVT instructions.  */
#define TARGET_HAVE_MOVT	(arm_arch_thumb2 || arm_arch8)

/* Nonzero if this chip provides the CBZ and CBNZ instructions.  */
#define TARGET_HAVE_CBZ		(arm_arch_thumb2 || arm_arch8)

/* Nonzero if this chip provides Armv8.1-M Mainline Security extensions
   instructions (most are floating-point related).  */
#define TARGET_HAVE_FPCXT_CMSE	(arm_arch8_1m_main)

#define TARGET_HAVE_MVE (arm_float_abi != ARM_FLOAT_ABI_SOFT \
			 && bitmap_bit_p (arm_active_target.isa, \
					  isa_bit_mve) \
			 && !TARGET_GENERAL_REGS_ONLY)

#define TARGET_HAVE_MVE_FLOAT (arm_float_abi != ARM_FLOAT_ABI_SOFT \
			       && bitmap_bit_p (arm_active_target.isa, \
						isa_bit_mve_float) \
			       && !TARGET_GENERAL_REGS_ONLY)

/* MVE have few common instructions as VFP, like VLDM alias VPOP, VLDR, VSTM
   alia VPUSH, VSTR and VMOV, VMSR and VMRS.  In the same manner it updates few
   registers such as FPCAR, FPCCR, FPDSCR, FPSCR, MVFR0, MVFR1 and MVFR2.  All
   the VFP instructions, RTL patterns and register are guarded by
   TARGET_HARD_FLOAT.  But the common instructions, RTL pattern and registers
   between MVE and VFP will be guarded by the following macro TARGET_VFP_BASE
   hereafter.  */

#define TARGET_VFP_BASE (arm_float_abi != ARM_FLOAT_ABI_SOFT \
			 && bitmap_bit_p (arm_active_target.isa, \
					  isa_bit_vfp_base) \
			 && !TARGET_GENERAL_REGS_ONLY)

/* Nonzero if integer division instructions supported.  */
#define TARGET_IDIV	((TARGET_ARM && arm_arch_arm_hwdiv)	\
			 || (TARGET_THUMB && arm_arch_thumb_hwdiv))

/* Nonzero if disallow volatile memory access in IT block.  */
#define TARGET_NO_VOLATILE_CE		(arm_arch_no_volatile_ce)

/* Nonzero if chip supports the Custom Datapath Extension.  */
#define TARGET_CDE	(arm_arch_cde && arm_arch8 && !arm_arch_notm)

/* Should constant I be slplit for OP.  */
#define DONT_EARLY_SPLIT_CONSTANT(i, op) \
				((optimize >= 2) \
				 && can_create_pseudo_p () \
				 && !const_ok_for_op (i, op))

/* True iff the full BPABI is being used.  If TARGET_BPABI is true,
   then TARGET_AAPCS_BASED must be true -- but the converse does not
   hold.  TARGET_BPABI implies the use of the BPABI runtime library,
   etc., in addition to just the AAPCS calling conventions.  */
#ifndef TARGET_BPABI
#define TARGET_BPABI false
#endif

/* Transform lane numbers on big endian targets. This is used to allow for the
   endianness difference between NEON architectural lane numbers and those
   used in RTL */
#define NEON_ENDIAN_LANE_N(mode, n)  \
  (BYTES_BIG_ENDIAN ? GET_MODE_NUNITS (mode) - 1 - n : n)

/* Support for a compile-time default CPU, et cetera.  The rules are:
   --with-arch is ignored if -march or -mcpu are specified.
   --with-cpu is ignored if -march or -mcpu are specified, and is overridden
    by --with-arch.
   --with-tune is ignored if -mtune or -mcpu are specified (but not affected
     by -march).
   --with-float is ignored if -mfloat-abi is specified.
   --with-fpu is ignored if -mfpu is specified.
   --with-abi is ignored if -mabi is specified.
   --with-tls is ignored if -mtls-dialect is specified.
   Note: --with-mode is not handled here, that has a special rule
   TARGET_MODE_CHECK that also takes into account the selected CPU and
   architecture.  */
#define OPTION_DEFAULT_SPECS \
  {"arch", "%{!march=*:%{!mcpu=*:-march=%(VALUE)}}" }, \
  {"cpu", "%{!march=*:%{!mcpu=*:-mcpu=%(VALUE)}}" }, \
  {"tune", "%{!mcpu=*:%{!mtune=*:-mtune=%(VALUE)}}" }, \
  {"float", "%{!mfloat-abi=*:-mfloat-abi=%(VALUE)}" }, \
  {"fpu", "%{!mfpu=*:-mfpu=%(VALUE)}"}, \
  {"abi", "%{!mabi=*:-mabi=%(VALUE)}"}, \
  {"tls", "%{!mtls-dialect=*:-mtls-dialect=%(VALUE)}"},

extern const struct arm_fpu_desc
{
  const char *name;
  enum isa_feature isa_bits[isa_num_bits];
} all_fpus[];

/* Which floating point hardware to schedule for.  */
extern int arm_fpu_attr;

#ifndef TARGET_DEFAULT_FLOAT_ABI
#define TARGET_DEFAULT_FLOAT_ABI ARM_FLOAT_ABI_SOFT
#endif

#ifndef ARM_DEFAULT_ABI
#define ARM_DEFAULT_ABI ARM_ABI_APCS
#endif

/* AAPCS based ABIs use short enums by default.  */
#ifndef ARM_DEFAULT_SHORT_ENUMS
#define ARM_DEFAULT_SHORT_ENUMS \
  (TARGET_AAPCS_BASED && arm_abi != ARM_ABI_AAPCS_LINUX)
#endif

/* Map each of the micro-architecture variants to their corresponding
   major architecture revision.  */

enum base_architecture
{
  BASE_ARCH_0 = 0,
  BASE_ARCH_2 = 2,
  BASE_ARCH_3 = 3,
  BASE_ARCH_3M = 3,
  BASE_ARCH_4 = 4,
  BASE_ARCH_4T = 4,
  BASE_ARCH_5T = 5,
  BASE_ARCH_5TE = 5,
  BASE_ARCH_5TEJ = 5,
  BASE_ARCH_6 = 6,
  BASE_ARCH_6J = 6,
  BASE_ARCH_6KZ = 6,
  BASE_ARCH_6K = 6,
  BASE_ARCH_6T2 = 6,
  BASE_ARCH_6M = 6,
  BASE_ARCH_6Z = 6,
  BASE_ARCH_7 = 7,
  BASE_ARCH_7A = 7,
  BASE_ARCH_7R = 7,
  BASE_ARCH_7M = 7,
  BASE_ARCH_7EM = 7,
  BASE_ARCH_8A = 8,
  BASE_ARCH_8M_BASE = 8,
  BASE_ARCH_8M_MAIN = 8,
  BASE_ARCH_8R = 8,
  BASE_ARCH_9A = 9
};

/* The major revision number of the ARM Architecture implemented by the target.  */
extern enum base_architecture arm_base_arch;

/* Nonzero if this chip supports the ARM Architecture 4 extensions.  */
extern int arm_arch4;

/* Nonzero if this chip supports the ARM Architecture 4T extensions.  */
extern int arm_arch4t;

/* Nonzero if this chip supports the ARM Architecture 5T extensions.  */
extern int arm_arch5t;

/* Nonzero if this chip supports the ARM Architecture 5TE extensions.  */
extern int arm_arch5te;

/* Nonzero if this chip supports the ARM Architecture 6 extensions.  */
extern int arm_arch6;

/* Nonzero if this chip supports the ARM Architecture 6k extensions.  */
extern int arm_arch6k;

/* Nonzero if instructions present in ARMv6-M can be used.  */
extern int arm_arch6m;

/* Nonzero if this chip supports the ARM Architecture 7 extensions.  */
extern int arm_arch7;

/* Nonzero if instructions not present in the 'M' profile can be used.  */
extern int arm_arch_notm;

/* Nonzero if instructions present in ARMv7E-M can be used.  */
extern int arm_arch7em;

/* Nonzero if this chip supports the ARM Architecture 8 extensions.  */
extern int arm_arch8;

/* Nonzero if this chip supports the ARM Architecture 8.1 extensions.  */
extern int arm_arch8_1;

/* Nonzero if this chip supports the ARM Architecture 8.2 extensions.  */
extern int arm_arch8_2;

/* Nonzero if this chip supports the ARM Architecture 8.3 extensions.  */
extern int arm_arch8_3;

/* Nonzero if this chip supports the ARM Architecture 8.4 extensions.  */
extern int arm_arch8_4;

/* Nonzero if this chip supports the ARM Architecture 8.1-M Mainline
   extensions.  */
extern int arm_arch8_1m_main;

/* Nonzero if this chip supports the FP16 instructions extension of ARM
   Architecture 8.2.  */
extern int arm_fp16_inst;

/* Nonzero if this chip can benefit from load scheduling.  */
extern int arm_ld_sched;

/* Nonzero if this chip is a StrongARM.  */
extern int arm_tune_strongarm;

/* Nonzero if this chip supports Intel XScale with Wireless MMX technology.  */
extern int arm_arch_iwmmxt;

/* Nonzero if this chip supports Intel Wireless MMX2 technology.  */
extern int arm_arch_iwmmxt2;

/* Nonzero if this chip is an XScale.  */
extern int arm_arch_xscale;

/* Nonzero if tuning for XScale.  */
extern int arm_tune_xscale;

/* Nonzero if tuning for stores via the write buffer.  */
extern int arm_tune_wbuf;

/* Nonzero if tuning for Cortex-A9.  */
extern int arm_tune_cortex_a9;

/* Nonzero if we should define __THUMB_INTERWORK__ in the
   preprocessor.
   XXX This is a bit of a hack, it's intended to help work around
   problems in GLD which doesn't understand that armv5t code is
   interworking clean.  */
extern int arm_cpp_interwork;

/* Nonzero if chip supports Thumb 1.  */
extern int arm_arch_thumb1;

/* Nonzero if chip supports Thumb 2.  */
extern int arm_arch_thumb2;

/* Nonzero if chip supports integer division instruction in ARM mode.  */
extern int arm_arch_arm_hwdiv;

/* Nonzero if chip supports integer division instruction in Thumb mode.  */
extern int arm_arch_thumb_hwdiv;

/* Nonzero if chip disallows volatile memory access in IT block.  */
extern int arm_arch_no_volatile_ce;

/* Nonzero if we shouldn't use literal pools.  */
#ifndef USED_FOR_TARGET
extern bool arm_disable_literal_pool;
#endif

/* Nonzero if chip supports the ARMv8 CRC instructions.  */
extern int arm_arch_crc;

/* Nonzero if chip supports the ARMv8-M Security Extensions.  */
extern int arm_arch_cmse;

/* Nonzero if chip supports the I8MM instructions.  */
extern int arm_arch_i8mm;

/* Nonzero if chip supports the BFloat16 instructions.  */
extern int arm_arch_bf16;

/* Nonzero if chip supports the Custom Datapath Extension.  */
extern int arm_arch_cde;
extern int arm_arch_cde_coproc;
extern const int arm_arch_cde_coproc_bits[];
#define ARM_CDE_CONST_COPROC	7
#define ARM_CCDE_CONST_1	((1 << 13) - 1)
#define ARM_CCDE_CONST_2	((1 << 9 ) - 1)
#define ARM_CCDE_CONST_3	((1 << 6 ) - 1)
#define ARM_VCDE_CONST_1	((1 << 11) - 1)
#define ARM_VCDE_CONST_2	((1 << 6 ) - 1)
#define ARM_VCDE_CONST_3	((1 << 3 ) - 1)
#define ARM_MVE_CDE_CONST_1	((1 << 12) - 1)
#define ARM_MVE_CDE_CONST_2	((1 << 7 ) - 1)
#define ARM_MVE_CDE_CONST_3	((1 << 4 ) - 1)

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT  (MASK_APCS_FRAME)
#endif

/* Nonzero if PIC code requires explicit qualifiers to generate
   PLT and GOT relocs rather than the assembler doing so implicitly.
   Subtargets can override these if required.  */
#ifndef NEED_GOT_RELOC
#define NEED_GOT_RELOC	0
#endif
#ifndef NEED_PLT_RELOC
#define NEED_PLT_RELOC	0
#endif

#ifndef TARGET_DEFAULT_PIC_DATA_IS_TEXT_RELATIVE
#define TARGET_DEFAULT_PIC_DATA_IS_TEXT_RELATIVE 1
#endif

/* Nonzero if we need to refer to the GOT with a PC-relative
   offset.  In other words, generate

   .word	_GLOBAL_OFFSET_TABLE_ - [. - (.Lxx + 8)]

   rather than

   .word	_GLOBAL_OFFSET_TABLE_ - (.Lxx + 8)

   The default is true, which matches NetBSD.  Subtargets can
   override this if required.  */
#ifndef GOT_PCREL
#define GOT_PCREL   1
#endif

/* Target machine storage Layout.  */

/* Nonzero if this chip provides Armv8.1-M Mainline
   LOB (low overhead branch features) extension instructions.  */
#define TARGET_HAVE_LOB (arm_arch8_1m_main)

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases,
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.  */

#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)	\
  if (GET_MODE_CLASS (MODE) == MODE_INT		\
      && GET_MODE_SIZE (MODE) < 4)      	\
    {						\
      (MODE) = SImode;				\
    }

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */
#define BITS_BIG_ENDIAN  0

/* Define this if most significant byte of a word is the lowest numbered.
   Most ARM processors are run in little endian mode, so that is the default.
   If you want to have it run-time selectable, change the definition in a
   cover file to be TARGET_BIG_ENDIAN.  */
#define BYTES_BIG_ENDIAN  (TARGET_BIG_END != 0)

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
#define WORDS_BIG_ENDIAN  (BYTES_BIG_ENDIAN)

#define UNITS_PER_WORD	4

/* True if natural alignment is used for doubleword types.  */
#define ARM_DOUBLEWORD_ALIGN	TARGET_AAPCS_BASED

#define DOUBLEWORD_ALIGNMENT 64

#define PARM_BOUNDARY  	32

#define STACK_BOUNDARY  (ARM_DOUBLEWORD_ALIGN ? DOUBLEWORD_ALIGNMENT : 32)

#define PREFERRED_STACK_BOUNDARY \
    (arm_abi == ARM_ABI_ATPCS ? 64 : STACK_BOUNDARY)

#define FUNCTION_BOUNDARY_P(flags)  (TARGET_THUMB_P (flags) ? 16 : 32)
#define FUNCTION_BOUNDARY           (FUNCTION_BOUNDARY_P (target_flags))

/* The lowest bit is used to indicate Thumb-mode functions, so the
   vbit must go into the delta field of pointers to member
   functions.  */
#define TARGET_PTRMEMFUNC_VBIT_LOCATION ptrmemfunc_vbit_in_delta

#define EMPTY_FIELD_BOUNDARY  32

#define BIGGEST_ALIGNMENT (ARM_DOUBLEWORD_ALIGN ? DOUBLEWORD_ALIGNMENT : 32)

#define MALLOC_ABI_ALIGNMENT  BIGGEST_ALIGNMENT

/* XXX Blah -- this macro is used directly by libobjc.  Since it
   supports no vector modes, cut out the complexity and fall back
   on BIGGEST_FIELD_ALIGNMENT.  */
#ifdef IN_TARGET_LIBS
#define BIGGEST_FIELD_ALIGNMENT 64
#endif

/* Align definitions of arrays, unions and structures so that
   initializations and copies can be made more efficient.  This is not
   ABI-changing, so it only affects places where we can see the
   definition. Increasing the alignment tends to introduce padding,
   so don't do this when optimizing for size/conserving stack space. */
#define ARM_EXPAND_ALIGNMENT(COND, EXP, ALIGN)				\
  (((COND) && ((ALIGN) < BITS_PER_WORD)					\
    && (TREE_CODE (EXP) == ARRAY_TYPE					\
	|| TREE_CODE (EXP) == UNION_TYPE				\
	|| TREE_CODE (EXP) == RECORD_TYPE)) ? BITS_PER_WORD : (ALIGN))

/* Align global data. */
#define DATA_ALIGNMENT(EXP, ALIGN)			\
  ARM_EXPAND_ALIGNMENT(!optimize_size, EXP, ALIGN)

/* Similarly, make sure that objects on the stack are sensibly aligned.  */
#define LOCAL_ALIGNMENT(EXP, ALIGN)				\
  ARM_EXPAND_ALIGNMENT(!flag_conserve_stack, EXP, ALIGN)

/* Setting STRUCTURE_SIZE_BOUNDARY to 32 produces more efficient code, but the
   value set in previous versions of this toolchain was 8, which produces more
   compact structures.  The command line option -mstructure_size_boundary=<n>
   can be used to change this value.  For compatibility with the ARM SDK
   however the value should be left at 32.  ARM SDT Reference Manual (ARM DUI
   0020D) page 2-20 says "Structures are aligned on word boundaries".
   The AAPCS specifies a value of 8.  */
#define STRUCTURE_SIZE_BOUNDARY arm_structure_size_boundary

/* This is the value used to initialize arm_structure_size_boundary.  If a
   particular arm target wants to change the default value it should change
   the definition of this macro, not STRUCTURE_SIZE_BOUNDARY.  See netbsd.h
   for an example of this.  */
#ifndef DEFAULT_STRUCTURE_SIZE_BOUNDARY
#define DEFAULT_STRUCTURE_SIZE_BOUNDARY 32
#endif

/* Nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* wchar_t is unsigned under the AAPCS.  */
#ifndef WCHAR_TYPE
#define WCHAR_TYPE (TARGET_AAPCS_BASED ? "unsigned int" : "int")

#define WCHAR_TYPE_SIZE BITS_PER_WORD
#endif

/* Sized for fixed-point types.  */

#define SHORT_FRACT_TYPE_SIZE 8
#define FRACT_TYPE_SIZE 16
#define LONG_FRACT_TYPE_SIZE 32
#define LONG_LONG_FRACT_TYPE_SIZE 64

#define SHORT_ACCUM_TYPE_SIZE 16
#define ACCUM_TYPE_SIZE 32
#define LONG_ACCUM_TYPE_SIZE 64
#define LONG_LONG_ACCUM_TYPE_SIZE 64

#define MAX_FIXED_MODE_SIZE 64

#ifndef SIZE_TYPE
#define SIZE_TYPE (TARGET_AAPCS_BASED ? "unsigned int" : "long unsigned int")
#endif

#ifndef PTRDIFF_TYPE
#define PTRDIFF_TYPE (TARGET_AAPCS_BASED ? "int" : "long int")
#endif

/* AAPCS requires that structure alignment is affected by bitfields.  */
#ifndef PCC_BITFIELD_TYPE_MATTERS
#define PCC_BITFIELD_TYPE_MATTERS TARGET_AAPCS_BASED
#endif

/* The maximum size of the sync library functions supported.  */
#ifndef MAX_SYNC_LIBFUNC_SIZE
#define MAX_SYNC_LIBFUNC_SIZE (2 * UNITS_PER_WORD)
#endif


/* Standard register usage.  */

/* Register allocation in ARM Procedure Call Standard
   (S - saved over call, F - Frame-related).

	r0	   *	argument word/integer result
	r1-r3		argument word

	r4-r8	     S	register variable
	r9	     S	(rfp) register variable (real frame pointer)

	r10  	   F S	(sl) stack limit (used by -mapcs-stack-check)
	r11 	   F S	(fp) argument pointer
	r12		(ip) temp workspace
	r13  	   F S	(sp) lower end of current stack frame
	r14		(lr) link address/workspace
	r15	   F	(pc) program counter

	cc		This is NOT a real register, but is used internally
	                to represent things that use or set the condition
			codes.
	sfp             This isn't either.  It is used during rtl generation
	                since the offset between the frame pointer and the
			auto's isn't known until after register allocation.
	afp		Nor this, we only need this because of non-local
	                goto.  Without it fp appears to be used and the
			elimination code won't get rid of sfp.  It tracks
			fp exactly at all times.
	apsrq		Nor this, it is used to track operations on the Q bit
			of APSR by ACLE saturating intrinsics.
	apsrge		Nor this, it is used to track operations on the GE bits
			of APSR by ACLE SIMD32 intrinsics

   *: See TARGET_CONDITIONAL_REGISTER_USAGE  */

/*	s0-s15		VFP scratch (aka d0-d7).
	s16-s31	      S	VFP variable (aka d8-d15).
	vfpcc		Not a real register.  Represents the VFP condition
			code flags.
	vpr		Used to represent MVE VPR predication.  */

/* The stack backtrace structure is as follows:
  fp points to here:  |  save code pointer  |      [fp]
                      |  return link value  |      [fp, #-4]
                      |  return sp value    |      [fp, #-8]
                      |  return fp value    |      [fp, #-12]
                     [|  saved r10 value    |]
                     [|  saved r9 value     |]
                     [|  saved r8 value     |]
                     [|  saved r7 value     |]
                     [|  saved r6 value     |]
                     [|  saved r5 value     |]
                     [|  saved r4 value     |]
                     [|  saved r3 value     |]
                     [|  saved r2 value     |]
                     [|  saved r1 value     |]
                     [|  saved r0 value     |]
  r0-r3 are not normally saved in a C function.  */

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.  */
#define FIXED_REGISTERS 	\
{				\
  /* Core regs.  */		\
  0,0,0,0,0,0,0,0,		\
  0,0,0,0,0,1,0,1,		\
  /* VFP regs.  */		\
  1,1,1,1,1,1,1,1,		\
  1,1,1,1,1,1,1,1,		\
  1,1,1,1,1,1,1,1,		\
  1,1,1,1,1,1,1,1,		\
  1,1,1,1,1,1,1,1,		\
  1,1,1,1,1,1,1,1,		\
  1,1,1,1,1,1,1,1,		\
  1,1,1,1,1,1,1,1,		\
  /* IWMMXT regs.  */		\
  1,1,1,1,1,1,1,1,		\
  1,1,1,1,1,1,1,1,		\
  1,1,1,1,			\
  /* Specials.  */		\
  1,1,1,1,1,1,1			\
}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.
   The CC is not preserved over function calls on the ARM 6, so it is
   easier to assume this for all.  SFP is preserved, since FP is.  */
#define CALL_USED_REGISTERS	\
{				\
  /* Core regs.  */		\
  1,1,1,1,0,0,0,0,		\
  0,0,0,0,1,1,1,1,		\
  /* VFP Regs.  */		\
  1,1,1,1,1,1,1,1,		\
  1,1,1,1,1,1,1,1,		\
  1,1,1,1,1,1,1,1,		\
  1,1,1,1,1,1,1,1,		\
  1,1,1,1,1,1,1,1,		\
  1,1,1,1,1,1,1,1,		\
  1,1,1,1,1,1,1,1,		\
  1,1,1,1,1,1,1,1,		\
  /* IWMMXT regs.  */		\
  1,1,1,1,1,1,1,1,		\
  1,1,1,1,1,1,1,1,		\
  1,1,1,1,			\
  /* Specials.  */		\
  1,1,1,1,1,1,1			\
}

#ifndef SUBTARGET_CONDITIONAL_REGISTER_USAGE
#define SUBTARGET_CONDITIONAL_REGISTER_USAGE
#endif

/* These are a couple of extensions to the formats accepted
   by asm_fprintf:
     %@ prints out ASM_COMMENT_START
     %r prints out REGISTER_PREFIX reg_names[arg]  */
#define ASM_FPRINTF_EXTENSIONS(FILE, ARGS, P)		\
  case '@':						\
    fputs (ASM_COMMENT_START, FILE);			\
    break;						\
							\
  case 'r':						\
    fputs (REGISTER_PREFIX, FILE);			\
    fputs (reg_names [va_arg (ARGS, int)], FILE);	\
    break;

/* Round X up to the nearest word.  */
#define ROUND_UP_WORD(X) (((X) + 3) & ~3)

/* Convert fron bytes to ints.  */
#define ARM_NUM_INTS(X) (((X) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* The number of (integer) registers required to hold a quantity of type MODE.
   Also used for VFP registers.  */
#define ARM_NUM_REGS(MODE)				\
  ARM_NUM_INTS (GET_MODE_SIZE (MODE))

/* The number of (integer) registers required to hold a quantity of TYPE MODE.  */
#define ARM_NUM_REGS2(MODE, TYPE)                   \
  ARM_NUM_INTS ((MODE) == BLKmode ? 		\
  int_size_in_bytes (TYPE) : GET_MODE_SIZE (MODE))

/* The number of (integer) argument register available.  */
#define NUM_ARG_REGS		4

/* And similarly for the VFP.  */
#define NUM_VFP_ARG_REGS	16

/* Return the register number of the N'th (integer) argument.  */
#define ARG_REGISTER(N) 	(N - 1)

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* The number of the last argument register.  */
#define LAST_ARG_REGNUM 	ARG_REGISTER (NUM_ARG_REGS)

/* The numbers of the Thumb register ranges.  */
#define FIRST_LO_REGNUM  	0
#define LAST_LO_REGNUM  	7
#define FIRST_HI_REGNUM		8
#define LAST_HI_REGNUM		11

/* Overridden by config/arm/bpabi.h.  */
#ifndef ARM_UNWIND_INFO
#define ARM_UNWIND_INFO  0
#endif

/* Use r0 and r1 to pass exception handling information.  */
#define EH_RETURN_DATA_REGNO(N) (((N) < 2) ? N : INVALID_REGNUM)

/* The register that holds the return address in exception handlers.  */
#define ARM_EH_STACKADJ_REGNUM	2
#define EH_RETURN_STACKADJ_RTX	gen_rtx_REG (SImode, ARM_EH_STACKADJ_REGNUM)

#ifndef ARM_TARGET2_DWARF_FORMAT
#define ARM_TARGET2_DWARF_FORMAT DW_EH_PE_pcrel
#endif

/* ttype entries (the only interesting data references used)
   use TARGET2 relocations.  */
#define ASM_PREFERRED_EH_DATA_FORMAT(code, data) \
  (((code) == 0 && (data) == 1 && ARM_UNWIND_INFO) ? ARM_TARGET2_DWARF_FORMAT \
			       : DW_EH_PE_absptr)

/* The native (Norcroft) Pascal compiler for the ARM passes the static chain
   as an invisible last argument (possible since varargs don't exist in
   Pascal), so the following is not true.  */
#define STATIC_CHAIN_REGNUM	12

/* r9 is the FDPIC register (base register for GOT and FUNCDESC accesses).  */
#define FDPIC_REGNUM		9

/* Define this to be where the real frame pointer is if it is not possible to
   work out the offset between the frame pointer and the automatic variables
   until after register allocation has taken place.  FRAME_POINTER_REGNUM
   should point to a special register that we will make sure is eliminated.

   For the Thumb we have another problem.  The TPCS defines the frame pointer
   as r11, and GCC believes that it is always possible to use the frame pointer
   as base register for addressing purposes.  (See comments in
   find_reloads_address()).  But - the Thumb does not allow high registers,
   including r11, to be used as base address registers.  Hence our problem.

   The solution used here, and in the old thumb port is to use r7 instead of
   r11 as the hard frame pointer and to have special code to generate
   backtrace structures on the stack (if required to do so via a command line
   option) using r11.  This is the only 'user visible' use of r11 as a frame
   pointer.  */
#define ARM_HARD_FRAME_POINTER_REGNUM	11
#define THUMB_HARD_FRAME_POINTER_REGNUM	 7

#define HARD_FRAME_POINTER_REGNUM		\
  (TARGET_ARM					\
   ? ARM_HARD_FRAME_POINTER_REGNUM		\
   : THUMB_HARD_FRAME_POINTER_REGNUM)

#define HARD_FRAME_POINTER_IS_FRAME_POINTER 0
#define HARD_FRAME_POINTER_IS_ARG_POINTER 0

#define FP_REGNUM	                HARD_FRAME_POINTER_REGNUM

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM	SP_REGNUM

#define FIRST_IWMMXT_REGNUM	(LAST_HI_VFP_REGNUM + 1)
#define LAST_IWMMXT_REGNUM	(FIRST_IWMMXT_REGNUM + 15)

/* Need to sync with WCGR in iwmmxt.md.  */
#define FIRST_IWMMXT_GR_REGNUM	(LAST_IWMMXT_REGNUM + 1)
#define LAST_IWMMXT_GR_REGNUM	(FIRST_IWMMXT_GR_REGNUM + 3)

#define IS_IWMMXT_REGNUM(REGNUM) \
  (((REGNUM) >= FIRST_IWMMXT_REGNUM) && ((REGNUM) <= LAST_IWMMXT_REGNUM))
#define IS_IWMMXT_GR_REGNUM(REGNUM) \
  (((REGNUM) >= FIRST_IWMMXT_GR_REGNUM) && ((REGNUM) <= LAST_IWMMXT_GR_REGNUM))

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM	102

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM	103

#define FIRST_VFP_REGNUM	16
#define D7_VFP_REGNUM		(FIRST_VFP_REGNUM + 15)
#define LAST_VFP_REGNUM	\
  (TARGET_VFPD32 ? LAST_HI_VFP_REGNUM : LAST_LO_VFP_REGNUM)

#define IS_VFP_REGNUM(REGNUM) \
  (((REGNUM) >= FIRST_VFP_REGNUM) && ((REGNUM) <= LAST_VFP_REGNUM))

/* VFP registers are split into two types: those defined by VFP versions < 3
   have D registers overlaid on consecutive pairs of S registers. VFP version 3
   defines 16 new D registers (d16-d31) which, for simplicity and correctness
   in various parts of the backend, we implement as "fake" single-precision
   registers (which would be S32-S63, but cannot be used in that way).  The
   following macros define these ranges of registers.  */
#define LAST_LO_VFP_REGNUM	(FIRST_VFP_REGNUM + 31)
#define FIRST_HI_VFP_REGNUM	(LAST_LO_VFP_REGNUM + 1)
#define LAST_HI_VFP_REGNUM	(FIRST_HI_VFP_REGNUM + 31)

#define VFP_REGNO_OK_FOR_SINGLE(REGNUM) \
  ((REGNUM) <= LAST_LO_VFP_REGNUM)

/* DFmode values are only valid in even register pairs.  */
#define VFP_REGNO_OK_FOR_DOUBLE(REGNUM) \
  ((((REGNUM) - FIRST_VFP_REGNUM) & 1) == 0)

/* Neon Quad values must start at a multiple of four registers.  */
#define NEON_REGNO_OK_FOR_QUAD(REGNUM) \
  ((((REGNUM) - FIRST_VFP_REGNUM) & 3) == 0)

/* Neon structures of vectors must be in even register pairs and there
   must be enough registers available.  Because of various patterns
   requiring quad registers, we require them to start at a multiple of
   four.  */
#define NEON_REGNO_OK_FOR_NREGS(REGNUM, N) \
  ((((REGNUM) - FIRST_VFP_REGNUM) & 3) == 0 \
   && (LAST_VFP_REGNUM - (REGNUM) >= 2 * (N) - 1))

/* The number of hard registers is 16 ARM + 1 CC + 1 SFP + 1 AFP
   + 1 APSRQ + 1 APSRGE + 1 VPR.  */
/* Intel Wireless MMX Technology registers add 16 + 4 more.  */
/* VFP (VFP3) adds 32 (64) + 1 VFPCC.  */
#define FIRST_PSEUDO_REGISTER   107

#define DEBUGGER_REGNO(REGNO) arm_debugger_regno (REGNO)

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms may be accessed
   via the stack pointer) in functions that seem suitable.
   If we have to have a frame pointer we might as well make use of it.
   APCS says that the frame pointer does not need to be pushed in leaf
   functions, or simple tail call functions.  */

#ifndef SUBTARGET_FRAME_POINTER_REQUIRED
#define SUBTARGET_FRAME_POINTER_REQUIRED 0
#endif

#define VALID_IWMMXT_REG_MODE(MODE) \
 (arm_vector_mode_supported_p (MODE) || (MODE) == DImode)

/* Modes valid for Neon D registers.  */
#define VALID_NEON_DREG_MODE(MODE) \
  ((MODE) == V2SImode || (MODE) == V4HImode || (MODE) == V8QImode \
   || (MODE) == V4HFmode || (MODE) == V2SFmode || (MODE) == DImode \
   || (MODE) == V4BFmode)

/* Modes valid for Neon Q registers.  */
#define VALID_NEON_QREG_MODE(MODE) \
  ((MODE) == V4SImode || (MODE) == V8HImode || (MODE) == V16QImode \
   || (MODE) == V8HFmode || (MODE) == V4SFmode || (MODE) == V2DImode \
   || (MODE) == V8BFmode)

#define VALID_MVE_MODE(MODE) \
  ((MODE) == V2DImode ||(MODE) == V4SImode || (MODE) == V8HImode \
   || (MODE) == V16QImode || (MODE) == V8HFmode || (MODE) == V4SFmode \
   || (MODE) == V2DFmode)

#define VALID_MVE_SI_MODE(MODE) \
  ((MODE) == V2DImode ||(MODE) == V4SImode || (MODE) == V8HImode \
   || (MODE) == V16QImode)

/* Modes used in MVE's narrowing stores or widening loads.  */
#define MVE_STN_LDW_MODE(MODE) \
  ((MODE) == V4QImode || (MODE) == V8QImode || (MODE) == V4HImode)

#define VALID_MVE_SF_MODE(MODE) \
  ((MODE) == V8HFmode || (MODE) == V4SFmode || (MODE) == V2DFmode)

/* Structure modes valid for Neon registers.  */
#define VALID_NEON_STRUCT_MODE(MODE) \
  ((MODE) == TImode || (MODE) == EImode || (MODE) == OImode \
   || (MODE) == CImode || (MODE) == XImode)

#define VALID_MVE_STRUCT_MODE(MODE) \
  ((MODE) == TImode || (MODE) == OImode || (MODE) == XImode)

/* The conditions under which vector modes are supported for general
   arithmetic using Neon.  */

#define ARM_HAVE_NEON_V8QI_ARITH TARGET_NEON
#define ARM_HAVE_NEON_V4HI_ARITH TARGET_NEON
#define ARM_HAVE_NEON_V2SI_ARITH TARGET_NEON

#define ARM_HAVE_NEON_V16QI_ARITH TARGET_NEON
#define ARM_HAVE_NEON_V8HI_ARITH TARGET_NEON
#define ARM_HAVE_NEON_V4SI_ARITH TARGET_NEON
#define ARM_HAVE_NEON_V2DI_ARITH TARGET_NEON

/* HF operations have their own flush-to-zero control (FPSCR.FZ16).  */
#define ARM_HAVE_NEON_V4HF_ARITH TARGET_NEON_FP16INST
#define ARM_HAVE_NEON_V8HF_ARITH TARGET_NEON_FP16INST

/* SF operations always flush to zero, regardless of FPSCR.FZ, so we can
   only use them for general arithmetic when -funsafe-math-optimizations
   is in effect.  */
#define ARM_HAVE_NEON_V2SF_ARITH \
  (TARGET_NEON && flag_unsafe_math_optimizations)
#define ARM_HAVE_NEON_V4SF_ARITH ARM_HAVE_NEON_V2SF_ARITH

/* The conditions under which vector modes are supported for general
   arithmetic by any vector extension.  */

#define ARM_HAVE_V8QI_ARITH (ARM_HAVE_NEON_V8QI_ARITH || TARGET_REALLY_IWMMXT)
#define ARM_HAVE_V4HI_ARITH (ARM_HAVE_NEON_V4HI_ARITH || TARGET_REALLY_IWMMXT)
#define ARM_HAVE_V2SI_ARITH (ARM_HAVE_NEON_V2SI_ARITH || TARGET_REALLY_IWMMXT)

#define ARM_HAVE_V16QI_ARITH (ARM_HAVE_NEON_V16QI_ARITH || TARGET_HAVE_MVE)
#define ARM_HAVE_V8HI_ARITH (ARM_HAVE_NEON_V8HI_ARITH || TARGET_HAVE_MVE)
#define ARM_HAVE_V4SI_ARITH (ARM_HAVE_NEON_V4SI_ARITH || TARGET_HAVE_MVE)
#define ARM_HAVE_V2DI_ARITH ARM_HAVE_NEON_V2DI_ARITH

#define ARM_HAVE_V4HF_ARITH ARM_HAVE_NEON_V4HF_ARITH
#define ARM_HAVE_V2SF_ARITH ARM_HAVE_NEON_V2SF_ARITH

#define ARM_HAVE_V8HF_ARITH (ARM_HAVE_NEON_V8HF_ARITH || TARGET_HAVE_MVE_FLOAT)
#define ARM_HAVE_V4SF_ARITH (ARM_HAVE_NEON_V4SF_ARITH || TARGET_HAVE_MVE_FLOAT)

/* The conditions under which vector modes are supported by load/store
   instructions using Neon.  */

#define ARM_HAVE_NEON_V8QI_LDST TARGET_NEON
#define ARM_HAVE_NEON_V16QI_LDST TARGET_NEON
#define ARM_HAVE_NEON_V4HI_LDST TARGET_NEON
#define ARM_HAVE_NEON_V8HI_LDST TARGET_NEON
#define ARM_HAVE_NEON_V2SI_LDST TARGET_NEON
#define ARM_HAVE_NEON_V4SI_LDST TARGET_NEON
#define ARM_HAVE_NEON_V4HF_LDST TARGET_NEON_FP16INST
#define ARM_HAVE_NEON_V8HF_LDST TARGET_NEON_FP16INST
#define ARM_HAVE_NEON_V4BF_LDST TARGET_BF16_SIMD
#define ARM_HAVE_NEON_V8BF_LDST TARGET_BF16_SIMD
#define ARM_HAVE_NEON_V2SF_LDST TARGET_NEON
#define ARM_HAVE_NEON_V4SF_LDST TARGET_NEON
#define ARM_HAVE_NEON_DI_LDST TARGET_NEON
#define ARM_HAVE_NEON_V2DI_LDST TARGET_NEON

/* The conditions under which vector modes are supported by load/store
   instructions by any vector extension.  */

#define ARM_HAVE_V8QI_LDST (ARM_HAVE_NEON_V8QI_LDST || TARGET_REALLY_IWMMXT)
#define ARM_HAVE_V4HI_LDST (ARM_HAVE_NEON_V4HI_LDST || TARGET_REALLY_IWMMXT)
#define ARM_HAVE_V2SI_LDST (ARM_HAVE_NEON_V2SI_LDST || TARGET_REALLY_IWMMXT)

#define ARM_HAVE_V16QI_LDST (ARM_HAVE_NEON_V16QI_LDST || TARGET_HAVE_MVE)
#define ARM_HAVE_V8HI_LDST (ARM_HAVE_NEON_V8HI_LDST || TARGET_HAVE_MVE)
#define ARM_HAVE_V4SI_LDST (ARM_HAVE_NEON_V4SI_LDST || TARGET_HAVE_MVE)
#define ARM_HAVE_DI_LDST ARM_HAVE_NEON_DI_LDST
#define ARM_HAVE_V2DI_LDST ARM_HAVE_NEON_V2DI_LDST

#define ARM_HAVE_V4HF_LDST ARM_HAVE_NEON_V4HF_LDST
#define ARM_HAVE_V2SF_LDST ARM_HAVE_NEON_V2SF_LDST

#define ARM_HAVE_V4BF_LDST ARM_HAVE_NEON_V4BF_LDST
#define ARM_HAVE_V8BF_LDST ARM_HAVE_NEON_V8BF_LDST

#define ARM_HAVE_V8HF_LDST (ARM_HAVE_NEON_V8HF_LDST || TARGET_HAVE_MVE_FLOAT)
#define ARM_HAVE_V4SF_LDST (ARM_HAVE_NEON_V4SF_LDST || TARGET_HAVE_MVE_FLOAT)

/* The register numbers in sequence, for passing to arm_gen_load_multiple.  */
extern int arm_regs_in_sequence[];

/* The order in which register should be allocated.  It is good to use ip
   since no saving is required (though calls clobber it) and it never contains
   function parameters.  It is quite good to use lr since other calls may
   clobber it anyway.  Allocate r0 through r3 in reverse order since r3 is
   least likely to contain a function parameter; in addition results are
   returned in r0.
   For VFP/VFPv3, allocate D16-D31 first, then caller-saved registers (D0-D7),
   then D8-D15.  The reason for doing this is to attempt to reduce register
   pressure when both single- and double-precision registers are used in a
   function.  */

#define VREG(X)  (FIRST_VFP_REGNUM + (X))
#define WREG(X)  (FIRST_IWMMXT_REGNUM + (X))
#define WGREG(X) (FIRST_IWMMXT_GR_REGNUM + (X))

#define REG_ALLOC_ORDER				\
{						\
  /* General registers.  */			\
  3,  2,  1,  0,  12, 14,  4,  5,		\
  6,  7,  8,  9,  10, 11,			\
  /* High VFP registers.  */			\
  VREG(32), VREG(33), VREG(34), VREG(35),	\
  VREG(36), VREG(37), VREG(38), VREG(39),	\
  VREG(40), VREG(41), VREG(42), VREG(43),	\
  VREG(44), VREG(45), VREG(46), VREG(47),	\
  VREG(48), VREG(49), VREG(50), VREG(51),	\
  VREG(52), VREG(53), VREG(54), VREG(55),	\
  VREG(56), VREG(57), VREG(58), VREG(59),	\
  VREG(60), VREG(61), VREG(62), VREG(63),	\
  /* VFP argument registers.  */		\
  VREG(15), VREG(14), VREG(13), VREG(12),	\
  VREG(11), VREG(10), VREG(9),  VREG(8),	\
  VREG(7),  VREG(6),  VREG(5),  VREG(4),	\
  VREG(3),  VREG(2),  VREG(1),  VREG(0),	\
  /* VFP call-saved registers.  */		\
  VREG(16), VREG(17), VREG(18), VREG(19),	\
  VREG(20), VREG(21), VREG(22), VREG(23),	\
  VREG(24), VREG(25), VREG(26), VREG(27),	\
  VREG(28), VREG(29), VREG(30), VREG(31),	\
  /* IWMMX registers.  */			\
  WREG(0),  WREG(1),  WREG(2),  WREG(3),	\
  WREG(4),  WREG(5),  WREG(6),  WREG(7),	\
  WREG(8),  WREG(9),  WREG(10), WREG(11),	\
  WREG(12), WREG(13), WREG(14), WREG(15),	\
  WGREG(0), WGREG(1), WGREG(2), WGREG(3),	\
  /* Registers not for general use.  */		\
  CC_REGNUM, VFPCC_REGNUM,			\
  FRAME_POINTER_REGNUM, ARG_POINTER_REGNUM,	\
  SP_REGNUM, PC_REGNUM, APSRQ_REGNUM,		\
  APSRGE_REGNUM, VPR_REGNUM			\
}

#define IS_VPR_REGNUM(REGNUM) \
  ((REGNUM) == VPR_REGNUM)

/* Use different register alloc ordering for Thumb.  */
#define ADJUST_REG_ALLOC_ORDER arm_order_regs_for_local_alloc ()

/* Tell IRA to use the order we define when optimizing for size.  */
#define HONOR_REG_ALLOC_ORDER optimize_function_for_size_p (cfun)

/* Interrupt functions can only use registers that have already been
   saved by the prologue, even if they would normally be
   call-clobbered.  */
#define HARD_REGNO_RENAME_OK(SRC, DST)					\
	(! IS_INTERRUPT (cfun->machine->func_type) ||			\
	 df_regs_ever_live_p (DST))

/* Register and constant classes.  */

/* Register classes.  */
enum reg_class
{
  NO_REGS,
  LO_REGS,
  STACK_REG,
  BASE_REGS,
  HI_REGS,
  CALLER_SAVE_REGS,
  EVEN_REG,
  GENERAL_REGS,
  CORE_REGS,
  VFP_D0_D7_REGS,
  VFP_LO_REGS,
  VFP_HI_REGS,
  VFP_REGS,
  IWMMXT_REGS,
  IWMMXT_GR_REGS,
  CC_REG,
  VFPCC_REG,
  SFP_REG,
  AFP_REG,
  VPR_REG,
  GENERAL_AND_VPR_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define N_REG_CLASSES  (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.  */
#define REG_CLASS_NAMES \
{			\
  "NO_REGS",		\
  "LO_REGS",		\
  "STACK_REG",		\
  "BASE_REGS",		\
  "HI_REGS",		\
  "CALLER_SAVE_REGS",	\
  "EVEN_REG",		\
  "GENERAL_REGS",	\
  "CORE_REGS",		\
  "VFP_D0_D7_REGS",	\
  "VFP_LO_REGS",	\
  "VFP_HI_REGS",	\
  "VFP_REGS",		\
  "IWMMXT_REGS",	\
  "IWMMXT_GR_REGS",	\
  "CC_REG",		\
  "VFPCC_REG",		\
  "SFP_REG",		\
  "AFP_REG",		\
  "VPR_REG",		\
  "GENERAL_AND_VPR_REGS", \
  "ALL_REGS"		\
}

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */
#define REG_CLASS_CONTENTS						\
{									\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000 }, /* NO_REGS  */	\
  { 0x000000FF, 0x00000000, 0x00000000, 0x00000000 }, /* LO_REGS */	\
  { 0x00002000, 0x00000000, 0x00000000, 0x00000000 }, /* STACK_REG */	\
  { 0x000020FF, 0x00000000, 0x00000000, 0x00000000 }, /* BASE_REGS */	\
  { 0x00005F00, 0x00000000, 0x00000000, 0x00000000 }, /* HI_REGS */	\
  { 0x0000100F, 0x00000000, 0x00000000, 0x00000000 }, /* CALLER_SAVE_REGS */ \
  { 0x00005555, 0x00000000, 0x00000000, 0x00000000 }, /* EVEN_REGS.  */ \
  { 0x00005FFF, 0x00000000, 0x00000000, 0x00000000 }, /* GENERAL_REGS */ \
  { 0x00007FFF, 0x00000000, 0x00000000, 0x00000000 }, /* CORE_REGS */	\
  { 0xFFFF0000, 0x00000000, 0x00000000, 0x00000000 }, /* VFP_D0_D7_REGS  */ \
  { 0xFFFF0000, 0x0000FFFF, 0x00000000, 0x00000000 }, /* VFP_LO_REGS  */ \
  { 0x00000000, 0xFFFF0000, 0x0000FFFF, 0x00000000 }, /* VFP_HI_REGS  */ \
  { 0xFFFF0000, 0xFFFFFFFF, 0x0000FFFF, 0x00000000 }, /* VFP_REGS  */	\
  { 0x00000000, 0x00000000, 0xFFFF0000, 0x00000000 }, /* IWMMXT_REGS */	\
  { 0x00000000, 0x00000000, 0x00000000, 0x0000000F }, /* IWMMXT_GR_REGS */ \
  { 0x00000000, 0x00000000, 0x00000000, 0x00000010 }, /* CC_REG */	\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000020 }, /* VFPCC_REG */	\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000040 }, /* SFP_REG */	\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000080 }, /* AFP_REG */	\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000400 }, /* VPR_REG.  */	\
  { 0x00005FFF, 0x00000000, 0x00000000, 0x00000400 }, /* GENERAL_AND_VPR_REGS.  */ \
  { 0xFFFF7FFF, 0xFFFFFFFF, 0xFFFFFFFF, 0x0000040F }  /* ALL_REGS.  */	\
}

#define FP_SYSREGS \
  DEF_FP_SYSREG (FPSCR) \
  DEF_FP_SYSREG (FPSCR_nzcvqc) \
  DEF_FP_SYSREG (VPR) \
  DEF_FP_SYSREG (P0) \
  DEF_FP_SYSREG (FPCXTNS) \
  DEF_FP_SYSREG (FPCXTS)

#define DEF_FP_SYSREG(reg) reg ## _ENUM,
enum vfp_sysregs_encoding {
  FP_SYSREGS
  NB_FP_SYSREGS
};
#undef DEF_FP_SYSREG
extern const char *fp_sysreg_names[NB_FP_SYSREGS];

/* Any of the VFP register classes.  */
#define IS_VFP_CLASS(X) \
  ((X) == VFP_D0_D7_REGS || (X) == VFP_LO_REGS \
   || (X) == VFP_HI_REGS || (X) == VFP_REGS)

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */
#define REGNO_REG_CLASS(REGNO)  arm_regno_class (REGNO)

/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS  (TARGET_THUMB1 ? LO_REGS : GENERAL_REGS)
#define BASE_REG_CLASS   (TARGET_THUMB1 ? LO_REGS : CORE_REGS)

/* For the Thumb the high registers cannot be used as base registers
   when addressing quantities in QI or HI mode; if we don't know the
   mode, then we must be conservative. For MVE we need to load from
   memory to low regs based on given modes i.e [Rn], Rn <= LO_REGS.  */
#define MODE_BASE_REG_CLASS(MODE)				\
   (TARGET_HAVE_MVE ? arm_mode_base_reg_class (MODE)		\
   :(TARGET_32BIT ? CORE_REGS					\
   : GET_MODE_SIZE (MODE) >= 4 ? BASE_REGS			\
   : LO_REGS))

/* For Thumb we cannot support SP+reg addressing, so we return LO_REGS
   instead of BASE_REGS.  */
#define MODE_BASE_REG_REG_CLASS(MODE) BASE_REG_CLASS

/* When this hook returns true for MODE, the compiler allows
   registers explicitly used in the rtl to be used as spill registers
   but prevents the compiler from extending the lifetime of these
   registers.  */
#define TARGET_SMALL_REGISTER_CLASSES_FOR_MODE_P \
  arm_small_register_classes_for_mode_p 

/* Must leave BASE_REGS reloads alone */
#define THUMB_SECONDARY_INPUT_RELOAD_CLASS(CLASS, MODE, X)		\
  (lra_in_progress ? NO_REGS						\
   : ((CLASS) != LO_REGS && (CLASS) != BASE_REGS			\
      ? ((true_regnum (X) == -1 ? LO_REGS				\
         : (true_regnum (X) + hard_regno_nregs (0, MODE) > 8) ? LO_REGS	\
         : NO_REGS)) 							\
      : NO_REGS))

#define THUMB_SECONDARY_OUTPUT_RELOAD_CLASS(CLASS, MODE, X)		\
  (lra_in_progress ? NO_REGS						\
   : (CLASS) != LO_REGS && (CLASS) != BASE_REGS				\
      ? ((true_regnum (X) == -1 ? LO_REGS				\
         : (true_regnum (X) + hard_regno_nregs (0, MODE) > 8) ? LO_REGS	\
         : NO_REGS)) 							\
      : NO_REGS)

/* Return the register class of a scratch register needed to copy IN into
   or out of a register in CLASS in MODE.  If it can be done directly,
   NO_REGS is returned.  */
#define SECONDARY_OUTPUT_RELOAD_CLASS(CLASS, MODE, X)		\
  /* Restrict which direct reloads are allowed for VFP/iWMMXt regs.  */ \
  ((TARGET_HARD_FLOAT && IS_VFP_CLASS (CLASS))			\
   ? coproc_secondary_reload_class (MODE, X, FALSE)		\
   : (TARGET_IWMMXT && (CLASS) == IWMMXT_REGS)			\
   ? coproc_secondary_reload_class (MODE, X, TRUE)		\
   : TARGET_32BIT						\
   ? (((MODE) == HImode && ! arm_arch4 && true_regnum (X) == -1) \
    ? GENERAL_REGS : NO_REGS)					\
   : THUMB_SECONDARY_OUTPUT_RELOAD_CLASS (CLASS, MODE, X))

/* If we need to load shorts byte-at-a-time, then we need a scratch.  */
#define SECONDARY_INPUT_RELOAD_CLASS(CLASS, MODE, X)		\
  /* Restrict which direct reloads are allowed for VFP/iWMMXt regs.  */ \
  ((TARGET_HARD_FLOAT && IS_VFP_CLASS (CLASS))			\
    ? coproc_secondary_reload_class (MODE, X, FALSE) :		\
    (TARGET_IWMMXT && (CLASS) == IWMMXT_REGS) ?			\
    coproc_secondary_reload_class (MODE, X, TRUE) :		\
   (TARGET_32BIT ?						\
    (((CLASS) == IWMMXT_REGS || (CLASS) == IWMMXT_GR_REGS)	\
     && CONSTANT_P (X))						\
    ? GENERAL_REGS :						\
    (((MODE) == HImode && ! arm_arch4				\
      && (MEM_P (X)					\
	  || ((REG_P (X) || GET_CODE (X) == SUBREG)	\
	      && true_regnum (X) == -1)))			\
     ? GENERAL_REGS : NO_REGS)					\
    : THUMB_SECONDARY_INPUT_RELOAD_CLASS (CLASS, MODE, X)))

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.
   ARM regs are UNITS_PER_WORD bits.  
   FIXME: Is this true for iWMMX?  */
#define CLASS_MAX_NREGS(CLASS, MODE)  \
  (CLASS == VPR_REG)		      \
  ? CEIL (GET_MODE_SIZE (MODE), 2)    \
  : (ARM_NUM_REGS (MODE))

/* If defined, gives a class of registers that cannot be used as the
   operand of a SUBREG that changes the mode of the object illegally.  */

/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD  1

/* Define this to nonzero if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */
#define FRAME_GROWS_DOWNWARD 1

/* The amount of scratch space needed by _interwork_{r7,r11}_call_via_rN().
   When present, it is one word in size, and sits at the top of the frame,
   between the soft frame pointer and either r7 or r11.

   We only need _interwork_rM_call_via_rN() for -mcaller-super-interworking,
   and only then if some outgoing arguments are passed on the stack.  It would
   be tempting to also check whether the stack arguments are passed by indirect
   calls, but there seems to be no reason in principle why a post-reload pass
   couldn't convert a direct call into an indirect one.  */
#define CALLER_INTERWORKING_SLOT_SIZE			\
  (TARGET_CALLER_INTERWORKING				\
   && maybe_ne (crtl->outgoing_args_size, 0)		\
   ? UNITS_PER_WORD : 0)

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.  */
/* The push insns do not do this rounding implicitly.
   So don't define this.  */
/* #define PUSH_ROUNDING(NPUSHED)  ROUND_UP_WORD (NPUSHED) */

/* Define this if the maximum size of all the outgoing args is to be
   accumulated and pushed during the prologue.  The amount can be
   found in the variable crtl->outgoing_args_size.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET(FNDECL)  (TARGET_ARM ? 4 : 0)

/* Amount of memory needed for an untyped call to save all possible return
   registers.  */
#define APPLY_RESULT_SIZE arm_apply_result_size()

/* Define DEFAULT_PCC_STRUCT_RETURN to 1 if all structure and union return
   values must be in memory.  On the ARM, they need only do so if larger
   than a word, or if they contain elements offset from zero in the struct.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* These bits describe the different types of function supported
   by the ARM backend.  They are exclusive.  i.e. a function cannot be both a
   normal function and an interworked function, for example.  Knowing the
   type of a function is important for determining its prologue and
   epilogue sequences.
   Note value 7 is currently unassigned.  Also note that the interrupt
   function types all have bit 2 set, so that they can be tested for easily.
   Note that 0 is deliberately chosen for ARM_FT_UNKNOWN so that when the
   machine_function structure is initialized (to zero) func_type will
   default to unknown.  This will force the first use of arm_current_func_type
   to call arm_compute_func_type.  */
#define ARM_FT_UNKNOWN		 0 /* Type has not yet been determined.  */
#define ARM_FT_NORMAL		 1 /* Your normal, straightforward function.  */
#define ARM_FT_INTERWORKED	 2 /* A function that supports interworking.  */
#define ARM_FT_ISR		 4 /* An interrupt service routine.  */
#define ARM_FT_FIQ		 5 /* A fast interrupt service routine.  */
#define ARM_FT_EXCEPTION	 6 /* An ARM exception handler (subcase of ISR).  */

#define ARM_FT_TYPE_MASK	((1 << 3) - 1)

/* In addition functions can have several type modifiers,
   outlined by these bit masks:  */
#define ARM_FT_INTERRUPT	(1 << 2) /* Note overlap with FT_ISR and above.  */
#define ARM_FT_NAKED		(1 << 3) /* No prologue or epilogue.  */
#define ARM_FT_VOLATILE		(1 << 4) /* Does not return.  */
#define ARM_FT_NESTED		(1 << 5) /* Embedded inside another func.  */
#define ARM_FT_STACKALIGN	(1 << 6) /* Called with misaligned stack.  */
#define ARM_FT_CMSE_ENTRY	(1 << 7) /* ARMv8-M non-secure entry function.  */

/* Some macros to test these flags.  */
#define ARM_FUNC_TYPE(t)	(t & ARM_FT_TYPE_MASK)
#define IS_INTERRUPT(t)		(t & ARM_FT_INTERRUPT)
#define IS_VOLATILE(t)     	(t & ARM_FT_VOLATILE)
#define IS_NAKED(t)        	(t & ARM_FT_NAKED)
#define IS_NESTED(t)       	(t & ARM_FT_NESTED)
#define IS_STACKALIGN(t)       	(t & ARM_FT_STACKALIGN)
#define IS_CMSE_ENTRY(t)	(t & ARM_FT_CMSE_ENTRY)


/* Structure used to hold the function stack frame layout.  Offsets are
   relative to the stack pointer on function entry.  Positive offsets are
   in the direction of stack growth.
   Only soft_frame is used in thumb mode.  */

typedef struct GTY(()) arm_stack_offsets
{
  int saved_args;	/* ARG_POINTER_REGNUM.  */
  int frame;		/* ARM_HARD_FRAME_POINTER_REGNUM.  */
  int saved_regs;
  int soft_frame;	/* FRAME_POINTER_REGNUM.  */
  int locals_base;	/* THUMB_HARD_FRAME_POINTER_REGNUM.  */
  int outgoing_args;	/* STACK_POINTER_REGNUM.  */
  unsigned int saved_regs_mask;
}
arm_stack_offsets;

#if !defined(GENERATOR_FILE) && !defined (USED_FOR_TARGET)
/* A C structure for machine-specific, per-function data.
   This is added to the cfun structure.  */
typedef struct GTY(()) machine_function
{
  /* Additional stack adjustment in __builtin_eh_throw.  */
  rtx eh_epilogue_sp_ofs;
  /* Records if LR has to be saved for far jumps.  */
  int far_jump_used;
  /* Records if ARG_POINTER was ever live.  */
  int arg_pointer_live;
  /* Records if the save of LR has been eliminated.  */
  int lr_save_eliminated;
  /* The size of the stack frame.  Only valid after reload.  */
  arm_stack_offsets stack_offsets;
  /* Records the type of the current function.  */
  unsigned long func_type;
  /* Record if the function has a variable argument list.  */
  int uses_anonymous_args;
  /* Records if sibcalls are blocked because an argument
     register is needed to preserve stack alignment.  */
  int sibcall_blocked;
  /* The PIC register for this function.  This might be a pseudo.  */
  rtx pic_reg;
  /* Labels for per-function Thumb call-via stubs.  One per potential calling
     register.  We can never call via LR or PC.  We can call via SP if a
     trampoline happens to be on the top of the stack.  */
  rtx call_via[14];
  /* Set to 1 when a return insn is output, this means that the epilogue
     is not needed.  */
  int return_used_this_function;
  /* When outputting Thumb-1 code, record the last insn that provides
     information about condition codes, and the comparison operands.  */
  rtx thumb1_cc_insn;
  rtx thumb1_cc_op0;
  rtx thumb1_cc_op1;
  /* Also record the CC mode that is supported.  */
  machine_mode thumb1_cc_mode;
  /* Set to 1 after arm_reorg has started.  */
  int after_arm_reorg;
  /* The number of bytes used to store the static chain register on the
     stack, above the stack frame.  */
  int static_chain_stack_bytes;
}
machine_function;
#endif

#define ARM_Q_BIT_READ (arm_q_bit_access ())
#define ARM_GE_BITS_READ (arm_ge_bits_access ())

/* As in the machine_function, a global set of call-via labels, for code 
   that is in text_section.  */
extern GTY(()) rtx thumb_call_via_label[14];

/* The number of potential ways of assigning to a co-processor.  */
#define ARM_NUM_COPROC_SLOTS 1

/* Enumeration of procedure calling standard variants.  We don't really 
   support all of these yet.  */
enum arm_pcs
{
  ARM_PCS_AAPCS,	/* Base standard AAPCS.  */
  ARM_PCS_AAPCS_VFP,	/* Use VFP registers for floating point values.  */
  ARM_PCS_AAPCS_IWMMXT, /* Use iWMMXT registers for vectors.  */
  /* This must be the last AAPCS variant.  */
  ARM_PCS_AAPCS_LOCAL,	/* Private call within this compilation unit.  */
  ARM_PCS_ATPCS,	/* ATPCS.  */
  ARM_PCS_APCS,		/* APCS (legacy Linux etc).  */
  ARM_PCS_UNKNOWN
};

/* Default procedure calling standard of current compilation unit. */
extern enum arm_pcs arm_pcs_default;

#if !defined (USED_FOR_TARGET)
/* A C type for declaring a variable that is used as the first argument of
   `FUNCTION_ARG' and other related values.  */
typedef struct
{
  /* This is the number of registers of arguments scanned so far.  */
  int nregs;
  /* This is the number of iWMMXt register arguments scanned so far.  */
  int iwmmxt_nregs;
  int named_count;
  int nargs;
  /* Which procedure call variant to use for this call.  */
  enum arm_pcs pcs_variant;

  /* AAPCS related state tracking.  */
  int aapcs_arg_processed;  /* No need to lay out this argument again.  */
  int aapcs_cprc_slot;      /* Index of co-processor rules to handle
			       this argument, or -1 if using core
			       registers.  */
  int aapcs_ncrn;
  int aapcs_next_ncrn;
  rtx aapcs_reg;	    /* Register assigned to this argument.  */
  int aapcs_partial;	    /* How many bytes are passed in regs (if
			       split between core regs and stack.
			       Zero otherwise.  */
  int aapcs_cprc_failed[ARM_NUM_COPROC_SLOTS];
  int can_split;	    /* Argument can be split between core regs
			       and the stack.  */
  /* Private data for tracking VFP register allocation */
  unsigned aapcs_vfp_regs_free;
  unsigned aapcs_vfp_reg_alloc;
  int aapcs_vfp_rcount;
  MACHMODE aapcs_vfp_rmode;
} CUMULATIVE_ARGS;
#endif

#define BLOCK_REG_PADDING(MODE, TYPE, FIRST) \
  (arm_pad_reg_upward (MODE, TYPE, FIRST) ? PAD_UPWARD : PAD_DOWNWARD)

/* For AAPCS, padding should never be below the argument. For other ABIs,
 * mimic the default.  */
#define PAD_VARARGS_DOWN \
  ((TARGET_AAPCS_BASED) ? 0 : BYTES_BIG_ENDIAN)

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.
   On the ARM, the offset starts at 0.  */
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, FNDECL, N_NAMED_ARGS) \
  arm_init_cumulative_args (&(CUM), (FNTYPE), (LIBNAME), (FNDECL))

/* 1 if N is a possible register number for function argument passing.
   On the ARM, r0-r3 are used to pass args.  */
#define FUNCTION_ARG_REGNO_P(REGNO)					\
   (IN_RANGE ((REGNO), 0, 3)						\
    || (TARGET_AAPCS_BASED && TARGET_HARD_FLOAT				\
	&& IN_RANGE ((REGNO), FIRST_VFP_REGNUM, FIRST_VFP_REGNUM + 15))	\
    || (TARGET_IWMMXT_ABI						\
	&& IN_RANGE ((REGNO), FIRST_IWMMXT_REGNUM, FIRST_IWMMXT_REGNUM + 9)))


/* If your target environment doesn't prefix user functions with an
   underscore, you may wish to re-define this to prevent any conflicts.  */
#ifndef ARM_MCOUNT_NAME
#define ARM_MCOUNT_NAME "*mcount"
#endif

/* Call the function profiler with a given profile label.  The Acorn
   compiler puts this BEFORE the prolog but gcc puts it afterwards.
   On the ARM the full profile code will look like:
	.data
	LP1
		.word	0
	.text
		mov	ip, lr
		bl	mcount
		.word	LP1

   profile_function() in final.cc outputs the .data section, FUNCTION_PROFILER
   will output the .text section.

   The ``mov ip,lr'' seems like a good idea to stick with cc convention.
   ``prof'' doesn't seem to mind about this!

   Note - this version of the code is designed to work in both ARM and
   Thumb modes.  */
#ifndef ARM_FUNCTION_PROFILER
#define ARM_FUNCTION_PROFILER(STREAM, LABELNO)  	\
{							\
  char temp[20];					\
  rtx sym;						\
							\
  asm_fprintf (STREAM, "\tmov\t%r, %r\n\tbl\t",		\
	   IP_REGNUM, LR_REGNUM);			\
  assemble_name (STREAM, ARM_MCOUNT_NAME);		\
  fputc ('\n', STREAM);					\
  ASM_GENERATE_INTERNAL_LABEL (temp, "LP", LABELNO);	\
  sym = gen_rtx_SYMBOL_REF (Pmode, temp);		\
  assemble_aligned_integer (UNITS_PER_WORD, sym);	\
}
#endif

#ifdef THUMB_FUNCTION_PROFILER
#define FUNCTION_PROFILER(STREAM, LABELNO)		\
  if (TARGET_ARM)					\
    ARM_FUNCTION_PROFILER (STREAM, LABELNO)		\
  else							\
    THUMB_FUNCTION_PROFILER (STREAM, LABELNO)
#else
#define FUNCTION_PROFILER(STREAM, LABELNO)		\
    ARM_FUNCTION_PROFILER (STREAM, LABELNO)
#endif

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.

   On the ARM, the function epilogue recovers the stack pointer from the
   frame.  */
#define EXIT_IGNORE_STACK 1

#define EPILOGUE_USES(REGNO) (epilogue_completed && (REGNO) == LR_REGNUM)

/* Determine if the epilogue should be output as RTL.
   You should override this if you define FUNCTION_EXTRA_EPILOGUE.  */
#define USE_RETURN_INSN(ISCOND)				\
  (TARGET_32BIT ? use_return_insn (ISCOND, NULL) : 0)

/* Definitions for register eliminations.

   This is an array of structures.  Each structure initializes one pair
   of eliminable registers.  The "from" register number is given first,
   followed by "to".  Eliminations of the same "from" register are listed
   in order of preference.

   We have two registers that can be eliminated on the ARM.  First, the
   arg pointer register can often be eliminated in favor of the stack
   pointer register.  Secondly, the pseudo frame pointer register can always
   be eliminated; it is replaced with either the stack or the real frame
   pointer.  Note we have to use {ARM|THUMB}_HARD_FRAME_POINTER_REGNUM
   because the definition of HARD_FRAME_POINTER_REGNUM is not a constant.  */

#define ELIMINABLE_REGS						\
{{ ARG_POINTER_REGNUM,        STACK_POINTER_REGNUM            },\
 { ARG_POINTER_REGNUM,        FRAME_POINTER_REGNUM            },\
 { ARG_POINTER_REGNUM,        ARM_HARD_FRAME_POINTER_REGNUM   },\
 { ARG_POINTER_REGNUM,        THUMB_HARD_FRAME_POINTER_REGNUM },\
 { FRAME_POINTER_REGNUM,      STACK_POINTER_REGNUM            },\
 { FRAME_POINTER_REGNUM,      ARM_HARD_FRAME_POINTER_REGNUM   },\
 { FRAME_POINTER_REGNUM,      THUMB_HARD_FRAME_POINTER_REGNUM }}

/* Define the offset between two registers, one to be eliminated, and the
   other its replacement, at the start of a routine.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
  if (TARGET_ARM)							\
    (OFFSET) = arm_compute_initial_elimination_offset (FROM, TO);	\
  else									\
    (OFFSET) = thumb_compute_initial_elimination_offset (FROM, TO)

/* Special case handling of the location of arguments passed on the stack.  */
#define DEBUGGER_ARG_OFFSET(value, addr) value ? value : arm_debugger_arg_offset (value, addr)

/* Initialize data used by insn expanders.  This is called from insn_emit,
   once for every function before code is generated.  */
#define INIT_EXPANDERS  arm_init_expanders ()

/* Length in units of the trampoline for entering a nested function.  */
#define TRAMPOLINE_SIZE  (TARGET_FDPIC ? 32 : (TARGET_32BIT ? 16 : 20))

/* Alignment required for a trampoline in bits.  */
#define TRAMPOLINE_ALIGNMENT  32

/* Addressing modes, and classification of registers for them.  */
#define HAVE_POST_INCREMENT   1
#define HAVE_PRE_INCREMENT    TARGET_32BIT
#define HAVE_POST_DECREMENT   TARGET_32BIT
#define HAVE_PRE_DECREMENT    TARGET_32BIT
#define HAVE_PRE_MODIFY_DISP  TARGET_32BIT
#define HAVE_POST_MODIFY_DISP TARGET_32BIT
#define HAVE_PRE_MODIFY_REG   TARGET_32BIT
#define HAVE_POST_MODIFY_REG  TARGET_32BIT

enum arm_auto_incmodes
  {
    ARM_POST_INC,
    ARM_PRE_INC,
    ARM_POST_DEC,
    ARM_PRE_DEC
  };

#define ARM_AUTOINC_VALID_FOR_MODE_P(mode, code) \
  (TARGET_32BIT && arm_autoinc_modes_ok_p (mode, code))
#define USE_LOAD_POST_INCREMENT(mode) \
  ARM_AUTOINC_VALID_FOR_MODE_P(mode, ARM_POST_INC)
#define USE_LOAD_PRE_INCREMENT(mode)  \
  ARM_AUTOINC_VALID_FOR_MODE_P(mode, ARM_PRE_INC)
#define USE_LOAD_POST_DECREMENT(mode) \
  ARM_AUTOINC_VALID_FOR_MODE_P(mode, ARM_POST_DEC)
#define USE_LOAD_PRE_DECREMENT(mode)  \
  ARM_AUTOINC_VALID_FOR_MODE_P(mode, ARM_PRE_DEC)

#define USE_STORE_PRE_DECREMENT(mode) USE_LOAD_PRE_DECREMENT(mode)
#define USE_STORE_PRE_INCREMENT(mode) USE_LOAD_PRE_INCREMENT(mode)
#define USE_STORE_POST_DECREMENT(mode) USE_LOAD_POST_DECREMENT(mode)
#define USE_STORE_POST_INCREMENT(mode) USE_LOAD_POST_INCREMENT(mode)

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.  */
#define TEST_REGNO(R, TEST, VALUE) \
  ((R TEST VALUE)	\
    || (reg_renumber && ((unsigned) reg_renumber[R] TEST VALUE)))

/* Don't allow the pc to be used.  */
#define ARM_REGNO_OK_FOR_BASE_P(REGNO)			\
  (TEST_REGNO (REGNO, <, PC_REGNUM)			\
   || TEST_REGNO (REGNO, ==, FRAME_POINTER_REGNUM)	\
   || TEST_REGNO (REGNO, ==, ARG_POINTER_REGNUM))

#define THUMB1_REGNO_MODE_OK_FOR_BASE_P(REGNO, MODE)		\
  (TEST_REGNO (REGNO, <=, LAST_LO_REGNUM)			\
   || (GET_MODE_SIZE (MODE) >= 4				\
       && TEST_REGNO (REGNO, ==, STACK_POINTER_REGNUM)))

#define REGNO_MODE_OK_FOR_BASE_P(REGNO, MODE)		\
  (TARGET_THUMB1					\
   ? THUMB1_REGNO_MODE_OK_FOR_BASE_P (REGNO, MODE)	\
   : ARM_REGNO_OK_FOR_BASE_P (REGNO))

/* Nonzero if X can be the base register in a reg+reg addressing mode.
   For Thumb, we cannot use SP + reg, so reject SP.  */
#define REGNO_MODE_OK_FOR_REG_BASE_P(X, MODE)	\
  REGNO_MODE_OK_FOR_BASE_P (X, QImode)

/* For ARM code, we don't care about the mode, but for Thumb, the index
   must be suitable for use in a QImode load.  */
#define REGNO_OK_FOR_INDEX_P(REGNO)	\
  (REGNO_MODE_OK_FOR_BASE_P (REGNO, QImode) \
   && !TEST_REGNO (REGNO, ==, STACK_POINTER_REGNUM))

/* Maximum number of registers that can appear in a valid memory address.
   Shifts in addresses can't be by a register.  */
#define MAX_REGS_PER_ADDRESS 2

/* Recognize any constant value that is a valid address.  */
/* XXX We can address any constant, eventually...  */
/* ??? Should the TARGET_ARM here also apply to thumb2?  */
#define CONSTANT_ADDRESS_P(X)  			\
  (GET_CODE (X) == SYMBOL_REF 			\
   && (CONSTANT_POOL_ADDRESS_P (X)		\
       || (TARGET_ARM && optimize > 0 && SYMBOL_REF_FLAG (X))))

/* True if SYMBOL + OFFSET constants must refer to something within
   SYMBOL's section.  */
#define ARM_OFFSETS_MUST_BE_WITHIN_SECTIONS_P 0

/* Nonzero if all target requires all absolute relocations be R_ARM_ABS32.  */
#ifndef TARGET_DEFAULT_WORD_RELOCATIONS
#define TARGET_DEFAULT_WORD_RELOCATIONS 0
#endif

#ifndef SUBTARGET_NAME_ENCODING_LENGTHS
#define SUBTARGET_NAME_ENCODING_LENGTHS
#endif

/* This is a C fragment for the inside of a switch statement.
   Each case label should return the number of characters to
   be stripped from the start of a function's name, if that
   name starts with the indicated character.  */
#define ARM_NAME_ENCODING_LENGTHS		\
  case '*':  return 1;				\
  SUBTARGET_NAME_ENCODING_LENGTHS

/* This is how to output a reference to a user-level label named NAME.
   `assemble_name' uses this.  */
#undef  ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(FILE, NAME)		\
   arm_asm_output_labelref (FILE, NAME)

/* Output IT instructions for conditionally executed Thumb-2 instructions.  */
#define ASM_OUTPUT_OPCODE(STREAM, PTR)	\
  if (TARGET_THUMB2)			\
    thumb2_asm_output_opcode (STREAM);

/* The EABI specifies that constructors should go in .init_array.
   Other targets use .ctors for compatibility.  */
#ifndef ARM_EABI_CTORS_SECTION_OP
#define ARM_EABI_CTORS_SECTION_OP \
  "\t.section\t.init_array,\"aw\",%init_array"
#endif
#ifndef ARM_EABI_DTORS_SECTION_OP
#define ARM_EABI_DTORS_SECTION_OP \
  "\t.section\t.fini_array,\"aw\",%fini_array"
#endif
#define ARM_CTORS_SECTION_OP \
  "\t.section\t.ctors,\"aw\",%progbits"
#define ARM_DTORS_SECTION_OP \
  "\t.section\t.dtors,\"aw\",%progbits"

/* Define CTORS_SECTION_ASM_OP.  */
#undef CTORS_SECTION_ASM_OP
#undef DTORS_SECTION_ASM_OP
#ifndef IN_LIBGCC2
# define CTORS_SECTION_ASM_OP \
   (TARGET_AAPCS_BASED ? ARM_EABI_CTORS_SECTION_OP : ARM_CTORS_SECTION_OP)
# define DTORS_SECTION_ASM_OP \
   (TARGET_AAPCS_BASED ? ARM_EABI_DTORS_SECTION_OP : ARM_DTORS_SECTION_OP)
#else /* !defined (IN_LIBGCC2) */
/* In libgcc, CTORS_SECTION_ASM_OP must be a compile-time constant,
   so we cannot use the definition above.  */
# ifdef __ARM_EABI__
/* The .ctors section is not part of the EABI, so we do not define
   CTORS_SECTION_ASM_OP when in libgcc; that prevents crtstuff
   from trying to use it.  We do define it when doing normal
   compilation, as .init_array can be used instead of .ctors.  */
/* There is no need to emit begin or end markers when using
   init_array; the dynamic linker will compute the size of the
   array itself based on special symbols created by the static
   linker.  However, we do need to arrange to set up
   exception-handling here.  */
#   define CTOR_LIST_BEGIN asm (ARM_EABI_CTORS_SECTION_OP)
#   define CTOR_LIST_END /* empty */
#   define DTOR_LIST_BEGIN asm (ARM_EABI_DTORS_SECTION_OP)
#   define DTOR_LIST_END /* empty */
# else /* !defined (__ARM_EABI__) */
#   define CTORS_SECTION_ASM_OP ARM_CTORS_SECTION_OP
#   define DTORS_SECTION_ASM_OP ARM_DTORS_SECTION_OP
# endif /* !defined (__ARM_EABI__) */
#endif /* !defined (IN_LIBCC2) */

/* True if the operating system can merge entities with vague linkage
   (e.g., symbols in COMDAT group) during dynamic linking.  */
#ifndef TARGET_ARM_DYNAMIC_VAGUE_LINKAGE_P
#define TARGET_ARM_DYNAMIC_VAGUE_LINKAGE_P true
#endif

#define ARM_OUTPUT_FN_UNWIND(F, PROLOGUE) arm_output_fn_unwind (F, PROLOGUE)

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx
   and check its validity for a certain class.
   We have two alternate definitions for each of them.
   The usual definition accepts all pseudo regs; the other rejects
   them unless they have been allocated suitable hard regs.
   The symbol REG_OK_STRICT causes the latter definition to be used.
   Thumb-2 has the same restrictions as arm.  */
#ifndef REG_OK_STRICT

#define ARM_REG_OK_FOR_BASE_P(X)		\
  (REGNO (X) <= LAST_ARM_REGNUM			\
   || REGNO (X) >= FIRST_PSEUDO_REGISTER	\
   || REGNO (X) == FRAME_POINTER_REGNUM		\
   || REGNO (X) == ARG_POINTER_REGNUM)

#define ARM_REG_OK_FOR_INDEX_P(X)		\
  ((REGNO (X) <= LAST_ARM_REGNUM		\
    && REGNO (X) != STACK_POINTER_REGNUM)	\
   || REGNO (X) >= FIRST_PSEUDO_REGISTER	\
   || REGNO (X) == FRAME_POINTER_REGNUM		\
   || REGNO (X) == ARG_POINTER_REGNUM)

#define THUMB1_REG_MODE_OK_FOR_BASE_P(X, MODE)	\
  (REGNO (X) <= LAST_LO_REGNUM			\
   || REGNO (X) >= FIRST_PSEUDO_REGISTER	\
   || (GET_MODE_SIZE (MODE) >= 4		\
       && (REGNO (X) == STACK_POINTER_REGNUM	\
	   || (X) == hard_frame_pointer_rtx	\
	   || (X) == arg_pointer_rtx)))

#define REG_STRICT_P 0

#else /* REG_OK_STRICT */

#define ARM_REG_OK_FOR_BASE_P(X) 		\
  ARM_REGNO_OK_FOR_BASE_P (REGNO (X))

#define ARM_REG_OK_FOR_INDEX_P(X) 		\
  ARM_REGNO_OK_FOR_INDEX_P (REGNO (X))

#define THUMB1_REG_MODE_OK_FOR_BASE_P(X, MODE)	\
  THUMB1_REGNO_MODE_OK_FOR_BASE_P (REGNO (X), MODE)

#define REG_STRICT_P 1

#endif /* REG_OK_STRICT */

/* Now define some helpers in terms of the above.  */

#define REG_MODE_OK_FOR_BASE_P(X, MODE)		\
  (TARGET_THUMB1				\
   ? THUMB1_REG_MODE_OK_FOR_BASE_P (X, MODE)	\
   : ARM_REG_OK_FOR_BASE_P (X))

/* For 16-bit Thumb, a valid index register is anything that can be used in
   a byte load instruction.  */
#define THUMB1_REG_OK_FOR_INDEX_P(X) \
  THUMB1_REG_MODE_OK_FOR_BASE_P (X, QImode)

/* Nonzero if X is a hard reg that can be used as an index
   or if it is a pseudo reg.  On the Thumb, the stack pointer
   is not suitable.  */
#define REG_OK_FOR_INDEX_P(X)			\
  (TARGET_THUMB1				\
   ? THUMB1_REG_OK_FOR_INDEX_P (X)		\
   : ARM_REG_OK_FOR_INDEX_P (X))

/* Nonzero if X can be the base register in a reg+reg addressing mode.
   For Thumb, we cannot use SP + reg, so reject SP.  */
#define REG_MODE_OK_FOR_REG_BASE_P(X, MODE)	\
  REG_OK_FOR_INDEX_P (X)

#define ARM_BASE_REGISTER_RTX_P(X)  \
  (REG_P (X) && ARM_REG_OK_FOR_BASE_P (X))

#define ARM_INDEX_REGISTER_RTX_P(X)  \
  (REG_P (X) && ARM_REG_OK_FOR_INDEX_P (X))

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE Pmode

#define CASE_VECTOR_PC_RELATIVE ((TARGET_THUMB2				\
				  || (TARGET_THUMB1			\
				      && (optimize_size || flag_pic)))	\
				 && (!target_pure_code))


#define CASE_VECTOR_SHORTEN_MODE(min, max, body)			\
  (TARGET_THUMB1							\
   ? (min >= 0 && max < 512						\
      ? (ADDR_DIFF_VEC_FLAGS (body).offset_unsigned = 1, QImode)	\
      : min >= -256 && max < 256					\
      ? (ADDR_DIFF_VEC_FLAGS (body).offset_unsigned = 0, QImode)	\
      : min >= 0 && max < 8192						\
      ? (ADDR_DIFF_VEC_FLAGS (body).offset_unsigned = 1, HImode)	\
      : min >= -4096 && max < 4096					\
      ? (ADDR_DIFF_VEC_FLAGS (body).offset_unsigned = 0, HImode)	\
      : SImode)								\
   : ((min < 0 || max >= 0x20000 || !TARGET_THUMB2) ? SImode		\
      : (max >= 0x200) ? HImode						\
      : QImode))

/* signed 'char' is most compatible, but RISC OS wants it unsigned.
   unsigned is probably best, but may break some code.  */
#ifndef DEFAULT_SIGNED_CHAR
#define DEFAULT_SIGNED_CHAR  0
#endif

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 4

#undef  MOVE_RATIO
#define MOVE_RATIO(speed) (arm_tune_xscale ? 4 : 2)

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS 1

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, UNKNOWN if none.  */
#define LOAD_EXTEND_OP(MODE)						\
  (TARGET_THUMB ? ZERO_EXTEND :						\
   ((arm_arch4 || (MODE) == QImode) ? ZERO_EXTEND			\
    : ((BYTES_BIG_ENDIAN && (MODE) == HImode) ? SIGN_EXTEND : UNKNOWN)))

/* Nonzero if access to memory by bytes is slow and undesirable.  */
#define SLOW_BYTE_ACCESS 0

/* Immediate shift counts are truncated by the output routines (or was it
   the assembler?).  Shift counts in a register are truncated by ARM.  Note
   that the native compiler puts too large (> 32) immediate shift counts
   into a register and shifts by the register, letting the ARM decide what
   to do instead of doing that itself.  */
/* This is all wrong.  Defining SHIFT_COUNT_TRUNCATED tells combine that
   code like (X << (Y % 32)) for register X, Y is equivalent to (X << Y).
   On the arm, Y in a register is used modulo 256 for the shift. Only for
   rotates is modulo 32 used.  */
/* #define SHIFT_COUNT_TRUNCATED 1 */

/* Calling from registers is a massive pain.  */
#define NO_FUNCTION_CSE 1

/* The machine modes of pointers and functions */
#define Pmode  SImode
#define FUNCTION_MODE  Pmode

#define ARM_FRAME_RTX(X)					\
  (   (X) == frame_pointer_rtx || (X) == stack_pointer_rtx	\
   || (X) == arg_pointer_rtx)

/* Try to generate sequences that don't involve branches, we can then use
   conditional instructions.  */
#define BRANCH_COST(speed_p, predictable_p)			\
  ((arm_branch_cost != -1) ? arm_branch_cost :			\
   (current_tune->branch_cost (speed_p, predictable_p)))

/* False if short circuit operation is preferred.  */
#define LOGICAL_OP_NON_SHORT_CIRCUIT					\
  ((optimize_size)							\
   ? (TARGET_THUMB ? false : true)					\
   : TARGET_THUMB ? static_cast<bool> (current_tune->logical_op_non_short_circuit_thumb) \
   : static_cast<bool> (current_tune->logical_op_non_short_circuit_arm))


/* Position Independent Code.  */
/* We decide which register to use based on the compilation options and
   the assembler in use; this is more general than the APCS restriction of
   using sb (r9) all the time.  */
extern unsigned arm_pic_register;

/* The register number of the register used to address a table of static
   data addresses in memory.  */
#define PIC_OFFSET_TABLE_REGNUM arm_pic_register

/* For FDPIC, the FDPIC register is call-clobbered (otherwise PLT
   entries would need to handle saving and restoring it).  */
#define PIC_OFFSET_TABLE_REG_CALL_CLOBBERED TARGET_FDPIC

/* We can't directly access anything that contains a symbol,
   nor can we indirect via the constant pool.  One exception is
   UNSPEC_TLS, which is always PIC.  */
#define LEGITIMATE_PIC_OPERAND_P(X)					\
	(!(symbol_mentioned_p (X)					\
	   || label_mentioned_p (X)					\
	   || (GET_CODE (X) == SYMBOL_REF				\
	       && CONSTANT_POOL_ADDRESS_P (X)				\
	       && (symbol_mentioned_p (get_pool_constant (X))		\
		   || label_mentioned_p (get_pool_constant (X)))))	\
	 || tls_mentioned_p (X))

/* We may want to save the PIC register if it is a dedicated one.  */
#define PIC_REGISTER_MAY_NEED_SAVING			\
  (flag_pic						\
   && !TARGET_SINGLE_PIC_BASE				\
   && !TARGET_FDPIC					\
   && arm_pic_register != INVALID_REGNUM)

/* We need to know when we are making a constant pool; this determines
   whether data needs to be in the GOT or can be referenced via a GOT
   offset.  */
extern int making_const_table;

/* Handle pragmas for compatibility with Intel's compilers.  */
/* Also abuse this to register additional C specific EABI attributes.  */
#define REGISTER_TARGET_PRAGMAS() do {					\
  c_register_pragma (0, "long_calls", arm_pr_long_calls);		\
  c_register_pragma (0, "no_long_calls", arm_pr_no_long_calls);		\
  c_register_pragma (0, "long_calls_off", arm_pr_long_calls_off);	\
  arm_lang_object_attributes_init();					\
  arm_register_target_pragmas();                                       \
} while (0)

/* Condition code information.  */
/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
   return the mode to be used for the comparison.  */

#define SELECT_CC_MODE(OP, X, Y)  arm_select_cc_mode (OP, X, Y)

#define REVERSIBLE_CC_MODE(MODE) 1

#define REVERSE_CONDITION(CODE,MODE) \
  (((MODE) == CCFPmode || (MODE) == CCFPEmode) \
   ? reverse_condition_maybe_unordered (code) \
   : reverse_condition (code))

#define CLZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE) \
  ((VALUE) = GET_MODE_UNIT_BITSIZE (MODE), 2)
#define CTZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE) \
  ((VALUE) = GET_MODE_UNIT_BITSIZE (MODE), 2)

#define CC_STATUS_INIT \
  do { cfun->machine->thumb1_cc_insn = NULL_RTX; } while (0)

#undef ASM_APP_ON
#define ASM_APP_ON (inline_asm_unified ? "\t.syntax unified\n" : \
		    "\t.syntax divided\n")

#undef  ASM_APP_OFF
#define ASM_APP_OFF (TARGET_ARM ? "\t.arm\n\t.syntax unified\n" : \
		     "\t.thumb\n\t.syntax unified\n")

/* Output a push or a pop instruction (only used when profiling).
   We can't push STATIC_CHAIN_REGNUM (r12) directly with Thumb-1.  We know
   that ASM_OUTPUT_REG_PUSH will be matched with ASM_OUTPUT_REG_POP, and
   that r7 isn't used by the function profiler, so we can use it as a
   scratch reg.  WARNING: This isn't safe in the general case!  It may be
   sensitive to future changes in final.cc:profile_function.  */
#define ASM_OUTPUT_REG_PUSH(STREAM, REGNO)		\
  do							\
    {							\
      if (TARGET_THUMB1					\
	       && (REGNO) == STATIC_CHAIN_REGNUM)	\
	{						\
	  asm_fprintf (STREAM, "\tpush\t{r7}\n");	\
	  asm_fprintf (STREAM, "\tmov\tr7, %r\n", REGNO);\
	  asm_fprintf (STREAM, "\tpush\t{r7}\n");	\
	}						\
      else						\
	asm_fprintf (STREAM, "\tpush {%r}\n", REGNO);	\
    } while (0)


/* See comment for ASM_OUTPUT_REG_PUSH concerning Thumb-1 issue.  */
#define ASM_OUTPUT_REG_POP(STREAM, REGNO)		\
  do							\
    {							\
      if (TARGET_THUMB1					\
	  && (REGNO) == STATIC_CHAIN_REGNUM)		\
	{						\
	  asm_fprintf (STREAM, "\tpop\t{r7}\n");	\
	  asm_fprintf (STREAM, "\tmov\t%r, r7\n", REGNO);\
	  asm_fprintf (STREAM, "\tpop\t{r7}\n");	\
	}						\
      else						\
	asm_fprintf (STREAM, "\tpop {%r}\n", REGNO);	\
    } while (0)

#define ADDR_VEC_ALIGN(JUMPTABLE)	\
  ((TARGET_THUMB && GET_MODE (PATTERN (JUMPTABLE)) == SImode) ? 2 : 0)

/* Alignment for case labels comes from ADDR_VEC_ALIGN; avoid the
   default alignment from elfos.h.  */
#undef ASM_OUTPUT_BEFORE_CASE_LABEL
#define ASM_OUTPUT_BEFORE_CASE_LABEL(FILE, PREFIX, NUM, TABLE) /* Empty.  */

#define LABEL_ALIGN_AFTER_BARRIER(LABEL)                \
   (GET_CODE (PATTERN (prev_active_insn (LABEL))) == ADDR_DIFF_VEC \
   ? 1 : 0)

#define ARM_DECLARE_FUNCTION_NAME(STREAM, NAME, DECL) 	\
  arm_declare_function_name ((STREAM), (NAME), (DECL));

/* For aliases of functions we use .thumb_set instead.  */
#define ASM_OUTPUT_DEF_FROM_DECLS(FILE, DECL1, DECL2)		\
  do						   		\
    {								\
      const char *const LABEL1 = XSTR (XEXP (DECL_RTL (decl), 0), 0); \
      const char *const LABEL2 = IDENTIFIER_POINTER (DECL2);	\
								\
      if (TARGET_THUMB && TREE_CODE (DECL1) == FUNCTION_DECL)	\
	{							\
	  fprintf (FILE, "\t.thumb_set ");			\
	  assemble_name (FILE, LABEL1);			   	\
	  fprintf (FILE, ",");			   		\
	  assemble_name (FILE, LABEL2);		   		\
	  fprintf (FILE, "\n");					\
	}							\
      else							\
	ASM_OUTPUT_DEF (FILE, LABEL1, LABEL2);			\
    }								\
  while (0)

#ifdef HAVE_GAS_MAX_SKIP_P2ALIGN
/* To support -falign-* switches we need to use .p2align so
   that alignment directives in code sections will be padded
   with no-op instructions, rather than zeroes.  */
#define ASM_OUTPUT_MAX_SKIP_ALIGN(FILE, LOG, MAX_SKIP)		\
  if ((LOG) != 0)						\
    {								\
      if ((MAX_SKIP) == 0)					\
        fprintf ((FILE), "\t.p2align %d\n", (int) (LOG));	\
      else							\
        fprintf ((FILE), "\t.p2align %d,,%d\n",			\
                 (int) (LOG), (int) (MAX_SKIP));		\
    }
#endif

/* Add two bytes to the length of conditionally executed Thumb-2
   instructions for the IT instruction.  */
#define ADJUST_INSN_LENGTH(insn, length) \
  if (TARGET_THUMB2 && GET_CODE (PATTERN (insn)) == COND_EXEC) \
    length += 2;

/* Only perform branch elimination (by making instructions conditional) if
   we're optimizing.  For Thumb-2 check if any IT instructions need
   outputting.  */
#define FINAL_PRESCAN_INSN(INSN, OPVEC, NOPERANDS)	\
  if (TARGET_ARM && optimize)				\
    arm_final_prescan_insn (INSN);			\
  else if (TARGET_THUMB2)				\
    thumb2_final_prescan_insn (INSN);			\
  else if (TARGET_THUMB1)				\
    thumb1_final_prescan_insn (INSN)

#define ARM_SIGN_EXTEND(x)  ((HOST_WIDE_INT)			\
  (HOST_BITS_PER_WIDE_INT <= 32 ? (unsigned HOST_WIDE_INT) (x)	\
   : ((((unsigned HOST_WIDE_INT)(x)) & (unsigned HOST_WIDE_INT) 0xffffffff) |\
      ((((unsigned HOST_WIDE_INT)(x)) & (unsigned HOST_WIDE_INT) 0x80000000) \
       ? ((~ (unsigned HOST_WIDE_INT) 0)			\
	  & ~ (unsigned HOST_WIDE_INT) 0xffffffff)		\
       : 0))))

/* A C expression whose value is RTL representing the value of the return
   address for the frame COUNT steps up from the current frame.  */

#define RETURN_ADDR_RTX(COUNT, FRAME) \
  arm_return_addr (COUNT, FRAME)

/* Mask of the bits in the PC that contain the real return address
   when running in 26-bit mode.  */
#define RETURN_ADDR_MASK26 (0x03fffffc)

/* Pick up the return address upon entry to a procedure. Used for
   dwarf2 unwind information.  This also enables the table driven
   mechanism.  */
#define INCOMING_RETURN_ADDR_RTX	gen_rtx_REG (Pmode, LR_REGNUM)
#define DWARF_FRAME_RETURN_COLUMN	DWARF_FRAME_REGNUM (LR_REGNUM)

/* Used to mask out junk bits from the return address, such as
   processor state, interrupt status, condition codes and the like.  */
#define MASK_RETURN_ADDR \
  /* If we are generating code for an ARM2/ARM3 machine or for an ARM6	\
     in 26 bit mode, the condition codes must be masked out of the	\
     return address.  This does not apply to ARM6 and later processors	\
     when running in 32 bit mode.  */					\
  ((arm_arch4 || TARGET_THUMB)						\
   ? (gen_int_mode ((unsigned long)0xffffffff, Pmode))			\
   : arm_gen_return_addr_mask ())


/* Do not emit .note.GNU-stack by default.  */
#ifndef NEED_INDICATE_EXEC_STACK
#define NEED_INDICATE_EXEC_STACK	0
#endif

#define TARGET_ARM_ARCH	\
  (arm_base_arch)	\

/* The highest Thumb instruction set version supported by the chip.  */
#define TARGET_ARM_ARCH_ISA_THUMB		\
  (arm_arch_thumb2 ? 2 : (arm_arch_thumb1 ? 1 : 0))

/* Expands to an upper-case char of the target's architectural
   profile.  */
#define TARGET_ARM_ARCH_PROFILE				\
  (arm_active_target.profile)

/* Bit-field indicating what size LDREX/STREX loads/stores are available.
   Bit 0 for bytes, up to bit 3 for double-words.  */
#define TARGET_ARM_FEATURE_LDREX				\
  ((TARGET_HAVE_LDREX ? 4 : 0)					\
   | (TARGET_HAVE_LDREXBH ? 3 : 0)				\
   | (TARGET_HAVE_LDREXD ? 8 : 0))

/* Set as a bit mask indicating the available widths of hardware floating
   point types.  Where bit 1 indicates 16-bit support, bit 2 indicates
   32-bit support, bit 3 indicates 64-bit support.  */
#define TARGET_ARM_FP			\
  (!TARGET_SOFT_FLOAT ? (TARGET_VFP_SINGLE ? 4		\
			: (TARGET_VFP_DOUBLE ? (TARGET_FP16 ? 14 : 12) : 0)) \
		      : 0)


/* Set as a bit mask indicating the available widths of floating point
   types for hardware NEON floating point.  This is the same as
   TARGET_ARM_FP without the 64-bit bit set.  */
#define TARGET_NEON_FP				 \
  (TARGET_NEON ? (TARGET_ARM_FP & (0xff ^ 0x08)) \
	       : 0)

/* Name of the automatic fpu-selection option.  */
#define FPUTYPE_AUTO "auto"

/* The maximum number of parallel loads or stores we support in an ldm/stm
   instruction.  */
#define MAX_LDM_STM_OPS 4

extern const char *arm_rewrite_mcpu (int argc, const char **argv);
extern const char *arm_rewrite_march (int argc, const char **argv);
extern const char *arm_asm_auto_mfpu (int argc, const char **argv);
#define ASM_CPU_SPEC_FUNCTIONS			\
  { "rewrite_mcpu", arm_rewrite_mcpu },	\
  { "rewrite_march", arm_rewrite_march },	\
  { "asm_auto_mfpu", arm_asm_auto_mfpu },

#define ASM_CPU_SPEC							\
  " %{mfpu=auto:%<mfpu=auto %:asm_auto_mfpu(%{march=*: arch %*})}"	\
  " %{mcpu=generic-*:-march=%:rewrite_march(%{mcpu=generic-*:%*});"	\
  "   march=*:-march=%:rewrite_march(%{march=*:%*});"			\
  "   mcpu=*:-mcpu=%:rewrite_mcpu(%{mcpu=*:%*})"			\
  " }"

extern const char *arm_target_mode (int argc, const char **argv);
#define TARGET_MODE_SPEC_FUNCTIONS			\
  { "target_mode_check", arm_target_mode },

/* -mcpu=native handling only makes sense with compiler running on
   an ARM chip.  */
#if defined(__arm__)
extern const char *host_detect_local_cpu (int argc, const char **argv);
#define HAVE_LOCAL_CPU_DETECT
# define MCPU_MTUNE_NATIVE_FUNCTIONS			\
  { "local_cpu_detect", host_detect_local_cpu },
# define MCPU_MTUNE_NATIVE_SPECS				\
   " %{march=native:%<march=native %:local_cpu_detect(arch)}"	\
   " %{mcpu=native:%<mcpu=native %:local_cpu_detect(cpu)}"	\
   " %{mtune=native:%<mtune=native %:local_cpu_detect(tune)}"
#else
# define MCPU_MTUNE_NATIVE_FUNCTIONS
# define MCPU_MTUNE_NATIVE_SPECS ""
#endif

const char *arm_canon_arch_option (int argc, const char **argv);
const char *arm_canon_arch_multilib_option (int argc, const char **argv);

#define CANON_ARCH_SPEC_FUNCTION		\
  { "canon_arch", arm_canon_arch_option },

#define CANON_ARCH_MULTILIB_SPEC_FUNCTION		\
  { "canon_arch_multilib", arm_canon_arch_multilib_option },

const char *arm_be8_option (int argc, const char **argv);
#define BE8_SPEC_FUNCTION			\
  { "be8_linkopt", arm_be8_option },

# define EXTRA_SPEC_FUNCTIONS			\
  MCPU_MTUNE_NATIVE_FUNCTIONS			\
  ASM_CPU_SPEC_FUNCTIONS			\
  CANON_ARCH_SPEC_FUNCTION			\
  CANON_ARCH_MULTILIB_SPEC_FUNCTION		\
  TARGET_MODE_SPEC_FUNCTIONS			\
  BE8_SPEC_FUNCTION

/* Automatically add -mthumb for Thumb-only targets if mode isn't specified
   via the configuration option --with-mode or via the command line. The
   function target_mode_check is called to do the check with either:
   - an array of -march values if any is given;
   - an array of -mcpu values if any is given;
   - an empty array.  */
#define TARGET_MODE_SPECS						\
  " %{!marm:%{!mthumb:%:target_mode_check(%{march=*:arch %*;mcpu=*:cpu %*;:})}}"

/* Generate a canonical string to represent the architecture selected.  */
#define ARCH_CANONICAL_SPECS				\
  " -march=%:canon_arch(%{mcpu=*: cpu %*} "		\
  "                     %{march=*: arch %*} "		\
  "                     %{mfpu=*: fpu %*} "		\
  "                     %{mfloat-abi=*: abi %*}"	\
  "                     %<march=*) "

/* Generate a canonical string to represent the architecture selected ignoring
   the options not required for multilib linking.  */
#define MULTILIB_ARCH_CANONICAL_SPECS				\
  "-mlibarch=%:canon_arch_multilib(%{mcpu=*: cpu %*} "		\
  "				   %{march=*: arch %*} "	\
  "				   %{mfpu=*: fpu %*} "		\
  "				   %{mfloat-abi=*: abi %*}"	\
  "				   %<mlibarch=*) "

/* Complete set of specs for the driver.  Commas separate the
   individual rules so that any option suppression (%<opt...)is
   completed before starting subsequent rules.  */
#define DRIVER_SELF_SPECS			\
  MCPU_MTUNE_NATIVE_SPECS,			\
  TARGET_MODE_SPECS,				\
  MULTILIB_ARCH_CANONICAL_SPECS,		\
  ARCH_CANONICAL_SPECS

#define TARGET_SUPPORTS_WIDE_INT 1

/* For switching between functions with different target attributes.  */
#define SWITCHABLE_TARGET 1

/* Define SECTION_ARM_PURECODE as the ARM specific section attribute
   representation for SHF_ARM_PURECODE in GCC.  */
#define SECTION_ARM_PURECODE SECTION_MACH_DEP

#endif /* ! GCC_ARM_H */
