/* Definitions of target machine for GNU compiler, for IBM S/390
   Copyright (C) 1999-2022 Free Software Foundation, Inc.
   Contributed by Hartmut Penner (hpenner@de.ibm.com) and
		  Ulrich Weigand (uweigand@de.ibm.com).
		  Andreas Krebbel (Andreas.Krebbel@de.ibm.com)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef _S390_H
#define _S390_H

/* Optional architectural facilities supported by the processor.  */

enum processor_flags
{
  PF_IEEE_FLOAT = 1,
  PF_ZARCH = 2,
  PF_LONG_DISPLACEMENT = 4,
  PF_EXTIMM = 8,
  PF_DFP = 16,
  PF_Z10 = 32,
  PF_Z196 = 64,
  PF_ZEC12 = 128,
  PF_TX = 256,
  PF_Z13 = 512,
  PF_VX = 1024,
  PF_Z14 = 2048,
  PF_VXE = 4096,
  PF_VXE2 = 8192,
  PF_Z15 = 16384,
  PF_NNPA = 32768,
  PF_Z16 = 65536
};

/* This is necessary to avoid a warning about comparing different enum
   types.  */
#define s390_tune_attr ((enum attr_cpu)(s390_tune > PROCESSOR_3931_Z16 ? PROCESSOR_3931_Z16 : s390_tune ))

/* These flags indicate that the generated code should run on a cpu
   providing the respective hardware facility regardless of the
   current cpu mode (ESA or z/Architecture).  */

#define TARGET_CPU_IEEE_FLOAT \
	(s390_arch_flags & PF_IEEE_FLOAT)
#define TARGET_CPU_IEEE_FLOAT_P(opts) \
	(opts->x_s390_arch_flags & PF_IEEE_FLOAT)
#define TARGET_CPU_LONG_DISPLACEMENT \
	(s390_arch_flags & PF_LONG_DISPLACEMENT)
#define TARGET_CPU_LONG_DISPLACEMENT_P(opts) \
	(opts->x_s390_arch_flags & PF_LONG_DISPLACEMENT)
#define TARGET_CPU_EXTIMM \
	(s390_arch_flags & PF_EXTIMM)
#define TARGET_CPU_EXTIMM_P(opts) \
	(opts->x_s390_arch_flags & PF_EXTIMM)
#define TARGET_CPU_DFP \
	(s390_arch_flags & PF_DFP)
#define TARGET_CPU_DFP_P(opts) \
	(opts->x_s390_arch_flags & PF_DFP)
#define TARGET_CPU_Z10 \
	(s390_arch_flags & PF_Z10)
#define TARGET_CPU_Z10_P(opts) \
	(opts->x_s390_arch_flags & PF_Z10)
#define TARGET_CPU_Z196 \
	(s390_arch_flags & PF_Z196)
#define TARGET_CPU_Z196_P(opts) \
	(opts->x_s390_arch_flags & PF_Z196)
#define TARGET_CPU_ZEC12 \
	(s390_arch_flags & PF_ZEC12)
#define TARGET_CPU_ZEC12_P(opts) \
	(opts->x_s390_arch_flags & PF_ZEC12)
#define TARGET_CPU_HTM \
	(s390_arch_flags & PF_TX)
#define TARGET_CPU_HTM_P(opts) \
	(opts->x_s390_arch_flags & PF_TX)
#define TARGET_CPU_Z13 \
	(s390_arch_flags & PF_Z13)
#define TARGET_CPU_Z13_P(opts) \
	(opts->x_s390_arch_flags & PF_Z13)
#define TARGET_CPU_VX \
	(s390_arch_flags & PF_VX)
#define TARGET_CPU_VX_P(opts) \
	(opts->x_s390_arch_flags & PF_VX)
#define TARGET_CPU_Z14 \
	(s390_arch_flags & PF_Z14)
#define TARGET_CPU_Z14_P(opts) \
	(opts->x_s390_arch_flags & PF_Z14)
#define TARGET_CPU_VXE \
	(s390_arch_flags & PF_VXE)
#define TARGET_CPU_VXE_P(opts) \
	(opts->x_s390_arch_flags & PF_VXE)
#define TARGET_CPU_Z15 \
	(s390_arch_flags & PF_Z15)
#define TARGET_CPU_Z15_P(opts) \
	(opts->x_s390_arch_flags & PF_Z15)
#define TARGET_CPU_VXE2 \
	(s390_arch_flags & PF_VXE2)
#define TARGET_CPU_VXE2_P(opts) \
	(opts->x_s390_arch_flags & PF_VXE2)
#define TARGET_CPU_Z16 \
	(s390_arch_flags & PF_Z16)
#define TARGET_CPU_Z16_P(opts) \
	(opts->x_s390_arch_flags & PF_Z16)
#define TARGET_CPU_NNPA \
	(s390_arch_flags & PF_NNPA)
#define TARGET_CPU_NNPA_P(opts) \
	(opts->x_s390_arch_flags & PF_NNPA)

#define TARGET_HARD_FLOAT_P(opts) (!TARGET_SOFT_FLOAT_P(opts))

/* These flags indicate that the generated code should run on a cpu
   providing the respective hardware facility when run in
   z/Architecture mode.  */

#define TARGET_LONG_DISPLACEMENT \
	(TARGET_ZARCH && TARGET_CPU_LONG_DISPLACEMENT)
#define TARGET_LONG_DISPLACEMENT_P(opts) \
	(TARGET_ZARCH_P (opts->x_target_flags) \
	 && TARGET_CPU_LONG_DISPLACEMENT_P (opts))
#define TARGET_EXTIMM \
	(TARGET_ZARCH && TARGET_CPU_EXTIMM)
#define TARGET_EXTIMM_P(opts) \
	(TARGET_ZARCH_P (opts->x_target_flags) && TARGET_CPU_EXTIMM_P (opts))
#define TARGET_DFP \
	(TARGET_ZARCH && TARGET_CPU_DFP && TARGET_HARD_FLOAT)
#define TARGET_DFP_P(opts) \
	(TARGET_ZARCH_P (opts->x_target_flags) && TARGET_CPU_DFP_P (opts) \
	 && TARGET_HARD_FLOAT_P (opts->x_target_flags))
#define TARGET_Z10 \
	(TARGET_ZARCH && TARGET_CPU_Z10)
#define TARGET_Z10_P(opts) \
	(TARGET_ZARCH_P (opts->x_target_flags) && TARGET_CPU_Z10_P (opts))
#define TARGET_Z196 \
	(TARGET_ZARCH && TARGET_CPU_Z196)
#define TARGET_Z196_P(opts) \
	(TARGET_ZARCH_P (opts->x_target_flags) && TARGET_CPU_Z196_P (opts))
#define TARGET_ZEC12 \
	(TARGET_ZARCH && TARGET_CPU_ZEC12)
#define TARGET_ZEC12_P(opts) \
	(TARGET_ZARCH_P (opts->x_target_flags) && TARGET_CPU_ZEC12_P (opts))
#define TARGET_HTM (TARGET_OPT_HTM)
#define TARGET_HTM_P(opts) (TARGET_OPT_HTM_P (opts->x_target_flags))
#define TARGET_Z13 \
	(TARGET_ZARCH && TARGET_CPU_Z13)
#define TARGET_Z13_P(opts) \
	(TARGET_ZARCH_P (opts->x_target_flags) && TARGET_CPU_Z13_P (opts))
#define TARGET_VX \
	(TARGET_ZARCH && TARGET_CPU_VX && TARGET_OPT_VX && TARGET_HARD_FLOAT)
#define TARGET_VX_P(opts) \
	(TARGET_ZARCH_P (opts->x_target_flags) && TARGET_CPU_VX_P (opts) \
	 && TARGET_OPT_VX_P (opts->x_target_flags) \
	 && TARGET_HARD_FLOAT_P (opts->x_target_flags))
#define TARGET_Z14 (TARGET_ZARCH && TARGET_CPU_Z14)
#define TARGET_Z14_P(opts)						\
	(TARGET_ZARCH_P (opts->x_target_flags) && TARGET_CPU_Z14_P (opts))
#define TARGET_VXE				\
	(TARGET_VX && TARGET_CPU_VXE)
#define TARGET_VXE_P(opts)						\
	(TARGET_VX_P (opts) && TARGET_CPU_VXE_P (opts))
#define TARGET_Z15 (TARGET_ZARCH && TARGET_CPU_Z15)
#define TARGET_Z15_P(opts)						\
	(TARGET_ZARCH_P (opts->x_target_flags) && TARGET_CPU_Z15_P (opts))
#define TARGET_VXE2					\
	(TARGET_VX && TARGET_CPU_VXE2)
#define TARGET_VXE2_P(opts)						\
	(TARGET_VX_P (opts) && TARGET_CPU_VXE2_P (opts))
#define TARGET_Z16 (TARGET_ZARCH && TARGET_CPU_Z16)
#define TARGET_Z16_P(opts)						\
	(TARGET_ZARCH_P (opts->x_target_flags) && TARGET_CPU_Z16_P (opts))
#define TARGET_NNPA					\
	(TARGET_ZARCH && TARGET_CPU_NNPA)
#define TARGET_NNPA_P(opts)						\
	(TARGET_ZARCH_P (opts) && TARGET_CPU_NNPA_P (opts))

#if defined(HAVE_AS_VECTOR_LOADSTORE_ALIGNMENT_HINTS_ON_Z13)
#define TARGET_VECTOR_LOADSTORE_ALIGNMENT_HINTS TARGET_Z13
#elif defined(HAVE_AS_VECTOR_LOADSTORE_ALIGNMENT_HINTS)
#define TARGET_VECTOR_LOADSTORE_ALIGNMENT_HINTS TARGET_Z14
#else
#define TARGET_VECTOR_LOADSTORE_ALIGNMENT_HINTS 0
#endif

/* Evaluate to true if it is ok to emit a non-signaling vector
   comparison.  */
#define TARGET_NONSIGNALING_VECTOR_COMPARE_OK \
  (TARGET_VX && !TARGET_VXE && (flag_finite_math_only || !flag_trapping_math))

#ifdef HAVE_AS_MACHINE_MACHINEMODE
#define S390_USE_TARGET_ATTRIBUTE 1
#else
#define S390_USE_TARGET_ATTRIBUTE 0
#endif

#ifdef HAVE_AS_ARCHITECTURE_MODIFIERS
#define S390_USE_ARCHITECTURE_MODIFIERS 1
#else
#define S390_USE_ARCHITECTURE_MODIFIERS 0
#endif

#if S390_USE_TARGET_ATTRIBUTE
/* For switching between functions with different target attributes.  */
#define SWITCHABLE_TARGET 1
#endif

#define TARGET_SUPPORTS_WIDE_INT 1

/* Use the ABI introduced with IBM z13:
   - pass vector arguments <= 16 bytes in VRs
   - align *all* vector types to 8 bytes  */
#define TARGET_VX_ABI TARGET_VX

#define TARGET_AVOID_CMP_AND_BRANCH (s390_tune == PROCESSOR_2817_Z196)

/* Issue a write prefetch for the +4 cache line.  */
#define TARGET_SETMEM_PREFETCH_DISTANCE 1024

/* Expand to a C expressions evaluating to true if a setmem to VAL of
   length LEN should be emitted using prefetch instructions.  */
#define TARGET_SETMEM_PFD(VAL,LEN)					\
  (TARGET_Z10								\
   && (s390_tune < PROCESSOR_2964_Z13 || (VAL) != const0_rtx)		\
   && (!CONST_INT_P (LEN) || INTVAL ((LEN)) > TARGET_SETMEM_PREFETCH_DISTANCE))

/* Run-time target specification.  */

/* Defaults for option flags defined only on some subtargets.  */
#ifndef TARGET_TPF_PROFILING
#define TARGET_TPF_PROFILING 0
#endif

/* This will be overridden by OS headers.  */
#define TARGET_TPF 0

/* Target CPU builtins.  */
#define TARGET_CPU_CPP_BUILTINS() s390_cpu_cpp_builtins (pfile)

#ifdef DEFAULT_TARGET_64BIT
#define TARGET_DEFAULT     (MASK_64BIT | MASK_ZARCH | MASK_HARD_DFP	\
			    | MASK_OPT_HTM | MASK_OPT_VX)
#else
#define TARGET_DEFAULT             0
#endif

/* Support for configure-time defaults.
   The order here is important so that -march doesn't squash the
   tune values.  */
#define OPTION_DEFAULT_SPECS					\
  { "mode", "%{!mesa:%{!mzarch:-m%(VALUE)}}" },			\
  { "tune", "%{!mtune=*:%{!march=*:-mtune=%(VALUE)}}" },	\
  { "arch", "%{!march=*:-march=%(VALUE)}" }

#ifdef __s390__
extern const char *s390_host_detect_local_cpu (int argc, const char **argv);
# define EXTRA_SPEC_FUNCTIONS \
  { "local_cpu_detect", s390_host_detect_local_cpu },

#define MARCH_MTUNE_NATIVE_SPECS				\
  "%{mtune=native:%<mtune=native %:local_cpu_detect(tune)} "	\
  "%{march=native:%<march=native"				\
  " %:local_cpu_detect(arch %{mesa|mzarch:mesa_mzarch})}"
#else
# define MARCH_MTUNE_NATIVE_SPECS ""
#endif

#ifdef DEFAULT_TARGET_64BIT
#define S390_TARGET_BITS_STRING "64"
#else
#define S390_TARGET_BITS_STRING "31"
#endif

/* Defaulting rules.  */
#define DRIVER_SELF_SPECS					\
  MARCH_MTUNE_NATIVE_SPECS,					\
  "%{!m31:%{!m64:-m" S390_TARGET_BITS_STRING "}}",		\
  "%{!mesa:%{!mzarch:%{m31:-mesa}%{m64:-mzarch}}}",		\
  "%{!march=*:-march=z900}"

/* Constants needed to control the TEST DATA CLASS (TDC) instruction.  */
#define S390_TDC_POSITIVE_ZERO                     (1 << 11)
#define S390_TDC_NEGATIVE_ZERO                     (1 << 10)
#define S390_TDC_POSITIVE_NORMALIZED_BFP_NUMBER    (1 << 9)
#define S390_TDC_NEGATIVE_NORMALIZED_BFP_NUMBER    (1 << 8)
#define S390_TDC_POSITIVE_DENORMALIZED_BFP_NUMBER  (1 << 7)
#define S390_TDC_NEGATIVE_DENORMALIZED_BFP_NUMBER  (1 << 6)
#define S390_TDC_POSITIVE_INFINITY                 (1 << 5)
#define S390_TDC_NEGATIVE_INFINITY                 (1 << 4)
#define S390_TDC_POSITIVE_QUIET_NAN                (1 << 3)
#define S390_TDC_NEGATIVE_QUIET_NAN                (1 << 2)
#define S390_TDC_POSITIVE_SIGNALING_NAN            (1 << 1)
#define S390_TDC_NEGATIVE_SIGNALING_NAN            (1 << 0)

/* The following values are different for DFP.  */
#define S390_TDC_POSITIVE_DENORMALIZED_DFP_NUMBER (1 << 9)
#define S390_TDC_NEGATIVE_DENORMALIZED_DFP_NUMBER (1 << 8)
#define S390_TDC_POSITIVE_NORMALIZED_DFP_NUMBER   (1 << 7)
#define S390_TDC_NEGATIVE_NORMALIZED_DFP_NUMBER   (1 << 6)

/* For signbit, the BFP-DFP-difference makes no difference. */
#define S390_TDC_SIGNBIT_SET (S390_TDC_NEGATIVE_ZERO \
			  | S390_TDC_NEGATIVE_NORMALIZED_BFP_NUMBER \
			  | S390_TDC_NEGATIVE_DENORMALIZED_BFP_NUMBER\
			  | S390_TDC_NEGATIVE_INFINITY \
			  | S390_TDC_NEGATIVE_QUIET_NAN \
			  | S390_TDC_NEGATIVE_SIGNALING_NAN )

#define S390_TDC_INFINITY (S390_TDC_POSITIVE_INFINITY \
			  | S390_TDC_NEGATIVE_INFINITY )

/* Target machine storage layout.  */

/* Everything is big-endian.  */
#define BITS_BIG_ENDIAN 1
#define BYTES_BIG_ENDIAN 1
#define WORDS_BIG_ENDIAN 1

#define STACK_SIZE_MODE (Pmode)

/* Make the stack pointer to be moved downwards while issuing stack probes with
   -fstack-check.  We need this to prevent memory below the stack pointer from
   being accessed.  */
#define STACK_CHECK_MOVING_SP 1

#ifndef IN_LIBGCC2

/* Width of a word, in units (bytes).  */
  #define UNITS_PER_WORD (TARGET_ZARCH ? 8 : 4)

/* Width of a pointer.  To be used instead of UNITS_PER_WORD in
   ABI-relevant contexts.  This always matches
   GET_MODE_SIZE (Pmode).  */
  #define UNITS_PER_LONG (TARGET_64BIT ? 8 : 4)
  #define MIN_UNITS_PER_WORD 4
  #define MAX_BITS_PER_WORD 64
#else

  /* In libgcc, UNITS_PER_WORD has ABI-relevant effects, e.g. whether
     the library should export TImode functions or not.  Thus, we have
     to redefine UNITS_PER_WORD depending on __s390x__ for libgcc.  */
  #ifdef __s390x__
    #define UNITS_PER_WORD 8
  #else
    #define UNITS_PER_WORD 4
  #endif
#endif

/* Width of a pointer, in bits.  */
#define POINTER_SIZE (TARGET_64BIT ? 64 : 32)

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY (TARGET_64BIT ? 64 : 32)

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY 64

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 64

/* There is no point aligning anything to a rounder boundary than this.  */
#define BIGGEST_ALIGNMENT 64

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Alignment on even addresses for LARL instruction.  */
#define DATA_ABI_ALIGNMENT(TYPE, ALIGN) (ALIGN) < 16 ? 16 : (ALIGN)

/* Alignment is not required by the hardware.  */
#define STRICT_ALIGNMENT 0

/* Mode of stack savearea.
   FUNCTION is VOIDmode because calling convention maintains SP.
   BLOCK needs Pmode for SP.
   NONLOCAL needs twice Pmode to maintain both backchain and SP.  */
#define STACK_SAVEAREA_MODE(LEVEL)					\
  ((LEVEL) == SAVE_FUNCTION ? VOIDmode					\
   : (LEVEL) == SAVE_NONLOCAL ? (TARGET_64BIT ? OImode : TImode) : Pmode)


/* Type layout.  */

/* Sizes in bits of the source language data types.  */
#define SHORT_TYPE_SIZE 16
#define INT_TYPE_SIZE 32
#define LONG_TYPE_SIZE (TARGET_64BIT ? 64 : 32)
#define LONG_LONG_TYPE_SIZE 64
#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 64
#define LONG_DOUBLE_TYPE_SIZE (TARGET_LONG_DOUBLE_128 ? 128 : 64)

/* Work around target_flags dependency in ada/targtyps.cc.  */
#define WIDEST_HARDWARE_FP_SIZE 64

/* We use "unsigned char" as default.  */
#define DEFAULT_SIGNED_CHAR 0


/* Register usage.  */

/* We have 16 general purpose registers (registers 0-15),
   and 16 floating point registers (registers 16-31).
   (On non-IEEE machines, we have only 4 fp registers.)

   Amongst the general purpose registers, some are used
   for specific purposes:
   GPR 11: Hard frame pointer (if needed)
   GPR 12: Global offset table pointer (if needed)
   GPR 13: Literal pool base register
   GPR 14: Return address register
   GPR 15: Stack pointer

   Registers 32-35 are 'fake' hard registers that do not
   correspond to actual hardware:
   Reg 32: Argument pointer
   Reg 33: Condition code
   Reg 34: Frame pointer
   Reg 35: Return address pointer

   Registers 36 and 37 are mapped to access registers
   0 and 1, used to implement thread-local storage.

   Reg 38-53: Vector registers v16-v31  */

#define FIRST_PSEUDO_REGISTER 54

/* Standard register usage.  */
#define GENERAL_REGNO_P(N)	((int)(N) >= 0 && (N) < 16)
#define ADDR_REGNO_P(N)		((N) >= 1 && (N) < 16)
#define FP_REGNO_P(N)		((N) >= 16 && (N) < 32)
#define CC_REGNO_P(N)		((N) == 33)
#define FRAME_REGNO_P(N)	((N) == 32 || (N) == 34 || (N) == 35)
#define ACCESS_REGNO_P(N)	((N) == 36 || (N) == 37)
#define VECTOR_NOFP_REGNO_P(N)  ((N) >= 38 && (N) <= 53)
#define VECTOR_REGNO_P(N)       (FP_REGNO_P (N) || VECTOR_NOFP_REGNO_P (N))

#define GENERAL_REG_P(X)	(REG_P (X) && GENERAL_REGNO_P (REGNO (X)))
#define ADDR_REG_P(X)		(REG_P (X) && ADDR_REGNO_P (REGNO (X)))
#define FP_REG_P(X)		(REG_P (X) && FP_REGNO_P (REGNO (X)))
#define CC_REG_P(X)		(REG_P (X) && CC_REGNO_P (REGNO (X)))
#define FRAME_REG_P(X)		(REG_P (X) && FRAME_REGNO_P (REGNO (X)))
#define ACCESS_REG_P(X)		(REG_P (X) && ACCESS_REGNO_P (REGNO (X)))
#define VECTOR_NOFP_REG_P(X)    (REG_P (X) && VECTOR_NOFP_REGNO_P (REGNO (X)))
#define VECTOR_REG_P(X)         (REG_P (X) && VECTOR_REGNO_P (REGNO (X)))

/* Set up fixed registers and calling convention:

   GPRs 0-5 are always call-clobbered,
   GPRs 6-15 are always call-saved.
   GPR 12 is fixed if used as GOT pointer.
   GPR 13 is always fixed (as literal pool pointer).
   GPR 14 is always fixed on S/390 machines (as return address).
   GPR 15 is always fixed (as stack pointer).
   The 'fake' hard registers are call-clobbered and fixed.
   The access registers are call-saved and fixed.

   On 31-bit, FPRs 18-19 are call-clobbered;
   on 64-bit, FPRs 24-31 are call-clobbered.
   The remaining FPRs are call-saved.

   All non-FP vector registers are call-clobbered v16-v31.  */

#define FIXED_REGISTERS				\
{ 0, 0, 0, 0,					\
  0, 0, 0, 0,					\
  0, 0, 0, 0,					\
  0, 1, 1, 1,					\
  0, 0, 0, 0,					\
  0, 0, 0, 0,					\
  0, 0, 0, 0,					\
  0, 0, 0, 0,					\
  1, 1, 1, 1,					\
  1, 1,						\
  0, 0, 0, 0,					\
  0, 0, 0, 0,					\
  0, 0, 0, 0,					\
  0, 0, 0, 0 }

#define CALL_REALLY_USED_REGISTERS		\
{ 1, 1, 1, 1,	/* r0 - r15 */			\
  1, 1, 0, 0,					\
  0, 0, 0, 0,					\
  0, 0, 0, 0,					\
  1, 1, 1, 1,	/* f0 (16) - f15 (31) */	\
  1, 1, 1, 1,					\
  1, 1, 1, 1,					\
  1, 1, 1, 1,					\
  1, 1, 1, 1,	/* arg, cc, fp, ret addr */	\
  0, 0,		/* a0 (36), a1 (37) */		\
  1, 1, 1, 1,	/* v16 (38) - v23 (45) */	\
  1, 1, 1, 1,					\
  1, 1, 1, 1,	/* v24 (46) - v31 (53) */	\
  1, 1, 1, 1 }

/* Preferred register allocation order.  */
#define REG_ALLOC_ORDER							\
  {  1, 2, 3, 4, 5, 0, 12, 11, 10, 9, 8, 7, 6, 14, 13,			\
     16, 17, 18, 19, 20, 21, 22, 23,					\
     24, 25, 26, 27, 28, 29, 30, 31,					\
     38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53,	\
     15, 32, 33, 34, 35, 36, 37 }


#define HARD_REGNO_RENAME_OK(FROM, TO)          \
  s390_hard_regno_rename_ok ((FROM), (TO))

/* Maximum number of registers to represent a value of mode MODE
   in a register of class CLASS.  */
#define CLASS_MAX_NREGS(CLASS, MODE)					\
  s390_class_max_nregs ((CLASS), (MODE))

/* We can reverse a CC mode safely if we know whether it comes from a
   floating point compare or not.  With the vector modes it is encoded
   as part of the mode.
   FIXME: It might make sense to do this for other cc modes as well.  */
#define REVERSIBLE_CC_MODE(MODE)				\
  ((MODE) == CCVIALLmode || (MODE) == CCVIANYmode		\
   || (MODE) == CCVFALLmode || (MODE) == CCVFANYmode)

/* Given a condition code and a mode, return the inverse condition.  */
#define REVERSE_CONDITION(CODE, MODE) s390_reverse_condition (MODE, CODE)


/* Register classes.  */

/* We use the following register classes:
   GENERAL_REGS     All general purpose registers
   ADDR_REGS        All general purpose registers except %r0
		    (These registers can be used in address generation)
   FP_REGS          All floating point registers
   CC_REGS          The condition code register
   ACCESS_REGS      The access registers

   GENERAL_FP_REGS  Union of GENERAL_REGS and FP_REGS
   ADDR_FP_REGS     Union of ADDR_REGS and FP_REGS
   GENERAL_CC_REGS  Union of GENERAL_REGS and CC_REGS
   ADDR_CC_REGS     Union of ADDR_REGS and CC_REGS

   NO_REGS          No registers
   ALL_REGS         All registers

   Note that the 'fake' frame pointer and argument pointer registers
   are included amongst the address registers here.  */

enum reg_class
{
  NO_REGS, CC_REGS, ADDR_REGS, GENERAL_REGS, ACCESS_REGS,
  ADDR_CC_REGS, GENERAL_CC_REGS,
  FP_REGS, ADDR_FP_REGS, GENERAL_FP_REGS,
  VEC_REGS, ADDR_VEC_REGS, GENERAL_VEC_REGS,
  ALL_REGS, LIM_REG_CLASSES
};
#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define REG_CLASS_NAMES							\
{ "NO_REGS", "CC_REGS", "ADDR_REGS", "GENERAL_REGS", "ACCESS_REGS",	\
  "ADDR_CC_REGS", "GENERAL_CC_REGS",					\
  "FP_REGS", "ADDR_FP_REGS", "GENERAL_FP_REGS",				\
  "VEC_REGS", "ADDR_VEC_REGS", "GENERAL_VEC_REGS",			\
  "ALL_REGS" }

/* Class -> register mapping.  */
#define REG_CLASS_CONTENTS				\
{							\
  { 0x00000000, 0x00000000 },	/* NO_REGS */		\
  { 0x00000000, 0x00000002 },	/* CC_REGS */		\
  { 0x0000fffe, 0x0000000d },	/* ADDR_REGS */		\
  { 0x0000ffff, 0x0000000d },	/* GENERAL_REGS */	\
  { 0x00000000, 0x00000030 },	/* ACCESS_REGS */	\
  { 0x0000fffe, 0x0000000f },	/* ADDR_CC_REGS */	\
  { 0x0000ffff, 0x0000000f },	/* GENERAL_CC_REGS */	\
  { 0xffff0000, 0x00000000 },	/* FP_REGS */		\
  { 0xfffffffe, 0x0000000d },	/* ADDR_FP_REGS */	\
  { 0xffffffff, 0x0000000d },	/* GENERAL_FP_REGS */	\
  { 0xffff0000, 0x003fffc0 },	/* VEC_REGS */		\
  { 0xfffffffe, 0x003fffcd },	/* ADDR_VEC_REGS */	\
  { 0xffffffff, 0x003fffcd },	/* GENERAL_VEC_REGS */	\
  { 0xffffffff, 0x003fffff },	/* ALL_REGS */		\
}

/* In some case register allocation order is not enough for IRA to
   generate a good code.  The following macro (if defined) increases
   cost of REGNO for a pseudo approximately by pseudo usage frequency
   multiplied by the macro value.

   We avoid usage of BASE_REGNUM by nonzero macro value because the
   reload can decide not to use the hard register because some
   constant was forced to be in memory.  */
#define IRA_HARD_REGNO_ADD_COST_MULTIPLIER(regno)	\
  ((regno) != BASE_REGNUM ? 0.0 : 0.5)

/* Register -> class mapping.  */
extern const enum reg_class regclass_map[FIRST_PSEUDO_REGISTER];
#define REGNO_REG_CLASS(REGNO) (regclass_map[REGNO])

/* ADDR_REGS can be used as base or index register.  */
#define INDEX_REG_CLASS ADDR_REGS
#define BASE_REG_CLASS ADDR_REGS

/* Check whether REGNO is a hard register of the suitable class
   or a pseudo register currently allocated to one such.  */
#define REGNO_OK_FOR_INDEX_P(REGNO)					\
    (((REGNO) < FIRST_PSEUDO_REGISTER					\
      && REGNO_REG_CLASS ((REGNO)) == ADDR_REGS)			\
     || ADDR_REGNO_P (reg_renumber[REGNO]))
#define REGNO_OK_FOR_BASE_P(REGNO) REGNO_OK_FOR_INDEX_P (REGNO)


/* Stack layout and calling conventions.  */

/* Our stack grows from higher to lower addresses.  However, local variables
   are accessed by positive offsets, and function arguments are stored at
   increasing addresses.  */
#define STACK_GROWS_DOWNWARD 1
#define FRAME_GROWS_DOWNWARD 1
/* #undef ARGS_GROW_DOWNWARD */

/* The basic stack layout looks like this: the stack pointer points
   to the register save area for called functions.  Above that area
   is the location to place outgoing arguments.  Above those follow
   dynamic allocations (alloca), and finally the local variables.  */

/* Offset from stack-pointer to first location of outgoing args.  */
#define STACK_POINTER_OFFSET (TARGET_64BIT ? 160 : 96)

/* Offset from the stack pointer register to an item dynamically
   allocated on the stack, e.g., by `alloca'.  */
#define STACK_DYNAMIC_OFFSET(FUNDECL) \
  (STACK_POINTER_OFFSET + crtl->outgoing_args_size)

/* Offset of first parameter from the argument pointer register value.
   We have a fake argument pointer register that points directly to
   the argument area.  */
#define FIRST_PARM_OFFSET(FNDECL) 0

/* Defining this macro makes __builtin_frame_address(0) and
   __builtin_return_address(0) work with -fomit-frame-pointer.  */
#define INITIAL_FRAME_ADDRESS_RTX                                             \
  (plus_constant (Pmode, arg_pointer_rtx, -STACK_POINTER_OFFSET))

/* The return address of the current frame is retrieved
   from the initial value of register RETURN_REGNUM.
   For frames farther back, we use the stack slot where
   the corresponding RETURN_REGNUM register was saved.  */
#define DYNAMIC_CHAIN_ADDRESS(FRAME)                                          \
  (TARGET_PACKED_STACK ?                                                      \
   plus_constant (Pmode, (FRAME),					      \
		  STACK_POINTER_OFFSET - UNITS_PER_LONG) : (FRAME))

/* For -mpacked-stack this adds 160 - 8 (96 - 4) to the output of
   builtin_frame_address.  Otherwise arg pointer -
   STACK_POINTER_OFFSET would be returned for
   __builtin_frame_address(0) what might result in an address pointing
   somewhere into the middle of the local variables since the packed
   stack layout generally does not need all the bytes in the register
   save area.  */
#define FRAME_ADDR_RTX(FRAME)			\
  DYNAMIC_CHAIN_ADDRESS ((FRAME))

#define RETURN_ADDR_RTX(COUNT, FRAME)					      \
  s390_return_addr_rtx ((COUNT), DYNAMIC_CHAIN_ADDRESS ((FRAME)))

/* In 31-bit mode, we need to mask off the high bit of return addresses.  */
#define MASK_RETURN_ADDR (TARGET_64BIT ? constm1_rtx : GEN_INT (0x7fffffff))


/* Exception handling.  */

/* Describe calling conventions for DWARF-2 exception handling.  */
#define INCOMING_RETURN_ADDR_RTX  gen_rtx_REG (Pmode, RETURN_REGNUM)
#define INCOMING_FRAME_SP_OFFSET STACK_POINTER_OFFSET
#define DWARF_FRAME_RETURN_COLUMN  14

/* Describe how we implement __builtin_eh_return.  */
#define EH_RETURN_DATA_REGNO(N) ((N) < 4 ? (N) + 6 : INVALID_REGNUM)
#define EH_RETURN_HANDLER_RTX gen_rtx_MEM (Pmode, return_address_pointer_rtx)

/* Select a format to encode pointers in exception handling data.  */
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE, GLOBAL)			    \
  (flag_pic								    \
    ? ((GLOBAL) ? DW_EH_PE_indirect : 0) | DW_EH_PE_pcrel | DW_EH_PE_sdata4 \
   : DW_EH_PE_absptr)

/* Register save slot alignment.  */
#define DWARF_CIE_DATA_ALIGNMENT (-UNITS_PER_LONG)

/* Let the assembler generate debug line info.  */
#define DWARF2_ASM_LINE_DEBUG_INFO 1

/* Define the dwarf register mapping.
   v16-v31 -> 68-83
   rX      -> X      otherwise  */
#define DEBUGGER_REGNO(regno)				\
  (((regno) >= 38 && (regno) <= 53) ? (regno) + 30 : (regno))

/* Frame registers.  */

#define STACK_POINTER_REGNUM 15
#define FRAME_POINTER_REGNUM 34
#define HARD_FRAME_POINTER_REGNUM 11
#define ARG_POINTER_REGNUM 32
#define RETURN_ADDRESS_POINTER_REGNUM 35

/* The static chain must be call-clobbered, but not used for
   function argument passing.  As register 1 is clobbered by
   the trampoline code, we only have one option.  */
#define STATIC_CHAIN_REGNUM 0

/* Number of hardware registers that go into the DWARF-2 unwind info.
   To avoid ABI incompatibility, this number must not change even as
   'fake' hard registers are added or removed.  */
#define DWARF_FRAME_REGISTERS 34


/* Frame pointer and argument pointer elimination.  */

#define ELIMINABLE_REGS						\
{{ FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM },		\
 { FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM },		\
 { ARG_POINTER_REGNUM, STACK_POINTER_REGNUM },			\
 { ARG_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM },		\
 { RETURN_ADDRESS_POINTER_REGNUM, STACK_POINTER_REGNUM },	\
 { RETURN_ADDRESS_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM },	\
 { BASE_REGNUM, BASE_REGNUM }}

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  (OFFSET) = s390_initial_elimination_offset ((FROM), (TO))


/* Stack arguments.  */

/* We need current_function_outgoing_args to be valid.  */
#define ACCUMULATE_OUTGOING_ARGS 1


/* Register arguments.  */

typedef struct s390_arg_structure
{
  int gprs;			/* gpr so far */
  int fprs;			/* fpr so far */
  int vrs;                      /* vr so far */
}
CUMULATIVE_ARGS;

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, NN, N_NAMED_ARGS) \
  ((CUM).gprs=0, (CUM).fprs=0, (CUM).vrs=0)

#define FIRST_VEC_ARG_REGNO 46
#define LAST_VEC_ARG_REGNO 53

/* Arguments can be placed in general registers 2 to 6, or in floating
   point registers 0 and 2 for 31 bit and fprs 0, 2, 4 and 6 for 64
   bit.  */
#define FUNCTION_ARG_REGNO_P(N)						\
  (((N) >=2 && (N) < 7) || (N) == 16 || (N) == 17			\
   || (TARGET_64BIT && ((N) == 18 || (N) == 19))			\
   || (TARGET_VX && ((N) >= FIRST_VEC_ARG_REGNO && (N) <= LAST_VEC_ARG_REGNO)))


/* Only gpr 2, fpr 0, and v24 are ever used as return registers.  */
#define FUNCTION_VALUE_REGNO_P(N)		\
  ((N) == 2 || (N) == 16			\
   || (TARGET_VX && (N) == FIRST_VEC_ARG_REGNO))


/* Function entry and exit.  */

/* When returning from a function, the stack pointer does not matter.  */
#define EXIT_IGNORE_STACK       1


/* Profiling.  */

#define FUNCTION_PROFILER(FILE, LABELNO)			\
  s390_function_profiler ((FILE), ((LABELNO)))

#define PROFILE_BEFORE_PROLOGUE 1

#define NO_PROFILE_COUNTERS 1


/* Trampolines for nested functions.  */

#define TRAMPOLINE_SIZE		(TARGET_64BIT ? 32 : 16)
#define TRAMPOLINE_ALIGNMENT	BITS_PER_WORD

/* Addressing modes, and classification of registers for them.  */

/* Recognize any constant value that is a valid address.  */
#define CONSTANT_ADDRESS_P(X) 0

/* Maximum number of registers that can appear in a valid memory address.  */
#define MAX_REGS_PER_ADDRESS 2

/* This definition replaces the formerly used 'm' constraint with a
   different constraint letter in order to avoid changing semantics of
   the 'm' constraint when accepting new address formats in
   TARGET_LEGITIMATE_ADDRESS_P.  The constraint letter defined here
   must not be used in insn definitions or inline assemblies.  */
#define TARGET_MEM_CONSTRAINT 'e'

/* Try a machine-dependent way of reloading an illegitimate address
   operand.  If we find one, push the reload and jump to WIN.  This
   macro is used in only one place: `find_reloads_address' in reload.cc.  */
#define LEGITIMIZE_RELOAD_ADDRESS(AD, MODE, OPNUM, TYPE, IND, WIN)	\
  do {									\
    rtx new_rtx = legitimize_reload_address ((AD), (MODE),		\
					     (OPNUM), (int)(TYPE));	\
    if (new_rtx)							\
      {									\
	(AD) = new_rtx;							\
	goto WIN;							\
      }									\
  } while (0)

/* Helper macro for s390.cc and s390.md to check for symbolic constants.  */
#define SYMBOLIC_CONST(X)						\
  (GET_CODE (X) == SYMBOL_REF						\
   || GET_CODE (X) == LABEL_REF						\
   || (GET_CODE (X) == CONST && symbolic_reference_mentioned_p (X)))

#define TLS_SYMBOLIC_CONST(X)						\
  ((GET_CODE (X) == SYMBOL_REF && tls_symbolic_operand (X))		\
   || (GET_CODE (X) == CONST && tls_symbolic_reference_mentioned_p (X)))


/* Condition codes.  */

/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
   return the mode to be used for the comparison.  */
#define SELECT_CC_MODE(OP, X, Y) s390_select_ccmode ((OP), (X), (Y))

/* Relative costs of operations.  */

/* A C expression for the cost of a branch instruction.  A value of 1
   is the default; other values are interpreted relative to that.  */
#define BRANCH_COST(speed_p, predictable_p) s390_branch_cost

/* Nonzero if access to memory by bytes is slow and undesirable.  */
#define SLOW_BYTE_ACCESS 1

/* An integer expression for the size in bits of the largest integer machine
   mode that should actually be used.  We allow pairs of registers.  */
#define MAX_FIXED_MODE_SIZE GET_MODE_BITSIZE (TARGET_64BIT ? TImode : DImode)

/* The maximum number of bytes that a single instruction can move quickly
   between memory and registers or between two memory locations.  */
#define MOVE_MAX (TARGET_ZARCH ? 16 : 8)
#define MOVE_MAX_PIECES (TARGET_ZARCH ? 8 : 4)
#define MAX_MOVE_MAX 16

/* Don't perform CSE on function addresses.  */
#define NO_FUNCTION_CSE 1

/* This value is used in tree-sra to decide whether it might benefical
   to split a struct move into several word-size moves.  For S/390
   only small values make sense here since struct moves are relatively
   cheap thanks to mvc so the small default value chosen for archs
   with memmove patterns should be ok.  But this value is multiplied
   in tree-sra with UNITS_PER_WORD to make a decision so we adjust it
   here to compensate for that factor since mvc costs exactly the same
   on 31 and 64 bit.  */
#define MOVE_RATIO(speed) (TARGET_64BIT? 2 : 4)


/* Sections.  */

/* Output before read-only data.  */
#define TEXT_SECTION_ASM_OP ".text"

/* Output before writable (initialized) data.  */
#define DATA_SECTION_ASM_OP ".data"

/* Output before writable (uninitialized) data.  */
#define BSS_SECTION_ASM_OP ".bss"

/* S/390 constant pool breaks the devices in crtstuff.c to control section
   in where code resides.  We have to write it as asm code.  */
#ifndef __s390x__
#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC) \
    asm (SECTION_OP "\n\
	bras\t%r2,1f\n\
0:	.long\t" USER_LABEL_PREFIX #FUNC " - 0b\n\
1:	l\t%r3,0(%r2)\n\
	bas\t%r14,0(%r3,%r2)\n\
	.previous");
#endif


/* Position independent code.  */

#define PIC_OFFSET_TABLE_REGNUM (flag_pic ? 12 : INVALID_REGNUM)

#define LEGITIMATE_PIC_OPERAND_P(X)  legitimate_pic_operand_p (X)

#ifndef TARGET_DEFAULT_PIC_DATA_IS_TEXT_RELATIVE
#define TARGET_DEFAULT_PIC_DATA_IS_TEXT_RELATIVE 1
#endif


/* Assembler file format.  */

/* Character to start a comment.  */
#define ASM_COMMENT_START "#"

/* Declare an uninitialized external linkage data object.  */
#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN)		\
  asm_output_aligned_bss ((FILE), (DECL), (NAME), (SIZE), (ALIGN))

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP ".globl "

/* Advance the location counter to a multiple of 2**LOG bytes.  */
#define ASM_OUTPUT_ALIGN(FILE, LOG) \
  if ((LOG)) fprintf ((FILE), "\t.align\t%d\n", 1 << (LOG))

/* Advance the location counter by SIZE bytes.  */
#define ASM_OUTPUT_SKIP(FILE, SIZE) \
  fprintf ((FILE), "\t.set\t.,.+" HOST_WIDE_INT_PRINT_UNSIGNED"\n", (SIZE))

/* The LOCAL_LABEL_PREFIX variable is used by dbxelf.h.  */
#define LOCAL_LABEL_PREFIX "."

#define LABEL_ALIGN(LABEL) \
  s390_label_align ((LABEL))

/* How to refer to registers in assembler output.  This sequence is
   indexed by compiler's hard-register-number (see above).  */
#define REGISTER_NAMES							\
  { "%r0",  "%r1",  "%r2",  "%r3",  "%r4",  "%r5",  "%r6",  "%r7",	\
    "%r8",  "%r9",  "%r10", "%r11", "%r12", "%r13", "%r14", "%r15",	\
    "%f0",  "%f2",  "%f4",  "%f6",  "%f1",  "%f3",  "%f5",  "%f7",	\
    "%f8",  "%f10", "%f12", "%f14", "%f9",  "%f11", "%f13", "%f15",	\
    "%ap",  "%cc",  "%fp",  "%rp",  "%a0",  "%a1",			\
    "%v16", "%v18", "%v20", "%v22", "%v17", "%v19", "%v21", "%v23",	\
    "%v24", "%v26", "%v28", "%v30", "%v25", "%v27", "%v29", "%v31"	\
  }

#define ADDITIONAL_REGISTER_NAMES					\
  { { "v0", 16 }, { "v2",  17 }, { "v4",  18 }, { "v6",  19 },		\
    { "v1", 20 }, { "v3",  21 }, { "v5",  22 }, { "v7",  23 },          \
    { "v8", 24 }, { "v10", 25 }, { "v12", 26 }, { "v14", 27 },          \
    { "v9", 28 }, { "v11", 29 }, { "v13", 30 }, { "v15", 31 } };

/* Print operand X (an rtx) in assembler syntax to file FILE.  */
#define PRINT_OPERAND(FILE, X, CODE) print_operand ((FILE), (X), (CODE))
#define PRINT_OPERAND_ADDRESS(FILE, ADDR) print_operand_address ((FILE), (ADDR))

/* Output an element of a case-vector that is absolute.  */
#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)				\
do {									\
  char buf[32];								\
  fputs (integer_asm_op (UNITS_PER_LONG, TRUE), (FILE));		\
  ASM_GENERATE_INTERNAL_LABEL (buf, "L", (VALUE));			\
  assemble_name ((FILE), buf);						\
  fputc ('\n', (FILE));							\
} while (0)

/* Output an element of a case-vector that is relative.  */
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL)		\
do {									\
  char buf[32];								\
  fputs (integer_asm_op (UNITS_PER_LONG, TRUE), (FILE));		\
  ASM_GENERATE_INTERNAL_LABEL (buf, "L", (VALUE));			\
  assemble_name ((FILE), buf);						\
  fputc ('-', (FILE));							\
  ASM_GENERATE_INTERNAL_LABEL (buf, "L", (REL));			\
  assemble_name ((FILE), buf);						\
  fputc ('\n', (FILE));							\
} while (0)

/* Mark the return register as used by the epilogue so that we can
   use it in unadorned (return) and (simple_return) instructions.  */
#define EPILOGUE_USES(REGNO) ((REGNO) == RETURN_REGNUM)

#undef ASM_OUTPUT_FUNCTION_LABEL
#define ASM_OUTPUT_FUNCTION_LABEL(FILE, NAME, DECL)		\
  s390_asm_output_function_label ((FILE), (NAME), (DECL))

#if S390_USE_TARGET_ATTRIBUTE
/* Hook to output .machine and .machinemode at start of function.  */
#undef ASM_OUTPUT_FUNCTION_PREFIX
#define ASM_OUTPUT_FUNCTION_PREFIX s390_asm_output_function_prefix

/* Hook to output .machine and .machinemode at end of function.  */
#undef ASM_DECLARE_FUNCTION_SIZE
#define ASM_DECLARE_FUNCTION_SIZE s390_asm_declare_function_size
#endif

/* Miscellaneous parameters.  */

/* Specify the machine mode that this machine uses for the index in the
   tablejump instruction.  */
#define CASE_VECTOR_MODE (TARGET_64BIT ? DImode : SImode)

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode (TARGET_64BIT ? DImode : SImode)

/* This is -1 for "pointer mode" extend.  See ptr_extend in s390.md.  */
#define POINTERS_EXTEND_UNSIGNED -1

/* A function address in a call instruction is a byte address (for
   indexing purposes) so give the MEM rtx a byte's mode.  */
#define FUNCTION_MODE QImode

/* Specify the value which is used when clz operand is zero.  */
#define CLZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE) ((VALUE) = 64, 1)

/* Machine-specific symbol_ref flags.  */
#define SYMBOL_FLAG_ALIGN_SHIFT	  SYMBOL_FLAG_MACH_DEP_SHIFT
#define SYMBOL_FLAG_ALIGN_MASK    \
  ((SYMBOL_FLAG_MACH_DEP << 0) | (SYMBOL_FLAG_MACH_DEP << 1))

#define SYMBOL_FLAG_SET_ALIGN(X, A) \
    (SYMBOL_REF_FLAGS (X) = (SYMBOL_REF_FLAGS (X) & ~SYMBOL_FLAG_ALIGN_MASK) \
     | (A << SYMBOL_FLAG_ALIGN_SHIFT))

#define SYMBOL_FLAG_GET_ALIGN(X) \
    ((SYMBOL_REF_FLAGS (X) & SYMBOL_FLAG_ALIGN_MASK) >> SYMBOL_FLAG_ALIGN_SHIFT)

/* Helpers to access symbol_ref flags.  They are used in
   check_symref_alignment() and larl_operand to detect if the
   available alignment matches the required one.  We do not use
   a positive check like _ALIGN2 because in that case we would have
   to annotate every symbol_ref.  However, we only want to touch
   the symbol_refs that can be misaligned and assume that the others
   are correctly aligned.  Hence, if a symbol_ref does not have
   a _NOTALIGN flag it is supposed to be correctly aligned.  */
#define SYMBOL_FLAG_SET_NOTALIGN2(X) SYMBOL_FLAG_SET_ALIGN((X), 1)
#define SYMBOL_FLAG_SET_NOTALIGN4(X) SYMBOL_FLAG_SET_ALIGN((X), 2)
#define SYMBOL_FLAG_SET_NOTALIGN8(X) SYMBOL_FLAG_SET_ALIGN((X), 3)

#define SYMBOL_FLAG_NOTALIGN2_P(X) (SYMBOL_FLAG_GET_ALIGN(X) == 1)
#define SYMBOL_FLAG_NOTALIGN4_P(X) (SYMBOL_FLAG_GET_ALIGN(X) == 2	\
				    || SYMBOL_FLAG_GET_ALIGN(X) == 1)
#define SYMBOL_FLAG_NOTALIGN8_P(X) (SYMBOL_FLAG_GET_ALIGN(X) == 3	\
				    || SYMBOL_FLAG_GET_ALIGN(X) == 2	\
				    || SYMBOL_FLAG_GET_ALIGN(X) == 1)

/* Check whether integer displacement is in range for a short displacement.  */
#define SHORT_DISP_IN_RANGE(d) ((d) >= 0 && (d) <= 4095)

/* Check whether integer displacement is in range.  */
#define DISP_IN_RANGE(d)				\
  (TARGET_LONG_DISPLACEMENT				\
   ? ((d) >= -524288 && (d) <= 524287)			\
   : SHORT_DISP_IN_RANGE(d))

/* Reads can reuse write prefetches, used by tree-ssa-prefetch-loops.c.  */
#define READ_CAN_USE_WRITE_PREFETCH 1

extern const int processor_flags_table[];

struct s390_processor
{
  /* The preferred name to be used in user visible output.  */
  const char *const name;
  /* CPU name as it should be passed to Binutils via .machine  */
  const char *const binutils_name;
  const enum processor_type processor;
  const struct processor_costs *cost;
  int arch_level;
};

extern const struct s390_processor processor_table[];

/* The truth element value for vector comparisons.  Our instructions
   always generate -1 in that case.  */
#define VECTOR_STORE_FLAG_VALUE(MODE) CONSTM1_RTX (GET_MODE_INNER (MODE))

/* Target pragma.  */

/* resolve_overloaded_builtin cannot be defined the normal way since
   it is defined in code which technically belongs to the
   front-end.  */
#define REGISTER_TARGET_PRAGMAS()		\
  do {						\
    s390_register_target_pragmas ();		\
  } while (0)

#ifndef USED_FOR_TARGET
/* The following structure is embedded in the machine
   specific part of struct function.  */

struct GTY (()) s390_frame_layout
{
  /* Offset within stack frame.  */
  HOST_WIDE_INT gprs_offset;
  HOST_WIDE_INT f0_offset;
  HOST_WIDE_INT f4_offset;
  HOST_WIDE_INT f8_offset;
  HOST_WIDE_INT backchain_offset;

  /* Number of first and last gpr where slots in the register
     save area are reserved for.  */
  int first_save_gpr_slot;
  int last_save_gpr_slot;

  /* Location (FP register number) where GPRs (r0-r15) should
     be saved to.
      0 - does not need to be saved at all
     -1 - stack slot  */
#define SAVE_SLOT_NONE   0
#define SAVE_SLOT_STACK -1
  signed char gpr_save_slots[16];

  /* Number of first and last gpr to be saved, restored.  */
  int first_save_gpr;
  int first_restore_gpr;
  int last_save_gpr;
  int last_restore_gpr;

  /* Bits standing for floating point registers. Set, if the
     respective register has to be saved. Starting with reg 16 (f0)
     at the rightmost bit.
     Bit 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
     fpr 15 13 11  9 14 12 10  8  7  5  3  1  6  4  2  0
     reg 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16  */
  unsigned int fpr_bitmap;

  /* Number of floating point registers f8-f15 which must be saved.  */
  int high_fprs;

  /* Set if return address needs to be saved.
     This flag is set by s390_return_addr_rtx if it could not use
     the initial value of r14 and therefore depends on r14 saved
     to the stack.  */
  bool save_return_addr_p;

  /* Size of stack frame.  */
  HOST_WIDE_INT frame_size;
};


/* Define the structure for the machine field in struct function.  */

struct GTY(()) machine_function
{
  struct s390_frame_layout frame_layout;

  /* Literal pool base register.  */
  rtx base_reg;

  bool has_landing_pad_p;

  /* True if the current function may contain a tbegin clobbering
     FPRs.  */
  bool tbegin_p;

  /* For -fsplit-stack support: A stack local which holds a pointer to
     the stack arguments for a function with a variable number of
     arguments.  This is set at the start of the function and is used
     to initialize the overflow_arg_area field of the va_list
     structure.  */
  rtx split_stack_varargs_pointer;

  enum indirect_branch indirect_branch_jump;
  enum indirect_branch indirect_branch_call;

  enum indirect_branch function_return_mem;
  enum indirect_branch function_return_reg;
};
#endif

#define TARGET_INDIRECT_BRANCH_NOBP_RET_OPTION				\
  (cfun->machine->function_return_reg != indirect_branch_keep		\
   || cfun->machine->function_return_mem != indirect_branch_keep)

#define TARGET_INDIRECT_BRANCH_NOBP_RET					\
  ((cfun->machine->function_return_reg != indirect_branch_keep		\
    && !s390_return_addr_from_memory ())				\
   || (cfun->machine->function_return_mem != indirect_branch_keep	\
       && s390_return_addr_from_memory ()))

#define TARGET_INDIRECT_BRANCH_NOBP_JUMP				\
  (cfun->machine->indirect_branch_jump != indirect_branch_keep)

#define TARGET_INDIRECT_BRANCH_NOBP_JUMP_THUNK				\
  (cfun->machine->indirect_branch_jump == indirect_branch_thunk		\
   || cfun->machine->indirect_branch_jump == indirect_branch_thunk_extern)

#define TARGET_INDIRECT_BRANCH_NOBP_JUMP_INLINE_THUNK			\
  (cfun->machine->indirect_branch_jump == indirect_branch_thunk_inline)

#define TARGET_INDIRECT_BRANCH_NOBP_CALL			\
  (cfun->machine->indirect_branch_call != indirect_branch_keep)

#ifndef TARGET_DEFAULT_INDIRECT_BRANCH_TABLE
#define TARGET_DEFAULT_INDIRECT_BRANCH_TABLE 0
#endif

#define TARGET_INDIRECT_BRANCH_THUNK_NAME_EXRL "__s390_indirect_jump_r%d"
#define TARGET_INDIRECT_BRANCH_THUNK_NAME_EX   "__s390_indirect_jump_r%duse_r%d"

#define TARGET_INDIRECT_BRANCH_TABLE s390_indirect_branch_table

#ifdef GENERATOR_FILE
/* gencondmd.cc is built before insn-flags.h.  Use an arbitrary opaque value
   that cannot be optimized away by gen_insn.  */
#define HAVE_TF(icode) TARGET_HARD_FLOAT
#else
#define HAVE_TF(icode) (HAVE_##icode##_fpr || HAVE_##icode##_vr)
#endif

/* Dispatcher for movtf.  */
#define EXPAND_MOVTF(icode)                                                   \
  do                                                                          \
    {                                                                         \
      if (TARGET_VXE)                                                         \
	emit_insn (gen_##icode##_vr (operands[0], operands[1]));              \
      else                                                                    \
	emit_insn (gen_##icode##_fpr (operands[0], operands[1]));             \
      DONE;                                                                   \
    }                                                                         \
  while (false)

/* Like EXPAND_MOVTF, but also legitimizes operands.  */
#define EXPAND_TF(icode, nops)                                                \
  do                                                                          \
    {                                                                         \
      const size_t __nops = (nops);                                           \
      expand_operand ops[__nops];                                             \
      create_output_operand (&ops[0], operands[0], GET_MODE (operands[0]));   \
      for (size_t i = 1; i < __nops; i++)                                     \
	create_input_operand (&ops[i], operands[i], GET_MODE (operands[i]));  \
      if (TARGET_VXE)                                                         \
	expand_insn (CODE_FOR_##icode##_vr, __nops, ops);                     \
      else                                                                    \
	expand_insn (CODE_FOR_##icode##_fpr, __nops, ops);                    \
      DONE;                                                                   \
    }                                                                         \
  while (false)

#endif /* S390_H */
