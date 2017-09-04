/* Definitions of target machine for GNU compiler.  MIPS version.
   Copyright (C) 1989-2017 Free Software Foundation, Inc.
   Contributed by A. Lichnewsky (lich@inria.inria.fr).
   Changed by Michael Meissner	(meissner@osf.org).
   64-bit r4000 support by Ian Lance Taylor (ian@cygnus.com) and
   Brendan Eich (brendan@microunity.com).

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

#ifdef GENERATOR_FILE
/* This is used in some insn conditions, so needs to be declared, but
   does not need to be defined.  */
extern int target_flags_explicit;
#endif

/* MIPS external variables defined in mips.c.  */

/* Which ABI to use.  ABI_32 (original 32, or o32), ABI_N32 (n32),
   ABI_64 (n64) are all defined by SGI.  ABI_O64 is o32 extended
   to work on a 64-bit machine.  */

#define ABI_32  0
#define ABI_N32 1
#define ABI_64  2
#define ABI_EABI 3
#define ABI_O64  4

/* Masks that affect tuning.

   PTF_AVOID_BRANCHLIKELY_SPEED
	Set if it is usually not profitable to use branch-likely instructions
	for this target when optimizing code for speed, typically because
	the branches are always predicted taken and so incur a large overhead
	when not taken.

   PTF_AVOID_BRANCHLIKELY_SIZE
	As above but when optimizing for size.

   PTF_AVOID_BRANCHLIKELY_ALWAYS
	As above but regardless of whether we optimize for speed or size.

   PTF_AVOID_IMADD
	Set if it is usually not profitable to use the integer MADD or MSUB
	instructions because of the overhead of getting the result out of
	the HI/LO registers.  */

#define PTF_AVOID_BRANCHLIKELY_SPEED	0x1
#define PTF_AVOID_BRANCHLIKELY_SIZE	0x2
#define PTF_AVOID_BRANCHLIKELY_ALWAYS	(PTF_AVOID_BRANCHLIKELY_SPEED | \
					 PTF_AVOID_BRANCHLIKELY_SIZE)
#define PTF_AVOID_IMADD			0x4

/* Information about one recognized processor.  Defined here for the
   benefit of TARGET_CPU_CPP_BUILTINS.  */
struct mips_cpu_info {
  /* The 'canonical' name of the processor as far as GCC is concerned.
     It's typically a manufacturer's prefix followed by a numerical
     designation.  It should be lowercase.  */
  const char *name;

  /* The internal processor number that most closely matches this
     entry.  Several processors can have the same value, if there's no
     difference between them from GCC's point of view.  */
  enum processor cpu;

  /* The ISA level that the processor implements.  */
  int isa;

  /* A mask of PTF_* values.  */
  unsigned int tune_flags;
};

#include "config/mips/mips-opts.h"

/* Macros to silence warnings about numbers being signed in traditional
   C and unsigned in ISO C when compiled on 32-bit hosts.  */

#define BITMASK_HIGH	(((unsigned long)1) << 31)	/* 0x80000000 */
#define BITMASK_UPPER16	((unsigned long)0xffff << 16)	/* 0xffff0000 */
#define BITMASK_LOWER16	((unsigned long)0xffff)		/* 0x0000ffff */


/* Run-time compilation parameters selecting different hardware subsets.  */

/* True if we are generating position-independent VxWorks RTP code.  */
#define TARGET_RTP_PIC (TARGET_VXWORKS_RTP && flag_pic)

/* Compact branches must not be used if the user either selects the
   'never' policy or the 'optimal' policy on a core that lacks
   compact branch instructions.  */
#define TARGET_CB_NEVER (mips_cb == MIPS_CB_NEVER	\
			 || (mips_cb == MIPS_CB_OPTIMAL \
			     && !ISA_HAS_COMPACT_BRANCHES))

/* Compact branches may be used if the user either selects the
   'always' policy or the 'optimal' policy on a core that supports
   compact branch instructions.  */
#define TARGET_CB_MAYBE (TARGET_CB_ALWAYS		\
			 || (mips_cb == MIPS_CB_OPTIMAL \
			     && ISA_HAS_COMPACT_BRANCHES))

/* Compact branches must always be generated if the user selects
   the 'always' policy or the 'optimal' policy om a core that
   lacks delay slot branch instructions.  */
#define TARGET_CB_ALWAYS (mips_cb == MIPS_CB_ALWAYS	\
			 || (mips_cb == MIPS_CB_OPTIMAL \
			     && !ISA_HAS_DELAY_SLOTS))

/* Special handling for JRC that exists in microMIPSR3 as well as R6
   ISAs with full compact branch support.  */
#define ISA_HAS_JRC ((ISA_HAS_COMPACT_BRANCHES		\
		      || TARGET_MICROMIPS)		\
		     && mips_cb != MIPS_CB_NEVER)

/* True if the output file is marked as ".abicalls; .option pic0"
   (-call_nonpic).  */
#define TARGET_ABICALLS_PIC0 \
  (TARGET_ABSOLUTE_ABICALLS && TARGET_PLT)

/* True if the output file is marked as ".abicalls; .option pic2" (-KPIC).  */
#define TARGET_ABICALLS_PIC2 \
  (TARGET_ABICALLS && !TARGET_ABICALLS_PIC0)

/* True if the call patterns should be split into a jalr followed by
   an instruction to restore $gp.  It is only safe to split the load
   from the call when every use of $gp is explicit.

   See mips_must_initialize_gp_p for details about how we manage the
   global pointer.  */

#define TARGET_SPLIT_CALLS \
  (TARGET_EXPLICIT_RELOCS && TARGET_CALL_CLOBBERED_GP && epilogue_completed)

/* True if we're generating a form of -mabicalls in which we can use
   operators like %hi and %lo to refer to locally-binding symbols.
   We can only do this for -mno-shared, and only then if we can use
   relocation operations instead of assembly macros.  It isn't really
   worth using absolute sequences for 64-bit symbols because GOT
   accesses are so much shorter.  */

#define TARGET_ABSOLUTE_ABICALLS	\
  (TARGET_ABICALLS			\
   && !TARGET_SHARED			\
   && TARGET_EXPLICIT_RELOCS		\
   && !ABI_HAS_64BIT_SYMBOLS)

/* True if we can optimize sibling calls.  For simplicity, we only
   handle cases in which call_insn_operand will reject invalid
   sibcall addresses.  There are two cases in which this isn't true:

      - TARGET_MIPS16.  call_insn_operand accepts constant addresses
	but there is no direct jump instruction.  It isn't worth
	using sibling calls in this case anyway; they would usually
	be longer than normal calls.

      - TARGET_USE_GOT && !TARGET_EXPLICIT_RELOCS.  call_insn_operand
	accepts global constants, but all sibcalls must be indirect.  */
#define TARGET_SIBCALLS \
  (!TARGET_MIPS16 && (!TARGET_USE_GOT || TARGET_EXPLICIT_RELOCS))

/* True if we need to use a global offset table to access some symbols.  */
#define TARGET_USE_GOT (TARGET_ABICALLS || TARGET_RTP_PIC)

/* True if TARGET_USE_GOT and if $gp is a call-clobbered register.  */
#define TARGET_CALL_CLOBBERED_GP (TARGET_ABICALLS && TARGET_OLDABI)

/* True if TARGET_USE_GOT and if $gp is a call-saved register.  */
#define TARGET_CALL_SAVED_GP (TARGET_USE_GOT && !TARGET_CALL_CLOBBERED_GP)

/* True if we should use .cprestore to store to the cprestore slot.

   We continue to use .cprestore for explicit-reloc code so that JALs
   inside inline asms will work correctly.  */
#define TARGET_CPRESTORE_DIRECTIVE \
  (TARGET_ABICALLS_PIC2 && !TARGET_MIPS16)

/* True if we can use the J and JAL instructions.  */
#define TARGET_ABSOLUTE_JUMPS \
  (!flag_pic || TARGET_ABSOLUTE_ABICALLS)

/* True if indirect calls must use register class PIC_FN_ADDR_REG.
   This is true for both the PIC and non-PIC VxWorks RTP modes.  */
#define TARGET_USE_PIC_FN_ADDR_REG (TARGET_ABICALLS || TARGET_VXWORKS_RTP)

/* True if .gpword or .gpdword should be used for switch tables.  */
#define TARGET_GPWORD				\
  (TARGET_ABICALLS && !TARGET_ABSOLUTE_ABICALLS)

/* True if the output must have a writable .eh_frame.
   See ASM_PREFERRED_EH_DATA_FORMAT for details.  */
#ifdef HAVE_LD_PERSONALITY_RELAXATION
#define TARGET_WRITABLE_EH_FRAME 0
#else
#define TARGET_WRITABLE_EH_FRAME (flag_pic && TARGET_SHARED)
#endif

/* Test the assembler to set ISA_HAS_DSP_MULT to DSP Rev 1 or 2.  */
#ifdef HAVE_AS_DSPR1_MULT
#define ISA_HAS_DSP_MULT ISA_HAS_DSP
#else
#define ISA_HAS_DSP_MULT ISA_HAS_DSPR2
#endif

/* ISA has LSA available.  */
#define ISA_HAS_LSA		(mips_isa_rev >= 6 || ISA_HAS_MSA)

/* ISA has DLSA available.  */
#define ISA_HAS_DLSA		(TARGET_64BIT \
				 && (mips_isa_rev >= 6 \
				     || ISA_HAS_MSA))

/* The ISA compression flags that are currently in effect.  */
#define TARGET_COMPRESSION (target_flags & (MASK_MIPS16 | MASK_MICROMIPS))

/* Generate mips16 code */
#define TARGET_MIPS16		((target_flags & MASK_MIPS16) != 0)
/* Generate mips16e code. Default 16bit ASE for mips32* and mips64* */
#define GENERATE_MIPS16E	(TARGET_MIPS16 && mips_isa >= 32)
/* Generate mips16e register save/restore sequences.  */
#define GENERATE_MIPS16E_SAVE_RESTORE (GENERATE_MIPS16E && mips_abi == ABI_32)

/* True if we're generating a form of MIPS16 code in which general
   text loads are allowed.  */
#define TARGET_MIPS16_TEXT_LOADS \
  (TARGET_MIPS16 && mips_code_readable == CODE_READABLE_YES)

/* True if we're generating a form of MIPS16 code in which PC-relative
   loads are allowed.  */
#define TARGET_MIPS16_PCREL_LOADS \
  (TARGET_MIPS16 && mips_code_readable >= CODE_READABLE_PCREL)

/* Generic ISA defines.  */
#define ISA_MIPS1		    (mips_isa == 1)
#define ISA_MIPS2		    (mips_isa == 2)
#define ISA_MIPS3                   (mips_isa == 3)
#define ISA_MIPS4		    (mips_isa == 4)
#define ISA_MIPS32		    (mips_isa == 32)
#define ISA_MIPS32R2		    (mips_isa == 33)
#define ISA_MIPS32R3		    (mips_isa == 34)
#define ISA_MIPS32R5		    (mips_isa == 36)
#define ISA_MIPS32R6		    (mips_isa == 37)
#define ISA_MIPS64                  (mips_isa == 64)
#define ISA_MIPS64R2		    (mips_isa == 65)
#define ISA_MIPS64R3		    (mips_isa == 66)
#define ISA_MIPS64R5		    (mips_isa == 68)
#define ISA_MIPS64R6		    (mips_isa == 69)

/* Architecture target defines.  */
#define TARGET_LOONGSON_2E          (mips_arch == PROCESSOR_LOONGSON_2E)
#define TARGET_LOONGSON_2F          (mips_arch == PROCESSOR_LOONGSON_2F)
#define TARGET_LOONGSON_2EF         (TARGET_LOONGSON_2E || TARGET_LOONGSON_2F)
#define TARGET_LOONGSON_3A          (mips_arch == PROCESSOR_LOONGSON_3A)
#define TARGET_MIPS3900             (mips_arch == PROCESSOR_R3900)
#define TARGET_MIPS4000             (mips_arch == PROCESSOR_R4000)
#define TARGET_MIPS4120             (mips_arch == PROCESSOR_R4120)
#define TARGET_MIPS4130             (mips_arch == PROCESSOR_R4130)
#define TARGET_MIPS5400             (mips_arch == PROCESSOR_R5400)
#define TARGET_MIPS5500             (mips_arch == PROCESSOR_R5500)
#define TARGET_MIPS5900             (mips_arch == PROCESSOR_R5900)
#define TARGET_MIPS7000             (mips_arch == PROCESSOR_R7000)
#define TARGET_MIPS8000             (mips_arch == PROCESSOR_R8000)
#define TARGET_MIPS9000             (mips_arch == PROCESSOR_R9000)
#define TARGET_OCTEON		    (mips_arch == PROCESSOR_OCTEON	\
				     || mips_arch == PROCESSOR_OCTEON2	\
				     || mips_arch == PROCESSOR_OCTEON3)
#define TARGET_OCTEON2		    (mips_arch == PROCESSOR_OCTEON2	\
				     || mips_arch == PROCESSOR_OCTEON3)
#define TARGET_SB1                  (mips_arch == PROCESSOR_SB1		\
				     || mips_arch == PROCESSOR_SB1A)
#define TARGET_SR71K                (mips_arch == PROCESSOR_SR71000)
#define TARGET_XLP                  (mips_arch == PROCESSOR_XLP)

/* Scheduling target defines.  */
#define TUNE_20KC		    (mips_tune == PROCESSOR_20KC)
#define TUNE_24K		    (mips_tune == PROCESSOR_24KC	\
				     || mips_tune == PROCESSOR_24KF2_1	\
				     || mips_tune == PROCESSOR_24KF1_1)
#define TUNE_74K                    (mips_tune == PROCESSOR_74KC	\
				     || mips_tune == PROCESSOR_74KF2_1	\
				     || mips_tune == PROCESSOR_74KF1_1  \
				     || mips_tune == PROCESSOR_74KF3_2)
#define TUNE_LOONGSON_2EF           (mips_tune == PROCESSOR_LOONGSON_2E	\
				     || mips_tune == PROCESSOR_LOONGSON_2F)
#define TUNE_LOONGSON_3A            (mips_tune == PROCESSOR_LOONGSON_3A)
#define TUNE_MIPS3000               (mips_tune == PROCESSOR_R3000)
#define TUNE_MIPS3900               (mips_tune == PROCESSOR_R3900)
#define TUNE_MIPS4000               (mips_tune == PROCESSOR_R4000)
#define TUNE_MIPS4120               (mips_tune == PROCESSOR_R4120)
#define TUNE_MIPS4130               (mips_tune == PROCESSOR_R4130)
#define TUNE_MIPS5000               (mips_tune == PROCESSOR_R5000)
#define TUNE_MIPS5400               (mips_tune == PROCESSOR_R5400)
#define TUNE_MIPS5500               (mips_tune == PROCESSOR_R5500)
#define TUNE_MIPS6000               (mips_tune == PROCESSOR_R6000)
#define TUNE_MIPS7000               (mips_tune == PROCESSOR_R7000)
#define TUNE_MIPS9000               (mips_tune == PROCESSOR_R9000)
#define TUNE_OCTEON		    (mips_tune == PROCESSOR_OCTEON	\
				     || mips_tune == PROCESSOR_OCTEON2	\
				     || mips_tune == PROCESSOR_OCTEON3)
#define TUNE_SB1                    (mips_tune == PROCESSOR_SB1		\
				     || mips_tune == PROCESSOR_SB1A)
#define TUNE_P5600                  (mips_tune == PROCESSOR_P5600)
#define TUNE_I6400                  (mips_tune == PROCESSOR_I6400)

/* Whether vector modes and intrinsics for ST Microelectronics
   Loongson-2E/2F processors should be enabled.  In o32 pairs of
   floating-point registers provide 64-bit values.  */
#define TARGET_LOONGSON_VECTORS	    (TARGET_HARD_FLOAT_ABI		\
				     && (TARGET_LOONGSON_2EF		\
					 || TARGET_LOONGSON_3A))

/* True if the pre-reload scheduler should try to create chains of
   multiply-add or multiply-subtract instructions.  For example,
   suppose we have:

	t1 = a * b
	t2 = t1 + c * d
	t3 = e * f
	t4 = t3 - g * h

   t1 will have a higher priority than t2 and t3 will have a higher
   priority than t4.  However, before reload, there is no dependence
   between t1 and t3, and they can often have similar priorities.
   The scheduler will then tend to prefer:

	t1 = a * b
	t3 = e * f
	t2 = t1 + c * d
	t4 = t3 - g * h

   which stops us from making full use of macc/madd-style instructions.
   This sort of situation occurs frequently in Fourier transforms and
   in unrolled loops.

   To counter this, the TUNE_MACC_CHAINS code will reorder the ready
   queue so that chained multiply-add and multiply-subtract instructions
   appear ahead of any other instruction that is likely to clobber lo.
   In the example above, if t2 and t3 become ready at the same time,
   the code ensures that t2 is scheduled first.

   Multiply-accumulate instructions are a bigger win for some targets
   than others, so this macro is defined on an opt-in basis.  */
#define TUNE_MACC_CHAINS	    (TUNE_MIPS5500		\
				     || TUNE_MIPS4120		\
				     || TUNE_MIPS4130		\
				     || TUNE_24K		\
				     || TUNE_P5600)

#define TARGET_OLDABI		    (mips_abi == ABI_32 || mips_abi == ABI_O64)
#define TARGET_NEWABI		    (mips_abi == ABI_N32 || mips_abi == ABI_64)

/* TARGET_HARD_FLOAT and TARGET_SOFT_FLOAT reflect whether the FPU is
   directly accessible, while the command-line options select
   TARGET_HARD_FLOAT_ABI and TARGET_SOFT_FLOAT_ABI to reflect the ABI
   in use.  */
#define TARGET_HARD_FLOAT (TARGET_HARD_FLOAT_ABI && !TARGET_MIPS16)
#define TARGET_SOFT_FLOAT (TARGET_SOFT_FLOAT_ABI || TARGET_MIPS16)

/* TARGET_FLOAT64 represents -mfp64 and TARGET_FLOATXX represents
   -mfpxx, derive TARGET_FLOAT32 to represent -mfp32.  */
#define TARGET_FLOAT32 (!TARGET_FLOAT64 && !TARGET_FLOATXX)

/* TARGET_O32_FP64A_ABI represents all the conditions that form the
   o32 FP64A ABI extension (-mabi=32 -mfp64 -mno-odd-spreg).  */
#define TARGET_O32_FP64A_ABI (mips_abi == ABI_32 && TARGET_FLOAT64 \
			      && !TARGET_ODD_SPREG)

/* False if SC acts as a memory barrier with respect to itself,
   otherwise a SYNC will be emitted after SC for atomic operations
   that require ordering between the SC and following loads and
   stores.  It does not tell anything about ordering of loads and
   stores prior to and following the SC, only about the SC itself and
   those loads and stores follow it.  */
#define TARGET_SYNC_AFTER_SC (!TARGET_OCTEON && !TARGET_XLP)

/* Define preprocessor macros for the -march and -mtune options.
   PREFIX is either _MIPS_ARCH or _MIPS_TUNE, INFO is the selected
   processor.  If INFO's canonical name is "foo", define PREFIX to
   be "foo", and define an additional macro PREFIX_FOO.  */
#define MIPS_CPP_SET_PROCESSOR(PREFIX, INFO)			\
  do								\
    {								\
      char *macro, *p;						\
								\
      macro = concat ((PREFIX), "_", (INFO)->name, NULL);	\
      for (p = macro; *p != 0; p++)				\
        if (*p == '+')                                          \
          *p = 'P';                                             \
        else                                                    \
          *p = TOUPPER (*p);                                    \
								\
      builtin_define (macro);					\
      builtin_define_with_value ((PREFIX), (INFO)->name, 1);	\
      free (macro);						\
    }								\
  while (0)

/* Target CPU builtins.  */
#define TARGET_CPU_CPP_BUILTINS()					\
  do									\
    {									\
      builtin_assert ("machine=mips");                        		\
      builtin_assert ("cpu=mips");					\
      builtin_define ("__mips__");     					\
      builtin_define ("_mips");						\
									\
      /* We do this here because __mips is defined below and so we	\
	 can't use builtin_define_std.  We don't ever want to define	\
	 "mips" for VxWorks because some of the VxWorks headers		\
	 construct include filenames from a root directory macro,	\
	 an architecture macro and a filename, where the architecture	\
	 macro expands to 'mips'.  If we define 'mips' to 1, the	\
	 architecture macro expands to 1 as well.  */			\
      if (!flag_iso && !TARGET_VXWORKS)					\
	builtin_define ("mips");					\
									\
      if (TARGET_64BIT)							\
	builtin_define ("__mips64");					\
									\
      /* Treat _R3000 and _R4000 like register-size			\
	 defines, which is how they've historically			\
	 been used.  */							\
      if (TARGET_64BIT)							\
	{								\
	  builtin_define_std ("R4000");					\
	  builtin_define ("_R4000");					\
	}								\
      else								\
	{								\
	  builtin_define_std ("R3000");					\
	  builtin_define ("_R3000");					\
	}								\
									\
      if (TARGET_FLOAT64)						\
	builtin_define ("__mips_fpr=64");				\
      else if (TARGET_FLOATXX)						\
	builtin_define ("__mips_fpr=0");				\
      else								\
	builtin_define ("__mips_fpr=32");				\
									\
      if (mips_base_compression_flags & MASK_MIPS16)			\
	builtin_define ("__mips16");					\
									\
      if (TARGET_MIPS3D)						\
	builtin_define ("__mips3d");					\
									\
      if (TARGET_SMARTMIPS)						\
	builtin_define ("__mips_smartmips");				\
									\
      if (mips_base_compression_flags & MASK_MICROMIPS)			\
	builtin_define ("__mips_micromips");				\
									\
      if (TARGET_MCU)							\
	builtin_define ("__mips_mcu");					\
									\
      if (TARGET_EVA)							\
	builtin_define ("__mips_eva");					\
									\
      if (TARGET_DSP)							\
	{								\
	  builtin_define ("__mips_dsp");				\
	  if (TARGET_DSPR2)						\
	    {								\
	      builtin_define ("__mips_dspr2");				\
	      builtin_define ("__mips_dsp_rev=2");			\
	    }								\
	  else								\
	    builtin_define ("__mips_dsp_rev=1");			\
	}								\
									\
      if (ISA_HAS_MSA)							\
	{								\
	  builtin_define ("__mips_msa");				\
	  builtin_define ("__mips_msa_width=128");			\
	}								\
									\
      MIPS_CPP_SET_PROCESSOR ("_MIPS_ARCH", mips_arch_info);		\
      MIPS_CPP_SET_PROCESSOR ("_MIPS_TUNE", mips_tune_info);		\
									\
      if (ISA_MIPS1)							\
	{								\
	  builtin_define ("__mips=1");					\
	  builtin_define ("_MIPS_ISA=_MIPS_ISA_MIPS1");			\
	}								\
      else if (ISA_MIPS2)						\
	{								\
	  builtin_define ("__mips=2");					\
	  builtin_define ("_MIPS_ISA=_MIPS_ISA_MIPS2");			\
	}								\
      else if (ISA_MIPS3)						\
	{								\
	  builtin_define ("__mips=3");					\
	  builtin_define ("_MIPS_ISA=_MIPS_ISA_MIPS3");			\
	}								\
      else if (ISA_MIPS4)						\
	{								\
	  builtin_define ("__mips=4");					\
	  builtin_define ("_MIPS_ISA=_MIPS_ISA_MIPS4");			\
	}								\
      else if (mips_isa >= 32 && mips_isa < 64)				\
	{								\
	  builtin_define ("__mips=32");					\
	  builtin_define ("_MIPS_ISA=_MIPS_ISA_MIPS32");		\
	}								\
      else if (mips_isa >= 64)						\
	{								\
	  builtin_define ("__mips=64");					\
	  builtin_define ("_MIPS_ISA=_MIPS_ISA_MIPS64");		\
	}								\
      if (mips_isa_rev > 0)						\
	builtin_define_with_int_value ("__mips_isa_rev",		\
				       mips_isa_rev);			\
									\
      switch (mips_abi)							\
	{								\
	case ABI_32:							\
	  builtin_define ("_ABIO32=1");					\
	  builtin_define ("_MIPS_SIM=_ABIO32");				\
	  break;							\
									\
	case ABI_N32:							\
	  builtin_define ("_ABIN32=2");					\
	  builtin_define ("_MIPS_SIM=_ABIN32");				\
	  break;							\
									\
	case ABI_64:							\
	  builtin_define ("_ABI64=3");					\
	  builtin_define ("_MIPS_SIM=_ABI64");				\
	  break;							\
									\
	case ABI_O64:							\
	  builtin_define ("_ABIO64=4");					\
	  builtin_define ("_MIPS_SIM=_ABIO64");				\
	  break;							\
	}								\
									\
      builtin_define_with_int_value ("_MIPS_SZINT", INT_TYPE_SIZE);	\
      builtin_define_with_int_value ("_MIPS_SZLONG", LONG_TYPE_SIZE);	\
      builtin_define_with_int_value ("_MIPS_SZPTR", POINTER_SIZE);	\
      builtin_define_with_int_value ("_MIPS_FPSET",			\
				     32 / MAX_FPRS_PER_FMT);		\
      builtin_define_with_int_value ("_MIPS_SPFPSET",			\
				     TARGET_ODD_SPREG ? 32 : 16);	\
									\
      /* These defines reflect the ABI in use, not whether the  	\
	 FPU is directly accessible.  */				\
      if (TARGET_NO_FLOAT)						\
	builtin_define ("__mips_no_float");				\
      else if (TARGET_HARD_FLOAT_ABI)					\
	builtin_define ("__mips_hard_float");				\
      else								\
	builtin_define ("__mips_soft_float");				\
									\
      if (TARGET_SINGLE_FLOAT)						\
	builtin_define ("__mips_single_float");				\
									\
      if (TARGET_PAIRED_SINGLE_FLOAT)					\
	builtin_define ("__mips_paired_single_float");			\
									\
      if (mips_abs == MIPS_IEEE_754_2008)				\
	builtin_define ("__mips_abs2008");				\
									\
      if (mips_nan == MIPS_IEEE_754_2008)				\
	builtin_define ("__mips_nan2008");				\
									\
      if (TARGET_BIG_ENDIAN)						\
	{								\
	  builtin_define_std ("MIPSEB");				\
	  builtin_define ("_MIPSEB");					\
	}								\
      else								\
	{								\
	  builtin_define_std ("MIPSEL");				\
	  builtin_define ("_MIPSEL");					\
	}								\
                                                                        \
      /* Whether calls should go through $25.  The separate __PIC__	\
	 macro indicates whether abicalls code might use a GOT.  */	\
      if (TARGET_ABICALLS)						\
	builtin_define ("__mips_abicalls");				\
									\
      /* Whether Loongson vector modes are enabled.  */                 \
      if (TARGET_LOONGSON_VECTORS)					\
        builtin_define ("__mips_loongson_vector_rev");                  \
									\
      /* Historical Octeon macro.  */					\
      if (TARGET_OCTEON)						\
	builtin_define ("__OCTEON__");					\
									\
      if (TARGET_SYNCI)							\
	builtin_define ("__mips_synci");				\
									\
      /* Macros dependent on the C dialect.  */				\
      if (preprocessing_asm_p ())					\
	{								\
	  builtin_define_std ("LANGUAGE_ASSEMBLY");			\
	  builtin_define ("_LANGUAGE_ASSEMBLY");			\
	}								\
      else if (c_dialect_cxx ())					\
	{								\
	  builtin_define ("_LANGUAGE_C_PLUS_PLUS");			\
	  builtin_define ("__LANGUAGE_C_PLUS_PLUS");			\
	  builtin_define ("__LANGUAGE_C_PLUS_PLUS__");			\
	}								\
      else								\
	{								\
	  builtin_define_std ("LANGUAGE_C");				\
	  builtin_define ("_LANGUAGE_C");				\
	}								\
      if (c_dialect_objc ())						\
	{								\
	  builtin_define ("_LANGUAGE_OBJECTIVE_C");			\
	  builtin_define ("__LANGUAGE_OBJECTIVE_C");			\
	  /* Bizarre, but retained for backwards compatibility.  */	\
	  builtin_define_std ("LANGUAGE_C");				\
	  builtin_define ("_LANGUAGE_C");				\
	}								\
									\
      if (mips_abi == ABI_EABI)						\
	builtin_define ("__mips_eabi");					\
									\
      if (TARGET_CACHE_BUILTIN)						\
	builtin_define ("__GCC_HAVE_BUILTIN_MIPS_CACHE");		\
      if (!ISA_HAS_LXC1_SXC1)						\
	builtin_define ("__mips_no_lxc1_sxc1");				\
      if (!ISA_HAS_UNFUSED_MADD4 && !ISA_HAS_FUSED_MADD4)		\
	builtin_define ("__mips_no_madd4");				\
    }									\
  while (0)

/* Default target_flags if no switches are specified  */

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT 0
#endif

#ifndef TARGET_CPU_DEFAULT
#define TARGET_CPU_DEFAULT 0
#endif

#ifndef TARGET_ENDIAN_DEFAULT
#define TARGET_ENDIAN_DEFAULT MASK_BIG_ENDIAN
#endif

#ifdef IN_LIBGCC2
#undef TARGET_64BIT
/* Make this compile time constant for libgcc2 */
#ifdef __mips64
#define TARGET_64BIT		1
#else
#define TARGET_64BIT		0
#endif
#endif /* IN_LIBGCC2 */

/* Force the call stack unwinders in unwind.inc not to be MIPS16 code
   when compiled with hardware floating point.  This is because MIPS16
   code cannot save and restore the floating-point registers, which is
   important if in a mixed MIPS16/non-MIPS16 environment.  */

#ifdef IN_LIBGCC2
#if __mips_hard_float
#define LIBGCC2_UNWIND_ATTRIBUTE __attribute__((__nomips16__))
#endif
#endif /* IN_LIBGCC2 */

#define TARGET_LIBGCC_SDATA_SECTION ".sdata"

#ifndef MULTILIB_ENDIAN_DEFAULT
#if TARGET_ENDIAN_DEFAULT == 0
#define MULTILIB_ENDIAN_DEFAULT "EL"
#else
#define MULTILIB_ENDIAN_DEFAULT "EB"
#endif
#endif

#ifndef MULTILIB_ISA_DEFAULT
#if MIPS_ISA_DEFAULT == 1
#define MULTILIB_ISA_DEFAULT "mips1"
#elif MIPS_ISA_DEFAULT == 2
#define MULTILIB_ISA_DEFAULT "mips2"
#elif MIPS_ISA_DEFAULT == 3
#define MULTILIB_ISA_DEFAULT "mips3"
#elif MIPS_ISA_DEFAULT == 4
#define MULTILIB_ISA_DEFAULT "mips4"
#elif MIPS_ISA_DEFAULT == 32
#define MULTILIB_ISA_DEFAULT "mips32"
#elif MIPS_ISA_DEFAULT == 33
#define MULTILIB_ISA_DEFAULT "mips32r2"
#elif MIPS_ISA_DEFAULT == 37
#define MULTILIB_ISA_DEFAULT "mips32r6"
#elif MIPS_ISA_DEFAULT == 64
#define MULTILIB_ISA_DEFAULT "mips64"
#elif MIPS_ISA_DEFAULT == 65
#define MULTILIB_ISA_DEFAULT "mips64r2"
#elif MIPS_ISA_DEFAULT == 69
#define MULTILIB_ISA_DEFAULT "mips64r6"
#else
#define MULTILIB_ISA_DEFAULT "mips1"
#endif
#endif

#ifndef MIPS_ABI_DEFAULT
#define MIPS_ABI_DEFAULT ABI_32
#endif

/* Use the most portable ABI flag for the ASM specs.  */

#if MIPS_ABI_DEFAULT == ABI_32
#define MULTILIB_ABI_DEFAULT "mabi=32"
#elif MIPS_ABI_DEFAULT == ABI_O64
#define MULTILIB_ABI_DEFAULT "mabi=o64"
#elif MIPS_ABI_DEFAULT == ABI_N32
#define MULTILIB_ABI_DEFAULT "mabi=n32"
#elif MIPS_ABI_DEFAULT == ABI_64
#define MULTILIB_ABI_DEFAULT "mabi=64"
#elif MIPS_ABI_DEFAULT == ABI_EABI
#define MULTILIB_ABI_DEFAULT "mabi=eabi"
#endif

#ifndef MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS \
    { MULTILIB_ENDIAN_DEFAULT, MULTILIB_ISA_DEFAULT, MULTILIB_ABI_DEFAULT }
#endif

/* We must pass -EL to the linker by default for little endian embedded
   targets using linker scripts with a OUTPUT_FORMAT line.  Otherwise, the
   linker will default to using big-endian output files.  The OUTPUT_FORMAT
   line must be in the linker script, otherwise -EB/-EL will not work.  */

#ifndef ENDIAN_SPEC
#if TARGET_ENDIAN_DEFAULT == 0
#define ENDIAN_SPEC "%{!EB:%{!meb:-EL}} %{EB|meb:-EB}"
#else
#define ENDIAN_SPEC "%{!EL:%{!mel:-EB}} %{EL|mel:-EL}"
#endif
#endif

/* A spec condition that matches all non-mips16 -mips arguments.  */

#define MIPS_ISA_LEVEL_OPTION_SPEC \
  "mips1|mips2|mips3|mips4|mips32*|mips64*"

/* A spec condition that matches all non-mips16 architecture arguments.  */

#define MIPS_ARCH_OPTION_SPEC \
  MIPS_ISA_LEVEL_OPTION_SPEC "|march=*"

/* A spec that infers a -mips argument from an -march argument.  */

#define MIPS_ISA_LEVEL_SPEC \
  "%{" MIPS_ISA_LEVEL_OPTION_SPEC ":;: \
     %{march=mips1|march=r2000|march=r3000|march=r3900:-mips1} \
     %{march=mips2|march=r6000:-mips2} \
     %{march=mips3|march=r4*|march=vr4*|march=orion|march=loongson2*:-mips3} \
     %{march=mips4|march=r8000|march=vr5*|march=rm7000|march=rm9000 \
       |march=r10000|march=r12000|march=r14000|march=r16000:-mips4} \
     %{march=mips32|march=4kc|march=4km|march=4kp|march=4ksc:-mips32} \
     %{march=mips32r2|march=m4k|march=4ke*|march=4ksd|march=24k* \
       |march=34k*|march=74k*|march=m14k*|march=1004k* \
       |march=interaptiv: -mips32r2} \
     %{march=mips32r3: -mips32r3} \
     %{march=mips32r5|march=p5600|march=m5100|march=m5101: -mips32r5} \
     %{march=mips32r6: -mips32r6} \
     %{march=mips64|march=5k*|march=20k*|march=sb1*|march=sr71000 \
       |march=xlr: -mips64} \
     %{march=mips64r2|march=loongson3a|march=octeon|march=xlp: -mips64r2} \
     %{march=mips64r3: -mips64r3} \
     %{march=mips64r5: -mips64r5} \
     %{march=mips64r6|march=i6400: -mips64r6}}"

/* A spec that injects the default multilib ISA if no architecture is
   specified.  */

#define MIPS_DEFAULT_ISA_LEVEL_SPEC \
  "%{" MIPS_ISA_LEVEL_OPTION_SPEC ":;: \
     %{!march=*: -" MULTILIB_ISA_DEFAULT "}}"

/* A spec that infers a -mhard-float or -msoft-float setting from an
   -march argument.  Note that soft-float and hard-float code are not
   link-compatible.  */

#define MIPS_ARCH_FLOAT_SPEC \
  "%{mhard-float|msoft-float|mno-float|march=mips*:; \
     march=vr41*|march=m4k|march=4k*|march=24kc|march=24kec \
     |march=34kc|march=34kn|march=74kc|march=1004kc|march=5kc \
     |march=m14k*|march=m5101|march=octeon|march=xlr: -msoft-float; \
     march=*: -mhard-float}"

/* A spec condition that matches 32-bit options.  It only works if
   MIPS_ISA_LEVEL_SPEC has been applied.  */

#define MIPS_32BIT_OPTION_SPEC \
  "mips1|mips2|mips32*|mgp32"

/* A spec condition that matches architectures should be targeted with
   o32 FPXX for compatibility reasons.  */
#define MIPS_FPXX_OPTION_SPEC \
  "mips2|mips3|mips4|mips5|mips32|mips32r2|mips32r3|mips32r5| \
   mips64|mips64r2|mips64r3|mips64r5"

/* Infer a -msynci setting from a -mips argument, on the assumption that
   -msynci is desired where possible.  */
#define MIPS_ISA_SYNCI_SPEC \
  "%{msynci|mno-synci:;:%{mips32r2|mips32r3|mips32r5|mips32r6|mips64r2 \
			  |mips64r3|mips64r5|mips64r6:-msynci;:-mno-synci}}"

/* Infer a -mnan=2008 setting from a -mips argument.  */
#define MIPS_ISA_NAN2008_SPEC \
  "%{mnan*:;mips32r6|mips64r6:-mnan=2008;march=m51*: \
					 %{!msoft-float:-mnan=2008}}"

#if (MIPS_ABI_DEFAULT == ABI_O64 \
     || MIPS_ABI_DEFAULT == ABI_N32 \
     || MIPS_ABI_DEFAULT == ABI_64)
#define OPT_ARCH64 "mabi=32|mgp32:;"
#define OPT_ARCH32 "mabi=32|mgp32"
#else
#define OPT_ARCH64 "mabi=o64|mabi=n32|mabi=64|mgp64"
#define OPT_ARCH32 "mabi=o64|mabi=n32|mabi=64|mgp64:;"
#endif

/* Support for a compile-time default CPU, et cetera.  The rules are:
   --with-arch is ignored if -march is specified or a -mips is specified
     (other than -mips16); likewise --with-arch-32 and --with-arch-64.
   --with-tune is ignored if -mtune is specified; likewise
     --with-tune-32 and --with-tune-64.
   --with-abi is ignored if -mabi is specified.
   --with-float is ignored if -mhard-float or -msoft-float are
     specified.
   --with-fpu is ignored if -msoft-float, -msingle-float or -mdouble-float are
     specified.
   --with-nan is ignored if -mnan is specified.
   --with-fp-32 is ignored if -msoft-float, -msingle-float, -mmsa or -mfp are
     specified.
   --with-odd-spreg-32 is ignored if -msoft-float, -msingle-float, -modd-spreg
     or -mno-odd-spreg are specified.
   --with-divide is ignored if -mdivide-traps or -mdivide-breaks are
     specified. */
#define OPTION_DEFAULT_SPECS \
  {"arch", "%{" MIPS_ARCH_OPTION_SPEC ":;: -march=%(VALUE)}" }, \
  {"arch_32", "%{" OPT_ARCH32 ":%{" MIPS_ARCH_OPTION_SPEC ":;: -march=%(VALUE)}}" }, \
  {"arch_64", "%{" OPT_ARCH64 ":%{" MIPS_ARCH_OPTION_SPEC ":;: -march=%(VALUE)}}" }, \
  {"tune", "%{!mtune=*:-mtune=%(VALUE)}" }, \
  {"tune_32", "%{" OPT_ARCH32 ":%{!mtune=*:-mtune=%(VALUE)}}" }, \
  {"tune_64", "%{" OPT_ARCH64 ":%{!mtune=*:-mtune=%(VALUE)}}" }, \
  {"abi", "%{!mabi=*:-mabi=%(VALUE)}" }, \
  {"float", "%{!msoft-float:%{!mhard-float:-m%(VALUE)-float}}" }, \
  {"fpu", "%{!msoft-float:%{!msingle-float:%{!mdouble-float:-m%(VALUE)-float}}}" }, \
  {"nan", "%{!mnan=*:-mnan=%(VALUE)}" }, \
  {"fp_32", "%{" OPT_ARCH32 \
	    ":%{!msoft-float:%{!msingle-float:%{!mfp*:%{!mmsa:-mfp%(VALUE)}}}}}" }, \
  {"odd_spreg_32", "%{" OPT_ARCH32 ":%{!msoft-float:%{!msingle-float:" \
		   "%{!modd-spreg:%{!mno-odd-spreg:-m%(VALUE)}}}}}" }, \
  {"divide", "%{!mdivide-traps:%{!mdivide-breaks:-mdivide-%(VALUE)}}" }, \
  {"llsc", "%{!mllsc:%{!mno-llsc:-m%(VALUE)}}" }, \
  {"mips-plt", "%{!mplt:%{!mno-plt:-m%(VALUE)}}" }, \
  {"synci", "%{!msynci:%{!mno-synci:-m%(VALUE)}}" },			\
  {"lxc1-sxc1", "%{!mlxc1-sxc1:%{!mno-lxc1-sxc1:-m%(VALUE)}}" }, \
  {"madd4", "%{!mmadd4:%{!mno-madd4:-m%(VALUE)}}" } \

/* A spec that infers the:
   -mnan=2008 setting from a -mips argument,
   -mdsp setting from a -march argument.  */
#define BASE_DRIVER_SELF_SPECS \
  MIPS_ISA_NAN2008_SPEC,       \
  "%{!mno-dsp: \
     %{march=24ke*|march=34kc*|march=34kf*|march=34kx*|march=1004k* \
       |march=interaptiv: -mdsp} \
     %{march=74k*|march=m14ke*: %{!mno-dspr2: -mdspr2 -mdsp}}}"

#define DRIVER_SELF_SPECS \
  MIPS_ISA_LEVEL_SPEC,	  \
  BASE_DRIVER_SELF_SPECS

#define GENERATE_DIVIDE_TRAPS (TARGET_DIVIDE_TRAPS \
                               && ISA_HAS_COND_TRAP)

#define GENERATE_BRANCHLIKELY   (TARGET_BRANCHLIKELY && !TARGET_MIPS16)

/* True if the ABI can only work with 64-bit integer registers.  We
   generally allow ad-hoc variations for TARGET_SINGLE_FLOAT, but
   otherwise floating-point registers must also be 64-bit.  */
#define ABI_NEEDS_64BIT_REGS	(TARGET_NEWABI || mips_abi == ABI_O64)

/* Likewise for 32-bit regs.  */
#define ABI_NEEDS_32BIT_REGS	(mips_abi == ABI_32)

/* True if the file format uses 64-bit symbols.  At present, this is
   only true for n64, which uses 64-bit ELF.  */
#define FILE_HAS_64BIT_SYMBOLS	(mips_abi == ABI_64)

/* True if symbols are 64 bits wide.  This is usually determined by
   the ABI's file format, but it can be overridden by -msym32.  Note that
   overriding the size with -msym32 changes the ABI of relocatable objects,
   although it doesn't change the ABI of a fully-linked object.  */
#define ABI_HAS_64BIT_SYMBOLS	(FILE_HAS_64BIT_SYMBOLS \
				 && Pmode == DImode	\
				 && !TARGET_SYM32)

/* ISA has instructions for managing 64-bit fp and gp regs (e.g. mips3).  */
#define ISA_HAS_64BIT_REGS	(ISA_MIPS3				\
				 || ISA_MIPS4				\
				 || ISA_MIPS64				\
				 || ISA_MIPS64R2			\
				 || ISA_MIPS64R3			\
				 || ISA_MIPS64R5			\
				 || ISA_MIPS64R6)

#define ISA_HAS_JR		(mips_isa_rev <= 5)

#define ISA_HAS_DELAY_SLOTS	1

#define ISA_HAS_COMPACT_BRANCHES (mips_isa_rev >= 6)

/* ISA has branch likely instructions (e.g. mips2).  */
/* Disable branchlikely for tx39 until compare rewrite.  They haven't
   been generated up to this point.  */
#define ISA_HAS_BRANCHLIKELY	(!ISA_MIPS1 && mips_isa_rev <= 5)

/* ISA has 32 single-precision registers.  */
#define ISA_HAS_ODD_SPREG	((mips_isa_rev >= 1			\
				  && !TARGET_LOONGSON_3A)		\
				 || TARGET_FLOAT64			\
				 || TARGET_MIPS5900)

/* ISA has a three-operand multiplication instruction (usually spelt "mul").  */
#define ISA_HAS_MUL3		((TARGET_MIPS3900                       \
				  || TARGET_MIPS5400			\
				  || TARGET_MIPS5500			\
				  || TARGET_MIPS5900			\
				  || TARGET_MIPS7000			\
				  || TARGET_MIPS9000			\
				  || TARGET_MAD				\
				  || (mips_isa_rev >= 1			\
				      && mips_isa_rev <= 5))		\
				 && !TARGET_MIPS16)

/* ISA has a three-operand multiplication instruction.  */
#define ISA_HAS_DMUL3		(TARGET_64BIT				\
				 && TARGET_OCTEON			\
				 && !TARGET_MIPS16)

/* ISA has HI and LO registers.  */
#define ISA_HAS_HILO		(mips_isa_rev <= 5)

/* ISA supports instructions DMULT and DMULTU. */
#define ISA_HAS_DMULT		(TARGET_64BIT				\
				 && !TARGET_MIPS5900			\
				 && mips_isa_rev <= 5)

/* ISA supports instructions MULT and MULTU.  */
#define ISA_HAS_MULT		(mips_isa_rev <= 5)

/* ISA supports instructions MUL, MULU, MUH, MUHU.  */
#define ISA_HAS_R6MUL		(mips_isa_rev >= 6)

/* ISA supports instructions DMUL, DMULU, DMUH, DMUHU.  */
#define ISA_HAS_R6DMUL		(TARGET_64BIT && mips_isa_rev >= 6)

/* For Loongson, it is preferable to use the Loongson-specific division and
   modulo instructions instead of the regular (D)DIV(U) instruction,
   because the former are faster and can also have the effect of reducing
   code size.  */
#define ISA_AVOID_DIV_HILO	((TARGET_LOONGSON_2EF			\
				  || TARGET_LOONGSON_3A)		\
				 && !TARGET_MIPS16)

/* ISA supports instructions DDIV and DDIVU. */
#define ISA_HAS_DDIV		(TARGET_64BIT				\
				 && !TARGET_MIPS5900			\
				 && !ISA_AVOID_DIV_HILO			\
				 && mips_isa_rev <= 5)

/* ISA supports instructions DIV and DIVU.
   This is always true, but the macro is needed for ISA_HAS_<D>DIV
   in mips.md.  */
#define ISA_HAS_DIV		(!ISA_AVOID_DIV_HILO			\
				 && mips_isa_rev <= 5)

/* ISA supports instructions DIV, DIVU, MOD and MODU.  */
#define ISA_HAS_R6DIV		(mips_isa_rev >= 6)

/* ISA supports instructions DDIV, DDIVU, DMOD and DMODU.  */
#define ISA_HAS_R6DDIV		(TARGET_64BIT && mips_isa_rev >= 6)

/* ISA has the floating-point conditional move instructions introduced
   in mips4.  */
#define ISA_HAS_FP_CONDMOVE	((ISA_MIPS4				\
				  || (mips_isa_rev >= 1			\
				      && mips_isa_rev <= 5))		\
				 && !TARGET_MIPS5500			\
				 && !TARGET_MIPS16)

/* ISA has the integer conditional move instructions introduced in mips4 and
   ST Loongson 2E/2F.  */
#define ISA_HAS_CONDMOVE        (ISA_HAS_FP_CONDMOVE			\
				 || TARGET_MIPS5900			\
				 || TARGET_LOONGSON_2EF)

/* ISA has LDC1 and SDC1.  */
#define ISA_HAS_LDC1_SDC1	(!ISA_MIPS1				\
				 && !TARGET_MIPS5900			\
				 && !TARGET_MIPS16)

/* ISA has the mips4 FP condition code instructions: FP-compare to CC,
   branch on CC, and move (both FP and non-FP) on CC.  */
#define ISA_HAS_8CC		(ISA_MIPS4				\
				 || (mips_isa_rev >= 1			\
				     && mips_isa_rev <= 5))

/* ISA has the FP condition code instructions that store the flag in an
   FP register.  */
#define ISA_HAS_CCF		(mips_isa_rev >= 6)

#define ISA_HAS_SEL		(mips_isa_rev >= 6)

/* This is a catch all for other mips4 instructions: indexed load, the
   FP madd and msub instructions, and the FP recip and recip sqrt
   instructions.  Note that this macro should only be used by other
   ISA_HAS_* macros.  */
#define ISA_HAS_FP4		((ISA_MIPS4				\
				  || ISA_MIPS64				\
				  || (mips_isa_rev >= 2			\
				      && mips_isa_rev <= 5))		\
				 && !TARGET_MIPS16)

/* ISA has floating-point indexed load and store instructions
   (LWXC1, LDXC1, SWXC1 and SDXC1).  */
#define ISA_HAS_LXC1_SXC1	(ISA_HAS_FP4				\
				 && mips_lxc1_sxc1)

/* ISA has paired-single instructions.  */
#define ISA_HAS_PAIRED_SINGLE	((ISA_MIPS64				\
				  || (mips_isa_rev >= 2			\
				      && mips_isa_rev <= 5))		\
				 && !TARGET_OCTEON)

/* ISA has conditional trap instructions.  */
#define ISA_HAS_COND_TRAP	(!ISA_MIPS1				\
				 && !TARGET_MIPS16)

/* ISA has conditional trap with immediate instructions.  */
#define ISA_HAS_COND_TRAPI	(!ISA_MIPS1				\
				 && mips_isa_rev <= 5			\
				 && !TARGET_MIPS16)

/* ISA has integer multiply-accumulate instructions, madd and msub.  */
#define ISA_HAS_MADD_MSUB	(mips_isa_rev >= 1			\
				 && mips_isa_rev <= 5)

/* Integer multiply-accumulate instructions should be generated.  */
#define GENERATE_MADD_MSUB	(TARGET_IMADD && !TARGET_MIPS16)

/* ISA has 4 operand fused madd instructions of the form
   'd = [+-] (a * b [+-] c)'.  */
#define ISA_HAS_FUSED_MADD4	(mips_madd4				\
				 && (TARGET_MIPS8000			\
				     || TARGET_LOONGSON_3A))

/* ISA has 4 operand unfused madd instructions of the form
   'd = [+-] (a * b [+-] c)'.  */
#define ISA_HAS_UNFUSED_MADD4	(mips_madd4				\
				 && ISA_HAS_FP4				\
				 && !TARGET_MIPS8000			\
				 && !TARGET_LOONGSON_3A)

/* ISA has 3 operand r6 fused madd instructions of the form
   'c = c [+-] (a * b)'.  */
#define ISA_HAS_FUSED_MADDF	(mips_isa_rev >= 6)

/* ISA has 3 operand loongson fused madd instructions of the form
   'c = [+-] (a * b [+-] c)'.  */
#define ISA_HAS_FUSED_MADD3	TARGET_LOONGSON_2EF

/* ISA has floating-point RECIP.fmt and RSQRT.fmt instructions.  The
   MIPS64 rev. 1 ISA says that RECIP.D and RSQRT.D are unpredictable when
   doubles are stored in pairs of FPRs, so for safety's sake, we apply
   this restriction to the MIPS IV ISA too.  */
#define ISA_HAS_FP_RECIP_RSQRT(MODE)					\
				(((ISA_HAS_FP4				\
				   && ((MODE) == SFmode			\
				       || ((TARGET_FLOAT64		\
					    || mips_isa_rev >= 2)	\
					   && (MODE) == DFmode)))	\
				  || (((MODE) == SFmode			\
				       || (MODE) == DFmode)		\
				      && (mips_isa_rev >= 6))		\
				  || (TARGET_SB1			\
				      && (MODE) == V2SFmode))		\
				 && !TARGET_MIPS16)

#define ISA_HAS_LWL_LWR		(mips_isa_rev <= 5 && !TARGET_MIPS16)

#define ISA_HAS_IEEE_754_LEGACY	(mips_isa_rev <= 5)

#define ISA_HAS_IEEE_754_2008	(mips_isa_rev >= 2)

/* ISA has count leading zeroes/ones instruction (not implemented).  */
#define ISA_HAS_CLZ_CLO		(mips_isa_rev >= 1 && !TARGET_MIPS16)

/* ISA has three operand multiply instructions that put
   the high part in an accumulator: mulhi or mulhiu.  */
#define ISA_HAS_MULHI		((TARGET_MIPS5400			 \
				  || TARGET_MIPS5500			 \
				  || TARGET_SR71K)			 \
				 && !TARGET_MIPS16)

/* ISA has three operand multiply instructions that negate the
   result and put the result in an accumulator.  */
#define ISA_HAS_MULS		((TARGET_MIPS5400			\
				  || TARGET_MIPS5500			\
				  || TARGET_SR71K)			\
				 && !TARGET_MIPS16)

/* ISA has three operand multiply instructions that subtract the
   result from a 4th operand and put the result in an accumulator.  */
#define ISA_HAS_MSAC		((TARGET_MIPS5400			\
				  || TARGET_MIPS5500			\
				  || TARGET_SR71K)			\
				 && !TARGET_MIPS16)

/* ISA has three operand multiply instructions that add the result
   to a 4th operand and put the result in an accumulator.  */
#define ISA_HAS_MACC		((TARGET_MIPS4120			\
				  || TARGET_MIPS4130			\
				  || TARGET_MIPS5400			\
				  || TARGET_MIPS5500			\
				  || TARGET_SR71K)			\
				 && !TARGET_MIPS16)

/* ISA has NEC VR-style MACC, MACCHI, DMACC and DMACCHI instructions.  */
#define ISA_HAS_MACCHI		((TARGET_MIPS4120			\
				  || TARGET_MIPS4130)			\
				 && !TARGET_MIPS16)

/* ISA has the "ror" (rotate right) instructions.  */
#define ISA_HAS_ROR		((mips_isa_rev >= 2			\
				  || TARGET_MIPS5400			\
				  || TARGET_MIPS5500			\
				  || TARGET_SR71K			\
				  || TARGET_SMARTMIPS)			\
				 && !TARGET_MIPS16)

/* ISA has the WSBH (word swap bytes within halfwords) instruction.
   64-bit targets also provide DSBH and DSHD.  */
#define ISA_HAS_WSBH		(mips_isa_rev >= 2 && !TARGET_MIPS16)

/* ISA has data prefetch instructions.  This controls use of 'pref'.  */
#define ISA_HAS_PREFETCH	((ISA_MIPS4				\
				  || TARGET_LOONGSON_2EF		\
				  || TARGET_MIPS5900			\
				  || mips_isa_rev >= 1)			\
				 && !TARGET_MIPS16)

/* ISA has data prefetch, LL and SC with limited 9-bit displacement.  */
#define ISA_HAS_9BIT_DISPLACEMENT	(mips_isa_rev >= 6)

/* ISA has data indexed prefetch instructions.  This controls use of
   'prefx', along with TARGET_HARD_FLOAT and TARGET_DOUBLE_FLOAT.
   (prefx is a cop1x instruction, so can only be used if FP is
   enabled.)  */
#define ISA_HAS_PREFETCHX	ISA_HAS_FP4

/* True if trunc.w.s and trunc.w.d are real (not synthetic)
   instructions.  Both require TARGET_HARD_FLOAT, and trunc.w.d
   also requires TARGET_DOUBLE_FLOAT.  */
#define ISA_HAS_TRUNC_W		(!ISA_MIPS1)

/* ISA includes the MIPS32r2 seb and seh instructions.  */
#define ISA_HAS_SEB_SEH		(mips_isa_rev >= 2 && !TARGET_MIPS16)

/* ISA includes the MIPS32/64 rev 2 ext and ins instructions.  */
#define ISA_HAS_EXT_INS		(mips_isa_rev >= 2 && !TARGET_MIPS16)

/* ISA has instructions for accessing top part of 64-bit fp regs.  */
#define ISA_HAS_MXHC1		(!TARGET_FLOAT32	\
				 && mips_isa_rev >= 2)

/* ISA has lwxs instruction (load w/scaled index address.  */
#define ISA_HAS_LWXS		((TARGET_SMARTMIPS || TARGET_MICROMIPS) \
				 && !TARGET_MIPS16)

/* ISA has lbx, lbux, lhx, lhx, lhux, lwx, lwux, or ldx instruction. */
#define ISA_HAS_LBX		(TARGET_OCTEON2)
#define ISA_HAS_LBUX		(ISA_HAS_DSP || TARGET_OCTEON2)
#define ISA_HAS_LHX		(ISA_HAS_DSP || TARGET_OCTEON2)
#define ISA_HAS_LHUX		(TARGET_OCTEON2)
#define ISA_HAS_LWX		(ISA_HAS_DSP || TARGET_OCTEON2)
#define ISA_HAS_LWUX		(TARGET_OCTEON2 && TARGET_64BIT)
#define ISA_HAS_LDX		((ISA_HAS_DSP || TARGET_OCTEON2) \
				 && TARGET_64BIT)

/* The DSP ASE is available.  */
#define ISA_HAS_DSP		(TARGET_DSP && !TARGET_MIPS16)

/* Revision 2 of the DSP ASE is available.  */
#define ISA_HAS_DSPR2		(TARGET_DSPR2 && !TARGET_MIPS16)

/* The MSA ASE is available.  */
#define ISA_HAS_MSA		(TARGET_MSA && !TARGET_MIPS16)

/* True if the result of a load is not available to the next instruction.
   A nop will then be needed between instructions like "lw $4,..."
   and "addiu $4,$4,1".  */
#define ISA_HAS_LOAD_DELAY	(ISA_MIPS1				\
				 && !TARGET_MIPS3900			\
				 && !TARGET_MIPS5900			\
				 && !TARGET_MIPS16			\
				 && !TARGET_MICROMIPS)

/* Likewise mtc1 and mfc1.  */
#define ISA_HAS_XFER_DELAY	(mips_isa <= 3			\
				 && !TARGET_MIPS5900		\
				 && !TARGET_LOONGSON_2EF)

/* Likewise floating-point comparisons.  */
#define ISA_HAS_FCMP_DELAY	(mips_isa <= 3			\
				 && !TARGET_MIPS5900		\
				 && !TARGET_LOONGSON_2EF)

/* True if mflo and mfhi can be immediately followed by instructions
   which write to the HI and LO registers.

   According to MIPS specifications, MIPS ISAs I, II, and III need
   (at least) two instructions between the reads of HI/LO and
   instructions which write them, and later ISAs do not.  Contradicting
   the MIPS specifications, some MIPS IV processor user manuals (e.g.
   the UM for the NEC Vr5000) document needing the instructions between
   HI/LO reads and writes, as well.  Therefore, we declare only MIPS32,
   MIPS64 and later ISAs to have the interlocks, plus any specific
   earlier-ISA CPUs for which CPU documentation declares that the
   instructions are really interlocked.  */
#define ISA_HAS_HILO_INTERLOCKS	(mips_isa_rev >= 1			\
				 || TARGET_MIPS5500			\
				 || TARGET_MIPS5900			\
				 || TARGET_LOONGSON_2EF)

/* ISA includes synci, jr.hb and jalr.hb.  */
#define ISA_HAS_SYNCI (mips_isa_rev >= 2 && !TARGET_MIPS16)

/* ISA includes sync.  */
#define ISA_HAS_SYNC ((mips_isa >= 2 || TARGET_MIPS3900) && !TARGET_MIPS16)
#define GENERATE_SYNC			\
  (target_flags_explicit & MASK_LLSC	\
   ? TARGET_LLSC && !TARGET_MIPS16	\
   : ISA_HAS_SYNC)

/* ISA includes ll and sc.  Note that this implies ISA_HAS_SYNC
   because the expanders use both ISA_HAS_SYNC and ISA_HAS_LL_SC
   instructions.  */
#define ISA_HAS_LL_SC (mips_isa >= 2 && !TARGET_MIPS5900 && !TARGET_MIPS16)
#define GENERATE_LL_SC			\
  (target_flags_explicit & MASK_LLSC	\
   ? TARGET_LLSC && !TARGET_MIPS16	\
   : ISA_HAS_LL_SC)

#define ISA_HAS_SWAP (TARGET_XLP)
#define ISA_HAS_LDADD (TARGET_XLP)

/* ISA includes the baddu instruction.  */
#define ISA_HAS_BADDU		(TARGET_OCTEON && !TARGET_MIPS16)

/* ISA includes the bbit* instructions.  */
#define ISA_HAS_BBIT		(TARGET_OCTEON && !TARGET_MIPS16)

/* ISA includes the cins instruction.  */
#define ISA_HAS_CINS		(TARGET_OCTEON && !TARGET_MIPS16)

/* ISA includes the exts instruction.  */
#define ISA_HAS_EXTS		(TARGET_OCTEON && !TARGET_MIPS16)

/* ISA includes the seq and sne instructions.  */
#define ISA_HAS_SEQ_SNE		(TARGET_OCTEON && !TARGET_MIPS16)

/* ISA includes the pop instruction.  */
#define ISA_HAS_POP		(TARGET_OCTEON && !TARGET_MIPS16)

/* The CACHE instruction is available in non-MIPS16 code.  */
#define TARGET_CACHE_BUILTIN (mips_isa >= 3)

/* The CACHE instruction is available.  */
#define ISA_HAS_CACHE (TARGET_CACHE_BUILTIN && !TARGET_MIPS16)

/* Tell collect what flags to pass to nm.  */
#ifndef NM_FLAGS
#define NM_FLAGS "-Bn"
#endif


/* SUBTARGET_ASM_DEBUGGING_SPEC handles passing debugging options to
   the assembler.  It may be overridden by subtargets.

   Beginning with gas 2.13, -mdebug must be passed to correctly handle
   COFF debugging info.  */

#ifndef SUBTARGET_ASM_DEBUGGING_SPEC
#define SUBTARGET_ASM_DEBUGGING_SPEC "\
%{g} %{g0} %{g1} %{g2} %{g3} \
%{ggdb:-g} %{ggdb0:-g0} %{ggdb1:-g1} %{ggdb2:-g2} %{ggdb3:-g3} \
%{gstabs:-g} %{gstabs0:-g0} %{gstabs1:-g1} %{gstabs2:-g2} %{gstabs3:-g3} \
%{gstabs+:-g} %{gstabs+0:-g0} %{gstabs+1:-g1} %{gstabs+2:-g2} %{gstabs+3:-g3} \
%{gcoff:-g} %{gcoff0:-g0} %{gcoff1:-g1} %{gcoff2:-g2} %{gcoff3:-g3} \
%{gcoff*:-mdebug} %{!gcoff*:-no-mdebug}"
#endif

/* FP_ASM_SPEC represents the floating-point options that must be passed
   to the assembler when FPXX support exists.  Prior to that point the
   assembler could accept the options but were not required for
   correctness.  We only add the options when absolutely necessary
   because passing -msoft-float to the assembler will cause it to reject
   all hard-float instructions which may require some user code to be
   updated.  */

#ifdef HAVE_AS_DOT_MODULE
#define FP_ASM_SPEC "\
%{mhard-float} %{msoft-float} \
%{msingle-float} %{mdouble-float}"
#else
#define FP_ASM_SPEC
#endif

/* SUBTARGET_ASM_SPEC is always passed to the assembler.  It may be
   overridden by subtargets.  */

#ifndef SUBTARGET_ASM_SPEC
#define SUBTARGET_ASM_SPEC ""
#endif

#undef ASM_SPEC
#define ASM_SPEC "\
%{G*} %(endian_spec) %{mips1} %{mips2} %{mips3} %{mips4} \
%{mips32*} %{mips64*} \
%{mips16} %{mno-mips16:-no-mips16} \
%{mmicromips} %{mno-micromips} \
%{mips3d} %{mno-mips3d:-no-mips3d} \
%{mdmx} %{mno-mdmx:-no-mdmx} \
%{mdsp} %{mno-dsp} \
%{mdspr2} %{mno-dspr2} \
%{mmcu} %{mno-mcu} \
%{meva} %{mno-eva} \
%{mvirt} %{mno-virt} \
%{mxpa} %{mno-xpa} \
%{mmsa} %{mno-msa} \
%{msmartmips} %{mno-smartmips} \
%{mmt} %{mno-mt} \
%{mfix-rm7000} %{mno-fix-rm7000} \
%{mfix-vr4120} %{mfix-vr4130} \
%{mfix-24k} \
%{noasmopt:-O0; O0|fno-delayed-branch:-O1; O*:-O2; :-O1} \
%(subtarget_asm_debugging_spec) \
%{mabi=*} %{!mabi=*: %(asm_abi_default_spec)} \
%{mgp32} %{mgp64} %{march=*} %{mxgot:-xgot} \
%{mfp32} %{mfpxx} %{mfp64} %{mnan=*} \
%{modd-spreg} %{mno-odd-spreg} \
%{mshared} %{mno-shared} \
%{msym32} %{mno-sym32} \
%{mtune=*}" \
FP_ASM_SPEC "\
%(subtarget_asm_spec)"

/* Extra switches sometimes passed to the linker.  */

#ifndef LINK_SPEC
#define LINK_SPEC "\
%(endian_spec) \
%{G*} %{mips1} %{mips2} %{mips3} %{mips4} %{mips32*} %{mips64*} \
%{shared}"
#endif  /* LINK_SPEC defined */


/* Specs for the compiler proper */

/* SUBTARGET_CC1_SPEC is passed to the compiler proper.  It may be
   overridden by subtargets.  */
#ifndef SUBTARGET_CC1_SPEC
#define SUBTARGET_CC1_SPEC ""
#endif

/* CC1_SPEC is the set of arguments to pass to the compiler proper.  */

#undef CC1_SPEC
#define CC1_SPEC "\
%{G*} %{EB:-meb} %{EL:-mel} %{EB:%{EL:%emay not use both -EB and -EL}} \
%(subtarget_cc1_spec)"

/* Preprocessor specs.  */

/* SUBTARGET_CPP_SPEC is passed to the preprocessor.  It may be
   overridden by subtargets.  */
#ifndef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC ""
#endif

#define CPP_SPEC "%(subtarget_cpp_spec)"

/* This macro defines names of additional specifications to put in the specs
   that can be used in various specifications like CC1_SPEC.  Its definition
   is an initializer with a subgrouping for each command option.

   Each subgrouping contains a string constant, that defines the
   specification name, and a string constant that used by the GCC driver
   program.

   Do not define this macro if it does not need to do anything.  */

#define EXTRA_SPECS							\
  { "subtarget_cc1_spec", SUBTARGET_CC1_SPEC },				\
  { "subtarget_cpp_spec", SUBTARGET_CPP_SPEC },				\
  { "subtarget_asm_debugging_spec", SUBTARGET_ASM_DEBUGGING_SPEC },	\
  { "subtarget_asm_spec", SUBTARGET_ASM_SPEC },				\
  { "asm_abi_default_spec", "-" MULTILIB_ABI_DEFAULT },			\
  { "endian_spec", ENDIAN_SPEC },					\
  SUBTARGET_EXTRA_SPECS

#ifndef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS
#endif

#define DBX_DEBUGGING_INFO 1		/* generate stabs (OSF/rose) */
#define DWARF2_DEBUGGING_INFO 1         /* dwarf2 debugging info */

#ifndef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG
#endif

/* The size of DWARF addresses should be the same as the size of symbols
   in the target file format.  They shouldn't depend on things like -msym32,
   because many DWARF consumers do not allow the mixture of address sizes
   that one would then get from linking -msym32 code with -msym64 code.

   Note that the default POINTER_SIZE test is not appropriate for MIPS.
   EABI64 has 64-bit pointers but uses 32-bit ELF.  */
#define DWARF2_ADDR_SIZE (FILE_HAS_64BIT_SYMBOLS ? 8 : 4)

/* By default, turn on GDB extensions.  */
#define DEFAULT_GDB_EXTENSIONS 1

/* Registers may have a prefix which can be ignored when matching
   user asm and register definitions.  */
#ifndef REGISTER_PREFIX
#define REGISTER_PREFIX    "$"
#endif

/* Local compiler-generated symbols must have a prefix that the assembler
   understands.   By default, this is $, although some targets (e.g.,
   NetBSD-ELF) need to override this.  */

#ifndef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX	"$"
#endif

/* By default on the mips, external symbols do not have an underscore
   prepended, but some targets (e.g., NetBSD) require this.  */

#ifndef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX	""
#endif

/* On Sun 4, this limit is 2048.  We use 1500 to be safe,
   since the length can run past this up to a continuation point.  */
#undef DBX_CONTIN_LENGTH
#define DBX_CONTIN_LENGTH 1500

/* How to renumber registers for dbx and gdb.  */
#define DBX_REGISTER_NUMBER(REGNO) mips_dbx_regno[REGNO]

/* The mapping from gcc register number to DWARF 2 CFA column number.  */
#define DWARF_FRAME_REGNUM(REGNO) mips_dwarf_regno[REGNO]

/* The DWARF 2 CFA column which tracks the return address.  */
#define DWARF_FRAME_RETURN_COLUMN RETURN_ADDR_REGNUM

/* Before the prologue, RA lives in r31.  */
#define INCOMING_RETURN_ADDR_RTX gen_rtx_REG (Pmode, RETURN_ADDR_REGNUM)

/* Describe how we implement __builtin_eh_return.  */
#define EH_RETURN_DATA_REGNO(N) \
  ((N) < (TARGET_MIPS16 ? 2 : 4) ? (N) + GP_ARG_FIRST : INVALID_REGNUM)

#define EH_RETURN_STACKADJ_RTX  gen_rtx_REG (Pmode, GP_REG_FIRST + 3)

#define EH_USES(N) mips_eh_uses (N)

/* Offsets recorded in opcodes are a multiple of this alignment factor.
   The default for this in 64-bit mode is 8, which causes problems with
   SFmode register saves.  */
#define DWARF_CIE_DATA_ALIGNMENT -4

/* Correct the offset of automatic variables and arguments.  Note that
   the MIPS debug format wants all automatic variables and arguments
   to be in terms of the virtual frame pointer (stack pointer before
   any adjustment in the function), while the MIPS 3.0 linker wants
   the frame pointer to be the stack pointer after the initial
   adjustment.  */

#define DEBUGGER_AUTO_OFFSET(X)				\
  mips_debugger_offset (X, (HOST_WIDE_INT) 0)
#define DEBUGGER_ARG_OFFSET(OFFSET, X)			\
  mips_debugger_offset (X, (HOST_WIDE_INT) OFFSET)

/* Target machine storage layout */

#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN (TARGET_BIG_ENDIAN != 0)
#define WORDS_BIG_ENDIAN (TARGET_BIG_ENDIAN != 0)

#define MAX_BITS_PER_WORD 64

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD (TARGET_64BIT ? 8 : 4)
#ifndef IN_LIBGCC2
#define MIN_UNITS_PER_WORD 4
#endif

/* Width of a MSA vector register in bytes.  */
#define UNITS_PER_MSA_REG 16
/* Width of a MSA vector register in bits.  */
#define BITS_PER_MSA_REG (UNITS_PER_MSA_REG * BITS_PER_UNIT)

/* For MIPS, width of a floating point register.  */
#define UNITS_PER_FPREG (TARGET_FLOAT64 ? 8 : 4)

/* The number of consecutive floating-point registers needed to store the
   largest format supported by the FPU.  */
#define MAX_FPRS_PER_FMT (TARGET_FLOAT64 || TARGET_SINGLE_FLOAT ? 1 : 2)

/* The number of consecutive floating-point registers needed to store the
   smallest format supported by the FPU.  */
#define MIN_FPRS_PER_FMT \
  (TARGET_ODD_SPREG ? 1 : MAX_FPRS_PER_FMT)

/* The largest size of value that can be held in floating-point
   registers and moved with a single instruction.  */
#define UNITS_PER_HWFPVALUE \
  (TARGET_SOFT_FLOAT_ABI ? 0 : MAX_FPRS_PER_FMT * UNITS_PER_FPREG)

/* The largest size of value that can be held in floating-point
   registers.  */
#define UNITS_PER_FPVALUE			\
  (TARGET_SOFT_FLOAT_ABI ? 0			\
   : TARGET_SINGLE_FLOAT ? UNITS_PER_FPREG	\
   : LONG_DOUBLE_TYPE_SIZE / BITS_PER_UNIT)

/* The number of bytes in a double.  */
#define UNITS_PER_DOUBLE (TYPE_PRECISION (double_type_node) / BITS_PER_UNIT)

/* Set the sizes of the core types.  */
#define SHORT_TYPE_SIZE 16
#define INT_TYPE_SIZE 32
#define LONG_TYPE_SIZE (TARGET_LONG64 ? 64 : 32)
#define LONG_LONG_TYPE_SIZE 64

#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 64
#define LONG_DOUBLE_TYPE_SIZE (TARGET_NEWABI ? 128 : 64)

/* Define the sizes of fixed-point types.  */
#define SHORT_FRACT_TYPE_SIZE 8
#define FRACT_TYPE_SIZE 16
#define LONG_FRACT_TYPE_SIZE 32
#define LONG_LONG_FRACT_TYPE_SIZE 64

#define SHORT_ACCUM_TYPE_SIZE 16
#define ACCUM_TYPE_SIZE 32
#define LONG_ACCUM_TYPE_SIZE 64
/* FIXME.  LONG_LONG_ACCUM_TYPE_SIZE should be 128 bits, but GCC
   doesn't support 128-bit integers for MIPS32 currently.  */
#define LONG_LONG_ACCUM_TYPE_SIZE (TARGET_64BIT ? 128 : 64)

/* long double is not a fixed mode, but the idea is that, if we
   support long double, we also want a 128-bit integer type.  */
#define MAX_FIXED_MODE_SIZE LONG_DOUBLE_TYPE_SIZE

/* Width in bits of a pointer.  */
#ifndef POINTER_SIZE
#define POINTER_SIZE ((TARGET_LONG64 && TARGET_64BIT) ? 64 : 32)
#endif

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY BITS_PER_WORD

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 32

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Every structure's size must be a multiple of this.  */
/* 8 is observed right on a DECstation and on riscos 4.02.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* There is no point aligning anything to a rounder boundary than
   LONG_DOUBLE_TYPE_SIZE, unless under MSA the bigggest alignment is
   BITS_PER_MSA_REG.  */
#define BIGGEST_ALIGNMENT \
  (ISA_HAS_MSA ? BITS_PER_MSA_REG : LONG_DOUBLE_TYPE_SIZE)

/* All accesses must be aligned.  */
#define STRICT_ALIGNMENT 1

/* Define this if you wish to imitate the way many other C compilers
   handle alignment of bitfields and the structures that contain
   them.

   The behavior is that the type written for a bit-field (`int',
   `short', or other integer type) imposes an alignment for the
   entire structure, as if the structure really did contain an
   ordinary field of that type.  In addition, the bit-field is placed
   within the structure so that it would fit within such a field,
   not crossing a boundary for it.

   Thus, on most machines, a bit-field whose type is written as `int'
   would not cross a four-byte boundary, and would force four-byte
   alignment for the whole structure.  (The alignment used may not
   be four bytes; it is controlled by the other alignment
   parameters.)

   If the macro is defined, its definition should be a C expression;
   a nonzero value for the expression enables this behavior.  */

#define PCC_BITFIELD_TYPE_MATTERS 1

/* If defined, a C expression to compute the alignment given to a
   constant that is being placed in memory.  CONSTANT is the constant
   and ALIGN is the alignment that the object would ordinarily have.
   The value of this macro is used instead of that alignment to align
   the object.

   If this macro is not defined, then ALIGN is used.

   The typical use of this macro is to increase alignment for string
   constants to be word aligned so that `strcpy' calls that copy
   constants can be done inline.  */

#define CONSTANT_ALIGNMENT(EXP, ALIGN)					\
  ((TREE_CODE (EXP) == STRING_CST  || TREE_CODE (EXP) == CONSTRUCTOR)	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

/* If defined, a C expression to compute the alignment for a static
   variable.  TYPE is the data type, and ALIGN is the alignment that
   the object would ordinarily have.  The value of this macro is used
   instead of that alignment to align the object.

   If this macro is not defined, then ALIGN is used.

   One use of this macro is to increase alignment of medium-size
   data to make it all fit in fewer cache lines.  Another is to
   cause character arrays to be word-aligned so that `strcpy' calls
   that copy constants to character arrays can be done inline.  */

#undef DATA_ALIGNMENT
#define DATA_ALIGNMENT(TYPE, ALIGN)					\
  ((((ALIGN) < BITS_PER_WORD)						\
    && (TREE_CODE (TYPE) == ARRAY_TYPE					\
	|| TREE_CODE (TYPE) == UNION_TYPE				\
	|| TREE_CODE (TYPE) == RECORD_TYPE)) ? BITS_PER_WORD : (ALIGN))

/* We need this for the same reason as DATA_ALIGNMENT, namely to cause
   character arrays to be word-aligned so that `strcpy' calls that copy
   constants to character arrays can be done inline, and 'strcmp' can be
   optimised to use word loads. */
#define LOCAL_ALIGNMENT(TYPE, ALIGN) \
  DATA_ALIGNMENT (TYPE, ALIGN)
  
#define PAD_VARARGS_DOWN \
  (FUNCTION_ARG_PADDING (TYPE_MODE (type), type) == downward)

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS 1

/* When in 64-bit mode, move insns will sign extend SImode and CCmode
   moves.  All other references are zero extended.  */
#define LOAD_EXTEND_OP(MODE) \
  (TARGET_64BIT && ((MODE) == SImode || (MODE) == CCmode) \
   ? SIGN_EXTEND : ZERO_EXTEND)

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases,
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.  */

#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)	\
  if (GET_MODE_CLASS (MODE) == MODE_INT		\
      && GET_MODE_SIZE (MODE) < UNITS_PER_WORD) \
    {                                           \
      if ((MODE) == SImode)                     \
        (UNSIGNEDP) = 0;                        \
      (MODE) = Pmode;                           \
    }

/* Pmode is always the same as ptr_mode, but not always the same as word_mode.
   Extensions of pointers to word_mode must be signed.  */
#define POINTERS_EXTEND_UNSIGNED false

/* Define if loading short immediate values into registers sign extends.  */
#define SHORT_IMMEDIATES_SIGN_EXTEND 1

/* The [d]clz instructions have the natural values at 0.  */

#define CLZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE) \
  ((VALUE) = GET_MODE_UNIT_BITSIZE (MODE), 2)

/* Standard register usage.  */

/* Number of hardware registers.  We have:

   - 32 integer registers
   - 32 floating point registers
   - 8 condition code registers
   - 2 accumulator registers (hi and lo)
   - 32 registers each for coprocessors 0, 2 and 3
   - 4 fake registers:
	- ARG_POINTER_REGNUM
	- FRAME_POINTER_REGNUM
	- GOT_VERSION_REGNUM (see the comment above load_call<mode> for details)
	- CPRESTORE_SLOT_REGNUM
   - 2 dummy entries that were used at various times in the past.
   - 6 DSP accumulator registers (3 hi-lo pairs) for MIPS DSP ASE
   - 6 DSP control registers  */

#define FIRST_PSEUDO_REGISTER 188

/* By default, fix the kernel registers ($26 and $27), the global
   pointer ($28) and the stack pointer ($29).  This can change
   depending on the command-line options.

   Regarding coprocessor registers: without evidence to the contrary,
   it's best to assume that each coprocessor register has a unique
   use.  This can be overridden, in, e.g., mips_option_override or
   TARGET_CONDITIONAL_REGISTER_USAGE should the assumption be
   inappropriate for a particular target.  */

#define FIXED_REGISTERS							\
{									\
  1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0,			\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1,			\
  /* COP0 registers */							\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,			\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,			\
  /* COP2 registers */							\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,			\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,			\
  /* COP3 registers */							\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,			\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,			\
  /* 6 DSP accumulator registers & 6 control registers */		\
  0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1					\
}


/* Set up this array for o32 by default.

   Note that we don't mark $31 as a call-clobbered register.  The idea is
   that it's really the call instructions themselves which clobber $31.
   We don't care what the called function does with it afterwards.

   This approach makes it easier to implement sibcalls.  Unlike normal
   calls, sibcalls don't clobber $31, so the register reaches the
   called function in tact.  EPILOGUE_USES says that $31 is useful
   to the called function.  */

#define CALL_USED_REGISTERS						\
{									\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,			\
  0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0,			\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,			\
  1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,			\
  /* COP0 registers */							\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,			\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,			\
  /* COP2 registers */							\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,			\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,			\
  /* COP3 registers */							\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,			\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,			\
  /* 6 DSP accumulator registers & 6 control registers */		\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1					\
}


/* Define this since $28, though fixed, is call-saved in many ABIs.  */

#define CALL_REALLY_USED_REGISTERS                                      \
{ /* General registers.  */                                             \
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,                       \
  0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 0,                       \
  /* Floating-point registers.  */                                      \
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,			\
  1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  /* Others.  */                                                        \
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,			\
  /* COP0 registers */							\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  /* COP2 registers */							\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  /* COP3 registers */							\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  /* 6 DSP accumulator registers & 6 control registers */		\
  1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0					\
}

/* Internal macros to classify a register number as to whether it's a
   general purpose register, a floating point register, a
   multiply/divide register, or a status register.  */

#define GP_REG_FIRST 0
#define GP_REG_LAST  31
#define GP_REG_NUM   (GP_REG_LAST - GP_REG_FIRST + 1)
#define GP_DBX_FIRST 0
#define K0_REG_NUM   (GP_REG_FIRST + 26)
#define K1_REG_NUM   (GP_REG_FIRST + 27)
#define KERNEL_REG_P(REGNO)	(IN_RANGE (REGNO, K0_REG_NUM, K1_REG_NUM))

#define FP_REG_FIRST 32
#define FP_REG_LAST  63
#define FP_REG_NUM   (FP_REG_LAST - FP_REG_FIRST + 1)
#define FP_DBX_FIRST ((write_symbols == DBX_DEBUG) ? 38 : 32)

#define MD_REG_FIRST 64
#define MD_REG_LAST  65
#define MD_REG_NUM   (MD_REG_LAST - MD_REG_FIRST + 1)
#define MD_DBX_FIRST (FP_DBX_FIRST + FP_REG_NUM)

#define MSA_REG_FIRST FP_REG_FIRST
#define MSA_REG_LAST  FP_REG_LAST
#define MSA_REG_NUM   FP_REG_NUM

/* The DWARF 2 CFA column which tracks the return address from a
   signal handler context.  This means that to maintain backwards
   compatibility, no hard register can be assigned this column if it
   would need to be handled by the DWARF unwinder.  */
#define DWARF_ALT_FRAME_RETURN_COLUMN 66

#define ST_REG_FIRST 67
#define ST_REG_LAST  74
#define ST_REG_NUM   (ST_REG_LAST - ST_REG_FIRST + 1)


/* FIXME: renumber.  */
#define COP0_REG_FIRST 80
#define COP0_REG_LAST 111
#define COP0_REG_NUM (COP0_REG_LAST - COP0_REG_FIRST + 1)

#define COP0_STATUS_REG_NUM	(COP0_REG_FIRST + 12)
#define COP0_CAUSE_REG_NUM	(COP0_REG_FIRST + 13)
#define COP0_EPC_REG_NUM	(COP0_REG_FIRST + 14)

#define COP2_REG_FIRST 112
#define COP2_REG_LAST 143
#define COP2_REG_NUM (COP2_REG_LAST - COP2_REG_FIRST + 1)

#define COP3_REG_FIRST 144
#define COP3_REG_LAST 175
#define COP3_REG_NUM (COP3_REG_LAST - COP3_REG_FIRST + 1)

/* These definitions assume that COP0, 2 and 3 are numbered consecutively.  */
#define ALL_COP_REG_FIRST COP0_REG_FIRST
#define ALL_COP_REG_LAST COP3_REG_LAST
#define ALL_COP_REG_NUM (ALL_COP_REG_LAST - ALL_COP_REG_FIRST + 1)

#define DSP_ACC_REG_FIRST 176
#define DSP_ACC_REG_LAST 181
#define DSP_ACC_REG_NUM (DSP_ACC_REG_LAST - DSP_ACC_REG_FIRST + 1)

#define AT_REGNUM	(GP_REG_FIRST + 1)
#define HI_REGNUM	(TARGET_BIG_ENDIAN ? MD_REG_FIRST : MD_REG_FIRST + 1)
#define LO_REGNUM	(TARGET_BIG_ENDIAN ? MD_REG_FIRST + 1 : MD_REG_FIRST)

/* A few bitfield locations for the coprocessor registers.  */
/* Request Interrupt Priority Level is from bit 10 to bit 15 of
   the cause register for the EIC interrupt mode.  */
#define CAUSE_IPL	10
/* COP1 Enable is at bit 29 of the status register.  */
#define SR_COP1         29
/* Interrupt Priority Level is from bit 10 to bit 15 of the status register.  */
#define SR_IPL		10
/* Interrupt masks start with IM0 at bit 8 to IM7 at bit 15 of the status
   register.  */
#define SR_IM0		8
/* Exception Level is at bit 1 of the status register.  */
#define SR_EXL		1
/* Interrupt Enable is at bit 0 of the status register.  */
#define SR_IE		0

/* FPSW_REGNUM is the single condition code used if !ISA_HAS_8CC.
   If ISA_HAS_8CC, it should not be used, and an arbitrary ST_REG
   should be used instead.  */
#define FPSW_REGNUM	ST_REG_FIRST

#define GP_REG_P(REGNO)	\
  ((unsigned int) ((int) (REGNO) - GP_REG_FIRST) < GP_REG_NUM)
#define M16_REG_P(REGNO) \
  (((REGNO) >= 2 && (REGNO) <= 7) || (REGNO) == 16 || (REGNO) == 17)
#define M16STORE_REG_P(REGNO) \
  (((REGNO) >= 2 && (REGNO) <= 7) || (REGNO) == 0 || (REGNO) == 17)
#define FP_REG_P(REGNO)  \
  ((unsigned int) ((int) (REGNO) - FP_REG_FIRST) < FP_REG_NUM)
#define MD_REG_P(REGNO) \
  ((unsigned int) ((int) (REGNO) - MD_REG_FIRST) < MD_REG_NUM)
#define ST_REG_P(REGNO) \
  ((unsigned int) ((int) (REGNO) - ST_REG_FIRST) < ST_REG_NUM)
#define COP0_REG_P(REGNO) \
  ((unsigned int) ((int) (REGNO) - COP0_REG_FIRST) < COP0_REG_NUM)
#define COP2_REG_P(REGNO) \
  ((unsigned int) ((int) (REGNO) - COP2_REG_FIRST) < COP2_REG_NUM)
#define COP3_REG_P(REGNO) \
  ((unsigned int) ((int) (REGNO) - COP3_REG_FIRST) < COP3_REG_NUM)
#define ALL_COP_REG_P(REGNO) \
  ((unsigned int) ((int) (REGNO) - COP0_REG_FIRST) < ALL_COP_REG_NUM)
/* Test if REGNO is one of the 6 new DSP accumulators.  */
#define DSP_ACC_REG_P(REGNO) \
  ((unsigned int) ((int) (REGNO) - DSP_ACC_REG_FIRST) < DSP_ACC_REG_NUM)
/* Test if REGNO is hi, lo, or one of the 6 new DSP accumulators.  */
#define ACC_REG_P(REGNO) \
  (MD_REG_P (REGNO) || DSP_ACC_REG_P (REGNO))
#define MSA_REG_P(REGNO) \
  ((unsigned int) ((int) (REGNO) - MSA_REG_FIRST) < MSA_REG_NUM)

#define FP_REG_RTX_P(X) (REG_P (X) && FP_REG_P (REGNO (X)))
#define MSA_REG_RTX_P(X) (REG_P (X) && MSA_REG_P (REGNO (X)))

/* True if X is (const (unspec [(const_int 0)] UNSPEC_GP)).  This is used
   to initialize the mips16 gp pseudo register.  */
#define CONST_GP_P(X)				\
  (GET_CODE (X) == CONST			\
   && GET_CODE (XEXP (X, 0)) == UNSPEC		\
   && XINT (XEXP (X, 0), 1) == UNSPEC_GP)

/* Return coprocessor number from register number.  */

#define COPNUM_AS_CHAR_FROM_REGNUM(REGNO) 				\
  (COP0_REG_P (REGNO) ? '0' : COP2_REG_P (REGNO) ? '2'			\
   : COP3_REG_P (REGNO) ? '3' : '?')


#define HARD_REGNO_NREGS(REGNO, MODE) mips_hard_regno_nregs (REGNO, MODE)

#define HARD_REGNO_MODE_OK(REGNO, MODE)					\
  mips_hard_regno_mode_ok[ (int)(MODE) ][ (REGNO) ]

#define HARD_REGNO_RENAME_OK(OLD_REG, NEW_REG)				\
  mips_hard_regno_rename_ok (OLD_REG, NEW_REG)

/* Select a register mode required for caller save of hard regno REGNO.  */
#define HARD_REGNO_CALLER_SAVE_MODE(REGNO, NREGS, MODE) \
  mips_hard_regno_caller_save_mode (REGNO, NREGS, MODE)

#define MODES_TIEABLE_P mips_modes_tieable_p

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM (GP_REG_FIRST + 29)

/* These two registers don't really exist: they get eliminated to either
   the stack or hard frame pointer.  */
#define ARG_POINTER_REGNUM 77
#define FRAME_POINTER_REGNUM 78

/* $30 is not available on the mips16, so we use $17 as the frame
   pointer.  */
#define HARD_FRAME_POINTER_REGNUM \
  (TARGET_MIPS16 ? GP_REG_FIRST + 17 : GP_REG_FIRST + 30)

#define HARD_FRAME_POINTER_IS_FRAME_POINTER 0
#define HARD_FRAME_POINTER_IS_ARG_POINTER 0

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM (GP_REG_FIRST + 15)

/* Registers used as temporaries in prologue/epilogue code:

   - If a MIPS16 PIC function needs access to _gp, it first loads
     the value into MIPS16_PIC_TEMP and then copies it to $gp.

   - The prologue can use MIPS_PROLOGUE_TEMP as a general temporary
     register.  The register must not conflict with MIPS16_PIC_TEMP.

   - If we aren't generating MIPS16 code, the prologue can also use
     MIPS_PROLOGUE_TEMP2 as a general temporary register.

   - The epilogue can use MIPS_EPILOGUE_TEMP as a general temporary
     register.

   If we're generating MIPS16 code, these registers must come from the
   core set of 8.  The prologue registers mustn't conflict with any
   incoming arguments, the static chain pointer, or the frame pointer.
   The epilogue temporary mustn't conflict with the return registers,
   the PIC call register ($25), the frame pointer, the EH stack adjustment,
   or the EH data registers.

   If we're generating interrupt handlers, we use K0 as a temporary register
   in prologue/epilogue code.  */

#define MIPS16_PIC_TEMP_REGNUM (GP_REG_FIRST + 2)
#define MIPS_PROLOGUE_TEMP_REGNUM \
  (cfun->machine->interrupt_handler_p ? K0_REG_NUM : GP_REG_FIRST + 3)
#define MIPS_PROLOGUE_TEMP2_REGNUM \
  (TARGET_MIPS16 \
   ? (gcc_unreachable (), INVALID_REGNUM) \
   : cfun->machine->interrupt_handler_p ? K1_REG_NUM : GP_REG_FIRST + 12)
#define MIPS_EPILOGUE_TEMP_REGNUM		\
  (cfun->machine->interrupt_handler_p		\
   ? K0_REG_NUM					\
   : GP_REG_FIRST + (TARGET_MIPS16 ? 6 : 8))

#define MIPS16_PIC_TEMP gen_rtx_REG (Pmode, MIPS16_PIC_TEMP_REGNUM)
#define MIPS_PROLOGUE_TEMP(MODE) gen_rtx_REG (MODE, MIPS_PROLOGUE_TEMP_REGNUM)
#define MIPS_PROLOGUE_TEMP2(MODE) \
  gen_rtx_REG (MODE, MIPS_PROLOGUE_TEMP2_REGNUM)
#define MIPS_EPILOGUE_TEMP(MODE) gen_rtx_REG (MODE, MIPS_EPILOGUE_TEMP_REGNUM)

/* Define this macro if it is as good or better to call a constant
   function address than to call an address kept in a register.  */
#define NO_FUNCTION_CSE 1

/* The ABI-defined global pointer.  Sometimes we use a different
   register in leaf functions: see PIC_OFFSET_TABLE_REGNUM.  */
#define GLOBAL_POINTER_REGNUM (GP_REG_FIRST + 28)

/* We normally use $28 as the global pointer.  However, when generating
   n32/64 PIC, it is better for leaf functions to use a call-clobbered
   register instead.  They can then avoid saving and restoring $28
   and perhaps avoid using a frame at all.

   When a leaf function uses something other than $28, mips_expand_prologue
   will modify pic_offset_table_rtx in place.  Take the register number
   from there after reload.  */
#define PIC_OFFSET_TABLE_REGNUM \
  (reload_completed ? REGNO (pic_offset_table_rtx) : GLOBAL_POINTER_REGNUM)

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

enum reg_class
{
  NO_REGS,			/* no registers in set */
  M16_STORE_REGS,		/* microMIPS store registers  */
  M16_REGS,			/* mips16 directly accessible registers */
  M16_SP_REGS,			/* mips16 + $sp */
  T_REG,			/* mips16 T register ($24) */
  M16_T_REGS,			/* mips16 registers plus T register */
  PIC_FN_ADDR_REG,		/* SVR4 PIC function address register */
  V1_REG,			/* Register $v1 ($3) used for TLS access.  */
  SPILL_REGS,			/* All but $sp and call preserved regs are in here */
  LEA_REGS,			/* Every GPR except $25 */
  GR_REGS,			/* integer registers */
  FP_REGS,			/* floating point registers */
  MD0_REG,			/* first multiply/divide register */
  MD1_REG,			/* second multiply/divide register */
  MD_REGS,			/* multiply/divide registers (hi/lo) */
  COP0_REGS,			/* generic coprocessor classes */
  COP2_REGS,
  COP3_REGS,
  ST_REGS,			/* status registers (fp status) */
  DSP_ACC_REGS,			/* DSP accumulator registers */
  ACC_REGS,			/* Hi/Lo and DSP accumulator registers */
  FRAME_REGS,			/* $arg and $frame */
  GR_AND_MD0_REGS,		/* union classes */
  GR_AND_MD1_REGS,
  GR_AND_MD_REGS,
  GR_AND_ACC_REGS,
  ALL_REGS,			/* all registers */
  LIM_REG_CLASSES		/* max value + 1 */
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define GENERAL_REGS GR_REGS

/* An initializer containing the names of the register classes as C
   string constants.  These names are used in writing some of the
   debugging dumps.  */

#define REG_CLASS_NAMES							\
{									\
  "NO_REGS",								\
  "M16_STORE_REGS",							\
  "M16_REGS",								\
  "M16_SP_REGS",								\
  "T_REG",								\
  "M16_T_REGS",								\
  "PIC_FN_ADDR_REG",							\
  "V1_REG",								\
  "SPILL_REGS",								\
  "LEA_REGS",								\
  "GR_REGS",								\
  "FP_REGS",								\
  "MD0_REG",								\
  "MD1_REG",								\
  "MD_REGS",								\
  /* coprocessor registers */						\
  "COP0_REGS",								\
  "COP2_REGS",								\
  "COP3_REGS",								\
  "ST_REGS",								\
  "DSP_ACC_REGS",							\
  "ACC_REGS",								\
  "FRAME_REGS",								\
  "GR_AND_MD0_REGS",							\
  "GR_AND_MD1_REGS",							\
  "GR_AND_MD_REGS",							\
  "GR_AND_ACC_REGS",							\
  "ALL_REGS"								\
}

/* An initializer containing the contents of the register classes,
   as integers which are bit masks.  The Nth integer specifies the
   contents of class N.  The way the integer MASK is interpreted is
   that register R is in the class if `MASK & (1 << R)' is 1.

   When the machine has more than 32 registers, an integer does not
   suffice.  Then the integers are replaced by sub-initializers,
   braced groupings containing several integers.  Each
   sub-initializer must be suitable as an initializer for the type
   `HARD_REG_SET' which is defined in `hard-reg-set.h'.  */

#define REG_CLASS_CONTENTS						                                \
{									                                \
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000 },	/* NO_REGS */		\
  { 0x000200fc, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000 },	/* M16_STORE_REGS */	\
  { 0x000300fc, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000 },	/* M16_REGS */		\
  { 0x200300fc, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000 },	/* M16_SP_REGS */		\
  { 0x01000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000 },	/* T_REG */		\
  { 0x010300fc, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000 },	/* M16_T_REGS */	\
  { 0x02000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000 },	/* PIC_FN_ADDR_REG */	\
  { 0x00000008, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000 },	/* V1_REG */		\
  { 0x0303fffc, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000 },	/* SPILL_REGS */      	\
  { 0xfdffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000 },	/* LEA_REGS */		\
  { 0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000 },	/* GR_REGS */		\
  { 0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000 },	/* FP_REGS */		\
  { 0x00000000, 0x00000000, 0x00000001, 0x00000000, 0x00000000, 0x00000000 },	/* MD0_REG */		\
  { 0x00000000, 0x00000000, 0x00000002, 0x00000000, 0x00000000, 0x00000000 },	/* MD1_REG */		\
  { 0x00000000, 0x00000000, 0x00000003, 0x00000000, 0x00000000, 0x00000000 },	/* MD_REGS */		\
  { 0x00000000, 0x00000000, 0xffff0000, 0x0000ffff, 0x00000000, 0x00000000 },   /* COP0_REGS */		\
  { 0x00000000, 0x00000000, 0x00000000, 0xffff0000, 0x0000ffff, 0x00000000 },   /* COP2_REGS */		\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0xffff0000, 0x0000ffff },   /* COP3_REGS */		\
  { 0x00000000, 0x00000000, 0x000007f8, 0x00000000, 0x00000000, 0x00000000 },	/* ST_REGS */		\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x003f0000 },	/* DSP_ACC_REGS */	\
  { 0x00000000, 0x00000000, 0x00000003, 0x00000000, 0x00000000, 0x003f0000 },	/* ACC_REGS */		\
  { 0x00000000, 0x00000000, 0x00006000, 0x00000000, 0x00000000, 0x00000000 },	/* FRAME_REGS */	\
  { 0xffffffff, 0x00000000, 0x00000001, 0x00000000, 0x00000000, 0x00000000 },	/* GR_AND_MD0_REGS */	\
  { 0xffffffff, 0x00000000, 0x00000002, 0x00000000, 0x00000000, 0x00000000 },	/* GR_AND_MD1_REGS */	\
  { 0xffffffff, 0x00000000, 0x00000003, 0x00000000, 0x00000000, 0x00000000 },	/* GR_AND_MD_REGS */	\
  { 0xffffffff, 0x00000000, 0x00000003, 0x00000000, 0x00000000, 0x003f0000 },	/* GR_AND_ACC_REGS */	\
  { 0xffffffff, 0xffffffff, 0xffff67ff, 0xffffffff, 0xffffffff, 0x0fffffff }	/* ALL_REGS */		\
}


/* A C expression whose value is a register class containing hard
   register REGNO.  In general there is more that one such class;
   choose a class which is "minimal", meaning that no smaller class
   also contains the register.  */

#define REGNO_REG_CLASS(REGNO) mips_regno_to_class[ (REGNO) ]

/* A macro whose definition is the name of the class to which a
   valid base register must belong.  A base register is one used in
   an address which is the register value plus a displacement.  */

#define BASE_REG_CLASS  (TARGET_MIPS16 ? M16_SP_REGS : GR_REGS)

/* A macro whose definition is the name of the class to which a
   valid index register must belong.  An index register is one used
   in an address where its value is either multiplied by a scale
   factor or added to another register (as well as added to a
   displacement).  */

#define INDEX_REG_CLASS NO_REGS

/* We generally want to put call-clobbered registers ahead of
   call-saved ones.  (IRA expects this.)  */

#define REG_ALLOC_ORDER							\
{ /* Accumulator registers.  When GPRs and accumulators have equal	\
     cost, we generally prefer to use accumulators.  For example,	\
     a division of multiplication result is better allocated to LO,	\
     so that we put the MFLO at the point of use instead of at the	\
     point of definition.  It's also needed if we're to take advantage	\
     of the extra accumulators available with -mdspr2.  In some cases,	\
     it can also help to reduce register pressure.  */			\
  64, 65,176,177,178,179,180,181,					\
  /* Call-clobbered GPRs.  */						\
  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,		\
  24, 25, 31,								\
  /* The global pointer.  This is call-clobbered for o32 and o64	\
     abicalls, call-saved for n32 and n64 abicalls, and a program	\
     invariant otherwise.  Putting it between the call-clobbered	\
     and call-saved registers should cope with all eventualities.  */	\
  28,									\
  /* Call-saved GPRs.  */						\
  16, 17, 18, 19, 20, 21, 22, 23, 30,					\
  /* GPRs that can never be exposed to the register allocator.  */	\
  0,  26, 27, 29,							\
  /* Call-clobbered FPRs.  */						\
  32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,	\
  48, 49, 50, 51,							\
  /* FPRs that are usually call-saved.  The odd ones are actually	\
     call-clobbered for n32, but listing them ahead of the even		\
     registers might encourage the register allocator to fragment	\
     the available FPR pairs.  We need paired FPRs to store long	\
     doubles, so it isn't clear that using a different order		\
     for n32 would be a win.  */					\
  52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,			\
  /* None of the remaining classes have defined call-saved		\
     registers.  */							\
  66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,		\
  80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,	\
  96, 97, 98, 99, 100,101,102,103,104,105,106,107,108,109,110,111,	\
  112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,	\
  128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,	\
  144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,	\
  160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,	\
  182,183,184,185,186,187						\
}

/* True if VALUE is an unsigned 6-bit number.  */

#define UIMM6_OPERAND(VALUE) \
  (((VALUE) & ~(unsigned HOST_WIDE_INT) 0x3f) == 0)

/* True if VALUE is a signed 10-bit number.  */

#define IMM10_OPERAND(VALUE) \
  ((unsigned HOST_WIDE_INT) (VALUE) + 0x200 < 0x400)

/* True if VALUE is a signed 16-bit number.  */

#define SMALL_OPERAND(VALUE) \
  ((unsigned HOST_WIDE_INT) (VALUE) + 0x8000 < 0x10000)

/* True if VALUE is an unsigned 16-bit number.  */

#define SMALL_OPERAND_UNSIGNED(VALUE) \
  (((VALUE) & ~(unsigned HOST_WIDE_INT) 0xffff) == 0)

/* True if VALUE can be loaded into a register using LUI.  */

#define LUI_OPERAND(VALUE)					\
  (((VALUE) | 0x7fff0000) == 0x7fff0000				\
   || ((VALUE) | 0x7fff0000) + 0x10000 == 0)

/* Return a value X with the low 16 bits clear, and such that
   VALUE - X is a signed 16-bit value.  */

#define CONST_HIGH_PART(VALUE) \
  (((VALUE) + 0x8000) & ~(unsigned HOST_WIDE_INT) 0xffff)

#define CONST_LOW_PART(VALUE) \
  ((VALUE) - CONST_HIGH_PART (VALUE))

#define SMALL_INT(X) SMALL_OPERAND (INTVAL (X))
#define SMALL_INT_UNSIGNED(X) SMALL_OPERAND_UNSIGNED (INTVAL (X))
#define LUI_INT(X) LUI_OPERAND (INTVAL (X))
#define UMIPS_12BIT_OFFSET_P(OFFSET) (IN_RANGE (OFFSET, -2048, 2047))
#define MIPS_9BIT_OFFSET_P(OFFSET) (IN_RANGE (OFFSET, -256, 255))

/* The HI and LO registers can only be reloaded via the general
   registers.  Condition code registers can only be loaded to the
   general registers, and from the floating point registers.  */

#define SECONDARY_INPUT_RELOAD_CLASS(CLASS, MODE, X)			\
  mips_secondary_reload_class (CLASS, MODE, X, true)
#define SECONDARY_OUTPUT_RELOAD_CLASS(CLASS, MODE, X)			\
  mips_secondary_reload_class (CLASS, MODE, X, false)

/* When targeting the o32 FPXX ABI, all moves with a length of doubleword
   or greater must be performed by FR-mode-aware instructions.
   This can be achieved using MFHC1/MTHC1 when these instructions are
   available but otherwise moves must go via memory.
   For the o32 FP64A ABI, all odd-numbered moves with a length of
   doubleword or greater are required to use memory.  Using MTC1/MFC1
   to access the lower-half of these registers would require a forbidden
   single-precision access.  We require all double-word moves to use
   memory because adding even and odd floating-point registers classes
   would have a significant impact on the backend.  */
#define SECONDARY_MEMORY_NEEDED(CLASS1, CLASS2, MODE)			\
  mips_secondary_memory_needed ((CLASS1), (CLASS2), (MODE))

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */

#define CLASS_MAX_NREGS(CLASS, MODE) mips_class_max_nregs (CLASS, MODE)

#define CANNOT_CHANGE_MODE_CLASS(FROM, TO, CLASS) \
  mips_cannot_change_mode_class (FROM, TO, CLASS)

/* Stack layout; function entry, exit and calling.  */

#define STACK_GROWS_DOWNWARD 1

#define FRAME_GROWS_DOWNWARD flag_stack_protect

/* Size of the area allocated in the frame to save the GP.  */

#define MIPS_GP_SAVE_AREA_SIZE \
  (TARGET_CALL_CLOBBERED_GP ? MIPS_STACK_ALIGN (UNITS_PER_WORD) : 0)

/* The offset of the first local variable from the frame pointer.  See
   mips_compute_frame_info for details about the frame layout.  */

#define STARTING_FRAME_OFFSET				\
  (FRAME_GROWS_DOWNWARD					\
   ? 0							\
   : crtl->outgoing_args_size + MIPS_GP_SAVE_AREA_SIZE)

#define RETURN_ADDR_RTX mips_return_addr

/* Mask off the MIPS16 ISA bit in unwind addresses.

   The reason for this is a little subtle.  When unwinding a call,
   we are given the call's return address, which on most targets
   is the address of the following instruction.  However, what we
   actually want to find is the EH region for the call itself.
   The target-independent unwind code therefore searches for "RA - 1".

   In the MIPS16 case, RA is always an odd-valued (ISA-encoded) address.
   RA - 1 is therefore the real (even-valued) start of the return
   instruction.  EH region labels are usually odd-valued MIPS16 symbols
   too, so a search for an even address within a MIPS16 region would
   usually work.

   However, there is an exception.  If the end of an EH region is also
   the end of a function, the end label is allowed to be even.  This is
   necessary because a following non-MIPS16 function may also need EH
   information for its first instruction.

   Thus a MIPS16 region may be terminated by an ISA-encoded or a
   non-ISA-encoded address.  This probably isn't ideal, but it is
   the traditional (legacy) behavior.  It is therefore only safe
   to search MIPS EH regions for an _odd-valued_ address.

   Masking off the ISA bit means that the target-independent code
   will search for "(RA & -2) - 1", which is guaranteed to be odd.  */
#define MASK_RETURN_ADDR GEN_INT (-2)


/* Similarly, don't use the least-significant bit to tell pointers to
   code from vtable index.  */

#define TARGET_PTRMEMFUNC_VBIT_LOCATION ptrmemfunc_vbit_in_delta

/* The eliminations to $17 are only used for mips16 code.  See the
   definition of HARD_FRAME_POINTER_REGNUM.  */

#define ELIMINABLE_REGS							\
{{ ARG_POINTER_REGNUM,   STACK_POINTER_REGNUM},				\
 { ARG_POINTER_REGNUM,   GP_REG_FIRST + 30},				\
 { ARG_POINTER_REGNUM,   GP_REG_FIRST + 17},				\
 { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},				\
 { FRAME_POINTER_REGNUM, GP_REG_FIRST + 30},				\
 { FRAME_POINTER_REGNUM, GP_REG_FIRST + 17}}

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  (OFFSET) = mips_initial_elimination_offset ((FROM), (TO))

/* Allocate stack space for arguments at the beginning of each function.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* The argument pointer always points to the first argument.  */
#define FIRST_PARM_OFFSET(FNDECL) 0

/* o32 and o64 reserve stack space for all argument registers.  */
#define REG_PARM_STACK_SPACE(FNDECL) 			\
  (TARGET_OLDABI					\
   ? (MAX_ARGS_IN_REGISTERS * UNITS_PER_WORD)		\
   : 0)

/* Define this if it is the responsibility of the caller to
   allocate the area reserved for arguments passed in registers.
   If `ACCUMULATE_OUTGOING_ARGS' is also defined, the only effect
   of this macro is to determine whether the space is included in
   `crtl->outgoing_args_size'.  */
#define OUTGOING_REG_PARM_STACK_SPACE(FNTYPE) 1

#define STACK_BOUNDARY (TARGET_NEWABI ? 128 : 64)

/* Symbolic macros for the registers used to return integer and floating
   point values.  */

#define GP_RETURN (GP_REG_FIRST + 2)
#define FP_RETURN ((TARGET_SOFT_FLOAT) ? GP_RETURN : (FP_REG_FIRST + 0))

#define MAX_ARGS_IN_REGISTERS (TARGET_OLDABI ? 4 : 8)

/* Symbolic macros for the first/last argument registers.  */

#define GP_ARG_FIRST (GP_REG_FIRST + 4)
#define GP_ARG_LAST  (GP_ARG_FIRST + MAX_ARGS_IN_REGISTERS - 1)
#define FP_ARG_FIRST (FP_REG_FIRST + 12)
#define FP_ARG_LAST  (FP_ARG_FIRST + MAX_ARGS_IN_REGISTERS - 1)

/* True if MODE is vector and supported in a MSA vector register.  */
#define MSA_SUPPORTED_MODE_P(MODE)			\
  (ISA_HAS_MSA						\
   && GET_MODE_SIZE (MODE) == UNITS_PER_MSA_REG		\
   && (GET_MODE_CLASS (MODE) == MODE_VECTOR_INT		\
       || GET_MODE_CLASS (MODE) == MODE_VECTOR_FLOAT))

/* Temporary register that is used when restoring $gp after a call.  $4 and $5
   are used for returning complex double values in soft-float code, so $6 is the
   first suitable candidate for TARGET_MIPS16.  For !TARGET_MIPS16 we can use
   $gp itself as the temporary.  */
#define POST_CALL_TMP_REG \
  (TARGET_MIPS16 ? GP_ARG_FIRST + 2 : PIC_OFFSET_TABLE_REGNUM)

/* 1 if N is a possible register number for function argument passing.
   We have no FP argument registers when soft-float.  Special handling
   is required for O32 where only even numbered registers are used for
   O32-FPXX and O32-FP64.  */

#define FUNCTION_ARG_REGNO_P(N)					\
  ((IN_RANGE((N), GP_ARG_FIRST, GP_ARG_LAST)			\
    || (IN_RANGE((N), FP_ARG_FIRST, FP_ARG_LAST) 		\
        && (mips_abi != ABI_32 					\
            || TARGET_FLOAT32 					\
            || ((N) % 2 == 0))))				\
   && !fixed_regs[N])

/* This structure has to cope with two different argument allocation
   schemes.  Most MIPS ABIs view the arguments as a structure, of which
   the first N words go in registers and the rest go on the stack.  If I
   < N, the Ith word might go in Ith integer argument register or in a
   floating-point register.  For these ABIs, we only need to remember
   the offset of the current argument into the structure.

   The EABI instead allocates the integer and floating-point arguments
   separately.  The first N words of FP arguments go in FP registers,
   the rest go on the stack.  Likewise, the first N words of the other
   arguments go in integer registers, and the rest go on the stack.  We
   need to maintain three counts: the number of integer registers used,
   the number of floating-point registers used, and the number of words
   passed on the stack.

   We could keep separate information for the two ABIs (a word count for
   the standard ABIs, and three separate counts for the EABI).  But it
   seems simpler to view the standard ABIs as forms of EABI that do not
   allocate floating-point registers.

   So for the standard ABIs, the first N words are allocated to integer
   registers, and mips_function_arg decides on an argument-by-argument
   basis whether that argument should really go in an integer register,
   or in a floating-point one.  */

typedef struct mips_args {
  /* Always true for varargs functions.  Otherwise true if at least
     one argument has been passed in an integer register.  */
  int gp_reg_found;

  /* The number of arguments seen so far.  */
  unsigned int arg_number;

  /* The number of integer registers used so far.  For all ABIs except
     EABI, this is the number of words that have been added to the
     argument structure, limited to MAX_ARGS_IN_REGISTERS.  */
  unsigned int num_gprs;

  /* For EABI, the number of floating-point registers used so far.  */
  unsigned int num_fprs;

  /* The number of words passed on the stack.  */
  unsigned int stack_words;

  /* On the mips16, we need to keep track of which floating point
     arguments were passed in general registers, but would have been
     passed in the FP regs if this were a 32-bit function, so that we
     can move them to the FP regs if we wind up calling a 32-bit
     function.  We record this information in fp_code, encoded in base
     four.  A zero digit means no floating point argument, a one digit
     means an SFmode argument, and a two digit means a DFmode argument,
     and a three digit is not used.  The low order digit is the first
     argument.  Thus 6 == 1 * 4 + 2 means a DFmode argument followed by
     an SFmode argument.  ??? A more sophisticated approach will be
     needed if MIPS_ABI != ABI_32.  */
  int fp_code;

  /* True if the function has a prototype.  */
  int prototype;
} CUMULATIVE_ARGS;

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
  mips_init_cumulative_args (&CUM, FNTYPE)

#define FUNCTION_ARG_PADDING(MODE, TYPE) \
  (mips_pad_arg_upward (MODE, TYPE) ? upward : downward)

#define BLOCK_REG_PADDING(MODE, TYPE, FIRST) \
  (mips_pad_reg_upward (MODE, TYPE) ? upward : downward)

/* True if using EABI and varargs can be passed in floating-point
   registers.  Under these conditions, we need a more complex form
   of va_list, which tracks GPR, FPR and stack arguments separately.  */
#define EABI_FLOAT_VARARGS_P \
	(mips_abi == ABI_EABI && UNITS_PER_FPVALUE >= UNITS_PER_DOUBLE)


#define EPILOGUE_USES(REGNO)	mips_epilogue_uses (REGNO)

/* Treat LOC as a byte offset from the stack pointer and round it up
   to the next fully-aligned offset.  */
#define MIPS_STACK_ALIGN(LOC) \
  (TARGET_NEWABI ? ROUND_UP ((LOC), 16) : ROUND_UP ((LOC), 8))


/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO) mips_function_profiler ((FILE))

/* The profiler preserves all interesting registers, including $31.  */
#define MIPS_SAVE_REG_FOR_PROFILING_P(REGNO) false

/* No mips port has ever used the profiler counter word, so don't emit it
   or the label for it.  */

#define NO_PROFILE_COUNTERS 1

/* Define this macro if the code for function profiling should come
   before the function prologue.  Normally, the profiling code comes
   after.  */

/* #define PROFILE_BEFORE_PROLOGUE */

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 1


/* Trampolines are a block of code followed by two pointers.  */

#define TRAMPOLINE_SIZE \
  (mips_trampoline_code_size () + GET_MODE_SIZE (ptr_mode) * 2)

/* Forcing a 64-bit alignment for 32-bit targets allows us to load two
   pointers from a single LUI base.  */

#define TRAMPOLINE_ALIGNMENT 64

/* mips_trampoline_init calls this library function to flush
   program and data caches.  */

#ifndef CACHE_FLUSH_FUNC
#define CACHE_FLUSH_FUNC "_flush_cache"
#endif

#define MIPS_ICACHE_SYNC(ADDR, SIZE)					\
  /* Flush both caches.  We need to flush the data cache in case	\
     the system has a write-back cache.  */				\
  emit_library_call (gen_rtx_SYMBOL_REF (Pmode, mips_cache_flush_func),	\
		     LCT_NORMAL, VOIDmode, ADDR, Pmode, SIZE, Pmode,	\
		     GEN_INT (3), TYPE_MODE (integer_type_node))


/* Addressing modes, and classification of registers for them.  */

#define REGNO_OK_FOR_INDEX_P(REGNO) 0
#define REGNO_MODE_OK_FOR_BASE_P(REGNO, MODE) \
  mips_regno_mode_ok_for_base_p (REGNO, MODE, 1)

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 1

/* Check for constness inline but use mips_legitimate_address_p
   to check whether a constant really is an address.  */

#define CONSTANT_ADDRESS_P(X) \
  (CONSTANT_P (X) && memory_address_p (SImode, X))

/* This handles the magic '..CURRENT_FUNCTION' symbol, which means
   'the start of the function that this code is output in'.  */

#define ASM_OUTPUT_LABELREF(FILE,NAME)  \
  if (strcmp (NAME, "..CURRENT_FUNCTION") == 0)				\
    asm_fprintf ((FILE), "%U%s",					\
		 XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0));	\
  else									\
    asm_fprintf ((FILE), "%U%s", (NAME))

/* Flag to mark a function decl symbol that requires a long call.  */
#define SYMBOL_FLAG_LONG_CALL	(SYMBOL_FLAG_MACH_DEP << 0)
#define SYMBOL_REF_LONG_CALL_P(X)					\
  ((SYMBOL_REF_FLAGS (X) & SYMBOL_FLAG_LONG_CALL) != 0)

/* This flag marks functions that cannot be lazily bound.  */
#define SYMBOL_FLAG_BIND_NOW (SYMBOL_FLAG_MACH_DEP << 1)
#define SYMBOL_REF_BIND_NOW_P(RTX) \
  ((SYMBOL_REF_FLAGS (RTX) & SYMBOL_FLAG_BIND_NOW) != 0)

/* True if we're generating a form of MIPS16 code in which jump tables
   are stored in the text section and encoded as 16-bit PC-relative
   offsets.  This is only possible when general text loads are allowed,
   since the table access itself will be an "lh" instruction.  If the
   PC-relative offsets grow too large, 32-bit offsets are used instead.  */
#define TARGET_MIPS16_SHORT_JUMP_TABLES TARGET_MIPS16_TEXT_LOADS

#define JUMP_TABLES_IN_TEXT_SECTION TARGET_MIPS16_SHORT_JUMP_TABLES

#define CASE_VECTOR_MODE (TARGET_MIPS16_SHORT_JUMP_TABLES ? SImode : ptr_mode)

/* Only use short offsets if their range will not overflow.  */
#define CASE_VECTOR_SHORTEN_MODE(MIN, MAX, BODY) \
  (!TARGET_MIPS16_SHORT_JUMP_TABLES ? ptr_mode \
   : ((MIN) >= -32768 && (MAX) < 32768) ? HImode \
   : SImode)

#define CASE_VECTOR_PC_RELATIVE TARGET_MIPS16_SHORT_JUMP_TABLES

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#ifndef DEFAULT_SIGNED_CHAR
#define DEFAULT_SIGNED_CHAR 1
#endif

/* Although LDC1 and SDC1 provide 64-bit moves on 32-bit targets,
   we generally don't want to use them for copying arbitrary data.
   A single N-word move is usually the same cost as N single-word moves.  */
#define MOVE_MAX UNITS_PER_WORD
/* We don't modify it for MSA as it is only used by the classic reload.  */
#define MAX_MOVE_MAX 8

/* Define this macro as a C expression which is nonzero if
   accessing less than a word of memory (i.e. a `char' or a
   `short') is no faster than accessing a word of memory, i.e., if
   such access require more than one instruction or if there is no
   difference in cost between byte and (aligned) word loads.

   On RISC machines, it tends to generate better code to define
   this as 1, since it avoids making a QI or HI mode register.

   But, generating word accesses for -mips16 is generally bad as shifts
   (often extended) would be needed for byte accesses.  */
#define SLOW_BYTE_ACCESS (!TARGET_MIPS16)

/* Standard MIPS integer shifts truncate the shift amount to the
   width of the shifted operand.  However, Loongson vector shifts
   do not truncate the shift amount at all.  */
#define SHIFT_COUNT_TRUNCATED (!TARGET_LOONGSON_VECTORS)

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) \
  (TARGET_64BIT ? ((INPREC) <= 32 || (OUTPREC) > 32) : 1)


/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */

#ifndef Pmode
#define Pmode (TARGET_64BIT && TARGET_LONG64 ? DImode : SImode)
#endif

/* Give call MEMs SImode since it is the "most permissive" mode
   for both 32-bit and 64-bit targets.  */

#define FUNCTION_MODE SImode


/* We allocate $fcc registers by hand and can't cope with moves of
   CCmode registers to and from pseudos (or memory).  */
#define AVOID_CCMODE_COPIES

/* A C expression for the cost of a branch instruction.  A value of
   1 is the default; other values are interpreted relative to that.  */

#define BRANCH_COST(speed_p, predictable_p) mips_branch_cost
#define LOGICAL_OP_NON_SHORT_CIRCUIT 0

/* The MIPS port has several functions that return an instruction count.
   Multiplying the count by this value gives the number of bytes that
   the instructions occupy.  */
#define BASE_INSN_LENGTH (TARGET_MIPS16 ? 2 : 4)

/* The length of a NOP in bytes.  */
#define NOP_INSN_LENGTH (TARGET_COMPRESSION ? 2 : 4)

/* If defined, modifies the length assigned to instruction INSN as a
   function of the context in which it is used.  LENGTH is an lvalue
   that contains the initially computed length of the insn and should
   be updated with the correct length of the insn.  */
#define ADJUST_INSN_LENGTH(INSN, LENGTH) \
  ((LENGTH) = mips_adjust_insn_length ((INSN), (LENGTH)))

/* Return the asm template for a non-MIPS16 conditional branch instruction.
   OPCODE is the opcode's mnemonic and OPERANDS is the asm template for
   its operands.  */
#define MIPS_BRANCH(OPCODE, OPERANDS) \
  "%*" OPCODE "%?\t" OPERANDS "%/"

#define MIPS_BRANCH_C(OPCODE, OPERANDS) \
  "%*" OPCODE "%:\t" OPERANDS

/* Return an asm string that forces INSN to be treated as an absolute
   J or JAL instruction instead of an assembler macro.  */
#define MIPS_ABSOLUTE_JUMP(INSN) \
  (TARGET_ABICALLS_PIC2						\
   ? ".option\tpic0\n\t" INSN "\n\t.option\tpic2"		\
   : INSN)


/* Control the assembler format that we output.  */

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#ifndef ASM_APP_ON
#define ASM_APP_ON " #APP\n"
#endif

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#ifndef ASM_APP_OFF
#define ASM_APP_OFF " #NO_APP\n"
#endif

#define REGISTER_NAMES							   \
{ "$0",   "$1",   "$2",   "$3",   "$4",   "$5",   "$6",   "$7",		   \
  "$8",   "$9",   "$10",  "$11",  "$12",  "$13",  "$14",  "$15",	   \
  "$16",  "$17",  "$18",  "$19",  "$20",  "$21",  "$22",  "$23",	   \
  "$24",  "$25",  "$26",  "$27",  "$28",  "$sp",  "$fp",  "$31",	   \
  "$f0",  "$f1",  "$f2",  "$f3",  "$f4",  "$f5",  "$f6",  "$f7",	   \
  "$f8",  "$f9",  "$f10", "$f11", "$f12", "$f13", "$f14", "$f15",	   \
  "$f16", "$f17", "$f18", "$f19", "$f20", "$f21", "$f22", "$f23",	   \
  "$f24", "$f25", "$f26", "$f27", "$f28", "$f29", "$f30", "$f31",	   \
  "hi",   "lo",   "",     "$fcc0","$fcc1","$fcc2","$fcc3","$fcc4",	   \
  "$fcc5","$fcc6","$fcc7","", "$cprestore", "$arg", "$frame", "$fakec",	   \
  "$c0r0", "$c0r1", "$c0r2", "$c0r3", "$c0r4", "$c0r5", "$c0r6", "$c0r7",  \
  "$c0r8", "$c0r9", "$c0r10","$c0r11","$c0r12","$c0r13","$c0r14","$c0r15", \
  "$c0r16","$c0r17","$c0r18","$c0r19","$c0r20","$c0r21","$c0r22","$c0r23", \
  "$c0r24","$c0r25","$c0r26","$c0r27","$c0r28","$c0r29","$c0r30","$c0r31", \
  "$c2r0", "$c2r1", "$c2r2", "$c2r3", "$c2r4", "$c2r5", "$c2r6", "$c2r7",  \
  "$c2r8", "$c2r9", "$c2r10","$c2r11","$c2r12","$c2r13","$c2r14","$c2r15", \
  "$c2r16","$c2r17","$c2r18","$c2r19","$c2r20","$c2r21","$c2r22","$c2r23", \
  "$c2r24","$c2r25","$c2r26","$c2r27","$c2r28","$c2r29","$c2r30","$c2r31", \
  "$c3r0", "$c3r1", "$c3r2", "$c3r3", "$c3r4", "$c3r5", "$c3r6", "$c3r7",  \
  "$c3r8", "$c3r9", "$c3r10","$c3r11","$c3r12","$c3r13","$c3r14","$c3r15", \
  "$c3r16","$c3r17","$c3r18","$c3r19","$c3r20","$c3r21","$c3r22","$c3r23", \
  "$c3r24","$c3r25","$c3r26","$c3r27","$c3r28","$c3r29","$c3r30","$c3r31", \
  "$ac1hi","$ac1lo","$ac2hi","$ac2lo","$ac3hi","$ac3lo","$dsp_po","$dsp_sc", \
  "$dsp_ca","$dsp_ou","$dsp_cc","$dsp_ef" }

/* List the "software" names for each register.  Also list the numerical
   names for $fp and $sp.  */

#define ADDITIONAL_REGISTER_NAMES					\
{									\
  { "$29",	29 + GP_REG_FIRST },					\
  { "$30",	30 + GP_REG_FIRST },					\
  { "at",	 1 + GP_REG_FIRST },					\
  { "v0",	 2 + GP_REG_FIRST },					\
  { "v1",	 3 + GP_REG_FIRST },					\
  { "a0",	 4 + GP_REG_FIRST },					\
  { "a1",	 5 + GP_REG_FIRST },					\
  { "a2",	 6 + GP_REG_FIRST },					\
  { "a3",	 7 + GP_REG_FIRST },					\
  { "t0",	 8 + GP_REG_FIRST },					\
  { "t1",	 9 + GP_REG_FIRST },					\
  { "t2",	10 + GP_REG_FIRST },					\
  { "t3",	11 + GP_REG_FIRST },					\
  { "t4",	12 + GP_REG_FIRST },					\
  { "t5",	13 + GP_REG_FIRST },					\
  { "t6",	14 + GP_REG_FIRST },					\
  { "t7",	15 + GP_REG_FIRST },					\
  { "s0",	16 + GP_REG_FIRST },					\
  { "s1",	17 + GP_REG_FIRST },					\
  { "s2",	18 + GP_REG_FIRST },					\
  { "s3",	19 + GP_REG_FIRST },					\
  { "s4",	20 + GP_REG_FIRST },					\
  { "s5",	21 + GP_REG_FIRST },					\
  { "s6",	22 + GP_REG_FIRST },					\
  { "s7",	23 + GP_REG_FIRST },					\
  { "t8",	24 + GP_REG_FIRST },					\
  { "t9",	25 + GP_REG_FIRST },					\
  { "k0",	26 + GP_REG_FIRST },					\
  { "k1",	27 + GP_REG_FIRST },					\
  { "gp",	28 + GP_REG_FIRST },					\
  { "sp",	29 + GP_REG_FIRST },					\
  { "fp",	30 + GP_REG_FIRST },					\
  { "ra",	31 + GP_REG_FIRST },					\
  { "$w0",	 0 + FP_REG_FIRST },					\
  { "$w1",	 1 + FP_REG_FIRST },					\
  { "$w2",	 2 + FP_REG_FIRST },					\
  { "$w3",	 3 + FP_REG_FIRST },					\
  { "$w4",	 4 + FP_REG_FIRST },					\
  { "$w5",	 5 + FP_REG_FIRST },					\
  { "$w6",	 6 + FP_REG_FIRST },					\
  { "$w7",	 7 + FP_REG_FIRST },					\
  { "$w8",	 8 + FP_REG_FIRST },					\
  { "$w9",	 9 + FP_REG_FIRST },					\
  { "$w10",	10 + FP_REG_FIRST },					\
  { "$w11",	11 + FP_REG_FIRST },					\
  { "$w12",	12 + FP_REG_FIRST },					\
  { "$w13",	13 + FP_REG_FIRST },					\
  { "$w14",	14 + FP_REG_FIRST },					\
  { "$w15",	15 + FP_REG_FIRST },					\
  { "$w16",	16 + FP_REG_FIRST },					\
  { "$w17",	17 + FP_REG_FIRST },					\
  { "$w18",	18 + FP_REG_FIRST },					\
  { "$w19",	19 + FP_REG_FIRST },					\
  { "$w20",	20 + FP_REG_FIRST },					\
  { "$w21",	21 + FP_REG_FIRST },					\
  { "$w22",	22 + FP_REG_FIRST },					\
  { "$w23",	23 + FP_REG_FIRST },					\
  { "$w24",	24 + FP_REG_FIRST },					\
  { "$w25",	25 + FP_REG_FIRST },					\
  { "$w26",	26 + FP_REG_FIRST },					\
  { "$w27",	27 + FP_REG_FIRST },					\
  { "$w28",	28 + FP_REG_FIRST },					\
  { "$w29",	29 + FP_REG_FIRST },					\
  { "$w30",	30 + FP_REG_FIRST },					\
  { "$w31",	31 + FP_REG_FIRST }					\
}

#define DBR_OUTPUT_SEQEND(STREAM)					\
do									\
  {									\
    /* Undo the effect of '%*'.  */					\
    mips_pop_asm_switch (&mips_nomacro);				\
    mips_pop_asm_switch (&mips_noreorder);				\
    /* Emit a blank line after the delay slot for emphasis.  */		\
    fputs ("\n", STREAM);						\
  }									\
while (0)

/* The MIPS implementation uses some labels for its own purpose.  The
   following lists what labels are created, and are all formed by the
   pattern $L[a-z].*.  The machine independent portion of GCC creates
   labels matching:  $L[A-Z][0-9]+ and $L[0-9]+.

	LM[0-9]+	Silicon Graphics/ECOFF stabs label before each stmt.
	$Lb[0-9]+	Begin blocks for MIPS debug support
	$Lc[0-9]+	Label for use in s<xx> operation.
	$Le[0-9]+	End blocks for MIPS debug support  */

#undef ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME(STREAM, NAME, DECL) \
  mips_declare_object (STREAM, NAME, "", ":\n")

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.globl\t"

/* This says how to define a global common symbol.  */

#define ASM_OUTPUT_ALIGNED_DECL_COMMON mips_output_aligned_decl_common

/* This says how to define a local common symbol (i.e., not visible to
   linker).  */

#ifndef ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(STREAM, NAME, SIZE, ALIGN) \
  mips_declare_common_object (STREAM, NAME, "\n\t.lcomm\t", SIZE, ALIGN, false)
#endif

/* This says how to output an external.  It would be possible not to
   output anything and let undefined symbol become external. However
   the assembler uses length information on externals to allocate in
   data/sdata bss/sbss, thereby saving exec time.  */

#undef ASM_OUTPUT_EXTERNAL
#define ASM_OUTPUT_EXTERNAL(STREAM,DECL,NAME) \
  mips_output_external(STREAM,DECL,NAME)

/* This is how to declare a function name.  The actual work of
   emitting the label is moved to function_prologue, so that we can
   get the line number correctly emitted before the .ent directive,
   and after any .file directives.  Define as empty so that the function
   is not declared before the .ent directive elsewhere.  */

#undef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(STREAM,NAME,DECL)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)			\
  sprintf ((LABEL), "*%s%s%ld", (LOCAL_LABEL_PREFIX), (PREFIX), (long)(NUM))

/* Print debug labels as "foo = ." rather than "foo:" because they should
   represent a byte pointer rather than an ISA-encoded address.  This is
   particularly important for code like:

	$LFBxxx = .
		.cfi_startproc
		...
		.section .gcc_except_table,...
		...
		.uleb128 foo-$LFBxxx

   The .uleb128 requies $LFBxxx to match the FDE start address, which is
   likewise a byte pointer rather than an ISA-encoded address.

   At the time of writing, this hook is not used for the function end
   label:

   	$LFExxx:
		.end foo

   But this doesn't matter, because GAS doesn't treat a pre-.end label
   as a MIPS16 one anyway.  */

#define ASM_OUTPUT_DEBUG_LABEL(FILE, PREFIX, NUM)			\
  fprintf (FILE, "%s%s%d = .\n", LOCAL_LABEL_PREFIX, PREFIX, NUM)

/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE)				\
  fprintf (STREAM, "\t%s\t%sL%d\n",					\
	   ptr_mode == DImode ? ".dword" : ".word",			\
	   LOCAL_LABEL_PREFIX,						\
	   VALUE)

/* This is how to output an element of a case-vector.  We can make the
   entries PC-relative in MIPS16 code and GP-relative when .gp(d)word
   is supported.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL)		\
do {									\
  if (TARGET_MIPS16_SHORT_JUMP_TABLES)					\
    {									\
      if (GET_MODE (BODY) == HImode)					\
	fprintf (STREAM, "\t.half\t%sL%d-%sL%d\n",			\
		 LOCAL_LABEL_PREFIX, VALUE, LOCAL_LABEL_PREFIX, REL);	\
      else								\
	fprintf (STREAM, "\t.word\t%sL%d-%sL%d\n",			\
		 LOCAL_LABEL_PREFIX, VALUE, LOCAL_LABEL_PREFIX, REL);	\
    }									\
  else if (TARGET_GPWORD)						\
    fprintf (STREAM, "\t%s\t%sL%d\n",					\
	     ptr_mode == DImode ? ".gpdword" : ".gpword",		\
	     LOCAL_LABEL_PREFIX, VALUE);				\
  else if (TARGET_RTP_PIC)						\
    {									\
      /* Make the entry relative to the start of the function.  */	\
      rtx fnsym = XEXP (DECL_RTL (current_function_decl), 0);		\
      fprintf (STREAM, "\t%s\t%sL%d-",					\
	       Pmode == DImode ? ".dword" : ".word",			\
	       LOCAL_LABEL_PREFIX, VALUE);				\
      assemble_name (STREAM, XSTR (fnsym, 0));				\
      fprintf (STREAM, "\n");						\
    }									\
  else									\
    fprintf (STREAM, "\t%s\t%sL%d\n",					\
	     ptr_mode == DImode ? ".dword" : ".word",			\
	     LOCAL_LABEL_PREFIX, VALUE);				\
} while (0)

/* Mark inline jump tables as data for the purpose of disassembly.  For
   simplicity embed the jump table's label number in the local symbol
   produced so that multiple jump tables within a single function end
   up marked with unique symbols.  Retain the alignment setting from
   `elfos.h' as we are replacing the definition from there.  */

#undef ASM_OUTPUT_BEFORE_CASE_LABEL
#define ASM_OUTPUT_BEFORE_CASE_LABEL(STREAM, PREFIX, NUM, TABLE)	\
  do									\
    {									\
      ASM_OUTPUT_ALIGN ((STREAM), 2);					\
      if (JUMP_TABLES_IN_TEXT_SECTION)					\
	mips_set_text_contents_type (STREAM, "__jump_", NUM, FALSE);	\
    }									\
  while (0);

/* Reset text marking to code after an inline jump table.  Like with
   the beginning of a jump table use the label number to keep symbols
   unique.  */

#define ASM_OUTPUT_CASE_END(STREAM, NUM, TABLE)				\
  do									\
    if (JUMP_TABLES_IN_TEXT_SECTION)					\
      mips_set_text_contents_type (STREAM, "__jend_", NUM, TRUE);	\
  while (0);

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(STREAM,LOG)					\
  fprintf (STREAM, "\t.align\t%d\n", (LOG))

/* This is how to output an assembler line to advance the location
   counter by SIZE bytes.  */

#undef ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(STREAM,SIZE)					\
  fprintf (STREAM, "\t.space\t" HOST_WIDE_INT_PRINT_UNSIGNED"\n", (SIZE))

/* This is how to output a string.  */
#undef ASM_OUTPUT_ASCII
#define ASM_OUTPUT_ASCII mips_output_ascii


/* Default to -G 8 */
#ifndef MIPS_DEFAULT_GVALUE
#define MIPS_DEFAULT_GVALUE 8
#endif

/* Define the strings to put out for each section in the object file.  */
#define TEXT_SECTION_ASM_OP	"\t.text"	/* instructions */
#define DATA_SECTION_ASM_OP	"\t.data"	/* large data */

#undef READONLY_DATA_SECTION_ASM_OP
#define READONLY_DATA_SECTION_ASM_OP	"\t.rdata"	/* read-only data */

#define ASM_OUTPUT_REG_PUSH(STREAM,REGNO)				\
do									\
  {									\
    fprintf (STREAM, "\t%s\t%s,%s,-8\n\t%s\t%s,0(%s)\n",		\
	     TARGET_64BIT ? "daddiu" : "addiu",				\
	     reg_names[STACK_POINTER_REGNUM],				\
	     reg_names[STACK_POINTER_REGNUM],				\
	     TARGET_64BIT ? "sd" : "sw",				\
	     reg_names[REGNO],						\
	     reg_names[STACK_POINTER_REGNUM]);				\
  }									\
while (0)

#define ASM_OUTPUT_REG_POP(STREAM,REGNO)				\
do									\
  {									\
    mips_push_asm_switch (&mips_noreorder);				\
    fprintf (STREAM, "\t%s\t%s,0(%s)\n\t%s\t%s,%s,8\n",			\
	     TARGET_64BIT ? "ld" : "lw",				\
	     reg_names[REGNO],						\
	     reg_names[STACK_POINTER_REGNUM],				\
	     TARGET_64BIT ? "daddu" : "addu",				\
	     reg_names[STACK_POINTER_REGNUM],				\
	     reg_names[STACK_POINTER_REGNUM]);				\
    mips_pop_asm_switch (&mips_noreorder);				\
  }									\
while (0)

/* How to start an assembler comment.
   The leading space is important (the mips native assembler requires it).  */
#ifndef ASM_COMMENT_START
#define ASM_COMMENT_START " #"
#endif

#undef SIZE_TYPE
#define SIZE_TYPE (POINTER_SIZE == 64 ? "long unsigned int" : "unsigned int")

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE (POINTER_SIZE == 64 ? "long int" : "int")

/* The minimum alignment of any expanded block move.  */
#define MIPS_MIN_MOVE_MEM_ALIGN 16

/* The maximum number of bytes that can be copied by one iteration of
   a movmemsi loop; see mips_block_move_loop.  */
#define MIPS_MAX_MOVE_BYTES_PER_LOOP_ITER \
  (UNITS_PER_WORD * 4)

/* The maximum number of bytes that can be copied by a straight-line
   implementation of movmemsi; see mips_block_move_straight.  We want
   to make sure that any loop-based implementation will iterate at
   least twice.  */
#define MIPS_MAX_MOVE_BYTES_STRAIGHT \
  (MIPS_MAX_MOVE_BYTES_PER_LOOP_ITER * 2)

/* The base cost of a memcpy call, for MOVE_RATIO and friends.  These
   values were determined experimentally by benchmarking with CSiBE.
   In theory, the call overhead is higher for TARGET_ABICALLS (especially
   for o32 where we have to restore $gp afterwards as well as make an
   indirect call), but in practice, bumping this up higher for
   TARGET_ABICALLS doesn't make much difference to code size.  */

#define MIPS_CALL_RATIO 8

/* Any loop-based implementation of movmemsi will have at least
   MIPS_MAX_MOVE_BYTES_STRAIGHT / UNITS_PER_WORD memory-to-memory
   moves, so allow individual copies of fewer elements.

   When movmemsi is not available, use a value approximating
   the length of a memcpy call sequence, so that move_by_pieces
   will generate inline code if it is shorter than a function call.
   Since move_by_pieces_ninsns counts memory-to-memory moves, but
   we'll have to generate a load/store pair for each, halve the
   value of MIPS_CALL_RATIO to take that into account.  */

#define MOVE_RATIO(speed)				\
  (HAVE_movmemsi					\
   ? MIPS_MAX_MOVE_BYTES_STRAIGHT / MOVE_MAX		\
   : MIPS_CALL_RATIO / 2)

/* For CLEAR_RATIO, when optimizing for size, give a better estimate
   of the length of a memset call, but use the default otherwise.  */

#define CLEAR_RATIO(speed)\
  ((speed) ? 15 : MIPS_CALL_RATIO)

/* This is similar to CLEAR_RATIO, but for a non-zero constant, so when
   optimizing for size adjust the ratio to account for the overhead of
   loading the constant and replicating it across the word.  */

#define SET_RATIO(speed) \
  ((speed) ? 15 : MIPS_CALL_RATIO - 2)

/* Since the bits of the _init and _fini function is spread across
   many object files, each potentially with its own GP, we must assume
   we need to load our GP.  We don't preserve $gp or $ra, since each
   init/fini chunk is supposed to initialize $gp, and crti/crtn
   already take care of preserving $ra and, when appropriate, $gp.  */
#if (defined _ABIO32 && _MIPS_SIM == _ABIO32)
#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC)	\
   asm (SECTION_OP "\n\
	.set push\n\
	.set nomips16\n\
	.set noreorder\n\
	bal 1f\n\
	nop\n\
1:	.cpload $31\n\
	.set reorder\n\
	la $25, " USER_LABEL_PREFIX #FUNC "\n\
	jalr $25\n\
	.set pop\n\
	" TEXT_SECTION_ASM_OP);
#elif (defined _ABIN32 && _MIPS_SIM == _ABIN32)
#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC)	\
   asm (SECTION_OP "\n\
	.set push\n\
	.set nomips16\n\
	.set noreorder\n\
	bal 1f\n\
	nop\n\
1:	.set reorder\n\
	.cpsetup $31, $2, 1b\n\
	la $25, " USER_LABEL_PREFIX #FUNC "\n\
	jalr $25\n\
	.set pop\n\
	" TEXT_SECTION_ASM_OP);
#elif (defined _ABI64 && _MIPS_SIM == _ABI64)
#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC)	\
   asm (SECTION_OP "\n\
	.set push\n\
	.set nomips16\n\
	.set noreorder\n\
	bal 1f\n\
	nop\n\
1:	.set reorder\n\
	.cpsetup $31, $2, 1b\n\
	dla $25, " USER_LABEL_PREFIX #FUNC "\n\
	jalr $25\n\
	.set pop\n\
	" TEXT_SECTION_ASM_OP);
#endif

#ifndef HAVE_AS_TLS
#define HAVE_AS_TLS 0
#endif

#ifndef HAVE_AS_NAN
#define HAVE_AS_NAN 0
#endif

#ifndef USED_FOR_TARGET
/* Information about ".set noFOO; ...; .set FOO" blocks.  */
struct mips_asm_switch {
  /* The FOO in the description above.  */
  const char *name;

  /* The current block nesting level, or 0 if we aren't in a block.  */
  int nesting_level;
};

extern const enum reg_class mips_regno_to_class[];
extern bool mips_hard_regno_mode_ok[][FIRST_PSEUDO_REGISTER];
extern const char *current_function_file; /* filename current function is in */
extern int num_source_filenames;	/* current .file # */
extern struct mips_asm_switch mips_noreorder;
extern struct mips_asm_switch mips_nomacro;
extern struct mips_asm_switch mips_noat;
extern int mips_dbx_regno[];
extern int mips_dwarf_regno[];
extern bool mips_split_p[];
extern bool mips_split_hi_p[];
extern bool mips_use_pcrel_pool_p[];
extern const char *mips_lo_relocs[];
extern const char *mips_hi_relocs[];
extern enum processor mips_arch;        /* which cpu to codegen for */
extern enum processor mips_tune;        /* which cpu to schedule for */
extern int mips_isa;			/* architectural level */
extern int mips_isa_rev;
extern const struct mips_cpu_info *mips_arch_info;
extern const struct mips_cpu_info *mips_tune_info;
extern unsigned int mips_base_compression_flags;
extern GTY(()) struct target_globals *mips16_globals;
extern GTY(()) struct target_globals *micromips_globals;

/* Information about a function's frame layout.  */
struct GTY(())  mips_frame_info {
  /* The size of the frame in bytes.  */
  HOST_WIDE_INT total_size;

  /* The number of bytes allocated to variables.  */
  HOST_WIDE_INT var_size;

  /* The number of bytes allocated to outgoing function arguments.  */
  HOST_WIDE_INT args_size;

  /* The number of bytes allocated to the .cprestore slot, or 0 if there
     is no such slot.  */
  HOST_WIDE_INT cprestore_size;

  /* Bit X is set if the function saves or restores GPR X.  */
  unsigned int mask;

  /* Likewise FPR X.  */
  unsigned int fmask;

  /* Likewise doubleword accumulator X ($acX).  */
  unsigned int acc_mask;

  /* The number of GPRs, FPRs, doubleword accumulators and COP0
     registers saved.  */
  unsigned int num_gp;
  unsigned int num_fp;
  unsigned int num_acc;
  unsigned int num_cop0_regs;

  /* The offset of the topmost GPR, FPR, accumulator and COP0-register
     save slots from the top of the frame, or zero if no such slots are
     needed.  */
  HOST_WIDE_INT gp_save_offset;
  HOST_WIDE_INT fp_save_offset;
  HOST_WIDE_INT acc_save_offset;
  HOST_WIDE_INT cop0_save_offset;

  /* Likewise, but giving offsets from the bottom of the frame.  */
  HOST_WIDE_INT gp_sp_offset;
  HOST_WIDE_INT fp_sp_offset;
  HOST_WIDE_INT acc_sp_offset;
  HOST_WIDE_INT cop0_sp_offset;

  /* Similar, but the value passed to _mcount.  */
  HOST_WIDE_INT ra_fp_offset;

  /* The offset of arg_pointer_rtx from the bottom of the frame.  */
  HOST_WIDE_INT arg_pointer_offset;

  /* The offset of hard_frame_pointer_rtx from the bottom of the frame.  */
  HOST_WIDE_INT hard_frame_pointer_offset;
};

/* Enumeration for masked vectored (VI) and non-masked (EIC) interrupts.  */
enum mips_int_mask
{
  INT_MASK_EIC = -1,
  INT_MASK_SW0 = 0,
  INT_MASK_SW1 = 1,
  INT_MASK_HW0 = 2,
  INT_MASK_HW1 = 3,
  INT_MASK_HW2 = 4,
  INT_MASK_HW3 = 5,
  INT_MASK_HW4 = 6,
  INT_MASK_HW5 = 7
};

/* Enumeration to mark the existence of the shadow register set.
   SHADOW_SET_INTSTACK indicates a shadow register set with a valid stack
   pointer.  */
enum mips_shadow_set
{
  SHADOW_SET_NO,
  SHADOW_SET_YES,
  SHADOW_SET_INTSTACK
};

struct GTY(())  machine_function {
  /* The next floating-point condition-code register to allocate
     for ISA_HAS_8CC targets, relative to ST_REG_FIRST.  */
  unsigned int next_fcc;

  /* The register returned by mips16_gp_pseudo_reg; see there for details.  */
  rtx mips16_gp_pseudo_rtx;

  /* The number of extra stack bytes taken up by register varargs.
     This area is allocated by the callee at the very top of the frame.  */
  int varargs_size;

  /* The current frame information, calculated by mips_compute_frame_info.  */
  struct mips_frame_info frame;

  /* The register to use as the function's global pointer, or INVALID_REGNUM
     if the function doesn't need one.  */
  unsigned int global_pointer;

  /* How many instructions it takes to load a label into $AT, or 0 if
     this property hasn't yet been calculated.  */
  unsigned int load_label_num_insns;

  /* True if mips_adjust_insn_length should ignore an instruction's
     hazard attribute.  */
  bool ignore_hazard_length_p;

  /* True if the whole function is suitable for .set noreorder and
     .set nomacro.  */
  bool all_noreorder_p;

  /* True if the function has "inflexible" and "flexible" references
     to the global pointer.  See mips_cfun_has_inflexible_gp_ref_p
     and mips_cfun_has_flexible_gp_ref_p for details.  */
  bool has_inflexible_gp_insn_p;
  bool has_flexible_gp_insn_p;

  /* True if the function's prologue must load the global pointer
     value into pic_offset_table_rtx and store the same value in
     the function's cprestore slot (if any).  Even if this value
     is currently false, we may decide to set it to true later;
     see mips_must_initialize_gp_p () for details.  */
  bool must_initialize_gp_p;

  /* True if the current function must restore $gp after any potential
     clobber.  This value is only meaningful during the first post-epilogue
     split_insns pass; see mips_must_initialize_gp_p () for details.  */
  bool must_restore_gp_when_clobbered_p;

  /* True if this is an interrupt handler.  */
  bool interrupt_handler_p;

  /* Records the way in which interrupts should be masked.  Only used if
     interrupts are not kept masked.  */
  enum mips_int_mask int_mask;

  /* Records if this is an interrupt handler that uses shadow registers.  */
  enum mips_shadow_set use_shadow_register_set;

  /* True if this is an interrupt handler that should keep interrupts
     masked.  */
  bool keep_interrupts_masked_p;

  /* True if this is an interrupt handler that should use DERET
     instead of ERET.  */
  bool use_debug_exception_return_p;

  /* True if at least one of the formal parameters to a function must be
     written to the frame header (probably so its address can be taken).  */
  bool does_not_use_frame_header;

  /* True if none of the functions that are called by this function need
     stack space allocated for their arguments.  */
  bool optimize_call_stack;

  /* True if one of the functions calling this function may not allocate
     a frame header.  */
  bool callers_may_not_allocate_frame;

  /* True if GCC stored callee saved registers in the frame header.  */
  bool use_frame_header_for_callee_saved_regs;
};
#endif

/* Enable querying of DFA units.  */
#define CPU_UNITS_QUERY 1

#define FINAL_PRESCAN_INSN(INSN, OPVEC, NOPERANDS)	\
  mips_final_prescan_insn (INSN, OPVEC, NOPERANDS)

/* As on most targets, we want the .eh_frame section to be read-only where
   possible.  And as on most targets, this means two things:

     (a) Non-locally-binding pointers must have an indirect encoding,
	 so that the addresses in the .eh_frame section itself become
	 locally-binding.

     (b) A shared library's .eh_frame section must encode locally-binding
	 pointers in a relative (relocation-free) form.

   However, MIPS has traditionally not allowed directives like:

	.long	x-.

   in cases where "x" is in a different section, or is not defined in the
   same assembly file.  We are therefore unable to emit the PC-relative
   form required by (b) at assembly time.

   Fortunately, the linker is able to convert absolute addresses into
   PC-relative addresses on our behalf.  Unfortunately, only certain
   versions of the linker know how to do this for indirect pointers,
   and for personality data.  We must fall back on using writable
   .eh_frame sections for shared libraries if the linker does not
   support this feature.  */
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE,GLOBAL) \
  (((GLOBAL) ? DW_EH_PE_indirect : 0) | DW_EH_PE_absptr)

/* For switching between MIPS16 and non-MIPS16 modes.  */
#define SWITCHABLE_TARGET 1

/* Several named MIPS patterns depend on Pmode.  These patterns have the
   form <NAME>_si for Pmode == SImode and <NAME>_di for Pmode == DImode.
   Add the appropriate suffix to generator function NAME and invoke it
   with arguments ARGS.  */
#define PMODE_INSN(NAME, ARGS) \
  (Pmode == SImode ? NAME ## _si ARGS : NAME ## _di ARGS)

/* If we are *not* using multilibs and the default ABI is not ABI_32 we
   need to change these from /lib and /usr/lib.  */
#if MIPS_ABI_DEFAULT == ABI_N32
#define STANDARD_STARTFILE_PREFIX_1 "/lib32/"
#define STANDARD_STARTFILE_PREFIX_2 "/usr/lib32/"
#elif MIPS_ABI_DEFAULT == ABI_64
#define STANDARD_STARTFILE_PREFIX_1 "/lib64/"
#define STANDARD_STARTFILE_PREFIX_2 "/usr/lib64/"
#endif

/* Load store bonding is not supported by micromips and fix_24k.  The
   performance can be degraded for those targets.  Hence, do not bond for
   micromips or fix_24k.  */
#define ENABLE_LD_ST_PAIRS \
  (TARGET_LOAD_STORE_PAIRS && (TUNE_P5600 || TUNE_I6400) \
   && !TARGET_MICROMIPS && !TARGET_FIX_24K)
