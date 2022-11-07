/* Definitions of target machine for GNU compiler, Synopsys DesignWare ARC cpu.
   Copyright (C) 1994-2022 Free Software Foundation, Inc.

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

#ifndef GCC_ARC_H
#define GCC_ARC_H

#include <stdbool.h>

/* Things to do:

   - incscc, decscc?

*/

#define SYMBOL_FLAG_SHORT_CALL	(SYMBOL_FLAG_MACH_DEP << 0)
#define SYMBOL_FLAG_MEDIUM_CALL	(SYMBOL_FLAG_MACH_DEP << 1)
#define SYMBOL_FLAG_LONG_CALL	(SYMBOL_FLAG_MACH_DEP << 2)
#define SYMBOL_FLAG_CMEM	(SYMBOL_FLAG_MACH_DEP << 3)

#ifndef TARGET_CPU_DEFAULT
#define TARGET_CPU_DEFAULT	PROCESSOR_hs38_linux
#endif

/* Check if this symbol has a long_call attribute in its declaration */
#define SYMBOL_REF_LONG_CALL_P(X)	\
	((SYMBOL_REF_FLAGS (X) & SYMBOL_FLAG_LONG_CALL) != 0)

/* Check if this symbol has a medium_call attribute in its declaration */
#define SYMBOL_REF_MEDIUM_CALL_P(X)	\
	((SYMBOL_REF_FLAGS (X) & SYMBOL_FLAG_MEDIUM_CALL) != 0)

/* Check if this symbol has a short_call attribute in its declaration */
#define SYMBOL_REF_SHORT_CALL_P(X)	\
	((SYMBOL_REF_FLAGS (X) & SYMBOL_FLAG_SHORT_CALL) != 0)

/* Names to predefine in the preprocessor for this target machine.  */
#define TARGET_CPU_CPP_BUILTINS() arc_cpu_cpp_builtins (pfile)

/* Macros enabled by specific command line option.  FIXME: to be
   deprecatd.  */
#define CPP_SPEC "\
%{msimd:-D__Xsimd} %{mno-mpy:-D__Xno_mpy} %{mswap:-D__Xswap} \
%{mmin-max:-D__Xmin_max} %{mEA:-D__Xea} \
%{mspfp*:-D__Xspfp} %{mdpfp*:-D__Xdpfp} \
%{mmac-d16:-D__Xxmac_d16} %{mmac-24:-D__Xxmac_24} \
%{mdsp-packa:-D__Xdsp_packa} %{mcrc:-D__Xcrc} %{mdvbf:-D__Xdvbf} \
%{mtelephony:-D__Xtelephony} %{mxy:-D__Xxy} %{mmul64: -D__Xmult32} \
%{mlock:-D__Xlock} %{mswape:-D__Xswape} %{mrtsc:-D__Xrtsc} \
%(subtarget_cpp_spec)"

#undef CC1_SPEC
#define CC1_SPEC "%{EB:%{EL:%emay not use both -EB and -EL}}	\
%{EB:-mbig-endian} %{EL:-mlittle-endian}			\
%{G*}								\
"
extern const char *arc_cpu_to_as (int argc, const char **argv);

#define EXTRA_SPEC_FUNCTIONS			\
  { "cpu_to_as", arc_cpu_to_as },

/* This macro defines names of additional specifications to put in the specs
   that can be used in various specifications like CC1_SPEC.  Its definition
   is an initializer with a subgrouping for each command option.

   Each subgrouping contains a string constant, that defines the
   specification name, and a string constant that used by the GCC driver
   program.

   Do not define this macro if it does not need to do anything.  */
#define EXTRA_SPECS				      \
  { "subtarget_cpp_spec",	SUBTARGET_CPP_SPEC }, \
  SUBTARGET_EXTRA_SPECS

#ifndef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS
#endif

#ifndef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC ""
#endif

#undef ASM_SPEC
#define ASM_SPEC  "%{mbig-endian|EB:-EB} %{EL} "			\
  "%:cpu_to_as(%{mcpu=*:%*}) %{mspfp*} %{mdpfp*} "                      \
  "%{mfpu=fpuda*:-mfpuda} %{mcode-density}"

/* Support for a compile-time default CPU and FPU.  The rules are:
   --with-cpu is ignored if -mcpu, mARC*, marc*, mA7, mA6 are specified.
   --with-fpu is ignored if -mfpu is specified.  */
#define OPTION_DEFAULT_SPECS						\
  {"fpu", "%{!mfpu=*:-mfpu=%(VALUE)}"},					\
  {"cpu", "%{!mcpu=*:%{!mARC*:%{!marc*:%{!mA7:%{!mA6:-mcpu=%(VALUE)}}}}}" }

#ifndef DRIVER_ENDIAN_SELF_SPECS
#define DRIVER_ENDIAN_SELF_SPECS ""
#endif

#define DRIVER_SELF_SPECS DRIVER_ENDIAN_SELF_SPECS		   \
  "%{mARC600|mA6: -mcpu=arc600 %<mARC600 %<mA6 %<mARC600}"	   \
  "%{mARC601: -mcpu=arc601 %<mARC601}"				   \
  "%{mARC700|mA7: -mcpu=arc700 %<mARC700 %<mA7}"		   \
  "%{mEA: -mea %<mEA}"

/* Run-time compilation parameters selecting different hardware subsets.  */

#define TARGET_SPFP (TARGET_SPFP_FAST_SET || TARGET_SPFP_COMPACT_SET)
#define TARGET_DPFP (TARGET_DPFP_FAST_SET || TARGET_DPFP_COMPACT_SET	\
		     || TARGET_FP_DP_AX)

#define SUBTARGET_SWITCHES

/* Instruction set characteristics.
   These are internal macros, set by the appropriate -m option.  */

/* Non-zero means the cpu supports norm instruction.  This flag is set by
   default for A7, and only for pre A7 cores when -mnorm is given.  */
#define TARGET_NORM (TARGET_ARC700 || TARGET_NORM_SET || TARGET_HS)
/* Indicate if an optimized floating point emulation library is available.  */
#define TARGET_OPTFPE (TARGET_ARC700 || TARGET_FPX_QUARK)

/* Non-zero means the cpu supports swap instruction.  This flag is set by
   default for A7, and only for pre A7 cores when -mswap is given.  */
#define TARGET_SWAP (TARGET_ARC700 || TARGET_SWAP_SET)

/* Provide some macros for size / scheduling features of the ARC700, so
   that we can pick & choose features if we get a new cpu family member.  */

/* Should we try to unalign likely taken branches without a delay slot.  */
#define TARGET_UNALIGN_BRANCH (TARGET_ARC700 && !optimize_size)

/* Should we add padding before a return insn to avoid mispredict?  */
#define TARGET_PAD_RETURN (TARGET_ARC700 && !optimize_size)

/* For an anulled-true delay slot insn for a delayed branch, should we only
   use conditional execution?  */
#define TARGET_AT_DBR_CONDEXEC  (!TARGET_ARC700 && !TARGET_V2)

#define TARGET_ARC600 ((arc_selected_cpu->arch_info->arch_id	\
			== BASE_ARCH_6xx)			\
		       && (TARGET_BARREL_SHIFTER))
#define TARGET_ARC601 ((arc_selected_cpu->arch_info->arch_id	\
			== BASE_ARCH_6xx)			\
		       && (!TARGET_BARREL_SHIFTER))
#define TARGET_ARC700 (arc_selected_cpu->arch_info->arch_id	\
		       == BASE_ARCH_700)
/* An NPS400 is a specialisation of ARC700, so it is correct for NPS400
   TARGET_ARC700 is true, and TARGET_NPS400 is true.  */
#define TARGET_NPS400 ((arc_selected_cpu->arch_info->arch_id	\
			== BASE_ARCH_700)			\
		       && (arc_selected_cpu->processor		\
			   == PROCESSOR_nps400))
#define TARGET_EM (arc_selected_cpu->arch_info->arch_id == BASE_ARCH_em)
#define TARGET_HS (arc_selected_cpu->arch_info->arch_id == BASE_ARCH_hs)
#define TARGET_V2 (TARGET_EM || TARGET_HS)

#ifndef UNALIGNED_ACCESS_DEFAULT
#define UNALIGNED_ACCESS_DEFAULT 0
#endif

#ifndef TARGET_NPS_BITOPS_DEFAULT
#define TARGET_NPS_BITOPS_DEFAULT 0
#endif

#ifndef TARGET_NPS_CMEM_DEFAULT
#define TARGET_NPS_CMEM_DEFAULT 0
#endif

/* Enable the RRQ instruction alternatives.  */

#define TARGET_RRQ_CLASS TARGET_NPS_BITOPS

/* Target machine storage layout.  */

/* We want zero_extract to mean the same
   no matter what the byte endianness is.  */
#define BITS_BIG_ENDIAN 0

/* Define this if most significant byte of a word is the lowest numbered.  */
#define BYTES_BIG_ENDIAN (TARGET_BIG_ENDIAN)

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
#define WORDS_BIG_ENDIAN (TARGET_BIG_ENDIAN)

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD 32

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 4

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases,
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.  */
#define PROMOTE_MODE(MODE,UNSIGNEDP,TYPE) \
if (GET_MODE_CLASS (MODE) == MODE_INT		\
    && GET_MODE_SIZE (MODE) < UNITS_PER_WORD)	\
{						\
  (MODE) = SImode;				\
}

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE 32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 32

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
/* TOCHECK: Changed from 64 to 32 */
#define STACK_BOUNDARY 32

/* ALIGN FRAMES on word boundaries.  */
#define ARC_STACK_ALIGN(LOC) \
  (((LOC) + STACK_BOUNDARY / BITS_PER_UNIT - 1) & -STACK_BOUNDARY/BITS_PER_UNIT)

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 32

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* A bitfield declared as `int' forces `int' alignment for the struct.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* An expression for the alignment of a structure field FIELD if the
   alignment computed in the usual way (including applying of
   `BIGGEST_ALIGNMENT' and `BIGGEST_FIELD_ALIGNMENT' to the
   alignment) is COMPUTED.  It overrides alignment only if the field
   alignment has not been set by the `__attribute__ ((aligned (N)))'
   construct.
*/

#define ADJUST_FIELD_ALIGN(FIELD, TYPE, COMPUTED) \
(TYPE_MODE (strip_array_types (TYPE)) == DFmode \
 ? MIN ((COMPUTED), 32) : (COMPUTED))



/* No data type wants to be aligned rounder than this.  */
/* This is bigger than currently necessary for the ARC.  If 8 byte floats are
   ever added it's not clear whether they'll need such alignment or not.  For
   now we assume they will.  We can always relax it if necessary but the
   reverse isn't true.  */
/* TOCHECK: Changed from 64 to 32 */
#define BIGGEST_ALIGNMENT 32

/* The best alignment to use in cases where we have a choice.  */
#define FASTEST_ALIGNMENT 32

/* Make arrays of chars word-aligned for the same reasons.  */
#define LOCAL_ALIGNMENT(TYPE, ALIGN)             \
  (TREE_CODE (TYPE) == ARRAY_TYPE               \
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode    \
   && (ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN))

#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && arc_size_opt_level < 3			\
   && (ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN))

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
/* On the ARC the lower address bits are masked to 0 as necessary.  The chip
   won't croak when given an unaligned address, but the insn will still fail
   to produce the correct result.  */
#define STRICT_ALIGNMENT (!unaligned_access)

/* Layout of source language data types.  */

#define SHORT_TYPE_SIZE		16
#define INT_TYPE_SIZE		32
#define LONG_TYPE_SIZE		32
#define LONG_LONG_TYPE_SIZE	64
#define FLOAT_TYPE_SIZE		32
#define DOUBLE_TYPE_SIZE	64
#define LONG_DOUBLE_TYPE_SIZE	64

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 0

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   Registers 61, 62, and 63 are not really registers and we needn't treat
   them as such.  We still need a register for the condition code and
   argument pointer.  */

/* r63 is pc, r64-r127 = simd vregs, r128-r143 = simd dma config regs
   r144, r145 = ARG_POINTER, FRAME_POINTER
   and therefore the pseudo registers start from r146. */
#define FIRST_PSEUDO_REGISTER 146

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.

   0-28  - general purpose registers
   29    - ilink1 (interrupt link register)
   30    - ilink2 (interrupt link register)
   31    - blink (branch link register)
   32-59 - reserved for extensions
   60    - LP_COUNT
   61    - condition code
   62    - argument pointer
   63    - program counter

   FWIW, this is how the 61-63 encodings are used by the hardware:
   61    - reserved
   62    - long immediate data indicator
   63    - PCL (program counter aligned to 32 bit, read-only)

   The general purpose registers are further broken down into:

   0-7   - arguments/results
   8-12  - call used (r11 - static chain pointer)
   13-25 - call saved
   26    - global pointer
   27    - frame pointer
   28    - stack pointer
   29    - ilink1
   30    - ilink2
   31    - return address register

   By default, the extension registers are not available.  */
/* Present implementations only have VR0-VR23 only.  */
#define FIXED_REGISTERS \
{ 0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 1, 0, 1, 1, 1, 1,	\
				\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  0, 0, 0, 0, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,       \
				\
  0, 0, 0, 0, 0, 0, 0, 0,       \
  0, 0, 0, 0, 0, 0, 0, 0,       \
  0, 0, 0, 0, 0, 0, 0, 0,       \
  1, 1, 1, 1, 1, 1, 1, 1,       \
				\
  1, 1, 1, 1, 1, 1, 1, 1,       \
  1, 1, 1, 1, 1, 1, 1, 1,       \
  1, 1, 1, 1, 1, 1, 1, 1,       \
  1, 1, 1, 1, 1, 1, 1, 1,       \
				\
  0, 0, 0, 0, 0, 0, 0, 0,       \
  0, 0, 0, 0, 0, 0, 0, 0,	\
  1, 1}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */
#define CALL_USED_REGISTERS     \
{                               \
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 1, 0, 1, 1, 1, 1,	\
				\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,       \
				\
  0, 0, 0, 0, 0, 0, 0, 0,       \
  0, 0, 0, 0, 0, 0, 0, 0,       \
  0, 0, 0, 0, 0, 0, 0, 0,       \
  1, 1, 1, 1, 1, 1, 1, 1,       \
				\
  1, 1, 1, 1, 1, 1, 1, 1,       \
  1, 1, 1, 1, 1, 1, 1, 1,       \
  1, 1, 1, 1, 1, 1, 1, 1,       \
  1, 1, 1, 1, 1, 1, 1, 1,       \
				\
  0, 0, 0, 0, 0, 0, 0, 0,       \
  0, 0, 0, 0, 0, 0, 0, 0,	\
  1, 1}

/* If defined, an initializer for a vector of integers, containing the
   numbers of hard registers in the order in which GCC should
   prefer to use them (from most preferred to least).  */
#define REG_ALLOC_ORDER							\
{									\
  /* General registers.  */						\
  2, 3, 12, 13, 14, 15, 1, 0, 4, 5, 6, 7, 8, 9, 10, 11,			\
  16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 30,			\
  /* Extension core registers.  */					\
  32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,	\
  48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,			\
  /* VR regs.  */							\
  64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,	\
  80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,	\
  96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109,	\
  110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, \
  124, 125, 126, 127,							\
  /* DMA registers.  */							\
  128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, \
  142, 143,								\
  /* Register not used for general use.  */				\
  62, FRAME_POINTER_REGNUM, ARG_POINTER_REGNUM,				\
  SP_REG, ILINK1_REG, RETURN_ADDR_REGNUM, LP_COUNT, CC_REG, PCL_REG	\
}

/* Use different register alloc ordering for Thumb.  */
#define ADJUST_REG_ALLOC_ORDER arc_adjust_reg_alloc_order ()

/* Tell IRA to use the order we define rather than messing it up with its
   own cost calculations.  */
#define HONOR_REG_ALLOC_ORDER 1

/* Internal macros to classify a register number as to whether it's a
   general purpose register for compact insns (r0-r3,r12-r15), or
   stack pointer (r28).  */

#define COMPACT_GP_REG_P(REGNO) \
   (((signed)(REGNO) >= 0 && (REGNO) <= 3) || ((REGNO) >= 12 && (REGNO) <= 15))
#define SP_REG_P(REGNO)  ((REGNO) == 28)



/* Register classes and constants.  */

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
   class that represents their union.

   It is important that any condition codes have class NO_REGS.
   See `register_operand'.  */

enum reg_class
{
   NO_REGS,
   R0_REGS,			/* 'x' */
   R0R1_CD_REGS,		/* 'Rsd' */
   R0R3_CD_REGS,		/* 'Rcd' */
   ARCOMPACT16_REGS,		/* 'q' */
   SIBCALL_REGS,		/* "Rsc" */
   AC16_H_REGS,			/* 'h' */
   DOUBLE_REGS,			/* 'D' */
   GENERAL_REGS,		/* 'r' */
   SIMD_VR_REGS,		/* 'v' */
   SIMD_DMA_CONFIG_REGS,	/* 'd' */
   ALL_REGS,
   LIM_REG_CLASSES
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.   */
#define REG_CLASS_NAMES	  \
{			  \
  "NO_REGS",		  \
  "R0_REGS",		  \
  "R0R1_CD_REGS",	  \
  "R0R3_CD_REGS",	  \
  "ARCOMPACT16_REGS",	  \
  "SIBCALL_REGS",	  \
  "AC16_H_REGS",	  \
  "DOUBLE_REGS",	  \
  "GENERAL_REGS",	  \
  "SIMD_VR_REGS",	  \
  "SIMD_DMA_CONFIG_REGS", \
  "ALL_REGS"		  \
}

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS \
{									\
  {0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000}, /* NO_REGS.  */\
  {0x00000001, 0x00000000, 0x00000000, 0x00000000, 0x00000000}, /* 'x'.  */ \
  {0x00000003, 0x00000000, 0x00000000, 0x00000000, 0x00000000}, /* 'Rsd'.  */ \
  {0x0000000f, 0x00000000, 0x00000000, 0x00000000, 0x00000000}, /* 'Rcd'.  */ \
  {0x0000f00f, 0x00000000, 0x00000000, 0x00000000, 0x00000000}, /* 'q'.  */ \
  {0x00001fff, 0x00000000, 0x00000000, 0x00000000, 0x00000000}, /* 'Rsc'.  */ \
  {0x9fffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000}, /* 'h'.  */ \
  {0x00000000, 0x00000f00, 0x00000000, 0x00000000, 0x00000000}, /* 'D'.  */ \
  {0xffffffff, 0x8fffffff, 0x00000000, 0x00000000, 0x00030000}, /* 'r'.  */ \
  {0x00000000, 0x00000000, 0xffffffff, 0xffffffff, 0x00000000}, /* 'v'.  */ \
  {0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x0000ffff}, /* 'd'.  */ \
  {0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x0003ffff} /* ALL_REGS.  */\
}

/* Local macros to mark the first and last regs of different classes.  */
#define ARC_FIRST_SIMD_VR_REG              64
#define ARC_LAST_SIMD_VR_REG               127

#define ARC_FIRST_SIMD_DMA_CONFIG_REG      128
#define ARC_FIRST_SIMD_DMA_CONFIG_IN_REG   128
#define ARC_FIRST_SIMD_DMA_CONFIG_OUT_REG  136
#define ARC_LAST_SIMD_DMA_CONFIG_REG       143

/* ARCv2 double-register accumulator.  */
#define ACC_REG_FIRST 58
#define ACC_REG_LAST  59
#define ACCL_REGNO    (TARGET_BIG_ENDIAN ? ACC_REG_FIRST + 1 : ACC_REG_FIRST)
#define ACCH_REGNO    (TARGET_BIG_ENDIAN ? ACC_REG_FIRST : ACC_REG_FIRST + 1)

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

extern enum reg_class arc_regno_reg_class[];

#define REGNO_REG_CLASS(REGNO) (arc_regno_reg_class[REGNO])

/* The class value for valid index registers. An index register is
   one used in an address where its value is either multiplied by
   a scale factor or added to another register (as well as added to a
   displacement).  */

#define INDEX_REG_CLASS GENERAL_REGS

/* The class value for valid base registers. A base register is one used in
   an address which is the register value plus a displacement.  */

#define BASE_REG_CLASS GENERAL_REGS

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */
#define REGNO_OK_FOR_BASE_P(REGNO)					\
  ((REGNO) < 29								\
   || ((REGNO) == ARG_POINTER_REGNUM)					\
   || ((REGNO) == FRAME_POINTER_REGNUM)					\
   || ((REGNO) == PCL_REG)						\
   || (reg_renumber && ((unsigned) reg_renumber[REGNO] < 29))		\
   || ((unsigned) (REGNO) == (unsigned) arc_tp_regno)			\
   || (fixed_regs[REGNO] == 0 && IN_RANGE (REGNO, 32, 59))		\
   || (fixed_regs[REGNO] == 0 && (REGNO) == R30_REG))

#define REGNO_OK_FOR_INDEX_P(REGNO) REGNO_OK_FOR_BASE_P(REGNO)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */

#define PREFERRED_RELOAD_CLASS(X, CLASS) \
  arc_preferred_reload_class((X), (CLASS))

  extern enum reg_class arc_preferred_reload_class (rtx, enum reg_class);

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */

#define CLASS_MAX_NREGS(CLASS, MODE) \
(( GET_MODE_SIZE (MODE) == 16 && CLASS == SIMD_VR_REGS) ? 1: \
((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

#define SMALL_INT(X) ((unsigned) ((X) + 0x100) < 0x200)
#define SMALL_INT_RANGE(X, OFFSET, SHIFT) \
  ((unsigned) (((X) >> (SHIFT)) + 0x100) \
   < 0x200 - ((unsigned) (OFFSET) >> (SHIFT)))
#define SIGNED_INT12(X) ((unsigned) ((X) + 0x800) < 0x1000)
#define SIGNED_INT16(X) ((unsigned) ((X) + 0x8000) < 0x10000)
#define LARGE_INT(X) \
(((X) < 0) \
 ? (X) >= (-(HOST_WIDE_INT) 0x7fffffff - 1) \
 : (unsigned HOST_WIDE_INT) (X) <= (unsigned HOST_WIDE_INT) 0xffffffff)
#define UNSIGNED_INT3(X) ((unsigned) (X) < 0x8)
#define UNSIGNED_INT5(X) ((unsigned) (X) < 0x20)
#define UNSIGNED_INT6(X) ((unsigned) (X) < 0x40)
#define UNSIGNED_INT7(X) ((unsigned) (X) < 0x80)
#define UNSIGNED_INT8(X) ((unsigned) (X) < 0x100)
#define UNSIGNED_INT12(X) ((unsigned) (X) < 0x800)
#define UNSIGNED_INT16(X) ((unsigned) (X) < 0x10000)
#define IS_ONE(X) ((X) == 1)
#define IS_ZERO(X) ((X) == 0)

/* Stack layout and stack pointer usage.  */

/* Define this macro if pushing a word onto the stack moves the stack
   pointer to a smaller address.  */
#define STACK_GROWS_DOWNWARD 1

/* Define this if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */
#define FRAME_GROWS_DOWNWARD 1

/* Offset from the stack pointer register to the first location at which
   outgoing arguments are placed.  */
#define STACK_POINTER_OFFSET (0)

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET(FNDECL) (0)

/* A C expression whose value is RTL representing the address in a
   stack frame where the pointer to the caller's frame is stored.
   Assume that FRAMEADDR is an RTL expression for the address of the
   stack frame itself.

   If you don't define this macro, the default is to return the value
   of FRAMEADDR--that is, the stack frame address is also the address
   of the stack word that points to the previous frame.  */
/* ??? unfinished */
/*define DYNAMIC_CHAIN_ADDRESS (FRAMEADDR)*/

/* A C expression whose value is RTL representing the value of the
   return address for the frame COUNT steps up from the current frame.
   FRAMEADDR is the frame pointer of the COUNT frame, or the frame
   pointer of the COUNT - 1 frame if `RETURN_ADDR_IN_PREVIOUS_FRAME'
   is defined.  */
/* The current return address is in r31.  The return address of anything
   farther back is at [%fp,4].  */

#define RETURN_ADDR_RTX(COUNT, FRAME) \
arc_return_addr_rtx(COUNT,FRAME)

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 28

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 145
#define HARD_FRAME_POINTER_REGNUM 27

/* Base register for access to arguments of the function. This register
   will be eliminated into either fp or sp.  */
#define ARG_POINTER_REGNUM 144

#define RETURN_ADDR_REGNUM 31

/* TODO - check usage of STATIC_CHAIN_REGNUM with a testcase */
/* Register in which static-chain is passed to a function.  This must
   not be a register used by the prologue.  */
#define STATIC_CHAIN_REGNUM  11

/* Function argument passing.  */

/* If defined, the maximum amount of space required for outgoing
   arguments will be computed and placed into the variable
   `crtl->outgoing_args_size'.  No space will be pushed
   onto the stack for each call; instead, the function prologue should
   increase the stack frame size by this amount.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.  */
#define CUMULATIVE_ARGS int

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */
#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,INDIRECT,N_NAMED_ARGS) \
  ((CUM) = 0)

/* The number of registers used for parameter passing.  Local to this file.  */
#define MAX_ARC_PARM_REGS (TARGET_RF16 ? 4 : 8)

/* 1 if N is a possible register number for function argument passing.  */
#define FUNCTION_ARG_REGNO_P(N) \
((unsigned) (N) < MAX_ARC_PARM_REGS)

/* The ROUND_ADVANCE* macros are local to this file.  */
/* Round SIZE up to a word boundary.  */
#define ROUND_ADVANCE(SIZE) \
(((SIZE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Round arg MODE/TYPE up to the next word boundary.  */
#define ROUND_ADVANCE_ARG(MODE, TYPE) \
((MODE) == BLKmode				\
 ? ROUND_ADVANCE (int_size_in_bytes (TYPE))	\
 : ROUND_ADVANCE (GET_MODE_SIZE (MODE)))

#define ARC_FUNCTION_ARG_BOUNDARY(MODE,TYPE) PARM_BOUNDARY
/* Round CUM up to the necessary point for argument MODE/TYPE.  */
/* N.B. Vectors have alignment exceeding BIGGEST_ALIGNMENT.
   ARC_FUNCTION_ARG_BOUNDARY reduces this to no more than 32 bit.  */
#define ROUND_ADVANCE_CUM(CUM, MODE, TYPE) \
  ((((CUM) - 1) | (ARC_FUNCTION_ARG_BOUNDARY ((MODE), (TYPE)) - 1)/BITS_PER_WORD)\
   + 1)

/* Return boolean indicating arg of type TYPE and mode MODE will be passed in
   a reg.  This includes arguments that have to be passed by reference as the
   pointer to them is passed in a reg if one is available (and that is what
   we're given).
   When passing arguments NAMED is always 1.  When receiving arguments NAMED
   is 1 for each argument except the last in a stdarg/varargs function.  In
   a stdarg function we want to treat the last named arg as named.  In a
   varargs function we want to treat the last named arg (which is
   `__builtin_va_alist') as unnamed.
   This macro is only used in this file.  */
#define PASS_IN_REG_P(CUM, MODE, TYPE) \
((CUM) < MAX_ARC_PARM_REGS)


/* Function results.  */

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */
#define LIBCALL_VALUE(MODE) gen_rtx_REG (MODE, 0)

/* 1 if N is a possible register number for a function value
   as seen by the caller.  */
/* ??? What about r1 in DI/DF values.  */
#define FUNCTION_VALUE_REGNO_P(N) ((N) == 0)

/* Tell GCC to use RETURN_IN_MEMORY.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */
#define EXIT_IGNORE_STACK 0

#define EPILOGUE_USES(REGNO) arc_epilogue_uses ((REGNO))

#define EH_USES(REGNO) arc_eh_uses((REGNO))

/* Definitions for register eliminations.

   This is an array of structures.  Each structure initializes one pair
   of eliminable registers.  The "from" register number is given first,
   followed by "to".  Eliminations of the same "from" register are listed
   in order of preference.

   We have two registers that can be eliminated on the ARC.  First, the
   argument pointer register can always be eliminated in favor of the stack
   pointer register or frame pointer register.  Secondly, the frame pointer
   register can often be eliminated in favor of the stack pointer register.
*/

#define ELIMINABLE_REGS					\
{{ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},		\
 {ARG_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM},	\
 {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},	\
 {FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM}}

/* Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */
extern int arc_initial_elimination_offset(int from, int to);
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)                    \
  (OFFSET) = arc_initial_elimination_offset ((FROM), (TO))

/* All the work done in PROFILE_HOOK, but still required.  */
#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(STREAM, LABELNO) do { } while (0)

#define NO_PROFILE_COUNTERS  1

/* Trampolines.  */

/* Length in units of the trampoline for entering a nested function.  */
#define TRAMPOLINE_SIZE 16

/* Alignment required for a trampoline in bits .  */
/* For actual data alignment we just need 32, no more than the stack;
   however, to reduce cache coherency issues, we want to make sure that
   trampoline instructions always appear the same in any given cache line.  */
#define TRAMPOLINE_ALIGNMENT 256

/* Library calls.  */

/* Addressing modes, and classification of registers for them.  */

/* Maximum number of registers that can appear in a valid memory address.  */
/* The `ld' insn allows 2, but the `st' insn only allows 1.  */
#define MAX_REGS_PER_ADDRESS 1

/* We have pre inc/dec (load/store with update).  */
#define HAVE_PRE_INCREMENT 1
#define HAVE_PRE_DECREMENT 1
#define HAVE_POST_INCREMENT 1
#define HAVE_POST_DECREMENT 1
#define HAVE_PRE_MODIFY_DISP 1
#define HAVE_POST_MODIFY_DISP 1
#define HAVE_PRE_MODIFY_REG 1
#define HAVE_POST_MODIFY_REG 1
/* ??? should also do PRE_MODIFY_REG / POST_MODIFY_REG, but that requires
   a special predicate for the memory operand of stores, like for the SH.  */

/* Recognize any constant value that is a valid address.  */
#define CONSTANT_ADDRESS_P(X)					\
  (flag_pic ? (arc_legitimate_pic_addr_p (X) || LABEL_P (X)):	\
   (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF	\
    || GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST))

/* Is the argument a const_int rtx, containing an exact power of 2 */
#define  IS_POWEROF2_P(X) (! ( (X) & ((X) - 1)) && (X))
#define  IS_POWEROF2_OR_0_P(X) (! ( (X) & ((X) - 1)))

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx
   and check its validity for a certain class.
   We have two alternate definitions for each of them.
   The *_NONSTRICT definition accepts all pseudo regs; the other rejects
   them unless they have been allocated suitable hard regs.

   Most source files want to accept pseudo regs in the hope that
   they will get allocated to the class that the insn wants them to be in.
   Source files for reload pass need to be strict.
   After reload, it makes no difference, since pseudo regs have
   been eliminated by then.  */

/* Nonzero if X is a hard reg that can be used as an index
   or if it is a pseudo reg.  */
#define REG_OK_FOR_INDEX_P_NONSTRICT(X)			\
  ((unsigned) REGNO (X) >= FIRST_PSEUDO_REGISTER	\
   || REGNO_OK_FOR_BASE_P (REGNO (X)))

/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P_NONSTRICT(X)			\
  ((unsigned) REGNO (X) >= FIRST_PSEUDO_REGISTER	\
   || REGNO_OK_FOR_BASE_P (REGNO (X)))

/* Nonzero if X is a hard reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P_STRICT(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
/* Nonzero if X is a hard reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P_STRICT(X) REGNO_OK_FOR_BASE_P (REGNO (X))

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.  */
/* The `ld' insn allows [reg],[reg+shimm],[reg+limm],[reg+reg],[limm]
   but the `st' insn only allows [reg],[reg+shimm],[limm].
   The only thing we can do is only allow the most strict case `st' and hope
   other parts optimize out the restrictions for `ld'.  */

#define RTX_OK_FOR_BASE_P(X, STRICT) \
(REG_P (X) \
 && ((STRICT) ? REG_OK_FOR_BASE_P_STRICT (X) : REG_OK_FOR_BASE_P_NONSTRICT (X)))

#define RTX_OK_FOR_INDEX_P(X, STRICT) \
(REG_P (X) \
 && ((STRICT) ? REG_OK_FOR_INDEX_P_STRICT (X) : REG_OK_FOR_INDEX_P_NONSTRICT (X)))

/* A C compound statement that attempts to replace X, which is an address
   that needs reloading, with a valid memory address for an operand of
   mode MODE.  WIN is a C statement label elsewhere in the code.

   We try to get a normal form
   of the address.  That will allow inheritance of the address reloads.  */

#define LEGITIMIZE_RELOAD_ADDRESS(X,MODE,OPNUM,TYPE,IND_LEVELS,WIN)	\
  do {									\
    if (arc_legitimize_reload_address (&(X), (MODE), (OPNUM), (TYPE)))	\
      goto WIN;								\
  } while (0)

/* Reading lp_count for anything but the lp instruction is very slow on the
   ARC700.  */
#define DONT_REALLOC(REGNO,MODE) \
  (TARGET_ARC700 && (REGNO) == 60)


/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
   return the mode to be used for the comparison.  */
/*extern machine_mode arc_select_cc_mode ();*/
#define SELECT_CC_MODE(OP, X, Y) \
arc_select_cc_mode (OP, X, Y)

/* Return non-zero if SELECT_CC_MODE will never return MODE for a
   floating point inequality comparison.  */
#define REVERSIBLE_CC_MODE(MODE) 1 /*???*/

/* Costs.  */

/* The cost of a branch insn.  */
/* ??? What's the right value here?  Branches are certainly more
   expensive than reg->reg moves.  */
#define BRANCH_COST(speed_p, predictable_p) 2

/* Scc sets the destination to 1 and then conditionally zeroes it.
   Best case, ORed SCCs can be made into clear - condset - condset.
   But it could also end up as five insns.  So say it costs four on
   average.
   These extra instructions - and the second comparison - will also be
   an extra cost if the first comparison would have been decisive.
   So get an average saving, with a probability of the first branch
   beging decisive of p0, we want:
   p0 * (branch_cost - 4) > (1 - p0) * 5
   ??? We don't get to see that probability to evaluate, so we can
   only wildly guess that it might be 50%.
   ??? The compiler also lacks the notion of branch predictability.  */
#define LOGICAL_OP_NON_SHORT_CIRCUIT \
  (BRANCH_COST (optimize_function_for_speed_p (cfun), \
		false) > 9)

/* Nonzero if access to memory by bytes is slow and undesirable.
   For RISC chips, it means that access to memory by bytes is no
   better than access by words when possible, so grab a whole word
   and maybe make use of that.  */
#define SLOW_BYTE_ACCESS  0

/* Define this macro if it is as good or better to call a constant
   function address than to call an address kept in a register.  */
/* On the ARC, calling through registers is slow.  */
#define NO_FUNCTION_CSE 1

/* Section selection.  */
/* WARNING: These section names also appear in dwarfout.c.  */

#define TEXT_SECTION_ASM_OP	"\t.section\t.text"
#define DATA_SECTION_ASM_OP	"\t.section\t.data"

#define BSS_SECTION_ASM_OP	"\t.section\t.bss"
#define SDATA_SECTION_ASM_OP	"\t.section\t.sdata"
#define SBSS_SECTION_ASM_OP	"\t.section\t.sbss"

/* Expression whose value is a string, including spacing, containing the
   assembler operation to identify the following data as initialization/termination
   code. If not defined, GCC will assume such a section does not exist. */
#define INIT_SECTION_ASM_OP "\t.section\t.init"
#define FINI_SECTION_ASM_OP "\t.section\t.fini"

/* Define this macro if jump tables (for tablejump insns) should be
   output in the text section, along with the assembler instructions.
   Otherwise, the readonly data section is used.
   This macro is irrelevant if there is no separate readonly data section.  */
#define JUMP_TABLES_IN_TEXT_SECTION  (flag_pic || CASE_VECTOR_PC_RELATIVE)

/* For DWARF.  Marginally different than default so output is "prettier"
   (and consistent with above).  */
#define PUSHSECTION_FORMAT "\t%s %s\n"

/* Tell crtstuff.c we're using ELF.  */
#define OBJECT_FORMAT_ELF

/* PIC */

/* The register number of the register used to address a table of static
   data addresses in memory.  In some cases this register is defined by a
   processor's ``application binary interface'' (ABI).  When this macro
   is defined, RTL is generated for this register once, as with the stack
   pointer and frame pointer registers.  If this macro is not defined, it
   is up to the machine-dependent files to allocate such a register (if
   necessary).  */
#define PIC_OFFSET_TABLE_REGNUM 26

/* Define this macro if the register defined by PIC_OFFSET_TABLE_REGNUM is
   clobbered by calls.  Do not define this macro if PIC_OFFSET_TABLE_REGNUM
   is not defined.  */
/* This register is call-saved on the ARC.  */
/*#define PIC_OFFSET_TABLE_REG_CALL_CLOBBERED*/

/* A C expression that is nonzero if X is a legitimate immediate
   operand on the target machine when generating position independent code.
   You can assume that X satisfies CONSTANT_P, so you need not
   check this.  You can also assume `flag_pic' is true, so you need not
   check it either.  You need not define this macro if all constants
   (including SYMBOL_REF) can be immediate operands when generating
   position independent code.  */
#define LEGITIMATE_PIC_OPERAND_P(X)  \
  (!arc_raw_symbolic_reference_mentioned_p ((X), true))

/* PIC and small data don't mix on ARC because they use the same register.  */
#define SDATA_BASE_REGNUM 26

#define ASM_PREFERRED_EH_DATA_FORMAT(CODE, GLOBAL) \
  (flag_pic \
   ? (GLOBAL ? DW_EH_PE_indirect : 0) | DW_EH_PE_pcrel | DW_EH_PE_sdata4 \
   : DW_EH_PE_absptr)

/* Control the assembler format that we output.  */

/* A C string constant describing how to begin a comment in the target
   assembler language.  The compiler assumes that the comment will
   end at the end of the line.  */
/* Gas needs this to be "#" in order to recognize line directives.  */
#define ASM_COMMENT_START "#"

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */
#undef ASM_APP_ON
#define ASM_APP_ON ""

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */
#undef ASM_APP_OFF
#define ASM_APP_OFF ""

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.global\t"

/* This is how to output an assembler line defining a `char' constant.  */
#define ASM_OUTPUT_CHAR(FILE, VALUE) \
( fprintf (FILE, "\t.byte\t"),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* This is how to output an assembler line defining a `short' constant.  */
#define ASM_OUTPUT_SHORT(FILE, VALUE) \
( fprintf (FILE, "\t.hword\t"),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* This is how to output an assembler line defining an `int' constant.
   We also handle symbol output here.  Code addresses must be right shifted
   by 2 because that's how the jump instruction wants them.  */
#define ASM_OUTPUT_INT(FILE, VALUE) \
do {									\
  fprintf (FILE, "\t.word\t");						\
  if (GET_CODE (VALUE) == LABEL_REF)					\
    {									\
      fprintf (FILE, "%%st(@");						\
      output_addr_const (FILE, (VALUE));				\
      fprintf (FILE, ")");						\
    }									\
  else									\
    output_addr_const (FILE, (VALUE));					\
  fprintf (FILE, "\n");					                \
} while (0)

/* This is how to output an assembler line defining a `float' constant.  */
#define ASM_OUTPUT_FLOAT(FILE, VALUE) \
{							\
  long t;						\
  char str[30];						\
  REAL_VALUE_TO_TARGET_SINGLE ((VALUE), t);		\
  REAL_VALUE_TO_DECIMAL ((VALUE), "%.20e", str);	\
  fprintf (FILE, "\t.word\t0x%lx %s %s\n",		\
	   t, ASM_COMMENT_START, str);			\
}

/* This is how to output an assembler line defining a `double' constant.  */
#define ASM_OUTPUT_DOUBLE(FILE, VALUE) \
{							\
  long t[2];						\
  char str[30];						\
  REAL_VALUE_TO_TARGET_DOUBLE ((VALUE), t);		\
  REAL_VALUE_TO_DECIMAL ((VALUE), "%.20e", str);	\
  fprintf (FILE, "\t.word\t0x%lx %s %s\n\t.word\t0x%lx\n", \
	   t[0], ASM_COMMENT_START, str, t[1]);		\
}

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */
#define ASM_OUTPUT_LABEL(FILE, NAME) \
do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0)

#define ASM_NAME_P(NAME) ( NAME[0]=='*')

/* This is how to output a reference to a user-level label named NAME.
   `assemble_name' uses this.  */
/* We work around a dwarfout.c deficiency by watching for labels from it and
   not adding the '_' prefix.  There is a comment in
   dwarfout.c that says it should be using ASM_OUTPUT_INTERNAL_LABEL.  */
#define ASM_OUTPUT_LABELREF(FILE, NAME1) \
do {							\
  const char *NAME;					\
  NAME = (*targetm.strip_name_encoding)(NAME1);		\
  if ((NAME)[0] == '.' && (NAME)[1] == 'L')		\
    fprintf (FILE, "%s", NAME);				\
  else							\
    {							\
      if (!ASM_NAME_P (NAME1))				\
	fprintf (FILE, "%s", user_label_prefix);	\
      fprintf (FILE, "%s", NAME);			\
    }							\
} while (0)

/* This is how to output a reference to a symbol_ref / label_ref as
   (part of) an operand.  To disambiguate from register names like
   a1 / a2 / status etc, symbols are preceded by '@'.  */
#define ASM_OUTPUT_SYMBOL_REF(FILE,SYM) \
  ASM_OUTPUT_LABEL_REF ((FILE), XSTR ((SYM), 0))
#define ASM_OUTPUT_LABEL_REF(FILE,STR)			\
  do							\
    {							\
      fputc ('@', file);				\
      assemble_name ((FILE), (STR));			\
    }							\
  while (0)

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */
#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)			\
  ((OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),			\
   sprintf ((OUTPUT), "%s.%u", (NAME), (unsigned int)(LABELNO)))

/* The following macro defines the format used to output the second
   operand of the .type assembler directive.  Different svr4 assemblers
   expect various different forms for this operand.  The one given here
   is just a default.  You may need to override it in your machine-
   specific tm.h file (depending upon the particulars of your assembler).  */

#undef  TYPE_OPERAND_FMT
#define TYPE_OPERAND_FMT	"@%s"

/*  A C string containing the appropriate assembler directive to
    specify the size of a symbol, without any arguments.  On systems
    that use ELF, the default (in `config/elfos.h') is `"\t.size\t"';
    on other systems, the default is not to define this macro.  */
#undef SIZE_ASM_OP
#define SIZE_ASM_OP "\t.size\t"

/* Assembler pseudo-op to equate one value with another.  */
/* ??? This is needed because dwarfout.c provides a default definition too
   late for defaults.h (which contains the default definition of ASM_OTPUT_DEF
   that we use).  */
#ifdef SET_ASM_OP
#undef SET_ASM_OP
#endif
#define SET_ASM_OP "\t.set\t"

extern char rname29[], rname30[];
extern char rname56[], rname57[], rname58[], rname59[];
/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */
#define REGISTER_NAMES								\
{  "r0",   "r1",   "r2",   "r3",       "r4",     "r5",     "r6",    "r7",	\
   "r8",   "r9",  "r10",  "r11",      "r12",    "r13",    "r14",   "r15",	\
  "r16",  "r17",  "r18",  "r19",      "r20",    "r21",    "r22",   "r23",	\
  "r24",  "r25",   "gp",   "fp",       "sp",  rname29,  rname30, "blink",	\
  "r32",  "r33",  "r34",  "r35",      "r36",    "r37",    "r38",   "r39",	\
   "d1",   "d1",   "d2",   "d2",      "r44",    "r45",    "r46",   "r47",	\
  "r48",  "r49",  "r50",  "r51",      "r52",    "r53",    "r54",   "r55",	\
  rname56,rname57,rname58,rname59,"lp_count",    "cc",   "limm",   "pcl",	\
  "vr0",  "vr1",  "vr2",  "vr3",      "vr4",    "vr5",    "vr6",   "vr7",       \
  "vr8",  "vr9", "vr10", "vr11",     "vr12",   "vr13",   "vr14",  "vr15",	\
 "vr16", "vr17", "vr18", "vr19",     "vr20",   "vr21",   "vr22",  "vr23",	\
 "vr24", "vr25", "vr26", "vr27",     "vr28",   "vr29",   "vr30",  "vr31",	\
 "vr32", "vr33", "vr34", "vr35",     "vr36",   "vr37",   "vr38",  "vr39",	\
 "vr40", "vr41", "vr42", "vr43",     "vr44",   "vr45",   "vr46",  "vr47",	\
 "vr48", "vr49", "vr50", "vr51",     "vr52",   "vr53",   "vr54",  "vr55",	\
 "vr56", "vr57", "vr58", "vr59",     "vr60",   "vr61",   "vr62",  "vr63",	\
  "dr0",  "dr1",  "dr2",  "dr3",      "dr4",    "dr5",    "dr6",   "dr7",	\
  "dr0",  "dr1",  "dr2",  "dr3",      "dr4",    "dr5",    "dr6",   "dr7",	\
  "arg", "frame" \
}

#define ADDITIONAL_REGISTER_NAMES		\
{						\
  {"r26",    26},				\
  {"r27",    27},				\
  {"ilink",  29},				\
  {"r29",    29},				\
  {"r30",    30},				\
  {"r40",    40},				\
  {"r41",    41},				\
  {"r42",    42},				\
  {"r43",    43},				\
  {"r56",    56},				\
  {"r57",    57},				\
  {"r58",    58},				\
  {"r59",    59}				\
}

/* Entry to the insn conditionalizer.  */
#define FINAL_PRESCAN_INSN(INSN, OPVEC, NOPERANDS) \
  arc_final_prescan_insn (INSN, OPVEC, NOPERANDS)

/* A C expression which evaluates to true if CODE is a valid
   punctuation character for use in the `PRINT_OPERAND' macro.  */
extern char arc_punct_chars[];
#define PRINT_OPERAND_PUNCT_VALID_P(CHAR) \
arc_punct_chars[(unsigned char) (CHAR)]

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */
#define PRINT_OPERAND(FILE, X, CODE) \
arc_print_operand (FILE, X, CODE)

/* A C compound statement to output to stdio stream STREAM the
   assembler syntax for an instruction operand that is a memory
   reference whose address is ADDR.  ADDR is an RTL expression.

   On some machines, the syntax for a symbolic address depends on
   the section that the address refers to.  On these machines,
   define the macro `ENCODE_SECTION_INFO' to store the information
   into the `symbol_ref', and then check for it here.  */
#define PRINT_OPERAND_ADDRESS(FILE, ADDR) \
arc_print_operand_address (FILE, ADDR)

/* This is how to output an element of a case-vector that is absolute.  */
#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
do {							\
  char label[30];					\
  ASM_GENERATE_INTERNAL_LABEL (label, "L", VALUE);	\
  fprintf (FILE, "\t.word ");				\
  assemble_name (FILE, label);				\
  fprintf (FILE, "\n");					\
} while (0)

/* This is how to output an element of a case-vector that is relative.  */
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL)	\
  do {								\
    char label[30];						\
    ASM_GENERATE_INTERNAL_LABEL (label, "L", VALUE);		\
    if (!TARGET_BI_BIH)						\
      {								\
	switch (GET_MODE (BODY))				\
	  {							\
	  case E_QImode: fprintf (FILE, "\t.byte "); break;	\
	  case E_HImode: fprintf (FILE, "\t.hword "); break;	\
	  case E_SImode: fprintf (FILE, "\t.word "); break;	\
	  default: gcc_unreachable ();				\
	  }							\
	assemble_name (FILE, label);				\
	fprintf (FILE, "-");					\
	ASM_GENERATE_INTERNAL_LABEL (label, "L", REL);		\
	assemble_name (FILE, label);				\
	fprintf (FILE, "\n");					\
      }								\
    else							\
      {								\
      switch (GET_MODE (BODY))					\
	{							\
	case E_SImode: fprintf (FILE, "\tb\t@"); break;		\
	case E_HImode:						\
	case E_QImode: fprintf (FILE, "\tb_s\t@"); break;	\
	default: gcc_unreachable ();				\
	}							\
      assemble_name (FILE, label);				\
      fprintf(FILE, "\n");					\
    }								\
  } while (0)

/* Defined to also emit an .align in elfos.h.  We don't want that.  */
#undef ASM_OUTPUT_CASE_LABEL

/* ADDR_DIFF_VECs are in the text section and thus can affect the
   current alignment.  */
#define ASM_OUTPUT_CASE_END(FILE, NUM, JUMPTABLE)       \
  do                                                    \
    {                                                   \
      if (GET_CODE (PATTERN (JUMPTABLE)) == ADDR_DIFF_VEC \
	  && ((GET_MODE_SIZE (as_a <scalar_int_mode>	\
			      (GET_MODE (PATTERN (JUMPTABLE)))) \
	       * XVECLEN (PATTERN (JUMPTABLE), 1) + 1)	\
	      & 2))					\
      arc_toggle_unalign ();				\
    }                                                   \
  while (0)

#define JUMP_ALIGN(LABEL) (arc_size_opt_level < 2 ? 2 : 0)
#define LABEL_ALIGN_AFTER_BARRIER(LABEL) \
  (JUMP_ALIGN(LABEL) \
   ? JUMP_ALIGN(LABEL) \
   : GET_CODE (PATTERN (prev_active_insn (LABEL))) == ADDR_DIFF_VEC \
   ? 1 : 0)
/* The desired alignment for the location counter at the beginning
   of a loop.  */
/* On the ARC, align loops to 4 byte boundaries unless doing all-out size
   optimization.  */
#define LOOP_ALIGN(X) 0

#define LABEL_ALIGN(LABEL) (arc_label_align (LABEL))

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */
#define ASM_OUTPUT_ALIGN(FILE,LOG) \
do { \
  if ((LOG) != 0) fprintf (FILE, "\t.align %d\n", 1 << (LOG)); \
  if ((LOG)  > 1) \
    arc_clear_unalign (); \
} while (0)

/*  ASM_OUTPUT_ALIGNED_DECL_LOCAL (STREAM, DECL, NAME, SIZE, ALIGNMENT)
    Define this macro when you need to see the variable's decl in order to
    chose what to output.  */
#define ASM_OUTPUT_ALIGNED_DECL_LOCAL(STREAM, DECL, NAME, SIZE, ALIGNMENT) \
  arc_asm_output_aligned_decl_local (STREAM, DECL, NAME, SIZE, ALIGNMENT, 0)

/* Debugging information.  */

/* Generate DWARF debugging information.  */
#ifdef DWARF2_DEBUGGING_INFO
#undef DWARF2_DEBUGGING_INFO
#endif
#define DWARF2_DEBUGGING_INFO

/* Prefer STABS (for now).  */
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* How to renumber registers for gdb.  */
#define DEBUGGER_REGNO(REGNO)				\
  ((TARGET_MULMAC_32BY16_SET && (REGNO) >= 56 && (REGNO) <= 57) \
   ? ((REGNO) ^ !TARGET_BIG_ENDIAN)				\
   : (TARGET_MUL64_SET && (REGNO) >= 57 && (REGNO) <= 58)	\
   ? (((REGNO) == 57)						\
      ? 58 /* MMED */						\
      : 57 + !!TARGET_MULMAC_32BY16_SET) /* MLO */		\
   : (REGNO))

/* Use gcc hard register numbering for eh_frame.  */
#define DWARF_FRAME_REGNUM(REG) ((REG) < 144 ? REG : INVALID_REGNUM)

/* Map register numbers held in the call frame info that gcc has
   collected using DWARF_FRAME_REGNUM to those that should be output
   in .debug_frame and .eh_frame.  */
#define DWARF2_FRAME_REG_OUT(REGNO, FOR_EH)			\
  ((TARGET_MULMAC_32BY16_SET && (REGNO) >= 56 && (REGNO) <= 57) \
   ? ((REGNO) ^ !TARGET_BIG_ENDIAN)				\
   : (TARGET_MUL64_SET && (REGNO) >= 57 && (REGNO) <= 58)	\
   ? (((REGNO) == 57)						\
      ? 58 /* MMED */						\
      : 57 + !!TARGET_MULMAC_32BY16_SET) /* MLO */		\
   : (REGNO))

/* The DWARF 2 CFA column which tracks the return address.  */
#define DWARF_FRAME_RETURN_COLUMN RETURN_ADDR_REGNUM
#define INCOMING_RETURN_ADDR_RTX gen_rtx_REG (Pmode, RETURN_ADDR_REGNUM)

/* The DWARF 2 CFA column which tracks the return address from a signal handler
   context.  This value must not correspond to a hard register and must be out
   of the range of DWARF_FRAME_REGNUM().  */
#define DWARF_ALT_FRAME_RETURN_COLUMN 144

/* Frame info.  */

#define EH_RETURN_DATA_REGNO(N)  ((N) < 2 ? (N) : INVALID_REGNUM)

#define EH_RETURN_STACKADJ_RTX   gen_rtx_REG (Pmode, 2)

/* Miscellaneous.  */

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.
   If we have pc relative case vectors, we start the case vector shortening
   with QImode.  */
#define CASE_VECTOR_MODE						\
  (TARGET_BI_BIH ? SImode						\
   : (optimize && (CASE_VECTOR_PC_RELATIVE || flag_pic)) ? QImode : Pmode)

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses.  */
#define CASE_VECTOR_PC_RELATIVE					\
  (TARGET_CASE_VECTOR_PC_RELATIVE || TARGET_BI_BIH)

#define CASE_VECTOR_SHORTEN_MODE(MIN_OFFSET, MAX_OFFSET, BODY)		\
  (TARGET_BI_BIH ?						\
   ((MIN_OFFSET) >= -512 && (MAX_OFFSET) <= 508 ? HImode : SImode)	\
   : ((MIN_OFFSET) >= 0 && (MAX_OFFSET) <= 255				\
      ? (ADDR_DIFF_VEC_FLAGS (BODY).offset_unsigned = 1, QImode)	\
      : (MIN_OFFSET) >= -128 && (MAX_OFFSET) <= 127			\
      ? (ADDR_DIFF_VEC_FLAGS (BODY).offset_unsigned = 0, QImode)	\
      : (MIN_OFFSET) >= 0 && (MAX_OFFSET) <= 65535			\
      ? (ADDR_DIFF_VEC_FLAGS (BODY).offset_unsigned = 1, HImode)	\
      : (MIN_OFFSET) >= -32768 && (MAX_OFFSET) <= 32767			\
      ? (ADDR_DIFF_VEC_FLAGS (BODY).offset_unsigned = 0, HImode)	\
      : SImode))

#define ADDR_VEC_ALIGN(VEC_INSN)					\
  (TARGET_BI_BIH ? 0							\
   : exact_log2 (GET_MODE_SIZE (as_a <scalar_int_mode>			\
				(GET_MODE (PATTERN (VEC_INSN))))))

#define INSN_LENGTH_ALIGNMENT(INSN)		  \
  ((JUMP_TABLE_DATA_P (INSN)			  \
    && GET_CODE (PATTERN (INSN)) == ADDR_DIFF_VEC \
    && GET_MODE (PATTERN (INSN)) == QImode)	  \
   ? 0 : length_unit_log)

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS 1

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, NIL if none.  */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND


/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 4

/* Undo the effects of the cpymem pattern presence on STORE_BY_PIECES_P .  */
#define MOVE_RATIO(SPEED) ((SPEED) ? 15 : 3)

/* Define this to be nonzero if shift instructions ignore all but the
   low-order few bits.
*/
#define SHIFT_COUNT_TRUNCATED 1

/* Defines if the CLZ result is undefined or has a useful value.  */
#define CLZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE)  ((VALUE) = 31, 2)

/* Defines if the CTZ result is undefined or has a useful value.  */
#define CTZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE)  ((VALUE) = 31, 2)

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */
#define STORE_FLAG_VALUE 1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
/* ARCompact has full 32-bit pointers.  */
#define Pmode SImode

/* A function address in a call instruction.  */
#define FUNCTION_MODE SImode

/* Define the information needed to generate branch and scc insns.  This is
   stored from the compare operation.  Note that we can't use "rtx" here
   since it hasn't been defined!  */
extern struct rtx_def *arc_compare_op0, *arc_compare_op1;

/* ARC function types.   */
enum arc_function_type {
  /* No function should have the unknown type.  This value is used to
   indicate the that function type has not yet been computed.  */
  ARC_FUNCTION_UNKNOWN  = 0,

  /* The normal function type indicates that the function has the
   standard prologue and epilogue.  */
  ARC_FUNCTION_NORMAL  = 1 << 0,
  /* These are interrupt handlers.  The name corresponds to the register
     name that contains the return address.  */
  ARC_FUNCTION_ILINK1  = 1 << 1,
  ARC_FUNCTION_ILINK2  = 1 << 2,
  /* Fast interrupt is only available on ARCv2 processors.  */
  ARC_FUNCTION_FIRQ    = 1 << 3,
  /* The naked function type indicates that the function does not have
   prologue or epilogue, and that no stack frame is available.  */
  ARC_FUNCTION_NAKED   = 1 << 4
};

/* Check if a function is an interrupt function.  */
#define ARC_INTERRUPT_P(TYPE)					\
  (((TYPE) & (ARC_FUNCTION_ILINK1 | ARC_FUNCTION_ILINK2		\
	      | ARC_FUNCTION_FIRQ)) != 0)

/* Check if a function is a fast interrupt function.  */
#define ARC_FAST_INTERRUPT_P(TYPE) (((TYPE) & ARC_FUNCTION_FIRQ) != 0)

/* Check if a function is normal, that is, has standard prologue and
   epilogue.  */
#define ARC_NORMAL_P(TYPE) (((TYPE) & ARC_FUNCTION_NORMAL) != 0)

/* Check if a function is naked.  */
#define ARC_NAKED_P(TYPE) (((TYPE) & ARC_FUNCTION_NAKED) != 0)

/* Called by crtstuff.c to make calls to function FUNCTION that are defined in
   SECTION_OP, and then to switch back to text section.  */
#undef CRT_CALL_STATIC_FUNCTION
#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC)		\
  asm (SECTION_OP "\n\t"					\
       "add r12,pcl,@" USER_LABEL_PREFIX #FUNC "@pcl\n\t"	\
       "jl  [r12]\n"						\
       TEXT_SECTION_ASM_OP);

/* This macro expands to the name of the scratch register r12, used for
   temporary calculations according to the ABI.  */
#define ARC_TEMP_SCRATCH_REG "r12"

/* The C++ compiler must use one bit to indicate whether the function
   that will be called through a pointer-to-member-function is
   virtual.  Normally, we assume that the low-order bit of a function
   pointer must always be zero.  Then, by ensuring that the
   vtable_index is odd, we can distinguish which variant of the union
   is in use.  But, on some platforms function pointers can be odd,
   and so this doesn't work.  In that case, we use the low-order bit
   of the `delta' field, and shift the remainder of the `delta' field
   to the left. We needed to do this for A4 because the address was always
   shifted and thus could be odd.  */
#define TARGET_PTRMEMFUNC_VBIT_LOCATION \
  (ptrmemfunc_vbit_in_pfn)

#define INSN_SETS_ARE_DELAYED(X)		\
  (GET_CODE (X) == INSN				\
   && GET_CODE (PATTERN (X)) != SEQUENCE	\
   && GET_CODE (PATTERN (X)) != USE		\
   && GET_CODE (PATTERN (X)) != CLOBBER		\
   && (get_attr_type (X) == TYPE_CALL || get_attr_type (X) == TYPE_SFUNC))

#define INSN_REFERENCES_ARE_DELAYED(insn)				\
  (INSN_SETS_ARE_DELAYED (insn))

#define CALL_ATTR(X, NAME) \
  ((CALL_P (X) || NONJUMP_INSN_P (X)) \
   && GET_CODE (PATTERN (X)) != USE \
   && GET_CODE (PATTERN (X)) != CLOBBER \
   && get_attr_is_##NAME (X) == IS_##NAME##_YES) \

#define REVERSE_CONDITION(CODE,MODE)				 \
  (((MODE) == CC_FP_GTmode || (MODE) == CC_FP_GEmode		 \
    || (MODE) == CC_FP_UNEQmode || (MODE) == CC_FP_ORDmode	 \
    || (MODE) == CC_FPXmode || (MODE) == CC_FPU_UNEQmode	 \
    || (MODE) == CC_FPUmode || (MODE) == CC_FPUEmode)		 \
   ? reverse_condition_maybe_unordered ((CODE))			 \
   : reverse_condition ((CODE)))

#define ADJUST_INSN_LENGTH(X, LENGTH) \
  ((LENGTH) \
   = (GET_CODE (PATTERN (X)) == SEQUENCE \
      ? ((LENGTH) \
	 + arc_adjust_insn_length ( \
             as_a <rtx_sequence *> (PATTERN (X))->insn (0), \
	     get_attr_length (as_a <rtx_sequence *> (PATTERN (X))->insn (0)), \
	     true)							\
	 - get_attr_length (as_a <rtx_sequence *> (PATTERN (X))->insn (0)) \
	 + arc_adjust_insn_length ( \
	     as_a <rtx_sequence *> (PATTERN (X))->insn (1), \
	     get_attr_length (as_a <rtx_sequence *> (PATTERN (X))->insn (1)), \
	     true)							\
	 - get_attr_length (as_a <rtx_sequence *> (PATTERN (X))->insn (1))) \
      : arc_adjust_insn_length ((X), (LENGTH), false)))

#define IS_ASM_LOGICAL_LINE_SEPARATOR(C,STR) ((C) == '`')

#define INIT_EXPANDERS arc_init_expanders ()

enum
{
  ARC_LRA_PRIORITY_NONE, ARC_LRA_PRIORITY_NONCOMPACT, ARC_LRA_PRIORITY_COMPACT
};

/* The define_cond_exec construct is rather crude, as we can't have
   different ones with different conditions apply to different sets
   of instructions.  We can't use an attribute test inside the condition,
   because that would lead to infinite recursion as the attribute test
   needs to recognize the insn.  So, instead we have a clause for
   the pattern condition of all sfunc patterns which is only relevant for
   the predicated varaint.  */
#define SFUNC_CHECK_PREDICABLE \
  (GET_CODE (PATTERN (insn)) != COND_EXEC || !flag_pic || !TARGET_MEDIUM_CALLS)

/* MPYW feature macro.  Only valid for ARCHS and ARCEM cores.  */
#define TARGET_MPYW     ((arc_mpy_option > 0) && TARGET_V2)
/* Full ARCv2 multiplication feature macro.  */
#define TARGET_MULTI    ((arc_mpy_option > 1) && TARGET_V2)
/* General MPY feature macro.  */
#define TARGET_MPY      ((TARGET_ARC700 && (!TARGET_NOMPY_SET)) || TARGET_MULTI)
/* ARC700 MPY feature macro.  */
#define TARGET_ARC700_MPY (TARGET_ARC700 && (!TARGET_NOMPY_SET))
/* Any multiplication feature macro.  */
#define TARGET_ANY_MPY						\
  (TARGET_MPY || TARGET_MUL64_SET || TARGET_MULMAC_32BY16_SET)
/* PLUS_DMPY feature macro.  */
#define TARGET_PLUS_DMPY  ((arc_mpy_option > 6) && TARGET_HS)
/* PLUS_MACD feature macro.  */
#define TARGET_PLUS_MACD  ((arc_mpy_option > 7) && TARGET_HS)
/* PLUS_QMACW feature macro.  */
#define TARGET_PLUS_QMACW ((arc_mpy_option > 8) && TARGET_HS)

/* ARC600 and ARC601 feature macro.  */
#define TARGET_ARC600_FAMILY (TARGET_ARC600 || TARGET_ARC601)
/* ARC600, ARC601 and ARC700 feature macro.  */
#define TARGET_ARCOMPACT_FAMILY				\
  (TARGET_ARC600 || TARGET_ARC601 || TARGET_ARC700)
/* Loop count register can be read in very next instruction after has
   been written to by an ordinary instruction.  */
#define TARGET_LP_WR_INTERLOCK (!TARGET_ARC600_FAMILY)

/* FPU defines.  */
/* Any FPU support.  */
#define TARGET_HARD_FLOAT   ((arc_fpu_build & (FPU_SP | FPU_DP)) != 0)
/* Single precision floating point support.  */
#define TARGET_FP_SP_BASE   ((arc_fpu_build & FPU_SP) != 0)
/* Double precision floating point support.  */
#define TARGET_FP_DP_BASE   ((arc_fpu_build & FPU_DP) != 0)
/* Single precision floating point support with fused operation.  */
#define TARGET_FP_SP_FUSED  ((arc_fpu_build & FPU_SF) != 0)
/* Double precision floating point support with fused operation.  */
#define TARGET_FP_DP_FUSED  ((arc_fpu_build & FPU_DF) != 0)
/* Single precision floating point conversion instruction support.  */
#define TARGET_FP_SP_CONV   ((arc_fpu_build & FPU_SC) != 0)
/* Double precision floating point conversion instruction support.  */
#define TARGET_FP_DP_CONV   ((arc_fpu_build & FPU_DC) != 0)
/* Single precision floating point SQRT/DIV instruction support.  */
#define TARGET_FP_SP_SQRT   ((arc_fpu_build & FPU_SD) != 0)
/* Double precision floating point SQRT/DIV instruction support.  */
#define TARGET_FP_DP_SQRT   ((arc_fpu_build & FPU_DD) != 0)
/* Double precision floating point assist instruction support.  */
#define TARGET_FP_DP_AX     ((arc_fpu_build & FPX_DP) != 0)
/* Custom FP instructions used by QuarkSE EM cpu.  */
#define TARGET_FPX_QUARK    (TARGET_EM && TARGET_SPFP		\
			     && (arc_fpu_build == FPX_QK))
/* DBNZ support is available for ARCv2 core3 and newer cpus.  */
#define TARGET_DBNZ (TARGET_V2 && (arc_tune >= ARC_TUNE_CORE_3))

/* BI/BIH feature macro.  */
#define TARGET_BI_BIH (TARGET_BRANCH_INDEX && TARGET_CODE_DENSITY)

/* The default option for BI/BIH instructions.  */
#define DEFAULT_BRANCH_INDEX 0

#ifndef TARGET_LRA
#define TARGET_LRA arc_lra_p()
#endif

#endif /* GCC_ARC_H */
