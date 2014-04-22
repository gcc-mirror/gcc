/* Machine description for AArch64 architecture.
   Copyright (C) 2009-2014 Free Software Foundation, Inc.
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

/* Target CPU builtins.  */
#define TARGET_CPU_CPP_BUILTINS()			\
  do							\
    {							\
      builtin_define ("__aarch64__");			\
      if (TARGET_BIG_END)				\
	builtin_define ("__AARCH64EB__");		\
      else						\
	builtin_define ("__AARCH64EL__");		\
							\
      if (!TARGET_GENERAL_REGS_ONLY)			\
	builtin_define ("__ARM_NEON");			\
							\
      switch (aarch64_cmodel)				\
	{						\
	  case AARCH64_CMODEL_TINY:			\
	  case AARCH64_CMODEL_TINY_PIC:			\
	    builtin_define ("__AARCH64_CMODEL_TINY__");	\
	    break;					\
	  case AARCH64_CMODEL_SMALL:			\
	  case AARCH64_CMODEL_SMALL_PIC:		\
	    builtin_define ("__AARCH64_CMODEL_SMALL__");\
	    break;					\
	  case AARCH64_CMODEL_LARGE:			\
	    builtin_define ("__AARCH64_CMODEL_LARGE__");	\
	    break;					\
	  default:					\
	    break;					\
	}						\
							\
      if (TARGET_ILP32)					\
	{						\
	  cpp_define (parse_in, "_ILP32");		\
	  cpp_define (parse_in, "__ILP32__");		\
	}						\
      if (TARGET_CRYPTO)				\
	builtin_define ("__ARM_FEATURE_CRYPTO");	\
    } while (0)



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
   -mgeneral-regs-only.  */
#define TARGET_SIMD !TARGET_GENERAL_REGS_ONLY
#define TARGET_FLOAT !TARGET_GENERAL_REGS_ONLY

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

/* The architecture reserves all bits of the address for hardware use,
   so the vbit must go into the delta field of pointers to member
   functions.  This is the same config as that in the AArch32
   port.  */
#define TARGET_PTRMEMFUNC_VBIT_LOCATION ptrmemfunc_vbit_in_delta

/* Make strings word-aligned so that strcpy from constants will be
   faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)		\
  ((TREE_CODE (EXP) == STRING_CST		\
    && !optimize_size				\
    && (ALIGN) < BITS_PER_WORD)			\
   ? BITS_PER_WORD : ALIGN)

#define DATA_ALIGNMENT(EXP, ALIGN)		\
  ((((ALIGN) < BITS_PER_WORD)			\
    && (TREE_CODE (EXP) == ARRAY_TYPE		\
	|| TREE_CODE (EXP) == UNION_TYPE	\
	|| TREE_CODE (EXP) == RECORD_TYPE))	\
   ? BITS_PER_WORD : (ALIGN))

#define LOCAL_ALIGNMENT(EXP, ALIGN) DATA_ALIGNMENT(EXP, ALIGN)

#define STRUCTURE_SIZE_BOUNDARY		8

/* Defined by the ABI */
#define WCHAR_TYPE "unsigned int"
#define WCHAR_TYPE_SIZE			32

/* Using long long breaks -ansi and -std=c90, so these will need to be
   made conditional for an LLP64 ABI.  */

#define SIZE_TYPE	"long unsigned int"

#define PTRDIFF_TYPE	"long int"

#define PCC_BITFIELD_TYPE_MATTERS	1


/* Instruction tuning/selection flags.  */

/* Bit values used to identify processor capabilities.  */
#define AARCH64_FL_SIMD       (1 << 0)	/* Has SIMD instructions.  */
#define AARCH64_FL_FP         (1 << 1)	/* Has FP.  */
#define AARCH64_FL_CRYPTO     (1 << 2)	/* Has crypto.  */
#define AARCH64_FL_SLOWMUL    (1 << 3)	/* A slow multiply core.  */
#define AARCH64_FL_CRC        (1 << 4)	/* Has CRC.  */

/* Has FP and SIMD.  */
#define AARCH64_FL_FPSIMD     (AARCH64_FL_FP | AARCH64_FL_SIMD)

/* Has FP without SIMD.  */
#define AARCH64_FL_FPQ16      (AARCH64_FL_FP & ~AARCH64_FL_SIMD)

/* Architecture flags that effect instruction selection.  */
#define AARCH64_FL_FOR_ARCH8       (AARCH64_FL_FPSIMD)

/* Macros to test ISA flags.  */
extern unsigned long aarch64_isa_flags;
#define AARCH64_ISA_CRC            (aarch64_isa_flags & AARCH64_FL_CRC)
#define AARCH64_ISA_CRYPTO         (aarch64_isa_flags & AARCH64_FL_CRYPTO)
#define AARCH64_ISA_FP             (aarch64_isa_flags & AARCH64_FL_FP)
#define AARCH64_ISA_SIMD           (aarch64_isa_flags & AARCH64_FL_SIMD)

/* Macros to test tuning flags.  */
extern unsigned long aarch64_tune_flags;
#define AARCH64_TUNE_SLOWMUL       (aarch64_tune_flags & AARCH64_FL_SLOWMUL)

/* Crypto is an optional feature.  */
#define TARGET_CRYPTO AARCH64_ISA_CRYPTO

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
   significant bits.  Unlike AArch32 S1 is not packed into D0,
   etc.  */

/* Note that we don't mark X30 as a call-clobbered register.  The idea is
   that it's really the call instructions themselves which clobber X30.
   We don't care what the called function does with it afterwards.

   This approach makes it easier to implement sibcalls.  Unlike normal
   calls, sibcalls don't clobber X30, so the register reaches the
   called function intact.  EPILOGUE_USES says that X30 is useful
   to the called function.  */

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
    1, 1, 1,			/* SFP, AP, CC */	\
  }

#define CALL_USED_REGISTERS				\
  {							\
    1, 1, 1, 1,   1, 1, 1, 1,	/* R0 - R7 */		\
    1, 1, 1, 1,   1, 1, 1, 1,	/* R8 - R15 */		\
    1, 1, 1, 0,   0, 0, 0, 0,	/* R16 - R23 */		\
    0, 0, 0, 0,   0, 1, 0, 1,	/* R24 - R30, SP */	\
    1, 1, 1, 1,   1, 1, 1, 1,	/* V0 - V7 */		\
    0, 0, 0, 0,   0, 0, 0, 0,	/* V8 - V15 */		\
    1, 1, 1, 1,   1, 1, 1, 1,   /* V16 - V23 */         \
    1, 1, 1, 1,   1, 1, 1, 1,   /* V24 - V31 */         \
    1, 1, 1,			/* SFP, AP, CC */	\
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
    "sfp", "ap",  "cc",						\
  }

/* Generate the register aliases for core register N */
#define R_ALIASES(N) {"r" # N, R0_REGNUM + (N)}, \
                     {"w" # N, R0_REGNUM + (N)}

#define V_ALIASES(N) {"q" # N, V0_REGNUM + (N)}, \
                     {"d" # N, V0_REGNUM + (N)}, \
                     {"s" # N, V0_REGNUM + (N)}, \
                     {"h" # N, V0_REGNUM + (N)}, \
                     {"b" # N, V0_REGNUM + (N)}

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
    V_ALIASES(28), V_ALIASES(29), V_ALIASES(30), V_ALIASES(31)  \
  }

/* Say that the epilogue uses the return address register.  Note that
   in the case of sibcalls, the values "used by the epilogue" are
   considered live at the start of the called function.  */

#define EPILOGUE_USES(REGNO) \
  ((REGNO) == LR_REGNUM)

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.  */
#define EXIT_IGNORE_STACK	1

#define STATIC_CHAIN_REGNUM		R18_REGNUM
#define HARD_FRAME_POINTER_REGNUM	R29_REGNUM
#define FRAME_POINTER_REGNUM		SFP_REGNUM
#define STACK_POINTER_REGNUM		SP_REGNUM
#define ARG_POINTER_REGNUM		AP_REGNUM
#define FIRST_PSEUDO_REGISTER		67

/* The number of (integer) argument register available.  */
#define NUM_ARG_REGS			8
#define NUM_FP_ARG_REGS			8

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


#define DBX_REGISTER_NUMBER(REGNO)	aarch64_dbx_register_number (REGNO)
/* Provide a definition of DWARF_FRAME_REGNUM here so that fallback unwinders
   can use DWARF_ALT_FRAME_RETURN_COLUMN defined below.  This is just the same
   as the default definition in dwarf2out.c.  */
#undef DWARF_FRAME_REGNUM
#define DWARF_FRAME_REGNUM(REGNO)	DBX_REGISTER_NUMBER (REGNO)

#define DWARF_FRAME_RETURN_COLUMN	DWARF_FRAME_REGNUM (LR_REGNUM)

#define HARD_REGNO_NREGS(REGNO, MODE)	aarch64_hard_regno_nregs (REGNO, MODE)

#define HARD_REGNO_MODE_OK(REGNO, MODE)	aarch64_hard_regno_mode_ok (REGNO, MODE)

#define MODES_TIEABLE_P(MODE1, MODE2)			\
  (GET_MODE_CLASS (MODE1) == GET_MODE_CLASS (MODE2))

#define DWARF2_UNWIND_INFO 1

/* Use R0 through R3 to pass exception handling information.  */
#define EH_RETURN_DATA_REGNO(N) \
  ((N) < 4 ? ((unsigned int) R0_REGNUM + (N)) : INVALID_REGNUM)

/* Select a format to encode pointers in exception handling data.  */
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE, GLOBAL) \
  aarch64_asm_preferred_eh_data_format ((CODE), (GLOBAL))

/* The register that holds the return address in exception handlers.  */
#define AARCH64_EH_STACKADJ_REGNUM	(R0_REGNUM + 4)
#define EH_RETURN_STACKADJ_RTX	gen_rtx_REG (Pmode, AARCH64_EH_STACKADJ_REGNUM)

/* Don't use __builtin_setjmp until we've defined it.  */
#undef DONT_USE_BUILTIN_SETJMP
#define DONT_USE_BUILTIN_SETJMP 1

/* Register in which the structure value is to be returned.  */
#define AARCH64_STRUCT_VALUE_REGNUM R8_REGNUM

/* Non-zero if REGNO is part of the Core register set.

   The rather unusual way of expressing this check is to avoid
   warnings when building the compiler when R0_REGNUM is 0 and REGNO
   is unsigned.  */
#define GP_REGNUM_P(REGNO)						\
  (((unsigned) (REGNO - R0_REGNUM)) <= (R30_REGNUM - R0_REGNUM))

#define FP_REGNUM_P(REGNO)			\
  (((unsigned) (REGNO - V0_REGNUM)) <= (V31_REGNUM - V0_REGNUM))

#define FP_LO_REGNUM_P(REGNO)            \
  (((unsigned) (REGNO - V0_REGNUM)) <= (V15_REGNUM - V0_REGNUM))


/* Register and constant classes.  */

enum reg_class
{
  NO_REGS,
  CORE_REGS,
  GENERAL_REGS,
  STACK_REG,
  POINTER_REGS,
  FP_LO_REGS,
  FP_REGS,
  ALL_REGS,
  LIM_REG_CLASSES		/* Last */
};

#define N_REG_CLASSES	((int) LIM_REG_CLASSES)

#define REG_CLASS_NAMES				\
{						\
  "NO_REGS",					\
  "CORE_REGS",					\
  "GENERAL_REGS",				\
  "STACK_REG",					\
  "POINTER_REGS",				\
  "FP_LO_REGS",					\
  "FP_REGS",					\
  "ALL_REGS"					\
}

#define REG_CLASS_CONTENTS						\
{									\
  { 0x00000000, 0x00000000, 0x00000000 },	/* NO_REGS */		\
  { 0x7fffffff, 0x00000000, 0x00000003 },	/* CORE_REGS */		\
  { 0x7fffffff, 0x00000000, 0x00000003 },	/* GENERAL_REGS */	\
  { 0x80000000, 0x00000000, 0x00000000 },	/* STACK_REG */		\
  { 0xffffffff, 0x00000000, 0x00000003 },	/* POINTER_REGS */	\
  { 0x00000000, 0x0000ffff, 0x00000000 },       /* FP_LO_REGS  */	\
  { 0x00000000, 0xffffffff, 0x00000000 },       /* FP_REGS  */		\
  { 0xffffffff, 0xffffffff, 0x00000007 }	/* ALL_REGS */		\
}

#define REGNO_REG_CLASS(REGNO)	aarch64_regno_regclass (REGNO)

#define INDEX_REG_CLASS	CORE_REGS
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
#define AARCH64_CORE(NAME, INTERNAL_IDENT, IDENT, ARCH, FLAGS, COSTS) \
  TARGET_CPU_##INTERNAL_IDENT,
#include "aarch64-cores.def"
#undef AARCH64_CORE
  TARGET_CPU_generic
};

/* If there is no CPU defined at configure, use generic as default.  */
#ifndef TARGET_CPU_DEFAULT
#define TARGET_CPU_DEFAULT \
  (TARGET_CPU_generic | (AARCH64_CPU_DEFAULT_FLAGS << 6))
#endif

/* The processor for which instructions should be scheduled.  */
extern enum aarch64_processor aarch64_tune;

/* RTL generation support.  */
#define INIT_EXPANDERS aarch64_init_expanders ()


/* Stack layout; function entry, exit and calling.  */
#define STACK_GROWS_DOWNWARD	1

#define FRAME_GROWS_DOWNWARD	1

#define STARTING_FRAME_OFFSET	0

#define ACCUMULATE_OUTGOING_ARGS	1

#define FIRST_PARM_OFFSET(FNDECL) 0

/* Fix for VFP */
#define LIBCALL_VALUE(MODE)  \
  gen_rtx_REG (MODE, FLOAT_MODE_P (MODE) ? V0_REGNUM : R0_REGNUM)

#define DEFAULT_PCC_STRUCT_RETURN 0

#define AARCH64_ROUND_UP(X, ALIGNMENT) \
  (((X) + ((ALIGNMENT) - 1)) & ~((ALIGNMENT) - 1))

#define AARCH64_ROUND_DOWN(X, ALIGNMENT) \
  ((X) & ~((ALIGNMENT) - 1))

#ifdef HOST_WIDE_INT
struct GTY (()) aarch64_frame
{
  HOST_WIDE_INT reg_offset[FIRST_PSEUDO_REGISTER];
  HOST_WIDE_INT saved_regs_size;
  /* Padding if needed after the all the callee save registers have
     been saved.  */
  HOST_WIDE_INT padding0;
  HOST_WIDE_INT hardfp_offset;	/* HARD_FRAME_POINTER_REGNUM */

  bool laid_out;
};

typedef struct GTY (()) machine_function
{
  struct aarch64_frame frame;

  /* The number of extra stack bytes taken up by register varargs.
     This area is allocated by the callee at the very top of the frame.  */
  HOST_WIDE_INT saved_varargs_size;

} machine_function;
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
  ARM_PCS_UNKNOWN
};


extern enum arm_pcs arm_pcs_variant;

#ifndef ARM_DEFAULT_PCS
#define ARM_DEFAULT_PCS ARM_PCS_AAPCS64
#endif

/* We can't use enum machine_mode inside a generator file because it
   hasn't been created yet; we shouldn't be using any code that
   needs the real definition though, so this ought to be safe.  */
#ifdef GENERATOR_FILE
#define MACHMODE int
#else
#include "insn-modes.h"
#define MACHMODE enum machine_mode
#endif


/* AAPCS related state tracking.  */
typedef struct
{
  enum arm_pcs pcs_variant;
  int aapcs_arg_processed;	/* No need to lay out this argument again.  */
  int aapcs_ncrn;		/* Next Core register number.  */
  int aapcs_nextncrn;		/* Next next core register number.  */
  int aapcs_nvrn;		/* Next Vector register number.  */
  int aapcs_nextnvrn;		/* Next Next Vector register number.  */
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
} CUMULATIVE_ARGS;

#define FUNCTION_ARG_PADDING(MODE, TYPE) \
  (aarch64_pad_arg_upward (MODE, TYPE) ? upward : downward)

#define BLOCK_REG_PADDING(MODE, TYPE, FIRST) \
  (aarch64_pad_reg_upward (MODE, TYPE, FIRST) ? upward : downward)

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

/* Try a machine-dependent way of reloading an illegitimate address
   operand.  If we find one, push the reload and jump to WIN.  This
   macro is used in only one place: `find_reloads_address' in reload.c.  */

#define LEGITIMIZE_RELOAD_ADDRESS(X, MODE, OPNUM, TYPE, IND_L, WIN)	     \
do {									     \
  rtx new_x = aarch64_legitimize_reload_address (&(X), MODE, OPNUM, TYPE,    \
						 IND_L);		     \
  if (new_x)								     \
    {									     \
      X = new_x;							     \
      goto WIN;								     \
    }									     \
} while (0)

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

/* When optimizing for size, give a better estimate of the length of a memcpy
   call, but use the default otherwise.  But move_by_pieces_ninsns() counts
   memory-to-memory moves, and we'll have to generate a load & store for each,
   so halve the value to take that into account.  */
#define MOVE_RATIO(speed) \
  (((speed) ? 15 : AARCH64_CALL_RATIO) / 2)

/* For CLEAR_RATIO, when optimizing for size, give a better estimate
   of the length of a memset call, but use the default otherwise.  */
#define CLEAR_RATIO(speed) \
  ((speed) ? 15 : AARCH64_CALL_RATIO)

/* SET_RATIO is similar to CLEAR_RATIO, but for a non-zero constant, so when
   optimizing for size adjust the ratio to account for the overhead of loading
   the constant.  */
#define SET_RATIO(speed) \
  ((speed) ? 15 : AARCH64_CALL_RATIO - 2)

/* STORE_BY_PIECES_P can be used when copying a constant string, but
   in that case each 64-bit chunk takes 5 insns instead of 2 (LDR/STR).
   For now we always fail this and let the move_by_pieces code copy
   the string from read-only memory.  */
#define STORE_BY_PIECES_P(SIZE, ALIGN) 0

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

/* ?? #define WORD_REGISTER_OPERATIONS  */

/* Define if loading from memory in MODE, an integral mode narrower than
   BITS_PER_WORD will either zero-extend or sign-extend.  The value of this
   macro should be the code that says which one of the two operations is
   implicitly done, or UNKNOWN if none.  */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* Define this macro to be non-zero if instructions will fail to work
   if given data not on the nominal alignment.  */
#define STRICT_ALIGNMENT		TARGET_STRICT_ALIGN

/* Define this macro to be non-zero if accessing less than a word of
   memory is no faster than accessing a word of memory, i.e., if such
   accesses require more than one instruction or if there is no
   difference in cost.
   Although there's no difference in instruction count or cycles,
   in AArch64 we don't want to expand to a sub-word to a 64-bit access
   if we don't have to, for power-saving reasons.  */
#define SLOW_BYTE_ACCESS		0

#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

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

#define REVERSIBLE_CC_MODE(MODE) 1

#define REVERSE_CONDITION(CODE, MODE)		\
  (((MODE) == CCFPmode || (MODE) == CCFPEmode)	\
   ? reverse_condition_maybe_unordered (CODE)	\
   : reverse_condition (CODE))

#define CLZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE) \
  ((VALUE) = GET_MODE_UNIT_BITSIZE (MODE))
#define CTZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE) \
  ((VALUE) = ((MODE) == SImode ? 32 : 64), 2)

#define INCOMING_RETURN_ADDR_RTX gen_rtx_REG (Pmode, LR_REGNUM)

#define RETURN_ADDR_RTX aarch64_return_addr

/* 3 insns + padding + 2 pointer-sized entries.  */
#define TRAMPOLINE_SIZE	(TARGET_ILP32 ? 24 : 32)

/* Trampolines contain dwords, so must be dword aligned.  */
#define TRAMPOLINE_ALIGNMENT 64

/* Put trampolines in the text section so that mapping symbols work
   correctly.  */
#define TRAMPOLINE_SECTION text_section

/* To start with.  */
#define BRANCH_COST(SPEED_P, PREDICTABLE_P) 2


/* Assembly output.  */

/* For now we'll make all jump tables pc-relative.  */
#define CASE_VECTOR_PC_RELATIVE	1

#define CASE_VECTOR_SHORTEN_MODE(min, max, body)	\
  ((min < -0x1fff0 || max > 0x1fff0) ? SImode		\
   : (min < -0x1f0 || max > 0x1f0) ? HImode		\
   : QImode)

/* Jump table alignment is explicit in ASM_OUTPUT_CASE_LABEL.  */
#define ADDR_VEC_ALIGN(JUMPTABLE) 0

#define PRINT_OPERAND(STREAM, X, CODE) aarch64_print_operand (STREAM, X, CODE)

#define PRINT_OPERAND_ADDRESS(STREAM, X) \
  aarch64_print_operand_address (STREAM, X)

#define MCOUNT_NAME "_mcount"

#define NO_PROFILE_COUNTERS 1

/* Emit rtl for profiling.  Output assembler code to FILE
   to call "_mcount" for profiling a function entry.  */
#define PROFILE_HOOK(LABEL)						\
  {									\
    rtx fun, lr;							\
    lr = get_hard_reg_initial_val (Pmode, LR_REGNUM);			\
    fun = gen_rtx_SYMBOL_REF (Pmode, MCOUNT_NAME);			\
    emit_library_call (fun, LCT_NORMAL, VOIDmode, 1, lr, Pmode);	\
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

#define CANNOT_CHANGE_MODE_CLASS(FROM, TO, CLASS)	\
  aarch64_cannot_change_mode_class (FROM, TO, CLASS)

#define SHIFT_COUNT_TRUNCATED !TARGET_SIMD

/* Callee only saves lower 64-bits of a 128-bit register.  Tell the
   compiler the callee clobbers the top 64-bits when restoring the
   bottom 64-bits.  */
#define HARD_REGNO_CALL_PART_CLOBBERED(REGNO, MODE) \
		(FP_REGNUM_P (REGNO) && GET_MODE_SIZE (MODE) > 8)

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

/* Modes valid for AdvSIMD Q registers.  */
#define AARCH64_VALID_SIMD_QREG_MODE(MODE) \
  ((MODE) == V4SImode || (MODE) == V8HImode || (MODE) == V16QImode \
   || (MODE) == V4SFmode || (MODE) == V2DImode || mode == V2DFmode)

#define ENDIAN_LANE_N(mode, n)  \
  (BYTES_BIG_ENDIAN ? GET_MODE_NUNITS (mode) - 1 - n : n)

#define BIG_LITTLE_SPEC \
   " %{mcpu=*:-mcpu=%:rewrite_mcpu(%{mcpu=*:%*})}"

extern const char *aarch64_rewrite_mcpu (int argc, const char **argv);
#define BIG_LITTLE_CPU_SPEC_FUNCTIONS \
  { "rewrite_mcpu", aarch64_rewrite_mcpu },

#define ASM_CPU_SPEC \
   BIG_LITTLE_SPEC

#define EXTRA_SPEC_FUNCTIONS BIG_LITTLE_CPU_SPEC_FUNCTIONS

#define EXTRA_SPECS						\
  { "asm_cpu_spec",		ASM_CPU_SPEC }

#endif /* GCC_AARCH64_H */
