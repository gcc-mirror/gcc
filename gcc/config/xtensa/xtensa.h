/* Definitions of Tensilica's Xtensa target machine for GNU compiler.
   Copyright 2001,2002,2003 Free Software Foundation, Inc.
   Contributed by Bob Wilson (bwilson@tensilica.com) at Tensilica.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* Get Xtensa configuration settings */
#include "xtensa/xtensa-config.h"

/* Standard GCC variables that we reference.  */
extern int current_function_calls_alloca;
extern int target_flags;
extern int optimize;

/* External variables defined in xtensa.c.  */

/* comparison type */
enum cmp_type {
  CMP_SI,				/* four byte integers */
  CMP_DI,				/* eight byte integers */
  CMP_SF,				/* single precision floats */
  CMP_DF,				/* double precision floats */
  CMP_MAX				/* max comparison type */
};

extern struct rtx_def * branch_cmp[2];	/* operands for compare */
extern enum cmp_type branch_type;	/* what type of branch to use */
extern unsigned xtensa_current_frame_size;

/* Run-time compilation parameters selecting different hardware subsets.  */

#define MASK_BIG_ENDIAN		0x00000001	/* big or little endian */
#define MASK_DENSITY		0x00000002	/* code density option */
#define MASK_MAC16		0x00000004	/* MAC16 option */
#define MASK_MUL16		0x00000008	/* 16-bit integer multiply */
#define MASK_MUL32		0x00000010	/* integer multiply/divide */
#define MASK_DIV32		0x00000020	/* integer multiply/divide */
#define MASK_NSA		0x00000040	/* nsa instruction option */
#define MASK_MINMAX		0x00000080	/* min/max instructions */
#define MASK_SEXT		0x00000100	/* sign extend insn option */
#define MASK_BOOLEANS		0x00000200	/* boolean register option */
#define MASK_HARD_FLOAT		0x00000400	/* floating-point option */
#define MASK_HARD_FLOAT_DIV	0x00000800	/* floating-point divide */
#define MASK_HARD_FLOAT_RECIP	0x00001000	/* floating-point reciprocal */
#define MASK_HARD_FLOAT_SQRT	0x00002000	/* floating-point sqrt */
#define MASK_HARD_FLOAT_RSQRT	0x00004000	/* floating-point recip sqrt */
#define MASK_NO_FUSED_MADD	0x00008000	/* avoid f-p mul/add */
#define MASK_SERIALIZE_VOLATILE 0x00010000	/* serialize volatile refs */

/* Macros used in the machine description to test the flags.  */

#define TARGET_BIG_ENDIAN	(target_flags & MASK_BIG_ENDIAN)
#define TARGET_DENSITY		(target_flags & MASK_DENSITY)
#define TARGET_MAC16		(target_flags & MASK_MAC16)
#define TARGET_MUL16		(target_flags & MASK_MUL16)
#define TARGET_MUL32		(target_flags & MASK_MUL32)
#define TARGET_DIV32		(target_flags & MASK_DIV32)
#define TARGET_NSA		(target_flags & MASK_NSA)
#define TARGET_MINMAX		(target_flags & MASK_MINMAX)
#define TARGET_SEXT		(target_flags & MASK_SEXT)
#define TARGET_BOOLEANS		(target_flags & MASK_BOOLEANS)
#define TARGET_HARD_FLOAT	(target_flags & MASK_HARD_FLOAT)
#define TARGET_HARD_FLOAT_DIV	(target_flags & MASK_HARD_FLOAT_DIV)
#define TARGET_HARD_FLOAT_RECIP	(target_flags & MASK_HARD_FLOAT_RECIP)
#define TARGET_HARD_FLOAT_SQRT	(target_flags & MASK_HARD_FLOAT_SQRT)
#define TARGET_HARD_FLOAT_RSQRT	(target_flags & MASK_HARD_FLOAT_RSQRT)
#define TARGET_NO_FUSED_MADD	(target_flags & MASK_NO_FUSED_MADD)
#define TARGET_SERIALIZE_VOLATILE (target_flags & MASK_SERIALIZE_VOLATILE)

/* Default target_flags if no switches are specified  */

#define TARGET_DEFAULT (						\
  (XCHAL_HAVE_BE	? MASK_BIG_ENDIAN : 0) |			\
  (XCHAL_HAVE_DENSITY	? MASK_DENSITY : 0) |				\
  (XCHAL_HAVE_MAC16	? MASK_MAC16 : 0) |				\
  (XCHAL_HAVE_MUL16	? MASK_MUL16 : 0) |				\
  (XCHAL_HAVE_MUL32	? MASK_MUL32 : 0) |				\
  (XCHAL_HAVE_DIV32	? MASK_DIV32 : 0) |				\
  (XCHAL_HAVE_NSA	? MASK_NSA : 0) |				\
  (XCHAL_HAVE_MINMAX	? MASK_MINMAX : 0) |				\
  (XCHAL_HAVE_SEXT	? MASK_SEXT : 0) |				\
  (XCHAL_HAVE_BOOLEANS	? MASK_BOOLEANS : 0) |				\
  (XCHAL_HAVE_FP	? MASK_HARD_FLOAT : 0) |			\
  (XCHAL_HAVE_FP_DIV	? MASK_HARD_FLOAT_DIV : 0) |			\
  (XCHAL_HAVE_FP_RECIP	? MASK_HARD_FLOAT_RECIP : 0) |			\
  (XCHAL_HAVE_FP_SQRT	? MASK_HARD_FLOAT_SQRT : 0) |			\
  (XCHAL_HAVE_FP_RSQRT	? MASK_HARD_FLOAT_RSQRT : 0) |			\
  MASK_SERIALIZE_VOLATILE)

/* Macro to define tables used to set the flags.  */

#define TARGET_SWITCHES							\
{									\
  {"big-endian",		MASK_BIG_ENDIAN,			\
    N_("Use big-endian byte order")},					\
  {"little-endian",		-MASK_BIG_ENDIAN,			\
    N_("Use little-endian byte order")},				\
  {"density",			MASK_DENSITY,				\
    N_("Use the Xtensa code density option")},				\
  {"no-density",		-MASK_DENSITY,				\
    N_("Do not use the Xtensa code density option")},			\
  {"mac16",			MASK_MAC16,				\
    N_("Use the Xtensa MAC16 option")},					\
  {"no-mac16",			-MASK_MAC16,				\
    N_("Do not use the Xtensa MAC16 option")},				\
  {"mul16",			MASK_MUL16,				\
    N_("Use the Xtensa MUL16 option")},					\
  {"no-mul16",			-MASK_MUL16,				\
    N_("Do not use the Xtensa MUL16 option")},				\
  {"mul32",			MASK_MUL32,				\
    N_("Use the Xtensa MUL32 option")},					\
  {"no-mul32",			-MASK_MUL32,				\
    N_("Do not use the Xtensa MUL32 option")},				\
  {"div32",			MASK_DIV32,				\
    0 /* undocumented */},						\
  {"no-div32",			-MASK_DIV32,				\
    0 /* undocumented */},						\
  {"nsa",			MASK_NSA,				\
    N_("Use the Xtensa NSA option")},					\
  {"no-nsa",			-MASK_NSA,				\
    N_("Do not use the Xtensa NSA option")},				\
  {"minmax",			MASK_MINMAX,				\
    N_("Use the Xtensa MIN/MAX option")},				\
  {"no-minmax",			-MASK_MINMAX,				\
    N_("Do not use the Xtensa MIN/MAX option")},			\
  {"sext",			MASK_SEXT,				\
    N_("Use the Xtensa SEXT option")},					\
  {"no-sext",			-MASK_SEXT,				\
    N_("Do not use the Xtensa SEXT option")},				\
  {"booleans",			MASK_BOOLEANS,				\
    N_("Use the Xtensa boolean register option")},			\
  {"no-booleans",		-MASK_BOOLEANS,				\
    N_("Do not use the Xtensa boolean register option")},		\
  {"hard-float",		MASK_HARD_FLOAT,			\
    N_("Use the Xtensa floating-point unit")},				\
  {"soft-float",		-MASK_HARD_FLOAT,			\
    N_("Do not use the Xtensa floating-point unit")},			\
  {"hard-float-div",		MASK_HARD_FLOAT_DIV,			\
    0 /* undocumented */},						\
  {"no-hard-float-div",		-MASK_HARD_FLOAT_DIV,			\
    0 /* undocumented */},						\
  {"hard-float-recip",		MASK_HARD_FLOAT_RECIP,			\
    0 /* undocumented */},						\
  {"no-hard-float-recip",	-MASK_HARD_FLOAT_RECIP,			\
    0 /* undocumented */},						\
  {"hard-float-sqrt",		MASK_HARD_FLOAT_SQRT,			\
    0 /* undocumented */},						\
  {"no-hard-float-sqrt",	-MASK_HARD_FLOAT_SQRT,			\
    0 /* undocumented */},						\
  {"hard-float-rsqrt",		MASK_HARD_FLOAT_RSQRT,			\
    0 /* undocumented */},						\
  {"no-hard-float-rsqrt",	-MASK_HARD_FLOAT_RSQRT,			\
    0 /* undocumented */},						\
  {"no-fused-madd",		MASK_NO_FUSED_MADD,			\
    N_("Disable fused multiply/add and multiply/subtract FP instructions")}, \
  {"fused-madd",		-MASK_NO_FUSED_MADD,			\
    N_("Enable fused multiply/add and multiply/subtract FP instructions")}, \
  {"serialize-volatile",	MASK_SERIALIZE_VOLATILE,		\
    N_("Serialize volatile memory references with MEMW instructions")},	\
  {"no-serialize-volatile",	-MASK_SERIALIZE_VOLATILE,		\
    N_("Do not serialize volatile memory references with MEMW instructions")},\
  {"text-section-literals",	0,					\
    N_("Intersperse literal pools with code in the text section")},	\
  {"no-text-section-literals",	0,					\
    N_("Put literal pools in a separate literal section")},		\
  {"target-align",		0,					\
    N_("Automatically align branch targets to reduce branch penalties")}, \
  {"no-target-align",		0,					\
    N_("Do not automatically align branch targets")},			\
  {"longcalls",			0,					\
    N_("Use indirect CALLXn instructions for large programs")},		\
  {"no-longcalls",		0,					\
    N_("Use direct CALLn instructions for fast calls")},		\
  {"",				TARGET_DEFAULT, 0}			\
}


#define OVERRIDE_OPTIONS override_options ()

/* Target CPU builtins.  */
#define TARGET_CPU_CPP_BUILTINS()					\
  do {									\
    builtin_assert ("cpu=xtensa");					\
    builtin_assert ("machine=xtensa");					\
    builtin_define ("__XTENSA__");					\
    builtin_define (TARGET_BIG_ENDIAN ? "__XTENSA_EB__" : "__XTENSA_EL__"); \
    if (!TARGET_HARD_FLOAT)						\
      builtin_define ("__XTENSA_SOFT_FLOAT__");				\
    if (flag_pic)							\
      {									\
        builtin_define ("__PIC__");					\
        builtin_define ("__pic__");					\
      }									\
  } while (0)

#define CPP_SPEC " %(subtarget_cpp_spec) "

#ifndef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC ""
#endif

#define EXTRA_SPECS							\
  { "subtarget_cpp_spec", SUBTARGET_CPP_SPEC },

/* Define this to set the endianness to use in libgcc2.c, which can
   not depend on target_flags.  */
#define LIBGCC2_WORDS_BIG_ENDIAN XCHAL_HAVE_BE

/* Show we can debug even without a frame pointer.  */
#define CAN_DEBUG_WITHOUT_FP


/* Target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */
#define BITS_BIG_ENDIAN (TARGET_BIG_ENDIAN != 0)

/* Define this if most significant byte of a word is the lowest numbered. */
#define BYTES_BIG_ENDIAN (TARGET_BIG_ENDIAN != 0)

/* Define this if most significant word of a multiword number is the lowest. */
#define WORDS_BIG_ENDIAN (TARGET_BIG_ENDIAN != 0)

#define MAX_BITS_PER_WORD 32

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 4
#define MIN_UNITS_PER_WORD 4

/* Width of a floating point register.  */
#define UNITS_PER_FPREG 4

/* Size in bits of various types on the target machine.  */
#define INT_TYPE_SIZE 32
#define SHORT_TYPE_SIZE 16
#define LONG_TYPE_SIZE 32
#define MAX_LONG_TYPE_SIZE 32
#define LONG_LONG_TYPE_SIZE 64
#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 64
#define LONG_DOUBLE_TYPE_SIZE 64

/* Allocation boundary (in *bits*) for storing pointers in memory.  */
#define POINTER_BOUNDARY 32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 32

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 32

/* Alignment of field after 'int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* There is no point aligning anything to a rounder boundary than this.  */
#define BIGGEST_ALIGNMENT 128

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* Promote integer modes smaller than a word to SImode.  Set UNSIGNEDP
   for QImode, because there is no 8-bit load from memory with sign
   extension.  Otherwise, leave UNSIGNEDP alone, since Xtensa has 16-bit
   loads both with and without sign extension.  */
#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)				\
  do {									\
    if (GET_MODE_CLASS (MODE) == MODE_INT				\
	&& GET_MODE_SIZE (MODE) < UNITS_PER_WORD)			\
      {									\
	if ((MODE) == QImode)						\
	  (UNSIGNEDP) = 1;						\
	(MODE) = SImode;						\
      }									\
  } while (0)

/* The promotion described by `PROMOTE_MODE' should also be done for
   outgoing function arguments.  */
#define PROMOTE_FUNCTION_ARGS

/* The promotion described by `PROMOTE_MODE' should also be done for
   the return value of functions.  Note: `FUNCTION_VALUE' must perform
   the same promotions done by `PROMOTE_MODE'.  */
#define PROMOTE_FUNCTION_RETURN

/* Imitate the way many other C compilers handle alignment of
   bitfields and the structures that contain them.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* Align string constants and constructors to at least a word boundary.
   The typical use of this macro is to increase alignment for string
   constants to be word aligned so that 'strcpy' calls that copy
   constants can be done inline.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)					\
  ((TREE_CODE (EXP) == STRING_CST || TREE_CODE (EXP) == CONSTRUCTOR)	\
   && (ALIGN) < BITS_PER_WORD						\
	? BITS_PER_WORD							\
	: (ALIGN))

/* Align arrays, unions and records to at least a word boundary.
   One use of this macro is to increase alignment of medium-size
   data to make it all fit in fewer cache lines.  Another is to
   cause character arrays to be word-aligned so that 'strcpy' calls
   that copy constants to character arrays can be done inline.  */
#undef DATA_ALIGNMENT
#define DATA_ALIGNMENT(TYPE, ALIGN)					\
  ((((ALIGN) < BITS_PER_WORD)						\
    && (TREE_CODE (TYPE) == ARRAY_TYPE					\
	|| TREE_CODE (TYPE) == UNION_TYPE				\
	|| TREE_CODE (TYPE) == RECORD_TYPE)) ? BITS_PER_WORD : (ALIGN))

/* An argument declared as 'char' or 'short' in a prototype should
   actually be passed as an 'int'.  */
#define PROMOTE_PROTOTYPES 1

/* Operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS

/* Xtensa loads are zero-extended by default.  */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   The fake frame pointer and argument pointer will never appear in
   the generated code, since they will always be eliminated and replaced
   by either the stack pointer or the hard frame pointer.

   0 - 15	AR[0] - AR[15]
   16		FRAME_POINTER (fake = initial sp)
   17		ARG_POINTER (fake = initial sp + framesize)
   18		BR[0] for floating-point CC
   19 - 34	FR[0] - FR[15]
   35		MAC16 accumulator */

#define FIRST_PSEUDO_REGISTER 36

/* Return the stabs register number to use for REGNO. */
#define DBX_REGISTER_NUMBER(REGNO) xtensa_dbx_register_number (REGNO)

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator. */
#define FIXED_REGISTERS							\
{									\
  1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  1, 1, 0,								\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  0,									\
}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */
#define CALL_USED_REGISTERS						\
{									\
  1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1,			\
  1, 1, 1,								\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,			\
  1,									\
}

/* For non-leaf procedures on Xtensa processors, the allocation order
   is as specified below by REG_ALLOC_ORDER.  For leaf procedures, we
   want to use the lowest numbered registers first to minimize
   register window overflows.  However, local-alloc is not smart
   enough to consider conflicts with incoming arguments.  If an
   incoming argument in a2 is live throughout the function and
   local-alloc decides to use a2, then the incoming argument must
   either be spilled or copied to another register.  To get around
   this, we define ORDER_REGS_FOR_LOCAL_ALLOC to redefine
   reg_alloc_order for leaf functions such that lowest numbered
   registers are used first with the exception that the incoming
   argument registers are not used until after other register choices
   have been exhausted.  */

#define REG_ALLOC_ORDER \
{  8,  9, 10, 11, 12, 13, 14, 15,  7,  6,  5,  4,  3,  2, \
  18, \
  19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, \
   0,  1, 16, 17, \
  35, \
}

#define ORDER_REGS_FOR_LOCAL_ALLOC order_regs_for_local_alloc ()

/* For Xtensa, the only point of this is to prevent GCC from otherwise
   giving preference to call-used registers.  To minimize window
   overflows for the AR registers, we want to give preference to the
   lower-numbered AR registers.  For other register files, which are
   not windowed, we still prefer call-used registers, if there are any. */
extern const char xtensa_leaf_regs[FIRST_PSEUDO_REGISTER];
#define LEAF_REGISTERS xtensa_leaf_regs

/* For Xtensa, no remapping is necessary, but this macro must be
   defined if LEAF_REGISTERS is defined. */
#define LEAF_REG_REMAP(REGNO) (REGNO)

/* this must be declared if LEAF_REGISTERS is set */
extern int leaf_function;

/* Internal macros to classify a register number. */

/* 16 address registers + fake registers */
#define GP_REG_FIRST 0
#define GP_REG_LAST  17
#define GP_REG_NUM   (GP_REG_LAST - GP_REG_FIRST + 1)

/* Coprocessor registers */
#define BR_REG_FIRST 18
#define BR_REG_LAST  18 
#define BR_REG_NUM   (BR_REG_LAST - BR_REG_FIRST + 1)

/* 16 floating-point registers */
#define FP_REG_FIRST 19
#define FP_REG_LAST  34
#define FP_REG_NUM   (FP_REG_LAST - FP_REG_FIRST + 1)

/* MAC16 accumulator */
#define ACC_REG_FIRST 35
#define ACC_REG_LAST 35
#define ACC_REG_NUM  (ACC_REG_LAST - ACC_REG_FIRST + 1)

#define GP_REG_P(REGNO) ((unsigned) ((REGNO) - GP_REG_FIRST) < GP_REG_NUM)
#define BR_REG_P(REGNO) ((unsigned) ((REGNO) - BR_REG_FIRST) < BR_REG_NUM)
#define FP_REG_P(REGNO) ((unsigned) ((REGNO) - FP_REG_FIRST) < FP_REG_NUM)
#define ACC_REG_P(REGNO) ((unsigned) ((REGNO) - ACC_REG_FIRST) < ACC_REG_NUM)

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.  */
#define HARD_REGNO_NREGS(REGNO, MODE)					\
  (FP_REG_P (REGNO) ?							\
	((GET_MODE_SIZE (MODE) + UNITS_PER_FPREG - 1) / UNITS_PER_FPREG) : \
	((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* Value is 1 if hard register REGNO can hold a value of machine-mode
   MODE. */
extern char xtensa_hard_regno_mode_ok[][FIRST_PSEUDO_REGISTER];

#define HARD_REGNO_MODE_OK(REGNO, MODE)					\
  xtensa_hard_regno_mode_ok[(int) (MODE)][(REGNO)]

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2)					\
  ((GET_MODE_CLASS (MODE1) == MODE_FLOAT ||				\
    GET_MODE_CLASS (MODE1) == MODE_COMPLEX_FLOAT)			\
   == (GET_MODE_CLASS (MODE2) == MODE_FLOAT ||				\
       GET_MODE_CLASS (MODE2) == MODE_COMPLEX_FLOAT))

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM (GP_REG_FIRST + 1)

/* Base register for access to local variables of the function.  */
#define HARD_FRAME_POINTER_REGNUM (GP_REG_FIRST + 7)

/* The register number of the frame pointer register, which is used to
   access automatic variables in the stack frame.  For Xtensa, this
   register never appears in the output.  It is always eliminated to
   either the stack pointer or the hard frame pointer. */
#define FRAME_POINTER_REGNUM (GP_REG_FIRST + 16)

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in 'reload', in reload1.c.  */
#define FRAME_POINTER_REQUIRED xtensa_frame_pointer_required ()

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM (GP_REG_FIRST + 17)

/* If the static chain is passed in memory, these macros provide rtx
   giving 'mem' expressions that denote where they are stored.
   'STATIC_CHAIN' and 'STATIC_CHAIN_INCOMING' give the locations as
   seen by the calling and called functions, respectively.  */

#define STATIC_CHAIN							\
  gen_rtx_MEM (Pmode, plus_constant (stack_pointer_rtx, -5 * UNITS_PER_WORD))

#define STATIC_CHAIN_INCOMING						\
  gen_rtx_MEM (Pmode, plus_constant (arg_pointer_rtx, -5 * UNITS_PER_WORD))

/* For now we don't try to use the full set of boolean registers.  Without
   software pipelining of FP operations, there's not much to gain and it's
   a real pain to get them reloaded.  */
#define FPCC_REGNUM (BR_REG_FIRST + 0)

/* Pass structure value address as an "invisible" first argument.  */
#define STRUCT_VALUE 0

/* It is as good or better to call a constant function address than to
   call an address kept in a register.  */
#define NO_FUNCTION_CSE 1

/* It is as good or better for a function to call itself with an
   explicit address than to call an address kept in a register.  */
#define NO_RECURSIVE_FUNCTION_CSE 1

/* Xtensa processors have "register windows".  GCC does not currently
   take advantage of the possibility for variable-sized windows; instead,
   we use a fixed window size of 8.  */

#define INCOMING_REGNO(OUT)						\
  ((GP_REG_P (OUT) &&							\
    ((unsigned) ((OUT) - GP_REG_FIRST) >= WINDOW_SIZE)) ?		\
   (OUT) - WINDOW_SIZE : (OUT))

#define OUTGOING_REGNO(IN)						\
  ((GP_REG_P (IN) &&							\
    ((unsigned) ((IN) - GP_REG_FIRST) < WINDOW_SIZE)) ?			\
   (IN) + WINDOW_SIZE : (IN))


/* Define the classes of registers for register constraints in the
   machine description.  */
enum reg_class
{
  NO_REGS,			/* no registers in set */
  BR_REGS,			/* coprocessor boolean registers */
  FP_REGS,			/* floating point registers */
  ACC_REG,			/* MAC16 accumulator */
  SP_REG,			/* sp register (aka a1) */
  RL_REGS,			/* preferred reload regs (not sp or fp) */
  GR_REGS,			/* integer registers except sp */
  AR_REGS,			/* all integer registers */
  ALL_REGS,			/* all registers */
  LIM_REG_CLASSES		/* max value + 1 */
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define GENERAL_REGS AR_REGS

/* An initializer containing the names of the register classes as C
   string constants.  These names are used in writing some of the
   debugging dumps.  */
#define REG_CLASS_NAMES							\
{									\
  "NO_REGS",								\
  "BR_REGS",								\
  "FP_REGS",								\
  "ACC_REG",								\
  "SP_REG",								\
  "RL_REGS",								\
  "GR_REGS",								\
  "AR_REGS",								\
  "ALL_REGS"								\
}

/* Contents of the register classes.  The Nth integer specifies the
   contents of class N.  The way the integer MASK is interpreted is
   that register R is in the class if 'MASK & (1 << R)' is 1.  */
#define REG_CLASS_CONTENTS \
{ \
  { 0x00000000, 0x00000000 }, /* no registers */ \
  { 0x00040000, 0x00000000 }, /* coprocessor boolean registers */ \
  { 0xfff80000, 0x00000007 }, /* floating-point registers */ \
  { 0x00000000, 0x00000008 }, /* MAC16 accumulator */ \
  { 0x00000002, 0x00000000 }, /* stack pointer register */ \
  { 0x0000ff7d, 0x00000000 }, /* preferred reload registers */ \
  { 0x0000fffd, 0x00000000 }, /* general-purpose registers */ \
  { 0x0003ffff, 0x00000000 }, /* integer registers */ \
  { 0xffffffff, 0x0000000f }  /* all registers */ \
}

/* A C expression whose value is a register class containing hard
   register REGNO.  In general there is more that one such class;
   choose a class which is "minimal", meaning that no smaller class
   also contains the register.  */
extern const enum reg_class xtensa_regno_to_class[FIRST_PSEUDO_REGISTER];

#define REGNO_REG_CLASS(REGNO) xtensa_regno_to_class[ (REGNO) ]

/* Use the Xtensa AR register file for base registers.
   No index registers.  */
#define BASE_REG_CLASS AR_REGS
#define INDEX_REG_CLASS NO_REGS

/* SMALL_REGISTER_CLASSES is required for Xtensa, because all of the
   16 AR registers may be explicitly used in the RTL, as either
   incoming or outgoing arguments. */
#define SMALL_REGISTER_CLASSES 1


/* REGISTER AND CONSTANT CLASSES */

/* Get reg_class from a letter such as appears in the machine
   description.

   Available letters: a-f,h,j-l,q,t-z,A-D,W,Y-Z

   DEFINED REGISTER CLASSES:

   'a'  general-purpose registers except sp
   'q'  sp (aka a1)
   'D'	general-purpose registers (only if density option enabled)
   'd'  general-purpose registers, including sp (only if density enabled)
   'A'	MAC16 accumulator (only if MAC16 option enabled)
   'B'	general-purpose registers (only if sext instruction enabled)
   'C'  general-purpose registers (only if mul16 option enabled)
   'b'	coprocessor boolean registers
   'f'	floating-point registers
*/

extern enum reg_class xtensa_char_to_class[256];

#define REG_CLASS_FROM_LETTER(C) xtensa_char_to_class[ (int) (C) ]

/* The letters I, J, K, L, M, N, O, and P in a register constraint
   string can be used to stand for particular ranges of immediate
   operands.  This macro defines what the ranges are.  C is the
   letter, and VALUE is a constant value.  Return 1 if VALUE is
   in the range specified by C.

   For Xtensa:

   I = 12-bit signed immediate for movi
   J = 8-bit signed immediate for addi
   K = 4-bit value in (b4const U {0})
   L = 4-bit value in b4constu
   M = 7-bit value in simm7
   N = 8-bit unsigned immediate shifted left by 8 bits for addmi
   O = 4-bit value in ai4const
   P = valid immediate mask value for extui */

#define CONST_OK_FOR_LETTER_P(VALUE, C)					\
  ((C) == 'I' ? (xtensa_simm12b (VALUE))				\
   : (C) == 'J' ? (xtensa_simm8 (VALUE))				\
   : (C) == 'K' ? (((VALUE) == 0) || xtensa_b4const (VALUE))		\
   : (C) == 'L' ? (xtensa_b4constu (VALUE))				\
   : (C) == 'M' ? (xtensa_simm7 (VALUE))				\
   : (C) == 'N' ? (xtensa_simm8x256 (VALUE))				\
   : (C) == 'O' ? (xtensa_ai4const (VALUE))				\
   : (C) == 'P' ? (xtensa_mask_immediate (VALUE))			\
   : FALSE)


/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.  */
#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C) (0)


/* Other letters can be defined in a machine-dependent fashion to
   stand for particular classes of registers or other arbitrary
   operand types.

   R = memory that can be accessed with a 4-bit unsigned offset
   S = memory where the second word can be addressed with a 4-bit offset
   T = memory in a constant pool (addressable with a pc-relative load)
   U = memory *NOT* in a constant pool

   The offset range should not be checked here (except to distinguish
   denser versions of the instructions for which more general versions
   are available).  Doing so leads to problems in reloading: an
   argptr-relative address may become invalid when the phony argptr is
   eliminated in favor of the stack pointer (the offset becomes too
   large to fit in the instruction's immediate field); a reload is
   generated to fix this but the RTL is not immediately updated; in
   the meantime, the constraints are checked and none match.  The
   solution seems to be to simply skip the offset check here.  The
   address will be checked anyway because of the code in
   GO_IF_LEGITIMATE_ADDRESS. */

#define EXTRA_CONSTRAINT(OP, CODE)					\
  ((GET_CODE (OP) != MEM) ?						\
       ((CODE) >= 'R' && (CODE) <= 'U'					\
	&& reload_in_progress && GET_CODE (OP) == REG			\
        && REGNO (OP) >= FIRST_PSEUDO_REGISTER)				\
   : ((CODE) == 'R') ? smalloffset_mem_p (OP)				\
   : ((CODE) == 'S') ? smalloffset_double_mem_p (OP)			\
   : ((CODE) == 'T') ? constantpool_mem_p (OP)				\
   : ((CODE) == 'U') ? !constantpool_mem_p (OP)				\
   : FALSE)

#define PREFERRED_RELOAD_CLASS(X, CLASS)				\
  xtensa_preferred_reload_class (X, CLASS, 0)

#define PREFERRED_OUTPUT_RELOAD_CLASS(X, CLASS)				\
  xtensa_preferred_reload_class (X, CLASS, 1)
  
#define SECONDARY_INPUT_RELOAD_CLASS(CLASS, MODE, X)			\
  xtensa_secondary_reload_class (CLASS, MODE, X, 0)

#define SECONDARY_OUTPUT_RELOAD_CLASS(CLASS, MODE, X)			\
  xtensa_secondary_reload_class (CLASS, MODE, X, 1)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
#define CLASS_UNITS(mode, size)						\
  ((GET_MODE_SIZE (mode) + (size) - 1) / (size))

#define CLASS_MAX_NREGS(CLASS, MODE)					\
  (CLASS_UNITS (MODE, UNITS_PER_WORD))


/* Stack layout; function entry, exit and calling.  */

#define STACK_GROWS_DOWNWARD

/* Offset within stack frame to start allocating local variables at.  */
#define STARTING_FRAME_OFFSET						\
  current_function_outgoing_args_size

/* The ARG_POINTER and FRAME_POINTER are not real Xtensa registers, so
   they are eliminated to either the stack pointer or hard frame pointer.  */
#define ELIMINABLE_REGS							\
{{ ARG_POINTER_REGNUM,		STACK_POINTER_REGNUM},			\
 { ARG_POINTER_REGNUM,		HARD_FRAME_POINTER_REGNUM},		\
 { FRAME_POINTER_REGNUM,	STACK_POINTER_REGNUM},			\
 { FRAME_POINTER_REGNUM,	HARD_FRAME_POINTER_REGNUM}}

#define CAN_ELIMINATE(FROM, TO) 1

/* Specify the initial difference between the specified pair of registers.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
  do {									\
    compute_frame_size (get_frame_size ());				\
    if ((FROM) == FRAME_POINTER_REGNUM)					\
      (OFFSET) = 0;							\
    else if ((FROM) == ARG_POINTER_REGNUM)				\
      (OFFSET) = xtensa_current_frame_size;				\
    else								\
      abort ();								\
  } while (0)

/* If defined, the maximum amount of space required for outgoing
   arguments will be computed and placed into the variable
   'current_function_outgoing_args_size'.  No space will be pushed
   onto the stack for each call; instead, the function prologue
   should increase the stack frame size by this amount.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* Offset from the argument pointer register to the first argument's
   address.  On some machines it may depend on the data type of the
   function.  If 'ARGS_GROW_DOWNWARD', this is the offset to the
   location above the first argument's address.  */
#define FIRST_PARM_OFFSET(FNDECL) 0

/* Align stack frames on 128 bits for Xtensa.  This is necessary for
   128-bit datatypes defined in TIE (e.g., for Vectra).  */
#define STACK_BOUNDARY 128

/* Functions do not pop arguments off the stack.  */
#define RETURN_POPS_ARGS(FUNDECL, FUNTYPE, SIZE) 0

/* Use a fixed register window size of 8.  */
#define WINDOW_SIZE 8

/* Symbolic macros for the registers used to return integer, floating
   point, and values of coprocessor and user-defined modes.  */
#define GP_RETURN (GP_REG_FIRST + 2 + WINDOW_SIZE)
#define GP_OUTGOING_RETURN (GP_REG_FIRST + 2)

/* Symbolic macros for the first/last argument registers.  */
#define GP_ARG_FIRST (GP_REG_FIRST + 2)
#define GP_ARG_LAST  (GP_REG_FIRST + 7)
#define GP_OUTGOING_ARG_FIRST (GP_REG_FIRST + 2 + WINDOW_SIZE)
#define GP_OUTGOING_ARG_LAST  (GP_REG_FIRST + 7 + WINDOW_SIZE)

#define MAX_ARGS_IN_REGISTERS 6

/* Don't worry about compatibility with PCC.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* For Xtensa, up to 4 words can be returned in registers.  (It would
   have been nice to allow up to 6 words in registers but GCC cannot
   support that.  The return value must be given one of the standard
   MODE_INT modes, and there is no 6 word mode.  Instead, if we try to
   return a 6 word structure, GCC selects the next biggest mode
   (OImode, 8 words) and then the register allocator fails because
   there is no 8-register group beginning with a10.)  */
#define RETURN_IN_MEMORY(TYPE)						\
  ((unsigned HOST_WIDE_INT) int_size_in_bytes (TYPE) > 4 * UNITS_PER_WORD)

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  Because we have defined
   PROMOTE_FUNCTION_RETURN, we have to perform the same promotions as
   PROMOTE_MODE. */
#define XTENSA_LIBCALL_VALUE(MODE, OUTGOINGP)				\
  gen_rtx_REG ((GET_MODE_CLASS (MODE) == MODE_INT			\
		&& GET_MODE_SIZE (MODE) < UNITS_PER_WORD)		\
	       ? SImode : (MODE),					\
	       OUTGOINGP ? GP_OUTGOING_RETURN : GP_RETURN)

#define LIBCALL_VALUE(MODE)						\
  XTENSA_LIBCALL_VALUE ((MODE), 0)

#define LIBCALL_OUTGOING_VALUE(MODE)			 		\
  XTENSA_LIBCALL_VALUE ((MODE), 1)

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */
#define XTENSA_FUNCTION_VALUE(VALTYPE, FUNC, OUTGOINGP)			\
  gen_rtx_REG ((INTEGRAL_TYPE_P (VALTYPE)				\
	        && TYPE_PRECISION (VALTYPE) < BITS_PER_WORD)		\
	       ? SImode: TYPE_MODE (VALTYPE),				\
	       OUTGOINGP ? GP_OUTGOING_RETURN : GP_RETURN)

#define FUNCTION_VALUE(VALTYPE, FUNC)					\
  XTENSA_FUNCTION_VALUE (VALTYPE, FUNC, 0)

#define FUNCTION_OUTGOING_VALUE(VALTYPE, FUNC)				\
  XTENSA_FUNCTION_VALUE (VALTYPE, FUNC, 1)

/* A C expression that is nonzero if REGNO is the number of a hard
   register in which the values of called function may come back.  A
   register whose use for returning values is limited to serving as
   the second of a pair (for a value of type 'double', say) need not
   be recognized by this macro.  If the machine has register windows,
   so that the caller and the called function use different registers
   for the return value, this macro should recognize only the caller's
   register numbers. */
#define FUNCTION_VALUE_REGNO_P(N)					\
  ((N) == GP_RETURN)

/* A C expression that is nonzero if REGNO is the number of a hard
   register in which function arguments are sometimes passed.  This
   does *not* include implicit arguments such as the static chain and
   the structure-value address.  On many machines, no registers can be
   used for this purpose since all function arguments are pushed on
   the stack. */
#define FUNCTION_ARG_REGNO_P(N)						\
  ((N) >= GP_OUTGOING_ARG_FIRST && (N) <= GP_OUTGOING_ARG_LAST)

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go. */
typedef struct xtensa_args {
    int arg_words;		/* # total words the arguments take */
} CUMULATIVE_ARGS;

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0. */
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT)		\
  init_cumulative_args (&CUM, FNTYPE, LIBNAME)

#define INIT_CUMULATIVE_INCOMING_ARGS(CUM, FNTYPE, LIBNAME)		\
  init_cumulative_args (&CUM, FNTYPE, LIBNAME)

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */
#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)			\
  function_arg_advance (&CUM, MODE, TYPE)

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
  function_arg (&CUM, MODE, TYPE, FALSE)

#define FUNCTION_INCOMING_ARG(CUM, MODE, TYPE, NAMED) \
  function_arg (&CUM, MODE, TYPE, TRUE)

/* Arguments are never passed partly in memory and partly in registers.  */
#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) (0)

/* Specify function argument alignment.  */
#define FUNCTION_ARG_BOUNDARY(MODE, TYPE)				\
  ((TYPE) != 0								\
   ? (TYPE_ALIGN (TYPE) <= PARM_BOUNDARY				\
      ? PARM_BOUNDARY							\
      : TYPE_ALIGN (TYPE))						\
   : (GET_MODE_ALIGNMENT (MODE) <= PARM_BOUNDARY			\
      ? PARM_BOUNDARY							\
      : GET_MODE_ALIGNMENT (MODE)))


/* Nonzero if we do not know how to pass TYPE solely in registers.
   We cannot do so in the following cases:

   - if the type has variable size
   - if the type is marked as addressable (it is required to be constructed
     into the stack)

   This differs from the default in that it does not check if the padding
   and mode of the type are such that a copy into a register would put it
   into the wrong part of the register. */

#define MUST_PASS_IN_STACK(MODE, TYPE)					\
  ((TYPE) != 0								\
   && (TREE_CODE (TYPE_SIZE (TYPE)) != INTEGER_CST			\
       || TREE_ADDRESSABLE (TYPE)))

/* Profiling Xtensa code is typically done with the built-in profiling
   feature of Tensilica's instruction set simulator, which does not
   require any compiler support.  Profiling code on a real (i.e.,
   non-simulated) Xtensa processor is currently only supported by
   GNU/Linux with glibc.  The glibc version of _mcount doesn't require
   counter variables.  The _mcount function needs the current PC and
   the current return address to identify an arc in the call graph.
   Pass the current return address as the first argument; the current
   PC is available as a0 in _mcount's register window.  Both of these
   values contain window size information in the two most significant
   bits; we assume that _mcount will mask off those bits.  The call to
   _mcount uses a window size of 8 to make sure that it doesn't clobber
   any incoming argument values. */

#define NO_PROFILE_COUNTERS

#define FUNCTION_PROFILER(FILE, LABELNO) \
  do {									\
    fprintf (FILE, "\t%s\ta10, a0\n", TARGET_DENSITY ? "mov.n" : "mov"); \
    if (flag_pic)							\
      {									\
	fprintf (FILE, "\tmovi\ta8, _mcount@PLT\n");			\
	fprintf (FILE, "\tcallx8\ta8\n");				\
      }									\
    else								\
      fprintf (FILE, "\tcall8\t_mcount\n");				\
  } while (0)

/* Stack pointer value doesn't matter at exit.  */
#define EXIT_IGNORE_STACK 1

/* A C statement to output, on the stream FILE, assembler code for a
   block of data that contains the constant parts of a trampoline. 
   This code should not include a label--the label is taken care of
   automatically.

   For Xtensa, the trampoline must perform an entry instruction with a
   minimal stack frame in order to get some free registers.  Once the
   actual call target is known, the proper stack frame size is extracted
   from the entry instruction at the target and the current frame is
   adjusted to match.  The trampoline then transfers control to the
   instruction following the entry at the target.  Note: this assumes
   that the target begins with an entry instruction. */

/* minimum frame = reg save area (4 words) plus static chain (1 word)
   and the total number of words must be a multiple of 128 bits */
#define MIN_FRAME_SIZE (8 * UNITS_PER_WORD)

#define TRAMPOLINE_TEMPLATE(STREAM)					\
  do {									\
    fprintf (STREAM, "\t.begin no-generics\n");				\
    fprintf (STREAM, "\tentry\tsp, %d\n", MIN_FRAME_SIZE);		\
									\
    /* GCC isn't prepared to deal with data at the beginning of the	\
       trampoline, and the Xtensa l32r instruction requires that the	\
       constant pool be located before the code.  We put the constant	\
       pool in the middle of the trampoline and jump around it. */ 	\
									\
    fprintf (STREAM, "\tj\t.Lskipconsts\n");				\
    fprintf (STREAM, "\t.align\t4\n");					\
    fprintf (STREAM, ".Lfnaddr:%s0\n", integer_asm_op (4, TRUE));	\
    fprintf (STREAM, ".Lchainval:%s0\n", integer_asm_op (4, TRUE));	\
    fprintf (STREAM, ".Lskipconsts:\n");				\
									\
    /* store the static chain */					\
    fprintf (STREAM, "\tl32r\ta8, .Lchainval\n");			\
    fprintf (STREAM, "\ts32i\ta8, sp, %d\n",				\
	     MIN_FRAME_SIZE - (5 * UNITS_PER_WORD));			\
									\
    /* set the proper stack pointer value */				\
    fprintf (STREAM, "\tl32r\ta8, .Lfnaddr\n");				\
    fprintf (STREAM, "\tl32i\ta9, a8, 0\n");				\
    fprintf (STREAM, "\textui\ta9, a9, %d, 12\n",			\
	     TARGET_BIG_ENDIAN ? 8 : 12);				\
    fprintf (STREAM, "\tslli\ta9, a9, 3\n");				\
    fprintf (STREAM, "\taddi\ta9, a9, %d\n", -MIN_FRAME_SIZE);		\
    fprintf (STREAM, "\tsub\ta9, sp, a9\n");				\
    fprintf (STREAM, "\tmovsp\tsp, a9\n");				\
									\
    /* jump to the instruction following the entry */			\
    fprintf (STREAM, "\taddi\ta8, a8, 3\n");				\
    fprintf (STREAM, "\tjx\ta8\n");					\
    fprintf (STREAM, "\t.end no-generics\n");				\
  } while (0)

/* Size in bytes of the trampoline, as an integer.  */
#define TRAMPOLINE_SIZE 49

/* Alignment required for trampolines, in bits.  */
#define TRAMPOLINE_ALIGNMENT (32)

/* A C statement to initialize the variable parts of a trampoline.  */
#define INITIALIZE_TRAMPOLINE(ADDR, FUNC, CHAIN)			\
  do {									\
    rtx addr = ADDR;							\
    emit_move_insn (gen_rtx_MEM (SImode, plus_constant (addr, 8)), FUNC); \
    emit_move_insn (gen_rtx_MEM (SImode, plus_constant (addr, 12)), CHAIN); \
    emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "__xtensa_sync_caches"), \
		       0, VOIDmode, 1, addr, Pmode);			\
  } while (0)

/* Define the `__builtin_va_list' type for the ABI.  */
#define BUILD_VA_LIST_TYPE(VALIST) \
  (VALIST) = xtensa_build_va_list ()

/* If defined, is a C expression that produces the machine-specific
   code for a call to '__builtin_saveregs'.  This code will be moved
   to the very beginning of the function, before any parameter access
   are made.  The return value of this function should be an RTX that
   contains the value to use as the return of '__builtin_saveregs'. */
#define EXPAND_BUILTIN_SAVEREGS \
  xtensa_builtin_saveregs

/* Implement `va_start' for varargs and stdarg.  */
#define EXPAND_BUILTIN_VA_START(valist, nextarg) \
  xtensa_va_start (valist, nextarg)

/* Implement `va_arg'.  */
#define EXPAND_BUILTIN_VA_ARG(valist, type) \
  xtensa_va_arg (valist, type)

/* If defined, a C expression that produces the machine-specific code
   to setup the stack so that arbitrary frames can be accessed.

   On Xtensa, a stack back-trace must always begin from the stack pointer,
   so that the register overflow save area can be located.  However, the
   stack-walking code in GCC always begins from the hard_frame_pointer
   register, not the stack pointer.  The frame pointer is usually equal
   to the stack pointer, but the __builtin_return_address and
   __builtin_frame_address functions will not work if count > 0 and
   they are called from a routine that uses alloca.  These functions
   are not guaranteed to work at all if count > 0 so maybe that is OK.

   A nicer solution would be to allow the architecture-specific files to
   specify whether to start from the stack pointer or frame pointer.  That
   would also allow us to skip the machine->accesses_prev_frame stuff that
   we currently need to ensure that there is a frame pointer when these
   builtin functions are used. */

#define SETUP_FRAME_ADDRESSES  xtensa_setup_frame_addresses

/* A C expression whose value is RTL representing the address in a
   stack frame where the pointer to the caller's frame is stored.
   Assume that FRAMEADDR is an RTL expression for the address of the
   stack frame itself.

   For Xtensa, there is no easy way to get the frame pointer if it is
   not equivalent to the stack pointer.  Moreover, the result of this
   macro is used for continuing to walk back up the stack, so it must
   return the stack pointer address.  Thus, there is some inconsistency
   here in that __builtin_frame_address will return the frame pointer
   when count == 0 and the stack pointer when count > 0. */

#define DYNAMIC_CHAIN_ADDRESS(frame)					\
  gen_rtx (PLUS, Pmode, frame,						\
	   gen_rtx_CONST_INT (VOIDmode, -3 * UNITS_PER_WORD))

/* Define this if the return address of a particular stack frame is
   accessed from the frame pointer of the previous stack frame. */
#define RETURN_ADDR_IN_PREVIOUS_FRAME

/* A C expression whose value is RTL representing the value of the
   return address for the frame COUNT steps up from the current
   frame, after the prologue.  */
#define RETURN_ADDR_RTX  xtensa_return_addr

/* Addressing modes, and classification of registers for them.  */

/* C expressions which are nonzero if register number NUM is suitable
   for use as a base or index register in operand addresses.  It may
   be either a suitable hard register or a pseudo register that has
   been allocated such a hard register. The difference between an
   index register and a base register is that the index register may
   be scaled. */

#define REGNO_OK_FOR_BASE_P(NUM) \
  (GP_REG_P (NUM) || GP_REG_P ((unsigned) reg_renumber[NUM]))

#define REGNO_OK_FOR_INDEX_P(NUM) 0

/* C expressions that are nonzero if X (assumed to be a `reg' RTX) is
   valid for use as a base or index register.  For hard registers, it
   should always accept those which the hardware permits and reject
   the others.  Whether the macro accepts or rejects pseudo registers
   must be controlled by `REG_OK_STRICT'.  This usually requires two
   variant definitions, of which `REG_OK_STRICT' controls the one
   actually used. The difference between an index register and a base
   register is that the index register may be scaled. */

#ifdef REG_OK_STRICT

#define REG_OK_FOR_INDEX_P(X) 0
#define REG_OK_FOR_BASE_P(X) \
  REGNO_OK_FOR_BASE_P (REGNO (X))

#else /* !REG_OK_STRICT */

#define REG_OK_FOR_INDEX_P(X) 0
#define REG_OK_FOR_BASE_P(X) \
  ((REGNO (X) >= FIRST_PSEUDO_REGISTER) || (GP_REG_P (REGNO (X))))

#endif /* !REG_OK_STRICT */

/* Maximum number of registers that can appear in a valid memory address.  */
#define MAX_REGS_PER_ADDRESS 1

/* Identify valid Xtensa addresses.  */
#define GO_IF_LEGITIMATE_ADDRESS(MODE, ADDR, LABEL)			\
  do {									\
    rtx xinsn = (ADDR);							\
									\
    /* allow constant pool addresses */					\
    if ((MODE) != BLKmode && GET_MODE_SIZE (MODE) >= UNITS_PER_WORD	\
	&& constantpool_address_p (xinsn))				\
      goto LABEL;							\
									\
    while (GET_CODE (xinsn) == SUBREG)					\
      xinsn = SUBREG_REG (xinsn);					\
									\
    /* allow base registers */						\
    if (GET_CODE (xinsn) == REG && REG_OK_FOR_BASE_P (xinsn))		\
      goto LABEL;							\
									\
    /* check for "register + offset" addressing */			\
    if (GET_CODE (xinsn) == PLUS)					\
      {									\
	rtx xplus0 = XEXP (xinsn, 0);					\
	rtx xplus1 = XEXP (xinsn, 1);					\
	enum rtx_code code0;						\
	enum rtx_code code1;						\
									\
	while (GET_CODE (xplus0) == SUBREG)				\
	  xplus0 = SUBREG_REG (xplus0);					\
	code0 = GET_CODE (xplus0);					\
									\
	while (GET_CODE (xplus1) == SUBREG)				\
	  xplus1 = SUBREG_REG (xplus1);					\
	code1 = GET_CODE (xplus1);					\
									\
	/* swap operands if necessary so the register is first */	\
	if (code0 != REG && code1 == REG)				\
	  {								\
	    xplus0 = XEXP (xinsn, 1);					\
	    xplus1 = XEXP (xinsn, 0);					\
	    code0 = GET_CODE (xplus0);					\
	    code1 = GET_CODE (xplus1);					\
	  }								\
									\
	if (code0 == REG && REG_OK_FOR_BASE_P (xplus0)			\
	    && code1 == CONST_INT					\
	    && xtensa_mem_offset (INTVAL (xplus1), (MODE)))		\
	  {								\
	    goto LABEL;							\
	  }								\
      }									\
  } while (0)

/* A C expression that is 1 if the RTX X is a constant which is a
   valid address.  This is defined to be the same as 'CONSTANT_P (X)',
   but rejecting CONST_DOUBLE.  */
#define CONSTANT_ADDRESS_P(X)						\
  ((GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
    || GET_CODE (X) == CONST_INT || GET_CODE (X) == HIGH		\
    || (GET_CODE (X) == CONST)))

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE. */
#define LEGITIMATE_CONSTANT_P(X) 1

/* A C expression that is nonzero if X is a legitimate immediate
   operand on the target machine when generating position independent
   code.  */
#define LEGITIMATE_PIC_OPERAND_P(X)					\
  ((GET_CODE (X) != SYMBOL_REF || SYMBOL_REF_FLAG (X))			\
   && GET_CODE (X) != LABEL_REF						\
   && GET_CODE (X) != CONST)

/* Tell GCC how to use ADDMI to generate addresses.  */
#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN)				\
  do {									\
    rtx xinsn = (X);							\
    if (GET_CODE (xinsn) == PLUS)					\
      { 								\
	rtx plus0 = XEXP (xinsn, 0);					\
	rtx plus1 = XEXP (xinsn, 1);					\
									\
	if (GET_CODE (plus0) != REG && GET_CODE (plus1) == REG)		\
	  {								\
	    plus0 = XEXP (xinsn, 1);					\
	    plus1 = XEXP (xinsn, 0);					\
	  }								\
									\
	if (GET_CODE (plus0) == REG					\
	    && GET_CODE (plus1) == CONST_INT				\
	    && !xtensa_mem_offset (INTVAL (plus1), MODE)		\
	    && !xtensa_simm8 (INTVAL (plus1))				\
	    && xtensa_mem_offset (INTVAL (plus1) & 0xff, MODE)		\
	    && xtensa_simm8x256 (INTVAL (plus1) & ~0xff))		\
	  {								\
	    rtx temp = gen_reg_rtx (Pmode);				\
	    emit_insn (gen_rtx (SET, Pmode, temp,			\
				gen_rtx (PLUS, Pmode, plus0,		\
					 GEN_INT (INTVAL (plus1) & ~0xff)))); \
	    (X) = gen_rtx (PLUS, Pmode, temp,				\
			   GEN_INT (INTVAL (plus1) & 0xff));		\
	    goto WIN;							\
	  }								\
      }									\
  } while (0)


/* Treat constant-pool references as "mode dependent" since they can
   only be accessed with SImode loads.  This works around a bug in the
   combiner where a constant pool reference is temporarily converted
   to an HImode load, which is then assumed to zero-extend based on
   our definition of LOAD_EXTEND_OP.  This is wrong because the high
   bits of a 16-bit value in the constant pool are now sign-extended
   by default.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR, LABEL)			\
  do {									\
    if (constantpool_address_p (ADDR))					\
      goto LABEL;							\
  } while (0)

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE (SImode)

/* Define this if the tablejump instruction expects the table
   to contain offsets from the address of the table.
   Do not define this if the table should contain absolute addresses.  */
/* #define CASE_VECTOR_PC_RELATIVE */

/* Define this as 1 if 'char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 0

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 4
#define MAX_MOVE_MAX 4

/* Prefer word-sized loads.  */
#define SLOW_BYTE_ACCESS 1

/* Xtensa doesn't have any instructions that set integer values based on the
   results of comparisons, but the simplification code in the combiner also
   uses this macro.  The value should be either 1 or -1 to enable some
   optimizations in the combiner; I'm not sure which is better for us.
   Since we've been using 1 for a while, it should probably stay that way for
   compatibility.  */
#define STORE_FLAG_VALUE 1

/* Shift instructions ignore all but the low-order few bits.  */
#define SHIFT_COUNT_TRUNCATED 1

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated. */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode SImode

/* A function address in a call instruction is a word address (for
   indexing purposes) so give the MEM rtx a words's mode.  */
#define FUNCTION_MODE SImode

/* A C expression that evaluates to true if it is ok to perform a
   sibling call to DECL.  */
/* TODO: fix this up to allow at least some sibcalls */
#define FUNCTION_OK_FOR_SIBCALL(DECL) 0

/* Xtensa constant costs.  */
#define CONST_COSTS(X, CODE, OUTER_CODE)				\
  case CONST_INT:							\
    switch (OUTER_CODE)							\
      {									\
      case SET:								\
	if (xtensa_simm12b (INTVAL (X))) return 4;			\
	break;								\
      case PLUS:							\
	if (xtensa_simm8 (INTVAL (X))) return 0;			\
	if (xtensa_simm8x256 (INTVAL (X))) return 0;			\
	break;								\
      case AND:								\
	if (xtensa_mask_immediate (INTVAL (X))) return 0;		\
	break;								\
      case COMPARE:							\
	if ((INTVAL (X) == 0) || xtensa_b4const (INTVAL (X))) return 0;	\
	break;								\
      case ASHIFT:							\
      case ASHIFTRT:							\
      case LSHIFTRT:							\
      case ROTATE:							\
      case ROTATERT:							\
        /* no way to tell if X is the 2nd operand so be conservative */	\
      default: break;							\
      }									\
    if (xtensa_simm12b (INTVAL (X))) return 5;				\
    return 6;								\
  case CONST:								\
  case LABEL_REF:							\
  case SYMBOL_REF:							\
    return 5;								\
  case CONST_DOUBLE:							\
    return 7;

/* Costs of various Xtensa operations.  */
#define RTX_COSTS(X, CODE, OUTER_CODE)					\
  case MEM:								\
    {									\
	int num_words =							\
	  (GET_MODE_SIZE (GET_MODE (X)) > UNITS_PER_WORD) ?  2 : 1;	\
	if (memory_address_p (GET_MODE (X), XEXP ((X), 0)))		\
	  return COSTS_N_INSNS (num_words);				\
									\
	return COSTS_N_INSNS (2*num_words);				\
    }									\
									\
  case FFS:								\
    return COSTS_N_INSNS (TARGET_NSA ? 5 : 50);				\
									\
  case NOT:								\
    return COSTS_N_INSNS ((GET_MODE (X) == DImode) ? 3 : 2);		\
									\
  case AND:								\
  case IOR:								\
  case XOR:								\
    if (GET_MODE (X) == DImode) return COSTS_N_INSNS (2);		\
    return COSTS_N_INSNS (1);						\
									\
  case ASHIFT:								\
  case ASHIFTRT:							\
  case LSHIFTRT:							\
    if (GET_MODE (X) == DImode) return COSTS_N_INSNS (50);		\
    return COSTS_N_INSNS (1);						\
									\
  case ABS:								\
    {									\
	enum machine_mode xmode = GET_MODE (X);				\
	if (xmode == SFmode)						\
	  return COSTS_N_INSNS (TARGET_HARD_FLOAT ? 1 : 50);		\
	if (xmode == DFmode)						\
	  return COSTS_N_INSNS (50);					\
	return COSTS_N_INSNS (4);					\
    }									\
									\
  case PLUS:								\
  case MINUS:								\
    {									\
	enum machine_mode xmode = GET_MODE (X);				\
	if (xmode == SFmode)						\
	  return COSTS_N_INSNS (TARGET_HARD_FLOAT ? 1 : 50);		\
	if (xmode == DFmode || xmode == DImode)				\
	  return COSTS_N_INSNS (50);					\
	return COSTS_N_INSNS (1);					\
    }									\
									\
  case NEG:								\
    return COSTS_N_INSNS ((GET_MODE (X) == DImode) ? 4 : 2);		\
									\
  case MULT:								\
    {									\
	enum machine_mode xmode = GET_MODE (X);				\
	if (xmode == SFmode)						\
	  return COSTS_N_INSNS (TARGET_HARD_FLOAT ? 4 : 50);		\
	if (xmode == DFmode || xmode == DImode)				\
	    return COSTS_N_INSNS (50);					\
	if (TARGET_MUL32)						\
	  return COSTS_N_INSNS (4);					\
	if (TARGET_MAC16)						\
	  return COSTS_N_INSNS (16);					\
	if (TARGET_MUL16)						\
	  return COSTS_N_INSNS (12);					\
	return COSTS_N_INSNS (50);					\
    }									\
									\
  case DIV:								\
  case MOD:								\
    {									\
	enum machine_mode xmode = GET_MODE (X);				\
	if (xmode == SFmode)						\
	  return COSTS_N_INSNS (TARGET_HARD_FLOAT_DIV ? 8 : 50);	\
	if (xmode == DFmode)						\
	  return COSTS_N_INSNS (50);					\
    }									\
    /* fall through */							\
									\
  case UDIV:								\
  case UMOD:								\
    {									\
	enum machine_mode xmode = GET_MODE (X);				\
	if (xmode == DImode)						\
	  return COSTS_N_INSNS (50);					\
	if (TARGET_DIV32)						\
	  return COSTS_N_INSNS (32);					\
	return COSTS_N_INSNS (50);					\
    }									\
									\
  case SQRT:								\
    if (GET_MODE (X) == SFmode)						\
      return COSTS_N_INSNS (TARGET_HARD_FLOAT_SQRT ? 8 : 50);		\
    return COSTS_N_INSNS (50);						\
									\
  case SMIN:								\
  case UMIN:								\
  case SMAX:								\
  case UMAX:								\
    return COSTS_N_INSNS (TARGET_MINMAX ? 1 : 50);			\
									\
  case SIGN_EXTRACT:							\
  case SIGN_EXTEND:							\
    return COSTS_N_INSNS (TARGET_SEXT ? 1 : 2);				\
									\
  case ZERO_EXTRACT:							\
  case ZERO_EXTEND:							\
    return COSTS_N_INSNS (1);


/* An expression giving the cost of an addressing mode that
   contains ADDRESS.  */
#define ADDRESS_COST(ADDR) 1

/* A C expression for the cost of moving data from a register in
   class FROM to one in class TO.  The classes are expressed using
   the enumeration values such as 'GENERAL_REGS'.  A value of 2 is
   the default; other values are interpreted relative to that.  */
#define REGISTER_MOVE_COST(MODE, FROM, TO)				\
  (((FROM) == (TO) && (FROM) != BR_REGS && (TO) != BR_REGS)		\
   ? 2									\
   : (reg_class_subset_p ((FROM), AR_REGS)				\
      && reg_class_subset_p ((TO), AR_REGS)				\
      ? 2								\
      : (reg_class_subset_p ((FROM), AR_REGS)				\
	 && (TO) == ACC_REG						\
	 ? 3								\
	 : ((FROM) == ACC_REG						\
	    && reg_class_subset_p ((TO), AR_REGS)			\
	    ? 3								\
	    : 10))))

#define MEMORY_MOVE_COST(MODE, CLASS, IN) 4

#define BRANCH_COST 3

/* Optionally define this if you have added predicates to
   'MACHINE.c'.  This macro is called within an initializer of an
   array of structures.  The first field in the structure is the
   name of a predicate and the second field is an array of rtl
   codes.  For each predicate, list all rtl codes that can be in
   expressions matched by the predicate.  The list should have a
   trailing comma.  */

#define PREDICATE_CODES							\
  {"add_operand",		{ REG, CONST_INT, SUBREG }},		\
  {"arith_operand",		{ REG, CONST_INT, SUBREG }},		\
  {"nonimmed_operand",		{ REG, SUBREG, MEM }},			\
  {"mem_operand",		{ MEM }},				\
  {"mask_operand",		{ REG, CONST_INT, SUBREG }},		\
  {"extui_fldsz_operand",	{ CONST_INT }},				\
  {"sext_fldsz_operand",	{ CONST_INT }},				\
  {"lsbitnum_operand",		{ CONST_INT }},				\
  {"fpmem_offset_operand",	{ CONST_INT }},				\
  {"sext_operand",		{ REG, SUBREG, MEM }},			\
  {"branch_operand",		{ REG, CONST_INT, SUBREG }},		\
  {"ubranch_operand",		{ REG, CONST_INT, SUBREG }},		\
  {"call_insn_operand",		{ CONST_INT, CONST, SYMBOL_REF, REG }},	\
  {"move_operand",		{ REG, SUBREG, MEM, CONST_INT, CONST_DOUBLE, \
				  CONST, SYMBOL_REF, LABEL_REF }},	\
  {"non_const_move_operand",	{ REG, SUBREG, MEM }},			\
  {"const_float_1_operand",	{ CONST_DOUBLE }},			\
  {"branch_operator",		{ EQ, NE, LT, GE }},			\
  {"ubranch_operator",		{ LTU, GEU }},				\
  {"boolean_operator",		{ EQ, NE }},

/* Control the assembler format that we output.  */

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above). */
#define REGISTER_NAMES							\
{									\
  "a0",   "sp",   "a2",   "a3",   "a4",   "a5",   "a6",   "a7",		\
  "a8",   "a9",   "a10",  "a11",  "a12",  "a13",  "a14",  "a15",	\
  "fp",   "argp", "b0",							\
  "f0",   "f1",   "f2",   "f3",   "f4",   "f5",   "f6",   "f7",		\
  "f8",   "f9",   "f10",  "f11",  "f12",  "f13",  "f14",  "f15",	\
  "acc"									\
}

/* If defined, a C initializer for an array of structures containing a
   name and a register number.  This macro defines additional names
   for hard registers, thus allowing the 'asm' option in declarations
   to refer to registers using alternate names. */
#define ADDITIONAL_REGISTER_NAMES					\
{									\
  { "a1",	 1 + GP_REG_FIRST }					\
}

#define PRINT_OPERAND(FILE, X, CODE) print_operand (FILE, X, CODE)
#define PRINT_OPERAND_ADDRESS(FILE, ADDR) print_operand_address (FILE, ADDR)

/* Recognize machine-specific patterns that may appear within
   constants.  Used for PIC-specific UNSPECs.  */
#define OUTPUT_ADDR_CONST_EXTRA(STREAM, X, FAIL)			\
  do {									\
    if (flag_pic && GET_CODE (X) == UNSPEC && XVECLEN ((X), 0) == 1)	\
      {									\
	switch (XINT ((X), 1))						\
	  {								\
	  case UNSPEC_PLT:						\
	    output_addr_const ((STREAM), XVECEXP ((X), 0, 0));		\
	    fputs ("@PLT", (STREAM));					\
	    break;							\
	  default:							\
	    goto FAIL;							\
	  }								\
	break;								\
      }									\
    else								\
      goto FAIL;							\
  } while (0)

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.global\t"

/* Declare an uninitialized external linkage data object.  */
#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss (FILE, DECL, NAME, SIZE, ALIGN)

/* This is how to output an element of a case-vector that is absolute.  */
#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE)				\
  fprintf (STREAM, "%s%sL%u\n", integer_asm_op (4, TRUE),		\
	   LOCAL_LABEL_PREFIX, VALUE)

/* This is how to output an element of a case-vector that is relative.
   This is used for pc-relative code. */
#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL)		\
  do {									\
    fprintf (STREAM, "%s%sL%u-%sL%u\n",	integer_asm_op (4, TRUE),	\
	     LOCAL_LABEL_PREFIX, (VALUE),				\
	     LOCAL_LABEL_PREFIX, (REL));				\
  } while (0)

/* This is how to output an assembler line that says to advance the
   location counter to a multiple of 2**LOG bytes.  */
#define ASM_OUTPUT_ALIGN(STREAM, LOG)					\
  do {									\
    if ((LOG) != 0)							\
      fprintf (STREAM, "\t.align\t%d\n", 1 << (LOG));			\
  } while (0)

/* Indicate that jump tables go in the text section.  This is
   necessary when compiling PIC code.  */
#define JUMP_TABLES_IN_TEXT_SECTION (flag_pic)


/* Define this macro for the rare case where the RTL needs some sort of
   machine-dependent fixup immediately before register allocation is done. 

   If the stack frame size is too big to fit in the immediate field of
   the ENTRY instruction, we need to store the frame size in the
   constant pool.  However, the code in xtensa_function_prologue runs too
   late to be able to add anything to the constant pool.  Since the
   final frame size isn't known until reload is complete, this seems
   like the best place to do it.

   There may also be some fixup required if there is an incoming argument
   in a7 and the function requires a frame pointer. */

#define MACHINE_DEPENDENT_REORG(INSN) xtensa_reorg (INSN)


/* Define the strings to put out for each section in the object file.  */
#define TEXT_SECTION_ASM_OP	"\t.text"
#define DATA_SECTION_ASM_OP	"\t.data"
#define BSS_SECTION_ASM_OP	"\t.section\t.bss"


/* Define output to appear before the constant pool.  If the function
   has been assigned to a specific ELF section, or if it goes into a
   unique section, set the name of that section to be the literal
   prefix. */
#define ASM_OUTPUT_POOL_PROLOGUE(FILE, FUNNAME, FUNDECL, SIZE)          \
  do {									\
    tree fnsection;							\
    resolve_unique_section ((FUNDECL), 0, flag_function_sections);	\
    fnsection = DECL_SECTION_NAME (FUNDECL);				\
    if (fnsection != NULL_TREE)						\
      {									\
	const char *fnsectname = TREE_STRING_POINTER (fnsection);	\
	fprintf (FILE, "\t.begin\tliteral_prefix %s\n",			\
		 strcmp (fnsectname, ".text") ? fnsectname : "");	\
      }									\
    if ((SIZE) > 0)							\
      {									\
	function_section (FUNDECL);  					\
	fprintf (FILE, "\t.literal_position\n");			\
      }									\
  } while (0)


/* Define code to write out the ".end literal_prefix" directive for a
   function in a special section.  This is appended to the standard ELF
   code for ASM_DECLARE_FUNCTION_SIZE.  */
#define XTENSA_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)			\
  if (DECL_SECTION_NAME (DECL) != NULL_TREE)				\
    fprintf (FILE, "\t.end\tliteral_prefix\n")

/* A C statement (with or without semicolon) to output a constant in
   the constant pool, if it needs special treatment.  */
#define ASM_OUTPUT_SPECIAL_POOL_ENTRY(FILE, X, MODE, ALIGN, LABELNO, JUMPTO) \
  do {									\
    xtensa_output_literal (FILE, X, MODE, LABELNO);			\
    goto JUMPTO;							\
  } while (0)

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */
#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)			\
  do {									\
    (OUTPUT) = (char *) alloca (strlen (NAME) + 10);			\
    sprintf ((OUTPUT), "%s.%u", (NAME), (LABELNO));			\
  } while (0)

/* How to start an assembler comment. */
#define ASM_COMMENT_START "#"

/* Exception handling TODO!! */
#define DWARF_UNWIND_INFO 0

