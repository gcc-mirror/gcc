/* Definitions of Tensilica's Xtensa target machine for GNU compiler.
   Copyright (C) 2001 Free Software Foundation, Inc.
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

#define MAX_REGFILE_NAME_LEN 16
extern char xtensa_reg_names[][MAX_REGFILE_NAME_LEN];	/* register names */
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

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

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

/* Sometimes certain combinations of command options do not make sense
   on a particular target machine.  You can define a macro
   'OVERRIDE_OPTIONS' to take account of this.  This macro, if
   defined, is executed once just after all the command options have
   been parsed. */

#define OVERRIDE_OPTIONS override_options ()

#if XCHAL_HAVE_BE
#define CPP_ENDIAN_SPEC "\
  %{mlittle-endian:-D__XTENSA_EL__} \
  %{!mlittle-endian:-D__XTENSA_EB__} "
#else /* !XCHAL_HAVE_BE */
#define CPP_ENDIAN_SPEC "\
  %{mbig-endian:-D__XTENSA_EB__} \
  %{!mbig-endian:-D__XTENSA_EL__} "
#endif /* !XCHAL_HAVE_BE */

#if XCHAL_HAVE_FP
#define CPP_FLOAT_SPEC "%{msoft-float:-D__XTENSA_SOFT_FLOAT__}"
#else
#define CPP_FLOAT_SPEC "%{!mhard-float:-D__XTENSA_SOFT_FLOAT__}"
#endif

#ifndef CPP_SPEC
#define CPP_SPEC CPP_ENDIAN_SPEC CPP_FLOAT_SPEC
#endif

/* Define this to set the endianness to use in libgcc2.c, which can
   not depend on target_flags.  */
#define LIBGCC2_WORDS_BIG_ENDIAN XCHAL_HAVE_BE

/* Show we can debug even without a frame pointer.  */
#define CAN_DEBUG_WITHOUT_FP


/* Target machine storage layout */

/* Define in order to support both big and little endian float formats
   in the same gcc binary.  */
#define REAL_ARITHMETIC

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.
*/
#define BITS_BIG_ENDIAN (TARGET_BIG_ENDIAN != 0)

/* Define this if most significant byte of a word is the lowest numbered. */
#define BYTES_BIG_ENDIAN (TARGET_BIG_ENDIAN != 0)

/* Define this if most significant word of a multiword number is the lowest. */
#define WORDS_BIG_ENDIAN (TARGET_BIG_ENDIAN != 0)

/* Number of bits in an addressable storage unit */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type 'int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD 32
#define MAX_BITS_PER_WORD 32

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 4
#define MIN_UNITS_PER_WORD 4

/* Width of a floating point register.  */
#define UNITS_PER_FPREG 4

/* A C expression for the size in bits of the type 'int' on the
   target machine.  If you don't define this, the default is one
   word.  */
#define INT_TYPE_SIZE 32
#define MAX_INT_TYPE_SIZE 32

/* Tell the preprocessor the maximum size of wchar_t.  */
#ifndef MAX_WCHAR_TYPE_SIZE
#ifndef WCHAR_TYPE_SIZE
#define MAX_WCHAR_TYPE_SIZE MAX_INT_TYPE_SIZE
#endif
#endif

/* A C expression for the size in bits of the type 'short' on the
   target machine.  If you don't define this, the default is half a
   word.  (If this would be less than one storage unit, it is
   rounded up to one unit.)  */
#define SHORT_TYPE_SIZE 16

/* A C expression for the size in bits of the type 'long' on the
   target machine.  If you don't define this, the default is one
   word.  */
#define LONG_TYPE_SIZE 32
#define MAX_LONG_TYPE_SIZE 32

/* A C expression for the size in bits of the type 'long long' on the
   target machine.  If you don't define this, the default is two
   words.  */
#define LONG_LONG_TYPE_SIZE 64

/* A C expression for the size in bits of the type 'char' on the
   target machine.  If you don't define this, the default is one
   quarter of a word.  (If this would be less than one storage unit,
   it is rounded up to one unit.)  */
#define CHAR_TYPE_SIZE BITS_PER_UNIT

/* A C expression for the size in bits of the type 'float' on the
   target machine.  If you don't define this, the default is one
   word.  */
#define FLOAT_TYPE_SIZE 32

/* A C expression for the size in bits of the type 'double' on the
   target machine.  If you don't define this, the default is two
   words.  */
#define DOUBLE_TYPE_SIZE 64

/* A C expression for the size in bits of the type 'long double' on
   the target machine.  If you don't define this, the default is two
   words.  */
#define LONG_DOUBLE_TYPE_SIZE 64

/* Width in bits of a pointer.
   See also the macro 'Pmode' defined below.  */
#define POINTER_SIZE 32

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

/* A macro to update M and UNSIGNEDP when an object whose type is
   TYPE and which has the specified mode and signedness is to be
   stored in a register.  This macro is only called when TYPE is a
   scalar type.

   On most RISC machines, which only have operations that operate on a
   full register, define this macro to set M to `word_mode' if M is an
   integer mode narrower than `BITS_PER_WORD'.  In most cases, only
   integer modes should be widened because wider-precision
   floating-point operations are usually more expensive than their
   narrower counterparts.

   For Xtensa, also set UNSIGNEDP for QImode, because there is no
   8-bit load from memory with sign extension.  Otherwise, leave
   UNSIGNEDP alone, since Xtensa has 16-bit loads both with and
   without sign extension. */

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

/* Define this macro if the promotion described by `PROMOTE_MODE'
   should also be done for outgoing function arguments. */

#define PROMOTE_FUNCTION_ARGS

/* Define this macro if the promotion described by `PROMOTE_MODE'
   should also be done for the return value of functions.

   If this macro is defined, `FUNCTION_VALUE' must perform the same
   promotions done by `PROMOTE_MODE'. */

#define PROMOTE_FUNCTION_RETURN

/* Define this if you wish to imitate the way many other C compilers
   handle alignment of bitfields and the structures that contain
   them.

   The behavior is that the type written for a bitfield ('int',
   'short', or other integer type) imposes an alignment for the
   entire structure, as if the structure really did contain an
   ordinary field of that type.  In addition, the bitfield is placed
   within the structure so that it would fit within such a field,
   not crossing a boundary for it.

   Thus, on most machines, a bitfield whose type is written as 'int'
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
   constants to be word aligned so that 'strcpy' calls that copy
   constants can be done inline.  */

#define CONSTANT_ALIGNMENT(EXP, ALIGN)					\
  ((TREE_CODE (EXP) == STRING_CST || TREE_CODE (EXP) == CONSTRUCTOR)	\
   && (ALIGN) < BITS_PER_WORD						\
	? BITS_PER_WORD							\
	: (ALIGN))

/* If defined, a C expression to compute the alignment for a static
   variable.  TYPE is the data type, and ALIGN is the alignment that
   the object would ordinarily have.  The value of this macro is used
   instead of that alignment to align the object.

   If this macro is not defined, then ALIGN is used.

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

/* Define this macro if an argument declared as 'char' or 'short' in a
   prototype should actually be passed as an 'int'.  In addition to
   avoiding errors in certain cases of mismatch, it also makes for
   better code on certain machines. */

#define PROMOTE_PROTOTYPES 1

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, NIL if none.  */
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
   18           LOOP_COUNT (loop count special register)
   18		BR[0] for floating-point CC
   19 - 34	FR[0] - FR[15]
   35		MAC16 accumulator */

#define FIRST_PSEUDO_REGISTER 36

/* Return the stabs register number to use for REGNO. */
#define DBX_REGISTER_NUMBER(REGNO) xtensa_dbx_register_number(REGNO)

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

/* If defined, an initializer for a vector of integers, containing the
   numbers of hard registers in the order in which GCC should prefer
   to use them (from most preferred to least).

   For non-leaf procedures on Xtensa processors, the allocation order
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
   have been exhausted. */

#define REG_ALLOC_ORDER \
{  8,  9, 10, 11, 12, 13, 14, 15,  7,  6,  5,  4,  3,  2, 19, \
  20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, \
   0,  1, 16, 17, \
  36, \
}

/* A C statement (sans semicolon) to choose the order in which to
   allocate hard registers for pseudo-registers local to a basic
   block. */

#define ORDER_REGS_FOR_LOCAL_ALLOC order_regs_for_local_alloc ()

/* Name of a char vector, indexed by hard register number, which
   contains 1 for a register that is allowable in a candidate for leaf
   function treatment.

   For Xtensa, the only point of this is to prevent GCC from otherwise
   giving preference to call-used registers.  To minimize window
   overflows for the AR registers, we want to give preference to the
   lower-numbered AR registers.  For other register files, which are
   not windowed, we still prefer call-used registers, if there are any. */

extern char xtensa_leaf_regs[];
#define LEAF_REGISTERS xtensa_leaf_regs

/* A C expression whose value is the register number to which REGNO
   should be renumbered, when a function is treated as a leaf
   function.

   For Xtensa, no remapping is necessary, but this macro must be
   defined if LEAF_REGISTERS is defined. */

#define LEAF_REG_REMAP(REGNO) (REGNO)

/* this must be declared if LEAF_REGISTERS is set */
extern int leaf_function;

/* Internal macros to classify a register number. */

/* 16 address registers + fake registers */
#define GP_REG_FIRST 0
#define GP_REG_LAST  17
#define GP_REG_NUM   (GP_REG_LAST - GP_REG_FIRST + 1)

/* Special registers */
#define SPEC_REG_FIRST 18
#define SPEC_REG_LAST  18
#define SPEC_REG_NUM   (SPEC_REG_LAST - SPEC_REG_FIRST + 1)

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
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers. */

#define HARD_REGNO_NREGS(REGNO, MODE)					\
  (FP_REG_P (REGNO) ?							\
	((GET_MODE_SIZE (MODE) + UNITS_PER_FPREG - 1) / UNITS_PER_FPREG) : \
	((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* Value is 1 if hard register REGNO can hold a value of machine-mode
   MODE. */

extern char xtensa_hard_regno_mode_ok[][FIRST_PSEUDO_REGISTER];

#define HARD_REGNO_MODE_OK(REGNO, MODE)					\
  xtensa_hard_regno_mode_ok[ (int)(MODE) ][ (REGNO) ]

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */

#define MODES_TIEABLE_P(MODE1, MODE2)					\
  ((GET_MODE_CLASS (MODE1) == MODE_FLOAT ||				\
    GET_MODE_CLASS (MODE1) == MODE_COMPLEX_FLOAT)			\
   == (GET_MODE_CLASS (MODE2) == MODE_FLOAT ||				\
       GET_MODE_CLASS (MODE2) == MODE_COMPLEX_FLOAT))

/* Register to use for LCOUNT special register.  */
#define COUNT_REGISTER_REGNUM (SPEC_REG_FIRST + 0)

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
   seen by the calling and called functions, respectively.  Often the
   former will be at an offset from the stack pointer and the latter
   at an offset from the frame pointer. */

#define STATIC_CHAIN							\
  gen_rtx_MEM (Pmode, plus_constant (stack_pointer_rtx, -5 * UNITS_PER_WORD))

#define STATIC_CHAIN_INCOMING						\
  gen_rtx_MEM (Pmode, plus_constant (arg_pointer_rtx, -5 * UNITS_PER_WORD))

/* For now we don't try to use the full set of boolean registers.  Without
   software pipelining of FP operations, there's not much to gain and it's
   a real pain to get them reloaded. */
#define FPCC_REGNUM (BR_REG_FIRST + 0)

/* If the structure value address is not passed in a register, define
   'STRUCT_VALUE' as an expression returning an RTX for the place
   where the address is passed.  If it returns 0, the address is
   passed as an "invisible" first argument.  */
#define STRUCT_VALUE 0

/* Define this macro if it is as good or better to call a constant
   function address than to call an address kept in a register.  */
#define NO_FUNCTION_CSE 1

/* Define this macro if it is as good or better for a function to
   call itself with an explicit address than to call an address
   kept in a register.  */
#define NO_RECURSIVE_FUNCTION_CSE 1

/* Define this macro if the target machine has "register windows".  This
   C expression returns the register number as seen by the called function
   corresponding to register number OUT as seen by the calling function.
   Return OUT if register number OUT is not an outbound register.  */

#define INCOMING_REGNO(OUT)						\
  ((GP_REG_P(OUT) &&							\
    ((unsigned) ((OUT) - GP_REG_FIRST) >= WINDOW_SIZE)) ?		\
   (OUT) - WINDOW_SIZE : (OUT))

/* Define this macro if the target machine has "register windows".  This
   C expression returns the register number as seen by the calling function
   corresponding to register number IN as seen by the called function.
   Return IN if register number IN is not an inbound register.  */

#define OUTGOING_REGNO(IN)						\
  ((GP_REG_P(IN) &&							\
    ((unsigned) ((IN) - GP_REG_FIRST) < WINDOW_SIZE)) ?			\
   (IN) + WINDOW_SIZE : (IN))


/* XTENSA-SPECIFIC FLAG: Define this to prevent reload from using outgoing
   argument registers for spills.  See the comment in reload1.c. */

#define DONT_USE_FUNCTION_ARGS_FOR_RELOADS


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
  BR_REGS,			/* coprocessor boolean registers */
  FP_REGS,			/* floating point registers */
  ACC_REG,			/* MAC16 accumulator */
  SP_REG,			/* sp register (aka a1) */
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
  "GR_REGS",								\
  "AR_REGS",								\
  "ALL_REGS"								\
}

/* An initializer containing the contents of the register classes,
   as integers which are bit masks.  The Nth integer specifies the
   contents of class N.  The way the integer MASK is interpreted is
   that register R is in the class if 'MASK & (1 << R)' is 1.

   When the machine has more than 32 registers, an integer does not
   suffice.  Then the integers are replaced by sub-initializers,
   braced groupings containing several integers.  Each
   sub-initializer must be suitable as an initializer for the type
   'HARD_REG_SET' which is defined in 'hard-reg-set.h'.  */

#define REG_CLASS_CONTENTS \
{ \
  { 0x00000000, 0x00000000 }, /* no registers */ \
  { 0x00040000, 0x00000000 }, /* coprocessor boolean registers */ \
  { 0xfff80000, 0x00000007 }, /* floating-point registers */ \
  { 0x00000000, 0x00000008 }, /* MAC16 accumulator */ \
  { 0x00000002, 0x00000000 }, /* stack pointer register */ \
  { 0x0000fffd, 0x00000000 }, /* general-purpose registers */ \
  { 0x0003ffff, 0x00000000 }, /* integer registers */ \
  { 0xffffffff, 0x0000000f }  /* all registers */ \
}

/* A C expression whose value is a register class containing hard
   register REGNO.  In general there is more that one such class;
   choose a class which is "minimal", meaning that no smaller class
   also contains the register.  */

extern enum reg_class xtensa_regno_to_class[];

#define REGNO_REG_CLASS(REGNO) xtensa_regno_to_class[ (REGNO) ]

/* A macro whose definition is the name of the class to which a
   valid base register must belong.  A base register is one used in
   an address which is the register value plus a displacement.  */

#define BASE_REG_CLASS AR_REGS

/* A macro whose definition is the name of the class to which a
   valid index register must belong.  An index register is one used
   in an address where its value is either multiplied by a scale
   factor or added to another register (as well as added to a
   displacement).  */

#define INDEX_REG_CLASS NO_REGS

/* Normally the compiler avoids choosing registers that have been
   explicitly mentioned in the rtl as spill registers (these
   registers are normally those used to pass parameters and return
   values).  However, some machines have so few registers of certain
   classes that there would not be enough registers to use as spill
   registers if this were done.

   Define 'SMALL_REGISTER_CLASSES' on these machines.  When it is
   defined, the compiler allows registers explicitly used in the rtl
   to be used as spill registers but avoids extending the lifetime of
   these registers.

   It is always safe to define this macro, but if you unnecessarily
   define it, you will reduce the amount of optimizations that can be
   performed in some cases.  If you do not define this macro when it
   is required, the compiler will run out of spill registers and
   print a fatal error message.  For most machines, you should not
   define this macro.

   For Xtensa, this macro is required because all of the 16 registers
   may be explicitly used in the RTL, as either incoming or outgoing
   arguments. */

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

extern enum reg_class xtensa_char_to_class[];

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
  ((C) == 'I' ? (xtensa_simm12b(VALUE))					\
   : (C) == 'J' ? (xtensa_simm8(VALUE))					\
   : (C) == 'K' ? (((VALUE) == 0) || xtensa_b4const(VALUE))		\
   : (C) == 'L' ? (xtensa_b4constu(VALUE))				\
   : (C) == 'M' ? (xtensa_simm7(VALUE))					\
   : (C) == 'N' ? (xtensa_simm8x256(VALUE))				\
   : (C) == 'O' ? (xtensa_ai4const(VALUE))				\
   : (C) == 'P' ? (xtensa_mask_immediate(VALUE))			\
   : FALSE)


/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C) (0)


/* Other letters can be defined in a machine-dependent fashion to
   stand for particular classes of registers or other arbitrary
   operand types.

   The machine description macro `REG_CLASS_FROM_LETTER' has first cut
   at the otherwise unused letters.  If it evaluates to `NO_REGS',
   then `EXTRA_CONSTRAINT' is evaluated.
 
   The machine description macro 'EXTRA_CONSTRAINT' is passed the
   operand as its first argument and the constraint letter as its
   second operand.

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
   : ((CODE) == 'R') ? smalloffset_mem_p(OP)				\
   : ((CODE) == 'S') ? smalloffset_double_mem_p(OP)			\
   : ((CODE) == 'T') ? constantpool_mem_p(OP)				\
   : ((CODE) == 'U') ? !constantpool_mem_p(OP)				\
   : FALSE)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */

#define PREFERRED_RELOAD_CLASS(X, CLASS)				\
  (CONSTANT_P (X)							\
   ? (GET_CODE(X) == CONST_DOUBLE) ? NO_REGS : (CLASS)			\
   : (CLASS))

#define PREFERRED_OUTPUT_RELOAD_CLASS(X, CLASS)				\
  (CLASS)
  
/* You should define these macros to indicate to the reload phase that
   it may need to allocate at least one register for a reload in
   addition to the register to contain the data.  Specifically, if
   copying X to a register CLASS in MODE requires an intermediate
   register, you should define 'SECONDARY_INPUT_RELOAD_CLASS' to
   return the largest register class all of whose registers can be
   used as intermediate registers or scratch registers.

   If copying a register CLASS in MODE to X requires an intermediate
   or scratch register, 'SECONDARY_OUTPUT_RELOAD_CLASS' should be
   defined to return the largest register class required.  If the
   requirements for input and output reloads are the same, the macro
   'SECONDARY_RELOAD_CLASS' should be used instead of defining both
   macros identically. */

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

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated. */

#define STARTING_FRAME_OFFSET						\
  current_function_outgoing_args_size

/* If defined, this macro specifies a table of register pairs used to
   eliminate unneeded registers that point into the stack frame.  If
   it is not defined, the only elimination attempted by the compiler
   is to replace references to the frame pointer with references to
   the stack pointer.

   The definition of this macro is a list of structure
   initializations, each of which specifies an original and
   replacement register.

   On some machines, the position of the argument pointer is not
   known until the compilation is completed.  In such a case, a
   separate hard register must be used for the argument pointer. 
   This register can be eliminated by replacing it with either the
   frame pointer or the argument pointer, depending on whether or not
   the frame pointer has been eliminated.

   In this case, you might specify:
        #define ELIMINABLE_REGS  \
        {{ARG_POINTER_REGNUM, STACK_POINTER_REGNUM}, \
         {ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM}, \
         {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}}

   Note that the elimination of the argument pointer with the stack
   pointer is specified first since that is the preferred elimination.  */

#define ELIMINABLE_REGS							\
{{ ARG_POINTER_REGNUM,		STACK_POINTER_REGNUM},			\
 { ARG_POINTER_REGNUM,		HARD_FRAME_POINTER_REGNUM},		\
 { FRAME_POINTER_REGNUM,	STACK_POINTER_REGNUM},			\
 { FRAME_POINTER_REGNUM,	HARD_FRAME_POINTER_REGNUM}}

/* A C expression that returns non-zero if the compiler is allowed to
   try to replace register number FROM-REG with register number
   TO-REG.  This macro need only be defined if 'ELIMINABLE_REGS' is
   defined, and will usually be the constant 1, since most of the
   cases preventing register elimination are things that the compiler
   already knows about.  */

#define CAN_ELIMINATE(FROM, TO) 1

/* This macro is similar to 'INITIAL_FRAME_POINTER_OFFSET'.  It
   specifies the initial difference between the specified pair of
   registers.  This macro must be defined if 'ELIMINABLE_REGS' is
   defined.  */

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
   should increase the stack frame size by this amount.

   It is not proper to define both 'PUSH_ROUNDING' and
   'ACCUMULATE_OUTGOING_ARGS'.  */

#define ACCUMULATE_OUTGOING_ARGS 1

/* Offset from the argument pointer register to the first argument's
   address.  On some machines it may depend on the data type of the
   function.  If 'ARGS_GROW_DOWNWARD', this is the offset to the
   location above the first argument's address. */

#define FIRST_PARM_OFFSET(FNDECL) 0

/* Define this macro if you wish to preserve a certain alignment for
   the stack pointer.  The definition is a C expression for the
   desired alignment (measured in bits).

   Align stack frames on 128 bits for Xtensa.  This is necessary for
   128-bit datatypes defined in TIE (e.g., for Vectra).  */

#define STACK_BOUNDARY 128

/* A C expression that should indicate the number of bytes of its
   own arguments that a function function pops on returning, or 0
   if the function pops no arguments and the caller must therefore
   pop them all after the function returns.

   FUNDECL is the declaration node of the function (as a tree).

   FUNTYPE is a C variable whose value is a tree node that
   describes the function in question.  Normally it is a node of
   type 'FUNCTION_TYPE' that describes the data type of the function.
   From this it is possible to obtain the data types of the value
   and arguments (if known).

   When a call to a library function is being considered, FUNTYPE
   will contain an identifier node for the library function.  Thus,
   if you need to distinguish among various library functions, you
   can do so by their names.  Note that "library function" in this
   context means a function used to perform arithmetic, whose name
   is known specially in the compiler and was not mentioned in the
   C code being compiled.

   STACK-SIZE is the number of bytes of arguments passed on the
   stack.  If a variable number of bytes is passed, it is zero, and
   argument popping will always be the responsibility of the
   calling function.  */

#define RETURN_POPS_ARGS(FUNDECL, FUNTYPE, SIZE) 0

/* if this is changed, the hardwired values for CALL insns in xtensa.md
   also need to be changed */
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

/* Define this macro to be 1 if all structure and union return values
   must be in memory.  Since this results in slower code, this should
   be defined only if needed for compatibility with other compilers or
   with an ABI.  If you define this macro to be 0, then the
   conventions used for structure and union return values are decided
   by the 'RETURN_IN_MEMORY' macro. */

#define DEFAULT_PCC_STRUCT_RETURN 0

/* A C expression which can inhibit the returning of certain function
   values in registers, based on the type of value.  A nonzero value
   says to return the function value in memory, just as large
   structures are always returned.  Here TYPE will be a C expression
   of type 'tree', representing the data type of the value.

   For Xtensa, we would like to be able to return up to 6 words in
   memory but GCC cannot support that.  The return value must be given
   one of the standard MODE_INT modes, and there is no 6 word mode.
   Instead, if we try to return a 6 word structure, GCC selects the
   next biggest mode (OImode, 8 words) and then the register allocator
   fails because there is no 8-register group beginning with a10.  So
   we have to fall back on the next largest size which is 4 words... */

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

/* A code distinguishing the floating point format of the target
   machine.  There are three defined values: IEEE_FLOAT_FORMAT,
   VAX_FLOAT_FORMAT, and UNKNOWN_FLOAT_FORMAT.  */

#define TARGET_FLOAT_FORMAT IEEE_FLOAT_FORMAT

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

/* Determine where to put an argument to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
  function_arg (&CUM, MODE, TYPE, FALSE)

#define FUNCTION_INCOMING_ARG(CUM, MODE, TYPE, NAMED) \
  function_arg (&CUM, MODE, TYPE, TRUE)

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero. */

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) (0)

/* If defined, a C expression that gives the alignment boundary, in
   bits, of an argument with the specified mode and type.  If it is
   not defined,  'PARM_BOUNDARY' is used for all arguments.  */

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

/* This macro generates the assembly code for function entry.
   FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.
   Refer to the array 'regs_ever_live' to determine which registers
   to save; 'regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This macro is responsible for
   knowing which registers should not be saved even if used.  */

#define FUNCTION_PROLOGUE(FILE, SIZE) xtensa_function_prologue (FILE, SIZE)

/* This macro generates the assembly code for function exit,
   on machines that need it.  If FUNCTION_EPILOGUE is not defined
   then individual return instructions are generated for each
   return statement.  Args are same as for FUNCTION_PROLOGUE.  */

#define FUNCTION_EPILOGUE(FILE, SIZE) xtensa_function_epilogue (FILE, SIZE)

/* Output assembler code to FILE to increment profiler label LABELNO
   for profiling a function entry.

   The mcount code in glibc doesn't seem to use this LABELNO stuff.
   Some ports (e.g., MIPS) don't even bother to pass the label
   address, and even those that do (e.g., i386) don't seem to use it.
   The information needed by mcount() is the current PC and the
   current return address, so that mcount can identify an arc in the
   call graph.  For Xtensa, we pass the current return address as
   the first argument to mcount, and the current PC is available as
   a0 in mcount's register window.  Both of these values contain
   window size information in the two most significant bits; we assume
   that the mcount code will mask off those bits.  The call to mcount
   uses a window size of 8 to make sure that mcount doesn't clobber
   any incoming argument values. */

#define FUNCTION_PROFILER(FILE, LABELNO)				\
  do {									\
    fprintf (FILE, "\taddi\t%s, %s, 0\t# save current return address\n", \
	     xtensa_reg_names[GP_REG_FIRST+10],				\
	     xtensa_reg_names[GP_REG_FIRST+0]);				\
    fprintf (FILE, "\tcall8\t_mcount\n");				\
  } while (0);


/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

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
    fprintf (STREAM, ".Lfnaddr:\n\t.word 0\n");				\
    fprintf (STREAM, ".Lchainval:\n\t.word 0\n");			\
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

/* A C expression for the size in bytes of the trampoline, as an
   integer.  */

#define TRAMPOLINE_SIZE 49

/* Alignment required for trampolines, in bits.  */

#define TRAMPOLINE_ALIGNMENT (32)

/* A C statement to initialize the variable parts of a trampoline. 
   ADDR is an RTX for the address of the trampoline; FNADDR is an
   RTX for the address of the nested function; CHAIN is an
   RTX for the static chain value that should be passed to the
   function when it is called. */

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
#define EXPAND_BUILTIN_VA_START(stdarg, valist, nextarg) \
  xtensa_va_start (stdarg, valist, nextarg)

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

#define SETUP_FRAME_ADDRESSES() \
  xtensa_setup_frame_addresses ()

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
   frame, after the prologue.  FRAMEADDR is the frame pointer of the
   COUNT frame, or the frame pointer of the COUNT - 1 frame if
   'RETURN_ADDR_IN_PREVIOUS_FRAME' is defined.

   The 2 most-significant bits of the return address on Xtensa hold
   the register window size.  To get the real return address, these bits
   must be masked off and replaced with the high bits from the current
   PC.  Since it is unclear how the __builtin_return_address function
   is used, the current code does not do this masking and simply returns
   the raw return address from the a0 register. */

#define RETURN_ADDR_RTX(count, frame)					\
  ((count) == -1							\
   ? gen_rtx_REG (Pmode, 0)						\
   : gen_rtx_MEM (Pmode, memory_address					\
		  (Pmode, plus_constant (frame, -4 * UNITS_PER_WORD))))

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
  REGNO_OK_FOR_BASE_P  (REGNO (X))

#else /* !REG_OK_STRICT */

#define REG_OK_FOR_INDEX_P(X) 0
#define REG_OK_FOR_BASE_P(X) \
  ((REGNO (X) >= FIRST_PSEUDO_REGISTER) || (GP_REG_P (REGNO (X))))

#endif /* !REG_OK_STRICT */

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 1

/* A C compound statement with a conditional 'goto LABEL;' executed
   if X (an RTX) is a legitimate memory address on the target
   machine for a memory operand of mode MODE.

   This macro must exist in two variants: a strict variant and a
   non-strict one.  The strict variant is used in the reload pass.  It
   must be defined so that any pseudo-register that has not been
   allocated a hard register is considered a memory reference.  In
   contexts where some kind of register is required, a pseudo-register
   with no hard register must be rejected.

   The non-strict variant is used in other passes.  It must be defined
   to accept all pseudo-registers in every context where some kind of
   register is required.

   Compiler source files that want to use the strict variant of this
   macro define the macro 'REG_OK_STRICT'.  You should use an '#ifdef
   REG_OK_STRICT' conditional to define the strict variant in that
   case and the non-strict variant otherwise.

   Normally, constant addresses which are the sum of a 'symbol_ref'
   and an integer are stored inside a 'const' RTX to mark them as
   constant.  Therefore, there is no need to recognize such sums
   specifically as legitimate addresses.  Normally you would simply
   recognize any 'const' as legitimate.

   Usually 'PRINT_OPERAND_ADDRESS' is not prepared to handle constant
   sums that are not marked with 'const'.  It assumes that a naked
   'plus' indicates indexing.  If so, then you *must* reject such
   naked constant sums as illegitimate addresses, so that none of them
   will be given to 'PRINT_OPERAND_ADDRESS'. */

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)				\
  do {									\
    rtx xinsn = (X);							\
									\
    /* allow constant pool addresses */					\
    if ((MODE) != BLKmode && GET_MODE_SIZE (MODE) >= UNITS_PER_WORD	\
	&& constantpool_address_p (xinsn))				\
      goto ADDR;							\
									\
    while (GET_CODE (xinsn) == SUBREG)					\
      xinsn = SUBREG_REG (xinsn);					\
									\
    /* allow base registers */						\
    if (GET_CODE (xinsn) == REG && REG_OK_FOR_BASE_P (xinsn))		\
      goto ADDR;							\
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
	    goto ADDR;							\
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

/* A C compound statement that attempts to replace X with a valid
   memory address for an operand of mode MODE.  WIN will be a C
   statement label elsewhere in the code; the macro definition may
   use

          GO_IF_LEGITIMATE_ADDRESS (MODE, X, WIN);

   to avoid further processing if the address has become legitimate.

   X will always be the result of a call to 'break_out_memory_refs',
   and OLDX will be the operand that was given to that function to
   produce X.

   The code generated by this macro should not alter the
   substructure of X.  If it transforms X into a more legitimate
   form, it should assign X (which will always be a C variable) a
   new value.

   It is not necessary for this macro to come up with a legitimate
   address.  The compiler has standard ways of doing so in all
   cases.  In fact, it is safe for this macro to do nothing.  But
   often a machine-dependent strategy can generate better code. */

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


/* A C statement or compound statement with a conditional 'goto
   LABEL;' executed if memory address X (an RTX) can have different
   meanings depending on the machine mode of the memory reference it
   is used for.

   Autoincrement and autodecrement addresses typically have
   mode-dependent effects because the amount of the increment or
   decrement is the size of the operand being addressed.  Some
   machines have other mode-dependent addresses.  Many RISC machines
   have no mode-dependent addresses.

   You may assume that ADDR is a valid address for the machine.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR, LABEL) {}

/* If we are referencing a function that is static, make the SYMBOL_REF
   special so that we can generate direct calls to it even with -fpic.  */

#define ENCODE_SECTION_INFO(DECL)					\
  do {									\
    if (TREE_CODE (DECL) == FUNCTION_DECL && ! TREE_PUBLIC (DECL))	\
      SYMBOL_REF_FLAG (XEXP (DECL_RTL (DECL), 0)) = 1;			\
  } while (0)

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE (SImode)

/* Define this if the tablejump instruction expects the table
   to contain offsets from the address of the table.
   Do not define this if the table should contain absolute addresses.  */
/* #define CASE_VECTOR_PC_RELATIVE */

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Define this as 1 if 'char' should by default be signed; else as 0.  */
#ifndef DEFAULT_SIGNED_CHAR
#define DEFAULT_SIGNED_CHAR 0
#endif

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 4
#define MAX_MOVE_MAX 4

/* Define this macro as a C expression which is nonzero if
   accessing less than a word of memory (i.e. a 'char' or a
   'short') is no faster than accessing a word of memory, i.e., if
   such access require more than one instruction or if there is no
   difference in cost between byte and (aligned) word loads.

   On RISC machines, it tends to generate better code to define
   this as 1, since it avoids making a QI or HI mode register.  */
#define SLOW_BYTE_ACCESS 1

/* Xtensa doesn't have any instructions that set integer values based on the
   results of comparisons, but the simplification code in the combiner also
   uses this macro.  The value should be either 1 or -1 to enable some
   optimizations in the combiner; I'm not sure which is better for us. */
#define STORE_FLAG_VALUE 1

/* Define this if zero-extension is slow (more than one real instruction).  */
#define SLOW_ZERO_EXTEND

/* Define this to be nonzero if shift instructions ignore all but the low-order
   few bits. */
#define SHIFT_COUNT_TRUNCATED 1

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated. */

#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */

#define Pmode SImode

/* A function address in a call instruction
   is a word address (for indexing purposes)
   so give the MEM rtx a words's mode.  */

#define FUNCTION_MODE SImode

/* A C expression that evaluates to true if it is ok to perform a
   sibling call to DECL.

   It is not uncommon for limitations of calling conventions to
   prevent tail calls to functions outside the current unit of
   translation, or during PIC compilation.  Use this macro to enforce
   these restrictions, as the `sibcall' md pattern can not fail, or
   fall over to a "normal" call. */

/* TODO: fix this up to allow at least some sibcalls */
#define FUNCTION_OK_FOR_SIBCALL(DECL) 0

/* A part of a C 'switch' statement that describes the relative
   costs of constant RTL expressions.  It must contain 'case'
   labels for expression codes 'const_int', 'const', 'symbol_ref',
   'label_ref' and 'const_double'.  Each case must ultimately reach
   a 'return' statement to return the relative cost of the use of
   that kind of constant value in an expression.  The cost may
   depend on the precise value of the constant, which is available
   for examination in X.

   CODE is the expression code--redundant, since it can be obtained
   with 'GET_CODE (X)'.  */

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


/* Like 'CONST_COSTS' but applies to nonconstant RTL expressions.
   This can be used, for example, to indicate how costly a multiply
   instruction is.  In writing this macro, you can use the construct
   'COSTS_N_INSNS (N)' to specify a cost equal to N fast instructions.

   This macro is optional; do not define it if the default cost
   assumptions are adequate for the target machine. */

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
   contains ADDRESS.  If not defined, the cost is computed from the
   form of the ADDRESS expression and the 'CONST_COSTS' values.
   This macro will normally either not be defined or be defined as
   a constant. */

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
  {"non_acc_reg_operand",	{ REG, SUBREG }},			\
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
   This sequence is indexed by compiler's hard-register-number (see above).

   In order to support the two different conventions for register names,
   we use the name of a table set up in xtensa.c, which is overwritten
   if -mrnames is used.  */

#define REGISTER_NAMES		\
{				\
  &xtensa_reg_names[0][0],	\
  &xtensa_reg_names[1][0],	\
  &xtensa_reg_names[2][0],	\
  &xtensa_reg_names[3][0],	\
  &xtensa_reg_names[4][0],	\
  &xtensa_reg_names[5][0],	\
  &xtensa_reg_names[6][0],	\
  &xtensa_reg_names[7][0],	\
  &xtensa_reg_names[8][0],	\
  &xtensa_reg_names[9][0],	\
  &xtensa_reg_names[10][0],	\
  &xtensa_reg_names[11][0],	\
  &xtensa_reg_names[12][0],	\
  &xtensa_reg_names[13][0],	\
  &xtensa_reg_names[14][0],	\
  &xtensa_reg_names[15][0],	\
  &xtensa_reg_names[16][0],	\
  &xtensa_reg_names[17][0],	\
  &xtensa_reg_names[18][0],	\
  &xtensa_reg_names[19][0],	\
  &xtensa_reg_names[20][0],	\
  &xtensa_reg_names[21][0],	\
  &xtensa_reg_names[22][0],	\
  &xtensa_reg_names[23][0],	\
  &xtensa_reg_names[24][0],	\
  &xtensa_reg_names[25][0],	\
  &xtensa_reg_names[26][0],	\
  &xtensa_reg_names[27][0],	\
  &xtensa_reg_names[28][0],	\
  &xtensa_reg_names[29][0],	\
  &xtensa_reg_names[30][0],	\
  &xtensa_reg_names[31][0],	\
  &xtensa_reg_names[32][0],	\
  &xtensa_reg_names[33][0],	\
  &xtensa_reg_names[34][0],	\
  &xtensa_reg_names[35][0]	\
}

/* print-rtl.c can't use REGISTER_NAMES, since it depends on xtensa.c.
   So define this for it.  */
#define DEBUG_REGISTER_NAMES						\
{									\
  "a0",   "sp",   "a2",   "a3",   "a4",   "a5",   "a6",   "a7",		\
  "a8",   "a9",   "a10",  "a11",  "a12",  "a13",  "a14",  "a15",	\
  "fp",   "argp", "b0",   					\
  "f0",   "f1",   "f2",   "f3",   "f4",   "f5",   "f6",   "f7",		\
  "f8",   "f9",   "f10",  "f11",  "f12",  "f13",  "f14",  "f15",	\
  "acc",								\
}

/* If defined, a C initializer for an array of structures
   containing a name and a register number.  This macro defines
   additional names for hard registers, thus allowing the 'asm'
   option in declarations to refer to registers using alternate
   names.

   We define both names for the integer registers here.  */

#define ADDITIONAL_REGISTER_NAMES					\
{									\
  { "a0",	 0 + GP_REG_FIRST },					\
  { "a1",	 1 + GP_REG_FIRST },					\
  { "a2",	 2 + GP_REG_FIRST },					\
  { "a3",	 3 + GP_REG_FIRST },					\
  { "a4",	 4 + GP_REG_FIRST },					\
  { "a5",	 5 + GP_REG_FIRST },					\
  { "a6",	 6 + GP_REG_FIRST },					\
  { "a7",	 7 + GP_REG_FIRST },					\
  { "a8",	 8 + GP_REG_FIRST },					\
  { "a9",	 9 + GP_REG_FIRST },					\
  { "a10",	10 + GP_REG_FIRST },					\
  { "a11",	11 + GP_REG_FIRST },					\
  { "a12",	12 + GP_REG_FIRST },					\
  { "a13",	13 + GP_REG_FIRST },					\
  { "a14",	14 + GP_REG_FIRST },					\
  { "a15",	15 + GP_REG_FIRST },					\
  { "sp",	 1 + GP_REG_FIRST },					\
  { "b0",	 0 + BR_REG_FIRST },					\
  { "f0",	 0 + FP_REG_FIRST },					\
  { "f1",	 1 + FP_REG_FIRST },					\
  { "f2",	 2 + FP_REG_FIRST },					\
  { "f3",	 3 + FP_REG_FIRST },					\
  { "f4",	 4 + FP_REG_FIRST },					\
  { "f5",	 5 + FP_REG_FIRST },					\
  { "f6",	 6 + FP_REG_FIRST },					\
  { "f7",	 7 + FP_REG_FIRST },					\
  { "f8",	 8 + FP_REG_FIRST },					\
  { "f9",	 9 + FP_REG_FIRST },					\
  { "f10",	10 + FP_REG_FIRST },					\
  { "f11",	11 + FP_REG_FIRST },					\
  { "f12",	12 + FP_REG_FIRST },					\
  { "f13",	13 + FP_REG_FIRST },					\
  { "f14",	14 + FP_REG_FIRST },					\
  { "f15",	15 + FP_REG_FIRST },					\
  { "acc",	 0 + ACC_REG_FIRST },					\
}

/* Define results of standard character escape sequences.  */
#define TARGET_BELL	007
#define TARGET_BS	010
#define TARGET_TAB	011
#define TARGET_NEWLINE	012
#define TARGET_VT	013
#define TARGET_FF	014
#define TARGET_CR	015


/* A C compound statement to output to stdio stream STREAM the
   assembler syntax for an instruction operand X.  */

#define PRINT_OPERAND(FILE, X, CODE) print_operand (FILE, X, CODE)


/* A C compound statement to output to stdio stream STREAM the
   assembler syntax for an instruction operand that is a memory
   reference whose address is ADDR.  */

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


/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME. */

#define ASM_OUTPUT_LABEL(STREAM, NAME)					\
  do {									\
    assemble_name (STREAM, NAME);					\
    fputs (":\n", STREAM);						\
  } while (0)


/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(STREAM, NAME)				\
  do {									\
    fputs ("\t.global\t", STREAM);					\
    assemble_name (STREAM, NAME);					\
    fputs ("\n", STREAM);						\
  } while (0)


/* This says how to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(STREAM, NAME, SIZE, ROUNDED)			\
  xtensa_declare_object (STREAM, NAME, "\n\t.comm\t", ",%u\n", (SIZE))


/* This says how to define a local common symbol (ie, not visible to
   linker).  */

#define ASM_OUTPUT_LOCAL(STREAM, NAME, SIZE, ROUNDED)			\
  xtensa_declare_object (STREAM, NAME, "\n\t.lcomm\t", ",%u\n", (SIZE))


/* This is how to output an assembler line defining a 'double' constant.  */

#define ASM_OUTPUT_DOUBLE(STREAM, VALUE)				\
  do {									\
    long value_long[2];							\
    REAL_VALUE_TO_TARGET_DOUBLE ((VALUE), value_long);			\
    fprintf (STREAM, "\t.word\t0x%08lx\t\t# %.20g\n",			\
	     value_long[0], (VALUE));					\
    fprintf (STREAM, "\t.word\t0x%08lx\n", value_long[1]);		\
  } while (0)


/* This is how to output an assembler line defining a 'float' constant.  */

#define ASM_OUTPUT_FLOAT(STREAM, VALUE)					\
  do {									\
    long value_long;							\
    REAL_VALUE_TO_TARGET_SINGLE ((VALUE), value_long);			\
    fprintf (STREAM, "\t.word\t0x%08lx\t\t# %.12g (float)\n",		\
	     value_long, (VALUE));					\
  } while (0)


/* This is how to output an assembler line defining an 'int' constant.  */

#define ASM_OUTPUT_INT(STREAM, VALUE)					\
  do {									\
    fprintf (STREAM, "\t.word\t");					\
    output_addr_const (STREAM, (VALUE));				\
    fprintf (STREAM, "\n");						\
  } while (0)

#define ASM_OUTPUT_SHORT(STREAM, VALUE)					\
  do {									\
    fprintf (STREAM, "\t.short\t");					\
    output_addr_const (STREAM, (VALUE));				\
    fprintf (STREAM, "\n");						\
  } while (0)

#define ASM_OUTPUT_CHAR(STREAM, VALUE)					\
  do {									\
    fprintf (STREAM, "\t.byte\t");					\
    output_addr_const (STREAM, (VALUE));				\
    fprintf (STREAM, "\n");						\
  } while (0)


/* This is how to output an assembler line for a numeric constant byte.  */

#define ASM_OUTPUT_BYTE(STREAM, VALUE)					\
  fprintf (STREAM, "\t.byte\t0x%x\n", (VALUE))


/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE)				\
  fprintf (STREAM, "\t.word\t%sL%u\n", LOCAL_LABEL_PREFIX, VALUE)


/* This is how to output an element of a case-vector that is relative.
   This is used for pc-relative code. */

#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL)		\
  do {									\
    fprintf (STREAM, "\t.word\t%sL%u-%sL%u\n",				\
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


/* This macro controls the order that induction variables are combined.
   In some cases, the strength reduction optimization pass can produce
   better code if this is defined. This macro is particularly useful if
   the target has limited addressing modes.  For instance, the SH target
   has only positive offsets in addresses.  Thus sorting to put the
   smallest address first allows the most combinations to be found.

   Since the Xtensa architecture lacks negative address offsets,
   the givs should be sorted smallest to largest so combine_givs
   has maximum opportunity to combine givs.  */

#define GIV_SORT_CRITERION(X, Y)					\
  do {									\
    if (GET_CODE ((X)->add_val) == CONST_INT				\
	&& GET_CODE ((Y)->add_val) == CONST_INT)			\
      return INTVAL ((X)->add_val) - INTVAL ((Y)->add_val);		\
  } while (0)


/* A C statement or statements to switch to the appropriate section
   for output of RTX in mode MODE.  You can assume that RTX is some
   kind of constant in RTL.  The argument MODE is redundant except in
   the case of a 'const_int' rtx.  Select the section by calling
   'text_section' or one of the alternatives for other sections.
 
   For Xtensa's constant pools, we use the ".literal" directive,
   and we don't switch sections at all. */
 
#undef SELECT_RTX_SECTION
#define SELECT_RTX_SECTION(MODE, RTX)
  
/* Define the strings to put out for each section in the object file.  */
#define TEXT_SECTION_ASM_OP	"\t.text"  	/* instructions */
#define DATA_SECTION_ASM_OP	"\t.data" 	/* large data */


/* Define output to appear before the constant pool.  If the function
   has been assigned to a specific ELF section, or if it goes into a
   unique section, set the name of that section to be the literal
   prefix. */

#define ASM_OUTPUT_POOL_PROLOGUE(FILE, FUNNAME, FUNDECL, SIZE)          \
  do {									\
    tree fnsection;							\
    if ((flag_function_sections						\
	 && DECL_SECTION_NAME (FUNDECL) == NULL_TREE)			\
	|| UNIQUE_SECTION_P (FUNDECL))					\
      UNIQUE_SECTION (FUNDECL, 0);					\
    fnsection = DECL_SECTION_NAME (FUNDECL);				\
    if (fnsection != NULL_TREE)						\
      {									\
	const char *fnsectname = TREE_STRING_POINTER (fnsection);	\
	fprintf (FILE, "\t.begin\tliteral_prefix %s\n",			\
		 strcmp (fnsectname, ".text") ? fnsectname : "");	\
      }									\
    if ((SIZE) > 0)							\
      function_section (FUNDECL);  					\
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

/* Define the parentheses used to group arithmetic operations
   in assembler code.  */

#define ASM_OPEN_PAREN "("
#define ASM_CLOSE_PAREN ")"

/* How to start an assembler comment. */
#ifndef ASM_COMMENT_START
#define ASM_COMMENT_START "#"
#endif

/* Exception handling TODO!! */
#define DWARF_UNWIND_INFO 0

