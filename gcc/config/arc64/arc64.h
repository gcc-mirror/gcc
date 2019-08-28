/* Machine description for ARC64 architecture.
   Copyright (C) 2021 Free Software Foundation, Inc.

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

#ifndef GCC_ARC64_H
#define GCC_ARC64_H

/* Bits are always numbered from the LSBit.  */
#define BITS_BIG_ENDIAN 0

/* Define this if most significant byte of a word is the lowest numbered.  */
#define BYTES_BIG_ENDIAN 0

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
#define WORDS_BIG_ENDIAN 0

/* Is the 64bit or 32bit variant of the CPU used?  */
#define TARGET_64BIT arc64_target_64bit

/* Determine TARGET_ARCH64 in all possible cases. */
#ifdef IN_LIBGCC2
#if defined(__ARC64_ARCH64__)
#define TARGET_ARCH64 1
#else
#define TARGET_ARCH64 0
#endif
#else /* not IN_LIBGCC2 */
#define TARGET_ARCH64 TARGET_64BIT
#endif

#define MAX_BITS_PER_WORD 64

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD  (TARGET_ARCH64 ? 8 : 4)
#ifndef IN_LIBGCC2
#define MIN_UNITS_PER_WORD 4
#endif

/* Width of a fp register, in bytes.  */
#define UNITS_PER_FP_REG ((arc64_fp_model == 2) ? 8 : 4)

/* Maximum number of registers that can appear in a valid memory
   address.  N.B. The ld insn allows 2, but the st insn only allows
   1.  */
#define MAX_REGS_PER_ADDRESS 2

/* Addressing modes.  */
#define HAVE_PRE_INCREMENT 1
#define HAVE_PRE_DECREMENT 1
#define HAVE_POST_INCREMENT 1
#define HAVE_POST_DECREMENT 1
#define HAVE_PRE_MODIFY_DISP 1
#define HAVE_POST_MODIFY_DISP 1
#define HAVE_PRE_MODIFY_REG 1
#define HAVE_POST_MODIFY_REG 1

/* The number of registers used for parameter passing.  Local to this
   file.  */
#define MAX_ARC64_PARM_REGS 8

/* 1 if N is a possible register number for function argument
   passing.  */
/* Hard floats: r0-r7, and f0-f7.  */
#define FUNCTION_ARG_REGNO_P(N)						\
  (IN_RANGE ((N), R0_REGNUM, R7_REGNUM)					\
   || (ARC64_HAS_FP_BASE && IN_RANGE ((N), F0_REGNUM, F7_REGNUM)))

/* Boundaries.  */
#define PARM_BOUNDARY		BITS_PER_WORD
#define STACK_BOUNDARY		POINTER_SIZE
#define FUNCTION_BOUNDARY	32
#define EMPTY_FIELD_BOUNDARY	32
#define STRUCTURE_SIZE_BOUNDARY 8

/* Look at the fundamental type that is used for a bit-field and use
   that to impose alignment on the enclosing structure.  struct s {int
   a:8}; should have same alignment as "int", not "char".  */
#define	PCC_BITFIELD_TYPE_MATTERS	1

/* Alignments.  */
#define FASTEST_ALIGNMENT       BITS_PER_WORD
/* pr64242.c is one interesting test which changing BIGGEST_ALIGNMENT triggers
   errors.  */
#define BIGGEST_ALIGNMENT	BITS_PER_WORD
#define ARC64_EXPAND_ALIGNMENT(COND, EXP, ALIGN)			\
  (((COND) && ((ALIGN) < FASTEST_ALIGNMENT)				\
    && (TREE_CODE (EXP) == ARRAY_TYPE)) ? FASTEST_ALIGNMENT : (ALIGN))

/* Align global data.  */
#define DATA_ALIGNMENT(EXP, ALIGN)			\
  ARC64_EXPAND_ALIGNMENT (!optimize_size, EXP, ALIGN)

/* Similarly, make sure that objects on the stack are sensibly
   aligned.  */
#define LOCAL_ALIGNMENT(EXP, ALIGN)				\
  ARC64_EXPAND_ALIGNMENT (!flag_conserve_stack, EXP, ALIGN)

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT        (!unaligned_access)

/* Default unaligned accesses.  */
#ifndef UNALIGNED_ACCESS_DEFAULT
#define UNALIGNED_ACCESS_DEFAULT 0
#endif

/* Layout of Source Language Data Types.  */
#define SHORT_TYPE_SIZE         16
#define INT_TYPE_SIZE           32
#define LONG_LONG_TYPE_SIZE     64
#define POINTER_SIZE            (TARGET_ARCH64 ? 64 : 32)
#define LONG_TYPE_SIZE          POINTER_SIZE

/* Defined for convenience.  */
#define POINTER_BYTES (POINTER_SIZE / BITS_PER_UNIT)

#define FLOAT_TYPE_SIZE		32
#define DOUBLE_TYPE_SIZE	64
#define LONG_DOUBLE_TYPE_SIZE	64

/* Defined by ABI.  */
#define WCHAR_TYPE "int"
#define WCHAR_TYPE_SIZE 32

#define DEFAULT_SIGNED_CHAR 0

#undef SIZE_TYPE
#define SIZE_TYPE       (POINTER_SIZE == 64 ? "long unsigned int" : "unsigned int")

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE	(POINTER_SIZE == 64 ? "long int" : "int")

/* Specify the machine mode that the hardware addresses have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */

#define Pmode word_mode

/* Mode of a function address in a call instruction (for indexing purposes).  */
#define FUNCTION_MODE	Pmode

#ifdef HAVE_AS_TLS
#define ARC64_TLS_REGNO 1
#else
#define ARC64_TLS_REGNO 0
#endif

/* Register usage:
   R0-R3    Parameter/result registers
   R4-R7    Parameter registers
   R8-R13   Temporary registers
   R14-R26  Callee-saved registers
   R27      FP (frame pointer)
   R28      SP (stack pointer)
   R29      ILINK (Interrupt link register)
   R30      GP/TP Global pointer, also it is used as thread pointer;
            otherwise can be used  as a temporary register.
   R31      BLINK (return register)
   R32-R57  Extension registers
   R58      ACC (accumulator)
   R59      Reserved
   --- Special registers ---
   R60      sign-extended 32-bit indicator
   R61      Reserved
   R62      zero extended 32-bit immediate indicator
   R63      PCL (program counter)
   --- Floating point registers ---
   F0       Parameter/result register
   F1-F7    Parameter registers
   F8-F13   Temporary registers
   F14-F31  Callee-saved registers
   -- Fake registers --
   AP       Argument pointer
   SFP      Soft frame pointer
   CC       Status register.
 */

/* 1 for registers that are not available for the register
   allocator.  */
#define FIXED_REGISTERS							\
  {									\
    0, 0, 0, 0,   0, 0, 0, 0,  /* R0 - R7 */				\
    0, 0, 0, 0,   0, 0, 0, 0,  /* R8 - R15 */				\
    0, 0, 0, 0,   0, 0, 0, 0,  /* R16 - R23 */				\
    0, 0, 0, 0,   1, 1, ARC64_TLS_REGNO, 1,  /* R24 - R26, FP, SP, ILINK, R30, BLINK */ \
    									\
    1, 1, 1, 1,   1, 1, 1, 1,  /* R32 - R39 */				\
    1, 1, 1, 1,   1, 1, 1, 1,  /* R40 - R47 */				\
    1, 1, 1, 1,   1, 1, 1, 1,  /* R48 - R55 */				\
    1, 1, 1, 1,   1, 1, 1, 1,  /* R56, R57, ACCL, R59, Specials */	\
    									\
    1, 1, 1, 1,   1, 1, 1, 1,  /* F0 - F7 */				\
    1, 1, 1, 1,   1, 1, 1, 1,  /* F8 - F15 */				\
    1, 1, 1, 1,   1, 1, 1, 1,  /* F16 - F23 */				\
    1, 1, 1, 1,   1, 1, 1, 1,  /* F24 - F31 */				\
    									\
    1, 1, 1,                   /* AP, SFP, CC */			\
  }

/* 1 for registers not available across function calls.  */
#define CALL_USED_REGISTERS						\
  {									\
   1, 1, 1, 1,   1, 1, 1, 1,  /* R0 - R7 */				\
   1, 1, 1, 1,   1, 1, 0, 0,  /* R8 - R15 */				\
   0, 0, 0, 0,   0, 0, 0, 0,  /* R16 - R23 */				\
   0, 0, 0, 0,   1, 1, 1, 1,  /* R24 - R26, FP, SP, ILINK, R30, BLINK */ \
									\
   1, 1, 1, 1,   1, 1, 1, 1,  /* R32 - R39 */				\
   1, 1, 1, 1,   1, 1, 1, 1,  /* R40 - R47 */				\
   1, 1, 1, 1,   1, 1, 1, 1,  /* R48 - R55 */				\
   1, 1, 1, 1,   1, 1, 1, 1,  /* R56, R57, ACCL, R59, Specials */	\
									\
   1, 1, 1, 1,   1, 1, 1, 1,  /* F0 - F7 */				\
   1, 1, 1, 1,   1, 1, 1, 1,  /* F8 - F15 */				\
   1, 1, 1, 1,   1, 1, 1, 1,  /* F16 - F23 */				\
   1, 1, 1, 1,   1, 1, 1, 1,  /* F24 - F31 */				\
    									\
   1, 1, 1,                   /* AP, SFP, CC */				\
  }

#define REGISTER_NAMES							\
  {									\
   "r0",  "r1",  "r2",  "r3",     "r4",   "r5",    "r6",  "r7",		\
   "r8",  "r9",  "r10", "r11",    "r12",  "r13",   "r14", "r15",	\
   "r16", "r17", "r18", "r19",    "r20",  "r21",   "r22", "r23",	\
   "r24", "r25", "r26", "r27",    "sp",   "ilink", "r30", "blink",	\
   "r32", "r33", "r34", "r35",    "r36",  "r37",   "r38", "r39",	\
   "r40", "r41", "r42", "r43",    "r44",  "r45",   "r46", "r47",	\
   "r48", "r49", "r50", "r51",    "r52",  "r53",   "r54", "r55",	\
   "r56", "r57", "r58", "r59",    "ximm", "rez",   "limm", "pcl",	\
									\
   "f0",  "f1",  "f2",  "f3",     "f4",   "f5",    "f6",  "f7",		\
   "f8",  "f9",  "f10", "f11",    "f12",  "f13",   "f14", "f15",	\
   "f16", "f17", "f18", "f19",    "f20",  "f21",   "f22", "f23",	\
   "f24", "f25", "f26", "f27",    "f28",  "f29",   "f30", "f31",	\
   "ap", "sfp", "cc",							\
  }

#define ADDITIONAL_REGISTER_NAMES		\
  {						\
   { "fp", 27 },				\
   { "gp", 30 },				\
   { "acc", 58 },				\
  }

#define EPILOGUE_USES(REGNO) (arc64_epilogue_uses (REGNO))

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  This is only true if the function
   uses alloca.  */
#define EXIT_IGNORE_STACK (cfun->calls_alloca)

#define STATIC_CHAIN_REGNUM		R11_REGNUM
#define HARD_FRAME_POINTER_REGNUM	R27_REGNUM
#define FRAME_POINTER_REGNUM		SFP_REGNUM
#define STACK_POINTER_REGNUM		SP_REGNUM
#define ARG_POINTER_REGNUM		AP_REGNUM
#define FIRST_PSEUDO_REGISTER		(CC_REGNUM + 1)

enum reg_class
{
   NO_REGS,
   AC16_REGS,
   SIBCALL_REGS,
   CORE_REGS,
   GENERAL_REGS,
   FP_REGS,
   ALL_REGS,
   LIM_REG_CLASSES
};

#define N_REG_CLASSES	((int) LIM_REG_CLASSES)

#define REG_CLASS_NAMES				\
{						\
    "NO_REGS",					\
    "AC16_REGS",				\
    "SIBCALL_REGS",				\
    "CORE_REGS",				\
    "GENERAL_REGS",				\
    "FP_REGS",					\
    "ALL_REGS"					\
}

#define REG_CLASS_CONTENTS					\
{								\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000 }, /* NO_REGS */	\
  { 0x0000f00f, 0x00000000, 0x00000000, 0x00000000 }, /* AC16_REGS */	\
  { 0x00001fff, 0x00000000, 0x00000000, 0x00000000 }, /* SIBCALL_REGS */ \
  { 0x0000ffff, 0x00000000, 0x00000000, 0x00000000 }, /* CORE_REGS */ \
  { 0xdfffffff, 0x0fffffff, 0x00000000, 0x00000003 }, /* GENERAL_REGS */ \
  { 0x00000000, 0x00000000, 0xffffffff, 0x00000000 }, /* FP_REGS */	\
  { 0xffffffff, 0xffffffff, 0xffffffff, 0x00000007 }, /* ALL_REGS */	\
}

/* A C expression whose value is a register class containing hard
   register REGNO.  In general there is more that one such class;
   choose a class which is "minimal", meaning that no smaller class
   also contains the register.  */

#define REGNO_REG_CLASS(REGNO) arc64_regno_to_regclass[ (REGNO) ]

/* A C expression that is nonzero if it is OK to rename a hard register FROM to
   another hard register TO.  */

#define HARD_REGNO_RENAME_OK(FROM, TO) arc64_hard_regno_rename_ok (FROM, TO)

/* The class value for valid base registers. A base register is one used in
   an address which is the register value plus a displacement.  */

#define BASE_REG_CLASS GENERAL_REGS
#define INDEX_REG_CLASS GENERAL_REGS

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

#define ELIMINABLE_REGS						\
  {								\
    { ARG_POINTER_REGNUM,   STACK_POINTER_REGNUM },		\
    { ARG_POINTER_REGNUM,   HARD_FRAME_POINTER_REGNUM }, 	\
    { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM },      	\
    { FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM }  	\
  }

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)		\
  (OFFSET) = arc64_initial_elimination_offset (FROM, TO)

/* RTL generation support.  */
#define INIT_EXPANDERS arc64_init_expanders ()

/* Stack layout; function entry, exit and calling.  */
#define STACK_GROWS_DOWNWARD	1

/* Addresses of local variables slots are at negative offsets from the
   frame pointer.  */
#define FRAME_GROWS_DOWNWARD	1

/* If defined, the maximum amount of space required for outgoing
   arguments will be computed and placed into the variable
   `crtl->outgoing_args_size'.  No space will be pushed onto the stack
   for each call; instead, the function prologue should increase the
   stack frame size by this amount.  */
#define ACCUMULATE_OUTGOING_ARGS	1

/* Offset of first parameter from the argument pointer register
   value.  */
#define FIRST_PARM_OFFSET(FNDECL) 0

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */
#define LIBCALL_VALUE(MODE)						\
  gen_rtx_REG (MODE, arc64_use_fp_regs (MODE) ? F0_REGNUM : R0_REGNUM)

/* Tell GCC to use RETURN_IN_MEMORY.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* To be check: WORD_REGISTER_OPERATIONS, ARC64 has 32bit
   opertations.  */
#define WORD_REGISTER_OPERATIONS 1

/* Define if loading from memory in MODE, an integral mode narrower than
   BITS_PER_WORD will either zero-extend or sign-extend.  The value of this
   macro should be the code that says which one of the two operations is
   implicitly done, or UNKNOWN if none.  */
#define LOAD_EXTEND_OP(MODE) (((MODE) == SImode) ? SIGN_EXTEND : ZERO_EXTEND)

/* Enable wide bitfield accesses for more efficient bitfield code.  */
#define SLOW_BYTE_ACCESS 1

#define NO_FUNCTION_CSE	1

/* Conditional info.  */
#define SELECT_CC_MODE(OP, X, Y) arc64_select_cc_mode (OP, X, Y)

/* Restrictions apply to floating-point comparisons.  */
#define REVERSIBLE_CC_MODE(MODE) ((MODE) != CC_FPUmode && (MODE) != CC_FPUEmode)

/* Returning.  */
#define INCOMING_RETURN_ADDR_RTX gen_rtx_REG (Pmode, BLINK_REGNUM)

#define RETURN_ADDR_RTX arc64_return_addr

/* Define this to be nonzero if shift instructions ignore all but the
   low-order few bits.  */
#define SHIFT_COUNT_TRUNCATED 1

/* Defines if the CLZ result is undefined or has a useful value.  */
#define CLZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE)			\
  ((VALUE) = GET_MODE_SIZE((MODE)) * BITS_PER_UNIT - 1, 2)

/* Defines if the CTZ result is undefined or has a useful value.  */
#define CTZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE)			\
  ((VALUE) = GET_MODE_SIZE((MODE)) * BITS_PER_UNIT - 1, 2)

/* Function argument passing.  */

/* Define a data type for recording info about an argument list during
   the scan of that argument list.  This data type should hold all
   necessary information about the function itself and about the args
   processed so far, enough to enable macros such as FUNCTION_ARG to
   determine where the next arg should go.  */
#define CUMULATIVE_ARGS struct arc64_args
struct arc64_args
{
  /* Number of integer registers used so far.  */
  int iregs;

  /* Number of floating-point registers used so far.  */
  int fregs;
};

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */
#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,INDIRECT,N_NAMED_ARGS) \
  ((CUM).iregs = 0, (CUM).fregs = 0)

/* An integer expression for the size in bits of the largest integer machine
   mode that should actually be used.  We allow pairs of registers.  */
#define MAX_FIXED_MODE_SIZE GET_MODE_BITSIZE (TARGET_ARCH64 ? TImode : DImode)

/* Maximum bytes moved by a single instruction (load/store pair).  */
#define MOVE_MAX (2*UNITS_PER_WORD)
#define MAX_MOVE_MAX 16

/* The base cost overhead of a memcpy call, for MOVE_RATIO and friends.  */
#define ARC64_CALL_RATIO 8

/* MOVE_RATIO dictates when we will use the move_by_pieces infrastructure.
   move_by_pieces will continually copy the largest safe chunks.  So a
   7-byte copy is a 4-byte + 2-byte + byte copy.  This proves inefficient
   for both size and speed of copy, so we will instead use the "cpymem"
   standard name to implement the copy.  This logic does not apply when
   targeting -mstrict-align, so keep a sensible default in that case.  */
#define MOVE_RATIO(speed) \
  (!STRICT_ALIGNMENT ? 2 : ((speed) ? 15 : ARC64_CALL_RATIO))

#ifndef USED_FOR_TARGET
extern const enum reg_class arc64_regno_to_regclass[];
#endif

#define SIGNED(X,V)							\
  ((unsigned long long) ((X) + (1ULL << (V - 1))) < (1ULL << V))
#define UNSIGNED(X,V) ((unsigned long long) (X) < (1ULL << V))
#define VERIFY_SHIFT(X,S) ((X & ((1 << S) - 1)) == 0)

#define UNSIGNED_INT3(X) (UNSIGNED(X,3))
#define UNSIGNED_INT5(X) (UNSIGNED(X,5))
#define UNSIGNED_INT6(X) (UNSIGNED(X,6))
#define UNSIGNED_INT7(X) (UNSIGNED(X,7))
#define UNSIGNED_INT8(X) (UNSIGNED(X,8))
#define UNSIGNED_INT9(X) (UNSIGNED(X,9))
#define UNSIGNED_INT10(X) (UNSIGNED(X,10))
#define UNSIGNED_INT12(X) (UNSIGNED(X,12))
#define UNSIGNED_INT16(X) (UNSIGNED(X,16))
// TODO: Fix for 32 bit compiler host architecture.
#define UNSIGNED_INT32(X) (UNSIGNED(X,32))

#define SIGNED_INT3(X) (SIGNED(X,3))
#define SIGNED_INT6(X) (SIGNED(X,6))
#define SIGNED_INT7(X) (SIGNED(X,7))
#define SIGNED_INT8(X) (SIGNED(X,8))
#define SIGNED_INT9(X) (SIGNED(X,9))
#define SIGNED_INT10(X) (SIGNED(X,10))
#define SIGNED_INT11(X) (SIGNED(X,11))
#define SIGNED_INT12(X) (SIGNED(X,12))
#define SIGNED_INT13(X) (SIGNED(X,13))
#define SIGNED_INT16(X) (SIGNED(X,16))
#define SIGNED_INT21(X) (SIGNED(X,21))
#define SIGNED_INT25(X) (SIGNED(X,25))

// TODO: Fix for 32 bit compiler host architecture.
#define SIGNED_INT32(X) (SIGNED(X,32))

#define UNSIGNED_INT7_SHIFTED(X,S) (VERIFY_SHIFT(X,S) && UNSIGNED_INT6(X >> S))
#define UNSIGNED_INT8_SHIFTED(X,S) (VERIFY_SHIFT(X,S) && UNSIGNED_INT6(X >> S))
#define UNSIGNED_INT9_SHIFTED(X,S) (VERIFY_SHIFT(X,S) && UNSIGNED_INT6(X >> S))

#define SIGNED_INT13_SHIFTED(X,S) (VERIFY_SHIFT(X,S) && SIGNED_INT12(X >> S))
#define SIGNED_INT14_SHIFTED(X,S) (VERIFY_SHIFT(X,S) && SIGNED_INT12(X >> S))
#define SIGNED_INT15_SHIFTED(X,S) (VERIFY_SHIFT(X,S) && SIGNED_INT12(X >> S))

#define  IS_POWEROF2_P(X) (! ( (X) & ((X) - 1)) && (X))

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */
#define REGNO_OK_FOR_BASE_P(REGNO)					\
  (GP_REGNUM_P (REGNO)							\
   || ((REGNO) == AP_REGNUM)						\
   || ((REGNO) == SFP_REGNUM))

#define REGNO_OK_FOR_INDEX_P(REGNO) REGNO_OK_FOR_BASE_P(REGNO)

/* Return true if regno is FP register.  */
#define FP_REGNUM_P(REGNO)						\
  (((unsigned) (REGNO - F0_REGNUM)) <= (F31_REGNUM - F0_REGNUM))

#define GP_REGNUM_P(REGNO)						\
  (((unsigned) (REGNO - R0_REGNUM)) <= (BLINK_REGNUM - R0_REGNUM))

/* Trampolines, used for entering nested functions, are a block of code
   followed by two pointers.  The sizes here are in bytes.  */
#define TRAMPOLINE_CODE_SIZE	  \
   ((Pmode == SImode)		  \
    ? 8	  /* ld_s, ld, j_s     */ \
    : 16) /* nop, ldl, ldl, j  */
#define TRAMPOLINE_SIZE (TRAMPOLINE_CODE_SIZE + 2 * POINTER_BYTES)
/* Alignment required for a trampoline in bits .  */
#define TRAMPOLINE_ALIGNMENT POINTER_SIZE

/* Names to predefine in the preprocessor for this target machine.  */
#define TARGET_CPU_CPP_BUILTINS() arc64_cpu_cpp_builtins (pfile)

/* Dispatch tables.  */
#define JUMP_TABLES_IN_TEXT_SECTION 1
#define CASE_VECTOR_MODE SImode
#define CASE_VECTOR_PC_RELATIVE 1
#define ADDR_VEC_ALIGN(VEC_INSN) 0

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases,
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.  */
#define PROMOTE_MODE(MODE,UNSIGNEDP,TYPE)	\
  if (GET_MODE_CLASS (MODE) == MODE_INT		\
      && GET_MODE_SIZE (MODE) < 4)		\
    {						\
      (MODE) = SImode;				\
    }


/* A C string constant describing how to begin a comment in the target
   assembler language.  The compiler assumes that the comment will
   end at the end of the line.  */
#define ASM_COMMENT_START "#"

#define ASM_OUTPUT_ALIGN(FILE,LOG) \
  fprintf(FILE, "\t.align\t%d\n", 1 << (int)LOG)

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */
#undef ASM_APP_ON
#define ASM_APP_ON ""

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */
#undef ASM_APP_OFF
#define ASM_APP_OFF ""

/* This is how to output a reference to a symbol_ref / label_ref as
   (part of) an operand.  To disambiguate from register names like a1
   / a2 / status etc, symbols are preceded by '@'.  */
#define ASM_OUTPUT_SYMBOL_REF(FILE,SYM) \
  ASM_OUTPUT_LABEL_REF ((FILE), XSTR ((SYM), 0))
#define ASM_OUTPUT_LABEL_REF(FILE,STR)			\
  do							\
    {							\
      fputs ("@", (FILE));				\
      assemble_name ((FILE), (STR));			\
    }							\
  while (0)

#define LOCAL_LABEL_PREFIX     "."

/* This is how to output an element of a PIC case-vector. */
#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL)	       \
  fprintf (STREAM, "\tb\t@%sL%d\n",				       \
          LOCAL_LABEL_PREFIX, VALUE)

/* Defined to also emit an .align in elfos.h.  We don't want that.  */
#undef ASM_OUTPUT_CASE_LABEL

/* Section selection.  */

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.global\t"

#define TEXT_SECTION_ASM_OP	"\t.section\t.text"
#define DATA_SECTION_ASM_OP	"\t.section\t.data"

#define BSS_SECTION_ASM_OP	"\t.section\t.bss"
#define SDATA_SECTION_ASM_OP	"\t.section\t.sdata"
#define SBSS_SECTION_ASM_OP	"\t.section\t.sbss"

/* Expression whose value is a string, including spacing, containing
   the assembler operation to identify the following data as
   initialization/termination code.  If not defined, GCC will assume
   such a section does not exist. */
#define INIT_SECTION_ASM_OP "\t.section\t.init"
#define FINI_SECTION_ASM_OP "\t.section\t.fini"

/* All the work done in PROFILE_HOOK, but still required.  */
#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(STREAM, LABELNO) do { } while (0)

#define NO_PROFILE_COUNTERS  1

/* Tell crtstuff.c we're using ELF.  */
#define OBJECT_FORMAT_ELF

/* Called by crtstuff.c to make calls to function FUNCTION that are defined in
   SECTION_OP, and then to switch back to text section.  */
#undef CRT_CALL_STATIC_FUNCTION
#ifdef __ARC64_ARCH32__
#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC)		\
  asm (SECTION_OP "\n\t"					\
       "mov\tr12,@" USER_LABEL_PREFIX #FUNC "\n\t" \
       "jl\t[r12]\n"						\
       TEXT_SECTION_ASM_OP);
#elif (defined __ARC64_ARCH64__)
#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC)		\
  asm (SECTION_OP "\n\t"					\
       "addl\tr12,pcl,@" USER_LABEL_PREFIX #FUNC "@pcl\n\t" \
       "jl\t[r12]\n"						\
       TEXT_SECTION_ASM_OP);
#endif

/* ATOMIC options.  */
/* FIXME: is 0 okay or should it be -1 like DEFAULT_arc_mpy_option?  */
/* Default atomic option value.  */
#undef DEFAULT_ARC64_ATOMIC_OPTION
#define DEFAULT_ARC64_ATOMIC_OPTION 1

#define ARC64_HAS_ATOMIC_1 (arc64_atomic_option > 0)
#define ARC64_HAS_ATOMIC_2 (arc64_atomic_option > 1)
#define ARC64_HAS_ATOMIC_3 (arc64_atomic_option > 2)

/* DIVREM options.  */
#undef TARGET_ARC64_DIVREM_DEFAULT
#define TARGET_ARC64_DIVREM_DEFAULT 1

/* FP options.  */
#define ARC64_HAS_FP_BASE (arc64_fp_model > 0)
#define ARC64_HAS_FPUH    (arc64_fp_model > 0)
#define ARC64_HAS_FPUS    (arc64_fp_model > 0)
#define ARC64_HAS_FPUD    (arc64_fp_model > 1)

#define TARGET_HARD_FLOAT ARC64_HAS_FP_BASE

/* Vector SIMD length.  */
#define ARC64_VFP_32    (arc64_fp_model == 1)
#define ARC64_VFP_64    ((arc64_fp_model == 2) && !TARGET_WIDE_SIMD)
#define ARC64_VFP_128   ((arc64_fp_model == 2) && TARGET_WIDE_SIMD)

/* IFCVT macros.  */
#define STORE_FLAG_VALUE 1
#define MAX_CONDITIONAL_EXECUTE 12
#define BRANCH_COST(speed_p, predictable_p) 10

/* DWARF macros.  */
#define DWARF2_DEBUGGING_INFO 1
/* The mapping from gcc register number to DWARF2 CFA column number.  */
#define DWARF_FRAME_REGNUM(REGNO) DBX_REGISTER_NUMBER(REGNO)
/* DWARF2 CFA column which tracks the return address.  */
#define DWARF_FRAME_RETURN_COLUMN BLINK_REGNUM
/* DWARF registers encodings.  */
#define DBX_REGISTER_NUMBER(REGNO) arc64_dbx_register_number (REGNO)
/* The DWARF 2 CFA column which tracks the return address from a signal handler
   context.  This value must not correspond to a hard register and must be out
   of the range of DWARF_FRAME_REGNUM().  The unwind-dw2.c file is using
   DWARF_REG_TO_UNWIND_COLUMN and DWARF_FRAME_REGISTERS macros.  The
   DWARF_FRAME_REGNUM macro returns no equivalent DWARF register for
   AP_REGNUM. Thus, we should be safe using AP_REGNUM.  */
#define DWARF_ALT_FRAME_RETURN_COLUMN AP_REGNUM

/* Exception Handling support.  */
/* Use R0 through R3 to pass exception handling information.  */
#define EH_RETURN_DATA_REGNO(N) \
  ((N) < 4 ? ((unsigned int) R0_REGNUM + (N)) : INVALID_REGNUM)
#define EH_RETURN_STACKADJ_RTX	gen_rtx_REG (Pmode, R4_REGNUM)
#define EH_RETURN_HANDLER_RTX  arc64_eh_return_handler_rtx ()
#define EH_USES(REGNO) (arc64_eh_uses((REGNO)))

/* Select a format to encode pointers in exception handling data.  */
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE, GLOBAL) \
  arc64_asm_preferred_eh_data_format ((CODE), (GLOBAL))

/* Specs.  */

/* Support for a compile-time defalt CPU or FPU.  */
#define OPTION_DEFAULT_SPECS			\
  { "fpu", "%{!mfpu=*:-mfpu=%(VALUE)}"},	\
  { "cpu", "%{!mcpu=*:-mcpu=%(VALUE)}"}

#define CPP_SPEC "%(subtarget_cpp_spec)"

#define EXTRA_SPECS				      \
  { "subtarget_cpp_spec",	SUBTARGET_CPP_SPEC }, \
  SUBTARGET_EXTRA_SPECS

#undef  ASM_SPEC
#define ASM_SPEC                                \
  "%{mcpu=*:-mcpu=%*}"

#ifndef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS
#endif

#ifndef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC ""
#endif

#undef ARC64_SUBTARGET_DEFAULT
#define ARC64_SUBTARGET_DEFAULT 0

#endif /* GCC_ARC64_H */
