/* Definition of RISC-V target for GNU compiler.
   Copyright (C) 2011-2017 Free Software Foundation, Inc.
   Contributed by Andrew Waterman (andrew@sifive.com).
   Based on MIPS target for GNU compiler.

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

#ifndef GCC_RISCV_H
#define GCC_RISCV_H

#include "config/riscv/riscv-opts.h"

/* Target CPU builtins.  */
#define TARGET_CPU_CPP_BUILTINS() riscv_cpu_cpp_builtins (pfile)

/* Default target_flags if no switches are specified  */

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT 0
#endif

#ifndef RISCV_TUNE_STRING_DEFAULT
#define RISCV_TUNE_STRING_DEFAULT "rocket"
#endif

/* Support for a compile-time default CPU, et cetera.  The rules are:
   --with-arch is ignored if -march is specified.
   --with-abi is ignored if -mabi is specified.
   --with-tune is ignored if -mtune is specified.  */
#define OPTION_DEFAULT_SPECS \
  {"tune", "%{!mtune=*:-mtune=%(VALUE)}" }, \
  {"arch", "%{!march=*:-march=%(VALUE)}" }, \
  {"abi", "%{!mabi=*:-mabi=%(VALUE)}" }, \

#ifdef IN_LIBGCC2
#undef TARGET_64BIT
/* Make this compile time constant for libgcc2 */
#define TARGET_64BIT           (__riscv_xlen == 64)
#endif /* IN_LIBGCC2 */

#undef ASM_SPEC
#define ASM_SPEC "\
%(subtarget_asm_debugging_spec) \
%{" FPIE_OR_FPIC_SPEC ":-fpic} \
%{march=*} \
%{mabi=*} \
%(subtarget_asm_spec)"

#define TARGET_DEFAULT_CMODEL CM_MEDLOW

#define LOCAL_LABEL_PREFIX	"."
#define USER_LABEL_PREFIX	""

/* Offsets recorded in opcodes are a multiple of this alignment factor.
   The default for this in 64-bit mode is 8, which causes problems with
   SFmode register saves.  */
#define DWARF_CIE_DATA_ALIGNMENT -4

/* The mapping from gcc register number to DWARF 2 CFA column number.  */
#define DWARF_FRAME_REGNUM(REGNO) \
  (GP_REG_P (REGNO) || FP_REG_P (REGNO) ? REGNO : INVALID_REGNUM)

/* The DWARF 2 CFA column which tracks the return address.  */
#define DWARF_FRAME_RETURN_COLUMN RETURN_ADDR_REGNUM
#define INCOMING_RETURN_ADDR_RTX gen_rtx_REG (VOIDmode, RETURN_ADDR_REGNUM)

/* Describe how we implement __builtin_eh_return.  */
#define EH_RETURN_DATA_REGNO(N) \
  ((N) < 4 ? (N) + GP_ARG_FIRST : INVALID_REGNUM)

#define EH_RETURN_STACKADJ_RTX  gen_rtx_REG (Pmode, GP_ARG_FIRST + 4)

/* Target machine storage layout */

#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN 0
#define WORDS_BIG_ENDIAN 0

#define MAX_BITS_PER_WORD 64

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD (TARGET_64BIT ? 8 : 4)
#ifndef IN_LIBGCC2
#define MIN_UNITS_PER_WORD 4
#endif

/* The `Q' extension is not yet supported.  */
#define UNITS_PER_FP_REG (TARGET_DOUBLE_FLOAT ? 8 : 4)

/* The largest type that can be passed in floating-point registers.  */
#define UNITS_PER_FP_ARG					\
  (riscv_abi == ABI_ILP32 || riscv_abi == ABI_LP64 ? 0 :	\
   riscv_abi == ABI_ILP32F || riscv_abi == ABI_LP64F ? 4 : 8)	\

/* Set the sizes of the core types.  */
#define SHORT_TYPE_SIZE 16
#define INT_TYPE_SIZE 32
#define LONG_LONG_TYPE_SIZE 64
#define POINTER_SIZE (riscv_abi >= ABI_LP64 ? 64 : 32)
#define LONG_TYPE_SIZE POINTER_SIZE

#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 64
#define LONG_DOUBLE_TYPE_SIZE 128

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY BITS_PER_WORD

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY (TARGET_RVC ? 16 : 32)

/* There is no point aligning anything to a rounder boundary than this.  */
#define BIGGEST_ALIGNMENT 128

/* The user-level ISA permits unaligned accesses, but they are not required
   of the privileged architecture.  */
#define STRICT_ALIGNMENT TARGET_STRICT_ALIGN

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

/* If defined, a C expression to compute the alignment for a static
   variable.  TYPE is the data type, and ALIGN is the alignment that
   the object would ordinarily have.  The value of this macro is used
   instead of that alignment to align the object.

   If this macro is not defined, then ALIGN is used.

   One use of this macro is to increase alignment of medium-size
   data to make it all fit in fewer cache lines.  Another is to
   cause character arrays to be word-aligned so that `strcpy' calls
   that copy constants to character arrays can be done inline.  */

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

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS 1

/* When in 64-bit mode, move insns will sign extend SImode and CCmode
   moves.  All other references are zero extended.  */
#define LOAD_EXTEND_OP(MODE) \
  (TARGET_64BIT && (MODE) == SImode ? SIGN_EXTEND : ZERO_EXTEND)

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases,
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.  */

#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)	\
  if (GET_MODE_CLASS (MODE) == MODE_INT		\
      && GET_MODE_SIZE (MODE) < UNITS_PER_WORD)	\
    {						\
      if ((MODE) == SImode)			\
	(UNSIGNEDP) = 0;			\
      (MODE) = word_mode;			\
    }

/* Pmode is always the same as ptr_mode, but not always the same as word_mode.
   Extensions of pointers to word_mode must be signed.  */
#define POINTERS_EXTEND_UNSIGNED false

/* Define if loading short immediate values into registers sign extends.  */
#define SHORT_IMMEDIATES_SIGN_EXTEND 1

/* Standard register usage.  */

/* Number of hardware registers.  We have:

   - 32 integer registers
   - 32 floating point registers
   - 2 fake registers:
	- ARG_POINTER_REGNUM
	- FRAME_POINTER_REGNUM */

#define FIRST_PSEUDO_REGISTER 66

/* x0, sp, gp, and tp are fixed.  */

#define FIXED_REGISTERS							\
{ /* General registers.  */						\
  1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  /* Floating-point registers.  */					\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  /* Others.  */							\
  1, 1									\
}

/* a0-a7, t0-a6, fa0-fa7, and ft0-ft11 are volatile across calls.
   The call RTLs themselves clobber ra.  */

#define CALL_USED_REGISTERS						\
{ /* General registers.  */						\
  1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1,			\
  1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,			\
  /* Floating-point registers.  */					\
  1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1,			\
  1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,			\
  /* Others.  */							\
  1, 1									\
}

/* Internal macros to classify an ISA register's type.  */

#define GP_REG_FIRST 0
#define GP_REG_LAST  31
#define GP_REG_NUM   (GP_REG_LAST - GP_REG_FIRST + 1)

#define FP_REG_FIRST 32
#define FP_REG_LAST  63
#define FP_REG_NUM   (FP_REG_LAST - FP_REG_FIRST + 1)

/* The DWARF 2 CFA column which tracks the return address from a
   signal handler context.  This means that to maintain backwards
   compatibility, no hard register can be assigned this column if it
   would need to be handled by the DWARF unwinder.  */
#define DWARF_ALT_FRAME_RETURN_COLUMN 64

#define GP_REG_P(REGNO)	\
  ((unsigned int) ((int) (REGNO) - GP_REG_FIRST) < GP_REG_NUM)
#define FP_REG_P(REGNO)  \
  ((unsigned int) ((int) (REGNO) - FP_REG_FIRST) < FP_REG_NUM)

#define FP_REG_RTX_P(X) (REG_P (X) && FP_REG_P (REGNO (X)))

/* Use s0 as the frame pointer if it is so requested.  */
#define HARD_FRAME_POINTER_REGNUM 8
#define STACK_POINTER_REGNUM 2
#define THREAD_POINTER_REGNUM 4

/* These two registers don't really exist: they get eliminated to either
   the stack or hard frame pointer.  */
#define ARG_POINTER_REGNUM 64
#define FRAME_POINTER_REGNUM 65

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM (GP_TEMP_FIRST + 2)

/* Registers used as temporaries in prologue/epilogue code.

   The prologue registers mustn't conflict with any
   incoming arguments, the static chain pointer, or the frame pointer.
   The epilogue temporary mustn't conflict with the return registers,
   the frame pointer, the EH stack adjustment, or the EH data registers. */

#define RISCV_PROLOGUE_TEMP_REGNUM (GP_TEMP_FIRST + 1)
#define RISCV_PROLOGUE_TEMP(MODE) gen_rtx_REG (MODE, RISCV_PROLOGUE_TEMP_REGNUM)

#define MCOUNT_NAME "_mcount"

#define NO_PROFILE_COUNTERS 1

/* Emit rtl for profiling.  Output assembler code to FILE
   to call "_mcount" for profiling a function entry.  */
#define PROFILE_HOOK(LABEL)						\
  {									\
    rtx fun, ra;							\
    ra = get_hard_reg_initial_val (Pmode, RETURN_ADDR_REGNUM);		\
    fun = gen_rtx_SYMBOL_REF (Pmode, MCOUNT_NAME);			\
    emit_library_call (fun, LCT_NORMAL, VOIDmode, ra, Pmode);		\
  }

/* All the work done in PROFILE_HOOK, but still required.  */
#define FUNCTION_PROFILER(STREAM, LABELNO) do { } while (0)

/* Define this macro if it is as good or better to call a constant
   function address than to call an address kept in a register.  */
#define NO_FUNCTION_CSE 1

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
  SIBCALL_REGS,			/* registers used by indirect sibcalls */
  JALR_REGS,			/* registers used by indirect calls */
  GR_REGS,			/* integer registers */
  FP_REGS,			/* floating-point registers */
  FRAME_REGS,			/* arg pointer and frame pointer */
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
  "SIBCALL_REGS",							\
  "JALR_REGS",								\
  "GR_REGS",								\
  "FP_REGS",								\
  "FRAME_REGS",								\
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

#define REG_CLASS_CONTENTS						\
{									\
  { 0x00000000, 0x00000000, 0x00000000 },	/* NO_REGS */		\
  { 0xf00000c0, 0x00000000, 0x00000000 },	/* SIBCALL_REGS */	\
  { 0xffffffc0, 0x00000000, 0x00000000 },	/* JALR_REGS */		\
  { 0xffffffff, 0x00000000, 0x00000000 },	/* GR_REGS */		\
  { 0x00000000, 0xffffffff, 0x00000000 },	/* FP_REGS */		\
  { 0x00000000, 0x00000000, 0x00000003 },	/* FRAME_REGS */	\
  { 0xffffffff, 0xffffffff, 0x00000003 }	/* ALL_REGS */		\
}

/* A C expression whose value is a register class containing hard
   register REGNO.  In general there is more that one such class;
   choose a class which is "minimal", meaning that no smaller class
   also contains the register.  */

#define REGNO_REG_CLASS(REGNO) riscv_regno_to_class[ (REGNO) ]

/* A macro whose definition is the name of the class to which a
   valid base register must belong.  A base register is one used in
   an address which is the register value plus a displacement.  */

#define BASE_REG_CLASS GR_REGS

/* A macro whose definition is the name of the class to which a
   valid index register must belong.  An index register is one used
   in an address where its value is either multiplied by a scale
   factor or added to another register (as well as added to a
   displacement).  */

#define INDEX_REG_CLASS NO_REGS

/* We generally want to put call-clobbered registers ahead of
   call-saved ones.  (IRA expects this.)  */

#define REG_ALLOC_ORDER							\
{ \
  /* Call-clobbered GPRs.  */						\
  15, 14, 13, 12, 11, 10, 16, 17, 6, 28, 29, 30, 31, 5, 7, 1,		\
  /* Call-saved GPRs.  */						\
  8, 9, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27,	       			\
  /* GPRs that can never be exposed to the register allocator.  */	\
  0, 2, 3, 4,								\
  /* Call-clobbered FPRs.  */						\
  47, 46, 45, 44, 43, 42, 32, 33, 34, 35, 36, 37, 38, 39, 48, 49,	\
  60, 61, 62, 63,							\
  /* Call-saved FPRs.  */						\
  40, 41, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,			\
  /* None of the remaining classes have defined call-saved		\
     registers.  */							\
  64, 65								\
}

/* True if VALUE is a signed 12-bit number.  */

#define SMALL_OPERAND(VALUE) \
  ((unsigned HOST_WIDE_INT) (VALUE) + IMM_REACH/2 < IMM_REACH)

/* True if VALUE can be loaded into a register using LUI.  */

#define LUI_OPERAND(VALUE)						\
  (((VALUE) | ((1UL<<31) - IMM_REACH)) == ((1UL<<31) - IMM_REACH)	\
   || ((VALUE) | ((1UL<<31) - IMM_REACH)) + IMM_REACH == 0)

/* Stack layout; function entry, exit and calling.  */

#define STACK_GROWS_DOWNWARD 1

#define FRAME_GROWS_DOWNWARD 1

#define RETURN_ADDR_RTX riscv_return_addr

#define ELIMINABLE_REGS							\
{{ ARG_POINTER_REGNUM,   STACK_POINTER_REGNUM},				\
 { ARG_POINTER_REGNUM,   HARD_FRAME_POINTER_REGNUM},			\
 { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},				\
 { FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM}}				\

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  (OFFSET) = riscv_initial_elimination_offset (FROM, TO)

/* Allocate stack space for arguments at the beginning of each function.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* The argument pointer always points to the first argument.  */
#define FIRST_PARM_OFFSET(FNDECL) 0

#define REG_PARM_STACK_SPACE(FNDECL) 0

/* Define this if it is the responsibility of the caller to
   allocate the area reserved for arguments passed in registers.
   If `ACCUMULATE_OUTGOING_ARGS' is also defined, the only effect
   of this macro is to determine whether the space is included in
   `crtl->outgoing_args_size'.  */
#define OUTGOING_REG_PARM_STACK_SPACE(FNTYPE) 1

#define STACK_BOUNDARY 128

/* Symbolic macros for the registers used to return integer and floating
   point values.  */

#define GP_RETURN GP_ARG_FIRST
#define FP_RETURN (UNITS_PER_FP_ARG == 0 ? GP_RETURN : FP_ARG_FIRST)

#define MAX_ARGS_IN_REGISTERS 8

/* Symbolic macros for the first/last argument registers.  */

#define GP_ARG_FIRST (GP_REG_FIRST + 10)
#define GP_ARG_LAST  (GP_ARG_FIRST + MAX_ARGS_IN_REGISTERS - 1)
#define GP_TEMP_FIRST (GP_REG_FIRST + 5)
#define FP_ARG_FIRST (FP_REG_FIRST + 10)
#define FP_ARG_LAST  (FP_ARG_FIRST + MAX_ARGS_IN_REGISTERS - 1)

#define CALLEE_SAVED_REG_NUMBER(REGNO)			\
  ((REGNO) >= 8 && (REGNO) <= 9 ? (REGNO) - 8 :		\
   (REGNO) >= 18 && (REGNO) <= 27 ? (REGNO) - 16 : -1)

#define LIBCALL_VALUE(MODE) \
  riscv_function_value (NULL_TREE, NULL_TREE, MODE)

#define FUNCTION_VALUE(VALTYPE, FUNC) \
  riscv_function_value (VALTYPE, FUNC, VOIDmode)

#define FUNCTION_VALUE_REGNO_P(N) ((N) == GP_RETURN || (N) == FP_RETURN)

/* 1 if N is a possible register number for function argument passing.
   We have no FP argument registers when soft-float.  When FP registers
   are 32 bits, we can't directly reference the odd numbered ones.  */

/* Accept arguments in a0-a7, and in fa0-fa7 if permitted by the ABI.  */
#define FUNCTION_ARG_REGNO_P(N)						\
  (IN_RANGE ((N), GP_ARG_FIRST, GP_ARG_LAST)				\
   || (UNITS_PER_FP_ARG && IN_RANGE ((N), FP_ARG_FIRST, FP_ARG_LAST)))

typedef struct {
  /* Number of integer registers used so far, up to MAX_ARGS_IN_REGISTERS. */
  unsigned int num_gprs;

  /* Number of floating-point registers used so far, likewise.  */
  unsigned int num_fprs;
} CUMULATIVE_ARGS;

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
  memset (&(CUM), 0, sizeof (CUM))

#define EPILOGUE_USES(REGNO)	((REGNO) == RETURN_ADDR_REGNUM)

/* ABI requires 16-byte alignment, even on RV32. */
#define RISCV_STACK_ALIGN(LOC) (((LOC) + 15) & -16)

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 1


/* Trampolines are a block of code followed by two pointers.  */

#define TRAMPOLINE_CODE_SIZE 16
#define TRAMPOLINE_SIZE		\
  ((Pmode == SImode)		\
   ? TRAMPOLINE_CODE_SIZE	\
   : (TRAMPOLINE_CODE_SIZE + POINTER_SIZE * 2))
#define TRAMPOLINE_ALIGNMENT POINTER_SIZE

/* Addressing modes, and classification of registers for them.  */

#define REGNO_OK_FOR_INDEX_P(REGNO) 0
#define REGNO_MODE_OK_FOR_BASE_P(REGNO, MODE) \
  riscv_regno_mode_ok_for_base_p (REGNO, MODE, 1)

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx
   and check its validity for a certain class.
   We have two alternate definitions for each of them.
   The usual definition accepts all pseudo regs; the other rejects them all.
   The symbol REG_OK_STRICT causes the latter definition to be used.

   Most source files want to accept pseudo regs in the hope that
   they will get allocated to the class that the insn wants them to be in.
   Some source files that are used after register allocation
   need to be strict.  */

#ifndef REG_OK_STRICT
#define REG_MODE_OK_FOR_BASE_P(X, MODE) \
  riscv_regno_mode_ok_for_base_p (REGNO (X), MODE, 0)
#else
#define REG_MODE_OK_FOR_BASE_P(X, MODE) \
  riscv_regno_mode_ok_for_base_p (REGNO (X), MODE, 1)
#endif

#define REG_OK_FOR_INDEX_P(X) 0

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 1

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

#define JUMP_TABLES_IN_TEXT_SECTION 0
#define CASE_VECTOR_MODE SImode
#define CASE_VECTOR_PC_RELATIVE (riscv_cmodel != CM_MEDLOW)

/* The load-address macro is used for PC-relative addressing of symbols
   that bind locally.  Don't use it for symbols that should be addressed
   via the GOT.  Also, avoid it for CM_MEDLOW, where LUI addressing
   currently results in more opportunities for linker relaxation.  */
#define USE_LOAD_ADDRESS_MACRO(sym)					\
  (!TARGET_EXPLICIT_RELOCS &&						\
   ((flag_pic								\
     && ((SYMBOL_REF_P (sym) && SYMBOL_REF_LOCAL_P (sym))		\
	 || ((GET_CODE (sym) == CONST)					\
	     && SYMBOL_REF_P (XEXP (XEXP (sym, 0),0))			\
	     && SYMBOL_REF_LOCAL_P (XEXP (XEXP (sym, 0),0)))))		\
     || riscv_cmodel == CM_MEDANY))

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 0

#define MOVE_MAX UNITS_PER_WORD
#define MAX_MOVE_MAX 8

/* The SPARC port says:
   Nonzero if access to memory by bytes is slow and undesirable.
   For RISC chips, it means that access to memory by bytes is no
   better than access by words when possible, so grab a whole word
   and maybe make use of that.  */
#define SLOW_BYTE_ACCESS 1

#define SHIFT_COUNT_TRUNCATED 1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */

#define Pmode word_mode

/* Give call MEMs SImode since it is the "most permissive" mode
   for both 32-bit and 64-bit targets.  */

#define FUNCTION_MODE SImode

/* A C expression for the cost of a branch instruction.  A value of 2
   seems to minimize code size.  */

#define BRANCH_COST(speed_p, predictable_p) \
  ((!(speed_p) || (predictable_p)) ? 2 : riscv_branch_cost)

#define LOGICAL_OP_NON_SHORT_CIRCUIT 0

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

#define REGISTER_NAMES						\
{ "zero","ra",  "sp",  "gp",  "tp",  "t0",  "t1",  "t2",	\
  "s0",  "s1",  "a0",  "a1",  "a2",  "a3",  "a4",  "a5",	\
  "a6",  "a7",  "s2",  "s3",  "s4",  "s5",  "s6",  "s7",	\
  "s8",  "s9",  "s10", "s11", "t3",  "t4",  "t5",  "t6",	\
  "ft0", "ft1", "ft2", "ft3", "ft4", "ft5", "ft6", "ft7",	\
  "fs0", "fs1", "fa0", "fa1", "fa2", "fa3", "fa4", "fa5",	\
  "fa6", "fa7", "fs2", "fs3", "fs4", "fs5", "fs6", "fs7",	\
  "fs8", "fs9", "fs10","fs11","ft8", "ft9", "ft10","ft11",	\
  "arg", "frame", }

#define ADDITIONAL_REGISTER_NAMES					\
{									\
  { "x0",	 0 + GP_REG_FIRST },					\
  { "x1",	 1 + GP_REG_FIRST },					\
  { "x2",	 2 + GP_REG_FIRST },					\
  { "x3",	 3 + GP_REG_FIRST },					\
  { "x4",	 4 + GP_REG_FIRST },					\
  { "x5",	 5 + GP_REG_FIRST },					\
  { "x6",	 6 + GP_REG_FIRST },					\
  { "x7",	 7 + GP_REG_FIRST },					\
  { "x8",	 8 + GP_REG_FIRST },					\
  { "x9",	 9 + GP_REG_FIRST },					\
  { "x10",	10 + GP_REG_FIRST },					\
  { "x11",	11 + GP_REG_FIRST },					\
  { "x12",	12 + GP_REG_FIRST },					\
  { "x13",	13 + GP_REG_FIRST },					\
  { "x14",	14 + GP_REG_FIRST },					\
  { "x15",	15 + GP_REG_FIRST },					\
  { "x16",	16 + GP_REG_FIRST },					\
  { "x17",	17 + GP_REG_FIRST },					\
  { "x18",	18 + GP_REG_FIRST },					\
  { "x19",	19 + GP_REG_FIRST },					\
  { "x20",	20 + GP_REG_FIRST },					\
  { "x21",	21 + GP_REG_FIRST },					\
  { "x22",	22 + GP_REG_FIRST },					\
  { "x23",	23 + GP_REG_FIRST },					\
  { "x24",	24 + GP_REG_FIRST },					\
  { "x25",	25 + GP_REG_FIRST },					\
  { "x26",	26 + GP_REG_FIRST },					\
  { "x27",	27 + GP_REG_FIRST },					\
  { "x28",	28 + GP_REG_FIRST },					\
  { "x29",	29 + GP_REG_FIRST },					\
  { "x30",	30 + GP_REG_FIRST },					\
  { "x31",	31 + GP_REG_FIRST },					\
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
  { "f16",	16 + FP_REG_FIRST },					\
  { "f17",	17 + FP_REG_FIRST },					\
  { "f18",	18 + FP_REG_FIRST },					\
  { "f19",	19 + FP_REG_FIRST },					\
  { "f20",	20 + FP_REG_FIRST },					\
  { "f21",	21 + FP_REG_FIRST },					\
  { "f22",	22 + FP_REG_FIRST },					\
  { "f23",	23 + FP_REG_FIRST },					\
  { "f24",	24 + FP_REG_FIRST },					\
  { "f25",	25 + FP_REG_FIRST },					\
  { "f26",	26 + FP_REG_FIRST },					\
  { "f27",	27 + FP_REG_FIRST },					\
  { "f28",	28 + FP_REG_FIRST },					\
  { "f29",	29 + FP_REG_FIRST },					\
  { "f30",	30 + FP_REG_FIRST },					\
  { "f31",	31 + FP_REG_FIRST },					\
}

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.globl\t"

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)			\
  sprintf ((LABEL), "*%s%s%ld", (LOCAL_LABEL_PREFIX), (PREFIX), (long)(NUM))

/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE)				\
  fprintf (STREAM, "\t.word\t%sL%d\n", LOCAL_LABEL_PREFIX, VALUE)

/* This is how to output an element of a PIC case-vector. */

#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL)		\
  fprintf (STREAM, "\t.word\t%sL%d-%sL%d\n",				\
	   LOCAL_LABEL_PREFIX, VALUE, LOCAL_LABEL_PREFIX, REL)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(STREAM,LOG)					\
  fprintf (STREAM, "\t.align\t%d\n", (LOG))

/* Define the strings to put out for each section in the object file.  */
#define TEXT_SECTION_ASM_OP	"\t.text"	/* instructions */
#define DATA_SECTION_ASM_OP	"\t.data"	/* large data */
#define READONLY_DATA_SECTION_ASM_OP	"\t.section\t.rodata"
#define BSS_SECTION_ASM_OP	"\t.bss"
#define SBSS_SECTION_ASM_OP	"\t.section\t.sbss,\"aw\",@nobits"
#define SDATA_SECTION_ASM_OP	"\t.section\t.sdata,\"aw\",@progbits"

#define ASM_OUTPUT_REG_PUSH(STREAM,REGNO)				\
do									\
  {									\
    fprintf (STREAM, "\taddi\t%s,%s,-8\n\t%s\t%s,0(%s)\n",		\
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
    fprintf (STREAM, "\t%s\t%s,0(%s)\n\taddi\t%s,%s,8\n",		\
	     TARGET_64BIT ? "ld" : "lw",				\
	     reg_names[REGNO],						\
	     reg_names[STACK_POINTER_REGNUM],				\
	     reg_names[STACK_POINTER_REGNUM],				\
	     reg_names[STACK_POINTER_REGNUM]);				\
  }									\
while (0)

#define ASM_COMMENT_START "#"

#undef SIZE_TYPE
#define SIZE_TYPE (POINTER_SIZE == 64 ? "long unsigned int" : "unsigned int")

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE (POINTER_SIZE == 64 ? "long int" : "int")

/* The maximum number of bytes copied by one iteration of a movmemsi loop.  */

#define RISCV_MAX_MOVE_BYTES_PER_LOOP_ITER (UNITS_PER_WORD * 4)

/* The maximum number of bytes that can be copied by a straight-line
   movmemsi implementation.  */

#define RISCV_MAX_MOVE_BYTES_STRAIGHT (RISCV_MAX_MOVE_BYTES_PER_LOOP_ITER * 3)

/* If a memory-to-memory move would take MOVE_RATIO or more simple
   move-instruction pairs, we will do a movmem or libcall instead.
   Do not use move_by_pieces at all when strict alignment is not
   in effect but the target has slow unaligned accesses; in this
   case, movmem or libcall is more efficient.  */

#define MOVE_RATIO(speed)						\
  (!STRICT_ALIGNMENT && riscv_slow_unaligned_access ? 1 :		\
   (speed) ? RISCV_MAX_MOVE_BYTES_PER_LOOP_ITER / UNITS_PER_WORD :	\
   CLEAR_RATIO (speed) / 2)

/* For CLEAR_RATIO, when optimizing for size, give a better estimate
   of the length of a memset call, but use the default otherwise.  */

#define CLEAR_RATIO(speed) ((speed) ? 16 : 6)

/* This is similar to CLEAR_RATIO, but for a non-zero constant, so when
   optimizing for size adjust the ratio to account for the overhead of
   loading the constant and replicating it across the word.  */

#define SET_RATIO(speed) (CLEAR_RATIO (speed) - ((speed) ? 0 : 2))

#ifndef USED_FOR_TARGET
extern const enum reg_class riscv_regno_to_class[];
#endif

#define ASM_PREFERRED_EH_DATA_FORMAT(CODE,GLOBAL) \
  (((GLOBAL) ? DW_EH_PE_indirect : 0) | DW_EH_PE_pcrel | DW_EH_PE_sdata4)

#define XLEN_SPEC \
  "%{march=rv32*:32}" \
  "%{march=rv64*:64}" \

#define ABI_SPEC \
  "%{mabi=ilp32:ilp32}" \
  "%{mabi=ilp32f:ilp32f}" \
  "%{mabi=ilp32d:ilp32d}" \
  "%{mabi=lp64:lp64}" \
  "%{mabi=lp64f:lp64f}" \
  "%{mabi=lp64d:lp64d}" \

#define STARTFILE_PREFIX_SPEC 			\
   "/lib" XLEN_SPEC "/" ABI_SPEC "/ "		\
   "/usr/lib" XLEN_SPEC "/" ABI_SPEC "/ "	\
   "/lib/ "					\
   "/usr/lib/ "

/* ISA constants needed for code generation.  */
#define OPCODE_LW    0x2003
#define OPCODE_LD    0x3003
#define OPCODE_AUIPC 0x17
#define OPCODE_JALR  0x67
#define OPCODE_LUI   0x37
#define OPCODE_ADDI  0x13
#define SHIFT_RD  7
#define SHIFT_RS1 15
#define SHIFT_IMM 20
#define IMM_BITS 12

#define IMM_REACH (1LL << IMM_BITS)
#define CONST_HIGH_PART(VALUE) (((VALUE) + (IMM_REACH/2)) & ~(IMM_REACH-1))
#define CONST_LOW_PART(VALUE) ((VALUE) - CONST_HIGH_PART (VALUE))

#endif /* ! GCC_RISCV_H */
