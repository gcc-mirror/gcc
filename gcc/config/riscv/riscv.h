/* Definition of RISC-V target for GNU compiler.
   Copyright (C) 2011-2024 Free Software Foundation, Inc.
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

#include <stdbool.h>
#include "config/riscv/riscv-opts.h"

#define SWITCHABLE_TARGET 1

/* Target CPU builtins.  */
#define TARGET_CPU_CPP_BUILTINS() riscv_cpu_cpp_builtins (pfile)

#ifdef TARGET_BIG_ENDIAN_DEFAULT
#define DEFAULT_ENDIAN_SPEC    "b"
#else
#define DEFAULT_ENDIAN_SPEC    "l"
#endif

/* Default target_flags if no switches are specified  */

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT 0
#endif

#ifndef RISCV_TUNE_STRING_DEFAULT
#define RISCV_TUNE_STRING_DEFAULT "rocket"
#endif

extern const char *riscv_expand_arch (int argc, const char **argv);
extern const char *riscv_expand_arch_from_cpu (int argc, const char **argv);
extern const char *riscv_default_mtune (int argc, const char **argv);
extern const char *riscv_multi_lib_check (int argc, const char **argv);
extern const char *riscv_arch_help (int argc, const char **argv);

# define EXTRA_SPEC_FUNCTIONS						\
  { "riscv_expand_arch", riscv_expand_arch },				\
  { "riscv_expand_arch_from_cpu", riscv_expand_arch_from_cpu },		\
  { "riscv_default_mtune", riscv_default_mtune },			\
  { "riscv_multi_lib_check", riscv_multi_lib_check },			\
  { "riscv_arch_help", riscv_arch_help },

/* Support for a compile-time default CPU, et cetera.  The rules are:
   --with-arch is ignored if -march or -mcpu is specified.
   --with-abi is ignored if -mabi is specified.
   --with-tune is ignored if -mtune or -mcpu is specified.
   --with-isa-spec is ignored if -misa-spec is specified.
   --with-tls is ignored if -mtls-dialect is specified.

   But using default -march/-mtune value if -mcpu don't have valid option.  */
#define OPTION_DEFAULT_SPECS \
  {"tune", "%{!mtune=*:"						\
	   "  %{!mcpu=*:-mtune=%(VALUE)}"				\
	   "  %{mcpu=*:-mtune=%:riscv_default_mtune(%* %(VALUE))}}" },	\
  {"arch", "%{!march=*:"						\
	   "  %{!mcpu=*:-march=%(VALUE)}"				\
	   "  %{mcpu=*:%:riscv_expand_arch_from_cpu(%* %(VALUE))}}" },	\
  {"abi", "%{!mabi=*:-mabi=%(VALUE)}" },				\
  {"isa_spec", "%{!misa-spec=*:-misa-spec=%(VALUE)}" },			\
  {"tls", "%{!mtls-dialect=*:-mtls-dialect=%(VALUE)}"},         	\

#ifdef IN_LIBGCC2
#undef TARGET_64BIT
/* Make this compile time constant for libgcc2 */
#define TARGET_64BIT           (__riscv_xlen == 64)
#endif /* IN_LIBGCC2 */

#ifdef HAVE_AS_MISA_SPEC
#define ASM_MISA_SPEC "%{misa-spec=*}"
#else
#define ASM_MISA_SPEC ""
#endif

/* Reference:
     https://gcc.gnu.org/onlinedocs/cpp/Stringizing.html#Stringizing  */
#define STRINGIZING(s) __STRINGIZING(s)
#define __STRINGIZING(s) #s

#define MULTILIB_DEFAULTS \
  {"march=" STRINGIZING (TARGET_RISCV_DEFAULT_ARCH), \
   "mabi=" STRINGIZING (TARGET_RISCV_DEFAULT_ABI) }

#undef ASM_SPEC
#define ASM_SPEC "\
%(subtarget_asm_debugging_spec) \
%{" FPIE_OR_FPIC_SPEC ":-fpic} \
%{march=*} \
%{mabi=*} \
%{mno-relax} \
%{mbig-endian} \
%{mlittle-endian} \
%(subtarget_asm_spec)" \
ASM_MISA_SPEC

#undef DRIVER_SELF_SPECS
#define DRIVER_SELF_SPECS					\
"%{march=help:%:riscv_arch_help()} "				\
"%{print-supported-extensions:%:riscv_arch_help()} "		\
"%{-print-supported-extensions:%:riscv_arch_help()} "		\
"%{march=*:%:riscv_expand_arch(%*)} "				\
"%{!march=*:%{mcpu=*:%:riscv_expand_arch_from_cpu(%*)}} "

#define TARGET_DEFAULT_CMODEL CM_MEDLOW

#define LOCAL_LABEL_PREFIX	"."
#define USER_LABEL_PREFIX	""

/* Offsets recorded in opcodes are a multiple of this alignment factor.
   The default for this in 64-bit mode is 8, which causes problems with
   SFmode register saves.  */
#define DWARF_CIE_DATA_ALIGNMENT -4

/* The mapping from gcc register number to DWARF 2 CFA column number.  */
#define DWARF_FRAME_REGNUM(REGNO)                                              \
  (FRM_REG_P (REGNO)	? RISCV_DWARF_FRM                                      \
   : VXRM_REG_P (REGNO) ? RISCV_DWARF_VXRM                                     \
   : VL_REG_P (REGNO)	? RISCV_DWARF_VL                                       \
   : VTYPE_REG_P (REGNO)                                                       \
     ? RISCV_DWARF_VTYPE                                                       \
     : (GP_REG_P (REGNO) || FP_REG_P (REGNO) || V_REG_P (REGNO)                \
	  ? REGNO                                                              \
	  : INVALID_REGNUM))

/* The DWARF 2 CFA column which tracks the return address.  */
#define DWARF_FRAME_RETURN_COLUMN RETURN_ADDR_REGNUM
#define INCOMING_RETURN_ADDR_RTX gen_rtx_REG (VOIDmode, RETURN_ADDR_REGNUM)

/* Describe how we implement __builtin_eh_return.  */
#define EH_RETURN_DATA_REGNO(N) \
  ((N) < 4 ? (N) + GP_ARG_FIRST : INVALID_REGNUM)

#define EH_RETURN_STACKADJ_RTX  gen_rtx_REG (Pmode, GP_ARG_FIRST + 4)

/* Target machine storage layout */

#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN (TARGET_BIG_ENDIAN != 0)
#define WORDS_BIG_ENDIAN (BYTES_BIG_ENDIAN)

#define MAX_BITS_PER_WORD 64

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD (TARGET_64BIT ? 8 : 4)
#define BITS_PER_WORD (BITS_PER_UNIT * UNITS_PER_WORD)
#ifndef IN_LIBGCC2
#define MIN_UNITS_PER_WORD 4
#endif

/* Allows SImode op in builtin overflow pattern, see internal-fn.cc.  */
#undef TARGET_MIN_ARITHMETIC_PRECISION
#define TARGET_MIN_ARITHMETIC_PRECISION riscv_min_arithmetic_precision

/* The `Q' extension is not yet supported.  */
#define UNITS_PER_FP_REG (TARGET_DOUBLE_FLOAT ? 8 : 4)
/* Size per vector register. For VLEN = 32, size = poly (4, 4). Otherwise, size = poly (8, 8). */
#define UNITS_PER_V_REG (riscv_vector_chunks * riscv_bytes_per_vector_chunk)

/* The largest type that can be passed in floating-point registers.  */
#define UNITS_PER_FP_ARG						\
  ((riscv_abi == ABI_ILP32 || riscv_abi == ABI_ILP32E			\
    || riscv_abi == ABI_LP64 || riscv_abi == ABI_LP64E)			\
   ? 0 									\
   : ((riscv_abi == ABI_ILP32F || riscv_abi == ABI_LP64F) ? 4 : 8))

/* Set the sizes of the core types.  */
#define SHORT_TYPE_SIZE 16
#define INT_TYPE_SIZE 32
#define LONG_LONG_TYPE_SIZE 64
#define POINTER_SIZE (riscv_abi >= ABI_LP64 ? 64 : 32)
#define LONG_TYPE_SIZE POINTER_SIZE

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY BITS_PER_WORD

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY ((TARGET_RVC || TARGET_ZCA) ? 16 : 32)

/* The smallest supported stack boundary the calling convention supports.  */
#define STACK_BOUNDARY \
  (riscv_abi == ABI_ILP32E || riscv_abi == ABI_LP64E \
   ? BITS_PER_WORD \
   : 2 * BITS_PER_WORD)

/* The ABI stack alignment.  */
#define ABI_STACK_BOUNDARY \
  (riscv_abi == ABI_ILP32E || riscv_abi == ABI_LP64E \
   ? BITS_PER_WORD \
   : 128)

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

/* An integer expression for the size in bits of the largest integer machine
   mode that should actually be used.  We allow pairs of registers.  */
#define MAX_FIXED_MODE_SIZE GET_MODE_BITSIZE (TARGET_64BIT ? TImode : DImode)

/* DATA_ALIGNMENT and LOCAL_ALIGNMENT common definition.  */
#define RISCV_EXPAND_ALIGNMENT(COND, TYPE, ALIGN)			\
  (((COND) && ((ALIGN) < BITS_PER_WORD)					\
    && (TREE_CODE (TYPE) == ARRAY_TYPE					\
	|| TREE_CODE (TYPE) == UNION_TYPE				\
	|| TREE_CODE (TYPE) == RECORD_TYPE)) ? BITS_PER_WORD : (ALIGN))

/* If defined, a C expression to compute the alignment for a static
   variable.  TYPE is the data type, and ALIGN is the alignment that
   the object would ordinarily have.  The value of this macro is used
   instead of that alignment to align the object.

   If this macro is not defined, then ALIGN is used.

   One use of this macro is to increase alignment of medium-size
   data to make it all fit in fewer cache lines.  Another is to
   cause character arrays to be word-aligned so that `strcpy' calls
   that copy constants to character arrays can be done inline.  */

#define DATA_ALIGNMENT(TYPE, ALIGN)						\
  RISCV_EXPAND_ALIGNMENT (riscv_align_data_type == riscv_align_data_type_xlen,	\
			  TYPE, ALIGN)

/* We need this for the same reason as DATA_ALIGNMENT, namely to cause
   character arrays to be word-aligned so that `strcpy' calls that copy
   constants to character arrays can be done inline, and 'strcmp' can be
   optimised to use word loads. */
#define LOCAL_ALIGNMENT(TYPE, ALIGN) \
  RISCV_EXPAND_ALIGNMENT (true, TYPE, ALIGN)

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
	- FRAME_POINTER_REGNUM
   - 1 vl register
   - 1 vtype register
   - 28 unused registers for future expansion
   - 32 vector registers  */

#define FIRST_PSEUDO_REGISTER 128

/* x0, ra, sp, gp, and tp are fixed.  */

#define FIXED_REGISTERS							\
{ /* General registers.  */						\
  1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  /* Floating-point registers.  */					\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  /* Others.  */							\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,			\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,			\
  /* Vector registers.  */						\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0			\
}

/* a0-a7, t0-t6, fa0-fa7, and ft0-ft11 are volatile across calls.
   The call RTLs themselves clobber ra.  */

#define CALL_USED_REGISTERS						\
{ /* General registers.  */						\
  1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1,			\
  1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,			\
  /* Floating-point registers.  */					\
  1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1,			\
  1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,			\
  /* Others.  */							\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,			\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,			\
  /* Vector registers.  */						\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,			\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1			\
}

/* Select a register mode required for caller save of hard regno REGNO.
   Contrary to what is documented, the default is not the smallest suitable
   mode but the largest suitable mode for the given (REGNO, NREGS) pair and
   it quickly creates paradoxical subregs that can be problematic.  */
#define HARD_REGNO_CALLER_SAVE_MODE(REGNO, NREGS, MODE) \
  ((MODE) == VOIDmode ? choose_hard_reg_mode (REGNO, NREGS, NULL) : (MODE))

/* Internal macros to classify an ISA register's type.  */

#define GP_REG_FIRST 0
#define GP_REG_LAST  (TARGET_RVE ? 15 : 31)
#define GP_REG_NUM   (GP_REG_LAST - GP_REG_FIRST + 1)

#define FP_REG_FIRST 32
#define FP_REG_LAST  63
#define FP_REG_NUM   (FP_REG_LAST - FP_REG_FIRST + 1)

#define V_REG_FIRST 96
#define V_REG_LAST  127
#define V_REG_NUM   (V_REG_LAST - V_REG_FIRST + 1)

/* The DWARF 2 CFA column which tracks the return address from a
   signal handler context.  This means that to maintain backwards
   compatibility, no hard register can be assigned this column if it
   would need to be handled by the DWARF unwinder.  */
#define DWARF_ALT_FRAME_RETURN_COLUMN 64

#define GP_REG_P(REGNO)	\
  ((unsigned int) ((int) (REGNO) - GP_REG_FIRST) < GP_REG_NUM)
#define FP_REG_P(REGNO)  \
  ((unsigned int) ((int) (REGNO) - FP_REG_FIRST) < FP_REG_NUM)
#define HARDFP_REG_P(REGNO)  \
  ((REGNO) >= FP_REG_FIRST && (REGNO) <= FP_REG_LAST)
#define V_REG_P(REGNO)  \
  ((unsigned int) ((int) (REGNO) - V_REG_FIRST) < V_REG_NUM)
#define VL_REG_P(REGNO) ((REGNO) == VL_REGNUM)
#define VTYPE_REG_P(REGNO) ((REGNO) == VTYPE_REGNUM)
#define VXRM_REG_P(REGNO) ((REGNO) == VXRM_REGNUM)
#define FRM_REG_P(REGNO) ((REGNO) == FRM_REGNUM)

/* True when REGNO is in SIBCALL_REGS set.  */
#define SIBCALL_REG_P(REGNO)	\
  TEST_HARD_REG_BIT (reg_class_contents[SIBCALL_REGS], REGNO)

#define FP_REG_RTX_P(X) (REG_P (X) && FP_REG_P (REGNO (X)))

/* Use s0 as the frame pointer if it is so requested.  */
#define HARD_FRAME_POINTER_REGNUM 8
#define STACK_POINTER_REGNUM 2
#define THREAD_POINTER_REGNUM 4

/* These two registers don't really exist: they get eliminated to either
   the stack or hard frame pointer.  */
#define ARG_POINTER_REGNUM 64
#define FRAME_POINTER_REGNUM 65

/* Define Dwarf for RVV.  */
#define RISCV_DWARF_FRM (4096 + 0x003)
#define RISCV_DWARF_VXRM (4096 + 0x00a)
#define RISCV_DWARF_VL (4096 + 0xc20)
#define RISCV_DWARF_VTYPE (4096 + 0xc21)
#define RISCV_DWARF_VLENB (4096 + 0xc22)

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM (GP_TEMP_FIRST + 2)

/* Registers used as temporaries in prologue/epilogue code.

   The prologue registers mustn't conflict with any
   incoming arguments, the static chain pointer, or the frame pointer.
   The epilogue temporary mustn't conflict with the return registers,
   the frame pointer, the EH stack adjustment, or the EH data registers. */

#define RISCV_PROLOGUE_TEMP_REGNUM (GP_TEMP_FIRST)
#define RISCV_PROLOGUE_TEMP(MODE) gen_rtx_REG (MODE, RISCV_PROLOGUE_TEMP_REGNUM)
#define RISCV_PROLOGUE_TEMP2_REGNUM (GP_TEMP_FIRST + 1)
#define RISCV_PROLOGUE_TEMP2(MODE) gen_rtx_REG (MODE, RISCV_PROLOGUE_TEMP2_REGNUM)

/* Both prologue temp registers are used in the vector probe loop for when
   stack-clash protection is enabled, so we need to copy SP to a new register
   and set it as CFA during the loop, we are using T3 for that.  */
#define RISCV_STACK_CLASH_VECTOR_CFA_REGNUM (GP_TEMP_FIRST + 23)

#define RISCV_CALL_ADDRESS_TEMP_REGNUM (GP_TEMP_FIRST + 1)
#define RISCV_CALL_ADDRESS_TEMP(MODE) \
  gen_rtx_REG (MODE, RISCV_CALL_ADDRESS_TEMP_REGNUM)

#define RETURN_ADDR_MASK (1 << RETURN_ADDR_REGNUM)
#define S0_MASK (1 << S0_REGNUM)
#define S1_MASK (1 << S1_REGNUM)
#define S2_MASK (1 << S2_REGNUM)
#define S3_MASK (1 << S3_REGNUM)
#define S4_MASK (1 << S4_REGNUM)
#define S5_MASK (1 << S5_REGNUM)
#define S6_MASK (1 << S6_REGNUM)
#define S7_MASK (1 << S7_REGNUM)
#define S8_MASK (1 << S8_REGNUM)
#define S9_MASK (1 << S9_REGNUM)
#define S10_MASK (1 << S10_REGNUM)
#define S11_MASK (1 << S11_REGNUM)

#define MULTI_PUSH_GPR_MASK                                                    \
  (RETURN_ADDR_MASK | S0_MASK | S1_MASK | S2_MASK | S3_MASK | S4_MASK          \
   | S5_MASK | S6_MASK | S7_MASK | S8_MASK | S9_MASK | S10_MASK | S11_MASK)
#define ZCMP_MAX_SPIMM 3
#define ZCMP_SP_INC_STEP 16
#define ZCMP_INVALID_S0S10_SREGS_COUNTS 11
#define ZCMP_S0S11_SREGS_COUNTS 12
#define ZCMP_MAX_GRP_SLOTS 13

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
  VM_REGS,			/* v0.t registers */
  VD_REGS,			/* vector registers except v0.t */
  V_REGS,			/* vector registers */
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
  "VM_REGS",								\
  "VD_REGS",								\
  "V_REGS",								\
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
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000 },	/* NO_REGS */		\
  { 0xf003fcc0, 0x00000000, 0x00000000, 0x00000000 },	/* SIBCALL_REGS */	\
  { 0xffffffc0, 0x00000000, 0x00000000, 0x00000000 },	/* JALR_REGS */		\
  { 0xffffffff, 0x00000000, 0x00000000, 0x00000000 },	/* GR_REGS */		\
  { 0x00000000, 0xffffffff, 0x00000000, 0x00000000 },	/* FP_REGS */		\
  { 0x00000000, 0x00000000, 0x00000003, 0x00000000 },	/* FRAME_REGS */	\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000001 },	/* V0_REGS */		\
  { 0x00000000, 0x00000000, 0x00000000, 0xfffffffe },	/* VNoV0_REGS */	\
  { 0x00000000, 0x00000000, 0x00000000, 0xffffffff },	/* V_REGS */		\
  { 0xffffffff, 0xffffffff, 0x00000003, 0xffffffff }	/* ALL_REGS */		\
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

#define INDEX_REG_CLASS riscv_index_reg_class()

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
  /* v1 ~ v31 vector registers.  */					\
  97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110,	\
  111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123,	\
  124, 125, 126, 127,							\
  /* The vector mask register.  */					\
  96,									\
  /* None of the remaining classes have defined call-saved		\
     registers.  */							\
  64, 65, 66, 67							\
}

/* True if VALUE is a signed 12-bit number.  */

#define SMALL_OPERAND(VALUE) \
  ((unsigned HOST_WIDE_INT) (VALUE) + IMM_REACH/2 < IMM_REACH)

#define POLY_SMALL_OPERAND_P(POLY_VALUE)		\
  (POLY_VALUE.is_constant () ?				\
     SMALL_OPERAND (POLY_VALUE.to_constant ()) : false)

/* True if VALUE can be loaded into a register using LUI.  */

#define LUI_OPERAND(VALUE)						\
  (((VALUE) | ((1UL<<31) - IMM_REACH)) == ((1UL<<31) - IMM_REACH)	\
   || ((VALUE) | ((1UL<<31) - IMM_REACH)) + IMM_REACH == 0)

/* True if a VALUE (constant) can be expressed as sum of two S12 constants
   (in range -2048 to 2047).
   Range check logic:
     from: min S12 + 1 (or -1 depending on what side of zero)
       to: two times the min S12 value (to max out S12 bits).  */

#define SUM_OF_TWO_S12_N(VALUE)						\
  (((VALUE) >= (-2048 * 2)) && ((VALUE) <= (-2048 - 1)))

#define SUM_OF_TWO_S12_P(VALUE)						\
  (((VALUE) >= (2047 + 1)) && ((VALUE) <= (2047 * 2)))

#define SUM_OF_TWO_S12(VALUE)						\
  (SUM_OF_TWO_S12_N (VALUE) || SUM_OF_TWO_S12_P (VALUE))

/* Variant with first value 8 byte aligned if involving stack regs.  */
#define SUM_OF_TWO_S12_P_ALGN(VALUE)				\
  (((VALUE) >= (2032 + 1)) && ((VALUE) <= (2032 * 2)))

#define SUM_OF_TWO_S12_ALGN(VALUE)				\
  (SUM_OF_TWO_S12_N (VALUE) || SUM_OF_TWO_S12_P_ALGN (VALUE))

/* If this is a single bit mask, then we can load it with bseti.  Special
   handling of SImode 0x80000000 on RV64 is done in riscv_build_integer_1. */
#define SINGLE_BIT_MASK_OPERAND(VALUE)					\
  (pow2p_hwi (TARGET_64BIT						\
		? (VALUE)						\
		: ((VALUE) & ((HOST_WIDE_INT_1U << 32)-1))))

/* True if VALUE can be represented as an immediate with 1 extra bit
   set: we check that it is not a SMALL_OPERAND (as this would be true
   for all small operands) unmodified and turns into a small operand
   once we clear the top bit. */
#define UIMM_EXTRA_BIT_OPERAND(VALUE)					\
  (!SMALL_OPERAND (VALUE)						\
   && SMALL_OPERAND (VALUE & ~(HOST_WIDE_INT_1U << floor_log2 (VALUE))))

/* True if bit BIT is set in VALUE.  */
#define BITSET_P(VALUE, BIT) (((VALUE) & (1ULL << (BIT))) != 0)

/* Returns the smaller (common) number of trailing zeros for VAL1 and VAL2.  */
#define COMMON_TRAILING_ZEROS(VAL1, VAL2)				\
  (ctz_hwi (VAL1) < ctz_hwi (VAL2)					\
   ? ctz_hwi (VAL1)							\
   : ctz_hwi (VAL2))

/* Returns true if both VAL1 and VAL2 are SMALL_OPERANDs after shifting by
   the common number of trailing zeros.  */
#define SMALL_AFTER_COMMON_TRAILING_SHIFT(VAL1, VAL2)			\
  (SMALL_OPERAND ((VAL1) >> COMMON_TRAILING_ZEROS (VAL1, VAL2))		\
   && SMALL_OPERAND ((VAL2) >> COMMON_TRAILING_ZEROS (VAL1, VAL2)))

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

#define PREFERRED_STACK_BOUNDARY riscv_stack_boundary

/* Symbolic macros for the registers used to return integer and floating
   point values.  */

#define GP_RETURN GP_ARG_FIRST
#define FP_RETURN (UNITS_PER_FP_ARG == 0 ? GP_RETURN : FP_ARG_FIRST)
#define V_RETURN  V_REG_FIRST

#define GP_RETURN_FIRST GP_ARG_FIRST
#define GP_RETURN_LAST  GP_ARG_FIRST + 1
#define FP_RETURN_FIRST FP_RETURN
#define FP_RETURN_LAST  FP_RETURN + 1

#define MAX_ARGS_IN_REGISTERS \
  (riscv_abi == ABI_ILP32E || riscv_abi == ABI_LP64E \
   ? 6 \
   : 8)

#define MAX_ARGS_IN_VECTOR_REGISTERS (16)
#define MAX_ARGS_IN_MASK_REGISTERS (1)

/* Symbolic macros for the first/last argument registers.  */

#define GP_ARG_FIRST (GP_REG_FIRST + 10)
#define GP_ARG_LAST  (GP_ARG_FIRST + MAX_ARGS_IN_REGISTERS - 1)
#define GP_TEMP_FIRST (GP_REG_FIRST + 5)
#define FP_ARG_FIRST (FP_REG_FIRST + 10)
#define FP_ARG_LAST  (FP_ARG_FIRST + MAX_ARGS_IN_REGISTERS - 1)
#define V_ARG_FIRST (V_REG_FIRST + 8)
#define V_ARG_LAST (V_ARG_FIRST + MAX_ARGS_IN_VECTOR_REGISTERS - 1)

#define CALLEE_SAVED_REG_NUMBER(REGNO)			\
  ((REGNO) >= 8 && (REGNO) <= 9 ? (REGNO) - 8 :		\
   (REGNO) >= 18 && (REGNO) <= 27 ? (REGNO) - 16 : -1)

#define CALLEE_SAVED_FREG_NUMBER(REGNO) CALLEE_SAVED_REG_NUMBER (REGNO - 32)

#define LIBCALL_VALUE(MODE) \
  riscv_function_value (NULL_TREE, NULL_TREE, MODE)

#define FUNCTION_VALUE(VALTYPE, FUNC) \
  riscv_function_value (VALTYPE, FUNC, VOIDmode)

/* 1 if N is a possible register number for function argument passing.
   We have no FP argument registers when soft-float.  */

/* Accept arguments in a0-a7, and in fa0-fa7 if permitted by the ABI.  */
#define FUNCTION_ARG_REGNO_P(N)						\
  (IN_RANGE ((N), GP_ARG_FIRST, GP_ARG_LAST)				\
   || (UNITS_PER_FP_ARG && IN_RANGE ((N), FP_ARG_FIRST, FP_ARG_LAST)))

/* Define the standard RISC-V calling convention and variants.  */

enum riscv_cc
{
  RISCV_CC_BASE = 0, /* Base standard RISC-V ABI.  */
  RISCV_CC_V, /* For functions that pass or return values in V registers.  */
  RISCV_CC_UNKNOWN
};

typedef struct {
  /* The calling convention that current function used.  */
  enum riscv_cc variant_cc;

  /* Number of integer registers used so far, up to MAX_ARGS_IN_REGISTERS. */
  unsigned int num_gprs;

  /* Number of floating-point registers used so far, likewise.  */
  unsigned int num_fprs;

  /* Number of mask registers used so far, up to MAX_ARGS_IN_MASK_REGISTERS.  */
  unsigned int num_mrs;

  /* The used state of args in vector registers, true for used by prev arg,
     initial to false.  */
  bool used_vrs[MAX_ARGS_IN_VECTOR_REGISTERS];
} CUMULATIVE_ARGS;

/* Return riscv calling convention of call_insn.  */
extern enum riscv_cc get_riscv_cc (const rtx use);

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
  riscv_init_cumulative_args (&(CUM), (FNTYPE), (LIBNAME), (INDIRECT),     \
			(N_NAMED_ARGS) != -1)

#define EPILOGUE_USES(REGNO)	riscv_epilogue_uses (REGNO)

/* Align based on stack boundary, which might have been set by the user.  */
#define RISCV_STACK_ALIGN(LOC) \
  (((LOC) + ((PREFERRED_STACK_BOUNDARY/8)-1)) & -(PREFERRED_STACK_BOUNDARY/8))

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

#define REGNO_OK_FOR_INDEX_P(REGNO) \
  riscv_regno_ok_for_index_p (REGNO)

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

#define ASM_OUTPUT_LABELREF(FILE,NAME)					\
  do {									\
    if (strcmp (NAME, "..CURRENT_FUNCTION") == 0)			\
      asm_fprintf ((FILE), "%U%s",					\
		   XSTR (XEXP (DECL_RTL (current_function_decl),	\
			       0), 0));					\
    else								\
      asm_fprintf ((FILE), "%U%s", (NAME));				\
  } while (0)

#undef ASM_OUTPUT_OPCODE
#define ASM_OUTPUT_OPCODE(STREAM, PTR)	\
  (PTR) = riscv_asm_output_opcode(STREAM, PTR)

#define JUMP_TABLES_IN_TEXT_SECTION 0
#define CASE_VECTOR_MODE SImode
#define CASE_VECTOR_PC_RELATIVE (riscv_cmodel != CM_MEDLOW)

#define LOCAL_SYM_P(sym)						\
     ((SYMBOL_REF_P (sym) && SYMBOL_REF_LOCAL_P (sym))			\
	 || ((GET_CODE (sym) == CONST)					\
	     && SYMBOL_REF_P (XEXP (XEXP (sym, 0),0))			\
	     && SYMBOL_REF_LOCAL_P (XEXP (XEXP (sym, 0),0))))

/* The load-address macro is used for PC-relative addressing of symbols
   that bind locally.  Don't use it for symbols that should be addressed
   via the GOT.  Also, avoid it for CM_MEDLOW, where LUI addressing
   currently results in more opportunities for linker relaxation.  */
#define USE_LOAD_ADDRESS_MACRO(sym)					\
  (!TARGET_EXPLICIT_RELOCS &&						\
   ((flag_pic && LOCAL_SYM_P (sym)) || riscv_cmodel == CM_MEDANY))

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

/* Using SHIFT_COUNT_TRUNCATED is discouraged, so we handle this with patterns
   in the md file instead.  */
#define SHIFT_COUNT_TRUNCATED 0

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */

#define Pmode word_mode

/* Specify the machine mode that registers have.  */

#define Xmode (TARGET_64BIT ? DImode : SImode)

/* Give call MEMs SImode since it is the "most permissive" mode
   for both 32-bit and 64-bit targets.  */

#define FUNCTION_MODE SImode

/* A C expression for the cost of a branch instruction.  A value of 2
   seems to minimize code size.  */

#define BRANCH_COST(speed_p, predictable_p) \
  ((!(speed_p) || (predictable_p)) ? 2 : riscv_branch_cost)

/* True if the target optimizes short forward branches around integer
   arithmetic instructions into predicated operations, e.g., for
   conditional-move operations.  The macro assumes that all branch
   instructions (BEQ, BNE, BLT, BLTU, BGE, BGEU, C.BEQZ, and C.BNEZ)
   support this feature.  The macro further assumes that any integer
   arithmetic and logical operation (ADD[I], SUB, SLL[I], SRL[I], SRA[I],
   SLT[I][U], AND[I], XOR[I], OR[I], LUI, AUIPC, and their compressed
   counterparts, including C.MV and C.LI) can be in the branch shadow.  */

#define TARGET_SFB_ALU \
 ((riscv_microarchitecture == sifive_7) \
  || (riscv_microarchitecture == sifive_p400) \
  || (riscv_microarchitecture == sifive_p600))

/* True if the target supports misaligned vector loads and stores.  */
#define TARGET_VECTOR_MISALIGN_SUPPORTED \
   riscv_vector_unaligned_access_p

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
  "arg", "frame", "vl", "vtype", "vxrm", "frm", "vxsat", "N/A", \
  "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A",	\
  "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A",	\
  "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A",	\
  "v0",  "v1",  "v2",  "v3",  "v4",  "v5",  "v6",  "v7",	\
  "v8",  "v9",  "v10", "v11", "v12", "v13", "v14", "v15",	\
  "v16", "v17", "v18", "v19", "v20", "v21", "v22", "v23",	\
  "v24", "v25", "v26", "v27", "v28", "v29", "v30", "v31",}

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

/* Add output .variant_cc directive for specific function definition.  */
#undef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(STR, NAME, DECL)                             \
  riscv_declare_function_name (STR, NAME, DECL)

#undef ASM_DECLARE_FUNCTION_SIZE
#define ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)                           \
  riscv_declare_function_size (FILE, FNAME, DECL)

/* Add output .variant_cc directive for specific alias definition.  */
#undef ASM_OUTPUT_DEF_FROM_DECLS
#define ASM_OUTPUT_DEF_FROM_DECLS(STR, DECL, TARGET)                           \
  riscv_asm_output_alias (STR, DECL, TARGET)

/* Add output .variant_cc directive for specific extern function.  */
#undef ASM_OUTPUT_EXTERNAL
#define ASM_OUTPUT_EXTERNAL(STR, DECL, NAME)                                   \
  riscv_asm_output_external (STR, DECL, NAME)

#undef SIZE_TYPE
#define SIZE_TYPE (POINTER_SIZE == 64 ? "long unsigned int" : "unsigned int")

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE (POINTER_SIZE == 64 ? "long int" : "int")

/* The maximum number of bytes copied by one iteration of a cpymemsi loop.  */

#define RISCV_MAX_MOVE_BYTES_PER_LOOP_ITER (UNITS_PER_WORD * 4)

/* The maximum number of bytes that can be copied by a straight-line
   cpymemsi implementation.  */

#define RISCV_MAX_MOVE_BYTES_STRAIGHT (RISCV_MAX_MOVE_BYTES_PER_LOOP_ITER * 3)

/* If a memory-to-memory move would take MOVE_RATIO or more simple
   move-instruction pairs, we will do a cpymem or libcall instead.
   Do not use move_by_pieces at all when strict alignment is not
   in effect but the target has slow unaligned accesses; in this
   case, cpymem or libcall is more efficient.  */

#define MOVE_RATIO(speed)						\
  (!STRICT_ALIGNMENT && riscv_slow_unaligned_access_p ? 1 :		\
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
extern bool riscv_slow_unaligned_access_p;
extern bool riscv_vector_unaligned_access_p;
extern bool riscv_user_wants_strict_align;
extern unsigned riscv_stack_boundary;
extern unsigned riscv_bytes_per_vector_chunk;
extern poly_uint16 riscv_vector_chunks;
extern poly_int64 riscv_v_adjust_nunits (enum machine_mode, int);
extern poly_int64 riscv_v_adjust_nunits (machine_mode, bool, int, int);
extern poly_int64 riscv_v_adjust_precision (enum machine_mode, int);
extern poly_int64 riscv_v_adjust_bytesize (enum machine_mode, int);
/* The number of bits and bytes in a RVV vector.  */
#define BITS_PER_RISCV_VECTOR (poly_uint16 (riscv_vector_chunks * riscv_bytes_per_vector_chunk * 8))
#define BYTES_PER_RISCV_VECTOR (poly_uint16 (riscv_vector_chunks * riscv_bytes_per_vector_chunk))
#endif

#define ASM_PREFERRED_EH_DATA_FORMAT(CODE,GLOBAL) \
  (((GLOBAL) ? DW_EH_PE_indirect : 0) | DW_EH_PE_pcrel | DW_EH_PE_sdata4)

#define XLEN_SPEC \
  "%{march=rv32*:32}" \
  "%{march=rv64*:64}" \

#define ABI_SPEC \
  "%{mabi=ilp32:ilp32}" \
  "%{mabi=ilp32e:ilp32e}" \
  "%{mabi=ilp32f:ilp32f}" \
  "%{mabi=ilp32d:ilp32d}" \
  "%{mabi=lp64:lp64}" \
  "%{mabi=lp64e:lp64e}" \
  "%{mabi=lp64f:lp64f}" \
  "%{mabi=lp64d:lp64d}" \

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
#define C_S_BITS 5
#define C_SxSP_BITS 6

#define IMM_REACH (1LL << IMM_BITS)
#define CONST_HIGH_PART(VALUE) (((VALUE) + (IMM_REACH/2)) & ~(IMM_REACH-1))
#define CONST_LOW_PART(VALUE) ((VALUE) - CONST_HIGH_PART (VALUE))

#define SWSP_REACH (4LL << C_SxSP_BITS)
#define SDSP_REACH (8LL << C_SxSP_BITS)

/* This is the maximum value that can be represented in a compressed load/store
   offset (an unsigned 5-bit value scaled by 4).  */
#define CSW_MAX_OFFSET (((4LL << C_S_BITS) - 1) & ~3)

/* Called from RISCV_REORG, this is defined in riscv-sr.cc.  */

extern void riscv_remove_unneeded_save_restore_calls (void);

#define HARD_REGNO_RENAME_OK(FROM, TO) riscv_hard_regno_rename_ok (FROM, TO)

#define CLZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE) \
  ((VALUE) = GET_MODE_UNIT_BITSIZE (MODE), 2)
#define CTZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE) \
  ((VALUE) = GET_MODE_UNIT_BITSIZE (MODE), 2)

#define TARGET_SUPPORTS_WIDE_INT 1

#define REGISTER_TARGET_PRAGMAS() riscv_register_pragmas ()

#define REGMODE_NATURAL_SIZE(MODE) riscv_regmode_natural_size (MODE)

#define DWARF_FRAME_REGISTERS (FIRST_PSEUDO_REGISTER + 1 /* VLENB */)

#define DWARF_REG_TO_UNWIND_COLUMN(REGNO) \
  ((REGNO == RISCV_DWARF_VLENB) ? (FIRST_PSEUDO_REGISTER + 1) : REGNO)

/* Like s390, riscv also defined this macro for the vector comparision.  Then
   the simplify-rtx relational_result will canonicalize the result to the
   CONST1_RTX for the simplification.  */
#define VECTOR_STORE_FLAG_VALUE(MODE) CONSTM1_RTX (GET_MODE_INNER (MODE))

/* Mode switching (Lazy code motion) for RVV rounding mode instructions.  */
#define OPTIMIZE_MODE_SWITCHING(ENTITY) (TARGET_VECTOR)
#define NUM_MODES_FOR_MODE_SWITCHING {VXRM_MODE_NONE, riscv_vector::FRM_NONE}

/* The size difference between different RVV modes can be up to 64 times.
   e.g. RVVMF64BI vs RVVMF1BI on zvl512b, which is [1, 1] vs [64, 64].  */
#define MAX_POLY_VARIANT 64

#define HAVE_POST_MODIFY_DISP TARGET_XTHEADMEMIDX
#define HAVE_PRE_MODIFY_DISP  TARGET_XTHEADMEMIDX

/* Check TLS Descriptors mechanism is selected.  */
#define TARGET_TLSDESC (riscv_tls_dialect == TLS_DESCRIPTORS)

/* This value is the amount of bytes a caller is allowed to drop the stack
   before probing has to be done for stack clash protection.  */
#define STACK_CLASH_CALLER_GUARD 1024

/* This value controls how many pages we manually unroll the loop for when
   generating stack clash probes.  */
#define STACK_CLASH_MAX_UNROLL_PAGES 4

/* This value represents the minimum amount of bytes we expect the function's
   outgoing arguments to be when stack-clash is enabled.  */
#define STACK_CLASH_MIN_BYTES_OUTGOING_ARGS 8

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

#endif /* ! GCC_RISCV_H */
