/* Definitions of target machine for GNU compiler.  LoongArch version.
   Copyright (C) 2020-2021 Free Software Foundation, Inc.

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

/* LoongArch external variables defined in loongarch.c.  */

/* Which ABI to use.  ABILP64 is defined by Loongson.  */

#define ABILP32 0
#define ABILP64 2

/* Information about one recognized processor.  Defined here for the
   benefit of TARGET_CPU_CPP_BUILTINS.  */
struct loongarch_cpu_info
{
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

#include "config/loongarch/loongarch-opts.h"

/* Macros to silence warnings about numbers being signed in traditional
   C and unsigned in ISO C when compiled on 32-bit hosts.  */

#define BITMASK_HIGH (((unsigned long) 1) << 31) /* 0x80000000  */


/* Run-time compilation parameters selecting different hardware subsets.  */

/* True if we can use the JIRL instructions.  */
#define TARGET_ABSOLUTE_JUMPS (!flag_pic)

/* True if the output must have a writable .eh_frame.
   See ASM_PREFERRED_EH_DATA_FORMAT for details.  */
#define TARGET_WRITABLE_EH_FRAME (flag_pic && TARGET_SHARED)

/* Architecture target defines.  */
#define TARGET_LOONGARCH64 (loongarch_arch == PROCESSOR_LOONGARCH64)
#define TUNE_LOONGARCH64 (loongarch_tune == PROCESSOR_LOONGARCH64)
#define TARGET_GS464V (loongarch_arch == PROCESSOR_GS464V)
#define TUNE_GS464V (loongarch_tune == PROCESSOR_GS464V)

#define TARGET_LP32ABI (loongarch_abi == ABILP32)
#define TARGET_LP64ABI (loongarch_abi == ABILP64)

/* TARGET_HARD_FLOAT and TARGET_SOFT_FLOAT reflect whether the FPU is
   directly accessible, while the command-line options select
   TARGET_HARD_FLOAT_ABI and TARGET_SOFT_FLOAT_ABI to reflect the ABI
   in use.  */
#define TARGET_HARD_FLOAT (TARGET_HARD_FLOAT_ABI)
#define TARGET_SOFT_FLOAT (TARGET_SOFT_FLOAT_ABI)

#define TARGET_FLOAT32 (!TARGET_FLOAT64)

/* Define preprocessor macros for the -march and -mtune options.
   PREFIX is either _LOONGARCH_ARCH or _LOONGARCH_TUNE, INFO is
   the selected processor.  If INFO's canonical name is "foo",
   define PREFIX to be "foo", and define an additional macro
   PREFIX_FOO.  */
#define LARCH_CPP_SET_PROCESSOR(PREFIX, INFO) \
  do \
    { \
      char *macro, *p; \
\
      macro = concat ((PREFIX), "_", (INFO)->name, NULL); \
      for (p = macro; *p != 0; p++) \
	*p = TOUPPER (*p); \
\
      builtin_define (macro); \
      builtin_define_with_value ((PREFIX), (INFO)->name, 1); \
      free (macro); \
    } \
  while (0)

/* Target CPU builtins.  */
#define TARGET_CPU_CPP_BUILTINS() loongarch_cpu_cpp_builtins (pfile)

/* Default target_flags if no switches are specified.  */

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT 0
#endif

#ifndef TARGET_CPU_DEFAULT
#define TARGET_CPU_DEFAULT 0
#endif

#ifdef IN_LIBGCC2
#undef TARGET_64BIT
/* Make this compile time constant for libgcc2.  */
#ifdef __loongarch64
#define TARGET_64BIT 1
#else
#define TARGET_64BIT 0
#endif
#endif /* IN_LIBGCC2  */

#define TARGET_LIBGCC_SDATA_SECTION ".sdata"

#ifndef MULTILIB_ISA_DEFAULT
#if LARCH_ISA_DEFAULT == 0
#define MULTILIB_ISA_DEFAULT "loongarch64"
#elif LARCH_ISA_DEFAULT == 1
#define MULTILIB_ISA_DEFAULT "loongarch32"
#elif LARCH_ISA_DEFAULT == 2
#define MULTILIB_ISA_DEFAULT "gs464v"
#endif
#endif

#ifndef LARCH_ABI_DEFAULT
#define LARCH_ABI_DEFAULT ABILP64
#endif

/* Use the most portable ABI flag for the ASM specs.  */
#if LARCH_ABI_DEFAULT == ABILP32
#define MULTILIB_ABI_DEFAULT "mabi=lp32"
#elif LARCH_ABI_DEFAULT == ABILP64
#define MULTILIB_ABI_DEFAULT "mabi=lp64"
#endif

#ifndef MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS \
    { MULTILIB_ISA_DEFAULT, MULTILIB_ABI_DEFAULT }
#endif

/* Support for a compile-time default CPU, et cetera.  The rules are:
   --with-arch is ignored if -march is specified
   --with-tune is ignored if -mtune is specified
   --with-abi is ignored if -mabi is specified.
   --with-float is ignored if -mhard-float or -msoft-float are
     specified.
   --with-fpu is ignored if -msoft-float, -msingle-float or -mdouble-float are
     specified.
   --with-divide is ignored if -mdivide-traps or -mdivide-breaks are
     specified.
   --with-fix-loongson3-llsc is ignored if -mfix-loongson3-llsc is specified.  */
#define OPTION_DEFAULT_SPECS \
  {"arch", "%{!match=*:-march=%(VALUE)}"}, \
  {"tune", "%{!mtune=*:-mtune=%(VALUE)}"}, \
  {"abi", "%{!mabi=*:-mabi=%(VALUE)}"}, \
  {"float", "%{!msoft-float:%{!mhard-float:-m%(VALUE)-float}}"}, \
  {"fpu", \
    "%{!msoft-float:%{!msingle-float:%{!mdouble-float:-m%(VALUE)-float}}}"}, \
  {"divide", "%{!mdivide-traps:%{!mdivide-breaks:-mdivide-%(VALUE)}}"}, \
  {"fix-loongson3-llsc", "%{!mfix-loongson3-llsc: \
    %{!mno-fix-loongson3-llsc:-m%(VALUE)}}" }

#define ABI_SPEC \
  "%{mabi=lp32:32}" \
  "%{mabi=lp64:64}"

/* Default system library search paths.  */
#if LARCH_ABI_DEFAULT == ABILP32
#define STANDARD_STARTFILE_PREFIX_1 "/lib32/"
#define STANDARD_STARTFILE_PREFIX_2 "/usr/lib32/"
#elif LARCH_ABI_DEFAULT == ABILP64
#define STANDARD_STARTFILE_PREFIX_1 "/lib64/"
#define STANDARD_STARTFILE_PREFIX_2 "/usr/lib64/"
#endif

/* This definition replaces the formerly used 'm' constraint with a
   different constraint letter in order to avoid changing semantics of
   the 'm' constraint when accepting new address formats in
   TARGET_LEGITIMATE_ADDRESS_P.  The constraint letter defined here
   must not be used in insn definitions or inline assemblies.  */
#define TARGET_MEM_CONSTRAINT 'w'

/* Tell collect what flags to pass to nm.  */
#ifndef NM_FLAGS
#define NM_FLAGS "-Bn"
#endif


/* SUBTARGET_ASM_DEBUGGING_SPEC handles passing debugging options to
   the assembler.  It may be overridden by subtargets.  */

#ifndef SUBTARGET_ASM_DEBUGGING_SPEC
#define SUBTARGET_ASM_DEBUGGING_SPEC "\
%{g} %{g0} %{g1} %{g2} %{g3} \
%{ggdb:-g} %{ggdb0:-g0} %{ggdb1:-g1} %{ggdb2:-g2} %{ggdb3:-g3} \
%{gstabs:-g} %{gstabs0:-g0} %{gstabs1:-g1} %{gstabs2:-g2} %{gstabs3:-g3} \
%{gstabs+:-g} %{gstabs+0:-g0} %{gstabs+1:-g1} %{gstabs+2:-g2} %{gstabs+3:-g3}"
#endif

/* SUBTARGET_ASM_SPEC is always passed to the assembler.  It may be
   overridden by subtargets.  */

#ifndef SUBTARGET_ASM_SPEC
#define SUBTARGET_ASM_SPEC ""
#endif

#undef ASM_SPEC
#define ASM_SPEC "\
%{mabi=*} %{!mabi=*: %(asm_abi_default_spec)} \
"
/* Extra switches sometimes passed to the linker.  */

#ifndef LINK_SPEC
#define LINK_SPEC ""
#endif /* LINK_SPEC defined  */

/* Specs for the compiler proper.  */

/* CC1_SPEC is the set of arguments to pass to the compiler proper.  */

#undef CC1_SPEC
#define CC1_SPEC "\
%{G*} \
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

#define EXTRA_SPECS \
  {"subtarget_cc1_spec", SUBTARGET_CC1_SPEC}, \
  {"subtarget_cpp_spec", SUBTARGET_CPP_SPEC}, \
  {"subtarget_asm_debugging_spec", SUBTARGET_ASM_DEBUGGING_SPEC}, \
  {"subtarget_asm_spec", SUBTARGET_ASM_SPEC}, \
  {"asm_abi_default_spec", "-" MULTILIB_ABI_DEFAULT}, \
  SUBTARGET_EXTRA_SPECS

#ifndef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS
#endif

/* Registers may have a prefix which can be ignored when matching
   user asm and register definitions.  */
#ifndef REGISTER_PREFIX
#define REGISTER_PREFIX "$"
#endif

/* Local compiler-generated symbols must have a prefix that the assembler
   understands.  */

#define LOCAL_LABEL_PREFIX "."

/* By default on the loongarch, external symbols do not have an underscore
   prepended.  */

#define USER_LABEL_PREFIX ""

#ifndef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG
#endif

/* The size of DWARF addresses should be the same as the size of symbols
   in the target file format.
*/
#define DWARF2_ADDR_SIZE (TARGET_64BIT ? 8 : 4)

/* By default, turn on GDB extensions.  */
#define DEFAULT_GDB_EXTENSIONS 1

#define DWARF2_DEBUGGING_INFO 1 /* dwarf2 debugging info  */

/* The mapping from gcc register number to DWARF 2 CFA column number.  */
#define DWARF_FRAME_REGNUM(REGNO) loongarch_dwarf_regno[REGNO]

/* The DWARF 2 CFA column which tracks the return address.  */
#define DWARF_FRAME_RETURN_COLUMN RETURN_ADDR_REGNUM

/* Before the prologue, RA lives in r1.  */
#define INCOMING_RETURN_ADDR_RTX gen_rtx_REG (Pmode, RETURN_ADDR_REGNUM)

/* Describe how we implement __builtin_eh_return.  */
#define EH_RETURN_DATA_REGNO(N) \
  ((N) < (4) ? (N) + GP_ARG_FIRST : INVALID_REGNUM)

#define EH_RETURN_STACKADJ_RTX gen_rtx_REG (Pmode, GP_ARG_FIRST + 4)

#define EH_USES(N) loongarch_eh_uses (N)

/* Offsets recorded in opcodes are a multiple of this alignment factor.
   The default for this in 64-bit mode is 8, which causes problems with
   SFmode register saves.  */
#define DWARF_CIE_DATA_ALIGNMENT -4


/* Target machine storage layout.  */

#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN 0
#define WORDS_BIG_ENDIAN 0

#define MAX_BITS_PER_WORD 64

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD (TARGET_64BIT ? 8 : 4)
#ifndef IN_LIBGCC2
#define MIN_UNITS_PER_WORD 4
#endif

/* For LARCH, width of a floating point register.  */
#define UNITS_PER_FPREG (TARGET_FLOAT64 ? 8 : 4)

/* The number of consecutive floating-point registers needed to store the
   largest format supported by the FPU.  */
#define MAX_FPRS_PER_FMT (TARGET_FLOAT64 || TARGET_SINGLE_FLOAT ? 1 : 2)

/* The number of consecutive floating-point registers needed to store the
   smallest format supported by the FPU.  */
#define MIN_FPRS_PER_FMT 1

/* The largest size of value that can be held in floating-point
   registers and moved with a single instruction.  */
#define UNITS_PER_HWFPVALUE \
  (TARGET_SOFT_FLOAT_ABI ? 0 : MAX_FPRS_PER_FMT * UNITS_PER_FPREG)

/* The largest size of value that can be held in floating-point
   registers.  */
#define UNITS_PER_FPVALUE \
  (TARGET_SOFT_FLOAT_ABI ? 0 \
   : TARGET_SINGLE_FLOAT ? UNITS_PER_FPREG \
			 : LONG_DOUBLE_TYPE_SIZE / BITS_PER_UNIT)

/* The number of bytes in a double.  */
#define UNITS_PER_DOUBLE (TYPE_PRECISION (double_type_node) / BITS_PER_UNIT)

/* Set the sizes of the core types.  */
#define SHORT_TYPE_SIZE 16
#define INT_TYPE_SIZE 32
#define LONG_TYPE_SIZE (TARGET_64BIT ? 64 : 32)
#define LONG_LONG_TYPE_SIZE 64

#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 64
#define LONG_DOUBLE_TYPE_SIZE (TARGET_64BIT ? 128 : 64)

/* Define the sizes of fixed-point types.  */
#define SHORT_FRACT_TYPE_SIZE 8
#define FRACT_TYPE_SIZE 16
#define LONG_FRACT_TYPE_SIZE 32
#define LONG_LONG_FRACT_TYPE_SIZE 64

#define SHORT_ACCUM_TYPE_SIZE 16
#define ACCUM_TYPE_SIZE 32
#define LONG_ACCUM_TYPE_SIZE 64
/* FIXME.  LONG_LONG_ACCUM_TYPE_SIZE should be 128 bits, but GCC
   doesn't support 128-bit integers for loongarch32 currently.  */
#define LONG_LONG_ACCUM_TYPE_SIZE (TARGET_64BIT ? 128 : 64)

/* long double is not a fixed mode, but the idea is that, if we
   support long double, we also want a 128-bit integer type.  */
#define MAX_FIXED_MODE_SIZE LONG_DOUBLE_TYPE_SIZE

/* Width in bits of a pointer.  */
#ifndef POINTER_SIZE
#define POINTER_SIZE (TARGET_64BIT ? 64 : 32)
#endif

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY BITS_PER_WORD

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 32

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Number of bits which any structure or union's size must be a multiple of.
   Each structure or union's size is rounded up to a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* There is no point aligning anything to a rounder boundary than
   LONG_DOUBLE_TYPE_SIZE.  */
#define BIGGEST_ALIGNMENT (LONG_DOUBLE_TYPE_SIZE)

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
   optimised to use word loads.  */
#define LOCAL_ALIGNMENT(TYPE, ALIGN) DATA_ALIGNMENT (TYPE, ALIGN)

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS 1

/* When in 64-bit mode, move insns will sign extend SImode and FCCmode
   moves.  All other references are zero extended.  */
#define LOAD_EXTEND_OP(MODE) \
  (TARGET_64BIT && ((MODE) == SImode || (MODE) == FCCmode) ? SIGN_EXTEND \
							   : ZERO_EXTEND)

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases,
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.  */

#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE) \
  if (GET_MODE_CLASS (MODE) == MODE_INT \
      && GET_MODE_SIZE (MODE) < UNITS_PER_WORD) \
    { \
      if ((MODE) == SImode) \
	(UNSIGNEDP) = 0; \
      (MODE) = Pmode; \
    }

/* Pmode is always the same as ptr_mode, but not always the same as word_mode.
   Extensions of pointers to word_mode must be signed.  */
#define POINTERS_EXTEND_UNSIGNED false

/* Define if loading short immediate values into registers sign extends.  */
#define SHORT_IMMEDIATES_SIGN_EXTEND 1

/* The clz.{w/d} instructions have the natural values at 0.  */

#define CLZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE) \
  ((VALUE) = GET_MODE_UNIT_BITSIZE (MODE), 2)

/* Standard register usage.  */

/* Number of hardware registers.  We have:

   - 32 integer registers
   - 32 floating point registers
   - 8 condition code registers
   - 2 fake registers:
	- ARG_POINTER_REGNUM
	- FRAME_POINTER_REGNUM
*/

#define FIRST_PSEUDO_REGISTER 74

/* zero, tp, sp and x are fixed.  */
#define FIXED_REGISTERS							\
{ /* General registers.  */						\
  1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  /* Floating-point registers.  */					\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  /* Others.  */								\
  0, 0, 0, 0, 0, 0, 0, 0, 1, 1}

/* The call RTLs themselves clobber ra.  */
#define CALL_USED_REGISTERS						\
{ /* General registers.  */						\
  1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,			\
  1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  /* Floating-point registers.  */					\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,			\
  1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,			\
  /* Others.  */								\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1}

/* Internal macros to classify a register number as to whether it's a
   general purpose register, a floating point register, or a status
   register.  */

#define GP_REG_FIRST 0
#define GP_REG_LAST 31
#define GP_REG_NUM (GP_REG_LAST - GP_REG_FIRST + 1)

#define FP_REG_FIRST 32
#define FP_REG_LAST 63
#define FP_REG_NUM (FP_REG_LAST - FP_REG_FIRST + 1)

/* The DWARF 2 CFA column which tracks the return address from a
   signal handler context.  This means that to maintain backwards
   compatibility, no hard register can be assigned this column if it
   would need to be handled by the DWARF unwinder.  */
#define DWARF_ALT_FRAME_RETURN_COLUMN 72

#define ST_REG_FIRST 64
#define ST_REG_LAST 71
#define ST_REG_NUM (ST_REG_LAST - ST_REG_FIRST + 1)

#define GP_REG_P(REGNO) \
  ((unsigned int) ((int) (REGNO) -GP_REG_FIRST) < GP_REG_NUM)
#define FP_REG_P(REGNO) \
  ((unsigned int) ((int) (REGNO) -FP_REG_FIRST) < FP_REG_NUM)
#define ST_REG_P(REGNO) \
  ((unsigned int) ((int) (REGNO) -ST_REG_FIRST) < ST_REG_NUM)

#define FP_REG_RTX_P(X) (REG_P (X) && FP_REG_P (REGNO (X)))

#define HARD_REGNO_RENAME_OK(OLD_REG, NEW_REG) \
  loongarch_hard_regno_rename_ok (OLD_REG, NEW_REG)

/* Select a register mode required for caller save of hard regno REGNO.  */
#define HARD_REGNO_CALLER_SAVE_MODE(REGNO, NREGS, MODE) \
  loongarch_hard_regno_caller_save_mode (REGNO, NREGS, MODE)

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM (GP_REG_FIRST + 3)

/* These two registers don't really exist: they get eliminated to either
   the stack or hard frame pointer.  */
#define ARG_POINTER_REGNUM 72
#define FRAME_POINTER_REGNUM 73

#define HARD_FRAME_POINTER_REGNUM (GP_REG_FIRST + 22)

#define HARD_FRAME_POINTER_IS_FRAME_POINTER 0
#define HARD_FRAME_POINTER_IS_ARG_POINTER 0

/* FIXME: */
/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM (GP_REG_FIRST + 20) /* $t8  */

#define GP_TEMP_FIRST (GP_REG_FIRST + 12)
#define LARCH_PROLOGUE_TEMP_REGNUM (GP_TEMP_FIRST + 1)
#define LARCH_PROLOGUE_TEMP2_REGNUM (GP_TEMP_FIRST)
#define LARCH_EPILOGUE_TEMP_REGNUM (GP_TEMP_FIRST)

#define CALLEE_SAVED_REG_NUMBER(REGNO) \
  ((REGNO) >= 22 && (REGNO) <= 31 ? (REGNO) -22 : -1)

#define LARCH_PROLOGUE_TEMP(MODE) \
  gen_rtx_REG (MODE, LARCH_PROLOGUE_TEMP_REGNUM)
#define LARCH_PROLOGUE_TEMP2(MODE) \
  gen_rtx_REG (MODE, LARCH_PROLOGUE_TEMP2_REGNUM)
#define LARCH_EPILOGUE_TEMP(MODE) \
  gen_rtx_REG (MODE, LARCH_EPILOGUE_TEMP_REGNUM)

/* Define this macro if it is as good or better to call a constant
   function address than to call an address kept in a register.  */
#define NO_FUNCTION_CSE 1

#define THREAD_POINTER_REGNUM (GP_REG_FIRST + 2)


/* Define the classes of registers for register constraints in the
   machine description.  Also define ranges of constants.

   One of the classes must always be named ALL_REGS and include all hard regs.
   If there is more than one class, another class must be named NO_REGS
   and contain no registers.

   The name GENERAL_REGS must be the name of a class (or an alias for
   another name such as ALL_REGS).  This is the class of registers
   that is allowed by "r" in a register constraint.
   Also, registers outside this class are allocated only when
   instructions express preferences for them.

   The classes must be numbered in nondecreasing order; that is,
   a larger-numbered class must never be contained completely
   in a smaller-numbered class.

   For any two classes, it is very desirable that there be another
   class that represents their union.  */

enum reg_class
{
  NO_REGS,	  /* no registers in set  */
  SIBCALL_REGS,	  /* SIBCALL_REGS  */
  JALR_REGS,	  /* JALR_REGS  */
  GR_REGS,	  /* integer registers  */
  CSR_REGS,	  /* integer registers except for $r0 and $r1 for lcsr.  */
  FP_REGS,	  /* floating point registers  */
  ST_REGS,	  /* status registers (fp status)  */
  FRAME_REGS,	  /* $arg and $frame  */
  ALL_REGS,	  /* all registers  */
  LIM_REG_CLASSES /* max value + 1  */
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
  "CSR_REGS",								\
  "FP_REGS",								\
  "ST_REGS",								\
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
  { 0x00000000, 0x00000000, 0x00000000 },	/* NO_REGS  */		\
  { 0x001ff000, 0x00000000, 0x00000000 },	/* SIBCALL_REGS  */	\
  { 0xff9ffff0, 0x00000000, 0x00000000 },	/* JALR_REGS  */	\
  { 0xffffffff, 0x00000000, 0x00000000 },	/* GR_REGS  */		\
  { 0xfffffffc, 0x00000000, 0x00000000 },	/* CSR_REGS  */		\
  { 0x00000000, 0xffffffff, 0x00000000 },	/* FP_REGS  */		\
  { 0x00000000, 0x00000000, 0x000000ff },	/* ST_REGS  */		\
  { 0x00400000, 0x00000000, 0x00000200 },	/* FRAME_REGS  */	\
  { 0xffffffff, 0xffffffff, 0x000003ff }	/* ALL_REGS  */		\
}

/* A C expression whose value is a register class containing hard
   register REGNO.  In general there is more that one such class;
   choose a class which is "minimal", meaning that no smaller class
   also contains the register.  */

#define REGNO_REG_CLASS(REGNO) loongarch_regno_to_class[(REGNO)]

/* A macro whose definition is the name of the class to which a
   valid base register must belong.  A base register is one used in
   an address which is the register value plus a displacement.  */

#define BASE_REG_CLASS (GR_REGS)

/* A macro whose definition is the name of the class to which a
   valid index register must belong.  An index register is one used
   in an address where its value is either multiplied by a scale
   factor or added to another register (as well as added to a
   displacement).  */

#define INDEX_REG_CLASS NO_REGS

/* We generally want to put call-clobbered registers ahead of
   call-saved ones.  (IRA expects this.)  */

#define REG_ALLOC_ORDER							\
{ /* Call-clobbered GPRs.  */						\
  12, 13, 14, 15, 16, 17, 18, 19, 20, 4, 5, 6, 7, 8, 9, 10, 11, 1,	\
  /* Call-saved GPRs.  */						\
  23, 24, 25, 26, 27, 28, 29, 30, 31,					\
  /* GPRs that can never be exposed to the register allocator.  */	\
  0, 2, 3, 21, 22, 							\
  /* Call-clobbered FPRs.  */						\
  32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,	\
  48, 49, 50, 51,52, 53, 54, 55, 					\
  56, 57, 58, 59, 60, 61, 62, 63,					\
  /* None of the remaining classes have defined call-saved		\
     registers.  */							\
  64, 65, 66, 67, 68, 69, 70, 71, 72, 73}

#define IMM_BITS 12
#define IMM_REACH (1LL << IMM_BITS)

/* True if VALUE is an unsigned 6-bit number.  */

#define UIMM6_OPERAND(VALUE) (((VALUE) & ~(unsigned HOST_WIDE_INT) 0x3f) == 0)

/* True if VALUE is a signed 10-bit number.  */

#define IMM10_OPERAND(VALUE) ((unsigned HOST_WIDE_INT) (VALUE) + 0x200 < 0x400)

/* True if VALUE is a signed 12-bit number.  */

#define IMM12_OPERAND(VALUE) \
  ((unsigned HOST_WIDE_INT) (VALUE) + 0x800 < 0x1000)

/* True if VALUE is a signed 16-bit number.  */

#define IMM16_OPERAND(VALUE) \
  ((unsigned HOST_WIDE_INT) (VALUE) + 0x8000 < 0x10000)

/* True if VALUE is a signed 12-bit number.  */

#define SMALL_OPERAND(VALUE) \
  ((unsigned HOST_WIDE_INT) (VALUE) + IMM_REACH / 2 < IMM_REACH)

/* True if VALUE is an unsigned 12-bit number.  */

#define SMALL_OPERAND_UNSIGNED(VALUE) \
  (((VALUE) & ~(unsigned HOST_WIDE_INT) (IMM_REACH - 1)) == 0)

/* True if VALUE can be loaded into a register using LU12I.  */

#define LU12I_OPERAND(VALUE) \
  (((VALUE) | ((1UL << 31) - IMM_REACH)) == ((1UL << 31) - IMM_REACH) \
   || ((VALUE) | ((1UL << 31) - IMM_REACH)) + IMM_REACH == 0)

/* True if VALUE can be loaded into a register using LU32I.  */

#define LU32I_OPERAND(VALUE) \
  (((VALUE) | (((1UL << 19) - 1) << 32)) == (((1UL << 19) - 1) << 32) \
   || ((VALUE) | (((1UL << 19) - 1) << 32)) + (1UL << 32) == 0)

/* True if VALUE can be loaded into a register using LU52I.  */

#define LU52I_OPERAND(VALUE) (((VALUE) | (0xfffUL << 52)) == (0xfffUL << 52))

/* Return a value X with the low 12 bits clear, and such that
   VALUE - X is a signed 12-bit value.  */

#define CONST_HIGH_PART(VALUE) (((VALUE) + (IMM_REACH / 2)) & ~(IMM_REACH - 1))

#define CONST_LOW_PART(VALUE) ((VALUE) -CONST_HIGH_PART (VALUE))

#define SMALL_INT(X) SMALL_OPERAND (INTVAL (X))
#define SMALL_INT_UNSIGNED(X) SMALL_OPERAND_UNSIGNED (INTVAL (X))
#define LU12I_INT(X) LU12I_OPERAND (INTVAL (X))
#define LU32I_INT(X) LU32I_OPERAND (INTVAL (X))
#define LU52I_INT(X) LU52I_OPERAND (INTVAL (X))
#define LARCH_U12BIT_OFFSET_P(OFFSET) (IN_RANGE (OFFSET, -2048, 2047))
#define LARCH_9BIT_OFFSET_P(OFFSET) (IN_RANGE (OFFSET, -256, 255))
#define LARCH_16BIT_OFFSET_P(OFFSET) (IN_RANGE (OFFSET, -32768, 32767))
#define LARCH_SHIFT_2_OFFSET_P(OFFSET) (((OFFSET) &0x3) == 0)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */

#define CLASS_MAX_NREGS(CLASS, MODE) loongarch_class_max_nregs (CLASS, MODE)

/* Stack layout; function entry, exit and calling.  */

#define STACK_GROWS_DOWNWARD 1

#define FRAME_GROWS_DOWNWARD 1

#define RETURN_ADDR_RTX loongarch_return_addr

/* Similarly, don't use the least-significant bit to tell pointers to
   code from vtable index.  */

#define TARGET_PTRMEMFUNC_VBIT_LOCATION ptrmemfunc_vbit_in_delta

#define ELIMINABLE_REGS \
  { \
    {ARG_POINTER_REGNUM, STACK_POINTER_REGNUM}, \
      {ARG_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM}, \
      {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}, \
      {FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM}, \
  }

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  (OFFSET) = loongarch_initial_elimination_offset ((FROM), (TO))

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

#define STACK_BOUNDARY (TARGET_LP64ABI ? 128 : 64)

/* Symbolic macros for the registers used to return integer and floating
   point values.  */

#define GP_RETURN (GP_REG_FIRST + 4)
#define FP_RETURN ((TARGET_SOFT_FLOAT) ? GP_RETURN : (FP_REG_FIRST + 0))

#define MAX_ARGS_IN_REGISTERS 8

/* Symbolic macros for the first/last argument registers.  */

#define GP_ARG_FIRST (GP_REG_FIRST + 4)
#define GP_ARG_LAST (GP_ARG_FIRST + MAX_ARGS_IN_REGISTERS - 1)
#define FP_ARG_FIRST (FP_REG_FIRST + 0)
#define FP_ARG_LAST (FP_ARG_FIRST + MAX_ARGS_IN_REGISTERS - 1)

/* 1 if N is a possible register number for function argument passing.
   We have no FP argument registers when soft-float.  */

/* Accept arguments in a0-a7, and in fa0-fa7 if permitted by the ABI.  */
#define FUNCTION_ARG_REGNO_P(N) \
  (IN_RANGE ((N), GP_ARG_FIRST, GP_ARG_LAST) \
   || (UNITS_PER_FP_ARG && IN_RANGE ((N), FP_ARG_FIRST, FP_ARG_LAST)))


/* This structure has to cope with two different argument allocation
   schemes.  Most LoongArch ABIs view the arguments as a structure, of which
   the first N words go in registers and the rest go on the stack.  If I
   < N, the Ith word might go in Ith integer argument register or in a
   floating-point register.  For these ABIs, we only need to remember
   the offset of the current argument into the structure.

   So for the standard ABIs, the first N words are allocated to integer
   registers, and loongarch_function_arg decides on an argument-by-argument
   basis whether that argument should really go in an integer register,
   or in a floating-point one.  */

typedef struct loongarch_args
{
  /* Number of integer registers used so far, up to MAX_ARGS_IN_REGISTERS.  */
  unsigned int num_gprs;

  /* Number of floating-point registers used so far, likewise.  */
  unsigned int num_fprs;

} CUMULATIVE_ARGS;

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
  memset (&(CUM), 0, sizeof (CUM))


#define EPILOGUE_USES(REGNO) loongarch_epilogue_uses (REGNO)

/* Treat LOC as a byte offset from the stack pointer and round it up
   to the next fully-aligned offset.  */
#define LARCH_STACK_ALIGN(LOC) \
  (TARGET_LP64ABI ? ROUND_UP ((LOC), 16) : ROUND_UP ((LOC), 8))


/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define MCOUNT_NAME "_mcount"

/* Emit rtl for profiling.  Output assembler code to FILE
   to call "_mcount" for profiling a function entry.  */
#define PROFILE_HOOK(LABEL) \
  { \
    rtx fun, ra; \
    ra = get_hard_reg_initial_val (Pmode, RETURN_ADDR_REGNUM); \
    fun = gen_rtx_SYMBOL_REF (Pmode, MCOUNT_NAME); \
    emit_library_call (fun, LCT_NORMAL, VOIDmode, ra, Pmode); \
  }

/* All the work done in PROFILE_HOOK, but still required.  */
#define FUNCTION_PROFILER(STREAM, LABELNO) \
  do \
    { \
    } \
  while (0)

#define NO_PROFILE_COUNTERS 1

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 1


/* Trampolines are a block of code followed by two pointers.  */

#define TRAMPOLINE_CODE_SIZE 16
#define TRAMPOLINE_SIZE \
  ((Pmode == SImode) ? TRAMPOLINE_CODE_SIZE \
		     : (TRAMPOLINE_CODE_SIZE + POINTER_SIZE * 2))
#define TRAMPOLINE_ALIGNMENT POINTER_SIZE

/* loongarch_trampoline_init calls this library function to flush
   program and data caches.  */

#ifndef CACHE_FLUSH_FUNC
#define CACHE_FLUSH_FUNC "_flush_cache"
#endif


/* Addressing modes, and classification of registers for them.  */

#define REGNO_OK_FOR_INDEX_P(REGNO) 0
#define REGNO_MODE_OK_FOR_BASE_P(REGNO, MODE) \
  loongarch_regno_mode_ok_for_base_p (REGNO, MODE, 1)

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 1

/* Check for constness inline but use loongarch_legitimate_address_p
   to check whether a constant really is an address.  */

#define CONSTANT_ADDRESS_P(X) (CONSTANT_P (X) && memory_address_p (SImode, X))

/* This handles the magic '..CURRENT_FUNCTION' symbol, which means
   'the start of the function that this code is output in'.  */

#define ASM_OUTPUT_LABELREF(FILE, NAME) \
  do \
    { \
      if (strcmp (NAME, "..CURRENT_FUNCTION") == 0) \
	asm_fprintf ((FILE), "%U%s", \
		     XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0)); \
      else \
	asm_fprintf ((FILE), "%U%s", (NAME)); \
    } \
  while (0)

#define CASE_VECTOR_MODE Pmode

/* Only use short offsets if their range will not overflow.  */
#define CASE_VECTOR_SHORTEN_MODE(MIN, MAX, BODY) Pmode

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#ifndef DEFAULT_SIGNED_CHAR
#define DEFAULT_SIGNED_CHAR 1
#endif

/* The SPARC port says:
   The maximum number of bytes that a single instruction
   can move quickly between memory and registers or between
   two memory locations.  */
#define MOVE_MAX UNITS_PER_WORD
#define MAX_MOVE_MAX 8

/* The SPARC port says:
   Nonzero if access to memory by bytes is slow and undesirable.
   For RISC chips, it means that access to memory by bytes is no
   better than access by words when possible, so grab a whole word
   and maybe make use of that.  */
#define SLOW_BYTE_ACCESS 1

/* Standard LoongArch integer shifts truncate the shift amount to the
   width of the shifted operand.  */
#define SHIFT_COUNT_TRUNCATED 1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */

#ifndef Pmode
#define Pmode (TARGET_64BIT ? DImode : SImode)
#endif

/* Give call MEMs SImode since it is the "most permissive" mode
   for both 32-bit and 64-bit targets.  */

#define FUNCTION_MODE SImode


/* We allocate $fcc registers by hand and can't cope with moves of
   CCmode registers to and from pseudos (or memory).  */
#define AVOID_CCMODE_COPIES

/* A C expression for the cost of a branch instruction.  A value of
   1 is the default; other values are interpreted relative to that.  */

#define BRANCH_COST(speed_p, predictable_p) loongarch_branch_cost
#define LOGICAL_OP_NON_SHORT_CIRCUIT 0

/* If defined, modifies the length assigned to instruction INSN as a
   function of the context in which it is used.  LENGTH is an lvalue
   that contains the initially computed length of the insn and should
   be updated with the correct length of the insn.  */
#define ADJUST_INSN_LENGTH(INSN, LENGTH) \
  ((LENGTH) = loongarch_adjust_insn_length ((INSN), (LENGTH)))

/* Return the asm template for a conditional branch instruction.
   OPCODE is the opcode's mnemonic and OPERANDS is the asm template for
   its operands.  */
#define LARCH_BRANCH(OPCODE, OPERANDS) OPCODE "\t" OPERANDS


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

#define REGISTER_NAMES							  \
{ "$r0",   "$r1",   "$r2",   "$r3",   "$r4",   "$r5",   "$r6",   "$r7",   \
  "$r8",   "$r9",   "$r10",  "$r11",  "$r12",  "$r13",  "$r14",  "$r15",  \
  "$r16",  "$r17",  "$r18",  "$r19",  "$r20",  "$r21",  "$r22",  "$r23",  \
  "$r24",  "$r25",  "$r26",  "$r27",  "$r28",  "$r29",  "$r30",  "$r31",  \
  "$f0",  "$f1",  "$f2",  "$f3",  "$f4",  "$f5",  "$f6",  "$f7",          \
  "$f8",  "$f9",  "$f10", "$f11", "$f12", "$f13", "$f14", "$f15",         \
  "$f16", "$f17", "$f18", "$f19", "$f20", "$f21", "$f22", "$f23",         \
  "$f24", "$f25", "$f26", "$f27", "$f28", "$f29", "$f30", "$f31",         \
  "$fcc0","$fcc1","$fcc2","$fcc3","$fcc4","$fcc5","$fcc6","$fcc7",	  \
  "$arg", "$frame"}

/* This macro defines additional names for hard registers.  */

#define ADDITIONAL_REGISTER_NAMES					\
{									\
  { "zero",	 0 + GP_REG_FIRST },					\
  { "ra",	 1 + GP_REG_FIRST },					\
  { "tp",	 2 + GP_REG_FIRST },					\
  { "sp",	 3 + GP_REG_FIRST },					\
  { "a0",	 4 + GP_REG_FIRST },					\
  { "a1",	 5 + GP_REG_FIRST },					\
  { "a2",	 6 + GP_REG_FIRST },					\
  { "a3",	 7 + GP_REG_FIRST },					\
  { "a4",	 8 + GP_REG_FIRST },					\
  { "a5",	 9 + GP_REG_FIRST },					\
  { "a6",	10 + GP_REG_FIRST },					\
  { "a7",	11 + GP_REG_FIRST },					\
  { "t0",	12 + GP_REG_FIRST },					\
  { "t1",	13 + GP_REG_FIRST },					\
  { "t2",	14 + GP_REG_FIRST },					\
  { "t3",	15 + GP_REG_FIRST },					\
  { "t4",	16 + GP_REG_FIRST },					\
  { "t5",	17 + GP_REG_FIRST },					\
  { "t6",	18 + GP_REG_FIRST },					\
  { "t7",	19 + GP_REG_FIRST },					\
  { "t8",	20 + GP_REG_FIRST },					\
  { "x",	21 + GP_REG_FIRST },					\
  { "fp",	22 + GP_REG_FIRST },					\
  { "s0",	23 + GP_REG_FIRST },					\
  { "s1",	24 + GP_REG_FIRST },					\
  { "s2",	25 + GP_REG_FIRST },					\
  { "s3",	26 + GP_REG_FIRST },					\
  { "s4",	27 + GP_REG_FIRST },					\
  { "s5",	28 + GP_REG_FIRST },					\
  { "s6",	29 + GP_REG_FIRST },					\
  { "s7",	30 + GP_REG_FIRST },					\
  { "s8",	31 + GP_REG_FIRST },					\
  { "v0",	 4 + GP_REG_FIRST },					\
  { "v1",	 5 + GP_REG_FIRST }					\
}

#define DBR_OUTPUT_SEQEND(STREAM) \
  do \
    { \
      /* Emit a blank line after the delay slot for emphasis.  */ \
      fputs ("\n", STREAM); \
    } \
  while (0)

/* The LoongArch implementation uses some labels for its own purpose.  The
   following lists what labels are created, and are all formed by the
   pattern $L[a-z].*.  The machine independent portion of GCC creates
   labels matching:  $L[A-Z][0-9]+ and $L[0-9]+.

	LM[0-9]+	Silicon Graphics/ECOFF stabs label before each stmt.
	$Lb[0-9]+	Begin blocks for LoongArch debug support
	$Lc[0-9]+	Label for use in s<xx> operation.
	$Le[0-9]+	End blocks for LoongArch debug support.  */

#undef ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME(STREAM, NAME, DECL) \
  loongarch_declare_object (STREAM, NAME, "", ":\n")

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.globl\t"

/* This says how to define a global common symbol.  */

#define ASM_OUTPUT_ALIGNED_DECL_COMMON loongarch_output_aligned_decl_common

/* This says how to define a local common symbol (i.e., not visible to
   linker).  */

#ifndef ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(STREAM, NAME, SIZE, ALIGN) \
  loongarch_declare_common_object (STREAM, NAME, "\n\t.lcomm\t", SIZE, ALIGN, \
				   false)
#endif

/* This says how to output an external.  It would be possible not to
   output anything and let undefined symbol become external.  However
   the assembler uses length information on externals to allocate in
   data/sdata bss/sbss, thereby saving exec time.  */

#undef ASM_OUTPUT_EXTERNAL
#define ASM_OUTPUT_EXTERNAL(STREAM, DECL, NAME) \
  loongarch_output_external (STREAM, DECL, NAME)

/* This is how to declare a function name.  The actual work of
   emitting the label is moved to function_prologue, so that we can
   get the line number correctly emitted before the .ent directive,
   and after any .file directives.  Define as empty so that the function
   is not declared before the .ent directive elsewhere.  */

#undef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(STREAM, NAME, DECL) \
  loongarch_declare_function_name (STREAM, NAME, DECL)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM) \
  sprintf ((LABEL), "*%s%s%ld", (LOCAL_LABEL_PREFIX), (PREFIX), (long) (NUM))

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

   */

#define ASM_OUTPUT_DEBUG_LABEL(FILE, PREFIX, NUM) \
  fprintf (FILE, "%s%s%d = .\n", LOCAL_LABEL_PREFIX, PREFIX, NUM)

/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE) \
  fprintf (STREAM, "\t%s\t%sL%d\n", ptr_mode == DImode ? ".dword" : ".word", \
	   LOCAL_LABEL_PREFIX, VALUE)

/* This is how to output an element of a case-vector.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL) \
  do \
    { \
      fprintf (STREAM, "\t%s\t%sL%d-%sL%d\n", \
	       ptr_mode == DImode ? ".dword" : ".word", LOCAL_LABEL_PREFIX, \
	       VALUE, LOCAL_LABEL_PREFIX, REL); \
    } \
  while (0)

#define JUMP_TABLES_IN_TEXT_SECTION 0

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(STREAM, LOG) fprintf (STREAM, "\t.align\t%d\n", (LOG))

/* "nop" instruction 54525952 (andi $r0,$r0,0) is
   used for padding. */
#define ASM_OUTPUT_ALIGN_WITH_NOP(STREAM, LOG) \
  fprintf (STREAM, "\t.align\t%d,54525952,4\n", (LOG))

/* This is how to output an assembler line to advance the location
   counter by SIZE bytes.  */

#undef ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(STREAM, SIZE) \
  fprintf (STREAM, "\t.space\t" HOST_WIDE_INT_PRINT_UNSIGNED "\n", (SIZE))

/* This is how to output a string.  */
#undef ASM_OUTPUT_ASCII
#define ASM_OUTPUT_ASCII loongarch_output_ascii

/* FIXME: delete ???
   Default to -G 8.  */
#ifndef LARCH_DEFAULT_GVALUE
#define LARCH_DEFAULT_GVALUE 8
#endif

/* Define the strings to put out for each section in the object file.  */
#define TEXT_SECTION_ASM_OP "\t.text" /* instructions  */
#define DATA_SECTION_ASM_OP "\t.data" /* large data  */

#undef READONLY_DATA_SECTION_ASM_OP
#define READONLY_DATA_SECTION_ASM_OP "\t.section\t.rodata" /* read-only data \
							    */

#define ASM_OUTPUT_REG_PUSH(STREAM, REGNO) \
  do \
    { \
      fprintf (STREAM, "\t%s\t%s,%s,-8\n\t%s\t%s,%s,0\n", \
	       TARGET_64BIT ? "addi.d" : "addi.w", \
	       reg_names[STACK_POINTER_REGNUM], \
	       reg_names[STACK_POINTER_REGNUM], \
	       TARGET_64BIT ? "st.d" : "st.w", reg_names[REGNO], \
	       reg_names[STACK_POINTER_REGNUM]); \
    } \
  while (0)

#define ASM_OUTPUT_REG_POP(STREAM, REGNO) \
  do \
    { \
      fprintf (STREAM, "\t%s\t%s,%s,0\n\t%s\t%s,%s,8\n", \
	       TARGET_64BIT ? "ld.d" : "ld.w", reg_names[REGNO], \
	       reg_names[STACK_POINTER_REGNUM], \
	       TARGET_64BIT ? "addi.d" : "addi.w", \
	       reg_names[STACK_POINTER_REGNUM], \
	       reg_names[STACK_POINTER_REGNUM]); \
    } \
  while (0)

/* How to start an assembler comment.
   The leading space is important (the loongarch native assembler requires it).
 */
#ifndef ASM_COMMENT_START
#define ASM_COMMENT_START " #"
#endif

#undef SIZE_TYPE
#define SIZE_TYPE (POINTER_SIZE == 64 ? "long unsigned int" : "unsigned int")

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE (POINTER_SIZE == 64 ? "long int" : "int")

/* The maximum number of bytes that can be copied by one iteration of
   a cpymemsi loop; see loongarch_block_move_loop.  */
#define LARCH_MAX_MOVE_BYTES_PER_LOOP_ITER (UNITS_PER_WORD * 4)

/* The maximum number of bytes that can be copied by a straight-line
   implementation of cpymemsi; see loongarch_block_move_straight.  We want
   to make sure that any loop-based implementation will iterate at
   least twice.  */
#define LARCH_MAX_MOVE_BYTES_STRAIGHT (LARCH_MAX_MOVE_BYTES_PER_LOOP_ITER * 2)

/* The base cost of a memcpy call, for MOVE_RATIO and friends.  These
   values were determined experimentally by benchmarking with CSiBE.
*/
#define LARCH_CALL_RATIO 8

/* Any loop-based implementation of cpymemsi will have at least
   LARCH_MAX_MOVE_BYTES_STRAIGHT / UNITS_PER_WORD memory-to-memory
   moves, so allow individual copies of fewer elements.

   When cpymemsi is not available, use a value approximating
   the length of a memcpy call sequence, so that move_by_pieces
   will generate inline code if it is shorter than a function call.
   Since move_by_pieces_ninsns counts memory-to-memory moves, but
   we'll have to generate a load/store pair for each, halve the
   value of LARCH_CALL_RATIO to take that into account.  */

#define MOVE_RATIO(speed) \
  (HAVE_cpymemsi ? LARCH_MAX_MOVE_BYTES_STRAIGHT / MOVE_MAX \
		 : LARCH_CALL_RATIO / 2)

/* For CLEAR_RATIO, when optimizing for size, give a better estimate
   of the length of a memset call, but use the default otherwise.  */

#define CLEAR_RATIO(speed) ((speed) ? 15 : LARCH_CALL_RATIO)

/* This is similar to CLEAR_RATIO, but for a non-zero constant, so when
   optimizing for size adjust the ratio to account for the overhead of
   loading the constant and replicating it across the word.  */

#define SET_RATIO(speed) ((speed) ? 15 : LARCH_CALL_RATIO - 2)

#ifndef USED_FOR_TARGET
extern const enum reg_class loongarch_regno_to_class[];
extern int loongarch_dwarf_regno[];
extern enum processor loongarch_arch; /* Which cpu to codegen for.  */
extern enum processor loongarch_tune; /* Which cpu to schedule for.  */
extern const struct loongarch_cpu_info *loongarch_arch_info;
extern const struct loongarch_cpu_info *loongarch_tune_info;

/* Information about a function's frame layout.  */
struct GTY (()) loongarch_frame_info
{
  /* The size of the frame in bytes.  */
  HOST_WIDE_INT total_size;

  /* Bit X is set if the function saves or restores GPR X.  */
  unsigned int mask;

  /* Likewise FPR X.  */
  unsigned int fmask;

  /* How much the GPR save/restore routines adjust sp (or 0 if unused).  */
  unsigned save_libcall_adjustment;

  /* Offsets of fixed-point and floating-point save areas from frame bottom.  */
  HOST_WIDE_INT gp_sp_offset;
  HOST_WIDE_INT fp_sp_offset;

  /* Offset of virtual frame pointer from stack pointer/frame bottom.  */
  HOST_WIDE_INT frame_pointer_offset;

  /* Offset of hard frame pointer from stack pointer/frame bottom.  */
  HOST_WIDE_INT hard_frame_pointer_offset;

  /* The offset of arg_pointer_rtx from the bottom of the frame.  */
  HOST_WIDE_INT arg_pointer_offset;
};

struct GTY (()) machine_function
{
  /* The next floating-point condition-code register to allocate
     for 8CC targets, relative to ST_REG_FIRST.  */
  unsigned int next_fcc;

  /* The number of extra stack bytes taken up by register varargs.
     This area is allocated by the callee at the very top of the frame.  */
  int varargs_size;

  /* The current frame information, calculated by loongarch_compute_frame_info.
   */
  struct loongarch_frame_info frame;

  /* True if loongarch_adjust_insn_length should ignore an instruction's
     hazard attribute.  */
  bool ignore_hazard_length_p;

  /* True if at least one of the formal parameters to a function must be
     written to the frame header (probably so its address can be taken).  */
  bool does_not_use_frame_header;

  /* True if none of the functions that are called by this function need
     stack space allocated for their arguments.  */
  bool optimize_call_stack;

  /* True if one of the functions calling this function may not allocate
     a frame header.  */
  bool callers_may_not_allocate_frame;
};
#endif

/* As on most targets, we want the .eh_frame section to be read-only where
   possible.  And as on most targets, this means two things:

     (a) Non-locally-binding pointers must have an indirect encoding,
	 so that the addresses in the .eh_frame section itself become
	 locally-binding.

     (b) A shared library's .eh_frame section must encode locally-binding
	 pointers in a relative (relocation-free) form.

   However, LoongArch has traditionally not allowed directives like:

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
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE, GLOBAL) \
  (((GLOBAL) ? DW_EH_PE_indirect : 0) | DW_EH_PE_absptr)

/* Several named LoongArch patterns depend on Pmode.  These patterns have the
   form <NAME>_si for Pmode == SImode and <NAME>_di for Pmode == DImode.
   Add the appropriate suffix to generator function NAME and invoke it
   with arguments ARGS.  */
#define PMODE_INSN(NAME, ARGS) \
  (Pmode == SImode ? NAME##_si ARGS : NAME##_di ARGS)

/* Do emit .note.GNU-stack by default.  */
#ifndef NEED_INDICATE_EXEC_STACK
#define NEED_INDICATE_EXEC_STACK 1
#endif

/* The `Q' extension is not yet supported.  */
/* TODO: according to march.  */
#define UNITS_PER_FP_REG (TARGET_DOUBLE_FLOAT ? 8 : 4)

/* The largest type that can be passed in floating-point registers.  */
/* TODO: according to mabi.  */
#define UNITS_PER_FP_ARG (TARGET_HARD_FLOAT ? (TARGET_64BIT ? 8 : 4) : 0)

#define FUNCTION_VALUE_REGNO_P(N) ((N) == GP_RETURN || (N) == FP_RETURN)
