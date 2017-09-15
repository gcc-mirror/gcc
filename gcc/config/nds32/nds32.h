/* Definitions of target machine of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2017 Free Software Foundation, Inc.
   Contributed by Andes Technology Corporation.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */


/* ------------------------------------------------------------------------ */

/* The following are auxiliary macros or structure declarations
   that are used all over the nds32.c and nds32.h.  */

/* Use SYMBOL_FLAG_MACH_DEP to define our own symbol_ref flag.
   It is used in nds32_encode_section_info() to store flag in symbol_ref
   in case the symbol should be placed in .rodata section.
   So that we can check it in nds32_legitimate_address_p().  */
#define NDS32_SYMBOL_FLAG_RODATA \
  (SYMBOL_FLAG_MACH_DEP << 0)
#define NDS32_SYMBOL_REF_RODATA_P(x) \
  ((SYMBOL_REF_FLAGS (x) & NDS32_SYMBOL_FLAG_RODATA) != 0)

/* Computing the Length of an Insn.  */
#define ADJUST_INSN_LENGTH(INSN, LENGTH) \
  (LENGTH = nds32_adjust_insn_length (INSN, LENGTH))

/* Check instruction LS-37-FP-implied form.
   Note: actually its immediate range is imm9u
         since it is used for lwi37/swi37 instructions.  */
#define NDS32_LS_37_FP_P(rt, ra, imm)       \
  (REGNO_REG_CLASS (REGNO (rt)) == LOW_REGS \
   && REGNO (ra) == FP_REGNUM               \
   && satisfies_constraint_Iu09 (imm))

/* Check instruction LS-37-SP-implied form.
   Note: actually its immediate range is imm9u
         since it is used for lwi37/swi37 instructions.  */
#define NDS32_LS_37_SP_P(rt, ra, imm)       \
  (REGNO_REG_CLASS (REGNO (rt)) == LOW_REGS \
   && REGNO (ra) == SP_REGNUM               \
   && satisfies_constraint_Iu09 (imm))


/* Check load/store instruction form : Rt3, Ra3, imm3u.  */
#define NDS32_LS_333_P(rt, ra, imm, mode) nds32_ls_333_p (rt, ra, imm, mode)

/* Check load/store instruction form : Rt4, Ra5, const_int_0.
   Note: no need to check ra because Ra5 means it covers all registers.  */
#define NDS32_LS_450_P(rt, ra, imm)                     \
  ((imm == const0_rtx)                                  \
   && (REGNO_REG_CLASS (REGNO (rt)) == LOW_REGS         \
       || REGNO_REG_CLASS (REGNO (rt)) == MIDDLE_REGS))

/* Check instruction RRI-333-form.  */
#define NDS32_RRI_333_P(rt, ra, imm)           \
  (REGNO_REG_CLASS (REGNO (rt)) == LOW_REGS    \
   && REGNO_REG_CLASS (REGNO (ra)) == LOW_REGS \
   && satisfies_constraint_Iu03 (imm))

/* Check instruction RI-45-form.  */
#define NDS32_RI_45_P(rt, ra, imm)                     \
  (REGNO (rt) == REGNO (ra)                            \
   && (REGNO_REG_CLASS (REGNO (rt)) == LOW_REGS        \
       || REGNO_REG_CLASS (REGNO (rt)) == MIDDLE_REGS) \
   && satisfies_constraint_Iu05 (imm))


/* Check instruction RR-33-form.  */
#define NDS32_RR_33_P(rt, ra)                   \
  (REGNO_REG_CLASS (REGNO (rt)) == LOW_REGS     \
   && REGNO_REG_CLASS (REGNO (ra)) == LOW_REGS)

/* Check instruction RRR-333-form.  */
#define NDS32_RRR_333_P(rt, ra, rb)             \
  (REGNO_REG_CLASS (REGNO (rt)) == LOW_REGS     \
   && REGNO_REG_CLASS (REGNO (ra)) == LOW_REGS  \
   && REGNO_REG_CLASS (REGNO (rb)) == LOW_REGS)

/* Check instruction RR-45-form.
   Note: no need to check rb because Rb5 means it covers all registers.  */
#define NDS32_RR_45_P(rt, ra, rb)               \
  (REGNO (rt) == REGNO (ra)                     \
   && (REGNO_REG_CLASS (REGNO (rt)) == LOW_REGS \
       || REGNO_REG_CLASS (REGNO (rt)) == MIDDLE_REGS))

/* Classifies address type to distinguish 16-bit/32-bit format.  */
enum nds32_16bit_address_type
{
  /* [reg]: 45 format address.  */
  ADDRESS_REG,
  /* [lo_reg + imm3u]: 333 format address.  */
  ADDRESS_LO_REG_IMM3U,
  /* post_inc [lo_reg + imm3u]: 333 format address.  */
  ADDRESS_POST_INC_LO_REG_IMM3U,
  /* [$fp + imm7u]: fp imply address.  */
  ADDRESS_FP_IMM7U,
  /* [$sp + imm7u]: sp imply address.  */
  ADDRESS_SP_IMM7U,
  /* Other address format.  */
  ADDRESS_NOT_16BIT_FORMAT
};


/* ------------------------------------------------------------------------ */

/* Define maximum numbers of registers for passing arguments.  */
#define NDS32_MAX_GPR_REGS_FOR_ARGS 6

/* Define the register number for first argument.  */
#define NDS32_GPR_ARG_FIRST_REGNUM 0

/* Define the register number for return value.  */
#define NDS32_GPR_RET_FIRST_REGNUM 0

/* Define the first integer register number.  */
#define NDS32_FIRST_GPR_REGNUM 0
/* Define the last integer register number.  */
#define NDS32_LAST_GPR_REGNUM 31

/* Define double word alignment bits.  */
#define NDS32_DOUBLE_WORD_ALIGNMENT 64

/* Define alignment checking macros for convenience.  */
#define NDS32_HALF_WORD_ALIGN_P(value)   (((value) & 0x01) == 0)
#define NDS32_SINGLE_WORD_ALIGN_P(value) (((value) & 0x03) == 0)
#define NDS32_DOUBLE_WORD_ALIGN_P(value) (((value) & 0x07) == 0)

/* Get alignment according to mode or type information.
   When 'type' is nonnull, there is no need to look at 'mode'.  */
#define NDS32_MODE_TYPE_ALIGN(mode, type) \
  (type ? TYPE_ALIGN (type) : GET_MODE_ALIGNMENT (mode))

/* Round X up to the nearest double word.  */
#define NDS32_ROUND_UP_DOUBLE_WORD(value)  (((value) + 7) & ~7)


/* This macro is used to calculate the numbers of registers for
   containing 'size' bytes of the argument.
   The size of a register is a word in nds32 target.
   So we use UNITS_PER_WORD to do the calculation.  */
#define NDS32_NEED_N_REGS_FOR_ARG(mode, type)                            \
  ((mode == BLKmode)                                                     \
   ? ((int_size_in_bytes (type) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)  \
   : ((GET_MODE_SIZE (mode)     + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* This macro is used to return the register number for passing argument.
   We need to obey the following rules:
     1. If it is required MORE THAN one register,
        we need to further check if it really needs to be
        aligned on double words.
          a) If double word alignment is necessary,
             the register number must be even value.
          b) Otherwise, the register number can be odd or even value.
     2. If it is required ONLY one register,
        the register number can be odd or even value.  */
#define NDS32_AVAILABLE_REGNUM_FOR_GPR_ARG(reg_offset, mode, type)  \
  ((NDS32_NEED_N_REGS_FOR_ARG (mode, type) > 1)                     \
   ? ((NDS32_MODE_TYPE_ALIGN (mode, type) > PARM_BOUNDARY)          \
      ? (((reg_offset) + NDS32_GPR_ARG_FIRST_REGNUM + 1) & ~1)      \
      : ((reg_offset) + NDS32_GPR_ARG_FIRST_REGNUM))                \
   : ((reg_offset) + NDS32_GPR_ARG_FIRST_REGNUM))

/* This macro is to check if there are still available registers
   for passing argument, which must be entirely in registers.  */
#define NDS32_ARG_ENTIRE_IN_GPR_REG_P(reg_offset, mode, type)   \
  ((NDS32_AVAILABLE_REGNUM_FOR_GPR_ARG (reg_offset, mode, type) \
    + NDS32_NEED_N_REGS_FOR_ARG (mode, type))                   \
   <= (NDS32_GPR_ARG_FIRST_REGNUM                               \
       + NDS32_MAX_GPR_REGS_FOR_ARGS))

/* This macro is to check if there are still available registers
   for passing argument, either entirely in registers or partially
   in registers.  */
#define NDS32_ARG_PARTIAL_IN_GPR_REG_P(reg_offset, mode, type) \
  (NDS32_AVAILABLE_REGNUM_FOR_GPR_ARG (reg_offset, mode, type) \
   < NDS32_GPR_ARG_FIRST_REGNUM + NDS32_MAX_GPR_REGS_FOR_ARGS)

/* This macro is to check if the register is required to be saved on stack.
   If call_used_regs[regno] == 0, regno is the callee-saved register.
   If df_regs_ever_live_p(regno) == true, it is used in the current function.
   As long as the register satisfies both criteria above,
   it is required to be saved.  */
#define NDS32_REQUIRED_CALLEE_SAVED_P(regno)                  \
  ((!call_used_regs[regno]) && (df_regs_ever_live_p (regno)))

/* ------------------------------------------------------------------------ */

/* A C structure for machine-specific, per-function data.
   This is added to the cfun structure.  */
struct GTY(()) machine_function
{
  /* Number of bytes allocated on the stack for variadic args
     if we want to push them into stack as pretend arguments by ourself.  */
  int va_args_size;
  /* Number of bytes reserved on the stack for
     local and temporary variables.  */
  int local_size;
  /* Number of bytes allocated on the stack for outgoing arguments.  */
  int out_args_size;

  /* Number of bytes on the stack for saving $fp.  */
  int fp_size;
  /* Number of bytes on the stack for saving $gp.  */
  int gp_size;
  /* Number of bytes on the stack for saving $lp.  */
  int lp_size;

  /* Number of bytes on the stack for saving general purpose
     callee-saved registers.  */
  int callee_saved_gpr_regs_size;

  /* The padding bytes in callee-saved area may be required.  */
  int callee_saved_area_gpr_padding_bytes;

  /* The first required general purpose callee-saved register.  */
  int callee_saved_first_gpr_regno;
  /* The last required general purpose callee-saved register.  */
  int callee_saved_last_gpr_regno;

  /* The padding bytes in varargs area may be required.  */
  int va_args_area_padding_bytes;

  /* The first required register that should be saved on stack for va_args.  */
  int va_args_first_regno;
  /* The last required register that should be saved on stack for va_args.  */
  int va_args_last_regno;

  /* Indicate that whether this function needs
     prologue/epilogue code generation.  */
  int naked_p;
  /* Indicate that whether this function
     uses fp_as_gp optimization.  */
  int fp_as_gp_p;
};

/* A C structure that contains the arguments information.  */
typedef struct
{
  unsigned int gpr_offset;
} nds32_cumulative_args;

/* ------------------------------------------------------------------------ */

/* The following we define C-ISR related stuff.
   In nds32 architecture, we have 73 vectors for interrupt/exception.
   For each vector (except for vector 0, which is used for reset behavior),
   we allow users to set its register saving scheme and interrupt level.  */

/* There are 73 vectors in nds32 architecture.
   0 for reset handler,
   1-8 for exception handler,
   and 9-72 for interrupt handler.
   We use an array, which is defined in nds32.c, to record
   essential information for each vector.  */
#define NDS32_N_ISR_VECTORS 73

/* Define possible isr category.  */
enum nds32_isr_category
{
  NDS32_ISR_NONE,
  NDS32_ISR_INTERRUPT,
  NDS32_ISR_EXCEPTION,
  NDS32_ISR_RESET
};

/* Define isr register saving scheme.  */
enum nds32_isr_save_reg
{
  NDS32_SAVE_ALL,
  NDS32_PARTIAL_SAVE
};

/* Define isr nested type.  */
enum nds32_isr_nested_type
{
  NDS32_NESTED,
  NDS32_NOT_NESTED,
  NDS32_NESTED_READY
};

/* Define structure to record isr information.
   The isr vector array 'isr_vectors[]' with this structure
   is defined in nds32.c.  */
struct nds32_isr_info
{
  /* The field to identify isr category.
     It should be set to NDS32_ISR_NONE by default.
     If user specifies a function as isr by using attribute,
     this field will be set accordingly.  */
  enum nds32_isr_category category;

  /* A string for the applied function name.
     It should be set to empty string by default.  */
  char func_name[100];

  /* The register saving scheme.
     It should be set to NDS32_PARTIAL_SAVE by default
     unless user specifies attribute to change it.  */
  enum nds32_isr_save_reg save_reg;

  /* The nested type.
     It should be set to NDS32_NOT_NESTED by default
     unless user specifies attribute to change it.  */
  enum nds32_isr_nested_type nested_type;

  /* Total vectors.
     The total vectors = interrupt + exception numbers + reset.
     It should be set to 0 by default.
     This field is ONLY used in NDS32_ISR_RESET category.  */
  unsigned int total_n_vectors;

  /* A string for nmi handler name.
     It should be set to empty string by default.
     This field is ONLY used in NDS32_ISR_RESET category.  */
  char nmi_name[100];

  /* A string for warm handler name.
     It should be set to empty string by default.
     This field is ONLY used in NDS32_ISR_RESET category.  */
  char warm_name[100];
};

/* ------------------------------------------------------------------------ */

/* Define code for all nds32 builtins.  */
enum nds32_builtins
{
  NDS32_BUILTIN_ISYNC,
  NDS32_BUILTIN_ISB,
  NDS32_BUILTIN_MFSR,
  NDS32_BUILTIN_MFUSR,
  NDS32_BUILTIN_MTSR,
  NDS32_BUILTIN_MTUSR,
  NDS32_BUILTIN_SETGIE_EN,
  NDS32_BUILTIN_SETGIE_DIS
};

/* ------------------------------------------------------------------------ */

#define TARGET_ISA_V2   (nds32_arch_option == ARCH_V2)
#define TARGET_ISA_V3   (nds32_arch_option == ARCH_V3)
#define TARGET_ISA_V3M  (nds32_arch_option == ARCH_V3M)

#define TARGET_CMODEL_SMALL \
   (nds32_cmodel_option == CMODEL_SMALL)
#define TARGET_CMODEL_MEDIUM \
   (nds32_cmodel_option == CMODEL_MEDIUM)
#define TARGET_CMODEL_LARGE \
   (nds32_cmodel_option == CMODEL_LARGE)

/* When -mcmodel=small or -mcmodel=medium,
   compiler may generate gp-base instruction directly.  */
#define TARGET_GP_DIRECT \
   (nds32_cmodel_option == CMODEL_SMALL\
    || nds32_cmodel_option == CMODEL_MEDIUM)

#define TARGET_SOFT_FLOAT 1
#define TARGET_HARD_FLOAT 0

/* ------------------------------------------------------------------------ */

/* Controlling the Compilation Driver.  */

#define OPTION_DEFAULT_SPECS \
  {"arch", "%{!march=*:-march=%(VALUE)}" }

#define CC1_SPEC \
  ""

#define ASM_SPEC \
  " %{mbig-endian:-EB} %{mlittle-endian:-EL}"

/* If user issues -mrelax, we need to pass '--relax' to linker.  */
#define LINK_SPEC \
  " %{mbig-endian:-EB} %{mlittle-endian:-EL}" \
  " %{mrelax:--relax}"

#define LIB_SPEC \
  " -lc -lgloss"

/* The option -mno-ctor-dtor can disable constructor/destructor feature
   by applying different crt stuff.  In the convention, crt0.o is the
   startup file without constructor/destructor;
   crt1.o, crti.o, crtbegin.o, crtend.o, and crtn.o are the
   startup files with constructor/destructor.
   Note that crt0.o, crt1.o, crti.o, and crtn.o are provided
   by newlib/mculib/glibc/ublic, while crtbegin.o and crtend.o are
   currently provided by GCC for nds32 target.

   For nds32 target so far:
   If -mno-ctor-dtor, we are going to link
   "crt0.o [user objects]".
   If general cases, we are going to link
   "crt1.o crtbegin1.o [user objects] crtend1.o".  */
#define STARTFILE_SPEC \
  " %{!mno-ctor-dtor:crt1.o%s;:crt0.o%s}" \
  " %{!mno-ctor-dtor:crtbegin1.o%s}"
#define ENDFILE_SPEC \
  " %{!mno-ctor-dtor:crtend1.o%s}"

/* The TARGET_BIG_ENDIAN_DEFAULT is defined if we
   configure gcc with --target=nds32be-* setting.
   Check gcc/config.gcc for more information.  */
#ifdef TARGET_BIG_ENDIAN_DEFAULT
#  define NDS32_ENDIAN_DEFAULT "mbig-endian"
#else
#  define NDS32_ENDIAN_DEFAULT "mlittle-endian"
#endif

/* Currently we only have elf toolchain,
   where -mcmodel=medium is always the default.  */
#define NDS32_CMODEL_DEFAULT "mcmodel=medium"

#define MULTILIB_DEFAULTS \
  { NDS32_ENDIAN_DEFAULT, NDS32_CMODEL_DEFAULT }


/* Run-time Target Specification.  */

#define TARGET_CPU_CPP_BUILTINS()                     \
  do                                                  \
    {                                                 \
      builtin_define ("__nds32__");                   \
                                                      \
      if (TARGET_ISA_V2)                              \
        builtin_define ("__NDS32_ISA_V2__");          \
      if (TARGET_ISA_V3)                              \
        builtin_define ("__NDS32_ISA_V3__");          \
      if (TARGET_ISA_V3M)                             \
        builtin_define ("__NDS32_ISA_V3M__");         \
                                                      \
      if (TARGET_BIG_ENDIAN)                          \
        builtin_define ("__big_endian__");            \
      if (TARGET_REDUCED_REGS)                        \
        builtin_define ("__NDS32_REDUCED_REGS__");    \
      if (TARGET_CMOV)                                \
        builtin_define ("__NDS32_CMOV__");            \
      if (TARGET_PERF_EXT)                            \
        builtin_define ("__NDS32_PERF_EXT__");        \
      if (TARGET_16_BIT)                              \
        builtin_define ("__NDS32_16_BIT__");          \
      if (TARGET_GP_DIRECT)                           \
        builtin_define ("__NDS32_GP_DIRECT__");       \
                                                      \
      builtin_assert ("cpu=nds32");                   \
      builtin_assert ("machine=nds32");               \
    } while (0)


/* Defining Data Structures for Per-function Information.  */

/* This macro is called once per function,
   before generation of any RTL has begun.  */
#define INIT_EXPANDERS  nds32_init_expanders ()


/* Storage Layout.  */

#define BITS_BIG_ENDIAN 0

#define BYTES_BIG_ENDIAN (TARGET_BIG_ENDIAN)

#define WORDS_BIG_ENDIAN (TARGET_BIG_ENDIAN)

#define UNITS_PER_WORD 4

#define PROMOTE_MODE(m, unsignedp, type)                                    \
  if (GET_MODE_CLASS (m) == MODE_INT && GET_MODE_SIZE (m) < UNITS_PER_WORD) \
    {                                                                       \
      (m) = SImode;                                                         \
    }

#define PARM_BOUNDARY 32

#define STACK_BOUNDARY 64

#define FUNCTION_BOUNDARY 32

#define BIGGEST_ALIGNMENT 64

#define EMPTY_FIELD_BOUNDARY 32

#define STRUCTURE_SIZE_BOUNDARY 8

#define STRICT_ALIGNMENT 1

#define PCC_BITFIELD_TYPE_MATTERS 1


/* Layout of Source Language Data Types.  */

#define INT_TYPE_SIZE           32
#define SHORT_TYPE_SIZE         16
#define LONG_TYPE_SIZE          32
#define LONG_LONG_TYPE_SIZE     64

#define FLOAT_TYPE_SIZE         32
#define DOUBLE_TYPE_SIZE        64
#define LONG_DOUBLE_TYPE_SIZE   64

#define DEFAULT_SIGNED_CHAR 1

#define SIZE_TYPE "long unsigned int"
#define PTRDIFF_TYPE "long int"
#define WCHAR_TYPE "short unsigned int"
#define WCHAR_TYPE_SIZE 16


/* Register Usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.  */
#define FIRST_PSEUDO_REGISTER 34

/* An initializer that says which registers are used for fixed
   purposes all throughout the compiled code and are therefore
   not available for general allocation.

   $r28 : $fp
   $r29 : $gp
   $r30 : $lp
   $r31 : $sp

   caller-save registers: $r0 ~ $r5, $r16 ~ $r23
   callee-save registers: $r6 ~ $r10, $r11 ~ $r14

   reserved for assembler : $r15
   reserved for other use : $r24, $r25, $r26, $r27 */
#define FIXED_REGISTERS                 \
{ /* r0  r1  r2  r3  r4  r5  r6  r7  */ \
      0,  0,  0,  0,  0,  0,  0,  0,    \
  /* r8  r9  r10 r11 r12 r13 r14 r15 */ \
      0,  0,  0,  0,  0,  0,  0,  1,    \
  /* r16 r17 r18 r19 r20 r21 r22 r23 */ \
      0,  0,  0,  0,  0,  0,  0,  0,    \
  /* r24 r25 r26 r27 r28 r29 r30 r31 */ \
      1,  1,  1,  1,  0,  1,  0,  1,    \
  /* ARG_POINTER:32 */                  \
      1,                                \
  /* FRAME_POINTER:33 */                \
      1                                 \
}

/* Identifies the registers that are not available for
   general allocation of values that must live across
   function calls -- so they are caller-save registers.

   0 : callee-save registers
   1 : caller-save registers */
#define CALL_USED_REGISTERS             \
{ /* r0  r1  r2  r3  r4  r5  r6  r7  */ \
      1,  1,  1,  1,  1,  1,  0,  0,    \
  /* r8  r9  r10 r11 r12 r13 r14 r15 */ \
      0,  0,  0,  0,  0,  0,  0,  1,    \
  /* r16 r17 r18 r19 r20 r21 r22 r23 */ \
      1,  1,  1,  1,  1,  1,  1,  1,    \
  /* r24 r25 r26 r27 r28 r29 r30 r31 */ \
      1,  1,  1,  1,  0,  1,  0,  1,    \
  /* ARG_POINTER:32 */                  \
      1,                                \
  /* FRAME_POINTER:33 */                \
      1                                 \
}

/* In nds32 target, we have three levels of registers:
     LOW_COST_REGS    : $r0 ~ $r7
     MIDDLE_COST_REGS : $r8 ~ $r11, $r16 ~ $r19
     HIGH_COST_REGS   : $r12 ~ $r14, $r20 ~ $r31 */
#define REG_ALLOC_ORDER           \
{                                 \
   0,  1,  2,  3,  4,  5,  6,  7, \
   8,  9, 10, 11, 16, 17, 18, 19, \
  12, 13, 14, 15, 20, 21, 22, 23, \
  24, 25, 26, 27, 28, 29, 30, 31, \
  32,                             \
  33                              \
}

/* Tell IRA to use the order we define rather than messing it up with its
   own cost calculations.  */
#define HONOR_REG_ALLOC_ORDER optimize_size


/* Register Classes.  */

/* In nds32 target, we have three levels of registers:
     Low cost regsiters    : $r0 ~ $r7
     Middle cost registers : $r8 ~ $r11, $r16 ~ $r19
     High cost registers   : $r12 ~ $r14, $r20 ~ $r31

   In practice, we have MIDDLE_REGS cover LOW_REGS register class contents
   so that it provides more chance to use low cost registers.  */
enum reg_class
{
  NO_REGS,
  R15_TA_REG,
  STACK_REG,
  LOW_REGS,
  MIDDLE_REGS,
  HIGH_REGS,
  GENERAL_REGS,
  FRAME_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define REG_CLASS_NAMES \
{                       \
  "NO_REGS",            \
  "R15_TA_REG",         \
  "STACK_REG",          \
  "LOW_REGS",           \
  "MIDDLE_REGS",        \
  "HIGH_REGS",          \
  "GENERAL_REGS",       \
  "FRAME_REGS",         \
  "ALL_REGS"            \
}

#define REG_CLASS_CONTENTS \
{                                                            \
  {0x00000000, 0x00000000}, /* NO_REGS     :              */ \
  {0x00008000, 0x00000000}, /* R15_TA_REG  : 15           */ \
  {0x80000000, 0x00000000}, /* STACK_REG   : 31           */ \
  {0x000000ff, 0x00000000}, /* LOW_REGS    : 0-7          */ \
  {0x000f0fff, 0x00000000}, /* MIDDLE_REGS : 0-11, 16-19  */ \
  {0xfff07000, 0x00000000}, /* HIGH_REGS   : 12-14, 20-31 */ \
  {0xffffffff, 0x00000000}, /* GENERAL_REGS: 0-31         */ \
  {0x00000000, 0x00000003}, /* FRAME_REGS  : 32, 33       */ \
  {0xffffffff, 0x00000003}  /* ALL_REGS    : 0-31, 32, 33 */ \
}

#define REGNO_REG_CLASS(regno) nds32_regno_reg_class (regno)

#define BASE_REG_CLASS GENERAL_REGS
#define INDEX_REG_CLASS GENERAL_REGS

/* Return nonzero if it is suitable for use as a
   base register in operand addresses.
   So far, we return nonzero only if "num" is a hard reg
   of the suitable class or a pseudo register which is
   allocated to a suitable hard reg.  */
#define REGNO_OK_FOR_BASE_P(num) \
  ((num) < 32 || (unsigned) reg_renumber[num] < 32)

/* Return nonzero if it is suitable for use as a
   index register in operand addresses.
   So far, we return nonzero only if "num" is a hard reg
   of the suitable class or a pseudo register which is
   allocated to a suitable hard reg.
   The difference between an index register and a base register is that
   the index register may be scaled.  */
#define REGNO_OK_FOR_INDEX_P(num) \
  ((num) < 32 || (unsigned) reg_renumber[num] < 32)


/* Obsolete Macros for Defining Constraints.  */


/* Stack Layout and Calling Conventions.  */

#define STACK_GROWS_DOWNWARD 1

#define FRAME_GROWS_DOWNWARD 1

#define STARTING_FRAME_OFFSET 0

#define STACK_POINTER_OFFSET 0

#define FIRST_PARM_OFFSET(fundecl) \
  (NDS32_DOUBLE_WORD_ALIGN_P (crtl->args.pretend_args_size) ? 0 : 4)

#define RETURN_ADDR_RTX(count, frameaddr) \
  nds32_return_addr_rtx (count, frameaddr)

/* A C expression whose value is RTL representing the location
   of the incoming return address at the beginning of any function
   before the prologue.
   If this RTL is REG, you should also define
   DWARF_FRAME_RETURN_COLUMN to DWARF_FRAME_REGNUM (REGNO).  */
#define INCOMING_RETURN_ADDR_RTX    gen_rtx_REG (Pmode, LP_REGNUM)
#define DWARF_FRAME_RETURN_COLUMN   DWARF_FRAME_REGNUM (LP_REGNUM)

#define STACK_POINTER_REGNUM SP_REGNUM

#define FRAME_POINTER_REGNUM 33

#define HARD_FRAME_POINTER_REGNUM FP_REGNUM

#define ARG_POINTER_REGNUM 32

#define STATIC_CHAIN_REGNUM 16

#define ELIMINABLE_REGS                                \
{ { ARG_POINTER_REGNUM,   STACK_POINTER_REGNUM },      \
  { ARG_POINTER_REGNUM,   HARD_FRAME_POINTER_REGNUM }, \
  { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM },      \
  { FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM } }

#define INITIAL_ELIMINATION_OFFSET(from_reg, to_reg, offset_var) \
  (offset_var) = nds32_initial_elimination_offset (from_reg, to_reg)

#define ACCUMULATE_OUTGOING_ARGS 1

#define OUTGOING_REG_PARM_STACK_SPACE(fntype) 1

#define CUMULATIVE_ARGS nds32_cumulative_args

#define INIT_CUMULATIVE_ARGS(cum, fntype, libname, fndecl, n_named_args) \
  nds32_init_cumulative_args (&cum, fntype, libname, fndecl, n_named_args)

/* The REGNO is an unsigned integer but NDS32_GPR_ARG_FIRST_REGNUM may be 0.
   We better cast REGNO into signed integer so that we can avoid
   'comparison of unsigned expression >= 0 is always true' warning.  */
#define FUNCTION_ARG_REGNO_P(regno)                                        \
  (((int) regno - NDS32_GPR_ARG_FIRST_REGNUM >= 0)                         \
   && ((int) regno - NDS32_GPR_ARG_FIRST_REGNUM < NDS32_MAX_GPR_REGS_FOR_ARGS))

#define DEFAULT_PCC_STRUCT_RETURN 0

/* EXIT_IGNORE_STACK should be nonzero if, when returning
   from a function, the stack pointer does not matter.
   The value is tested only in functions that have frame pointers.
   In nds32 target, the function epilogue recovers the
   stack pointer from the frame.  */
#define EXIT_IGNORE_STACK 1

#define FUNCTION_PROFILER(file, labelno) \
  fprintf (file, "/* profiler %d */", (labelno))


/* Implementing the Varargs Macros.  */


/* Trampolines for Nested Functions.  */

/* Giving A-function and B-function,
   if B-function wants to call A-function's nested function,
   we need to fill trampoline code into A-function's stack
   so that B-function can execute the code in stack to indirectly
   jump to (like 'trampoline' action) desired nested function.

   The trampoline code for nds32 target must contains following parts:

     1. instructions (4 * 4 = 16 bytes):
          get $pc first
          load chain_value to static chain register via $pc
          load nested function address to $r15 via $pc
          jump to desired nested function via $r15
     2. data (4 * 2 = 8 bytes):
          chain_value
          nested function address

   Please check nds32.c implementation for more information.  */
#define TRAMPOLINE_SIZE 24

/* Because all instructions/data in trampoline template are 4-byte size,
   we set trampoline alignment 8*4=32 bits.  */
#define TRAMPOLINE_ALIGNMENT 32


/* Implicit Calls to Library Routines.  */


/* Addressing Modes.  */

/* We can use "LWI.bi  Rt, [Ra], 4" to support post increment.  */
#define HAVE_POST_INCREMENT 1
/* We can use "LWI.bi  Rt, [Ra], -4" to support post decrement.  */
#define HAVE_POST_DECREMENT 1

/* We have "LWI.bi  Rt, [Ra], imm" instruction form.  */
#define HAVE_POST_MODIFY_DISP 1
/* We have "LW.bi   Rt, [Ra], Rb" instruction form.  */
#define HAVE_POST_MODIFY_REG  1

#define CONSTANT_ADDRESS_P(x) (CONSTANT_P (x) && GET_CODE (x) != CONST_DOUBLE)

#define MAX_REGS_PER_ADDRESS 2


/* Anchored Addresses.  */


/* Condition Code Status.  */


/* Describing Relative Costs of Operations.  */

/* A C expression for the cost of a branch instruction.
   A value of 1 is the default;
   other values are interpreted relative to that.  */
#define BRANCH_COST(speed_p, predictable_p) ((speed_p) ? 2 : 0)

#define SLOW_BYTE_ACCESS 1

#define NO_FUNCTION_CSE 1


/* Adjusting the Instruction Scheduler.  */


/* Dividing the Output into Sections (Texts, Data, . . . ).  */

#define TEXT_SECTION_ASM_OP     "\t.text"
#define DATA_SECTION_ASM_OP     "\t.data"

/* Currently, nds32 assembler does NOT handle '.bss' pseudo-op.
   So we use '.section .bss' alternatively.  */
#define BSS_SECTION_ASM_OP      "\t.section\t.bss"

/* Define this macro to be an expression with a nonzero value if jump tables
   (for tablejump insns) should be output in the text section,
   along with the assembler instructions.
   Otherwise, the readonly data section is used.  */
#define JUMP_TABLES_IN_TEXT_SECTION 1


/* Position Independent Code.  */

#define PIC_OFFSET_TABLE_REGNUM GP_REGNUM


/* Defining the Output Assembler Language.  */

#define ASM_COMMENT_START "!"

#define ASM_APP_ON "! #APP"

#define ASM_APP_OFF "! #NO_APP\n"

#define ASM_OUTPUT_LABELREF(stream, name) \
  asm_fprintf (stream, "%U%s", (*targetm.strip_name_encoding) (name))

#define ASM_OUTPUT_SYMBOL_REF(stream, sym) \
  assemble_name (stream, XSTR (sym, 0))

#define ASM_OUTPUT_LABEL_REF(stream, buf) \
  assemble_name (stream, buf)

#define LOCAL_LABEL_PREFIX "."

#define REGISTER_NAMES                                            \
{                                                                 \
  "$r0",  "$r1",  "$r2",  "$r3",  "$r4",  "$r5",  "$r6",  "$r7",  \
  "$r8",  "$r9",  "$r10", "$r11", "$r12", "$r13", "$r14", "$ta",  \
  "$r16", "$r17", "$r18", "$r19", "$r20", "$r21", "$r22", "$r23", \
  "$r24", "$r25", "$r26", "$r27", "$fp",  "$gp",  "$lp",  "$sp",  \
  "$AP",                                                          \
  "$SFP"                                                          \
}

/* Output normal jump table entry.  */
#define ASM_OUTPUT_ADDR_VEC_ELT(stream, value) \
  asm_fprintf (stream, "\t.word\t%LL%d\n", value)

/* Output pc relative jump table entry.  */
#define ASM_OUTPUT_ADDR_DIFF_ELT(stream, body, value, rel)              \
  do                                                                    \
    {                                                                   \
      switch (GET_MODE (body))                                          \
        {                                                               \
        case E_QImode:                                                    \
          asm_fprintf (stream, "\t.byte\t.L%d-.L%d\n", value, rel);     \
          break;                                                        \
        case E_HImode:                                                    \
          asm_fprintf (stream, "\t.short\t.L%d-.L%d\n", value, rel);    \
          break;                                                        \
        case E_SImode:                                                    \
          asm_fprintf (stream, "\t.word\t.L%d-.L%d\n", value, rel);     \
          break;                                                        \
        default:                                                        \
          gcc_unreachable();                                            \
        }                                                               \
    } while (0)

/* We have to undef it first because elfos.h formerly define it
   check gcc/config.gcc and gcc/config/elfos.h for more information.  */
#undef  ASM_OUTPUT_CASE_LABEL
#define ASM_OUTPUT_CASE_LABEL(stream, prefix, num, table)          \
  do                                                               \
    {                                                              \
      asm_fprintf (stream, "\t! Jump Table Begin\n");              \
      (*targetm.asm_out.internal_label) (stream, prefix, num);     \
    } while (0)

#define ASM_OUTPUT_CASE_END(stream, num, table)        \
  do                                                   \
    {                                                  \
      /* Because our jump table is in text section,    \
         we need to make sure 2-byte alignment after   \
         the jump table for instructions fetch.  */    \
      if (GET_MODE (PATTERN (table)) == QImode)        \
        ASM_OUTPUT_ALIGN (stream, 1);                  \
      asm_fprintf (stream, "\t! Jump Table End\n");    \
    }  while (0)

/* This macro is not documented yet.
   But we do need it to make jump table vector aligned.  */
#define ADDR_VEC_ALIGN(JUMPTABLE) 2

#define DWARF2_UNWIND_INFO 1

#define JUMP_ALIGN(x) \
  (align_jumps_log ? align_jumps_log : nds32_target_alignment (x))

#define LOOP_ALIGN(x) \
  (align_loops_log ? align_loops_log : nds32_target_alignment (x))

#define LABEL_ALIGN(x) \
  (align_labels_log ? align_labels_log : nds32_target_alignment (x))

#define ASM_OUTPUT_ALIGN(stream, power) \
  fprintf (stream, "\t.align\t%d\n", power)


/* Controlling Debugging Information Format.  */

#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

#define DWARF2_DEBUGGING_INFO 1

#define DWARF2_ASM_LINE_DEBUG_INFO 1


/* Cross Compilation and Floating Point.  */


/* Mode Switching Instructions.  */


/* Defining target-specific uses of __attribute__.  */


/* Emulating TLS.  */


/* Defining coprocessor specifics for MIPS targets.  */


/* Parameters for Precompiled Header Validity Checking.  */


/* C++ ABI parameters.  */


/* Adding support for named address spaces.  */


/* Miscellaneous Parameters.  */

/* This is the machine mode that elements of a jump-table should have.  */
#define CASE_VECTOR_MODE Pmode

/* Return the preferred mode for and addr_diff_vec when the mininum
   and maximum offset are known.  */
#define CASE_VECTOR_SHORTEN_MODE(min_offset, max_offset, body)  \
   ((min_offset < 0 || max_offset >= 0x2000 ) ? SImode          \
   : (max_offset >= 100) ? HImode                               \
   : QImode)

/* Generate pc relative jump table when -fpic or -Os.  */
#define CASE_VECTOR_PC_RELATIVE (flag_pic || optimize_size)

/* Define this macro if operations between registers with integral mode
   smaller than a word are always performed on the entire register.  */
#define WORD_REGISTER_OPERATIONS 1

/* A C expression indicating when insns that read memory in mem_mode,
   an integral mode narrower than a word, set the bits outside of mem_mode
   to be either the sign-extension or the zero-extension of the data read.  */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* The maximum number of bytes that a single instruction can move quickly
   between memory and registers or between two memory locations.  */
#define MOVE_MAX 4

/* A C expression that is nonzero if on this machine the number of bits
   actually used for the count of a shift operation is equal to the number
   of bits needed to represent the size of the object being shifted.  */
#define SHIFT_COUNT_TRUNCATED 1

/* A C expression describing the value returned by a comparison operator with
   an integral mode and stored by a store-flag instruction ('cstoremode4')
   when the condition is true.  */
#define STORE_FLAG_VALUE 1

/* An alias for the machine mode for pointers.  */
#define Pmode SImode

/* An alias for the machine mode used for memory references to functions
   being called, in call RTL expressions.  */
#define FUNCTION_MODE SImode

/* ------------------------------------------------------------------------ */
