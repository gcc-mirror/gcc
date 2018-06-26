/* Definitions of target machine of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2018 Free Software Foundation, Inc.
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

#define ADJUST_INSN_LENGTH(INSN, LENGTH) \
  (LENGTH = nds32_adjust_insn_length (INSN, LENGTH))

/* Use SYMBOL_FLAG_MACH_DEP to define our own symbol_ref flag.
   It is used in nds32_encode_section_info() to store flag in symbol_ref
   in case the symbol should be placed in .rodata section.
   So that we can check it in nds32_legitimate_address_p().  */
#define NDS32_SYMBOL_FLAG_RODATA \
  (SYMBOL_FLAG_MACH_DEP << 0)
#define NDS32_SYMBOL_REF_RODATA_P(x) \
  ((SYMBOL_REF_FLAGS (x) & NDS32_SYMBOL_FLAG_RODATA) != 0)

enum nds32_relax_insn_type
{
  RELAX_ORI,
  RELAX_PLT_ADD,
  RELAX_TLS_ADD_or_LW,
  RELAX_TLS_ADD_LW,
  RELAX_TLS_LW_JRAL,
  RELAX_DONE
};

/* Classifies expand result for expand helper function.  */
enum nds32_expand_result_type
{
  EXPAND_DONE,
  EXPAND_FAIL,
  EXPAND_CREATE_TEMPLATE
};

/* Classifies address type to distinguish 16-bit/32-bit format.  */
enum nds32_16bit_address_type
{
  /* [reg]: 45 format address.  */
  ADDRESS_REG,
  /* [lo_reg + imm3u]: 333 format address.  */
  ADDRESS_LO_REG_IMM3U,
  /* post_inc [lo_reg + imm3u]: 333 format address.  */
  ADDRESS_POST_INC_LO_REG_IMM3U,
  /* post_modify [lo_reg + imm3u]: 333 format address.  */
  ADDRESS_POST_MODIFY_LO_REG_IMM3U,
  /* [$r8 + imm7u]: r8 imply address.  */
  ADDRESS_R8_IMM7U,
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
#define NDS32_MAX_FPR_REGS_FOR_ARGS 6

/* Define the register number for first argument.  */
#define NDS32_GPR_ARG_FIRST_REGNUM 0
#define NDS32_FPR_ARG_FIRST_REGNUM 34

/* Define the register number for return value.  */
#define NDS32_GPR_RET_FIRST_REGNUM 0
#define NDS32_FPR_RET_FIRST_REGNUM 34

/* Define the first integer register number.  */
#define NDS32_FIRST_GPR_REGNUM 0
/* Define the last integer register number.  */
#define NDS32_LAST_GPR_REGNUM 31

#define NDS32_FIRST_CALLEE_SAVE_GPR_REGNUM 6
#define NDS32_LAST_CALLEE_SAVE_GPR_REGNUM \
  (TARGET_REDUCED_REGS ? 10 : 14)

/* Define the floating-point number of registers.  */
#define NDS32_FLOAT_REGISTER_NUMBER                           \
 (((nds32_fp_regnum == NDS32_CONFIG_FPU_0)              \
   || (nds32_fp_regnum == NDS32_CONFIG_FPU_4)) ? 8      \
  : ((nds32_fp_regnum == NDS32_CONFIG_FPU_1)            \
    || (nds32_fp_regnum == NDS32_CONFIG_FPU_5)) ? 16    \
  : ((nds32_fp_regnum == NDS32_CONFIG_FPU_2)            \
    || (nds32_fp_regnum == NDS32_CONFIG_FPU_6)) ? 32    \
  : ((nds32_fp_regnum == NDS32_CONFIG_FPU_3)            \
    || (nds32_fp_regnum == NDS32_CONFIG_FPU_7)) ? 64    \
  : 32)

#define NDS32_EXT_FPU_DOT_E (nds32_fp_regnum >= 4)

/* Define the first floating-point register number.  */
#define NDS32_FIRST_FPR_REGNUM 34
/* Define the last floating-point register number.  */
#define NDS32_LAST_FPR_REGNUM \
  (NDS32_FIRST_FPR_REGNUM + NDS32_FLOAT_REGISTER_NUMBER - 1)


#define NDS32_IS_EXT_FPR_REGNUM(regno) \
  (((regno) >= NDS32_FIRST_FPR_REGNUM + 32) \
   && ((regno) < NDS32_FIRST_FPR_REGNUM + 64))

#define NDS32_IS_FPR_REGNUM(regno) \
  (((regno) >= NDS32_FIRST_FPR_REGNUM) \
   && ((regno) <= NDS32_LAST_FPR_REGNUM))

#define NDS32_FPR_REGNO_OK_FOR_SINGLE(regno) \
  ((regno) <= NDS32_LAST_FPR_REGNUM)

#define NDS32_FPR_REGNO_OK_FOR_DOUBLE(regno) \
  ((((regno) - NDS32_FIRST_FPR_REGNUM) & 1) == 0)

#define NDS32_IS_GPR_REGNUM(regno) \
  (((regno) <= NDS32_LAST_GPR_REGNUM))

/* Define double word alignment bits.  */
#define NDS32_DOUBLE_WORD_ALIGNMENT 64

/* Define alignment checking macros for convenience.  */
#define NDS32_HALF_WORD_ALIGN_P(value)   (((value) & 0x01) == 0)
#define NDS32_SINGLE_WORD_ALIGN_P(value) (((value) & 0x03) == 0)
#define NDS32_DOUBLE_WORD_ALIGN_P(value) (((value) & 0x07) == 0)

/* Determine whether we would like to have code generation strictly aligned.
   We set it strictly aligned when -malways-align is enabled.
   Check gcc/common/config/nds32/nds32-common.c for the optimizations that
   apply -malways-align.  */
#define NDS32_ALIGN_P() (TARGET_ALWAYS_ALIGN)

#define NDS32_EXT_DSP_P() (TARGET_EXT_DSP && !TARGET_FORCE_NO_EXT_DSP)

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
#define NDS32_AVAILABLE_REGNUM_FOR_GPR_ARG(reg_offset, mode, type) \
  ((NDS32_NEED_N_REGS_FOR_ARG (mode, type) > 1)                    \
   ? ((NDS32_MODE_TYPE_ALIGN (mode, type) > PARM_BOUNDARY)         \
      ? (((reg_offset) + NDS32_GPR_ARG_FIRST_REGNUM + 1) & ~1)     \
      : ((reg_offset) + NDS32_GPR_ARG_FIRST_REGNUM))               \
   : ((reg_offset) + NDS32_GPR_ARG_FIRST_REGNUM))

#define NDS32_AVAILABLE_REGNUM_FOR_FPR_ARG(reg_offset, mode, type) \
  ((NDS32_NEED_N_REGS_FOR_ARG (mode, type) > 1)                    \
   ? ((NDS32_MODE_TYPE_ALIGN (mode, type) > PARM_BOUNDARY)         \
      ? (((reg_offset) + NDS32_FPR_ARG_FIRST_REGNUM + 1) & ~1)     \
      : ((reg_offset) + NDS32_FPR_ARG_FIRST_REGNUM))               \
   : ((reg_offset) + NDS32_FPR_ARG_FIRST_REGNUM))

/* These two macros are to check if there are still available registers
   for passing argument, which must be entirely in registers.  */
#define NDS32_ARG_ENTIRE_IN_GPR_REG_P(reg_offset, mode, type)   \
  ((NDS32_AVAILABLE_REGNUM_FOR_GPR_ARG (reg_offset, mode, type) \
    + NDS32_NEED_N_REGS_FOR_ARG (mode, type))                   \
   <= (NDS32_GPR_ARG_FIRST_REGNUM                               \
       + NDS32_MAX_GPR_REGS_FOR_ARGS))

#define NDS32_ARG_ENTIRE_IN_FPR_REG_P(reg_offset, mode, type)   \
  ((NDS32_AVAILABLE_REGNUM_FOR_FPR_ARG (reg_offset, mode, type) \
    + NDS32_NEED_N_REGS_FOR_ARG (mode, type))                   \
   <= (NDS32_FPR_ARG_FIRST_REGNUM                               \
       + NDS32_MAX_FPR_REGS_FOR_ARGS))

/* These two macros are to check if there are still available registers
   for passing argument, either entirely in registers or partially
   in registers.  */
#define NDS32_ARG_PARTIAL_IN_GPR_REG_P(reg_offset, mode, type) \
  (NDS32_AVAILABLE_REGNUM_FOR_GPR_ARG (reg_offset, mode, type) \
   < NDS32_GPR_ARG_FIRST_REGNUM + NDS32_MAX_GPR_REGS_FOR_ARGS)

#define NDS32_ARG_PARTIAL_IN_FPR_REG_P(reg_offset, mode, type) \
  (NDS32_AVAILABLE_REGNUM_FOR_FPR_ARG (reg_offset, mode, type) \
   < NDS32_FPR_ARG_FIRST_REGNUM + NDS32_MAX_FPR_REGS_FOR_ARGS)

/* This macro is to check if the register is required to be saved on stack.
   If call_used_regs[regno] == 0, regno is the callee-saved register.
   If df_regs_ever_live_p(regno) == true, it is used in the current function.
   As long as the register satisfies both criteria above,
   it is required to be saved.  */
#define NDS32_REQUIRED_CALLEE_SAVED_P(regno)                  \
  ((!call_used_regs[regno]) && (df_regs_ever_live_p (regno)))

/* This macro is to check if the push25/pop25 are available to be used
   for code generation.  Because pop25 also performs return behavior,
   the instructions may not be available for some cases.
   If we want to use push25/pop25, all the following conditions must
   be satisfied:
     1. TARGET_V3PUSH is set.
     2. Current function is not an ISR function.
     3. Current function is not a variadic function.*/
#define NDS32_V3PUSH_AVAILABLE_P  \
  (TARGET_V3PUSH \
   && !nds32_isr_function_p (current_function_decl) \
   && (cfun->machine->va_args_size == 0))

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

  /* Number of bytes on the stack for saving floating-point
     callee-saved registers.  */
  int callee_saved_fpr_regs_size;

  /* The padding bytes in callee-saved area may be required.  */
  int callee_saved_area_gpr_padding_bytes;

  /* The first required general purpose callee-saved register.  */
  int callee_saved_first_gpr_regno;
  /* The last required general purpose callee-saved register.  */
  int callee_saved_last_gpr_regno;

  /* The first required floating-point callee-saved register.  */
  int callee_saved_first_fpr_regno;
  /* The last required floating-point callee-saved register.  */
  int callee_saved_last_fpr_regno;

  /* The padding bytes in varargs area may be required.  */
  int va_args_area_padding_bytes;

  /* The first required register that should be saved on stack for va_args.  */
  int va_args_first_regno;
  /* The last required register that should be saved on stack for va_args.  */
  int va_args_last_regno;

  /* Number of bytes on the stack for saving exception handling registers.  */
  int eh_return_data_regs_size;
  /* The first register of passing exception handling information.  */
  int eh_return_data_first_regno;
  /* The last register of passing exception handling information.  */
  int eh_return_data_last_regno;

  /* Indicate that whether this function
     calls __builtin_eh_return.  */
  int use_eh_return_p;

  /* Indicate that whether this function needs
     prologue/epilogue code generation.  */
  int naked_p;
  /* Indicate that whether this function
     uses fp_as_gp optimization.  */
  int fp_as_gp_p;
  /* Indicate that whether this function is under strictly aligned
     situation for legitimate address checking.  This flag informs
     nds32_legitimate_address_p() how to treat offset alignment:
       1. The IVOPT phase needs to detect available range for memory access,
	  such as checking [base + 32767] ~ [base + (-32768)].
	  For this case we do not want address to be strictly aligned.
       2. The rtl lowering and optimization are close to target code.
	  For this case we need address to be strictly aligned.  */
  int strict_aligned_p;

  /* Record two similar attributes status.  */
  int attr_naked_p;
  int attr_no_prologue_p;
};

/* A C structure that contains the arguments information.  */
typedef struct
{
  unsigned int gpr_offset;
  unsigned int fpr_offset;
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
  NDS32_BUILTIN_DSB,
  NDS32_BUILTIN_MSYNC_ALL,
  NDS32_BUILTIN_MSYNC_STORE,
  NDS32_BUILTIN_MFSR,
  NDS32_BUILTIN_MFUSR,
  NDS32_BUILTIN_MTSR,
  NDS32_BUILTIN_MTSR_ISB,
  NDS32_BUILTIN_MTSR_DSB,
  NDS32_BUILTIN_MTUSR,
  NDS32_BUILTIN_SETGIE_EN,
  NDS32_BUILTIN_SETGIE_DIS,
  NDS32_BUILTIN_FMFCFG,
  NDS32_BUILTIN_FMFCSR,
  NDS32_BUILTIN_FMTCSR,
  NDS32_BUILTIN_FCPYNSS,
  NDS32_BUILTIN_FCPYSS,
  NDS32_BUILTIN_FCPYNSD,
  NDS32_BUILTIN_FCPYSD,
  NDS32_BUILTIN_ABS,
  NDS32_BUILTIN_AVE,
  NDS32_BUILTIN_BCLR,
  NDS32_BUILTIN_BSET,
  NDS32_BUILTIN_BTGL,
  NDS32_BUILTIN_BTST,
  NDS32_BUILTIN_CLIP,
  NDS32_BUILTIN_CLIPS,
  NDS32_BUILTIN_CLZ,
  NDS32_BUILTIN_CLO,
  NDS32_BUILTIN_MAX,
  NDS32_BUILTIN_MIN,
  NDS32_BUILTIN_PBSAD,
  NDS32_BUILTIN_PBSADA,
  NDS32_BUILTIN_BSE,
  NDS32_BUILTIN_BSP,
  NDS32_BUILTIN_FFB,
  NDS32_BUILTIN_FFMISM,
  NDS32_BUILTIN_FLMISM,
  NDS32_BUILTIN_KADDW,
  NDS32_BUILTIN_KSUBW,
  NDS32_BUILTIN_KADDH,
  NDS32_BUILTIN_KSUBH,
  NDS32_BUILTIN_KDMBB,
  NDS32_BUILTIN_V_KDMBB,
  NDS32_BUILTIN_KDMBT,
  NDS32_BUILTIN_V_KDMBT,
  NDS32_BUILTIN_KDMTB,
  NDS32_BUILTIN_V_KDMTB,
  NDS32_BUILTIN_KDMTT,
  NDS32_BUILTIN_V_KDMTT,
  NDS32_BUILTIN_KHMBB,
  NDS32_BUILTIN_V_KHMBB,
  NDS32_BUILTIN_KHMBT,
  NDS32_BUILTIN_V_KHMBT,
  NDS32_BUILTIN_KHMTB,
  NDS32_BUILTIN_V_KHMTB,
  NDS32_BUILTIN_KHMTT,
  NDS32_BUILTIN_V_KHMTT,
  NDS32_BUILTIN_KSLRAW,
  NDS32_BUILTIN_KSLRAW_U,
  NDS32_BUILTIN_RDOV,
  NDS32_BUILTIN_CLROV,
  NDS32_BUILTIN_ROTR,
  NDS32_BUILTIN_SVA,
  NDS32_BUILTIN_SVS,
  NDS32_BUILTIN_WSBH,
  NDS32_BUILTIN_JR_ITOFF,
  NDS32_BUILTIN_JR_TOFF,
  NDS32_BUILTIN_JRAL_ITON,
  NDS32_BUILTIN_JRAL_TON,
  NDS32_BUILTIN_RET_ITOFF,
  NDS32_BUILTIN_RET_TOFF,
  NDS32_BUILTIN_STANDBY_NO_WAKE_GRANT,
  NDS32_BUILTIN_STANDBY_WAKE_GRANT,
  NDS32_BUILTIN_STANDBY_WAKE_DONE,
  NDS32_BUILTIN_TEQZ,
  NDS32_BUILTIN_TNEZ,
  NDS32_BUILTIN_TRAP,
  NDS32_BUILTIN_SETEND_BIG,
  NDS32_BUILTIN_SETEND_LITTLE,
  NDS32_BUILTIN_SYSCALL,
  NDS32_BUILTIN_BREAK,
  NDS32_BUILTIN_NOP,
  NDS32_BUILTIN_SCHE_BARRIER,
  NDS32_BUILTIN_GET_CURRENT_SP,
  NDS32_BUILTIN_SET_CURRENT_SP,
  NDS32_BUILTIN_RETURN_ADDRESS,
  NDS32_BUILTIN_LLW,
  NDS32_BUILTIN_LWUP,
  NDS32_BUILTIN_LBUP,
  NDS32_BUILTIN_SCW,
  NDS32_BUILTIN_SWUP,
  NDS32_BUILTIN_SBUP,
  NDS32_BUILTIN_CCTL_VA_LCK,
  NDS32_BUILTIN_CCTL_IDX_WBINVAL,
  NDS32_BUILTIN_CCTL_VA_WBINVAL_L1,
  NDS32_BUILTIN_CCTL_VA_WBINVAL_LA,
  NDS32_BUILTIN_CCTL_IDX_READ,
  NDS32_BUILTIN_CCTL_IDX_WRITE,
  NDS32_BUILTIN_CCTL_L1D_INVALALL,
  NDS32_BUILTIN_CCTL_L1D_WBALL_ALVL,
  NDS32_BUILTIN_CCTL_L1D_WBALL_ONE_LVL,
  NDS32_BUILTIN_DPREF_QW,
  NDS32_BUILTIN_DPREF_HW,
  NDS32_BUILTIN_DPREF_W,
  NDS32_BUILTIN_DPREF_DW,
  NDS32_BUILTIN_TLBOP_TRD,
  NDS32_BUILTIN_TLBOP_TWR,
  NDS32_BUILTIN_TLBOP_RWR,
  NDS32_BUILTIN_TLBOP_RWLK,
  NDS32_BUILTIN_TLBOP_UNLK,
  NDS32_BUILTIN_TLBOP_PB,
  NDS32_BUILTIN_TLBOP_INV,
  NDS32_BUILTIN_TLBOP_FLUA,
  NDS32_BUILTIN_UALOAD_HW,
  NDS32_BUILTIN_UALOAD_W,
  NDS32_BUILTIN_UALOAD_DW,
  NDS32_BUILTIN_UASTORE_HW,
  NDS32_BUILTIN_UASTORE_W,
  NDS32_BUILTIN_UASTORE_DW,
  NDS32_BUILTIN_GIE_DIS,
  NDS32_BUILTIN_GIE_EN,
  NDS32_BUILTIN_ENABLE_INT,
  NDS32_BUILTIN_DISABLE_INT,
  NDS32_BUILTIN_SET_PENDING_SWINT,
  NDS32_BUILTIN_CLR_PENDING_SWINT,
  NDS32_BUILTIN_CLR_PENDING_HWINT,
  NDS32_BUILTIN_GET_ALL_PENDING_INT,
  NDS32_BUILTIN_GET_PENDING_INT,
  NDS32_BUILTIN_SET_INT_PRIORITY,
  NDS32_BUILTIN_GET_INT_PRIORITY,
  NDS32_BUILTIN_SET_TRIG_LEVEL,
  NDS32_BUILTIN_SET_TRIG_EDGE,
  NDS32_BUILTIN_GET_TRIG_TYPE,
  NDS32_BUILTIN_DSP_BEGIN,
  NDS32_BUILTIN_ADD16,
  NDS32_BUILTIN_V_UADD16,
  NDS32_BUILTIN_V_SADD16,
  NDS32_BUILTIN_RADD16,
  NDS32_BUILTIN_V_RADD16,
  NDS32_BUILTIN_URADD16,
  NDS32_BUILTIN_V_URADD16,
  NDS32_BUILTIN_KADD16,
  NDS32_BUILTIN_V_KADD16,
  NDS32_BUILTIN_UKADD16,
  NDS32_BUILTIN_V_UKADD16,
  NDS32_BUILTIN_SUB16,
  NDS32_BUILTIN_V_USUB16,
  NDS32_BUILTIN_V_SSUB16,
  NDS32_BUILTIN_RSUB16,
  NDS32_BUILTIN_V_RSUB16,
  NDS32_BUILTIN_URSUB16,
  NDS32_BUILTIN_V_URSUB16,
  NDS32_BUILTIN_KSUB16,
  NDS32_BUILTIN_V_KSUB16,
  NDS32_BUILTIN_UKSUB16,
  NDS32_BUILTIN_V_UKSUB16,
  NDS32_BUILTIN_CRAS16,
  NDS32_BUILTIN_V_UCRAS16,
  NDS32_BUILTIN_V_SCRAS16,
  NDS32_BUILTIN_RCRAS16,
  NDS32_BUILTIN_V_RCRAS16,
  NDS32_BUILTIN_URCRAS16,
  NDS32_BUILTIN_V_URCRAS16,
  NDS32_BUILTIN_KCRAS16,
  NDS32_BUILTIN_V_KCRAS16,
  NDS32_BUILTIN_UKCRAS16,
  NDS32_BUILTIN_V_UKCRAS16,
  NDS32_BUILTIN_CRSA16,
  NDS32_BUILTIN_V_UCRSA16,
  NDS32_BUILTIN_V_SCRSA16,
  NDS32_BUILTIN_RCRSA16,
  NDS32_BUILTIN_V_RCRSA16,
  NDS32_BUILTIN_URCRSA16,
  NDS32_BUILTIN_V_URCRSA16,
  NDS32_BUILTIN_KCRSA16,
  NDS32_BUILTIN_V_KCRSA16,
  NDS32_BUILTIN_UKCRSA16,
  NDS32_BUILTIN_V_UKCRSA16,
  NDS32_BUILTIN_ADD8,
  NDS32_BUILTIN_V_UADD8,
  NDS32_BUILTIN_V_SADD8,
  NDS32_BUILTIN_RADD8,
  NDS32_BUILTIN_V_RADD8,
  NDS32_BUILTIN_URADD8,
  NDS32_BUILTIN_V_URADD8,
  NDS32_BUILTIN_KADD8,
  NDS32_BUILTIN_V_KADD8,
  NDS32_BUILTIN_UKADD8,
  NDS32_BUILTIN_V_UKADD8,
  NDS32_BUILTIN_SUB8,
  NDS32_BUILTIN_V_USUB8,
  NDS32_BUILTIN_V_SSUB8,
  NDS32_BUILTIN_RSUB8,
  NDS32_BUILTIN_V_RSUB8,
  NDS32_BUILTIN_URSUB8,
  NDS32_BUILTIN_V_URSUB8,
  NDS32_BUILTIN_KSUB8,
  NDS32_BUILTIN_V_KSUB8,
  NDS32_BUILTIN_UKSUB8,
  NDS32_BUILTIN_V_UKSUB8,
  NDS32_BUILTIN_SRA16,
  NDS32_BUILTIN_V_SRA16,
  NDS32_BUILTIN_SRA16_U,
  NDS32_BUILTIN_V_SRA16_U,
  NDS32_BUILTIN_SRL16,
  NDS32_BUILTIN_V_SRL16,
  NDS32_BUILTIN_SRL16_U,
  NDS32_BUILTIN_V_SRL16_U,
  NDS32_BUILTIN_SLL16,
  NDS32_BUILTIN_V_SLL16,
  NDS32_BUILTIN_KSLL16,
  NDS32_BUILTIN_V_KSLL16,
  NDS32_BUILTIN_KSLRA16,
  NDS32_BUILTIN_V_KSLRA16,
  NDS32_BUILTIN_KSLRA16_U,
  NDS32_BUILTIN_V_KSLRA16_U,
  NDS32_BUILTIN_CMPEQ16,
  NDS32_BUILTIN_V_SCMPEQ16,
  NDS32_BUILTIN_V_UCMPEQ16,
  NDS32_BUILTIN_SCMPLT16,
  NDS32_BUILTIN_V_SCMPLT16,
  NDS32_BUILTIN_SCMPLE16,
  NDS32_BUILTIN_V_SCMPLE16,
  NDS32_BUILTIN_UCMPLT16,
  NDS32_BUILTIN_V_UCMPLT16,
  NDS32_BUILTIN_UCMPLE16,
  NDS32_BUILTIN_V_UCMPLE16,
  NDS32_BUILTIN_CMPEQ8,
  NDS32_BUILTIN_V_SCMPEQ8,
  NDS32_BUILTIN_V_UCMPEQ8,
  NDS32_BUILTIN_SCMPLT8,
  NDS32_BUILTIN_V_SCMPLT8,
  NDS32_BUILTIN_SCMPLE8,
  NDS32_BUILTIN_V_SCMPLE8,
  NDS32_BUILTIN_UCMPLT8,
  NDS32_BUILTIN_V_UCMPLT8,
  NDS32_BUILTIN_UCMPLE8,
  NDS32_BUILTIN_V_UCMPLE8,
  NDS32_BUILTIN_SMIN16,
  NDS32_BUILTIN_V_SMIN16,
  NDS32_BUILTIN_UMIN16,
  NDS32_BUILTIN_V_UMIN16,
  NDS32_BUILTIN_SMAX16,
  NDS32_BUILTIN_V_SMAX16,
  NDS32_BUILTIN_UMAX16,
  NDS32_BUILTIN_V_UMAX16,
  NDS32_BUILTIN_SCLIP16,
  NDS32_BUILTIN_V_SCLIP16,
  NDS32_BUILTIN_UCLIP16,
  NDS32_BUILTIN_V_UCLIP16,
  NDS32_BUILTIN_KHM16,
  NDS32_BUILTIN_V_KHM16,
  NDS32_BUILTIN_KHMX16,
  NDS32_BUILTIN_V_KHMX16,
  NDS32_BUILTIN_KABS16,
  NDS32_BUILTIN_V_KABS16,
  NDS32_BUILTIN_SMIN8,
  NDS32_BUILTIN_V_SMIN8,
  NDS32_BUILTIN_UMIN8,
  NDS32_BUILTIN_V_UMIN8,
  NDS32_BUILTIN_SMAX8,
  NDS32_BUILTIN_V_SMAX8,
  NDS32_BUILTIN_UMAX8,
  NDS32_BUILTIN_V_UMAX8,
  NDS32_BUILTIN_KABS8,
  NDS32_BUILTIN_V_KABS8,
  NDS32_BUILTIN_SUNPKD810,
  NDS32_BUILTIN_V_SUNPKD810,
  NDS32_BUILTIN_SUNPKD820,
  NDS32_BUILTIN_V_SUNPKD820,
  NDS32_BUILTIN_SUNPKD830,
  NDS32_BUILTIN_V_SUNPKD830,
  NDS32_BUILTIN_SUNPKD831,
  NDS32_BUILTIN_V_SUNPKD831,
  NDS32_BUILTIN_ZUNPKD810,
  NDS32_BUILTIN_V_ZUNPKD810,
  NDS32_BUILTIN_ZUNPKD820,
  NDS32_BUILTIN_V_ZUNPKD820,
  NDS32_BUILTIN_ZUNPKD830,
  NDS32_BUILTIN_V_ZUNPKD830,
  NDS32_BUILTIN_ZUNPKD831,
  NDS32_BUILTIN_V_ZUNPKD831,
  NDS32_BUILTIN_RADDW,
  NDS32_BUILTIN_URADDW,
  NDS32_BUILTIN_RSUBW,
  NDS32_BUILTIN_URSUBW,
  NDS32_BUILTIN_SRA_U,
  NDS32_BUILTIN_KSLL,
  NDS32_BUILTIN_PKBB16,
  NDS32_BUILTIN_V_PKBB16,
  NDS32_BUILTIN_PKBT16,
  NDS32_BUILTIN_V_PKBT16,
  NDS32_BUILTIN_PKTB16,
  NDS32_BUILTIN_V_PKTB16,
  NDS32_BUILTIN_PKTT16,
  NDS32_BUILTIN_V_PKTT16,
  NDS32_BUILTIN_SMMUL,
  NDS32_BUILTIN_SMMUL_U,
  NDS32_BUILTIN_KMMAC,
  NDS32_BUILTIN_KMMAC_U,
  NDS32_BUILTIN_KMMSB,
  NDS32_BUILTIN_KMMSB_U,
  NDS32_BUILTIN_KWMMUL,
  NDS32_BUILTIN_KWMMUL_U,
  NDS32_BUILTIN_SMMWB,
  NDS32_BUILTIN_V_SMMWB,
  NDS32_BUILTIN_SMMWB_U,
  NDS32_BUILTIN_V_SMMWB_U,
  NDS32_BUILTIN_SMMWT,
  NDS32_BUILTIN_V_SMMWT,
  NDS32_BUILTIN_SMMWT_U,
  NDS32_BUILTIN_V_SMMWT_U,
  NDS32_BUILTIN_KMMAWB,
  NDS32_BUILTIN_V_KMMAWB,
  NDS32_BUILTIN_KMMAWB_U,
  NDS32_BUILTIN_V_KMMAWB_U,
  NDS32_BUILTIN_KMMAWT,
  NDS32_BUILTIN_V_KMMAWT,
  NDS32_BUILTIN_KMMAWT_U,
  NDS32_BUILTIN_V_KMMAWT_U,
  NDS32_BUILTIN_SMBB,
  NDS32_BUILTIN_V_SMBB,
  NDS32_BUILTIN_SMBT,
  NDS32_BUILTIN_V_SMBT,
  NDS32_BUILTIN_SMTT,
  NDS32_BUILTIN_V_SMTT,
  NDS32_BUILTIN_KMDA,
  NDS32_BUILTIN_V_KMDA,
  NDS32_BUILTIN_KMXDA,
  NDS32_BUILTIN_V_KMXDA,
  NDS32_BUILTIN_SMDS,
  NDS32_BUILTIN_V_SMDS,
  NDS32_BUILTIN_SMDRS,
  NDS32_BUILTIN_V_SMDRS,
  NDS32_BUILTIN_SMXDS,
  NDS32_BUILTIN_V_SMXDS,
  NDS32_BUILTIN_KMABB,
  NDS32_BUILTIN_V_KMABB,
  NDS32_BUILTIN_KMABT,
  NDS32_BUILTIN_V_KMABT,
  NDS32_BUILTIN_KMATT,
  NDS32_BUILTIN_V_KMATT,
  NDS32_BUILTIN_KMADA,
  NDS32_BUILTIN_V_KMADA,
  NDS32_BUILTIN_KMAXDA,
  NDS32_BUILTIN_V_KMAXDA,
  NDS32_BUILTIN_KMADS,
  NDS32_BUILTIN_V_KMADS,
  NDS32_BUILTIN_KMADRS,
  NDS32_BUILTIN_V_KMADRS,
  NDS32_BUILTIN_KMAXDS,
  NDS32_BUILTIN_V_KMAXDS,
  NDS32_BUILTIN_KMSDA,
  NDS32_BUILTIN_V_KMSDA,
  NDS32_BUILTIN_KMSXDA,
  NDS32_BUILTIN_V_KMSXDA,
  NDS32_BUILTIN_SMAL,
  NDS32_BUILTIN_V_SMAL,
  NDS32_BUILTIN_BITREV,
  NDS32_BUILTIN_WEXT,
  NDS32_BUILTIN_BPICK,
  NDS32_BUILTIN_INSB,
  NDS32_BUILTIN_SADD64,
  NDS32_BUILTIN_UADD64,
  NDS32_BUILTIN_RADD64,
  NDS32_BUILTIN_URADD64,
  NDS32_BUILTIN_KADD64,
  NDS32_BUILTIN_UKADD64,
  NDS32_BUILTIN_SSUB64,
  NDS32_BUILTIN_USUB64,
  NDS32_BUILTIN_RSUB64,
  NDS32_BUILTIN_URSUB64,
  NDS32_BUILTIN_KSUB64,
  NDS32_BUILTIN_UKSUB64,
  NDS32_BUILTIN_SMAR64,
  NDS32_BUILTIN_SMSR64,
  NDS32_BUILTIN_UMAR64,
  NDS32_BUILTIN_UMSR64,
  NDS32_BUILTIN_KMAR64,
  NDS32_BUILTIN_KMSR64,
  NDS32_BUILTIN_UKMAR64,
  NDS32_BUILTIN_UKMSR64,
  NDS32_BUILTIN_SMALBB,
  NDS32_BUILTIN_V_SMALBB,
  NDS32_BUILTIN_SMALBT,
  NDS32_BUILTIN_V_SMALBT,
  NDS32_BUILTIN_SMALTT,
  NDS32_BUILTIN_V_SMALTT,
  NDS32_BUILTIN_SMALDA,
  NDS32_BUILTIN_V_SMALDA,
  NDS32_BUILTIN_SMALXDA,
  NDS32_BUILTIN_V_SMALXDA,
  NDS32_BUILTIN_SMALDS,
  NDS32_BUILTIN_V_SMALDS,
  NDS32_BUILTIN_SMALDRS,
  NDS32_BUILTIN_V_SMALDRS,
  NDS32_BUILTIN_SMALXDS,
  NDS32_BUILTIN_V_SMALXDS,
  NDS32_BUILTIN_SMUL16,
  NDS32_BUILTIN_V_SMUL16,
  NDS32_BUILTIN_SMULX16,
  NDS32_BUILTIN_V_SMULX16,
  NDS32_BUILTIN_UMUL16,
  NDS32_BUILTIN_V_UMUL16,
  NDS32_BUILTIN_UMULX16,
  NDS32_BUILTIN_V_UMULX16,
  NDS32_BUILTIN_SMSLDA,
  NDS32_BUILTIN_V_SMSLDA,
  NDS32_BUILTIN_SMSLXDA,
  NDS32_BUILTIN_V_SMSLXDA,
  NDS32_BUILTIN_UCLIP32,
  NDS32_BUILTIN_SCLIP32,
  NDS32_BUILTIN_KABS,
  NDS32_BUILTIN_UALOAD_U16,
  NDS32_BUILTIN_UALOAD_S16,
  NDS32_BUILTIN_UALOAD_U8,
  NDS32_BUILTIN_UALOAD_S8,
  NDS32_BUILTIN_UASTORE_U16,
  NDS32_BUILTIN_UASTORE_S16,
  NDS32_BUILTIN_UASTORE_U8,
  NDS32_BUILTIN_UASTORE_S8,
  NDS32_BUILTIN_DSP_END,
  NDS32_BUILTIN_UNALIGNED_FEATURE,
  NDS32_BUILTIN_ENABLE_UNALIGNED,
  NDS32_BUILTIN_DISABLE_UNALIGNED,
  NDS32_BUILTIN_COUNT
};

/* ------------------------------------------------------------------------ */

#define TARGET_ISA_V2   (nds32_arch_option == ARCH_V2)

#define TARGET_ISA_V3 \
  (nds32_arch_option == ARCH_V3 \
   || nds32_arch_option == ARCH_V3J \
   || nds32_arch_option == ARCH_V3F \
   || nds32_arch_option == ARCH_V3S)
#define TARGET_ISA_V3M  (nds32_arch_option == ARCH_V3M)

#define TARGET_PIPELINE_N7 \
  (nds32_cpu_option == CPU_N7)
#define TARGET_PIPELINE_N8 \
  (nds32_cpu_option == CPU_N6 \
   || nds32_cpu_option == CPU_N8)
#define TARGET_PIPELINE_N9 \
  (nds32_cpu_option == CPU_N9)
#define TARGET_PIPELINE_N10 \
  (nds32_cpu_option == CPU_N10)
#define TARGET_PIPELINE_N13 \
  (nds32_cpu_option == CPU_N12 || nds32_cpu_option == CPU_N13)
#define TARGET_PIPELINE_GRAYWOLF \
  (nds32_cpu_option == CPU_GRAYWOLF)
#define TARGET_PIPELINE_SIMPLE \
  (nds32_cpu_option == CPU_SIMPLE)

#define TARGET_CMODEL_SMALL \
   (nds32_cmodel_option == CMODEL_SMALL)
#define TARGET_CMODEL_MEDIUM \
   (nds32_cmodel_option == CMODEL_MEDIUM)
#define TARGET_CMODEL_LARGE \
   (nds32_cmodel_option == CMODEL_LARGE)

#define TARGET_ICT_MODEL_SMALL \
   (nds32_ict_model == ICT_MODEL_SMALL)

#define TARGET_ICT_MODEL_LARGE \
   (nds32_ict_model == ICT_MODEL_LARGE)

/* When -mcmodel=small or -mcmodel=medium,
   compiler may generate gp-base instruction directly.  */
#define TARGET_GP_DIRECT \
   (nds32_cmodel_option == CMODEL_SMALL\
    || nds32_cmodel_option == CMODEL_MEDIUM)

#define TARGET_MUL_SLOW \
  (nds32_mul_config == MUL_TYPE_SLOW)

/* Run-time Target Specification.  */
#define TARGET_SOFT_FLOAT (nds32_abi == NDS32_ABI_V2)
/* Use hardware floating point calling convention.  */
#define TARGET_HARD_FLOAT (nds32_abi == NDS32_ABI_V2_FP_PLUS)

/* Record arch version in TARGET_ARCH_DEFAULT. 0 means soft ABI,
   1 means  hard ABI and using full floating-point instruction,
   2 means hard ABI and only using single-precision floating-point
   instruction  */
#if TARGET_ARCH_DEFAULT == 1
#  define TARGET_DEFAULT_ABI NDS32_ABI_V2_FP_PLUS
#  define TARGET_DEFAULT_FPU_ISA MASK_FPU_DOUBLE | MASK_FPU_SINGLE
#  define TARGET_DEFAULT_FPU_FMA 0
#else
#  if TARGET_ARCH_DEFAULT == 2
#    define TARGET_DEFAULT_ABI NDS32_ABI_V2_FP_PLUS
#    define TARGET_DEFAULT_FPU_ISA MASK_FPU_SINGLE
#    define TARGET_DEFAULT_FPU_FMA 0
#  else
#    define TARGET_DEFAULT_ABI NDS32_ABI_V2
#    define TARGET_DEFAULT_FPU_ISA 0
#    define TARGET_DEFAULT_FPU_FMA 0
#  endif
#endif

#define TARGET_CONFIG_FPU_DEFAULT NDS32_CONFIG_FPU_2

/* ------------------------------------------------------------------------ */

#ifdef TARGET_DEFAULT_RELAX
#  define NDS32_RELAX_SPEC " %{!mno-relax:--relax}"
#else
#  define NDS32_RELAX_SPEC " %{mrelax:--relax}"
#endif

#ifdef TARGET_DEFAULT_EXT_DSP
#  define NDS32_EXT_DSP_SPEC " %{!mno-ext-dsp:-mext-dsp}"
#else
#  define NDS32_EXT_DSP_SPEC ""
#endif

/* ------------------------------------------------------------------------ */

/* Controlling the Compilation Driver.  */

#define OPTION_DEFAULT_SPECS \
  {"arch", " %{!march=*:-march=%(VALUE)}" \
	   " %{march=v3f:%{!mfloat-abi=*:-mfloat-abi=hard}" \
	   " %{!mno-ext-fpu-sp:%{!mext-fpu-sp:-mext-fpu-sp}}" \
	   " %{!mno-ext-fpu-dp:%{!mext-fpu-dp:-mext-fpu-dp}}}" \
	   " %{march=v3s:%{!mfloat-abi=*:-mfloat-abi=hard}" \
	   " %{!mno-ext-fpu-sp:%{!mext-fpu-sp:-mext-fpu-sp}}}" }, \
  {"cpu",  "%{!mcpu=*:-mcpu=%(VALUE)}" },   \
  {"float", "%{!mfloat-abi=*:-mfloat-abi=%(VALUE)}" }

#define CC1_SPEC \
  NDS32_EXT_DSP_SPEC

#define ASM_SPEC \
  " %{mbig-endian:-EB} %{mlittle-endian:-EL}" \
  " %{march=*:-march=%*}" \
  " %{mno-16-bit|mno-16bit:-mno-16bit-ext}" \
  " %{march=v3m:%{!mfull-regs:%{!mreduced-regs:-mreduced-regs}}}" \
  " %{mfull-regs:-mno-reduced-regs}" \
  " %{mreduced-regs:-mreduced-regs}" \
  " %{mabi=*:-mabi=v%*}" \
  " %{mconfig-fpu=*:-mfpu-freg=%*}" \
  " %{mext-fpu-mac:-mmac}" \
  " %{mno-ext-fpu-mac:-mno-mac}" \
  " %{mext-fpu-sp:-mfpu-sp-ext}" \
  " %{mno-ext-fpu-sp:-mno-fpu-sp-ext}" \
  " %{mext-fpu-dp:-mfpu-dp-ext}" \
  " %{mno-ext-fpu-sp:-mno-fpu-dp-ext}" \
  " %{mext-dsp:-mdsp-ext}" \
  " %{O|O1|O2|O3|Ofast:-O1;:-Os}"

/* The TARGET_BIG_ENDIAN_DEFAULT is defined if we
   configure gcc with --target=nds32be-* setting.
   Check gcc/config.gcc for more information.  */
#ifdef TARGET_BIG_ENDIAN_DEFAULT
#  define NDS32_ENDIAN_DEFAULT "mbig-endian"
#else
#  define NDS32_ENDIAN_DEFAULT "mlittle-endian"
#endif

#if TARGET_ELF
#  define NDS32_CMODEL_DEFAULT "mcmodel=medium"
#else
#  define NDS32_CMODEL_DEFAULT "mcmodel=large"
#endif

#define MULTILIB_DEFAULTS \
  { NDS32_ENDIAN_DEFAULT, NDS32_CMODEL_DEFAULT }


/* Run-time Target Specification.  */

#define TARGET_CPU_CPP_BUILTINS() \
  nds32_cpu_cpp_builtins (pfile)


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

#define FUNCTION_BOUNDARY \
  ((NDS32_ALIGN_P () || TARGET_ALIGN_FUNCTION) ? 32 : 16)

#define BIGGEST_ALIGNMENT 64

#define DATA_ALIGNMENT(constant, basic_align) \
  nds32_data_alignment (constant, basic_align)

#define LOCAL_ALIGNMENT(type, basic_align) \
  nds32_local_alignment (type, basic_align)

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
#define WCHAR_TYPE "unsigned int"
#define WCHAR_TYPE_SIZE 32


/* Register Usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.  */
#define FIRST_PSEUDO_REGISTER 101

/* An initializer that says which registers are used for fixed
   purposes all throughout the compiled code and are therefore
   not available for general allocation.

   $r28 : $fp
   $r29 : $gp
   $r30 : $lp
   $r31 : $sp

   caller-save registers: $r0 ~ $r5, $r16 ~ $r23, $fs0 ~ $fs5, $fs22 ~ $fs47
   callee-save registers: $r6 ~ $r10, $r11 ~ $r14, $fs6 ~ $fs21, $fs48 ~ $fs63

   reserved for assembler : $r15
   reserved for other use : $r24, $r25, $r26, $r27 */
#define FIXED_REGISTERS \
{ /* r0   r1   r2   r3   r4   r5   r6   r7   */ \
      0,   0,   0,   0,   0,   0,   0,   0,     \
  /* r8   r9   r10  r11  r12  r13  r14  r15  */ \
      0,   0,   0,   0,   0,   0,   0,   1,     \
  /* r16  r17  r18  r19  r20  r21  r22  r23  */ \
      0,   0,   0,   0,   0,   0,   0,   0,     \
  /* r24  r25  r26  r27  r28  r29  r30  r31  */ \
      1,   1,   1,   1,   0,   1,   0,   1,     \
  /* AP   FP   fs0  fs1  fs2  fs3  fs4  fs5  */ \
      1,   1,   1,   1,   1,   1,   1,   1,     \
  /* fs6  fs7  fs8  fs9  fs10 fs11 fs12 fs13 */ \
      1,   1,   1,   1,   1,   1,   1,   1,     \
  /* fs14 fs15 fs16 fs17 fs18 fs19 fs20 fs21 */ \
      1,   1,   1,   1,   1,   1,   1,   1,     \
  /* fs22 fs23 fs24 fs25 fs26 fs27 fs28 fs29 */ \
      1,   1,   1,   1,   1,   1,   1,   1,     \
  /* fs30 fs31 fd16      fd17      fd18      */ \
      1,   1,   1,   1,   1,   1,   1,   1,     \
  /* fd19      fd20      fd21      fd22      */ \
      1,   1,   1,   1,   1,   1,   1,   1,     \
  /* fd23      fd24      fd25      fd26      */ \
      1,   1,   1,   1,   1,   1,   1,   1,     \
  /* fd27      fd28      fd29      fd30      */ \
      1,   1,   1,   1,   1,   1,   1,   1,     \
  /* fd31      Reserved..................... */ \
      1,   1,   1,   1,   1                     \
}

/* Identifies the registers that are not available for
   general allocation of values that must live across
   function calls -- so they are caller-save registers.

   0 : callee-save registers
   1 : caller-save registers */
#define CALL_USED_REGISTERS \
{ /* r0   r1   r2   r3   r4   r5   r6   r7   */ \
      1,   1,   1,   1,   1,   1,   0,   0,     \
  /* r8   r9   r10  r11  r12  r13  r14  r15  */ \
      0,   0,   0,   0,   0,   0,   0,   1,     \
  /* r16  r17  r18  r19  r20  r21  r22  r23  */ \
      1,   1,   1,   1,   1,   1,   1,   1,     \
  /* r24  r25  r26  r27  r28  r29  r30  r31  */ \
      1,   1,   1,   1,   0,   1,   0,   1,     \
  /* AP   FP   fs0  fs1  fs2  fs3  fs4  fs5  */ \
      1,   1,   1,   1,   1,   1,   1,   1,     \
  /* fs6  fs7  fs8  fs9  fs10 fs11 fs12 fs13 */ \
      1,   1,   1,   1,   1,   1,   1,   1,     \
  /* fs14 fs15 fs16 fs17 fs18 fs19 fs20 fs21 */ \
      1,   1,   1,   1,   1,   1,   1,   1,     \
  /* fs22 fs23 fs24 fs25 fs26 fs27 fs28 fs29 */ \
      1,   1,   1,   1,   1,   1,   1,   1,     \
  /* fs30 fs31 fd16      fd17      fd18      */ \
      1,   1,   1,   1,   1,   1,   1,   1,     \
  /* fd19      fd20      fd21      fd22      */ \
      1,   1,   1,   1,   1,   1,   1,   1,     \
  /* fd23      fd24      fd25      fd26      */ \
      1,   1,   1,   1,   1,   1,   1,   1,     \
  /* fd27      fd28      fd29      fd30      */ \
      1,   1,   1,   1,   1,   1,   1,   1,     \
  /* fd31      Reserved..................... */ \
      1,   1,   1,   1,   1                     \
}

/* In nds32 target, we have three levels of registers:
     LOW_COST_REGS    : $r0 ~ $r7
     MIDDLE_COST_REGS : $r8 ~ $r11, $r16 ~ $r19
     HIGH_COST_REGS   : $r12 ~ $r14, $r20 ~ $r31 */
#define REG_ALLOC_ORDER \
{   0,   1,   2,   3,   4,   5,   6,   7, \
   16,  17,  18,  19,   9,  10,  11,  12, \
   13,  14,  8,   15,  20,  21,  22,  23, \
   24,  25,  26,  27,  28,  29,  30,  31, \
   32,  33,  34,  35,  36,  37,  38,  39, \
   40,  41,  42,  43,  44,  45,  46,  47, \
   48,  49,  50,  51,  52,  53,  54,  55, \
   56,  57,  58,  59,  60,  61,  62,  63, \
   64,  65,  66,  67,  68,  69,  70,  71, \
   72,  73,  74,  75,  76,  77,  78,  79, \
   80,  81,  82,  83,  84,  85,  86,  87, \
   88,  89,  90,  91,  92,  93,  94,  95, \
   96,  97,  98,  99, 100,                \
}

/* ADJUST_REG_ALLOC_ORDER is a macro which permits reg_alloc_order
   to be rearranged based on optimizing for speed or size.  */
#define ADJUST_REG_ALLOC_ORDER nds32_adjust_reg_alloc_order ()

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
  R5_REG,
  R8_REG,
  R15_TA_REG,
  STACK_REG,
  FRAME_POINTER_REG,
  LOW_REGS,
  MIDDLE_REGS,
  HIGH_REGS,
  GENERAL_REGS,
  FRAME_REGS,
  FP_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define REG_CLASS_NAMES \
{                       \
  "NO_REGS",            \
  "R5_REG",             \
  "R8_REG",             \
  "R15_TA_REG",         \
  "STACK_REG",          \
  "FRAME_POINTER_REG",  \
  "LOW_REGS",           \
  "MIDDLE_REGS",        \
  "HIGH_REGS",          \
  "GENERAL_REGS",       \
  "FRAME_REGS",         \
  "FP_REGS",            \
  "ALL_REGS"            \
}

#define REG_CLASS_CONTENTS \
{ /* NO_REGS                                    */  \
  {0x00000000, 0x00000000, 0x00000000, 0x00000000}, \
  /* R5_REG              : 5                    */  \
  {0x00000020, 0x00000000, 0x00000000, 0x00000000}, \
  /* R8_REG              : 8                    */  \
  {0x00000100, 0x00000000, 0x00000000, 0x00000000}, \
  /* R15_TA_REG          : 15                   */  \
  {0x00008000, 0x00000000, 0x00000000, 0x00000000}, \
  /* STACK_REG           : 31                   */  \
  {0x80000000, 0x00000000, 0x00000000, 0x00000000}, \
  /* FRAME_POINTER_REG   : 28                   */  \
  {0x10000000, 0x00000000, 0x00000000, 0x00000000}, \
  /* LOW_REGS            : 0-7                  */  \
  {0x000000ff, 0x00000000, 0x00000000, 0x00000000}, \
  /* MIDDLE_REGS         : 0-11, 16-19          */  \
  {0x000f0fff, 0x00000000, 0x00000000, 0x00000000}, \
  /* HIGH_REGS           : 12-14, 20-31         */  \
  {0xfff07000, 0x00000000, 0x00000000, 0x00000000}, \
  /* GENERAL_REGS        : 0-31                 */  \
  {0xffffffff, 0x00000000, 0x00000000, 0x00000000}, \
  /* FRAME_REGS          : 32, 33               */  \
  {0x00000000, 0x00000003, 0x00000000, 0x00000000}, \
  /* FP_REGS             : 34-98                */  \
  {0x00000000, 0xfffffffc, 0xffffffff, 0x00000003}, \
  /* ALL_REGS            : 0-100                */  \
  {0xffffffff, 0xffffffff, 0xffffffff, 0x0000001f}  \
}

#define REGNO_REG_CLASS(regno) nds32_regno_reg_class (regno)

#define BASE_REG_CLASS GENERAL_REGS
#define INDEX_REG_CLASS GENERAL_REGS

#define TEST_REGNO(R, TEST, VALUE) \
  ((R TEST VALUE) || ((unsigned) reg_renumber[R] TEST VALUE))

/* Return nonzero if it is suitable for use as a
   base register in operand addresses.
   So far, we return nonzero only if "num" is a hard reg
   of the suitable class or a pseudo register which is
   allocated to a suitable hard reg.  */
#define REGNO_OK_FOR_BASE_P(num) \
  (TEST_REGNO (num, <, 32) \
   || TEST_REGNO (num, ==, FRAME_POINTER_REGNUM) \
   || TEST_REGNO (num, ==, ARG_POINTER_REGNUM))

/* Return nonzero if it is suitable for use as a
   index register in operand addresses.
   So far, we return nonzero only if "num" is a hard reg
   of the suitable class or a pseudo register which is
   allocated to a suitable hard reg.
   The difference between an index register and a base register is that
   the index register may be scaled.  */
#define REGNO_OK_FOR_INDEX_P(num) \
  (TEST_REGNO (num, <, 32) \
   || TEST_REGNO (num, ==, FRAME_POINTER_REGNUM) \
   || TEST_REGNO (num, ==, ARG_POINTER_REGNUM))


/* Obsolete Macros for Defining Constraints.  */


/* Stack Layout and Calling Conventions.  */

#define STACK_GROWS_DOWNWARD 1

#define FRAME_GROWS_DOWNWARD 1

#define STACK_POINTER_OFFSET 0

#define FIRST_PARM_OFFSET(fundecl) \
  (NDS32_DOUBLE_WORD_ALIGN_P (crtl->args.pretend_args_size) ? 0 : 4)

/* A C expression whose value is RTL representing the address in a stack frame
   where the pointer to the caller's frame is stored.  */
#define DYNAMIC_CHAIN_ADDRESS(frameaddr) \
  nds32_dynamic_chain_address (frameaddr)

#define RETURN_ADDR_RTX(count, frameaddr) \
  nds32_return_addr_rtx (count, frameaddr)

/* A C expression whose value is RTL representing the location
   of the incoming return address at the beginning of any function
   before the prologue.
   If this RTL is REG, you should also define
   DWARF_FRAME_RETURN_COLUMN to DWARF_FRAME_REGNUM (REGNO).  */
#define INCOMING_RETURN_ADDR_RTX    gen_rtx_REG (Pmode, LP_REGNUM)
#define DWARF_FRAME_RETURN_COLUMN   DWARF_FRAME_REGNUM (LP_REGNUM)

/* Use $r0 $r1 to pass exception handling information.  */
#define EH_RETURN_DATA_REGNO(N) (((N) < 2) ? (N) : INVALID_REGNUM)
/* The register $r2 that represents a location in which to store a stack
   adjustment to be applied before function return.
   This is used to unwind the stack to an exception handler's call frame.  */
#define EH_RETURN_STACKADJ_RTX gen_rtx_REG (Pmode, 2)

#define DBX_REGISTER_NUMBER(REGNO) nds32_dbx_register_number (REGNO)

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

#define FUNCTION_ARG_REGNO_P(regno)                                           \
 (IN_RANGE ((regno), NDS32_FIRST_GPR_REGNUM, NDS32_MAX_GPR_REGS_FOR_ARGS - 1) \
  || ((TARGET_FPU_SINGLE || TARGET_FPU_DOUBLE)                                \
      && IN_RANGE ((regno), NDS32_FPR_ARG_FIRST_REGNUM,                       \
		   NDS32_FIRST_FPR_REGNUM + NDS32_MAX_FPR_REGS_FOR_ARGS - 1)))

#define DEFAULT_PCC_STRUCT_RETURN 0

/* EXIT_IGNORE_STACK should be nonzero if, when returning
   from a function, the stack pointer does not matter.
   The value is tested only in functions that have frame pointers.
   In nds32 target, the function epilogue recovers the
   stack pointer from the frame.  */
#define EXIT_IGNORE_STACK 1

#define FUNCTION_PROFILER(file, labelno) \
  fprintf (file, "/* profiler %d */\n", (labelno))

#define PROFILE_HOOK(LABEL)						\
  {									\
    rtx fun, lp;							\
    lp = get_hard_reg_initial_val (Pmode, LP_REGNUM);			\
    fun = gen_rtx_SYMBOL_REF (Pmode, "_mcount");			\
    emit_library_call (fun, LCT_NORMAL, VOIDmode, lp, Pmode);		\
  }


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

#define USE_LOAD_POST_INCREMENT(mode) \
  nds32_use_load_post_increment(mode)
#define USE_LOAD_POST_DECREMENT(mode) USE_LOAD_POST_INCREMENT(mode)
#define USE_STORE_POST_DECREMENT(mode) USE_LOAD_POST_DECREMENT(mode)
#define USE_STORE_POST_INCREMENT(mode) USE_LOAD_POST_INCREMENT(mode)

#define CONSTANT_ADDRESS_P(x) (CONSTANT_P (x) && GET_CODE (x) != CONST_DOUBLE)

#define MAX_REGS_PER_ADDRESS 3


/* Anchored Addresses.  */


/* Condition Code Status.  */


/* Describing Relative Costs of Operations.  */

/* A C expression for the cost of a branch instruction.
   A value of 1 is the default;
   other values are interpreted relative to that.  */
#define BRANCH_COST(speed_p, predictable_p) ((speed_p) ? 2 : 1)

/* Override BRANCH_COST heuristic which empirically produces worse
   performance for removing short circuiting from the logical ops.  */
#define LOGICAL_OP_NON_SHORT_CIRCUIT 0

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

#define SYMBOLIC_CONST_P(X)	\
(GET_CODE (X) == SYMBOL_REF						\
 || GET_CODE (X) == LABEL_REF						\
 || (GET_CODE (X) == CONST && symbolic_reference_mentioned_p (X)))


/* Defining the Output Assembler Language.  */

#define ASM_COMMENT_START "!"

#define ASM_APP_ON "! #APP\n"

#define ASM_APP_OFF "! #NO_APP\n"

#define ASM_OUTPUT_LABELREF(stream, name) \
  asm_fprintf (stream, "%U%s", (*targetm.strip_name_encoding) (name))

#define ASM_OUTPUT_SYMBOL_REF(stream, sym) \
  assemble_name (stream, XSTR (sym, 0))

#define ASM_OUTPUT_LABEL_REF(stream, buf) \
  assemble_name (stream, buf)

#define LOCAL_LABEL_PREFIX "."

#define REGISTER_NAMES \
{ "$r0",  "$r1",  "$r2",  "$r3",  "$r4",  "$r5",  "$r6",  "$r7",  \
  "$r8",  "$r9",  "$r10", "$r11", "$r12", "$r13", "$r14", "$ta",  \
  "$r16", "$r17", "$r18", "$r19", "$r20", "$r21", "$r22", "$r23", \
  "$r24", "$r25", "$r26", "$r27", "$fp",  "$gp",  "$lp",  "$sp",  \
  "$AP",  "$SFP", "$fs0", "$fs1", "$fs2", "$fs3", "$fs4", "$fs5", \
  "$fs6", "$fs7", "$fs8", "$fs9", "$fs10","$fs11","$fs12","$fs13",\
  "$fs14","$fs15","$fs16","$fs17","$fs18","$fs19","$fs20","$fs21",\
  "$fs22","$fs23","$fs24","$fs25","$fs26","$fs27","$fs28","$fs29",\
  "$fs30","$fs31","$fs32","$fs33","$fs34","$fs35","$fs36","$fs37",\
  "$fs38","$fs39","$fs40","$fs41","$fs42","$fs43","$fs44","$fs45",\
  "$fs46","$fs47","$fs48","$fs49","$fs50","$fs51","$fs52","$fs53",\
  "$fs54","$fs55","$fs56","$fs57","$fs58","$fs59","$fs60","$fs61",\
  "$fs62","$fs63",   "LB",   "LE",   "LC"                         \
}

#define ADDITIONAL_REGISTER_NAMES				\
{								\
  {"$r15", 15},							\
  {"$r28", 28},	{"$r29", 29},	{"$r30", 30},	{"$r31", 31},	\
  {"$a0", 0},	{"$a1", 1},	{"$a2", 2},			\
  {"$a3", 3},	{"$a4", 4},	{"$a5", 5},			\
  {"$s0", 6},	{"$s1", 7},	{"$s2", 8},	{"$s3", 9},	\
  {"$s4", 10},	{"$s5", 11},	{"$s6", 12},	{"$s7", 13},	\
  {"$s8", 14},							\
  {"$t0", 16},	{"$t1", 17},	{"$t2", 18},	{"$t3", 19},	\
  {"$t4", 20},	{"$t5", 21},	{"$t6", 22},	{"$t7", 23},	\
  {"$t8", 24},	{"$t9", 25},					\
  {"$p0", 26},	{"$p1", 27},					\
  {"$h0", 0},	{"$h1", 1},	{"$h2", 2},	{"$h3", 3},	\
  {"$h4", 4},	{"$h5", 5},	{"$h6", 6},	{"$h7", 7},	\
  {"$h8", 8},	{"$h9", 9},	{"$h10", 10},	{"$h11", 11},	\
  {"$h12", 16},	{"$h13", 17},	{"$h14", 18},	{"$h15", 19},	\
  {"$o0", 0},	{"$o1", 1},	{"$o2", 2},	{"$o3", 3},	\
  {"$o4", 4},	{"$o5", 5},	{"$o6", 6},	{"$o7", 7},	\
}

#define OVERLAPPING_REGISTER_NAMES		\
{						\
  {"$fd0",  NDS32_FIRST_FPR_REGNUM + 0,  2},	\
  {"$fd1",  NDS32_FIRST_FPR_REGNUM + 2,  2},	\
  {"$fd2",  NDS32_FIRST_FPR_REGNUM + 4,  2},	\
  {"$fd3",  NDS32_FIRST_FPR_REGNUM + 6,  2},	\
  {"$fd4",  NDS32_FIRST_FPR_REGNUM + 8,  2},	\
  {"$fd5",  NDS32_FIRST_FPR_REGNUM + 10, 2},	\
  {"$fd6",  NDS32_FIRST_FPR_REGNUM + 12, 2},	\
  {"$fd7",  NDS32_FIRST_FPR_REGNUM + 14, 2},	\
  {"$fd8",  NDS32_FIRST_FPR_REGNUM + 16, 2},	\
  {"$fd9",  NDS32_FIRST_FPR_REGNUM + 18, 2},	\
  {"$fd10", NDS32_FIRST_FPR_REGNUM + 20, 2},	\
  {"$fd11", NDS32_FIRST_FPR_REGNUM + 22, 2},	\
  {"$fd12", NDS32_FIRST_FPR_REGNUM + 24, 2},	\
  {"$fd13", NDS32_FIRST_FPR_REGNUM + 26, 2},	\
  {"$fd14", NDS32_FIRST_FPR_REGNUM + 28, 2},	\
  {"$fd15", NDS32_FIRST_FPR_REGNUM + 30, 2},	\
  {"$fd16", NDS32_FIRST_FPR_REGNUM + 32, 2},	\
  {"$fd17", NDS32_FIRST_FPR_REGNUM + 34, 2},	\
  {"$fd18", NDS32_FIRST_FPR_REGNUM + 36, 2},	\
  {"$fd19", NDS32_FIRST_FPR_REGNUM + 38, 2},	\
  {"$fd20", NDS32_FIRST_FPR_REGNUM + 40, 2},	\
  {"$fd21", NDS32_FIRST_FPR_REGNUM + 42, 2},	\
  {"$fd22", NDS32_FIRST_FPR_REGNUM + 44, 2},	\
  {"$fd23", NDS32_FIRST_FPR_REGNUM + 46, 2},	\
  {"$fd24", NDS32_FIRST_FPR_REGNUM + 48, 2},	\
  {"$fd25", NDS32_FIRST_FPR_REGNUM + 50, 2},	\
  {"$fd26", NDS32_FIRST_FPR_REGNUM + 52, 2},	\
  {"$fd27", NDS32_FIRST_FPR_REGNUM + 54, 2},	\
  {"$fd28", NDS32_FIRST_FPR_REGNUM + 56, 2},	\
  {"$fd29", NDS32_FIRST_FPR_REGNUM + 58, 2},	\
  {"$fd30", NDS32_FIRST_FPR_REGNUM + 60, 2},	\
  {"$fd31", NDS32_FIRST_FPR_REGNUM + 62, 2},	\
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
  nds32_case_vector_shorten_mode (min_offset, max_offset, body)

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

/* A C expression that indicates whether the architecture defines a value for
   clz or ctz with a zero operand.  In nds32 clz for 0 result 32 is defined
   in ISA spec */
#define CLZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE)  ((VALUE) = 32, 1)

/* An alias for the machine mode for pointers.  */
#define Pmode SImode

/* An alias for the machine mode used for memory references to functions
   being called, in call RTL expressions.  */
#define FUNCTION_MODE SImode

/* ------------------------------------------------------------------------ */
