/* Subroutines used for MIPS code generation.
   Copyright (C) 1989, 1990, 1991, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
   2011
   Free Software Foundation, Inc.
   Contributed by A. Lichnewsky, lich@inria.inria.fr.
   Changes by Michael Meissner, meissner@osf.org.
   64-bit r4000 support by Ian Lance Taylor, ian@cygnus.com, and
   Brendan Eich, brendan@microunity.com.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-attr.h"
#include "recog.h"
#include "output.h"
#include "tree.h"
#include "function.h"
#include "expr.h"
#include "optabs.h"
#include "libfuncs.h"
#include "flags.h"
#include "reload.h"
#include "tm_p.h"
#include "ggc.h"
#include "gstab.h"
#include "hashtab.h"
#include "debug.h"
#include "target.h"
#include "target-def.h"
#include "integrate.h"
#include "langhooks.h"
#include "cfglayout.h"
#include "sched-int.h"
#include "gimple.h"
#include "bitmap.h"
#include "diagnostic.h"
#include "target-globals.h"

/* True if X is an UNSPEC wrapper around a SYMBOL_REF or LABEL_REF.  */
#define UNSPEC_ADDRESS_P(X)					\
  (GET_CODE (X) == UNSPEC					\
   && XINT (X, 1) >= UNSPEC_ADDRESS_FIRST			\
   && XINT (X, 1) < UNSPEC_ADDRESS_FIRST + NUM_SYMBOL_TYPES)

/* Extract the symbol or label from UNSPEC wrapper X.  */
#define UNSPEC_ADDRESS(X) \
  XVECEXP (X, 0, 0)

/* Extract the symbol type from UNSPEC wrapper X.  */
#define UNSPEC_ADDRESS_TYPE(X) \
  ((enum mips_symbol_type) (XINT (X, 1) - UNSPEC_ADDRESS_FIRST))

/* The maximum distance between the top of the stack frame and the
   value $sp has when we save and restore registers.

   The value for normal-mode code must be a SMALL_OPERAND and must
   preserve the maximum stack alignment.  We therefore use a value
   of 0x7ff0 in this case.

   MIPS16e SAVE and RESTORE instructions can adjust the stack pointer by
   up to 0x7f8 bytes and can usually save or restore all the registers
   that we need to save or restore.  (Note that we can only use these
   instructions for o32, for which the stack alignment is 8 bytes.)

   We use a maximum gap of 0x100 or 0x400 for MIPS16 code when SAVE and
   RESTORE are not available.  We can then use unextended instructions
   to save and restore registers, and to allocate and deallocate the top
   part of the frame.  */
#define MIPS_MAX_FIRST_STACK_STEP					\
  (!TARGET_MIPS16 ? 0x7ff0						\
   : GENERATE_MIPS16E_SAVE_RESTORE ? 0x7f8				\
   : TARGET_64BIT ? 0x100 : 0x400)

/* True if INSN is a mips.md pattern or asm statement.  */
#define USEFUL_INSN_P(INSN)						\
  (NONDEBUG_INSN_P (INSN)						\
   && GET_CODE (PATTERN (INSN)) != USE					\
   && GET_CODE (PATTERN (INSN)) != CLOBBER				\
   && GET_CODE (PATTERN (INSN)) != ADDR_VEC				\
   && GET_CODE (PATTERN (INSN)) != ADDR_DIFF_VEC)

/* If INSN is a delayed branch sequence, return the first instruction
   in the sequence, otherwise return INSN itself.  */
#define SEQ_BEGIN(INSN)							\
  (INSN_P (INSN) && GET_CODE (PATTERN (INSN)) == SEQUENCE		\
   ? XVECEXP (PATTERN (INSN), 0, 0)					\
   : (INSN))

/* Likewise for the last instruction in a delayed branch sequence.  */
#define SEQ_END(INSN)							\
  (INSN_P (INSN) && GET_CODE (PATTERN (INSN)) == SEQUENCE		\
   ? XVECEXP (PATTERN (INSN), 0, XVECLEN (PATTERN (INSN), 0) - 1)	\
   : (INSN))

/* Execute the following loop body with SUBINSN set to each instruction
   between SEQ_BEGIN (INSN) and SEQ_END (INSN) inclusive.  */
#define FOR_EACH_SUBINSN(SUBINSN, INSN)					\
  for ((SUBINSN) = SEQ_BEGIN (INSN);					\
       (SUBINSN) != NEXT_INSN (SEQ_END (INSN));				\
       (SUBINSN) = NEXT_INSN (SUBINSN))

/* True if bit BIT is set in VALUE.  */
#define BITSET_P(VALUE, BIT) (((VALUE) & (1 << (BIT))) != 0)

/* Return the opcode for a ptr_mode load of the form:

       l[wd]    DEST, OFFSET(BASE).  */
#define MIPS_LOAD_PTR(DEST, OFFSET, BASE)	\
  (((ptr_mode == DImode ? 0x37 : 0x23) << 26)	\
   | ((BASE) << 21)				\
   | ((DEST) << 16)				\
   | (OFFSET))

/* Return the opcode to move register SRC into register DEST.  */
#define MIPS_MOVE(DEST, SRC)		\
  ((TARGET_64BIT ? 0x2d : 0x21)		\
   | ((DEST) << 11)			\
   | ((SRC) << 21))

/* Return the opcode for:

       lui      DEST, VALUE.  */
#define MIPS_LUI(DEST, VALUE) \
  ((0xf << 26) | ((DEST) << 16) | (VALUE))

/* Return the opcode to jump to register DEST.  */
#define MIPS_JR(DEST) \
  (((DEST) << 21) | 0x8)

/* Return the opcode for:

       bal     . + (1 + OFFSET) * 4.  */
#define MIPS_BAL(OFFSET) \
  ((0x1 << 26) | (0x11 << 16) | (OFFSET))

/* Return the usual opcode for a nop.  */
#define MIPS_NOP 0

/* Classifies an address.

   ADDRESS_REG
       A natural register + offset address.  The register satisfies
       mips_valid_base_register_p and the offset is a const_arith_operand.

   ADDRESS_LO_SUM
       A LO_SUM rtx.  The first operand is a valid base register and
       the second operand is a symbolic address.

   ADDRESS_CONST_INT
       A signed 16-bit constant address.

   ADDRESS_SYMBOLIC:
       A constant symbolic address.  */
enum mips_address_type {
  ADDRESS_REG,
  ADDRESS_LO_SUM,
  ADDRESS_CONST_INT,
  ADDRESS_SYMBOLIC
};

/* Enumerates the setting of the -mr10k-cache-barrier option.  */
enum mips_r10k_cache_barrier_setting {
  R10K_CACHE_BARRIER_NONE,
  R10K_CACHE_BARRIER_STORE,
  R10K_CACHE_BARRIER_LOAD_STORE
};

/* Macros to create an enumeration identifier for a function prototype.  */
#define MIPS_FTYPE_NAME1(A, B) MIPS_##A##_FTYPE_##B
#define MIPS_FTYPE_NAME2(A, B, C) MIPS_##A##_FTYPE_##B##_##C
#define MIPS_FTYPE_NAME3(A, B, C, D) MIPS_##A##_FTYPE_##B##_##C##_##D
#define MIPS_FTYPE_NAME4(A, B, C, D, E) MIPS_##A##_FTYPE_##B##_##C##_##D##_##E

/* Classifies the prototype of a built-in function.  */
enum mips_function_type {
#define DEF_MIPS_FTYPE(NARGS, LIST) MIPS_FTYPE_NAME##NARGS LIST,
#include "config/mips/mips-ftypes.def"
#undef DEF_MIPS_FTYPE
  MIPS_MAX_FTYPE_MAX
};

/* Specifies how a built-in function should be converted into rtl.  */
enum mips_builtin_type {
  /* The function corresponds directly to an .md pattern.  The return
     value is mapped to operand 0 and the arguments are mapped to
     operands 1 and above.  */
  MIPS_BUILTIN_DIRECT,

  /* The function corresponds directly to an .md pattern.  There is no return
     value and the arguments are mapped to operands 0 and above.  */
  MIPS_BUILTIN_DIRECT_NO_TARGET,

  /* The function corresponds to a comparison instruction followed by
     a mips_cond_move_tf_ps pattern.  The first two arguments are the
     values to compare and the second two arguments are the vector
     operands for the movt.ps or movf.ps instruction (in assembly order).  */
  MIPS_BUILTIN_MOVF,
  MIPS_BUILTIN_MOVT,

  /* The function corresponds to a V2SF comparison instruction.  Operand 0
     of this instruction is the result of the comparison, which has mode
     CCV2 or CCV4.  The function arguments are mapped to operands 1 and
     above.  The function's return value is an SImode boolean that is
     true under the following conditions:

     MIPS_BUILTIN_CMP_ANY: one of the registers is true
     MIPS_BUILTIN_CMP_ALL: all of the registers are true
     MIPS_BUILTIN_CMP_LOWER: the first register is true
     MIPS_BUILTIN_CMP_UPPER: the second register is true.  */
  MIPS_BUILTIN_CMP_ANY,
  MIPS_BUILTIN_CMP_ALL,
  MIPS_BUILTIN_CMP_UPPER,
  MIPS_BUILTIN_CMP_LOWER,

  /* As above, but the instruction only sets a single $fcc register.  */
  MIPS_BUILTIN_CMP_SINGLE,

  /* For generating bposge32 branch instructions in MIPS32 DSP ASE.  */
  MIPS_BUILTIN_BPOSGE32
};

/* Invoke MACRO (COND) for each C.cond.fmt condition.  */
#define MIPS_FP_CONDITIONS(MACRO) \
  MACRO (f),	\
  MACRO (un),	\
  MACRO (eq),	\
  MACRO (ueq),	\
  MACRO (olt),	\
  MACRO (ult),	\
  MACRO (ole),	\
  MACRO (ule),	\
  MACRO (sf),	\
  MACRO (ngle),	\
  MACRO (seq),	\
  MACRO (ngl),	\
  MACRO (lt),	\
  MACRO (nge),	\
  MACRO (le),	\
  MACRO (ngt)

/* Enumerates the codes above as MIPS_FP_COND_<X>.  */
#define DECLARE_MIPS_COND(X) MIPS_FP_COND_ ## X
enum mips_fp_condition {
  MIPS_FP_CONDITIONS (DECLARE_MIPS_COND)
};

/* Index X provides the string representation of MIPS_FP_COND_<X>.  */
#define STRINGIFY(X) #X
static const char *const mips_fp_conditions[] = {
  MIPS_FP_CONDITIONS (STRINGIFY)
};

/* Information about a function's frame layout.  */
struct GTY(())  mips_frame_info {
  /* The size of the frame in bytes.  */
  HOST_WIDE_INT total_size;

  /* The number of bytes allocated to variables.  */
  HOST_WIDE_INT var_size;

  /* The number of bytes allocated to outgoing function arguments.  */
  HOST_WIDE_INT args_size;

  /* The number of bytes allocated to the .cprestore slot, or 0 if there
     is no such slot.  */
  HOST_WIDE_INT cprestore_size;

  /* Bit X is set if the function saves or restores GPR X.  */
  unsigned int mask;

  /* Likewise FPR X.  */
  unsigned int fmask;

  /* Likewise doubleword accumulator X ($acX).  */
  unsigned int acc_mask;

  /* The number of GPRs, FPRs, doubleword accumulators and COP0
     registers saved.  */
  unsigned int num_gp;
  unsigned int num_fp;
  unsigned int num_acc;
  unsigned int num_cop0_regs;

  /* The offset of the topmost GPR, FPR, accumulator and COP0-register
     save slots from the top of the frame, or zero if no such slots are
     needed.  */
  HOST_WIDE_INT gp_save_offset;
  HOST_WIDE_INT fp_save_offset;
  HOST_WIDE_INT acc_save_offset;
  HOST_WIDE_INT cop0_save_offset;

  /* Likewise, but giving offsets from the bottom of the frame.  */
  HOST_WIDE_INT gp_sp_offset;
  HOST_WIDE_INT fp_sp_offset;
  HOST_WIDE_INT acc_sp_offset;
  HOST_WIDE_INT cop0_sp_offset;

  /* Similar, but the value passed to _mcount.  */
  HOST_WIDE_INT ra_fp_offset;

  /* The offset of arg_pointer_rtx from the bottom of the frame.  */
  HOST_WIDE_INT arg_pointer_offset;

  /* The offset of hard_frame_pointer_rtx from the bottom of the frame.  */
  HOST_WIDE_INT hard_frame_pointer_offset;
};

struct GTY(())  machine_function {
  /* The register returned by mips16_gp_pseudo_reg; see there for details.  */
  rtx mips16_gp_pseudo_rtx;

  /* The number of extra stack bytes taken up by register varargs.
     This area is allocated by the callee at the very top of the frame.  */
  int varargs_size;

  /* The current frame information, calculated by mips_compute_frame_info.  */
  struct mips_frame_info frame;

  /* The register to use as the function's global pointer, or INVALID_REGNUM
     if the function doesn't need one.  */
  unsigned int global_pointer;

  /* How many instructions it takes to load a label into $AT, or 0 if
     this property hasn't yet been calculated.  */
  unsigned int load_label_num_insns;

  /* True if mips_adjust_insn_length should ignore an instruction's
     hazard attribute.  */
  bool ignore_hazard_length_p;

  /* True if the whole function is suitable for .set noreorder and
     .set nomacro.  */
  bool all_noreorder_p;

  /* True if the function has "inflexible" and "flexible" references
     to the global pointer.  See mips_cfun_has_inflexible_gp_ref_p
     and mips_cfun_has_flexible_gp_ref_p for details.  */
  bool has_inflexible_gp_insn_p;
  bool has_flexible_gp_insn_p;

  /* True if the function's prologue must load the global pointer
     value into pic_offset_table_rtx and store the same value in
     the function's cprestore slot (if any).  Even if this value
     is currently false, we may decide to set it to true later;
     see mips_must_initialize_gp_p () for details.  */
  bool must_initialize_gp_p;

  /* True if the current function must restore $gp after any potential
     clobber.  This value is only meaningful during the first post-epilogue
     split_insns pass; see mips_must_initialize_gp_p () for details.  */
  bool must_restore_gp_when_clobbered_p;

  /* True if this is an interrupt handler.  */
  bool interrupt_handler_p;

  /* True if this is an interrupt handler that uses shadow registers.  */
  bool use_shadow_register_set_p;

  /* True if this is an interrupt handler that should keep interrupts
     masked.  */
  bool keep_interrupts_masked_p;

  /* True if this is an interrupt handler that should use DERET
     instead of ERET.  */
  bool use_debug_exception_return_p;
};

/* Information about a single argument.  */
struct mips_arg_info {
  /* True if the argument is passed in a floating-point register, or
     would have been if we hadn't run out of registers.  */
  bool fpr_p;

  /* The number of words passed in registers, rounded up.  */
  unsigned int reg_words;

  /* For EABI, the offset of the first register from GP_ARG_FIRST or
     FP_ARG_FIRST.  For other ABIs, the offset of the first register from
     the start of the ABI's argument structure (see the CUMULATIVE_ARGS
     comment for details).

     The value is MAX_ARGS_IN_REGISTERS if the argument is passed entirely
     on the stack.  */
  unsigned int reg_offset;

  /* The number of words that must be passed on the stack, rounded up.  */
  unsigned int stack_words;

  /* The offset from the start of the stack overflow area of the argument's
     first stack word.  Only meaningful when STACK_WORDS is nonzero.  */
  unsigned int stack_offset;
};

/* Information about an address described by mips_address_type.

   ADDRESS_CONST_INT
       No fields are used.

   ADDRESS_REG
       REG is the base register and OFFSET is the constant offset.

   ADDRESS_LO_SUM
       REG and OFFSET are the operands to the LO_SUM and SYMBOL_TYPE
       is the type of symbol it references.

   ADDRESS_SYMBOLIC
       SYMBOL_TYPE is the type of symbol that the address references.  */
struct mips_address_info {
  enum mips_address_type type;
  rtx reg;
  rtx offset;
  enum mips_symbol_type symbol_type;
};

/* One stage in a constant building sequence.  These sequences have
   the form:

	A = VALUE[0]
	A = A CODE[1] VALUE[1]
	A = A CODE[2] VALUE[2]
	...

   where A is an accumulator, each CODE[i] is a binary rtl operation
   and each VALUE[i] is a constant integer.  CODE[0] is undefined.  */
struct mips_integer_op {
  enum rtx_code code;
  unsigned HOST_WIDE_INT value;
};

/* The largest number of operations needed to load an integer constant.
   The worst accepted case for 64-bit constants is LUI,ORI,SLL,ORI,SLL,ORI.
   When the lowest bit is clear, we can try, but reject a sequence with
   an extra SLL at the end.  */
#define MIPS_MAX_INTEGER_OPS 7

/* Information about a MIPS16e SAVE or RESTORE instruction.  */
struct mips16e_save_restore_info {
  /* The number of argument registers saved by a SAVE instruction.
     0 for RESTORE instructions.  */
  unsigned int nargs;

  /* Bit X is set if the instruction saves or restores GPR X.  */
  unsigned int mask;

  /* The total number of bytes to allocate.  */
  HOST_WIDE_INT size;
};

/* Costs of various operations on the different architectures.  */

struct mips_rtx_cost_data
{
  unsigned short fp_add;
  unsigned short fp_mult_sf;
  unsigned short fp_mult_df;
  unsigned short fp_div_sf;
  unsigned short fp_div_df;
  unsigned short int_mult_si;
  unsigned short int_mult_di;
  unsigned short int_div_si;
  unsigned short int_div_di;
  unsigned short branch_cost;
  unsigned short memory_latency;
};

/* Global variables for machine-dependent things.  */

/* The -G setting, or the configuration's default small-data limit if
   no -G option is given.  */
static unsigned int mips_small_data_threshold;

/* The number of file directives written by mips_output_filename.  */
int num_source_filenames;

/* The name that appeared in the last .file directive written by
   mips_output_filename, or "" if mips_output_filename hasn't
   written anything yet.  */
const char *current_function_file = "";

/* A label counter used by PUT_SDB_BLOCK_START and PUT_SDB_BLOCK_END.  */
int sdb_label_count;

/* Arrays that map GCC register numbers to debugger register numbers.  */
int mips_dbx_regno[FIRST_PSEUDO_REGISTER];
int mips_dwarf_regno[FIRST_PSEUDO_REGISTER];

/* The nesting depth of the PRINT_OPERAND '%(', '%<' and '%[' constructs.  */
struct mips_asm_switch mips_noreorder = { "reorder", 0 };
struct mips_asm_switch mips_nomacro = { "macro", 0 };
struct mips_asm_switch mips_noat = { "at", 0 };

/* True if we're writing out a branch-likely instruction rather than a
   normal branch.  */
static bool mips_branch_likely;

/* The current instruction-set architecture.  */
enum processor mips_arch;
const struct mips_cpu_info *mips_arch_info;

/* The processor that we should tune the code for.  */
enum processor mips_tune;
const struct mips_cpu_info *mips_tune_info;

/* The ISA level associated with mips_arch.  */
int mips_isa;

/* The architecture selected by -mipsN, or null if -mipsN wasn't used.  */
static const struct mips_cpu_info *mips_isa_option_info;

/* Which ABI to use.  */
int mips_abi = MIPS_ABI_DEFAULT;

/* Which cost information to use.  */
static const struct mips_rtx_cost_data *mips_cost;

/* The ambient target flags, excluding MASK_MIPS16.  */
static int mips_base_target_flags;

/* True if MIPS16 is the default mode.  */
bool mips_base_mips16;

/* The ambient values of other global variables.  */
static int mips_base_schedule_insns; /* flag_schedule_insns */
static int mips_base_reorder_blocks_and_partition; /* flag_reorder... */
static int mips_base_move_loop_invariants; /* flag_move_loop_invariants */
static int mips_base_align_loops; /* align_loops */
static int mips_base_align_jumps; /* align_jumps */
static int mips_base_align_functions; /* align_functions */

/* The -mcode-readable setting.  */
enum mips_code_readable_setting mips_code_readable = CODE_READABLE_YES;

/* The -mr10k-cache-barrier setting.  */
static enum mips_r10k_cache_barrier_setting mips_r10k_cache_barrier;

/* Index [M][R] is true if register R is allowed to hold a value of mode M.  */
bool mips_hard_regno_mode_ok[(int) MAX_MACHINE_MODE][FIRST_PSEUDO_REGISTER];

/* Index C is true if character C is a valid PRINT_OPERAND punctation
   character.  */
static bool mips_print_operand_punct[256];

static GTY (()) int mips_output_filename_first_time = 1;

/* mips_split_p[X] is true if symbols of type X can be split by
   mips_split_symbol.  */
bool mips_split_p[NUM_SYMBOL_TYPES];

/* mips_split_hi_p[X] is true if the high parts of symbols of type X
   can be split by mips_split_symbol.  */
bool mips_split_hi_p[NUM_SYMBOL_TYPES];

/* mips_lo_relocs[X] is the relocation to use when a symbol of type X
   appears in a LO_SUM.  It can be null if such LO_SUMs aren't valid or
   if they are matched by a special .md file pattern.  */
static const char *mips_lo_relocs[NUM_SYMBOL_TYPES];

/* Likewise for HIGHs.  */
static const char *mips_hi_relocs[NUM_SYMBOL_TYPES];

/* Target state for MIPS16.  */
struct target_globals *mips16_globals;

/* Cached value of can_issue_more. This is cached in mips_variable_issue hook
   and returned from mips_sched_reorder2.  */
static int cached_can_issue_more;

/* Index R is the smallest register class that contains register R.  */
const enum reg_class mips_regno_to_class[FIRST_PSEUDO_REGISTER] = {
  LEA_REGS,	LEA_REGS,	M16_REGS,	V1_REG,
  M16_REGS,	M16_REGS,	M16_REGS,	M16_REGS,
  LEA_REGS,	LEA_REGS,	LEA_REGS,	LEA_REGS,
  LEA_REGS,	LEA_REGS,	LEA_REGS,	LEA_REGS,
  M16_REGS,	M16_REGS,	LEA_REGS,	LEA_REGS,
  LEA_REGS,	LEA_REGS,	LEA_REGS,	LEA_REGS,
  T_REG,	PIC_FN_ADDR_REG, LEA_REGS,	LEA_REGS,
  LEA_REGS,	LEA_REGS,	LEA_REGS,	LEA_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  MD0_REG,	MD1_REG,	NO_REGS,	ST_REGS,
  ST_REGS,	ST_REGS,	ST_REGS,	ST_REGS,
  ST_REGS,	ST_REGS,	ST_REGS,	NO_REGS,
  NO_REGS,	FRAME_REGS,	FRAME_REGS,	NO_REGS,
  COP0_REGS,	COP0_REGS,	COP0_REGS,	COP0_REGS,
  COP0_REGS,	COP0_REGS,	COP0_REGS,	COP0_REGS,
  COP0_REGS,	COP0_REGS,	COP0_REGS,	COP0_REGS,
  COP0_REGS,	COP0_REGS,	COP0_REGS,	COP0_REGS,
  COP0_REGS,	COP0_REGS,	COP0_REGS,	COP0_REGS,
  COP0_REGS,	COP0_REGS,	COP0_REGS,	COP0_REGS,
  COP0_REGS,	COP0_REGS,	COP0_REGS,	COP0_REGS,
  COP0_REGS,	COP0_REGS,	COP0_REGS,	COP0_REGS,
  COP2_REGS,	COP2_REGS,	COP2_REGS,	COP2_REGS,
  COP2_REGS,	COP2_REGS,	COP2_REGS,	COP2_REGS,
  COP2_REGS,	COP2_REGS,	COP2_REGS,	COP2_REGS,
  COP2_REGS,	COP2_REGS,	COP2_REGS,	COP2_REGS,
  COP2_REGS,	COP2_REGS,	COP2_REGS,	COP2_REGS,
  COP2_REGS,	COP2_REGS,	COP2_REGS,	COP2_REGS,
  COP2_REGS,	COP2_REGS,	COP2_REGS,	COP2_REGS,
  COP2_REGS,	COP2_REGS,	COP2_REGS,	COP2_REGS,
  COP3_REGS,	COP3_REGS,	COP3_REGS,	COP3_REGS,
  COP3_REGS,	COP3_REGS,	COP3_REGS,	COP3_REGS,
  COP3_REGS,	COP3_REGS,	COP3_REGS,	COP3_REGS,
  COP3_REGS,	COP3_REGS,	COP3_REGS,	COP3_REGS,
  COP3_REGS,	COP3_REGS,	COP3_REGS,	COP3_REGS,
  COP3_REGS,	COP3_REGS,	COP3_REGS,	COP3_REGS,
  COP3_REGS,	COP3_REGS,	COP3_REGS,	COP3_REGS,
  COP3_REGS,	COP3_REGS,	COP3_REGS,	COP3_REGS,
  DSP_ACC_REGS,	DSP_ACC_REGS,	DSP_ACC_REGS,	DSP_ACC_REGS,
  DSP_ACC_REGS,	DSP_ACC_REGS,	ALL_REGS,	ALL_REGS,
  ALL_REGS,	ALL_REGS,	ALL_REGS,	ALL_REGS
};

/* The value of TARGET_ATTRIBUTE_TABLE.  */
static const struct attribute_spec mips_attribute_table[] = {
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler } */
  { "long_call",   0, 0, false, true,  true,  NULL },
  { "far",     	   0, 0, false, true,  true,  NULL },
  { "near",        0, 0, false, true,  true,  NULL },
  /* We would really like to treat "mips16" and "nomips16" as type
     attributes, but GCC doesn't provide the hooks we need to support
     the right conversion rules.  As declaration attributes, they affect
     code generation but don't carry other semantics.  */
  { "mips16", 	   0, 0, true,  false, false, NULL },
  { "nomips16",    0, 0, true,  false, false, NULL },
  /* Allow functions to be specified as interrupt handlers */
  { "interrupt",   0, 0, false, true,  true, NULL },
  { "use_shadow_register_set",	0, 0, false, true,  true, NULL },
  { "keep_interrupts_masked",	0, 0, false, true,  true, NULL },
  { "use_debug_exception_return", 0, 0, false, true,  true, NULL },
  { NULL,	   0, 0, false, false, false, NULL }
};

/* A table describing all the processors GCC knows about.  Names are
   matched in the order listed.  The first mention of an ISA level is
   taken as the canonical name for that ISA.

   To ease comparison, please keep this table in the same order
   as GAS's mips_cpu_info_table.  Please also make sure that
   MIPS_ISA_LEVEL_SPEC and MIPS_ARCH_FLOAT_SPEC handle all -march
   options correctly.  */
static const struct mips_cpu_info mips_cpu_info_table[] = {
  /* Entries for generic ISAs.  */
  { "mips1", PROCESSOR_R3000, 1, 0 },
  { "mips2", PROCESSOR_R6000, 2, 0 },
  { "mips3", PROCESSOR_R4000, 3, 0 },
  { "mips4", PROCESSOR_R8000, 4, 0 },
  /* Prefer not to use branch-likely instructions for generic MIPS32rX
     and MIPS64rX code.  The instructions were officially deprecated
     in revisions 2 and earlier, but revision 3 is likely to downgrade
     that to a recommendation to avoid the instructions in code that
     isn't tuned to a specific processor.  */
  { "mips32", PROCESSOR_4KC, 32, PTF_AVOID_BRANCHLIKELY },
  { "mips32r2", PROCESSOR_M4K, 33, PTF_AVOID_BRANCHLIKELY },
  { "mips64", PROCESSOR_5KC, 64, PTF_AVOID_BRANCHLIKELY },
  /* ??? For now just tune the generic MIPS64r2 for 5KC as well.   */
  { "mips64r2", PROCESSOR_5KC, 65, PTF_AVOID_BRANCHLIKELY },

  /* MIPS I processors.  */
  { "r3000", PROCESSOR_R3000, 1, 0 },
  { "r2000", PROCESSOR_R3000, 1, 0 },
  { "r3900", PROCESSOR_R3900, 1, 0 },

  /* MIPS II processors.  */
  { "r6000", PROCESSOR_R6000, 2, 0 },

  /* MIPS III processors.  */
  { "r4000", PROCESSOR_R4000, 3, 0 },
  { "vr4100", PROCESSOR_R4100, 3, 0 },
  { "vr4111", PROCESSOR_R4111, 3, 0 },
  { "vr4120", PROCESSOR_R4120, 3, 0 },
  { "vr4130", PROCESSOR_R4130, 3, 0 },
  { "vr4300", PROCESSOR_R4300, 3, 0 },
  { "r4400", PROCESSOR_R4000, 3, 0 },
  { "r4600", PROCESSOR_R4600, 3, 0 },
  { "orion", PROCESSOR_R4600, 3, 0 },
  { "r4650", PROCESSOR_R4650, 3, 0 },
  /* ST Loongson 2E/2F processors.  */
  { "loongson2e", PROCESSOR_LOONGSON_2E, 3, PTF_AVOID_BRANCHLIKELY },
  { "loongson2f", PROCESSOR_LOONGSON_2F, 3, PTF_AVOID_BRANCHLIKELY },

  /* MIPS IV processors. */
  { "r8000", PROCESSOR_R8000, 4, 0 },
  { "r10000", PROCESSOR_R10000, 4, 0 },
  { "r12000", PROCESSOR_R10000, 4, 0 },
  { "r14000", PROCESSOR_R10000, 4, 0 },
  { "r16000", PROCESSOR_R10000, 4, 0 },
  { "vr5000", PROCESSOR_R5000, 4, 0 },
  { "vr5400", PROCESSOR_R5400, 4, 0 },
  { "vr5500", PROCESSOR_R5500, 4, PTF_AVOID_BRANCHLIKELY },
  { "rm7000", PROCESSOR_R7000, 4, 0 },
  { "rm9000", PROCESSOR_R9000, 4, 0 },

  /* MIPS32 processors.  */
  { "4kc", PROCESSOR_4KC, 32, 0 },
  { "4km", PROCESSOR_4KC, 32, 0 },
  { "4kp", PROCESSOR_4KP, 32, 0 },
  { "4ksc", PROCESSOR_4KC, 32, 0 },

  /* MIPS32 Release 2 processors.  */
  { "m4k", PROCESSOR_M4K, 33, 0 },
  { "4kec", PROCESSOR_4KC, 33, 0 },
  { "4kem", PROCESSOR_4KC, 33, 0 },
  { "4kep", PROCESSOR_4KP, 33, 0 },
  { "4ksd", PROCESSOR_4KC, 33, 0 },

  { "24kc", PROCESSOR_24KC, 33, 0 },
  { "24kf2_1", PROCESSOR_24KF2_1, 33, 0 },
  { "24kf", PROCESSOR_24KF2_1, 33, 0 },
  { "24kf1_1", PROCESSOR_24KF1_1, 33, 0 },
  { "24kfx", PROCESSOR_24KF1_1, 33, 0 },
  { "24kx", PROCESSOR_24KF1_1, 33, 0 },

  { "24kec", PROCESSOR_24KC, 33, 0 }, /* 24K with DSP.  */
  { "24kef2_1", PROCESSOR_24KF2_1, 33, 0 },
  { "24kef", PROCESSOR_24KF2_1, 33, 0 },
  { "24kef1_1", PROCESSOR_24KF1_1, 33, 0 },
  { "24kefx", PROCESSOR_24KF1_1, 33, 0 },
  { "24kex", PROCESSOR_24KF1_1, 33, 0 },

  { "34kc", PROCESSOR_24KC, 33, 0 }, /* 34K with MT/DSP.  */
  { "34kf2_1", PROCESSOR_24KF2_1, 33, 0 },
  { "34kf", PROCESSOR_24KF2_1, 33, 0 },
  { "34kf1_1", PROCESSOR_24KF1_1, 33, 0 },
  { "34kfx", PROCESSOR_24KF1_1, 33, 0 },
  { "34kx", PROCESSOR_24KF1_1, 33, 0 },

  { "74kc", PROCESSOR_74KC, 33, 0 }, /* 74K with DSPr2.  */
  { "74kf2_1", PROCESSOR_74KF2_1, 33, 0 },
  { "74kf", PROCESSOR_74KF2_1, 33, 0 },
  { "74kf1_1", PROCESSOR_74KF1_1, 33, 0 },
  { "74kfx", PROCESSOR_74KF1_1, 33, 0 },
  { "74kx", PROCESSOR_74KF1_1, 33, 0 },
  { "74kf3_2", PROCESSOR_74KF3_2, 33, 0 },

  { "1004kc", PROCESSOR_24KC, 33, 0 }, /* 1004K with MT/DSP.  */
  { "1004kf2_1", PROCESSOR_24KF2_1, 33, 0 },
  { "1004kf", PROCESSOR_24KF2_1, 33, 0 },
  { "1004kf1_1", PROCESSOR_24KF1_1, 33, 0 },

  /* MIPS64 processors.  */
  { "5kc", PROCESSOR_5KC, 64, 0 },
  { "5kf", PROCESSOR_5KF, 64, 0 },
  { "20kc", PROCESSOR_20KC, 64, PTF_AVOID_BRANCHLIKELY },
  { "sb1", PROCESSOR_SB1, 64, PTF_AVOID_BRANCHLIKELY },
  { "sb1a", PROCESSOR_SB1A, 64, PTF_AVOID_BRANCHLIKELY },
  { "sr71000", PROCESSOR_SR71000, 64, PTF_AVOID_BRANCHLIKELY },
  { "xlr", PROCESSOR_XLR, 64, 0 },
  { "loongson3a", PROCESSOR_LOONGSON_3A, 64, PTF_AVOID_BRANCHLIKELY },

  /* MIPS64 Release 2 processors.  */
  { "octeon", PROCESSOR_OCTEON, 65, PTF_AVOID_BRANCHLIKELY }
};

/* Default costs.  If these are used for a processor we should look
   up the actual costs.  */
#define DEFAULT_COSTS COSTS_N_INSNS (6),  /* fp_add */       \
                      COSTS_N_INSNS (7),  /* fp_mult_sf */   \
                      COSTS_N_INSNS (8),  /* fp_mult_df */   \
                      COSTS_N_INSNS (23), /* fp_div_sf */    \
                      COSTS_N_INSNS (36), /* fp_div_df */    \
                      COSTS_N_INSNS (10), /* int_mult_si */  \
                      COSTS_N_INSNS (10), /* int_mult_di */  \
                      COSTS_N_INSNS (69), /* int_div_si */   \
                      COSTS_N_INSNS (69), /* int_div_di */   \
                                       2, /* branch_cost */  \
                                       4  /* memory_latency */

/* Floating-point costs for processors without an FPU.  Just assume that
   all floating-point libcalls are very expensive.  */
#define SOFT_FP_COSTS COSTS_N_INSNS (256), /* fp_add */       \
                      COSTS_N_INSNS (256), /* fp_mult_sf */   \
                      COSTS_N_INSNS (256), /* fp_mult_df */   \
                      COSTS_N_INSNS (256), /* fp_div_sf */    \
                      COSTS_N_INSNS (256)  /* fp_div_df */

/* Costs to use when optimizing for size.  */
static const struct mips_rtx_cost_data mips_rtx_cost_optimize_size = {
  COSTS_N_INSNS (1),            /* fp_add */
  COSTS_N_INSNS (1),            /* fp_mult_sf */
  COSTS_N_INSNS (1),            /* fp_mult_df */
  COSTS_N_INSNS (1),            /* fp_div_sf */
  COSTS_N_INSNS (1),            /* fp_div_df */
  COSTS_N_INSNS (1),            /* int_mult_si */
  COSTS_N_INSNS (1),            /* int_mult_di */
  COSTS_N_INSNS (1),            /* int_div_si */
  COSTS_N_INSNS (1),            /* int_div_di */
		   2,           /* branch_cost */
		   4            /* memory_latency */
};

/* Costs to use when optimizing for speed, indexed by processor.  */
static const struct mips_rtx_cost_data
  mips_rtx_cost_data[NUM_PROCESSOR_VALUES] = {
  { /* R3000 */
    COSTS_N_INSNS (2),            /* fp_add */
    COSTS_N_INSNS (4),            /* fp_mult_sf */
    COSTS_N_INSNS (5),            /* fp_mult_df */
    COSTS_N_INSNS (12),           /* fp_div_sf */
    COSTS_N_INSNS (19),           /* fp_div_df */
    COSTS_N_INSNS (12),           /* int_mult_si */
    COSTS_N_INSNS (12),           /* int_mult_di */
    COSTS_N_INSNS (35),           /* int_div_si */
    COSTS_N_INSNS (35),           /* int_div_di */
		     1,           /* branch_cost */
		     4            /* memory_latency */
  },
  { /* 4KC */
    SOFT_FP_COSTS,
    COSTS_N_INSNS (6),            /* int_mult_si */
    COSTS_N_INSNS (6),            /* int_mult_di */
    COSTS_N_INSNS (36),           /* int_div_si */
    COSTS_N_INSNS (36),           /* int_div_di */
		     1,           /* branch_cost */
		     4            /* memory_latency */
  },
  { /* 4KP */
    SOFT_FP_COSTS,
    COSTS_N_INSNS (36),           /* int_mult_si */
    COSTS_N_INSNS (36),           /* int_mult_di */
    COSTS_N_INSNS (37),           /* int_div_si */
    COSTS_N_INSNS (37),           /* int_div_di */
		     1,           /* branch_cost */
		     4            /* memory_latency */
  },
  { /* 5KC */
    SOFT_FP_COSTS,
    COSTS_N_INSNS (4),            /* int_mult_si */
    COSTS_N_INSNS (11),           /* int_mult_di */
    COSTS_N_INSNS (36),           /* int_div_si */
    COSTS_N_INSNS (68),           /* int_div_di */
		     1,           /* branch_cost */
		     4            /* memory_latency */
  },
  { /* 5KF */
    COSTS_N_INSNS (4),            /* fp_add */
    COSTS_N_INSNS (4),            /* fp_mult_sf */
    COSTS_N_INSNS (5),            /* fp_mult_df */
    COSTS_N_INSNS (17),           /* fp_div_sf */
    COSTS_N_INSNS (32),           /* fp_div_df */
    COSTS_N_INSNS (4),            /* int_mult_si */
    COSTS_N_INSNS (11),           /* int_mult_di */
    COSTS_N_INSNS (36),           /* int_div_si */
    COSTS_N_INSNS (68),           /* int_div_di */
		     1,           /* branch_cost */
		     4            /* memory_latency */
  },
  { /* 20KC */
    COSTS_N_INSNS (4),            /* fp_add */
    COSTS_N_INSNS (4),            /* fp_mult_sf */
    COSTS_N_INSNS (5),            /* fp_mult_df */
    COSTS_N_INSNS (17),           /* fp_div_sf */
    COSTS_N_INSNS (32),           /* fp_div_df */
    COSTS_N_INSNS (4),            /* int_mult_si */
    COSTS_N_INSNS (7),            /* int_mult_di */
    COSTS_N_INSNS (42),           /* int_div_si */
    COSTS_N_INSNS (72),           /* int_div_di */
		     1,           /* branch_cost */
		     4            /* memory_latency */
  },
  { /* 24KC */
    SOFT_FP_COSTS,
    COSTS_N_INSNS (5),            /* int_mult_si */
    COSTS_N_INSNS (5),            /* int_mult_di */
    COSTS_N_INSNS (41),           /* int_div_si */
    COSTS_N_INSNS (41),           /* int_div_di */
		     1,           /* branch_cost */
		     4            /* memory_latency */
  },
  { /* 24KF2_1 */
    COSTS_N_INSNS (8),            /* fp_add */
    COSTS_N_INSNS (8),            /* fp_mult_sf */
    COSTS_N_INSNS (10),           /* fp_mult_df */
    COSTS_N_INSNS (34),           /* fp_div_sf */
    COSTS_N_INSNS (64),           /* fp_div_df */
    COSTS_N_INSNS (5),            /* int_mult_si */
    COSTS_N_INSNS (5),            /* int_mult_di */
    COSTS_N_INSNS (41),           /* int_div_si */
    COSTS_N_INSNS (41),           /* int_div_di */
		     1,           /* branch_cost */
		     4            /* memory_latency */
  },
  { /* 24KF1_1 */
    COSTS_N_INSNS (4),            /* fp_add */
    COSTS_N_INSNS (4),            /* fp_mult_sf */
    COSTS_N_INSNS (5),            /* fp_mult_df */
    COSTS_N_INSNS (17),           /* fp_div_sf */
    COSTS_N_INSNS (32),           /* fp_div_df */
    COSTS_N_INSNS (5),            /* int_mult_si */
    COSTS_N_INSNS (5),            /* int_mult_di */
    COSTS_N_INSNS (41),           /* int_div_si */
    COSTS_N_INSNS (41),           /* int_div_di */
		     1,           /* branch_cost */
		     4            /* memory_latency */
  },
  { /* 74KC */
    SOFT_FP_COSTS,
    COSTS_N_INSNS (5),            /* int_mult_si */
    COSTS_N_INSNS (5),            /* int_mult_di */
    COSTS_N_INSNS (41),           /* int_div_si */
    COSTS_N_INSNS (41),           /* int_div_di */
		     1,           /* branch_cost */
		     4            /* memory_latency */
  },
  { /* 74KF2_1 */
    COSTS_N_INSNS (8),            /* fp_add */
    COSTS_N_INSNS (8),            /* fp_mult_sf */
    COSTS_N_INSNS (10),           /* fp_mult_df */
    COSTS_N_INSNS (34),           /* fp_div_sf */
    COSTS_N_INSNS (64),           /* fp_div_df */
    COSTS_N_INSNS (5),            /* int_mult_si */
    COSTS_N_INSNS (5),            /* int_mult_di */
    COSTS_N_INSNS (41),           /* int_div_si */
    COSTS_N_INSNS (41),           /* int_div_di */
		     1,           /* branch_cost */
		     4            /* memory_latency */
  },
  { /* 74KF1_1 */
    COSTS_N_INSNS (4),            /* fp_add */
    COSTS_N_INSNS (4),            /* fp_mult_sf */
    COSTS_N_INSNS (5),            /* fp_mult_df */
    COSTS_N_INSNS (17),           /* fp_div_sf */
    COSTS_N_INSNS (32),           /* fp_div_df */
    COSTS_N_INSNS (5),            /* int_mult_si */
    COSTS_N_INSNS (5),            /* int_mult_di */
    COSTS_N_INSNS (41),           /* int_div_si */
    COSTS_N_INSNS (41),           /* int_div_di */
		     1,           /* branch_cost */
		     4            /* memory_latency */
  },
  { /* 74KF3_2 */
    COSTS_N_INSNS (6),            /* fp_add */
    COSTS_N_INSNS (6),            /* fp_mult_sf */
    COSTS_N_INSNS (7),            /* fp_mult_df */
    COSTS_N_INSNS (25),           /* fp_div_sf */
    COSTS_N_INSNS (48),           /* fp_div_df */
    COSTS_N_INSNS (5),            /* int_mult_si */
    COSTS_N_INSNS (5),            /* int_mult_di */
    COSTS_N_INSNS (41),           /* int_div_si */
    COSTS_N_INSNS (41),           /* int_div_di */
		     1,           /* branch_cost */
		     4            /* memory_latency */
  },
  { /* Loongson-2E */
    DEFAULT_COSTS
  },
  { /* Loongson-2F */
    DEFAULT_COSTS
  },
  { /* Loongson-3A */
    DEFAULT_COSTS
  },
  { /* M4k */
    DEFAULT_COSTS
  },
    /* Octeon */
  {
    SOFT_FP_COSTS,
    COSTS_N_INSNS (5),            /* int_mult_si */
    COSTS_N_INSNS (5),            /* int_mult_di */
    COSTS_N_INSNS (72),           /* int_div_si */
    COSTS_N_INSNS (72),           /* int_div_di */
                     1,		  /* branch_cost */
                     4		  /* memory_latency */
  },
  { /* R3900 */
    COSTS_N_INSNS (2),            /* fp_add */
    COSTS_N_INSNS (4),            /* fp_mult_sf */
    COSTS_N_INSNS (5),            /* fp_mult_df */
    COSTS_N_INSNS (12),           /* fp_div_sf */
    COSTS_N_INSNS (19),           /* fp_div_df */
    COSTS_N_INSNS (2),            /* int_mult_si */
    COSTS_N_INSNS (2),            /* int_mult_di */
    COSTS_N_INSNS (35),           /* int_div_si */
    COSTS_N_INSNS (35),           /* int_div_di */
		     1,           /* branch_cost */
		     4            /* memory_latency */
  },
  { /* R6000 */
    COSTS_N_INSNS (3),            /* fp_add */
    COSTS_N_INSNS (5),            /* fp_mult_sf */
    COSTS_N_INSNS (6),            /* fp_mult_df */
    COSTS_N_INSNS (15),           /* fp_div_sf */
    COSTS_N_INSNS (16),           /* fp_div_df */
    COSTS_N_INSNS (17),           /* int_mult_si */
    COSTS_N_INSNS (17),           /* int_mult_di */
    COSTS_N_INSNS (38),           /* int_div_si */
    COSTS_N_INSNS (38),           /* int_div_di */
		     2,           /* branch_cost */
		     6            /* memory_latency */
  },
  { /* R4000 */
     COSTS_N_INSNS (6),           /* fp_add */
     COSTS_N_INSNS (7),           /* fp_mult_sf */
     COSTS_N_INSNS (8),           /* fp_mult_df */
     COSTS_N_INSNS (23),          /* fp_div_sf */
     COSTS_N_INSNS (36),          /* fp_div_df */
     COSTS_N_INSNS (10),          /* int_mult_si */
     COSTS_N_INSNS (10),          /* int_mult_di */
     COSTS_N_INSNS (69),          /* int_div_si */
     COSTS_N_INSNS (69),          /* int_div_di */
		      2,          /* branch_cost */
		      6           /* memory_latency */
  },
  { /* R4100 */
    DEFAULT_COSTS
  },
  { /* R4111 */
    DEFAULT_COSTS
  },
  { /* R4120 */
    DEFAULT_COSTS
  },
  { /* R4130 */
    /* The only costs that appear to be updated here are
       integer multiplication.  */
    SOFT_FP_COSTS,
    COSTS_N_INSNS (4),            /* int_mult_si */
    COSTS_N_INSNS (6),            /* int_mult_di */
    COSTS_N_INSNS (69),           /* int_div_si */
    COSTS_N_INSNS (69),           /* int_div_di */
		     1,           /* branch_cost */
		     4            /* memory_latency */
  },
  { /* R4300 */
    DEFAULT_COSTS
  },
  { /* R4600 */
    DEFAULT_COSTS
  },
  { /* R4650 */
    DEFAULT_COSTS
  },
  { /* R5000 */
    COSTS_N_INSNS (6),            /* fp_add */
    COSTS_N_INSNS (4),            /* fp_mult_sf */
    COSTS_N_INSNS (5),            /* fp_mult_df */
    COSTS_N_INSNS (23),           /* fp_div_sf */
    COSTS_N_INSNS (36),           /* fp_div_df */
    COSTS_N_INSNS (5),            /* int_mult_si */
    COSTS_N_INSNS (5),            /* int_mult_di */
    COSTS_N_INSNS (36),           /* int_div_si */
    COSTS_N_INSNS (36),           /* int_div_di */
		     1,           /* branch_cost */
		     4            /* memory_latency */
  },
  { /* R5400 */
    COSTS_N_INSNS (6),            /* fp_add */
    COSTS_N_INSNS (5),            /* fp_mult_sf */
    COSTS_N_INSNS (6),            /* fp_mult_df */
    COSTS_N_INSNS (30),           /* fp_div_sf */
    COSTS_N_INSNS (59),           /* fp_div_df */
    COSTS_N_INSNS (3),            /* int_mult_si */
    COSTS_N_INSNS (4),            /* int_mult_di */
    COSTS_N_INSNS (42),           /* int_div_si */
    COSTS_N_INSNS (74),           /* int_div_di */
		     1,           /* branch_cost */
		     4            /* memory_latency */
  },
  { /* R5500 */
    COSTS_N_INSNS (6),            /* fp_add */
    COSTS_N_INSNS (5),            /* fp_mult_sf */
    COSTS_N_INSNS (6),            /* fp_mult_df */
    COSTS_N_INSNS (30),           /* fp_div_sf */
    COSTS_N_INSNS (59),           /* fp_div_df */
    COSTS_N_INSNS (5),            /* int_mult_si */
    COSTS_N_INSNS (9),            /* int_mult_di */
    COSTS_N_INSNS (42),           /* int_div_si */
    COSTS_N_INSNS (74),           /* int_div_di */
		     1,           /* branch_cost */
		     4            /* memory_latency */
  },
  { /* R7000 */
    /* The only costs that are changed here are
       integer multiplication.  */
    COSTS_N_INSNS (6),            /* fp_add */
    COSTS_N_INSNS (7),            /* fp_mult_sf */
    COSTS_N_INSNS (8),            /* fp_mult_df */
    COSTS_N_INSNS (23),           /* fp_div_sf */
    COSTS_N_INSNS (36),           /* fp_div_df */
    COSTS_N_INSNS (5),            /* int_mult_si */
    COSTS_N_INSNS (9),            /* int_mult_di */
    COSTS_N_INSNS (69),           /* int_div_si */
    COSTS_N_INSNS (69),           /* int_div_di */
		     1,           /* branch_cost */
		     4            /* memory_latency */
  },
  { /* R8000 */
    DEFAULT_COSTS
  },
  { /* R9000 */
    /* The only costs that are changed here are
       integer multiplication.  */
    COSTS_N_INSNS (6),            /* fp_add */
    COSTS_N_INSNS (7),            /* fp_mult_sf */
    COSTS_N_INSNS (8),            /* fp_mult_df */
    COSTS_N_INSNS (23),           /* fp_div_sf */
    COSTS_N_INSNS (36),           /* fp_div_df */
    COSTS_N_INSNS (3),            /* int_mult_si */
    COSTS_N_INSNS (8),            /* int_mult_di */
    COSTS_N_INSNS (69),           /* int_div_si */
    COSTS_N_INSNS (69),           /* int_div_di */
		     1,           /* branch_cost */
		     4            /* memory_latency */
  },
  { /* R1x000 */
    COSTS_N_INSNS (2),            /* fp_add */
    COSTS_N_INSNS (2),            /* fp_mult_sf */
    COSTS_N_INSNS (2),            /* fp_mult_df */
    COSTS_N_INSNS (12),           /* fp_div_sf */
    COSTS_N_INSNS (19),           /* fp_div_df */
    COSTS_N_INSNS (5),            /* int_mult_si */
    COSTS_N_INSNS (9),            /* int_mult_di */
    COSTS_N_INSNS (34),           /* int_div_si */
    COSTS_N_INSNS (66),           /* int_div_di */
		     1,           /* branch_cost */
		     4            /* memory_latency */
  },
  { /* SB1 */
    /* These costs are the same as the SB-1A below.  */
    COSTS_N_INSNS (4),            /* fp_add */
    COSTS_N_INSNS (4),            /* fp_mult_sf */
    COSTS_N_INSNS (4),            /* fp_mult_df */
    COSTS_N_INSNS (24),           /* fp_div_sf */
    COSTS_N_INSNS (32),           /* fp_div_df */
    COSTS_N_INSNS (3),            /* int_mult_si */
    COSTS_N_INSNS (4),            /* int_mult_di */
    COSTS_N_INSNS (36),           /* int_div_si */
    COSTS_N_INSNS (68),           /* int_div_di */
		     1,           /* branch_cost */
		     4            /* memory_latency */
  },
  { /* SB1-A */
    /* These costs are the same as the SB-1 above.  */
    COSTS_N_INSNS (4),            /* fp_add */
    COSTS_N_INSNS (4),            /* fp_mult_sf */
    COSTS_N_INSNS (4),            /* fp_mult_df */
    COSTS_N_INSNS (24),           /* fp_div_sf */
    COSTS_N_INSNS (32),           /* fp_div_df */
    COSTS_N_INSNS (3),            /* int_mult_si */
    COSTS_N_INSNS (4),            /* int_mult_di */
    COSTS_N_INSNS (36),           /* int_div_si */
    COSTS_N_INSNS (68),           /* int_div_di */
		     1,           /* branch_cost */
		     4            /* memory_latency */
  },
  { /* SR71000 */
    DEFAULT_COSTS
  },
  { /* XLR */
    SOFT_FP_COSTS,
    COSTS_N_INSNS (8),            /* int_mult_si */
    COSTS_N_INSNS (8),            /* int_mult_di */
    COSTS_N_INSNS (72),           /* int_div_si */
    COSTS_N_INSNS (72),           /* int_div_di */
		     1,           /* branch_cost */
		     4            /* memory_latency */
  }
};

static rtx mips_find_pic_call_symbol (rtx, rtx, bool);
static int mips_register_move_cost (enum machine_mode, reg_class_t,
				    reg_class_t);
static unsigned int mips_function_arg_boundary (enum machine_mode, const_tree);

/* This hash table keeps track of implicit "mips16" and "nomips16" attributes
   for -mflip_mips16.  It maps decl names onto a boolean mode setting.  */
struct GTY (())  mflip_mips16_entry {
  const char *name;
  bool mips16_p;
};
static GTY ((param_is (struct mflip_mips16_entry))) htab_t mflip_mips16_htab;

/* Hash table callbacks for mflip_mips16_htab.  */

static hashval_t
mflip_mips16_htab_hash (const void *entry)
{
  return htab_hash_string (((const struct mflip_mips16_entry *) entry)->name);
}

static int
mflip_mips16_htab_eq (const void *entry, const void *name)
{
  return strcmp (((const struct mflip_mips16_entry *) entry)->name,
		 (const char *) name) == 0;
}

/* True if -mflip-mips16 should next add an attribute for the default MIPS16
   mode, false if it should next add an attribute for the opposite mode.  */
static GTY(()) bool mips16_flipper;

/* DECL is a function that needs a default "mips16" or "nomips16" attribute
   for -mflip-mips16.  Return true if it should use "mips16" and false if
   it should use "nomips16".  */

static bool
mflip_mips16_use_mips16_p (tree decl)
{
  struct mflip_mips16_entry *entry;
  const char *name;
  hashval_t hash;
  void **slot;

  /* Use the opposite of the command-line setting for anonymous decls.  */
  if (!DECL_NAME (decl))
    return !mips_base_mips16;

  if (!mflip_mips16_htab)
    mflip_mips16_htab = htab_create_ggc (37, mflip_mips16_htab_hash,
					 mflip_mips16_htab_eq, NULL);

  name = IDENTIFIER_POINTER (DECL_NAME (decl));
  hash = htab_hash_string (name);
  slot = htab_find_slot_with_hash (mflip_mips16_htab, name, hash, INSERT);
  entry = (struct mflip_mips16_entry *) *slot;
  if (!entry)
    {
      mips16_flipper = !mips16_flipper;
      entry = ggc_alloc_mflip_mips16_entry ();
      entry->name = name;
      entry->mips16_p = mips16_flipper ? !mips_base_mips16 : mips_base_mips16;
      *slot = entry;
    }
  return entry->mips16_p;
}

/* Predicates to test for presence of "near" and "far"/"long_call"
   attributes on the given TYPE.  */

static bool
mips_near_type_p (const_tree type)
{
  return lookup_attribute ("near", TYPE_ATTRIBUTES (type)) != NULL;
}

static bool
mips_far_type_p (const_tree type)
{
  return (lookup_attribute ("long_call", TYPE_ATTRIBUTES (type)) != NULL
	  || lookup_attribute ("far", TYPE_ATTRIBUTES (type)) != NULL);
}

/* Similar predicates for "mips16"/"nomips16" function attributes.  */

static bool
mips_mips16_decl_p (const_tree decl)
{
  return lookup_attribute ("mips16", DECL_ATTRIBUTES (decl)) != NULL;
}

static bool
mips_nomips16_decl_p (const_tree decl)
{
  return lookup_attribute ("nomips16", DECL_ATTRIBUTES (decl)) != NULL;
}

/* Check if the interrupt attribute is set for a function.  */

static bool
mips_interrupt_type_p (tree type)
{
  return lookup_attribute ("interrupt", TYPE_ATTRIBUTES (type)) != NULL;
}

/* Check if the attribute to use shadow register set is set for a function.  */

static bool
mips_use_shadow_register_set_p (tree type)
{
  return lookup_attribute ("use_shadow_register_set",
			   TYPE_ATTRIBUTES (type)) != NULL;
}

/* Check if the attribute to keep interrupts masked is set for a function.  */

static bool
mips_keep_interrupts_masked_p (tree type)
{
  return lookup_attribute ("keep_interrupts_masked",
			   TYPE_ATTRIBUTES (type)) != NULL;
}

/* Check if the attribute to use debug exception return is set for
   a function.  */

static bool
mips_use_debug_exception_return_p (tree type)
{
  return lookup_attribute ("use_debug_exception_return",
			   TYPE_ATTRIBUTES (type)) != NULL;
}

/* Return true if function DECL is a MIPS16 function.  Return the ambient
   setting if DECL is null.  */

static bool
mips_use_mips16_mode_p (tree decl)
{
  if (decl)
    {
      /* Nested functions must use the same frame pointer as their
	 parent and must therefore use the same ISA mode.  */
      tree parent = decl_function_context (decl);
      if (parent)
	decl = parent;
      if (mips_mips16_decl_p (decl))
	return true;
      if (mips_nomips16_decl_p (decl))
	return false;
    }
  return mips_base_mips16;
}

/* Implement TARGET_COMP_TYPE_ATTRIBUTES.  */

static int
mips_comp_type_attributes (const_tree type1, const_tree type2)
{
  /* Disallow mixed near/far attributes.  */
  if (mips_far_type_p (type1) && mips_near_type_p (type2))
    return 0;
  if (mips_near_type_p (type1) && mips_far_type_p (type2))
    return 0;
  return 1;
}

/* Implement TARGET_INSERT_ATTRIBUTES.  */

static void
mips_insert_attributes (tree decl, tree *attributes)
{
  const char *name;
  bool mips16_p, nomips16_p;

  /* Check for "mips16" and "nomips16" attributes.  */
  mips16_p = lookup_attribute ("mips16", *attributes) != NULL;
  nomips16_p = lookup_attribute ("nomips16", *attributes) != NULL;
  if (TREE_CODE (decl) != FUNCTION_DECL)
    {
      if (mips16_p)
	error ("%qs attribute only applies to functions", "mips16");
      if (nomips16_p)
	error ("%qs attribute only applies to functions", "nomips16");
    }
  else
    {
      mips16_p |= mips_mips16_decl_p (decl);
      nomips16_p |= mips_nomips16_decl_p (decl);
      if (mips16_p || nomips16_p)
	{
	  /* DECL cannot be simultaneously "mips16" and "nomips16".  */
	  if (mips16_p && nomips16_p)
	    error ("%qE cannot have both %<mips16%> and "
		   "%<nomips16%> attributes",
		   DECL_NAME (decl));
	}
      else if (TARGET_FLIP_MIPS16 && !DECL_ARTIFICIAL (decl))
	{
	  /* Implement -mflip-mips16.  If DECL has neither a "nomips16" nor a
	     "mips16" attribute, arbitrarily pick one.  We must pick the same
	     setting for duplicate declarations of a function.  */
	  name = mflip_mips16_use_mips16_p (decl) ? "mips16" : "nomips16";
	  *attributes = tree_cons (get_identifier (name), NULL, *attributes);
	}
    }
}

/* Implement TARGET_MERGE_DECL_ATTRIBUTES.  */

static tree
mips_merge_decl_attributes (tree olddecl, tree newdecl)
{
  /* The decls' "mips16" and "nomips16" attributes must match exactly.  */
  if (mips_mips16_decl_p (olddecl) != mips_mips16_decl_p (newdecl))
    error ("%qE redeclared with conflicting %qs attributes",
	   DECL_NAME (newdecl), "mips16");
  if (mips_nomips16_decl_p (olddecl) != mips_nomips16_decl_p (newdecl))
    error ("%qE redeclared with conflicting %qs attributes",
	   DECL_NAME (newdecl), "nomips16");

  return merge_attributes (DECL_ATTRIBUTES (olddecl),
			   DECL_ATTRIBUTES (newdecl));
}

/* If X is a PLUS of a CONST_INT, return the two terms in *BASE_PTR
   and *OFFSET_PTR.  Return X in *BASE_PTR and 0 in *OFFSET_PTR otherwise.  */

static void
mips_split_plus (rtx x, rtx *base_ptr, HOST_WIDE_INT *offset_ptr)
{
  if (GET_CODE (x) == PLUS && CONST_INT_P (XEXP (x, 1)))
    {
      *base_ptr = XEXP (x, 0);
      *offset_ptr = INTVAL (XEXP (x, 1));
    }
  else
    {
      *base_ptr = x;
      *offset_ptr = 0;
    }
}

static unsigned int mips_build_integer (struct mips_integer_op *,
					unsigned HOST_WIDE_INT);

/* A subroutine of mips_build_integer, with the same interface.
   Assume that the final action in the sequence should be a left shift.  */

static unsigned int
mips_build_shift (struct mips_integer_op *codes, HOST_WIDE_INT value)
{
  unsigned int i, shift;

  /* Shift VALUE right until its lowest bit is set.  Shift arithmetically
     since signed numbers are easier to load than unsigned ones.  */
  shift = 0;
  while ((value & 1) == 0)
    value /= 2, shift++;

  i = mips_build_integer (codes, value);
  codes[i].code = ASHIFT;
  codes[i].value = shift;
  return i + 1;
}

/* As for mips_build_shift, but assume that the final action will be
   an IOR or PLUS operation.  */

static unsigned int
mips_build_lower (struct mips_integer_op *codes, unsigned HOST_WIDE_INT value)
{
  unsigned HOST_WIDE_INT high;
  unsigned int i;

  high = value & ~(unsigned HOST_WIDE_INT) 0xffff;
  if (!LUI_OPERAND (high) && (value & 0x18000) == 0x18000)
    {
      /* The constant is too complex to load with a simple LUI/ORI pair,
	 so we want to give the recursive call as many trailing zeros as
	 possible.  In this case, we know bit 16 is set and that the
	 low 16 bits form a negative number.  If we subtract that number
	 from VALUE, we will clear at least the lowest 17 bits, maybe more.  */
      i = mips_build_integer (codes, CONST_HIGH_PART (value));
      codes[i].code = PLUS;
      codes[i].value = CONST_LOW_PART (value);
    }
  else
    {
      /* Either this is a simple LUI/ORI pair, or clearing the lowest 16
	 bits gives a value with at least 17 trailing zeros.  */
      i = mips_build_integer (codes, high);
      codes[i].code = IOR;
      codes[i].value = value & 0xffff;
    }
  return i + 1;
}

/* Fill CODES with a sequence of rtl operations to load VALUE.
   Return the number of operations needed.  */

static unsigned int
mips_build_integer (struct mips_integer_op *codes,
		    unsigned HOST_WIDE_INT value)
{
  if (SMALL_OPERAND (value)
      || SMALL_OPERAND_UNSIGNED (value)
      || LUI_OPERAND (value))
    {
      /* The value can be loaded with a single instruction.  */
      codes[0].code = UNKNOWN;
      codes[0].value = value;
      return 1;
    }
  else if ((value & 1) != 0 || LUI_OPERAND (CONST_HIGH_PART (value)))
    {
      /* Either the constant is a simple LUI/ORI combination or its
	 lowest bit is set.  We don't want to shift in this case.  */
      return mips_build_lower (codes, value);
    }
  else if ((value & 0xffff) == 0)
    {
      /* The constant will need at least three actions.  The lowest
	 16 bits are clear, so the final action will be a shift.  */
      return mips_build_shift (codes, value);
    }
  else
    {
      /* The final action could be a shift, add or inclusive OR.
	 Rather than use a complex condition to select the best
	 approach, try both mips_build_shift and mips_build_lower
	 and pick the one that gives the shortest sequence.
	 Note that this case is only used once per constant.  */
      struct mips_integer_op alt_codes[MIPS_MAX_INTEGER_OPS];
      unsigned int cost, alt_cost;

      cost = mips_build_shift (codes, value);
      alt_cost = mips_build_lower (alt_codes, value);
      if (alt_cost < cost)
	{
	  memcpy (codes, alt_codes, alt_cost * sizeof (codes[0]));
	  cost = alt_cost;
	}
      return cost;
    }
}

/* Return true if symbols of type TYPE require a GOT access.  */

static bool
mips_got_symbol_type_p (enum mips_symbol_type type)
{
  switch (type)
    {
    case SYMBOL_GOT_PAGE_OFST:
    case SYMBOL_GOT_DISP:
      return true;

    default:
      return false;
    }
}

/* Return true if X is a thread-local symbol.  */

static bool
mips_tls_symbol_p (rtx x)
{
  return GET_CODE (x) == SYMBOL_REF && SYMBOL_REF_TLS_MODEL (x) != 0;
}

/* Return true if SYMBOL_REF X is associated with a global symbol
   (in the STB_GLOBAL sense).  */

static bool
mips_global_symbol_p (const_rtx x)
{
  const_tree decl = SYMBOL_REF_DECL (x);

  if (!decl)
    return !SYMBOL_REF_LOCAL_P (x) || SYMBOL_REF_EXTERNAL_P (x);

  /* Weakref symbols are not TREE_PUBLIC, but their targets are global
     or weak symbols.  Relocations in the object file will be against
     the target symbol, so it's that symbol's binding that matters here.  */
  return DECL_P (decl) && (TREE_PUBLIC (decl) || DECL_WEAK (decl));
}

/* Return true if function X is a libgcc MIPS16 stub function.  */

static bool
mips16_stub_function_p (const_rtx x)
{
  return (GET_CODE (x) == SYMBOL_REF
	  && strncmp (XSTR (x, 0), "__mips16_", 9) == 0);
}

/* Return true if function X is a locally-defined and locally-binding
   MIPS16 function.  */

static bool
mips16_local_function_p (const_rtx x)
{
  return (GET_CODE (x) == SYMBOL_REF
	  && SYMBOL_REF_LOCAL_P (x)
	  && !SYMBOL_REF_EXTERNAL_P (x)
	  && mips_use_mips16_mode_p (SYMBOL_REF_DECL (x)));
}

/* Return true if SYMBOL_REF X binds locally.  */

static bool
mips_symbol_binds_local_p (const_rtx x)
{
  return (SYMBOL_REF_DECL (x)
	  ? targetm.binds_local_p (SYMBOL_REF_DECL (x))
	  : SYMBOL_REF_LOCAL_P (x));
}

/* Return true if rtx constants of mode MODE should be put into a small
   data section.  */

static bool
mips_rtx_constant_in_small_data_p (enum machine_mode mode)
{
  return (!TARGET_EMBEDDED_DATA
	  && TARGET_LOCAL_SDATA
	  && GET_MODE_SIZE (mode) <= mips_small_data_threshold);
}

/* Return true if X should not be moved directly into register $25.
   We need this because many versions of GAS will treat "la $25,foo" as
   part of a call sequence and so allow a global "foo" to be lazily bound.  */

bool
mips_dangerous_for_la25_p (rtx x)
{
  return (!TARGET_EXPLICIT_RELOCS
	  && TARGET_USE_GOT
	  && GET_CODE (x) == SYMBOL_REF
	  && mips_global_symbol_p (x));
}

/* Return true if calls to X might need $25 to be valid on entry.  */

bool
mips_use_pic_fn_addr_reg_p (const_rtx x)
{
  if (!TARGET_USE_PIC_FN_ADDR_REG)
    return false;

  /* MIPS16 stub functions are guaranteed not to use $25.  */
  if (mips16_stub_function_p (x))
    return false;

  if (GET_CODE (x) == SYMBOL_REF)
    {
      /* If PLTs and copy relocations are available, the static linker
	 will make sure that $25 is valid on entry to the target function.  */
      if (TARGET_ABICALLS_PIC0)
	return false;

      /* Locally-defined functions use absolute accesses to set up
	 the global pointer.  */
      if (TARGET_ABSOLUTE_ABICALLS
	  && mips_symbol_binds_local_p (x)
	  && !SYMBOL_REF_EXTERNAL_P (x))
	return false;
    }

  return true;
}

/* Return the method that should be used to access SYMBOL_REF or
   LABEL_REF X in context CONTEXT.  */

static enum mips_symbol_type
mips_classify_symbol (const_rtx x, enum mips_symbol_context context)
{
  if (TARGET_RTP_PIC)
    return SYMBOL_GOT_DISP;

  if (GET_CODE (x) == LABEL_REF)
    {
      /* Only return SYMBOL_PC_RELATIVE if we are generating MIPS16
	 code and if we know that the label is in the current function's
	 text section.  LABEL_REFs are used for jump tables as well as
	 text labels, so we must check whether jump tables live in the
	 text section.  */
      if (TARGET_MIPS16_SHORT_JUMP_TABLES
	  && !LABEL_REF_NONLOCAL_P (x))
	return SYMBOL_PC_RELATIVE;

      if (TARGET_ABICALLS && !TARGET_ABSOLUTE_ABICALLS)
	return SYMBOL_GOT_PAGE_OFST;

      return SYMBOL_ABSOLUTE;
    }

  gcc_assert (GET_CODE (x) == SYMBOL_REF);

  if (SYMBOL_REF_TLS_MODEL (x))
    return SYMBOL_TLS;

  if (CONSTANT_POOL_ADDRESS_P (x))
    {
      if (TARGET_MIPS16_TEXT_LOADS)
	return SYMBOL_PC_RELATIVE;

      if (TARGET_MIPS16_PCREL_LOADS && context == SYMBOL_CONTEXT_MEM)
	return SYMBOL_PC_RELATIVE;

      if (mips_rtx_constant_in_small_data_p (get_pool_mode (x)))
	return SYMBOL_GP_RELATIVE;
    }

  /* Do not use small-data accesses for weak symbols; they may end up
     being zero.  */
  if (TARGET_GPOPT && SYMBOL_REF_SMALL_P (x) && !SYMBOL_REF_WEAK (x))
    return SYMBOL_GP_RELATIVE;

  /* Don't use GOT accesses for locally-binding symbols when -mno-shared
     is in effect.  */
  if (TARGET_ABICALLS_PIC2
      && !(TARGET_ABSOLUTE_ABICALLS && mips_symbol_binds_local_p (x)))
    {
      /* There are three cases to consider:

	    - o32 PIC (either with or without explicit relocs)
	    - n32/n64 PIC without explicit relocs
	    - n32/n64 PIC with explicit relocs

	 In the first case, both local and global accesses will use an
	 R_MIPS_GOT16 relocation.  We must correctly predict which of
	 the two semantics (local or global) the assembler and linker
	 will apply.  The choice depends on the symbol's binding rather
	 than its visibility.

	 In the second case, the assembler will not use R_MIPS_GOT16
	 relocations, but it chooses between local and global accesses
	 in the same way as for o32 PIC.

	 In the third case we have more freedom since both forms of
	 access will work for any kind of symbol.  However, there seems
	 little point in doing things differently.  */
      if (mips_global_symbol_p (x))
	return SYMBOL_GOT_DISP;

      return SYMBOL_GOT_PAGE_OFST;
    }

  if (TARGET_MIPS16_PCREL_LOADS && context != SYMBOL_CONTEXT_CALL)
    return SYMBOL_FORCE_TO_MEM;

  return SYMBOL_ABSOLUTE;
}

/* Classify the base of symbolic expression X, given that X appears in
   context CONTEXT.  */

static enum mips_symbol_type
mips_classify_symbolic_expression (rtx x, enum mips_symbol_context context)
{
  rtx offset;

  split_const (x, &x, &offset);
  if (UNSPEC_ADDRESS_P (x))
    return UNSPEC_ADDRESS_TYPE (x);

  return mips_classify_symbol (x, context);
}

/* Return true if OFFSET is within the range [0, ALIGN), where ALIGN
   is the alignment in bytes of SYMBOL_REF X.  */

static bool
mips_offset_within_alignment_p (rtx x, HOST_WIDE_INT offset)
{
  HOST_WIDE_INT align;

  align = SYMBOL_REF_DECL (x) ? DECL_ALIGN_UNIT (SYMBOL_REF_DECL (x)) : 1;
  return IN_RANGE (offset, 0, align - 1);
}

/* Return true if X is a symbolic constant that can be used in context
   CONTEXT.  If it is, store the type of the symbol in *SYMBOL_TYPE.  */

bool
mips_symbolic_constant_p (rtx x, enum mips_symbol_context context,
			  enum mips_symbol_type *symbol_type)
{
  rtx offset;

  split_const (x, &x, &offset);
  if (UNSPEC_ADDRESS_P (x))
    {
      *symbol_type = UNSPEC_ADDRESS_TYPE (x);
      x = UNSPEC_ADDRESS (x);
    }
  else if (GET_CODE (x) == SYMBOL_REF || GET_CODE (x) == LABEL_REF)
    {
      *symbol_type = mips_classify_symbol (x, context);
      if (*symbol_type == SYMBOL_TLS)
	return false;
    }
  else
    return false;

  if (offset == const0_rtx)
    return true;

  /* Check whether a nonzero offset is valid for the underlying
     relocations.  */
  switch (*symbol_type)
    {
    case SYMBOL_ABSOLUTE:
    case SYMBOL_FORCE_TO_MEM:
    case SYMBOL_32_HIGH:
    case SYMBOL_64_HIGH:
    case SYMBOL_64_MID:
    case SYMBOL_64_LOW:
      /* If the target has 64-bit pointers and the object file only
	 supports 32-bit symbols, the values of those symbols will be
	 sign-extended.  In this case we can't allow an arbitrary offset
	 in case the 32-bit value X + OFFSET has a different sign from X.  */
      if (Pmode == DImode && !ABI_HAS_64BIT_SYMBOLS)
	return offset_within_block_p (x, INTVAL (offset));

      /* In other cases the relocations can handle any offset.  */
      return true;

    case SYMBOL_PC_RELATIVE:
      /* Allow constant pool references to be converted to LABEL+CONSTANT.
	 In this case, we no longer have access to the underlying constant,
	 but the original symbol-based access was known to be valid.  */
      if (GET_CODE (x) == LABEL_REF)
	return true;

      /* Fall through.  */

    case SYMBOL_GP_RELATIVE:
      /* Make sure that the offset refers to something within the
	 same object block.  This should guarantee that the final
	 PC- or GP-relative offset is within the 16-bit limit.  */
      return offset_within_block_p (x, INTVAL (offset));

    case SYMBOL_GOT_PAGE_OFST:
    case SYMBOL_GOTOFF_PAGE:
      /* If the symbol is global, the GOT entry will contain the symbol's
	 address, and we will apply a 16-bit offset after loading it.
	 If the symbol is local, the linker should provide enough local
	 GOT entries for a 16-bit offset, but larger offsets may lead
	 to GOT overflow.  */
      return SMALL_INT (offset);

    case SYMBOL_TPREL:
    case SYMBOL_DTPREL:
      /* There is no carry between the HI and LO REL relocations, so the
	 offset is only valid if we know it won't lead to such a carry.  */
      return mips_offset_within_alignment_p (x, INTVAL (offset));

    case SYMBOL_GOT_DISP:
    case SYMBOL_GOTOFF_DISP:
    case SYMBOL_GOTOFF_CALL:
    case SYMBOL_GOTOFF_LOADGP:
    case SYMBOL_TLSGD:
    case SYMBOL_TLSLDM:
    case SYMBOL_GOTTPREL:
    case SYMBOL_TLS:
    case SYMBOL_HALF:
      return false;
    }
  gcc_unreachable ();
}

/* Like mips_symbol_insns, but treat extended MIPS16 instructions as a
   single instruction.  We rely on the fact that, in the worst case,
   all instructions involved in a MIPS16 address calculation are usually
   extended ones.  */

static int
mips_symbol_insns_1 (enum mips_symbol_type type, enum machine_mode mode)
{
  switch (type)
    {
    case SYMBOL_ABSOLUTE:
      /* When using 64-bit symbols, we need 5 preparatory instructions,
	 such as:

	     lui     $at,%highest(symbol)
	     daddiu  $at,$at,%higher(symbol)
	     dsll    $at,$at,16
	     daddiu  $at,$at,%hi(symbol)
	     dsll    $at,$at,16

	 The final address is then $at + %lo(symbol).  With 32-bit
	 symbols we just need a preparatory LUI for normal mode and
	 a preparatory LI and SLL for MIPS16.  */
      return ABI_HAS_64BIT_SYMBOLS ? 6 : TARGET_MIPS16 ? 3 : 2;

    case SYMBOL_GP_RELATIVE:
      /* Treat GP-relative accesses as taking a single instruction on
	 MIPS16 too; the copy of $gp can often be shared.  */
      return 1;

    case SYMBOL_PC_RELATIVE:
      /* PC-relative constants can be only be used with ADDIUPC,
	 DADDIUPC, LWPC and LDPC.  */
      if (mode == MAX_MACHINE_MODE
	  || GET_MODE_SIZE (mode) == 4
	  || GET_MODE_SIZE (mode) == 8)
	return 1;

      /* The constant must be loaded using ADDIUPC or DADDIUPC first.  */
      return 0;

    case SYMBOL_FORCE_TO_MEM:
      /* LEAs will be converted into constant-pool references by
	 mips_reorg.  */
      if (mode == MAX_MACHINE_MODE)
	return 1;

      /* The constant must be loaded and then dereferenced.  */
      return 0;

    case SYMBOL_GOT_DISP:
      /* The constant will have to be loaded from the GOT before it
	 is used in an address.  */
      if (mode != MAX_MACHINE_MODE)
	return 0;

      /* Fall through.  */

    case SYMBOL_GOT_PAGE_OFST:
      /* Unless -funit-at-a-time is in effect, we can't be sure whether the
	 local/global classification is accurate.  The worst cases are:

	 (1) For local symbols when generating o32 or o64 code.  The assembler
	     will use:

		 lw	      $at,%got(symbol)
		 nop

	     ...and the final address will be $at + %lo(symbol).

	 (2) For global symbols when -mxgot.  The assembler will use:

	         lui     $at,%got_hi(symbol)
	         (d)addu $at,$at,$gp

	     ...and the final address will be $at + %got_lo(symbol).  */
      return 3;

    case SYMBOL_GOTOFF_PAGE:
    case SYMBOL_GOTOFF_DISP:
    case SYMBOL_GOTOFF_CALL:
    case SYMBOL_GOTOFF_LOADGP:
    case SYMBOL_32_HIGH:
    case SYMBOL_64_HIGH:
    case SYMBOL_64_MID:
    case SYMBOL_64_LOW:
    case SYMBOL_TLSGD:
    case SYMBOL_TLSLDM:
    case SYMBOL_DTPREL:
    case SYMBOL_GOTTPREL:
    case SYMBOL_TPREL:
    case SYMBOL_HALF:
      /* A 16-bit constant formed by a single relocation, or a 32-bit
	 constant formed from a high 16-bit relocation and a low 16-bit
	 relocation.  Use mips_split_p to determine which.  32-bit
	 constants need an "lui; addiu" sequence for normal mode and
	 an "li; sll; addiu" sequence for MIPS16 mode.  */
      return !mips_split_p[type] ? 1 : TARGET_MIPS16 ? 3 : 2;

    case SYMBOL_TLS:
      /* We don't treat a bare TLS symbol as a constant.  */
      return 0;
    }
  gcc_unreachable ();
}

/* If MODE is MAX_MACHINE_MODE, return the number of instructions needed
   to load symbols of type TYPE into a register.  Return 0 if the given
   type of symbol cannot be used as an immediate operand.

   Otherwise, return the number of instructions needed to load or store
   values of mode MODE to or from addresses of type TYPE.  Return 0 if
   the given type of symbol is not valid in addresses.

   In both cases, treat extended MIPS16 instructions as two instructions.  */

static int
mips_symbol_insns (enum mips_symbol_type type, enum machine_mode mode)
{
  return mips_symbol_insns_1 (type, mode) * (TARGET_MIPS16 ? 2 : 1);
}

/* A for_each_rtx callback.  Stop the search if *X references a
   thread-local symbol.  */

static int
mips_tls_symbol_ref_1 (rtx *x, void *data ATTRIBUTE_UNUSED)
{
  return mips_tls_symbol_p (*x);
}

/* Implement TARGET_CANNOT_FORCE_CONST_MEM.  */

static bool
mips_cannot_force_const_mem (rtx x)
{
  enum mips_symbol_type type;
  rtx base, offset;

  /* There is no assembler syntax for expressing an address-sized
     high part.  */
  if (GET_CODE (x) == HIGH)
    return true;

  /* As an optimization, reject constants that mips_legitimize_move
     can expand inline.

     Suppose we have a multi-instruction sequence that loads constant C
     into register R.  If R does not get allocated a hard register, and
     R is used in an operand that allows both registers and memory
     references, reload will consider forcing C into memory and using
     one of the instruction's memory alternatives.  Returning false
     here will force it to use an input reload instead.  */
  if (CONST_INT_P (x) && LEGITIMATE_CONSTANT_P (x))
    return true;

  split_const (x, &base, &offset);
  if (mips_symbolic_constant_p (base, SYMBOL_CONTEXT_LEA, &type)
      && type != SYMBOL_FORCE_TO_MEM)
    {
      /* The same optimization as for CONST_INT.  */
      if (SMALL_INT (offset) && mips_symbol_insns (type, MAX_MACHINE_MODE) > 0)
	return true;

      /* If MIPS16 constant pools live in the text section, they should
	 not refer to anything that might need run-time relocation.  */
      if (TARGET_MIPS16_PCREL_LOADS && mips_got_symbol_type_p (type))
	return true;
    }

  /* TLS symbols must be computed by mips_legitimize_move.  */
  if (for_each_rtx (&x, &mips_tls_symbol_ref_1, NULL))
    return true;

  return false;
}

/* Implement TARGET_USE_BLOCKS_FOR_CONSTANT_P.  We can't use blocks for
   constants when we're using a per-function constant pool.  */

static bool
mips_use_blocks_for_constant_p (enum machine_mode mode ATTRIBUTE_UNUSED,
				const_rtx x ATTRIBUTE_UNUSED)
{
  return !TARGET_MIPS16_PCREL_LOADS;
}

/* Return true if register REGNO is a valid base register for mode MODE.
   STRICT_P is true if REG_OK_STRICT is in effect.  */

int
mips_regno_mode_ok_for_base_p (int regno, enum machine_mode mode,
			       bool strict_p)
{
  if (!HARD_REGISTER_NUM_P (regno))
    {
      if (!strict_p)
	return true;
      regno = reg_renumber[regno];
    }

  /* These fake registers will be eliminated to either the stack or
     hard frame pointer, both of which are usually valid base registers.
     Reload deals with the cases where the eliminated form isn't valid.  */
  if (regno == ARG_POINTER_REGNUM || regno == FRAME_POINTER_REGNUM)
    return true;

  /* In MIPS16 mode, the stack pointer can only address word and doubleword
     values, nothing smaller.  There are two problems here:

       (a) Instantiating virtual registers can introduce new uses of the
	   stack pointer.  If these virtual registers are valid addresses,
	   the stack pointer should be too.

       (b) Most uses of the stack pointer are not made explicit until
	   FRAME_POINTER_REGNUM and ARG_POINTER_REGNUM have been eliminated.
	   We don't know until that stage whether we'll be eliminating to the
	   stack pointer (which needs the restriction) or the hard frame
	   pointer (which doesn't).

     All in all, it seems more consistent to only enforce this restriction
     during and after reload.  */
  if (TARGET_MIPS16 && regno == STACK_POINTER_REGNUM)
    return !strict_p || GET_MODE_SIZE (mode) == 4 || GET_MODE_SIZE (mode) == 8;

  return TARGET_MIPS16 ? M16_REG_P (regno) : GP_REG_P (regno);
}

/* Return true if X is a valid base register for mode MODE.
   STRICT_P is true if REG_OK_STRICT is in effect.  */

static bool
mips_valid_base_register_p (rtx x, enum machine_mode mode, bool strict_p)
{
  if (!strict_p && GET_CODE (x) == SUBREG)
    x = SUBREG_REG (x);

  return (REG_P (x)
	  && mips_regno_mode_ok_for_base_p (REGNO (x), mode, strict_p));
}

/* Return true if, for every base register BASE_REG, (plus BASE_REG X)
   can address a value of mode MODE.  */

static bool
mips_valid_offset_p (rtx x, enum machine_mode mode)
{
  /* Check that X is a signed 16-bit number.  */
  if (!const_arith_operand (x, Pmode))
    return false;

  /* We may need to split multiword moves, so make sure that every word
     is accessible.  */
  if (GET_MODE_SIZE (mode) > UNITS_PER_WORD
      && !SMALL_OPERAND (INTVAL (x) + GET_MODE_SIZE (mode) - UNITS_PER_WORD))
    return false;

  return true;
}

/* Return true if a LO_SUM can address a value of mode MODE when the
   LO_SUM symbol has type SYMBOL_TYPE.  */

static bool
mips_valid_lo_sum_p (enum mips_symbol_type symbol_type, enum machine_mode mode)
{
  /* Check that symbols of type SYMBOL_TYPE can be used to access values
     of mode MODE.  */
  if (mips_symbol_insns (symbol_type, mode) == 0)
    return false;

  /* Check that there is a known low-part relocation.  */
  if (mips_lo_relocs[symbol_type] == NULL)
    return false;

  /* We may need to split multiword moves, so make sure that each word
     can be accessed without inducing a carry.  This is mainly needed
     for o64, which has historically only guaranteed 64-bit alignment
     for 128-bit types.  */
  if (GET_MODE_SIZE (mode) > UNITS_PER_WORD
      && GET_MODE_BITSIZE (mode) > GET_MODE_ALIGNMENT (mode))
    return false;

  return true;
}

/* Return true if X is a valid address for machine mode MODE.  If it is,
   fill in INFO appropriately.  STRICT_P is true if REG_OK_STRICT is in
   effect.  */

static bool
mips_classify_address (struct mips_address_info *info, rtx x,
		       enum machine_mode mode, bool strict_p)
{
  switch (GET_CODE (x))
    {
    case REG:
    case SUBREG:
      info->type = ADDRESS_REG;
      info->reg = x;
      info->offset = const0_rtx;
      return mips_valid_base_register_p (info->reg, mode, strict_p);

    case PLUS:
      info->type = ADDRESS_REG;
      info->reg = XEXP (x, 0);
      info->offset = XEXP (x, 1);
      return (mips_valid_base_register_p (info->reg, mode, strict_p)
	      && mips_valid_offset_p (info->offset, mode));

    case LO_SUM:
      info->type = ADDRESS_LO_SUM;
      info->reg = XEXP (x, 0);
      info->offset = XEXP (x, 1);
      /* We have to trust the creator of the LO_SUM to do something vaguely
	 sane.  Target-independent code that creates a LO_SUM should also
	 create and verify the matching HIGH.  Target-independent code that
	 adds an offset to a LO_SUM must prove that the offset will not
	 induce a carry.  Failure to do either of these things would be
	 a bug, and we are not required to check for it here.  The MIPS
	 backend itself should only create LO_SUMs for valid symbolic
	 constants, with the high part being either a HIGH or a copy
	 of _gp. */
      info->symbol_type
	= mips_classify_symbolic_expression (info->offset, SYMBOL_CONTEXT_MEM);
      return (mips_valid_base_register_p (info->reg, mode, strict_p)
	      && mips_valid_lo_sum_p (info->symbol_type, mode));

    case CONST_INT:
      /* Small-integer addresses don't occur very often, but they
	 are legitimate if $0 is a valid base register.  */
      info->type = ADDRESS_CONST_INT;
      return !TARGET_MIPS16 && SMALL_INT (x);

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      info->type = ADDRESS_SYMBOLIC;
      return (mips_symbolic_constant_p (x, SYMBOL_CONTEXT_MEM,
					&info->symbol_type)
	      && mips_symbol_insns (info->symbol_type, mode) > 0
	      && !mips_split_p[info->symbol_type]);

    default:
      return false;
    }
}

/* Implement TARGET_LEGITIMATE_ADDRESS_P.  */

static bool
mips_legitimate_address_p (enum machine_mode mode, rtx x, bool strict_p)
{
  struct mips_address_info addr;

  return mips_classify_address (&addr, x, mode, strict_p);
}

/* Return true if X is a legitimate $sp-based address for mode MDOE.  */

bool
mips_stack_address_p (rtx x, enum machine_mode mode)
{
  struct mips_address_info addr;

  return (mips_classify_address (&addr, x, mode, false)
	  && addr.type == ADDRESS_REG
	  && addr.reg == stack_pointer_rtx);
}

/* Return true if ADDR matches the pattern for the LWXS load scaled indexed
   address instruction.  Note that such addresses are not considered
   legitimate in the TARGET_LEGITIMATE_ADDRESS_P sense, because their use
   is so restricted.  */

static bool
mips_lwxs_address_p (rtx addr)
{
  if (ISA_HAS_LWXS
      && GET_CODE (addr) == PLUS
      && REG_P (XEXP (addr, 1)))
    {
      rtx offset = XEXP (addr, 0);
      if (GET_CODE (offset) == MULT
	  && REG_P (XEXP (offset, 0))
	  && CONST_INT_P (XEXP (offset, 1))
	  && INTVAL (XEXP (offset, 1)) == 4)
	return true;
    }
  return false;
}

/* Return true if a value at OFFSET bytes from base register BASE can be
   accessed using an unextended MIPS16 instruction.  MODE is the mode of
   the value.

   Usually the offset in an unextended instruction is a 5-bit field.
   The offset is unsigned and shifted left once for LH and SH, twice
   for LW and SW, and so on.  An exception is LWSP and SWSP, which have
   an 8-bit immediate field that's shifted left twice.  */

static bool
mips16_unextended_reference_p (enum machine_mode mode, rtx base,
			       unsigned HOST_WIDE_INT offset)
{
  if (offset % GET_MODE_SIZE (mode) == 0)
    {
      if (GET_MODE_SIZE (mode) == 4 && base == stack_pointer_rtx)
	return offset < 256U * GET_MODE_SIZE (mode);
      return offset < 32U * GET_MODE_SIZE (mode);
    }
  return false;
}

/* Return the number of instructions needed to load or store a value
   of mode MODE at address X.  Return 0 if X isn't valid for MODE.
   Assume that multiword moves may need to be split into word moves
   if MIGHT_SPLIT_P, otherwise assume that a single load or store is
   enough.

   For MIPS16 code, count extended instructions as two instructions.  */

int
mips_address_insns (rtx x, enum machine_mode mode, bool might_split_p)
{
  struct mips_address_info addr;
  int factor;

  /* BLKmode is used for single unaligned loads and stores and should
     not count as a multiword mode.  (GET_MODE_SIZE (BLKmode) is pretty
     meaningless, so we have to single it out as a special case one way
     or the other.)  */
  if (mode != BLKmode && might_split_p)
    factor = (GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
  else
    factor = 1;

  if (mips_classify_address (&addr, x, mode, false))
    switch (addr.type)
      {
      case ADDRESS_REG:
	if (TARGET_MIPS16
	    && !mips16_unextended_reference_p (mode, addr.reg,
					       UINTVAL (addr.offset)))
	  return factor * 2;
	return factor;

      case ADDRESS_LO_SUM:
	return TARGET_MIPS16 ? factor * 2 : factor;

      case ADDRESS_CONST_INT:
	return factor;

      case ADDRESS_SYMBOLIC:
	return factor * mips_symbol_insns (addr.symbol_type, mode);
      }
  return 0;
}

/* Return the number of instructions needed to load constant X.
   Return 0 if X isn't a valid constant.  */

int
mips_const_insns (rtx x)
{
  struct mips_integer_op codes[MIPS_MAX_INTEGER_OPS];
  enum mips_symbol_type symbol_type;
  rtx offset;

  switch (GET_CODE (x))
    {
    case HIGH:
      if (!mips_symbolic_constant_p (XEXP (x, 0), SYMBOL_CONTEXT_LEA,
				     &symbol_type)
	  || !mips_split_p[symbol_type])
	return 0;

      /* This is simply an LUI for normal mode.  It is an extended
	 LI followed by an extended SLL for MIPS16.  */
      return TARGET_MIPS16 ? 4 : 1;

    case CONST_INT:
      if (TARGET_MIPS16)
	/* Unsigned 8-bit constants can be loaded using an unextended
	   LI instruction.  Unsigned 16-bit constants can be loaded
	   using an extended LI.  Negative constants must be loaded
	   using LI and then negated.  */
	return (IN_RANGE (INTVAL (x), 0, 255) ? 1
		: SMALL_OPERAND_UNSIGNED (INTVAL (x)) ? 2
		: IN_RANGE (-INTVAL (x), 0, 255) ? 2
		: SMALL_OPERAND_UNSIGNED (-INTVAL (x)) ? 3
		: 0);

      return mips_build_integer (codes, INTVAL (x));

    case CONST_DOUBLE:
    case CONST_VECTOR:
      /* Allow zeros for normal mode, where we can use $0.  */
      return !TARGET_MIPS16 && x == CONST0_RTX (GET_MODE (x)) ? 1 : 0;

    case CONST:
      if (CONST_GP_P (x))
	return 1;

      /* See if we can refer to X directly.  */
      if (mips_symbolic_constant_p (x, SYMBOL_CONTEXT_LEA, &symbol_type))
	return mips_symbol_insns (symbol_type, MAX_MACHINE_MODE);

      /* Otherwise try splitting the constant into a base and offset.
	 If the offset is a 16-bit value, we can load the base address
	 into a register and then use (D)ADDIU to add in the offset.
	 If the offset is larger, we can load the base and offset
	 into separate registers and add them together with (D)ADDU.
	 However, the latter is only possible before reload; during
	 and after reload, we must have the option of forcing the
	 constant into the pool instead.  */
      split_const (x, &x, &offset);
      if (offset != 0)
	{
	  int n = mips_const_insns (x);
	  if (n != 0)
	    {
	      if (SMALL_INT (offset))
		return n + 1;
	      else if (!targetm.cannot_force_const_mem (x))
		return n + 1 + mips_build_integer (codes, INTVAL (offset));
	    }
	}
      return 0;

    case SYMBOL_REF:
    case LABEL_REF:
      return mips_symbol_insns (mips_classify_symbol (x, SYMBOL_CONTEXT_LEA),
				MAX_MACHINE_MODE);

    default:
      return 0;
    }
}

/* X is a doubleword constant that can be handled by splitting it into
   two words and loading each word separately.  Return the number of
   instructions required to do this.  */

int
mips_split_const_insns (rtx x)
{
  unsigned int low, high;

  low = mips_const_insns (mips_subword (x, false));
  high = mips_const_insns (mips_subword (x, true));
  gcc_assert (low > 0 && high > 0);
  return low + high;
}

/* Return the number of instructions needed to implement INSN,
   given that it loads from or stores to MEM.  Count extended
   MIPS16 instructions as two instructions.  */

int
mips_load_store_insns (rtx mem, rtx insn)
{
  enum machine_mode mode;
  bool might_split_p;
  rtx set;

  gcc_assert (MEM_P (mem));
  mode = GET_MODE (mem);

  /* Try to prove that INSN does not need to be split.  */
  might_split_p = true;
  if (GET_MODE_BITSIZE (mode) == 64)
    {
      set = single_set (insn);
      if (set && !mips_split_64bit_move_p (SET_DEST (set), SET_SRC (set)))
	might_split_p = false;
    }

  return mips_address_insns (XEXP (mem, 0), mode, might_split_p);
}

/* Return the number of instructions needed for an integer division.  */

int
mips_idiv_insns (void)
{
  int count;

  count = 1;
  if (TARGET_CHECK_ZERO_DIV)
    {
      if (GENERATE_DIVIDE_TRAPS)
        count++;
      else
        count += 2;
    }

  if (TARGET_FIX_R4000 || TARGET_FIX_R4400)
    count++;
  return count;
}

/* Emit a move from SRC to DEST.  Assume that the move expanders can
   handle all moves if !can_create_pseudo_p ().  The distinction is
   important because, unlike emit_move_insn, the move expanders know
   how to force Pmode objects into the constant pool even when the
   constant pool address is not itself legitimate.  */

rtx
mips_emit_move (rtx dest, rtx src)
{
  return (can_create_pseudo_p ()
	  ? emit_move_insn (dest, src)
	  : emit_move_insn_1 (dest, src));
}

/* Emit an instruction of the form (set TARGET (CODE OP0)).  */

static void
mips_emit_unary (enum rtx_code code, rtx target, rtx op0)
{
  emit_insn (gen_rtx_SET (VOIDmode, target,
			  gen_rtx_fmt_e (code, GET_MODE (op0), op0)));
}

/* Compute (CODE OP0) and store the result in a new register of mode MODE.
   Return that new register.  */

static rtx
mips_force_unary (enum machine_mode mode, enum rtx_code code, rtx op0)
{
  rtx reg;

  reg = gen_reg_rtx (mode);
  mips_emit_unary (code, reg, op0);
  return reg;
}

/* Emit an instruction of the form (set TARGET (CODE OP0 OP1)).  */

static void
mips_emit_binary (enum rtx_code code, rtx target, rtx op0, rtx op1)
{
  emit_insn (gen_rtx_SET (VOIDmode, target,
			  gen_rtx_fmt_ee (code, GET_MODE (target), op0, op1)));
}

/* Compute (CODE OP0 OP1) and store the result in a new register
   of mode MODE.  Return that new register.  */

static rtx
mips_force_binary (enum machine_mode mode, enum rtx_code code, rtx op0, rtx op1)
{
  rtx reg;

  reg = gen_reg_rtx (mode);
  mips_emit_binary (code, reg, op0, op1);
  return reg;
}

/* Copy VALUE to a register and return that register.  If new pseudos
   are allowed, copy it into a new register, otherwise use DEST.  */

static rtx
mips_force_temporary (rtx dest, rtx value)
{
  if (can_create_pseudo_p ())
    return force_reg (Pmode, value);
  else
    {
      mips_emit_move (dest, value);
      return dest;
    }
}

/* Emit a call sequence with call pattern PATTERN and return the call
   instruction itself (which is not necessarily the last instruction
   emitted).  ORIG_ADDR is the original, unlegitimized address,
   ADDR is the legitimized form, and LAZY_P is true if the call
   address is lazily-bound.  */

static rtx
mips_emit_call_insn (rtx pattern, rtx orig_addr, rtx addr, bool lazy_p)
{
  rtx insn, reg;

  insn = emit_call_insn (pattern);

  if (TARGET_MIPS16 && mips_use_pic_fn_addr_reg_p (orig_addr))
    {
      /* MIPS16 JALRs only take MIPS16 registers.  If the target
	 function requires $25 to be valid on entry, we must copy it
	 there separately.  The move instruction can be put in the
	 call's delay slot.  */
      reg = gen_rtx_REG (Pmode, PIC_FUNCTION_ADDR_REGNUM);
      emit_insn_before (gen_move_insn (reg, addr), insn);
      use_reg (&CALL_INSN_FUNCTION_USAGE (insn), reg);
    }

  if (lazy_p)
    /* Lazy-binding stubs require $gp to be valid on entry.  */
    use_reg (&CALL_INSN_FUNCTION_USAGE (insn), pic_offset_table_rtx);

  if (TARGET_USE_GOT)
    {
      /* See the comment above load_call<mode> for details.  */
      use_reg (&CALL_INSN_FUNCTION_USAGE (insn),
	       gen_rtx_REG (Pmode, GOT_VERSION_REGNUM));
      emit_insn (gen_update_got_version ());
    }
  return insn;
}

/* Wrap symbol or label BASE in an UNSPEC address of type SYMBOL_TYPE,
   then add CONST_INT OFFSET to the result.  */

static rtx
mips_unspec_address_offset (rtx base, rtx offset,
			    enum mips_symbol_type symbol_type)
{
  base = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, base),
			 UNSPEC_ADDRESS_FIRST + symbol_type);
  if (offset != const0_rtx)
    base = gen_rtx_PLUS (Pmode, base, offset);
  return gen_rtx_CONST (Pmode, base);
}

/* Return an UNSPEC address with underlying address ADDRESS and symbol
   type SYMBOL_TYPE.  */

rtx
mips_unspec_address (rtx address, enum mips_symbol_type symbol_type)
{
  rtx base, offset;

  split_const (address, &base, &offset);
  return mips_unspec_address_offset (base, offset, symbol_type);
}

/* If OP is an UNSPEC address, return the address to which it refers,
   otherwise return OP itself.  */

static rtx
mips_strip_unspec_address (rtx op)
{
  rtx base, offset;

  split_const (op, &base, &offset);
  if (UNSPEC_ADDRESS_P (base))
    op = plus_constant (UNSPEC_ADDRESS (base), INTVAL (offset));
  return op;
}

/* If mips_unspec_address (ADDR, SYMBOL_TYPE) is a 32-bit value, add the
   high part to BASE and return the result.  Just return BASE otherwise.
   TEMP is as for mips_force_temporary.

   The returned expression can be used as the first operand to a LO_SUM.  */

static rtx
mips_unspec_offset_high (rtx temp, rtx base, rtx addr,
			 enum mips_symbol_type symbol_type)
{
  if (mips_split_p[symbol_type])
    {
      addr = gen_rtx_HIGH (Pmode, mips_unspec_address (addr, symbol_type));
      addr = mips_force_temporary (temp, addr);
      base = mips_force_temporary (temp, gen_rtx_PLUS (Pmode, addr, base));
    }
  return base;
}

/* Return an instruction that copies $gp into register REG.  We want
   GCC to treat the register's value as constant, so that its value
   can be rematerialized on demand.  */

static rtx
gen_load_const_gp (rtx reg)
{
  return (Pmode == SImode
	  ? gen_load_const_gp_si (reg)
	  : gen_load_const_gp_di (reg));
}

/* Return a pseudo register that contains the value of $gp throughout
   the current function.  Such registers are needed by MIPS16 functions,
   for which $gp itself is not a valid base register or addition operand.  */

static rtx
mips16_gp_pseudo_reg (void)
{
  if (cfun->machine->mips16_gp_pseudo_rtx == NULL_RTX)
    {
      rtx insn, scan;

      cfun->machine->mips16_gp_pseudo_rtx = gen_reg_rtx (Pmode);

      push_topmost_sequence ();

      scan = get_insns ();
      while (NEXT_INSN (scan) && !INSN_P (NEXT_INSN (scan)))
	scan = NEXT_INSN (scan);

      insn = gen_load_const_gp (cfun->machine->mips16_gp_pseudo_rtx);
      emit_insn_after (insn, scan);

      pop_topmost_sequence ();
    }

  return cfun->machine->mips16_gp_pseudo_rtx;
}

/* Return a base register that holds pic_offset_table_rtx.
   TEMP, if nonnull, is a scratch Pmode base register.  */

rtx
mips_pic_base_register (rtx temp)
{
  if (!TARGET_MIPS16)
    return pic_offset_table_rtx;

  if (currently_expanding_to_rtl)
    return mips16_gp_pseudo_reg ();

  if (can_create_pseudo_p ())
    temp = gen_reg_rtx (Pmode);

  if (TARGET_USE_GOT)
    /* The first post-reload split exposes all references to $gp
       (both uses and definitions).  All references must remain
       explicit after that point.

       It is safe to introduce uses of $gp at any time, so for
       simplicity, we do that before the split too.  */
    mips_emit_move (temp, pic_offset_table_rtx);
  else
    emit_insn (gen_load_const_gp (temp));
  return temp;
}

/* Return the RHS of a load_call<mode> insn.  */

static rtx
mips_unspec_call (rtx reg, rtx symbol)
{
  rtvec vec;

  vec = gen_rtvec (3, reg, symbol, gen_rtx_REG (SImode, GOT_VERSION_REGNUM));
  return gen_rtx_UNSPEC (Pmode, vec, UNSPEC_LOAD_CALL);
}

/* If SRC is the RHS of a load_call<mode> insn, return the underlying symbol
   reference.  Return NULL_RTX otherwise.  */

static rtx
mips_strip_unspec_call (rtx src)
{
  if (GET_CODE (src) == UNSPEC && XINT (src, 1) == UNSPEC_LOAD_CALL)
    return mips_strip_unspec_address (XVECEXP (src, 0, 1));
  return NULL_RTX;
}

/* Create and return a GOT reference of type TYPE for address ADDR.
   TEMP, if nonnull, is a scratch Pmode base register.  */

rtx
mips_got_load (rtx temp, rtx addr, enum mips_symbol_type type)
{
  rtx base, high, lo_sum_symbol;

  base = mips_pic_base_register (temp);

  /* If we used the temporary register to load $gp, we can't use
     it for the high part as well.  */
  if (temp != NULL && reg_overlap_mentioned_p (base, temp))
    temp = NULL;

  high = mips_unspec_offset_high (temp, base, addr, type);
  lo_sum_symbol = mips_unspec_address (addr, type);

  if (type == SYMBOL_GOTOFF_CALL)
    return mips_unspec_call (high, lo_sum_symbol);
  else
    return (Pmode == SImode
	    ? gen_unspec_gotsi (high, lo_sum_symbol)
	    : gen_unspec_gotdi (high, lo_sum_symbol));
}

/* If MODE is MAX_MACHINE_MODE, ADDR appears as a move operand, otherwise
   it appears in a MEM of that mode.  Return true if ADDR is a legitimate
   constant in that context and can be split into high and low parts.
   If so, and if LOW_OUT is nonnull, emit the high part and store the
   low part in *LOW_OUT.  Leave *LOW_OUT unchanged otherwise.

   TEMP is as for mips_force_temporary and is used to load the high
   part into a register.

   When MODE is MAX_MACHINE_MODE, the low part is guaranteed to be
   a legitimize SET_SRC for an .md pattern, otherwise the low part
   is guaranteed to be a legitimate address for mode MODE.  */

bool
mips_split_symbol (rtx temp, rtx addr, enum machine_mode mode, rtx *low_out)
{
  enum mips_symbol_context context;
  enum mips_symbol_type symbol_type;
  rtx high;

  context = (mode == MAX_MACHINE_MODE
	     ? SYMBOL_CONTEXT_LEA
	     : SYMBOL_CONTEXT_MEM);
  if (GET_CODE (addr) == HIGH && context == SYMBOL_CONTEXT_LEA)
    {
      addr = XEXP (addr, 0);
      if (mips_symbolic_constant_p (addr, context, &symbol_type)
	  && mips_symbol_insns (symbol_type, mode) > 0
	  && mips_split_hi_p[symbol_type])
	{
	  if (low_out)
	    switch (symbol_type)
	      {
	      case SYMBOL_GOT_PAGE_OFST:
		/* The high part of a page/ofst pair is loaded from the GOT.  */
		*low_out = mips_got_load (temp, addr, SYMBOL_GOTOFF_PAGE);
		break;

	      default:
		gcc_unreachable ();
	      }
	  return true;
	}
    }
  else
    {
      if (mips_symbolic_constant_p (addr, context, &symbol_type)
	  && mips_symbol_insns (symbol_type, mode) > 0
	  && mips_split_p[symbol_type])
	{
	  if (low_out)
	    switch (symbol_type)
	      {
	      case SYMBOL_GOT_DISP:
		/* SYMBOL_GOT_DISP symbols are loaded from the GOT.  */
		*low_out = mips_got_load (temp, addr, SYMBOL_GOTOFF_DISP);
		break;

	      case SYMBOL_GP_RELATIVE:
		high = mips_pic_base_register (temp);
		*low_out = gen_rtx_LO_SUM (Pmode, high, addr);
		break;

	      default:
		high = gen_rtx_HIGH (Pmode, copy_rtx (addr));
		high = mips_force_temporary (temp, high);
		*low_out = gen_rtx_LO_SUM (Pmode, high, addr);
		break;
	      }
	  return true;
	}
    }
  return false;
}

/* Return a legitimate address for REG + OFFSET.  TEMP is as for
   mips_force_temporary; it is only needed when OFFSET is not a
   SMALL_OPERAND.  */

static rtx
mips_add_offset (rtx temp, rtx reg, HOST_WIDE_INT offset)
{
  if (!SMALL_OPERAND (offset))
    {
      rtx high;

      if (TARGET_MIPS16)
	{
	  /* Load the full offset into a register so that we can use
	     an unextended instruction for the address itself.  */
	  high = GEN_INT (offset);
	  offset = 0;
	}
      else
	{
	  /* Leave OFFSET as a 16-bit offset and put the excess in HIGH.
	     The addition inside the macro CONST_HIGH_PART may cause an
	     overflow, so we need to force a sign-extension check.  */
	  high = gen_int_mode (CONST_HIGH_PART (offset), Pmode);
	  offset = CONST_LOW_PART (offset);
	}
      high = mips_force_temporary (temp, high);
      reg = mips_force_temporary (temp, gen_rtx_PLUS (Pmode, high, reg));
    }
  return plus_constant (reg, offset);
}

/* The __tls_get_attr symbol.  */
static GTY(()) rtx mips_tls_symbol;

/* Return an instruction sequence that calls __tls_get_addr.  SYM is
   the TLS symbol we are referencing and TYPE is the symbol type to use
   (either global dynamic or local dynamic).  V0 is an RTX for the
   return value location.  */

static rtx
mips_call_tls_get_addr (rtx sym, enum mips_symbol_type type, rtx v0)
{
  rtx insn, loc, a0;

  a0 = gen_rtx_REG (Pmode, GP_ARG_FIRST);

  if (!mips_tls_symbol)
    mips_tls_symbol = init_one_libfunc ("__tls_get_addr");

  loc = mips_unspec_address (sym, type);

  start_sequence ();

  emit_insn (gen_rtx_SET (Pmode, a0,
			  gen_rtx_LO_SUM (Pmode, pic_offset_table_rtx, loc)));
  insn = mips_expand_call (MIPS_CALL_NORMAL, v0, mips_tls_symbol,
			   const0_rtx, NULL_RTX, false);
  RTL_CONST_CALL_P (insn) = 1;
  use_reg (&CALL_INSN_FUNCTION_USAGE (insn), a0);
  insn = get_insns ();

  end_sequence ();

  return insn;
}

/* Return a pseudo register that contains the current thread pointer.  */

static rtx
mips_get_tp (void)
{
  rtx tp;

  tp = gen_reg_rtx (Pmode);
  if (Pmode == DImode)
    emit_insn (gen_tls_get_tp_di (tp));
  else
    emit_insn (gen_tls_get_tp_si (tp));
  return tp;
}

/* Generate the code to access LOC, a thread-local SYMBOL_REF, and return
   its address.  The return value will be both a valid address and a valid
   SET_SRC (either a REG or a LO_SUM).  */

static rtx
mips_legitimize_tls_address (rtx loc)
{
  rtx dest, insn, v0, tp, tmp1, tmp2, eqv;
  enum tls_model model;

  if (TARGET_MIPS16)
    {
      sorry ("MIPS16 TLS");
      return gen_reg_rtx (Pmode);
    }

  model = SYMBOL_REF_TLS_MODEL (loc);
  /* Only TARGET_ABICALLS code can have more than one module; other
     code must be be static and should not use a GOT.  All TLS models
     reduce to local exec in this situation.  */
  if (!TARGET_ABICALLS)
    model = TLS_MODEL_LOCAL_EXEC;

  switch (model)
    {
    case TLS_MODEL_GLOBAL_DYNAMIC:
      v0 = gen_rtx_REG (Pmode, GP_RETURN);
      insn = mips_call_tls_get_addr (loc, SYMBOL_TLSGD, v0);
      dest = gen_reg_rtx (Pmode);
      emit_libcall_block (insn, dest, v0, loc);
      break;

    case TLS_MODEL_LOCAL_DYNAMIC:
      v0 = gen_rtx_REG (Pmode, GP_RETURN);
      insn = mips_call_tls_get_addr (loc, SYMBOL_TLSLDM, v0);
      tmp1 = gen_reg_rtx (Pmode);

      /* Attach a unique REG_EQUIV, to allow the RTL optimizers to
	 share the LDM result with other LD model accesses.  */
      eqv = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, const0_rtx),
			    UNSPEC_TLS_LDM);
      emit_libcall_block (insn, tmp1, v0, eqv);

      tmp2 = mips_unspec_offset_high (NULL, tmp1, loc, SYMBOL_DTPREL);
      dest = gen_rtx_LO_SUM (Pmode, tmp2,
			     mips_unspec_address (loc, SYMBOL_DTPREL));
      break;

    case TLS_MODEL_INITIAL_EXEC:
      tp = mips_get_tp ();
      tmp1 = gen_reg_rtx (Pmode);
      tmp2 = mips_unspec_address (loc, SYMBOL_GOTTPREL);
      if (Pmode == DImode)
	emit_insn (gen_load_gotdi (tmp1, pic_offset_table_rtx, tmp2));
      else
	emit_insn (gen_load_gotsi (tmp1, pic_offset_table_rtx, tmp2));
      dest = gen_reg_rtx (Pmode);
      emit_insn (gen_add3_insn (dest, tmp1, tp));
      break;

    case TLS_MODEL_LOCAL_EXEC:
      tp = mips_get_tp ();
      tmp1 = mips_unspec_offset_high (NULL, tp, loc, SYMBOL_TPREL);
      dest = gen_rtx_LO_SUM (Pmode, tmp1,
			     mips_unspec_address (loc, SYMBOL_TPREL));
      break;

    default:
      gcc_unreachable ();
    }
  return dest;
}

/* If X is not a valid address for mode MODE, force it into a register.  */

static rtx
mips_force_address (rtx x, enum machine_mode mode)
{
  if (!mips_legitimate_address_p (mode, x, false))
    x = force_reg (Pmode, x);
  return x;
}

/* This function is used to implement LEGITIMIZE_ADDRESS.  If X can
   be legitimized in a way that the generic machinery might not expect,
   return a new address, otherwise return NULL.  MODE is the mode of
   the memory being accessed.  */

static rtx
mips_legitimize_address (rtx x, rtx oldx ATTRIBUTE_UNUSED,
			 enum machine_mode mode)
{
  rtx base, addr;
  HOST_WIDE_INT offset;

  if (mips_tls_symbol_p (x))
    return mips_legitimize_tls_address (x);

  /* See if the address can split into a high part and a LO_SUM.  */
  if (mips_split_symbol (NULL, x, mode, &addr))
    return mips_force_address (addr, mode);

  /* Handle BASE + OFFSET using mips_add_offset.  */
  mips_split_plus (x, &base, &offset);
  if (offset != 0)
    {
      if (!mips_valid_base_register_p (base, mode, false))
	base = copy_to_mode_reg (Pmode, base);
      addr = mips_add_offset (NULL, base, offset);
      return mips_force_address (addr, mode);
    }

  return x;
}

/* Load VALUE into DEST.  TEMP is as for mips_force_temporary.  */

void
mips_move_integer (rtx temp, rtx dest, unsigned HOST_WIDE_INT value)
{
  struct mips_integer_op codes[MIPS_MAX_INTEGER_OPS];
  enum machine_mode mode;
  unsigned int i, num_ops;
  rtx x;

  mode = GET_MODE (dest);
  num_ops = mips_build_integer (codes, value);

  /* Apply each binary operation to X.  Invariant: X is a legitimate
     source operand for a SET pattern.  */
  x = GEN_INT (codes[0].value);
  for (i = 1; i < num_ops; i++)
    {
      if (!can_create_pseudo_p ())
	{
	  emit_insn (gen_rtx_SET (VOIDmode, temp, x));
	  x = temp;
	}
      else
	x = force_reg (mode, x);
      x = gen_rtx_fmt_ee (codes[i].code, mode, x, GEN_INT (codes[i].value));
    }

  emit_insn (gen_rtx_SET (VOIDmode, dest, x));
}

/* Subroutine of mips_legitimize_move.  Move constant SRC into register
   DEST given that SRC satisfies immediate_operand but doesn't satisfy
   move_operand.  */

static void
mips_legitimize_const_move (enum machine_mode mode, rtx dest, rtx src)
{
  rtx base, offset;

  /* Split moves of big integers into smaller pieces.  */
  if (splittable_const_int_operand (src, mode))
    {
      mips_move_integer (dest, dest, INTVAL (src));
      return;
    }

  /* Split moves of symbolic constants into high/low pairs.  */
  if (mips_split_symbol (dest, src, MAX_MACHINE_MODE, &src))
    {
      emit_insn (gen_rtx_SET (VOIDmode, dest, src));
      return;
    }

  /* Generate the appropriate access sequences for TLS symbols.  */
  if (mips_tls_symbol_p (src))
    {
      mips_emit_move (dest, mips_legitimize_tls_address (src));
      return;
    }

  /* If we have (const (plus symbol offset)), and that expression cannot
     be forced into memory, load the symbol first and add in the offset.
     In non-MIPS16 mode, prefer to do this even if the constant _can_ be
     forced into memory, as it usually produces better code.  */
  split_const (src, &base, &offset);
  if (offset != const0_rtx
      && (targetm.cannot_force_const_mem (src)
	  || (!TARGET_MIPS16 && can_create_pseudo_p ())))
    {
      base = mips_force_temporary (dest, base);
      mips_emit_move (dest, mips_add_offset (NULL, base, INTVAL (offset)));
      return;
    }

  src = force_const_mem (mode, src);

  /* When using explicit relocs, constant pool references are sometimes
     not legitimate addresses.  */
  mips_split_symbol (dest, XEXP (src, 0), mode, &XEXP (src, 0));
  mips_emit_move (dest, src);
}

/* If (set DEST SRC) is not a valid move instruction, emit an equivalent
   sequence that is valid.  */

bool
mips_legitimize_move (enum machine_mode mode, rtx dest, rtx src)
{
  if (!register_operand (dest, mode) && !reg_or_0_operand (src, mode))
    {
      mips_emit_move (dest, force_reg (mode, src));
      return true;
    }

  /* We need to deal with constants that would be legitimate
     immediate_operands but aren't legitimate move_operands.  */
  if (CONSTANT_P (src) && !move_operand (src, mode))
    {
      mips_legitimize_const_move (mode, dest, src);
      set_unique_reg_note (get_last_insn (), REG_EQUAL, copy_rtx (src));
      return true;
    }
  return false;
}

/* Return true if value X in context CONTEXT is a small-data address
   that can be rewritten as a LO_SUM.  */

static bool
mips_rewrite_small_data_p (rtx x, enum mips_symbol_context context)
{
  enum mips_symbol_type symbol_type;

  return (mips_lo_relocs[SYMBOL_GP_RELATIVE]
	  && !mips_split_p[SYMBOL_GP_RELATIVE]
	  && mips_symbolic_constant_p (x, context, &symbol_type)
	  && symbol_type == SYMBOL_GP_RELATIVE);
}

/* A for_each_rtx callback for mips_small_data_pattern_p.  DATA is the
   containing MEM, or null if none.  */

static int
mips_small_data_pattern_1 (rtx *loc, void *data)
{
  enum mips_symbol_context context;

  if (GET_CODE (*loc) == LO_SUM)
    return -1;

  if (MEM_P (*loc))
    {
      if (for_each_rtx (&XEXP (*loc, 0), mips_small_data_pattern_1, *loc))
	return 1;
      return -1;
    }

  context = data ? SYMBOL_CONTEXT_MEM : SYMBOL_CONTEXT_LEA;
  return mips_rewrite_small_data_p (*loc, context);
}

/* Return true if OP refers to small data symbols directly, not through
   a LO_SUM.  */

bool
mips_small_data_pattern_p (rtx op)
{
  return for_each_rtx (&op, mips_small_data_pattern_1, NULL);
}

/* A for_each_rtx callback, used by mips_rewrite_small_data.
   DATA is the containing MEM, or null if none.  */

static int
mips_rewrite_small_data_1 (rtx *loc, void *data)
{
  enum mips_symbol_context context;

  if (MEM_P (*loc))
    {
      for_each_rtx (&XEXP (*loc, 0), mips_rewrite_small_data_1, *loc);
      return -1;
    }

  context = data ? SYMBOL_CONTEXT_MEM : SYMBOL_CONTEXT_LEA;
  if (mips_rewrite_small_data_p (*loc, context))
    *loc = gen_rtx_LO_SUM (Pmode, pic_offset_table_rtx, *loc);

  if (GET_CODE (*loc) == LO_SUM)
    return -1;

  return 0;
}

/* Rewrite instruction pattern PATTERN so that it refers to small data
   using explicit relocations.  */

rtx
mips_rewrite_small_data (rtx pattern)
{
  pattern = copy_insn (pattern);
  for_each_rtx (&pattern, mips_rewrite_small_data_1, NULL);
  return pattern;
}

/* We need a lot of little routines to check the range of MIPS16 immediate
   operands.  */

static int
m16_check_op (rtx op, int low, int high, int mask)
{
  return (CONST_INT_P (op)
	  && IN_RANGE (INTVAL (op), low, high)
	  && (INTVAL (op) & mask) == 0);
}

int
m16_uimm3_b (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return m16_check_op (op, 0x1, 0x8, 0);
}

int
m16_simm4_1 (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return m16_check_op (op, -0x8, 0x7, 0);
}

int
m16_nsimm4_1 (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return m16_check_op (op, -0x7, 0x8, 0);
}

int
m16_simm5_1 (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return m16_check_op (op, -0x10, 0xf, 0);
}

int
m16_nsimm5_1 (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return m16_check_op (op, -0xf, 0x10, 0);
}

int
m16_uimm5_4 (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return m16_check_op (op, -0x10 << 2, 0xf << 2, 3);
}

int
m16_nuimm5_4 (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return m16_check_op (op, -0xf << 2, 0x10 << 2, 3);
}

int
m16_simm8_1 (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return m16_check_op (op, -0x80, 0x7f, 0);
}

int
m16_nsimm8_1 (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return m16_check_op (op, -0x7f, 0x80, 0);
}

int
m16_uimm8_1 (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return m16_check_op (op, 0x0, 0xff, 0);
}

int
m16_nuimm8_1 (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return m16_check_op (op, -0xff, 0x0, 0);
}

int
m16_uimm8_m1_1 (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return m16_check_op (op, -0x1, 0xfe, 0);
}

int
m16_uimm8_4 (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return m16_check_op (op, 0x0, 0xff << 2, 3);
}

int
m16_nuimm8_4 (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return m16_check_op (op, -0xff << 2, 0x0, 3);
}

int
m16_simm8_8 (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return m16_check_op (op, -0x80 << 3, 0x7f << 3, 7);
}

int
m16_nsimm8_8 (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return m16_check_op (op, -0x7f << 3, 0x80 << 3, 7);
}

/* The cost of loading values from the constant pool.  It should be
   larger than the cost of any constant we want to synthesize inline.  */
#define CONSTANT_POOL_COST COSTS_N_INSNS (TARGET_MIPS16 ? 4 : 8)

/* Return the cost of X when used as an operand to the MIPS16 instruction
   that implements CODE.  Return -1 if there is no such instruction, or if
   X is not a valid immediate operand for it.  */

static int
mips16_constant_cost (int code, HOST_WIDE_INT x)
{
  switch (code)
    {
    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      /* Shifts by between 1 and 8 bits (inclusive) are unextended,
	 other shifts are extended.  The shift patterns truncate the shift
	 count to the right size, so there are no out-of-range values.  */
      if (IN_RANGE (x, 1, 8))
	return 0;
      return COSTS_N_INSNS (1);

    case PLUS:
      if (IN_RANGE (x, -128, 127))
	return 0;
      if (SMALL_OPERAND (x))
	return COSTS_N_INSNS (1);
      return -1;

    case LEU:
      /* Like LE, but reject the always-true case.  */
      if (x == -1)
	return -1;
    case LE:
      /* We add 1 to the immediate and use SLT.  */
      x += 1;
    case XOR:
      /* We can use CMPI for an xor with an unsigned 16-bit X.  */
    case LT:
    case LTU:
      if (IN_RANGE (x, 0, 255))
	return 0;
      if (SMALL_OPERAND_UNSIGNED (x))
	return COSTS_N_INSNS (1);
      return -1;

    case EQ:
    case NE:
      /* Equality comparisons with 0 are cheap.  */
      if (x == 0)
	return 0;
      return -1;

    default:
      return -1;
    }
}

/* Return true if there is a non-MIPS16 instruction that implements CODE
   and if that instruction accepts X as an immediate operand.  */

static int
mips_immediate_operand_p (int code, HOST_WIDE_INT x)
{
  switch (code)
    {
    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      /* All shift counts are truncated to a valid constant.  */
      return true;

    case ROTATE:
    case ROTATERT:
      /* Likewise rotates, if the target supports rotates at all.  */
      return ISA_HAS_ROR;

    case AND:
    case IOR:
    case XOR:
      /* These instructions take 16-bit unsigned immediates.  */
      return SMALL_OPERAND_UNSIGNED (x);

    case PLUS:
    case LT:
    case LTU:
      /* These instructions take 16-bit signed immediates.  */
      return SMALL_OPERAND (x);

    case EQ:
    case NE:
    case GT:
    case GTU:
      /* The "immediate" forms of these instructions are really
	 implemented as comparisons with register 0.  */
      return x == 0;

    case GE:
    case GEU:
      /* Likewise, meaning that the only valid immediate operand is 1.  */
      return x == 1;

    case LE:
      /* We add 1 to the immediate and use SLT.  */
      return SMALL_OPERAND (x + 1);

    case LEU:
      /* Likewise SLTU, but reject the always-true case.  */
      return SMALL_OPERAND (x + 1) && x + 1 != 0;

    case SIGN_EXTRACT:
    case ZERO_EXTRACT:
      /* The bit position and size are immediate operands.  */
      return ISA_HAS_EXT_INS;

    default:
      /* By default assume that $0 can be used for 0.  */
      return x == 0;
    }
}

/* Return the cost of binary operation X, given that the instruction
   sequence for a word-sized or smaller operation has cost SINGLE_COST
   and that the sequence of a double-word operation has cost DOUBLE_COST.
   If SPEED is true, optimize for speed otherwise optimize for size.  */

static int
mips_binary_cost (rtx x, int single_cost, int double_cost, bool speed)
{
  int cost;

  if (GET_MODE_SIZE (GET_MODE (x)) == UNITS_PER_WORD * 2)
    cost = double_cost;
  else
    cost = single_cost;
  return (cost
	  + rtx_cost (XEXP (x, 0), SET, speed)
	  + rtx_cost (XEXP (x, 1), GET_CODE (x), speed));
}

/* Return the cost of floating-point multiplications of mode MODE.  */

static int
mips_fp_mult_cost (enum machine_mode mode)
{
  return mode == DFmode ? mips_cost->fp_mult_df : mips_cost->fp_mult_sf;
}

/* Return the cost of floating-point divisions of mode MODE.  */

static int
mips_fp_div_cost (enum machine_mode mode)
{
  return mode == DFmode ? mips_cost->fp_div_df : mips_cost->fp_div_sf;
}

/* Return the cost of sign-extending OP to mode MODE, not including the
   cost of OP itself.  */

static int
mips_sign_extend_cost (enum machine_mode mode, rtx op)
{
  if (MEM_P (op))
    /* Extended loads are as cheap as unextended ones.  */
    return 0;

  if (TARGET_64BIT && mode == DImode && GET_MODE (op) == SImode)
    /* A sign extension from SImode to DImode in 64-bit mode is free.  */
    return 0;

  if (ISA_HAS_SEB_SEH || GENERATE_MIPS16E)
    /* We can use SEB or SEH.  */
    return COSTS_N_INSNS (1);

  /* We need to use a shift left and a shift right.  */
  return COSTS_N_INSNS (TARGET_MIPS16 ? 4 : 2);
}

/* Return the cost of zero-extending OP to mode MODE, not including the
   cost of OP itself.  */

static int
mips_zero_extend_cost (enum machine_mode mode, rtx op)
{
  if (MEM_P (op))
    /* Extended loads are as cheap as unextended ones.  */
    return 0;

  if (TARGET_64BIT && mode == DImode && GET_MODE (op) == SImode)
    /* We need a shift left by 32 bits and a shift right by 32 bits.  */
    return COSTS_N_INSNS (TARGET_MIPS16 ? 4 : 2);

  if (GENERATE_MIPS16E)
    /* We can use ZEB or ZEH.  */
    return COSTS_N_INSNS (1);

  if (TARGET_MIPS16)
    /* We need to load 0xff or 0xffff into a register and use AND.  */
    return COSTS_N_INSNS (GET_MODE (op) == QImode ? 2 : 3);

  /* We can use ANDI.  */
  return COSTS_N_INSNS (1);
}

/* Implement TARGET_RTX_COSTS.  */

static bool
mips_rtx_costs (rtx x, int code, int outer_code, int *total, bool speed)
{
  enum machine_mode mode = GET_MODE (x);
  bool float_mode_p = FLOAT_MODE_P (mode);
  int cost;
  rtx addr;

  /* The cost of a COMPARE is hard to define for MIPS.  COMPAREs don't
     appear in the instruction stream, and the cost of a comparison is
     really the cost of the branch or scc condition.  At the time of
     writing, GCC only uses an explicit outer COMPARE code when optabs
     is testing whether a constant is expensive enough to force into a
     register.  We want optabs to pass such constants through the MIPS
     expanders instead, so make all constants very cheap here.  */
  if (outer_code == COMPARE)
    {
      gcc_assert (CONSTANT_P (x));
      *total = 0;
      return true;
    }

  switch (code)
    {
    case CONST_INT:
      /* Treat *clear_upper32-style ANDs as having zero cost in the
	 second operand.  The cost is entirely in the first operand.

	 ??? This is needed because we would otherwise try to CSE
	 the constant operand.  Although that's the right thing for
	 instructions that continue to be a register operation throughout
	 compilation, it is disastrous for instructions that could
	 later be converted into a memory operation.  */
      if (TARGET_64BIT
	  && outer_code == AND
	  && UINTVAL (x) == 0xffffffff)
	{
	  *total = 0;
	  return true;
	}

      if (TARGET_MIPS16)
	{
	  cost = mips16_constant_cost (outer_code, INTVAL (x));
	  if (cost >= 0)
	    {
	      *total = cost;
	      return true;
	    }
	}
      else
	{
	  /* When not optimizing for size, we care more about the cost
	     of hot code, and hot code is often in a loop.  If a constant
	     operand needs to be forced into a register, we will often be
	     able to hoist the constant load out of the loop, so the load
	     should not contribute to the cost.  */
	  if (speed || mips_immediate_operand_p (outer_code, INTVAL (x)))
	    {
	      *total = 0;
	      return true;
	    }
	}
      /* Fall through.  */

    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
    case CONST_DOUBLE:
      if (force_to_mem_operand (x, VOIDmode))
	{
	  *total = COSTS_N_INSNS (1);
	  return true;
	}
      cost = mips_const_insns (x);
      if (cost > 0)
	{
	  /* If the constant is likely to be stored in a GPR, SETs of
	     single-insn constants are as cheap as register sets; we
	     never want to CSE them.

	     Don't reduce the cost of storing a floating-point zero in
	     FPRs.  If we have a zero in an FPR for other reasons, we
	     can get better cfg-cleanup and delayed-branch results by
	     using it consistently, rather than using $0 sometimes and
	     an FPR at other times.  Also, moves between floating-point
	     registers are sometimes cheaper than (D)MTC1 $0.  */
	  if (cost == 1
	      && outer_code == SET
	      && !(float_mode_p && TARGET_HARD_FLOAT))
	    cost = 0;
	  /* When non-MIPS16 code loads a constant N>1 times, we rarely
	     want to CSE the constant itself.  It is usually better to
	     have N copies of the last operation in the sequence and one
	     shared copy of the other operations.  (Note that this is
	     not true for MIPS16 code, where the final operation in the
	     sequence is often an extended instruction.)

	     Also, if we have a CONST_INT, we don't know whether it is
	     for a word or doubleword operation, so we cannot rely on
	     the result of mips_build_integer.  */
	  else if (!TARGET_MIPS16
		   && (outer_code == SET || mode == VOIDmode))
	    cost = 1;
	  *total = COSTS_N_INSNS (cost);
	  return true;
	}
      /* The value will need to be fetched from the constant pool.  */
      *total = CONSTANT_POOL_COST;
      return true;

    case MEM:
      /* If the address is legitimate, return the number of
	 instructions it needs.  */
      addr = XEXP (x, 0);
      cost = mips_address_insns (addr, mode, true);
      if (cost > 0)
	{
	  *total = COSTS_N_INSNS (cost + 1);
	  return true;
	}
      /* Check for a scaled indexed address.  */
      if (mips_lwxs_address_p (addr))
	{
	  *total = COSTS_N_INSNS (2);
	  return true;
	}
      /* Otherwise use the default handling.  */
      return false;

    case FFS:
      *total = COSTS_N_INSNS (6);
      return false;

    case NOT:
      *total = COSTS_N_INSNS (GET_MODE_SIZE (mode) > UNITS_PER_WORD ? 2 : 1);
      return false;

    case AND:
      /* Check for a *clear_upper32 pattern and treat it like a zero
	 extension.  See the pattern's comment for details.  */
      if (TARGET_64BIT
	  && mode == DImode
	  && CONST_INT_P (XEXP (x, 1))
	  && UINTVAL (XEXP (x, 1)) == 0xffffffff)
	{
	  *total = (mips_zero_extend_cost (mode, XEXP (x, 0))
		    + rtx_cost (XEXP (x, 0), SET, speed));
	  return true;
	}
      /* Fall through.  */

    case IOR:
    case XOR:
      /* Double-word operations use two single-word operations.  */
      *total = mips_binary_cost (x, COSTS_N_INSNS (1), COSTS_N_INSNS (2),
				 speed);
      return true;

    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
    case ROTATE:
    case ROTATERT:
      if (CONSTANT_P (XEXP (x, 1)))
	*total = mips_binary_cost (x, COSTS_N_INSNS (1), COSTS_N_INSNS (4),
				   speed);
      else
	*total = mips_binary_cost (x, COSTS_N_INSNS (1), COSTS_N_INSNS (12),
				   speed);
      return true;

    case ABS:
      if (float_mode_p)
        *total = mips_cost->fp_add;
      else
        *total = COSTS_N_INSNS (4);
      return false;

    case LO_SUM:
      /* Low-part immediates need an extended MIPS16 instruction.  */
      *total = (COSTS_N_INSNS (TARGET_MIPS16 ? 2 : 1)
		+ rtx_cost (XEXP (x, 0), SET, speed));
      return true;

    case LT:
    case LTU:
    case LE:
    case LEU:
    case GT:
    case GTU:
    case GE:
    case GEU:
    case EQ:
    case NE:
    case UNORDERED:
    case LTGT:
      /* Branch comparisons have VOIDmode, so use the first operand's
	 mode instead.  */
      mode = GET_MODE (XEXP (x, 0));
      if (FLOAT_MODE_P (mode))
	{
	  *total = mips_cost->fp_add;
	  return false;
	}
      *total = mips_binary_cost (x, COSTS_N_INSNS (1), COSTS_N_INSNS (4),
				 speed);
      return true;

    case MINUS:
      if (float_mode_p
	  && (ISA_HAS_NMADD4_NMSUB4 (mode) || ISA_HAS_NMADD3_NMSUB3 (mode))
	  && TARGET_FUSED_MADD
	  && !HONOR_NANS (mode)
	  && !HONOR_SIGNED_ZEROS (mode))
	{
	  /* See if we can use NMADD or NMSUB.  See mips.md for the
	     associated patterns.  */
	  rtx op0 = XEXP (x, 0);
	  rtx op1 = XEXP (x, 1);
	  if (GET_CODE (op0) == MULT && GET_CODE (XEXP (op0, 0)) == NEG)
	    {
	      *total = (mips_fp_mult_cost (mode)
			+ rtx_cost (XEXP (XEXP (op0, 0), 0), SET, speed)
			+ rtx_cost (XEXP (op0, 1), SET, speed)
			+ rtx_cost (op1, SET, speed));
	      return true;
	    }
	  if (GET_CODE (op1) == MULT)
	    {
	      *total = (mips_fp_mult_cost (mode)
			+ rtx_cost (op0, SET, speed)
			+ rtx_cost (XEXP (op1, 0), SET, speed)
			+ rtx_cost (XEXP (op1, 1), SET, speed));
	      return true;
	    }
	}
      /* Fall through.  */

    case PLUS:
      if (float_mode_p)
	{
	  /* If this is part of a MADD or MSUB, treat the PLUS as
	     being free.  */
	  if (ISA_HAS_FP4
	      && TARGET_FUSED_MADD
	      && GET_CODE (XEXP (x, 0)) == MULT)
	    *total = 0;
	  else
	    *total = mips_cost->fp_add;
	  return false;
	}

      /* Double-word operations require three single-word operations and
	 an SLTU.  The MIPS16 version then needs to move the result of
	 the SLTU from $24 to a MIPS16 register.  */
      *total = mips_binary_cost (x, COSTS_N_INSNS (1),
				 COSTS_N_INSNS (TARGET_MIPS16 ? 5 : 4),
				 speed);
      return true;

    case NEG:
      if (float_mode_p
	  && (ISA_HAS_NMADD4_NMSUB4 (mode) || ISA_HAS_NMADD3_NMSUB3 (mode))
	  && TARGET_FUSED_MADD
	  && !HONOR_NANS (mode)
	  && HONOR_SIGNED_ZEROS (mode))
	{
	  /* See if we can use NMADD or NMSUB.  See mips.md for the
	     associated patterns.  */
	  rtx op = XEXP (x, 0);
	  if ((GET_CODE (op) == PLUS || GET_CODE (op) == MINUS)
	      && GET_CODE (XEXP (op, 0)) == MULT)
	    {
	      *total = (mips_fp_mult_cost (mode)
			+ rtx_cost (XEXP (XEXP (op, 0), 0), SET, speed)
			+ rtx_cost (XEXP (XEXP (op, 0), 1), SET, speed)
			+ rtx_cost (XEXP (op, 1), SET, speed));
	      return true;
	    }
	}

      if (float_mode_p)
	*total = mips_cost->fp_add;
      else
	*total = COSTS_N_INSNS (GET_MODE_SIZE (mode) > UNITS_PER_WORD ? 4 : 1);
      return false;

    case MULT:
      if (float_mode_p)
	*total = mips_fp_mult_cost (mode);
      else if (mode == DImode && !TARGET_64BIT)
	/* Synthesized from 2 mulsi3s, 1 mulsidi3 and two additions,
	   where the mulsidi3 always includes an MFHI and an MFLO.  */
	*total = (speed
		  ? mips_cost->int_mult_si * 3 + 6
		  : COSTS_N_INSNS (ISA_HAS_MUL3 ? 7 : 9));
      else if (!speed)
	*total = (ISA_HAS_MUL3 ? 1 : 2);
      else if (mode == DImode)
	*total = mips_cost->int_mult_di;
      else
	*total = mips_cost->int_mult_si;
      return false;

    case DIV:
      /* Check for a reciprocal.  */
      if (float_mode_p
	  && ISA_HAS_FP4
	  && flag_unsafe_math_optimizations
	  && XEXP (x, 0) == CONST1_RTX (mode))
	{
	  if (outer_code == SQRT || GET_CODE (XEXP (x, 1)) == SQRT)
	    /* An rsqrt<mode>a or rsqrt<mode>b pattern.  Count the
	       division as being free.  */
	    *total = rtx_cost (XEXP (x, 1), SET, speed);
	  else
	    *total = (mips_fp_div_cost (mode)
		      + rtx_cost (XEXP (x, 1), SET, speed));
	  return true;
	}
      /* Fall through.  */

    case SQRT:
    case MOD:
      if (float_mode_p)
	{
	  *total = mips_fp_div_cost (mode);
	  return false;
	}
      /* Fall through.  */

    case UDIV:
    case UMOD:
      if (!speed)
	{
	  /* It is our responsibility to make division by a power of 2
	     as cheap as 2 register additions if we want the division
	     expanders to be used for such operations; see the setting
	     of sdiv_pow2_cheap in optabs.c.  Using (D)DIV for MIPS16
	     should always produce shorter code than using
	     expand_sdiv2_pow2.  */
	  if (TARGET_MIPS16
	      && CONST_INT_P (XEXP (x, 1))
	      && exact_log2 (INTVAL (XEXP (x, 1))) >= 0)
	    {
	      *total = COSTS_N_INSNS (2) + rtx_cost (XEXP (x, 0), SET, speed);
	      return true;
	    }
	  *total = COSTS_N_INSNS (mips_idiv_insns ());
	}
      else if (mode == DImode)
        *total = mips_cost->int_div_di;
      else
	*total = mips_cost->int_div_si;
      return false;

    case SIGN_EXTEND:
      *total = mips_sign_extend_cost (mode, XEXP (x, 0));
      return false;

    case ZERO_EXTEND:
      *total = mips_zero_extend_cost (mode, XEXP (x, 0));
      return false;

    case FLOAT:
    case UNSIGNED_FLOAT:
    case FIX:
    case FLOAT_EXTEND:
    case FLOAT_TRUNCATE:
      *total = mips_cost->fp_add;
      return false;

    default:
      return false;
    }
}

/* Implement TARGET_ADDRESS_COST.  */

static int
mips_address_cost (rtx addr, bool speed ATTRIBUTE_UNUSED)
{
  return mips_address_insns (addr, SImode, false);
}

/* Information about a single instruction in a multi-instruction
   asm sequence.  */
struct mips_multi_member {
  /* True if this is a label, false if it is code.  */
  bool is_label_p;

  /* The output_asm_insn format of the instruction.  */
  const char *format;

  /* The operands to the instruction.  */
  rtx operands[MAX_RECOG_OPERANDS];
};
typedef struct mips_multi_member mips_multi_member;

/* Vector definitions for the above.  */
DEF_VEC_O(mips_multi_member);
DEF_VEC_ALLOC_O(mips_multi_member, heap);

/* The instructions that make up the current multi-insn sequence.  */
static VEC (mips_multi_member, heap) *mips_multi_members;

/* How many instructions (as opposed to labels) are in the current
   multi-insn sequence.  */
static unsigned int mips_multi_num_insns;

/* Start a new multi-insn sequence.  */

static void
mips_multi_start (void)
{
  VEC_truncate (mips_multi_member, mips_multi_members, 0);
  mips_multi_num_insns = 0;
}

/* Add a new, uninitialized member to the current multi-insn sequence.  */

static struct mips_multi_member *
mips_multi_add (void)
{
  return VEC_safe_push (mips_multi_member, heap, mips_multi_members, 0);
}

/* Add a normal insn with the given asm format to the current multi-insn
   sequence.  The other arguments are a null-terminated list of operands.  */

static void
mips_multi_add_insn (const char *format, ...)
{
  struct mips_multi_member *member;
  va_list ap;
  unsigned int i;
  rtx op;

  member = mips_multi_add ();
  member->is_label_p = false;
  member->format = format;
  va_start (ap, format);
  i = 0;
  while ((op = va_arg (ap, rtx)))
    member->operands[i++] = op;
  va_end (ap);
  mips_multi_num_insns++;
}

/* Add the given label definition to the current multi-insn sequence.
   The definition should include the colon.  */

static void
mips_multi_add_label (const char *label)
{
  struct mips_multi_member *member;

  member = mips_multi_add ();
  member->is_label_p = true;
  member->format = label;
}

/* Return the index of the last member of the current multi-insn sequence.  */

static unsigned int
mips_multi_last_index (void)
{
  return VEC_length (mips_multi_member, mips_multi_members) - 1;
}

/* Add a copy of an existing instruction to the current multi-insn
   sequence.  I is the index of the instruction that should be copied.  */

static void
mips_multi_copy_insn (unsigned int i)
{
  struct mips_multi_member *member;

  member = mips_multi_add ();
  memcpy (member, VEC_index (mips_multi_member, mips_multi_members, i),
	  sizeof (*member));
  gcc_assert (!member->is_label_p);
}

/* Change the operand of an existing instruction in the current
   multi-insn sequence.  I is the index of the instruction,
   OP is the index of the operand, and X is the new value.  */

static void
mips_multi_set_operand (unsigned int i, unsigned int op, rtx x)
{
  VEC_index (mips_multi_member, mips_multi_members, i)->operands[op] = x;
}

/* Write out the asm code for the current multi-insn sequence.  */

static void
mips_multi_write (void)
{
  struct mips_multi_member *member;
  unsigned int i;

  FOR_EACH_VEC_ELT (mips_multi_member, mips_multi_members, i, member)
    if (member->is_label_p)
      fprintf (asm_out_file, "%s\n", member->format);
    else
      output_asm_insn (member->format, member->operands);
}

/* Return one word of double-word value OP, taking into account the fixed
   endianness of certain registers.  HIGH_P is true to select the high part,
   false to select the low part.  */

rtx
mips_subword (rtx op, bool high_p)
{
  unsigned int byte, offset;
  enum machine_mode mode;

  mode = GET_MODE (op);
  if (mode == VOIDmode)
    mode = TARGET_64BIT ? TImode : DImode;

  if (TARGET_BIG_ENDIAN ? !high_p : high_p)
    byte = UNITS_PER_WORD;
  else
    byte = 0;

  if (FP_REG_RTX_P (op))
    {
      /* Paired FPRs are always ordered little-endian.  */
      offset = (UNITS_PER_WORD < UNITS_PER_HWFPVALUE ? high_p : byte != 0);
      return gen_rtx_REG (word_mode, REGNO (op) + offset);
    }

  if (MEM_P (op))
    return mips_rewrite_small_data (adjust_address (op, word_mode, byte));

  return simplify_gen_subreg (word_mode, op, mode, byte);
}

/* Return true if a 64-bit move from SRC to DEST should be split into two.  */

bool
mips_split_64bit_move_p (rtx dest, rtx src)
{
  if (TARGET_64BIT)
    return false;

  /* FPR-to-FPR moves can be done in a single instruction, if they're
     allowed at all.  */
  if (FP_REG_RTX_P (src) && FP_REG_RTX_P (dest))
    return false;

  /* Check for floating-point loads and stores.  */
  if (ISA_HAS_LDC1_SDC1)
    {
      if (FP_REG_RTX_P (dest) && MEM_P (src))
	return false;
      if (FP_REG_RTX_P (src) && MEM_P (dest))
	return false;
    }
  return true;
}

/* Split a doubleword move from SRC to DEST.  On 32-bit targets,
   this function handles 64-bit moves for which mips_split_64bit_move_p
   holds.  For 64-bit targets, this function handles 128-bit moves.  */

void
mips_split_doubleword_move (rtx dest, rtx src)
{
  rtx low_dest;

  if (FP_REG_RTX_P (dest) || FP_REG_RTX_P (src))
    {
      if (!TARGET_64BIT && GET_MODE (dest) == DImode)
	emit_insn (gen_move_doubleword_fprdi (dest, src));
      else if (!TARGET_64BIT && GET_MODE (dest) == DFmode)
	emit_insn (gen_move_doubleword_fprdf (dest, src));
      else if (!TARGET_64BIT && GET_MODE (dest) == V2SFmode)
	emit_insn (gen_move_doubleword_fprv2sf (dest, src));
      else if (!TARGET_64BIT && GET_MODE (dest) == V2SImode)
	emit_insn (gen_move_doubleword_fprv2si (dest, src));
      else if (!TARGET_64BIT && GET_MODE (dest) == V4HImode)
	emit_insn (gen_move_doubleword_fprv4hi (dest, src));
      else if (!TARGET_64BIT && GET_MODE (dest) == V8QImode)
	emit_insn (gen_move_doubleword_fprv8qi (dest, src));
      else if (TARGET_64BIT && GET_MODE (dest) == TFmode)
	emit_insn (gen_move_doubleword_fprtf (dest, src));
      else
	gcc_unreachable ();
    }
  else if (REG_P (dest) && REGNO (dest) == MD_REG_FIRST)
    {
      low_dest = mips_subword (dest, false);
      mips_emit_move (low_dest, mips_subword (src, false));
      if (TARGET_64BIT)
	emit_insn (gen_mthidi_ti (dest, mips_subword (src, true), low_dest));
      else
	emit_insn (gen_mthisi_di (dest, mips_subword (src, true), low_dest));
    }
  else if (REG_P (src) && REGNO (src) == MD_REG_FIRST)
    {
      mips_emit_move (mips_subword (dest, false), mips_subword (src, false));
      if (TARGET_64BIT)
	emit_insn (gen_mfhidi_ti (mips_subword (dest, true), src));
      else
	emit_insn (gen_mfhisi_di (mips_subword (dest, true), src));
    }
  else
    {
      /* The operation can be split into two normal moves.  Decide in
	 which order to do them.  */
      low_dest = mips_subword (dest, false);
      if (REG_P (low_dest)
	  && reg_overlap_mentioned_p (low_dest, src))
	{
	  mips_emit_move (mips_subword (dest, true), mips_subword (src, true));
	  mips_emit_move (low_dest, mips_subword (src, false));
	}
      else
	{
	  mips_emit_move (low_dest, mips_subword (src, false));
	  mips_emit_move (mips_subword (dest, true), mips_subword (src, true));
	}
    }
}

/* Return the appropriate instructions to move SRC into DEST.  Assume
   that SRC is operand 1 and DEST is operand 0.  */

const char *
mips_output_move (rtx dest, rtx src)
{
  enum rtx_code dest_code, src_code;
  enum machine_mode mode;
  enum mips_symbol_type symbol_type;
  bool dbl_p;

  dest_code = GET_CODE (dest);
  src_code = GET_CODE (src);
  mode = GET_MODE (dest);
  dbl_p = (GET_MODE_SIZE (mode) == 8);

  if (dbl_p && mips_split_64bit_move_p (dest, src))
    return "#";

  if ((src_code == REG && GP_REG_P (REGNO (src)))
      || (!TARGET_MIPS16 && src == CONST0_RTX (mode)))
    {
      if (dest_code == REG)
	{
	  if (GP_REG_P (REGNO (dest)))
	    return "move\t%0,%z1";

	  /* Moves to HI are handled by special .md insns.  */
	  if (REGNO (dest) == LO_REGNUM)
	    return "mtlo\t%z1";

	  if (DSP_ACC_REG_P (REGNO (dest)))
	    {
	      static char retval[] = "mt__\t%z1,%q0";

	      retval[2] = reg_names[REGNO (dest)][4];
	      retval[3] = reg_names[REGNO (dest)][5];
	      return retval;
	    }

	  if (FP_REG_P (REGNO (dest)))
	    return dbl_p ? "dmtc1\t%z1,%0" : "mtc1\t%z1,%0";

	  if (ALL_COP_REG_P (REGNO (dest)))
	    {
	      static char retval[] = "dmtc_\t%z1,%0";

	      retval[4] = COPNUM_AS_CHAR_FROM_REGNUM (REGNO (dest));
	      return dbl_p ? retval : retval + 1;
	    }
	}
      if (dest_code == MEM)
	switch (GET_MODE_SIZE (mode))
	  {
	  case 1: return "sb\t%z1,%0";
	  case 2: return "sh\t%z1,%0";
	  case 4: return "sw\t%z1,%0";
	  case 8: return "sd\t%z1,%0";
	  }
    }
  if (dest_code == REG && GP_REG_P (REGNO (dest)))
    {
      if (src_code == REG)
	{
	  /* Moves from HI are handled by special .md insns.  */
	  if (REGNO (src) == LO_REGNUM)
	    {
	      /* When generating VR4120 or VR4130 code, we use MACC and
		 DMACC instead of MFLO.  This avoids both the normal
		 MIPS III HI/LO hazards and the errata related to
		 -mfix-vr4130.  */
	      if (ISA_HAS_MACCHI)
		return dbl_p ? "dmacc\t%0,%.,%." : "macc\t%0,%.,%.";
	      return "mflo\t%0";
	    }

	  if (DSP_ACC_REG_P (REGNO (src)))
	    {
	      static char retval[] = "mf__\t%0,%q1";

	      retval[2] = reg_names[REGNO (src)][4];
	      retval[3] = reg_names[REGNO (src)][5];
	      return retval;
	    }

	  if (FP_REG_P (REGNO (src)))
	    return dbl_p ? "dmfc1\t%0,%1" : "mfc1\t%0,%1";

	  if (ALL_COP_REG_P (REGNO (src)))
	    {
	      static char retval[] = "dmfc_\t%0,%1";

	      retval[4] = COPNUM_AS_CHAR_FROM_REGNUM (REGNO (src));
	      return dbl_p ? retval : retval + 1;
	    }

	  if (ST_REG_P (REGNO (src)) && ISA_HAS_8CC)
	    return "lui\t%0,0x3f80\n\tmovf\t%0,%.,%1";
	}

      if (src_code == MEM)
	switch (GET_MODE_SIZE (mode))
	  {
	  case 1: return "lbu\t%0,%1";
	  case 2: return "lhu\t%0,%1";
	  case 4: return "lw\t%0,%1";
	  case 8: return "ld\t%0,%1";
	  }

      if (src_code == CONST_INT)
	{
	  /* Don't use the X format for the operand itself, because that
	     will give out-of-range numbers for 64-bit hosts and 32-bit
	     targets.  */
	  if (!TARGET_MIPS16)
	    return "li\t%0,%1\t\t\t# %X1";

	  if (SMALL_OPERAND_UNSIGNED (INTVAL (src)))
	    return "li\t%0,%1";

	  if (SMALL_OPERAND_UNSIGNED (-INTVAL (src)))
	    return "#";
	}

      if (src_code == HIGH)
	return TARGET_MIPS16 ? "#" : "lui\t%0,%h1";

      if (CONST_GP_P (src))
	return "move\t%0,%1";

      if (mips_symbolic_constant_p (src, SYMBOL_CONTEXT_LEA, &symbol_type)
	  && mips_lo_relocs[symbol_type] != 0)
	{
	  /* A signed 16-bit constant formed by applying a relocation
	     operator to a symbolic address.  */
	  gcc_assert (!mips_split_p[symbol_type]);
	  return "li\t%0,%R1";
	}

      if (symbolic_operand (src, VOIDmode))
	{
	  gcc_assert (TARGET_MIPS16
		      ? TARGET_MIPS16_TEXT_LOADS
		      : !TARGET_EXPLICIT_RELOCS);
	  return dbl_p ? "dla\t%0,%1" : "la\t%0,%1";
	}
    }
  if (src_code == REG && FP_REG_P (REGNO (src)))
    {
      if (dest_code == REG && FP_REG_P (REGNO (dest)))
	{
	  if (GET_MODE (dest) == V2SFmode)
	    return "mov.ps\t%0,%1";
	  else
	    return dbl_p ? "mov.d\t%0,%1" : "mov.s\t%0,%1";
	}

      if (dest_code == MEM)
	return dbl_p ? "sdc1\t%1,%0" : "swc1\t%1,%0";
    }
  if (dest_code == REG && FP_REG_P (REGNO (dest)))
    {
      if (src_code == MEM)
	return dbl_p ? "ldc1\t%0,%1" : "lwc1\t%0,%1";
    }
  if (dest_code == REG && ALL_COP_REG_P (REGNO (dest)) && src_code == MEM)
    {
      static char retval[] = "l_c_\t%0,%1";

      retval[1] = (dbl_p ? 'd' : 'w');
      retval[3] = COPNUM_AS_CHAR_FROM_REGNUM (REGNO (dest));
      return retval;
    }
  if (dest_code == MEM && src_code == REG && ALL_COP_REG_P (REGNO (src)))
    {
      static char retval[] = "s_c_\t%1,%0";

      retval[1] = (dbl_p ? 'd' : 'w');
      retval[3] = COPNUM_AS_CHAR_FROM_REGNUM (REGNO (src));
      return retval;
    }
  gcc_unreachable ();
}

/* Return true if CMP1 is a suitable second operand for integer ordering
   test CODE.  See also the *sCC patterns in mips.md.  */

static bool
mips_int_order_operand_ok_p (enum rtx_code code, rtx cmp1)
{
  switch (code)
    {
    case GT:
    case GTU:
      return reg_or_0_operand (cmp1, VOIDmode);

    case GE:
    case GEU:
      return !TARGET_MIPS16 && cmp1 == const1_rtx;

    case LT:
    case LTU:
      return arith_operand (cmp1, VOIDmode);

    case LE:
      return sle_operand (cmp1, VOIDmode);

    case LEU:
      return sleu_operand (cmp1, VOIDmode);

    default:
      gcc_unreachable ();
    }
}

/* Return true if *CMP1 (of mode MODE) is a valid second operand for
   integer ordering test *CODE, or if an equivalent combination can
   be formed by adjusting *CODE and *CMP1.  When returning true, update
   *CODE and *CMP1 with the chosen code and operand, otherwise leave
   them alone.  */

static bool
mips_canonicalize_int_order_test (enum rtx_code *code, rtx *cmp1,
				  enum machine_mode mode)
{
  HOST_WIDE_INT plus_one;

  if (mips_int_order_operand_ok_p (*code, *cmp1))
    return true;

  if (CONST_INT_P (*cmp1))
    switch (*code)
      {
      case LE:
	plus_one = trunc_int_for_mode (UINTVAL (*cmp1) + 1, mode);
	if (INTVAL (*cmp1) < plus_one)
	  {
	    *code = LT;
	    *cmp1 = force_reg (mode, GEN_INT (plus_one));
	    return true;
	  }
	break;

      case LEU:
	plus_one = trunc_int_for_mode (UINTVAL (*cmp1) + 1, mode);
	if (plus_one != 0)
	  {
	    *code = LTU;
	    *cmp1 = force_reg (mode, GEN_INT (plus_one));
	    return true;
	  }
	break;

      default:
	break;
      }
  return false;
}

/* Compare CMP0 and CMP1 using ordering test CODE and store the result
   in TARGET.  CMP0 and TARGET are register_operands.  If INVERT_PTR
   is nonnull, it's OK to set TARGET to the inverse of the result and
   flip *INVERT_PTR instead.  */

static void
mips_emit_int_order_test (enum rtx_code code, bool *invert_ptr,
			  rtx target, rtx cmp0, rtx cmp1)
{
  enum machine_mode mode;

  /* First see if there is a MIPS instruction that can do this operation.
     If not, try doing the same for the inverse operation.  If that also
     fails, force CMP1 into a register and try again.  */
  mode = GET_MODE (cmp0);
  if (mips_canonicalize_int_order_test (&code, &cmp1, mode))
    mips_emit_binary (code, target, cmp0, cmp1);
  else
    {
      enum rtx_code inv_code = reverse_condition (code);
      if (!mips_canonicalize_int_order_test (&inv_code, &cmp1, mode))
	{
	  cmp1 = force_reg (mode, cmp1);
	  mips_emit_int_order_test (code, invert_ptr, target, cmp0, cmp1);
	}
      else if (invert_ptr == 0)
	{
	  rtx inv_target;

	  inv_target = mips_force_binary (GET_MODE (target),
					  inv_code, cmp0, cmp1);
	  mips_emit_binary (XOR, target, inv_target, const1_rtx);
	}
      else
	{
	  *invert_ptr = !*invert_ptr;
	  mips_emit_binary (inv_code, target, cmp0, cmp1);
	}
    }
}

/* Return a register that is zero iff CMP0 and CMP1 are equal.
   The register will have the same mode as CMP0.  */

static rtx
mips_zero_if_equal (rtx cmp0, rtx cmp1)
{
  if (cmp1 == const0_rtx)
    return cmp0;

  if (uns_arith_operand (cmp1, VOIDmode))
    return expand_binop (GET_MODE (cmp0), xor_optab,
			 cmp0, cmp1, 0, 0, OPTAB_DIRECT);

  return expand_binop (GET_MODE (cmp0), sub_optab,
		       cmp0, cmp1, 0, 0, OPTAB_DIRECT);
}

/* Convert *CODE into a code that can be used in a floating-point
   scc instruction (C.cond.fmt).  Return true if the values of
   the condition code registers will be inverted, with 0 indicating
   that the condition holds.  */

static bool
mips_reversed_fp_cond (enum rtx_code *code)
{
  switch (*code)
    {
    case NE:
    case LTGT:
    case ORDERED:
      *code = reverse_condition_maybe_unordered (*code);
      return true;

    default:
      return false;
    }
}

/* Convert a comparison into something that can be used in a branch or
   conditional move.  On entry, *OP0 and *OP1 are the values being
   compared and *CODE is the code used to compare them.

   Update *CODE, *OP0 and *OP1 so that they describe the final comparison.
   If NEED_EQ_NE_P, then only EQ or NE comparisons against zero are possible,
   otherwise any standard branch condition can be used.  The standard branch
   conditions are:

      - EQ or NE between two registers.
      - any comparison between a register and zero.  */

static void
mips_emit_compare (enum rtx_code *code, rtx *op0, rtx *op1, bool need_eq_ne_p)
{
  rtx cmp_op0 = *op0;
  rtx cmp_op1 = *op1;

  if (GET_MODE_CLASS (GET_MODE (*op0)) == MODE_INT)
    {
      if (!need_eq_ne_p && *op1 == const0_rtx)
	;
      else if (*code == EQ || *code == NE)
	{
	  if (need_eq_ne_p)
	    {
	      *op0 = mips_zero_if_equal (cmp_op0, cmp_op1);
	      *op1 = const0_rtx;
	    }
	  else
	    *op1 = force_reg (GET_MODE (cmp_op0), cmp_op1);
	}
      else
	{
	  /* The comparison needs a separate scc instruction.  Store the
	     result of the scc in *OP0 and compare it against zero.  */
	  bool invert = false;
	  *op0 = gen_reg_rtx (GET_MODE (cmp_op0));
	  mips_emit_int_order_test (*code, &invert, *op0, cmp_op0, cmp_op1);
	  *code = (invert ? EQ : NE);
	  *op1 = const0_rtx;
	}
    }
  else if (ALL_FIXED_POINT_MODE_P (GET_MODE (cmp_op0)))
    {
      *op0 = gen_rtx_REG (CCDSPmode, CCDSP_CC_REGNUM);
      mips_emit_binary (*code, *op0, cmp_op0, cmp_op1);
      *code = NE;
      *op1 = const0_rtx;
    }
  else
    {
      enum rtx_code cmp_code;

      /* Floating-point tests use a separate C.cond.fmt comparison to
	 set a condition code register.  The branch or conditional move
	 will then compare that register against zero.

	 Set CMP_CODE to the code of the comparison instruction and
	 *CODE to the code that the branch or move should use.  */
      cmp_code = *code;
      *code = mips_reversed_fp_cond (&cmp_code) ? EQ : NE;
      *op0 = (ISA_HAS_8CC
	      ? gen_reg_rtx (CCmode)
	      : gen_rtx_REG (CCmode, FPSW_REGNUM));
      *op1 = const0_rtx;
      mips_emit_binary (cmp_code, *op0, cmp_op0, cmp_op1);
    }
}

/* Try performing the comparison in OPERANDS[1], whose arms are OPERANDS[2]
   and OPERAND[3].  Store the result in OPERANDS[0].

   On 64-bit targets, the mode of the comparison and target will always be
   SImode, thus possibly narrower than that of the comparison's operands.  */

void
mips_expand_scc (rtx operands[])
{
  rtx target = operands[0];
  enum rtx_code code = GET_CODE (operands[1]);
  rtx op0 = operands[2];
  rtx op1 = operands[3];

  gcc_assert (GET_MODE_CLASS (GET_MODE (op0)) == MODE_INT);

  if (code == EQ || code == NE)
    {
      if (ISA_HAS_SEQ_SNE
	  && reg_imm10_operand (op1, GET_MODE (op1)))
	mips_emit_binary (code, target, op0, op1);
      else
	{
	  rtx zie = mips_zero_if_equal (op0, op1);
	  mips_emit_binary (code, target, zie, const0_rtx);
	}
    }
  else
    mips_emit_int_order_test (code, 0, target, op0, op1);
}

/* Compare OPERANDS[1] with OPERANDS[2] using comparison code
   CODE and jump to OPERANDS[3] if the condition holds.  */

void
mips_expand_conditional_branch (rtx *operands)
{
  enum rtx_code code = GET_CODE (operands[0]);
  rtx op0 = operands[1];
  rtx op1 = operands[2];
  rtx condition;

  mips_emit_compare (&code, &op0, &op1, TARGET_MIPS16);
  condition = gen_rtx_fmt_ee (code, VOIDmode, op0, op1);
  emit_jump_insn (gen_condjump (condition, operands[3]));
}

/* Implement:

   (set temp (COND:CCV2 CMP_OP0 CMP_OP1))
   (set DEST (unspec [TRUE_SRC FALSE_SRC temp] UNSPEC_MOVE_TF_PS))  */

void
mips_expand_vcondv2sf (rtx dest, rtx true_src, rtx false_src,
		       enum rtx_code cond, rtx cmp_op0, rtx cmp_op1)
{
  rtx cmp_result;
  bool reversed_p;

  reversed_p = mips_reversed_fp_cond (&cond);
  cmp_result = gen_reg_rtx (CCV2mode);
  emit_insn (gen_scc_ps (cmp_result,
			 gen_rtx_fmt_ee (cond, VOIDmode, cmp_op0, cmp_op1)));
  if (reversed_p)
    emit_insn (gen_mips_cond_move_tf_ps (dest, false_src, true_src,
					 cmp_result));
  else
    emit_insn (gen_mips_cond_move_tf_ps (dest, true_src, false_src,
					 cmp_result));
}

/* Perform the comparison in OPERANDS[1].  Move OPERANDS[2] into OPERANDS[0]
   if the condition holds, otherwise move OPERANDS[3] into OPERANDS[0].  */

void
mips_expand_conditional_move (rtx *operands)
{
  rtx cond;
  enum rtx_code code = GET_CODE (operands[1]);
  rtx op0 = XEXP (operands[1], 0);
  rtx op1 = XEXP (operands[1], 1);

  mips_emit_compare (&code, &op0, &op1, true);
  cond = gen_rtx_fmt_ee (code, GET_MODE (op0), op0, op1);
  emit_insn (gen_rtx_SET (VOIDmode, operands[0],
			  gen_rtx_IF_THEN_ELSE (GET_MODE (operands[0]), cond,
						operands[2], operands[3])));
}

/* Perform the comparison in COMPARISON, then trap if the condition holds.  */

void
mips_expand_conditional_trap (rtx comparison)
{
  rtx op0, op1;
  enum machine_mode mode;
  enum rtx_code code;

  /* MIPS conditional trap instructions don't have GT or LE flavors,
     so we must swap the operands and convert to LT and GE respectively.  */
  code = GET_CODE (comparison);
  switch (code)
    {
    case GT:
    case LE:
    case GTU:
    case LEU:
      code = swap_condition (code);
      op0 = XEXP (comparison, 1);
      op1 = XEXP (comparison, 0);
      break;

    default:
      op0 = XEXP (comparison, 0);
      op1 = XEXP (comparison, 1);
      break;
    }

  mode = GET_MODE (XEXP (comparison, 0));
  op0 = force_reg (mode, op0);
  if (!arith_operand (op1, mode))
    op1 = force_reg (mode, op1);

  emit_insn (gen_rtx_TRAP_IF (VOIDmode,
			      gen_rtx_fmt_ee (code, mode, op0, op1),
			      const0_rtx));
}

/* Initialize *CUM for a call to a function of type FNTYPE.  */

void
mips_init_cumulative_args (CUMULATIVE_ARGS *cum, tree fntype)
{
  memset (cum, 0, sizeof (*cum));
  cum->prototype = (fntype && prototype_p (fntype));
  cum->gp_reg_found = (cum->prototype && stdarg_p (fntype));
}

/* Fill INFO with information about a single argument.  CUM is the
   cumulative state for earlier arguments.  MODE is the mode of this
   argument and TYPE is its type (if known).  NAMED is true if this
   is a named (fixed) argument rather than a variable one.  */

static void
mips_get_arg_info (struct mips_arg_info *info, const CUMULATIVE_ARGS *cum,
		   enum machine_mode mode, const_tree type, bool named)
{
  bool doubleword_aligned_p;
  unsigned int num_bytes, num_words, max_regs;

  /* Work out the size of the argument.  */
  num_bytes = type ? int_size_in_bytes (type) : GET_MODE_SIZE (mode);
  num_words = (num_bytes + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

  /* Decide whether it should go in a floating-point register, assuming
     one is free.  Later code checks for availability.

     The checks against UNITS_PER_FPVALUE handle the soft-float and
     single-float cases.  */
  switch (mips_abi)
    {
    case ABI_EABI:
      /* The EABI conventions have traditionally been defined in terms
	 of TYPE_MODE, regardless of the actual type.  */
      info->fpr_p = ((GET_MODE_CLASS (mode) == MODE_FLOAT
		      || GET_MODE_CLASS (mode) == MODE_VECTOR_FLOAT)
		     && GET_MODE_SIZE (mode) <= UNITS_PER_FPVALUE);
      break;

    case ABI_32:
    case ABI_O64:
      /* Only leading floating-point scalars are passed in
	 floating-point registers.  We also handle vector floats the same
	 say, which is OK because they are not covered by the standard ABI.  */
      info->fpr_p = (!cum->gp_reg_found
		     && cum->arg_number < 2
		     && (type == 0
			 || SCALAR_FLOAT_TYPE_P (type)
			 || VECTOR_FLOAT_TYPE_P (type))
		     && (GET_MODE_CLASS (mode) == MODE_FLOAT
			 || GET_MODE_CLASS (mode) == MODE_VECTOR_FLOAT)
		     && GET_MODE_SIZE (mode) <= UNITS_PER_FPVALUE);
      break;

    case ABI_N32:
    case ABI_64:
      /* Scalar, complex and vector floating-point types are passed in
	 floating-point registers, as long as this is a named rather
	 than a variable argument.  */
      info->fpr_p = (named
		     && (type == 0 || FLOAT_TYPE_P (type))
		     && (GET_MODE_CLASS (mode) == MODE_FLOAT
			 || GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT
			 || GET_MODE_CLASS (mode) == MODE_VECTOR_FLOAT)
		     && GET_MODE_UNIT_SIZE (mode) <= UNITS_PER_FPVALUE);

      /* ??? According to the ABI documentation, the real and imaginary
	 parts of complex floats should be passed in individual registers.
	 The real and imaginary parts of stack arguments are supposed
	 to be contiguous and there should be an extra word of padding
	 at the end.

	 This has two problems.  First, it makes it impossible to use a
	 single "void *" va_list type, since register and stack arguments
	 are passed differently.  (At the time of writing, MIPSpro cannot
	 handle complex float varargs correctly.)  Second, it's unclear
	 what should happen when there is only one register free.

	 For now, we assume that named complex floats should go into FPRs
	 if there are two FPRs free, otherwise they should be passed in the
	 same way as a struct containing two floats.  */
      if (info->fpr_p
	  && GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT
	  && GET_MODE_UNIT_SIZE (mode) < UNITS_PER_FPVALUE)
	{
	  if (cum->num_gprs >= MAX_ARGS_IN_REGISTERS - 1)
	    info->fpr_p = false;
	  else
	    num_words = 2;
	}
      break;

    default:
      gcc_unreachable ();
    }

  /* See whether the argument has doubleword alignment.  */
  doubleword_aligned_p = (mips_function_arg_boundary (mode, type)
			  > BITS_PER_WORD);

  /* Set REG_OFFSET to the register count we're interested in.
     The EABI allocates the floating-point registers separately,
     but the other ABIs allocate them like integer registers.  */
  info->reg_offset = (mips_abi == ABI_EABI && info->fpr_p
		      ? cum->num_fprs
		      : cum->num_gprs);

  /* Advance to an even register if the argument is doubleword-aligned.  */
  if (doubleword_aligned_p)
    info->reg_offset += info->reg_offset & 1;

  /* Work out the offset of a stack argument.  */
  info->stack_offset = cum->stack_words;
  if (doubleword_aligned_p)
    info->stack_offset += info->stack_offset & 1;

  max_regs = MAX_ARGS_IN_REGISTERS - info->reg_offset;

  /* Partition the argument between registers and stack.  */
  info->reg_words = MIN (num_words, max_regs);
  info->stack_words = num_words - info->reg_words;
}

/* INFO describes a register argument that has the normal format for the
   argument's mode.  Return the register it uses, assuming that FPRs are
   available if HARD_FLOAT_P.  */

static unsigned int
mips_arg_regno (const struct mips_arg_info *info, bool hard_float_p)
{
  if (!info->fpr_p || !hard_float_p)
    return GP_ARG_FIRST + info->reg_offset;
  else if (mips_abi == ABI_32 && TARGET_DOUBLE_FLOAT && info->reg_offset > 0)
    /* In o32, the second argument is always passed in $f14
       for TARGET_DOUBLE_FLOAT, regardless of whether the
       first argument was a word or doubleword.  */
    return FP_ARG_FIRST + 2;
  else
    return FP_ARG_FIRST + info->reg_offset;
}

/* Implement TARGET_STRICT_ARGUMENT_NAMING.  */

static bool
mips_strict_argument_naming (CUMULATIVE_ARGS *ca ATTRIBUTE_UNUSED)
{
  return !TARGET_OLDABI;
}

/* Implement TARGET_FUNCTION_ARG.  */

static rtx
mips_function_arg (CUMULATIVE_ARGS *cum, enum machine_mode mode,
		   const_tree type, bool named)
{
  struct mips_arg_info info;

  /* We will be called with a mode of VOIDmode after the last argument
     has been seen.  Whatever we return will be passed to the call expander.
     If we need a MIPS16 fp_code, return a REG with the code stored as
     the mode.  */
  if (mode == VOIDmode)
    {
      if (TARGET_MIPS16 && cum->fp_code != 0)
	return gen_rtx_REG ((enum machine_mode) cum->fp_code, 0);
      else
	return NULL;
    }

  mips_get_arg_info (&info, cum, mode, type, named);

  /* Return straight away if the whole argument is passed on the stack.  */
  if (info.reg_offset == MAX_ARGS_IN_REGISTERS)
    return NULL;

  /* The n32 and n64 ABIs say that if any 64-bit chunk of the structure
     contains a double in its entirety, then that 64-bit chunk is passed
     in a floating-point register.  */
  if (TARGET_NEWABI
      && TARGET_HARD_FLOAT
      && named
      && type != 0
      && TREE_CODE (type) == RECORD_TYPE
      && TYPE_SIZE_UNIT (type)
      && host_integerp (TYPE_SIZE_UNIT (type), 1))
    {
      tree field;

      /* First check to see if there is any such field.  */
      for (field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
	if (TREE_CODE (field) == FIELD_DECL
	    && SCALAR_FLOAT_TYPE_P (TREE_TYPE (field))
	    && TYPE_PRECISION (TREE_TYPE (field)) == BITS_PER_WORD
	    && host_integerp (bit_position (field), 0)
	    && int_bit_position (field) % BITS_PER_WORD == 0)
	  break;

      if (field != 0)
	{
	  /* Now handle the special case by returning a PARALLEL
	     indicating where each 64-bit chunk goes.  INFO.REG_WORDS
	     chunks are passed in registers.  */
	  unsigned int i;
	  HOST_WIDE_INT bitpos;
	  rtx ret;

	  /* assign_parms checks the mode of ENTRY_PARM, so we must
	     use the actual mode here.  */
	  ret = gen_rtx_PARALLEL (mode, rtvec_alloc (info.reg_words));

	  bitpos = 0;
	  field = TYPE_FIELDS (type);
	  for (i = 0; i < info.reg_words; i++)
	    {
	      rtx reg;

	      for (; field; field = DECL_CHAIN (field))
		if (TREE_CODE (field) == FIELD_DECL
		    && int_bit_position (field) >= bitpos)
		  break;

	      if (field
		  && int_bit_position (field) == bitpos
		  && SCALAR_FLOAT_TYPE_P (TREE_TYPE (field))
		  && TYPE_PRECISION (TREE_TYPE (field)) == BITS_PER_WORD)
		reg = gen_rtx_REG (DFmode, FP_ARG_FIRST + info.reg_offset + i);
	      else
		reg = gen_rtx_REG (DImode, GP_ARG_FIRST + info.reg_offset + i);

	      XVECEXP (ret, 0, i)
		= gen_rtx_EXPR_LIST (VOIDmode, reg,
				     GEN_INT (bitpos / BITS_PER_UNIT));

	      bitpos += BITS_PER_WORD;
	    }
	  return ret;
	}
    }

  /* Handle the n32/n64 conventions for passing complex floating-point
     arguments in FPR pairs.  The real part goes in the lower register
     and the imaginary part goes in the upper register.  */
  if (TARGET_NEWABI
      && info.fpr_p
      && GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT)
    {
      rtx real, imag;
      enum machine_mode inner;
      unsigned int regno;

      inner = GET_MODE_INNER (mode);
      regno = FP_ARG_FIRST + info.reg_offset;
      if (info.reg_words * UNITS_PER_WORD == GET_MODE_SIZE (inner))
	{
	  /* Real part in registers, imaginary part on stack.  */
	  gcc_assert (info.stack_words == info.reg_words);
	  return gen_rtx_REG (inner, regno);
	}
      else
	{
	  gcc_assert (info.stack_words == 0);
	  real = gen_rtx_EXPR_LIST (VOIDmode,
				    gen_rtx_REG (inner, regno),
				    const0_rtx);
	  imag = gen_rtx_EXPR_LIST (VOIDmode,
				    gen_rtx_REG (inner,
						 regno + info.reg_words / 2),
				    GEN_INT (GET_MODE_SIZE (inner)));
	  return gen_rtx_PARALLEL (mode, gen_rtvec (2, real, imag));
	}
    }

  return gen_rtx_REG (mode, mips_arg_regno (&info, TARGET_HARD_FLOAT));
}

/* Implement TARGET_FUNCTION_ARG_ADVANCE.  */

static void
mips_function_arg_advance (CUMULATIVE_ARGS *cum, enum machine_mode mode,
			   const_tree type, bool named)
{
  struct mips_arg_info info;

  mips_get_arg_info (&info, cum, mode, type, named);

  if (!info.fpr_p)
    cum->gp_reg_found = true;

  /* See the comment above the CUMULATIVE_ARGS structure in mips.h for
     an explanation of what this code does.  It assumes that we're using
     either the o32 or the o64 ABI, both of which pass at most 2 arguments
     in FPRs.  */
  if (cum->arg_number < 2 && info.fpr_p)
    cum->fp_code += (mode == SFmode ? 1 : 2) << (cum->arg_number * 2);

  /* Advance the register count.  This has the effect of setting
     num_gprs to MAX_ARGS_IN_REGISTERS if a doubleword-aligned
     argument required us to skip the final GPR and pass the whole
     argument on the stack.  */
  if (mips_abi != ABI_EABI || !info.fpr_p)
    cum->num_gprs = info.reg_offset + info.reg_words;
  else if (info.reg_words > 0)
    cum->num_fprs += MAX_FPRS_PER_FMT;

  /* Advance the stack word count.  */
  if (info.stack_words > 0)
    cum->stack_words = info.stack_offset + info.stack_words;

  cum->arg_number++;
}

/* Implement TARGET_ARG_PARTIAL_BYTES.  */

static int
mips_arg_partial_bytes (CUMULATIVE_ARGS *cum,
			enum machine_mode mode, tree type, bool named)
{
  struct mips_arg_info info;

  mips_get_arg_info (&info, cum, mode, type, named);
  return info.stack_words > 0 ? info.reg_words * UNITS_PER_WORD : 0;
}

/* Implement TARGET_FUNCTION_ARG_BOUNDARY.  Every parameter gets at
   least PARM_BOUNDARY bits of alignment, but will be given anything up
   to STACK_BOUNDARY bits if the type requires it.  */

static unsigned int
mips_function_arg_boundary (enum machine_mode mode, const_tree type)
{
  unsigned int alignment;

  alignment = type ? TYPE_ALIGN (type) : GET_MODE_ALIGNMENT (mode);
  if (alignment < PARM_BOUNDARY)
    alignment = PARM_BOUNDARY;
  if (alignment > STACK_BOUNDARY)
    alignment = STACK_BOUNDARY;
  return alignment;
}

/* Return true if FUNCTION_ARG_PADDING (MODE, TYPE) should return
   upward rather than downward.  In other words, return true if the
   first byte of the stack slot has useful data, false if the last
   byte does.  */

bool
mips_pad_arg_upward (enum machine_mode mode, const_tree type)
{
  /* On little-endian targets, the first byte of every stack argument
     is passed in the first byte of the stack slot.  */
  if (!BYTES_BIG_ENDIAN)
    return true;

  /* Otherwise, integral types are padded downward: the last byte of a
     stack argument is passed in the last byte of the stack slot.  */
  if (type != 0
      ? (INTEGRAL_TYPE_P (type)
	 || POINTER_TYPE_P (type)
	 || FIXED_POINT_TYPE_P (type))
      : (SCALAR_INT_MODE_P (mode)
	 || ALL_SCALAR_FIXED_POINT_MODE_P (mode)))
    return false;

  /* Big-endian o64 pads floating-point arguments downward.  */
  if (mips_abi == ABI_O64)
    if (type != 0 ? FLOAT_TYPE_P (type) : GET_MODE_CLASS (mode) == MODE_FLOAT)
      return false;

  /* Other types are padded upward for o32, o64, n32 and n64.  */
  if (mips_abi != ABI_EABI)
    return true;

  /* Arguments smaller than a stack slot are padded downward.  */
  if (mode != BLKmode)
    return GET_MODE_BITSIZE (mode) >= PARM_BOUNDARY;
  else
    return int_size_in_bytes (type) >= (PARM_BOUNDARY / BITS_PER_UNIT);
}

/* Likewise BLOCK_REG_PADDING (MODE, TYPE, ...).  Return !BYTES_BIG_ENDIAN
   if the least significant byte of the register has useful data.  Return
   the opposite if the most significant byte does.  */

bool
mips_pad_reg_upward (enum machine_mode mode, tree type)
{
  /* No shifting is required for floating-point arguments.  */
  if (type != 0 ? FLOAT_TYPE_P (type) : GET_MODE_CLASS (mode) == MODE_FLOAT)
    return !BYTES_BIG_ENDIAN;

  /* Otherwise, apply the same padding to register arguments as we do
     to stack arguments.  */
  return mips_pad_arg_upward (mode, type);
}

/* Return nonzero when an argument must be passed by reference.  */

static bool
mips_pass_by_reference (CUMULATIVE_ARGS *cum ATTRIBUTE_UNUSED,
			enum machine_mode mode, const_tree type,
			bool named ATTRIBUTE_UNUSED)
{
  if (mips_abi == ABI_EABI)
    {
      int size;

      /* ??? How should SCmode be handled?  */
      if (mode == DImode || mode == DFmode
	  || mode == DQmode || mode == UDQmode
	  || mode == DAmode || mode == UDAmode)
	return 0;

      size = type ? int_size_in_bytes (type) : GET_MODE_SIZE (mode);
      return size == -1 || size > UNITS_PER_WORD;
    }
  else
    {
      /* If we have a variable-sized parameter, we have no choice.  */
      return targetm.calls.must_pass_in_stack (mode, type);
    }
}

/* Implement TARGET_CALLEE_COPIES.  */

static bool
mips_callee_copies (CUMULATIVE_ARGS *cum ATTRIBUTE_UNUSED,
		    enum machine_mode mode ATTRIBUTE_UNUSED,
		    const_tree type ATTRIBUTE_UNUSED, bool named)
{
  return mips_abi == ABI_EABI && named;
}

/* See whether VALTYPE is a record whose fields should be returned in
   floating-point registers.  If so, return the number of fields and
   list them in FIELDS (which should have two elements).  Return 0
   otherwise.

   For n32 & n64, a structure with one or two fields is returned in
   floating-point registers as long as every field has a floating-point
   type.  */

static int
mips_fpr_return_fields (const_tree valtype, tree *fields)
{
  tree field;
  int i;

  if (!TARGET_NEWABI)
    return 0;

  if (TREE_CODE (valtype) != RECORD_TYPE)
    return 0;

  i = 0;
  for (field = TYPE_FIELDS (valtype); field != 0; field = DECL_CHAIN (field))
    {
      if (TREE_CODE (field) != FIELD_DECL)
	continue;

      if (!SCALAR_FLOAT_TYPE_P (TREE_TYPE (field)))
	return 0;

      if (i == 2)
	return 0;

      fields[i++] = field;
    }
  return i;
}

/* Implement TARGET_RETURN_IN_MSB.  For n32 & n64, we should return
   a value in the most significant part of $2/$3 if:

      - the target is big-endian;

      - the value has a structure or union type (we generalize this to
	cover aggregates from other languages too); and

      - the structure is not returned in floating-point registers.  */

static bool
mips_return_in_msb (const_tree valtype)
{
  tree fields[2];

  return (TARGET_NEWABI
	  && TARGET_BIG_ENDIAN
	  && AGGREGATE_TYPE_P (valtype)
	  && mips_fpr_return_fields (valtype, fields) == 0);
}

/* Return true if the function return value MODE will get returned in a
   floating-point register.  */

static bool
mips_return_mode_in_fpr_p (enum machine_mode mode)
{
  return ((GET_MODE_CLASS (mode) == MODE_FLOAT
	   || GET_MODE_CLASS (mode) == MODE_VECTOR_FLOAT
	   || GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT)
	  && GET_MODE_UNIT_SIZE (mode) <= UNITS_PER_HWFPVALUE);
}

/* Return the representation of an FPR return register when the
   value being returned in FP_RETURN has mode VALUE_MODE and the
   return type itself has mode TYPE_MODE.  On NewABI targets,
   the two modes may be different for structures like:

       struct __attribute__((packed)) foo { float f; }

   where we return the SFmode value of "f" in FP_RETURN, but where
   the structure itself has mode BLKmode.  */

static rtx
mips_return_fpr_single (enum machine_mode type_mode,
			enum machine_mode value_mode)
{
  rtx x;

  x = gen_rtx_REG (value_mode, FP_RETURN);
  if (type_mode != value_mode)
    {
      x = gen_rtx_EXPR_LIST (VOIDmode, x, const0_rtx);
      x = gen_rtx_PARALLEL (type_mode, gen_rtvec (1, x));
    }
  return x;
}

/* Return a composite value in a pair of floating-point registers.
   MODE1 and OFFSET1 are the mode and byte offset for the first value,
   likewise MODE2 and OFFSET2 for the second.  MODE is the mode of the
   complete value.

   For n32 & n64, $f0 always holds the first value and $f2 the second.
   Otherwise the values are packed together as closely as possible.  */

static rtx
mips_return_fpr_pair (enum machine_mode mode,
		      enum machine_mode mode1, HOST_WIDE_INT offset1,
		      enum machine_mode mode2, HOST_WIDE_INT offset2)
{
  int inc;

  inc = (TARGET_NEWABI ? 2 : MAX_FPRS_PER_FMT);
  return gen_rtx_PARALLEL
    (mode,
     gen_rtvec (2,
		gen_rtx_EXPR_LIST (VOIDmode,
				   gen_rtx_REG (mode1, FP_RETURN),
				   GEN_INT (offset1)),
		gen_rtx_EXPR_LIST (VOIDmode,
				   gen_rtx_REG (mode2, FP_RETURN + inc),
				   GEN_INT (offset2))));

}

/* Implement FUNCTION_VALUE and LIBCALL_VALUE.  For normal calls,
   VALTYPE is the return type and MODE is VOIDmode.  For libcalls,
   VALTYPE is null and MODE is the mode of the return value.  */

rtx
mips_function_value (const_tree valtype, const_tree func, enum machine_mode mode)
{
  if (valtype)
    {
      tree fields[2];
      int unsigned_p;

      mode = TYPE_MODE (valtype);
      unsigned_p = TYPE_UNSIGNED (valtype);

      /* Since TARGET_PROMOTE_FUNCTION_MODE unconditionally promotes,
	 return values, promote the mode here too.  */
      mode = promote_function_mode (valtype, mode, &unsigned_p, func, 1);

      /* Handle structures whose fields are returned in $f0/$f2.  */
      switch (mips_fpr_return_fields (valtype, fields))
	{
	case 1:
	  return mips_return_fpr_single (mode,
					 TYPE_MODE (TREE_TYPE (fields[0])));

	case 2:
	  return mips_return_fpr_pair (mode,
				       TYPE_MODE (TREE_TYPE (fields[0])),
				       int_byte_position (fields[0]),
				       TYPE_MODE (TREE_TYPE (fields[1])),
				       int_byte_position (fields[1]));
	}

      /* If a value is passed in the most significant part of a register, see
	 whether we have to round the mode up to a whole number of words.  */
      if (mips_return_in_msb (valtype))
	{
	  HOST_WIDE_INT size = int_size_in_bytes (valtype);
	  if (size % UNITS_PER_WORD != 0)
	    {
	      size += UNITS_PER_WORD - size % UNITS_PER_WORD;
	      mode = mode_for_size (size * BITS_PER_UNIT, MODE_INT, 0);
	    }
	}

      /* For EABI, the class of return register depends entirely on MODE.
	 For example, "struct { some_type x; }" and "union { some_type x; }"
	 are returned in the same way as a bare "some_type" would be.
	 Other ABIs only use FPRs for scalar, complex or vector types.  */
      if (mips_abi != ABI_EABI && !FLOAT_TYPE_P (valtype))
	return gen_rtx_REG (mode, GP_RETURN);
    }

  if (!TARGET_MIPS16)
    {
      /* Handle long doubles for n32 & n64.  */
      if (mode == TFmode)
	return mips_return_fpr_pair (mode,
				     DImode, 0,
				     DImode, GET_MODE_SIZE (mode) / 2);

      if (mips_return_mode_in_fpr_p (mode))
	{
	  if (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT)
	    return mips_return_fpr_pair (mode,
					 GET_MODE_INNER (mode), 0,
					 GET_MODE_INNER (mode),
					 GET_MODE_SIZE (mode) / 2);
	  else
	    return gen_rtx_REG (mode, FP_RETURN);
	}
    }

  return gen_rtx_REG (mode, GP_RETURN);
}

/* Implement TARGET_RETURN_IN_MEMORY.  Under the o32 and o64 ABIs,
   all BLKmode objects are returned in memory.  Under the n32, n64
   and embedded ABIs, small structures are returned in a register.
   Objects with varying size must still be returned in memory, of
   course.  */

static bool
mips_return_in_memory (const_tree type, const_tree fndecl ATTRIBUTE_UNUSED)
{
  return (TARGET_OLDABI
	  ? TYPE_MODE (type) == BLKmode
	  : !IN_RANGE (int_size_in_bytes (type), 0, 2 * UNITS_PER_WORD));
}

/* Implement TARGET_SETUP_INCOMING_VARARGS.  */

static void
mips_setup_incoming_varargs (CUMULATIVE_ARGS *cum, enum machine_mode mode,
			     tree type, int *pretend_size ATTRIBUTE_UNUSED,
			     int no_rtl)
{
  CUMULATIVE_ARGS local_cum;
  int gp_saved, fp_saved;

  /* The caller has advanced CUM up to, but not beyond, the last named
     argument.  Advance a local copy of CUM past the last "real" named
     argument, to find out how many registers are left over.  */
  local_cum = *cum;
  mips_function_arg_advance (&local_cum, mode, type, true);

  /* Found out how many registers we need to save.  */
  gp_saved = MAX_ARGS_IN_REGISTERS - local_cum.num_gprs;
  fp_saved = (EABI_FLOAT_VARARGS_P
	      ? MAX_ARGS_IN_REGISTERS - local_cum.num_fprs
	      : 0);

  if (!no_rtl)
    {
      if (gp_saved > 0)
	{
	  rtx ptr, mem;

	  ptr = plus_constant (virtual_incoming_args_rtx,
			       REG_PARM_STACK_SPACE (cfun->decl)
			       - gp_saved * UNITS_PER_WORD);
	  mem = gen_frame_mem (BLKmode, ptr);
	  set_mem_alias_set (mem, get_varargs_alias_set ());

	  move_block_from_reg (local_cum.num_gprs + GP_ARG_FIRST,
			       mem, gp_saved);
	}
      if (fp_saved > 0)
	{
	  /* We can't use move_block_from_reg, because it will use
	     the wrong mode.  */
	  enum machine_mode mode;
	  int off, i;

	  /* Set OFF to the offset from virtual_incoming_args_rtx of
	     the first float register.  The FP save area lies below
	     the integer one, and is aligned to UNITS_PER_FPVALUE bytes.  */
	  off = (-gp_saved * UNITS_PER_WORD) & -UNITS_PER_FPVALUE;
	  off -= fp_saved * UNITS_PER_FPREG;

	  mode = TARGET_SINGLE_FLOAT ? SFmode : DFmode;

	  for (i = local_cum.num_fprs; i < MAX_ARGS_IN_REGISTERS;
	       i += MAX_FPRS_PER_FMT)
	    {
	      rtx ptr, mem;

	      ptr = plus_constant (virtual_incoming_args_rtx, off);
	      mem = gen_frame_mem (mode, ptr);
	      set_mem_alias_set (mem, get_varargs_alias_set ());
	      mips_emit_move (mem, gen_rtx_REG (mode, FP_ARG_FIRST + i));
	      off += UNITS_PER_HWFPVALUE;
	    }
	}
    }
  if (REG_PARM_STACK_SPACE (cfun->decl) == 0)
    cfun->machine->varargs_size = (gp_saved * UNITS_PER_WORD
				   + fp_saved * UNITS_PER_FPREG);
}

/* Implement TARGET_BUILTIN_VA_LIST.  */

static tree
mips_build_builtin_va_list (void)
{
  if (EABI_FLOAT_VARARGS_P)
    {
      /* We keep 3 pointers, and two offsets.

	 Two pointers are to the overflow area, which starts at the CFA.
	 One of these is constant, for addressing into the GPR save area
	 below it.  The other is advanced up the stack through the
	 overflow region.

	 The third pointer is to the bottom of the GPR save area.
	 Since the FPR save area is just below it, we can address
	 FPR slots off this pointer.

	 We also keep two one-byte offsets, which are to be subtracted
	 from the constant pointers to yield addresses in the GPR and
	 FPR save areas.  These are downcounted as float or non-float
	 arguments are used, and when they get to zero, the argument
	 must be obtained from the overflow region.  */
      tree f_ovfl, f_gtop, f_ftop, f_goff, f_foff, f_res, record;
      tree array, index;

      record = lang_hooks.types.make_type (RECORD_TYPE);

      f_ovfl = build_decl (BUILTINS_LOCATION,
			   FIELD_DECL, get_identifier ("__overflow_argptr"),
			   ptr_type_node);
      f_gtop = build_decl (BUILTINS_LOCATION,
			   FIELD_DECL, get_identifier ("__gpr_top"),
			   ptr_type_node);
      f_ftop = build_decl (BUILTINS_LOCATION,
			   FIELD_DECL, get_identifier ("__fpr_top"),
			   ptr_type_node);
      f_goff = build_decl (BUILTINS_LOCATION,
			   FIELD_DECL, get_identifier ("__gpr_offset"),
			   unsigned_char_type_node);
      f_foff = build_decl (BUILTINS_LOCATION,
			   FIELD_DECL, get_identifier ("__fpr_offset"),
			   unsigned_char_type_node);
      /* Explicitly pad to the size of a pointer, so that -Wpadded won't
	 warn on every user file.  */
      index = build_int_cst (NULL_TREE, GET_MODE_SIZE (ptr_mode) - 2 - 1);
      array = build_array_type (unsigned_char_type_node,
			        build_index_type (index));
      f_res = build_decl (BUILTINS_LOCATION,
			  FIELD_DECL, get_identifier ("__reserved"), array);

      DECL_FIELD_CONTEXT (f_ovfl) = record;
      DECL_FIELD_CONTEXT (f_gtop) = record;
      DECL_FIELD_CONTEXT (f_ftop) = record;
      DECL_FIELD_CONTEXT (f_goff) = record;
      DECL_FIELD_CONTEXT (f_foff) = record;
      DECL_FIELD_CONTEXT (f_res) = record;

      TYPE_FIELDS (record) = f_ovfl;
      DECL_CHAIN (f_ovfl) = f_gtop;
      DECL_CHAIN (f_gtop) = f_ftop;
      DECL_CHAIN (f_ftop) = f_goff;
      DECL_CHAIN (f_goff) = f_foff;
      DECL_CHAIN (f_foff) = f_res;

      layout_type (record);
      return record;
    }
  else if (TARGET_IRIX6)
    /* On IRIX 6, this type is 'char *'.  */
    return build_pointer_type (char_type_node);
  else
    /* Otherwise, we use 'void *'.  */
    return ptr_type_node;
}

/* Implement TARGET_EXPAND_BUILTIN_VA_START.  */

static void
mips_va_start (tree valist, rtx nextarg)
{
  if (EABI_FLOAT_VARARGS_P)
    {
      const CUMULATIVE_ARGS *cum;
      tree f_ovfl, f_gtop, f_ftop, f_goff, f_foff;
      tree ovfl, gtop, ftop, goff, foff;
      tree t;
      int gpr_save_area_size;
      int fpr_save_area_size;
      int fpr_offset;

      cum = &crtl->args.info;
      gpr_save_area_size
	= (MAX_ARGS_IN_REGISTERS - cum->num_gprs) * UNITS_PER_WORD;
      fpr_save_area_size
	= (MAX_ARGS_IN_REGISTERS - cum->num_fprs) * UNITS_PER_FPREG;

      f_ovfl = TYPE_FIELDS (va_list_type_node);
      f_gtop = DECL_CHAIN (f_ovfl);
      f_ftop = DECL_CHAIN (f_gtop);
      f_goff = DECL_CHAIN (f_ftop);
      f_foff = DECL_CHAIN (f_goff);

      ovfl = build3 (COMPONENT_REF, TREE_TYPE (f_ovfl), valist, f_ovfl,
		     NULL_TREE);
      gtop = build3 (COMPONENT_REF, TREE_TYPE (f_gtop), valist, f_gtop,
		     NULL_TREE);
      ftop = build3 (COMPONENT_REF, TREE_TYPE (f_ftop), valist, f_ftop,
		     NULL_TREE);
      goff = build3 (COMPONENT_REF, TREE_TYPE (f_goff), valist, f_goff,
		     NULL_TREE);
      foff = build3 (COMPONENT_REF, TREE_TYPE (f_foff), valist, f_foff,
		     NULL_TREE);

      /* Emit code to initialize OVFL, which points to the next varargs
	 stack argument.  CUM->STACK_WORDS gives the number of stack
	 words used by named arguments.  */
      t = make_tree (TREE_TYPE (ovfl), virtual_incoming_args_rtx);
      if (cum->stack_words > 0)
	t = build2 (POINTER_PLUS_EXPR, TREE_TYPE (ovfl), t,
		    size_int (cum->stack_words * UNITS_PER_WORD));
      t = build2 (MODIFY_EXPR, TREE_TYPE (ovfl), ovfl, t);
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

      /* Emit code to initialize GTOP, the top of the GPR save area.  */
      t = make_tree (TREE_TYPE (gtop), virtual_incoming_args_rtx);
      t = build2 (MODIFY_EXPR, TREE_TYPE (gtop), gtop, t);
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

      /* Emit code to initialize FTOP, the top of the FPR save area.
	 This address is gpr_save_area_bytes below GTOP, rounded
	 down to the next fp-aligned boundary.  */
      t = make_tree (TREE_TYPE (ftop), virtual_incoming_args_rtx);
      fpr_offset = gpr_save_area_size + UNITS_PER_FPVALUE - 1;
      fpr_offset &= -UNITS_PER_FPVALUE;
      if (fpr_offset)
	t = build2 (POINTER_PLUS_EXPR, TREE_TYPE (ftop), t,
		    size_int (-fpr_offset));
      t = build2 (MODIFY_EXPR, TREE_TYPE (ftop), ftop, t);
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

      /* Emit code to initialize GOFF, the offset from GTOP of the
	 next GPR argument.  */
      t = build2 (MODIFY_EXPR, TREE_TYPE (goff), goff,
		  build_int_cst (TREE_TYPE (goff), gpr_save_area_size));
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

      /* Likewise emit code to initialize FOFF, the offset from FTOP
	 of the next FPR argument.  */
      t = build2 (MODIFY_EXPR, TREE_TYPE (foff), foff,
		  build_int_cst (TREE_TYPE (foff), fpr_save_area_size));
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
    }
  else
    {
      nextarg = plus_constant (nextarg, -cfun->machine->varargs_size);
      std_expand_builtin_va_start (valist, nextarg);
    }
}

/* Implement TARGET_GIMPLIFY_VA_ARG_EXPR.  */

static tree
mips_gimplify_va_arg_expr (tree valist, tree type, gimple_seq *pre_p,
			   gimple_seq *post_p)
{
  tree addr;
  bool indirect_p;

  indirect_p = pass_by_reference (NULL, TYPE_MODE (type), type, 0);
  if (indirect_p)
    type = build_pointer_type (type);

  if (!EABI_FLOAT_VARARGS_P)
    addr = std_gimplify_va_arg_expr (valist, type, pre_p, post_p);
  else
    {
      tree f_ovfl, f_gtop, f_ftop, f_goff, f_foff;
      tree ovfl, top, off, align;
      HOST_WIDE_INT size, rsize, osize;
      tree t, u;

      f_ovfl = TYPE_FIELDS (va_list_type_node);
      f_gtop = DECL_CHAIN (f_ovfl);
      f_ftop = DECL_CHAIN (f_gtop);
      f_goff = DECL_CHAIN (f_ftop);
      f_foff = DECL_CHAIN (f_goff);

      /* Let:

	 TOP be the top of the GPR or FPR save area;
	 OFF be the offset from TOP of the next register;
	 ADDR_RTX be the address of the argument;
	 SIZE be the number of bytes in the argument type;
	 RSIZE be the number of bytes used to store the argument
	   when it's in the register save area; and
	 OSIZE be the number of bytes used to store it when it's
	   in the stack overflow area.

	 The code we want is:

	 1: off &= -rsize;	  // round down
	 2: if (off != 0)
	 3:   {
	 4:	addr_rtx = top - off + (BYTES_BIG_ENDIAN ? RSIZE - SIZE : 0);
	 5:	off -= rsize;
	 6:   }
	 7: else
	 8:   {
	 9:	ovfl = ((intptr_t) ovfl + osize - 1) & -osize;
	 10:	addr_rtx = ovfl + (BYTES_BIG_ENDIAN ? OSIZE - SIZE : 0);
	 11:	ovfl += osize;
	 14:  }

	 [1] and [9] can sometimes be optimized away.  */

      ovfl = build3 (COMPONENT_REF, TREE_TYPE (f_ovfl), valist, f_ovfl,
		     NULL_TREE);
      size = int_size_in_bytes (type);

      if (GET_MODE_CLASS (TYPE_MODE (type)) == MODE_FLOAT
	  && GET_MODE_SIZE (TYPE_MODE (type)) <= UNITS_PER_FPVALUE)
	{
	  top = build3 (COMPONENT_REF, TREE_TYPE (f_ftop),
			unshare_expr (valist), f_ftop, NULL_TREE);
	  off = build3 (COMPONENT_REF, TREE_TYPE (f_foff),
			unshare_expr (valist), f_foff, NULL_TREE);

	  /* When va_start saves FPR arguments to the stack, each slot
	     takes up UNITS_PER_HWFPVALUE bytes, regardless of the
	     argument's precision.  */
	  rsize = UNITS_PER_HWFPVALUE;

	  /* Overflow arguments are padded to UNITS_PER_WORD bytes
	     (= PARM_BOUNDARY bits).  This can be different from RSIZE
	     in two cases:

	     (1) On 32-bit targets when TYPE is a structure such as:

	     struct s { float f; };

	     Such structures are passed in paired FPRs, so RSIZE
	     will be 8 bytes.  However, the structure only takes
	     up 4 bytes of memory, so OSIZE will only be 4.

	     (2) In combinations such as -mgp64 -msingle-float
	     -fshort-double.  Doubles passed in registers will then take
	     up 4 (UNITS_PER_HWFPVALUE) bytes, but those passed on the
	     stack take up UNITS_PER_WORD bytes.  */
	  osize = MAX (GET_MODE_SIZE (TYPE_MODE (type)), UNITS_PER_WORD);
	}
      else
	{
	  top = build3 (COMPONENT_REF, TREE_TYPE (f_gtop),
			unshare_expr (valist), f_gtop, NULL_TREE);
	  off = build3 (COMPONENT_REF, TREE_TYPE (f_goff),
			unshare_expr (valist), f_goff, NULL_TREE);
	  rsize = (size + UNITS_PER_WORD - 1) & -UNITS_PER_WORD;
	  if (rsize > UNITS_PER_WORD)
	    {
	      /* [1] Emit code for: off &= -rsize.	*/
	      t = build2 (BIT_AND_EXPR, TREE_TYPE (off), unshare_expr (off),
			  build_int_cst (TREE_TYPE (off), -rsize));
	      gimplify_assign (unshare_expr (off), t, pre_p);
	    }
	  osize = rsize;
	}

      /* [2] Emit code to branch if off == 0.  */
      t = build2 (NE_EXPR, boolean_type_node, off,
		  build_int_cst (TREE_TYPE (off), 0));
      addr = build3 (COND_EXPR, ptr_type_node, t, NULL_TREE, NULL_TREE);

      /* [5] Emit code for: off -= rsize.  We do this as a form of
	 post-decrement not available to C.  */
      t = fold_convert (TREE_TYPE (off), build_int_cst (NULL_TREE, rsize));
      t = build2 (POSTDECREMENT_EXPR, TREE_TYPE (off), off, t);

      /* [4] Emit code for:
	 addr_rtx = top - off + (BYTES_BIG_ENDIAN ? RSIZE - SIZE : 0).  */
      t = fold_convert (sizetype, t);
      t = fold_build1 (NEGATE_EXPR, sizetype, t);
      t = build2 (POINTER_PLUS_EXPR, TREE_TYPE (top), top, t);
      if (BYTES_BIG_ENDIAN && rsize > size)
	{
	  u = size_int (rsize - size);
	  t = build2 (POINTER_PLUS_EXPR, TREE_TYPE (t), t, u);
	}
      COND_EXPR_THEN (addr) = t;

      if (osize > UNITS_PER_WORD)
	{
	  /* [9] Emit: ovfl = ((intptr_t) ovfl + osize - 1) & -osize.  */
	  u = size_int (osize - 1);
	  t = build2 (POINTER_PLUS_EXPR, TREE_TYPE (ovfl),
		      unshare_expr (ovfl), u);
	  t = fold_convert (sizetype, t);
	  u = size_int (-osize);
	  t = build2 (BIT_AND_EXPR, sizetype, t, u);
	  t = fold_convert (TREE_TYPE (ovfl), t);
	  align = build2 (MODIFY_EXPR, TREE_TYPE (ovfl),
			  unshare_expr (ovfl), t);
	}
      else
	align = NULL;

      /* [10, 11] Emit code for:
	 addr_rtx = ovfl + (BYTES_BIG_ENDIAN ? OSIZE - SIZE : 0)
	 ovfl += osize.  */
      u = fold_convert (TREE_TYPE (ovfl), build_int_cst (NULL_TREE, osize));
      t = build2 (POSTINCREMENT_EXPR, TREE_TYPE (ovfl), ovfl, u);
      if (BYTES_BIG_ENDIAN && osize > size)
	{
	  u = size_int (osize - size);
	  t = build2 (POINTER_PLUS_EXPR, TREE_TYPE (t), t, u);
	}

      /* String [9] and [10, 11] together.  */
      if (align)
	t = build2 (COMPOUND_EXPR, TREE_TYPE (t), align, t);
      COND_EXPR_ELSE (addr) = t;

      addr = fold_convert (build_pointer_type (type), addr);
      addr = build_va_arg_indirect_ref (addr);
    }

  if (indirect_p)
    addr = build_va_arg_indirect_ref (addr);

  return addr;
}

/* Start a definition of function NAME.  MIPS16_P indicates whether the
   function contains MIPS16 code.  */

static void
mips_start_function_definition (const char *name, bool mips16_p)
{
  if (mips16_p)
    fprintf (asm_out_file, "\t.set\tmips16\n");
  else
    fprintf (asm_out_file, "\t.set\tnomips16\n");

  if (!flag_inhibit_size_directive)
    {
      fputs ("\t.ent\t", asm_out_file);
      assemble_name (asm_out_file, name);
      fputs ("\n", asm_out_file);
    }

  ASM_OUTPUT_TYPE_DIRECTIVE (asm_out_file, name, "function");

  /* Start the definition proper.  */
  assemble_name (asm_out_file, name);
  fputs (":\n", asm_out_file);
}

/* End a function definition started by mips_start_function_definition.  */

static void
mips_end_function_definition (const char *name)
{
  if (!flag_inhibit_size_directive)
    {
      fputs ("\t.end\t", asm_out_file);
      assemble_name (asm_out_file, name);
      fputs ("\n", asm_out_file);
    }
}

/* Return true if calls to X can use R_MIPS_CALL* relocations.  */

static bool
mips_ok_for_lazy_binding_p (rtx x)
{
  return (TARGET_USE_GOT
	  && GET_CODE (x) == SYMBOL_REF
	  && !SYMBOL_REF_BIND_NOW_P (x)
	  && !mips_symbol_binds_local_p (x));
}

/* Load function address ADDR into register DEST.  TYPE is as for
   mips_expand_call.  Return true if we used an explicit lazy-binding
   sequence.  */

static bool
mips_load_call_address (enum mips_call_type type, rtx dest, rtx addr)
{
  /* If we're generating PIC, and this call is to a global function,
     try to allow its address to be resolved lazily.  This isn't
     possible for sibcalls when $gp is call-saved because the value
     of $gp on entry to the stub would be our caller's gp, not ours.  */
  if (TARGET_EXPLICIT_RELOCS
      && !(type == MIPS_CALL_SIBCALL && TARGET_CALL_SAVED_GP)
      && mips_ok_for_lazy_binding_p (addr))
    {
      addr = mips_got_load (dest, addr, SYMBOL_GOTOFF_CALL);
      emit_insn (gen_rtx_SET (VOIDmode, dest, addr));
      return true;
    }
  else
    {
      mips_emit_move (dest, addr);
      return false;
    }
}

/* Each locally-defined hard-float MIPS16 function has a local symbol
   associated with it.  This hash table maps the function symbol (FUNC)
   to the local symbol (LOCAL). */
struct GTY(()) mips16_local_alias {
  rtx func;
  rtx local;
};
static GTY ((param_is (struct mips16_local_alias))) htab_t mips16_local_aliases;

/* Hash table callbacks for mips16_local_aliases.  */

static hashval_t
mips16_local_aliases_hash (const void *entry)
{
  const struct mips16_local_alias *alias;

  alias = (const struct mips16_local_alias *) entry;
  return htab_hash_string (XSTR (alias->func, 0));
}

static int
mips16_local_aliases_eq (const void *entry1, const void *entry2)
{
  const struct mips16_local_alias *alias1, *alias2;

  alias1 = (const struct mips16_local_alias *) entry1;
  alias2 = (const struct mips16_local_alias *) entry2;
  return rtx_equal_p (alias1->func, alias2->func);
}

/* FUNC is the symbol for a locally-defined hard-float MIPS16 function.
   Return a local alias for it, creating a new one if necessary.  */

static rtx
mips16_local_alias (rtx func)
{
  struct mips16_local_alias *alias, tmp_alias;
  void **slot;

  /* Create the hash table if this is the first call.  */
  if (mips16_local_aliases == NULL)
    mips16_local_aliases = htab_create_ggc (37, mips16_local_aliases_hash,
					    mips16_local_aliases_eq, NULL);

  /* Look up the function symbol, creating a new entry if need be.  */
  tmp_alias.func = func;
  slot = htab_find_slot (mips16_local_aliases, &tmp_alias, INSERT);
  gcc_assert (slot != NULL);

  alias = (struct mips16_local_alias *) *slot;
  if (alias == NULL)
    {
      const char *func_name, *local_name;
      rtx local;

      /* Create a new SYMBOL_REF for the local symbol.  The choice of
	 __fn_local_* is based on the __fn_stub_* names that we've
	 traditionally used for the non-MIPS16 stub.  */
      func_name = targetm.strip_name_encoding (XSTR (func, 0));
      local_name = ACONCAT (("__fn_local_", func_name, NULL));
      local = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (local_name));
      SYMBOL_REF_FLAGS (local) = SYMBOL_REF_FLAGS (func) | SYMBOL_FLAG_LOCAL;

      /* Create a new structure to represent the mapping.  */
      alias = ggc_alloc_mips16_local_alias ();
      alias->func = func;
      alias->local = local;
      *slot = alias;
    }
  return alias->local;
}

/* A chained list of functions for which mips16_build_call_stub has already
   generated a stub.  NAME is the name of the function and FP_RET_P is true
   if the function returns a value in floating-point registers.  */
struct mips16_stub {
  struct mips16_stub *next;
  char *name;
  bool fp_ret_p;
};
static struct mips16_stub *mips16_stubs;

/* Return a SYMBOL_REF for a MIPS16 function called NAME.  */

static rtx
mips16_stub_function (const char *name)
{
  rtx x;

  x = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (name));
  SYMBOL_REF_FLAGS (x) |= (SYMBOL_FLAG_EXTERNAL | SYMBOL_FLAG_FUNCTION);
  return x;
}

/* Return the two-character string that identifies floating-point
   return mode MODE in the name of a MIPS16 function stub.  */

static const char *
mips16_call_stub_mode_suffix (enum machine_mode mode)
{
  if (mode == SFmode)
    return "sf";
  else if (mode == DFmode)
    return "df";
  else if (mode == SCmode)
    return "sc";
  else if (mode == DCmode)
    return "dc";
  else if (mode == V2SFmode)
    return "df";
  else
    gcc_unreachable ();
}

/* Write instructions to move a 32-bit value between general register
   GPREG and floating-point register FPREG.  DIRECTION is 't' to move
   from GPREG to FPREG and 'f' to move in the opposite direction.  */

static void
mips_output_32bit_xfer (char direction, unsigned int gpreg, unsigned int fpreg)
{
  fprintf (asm_out_file, "\tm%cc1\t%s,%s\n", direction,
	   reg_names[gpreg], reg_names[fpreg]);
}

/* Likewise for 64-bit values.  */

static void
mips_output_64bit_xfer (char direction, unsigned int gpreg, unsigned int fpreg)
{
  if (TARGET_64BIT)
    fprintf (asm_out_file, "\tdm%cc1\t%s,%s\n", direction,
 	     reg_names[gpreg], reg_names[fpreg]);
  else if (TARGET_FLOAT64)
    {
      fprintf (asm_out_file, "\tm%cc1\t%s,%s\n", direction,
 	       reg_names[gpreg + TARGET_BIG_ENDIAN], reg_names[fpreg]);
      fprintf (asm_out_file, "\tm%chc1\t%s,%s\n", direction,
 	       reg_names[gpreg + TARGET_LITTLE_ENDIAN], reg_names[fpreg]);
    }
  else
    {
      /* Move the least-significant word.  */
      fprintf (asm_out_file, "\tm%cc1\t%s,%s\n", direction,
	       reg_names[gpreg + TARGET_BIG_ENDIAN], reg_names[fpreg]);
      /* ...then the most significant word.  */
      fprintf (asm_out_file, "\tm%cc1\t%s,%s\n", direction,
	       reg_names[gpreg + TARGET_LITTLE_ENDIAN], reg_names[fpreg + 1]);
    }
}

/* Write out code to move floating-point arguments into or out of
   general registers.  FP_CODE is the code describing which arguments
   are present (see the comment above the definition of CUMULATIVE_ARGS
   in mips.h).  DIRECTION is as for mips_output_32bit_xfer.  */

static void
mips_output_args_xfer (int fp_code, char direction)
{
  unsigned int gparg, fparg, f;
  CUMULATIVE_ARGS cum;

  /* This code only works for o32 and o64.  */
  gcc_assert (TARGET_OLDABI);

  mips_init_cumulative_args (&cum, NULL);

  for (f = (unsigned int) fp_code; f != 0; f >>= 2)
    {
      enum machine_mode mode;
      struct mips_arg_info info;

      if ((f & 3) == 1)
	mode = SFmode;
      else if ((f & 3) == 2)
	mode = DFmode;
      else
	gcc_unreachable ();

      mips_get_arg_info (&info, &cum, mode, NULL, true);
      gparg = mips_arg_regno (&info, false);
      fparg = mips_arg_regno (&info, true);

      if (mode == SFmode)
	mips_output_32bit_xfer (direction, gparg, fparg);
      else
	mips_output_64bit_xfer (direction, gparg, fparg);

      mips_function_arg_advance (&cum, mode, NULL, true);
    }
}

/* Write a MIPS16 stub for the current function.  This stub is used
   for functions which take arguments in the floating-point registers.
   It is normal-mode code that moves the floating-point arguments
   into the general registers and then jumps to the MIPS16 code.  */

static void
mips16_build_function_stub (void)
{
  const char *fnname, *alias_name, *separator;
  char *secname, *stubname;
  tree stubdecl;
  unsigned int f;
  rtx symbol, alias;

  /* Create the name of the stub, and its unique section.  */
  symbol = XEXP (DECL_RTL (current_function_decl), 0);
  alias = mips16_local_alias (symbol);

  fnname = targetm.strip_name_encoding (XSTR (symbol, 0));
  alias_name = targetm.strip_name_encoding (XSTR (alias, 0));
  secname = ACONCAT ((".mips16.fn.", fnname, NULL));
  stubname = ACONCAT (("__fn_stub_", fnname, NULL));

  /* Build a decl for the stub.  */
  stubdecl = build_decl (BUILTINS_LOCATION,
			 FUNCTION_DECL, get_identifier (stubname),
			 build_function_type (void_type_node, NULL_TREE));
  DECL_SECTION_NAME (stubdecl) = build_string (strlen (secname), secname);
  DECL_RESULT (stubdecl) = build_decl (BUILTINS_LOCATION,
				       RESULT_DECL, NULL_TREE, void_type_node);

  /* Output a comment.  */
  fprintf (asm_out_file, "\t# Stub function for %s (",
	   current_function_name ());
  separator = "";
  for (f = (unsigned int) crtl->args.info.fp_code; f != 0; f >>= 2)
    {
      fprintf (asm_out_file, "%s%s", separator,
	       (f & 3) == 1 ? "float" : "double");
      separator = ", ";
    }
  fprintf (asm_out_file, ")\n");

  /* Start the function definition.  */
  assemble_start_function (stubdecl, stubname);
  mips_start_function_definition (stubname, false);

  /* If generating pic2 code, either set up the global pointer or
     switch to pic0.  */
  if (TARGET_ABICALLS_PIC2)
    {
      if (TARGET_ABSOLUTE_ABICALLS)
	fprintf (asm_out_file, "\t.option\tpic0\n");
      else
	{
	  output_asm_insn ("%(.cpload\t%^%)", NULL);
	  /* Emit an R_MIPS_NONE relocation to tell the linker what the
	     target function is.  Use a local GOT access when loading the
	     symbol, to cut down on the number of unnecessary GOT entries
	     for stubs that aren't needed.  */
	  output_asm_insn (".reloc\t0,R_MIPS_NONE,%0", &symbol);
	  symbol = alias;
	}
    }

  /* Load the address of the MIPS16 function into $25.  Do this first so
     that targets with coprocessor interlocks can use an MFC1 to fill the
     delay slot.  */
  output_asm_insn ("la\t%^,%0", &symbol);

  /* Move the arguments from floating-point registers to general registers.  */
  mips_output_args_xfer (crtl->args.info.fp_code, 'f');

  /* Jump to the MIPS16 function.  */
  output_asm_insn ("jr\t%^", NULL);

  if (TARGET_ABICALLS_PIC2 && TARGET_ABSOLUTE_ABICALLS)
    fprintf (asm_out_file, "\t.option\tpic2\n");

  mips_end_function_definition (stubname);

  /* If the linker needs to create a dynamic symbol for the target
     function, it will associate the symbol with the stub (which,
     unlike the target function, follows the proper calling conventions).
     It is therefore useful to have a local alias for the target function,
     so that it can still be identified as MIPS16 code.  As an optimization,
     this symbol can also be used for indirect MIPS16 references from
     within this file.  */
  ASM_OUTPUT_DEF (asm_out_file, alias_name, fnname);

  switch_to_section (function_section (current_function_decl));
}

/* The current function is a MIPS16 function that returns a value in an FPR.
   Copy the return value from its soft-float to its hard-float location.
   libgcc2 has special non-MIPS16 helper functions for each case.  */

static void
mips16_copy_fpr_return_value (void)
{
  rtx fn, insn, retval;
  tree return_type;
  enum machine_mode return_mode;
  const char *name;

  return_type = DECL_RESULT (current_function_decl);
  return_mode = DECL_MODE (return_type);

  name = ACONCAT (("__mips16_ret_",
		   mips16_call_stub_mode_suffix (return_mode),
		   NULL));
  fn = mips16_stub_function (name);

  /* The function takes arguments in $2 (and possibly $3), so calls
     to it cannot be lazily bound.  */
  SYMBOL_REF_FLAGS (fn) |= SYMBOL_FLAG_BIND_NOW;

  /* Model the call as something that takes the GPR return value as
     argument and returns an "updated" value.  */
  retval = gen_rtx_REG (return_mode, GP_RETURN);
  insn = mips_expand_call (MIPS_CALL_EPILOGUE, retval, fn,
			   const0_rtx, NULL_RTX, false);
  use_reg (&CALL_INSN_FUNCTION_USAGE (insn), retval);
}

/* Consider building a stub for a MIPS16 call to function *FN_PTR.
   RETVAL is the location of the return value, or null if this is
   a "call" rather than a "call_value".  ARGS_SIZE is the size of the
   arguments and FP_CODE is the code built by mips_function_arg;
   see the comment before the fp_code field in CUMULATIVE_ARGS for details.

   There are three alternatives:

   - If a stub was needed, emit the call and return the call insn itself.

   - If we can avoid using a stub by redirecting the call, set *FN_PTR
     to the new target and return null.

   - If *FN_PTR doesn't need a stub, return null and leave *FN_PTR
     unmodified.

   A stub is needed for calls to functions that, in normal mode,
   receive arguments in FPRs or return values in FPRs.  The stub
   copies the arguments from their soft-float positions to their
   hard-float positions, calls the real function, then copies the
   return value from its hard-float position to its soft-float
   position.

   We can emit a JAL to *FN_PTR even when *FN_PTR might need a stub.
   If *FN_PTR turns out to be to a non-MIPS16 function, the linker
   automatically redirects the JAL to the stub, otherwise the JAL
   continues to call FN directly.  */

static rtx
mips16_build_call_stub (rtx retval, rtx *fn_ptr, rtx args_size, int fp_code)
{
  const char *fnname;
  bool fp_ret_p;
  struct mips16_stub *l;
  rtx insn, fn;

  /* We don't need to do anything if we aren't in MIPS16 mode, or if
     we were invoked with the -msoft-float option.  */
  if (!TARGET_MIPS16 || TARGET_SOFT_FLOAT_ABI)
    return NULL_RTX;

  /* Figure out whether the value might come back in a floating-point
     register.  */
  fp_ret_p = retval && mips_return_mode_in_fpr_p (GET_MODE (retval));

  /* We don't need to do anything if there were no floating-point
     arguments and the value will not be returned in a floating-point
     register.  */
  if (fp_code == 0 && !fp_ret_p)
    return NULL_RTX;

  /* We don't need to do anything if this is a call to a special
     MIPS16 support function.  */
  fn = *fn_ptr;
  if (mips16_stub_function_p (fn))
    return NULL_RTX;

  /* This code will only work for o32 and o64 abis.  The other ABI's
     require more sophisticated support.  */
  gcc_assert (TARGET_OLDABI);

  /* If we're calling via a function pointer, use one of the magic
     libgcc.a stubs provided for each (FP_CODE, FP_RET_P) combination.
     Each stub expects the function address to arrive in register $2.  */
  if (GET_CODE (fn) != SYMBOL_REF
      || !call_insn_operand (fn, VOIDmode))
    {
      char buf[30];
      rtx stub_fn, insn, addr;
      bool lazy_p;

      /* If this is a locally-defined and locally-binding function,
	 avoid the stub by calling the local alias directly.  */
      if (mips16_local_function_p (fn))
	{
	  *fn_ptr = mips16_local_alias (fn);
	  return NULL_RTX;
	}

      /* Create a SYMBOL_REF for the libgcc.a function.  */
      if (fp_ret_p)
	sprintf (buf, "__mips16_call_stub_%s_%d",
		 mips16_call_stub_mode_suffix (GET_MODE (retval)),
		 fp_code);
      else
	sprintf (buf, "__mips16_call_stub_%d", fp_code);
      stub_fn = mips16_stub_function (buf);

      /* The function uses $2 as an argument, so calls to it
	 cannot be lazily bound.  */
      SYMBOL_REF_FLAGS (stub_fn) |= SYMBOL_FLAG_BIND_NOW;

      /* Load the target function into $2.  */
      addr = gen_rtx_REG (Pmode, GP_REG_FIRST + 2);
      lazy_p = mips_load_call_address (MIPS_CALL_NORMAL, addr, fn);

      /* Emit the call.  */
      insn = mips_expand_call (MIPS_CALL_NORMAL, retval, stub_fn,
			       args_size, NULL_RTX, lazy_p);

      /* Tell GCC that this call does indeed use the value of $2.  */
      use_reg (&CALL_INSN_FUNCTION_USAGE (insn), addr);

      /* If we are handling a floating-point return value, we need to
         save $18 in the function prologue.  Putting a note on the
         call will mean that df_regs_ever_live_p ($18) will be true if the
         call is not eliminated, and we can check that in the prologue
         code.  */
      if (fp_ret_p)
	CALL_INSN_FUNCTION_USAGE (insn) =
	  gen_rtx_EXPR_LIST (VOIDmode,
			     gen_rtx_CLOBBER (VOIDmode,
					      gen_rtx_REG (word_mode, 18)),
			     CALL_INSN_FUNCTION_USAGE (insn));

      return insn;
    }

  /* We know the function we are going to call.  If we have already
     built a stub, we don't need to do anything further.  */
  fnname = targetm.strip_name_encoding (XSTR (fn, 0));
  for (l = mips16_stubs; l != NULL; l = l->next)
    if (strcmp (l->name, fnname) == 0)
      break;

  if (l == NULL)
    {
      const char *separator;
      char *secname, *stubname;
      tree stubid, stubdecl;
      unsigned int f;

      /* If the function does not return in FPRs, the special stub
	 section is named
	     .mips16.call.FNNAME

	 If the function does return in FPRs, the stub section is named
	     .mips16.call.fp.FNNAME

	 Build a decl for the stub.  */
      secname = ACONCAT ((".mips16.call.", fp_ret_p ? "fp." : "",
			  fnname, NULL));
      stubname = ACONCAT (("__call_stub_", fp_ret_p ? "fp_" : "",
			   fnname, NULL));
      stubid = get_identifier (stubname);
      stubdecl = build_decl (BUILTINS_LOCATION,
			     FUNCTION_DECL, stubid,
			     build_function_type (void_type_node, NULL_TREE));
      DECL_SECTION_NAME (stubdecl) = build_string (strlen (secname), secname);
      DECL_RESULT (stubdecl) = build_decl (BUILTINS_LOCATION,
					   RESULT_DECL, NULL_TREE,
					   void_type_node);

      /* Output a comment.  */
      fprintf (asm_out_file, "\t# Stub function to call %s%s (",
	       (fp_ret_p
		? (GET_MODE (retval) == SFmode ? "float " : "double ")
		: ""),
	       fnname);
      separator = "";
      for (f = (unsigned int) fp_code; f != 0; f >>= 2)
	{
	  fprintf (asm_out_file, "%s%s", separator,
		   (f & 3) == 1 ? "float" : "double");
	  separator = ", ";
	}
      fprintf (asm_out_file, ")\n");

      /* Start the function definition.  */
      assemble_start_function (stubdecl, stubname);
      mips_start_function_definition (stubname, false);

      if (!fp_ret_p)
	{
	  /* Load the address of the MIPS16 function into $25.  Do this
	     first so that targets with coprocessor interlocks can use
	     an MFC1 to fill the delay slot.  */
	  if (TARGET_EXPLICIT_RELOCS)
	    {
	      output_asm_insn ("lui\t%^,%%hi(%0)", &fn);
	      output_asm_insn ("addiu\t%^,%^,%%lo(%0)", &fn);
	    }
	  else
	    output_asm_insn ("la\t%^,%0", &fn);
	}

      /* Move the arguments from general registers to floating-point
	 registers.  */
      mips_output_args_xfer (fp_code, 't');

      if (!fp_ret_p)
	{
	  /* Jump to the previously-loaded address.  */
	  output_asm_insn ("jr\t%^", NULL);
	}
      else
	{
	  /* Save the return address in $18 and call the non-MIPS16 function.
	     The stub's caller knows that $18 might be clobbered, even though
	     $18 is usually a call-saved register.  */
	  fprintf (asm_out_file, "\tmove\t%s,%s\n",
		   reg_names[GP_REG_FIRST + 18], reg_names[RETURN_ADDR_REGNUM]);
	  output_asm_insn (MIPS_CALL ("jal", &fn, 0, -1), &fn);

	  /* Move the result from floating-point registers to
	     general registers.  */
	  switch (GET_MODE (retval))
	    {
	    case SCmode:
	      mips_output_32bit_xfer ('f', GP_RETURN + TARGET_BIG_ENDIAN,
				      TARGET_BIG_ENDIAN
				      ? FP_REG_FIRST + MAX_FPRS_PER_FMT
				      : FP_REG_FIRST);
	      mips_output_32bit_xfer ('f', GP_RETURN + TARGET_LITTLE_ENDIAN,
				      TARGET_LITTLE_ENDIAN
				      ? FP_REG_FIRST + MAX_FPRS_PER_FMT
				      : FP_REG_FIRST);
	      if (GET_MODE (retval) == SCmode && TARGET_64BIT)
		{
		  /* On 64-bit targets, complex floats are returned in
		     a single GPR, such that "sd" on a suitably-aligned
		     target would store the value correctly.  */
		  fprintf (asm_out_file, "\tdsll\t%s,%s,32\n",
			   reg_names[GP_RETURN + TARGET_BIG_ENDIAN],
			   reg_names[GP_RETURN + TARGET_BIG_ENDIAN]);
		  fprintf (asm_out_file, "\tdsll\t%s,%s,32\n",
			   reg_names[GP_RETURN + TARGET_LITTLE_ENDIAN],
			   reg_names[GP_RETURN + TARGET_LITTLE_ENDIAN]);
		  fprintf (asm_out_file, "\tdsrl\t%s,%s,32\n",
			   reg_names[GP_RETURN + TARGET_BIG_ENDIAN],
			   reg_names[GP_RETURN + TARGET_BIG_ENDIAN]);
		  fprintf (asm_out_file, "\tor\t%s,%s,%s\n",
			   reg_names[GP_RETURN],
			   reg_names[GP_RETURN],
			   reg_names[GP_RETURN + 1]);
		}
	      break;

	    case SFmode:
	      mips_output_32bit_xfer ('f', GP_RETURN, FP_REG_FIRST);
	      break;

	    case DCmode:
	      mips_output_64bit_xfer ('f', GP_RETURN + (8 / UNITS_PER_WORD),
				      FP_REG_FIRST + MAX_FPRS_PER_FMT);
	      /* Fall though.  */
 	    case DFmode:
	    case V2SFmode:
	      mips_output_64bit_xfer ('f', GP_RETURN, FP_REG_FIRST);
	      break;

	    default:
	      gcc_unreachable ();
	    }
	  fprintf (asm_out_file, "\tjr\t%s\n", reg_names[GP_REG_FIRST + 18]);
	}

#ifdef ASM_DECLARE_FUNCTION_SIZE
      ASM_DECLARE_FUNCTION_SIZE (asm_out_file, stubname, stubdecl);
#endif

      mips_end_function_definition (stubname);

      /* Record this stub.  */
      l = XNEW (struct mips16_stub);
      l->name = xstrdup (fnname);
      l->fp_ret_p = fp_ret_p;
      l->next = mips16_stubs;
      mips16_stubs = l;
    }

  /* If we expect a floating-point return value, but we've built a
     stub which does not expect one, then we're in trouble.  We can't
     use the existing stub, because it won't handle the floating-point
     value.  We can't build a new stub, because the linker won't know
     which stub to use for the various calls in this object file.
     Fortunately, this case is illegal, since it means that a function
     was declared in two different ways in a single compilation.  */
  if (fp_ret_p && !l->fp_ret_p)
    error ("cannot handle inconsistent calls to %qs", fnname);

  if (retval == NULL_RTX)
    insn = gen_call_internal_direct (fn, args_size);
  else
    insn = gen_call_value_internal_direct (retval, fn, args_size);
  insn = mips_emit_call_insn (insn, fn, fn, false);

  /* If we are calling a stub which handles a floating-point return
     value, we need to arrange to save $18 in the prologue.  We do this
     by marking the function call as using the register.  The prologue
     will later see that it is used, and emit code to save it.  */
  if (fp_ret_p)
    CALL_INSN_FUNCTION_USAGE (insn) =
      gen_rtx_EXPR_LIST (VOIDmode,
			 gen_rtx_CLOBBER (VOIDmode,
					  gen_rtx_REG (word_mode, 18)),
			 CALL_INSN_FUNCTION_USAGE (insn));

  return insn;
}

/* Expand a call of type TYPE.  RESULT is where the result will go (null
   for "call"s and "sibcall"s), ADDR is the address of the function,
   ARGS_SIZE is the size of the arguments and AUX is the value passed
   to us by mips_function_arg.  LAZY_P is true if this call already
   involves a lazily-bound function address (such as when calling
   functions through a MIPS16 hard-float stub).

   Return the call itself.  */

rtx
mips_expand_call (enum mips_call_type type, rtx result, rtx addr,
		  rtx args_size, rtx aux, bool lazy_p)
{
  rtx orig_addr, pattern, insn;
  int fp_code;

  fp_code = aux == 0 ? 0 : (int) GET_MODE (aux);
  insn = mips16_build_call_stub (result, &addr, args_size, fp_code);
  if (insn)
    {
      gcc_assert (!lazy_p && type == MIPS_CALL_NORMAL);
      return insn;
    }
				 ;
  orig_addr = addr;
  if (!call_insn_operand (addr, VOIDmode))
    {
      if (type == MIPS_CALL_EPILOGUE)
	addr = MIPS_EPILOGUE_TEMP (Pmode);
      else
	addr = gen_reg_rtx (Pmode);
      lazy_p |= mips_load_call_address (type, addr, orig_addr);
    }

  if (result == 0)
    {
      rtx (*fn) (rtx, rtx);

      if (type == MIPS_CALL_SIBCALL)
	fn = gen_sibcall_internal;
      else
	fn = gen_call_internal;

      pattern = fn (addr, args_size);
    }
  else if (GET_CODE (result) == PARALLEL && XVECLEN (result, 0) == 2)
    {
      /* Handle return values created by mips_return_fpr_pair.  */
      rtx (*fn) (rtx, rtx, rtx, rtx);
      rtx reg1, reg2;

      if (type == MIPS_CALL_SIBCALL)
	fn = gen_sibcall_value_multiple_internal;
      else
	fn = gen_call_value_multiple_internal;

      reg1 = XEXP (XVECEXP (result, 0, 0), 0);
      reg2 = XEXP (XVECEXP (result, 0, 1), 0);
      pattern = fn (reg1, addr, args_size, reg2);
    }
  else
    {
      rtx (*fn) (rtx, rtx, rtx);

      if (type == MIPS_CALL_SIBCALL)
	fn = gen_sibcall_value_internal;
      else
	fn = gen_call_value_internal;

      /* Handle return values created by mips_return_fpr_single.  */
      if (GET_CODE (result) == PARALLEL && XVECLEN (result, 0) == 1)
	result = XEXP (XVECEXP (result, 0, 0), 0);
      pattern = fn (result, addr, args_size);
    }

  return mips_emit_call_insn (pattern, orig_addr, addr, lazy_p);
}

/* Split call instruction INSN into a $gp-clobbering call and
   (where necessary) an instruction to restore $gp from its save slot.
   CALL_PATTERN is the pattern of the new call.  */

void
mips_split_call (rtx insn, rtx call_pattern)
{
  emit_call_insn (call_pattern);
  if (!find_reg_note (insn, REG_NORETURN, 0))
    /* Pick a temporary register that is suitable for both MIPS16 and
       non-MIPS16 code.  $4 and $5 are used for returning complex double
       values in soft-float code, so $6 is the first suitable candidate.  */
    mips_restore_gp_from_cprestore_slot (gen_rtx_REG (Pmode, GP_ARG_FIRST + 2));
}

/* Implement TARGET_FUNCTION_OK_FOR_SIBCALL.  */

static bool
mips_function_ok_for_sibcall (tree decl, tree exp ATTRIBUTE_UNUSED)
{
  if (!TARGET_SIBCALLS)
    return false;

  /* Interrupt handlers need special epilogue code and therefore can't
     use sibcalls.  */
  if (mips_interrupt_type_p (TREE_TYPE (current_function_decl)))
    return false;

  /* We can't do a sibcall if the called function is a MIPS16 function
     because there is no direct "jx" instruction equivalent to "jalx" to
     switch the ISA mode.  We only care about cases where the sibling
     and normal calls would both be direct.  */
  if (decl
      && mips_use_mips16_mode_p (decl)
      && const_call_insn_operand (XEXP (DECL_RTL (decl), 0), VOIDmode))
    return false;

  /* When -minterlink-mips16 is in effect, assume that non-locally-binding
     functions could be MIPS16 ones unless an attribute explicitly tells
     us otherwise.  */
  if (TARGET_INTERLINK_MIPS16
      && decl
      && (DECL_EXTERNAL (decl) || !targetm.binds_local_p (decl))
      && !mips_nomips16_decl_p (decl)
      && const_call_insn_operand (XEXP (DECL_RTL (decl), 0), VOIDmode))
    return false;

  /* Otherwise OK.  */
  return true;
}

/* Emit code to move general operand SRC into condition-code
   register DEST given that SCRATCH is a scratch TFmode FPR.
   The sequence is:

	FP1 = SRC
	FP2 = 0.0f
	DEST = FP2 < FP1

   where FP1 and FP2 are single-precision FPRs taken from SCRATCH.  */

void
mips_expand_fcc_reload (rtx dest, rtx src, rtx scratch)
{
  rtx fp1, fp2;

  /* Change the source to SFmode.  */
  if (MEM_P (src))
    src = adjust_address (src, SFmode, 0);
  else if (REG_P (src) || GET_CODE (src) == SUBREG)
    src = gen_rtx_REG (SFmode, true_regnum (src));

  fp1 = gen_rtx_REG (SFmode, REGNO (scratch));
  fp2 = gen_rtx_REG (SFmode, REGNO (scratch) + MAX_FPRS_PER_FMT);

  mips_emit_move (copy_rtx (fp1), src);
  mips_emit_move (copy_rtx (fp2), CONST0_RTX (SFmode));
  emit_insn (gen_slt_sf (dest, fp2, fp1));
}

/* Emit straight-line code to move LENGTH bytes from SRC to DEST.
   Assume that the areas do not overlap.  */

static void
mips_block_move_straight (rtx dest, rtx src, HOST_WIDE_INT length)
{
  HOST_WIDE_INT offset, delta;
  unsigned HOST_WIDE_INT bits;
  int i;
  enum machine_mode mode;
  rtx *regs;

  /* Work out how many bits to move at a time.  If both operands have
     half-word alignment, it is usually better to move in half words.
     For instance, lh/lh/sh/sh is usually better than lwl/lwr/swl/swr
     and lw/lw/sw/sw is usually better than ldl/ldr/sdl/sdr.
     Otherwise move word-sized chunks.  */
  if (MEM_ALIGN (src) == BITS_PER_WORD / 2
      && MEM_ALIGN (dest) == BITS_PER_WORD / 2)
    bits = BITS_PER_WORD / 2;
  else
    bits = BITS_PER_WORD;

  mode = mode_for_size (bits, MODE_INT, 0);
  delta = bits / BITS_PER_UNIT;

  /* Allocate a buffer for the temporary registers.  */
  regs = XALLOCAVEC (rtx, length / delta);

  /* Load as many BITS-sized chunks as possible.  Use a normal load if
     the source has enough alignment, otherwise use left/right pairs.  */
  for (offset = 0, i = 0; offset + delta <= length; offset += delta, i++)
    {
      regs[i] = gen_reg_rtx (mode);
      if (MEM_ALIGN (src) >= bits)
	mips_emit_move (regs[i], adjust_address (src, mode, offset));
      else
	{
	  rtx part = adjust_address (src, BLKmode, offset);
	  if (!mips_expand_ext_as_unaligned_load (regs[i], part, bits, 0))
	    gcc_unreachable ();
	}
    }

  /* Copy the chunks to the destination.  */
  for (offset = 0, i = 0; offset + delta <= length; offset += delta, i++)
    if (MEM_ALIGN (dest) >= bits)
      mips_emit_move (adjust_address (dest, mode, offset), regs[i]);
    else
      {
	rtx part = adjust_address (dest, BLKmode, offset);
	if (!mips_expand_ins_as_unaligned_store (part, regs[i], bits, 0))
	  gcc_unreachable ();
      }

  /* Mop up any left-over bytes.  */
  if (offset < length)
    {
      src = adjust_address (src, BLKmode, offset);
      dest = adjust_address (dest, BLKmode, offset);
      move_by_pieces (dest, src, length - offset,
		      MIN (MEM_ALIGN (src), MEM_ALIGN (dest)), 0);
    }
}

/* Helper function for doing a loop-based block operation on memory
   reference MEM.  Each iteration of the loop will operate on LENGTH
   bytes of MEM.

   Create a new base register for use within the loop and point it to
   the start of MEM.  Create a new memory reference that uses this
   register.  Store them in *LOOP_REG and *LOOP_MEM respectively.  */

static void
mips_adjust_block_mem (rtx mem, HOST_WIDE_INT length,
		       rtx *loop_reg, rtx *loop_mem)
{
  *loop_reg = copy_addr_to_reg (XEXP (mem, 0));

  /* Although the new mem does not refer to a known location,
     it does keep up to LENGTH bytes of alignment.  */
  *loop_mem = change_address (mem, BLKmode, *loop_reg);
  set_mem_align (*loop_mem, MIN (MEM_ALIGN (mem), length * BITS_PER_UNIT));
}

/* Move LENGTH bytes from SRC to DEST using a loop that moves BYTES_PER_ITER
   bytes at a time.  LENGTH must be at least BYTES_PER_ITER.  Assume that
   the memory regions do not overlap.  */

static void
mips_block_move_loop (rtx dest, rtx src, HOST_WIDE_INT length,
		      HOST_WIDE_INT bytes_per_iter)
{
  rtx label, src_reg, dest_reg, final_src, test;
  HOST_WIDE_INT leftover;

  leftover = length % bytes_per_iter;
  length -= leftover;

  /* Create registers and memory references for use within the loop.  */
  mips_adjust_block_mem (src, bytes_per_iter, &src_reg, &src);
  mips_adjust_block_mem (dest, bytes_per_iter, &dest_reg, &dest);

  /* Calculate the value that SRC_REG should have after the last iteration
     of the loop.  */
  final_src = expand_simple_binop (Pmode, PLUS, src_reg, GEN_INT (length),
				   0, 0, OPTAB_WIDEN);

  /* Emit the start of the loop.  */
  label = gen_label_rtx ();
  emit_label (label);

  /* Emit the loop body.  */
  mips_block_move_straight (dest, src, bytes_per_iter);

  /* Move on to the next block.  */
  mips_emit_move (src_reg, plus_constant (src_reg, bytes_per_iter));
  mips_emit_move (dest_reg, plus_constant (dest_reg, bytes_per_iter));

  /* Emit the loop condition.  */
  test = gen_rtx_NE (VOIDmode, src_reg, final_src);
  if (Pmode == DImode)
    emit_jump_insn (gen_cbranchdi4 (test, src_reg, final_src, label));
  else
    emit_jump_insn (gen_cbranchsi4 (test, src_reg, final_src, label));

  /* Mop up any left-over bytes.  */
  if (leftover)
    mips_block_move_straight (dest, src, leftover);
}

/* Expand a movmemsi instruction, which copies LENGTH bytes from
   memory reference SRC to memory reference DEST.  */

bool
mips_expand_block_move (rtx dest, rtx src, rtx length)
{
  if (CONST_INT_P (length))
    {
      if (INTVAL (length) <= MIPS_MAX_MOVE_BYTES_STRAIGHT)
	{
	  mips_block_move_straight (dest, src, INTVAL (length));
	  return true;
	}
      else if (optimize)
	{
	  mips_block_move_loop (dest, src, INTVAL (length),
				MIPS_MAX_MOVE_BYTES_PER_LOOP_ITER);
	  return true;
	}
    }
  return false;
}

/* Expand a loop of synci insns for the address range [BEGIN, END).  */

void
mips_expand_synci_loop (rtx begin, rtx end)
{
  rtx inc, label, end_label, cmp_result, mask, length;

  /* Create end_label.  */
  end_label = gen_label_rtx ();

  /* Check if begin equals end.  */
  cmp_result = gen_rtx_EQ (VOIDmode, begin, end);
  emit_jump_insn (gen_condjump (cmp_result, end_label));

  /* Load INC with the cache line size (rdhwr INC,$1).  */
  inc = gen_reg_rtx (Pmode);
  emit_insn (Pmode == SImode
	     ? gen_rdhwr_synci_step_si (inc)
	     : gen_rdhwr_synci_step_di (inc));

  /* Check if inc is 0.  */
  cmp_result = gen_rtx_EQ (VOIDmode, inc, const0_rtx);
  emit_jump_insn (gen_condjump (cmp_result, end_label));

  /* Calculate mask.  */
  mask = mips_force_unary (Pmode, NEG, inc);

  /* Mask out begin by mask.  */
  begin = mips_force_binary (Pmode, AND, begin, mask);

  /* Calculate length.  */
  length = mips_force_binary (Pmode, MINUS, end, begin);

  /* Loop back to here.  */
  label = gen_label_rtx ();
  emit_label (label);

  emit_insn (gen_synci (begin));

  /* Update length.  */
  mips_emit_binary (MINUS, length, length, inc);

  /* Update begin.  */
  mips_emit_binary (PLUS, begin, begin, inc);

  /* Check if length is greater than 0.  */
  cmp_result = gen_rtx_GT (VOIDmode, length, const0_rtx);
  emit_jump_insn (gen_condjump (cmp_result, label));

  emit_label (end_label);
}

/* Expand a QI or HI mode atomic memory operation.

   GENERATOR contains a pointer to the gen_* function that generates
   the SI mode underlying atomic operation using masks that we
   calculate.

   RESULT is the return register for the operation.  Its value is NULL
   if unused.

   MEM is the location of the atomic access.

   OLDVAL is the first operand for the operation.

   NEWVAL is the optional second operand for the operation.  Its value
   is NULL if unused.  */

void
mips_expand_atomic_qihi (union mips_gen_fn_ptrs generator,
                         rtx result, rtx mem, rtx oldval, rtx newval)
{
  rtx orig_addr, memsi_addr, memsi, shift, shiftsi, unshifted_mask;
  rtx unshifted_mask_reg, mask, inverted_mask, si_op;
  rtx res = NULL;
  enum machine_mode mode;

  mode = GET_MODE (mem);

  /* Compute the address of the containing SImode value.  */
  orig_addr = force_reg (Pmode, XEXP (mem, 0));
  memsi_addr = mips_force_binary (Pmode, AND, orig_addr,
				  force_reg (Pmode, GEN_INT (-4)));

  /* Create a memory reference for it.  */
  memsi = gen_rtx_MEM (SImode, memsi_addr);
  set_mem_alias_set (memsi, ALIAS_SET_MEMORY_BARRIER);
  MEM_VOLATILE_P (memsi) = MEM_VOLATILE_P (mem);

  /* Work out the byte offset of the QImode or HImode value,
     counting from the least significant byte.  */
  shift = mips_force_binary (Pmode, AND, orig_addr, GEN_INT (3));
  if (TARGET_BIG_ENDIAN)
    mips_emit_binary (XOR, shift, shift, GEN_INT (mode == QImode ? 3 : 2));

  /* Multiply by eight to convert the shift value from bytes to bits.  */
  mips_emit_binary (ASHIFT, shift, shift, GEN_INT (3));

  /* Make the final shift an SImode value, so that it can be used in
     SImode operations.  */
  shiftsi = force_reg (SImode, gen_lowpart (SImode, shift));

  /* Set MASK to an inclusive mask of the QImode or HImode value.  */
  unshifted_mask = GEN_INT (GET_MODE_MASK (mode));
  unshifted_mask_reg = force_reg (SImode, unshifted_mask);
  mask = mips_force_binary (SImode, ASHIFT, unshifted_mask_reg, shiftsi);

  /* Compute the equivalent exclusive mask.  */
  inverted_mask = gen_reg_rtx (SImode);
  emit_insn (gen_rtx_SET (VOIDmode, inverted_mask,
			  gen_rtx_NOT (SImode, mask)));

  /* Shift the old value into place.  */
  if (oldval != const0_rtx)
    {
      oldval = convert_modes (SImode, mode, oldval, true);
      oldval = force_reg (SImode, oldval);
      oldval = mips_force_binary (SImode, ASHIFT, oldval, shiftsi);
    }

  /* Do the same for the new value.  */
  if (newval && newval != const0_rtx)
    {
      newval = convert_modes (SImode, mode, newval, true);
      newval = force_reg (SImode, newval);
      newval = mips_force_binary (SImode, ASHIFT, newval, shiftsi);
    }

  /* Do the SImode atomic access.  */
  if (result)
    res = gen_reg_rtx (SImode);
  if (newval)
    si_op = generator.fn_6 (res, memsi, mask, inverted_mask, oldval, newval);
  else if (result)
    si_op = generator.fn_5 (res, memsi, mask, inverted_mask, oldval);
  else
    si_op = generator.fn_4 (memsi, mask, inverted_mask, oldval);

  emit_insn (si_op);

  if (result)
    {
      /* Shift and convert the result.  */
      mips_emit_binary (AND, res, res, mask);
      mips_emit_binary (LSHIFTRT, res, res, shiftsi);
      mips_emit_move (result, gen_lowpart (GET_MODE (result), res));
    }
}

/* Return true if it is possible to use left/right accesses for a
   bitfield of WIDTH bits starting BITPOS bits into *OP.  When
   returning true, update *OP, *LEFT and *RIGHT as follows:

   *OP is a BLKmode reference to the whole field.

   *LEFT is a QImode reference to the first byte if big endian or
   the last byte if little endian.  This address can be used in the
   left-side instructions (LWL, SWL, LDL, SDL).

   *RIGHT is a QImode reference to the opposite end of the field and
   can be used in the patterning right-side instruction.  */

static bool
mips_get_unaligned_mem (rtx *op, HOST_WIDE_INT width, HOST_WIDE_INT bitpos,
			rtx *left, rtx *right)
{
  rtx first, last;

  /* Check that the operand really is a MEM.  Not all the extv and
     extzv predicates are checked.  */
  if (!MEM_P (*op))
    return false;

  /* Check that the size is valid.  */
  if (width != 32 && (!TARGET_64BIT || width != 64))
    return false;

  /* We can only access byte-aligned values.  Since we are always passed
     a reference to the first byte of the field, it is not necessary to
     do anything with BITPOS after this check.  */
  if (bitpos % BITS_PER_UNIT != 0)
    return false;

  /* Reject aligned bitfields: we want to use a normal load or store
     instead of a left/right pair.  */
  if (MEM_ALIGN (*op) >= width)
    return false;

  /* Adjust *OP to refer to the whole field.  This also has the effect
     of legitimizing *OP's address for BLKmode, possibly simplifying it.  */
  *op = adjust_address (*op, BLKmode, 0);
  set_mem_size (*op, GEN_INT (width / BITS_PER_UNIT));

  /* Get references to both ends of the field.  We deliberately don't
     use the original QImode *OP for FIRST since the new BLKmode one
     might have a simpler address.  */
  first = adjust_address (*op, QImode, 0);
  last = adjust_address (*op, QImode, width / BITS_PER_UNIT - 1);

  /* Allocate to LEFT and RIGHT according to endianness.  LEFT should
     correspond to the MSB and RIGHT to the LSB.  */
  if (TARGET_BIG_ENDIAN)
    *left = first, *right = last;
  else
    *left = last, *right = first;

  return true;
}

/* Try to use left/right loads to expand an "extv" or "extzv" pattern.
   DEST, SRC, WIDTH and BITPOS are the operands passed to the expander;
   the operation is the equivalent of:

      (set DEST (*_extract SRC WIDTH BITPOS))

   Return true on success.  */

bool
mips_expand_ext_as_unaligned_load (rtx dest, rtx src, HOST_WIDE_INT width,
				   HOST_WIDE_INT bitpos)
{
  rtx left, right, temp;

  /* If TARGET_64BIT, the destination of a 32-bit "extz" or "extzv" will
     be a paradoxical word_mode subreg.  This is the only case in which
     we allow the destination to be larger than the source.  */
  if (GET_CODE (dest) == SUBREG
      && GET_MODE (dest) == DImode
      && GET_MODE (SUBREG_REG (dest)) == SImode)
    dest = SUBREG_REG (dest);

  /* After the above adjustment, the destination must be the same
     width as the source.  */
  if (GET_MODE_BITSIZE (GET_MODE (dest)) != width)
    return false;

  if (!mips_get_unaligned_mem (&src, width, bitpos, &left, &right))
    return false;

  temp = gen_reg_rtx (GET_MODE (dest));
  if (GET_MODE (dest) == DImode)
    {
      emit_insn (gen_mov_ldl (temp, src, left));
      emit_insn (gen_mov_ldr (dest, copy_rtx (src), right, temp));
    }
  else
    {
      emit_insn (gen_mov_lwl (temp, src, left));
      emit_insn (gen_mov_lwr (dest, copy_rtx (src), right, temp));
    }
  return true;
}

/* Try to use left/right stores to expand an "ins" pattern.  DEST, WIDTH,
   BITPOS and SRC are the operands passed to the expander; the operation
   is the equivalent of:

       (set (zero_extract DEST WIDTH BITPOS) SRC)

   Return true on success.  */

bool
mips_expand_ins_as_unaligned_store (rtx dest, rtx src, HOST_WIDE_INT width,
				    HOST_WIDE_INT bitpos)
{
  rtx left, right;
  enum machine_mode mode;

  if (!mips_get_unaligned_mem (&dest, width, bitpos, &left, &right))
    return false;

  mode = mode_for_size (width, MODE_INT, 0);
  src = gen_lowpart (mode, src);
  if (mode == DImode)
    {
      emit_insn (gen_mov_sdl (dest, src, left));
      emit_insn (gen_mov_sdr (copy_rtx (dest), copy_rtx (src), right));
    }
  else
    {
      emit_insn (gen_mov_swl (dest, src, left));
      emit_insn (gen_mov_swr (copy_rtx (dest), copy_rtx (src), right));
    }
  return true;
}

/* Return true if X is a MEM with the same size as MODE.  */

bool
mips_mem_fits_mode_p (enum machine_mode mode, rtx x)
{
  rtx size;

  if (!MEM_P (x))
    return false;

  size = MEM_SIZE (x);
  return size && INTVAL (size) == GET_MODE_SIZE (mode);
}

/* Return true if (zero_extract OP WIDTH BITPOS) can be used as the
   source of an "ext" instruction or the destination of an "ins"
   instruction.  OP must be a register operand and the following
   conditions must hold:

     0 <= BITPOS < GET_MODE_BITSIZE (GET_MODE (op))
     0 < WIDTH <= GET_MODE_BITSIZE (GET_MODE (op))
     0 < BITPOS + WIDTH <= GET_MODE_BITSIZE (GET_MODE (op))

   Also reject lengths equal to a word as they are better handled
   by the move patterns.  */

bool
mips_use_ins_ext_p (rtx op, HOST_WIDE_INT width, HOST_WIDE_INT bitpos)
{
  if (!ISA_HAS_EXT_INS
      || !register_operand (op, VOIDmode)
      || GET_MODE_BITSIZE (GET_MODE (op)) > BITS_PER_WORD)
    return false;

  if (!IN_RANGE (width, 1, GET_MODE_BITSIZE (GET_MODE (op)) - 1))
    return false;

  if (bitpos < 0 || bitpos + width > GET_MODE_BITSIZE (GET_MODE (op)))
    return false;

  return true;
}

/* Check if MASK and SHIFT are valid in mask-low-and-shift-left
   operation if MAXLEN is the maxium length of consecutive bits that
   can make up MASK.  MODE is the mode of the operation.  See
   mask_low_and_shift_len for the actual definition.  */

bool
mask_low_and_shift_p (enum machine_mode mode, rtx mask, rtx shift, int maxlen)
{
  return IN_RANGE (mask_low_and_shift_len (mode, mask, shift), 1, maxlen);
}

/* Return true iff OP1 and OP2 are valid operands together for the
   *and<MODE>3 and *and<MODE>3_mips16 patterns.  For the cases to consider,
   see the table in the comment before the pattern.  */

bool
and_operands_ok (enum machine_mode mode, rtx op1, rtx op2)
{
  return (memory_operand (op1, mode)
	  ? and_load_operand (op2, mode)
	  : and_reg_operand (op2, mode));
}

/* The canonical form of a mask-low-and-shift-left operation is
   (and (ashift X SHIFT) MASK) where MASK has the lower SHIFT number of bits
   cleared.  Thus we need to shift MASK to the right before checking if it
   is a valid mask value.  MODE is the mode of the operation.  If true
   return the length of the mask, otherwise return -1.  */

int
mask_low_and_shift_len (enum machine_mode mode, rtx mask, rtx shift)
{
  HOST_WIDE_INT shval;

  shval = INTVAL (shift) & (GET_MODE_BITSIZE (mode) - 1);
  return exact_log2 ((UINTVAL (mask) >> shval) + 1);
}

/* Return true if -msplit-addresses is selected and should be honored.

   -msplit-addresses is a half-way house between explicit relocations
   and the traditional assembler macros.  It can split absolute 32-bit
   symbolic constants into a high/lo_sum pair but uses macros for other
   sorts of access.

   Like explicit relocation support for REL targets, it relies
   on GNU extensions in the assembler and the linker.

   Although this code should work for -O0, it has traditionally
   been treated as an optimization.  */

static bool
mips_split_addresses_p (void)
{
  return (TARGET_SPLIT_ADDRESSES
	  && optimize
	  && !TARGET_MIPS16
	  && !flag_pic
	  && !ABI_HAS_64BIT_SYMBOLS);
}

/* (Re-)Initialize mips_split_p, mips_lo_relocs and mips_hi_relocs.  */

static void
mips_init_relocs (void)
{
  memset (mips_split_p, '\0', sizeof (mips_split_p));
  memset (mips_split_hi_p, '\0', sizeof (mips_split_hi_p));
  memset (mips_hi_relocs, '\0', sizeof (mips_hi_relocs));
  memset (mips_lo_relocs, '\0', sizeof (mips_lo_relocs));

  if (ABI_HAS_64BIT_SYMBOLS)
    {
      if (TARGET_EXPLICIT_RELOCS)
	{
	  mips_split_p[SYMBOL_64_HIGH] = true;
	  mips_hi_relocs[SYMBOL_64_HIGH] = "%highest(";
	  mips_lo_relocs[SYMBOL_64_HIGH] = "%higher(";

	  mips_split_p[SYMBOL_64_MID] = true;
	  mips_hi_relocs[SYMBOL_64_MID] = "%higher(";
	  mips_lo_relocs[SYMBOL_64_MID] = "%hi(";

	  mips_split_p[SYMBOL_64_LOW] = true;
	  mips_hi_relocs[SYMBOL_64_LOW] = "%hi(";
	  mips_lo_relocs[SYMBOL_64_LOW] = "%lo(";

	  mips_split_p[SYMBOL_ABSOLUTE] = true;
	  mips_lo_relocs[SYMBOL_ABSOLUTE] = "%lo(";
	}
    }
  else
    {
      if (TARGET_EXPLICIT_RELOCS || mips_split_addresses_p () || TARGET_MIPS16)
	{
	  mips_split_p[SYMBOL_ABSOLUTE] = true;
	  mips_hi_relocs[SYMBOL_ABSOLUTE] = "%hi(";
	  mips_lo_relocs[SYMBOL_ABSOLUTE] = "%lo(";

	  mips_lo_relocs[SYMBOL_32_HIGH] = "%hi(";
	}
    }

  if (TARGET_MIPS16)
    {
      /* The high part is provided by a pseudo copy of $gp.  */
      mips_split_p[SYMBOL_GP_RELATIVE] = true;
      mips_lo_relocs[SYMBOL_GP_RELATIVE] = "%gprel(";
    }
  else if (TARGET_EXPLICIT_RELOCS)
    /* Small data constants are kept whole until after reload,
       then lowered by mips_rewrite_small_data.  */
    mips_lo_relocs[SYMBOL_GP_RELATIVE] = "%gp_rel(";

  if (TARGET_EXPLICIT_RELOCS)
    {
      mips_split_p[SYMBOL_GOT_PAGE_OFST] = true;
      if (TARGET_NEWABI)
	{
	  mips_lo_relocs[SYMBOL_GOTOFF_PAGE] = "%got_page(";
	  mips_lo_relocs[SYMBOL_GOT_PAGE_OFST] = "%got_ofst(";
	}
      else
	{
	  mips_lo_relocs[SYMBOL_GOTOFF_PAGE] = "%got(";
	  mips_lo_relocs[SYMBOL_GOT_PAGE_OFST] = "%lo(";
	}
      if (TARGET_MIPS16)
	/* Expose the use of $28 as soon as possible.  */
	mips_split_hi_p[SYMBOL_GOT_PAGE_OFST] = true;

      if (TARGET_XGOT)
	{
	  /* The HIGH and LO_SUM are matched by special .md patterns.  */
	  mips_split_p[SYMBOL_GOT_DISP] = true;

	  mips_split_p[SYMBOL_GOTOFF_DISP] = true;
	  mips_hi_relocs[SYMBOL_GOTOFF_DISP] = "%got_hi(";
	  mips_lo_relocs[SYMBOL_GOTOFF_DISP] = "%got_lo(";

	  mips_split_p[SYMBOL_GOTOFF_CALL] = true;
	  mips_hi_relocs[SYMBOL_GOTOFF_CALL] = "%call_hi(";
	  mips_lo_relocs[SYMBOL_GOTOFF_CALL] = "%call_lo(";
	}
      else
	{
	  if (TARGET_NEWABI)
	    mips_lo_relocs[SYMBOL_GOTOFF_DISP] = "%got_disp(";
	  else
	    mips_lo_relocs[SYMBOL_GOTOFF_DISP] = "%got(";
	  mips_lo_relocs[SYMBOL_GOTOFF_CALL] = "%call16(";
	  if (TARGET_MIPS16)
	    /* Expose the use of $28 as soon as possible.  */
	    mips_split_p[SYMBOL_GOT_DISP] = true;
	}
    }

  if (TARGET_NEWABI)
    {
      mips_split_p[SYMBOL_GOTOFF_LOADGP] = true;
      mips_hi_relocs[SYMBOL_GOTOFF_LOADGP] = "%hi(%neg(%gp_rel(";
      mips_lo_relocs[SYMBOL_GOTOFF_LOADGP] = "%lo(%neg(%gp_rel(";
    }

  mips_lo_relocs[SYMBOL_TLSGD] = "%tlsgd(";
  mips_lo_relocs[SYMBOL_TLSLDM] = "%tlsldm(";

  mips_split_p[SYMBOL_DTPREL] = true;
  mips_hi_relocs[SYMBOL_DTPREL] = "%dtprel_hi(";
  mips_lo_relocs[SYMBOL_DTPREL] = "%dtprel_lo(";

  mips_lo_relocs[SYMBOL_GOTTPREL] = "%gottprel(";

  mips_split_p[SYMBOL_TPREL] = true;
  mips_hi_relocs[SYMBOL_TPREL] = "%tprel_hi(";
  mips_lo_relocs[SYMBOL_TPREL] = "%tprel_lo(";

  mips_lo_relocs[SYMBOL_HALF] = "%half(";
}

/* Print symbolic operand OP, which is part of a HIGH or LO_SUM
   in context CONTEXT.  RELOCS is the array of relocations to use.  */

static void
mips_print_operand_reloc (FILE *file, rtx op, enum mips_symbol_context context,
			  const char **relocs)
{
  enum mips_symbol_type symbol_type;
  const char *p;

  symbol_type = mips_classify_symbolic_expression (op, context);
  gcc_assert (relocs[symbol_type]);

  fputs (relocs[symbol_type], file);
  output_addr_const (file, mips_strip_unspec_address (op));
  for (p = relocs[symbol_type]; *p != 0; p++)
    if (*p == '(')
      fputc (')', file);
}

/* Start a new block with the given asm switch enabled.  If we need
   to print a directive, emit PREFIX before it and SUFFIX after it.  */

static void
mips_push_asm_switch_1 (struct mips_asm_switch *asm_switch,
			const char *prefix, const char *suffix)
{
  if (asm_switch->nesting_level == 0)
    fprintf (asm_out_file, "%s.set\tno%s%s", prefix, asm_switch->name, suffix);
  asm_switch->nesting_level++;
}

/* Likewise, but end a block.  */

static void
mips_pop_asm_switch_1 (struct mips_asm_switch *asm_switch,
		       const char *prefix, const char *suffix)
{
  gcc_assert (asm_switch->nesting_level);
  asm_switch->nesting_level--;
  if (asm_switch->nesting_level == 0)
    fprintf (asm_out_file, "%s.set\t%s%s", prefix, asm_switch->name, suffix);
}

/* Wrappers around mips_push_asm_switch_1 and mips_pop_asm_switch_1
   that either print a complete line or print nothing.  */

void
mips_push_asm_switch (struct mips_asm_switch *asm_switch)
{
  mips_push_asm_switch_1 (asm_switch, "\t", "\n");
}

void
mips_pop_asm_switch (struct mips_asm_switch *asm_switch)
{
  mips_pop_asm_switch_1 (asm_switch, "\t", "\n");
}

/* Print the text for PRINT_OPERAND punctation character CH to FILE.
   The punctuation characters are:

   '('	Start a nested ".set noreorder" block.
   ')'	End a nested ".set noreorder" block.
   '['	Start a nested ".set noat" block.
   ']'	End a nested ".set noat" block.
   '<'	Start a nested ".set nomacro" block.
   '>'	End a nested ".set nomacro" block.
   '*'	Behave like %(%< if generating a delayed-branch sequence.
   '#'	Print a nop if in a ".set noreorder" block.
   '/'	Like '#', but do nothing within a delayed-branch sequence.
   '?'	Print "l" if mips_branch_likely is true
   '~'	Print a nop if mips_branch_likely is true
   '.'	Print the name of the register with a hard-wired zero (zero or $0).
   '@'	Print the name of the assembler temporary register (at or $1).
   '^'	Print the name of the pic call-through register (t9 or $25).
   '+'	Print the name of the gp register (usually gp or $28).
   '$'	Print the name of the stack pointer register (sp or $29).

   See also mips_init_print_operand_pucnt.  */

static void
mips_print_operand_punctuation (FILE *file, int ch)
{
  switch (ch)
    {
    case '(':
      mips_push_asm_switch_1 (&mips_noreorder, "", "\n\t");
      break;

    case ')':
      mips_pop_asm_switch_1 (&mips_noreorder, "\n\t", "");
      break;

    case '[':
      mips_push_asm_switch_1 (&mips_noat, "", "\n\t");
      break;

    case ']':
      mips_pop_asm_switch_1 (&mips_noat, "\n\t", "");
      break;

    case '<':
      mips_push_asm_switch_1 (&mips_nomacro, "", "\n\t");
      break;

    case '>':
      mips_pop_asm_switch_1 (&mips_nomacro, "\n\t", "");
      break;

    case '*':
      if (final_sequence != 0)
	{
	  mips_print_operand_punctuation (file, '(');
	  mips_print_operand_punctuation (file, '<');
	}
      break;

    case '#':
      if (mips_noreorder.nesting_level > 0)
	fputs ("\n\tnop", file);
      break;

    case '/':
      /* Print an extra newline so that the delayed insn is separated
	 from the following ones.  This looks neater and is consistent
	 with non-nop delayed sequences.  */
      if (mips_noreorder.nesting_level > 0 && final_sequence == 0)
	fputs ("\n\tnop\n", file);
      break;

    case '?':
      if (mips_branch_likely)
	putc ('l', file);
      break;

    case '~':
      if (mips_branch_likely)
	fputs ("\n\tnop", file);
      break;

    case '.':
      fputs (reg_names[GP_REG_FIRST + 0], file);
      break;

    case '@':
      fputs (reg_names[AT_REGNUM], file);
      break;

    case '^':
      fputs (reg_names[PIC_FUNCTION_ADDR_REGNUM], file);
      break;

    case '+':
      fputs (reg_names[PIC_OFFSET_TABLE_REGNUM], file);
      break;

    case '$':
      fputs (reg_names[STACK_POINTER_REGNUM], file);
      break;

    default:
      gcc_unreachable ();
      break;
    }
}

/* Initialize mips_print_operand_punct.  */

static void
mips_init_print_operand_punct (void)
{
  const char *p;

  for (p = "()[]<>*#/?~.@^+$"; *p; p++)
    mips_print_operand_punct[(unsigned char) *p] = true;
}

/* PRINT_OPERAND prefix LETTER refers to the integer branch instruction
   associated with condition CODE.  Print the condition part of the
   opcode to FILE.  */

static void
mips_print_int_branch_condition (FILE *file, enum rtx_code code, int letter)
{
  switch (code)
    {
    case EQ:
    case NE:
    case GT:
    case GE:
    case LT:
    case LE:
    case GTU:
    case GEU:
    case LTU:
    case LEU:
      /* Conveniently, the MIPS names for these conditions are the same
	 as their RTL equivalents.  */
      fputs (GET_RTX_NAME (code), file);
      break;

    default:
      output_operand_lossage ("'%%%c' is not a valid operand prefix", letter);
      break;
    }
}

/* Likewise floating-point branches.  */

static void
mips_print_float_branch_condition (FILE *file, enum rtx_code code, int letter)
{
  switch (code)
    {
    case EQ:
      fputs ("c1f", file);
      break;

    case NE:
      fputs ("c1t", file);
      break;

    default:
      output_operand_lossage ("'%%%c' is not a valid operand prefix", letter);
      break;
    }
}

/* Implement TARGET_PRINT_OPERAND_PUNCT_VALID_P.  */

static bool
mips_print_operand_punct_valid_p (unsigned char code)
{
  return mips_print_operand_punct[code];
}

/* Implement TARGET_PRINT_OPERAND.  The MIPS-specific operand codes are:

   'X'	Print CONST_INT OP in hexadecimal format.
   'x'	Print the low 16 bits of CONST_INT OP in hexadecimal format.
   'd'	Print CONST_INT OP in decimal.
   'm'	Print one less than CONST_INT OP in decimal.
   'h'	Print the high-part relocation associated with OP, after stripping
	  any outermost HIGH.
   'R'	Print the low-part relocation associated with OP.
   'C'	Print the integer branch condition for comparison OP.
   'N'	Print the inverse of the integer branch condition for comparison OP.
   'F'	Print the FPU branch condition for comparison OP.
   'W'	Print the inverse of the FPU branch condition for comparison OP.
   'T'	Print 'f' for (eq:CC ...), 't' for (ne:CC ...),
	      'z' for (eq:?I ...), 'n' for (ne:?I ...).
   't'	Like 'T', but with the EQ/NE cases reversed
   'Y'	Print mips_fp_conditions[INTVAL (OP)]
   'Z'	Print OP and a comma for ISA_HAS_8CC, otherwise print nothing.
   'q'	Print a DSP accumulator register.
   'D'	Print the second part of a double-word register or memory operand.
   'L'	Print the low-order register in a double-word register operand.
   'M'	Print high-order register in a double-word register operand.
   'z'	Print $0 if OP is zero, otherwise print OP normally.  */

static void
mips_print_operand (FILE *file, rtx op, int letter)
{
  enum rtx_code code;

  if (mips_print_operand_punct_valid_p (letter))
    {
      mips_print_operand_punctuation (file, letter);
      return;
    }

  gcc_assert (op);
  code = GET_CODE (op);

  switch (letter)
    {
    case 'X':
      if (CONST_INT_P (op))
	fprintf (file, HOST_WIDE_INT_PRINT_HEX, INTVAL (op));
      else
	output_operand_lossage ("invalid use of '%%%c'", letter);
      break;

    case 'x':
      if (CONST_INT_P (op))
	fprintf (file, HOST_WIDE_INT_PRINT_HEX, INTVAL (op) & 0xffff);
      else
	output_operand_lossage ("invalid use of '%%%c'", letter);
      break;

    case 'd':
      if (CONST_INT_P (op))
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (op));
      else
	output_operand_lossage ("invalid use of '%%%c'", letter);
      break;

    case 'm':
      if (CONST_INT_P (op))
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (op) - 1);
      else
	output_operand_lossage ("invalid use of '%%%c'", letter);
      break;

    case 'h':
      if (code == HIGH)
	op = XEXP (op, 0);
      mips_print_operand_reloc (file, op, SYMBOL_CONTEXT_LEA, mips_hi_relocs);
      break;

    case 'R':
      mips_print_operand_reloc (file, op, SYMBOL_CONTEXT_LEA, mips_lo_relocs);
      break;

    case 'C':
      mips_print_int_branch_condition (file, code, letter);
      break;

    case 'N':
      mips_print_int_branch_condition (file, reverse_condition (code), letter);
      break;

    case 'F':
      mips_print_float_branch_condition (file, code, letter);
      break;

    case 'W':
      mips_print_float_branch_condition (file, reverse_condition (code),
					 letter);
      break;

    case 'T':
    case 't':
      {
	int truth = (code == NE) == (letter == 'T');
	fputc ("zfnt"[truth * 2 + (GET_MODE (op) == CCmode)], file);
      }
      break;

    case 'Y':
      if (code == CONST_INT && UINTVAL (op) < ARRAY_SIZE (mips_fp_conditions))
	fputs (mips_fp_conditions[UINTVAL (op)], file);
      else
	output_operand_lossage ("'%%%c' is not a valid operand prefix",
				letter);
      break;

    case 'Z':
      if (ISA_HAS_8CC)
	{
	  mips_print_operand (file, op, 0);
	  fputc (',', file);
	}
      break;

    case 'q':
      if (code == REG && MD_REG_P (REGNO (op)))
	fprintf (file, "$ac0");
      else if (code == REG && DSP_ACC_REG_P (REGNO (op)))
	fprintf (file, "$ac%c", reg_names[REGNO (op)][3]);
      else
	output_operand_lossage ("invalid use of '%%%c'", letter);
      break;

    default:
      switch (code)
	{
	case REG:
	  {
	    unsigned int regno = REGNO (op);
	    if ((letter == 'M' && TARGET_LITTLE_ENDIAN)
		|| (letter == 'L' && TARGET_BIG_ENDIAN)
		|| letter == 'D')
	      regno++;
	    else if (letter && letter != 'z' && letter != 'M' && letter != 'L')
	      output_operand_lossage ("invalid use of '%%%c'", letter);
	    /* We need to print $0 .. $31 for COP0 registers.  */
	    if (COP0_REG_P (regno))
	      fprintf (file, "$%s", &reg_names[regno][4]);
	    else
	      fprintf (file, "%s", reg_names[regno]);
	  }
	  break;

	case MEM:
	  if (letter == 'D')
	    output_address (plus_constant (XEXP (op, 0), 4));
	  else if (letter && letter != 'z')
	    output_operand_lossage ("invalid use of '%%%c'", letter);
	  else
	    output_address (XEXP (op, 0));
	  break;

	default:
	  if (letter == 'z' && op == CONST0_RTX (GET_MODE (op)))
	    fputs (reg_names[GP_REG_FIRST], file);
	  else if (letter && letter != 'z')
	    output_operand_lossage ("invalid use of '%%%c'", letter);
	  else if (CONST_GP_P (op))
	    fputs (reg_names[GLOBAL_POINTER_REGNUM], file);
	  else
	    output_addr_const (file, mips_strip_unspec_address (op));
	  break;
	}
    }
}

/* Implement TARGET_PRINT_OPERAND_ADDRESS.  */

static void
mips_print_operand_address (FILE *file, rtx x)
{
  struct mips_address_info addr;

  if (mips_classify_address (&addr, x, word_mode, true))
    switch (addr.type)
      {
      case ADDRESS_REG:
	mips_print_operand (file, addr.offset, 0);
	fprintf (file, "(%s)", reg_names[REGNO (addr.reg)]);
	return;

      case ADDRESS_LO_SUM:
	mips_print_operand_reloc (file, addr.offset, SYMBOL_CONTEXT_MEM,
				  mips_lo_relocs);
	fprintf (file, "(%s)", reg_names[REGNO (addr.reg)]);
	return;

      case ADDRESS_CONST_INT:
	output_addr_const (file, x);
	fprintf (file, "(%s)", reg_names[GP_REG_FIRST]);
	return;

      case ADDRESS_SYMBOLIC:
	output_addr_const (file, mips_strip_unspec_address (x));
	return;
      }
  gcc_unreachable ();
}

/* Implement TARGET_ENCODE_SECTION_INFO.  */

static void
mips_encode_section_info (tree decl, rtx rtl, int first)
{
  default_encode_section_info (decl, rtl, first);

  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      rtx symbol = XEXP (rtl, 0);
      tree type = TREE_TYPE (decl);

      /* Encode whether the symbol is short or long.  */
      if ((TARGET_LONG_CALLS && !mips_near_type_p (type))
	  || mips_far_type_p (type))
	SYMBOL_REF_FLAGS (symbol) |= SYMBOL_FLAG_LONG_CALL;
    }
}

/* Implement TARGET_SELECT_RTX_SECTION.  */

static section *
mips_select_rtx_section (enum machine_mode mode, rtx x,
			 unsigned HOST_WIDE_INT align)
{
  /* ??? Consider using mergeable small data sections.  */
  if (mips_rtx_constant_in_small_data_p (mode))
    return get_named_section (NULL, ".sdata", 0);

  return default_elf_select_rtx_section (mode, x, align);
}

/* Implement TARGET_ASM_FUNCTION_RODATA_SECTION.

   The complication here is that, with the combination TARGET_ABICALLS
   && !TARGET_ABSOLUTE_ABICALLS && !TARGET_GPWORD, jump tables will use
   absolute addresses, and should therefore not be included in the
   read-only part of a DSO.  Handle such cases by selecting a normal
   data section instead of a read-only one.  The logic apes that in
   default_function_rodata_section.  */

static section *
mips_function_rodata_section (tree decl)
{
  if (!TARGET_ABICALLS || TARGET_ABSOLUTE_ABICALLS || TARGET_GPWORD)
    return default_function_rodata_section (decl);

  if (decl && DECL_SECTION_NAME (decl))
    {
      const char *name = TREE_STRING_POINTER (DECL_SECTION_NAME (decl));
      if (DECL_ONE_ONLY (decl) && strncmp (name, ".gnu.linkonce.t.", 16) == 0)
	{
	  char *rname = ASTRDUP (name);
	  rname[14] = 'd';
	  return get_section (rname, SECTION_LINKONCE | SECTION_WRITE, decl);
	}
      else if (flag_function_sections
	       && flag_data_sections
	       && strncmp (name, ".text.", 6) == 0)
	{
	  char *rname = ASTRDUP (name);
	  memcpy (rname + 1, "data", 4);
	  return get_section (rname, SECTION_WRITE, decl);
	}
    }
  return data_section;
}

/* Implement TARGET_IN_SMALL_DATA_P.  */

static bool
mips_in_small_data_p (const_tree decl)
{
  unsigned HOST_WIDE_INT size;

  if (TREE_CODE (decl) == STRING_CST || TREE_CODE (decl) == FUNCTION_DECL)
    return false;

  /* We don't yet generate small-data references for -mabicalls
     or VxWorks RTP code.  See the related -G handling in
     mips_option_override.  */
  if (TARGET_ABICALLS || TARGET_VXWORKS_RTP)
    return false;

  if (TREE_CODE (decl) == VAR_DECL && DECL_SECTION_NAME (decl) != 0)
    {
      const char *name;

      /* Reject anything that isn't in a known small-data section.  */
      name = TREE_STRING_POINTER (DECL_SECTION_NAME (decl));
      if (strcmp (name, ".sdata") != 0 && strcmp (name, ".sbss") != 0)
	return false;

      /* If a symbol is defined externally, the assembler will use the
	 usual -G rules when deciding how to implement macros.  */
      if (mips_lo_relocs[SYMBOL_GP_RELATIVE] || !DECL_EXTERNAL (decl))
	return true;
    }
  else if (TARGET_EMBEDDED_DATA)
    {
      /* Don't put constants into the small data section: we want them
	 to be in ROM rather than RAM.  */
      if (TREE_CODE (decl) != VAR_DECL)
	return false;

      if (TREE_READONLY (decl)
	  && !TREE_SIDE_EFFECTS (decl)
	  && (!DECL_INITIAL (decl) || TREE_CONSTANT (DECL_INITIAL (decl))))
	return false;
    }

  /* Enforce -mlocal-sdata.  */
  if (!TARGET_LOCAL_SDATA && !TREE_PUBLIC (decl))
    return false;

  /* Enforce -mextern-sdata.  */
  if (!TARGET_EXTERN_SDATA && DECL_P (decl))
    {
      if (DECL_EXTERNAL (decl))
	return false;
      if (DECL_COMMON (decl) && DECL_INITIAL (decl) == NULL)
	return false;
    }

  /* We have traditionally not treated zero-sized objects as small data,
     so this is now effectively part of the ABI.  */
  size = int_size_in_bytes (TREE_TYPE (decl));
  return size > 0 && size <= mips_small_data_threshold;
}

/* Implement TARGET_USE_ANCHORS_FOR_SYMBOL_P.  We don't want to use
   anchors for small data: the GP register acts as an anchor in that
   case.  We also don't want to use them for PC-relative accesses,
   where the PC acts as an anchor.  */

static bool
mips_use_anchors_for_symbol_p (const_rtx symbol)
{
  switch (mips_classify_symbol (symbol, SYMBOL_CONTEXT_MEM))
    {
    case SYMBOL_PC_RELATIVE:
    case SYMBOL_GP_RELATIVE:
      return false;

    default:
      return default_use_anchors_for_symbol_p (symbol);
    }
}

/* The MIPS debug format wants all automatic variables and arguments
   to be in terms of the virtual frame pointer (stack pointer before
   any adjustment in the function), while the MIPS 3.0 linker wants
   the frame pointer to be the stack pointer after the initial
   adjustment.  So, we do the adjustment here.  The arg pointer (which
   is eliminated) points to the virtual frame pointer, while the frame
   pointer (which may be eliminated) points to the stack pointer after
   the initial adjustments.  */

HOST_WIDE_INT
mips_debugger_offset (rtx addr, HOST_WIDE_INT offset)
{
  rtx offset2 = const0_rtx;
  rtx reg = eliminate_constant_term (addr, &offset2);

  if (offset == 0)
    offset = INTVAL (offset2);

  if (reg == stack_pointer_rtx
      || reg == frame_pointer_rtx
      || reg == hard_frame_pointer_rtx)
    {
      offset -= cfun->machine->frame.total_size;
      if (reg == hard_frame_pointer_rtx)
	offset += cfun->machine->frame.hard_frame_pointer_offset;
    }

  /* sdbout_parms does not want this to crash for unrecognized cases.  */
#if 0
  else if (reg != arg_pointer_rtx)
    fatal_insn ("mips_debugger_offset called with non stack/frame/arg pointer",
		addr);
#endif

  return offset;
}

/* Implement ASM_OUTPUT_EXTERNAL.  */

void
mips_output_external (FILE *file, tree decl, const char *name)
{
  default_elf_asm_output_external (file, decl, name);

  /* We output the name if and only if TREE_SYMBOL_REFERENCED is
     set in order to avoid putting out names that are never really
     used. */
  if (TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl)))
    {
      if (!TARGET_EXPLICIT_RELOCS && mips_in_small_data_p (decl))
	{
	  /* When using assembler macros, emit .extern directives for
	     all small-data externs so that the assembler knows how
	     big they are.

	     In most cases it would be safe (though pointless) to emit
	     .externs for other symbols too.  One exception is when an
	     object is within the -G limit but declared by the user to
	     be in a section other than .sbss or .sdata.  */
	  fputs ("\t.extern\t", file);
	  assemble_name (file, name);
	  fprintf (file, ", " HOST_WIDE_INT_PRINT_DEC "\n",
		   int_size_in_bytes (TREE_TYPE (decl)));
	}
    }
}

/* Implement TARGET_ASM_OUTPUT_SOURCE_FILENAME.  */

static void
mips_output_filename (FILE *stream, const char *name)
{
  /* If we are emitting DWARF-2, let dwarf2out handle the ".file"
     directives.  */
  if (write_symbols == DWARF2_DEBUG)
    return;
  else if (mips_output_filename_first_time)
    {
      mips_output_filename_first_time = 0;
      num_source_filenames += 1;
      current_function_file = name;
      fprintf (stream, "\t.file\t%d ", num_source_filenames);
      output_quoted_string (stream, name);
      putc ('\n', stream);
    }
  /* If we are emitting stabs, let dbxout.c handle this (except for
     the mips_output_filename_first_time case).  */
  else if (write_symbols == DBX_DEBUG)
    return;
  else if (name != current_function_file
	   && strcmp (name, current_function_file) != 0)
    {
      num_source_filenames += 1;
      current_function_file = name;
      fprintf (stream, "\t.file\t%d ", num_source_filenames);
      output_quoted_string (stream, name);
      putc ('\n', stream);
    }
}

/* Implement TARGET_ASM_OUTPUT_DWARF_DTPREL.  */

static void ATTRIBUTE_UNUSED
mips_output_dwarf_dtprel (FILE *file, int size, rtx x)
{
  switch (size)
    {
    case 4:
      fputs ("\t.dtprelword\t", file);
      break;

    case 8:
      fputs ("\t.dtpreldword\t", file);
      break;

    default:
      gcc_unreachable ();
    }
  output_addr_const (file, x);
  fputs ("+0x8000", file);
}

/* Implement TARGET_DWARF_REGISTER_SPAN.  */

static rtx
mips_dwarf_register_span (rtx reg)
{
  rtx high, low;
  enum machine_mode mode;

  /* By default, GCC maps increasing register numbers to increasing
     memory locations, but paired FPRs are always little-endian,
     regardless of the prevailing endianness.  */
  mode = GET_MODE (reg);
  if (FP_REG_P (REGNO (reg))
      && TARGET_BIG_ENDIAN
      && MAX_FPRS_PER_FMT > 1
      && GET_MODE_SIZE (mode) > UNITS_PER_FPREG)
    {
      gcc_assert (GET_MODE_SIZE (mode) == UNITS_PER_HWFPVALUE);
      high = mips_subword (reg, true);
      low = mips_subword (reg, false);
      return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, high, low));
    }

  return NULL_RTX;
}

/* Implement ASM_OUTPUT_ASCII.  */

void
mips_output_ascii (FILE *stream, const char *string, size_t len)
{
  size_t i;
  int cur_pos;

  cur_pos = 17;
  fprintf (stream, "\t.ascii\t\"");
  for (i = 0; i < len; i++)
    {
      int c;

      c = (unsigned char) string[i];
      if (ISPRINT (c))
	{
	  if (c == '\\' || c == '\"')
	    {
	      putc ('\\', stream);
	      cur_pos++;
	    }
	  putc (c, stream);
	  cur_pos++;
	}
      else
	{
	  fprintf (stream, "\\%03o", c);
	  cur_pos += 4;
	}

      if (cur_pos > 72 && i+1 < len)
	{
	  cur_pos = 17;
	  fprintf (stream, "\"\n\t.ascii\t\"");
	}
    }
  fprintf (stream, "\"\n");
}

/* Emit either a label, .comm, or .lcomm directive.  When using assembler
   macros, mark the symbol as written so that mips_asm_output_external
   won't emit an .extern for it.  STREAM is the output file, NAME is the
   name of the symbol, INIT_STRING is the string that should be written
   before the symbol and FINAL_STRING is the string that should be
   written after it.  FINAL_STRING is a printf format that consumes the
   remaining arguments.  */

void
mips_declare_object (FILE *stream, const char *name, const char *init_string,
		     const char *final_string, ...)
{
  va_list ap;

  fputs (init_string, stream);
  assemble_name (stream, name);
  va_start (ap, final_string);
  vfprintf (stream, final_string, ap);
  va_end (ap);

  if (!TARGET_EXPLICIT_RELOCS)
    {
      tree name_tree = get_identifier (name);
      TREE_ASM_WRITTEN (name_tree) = 1;
    }
}

/* Declare a common object of SIZE bytes using asm directive INIT_STRING.
   NAME is the name of the object and ALIGN is the required alignment
   in bytes.  TAKES_ALIGNMENT_P is true if the directive takes a third
   alignment argument.  */

void
mips_declare_common_object (FILE *stream, const char *name,
			    const char *init_string,
			    unsigned HOST_WIDE_INT size,
			    unsigned int align, bool takes_alignment_p)
{
  if (!takes_alignment_p)
    {
      size += (align / BITS_PER_UNIT) - 1;
      size -= size % (align / BITS_PER_UNIT);
      mips_declare_object (stream, name, init_string,
			   "," HOST_WIDE_INT_PRINT_UNSIGNED "\n", size);
    }
  else
    mips_declare_object (stream, name, init_string,
			 "," HOST_WIDE_INT_PRINT_UNSIGNED ",%u\n",
			 size, align / BITS_PER_UNIT);
}

/* Implement ASM_OUTPUT_ALIGNED_DECL_COMMON.  This is usually the same as the
   elfos.h version, but we also need to handle -muninit-const-in-rodata.  */

void
mips_output_aligned_decl_common (FILE *stream, tree decl, const char *name,
				 unsigned HOST_WIDE_INT size,
				 unsigned int align)
{
  /* If the target wants uninitialized const declarations in
     .rdata then don't put them in .comm.  */
  if (TARGET_EMBEDDED_DATA
      && TARGET_UNINIT_CONST_IN_RODATA
      && TREE_CODE (decl) == VAR_DECL
      && TREE_READONLY (decl)
      && (DECL_INITIAL (decl) == 0 || DECL_INITIAL (decl) == error_mark_node))
    {
      if (TREE_PUBLIC (decl) && DECL_NAME (decl))
	targetm.asm_out.globalize_label (stream, name);

      switch_to_section (readonly_data_section);
      ASM_OUTPUT_ALIGN (stream, floor_log2 (align / BITS_PER_UNIT));
      mips_declare_object (stream, name, "",
			   ":\n\t.space\t" HOST_WIDE_INT_PRINT_UNSIGNED "\n",
			   size);
    }
  else
    mips_declare_common_object (stream, name, "\n\t.comm\t",
				size, align, true);
}

#ifdef ASM_OUTPUT_SIZE_DIRECTIVE
extern int size_directive_output;

/* Implement ASM_DECLARE_OBJECT_NAME.  This is like most of the standard ELF
   definitions except that it uses mips_declare_object to emit the label.  */

void
mips_declare_object_name (FILE *stream, const char *name,
			  tree decl ATTRIBUTE_UNUSED)
{
#ifdef ASM_OUTPUT_TYPE_DIRECTIVE
  ASM_OUTPUT_TYPE_DIRECTIVE (stream, name, "object");
#endif

  size_directive_output = 0;
  if (!flag_inhibit_size_directive && DECL_SIZE (decl))
    {
      HOST_WIDE_INT size;

      size_directive_output = 1;
      size = int_size_in_bytes (TREE_TYPE (decl));
      ASM_OUTPUT_SIZE_DIRECTIVE (stream, name, size);
    }

  mips_declare_object (stream, name, "", ":\n");
}

/* Implement ASM_FINISH_DECLARE_OBJECT.  This is generic ELF stuff.  */

void
mips_finish_declare_object (FILE *stream, tree decl, int top_level, int at_end)
{
  const char *name;

  name = XSTR (XEXP (DECL_RTL (decl), 0), 0);
  if (!flag_inhibit_size_directive
      && DECL_SIZE (decl) != 0
      && !at_end
      && top_level
      && DECL_INITIAL (decl) == error_mark_node
      && !size_directive_output)
    {
      HOST_WIDE_INT size;

      size_directive_output = 1;
      size = int_size_in_bytes (TREE_TYPE (decl));
      ASM_OUTPUT_SIZE_DIRECTIVE (stream, name, size);
    }
}
#endif

/* Return the FOO in the name of the ".mdebug.FOO" section associated
   with the current ABI.  */

static const char *
mips_mdebug_abi_name (void)
{
  switch (mips_abi)
    {
    case ABI_32:
      return "abi32";
    case ABI_O64:
      return "abiO64";
    case ABI_N32:
      return "abiN32";
    case ABI_64:
      return "abi64";
    case ABI_EABI:
      return TARGET_64BIT ? "eabi64" : "eabi32";
    default:
      gcc_unreachable ();
    }
}

/* Implement TARGET_ASM_FILE_START.  */

static void
mips_file_start (void)
{
  default_file_start ();

  /* Generate a special section to describe the ABI switches used to
     produce the resultant binary.  This is unnecessary on IRIX and
     causes unwanted warnings from the native linker.  */
  if (!TARGET_IRIX6)
    {
      /* Record the ABI itself.  Modern versions of binutils encode
	 this information in the ELF header flags, but GDB needs the
	 information in order to correctly debug binaries produced by
	 older binutils.  See the function mips_gdbarch_init in
	 gdb/mips-tdep.c.  */
      fprintf (asm_out_file, "\t.section .mdebug.%s\n\t.previous\n",
	       mips_mdebug_abi_name ());

      /* There is no ELF header flag to distinguish long32 forms of the
	 EABI from long64 forms.  Emit a special section to help tools
	 such as GDB.  Do the same for o64, which is sometimes used with
	 -mlong64.  */
      if (mips_abi == ABI_EABI || mips_abi == ABI_O64)
	fprintf (asm_out_file, "\t.section .gcc_compiled_long%d\n"
		 "\t.previous\n", TARGET_LONG64 ? 64 : 32);

#ifdef HAVE_AS_GNU_ATTRIBUTE
      {
	int attr;

	/* No floating-point operations, -mno-float.  */
	if (TARGET_NO_FLOAT)
	  attr = 0;
	/* Soft-float code, -msoft-float.  */
	else if (!TARGET_HARD_FLOAT_ABI)
	  attr = 3;
	/* Single-float code, -msingle-float.  */
	else if (!TARGET_DOUBLE_FLOAT)
	  attr = 2;
	/* 64-bit FP registers on a 32-bit target, -mips32r2 -mfp64.  */
	else if (!TARGET_64BIT && TARGET_FLOAT64)
	  attr = 4;
	/* Regular FP code, FP regs same size as GP regs, -mdouble-float.  */
	else
	  attr = 1;

	fprintf (asm_out_file, "\t.gnu_attribute 4, %d\n", attr);
      }
#endif
    }

  /* If TARGET_ABICALLS, tell GAS to generate -KPIC code.  */
  if (TARGET_ABICALLS)
    {
      fprintf (asm_out_file, "\t.abicalls\n");
      if (TARGET_ABICALLS_PIC0)
	fprintf (asm_out_file, "\t.option\tpic0\n");
    }

  if (flag_verbose_asm)
    fprintf (asm_out_file, "\n%s -G value = %d, Arch = %s, ISA = %d\n",
	     ASM_COMMENT_START,
	     mips_small_data_threshold, mips_arch_info->name, mips_isa);
}

/* Make the last instruction frame-related and note that it performs
   the operation described by FRAME_PATTERN.  */

static void
mips_set_frame_expr (rtx frame_pattern)
{
  rtx insn;

  insn = get_last_insn ();
  RTX_FRAME_RELATED_P (insn) = 1;
  REG_NOTES (insn) = alloc_EXPR_LIST (REG_FRAME_RELATED_EXPR,
				      frame_pattern,
				      REG_NOTES (insn));
}

/* Return a frame-related rtx that stores REG at MEM.
   REG must be a single register.  */

static rtx
mips_frame_set (rtx mem, rtx reg)
{
  rtx set;

  /* If we're saving the return address register and the DWARF return
     address column differs from the hard register number, adjust the
     note reg to refer to the former.  */
  if (REGNO (reg) == RETURN_ADDR_REGNUM
      && DWARF_FRAME_RETURN_COLUMN != RETURN_ADDR_REGNUM)
    reg = gen_rtx_REG (GET_MODE (reg), DWARF_FRAME_RETURN_COLUMN);

  set = gen_rtx_SET (VOIDmode, mem, reg);
  RTX_FRAME_RELATED_P (set) = 1;

  return set;
}

/* If a MIPS16e SAVE or RESTORE instruction saves or restores register
   mips16e_s2_s8_regs[X], it must also save the registers in indexes
   X + 1 onwards.  Likewise mips16e_a0_a3_regs.  */
static const unsigned char mips16e_s2_s8_regs[] = {
  30, 23, 22, 21, 20, 19, 18
};
static const unsigned char mips16e_a0_a3_regs[] = {
  4, 5, 6, 7
};

/* A list of the registers that can be saved by the MIPS16e SAVE instruction,
   ordered from the uppermost in memory to the lowest in memory.  */
static const unsigned char mips16e_save_restore_regs[] = {
  31, 30, 23, 22, 21, 20, 19, 18, 17, 16, 7, 6, 5, 4
};

/* Return the index of the lowest X in the range [0, SIZE) for which
   bit REGS[X] is set in MASK.  Return SIZE if there is no such X.  */

static unsigned int
mips16e_find_first_register (unsigned int mask, const unsigned char *regs,
			     unsigned int size)
{
  unsigned int i;

  for (i = 0; i < size; i++)
    if (BITSET_P (mask, regs[i]))
      break;

  return i;
}

/* *MASK_PTR is a mask of general-purpose registers and *NUM_REGS_PTR
   is the number of set bits.  If *MASK_PTR contains REGS[X] for some X
   in [0, SIZE), adjust *MASK_PTR and *NUM_REGS_PTR so that the same
   is true for all indexes (X, SIZE).  */

static void
mips16e_mask_registers (unsigned int *mask_ptr, const unsigned char *regs,
			unsigned int size, unsigned int *num_regs_ptr)
{
  unsigned int i;

  i = mips16e_find_first_register (*mask_ptr, regs, size);
  for (i++; i < size; i++)
    if (!BITSET_P (*mask_ptr, regs[i]))
      {
	*num_regs_ptr += 1;
	*mask_ptr |= 1 << regs[i];
      }
}

/* Return a simplified form of X using the register values in REG_VALUES.
   REG_VALUES[R] is the last value assigned to hard register R, or null
   if R has not been modified.

   This function is rather limited, but is good enough for our purposes.  */

static rtx
mips16e_collect_propagate_value (rtx x, rtx *reg_values)
{
  x = avoid_constant_pool_reference (x);

  if (UNARY_P (x))
    {
      rtx x0 = mips16e_collect_propagate_value (XEXP (x, 0), reg_values);
      return simplify_gen_unary (GET_CODE (x), GET_MODE (x),
				 x0, GET_MODE (XEXP (x, 0)));
    }

  if (ARITHMETIC_P (x))
    {
      rtx x0 = mips16e_collect_propagate_value (XEXP (x, 0), reg_values);
      rtx x1 = mips16e_collect_propagate_value (XEXP (x, 1), reg_values);
      return simplify_gen_binary (GET_CODE (x), GET_MODE (x), x0, x1);
    }

  if (REG_P (x)
      && reg_values[REGNO (x)]
      && !rtx_unstable_p (reg_values[REGNO (x)]))
    return reg_values[REGNO (x)];

  return x;
}

/* Return true if (set DEST SRC) stores an argument register into its
   caller-allocated save slot, storing the number of that argument
   register in *REGNO_PTR if so.  REG_VALUES is as for
   mips16e_collect_propagate_value.  */

static bool
mips16e_collect_argument_save_p (rtx dest, rtx src, rtx *reg_values,
				 unsigned int *regno_ptr)
{
  unsigned int argno, regno;
  HOST_WIDE_INT offset, required_offset;
  rtx addr, base;

  /* Check that this is a word-mode store.  */
  if (!MEM_P (dest) || !REG_P (src) || GET_MODE (dest) != word_mode)
    return false;

  /* Check that the register being saved is an unmodified argument
     register.  */
  regno = REGNO (src);
  if (!IN_RANGE (regno, GP_ARG_FIRST, GP_ARG_LAST) || reg_values[regno])
    return false;
  argno = regno - GP_ARG_FIRST;

  /* Check whether the address is an appropriate stack-pointer or
     frame-pointer access.  */
  addr = mips16e_collect_propagate_value (XEXP (dest, 0), reg_values);
  mips_split_plus (addr, &base, &offset);
  required_offset = cfun->machine->frame.total_size + argno * UNITS_PER_WORD;
  if (base == hard_frame_pointer_rtx)
    required_offset -= cfun->machine->frame.hard_frame_pointer_offset;
  else if (base != stack_pointer_rtx)
    return false;
  if (offset != required_offset)
    return false;

  *regno_ptr = regno;
  return true;
}

/* A subroutine of mips_expand_prologue, called only when generating
   MIPS16e SAVE instructions.  Search the start of the function for any
   instructions that save argument registers into their caller-allocated
   save slots.  Delete such instructions and return a value N such that
   saving [GP_ARG_FIRST, GP_ARG_FIRST + N) would make all the deleted
   instructions redundant.  */

static unsigned int
mips16e_collect_argument_saves (void)
{
  rtx reg_values[FIRST_PSEUDO_REGISTER];
  rtx insn, next, set, dest, src;
  unsigned int nargs, regno;

  push_topmost_sequence ();
  nargs = 0;
  memset (reg_values, 0, sizeof (reg_values));
  for (insn = get_insns (); insn; insn = next)
    {
      next = NEXT_INSN (insn);
      if (NOTE_P (insn) || DEBUG_INSN_P (insn))
	continue;

      if (!INSN_P (insn))
	break;

      set = PATTERN (insn);
      if (GET_CODE (set) != SET)
	break;

      dest = SET_DEST (set);
      src = SET_SRC (set);
      if (mips16e_collect_argument_save_p (dest, src, reg_values, &regno))
	{
	  if (!BITSET_P (cfun->machine->frame.mask, regno))
	    {
	      delete_insn (insn);
	      nargs = MAX (nargs, (regno - GP_ARG_FIRST) + 1);
	    }
	}
      else if (REG_P (dest) && GET_MODE (dest) == word_mode)
	reg_values[REGNO (dest)]
	  = mips16e_collect_propagate_value (src, reg_values);
      else
	break;
    }
  pop_topmost_sequence ();

  return nargs;
}

/* Return a move between register REGNO and memory location SP + OFFSET.
   Make the move a load if RESTORE_P, otherwise make it a frame-related
   store.  */

static rtx
mips16e_save_restore_reg (bool restore_p, HOST_WIDE_INT offset,
			  unsigned int regno)
{
  rtx reg, mem;

  mem = gen_frame_mem (SImode, plus_constant (stack_pointer_rtx, offset));
  reg = gen_rtx_REG (SImode, regno);
  return (restore_p
	  ? gen_rtx_SET (VOIDmode, reg, mem)
	  : mips_frame_set (mem, reg));
}

/* Return RTL for a MIPS16e SAVE or RESTORE instruction; RESTORE_P says which.
   The instruction must:

     - Allocate or deallocate SIZE bytes in total; SIZE is known
       to be nonzero.

     - Save or restore as many registers in *MASK_PTR as possible.
       The instruction saves the first registers at the top of the
       allocated area, with the other registers below it.

     - Save NARGS argument registers above the allocated area.

   (NARGS is always zero if RESTORE_P.)

   The SAVE and RESTORE instructions cannot save and restore all general
   registers, so there may be some registers left over for the caller to
   handle.  Destructively modify *MASK_PTR so that it contains the registers
   that still need to be saved or restored.  The caller can save these
   registers in the memory immediately below *OFFSET_PTR, which is a
   byte offset from the bottom of the allocated stack area.  */

static rtx
mips16e_build_save_restore (bool restore_p, unsigned int *mask_ptr,
			    HOST_WIDE_INT *offset_ptr, unsigned int nargs,
			    HOST_WIDE_INT size)
{
  rtx pattern, set;
  HOST_WIDE_INT offset, top_offset;
  unsigned int i, regno;
  int n;

  gcc_assert (cfun->machine->frame.num_fp == 0);

  /* Calculate the number of elements in the PARALLEL.  We need one element
     for the stack adjustment, one for each argument register save, and one
     for each additional register move.  */
  n = 1 + nargs;
  for (i = 0; i < ARRAY_SIZE (mips16e_save_restore_regs); i++)
    if (BITSET_P (*mask_ptr, mips16e_save_restore_regs[i]))
      n++;

  /* Create the final PARALLEL.  */
  pattern = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (n));
  n = 0;

  /* Add the stack pointer adjustment.  */
  set = gen_rtx_SET (VOIDmode, stack_pointer_rtx,
		     plus_constant (stack_pointer_rtx,
				    restore_p ? size : -size));
  RTX_FRAME_RELATED_P (set) = 1;
  XVECEXP (pattern, 0, n++) = set;

  /* Stack offsets in the PARALLEL are relative to the old stack pointer.  */
  top_offset = restore_p ? size : 0;

  /* Save the arguments.  */
  for (i = 0; i < nargs; i++)
    {
      offset = top_offset + i * UNITS_PER_WORD;
      set = mips16e_save_restore_reg (restore_p, offset, GP_ARG_FIRST + i);
      XVECEXP (pattern, 0, n++) = set;
    }

  /* Then fill in the other register moves.  */
  offset = top_offset;
  for (i = 0; i < ARRAY_SIZE (mips16e_save_restore_regs); i++)
    {
      regno = mips16e_save_restore_regs[i];
      if (BITSET_P (*mask_ptr, regno))
	{
	  offset -= UNITS_PER_WORD;
	  set = mips16e_save_restore_reg (restore_p, offset, regno);
	  XVECEXP (pattern, 0, n++) = set;
	  *mask_ptr &= ~(1 << regno);
	}
    }

  /* Tell the caller what offset it should use for the remaining registers.  */
  *offset_ptr = size + (offset - top_offset);

  gcc_assert (n == XVECLEN (pattern, 0));

  return pattern;
}

/* PATTERN is a PARALLEL whose first element adds ADJUST to the stack
   pointer.  Return true if PATTERN matches the kind of instruction
   generated by mips16e_build_save_restore.  If INFO is nonnull,
   initialize it when returning true.  */

bool
mips16e_save_restore_pattern_p (rtx pattern, HOST_WIDE_INT adjust,
				struct mips16e_save_restore_info *info)
{
  unsigned int i, nargs, mask, extra;
  HOST_WIDE_INT top_offset, save_offset, offset;
  rtx set, reg, mem, base;
  int n;

  if (!GENERATE_MIPS16E_SAVE_RESTORE)
    return false;

  /* Stack offsets in the PARALLEL are relative to the old stack pointer.  */
  top_offset = adjust > 0 ? adjust : 0;

  /* Interpret all other members of the PARALLEL.  */
  save_offset = top_offset - UNITS_PER_WORD;
  mask = 0;
  nargs = 0;
  i = 0;
  for (n = 1; n < XVECLEN (pattern, 0); n++)
    {
      /* Check that we have a SET.  */
      set = XVECEXP (pattern, 0, n);
      if (GET_CODE (set) != SET)
	return false;

      /* Check that the SET is a load (if restoring) or a store
	 (if saving).  */
      mem = adjust > 0 ? SET_SRC (set) : SET_DEST (set);
      if (!MEM_P (mem))
	return false;

      /* Check that the address is the sum of the stack pointer and a
	 possibly-zero constant offset.  */
      mips_split_plus (XEXP (mem, 0), &base, &offset);
      if (base != stack_pointer_rtx)
	return false;

      /* Check that SET's other operand is a register.  */
      reg = adjust > 0 ? SET_DEST (set) : SET_SRC (set);
      if (!REG_P (reg))
	return false;

      /* Check for argument saves.  */
      if (offset == top_offset + nargs * UNITS_PER_WORD
	  && REGNO (reg) == GP_ARG_FIRST + nargs)
	nargs++;
      else if (offset == save_offset)
	{
	  while (mips16e_save_restore_regs[i++] != REGNO (reg))
	    if (i == ARRAY_SIZE (mips16e_save_restore_regs))
	      return false;

	  mask |= 1 << REGNO (reg);
	  save_offset -= UNITS_PER_WORD;
	}
      else
	return false;
    }

  /* Check that the restrictions on register ranges are met.  */
  extra = 0;
  mips16e_mask_registers (&mask, mips16e_s2_s8_regs,
			  ARRAY_SIZE (mips16e_s2_s8_regs), &extra);
  mips16e_mask_registers (&mask, mips16e_a0_a3_regs,
			  ARRAY_SIZE (mips16e_a0_a3_regs), &extra);
  if (extra != 0)
    return false;

  /* Make sure that the topmost argument register is not saved twice.
     The checks above ensure that the same is then true for the other
     argument registers.  */
  if (nargs > 0 && BITSET_P (mask, GP_ARG_FIRST + nargs - 1))
    return false;

  /* Pass back information, if requested.  */
  if (info)
    {
      info->nargs = nargs;
      info->mask = mask;
      info->size = (adjust > 0 ? adjust : -adjust);
    }

  return true;
}

/* Add a MIPS16e SAVE or RESTORE register-range argument to string S
   for the register range [MIN_REG, MAX_REG].  Return a pointer to
   the null terminator.  */

static char *
mips16e_add_register_range (char *s, unsigned int min_reg,
			    unsigned int max_reg)
{
  if (min_reg != max_reg)
    s += sprintf (s, ",%s-%s", reg_names[min_reg], reg_names[max_reg]);
  else
    s += sprintf (s, ",%s", reg_names[min_reg]);
  return s;
}

/* Return the assembly instruction for a MIPS16e SAVE or RESTORE instruction.
   PATTERN and ADJUST are as for mips16e_save_restore_pattern_p.  */

const char *
mips16e_output_save_restore (rtx pattern, HOST_WIDE_INT adjust)
{
  static char buffer[300];

  struct mips16e_save_restore_info info;
  unsigned int i, end;
  char *s;

  /* Parse the pattern.  */
  if (!mips16e_save_restore_pattern_p (pattern, adjust, &info))
    gcc_unreachable ();

  /* Add the mnemonic.  */
  s = strcpy (buffer, adjust > 0 ? "restore\t" : "save\t");
  s += strlen (s);

  /* Save the arguments.  */
  if (info.nargs > 1)
    s += sprintf (s, "%s-%s,", reg_names[GP_ARG_FIRST],
		  reg_names[GP_ARG_FIRST + info.nargs - 1]);
  else if (info.nargs == 1)
    s += sprintf (s, "%s,", reg_names[GP_ARG_FIRST]);

  /* Emit the amount of stack space to allocate or deallocate.  */
  s += sprintf (s, "%d", (int) info.size);

  /* Save or restore $16.  */
  if (BITSET_P (info.mask, 16))
    s += sprintf (s, ",%s", reg_names[GP_REG_FIRST + 16]);

  /* Save or restore $17.  */
  if (BITSET_P (info.mask, 17))
    s += sprintf (s, ",%s", reg_names[GP_REG_FIRST + 17]);

  /* Save or restore registers in the range $s2...$s8, which
     mips16e_s2_s8_regs lists in decreasing order.  Note that this
     is a software register range; the hardware registers are not
     numbered consecutively.  */
  end = ARRAY_SIZE (mips16e_s2_s8_regs);
  i = mips16e_find_first_register (info.mask, mips16e_s2_s8_regs, end);
  if (i < end)
    s = mips16e_add_register_range (s, mips16e_s2_s8_regs[end - 1],
				    mips16e_s2_s8_regs[i]);

  /* Save or restore registers in the range $a0...$a3.  */
  end = ARRAY_SIZE (mips16e_a0_a3_regs);
  i = mips16e_find_first_register (info.mask, mips16e_a0_a3_regs, end);
  if (i < end)
    s = mips16e_add_register_range (s, mips16e_a0_a3_regs[i],
				    mips16e_a0_a3_regs[end - 1]);

  /* Save or restore $31.  */
  if (BITSET_P (info.mask, RETURN_ADDR_REGNUM))
    s += sprintf (s, ",%s", reg_names[RETURN_ADDR_REGNUM]);

  return buffer;
}

/* Return true if the current function returns its value in a floating-point
   register in MIPS16 mode.  */

static bool
mips16_cfun_returns_in_fpr_p (void)
{
  tree return_type = DECL_RESULT (current_function_decl);
  return (TARGET_MIPS16
	  && TARGET_HARD_FLOAT_ABI
	  && !aggregate_value_p (return_type, current_function_decl)
 	  && mips_return_mode_in_fpr_p (DECL_MODE (return_type)));
}

/* Return true if predicate PRED is true for at least one instruction.
   Cache the result in *CACHE, and assume that the result is true
   if *CACHE is already true.  */

static bool
mips_find_gp_ref (bool *cache, bool (*pred) (rtx))
{
  rtx insn;

  if (!*cache)
    {
      push_topmost_sequence ();
      for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
	if (USEFUL_INSN_P (insn) && pred (insn))
	  {
	    *cache = true;
	    break;
	  }
      pop_topmost_sequence ();
    }
  return *cache;
}

/* Return true if INSN refers to the global pointer in an "inflexible" way.
   See mips_cfun_has_inflexible_gp_ref_p for details.  */

static bool
mips_insn_has_inflexible_gp_ref_p (rtx insn)
{
  /* Uses of pic_offset_table_rtx in CALL_INSN_FUNCTION_USAGE
     indicate that the target could be a traditional MIPS
     lazily-binding stub.  */
  return find_reg_fusage (insn, USE, pic_offset_table_rtx);
}

/* Return true if the current function refers to the global pointer
   in a way that forces $28 to be valid.  This means that we can't
   change the choice of global pointer, even for NewABI code.

   One example of this (and one which needs several checks) is that
   $28 must be valid when calling traditional MIPS lazy-binding stubs.
   (This restriction does not apply to PLTs.)  */

static bool
mips_cfun_has_inflexible_gp_ref_p (void)
{
  /* If the function has a nonlocal goto, $28 must hold the correct
     global pointer for the target function.  That is, the target
     of the goto implicitly uses $28.  */
  if (crtl->has_nonlocal_goto)
    return true;

  if (TARGET_ABICALLS_PIC2)
    {
      /* Symbolic accesses implicitly use the global pointer unless
	 -mexplicit-relocs is in effect.  JAL macros to symbolic addresses
	 might go to traditional MIPS lazy-binding stubs.  */
      if (!TARGET_EXPLICIT_RELOCS)
	return true;

      /* FUNCTION_PROFILER includes a JAL to _mcount, which again
	 can be lazily-bound.  */
      if (crtl->profile)
	return true;

      /* MIPS16 functions that return in FPRs need to call an
	 external libgcc routine.  This call is only made explict
	 during mips_expand_epilogue, and it too might be lazily bound.  */
      if (mips16_cfun_returns_in_fpr_p ())
	return true;
    }

  return mips_find_gp_ref (&cfun->machine->has_inflexible_gp_insn_p,
			   mips_insn_has_inflexible_gp_ref_p);
}

/* Return true if INSN refers to the global pointer in a "flexible" way.
   See mips_cfun_has_flexible_gp_ref_p for details.  */

static bool
mips_insn_has_flexible_gp_ref_p (rtx insn)
{
  return (get_attr_got (insn) != GOT_UNSET
	  || mips_small_data_pattern_p (PATTERN (insn))
	  || reg_overlap_mentioned_p (pic_offset_table_rtx, PATTERN (insn)));
}

/* Return true if the current function references the global pointer,
   but if those references do not inherently require the global pointer
   to be $28.  Assume !mips_cfun_has_inflexible_gp_ref_p ().  */

static bool
mips_cfun_has_flexible_gp_ref_p (void)
{
  /* Reload can sometimes introduce constant pool references
     into a function that otherwise didn't need them.  For example,
     suppose we have an instruction like:

	(set (reg:DF R1) (float:DF (reg:SI R2)))

     If R2 turns out to be a constant such as 1, the instruction may
     have a REG_EQUAL note saying that R1 == 1.0.  Reload then has
     the option of using this constant if R2 doesn't get allocated
     to a register.

     In cases like these, reload will have added the constant to the
     pool but no instruction will yet refer to it.  */
  if (TARGET_ABICALLS_PIC2 && !reload_completed && crtl->uses_const_pool)
    return true;

  return mips_find_gp_ref (&cfun->machine->has_flexible_gp_insn_p,
			   mips_insn_has_flexible_gp_ref_p);
}

/* Return the register that should be used as the global pointer
   within this function.  Return INVALID_REGNUM if the function
   doesn't need a global pointer.  */

static unsigned int
mips_global_pointer (void)
{
  unsigned int regno;

  /* $gp is always available unless we're using a GOT.  */
  if (!TARGET_USE_GOT)
    return GLOBAL_POINTER_REGNUM;

  /* If there are inflexible references to $gp, we must use the
     standard register.  */
  if (mips_cfun_has_inflexible_gp_ref_p ())
    return GLOBAL_POINTER_REGNUM;

  /* If there are no current references to $gp, then the only uses
     we can introduce later are those involved in long branches.  */
  if (TARGET_ABSOLUTE_JUMPS && !mips_cfun_has_flexible_gp_ref_p ())
    return INVALID_REGNUM;

  /* If the global pointer is call-saved, try to use a call-clobbered
     alternative.  */
  if (TARGET_CALL_SAVED_GP && current_function_is_leaf)
    for (regno = GP_REG_FIRST; regno <= GP_REG_LAST; regno++)
      if (!df_regs_ever_live_p (regno)
	  && call_really_used_regs[regno]
	  && !fixed_regs[regno]
	  && regno != PIC_FUNCTION_ADDR_REGNUM)
	return regno;

  return GLOBAL_POINTER_REGNUM;
}

/* Return true if the current function's prologue must load the global
   pointer value into pic_offset_table_rtx and store the same value in
   the function's cprestore slot (if any).

   One problem we have to deal with is that, when emitting GOT-based
   position independent code, long-branch sequences will need to load
   the address of the branch target from the GOT.  We don't know until
   the very end of compilation whether (and where) the function needs
   long branches, so we must ensure that _any_ branch can access the
   global pointer in some form.  However, we do not want to pessimize
   the usual case in which all branches are short.

   We handle this as follows:

   (1) During reload, we set cfun->machine->global_pointer to
       INVALID_REGNUM if we _know_ that the current function
       doesn't need a global pointer.  This is only valid if
       long branches don't need the GOT.

       Otherwise, we assume that we might need a global pointer
       and pick an appropriate register.

   (2) If cfun->machine->global_pointer != INVALID_REGNUM,
       we ensure that the global pointer is available at every
       block boundary bar entry and exit.  We do this in one of two ways:

       - If the function has a cprestore slot, we ensure that this
	 slot is valid at every branch.  However, as explained in
	 point (6) below, there is no guarantee that pic_offset_table_rtx
	 itself is valid if new uses of the global pointer are introduced
	 after the first post-epilogue split.

	 We guarantee that the cprestore slot is valid by loading it
	 into a fake register, CPRESTORE_SLOT_REGNUM.  We then make
	 this register live at every block boundary bar function entry
	 and exit.  It is then invalid to move the load (and thus the
	 preceding store) across a block boundary.

       - If the function has no cprestore slot, we guarantee that
	 pic_offset_table_rtx itself is valid at every branch.

       See mips_eh_uses for the handling of the register liveness.

   (3) During prologue and epilogue generation, we emit "ghost"
       placeholder instructions to manipulate the global pointer.

   (4) During prologue generation, we set cfun->machine->must_initialize_gp_p
       and cfun->machine->must_restore_gp_when_clobbered_p if we already know
       that the function needs a global pointer.  (There is no need to set
       them earlier than this, and doing it as late as possible leads to
       fewer false positives.)

   (5) If cfun->machine->must_initialize_gp_p is true during a
       split_insns pass, we split the ghost instructions into real
       instructions.  These split instructions can then be optimized in
       the usual way.  Otherwise, we keep the ghost instructions intact,
       and optimize for the case where they aren't needed.  We still
       have the option of splitting them later, if we need to introduce
       new uses of the global pointer.

       For example, the scheduler ignores a ghost instruction that
       stores $28 to the stack, but it handles the split form of
       the ghost instruction as an ordinary store.

   (6) [OldABI only.]  If cfun->machine->must_restore_gp_when_clobbered_p
       is true during the first post-epilogue split_insns pass, we split
       calls and restore_gp patterns into instructions that explicitly
       load pic_offset_table_rtx from the cprestore slot.  Otherwise,
       we split these patterns into instructions that _don't_ load from
       the cprestore slot.

       If cfun->machine->must_restore_gp_when_clobbered_p is true at the
       time of the split, then any instructions that exist at that time
       can make free use of pic_offset_table_rtx.  However, if we want
       to introduce new uses of the global pointer after the split,
       we must explicitly load the value from the cprestore slot, since
       pic_offset_table_rtx itself might not be valid at a given point
       in the function.

       The idea is that we want to be able to delete redundant
       loads from the cprestore slot in the usual case where no
       long branches are needed.

   (7) If cfun->machine->must_initialize_gp_p is still false at the end
       of md_reorg, we decide whether the global pointer is needed for
       long branches.  If so, we set cfun->machine->must_initialize_gp_p
       to true and split the ghost instructions into real instructions
       at that stage.

   Note that the ghost instructions must have a zero length for three reasons:

   - Giving the length of the underlying $gp sequence might cause
     us to use long branches in cases where they aren't really needed.

   - They would perturb things like alignment calculations.

   - More importantly, the hazard detection in md_reorg relies on
     empty instructions having a zero length.

   If we find a long branch and split the ghost instructions at the
   end of md_reorg, the split could introduce more long branches.
   That isn't a problem though, because we still do the split before
   the final shorten_branches pass.

   This is extremely ugly, but it seems like the best compromise between
   correctness and efficiency.  */

bool
mips_must_initialize_gp_p (void)
{
  return cfun->machine->must_initialize_gp_p;
}

/* Return true if REGNO is a register that is ordinarily call-clobbered
   but must nevertheless be preserved by an interrupt handler.  */

static bool
mips_interrupt_extra_call_saved_reg_p (unsigned int regno)
{
  if (MD_REG_P (regno))
    return true;

  if (TARGET_DSP && DSP_ACC_REG_P (regno))
    return true;

  if (GP_REG_P (regno) && !cfun->machine->use_shadow_register_set_p)
    {
      /* $0 is hard-wired.  */
      if (regno == GP_REG_FIRST)
	return false;

      /* The interrupt handler can treat kernel registers as
	 scratch registers.  */
      if (KERNEL_REG_P (regno))
	return false;

      /* The function will return the stack pointer to its original value
	 anyway.  */
      if (regno == STACK_POINTER_REGNUM)
	return false;

      /* Otherwise, return true for registers that aren't ordinarily
	 call-clobbered.  */
      return call_really_used_regs[regno];
    }

  return false;
}

/* Return true if the current function should treat register REGNO
   as call-saved.  */

static bool
mips_cfun_call_saved_reg_p (unsigned int regno)
{
  /* If the user makes an ordinarily-call-saved register global,
     that register is no longer call-saved.  */
  if (global_regs[regno])
    return false;

  /* Interrupt handlers need to save extra registers.  */
  if (cfun->machine->interrupt_handler_p
      && mips_interrupt_extra_call_saved_reg_p (regno))
    return true;

  /* call_insns preserve $28 unless they explicitly say otherwise,
     so call_really_used_regs[] treats $28 as call-saved.  However,
     we want the ABI property rather than the default call_insn
     property here.  */
  return (regno == GLOBAL_POINTER_REGNUM
	  ? TARGET_CALL_SAVED_GP
	  : !call_really_used_regs[regno]);
}

/* Return true if the function body might clobber register REGNO.
   We know that REGNO is call-saved.  */

static bool
mips_cfun_might_clobber_call_saved_reg_p (unsigned int regno)
{
  /* Some functions should be treated as clobbering all call-saved
     registers.  */
  if (crtl->saves_all_registers)
    return true;

  /* DF handles cases where a register is explicitly referenced in
     the rtl.  Incoming values are passed in call-clobbered registers,
     so we can assume that any live call-saved register is set within
     the function.  */
  if (df_regs_ever_live_p (regno))
    return true;

  /* Check for registers that are clobbered by FUNCTION_PROFILER.
     These clobbers are not explicit in the rtl.  */
  if (crtl->profile && MIPS_SAVE_REG_FOR_PROFILING_P (regno))
    return true;

  /* If we're using a call-saved global pointer, the function's
     prologue will need to set it up.  */
  if (cfun->machine->global_pointer == regno)
    return true;

  /* The function's prologue will need to set the frame pointer if
     frame_pointer_needed.  */
  if (regno == HARD_FRAME_POINTER_REGNUM && frame_pointer_needed)
    return true;

  /* If a MIPS16 function returns a value in FPRs, its epilogue
     will need to call an external libgcc routine.  This yet-to-be
     generated call_insn will clobber $31.  */
  if (regno == RETURN_ADDR_REGNUM && mips16_cfun_returns_in_fpr_p ())
    return true;

  /* If REGNO is ordinarily call-clobbered, we must assume that any
     called function could modify it.  */
  if (cfun->machine->interrupt_handler_p
      && !current_function_is_leaf
      && mips_interrupt_extra_call_saved_reg_p (regno))
    return true;

  return false;
}

/* Return true if the current function must save register REGNO.  */

static bool
mips_save_reg_p (unsigned int regno)
{
  if (mips_cfun_call_saved_reg_p (regno))
    {
      if (mips_cfun_might_clobber_call_saved_reg_p (regno))
	return true;

      /* Save both registers in an FPR pair if either one is used.  This is
	 needed for the case when MIN_FPRS_PER_FMT == 1, which allows the odd
	 register to be used without the even register.  */
      if (FP_REG_P (regno)
	  && MAX_FPRS_PER_FMT == 2
	  && mips_cfun_might_clobber_call_saved_reg_p (regno + 1))
	return true;
    }

  /* We need to save the incoming return address if __builtin_eh_return
     is being used to set a different return address.  */
  if (regno == RETURN_ADDR_REGNUM && crtl->calls_eh_return)
    return true;

  return false;
}

/* Populate the current function's mips_frame_info structure.

   MIPS stack frames look like:

	+-------------------------------+
	|                               |
	|  incoming stack arguments     |
	|                               |
	+-------------------------------+
	|                               |
	|  caller-allocated save area   |
      A |  for register arguments       |
	|                               |
	+-------------------------------+ <-- incoming stack pointer
	|                               |
	|  callee-allocated save area   |
      B |  for arguments that are       |
	|  split between registers and  |
	|  the stack                    |
	|                               |
	+-------------------------------+ <-- arg_pointer_rtx
	|                               |
      C |  callee-allocated save area   |
	|  for register varargs         |
	|                               |
	+-------------------------------+ <-- frame_pointer_rtx
	|                               |       + cop0_sp_offset
	|  COP0 reg save area           |	+ UNITS_PER_WORD
	|                               |
	+-------------------------------+ <-- frame_pointer_rtx + acc_sp_offset
	|                               |       + UNITS_PER_WORD
	|  accumulator save area        |
	|                               |
	+-------------------------------+ <-- stack_pointer_rtx + fp_sp_offset
	|                               |       + UNITS_PER_HWFPVALUE
	|  FPR save area                |
	|                               |
	+-------------------------------+ <-- stack_pointer_rtx + gp_sp_offset
	|                               |       + UNITS_PER_WORD
	|  GPR save area                |
	|                               |
	+-------------------------------+ <-- frame_pointer_rtx with
	|                               | \     -fstack-protector
	|  local variables              |  | var_size
	|                               | /
	+-------------------------------+
	|                               | \
	|  $gp save area                |  | cprestore_size
	|                               | /
      P +-------------------------------+ <-- hard_frame_pointer_rtx for
	|                               | \     MIPS16 code
	|  outgoing stack arguments     |  |
	|                               |  |
	+-------------------------------+  | args_size
	|                               |  |
	|  caller-allocated save area   |  |
	|  for register arguments       |  |
	|                               | /
	+-------------------------------+ <-- stack_pointer_rtx
					      frame_pointer_rtx without
					        -fstack-protector
					      hard_frame_pointer_rtx for
						non-MIPS16 code.

   At least two of A, B and C will be empty.

   Dynamic stack allocations such as alloca insert data at point P.
   They decrease stack_pointer_rtx but leave frame_pointer_rtx and
   hard_frame_pointer_rtx unchanged.  */

static void
mips_compute_frame_info (void)
{
  struct mips_frame_info *frame;
  HOST_WIDE_INT offset, size;
  unsigned int regno, i;

  /* Set this function's interrupt properties.  */
  if (mips_interrupt_type_p (TREE_TYPE (current_function_decl)))
    {
      if (!ISA_MIPS32R2)
	error ("the %<interrupt%> attribute requires a MIPS32r2 processor");
      else if (TARGET_HARD_FLOAT)
	error ("the %<interrupt%> attribute requires %<-msoft-float%>");
      else if (TARGET_MIPS16)
	error ("interrupt handlers cannot be MIPS16 functions");
      else
	{
	  cfun->machine->interrupt_handler_p = true;
	  cfun->machine->use_shadow_register_set_p =
	    mips_use_shadow_register_set_p (TREE_TYPE (current_function_decl));
	  cfun->machine->keep_interrupts_masked_p =
	    mips_keep_interrupts_masked_p (TREE_TYPE (current_function_decl));
	  cfun->machine->use_debug_exception_return_p =
	    mips_use_debug_exception_return_p (TREE_TYPE
					       (current_function_decl));
	}
    }

  frame = &cfun->machine->frame;
  memset (frame, 0, sizeof (*frame));
  size = get_frame_size ();

  cfun->machine->global_pointer = mips_global_pointer ();

  /* The first two blocks contain the outgoing argument area and the $gp save
     slot.  This area isn't needed in leaf functions, but if the
     target-independent frame size is nonzero, we have already committed to
     allocating these in STARTING_FRAME_OFFSET for !FRAME_GROWS_DOWNWARD.  */
  if ((size == 0 || FRAME_GROWS_DOWNWARD) && current_function_is_leaf)
    {
      /* The MIPS 3.0 linker does not like functions that dynamically
	 allocate the stack and have 0 for STACK_DYNAMIC_OFFSET, since it
	 looks like we are trying to create a second frame pointer to the
	 function, so allocate some stack space to make it happy.  */
      if (cfun->calls_alloca)
	frame->args_size = REG_PARM_STACK_SPACE (cfun->decl);
      else
	frame->args_size = 0;
      frame->cprestore_size = 0;
    }
  else
    {
      frame->args_size = crtl->outgoing_args_size;
      frame->cprestore_size = MIPS_GP_SAVE_AREA_SIZE;
    }
  offset = frame->args_size + frame->cprestore_size;

  /* Move above the local variables.  */
  frame->var_size = MIPS_STACK_ALIGN (size);
  offset += frame->var_size;

  /* Find out which GPRs we need to save.  */
  for (regno = GP_REG_FIRST; regno <= GP_REG_LAST; regno++)
    if (mips_save_reg_p (regno))
      {
	frame->num_gp++;
	frame->mask |= 1 << (regno - GP_REG_FIRST);
      }

  /* If this function calls eh_return, we must also save and restore the
     EH data registers.  */
  if (crtl->calls_eh_return)
    for (i = 0; EH_RETURN_DATA_REGNO (i) != INVALID_REGNUM; i++)
      {
	frame->num_gp++;
	frame->mask |= 1 << (EH_RETURN_DATA_REGNO (i) - GP_REG_FIRST);
      }

  /* The MIPS16e SAVE and RESTORE instructions have two ranges of registers:
     $a3-$a0 and $s2-$s8.  If we save one register in the range, we must
     save all later registers too.  */
  if (GENERATE_MIPS16E_SAVE_RESTORE)
    {
      mips16e_mask_registers (&frame->mask, mips16e_s2_s8_regs,
 			      ARRAY_SIZE (mips16e_s2_s8_regs), &frame->num_gp);
      mips16e_mask_registers (&frame->mask, mips16e_a0_a3_regs,
 			      ARRAY_SIZE (mips16e_a0_a3_regs), &frame->num_gp);
    }

  /* Move above the GPR save area.  */
  if (frame->num_gp > 0)
    {
      offset += MIPS_STACK_ALIGN (frame->num_gp * UNITS_PER_WORD);
      frame->gp_sp_offset = offset - UNITS_PER_WORD;
    }

  /* Find out which FPRs we need to save.  This loop must iterate over
     the same space as its companion in mips_for_each_saved_gpr_and_fpr.  */
  if (TARGET_HARD_FLOAT)
    for (regno = FP_REG_FIRST; regno <= FP_REG_LAST; regno += MAX_FPRS_PER_FMT)
      if (mips_save_reg_p (regno))
	{
	  frame->num_fp += MAX_FPRS_PER_FMT;
	  frame->fmask |= ~(~0 << MAX_FPRS_PER_FMT) << (regno - FP_REG_FIRST);
	}

  /* Move above the FPR save area.  */
  if (frame->num_fp > 0)
    {
      offset += MIPS_STACK_ALIGN (frame->num_fp * UNITS_PER_FPREG);
      frame->fp_sp_offset = offset - UNITS_PER_HWFPVALUE;
    }

  /* Add in space for the interrupt context information.  */
  if (cfun->machine->interrupt_handler_p)
    {
      /* Check HI/LO.  */
      if (mips_save_reg_p (LO_REGNUM) || mips_save_reg_p (HI_REGNUM))
	{
	  frame->num_acc++;
	  frame->acc_mask |= (1 << 0);
	}

      /* Check accumulators 1, 2, 3.  */
      for (i = DSP_ACC_REG_FIRST; i <= DSP_ACC_REG_LAST; i += 2)
	if (mips_save_reg_p (i) || mips_save_reg_p (i + 1))
	  {
	    frame->num_acc++;
	    frame->acc_mask |= 1 << (((i - DSP_ACC_REG_FIRST) / 2) + 1);
	  }

      /* All interrupt context functions need space to preserve STATUS.  */
      frame->num_cop0_regs++;

      /* If we don't keep interrupts masked, we need to save EPC.  */
      if (!cfun->machine->keep_interrupts_masked_p)
	frame->num_cop0_regs++;
    }

  /* Move above the accumulator save area.  */
  if (frame->num_acc > 0)
    {
      /* Each accumulator needs 2 words.  */
      offset += frame->num_acc * 2 * UNITS_PER_WORD;
      frame->acc_sp_offset = offset - UNITS_PER_WORD;
    }

  /* Move above the COP0 register save area.  */
  if (frame->num_cop0_regs > 0)
    {
      offset += frame->num_cop0_regs * UNITS_PER_WORD;
      frame->cop0_sp_offset = offset - UNITS_PER_WORD;
    }

  /* Move above the callee-allocated varargs save area.  */
  offset += MIPS_STACK_ALIGN (cfun->machine->varargs_size);
  frame->arg_pointer_offset = offset;

  /* Move above the callee-allocated area for pretend stack arguments.  */
  offset += crtl->args.pretend_args_size;
  frame->total_size = offset;

  /* Work out the offsets of the save areas from the top of the frame.  */
  if (frame->gp_sp_offset > 0)
    frame->gp_save_offset = frame->gp_sp_offset - offset;
  if (frame->fp_sp_offset > 0)
    frame->fp_save_offset = frame->fp_sp_offset - offset;
  if (frame->acc_sp_offset > 0)
    frame->acc_save_offset = frame->acc_sp_offset - offset;
  if (frame->num_cop0_regs > 0)
    frame->cop0_save_offset = frame->cop0_sp_offset - offset;

  /* MIPS16 code offsets the frame pointer by the size of the outgoing
     arguments.  This tends to increase the chances of using unextended
     instructions for local variables and incoming arguments.  */
  if (TARGET_MIPS16)
    frame->hard_frame_pointer_offset = frame->args_size;
}

/* Return the style of GP load sequence that is being used for the
   current function.  */

enum mips_loadgp_style
mips_current_loadgp_style (void)
{
  if (!TARGET_USE_GOT || cfun->machine->global_pointer == INVALID_REGNUM)
    return LOADGP_NONE;

  if (TARGET_RTP_PIC)
    return LOADGP_RTP;

  if (TARGET_ABSOLUTE_ABICALLS)
    return LOADGP_ABSOLUTE;

  return TARGET_NEWABI ? LOADGP_NEWABI : LOADGP_OLDABI;
}

/* Implement TARGET_FRAME_POINTER_REQUIRED.  */

static bool
mips_frame_pointer_required (void)
{
  /* If the function contains dynamic stack allocations, we need to
     use the frame pointer to access the static parts of the frame.  */
  if (cfun->calls_alloca)
    return true;

  /* In MIPS16 mode, we need a frame pointer for a large frame; otherwise,
     reload may be unable to compute the address of a local variable,
     since there is no way to add a large constant to the stack pointer
     without using a second temporary register.  */
  if (TARGET_MIPS16)
    {
      mips_compute_frame_info ();
      if (!SMALL_OPERAND (cfun->machine->frame.total_size))
	return true;
    }

  return false;
}

/* Make sure that we're not trying to eliminate to the wrong hard frame
   pointer.  */

static bool
mips_can_eliminate (const int from ATTRIBUTE_UNUSED, const int to)
{
  return (to == HARD_FRAME_POINTER_REGNUM || to == STACK_POINTER_REGNUM);
}

/* Implement INITIAL_ELIMINATION_OFFSET.  FROM is either the frame pointer
   or argument pointer.  TO is either the stack pointer or hard frame
   pointer.  */

HOST_WIDE_INT
mips_initial_elimination_offset (int from, int to)
{
  HOST_WIDE_INT offset;

  mips_compute_frame_info ();

  /* Set OFFSET to the offset from the end-of-prologue stack pointer.  */
  switch (from)
    {
    case FRAME_POINTER_REGNUM:
      if (FRAME_GROWS_DOWNWARD)
	offset = (cfun->machine->frame.args_size
		  + cfun->machine->frame.cprestore_size
		  + cfun->machine->frame.var_size);
      else
	offset = 0;
      break;

    case ARG_POINTER_REGNUM:
      offset = cfun->machine->frame.arg_pointer_offset;
      break;

    default:
      gcc_unreachable ();
    }

  if (to == HARD_FRAME_POINTER_REGNUM)
    offset -= cfun->machine->frame.hard_frame_pointer_offset;

  return offset;
}

/* Implement TARGET_EXTRA_LIVE_ON_ENTRY.  */

static void
mips_extra_live_on_entry (bitmap regs)
{
  if (TARGET_USE_GOT)
    {
      /* PIC_FUNCTION_ADDR_REGNUM is live if we need it to set up
	 the global pointer.   */
      if (!TARGET_ABSOLUTE_ABICALLS)
	bitmap_set_bit (regs, PIC_FUNCTION_ADDR_REGNUM);

      /* The prologue may set MIPS16_PIC_TEMP_REGNUM to the value of
	 the global pointer.  */
      if (TARGET_MIPS16)
	bitmap_set_bit (regs, MIPS16_PIC_TEMP_REGNUM);

      /* See the comment above load_call<mode> for details.  */
      bitmap_set_bit (regs, GOT_VERSION_REGNUM);
    }
}

/* Implement RETURN_ADDR_RTX.  We do not support moving back to a
   previous frame.  */

rtx
mips_return_addr (int count, rtx frame ATTRIBUTE_UNUSED)
{
  if (count != 0)
    return const0_rtx;

  return get_hard_reg_initial_val (Pmode, RETURN_ADDR_REGNUM);
}

/* Emit code to change the current function's return address to
   ADDRESS.  SCRATCH is available as a scratch register, if needed.
   ADDRESS and SCRATCH are both word-mode GPRs.  */

void
mips_set_return_address (rtx address, rtx scratch)
{
  rtx slot_address;

  gcc_assert (BITSET_P (cfun->machine->frame.mask, RETURN_ADDR_REGNUM));
  slot_address = mips_add_offset (scratch, stack_pointer_rtx,
				  cfun->machine->frame.gp_sp_offset);
  mips_emit_move (gen_frame_mem (GET_MODE (address), slot_address), address);
}

/* Return true if the current function has a cprestore slot.  */

bool
mips_cfun_has_cprestore_slot_p (void)
{
  return (cfun->machine->global_pointer != INVALID_REGNUM
	  && cfun->machine->frame.cprestore_size > 0);
}

/* Fill *BASE and *OFFSET such that *BASE + *OFFSET refers to the
   cprestore slot.  LOAD_P is true if the caller wants to load from
   the cprestore slot; it is false if the caller wants to store to
   the slot.  */

static void
mips_get_cprestore_base_and_offset (rtx *base, HOST_WIDE_INT *offset,
				    bool load_p)
{
  const struct mips_frame_info *frame;

  frame = &cfun->machine->frame;
  /* .cprestore always uses the stack pointer instead of the frame pointer.
     We have a free choice for direct stores for non-MIPS16 functions,
     and for MIPS16 functions whose cprestore slot is in range of the
     stack pointer.  Using the stack pointer would sometimes give more
     (early) scheduling freedom, but using the frame pointer would
     sometimes give more (late) scheduling freedom.  It's hard to
     predict which applies to a given function, so let's keep things
     simple.

     Loads must always use the frame pointer in functions that call
     alloca, and there's little benefit to using the stack pointer
     otherwise.  */
  if (frame_pointer_needed && !(TARGET_CPRESTORE_DIRECTIVE && !load_p))
    {
      *base = hard_frame_pointer_rtx;
      *offset = frame->args_size - frame->hard_frame_pointer_offset;
    }
  else
    {
      *base = stack_pointer_rtx;
      *offset = frame->args_size;
    }
}

/* Return true if X is the load or store address of the cprestore slot;
   LOAD_P says which.  */

bool
mips_cprestore_address_p (rtx x, bool load_p)
{
  rtx given_base, required_base;
  HOST_WIDE_INT given_offset, required_offset;

  mips_split_plus (x, &given_base, &given_offset);
  mips_get_cprestore_base_and_offset (&required_base, &required_offset, load_p);
  return given_base == required_base && given_offset == required_offset;
}

/* Return a MEM rtx for the cprestore slot.  LOAD_P is true if we are
   going to load from it, false if we are going to store to it.
   Use TEMP as a temporary register if need be.  */

static rtx
mips_cprestore_slot (rtx temp, bool load_p)
{
  rtx base;
  HOST_WIDE_INT offset;

  mips_get_cprestore_base_and_offset (&base, &offset, load_p);
  return gen_frame_mem (Pmode, mips_add_offset (temp, base, offset));
}

/* Emit instructions to save global pointer value GP into cprestore
   slot MEM.  OFFSET is the offset that MEM applies to the base register.

   MEM may not be a legitimate address.  If it isn't, TEMP is a
   temporary register that can be used, otherwise it is a SCRATCH.  */

void
mips_save_gp_to_cprestore_slot (rtx mem, rtx offset, rtx gp, rtx temp)
{
  if (TARGET_CPRESTORE_DIRECTIVE)
    {
      gcc_assert (gp == pic_offset_table_rtx);
      emit_insn (gen_cprestore (mem, offset));
    }
  else
    mips_emit_move (mips_cprestore_slot (temp, false), gp);
}

/* Restore $gp from its save slot, using TEMP as a temporary base register
   if need be.  This function is for o32 and o64 abicalls only.

   See mips_must_initialize_gp_p for details about how we manage the
   global pointer.  */

void
mips_restore_gp_from_cprestore_slot (rtx temp)
{
  gcc_assert (TARGET_ABICALLS && TARGET_OLDABI && epilogue_completed);

  if (!cfun->machine->must_restore_gp_when_clobbered_p)
    {
      emit_note (NOTE_INSN_DELETED);
      return;
    }

  if (TARGET_MIPS16)
    {
      mips_emit_move (temp, mips_cprestore_slot (temp, true));
      mips_emit_move (pic_offset_table_rtx, temp);
    }
  else
    mips_emit_move (pic_offset_table_rtx, mips_cprestore_slot (temp, true));
  if (!TARGET_EXPLICIT_RELOCS)
    emit_insn (gen_blockage ());
}

/* A function to save or store a register.  The first argument is the
   register and the second is the stack slot.  */
typedef void (*mips_save_restore_fn) (rtx, rtx);

/* Use FN to save or restore register REGNO.  MODE is the register's
   mode and OFFSET is the offset of its save slot from the current
   stack pointer.  */

static void
mips_save_restore_reg (enum machine_mode mode, int regno,
		       HOST_WIDE_INT offset, mips_save_restore_fn fn)
{
  rtx mem;

  mem = gen_frame_mem (mode, plus_constant (stack_pointer_rtx, offset));
  fn (gen_rtx_REG (mode, regno), mem);
}

/* Call FN for each accumlator that is saved by the current function.
   SP_OFFSET is the offset of the current stack pointer from the start
   of the frame.  */

static void
mips_for_each_saved_acc (HOST_WIDE_INT sp_offset, mips_save_restore_fn fn)
{
  HOST_WIDE_INT offset;
  int regno;

  offset = cfun->machine->frame.acc_sp_offset - sp_offset;
  if (BITSET_P (cfun->machine->frame.acc_mask, 0))
    {
      mips_save_restore_reg (word_mode, LO_REGNUM, offset, fn);
      offset -= UNITS_PER_WORD;
      mips_save_restore_reg (word_mode, HI_REGNUM, offset, fn);
      offset -= UNITS_PER_WORD;
    }

  for (regno = DSP_ACC_REG_FIRST; regno <= DSP_ACC_REG_LAST; regno++)
    if (BITSET_P (cfun->machine->frame.acc_mask,
		  ((regno - DSP_ACC_REG_FIRST) / 2) + 1))
      {
	mips_save_restore_reg (word_mode, regno, offset, fn);
	offset -= UNITS_PER_WORD;
      }
}

/* Call FN for each register that is saved by the current function.
   SP_OFFSET is the offset of the current stack pointer from the start
   of the frame.  */

static void
mips_for_each_saved_gpr_and_fpr (HOST_WIDE_INT sp_offset,
				 mips_save_restore_fn fn)
{
  enum machine_mode fpr_mode;
  HOST_WIDE_INT offset;
  int regno;

  /* Save registers starting from high to low.  The debuggers prefer at least
     the return register be stored at func+4, and also it allows us not to
     need a nop in the epilogue if at least one register is reloaded in
     addition to return address.  */
  offset = cfun->machine->frame.gp_sp_offset - sp_offset;
  for (regno = GP_REG_LAST; regno >= GP_REG_FIRST; regno--)
    if (BITSET_P (cfun->machine->frame.mask, regno - GP_REG_FIRST))
      {
	/* Record the ra offset for use by mips_function_profiler.  */
	if (regno == RETURN_ADDR_REGNUM)
	  cfun->machine->frame.ra_fp_offset = offset + sp_offset;
	mips_save_restore_reg (word_mode, regno, offset, fn);
	offset -= UNITS_PER_WORD;
      }

  /* This loop must iterate over the same space as its companion in
     mips_compute_frame_info.  */
  offset = cfun->machine->frame.fp_sp_offset - sp_offset;
  fpr_mode = (TARGET_SINGLE_FLOAT ? SFmode : DFmode);
  for (regno = FP_REG_LAST - MAX_FPRS_PER_FMT + 1;
       regno >= FP_REG_FIRST;
       regno -= MAX_FPRS_PER_FMT)
    if (BITSET_P (cfun->machine->frame.fmask, regno - FP_REG_FIRST))
      {
	mips_save_restore_reg (fpr_mode, regno, offset, fn);
	offset -= GET_MODE_SIZE (fpr_mode);
      }
}

/* Return true if a move between register REGNO and its save slot (MEM)
   can be done in a single move.  LOAD_P is true if we are loading
   from the slot, false if we are storing to it.  */

static bool
mips_direct_save_slot_move_p (unsigned int regno, rtx mem, bool load_p)
{
  /* There is a specific MIPS16 instruction for saving $31 to the stack.  */
  if (TARGET_MIPS16 && !load_p && regno == RETURN_ADDR_REGNUM)
    return false;

  return mips_secondary_reload_class (REGNO_REG_CLASS (regno),
				      GET_MODE (mem), mem, load_p) == NO_REGS;
}

/* Emit a move from SRC to DEST, given that one of them is a register
   save slot and that the other is a register.  TEMP is a temporary
   GPR of the same mode that is available if need be.  */

void
mips_emit_save_slot_move (rtx dest, rtx src, rtx temp)
{
  unsigned int regno;
  rtx mem;

  if (REG_P (src))
    {
      regno = REGNO (src);
      mem = dest;
    }
  else
    {
      regno = REGNO (dest);
      mem = src;
    }

  if (regno == cfun->machine->global_pointer && !mips_must_initialize_gp_p ())
    {
      /* We don't yet know whether we'll need this instruction or not.
	 Postpone the decision by emitting a ghost move.  This move
	 is specifically not frame-related; only the split version is.  */
      if (TARGET_64BIT)
	emit_insn (gen_move_gpdi (dest, src));
      else
	emit_insn (gen_move_gpsi (dest, src));
      return;
    }

  if (regno == HI_REGNUM)
    {
      if (REG_P (dest))
	{
	  mips_emit_move (temp, src);
	  if (TARGET_64BIT)
	    emit_insn (gen_mthisi_di (gen_rtx_REG (TImode, MD_REG_FIRST),
				      temp, gen_rtx_REG (DImode, LO_REGNUM)));
	  else
	    emit_insn (gen_mthisi_di (gen_rtx_REG (DImode, MD_REG_FIRST),
				      temp, gen_rtx_REG (SImode, LO_REGNUM)));
	}
      else
	{
	  if (TARGET_64BIT)
	    emit_insn (gen_mfhidi_ti (temp,
				      gen_rtx_REG (TImode, MD_REG_FIRST)));
	  else
	    emit_insn (gen_mfhisi_di (temp,
				      gen_rtx_REG (DImode, MD_REG_FIRST)));
	  mips_emit_move (dest, temp);
	}
    }
  else if (mips_direct_save_slot_move_p (regno, mem, mem == src))
    mips_emit_move (dest, src);
  else
    {
      gcc_assert (!reg_overlap_mentioned_p (dest, temp));
      mips_emit_move (temp, src);
      mips_emit_move (dest, temp);
    }
  if (MEM_P (dest))
    mips_set_frame_expr (mips_frame_set (dest, src));
}

/* If we're generating n32 or n64 abicalls, and the current function
   does not use $28 as its global pointer, emit a cplocal directive.
   Use pic_offset_table_rtx as the argument to the directive.  */

static void
mips_output_cplocal (void)
{
  if (!TARGET_EXPLICIT_RELOCS
      && mips_must_initialize_gp_p ()
      && cfun->machine->global_pointer != GLOBAL_POINTER_REGNUM)
    output_asm_insn (".cplocal %+", 0);
}

/* Implement TARGET_OUTPUT_FUNCTION_PROLOGUE.  */

static void
mips_output_function_prologue (FILE *file, HOST_WIDE_INT size ATTRIBUTE_UNUSED)
{
  const char *fnname;

#ifdef SDB_DEBUGGING_INFO
  if (debug_info_level != DINFO_LEVEL_TERSE && write_symbols == SDB_DEBUG)
    SDB_OUTPUT_SOURCE_LINE (file, DECL_SOURCE_LINE (current_function_decl));
#endif

  /* In MIPS16 mode, we may need to generate a non-MIPS16 stub to handle
     floating-point arguments.  */
  if (TARGET_MIPS16
      && TARGET_HARD_FLOAT_ABI
      && crtl->args.info.fp_code != 0)
    mips16_build_function_stub ();

  /* Get the function name the same way that toplev.c does before calling
     assemble_start_function.  This is needed so that the name used here
     exactly matches the name used in ASM_DECLARE_FUNCTION_NAME.  */
  fnname = XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0);
  mips_start_function_definition (fnname, TARGET_MIPS16);

  /* Output MIPS-specific frame information.  */
  if (!flag_inhibit_size_directive)
    {
      const struct mips_frame_info *frame;

      frame = &cfun->machine->frame;

      /* .frame FRAMEREG, FRAMESIZE, RETREG.  */
      fprintf (file,
	       "\t.frame\t%s," HOST_WIDE_INT_PRINT_DEC ",%s\t\t"
	       "# vars= " HOST_WIDE_INT_PRINT_DEC
	       ", regs= %d/%d"
	       ", args= " HOST_WIDE_INT_PRINT_DEC
	       ", gp= " HOST_WIDE_INT_PRINT_DEC "\n",
	       reg_names[frame_pointer_needed
			 ? HARD_FRAME_POINTER_REGNUM
			 : STACK_POINTER_REGNUM],
	       (frame_pointer_needed
		? frame->total_size - frame->hard_frame_pointer_offset
		: frame->total_size),
	       reg_names[RETURN_ADDR_REGNUM],
	       frame->var_size,
	       frame->num_gp, frame->num_fp,
	       frame->args_size,
	       frame->cprestore_size);

      /* .mask MASK, OFFSET.  */
      fprintf (file, "\t.mask\t0x%08x," HOST_WIDE_INT_PRINT_DEC "\n",
	       frame->mask, frame->gp_save_offset);

      /* .fmask MASK, OFFSET.  */
      fprintf (file, "\t.fmask\t0x%08x," HOST_WIDE_INT_PRINT_DEC "\n",
	       frame->fmask, frame->fp_save_offset);
    }

  /* Handle the initialization of $gp for SVR4 PIC, if applicable.
     Also emit the ".set noreorder; .set nomacro" sequence for functions
     that need it.  */
  if (mips_must_initialize_gp_p ()
      && mips_current_loadgp_style () == LOADGP_OLDABI)
    {
      if (TARGET_MIPS16)
	{
	  /* This is a fixed-form sequence.  The position of the
	     first two instructions is important because of the
	     way _gp_disp is defined.  */
	  output_asm_insn ("li\t$2,%%hi(_gp_disp)", 0);
	  output_asm_insn ("addiu\t$3,$pc,%%lo(_gp_disp)", 0);
	  output_asm_insn ("sll\t$2,16", 0);
	  output_asm_insn ("addu\t$2,$3", 0);
	}
      else
	{
	  /* .cpload must be in a .set noreorder but not a
	     .set nomacro block.  */
	  mips_push_asm_switch (&mips_noreorder);
	  output_asm_insn (".cpload\t%^", 0);
	  if (!cfun->machine->all_noreorder_p)
	    mips_pop_asm_switch (&mips_noreorder);
	  else
	    mips_push_asm_switch (&mips_nomacro);
	}
    }
  else if (cfun->machine->all_noreorder_p)
    {
      mips_push_asm_switch (&mips_noreorder);
      mips_push_asm_switch (&mips_nomacro);
    }

  /* Tell the assembler which register we're using as the global
     pointer.  This is needed for thunks, since they can use either
     explicit relocs or assembler macros.  */
  mips_output_cplocal ();
}

/* Implement TARGET_OUTPUT_FUNCTION_EPILOGUE.  */

static void
mips_output_function_epilogue (FILE *file ATTRIBUTE_UNUSED,
			       HOST_WIDE_INT size ATTRIBUTE_UNUSED)
{
  const char *fnname;

  /* Reinstate the normal $gp.  */
  SET_REGNO (pic_offset_table_rtx, GLOBAL_POINTER_REGNUM);
  mips_output_cplocal ();

  if (cfun->machine->all_noreorder_p)
    {
      mips_pop_asm_switch (&mips_nomacro);
      mips_pop_asm_switch (&mips_noreorder);
    }

  /* Get the function name the same way that toplev.c does before calling
     assemble_start_function.  This is needed so that the name used here
     exactly matches the name used in ASM_DECLARE_FUNCTION_NAME.  */
  fnname = XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0);
  mips_end_function_definition (fnname);
}

/* Save register REG to MEM.  Make the instruction frame-related.  */

static void
mips_save_reg (rtx reg, rtx mem)
{
  if (GET_MODE (reg) == DFmode && !TARGET_FLOAT64)
    {
      rtx x1, x2;

      if (mips_split_64bit_move_p (mem, reg))
	mips_split_doubleword_move (mem, reg);
      else
	mips_emit_move (mem, reg);

      x1 = mips_frame_set (mips_subword (mem, false),
			   mips_subword (reg, false));
      x2 = mips_frame_set (mips_subword (mem, true),
			   mips_subword (reg, true));
      mips_set_frame_expr (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, x1, x2)));
    }
  else
    mips_emit_save_slot_move (mem, reg, MIPS_PROLOGUE_TEMP (GET_MODE (reg)));
}

/* The __gnu_local_gp symbol.  */

static GTY(()) rtx mips_gnu_local_gp;

/* If we're generating n32 or n64 abicalls, emit instructions
   to set up the global pointer.  */

static void
mips_emit_loadgp (void)
{
  rtx addr, offset, incoming_address, base, index, pic_reg;

  pic_reg = TARGET_MIPS16 ? MIPS16_PIC_TEMP : pic_offset_table_rtx;
  switch (mips_current_loadgp_style ())
    {
    case LOADGP_ABSOLUTE:
      if (mips_gnu_local_gp == NULL)
	{
	  mips_gnu_local_gp = gen_rtx_SYMBOL_REF (Pmode, "__gnu_local_gp");
	  SYMBOL_REF_FLAGS (mips_gnu_local_gp) |= SYMBOL_FLAG_LOCAL;
	}
      emit_insn (Pmode == SImode
		 ? gen_loadgp_absolute_si (pic_reg, mips_gnu_local_gp)
		 : gen_loadgp_absolute_di (pic_reg, mips_gnu_local_gp));
      break;

    case LOADGP_OLDABI:
      /* Added by mips_output_function_prologue.  */
      break;

    case LOADGP_NEWABI:
      addr = XEXP (DECL_RTL (current_function_decl), 0);
      offset = mips_unspec_address (addr, SYMBOL_GOTOFF_LOADGP);
      incoming_address = gen_rtx_REG (Pmode, PIC_FUNCTION_ADDR_REGNUM);
      emit_insn (Pmode == SImode
		 ? gen_loadgp_newabi_si (pic_reg, offset, incoming_address)
		 : gen_loadgp_newabi_di (pic_reg, offset, incoming_address));
      break;

    case LOADGP_RTP:
      base = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (VXWORKS_GOTT_BASE));
      index = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (VXWORKS_GOTT_INDEX));
      emit_insn (Pmode == SImode
		 ? gen_loadgp_rtp_si (pic_reg, base, index)
		 : gen_loadgp_rtp_di (pic_reg, base, index));
      break;

    default:
      return;
    }

  if (TARGET_MIPS16)
    emit_insn (gen_copygp_mips16 (pic_offset_table_rtx, pic_reg));

  /* Emit a blockage if there are implicit uses of the GP register.
     This includes profiled functions, because FUNCTION_PROFILE uses
     a jal macro.  */
  if (!TARGET_EXPLICIT_RELOCS || crtl->profile)
    emit_insn (gen_loadgp_blockage ());
}

/* A for_each_rtx callback.  Stop the search if *X is a kernel register.  */

static int
mips_kernel_reg_p (rtx *x, void *data ATTRIBUTE_UNUSED)
{
  return REG_P (*x) && KERNEL_REG_P (REGNO (*x));
}

/* Expand the "prologue" pattern.  */

void
mips_expand_prologue (void)
{
  const struct mips_frame_info *frame;
  HOST_WIDE_INT size;
  unsigned int nargs;
  rtx insn;

  if (cfun->machine->global_pointer != INVALID_REGNUM)
    {
      /* Check whether an insn uses pic_offset_table_rtx, either explicitly
	 or implicitly.  If so, we can commit to using a global pointer
	 straight away, otherwise we need to defer the decision.  */
      if (mips_cfun_has_inflexible_gp_ref_p ()
	  || mips_cfun_has_flexible_gp_ref_p ())
	{
	  cfun->machine->must_initialize_gp_p = true;
	  cfun->machine->must_restore_gp_when_clobbered_p = true;
	}

      SET_REGNO (pic_offset_table_rtx, cfun->machine->global_pointer);
    }

  frame = &cfun->machine->frame;
  size = frame->total_size;

  if (flag_stack_usage)
    current_function_static_stack_size = size;

  /* Save the registers.  Allocate up to MIPS_MAX_FIRST_STACK_STEP
     bytes beforehand; this is enough to cover the register save area
     without going out of range.  */
  if (((frame->mask | frame->fmask | frame->acc_mask) != 0)
      || frame->num_cop0_regs > 0)
    {
      HOST_WIDE_INT step1;

      step1 = MIN (size, MIPS_MAX_FIRST_STACK_STEP);
      if (GENERATE_MIPS16E_SAVE_RESTORE)
 	{
 	  HOST_WIDE_INT offset;
 	  unsigned int mask, regno;

	  /* Try to merge argument stores into the save instruction.  */
	  nargs = mips16e_collect_argument_saves ();

	  /* Build the save instruction.  */
	  mask = frame->mask;
	  insn = mips16e_build_save_restore (false, &mask, &offset,
					     nargs, step1);
	  RTX_FRAME_RELATED_P (emit_insn (insn)) = 1;
 	  size -= step1;

 	  /* Check if we need to save other registers.  */
 	  for (regno = GP_REG_FIRST; regno < GP_REG_LAST; regno++)
 	    if (BITSET_P (mask, regno - GP_REG_FIRST))
 	      {
		offset -= UNITS_PER_WORD;
		mips_save_restore_reg (word_mode, regno,
				       offset, mips_save_reg);
 	      }
 	}
      else
 	{
	  if (cfun->machine->interrupt_handler_p)
	    {
	      HOST_WIDE_INT offset;
	      rtx mem;

	      /* If this interrupt is using a shadow register set, we need to
		 get the stack pointer from the previous register set.  */
	      if (cfun->machine->use_shadow_register_set_p)
		emit_insn (gen_mips_rdpgpr (stack_pointer_rtx,
					    stack_pointer_rtx));

	      if (!cfun->machine->keep_interrupts_masked_p)
		{
		  /* Move from COP0 Cause to K0.  */
		  emit_insn (gen_cop0_move (gen_rtx_REG (SImode, K0_REG_NUM),
					    gen_rtx_REG (SImode,
							 COP0_CAUSE_REG_NUM)));
		  /* Move from COP0 EPC to K1.  */
		  emit_insn (gen_cop0_move (gen_rtx_REG (SImode, K1_REG_NUM),
					    gen_rtx_REG (SImode,
							 COP0_EPC_REG_NUM)));
		}

	      /* Allocate the first part of the frame.  */
	      insn = gen_add3_insn (stack_pointer_rtx, stack_pointer_rtx,
				    GEN_INT (-step1));
	      RTX_FRAME_RELATED_P (emit_insn (insn)) = 1;
	      size -= step1;

	      /* Start at the uppermost location for saving.  */
	      offset = frame->cop0_sp_offset - size;
	      if (!cfun->machine->keep_interrupts_masked_p)
		{
		  /* Push EPC into its stack slot.  */
		  mem = gen_frame_mem (word_mode,
				       plus_constant (stack_pointer_rtx,
						      offset));
		  mips_emit_move (mem, gen_rtx_REG (word_mode, K1_REG_NUM));
		  offset -= UNITS_PER_WORD;
		}

	      /* Move from COP0 Status to K1.  */
	      emit_insn (gen_cop0_move (gen_rtx_REG (SImode, K1_REG_NUM),
					gen_rtx_REG (SImode,
						     COP0_STATUS_REG_NUM)));

	      /* Right justify the RIPL in k0.  */
	      if (!cfun->machine->keep_interrupts_masked_p)
		emit_insn (gen_lshrsi3 (gen_rtx_REG (SImode, K0_REG_NUM),
					gen_rtx_REG (SImode, K0_REG_NUM),
					GEN_INT (CAUSE_IPL)));

	      /* Push Status into its stack slot.  */
	      mem = gen_frame_mem (word_mode,
				   plus_constant (stack_pointer_rtx, offset));
	      mips_emit_move (mem, gen_rtx_REG (word_mode, K1_REG_NUM));
	      offset -= UNITS_PER_WORD;

	      /* Insert the RIPL into our copy of SR (k1) as the new IPL.  */
	      if (!cfun->machine->keep_interrupts_masked_p)
		emit_insn (gen_insvsi (gen_rtx_REG (SImode, K1_REG_NUM),
				       GEN_INT (6),
				       GEN_INT (SR_IPL),
				       gen_rtx_REG (SImode, K0_REG_NUM)));

	      if (!cfun->machine->keep_interrupts_masked_p)
		/* Enable interrupts by clearing the KSU ERL and EXL bits.
		   IE is already the correct value, so we don't have to do
		   anything explicit.  */
		emit_insn (gen_insvsi (gen_rtx_REG (SImode, K1_REG_NUM),
				       GEN_INT (4),
				       GEN_INT (SR_EXL),
				       gen_rtx_REG (SImode, GP_REG_FIRST)));
	      else
		/* Disable interrupts by clearing the KSU, ERL, EXL,
		   and IE bits.  */
		emit_insn (gen_insvsi (gen_rtx_REG (SImode, K1_REG_NUM),
				       GEN_INT (5),
				       GEN_INT (SR_IE),
				       gen_rtx_REG (SImode, GP_REG_FIRST)));
	    }
	  else
	    {
	      insn = gen_add3_insn (stack_pointer_rtx,
				    stack_pointer_rtx,
				    GEN_INT (-step1));
	      RTX_FRAME_RELATED_P (emit_insn (insn)) = 1;
	      size -= step1;
	    }
	  mips_for_each_saved_acc (size, mips_save_reg);
	  mips_for_each_saved_gpr_and_fpr (size, mips_save_reg);
	}
    }

  /* Allocate the rest of the frame.  */
  if (size > 0)
    {
      if (SMALL_OPERAND (-size))
	RTX_FRAME_RELATED_P (emit_insn (gen_add3_insn (stack_pointer_rtx,
						       stack_pointer_rtx,
						       GEN_INT (-size)))) = 1;
      else
	{
	  mips_emit_move (MIPS_PROLOGUE_TEMP (Pmode), GEN_INT (size));
	  if (TARGET_MIPS16)
	    {
	      /* There are no instructions to add or subtract registers
		 from the stack pointer, so use the frame pointer as a
		 temporary.  We should always be using a frame pointer
		 in this case anyway.  */
	      gcc_assert (frame_pointer_needed);
	      mips_emit_move (hard_frame_pointer_rtx, stack_pointer_rtx);
	      emit_insn (gen_sub3_insn (hard_frame_pointer_rtx,
					hard_frame_pointer_rtx,
					MIPS_PROLOGUE_TEMP (Pmode)));
	      mips_emit_move (stack_pointer_rtx, hard_frame_pointer_rtx);
	    }
	  else
	    emit_insn (gen_sub3_insn (stack_pointer_rtx,
				      stack_pointer_rtx,
				      MIPS_PROLOGUE_TEMP (Pmode)));

	  /* Describe the combined effect of the previous instructions.  */
	  mips_set_frame_expr
	    (gen_rtx_SET (VOIDmode, stack_pointer_rtx,
			  plus_constant (stack_pointer_rtx, -size)));
	}
    }

  /* Set up the frame pointer, if we're using one.  */
  if (frame_pointer_needed)
    {
      HOST_WIDE_INT offset;

      offset = frame->hard_frame_pointer_offset;
      if (offset == 0)
	{
	  insn = mips_emit_move (hard_frame_pointer_rtx, stack_pointer_rtx);
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
      else if (SMALL_OPERAND (offset))
	{
	  insn = gen_add3_insn (hard_frame_pointer_rtx,
				stack_pointer_rtx, GEN_INT (offset));
	  RTX_FRAME_RELATED_P (emit_insn (insn)) = 1;
	}
      else
	{
	  mips_emit_move (MIPS_PROLOGUE_TEMP (Pmode), GEN_INT (offset));
	  mips_emit_move (hard_frame_pointer_rtx, stack_pointer_rtx);
	  emit_insn (gen_add3_insn (hard_frame_pointer_rtx,
				    hard_frame_pointer_rtx,
				    MIPS_PROLOGUE_TEMP (Pmode)));
	  mips_set_frame_expr
	    (gen_rtx_SET (VOIDmode, hard_frame_pointer_rtx,
			  plus_constant (stack_pointer_rtx, offset)));
	}
    }

  mips_emit_loadgp ();

  /* Initialize the $gp save slot.  */
  if (mips_cfun_has_cprestore_slot_p ())
    {
      rtx base, mem, gp, temp;
      HOST_WIDE_INT offset;

      mips_get_cprestore_base_and_offset (&base, &offset, false);
      mem = gen_frame_mem (Pmode, plus_constant (base, offset));
      gp = TARGET_MIPS16 ? MIPS16_PIC_TEMP : pic_offset_table_rtx;
      temp = (SMALL_OPERAND (offset)
	      ? gen_rtx_SCRATCH (Pmode)
	      : MIPS_PROLOGUE_TEMP (Pmode));
      emit_insn (gen_potential_cprestore (mem, GEN_INT (offset), gp, temp));

      mips_get_cprestore_base_and_offset (&base, &offset, true);
      mem = gen_frame_mem (Pmode, plus_constant (base, offset));
      emit_insn (gen_use_cprestore (mem));
    }

  /* We need to search back to the last use of K0 or K1.  */
  if (cfun->machine->interrupt_handler_p)
    {
      for (insn = get_last_insn (); insn != NULL_RTX; insn = PREV_INSN (insn))
	if (INSN_P (insn)
	    && for_each_rtx (&PATTERN (insn), mips_kernel_reg_p, NULL))
	  break;
      /* Emit a move from K1 to COP0 Status after insn.  */
      gcc_assert (insn != NULL_RTX);
      emit_insn_after (gen_cop0_move (gen_rtx_REG (SImode, COP0_STATUS_REG_NUM),
				      gen_rtx_REG (SImode, K1_REG_NUM)),
		       insn);
    }

  /* If we are profiling, make sure no instructions are scheduled before
     the call to mcount.  */
  if (crtl->profile)
    emit_insn (gen_blockage ());
}

/* Emit instructions to restore register REG from slot MEM.  */

static void
mips_restore_reg (rtx reg, rtx mem)
{
  /* There's no MIPS16 instruction to load $31 directly.  Load into
     $7 instead and adjust the return insn appropriately.  */
  if (TARGET_MIPS16 && REGNO (reg) == RETURN_ADDR_REGNUM)
    reg = gen_rtx_REG (GET_MODE (reg), GP_REG_FIRST + 7);

  mips_emit_save_slot_move (reg, mem, MIPS_EPILOGUE_TEMP (GET_MODE (reg)));
}

/* Emit any instructions needed before a return.  */

void
mips_expand_before_return (void)
{
  /* When using a call-clobbered gp, we start out with unified call
     insns that include instructions to restore the gp.  We then split
     these unified calls after reload.  These split calls explicitly
     clobber gp, so there is no need to define
     PIC_OFFSET_TABLE_REG_CALL_CLOBBERED.

     For consistency, we should also insert an explicit clobber of $28
     before return insns, so that the post-reload optimizers know that
     the register is not live on exit.  */
  if (TARGET_CALL_CLOBBERED_GP)
    emit_clobber (pic_offset_table_rtx);
}

/* Expand an "epilogue" or "sibcall_epilogue" pattern; SIBCALL_P
   says which.  */

void
mips_expand_epilogue (bool sibcall_p)
{
  const struct mips_frame_info *frame;
  HOST_WIDE_INT step1, step2;
  rtx base, target, insn;

  if (!sibcall_p && mips_can_use_return_insn ())
    {
      emit_jump_insn (gen_return ());
      return;
    }

  /* In MIPS16 mode, if the return value should go into a floating-point
     register, we need to call a helper routine to copy it over.  */
  if (mips16_cfun_returns_in_fpr_p ())
    mips16_copy_fpr_return_value ();

  /* Split the frame into two.  STEP1 is the amount of stack we should
     deallocate before restoring the registers.  STEP2 is the amount we
     should deallocate afterwards.

     Start off by assuming that no registers need to be restored.  */
  frame = &cfun->machine->frame;
  step1 = frame->total_size;
  step2 = 0;

  /* Work out which register holds the frame address.  */
  if (!frame_pointer_needed)
    base = stack_pointer_rtx;
  else
    {
      base = hard_frame_pointer_rtx;
      step1 -= frame->hard_frame_pointer_offset;
    }

  /* If we need to restore registers, deallocate as much stack as
     possible in the second step without going out of range.  */
  if ((frame->mask | frame->fmask | frame->acc_mask) != 0
      || frame->num_cop0_regs > 0)
    {
      step2 = MIN (step1, MIPS_MAX_FIRST_STACK_STEP);
      step1 -= step2;
    }

  /* Set TARGET to BASE + STEP1.  */
  target = base;
  if (step1 > 0)
    {
      rtx adjust;

      /* Get an rtx for STEP1 that we can add to BASE.  */
      adjust = GEN_INT (step1);
      if (!SMALL_OPERAND (step1))
	{
	  mips_emit_move (MIPS_EPILOGUE_TEMP (Pmode), adjust);
	  adjust = MIPS_EPILOGUE_TEMP (Pmode);
	}

      /* Normal mode code can copy the result straight into $sp.  */
      if (!TARGET_MIPS16)
	target = stack_pointer_rtx;

      emit_insn (gen_add3_insn (target, base, adjust));
    }

  /* Copy TARGET into the stack pointer.  */
  if (target != stack_pointer_rtx)
    mips_emit_move (stack_pointer_rtx, target);

  /* If we're using addressing macros, $gp is implicitly used by all
     SYMBOL_REFs.  We must emit a blockage insn before restoring $gp
     from the stack.  */
  if (TARGET_CALL_SAVED_GP && !TARGET_EXPLICIT_RELOCS)
    emit_insn (gen_blockage ());

  if (GENERATE_MIPS16E_SAVE_RESTORE && frame->mask != 0)
    {
      unsigned int regno, mask;
      HOST_WIDE_INT offset;
      rtx restore;

      /* Generate the restore instruction.  */
      mask = frame->mask;
      restore = mips16e_build_save_restore (true, &mask, &offset, 0, step2);

      /* Restore any other registers manually.  */
      for (regno = GP_REG_FIRST; regno < GP_REG_LAST; regno++)
 	if (BITSET_P (mask, regno - GP_REG_FIRST))
 	  {
 	    offset -= UNITS_PER_WORD;
 	    mips_save_restore_reg (word_mode, regno, offset, mips_restore_reg);
 	  }

      /* Restore the remaining registers and deallocate the final bit
	 of the frame.  */
      emit_insn (restore);
    }
  else
    {
      /* Restore the registers.  */
      mips_for_each_saved_acc (frame->total_size - step2, mips_restore_reg);
      mips_for_each_saved_gpr_and_fpr (frame->total_size - step2,
				       mips_restore_reg);

      if (cfun->machine->interrupt_handler_p)
	{
	  HOST_WIDE_INT offset;
	  rtx mem;

	  offset = frame->cop0_sp_offset - (frame->total_size - step2);
	  if (!cfun->machine->keep_interrupts_masked_p)
	    {
	      /* Restore the original EPC.  */
	      mem = gen_frame_mem (word_mode,
				   plus_constant (stack_pointer_rtx, offset));
	      mips_emit_move (gen_rtx_REG (word_mode, K0_REG_NUM), mem);
	      offset -= UNITS_PER_WORD;

	      /* Move to COP0 EPC.  */
	      emit_insn (gen_cop0_move (gen_rtx_REG (SImode, COP0_EPC_REG_NUM),
					gen_rtx_REG (SImode, K0_REG_NUM)));
	    }

	  /* Restore the original Status.  */
	  mem = gen_frame_mem (word_mode,
			       plus_constant (stack_pointer_rtx, offset));
	  mips_emit_move (gen_rtx_REG (word_mode, K0_REG_NUM), mem);
	  offset -= UNITS_PER_WORD;

	  /* If we don't use shoadow register set, we need to update SP.  */
	  if (!cfun->machine->use_shadow_register_set_p && step2 > 0)
	    emit_insn (gen_add3_insn (stack_pointer_rtx,
				      stack_pointer_rtx,
				      GEN_INT (step2)));

	  /* Move to COP0 Status.  */
	  emit_insn (gen_cop0_move (gen_rtx_REG (SImode, COP0_STATUS_REG_NUM),
				    gen_rtx_REG (SImode, K0_REG_NUM)));
	}
      else
	{
	  /* Deallocate the final bit of the frame.  */
	  if (step2 > 0)
	    emit_insn (gen_add3_insn (stack_pointer_rtx,
				      stack_pointer_rtx,
				      GEN_INT (step2)));
	}
    }

  /* Add in the __builtin_eh_return stack adjustment.  We need to
     use a temporary in MIPS16 code.  */
  if (crtl->calls_eh_return)
    {
      if (TARGET_MIPS16)
	{
	  mips_emit_move (MIPS_EPILOGUE_TEMP (Pmode), stack_pointer_rtx);
	  emit_insn (gen_add3_insn (MIPS_EPILOGUE_TEMP (Pmode),
				    MIPS_EPILOGUE_TEMP (Pmode),
				    EH_RETURN_STACKADJ_RTX));
	  mips_emit_move (stack_pointer_rtx, MIPS_EPILOGUE_TEMP (Pmode));
	}
      else
	emit_insn (gen_add3_insn (stack_pointer_rtx,
				  stack_pointer_rtx,
				  EH_RETURN_STACKADJ_RTX));
    }

  if (!sibcall_p)
    {
      mips_expand_before_return ();
      if (cfun->machine->interrupt_handler_p)
	{
	  /* Interrupt handlers generate eret or deret.  */
	  if (cfun->machine->use_debug_exception_return_p)
	    emit_jump_insn (gen_mips_deret ());
	  else
	    emit_jump_insn (gen_mips_eret ());
	}
      else
	{
	  unsigned int regno;

	  /* When generating MIPS16 code, the normal
	     mips_for_each_saved_gpr_and_fpr path will restore the return
	     address into $7 rather than $31.  */
	  if (TARGET_MIPS16
	      && !GENERATE_MIPS16E_SAVE_RESTORE
	      && BITSET_P (frame->mask, RETURN_ADDR_REGNUM))
	    regno = GP_REG_FIRST + 7;
	  else
	    regno = RETURN_ADDR_REGNUM;
	  emit_jump_insn (gen_return_internal (gen_rtx_REG (Pmode, regno)));
	}
    }

  /* Search from the beginning to the first use of K0 or K1.  */
  if (cfun->machine->interrupt_handler_p
      && !cfun->machine->keep_interrupts_masked_p)
    {
      for (insn = get_insns (); insn != NULL_RTX; insn = NEXT_INSN (insn))
	if (INSN_P (insn)
	    && for_each_rtx (&PATTERN(insn), mips_kernel_reg_p, NULL))
	  break;
      gcc_assert (insn != NULL_RTX);
      /* Insert disable interrupts before the first use of K0 or K1.  */
      emit_insn_before (gen_mips_di (), insn);
      emit_insn_before (gen_mips_ehb (), insn);
    }
}

/* Return nonzero if this function is known to have a null epilogue.
   This allows the optimizer to omit jumps to jumps if no stack
   was created.  */

bool
mips_can_use_return_insn (void)
{
  /* Interrupt handlers need to go through the epilogue.  */
  if (cfun->machine->interrupt_handler_p)
    return false;

  if (!reload_completed)
    return false;

  if (crtl->profile)
    return false;

  /* In MIPS16 mode, a function that returns a floating-point value
     needs to arrange to copy the return value into the floating-point
     registers.  */
  if (mips16_cfun_returns_in_fpr_p ())
    return false;

  return cfun->machine->frame.total_size == 0;
}

/* Return true if register REGNO can store a value of mode MODE.
   The result of this function is cached in mips_hard_regno_mode_ok.  */

static bool
mips_hard_regno_mode_ok_p (unsigned int regno, enum machine_mode mode)
{
  unsigned int size;
  enum mode_class mclass;

  if (mode == CCV2mode)
    return (ISA_HAS_8CC
	    && ST_REG_P (regno)
	    && (regno - ST_REG_FIRST) % 2 == 0);

  if (mode == CCV4mode)
    return (ISA_HAS_8CC
	    && ST_REG_P (regno)
	    && (regno - ST_REG_FIRST) % 4 == 0);

  if (mode == CCmode)
    {
      if (!ISA_HAS_8CC)
	return regno == FPSW_REGNUM;

      return (ST_REG_P (regno)
	      || GP_REG_P (regno)
	      || FP_REG_P (regno));
    }

  size = GET_MODE_SIZE (mode);
  mclass = GET_MODE_CLASS (mode);

  if (GP_REG_P (regno))
    return ((regno - GP_REG_FIRST) & 1) == 0 || size <= UNITS_PER_WORD;

  if (FP_REG_P (regno)
      && (((regno - FP_REG_FIRST) % MAX_FPRS_PER_FMT) == 0
	  || (MIN_FPRS_PER_FMT == 1 && size <= UNITS_PER_FPREG)))
    {
      /* Allow TFmode for CCmode reloads.  */
      if (mode == TFmode && ISA_HAS_8CC)
	return true;

      /* Allow 64-bit vector modes for Loongson-2E/2F.  */
      if (TARGET_LOONGSON_VECTORS
	  && (mode == V2SImode
	      || mode == V4HImode
	      || mode == V8QImode
	      || mode == DImode))
	return true;

      if (mclass == MODE_FLOAT
	  || mclass == MODE_COMPLEX_FLOAT
	  || mclass == MODE_VECTOR_FLOAT)
	return size <= UNITS_PER_FPVALUE;

      /* Allow integer modes that fit into a single register.  We need
	 to put integers into FPRs when using instructions like CVT
	 and TRUNC.  There's no point allowing sizes smaller than a word,
	 because the FPU has no appropriate load/store instructions.  */
      if (mclass == MODE_INT)
	return size >= MIN_UNITS_PER_WORD && size <= UNITS_PER_FPREG;
    }

  if (ACC_REG_P (regno)
      && (INTEGRAL_MODE_P (mode) || ALL_FIXED_POINT_MODE_P (mode)))
    {
      if (MD_REG_P (regno))
	{
	  /* After a multiplication or division, clobbering HI makes
	     the value of LO unpredictable, and vice versa.  This means
	     that, for all interesting cases, HI and LO are effectively
	     a single register.

	     We model this by requiring that any value that uses HI
	     also uses LO.  */
	  if (size <= UNITS_PER_WORD * 2)
	    return regno == (size <= UNITS_PER_WORD ? LO_REGNUM : MD_REG_FIRST);
	}
      else
	{
	  /* DSP accumulators do not have the same restrictions as
	     HI and LO, so we can treat them as normal doubleword
	     registers.  */
	  if (size <= UNITS_PER_WORD)
	    return true;

	  if (size <= UNITS_PER_WORD * 2
	      && ((regno - DSP_ACC_REG_FIRST) & 1) == 0)
	    return true;
	}
    }

  if (ALL_COP_REG_P (regno))
    return mclass == MODE_INT && size <= UNITS_PER_WORD;

  if (regno == GOT_VERSION_REGNUM)
    return mode == SImode;

  return false;
}

/* Implement HARD_REGNO_NREGS.  */

unsigned int
mips_hard_regno_nregs (int regno, enum machine_mode mode)
{
  if (ST_REG_P (regno))
    /* The size of FP status registers is always 4, because they only hold
       CCmode values, and CCmode is always considered to be 4 bytes wide.  */
    return (GET_MODE_SIZE (mode) + 3) / 4;

  if (FP_REG_P (regno))
    return (GET_MODE_SIZE (mode) + UNITS_PER_FPREG - 1) / UNITS_PER_FPREG;

  /* All other registers are word-sized.  */
  return (GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
}

/* Implement CLASS_MAX_NREGS, taking the maximum of the cases
   in mips_hard_regno_nregs.  */

int
mips_class_max_nregs (enum reg_class rclass, enum machine_mode mode)
{
  int size;
  HARD_REG_SET left;

  size = 0x8000;
  COPY_HARD_REG_SET (left, reg_class_contents[(int) rclass]);
  if (hard_reg_set_intersect_p (left, reg_class_contents[(int) ST_REGS]))
    {
      size = MIN (size, 4);
      AND_COMPL_HARD_REG_SET (left, reg_class_contents[(int) ST_REGS]);
    }
  if (hard_reg_set_intersect_p (left, reg_class_contents[(int) FP_REGS]))
    {
      size = MIN (size, UNITS_PER_FPREG);
      AND_COMPL_HARD_REG_SET (left, reg_class_contents[(int) FP_REGS]);
    }
  if (!hard_reg_set_empty_p (left))
    size = MIN (size, UNITS_PER_WORD);
  return (GET_MODE_SIZE (mode) + size - 1) / size;
}

/* Implement CANNOT_CHANGE_MODE_CLASS.  */

bool
mips_cannot_change_mode_class (enum machine_mode from ATTRIBUTE_UNUSED,
			       enum machine_mode to ATTRIBUTE_UNUSED,
			       enum reg_class rclass)
{
  /* There are several problems with changing the modes of values
     in floating-point registers:

     - When a multi-word value is stored in paired floating-point
       registers, the first register always holds the low word.
       We therefore can't allow FPRs to change between single-word
       and multi-word modes on big-endian targets.

     - GCC assumes that each word of a multiword register can be accessed
       individually using SUBREGs.  This is not true for floating-point
       registers if they are bigger than a word.

     - Loading a 32-bit value into a 64-bit floating-point register
       will not sign-extend the value, despite what LOAD_EXTEND_OP says.
       We can't allow FPRs to change from SImode to to a wider mode on
       64-bit targets.

     - If the FPU has already interpreted a value in one format, we must
       not ask it to treat the value as having a different format.

     We therefore disallow all mode changes involving FPRs.  */
  return reg_classes_intersect_p (FP_REGS, rclass);
}

/* Implement target hook small_register_classes_for_mode_p.  */

static bool
mips_small_register_classes_for_mode_p (enum machine_mode mode
					ATTRIBUTE_UNUSED)
{
  return TARGET_MIPS16;
}

/* Return true if moves in mode MODE can use the FPU's mov.fmt instruction.  */

static bool
mips_mode_ok_for_mov_fmt_p (enum machine_mode mode)
{
  switch (mode)
    {
    case SFmode:
      return TARGET_HARD_FLOAT;

    case DFmode:
      return TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT;

    case V2SFmode:
      return TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT;

    default:
      return false;
    }
}

/* Implement MODES_TIEABLE_P.  */

bool
mips_modes_tieable_p (enum machine_mode mode1, enum machine_mode mode2)
{
  /* FPRs allow no mode punning, so it's not worth tying modes if we'd
     prefer to put one of them in FPRs.  */
  return (mode1 == mode2
	  || (!mips_mode_ok_for_mov_fmt_p (mode1)
	      && !mips_mode_ok_for_mov_fmt_p (mode2)));
}

/* Implement TARGET_PREFERRED_RELOAD_CLASS.  */

static reg_class_t
mips_preferred_reload_class (rtx x, reg_class_t rclass)
{
  if (mips_dangerous_for_la25_p (x) && reg_class_subset_p (LEA_REGS, rclass))
    return LEA_REGS;

  if (reg_class_subset_p (FP_REGS, rclass)
      && mips_mode_ok_for_mov_fmt_p (GET_MODE (x)))
    return FP_REGS;

  if (reg_class_subset_p (GR_REGS, rclass))
    rclass = GR_REGS;

  if (TARGET_MIPS16 && reg_class_subset_p (M16_REGS, rclass))
    rclass = M16_REGS;

  return rclass;
}

/* RCLASS is a class involved in a REGISTER_MOVE_COST calculation.
   Return a "canonical" class to represent it in later calculations.  */

static reg_class_t
mips_canonicalize_move_class (reg_class_t rclass)
{
  /* All moves involving accumulator registers have the same cost.  */
  if (reg_class_subset_p (rclass, ACC_REGS))
    rclass = ACC_REGS;

  /* Likewise promote subclasses of general registers to the most
     interesting containing class.  */
  if (TARGET_MIPS16 && reg_class_subset_p (rclass, M16_REGS))
    rclass = M16_REGS;
  else if (reg_class_subset_p (rclass, GENERAL_REGS))
    rclass = GENERAL_REGS;

  return rclass;
}

/* Return the cost of moving a value of mode MODE from a register of
   class FROM to a GPR.  Return 0 for classes that are unions of other
   classes handled by this function.  */

static int
mips_move_to_gpr_cost (enum machine_mode mode ATTRIBUTE_UNUSED,
		       reg_class_t from)
{
  switch (from)
    {
    case GENERAL_REGS:
      /* A MIPS16 MOVE instruction, or a non-MIPS16 MOVE macro.  */
      return 2;

    case ACC_REGS:
      /* MFLO and MFHI.  */
      return 6;

    case FP_REGS:
      /* MFC1, etc.  */
      return 4;

    case ST_REGS:
      /* LUI followed by MOVF.  */
      return 4;

    case COP0_REGS:
    case COP2_REGS:
    case COP3_REGS:
      /* This choice of value is historical.  */
      return 5;

    default:
      return 0;
    }
}

/* Return the cost of moving a value of mode MODE from a GPR to a
   register of class TO.  Return 0 for classes that are unions of
   other classes handled by this function.  */

static int
mips_move_from_gpr_cost (enum machine_mode mode, reg_class_t to)
{
  switch (to)
    {
    case GENERAL_REGS:
      /* A MIPS16 MOVE instruction, or a non-MIPS16 MOVE macro.  */
      return 2;

    case ACC_REGS:
      /* MTLO and MTHI.  */
      return 6;

    case FP_REGS:
      /* MTC1, etc.  */
      return 4;

    case ST_REGS:
      /* A secondary reload through an FPR scratch.  */
      return (mips_register_move_cost (mode, GENERAL_REGS, FP_REGS)
	      + mips_register_move_cost (mode, FP_REGS, ST_REGS));

    case COP0_REGS:
    case COP2_REGS:
    case COP3_REGS:
      /* This choice of value is historical.  */
      return 5;

    default:
      return 0;
    }
}

/* Implement TARGET_REGISTER_MOVE_COST.  Return 0 for classes that are the
   maximum of the move costs for subclasses; regclass will work out
   the maximum for us.  */

static int
mips_register_move_cost (enum machine_mode mode,
			 reg_class_t from, reg_class_t to)
{
  reg_class_t dregs;
  int cost1, cost2;

  from = mips_canonicalize_move_class (from);
  to = mips_canonicalize_move_class (to);

  /* Handle moves that can be done without using general-purpose registers.  */
  if (from == FP_REGS)
    {
      if (to == FP_REGS && mips_mode_ok_for_mov_fmt_p (mode))
	/* MOV.FMT.  */
	return 4;
      if (to == ST_REGS)
	/* The sequence generated by mips_expand_fcc_reload.  */
	return 8;
    }

  /* Handle cases in which only one class deviates from the ideal.  */
  dregs = TARGET_MIPS16 ? M16_REGS : GENERAL_REGS;
  if (from == dregs)
    return mips_move_from_gpr_cost (mode, to);
  if (to == dregs)
    return mips_move_to_gpr_cost (mode, from);

  /* Handles cases that require a GPR temporary.  */
  cost1 = mips_move_to_gpr_cost (mode, from);
  if (cost1 != 0)
    {
      cost2 = mips_move_from_gpr_cost (mode, to);
      if (cost2 != 0)
	return cost1 + cost2;
    }

  return 0;
}

/* Implement TARGET_MEMORY_MOVE_COST.  */

static int
mips_memory_move_cost (enum machine_mode mode, reg_class_t rclass, bool in)
{
  return (mips_cost->memory_latency
	  + memory_move_secondary_cost (mode, rclass, in));
} 

/* Implement TARGET_IRA_COVER_CLASSES.  */

static const reg_class_t *
mips_ira_cover_classes (void)
{
  static const reg_class_t acc_classes[] = {
    GR_AND_ACC_REGS, FP_REGS, COP0_REGS, COP2_REGS, COP3_REGS,
    ST_REGS, LIM_REG_CLASSES
  };
  static const reg_class_t no_acc_classes[] = {
    GR_REGS, FP_REGS, COP0_REGS, COP2_REGS, COP3_REGS,
    ST_REGS, LIM_REG_CLASSES
  };

  /* Don't allow the register allocators to use LO and HI in MIPS16 mode,
     which has no MTLO or MTHI instructions.  Also, using GR_AND_ACC_REGS
     as a cover class only works well when we keep per-register costs.
     Using it when not optimizing can cause us to think accumulators
     have the same cost as GPRs in cases where GPRs are actually much
     cheaper.  */
  return TARGET_MIPS16 || !optimize ? no_acc_classes : acc_classes;
}

/* Return the register class required for a secondary register when
   copying between one of the registers in RCLASS and value X, which
   has mode MODE.  X is the source of the move if IN_P, otherwise it
   is the destination.  Return NO_REGS if no secondary register is
   needed.  */

enum reg_class
mips_secondary_reload_class (enum reg_class rclass,
			     enum machine_mode mode, rtx x, bool in_p)
{
  int regno;

  /* If X is a constant that cannot be loaded into $25, it must be loaded
     into some other GPR.  No other register class allows a direct move.  */
  if (mips_dangerous_for_la25_p (x))
    return reg_class_subset_p (rclass, LEA_REGS) ? NO_REGS : LEA_REGS;

  regno = true_regnum (x);
  if (TARGET_MIPS16)
    {
      /* In MIPS16 mode, every move must involve a member of M16_REGS.  */
      if (!reg_class_subset_p (rclass, M16_REGS) && !M16_REG_P (regno))
	return M16_REGS;

      return NO_REGS;
    }

  /* Copying from accumulator registers to anywhere other than a general
     register requires a temporary general register.  */
  if (reg_class_subset_p (rclass, ACC_REGS))
    return GP_REG_P (regno) ? NO_REGS : GR_REGS;
  if (ACC_REG_P (regno))
    return reg_class_subset_p (rclass, GR_REGS) ? NO_REGS : GR_REGS;

  /* We can only copy a value to a condition code register from a
     floating-point register, and even then we require a scratch
     floating-point register.  We can only copy a value out of a
     condition-code register into a general register.  */
  if (reg_class_subset_p (rclass, ST_REGS))
    {
      if (in_p)
	return FP_REGS;
      return GP_REG_P (regno) ? NO_REGS : GR_REGS;
    }
  if (ST_REG_P (regno))
    {
      if (!in_p)
	return FP_REGS;
      return reg_class_subset_p (rclass, GR_REGS) ? NO_REGS : GR_REGS;
    }

  if (reg_class_subset_p (rclass, FP_REGS))
    {
      if (MEM_P (x)
	  && (GET_MODE_SIZE (mode) == 4 || GET_MODE_SIZE (mode) == 8))
	/* In this case we can use lwc1, swc1, ldc1 or sdc1.  We'll use
	   pairs of lwc1s and swc1s if ldc1 and sdc1 are not supported.  */
	return NO_REGS;

      if (GP_REG_P (regno) || x == CONST0_RTX (mode))
	/* In this case we can use mtc1, mfc1, dmtc1 or dmfc1.  */
	return NO_REGS;

      if (CONSTANT_P (x) && !targetm.cannot_force_const_mem (x))
	/* We can force the constant to memory and use lwc1
	   and ldc1.  As above, we will use pairs of lwc1s if
	   ldc1 is not supported.  */
	return NO_REGS;

      if (FP_REG_P (regno) && mips_mode_ok_for_mov_fmt_p (mode))
	/* In this case we can use mov.fmt.  */
	return NO_REGS;

      /* Otherwise, we need to reload through an integer register.  */
      return GR_REGS;
    }
  if (FP_REG_P (regno))
    return reg_class_subset_p (rclass, GR_REGS) ? NO_REGS : GR_REGS;

  return NO_REGS;
}

/* Implement TARGET_MODE_REP_EXTENDED.  */

static int
mips_mode_rep_extended (enum machine_mode mode, enum machine_mode mode_rep)
{
  /* On 64-bit targets, SImode register values are sign-extended to DImode.  */
  if (TARGET_64BIT && mode == SImode && mode_rep == DImode)
    return SIGN_EXTEND;

  return UNKNOWN;
}

/* Implement TARGET_VALID_POINTER_MODE.  */

static bool
mips_valid_pointer_mode (enum machine_mode mode)
{
  return mode == SImode || (TARGET_64BIT && mode == DImode);
}

/* Implement TARGET_VECTOR_MODE_SUPPORTED_P.  */

static bool
mips_vector_mode_supported_p (enum machine_mode mode)
{
  switch (mode)
    {
    case V2SFmode:
      return TARGET_PAIRED_SINGLE_FLOAT;

    case V2HImode:
    case V4QImode:
    case V2HQmode:
    case V2UHQmode:
    case V2HAmode:
    case V2UHAmode:
    case V4QQmode:
    case V4UQQmode:
      return TARGET_DSP;

    case V2SImode:
    case V4HImode:
    case V8QImode:
      return TARGET_LOONGSON_VECTORS;

    default:
      return false;
    }
}

/* Implement TARGET_SCALAR_MODE_SUPPORTED_P.  */

static bool
mips_scalar_mode_supported_p (enum machine_mode mode)
{
  if (ALL_FIXED_POINT_MODE_P (mode)
      && GET_MODE_PRECISION (mode) <= 2 * BITS_PER_WORD)
    return true;

  return default_scalar_mode_supported_p (mode);
}

/* Implement TARGET_VECTORIZE_PREFERRED_SIMD_MODE.  */

static enum machine_mode
mips_preferred_simd_mode (enum machine_mode mode ATTRIBUTE_UNUSED)
{
  if (TARGET_PAIRED_SINGLE_FLOAT
      && mode == SFmode)
    return V2SFmode;
  return word_mode;
}

/* Implement TARGET_INIT_LIBFUNCS.  */

static void
mips_init_libfuncs (void)
{
  if (TARGET_FIX_VR4120)
    {
      /* Register the special divsi3 and modsi3 functions needed to work
	 around VR4120 division errata.  */
      set_optab_libfunc (sdiv_optab, SImode, "__vr4120_divsi3");
      set_optab_libfunc (smod_optab, SImode, "__vr4120_modsi3");
    }

  if (TARGET_MIPS16 && TARGET_HARD_FLOAT_ABI)
    {
      /* Register the MIPS16 -mhard-float stubs.  */
      set_optab_libfunc (add_optab, SFmode, "__mips16_addsf3");
      set_optab_libfunc (sub_optab, SFmode, "__mips16_subsf3");
      set_optab_libfunc (smul_optab, SFmode, "__mips16_mulsf3");
      set_optab_libfunc (sdiv_optab, SFmode, "__mips16_divsf3");

      set_optab_libfunc (eq_optab, SFmode, "__mips16_eqsf2");
      set_optab_libfunc (ne_optab, SFmode, "__mips16_nesf2");
      set_optab_libfunc (gt_optab, SFmode, "__mips16_gtsf2");
      set_optab_libfunc (ge_optab, SFmode, "__mips16_gesf2");
      set_optab_libfunc (lt_optab, SFmode, "__mips16_ltsf2");
      set_optab_libfunc (le_optab, SFmode, "__mips16_lesf2");
      set_optab_libfunc (unord_optab, SFmode, "__mips16_unordsf2");

      set_conv_libfunc (sfix_optab, SImode, SFmode, "__mips16_fix_truncsfsi");
      set_conv_libfunc (sfloat_optab, SFmode, SImode, "__mips16_floatsisf");
      set_conv_libfunc (ufloat_optab, SFmode, SImode, "__mips16_floatunsisf");

      if (TARGET_DOUBLE_FLOAT)
	{
	  set_optab_libfunc (add_optab, DFmode, "__mips16_adddf3");
	  set_optab_libfunc (sub_optab, DFmode, "__mips16_subdf3");
	  set_optab_libfunc (smul_optab, DFmode, "__mips16_muldf3");
	  set_optab_libfunc (sdiv_optab, DFmode, "__mips16_divdf3");

	  set_optab_libfunc (eq_optab, DFmode, "__mips16_eqdf2");
	  set_optab_libfunc (ne_optab, DFmode, "__mips16_nedf2");
	  set_optab_libfunc (gt_optab, DFmode, "__mips16_gtdf2");
	  set_optab_libfunc (ge_optab, DFmode, "__mips16_gedf2");
	  set_optab_libfunc (lt_optab, DFmode, "__mips16_ltdf2");
	  set_optab_libfunc (le_optab, DFmode, "__mips16_ledf2");
	  set_optab_libfunc (unord_optab, DFmode, "__mips16_unorddf2");

	  set_conv_libfunc (sext_optab, DFmode, SFmode,
			    "__mips16_extendsfdf2");
	  set_conv_libfunc (trunc_optab, SFmode, DFmode,
			    "__mips16_truncdfsf2");
	  set_conv_libfunc (sfix_optab, SImode, DFmode,
			    "__mips16_fix_truncdfsi");
	  set_conv_libfunc (sfloat_optab, DFmode, SImode,
			    "__mips16_floatsidf");
	  set_conv_libfunc (ufloat_optab, DFmode, SImode,
			    "__mips16_floatunsidf");
	}
    }

  /* The MIPS16 ISA does not have an encoding for "sync", so we rely
     on an external non-MIPS16 routine to implement __sync_synchronize.  */
  if (TARGET_MIPS16)
    synchronize_libfunc = init_one_libfunc ("__sync_synchronize");
}

/* Build up a multi-insn sequence that loads label TARGET into $AT.  */

static void
mips_process_load_label (rtx target)
{
  rtx base, gp, intop;
  HOST_WIDE_INT offset;

  mips_multi_start ();
  switch (mips_abi)
    {
    case ABI_N32:
      mips_multi_add_insn ("lw\t%@,%%got_page(%0)(%+)", target, 0);
      mips_multi_add_insn ("addiu\t%@,%@,%%got_ofst(%0)", target, 0);
      break;

    case ABI_64:
      mips_multi_add_insn ("ld\t%@,%%got_page(%0)(%+)", target, 0);
      mips_multi_add_insn ("daddiu\t%@,%@,%%got_ofst(%0)", target, 0);
      break;

    default:
      gp = pic_offset_table_rtx;
      if (mips_cfun_has_cprestore_slot_p ())
	{
	  gp = gen_rtx_REG (Pmode, AT_REGNUM);
	  mips_get_cprestore_base_and_offset (&base, &offset, true);
	  if (!SMALL_OPERAND (offset))
	    {
	      intop = GEN_INT (CONST_HIGH_PART (offset));
	      mips_multi_add_insn ("lui\t%0,%1", gp, intop, 0);
	      mips_multi_add_insn ("addu\t%0,%0,%1", gp, base, 0);

	      base = gp;
	      offset = CONST_LOW_PART (offset);
	    }
	  intop = GEN_INT (offset);
	  if (ISA_HAS_LOAD_DELAY)
	    mips_multi_add_insn ("lw\t%0,%1(%2)%#", gp, intop, base, 0);
	  else
	    mips_multi_add_insn ("lw\t%0,%1(%2)", gp, intop, base, 0);
	}
      if (ISA_HAS_LOAD_DELAY)
	mips_multi_add_insn ("lw\t%@,%%got(%0)(%1)%#", target, gp, 0);
      else
	mips_multi_add_insn ("lw\t%@,%%got(%0)(%1)", target, gp, 0);
      mips_multi_add_insn ("addiu\t%@,%@,%%lo(%0)", target, 0);
      break;
    }
}

/* Return the number of instructions needed to load a label into $AT.  */

static unsigned int
mips_load_label_num_insns (void)
{
  if (cfun->machine->load_label_num_insns == 0)
    {
      mips_process_load_label (pc_rtx);
      cfun->machine->load_label_num_insns = mips_multi_num_insns;
    }
  return cfun->machine->load_label_num_insns;
}

/* Emit an asm sequence to start a noat block and load the address
   of a label into $1.  */

void
mips_output_load_label (rtx target)
{
  mips_push_asm_switch (&mips_noat);
  if (TARGET_EXPLICIT_RELOCS)
    {
      mips_process_load_label (target);
      mips_multi_write ();
    }
  else
    {
      if (Pmode == DImode)
	output_asm_insn ("dla\t%@,%0", &target);
      else
	output_asm_insn ("la\t%@,%0", &target);
    }
}

/* Return the length of INSN.  LENGTH is the initial length computed by
   attributes in the machine-description file.  */

int
mips_adjust_insn_length (rtx insn, int length)
{
  /* mips.md uses MAX_PIC_BRANCH_LENGTH as a placeholder for the length
     of a PIC long-branch sequence.  Substitute the correct value.  */
  if (length == MAX_PIC_BRANCH_LENGTH
      && INSN_CODE (insn) >= 0
      && get_attr_type (insn) == TYPE_BRANCH)
    {
      /* Add the branch-over instruction and its delay slot, if this
	 is a conditional branch.  */
      length = simplejump_p (insn) ? 0 : 8;

      /* Load the label into $AT and jump to it.  Ignore the delay
	 slot of the jump.  */
      length += 4 * mips_load_label_num_insns() + 4;
    }

  /* A unconditional jump has an unfilled delay slot if it is not part
     of a sequence.  A conditional jump normally has a delay slot, but
     does not on MIPS16.  */
  if (CALL_P (insn) || (TARGET_MIPS16 ? simplejump_p (insn) : JUMP_P (insn)))
    length += 4;

  /* See how many nops might be needed to avoid hardware hazards.  */
  if (!cfun->machine->ignore_hazard_length_p && INSN_CODE (insn) >= 0)
    switch (get_attr_hazard (insn))
      {
      case HAZARD_NONE:
	break;

      case HAZARD_DELAY:
	length += 4;
	break;

      case HAZARD_HILO:
	length += 8;
	break;
      }

  /* In order to make it easier to share MIPS16 and non-MIPS16 patterns,
     the .md file length attributes are 4-based for both modes.
     Adjust the MIPS16 ones here.  */
  if (TARGET_MIPS16)
    length /= 2;

  return length;
}

/* Return the assembly code for INSN, which has the operands given by
   OPERANDS, and which branches to OPERANDS[0] if some condition is true.
   BRANCH_IF_TRUE is the asm template that should be used if OPERANDS[0]
   is in range of a direct branch.  BRANCH_IF_FALSE is an inverted
   version of BRANCH_IF_TRUE.  */

const char *
mips_output_conditional_branch (rtx insn, rtx *operands,
				const char *branch_if_true,
				const char *branch_if_false)
{
  unsigned int length;
  rtx taken, not_taken;

  gcc_assert (LABEL_P (operands[0]));

  length = get_attr_length (insn);
  if (length <= 8)
    {
      /* Just a simple conditional branch.  */
      mips_branch_likely = (final_sequence && INSN_ANNULLED_BRANCH_P (insn));
      return branch_if_true;
    }

  /* Generate a reversed branch around a direct jump.  This fallback does
     not use branch-likely instructions.  */
  mips_branch_likely = false;
  not_taken = gen_label_rtx ();
  taken = operands[0];

  /* Generate the reversed branch to NOT_TAKEN.  */
  operands[0] = not_taken;
  output_asm_insn (branch_if_false, operands);

  /* If INSN has a delay slot, we must provide delay slots for both the
     branch to NOT_TAKEN and the conditional jump.  We must also ensure
     that INSN's delay slot is executed in the appropriate cases.  */
  if (final_sequence)
    {
      /* This first delay slot will always be executed, so use INSN's
	 delay slot if is not annulled.  */
      if (!INSN_ANNULLED_BRANCH_P (insn))
	{
	  final_scan_insn (XVECEXP (final_sequence, 0, 1),
			   asm_out_file, optimize, 1, NULL);
	  INSN_DELETED_P (XVECEXP (final_sequence, 0, 1)) = 1;
	}
      else
	output_asm_insn ("nop", 0);
      fprintf (asm_out_file, "\n");
    }

  /* Output the unconditional branch to TAKEN.  */
  if (TARGET_ABSOLUTE_JUMPS)
    output_asm_insn (MIPS_ABSOLUTE_JUMP ("j\t%0%/"), &taken);
  else
    {
      mips_output_load_label (taken);
      output_asm_insn ("jr\t%@%]%/", 0);
    }

  /* Now deal with its delay slot; see above.  */
  if (final_sequence)
    {
      /* This delay slot will only be executed if the branch is taken.
	 Use INSN's delay slot if is annulled.  */
      if (INSN_ANNULLED_BRANCH_P (insn))
	{
	  final_scan_insn (XVECEXP (final_sequence, 0, 1),
			   asm_out_file, optimize, 1, NULL);
	  INSN_DELETED_P (XVECEXP (final_sequence, 0, 1)) = 1;
	}
      else
	output_asm_insn ("nop", 0);
      fprintf (asm_out_file, "\n");
    }

  /* Output NOT_TAKEN.  */
  targetm.asm_out.internal_label (asm_out_file, "L",
				  CODE_LABEL_NUMBER (not_taken));
  return "";
}

/* Return the assembly code for INSN, which branches to OPERANDS[0]
   if some ordering condition is true.  The condition is given by
   OPERANDS[1] if !INVERTED_P, otherwise it is the inverse of
   OPERANDS[1].  OPERANDS[2] is the comparison's first operand;
   its second is always zero.  */

const char *
mips_output_order_conditional_branch (rtx insn, rtx *operands, bool inverted_p)
{
  const char *branch[2];

  /* Make BRANCH[1] branch to OPERANDS[0] when the condition is true.
     Make BRANCH[0] branch on the inverse condition.  */
  switch (GET_CODE (operands[1]))
    {
      /* These cases are equivalent to comparisons against zero.  */
    case LEU:
      inverted_p = !inverted_p;
      /* Fall through.  */
    case GTU:
      branch[!inverted_p] = MIPS_BRANCH ("bne", "%2,%.,%0");
      branch[inverted_p] = MIPS_BRANCH ("beq", "%2,%.,%0");
      break;

      /* These cases are always true or always false.  */
    case LTU:
      inverted_p = !inverted_p;
      /* Fall through.  */
    case GEU:
      branch[!inverted_p] = MIPS_BRANCH ("beq", "%.,%.,%0");
      branch[inverted_p] = MIPS_BRANCH ("bne", "%.,%.,%0");
      break;

    default:
      branch[!inverted_p] = MIPS_BRANCH ("b%C1z", "%2,%0");
      branch[inverted_p] = MIPS_BRANCH ("b%N1z", "%2,%0");
      break;
    }
  return mips_output_conditional_branch (insn, operands, branch[1], branch[0]);
}

/* Start a block of code that needs access to the LL, SC and SYNC
   instructions.  */

static void
mips_start_ll_sc_sync_block (void)
{
  if (!ISA_HAS_LL_SC)
    {
      output_asm_insn (".set\tpush", 0);
      output_asm_insn (".set\tmips2", 0);
    }
}

/* End a block started by mips_start_ll_sc_sync_block.  */

static void
mips_end_ll_sc_sync_block (void)
{
  if (!ISA_HAS_LL_SC)
    output_asm_insn (".set\tpop", 0);
}

/* Output and/or return the asm template for a sync instruction.  */

const char *
mips_output_sync (void)
{
  mips_start_ll_sc_sync_block ();
  output_asm_insn ("sync", 0);
  mips_end_ll_sc_sync_block ();
  return "";
}

/* Return the asm template associated with sync_insn1 value TYPE.
   IS_64BIT_P is true if we want a 64-bit rather than 32-bit operation.  */

static const char *
mips_sync_insn1_template (enum attr_sync_insn1 type, bool is_64bit_p)
{
  switch (type)
    {
    case SYNC_INSN1_MOVE:
      return "move\t%0,%z2";
    case SYNC_INSN1_LI:
      return "li\t%0,%2";
    case SYNC_INSN1_ADDU:
      return is_64bit_p ? "daddu\t%0,%1,%z2" : "addu\t%0,%1,%z2";
    case SYNC_INSN1_ADDIU:
      return is_64bit_p ? "daddiu\t%0,%1,%2" : "addiu\t%0,%1,%2";
    case SYNC_INSN1_SUBU:
      return is_64bit_p ? "dsubu\t%0,%1,%z2" : "subu\t%0,%1,%z2";
    case SYNC_INSN1_AND:
      return "and\t%0,%1,%z2";
    case SYNC_INSN1_ANDI:
      return "andi\t%0,%1,%2";
    case SYNC_INSN1_OR:
      return "or\t%0,%1,%z2";
    case SYNC_INSN1_ORI:
      return "ori\t%0,%1,%2";
    case SYNC_INSN1_XOR:
      return "xor\t%0,%1,%z2";
    case SYNC_INSN1_XORI:
      return "xori\t%0,%1,%2";
    }
  gcc_unreachable ();
}

/* Return the asm template associated with sync_insn2 value TYPE.  */

static const char *
mips_sync_insn2_template (enum attr_sync_insn2 type)
{
  switch (type)
    {
    case SYNC_INSN2_NOP:
      gcc_unreachable ();
    case SYNC_INSN2_AND:
      return "and\t%0,%1,%z2";
    case SYNC_INSN2_XOR:
      return "xor\t%0,%1,%z2";
    case SYNC_INSN2_NOT:
      return "nor\t%0,%1,%.";
    }
  gcc_unreachable ();
}

/* OPERANDS are the operands to a sync loop instruction and INDEX is
   the value of the one of the sync_* attributes.  Return the operand
   referred to by the attribute, or DEFAULT_VALUE if the insn doesn't
   have the associated attribute.  */

static rtx
mips_get_sync_operand (rtx *operands, int index, rtx default_value)
{
  if (index > 0)
    default_value = operands[index - 1];
  return default_value;
}

/* INSN is a sync loop with operands OPERANDS.  Build up a multi-insn
   sequence for it.  */

static void
mips_process_sync_loop (rtx insn, rtx *operands)
{
  rtx at, mem, oldval, newval, inclusive_mask, exclusive_mask;
  rtx required_oldval, insn1_op2, tmp1, tmp2, tmp3;
  unsigned int tmp3_insn;
  enum attr_sync_insn1 insn1;
  enum attr_sync_insn2 insn2;
  bool is_64bit_p;

  /* Read an operand from the sync_WHAT attribute and store it in
     variable WHAT.  DEFAULT is the default value if no attribute
     is specified.  */
#define READ_OPERAND(WHAT, DEFAULT) \
  WHAT = mips_get_sync_operand (operands, (int) get_attr_sync_##WHAT (insn), \
  				DEFAULT)

  /* Read the memory.  */
  READ_OPERAND (mem, 0);
  gcc_assert (mem);
  is_64bit_p = (GET_MODE_BITSIZE (GET_MODE (mem)) == 64);

  /* Read the other attributes.  */
  at = gen_rtx_REG (GET_MODE (mem), AT_REGNUM);
  READ_OPERAND (oldval, at);
  READ_OPERAND (newval, at);
  READ_OPERAND (inclusive_mask, 0);
  READ_OPERAND (exclusive_mask, 0);
  READ_OPERAND (required_oldval, 0);
  READ_OPERAND (insn1_op2, 0);
  insn1 = get_attr_sync_insn1 (insn);
  insn2 = get_attr_sync_insn2 (insn);

  mips_multi_start ();

  /* Output the release side of the memory barrier.  */
  if (get_attr_sync_release_barrier (insn) == SYNC_RELEASE_BARRIER_YES)
    {
      if (required_oldval == 0 && TARGET_OCTEON)
	{
	  /* Octeon doesn't reorder reads, so a full barrier can be
	     created by using SYNCW to order writes combined with the
	     write from the following SC.  When the SC successfully
	     completes, we know that all preceding writes are also
	     committed to the coherent memory system.  It is possible
	     for a single SYNCW to fail, but a pair of them will never
	     fail, so we use two.  */
	  mips_multi_add_insn ("syncw", NULL);
	  mips_multi_add_insn ("syncw", NULL);
	}
      else
	mips_multi_add_insn ("sync", NULL);
    }

  /* Output the branch-back label.  */
  mips_multi_add_label ("1:");

  /* OLDVAL = *MEM.  */
  mips_multi_add_insn (is_64bit_p ? "lld\t%0,%1" : "ll\t%0,%1",
		       oldval, mem, NULL);

  /* if ((OLDVAL & INCLUSIVE_MASK) != REQUIRED_OLDVAL) goto 2.  */
  if (required_oldval)
    {
      if (inclusive_mask == 0)
	tmp1 = oldval;
      else
	{
	  gcc_assert (oldval != at);
	  mips_multi_add_insn ("and\t%0,%1,%2",
			       at, oldval, inclusive_mask, NULL);
	  tmp1 = at;
	}
      mips_multi_add_insn ("bne\t%0,%z1,2f", tmp1, required_oldval, NULL);
    }

  /* $TMP1 = OLDVAL & EXCLUSIVE_MASK.  */
  if (exclusive_mask == 0)
    tmp1 = const0_rtx;
  else
    {
      gcc_assert (oldval != at);
      mips_multi_add_insn ("and\t%0,%1,%z2",
			   at, oldval, exclusive_mask, NULL);
      tmp1 = at;
    }

  /* $TMP2 = INSN1 (OLDVAL, INSN1_OP2).

     We can ignore moves if $TMP4 != INSN1_OP2, since we'll still emit
     at least one instruction in that case.  */
  if (insn1 == SYNC_INSN1_MOVE
      && (tmp1 != const0_rtx || insn2 != SYNC_INSN2_NOP))
    tmp2 = insn1_op2;
  else
    {
      mips_multi_add_insn (mips_sync_insn1_template (insn1, is_64bit_p),
			   newval, oldval, insn1_op2, NULL);
      tmp2 = newval;
    }

  /* $TMP3 = INSN2 ($TMP2, INCLUSIVE_MASK).  */
  if (insn2 == SYNC_INSN2_NOP)
    tmp3 = tmp2;
  else
    {
      mips_multi_add_insn (mips_sync_insn2_template (insn2),
			   newval, tmp2, inclusive_mask, NULL);
      tmp3 = newval;
    }
  tmp3_insn = mips_multi_last_index ();

  /* $AT = $TMP1 | $TMP3.  */
  if (tmp1 == const0_rtx || tmp3 == const0_rtx)
    {
      mips_multi_set_operand (tmp3_insn, 0, at);
      tmp3 = at;
    }
  else
    {
      gcc_assert (tmp1 != tmp3);
      mips_multi_add_insn ("or\t%0,%1,%2", at, tmp1, tmp3, NULL);
    }

  /* if (!commit (*MEM = $AT)) goto 1.

     This will sometimes be a delayed branch; see the write code below
     for details.  */
  mips_multi_add_insn (is_64bit_p ? "scd\t%0,%1" : "sc\t%0,%1", at, mem, NULL);
  mips_multi_add_insn ("beq%?\t%0,%.,1b", at, NULL);

  /* if (INSN1 != MOVE && INSN1 != LI) NEWVAL = $TMP3 [delay slot].  */
  if (insn1 != SYNC_INSN1_MOVE && insn1 != SYNC_INSN1_LI && tmp3 != newval)
    {
      mips_multi_copy_insn (tmp3_insn);
      mips_multi_set_operand (mips_multi_last_index (), 0, newval);
    }
  else
    mips_multi_add_insn ("nop", NULL);

  /* Output the acquire side of the memory barrier.  */
  if (TARGET_SYNC_AFTER_SC)
    mips_multi_add_insn ("sync", NULL);

  /* Output the exit label, if needed.  */
  if (required_oldval)
    mips_multi_add_label ("2:");

#undef READ_OPERAND
}

/* Output and/or return the asm template for sync loop INSN, which has
   the operands given by OPERANDS.  */

const char *
mips_output_sync_loop (rtx insn, rtx *operands)
{
  mips_process_sync_loop (insn, operands);

  /* Use branch-likely instructions to work around the LL/SC R10000
     errata.  */
  mips_branch_likely = TARGET_FIX_R10000;

  mips_push_asm_switch (&mips_noreorder);
  mips_push_asm_switch (&mips_nomacro);
  mips_push_asm_switch (&mips_noat);
  mips_start_ll_sc_sync_block ();

  mips_multi_write ();

  mips_end_ll_sc_sync_block ();
  mips_pop_asm_switch (&mips_noat);
  mips_pop_asm_switch (&mips_nomacro);
  mips_pop_asm_switch (&mips_noreorder);

  return "";
}

/* Return the number of individual instructions in sync loop INSN,
   which has the operands given by OPERANDS.  */

unsigned int
mips_sync_loop_insns (rtx insn, rtx *operands)
{
  mips_process_sync_loop (insn, operands);
  return mips_multi_num_insns;
}

/* Return the assembly code for DIV or DDIV instruction DIVISION, which has
   the operands given by OPERANDS.  Add in a divide-by-zero check if needed.

   When working around R4000 and R4400 errata, we need to make sure that
   the division is not immediately followed by a shift[1][2].  We also
   need to stop the division from being put into a branch delay slot[3].
   The easiest way to avoid both problems is to add a nop after the
   division.  When a divide-by-zero check is needed, this nop can be
   used to fill the branch delay slot.

   [1] If a double-word or a variable shift executes immediately
       after starting an integer division, the shift may give an
       incorrect result.  See quotations of errata #16 and #28 from
       "MIPS R4000PC/SC Errata, Processor Revision 2.2 and 3.0"
       in mips.md for details.

   [2] A similar bug to [1] exists for all revisions of the
       R4000 and the R4400 when run in an MC configuration.
       From "MIPS R4000MC Errata, Processor Revision 2.2 and 3.0":

       "19. In this following sequence:

		    ddiv		(or ddivu or div or divu)
		    dsll32		(or dsrl32, dsra32)

	    if an MPT stall occurs, while the divide is slipping the cpu
	    pipeline, then the following double shift would end up with an
	    incorrect result.

	    Workaround: The compiler needs to avoid generating any
	    sequence with divide followed by extended double shift."

       This erratum is also present in "MIPS R4400MC Errata, Processor
       Revision 1.0" and "MIPS R4400MC Errata, Processor Revision 2.0
       & 3.0" as errata #10 and #4, respectively.

   [3] From "MIPS R4000PC/SC Errata, Processor Revision 2.2 and 3.0"
       (also valid for MIPS R4000MC processors):

       "52. R4000SC: This bug does not apply for the R4000PC.

	    There are two flavors of this bug:

	    1) If the instruction just after divide takes an RF exception
	       (tlb-refill, tlb-invalid) and gets an instruction cache
	       miss (both primary and secondary) and the line which is
	       currently in secondary cache at this index had the first
	       data word, where the bits 5..2 are set, then R4000 would
	       get a wrong result for the div.

	    ##1
		    nop
		    div	r8, r9
		    -------------------		# end-of page. -tlb-refill
		    nop
	    ##2
		    nop
		    div	r8, r9
		    -------------------		# end-of page. -tlb-invalid
		    nop

	    2) If the divide is in the taken branch delay slot, where the
	       target takes RF exception and gets an I-cache miss for the
	       exception vector or where I-cache miss occurs for the
	       target address, under the above mentioned scenarios, the
	       div would get wrong results.

	    ##1
		    j	r2		# to next page mapped or unmapped
		    div	r8,r9		# this bug would be there as long
					# as there is an ICache miss and
		    nop			# the "data pattern" is present

	    ##2
		    beq	r0, r0, NextPage	# to Next page
		    div	r8,r9
		    nop

	    This bug is present for div, divu, ddiv, and ddivu
	    instructions.

	    Workaround: For item 1), OS could make sure that the next page
	    after the divide instruction is also mapped.  For item 2), the
	    compiler could make sure that the divide instruction is not in
	    the branch delay slot."

       These processors have PRId values of 0x00004220 and 0x00004300 for
       the R4000 and 0x00004400, 0x00004500 and 0x00004600 for the R4400.  */

const char *
mips_output_division (const char *division, rtx *operands)
{
  const char *s;

  s = division;
  if (TARGET_FIX_R4000 || TARGET_FIX_R4400)
    {
      output_asm_insn (s, operands);
      s = "nop";
    }
  if (TARGET_CHECK_ZERO_DIV)
    {
      if (TARGET_MIPS16)
	{
	  output_asm_insn (s, operands);
	  s = "bnez\t%2,1f\n\tbreak\t7\n1:";
	}
      else if (GENERATE_DIVIDE_TRAPS)
	{
	  /* Avoid long replay penalty on load miss by putting the trap before
	     the divide.  */
	  if (TUNE_74K)
	    output_asm_insn ("teq\t%2,%.,7", operands);
	  else
	    {
	      output_asm_insn (s, operands);
	      s = "teq\t%2,%.,7";
	    }
	}
      else
	{
	  output_asm_insn ("%(bne\t%2,%.,1f", operands);
	  output_asm_insn (s, operands);
	  s = "break\t7%)\n1:";
	}
    }
  return s;
}

/* Return true if IN_INSN is a multiply-add or multiply-subtract
   instruction and if OUT_INSN assigns to the accumulator operand.  */

bool
mips_linked_madd_p (rtx out_insn, rtx in_insn)
{
  rtx x;

  x = single_set (in_insn);
  if (x == 0)
    return false;

  x = SET_SRC (x);

  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 0)) == MULT
      && reg_set_p (XEXP (x, 1), out_insn))
    return true;

  if (GET_CODE (x) == MINUS
      && GET_CODE (XEXP (x, 1)) == MULT
      && reg_set_p (XEXP (x, 0), out_insn))
    return true;

  return false;
}

/* True if the dependency between OUT_INSN and IN_INSN is on the store
   data rather than the address.  We need this because the cprestore
   pattern is type "store", but is defined using an UNSPEC_VOLATILE,
   which causes the default routine to abort.  We just return false
   for that case.  */

bool
mips_store_data_bypass_p (rtx out_insn, rtx in_insn)
{
  if (GET_CODE (PATTERN (in_insn)) == UNSPEC_VOLATILE)
    return false;

  return !store_data_bypass_p (out_insn, in_insn);
}


/* Variables and flags used in scheduler hooks when tuning for
   Loongson 2E/2F.  */
static struct
{
  /* Variables to support Loongson 2E/2F round-robin [F]ALU1/2 dispatch
     strategy.  */

  /* If true, then next ALU1/2 instruction will go to ALU1.  */
  bool alu1_turn_p;

  /* If true, then next FALU1/2 unstruction will go to FALU1.  */
  bool falu1_turn_p;

  /* Codes to query if [f]alu{1,2}_core units are subscribed or not.  */
  int alu1_core_unit_code;
  int alu2_core_unit_code;
  int falu1_core_unit_code;
  int falu2_core_unit_code;

  /* True if current cycle has a multi instruction.
     This flag is used in mips_ls2_dfa_post_advance_cycle.  */
  bool cycle_has_multi_p;

  /* Instructions to subscribe ls2_[f]alu{1,2}_turn_enabled units.
     These are used in mips_ls2_dfa_post_advance_cycle to initialize
     DFA state.
     E.g., when alu1_turn_enabled_insn is issued it makes next ALU1/2
     instruction to go ALU1.  */
  rtx alu1_turn_enabled_insn;
  rtx alu2_turn_enabled_insn;
  rtx falu1_turn_enabled_insn;
  rtx falu2_turn_enabled_insn;
} mips_ls2;

/* Implement TARGET_SCHED_ADJUST_COST.  We assume that anti and output
   dependencies have no cost, except on the 20Kc where output-dependence
   is treated like input-dependence.  */

static int
mips_adjust_cost (rtx insn ATTRIBUTE_UNUSED, rtx link,
		  rtx dep ATTRIBUTE_UNUSED, int cost)
{
  if (REG_NOTE_KIND (link) == REG_DEP_OUTPUT
      && TUNE_20KC)
    return cost;
  if (REG_NOTE_KIND (link) != 0)
    return 0;
  return cost;
}

/* Return the number of instructions that can be issued per cycle.  */

static int
mips_issue_rate (void)
{
  switch (mips_tune)
    {
    case PROCESSOR_74KC:
    case PROCESSOR_74KF2_1:
    case PROCESSOR_74KF1_1:
    case PROCESSOR_74KF3_2:
      /* The 74k is not strictly quad-issue cpu, but can be seen as one
	 by the scheduler.  It can issue 1 ALU, 1 AGEN and 2 FPU insns,
	 but in reality only a maximum of 3 insns can be issued as
	 floating-point loads and stores also require a slot in the
	 AGEN pipe.  */
    case PROCESSOR_R10000:
      /* All R10K Processors are quad-issue (being the first MIPS
         processors to support this feature). */
      return 4;

    case PROCESSOR_20KC:
    case PROCESSOR_R4130:
    case PROCESSOR_R5400:
    case PROCESSOR_R5500:
    case PROCESSOR_R7000:
    case PROCESSOR_R9000:
    case PROCESSOR_OCTEON:
      return 2;

    case PROCESSOR_SB1:
    case PROCESSOR_SB1A:
      /* This is actually 4, but we get better performance if we claim 3.
	 This is partly because of unwanted speculative code motion with the
	 larger number, and partly because in most common cases we can't
	 reach the theoretical max of 4.  */
      return 3;

    case PROCESSOR_LOONGSON_2E:
    case PROCESSOR_LOONGSON_2F:
    case PROCESSOR_LOONGSON_3A:
      return 4;

    default:
      return 1;
    }
}

/* Implement TARGET_SCHED_INIT_DFA_POST_CYCLE_INSN hook for Loongson2.  */

static void
mips_ls2_init_dfa_post_cycle_insn (void)
{
  start_sequence ();
  emit_insn (gen_ls2_alu1_turn_enabled_insn ());
  mips_ls2.alu1_turn_enabled_insn = get_insns ();
  end_sequence ();

  start_sequence ();
  emit_insn (gen_ls2_alu2_turn_enabled_insn ());
  mips_ls2.alu2_turn_enabled_insn = get_insns ();
  end_sequence ();

  start_sequence ();
  emit_insn (gen_ls2_falu1_turn_enabled_insn ());
  mips_ls2.falu1_turn_enabled_insn = get_insns ();
  end_sequence ();

  start_sequence ();
  emit_insn (gen_ls2_falu2_turn_enabled_insn ());
  mips_ls2.falu2_turn_enabled_insn = get_insns ();
  end_sequence ();

  mips_ls2.alu1_core_unit_code = get_cpu_unit_code ("ls2_alu1_core");
  mips_ls2.alu2_core_unit_code = get_cpu_unit_code ("ls2_alu2_core");
  mips_ls2.falu1_core_unit_code = get_cpu_unit_code ("ls2_falu1_core");
  mips_ls2.falu2_core_unit_code = get_cpu_unit_code ("ls2_falu2_core");
}

/* Implement TARGET_SCHED_INIT_DFA_POST_CYCLE_INSN hook.
   Init data used in mips_dfa_post_advance_cycle.  */

static void
mips_init_dfa_post_cycle_insn (void)
{
  if (TUNE_LOONGSON_2EF)
    mips_ls2_init_dfa_post_cycle_insn ();
}

/* Initialize STATE when scheduling for Loongson 2E/2F.
   Support round-robin dispatch scheme by enabling only one of
   ALU1/ALU2 and one of FALU1/FALU2 units for ALU1/2 and FALU1/2 instructions
   respectively.  */

static void
mips_ls2_dfa_post_advance_cycle (state_t state)
{
  if (cpu_unit_reservation_p (state, mips_ls2.alu1_core_unit_code))
    {
      /* Though there are no non-pipelined ALU1 insns,
	 we can get an instruction of type 'multi' before reload.  */
      gcc_assert (mips_ls2.cycle_has_multi_p);
      mips_ls2.alu1_turn_p = false;
    }

  mips_ls2.cycle_has_multi_p = false;

  if (cpu_unit_reservation_p (state, mips_ls2.alu2_core_unit_code))
    /* We have a non-pipelined alu instruction in the core,
       adjust round-robin counter.  */
    mips_ls2.alu1_turn_p = true;

  if (mips_ls2.alu1_turn_p)
    {
      if (state_transition (state, mips_ls2.alu1_turn_enabled_insn) >= 0)
	gcc_unreachable ();
    }
  else
    {
      if (state_transition (state, mips_ls2.alu2_turn_enabled_insn) >= 0)
	gcc_unreachable ();
    }

  if (cpu_unit_reservation_p (state, mips_ls2.falu1_core_unit_code))
    {
      /* There are no non-pipelined FALU1 insns.  */
      gcc_unreachable ();
      mips_ls2.falu1_turn_p = false;
    }

  if (cpu_unit_reservation_p (state, mips_ls2.falu2_core_unit_code))
    /* We have a non-pipelined falu instruction in the core,
       adjust round-robin counter.  */
    mips_ls2.falu1_turn_p = true;

  if (mips_ls2.falu1_turn_p)
    {
      if (state_transition (state, mips_ls2.falu1_turn_enabled_insn) >= 0)
	gcc_unreachable ();
    }
  else
    {
      if (state_transition (state, mips_ls2.falu2_turn_enabled_insn) >= 0)
	gcc_unreachable ();
    }
}

/* Implement TARGET_SCHED_DFA_POST_ADVANCE_CYCLE.
   This hook is being called at the start of each cycle.  */

static void
mips_dfa_post_advance_cycle (void)
{
  if (TUNE_LOONGSON_2EF)
    mips_ls2_dfa_post_advance_cycle (curr_state);
}

/* Implement TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD.  This should
   be as wide as the scheduling freedom in the DFA.  */

static int
mips_multipass_dfa_lookahead (void)
{
  /* Can schedule up to 4 of the 6 function units in any one cycle.  */
  if (TUNE_SB1)
    return 4;

  if (TUNE_LOONGSON_2EF || TUNE_LOONGSON_3A)
    return 4;

  if (TUNE_OCTEON)
    return 2;

  return 0;
}

/* Remove the instruction at index LOWER from ready queue READY and
   reinsert it in front of the instruction at index HIGHER.  LOWER must
   be <= HIGHER.  */

static void
mips_promote_ready (rtx *ready, int lower, int higher)
{
  rtx new_head;
  int i;

  new_head = ready[lower];
  for (i = lower; i < higher; i++)
    ready[i] = ready[i + 1];
  ready[i] = new_head;
}

/* If the priority of the instruction at POS2 in the ready queue READY
   is within LIMIT units of that of the instruction at POS1, swap the
   instructions if POS2 is not already less than POS1.  */

static void
mips_maybe_swap_ready (rtx *ready, int pos1, int pos2, int limit)
{
  if (pos1 < pos2
      && INSN_PRIORITY (ready[pos1]) + limit >= INSN_PRIORITY (ready[pos2]))
    {
      rtx temp;

      temp = ready[pos1];
      ready[pos1] = ready[pos2];
      ready[pos2] = temp;
    }
}

/* Used by TUNE_MACC_CHAINS to record the last scheduled instruction
   that may clobber hi or lo.  */
static rtx mips_macc_chains_last_hilo;

/* A TUNE_MACC_CHAINS helper function.  Record that instruction INSN has
   been scheduled, updating mips_macc_chains_last_hilo appropriately.  */

static void
mips_macc_chains_record (rtx insn)
{
  if (get_attr_may_clobber_hilo (insn))
    mips_macc_chains_last_hilo = insn;
}

/* A TUNE_MACC_CHAINS helper function.  Search ready queue READY, which
   has NREADY elements, looking for a multiply-add or multiply-subtract
   instruction that is cumulative with mips_macc_chains_last_hilo.
   If there is one, promote it ahead of anything else that might
   clobber hi or lo.  */

static void
mips_macc_chains_reorder (rtx *ready, int nready)
{
  int i, j;

  if (mips_macc_chains_last_hilo != 0)
    for (i = nready - 1; i >= 0; i--)
      if (mips_linked_madd_p (mips_macc_chains_last_hilo, ready[i]))
	{
	  for (j = nready - 1; j > i; j--)
	    if (recog_memoized (ready[j]) >= 0
		&& get_attr_may_clobber_hilo (ready[j]))
	      {
		mips_promote_ready (ready, i, j);
		break;
	      }
	  break;
	}
}

/* The last instruction to be scheduled.  */
static rtx vr4130_last_insn;

/* A note_stores callback used by vr4130_true_reg_dependence_p.  DATA
   points to an rtx that is initially an instruction.  Nullify the rtx
   if the instruction uses the value of register X.  */

static void
vr4130_true_reg_dependence_p_1 (rtx x, const_rtx pat ATTRIBUTE_UNUSED,
				void *data)
{
  rtx *insn_ptr;

  insn_ptr = (rtx *) data;
  if (REG_P (x)
      && *insn_ptr != 0
      && reg_referenced_p (x, PATTERN (*insn_ptr)))
    *insn_ptr = 0;
}

/* Return true if there is true register dependence between vr4130_last_insn
   and INSN.  */

static bool
vr4130_true_reg_dependence_p (rtx insn)
{
  note_stores (PATTERN (vr4130_last_insn),
	       vr4130_true_reg_dependence_p_1, &insn);
  return insn == 0;
}

/* A TUNE_MIPS4130 helper function.  Given that INSN1 is at the head of
   the ready queue and that INSN2 is the instruction after it, return
   true if it is worth promoting INSN2 ahead of INSN1.  Look for cases
   in which INSN1 and INSN2 can probably issue in parallel, but for
   which (INSN2, INSN1) should be less sensitive to instruction
   alignment than (INSN1, INSN2).  See 4130.md for more details.  */

static bool
vr4130_swap_insns_p (rtx insn1, rtx insn2)
{
  sd_iterator_def sd_it;
  dep_t dep;

  /* Check for the following case:

     1) there is some other instruction X with an anti dependence on INSN1;
     2) X has a higher priority than INSN2; and
     3) X is an arithmetic instruction (and thus has no unit restrictions).

     If INSN1 is the last instruction blocking X, it would better to
     choose (INSN1, X) over (INSN2, INSN1).  */
  FOR_EACH_DEP (insn1, SD_LIST_FORW, sd_it, dep)
    if (DEP_TYPE (dep) == REG_DEP_ANTI
	&& INSN_PRIORITY (DEP_CON (dep)) > INSN_PRIORITY (insn2)
	&& recog_memoized (DEP_CON (dep)) >= 0
	&& get_attr_vr4130_class (DEP_CON (dep)) == VR4130_CLASS_ALU)
      return false;

  if (vr4130_last_insn != 0
      && recog_memoized (insn1) >= 0
      && recog_memoized (insn2) >= 0)
    {
      /* See whether INSN1 and INSN2 use different execution units,
	 or if they are both ALU-type instructions.  If so, they can
	 probably execute in parallel.  */
      enum attr_vr4130_class class1 = get_attr_vr4130_class (insn1);
      enum attr_vr4130_class class2 = get_attr_vr4130_class (insn2);
      if (class1 != class2 || class1 == VR4130_CLASS_ALU)
	{
	  /* If only one of the instructions has a dependence on
	     vr4130_last_insn, prefer to schedule the other one first.  */
	  bool dep1_p = vr4130_true_reg_dependence_p (insn1);
	  bool dep2_p = vr4130_true_reg_dependence_p (insn2);
	  if (dep1_p != dep2_p)
	    return dep1_p;

	  /* Prefer to schedule INSN2 ahead of INSN1 if vr4130_last_insn
	     is not an ALU-type instruction and if INSN1 uses the same
	     execution unit.  (Note that if this condition holds, we already
	     know that INSN2 uses a different execution unit.)  */
	  if (class1 != VR4130_CLASS_ALU
	      && recog_memoized (vr4130_last_insn) >= 0
	      && class1 == get_attr_vr4130_class (vr4130_last_insn))
	    return true;
	}
    }
  return false;
}

/* A TUNE_MIPS4130 helper function.  (READY, NREADY) describes a ready
   queue with at least two instructions.  Swap the first two if
   vr4130_swap_insns_p says that it could be worthwhile.  */

static void
vr4130_reorder (rtx *ready, int nready)
{
  if (vr4130_swap_insns_p (ready[nready - 1], ready[nready - 2]))
    mips_promote_ready (ready, nready - 2, nready - 1);
}

/* Record whether last 74k AGEN instruction was a load or store.  */
static enum attr_type mips_last_74k_agen_insn = TYPE_UNKNOWN;

/* Initialize mips_last_74k_agen_insn from INSN.  A null argument
   resets to TYPE_UNKNOWN state.  */

static void
mips_74k_agen_init (rtx insn)
{
  if (!insn || CALL_P (insn) || JUMP_P (insn))
    mips_last_74k_agen_insn = TYPE_UNKNOWN;
  else
    {
      enum attr_type type = get_attr_type (insn);
      if (type == TYPE_LOAD || type == TYPE_STORE)
	mips_last_74k_agen_insn = type;
    }
}

/* A TUNE_74K helper function.  The 74K AGEN pipeline likes multiple
   loads to be grouped together, and multiple stores to be grouped
   together.  Swap things around in the ready queue to make this happen.  */

static void
mips_74k_agen_reorder (rtx *ready, int nready)
{
  int i;
  int store_pos, load_pos;

  store_pos = -1;
  load_pos = -1;

  for (i = nready - 1; i >= 0; i--)
    {
      rtx insn = ready[i];
      if (USEFUL_INSN_P (insn))
	switch (get_attr_type (insn))
	  {
	  case TYPE_STORE:
	    if (store_pos == -1)
	      store_pos = i;
	    break;

	  case TYPE_LOAD:
	    if (load_pos == -1)
	      load_pos = i;
	    break;

	  default:
	    break;
	  }
    }

  if (load_pos == -1 || store_pos == -1)
    return;

  switch (mips_last_74k_agen_insn)
    {
    case TYPE_UNKNOWN:
      /* Prefer to schedule loads since they have a higher latency.  */
    case TYPE_LOAD:
      /* Swap loads to the front of the queue.  */
      mips_maybe_swap_ready (ready, load_pos, store_pos, 4);
      break;
    case TYPE_STORE:
      /* Swap stores to the front of the queue.  */
      mips_maybe_swap_ready (ready, store_pos, load_pos, 4);
      break;
    default:
      break;
    }
}

/* Implement TARGET_SCHED_INIT.  */

static void
mips_sched_init (FILE *file ATTRIBUTE_UNUSED, int verbose ATTRIBUTE_UNUSED,
		 int max_ready ATTRIBUTE_UNUSED)
{
  mips_macc_chains_last_hilo = 0;
  vr4130_last_insn = 0;
  mips_74k_agen_init (NULL_RTX);

  /* When scheduling for Loongson2, branch instructions go to ALU1,
     therefore basic block is most likely to start with round-robin counter
     pointed to ALU2.  */
  mips_ls2.alu1_turn_p = false;
  mips_ls2.falu1_turn_p = true;
}

/* Subroutine used by TARGET_SCHED_REORDER and TARGET_SCHED_REORDER2.  */

static void
mips_sched_reorder_1 (FILE *file ATTRIBUTE_UNUSED, int verbose ATTRIBUTE_UNUSED,
		      rtx *ready, int *nreadyp, int cycle ATTRIBUTE_UNUSED)
{
  if (!reload_completed
      && TUNE_MACC_CHAINS
      && *nreadyp > 0)
    mips_macc_chains_reorder (ready, *nreadyp);

  if (reload_completed
      && TUNE_MIPS4130
      && !TARGET_VR4130_ALIGN
      && *nreadyp > 1)
    vr4130_reorder (ready, *nreadyp);

  if (TUNE_74K)
    mips_74k_agen_reorder (ready, *nreadyp);
}

/* Implement TARGET_SCHED_REORDER.  */

static int
mips_sched_reorder (FILE *file ATTRIBUTE_UNUSED, int verbose ATTRIBUTE_UNUSED,
		    rtx *ready, int *nreadyp, int cycle ATTRIBUTE_UNUSED)
{
  mips_sched_reorder_1 (file, verbose, ready, nreadyp, cycle);
  return mips_issue_rate ();
}

/* Implement TARGET_SCHED_REORDER2.  */

static int
mips_sched_reorder2 (FILE *file ATTRIBUTE_UNUSED, int verbose ATTRIBUTE_UNUSED,
		     rtx *ready, int *nreadyp, int cycle ATTRIBUTE_UNUSED)
{
  mips_sched_reorder_1 (file, verbose, ready, nreadyp, cycle);
  return cached_can_issue_more;
}

/* Update round-robin counters for ALU1/2 and FALU1/2.  */

static void
mips_ls2_variable_issue (rtx insn)
{
  if (mips_ls2.alu1_turn_p)
    {
      if (cpu_unit_reservation_p (curr_state, mips_ls2.alu1_core_unit_code))
	mips_ls2.alu1_turn_p = false;
    }
  else
    {
      if (cpu_unit_reservation_p (curr_state, mips_ls2.alu2_core_unit_code))
	mips_ls2.alu1_turn_p = true;
    }

  if (mips_ls2.falu1_turn_p)
    {
      if (cpu_unit_reservation_p (curr_state, mips_ls2.falu1_core_unit_code))
	mips_ls2.falu1_turn_p = false;
    }
  else
    {
      if (cpu_unit_reservation_p (curr_state, mips_ls2.falu2_core_unit_code))
	mips_ls2.falu1_turn_p = true;
    }

  if (recog_memoized (insn) >= 0)
    mips_ls2.cycle_has_multi_p |= (get_attr_type (insn) == TYPE_MULTI);
}

/* Implement TARGET_SCHED_VARIABLE_ISSUE.  */

static int
mips_variable_issue (FILE *file ATTRIBUTE_UNUSED, int verbose ATTRIBUTE_UNUSED,
		     rtx insn, int more)
{
  /* Ignore USEs and CLOBBERs; don't count them against the issue rate.  */
  if (USEFUL_INSN_P (insn))
    {
      if (get_attr_type (insn) != TYPE_GHOST)
	more--;
      if (!reload_completed && TUNE_MACC_CHAINS)
	mips_macc_chains_record (insn);
      vr4130_last_insn = insn;
      if (TUNE_74K)
	mips_74k_agen_init (insn);
      else if (TUNE_LOONGSON_2EF)
	mips_ls2_variable_issue (insn);
    }

  /* Instructions of type 'multi' should all be split before
     the second scheduling pass.  */
  gcc_assert (!reload_completed
	      || recog_memoized (insn) < 0
	      || get_attr_type (insn) != TYPE_MULTI);

  cached_can_issue_more = more;
  return more;
}

/* Given that we have an rtx of the form (prefetch ... WRITE LOCALITY),
   return the first operand of the associated PREF or PREFX insn.  */

rtx
mips_prefetch_cookie (rtx write, rtx locality)
{
  /* store_streamed / load_streamed.  */
  if (INTVAL (locality) <= 0)
    return GEN_INT (INTVAL (write) + 4);

  /* store / load.  */
  if (INTVAL (locality) <= 2)
    return write;

  /* store_retained / load_retained.  */
  return GEN_INT (INTVAL (write) + 6);
}

/* Flags that indicate when a built-in function is available.

   BUILTIN_AVAIL_NON_MIPS16
	The function is available on the current target, but only
	in non-MIPS16 mode.  */
#define BUILTIN_AVAIL_NON_MIPS16 1

/* Declare an availability predicate for built-in functions that
   require non-MIPS16 mode and also require COND to be true.
   NAME is the main part of the predicate's name.  */
#define AVAIL_NON_MIPS16(NAME, COND)					\
 static unsigned int							\
 mips_builtin_avail_##NAME (void)					\
 {									\
   return (COND) ? BUILTIN_AVAIL_NON_MIPS16 : 0;			\
 }

/* This structure describes a single built-in function.  */
struct mips_builtin_description {
  /* The code of the main .md file instruction.  See mips_builtin_type
     for more information.  */
  enum insn_code icode;

  /* The floating-point comparison code to use with ICODE, if any.  */
  enum mips_fp_condition cond;

  /* The name of the built-in function.  */
  const char *name;

  /* Specifies how the function should be expanded.  */
  enum mips_builtin_type builtin_type;

  /* The function's prototype.  */
  enum mips_function_type function_type;

  /* Whether the function is available.  */
  unsigned int (*avail) (void);
};

AVAIL_NON_MIPS16 (paired_single, TARGET_PAIRED_SINGLE_FLOAT)
AVAIL_NON_MIPS16 (sb1_paired_single, TARGET_SB1 && TARGET_PAIRED_SINGLE_FLOAT)
AVAIL_NON_MIPS16 (mips3d, TARGET_MIPS3D)
AVAIL_NON_MIPS16 (dsp, TARGET_DSP)
AVAIL_NON_MIPS16 (dspr2, TARGET_DSPR2)
AVAIL_NON_MIPS16 (dsp_32, !TARGET_64BIT && TARGET_DSP)
AVAIL_NON_MIPS16 (dspr2_32, !TARGET_64BIT && TARGET_DSPR2)
AVAIL_NON_MIPS16 (loongson, TARGET_LOONGSON_VECTORS)
AVAIL_NON_MIPS16 (cache, TARGET_CACHE_BUILTIN)

/* Construct a mips_builtin_description from the given arguments.

   INSN is the name of the associated instruction pattern, without the
   leading CODE_FOR_mips_.

   CODE is the floating-point condition code associated with the
   function.  It can be 'f' if the field is not applicable.

   NAME is the name of the function itself, without the leading
   "__builtin_mips_".

   BUILTIN_TYPE and FUNCTION_TYPE are mips_builtin_description fields.

   AVAIL is the name of the availability predicate, without the leading
   mips_builtin_avail_.  */
#define MIPS_BUILTIN(INSN, COND, NAME, BUILTIN_TYPE,			\
		     FUNCTION_TYPE, AVAIL)				\
  { CODE_FOR_mips_ ## INSN, MIPS_FP_COND_ ## COND,			\
    "__builtin_mips_" NAME, BUILTIN_TYPE, FUNCTION_TYPE,		\
    mips_builtin_avail_ ## AVAIL }

/* Define __builtin_mips_<INSN>, which is a MIPS_BUILTIN_DIRECT function
   mapped to instruction CODE_FOR_mips_<INSN>,  FUNCTION_TYPE and AVAIL
   are as for MIPS_BUILTIN.  */
#define DIRECT_BUILTIN(INSN, FUNCTION_TYPE, AVAIL)			\
  MIPS_BUILTIN (INSN, f, #INSN, MIPS_BUILTIN_DIRECT, FUNCTION_TYPE, AVAIL)

/* Define __builtin_mips_<INSN>_<COND>_{s,d} functions, both of which
   are subject to mips_builtin_avail_<AVAIL>.  */
#define CMP_SCALAR_BUILTINS(INSN, COND, AVAIL)				\
  MIPS_BUILTIN (INSN ## _cond_s, COND, #INSN "_" #COND "_s",		\
		MIPS_BUILTIN_CMP_SINGLE, MIPS_INT_FTYPE_SF_SF, AVAIL),	\
  MIPS_BUILTIN (INSN ## _cond_d, COND, #INSN "_" #COND "_d",		\
		MIPS_BUILTIN_CMP_SINGLE, MIPS_INT_FTYPE_DF_DF, AVAIL)

/* Define __builtin_mips_{any,all,upper,lower}_<INSN>_<COND>_ps.
   The lower and upper forms are subject to mips_builtin_avail_<AVAIL>
   while the any and all forms are subject to mips_builtin_avail_mips3d.  */
#define CMP_PS_BUILTINS(INSN, COND, AVAIL)				\
  MIPS_BUILTIN (INSN ## _cond_ps, COND, "any_" #INSN "_" #COND "_ps",	\
		MIPS_BUILTIN_CMP_ANY, MIPS_INT_FTYPE_V2SF_V2SF,		\
		mips3d),						\
  MIPS_BUILTIN (INSN ## _cond_ps, COND, "all_" #INSN "_" #COND "_ps",	\
		MIPS_BUILTIN_CMP_ALL, MIPS_INT_FTYPE_V2SF_V2SF,		\
		mips3d),						\
  MIPS_BUILTIN (INSN ## _cond_ps, COND, "lower_" #INSN "_" #COND "_ps",	\
		MIPS_BUILTIN_CMP_LOWER, MIPS_INT_FTYPE_V2SF_V2SF,	\
		AVAIL),							\
  MIPS_BUILTIN (INSN ## _cond_ps, COND, "upper_" #INSN "_" #COND "_ps",	\
		MIPS_BUILTIN_CMP_UPPER, MIPS_INT_FTYPE_V2SF_V2SF,	\
		AVAIL)

/* Define __builtin_mips_{any,all}_<INSN>_<COND>_4s.  The functions
   are subject to mips_builtin_avail_mips3d.  */
#define CMP_4S_BUILTINS(INSN, COND)					\
  MIPS_BUILTIN (INSN ## _cond_4s, COND, "any_" #INSN "_" #COND "_4s",	\
		MIPS_BUILTIN_CMP_ANY,					\
		MIPS_INT_FTYPE_V2SF_V2SF_V2SF_V2SF, mips3d),		\
  MIPS_BUILTIN (INSN ## _cond_4s, COND, "all_" #INSN "_" #COND "_4s",	\
		MIPS_BUILTIN_CMP_ALL,					\
		MIPS_INT_FTYPE_V2SF_V2SF_V2SF_V2SF, mips3d)

/* Define __builtin_mips_mov{t,f}_<INSN>_<COND>_ps.  The comparison
   instruction requires mips_builtin_avail_<AVAIL>.  */
#define MOVTF_BUILTINS(INSN, COND, AVAIL)				\
  MIPS_BUILTIN (INSN ## _cond_ps, COND, "movt_" #INSN "_" #COND "_ps",	\
		MIPS_BUILTIN_MOVT, MIPS_V2SF_FTYPE_V2SF_V2SF_V2SF_V2SF,	\
		AVAIL),							\
  MIPS_BUILTIN (INSN ## _cond_ps, COND, "movf_" #INSN "_" #COND "_ps",	\
		MIPS_BUILTIN_MOVF, MIPS_V2SF_FTYPE_V2SF_V2SF_V2SF_V2SF,	\
		AVAIL)

/* Define all the built-in functions related to C.cond.fmt condition COND.  */
#define CMP_BUILTINS(COND)						\
  MOVTF_BUILTINS (c, COND, paired_single),				\
  MOVTF_BUILTINS (cabs, COND, mips3d),					\
  CMP_SCALAR_BUILTINS (cabs, COND, mips3d),				\
  CMP_PS_BUILTINS (c, COND, paired_single),				\
  CMP_PS_BUILTINS (cabs, COND, mips3d),					\
  CMP_4S_BUILTINS (c, COND),						\
  CMP_4S_BUILTINS (cabs, COND)

/* Define __builtin_mips_<INSN>, which is a MIPS_BUILTIN_DIRECT_NO_TARGET
   function mapped to instruction CODE_FOR_mips_<INSN>,  FUNCTION_TYPE
   and AVAIL are as for MIPS_BUILTIN.  */
#define DIRECT_NO_TARGET_BUILTIN(INSN, FUNCTION_TYPE, AVAIL)		\
  MIPS_BUILTIN (INSN, f, #INSN,	MIPS_BUILTIN_DIRECT_NO_TARGET,		\
		FUNCTION_TYPE, AVAIL)

/* Define __builtin_mips_bposge<VALUE>.  <VALUE> is 32 for the MIPS32 DSP
   branch instruction.  AVAIL is as for MIPS_BUILTIN.  */
#define BPOSGE_BUILTIN(VALUE, AVAIL)					\
  MIPS_BUILTIN (bposge, f, "bposge" #VALUE,				\
		MIPS_BUILTIN_BPOSGE ## VALUE, MIPS_SI_FTYPE_VOID, AVAIL)

/* Define a Loongson MIPS_BUILTIN_DIRECT function __builtin_loongson_<FN_NAME>
   for instruction CODE_FOR_loongson_<INSN>.  FUNCTION_TYPE is a
   builtin_description field.  */
#define LOONGSON_BUILTIN_ALIAS(INSN, FN_NAME, FUNCTION_TYPE)		\
  { CODE_FOR_loongson_ ## INSN, MIPS_FP_COND_f,				\
    "__builtin_loongson_" #FN_NAME, MIPS_BUILTIN_DIRECT,		\
    FUNCTION_TYPE, mips_builtin_avail_loongson }

/* Define a Loongson MIPS_BUILTIN_DIRECT function __builtin_loongson_<INSN>
   for instruction CODE_FOR_loongson_<INSN>.  FUNCTION_TYPE is a
   builtin_description field.  */
#define LOONGSON_BUILTIN(INSN, FUNCTION_TYPE)				\
  LOONGSON_BUILTIN_ALIAS (INSN, INSN, FUNCTION_TYPE)

/* Like LOONGSON_BUILTIN, but add _<SUFFIX> to the end of the function name.
   We use functions of this form when the same insn can be usefully applied
   to more than one datatype.  */
#define LOONGSON_BUILTIN_SUFFIX(INSN, SUFFIX, FUNCTION_TYPE)		\
  LOONGSON_BUILTIN_ALIAS (INSN, INSN ## _ ## SUFFIX, FUNCTION_TYPE)

#define CODE_FOR_mips_sqrt_ps CODE_FOR_sqrtv2sf2
#define CODE_FOR_mips_addq_ph CODE_FOR_addv2hi3
#define CODE_FOR_mips_addu_qb CODE_FOR_addv4qi3
#define CODE_FOR_mips_subq_ph CODE_FOR_subv2hi3
#define CODE_FOR_mips_subu_qb CODE_FOR_subv4qi3
#define CODE_FOR_mips_mul_ph CODE_FOR_mulv2hi3
#define CODE_FOR_mips_mult CODE_FOR_mulsidi3_32bit
#define CODE_FOR_mips_multu CODE_FOR_umulsidi3_32bit

#define CODE_FOR_loongson_packsswh CODE_FOR_vec_pack_ssat_v2si
#define CODE_FOR_loongson_packsshb CODE_FOR_vec_pack_ssat_v4hi
#define CODE_FOR_loongson_packushb CODE_FOR_vec_pack_usat_v4hi
#define CODE_FOR_loongson_paddw CODE_FOR_addv2si3
#define CODE_FOR_loongson_paddh CODE_FOR_addv4hi3
#define CODE_FOR_loongson_paddb CODE_FOR_addv8qi3
#define CODE_FOR_loongson_paddsh CODE_FOR_ssaddv4hi3
#define CODE_FOR_loongson_paddsb CODE_FOR_ssaddv8qi3
#define CODE_FOR_loongson_paddush CODE_FOR_usaddv4hi3
#define CODE_FOR_loongson_paddusb CODE_FOR_usaddv8qi3
#define CODE_FOR_loongson_pmaxsh CODE_FOR_smaxv4hi3
#define CODE_FOR_loongson_pmaxub CODE_FOR_umaxv8qi3
#define CODE_FOR_loongson_pminsh CODE_FOR_sminv4hi3
#define CODE_FOR_loongson_pminub CODE_FOR_uminv8qi3
#define CODE_FOR_loongson_pmulhuh CODE_FOR_umulv4hi3_highpart
#define CODE_FOR_loongson_pmulhh CODE_FOR_smulv4hi3_highpart
#define CODE_FOR_loongson_pmullh CODE_FOR_mulv4hi3
#define CODE_FOR_loongson_psllh CODE_FOR_ashlv4hi3
#define CODE_FOR_loongson_psllw CODE_FOR_ashlv2si3
#define CODE_FOR_loongson_psrlh CODE_FOR_lshrv4hi3
#define CODE_FOR_loongson_psrlw CODE_FOR_lshrv2si3
#define CODE_FOR_loongson_psrah CODE_FOR_ashrv4hi3
#define CODE_FOR_loongson_psraw CODE_FOR_ashrv2si3
#define CODE_FOR_loongson_psubw CODE_FOR_subv2si3
#define CODE_FOR_loongson_psubh CODE_FOR_subv4hi3
#define CODE_FOR_loongson_psubb CODE_FOR_subv8qi3
#define CODE_FOR_loongson_psubsh CODE_FOR_sssubv4hi3
#define CODE_FOR_loongson_psubsb CODE_FOR_sssubv8qi3
#define CODE_FOR_loongson_psubush CODE_FOR_ussubv4hi3
#define CODE_FOR_loongson_psubusb CODE_FOR_ussubv8qi3
#define CODE_FOR_loongson_punpckhbh CODE_FOR_vec_interleave_highv8qi
#define CODE_FOR_loongson_punpckhhw CODE_FOR_vec_interleave_highv4hi
#define CODE_FOR_loongson_punpckhwd CODE_FOR_vec_interleave_highv2si
#define CODE_FOR_loongson_punpcklbh CODE_FOR_vec_interleave_lowv8qi
#define CODE_FOR_loongson_punpcklhw CODE_FOR_vec_interleave_lowv4hi
#define CODE_FOR_loongson_punpcklwd CODE_FOR_vec_interleave_lowv2si

static const struct mips_builtin_description mips_builtins[] = {
  DIRECT_BUILTIN (pll_ps, MIPS_V2SF_FTYPE_V2SF_V2SF, paired_single),
  DIRECT_BUILTIN (pul_ps, MIPS_V2SF_FTYPE_V2SF_V2SF, paired_single),
  DIRECT_BUILTIN (plu_ps, MIPS_V2SF_FTYPE_V2SF_V2SF, paired_single),
  DIRECT_BUILTIN (puu_ps, MIPS_V2SF_FTYPE_V2SF_V2SF, paired_single),
  DIRECT_BUILTIN (cvt_ps_s, MIPS_V2SF_FTYPE_SF_SF, paired_single),
  DIRECT_BUILTIN (cvt_s_pl, MIPS_SF_FTYPE_V2SF, paired_single),
  DIRECT_BUILTIN (cvt_s_pu, MIPS_SF_FTYPE_V2SF, paired_single),
  DIRECT_BUILTIN (abs_ps, MIPS_V2SF_FTYPE_V2SF, paired_single),

  DIRECT_BUILTIN (alnv_ps, MIPS_V2SF_FTYPE_V2SF_V2SF_INT, paired_single),
  DIRECT_BUILTIN (addr_ps, MIPS_V2SF_FTYPE_V2SF_V2SF, mips3d),
  DIRECT_BUILTIN (mulr_ps, MIPS_V2SF_FTYPE_V2SF_V2SF, mips3d),
  DIRECT_BUILTIN (cvt_pw_ps, MIPS_V2SF_FTYPE_V2SF, mips3d),
  DIRECT_BUILTIN (cvt_ps_pw, MIPS_V2SF_FTYPE_V2SF, mips3d),

  DIRECT_BUILTIN (recip1_s, MIPS_SF_FTYPE_SF, mips3d),
  DIRECT_BUILTIN (recip1_d, MIPS_DF_FTYPE_DF, mips3d),
  DIRECT_BUILTIN (recip1_ps, MIPS_V2SF_FTYPE_V2SF, mips3d),
  DIRECT_BUILTIN (recip2_s, MIPS_SF_FTYPE_SF_SF, mips3d),
  DIRECT_BUILTIN (recip2_d, MIPS_DF_FTYPE_DF_DF, mips3d),
  DIRECT_BUILTIN (recip2_ps, MIPS_V2SF_FTYPE_V2SF_V2SF, mips3d),

  DIRECT_BUILTIN (rsqrt1_s, MIPS_SF_FTYPE_SF, mips3d),
  DIRECT_BUILTIN (rsqrt1_d, MIPS_DF_FTYPE_DF, mips3d),
  DIRECT_BUILTIN (rsqrt1_ps, MIPS_V2SF_FTYPE_V2SF, mips3d),
  DIRECT_BUILTIN (rsqrt2_s, MIPS_SF_FTYPE_SF_SF, mips3d),
  DIRECT_BUILTIN (rsqrt2_d, MIPS_DF_FTYPE_DF_DF, mips3d),
  DIRECT_BUILTIN (rsqrt2_ps, MIPS_V2SF_FTYPE_V2SF_V2SF, mips3d),

  MIPS_FP_CONDITIONS (CMP_BUILTINS),

  /* Built-in functions for the SB-1 processor.  */
  DIRECT_BUILTIN (sqrt_ps, MIPS_V2SF_FTYPE_V2SF, sb1_paired_single),

  /* Built-in functions for the DSP ASE (32-bit and 64-bit).  */
  DIRECT_BUILTIN (addq_ph, MIPS_V2HI_FTYPE_V2HI_V2HI, dsp),
  DIRECT_BUILTIN (addq_s_ph, MIPS_V2HI_FTYPE_V2HI_V2HI, dsp),
  DIRECT_BUILTIN (addq_s_w, MIPS_SI_FTYPE_SI_SI, dsp),
  DIRECT_BUILTIN (addu_qb, MIPS_V4QI_FTYPE_V4QI_V4QI, dsp),
  DIRECT_BUILTIN (addu_s_qb, MIPS_V4QI_FTYPE_V4QI_V4QI, dsp),
  DIRECT_BUILTIN (subq_ph, MIPS_V2HI_FTYPE_V2HI_V2HI, dsp),
  DIRECT_BUILTIN (subq_s_ph, MIPS_V2HI_FTYPE_V2HI_V2HI, dsp),
  DIRECT_BUILTIN (subq_s_w, MIPS_SI_FTYPE_SI_SI, dsp),
  DIRECT_BUILTIN (subu_qb, MIPS_V4QI_FTYPE_V4QI_V4QI, dsp),
  DIRECT_BUILTIN (subu_s_qb, MIPS_V4QI_FTYPE_V4QI_V4QI, dsp),
  DIRECT_BUILTIN (addsc, MIPS_SI_FTYPE_SI_SI, dsp),
  DIRECT_BUILTIN (addwc, MIPS_SI_FTYPE_SI_SI, dsp),
  DIRECT_BUILTIN (modsub, MIPS_SI_FTYPE_SI_SI, dsp),
  DIRECT_BUILTIN (raddu_w_qb, MIPS_SI_FTYPE_V4QI, dsp),
  DIRECT_BUILTIN (absq_s_ph, MIPS_V2HI_FTYPE_V2HI, dsp),
  DIRECT_BUILTIN (absq_s_w, MIPS_SI_FTYPE_SI, dsp),
  DIRECT_BUILTIN (precrq_qb_ph, MIPS_V4QI_FTYPE_V2HI_V2HI, dsp),
  DIRECT_BUILTIN (precrq_ph_w, MIPS_V2HI_FTYPE_SI_SI, dsp),
  DIRECT_BUILTIN (precrq_rs_ph_w, MIPS_V2HI_FTYPE_SI_SI, dsp),
  DIRECT_BUILTIN (precrqu_s_qb_ph, MIPS_V4QI_FTYPE_V2HI_V2HI, dsp),
  DIRECT_BUILTIN (preceq_w_phl, MIPS_SI_FTYPE_V2HI, dsp),
  DIRECT_BUILTIN (preceq_w_phr, MIPS_SI_FTYPE_V2HI, dsp),
  DIRECT_BUILTIN (precequ_ph_qbl, MIPS_V2HI_FTYPE_V4QI, dsp),
  DIRECT_BUILTIN (precequ_ph_qbr, MIPS_V2HI_FTYPE_V4QI, dsp),
  DIRECT_BUILTIN (precequ_ph_qbla, MIPS_V2HI_FTYPE_V4QI, dsp),
  DIRECT_BUILTIN (precequ_ph_qbra, MIPS_V2HI_FTYPE_V4QI, dsp),
  DIRECT_BUILTIN (preceu_ph_qbl, MIPS_V2HI_FTYPE_V4QI, dsp),
  DIRECT_BUILTIN (preceu_ph_qbr, MIPS_V2HI_FTYPE_V4QI, dsp),
  DIRECT_BUILTIN (preceu_ph_qbla, MIPS_V2HI_FTYPE_V4QI, dsp),
  DIRECT_BUILTIN (preceu_ph_qbra, MIPS_V2HI_FTYPE_V4QI, dsp),
  DIRECT_BUILTIN (shll_qb, MIPS_V4QI_FTYPE_V4QI_SI, dsp),
  DIRECT_BUILTIN (shll_ph, MIPS_V2HI_FTYPE_V2HI_SI, dsp),
  DIRECT_BUILTIN (shll_s_ph, MIPS_V2HI_FTYPE_V2HI_SI, dsp),
  DIRECT_BUILTIN (shll_s_w, MIPS_SI_FTYPE_SI_SI, dsp),
  DIRECT_BUILTIN (shrl_qb, MIPS_V4QI_FTYPE_V4QI_SI, dsp),
  DIRECT_BUILTIN (shra_ph, MIPS_V2HI_FTYPE_V2HI_SI, dsp),
  DIRECT_BUILTIN (shra_r_ph, MIPS_V2HI_FTYPE_V2HI_SI, dsp),
  DIRECT_BUILTIN (shra_r_w, MIPS_SI_FTYPE_SI_SI, dsp),
  DIRECT_BUILTIN (muleu_s_ph_qbl, MIPS_V2HI_FTYPE_V4QI_V2HI, dsp),
  DIRECT_BUILTIN (muleu_s_ph_qbr, MIPS_V2HI_FTYPE_V4QI_V2HI, dsp),
  DIRECT_BUILTIN (mulq_rs_ph, MIPS_V2HI_FTYPE_V2HI_V2HI, dsp),
  DIRECT_BUILTIN (muleq_s_w_phl, MIPS_SI_FTYPE_V2HI_V2HI, dsp),
  DIRECT_BUILTIN (muleq_s_w_phr, MIPS_SI_FTYPE_V2HI_V2HI, dsp),
  DIRECT_BUILTIN (bitrev, MIPS_SI_FTYPE_SI, dsp),
  DIRECT_BUILTIN (insv, MIPS_SI_FTYPE_SI_SI, dsp),
  DIRECT_BUILTIN (repl_qb, MIPS_V4QI_FTYPE_SI, dsp),
  DIRECT_BUILTIN (repl_ph, MIPS_V2HI_FTYPE_SI, dsp),
  DIRECT_NO_TARGET_BUILTIN (cmpu_eq_qb, MIPS_VOID_FTYPE_V4QI_V4QI, dsp),
  DIRECT_NO_TARGET_BUILTIN (cmpu_lt_qb, MIPS_VOID_FTYPE_V4QI_V4QI, dsp),
  DIRECT_NO_TARGET_BUILTIN (cmpu_le_qb, MIPS_VOID_FTYPE_V4QI_V4QI, dsp),
  DIRECT_BUILTIN (cmpgu_eq_qb, MIPS_SI_FTYPE_V4QI_V4QI, dsp),
  DIRECT_BUILTIN (cmpgu_lt_qb, MIPS_SI_FTYPE_V4QI_V4QI, dsp),
  DIRECT_BUILTIN (cmpgu_le_qb, MIPS_SI_FTYPE_V4QI_V4QI, dsp),
  DIRECT_NO_TARGET_BUILTIN (cmp_eq_ph, MIPS_VOID_FTYPE_V2HI_V2HI, dsp),
  DIRECT_NO_TARGET_BUILTIN (cmp_lt_ph, MIPS_VOID_FTYPE_V2HI_V2HI, dsp),
  DIRECT_NO_TARGET_BUILTIN (cmp_le_ph, MIPS_VOID_FTYPE_V2HI_V2HI, dsp),
  DIRECT_BUILTIN (pick_qb, MIPS_V4QI_FTYPE_V4QI_V4QI, dsp),
  DIRECT_BUILTIN (pick_ph, MIPS_V2HI_FTYPE_V2HI_V2HI, dsp),
  DIRECT_BUILTIN (packrl_ph, MIPS_V2HI_FTYPE_V2HI_V2HI, dsp),
  DIRECT_NO_TARGET_BUILTIN (wrdsp, MIPS_VOID_FTYPE_SI_SI, dsp),
  DIRECT_BUILTIN (rddsp, MIPS_SI_FTYPE_SI, dsp),
  DIRECT_BUILTIN (lbux, MIPS_SI_FTYPE_POINTER_SI, dsp),
  DIRECT_BUILTIN (lhx, MIPS_SI_FTYPE_POINTER_SI, dsp),
  DIRECT_BUILTIN (lwx, MIPS_SI_FTYPE_POINTER_SI, dsp),
  BPOSGE_BUILTIN (32, dsp),

  /* The following are for the MIPS DSP ASE REV 2 (32-bit and 64-bit).  */
  DIRECT_BUILTIN (absq_s_qb, MIPS_V4QI_FTYPE_V4QI, dspr2),
  DIRECT_BUILTIN (addu_ph, MIPS_V2HI_FTYPE_V2HI_V2HI, dspr2),
  DIRECT_BUILTIN (addu_s_ph, MIPS_V2HI_FTYPE_V2HI_V2HI, dspr2),
  DIRECT_BUILTIN (adduh_qb, MIPS_V4QI_FTYPE_V4QI_V4QI, dspr2),
  DIRECT_BUILTIN (adduh_r_qb, MIPS_V4QI_FTYPE_V4QI_V4QI, dspr2),
  DIRECT_BUILTIN (append, MIPS_SI_FTYPE_SI_SI_SI, dspr2),
  DIRECT_BUILTIN (balign, MIPS_SI_FTYPE_SI_SI_SI, dspr2),
  DIRECT_BUILTIN (cmpgdu_eq_qb, MIPS_SI_FTYPE_V4QI_V4QI, dspr2),
  DIRECT_BUILTIN (cmpgdu_lt_qb, MIPS_SI_FTYPE_V4QI_V4QI, dspr2),
  DIRECT_BUILTIN (cmpgdu_le_qb, MIPS_SI_FTYPE_V4QI_V4QI, dspr2),
  DIRECT_BUILTIN (mul_ph, MIPS_V2HI_FTYPE_V2HI_V2HI, dspr2),
  DIRECT_BUILTIN (mul_s_ph, MIPS_V2HI_FTYPE_V2HI_V2HI, dspr2),
  DIRECT_BUILTIN (mulq_rs_w, MIPS_SI_FTYPE_SI_SI, dspr2),
  DIRECT_BUILTIN (mulq_s_ph, MIPS_V2HI_FTYPE_V2HI_V2HI, dspr2),
  DIRECT_BUILTIN (mulq_s_w, MIPS_SI_FTYPE_SI_SI, dspr2),
  DIRECT_BUILTIN (precr_qb_ph, MIPS_V4QI_FTYPE_V2HI_V2HI, dspr2),
  DIRECT_BUILTIN (precr_sra_ph_w, MIPS_V2HI_FTYPE_SI_SI_SI, dspr2),
  DIRECT_BUILTIN (precr_sra_r_ph_w, MIPS_V2HI_FTYPE_SI_SI_SI, dspr2),
  DIRECT_BUILTIN (prepend, MIPS_SI_FTYPE_SI_SI_SI, dspr2),
  DIRECT_BUILTIN (shra_qb, MIPS_V4QI_FTYPE_V4QI_SI, dspr2),
  DIRECT_BUILTIN (shra_r_qb, MIPS_V4QI_FTYPE_V4QI_SI, dspr2),
  DIRECT_BUILTIN (shrl_ph, MIPS_V2HI_FTYPE_V2HI_SI, dspr2),
  DIRECT_BUILTIN (subu_ph, MIPS_V2HI_FTYPE_V2HI_V2HI, dspr2),
  DIRECT_BUILTIN (subu_s_ph, MIPS_V2HI_FTYPE_V2HI_V2HI, dspr2),
  DIRECT_BUILTIN (subuh_qb, MIPS_V4QI_FTYPE_V4QI_V4QI, dspr2),
  DIRECT_BUILTIN (subuh_r_qb, MIPS_V4QI_FTYPE_V4QI_V4QI, dspr2),
  DIRECT_BUILTIN (addqh_ph, MIPS_V2HI_FTYPE_V2HI_V2HI, dspr2),
  DIRECT_BUILTIN (addqh_r_ph, MIPS_V2HI_FTYPE_V2HI_V2HI, dspr2),
  DIRECT_BUILTIN (addqh_w, MIPS_SI_FTYPE_SI_SI, dspr2),
  DIRECT_BUILTIN (addqh_r_w, MIPS_SI_FTYPE_SI_SI, dspr2),
  DIRECT_BUILTIN (subqh_ph, MIPS_V2HI_FTYPE_V2HI_V2HI, dspr2),
  DIRECT_BUILTIN (subqh_r_ph, MIPS_V2HI_FTYPE_V2HI_V2HI, dspr2),
  DIRECT_BUILTIN (subqh_w, MIPS_SI_FTYPE_SI_SI, dspr2),
  DIRECT_BUILTIN (subqh_r_w, MIPS_SI_FTYPE_SI_SI, dspr2),

  /* Built-in functions for the DSP ASE (32-bit only).  */
  DIRECT_BUILTIN (dpau_h_qbl, MIPS_DI_FTYPE_DI_V4QI_V4QI, dsp_32),
  DIRECT_BUILTIN (dpau_h_qbr, MIPS_DI_FTYPE_DI_V4QI_V4QI, dsp_32),
  DIRECT_BUILTIN (dpsu_h_qbl, MIPS_DI_FTYPE_DI_V4QI_V4QI, dsp_32),
  DIRECT_BUILTIN (dpsu_h_qbr, MIPS_DI_FTYPE_DI_V4QI_V4QI, dsp_32),
  DIRECT_BUILTIN (dpaq_s_w_ph, MIPS_DI_FTYPE_DI_V2HI_V2HI, dsp_32),
  DIRECT_BUILTIN (dpsq_s_w_ph, MIPS_DI_FTYPE_DI_V2HI_V2HI, dsp_32),
  DIRECT_BUILTIN (mulsaq_s_w_ph, MIPS_DI_FTYPE_DI_V2HI_V2HI, dsp_32),
  DIRECT_BUILTIN (dpaq_sa_l_w, MIPS_DI_FTYPE_DI_SI_SI, dsp_32),
  DIRECT_BUILTIN (dpsq_sa_l_w, MIPS_DI_FTYPE_DI_SI_SI, dsp_32),
  DIRECT_BUILTIN (maq_s_w_phl, MIPS_DI_FTYPE_DI_V2HI_V2HI, dsp_32),
  DIRECT_BUILTIN (maq_s_w_phr, MIPS_DI_FTYPE_DI_V2HI_V2HI, dsp_32),
  DIRECT_BUILTIN (maq_sa_w_phl, MIPS_DI_FTYPE_DI_V2HI_V2HI, dsp_32),
  DIRECT_BUILTIN (maq_sa_w_phr, MIPS_DI_FTYPE_DI_V2HI_V2HI, dsp_32),
  DIRECT_BUILTIN (extr_w, MIPS_SI_FTYPE_DI_SI, dsp_32),
  DIRECT_BUILTIN (extr_r_w, MIPS_SI_FTYPE_DI_SI, dsp_32),
  DIRECT_BUILTIN (extr_rs_w, MIPS_SI_FTYPE_DI_SI, dsp_32),
  DIRECT_BUILTIN (extr_s_h, MIPS_SI_FTYPE_DI_SI, dsp_32),
  DIRECT_BUILTIN (extp, MIPS_SI_FTYPE_DI_SI, dsp_32),
  DIRECT_BUILTIN (extpdp, MIPS_SI_FTYPE_DI_SI, dsp_32),
  DIRECT_BUILTIN (shilo, MIPS_DI_FTYPE_DI_SI, dsp_32),
  DIRECT_BUILTIN (mthlip, MIPS_DI_FTYPE_DI_SI, dsp_32),
  DIRECT_BUILTIN (madd, MIPS_DI_FTYPE_DI_SI_SI, dsp_32),
  DIRECT_BUILTIN (maddu, MIPS_DI_FTYPE_DI_USI_USI, dsp_32),
  DIRECT_BUILTIN (msub, MIPS_DI_FTYPE_DI_SI_SI, dsp_32),
  DIRECT_BUILTIN (msubu, MIPS_DI_FTYPE_DI_USI_USI, dsp_32),
  DIRECT_BUILTIN (mult, MIPS_DI_FTYPE_SI_SI, dsp_32),
  DIRECT_BUILTIN (multu, MIPS_DI_FTYPE_USI_USI, dsp_32),

  /* The following are for the MIPS DSP ASE REV 2 (32-bit only).  */
  DIRECT_BUILTIN (dpa_w_ph, MIPS_DI_FTYPE_DI_V2HI_V2HI, dspr2_32),
  DIRECT_BUILTIN (dps_w_ph, MIPS_DI_FTYPE_DI_V2HI_V2HI, dspr2_32),
  DIRECT_BUILTIN (mulsa_w_ph, MIPS_DI_FTYPE_DI_V2HI_V2HI, dspr2_32),
  DIRECT_BUILTIN (dpax_w_ph, MIPS_DI_FTYPE_DI_V2HI_V2HI, dspr2_32),
  DIRECT_BUILTIN (dpsx_w_ph, MIPS_DI_FTYPE_DI_V2HI_V2HI, dspr2_32),
  DIRECT_BUILTIN (dpaqx_s_w_ph, MIPS_DI_FTYPE_DI_V2HI_V2HI, dspr2_32),
  DIRECT_BUILTIN (dpaqx_sa_w_ph, MIPS_DI_FTYPE_DI_V2HI_V2HI, dspr2_32),
  DIRECT_BUILTIN (dpsqx_s_w_ph, MIPS_DI_FTYPE_DI_V2HI_V2HI, dspr2_32),
  DIRECT_BUILTIN (dpsqx_sa_w_ph, MIPS_DI_FTYPE_DI_V2HI_V2HI, dspr2_32),

  /* Builtin functions for ST Microelectronics Loongson-2E/2F cores.  */
  LOONGSON_BUILTIN (packsswh, MIPS_V4HI_FTYPE_V2SI_V2SI),
  LOONGSON_BUILTIN (packsshb, MIPS_V8QI_FTYPE_V4HI_V4HI),
  LOONGSON_BUILTIN (packushb, MIPS_UV8QI_FTYPE_UV4HI_UV4HI),
  LOONGSON_BUILTIN_SUFFIX (paddw, u, MIPS_UV2SI_FTYPE_UV2SI_UV2SI),
  LOONGSON_BUILTIN_SUFFIX (paddh, u, MIPS_UV4HI_FTYPE_UV4HI_UV4HI),
  LOONGSON_BUILTIN_SUFFIX (paddb, u, MIPS_UV8QI_FTYPE_UV8QI_UV8QI),
  LOONGSON_BUILTIN_SUFFIX (paddw, s, MIPS_V2SI_FTYPE_V2SI_V2SI),
  LOONGSON_BUILTIN_SUFFIX (paddh, s, MIPS_V4HI_FTYPE_V4HI_V4HI),
  LOONGSON_BUILTIN_SUFFIX (paddb, s, MIPS_V8QI_FTYPE_V8QI_V8QI),
  LOONGSON_BUILTIN_SUFFIX (paddd, u, MIPS_UDI_FTYPE_UDI_UDI),
  LOONGSON_BUILTIN_SUFFIX (paddd, s, MIPS_DI_FTYPE_DI_DI),
  LOONGSON_BUILTIN (paddsh, MIPS_V4HI_FTYPE_V4HI_V4HI),
  LOONGSON_BUILTIN (paddsb, MIPS_V8QI_FTYPE_V8QI_V8QI),
  LOONGSON_BUILTIN (paddush, MIPS_UV4HI_FTYPE_UV4HI_UV4HI),
  LOONGSON_BUILTIN (paddusb, MIPS_UV8QI_FTYPE_UV8QI_UV8QI),
  LOONGSON_BUILTIN_ALIAS (pandn_d, pandn_ud, MIPS_UDI_FTYPE_UDI_UDI),
  LOONGSON_BUILTIN_ALIAS (pandn_w, pandn_uw, MIPS_UV2SI_FTYPE_UV2SI_UV2SI),
  LOONGSON_BUILTIN_ALIAS (pandn_h, pandn_uh, MIPS_UV4HI_FTYPE_UV4HI_UV4HI),
  LOONGSON_BUILTIN_ALIAS (pandn_b, pandn_ub, MIPS_UV8QI_FTYPE_UV8QI_UV8QI),
  LOONGSON_BUILTIN_ALIAS (pandn_d, pandn_sd, MIPS_DI_FTYPE_DI_DI),
  LOONGSON_BUILTIN_ALIAS (pandn_w, pandn_sw, MIPS_V2SI_FTYPE_V2SI_V2SI),
  LOONGSON_BUILTIN_ALIAS (pandn_h, pandn_sh, MIPS_V4HI_FTYPE_V4HI_V4HI),
  LOONGSON_BUILTIN_ALIAS (pandn_b, pandn_sb, MIPS_V8QI_FTYPE_V8QI_V8QI),
  LOONGSON_BUILTIN (pavgh, MIPS_UV4HI_FTYPE_UV4HI_UV4HI),
  LOONGSON_BUILTIN (pavgb, MIPS_UV8QI_FTYPE_UV8QI_UV8QI),
  LOONGSON_BUILTIN_SUFFIX (pcmpeqw, u, MIPS_UV2SI_FTYPE_UV2SI_UV2SI),
  LOONGSON_BUILTIN_SUFFIX (pcmpeqh, u, MIPS_UV4HI_FTYPE_UV4HI_UV4HI),
  LOONGSON_BUILTIN_SUFFIX (pcmpeqb, u, MIPS_UV8QI_FTYPE_UV8QI_UV8QI),
  LOONGSON_BUILTIN_SUFFIX (pcmpeqw, s, MIPS_V2SI_FTYPE_V2SI_V2SI),
  LOONGSON_BUILTIN_SUFFIX (pcmpeqh, s, MIPS_V4HI_FTYPE_V4HI_V4HI),
  LOONGSON_BUILTIN_SUFFIX (pcmpeqb, s, MIPS_V8QI_FTYPE_V8QI_V8QI),
  LOONGSON_BUILTIN_SUFFIX (pcmpgtw, u, MIPS_UV2SI_FTYPE_UV2SI_UV2SI),
  LOONGSON_BUILTIN_SUFFIX (pcmpgth, u, MIPS_UV4HI_FTYPE_UV4HI_UV4HI),
  LOONGSON_BUILTIN_SUFFIX (pcmpgtb, u, MIPS_UV8QI_FTYPE_UV8QI_UV8QI),
  LOONGSON_BUILTIN_SUFFIX (pcmpgtw, s, MIPS_V2SI_FTYPE_V2SI_V2SI),
  LOONGSON_BUILTIN_SUFFIX (pcmpgth, s, MIPS_V4HI_FTYPE_V4HI_V4HI),
  LOONGSON_BUILTIN_SUFFIX (pcmpgtb, s, MIPS_V8QI_FTYPE_V8QI_V8QI),
  LOONGSON_BUILTIN_SUFFIX (pextrh, u, MIPS_UV4HI_FTYPE_UV4HI_USI),
  LOONGSON_BUILTIN_SUFFIX (pextrh, s, MIPS_V4HI_FTYPE_V4HI_USI),
  LOONGSON_BUILTIN_SUFFIX (pinsrh_0, u, MIPS_UV4HI_FTYPE_UV4HI_UV4HI),
  LOONGSON_BUILTIN_SUFFIX (pinsrh_1, u, MIPS_UV4HI_FTYPE_UV4HI_UV4HI),
  LOONGSON_BUILTIN_SUFFIX (pinsrh_2, u, MIPS_UV4HI_FTYPE_UV4HI_UV4HI),
  LOONGSON_BUILTIN_SUFFIX (pinsrh_3, u, MIPS_UV4HI_FTYPE_UV4HI_UV4HI),
  LOONGSON_BUILTIN_SUFFIX (pinsrh_0, s, MIPS_V4HI_FTYPE_V4HI_V4HI),
  LOONGSON_BUILTIN_SUFFIX (pinsrh_1, s, MIPS_V4HI_FTYPE_V4HI_V4HI),
  LOONGSON_BUILTIN_SUFFIX (pinsrh_2, s, MIPS_V4HI_FTYPE_V4HI_V4HI),
  LOONGSON_BUILTIN_SUFFIX (pinsrh_3, s, MIPS_V4HI_FTYPE_V4HI_V4HI),
  LOONGSON_BUILTIN (pmaddhw, MIPS_V2SI_FTYPE_V4HI_V4HI),
  LOONGSON_BUILTIN (pmaxsh, MIPS_V4HI_FTYPE_V4HI_V4HI),
  LOONGSON_BUILTIN (pmaxub, MIPS_UV8QI_FTYPE_UV8QI_UV8QI),
  LOONGSON_BUILTIN (pminsh, MIPS_V4HI_FTYPE_V4HI_V4HI),
  LOONGSON_BUILTIN (pminub, MIPS_UV8QI_FTYPE_UV8QI_UV8QI),
  LOONGSON_BUILTIN_SUFFIX (pmovmskb, u, MIPS_UV8QI_FTYPE_UV8QI),
  LOONGSON_BUILTIN_SUFFIX (pmovmskb, s, MIPS_V8QI_FTYPE_V8QI),
  LOONGSON_BUILTIN (pmulhuh, MIPS_UV4HI_FTYPE_UV4HI_UV4HI),
  LOONGSON_BUILTIN (pmulhh, MIPS_V4HI_FTYPE_V4HI_V4HI),
  LOONGSON_BUILTIN (pmullh, MIPS_V4HI_FTYPE_V4HI_V4HI),
  LOONGSON_BUILTIN (pmuluw, MIPS_UDI_FTYPE_UV2SI_UV2SI),
  LOONGSON_BUILTIN (pasubub, MIPS_UV8QI_FTYPE_UV8QI_UV8QI),
  LOONGSON_BUILTIN (biadd, MIPS_UV4HI_FTYPE_UV8QI),
  LOONGSON_BUILTIN (psadbh, MIPS_UV4HI_FTYPE_UV8QI_UV8QI),
  LOONGSON_BUILTIN_SUFFIX (pshufh, u, MIPS_UV4HI_FTYPE_UV4HI_UV4HI_UQI),
  LOONGSON_BUILTIN_SUFFIX (pshufh, s, MIPS_V4HI_FTYPE_V4HI_V4HI_UQI),
  LOONGSON_BUILTIN_SUFFIX (psllh, u, MIPS_UV4HI_FTYPE_UV4HI_UQI),
  LOONGSON_BUILTIN_SUFFIX (psllh, s, MIPS_V4HI_FTYPE_V4HI_UQI),
  LOONGSON_BUILTIN_SUFFIX (psllw, u, MIPS_UV2SI_FTYPE_UV2SI_UQI),
  LOONGSON_BUILTIN_SUFFIX (psllw, s, MIPS_V2SI_FTYPE_V2SI_UQI),
  LOONGSON_BUILTIN_SUFFIX (psrah, u, MIPS_UV4HI_FTYPE_UV4HI_UQI),
  LOONGSON_BUILTIN_SUFFIX (psrah, s, MIPS_V4HI_FTYPE_V4HI_UQI),
  LOONGSON_BUILTIN_SUFFIX (psraw, u, MIPS_UV2SI_FTYPE_UV2SI_UQI),
  LOONGSON_BUILTIN_SUFFIX (psraw, s, MIPS_V2SI_FTYPE_V2SI_UQI),
  LOONGSON_BUILTIN_SUFFIX (psrlh, u, MIPS_UV4HI_FTYPE_UV4HI_UQI),
  LOONGSON_BUILTIN_SUFFIX (psrlh, s, MIPS_V4HI_FTYPE_V4HI_UQI),
  LOONGSON_BUILTIN_SUFFIX (psrlw, u, MIPS_UV2SI_FTYPE_UV2SI_UQI),
  LOONGSON_BUILTIN_SUFFIX (psrlw, s, MIPS_V2SI_FTYPE_V2SI_UQI),
  LOONGSON_BUILTIN_SUFFIX (psubw, u, MIPS_UV2SI_FTYPE_UV2SI_UV2SI),
  LOONGSON_BUILTIN_SUFFIX (psubh, u, MIPS_UV4HI_FTYPE_UV4HI_UV4HI),
  LOONGSON_BUILTIN_SUFFIX (psubb, u, MIPS_UV8QI_FTYPE_UV8QI_UV8QI),
  LOONGSON_BUILTIN_SUFFIX (psubw, s, MIPS_V2SI_FTYPE_V2SI_V2SI),
  LOONGSON_BUILTIN_SUFFIX (psubh, s, MIPS_V4HI_FTYPE_V4HI_V4HI),
  LOONGSON_BUILTIN_SUFFIX (psubb, s, MIPS_V8QI_FTYPE_V8QI_V8QI),
  LOONGSON_BUILTIN_SUFFIX (psubd, u, MIPS_UDI_FTYPE_UDI_UDI),
  LOONGSON_BUILTIN_SUFFIX (psubd, s, MIPS_DI_FTYPE_DI_DI),
  LOONGSON_BUILTIN (psubsh, MIPS_V4HI_FTYPE_V4HI_V4HI),
  LOONGSON_BUILTIN (psubsb, MIPS_V8QI_FTYPE_V8QI_V8QI),
  LOONGSON_BUILTIN (psubush, MIPS_UV4HI_FTYPE_UV4HI_UV4HI),
  LOONGSON_BUILTIN (psubusb, MIPS_UV8QI_FTYPE_UV8QI_UV8QI),
  LOONGSON_BUILTIN_SUFFIX (punpckhbh, u, MIPS_UV8QI_FTYPE_UV8QI_UV8QI),
  LOONGSON_BUILTIN_SUFFIX (punpckhhw, u, MIPS_UV4HI_FTYPE_UV4HI_UV4HI),
  LOONGSON_BUILTIN_SUFFIX (punpckhwd, u, MIPS_UV2SI_FTYPE_UV2SI_UV2SI),
  LOONGSON_BUILTIN_SUFFIX (punpckhbh, s, MIPS_V8QI_FTYPE_V8QI_V8QI),
  LOONGSON_BUILTIN_SUFFIX (punpckhhw, s, MIPS_V4HI_FTYPE_V4HI_V4HI),
  LOONGSON_BUILTIN_SUFFIX (punpckhwd, s, MIPS_V2SI_FTYPE_V2SI_V2SI),
  LOONGSON_BUILTIN_SUFFIX (punpcklbh, u, MIPS_UV8QI_FTYPE_UV8QI_UV8QI),
  LOONGSON_BUILTIN_SUFFIX (punpcklhw, u, MIPS_UV4HI_FTYPE_UV4HI_UV4HI),
  LOONGSON_BUILTIN_SUFFIX (punpcklwd, u, MIPS_UV2SI_FTYPE_UV2SI_UV2SI),
  LOONGSON_BUILTIN_SUFFIX (punpcklbh, s, MIPS_V8QI_FTYPE_V8QI_V8QI),
  LOONGSON_BUILTIN_SUFFIX (punpcklhw, s, MIPS_V4HI_FTYPE_V4HI_V4HI),
  LOONGSON_BUILTIN_SUFFIX (punpcklwd, s, MIPS_V2SI_FTYPE_V2SI_V2SI),

  /* Sundry other built-in functions.  */
  DIRECT_NO_TARGET_BUILTIN (cache, MIPS_VOID_FTYPE_SI_CVPOINTER, cache)
};

/* Index I is the function declaration for mips_builtins[I], or null if the
   function isn't defined on this target.  */
static GTY(()) tree mips_builtin_decls[ARRAY_SIZE (mips_builtins)];

/* MODE is a vector mode whose elements have type TYPE.  Return the type
   of the vector itself.  */

static tree
mips_builtin_vector_type (tree type, enum machine_mode mode)
{
  static tree types[2 * (int) MAX_MACHINE_MODE];
  int mode_index;

  mode_index = (int) mode;

  if (TREE_CODE (type) == INTEGER_TYPE && TYPE_UNSIGNED (type))
    mode_index += MAX_MACHINE_MODE;

  if (types[mode_index] == NULL_TREE)
    types[mode_index] = build_vector_type_for_mode (type, mode);
  return types[mode_index];
}

/* Return a type for 'const volatile void *'.  */

static tree
mips_build_cvpointer_type (void)
{
  static tree cache;

  if (cache == NULL_TREE)
    cache = build_pointer_type (build_qualified_type
				(void_type_node,
				 TYPE_QUAL_CONST | TYPE_QUAL_VOLATILE));
  return cache;
}

/* Source-level argument types.  */
#define MIPS_ATYPE_VOID void_type_node
#define MIPS_ATYPE_INT integer_type_node
#define MIPS_ATYPE_POINTER ptr_type_node
#define MIPS_ATYPE_CVPOINTER mips_build_cvpointer_type ()

/* Standard mode-based argument types.  */
#define MIPS_ATYPE_UQI unsigned_intQI_type_node
#define MIPS_ATYPE_SI intSI_type_node
#define MIPS_ATYPE_USI unsigned_intSI_type_node
#define MIPS_ATYPE_DI intDI_type_node
#define MIPS_ATYPE_UDI unsigned_intDI_type_node
#define MIPS_ATYPE_SF float_type_node
#define MIPS_ATYPE_DF double_type_node

/* Vector argument types.  */
#define MIPS_ATYPE_V2SF mips_builtin_vector_type (float_type_node, V2SFmode)
#define MIPS_ATYPE_V2HI mips_builtin_vector_type (intHI_type_node, V2HImode)
#define MIPS_ATYPE_V2SI mips_builtin_vector_type (intSI_type_node, V2SImode)
#define MIPS_ATYPE_V4QI mips_builtin_vector_type (intQI_type_node, V4QImode)
#define MIPS_ATYPE_V4HI mips_builtin_vector_type (intHI_type_node, V4HImode)
#define MIPS_ATYPE_V8QI mips_builtin_vector_type (intQI_type_node, V8QImode)
#define MIPS_ATYPE_UV2SI					\
  mips_builtin_vector_type (unsigned_intSI_type_node, V2SImode)
#define MIPS_ATYPE_UV4HI					\
  mips_builtin_vector_type (unsigned_intHI_type_node, V4HImode)
#define MIPS_ATYPE_UV8QI					\
  mips_builtin_vector_type (unsigned_intQI_type_node, V8QImode)

/* MIPS_FTYPE_ATYPESN takes N MIPS_FTYPES-like type codes and lists
   their associated MIPS_ATYPEs.  */
#define MIPS_FTYPE_ATYPES1(A, B) \
  MIPS_ATYPE_##A, MIPS_ATYPE_##B

#define MIPS_FTYPE_ATYPES2(A, B, C) \
  MIPS_ATYPE_##A, MIPS_ATYPE_##B, MIPS_ATYPE_##C

#define MIPS_FTYPE_ATYPES3(A, B, C, D) \
  MIPS_ATYPE_##A, MIPS_ATYPE_##B, MIPS_ATYPE_##C, MIPS_ATYPE_##D

#define MIPS_FTYPE_ATYPES4(A, B, C, D, E) \
  MIPS_ATYPE_##A, MIPS_ATYPE_##B, MIPS_ATYPE_##C, MIPS_ATYPE_##D, \
  MIPS_ATYPE_##E

/* Return the function type associated with function prototype TYPE.  */

static tree
mips_build_function_type (enum mips_function_type type)
{
  static tree types[(int) MIPS_MAX_FTYPE_MAX];

  if (types[(int) type] == NULL_TREE)
    switch (type)
      {
#define DEF_MIPS_FTYPE(NUM, ARGS)					\
  case MIPS_FTYPE_NAME##NUM ARGS:					\
    types[(int) type]							\
      = build_function_type_list (MIPS_FTYPE_ATYPES##NUM ARGS,		\
				  NULL_TREE);				\
    break;
#include "config/mips/mips-ftypes.def"
#undef DEF_MIPS_FTYPE
      default:
	gcc_unreachable ();
      }

  return types[(int) type];
}

/* Implement TARGET_INIT_BUILTINS.  */

static void
mips_init_builtins (void)
{
  const struct mips_builtin_description *d;
  unsigned int i;

  /* Iterate through all of the bdesc arrays, initializing all of the
     builtin functions.  */
  for (i = 0; i < ARRAY_SIZE (mips_builtins); i++)
    {
      d = &mips_builtins[i];
      if (d->avail ())
	mips_builtin_decls[i]
	  = add_builtin_function (d->name,
				  mips_build_function_type (d->function_type),
				  i, BUILT_IN_MD, NULL, NULL);
    }
}

/* Implement TARGET_BUILTIN_DECL.  */

static tree
mips_builtin_decl (unsigned int code, bool initialize_p ATTRIBUTE_UNUSED)
{
  if (code >= ARRAY_SIZE (mips_builtins))
    return error_mark_node;
  return mips_builtin_decls[code];
}

/* Take argument ARGNO from EXP's argument list and convert it into a
   form suitable for input operand OPNO of instruction ICODE.  Return the
   value.  */

static rtx
mips_prepare_builtin_arg (enum insn_code icode,
			  unsigned int opno, tree exp, unsigned int argno)
{
  tree arg;
  rtx value;
  enum machine_mode mode;

  arg = CALL_EXPR_ARG (exp, argno);
  value = expand_normal (arg);
  mode = insn_data[icode].operand[opno].mode;
  if (!insn_data[icode].operand[opno].predicate (value, mode))
    {
      /* We need to get the mode from ARG for two reasons:

	   - to cope with address operands, where MODE is the mode of the
	     memory, rather than of VALUE itself.

	   - to cope with special predicates like pmode_register_operand,
	     where MODE is VOIDmode.  */
      value = copy_to_mode_reg (TYPE_MODE (TREE_TYPE (arg)), value);

      /* Check the predicate again.  */
      if (!insn_data[icode].operand[opno].predicate (value, mode))
	{
	  error ("invalid argument to built-in function");
	  return const0_rtx;
	}
    }

  return value;
}

/* Return an rtx suitable for output operand OP of instruction ICODE.
   If TARGET is non-null, try to use it where possible.  */

static rtx
mips_prepare_builtin_target (enum insn_code icode, unsigned int op, rtx target)
{
  enum machine_mode mode;

  mode = insn_data[icode].operand[op].mode;
  if (target == 0 || !insn_data[icode].operand[op].predicate (target, mode))
    target = gen_reg_rtx (mode);

  return target;
}

/* Expand a MIPS_BUILTIN_DIRECT or MIPS_BUILTIN_DIRECT_NO_TARGET function;
   HAS_TARGET_P says which.  EXP is the CALL_EXPR that calls the function
   and ICODE is the code of the associated .md pattern.  TARGET, if nonnull,
   suggests a good place to put the result.  */

static rtx
mips_expand_builtin_direct (enum insn_code icode, rtx target, tree exp,
			    bool has_target_p)
{
  rtx ops[MAX_RECOG_OPERANDS];
  int opno, argno;

  /* Map any target to operand 0.  */
  opno = 0;
  if (has_target_p)
    {
      target = mips_prepare_builtin_target (icode, opno, target);
      ops[opno] = target;
      opno++;
    }

  /* Map the arguments to the other operands.  The n_operands value
     for an expander includes match_dups and match_scratches as well as
     match_operands, so n_operands is only an upper bound on the number
     of arguments to the expander function.  */
  gcc_assert (opno + call_expr_nargs (exp) <= insn_data[icode].n_operands);
  for (argno = 0; argno < call_expr_nargs (exp); argno++, opno++)
    ops[opno] = mips_prepare_builtin_arg (icode, opno, exp, argno);

  switch (opno)
    {
    case 2:
      emit_insn (GEN_FCN (icode) (ops[0], ops[1]));
      break;

    case 3:
      emit_insn (GEN_FCN (icode) (ops[0], ops[1], ops[2]));
      break;

    case 4:
      emit_insn (GEN_FCN (icode) (ops[0], ops[1], ops[2], ops[3]));
      break;

    default:
      gcc_unreachable ();
    }
  return target;
}

/* Expand a __builtin_mips_movt_*_ps or __builtin_mips_movf_*_ps
   function; TYPE says which.  EXP is the CALL_EXPR that calls the
   function, ICODE is the instruction that should be used to compare
   the first two arguments, and COND is the condition it should test.
   TARGET, if nonnull, suggests a good place to put the result.  */

static rtx
mips_expand_builtin_movtf (enum mips_builtin_type type,
			   enum insn_code icode, enum mips_fp_condition cond,
			   rtx target, tree exp)
{
  rtx cmp_result, op0, op1;

  cmp_result = mips_prepare_builtin_target (icode, 0, 0);
  op0 = mips_prepare_builtin_arg (icode, 1, exp, 0);
  op1 = mips_prepare_builtin_arg (icode, 2, exp, 1);
  emit_insn (GEN_FCN (icode) (cmp_result, op0, op1, GEN_INT (cond)));

  icode = CODE_FOR_mips_cond_move_tf_ps;
  target = mips_prepare_builtin_target (icode, 0, target);
  if (type == MIPS_BUILTIN_MOVT)
    {
      op1 = mips_prepare_builtin_arg (icode, 2, exp, 2);
      op0 = mips_prepare_builtin_arg (icode, 1, exp, 3);
    }
  else
    {
      op0 = mips_prepare_builtin_arg (icode, 1, exp, 2);
      op1 = mips_prepare_builtin_arg (icode, 2, exp, 3);
    }
  emit_insn (gen_mips_cond_move_tf_ps (target, op0, op1, cmp_result));
  return target;
}

/* Move VALUE_IF_TRUE into TARGET if CONDITION is true; move VALUE_IF_FALSE
   into TARGET otherwise.  Return TARGET.  */

static rtx
mips_builtin_branch_and_move (rtx condition, rtx target,
			      rtx value_if_true, rtx value_if_false)
{
  rtx true_label, done_label;

  true_label = gen_label_rtx ();
  done_label = gen_label_rtx ();

  /* First assume that CONDITION is false.  */
  mips_emit_move (target, value_if_false);

  /* Branch to TRUE_LABEL if CONDITION is true and DONE_LABEL otherwise.  */
  emit_jump_insn (gen_condjump (condition, true_label));
  emit_jump_insn (gen_jump (done_label));
  emit_barrier ();

  /* Fix TARGET if CONDITION is true.  */
  emit_label (true_label);
  mips_emit_move (target, value_if_true);

  emit_label (done_label);
  return target;
}

/* Expand a comparison built-in function of type BUILTIN_TYPE.  EXP is
   the CALL_EXPR that calls the function, ICODE is the code of the
   comparison instruction, and COND is the condition it should test.
   TARGET, if nonnull, suggests a good place to put the boolean result.  */

static rtx
mips_expand_builtin_compare (enum mips_builtin_type builtin_type,
			     enum insn_code icode, enum mips_fp_condition cond,
			     rtx target, tree exp)
{
  rtx offset, condition, cmp_result, args[MAX_RECOG_OPERANDS];
  int argno;

  if (target == 0 || GET_MODE (target) != SImode)
    target = gen_reg_rtx (SImode);

  /* The instruction should have a target operand, an operand for each
     argument, and an operand for COND.  */
  gcc_assert (call_expr_nargs (exp) + 2 == insn_data[icode].n_operands);

  /* Prepare the operands to the comparison.  */
  cmp_result = mips_prepare_builtin_target (icode, 0, 0);
  for (argno = 0; argno < call_expr_nargs (exp); argno++)
    args[argno] = mips_prepare_builtin_arg (icode, argno + 1, exp, argno);

  switch (insn_data[icode].n_operands)
    {
    case 4:
      emit_insn (GEN_FCN (icode) (cmp_result, args[0], args[1],
				  GEN_INT (cond)));
      break;

    case 6:
      emit_insn (GEN_FCN (icode) (cmp_result, args[0], args[1],
				  args[2], args[3], GEN_INT (cond)));
      break;

    default:
      gcc_unreachable ();
    }

  /* If the comparison sets more than one register, we define the result
     to be 0 if all registers are false and -1 if all registers are true.
     The value of the complete result is indeterminate otherwise.  */
  switch (builtin_type)
    {
    case MIPS_BUILTIN_CMP_ALL:
      condition = gen_rtx_NE (VOIDmode, cmp_result, constm1_rtx);
      return mips_builtin_branch_and_move (condition, target,
					   const0_rtx, const1_rtx);

    case MIPS_BUILTIN_CMP_UPPER:
    case MIPS_BUILTIN_CMP_LOWER:
      offset = GEN_INT (builtin_type == MIPS_BUILTIN_CMP_UPPER);
      condition = gen_single_cc (cmp_result, offset);
      return mips_builtin_branch_and_move (condition, target,
					   const1_rtx, const0_rtx);

    default:
      condition = gen_rtx_NE (VOIDmode, cmp_result, const0_rtx);
      return mips_builtin_branch_and_move (condition, target,
					   const1_rtx, const0_rtx);
    }
}

/* Expand a bposge built-in function of type BUILTIN_TYPE.  TARGET,
   if nonnull, suggests a good place to put the boolean result.  */

static rtx
mips_expand_builtin_bposge (enum mips_builtin_type builtin_type, rtx target)
{
  rtx condition, cmp_result;
  int cmp_value;

  if (target == 0 || GET_MODE (target) != SImode)
    target = gen_reg_rtx (SImode);

  cmp_result = gen_rtx_REG (CCDSPmode, CCDSP_PO_REGNUM);

  if (builtin_type == MIPS_BUILTIN_BPOSGE32)
    cmp_value = 32;
  else
    gcc_assert (0);

  condition = gen_rtx_GE (VOIDmode, cmp_result, GEN_INT (cmp_value));
  return mips_builtin_branch_and_move (condition, target,
				       const1_rtx, const0_rtx);
}

/* Implement TARGET_EXPAND_BUILTIN.  */

static rtx
mips_expand_builtin (tree exp, rtx target, rtx subtarget ATTRIBUTE_UNUSED,
		     enum machine_mode mode, int ignore)
{
  tree fndecl;
  unsigned int fcode, avail;
  const struct mips_builtin_description *d;

  fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  fcode = DECL_FUNCTION_CODE (fndecl);
  gcc_assert (fcode < ARRAY_SIZE (mips_builtins));
  d = &mips_builtins[fcode];
  avail = d->avail ();
  gcc_assert (avail != 0);
  if (TARGET_MIPS16)
    {
      error ("built-in function %qE not supported for MIPS16",
	     DECL_NAME (fndecl));
      return ignore ? const0_rtx : CONST0_RTX (mode);
    }
  switch (d->builtin_type)
    {
    case MIPS_BUILTIN_DIRECT:
      return mips_expand_builtin_direct (d->icode, target, exp, true);

    case MIPS_BUILTIN_DIRECT_NO_TARGET:
      return mips_expand_builtin_direct (d->icode, target, exp, false);

    case MIPS_BUILTIN_MOVT:
    case MIPS_BUILTIN_MOVF:
      return mips_expand_builtin_movtf (d->builtin_type, d->icode,
					d->cond, target, exp);

    case MIPS_BUILTIN_CMP_ANY:
    case MIPS_BUILTIN_CMP_ALL:
    case MIPS_BUILTIN_CMP_UPPER:
    case MIPS_BUILTIN_CMP_LOWER:
    case MIPS_BUILTIN_CMP_SINGLE:
      return mips_expand_builtin_compare (d->builtin_type, d->icode,
					  d->cond, target, exp);

    case MIPS_BUILTIN_BPOSGE32:
      return mips_expand_builtin_bposge (d->builtin_type, target);
    }
  gcc_unreachable ();
}

/* An entry in the MIPS16 constant pool.  VALUE is the pool constant,
   MODE is its mode, and LABEL is the CODE_LABEL associated with it.  */
struct mips16_constant {
  struct mips16_constant *next;
  rtx value;
  rtx label;
  enum machine_mode mode;
};

/* Information about an incomplete MIPS16 constant pool.  FIRST is the
   first constant, HIGHEST_ADDRESS is the highest address that the first
   byte of the pool can have, and INSN_ADDRESS is the current instruction
   address.  */
struct mips16_constant_pool {
  struct mips16_constant *first;
  int highest_address;
  int insn_address;
};

/* Add constant VALUE to POOL and return its label.  MODE is the
   value's mode (used for CONST_INTs, etc.).  */

static rtx
mips16_add_constant (struct mips16_constant_pool *pool,
		     rtx value, enum machine_mode mode)
{
  struct mips16_constant **p, *c;
  bool first_of_size_p;

  /* See whether the constant is already in the pool.  If so, return the
     existing label, otherwise leave P pointing to the place where the
     constant should be added.

     Keep the pool sorted in increasing order of mode size so that we can
     reduce the number of alignments needed.  */
  first_of_size_p = true;
  for (p = &pool->first; *p != 0; p = &(*p)->next)
    {
      if (mode == (*p)->mode && rtx_equal_p (value, (*p)->value))
	return (*p)->label;
      if (GET_MODE_SIZE (mode) < GET_MODE_SIZE ((*p)->mode))
	break;
      if (GET_MODE_SIZE (mode) == GET_MODE_SIZE ((*p)->mode))
	first_of_size_p = false;
    }

  /* In the worst case, the constant needed by the earliest instruction
     will end up at the end of the pool.  The entire pool must then be
     accessible from that instruction.

     When adding the first constant, set the pool's highest address to
     the address of the first out-of-range byte.  Adjust this address
     downwards each time a new constant is added.  */
  if (pool->first == 0)
    /* For LWPC, ADDIUPC and DADDIUPC, the base PC value is the address
       of the instruction with the lowest two bits clear.  The base PC
       value for LDPC has the lowest three bits clear.  Assume the worst
       case here; namely that the PC-relative instruction occupies the
       last 2 bytes in an aligned word.  */
    pool->highest_address = pool->insn_address - (UNITS_PER_WORD - 2) + 0x8000;
  pool->highest_address -= GET_MODE_SIZE (mode);
  if (first_of_size_p)
    /* Take into account the worst possible padding due to alignment.  */
    pool->highest_address -= GET_MODE_SIZE (mode) - 1;

  /* Create a new entry.  */
  c = XNEW (struct mips16_constant);
  c->value = value;
  c->mode = mode;
  c->label = gen_label_rtx ();
  c->next = *p;
  *p = c;

  return c->label;
}

/* Output constant VALUE after instruction INSN and return the last
   instruction emitted.  MODE is the mode of the constant.  */

static rtx
mips16_emit_constants_1 (enum machine_mode mode, rtx value, rtx insn)
{
  if (SCALAR_INT_MODE_P (mode) || ALL_SCALAR_FIXED_POINT_MODE_P (mode))
    {
      rtx size = GEN_INT (GET_MODE_SIZE (mode));
      return emit_insn_after (gen_consttable_int (value, size), insn);
    }

  if (SCALAR_FLOAT_MODE_P (mode))
    return emit_insn_after (gen_consttable_float (value), insn);

  if (VECTOR_MODE_P (mode))
    {
      int i;

      for (i = 0; i < CONST_VECTOR_NUNITS (value); i++)
	insn = mips16_emit_constants_1 (GET_MODE_INNER (mode),
					CONST_VECTOR_ELT (value, i), insn);
      return insn;
    }

  gcc_unreachable ();
}

/* Dump out the constants in CONSTANTS after INSN.  */

static void
mips16_emit_constants (struct mips16_constant *constants, rtx insn)
{
  struct mips16_constant *c, *next;
  int align;

  align = 0;
  for (c = constants; c != NULL; c = next)
    {
      /* If necessary, increase the alignment of PC.  */
      if (align < GET_MODE_SIZE (c->mode))
	{
	  int align_log = floor_log2 (GET_MODE_SIZE (c->mode));
	  insn = emit_insn_after (gen_align (GEN_INT (align_log)), insn);
	}
      align = GET_MODE_SIZE (c->mode);

      insn = emit_label_after (c->label, insn);
      insn = mips16_emit_constants_1 (c->mode, c->value, insn);

      next = c->next;
      free (c);
    }

  emit_barrier_after (insn);
}

/* Return the length of instruction INSN.  */

static int
mips16_insn_length (rtx insn)
{
  if (JUMP_P (insn))
    {
      rtx body = PATTERN (insn);
      if (GET_CODE (body) == ADDR_VEC)
	return GET_MODE_SIZE (GET_MODE (body)) * XVECLEN (body, 0);
      if (GET_CODE (body) == ADDR_DIFF_VEC)
	return GET_MODE_SIZE (GET_MODE (body)) * XVECLEN (body, 1);
    }
  return get_attr_length (insn);
}

/* If *X is a symbolic constant that refers to the constant pool, add
   the constant to POOL and rewrite *X to use the constant's label.  */

static void
mips16_rewrite_pool_constant (struct mips16_constant_pool *pool, rtx *x)
{
  rtx base, offset, label;

  split_const (*x, &base, &offset);
  if (GET_CODE (base) == SYMBOL_REF && CONSTANT_POOL_ADDRESS_P (base))
    {
      label = mips16_add_constant (pool, get_pool_constant (base),
				   get_pool_mode (base));
      base = gen_rtx_LABEL_REF (Pmode, label);
      *x = mips_unspec_address_offset (base, offset, SYMBOL_PC_RELATIVE);
    }
}

/* This structure is used to communicate with mips16_rewrite_pool_refs.
   INSN is the instruction we're rewriting and POOL points to the current
   constant pool.  */
struct mips16_rewrite_pool_refs_info {
  rtx insn;
  struct mips16_constant_pool *pool;
};

/* Rewrite *X so that constant pool references refer to the constant's
   label instead.  DATA points to a mips16_rewrite_pool_refs_info
   structure.  */

static int
mips16_rewrite_pool_refs (rtx *x, void *data)
{
  struct mips16_rewrite_pool_refs_info *info =
    (struct mips16_rewrite_pool_refs_info *) data;

  if (force_to_mem_operand (*x, Pmode))
    {
      rtx mem = force_const_mem (GET_MODE (*x), *x);
      validate_change (info->insn, x, mem, false);
    }

  if (MEM_P (*x))
    {
      mips16_rewrite_pool_constant (info->pool, &XEXP (*x, 0));
      return -1;
    }

  if (TARGET_MIPS16_TEXT_LOADS)
    mips16_rewrite_pool_constant (info->pool, x);

  return GET_CODE (*x) == CONST ? -1 : 0;
}

/* Return whether CFG is used in mips_reorg.  */

static bool
mips_cfg_in_reorg (void)
{
  return (mips_r10k_cache_barrier != R10K_CACHE_BARRIER_NONE
	  || TARGET_RELAX_PIC_CALLS);
}

/* Build MIPS16 constant pools.  */

static void
mips16_lay_out_constants (void)
{
  struct mips16_constant_pool pool;
  struct mips16_rewrite_pool_refs_info info;
  rtx insn, barrier;

  if (!TARGET_MIPS16_PCREL_LOADS)
    return;

  if (mips_cfg_in_reorg ())
    split_all_insns ();
  else
    split_all_insns_noflow ();
  barrier = 0;
  memset (&pool, 0, sizeof (pool));
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      /* Rewrite constant pool references in INSN.  */
      if (USEFUL_INSN_P (insn))
	{
	  info.insn = insn;
	  info.pool = &pool;
	  for_each_rtx (&PATTERN (insn), mips16_rewrite_pool_refs, &info);
	}

      pool.insn_address += mips16_insn_length (insn);

      if (pool.first != NULL)
	{
	  /* If there are no natural barriers between the first user of
	     the pool and the highest acceptable address, we'll need to
	     create a new instruction to jump around the constant pool.
	     In the worst case, this instruction will be 4 bytes long.

	     If it's too late to do this transformation after INSN,
	     do it immediately before INSN.  */
	  if (barrier == 0 && pool.insn_address + 4 > pool.highest_address)
	    {
	      rtx label, jump;

	      label = gen_label_rtx ();

	      jump = emit_jump_insn_before (gen_jump (label), insn);
	      JUMP_LABEL (jump) = label;
	      LABEL_NUSES (label) = 1;
	      barrier = emit_barrier_after (jump);

	      emit_label_after (label, barrier);
	      pool.insn_address += 4;
	    }

	  /* See whether the constant pool is now out of range of the first
	     user.  If so, output the constants after the previous barrier.
	     Note that any instructions between BARRIER and INSN (inclusive)
	     will use negative offsets to refer to the pool.  */
	  if (pool.insn_address > pool.highest_address)
	    {
	      mips16_emit_constants (pool.first, barrier);
	      pool.first = NULL;
	      barrier = 0;
	    }
	  else if (BARRIER_P (insn))
	    barrier = insn;
	}
    }
  mips16_emit_constants (pool.first, get_last_insn ());
}

/* Return true if it is worth r10k_simplify_address's while replacing
   an address with X.  We are looking for constants, and for addresses
   at a known offset from the incoming stack pointer.  */

static bool
r10k_simplified_address_p (rtx x)
{
  if (GET_CODE (x) == PLUS && CONST_INT_P (XEXP (x, 1)))
    x = XEXP (x, 0);
  return x == virtual_incoming_args_rtx || CONSTANT_P (x);
}

/* X is an expression that appears in INSN.  Try to use the UD chains
   to simplify it, returning the simplified form on success and the
   original form otherwise.  Replace the incoming value of $sp with
   virtual_incoming_args_rtx (which should never occur in X otherwise).  */

static rtx
r10k_simplify_address (rtx x, rtx insn)
{
  rtx newx, op0, op1, set, def_insn, note;
  df_ref use, def;
  struct df_link *defs;

  newx = NULL_RTX;
  if (UNARY_P (x))
    {
      op0 = r10k_simplify_address (XEXP (x, 0), insn);
      if (op0 != XEXP (x, 0))
	newx = simplify_gen_unary (GET_CODE (x), GET_MODE (x),
				   op0, GET_MODE (XEXP (x, 0)));
    }
  else if (BINARY_P (x))
    {
      op0 = r10k_simplify_address (XEXP (x, 0), insn);
      op1 = r10k_simplify_address (XEXP (x, 1), insn);
      if (op0 != XEXP (x, 0) || op1 != XEXP (x, 1))
	newx = simplify_gen_binary (GET_CODE (x), GET_MODE (x), op0, op1);
    }
  else if (GET_CODE (x) == LO_SUM)
    {
      /* LO_SUMs can be offset from HIGHs, if we know they won't
	 overflow.  See mips_classify_address for the rationale behind
	 the lax check.  */
      op0 = r10k_simplify_address (XEXP (x, 0), insn);
      if (GET_CODE (op0) == HIGH)
	newx = XEXP (x, 1);
    }
  else if (REG_P (x))
    {
      /* Uses are recorded by regno_reg_rtx, not X itself.  */
      use = df_find_use (insn, regno_reg_rtx[REGNO (x)]);
      gcc_assert (use);
      defs = DF_REF_CHAIN (use);

      /* Require a single definition.  */
      if (defs && defs->next == NULL)
	{
	  def = defs->ref;
	  if (DF_REF_IS_ARTIFICIAL (def))
	    {
	      /* Replace the incoming value of $sp with
		 virtual_incoming_args_rtx.  */
	      if (x == stack_pointer_rtx
		  && DF_REF_BB (def) == ENTRY_BLOCK_PTR)
		newx = virtual_incoming_args_rtx;
	    }
	  else if (dominated_by_p (CDI_DOMINATORS, DF_REF_BB (use),
				   DF_REF_BB (def)))
	    {
	      /* Make sure that DEF_INSN is a single set of REG.  */
	      def_insn = DF_REF_INSN (def);
	      if (NONJUMP_INSN_P (def_insn))
		{
		  set = single_set (def_insn);
		  if (set && rtx_equal_p (SET_DEST (set), x))
		    {
		      /* Prefer to use notes, since the def-use chains
			 are often shorter.  */
		      note = find_reg_equal_equiv_note (def_insn);
		      if (note)
			newx = XEXP (note, 0);
		      else
			newx = SET_SRC (set);
		      newx = r10k_simplify_address (newx, def_insn);
		    }
		}
	    }
	}
    }
  if (newx && r10k_simplified_address_p (newx))
    return newx;
  return x;
}

/* Return true if ADDRESS is known to be an uncached address
   on R10K systems.  */

static bool
r10k_uncached_address_p (unsigned HOST_WIDE_INT address)
{
  unsigned HOST_WIDE_INT upper;

  /* Check for KSEG1.  */
  if (address + 0x60000000 < 0x20000000)
    return true;

  /* Check for uncached XKPHYS addresses.  */
  if (Pmode == DImode)
    {
      upper = (address >> 40) & 0xf9ffff;
      if (upper == 0x900000 || upper == 0xb80000)
	return true;
    }
  return false;
}

/* Return true if we can prove that an access to address X in instruction
   INSN would be safe from R10K speculation.  This X is a general
   expression; it might not be a legitimate address.  */

static bool
r10k_safe_address_p (rtx x, rtx insn)
{
  rtx base, offset;
  HOST_WIDE_INT offset_val;

  x = r10k_simplify_address (x, insn);

  /* Check for references to the stack frame.  It doesn't really matter
     how much of the frame has been allocated at INSN; -mr10k-cache-barrier
     allows us to assume that accesses to any part of the eventual frame
     is safe from speculation at any point in the function.  */
  mips_split_plus (x, &base, &offset_val);
  if (base == virtual_incoming_args_rtx
      && offset_val >= -cfun->machine->frame.total_size
      && offset_val < cfun->machine->frame.args_size)
    return true;

  /* Check for uncached addresses.  */
  if (CONST_INT_P (x))
    return r10k_uncached_address_p (INTVAL (x));

  /* Check for accesses to a static object.  */
  split_const (x, &base, &offset);
  return offset_within_block_p (base, INTVAL (offset));
}

/* Return true if a MEM with MEM_EXPR EXPR and MEM_OFFSET OFFSET is
   an in-range access to an automatic variable, or to an object with
   a link-time-constant address.  */

static bool
r10k_safe_mem_expr_p (tree expr, rtx offset)
{
  if (expr == NULL_TREE
      || offset == NULL_RTX
      || !CONST_INT_P (offset)
      || INTVAL (offset) < 0
      || INTVAL (offset) >= int_size_in_bytes (TREE_TYPE (expr)))
    return false;

  while (TREE_CODE (expr) == COMPONENT_REF)
    {
      expr = TREE_OPERAND (expr, 0);
      if (expr == NULL_TREE)
	return false;
    }

  return DECL_P (expr);
}

/* A for_each_rtx callback for which DATA points to the instruction
   containing *X.  Stop the search if we find a MEM that is not safe
   from R10K speculation.  */

static int
r10k_needs_protection_p_1 (rtx *loc, void *data)
{
  rtx mem;

  mem = *loc;
  if (!MEM_P (mem))
    return 0;

  if (r10k_safe_mem_expr_p (MEM_EXPR (mem), MEM_OFFSET (mem)))
    return -1;

  if (r10k_safe_address_p (XEXP (mem, 0), (rtx) data))
    return -1;

  return 1;
}

/* A note_stores callback for which DATA points to an instruction pointer.
   If *DATA is nonnull, make it null if it X contains a MEM that is not
   safe from R10K speculation.  */

static void
r10k_needs_protection_p_store (rtx x, const_rtx pat ATTRIBUTE_UNUSED,
			       void *data)
{
  rtx *insn_ptr;

  insn_ptr = (rtx *) data;
  if (*insn_ptr && for_each_rtx (&x, r10k_needs_protection_p_1, *insn_ptr))
    *insn_ptr = NULL_RTX;
}

/* A for_each_rtx callback that iterates over the pattern of a CALL_INSN.
   Return nonzero if the call is not to a declared function.  */

static int
r10k_needs_protection_p_call (rtx *loc, void *data ATTRIBUTE_UNUSED)
{
  rtx x;

  x = *loc;
  if (!MEM_P (x))
    return 0;

  x = XEXP (x, 0);
  if (GET_CODE (x) == SYMBOL_REF && SYMBOL_REF_DECL (x))
    return -1;

  return 1;
}

/* Return true if instruction INSN needs to be protected by an R10K
   cache barrier.  */

static bool
r10k_needs_protection_p (rtx insn)
{
  if (CALL_P (insn))
    return for_each_rtx (&PATTERN (insn), r10k_needs_protection_p_call, NULL);

  if (mips_r10k_cache_barrier == R10K_CACHE_BARRIER_STORE)
    {
      note_stores (PATTERN (insn), r10k_needs_protection_p_store, &insn);
      return insn == NULL_RTX;
    }

  return for_each_rtx (&PATTERN (insn), r10k_needs_protection_p_1, insn);
}

/* Return true if BB is only reached by blocks in PROTECTED_BBS and if every
   edge is unconditional.  */

static bool
r10k_protected_bb_p (basic_block bb, sbitmap protected_bbs)
{
  edge_iterator ei;
  edge e;

  FOR_EACH_EDGE (e, ei, bb->preds)
    if (!single_succ_p (e->src)
	|| !TEST_BIT (protected_bbs, e->src->index)
	|| (e->flags & EDGE_COMPLEX) != 0)
      return false;
  return true;
}

/* Implement -mr10k-cache-barrier= for the current function.  */

static void
r10k_insert_cache_barriers (void)
{
  int *rev_post_order;
  unsigned int i, n;
  basic_block bb;
  sbitmap protected_bbs;
  rtx insn, end, unprotected_region;

  if (TARGET_MIPS16)
    {
      sorry ("%qs does not support MIPS16 code", "-mr10k-cache-barrier");
      return;
    }

  /* Calculate dominators.  */
  calculate_dominance_info (CDI_DOMINATORS);

  /* Bit X of PROTECTED_BBS is set if the last operation in basic block
     X is protected by a cache barrier.  */
  protected_bbs = sbitmap_alloc (last_basic_block);
  sbitmap_zero (protected_bbs);

  /* Iterate over the basic blocks in reverse post-order.  */
  rev_post_order = XNEWVEC (int, last_basic_block);
  n = pre_and_rev_post_order_compute (NULL, rev_post_order, false);
  for (i = 0; i < n; i++)
    {
      bb = BASIC_BLOCK (rev_post_order[i]);

      /* If this block is only reached by unconditional edges, and if the
	 source of every edge is protected, the beginning of the block is
	 also protected.  */
      if (r10k_protected_bb_p (bb, protected_bbs))
	unprotected_region = NULL_RTX;
      else
	unprotected_region = pc_rtx;
      end = NEXT_INSN (BB_END (bb));

      /* UNPROTECTED_REGION is:

	 - null if we are processing a protected region,
	 - pc_rtx if we are processing an unprotected region but have
	   not yet found the first instruction in it
	 - the first instruction in an unprotected region otherwise.  */
      for (insn = BB_HEAD (bb); insn != end; insn = NEXT_INSN (insn))
	{
	  if (unprotected_region && USEFUL_INSN_P (insn))
	    {
	      if (recog_memoized (insn) == CODE_FOR_mips_cache)
		/* This CACHE instruction protects the following code.  */
		unprotected_region = NULL_RTX;
	      else
		{
		  /* See if INSN is the first instruction in this
		     unprotected region.  */
		  if (unprotected_region == pc_rtx)
		    unprotected_region = insn;

		  /* See if INSN needs to be protected.  If so,
		     we must insert a cache barrier somewhere between
		     PREV_INSN (UNPROTECTED_REGION) and INSN.  It isn't
		     clear which position is better performance-wise,
		     but as a tie-breaker, we assume that it is better
		     to allow delay slots to be back-filled where
		     possible, and that it is better not to insert
		     barriers in the middle of already-scheduled code.
		     We therefore insert the barrier at the beginning
		     of the region.  */
		  if (r10k_needs_protection_p (insn))
		    {
		      emit_insn_before (gen_r10k_cache_barrier (),
					unprotected_region);
		      unprotected_region = NULL_RTX;
		    }
		}
	    }

	  if (CALL_P (insn))
	    /* The called function is not required to protect the exit path.
	       The code that follows a call is therefore unprotected.  */
	    unprotected_region = pc_rtx;
	}

      /* Record whether the end of this block is protected.  */
      if (unprotected_region == NULL_RTX)
	SET_BIT (protected_bbs, bb->index);
    }
  XDELETEVEC (rev_post_order);

  sbitmap_free (protected_bbs);

  free_dominance_info (CDI_DOMINATORS);
}

/* If INSN is a call, return the underlying CALL expr.  Return NULL_RTX
   otherwise.  If INSN has two call rtx, then store the second one in
   SECOND_CALL.  */

static rtx
mips_call_expr_from_insn (rtx insn, rtx *second_call)
{
  rtx x;
  rtx x2;

  if (!CALL_P (insn))
    return NULL_RTX;

  x = PATTERN (insn);
  if (GET_CODE (x) == PARALLEL)
    {
      /* Calls returning complex values have two CALL rtx.  Look for the second
	 one here, and return it via the SECOND_CALL arg.  */
      x2 = XVECEXP (x, 0, 1);
      if (GET_CODE (x2) == SET)
	x2 = XEXP (x2, 1);
      if (GET_CODE (x2) == CALL)
	*second_call = x2;

      x = XVECEXP (x, 0, 0);
    }
  if (GET_CODE (x) == SET)
    x = XEXP (x, 1);
  gcc_assert (GET_CODE (x) == CALL);

  return x;
}

/* REG is set in DEF.  See if the definition is one of the ways we load a
   register with a symbol address for a mips_use_pic_fn_addr_reg_p call.
   If it is, return the symbol reference of the function, otherwise return
   NULL_RTX.

   If RECURSE_P is true, use mips_find_pic_call_symbol to interpret
   the values of source registers, otherwise treat such registers as
   having an unknown value.  */

static rtx
mips_pic_call_symbol_from_set (df_ref def, rtx reg, bool recurse_p)
{
  rtx def_insn, set;

  if (DF_REF_IS_ARTIFICIAL (def))
    return NULL_RTX;

  def_insn = DF_REF_INSN (def);
  set = single_set (def_insn);
  if (set && rtx_equal_p (SET_DEST (set), reg))
    {
      rtx note, src, symbol;

      /* First, look at REG_EQUAL/EQUIV notes.  */
      note = find_reg_equal_equiv_note (def_insn);
      if (note && GET_CODE (XEXP (note, 0)) == SYMBOL_REF)
	return XEXP (note, 0);

      /* For %call16 references we don't have REG_EQUAL.  */
      src = SET_SRC (set);
      symbol = mips_strip_unspec_call (src);
      if (symbol)
	{
	  gcc_assert (GET_CODE (symbol) == SYMBOL_REF);
	  return symbol;
	}

      /* Follow at most one simple register copy.  Such copies are
	 interesting in cases like:

	     for (...)
	       {
	         locally_binding_fn (...);
	       }

	 and:

	     locally_binding_fn (...);
	     ...
	     locally_binding_fn (...);

	 where the load of locally_binding_fn can legitimately be
	 hoisted or shared.  However, we do not expect to see complex
	 chains of copies, so a full worklist solution to the problem
	 would probably be overkill.  */
      if (recurse_p && REG_P (src))
	return mips_find_pic_call_symbol (def_insn, src, false);
    }

  return NULL_RTX;
}

/* Find the definition of the use of REG in INSN.  See if the definition
   is one of the ways we load a register with a symbol address for a
   mips_use_pic_fn_addr_reg_p call.  If it is return the symbol reference
   of the function, otherwise return NULL_RTX.  RECURSE_P is as for
   mips_pic_call_symbol_from_set.  */

static rtx
mips_find_pic_call_symbol (rtx insn, rtx reg, bool recurse_p)
{
  df_ref use;
  struct df_link *defs;
  rtx symbol;

  use = df_find_use (insn, regno_reg_rtx[REGNO (reg)]);
  if (!use)
    return NULL_RTX;
  defs = DF_REF_CHAIN (use);
  if (!defs)
    return NULL_RTX;
  symbol = mips_pic_call_symbol_from_set (defs->ref, reg, recurse_p);
  if (!symbol)
    return NULL_RTX;

  /* If we have more than one definition, they need to be identical.  */
  for (defs = defs->next; defs; defs = defs->next)
    {
      rtx other;

      other = mips_pic_call_symbol_from_set (defs->ref, reg, recurse_p);
      if (!rtx_equal_p (symbol, other))
	return NULL_RTX;
    }

  return symbol;
}

/* Replace the args_size operand of the call expression CALL with the
   call-attribute UNSPEC and fill in SYMBOL as the function symbol.  */

static void
mips_annotate_pic_call_expr (rtx call, rtx symbol)
{
  rtx args_size;

  args_size = XEXP (call, 1);
  XEXP (call, 1) = gen_rtx_UNSPEC (GET_MODE (args_size),
				   gen_rtvec (2, args_size, symbol),
				   UNSPEC_CALL_ATTR);
}

/* OPERANDS[ARGS_SIZE_OPNO] is the arg_size operand of a CALL expression.  See
   if instead of the arg_size argument it contains the call attributes.  If
   yes return true along with setting OPERANDS[ARGS_SIZE_OPNO] to the function
   symbol from the call attributes.  Also return false if ARGS_SIZE_OPNO is
   -1.  */

bool
mips_get_pic_call_symbol (rtx *operands, int args_size_opno)
{
  rtx args_size, symbol;

  if (!TARGET_RELAX_PIC_CALLS || args_size_opno == -1)
    return false;

  args_size = operands[args_size_opno];
  if (GET_CODE (args_size) != UNSPEC)
    return false;
  gcc_assert (XINT (args_size, 1) == UNSPEC_CALL_ATTR);

  symbol = XVECEXP (args_size, 0, 1);
  gcc_assert (GET_CODE (symbol) == SYMBOL_REF);

  operands[args_size_opno] = symbol;
  return true;
}

/* Use DF to annotate PIC indirect calls with the function symbol they
   dispatch to.  */

static void
mips_annotate_pic_calls (void)
{
  basic_block bb;
  rtx insn;

  FOR_EACH_BB (bb)
    FOR_BB_INSNS (bb, insn)
    {
      rtx call, reg, symbol, second_call;

      second_call = 0;
      call = mips_call_expr_from_insn (insn, &second_call);
      if (!call)
	continue;
      gcc_assert (MEM_P (XEXP (call, 0)));
      reg = XEXP (XEXP (call, 0), 0);
      if (!REG_P (reg))
	continue;

      symbol = mips_find_pic_call_symbol (insn, reg, true);
      if (symbol)
	{
	  mips_annotate_pic_call_expr (call, symbol);
	  if (second_call)
	    mips_annotate_pic_call_expr (second_call, symbol);
	}
    }
}

/* A temporary variable used by for_each_rtx callbacks, etc.  */
static rtx mips_sim_insn;

/* A structure representing the state of the processor pipeline.
   Used by the mips_sim_* family of functions.  */
struct mips_sim {
  /* The maximum number of instructions that can be issued in a cycle.
     (Caches mips_issue_rate.)  */
  unsigned int issue_rate;

  /* The current simulation time.  */
  unsigned int time;

  /* How many more instructions can be issued in the current cycle.  */
  unsigned int insns_left;

  /* LAST_SET[X].INSN is the last instruction to set register X.
     LAST_SET[X].TIME is the time at which that instruction was issued.
     INSN is null if no instruction has yet set register X.  */
  struct {
    rtx insn;
    unsigned int time;
  } last_set[FIRST_PSEUDO_REGISTER];

  /* The pipeline's current DFA state.  */
  state_t dfa_state;
};

/* Reset STATE to the initial simulation state.  */

static void
mips_sim_reset (struct mips_sim *state)
{
  state->time = 0;
  state->insns_left = state->issue_rate;
  memset (&state->last_set, 0, sizeof (state->last_set));
  state_reset (state->dfa_state);
}

/* Initialize STATE before its first use.  DFA_STATE points to an
   allocated but uninitialized DFA state.  */

static void
mips_sim_init (struct mips_sim *state, state_t dfa_state)
{
  state->issue_rate = mips_issue_rate ();
  state->dfa_state = dfa_state;
  mips_sim_reset (state);
}

/* Advance STATE by one clock cycle.  */

static void
mips_sim_next_cycle (struct mips_sim *state)
{
  state->time++;
  state->insns_left = state->issue_rate;
  state_transition (state->dfa_state, 0);
}

/* Advance simulation state STATE until instruction INSN can read
   register REG.  */

static void
mips_sim_wait_reg (struct mips_sim *state, rtx insn, rtx reg)
{
  unsigned int regno, end_regno;

  end_regno = END_REGNO (reg);
  for (regno = REGNO (reg); regno < end_regno; regno++)
    if (state->last_set[regno].insn != 0)
      {
	unsigned int t;

	t = (state->last_set[regno].time
	     + insn_latency (state->last_set[regno].insn, insn));
	while (state->time < t)
	  mips_sim_next_cycle (state);
    }
}

/* A for_each_rtx callback.  If *X is a register, advance simulation state
   DATA until mips_sim_insn can read the register's value.  */

static int
mips_sim_wait_regs_2 (rtx *x, void *data)
{
  if (REG_P (*x))
    mips_sim_wait_reg ((struct mips_sim *) data, mips_sim_insn, *x);
  return 0;
}

/* Call mips_sim_wait_regs_2 (R, DATA) for each register R mentioned in *X.  */

static void
mips_sim_wait_regs_1 (rtx *x, void *data)
{
  for_each_rtx (x, mips_sim_wait_regs_2, data);
}

/* Advance simulation state STATE until all of INSN's register
   dependencies are satisfied.  */

static void
mips_sim_wait_regs (struct mips_sim *state, rtx insn)
{
  mips_sim_insn = insn;
  note_uses (&PATTERN (insn), mips_sim_wait_regs_1, state);
}

/* Advance simulation state STATE until the units required by
   instruction INSN are available.  */

static void
mips_sim_wait_units (struct mips_sim *state, rtx insn)
{
  state_t tmp_state;

  tmp_state = alloca (state_size ());
  while (state->insns_left == 0
	 || (memcpy (tmp_state, state->dfa_state, state_size ()),
	     state_transition (tmp_state, insn) >= 0))
    mips_sim_next_cycle (state);
}

/* Advance simulation state STATE until INSN is ready to issue.  */

static void
mips_sim_wait_insn (struct mips_sim *state, rtx insn)
{
  mips_sim_wait_regs (state, insn);
  mips_sim_wait_units (state, insn);
}

/* mips_sim_insn has just set X.  Update the LAST_SET array
   in simulation state DATA.  */

static void
mips_sim_record_set (rtx x, const_rtx pat ATTRIBUTE_UNUSED, void *data)
{
  struct mips_sim *state;

  state = (struct mips_sim *) data;
  if (REG_P (x))
    {
      unsigned int regno, end_regno;

      end_regno = END_REGNO (x);
      for (regno = REGNO (x); regno < end_regno; regno++)
	{
	  state->last_set[regno].insn = mips_sim_insn;
	  state->last_set[regno].time = state->time;
	}
    }
}

/* Issue instruction INSN in scheduler state STATE.  Assume that INSN
   can issue immediately (i.e., that mips_sim_wait_insn has already
   been called).  */

static void
mips_sim_issue_insn (struct mips_sim *state, rtx insn)
{
  state_transition (state->dfa_state, insn);
  state->insns_left--;

  mips_sim_insn = insn;
  note_stores (PATTERN (insn), mips_sim_record_set, state);
}

/* Simulate issuing a NOP in state STATE.  */

static void
mips_sim_issue_nop (struct mips_sim *state)
{
  if (state->insns_left == 0)
    mips_sim_next_cycle (state);
  state->insns_left--;
}

/* Update simulation state STATE so that it's ready to accept the instruction
   after INSN.  INSN should be part of the main rtl chain, not a member of a
   SEQUENCE.  */

static void
mips_sim_finish_insn (struct mips_sim *state, rtx insn)
{
  /* If INSN is a jump with an implicit delay slot, simulate a nop.  */
  if (JUMP_P (insn))
    mips_sim_issue_nop (state);

  switch (GET_CODE (SEQ_BEGIN (insn)))
    {
    case CODE_LABEL:
    case CALL_INSN:
      /* We can't predict the processor state after a call or label.  */
      mips_sim_reset (state);
      break;

    case JUMP_INSN:
      /* The delay slots of branch likely instructions are only executed
	 when the branch is taken.  Therefore, if the caller has simulated
	 the delay slot instruction, STATE does not really reflect the state
	 of the pipeline for the instruction after the delay slot.  Also,
	 branch likely instructions tend to incur a penalty when not taken,
	 so there will probably be an extra delay between the branch and
	 the instruction after the delay slot.  */
      if (INSN_ANNULLED_BRANCH_P (SEQ_BEGIN (insn)))
	mips_sim_reset (state);
      break;

    default:
      break;
    }
}

/* The VR4130 pipeline issues aligned pairs of instructions together,
   but it stalls the second instruction if it depends on the first.
   In order to cut down the amount of logic required, this dependence
   check is not based on a full instruction decode.  Instead, any non-SPECIAL
   instruction is assumed to modify the register specified by bits 20-16
   (which is usually the "rt" field).

   In BEQ, BEQL, BNE and BNEL instructions, the rt field is actually an
   input, so we can end up with a false dependence between the branch
   and its delay slot.  If this situation occurs in instruction INSN,
   try to avoid it by swapping rs and rt.  */

static void
vr4130_avoid_branch_rt_conflict (rtx insn)
{
  rtx first, second;

  first = SEQ_BEGIN (insn);
  second = SEQ_END (insn);
  if (JUMP_P (first)
      && NONJUMP_INSN_P (second)
      && GET_CODE (PATTERN (first)) == SET
      && GET_CODE (SET_DEST (PATTERN (first))) == PC
      && GET_CODE (SET_SRC (PATTERN (first))) == IF_THEN_ELSE)
    {
      /* Check for the right kind of condition.  */
      rtx cond = XEXP (SET_SRC (PATTERN (first)), 0);
      if ((GET_CODE (cond) == EQ || GET_CODE (cond) == NE)
	  && REG_P (XEXP (cond, 0))
	  && REG_P (XEXP (cond, 1))
	  && reg_referenced_p (XEXP (cond, 1), PATTERN (second))
	  && !reg_referenced_p (XEXP (cond, 0), PATTERN (second)))
	{
	  /* SECOND mentions the rt register but not the rs register.  */
	  rtx tmp = XEXP (cond, 0);
	  XEXP (cond, 0) = XEXP (cond, 1);
	  XEXP (cond, 1) = tmp;
	}
    }
}

/* Implement -mvr4130-align.  Go through each basic block and simulate the
   processor pipeline.  If we find that a pair of instructions could execute
   in parallel, and the first of those instructions is not 8-byte aligned,
   insert a nop to make it aligned.  */

static void
vr4130_align_insns (void)
{
  struct mips_sim state;
  rtx insn, subinsn, last, last2, next;
  bool aligned_p;

  dfa_start ();

  /* LAST is the last instruction before INSN to have a nonzero length.
     LAST2 is the last such instruction before LAST.  */
  last = 0;
  last2 = 0;

  /* ALIGNED_P is true if INSN is known to be at an aligned address.  */
  aligned_p = true;

  mips_sim_init (&state, alloca (state_size ()));
  for (insn = get_insns (); insn != 0; insn = next)
    {
      unsigned int length;

      next = NEXT_INSN (insn);

      /* See the comment above vr4130_avoid_branch_rt_conflict for details.
	 This isn't really related to the alignment pass, but we do it on
	 the fly to avoid a separate instruction walk.  */
      vr4130_avoid_branch_rt_conflict (insn);

      if (USEFUL_INSN_P (insn))
	FOR_EACH_SUBINSN (subinsn, insn)
	  {
	    mips_sim_wait_insn (&state, subinsn);

	    /* If we want this instruction to issue in parallel with the
	       previous one, make sure that the previous instruction is
	       aligned.  There are several reasons why this isn't worthwhile
	       when the second instruction is a call:

	          - Calls are less likely to be performance critical,
		  - There's a good chance that the delay slot can execute
		    in parallel with the call.
	          - The return address would then be unaligned.

	       In general, if we're going to insert a nop between instructions
	       X and Y, it's better to insert it immediately after X.  That
	       way, if the nop makes Y aligned, it will also align any labels
	       between X and Y.  */
	    if (state.insns_left != state.issue_rate
		&& !CALL_P (subinsn))
	      {
		if (subinsn == SEQ_BEGIN (insn) && aligned_p)
		  {
		    /* SUBINSN is the first instruction in INSN and INSN is
		       aligned.  We want to align the previous instruction
		       instead, so insert a nop between LAST2 and LAST.

		       Note that LAST could be either a single instruction
		       or a branch with a delay slot.  In the latter case,
		       LAST, like INSN, is already aligned, but the delay
		       slot must have some extra delay that stops it from
		       issuing at the same time as the branch.  We therefore
		       insert a nop before the branch in order to align its
		       delay slot.  */
		    emit_insn_after (gen_nop (), last2);
		    aligned_p = false;
		  }
		else if (subinsn != SEQ_BEGIN (insn) && !aligned_p)
		  {
		    /* SUBINSN is the delay slot of INSN, but INSN is
		       currently unaligned.  Insert a nop between
		       LAST and INSN to align it.  */
		    emit_insn_after (gen_nop (), last);
		    aligned_p = true;
		  }
	      }
	    mips_sim_issue_insn (&state, subinsn);
	  }
      mips_sim_finish_insn (&state, insn);

      /* Update LAST, LAST2 and ALIGNED_P for the next instruction.  */
      length = get_attr_length (insn);
      if (length > 0)
	{
	  /* If the instruction is an asm statement or multi-instruction
	     mips.md patern, the length is only an estimate.  Insert an
	     8 byte alignment after it so that the following instructions
	     can be handled correctly.  */
	  if (NONJUMP_INSN_P (SEQ_BEGIN (insn))
	      && (recog_memoized (insn) < 0 || length >= 8))
	    {
	      next = emit_insn_after (gen_align (GEN_INT (3)), insn);
	      next = NEXT_INSN (next);
	      mips_sim_next_cycle (&state);
	      aligned_p = true;
	    }
	  else if (length & 4)
	    aligned_p = !aligned_p;
	  last2 = last;
	  last = insn;
	}

      /* See whether INSN is an aligned label.  */
      if (LABEL_P (insn) && label_to_alignment (insn) >= 3)
	aligned_p = true;
    }
  dfa_finish ();
}

/* This structure records that the current function has a LO_SUM
   involving SYMBOL_REF or LABEL_REF BASE and that MAX_OFFSET is
   the largest offset applied to BASE by all such LO_SUMs.  */
struct mips_lo_sum_offset {
  rtx base;
  HOST_WIDE_INT offset;
};

/* Return a hash value for SYMBOL_REF or LABEL_REF BASE.  */

static hashval_t
mips_hash_base (rtx base)
{
  int do_not_record_p;

  return hash_rtx (base, GET_MODE (base), &do_not_record_p, NULL, false);
}

/* Hash-table callbacks for mips_lo_sum_offsets.  */

static hashval_t
mips_lo_sum_offset_hash (const void *entry)
{
  return mips_hash_base (((const struct mips_lo_sum_offset *) entry)->base);
}

static int
mips_lo_sum_offset_eq (const void *entry, const void *value)
{
  return rtx_equal_p (((const struct mips_lo_sum_offset *) entry)->base,
		      (const_rtx) value);
}

/* Look up symbolic constant X in HTAB, which is a hash table of
   mips_lo_sum_offsets.  If OPTION is NO_INSERT, return true if X can be
   paired with a recorded LO_SUM, otherwise record X in the table.  */

static bool
mips_lo_sum_offset_lookup (htab_t htab, rtx x, enum insert_option option)
{
  rtx base, offset;
  void **slot;
  struct mips_lo_sum_offset *entry;

  /* Split X into a base and offset.  */
  split_const (x, &base, &offset);
  if (UNSPEC_ADDRESS_P (base))
    base = UNSPEC_ADDRESS (base);

  /* Look up the base in the hash table.  */
  slot = htab_find_slot_with_hash (htab, base, mips_hash_base (base), option);
  if (slot == NULL)
    return false;

  entry = (struct mips_lo_sum_offset *) *slot;
  if (option == INSERT)
    {
      if (entry == NULL)
	{
	  entry = XNEW (struct mips_lo_sum_offset);
	  entry->base = base;
	  entry->offset = INTVAL (offset);
	  *slot = entry;
	}
      else
	{
	  if (INTVAL (offset) > entry->offset)
	    entry->offset = INTVAL (offset);
	}
    }
  return INTVAL (offset) <= entry->offset;
}

/* A for_each_rtx callback for which DATA is a mips_lo_sum_offset hash table.
   Record every LO_SUM in *LOC.  */

static int
mips_record_lo_sum (rtx *loc, void *data)
{
  if (GET_CODE (*loc) == LO_SUM)
    mips_lo_sum_offset_lookup ((htab_t) data, XEXP (*loc, 1), INSERT);
  return 0;
}

/* Return true if INSN is a SET of an orphaned high-part relocation.
   HTAB is a hash table of mips_lo_sum_offsets that describes all the
   LO_SUMs in the current function.  */

static bool
mips_orphaned_high_part_p (htab_t htab, rtx insn)
{
  enum mips_symbol_type type;
  rtx x, set;

  set = single_set (insn);
  if (set)
    {
      /* Check for %his.  */
      x = SET_SRC (set);
      if (GET_CODE (x) == HIGH
	  && absolute_symbolic_operand (XEXP (x, 0), VOIDmode))
	return !mips_lo_sum_offset_lookup (htab, XEXP (x, 0), NO_INSERT);

      /* Check for local %gots (and %got_pages, which is redundant but OK).  */
      if (GET_CODE (x) == UNSPEC
	  && XINT (x, 1) == UNSPEC_LOAD_GOT
	  && mips_symbolic_constant_p (XVECEXP (x, 0, 1),
				       SYMBOL_CONTEXT_LEA, &type)
	  && type == SYMBOL_GOTOFF_PAGE)
	return !mips_lo_sum_offset_lookup (htab, XVECEXP (x, 0, 1), NO_INSERT);
    }
  return false;
}

/* Subroutine of mips_reorg_process_insns.  If there is a hazard between
   INSN and a previous instruction, avoid it by inserting nops after
   instruction AFTER.

   *DELAYED_REG and *HILO_DELAY describe the hazards that apply at
   this point.  If *DELAYED_REG is non-null, INSN must wait a cycle
   before using the value of that register.  *HILO_DELAY counts the
   number of instructions since the last hilo hazard (that is,
   the number of instructions since the last MFLO or MFHI).

   After inserting nops for INSN, update *DELAYED_REG and *HILO_DELAY
   for the next instruction.

   LO_REG is an rtx for the LO register, used in dependence checking.  */

static void
mips_avoid_hazard (rtx after, rtx insn, int *hilo_delay,
		   rtx *delayed_reg, rtx lo_reg)
{
  rtx pattern, set;
  int nops, ninsns;

  pattern = PATTERN (insn);

  /* Do not put the whole function in .set noreorder if it contains
     an asm statement.  We don't know whether there will be hazards
     between the asm statement and the gcc-generated code.  */
  if (GET_CODE (pattern) == ASM_INPUT || asm_noperands (pattern) >= 0)
    cfun->machine->all_noreorder_p = false;

  /* Ignore zero-length instructions (barriers and the like).  */
  ninsns = get_attr_length (insn) / 4;
  if (ninsns == 0)
    return;

  /* Work out how many nops are needed.  Note that we only care about
     registers that are explicitly mentioned in the instruction's pattern.
     It doesn't matter that calls use the argument registers or that they
     clobber hi and lo.  */
  if (*hilo_delay < 2 && reg_set_p (lo_reg, pattern))
    nops = 2 - *hilo_delay;
  else if (*delayed_reg != 0 && reg_referenced_p (*delayed_reg, pattern))
    nops = 1;
  else
    nops = 0;

  /* Insert the nops between this instruction and the previous one.
     Each new nop takes us further from the last hilo hazard.  */
  *hilo_delay += nops;
  while (nops-- > 0)
    emit_insn_after (gen_hazard_nop (), after);

  /* Set up the state for the next instruction.  */
  *hilo_delay += ninsns;
  *delayed_reg = 0;
  if (INSN_CODE (insn) >= 0)
    switch (get_attr_hazard (insn))
      {
      case HAZARD_NONE:
	break;

      case HAZARD_HILO:
	*hilo_delay = 0;
	break;

      case HAZARD_DELAY:
	set = single_set (insn);
	gcc_assert (set);
	*delayed_reg = SET_DEST (set);
	break;
      }
}

/* Go through the instruction stream and insert nops where necessary.
   Also delete any high-part relocations whose partnering low parts
   are now all dead.  See if the whole function can then be put into
   .set noreorder and .set nomacro.  */

static void
mips_reorg_process_insns (void)
{
  rtx insn, last_insn, subinsn, next_insn, lo_reg, delayed_reg;
  int hilo_delay;
  htab_t htab;

  /* Force all instructions to be split into their final form.  */
  split_all_insns_noflow ();

  /* Recalculate instruction lengths without taking nops into account.  */
  cfun->machine->ignore_hazard_length_p = true;
  shorten_branches (get_insns ());

  cfun->machine->all_noreorder_p = true;

  /* We don't track MIPS16 PC-relative offsets closely enough to make
     a good job of "set .noreorder" code in MIPS16 mode.  */
  if (TARGET_MIPS16)
    cfun->machine->all_noreorder_p = false;

  /* Code that doesn't use explicit relocs can't be ".set nomacro".  */
  if (!TARGET_EXPLICIT_RELOCS)
    cfun->machine->all_noreorder_p = false;

  /* Profiled functions can't be all noreorder because the profiler
     support uses assembler macros.  */
  if (crtl->profile)
    cfun->machine->all_noreorder_p = false;

  /* Code compiled with -mfix-vr4120 can't be all noreorder because
     we rely on the assembler to work around some errata.  */
  if (TARGET_FIX_VR4120)
    cfun->machine->all_noreorder_p = false;

  /* The same is true for -mfix-vr4130 if we might generate MFLO or
     MFHI instructions.  Note that we avoid using MFLO and MFHI if
     the VR4130 MACC and DMACC instructions are available instead;
     see the *mfhilo_{si,di}_macc patterns.  */
  if (TARGET_FIX_VR4130 && !ISA_HAS_MACCHI)
    cfun->machine->all_noreorder_p = false;

  htab = htab_create (37, mips_lo_sum_offset_hash,
		      mips_lo_sum_offset_eq, free);

  /* Make a first pass over the instructions, recording all the LO_SUMs.  */
  for (insn = get_insns (); insn != 0; insn = NEXT_INSN (insn))
    FOR_EACH_SUBINSN (subinsn, insn)
      if (USEFUL_INSN_P (subinsn))
	for_each_rtx (&PATTERN (subinsn), mips_record_lo_sum, htab);

  last_insn = 0;
  hilo_delay = 2;
  delayed_reg = 0;
  lo_reg = gen_rtx_REG (SImode, LO_REGNUM);

  /* Make a second pass over the instructions.  Delete orphaned
     high-part relocations or turn them into NOPs.  Avoid hazards
     by inserting NOPs.  */
  for (insn = get_insns (); insn != 0; insn = next_insn)
    {
      next_insn = NEXT_INSN (insn);
      if (USEFUL_INSN_P (insn))
	{
	  if (GET_CODE (PATTERN (insn)) == SEQUENCE)
	    {
	      /* If we find an orphaned high-part relocation in a delay
		 slot, it's easier to turn that instruction into a NOP than
		 to delete it.  The delay slot will be a NOP either way.  */
	      FOR_EACH_SUBINSN (subinsn, insn)
		if (INSN_P (subinsn))
		  {
		    if (mips_orphaned_high_part_p (htab, subinsn))
		      {
			PATTERN (subinsn) = gen_nop ();
			INSN_CODE (subinsn) = CODE_FOR_nop;
		      }
		    mips_avoid_hazard (last_insn, subinsn, &hilo_delay,
				       &delayed_reg, lo_reg);
		  }
	      last_insn = insn;
	    }
	  else
	    {
	      /* INSN is a single instruction.  Delete it if it's an
		 orphaned high-part relocation.  */
	      if (mips_orphaned_high_part_p (htab, insn))
		delete_insn (insn);
	      /* Also delete cache barriers if the last instruction
		 was an annulled branch.  INSN will not be speculatively
		 executed.  */
	      else if (recog_memoized (insn) == CODE_FOR_r10k_cache_barrier
		       && last_insn
		       && INSN_ANNULLED_BRANCH_P (SEQ_BEGIN (last_insn)))
		delete_insn (insn);
	      else
		{
		  mips_avoid_hazard (last_insn, insn, &hilo_delay,
				     &delayed_reg, lo_reg);
		  last_insn = insn;
		}
	    }
	}
    }

  htab_delete (htab);
}

/* If we are using a GOT, but have not decided to use a global pointer yet,
   see whether we need one to implement long branches.  Convert the ghost
   global-pointer instructions into real ones if so.  */

static bool
mips_expand_ghost_gp_insns (void)
{
  rtx insn;
  int normal_length;

  /* Quick exit if we already know that we will or won't need a
     global pointer.  */
  if (!TARGET_USE_GOT
      || cfun->machine->global_pointer == INVALID_REGNUM
      || mips_must_initialize_gp_p ())
    return false;

  shorten_branches (get_insns ());

  /* Look for a branch that is longer than normal.  The normal length for
     non-MIPS16 branches is 8, because the length includes the delay slot.
     It is 4 for MIPS16, because MIPS16 branches are extended instructions,
     but they have no delay slot.  */
  normal_length = (TARGET_MIPS16 ? 4 : 8);
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if (JUMP_P (insn)
	&& USEFUL_INSN_P (insn)
	&& get_attr_length (insn) > normal_length)
      break;

  if (insn == NULL_RTX)
    return false;

  /* We've now established that we need $gp.  */
  cfun->machine->must_initialize_gp_p = true;
  split_all_insns_noflow ();

  return true;
}

/* Subroutine of mips_reorg to manage passes that require DF.  */

static void
mips_df_reorg (void)
{
  /* Create def-use chains.  */
  df_set_flags (DF_EQ_NOTES);
  df_chain_add_problem (DF_UD_CHAIN);
  df_analyze ();

  if (TARGET_RELAX_PIC_CALLS)
    mips_annotate_pic_calls ();

  if (mips_r10k_cache_barrier != R10K_CACHE_BARRIER_NONE)
    r10k_insert_cache_barriers ();

  df_finish_pass (false);
}

/* Implement TARGET_MACHINE_DEPENDENT_REORG.  */

static void
mips_reorg (void)
{
  /* Restore the BLOCK_FOR_INSN pointers, which are needed by DF.  Also during
     insn splitting in mips16_lay_out_constants, DF insn info is only kept up
     to date if the CFG is available.  */
  if (mips_cfg_in_reorg ())
    compute_bb_for_insn ();
  mips16_lay_out_constants ();
  if (mips_cfg_in_reorg ())
    {
      mips_df_reorg ();
      free_bb_for_insn ();
    }

  if (optimize > 0 && flag_delayed_branch)
    dbr_schedule (get_insns ());
  mips_reorg_process_insns ();
  if (!TARGET_MIPS16
      && TARGET_EXPLICIT_RELOCS
      && TUNE_MIPS4130
      && TARGET_VR4130_ALIGN)
    vr4130_align_insns ();
  if (mips_expand_ghost_gp_insns ())
    /* The expansion could invalidate some of the VR4130 alignment
       optimizations, but this should be an extremely rare case anyhow.  */
    mips_reorg_process_insns ();
}

/* Implement TARGET_ASM_OUTPUT_MI_THUNK.  Generate rtl rather than asm text
   in order to avoid duplicating too much logic from elsewhere.  */

static void
mips_output_mi_thunk (FILE *file, tree thunk_fndecl ATTRIBUTE_UNUSED,
		      HOST_WIDE_INT delta, HOST_WIDE_INT vcall_offset,
		      tree function)
{
  rtx this_rtx, temp1, temp2, insn, fnaddr;
  bool use_sibcall_p;

  /* Pretend to be a post-reload pass while generating rtl.  */
  reload_completed = 1;

  /* Mark the end of the (empty) prologue.  */
  emit_note (NOTE_INSN_PROLOGUE_END);

  /* Determine if we can use a sibcall to call FUNCTION directly.  */
  fnaddr = XEXP (DECL_RTL (function), 0);
  use_sibcall_p = (mips_function_ok_for_sibcall (function, NULL)
		   && const_call_insn_operand (fnaddr, Pmode));

  /* Determine if we need to load FNADDR from the GOT.  */
  if (!use_sibcall_p
      && (mips_got_symbol_type_p
	  (mips_classify_symbol (fnaddr, SYMBOL_CONTEXT_LEA))))
    {
      /* Pick a global pointer.  Use a call-clobbered register if
	 TARGET_CALL_SAVED_GP.  */
      cfun->machine->global_pointer
	= TARGET_CALL_SAVED_GP ? 15 : GLOBAL_POINTER_REGNUM;
      cfun->machine->must_initialize_gp_p = true;
      SET_REGNO (pic_offset_table_rtx, cfun->machine->global_pointer);

      /* Set up the global pointer for n32 or n64 abicalls.  */
      mips_emit_loadgp ();
    }

  /* We need two temporary registers in some cases.  */
  temp1 = gen_rtx_REG (Pmode, 2);
  temp2 = gen_rtx_REG (Pmode, 3);

  /* Find out which register contains the "this" pointer.  */
  if (aggregate_value_p (TREE_TYPE (TREE_TYPE (function)), function))
    this_rtx = gen_rtx_REG (Pmode, GP_ARG_FIRST + 1);
  else
    this_rtx = gen_rtx_REG (Pmode, GP_ARG_FIRST);

  /* Add DELTA to THIS_RTX.  */
  if (delta != 0)
    {
      rtx offset = GEN_INT (delta);
      if (!SMALL_OPERAND (delta))
	{
	  mips_emit_move (temp1, offset);
	  offset = temp1;
	}
      emit_insn (gen_add3_insn (this_rtx, this_rtx, offset));
    }

  /* If needed, add *(*THIS_RTX + VCALL_OFFSET) to THIS_RTX.  */
  if (vcall_offset != 0)
    {
      rtx addr;

      /* Set TEMP1 to *THIS_RTX.  */
      mips_emit_move (temp1, gen_rtx_MEM (Pmode, this_rtx));

      /* Set ADDR to a legitimate address for *THIS_RTX + VCALL_OFFSET.  */
      addr = mips_add_offset (temp2, temp1, vcall_offset);

      /* Load the offset and add it to THIS_RTX.  */
      mips_emit_move (temp1, gen_rtx_MEM (Pmode, addr));
      emit_insn (gen_add3_insn (this_rtx, this_rtx, temp1));
    }

  /* Jump to the target function.  Use a sibcall if direct jumps are
     allowed, otherwise load the address into a register first.  */
  if (use_sibcall_p)
    {
      insn = emit_call_insn (gen_sibcall_internal (fnaddr, const0_rtx));
      SIBLING_CALL_P (insn) = 1;
    }
  else
    {
      /* This is messy.  GAS treats "la $25,foo" as part of a call
	 sequence and may allow a global "foo" to be lazily bound.
	 The general move patterns therefore reject this combination.

	 In this context, lazy binding would actually be OK
	 for TARGET_CALL_CLOBBERED_GP, but it's still wrong for
	 TARGET_CALL_SAVED_GP; see mips_load_call_address.
	 We must therefore load the address via a temporary
	 register if mips_dangerous_for_la25_p.

	 If we jump to the temporary register rather than $25,
	 the assembler can use the move insn to fill the jump's
	 delay slot.

	 We can use the same technique for MIPS16 code, where $25
	 is not a valid JR register.  */
      if (TARGET_USE_PIC_FN_ADDR_REG
	  && !TARGET_MIPS16
	  && !mips_dangerous_for_la25_p (fnaddr))
	temp1 = gen_rtx_REG (Pmode, PIC_FUNCTION_ADDR_REGNUM);
      mips_load_call_address (MIPS_CALL_SIBCALL, temp1, fnaddr);

      if (TARGET_USE_PIC_FN_ADDR_REG
	  && REGNO (temp1) != PIC_FUNCTION_ADDR_REGNUM)
	mips_emit_move (gen_rtx_REG (Pmode, PIC_FUNCTION_ADDR_REGNUM), temp1);
      emit_jump_insn (gen_indirect_jump (temp1));
    }

  /* Run just enough of rest_of_compilation.  This sequence was
     "borrowed" from alpha.c.  */
  insn = get_insns ();
  insn_locators_alloc ();
  split_all_insns_noflow ();
  mips16_lay_out_constants ();
  shorten_branches (insn);
  final_start_function (insn, file, 1);
  final (insn, file, 1);
  final_end_function ();

  /* Clean up the vars set above.  Note that final_end_function resets
     the global pointer for us.  */
  reload_completed = 0;
}

/* The last argument passed to mips_set_mips16_mode, or negative if the
   function hasn't been called yet.

   There are two copies of this information.  One is saved and restored
   by the PCH process while the other is specific to this compiler
   invocation.  The information calculated by mips_set_mips16_mode
   is invalid unless the two variables are the same.  */
static int was_mips16_p = -1;
static GTY(()) int was_mips16_pch_p = -1;

/* Set up the target-dependent global state so that it matches the
   current function's ISA mode.  */

static void
mips_set_mips16_mode (int mips16_p)
{
  if (mips16_p == was_mips16_p
      && mips16_p == was_mips16_pch_p)
    return;

  /* Restore base settings of various flags.  */
  target_flags = mips_base_target_flags;
  flag_schedule_insns = mips_base_schedule_insns;
  flag_reorder_blocks_and_partition = mips_base_reorder_blocks_and_partition;
  flag_move_loop_invariants = mips_base_move_loop_invariants;
  align_loops = mips_base_align_loops;
  align_jumps = mips_base_align_jumps;
  align_functions = mips_base_align_functions;

  if (mips16_p)
    {
      /* Switch to MIPS16 mode.  */
      target_flags |= MASK_MIPS16;

      /* Don't run the scheduler before reload, since it tends to
         increase register pressure.  */
      flag_schedule_insns = 0;

      /* Don't do hot/cold partitioning.  mips16_lay_out_constants expects
	 the whole function to be in a single section.  */
      flag_reorder_blocks_and_partition = 0;

      /* Don't move loop invariants, because it tends to increase
	 register pressure.  It also introduces an extra move in cases
	 where the constant is the first operand in a two-operand binary
	 instruction, or when it forms a register argument to a functon
	 call.  */
      flag_move_loop_invariants = 0;

      target_flags |= MASK_EXPLICIT_RELOCS;

      /* Experiments suggest we get the best overall section-anchor
	 results from using the range of an unextended LW or SW.  Code
	 that makes heavy use of byte or short accesses can do better
	 with ranges of 0...31 and 0...63 respectively, but most code is
	 sensitive to the range of LW and SW instead.  */
      targetm.min_anchor_offset = 0;
      targetm.max_anchor_offset = 127;

      targetm.const_anchor = 0;

      /* MIPS16 has no BAL instruction.  */
      target_flags &= ~MASK_RELAX_PIC_CALLS;

      if (flag_pic && !TARGET_OLDABI)
	sorry ("MIPS16 PIC for ABIs other than o32 and o64");

      if (TARGET_XGOT)
	sorry ("MIPS16 -mxgot code");

      if (TARGET_HARD_FLOAT_ABI && !TARGET_OLDABI)
	sorry ("hard-float MIPS16 code for ABIs other than o32 and o64");
    }
  else
    {
      /* Switch to normal (non-MIPS16) mode.  */
      target_flags &= ~MASK_MIPS16;

      /* Provide default values for align_* for 64-bit targets.  */
      if (TARGET_64BIT)
	{
	  if (align_loops == 0)
	    align_loops = 8;
	  if (align_jumps == 0)
	    align_jumps = 8;
	  if (align_functions == 0)
	    align_functions = 8;
	}

      targetm.min_anchor_offset = -32768;
      targetm.max_anchor_offset = 32767;

      targetm.const_anchor = 0x8000;
    }

  /* (Re)initialize MIPS target internals for new ISA.  */
  mips_init_relocs ();

  if (mips16_p)
    {
      if (!mips16_globals)
	mips16_globals = save_target_globals ();
      else
	restore_target_globals (mips16_globals);
    }
  else
    restore_target_globals (&default_target_globals);

  was_mips16_p = mips16_p;
  was_mips16_pch_p = mips16_p;
}

/* Implement TARGET_SET_CURRENT_FUNCTION.  Decide whether the current
   function should use the MIPS16 ISA and switch modes accordingly.  */

static void
mips_set_current_function (tree fndecl)
{
  mips_set_mips16_mode (mips_use_mips16_mode_p (fndecl));
}

/* Allocate a chunk of memory for per-function machine-dependent data.  */

static struct machine_function *
mips_init_machine_status (void)
{
  return ggc_alloc_cleared_machine_function ();
}

/* Return the processor associated with the given ISA level, or null
   if the ISA isn't valid.  */

static const struct mips_cpu_info *
mips_cpu_info_from_isa (int isa)
{
  unsigned int i;

  for (i = 0; i < ARRAY_SIZE (mips_cpu_info_table); i++)
    if (mips_cpu_info_table[i].isa == isa)
      return mips_cpu_info_table + i;

  return NULL;
}

/* Return true if GIVEN is the same as CANONICAL, or if it is CANONICAL
   with a final "000" replaced by "k".  Ignore case.

   Note: this function is shared between GCC and GAS.  */

static bool
mips_strict_matching_cpu_name_p (const char *canonical, const char *given)
{
  while (*given != 0 && TOLOWER (*given) == TOLOWER (*canonical))
    given++, canonical++;

  return ((*given == 0 && *canonical == 0)
	  || (strcmp (canonical, "000") == 0 && strcasecmp (given, "k") == 0));
}

/* Return true if GIVEN matches CANONICAL, where GIVEN is a user-supplied
   CPU name.  We've traditionally allowed a lot of variation here.

   Note: this function is shared between GCC and GAS.  */

static bool
mips_matching_cpu_name_p (const char *canonical, const char *given)
{
  /* First see if the name matches exactly, or with a final "000"
     turned into "k".  */
  if (mips_strict_matching_cpu_name_p (canonical, given))
    return true;

  /* If not, try comparing based on numerical designation alone.
     See if GIVEN is an unadorned number, or 'r' followed by a number.  */
  if (TOLOWER (*given) == 'r')
    given++;
  if (!ISDIGIT (*given))
    return false;

  /* Skip over some well-known prefixes in the canonical name,
     hoping to find a number there too.  */
  if (TOLOWER (canonical[0]) == 'v' && TOLOWER (canonical[1]) == 'r')
    canonical += 2;
  else if (TOLOWER (canonical[0]) == 'r' && TOLOWER (canonical[1]) == 'm')
    canonical += 2;
  else if (TOLOWER (canonical[0]) == 'r')
    canonical += 1;

  return mips_strict_matching_cpu_name_p (canonical, given);
}

/* Return the mips_cpu_info entry for the processor or ISA given
   by CPU_STRING.  Return null if the string isn't recognized.

   A similar function exists in GAS.  */

static const struct mips_cpu_info *
mips_parse_cpu (const char *cpu_string)
{
  unsigned int i;
  const char *s;

  /* In the past, we allowed upper-case CPU names, but it doesn't
     work well with the multilib machinery.  */
  for (s = cpu_string; *s != 0; s++)
    if (ISUPPER (*s))
      {
	warning (0, "CPU names must be lower case");
	break;
      }

  /* 'from-abi' selects the most compatible architecture for the given
     ABI: MIPS I for 32-bit ABIs and MIPS III for 64-bit ABIs.  For the
     EABIs, we have to decide whether we're using the 32-bit or 64-bit
     version.  */
  if (strcasecmp (cpu_string, "from-abi") == 0)
    return mips_cpu_info_from_isa (ABI_NEEDS_32BIT_REGS ? 1
				   : ABI_NEEDS_64BIT_REGS ? 3
				   : (TARGET_64BIT ? 3 : 1));

  /* 'default' has traditionally been a no-op.  Probably not very useful.  */
  if (strcasecmp (cpu_string, "default") == 0)
    return NULL;

  for (i = 0; i < ARRAY_SIZE (mips_cpu_info_table); i++)
    if (mips_matching_cpu_name_p (mips_cpu_info_table[i].name, cpu_string))
      return mips_cpu_info_table + i;

  return NULL;
}

/* Set up globals to generate code for the ISA or processor
   described by INFO.  */

static void
mips_set_architecture (const struct mips_cpu_info *info)
{
  if (info != 0)
    {
      mips_arch_info = info;
      mips_arch = info->cpu;
      mips_isa = info->isa;
    }
}

/* Likewise for tuning.  */

static void
mips_set_tune (const struct mips_cpu_info *info)
{
  if (info != 0)
    {
      mips_tune_info = info;
      mips_tune = info->cpu;
    }
}

/* Implement TARGET_HANDLE_OPTION.  */

static bool
mips_handle_option (size_t code, const char *arg, int value ATTRIBUTE_UNUSED)
{
  switch (code)
    {
    case OPT_mabi_:
      if (strcmp (arg, "32") == 0)
	mips_abi = ABI_32;
      else if (strcmp (arg, "o64") == 0)
	mips_abi = ABI_O64;
      else if (strcmp (arg, "n32") == 0)
	mips_abi = ABI_N32;
      else if (strcmp (arg, "64") == 0)
	mips_abi = ABI_64;
      else if (strcmp (arg, "eabi") == 0)
	mips_abi = ABI_EABI;
      else
	return false;
      return true;

    case OPT_march_:
    case OPT_mtune_:
      return mips_parse_cpu (arg) != 0;

    case OPT_mips:
      mips_isa_option_info = mips_parse_cpu (ACONCAT (("mips", arg, NULL)));
      return mips_isa_option_info != 0;

    case OPT_mno_flush_func:
      mips_cache_flush_func = NULL;
      return true;

    case OPT_mcode_readable_:
      if (strcmp (arg, "yes") == 0)
	mips_code_readable = CODE_READABLE_YES;
      else if (strcmp (arg, "pcrel") == 0)
	mips_code_readable = CODE_READABLE_PCREL;
      else if (strcmp (arg, "no") == 0)
	mips_code_readable = CODE_READABLE_NO;
      else
	return false;
      return true;

    case OPT_mr10k_cache_barrier_:
      if (strcmp (arg, "load-store") == 0)
	mips_r10k_cache_barrier = R10K_CACHE_BARRIER_LOAD_STORE;
      else if (strcmp (arg, "store") == 0)
	mips_r10k_cache_barrier = R10K_CACHE_BARRIER_STORE;
      else if (strcmp (arg, "none") == 0)
	mips_r10k_cache_barrier = R10K_CACHE_BARRIER_NONE;
      else
	return false;
      return true;

    default:
      return true;
    }
}

/* Implement TARGET_OPTION_OVERRIDE.  */

static void
mips_option_override (void)
{
  int i, start, regno, mode;

  /* Process flags as though we were generating non-MIPS16 code.  */
  mips_base_mips16 = TARGET_MIPS16;
  target_flags &= ~MASK_MIPS16;

#ifdef SUBTARGET_OVERRIDE_OPTIONS
  SUBTARGET_OVERRIDE_OPTIONS;
#endif

  /* -mno-float overrides -mhard-float and -msoft-float.  */
  if (TARGET_NO_FLOAT)
    {
      target_flags |= MASK_SOFT_FLOAT_ABI;
      target_flags_explicit |= MASK_SOFT_FLOAT_ABI;
    }

  if (TARGET_FLIP_MIPS16)
    TARGET_INTERLINK_MIPS16 = 1;

  /* Set the small data limit.  */
  mips_small_data_threshold = (global_options_set.x_g_switch_value
			       ? g_switch_value
			       : MIPS_DEFAULT_GVALUE);

  /* The following code determines the architecture and register size.
     Similar code was added to GAS 2.14 (see tc-mips.c:md_after_parse_args()).
     The GAS and GCC code should be kept in sync as much as possible.  */

  if (mips_arch_string != 0)
    mips_set_architecture (mips_parse_cpu (mips_arch_string));

  if (mips_isa_option_info != 0)
    {
      if (mips_arch_info == 0)
	mips_set_architecture (mips_isa_option_info);
      else if (mips_arch_info->isa != mips_isa_option_info->isa)
	error ("%<-%s%> conflicts with the other architecture options, "
	       "which specify a %s processor",
	       mips_isa_option_info->name,
	       mips_cpu_info_from_isa (mips_arch_info->isa)->name);
    }

  if (mips_arch_info == 0)
    {
#ifdef MIPS_CPU_STRING_DEFAULT
      mips_set_architecture (mips_parse_cpu (MIPS_CPU_STRING_DEFAULT));
#else
      mips_set_architecture (mips_cpu_info_from_isa (MIPS_ISA_DEFAULT));
#endif
    }

  if (ABI_NEEDS_64BIT_REGS && !ISA_HAS_64BIT_REGS)
    error ("%<-march=%s%> is not compatible with the selected ABI",
	   mips_arch_info->name);

  /* Optimize for mips_arch, unless -mtune selects a different processor.  */
  if (mips_tune_string != 0)
    mips_set_tune (mips_parse_cpu (mips_tune_string));

  if (mips_tune_info == 0)
    mips_set_tune (mips_arch_info);

  if ((target_flags_explicit & MASK_64BIT) != 0)
    {
      /* The user specified the size of the integer registers.  Make sure
	 it agrees with the ABI and ISA.  */
      if (TARGET_64BIT && !ISA_HAS_64BIT_REGS)
	error ("%<-mgp64%> used with a 32-bit processor");
      else if (!TARGET_64BIT && ABI_NEEDS_64BIT_REGS)
	error ("%<-mgp32%> used with a 64-bit ABI");
      else if (TARGET_64BIT && ABI_NEEDS_32BIT_REGS)
	error ("%<-mgp64%> used with a 32-bit ABI");
    }
  else
    {
      /* Infer the integer register size from the ABI and processor.
	 Restrict ourselves to 32-bit registers if that's all the
	 processor has, or if the ABI cannot handle 64-bit registers.  */
      if (ABI_NEEDS_32BIT_REGS || !ISA_HAS_64BIT_REGS)
	target_flags &= ~MASK_64BIT;
      else
	target_flags |= MASK_64BIT;
    }

  if ((target_flags_explicit & MASK_FLOAT64) != 0)
    {
      if (TARGET_SINGLE_FLOAT && TARGET_FLOAT64)
	error ("unsupported combination: %s", "-mfp64 -msingle-float");
      else if (TARGET_64BIT && TARGET_DOUBLE_FLOAT && !TARGET_FLOAT64)
	error ("unsupported combination: %s", "-mgp64 -mfp32 -mdouble-float");
      else if (!TARGET_64BIT && TARGET_FLOAT64)
	{
	  if (!ISA_HAS_MXHC1)
	    error ("%<-mgp32%> and %<-mfp64%> can only be combined if"
		   " the target supports the mfhc1 and mthc1 instructions");
	  else if (mips_abi != ABI_32)
	    error ("%<-mgp32%> and %<-mfp64%> can only be combined when using"
		   " the o32 ABI");
	}
    }
  else
    {
      /* -msingle-float selects 32-bit float registers.  Otherwise the
	 float registers should be the same size as the integer ones.  */
      if (TARGET_64BIT && TARGET_DOUBLE_FLOAT)
	target_flags |= MASK_FLOAT64;
      else
	target_flags &= ~MASK_FLOAT64;
    }

  /* End of code shared with GAS.  */

  /* If no -mlong* option was given, infer it from the other options.  */
  if ((target_flags_explicit & MASK_LONG64) == 0)
    {
      if ((mips_abi == ABI_EABI && TARGET_64BIT) || mips_abi == ABI_64)
	target_flags |= MASK_LONG64;
      else
	target_flags &= ~MASK_LONG64;
    }

  if (!TARGET_OLDABI)
    flag_pcc_struct_return = 0;

  /* Decide which rtx_costs structure to use.  */
  if (optimize_size)
    mips_cost = &mips_rtx_cost_optimize_size;
  else
    mips_cost = &mips_rtx_cost_data[mips_tune];

  /* If the user hasn't specified a branch cost, use the processor's
     default.  */
  if (mips_branch_cost == 0)
    mips_branch_cost = mips_cost->branch_cost;

  /* If neither -mbranch-likely nor -mno-branch-likely was given
     on the command line, set MASK_BRANCHLIKELY based on the target
     architecture and tuning flags.  Annulled delay slots are a
     size win, so we only consider the processor-specific tuning
     for !optimize_size.  */
  if ((target_flags_explicit & MASK_BRANCHLIKELY) == 0)
    {
      if (ISA_HAS_BRANCHLIKELY
	  && (optimize_size
	      || (mips_tune_info->tune_flags & PTF_AVOID_BRANCHLIKELY) == 0))
	target_flags |= MASK_BRANCHLIKELY;
      else
	target_flags &= ~MASK_BRANCHLIKELY;
    }
  else if (TARGET_BRANCHLIKELY && !ISA_HAS_BRANCHLIKELY)
    warning (0, "the %qs architecture does not support branch-likely"
	     " instructions", mips_arch_info->name);

  /* The effect of -mabicalls isn't defined for the EABI.  */
  if (mips_abi == ABI_EABI && TARGET_ABICALLS)
    {
      error ("unsupported combination: %s", "-mabicalls -mabi=eabi");
      target_flags &= ~MASK_ABICALLS;
    }

  if (TARGET_ABICALLS_PIC2)
    /* We need to set flag_pic for executables as well as DSOs
       because we may reference symbols that are not defined in
       the final executable.  (MIPS does not use things like
       copy relocs, for example.)

       There is a body of code that uses __PIC__ to distinguish
       between -mabicalls and -mno-abicalls code.  The non-__PIC__
       variant is usually appropriate for TARGET_ABICALLS_PIC0, as
       long as any indirect jumps use $25.  */
    flag_pic = 1;

  /* -mvr4130-align is a "speed over size" optimization: it usually produces
     faster code, but at the expense of more nops.  Enable it at -O3 and
     above.  */
  if (optimize > 2 && (target_flags_explicit & MASK_VR4130_ALIGN) == 0)
    target_flags |= MASK_VR4130_ALIGN;

  /* Prefer a call to memcpy over inline code when optimizing for size,
     though see MOVE_RATIO in mips.h.  */
  if (optimize_size && (target_flags_explicit & MASK_MEMCPY) == 0)
    target_flags |= MASK_MEMCPY;

  /* If we have a nonzero small-data limit, check that the -mgpopt
     setting is consistent with the other target flags.  */
  if (mips_small_data_threshold > 0)
    {
      if (!TARGET_GPOPT)
	{
	  if (!TARGET_EXPLICIT_RELOCS)
	    error ("%<-mno-gpopt%> needs %<-mexplicit-relocs%>");

	  TARGET_LOCAL_SDATA = false;
	  TARGET_EXTERN_SDATA = false;
	}
      else
	{
	  if (TARGET_VXWORKS_RTP)
	    warning (0, "cannot use small-data accesses for %qs", "-mrtp");

	  if (TARGET_ABICALLS)
	    warning (0, "cannot use small-data accesses for %qs",
		     "-mabicalls");
	}
    }

#ifdef MIPS_TFMODE_FORMAT
  REAL_MODE_FORMAT (TFmode) = &MIPS_TFMODE_FORMAT;
#endif

  /* Make sure that the user didn't turn off paired single support when
     MIPS-3D support is requested.  */
  if (TARGET_MIPS3D
      && (target_flags_explicit & MASK_PAIRED_SINGLE_FLOAT)
      && !TARGET_PAIRED_SINGLE_FLOAT)
    error ("%<-mips3d%> requires %<-mpaired-single%>");

  /* If TARGET_MIPS3D, enable MASK_PAIRED_SINGLE_FLOAT.  */
  if (TARGET_MIPS3D)
    target_flags |= MASK_PAIRED_SINGLE_FLOAT;

  /* Make sure that when TARGET_PAIRED_SINGLE_FLOAT is true, TARGET_FLOAT64
     and TARGET_HARD_FLOAT_ABI are both true.  */
  if (TARGET_PAIRED_SINGLE_FLOAT && !(TARGET_FLOAT64 && TARGET_HARD_FLOAT_ABI))
    error ("%qs must be used with %qs",
	   TARGET_MIPS3D ? "-mips3d" : "-mpaired-single",
	   TARGET_HARD_FLOAT_ABI ? "-mfp64" : "-mhard-float");

  /* Make sure that the ISA supports TARGET_PAIRED_SINGLE_FLOAT when it is
     enabled.  */
  if (TARGET_PAIRED_SINGLE_FLOAT && !ISA_HAS_PAIRED_SINGLE)
    warning (0, "the %qs architecture does not support paired-single"
	     " instructions", mips_arch_info->name);

  if (mips_r10k_cache_barrier != R10K_CACHE_BARRIER_NONE
      && !TARGET_CACHE_BUILTIN)
    {
      error ("%qs requires a target that provides the %qs instruction",
	     "-mr10k-cache-barrier", "cache");
      mips_r10k_cache_barrier = R10K_CACHE_BARRIER_NONE;
    }

  /* If TARGET_DSPR2, enable MASK_DSP.  */
  if (TARGET_DSPR2)
    target_flags |= MASK_DSP;

  /* .eh_frame addresses should be the same width as a C pointer.
     Most MIPS ABIs support only one pointer size, so the assembler
     will usually know exactly how big an .eh_frame address is.

     Unfortunately, this is not true of the 64-bit EABI.  The ABI was
     originally defined to use 64-bit pointers (i.e. it is LP64), and
     this is still the default mode.  However, we also support an n32-like
     ILP32 mode, which is selected by -mlong32.  The problem is that the
     assembler has traditionally not had an -mlong option, so it has
     traditionally not known whether we're using the ILP32 or LP64 form.

     As it happens, gas versions up to and including 2.19 use _32-bit_
     addresses for EABI64 .cfi_* directives.  This is wrong for the
     default LP64 mode, so we can't use the directives by default.
     Moreover, since gas's current behavior is at odds with gcc's
     default behavior, it seems unwise to rely on future versions
     of gas behaving the same way.  We therefore avoid using .cfi
     directives for -mlong32 as well.  */
  if (mips_abi == ABI_EABI && TARGET_64BIT)
    flag_dwarf2_cfi_asm = 0;

  /* .cfi_* directives generate a read-only section, so fall back on
     manual .eh_frame creation if we need the section to be writable.  */
  if (TARGET_WRITABLE_EH_FRAME)
    flag_dwarf2_cfi_asm = 0;

  mips_init_print_operand_punct ();

  /* Set up array to map GCC register number to debug register number.
     Ignore the special purpose register numbers.  */

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      mips_dbx_regno[i] = INVALID_REGNUM;
      if (GP_REG_P (i) || FP_REG_P (i) || ALL_COP_REG_P (i))
	mips_dwarf_regno[i] = i;
      else
	mips_dwarf_regno[i] = INVALID_REGNUM;
    }

  start = GP_DBX_FIRST - GP_REG_FIRST;
  for (i = GP_REG_FIRST; i <= GP_REG_LAST; i++)
    mips_dbx_regno[i] = i + start;

  start = FP_DBX_FIRST - FP_REG_FIRST;
  for (i = FP_REG_FIRST; i <= FP_REG_LAST; i++)
    mips_dbx_regno[i] = i + start;

  /* Accumulator debug registers use big-endian ordering.  */
  mips_dbx_regno[HI_REGNUM] = MD_DBX_FIRST + 0;
  mips_dbx_regno[LO_REGNUM] = MD_DBX_FIRST + 1;
  mips_dwarf_regno[HI_REGNUM] = MD_REG_FIRST + 0;
  mips_dwarf_regno[LO_REGNUM] = MD_REG_FIRST + 1;
  for (i = DSP_ACC_REG_FIRST; i <= DSP_ACC_REG_LAST; i += 2)
    {
      mips_dwarf_regno[i + TARGET_LITTLE_ENDIAN] = i;
      mips_dwarf_regno[i + TARGET_BIG_ENDIAN] = i + 1;
    }

  /* Set up mips_hard_regno_mode_ok.  */
  for (mode = 0; mode < MAX_MACHINE_MODE; mode++)
    for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
      mips_hard_regno_mode_ok[mode][regno]
	= mips_hard_regno_mode_ok_p (regno, (enum machine_mode) mode);

  /* Function to allocate machine-dependent function status.  */
  init_machine_status = &mips_init_machine_status;

  /* Default to working around R4000 errata only if the processor
     was selected explicitly.  */
  if ((target_flags_explicit & MASK_FIX_R4000) == 0
      && mips_matching_cpu_name_p (mips_arch_info->name, "r4000"))
    target_flags |= MASK_FIX_R4000;

  /* Default to working around R4400 errata only if the processor
     was selected explicitly.  */
  if ((target_flags_explicit & MASK_FIX_R4400) == 0
      && mips_matching_cpu_name_p (mips_arch_info->name, "r4400"))
    target_flags |= MASK_FIX_R4400;

  /* Default to working around R10000 errata only if the processor
     was selected explicitly.  */
  if ((target_flags_explicit & MASK_FIX_R10000) == 0
      && mips_matching_cpu_name_p (mips_arch_info->name, "r10000"))
    target_flags |= MASK_FIX_R10000;

  /* Make sure that branch-likely instructions available when using
     -mfix-r10000.  The instructions are not available if either:

	1. -mno-branch-likely was passed.
	2. The selected ISA does not support branch-likely and
	   the command line does not include -mbranch-likely.  */
  if (TARGET_FIX_R10000
      && ((target_flags_explicit & MASK_BRANCHLIKELY) == 0
          ? !ISA_HAS_BRANCHLIKELY
          : !TARGET_BRANCHLIKELY))
    sorry ("%qs requires branch-likely instructions", "-mfix-r10000");

  if (TARGET_SYNCI && !ISA_HAS_SYNCI)
    {
      warning (0, "the %qs architecture does not support the synci "
	       "instruction", mips_arch_info->name);
      target_flags &= ~MASK_SYNCI;
    }

  /* Only optimize PIC indirect calls if they are actually required.  */
  if (!TARGET_USE_GOT || !TARGET_EXPLICIT_RELOCS)
    target_flags &= ~MASK_RELAX_PIC_CALLS;

  /* Save base state of options.  */
  mips_base_target_flags = target_flags;
  mips_base_schedule_insns = flag_schedule_insns;
  mips_base_reorder_blocks_and_partition = flag_reorder_blocks_and_partition;
  mips_base_move_loop_invariants = flag_move_loop_invariants;
  mips_base_align_loops = align_loops;
  mips_base_align_jumps = align_jumps;
  mips_base_align_functions = align_functions;

  /* Now select the ISA mode.

     Do all CPP-sensitive stuff in non-MIPS16 mode; we'll switch to
     MIPS16 mode afterwards if need be.  */
  mips_set_mips16_mode (false);
}

/* Implement TARGET_OPTION_OPTIMIZATION_TABLE.  */
static const struct default_options mips_option_optimization_table[] =
  {
    { OPT_LEVELS_1_PLUS, OPT_fomit_frame_pointer, NULL, 1 },
    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

/* Swap the register information for registers I and I + 1, which
   currently have the wrong endianness.  Note that the registers'
   fixedness and call-clobberedness might have been set on the
   command line.  */

static void
mips_swap_registers (unsigned int i)
{
  int tmpi;
  const char *tmps;

#define SWAP_INT(X, Y) (tmpi = (X), (X) = (Y), (Y) = tmpi)
#define SWAP_STRING(X, Y) (tmps = (X), (X) = (Y), (Y) = tmps)

  SWAP_INT (fixed_regs[i], fixed_regs[i + 1]);
  SWAP_INT (call_used_regs[i], call_used_regs[i + 1]);
  SWAP_INT (call_really_used_regs[i], call_really_used_regs[i + 1]);
  SWAP_STRING (reg_names[i], reg_names[i + 1]);

#undef SWAP_STRING
#undef SWAP_INT
}

/* Implement TARGET_CONDITIONAL_REGISTER_USAGE.  */

static void
mips_conditional_register_usage (void)
{

  if (ISA_HAS_DSP)
    {
      /* These DSP control register fields are global.  */
      global_regs[CCDSP_PO_REGNUM] = 1;
      global_regs[CCDSP_SC_REGNUM] = 1;
    }
  else 
    {
      int regno;

      for (regno = DSP_ACC_REG_FIRST; regno <= DSP_ACC_REG_LAST; regno++)
	fixed_regs[regno] = call_used_regs[regno] = 1;
    }
  if (!TARGET_HARD_FLOAT)
    {
      int regno;

      for (regno = FP_REG_FIRST; regno <= FP_REG_LAST; regno++)
	fixed_regs[regno] = call_used_regs[regno] = 1;
      for (regno = ST_REG_FIRST; regno <= ST_REG_LAST; regno++)
	fixed_regs[regno] = call_used_regs[regno] = 1;
    }
  else if (! ISA_HAS_8CC)
    {
      int regno;

      /* We only have a single condition-code register.  We implement
	 this by fixing all the condition-code registers and generating
	 RTL that refers directly to ST_REG_FIRST.  */
      for (regno = ST_REG_FIRST; regno <= ST_REG_LAST; regno++)
	fixed_regs[regno] = call_used_regs[regno] = 1;
    }
  /* In MIPS16 mode, we permit the $t temporary registers to be used
     for reload.  We prohibit the unused $s registers, since they
     are call-saved, and saving them via a MIPS16 register would
     probably waste more time than just reloading the value.  */
  if (TARGET_MIPS16)
    {
      fixed_regs[18] = call_used_regs[18] = 1;
      fixed_regs[19] = call_used_regs[19] = 1;
      fixed_regs[20] = call_used_regs[20] = 1;
      fixed_regs[21] = call_used_regs[21] = 1;
      fixed_regs[22] = call_used_regs[22] = 1;
      fixed_regs[23] = call_used_regs[23] = 1;
      fixed_regs[26] = call_used_regs[26] = 1;
      fixed_regs[27] = call_used_regs[27] = 1;
      fixed_regs[30] = call_used_regs[30] = 1;
    }
  /* $f20-$f23 are call-clobbered for n64.  */
  if (mips_abi == ABI_64)
    {
      int regno;
      for (regno = FP_REG_FIRST + 20; regno < FP_REG_FIRST + 24; regno++)
	call_really_used_regs[regno] = call_used_regs[regno] = 1;
    }
  /* Odd registers in the range $f21-$f31 (inclusive) are call-clobbered
     for n32.  */
  if (mips_abi == ABI_N32)
    {
      int regno;
      for (regno = FP_REG_FIRST + 21; regno <= FP_REG_FIRST + 31; regno+=2)
	call_really_used_regs[regno] = call_used_regs[regno] = 1;
    }
  /* Make sure that double-register accumulator values are correctly
     ordered for the current endianness.  */
  if (TARGET_LITTLE_ENDIAN)
    {
      unsigned int regno;

      mips_swap_registers (MD_REG_FIRST);
      for (regno = DSP_ACC_REG_FIRST; regno <= DSP_ACC_REG_LAST; regno += 2)
	mips_swap_registers (regno);
    }
}

/* Initialize vector TARGET to VALS.  */

void
mips_expand_vector_init (rtx target, rtx vals)
{
  enum machine_mode mode;
  enum machine_mode inner;
  unsigned int i, n_elts;
  rtx mem;

  mode = GET_MODE (target);
  inner = GET_MODE_INNER (mode);
  n_elts = GET_MODE_NUNITS (mode);

  gcc_assert (VECTOR_MODE_P (mode));

  mem = assign_stack_temp (mode, GET_MODE_SIZE (mode), 0);
  for (i = 0; i < n_elts; i++)
    emit_move_insn (adjust_address_nv (mem, inner, i * GET_MODE_SIZE (inner)),
                    XVECEXP (vals, 0, i));

  emit_move_insn (target, mem);
}

/* When generating MIPS16 code, we want to allocate $24 (T_REG) before
   other registers for instructions for which it is possible.  This
   encourages the compiler to use CMP in cases where an XOR would
   require some register shuffling.  */

void
mips_order_regs_for_local_alloc (void)
{
  int i;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    reg_alloc_order[i] = i;

  if (TARGET_MIPS16)
    {
      /* It really doesn't matter where we put register 0, since it is
         a fixed register anyhow.  */
      reg_alloc_order[0] = 24;
      reg_alloc_order[24] = 0;
    }
}

/* Implement EH_USES.  */

bool
mips_eh_uses (unsigned int regno)
{
  if (reload_completed && !TARGET_ABSOLUTE_JUMPS)
    {
      /* We need to force certain registers to be live in order to handle
	 PIC long branches correctly.  See mips_must_initialize_gp_p for
	 details.  */
      if (mips_cfun_has_cprestore_slot_p ())
	{
	  if (regno == CPRESTORE_SLOT_REGNUM)
	    return true;
	}
      else
	{
	  if (cfun->machine->global_pointer == regno)
	    return true;
	}
    }

  return false;
}

/* Implement EPILOGUE_USES.  */

bool
mips_epilogue_uses (unsigned int regno)
{
  /* Say that the epilogue uses the return address register.  Note that
     in the case of sibcalls, the values "used by the epilogue" are
     considered live at the start of the called function.  */
  if (regno == RETURN_ADDR_REGNUM)
    return true;

  /* If using a GOT, say that the epilogue also uses GOT_VERSION_REGNUM.
     See the comment above load_call<mode> for details.  */
  if (TARGET_USE_GOT && (regno) == GOT_VERSION_REGNUM)
    return true;

  /* An interrupt handler must preserve some registers that are
     ordinarily call-clobbered.  */
  if (cfun->machine->interrupt_handler_p
      && mips_interrupt_extra_call_saved_reg_p (regno))
    return true;

  return false;
}

/* A for_each_rtx callback.  Stop the search if *X is an AT register.  */

static int
mips_at_reg_p (rtx *x, void *data ATTRIBUTE_UNUSED)
{
  return REG_P (*x) && REGNO (*x) == AT_REGNUM;
}

/* Return true if INSN needs to be wrapped in ".set noat".
   INSN has NOPERANDS operands, stored in OPVEC.  */

static bool
mips_need_noat_wrapper_p (rtx insn, rtx *opvec, int noperands)
{
  int i;

  if (recog_memoized (insn) >= 0)
    for (i = 0; i < noperands; i++)
      if (for_each_rtx (&opvec[i], mips_at_reg_p, NULL))
	return true;
  return false;
}

/* Implement FINAL_PRESCAN_INSN.  */

void
mips_final_prescan_insn (rtx insn, rtx *opvec, int noperands)
{
  if (mips_need_noat_wrapper_p (insn, opvec, noperands))
    mips_push_asm_switch (&mips_noat);
}

/* Implement TARGET_ASM_FINAL_POSTSCAN_INSN.  */

static void
mips_final_postscan_insn (FILE *file ATTRIBUTE_UNUSED, rtx insn,
			  rtx *opvec, int noperands)
{
  if (mips_need_noat_wrapper_p (insn, opvec, noperands))
    mips_pop_asm_switch (&mips_noat);
}

/* Return the function that is used to expand the <u>mulsidi3 pattern.
   EXT_CODE is the code of the extension used.  Return NULL if widening
   multiplication shouldn't be used.  */

mulsidi3_gen_fn
mips_mulsidi3_gen_fn (enum rtx_code ext_code)
{
  bool signed_p;

  signed_p = ext_code == SIGN_EXTEND;
  if (TARGET_64BIT)
    {
      /* Don't use widening multiplication with MULT when we have DMUL.  Even
	 with the extension of its input operands DMUL is faster.  Note that
	 the extension is not needed for signed multiplication.  In order to
	 ensure that we always remove the redundant sign-extension in this
	 case we still expand mulsidi3 for DMUL.  */
      if (ISA_HAS_DMUL3)
	return signed_p ? gen_mulsidi3_64bit_dmul : NULL;
      if (TARGET_FIX_R4000)
	return NULL;
      return signed_p ? gen_mulsidi3_64bit : gen_umulsidi3_64bit;
    }
  else
    {
      if (TARGET_FIX_R4000 && !ISA_HAS_DSP)
	return signed_p ? gen_mulsidi3_32bit_r4000 : gen_umulsidi3_32bit_r4000;
      return signed_p ? gen_mulsidi3_32bit : gen_umulsidi3_32bit;
    }
}

/* Return the size in bytes of the trampoline code, padded to
   TRAMPOLINE_ALIGNMENT bits.  The static chain pointer and target
   function address immediately follow.  */

int
mips_trampoline_code_size (void)
{
  if (TARGET_USE_PIC_FN_ADDR_REG)
    return 4 * 4;
  else if (ptr_mode == DImode)
    return 8 * 4;
  else if (ISA_HAS_LOAD_DELAY)
    return 6 * 4;
  else
    return 4 * 4;
}

/* Implement TARGET_TRAMPOLINE_INIT.  */

static void
mips_trampoline_init (rtx m_tramp, tree fndecl, rtx chain_value)
{
  rtx addr, end_addr, high, low, opcode, mem;
  rtx trampoline[8];
  unsigned int i, j;
  HOST_WIDE_INT end_addr_offset, static_chain_offset, target_function_offset;

  /* Work out the offsets of the pointers from the start of the
     trampoline code.  */
  end_addr_offset = mips_trampoline_code_size ();
  static_chain_offset = end_addr_offset;
  target_function_offset = static_chain_offset + GET_MODE_SIZE (ptr_mode);

  /* Get pointers to the beginning and end of the code block.  */
  addr = force_reg (Pmode, XEXP (m_tramp, 0));
  end_addr = mips_force_binary (Pmode, PLUS, addr, GEN_INT (end_addr_offset));

#define OP(X) gen_int_mode (X, SImode)

  /* Build up the code in TRAMPOLINE.  */
  i = 0;
  if (TARGET_USE_PIC_FN_ADDR_REG)
    {
      /* $25 contains the address of the trampoline.  Emit code of the form:

	     l[wd]    $1, target_function_offset($25)
	     l[wd]    $static_chain, static_chain_offset($25)
	     jr       $1
	     move     $25,$1.  */
      trampoline[i++] = OP (MIPS_LOAD_PTR (AT_REGNUM,
					   target_function_offset,
					   PIC_FUNCTION_ADDR_REGNUM));
      trampoline[i++] = OP (MIPS_LOAD_PTR (STATIC_CHAIN_REGNUM,
					   static_chain_offset,
					   PIC_FUNCTION_ADDR_REGNUM));
      trampoline[i++] = OP (MIPS_JR (AT_REGNUM));
      trampoline[i++] = OP (MIPS_MOVE (PIC_FUNCTION_ADDR_REGNUM, AT_REGNUM));
    }
  else if (ptr_mode == DImode)
    {
      /* It's too cumbersome to create the full 64-bit address, so let's
	 instead use:

	     move    $1, $31
	     bal     1f
	     nop
	 1:  l[wd]   $25, target_function_offset - 12($31)
	     l[wd]   $static_chain, static_chain_offset - 12($31)
	     jr      $25
	     move    $31, $1

	where 12 is the offset of "1:" from the start of the code block.  */
      trampoline[i++] = OP (MIPS_MOVE (AT_REGNUM, RETURN_ADDR_REGNUM));
      trampoline[i++] = OP (MIPS_BAL (1));
      trampoline[i++] = OP (MIPS_NOP);
      trampoline[i++] = OP (MIPS_LOAD_PTR (PIC_FUNCTION_ADDR_REGNUM,
					   target_function_offset - 12,
					   RETURN_ADDR_REGNUM));
      trampoline[i++] = OP (MIPS_LOAD_PTR (STATIC_CHAIN_REGNUM,
					   static_chain_offset - 12,
					   RETURN_ADDR_REGNUM));
      trampoline[i++] = OP (MIPS_JR (PIC_FUNCTION_ADDR_REGNUM));
      trampoline[i++] = OP (MIPS_MOVE (RETURN_ADDR_REGNUM, AT_REGNUM));
    }
  else
    {
      /* If the target has load delays, emit:

	     lui     $1, %hi(end_addr)
	     lw      $25, %lo(end_addr + ...)($1)
	     lw      $static_chain, %lo(end_addr + ...)($1)
	     jr      $25
	     nop

	 Otherwise emit:

	     lui     $1, %hi(end_addr)
	     lw      $25, %lo(end_addr + ...)($1)
	     jr      $25
	     lw      $static_chain, %lo(end_addr + ...)($1).  */

      /* Split END_ADDR into %hi and %lo values.  Trampolines are aligned
	 to 64 bits, so the %lo value will have the bottom 3 bits clear.  */
      high = expand_simple_binop (SImode, PLUS, end_addr, GEN_INT (0x8000),
				  NULL, false, OPTAB_WIDEN);
      high = expand_simple_binop (SImode, LSHIFTRT, high, GEN_INT (16),
				  NULL, false, OPTAB_WIDEN);
      low = convert_to_mode (SImode, gen_lowpart (HImode, end_addr), true);

      /* Emit the LUI.  */
      opcode = OP (MIPS_LUI (AT_REGNUM, 0));
      trampoline[i++] = expand_simple_binop (SImode, IOR, opcode, high,
					     NULL, false, OPTAB_WIDEN);

      /* Emit the load of the target function.  */
      opcode = OP (MIPS_LOAD_PTR (PIC_FUNCTION_ADDR_REGNUM,
				  target_function_offset - end_addr_offset,
				  AT_REGNUM));
      trampoline[i++] = expand_simple_binop (SImode, IOR, opcode, low,
					     NULL, false, OPTAB_WIDEN);

      /* Emit the JR here, if we can.  */
      if (!ISA_HAS_LOAD_DELAY)
	trampoline[i++] = OP (MIPS_JR (PIC_FUNCTION_ADDR_REGNUM));

      /* Emit the load of the static chain register.  */
      opcode = OP (MIPS_LOAD_PTR (STATIC_CHAIN_REGNUM,
				  static_chain_offset - end_addr_offset,
				  AT_REGNUM));
      trampoline[i++] = expand_simple_binop (SImode, IOR, opcode, low,
					     NULL, false, OPTAB_WIDEN);

      /* Emit the JR, if we couldn't above.  */
      if (ISA_HAS_LOAD_DELAY)
	{
	  trampoline[i++] = OP (MIPS_JR (PIC_FUNCTION_ADDR_REGNUM));
	  trampoline[i++] = OP (MIPS_NOP);
	}
    }

#undef OP

  /* Copy the trampoline code.  Leave any padding uninitialized.  */
  for (j = 0; j < i; j++)
    {
      mem = adjust_address (m_tramp, SImode, j * GET_MODE_SIZE (SImode));
      mips_emit_move (mem, trampoline[j]);
    }

  /* Set up the static chain pointer field.  */
  mem = adjust_address (m_tramp, ptr_mode, static_chain_offset);
  mips_emit_move (mem, chain_value);

  /* Set up the target function field.  */
  mem = adjust_address (m_tramp, ptr_mode, target_function_offset);
  mips_emit_move (mem, XEXP (DECL_RTL (fndecl), 0));

  /* Flush the code part of the trampoline.  */
  emit_insn (gen_add3_insn (end_addr, addr, GEN_INT (TRAMPOLINE_SIZE)));
  emit_insn (gen_clear_cache (addr, end_addr));
}

/* Implement FUNCTION_PROFILER.  */

void mips_function_profiler (FILE *file)
{
  if (TARGET_MIPS16)
    sorry ("mips16 function profiling");
  if (TARGET_LONG_CALLS)
    {
      /* For TARGET_LONG_CALLS use $3 for the address of _mcount.  */
      if (Pmode == DImode)
	fprintf (file, "\tdla\t%s,_mcount\n", reg_names[3]);
      else
	fprintf (file, "\tla\t%s,_mcount\n", reg_names[3]);
    }
  mips_push_asm_switch (&mips_noat);
  fprintf (file, "\tmove\t%s,%s\t\t# save current return address\n",
	   reg_names[AT_REGNUM], reg_names[RETURN_ADDR_REGNUM]);
  /* _mcount treats $2 as the static chain register.  */
  if (cfun->static_chain_decl != NULL)
    fprintf (file, "\tmove\t%s,%s\n", reg_names[2],
	     reg_names[STATIC_CHAIN_REGNUM]);
  if (TARGET_MCOUNT_RA_ADDRESS)
    {
      /* If TARGET_MCOUNT_RA_ADDRESS load $12 with the address of the
	 ra save location.  */
      if (cfun->machine->frame.ra_fp_offset == 0)
	/* ra not saved, pass zero.  */
	fprintf (file, "\tmove\t%s,%s\n", reg_names[12], reg_names[0]);
      else
	fprintf (file, "\t%s\t%s," HOST_WIDE_INT_PRINT_DEC "(%s)\n",
		 Pmode == DImode ? "dla" : "la", reg_names[12],
		 cfun->machine->frame.ra_fp_offset,
		 reg_names[STACK_POINTER_REGNUM]);
    }
  if (!TARGET_NEWABI)
    fprintf (file,
	     "\t%s\t%s,%s,%d\t\t# _mcount pops 2 words from  stack\n",
	     TARGET_64BIT ? "dsubu" : "subu",
	     reg_names[STACK_POINTER_REGNUM],
	     reg_names[STACK_POINTER_REGNUM],
	     Pmode == DImode ? 16 : 8);

  if (TARGET_LONG_CALLS)
    fprintf (file, "\tjalr\t%s\n", reg_names[3]);
  else
    fprintf (file, "\tjal\t_mcount\n");
  mips_pop_asm_switch (&mips_noat);
  /* _mcount treats $2 as the static chain register.  */
  if (cfun->static_chain_decl != NULL)
    fprintf (file, "\tmove\t%s,%s\n", reg_names[STATIC_CHAIN_REGNUM],
	     reg_names[2]);
}

/* Implement TARGET_SHIFT_TRUNCATION_MASK.  We want to keep the default
   behaviour of TARGET_SHIFT_TRUNCATION_MASK for non-vector modes even
   when TARGET_LOONGSON_VECTORS is true.  */

static unsigned HOST_WIDE_INT
mips_shift_truncation_mask (enum machine_mode mode)
{
  if (TARGET_LOONGSON_VECTORS && VECTOR_MODE_P (mode))
    return 0;

  return GET_MODE_BITSIZE (mode) - 1;
}


/* Initialize the GCC target structure.  */
#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.half\t"
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.word\t"
#undef TARGET_ASM_ALIGNED_DI_OP
#define TARGET_ASM_ALIGNED_DI_OP "\t.dword\t"

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE mips_option_override
#undef TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE mips_option_optimization_table

#undef TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS mips_legitimize_address

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE mips_output_function_prologue
#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE mips_output_function_epilogue
#undef TARGET_ASM_SELECT_RTX_SECTION
#define TARGET_ASM_SELECT_RTX_SECTION mips_select_rtx_section
#undef TARGET_ASM_FUNCTION_RODATA_SECTION
#define TARGET_ASM_FUNCTION_RODATA_SECTION mips_function_rodata_section

#undef TARGET_SCHED_INIT
#define TARGET_SCHED_INIT mips_sched_init
#undef TARGET_SCHED_REORDER
#define TARGET_SCHED_REORDER mips_sched_reorder
#undef TARGET_SCHED_REORDER2
#define TARGET_SCHED_REORDER2 mips_sched_reorder2
#undef TARGET_SCHED_VARIABLE_ISSUE
#define TARGET_SCHED_VARIABLE_ISSUE mips_variable_issue
#undef TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST mips_adjust_cost
#undef TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE mips_issue_rate
#undef TARGET_SCHED_INIT_DFA_POST_CYCLE_INSN
#define TARGET_SCHED_INIT_DFA_POST_CYCLE_INSN mips_init_dfa_post_cycle_insn
#undef TARGET_SCHED_DFA_POST_ADVANCE_CYCLE
#define TARGET_SCHED_DFA_POST_ADVANCE_CYCLE mips_dfa_post_advance_cycle
#undef TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD
#define TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD \
  mips_multipass_dfa_lookahead
#undef TARGET_SMALL_REGISTER_CLASSES_FOR_MODE_P
#define TARGET_SMALL_REGISTER_CLASSES_FOR_MODE_P \
  mips_small_register_classes_for_mode_p

#undef TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS		\
  (TARGET_DEFAULT				\
   | TARGET_CPU_DEFAULT				\
   | TARGET_ENDIAN_DEFAULT			\
   | TARGET_FP_EXCEPTIONS_DEFAULT		\
   | MASK_CHECK_ZERO_DIV			\
   | MASK_FUSED_MADD)
#undef TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION mips_handle_option

#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL mips_function_ok_for_sibcall

#undef TARGET_INSERT_ATTRIBUTES
#define TARGET_INSERT_ATTRIBUTES mips_insert_attributes
#undef TARGET_MERGE_DECL_ATTRIBUTES
#define TARGET_MERGE_DECL_ATTRIBUTES mips_merge_decl_attributes
#undef TARGET_SET_CURRENT_FUNCTION
#define TARGET_SET_CURRENT_FUNCTION mips_set_current_function

#undef TARGET_VALID_POINTER_MODE
#define TARGET_VALID_POINTER_MODE mips_valid_pointer_mode
#undef TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST mips_register_move_cost
#undef TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST mips_memory_move_cost
#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS mips_rtx_costs
#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST mips_address_cost

#undef TARGET_IN_SMALL_DATA_P
#define TARGET_IN_SMALL_DATA_P mips_in_small_data_p

#undef TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG mips_reorg

#undef  TARGET_PREFERRED_RELOAD_CLASS
#define TARGET_PREFERRED_RELOAD_CLASS mips_preferred_reload_class

#undef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START mips_file_start
#undef TARGET_ASM_FILE_START_FILE_DIRECTIVE
#define TARGET_ASM_FILE_START_FILE_DIRECTIVE true

#undef TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS mips_init_libfuncs

#undef TARGET_BUILD_BUILTIN_VA_LIST
#define TARGET_BUILD_BUILTIN_VA_LIST mips_build_builtin_va_list
#undef TARGET_EXPAND_BUILTIN_VA_START
#define TARGET_EXPAND_BUILTIN_VA_START mips_va_start
#undef TARGET_GIMPLIFY_VA_ARG_EXPR
#define TARGET_GIMPLIFY_VA_ARG_EXPR mips_gimplify_va_arg_expr

#undef  TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE default_promote_function_mode_always_promote
#undef TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES hook_bool_const_tree_true

#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY mips_return_in_memory
#undef TARGET_RETURN_IN_MSB
#define TARGET_RETURN_IN_MSB mips_return_in_msb

#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK mips_output_mi_thunk
#undef TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK hook_bool_const_tree_hwi_hwi_const_tree_true

#undef TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND mips_print_operand
#undef TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS mips_print_operand_address
#undef TARGET_PRINT_OPERAND_PUNCT_VALID_P
#define TARGET_PRINT_OPERAND_PUNCT_VALID_P mips_print_operand_punct_valid_p

#undef TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS mips_setup_incoming_varargs
#undef TARGET_STRICT_ARGUMENT_NAMING
#define TARGET_STRICT_ARGUMENT_NAMING mips_strict_argument_naming
#undef TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK must_pass_in_stack_var_size
#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE mips_pass_by_reference
#undef TARGET_CALLEE_COPIES
#define TARGET_CALLEE_COPIES mips_callee_copies
#undef TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES mips_arg_partial_bytes
#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG mips_function_arg
#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE mips_function_arg_advance
#undef TARGET_FUNCTION_ARG_BOUNDARY
#define TARGET_FUNCTION_ARG_BOUNDARY mips_function_arg_boundary

#undef TARGET_MODE_REP_EXTENDED
#define TARGET_MODE_REP_EXTENDED mips_mode_rep_extended

#undef TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P mips_vector_mode_supported_p

#undef TARGET_SCALAR_MODE_SUPPORTED_P
#define TARGET_SCALAR_MODE_SUPPORTED_P mips_scalar_mode_supported_p

#undef TARGET_VECTORIZE_PREFERRED_SIMD_MODE
#define TARGET_VECTORIZE_PREFERRED_SIMD_MODE mips_preferred_simd_mode

#undef TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS mips_init_builtins
#undef TARGET_BUILTIN_DECL
#define TARGET_BUILTIN_DECL mips_builtin_decl
#undef TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN mips_expand_builtin

#undef TARGET_HAVE_TLS
#define TARGET_HAVE_TLS HAVE_AS_TLS

#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM mips_cannot_force_const_mem

#undef TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO mips_encode_section_info

#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE mips_attribute_table
/* All our function attributes are related to how out-of-line copies should
   be compiled or called.  They don't in themselves prevent inlining.  */
#undef TARGET_FUNCTION_ATTRIBUTE_INLINABLE_P
#define TARGET_FUNCTION_ATTRIBUTE_INLINABLE_P hook_bool_const_tree_true

#undef TARGET_EXTRA_LIVE_ON_ENTRY
#define TARGET_EXTRA_LIVE_ON_ENTRY mips_extra_live_on_entry

#undef TARGET_USE_BLOCKS_FOR_CONSTANT_P
#define TARGET_USE_BLOCKS_FOR_CONSTANT_P mips_use_blocks_for_constant_p
#undef TARGET_USE_ANCHORS_FOR_SYMBOL_P
#define TARGET_USE_ANCHORS_FOR_SYMBOL_P mips_use_anchors_for_symbol_p

#undef  TARGET_COMP_TYPE_ATTRIBUTES
#define TARGET_COMP_TYPE_ATTRIBUTES mips_comp_type_attributes

#ifdef HAVE_AS_DTPRELWORD
#undef TARGET_ASM_OUTPUT_DWARF_DTPREL
#define TARGET_ASM_OUTPUT_DWARF_DTPREL mips_output_dwarf_dtprel
#endif
#undef TARGET_DWARF_REGISTER_SPAN
#define TARGET_DWARF_REGISTER_SPAN mips_dwarf_register_span

#undef TARGET_IRA_COVER_CLASSES
#define TARGET_IRA_COVER_CLASSES mips_ira_cover_classes

#undef TARGET_ASM_FINAL_POSTSCAN_INSN
#define TARGET_ASM_FINAL_POSTSCAN_INSN mips_final_postscan_insn

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P	mips_legitimate_address_p

#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED mips_frame_pointer_required

#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE mips_can_eliminate

#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE mips_conditional_register_usage

#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT mips_trampoline_init

#undef TARGET_ASM_OUTPUT_SOURCE_FILENAME
#define TARGET_ASM_OUTPUT_SOURCE_FILENAME mips_output_filename

#undef TARGET_SHIFT_TRUNCATION_MASK
#define TARGET_SHIFT_TRUNCATION_MASK mips_shift_truncation_mask

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-mips.h"
