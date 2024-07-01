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

#ifndef GCC_RISCV_PROTOS_H
#define GCC_RISCV_PROTOS_H

#include "memmodel.h"

/* Symbol types we understand.  The order of this list must match that of
   the unspec enum in riscv.md, subsequent to UNSPEC_ADDRESS_FIRST.  */
enum riscv_symbol_type {
  SYMBOL_ABSOLUTE,
  SYMBOL_FORCE_TO_MEM,
  SYMBOL_PCREL,
  SYMBOL_GOT_DISP,
  SYMBOL_TLS,
  SYMBOL_TLS_LE,
  SYMBOL_TLS_IE,
  SYMBOL_TLS_GD,
  SYMBOL_TLSDESC,
};
#define NUM_SYMBOL_TYPES (SYMBOL_TLSDESC + 1)

/* Classifies an address.

   ADDRESS_REG
       A natural register + offset address.  The register satisfies
       riscv_valid_base_register_p and the offset is a const_arith_operand.

   ADDRESS_REG_REG
       A base register indexed by (optionally scaled) register.

   ADDRESS_REG_UREG
       A base register indexed by (optionally scaled) zero-extended register.

   ADDRESS_REG_WB
       A base register indexed by immediate offset with writeback.

   ADDRESS_LO_SUM
       A LO_SUM rtx.  The first operand is a valid base register and
       the second operand is a symbolic address.

   ADDRESS_CONST_INT
       A signed 16-bit constant address.

   ADDRESS_SYMBOLIC:
       A constant symbolic address.  */
enum riscv_address_type {
  ADDRESS_REG,
  ADDRESS_REG_REG,
  ADDRESS_REG_UREG,
  ADDRESS_REG_WB,
  ADDRESS_LO_SUM,
  ADDRESS_CONST_INT,
  ADDRESS_SYMBOLIC
};

/* Information about an address described by riscv_address_type.

   ADDRESS_CONST_INT
       No fields are used.

   ADDRESS_REG
       REG is the base register and OFFSET is the constant offset.

   ADDRESS_REG_REG and ADDRESS_REG_UREG
       REG is the base register and OFFSET is the index register.

   ADDRESS_REG_WB
       REG is the base register, OFFSET is the constant offset, and
       shift is the shift amount for the offset.

   ADDRESS_LO_SUM
       REG and OFFSET are the operands to the LO_SUM and SYMBOL_TYPE
       is the type of symbol it references.

   ADDRESS_SYMBOLIC
       SYMBOL_TYPE is the type of symbol that the address references.  */
struct riscv_address_info {
  enum riscv_address_type type;
  rtx reg;
  rtx offset;
  enum riscv_symbol_type symbol_type;
  int shift;
};

/* Routines implemented in riscv.cc.  */
extern const char *riscv_asm_output_opcode (FILE *asm_out_file, const char *p);
extern enum riscv_symbol_type riscv_classify_symbolic_expression (rtx);
extern bool riscv_symbolic_constant_p (rtx, enum riscv_symbol_type *);
extern int riscv_float_const_rtx_index_for_fli (rtx);
extern int riscv_regno_mode_ok_for_base_p (int, machine_mode, bool);
extern bool riscv_valid_base_register_p (rtx, machine_mode, bool);
extern enum reg_class riscv_index_reg_class ();
extern int riscv_regno_ok_for_index_p (int);
extern int riscv_address_insns (rtx, machine_mode, bool);
extern int riscv_const_insns (rtx);
extern int riscv_split_const_insns (rtx);
extern int riscv_load_store_insns (rtx, rtx_insn *);
extern rtx riscv_emit_move (rtx, rtx);
extern bool riscv_split_symbol (rtx, rtx, machine_mode, rtx *);
extern bool riscv_split_symbol_type (enum riscv_symbol_type);
extern rtx riscv_unspec_address (rtx, enum riscv_symbol_type);
extern void riscv_move_integer (rtx, rtx, HOST_WIDE_INT, machine_mode);
extern bool riscv_legitimize_move (machine_mode, rtx, rtx);
extern rtx riscv_subword (rtx, bool);
extern bool riscv_split_64bit_move_p (rtx, rtx);
extern void riscv_split_doubleword_move (rtx, rtx);
extern const char *riscv_output_move (rtx, rtx);
extern const char *riscv_output_return ();
extern void riscv_declare_function_name (FILE *, const char *, tree);
extern void riscv_declare_function_size (FILE *, const char *, tree);
extern void riscv_asm_output_alias (FILE *, const tree, const tree);
extern void riscv_asm_output_external (FILE *, const tree, const char *);
extern bool
riscv_zcmp_valid_stack_adj_bytes_p (HOST_WIDE_INT, int);
extern void riscv_legitimize_poly_move (machine_mode, rtx, rtx, rtx);

#ifdef RTX_CODE
extern void riscv_expand_int_scc (rtx, enum rtx_code, rtx, rtx, bool *invert_ptr = 0);
extern void riscv_expand_float_scc (rtx, enum rtx_code, rtx, rtx,
				    bool *invert_ptr = nullptr);
extern void riscv_expand_conditional_branch (rtx, enum rtx_code, rtx, rtx);
extern rtx riscv_emit_unary (enum rtx_code code, rtx dest, rtx x);
extern rtx riscv_emit_binary (enum rtx_code code, rtx dest, rtx x, rtx y);
#endif
extern bool riscv_expand_conditional_move (rtx, rtx, rtx, rtx);
extern rtx riscv_legitimize_call_address (rtx);
extern void riscv_set_return_address (rtx, rtx);
extern rtx riscv_return_addr (int, rtx);
extern poly_int64 riscv_initial_elimination_offset (int, int);
extern void riscv_expand_prologue (void);
extern void riscv_expand_epilogue (int);
extern bool riscv_epilogue_uses (unsigned int);
extern bool riscv_can_use_return_insn (void);
extern rtx riscv_function_value (const_tree, const_tree, enum machine_mode);
extern bool riscv_store_data_bypass_p (rtx_insn *, rtx_insn *);
extern rtx riscv_gen_gpr_save_insn (struct riscv_frame_info *);
extern bool riscv_gpr_save_operation_p (rtx);
extern void riscv_reinit (void);
extern poly_uint64 riscv_regmode_natural_size (machine_mode);
extern bool riscv_v_ext_vector_mode_p (machine_mode);
extern bool riscv_v_ext_tuple_mode_p (machine_mode);
extern bool riscv_v_ext_vls_mode_p (machine_mode);
extern int riscv_get_v_regno_alignment (machine_mode);
extern bool riscv_shamt_matches_mask_p (int, HOST_WIDE_INT);
extern void riscv_subword_address (rtx, rtx *, rtx *, rtx *, rtx *);
extern void riscv_lshift_subword (machine_mode, rtx, rtx, rtx *);
extern enum memmodel riscv_union_memmodels (enum memmodel, enum memmodel);

/* Routines implemented in riscv-c.cc.  */
void riscv_cpu_cpp_builtins (cpp_reader *);
void riscv_register_pragmas (void);

/* Routines implemented in riscv-builtins.cc.  */
extern void riscv_atomic_assign_expand_fenv (tree *, tree *, tree *);
extern bool riscv_gimple_fold_builtin (gimple_stmt_iterator *);
extern rtx riscv_expand_builtin (tree, rtx, rtx, machine_mode, int);
extern tree riscv_builtin_decl (unsigned int, bool);
extern void riscv_init_builtins (void);

/* Routines implemented in riscv-common.cc.  */
extern std::string riscv_arch_str (bool version_p = true);
extern void riscv_parse_arch_string (const char *, struct gcc_options *, location_t);

extern bool riscv_hard_regno_rename_ok (unsigned, unsigned);

rtl_opt_pass * make_pass_shorten_memrefs (gcc::context *ctxt);
rtl_opt_pass * make_pass_avlprop (gcc::context *ctxt);
rtl_opt_pass * make_pass_vsetvl (gcc::context *ctxt);

/* Routines implemented in riscv-string.c.  */
extern bool riscv_expand_block_move (rtx, rtx, rtx);

/* Information about one CPU we know about.  */
struct riscv_cpu_info {
  /* This CPU's canonical name.  */
  const char *name;

  /* Default arch for this CPU, could be NULL if no default arch.  */
  const char *arch;

  /* Which automaton to use for tuning.  */
  const char *tune;
};

extern const riscv_cpu_info *riscv_find_cpu (const char *);

/* Common vector costs in any kind of vectorization (e.g VLA and VLS).  */
struct common_vector_cost
{
  /* Cost of any integer vector operation, excluding the ones handled
     specially below.  */
  const int int_stmt_cost;

  /* Cost of any fp vector operation, excluding the ones handled
     specially below.  */
  const int fp_stmt_cost;

  /* Gather/scatter vectorization cost.  */
  const int gather_load_cost;
  const int scatter_store_cost;

  /* Cost of a vector-to-scalar operation.  */
  const int vec_to_scalar_cost;

  /* Cost of a scalar-to-vector operation.  */
  const int scalar_to_vec_cost;

  /* Cost of a permute operation.  */
  const int permute_cost;

  /* Cost of an aligned vector load.  */
  const int align_load_cost;

  /* Cost of an aligned vector store.  */
  const int align_store_cost;

  /* Cost of an unaligned vector load.  */
  const int unalign_load_cost;

  /* Cost of an unaligned vector store.  */
  const int unalign_store_cost;
};

/* scalable vectorization (VLA) specific cost.  */
struct scalable_vector_cost : common_vector_cost
{
  CONSTEXPR scalable_vector_cost (const common_vector_cost &base)
    : common_vector_cost (base)
  {}

  /* TODO: We will need more other kinds of vector cost for VLA.
     E.g. fold_left reduction cost, lanes load/store cost, ..., etc.  */
};

/* Additional costs for register copies.  Cost is for one register.  */
struct regmove_vector_cost
{
  const int GR2VR;
  const int FR2VR;
  const int VR2GR;
  const int VR2FR;
};

/* Cost for vector insn classes.  */
struct cpu_vector_cost
{
  /* Cost of any integer scalar operation, excluding load and store.  */
  const int scalar_int_stmt_cost;

  /* Cost of any fp scalar operation, excluding load and store.  */
  const int scalar_fp_stmt_cost;

  /* Cost of a scalar load.  */
  const int scalar_load_cost;

  /* Cost of a scalar store.  */
  const int scalar_store_cost;

  /* Cost of a taken branch.  */
  const int cond_taken_branch_cost;

  /* Cost of a not-taken branch.  */
  const int cond_not_taken_branch_cost;

  /* Cost of an VLS modes operations.  */
  const common_vector_cost *vls;

  /* Cost of an VLA modes operations.  */
  const scalable_vector_cost *vla;

  /* Cost of vector register move operations.  */
  const regmove_vector_cost *regmove;
};

/* Routines implemented in riscv-selftests.cc.  */
#if CHECKING_P
namespace selftest {
void riscv_run_selftests (void);
} // namespace selftest
#endif

namespace riscv_vector {
#define RVV_VLMAX regno_reg_rtx[X0_REGNUM]
#define RVV_VUNDEF(MODE)                                                       \
  gen_rtx_UNSPEC (MODE, gen_rtvec (1, RVV_VLMAX), UNSPEC_VUNDEF)

/* These flags describe how to pass the operands to a rvv insn pattern.
   e.g.:
     If a insn has this flags:
       HAS_DEST_P | HAS_MASK_P | USE_VUNDEF_MERGE_P
	 | TU_POLICY_P | BINARY_OP_P | FRM_DYN_P
     that means:
       operands[0] is the dest operand
       operands[1] is the mask operand
       operands[2] is the merge operand
       operands[3] and operands[4] is the two operand to do the operation.
       operands[5] is the vl operand
       operands[6] is the tail policy operand
       operands[7] is the mask policy operands
       operands[8] is the rounding mode operands

     Then you can call `emit_vlmax_insn (flags, icode, ops)` to emit a insn.
     and ops[0] is the dest operand (operands[0]), ops[1] is the mask
     operand (operands[1]), ops[2] and ops[3] is the two
     operands (operands[3], operands[4]) to do the operation. Other operands
     will be created by emit_vlmax_insn according to the flags information.
*/
enum insn_flags : unsigned int
{
  /* flags for dest, mask, merge operands.  */
  /* Means INSN has dest operand. False for STORE insn.  */
  HAS_DEST_P = 1 << 0,
  /* Means INSN has mask operand.  */
  HAS_MASK_P = 1 << 1,
  /* Means using ALL_TRUES for mask operand.  */
  USE_ALL_TRUES_MASK_P = 1 << 2,
  /* Means using ONE_TRUE for mask operand.  */
  USE_ONE_TRUE_MASK_P = 1 << 3,
  /* Means INSN has merge operand.  */
  HAS_MERGE_P = 1 << 4,
  /* Means using VUNDEF for merge operand.  */
  USE_VUNDEF_MERGE_P = 1 << 5,

  /* flags for tail policy and mask plicy operands.  */
  /* Means the tail policy is TAIL_UNDISTURBED.  */
  TU_POLICY_P = 1 << 6,
  /* Means the tail policy is default (return by get_prefer_tail_policy).  */
  TDEFAULT_POLICY_P = 1 << 7,
  /* Means the mask policy is MASK_UNDISTURBED.  */
  MU_POLICY_P = 1 << 8,
  /* Means the mask policy is default (return by get_prefer_mask_policy).  */
  MDEFAULT_POLICY_P = 1 << 9,

  /* flags for the number operands to do the operation.  */
  /* Means INSN need zero operand to do the operation. e.g. vid.v */
  NULLARY_OP_P = 1 << 10,
  /* Means INSN need one operand to do the operation.  */
  UNARY_OP_P = 1 << 11,
  /* Means INSN need two operands to do the operation.  */
  BINARY_OP_P = 1 << 12,
  /* Means INSN need two operands to do the operation.  */
  TERNARY_OP_P = 1 << 13,

  /* flags for get vtype mode from the index number. default from dest operand.  */
  VTYPE_MODE_FROM_OP1_P = 1 << 14,

  /* flags for the floating-point rounding mode.  */
  /* Means INSN has FRM operand and the value is FRM_DYN.  */
  FRM_DYN_P = 1 << 15,

  /* Means INSN has FRM operand and the value is FRM_RUP.  */
  FRM_RUP_P = 1 << 16,

  /* Means INSN has FRM operand and the value is FRM_RDN.  */
  FRM_RDN_P = 1 << 17,

  /* Means INSN has FRM operand and the value is FRM_RMM.  */
  FRM_RMM_P = 1 << 18,

  /* Means INSN has FRM operand and the value is FRM_RNE.  */
  FRM_RNE_P = 1 << 19,

  /* Means INSN has VXRM operand and the value is VXRM_RNU.  */
  VXRM_RNU_P = 1 << 20,

  /* Means INSN has VXRM operand and the value is VXRM_RDN.  */
  VXRM_RDN_P = 1 << 21,
};

enum insn_type : unsigned int
{
  /* some flags macros.  */
  /* For non-mask insn with tama.  */
  __NORMAL_OP = HAS_DEST_P | HAS_MASK_P | USE_ALL_TRUES_MASK_P | HAS_MERGE_P
		| USE_VUNDEF_MERGE_P | TDEFAULT_POLICY_P | MDEFAULT_POLICY_P,
  /* For non-mask insn with ta, without mask policy operand.  */
  __NORMAL_OP_TA = HAS_DEST_P | HAS_MASK_P | USE_ALL_TRUES_MASK_P | HAS_MERGE_P
		   | USE_VUNDEF_MERGE_P | TDEFAULT_POLICY_P,
  /* For non-mask insn with ta, without mask operand and mask policy operand. */
  __NORMAL_OP_TA2
  = HAS_DEST_P | HAS_MERGE_P | USE_VUNDEF_MERGE_P | TDEFAULT_POLICY_P,
  /* For non-mask insn with ma, without tail policy operand.  */
  __NORMAL_OP_MA = HAS_DEST_P | HAS_MASK_P | USE_ALL_TRUES_MASK_P | HAS_MERGE_P
		   | USE_VUNDEF_MERGE_P | MDEFAULT_POLICY_P,
  /* For mask insn with tama.  */
  __MASK_OP_TAMA = HAS_DEST_P | HAS_MASK_P | HAS_MERGE_P | USE_VUNDEF_MERGE_P
		   | TDEFAULT_POLICY_P | MDEFAULT_POLICY_P,
  /* For mask insn with tamu.  */
  __MASK_OP_TAMU
  = HAS_DEST_P | HAS_MASK_P | HAS_MERGE_P | TDEFAULT_POLICY_P | MU_POLICY_P,
  /* For mask insn with tuma.  */
  __MASK_OP_TUMA = HAS_DEST_P | HAS_MASK_P | USE_ALL_TRUES_MASK_P | HAS_MERGE_P
		   | TU_POLICY_P | MDEFAULT_POLICY_P,
  /* For mask insn with mu.  */
  __MASK_OP_MU = HAS_DEST_P | HAS_MASK_P | HAS_MERGE_P | MU_POLICY_P,
  /* For mask insn with ta, without mask policy operand.  */
  __MASK_OP_TA = HAS_DEST_P | HAS_MASK_P | HAS_MERGE_P | USE_VUNDEF_MERGE_P
		 | TDEFAULT_POLICY_P,

  /* Nullary operator. e.g. vid.v  */
  NULLARY_OP = __NORMAL_OP | NULLARY_OP_P,

  /* Unary operator.  */
  UNARY_OP = __NORMAL_OP | UNARY_OP_P,
  UNARY_OP_TAMA = __MASK_OP_TAMA | UNARY_OP_P,
  UNARY_OP_TAMU = __MASK_OP_TAMU | UNARY_OP_P,
  UNARY_OP_FRM_DYN = UNARY_OP | FRM_DYN_P,
  UNARY_OP_FRM_RMM = UNARY_OP | FRM_RMM_P,
  UNARY_OP_FRM_RUP = UNARY_OP | FRM_RUP_P,
  UNARY_OP_FRM_RDN = UNARY_OP | FRM_RDN_P,
  UNARY_OP_TAMA_FRM_DYN = UNARY_OP_TAMA | FRM_DYN_P,
  UNARY_OP_TAMA_FRM_RUP = UNARY_OP_TAMA | FRM_RUP_P,
  UNARY_OP_TAMA_FRM_RDN = UNARY_OP_TAMA | FRM_RDN_P,
  UNARY_OP_TAMA_FRM_RMM = UNARY_OP_TAMA | FRM_RMM_P,
  UNARY_OP_TAMA_FRM_RNE = UNARY_OP_TAMA | FRM_RNE_P,
  UNARY_OP_TAMU_FRM_DYN = UNARY_OP_TAMU | FRM_DYN_P,
  UNARY_OP_TAMU_FRM_RUP = UNARY_OP_TAMU | FRM_RUP_P,
  UNARY_OP_TAMU_FRM_RDN = UNARY_OP_TAMU | FRM_RDN_P,
  UNARY_OP_TAMU_FRM_RMM = UNARY_OP_TAMU | FRM_RMM_P,
  UNARY_OP_TAMU_FRM_RNE = UNARY_OP_TAMU | FRM_RNE_P,

  /* Binary operator.  */
  BINARY_OP = __NORMAL_OP | BINARY_OP_P,
  BINARY_OP_TAMA = __MASK_OP_TAMA | BINARY_OP_P,
  BINARY_OP_TAMU = __MASK_OP_TAMU | BINARY_OP_P,
  BINARY_OP_TUMA = __MASK_OP_TUMA | BINARY_OP_P,
  BINARY_OP_FRM_DYN = BINARY_OP | FRM_DYN_P,
  BINARY_OP_VXRM_RNU = BINARY_OP | VXRM_RNU_P,
  BINARY_OP_VXRM_RDN = BINARY_OP | VXRM_RDN_P,

  /* Ternary operator. Always have real merge operand.  */
  TERNARY_OP = HAS_DEST_P | HAS_MASK_P | USE_ALL_TRUES_MASK_P | HAS_MERGE_P
	       | TDEFAULT_POLICY_P | MDEFAULT_POLICY_P | TERNARY_OP_P,
  TERNARY_OP_FRM_DYN = TERNARY_OP | FRM_DYN_P,

  /* For vwmacc, no merge operand.  */
  WIDEN_TERNARY_OP = HAS_DEST_P | HAS_MASK_P | USE_ALL_TRUES_MASK_P
		     | TDEFAULT_POLICY_P | MDEFAULT_POLICY_P | TERNARY_OP_P,
  WIDEN_TERNARY_OP_FRM_DYN = WIDEN_TERNARY_OP | FRM_DYN_P,

  /* For vmerge, no mask operand, no mask policy operand.  */
  MERGE_OP = __NORMAL_OP_TA2 | TERNARY_OP_P,

  /* For vmerge with TU policy.  */
  MERGE_OP_TU = HAS_DEST_P | HAS_MERGE_P | TERNARY_OP_P | TU_POLICY_P,

  /* For vm<compare>, no tail policy operand.  */
  COMPARE_OP = __NORMAL_OP_MA | TERNARY_OP_P,
  COMPARE_OP_MU = __MASK_OP_MU | TERNARY_OP_P,

  /* For scatter insn: no dest operand, no merge operand, no tail and mask
     policy operands.  */
  SCATTER_OP_M = HAS_MASK_P | TERNARY_OP_P,

  /* For vcpop.m, no merge operand, no tail and mask policy operands.  */
  CPOP_OP = HAS_DEST_P | HAS_MASK_P | USE_ALL_TRUES_MASK_P | UNARY_OP_P
	    | VTYPE_MODE_FROM_OP1_P,

  /* For mask instrunctions, no tail and mask policy operands.  */
  UNARY_MASK_OP = HAS_DEST_P | HAS_MASK_P | USE_ALL_TRUES_MASK_P | HAS_MERGE_P
		  | USE_VUNDEF_MERGE_P | UNARY_OP_P,
  BINARY_MASK_OP = HAS_DEST_P | HAS_MASK_P | USE_ALL_TRUES_MASK_P | HAS_MERGE_P
		   | USE_VUNDEF_MERGE_P | BINARY_OP_P,

  /* For vcompress.vm */
  COMPRESS_OP = __NORMAL_OP_TA2 | BINARY_OP_P,
  /* has merge operand but use ta.  */
  COMPRESS_OP_MERGE
  = HAS_DEST_P | HAS_MERGE_P | TDEFAULT_POLICY_P | BINARY_OP_P,

  /* For vslideup.up has merge operand but use ta.  */
  SLIDEUP_OP_MERGE = HAS_DEST_P | HAS_MASK_P | USE_ALL_TRUES_MASK_P
		     | HAS_MERGE_P | TDEFAULT_POLICY_P | MDEFAULT_POLICY_P
		     | BINARY_OP_P,

  /* For vreduce, no mask policy operand. */
  REDUCE_OP = __NORMAL_OP_TA | BINARY_OP_P | VTYPE_MODE_FROM_OP1_P,
  REDUCE_OP_M = __MASK_OP_TA | BINARY_OP_P | VTYPE_MODE_FROM_OP1_P,
  REDUCE_OP_FRM_DYN = REDUCE_OP | FRM_DYN_P | VTYPE_MODE_FROM_OP1_P,
  REDUCE_OP_M_FRM_DYN
  = __MASK_OP_TA | BINARY_OP_P | FRM_DYN_P | VTYPE_MODE_FROM_OP1_P,

  /* For vmv.s.x/vfmv.s.f.  */
  SCALAR_MOVE_OP = HAS_DEST_P | HAS_MASK_P | USE_ONE_TRUE_MASK_P | HAS_MERGE_P
		   | USE_VUNDEF_MERGE_P | TDEFAULT_POLICY_P | MDEFAULT_POLICY_P
		   | UNARY_OP_P,

  SCALAR_MOVE_MERGED_OP = HAS_DEST_P | HAS_MASK_P | USE_ONE_TRUE_MASK_P
			  | HAS_MERGE_P | TDEFAULT_POLICY_P | MDEFAULT_POLICY_P
			  | UNARY_OP_P,

  SCALAR_MOVE_MERGED_OP_TU = HAS_DEST_P | HAS_MASK_P | USE_ONE_TRUE_MASK_P
			  | HAS_MERGE_P | TU_POLICY_P | MDEFAULT_POLICY_P
			  | UNARY_OP_P,
};

enum vlmul_type
{
  LMUL_1 = 0,
  LMUL_2 = 1,
  LMUL_4 = 2,
  LMUL_8 = 3,
  LMUL_RESERVED = 4,
  LMUL_F8 = 5,
  LMUL_F4 = 6,
  LMUL_F2 = 7,
  NUM_LMUL = 8
};

/* The RISC-V vsetvli pass uses "known vlmax" operations for optimization.
   Whether or not an instruction actually is a vlmax operation is not
   recognizable from the length operand alone but the avl_type operand
   is used instead.  In general, there are two cases:

    - Emit a vlmax operation by calling emit_vlmax_insn[_lra].  Here we emit
      a vsetvli with vlmax configuration and set the avl_type to VLMAX for
      VLA modes or VLS for VLS modes.
    - Emit an operation that uses the existing (last-set) length and
      set the avl_type to NONVLMAX.

    Sometimes we also need to set the VLMAX or VLS avl_type to an operation that
    already uses a given length register.  This can happen during or after
    register allocation when we are not allowed to create a new register.
    For that case we also allow to set the avl_type to VLMAX or VLS.
*/
enum avl_type
{
  NONVLMAX = 0,
  VLMAX = 1,
  VLS = 2,
};
/* Routines implemented in riscv-vector-builtins.cc.  */
void init_builtins (void);
void reinit_builtins (void);
const char *mangle_builtin_type (const_tree);
tree lookup_vector_type_attribute (const_tree);
bool builtin_type_p (const_tree);
#ifdef GCC_TARGET_H
bool verify_type_context (location_t, type_context_kind, const_tree, bool);
bool expand_vec_perm_const (machine_mode, machine_mode, rtx, rtx, rtx,
			    const vec_perm_indices &);
#endif
void handle_pragma_vector (void);
tree builtin_decl (unsigned, bool);
gimple *gimple_fold_builtin (unsigned int, gimple_stmt_iterator *, gcall *);
rtx expand_builtin (unsigned int, tree, rtx);
bool check_builtin_call (location_t, vec<location_t>, unsigned int,
			   tree, unsigned int, tree *);
tree resolve_overloaded_builtin (location_t, unsigned int, tree, vec<tree, va_gc> *);
bool const_vec_all_same_in_range_p (rtx, HOST_WIDE_INT, HOST_WIDE_INT);
bool legitimize_move (rtx, rtx *);
void emit_vlmax_vsetvl (machine_mode, rtx);
void emit_hard_vlmax_vsetvl (machine_mode, rtx);
void emit_vlmax_insn (unsigned, unsigned, rtx *);
void emit_nonvlmax_insn (unsigned, unsigned, rtx *, rtx);
void emit_vlmax_insn_lra (unsigned, unsigned, rtx *, rtx);
enum vlmul_type get_vlmul (machine_mode);
rtx get_vlmax_rtx (machine_mode);
unsigned int get_ratio (machine_mode);
unsigned int get_nf (machine_mode);
machine_mode get_subpart_mode (machine_mode);
int get_ta (rtx);
int get_ma (rtx);
int get_avl_type (rtx);
unsigned int calculate_ratio (unsigned int, enum vlmul_type);
enum tail_policy
{
  TAIL_UNDISTURBED = 0,
  TAIL_AGNOSTIC = 1,
  TAIL_ANY = 2,
};

enum mask_policy
{
  MASK_UNDISTURBED = 0,
  MASK_AGNOSTIC = 1,
  MASK_ANY = 2,
};

/* Return true if VALUE is agnostic or any policy.  */
#define IS_AGNOSTIC(VALUE) (bool) (VALUE & 0x1 || (VALUE >> 1 & 0x1))

enum tail_policy get_prefer_tail_policy ();
enum mask_policy get_prefer_mask_policy ();
rtx get_avl_type_rtx (enum avl_type);
opt_machine_mode get_vector_mode (scalar_mode, poly_uint64);
opt_machine_mode get_tuple_mode (machine_mode, unsigned int);
bool simm5_p (rtx);
bool neg_simm5_p (rtx);
#ifdef RTX_CODE
bool has_vi_variant_p (rtx_code, rtx);
void expand_vec_cmp (rtx, rtx_code, rtx, rtx, rtx = nullptr, rtx = nullptr);
bool expand_vec_cmp_float (rtx, rtx_code, rtx, rtx, bool);
void expand_cond_len_unop (unsigned, rtx *);
void expand_cond_len_binop (unsigned, rtx *);
void expand_reduction (unsigned, unsigned, rtx *, rtx);
void expand_vec_ceil (rtx, rtx, machine_mode, machine_mode);
void expand_vec_floor (rtx, rtx, machine_mode, machine_mode);
void expand_vec_nearbyint (rtx, rtx, machine_mode, machine_mode);
void expand_vec_rint (rtx, rtx, machine_mode, machine_mode);
void expand_vec_round (rtx, rtx, machine_mode, machine_mode);
void expand_vec_trunc (rtx, rtx, machine_mode, machine_mode);
void expand_vec_roundeven (rtx, rtx, machine_mode, machine_mode);
void expand_vec_lrint (rtx, rtx, machine_mode, machine_mode, machine_mode);
void expand_vec_lround (rtx, rtx, machine_mode, machine_mode, machine_mode);
void expand_vec_lceil (rtx, rtx, machine_mode, machine_mode);
void expand_vec_lfloor (rtx, rtx, machine_mode, machine_mode);
#endif
bool sew64_scalar_helper (rtx *, rtx *, rtx, machine_mode,
			  bool, void (*)(rtx *, rtx), enum avl_type);
rtx gen_scalar_move_mask (machine_mode);
rtx gen_no_side_effects_vsetvl_rtx (machine_mode, rtx, rtx);

/* RVV vector register sizes.
   TODO: Currently, we only add RVV_32/RVV_64/RVV_128, we may need to
   support other values in the future.  */
enum vlen_enum
{
  RVV_32 = 32,
  RVV_64 = 64,
  RVV_65536 = 65536
};
bool slide1_sew64_helper (int, machine_mode, machine_mode,
			  machine_mode, rtx *);
rtx gen_avl_for_scalar_move (rtx);
void expand_tuple_move (rtx *);
bool expand_block_move (rtx, rtx, rtx);
machine_mode preferred_simd_mode (scalar_mode);
machine_mode get_mask_mode (machine_mode);
void expand_vec_series (rtx, rtx, rtx, rtx = 0);
void expand_vec_init (rtx, rtx);
void expand_vec_perm (rtx, rtx, rtx, rtx);
void expand_select_vl (rtx *);
void expand_load_store (rtx *, bool);
void expand_gather_scatter (rtx *, bool);
void expand_cond_len_ternop (unsigned, rtx *);
void prepare_ternary_operands (rtx *);
void expand_lanes_load_store (rtx *, bool);
void expand_fold_extract_last (rtx *);
void expand_cond_unop (unsigned, rtx *);
void expand_cond_binop (unsigned, rtx *);
void expand_cond_ternop (unsigned, rtx *);
void expand_popcount (rtx *);
void expand_rawmemchr (machine_mode, rtx, rtx, rtx, bool = false);
bool expand_strcmp (rtx, rtx, rtx, rtx, unsigned HOST_WIDE_INT, bool);
void emit_vec_extract (rtx, rtx, rtx);

/* Rounding mode bitfield for fixed point VXRM.  */
enum fixed_point_rounding_mode
{
  VXRM_RNU,
  VXRM_RNE,
  VXRM_RDN,
  VXRM_ROD
};

/* Rounding mode bitfield for floating point FRM.  The value of enum comes
   from the below link.
   https://github.com/riscv/riscv-isa-manual/blob/main/src/f-st-ext.adoc#floating-point-control-and-status-register
 */
enum floating_point_rounding_mode
{
  FRM_RNE = 0, /* Aka 0b000.  */
  FRM_RTZ = 1, /* Aka 0b001.  */
  FRM_RDN = 2, /* Aka 0b010.  */
  FRM_RUP = 3, /* Aka 0b011.  */
  FRM_RMM = 4, /* Aka 0b100.  */
  FRM_DYN = 7, /* Aka 0b111.  */
  FRM_STATIC_MIN = FRM_RNE,
  FRM_STATIC_MAX = FRM_RMM,
  FRM_DYN_EXIT = 8,
  FRM_DYN_CALL = 9,
  FRM_NONE = 10,
};

enum floating_point_rounding_mode get_frm_mode (rtx);
opt_machine_mode vectorize_related_mode (machine_mode, scalar_mode,
					 poly_uint64);
unsigned int autovectorize_vector_modes (vec<machine_mode> *, bool);
bool cmp_lmul_le_one (machine_mode);
bool cmp_lmul_gt_one (machine_mode);
bool vls_mode_valid_p (machine_mode);
bool vlmax_avl_type_p (rtx_insn *);
bool has_vl_op (rtx_insn *);
bool tail_agnostic_p (rtx_insn *);
void validate_change_or_fail (rtx, rtx *, rtx, bool);
bool nonvlmax_avl_type_p (rtx_insn *);
bool vlmax_avl_p (rtx);
uint8_t get_sew (rtx_insn *);
enum vlmul_type get_vlmul (rtx_insn *);
int count_regno_occurrences (rtx_insn *, unsigned int);
bool imm_avl_p (machine_mode);
bool can_be_broadcasted_p (rtx);
bool gather_scatter_valid_offset_p (machine_mode);
HOST_WIDE_INT estimated_poly_value (poly_int64, unsigned int);
bool whole_reg_to_reg_move_p (rtx *, machine_mode, int);
bool splat_to_scalar_move_p (rtx *);
}

/* We classify builtin types into two classes:
   1. General builtin class which is defined in riscv_builtins.
   2. Vector builtin class which is a special builtin architecture
      that implement intrinsic short into "pragma".  */
enum riscv_builtin_class
{
  RISCV_BUILTIN_GENERAL,
  RISCV_BUILTIN_VECTOR
};

const unsigned int RISCV_BUILTIN_SHIFT = 1;

/* Mask that selects the riscv_builtin_class part of a function code.  */
const unsigned int RISCV_BUILTIN_CLASS = (1 << RISCV_BUILTIN_SHIFT) - 1;

/* Routines implemented in riscv-string.cc.  */
extern bool riscv_expand_strcmp (rtx, rtx, rtx, rtx, rtx);
extern bool riscv_expand_strlen (rtx, rtx, rtx, rtx);

/* Routines implemented in thead.cc.  */
extern bool extract_base_offset_in_addr (rtx, rtx *, rtx *);
extern bool th_mempair_operands_p (rtx[4], bool, machine_mode);
extern void th_mempair_order_operands (rtx[4], bool, machine_mode);
extern void th_mempair_prepare_save_restore_operands (rtx[4], bool,
						      machine_mode,
						      int, HOST_WIDE_INT,
						      int, HOST_WIDE_INT);
extern void th_mempair_save_restore_regs (rtx[4], bool, machine_mode);
extern unsigned int th_int_get_mask (unsigned int);
extern unsigned int th_int_get_save_adjustment (void);
extern rtx th_int_adjust_cfi_prologue (unsigned int);
extern const char *th_asm_output_opcode (FILE *asm_out_file, const char *p);
#ifdef RTX_CODE
extern const char*
th_mempair_output_move (rtx[4], bool, machine_mode, RTX_CODE);
extern bool th_memidx_legitimate_modify_p (rtx);
extern bool th_memidx_legitimate_modify_p (rtx, bool);
extern bool th_memidx_legitimate_index_p (rtx);
extern bool th_memidx_legitimate_index_p (rtx, bool);
extern bool th_classify_address (struct riscv_address_info *,
					rtx, machine_mode, bool);
extern const char *th_output_move (rtx, rtx);
extern bool th_print_operand_address (FILE *, machine_mode, rtx);
#endif

extern bool riscv_use_divmod_expander (void);
void riscv_init_cumulative_args (CUMULATIVE_ARGS *, tree, rtx, tree, int);
extern bool
riscv_option_valid_attribute_p (tree, tree, tree, int);
extern void
riscv_override_options_internal (struct gcc_options *);
extern void riscv_option_override (void);

struct riscv_tune_param;
/* Information about one micro-arch we know about.  */
struct riscv_tune_info {
  /* This micro-arch canonical name.  */
  const char *name;

  /* Which automaton to use for tuning.  */
  enum riscv_microarchitecture_type microarchitecture;

  /* Tuning parameters for this micro-arch.  */
  const struct riscv_tune_param *tune_param;
};

const struct riscv_tune_info *
riscv_parse_tune (const char *, bool);
const cpu_vector_cost *get_vector_costs ();

enum
{
  RISCV_MAJOR_VERSION_BASE = 1000000,
  RISCV_MINOR_VERSION_BASE = 1000,
  RISCV_REVISION_VERSION_BASE = 1,
};

#endif /* ! GCC_RISCV_PROTOS_H */
