/* Definition of RISC-V target for GNU compiler.
   Copyright (C) 2011-2023 Free Software Foundation, Inc.
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
  SYMBOL_PCREL,
  SYMBOL_GOT_DISP,
  SYMBOL_TLS,
  SYMBOL_TLS_LE,
  SYMBOL_TLS_IE,
  SYMBOL_TLS_GD
};
#define NUM_SYMBOL_TYPES (SYMBOL_TLS_GD + 1)

/* Classifies an address.

   ADDRESS_REG
       A natural register + offset address.  The register satisfies
       riscv_valid_base_register_p and the offset is a const_arith_operand.

   ADDRESS_LO_SUM
       A LO_SUM rtx.  The first operand is a valid base register and
       the second operand is a symbolic address.

   ADDRESS_CONST_INT
       A signed 16-bit constant address.

   ADDRESS_SYMBOLIC:
       A constant symbolic address.  */
enum riscv_address_type {
  ADDRESS_REG,
  ADDRESS_LO_SUM,
  ADDRESS_CONST_INT,
  ADDRESS_SYMBOLIC
};

/* Information about an address described by riscv_address_type.

   ADDRESS_CONST_INT
       No fields are used.

   ADDRESS_REG
       REG is the base register and OFFSET is the constant offset.

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
};

/* Routines implemented in riscv.cc.  */
extern enum riscv_symbol_type riscv_classify_symbolic_expression (rtx);
extern bool riscv_symbolic_constant_p (rtx, enum riscv_symbol_type *);
extern int riscv_regno_mode_ok_for_base_p (int, machine_mode, bool);
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

#ifdef RTX_CODE
extern void riscv_expand_int_scc (rtx, enum rtx_code, rtx, rtx);
extern void riscv_expand_float_scc (rtx, enum rtx_code, rtx, rtx);
extern void riscv_expand_conditional_branch (rtx, enum rtx_code, rtx, rtx);
extern rtx riscv_emit_binary (enum rtx_code code, rtx dest, rtx x, rtx y);
#endif
extern bool riscv_expand_conditional_move (rtx, rtx, rtx, rtx);
extern rtx riscv_legitimize_call_address (rtx);
extern void riscv_set_return_address (rtx, rtx);
extern bool riscv_expand_block_move (rtx, rtx, rtx);
extern rtx riscv_return_addr (int, rtx);
extern poly_int64 riscv_initial_elimination_offset (int, int);
extern void riscv_expand_prologue (void);
extern void riscv_expand_epilogue (int);
extern bool riscv_epilogue_uses (unsigned int);
extern bool riscv_can_use_return_insn (void);
extern rtx riscv_function_value (const_tree, const_tree, enum machine_mode);
extern bool riscv_expand_block_move (rtx, rtx, rtx);
extern bool riscv_store_data_bypass_p (rtx_insn *, rtx_insn *);
extern rtx riscv_gen_gpr_save_insn (struct riscv_frame_info *);
extern bool riscv_gpr_save_operation_p (rtx);
extern void riscv_reinit (void);
extern poly_uint64 riscv_regmode_natural_size (machine_mode);
extern bool riscv_v_ext_vector_mode_p (machine_mode);
extern bool riscv_v_ext_tuple_mode_p (machine_mode);
extern bool riscv_v_ext_vls_mode_p (machine_mode);
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
rtl_opt_pass * make_pass_vsetvl (gcc::context *ctxt);

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

/* Routines implemented in riscv-selftests.cc.  */
#if CHECKING_P
namespace selftest {
void riscv_run_selftests (void);
} // namespace selftest
#endif

namespace riscv_vector {
#define RVV_VLMAX gen_rtx_REG (Pmode, X0_REGNUM)
#define RVV_VUNDEF(MODE)                                                       \
  gen_rtx_UNSPEC (MODE, gen_rtvec (1, gen_rtx_REG (SImode, X0_REGNUM)),        \
		  UNSPEC_VUNDEF)
enum insn_type
{
  RVV_MISC_OP = 1,
  RVV_UNOP = 2,
  RVV_BINOP = 3,
  RVV_BINOP_MU = RVV_BINOP + 2,
  RVV_BINOP_TU = RVV_BINOP + 2,
  RVV_MERGE_OP = 4,
  RVV_CMP_OP = 4,
  RVV_CMP_MU_OP = RVV_CMP_OP + 2, /* +2 means mask and maskoff operand.  */
  RVV_UNOP_MU = RVV_UNOP + 2,	  /* Likewise.  */
  RVV_UNOP_M = RVV_UNOP + 2,	  /* Likewise.  */
  RVV_TERNOP = 5,
  RVV_TERNOP_TU = RVV_TERNOP + 1,
  RVV_WIDEN_TERNOP = 4,
  RVV_SCALAR_MOV_OP = 4, /* +1 for VUNDEF according to vector.md.  */
  RVV_SLIDE_OP = 4,      /* Dest, VUNDEF, source and offset.  */
  RVV_COMPRESS_OP = 4,
  RVV_GATHER_M_OP = 5,
  RVV_SCATTER_M_OP = 4,
  RVV_REDUCTION_OP = 3,
  RVV_REDUCTION_TU_OP = RVV_REDUCTION_OP + 2,
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

enum avl_type
{
  NONVLMAX,
  VLMAX,
};
/* Routines implemented in riscv-vector-builtins.cc.  */
void init_builtins (void);
const char *mangle_builtin_type (const_tree);
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
bool const_vec_all_same_in_range_p (rtx, HOST_WIDE_INT, HOST_WIDE_INT);
bool legitimize_move (rtx, rtx);
void emit_vlmax_vsetvl (machine_mode, rtx);
void emit_hard_vlmax_vsetvl (machine_mode, rtx);
void emit_vlmax_insn (unsigned, int, rtx *, rtx = 0);
void emit_vlmax_fp_insn (unsigned, int, rtx *, rtx = 0);
void emit_vlmax_ternary_insn (unsigned, int, rtx *, rtx = 0);
void emit_vlmax_fp_ternary_insn (unsigned, int, rtx *, rtx = 0);
void emit_nonvlmax_insn (unsigned, int, rtx *, rtx);
void emit_vlmax_slide_insn (unsigned, rtx *);
void emit_nonvlmax_slide_tu_insn (unsigned, rtx *, rtx);
void emit_vlmax_merge_insn (unsigned, int, rtx *);
void emit_vlmax_cmp_insn (unsigned, rtx *);
void emit_vlmax_cmp_mu_insn (unsigned, rtx *);
void emit_vlmax_masked_mu_insn (unsigned, int, rtx *);
void emit_scalar_move_insn (unsigned, rtx *, rtx = 0);
void emit_nonvlmax_integer_move_insn (unsigned, rtx *, rtx);
enum vlmul_type get_vlmul (machine_mode);
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

enum class reduction_type
{
  UNORDERED,
  FOLD_LEFT,
  MASK_LEN_FOLD_LEFT,
};
enum tail_policy get_prefer_tail_policy ();
enum mask_policy get_prefer_mask_policy ();
rtx get_avl_type_rtx (enum avl_type);
opt_machine_mode get_vector_mode (scalar_mode, poly_uint64);
opt_machine_mode get_tuple_mode (machine_mode, unsigned int);
bool simm5_p (rtx);
bool neg_simm5_p (rtx);
#ifdef RTX_CODE
bool has_vi_variant_p (rtx_code, rtx);
void expand_vec_cmp (rtx, rtx_code, rtx, rtx);
bool expand_vec_cmp_float (rtx, rtx_code, rtx, rtx, bool);
void expand_cond_len_binop (rtx_code, rtx *);
void expand_reduction (rtx_code, rtx *, rtx,
		       reduction_type = reduction_type::UNORDERED);
#endif
bool sew64_scalar_helper (rtx *, rtx *, rtx, machine_mode,
			  bool, void (*)(rtx *, rtx));
rtx gen_scalar_move_mask (machine_mode);

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
machine_mode preferred_simd_mode (scalar_mode);
machine_mode get_mask_mode (machine_mode);
void expand_vec_series (rtx, rtx, rtx);
void expand_vec_init (rtx, rtx);
void expand_vec_perm (rtx, rtx, rtx, rtx);
void expand_select_vl (rtx *);
void expand_load_store (rtx *, bool);
void expand_gather_scatter (rtx *, bool);
void expand_cond_len_ternop (unsigned, rtx *);

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
};

opt_machine_mode vectorize_related_mode (machine_mode, scalar_mode,
					 poly_uint64);
unsigned int autovectorize_vector_modes (vec<machine_mode> *, bool);
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

/* Routines implemented in thead.cc.  */
extern bool th_mempair_operands_p (rtx[4], bool, machine_mode);
extern void th_mempair_order_operands (rtx[4], bool, machine_mode);
extern void th_mempair_prepare_save_restore_operands (rtx[4], bool,
						      machine_mode,
						      int, HOST_WIDE_INT,
						      int, HOST_WIDE_INT);
extern void th_mempair_save_restore_regs (rtx[4], bool, machine_mode);
#ifdef RTX_CODE
extern const char*
th_mempair_output_move (rtx[4], bool, machine_mode, RTX_CODE);
#endif

extern bool riscv_use_divmod_expander (void);
void riscv_init_cumulative_args (CUMULATIVE_ARGS *, tree, rtx, tree, int);

#endif /* ! GCC_RISCV_PROTOS_H */
