/* Machine description for AArch64 architecture.
   Copyright (C) 2009-2014 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

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


#ifndef GCC_AARCH64_PROTOS_H
#define GCC_AARCH64_PROTOS_H

/*
  SYMBOL_CONTEXT_ADR
  The symbol is used in a load-address operation.
  SYMBOL_CONTEXT_MEM
  The symbol is used as the address in a MEM.
 */
enum aarch64_symbol_context
{
  SYMBOL_CONTEXT_MEM,
  SYMBOL_CONTEXT_ADR
};

/* SYMBOL_SMALL_ABSOLUTE: Generate symbol accesses through
   high and lo relocs that calculate the base address using a PC
   relative reloc.
   So to get the address of foo, we generate
   adrp x0, foo
   add  x0, x0, :lo12:foo

   To load or store something to foo, we could use the corresponding
   load store variants that generate an
   ldr x0, [x0,:lo12:foo]
   or
   str x1, [x0, :lo12:foo]

   This corresponds to the small code model of the compiler.

   SYMBOL_SMALL_GOT: Similar to the one above but this
   gives us the GOT entry of the symbol being referred to :
   Thus calculating the GOT entry for foo is done using the
   following sequence of instructions.  The ADRP instruction
   gets us to the page containing the GOT entry of the symbol
   and the got_lo12 gets us the actual offset in it.

   adrp  x0, :got:foo
   ldr   x0, [x0, :gotoff_lo12:foo]

   This corresponds to the small PIC model of the compiler.

   SYMBOL_SMALL_TLSGD
   SYMBOL_SMALL_TLSDESC
   SYMBOL_SMALL_GOTTPREL
   SYMBOL_SMALL_TPREL
   Each of of these represents a thread-local symbol, and corresponds to the
   thread local storage relocation operator for the symbol being referred to.

   SYMBOL_TINY_ABSOLUTE

   Generate symbol accesses as a PC relative address using a single
   instruction.  To compute the address of symbol foo, we generate:

   ADR x0, foo

   SYMBOL_TINY_GOT

   Generate symbol accesses via the GOT using a single PC relative
   instruction.  To compute the address of symbol foo, we generate:

   ldr t0, :got:foo

   The value of foo can subsequently read using:

   ldrb    t0, [t0]

   SYMBOL_FORCE_TO_MEM : Global variables are addressed using
   constant pool.  All variable addresses are spilled into constant
   pools.  The constant pools themselves are addressed using PC
   relative accesses.  This only works for the large code model.
 */
enum aarch64_symbol_type
{
  SYMBOL_SMALL_ABSOLUTE,
  SYMBOL_SMALL_GOT,
  SYMBOL_SMALL_TLSGD,
  SYMBOL_SMALL_TLSDESC,
  SYMBOL_SMALL_GOTTPREL,
  SYMBOL_SMALL_TPREL,
  SYMBOL_TINY_ABSOLUTE,
  SYMBOL_TINY_GOT,
  SYMBOL_FORCE_TO_MEM
};

/* A set of tuning parameters contains references to size and time
   cost models and vectors for address cost calculations, register
   move costs and memory move costs.  */

/* Additional cost for addresses.  */
struct cpu_addrcost_table
{
  const int pre_modify;
  const int post_modify;
  const int register_offset;
  const int register_extend;
  const int imm_offset;
};

/* Additional costs for register copies.  Cost is for one register.  */
struct cpu_regmove_cost
{
  const int GP2GP;
  const int GP2FP;
  const int FP2GP;
  const int FP2FP;
};

/* Cost for vector insn classes.  */
struct cpu_vector_cost
{
  const int scalar_stmt_cost;		 /* Cost of any scalar operation,
					    excluding load and store.  */
  const int scalar_load_cost;		 /* Cost of scalar load.  */
  const int scalar_store_cost;		 /* Cost of scalar store.  */
  const int vec_stmt_cost;		 /* Cost of any vector operation,
					    excluding load, store,
					    vector-to-scalar and
					    scalar-to-vector operation.  */
  const int vec_to_scalar_cost;		 /* Cost of vec-to-scalar operation.  */
  const int scalar_to_vec_cost;		 /* Cost of scalar-to-vector
					    operation.  */
  const int vec_align_load_cost;	 /* Cost of aligned vector load.  */
  const int vec_unalign_load_cost;	 /* Cost of unaligned vector load.  */
  const int vec_unalign_store_cost;	 /* Cost of unaligned vector store.  */
  const int vec_store_cost;		 /* Cost of vector store.  */
  const int cond_taken_branch_cost;	 /* Cost of taken branch.  */
  const int cond_not_taken_branch_cost;  /* Cost of not taken branch.  */
};

struct tune_params
{
  const struct cpu_cost_table *const insn_extra_cost;
  const struct cpu_addrcost_table *const addr_cost;
  const struct cpu_regmove_cost *const regmove_cost;
  const struct cpu_vector_cost *const vec_costs;
  const int memmov_cost;
  const int issue_rate;
};

HOST_WIDE_INT aarch64_initial_elimination_offset (unsigned, unsigned);
bool aarch64_bitmask_imm (HOST_WIDE_INT val, enum machine_mode);
bool aarch64_cannot_change_mode_class (enum machine_mode,
				       enum machine_mode,
				       enum reg_class);
enum aarch64_symbol_type
aarch64_classify_symbolic_expression (rtx, enum aarch64_symbol_context);
bool aarch64_constant_address_p (rtx);
bool aarch64_float_const_zero_rtx_p (rtx);
bool aarch64_function_arg_regno_p (unsigned);
bool aarch64_gen_movmemqi (rtx *);
bool aarch64_gimple_fold_builtin (gimple_stmt_iterator *);
bool aarch64_is_extend_from_extract (enum machine_mode, rtx, rtx);
bool aarch64_is_long_call_p (rtx);
bool aarch64_label_mentioned_p (rtx);
bool aarch64_legitimate_pic_operand_p (rtx);
bool aarch64_modes_tieable_p (enum machine_mode mode1,
			      enum machine_mode mode2);
bool aarch64_move_imm (HOST_WIDE_INT, enum machine_mode);
bool aarch64_mov_operand_p (rtx, enum aarch64_symbol_context,
			    enum machine_mode);
char *aarch64_output_scalar_simd_mov_immediate (rtx, enum machine_mode);
char *aarch64_output_simd_mov_immediate (rtx, enum machine_mode, unsigned);
bool aarch64_pad_arg_upward (enum machine_mode, const_tree);
bool aarch64_pad_reg_upward (enum machine_mode, const_tree, bool);
bool aarch64_regno_ok_for_base_p (int, bool);
bool aarch64_regno_ok_for_index_p (int, bool);
bool aarch64_simd_imm_scalar_p (rtx x, enum machine_mode mode);
bool aarch64_simd_imm_zero_p (rtx, enum machine_mode);
bool aarch64_simd_scalar_immediate_valid_for_move (rtx, enum machine_mode);
bool aarch64_simd_shift_imm_p (rtx, enum machine_mode, bool);
bool aarch64_simd_valid_immediate (rtx, enum machine_mode, bool,
				   struct simd_immediate_info *);
bool aarch64_symbolic_address_p (rtx);
bool aarch64_uimm12_shift (HOST_WIDE_INT);
const char *aarch64_output_casesi (rtx *);
const char *aarch64_rewrite_selected_cpu (const char *name);

enum aarch64_symbol_type aarch64_classify_symbol (rtx,
						  enum aarch64_symbol_context);
enum aarch64_symbol_type aarch64_classify_tls_symbol (rtx);
enum reg_class aarch64_regno_regclass (unsigned);
int aarch64_asm_preferred_eh_data_format (int, int);
int aarch64_hard_regno_mode_ok (unsigned, enum machine_mode);
int aarch64_hard_regno_nregs (unsigned, enum machine_mode);
int aarch64_simd_attr_length_move (rtx);
int aarch64_uxt_size (int, HOST_WIDE_INT);
rtx aarch64_final_eh_return_addr (void);
rtx aarch64_legitimize_reload_address (rtx *, enum machine_mode, int, int, int);
const char *aarch64_output_move_struct (rtx *operands);
rtx aarch64_return_addr (int, rtx);
rtx aarch64_simd_gen_const_vector_dup (enum machine_mode, int);
bool aarch64_simd_mem_operand_p (rtx);
rtx aarch64_simd_vect_par_cnst_half (enum machine_mode, bool);
rtx aarch64_tls_get_addr (void);
tree aarch64_fold_builtin (tree, int, tree *, bool);
unsigned aarch64_dbx_register_number (unsigned);
unsigned aarch64_trampoline_size (void);
void aarch64_asm_output_labelref (FILE *, const char *);
void aarch64_elf_asm_named_section (const char *, unsigned, tree);
void aarch64_expand_epilogue (bool);
void aarch64_expand_mov_immediate (rtx, rtx);
void aarch64_expand_prologue (void);
void aarch64_expand_vector_init (rtx, rtx);
void aarch64_function_profiler (FILE *, int);
void aarch64_init_cumulative_args (CUMULATIVE_ARGS *, const_tree, rtx,
				   const_tree, unsigned);
void aarch64_init_expanders (void);
void aarch64_print_operand (FILE *, rtx, char);
void aarch64_print_operand_address (FILE *, rtx);

/* Initialize builtins for SIMD intrinsics.  */
void init_aarch64_simd_builtins (void);

void aarch64_simd_const_bounds (rtx, HOST_WIDE_INT, HOST_WIDE_INT);
void aarch64_simd_disambiguate_copy (rtx *, rtx *, rtx *, unsigned int);

/* Emit code to place a AdvSIMD pair result in memory locations (with equal
   registers).  */
void aarch64_simd_emit_pair_result_insn (enum machine_mode,
					 rtx (*intfn) (rtx, rtx, rtx), rtx,
					 rtx);

/* Expand builtins for SIMD intrinsics.  */
rtx aarch64_simd_expand_builtin (int, tree, rtx);

void aarch64_simd_lane_bounds (rtx, HOST_WIDE_INT, HOST_WIDE_INT);

/* Emit code for reinterprets.  */
void aarch64_simd_reinterpret (rtx, rtx);

void aarch64_split_128bit_move (rtx, rtx);

bool aarch64_split_128bit_move_p (rtx, rtx);

void aarch64_split_simd_combine (rtx, rtx, rtx);

void aarch64_split_simd_move (rtx, rtx);

/* Check for a legitimate floating point constant for FMOV.  */
bool aarch64_float_const_representable_p (rtx);

#if defined (RTX_CODE)

bool aarch64_legitimate_address_p (enum machine_mode, rtx, RTX_CODE, bool);
enum machine_mode aarch64_select_cc_mode (RTX_CODE, rtx, rtx);
rtx aarch64_gen_compare_reg (RTX_CODE, rtx, rtx);
rtx aarch64_load_tp (rtx);

void aarch64_expand_compare_and_swap (rtx op[]);
void aarch64_split_compare_and_swap (rtx op[]);
void aarch64_split_atomic_op (enum rtx_code, rtx, rtx, rtx, rtx, rtx, rtx);

#endif /* RTX_CODE */

void aarch64_init_builtins (void);
rtx aarch64_expand_builtin (tree exp,
			    rtx target,
			    rtx subtarget ATTRIBUTE_UNUSED,
			    enum machine_mode mode ATTRIBUTE_UNUSED,
			    int ignore ATTRIBUTE_UNUSED);
tree aarch64_builtin_decl (unsigned, bool ATTRIBUTE_UNUSED);

tree
aarch64_builtin_vectorized_function (tree fndecl,
				     tree type_out,
				     tree type_in);

extern void aarch64_split_combinev16qi (rtx operands[3]);
extern void aarch64_expand_vec_perm (rtx target, rtx op0, rtx op1, rtx sel);
extern bool
aarch64_expand_vec_perm_const (rtx target, rtx op0, rtx op1, rtx sel);
#endif /* GCC_AARCH64_PROTOS_H */
