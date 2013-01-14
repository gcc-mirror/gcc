/* Machine description for AArch64 architecture.
   Copyright (C) 2009-2013 Free Software Foundation, Inc.
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
  SYMBOL_FORCE_TO_MEM
};

/* A set of tuning parameters contains references to size and time
   cost models and vectors for address cost calculations, register
   move costs and memory move costs.  */

/* Extra costs for specific insns.  Only records the cost above a
   single insn.  */

struct cpu_rtx_cost_table
{
  const int memory_load;
  const int memory_store;
  const int register_shift;
  const int int_divide;
  const int float_divide;
  const int double_divide;
  const int int_multiply;
  const int int_multiply_extend;
  const int int_multiply_add;
  const int int_multiply_extend_add;
  const int float_multiply;
  const int double_multiply;
};

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

struct tune_params
{
  const struct cpu_rtx_cost_table *const insn_extra_cost;
  const struct cpu_addrcost_table *const addr_cost;
  const struct cpu_regmove_cost *const regmove_cost;
  const int memmov_cost;
};

HOST_WIDE_INT aarch64_initial_elimination_offset (unsigned, unsigned);
bool aarch64_bitmask_imm (HOST_WIDE_INT val, enum machine_mode);
bool aarch64_constant_address_p (rtx);
bool aarch64_float_const_zero_rtx_p (rtx);
bool aarch64_function_arg_regno_p (unsigned);
bool aarch64_gen_movmemqi (rtx *);
bool aarch64_is_extend_from_extract (enum machine_mode, rtx, rtx);
bool aarch64_is_long_call_p (rtx);
bool aarch64_label_mentioned_p (rtx);
bool aarch64_legitimate_pic_operand_p (rtx);
bool aarch64_move_imm (HOST_WIDE_INT, enum machine_mode);
bool aarch64_pad_arg_upward (enum machine_mode, const_tree);
bool aarch64_pad_reg_upward (enum machine_mode, const_tree, bool);
bool aarch64_regno_ok_for_base_p (int, bool);
bool aarch64_regno_ok_for_index_p (int, bool);
bool aarch64_simd_imm_scalar_p (rtx x, enum machine_mode mode);
bool aarch64_simd_imm_zero_p (rtx, enum machine_mode);
bool aarch64_simd_shift_imm_p (rtx, enum machine_mode, bool);
bool aarch64_symbolic_address_p (rtx);
bool aarch64_symbolic_constant_p (rtx, enum aarch64_symbol_context,
				  enum aarch64_symbol_type *);
bool aarch64_uimm12_shift (HOST_WIDE_INT);
const char *aarch64_output_casesi (rtx *);
enum aarch64_symbol_type aarch64_classify_symbol (rtx,
						  enum aarch64_symbol_context);
enum aarch64_symbol_type aarch64_classify_tls_symbol (rtx);
enum reg_class aarch64_regno_regclass (unsigned);
int aarch64_asm_preferred_eh_data_format (int, int);
int aarch64_hard_regno_mode_ok (unsigned, enum machine_mode);
int aarch64_hard_regno_nregs (unsigned, enum machine_mode);
int aarch64_simd_attr_length_move (rtx);
int aarch64_simd_immediate_valid_for_move (rtx, enum machine_mode, rtx *,
					   int *, unsigned char *, int *,
					   int *);
int aarch64_uxt_size (int, HOST_WIDE_INT);
rtx aarch64_final_eh_return_addr (void);
rtx aarch64_legitimize_reload_address (rtx *, enum machine_mode, int, int, int);
const char *aarch64_output_move_struct (rtx *operands);
rtx aarch64_return_addr (int, rtx);
rtx aarch64_simd_gen_const_vector_dup (enum machine_mode, int);
bool aarch64_simd_mem_operand_p (rtx);
rtx aarch64_simd_vect_par_cnst_half (enum machine_mode, bool);
rtx aarch64_tls_get_addr (void);
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

char* aarch64_output_simd_mov_immediate (rtx *, enum machine_mode, unsigned);
#endif /* GCC_AARCH64_PROTOS_H */
