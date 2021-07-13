/* Definitions of target machine for GNU compiler, for IBM RS/6000.
   Copyright (C) 2000-2021 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@vlsi1.ultra.nyu.edu)

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

#ifndef GCC_RS6000_PROTOS_H
#define GCC_RS6000_PROTOS_H

/* Declare functions in rs6000.c */

#ifdef RTX_CODE

#ifdef TREE_CODE
extern void init_cumulative_args (CUMULATIVE_ARGS *, tree, rtx, int, int, int,
				  tree, machine_mode);
#endif /* TREE_CODE */

extern bool easy_altivec_constant (rtx, machine_mode);
extern bool xxspltib_constant_p (rtx, machine_mode, int *, int *);
extern int vspltis_shifted (rtx);
extern HOST_WIDE_INT const_vector_elt_as_int (rtx, unsigned int);
extern bool macho_lo_sum_memory_operand (rtx, machine_mode);
extern int num_insns_constant (rtx, machine_mode);
extern int small_data_operand (rtx, machine_mode);
extern bool mem_operand_gpr (rtx, machine_mode);
extern bool mem_operand_ds_form (rtx, machine_mode);
extern bool toc_relative_expr_p (const_rtx, bool, const_rtx *, const_rtx *);
extern void validate_condition_mode (enum rtx_code, machine_mode);
extern bool legitimate_constant_pool_address_p (const_rtx, machine_mode,
						bool);
extern bool legitimate_indirect_address_p (rtx, int);
extern bool legitimate_indexed_address_p (rtx, int);
extern bool avoiding_indexed_address_p (machine_mode);
extern rtx rs6000_force_indexed_or_indirect_mem (rtx x);

extern rtx rs6000_got_register (rtx);
extern rtx find_addr_reg (rtx);
extern rtx gen_easy_altivec_constant (rtx);
extern const char *output_vec_const_move (rtx *);
extern const char *rs6000_output_move_128bit (rtx *);
extern bool rs6000_move_128bit_ok_p (rtx []);
extern bool rs6000_split_128bit_ok_p (rtx []);
extern void rs6000_expand_float128_convert (rtx, rtx, bool);
extern void rs6000_expand_vector_init (rtx, rtx);
extern void rs6000_expand_vector_set (rtx, rtx, rtx);
extern void rs6000_expand_vector_extract (rtx, rtx, rtx);
extern void rs6000_split_vec_extract_var (rtx, rtx, rtx, rtx, rtx);
extern rtx rs6000_adjust_vec_address (rtx, rtx, rtx, rtx, machine_mode);
extern void altivec_expand_vec_perm_le (rtx op[4]);
extern void rs6000_expand_extract_even (rtx, rtx, rtx);
extern void rs6000_expand_interleave (rtx, rtx, rtx, bool);
extern void rs6000_scale_v2df (rtx, rtx, int);
extern void rs6000_generate_float2_code (bool, rtx, rtx, rtx);
extern void rs6000_generate_float2_double_code (rtx, rtx, rtx);
extern void rs6000_generate_vsigned2_code (bool, rtx, rtx, rtx);
extern int expand_block_clear (rtx[]);
extern int expand_block_move (rtx[], bool);
extern bool expand_block_compare (rtx[]);
extern bool expand_strn_compare (rtx[], int);
extern bool rs6000_is_valid_mask (rtx, int *, int *, machine_mode);
extern bool rs6000_is_valid_and_mask (rtx, machine_mode);
extern bool rs6000_is_valid_shift_mask (rtx, rtx, machine_mode);
extern bool rs6000_is_valid_insert_mask (rtx, rtx, machine_mode);
extern const char *rs6000_insn_for_and_mask (machine_mode, rtx *, bool);
extern const char *rs6000_insn_for_shift_mask (machine_mode, rtx *, bool);
extern const char *rs6000_insn_for_insert_mask (machine_mode, rtx *, bool);
extern bool rs6000_is_valid_2insn_and (rtx, machine_mode);
extern void rs6000_emit_2insn_and (machine_mode, rtx *, bool, int);
extern int registers_ok_for_quad_peep (rtx, rtx);
extern int mems_ok_for_quad_peep (rtx, rtx);
extern bool gpr_or_gpr_p (rtx, rtx);
extern bool direct_move_p (rtx, rtx);
extern bool quad_address_p (rtx, machine_mode, bool);
extern bool quad_load_store_p (rtx, rtx);
extern bool fusion_gpr_load_p (rtx, rtx, rtx, rtx);
extern void expand_fusion_gpr_load (rtx *);
extern void emit_fusion_addis (rtx, rtx);
extern const char *emit_fusion_gpr_load (rtx, rtx);
extern enum reg_class (*rs6000_preferred_reload_class_ptr) (rtx,
							    enum reg_class);
extern enum reg_class (*rs6000_secondary_reload_class_ptr) (enum reg_class,
							    machine_mode,
							    rtx);
extern void rs6000_secondary_reload_inner (rtx, rtx, rtx, bool);
extern void rs6000_secondary_reload_gpr (rtx, rtx, rtx, bool);


extern int ccr_bit (rtx, int);
extern void rs6000_output_function_entry (FILE *, const char *);
extern void print_operand (FILE *, rtx, int);
extern void print_operand_address (FILE *, rtx);
extern const char *rs6000_call_template (rtx *, unsigned int);
extern const char *rs6000_sibcall_template (rtx *, unsigned int);
extern const char *rs6000_indirect_call_template (rtx *, unsigned int);
extern const char *rs6000_indirect_sibcall_template (rtx *, unsigned int);
extern const char *rs6000_pltseq_template (rtx *, int);
extern enum rtx_code rs6000_reverse_condition (machine_mode,
					       enum rtx_code);
extern rtx rs6000_emit_eqne (machine_mode, rtx, rtx, rtx);
extern rtx rs6000_emit_fp_cror (rtx_code, machine_mode, rtx);
extern void rs6000_emit_sCOND (machine_mode, rtx[]);
extern void rs6000_emit_cbranch (machine_mode, rtx[]);
extern char * output_cbranch (rtx, const char *, int, rtx_insn *);
extern const char * output_probe_stack_range (rtx, rtx, rtx);
extern void rs6000_emit_dot_insn (rtx dst, rtx src, int dot, rtx ccreg);
extern bool rs6000_emit_set_const (rtx, rtx);
extern bool rs6000_emit_cmove (rtx, rtx, rtx, rtx);
extern bool rs6000_emit_int_cmove (rtx, rtx, rtx, rtx);
extern int rs6000_emit_vector_cond_expr (rtx, rtx, rtx, rtx, rtx, rtx);
extern void rs6000_emit_minmax (rtx, enum rtx_code, rtx, rtx);
extern void rs6000_expand_atomic_compare_and_swap (rtx op[]);
extern rtx swap_endian_selector_for_mode (machine_mode mode);

extern void rs6000_expand_atomic_exchange (rtx op[]);
extern void rs6000_expand_atomic_op (enum rtx_code, rtx, rtx, rtx, rtx, rtx);
extern void rs6000_emit_swdiv (rtx, rtx, rtx, bool);
extern void rs6000_emit_swsqrt (rtx, rtx, bool);
extern void output_toc (FILE *, rtx, int, machine_mode);
extern void rs6000_fatal_bad_address (rtx);
extern rtx create_TOC_reference (rtx, rtx);
extern void rs6000_split_multireg_move (rtx, rtx);
extern void rs6000_emit_le_vsx_permute (rtx, rtx, machine_mode);
extern void rs6000_emit_le_vsx_move (rtx, rtx, machine_mode);
extern bool valid_sf_si_move (rtx, rtx, machine_mode);
extern void rs6000_emit_move (rtx, rtx, machine_mode);
extern bool rs6000_legitimate_offset_address_p (machine_mode, rtx,
						bool, bool);
extern rtx rs6000_find_base_term (rtx);
extern rtx rs6000_return_addr (int, rtx);
extern void rs6000_output_symbol_ref (FILE*, rtx);
extern HOST_WIDE_INT rs6000_initial_elimination_offset (int, int);
extern void rs6000_emit_popcount (rtx, rtx);
extern void rs6000_emit_parity (rtx, rtx);

extern rtx rs6000_machopic_legitimize_pic_address (rtx, machine_mode,
						   rtx);
extern rtx rs6000_allocate_stack_temp (machine_mode, bool, bool);
extern align_flags rs6000_loop_align (rtx);
extern void rs6000_split_logical (rtx [], enum rtx_code, bool, bool, bool);
extern bool rs6000_function_pcrel_p (struct function *);
extern bool rs6000_pcrel_p (void);
extern bool rs6000_fndecl_pcrel_p (const_tree);
extern void rs6000_output_addr_vec_elt (FILE *, int);

/* Different PowerPC instruction formats that are used by GCC.  There are
   various other instruction formats used by the PowerPC hardware, but these
   formats are not currently used by GCC.  */

enum insn_form {
  INSN_FORM_BAD,		/* Bad instruction format.  */
  INSN_FORM_BASE_REG,		/* Base register only.  */
  INSN_FORM_D,			/* Reg + 16-bit numeric offset.  */
  INSN_FORM_DS,			/* Reg + offset, bottom 2 bits must be 0.  */
  INSN_FORM_DQ,			/* Reg + offset, bottom 4 bits must be 0.  */
  INSN_FORM_X,			/* Base register + index register.  */
  INSN_FORM_UPDATE,		/* Address updates base register.  */
  INSN_FORM_LO_SUM,		/* Reg + offset using symbol.  */
  INSN_FORM_PREFIXED_NUMERIC,	/* Reg + 34 bit numeric offset.  */
  INSN_FORM_PCREL_LOCAL,	/* PC-relative local symbol.  */
  INSN_FORM_PCREL_EXTERNAL	/* PC-relative external symbol.  */
};

/* Instruction format for the non-prefixed version of a load or store.  This is
   used to determine if a 16-bit offset is valid to be used with a non-prefixed
   (traditional) instruction or if the bottom bits of the offset cannot be used
   with a DS or DQ instruction format, and GCC has to use a prefixed
   instruction for the load or store.  */

enum non_prefixed_form {
  NON_PREFIXED_DEFAULT,		/* Use the default.  */
  NON_PREFIXED_D,		/* All 16-bits are valid.  */
  NON_PREFIXED_DS,		/* Bottom 2 bits must be 0.  */
  NON_PREFIXED_DQ,		/* Bottom 4 bits must be 0.  */
  NON_PREFIXED_X		/* No offset memory form exists.  */
};

extern enum insn_form address_to_insn_form (rtx, machine_mode,
					    enum non_prefixed_form);
extern bool address_is_non_pfx_d_or_x (rtx addr, machine_mode mode,
				       enum non_prefixed_form non_prefix_format);
extern bool pcrel_opt_valid_mem_p (rtx, machine_mode, rtx);
enum non_prefixed_form reg_to_non_prefixed (rtx reg, machine_mode mode);
extern bool prefixed_load_p (rtx_insn *);
extern bool prefixed_store_p (rtx_insn *);
extern bool prefixed_paddi_p (rtx_insn *);
extern void rs6000_asm_output_opcode (FILE *);
extern void output_pcrel_opt_reloc (rtx);
extern void rs6000_final_prescan_insn (rtx_insn *, rtx [], int);
extern int rs6000_adjust_insn_length (rtx_insn *, int);

/* Return true if the address can be used for a prefixed load, store, or add
   immediate instructions that cannot be used with a non-prefixed instruction.
   For example, using a numeric offset that is not valid for the non-prefixed
   instruction or a PC-relative reference to a local symbol would return true,
   but an address with an offset of 64 would not return true.

   References to external PC-relative symbols aren't allowed, because GCC has
   to load the address into a register and then issue a separate load or
   store.  */

static inline bool
address_is_prefixed (rtx addr,
		     machine_mode mode,
		     enum non_prefixed_form non_prefixed)
{
  enum insn_form iform = address_to_insn_form (addr, mode, non_prefixed);
  return (iform == INSN_FORM_PREFIXED_NUMERIC
	  || iform == INSN_FORM_PCREL_LOCAL);
}
#endif /* RTX_CODE */

#ifdef TREE_CODE
extern unsigned int rs6000_data_alignment (tree, unsigned int, enum data_align);
extern unsigned int rs6000_special_adjust_field_align (tree, unsigned int);
extern unsigned int rs6000_special_round_type_align (tree, unsigned int,
						     unsigned int);
extern unsigned int darwin_rs6000_special_round_type_align (tree, unsigned int,
							    unsigned int);
extern tree altivec_resolve_overloaded_builtin (location_t, tree, void *);
extern rtx rs6000_libcall_value (machine_mode);
extern rtx rs6000_va_arg (tree, tree);
extern int function_ok_for_sibcall (tree);
extern int rs6000_reg_parm_stack_space (tree, bool);
extern void rs6000_asm_weaken_decl (FILE *, tree, const char *, const char *);
extern void rs6000_xcoff_declare_function_name (FILE *, const char *, tree);
extern void rs6000_xcoff_declare_object_name (FILE *, const char *, tree);
extern void rs6000_xcoff_asm_output_aligned_decl_common (FILE *, tree,
							 const char *,
							 unsigned HOST_WIDE_INT,
							 unsigned int);
extern void rs6000_elf_declare_function_name (FILE *, const char *, tree);
extern bool rs6000_elf_in_small_data_p (const_tree);

#endif /* TREE_CODE */

extern int direct_return (void);
extern int first_reg_to_save (void);
extern int first_fp_reg_to_save (void);
extern void output_ascii (FILE *, const char *, int);
extern void rs6000_gen_section_name (char **, const char *, const char *);
extern void output_function_profiler (FILE *, int);
extern void output_profile_hook  (int);
extern int rs6000_trampoline_size (void);
extern alias_set_type get_TOC_alias_set (void);
extern void rs6000_emit_prologue (void);
extern void rs6000_emit_load_toc_table (int);
extern unsigned int rs6000_dbx_register_number (unsigned int, unsigned int);
extern void rs6000_emit_epilogue (enum epilogue_type);
extern void rs6000_expand_split_stack_prologue (void);
extern void rs6000_split_stack_space_check (rtx, rtx);
extern void rs6000_emit_eh_reg_restore (rtx, rtx);
extern void rs6000_call_aix (rtx, rtx, rtx, rtx);
extern void rs6000_sibcall_aix (rtx, rtx, rtx, rtx);
extern void rs6000_call_sysv (rtx, rtx, rtx, rtx);
extern void rs6000_sibcall_sysv (rtx, rtx, rtx, rtx);
extern void rs6000_call_darwin (rtx, rtx, rtx, rtx);
extern void rs6000_sibcall_darwin (rtx, rtx, rtx, rtx);
extern void rs6000_aix_asm_output_dwarf_table_ref (char *);
extern void get_ppc476_thunk_name (char name[32]);
extern bool rs6000_overloaded_builtin_p (enum rs6000_builtins);
extern bool rs6000_builtin_is_supported_p (enum rs6000_builtins);
extern const char *rs6000_overloaded_builtin_name (enum rs6000_builtins);
extern int rs6000_store_data_bypass_p (rtx_insn *, rtx_insn *);
extern HOST_WIDE_INT rs6000_builtin_mask_calculate (void);
extern void rs6000_asm_output_dwarf_pcrel (FILE *file, int size,
					   const char *label);
extern void rs6000_asm_output_dwarf_datarel (FILE *file, int size,
					     const char *label);
extern long rs6000_const_f32_to_i32 (rtx operand);

/* Declare functions in rs6000-c.c */

extern void rs6000_pragma_longcall (struct cpp_reader *);
extern void rs6000_cpu_cpp_builtins (struct cpp_reader *);
#ifdef TREE_CODE
extern bool rs6000_pragma_target_parse (tree, tree);
#endif
extern void rs6000_activate_target_options (tree new_tree);
extern void rs6000_target_modify_macros (bool, HOST_WIDE_INT, HOST_WIDE_INT);
extern void (*rs6000_target_modify_macros_ptr) (bool, HOST_WIDE_INT,
						HOST_WIDE_INT);

/* Declare functions in rs6000-d.c  */
extern void rs6000_d_target_versions (void);
extern void rs6000_d_register_target_info (void);

#ifdef NO_DOLLAR_IN_LABEL
const char * rs6000_xcoff_strip_dollar (const char *);
#endif

extern unsigned char rs6000_class_max_nregs[][LIM_REG_CLASSES];
extern unsigned char rs6000_hard_regno_nregs[][FIRST_PSEUDO_REGISTER];

extern bool rs6000_linux_float_exceptions_rounding_supported_p (void);

/* Pass management.  */
namespace gcc { class context; }
class rtl_opt_pass;

extern rtl_opt_pass *make_pass_analyze_swaps (gcc::context *);
extern rtl_opt_pass *make_pass_pcrel_opt (gcc::context *);
extern bool rs6000_sum_of_two_registers_p (const_rtx expr);
extern bool rs6000_quadword_masked_address_p (const_rtx exp);
extern rtx rs6000_gen_lvx (enum machine_mode, rtx, rtx);
extern rtx rs6000_gen_stvx (enum machine_mode, rtx, rtx);

extern void rs6000_emit_xxspltidp_v2df (rtx, long value);
#endif  /* rs6000-protos.h */
