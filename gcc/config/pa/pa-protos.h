/* Prototypes for pa.c functions used in the md file & elsewhere.
   Copyright (C) 2000-2021 Free Software Foundation, Inc.

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

#ifdef RTX_CODE
/* Prototype function used in various macros.  */
extern rtx pa_eh_return_handler_rtx (void);

/* Define functions in pa.c and used in insn-output.c.  */

extern const char *pa_output_and (rtx *);
extern const char *pa_output_64bit_and (rtx *);
extern const char *pa_output_ior (rtx *);
extern const char *pa_output_64bit_ior (rtx *);
extern const char *pa_output_move_double (rtx *);
extern const char *pa_output_fp_move_double (rtx *);
extern const char *pa_output_block_move (rtx *, int);
extern const char *pa_output_block_clear (rtx *, int);
extern const char *pa_output_cbranch (rtx *, int, rtx_insn *);
extern const char *pa_output_lbranch (rtx, rtx_insn *, int);
extern const char *pa_output_bb (rtx *, int, rtx_insn *, int);
extern const char *pa_output_bvb (rtx *, int, rtx_insn *, int);
extern const char *pa_output_dbra (rtx *, rtx_insn *, int);
extern const char *pa_output_movb (rtx *, rtx_insn *, int, int);
extern const char *pa_output_parallel_movb (rtx *, rtx_insn *);
extern const char *pa_output_parallel_addb (rtx *, rtx_insn *);
extern const char *pa_output_call (rtx_insn *, rtx, int);
extern const char *pa_output_indirect_call (rtx_insn *, rtx);
extern const char *pa_output_millicode_call (rtx_insn *, rtx);
extern const char *pa_output_mul_insn (int, rtx_insn *);
extern const char *pa_output_div_insn (rtx *, int, rtx_insn *);
extern const char *pa_output_mod_insn (int, rtx_insn *);
extern const char *pa_singlemove_string (rtx *);
extern void pa_output_addr_vec (rtx, rtx);
extern void pa_output_addr_diff_vec (rtx, rtx);
extern void pa_output_arg_descriptor (rtx_insn *);
extern void pa_output_global_address (FILE *, rtx, int);
extern void pa_print_operand (FILE *, rtx, int);
extern void pa_encode_label (rtx);
extern int pa_symbolic_expression_p (rtx);
extern int pa_adjust_insn_length (rtx_insn *, int);
extern int pa_fmpyaddoperands (rtx *);
extern int pa_fmpysuboperands (rtx *);
extern void pa_emit_bcond_fp (rtx[]);
extern int pa_emit_move_sequence (rtx *, machine_mode, rtx);
extern int pa_emit_hpdiv_const (rtx *, int);
extern int pa_is_function_label_plus_const (rtx);
extern int pa_fpstore_bypass_p (rtx_insn *, rtx_insn *);
extern int pa_attr_length_millicode_call (rtx_insn *);
extern int pa_attr_length_call (rtx_insn *, int);
extern int pa_attr_length_indirect_call (rtx_insn *);
extern rtx pa_legitimize_reload_address (rtx, machine_mode,
					 int, int, int);

/* Declare functions defined in pa.c and used in templates.  */

extern rtx pa_return_addr_rtx (int, rtx);

extern int pa_insn_refs_are_delayed (rtx_insn *);
extern rtx pa_get_deferred_plabel (rtx);
extern rtx pa_maybe_emit_compare_and_swap_exchange_loop (rtx, rtx, rtx);
#endif /* RTX_CODE */

extern int pa_and_mask_p (unsigned HOST_WIDE_INT);
extern int pa_cint_ok_for_move (unsigned HOST_WIDE_INT);
extern int pa_ior_mask_p (unsigned HOST_WIDE_INT);
extern int pa_ldil_cint_p (unsigned HOST_WIDE_INT);
extern int pa_mem_shadd_constant_p (int);
extern int pa_shadd_constant_p (int);
extern int pa_zdepi_cint_p (unsigned HOST_WIDE_INT);

extern void pa_output_ascii (FILE *, const char *, int);
extern HOST_WIDE_INT pa_compute_frame_size (poly_int64, int *);
extern void pa_expand_prologue (void);
extern void pa_expand_epilogue (void);
extern bool pa_can_use_return_insn (void);

/* Miscellaneous functions in pa.c.  */
#ifdef TREE_CODE
extern int pa_reloc_needed (tree);
extern bool pa_return_in_memory (const_tree, const_tree);
#endif /* TREE_CODE */

extern void pa_asm_output_aligned_bss (FILE *, const char *,
				       unsigned HOST_WIDE_INT,
				       unsigned int);
extern void pa_asm_output_aligned_common (FILE *, const char *,
					  unsigned HOST_WIDE_INT,
					  unsigned int);
extern void pa_asm_output_aligned_local (FILE *, const char *,
					 unsigned HOST_WIDE_INT,
					 unsigned int);
extern void pa_hpux_asm_output_external (FILE *, tree, const char *);
extern HOST_WIDE_INT pa_initial_elimination_offset (int, int);
extern HOST_WIDE_INT pa_function_arg_size (machine_mode, const_tree);
extern void pa_output_function_label (FILE *);
extern void hppa_profile_hook (int);

extern const int pa_magic_milli[];

/* Routines implemented in pa-d.c  */
extern void pa_d_target_versions (void);
extern void pa_d_register_target_info (void);
