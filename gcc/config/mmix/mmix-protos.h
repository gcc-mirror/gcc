/* Prototypes for exported functions defined in mmix.c
   Copyright (C) 2000-2020 Free Software Foundation, Inc.
   Contributed by Hans-Peter Nilsson (hp@bitrange.com)

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

extern void mmix_init_expanders (void);
extern int mmix_eh_return_data_regno (int);
extern int mmix_initial_elimination_offset (int, int);
extern int mmix_function_arg_regno_p (int, int);
extern void mmix_function_profiler (FILE *, int);
extern int mmix_reversible_cc_mode (machine_mode);
extern const char *mmix_text_section_asm_op (void);
extern const char *mmix_data_section_asm_op (void);
extern void mmix_output_quoted_string (FILE *, const char *, int);
extern void mmix_asm_output_ascii (FILE *, const char *, int);
extern void mmix_asm_output_label (FILE *, const char *);
extern void mmix_asm_output_internal_label (FILE *, const char *);
extern void mmix_asm_weaken_label (FILE *, const char *);
extern void mmix_asm_output_labelref (FILE *, const char *);
extern void mmix_asm_output_def (FILE *, const char *, const char *);
extern void mmix_asm_output_reg_push (FILE *, int);
extern void mmix_asm_output_reg_pop (FILE *, int);
extern void mmix_asm_output_skip (FILE *, int);
extern void mmix_asm_output_align (FILE *, int);
extern int64_t mmix_intval (const_rtx);
extern int mmix_shiftable_wyde_value (uint64_t);
extern void mmix_output_register_setting (FILE *, int, int64_t, int);
extern int mmix_opposite_regno (int, int);
extern int mmix_local_regno (int);
extern unsigned mmix_dbx_register_number (unsigned);
extern int mmix_use_simple_return (void);
extern void mmix_make_decl_one_only (tree);
extern int mmix_data_alignment (tree, int);
extern unsigned mmix_local_alignment (tree, unsigned);
extern void mmix_asm_output_pool_prologue (FILE *, const char *, tree, int);
extern void mmix_asm_output_aligned_common (FILE *, const char *, int, int);
extern void mmix_asm_output_aligned_local (FILE *, const char *, int, int);
extern void mmix_asm_declare_register_global
  (FILE *, tree, int, const char *);
extern void mmix_asm_output_addr_diff_elt (FILE *, rtx, int, int);
extern void mmix_asm_output_addr_vec_elt (FILE *, int);
extern enum reg_class mmix_secondary_reload_class
  (enum reg_class, machine_mode, rtx, int);
extern rtx mmix_dynamic_chain_address (rtx);
extern rtx mmix_return_addr_rtx (int, rtx);
extern rtx mmix_eh_return_stackadj_rtx (void);
extern rtx mmix_eh_return_handler_rtx (void);
extern int mmix_constant_address_p (rtx);
extern void mmix_expand_prologue (void);
extern void mmix_expand_epilogue (void);
extern rtx mmix_get_hard_reg_initial_val (machine_mode, int);
extern int mmix_asm_preferred_eh_data_format (int, int);
extern void mmix_setup_frame_addresses (void);

#ifdef RTX_CODE
/* Needs to be ifdef:d for sake of enum rtx_code.  */
extern machine_mode mmix_select_cc_mode (enum rtx_code, rtx, rtx);
extern void mmix_canonicalize_comparison (enum rtx_code *, rtx *, rtx *);
extern rtx mmix_gen_compare_reg (enum rtx_code, rtx, rtx);
#endif

/*
 * Local variables:
 * eval: (c-set-style "gnu")
 * indent-tabs-mode: t
 * End:
 */
