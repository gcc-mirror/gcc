/* Prototypes for pa.c functions used in the md file & elsewhere.
   Copyright (C) 2000-2014 Free Software Foundation, Inc.

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

/* Used in insn-*.c.  */
extern int pa_following_call (rtx);

/* Define functions in pa.c and used in insn-output.c.  */

extern const char *pa_output_and (rtx *);
extern const char *pa_output_64bit_and (rtx *);
extern const char *pa_output_ior (rtx *);
extern const char *pa_output_64bit_ior (rtx *);
extern const char *pa_output_move_double (rtx *);
extern const char *pa_output_fp_move_double (rtx *);
extern const char *pa_output_block_move (rtx *, int);
extern const char *pa_output_block_clear (rtx *, int);
extern const char *pa_output_cbranch (rtx *, int, rtx);
extern const char *pa_output_lbranch (rtx, rtx, int);
extern const char *pa_output_bb (rtx *, int, rtx, int);
extern const char *pa_output_bvb (rtx *, int, rtx, int);
extern const char *pa_output_dbra (rtx *, rtx, int);
extern const char *pa_output_movb (rtx *, rtx, int, int);
extern const char *pa_output_parallel_movb (rtx *, rtx);
extern const char *pa_output_parallel_addb (rtx *, rtx);
extern const char *pa_output_call (rtx, rtx, int);
extern const char *pa_output_indirect_call (rtx, rtx);
extern const char *pa_output_millicode_call (rtx, rtx);
extern const char *pa_output_mul_insn (int, rtx);
extern const char *pa_output_div_insn (rtx *, int, rtx);
extern const char *pa_output_mod_insn (int, rtx);
extern const char *pa_singlemove_string (rtx *);
extern void pa_output_arg_descriptor (rtx);
extern void pa_output_global_address (FILE *, rtx, int);
extern void pa_print_operand (FILE *, rtx, int);
extern void pa_encode_label (rtx);
extern int pa_symbolic_expression_p (rtx);
extern bool pa_tls_referenced_p (rtx);
extern int pa_adjust_insn_length (rtx, int);
extern int pa_fmpyaddoperands (rtx *);
extern int pa_fmpysuboperands (rtx *);
extern void pa_emit_bcond_fp (rtx[]);
extern int pa_emit_move_sequence (rtx *, enum machine_mode, rtx);
extern int pa_emit_hpdiv_const (rtx *, int);
extern int pa_is_function_label_plus_const (rtx);
extern int pa_jump_in_call_delay (rtx);
extern int pa_fpstore_bypass_p (rtx, rtx);
extern int pa_attr_length_millicode_call (rtx);
extern int pa_attr_length_call (rtx, int);
extern int pa_attr_length_indirect_call (rtx);
extern rtx pa_legitimize_reload_address (rtx, enum machine_mode,
					 int, int, int);

/* Declare functions defined in pa.c and used in templates.  */

extern rtx pa_return_addr_rtx (int, rtx);

#ifdef ARGS_SIZE_RTX
/* expr.h defines ARGS_SIZE_RTX and `enum direction' */
#ifdef TREE_CODE
extern enum direction pa_function_arg_padding (enum machine_mode, const_tree);
#endif
#endif /* ARGS_SIZE_RTX */
extern int pa_insn_refs_are_delayed (rtx);
extern rtx pa_get_deferred_plabel (rtx);
#endif /* RTX_CODE */

extern int pa_and_mask_p (unsigned HOST_WIDE_INT);
extern int pa_cint_ok_for_move (HOST_WIDE_INT);
extern int pa_ior_mask_p (unsigned HOST_WIDE_INT);
extern int pa_ldil_cint_p (HOST_WIDE_INT);
extern int pa_shadd_constant_p (int);
extern int pa_zdepi_cint_p (unsigned HOST_WIDE_INT);

extern void pa_output_ascii (FILE *, const char *, int);
extern HOST_WIDE_INT pa_compute_frame_size (HOST_WIDE_INT, int *);
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
extern bool pa_cannot_change_mode_class (enum machine_mode, enum machine_mode,
					 enum reg_class);
extern bool pa_modes_tieable_p (enum machine_mode, enum machine_mode);
extern HOST_WIDE_INT pa_initial_elimination_offset (int, int);

extern const int pa_magic_milli[];
