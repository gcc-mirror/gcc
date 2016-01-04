/* Definitions of target machine for GNU compiler for IA-64.
   Copyright (C) 1999-2016 Free Software Foundation, Inc.

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

/* Shared between the driver and cc1.  */
extern enum unwind_info_type ia64_except_unwind_info (struct gcc_options *);

/* Functions defined in ia64.c */

extern int bundling_p;
#ifdef RTX_CODE
extern int ia64_st_address_bypass_p (rtx_insn *, rtx_insn *);
extern int ia64_ld_address_bypass_p (rtx_insn *, rtx_insn *);
extern int ia64_produce_address_p (rtx);

extern rtx ia64_expand_move (rtx, rtx);
extern int ia64_move_ok (rtx, rtx);
extern int ia64_load_pair_ok (rtx, rtx);
extern int addp4_optimize_ok (rtx, rtx);
extern void ia64_emit_cond_move (rtx, rtx, rtx);
extern int ia64_depz_field_mask (rtx, rtx);
extern void ia64_split_tmode_move (rtx[]);
extern bool ia64_expand_movxf_movrf (machine_mode, rtx[]);
extern void ia64_expand_compare (rtx *, rtx *, rtx *);
extern void ia64_expand_vecint_cmov (rtx[]);
extern bool ia64_expand_vecint_minmax (enum rtx_code, machine_mode, rtx[]);
extern void ia64_unpack_assemble (rtx, rtx, rtx, bool);
extern void ia64_expand_unpack (rtx [], bool, bool);
extern void ia64_expand_widen_sum (rtx[], bool);
extern void ia64_expand_call (rtx, rtx, rtx, int);
extern void ia64_split_call (rtx, rtx, rtx, rtx, rtx, int, int);
extern void ia64_reload_gp (void);
extern void ia64_expand_atomic_op (enum rtx_code, rtx, rtx, rtx, rtx,
				   enum memmodel);

extern HOST_WIDE_INT ia64_initial_elimination_offset (int, int);
extern void ia64_expand_prologue (void);
extern void ia64_expand_epilogue (int);

extern int ia64_direct_return (void);
extern bool ia64_expand_load_address (rtx, rtx);
extern int ia64_hard_regno_rename_ok (int, int);

extern enum reg_class ia64_secondary_reload_class (enum reg_class,
						   machine_mode, rtx);
extern const char *get_bundle_name (int);
extern const char *output_probe_stack_range (rtx, rtx);

extern void ia64_expand_vec_perm_even_odd (rtx, rtx, rtx, int);
extern bool ia64_expand_vec_perm_const (rtx op[4]);
extern void ia64_expand_vec_setv2sf (rtx op[3]);
#endif /* RTX_CODE */

#ifdef TREE_CODE
#ifdef RTX_CODE
extern rtx ia64_expand_builtin (tree, rtx, rtx, machine_mode, int);
extern rtx ia64_va_arg (tree, tree);
#endif /* RTX_CODE */

extern void ia64_asm_output_external (FILE *, tree, const char *);
extern void ia64_vms_output_aligned_decl_common (FILE *, tree, const char *,
						 unsigned HOST_WIDE_INT,
						 unsigned int);
extern void ia64_vms_elf_asm_named_section (const char *, unsigned int, tree);
extern void ia64_start_function (FILE *, const char *, tree);
#endif /* TREE_CODE */

extern int ia64_epilogue_uses (int);
extern int ia64_eh_uses (int);
extern void emit_safe_across_calls (void);
extern void ia64_init_builtins (void);
extern int ia64_dbx_register_number (int);

extern rtx ia64_return_addr_rtx (HOST_WIDE_INT, rtx);
extern void ia64_split_return_addr_rtx (rtx);

#ifdef ARGS_SIZE_RTX
/* expr.h defines ARGS_SIZE_RTX and `enum direction'.  */
extern enum direction ia64_hpux_function_arg_padding (machine_mode, const_tree);
#endif /* ARGS_SIZE_RTX */

extern void ia64_hpux_handle_builtin_pragma (struct cpp_reader *);
extern void ia64_output_function_profiler (FILE *, int);
extern void ia64_profile_hook (int);

extern void ia64_init_expanders (void);

extern rtx ia64_dconst_0_5 (void);
extern rtx ia64_dconst_0_375 (void);
