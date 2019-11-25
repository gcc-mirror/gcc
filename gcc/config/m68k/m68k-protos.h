/* Definitions of target machine for GNU compiler.  Sun 68000/68020 version.
   Copyright (C) 2000-2019 Free Software Foundation, Inc.

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

/* Define functions defined in aux-output.c and used in templates.  */

#ifdef RTX_CODE
extern enum m68k_function_kind m68k_get_function_kind (tree);
extern HOST_WIDE_INT m68k_initial_elimination_offset (int from, int to);

extern void split_di (rtx[], int, rtx[], rtx[]);

extern bool valid_mov3q_const (HOST_WIDE_INT);
extern const char *output_move_simode (rtx *);
extern const char *output_move_himode (rtx *);
extern const char *output_move_qimode (rtx *);
extern const char *output_move_stricthi (rtx *);
extern const char *output_move_strictqi (rtx *);
extern const char *output_move_double (rtx *);
extern const char *output_move_const_single (rtx *);
extern const char *output_move_const_double (rtx *);
extern const char *output_btst (rtx *, rtx, rtx, rtx_insn *, int);
extern const char *output_scc_di (rtx, rtx, rtx, rtx);
extern const char *output_addsi3 (rtx *);
extern const char *output_andsi3 (rtx *);
extern const char *output_iorsi3 (rtx *);
extern const char *output_xorsi3 (rtx *);
extern const char *output_call (rtx);
extern const char *output_sibcall (rtx);
extern void m68k_init_cc ();
extern void output_dbcc_and_branch (rtx *, rtx_code);
extern rtx_code m68k_output_compare_di (rtx, rtx, rtx, rtx, rtx_insn *, rtx_code);
extern rtx_code m68k_output_compare_si (rtx, rtx, rtx_code);
extern rtx_code m68k_output_compare_hi (rtx, rtx, rtx_code);
extern rtx_code m68k_output_compare_qi (rtx, rtx, rtx_code);
extern rtx_code m68k_output_compare_fp (rtx, rtx, rtx_code);
extern rtx_code m68k_output_btst (rtx, rtx, rtx_code, int);
extern rtx_code m68k_output_bftst (rtx, rtx, rtx, rtx_code);
extern rtx_code m68k_find_flags_value (rtx, rtx, rtx_code);

extern const char *m68k_output_scc (rtx_code);
extern const char *m68k_output_scc_float (rtx_code);
extern const char *m68k_output_branch_integer (rtx_code);
extern const char *m68k_output_branch_integer_rev (rtx_code);
extern const char *m68k_output_branch_float (rtx_code);
extern const char *m68k_output_branch_float_rev (rtx_code);
extern int floating_exact_log2 (rtx);
extern bool strict_low_part_peephole_ok (machine_mode mode,
					 rtx_insn *first_insn, rtx target);

/* Functions from m68k.c used in macros.  */
extern int standard_68881_constant_p (rtx);
extern void print_operand_address (FILE *, rtx);
extern void print_operand (FILE *, rtx, int);
extern void notice_update_cc (rtx, rtx);
extern bool m68k_legitimate_base_reg_p (rtx, bool);
extern bool m68k_legitimate_index_reg_p (rtx, bool);
extern bool m68k_illegitimate_symbolic_constant_p (rtx);
extern bool m68k_legitimate_constant_p (machine_mode, rtx);
extern bool m68k_matches_q_p (rtx);
extern bool m68k_matches_u_p (rtx);
extern rtx legitimize_pic_address (rtx, machine_mode, rtx);
extern rtx m68k_legitimize_tls_address (rtx);
extern bool m68k_tls_reference_p (rtx, bool);
extern int valid_dbcc_comparison_p_2 (rtx, machine_mode);
extern rtx m68k_libcall_value (machine_mode);
extern rtx m68k_function_value (const_tree, const_tree);
extern int emit_move_sequence (rtx *, machine_mode, rtx);
extern bool m68k_movem_pattern_p (rtx, rtx, HOST_WIDE_INT, bool);
extern const char *m68k_output_movem (rtx *, rtx, HOST_WIDE_INT, bool);
extern bool m68k_epilogue_uses (int);

/* Functions from m68k.c used in constraints.md.  */
extern rtx m68k_unwrap_symbol (rtx, bool);

/* Functions from m68k.c used in genattrtab.  */
#ifdef HAVE_ATTR_cpu
extern enum attr_cpu m68k_sched_cpu;
extern enum attr_mac m68k_sched_mac;

extern enum attr_opx_type m68k_sched_attr_opx_type (rtx_insn *, int);
extern enum attr_opy_type m68k_sched_attr_opy_type (rtx_insn *, int);
extern enum attr_size m68k_sched_attr_size (rtx_insn *);
extern enum attr_op_mem m68k_sched_attr_op_mem (rtx_insn *);
#endif /* HAVE_ATTR_cpu */

#endif /* RTX_CODE */

extern enum reg_class m68k_secondary_reload_class (enum reg_class,
						   machine_mode, rtx);
extern enum reg_class m68k_preferred_reload_class (rtx, enum reg_class);
extern void m68k_expand_prologue (void);
extern bool m68k_use_return_insn (void);
extern void m68k_expand_epilogue (bool);
extern const char *m68k_cpp_cpu_ident (const char *);
extern const char *m68k_cpp_cpu_family (const char *);
extern void init_68881_table (void);
extern rtx m68k_legitimize_call_address (rtx);
extern rtx m68k_legitimize_sibcall_address (rtx);
extern int m68k_hard_regno_rename_ok(unsigned int, unsigned int);
extern poly_int64 m68k_push_rounding (poly_int64);
