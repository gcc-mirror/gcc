/* Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Red Hat, Inc and Ubicom, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

extern void function_prologue (FILE *, HOST_WIDE_INT);
extern void function_epilogue (FILE *, HOST_WIDE_INT);
extern int find_one_set_bit_p (HOST_WIDE_INT);
extern int find_one_clear_bit_p (HOST_WIDE_INT);

#ifdef TREE_CODE
extern void unique_section (tree, int);
extern int valid_machine_type_attribute (tree, tree, tree, tree);
extern int valid_machine_decl_attribute (tree, tree, tree, tree);
extern int ip2k_return_pops_args (tree, tree, int);
#endif /* TREE_CODE */

#ifdef RTX_CODE
extern int legitimate_address_p (enum machine_mode, rtx, int);
extern int ip2k_extra_constraint (rtx, int);
extern rtx legitimize_address (rtx, rtx, enum machine_mode, rtx);
extern int adjust_insn_length (rtx insn, int len);
extern void asm_output_char (FILE *, rtx);
extern void asm_output_short (FILE *, rtx);
extern void asm_output_byte (FILE *, int);
extern void print_operand (FILE *, rtx, int);
extern void print_operand_address (FILE *, rtx);
extern int ip2k_jump_mode (rtx, rtx);
extern void ip2k_split_words (enum machine_mode, enum machine_mode, rtx *);
extern rtx ip2k_get_low_half (rtx, enum machine_mode);
extern rtx ip2k_get_high_half (rtx, enum machine_mode);
extern int ip2k_nonptr_operand (rtx, enum machine_mode);
extern int ip2k_ptr_operand (rtx, enum machine_mode);
extern int ip2k_ip_operand (rtx, enum machine_mode);
extern int ip2k_short_operand (rtx, enum machine_mode);
extern int ip2k_gen_operand (rtx, enum machine_mode);
extern int ip2k_nonsp_reg_operand (rtx, enum machine_mode);
extern int ip2k_symbol_ref_operand (rtx, enum machine_mode);
extern const char *ip2k_set_compare (rtx, rtx);
extern const char *ip2k_gen_sCOND (rtx, enum rtx_code, rtx);
extern const char *ip2k_gen_signed_comp_branch (rtx, enum rtx_code, rtx);
extern const char *ip2k_gen_unsigned_comp_branch (rtx, enum rtx_code, rtx);
extern int is_regfile_address (rtx);
extern int ip2k_mode_dependent_address (rtx);
extern int ip2k_address_uses_reg_p (rtx, unsigned int);
extern int ip2k_xexp_not_uses_reg_p (rtx, unsigned int, int);
extern int ip2k_composite_xexp_not_uses_reg_p (rtx, unsigned int, int);
extern int ip2k_composite_xexp_not_uses_cc0_p (rtx);
extern int ip2k_signed_comparison_operator (rtx, enum machine_mode);
extern int ip2k_unsigned_comparison_operator (rtx, enum machine_mode);
extern int ip2k_unary_operator (rtx, enum machine_mode);
extern int ip2k_binary_operator (rtx, enum machine_mode);

extern rtx ip2k_compare_operands[3];
#endif /* RTX_CODE */

#ifdef HAVE_MACHINE_MODES
extern int class_max_nregs (enum reg_class, enum machine_mode);
extern enum reg_class class_likely_spilled_p (int c);
#endif /* HAVE_MACHINE_MODES */

#ifdef REAL_VALUE_TYPE
extern void asm_output_float (FILE *, REAL_VALUE_TYPE);
#endif 

extern int ip2k_init_elim_offset (int, int);
extern void ip2k_init_local_alloc (int *);

