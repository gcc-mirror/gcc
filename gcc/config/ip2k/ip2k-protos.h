/* Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Red Hat, Inc and Ubicom, Inc.

   This file is part of GNU CC.

   GNU CC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GNU CC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

extern void asm_file_start PARAMS ((FILE *));
extern void asm_file_end PARAMS ((FILE *));

extern void function_prologue PARAMS ((FILE *, HOST_WIDE_INT));
extern void function_epilogue PARAMS ((FILE *, HOST_WIDE_INT));
extern int find_one_set_bit_p PARAMS ((HOST_WIDE_INT));
extern int find_one_clear_bit_p PARAMS ((HOST_WIDE_INT));

#ifdef TREE_CODE
extern void unique_section PARAMS ((tree, int));
extern void encode_section_info PARAMS ((tree, int));
extern void asm_output_section_name PARAMS ((FILE *, tree, const char *,
					     int));
extern int valid_machine_type_attribute PARAMS ((tree, tree, tree, tree));
extern int valid_machine_decl_attribute PARAMS ((tree, tree, tree, tree));
extern int ip2k_return_pops_args PARAMS ((tree, tree, int));
#endif /* TREE_CODE */

#ifdef RTX_CODE
extern int legitimate_address_p PARAMS ((enum machine_mode, rtx, int));
extern void machine_dependent_reorg PARAMS ((rtx));
extern int ip2k_address_cost PARAMS ((rtx));
extern int ip2k_extra_constraint PARAMS ((rtx, int));
extern rtx legitimize_address PARAMS ((rtx, rtx, enum machine_mode, rtx));
extern int adjust_insn_length PARAMS ((rtx insn, int len));
extern int default_rtx_costs PARAMS ((rtx, enum rtx_code, enum rtx_code));
extern void asm_output_char PARAMS ((FILE *, rtx));
extern void asm_output_short PARAMS ((FILE *, rtx));
extern void asm_output_byte PARAMS ((FILE *, int));
extern void print_operand PARAMS ((FILE *, rtx, int));
extern void print_operand_address PARAMS ((FILE *, rtx));
extern int ip2k_jump_mode PARAMS ((rtx, rtx));
extern void ip2k_split_words PARAMS ((enum machine_mode, enum machine_mode,
				      rtx *));
extern rtx ip2k_get_low_half PARAMS ((rtx, enum machine_mode));
extern rtx ip2k_get_high_half PARAMS ((rtx, enum machine_mode));
extern int ip2k_nonptr_operand PARAMS ((rtx, enum machine_mode));
extern int ip2k_ptr_operand PARAMS ((rtx, enum machine_mode));
extern int ip2k_ip_operand PARAMS ((rtx, enum machine_mode));
extern int ip2k_short_operand PARAMS ((rtx, enum machine_mode));
extern int ip2k_gen_operand PARAMS ((rtx, enum machine_mode));
extern int ip2k_nonsp_reg_operand PARAMS ((rtx, enum machine_mode));
extern int ip2k_symbol_ref_operand PARAMS ((rtx, enum machine_mode));
extern const char *ip2k_set_compare PARAMS ((rtx, rtx));
extern const char *ip2k_gen_sCOND PARAMS ((rtx, enum rtx_code, rtx));
extern const char *ip2k_gen_signed_comp_branch PARAMS ((rtx,
							enum rtx_code,
							rtx));
extern const char *ip2k_gen_unsigned_comp_branch PARAMS ((rtx,
							  enum rtx_code,
							  rtx));
extern int is_regfile_address PARAMS ((rtx));
extern int ip2k_mode_dependent_address PARAMS ((rtx));
extern int ip2k_address_uses_reg_p PARAMS ((rtx, unsigned int));
extern int ip2k_xexp_not_uses_reg_p PARAMS ((rtx, unsigned int, int));
extern int ip2k_composite_xexp_not_uses_reg_p PARAMS ((rtx, unsigned int, int));
extern int ip2k_composite_xexp_not_uses_cc0_p PARAMS ((rtx));
extern int ip2k_signed_comparison_operator PARAMS ((rtx,
						    enum machine_mode));
extern int ip2k_unsigned_comparison_operator PARAMS ((rtx,
						      enum machine_mode));
extern int ip2k_unary_operator PARAMS ((rtx, enum machine_mode));
extern int ip2k_binary_operator PARAMS ((rtx, enum machine_mode));

extern rtx ip2k_compare_operands[3];
#endif /* RTX_CODE */

#ifdef HAVE_MACHINE_MODES
extern int class_max_nregs PARAMS ((enum reg_class, enum machine_mode));
extern enum reg_class class_likely_spilled_p PARAMS ((int c));
#endif /* HAVE_MACHINE_MODES */

#ifdef REAL_VALUE_TYPE
extern void asm_output_float PARAMS ((FILE *, REAL_VALUE_TYPE));
#endif 

extern int ip2k_init_elim_offset PARAMS ((int, int));
extern void ip2k_init_local_alloc PARAMS ((int *));

