/* Prototypes of target machine for GNU compiler.  MIPS version.
   Copyright (C) 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999 Free Software Foundation, Inc.
   Contributed by A. Lichnewsky (lich@inria.inria.fr).
   Changed by Michael Meissner	(meissner@osf.org).
   64 bit r4000 support by Ian Lance Taylor (ian@cygnus.com) and
   Brendan Eich (brendan@microunity.com).

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

#ifndef __MIPS_PROTOS_H__
#define __MIPS_PROTOS_H__

extern HOST_WIDE_INT	compute_frame_size PARAMS ((HOST_WIDE_INT));
extern void		function_epilogue PARAMS ((FILE *, HOST_WIDE_INT));
extern void		function_prologue PARAMS ((FILE *, HOST_WIDE_INT));
extern void		mips_asm_file_end PARAMS ((FILE *));
extern void		mips_asm_file_start PARAMS ((FILE *));
extern int		mips_can_use_return_insn PARAMS ((void));
extern void		mips_declare_object PARAMS ((FILE *, const char *, const char *, const char *, int));
extern void		mips_expand_epilogue PARAMS ((void));
extern void		mips_expand_prologue PARAMS ((void));
#ifdef REAL_VALUE_TYPE
extern void		mips_output_double PARAMS ((FILE *, REAL_VALUE_TYPE));
extern void		mips_output_float PARAMS ((FILE *, REAL_VALUE_TYPE));
#endif /* REAL_VALUE_TYPE */
extern void		mips_output_filename PARAMS ((FILE *, const char *));
extern void		mips_output_lineno PARAMS ((FILE *, int));
extern void		mips_order_regs_for_local_alloc PARAMS ((void));
extern struct rtx_def *	mips16_gp_pseudo_reg PARAMS ((void));
#ifdef ASM_OUTPUT_UNDEF_FUNCTION
extern int		mips_output_external_libcall PARAMS ((FILE *, const char *));
#endif /* ASM_OUTPUT_UNDEF_FUNCTION */


#ifdef TREE_CODE
extern struct rtx_def *	function_arg PARAMS ((CUMULATIVE_ARGS *, enum machine_mode, tree, int));
extern void		function_arg_advance PARAMS ((CUMULATIVE_ARGS *, enum machine_mode, tree, int));
extern int		function_arg_partial_nregs PARAMS ((CUMULATIVE_ARGS *, enum machine_mode, tree, int));
extern int		function_arg_pass_by_reference PARAMS ((CUMULATIVE_ARGS *, enum machine_mode, tree, int));
extern int		mips16_constant_after_function_p PARAMS ((tree));
extern int		mips_output_external PARAMS ((FILE *, tree, const char *));
extern tree		mips_build_va_list PARAMS ((void));
#ifdef RTX_CODE
extern void		mips_va_start PARAMS ((int, tree, rtx));
#endif /* RTX_CODE */
extern struct rtx_def  *mips_va_arg PARAMS ((tree, tree));
extern void		mips_select_section PARAMS ((tree, int));
#endif /* TREE_CODE */

#ifdef RTX_CODE
extern int		arith32_operand PARAMS ((rtx, enum machine_mode));
extern int		arith_operand PARAMS ((rtx, enum machine_mode));
extern int		cmp_op PARAMS ((rtx, enum machine_mode));
extern int		const_float_1_operand PARAMS ((rtx, enum machine_mode));
extern void		expand_block_move PARAMS ((rtx []));
extern int		equality_op PARAMS ((rtx, enum machine_mode));
extern void		final_prescan_insn PARAMS ((rtx, rtx [], int));
extern struct rtx_def * gen_int_relational PARAMS ((enum rtx_code, rtx, rtx, rtx,int *));
#ifdef TREE_CODE
extern void		init_cumulative_args PARAMS ((CUMULATIVE_ARGS *c, tree, rtx));
#endif /* TREE_CODE */
extern void		gen_conditional_branch PARAMS ((rtx[], enum rtx_code));
extern void		gen_conditional_move PARAMS ((rtx *));
extern int		large_int PARAMS ((rtx, enum machine_mode));
extern void		machine_dependent_reorg PARAMS ((rtx));
extern int		mips_address_cost PARAMS ((rtx));
extern int		mips_const_double_ok PARAMS ((rtx, enum machine_mode));
extern void		mips_count_memory_refs PARAMS ((rtx, int));
extern HOST_WIDE_INT	mips_debugger_offset PARAMS ((rtx, HOST_WIDE_INT));
extern int		mips_check_split PARAMS ((rtx, enum machine_mode));
extern const char      *mips_fill_delay_slot PARAMS ((const char *, enum delay_type, rtx[], rtx));
extern const char      *mips_move_1word PARAMS ((rtx[], rtx, int));
extern const char      *mips_move_2words PARAMS ((rtx[], rtx));
extern const char      *output_block_move PARAMS ((rtx, rtx[], int, enum block_move_type));
extern void		override_options PARAMS ((void));
extern int		pc_or_label_operand PARAMS ((rtx, enum machine_mode));
extern void		print_operand_address PARAMS ((FILE *, rtx));
extern void		print_operand PARAMS ((FILE *, rtx, int));
extern int		reg_or_0_operand PARAMS ((rtx, enum machine_mode));
extern int		true_reg_or_0_operand PARAMS ((rtx, enum machine_mode));
extern int		simple_memory_operand PARAMS ((rtx, enum machine_mode));
extern int		double_memory_operand PARAMS ((rtx, enum machine_mode));
extern int		small_int PARAMS ((rtx, enum machine_mode));
extern int		uns_arith_operand PARAMS ((rtx, enum machine_mode));
extern struct rtx_def *	embedded_pic_offset PARAMS ((rtx));
extern struct rtx_def * mips16_gp_offset PARAMS ((rtx));
extern int		mips16_gp_offset_p PARAMS ((rtx));
extern int		mips16_constant PARAMS ((rtx, enum machine_mode, int, int));
extern int		build_mips16_call_stub PARAMS ((rtx, rtx, rtx, int));
extern char  	       *mips_output_conditional_branch PARAMS ((rtx, rtx *, int, int, int, int));
extern int              mips_adjust_insn_length PARAMS ((rtx, int));
extern enum reg_class	mips_secondary_reload_class PARAMS ((enum reg_class, enum machine_mode, rtx, int));
extern void		mips_select_rtx_section PARAMS ((enum machine_mode, rtx));

/* Recognition functions that return if a condition is true.  */
extern int		address_operand PARAMS ((rtx, enum machine_mode));
extern int		call_insn_operand PARAMS ((rtx, enum machine_mode));
extern int		const_double_operand PARAMS ((rtx, enum machine_mode));
extern int		const_int_operand PARAMS ((rtx, enum machine_mode));
extern int		consttable_operand PARAMS ((rtx, enum machine_mode));
extern int		general_operand PARAMS ((rtx, enum machine_mode));
extern int		immediate_operand PARAMS ((rtx, enum machine_mode));
extern int		memory_operand PARAMS ((rtx, enum machine_mode));
extern int		nonimmediate_operand PARAMS ((rtx, enum machine_mode));
extern int		nonmemory_operand PARAMS ((rtx, enum machine_mode));
extern int		pic_address_needs_scratch PARAMS ((rtx));
extern int		register_operand PARAMS ((rtx, enum machine_mode));
extern int		scratch_operand PARAMS ((rtx, enum machine_mode));
extern int		move_operand PARAMS ((rtx, enum machine_mode));
extern int		movdi_operand PARAMS ((rtx, enum machine_mode));
extern int		se_register_operand PARAMS ((rtx, enum machine_mode));
extern int		se_reg_or_0_operand PARAMS ((rtx, enum machine_mode));
extern int		se_uns_arith_operand PARAMS ((rtx, enum machine_mode));
extern int		se_arith_operand PARAMS ((rtx, enum machine_mode));
extern int		se_nonmemory_operand PARAMS ((rtx, enum machine_mode));
extern int		se_nonimmediate_operand PARAMS ((rtx, enum machine_mode));
extern int              mips_legitimate_address_p PARAMS ((enum machine_mode, rtx, int));
extern int              mips_reg_mode_ok_for_base_p PARAMS ((rtx, enum machine_mode, int));
extern int              extend_operator PARAMS ((rtx, enum machine_mode));
extern int              highpart_shift_operator PARAMS ((rtx, enum machine_mode));
extern int		m16_uimm3_b PARAMS ((rtx, enum machine_mode));
extern int		m16_simm4_1 PARAMS ((rtx, enum machine_mode));
extern int		m16_nsimm4_1 PARAMS ((rtx, enum machine_mode));
extern int		m16_simm5_1 PARAMS ((rtx, enum machine_mode));
extern int		m16_nsimm5_1 PARAMS ((rtx, enum machine_mode));
extern int		m16_uimm5_4 PARAMS ((rtx, enum machine_mode));
extern int		m16_nuimm5_4 PARAMS ((rtx, enum machine_mode));
extern int		m16_simm8_1 PARAMS ((rtx, enum machine_mode));
extern int		m16_nsimm8_1 PARAMS ((rtx, enum machine_mode));
extern int		m16_uimm8_1 PARAMS ((rtx, enum machine_mode));
extern int		m16_nuimm8_1 PARAMS ((rtx, enum machine_mode));
extern int		m16_uimm8_m1_1 PARAMS ((rtx, enum machine_mode));
extern int		m16_uimm8_4 PARAMS ((rtx, enum machine_mode));
extern int		m16_nuimm8_4 PARAMS ((rtx, enum machine_mode));
extern int		m16_simm8_8 PARAMS ((rtx, enum machine_mode));
extern int		m16_nsimm8_8 PARAMS ((rtx, enum machine_mode));
extern int		m16_usym8_4 PARAMS ((rtx, enum machine_mode));
extern int		m16_usym5_4 PARAMS ((rtx, enum machine_mode));
#endif /* RTX_CODE */

#endif /* __MIPS_PROTOS_H__ */
