/* Prototypes for exported functions defined in mcore.c
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Nick Clifton (nickc@cygnus.com)

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

extern char * mcore_output_jump_label_table	PARAMS ((void));
extern void   mcore_expand_prolog          	PARAMS ((void));
extern void   mcore_expand_epilog          	PARAMS ((void));
extern int    mcore_const_ok_for_inline    	PARAMS ((long));
extern int    mcore_num_ones               	PARAMS ((int));
extern int    mcore_num_zeros              	PARAMS ((int));
extern int    mcore_initial_elimination_offset	PARAMS ((int, int));
extern int    mcore_byte_offset            	PARAMS ((unsigned int));
extern int    mcore_halfword_offset        	PARAMS ((unsigned int));
extern int    mcore_const_trick_uses_not   	PARAMS ((long));
extern void   mcore_override_options       	PARAMS ((void));
extern int    mcore_dllexport_name_p       	PARAMS ((char *));
extern int    mcore_dllimport_name_p       	PARAMS ((char *));
extern int    mcore_naked_function_p       	PARAMS ((void));

#ifdef TREE_CODE
extern void   mcore_unique_section         	PARAMS ((tree, int));
extern void   mcore_encode_section_info    	PARAMS ((tree));
extern int    mcore_valid_machine_decl_attribute PARAMS ((tree, tree, tree, tree));
extern tree   mcore_merge_machine_decl_attributes PARAMS ((tree, tree));

#ifdef HAVE_MACHINE_MODES
extern int    mcore_function_arg_partial_nregs	PARAMS ((CUMULATIVE_ARGS, enum machine_mode, tree, int));
extern void   mcore_setup_incoming_varargs	PARAMS ((CUMULATIVE_ARGS, enum machine_mode, tree, int *));
extern int    mcore_num_arg_regs           	PARAMS ((enum machine_mode, tree));
extern int    mcore_must_pass_on_stack     	PARAMS ((enum machine_mode, tree));
#endif /* HAVE_MACHINE_MODES */

#ifdef RTX_CODE
extern rtx  mcore_function_value         PARAMS ((tree, tree));
#endif /* RTX_CODE */
#endif /* TREE_CODE */

#ifdef RTX_CODE

extern rtx arch_compare_op0;
extern rtx arch_compare_op1;

extern char * mcore_output_bclri         	PARAMS ((rtx, int));
extern char * mcore_output_bseti         	PARAMS ((rtx, int));
extern char * mcore_output_cmov          	PARAMS ((rtx *, int, char *));
extern char * mcore_output_call          	PARAMS ((rtx *, int));
extern int    mcore_is_dead                	PARAMS ((rtx, rtx));
extern int    mcore_expand_insv            	PARAMS ((rtx *));
extern int    mcore_modify_comparison      	PARAMS ((RTX_CODE));
extern void   mcore_expand_block_move      	PARAMS ((rtx, rtx, rtx *));
extern rtx    mcore_dependent_simplify_rtx 	PARAMS ((rtx, int, int, int, int *));
extern void   mcore_dependent_reorg        	PARAMS ((rtx));
extern int    mcore_const_costs            	PARAMS ((rtx, RTX_CODE));
extern int    mcore_and_cost               	PARAMS ((rtx));
extern int    mcore_ior_cost               	PARAMS ((rtx));
extern char * mcore_output_andn          	PARAMS ((rtx, rtx *));
extern void   mcore_print_operand_address  	PARAMS ((FILE *, rtx));
extern void   mcore_print_operand          	PARAMS ((FILE *, rtx, int));
extern rtx    mcore_gen_compare_reg        	PARAMS ((RTX_CODE));
extern int    mcore_symbolic_address_p     	PARAMS ((rtx));
extern enum reg_class mcore_reload_class 	PARAMS ((rtx, enum reg_class));
extern int    mcore_is_same_reg            	PARAMS ((rtx, rtx));
extern int    mcore_arith_S_operand         	PARAMS ((rtx));

#ifdef HAVE_MACHINE_MODES
extern char * mcore_output_move          	PARAMS ((rtx, rtx *, enum machine_mode));
extern char * mcore_output_movedouble    	PARAMS ((rtx *, enum machine_mode));
extern char * mcore_output_inline_const_forced	PARAMS ((rtx, rtx *, enum machine_mode));
extern int    mcore_arith_reg_operand       	PARAMS ((rtx, enum machine_mode));
extern int    mcore_general_movsrc_operand  	PARAMS ((rtx, enum machine_mode));
extern int    mcore_general_movdst_operand  	PARAMS ((rtx, enum machine_mode));
extern int    mcore_reload_operand          	PARAMS ((rtx, enum machine_mode));
extern int    mcore_arith_J_operand         	PARAMS ((rtx, enum machine_mode));
extern int    mcore_arith_K_operand         	PARAMS ((rtx, enum machine_mode));
extern int    mcore_arith_K_operand_not_0   	PARAMS ((rtx, enum machine_mode));
extern int    mcore_arith_M_operand         	PARAMS ((rtx, enum machine_mode));
extern int    mcore_arith_K_S_operand       	PARAMS ((rtx, enum machine_mode));
extern int    mcore_arith_imm_operand       	PARAMS ((rtx, enum machine_mode));
extern int    mcore_arith_any_imm_operand   	PARAMS ((rtx, enum machine_mode));
extern int    mcore_arith_O_operand         	PARAMS ((rtx, enum machine_mode));
extern int    mcore_literal_K_operand       	PARAMS ((rtx, enum machine_mode));
extern int    mcore_addsub_operand          	PARAMS ((rtx, enum machine_mode));
extern int    mcore_compare_operand         	PARAMS ((rtx, enum machine_mode));
extern int    mcore_load_multiple_operation 	PARAMS ((rtx, enum machine_mode));
extern int    mcore_store_multiple_operation	PARAMS ((rtx, enum machine_mode));
extern int    mcore_call_address_operand    	PARAMS ((rtx, enum machine_mode));

#ifdef TREE_CODE
extern rtx    mcore_function_arg           	PARAMS ((CUMULATIVE_ARGS, enum machine_mode, tree, int));
#endif /* TREE_CODE */
#endif /* HAVE_MACHINE_MODES */
#endif /* RTX_CODE */
