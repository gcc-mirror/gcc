/* Definitions of target machine for GNU compiler, Argonaut ARC cpu.
   Copyright (C) 2000 Free Software Foundation, Inc.

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

#ifdef RTX_CODE
#ifdef TREE_CODE
extern void arc_va_start PARAMS ((int, tree, rtx));
extern rtx arc_va_arg PARAMS ((tree, tree));
#endif /* TREE_CODE */

extern enum machine_mode arc_select_cc_mode PARAMS ((enum rtx_code, rtx, rtx));

/* Define the function that build the compare insn for scc and bcc.  */
extern struct rtx_def *gen_compare_reg PARAMS ((enum rtx_code, rtx, rtx));

/* Declarations for various fns used in the .md file.  */
extern const char *output_shift PARAMS ((rtx *));

extern int symbolic_operand PARAMS ((rtx, enum machine_mode));
extern int arc_double_limm_p PARAMS ((rtx));
extern int arc_address_cost PARAMS ((rtx));
extern int arc_eligible_for_epilogue_delay PARAMS ((rtx, int));
extern void arc_initialize_trampoline PARAMS ((rtx, rtx, rtx));
extern void arc_print_operand PARAMS ((FILE *, rtx, int));
extern void arc_print_operand_address PARAMS ((FILE *, rtx));
extern void arc_final_prescan_insn PARAMS ((rtx, rtx *, int));
extern int call_address_operand PARAMS ((rtx, enum machine_mode));
extern int call_operand PARAMS ((rtx, enum machine_mode));
extern int symbolic_memory_operand PARAMS ((rtx, enum machine_mode));
extern int short_immediate_operand PARAMS ((rtx, enum machine_mode));
extern int long_immediate_operand PARAMS ((rtx, enum machine_mode));
extern int long_immediate_loadstore_operand PARAMS ((rtx, enum machine_mode));
extern int move_src_operand PARAMS ((rtx, enum machine_mode));
extern int move_double_src_operand PARAMS ((rtx, enum machine_mode));
extern int move_dest_operand PARAMS ((rtx, enum machine_mode));
extern int load_update_operand PARAMS ((rtx, enum machine_mode));
extern int store_update_operand PARAMS ((rtx, enum machine_mode));
extern int nonvol_nonimm_operand PARAMS ((rtx, enum machine_mode));
extern int const_sint32_operand PARAMS ((rtx, enum machine_mode));
extern int const_uint32_operand PARAMS ((rtx, enum machine_mode));
extern int proper_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int shift_operator PARAMS ((rtx, enum machine_mode));
#endif /* RTX_CODE */

#ifdef TREE_CODE
extern enum arc_function_type arc_compute_function_type PARAMS ((tree));
extern void arc_setup_incoming_varargs PARAMS ((CUMULATIVE_ARGS *,
						enum machine_mode, tree,
						int *, int));
#endif /* TREE_CODE */


extern void arc_init PARAMS ((void));
extern void arc_asm_file_start PARAMS ((FILE *));
extern unsigned int arc_compute_frame_size PARAMS ((int));
extern void arc_save_restore PARAMS ((FILE *, const char *, unsigned int,
				      unsigned int, const char *));
extern int arc_delay_slots_for_epilogue PARAMS ((void));
extern void arc_finalize_pic PARAMS ((void));
extern void arc_ccfsm_at_label PARAMS ((const char *, int));
extern int arc_ccfsm_branch_deleted_p PARAMS ((void));
extern void arc_ccfsm_record_branch_deleted PARAMS ((void));

