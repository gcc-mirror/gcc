/* Definitions of target machine for GNU compiler.  AT&T DSP1600.
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Michael Collison (collison@world.std.com).

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
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
extern struct rtx_def *gen_compare_reg PARAMS ((enum rtx_code, rtx, rtx));
extern int call_address_operand PARAMS ((rtx, enum machine_mode));
extern int arith_reg_operand PARAMS ((rtx, enum machine_mode));
extern int symbolic_address_operand PARAMS ((rtx, enum machine_mode));
extern int Y_address_operand PARAMS ((rtx, enum machine_mode));
extern int sp_operand PARAMS ((rtx, enum machine_mode));
extern int sp_operand2 PARAMS ((rtx, enum machine_mode));
extern int nonmemory_arith_operand PARAMS ((rtx, enum machine_mode));
extern int dsp16xx_comparison_operator PARAMS ((rtx, enum machine_mode));

extern void notice_update_cc PARAMS ((rtx));
extern void double_reg_from_memory PARAMS ((rtx[]));
extern void double_reg_to_memory PARAMS ((rtx[]));
extern enum rtx_code next_cc_user_code PARAMS ((rtx));
extern int next_cc_user_unsigned PARAMS ((rtx));
extern struct rtx_def *gen_tst_reg PARAMS ((rtx));
extern const char *output_block_move PARAMS ((rtx[]));
extern enum reg_class preferred_reload_class PARAMS ((rtx, enum reg_class));
extern enum reg_class secondary_reload_class PARAMS ((enum reg_class,
						      enum machine_mode, rtx));
extern int emit_move_sequence PARAMS ((rtx *, enum machine_mode));
extern void print_operand PARAMS ((FILE *, rtx, int));
extern void print_operand_address PARAMS ((FILE *, rtx));
extern void output_dsp16xx_float_const PARAMS ((rtx *));
extern void emit_1600_core_shift PARAMS ((enum rtx_code, rtx *, int));
extern int dsp16xx_address_cost PARAMS ((rtx));
extern int symbolic_address_p PARAMS ((rtx));
#endif /* RTX_CODE */


#ifdef TREE_CODE
extern struct rtx_def *dsp16xx_function_arg PARAMS ((CUMULATIVE_ARGS,
						     enum machine_mode,
						     tree, int));
extern void dsp16xx_function_arg_advance PARAMS ((CUMULATIVE_ARGS *,
						  enum machine_mode,
						  tree, int));
#endif /* TREE_CODE */

extern void dsp16xx_invalid_register_for_compare PARAMS ((void));
extern int class_max_nregs PARAMS ((enum reg_class, enum machine_mode));
extern enum reg_class limit_reload_class PARAMS ((enum reg_class, enum machine_mode));
extern int dsp16xx_register_move_cost PARAMS ((enum reg_class, enum reg_class));
extern int dsp16xx_makes_calls PARAMS ((void));
extern long compute_frame_size PARAMS ((int));
extern int dsp16xx_call_saved_register PARAMS ((int));
extern int dsp16xx_call_saved_register PARAMS ((int));
extern void init_emulation_routines PARAMS ((void));
extern int ybase_regs_ever_used PARAMS ((void));
extern void override_options PARAMS ((void));
extern int dsp16xx_starting_frame_offset PARAMS ((void));
extern int initial_frame_pointer_offset PARAMS ((void));
extern void asm_output_common PARAMS ((FILE *, const char *, int, int));
extern void asm_output_local PARAMS ((FILE *, const char *, int, int));
extern void asm_output_float PARAMS ((FILE *, double));
extern void asm_output_long PARAMS ((FILE *, long));
extern void dsp16xx_file_start PARAMS ((void));
extern struct rtx_def *(*dsp16xx_compare_gen) PARAMS (());
extern int hard_regno_mode_ok PARAMS ((int, enum machine_mode));
extern enum reg_class dsp16xx_reg_class_from_letter PARAMS ((int));
extern int regno_reg_class PARAMS ((int));
extern void function_prologue PARAMS ((FILE *, int));
extern void function_epilogue PARAMS ((FILE *, int));
