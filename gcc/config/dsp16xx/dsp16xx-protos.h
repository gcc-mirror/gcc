/* Definitions of target machine for GNU compiler.  AT&T DSP1600.
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Michael Collison (collison@world.std.com).

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

#ifdef RTX_CODE
extern struct rtx_def *gen_compare_reg (enum rtx_code, rtx, rtx);
extern int call_address_operand (rtx, enum machine_mode);
extern int arith_reg_operand (rtx, enum machine_mode);
extern int symbolic_address_operand (rtx, enum machine_mode);
extern int Y_address_operand (rtx, enum machine_mode);
extern int sp_operand (rtx, enum machine_mode);
extern int sp_operand2 (rtx, enum machine_mode);
extern int nonmemory_arith_operand (rtx, enum machine_mode);
extern int dsp16xx_comparison_operator (rtx, enum machine_mode);
extern int unx_comparison_operator (rtx, enum machine_mode);
extern int signed_comparison_operator (rtx, enum machine_mode);

extern void notice_update_cc (rtx);
extern void double_reg_from_memory (rtx[]);
extern void double_reg_to_memory (rtx[]);
extern enum rtx_code next_cc_user_code (rtx);
extern int next_cc_user_unsigned (rtx);
extern struct rtx_def *gen_tst_reg (rtx);
extern const char *output_block_move (rtx[]);
extern enum reg_class preferred_reload_class (rtx, enum reg_class);
extern enum reg_class secondary_reload_class (enum reg_class,
					      enum machine_mode, rtx);
extern int emit_move_sequence (rtx *, enum machine_mode);
extern void print_operand (FILE *, rtx, int);
extern void print_operand_address (FILE *, rtx);
extern void output_dsp16xx_float_const (rtx *);
extern void emit_1600_core_shift (enum rtx_code, rtx *, int);
extern int symbolic_address_p (rtx);
extern int uns_comparison_operator (rtx, enum machine_mode);
#endif /* RTX_CODE */


#ifdef TREE_CODE
extern struct rtx_def *dsp16xx_function_arg (CUMULATIVE_ARGS,
					     enum machine_mode,
					     tree, int);
extern void dsp16xx_function_arg_advance (CUMULATIVE_ARGS *,
					  enum machine_mode,
					  tree, int);
#endif /* TREE_CODE */

extern void dsp16xx_invalid_register_for_compare (void);
extern int class_max_nregs (enum reg_class, enum machine_mode);
extern enum reg_class limit_reload_class (enum reg_class, enum machine_mode);
extern int dsp16xx_register_move_cost (enum reg_class, enum reg_class);
extern int dsp16xx_makes_calls (void);
extern long compute_frame_size (int);
extern int dsp16xx_call_saved_register (int);
extern int dsp16xx_call_saved_register (int);
extern void init_emulation_routines (void);
extern int ybase_regs_ever_used (void);
extern void override_options (void);
extern int dsp16xx_starting_frame_offset (void);
extern int initial_frame_pointer_offset (void);
extern void asm_output_common (FILE *, const char *, int, int);
extern void asm_output_local (FILE *, const char *, int, int);
extern void asm_output_float (FILE *, double);
extern bool dsp16xx_compare_gen;
extern int hard_regno_mode_ok (int, enum machine_mode);
extern enum reg_class dsp16xx_reg_class_from_letter (int);
extern int regno_reg_class (int);
extern void function_prologue (FILE *, int);
extern void function_epilogue (FILE *, int);
extern int num_1600_core_shifts (int);
