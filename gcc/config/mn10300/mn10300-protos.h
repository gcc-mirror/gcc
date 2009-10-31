/* Definitions of target machine for GNU compiler. Matsushita MN10300 series
   Copyright (C) 2000, 2003, 2004, 2005, 2007 Free Software Foundation, Inc.
   Contributed by Jeff Law (law@cygnus.com).

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

extern void mn10300_override_options (void);
extern rtx legitimize_pic_address (rtx, rtx);
extern int legitimate_pic_operand_p (rtx);
extern void print_operand (FILE *, rtx, int);
extern void print_operand_address (FILE *, rtx);
extern void mn10300_print_reg_list (FILE *, int);
extern int mn10300_get_live_callee_saved_regs (void);
extern void mn10300_gen_multiple_store (int);
extern void notice_update_cc (rtx, rtx);
extern enum reg_class mn10300_secondary_reload_class (enum reg_class,
						      enum machine_mode, rtx);
extern const char *output_tst (rtx, rtx);
extern int store_multiple_operation (rtx, enum machine_mode);
extern int symbolic_operand (rtx, enum machine_mode);
extern int impossible_plus_operand (rtx, enum machine_mode);

extern bool mn10300_wide_const_load_uses_clr (rtx operands[2]);

extern bool mn10300_function_value_regno_p (const unsigned int);
#endif /* RTX_CODE */

#ifdef TREE_CODE
extern struct rtx_def *function_arg (CUMULATIVE_ARGS *,
				     enum machine_mode, tree, int);
#endif /* TREE_CODE */

extern void expand_prologue (void);
extern void expand_epilogue (void);
extern int initial_offset (int, int);
extern int can_use_return_insn (void);
extern int mask_ok_for_mem_btst (int, int);
