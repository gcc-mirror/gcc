/* Definitions of target machine for GNU compiler. Matsushita MN10300 series
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Jeff Law (law@cygnus.com).

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
extern void mn10300_va_start PARAMS ((int, tree, rtx));
#endif /* TREE_CODE */

extern struct rtx_def *legitimize_address PARAMS ((rtx, rtx, enum machine_mode));
extern void print_operand PARAMS ((FILE *, rtx, int));
extern void print_operand_address PARAMS ((FILE *, rtx));
extern void notice_update_cc PARAMS ((rtx, rtx));
extern enum reg_class secondary_reload_class PARAMS ((enum reg_class,
						      enum machine_mode, rtx));
extern char *output_tst PARAMS ((rtx, rtx));
extern int symbolic_operand PARAMS ((rtx, enum machine_mode));
extern int call_address_operand PARAMS ((rtx, enum machine_mode));
extern int impossible_plus_operand PARAMS ((rtx, enum machine_mode));
extern int const_8bit_operand PARAMS ((rtx, enum machine_mode));
#endif /* RTX_CODE */

#ifdef TREE_CODE
extern struct rtx_def *function_arg PARAMS ((CUMULATIVE_ARGS *,
					     enum machine_mode, tree, int));
extern int function_arg_partial_nregs PARAMS ((CUMULATIVE_ARGS *,
					       enum machine_mode, tree, int));
extern struct rtx_def *mn10300_va_arg PARAMS ((tree, tree));
#endif /* TREE_CODE */

extern struct rtx_def *mn10300_builtin_saveregs PARAMS ((void));
extern void asm_file_start PARAMS ((FILE *));
extern void expand_prologue PARAMS ((void));
extern void expand_epilogue PARAMS ((void));
extern int initial_offset PARAMS ((int, int));
extern int can_use_return_insn PARAMS ((void));
extern int mask_ok_for_mem_btst PARAMS ((int, int));

