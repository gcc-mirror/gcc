/* Definitions of target machine for GNU compiler. Matsushita MN10200 series
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
extern void print_operand PARAMS ((FILE *, rtx, int));
extern void print_operand_address PARAMS ((FILE *, rtx));
extern void notice_update_cc PARAMS ((rtx, rtx));
extern enum reg_class secondary_reload_class PARAMS ((enum reg_class,
						      enum machine_mode,
						      rtx, int));
extern const char *emit_a_shift PARAMS ((rtx, rtx *));
extern const char *output_tst PARAMS ((rtx, rtx));
extern int expand_a_shift PARAMS ((enum machine_mode, int, rtx[]));

extern int call_address_operand PARAMS ((rtx, enum machine_mode));
extern int extendpsi_operand PARAMS ((rtx, enum machine_mode));
extern int psimode_truncation_operand PARAMS ((rtx, enum machine_mode));
extern int constant_memory_operand PARAMS ((rtx, enum machine_mode));
extern int nshift_operator PARAMS ((rtx, enum machine_mode));
#endif /* RTX_CODE */

#ifdef TREE_CODE
extern struct rtx_def *function_arg PARAMS ((CUMULATIVE_ARGS *,
					     enum machine_mode, tree, int));
extern struct rtx_def *mn10200_va_arg PARAMS ((tree, tree));
extern int function_arg_partial_nregs PARAMS ((CUMULATIVE_ARGS *,
					       enum machine_mode, tree, int));
#endif /* TREE_CODE */

extern void asm_file_start PARAMS ((FILE *));
extern void expand_prologue PARAMS ((void));
extern void expand_epilogue PARAMS ((void));
extern int total_frame_size PARAMS ((void));
