/* Definitions of target machine for GNU compiler.
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by O.M.Kellogg, DASA (oliver.kellogg@space.otn.dasa.de)

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
#ifdef TREE_CODE
extern struct rtx_def *function_arg PARAMS ((int, enum machine_mode, tree, int));
#endif /* TREE_CODE */
extern const char *movcnt_regno_adjust PARAMS ((rtx *));
extern const char *mod_regno_adjust PARAMS ((const char *, rtx *));
extern void notice_update_cc PARAMS ((rtx));
extern double get_double PARAMS ((rtx));
extern int memop_valid PARAMS ((rtx));
extern int mov_memory_operand PARAMS ((rtx, enum machine_mode));
extern int small_nonneg_const PARAMS ((rtx, enum machine_mode));
extern int zero_operand PARAMS ((rtx, enum machine_mode));
extern int b_mode_operand PARAMS ((rtx));
extern int unsigned_comparison_operator PARAMS ((rtx));
extern int next_cc_user_is_unsigned PARAMS ((rtx));
extern void print_operand PARAMS ((FILE *, rtx, int));
extern void print_operand_address PARAMS ((FILE *, rtx));
#endif /* RTX_CODE */

extern char *float_label PARAMS ((int, double));
extern const char *branch_or_jump PARAMS ((const char *, int));
extern int find_jmplbl PARAMS ((int));
extern int one_bit_set_p PARAMS ((int));
extern void check_section PARAMS ((enum section));
