/* Definitions of target machine for GNU compiler.  NS32000 version.
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com)

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

/* Prototypes for functions in ns32k.c */

#ifdef RTX_CODE
extern enum reg_class secondary_reload_class (enum reg_class,
					      enum machine_mode, rtx);
extern int reg_or_mem_operand (rtx, enum machine_mode);

extern void split_di (rtx[], int, rtx[], rtx[]);
extern void expand_block_move (rtx[]);
extern int global_symbolic_reference_mentioned_p (rtx, int);
extern void print_operand (FILE *, rtx, int);
extern void print_operand_address (FILE *, rtx);
extern const char *output_move_double (rtx *);
extern const char *output_shift_insn (rtx *);
extern int symbolic_reference_mentioned_p (rtx);
#endif /* RTX_CODE */

#ifdef TREE_CODE
extern int ns32k_return_pops_args (tree, tree, int);
#endif /* TREE_CODE */

extern int hard_regno_mode_ok (int, enum machine_mode);
extern int register_move_cost (enum reg_class, enum reg_class);
extern const char *output_move_dconst (int, const char *);
