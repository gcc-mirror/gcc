/* Definitions of target machine for GNU compiler.  NS32000 version.
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com)

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

/* Prototypes for functions in ns32k.c */

#ifdef RTX_CODE
extern int calc_address_cost PARAMS ((rtx));
extern enum reg_class secondary_reload_class PARAMS ((enum reg_class,
						      enum machine_mode, rtx));
extern int reg_or_mem_operand PARAMS ((rtx, enum machine_mode));

extern void split_di PARAMS ((rtx[], int, rtx[], rtx[]));
extern void expand_block_move PARAMS ((rtx[]));
extern int global_symbolic_reference_mentioned_p PARAMS ((rtx, int));
extern void print_operand PARAMS ((FILE *, rtx, int));
extern void print_operand_address PARAMS ((FILE *, rtx));
extern const char *output_move_double PARAMS ((rtx *));
extern const char *output_shift_insn PARAMS ((rtx *));
extern int symbolic_reference_mentioned_p PARAMS ((rtx));
#endif /* RTX_CODE */

#ifdef TREE_CODE
extern int ns32k_comp_type_attributes PARAMS ((tree, tree));
extern int ns32k_return_pops_args PARAMS ((tree, tree, int));
extern int ns32k_valid_decl_attribute_p PARAMS ((tree, tree, tree, tree));
extern int ns32k_valid_type_attribute_p PARAMS ((tree, tree, tree, tree));
#endif /* TREE_CODE */

extern int hard_regno_mode_ok PARAMS ((int, enum machine_mode));
extern int register_move_cost PARAMS ((enum reg_class, enum reg_class));
extern const char *output_move_dconst PARAMS ((int, const char *));
