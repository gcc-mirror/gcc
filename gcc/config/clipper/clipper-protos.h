/* Definitions of target machine for GNU compiler.  Clipper version.
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Holger Teutsch (holger@hotbso.rhein-main.de)

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
extern void clipper_va_start PARAMS ((int, tree, rtx));
extern rtx clipper_va_arg PARAMS ((tree, tree));
#endif /* TREE_CODE */
extern void clipper_movstr PARAMS ((rtx *));
extern void print_operand_address PARAMS ((FILE *, rtx));
extern const char *rev_cond_name PARAMS ((rtx));
extern int int_reg_operand PARAMS ((rtx, enum machine_mode));
extern int fp_reg_operand PARAMS ((rtx, enum machine_mode));
#endif /* RTX_CODE */

extern struct rtx_def *clipper_builtin_saveregs PARAMS ((void));
extern int clipper_frame_size PARAMS ((int));
extern void output_function_prologue PARAMS ((FILE *, int));
extern void output_function_epilogue PARAMS ((FILE *, int));
#ifdef TREE_CODE
extern tree clipper_build_va_list PARAMS ((void));
#endif /* TREE_CODE */
