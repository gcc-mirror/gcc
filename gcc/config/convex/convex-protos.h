/* Definitions of target machine for GNU compiler.  Convex version.
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
extern int const_double_low_int PARAMS ((rtx));
extern int const_double_high_int PARAMS ((rtx));
extern const char *output_cmp PARAMS ((rtx, rtx, int));
extern const char *output_condjump PARAMS ((rtx, const char *, int));
extern const char *output_call PARAMS ((rtx, rtx *));
extern rtx simplify_for_convex PARAMS ((rtx));
extern void print_operand PARAMS ((FILE *, rtx, int));
extern void print_operand_address PARAMS ((FILE *, rtx));
extern void expand_movstr PARAMS ((rtx *));

extern int nonmemory_operand PARAMS ((rtx, enum machine_mode));
extern int nonmemory_cmpsf_operand PARAMS ((rtx, enum machine_mode));
#endif /* RTX_CODE */

#ifdef TREE_CODE
extern void asm_declare_function_name PARAMS ((FILE *, const char *, tree));
#endif /* TREE_CODE */

#ifdef REAL_VALUE_TYPE
extern int check_float_value PARAMS ((enum machine_mode, REAL_VALUE_TYPE *,
				      int));
extern void outfloat PARAMS ((FILE *, REAL_VALUE_TYPE, const char *,
			      const char *, const char *));
#endif /* REAL_VALUE_TYPE */

extern void psw_disable_float PARAMS ((void));
extern void init_convex PARAMS ((void));
extern void replace_arg_pushes PARAMS ((void));
extern void emit_ap_optimizations PARAMS ((void));
