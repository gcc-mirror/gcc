/* Prototypes for pj.c functions used in the md file & elsewhere.
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

void pj_expand_prologue PARAMS ((void));
void pj_expand_epilogue PARAMS ((void));
void pj_asm_output_opcode PARAMS ((FILE *, const char *));

#ifdef RTX_CODE
extern rtx pj_cmp_op0;
extern rtx pj_cmp_op1;
extern enum machine_mode pj_cmp_mode;
extern int pj_stuff_on_line;
extern const char *pj_standard_float_constant PARAMS ((rtx));
extern int pj_source_operand PARAMS ((rtx op, enum machine_mode mode));
extern int pj_signed_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int pj_unsigned_comparison_operator PARAMS ((rtx, enum machine_mode));
extern rtx pj_workout_arg_words PARAMS ((rtx, rtx));
extern void pj_machine_dependent_reorg PARAMS ((rtx));
extern void pj_print_operand PARAMS ((FILE * stream, rtx x, int code));
extern char *pj_output_addsi3 PARAMS ((rtx * operands));

#ifdef TREE_CODE
extern rtx pj_expand_builtin_va_arg PARAMS ((tree valist, tree type));
extern rtx pj_function_incoming_arg PARAMS ((CUMULATIVE_ARGS * args_so_far,
					     enum machine_mode promote_mode,
					     tree passed_type,
					     int named_arg));
#endif
#endif
