/* Prototypes for fr30.c functions used in the md file & elsewhere.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

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

extern void  fr30_expand_prologue PARAMS ((void));
extern void  fr30_expand_epilogue PARAMS ((void));
extern unsigned int fr30_compute_frame_size PARAMS ((int, int));

#ifdef RTX_CODE
extern int   fr30_check_multiple_regs   PARAMS ((rtx *, int, int));
extern void  fr30_print_operand         PARAMS ((FILE *, rtx, int));
extern void  fr30_print_operand_address PARAMS ((FILE *, rtx));
extern rtx   fr30_move_double		PARAMS ((rtx *));
#ifdef TREE_CODE
extern rtx   fr30_va_arg                PARAMS ((tree, tree));
#endif /* TREE_CODE */
#ifdef HAVE_MACHINE_MODES
#define Mmode enum machine_mode
extern int   stack_add_operand          PARAMS ((rtx, Mmode));
extern int   add_immediate_operand      PARAMS ((rtx, Mmode));
extern int   high_register_operand      PARAMS ((rtx, Mmode));
extern int   low_register_operand       PARAMS ((rtx, Mmode));
extern int   call_operand               PARAMS ((rtx, Mmode));
extern int   di_operand 		PARAMS ((rtx, Mmode));
extern int   nonimmediate_di_operand 	PARAMS ((rtx, Mmode));
#undef Mmode
#endif /* HAVE_MACHINE_MODES */
#endif /* RTX_CODE */

#ifdef TREE_CODE
extern int   fr30_num_arg_regs               PARAMS ((int, tree));
extern int   fr30_function_arg_partial_nregs PARAMS ((CUMULATIVE_ARGS, int, tree, int));
extern void  fr30_setup_incoming_varargs     PARAMS ((CUMULATIVE_ARGS, int, tree, int *));
#endif /* TREE_CODE */
