/* Prototypes for exported functions defined in xstormy16.c
   Copyright (C) 2000, 2001, 2003 Free Software Foundation, Inc.
   Contributed by Red Hat, Inc.

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



extern struct xstormy16_stack_layout xstormy16_compute_stack_layout (void);
extern void xstormy16_expand_prologue (void);
extern void xstormy16_expand_epilogue (void);
extern int xstormy16_initial_elimination_offset (int, int);
extern int direct_return (void);
extern int xstormy16_interrupt_function_p (void);
extern int xstormy16_epilogue_uses (int);
extern void xstormy16_function_profiler (void);

#if defined (TREE_CODE)
#  if defined (HAVE_MACHINE_MODES)
extern CUMULATIVE_ARGS xstormy16_function_arg_advance 
 (CUMULATIVE_ARGS, enum machine_mode, tree, int);
extern rtx xstormy16_function_arg
 (CUMULATIVE_ARGS, enum machine_mode, tree, int);
#  endif
extern void xstormy16_setup_incoming_varargs 
 (CUMULATIVE_ARGS, int, tree, int *);
#endif

#if defined (TREE_CODE) && defined (RTX_CODE)
extern void xstormy16_expand_builtin_va_start (tree, rtx);
extern rtx xstormy16_expand_builtin_va_arg (tree, tree);
extern void xstormy16_initialize_trampoline (rtx, rtx, rtx);
extern rtx xstormy16_function_value (tree, tree);
#endif

#ifdef RTX_CODE
extern void xstormy16_emit_cbranch (enum rtx_code, rtx);
extern char *xstormy16_output_cbranch_hi (rtx, const char *, int, rtx);
extern char *xstormy16_output_cbranch_si (rtx, const char *, int, rtx);
extern int xstormy16_mode_dependent_address_p (rtx);
extern int xstormy16_extra_constraint_p (rtx, int);

extern void xstormy16_print_operand (FILE *, rtx, int);
extern void xstormy16_print_operand_address (FILE *, rtx);

extern void xstormy16_expand_casesi (rtx, rtx, rtx, rtx, rtx);
extern void xstormy16_output_addr_vec (FILE *, rtx, rtx);
extern void xstormy16_expand_call (rtx, rtx, rtx);
#endif

#if defined (HAVE_MACHINE_MODES) && defined (RTX_CODE)
extern int xstormy16_ineqsi_operator (rtx, enum machine_mode);
extern int equality_operator (rtx, enum machine_mode);
extern int inequality_operator (rtx, enum machine_mode);
extern void xstormy16_split_cbranch (enum machine_mode, rtx, rtx, rtx, rtx);
extern int  short_memory_operand (rtx, enum machine_mode);
extern int  nonimmediate_nonstack_operand (rtx, enum machine_mode);
extern enum reg_class xstormy16_secondary_reload_class 
 (enum reg_class, enum machine_mode, rtx);
extern int  xstormy16_carry_plus_operand (rtx, enum machine_mode);
extern int  xs_hi_general_operand (rtx, enum machine_mode);
extern int  xs_hi_nonmemory_operand (rtx, enum machine_mode);
extern enum reg_class xstormy16_preferred_reload_class (rtx, enum reg_class);
extern int xstormy16_legitimate_address_p (enum machine_mode, rtx, int);
extern void xstormy16_split_move (enum machine_mode, rtx, rtx);
extern void xstormy16_expand_move (enum machine_mode, rtx, rtx);
extern void xstormy16_expand_arith (enum machine_mode, enum rtx_code, 
				    rtx, rtx, rtx, rtx);
extern int  shift_operator (rtx, enum machine_mode);
extern const char * xstormy16_output_shift (enum machine_mode, enum rtx_code, 
					    rtx, rtx, rtx);
#endif

