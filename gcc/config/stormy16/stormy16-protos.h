/* Prototypes for exported functions defined in stormy16.c
   Copyright (C) 2000, 2001 Free Software Foundation, Inc.
   Contributed by Red Hat, Inc.

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



extern struct stormy16_stack_layout 
		    stormy16_compute_stack_layout PARAMS((void));
extern void         stormy16_expand_prologue      PARAMS ((void));
extern void         stormy16_expand_epilogue      PARAMS ((void));
extern int	stormy16_initial_elimination_offset PARAMS ((int, int));
extern int	direct_return PARAMS ((void));
extern int	stormy16_interrupt_function_p PARAMS ((void));
extern int	stormy16_epilogue_uses PARAMS ((int));

#if defined (TREE_CODE)
#  if defined (HAVE_MACHINE_MODES)
extern CUMULATIVE_ARGS stormy16_function_arg_advance 
  PARAMS ((CUMULATIVE_ARGS, enum machine_mode, tree, int));
#  endif
extern void    stormy16_setup_incoming_varargs 
  PARAMS ((CUMULATIVE_ARGS, int, tree, int *));
extern tree    stormy16_build_va_list PARAMS ((void));
extern void    stormy16_encode_section_info PARAMS ((tree));
#endif

#if defined (TREE_CODE) && defined (RTX_CODE)
extern void    stormy16_expand_builtin_va_start PARAMS ((int, tree, rtx));
extern rtx     stormy16_expand_builtin_va_arg PARAMS ((tree, tree));
extern void    stormy16_initialize_trampoline PARAMS ((rtx, rtx, rtx));
extern rtx     stormy16_function_value PARAMS ((tree, tree));
extern void    stormy16_asm_output_mi_thunk PARAMS ((FILE *, tree, int, tree));
#endif

#ifdef RTX_CODE
extern void stormy16_emit_cbranch PARAMS ((enum rtx_code, rtx));
extern char * stormy16_output_cbranch_hi PARAMS ((rtx, const char *, int, 
						  rtx));
extern char * stormy16_output_cbranch_si PARAMS ((rtx, const char *, int, 
						  rtx));
extern int stormy16_mode_dependent_address_p PARAMS ((rtx));
extern int stormy16_extra_constraint_p PARAMS ((rtx, int));

extern void stormy16_print_operand          PARAMS ((FILE *, rtx, int));
extern void stormy16_print_operand_address  PARAMS ((FILE *, rtx));

extern void stormy16_expand_casesi PARAMS ((rtx, rtx, rtx, rtx, rtx));
extern void stormy16_output_addr_vec PARAMS ((FILE *, rtx, rtx));
extern void stormy16_expand_call PARAMS ((rtx, rtx, rtx));
#endif

#if defined (HAVE_MACHINE_MODES) && defined (RTX_CODE)
extern int stormy16_ineqsi_operator PARAMS ((rtx, enum machine_mode));
extern int equality_operator PARAMS ((rtx, enum machine_mode));
extern int inequality_operator PARAMS ((rtx, enum machine_mode));
extern void stormy16_split_cbranch PARAMS ((enum machine_mode, 
					   rtx, rtx, rtx, rtx));
extern int  short_memory_operand PARAMS ((rtx, enum machine_mode));
extern enum reg_class stormy16_secondary_reload_class 
  PARAMS ((enum reg_class, enum machine_mode, rtx));
extern int  stormy16_carry_plus_operand PARAMS ((rtx, enum machine_mode));
extern enum reg_class stormy16_preferred_reload_class 
  PARAMS ((rtx, enum reg_class));
extern int stormy16_legitimate_address_p 
   PARAMS ((enum machine_mode, rtx, int));
extern void stormy16_split_move PARAMS ((enum machine_mode, rtx, rtx));
extern void stormy16_expand_move PARAMS ((enum machine_mode, rtx, rtx));
extern void stormy16_expand_arith PARAMS ((enum machine_mode, enum rtx_code, 
					   rtx, rtx, rtx, rtx));
extern int  shift_operator PARAMS ((rtx, enum machine_mode));
extern const char * stormy16_output_shift PARAMS ((enum machine_mode, 
						   enum rtx_code, 
						   rtx, rtx, rtx));
#endif

