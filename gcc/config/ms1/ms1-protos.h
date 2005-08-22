/* Prototypes for exported functions defined in ms1.c
   Copyright (C) 2005 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful,but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to the Free
   Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.  */

extern void         ms1_init_expanders	 (void);
extern void         ms1_expand_prologue	 (void);
extern void         ms1_expand_epilogue	 (enum epilogue_type);
extern unsigned     ms1_compute_frame_size (int);
extern void	    ms1_override_options (void);
extern int	    ms1_initial_elimination_offset (int, int);
extern const char * ms1_asm_output_opcode (FILE *, const char *);
extern int          ms1_epilogue_uses	 (int);

#ifdef TREE_CODE
extern const char * ms1_cannot_inline_p	 (tree);
extern int          ms1_function_arg_boundary (enum machine_mode, tree);
extern void         ms1_function_arg_advance (CUMULATIVE_ARGS *, enum machine_mode, tree,  int);
#endif

#ifdef RTX_CODE
extern void	    ms1_expand_eh_return (rtx *);
extern void	    ms1_emit_eh_epilogue (rtx *);
extern void         ms1_print_operand	 (FILE *, rtx, int);
extern void         ms1_print_operand_address (FILE *, rtx);
extern int          ms1_check_split	 (rtx, enum machine_mode);
extern int          ms1_reg_ok_for_base_p (rtx, int);
extern int          ms1_legitimate_address_p (enum machine_mode, rtx, int);
/* Predicates for machine description.  */
extern int          uns_arith_operand	 (rtx, enum machine_mode);
extern int          arith_operand	 (rtx, enum machine_mode);
extern int          reg_or_0_operand	 (rtx, enum machine_mode);
extern int	    big_const_operand	 (rtx, enum machine_mode);
extern int	    single_const_operand (rtx, enum machine_mode);
extern void	    ms1_emit_cbranch	 (enum rtx_code, rtx, rtx, rtx);
extern void	    ms1_set_memflags	 (rtx);
extern rtx	    ms1_return_addr_rtx	 (int);
extern void	    ms1_split_words	 (enum machine_mode, enum machine_mode, rtx *);
extern void	    ms1_final_prescan_insn (rtx, rtx *, int);
#endif

#ifdef TREE_CODE
#ifdef RTX_CODE
extern void         ms1_init_cumulative_args (CUMULATIVE_ARGS *, tree, rtx, tree, int);
extern rtx          ms1_function_arg	 (const CUMULATIVE_ARGS *, enum machine_mode, tree, int, int);
extern void	    ms1_va_start	 (tree, rtx);
extern enum reg_class ms1_secondary_reload_class (enum reg_class, enum machine_mode, rtx);
extern rtx	    ms1_function_value	 (tree, enum machine_mode, tree);
#endif
#endif
