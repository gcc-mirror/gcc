/* Definitions of target machine for GNU compiler for
   Motorola m88100 in an 88open OCS/BCS environment.
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com).
   Currently maintained by (gcc@dg-rtp.dg.com)

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
extern int m88k_debugger_offset PARAMS ((rtx, int));
extern void emit_bcnd PARAMS ((enum rtx_code, rtx));
extern void expand_block_move PARAMS ((rtx, rtx, rtx *));
extern void print_operand PARAMS ((FILE *, rtx, int));
extern void print_operand_address PARAMS ((FILE *, rtx));
extern const char *output_load_const_int PARAMS ((enum machine_mode, rtx *));
extern const char *output_load_const_float PARAMS ((rtx *));
extern const char *output_load_const_double PARAMS ((rtx *));
extern const char *output_load_const_dimode PARAMS ((rtx *));
extern const char *output_and PARAMS ((rtx[]));
extern const char *output_ior PARAMS ((rtx[]));
extern const char *output_xor PARAMS ((rtx[]));
extern const char *output_call PARAMS ((rtx[], rtx));

extern struct rtx_def *emit_test PARAMS ((enum rtx_code, enum machine_mode));
extern struct rtx_def *legitimize_address PARAMS ((int, rtx, rtx, rtx));
extern struct rtx_def *legitimize_operand PARAMS ((rtx, enum machine_mode));

extern int pic_address_needs_scratch PARAMS ((rtx));
extern int symbolic_address_p PARAMS ((rtx));
extern int condition_value PARAMS ((rtx));
extern int emit_move_sequence PARAMS ((rtx *, enum machine_mode, rtx));
extern int mostly_false_jump PARAMS ((rtx, rtx));
extern int real_power_of_2_operand PARAMS ((rtx, enum machine_mode));
extern int move_operand PARAMS ((rtx, enum machine_mode));
extern int call_address_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_0_operand PARAMS ((rtx, enum machine_mode));
extern int arith_operand PARAMS ((rtx, enum machine_mode));
extern int arith5_operand PARAMS ((rtx, enum machine_mode));
extern int arith32_operand PARAMS ((rtx, enum machine_mode));
extern int arith64_operand PARAMS ((rtx, enum machine_mode));
extern int int5_operand PARAMS ((rtx, enum machine_mode));
extern int int32_operand PARAMS ((rtx, enum machine_mode));
extern int add_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_bbx_mask_operand PARAMS ((rtx, enum machine_mode));
extern int real_or_0_operand PARAMS ((rtx, enum machine_mode));
extern int partial_ccmode_register_operand PARAMS ((rtx, enum machine_mode));
extern int relop PARAMS ((rtx, enum machine_mode));
extern int even_relop PARAMS ((rtx, enum machine_mode));
extern int odd_relop PARAMS ((rtx, enum machine_mode));
extern int relop_no_unsigned PARAMS ((rtx, enum machine_mode));
extern int equality_op PARAMS ((rtx, enum machine_mode));
extern int pc_or_label_ref PARAMS ((rtx, enum machine_mode));
extern int symbolic_operand PARAMS ((rtx, enum machine_mode));
#ifdef TREE_CODE
extern void m88k_va_start PARAMS ((tree, rtx));
#endif /* TREE_CODE */
#endif /* RTX_CODE */

#ifdef ANSI_PROTOTYPES
struct m88k_lang_independent_options;
#endif
extern void output_file_start PARAMS ((FILE *,
				       const struct m88k_lang_independent_options *,
				       int,
				       const struct m88k_lang_independent_options *,
				       int));

extern int null_prologue PARAMS ((void));
extern int integer_ok_for_set PARAMS ((unsigned));
extern void m88k_layout_frame PARAMS ((void));
extern void m88k_expand_prologue PARAMS ((void));
extern void m88k_expand_epilogue PARAMS ((void));
extern void output_function_profiler PARAMS ((FILE *, int, const char *, int));
extern void output_ascii PARAMS ((FILE *, const char *, int,
				  const char *, int));
extern void output_label PARAMS ((int));
extern struct rtx_def *m88k_builtin_saveregs PARAMS ((void));
extern enum m88k_instruction classify_integer PARAMS ((enum machine_mode, int));
extern int mak_mask_p PARAMS ((int));

#ifdef TREE_CODE
extern struct rtx_def *m88k_function_arg PARAMS ((CUMULATIVE_ARGS,
						  enum machine_mode, tree,
						  int));
extern struct rtx_def *m88k_va_arg PARAMS ((tree, tree));
extern tree m88k_build_va_list PARAMS ((void));
#endif /* TREE_CODE */
