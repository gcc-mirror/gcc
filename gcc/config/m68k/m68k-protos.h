/* Definitions of target machine for GNU compiler.  Sun 68000/68020 version.
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

/* Define functions defined in aux-output.c and used in templates.  */

#ifdef RTX_CODE
extern const char *output_move_const_into_data_reg PARAMS ((rtx *));
extern const char *output_move_simode_const PARAMS ((rtx *));
extern const char *output_move_simode PARAMS ((rtx *));
extern const char *output_move_himode PARAMS ((rtx *));
extern const char *output_move_qimode PARAMS ((rtx *));
extern const char *output_move_stricthi PARAMS ((rtx *));
extern const char *output_move_strictqi PARAMS ((rtx *));
extern const char *output_move_double PARAMS ((rtx *));
extern const char *output_move_const_single PARAMS ((rtx *));
extern const char *output_move_const_double PARAMS ((rtx *));
extern const char *output_btst PARAMS ((rtx *, rtx, rtx, rtx, int));
extern const char *output_scc_di PARAMS ((rtx, rtx, rtx, rtx));
extern const char *output_addsi3 PARAMS ((rtx *));
extern const char *output_andsi3 PARAMS ((rtx *));
extern const char *output_iorsi3 PARAMS ((rtx *));
extern const char *output_xorsi3 PARAMS ((rtx *));
extern void output_dbcc_and_branch PARAMS ((rtx *));
extern int const_uint32_operand PARAMS ((rtx, enum machine_mode));
extern int const_sint32_operand PARAMS ((rtx, enum machine_mode));
extern int floating_exact_log2 PARAMS ((rtx));
extern int not_sp_operand PARAMS ((rtx, enum machine_mode));
extern int valid_dbcc_comparison_p PARAMS ((rtx, enum machine_mode));
extern int extend_operator PARAMS ((rtx, enum machine_mode));
extern int strict_low_part_peephole_ok PARAMS ((enum machine_mode, rtx, rtx));

/* Functions from m68k.c used in macros.  */
extern int symbolic_operand PARAMS ((rtx, enum machine_mode));
extern int const_int_cost PARAMS ((rtx));
extern int standard_68881_constant_p PARAMS ((rtx));
extern int standard_sun_fpa_constant_p PARAMS ((rtx));
extern void print_operand_address PARAMS ((FILE *, rtx));
extern void print_operand PARAMS ((FILE *, rtx, int));
extern void notice_update_cc PARAMS ((rtx, rtx));
//extern void finalize_pic PARAMS ((rtx, enum machine_mode));
extern int general_src_operand PARAMS ((rtx, enum machine_mode));
extern int nonimmediate_src_operand PARAMS ((rtx, enum machine_mode));
extern int memory_src_operand PARAMS ((rtx, enum machine_mode));
extern int pcrel_address PARAMS ((rtx, enum machine_mode));
#endif /* RTX_CODE */

extern int flags_in_68881 PARAMS ((void));
extern void output_function_prologue PARAMS ((FILE *, int));
extern void output_function_epilogue PARAMS ((FILE *, int));
extern int use_return_insn PARAMS ((void));
extern void override_options PARAMS ((void));
extern void init_68881_table PARAMS ((void));
