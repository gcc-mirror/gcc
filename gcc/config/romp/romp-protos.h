/* Definitions of target machine for GNU compiler, for ROMP chip.
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@nyu.edu)

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
extern int next_insn_tests_no_unsigned PARAMS ((rtx));
extern void update_cc PARAMS ((rtx, rtx));
extern int restore_compare_p PARAMS ((rtx));
extern void print_operand PARAMS ((FILE *, rtx, int));
extern rtx get_symref PARAMS ((const char *));
extern int check_precision PARAMS ((enum machine_mode, rtx, rtx));
extern const char *output_fpop PARAMS ((enum rtx_code, rtx, rtx, rtx, rtx));
extern int constant_pool_address_operand PARAMS ((rtx, enum machine_mode));
extern int romp_symbolic_operand PARAMS ((rtx, enum machine_mode));
extern int zero_memory_operand PARAMS ((rtx, enum machine_mode));
extern int short_memory_operand PARAMS ((rtx, enum machine_mode));
extern int symbolic_memory_operand PARAMS ((rtx, enum machine_mode));
extern int current_function_operand PARAMS ((rtx, enum machine_mode));
extern int constant_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_cint_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_any_cint_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_D_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_add_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_and_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_mem_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_nonsymb_mem_operand PARAMS ((rtx, enum machine_mode));
extern int romp_operand PARAMS ((rtx, enum machine_mode));
extern int reg_0_operand PARAMS ((rtx, enum machine_mode));
extern int reg_15_operand PARAMS ((rtx, enum machine_mode));
extern int float_binary PARAMS ((rtx, enum machine_mode));
extern int float_unary PARAMS ((rtx, enum machine_mode));
extern int float_conversion PARAMS ((rtx, enum machine_mode));
extern void romp_initialize_trampoline PARAMS ((rtx, rtx, rtx));
#endif /* RTX_CODE */

extern int first_reg_to_save PARAMS ((void));
extern int romp_pushes_stack PARAMS ((void));
extern int romp_using_r14 PARAMS ((void));
extern int null_epilogue PARAMS ((void));
extern int romp_sa_size PARAMS ((void));
extern int romp_makes_calls PARAMS ((void));
extern void output_encoded_offset PARAMS ((FILE *, unsigned));
extern int romp_debugger_auto_correction PARAMS ((int));
extern int romp_debugger_arg_correction PARAMS ((int));
extern const char *output_in_line_mul PARAMS ((void));
