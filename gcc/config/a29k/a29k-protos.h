/* Definitions of target machine for GNU compiler, for AMD Am29000 CPU.
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
/* This function is used to get the address of an object.  */
extern struct rtx_def *a29k_get_reloaded_address PARAMS ((rtx));
extern int gpc_reg_operand PARAMS ((rtx, enum machine_mode));
extern int long_const_operand PARAMS ((rtx, enum machine_mode));
extern int cint_8_operand PARAMS ((rtx, enum machine_mode));
extern int cint_16_operand PARAMS ((rtx, enum machine_mode));
extern int const_0_operand PARAMS ((rtx, enum machine_mode));
extern int const_8_operand PARAMS ((rtx, enum machine_mode));
extern int const_16_operand PARAMS ((rtx, enum machine_mode));
extern int const_24_operand PARAMS ((rtx, enum machine_mode));
extern int float_const_operand PARAMS ((rtx, enum machine_mode));
extern int gpc_reg_or_float_constant_operand PARAMS ((rtx, enum machine_mode));
extern int gpc_reg_or_integer_constant_operand PARAMS ((rtx, enum machine_mode));
extern int spec_reg_operand PARAMS ((rtx, enum machine_mode));
extern int accum_reg_operand PARAMS ((rtx, enum machine_mode));
extern int srcb_operand PARAMS ((rtx, enum machine_mode));
extern int cmplsrcb_operand PARAMS ((rtx, enum machine_mode));
extern int gpc_reg_or_immediate_operand PARAMS ((rtx, enum machine_mode));
extern int and_operand PARAMS ((rtx, enum machine_mode));
extern int add_operand PARAMS ((rtx, enum machine_mode));
extern int call_operand PARAMS ((rtx, enum machine_mode));
extern int in_operand PARAMS ((rtx, enum machine_mode));
extern int out_operand PARAMS ((rtx, enum machine_mode));
extern int reload_memory_operand PARAMS ((rtx, enum machine_mode));
extern void a29k_set_memflags PARAMS ((rtx, rtx));
extern int fp_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int branch_operator PARAMS ((rtx, enum machine_mode));
extern int load_multiple_operation PARAMS ((rtx, enum machine_mode));
extern int store_multiple_operation PARAMS ((rtx, enum machine_mode));
extern int masks_bits_for_special PARAMS ((rtx, rtx));
extern int epilogue_operand PARAMS ((rtx, enum machine_mode));
extern enum reg_class secondary_reload_class PARAMS ((enum reg_class,
						      enum machine_mode, rtx));
extern int incoming_reg PARAMS ((int, int));
extern void a29k_clobbers_to PARAMS ((rtx, rtx));
extern int needs_regstack_p PARAMS ((void));
extern int uses_local_reg_p PARAMS ((rtx));
extern int null_epilogue PARAMS ((void));
extern void print_operand PARAMS ((FILE *, rtx, int));
extern void a29k_compute_reg_names PARAMS ((void));
extern void output_prolog PARAMS ((FILE *, int));
extern void output_epilog PARAMS ((FILE *, int));
#endif /* RTX_CODE */
