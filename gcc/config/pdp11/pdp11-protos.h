/* Definitions of target machine for GNU compiler, for the pdp-11
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Michael K. Gschwind (mike@vlsivie.tuwien.ac.at).

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

/* declarations */
#ifdef RTX_CODE
extern int arith_operand PARAMS ((rtx, enum machine_mode));
extern int const_immediate_operand PARAMS ((rtx, enum machine_mode));
extern int expand_shift_operand PARAMS ((rtx, enum machine_mode));
extern int immediate15_operand PARAMS ((rtx, enum machine_mode));
extern int simple_memory_operand PARAMS ((rtx, enum machine_mode));
extern int comp_operator PARAMS ((rtx, enum machine_mode));

extern int legitimate_address_p PARAMS ((enum machine_mode, rtx));
extern void notice_update_cc_on_set PARAMS ((rtx, rtx));
extern void output_addr_const_pdp11 PARAMS ((FILE *, rtx));
extern const char *output_move_double PARAMS ((rtx *));
extern const char *output_move_quad PARAMS ((rtx *));
extern const char *output_block_move PARAMS ((rtx *));
extern void print_operand_address PARAMS ((FILE *, rtx));
extern int register_move_cost PARAMS ((enum reg_class, enum reg_class));
extern int comparison_operator_index PARAMS ((rtx));
#endif /* RTX_CODE */

extern void output_ascii PARAMS ((FILE *, const char *, int));
extern const char *output_jump PARAMS ((const char *, const char *, int));
