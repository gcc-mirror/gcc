/* Prototypes for exported functions defined in c6x.c.
   Copyright (C) 2010-2014 Free Software Foundation, Inc.
   Contributed by CodeSourcery.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_C6X_PROTOS_H
#define GCC_C6X_PROTOS_H

/* Functions defined in c6x.c.  */

#ifdef RTX_CODE
extern void c6x_init_cumulative_args (CUMULATIVE_ARGS *, const_tree, rtx, int);
extern bool c6x_block_reg_pad_upward (enum machine_mode, const_tree, bool);

extern bool c6x_legitimate_address_p_1 (enum machine_mode, rtx, bool, bool);
extern bool c6x_mem_operand (rtx, enum reg_class, bool);
extern bool expand_move (rtx *, enum machine_mode);

extern bool c6x_long_call_p (rtx);
extern void c6x_expand_call (rtx, rtx, bool);
extern rtx c6x_expand_compare (rtx, enum machine_mode);
extern bool c6x_force_op_for_comparison_p (enum rtx_code, rtx);
extern bool c6x_expand_movmem (rtx, rtx, rtx, rtx, rtx, rtx);

extern rtx c6x_subword (rtx, bool);
extern void split_di (rtx *, int, rtx *, rtx *);
extern bool c6x_valid_mask_p (HOST_WIDE_INT);

extern char c6x_get_unit_specifier (rtx);

extern void c6x_final_prescan_insn(rtx insn, rtx *opvec, int noperands);

extern int c6x_nsaved_regs (void);
extern HOST_WIDE_INT c6x_initial_elimination_offset (int, int);
extern void c6x_expand_prologue (void);
extern void c6x_expand_epilogue (bool);

extern rtx c6x_return_addr_rtx (int);

extern void c6x_set_return_address (rtx, rtx);
#endif

extern void c6x_override_options (void);
extern void c6x_optimization_options (int, int);

extern void c6x_output_file_unwind (FILE *);

extern void c6x_function_end (FILE *, const char *);

#endif /* GCC_C6X_PROTOS_H */
