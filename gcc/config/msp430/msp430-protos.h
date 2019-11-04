/* Exported function prototypes from the TI MSP430 backend.
   Copyright (C) 2012-2019 Free Software Foundation, Inc.
   Contributed by Red Hat.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_MSP430_PROTOS_H
#define GCC_MSP430_PROTOS_H

bool	msp430_do_not_relax_short_jumps (void);
rtx	msp430_eh_return_stackadj_rtx (void);
void	msp430_expand_eh_return (rtx);
void	msp430_expand_epilogue (int);
void	msp430_expand_helper (rtx *operands, const char *, bool);
void	msp430_expand_prologue (void);
const char * msp430x_extendhisi (rtx *);
void	msp430_fixup_compare_operands (machine_mode, rtx *);
int	msp430_hard_regno_nregs_has_padding (int, machine_mode);
int	msp430_hard_regno_nregs_with_padding (int, machine_mode);
bool    msp430_hwmult_enabled (void);
rtx	msp430_incoming_return_addr_rtx (void);
void	msp430_init_cumulative_args (CUMULATIVE_ARGS *, tree, rtx, tree, int);
int	msp430_initial_elimination_offset (int, int);
bool    msp430_is_interrupt_func (void);
const char * msp430x_logical_shift_right (rtx);
const char * msp430_mcu_name (void);
void    msp430_output_aligned_decl_common (FILE *, const tree, const char *,
					   unsigned HOST_WIDE_INT, unsigned);
void	msp430_output_labelref (FILE *, const char *);
void	msp430_register_pragmas (void);
rtx	msp430_return_addr_rtx (int);
void	msp430_split_movsi (rtx *);
int msp430_split_addsi (rtx *);
void    msp430_start_function (FILE *, const char *, tree);
rtx	msp430_subreg (machine_mode, rtx, machine_mode, int);
bool    msp430_use_f5_series_hwmult (void);
bool	msp430_has_hwmult (void);
bool msp430_op_not_in_high_mem (rtx op);

#endif /* GCC_MSP430_PROTOS_H */
