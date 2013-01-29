/* Exported function prototypes from the Renesas RX backend.
   Copyright (C) 2008-2013 Free Software Foundation, Inc.
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

#ifndef GCC_RX_PROTOS_H
#define GCC_RX_PROTOS_H

extern bool             rx_can_use_simple_return (void);
extern void		rx_expand_epilogue (bool);
extern void		rx_expand_prologue (void);
extern int		rx_initial_elimination_offset (int, int);

#ifdef RTX_CODE
extern int		rx_adjust_insn_length (rtx, int);
extern int 		rx_align_for_label (rtx, int);
extern void             rx_emit_stack_popm (rtx *, bool);
extern void             rx_emit_stack_pushm (rtx *);
extern char *		rx_gen_move_template (rtx *, bool);
extern bool		rx_is_legitimate_constant (enum machine_mode, rtx);
extern bool		rx_is_restricted_memory_address (rtx,
							 enum machine_mode);
extern bool		rx_match_ccmode (rtx, enum machine_mode);
extern rtx		rx_maybe_pidify_operand (rtx, int);
extern void		rx_notice_update_cc (rtx, rtx);
extern void		rx_split_cbranch (enum machine_mode, enum rtx_code,
					  rtx, rtx, rtx);
extern enum machine_mode	rx_select_cc_mode (enum rtx_code, rtx, rtx);
#endif

#endif /* GCC_RX_PROTOS_H */
