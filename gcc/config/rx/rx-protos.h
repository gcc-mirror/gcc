/* Exported function prototypes from the Renesas RX backend.
   Copyright (C) 2008-2016 Free Software Foundation, Inc.
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

bool is_interrupt_func (const_tree decl);
bool is_fast_interrupt_func (const_tree decl);

/* rx_atomic_sequence is used to emit the header and footer
   of an atomic sequence.  It's supposed to be used in a scope.
   When constructed, it will emit the atomic sequence header insns.
   When destructred (goes out of scope), it will emit the
   corresponding atomic sequence footer insns.  */
class rx_atomic_sequence
{
public:
  rx_atomic_sequence (const_tree fun_decl);
  ~rx_atomic_sequence (void);

private:
  rx_atomic_sequence (void);
  rx_atomic_sequence (const rx_atomic_sequence&);
  rx_atomic_sequence& operator = (const rx_atomic_sequence&);

  rtx m_prev_psw_reg;
};

#ifdef RTX_CODE
extern int		rx_adjust_insn_length (rtx_insn *, int);
extern int 		rx_align_for_label (rtx, int);
extern void             rx_emit_stack_popm (rtx *, bool);
extern void             rx_emit_stack_pushm (rtx *);
extern char *		rx_gen_move_template (rtx *, bool);
extern bool		rx_is_legitimate_constant (machine_mode, rtx);
extern bool		rx_is_restricted_memory_address (rtx,
							 machine_mode);
extern bool		rx_match_ccmode (rtx, machine_mode);
extern rtx		rx_maybe_pidify_operand (rtx, int);
extern void		rx_notice_update_cc (rtx, rtx);
extern void		rx_split_cbranch (machine_mode, enum rtx_code,
					  rtx, rtx, rtx);
extern machine_mode	rx_select_cc_mode (enum rtx_code, rtx, rtx);
#endif

#endif /* GCC_RX_PROTOS_H */
