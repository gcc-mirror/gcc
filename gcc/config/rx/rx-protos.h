/* Exported function prototypes from the Renesas RX backend.
   Copyright (C) 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
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

/* A few abbreviations to make the prototypes shorter.  */
#define Mmode 	enum machine_mode
#define Fargs	CUMULATIVE_ARGS
#define Rcode	enum rtx_code

extern void		rx_expand_prologue (void);
extern int		rx_initial_elimination_offset (int, int);

#ifdef RTX_CODE
extern void             rx_emit_stack_popm (rtx *, bool);
extern void             rx_emit_stack_pushm (rtx *);
extern void		rx_expand_epilogue (bool);
extern char *		rx_gen_move_template (rtx *, bool);
extern bool		rx_is_legitimate_constant (rtx);
extern bool		rx_is_restricted_memory_address (rtx, Mmode);
extern void		rx_notice_update_cc (rtx body, rtx insn);
extern void		rx_split_cbranch (Mmode, Rcode, rtx, rtx, rtx);
extern Mmode		rx_select_cc_mode (Rcode, rtx, rtx);
extern bool		rx_match_ccmode (rtx, Mmode);
#endif

#endif /* GCC_RX_PROTOS_H */
