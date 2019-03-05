/* Prototypes of target machine for Visium.
   Copyright (C) 2002-2019 Free Software Foundation, Inc.
   Contributed by C.Nettleton,J.P.Parkes and P.Garbett.

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

#ifndef GCC_VISIUM_PROTOS_H
#define GCC_VISIUM_PROTOS_H

extern unsigned int visium_data_alignment (tree, unsigned int);
extern void visium_init_expanders (void);
extern int visium_interrupt_function_p (void);
extern bool visium_can_use_return_insn_p (void);
extern void visium_expand_prologue (void);
extern void visium_expand_epilogue (void);
extern int visium_epilogue_uses (int);
extern void visium_profile_hook (void);
extern int visium_hard_regno_rename_ok (unsigned int, unsigned int);
extern int visium_initial_elimination_offset (int from, int to);
#ifdef RTX_CODE
extern void prepare_move_operands (rtx *, machine_mode);
extern bool ok_for_simple_move_operands (rtx *, machine_mode);
extern bool ok_for_simple_move_strict_operands (rtx *, machine_mode);
extern bool ok_for_simple_arith_logic_operands (rtx *, machine_mode);
extern void visium_initialize_trampoline (rtx, rtx, rtx);
extern int empty_delay_slot (rtx_insn *);
extern int gr5_hazard_bypass_p (rtx_insn *, rtx_insn *);
extern rtx visium_return_addr_rtx (int, rtx);
extern rtx visium_eh_return_handler_rtx (void);
extern rtx visium_dynamic_chain_address (rtx);
extern rtx visium_legitimize_reload_address (rtx, machine_mode, int, int,
					     int);
extern machine_mode visium_select_cc_mode (enum rtx_code, rtx, rtx);
extern void visium_split_cbranch (enum rtx_code, rtx, rtx, rtx);
extern const char *output_ubranch (rtx, rtx_insn *);
extern const char *output_cbranch (rtx, enum rtx_code, machine_mode, int,
				   rtx_insn *);
extern void visium_split_double_move (rtx *, machine_mode);
extern void visium_split_double_add (enum rtx_code, rtx, rtx, rtx);
extern void visium_expand_copysign (rtx *, machine_mode);
extern void visium_expand_int_cstore (rtx *, machine_mode);
extern void visium_expand_fp_cstore (rtx *, machine_mode);
extern void visium_split_cstore (enum rtx_code, rtx, rtx,
				 enum rtx_code, rtx, rtx);
extern int visium_expand_block_move (rtx *);
extern int visium_expand_block_set (rtx *);
extern unsigned int reg_or_subreg_regno (rtx);
#endif /* RTX_CODE */

extern rtl_opt_pass * make_pass_visium_reorg (gcc::context *);

#endif
