/* Prototypes for Renesas RL78 processors
   Copyright (C) 2011-2024 Free Software Foundation, Inc.
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


#include "tree.h"  /* For ERROR_MARK.  */

const char *    rl78_addsi3_internal (rtx *, unsigned int);
void		rl78_emit_eh_epilogue (rtx);
void		rl78_expand_compare (rtx *);
void		rl78_expand_movsi (rtx *);
void		rl78_split_movsi (rtx *, machine_mode);
void 		rl78_split_movdi (rtx *, enum machine_mode);
int		rl78_force_nonfar_2 (rtx *, rtx (*gen)(rtx,rtx));
int		rl78_force_nonfar_3 (rtx *, rtx (*gen)(rtx,rtx,rtx));
void		rl78_expand_eh_epilogue (rtx);
void		rl78_expand_epilogue (void);
void		rl78_expand_prologue (void);
int		rl78_far_p (rtx x);
bool		rl78_hl_b_c_addr_p (rtx);
int		rl78_initial_elimination_offset (int, int);
bool		rl78_as_legitimate_address (machine_mode, rtx,
					    bool, addr_space_t,
					    code_helper = ERROR_MARK);
int		rl78_legitimize_reload_address (rtx *, machine_mode, int,int, int);
enum reg_class	rl78_mode_code_base_reg_class (machine_mode, addr_space_t, int, int);
bool		rl78_peep_movhi_p (rtx *);
bool		rl78_real_insns_ok (void);
void		rl78_register_pragmas (void);
bool		rl78_regno_mode_code_ok_for_base_p (int, machine_mode, addr_space_t, int, int);
void		rl78_setup_peep_movhi (rtx *);
bool		rl78_virt_insns_ok (void);

bool		rl78_es_addr (rtx);
rtx		rl78_es_base (rtx);

bool		rl78_flags_already_set (rtx, rtx);
void		rl78_output_symbol_ref (FILE *, rtx);
void		rl78_output_labelref (FILE *, const char *);
int		rl78_saddr_p (rtx x);
int		rl78_sfr_p (rtx x);
void		rl78_output_aligned_common (FILE *, tree, const char *,
					    int, int, int);

int		rl78_one_far_p (rtx *operands, int num_operands);

#ifdef RTX_CODE
#ifdef HAVE_MACHINE_MODES

rtx rl78_emit_libcall (const char*, enum rtx_code,
                       enum machine_mode, enum machine_mode,
                       int, rtx*);

#endif
#endif
