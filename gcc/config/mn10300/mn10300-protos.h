/* Definitions of target machine for GNU compiler. Matsushita MN10300 series
   Copyright (C) 2000-2017 Free Software Foundation, Inc.
   Contributed by Jeff Law (law@cygnus.com).

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

#ifdef RTX_CODE
extern rtx   mn10300_legitimize_pic_address (rtx, rtx);
extern int   mn10300_legitimate_pic_operand_p (rtx);
extern rtx   mn10300_legitimize_reload_address (rtx, machine_mode,
						int, int, int);
extern bool  mn10300_function_value_regno_p (const unsigned int);
extern unsigned int   mn10300_get_live_callee_saved_regs (unsigned int *);
extern const char *mn10300_output_add (rtx[3], bool);
extern void  mn10300_print_operand (FILE *, rtx, int);
extern void  mn10300_print_operand_address (FILE *, rtx);
extern void  mn10300_print_reg_list (FILE *, int);
extern machine_mode mn10300_select_cc_mode (enum rtx_code, rtx, rtx);
extern unsigned int mn10300_store_multiple_regs (rtx);
extern int   mn10300_symbolic_operand (rtx, machine_mode);
extern void  mn10300_split_cbranch (machine_mode, rtx, rtx);
extern int   mn10300_split_and_operand_count (rtx);
extern bool  mn10300_match_ccmode (rtx, machine_mode);
#endif /* RTX_CODE */

extern bool  mn10300_regno_in_class_p (unsigned, int, bool);
extern bool  mn10300_can_use_rets_insn (void);
extern bool  mn10300_can_use_retf_insn (void);
extern void  mn10300_expand_prologue (void);
extern void  mn10300_expand_epilogue (void);
extern int   mn10300_initial_offset (int, int);
extern int   mn10300_frame_size (void);
