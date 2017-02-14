/* Definitions of target machine for GNU compiler, EPIPHANY cpu.
   Copyright (C) 2000-2017 Free Software Foundation, Inc.
   Contributed by Embecosm on behalf of Adapteva, Inc.

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
extern machine_mode epiphany_select_cc_mode (enum rtx_code, rtx, rtx);

/* Define the function that build the compare insn for scc and bcc.  */
extern struct rtx_def *gen_compare_reg (machine_mode, enum rtx_code,
					machine_mode, rtx, rtx);
#endif

/* Declarations for various fns used in the .md file.  */
extern void epiphany_final_prescan_insn (rtx_insn *, rtx *, int);
extern bool epiphany_is_long_call_p (rtx);
extern bool epiphany_small16 (rtx);
bool epiphany_uninterruptible_p (tree decl);
bool epiphany_call_uninterruptible_p (rtx mem);
extern rtx sfunc_symbol (const char *name);

extern void epiphany_expand_prologue (void);
extern void epiphany_expand_epilogue (int);
extern int epiphany_initial_elimination_offset (int, int);
extern void epiphany_init_expanders (void);
extern int hard_regno_mode_ok (int regno, machine_mode mode);
#ifdef HARD_CONST
extern void emit_set_fp_mode (int entity, int mode, int prev_mode,
			      HARD_REG_SET regs_live);
#endif
extern void epiphany_insert_mode_switch_use (rtx_insn *insn, int, int);
extern void epiphany_expand_set_fp_mode (rtx *operands);
extern int epiphany_mode_needed (int entity, rtx_insn *insn);
extern int epiphany_mode_after (int entity, int last_mode, rtx_insn *insn);
extern bool epiphany_epilogue_uses (int regno);
extern bool epiphany_optimize_mode_switching (int entity);
extern bool epiphany_is_interrupt_p (tree);
extern unsigned epiphany_special_round_type_align (tree, unsigned, unsigned);
extern unsigned epiphany_adjust_field_align (tree, unsigned);
extern void epiphany_start_function (FILE *f, const char *name, tree decl);
extern bool epiphany_regno_rename_ok (unsigned src, unsigned dst);

/* Also declared in insn-attr.h, but files generated from epiphany.md
   can't / won't include that.  In particular:
   PR other/55523: gencondmd file includes / dependencies are messed up,
   it uses peephole2 predicates without having all the necessary headers.  */
extern int get_attr_sched_use_fpu (rtx_insn *);

