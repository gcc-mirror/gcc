/* Definitions of target machine for GNU compiler, for Intel 860.
   Copyright (C) 2000 Free Software Foundation, Inc.
   Hacked substantially by Ron Guilmette (rfg@monkeys.com) to cater to
   the whims of the System V Release 4 assembler.

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

/* Declare things which are defined in i860.c but called from
   insn-output.c.  */

#ifdef RTX_CODE
extern unsigned long sfmode_constant_to_ulong PARAMS ((rtx));
extern const char *output_load PARAMS ((rtx *));
extern const char *output_store PARAMS ((rtx *));
extern const char *output_move_double PARAMS ((rtx *));
extern const char *output_fp_move_double PARAMS ((rtx *));
extern const char *output_block_move PARAMS ((rtx *));
extern const char *output_delay_insn PARAMS ((rtx));
#if 0
extern const char *output_delayed_branch PARAMS ((const char *, rtx *, rtx));
#endif
extern void output_load_address PARAMS ((rtx *));
extern int safe_insn_src_p PARAMS ((rtx, enum machine_mode));
extern int operand_clobbered_before_used_after PARAMS ((rtx, rtx));
extern int single_insn_src_p PARAMS ((rtx, enum machine_mode));
extern int reg_or_0_operand PARAMS ((rtx, enum machine_mode));
extern int arith_operand PARAMS ((rtx, enum machine_mode));
extern int logic_operand PARAMS ((rtx, enum machine_mode));
extern int shift_operand PARAMS ((rtx, enum machine_mode));
extern int compare_operand PARAMS ((rtx, enum machine_mode));
extern int bte_operand PARAMS ((rtx, enum machine_mode));
extern int indexed_operand PARAMS ((rtx, enum machine_mode));
extern int load_operand PARAMS ((rtx, enum machine_mode));
extern int small_int PARAMS ((rtx, enum machine_mode));
extern int logic_int PARAMS ((rtx, enum machine_mode));
extern int call_insn_operand PARAMS ((rtx, enum machine_mode));
extern rtx i860_saveregs PARAMS ((void));
#ifdef TREE_CODE
extern void i860_va_start PARAMS ((int, tree, rtx));
extern rtx i860_va_arg PARAMS ((tree, tree));
#endif /* TREE_CODE */
#endif /* RTX_CODE */

#ifdef TREE_CODE
extern tree i860_build_va_list PARAMS ((void));
#endif /* TREE_CODE */
