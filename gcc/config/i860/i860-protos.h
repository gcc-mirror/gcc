/* Definitions of target machine for GNU compiler, for Intel 860.
   Copyright (C) 2000, 2003 Free Software Foundation, Inc.
   Hacked substantially by Ron Guilmette (rfg@monkeys.com) to cater to
   the whims of the System V Release 4 assembler.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Declare things which are defined in i860.c but called from
   insn-output.c.  */

#ifdef RTX_CODE
extern unsigned long sfmode_constant_to_ulong (rtx);
extern const char *output_load (rtx *);
extern const char *output_store (rtx *);
extern const char *output_move_double (rtx *);
extern const char *output_fp_move_double (rtx *);
extern const char *output_block_move (rtx *);
extern void output_load_address (rtx *);
extern int safe_insn_src_p (rtx, enum machine_mode);
extern int operand_clobbered_before_used_after (rtx, rtx);
extern int reg_or_0_operand (rtx, enum machine_mode);
extern int arith_operand (rtx, enum machine_mode);
extern int logic_operand (rtx, enum machine_mode);
extern int shift_operand (rtx, enum machine_mode);
extern int compare_operand (rtx, enum machine_mode);
extern int bte_operand (rtx, enum machine_mode);
extern int indexed_operand (rtx, enum machine_mode);
extern int load_operand (rtx, enum machine_mode);
extern int small_int (rtx, enum machine_mode);
extern int logic_int (rtx, enum machine_mode);
extern int call_insn_operand (rtx, enum machine_mode);
extern rtx i860_saveregs (void);
#ifdef TREE_CODE
extern void i860_va_start (tree, rtx);
extern rtx i860_va_arg (tree, tree);
#endif /* TREE_CODE */
#endif /* RTX_CODE */

extern void tdesc_section (void);

