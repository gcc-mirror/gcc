/* Communication between reload.c and reload1.c.
   Copyright (C) 1987, 1991 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* See reload.c and reload1.c for comments on these variables.  */

/* Maximum number of reloads we can need.  */
#define MAX_RELOADS (2 * MAX_RECOG_OPERANDS * (MAX_REGS_PER_ADDRESS + 1))

extern rtx reload_in[MAX_RELOADS];
extern rtx reload_out[MAX_RELOADS];
extern rtx reload_in_reg[MAX_RELOADS];
extern enum reg_class reload_reg_class[MAX_RELOADS];
extern enum machine_mode reload_inmode[MAX_RELOADS];
extern enum machine_mode reload_outmode[MAX_RELOADS];
extern char reload_strict_low[MAX_RELOADS];
extern char reload_optional[MAX_RELOADS];
extern int reload_inc[MAX_RELOADS];
extern int reload_needed_for_multiple[MAX_RELOADS];
extern rtx reload_needed_for[MAX_RELOADS];
extern int reload_secondary_reload[MAX_RELOADS];
extern int reload_secondary_p[MAX_RELOADS];
extern int n_reloads;

extern rtx reload_reg_rtx[MAX_RELOADS];

enum reload_when_needed
{
  RELOAD_FOR_INPUT_RELOAD_ADDRESS,
  RELOAD_FOR_OUTPUT_RELOAD_ADDRESS,
  RELOAD_FOR_OPERAND_ADDRESS,
  /* The following two are not fully implemented.
     They are used in emitting insns, but they aren't distinguished from
     RELOAD_OTHER when computing the number of spills.  What they accomplish
     is to avoid precluding inheritance of already loaded values
     for input reloads when there are also output reloads.  */
  RELOAD_FOR_INPUT,
  RELOAD_FOR_OUTPUT,
  RELOAD_OTHER
};

extern enum reload_when_needed reload_when_needed[MAX_RELOADS];

extern rtx *reg_equiv_constant;
extern rtx *reg_equiv_address;
extern rtx *reg_equiv_mem;

/* All the "earlyclobber" operands of the current insn
   are recorded here.  */
extern int n_earlyclobbers;
extern rtx reload_earlyclobbers[MAX_RECOG_OPERANDS];

/* First uid used by insns created by reload in this function.
   Used in find_equiv_reg.  */
extern int reload_first_uid;

/* Nonzero if indirect addressing is supported when the innermost MEM is
   of the form (MEM (SYMBOL_REF sym)).  It is assumed that the level to
   which these are valid is the same as spill_indirect_levels, above.   */

extern char indirect_symref_ok;

/* Nonzero if an address (plus (reg frame_pointer) (reg ...)) is valid.  */
extern char double_reg_address_ok;

void init_reload ();
void find_reloads ();
void subst_reloads ();
rtx eliminate_regs ();
