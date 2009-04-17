/* Initialization of uninitialized regs. 
   Copyright (C) 2007, 2008, 2009 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "regs.h"
#include "expr.h"
#include "tree-pass.h"
#include "basic-block.h"
#include "flags.h"
#include "df.h"

/* Check all of the uses of pseudo variables.  If any use that is MUST
   uninitialized, add a store of 0 immediately before it.  For
   subregs, this makes combine happy.  For full word regs, this makes
   other optimizations, like the register allocator and the reg-stack
   happy as well as papers over some problems on the arm and other
   processors where certain isa constraints cannot be handled by gcc.
   These are of the form where two operands to an insn my not be the
   same.  The ra will only make them the same if they do not
   interfere, and this can only happen if one is not initialized.

   There is also the unfortunate consequence that this may mask some
   buggy programs where people forget to initialize stack variable.
   Any programmer with half a brain would look at the uninitialized
   variable warnings.  */

static void
initialize_uninitialized_regs (void)
{
  basic_block bb;
  bitmap already_genned = BITMAP_ALLOC (NULL);

  if (optimize == 1)
    {
      df_live_add_problem ();
      df_live_set_all_dirty ();
    }

  df_analyze ();

  FOR_EACH_BB (bb)
    {
      rtx insn;
      bitmap lr = DF_LR_IN (bb);
      bitmap ur = DF_LIVE_IN (bb);
      bitmap_clear (already_genned);

      FOR_BB_INSNS (bb, insn)
	{
	  unsigned int uid = INSN_UID (insn);
	  df_ref *use_rec;
	  if (!INSN_P (insn))
	    continue;

	  for (use_rec = DF_INSN_UID_USES (uid); *use_rec; use_rec++)
	    {
	      df_ref use = *use_rec;
	      unsigned int regno = DF_REF_REGNO (use);

	      /* Only do this for the pseudos.  */
	      if (regno < FIRST_PSEUDO_REGISTER)
		continue;

	      /* Do not generate multiple moves for the same regno.
		 This is common for sequences of subreg operations.
		 They would be deleted during combine but there is no
		 reason to churn the system.  */
	      if (bitmap_bit_p (already_genned, regno))
		continue;

	      /* A use is MUST uninitialized if it reaches the top of
		 the block from the inside of the block (the lr test)
		 and no def for it reaches the top of the block from
		 outside of the block (the ur test).  */
	      if (bitmap_bit_p (lr, regno)
		  && (!bitmap_bit_p (ur, regno)))
		{
		  rtx move_insn;
		  rtx reg = DF_REF_REAL_REG (use);

		  bitmap_set_bit (already_genned, regno); 

		  start_sequence ();
		  emit_move_insn (reg, CONST0_RTX (GET_MODE (reg)));
		  move_insn = get_insns ();
		  end_sequence ();
		  emit_insn_before (move_insn, insn);
		  if (dump_file)
		    fprintf (dump_file, 
			     "adding initialization in %s of reg %d at in block %d for insn %d.\n", 
			     current_function_name (), regno, bb->index, uid);
		}
	    }
	}
    }

  if (optimize == 1)
    {
      if (dump_file) 
	df_dump (dump_file);
      df_remove_problem (df_live);
    }

  BITMAP_FREE (already_genned);
}

static bool
gate_initialize_regs (void)
{
  return optimize > 0;
}

static unsigned int
rest_of_handle_initialize_regs (void)
{
  initialize_uninitialized_regs ();
  return 0;
}

struct rtl_opt_pass pass_initialize_regs =
{
 {
  RTL_PASS,
  "init-regs",                          /* name */
  gate_initialize_regs,                 /* gate */
  rest_of_handle_initialize_regs,       /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_NONE,                              /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_dump_func |
  TODO_df_finish                        /* todo_flags_finish */
 }
};
