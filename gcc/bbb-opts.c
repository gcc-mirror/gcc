/* Bebbo's Optimizations.
 Copyright (C) 2010-2016 Free Software Foundation, Inc.

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

/**
 * SBF (Stefan "Bebbo" Franke):
 *
 * This pass performs multiple optimizations.
 *
 * #1 propagate_moves
 * check if a->b->a can be moved out of a loop.
 *
 * #2 strcpy
 * move a,reg
 * move reg,b
 * cmp  #0,reg
 * jne/jeq
 */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "df.h"
#include "tm_p.h"
#include "insn-config.h"
#include "recog.h"
#include "cfgrtl.h"
#include "emit-rtl.h"
#include "tree-pass.h"
#include "conditions.h"
#include <vector>
#include <map>

extern void
dump_insns (char const * name);

/*
 *  #1 propagate a->b->a moves out of a loop.
 *
 * consider a loop:
 *
 * .L1
 *   ...
 *   move d0, a0 ; (1)
 *   ...
 *   move xy, (a0)+
 *   ...
 *   move a0, d0 ; (2)
 *   ...
 *   jxx .L1
 *
 *  Then the statements (1) and (2) can be moved out of the loop:
 *
 *   move d0, a0 ; (3)
 * .L1
 *   ...
 *   move *, (a0)+ ; a0 is modified somehow
 *   ...
 *   jxx .L1
 *   move a0, d0 ; (4)
 *
 *  if all criteria are met:
 *
 *  a) no other jump to .L1 -> (LABEL_NUSES(insn) == 1)
 *  b) no other use of d0 inside the loop
 *  c) no other use of a0 before (1)
 *  d) no other use of a1 after (2)
 *
 *  Optional:
 *  - omit (4) if d0 is dead
 *
 *  this will e.g. convert
 .L6:
 move.l d0,a1
 move.b (a1)+,d1
 move.l a1,d0
 move.b d1,(a0)+
 cmp.b #0, d1
 jne .L6
 *  to
 move.l d0,a1
 .L6:
 move.b (a1)+,d1
 move.b d1,(a0)+
 cmp.b #0, d1
 jne .L6
 *
 */
static int
propagate_moves ()
{
  rtx_insn *insn, *next;
  rtx_insn * current_label = 0;
  int change_count = 0;
  std
  ::vector<rtx_insn *> reg_reg;

  for (insn = get_insns (); insn; insn = next)
    {
      next = NEXT_INSN (insn);

      if (DEBUG_INSN_P(insn))
	continue;

      if (LABEL_P(insn))
	{
	  if (LABEL_NUSES(insn) == 1)
	    {
	      current_label = insn;
	      reg_reg.clear ();
	    }
	  else
	    current_label = 0;
	}

      if (current_label == 0)
	continue;

      if (NONJUMP_INSN_P(insn))
	{
	  // check for set reg, reg
	  rtx set = single_set (insn);
	  if (set)
	    {
	      rtx src = SET_SRC(set);
	      rtx dst = SET_DEST(set);
	      if (REG_P(src) && REG_P(dst))
		reg_reg.push_back (insn);
	    }
	  continue;
	}

      if (JUMP_P(insn))
	{
	  if (JUMP_LABEL (insn) == current_label && reg_reg.size () > 1)
	    {
	      /* Search for reg/reg pairs. */
	      for (auto i = reg_reg.begin ();
		  i != reg_reg.end () && i + 1 != reg_reg.end ();)
		{
		  bool inc = true;
		  for (auto j = i + 1; j != reg_reg.end ();)
		    {
		      rtx seti = single_set (*i);
		      rtx srci = SET_SRC(seti);
		      rtx dsti = SET_DEST(seti);
		      rtx setj = single_set (*j);
		      rtx srcj = SET_SRC(setj);
		      rtx dstj = SET_DEST(setj);

		      if (rtx_equal_p (srci, dstj) && rtx_equal_p (srcj, dsti))
			{
			  /* Ensure correct usage. */
			  if (!reg_used_between_p (srci, current_label, *i)
			      && !reg_used_between_p (srci, *i, *j)
			      && !reg_used_between_p (srci, *j, insn)
			      && !reg_used_between_p (dsti, current_label, *i)
			      && !reg_used_between_p (dsti, *j, insn))
			    {
			      fprintf (stderr,
				       "condition met, moving regs %d, %d\n",
				       REGNO(srci), REGNO(dsti));

			      /* Move out of loop. */
			      remove_insn (*i);
			      if (PREV_INSN (current_label))
				add_insn_after (*i, PREV_INSN (current_label),
						0);
			      else
				add_insn_before (*i, current_label, 0);

			      remove_insn (*j);
			      add_insn_after (*j, insn, 0);

			      reg_reg.erase (j);
			      reg_reg.erase (i);
			      j = reg_reg.end ();
			      inc = false;

			      ++change_count;
			    }
			}
		      if (inc)
			++j;
		    }
		  if (inc)
		    ++i;
		}
	    }
	  current_label = 0;
	}
    }

  return change_count;
}

/**
 * Search for
 *
 *   mov x,reg
 *   mov reg,x
 *   cmp #0, reg
 *   jxx
 *
 * patterns.
 *
 * Use a simple state machine to find the patterns.
 */
static void
opt_strcpy ()
{
  rtx_insn *insn, *next;
  rtx_insn * x2reg = 0;
  rtx_insn * reg2x;
  unsigned int regno;

  for (insn = get_insns (); insn; insn = next)
    {
      rtx src;

      next = NEXT_INSN (insn);

      if (NONJUMP_INSN_P(insn))
	{
	  rtx set = single_set (insn);
	  if (!set)
	    continue;

	  if (x2reg && reg2x)
	    {
	      src = SET_SRC(set);
	      if (GET_CODE(src) == COMPARE)
		{
		  rtx dst = XEXP(src, 0);
		  src = XEXP(src, 1);

		  if (CONST_INT_P(src) && INTVAL(src) == 0
		      && find_reg_note (insn, REG_DEAD, dst))
		    {
		      /* now check via NOTICE_UPDATE_CC*/
		      NOTICE_UPDATE_CC (PATTERN (reg2x), reg2x);
		      if (cc_status.flags == 0
			  && rtx_equal_p (dst, cc_status.value2))
			{
			  int num_clobbers_to_add = 0;
			  int insn_code_number;

			  SET_SRC(single_set(reg2x)) = SET_SRC(
			      single_set (x2reg));
			  insn_code_number = recog (PATTERN (reg2x), reg2x,
						    &num_clobbers_to_add);

			  if (insn_code_number < 0)
			    {
			      /* restore register. */
			      SET_SRC(single_set(reg2x)) = SET_DEST(
				  single_set (x2reg));
			    }
			  else
			    {

			      rtx link;

			      fprintf (
				  stderr,
				  "condition met, removing compare and joining insns - omit reg %d\n",
				  REGNO(dst));

			      for (link = REG_NOTES(x2reg); link;
				  link = XEXP(link, 1))
				if (REG_NOTE_KIND (link) != REG_LABEL_OPERAND)
				  {
				    if (GET_CODE (link) == EXPR_LIST)
				      add_reg_note (
					  reg2x, REG_NOTE_KIND(link),
					  copy_insn_1 (XEXP(link, 0)));
				    else
				      add_shallow_copy_of_reg_note (reg2x,
								    link);
				  }

			      SET_INSN_DELETED(x2reg);
			      SET_INSN_DELETED(insn);

			      df_insn_rescan (reg2x);

			    }
			}
		    }
		  x2reg = 0;
		  continue;
		}
	      reg2x = 0;
	    }

	  /* check for reg2x first, maybe fallback to x2reg. */
	  if (x2reg && reg2x == 0)
	    {
	      if (REG_P(SET_SRC(set)) && REGNO(SET_SRC(set)) == regno)
		{
		  reg2x = insn;
		  continue;
		}
	      x2reg = 0;
	    }

	  /* check for a match for x2reg. */
	  if (x2reg == 0)
	    {
	      if (REG_P(SET_DEST(set)))
		{
		  x2reg = insn;
		  reg2x = 0;
		  regno = REGNO(SET_DEST(set));
		}
	    }
	}
    }
}

/* Main entry point to the pass.  */

static unsigned int
execute_bbb_optimizations (void)
{
  df_set_flags (DF_LR_RUN_DCE + DF_DEFER_INSN_RESCAN);
  df_note_add_problem ();
  df_analyze ();

//  dump_insns ("bbb 0");

  propagate_moves ();

  opt_strcpy ();

  dump_insns ("bbb 1");

  return 0;
}

namespace
  {

    const pass_data pass_data_bbb_optimizations =
      {
	RTL_PASS, /* type */
	"bbb", /* name */
	OPTGROUP_NONE, /* optinfo_flags */
	TV_NONE, /* tv_id */
	0, /* properties_required */
	0, /* properties_provided */
	0, /* properties_destroyed */
	0, /* todo_flags_start */
	( TODO_df_finish | TODO_df_verify ), /* todo_flags_finish */
      };

    class pass_bbb_optimizations : public rtl_opt_pass
      {
      public:
	pass_bbb_optimizations (gcc::context *ctxt)
	: rtl_opt_pass (pass_data_bbb_optimizations, ctxt)
	  {}

	/* opt_pass methods: */
	virtual bool gate (function *)
	  {
	    return true;
	  }

	virtual unsigned int execute (function *)
	  {
	    return execute_bbb_optimizations ();
	  }

      }; // class pass_bbb_optimizations

  } // anon namespace

rtl_opt_pass *
make_pass_bbb_optimizations (gcc::context *ctxt)
  {
    return new pass_bbb_optimizations (ctxt);
  }
