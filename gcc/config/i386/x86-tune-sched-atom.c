/* Scheduler hooks for IA-32 which implement atom+ specific logic.
   Copyright (C) 1988-2021 Free Software Foundation, Inc.

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

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "cfghooks.h"
#include "tm_p.h"
#include "insn-config.h"
#include "insn-attr.h"
#include "recog.h"
#include "target.h"
#include "rtl-iter.h"
#include "regset.h"
#include "sched-int.h"

/* Try to reorder ready list to take advantage of Atom pipelined IMUL
   execution. It is applied if
   (1) IMUL instruction is on the top of list;
   (2) There exists the only producer of independent IMUL instruction in
       ready list.
   Return index of IMUL producer if it was found and -1 otherwise.  */
static int
do_reorder_for_imul (rtx_insn **ready, int n_ready)
{
  rtx_insn *insn;
  rtx set, insn1, insn2;
  sd_iterator_def sd_it;
  dep_t dep;
  int index = -1;
  int i;

  if (!TARGET_CPU_P (BONNELL))
    return index;

  /* Check that IMUL instruction is on the top of ready list.  */
  insn = ready[n_ready - 1];
  set = single_set (insn);
  if (!set)
    return index;
  if (!(GET_CODE (SET_SRC (set)) == MULT
      && GET_MODE (SET_SRC (set)) == SImode))
    return index;

  /* Search for producer of independent IMUL instruction.  */
  for (i = n_ready - 2; i >= 0; i--)
    {
      insn = ready[i];
      if (!NONDEBUG_INSN_P (insn))
	continue;
      /* Skip IMUL instruction.  */
      insn2 = PATTERN (insn);
      if (GET_CODE (insn2) == PARALLEL)
	insn2 = XVECEXP (insn2, 0, 0);
      if (GET_CODE (insn2) == SET
	  && GET_CODE (SET_SRC (insn2)) == MULT
	  && GET_MODE (SET_SRC (insn2)) == SImode)
	continue;

      FOR_EACH_DEP (insn, SD_LIST_FORW, sd_it, dep)
	{
	  rtx con;
	  con = DEP_CON (dep);
	  if (!NONDEBUG_INSN_P (con))
	    continue;
	  insn1 = PATTERN (con);
	  if (GET_CODE (insn1) == PARALLEL)
	    insn1 = XVECEXP (insn1, 0, 0);

	  if (GET_CODE (insn1) == SET
	      && GET_CODE (SET_SRC (insn1)) == MULT
	      && GET_MODE (SET_SRC (insn1)) == SImode)
	    {
	      sd_iterator_def sd_it1;
	      dep_t dep1;
	      /* Check if there is no other dependee for IMUL.  */
	      index = i;
	      FOR_EACH_DEP (con, SD_LIST_BACK, sd_it1, dep1)
		{
		  rtx pro;
		  pro = DEP_PRO (dep1);
		  if (!NONDEBUG_INSN_P (pro))
		    continue;
		  if (pro != insn)
		    index = -1;
		}
	      if (index >= 0)
		break;
	    }
	}
      if (index >= 0)
	break;
    }
  return index;
}

/* Try to find the best candidate on the top of ready list if two insns
   have the same priority - candidate is best if its dependees were
   scheduled earlier. Applied for Silvermont only.
   Return true if top 2 insns must be interchanged.  */
static bool
swap_top_of_ready_list (rtx_insn **ready, int n_ready)
{
  rtx_insn *top = ready[n_ready - 1];
  rtx_insn *next = ready[n_ready - 2];
  rtx set;
  sd_iterator_def sd_it;
  dep_t dep;
  int clock1 = -1;
  int clock2 = -1;
  #define INSN_TICK(INSN) (HID (INSN)->tick)

  if (!TARGET_CPU_P (SILVERMONT) && !TARGET_CPU_P (INTEL))
    return false;

  if (!NONDEBUG_INSN_P (top))
    return false;
  if (!NONJUMP_INSN_P (top))
    return false;
  if (!NONDEBUG_INSN_P (next))
    return false;
  if (!NONJUMP_INSN_P (next))
    return false;
  set = single_set (top);
  if (!set)
    return false;
  set = single_set (next);
  if (!set)
    return false;

  if (INSN_PRIORITY_KNOWN (top) && INSN_PRIORITY_KNOWN (next))
    {
      if (INSN_PRIORITY (top) != INSN_PRIORITY (next))
	return false;
      /* Determine winner more precise.  */
      FOR_EACH_DEP (top, SD_LIST_RES_BACK, sd_it, dep)
	{
	  rtx pro;
	  pro = DEP_PRO (dep);
	  if (!NONDEBUG_INSN_P (pro))
	    continue;
	  if (INSN_TICK (pro) > clock1)
	    clock1 = INSN_TICK (pro);
	}
      FOR_EACH_DEP (next, SD_LIST_RES_BACK, sd_it, dep)
	{
	  rtx pro;
	  pro = DEP_PRO (dep);
	  if (!NONDEBUG_INSN_P (pro))
	    continue;
	  if (INSN_TICK (pro) > clock2)
	    clock2 = INSN_TICK (pro);
	}

      if (clock1 == clock2)
	{
	  /* Determine winner - load must win. */
	  enum attr_memory memory1, memory2;
	  memory1 = get_attr_memory (top);
	  memory2 = get_attr_memory (next);
	  if (memory2 == MEMORY_LOAD && memory1 != MEMORY_LOAD)
	    return true;
	}
	return (bool) (clock2 < clock1);
    }
  return false;
  #undef INSN_TICK
}

/* Perform possible reodering of ready list for Atom/Silvermont only.
   Return issue rate.  */
int
ix86_atom_sched_reorder (FILE *dump, int sched_verbose, rtx_insn **ready,
		         int *pn_ready, int clock_var)
{
  int issue_rate = -1;
  int n_ready = *pn_ready;
  int i;
  rtx_insn *insn;
  int index = -1;

  /* Set up issue rate.  */
  issue_rate = ix86_issue_rate ();

  /* Do reodering for BONNELL/SILVERMONT only.  */
  if (!TARGET_CPU_P (BONNELL) && !TARGET_CPU_P (SILVERMONT)
      && !TARGET_CPU_P (INTEL))
    return issue_rate;

  /* Nothing to do if ready list contains only 1 instruction.  */
  if (n_ready <= 1)
    return issue_rate;

  /* Do reodering for post-reload scheduler only.  */
  if (!reload_completed)
    return issue_rate;

  if ((index = do_reorder_for_imul (ready, n_ready)) >= 0)
    {
      if (sched_verbose > 1)
	fprintf (dump, ";;\tatom sched_reorder: put %d insn on top\n",
		 INSN_UID (ready[index]));

      /* Put IMUL producer (ready[index]) at the top of ready list.  */
      insn = ready[index];
      for (i = index; i < n_ready - 1; i++)
	ready[i] = ready[i + 1];
      ready[n_ready - 1] = insn;
      return issue_rate;
    }

  /* Skip selective scheduling since HID is not populated in it.  */
  if (clock_var != 0
      && !sel_sched_p ()
      && swap_top_of_ready_list (ready, n_ready))
    {
      if (sched_verbose > 1)
	fprintf (dump, ";;\tslm sched_reorder: swap %d and %d insns\n",
		 INSN_UID (ready[n_ready - 1]), INSN_UID (ready[n_ready - 2]));
      /* Swap 2 top elements of ready list.  */
      insn = ready[n_ready - 1];
      ready[n_ready - 1] = ready[n_ready - 2];
      ready[n_ready - 2] = insn;
    }
  return issue_rate;
}
