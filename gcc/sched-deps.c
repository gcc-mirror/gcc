/* Instruction scheduling pass.  This file computes dependencies between
   instructions.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com) Enhanced by,
   and currently maintained by, Jim Wilson (wilson@cygnus.com)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "toplev.h"
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "regs.h"
#include "function.h"
#include "flags.h"
#include "insn-config.h"
#include "insn-attr.h"
#include "except.h"
#include "toplev.h"
#include "recog.h"
#include "sched-int.h"
#include "params.h"
#include "cselib.h"
#include "df.h"


static regset reg_pending_sets;
static regset reg_pending_clobbers;
static regset reg_pending_uses;

/* The following enumeration values tell us what dependencies we
   should use to implement the barrier.  We use true-dependencies for
   TRUE_BARRIER and anti-dependencies for MOVE_BARRIER.  */
enum reg_pending_barrier_mode
{
  NOT_A_BARRIER = 0,
  MOVE_BARRIER,
  TRUE_BARRIER
};

static enum reg_pending_barrier_mode reg_pending_barrier;

/* To speed up the test for duplicate dependency links we keep a
   record of dependencies created by add_dependence when the average
   number of instructions in a basic block is very large.

   Studies have shown that there is typically around 5 instructions between
   branches for typical C code.  So we can make a guess that the average
   basic block is approximately 5 instructions long; we will choose 100X
   the average size as a very large basic block.

   Each insn has associated bitmaps for its dependencies.  Each bitmap
   has enough entries to represent a dependency on any other insn in
   the insn chain.  All bitmap for true dependencies cache is
   allocated then the rest two ones are also allocated.  */
static bitmap_head *true_dependency_cache;
static bitmap_head *anti_dependency_cache;
static bitmap_head *output_dependency_cache;
static int cache_size;

/* To speed up checking consistency of formed forward insn
   dependencies we use the following cache.  Another possible solution
   could be switching off checking duplication of insns in forward
   dependencies.  */
#ifdef ENABLE_CHECKING
static bitmap_head *forward_dependency_cache;
#endif

static int deps_may_trap_p (rtx);
static void add_dependence_list (rtx, rtx, int, enum reg_note);
static void add_dependence_list_and_free (rtx, rtx *, int, enum reg_note);
static void delete_all_dependences (rtx);
static void fixup_sched_groups (rtx);

static void flush_pending_lists (struct deps *, rtx, int, int);
static void sched_analyze_1 (struct deps *, rtx, rtx);
static void sched_analyze_2 (struct deps *, rtx, rtx);
static void sched_analyze_insn (struct deps *, rtx, rtx, rtx);

static rtx sched_get_condition (rtx);
static int conditions_mutex_p (rtx, rtx);

/* Return nonzero if a load of the memory reference MEM can cause a trap.  */

static int
deps_may_trap_p (rtx mem)
{
  rtx addr = XEXP (mem, 0);

  if (REG_P (addr) && REGNO (addr) >= FIRST_PSEUDO_REGISTER)
    {
      rtx t = get_reg_known_value (REGNO (addr));
      if (t)
	addr = t;
    }
  return rtx_addr_can_trap_p (addr);
}

/* Return the INSN_LIST containing INSN in LIST, or NULL
   if LIST does not contain INSN.  */

rtx
find_insn_list (rtx insn, rtx list)
{
  while (list)
    {
      if (XEXP (list, 0) == insn)
	return list;
      list = XEXP (list, 1);
    }
  return 0;
}

/* Find the condition under which INSN is executed.  */

static rtx
sched_get_condition (rtx insn)
{
  rtx pat = PATTERN (insn);
  rtx src;

  if (pat == 0)
    return 0;

  if (GET_CODE (pat) == COND_EXEC)
    return COND_EXEC_TEST (pat);

  if (!any_condjump_p (insn) || !onlyjump_p (insn))
    return 0;

  src = SET_SRC (pc_set (insn));

  if (XEXP (src, 2) == pc_rtx)
    return XEXP (src, 0);
  else if (XEXP (src, 1) == pc_rtx)
    {
      rtx cond = XEXP (src, 0);
      enum rtx_code revcode = reversed_comparison_code (cond, insn);

      if (revcode == UNKNOWN)
	return 0;
      return gen_rtx_fmt_ee (revcode, GET_MODE (cond), XEXP (cond, 0),
			     XEXP (cond, 1));
    }

  return 0;
}


/* Return nonzero if conditions COND1 and COND2 can never be both true.  */

static int
conditions_mutex_p (rtx cond1, rtx cond2)
{
  if (COMPARISON_P (cond1)
      && COMPARISON_P (cond2)
      && GET_CODE (cond1) == reversed_comparison_code (cond2, NULL)
      && XEXP (cond1, 0) == XEXP (cond2, 0)
      && XEXP (cond1, 1) == XEXP (cond2, 1))
    return 1;
  return 0;
}

/* Return true if insn1 and insn2 can never depend on one another because
   the conditions under which they are executed are mutually exclusive.  */
bool
sched_insns_conditions_mutex_p (rtx insn1, rtx insn2)
{
  rtx cond1, cond2;

  /* flow.c doesn't handle conditional lifetimes entirely correctly;
     calls mess up the conditional lifetimes.  */
  if (!CALL_P (insn1) && !CALL_P (insn2))
    {
      cond1 = sched_get_condition (insn1);
      cond2 = sched_get_condition (insn2);
      if (cond1 && cond2
	  && conditions_mutex_p (cond1, cond2)
	  /* Make sure first instruction doesn't affect condition of second
	     instruction if switched.  */
	  && !modified_in_p (cond1, insn2)
	  /* Make sure second instruction doesn't affect condition of first
	     instruction if switched.  */
	  && !modified_in_p (cond2, insn1))
	return true;
    }
  return false;
}

/* Add ELEM wrapped in an INSN_LIST with reg note kind DEP_TYPE to the
   LOG_LINKS of INSN, if not already there.  DEP_TYPE indicates the
   type of dependence that this link represents.  The function returns
   nonzero if a new entry has been added to insn's LOG_LINK.  */

int
add_dependence (rtx insn, rtx elem, enum reg_note dep_type)
{
  rtx link;
  int present_p;

  /* Don't depend an insn on itself.  */
  if (insn == elem)
    return 0;

  /* We can get a dependency on deleted insns due to optimizations in
     the register allocation and reloading or due to splitting.  Any
     such dependency is useless and can be ignored.  */
  if (NOTE_P (elem))
    return 0;

  present_p = 1;
#ifdef INSN_SCHEDULING
  /* ??? No good way to tell from here whether we're doing interblock
     scheduling.  Possibly add another callback.  */
#if 0
  /* (This code is guarded by INSN_SCHEDULING, otherwise INSN_BB is undefined.)
     No need for interblock dependences with calls, since
     calls are not moved between blocks.   Note: the edge where
     elem is a CALL is still required.  */
  if (CALL_P (insn)
      && (INSN_BB (elem) != INSN_BB (insn)))
    return 0;
#endif

  /* If we already have a dependency for ELEM, then we do not need to
     do anything.  Avoiding the list walk below can cut compile times
     dramatically for some code.  */
  if (true_dependency_cache != NULL)
    {
      enum reg_note present_dep_type = 0;

      gcc_assert (anti_dependency_cache);
      gcc_assert (output_dependency_cache);
      if (bitmap_bit_p (&true_dependency_cache[INSN_LUID (insn)],
			INSN_LUID (elem)))
	/* Do nothing (present_set_type is already 0).  */
	;
      else if (bitmap_bit_p (&anti_dependency_cache[INSN_LUID (insn)],
			 INSN_LUID (elem)))
	present_dep_type = REG_DEP_ANTI;
      else if (bitmap_bit_p (&output_dependency_cache[INSN_LUID (insn)],
			 INSN_LUID (elem)))
	present_dep_type = REG_DEP_OUTPUT;
      else
	present_p = 0;
      if (present_p && (int) dep_type >= (int) present_dep_type)
	return 0;
    }
#endif

  /* Check that we don't already have this dependence.  */
  if (present_p)
    for (link = LOG_LINKS (insn); link; link = XEXP (link, 1))
      if (XEXP (link, 0) == elem)
	{
#ifdef INSN_SCHEDULING
	  /* Clear corresponding cache entry because type of the link
             may be changed.  */
	  if (true_dependency_cache != NULL)
	    {
	      enum reg_note kind = REG_NOTE_KIND (link);
	      switch (kind)
		{
		case REG_DEP_ANTI:
		  bitmap_clear_bit (&anti_dependency_cache[INSN_LUID (insn)],
				    INSN_LUID (elem));
		  break;
		case REG_DEP_OUTPUT:
		  gcc_assert (output_dependency_cache);
		  bitmap_clear_bit (&output_dependency_cache[INSN_LUID (insn)],
				    INSN_LUID (elem));
		  break;
		default:
		  gcc_unreachable ();
		}
	    }
#endif

	  /* If this is a more restrictive type of dependence than the existing
	     one, then change the existing dependence to this type.  */
	  if ((int) dep_type < (int) REG_NOTE_KIND (link))
	    PUT_REG_NOTE_KIND (link, dep_type);

#ifdef INSN_SCHEDULING
	  /* If we are adding a dependency to INSN's LOG_LINKs, then
	     note that in the bitmap caches of dependency information.  */
	  if (true_dependency_cache != NULL)
	    {
	      if ((int) REG_NOTE_KIND (link) == 0)
		bitmap_set_bit (&true_dependency_cache[INSN_LUID (insn)],
				INSN_LUID (elem));
	      else if (REG_NOTE_KIND (link) == REG_DEP_ANTI)
		bitmap_set_bit (&anti_dependency_cache[INSN_LUID (insn)],
				INSN_LUID (elem));
	      else if (REG_NOTE_KIND (link) == REG_DEP_OUTPUT)
		bitmap_set_bit (&output_dependency_cache[INSN_LUID (insn)],
				INSN_LUID (elem));
	    }
#endif
	  return 0;
	}
  /* Might want to check one level of transitivity to save conses.  */

  link = alloc_INSN_LIST (elem, LOG_LINKS (insn));
  LOG_LINKS (insn) = link;

  /* Insn dependency, not data dependency.  */
  PUT_REG_NOTE_KIND (link, dep_type);

#ifdef INSN_SCHEDULING
  /* If we are adding a dependency to INSN's LOG_LINKs, then note that
     in the bitmap caches of dependency information.  */
  if (true_dependency_cache != NULL)
    {
      if ((int) dep_type == 0)
	bitmap_set_bit (&true_dependency_cache[INSN_LUID (insn)], INSN_LUID (elem));
      else if (dep_type == REG_DEP_ANTI)
	bitmap_set_bit (&anti_dependency_cache[INSN_LUID (insn)], INSN_LUID (elem));
      else if (dep_type == REG_DEP_OUTPUT)
	bitmap_set_bit (&output_dependency_cache[INSN_LUID (insn)], INSN_LUID (elem));
    }
#endif
  return 1;
}

/* A convenience wrapper to operate on an entire list.  */

static void
add_dependence_list (rtx insn, rtx list, int uncond, enum reg_note dep_type)
{
  for (; list; list = XEXP (list, 1))
    {
      if (uncond || ! sched_insns_conditions_mutex_p (insn, XEXP (list, 0)))
	add_dependence (insn, XEXP (list, 0), dep_type);
    }
}

/* Similar, but free *LISTP at the same time.  */

static void
add_dependence_list_and_free (rtx insn, rtx *listp, int uncond,
			      enum reg_note dep_type)
{
  rtx list, next;
  for (list = *listp, *listp = NULL; list ; list = next)
    {
      next = XEXP (list, 1);
      if (uncond || ! sched_insns_conditions_mutex_p (insn, XEXP (list, 0)))
	add_dependence (insn, XEXP (list, 0), dep_type);
      free_INSN_LIST_node (list);
    }
}

/* Clear all dependencies for an insn.  */

static void
delete_all_dependences (rtx insn)
{
  /* Clear caches, if they exist, as well as free the dependence.  */

#ifdef INSN_SCHEDULING
  if (true_dependency_cache != NULL)
    {
      bitmap_clear (&true_dependency_cache[INSN_LUID (insn)]);
      bitmap_clear (&anti_dependency_cache[INSN_LUID (insn)]);
      bitmap_clear (&output_dependency_cache[INSN_LUID (insn)]);
    }
#endif

  free_INSN_LIST_list (&LOG_LINKS (insn));
}

/* All insns in a scheduling group except the first should only have
   dependencies on the previous insn in the group.  So we find the
   first instruction in the scheduling group by walking the dependence
   chains backwards. Then we add the dependencies for the group to
   the previous nonnote insn.  */

static void
fixup_sched_groups (rtx insn)
{
  rtx link, prev_nonnote;

  for (link = LOG_LINKS (insn); link ; link = XEXP (link, 1))
    {
      rtx i = insn;
      do
	{
	  i = prev_nonnote_insn (i);

	  if (XEXP (link, 0) == i)
	    goto next_link;
	} while (SCHED_GROUP_P (i));
      if (! sched_insns_conditions_mutex_p (i, XEXP (link, 0)))
	add_dependence (i, XEXP (link, 0), REG_NOTE_KIND (link));
    next_link:;
    }

  delete_all_dependences (insn);

  prev_nonnote = prev_nonnote_insn (insn);
  if (BLOCK_FOR_INSN (insn) == BLOCK_FOR_INSN (prev_nonnote)
      && ! sched_insns_conditions_mutex_p (insn, prev_nonnote))
    add_dependence (insn, prev_nonnote, REG_DEP_ANTI);
}

/* Process an insn's memory dependencies.  There are four kinds of
   dependencies:

   (0) read dependence: read follows read
   (1) true dependence: read follows write
   (2) anti dependence: write follows read
   (3) output dependence: write follows write

   We are careful to build only dependencies which actually exist, and
   use transitivity to avoid building too many links.  */

/* Add an INSN and MEM reference pair to a pending INSN_LIST and MEM_LIST.
   The MEM is a memory reference contained within INSN, which we are saving
   so that we can do memory aliasing on it.  */

static void
add_insn_mem_dependence (struct deps *deps, rtx *insn_list, rtx *mem_list,
			 rtx insn, rtx mem)
{
  rtx link;

  link = alloc_INSN_LIST (insn, *insn_list);
  *insn_list = link;

  if (current_sched_info->use_cselib)
    {
      mem = shallow_copy_rtx (mem);
      XEXP (mem, 0) = cselib_subst_to_values (XEXP (mem, 0));
    }
  link = alloc_EXPR_LIST (VOIDmode, canon_rtx (mem), *mem_list);
  *mem_list = link;

  deps->pending_lists_length++;
}

/* Make a dependency between every memory reference on the pending lists
   and INSN, thus flushing the pending lists.  FOR_READ is true if emitting
   dependencies for a read operation, similarly with FOR_WRITE.  */

static void
flush_pending_lists (struct deps *deps, rtx insn, int for_read,
		     int for_write)
{
  if (for_write)
    {
      add_dependence_list_and_free (insn, &deps->pending_read_insns, 0,
				    REG_DEP_ANTI);
      free_EXPR_LIST_list (&deps->pending_read_mems);
    }

  add_dependence_list_and_free (insn, &deps->pending_write_insns, 0,
				for_read ? REG_DEP_ANTI : REG_DEP_OUTPUT);
  free_EXPR_LIST_list (&deps->pending_write_mems);
  deps->pending_lists_length = 0;

  add_dependence_list_and_free (insn, &deps->last_pending_memory_flush, 1,
				for_read ? REG_DEP_ANTI : REG_DEP_OUTPUT);
  deps->last_pending_memory_flush = alloc_INSN_LIST (insn, NULL_RTX);
  deps->pending_flush_length = 1;
}

/* Analyze a single SET, CLOBBER, PRE_DEC, POST_DEC, PRE_INC or POST_INC
   rtx, X, creating all dependencies generated by the write to the
   destination of X, and reads of everything mentioned.  */

static void
sched_analyze_1 (struct deps *deps, rtx x, rtx insn)
{
  int regno;
  rtx dest = XEXP (x, 0);
  enum rtx_code code = GET_CODE (x);

  if (dest == 0)
    return;

  if (GET_CODE (dest) == PARALLEL)
    {
      int i;

      for (i = XVECLEN (dest, 0) - 1; i >= 0; i--)
	if (XEXP (XVECEXP (dest, 0, i), 0) != 0)
	  sched_analyze_1 (deps,
			   gen_rtx_CLOBBER (VOIDmode,
					    XEXP (XVECEXP (dest, 0, i), 0)),
			   insn);

      if (GET_CODE (x) == SET)
	sched_analyze_2 (deps, SET_SRC (x), insn);
      return;
    }

  while (GET_CODE (dest) == STRICT_LOW_PART || GET_CODE (dest) == SUBREG
	 || GET_CODE (dest) == ZERO_EXTRACT)
    {
      if (GET_CODE (dest) == STRICT_LOW_PART
	 || GET_CODE (dest) == ZERO_EXTRACT
	 || read_modify_subreg_p (dest))
        {
	  /* These both read and modify the result.  We must handle
             them as writes to get proper dependencies for following
             instructions.  We must handle them as reads to get proper
             dependencies from this to previous instructions.
             Thus we need to call sched_analyze_2.  */

	  sched_analyze_2 (deps, XEXP (dest, 0), insn);
	}
      if (GET_CODE (dest) == ZERO_EXTRACT)
	{
	  /* The second and third arguments are values read by this insn.  */
	  sched_analyze_2 (deps, XEXP (dest, 1), insn);
	  sched_analyze_2 (deps, XEXP (dest, 2), insn);
	}
      dest = XEXP (dest, 0);
    }

  if (REG_P (dest))
    {
      regno = REGNO (dest);

#ifdef STACK_REGS
      /* Treat all writes to a stack register as modifying the TOS.  */
      if (regno >= FIRST_STACK_REG && regno <= LAST_STACK_REG)
	{
	  SET_REGNO_REG_SET (reg_pending_uses, FIRST_STACK_REG);
	  regno = FIRST_STACK_REG;
	}
#endif

      /* A hard reg in a wide mode may really be multiple registers.
         If so, mark all of them just like the first.  */
      if (regno < FIRST_PSEUDO_REGISTER)
	{
	  int i = hard_regno_nregs[regno][GET_MODE (dest)];
	  if (code == SET)
	    {
	      while (--i >= 0)
		SET_REGNO_REG_SET (reg_pending_sets, regno + i);
	    }
	  else
	    {
	      while (--i >= 0)
		SET_REGNO_REG_SET (reg_pending_clobbers, regno + i);
	    }
	}
      /* ??? Reload sometimes emits USEs and CLOBBERs of pseudos that
	 it does not reload.  Ignore these as they have served their
	 purpose already.  */
      else if (regno >= deps->max_reg)
	{
	  gcc_assert (GET_CODE (PATTERN (insn)) == USE
		      || GET_CODE (PATTERN (insn)) == CLOBBER);
	}
      else
	{
	  if (code == SET)
	    SET_REGNO_REG_SET (reg_pending_sets, regno);
	  else
	    SET_REGNO_REG_SET (reg_pending_clobbers, regno);

	  /* Pseudos that are REG_EQUIV to something may be replaced
	     by that during reloading.  We need only add dependencies for
	     the address in the REG_EQUIV note.  */
	  if (!reload_completed && get_reg_known_equiv_p (regno))
	    {
	      rtx t = get_reg_known_value (regno);
	      if (MEM_P (t))
	        sched_analyze_2 (deps, XEXP (t, 0), insn);
	    }

	  /* Don't let it cross a call after scheduling if it doesn't
	     already cross one.  */
	  if (REG_N_CALLS_CROSSED (regno) == 0)
	    add_dependence_list (insn, deps->last_function_call, 1,
				 REG_DEP_ANTI);
	}
    }
  else if (MEM_P (dest))
    {
      /* Writing memory.  */
      rtx t = dest;

      if (current_sched_info->use_cselib)
	{
	  t = shallow_copy_rtx (dest);
	  cselib_lookup (XEXP (t, 0), Pmode, 1);
	  XEXP (t, 0) = cselib_subst_to_values (XEXP (t, 0));
	}
      t = canon_rtx (t);

      if (deps->pending_lists_length > MAX_PENDING_LIST_LENGTH)
	{
	  /* Flush all pending reads and writes to prevent the pending lists
	     from getting any larger.  Insn scheduling runs too slowly when
	     these lists get long.  When compiling GCC with itself,
	     this flush occurs 8 times for sparc, and 10 times for m88k using
	     the default value of 32.  */
	  flush_pending_lists (deps, insn, false, true);
	}
      else
	{
	  rtx pending, pending_mem;

	  pending = deps->pending_read_insns;
	  pending_mem = deps->pending_read_mems;
	  while (pending)
	    {
	      if (anti_dependence (XEXP (pending_mem, 0), t)
		  && ! sched_insns_conditions_mutex_p (insn, XEXP (pending, 0)))
		add_dependence (insn, XEXP (pending, 0), REG_DEP_ANTI);

	      pending = XEXP (pending, 1);
	      pending_mem = XEXP (pending_mem, 1);
	    }

	  pending = deps->pending_write_insns;
	  pending_mem = deps->pending_write_mems;
	  while (pending)
	    {
	      if (output_dependence (XEXP (pending_mem, 0), t)
		  && ! sched_insns_conditions_mutex_p (insn, XEXP (pending, 0)))
		add_dependence (insn, XEXP (pending, 0), REG_DEP_OUTPUT);

	      pending = XEXP (pending, 1);
	      pending_mem = XEXP (pending_mem, 1);
	    }

	  add_dependence_list (insn, deps->last_pending_memory_flush, 1,
			       REG_DEP_ANTI);

	  add_insn_mem_dependence (deps, &deps->pending_write_insns,
				   &deps->pending_write_mems, insn, dest);
	}
      sched_analyze_2 (deps, XEXP (dest, 0), insn);
    }

  /* Analyze reads.  */
  if (GET_CODE (x) == SET)
    sched_analyze_2 (deps, SET_SRC (x), insn);
}

/* Analyze the uses of memory and registers in rtx X in INSN.  */

static void
sched_analyze_2 (struct deps *deps, rtx x, rtx insn)
{
  int i;
  int j;
  enum rtx_code code;
  const char *fmt;

  if (x == 0)
    return;

  code = GET_CODE (x);

  switch (code)
    {
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_VECTOR:
    case SYMBOL_REF:
    case CONST:
    case LABEL_REF:
      /* Ignore constants.  Note that we must handle CONST_DOUBLE here
         because it may have a cc0_rtx in its CONST_DOUBLE_CHAIN field, but
         this does not mean that this insn is using cc0.  */
      return;

#ifdef HAVE_cc0
    case CC0:
      /* User of CC0 depends on immediately preceding insn.  */
      SCHED_GROUP_P (insn) = 1;
       /* Don't move CC0 setter to another block (it can set up the
        same flag for previous CC0 users which is safe).  */
      CANT_MOVE (prev_nonnote_insn (insn)) = 1;
      return;
#endif

    case REG:
      {
	int regno = REGNO (x);

#ifdef STACK_REGS
      /* Treat all reads of a stack register as modifying the TOS.  */
      if (regno >= FIRST_STACK_REG && regno <= LAST_STACK_REG)
	{
	  SET_REGNO_REG_SET (reg_pending_sets, FIRST_STACK_REG);
	  regno = FIRST_STACK_REG;
	}
#endif

	if (regno < FIRST_PSEUDO_REGISTER)
	  {
	    int i = hard_regno_nregs[regno][GET_MODE (x)];
	    while (--i >= 0)
	      SET_REGNO_REG_SET (reg_pending_uses, regno + i);
	  }
	/* ??? Reload sometimes emits USEs and CLOBBERs of pseudos that
	   it does not reload.  Ignore these as they have served their
	   purpose already.  */
	else if (regno >= deps->max_reg)
	  {
	    gcc_assert (GET_CODE (PATTERN (insn)) == USE
			|| GET_CODE (PATTERN (insn)) == CLOBBER);
	  }
	else
	  {
	    SET_REGNO_REG_SET (reg_pending_uses, regno);

	    /* Pseudos that are REG_EQUIV to something may be replaced
	       by that during reloading.  We need only add dependencies for
	       the address in the REG_EQUIV note.  */
	    if (!reload_completed && get_reg_known_equiv_p (regno))
	      {
		rtx t = get_reg_known_value (regno);
		if (MEM_P (t))
		  sched_analyze_2 (deps, XEXP (t, 0), insn);
	      }

	    /* If the register does not already cross any calls, then add this
	       insn to the sched_before_next_call list so that it will still
	       not cross calls after scheduling.  */
	    if (REG_N_CALLS_CROSSED (regno) == 0)
	      deps->sched_before_next_call
		= alloc_INSN_LIST (insn, deps->sched_before_next_call);
	  }
	return;
      }

    case MEM:
      {
	/* Reading memory.  */
	rtx u;
	rtx pending, pending_mem;
	rtx t = x;

	if (current_sched_info->use_cselib)
	  {
	    t = shallow_copy_rtx (t);
	    cselib_lookup (XEXP (t, 0), Pmode, 1);
	    XEXP (t, 0) = cselib_subst_to_values (XEXP (t, 0));
	  }
	t = canon_rtx (t);
	pending = deps->pending_read_insns;
	pending_mem = deps->pending_read_mems;
	while (pending)
	  {
	    if (read_dependence (XEXP (pending_mem, 0), t)
		&& ! sched_insns_conditions_mutex_p (insn, XEXP (pending, 0)))
	      add_dependence (insn, XEXP (pending, 0), REG_DEP_ANTI);

	    pending = XEXP (pending, 1);
	    pending_mem = XEXP (pending_mem, 1);
	  }

	pending = deps->pending_write_insns;
	pending_mem = deps->pending_write_mems;
	while (pending)
	  {
	    if (true_dependence (XEXP (pending_mem, 0), VOIDmode,
				 t, rtx_varies_p)
		&& ! sched_insns_conditions_mutex_p (insn, XEXP (pending, 0)))
	      add_dependence (insn, XEXP (pending, 0), REG_DEP_TRUE);

	    pending = XEXP (pending, 1);
	    pending_mem = XEXP (pending_mem, 1);
	  }

	for (u = deps->last_pending_memory_flush; u; u = XEXP (u, 1))
	  if (! JUMP_P (XEXP (u, 0)) || deps_may_trap_p (x))
	    add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);

	/* Always add these dependencies to pending_reads, since
	   this insn may be followed by a write.  */
	add_insn_mem_dependence (deps, &deps->pending_read_insns,
				 &deps->pending_read_mems, insn, x);

	/* Take advantage of tail recursion here.  */
	sched_analyze_2 (deps, XEXP (x, 0), insn);
	return;
      }

    /* Force pending stores to memory in case a trap handler needs them.  */
    case TRAP_IF:
      flush_pending_lists (deps, insn, true, false);
      break;

    case ASM_OPERANDS:
    case ASM_INPUT:
    case UNSPEC_VOLATILE:
      {
	/* Traditional and volatile asm instructions must be considered to use
	   and clobber all hard registers, all pseudo-registers and all of
	   memory.  So must TRAP_IF and UNSPEC_VOLATILE operations.

	   Consider for instance a volatile asm that changes the fpu rounding
	   mode.  An insn should not be moved across this even if it only uses
	   pseudo-regs because it might give an incorrectly rounded result.  */
	if (code != ASM_OPERANDS || MEM_VOLATILE_P (x))
	  reg_pending_barrier = TRUE_BARRIER;

	/* For all ASM_OPERANDS, we must traverse the vector of input operands.
	   We can not just fall through here since then we would be confused
	   by the ASM_INPUT rtx inside ASM_OPERANDS, which do not indicate
	   traditional asms unlike their normal usage.  */

	if (code == ASM_OPERANDS)
	  {
	    for (j = 0; j < ASM_OPERANDS_INPUT_LENGTH (x); j++)
	      sched_analyze_2 (deps, ASM_OPERANDS_INPUT (x, j), insn);
	    return;
	  }
	break;
      }

    case PRE_DEC:
    case POST_DEC:
    case PRE_INC:
    case POST_INC:
      /* These both read and modify the result.  We must handle them as writes
         to get proper dependencies for following instructions.  We must handle
         them as reads to get proper dependencies from this to previous
         instructions.  Thus we need to pass them to both sched_analyze_1
         and sched_analyze_2.  We must call sched_analyze_2 first in order
         to get the proper antecedent for the read.  */
      sched_analyze_2 (deps, XEXP (x, 0), insn);
      sched_analyze_1 (deps, x, insn);
      return;

    case POST_MODIFY:
    case PRE_MODIFY:
      /* op0 = op0 + op1 */
      sched_analyze_2 (deps, XEXP (x, 0), insn);
      sched_analyze_2 (deps, XEXP (x, 1), insn);
      sched_analyze_1 (deps, x, insn);
      return;

    default:
      break;
    }

  /* Other cases: walk the insn.  */
  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	sched_analyze_2 (deps, XEXP (x, i), insn);
      else if (fmt[i] == 'E')
	for (j = 0; j < XVECLEN (x, i); j++)
	  sched_analyze_2 (deps, XVECEXP (x, i, j), insn);
    }
}

/* Analyze an INSN with pattern X to find all dependencies.  */

static void
sched_analyze_insn (struct deps *deps, rtx x, rtx insn, rtx loop_notes)
{
  RTX_CODE code = GET_CODE (x);
  rtx link;
  unsigned i;
  reg_set_iterator rsi;

  if (code == COND_EXEC)
    {
      sched_analyze_2 (deps, COND_EXEC_TEST (x), insn);

      /* ??? Should be recording conditions so we reduce the number of
	 false dependencies.  */
      x = COND_EXEC_CODE (x);
      code = GET_CODE (x);
    }
  if (code == SET || code == CLOBBER)
    {
      sched_analyze_1 (deps, x, insn);

      /* Bare clobber insns are used for letting life analysis, reg-stack
	 and others know that a value is dead.  Depend on the last call
	 instruction so that reg-stack won't get confused.  */
      if (code == CLOBBER)
	add_dependence_list (insn, deps->last_function_call, 1, REG_DEP_OUTPUT);
    }
  else if (code == PARALLEL)
    {
      for (i = XVECLEN (x, 0); i--;)
	{
	  rtx sub = XVECEXP (x, 0, i);
	  code = GET_CODE (sub);

	  if (code == COND_EXEC)
	    {
	      sched_analyze_2 (deps, COND_EXEC_TEST (sub), insn);
	      sub = COND_EXEC_CODE (sub);
	      code = GET_CODE (sub);
	    }
	  if (code == SET || code == CLOBBER)
	    sched_analyze_1 (deps, sub, insn);
	  else
	    sched_analyze_2 (deps, sub, insn);
	}
    }
  else
    sched_analyze_2 (deps, x, insn);

  /* Mark registers CLOBBERED or used by called function.  */
  if (CALL_P (insn))
    {
      for (link = CALL_INSN_FUNCTION_USAGE (insn); link; link = XEXP (link, 1))
	{
	  if (GET_CODE (XEXP (link, 0)) == CLOBBER)
	    sched_analyze_1 (deps, XEXP (link, 0), insn);
	  else
	    sched_analyze_2 (deps, XEXP (link, 0), insn);
	}
      if (find_reg_note (insn, REG_SETJMP, NULL))
	reg_pending_barrier = MOVE_BARRIER;
    }

  if (JUMP_P (insn))
    {
      rtx next;
      next = next_nonnote_insn (insn);
      if (next && BARRIER_P (next))
	reg_pending_barrier = TRUE_BARRIER;
      else
	{
	  rtx pending, pending_mem;
	  regset_head tmp_uses, tmp_sets;
	  INIT_REG_SET (&tmp_uses);
	  INIT_REG_SET (&tmp_sets);

	  (*current_sched_info->compute_jump_reg_dependencies)
	    (insn, &deps->reg_conditional_sets, &tmp_uses, &tmp_sets);
	  /* Make latency of jump equal to 0 by using anti-dependence.  */
	  EXECUTE_IF_SET_IN_REG_SET (&tmp_uses, 0, i, rsi)
	    {
	      struct deps_reg *reg_last = &deps->reg_last[i];
	      add_dependence_list (insn, reg_last->sets, 0, REG_DEP_ANTI);
	      add_dependence_list (insn, reg_last->clobbers, 0, REG_DEP_ANTI);
	      reg_last->uses_length++;
	      reg_last->uses = alloc_INSN_LIST (insn, reg_last->uses);
	    }
	  IOR_REG_SET (reg_pending_sets, &tmp_sets);

	  CLEAR_REG_SET (&tmp_uses);
	  CLEAR_REG_SET (&tmp_sets);

	  /* All memory writes and volatile reads must happen before the
	     jump.  Non-volatile reads must happen before the jump iff
	     the result is needed by the above register used mask.  */

	  pending = deps->pending_write_insns;
	  pending_mem = deps->pending_write_mems;
	  while (pending)
	    {
	      if (! sched_insns_conditions_mutex_p (insn, XEXP (pending, 0)))
		add_dependence (insn, XEXP (pending, 0), REG_DEP_OUTPUT);
	      pending = XEXP (pending, 1);
	      pending_mem = XEXP (pending_mem, 1);
	    }

	  pending = deps->pending_read_insns;
	  pending_mem = deps->pending_read_mems;
	  while (pending)
	    {
	      if (MEM_VOLATILE_P (XEXP (pending_mem, 0))
		  && ! sched_insns_conditions_mutex_p (insn, XEXP (pending, 0)))
		add_dependence (insn, XEXP (pending, 0), REG_DEP_OUTPUT);
	      pending = XEXP (pending, 1);
	      pending_mem = XEXP (pending_mem, 1);
	    }

	  add_dependence_list (insn, deps->last_pending_memory_flush, 1,
			       REG_DEP_ANTI);
	}
    }

  /* If there is a {LOOP,EHREGION}_{BEG,END} note in the middle of a basic
     block, then we must be sure that no instructions are scheduled across it.
     Otherwise, the reg_n_refs info (which depends on loop_depth) would
     become incorrect.  */
  if (loop_notes)
    {
      rtx link;

      /* Update loop_notes with any notes from this insn.  */
      link = loop_notes;
      while (XEXP (link, 1))
	{
	  gcc_assert (INTVAL (XEXP (link, 0)) == NOTE_INSN_LOOP_BEG
		      || INTVAL (XEXP (link, 0)) == NOTE_INSN_LOOP_END);

	  reg_pending_barrier = MOVE_BARRIER;
	  link = XEXP (link, 1);
	}
      XEXP (link, 1) = REG_NOTES (insn);
      REG_NOTES (insn) = loop_notes;
    }

  /* If this instruction can throw an exception, then moving it changes
     where block boundaries fall.  This is mighty confusing elsewhere.
     Therefore, prevent such an instruction from being moved.  */
  if (can_throw_internal (insn))
    reg_pending_barrier = MOVE_BARRIER;

  /* Add dependencies if a scheduling barrier was found.  */
  if (reg_pending_barrier)
    {
      /* In the case of barrier the most added dependencies are not
         real, so we use anti-dependence here.  */
      if (sched_get_condition (insn))
	{
	  EXECUTE_IF_SET_IN_REG_SET (&deps->reg_last_in_use, 0, i, rsi)
	    {
	      struct deps_reg *reg_last = &deps->reg_last[i];
	      add_dependence_list (insn, reg_last->uses, 0, REG_DEP_ANTI);
	      add_dependence_list
		(insn, reg_last->sets, 0,
		 reg_pending_barrier == TRUE_BARRIER ? REG_DEP_TRUE : REG_DEP_ANTI);
	      add_dependence_list
		(insn, reg_last->clobbers, 0,
		 reg_pending_barrier == TRUE_BARRIER ? REG_DEP_TRUE : REG_DEP_ANTI);
	    }
	}
      else
	{
	  EXECUTE_IF_SET_IN_REG_SET (&deps->reg_last_in_use, 0, i, rsi)
	    {
	      struct deps_reg *reg_last = &deps->reg_last[i];
	      add_dependence_list_and_free (insn, &reg_last->uses, 0,
					    REG_DEP_ANTI);
	      add_dependence_list_and_free
		(insn, &reg_last->sets, 0,
		 reg_pending_barrier == TRUE_BARRIER ? REG_DEP_TRUE : REG_DEP_ANTI);
	      add_dependence_list_and_free
		(insn, &reg_last->clobbers, 0,
		 reg_pending_barrier == TRUE_BARRIER ? REG_DEP_TRUE : REG_DEP_ANTI);
	      reg_last->uses_length = 0;
	      reg_last->clobbers_length = 0;
	    }
	}

      for (i = 0; i < (unsigned)deps->max_reg; i++)
	{
	  struct deps_reg *reg_last = &deps->reg_last[i];
	  reg_last->sets = alloc_INSN_LIST (insn, reg_last->sets);
	  SET_REGNO_REG_SET (&deps->reg_last_in_use, i);
	}

      flush_pending_lists (deps, insn, true, true);
      CLEAR_REG_SET (&deps->reg_conditional_sets);
      reg_pending_barrier = NOT_A_BARRIER;
    }
  else
    {
      /* If the current insn is conditional, we can't free any
	 of the lists.  */
      if (sched_get_condition (insn))
	{
	  EXECUTE_IF_SET_IN_REG_SET (reg_pending_uses, 0, i, rsi)
	    {
	      struct deps_reg *reg_last = &deps->reg_last[i];
	      add_dependence_list (insn, reg_last->sets, 0, REG_DEP_TRUE);
	      add_dependence_list (insn, reg_last->clobbers, 0, REG_DEP_TRUE);
	      reg_last->uses = alloc_INSN_LIST (insn, reg_last->uses);
	      reg_last->uses_length++;
	    }
	  EXECUTE_IF_SET_IN_REG_SET (reg_pending_clobbers, 0, i, rsi)
	    {
	      struct deps_reg *reg_last = &deps->reg_last[i];
	      add_dependence_list (insn, reg_last->sets, 0, REG_DEP_OUTPUT);
	      add_dependence_list (insn, reg_last->uses, 0, REG_DEP_ANTI);
	      reg_last->clobbers = alloc_INSN_LIST (insn, reg_last->clobbers);
	      reg_last->clobbers_length++;
	    }
	  EXECUTE_IF_SET_IN_REG_SET (reg_pending_sets, 0, i, rsi)
	    {
	      struct deps_reg *reg_last = &deps->reg_last[i];
	      add_dependence_list (insn, reg_last->sets, 0, REG_DEP_OUTPUT);
	      add_dependence_list (insn, reg_last->clobbers, 0, REG_DEP_OUTPUT);
	      add_dependence_list (insn, reg_last->uses, 0, REG_DEP_ANTI);
	      reg_last->sets = alloc_INSN_LIST (insn, reg_last->sets);
	      SET_REGNO_REG_SET (&deps->reg_conditional_sets, i);
	    }
	}
      else
	{
	  EXECUTE_IF_SET_IN_REG_SET (reg_pending_uses, 0, i, rsi)
	    {
	      struct deps_reg *reg_last = &deps->reg_last[i];
	      add_dependence_list (insn, reg_last->sets, 0, REG_DEP_TRUE);
	      add_dependence_list (insn, reg_last->clobbers, 0, REG_DEP_TRUE);
	      reg_last->uses_length++;
	      reg_last->uses = alloc_INSN_LIST (insn, reg_last->uses);
	    }
	  EXECUTE_IF_SET_IN_REG_SET (reg_pending_clobbers, 0, i, rsi)
	    {
	      struct deps_reg *reg_last = &deps->reg_last[i];
	      if (reg_last->uses_length > MAX_PENDING_LIST_LENGTH
		  || reg_last->clobbers_length > MAX_PENDING_LIST_LENGTH)
		{
		  add_dependence_list_and_free (insn, &reg_last->sets, 0,
					        REG_DEP_OUTPUT);
		  add_dependence_list_and_free (insn, &reg_last->uses, 0,
						REG_DEP_ANTI);
		  add_dependence_list_and_free (insn, &reg_last->clobbers, 0,
						REG_DEP_OUTPUT);
		  reg_last->sets = alloc_INSN_LIST (insn, reg_last->sets);
		  reg_last->clobbers_length = 0;
		  reg_last->uses_length = 0;
		}
	      else
		{
		  add_dependence_list (insn, reg_last->sets, 0, REG_DEP_OUTPUT);
		  add_dependence_list (insn, reg_last->uses, 0, REG_DEP_ANTI);
		}
	      reg_last->clobbers_length++;
	      reg_last->clobbers = alloc_INSN_LIST (insn, reg_last->clobbers);
	    }
	  EXECUTE_IF_SET_IN_REG_SET (reg_pending_sets, 0, i, rsi)
	    {
	      struct deps_reg *reg_last = &deps->reg_last[i];
	      add_dependence_list_and_free (insn, &reg_last->sets, 0,
					    REG_DEP_OUTPUT);
	      add_dependence_list_and_free (insn, &reg_last->clobbers, 0,
					    REG_DEP_OUTPUT);
	      add_dependence_list_and_free (insn, &reg_last->uses, 0,
					    REG_DEP_ANTI);
	      reg_last->sets = alloc_INSN_LIST (insn, reg_last->sets);
	      reg_last->uses_length = 0;
	      reg_last->clobbers_length = 0;
	      CLEAR_REGNO_REG_SET (&deps->reg_conditional_sets, i);
	    }
	}

      IOR_REG_SET (&deps->reg_last_in_use, reg_pending_uses);
      IOR_REG_SET (&deps->reg_last_in_use, reg_pending_clobbers);
      IOR_REG_SET (&deps->reg_last_in_use, reg_pending_sets);
    }
  CLEAR_REG_SET (reg_pending_uses);
  CLEAR_REG_SET (reg_pending_clobbers);
  CLEAR_REG_SET (reg_pending_sets);

  /* If we are currently in a libcall scheduling group, then mark the
     current insn as being in a scheduling group and that it can not
     be moved into a different basic block.  */

  if (deps->libcall_block_tail_insn)
    {
      SCHED_GROUP_P (insn) = 1;
      CANT_MOVE (insn) = 1;
    }

  /* If a post-call group is still open, see if it should remain so.
     This insn must be a simple move of a hard reg to a pseudo or
     vice-versa.

     We must avoid moving these insns for correctness on
     SMALL_REGISTER_CLASS machines, and for special registers like
     PIC_OFFSET_TABLE_REGNUM.  For simplicity, extend this to all
     hard regs for all targets.  */

  if (deps->in_post_call_group_p)
    {
      rtx tmp, set = single_set (insn);
      int src_regno, dest_regno;

      if (set == NULL)
	goto end_call_group;

      tmp = SET_DEST (set);
      if (GET_CODE (tmp) == SUBREG)
	tmp = SUBREG_REG (tmp);
      if (REG_P (tmp))
	dest_regno = REGNO (tmp);
      else
	goto end_call_group;

      tmp = SET_SRC (set);
      if (GET_CODE (tmp) == SUBREG)
	tmp = SUBREG_REG (tmp);
      if ((GET_CODE (tmp) == PLUS
	   || GET_CODE (tmp) == MINUS)
	  && REG_P (XEXP (tmp, 0))
	  && REGNO (XEXP (tmp, 0)) == STACK_POINTER_REGNUM
	  && dest_regno == STACK_POINTER_REGNUM)
	src_regno = STACK_POINTER_REGNUM;
      else if (REG_P (tmp))
	src_regno = REGNO (tmp);
      else
	goto end_call_group;

      if (src_regno < FIRST_PSEUDO_REGISTER
	  || dest_regno < FIRST_PSEUDO_REGISTER)
	{
	  if (deps->in_post_call_group_p == post_call_initial)
	    deps->in_post_call_group_p = post_call;

	  SCHED_GROUP_P (insn) = 1;
	  CANT_MOVE (insn) = 1;
	}
      else
	{
	end_call_group:
	  deps->in_post_call_group_p = not_post_call;
	}
    }

  /* Fixup the dependencies in the sched group.  */
  if (SCHED_GROUP_P (insn))
    fixup_sched_groups (insn);
}

/* Analyze every insn between HEAD and TAIL inclusive, creating LOG_LINKS
   for every dependency.  */

void
sched_analyze (struct deps *deps, rtx head, rtx tail)
{
  rtx insn;
  rtx loop_notes = 0;

  if (current_sched_info->use_cselib)
    cselib_init (true);

  /* Before reload, if the previous block ended in a call, show that
     we are inside a post-call group, so as to keep the lifetimes of
     hard registers correct.  */
  if (! reload_completed && !LABEL_P (head))
    {
      insn = prev_nonnote_insn (head);
      if (insn && CALL_P (insn))
	deps->in_post_call_group_p = post_call_initial;
    }
  for (insn = head;; insn = NEXT_INSN (insn))
    {
      rtx link, end_seq, r0, set;

      if (NONJUMP_INSN_P (insn) || JUMP_P (insn))
	{
	  /* Clear out the stale LOG_LINKS from flow.  */
	  free_INSN_LIST_list (&LOG_LINKS (insn));

	  /* Make each JUMP_INSN a scheduling barrier for memory
             references.  */
	  if (JUMP_P (insn))
	    {
	      /* Keep the list a reasonable size.  */
	      if (deps->pending_flush_length++ > MAX_PENDING_LIST_LENGTH)
		flush_pending_lists (deps, insn, true, true);
	      else
		deps->last_pending_memory_flush
		  = alloc_INSN_LIST (insn, deps->last_pending_memory_flush);
	    }
	  sched_analyze_insn (deps, PATTERN (insn), insn, loop_notes);
	  loop_notes = 0;
	}
      else if (CALL_P (insn))
	{
	  int i;

	  CANT_MOVE (insn) = 1;

	  /* Clear out the stale LOG_LINKS from flow.  */
	  free_INSN_LIST_list (&LOG_LINKS (insn));

	  if (find_reg_note (insn, REG_SETJMP, NULL))
	    {
	      /* This is setjmp.  Assume that all registers, not just
		 hard registers, may be clobbered by this call.  */
	      reg_pending_barrier = MOVE_BARRIER;
	    }
	  else
	    {
	      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
		/* A call may read and modify global register variables.  */
		if (global_regs[i])
		  {
		    SET_REGNO_REG_SET (reg_pending_sets, i);
		    SET_REGNO_REG_SET (reg_pending_uses, i);
		  }
		/* Other call-clobbered hard regs may be clobbered.
		   Since we only have a choice between 'might be clobbered'
		   and 'definitely not clobbered', we must include all
		   partly call-clobbered registers here.  */
		else if (HARD_REGNO_CALL_PART_CLOBBERED (i, reg_raw_mode[i])
			 || TEST_HARD_REG_BIT (regs_invalidated_by_call, i))
		  SET_REGNO_REG_SET (reg_pending_clobbers, i);
		/* We don't know what set of fixed registers might be used
		   by the function, but it is certain that the stack pointer
		   is among them, but be conservative.  */
		else if (fixed_regs[i])
		  SET_REGNO_REG_SET (reg_pending_uses, i);
		/* The frame pointer is normally not used by the function
		   itself, but by the debugger.  */
		/* ??? MIPS o32 is an exception.  It uses the frame pointer
		   in the macro expansion of jal but does not represent this
		   fact in the call_insn rtl.  */
		else if (i == FRAME_POINTER_REGNUM
			 || (i == HARD_FRAME_POINTER_REGNUM
			     && (! reload_completed || frame_pointer_needed)))
		  SET_REGNO_REG_SET (reg_pending_uses, i);
	    }

	  /* For each insn which shouldn't cross a call, add a dependence
	     between that insn and this call insn.  */
	  add_dependence_list_and_free (insn, &deps->sched_before_next_call, 1,
					REG_DEP_ANTI);

	  sched_analyze_insn (deps, PATTERN (insn), insn, loop_notes);
	  loop_notes = 0;

	  /* In the absence of interprocedural alias analysis, we must flush
	     all pending reads and writes, and start new dependencies starting
	     from here.  But only flush writes for constant calls (which may
	     be passed a pointer to something we haven't written yet).  */
	  flush_pending_lists (deps, insn, true, !CONST_OR_PURE_CALL_P (insn));

	  /* Remember the last function call for limiting lifetimes.  */
	  free_INSN_LIST_list (&deps->last_function_call);
	  deps->last_function_call = alloc_INSN_LIST (insn, NULL_RTX);

	  /* Before reload, begin a post-call group, so as to keep the
	     lifetimes of hard registers correct.  */
	  if (! reload_completed)
	    deps->in_post_call_group_p = post_call;
	}

      /* EH_REGION insn notes can not appear until well after we complete
	 scheduling.  */
      if (NOTE_P (insn))
	gcc_assert (NOTE_LINE_NUMBER (insn) != NOTE_INSN_EH_REGION_BEG
		    && NOTE_LINE_NUMBER (insn) != NOTE_INSN_EH_REGION_END);

      /* See comments on reemit_notes as to why we do this.
	 ??? Actually, the reemit_notes just say what is done, not why.  */

      if (NOTE_P (insn)
	  && (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_BEG
	      || NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_END))
	{
	  loop_notes = alloc_EXPR_LIST (REG_SAVE_NOTE,
					GEN_INT (NOTE_LINE_NUMBER (insn)),
					loop_notes);
	  CONST_OR_PURE_CALL_P (loop_notes) = CONST_OR_PURE_CALL_P (insn);
	}

      if (current_sched_info->use_cselib)
	cselib_process_insn (insn);

      /* Now that we have completed handling INSN, check and see if it is
	 a CLOBBER beginning a libcall block.   If it is, record the
	 end of the libcall sequence.

	 We want to schedule libcall blocks as a unit before reload.  While
	 this restricts scheduling, it preserves the meaning of a libcall
	 block.

	 As a side effect, we may get better code due to decreased register
	 pressure as well as less chance of a foreign insn appearing in
	 a libcall block.  */
      if (!reload_completed
	  /* Note we may have nested libcall sequences.  We only care about
	     the outermost libcall sequence.  */
	  && deps->libcall_block_tail_insn == 0
	  /* The sequence must start with a clobber of a register.  */
	  && NONJUMP_INSN_P (insn)
	  && GET_CODE (PATTERN (insn)) == CLOBBER
          && (r0 = XEXP (PATTERN (insn), 0), REG_P (r0))
	  && REG_P (XEXP (PATTERN (insn), 0))
	  /* The CLOBBER must also have a REG_LIBCALL note attached.  */
	  && (link = find_reg_note (insn, REG_LIBCALL, NULL_RTX)) != 0
	  && (end_seq = XEXP (link, 0)) != 0
	  /* The insn referenced by the REG_LIBCALL note must be a
	     simple nop copy with the same destination as the register
	     mentioned in the clobber.  */
	  && (set = single_set (end_seq)) != 0
	  && SET_DEST (set) == r0 && SET_SRC (set) == r0
	  /* And finally the insn referenced by the REG_LIBCALL must
	     also contain a REG_EQUAL note and a REG_RETVAL note.  */
	  && find_reg_note (end_seq, REG_EQUAL, NULL_RTX) != 0
	  && find_reg_note (end_seq, REG_RETVAL, NULL_RTX) != 0)
	deps->libcall_block_tail_insn = XEXP (link, 0);

      /* If we have reached the end of a libcall block, then close the
	 block.  */
      if (deps->libcall_block_tail_insn == insn)
	deps->libcall_block_tail_insn = 0;

      if (insn == tail)
	{
	  if (current_sched_info->use_cselib)
	    cselib_finish ();
	  return;
	}
    }
  gcc_unreachable ();
}


/* The following function adds forward dependence (FROM, TO) with
   given DEP_TYPE.  The forward dependence should be not exist before.  */

void
add_forward_dependence (rtx from, rtx to, enum reg_note dep_type)
{
  rtx new_link;

#ifdef ENABLE_CHECKING
  /* If add_dependence is working properly there should never
     be notes, deleted insns or duplicates in the backward
     links.  Thus we need not check for them here.

     However, if we have enabled checking we might as well go
     ahead and verify that add_dependence worked properly.  */
  gcc_assert (!NOTE_P (from));
  gcc_assert (!INSN_DELETED_P (from));
  if (forward_dependency_cache)
    gcc_assert (!bitmap_bit_p (&forward_dependency_cache[INSN_LUID (from)],
			       INSN_LUID (to)));
  else
    gcc_assert (!find_insn_list (to, INSN_DEPEND (from)));

  /* ??? If bitmap_bit_p is a predicate, what is this supposed to do? */
  if (forward_dependency_cache != NULL)
    bitmap_bit_p (&forward_dependency_cache[INSN_LUID (from)],
		  INSN_LUID (to));
#endif

  new_link = alloc_INSN_LIST (to, INSN_DEPEND (from));

  PUT_REG_NOTE_KIND (new_link, dep_type);

  INSN_DEPEND (from) = new_link;
  INSN_DEP_COUNT (to) += 1;
}

/* Examine insns in the range [ HEAD, TAIL ] and Use the backward
   dependences from LOG_LINKS to build forward dependences in
   INSN_DEPEND.  */

void
compute_forward_dependences (rtx head, rtx tail)
{
  rtx insn, link;
  rtx next_tail;

  next_tail = NEXT_INSN (tail);
  for (insn = head; insn != next_tail; insn = NEXT_INSN (insn))
    {
      if (! INSN_P (insn))
	continue;

      for (link = LOG_LINKS (insn); link; link = XEXP (link, 1))
	add_forward_dependence (XEXP (link, 0), insn, REG_NOTE_KIND (link));
    }
}

/* Initialize variables for region data dependence analysis.
   n_bbs is the number of region blocks.  */

void
init_deps (struct deps *deps)
{
  int max_reg = (reload_completed ? FIRST_PSEUDO_REGISTER : max_reg_num ());

  deps->max_reg = max_reg;
  deps->reg_last = xcalloc (max_reg, sizeof (struct deps_reg));
  INIT_REG_SET (&deps->reg_last_in_use);
  INIT_REG_SET (&deps->reg_conditional_sets);

  deps->pending_read_insns = 0;
  deps->pending_read_mems = 0;
  deps->pending_write_insns = 0;
  deps->pending_write_mems = 0;
  deps->pending_lists_length = 0;
  deps->pending_flush_length = 0;
  deps->last_pending_memory_flush = 0;
  deps->last_function_call = 0;
  deps->sched_before_next_call = 0;
  deps->in_post_call_group_p = not_post_call;
  deps->libcall_block_tail_insn = 0;
}

/* Free insn lists found in DEPS.  */

void
free_deps (struct deps *deps)
{
  unsigned i;
  reg_set_iterator rsi;

  free_INSN_LIST_list (&deps->pending_read_insns);
  free_EXPR_LIST_list (&deps->pending_read_mems);
  free_INSN_LIST_list (&deps->pending_write_insns);
  free_EXPR_LIST_list (&deps->pending_write_mems);
  free_INSN_LIST_list (&deps->last_pending_memory_flush);

  /* Without the EXECUTE_IF_SET, this loop is executed max_reg * nr_regions
     times.  For a testcase with 42000 regs and 8000 small basic blocks,
     this loop accounted for nearly 60% (84 sec) of the total -O2 runtime.  */
  EXECUTE_IF_SET_IN_REG_SET (&deps->reg_last_in_use, 0, i, rsi)
    {
      struct deps_reg *reg_last = &deps->reg_last[i];
      if (reg_last->uses)
	free_INSN_LIST_list (&reg_last->uses);
      if (reg_last->sets)
	free_INSN_LIST_list (&reg_last->sets);
      if (reg_last->clobbers)
	free_INSN_LIST_list (&reg_last->clobbers);
    }
  CLEAR_REG_SET (&deps->reg_last_in_use);
  CLEAR_REG_SET (&deps->reg_conditional_sets);

  free (deps->reg_last);
}

/* If it is profitable to use them, initialize caches for tracking
   dependency information.  LUID is the number of insns to be scheduled,
   it is used in the estimate of profitability.  */

void
init_dependency_caches (int luid)
{
  /* ?!? We could save some memory by computing a per-region luid mapping
     which could reduce both the number of vectors in the cache and the size
     of each vector.  Instead we just avoid the cache entirely unless the
     average number of instructions in a basic block is very high.  See
     the comment before the declaration of true_dependency_cache for
     what we consider "very high".  */
  if (luid / n_basic_blocks > 100 * 5)
    {
      int i;
      true_dependency_cache = xmalloc (luid * sizeof (bitmap_head));
      anti_dependency_cache = xmalloc (luid * sizeof (bitmap_head));
      output_dependency_cache = xmalloc (luid * sizeof (bitmap_head));
#ifdef ENABLE_CHECKING
      forward_dependency_cache = xmalloc (luid * sizeof (bitmap_head));
#endif
      for (i = 0; i < luid; i++)
	{
	  bitmap_initialize (&true_dependency_cache[i], 0);
	  bitmap_initialize (&anti_dependency_cache[i], 0);
	  bitmap_initialize (&output_dependency_cache[i], 0);
#ifdef ENABLE_CHECKING
	  bitmap_initialize (&forward_dependency_cache[i], 0);
#endif
	}
      cache_size = luid;
    }
}

/* Free the caches allocated in init_dependency_caches.  */

void
free_dependency_caches (void)
{
  if (true_dependency_cache)
    {
      int i;

      for (i = 0; i < cache_size; i++)
	{
	  bitmap_clear (&true_dependency_cache[i]);
	  bitmap_clear (&anti_dependency_cache[i]);
	  bitmap_clear (&output_dependency_cache[i]);
#ifdef ENABLE_CHECKING
	  bitmap_clear (&forward_dependency_cache[i]);
#endif
	}
      free (true_dependency_cache);
      true_dependency_cache = NULL;
      free (anti_dependency_cache);
      anti_dependency_cache = NULL;
      free (output_dependency_cache);
      output_dependency_cache = NULL;
#ifdef ENABLE_CHECKING
      free (forward_dependency_cache);
      forward_dependency_cache = NULL;
#endif
    }
}

/* Initialize some global variables needed by the dependency analysis
   code.  */

void
init_deps_global (void)
{
  reg_pending_sets = ALLOC_REG_SET (&reg_obstack);
  reg_pending_clobbers = ALLOC_REG_SET (&reg_obstack);
  reg_pending_uses = ALLOC_REG_SET (&reg_obstack);
  reg_pending_barrier = NOT_A_BARRIER;
}

/* Free everything used by the dependency analysis code.  */

void
finish_deps_global (void)
{
  FREE_REG_SET (reg_pending_sets);
  FREE_REG_SET (reg_pending_clobbers);
  FREE_REG_SET (reg_pending_uses);
}
