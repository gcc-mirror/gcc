/* Instruction scheduling pass.  This file computes dependencies between
   instructions.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002 Free Software Foundation, Inc.
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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "toplev.h"
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "basic-block.h"
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

extern char *reg_known_equiv_p;
extern rtx *reg_known_value;

static regset_head reg_pending_sets_head;
static regset_head reg_pending_clobbers_head;
static regset_head reg_pending_uses_head;

static regset reg_pending_sets;
static regset reg_pending_clobbers;
static regset reg_pending_uses;
static bool reg_pending_barrier;

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
static sbitmap *true_dependency_cache;
static sbitmap *anti_dependency_cache;
static sbitmap *output_dependency_cache;

/* To speed up checking consistency of formed forward insn
   dependencies we use the following cache.  Another possible solution
   could be switching off checking duplication of insns in forward
   dependencies.  */
#ifdef ENABLE_CHECKING
static sbitmap *forward_dependency_cache;
#endif

static int deps_may_trap_p PARAMS ((rtx));
static void add_dependence_list PARAMS ((rtx, rtx, enum reg_note));
static void add_dependence_list_and_free PARAMS ((rtx, rtx *, enum reg_note));
static void remove_dependence PARAMS ((rtx, rtx));
static void set_sched_group_p PARAMS ((rtx));

static void flush_pending_lists PARAMS ((struct deps *, rtx, int, int));
static void sched_analyze_1 PARAMS ((struct deps *, rtx, rtx));
static void sched_analyze_2 PARAMS ((struct deps *, rtx, rtx));
static void sched_analyze_insn PARAMS ((struct deps *, rtx, rtx, rtx));
static rtx group_leader PARAMS ((rtx));

static rtx get_condition PARAMS ((rtx));
static int conditions_mutex_p PARAMS ((rtx, rtx));

/* Return nonzero if a load of the memory reference MEM can cause a trap.  */

static int
deps_may_trap_p (mem)
     rtx mem;
{
  rtx addr = XEXP (mem, 0);

  if (REG_P (addr)
      && REGNO (addr) >= FIRST_PSEUDO_REGISTER
      && reg_known_value[REGNO (addr)])
    addr = reg_known_value[REGNO (addr)];
  return rtx_addr_can_trap_p (addr);
}

/* Return the INSN_LIST containing INSN in LIST, or NULL
   if LIST does not contain INSN.  */

rtx
find_insn_list (insn, list)
     rtx insn;
     rtx list;
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
get_condition (insn)
     rtx insn;
{
  rtx pat = PATTERN (insn);
  rtx cond;

  if (pat == 0)
    return 0;
  if (GET_CODE (pat) == COND_EXEC)
    return COND_EXEC_TEST (pat);
  if (GET_CODE (insn) != JUMP_INSN)
    return 0;
  if (GET_CODE (pat) != SET || SET_SRC (pat) != pc_rtx)
    return 0;
  if (GET_CODE (SET_DEST (pat)) != IF_THEN_ELSE)
    return 0;
  pat = SET_DEST (pat);
  cond = XEXP (pat, 0);
  if (GET_CODE (XEXP (cond, 1)) == LABEL_REF
      && XEXP (cond, 2) == pc_rtx)
    return cond;
  else if (GET_CODE (XEXP (cond, 2)) == LABEL_REF
	   && XEXP (cond, 1) == pc_rtx)
    return gen_rtx_fmt_ee (reverse_condition (GET_CODE (cond)), GET_MODE (cond),
			   XEXP (cond, 0), XEXP (cond, 1));
  else
    return 0;
}

/* Return nonzero if conditions COND1 and COND2 can never be both true.  */

static int
conditions_mutex_p (cond1, cond2)
     rtx cond1, cond2;
{
  if (GET_RTX_CLASS (GET_CODE (cond1)) == '<'
      && GET_RTX_CLASS (GET_CODE (cond2)) == '<'
      && GET_CODE (cond1) == reverse_condition (GET_CODE (cond2))
      && XEXP (cond1, 0) == XEXP (cond2, 0)
      && XEXP (cond1, 1) == XEXP (cond2, 1))
    return 1;
  return 0;
}

/* Add ELEM wrapped in an INSN_LIST with reg note kind DEP_TYPE to the
   LOG_LINKS of INSN, if not already there.  DEP_TYPE indicates the type
   of dependence that this link represents.  */

void
add_dependence (insn, elem, dep_type)
     rtx insn;
     rtx elem;
     enum reg_note dep_type;
{
  rtx link, next;
  int present_p;
  rtx cond1, cond2;

  /* Don't depend an insn on itself.  */
  if (insn == elem)
    return;

  /* We can get a dependency on deleted insns due to optimizations in
     the register allocation and reloading or due to splitting.  Any
     such dependency is useless and can be ignored.  */
  if (GET_CODE (elem) == NOTE)
    return;

  /* flow.c doesn't handle conditional lifetimes entirely correctly;
     calls mess up the conditional lifetimes.  */
  /* ??? add_dependence is the wrong place to be eliding dependencies,
     as that forgets that the condition expressions themselves may
     be dependent.  */
  if (GET_CODE (insn) != CALL_INSN && GET_CODE (elem) != CALL_INSN)
    {
      cond1 = get_condition (insn);
      cond2 = get_condition (elem);
      if (cond1 && cond2
	  && conditions_mutex_p (cond1, cond2)
	  /* Make sure first instruction doesn't affect condition of second
	     instruction if switched.  */
	  && !modified_in_p (cond1, elem)
	  /* Make sure second instruction doesn't affect condition of first
	     instruction if switched.  */
	  && !modified_in_p (cond2, insn))
	return;
    }

  /* If elem is part of a sequence that must be scheduled together, then
     make the dependence point to the last insn of the sequence.
     When HAVE_cc0, it is possible for NOTEs to exist between users and
     setters of the condition codes, so we must skip past notes here.
     Otherwise, NOTEs are impossible here.  */
  next = next_nonnote_insn (elem);
  if (next && SCHED_GROUP_P (next)
      && GET_CODE (next) != CODE_LABEL)
    {
      /* Notes will never intervene here though, so don't bother checking
         for them.  */
      /* Hah!  Wrong.  */
      /* We must reject CODE_LABELs, so that we don't get confused by one
         that has LABEL_PRESERVE_P set, which is represented by the same
         bit in the rtl as SCHED_GROUP_P.  A CODE_LABEL can never be
         SCHED_GROUP_P.  */

      rtx nnext;
      while ((nnext = next_nonnote_insn (next)) != NULL
	     && SCHED_GROUP_P (nnext)
	     && GET_CODE (nnext) != CODE_LABEL)
	next = nnext;

      /* Again, don't depend an insn on itself.  */
      if (insn == next)
	return;

      /* Make the dependence to NEXT, the last insn of the group, instead
         of the original ELEM.  */
      elem = next;
    }

  present_p = 1;
#ifdef INSN_SCHEDULING
  /* ??? No good way to tell from here whether we're doing interblock
     scheduling.  Possibly add another callback.  */
#if 0
  /* (This code is guarded by INSN_SCHEDULING, otherwise INSN_BB is undefined.)
     No need for interblock dependences with calls, since
     calls are not moved between blocks.   Note: the edge where
     elem is a CALL is still required.  */
  if (GET_CODE (insn) == CALL_INSN
      && (INSN_BB (elem) != INSN_BB (insn)))
    return;
#endif

  /* If we already have a dependency for ELEM, then we do not need to
     do anything.  Avoiding the list walk below can cut compile times
     dramatically for some code.  */
  if (true_dependency_cache != NULL)
    {
      enum reg_note present_dep_type = 0;

      if (anti_dependency_cache == NULL || output_dependency_cache == NULL)
	abort ();
      if (TEST_BIT (true_dependency_cache[INSN_LUID (insn)], INSN_LUID (elem)))
	/* Do nothing (present_set_type is already 0).  */
	;
      else if (TEST_BIT (anti_dependency_cache[INSN_LUID (insn)],
			 INSN_LUID (elem)))
	present_dep_type = REG_DEP_ANTI;
      else if (TEST_BIT (output_dependency_cache[INSN_LUID (insn)],
			 INSN_LUID (elem)))
	present_dep_type = REG_DEP_OUTPUT;
      else 
	present_p = 0;
      if (present_p && (int) dep_type >= (int) present_dep_type)
	return;
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
	      if (REG_NOTE_KIND (link) == REG_DEP_ANTI)
		RESET_BIT (anti_dependency_cache[INSN_LUID (insn)],
			   INSN_LUID (elem));
	      else if (REG_NOTE_KIND (link) == REG_DEP_OUTPUT
		       && output_dependency_cache)
		RESET_BIT (output_dependency_cache[INSN_LUID (insn)],
			   INSN_LUID (elem));
	      else
		abort ();
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
		SET_BIT (true_dependency_cache[INSN_LUID (insn)],
			 INSN_LUID (elem));
	      else if (REG_NOTE_KIND (link) == REG_DEP_ANTI)
		SET_BIT (anti_dependency_cache[INSN_LUID (insn)],
			 INSN_LUID (elem));
	      else if (REG_NOTE_KIND (link) == REG_DEP_OUTPUT)
		SET_BIT (output_dependency_cache[INSN_LUID (insn)],
			 INSN_LUID (elem));
	    }
#endif
	  return;
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
	SET_BIT (true_dependency_cache[INSN_LUID (insn)], INSN_LUID (elem));
      else if (dep_type == REG_DEP_ANTI)
	SET_BIT (anti_dependency_cache[INSN_LUID (insn)], INSN_LUID (elem));
      else if (dep_type == REG_DEP_OUTPUT)
	SET_BIT (output_dependency_cache[INSN_LUID (insn)], INSN_LUID (elem));
    }
#endif
}

/* A convenience wrapper to operate on an entire list.  */

static void
add_dependence_list (insn, list, dep_type)
     rtx insn, list;
     enum reg_note dep_type;
{
  for (; list; list = XEXP (list, 1))
    add_dependence (insn, XEXP (list, 0), dep_type);
}

/* Similar, but free *LISTP at the same time.  */

static void
add_dependence_list_and_free (insn, listp, dep_type)
     rtx insn;
     rtx *listp;
     enum reg_note dep_type;
{
  rtx list, next;
  for (list = *listp, *listp = NULL; list ; list = next)
    {
      next = XEXP (list, 1);
      add_dependence (insn, XEXP (list, 0), dep_type);
      free_INSN_LIST_node (list);
    }
}

/* Remove ELEM wrapped in an INSN_LIST from the LOG_LINKS
   of INSN.  Abort if not found.  */

static void
remove_dependence (insn, elem)
     rtx insn;
     rtx elem;
{
  rtx prev, link, next;
  int found = 0;

  for (prev = 0, link = LOG_LINKS (insn); link; link = next)
    {
      next = XEXP (link, 1);
      if (XEXP (link, 0) == elem)
	{
	  if (prev)
	    XEXP (prev, 1) = next;
	  else
	    LOG_LINKS (insn) = next;

#ifdef INSN_SCHEDULING
	  /* If we are removing a dependency from the LOG_LINKS list,
	     make sure to remove it from the cache too.  */
	  if (true_dependency_cache != NULL)
	    {
	      if (REG_NOTE_KIND (link) == 0)
		RESET_BIT (true_dependency_cache[INSN_LUID (insn)],
			   INSN_LUID (elem));
	      else if (REG_NOTE_KIND (link) == REG_DEP_ANTI)
		RESET_BIT (anti_dependency_cache[INSN_LUID (insn)],
			   INSN_LUID (elem));
	      else if (REG_NOTE_KIND (link) == REG_DEP_OUTPUT)
		RESET_BIT (output_dependency_cache[INSN_LUID (insn)],
			   INSN_LUID (elem));
	    }
#endif

	  free_INSN_LIST_node (link);

	  found = 1;
	}
      else
	prev = link;
    }

  if (!found)
    abort ();
  return;
}

/* Return an insn which represents a SCHED_GROUP, which is
   the last insn in the group.  */

static rtx
group_leader (insn)
     rtx insn;
{
  rtx prev;

  do
    {
      prev = insn;
      insn = next_nonnote_insn (insn);
    }
  while (insn && SCHED_GROUP_P (insn) && (GET_CODE (insn) != CODE_LABEL));

  return prev;
}

/* Set SCHED_GROUP_P and care for the rest of the bookkeeping that
   goes along with that.  */

static void
set_sched_group_p (insn)
     rtx insn;
{
  rtx link, prev;

  SCHED_GROUP_P (insn) = 1;

  /* There may be a note before this insn now, but all notes will
     be removed before we actually try to schedule the insns, so
     it won't cause a problem later.  We must avoid it here though.  */
  prev = prev_nonnote_insn (insn);

  /* Make a copy of all dependencies on the immediately previous insn,
     and add to this insn.  This is so that all the dependencies will
     apply to the group.  Remove an explicit dependence on this insn
     as SCHED_GROUP_P now represents it.  */

  if (find_insn_list (prev, LOG_LINKS (insn)))
    remove_dependence (insn, prev);

  for (link = LOG_LINKS (prev); link; link = XEXP (link, 1))
    add_dependence (insn, XEXP (link, 0), REG_NOTE_KIND (link));
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

void
add_insn_mem_dependence (deps, insn_list, mem_list, insn, mem)
     struct deps *deps;
     rtx *insn_list, *mem_list, insn, mem;
{
  rtx link;

  link = alloc_INSN_LIST (insn, *insn_list);
  *insn_list = link;

  if (current_sched_info->use_cselib)
    {
      mem = shallow_copy_rtx (mem);
      XEXP (mem, 0) = cselib_subst_to_values (XEXP (mem, 0));
    }
  link = alloc_EXPR_LIST (VOIDmode, mem, *mem_list);
  *mem_list = link;

  deps->pending_lists_length++;
}

/* Make a dependency between every memory reference on the pending lists
   and INSN, thus flushing the pending lists.  FOR_READ is true if emitting
   dependencies for a read operation, similarly with FOR_WRITE.  */

static void
flush_pending_lists (deps, insn, for_read, for_write)
     struct deps *deps;
     rtx insn;
     int for_read, for_write;
{
  if (for_write)
    {
      add_dependence_list_and_free (insn, &deps->pending_read_insns,
				    REG_DEP_ANTI);
      free_EXPR_LIST_list (&deps->pending_read_mems);
    }

  add_dependence_list_and_free (insn, &deps->pending_write_insns,
				for_read ? REG_DEP_ANTI : REG_DEP_OUTPUT);
  free_EXPR_LIST_list (&deps->pending_write_mems);
  deps->pending_lists_length = 0;

  add_dependence_list_and_free (insn, &deps->last_pending_memory_flush,
				for_read ? REG_DEP_ANTI : REG_DEP_OUTPUT);
  deps->last_pending_memory_flush = alloc_INSN_LIST (insn, NULL_RTX);
  deps->pending_flush_length = 1;
}

/* Analyze a single SET, CLOBBER, PRE_DEC, POST_DEC, PRE_INC or POST_INC
   rtx, X, creating all dependencies generated by the write to the
   destination of X, and reads of everything mentioned.  */

static void
sched_analyze_1 (deps, x, insn)
     struct deps *deps;
     rtx x;
     rtx insn;
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
	 || GET_CODE (dest) == ZERO_EXTRACT || GET_CODE (dest) == SIGN_EXTRACT)
    {
      if (GET_CODE (dest) == ZERO_EXTRACT || GET_CODE (dest) == SIGN_EXTRACT)
	{
	  /* The second and third arguments are values read by this insn.  */
	  sched_analyze_2 (deps, XEXP (dest, 1), insn);
	  sched_analyze_2 (deps, XEXP (dest, 2), insn);
	}
      dest = XEXP (dest, 0);
    }

  if (GET_CODE (dest) == REG)
    {
      regno = REGNO (dest);

      /* A hard reg in a wide mode may really be multiple registers.
         If so, mark all of them just like the first.  */
      if (regno < FIRST_PSEUDO_REGISTER)
	{
	  int i = HARD_REGNO_NREGS (regno, GET_MODE (dest));
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
	  if (GET_CODE (PATTERN (insn)) != USE
	      && GET_CODE (PATTERN (insn)) != CLOBBER)
	    abort ();
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
	  if (!reload_completed
	      && reg_known_equiv_p[regno]
	      && GET_CODE (reg_known_value[regno]) == MEM)
	    sched_analyze_2 (deps, XEXP (reg_known_value[regno], 0), insn);

	  /* Don't let it cross a call after scheduling if it doesn't
	     already cross one.  */
	  if (REG_N_CALLS_CROSSED (regno) == 0)
	    add_dependence_list (insn, deps->last_function_call, REG_DEP_ANTI);
	}
    }
  else if (GET_CODE (dest) == MEM)
    {
      /* Writing memory.  */
      rtx t = dest;

      if (current_sched_info->use_cselib)
	{
	  t = shallow_copy_rtx (dest);
	  cselib_lookup (XEXP (t, 0), Pmode, 1);
	  XEXP (t, 0) = cselib_subst_to_values (XEXP (t, 0));
	}

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
	      if (anti_dependence (XEXP (pending_mem, 0), t))
		add_dependence (insn, XEXP (pending, 0), REG_DEP_ANTI);

	      pending = XEXP (pending, 1);
	      pending_mem = XEXP (pending_mem, 1);
	    }

	  pending = deps->pending_write_insns;
	  pending_mem = deps->pending_write_mems;
	  while (pending)
	    {
	      if (output_dependence (XEXP (pending_mem, 0), t))
		add_dependence (insn, XEXP (pending, 0), REG_DEP_OUTPUT);

	      pending = XEXP (pending, 1);
	      pending_mem = XEXP (pending_mem, 1);
	    }

	  add_dependence_list (insn, deps->last_pending_memory_flush,
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
sched_analyze_2 (deps, x, insn)
     struct deps *deps;
     rtx x;
     rtx insn;
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
      set_sched_group_p (insn);
      return;
#endif

    case REG:
      {
	int regno = REGNO (x);
	if (regno < FIRST_PSEUDO_REGISTER)
	  {
	    int i = HARD_REGNO_NREGS (regno, GET_MODE (x));
	    while (--i >= 0)
	      SET_REGNO_REG_SET (reg_pending_uses, regno + i);
	  }
	/* ??? Reload sometimes emits USEs and CLOBBERs of pseudos that
	   it does not reload.  Ignore these as they have served their
	   purpose already.  */
	else if (regno >= deps->max_reg)
	  {
	    if (GET_CODE (PATTERN (insn)) != USE
		&& GET_CODE (PATTERN (insn)) != CLOBBER)
	      abort ();
	  }
	else
	  {
	    SET_REGNO_REG_SET (reg_pending_uses, regno);

	    /* Pseudos that are REG_EQUIV to something may be replaced
	       by that during reloading.  We need only add dependencies for
	       the address in the REG_EQUIV note.  */
	    if (!reload_completed
		&& reg_known_equiv_p[regno]
		&& GET_CODE (reg_known_value[regno]) == MEM)
	      sched_analyze_2 (deps, XEXP (reg_known_value[regno], 0), insn);

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
	pending = deps->pending_read_insns;
	pending_mem = deps->pending_read_mems;
	while (pending)
	  {
	    if (read_dependence (XEXP (pending_mem, 0), t))
	      add_dependence (insn, XEXP (pending, 0), REG_DEP_ANTI);

	    pending = XEXP (pending, 1);
	    pending_mem = XEXP (pending_mem, 1);
	  }

	pending = deps->pending_write_insns;
	pending_mem = deps->pending_write_mems;
	while (pending)
	  {
	    if (true_dependence (XEXP (pending_mem, 0), VOIDmode,
				 t, rtx_varies_p))
	      add_dependence (insn, XEXP (pending, 0), 0);

	    pending = XEXP (pending, 1);
	    pending_mem = XEXP (pending_mem, 1);
	  }

	for (u = deps->last_pending_memory_flush; u; u = XEXP (u, 1))
	  if (GET_CODE (XEXP (u, 0)) != JUMP_INSN
	      || deps_may_trap_p (x))
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
	  reg_pending_barrier = true;

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
sched_analyze_insn (deps, x, insn, loop_notes)
     struct deps *deps;
     rtx x, insn;
     rtx loop_notes;
{
  RTX_CODE code = GET_CODE (x);
  rtx link;
  int i;

  if (code == COND_EXEC)
    {
      sched_analyze_2 (deps, COND_EXEC_TEST (x), insn);

      /* ??? Should be recording conditions so we reduce the number of
	 false dependencies.  */
      x = COND_EXEC_CODE (x);
      code = GET_CODE (x);
    }
  if (code == SET || code == CLOBBER)
    sched_analyze_1 (deps, x, insn);
  else if (code == PARALLEL)
    {
      int i;
      for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
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
  if (GET_CODE (insn) == CALL_INSN)
    {
      for (link = CALL_INSN_FUNCTION_USAGE (insn); link; link = XEXP (link, 1))
	{
	  if (GET_CODE (XEXP (link, 0)) == CLOBBER)
	    sched_analyze_1 (deps, XEXP (link, 0), insn);
	  else
	    sched_analyze_2 (deps, XEXP (link, 0), insn);
	}
      if (find_reg_note (insn, REG_SETJMP, NULL))
	reg_pending_barrier = true;
    }

  if (GET_CODE (insn) == JUMP_INSN)
    {
      rtx next;
      next = next_nonnote_insn (insn);
      if (next && GET_CODE (next) == BARRIER)
	reg_pending_barrier = true;
      else
	{
	  rtx pending, pending_mem;
	  regset_head tmp;
	  INIT_REG_SET (&tmp);

	  (*current_sched_info->compute_jump_reg_dependencies) (insn, &tmp);
	  IOR_REG_SET (reg_pending_uses, &tmp);
	  CLEAR_REG_SET (&tmp);

	  /* All memory writes and volatile reads must happen before the
	     jump.  Non-volatile reads must happen before the jump iff
	     the result is needed by the above register used mask.  */

	  pending = deps->pending_write_insns;
	  pending_mem = deps->pending_write_mems;
	  while (pending)
	    {
	      add_dependence (insn, XEXP (pending, 0), REG_DEP_OUTPUT);
	      pending = XEXP (pending, 1);
	      pending_mem = XEXP (pending_mem, 1);
	    }

	  pending = deps->pending_read_insns;
	  pending_mem = deps->pending_read_mems;
	  while (pending)
	    {
	      if (MEM_VOLATILE_P (XEXP (pending_mem, 0)))
		add_dependence (insn, XEXP (pending, 0), REG_DEP_OUTPUT);
	      pending = XEXP (pending, 1);
	      pending_mem = XEXP (pending_mem, 1);
	    }

	  add_dependence_list (insn, deps->last_pending_memory_flush,
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

      /* Update loop_notes with any notes from this insn.  Also determine
	 if any of the notes on the list correspond to instruction scheduling
	 barriers (loop, eh & setjmp notes, but not range notes).  */
      link = loop_notes;
      while (XEXP (link, 1))
	{
	  if (INTVAL (XEXP (link, 0)) == NOTE_INSN_LOOP_BEG
	      || INTVAL (XEXP (link, 0)) == NOTE_INSN_LOOP_END
	      || INTVAL (XEXP (link, 0)) == NOTE_INSN_EH_REGION_BEG
	      || INTVAL (XEXP (link, 0)) == NOTE_INSN_EH_REGION_END)
	    reg_pending_barrier = true;

	  link = XEXP (link, 1);
	}
      XEXP (link, 1) = REG_NOTES (insn);
      REG_NOTES (insn) = loop_notes;
    }

  /* If this instruction can throw an exception, then moving it changes
     where block boundaries fall.  This is mighty confusing elsewhere. 
     Therefore, prevent such an instruction from being moved.  */
  if (can_throw_internal (insn))
    reg_pending_barrier = true;

  /* Add dependencies if a scheduling barrier was found.  */
  if (reg_pending_barrier)
    {
      if (GET_CODE (PATTERN (insn)) == COND_EXEC)
	{
	  EXECUTE_IF_SET_IN_REG_SET (&deps->reg_last_in_use, 0, i,
	    {
	      struct deps_reg *reg_last = &deps->reg_last[i];
	      add_dependence_list (insn, reg_last->uses, REG_DEP_ANTI);
	      add_dependence_list (insn, reg_last->sets, 0);
	      add_dependence_list (insn, reg_last->clobbers, 0);
	    });
	}
      else
	{
	  EXECUTE_IF_SET_IN_REG_SET (&deps->reg_last_in_use, 0, i,
	    {
	      struct deps_reg *reg_last = &deps->reg_last[i];
	      add_dependence_list_and_free (insn, &reg_last->uses,
					    REG_DEP_ANTI);
	      add_dependence_list_and_free (insn, &reg_last->sets, 0);
	      add_dependence_list_and_free (insn, &reg_last->clobbers, 0);
	      reg_last->uses_length = 0;
	      reg_last->clobbers_length = 0;
	    });
	}

      for (i = 0; i < deps->max_reg; i++)
	{
	  struct deps_reg *reg_last = &deps->reg_last[i];
	  reg_last->sets = alloc_INSN_LIST (insn, reg_last->sets);
	  SET_REGNO_REG_SET (&deps->reg_last_in_use, i);
	}

      flush_pending_lists (deps, insn, true, true);
      reg_pending_barrier = false;
    }
  else
    {
      /* If the current insn is conditional, we can't free any
	 of the lists.  */
      if (GET_CODE (PATTERN (insn)) == COND_EXEC)
	{
	  EXECUTE_IF_SET_IN_REG_SET (reg_pending_uses, 0, i,
	    {
	      struct deps_reg *reg_last = &deps->reg_last[i];
	      add_dependence_list (insn, reg_last->sets, 0);
	      add_dependence_list (insn, reg_last->clobbers, 0);
	      reg_last->uses = alloc_INSN_LIST (insn, reg_last->uses);
	      reg_last->uses_length++;
	    });
	  EXECUTE_IF_SET_IN_REG_SET (reg_pending_clobbers, 0, i,
	    {
	      struct deps_reg *reg_last = &deps->reg_last[i];
	      add_dependence_list (insn, reg_last->sets, REG_DEP_OUTPUT);
	      add_dependence_list (insn, reg_last->uses, REG_DEP_ANTI);
	      reg_last->clobbers = alloc_INSN_LIST (insn, reg_last->clobbers);
	      reg_last->clobbers_length++;
	    });
	  EXECUTE_IF_SET_IN_REG_SET (reg_pending_sets, 0, i,
	    {
	      struct deps_reg *reg_last = &deps->reg_last[i];
	      add_dependence_list (insn, reg_last->sets, REG_DEP_OUTPUT);
	      add_dependence_list (insn, reg_last->clobbers, REG_DEP_OUTPUT);
	      add_dependence_list (insn, reg_last->uses, REG_DEP_ANTI);
	      reg_last->sets = alloc_INSN_LIST (insn, reg_last->sets);
	    });
	}
      else
	{
	  EXECUTE_IF_SET_IN_REG_SET (reg_pending_uses, 0, i,
	    {
	      struct deps_reg *reg_last = &deps->reg_last[i];
	      add_dependence_list (insn, reg_last->sets, 0);
	      add_dependence_list (insn, reg_last->clobbers, 0);
	      reg_last->uses_length++;
	      reg_last->uses = alloc_INSN_LIST (insn, reg_last->uses);
	    });
	  EXECUTE_IF_SET_IN_REG_SET (reg_pending_clobbers, 0, i,
	    {
	      struct deps_reg *reg_last = &deps->reg_last[i];
	      add_dependence_list (insn, reg_last->sets, REG_DEP_OUTPUT);
	      add_dependence_list (insn, reg_last->uses, REG_DEP_ANTI);
	      if (reg_last->uses_length > MAX_PENDING_LIST_LENGTH
		  || reg_last->clobbers_length > MAX_PENDING_LIST_LENGTH)
		{
		  add_dependence_list_and_free (insn, &reg_last->sets,
					        REG_DEP_OUTPUT);
		  add_dependence_list_and_free (insn, &reg_last->uses,
						REG_DEP_ANTI);
		  add_dependence_list_and_free (insn, &reg_last->clobbers,
						REG_DEP_OUTPUT);
		  reg_last->clobbers_length = 0;
		  reg_last->uses_length = 0;
		}
	      else
		{
		  add_dependence_list (insn, reg_last->sets, REG_DEP_OUTPUT);
		  add_dependence_list (insn, reg_last->uses, REG_DEP_ANTI);
		}
	      reg_last->clobbers_length++;
	      reg_last->clobbers = alloc_INSN_LIST (insn, reg_last->clobbers);
	    });
	  EXECUTE_IF_SET_IN_REG_SET (reg_pending_sets, 0, i,
	    {
	      struct deps_reg *reg_last = &deps->reg_last[i];
	      add_dependence_list_and_free (insn, &reg_last->sets,
					    REG_DEP_OUTPUT);
	      add_dependence_list_and_free (insn, &reg_last->clobbers,
					    REG_DEP_OUTPUT);
	      add_dependence_list_and_free (insn, &reg_last->uses,
					    REG_DEP_ANTI);
	      reg_last->sets = alloc_INSN_LIST (insn, reg_last->sets);
	      reg_last->uses_length = 0;
	      reg_last->clobbers_length = 0;
	    });
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
      set_sched_group_p (insn);
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
      if (GET_CODE (tmp) == REG)
	dest_regno = REGNO (tmp);
      else
	goto end_call_group;

      tmp = SET_SRC (set);
      if (GET_CODE (tmp) == SUBREG)
	tmp = SUBREG_REG (tmp);
      if (GET_CODE (tmp) == REG)
	src_regno = REGNO (tmp);
      else
	goto end_call_group;

      if (src_regno < FIRST_PSEUDO_REGISTER
	  || dest_regno < FIRST_PSEUDO_REGISTER)
	{
	  set_sched_group_p (insn);
	  CANT_MOVE (insn) = 1;
	}
      else
	{
	end_call_group:
	  deps->in_post_call_group_p = false;
	}
    }
}

/* Analyze every insn between HEAD and TAIL inclusive, creating LOG_LINKS
   for every dependency.  */

void
sched_analyze (deps, head, tail)
     struct deps *deps;
     rtx head, tail;
{
  rtx insn;
  rtx loop_notes = 0;

  if (current_sched_info->use_cselib)
    cselib_init ();

  for (insn = head;; insn = NEXT_INSN (insn))
    {
      rtx link, end_seq, r0, set, note;

      if (GET_CODE (insn) == INSN || GET_CODE (insn) == JUMP_INSN)
	{
	  /* Clear out the stale LOG_LINKS from flow.  */
	  free_INSN_LIST_list (&LOG_LINKS (insn));

	  /* Clear out stale SCHED_GROUP_P.  */
	  SCHED_GROUP_P (insn) = 0;

	  /* Make each JUMP_INSN a scheduling barrier for memory
             references.  */
	  if (GET_CODE (insn) == JUMP_INSN)
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
      else if (GET_CODE (insn) == CALL_INSN)
	{
	  int i;

	  CANT_MOVE (insn) = 1;

	  /* Clear out the stale LOG_LINKS from flow.  */
	  free_INSN_LIST_list (&LOG_LINKS (insn));

	  if (find_reg_note (insn, REG_SETJMP, NULL))
	    {
	      /* This is setjmp.  Assume that all registers, not just
		 hard registers, may be clobbered by this call.  */
	      reg_pending_barrier = true;
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
		/* Other call-clobbered hard regs may be clobbered.  */
		else if (TEST_HARD_REG_BIT (regs_invalidated_by_call, i))
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
	  add_dependence_list_and_free (insn, &deps->sched_before_next_call,
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
	    deps->in_post_call_group_p = true;
	}

      /* See comments on reemit_notes as to why we do this.
	 ??? Actually, the reemit_notes just say what is done, not why.  */

      else if (GET_CODE (insn) == NOTE
	       && (NOTE_LINE_NUMBER (insn) == NOTE_INSN_RANGE_BEG
		   || NOTE_LINE_NUMBER (insn) == NOTE_INSN_RANGE_END))
	{
	  loop_notes = alloc_EXPR_LIST (REG_SAVE_NOTE, NOTE_RANGE_INFO (insn),
					loop_notes);
	  loop_notes = alloc_EXPR_LIST (REG_SAVE_NOTE,
					GEN_INT (NOTE_LINE_NUMBER (insn)),
					loop_notes);
	}
      else if (GET_CODE (insn) == NOTE
	       && (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_BEG
		   || NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_END
		   || NOTE_LINE_NUMBER (insn) == NOTE_INSN_EH_REGION_BEG
		   || NOTE_LINE_NUMBER (insn) == NOTE_INSN_EH_REGION_END))
	{
	  rtx rtx_region;

	  if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_EH_REGION_BEG
	      || NOTE_LINE_NUMBER (insn) == NOTE_INSN_EH_REGION_END)
	    rtx_region = GEN_INT (NOTE_EH_HANDLER (insn));
	  else
	    rtx_region = GEN_INT (0);

	  loop_notes = alloc_EXPR_LIST (REG_SAVE_NOTE,
					rtx_region,
					loop_notes);
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
	  && GET_CODE (insn) == INSN
	  && GET_CODE (PATTERN (insn)) == CLOBBER
          && (r0 = XEXP (PATTERN (insn), 0), GET_CODE (r0) == REG)
	  && GET_CODE (XEXP (PATTERN (insn), 0)) == REG
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
  abort ();
}

/* Examine insns in the range [ HEAD, TAIL ] and Use the backward
   dependences from LOG_LINKS to build forward dependences in
   INSN_DEPEND.  */

void
compute_forward_dependences (head, tail)
     rtx head, tail;
{
  rtx insn, link;
  rtx next_tail;
  enum reg_note dep_type;

  next_tail = NEXT_INSN (tail);
  for (insn = head; insn != next_tail; insn = NEXT_INSN (insn))
    {
      if (! INSN_P (insn))
	continue;

      insn = group_leader (insn);

      for (link = LOG_LINKS (insn); link; link = XEXP (link, 1))
	{
	  rtx x = group_leader (XEXP (link, 0));
	  rtx new_link;

	  if (x != XEXP (link, 0))
	    continue;

#ifdef ENABLE_CHECKING
	  /* If add_dependence is working properly there should never
	     be notes, deleted insns or duplicates in the backward
	     links.  Thus we need not check for them here.

	     However, if we have enabled checking we might as well go
	     ahead and verify that add_dependence worked properly.  */
	  if (GET_CODE (x) == NOTE
	      || INSN_DELETED_P (x)
	      || (forward_dependency_cache != NULL
		  && TEST_BIT (forward_dependency_cache[INSN_LUID (x)],
			       INSN_LUID (insn)))
	      || (forward_dependency_cache == NULL
		  && find_insn_list (insn, INSN_DEPEND (x))))
	    abort ();
	  if (forward_dependency_cache != NULL)
	    SET_BIT (forward_dependency_cache[INSN_LUID (x)],
		     INSN_LUID (insn));
#endif

	  new_link = alloc_INSN_LIST (insn, INSN_DEPEND (x));

	  dep_type = REG_NOTE_KIND (link);
	  PUT_REG_NOTE_KIND (new_link, dep_type);

	  INSN_DEPEND (x) = new_link;
	  INSN_DEP_COUNT (insn) += 1;
	}
    }
}

/* Initialize variables for region data dependence analysis.
   n_bbs is the number of region blocks.  */

void
init_deps (deps)
     struct deps *deps;
{
  int max_reg = (reload_completed ? FIRST_PSEUDO_REGISTER : max_reg_num ());

  deps->max_reg = max_reg;
  deps->reg_last = (struct deps_reg *)
    xcalloc (max_reg, sizeof (struct deps_reg));
  INIT_REG_SET (&deps->reg_last_in_use);

  deps->pending_read_insns = 0;
  deps->pending_read_mems = 0;
  deps->pending_write_insns = 0;
  deps->pending_write_mems = 0;
  deps->pending_lists_length = 0;
  deps->pending_flush_length = 0;
  deps->last_pending_memory_flush = 0;
  deps->last_function_call = 0;
  deps->sched_before_next_call = 0;
  deps->in_post_call_group_p = false;
  deps->libcall_block_tail_insn = 0;
}

/* Free insn lists found in DEPS.  */

void
free_deps (deps)
     struct deps *deps;
{
  int i;

  free_INSN_LIST_list (&deps->pending_read_insns);
  free_EXPR_LIST_list (&deps->pending_read_mems);
  free_INSN_LIST_list (&deps->pending_write_insns);
  free_EXPR_LIST_list (&deps->pending_write_mems);
  free_INSN_LIST_list (&deps->last_pending_memory_flush);

  /* Without the EXECUTE_IF_SET, this loop is executed max_reg * nr_regions
     times.  For a test case with 42000 regs and 8000 small basic blocks,
     this loop accounted for nearly 60% (84 sec) of the total -O2 runtime.  */
  EXECUTE_IF_SET_IN_REG_SET (&deps->reg_last_in_use, 0, i,
    {
      struct deps_reg *reg_last = &deps->reg_last[i];
      free_INSN_LIST_list (&reg_last->uses);
      free_INSN_LIST_list (&reg_last->sets);
      free_INSN_LIST_list (&reg_last->clobbers);
    });
  CLEAR_REG_SET (&deps->reg_last_in_use);

  free (deps->reg_last);
}

/* If it is profitable to use them, initialize caches for tracking
   dependency informatino.  LUID is the number of insns to be scheduled,
   it is used in the estimate of profitability.  */

void
init_dependency_caches (luid)
     int luid;
{
  /* ?!? We could save some memory by computing a per-region luid mapping
     which could reduce both the number of vectors in the cache and the size
     of each vector.  Instead we just avoid the cache entirely unless the
     average number of instructions in a basic block is very high.  See
     the comment before the declaration of true_dependency_cache for
     what we consider "very high".  */
  if (luid / n_basic_blocks > 100 * 5)
    {
      true_dependency_cache = sbitmap_vector_alloc (luid, luid);
      sbitmap_vector_zero (true_dependency_cache, luid);
      anti_dependency_cache = sbitmap_vector_alloc (luid, luid);
      sbitmap_vector_zero (anti_dependency_cache, luid);
      output_dependency_cache = sbitmap_vector_alloc (luid, luid);
      sbitmap_vector_zero (output_dependency_cache, luid);
#ifdef ENABLE_CHECKING
      forward_dependency_cache = sbitmap_vector_alloc (luid, luid);
      sbitmap_vector_zero (forward_dependency_cache, luid);
#endif
    }
}

/* Free the caches allocated in init_dependency_caches.  */

void
free_dependency_caches ()
{
  if (true_dependency_cache)
    {
      sbitmap_vector_free (true_dependency_cache);
      true_dependency_cache = NULL;
      sbitmap_vector_free (anti_dependency_cache);
      anti_dependency_cache = NULL;
      sbitmap_vector_free (output_dependency_cache);
      output_dependency_cache = NULL;
#ifdef ENABLE_CHECKING
      sbitmap_vector_free (forward_dependency_cache);
      forward_dependency_cache = NULL;
#endif
    }
}

/* Initialize some global variables needed by the dependency analysis
   code.  */

void
init_deps_global ()
{
  reg_pending_sets = INITIALIZE_REG_SET (reg_pending_sets_head);
  reg_pending_clobbers = INITIALIZE_REG_SET (reg_pending_clobbers_head);
  reg_pending_uses = INITIALIZE_REG_SET (reg_pending_uses_head);
  reg_pending_barrier = false;
}

/* Free everything used by the dependency analysis code.  */

void
finish_deps_global ()
{
  FREE_REG_SET (reg_pending_sets);
  FREE_REG_SET (reg_pending_clobbers);
  FREE_REG_SET (reg_pending_uses);
}
