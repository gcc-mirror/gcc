/* Instruction scheduling pass.  This file computes dependencies between
   instructions.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com) Enhanced by,
   and currently maintained by, Jim Wilson (wilson@cygnus.com)

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
#include "ira.h"
#include "target.h"

#ifdef INSN_SCHEDULING

#ifdef ENABLE_CHECKING
#define CHECK (true)
#else
#define CHECK (false)
#endif

/* Holds current parameters for the dependency analyzer.  */
struct sched_deps_info_def *sched_deps_info;

/* The data is specific to the Haifa scheduler.  */
VEC(haifa_deps_insn_data_def, heap) *h_d_i_d = NULL;

/* Return the major type present in the DS.  */
enum reg_note
ds_to_dk (ds_t ds)
{
  if (ds & DEP_TRUE)
    return REG_DEP_TRUE;

  if (ds & DEP_OUTPUT)
    return REG_DEP_OUTPUT;

  gcc_assert (ds & DEP_ANTI);

  return REG_DEP_ANTI;
}

/* Return equivalent dep_status.  */
ds_t
dk_to_ds (enum reg_note dk)
{
  switch (dk)
    {
    case REG_DEP_TRUE:
      return DEP_TRUE;

    case REG_DEP_OUTPUT:
      return DEP_OUTPUT;

    default:
      gcc_assert (dk == REG_DEP_ANTI);
      return DEP_ANTI;
    }
}

/* Functions to operate with dependence information container - dep_t.  */

/* Init DEP with the arguments.  */
void
init_dep_1 (dep_t dep, rtx pro, rtx con, enum reg_note type, ds_t ds)
{
  DEP_PRO (dep) = pro;
  DEP_CON (dep) = con;
  DEP_TYPE (dep) = type;
  DEP_STATUS (dep) = ds;
}

/* Init DEP with the arguments.
   While most of the scheduler (including targets) only need the major type
   of the dependency, it is convenient to hide full dep_status from them.  */
void
init_dep (dep_t dep, rtx pro, rtx con, enum reg_note kind)
{
  ds_t ds;

  if ((current_sched_info->flags & USE_DEPS_LIST))
    ds = dk_to_ds (kind);
  else
    ds = -1;

  init_dep_1 (dep, pro, con, kind, ds);
}

/* Make a copy of FROM in TO.  */
static void
copy_dep (dep_t to, dep_t from)
{
  memcpy (to, from, sizeof (*to));
}

static void dump_ds (FILE *, ds_t);

/* Define flags for dump_dep ().  */

/* Dump producer of the dependence.  */
#define DUMP_DEP_PRO (2)

/* Dump consumer of the dependence.  */
#define DUMP_DEP_CON (4)

/* Dump type of the dependence.  */
#define DUMP_DEP_TYPE (8)

/* Dump status of the dependence.  */
#define DUMP_DEP_STATUS (16)

/* Dump all information about the dependence.  */
#define DUMP_DEP_ALL (DUMP_DEP_PRO | DUMP_DEP_CON | DUMP_DEP_TYPE	\
		      |DUMP_DEP_STATUS)

/* Dump DEP to DUMP.
   FLAGS is a bit mask specifying what information about DEP needs
   to be printed.
   If FLAGS has the very first bit set, then dump all information about DEP
   and propagate this bit into the callee dump functions.  */
static void
dump_dep (FILE *dump, dep_t dep, int flags)
{
  if (flags & 1)
    flags |= DUMP_DEP_ALL;

  fprintf (dump, "<");

  if (flags & DUMP_DEP_PRO)
    fprintf (dump, "%d; ", INSN_UID (DEP_PRO (dep)));

  if (flags & DUMP_DEP_CON)
    fprintf (dump, "%d; ", INSN_UID (DEP_CON (dep)));

  if (flags & DUMP_DEP_TYPE)
    {
      char t;
      enum reg_note type = DEP_TYPE (dep);

      switch (type)
	{
	case REG_DEP_TRUE:
	  t = 't';
	  break;

	case REG_DEP_OUTPUT:
	  t = 'o';
	  break;

	case REG_DEP_ANTI:
	  t = 'a';
	  break;

	default:
	  gcc_unreachable ();
	  break;
	}

      fprintf (dump, "%c; ", t);
    }

  if (flags & DUMP_DEP_STATUS)
    {
      if (current_sched_info->flags & USE_DEPS_LIST)
	dump_ds (dump, DEP_STATUS (dep));
    }

  fprintf (dump, ">");
}

/* Default flags for dump_dep ().  */
static int dump_dep_flags = (DUMP_DEP_PRO | DUMP_DEP_CON);

/* Dump all fields of DEP to STDERR.  */
void
sd_debug_dep (dep_t dep)
{
  dump_dep (stderr, dep, 1);
  fprintf (stderr, "\n");
}

/* Determine whether DEP is a dependency link of a non-debug insn on a
   debug insn.  */

static inline bool
depl_on_debug_p (dep_link_t dep)
{
  return (DEBUG_INSN_P (DEP_LINK_PRO (dep))
	  && !DEBUG_INSN_P (DEP_LINK_CON (dep)));
}

/* Functions to operate with a single link from the dependencies lists -
   dep_link_t.  */

/* Attach L to appear after link X whose &DEP_LINK_NEXT (X) is given by
   PREV_NEXT_P.  */
static void
attach_dep_link (dep_link_t l, dep_link_t *prev_nextp)
{
  dep_link_t next = *prev_nextp;

  gcc_assert (DEP_LINK_PREV_NEXTP (l) == NULL
	      && DEP_LINK_NEXT (l) == NULL);

  /* Init node being inserted.  */
  DEP_LINK_PREV_NEXTP (l) = prev_nextp;
  DEP_LINK_NEXT (l) = next;

  /* Fix next node.  */
  if (next != NULL)
    {
      gcc_assert (DEP_LINK_PREV_NEXTP (next) == prev_nextp);

      DEP_LINK_PREV_NEXTP (next) = &DEP_LINK_NEXT (l);
    }

  /* Fix prev node.  */
  *prev_nextp = l;
}

/* Add dep_link LINK to deps_list L.  */
static void
add_to_deps_list (dep_link_t link, deps_list_t l)
{
  attach_dep_link (link, &DEPS_LIST_FIRST (l));

  /* Don't count debug deps.  */
  if (!depl_on_debug_p (link))
    ++DEPS_LIST_N_LINKS (l);
}

/* Detach dep_link L from the list.  */
static void
detach_dep_link (dep_link_t l)
{
  dep_link_t *prev_nextp = DEP_LINK_PREV_NEXTP (l);
  dep_link_t next = DEP_LINK_NEXT (l);

  *prev_nextp = next;

  if (next != NULL)
    DEP_LINK_PREV_NEXTP (next) = prev_nextp;

  DEP_LINK_PREV_NEXTP (l) = NULL;
  DEP_LINK_NEXT (l) = NULL;
}

/* Remove link LINK from list LIST.  */
static void
remove_from_deps_list (dep_link_t link, deps_list_t list)
{
  detach_dep_link (link);

  /* Don't count debug deps.  */
  if (!depl_on_debug_p (link))
    --DEPS_LIST_N_LINKS (list);
}

/* Move link LINK from list FROM to list TO.  */
static void
move_dep_link (dep_link_t link, deps_list_t from, deps_list_t to)
{
  remove_from_deps_list (link, from);
  add_to_deps_list (link, to);
}

/* Return true of LINK is not attached to any list.  */
static bool
dep_link_is_detached_p (dep_link_t link)
{
  return DEP_LINK_PREV_NEXTP (link) == NULL;
}

/* Pool to hold all dependency nodes (dep_node_t).  */
static alloc_pool dn_pool;

/* Number of dep_nodes out there.  */
static int dn_pool_diff = 0;

/* Create a dep_node.  */
static dep_node_t
create_dep_node (void)
{
  dep_node_t n = (dep_node_t) pool_alloc (dn_pool);
  dep_link_t back = DEP_NODE_BACK (n);
  dep_link_t forw = DEP_NODE_FORW (n);

  DEP_LINK_NODE (back) = n;
  DEP_LINK_NEXT (back) = NULL;
  DEP_LINK_PREV_NEXTP (back) = NULL;

  DEP_LINK_NODE (forw) = n;
  DEP_LINK_NEXT (forw) = NULL;
  DEP_LINK_PREV_NEXTP (forw) = NULL;

  ++dn_pool_diff;

  return n;
}

/* Delete dep_node N.  N must not be connected to any deps_list.  */
static void
delete_dep_node (dep_node_t n)
{
  gcc_assert (dep_link_is_detached_p (DEP_NODE_BACK (n))
	      && dep_link_is_detached_p (DEP_NODE_FORW (n)));

  --dn_pool_diff;

  pool_free (dn_pool, n);
}

/* Pool to hold dependencies lists (deps_list_t).  */
static alloc_pool dl_pool;

/* Number of deps_lists out there.  */
static int dl_pool_diff = 0;

/* Functions to operate with dependences lists - deps_list_t.  */

/* Return true if list L is empty.  */
static bool
deps_list_empty_p (deps_list_t l)
{
  return DEPS_LIST_N_LINKS (l) == 0;
}

/* Create a new deps_list.  */
static deps_list_t
create_deps_list (void)
{
  deps_list_t l = (deps_list_t) pool_alloc (dl_pool);

  DEPS_LIST_FIRST (l) = NULL;
  DEPS_LIST_N_LINKS (l) = 0;

  ++dl_pool_diff;
  return l;
}

/* Free deps_list L.  */
static void
free_deps_list (deps_list_t l)
{
  gcc_assert (deps_list_empty_p (l));

  --dl_pool_diff;

  pool_free (dl_pool, l);
}

/* Return true if there is no dep_nodes and deps_lists out there.
   After the region is scheduled all the dependency nodes and lists
   should [generally] be returned to pool.  */
bool
deps_pools_are_empty_p (void)
{
  return dn_pool_diff == 0 && dl_pool_diff == 0;
}

/* Remove all elements from L.  */
static void
clear_deps_list (deps_list_t l)
{
  do
    {
      dep_link_t link = DEPS_LIST_FIRST (l);

      if (link == NULL)
	break;

      remove_from_deps_list (link, l);
    }
  while (1);
}

static regset reg_pending_sets;
static regset reg_pending_clobbers;
static regset reg_pending_uses;
static enum reg_pending_barrier_mode reg_pending_barrier;

/* Hard registers implicitly clobbered or used (or may be implicitly
   clobbered or used) by the currently analyzed insn.  For example,
   insn in its constraint has one register class.  Even if there is
   currently no hard register in the insn, the particular hard
   register will be in the insn after reload pass because the
   constraint requires it.  */
static HARD_REG_SET implicit_reg_pending_clobbers;
static HARD_REG_SET implicit_reg_pending_uses;

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
static bitmap_head *true_dependency_cache = NULL;
static bitmap_head *output_dependency_cache = NULL;
static bitmap_head *anti_dependency_cache = NULL;
static bitmap_head *spec_dependency_cache = NULL;
static int cache_size;

static int deps_may_trap_p (const_rtx);
static void add_dependence_list (rtx, rtx, int, enum reg_note);
static void add_dependence_list_and_free (struct deps *, rtx,
					  rtx *, int, enum reg_note);
static void delete_all_dependences (rtx);
static void fixup_sched_groups (rtx);

static void flush_pending_lists (struct deps *, rtx, int, int);
static void sched_analyze_1 (struct deps *, rtx, rtx);
static void sched_analyze_2 (struct deps *, rtx, rtx);
static void sched_analyze_insn (struct deps *, rtx, rtx);

static bool sched_has_condition_p (const_rtx);
static int conditions_mutex_p (const_rtx, const_rtx, bool, bool);

static enum DEPS_ADJUST_RESULT maybe_add_or_update_dep_1 (dep_t, bool,
							  rtx, rtx);
static enum DEPS_ADJUST_RESULT add_or_update_dep_1 (dep_t, bool, rtx, rtx);

#ifdef ENABLE_CHECKING
static void check_dep (dep_t, bool);
#endif

/* Return nonzero if a load of the memory reference MEM can cause a trap.  */

static int
deps_may_trap_p (const_rtx mem)
{
  const_rtx addr = XEXP (mem, 0);

  if (REG_P (addr) && REGNO (addr) >= FIRST_PSEUDO_REGISTER)
    {
      const_rtx t = get_reg_known_value (REGNO (addr));
      if (t)
	addr = t;
    }
  return rtx_addr_can_trap_p (addr);
}


/* Find the condition under which INSN is executed.  If REV is not NULL,
   it is set to TRUE when the returned comparison should be reversed
   to get the actual condition.  */
static rtx
sched_get_condition_with_rev (const_rtx insn, bool *rev)
{
  rtx pat = PATTERN (insn);
  rtx src;

  if (pat == 0)
    return 0;

  if (rev)
    *rev = false;

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

      if (rev)
	*rev = true;
      return cond;
    }

  return 0;
}

/* True when we can find a condition under which INSN is executed.  */
static bool
sched_has_condition_p (const_rtx insn)
{
  return !! sched_get_condition_with_rev (insn, NULL);
}



/* Return nonzero if conditions COND1 and COND2 can never be both true.  */
static int
conditions_mutex_p (const_rtx cond1, const_rtx cond2, bool rev1, bool rev2)
{
  if (COMPARISON_P (cond1)
      && COMPARISON_P (cond2)
      && GET_CODE (cond1) ==
	  (rev1==rev2
	  ? reversed_comparison_code (cond2, NULL)
	  : GET_CODE (cond2))
      && XEXP (cond1, 0) == XEXP (cond2, 0)
      && XEXP (cond1, 1) == XEXP (cond2, 1))
    return 1;
  return 0;
}

/* Return true if insn1 and insn2 can never depend on one another because
   the conditions under which they are executed are mutually exclusive.  */
bool
sched_insns_conditions_mutex_p (const_rtx insn1, const_rtx insn2)
{
  rtx cond1, cond2;
  bool rev1 = false, rev2 = false;

  /* df doesn't handle conditional lifetimes entirely correctly;
     calls mess up the conditional lifetimes.  */
  if (!CALL_P (insn1) && !CALL_P (insn2))
    {
      cond1 = sched_get_condition_with_rev (insn1, &rev1);
      cond2 = sched_get_condition_with_rev (insn2, &rev2);
      if (cond1 && cond2
	  && conditions_mutex_p (cond1, cond2, rev1, rev2)
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


/* Return true if INSN can potentially be speculated with type DS.  */
bool
sched_insn_is_legitimate_for_speculation_p (const_rtx insn, ds_t ds)
{
  if (HAS_INTERNAL_DEP (insn))
    return false;

  if (!NONJUMP_INSN_P (insn))
    return false;

  if (SCHED_GROUP_P (insn))
    return false;

  if (IS_SPECULATION_CHECK_P (CONST_CAST_RTX (insn)))
    return false;

  if (side_effects_p (PATTERN (insn)))
    return false;

  if (ds & BE_IN_SPEC)
    /* The following instructions, which depend on a speculatively scheduled
       instruction, cannot be speculatively scheduled along.  */
    {
      if (may_trap_p (PATTERN (insn)))
	/* If instruction might trap, it cannot be speculatively scheduled.
	   For control speculation it's obvious why and for data speculation
	   it's because the insn might get wrong input if speculation
	   wasn't successful.  */
	return false;

      if ((ds & BE_IN_DATA)
	  && sched_has_condition_p (insn))
	/* If this is a predicated instruction, then it cannot be
	   speculatively scheduled.  See PR35659.  */
	return false;
    }

  return true;
}

/* Initialize LIST_PTR to point to one of the lists present in TYPES_PTR,
   initialize RESOLVED_P_PTR with true if that list consists of resolved deps,
   and remove the type of returned [through LIST_PTR] list from TYPES_PTR.
   This function is used to switch sd_iterator to the next list.
   !!! For internal use only.  Might consider moving it to sched-int.h.  */
void
sd_next_list (const_rtx insn, sd_list_types_def *types_ptr,
	      deps_list_t *list_ptr, bool *resolved_p_ptr)
{
  sd_list_types_def types = *types_ptr;

  if (types & SD_LIST_HARD_BACK)
    {
      *list_ptr = INSN_HARD_BACK_DEPS (insn);
      *resolved_p_ptr = false;
      *types_ptr = types & ~SD_LIST_HARD_BACK;
    }
  else if (types & SD_LIST_SPEC_BACK)
    {
      *list_ptr = INSN_SPEC_BACK_DEPS (insn);
      *resolved_p_ptr = false;
      *types_ptr = types & ~SD_LIST_SPEC_BACK;
    }
  else if (types & SD_LIST_FORW)
    {
      *list_ptr = INSN_FORW_DEPS (insn);
      *resolved_p_ptr = false;
      *types_ptr = types & ~SD_LIST_FORW;
    }
  else if (types & SD_LIST_RES_BACK)
    {
      *list_ptr = INSN_RESOLVED_BACK_DEPS (insn);
      *resolved_p_ptr = true;
      *types_ptr = types & ~SD_LIST_RES_BACK;
    }
  else if (types & SD_LIST_RES_FORW)
    {
      *list_ptr = INSN_RESOLVED_FORW_DEPS (insn);
      *resolved_p_ptr = true;
      *types_ptr = types & ~SD_LIST_RES_FORW;
    }
  else
    {
      *list_ptr = NULL;
      *resolved_p_ptr = false;
      *types_ptr = SD_LIST_NONE;
    }
}

/* Return the summary size of INSN's lists defined by LIST_TYPES.  */
int
sd_lists_size (const_rtx insn, sd_list_types_def list_types)
{
  int size = 0;

  while (list_types != SD_LIST_NONE)
    {
      deps_list_t list;
      bool resolved_p;

      sd_next_list (insn, &list_types, &list, &resolved_p);
      if (list)
	size += DEPS_LIST_N_LINKS (list);
    }

  return size;
}

/* Return true if INSN's lists defined by LIST_TYPES are all empty.  */

bool
sd_lists_empty_p (const_rtx insn, sd_list_types_def list_types)
{
  while (list_types != SD_LIST_NONE)
    {
      deps_list_t list;
      bool resolved_p;

      sd_next_list (insn, &list_types, &list, &resolved_p);
      if (!deps_list_empty_p (list))
	return false;
    }

  return true;
}

/* Initialize data for INSN.  */
void
sd_init_insn (rtx insn)
{
  INSN_HARD_BACK_DEPS (insn) = create_deps_list ();
  INSN_SPEC_BACK_DEPS (insn) = create_deps_list ();
  INSN_RESOLVED_BACK_DEPS (insn) = create_deps_list ();
  INSN_FORW_DEPS (insn) = create_deps_list ();
  INSN_RESOLVED_FORW_DEPS (insn) = create_deps_list ();

  if (DEBUG_INSN_P (insn))
    DEBUG_INSN_SCHED_P (insn) = TRUE;

  /* ??? It would be nice to allocate dependency caches here.  */
}

/* Free data for INSN.  */
void
sd_finish_insn (rtx insn)
{
  /* ??? It would be nice to deallocate dependency caches here.  */

  if (DEBUG_INSN_P (insn))
    {
      gcc_assert (DEBUG_INSN_SCHED_P (insn));
      DEBUG_INSN_SCHED_P (insn) = FALSE;
    }

  free_deps_list (INSN_HARD_BACK_DEPS (insn));
  INSN_HARD_BACK_DEPS (insn) = NULL;

  free_deps_list (INSN_SPEC_BACK_DEPS (insn));
  INSN_SPEC_BACK_DEPS (insn) = NULL;

  free_deps_list (INSN_RESOLVED_BACK_DEPS (insn));
  INSN_RESOLVED_BACK_DEPS (insn) = NULL;

  free_deps_list (INSN_FORW_DEPS (insn));
  INSN_FORW_DEPS (insn) = NULL;

  free_deps_list (INSN_RESOLVED_FORW_DEPS (insn));
  INSN_RESOLVED_FORW_DEPS (insn) = NULL;
}

/* Find a dependency between producer PRO and consumer CON.
   Search through resolved dependency lists if RESOLVED_P is true.
   If no such dependency is found return NULL,
   otherwise return the dependency and initialize SD_IT_PTR [if it is nonnull]
   with an iterator pointing to it.  */
static dep_t
sd_find_dep_between_no_cache (rtx pro, rtx con, bool resolved_p,
			      sd_iterator_def *sd_it_ptr)
{
  sd_list_types_def pro_list_type;
  sd_list_types_def con_list_type;
  sd_iterator_def sd_it;
  dep_t dep;
  bool found_p = false;

  if (resolved_p)
    {
      pro_list_type = SD_LIST_RES_FORW;
      con_list_type = SD_LIST_RES_BACK;
    }
  else
    {
      pro_list_type = SD_LIST_FORW;
      con_list_type = SD_LIST_BACK;
    }

  /* Walk through either back list of INSN or forw list of ELEM
     depending on which one is shorter.  */
  if (sd_lists_size (con, con_list_type) < sd_lists_size (pro, pro_list_type))
    {
      /* Find the dep_link with producer PRO in consumer's back_deps.  */
      FOR_EACH_DEP (con, con_list_type, sd_it, dep)
	if (DEP_PRO (dep) == pro)
	  {
	    found_p = true;
	    break;
	  }
    }
  else
    {
      /* Find the dep_link with consumer CON in producer's forw_deps.  */
      FOR_EACH_DEP (pro, pro_list_type, sd_it, dep)
	if (DEP_CON (dep) == con)
	  {
	    found_p = true;
	    break;
	  }
    }

  if (found_p)
    {
      if (sd_it_ptr != NULL)
	*sd_it_ptr = sd_it;

      return dep;
    }

  return NULL;
}

/* Find a dependency between producer PRO and consumer CON.
   Use dependency [if available] to check if dependency is present at all.
   Search through resolved dependency lists if RESOLVED_P is true.
   If the dependency or NULL if none found.  */
dep_t
sd_find_dep_between (rtx pro, rtx con, bool resolved_p)
{
  if (true_dependency_cache != NULL)
    /* Avoiding the list walk below can cut compile times dramatically
       for some code.  */
    {
      int elem_luid = INSN_LUID (pro);
      int insn_luid = INSN_LUID (con);

      gcc_assert (output_dependency_cache != NULL
		  && anti_dependency_cache != NULL);

      if (!bitmap_bit_p (&true_dependency_cache[insn_luid], elem_luid)
	  && !bitmap_bit_p (&output_dependency_cache[insn_luid], elem_luid)
	  && !bitmap_bit_p (&anti_dependency_cache[insn_luid], elem_luid))
	return NULL;
    }

  return sd_find_dep_between_no_cache (pro, con, resolved_p, NULL);
}

/* Add or update  a dependence described by DEP.
   MEM1 and MEM2, if non-null, correspond to memory locations in case of
   data speculation.

   The function returns a value indicating if an old entry has been changed
   or a new entry has been added to insn's backward deps.

   This function merely checks if producer and consumer is the same insn
   and doesn't create a dep in this case.  Actual manipulation of
   dependence data structures is performed in add_or_update_dep_1.  */
static enum DEPS_ADJUST_RESULT
maybe_add_or_update_dep_1 (dep_t dep, bool resolved_p, rtx mem1, rtx mem2)
{
  rtx elem = DEP_PRO (dep);
  rtx insn = DEP_CON (dep);

  gcc_assert (INSN_P (insn) && INSN_P (elem));

  /* Don't depend an insn on itself.  */
  if (insn == elem)
    {
      if (sched_deps_info->generate_spec_deps)
        /* INSN has an internal dependence, which we can't overcome.  */
        HAS_INTERNAL_DEP (insn) = 1;

      return DEP_NODEP;
    }

  return add_or_update_dep_1 (dep, resolved_p, mem1, mem2);
}

/* Ask dependency caches what needs to be done for dependence DEP.
   Return DEP_CREATED if new dependence should be created and there is no
   need to try to find one searching the dependencies lists.
   Return DEP_PRESENT if there already is a dependence described by DEP and
   hence nothing is to be done.
   Return DEP_CHANGED if there already is a dependence, but it should be
   updated to incorporate additional information from DEP.  */
static enum DEPS_ADJUST_RESULT
ask_dependency_caches (dep_t dep)
{
  int elem_luid = INSN_LUID (DEP_PRO (dep));
  int insn_luid = INSN_LUID (DEP_CON (dep));

  gcc_assert (true_dependency_cache != NULL
	      && output_dependency_cache != NULL
	      && anti_dependency_cache != NULL);

  if (!(current_sched_info->flags & USE_DEPS_LIST))
    {
      enum reg_note present_dep_type;

      if (bitmap_bit_p (&true_dependency_cache[insn_luid], elem_luid))
	present_dep_type = REG_DEP_TRUE;
      else if (bitmap_bit_p (&output_dependency_cache[insn_luid], elem_luid))
	present_dep_type = REG_DEP_OUTPUT;
      else if (bitmap_bit_p (&anti_dependency_cache[insn_luid], elem_luid))
	present_dep_type = REG_DEP_ANTI;
      else
	/* There is no existing dep so it should be created.  */
	return DEP_CREATED;

      if ((int) DEP_TYPE (dep) >= (int) present_dep_type)
	/* DEP does not add anything to the existing dependence.  */
	return DEP_PRESENT;
    }
  else
    {
      ds_t present_dep_types = 0;

      if (bitmap_bit_p (&true_dependency_cache[insn_luid], elem_luid))
	present_dep_types |= DEP_TRUE;
      if (bitmap_bit_p (&output_dependency_cache[insn_luid], elem_luid))
	present_dep_types |= DEP_OUTPUT;
      if (bitmap_bit_p (&anti_dependency_cache[insn_luid], elem_luid))
	present_dep_types |= DEP_ANTI;

      if (present_dep_types == 0)
	/* There is no existing dep so it should be created.  */
	return DEP_CREATED;

      if (!(current_sched_info->flags & DO_SPECULATION)
	  || !bitmap_bit_p (&spec_dependency_cache[insn_luid], elem_luid))
	{
	  if ((present_dep_types | (DEP_STATUS (dep) & DEP_TYPES))
	      == present_dep_types)
	    /* DEP does not add anything to the existing dependence.  */
	    return DEP_PRESENT;
	}
      else
	{
	  /* Only true dependencies can be data speculative and
	     only anti dependencies can be control speculative.  */
	  gcc_assert ((present_dep_types & (DEP_TRUE | DEP_ANTI))
		      == present_dep_types);

	  /* if (DEP is SPECULATIVE) then
	     ..we should update DEP_STATUS
	     else
	     ..we should reset existing dep to non-speculative.  */
	}
    }

  return DEP_CHANGED;
}

/* Set dependency caches according to DEP.  */
static void
set_dependency_caches (dep_t dep)
{
  int elem_luid = INSN_LUID (DEP_PRO (dep));
  int insn_luid = INSN_LUID (DEP_CON (dep));

  if (!(current_sched_info->flags & USE_DEPS_LIST))
    {
      switch (DEP_TYPE (dep))
	{
	case REG_DEP_TRUE:
	  bitmap_set_bit (&true_dependency_cache[insn_luid], elem_luid);
	  break;

	case REG_DEP_OUTPUT:
	  bitmap_set_bit (&output_dependency_cache[insn_luid], elem_luid);
	  break;

	case REG_DEP_ANTI:
	  bitmap_set_bit (&anti_dependency_cache[insn_luid], elem_luid);
	  break;

	default:
	  gcc_unreachable ();
	}
    }
  else
    {
      ds_t ds = DEP_STATUS (dep);

      if (ds & DEP_TRUE)
	bitmap_set_bit (&true_dependency_cache[insn_luid], elem_luid);
      if (ds & DEP_OUTPUT)
	bitmap_set_bit (&output_dependency_cache[insn_luid], elem_luid);
      if (ds & DEP_ANTI)
	bitmap_set_bit (&anti_dependency_cache[insn_luid], elem_luid);

      if (ds & SPECULATIVE)
	{
	  gcc_assert (current_sched_info->flags & DO_SPECULATION);
	  bitmap_set_bit (&spec_dependency_cache[insn_luid], elem_luid);
	}
    }
}

/* Type of dependence DEP have changed from OLD_TYPE.  Update dependency
   caches accordingly.  */
static void
update_dependency_caches (dep_t dep, enum reg_note old_type)
{
  int elem_luid = INSN_LUID (DEP_PRO (dep));
  int insn_luid = INSN_LUID (DEP_CON (dep));

  /* Clear corresponding cache entry because type of the link
     may have changed.  Keep them if we use_deps_list.  */
  if (!(current_sched_info->flags & USE_DEPS_LIST))
    {
      switch (old_type)
	{
	case REG_DEP_OUTPUT:
	  bitmap_clear_bit (&output_dependency_cache[insn_luid], elem_luid);
	  break;

	case REG_DEP_ANTI:
	  bitmap_clear_bit (&anti_dependency_cache[insn_luid], elem_luid);
	  break;

	default:
	  gcc_unreachable ();
	}
    }

  set_dependency_caches (dep);
}

/* Convert a dependence pointed to by SD_IT to be non-speculative.  */
static void
change_spec_dep_to_hard (sd_iterator_def sd_it)
{
  dep_node_t node = DEP_LINK_NODE (*sd_it.linkp);
  dep_link_t link = DEP_NODE_BACK (node);
  dep_t dep = DEP_NODE_DEP (node);
  rtx elem = DEP_PRO (dep);
  rtx insn = DEP_CON (dep);

  move_dep_link (link, INSN_SPEC_BACK_DEPS (insn), INSN_HARD_BACK_DEPS (insn));

  DEP_STATUS (dep) &= ~SPECULATIVE;

  if (true_dependency_cache != NULL)
    /* Clear the cache entry.  */
    bitmap_clear_bit (&spec_dependency_cache[INSN_LUID (insn)],
		      INSN_LUID (elem));
}

/* Update DEP to incorporate information from NEW_DEP.
   SD_IT points to DEP in case it should be moved to another list.
   MEM1 and MEM2, if nonnull, correspond to memory locations in case if
   data-speculative dependence should be updated.  */
static enum DEPS_ADJUST_RESULT
update_dep (dep_t dep, dep_t new_dep,
	    sd_iterator_def sd_it ATTRIBUTE_UNUSED,
	    rtx mem1 ATTRIBUTE_UNUSED,
	    rtx mem2 ATTRIBUTE_UNUSED)
{
  enum DEPS_ADJUST_RESULT res = DEP_PRESENT;
  enum reg_note old_type = DEP_TYPE (dep);

  /* If this is a more restrictive type of dependence than the
     existing one, then change the existing dependence to this
     type.  */
  if ((int) DEP_TYPE (new_dep) < (int) old_type)
    {
      DEP_TYPE (dep) = DEP_TYPE (new_dep);
      res = DEP_CHANGED;
    }

  if (current_sched_info->flags & USE_DEPS_LIST)
    /* Update DEP_STATUS.  */
    {
      ds_t dep_status = DEP_STATUS (dep);
      ds_t ds = DEP_STATUS (new_dep);
      ds_t new_status = ds | dep_status;

      if (new_status & SPECULATIVE)
	/* Either existing dep or a dep we're adding or both are
	   speculative.  */
	{
	  if (!(ds & SPECULATIVE)
	      || !(dep_status & SPECULATIVE))
	    /* The new dep can't be speculative.  */
	    {
	      new_status &= ~SPECULATIVE;

	      if (dep_status & SPECULATIVE)
		/* The old dep was speculative, but now it
		   isn't.  */
		change_spec_dep_to_hard (sd_it);
	    }
	  else
	    {
	      /* Both are speculative.  Merge probabilities.  */
	      if (mem1 != NULL)
		{
		  dw_t dw;

		  dw = estimate_dep_weak (mem1, mem2);
		  ds = set_dep_weak (ds, BEGIN_DATA, dw);
		}

	      new_status = ds_merge (dep_status, ds);
	    }
	}

      ds = new_status;

      if (dep_status != ds)
	{
	  DEP_STATUS (dep) = ds;
	  res = DEP_CHANGED;
	}
    }

  if (true_dependency_cache != NULL
      && res == DEP_CHANGED)
    update_dependency_caches (dep, old_type);

  return res;
}

/* Add or update  a dependence described by DEP.
   MEM1 and MEM2, if non-null, correspond to memory locations in case of
   data speculation.

   The function returns a value indicating if an old entry has been changed
   or a new entry has been added to insn's backward deps or nothing has
   been updated at all.  */
static enum DEPS_ADJUST_RESULT
add_or_update_dep_1 (dep_t new_dep, bool resolved_p,
		     rtx mem1 ATTRIBUTE_UNUSED, rtx mem2 ATTRIBUTE_UNUSED)
{
  bool maybe_present_p = true;
  bool present_p = false;

  gcc_assert (INSN_P (DEP_PRO (new_dep)) && INSN_P (DEP_CON (new_dep))
	      && DEP_PRO (new_dep) != DEP_CON (new_dep));

#ifdef ENABLE_CHECKING
  check_dep (new_dep, mem1 != NULL);
#endif

  if (true_dependency_cache != NULL)
    {
      switch (ask_dependency_caches (new_dep))
	{
	case DEP_PRESENT:
	  return DEP_PRESENT;

	case DEP_CHANGED:
	  maybe_present_p = true;
	  present_p = true;
	  break;

	case DEP_CREATED:
	  maybe_present_p = false;
	  present_p = false;
	  break;

	default:
	  gcc_unreachable ();
	  break;
	}
    }

  /* Check that we don't already have this dependence.  */
  if (maybe_present_p)
    {
      dep_t present_dep;
      sd_iterator_def sd_it;

      gcc_assert (true_dependency_cache == NULL || present_p);

      present_dep = sd_find_dep_between_no_cache (DEP_PRO (new_dep),
						  DEP_CON (new_dep),
						  resolved_p, &sd_it);

      if (present_dep != NULL)
	/* We found an existing dependency between ELEM and INSN.  */
	return update_dep (present_dep, new_dep, sd_it, mem1, mem2);
      else
	/* We didn't find a dep, it shouldn't present in the cache.  */
	gcc_assert (!present_p);
    }

  /* Might want to check one level of transitivity to save conses.
     This check should be done in maybe_add_or_update_dep_1.
     Since we made it to add_or_update_dep_1, we must create
     (or update) a link.  */

  if (mem1 != NULL_RTX)
    {
      gcc_assert (sched_deps_info->generate_spec_deps);
      DEP_STATUS (new_dep) = set_dep_weak (DEP_STATUS (new_dep), BEGIN_DATA,
					   estimate_dep_weak (mem1, mem2));
    }

  sd_add_dep (new_dep, resolved_p);

  return DEP_CREATED;
}

/* Initialize BACK_LIST_PTR with consumer's backward list and
   FORW_LIST_PTR with producer's forward list.  If RESOLVED_P is true
   initialize with lists that hold resolved deps.  */
static void
get_back_and_forw_lists (dep_t dep, bool resolved_p,
			 deps_list_t *back_list_ptr,
			 deps_list_t *forw_list_ptr)
{
  rtx con = DEP_CON (dep);

  if (!resolved_p)
    {
      if ((current_sched_info->flags & DO_SPECULATION)
	  && (DEP_STATUS (dep) & SPECULATIVE))
	*back_list_ptr = INSN_SPEC_BACK_DEPS (con);
      else
	*back_list_ptr = INSN_HARD_BACK_DEPS (con);

      *forw_list_ptr = INSN_FORW_DEPS (DEP_PRO (dep));
    }
  else
    {
      *back_list_ptr = INSN_RESOLVED_BACK_DEPS (con);
      *forw_list_ptr = INSN_RESOLVED_FORW_DEPS (DEP_PRO (dep));
    }
}

/* Add dependence described by DEP.
   If RESOLVED_P is true treat the dependence as a resolved one.  */
void
sd_add_dep (dep_t dep, bool resolved_p)
{
  dep_node_t n = create_dep_node ();
  deps_list_t con_back_deps;
  deps_list_t pro_forw_deps;
  rtx elem = DEP_PRO (dep);
  rtx insn = DEP_CON (dep);

  gcc_assert (INSN_P (insn) && INSN_P (elem) && insn != elem);

  if ((current_sched_info->flags & DO_SPECULATION)
      && !sched_insn_is_legitimate_for_speculation_p (insn, DEP_STATUS (dep)))
    DEP_STATUS (dep) &= ~SPECULATIVE;

  copy_dep (DEP_NODE_DEP (n), dep);

  get_back_and_forw_lists (dep, resolved_p, &con_back_deps, &pro_forw_deps);

  add_to_deps_list (DEP_NODE_BACK (n), con_back_deps);

#ifdef ENABLE_CHECKING
  check_dep (dep, false);
#endif

  add_to_deps_list (DEP_NODE_FORW (n), pro_forw_deps);

  /* If we are adding a dependency to INSN's LOG_LINKs, then note that
     in the bitmap caches of dependency information.  */
  if (true_dependency_cache != NULL)
    set_dependency_caches (dep);
}

/* Add or update backward dependence between INSN and ELEM
   with given type DEP_TYPE and dep_status DS.
   This function is a convenience wrapper.  */
enum DEPS_ADJUST_RESULT
sd_add_or_update_dep (dep_t dep, bool resolved_p)
{
  return add_or_update_dep_1 (dep, resolved_p, NULL_RTX, NULL_RTX);
}

/* Resolved dependence pointed to by SD_IT.
   SD_IT will advance to the next element.  */
void
sd_resolve_dep (sd_iterator_def sd_it)
{
  dep_node_t node = DEP_LINK_NODE (*sd_it.linkp);
  dep_t dep = DEP_NODE_DEP (node);
  rtx pro = DEP_PRO (dep);
  rtx con = DEP_CON (dep);

  if ((current_sched_info->flags & DO_SPECULATION)
      && (DEP_STATUS (dep) & SPECULATIVE))
    move_dep_link (DEP_NODE_BACK (node), INSN_SPEC_BACK_DEPS (con),
		   INSN_RESOLVED_BACK_DEPS (con));
  else
    move_dep_link (DEP_NODE_BACK (node), INSN_HARD_BACK_DEPS (con),
		   INSN_RESOLVED_BACK_DEPS (con));

  move_dep_link (DEP_NODE_FORW (node), INSN_FORW_DEPS (pro),
		 INSN_RESOLVED_FORW_DEPS (pro));
}

/* Make TO depend on all the FROM's producers.
   If RESOLVED_P is true add dependencies to the resolved lists.  */
void
sd_copy_back_deps (rtx to, rtx from, bool resolved_p)
{
  sd_list_types_def list_type;
  sd_iterator_def sd_it;
  dep_t dep;

  list_type = resolved_p ? SD_LIST_RES_BACK : SD_LIST_BACK;

  FOR_EACH_DEP (from, list_type, sd_it, dep)
    {
      dep_def _new_dep, *new_dep = &_new_dep;

      copy_dep (new_dep, dep);
      DEP_CON (new_dep) = to;
      sd_add_dep (new_dep, resolved_p);
    }
}

/* Remove a dependency referred to by SD_IT.
   SD_IT will point to the next dependence after removal.  */
void
sd_delete_dep (sd_iterator_def sd_it)
{
  dep_node_t n = DEP_LINK_NODE (*sd_it.linkp);
  dep_t dep = DEP_NODE_DEP (n);
  rtx pro = DEP_PRO (dep);
  rtx con = DEP_CON (dep);
  deps_list_t con_back_deps;
  deps_list_t pro_forw_deps;

  if (true_dependency_cache != NULL)
    {
      int elem_luid = INSN_LUID (pro);
      int insn_luid = INSN_LUID (con);

      bitmap_clear_bit (&true_dependency_cache[insn_luid], elem_luid);
      bitmap_clear_bit (&anti_dependency_cache[insn_luid], elem_luid);
      bitmap_clear_bit (&output_dependency_cache[insn_luid], elem_luid);

      if (current_sched_info->flags & DO_SPECULATION)
	bitmap_clear_bit (&spec_dependency_cache[insn_luid], elem_luid);
    }

  get_back_and_forw_lists (dep, sd_it.resolved_p,
			   &con_back_deps, &pro_forw_deps);

  remove_from_deps_list (DEP_NODE_BACK (n), con_back_deps);
  remove_from_deps_list (DEP_NODE_FORW (n), pro_forw_deps);

  delete_dep_node (n);
}

/* Dump size of the lists.  */
#define DUMP_LISTS_SIZE (2)

/* Dump dependencies of the lists.  */
#define DUMP_LISTS_DEPS (4)

/* Dump all information about the lists.  */
#define DUMP_LISTS_ALL (DUMP_LISTS_SIZE | DUMP_LISTS_DEPS)

/* Dump deps_lists of INSN specified by TYPES to DUMP.
   FLAGS is a bit mask specifying what information about the lists needs
   to be printed.
   If FLAGS has the very first bit set, then dump all information about
   the lists and propagate this bit into the callee dump functions.  */
static void
dump_lists (FILE *dump, rtx insn, sd_list_types_def types, int flags)
{
  sd_iterator_def sd_it;
  dep_t dep;
  int all;

  all = (flags & 1);

  if (all)
    flags |= DUMP_LISTS_ALL;

  fprintf (dump, "[");

  if (flags & DUMP_LISTS_SIZE)
    fprintf (dump, "%d; ", sd_lists_size (insn, types));

  if (flags & DUMP_LISTS_DEPS)
    {
      FOR_EACH_DEP (insn, types, sd_it, dep)
	{
	  dump_dep (dump, dep, dump_dep_flags | all);
	  fprintf (dump, " ");
	}
    }
}

/* Dump all information about deps_lists of INSN specified by TYPES
   to STDERR.  */
void
sd_debug_lists (rtx insn, sd_list_types_def types)
{
  dump_lists (stderr, insn, types, 1);
  fprintf (stderr, "\n");
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

/* Similar, but free *LISTP at the same time, when the context
   is not readonly.  */

static void
add_dependence_list_and_free (struct deps *deps, rtx insn, rtx *listp,
                              int uncond, enum reg_note dep_type)
{
  rtx list, next;

  if (deps->readonly)
    {
      add_dependence_list (insn, *listp, uncond, dep_type);
      return;
    }

  for (list = *listp, *listp = NULL; list ; list = next)
    {
      next = XEXP (list, 1);
      if (uncond || ! sched_insns_conditions_mutex_p (insn, XEXP (list, 0)))
	add_dependence (insn, XEXP (list, 0), dep_type);
      free_INSN_LIST_node (list);
    }
}

/* Remove all occurences of INSN from LIST.  Return the number of
   occurences removed.  */

static int
remove_from_dependence_list (rtx insn, rtx* listp)
{
  int removed = 0;

  while (*listp)
    {
      if (XEXP (*listp, 0) == insn)
        {
          remove_free_INSN_LIST_node (listp);
          removed++;
          continue;
        }

      listp = &XEXP (*listp, 1);
    }

  return removed;
}

/* Same as above, but process two lists at once.  */
static int
remove_from_both_dependence_lists (rtx insn, rtx *listp, rtx *exprp)
{
  int removed = 0;

  while (*listp)
    {
      if (XEXP (*listp, 0) == insn)
        {
          remove_free_INSN_LIST_node (listp);
          remove_free_EXPR_LIST_node (exprp);
          removed++;
          continue;
        }

      listp = &XEXP (*listp, 1);
      exprp = &XEXP (*exprp, 1);
    }

  return removed;
}

/* Clear all dependencies for an insn.  */
static void
delete_all_dependences (rtx insn)
{
  sd_iterator_def sd_it;
  dep_t dep;

  /* The below cycle can be optimized to clear the caches and back_deps
     in one call but that would provoke duplication of code from
     delete_dep ().  */

  for (sd_it = sd_iterator_start (insn, SD_LIST_BACK);
       sd_iterator_cond (&sd_it, &dep);)
    sd_delete_dep (sd_it);
}

/* All insns in a scheduling group except the first should only have
   dependencies on the previous insn in the group.  So we find the
   first instruction in the scheduling group by walking the dependence
   chains backwards. Then we add the dependencies for the group to
   the previous nonnote insn.  */

static void
fixup_sched_groups (rtx insn)
{
  sd_iterator_def sd_it;
  dep_t dep;
  rtx prev_nonnote;

  FOR_EACH_DEP (insn, SD_LIST_BACK, sd_it, dep)
    {
      rtx i = insn;
      rtx pro = DEP_PRO (dep);

      do
	{
	  i = prev_nonnote_insn (i);

	  if (pro == i)
	    goto next_link;
	} while (SCHED_GROUP_P (i) || DEBUG_INSN_P (i));

      if (! sched_insns_conditions_mutex_p (i, pro))
	add_dependence (i, pro, DEP_TYPE (dep));
    next_link:;
    }

  delete_all_dependences (insn);

  prev_nonnote = prev_nonnote_insn (insn);
  while (DEBUG_INSN_P (prev_nonnote))
    prev_nonnote = prev_nonnote_insn (prev_nonnote);
  if (BLOCK_FOR_INSN (insn) == BLOCK_FOR_INSN (prev_nonnote)
      && ! sched_insns_conditions_mutex_p (insn, prev_nonnote))
    add_dependence (insn, prev_nonnote, REG_DEP_ANTI);
}

/* Process an insn's memory dependencies.  There are four kinds of
   dependencies:

   (0) read dependence: read follows read
   (1) true dependence: read follows write
   (2) output dependence: write follows write
   (3) anti dependence: write follows read

   We are careful to build only dependencies which actually exist, and
   use transitivity to avoid building too many links.  */

/* Add an INSN and MEM reference pair to a pending INSN_LIST and MEM_LIST.
   The MEM is a memory reference contained within INSN, which we are saving
   so that we can do memory aliasing on it.  */

static void
add_insn_mem_dependence (struct deps *deps, bool read_p,
			 rtx insn, rtx mem)
{
  rtx *insn_list;
  rtx *mem_list;
  rtx link;

  gcc_assert (!deps->readonly);
  if (read_p)
    {
      insn_list = &deps->pending_read_insns;
      mem_list = &deps->pending_read_mems;
      if (!DEBUG_INSN_P (insn))
	deps->pending_read_list_length++;
    }
  else
    {
      insn_list = &deps->pending_write_insns;
      mem_list = &deps->pending_write_mems;
      deps->pending_write_list_length++;
    }

  link = alloc_INSN_LIST (insn, *insn_list);
  *insn_list = link;

  if (sched_deps_info->use_cselib)
    {
      mem = shallow_copy_rtx (mem);
      XEXP (mem, 0) = cselib_subst_to_values (XEXP (mem, 0));
    }
  link = alloc_EXPR_LIST (VOIDmode, canon_rtx (mem), *mem_list);
  *mem_list = link;
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
      add_dependence_list_and_free (deps, insn, &deps->pending_read_insns,
                                    1, REG_DEP_ANTI);
      if (!deps->readonly)
        {
          free_EXPR_LIST_list (&deps->pending_read_mems);
          deps->pending_read_list_length = 0;
        }
    }

  add_dependence_list_and_free (deps, insn, &deps->pending_write_insns, 1,
				for_read ? REG_DEP_ANTI : REG_DEP_OUTPUT);

  add_dependence_list_and_free (deps, insn,
                                &deps->last_pending_memory_flush, 1,
                                for_read ? REG_DEP_ANTI : REG_DEP_OUTPUT);
  if (!deps->readonly)
    {
      free_EXPR_LIST_list (&deps->pending_write_mems);
      deps->pending_write_list_length = 0;

      deps->last_pending_memory_flush = alloc_INSN_LIST (insn, NULL_RTX);
      deps->pending_flush_length = 1;
    }
}

/* Instruction which dependencies we are analyzing.  */
static rtx cur_insn = NULL_RTX;

/* Implement hooks for haifa scheduler.  */

static void
haifa_start_insn (rtx insn)
{
  gcc_assert (insn && !cur_insn);

  cur_insn = insn;
}

static void
haifa_finish_insn (void)
{
  cur_insn = NULL;
}

void
haifa_note_reg_set (int regno)
{
  SET_REGNO_REG_SET (reg_pending_sets, regno);
}

void
haifa_note_reg_clobber (int regno)
{
  SET_REGNO_REG_SET (reg_pending_clobbers, regno);
}

void
haifa_note_reg_use (int regno)
{
  SET_REGNO_REG_SET (reg_pending_uses, regno);
}

static void
haifa_note_mem_dep (rtx mem, rtx pending_mem, rtx pending_insn, ds_t ds)
{
  if (!(ds & SPECULATIVE))
    {
      mem = NULL_RTX;
      pending_mem = NULL_RTX;
    }
  else
    gcc_assert (ds & BEGIN_DATA);

  {
    dep_def _dep, *dep = &_dep;

    init_dep_1 (dep, pending_insn, cur_insn, ds_to_dt (ds),
                current_sched_info->flags & USE_DEPS_LIST ? ds : -1);
    maybe_add_or_update_dep_1 (dep, false, pending_mem, mem);
  }

}

static void
haifa_note_dep (rtx elem, ds_t ds)
{
  dep_def _dep;
  dep_t dep = &_dep;

  init_dep (dep, elem, cur_insn, ds_to_dt (ds));
  maybe_add_or_update_dep_1 (dep, false, NULL_RTX, NULL_RTX);
}

static void
note_reg_use (int r)
{
  if (sched_deps_info->note_reg_use)
    sched_deps_info->note_reg_use (r);
}

static void
note_reg_set (int r)
{
  if (sched_deps_info->note_reg_set)
    sched_deps_info->note_reg_set (r);
}

static void
note_reg_clobber (int r)
{
  if (sched_deps_info->note_reg_clobber)
    sched_deps_info->note_reg_clobber (r);
}

static void
note_mem_dep (rtx m1, rtx m2, rtx e, ds_t ds)
{
  if (sched_deps_info->note_mem_dep)
    sched_deps_info->note_mem_dep (m1, m2, e, ds);
}

static void
note_dep (rtx e, ds_t ds)
{
  if (sched_deps_info->note_dep)
    sched_deps_info->note_dep (e, ds);
}

/* Return corresponding to DS reg_note.  */
enum reg_note
ds_to_dt (ds_t ds)
{
  if (ds & DEP_TRUE)
    return REG_DEP_TRUE;
  else if (ds & DEP_OUTPUT)
    return REG_DEP_OUTPUT;
  else
    {
      gcc_assert (ds & DEP_ANTI);
      return REG_DEP_ANTI;
    }
}



/* Functions for computation of info needed for register pressure
   sensitive insn scheduling.  */


/* Allocate and return reg_use_data structure for REGNO and INSN.  */
static struct reg_use_data *
create_insn_reg_use (int regno, rtx insn)
{
  struct reg_use_data *use;

  use = (struct reg_use_data *) xmalloc (sizeof (struct reg_use_data));
  use->regno = regno;
  use->insn = insn;
  use->next_insn_use = INSN_REG_USE_LIST (insn);
  INSN_REG_USE_LIST (insn) = use;
  return use;
}

/* Allocate and return reg_set_data structure for REGNO and INSN.  */
static struct reg_set_data *
create_insn_reg_set (int regno, rtx insn)
{
  struct reg_set_data *set;

  set = (struct reg_set_data *) xmalloc (sizeof (struct reg_set_data));
  set->regno = regno;
  set->insn = insn;
  set->next_insn_set = INSN_REG_SET_LIST (insn);
  INSN_REG_SET_LIST (insn) = set;
  return set;
}

/* Set up insn register uses for INSN and dependency context DEPS.  */
static void
setup_insn_reg_uses (struct deps *deps, rtx insn)
{
  unsigned i;
  reg_set_iterator rsi;
  rtx list;
  struct reg_use_data *use, *use2, *next;
  struct deps_reg *reg_last;

  EXECUTE_IF_SET_IN_REG_SET (reg_pending_uses, 0, i, rsi)
    {
      if (i < FIRST_PSEUDO_REGISTER
	  && TEST_HARD_REG_BIT (ira_no_alloc_regs, i))
	continue;

      if (find_regno_note (insn, REG_DEAD, i) == NULL_RTX
	  && ! REGNO_REG_SET_P (reg_pending_sets, i)
	  && ! REGNO_REG_SET_P (reg_pending_clobbers, i))
	/* Ignore use which is not dying.  */
	continue;

      use = create_insn_reg_use (i, insn);
      use->next_regno_use = use;
      reg_last = &deps->reg_last[i];

      /* Create the cycle list of uses.  */
      for (list = reg_last->uses; list; list = XEXP (list, 1))
	{
	  use2 = create_insn_reg_use (i, XEXP (list, 0));
	  next = use->next_regno_use;
	  use->next_regno_use = use2;
	  use2->next_regno_use = next;
	}
    }
}

/* Register pressure info for the currently processed insn.  */
static struct reg_pressure_data reg_pressure_info[N_REG_CLASSES];

/* Return TRUE if INSN has the use structure for REGNO.  */
static bool
insn_use_p (rtx insn, int regno)
{
  struct reg_use_data *use;

  for (use = INSN_REG_USE_LIST (insn); use != NULL; use = use->next_insn_use)
    if (use->regno == regno)
      return true;
  return false;
}

/* Update the register pressure info after birth of pseudo register REGNO
   in INSN.  Arguments CLOBBER_P and UNUSED_P say correspondingly that
   the register is in clobber or unused after the insn.  */
static void
mark_insn_pseudo_birth (rtx insn, int regno, bool clobber_p, bool unused_p)
{
  int incr, new_incr;
  enum reg_class cl;

  gcc_assert (regno >= FIRST_PSEUDO_REGISTER);
  cl = sched_regno_cover_class[regno];
  if (cl != NO_REGS)
    {
      incr = ira_reg_class_nregs[cl][PSEUDO_REGNO_MODE (regno)];
      if (clobber_p)
	{
	  new_incr = reg_pressure_info[cl].clobber_increase + incr;
	  reg_pressure_info[cl].clobber_increase = new_incr;
	}
      else if (unused_p)
	{
	  new_incr = reg_pressure_info[cl].unused_set_increase + incr;
	  reg_pressure_info[cl].unused_set_increase = new_incr;
	}
      else
	{
	  new_incr = reg_pressure_info[cl].set_increase + incr;
	  reg_pressure_info[cl].set_increase = new_incr;
	  if (! insn_use_p (insn, regno))
	    reg_pressure_info[cl].change += incr;
	  create_insn_reg_set (regno, insn);
	}
      gcc_assert (new_incr < (1 << INCREASE_BITS));
    }
}

/* Like mark_insn_pseudo_regno_birth except that NREGS saying how many
   hard registers involved in the birth.  */
static void
mark_insn_hard_regno_birth (rtx insn, int regno, int nregs,
			    bool clobber_p, bool unused_p)
{
  enum reg_class cl;
  int new_incr, last = regno + nregs;

  while (regno < last)
    {
      gcc_assert (regno < FIRST_PSEUDO_REGISTER);
      if (! TEST_HARD_REG_BIT (ira_no_alloc_regs, regno))
	{
	  cl = sched_regno_cover_class[regno];
	  if (cl != NO_REGS)
	    {
	      if (clobber_p)
		{
		  new_incr = reg_pressure_info[cl].clobber_increase + 1;
		  reg_pressure_info[cl].clobber_increase = new_incr;
		}
	      else if (unused_p)
		{
		  new_incr = reg_pressure_info[cl].unused_set_increase + 1;
		  reg_pressure_info[cl].unused_set_increase = new_incr;
		}
	      else
		{
		  new_incr = reg_pressure_info[cl].set_increase + 1;
		  reg_pressure_info[cl].set_increase = new_incr;
		  if (! insn_use_p (insn, regno))
		    reg_pressure_info[cl].change += 1;
		  create_insn_reg_set (regno, insn);
		}
	      gcc_assert (new_incr < (1 << INCREASE_BITS));
	    }
	}
      regno++;
    }
}

/* Update the register pressure info after birth of pseudo or hard
   register REG in INSN.  Arguments CLOBBER_P and UNUSED_P say
   correspondingly that the register is in clobber or unused after the
   insn.  */
static void
mark_insn_reg_birth (rtx insn, rtx reg, bool clobber_p, bool unused_p)
{
  int regno;

  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);

  if (! REG_P (reg))
    return;

  regno = REGNO (reg);
  if (regno < FIRST_PSEUDO_REGISTER)
    mark_insn_hard_regno_birth (insn, regno,
				hard_regno_nregs[regno][GET_MODE (reg)],
				clobber_p, unused_p);
  else
    mark_insn_pseudo_birth (insn, regno, clobber_p, unused_p);
}

/* Update the register pressure info after death of pseudo register
   REGNO.  */
static void
mark_pseudo_death (int regno)
{
  int incr;
  enum reg_class cl;

  gcc_assert (regno >= FIRST_PSEUDO_REGISTER);
  cl = sched_regno_cover_class[regno];
  if (cl != NO_REGS)
    {
      incr = ira_reg_class_nregs[cl][PSEUDO_REGNO_MODE (regno)];
      reg_pressure_info[cl].change -= incr;
    }
}

/* Like mark_pseudo_death except that NREGS saying how many hard
   registers involved in the death.  */
static void
mark_hard_regno_death (int regno, int nregs)
{
  enum reg_class cl;
  int last = regno + nregs;

  while (regno < last)
    {
      gcc_assert (regno < FIRST_PSEUDO_REGISTER);
      if (! TEST_HARD_REG_BIT (ira_no_alloc_regs, regno))
	{
	  cl = sched_regno_cover_class[regno];
	  if (cl != NO_REGS)
	    reg_pressure_info[cl].change -= 1;
	}
      regno++;
    }
}

/* Update the register pressure info after death of pseudo or hard
   register REG.  */
static void
mark_reg_death (rtx reg)
{
  int regno;

  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);

  if (! REG_P (reg))
    return;

  regno = REGNO (reg);
  if (regno < FIRST_PSEUDO_REGISTER)
    mark_hard_regno_death (regno, hard_regno_nregs[regno][GET_MODE (reg)]);
  else
    mark_pseudo_death (regno);
}

/* Process SETTER of REG.  DATA is an insn containing the setter.  */
static void
mark_insn_reg_store (rtx reg, const_rtx setter, void *data)
{
  if (setter != NULL_RTX && GET_CODE (setter) != SET)
    return;
  mark_insn_reg_birth
    ((rtx) data, reg, false,
     find_reg_note ((const_rtx) data, REG_UNUSED, reg) != NULL_RTX);
}

/* Like mark_insn_reg_store except notice just CLOBBERs; ignore SETs.  */
static void
mark_insn_reg_clobber (rtx reg, const_rtx setter, void *data)
{
  if (GET_CODE (setter) == CLOBBER)
    mark_insn_reg_birth ((rtx) data, reg, true, false);
}

/* Set up reg pressure info related to INSN.  */
static void
setup_insn_reg_pressure_info (rtx insn)
{
  int i, len;
  enum reg_class cl;
  static struct reg_pressure_data *pressure_info;
  rtx link;

  gcc_assert (sched_pressure_p);

  if (! INSN_P (insn))
    return;

  for (i = 0; i < ira_reg_class_cover_size; i++)
    {
      cl = ira_reg_class_cover[i];
      reg_pressure_info[cl].clobber_increase = 0;
      reg_pressure_info[cl].set_increase = 0;
      reg_pressure_info[cl].unused_set_increase = 0;
      reg_pressure_info[cl].change = 0;
    }

  note_stores (PATTERN (insn), mark_insn_reg_clobber, insn);

  note_stores (PATTERN (insn), mark_insn_reg_store, insn);

#ifdef AUTO_INC_DEC
  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
    if (REG_NOTE_KIND (link) == REG_INC)
      mark_insn_reg_store (XEXP (link, 0), NULL_RTX, insn);
#endif

  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
    if (REG_NOTE_KIND (link) == REG_DEAD)
      mark_reg_death (XEXP (link, 0));

  len = sizeof (struct reg_pressure_data) * ira_reg_class_cover_size;
  pressure_info
    = INSN_REG_PRESSURE (insn) = (struct reg_pressure_data *) xmalloc (len);
  INSN_MAX_REG_PRESSURE (insn) = (int *) xcalloc (ira_reg_class_cover_size
						  * sizeof (int), 1);
  for (i = 0; i < ira_reg_class_cover_size; i++)
    {
      cl = ira_reg_class_cover[i];
      pressure_info[i].clobber_increase
	= reg_pressure_info[cl].clobber_increase;
      pressure_info[i].set_increase = reg_pressure_info[cl].set_increase;
      pressure_info[i].unused_set_increase
	= reg_pressure_info[cl].unused_set_increase;
      pressure_info[i].change = reg_pressure_info[cl].change;
    }
}




/* Internal variable for sched_analyze_[12] () functions.
   If it is nonzero, this means that sched_analyze_[12] looks
   at the most toplevel SET.  */
static bool can_start_lhs_rhs_p;

/* Extend reg info for the deps context DEPS given that
   we have just generated a register numbered REGNO.  */
static void
extend_deps_reg_info (struct deps *deps, int regno)
{
  int max_regno = regno + 1;

  gcc_assert (!reload_completed);

  /* In a readonly context, it would not hurt to extend info,
     but it should not be needed.  */
  if (reload_completed && deps->readonly)
    {
      deps->max_reg = max_regno;
      return;
    }

  if (max_regno > deps->max_reg)
    {
      deps->reg_last = XRESIZEVEC (struct deps_reg, deps->reg_last,
                                   max_regno);
      memset (&deps->reg_last[deps->max_reg],
              0, (max_regno - deps->max_reg)
              * sizeof (struct deps_reg));
      deps->max_reg = max_regno;
    }
}

/* Extends REG_INFO_P if needed.  */
void
maybe_extend_reg_info_p (void)
{
  /* Extend REG_INFO_P, if needed.  */
  if ((unsigned int)max_regno - 1 >= reg_info_p_size)
    {
      size_t new_reg_info_p_size = max_regno + 128;

      gcc_assert (!reload_completed && sel_sched_p ());

      reg_info_p = (struct reg_info_t *) xrecalloc (reg_info_p,
                                                    new_reg_info_p_size,
                                                    reg_info_p_size,
                                                    sizeof (*reg_info_p));
      reg_info_p_size = new_reg_info_p_size;
    }
}

/* Analyze a single reference to register (reg:MODE REGNO) in INSN.
   The type of the reference is specified by REF and can be SET,
   CLOBBER, PRE_DEC, POST_DEC, PRE_INC, POST_INC or USE.  */

static void
sched_analyze_reg (struct deps *deps, int regno, enum machine_mode mode,
		   enum rtx_code ref, rtx insn)
{
  /* We could emit new pseudos in renaming.  Extend the reg structures.  */
  if (!reload_completed && sel_sched_p ()
      && (regno >= max_reg_num () - 1 || regno >= deps->max_reg))
    extend_deps_reg_info (deps, regno);

  maybe_extend_reg_info_p ();

  /* A hard reg in a wide mode may really be multiple registers.
     If so, mark all of them just like the first.  */
  if (regno < FIRST_PSEUDO_REGISTER)
    {
      int i = hard_regno_nregs[regno][mode];
      if (ref == SET)
	{
	  while (--i >= 0)
	    note_reg_set (regno + i);
	}
      else if (ref == USE)
	{
	  while (--i >= 0)
	    note_reg_use (regno + i);
	}
      else
	{
	  while (--i >= 0)
	    note_reg_clobber (regno + i);
	}
    }

  /* ??? Reload sometimes emits USEs and CLOBBERs of pseudos that
     it does not reload.  Ignore these as they have served their
     purpose already.  */
  else if (regno >= deps->max_reg)
    {
      enum rtx_code code = GET_CODE (PATTERN (insn));
      gcc_assert (code == USE || code == CLOBBER);
    }

  else
    {
      if (ref == SET)
	note_reg_set (regno);
      else if (ref == USE)
	note_reg_use (regno);
      else
	note_reg_clobber (regno);

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
	{
	  if (!deps->readonly && ref == USE && !DEBUG_INSN_P (insn))
	    deps->sched_before_next_call
	      = alloc_INSN_LIST (insn, deps->sched_before_next_call);
	  else
	    add_dependence_list (insn, deps->last_function_call, 1,
				 REG_DEP_ANTI);
	}
    }
}

/* Analyze a single SET, CLOBBER, PRE_DEC, POST_DEC, PRE_INC or POST_INC
   rtx, X, creating all dependencies generated by the write to the
   destination of X, and reads of everything mentioned.  */

static void
sched_analyze_1 (struct deps *deps, rtx x, rtx insn)
{
  rtx dest = XEXP (x, 0);
  enum rtx_code code = GET_CODE (x);
  bool cslr_p = can_start_lhs_rhs_p;

  can_start_lhs_rhs_p = false;

  gcc_assert (dest);
  if (dest == 0)
    return;

  if (cslr_p && sched_deps_info->start_lhs)
    sched_deps_info->start_lhs (dest);

  if (GET_CODE (dest) == PARALLEL)
    {
      int i;

      for (i = XVECLEN (dest, 0) - 1; i >= 0; i--)
	if (XEXP (XVECEXP (dest, 0, i), 0) != 0)
	  sched_analyze_1 (deps,
			   gen_rtx_CLOBBER (VOIDmode,
					    XEXP (XVECEXP (dest, 0, i), 0)),
			   insn);

      if (cslr_p && sched_deps_info->finish_lhs)
	sched_deps_info->finish_lhs ();

      if (code == SET)
	{
	  can_start_lhs_rhs_p = cslr_p;

	  sched_analyze_2 (deps, SET_SRC (x), insn);

	  can_start_lhs_rhs_p = false;
	}

      return;
    }

  while (GET_CODE (dest) == STRICT_LOW_PART || GET_CODE (dest) == SUBREG
	 || GET_CODE (dest) == ZERO_EXTRACT)
    {
      if (GET_CODE (dest) == STRICT_LOW_PART
	 || GET_CODE (dest) == ZERO_EXTRACT
	 || df_read_modify_subreg_p (dest))
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
      int regno = REGNO (dest);
      enum machine_mode mode = GET_MODE (dest);

      sched_analyze_reg (deps, regno, mode, code, insn);

#ifdef STACK_REGS
      /* Treat all writes to a stack register as modifying the TOS.  */
      if (regno >= FIRST_STACK_REG && regno <= LAST_STACK_REG)
	{
	  int nregs;

	  /* Avoid analyzing the same register twice.  */
	  if (regno != FIRST_STACK_REG)
	    sched_analyze_reg (deps, FIRST_STACK_REG, mode, code, insn);

	  nregs = hard_regno_nregs[FIRST_STACK_REG][mode];
	  while (--nregs >= 0)
	    SET_HARD_REG_BIT (implicit_reg_pending_uses,
			      FIRST_STACK_REG + nregs);
	}
#endif
    }
  else if (MEM_P (dest))
    {
      /* Writing memory.  */
      rtx t = dest;

      if (sched_deps_info->use_cselib)
	{
	  enum machine_mode address_mode
	    = targetm.addr_space.address_mode (MEM_ADDR_SPACE (dest));

	  t = shallow_copy_rtx (dest);
	  cselib_lookup_from_insn (XEXP (t, 0), address_mode, 1, insn);
	  XEXP (t, 0) = cselib_subst_to_values (XEXP (t, 0));
	}
      t = canon_rtx (t);

      /* Pending lists can't get larger with a readonly context.  */
      if (!deps->readonly
          && ((deps->pending_read_list_length + deps->pending_write_list_length)
              > MAX_PENDING_LIST_LENGTH))
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
		note_mem_dep (t, XEXP (pending_mem, 0), XEXP (pending, 0),
			      DEP_ANTI);

	      pending = XEXP (pending, 1);
	      pending_mem = XEXP (pending_mem, 1);
	    }

	  pending = deps->pending_write_insns;
	  pending_mem = deps->pending_write_mems;
	  while (pending)
	    {
	      if (output_dependence (XEXP (pending_mem, 0), t)
		  && ! sched_insns_conditions_mutex_p (insn, XEXP (pending, 0)))
		note_mem_dep (t, XEXP (pending_mem, 0), XEXP (pending, 0),
			      DEP_OUTPUT);

	      pending = XEXP (pending, 1);
	      pending_mem = XEXP (pending_mem, 1);
	    }

	  add_dependence_list (insn, deps->last_pending_memory_flush, 1,
			       REG_DEP_ANTI);

          if (!deps->readonly)
            add_insn_mem_dependence (deps, false, insn, dest);
	}
      sched_analyze_2 (deps, XEXP (dest, 0), insn);
    }

  if (cslr_p && sched_deps_info->finish_lhs)
    sched_deps_info->finish_lhs ();

  /* Analyze reads.  */
  if (GET_CODE (x) == SET)
    {
      can_start_lhs_rhs_p = cslr_p;

      sched_analyze_2 (deps, SET_SRC (x), insn);

      can_start_lhs_rhs_p = false;
    }
}

/* Analyze the uses of memory and registers in rtx X in INSN.  */
static void
sched_analyze_2 (struct deps *deps, rtx x, rtx insn)
{
  int i;
  int j;
  enum rtx_code code;
  const char *fmt;
  bool cslr_p = can_start_lhs_rhs_p;

  can_start_lhs_rhs_p = false;

  gcc_assert (x);
  if (x == 0)
    return;

  if (cslr_p && sched_deps_info->start_rhs)
    sched_deps_info->start_rhs (x);

  code = GET_CODE (x);

  switch (code)
    {
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_FIXED:
    case CONST_VECTOR:
    case SYMBOL_REF:
    case CONST:
    case LABEL_REF:
      /* Ignore constants.  */
      if (cslr_p && sched_deps_info->finish_rhs)
	sched_deps_info->finish_rhs ();

      return;

#ifdef HAVE_cc0
    case CC0:
      /* User of CC0 depends on immediately preceding insn.  */
      SCHED_GROUP_P (insn) = 1;
       /* Don't move CC0 setter to another block (it can set up the
        same flag for previous CC0 users which is safe).  */
      CANT_MOVE (prev_nonnote_insn (insn)) = 1;

      if (cslr_p && sched_deps_info->finish_rhs)
	sched_deps_info->finish_rhs ();

      return;
#endif

    case REG:
      {
	int regno = REGNO (x);
	enum machine_mode mode = GET_MODE (x);

	sched_analyze_reg (deps, regno, mode, USE, insn);

#ifdef STACK_REGS
      /* Treat all reads of a stack register as modifying the TOS.  */
      if (regno >= FIRST_STACK_REG && regno <= LAST_STACK_REG)
	{
	  /* Avoid analyzing the same register twice.  */
	  if (regno != FIRST_STACK_REG)
	    sched_analyze_reg (deps, FIRST_STACK_REG, mode, USE, insn);
	  sched_analyze_reg (deps, FIRST_STACK_REG, mode, SET, insn);
	}
#endif

	if (cslr_p && sched_deps_info->finish_rhs)
	  sched_deps_info->finish_rhs ();

	return;
      }

    case MEM:
      {
	/* Reading memory.  */
	rtx u;
	rtx pending, pending_mem;
	rtx t = x;

	if (sched_deps_info->use_cselib)
	  {
	    enum machine_mode address_mode
	      = targetm.addr_space.address_mode (MEM_ADDR_SPACE (t));

	    t = shallow_copy_rtx (t);
	    cselib_lookup_from_insn (XEXP (t, 0), address_mode, 1, insn);
	    XEXP (t, 0) = cselib_subst_to_values (XEXP (t, 0));
	  }

	if (!DEBUG_INSN_P (insn))
	  {
	    t = canon_rtx (t);
	    pending = deps->pending_read_insns;
	    pending_mem = deps->pending_read_mems;
	    while (pending)
	      {
		if (read_dependence (XEXP (pending_mem, 0), t)
		    && ! sched_insns_conditions_mutex_p (insn,
							 XEXP (pending, 0)))
		  note_mem_dep (t, XEXP (pending_mem, 0), XEXP (pending, 0),
				DEP_ANTI);

		pending = XEXP (pending, 1);
		pending_mem = XEXP (pending_mem, 1);
	      }

	    pending = deps->pending_write_insns;
	    pending_mem = deps->pending_write_mems;
	    while (pending)
	      {
		if (true_dependence (XEXP (pending_mem, 0), VOIDmode,
				     t, rtx_varies_p)
		    && ! sched_insns_conditions_mutex_p (insn,
							 XEXP (pending, 0)))
		  note_mem_dep (t, XEXP (pending_mem, 0), XEXP (pending, 0),
				sched_deps_info->generate_spec_deps
				? BEGIN_DATA | DEP_TRUE : DEP_TRUE);

		pending = XEXP (pending, 1);
		pending_mem = XEXP (pending_mem, 1);
	      }

	    for (u = deps->last_pending_memory_flush; u; u = XEXP (u, 1))
	      {
		if (! JUMP_P (XEXP (u, 0)))
		  add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);
		else if (deps_may_trap_p (x))
		  {
		    if ((sched_deps_info->generate_spec_deps)
			&& sel_sched_p () && (spec_info->mask & BEGIN_CONTROL))
		      {
			ds_t ds = set_dep_weak (DEP_ANTI, BEGIN_CONTROL,
						MAX_DEP_WEAK);

			note_dep (XEXP (u, 0), ds);
		      }
		    else
		      add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);
		  }
	      }
	  }

	/* Always add these dependencies to pending_reads, since
	   this insn may be followed by a write.  */
        if (!deps->readonly)
          add_insn_mem_dependence (deps, true, insn, x);

	sched_analyze_2 (deps, XEXP (x, 0), insn);

	if (cslr_p && sched_deps_info->finish_rhs)
	  sched_deps_info->finish_rhs ();

	return;
      }

    /* Force pending stores to memory in case a trap handler needs them.  */
    case TRAP_IF:
      flush_pending_lists (deps, insn, true, false);
      break;

    case PREFETCH:
      if (PREFETCH_SCHEDULE_BARRIER_P (x))
	reg_pending_barrier = TRUE_BARRIER;
      break;

    case UNSPEC_VOLATILE:
      flush_pending_lists (deps, insn, true, true);
      /* FALLTHRU */

    case ASM_OPERANDS:
    case ASM_INPUT:
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

	    if (cslr_p && sched_deps_info->finish_rhs)
	      sched_deps_info->finish_rhs ();

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

      if (cslr_p && sched_deps_info->finish_rhs)
	sched_deps_info->finish_rhs ();

      return;

    case POST_MODIFY:
    case PRE_MODIFY:
      /* op0 = op0 + op1 */
      sched_analyze_2 (deps, XEXP (x, 0), insn);
      sched_analyze_2 (deps, XEXP (x, 1), insn);
      sched_analyze_1 (deps, x, insn);

      if (cslr_p && sched_deps_info->finish_rhs)
	sched_deps_info->finish_rhs ();

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

  if (cslr_p && sched_deps_info->finish_rhs)
    sched_deps_info->finish_rhs ();
}

/* Analyze an INSN with pattern X to find all dependencies.  */
static void
sched_analyze_insn (struct deps *deps, rtx x, rtx insn)
{
  RTX_CODE code = GET_CODE (x);
  rtx link;
  unsigned i;
  reg_set_iterator rsi;

  if (! reload_completed)
    {
      HARD_REG_SET temp;

      extract_insn (insn);
      preprocess_constraints ();
      ira_implicitly_set_insn_hard_regs (&temp);
      AND_COMPL_HARD_REG_SET (temp, ira_no_alloc_regs);
      IOR_HARD_REG_SET (implicit_reg_pending_clobbers, temp);
    }

  can_start_lhs_rhs_p = (NONJUMP_INSN_P (insn)
			 && code == SET);

  if (may_trap_p (x))
    /* Avoid moving trapping instructions accross function calls that might
       not always return.  */
    add_dependence_list (insn, deps->last_function_call_may_noreturn,
			 1, REG_DEP_ANTI);

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
	add_dependence_list (insn, deps->last_function_call, 1,
			     REG_DEP_OUTPUT);
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
      while (next && DEBUG_INSN_P (next))
	next = next_nonnote_insn (next);
      if (next && BARRIER_P (next))
	reg_pending_barrier = MOVE_BARRIER;
      else
	{
	  rtx pending, pending_mem;

          if (sched_deps_info->compute_jump_reg_dependencies)
            {
              regset_head tmp_uses, tmp_sets;
              INIT_REG_SET (&tmp_uses);
              INIT_REG_SET (&tmp_sets);

              (*sched_deps_info->compute_jump_reg_dependencies)
                (insn, &deps->reg_conditional_sets, &tmp_uses, &tmp_sets);
              /* Make latency of jump equal to 0 by using anti-dependence.  */
              EXECUTE_IF_SET_IN_REG_SET (&tmp_uses, 0, i, rsi)
                {
                  struct deps_reg *reg_last = &deps->reg_last[i];
                  add_dependence_list (insn, reg_last->sets, 0, REG_DEP_ANTI);
                  add_dependence_list (insn, reg_last->implicit_sets,
				       0, REG_DEP_ANTI);
                  add_dependence_list (insn, reg_last->clobbers, 0,
				       REG_DEP_ANTI);

                  if (!deps->readonly)
                    {
                      reg_last->uses_length++;
                      reg_last->uses = alloc_INSN_LIST (insn, reg_last->uses);
                    }
                }
              IOR_REG_SET (reg_pending_sets, &tmp_sets);

              CLEAR_REG_SET (&tmp_uses);
              CLEAR_REG_SET (&tmp_sets);
            }

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

  /* If this instruction can throw an exception, then moving it changes
     where block boundaries fall.  This is mighty confusing elsewhere.
     Therefore, prevent such an instruction from being moved.  Same for
     non-jump instructions that define block boundaries.
     ??? Unclear whether this is still necessary in EBB mode.  If not,
     add_branch_dependences should be adjusted for RGN mode instead.  */
  if (((CALL_P (insn) || JUMP_P (insn)) && can_throw_internal (insn))
      || (NONJUMP_INSN_P (insn) && control_flow_insn_p (insn)))
    reg_pending_barrier = MOVE_BARRIER;

  if (sched_pressure_p)
    {
      setup_insn_reg_uses (deps, insn);
      setup_insn_reg_pressure_info (insn);
    }

  /* Add register dependencies for insn.  */
  if (DEBUG_INSN_P (insn))
    {
      rtx prev = deps->last_debug_insn;
      rtx u;

      if (!deps->readonly)
	deps->last_debug_insn = insn;

      if (prev)
	add_dependence (insn, prev, REG_DEP_ANTI);

      add_dependence_list (insn, deps->last_function_call, 1,
			   REG_DEP_ANTI);

      for (u = deps->last_pending_memory_flush; u; u = XEXP (u, 1))
	if (! JUMP_P (XEXP (u, 0))
	    || !sel_sched_p ())
	  add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);

      EXECUTE_IF_SET_IN_REG_SET (reg_pending_uses, 0, i, rsi)
	{
	  struct deps_reg *reg_last = &deps->reg_last[i];
	  add_dependence_list (insn, reg_last->sets, 1, REG_DEP_ANTI);
	  add_dependence_list (insn, reg_last->clobbers, 1, REG_DEP_ANTI);

	  if (!deps->readonly)
	    reg_last->uses = alloc_INSN_LIST (insn, reg_last->uses);
	}
      CLEAR_REG_SET (reg_pending_uses);

      /* Quite often, a debug insn will refer to stuff in the
	 previous instruction, but the reason we want this
	 dependency here is to make sure the scheduler doesn't
	 gratuitously move a debug insn ahead.  This could dirty
	 DF flags and cause additional analysis that wouldn't have
	 occurred in compilation without debug insns, and such
	 additional analysis can modify the generated code.  */
      prev = PREV_INSN (insn);

      if (prev && NONDEBUG_INSN_P (prev))
	add_dependence (insn, prev, REG_DEP_ANTI);
    }
  else
    {
      EXECUTE_IF_SET_IN_REG_SET (reg_pending_uses, 0, i, rsi)
	{
	  struct deps_reg *reg_last = &deps->reg_last[i];
	  add_dependence_list (insn, reg_last->sets, 0, REG_DEP_TRUE);
	  add_dependence_list (insn, reg_last->implicit_sets, 0, REG_DEP_ANTI);
	  add_dependence_list (insn, reg_last->clobbers, 0, REG_DEP_TRUE);

	  if (!deps->readonly)
	    {
	      reg_last->uses = alloc_INSN_LIST (insn, reg_last->uses);
	      reg_last->uses_length++;
	    }
	}

      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (TEST_HARD_REG_BIT (implicit_reg_pending_uses, i))
	  {
	    struct deps_reg *reg_last = &deps->reg_last[i];
	    add_dependence_list (insn, reg_last->sets, 0, REG_DEP_TRUE);
	    add_dependence_list (insn, reg_last->implicit_sets, 0,
				 REG_DEP_ANTI);
	    add_dependence_list (insn, reg_last->clobbers, 0, REG_DEP_TRUE);

	    if (!deps->readonly)
	      {
		reg_last->uses = alloc_INSN_LIST (insn, reg_last->uses);
		reg_last->uses_length++;
	      }
	  }

      /* If the current insn is conditional, we can't free any
	 of the lists.  */
      if (sched_has_condition_p (insn))
	{
	  EXECUTE_IF_SET_IN_REG_SET (reg_pending_clobbers, 0, i, rsi)
	    {
	      struct deps_reg *reg_last = &deps->reg_last[i];
	      add_dependence_list (insn, reg_last->sets, 0, REG_DEP_OUTPUT);
	      add_dependence_list (insn, reg_last->implicit_sets, 0,
				   REG_DEP_ANTI);
	      add_dependence_list (insn, reg_last->uses, 0, REG_DEP_ANTI);

	      if (!deps->readonly)
		{
		  reg_last->clobbers
		    = alloc_INSN_LIST (insn, reg_last->clobbers);
		  reg_last->clobbers_length++;
		}
	    }
	  EXECUTE_IF_SET_IN_REG_SET (reg_pending_sets, 0, i, rsi)
	    {
	      struct deps_reg *reg_last = &deps->reg_last[i];
	      add_dependence_list (insn, reg_last->sets, 0, REG_DEP_OUTPUT);
	      add_dependence_list (insn, reg_last->implicit_sets, 0,
				   REG_DEP_ANTI);
	      add_dependence_list (insn, reg_last->clobbers, 0, REG_DEP_OUTPUT);
	      add_dependence_list (insn, reg_last->uses, 0, REG_DEP_ANTI);

	      if (!deps->readonly)
		{
		  reg_last->sets = alloc_INSN_LIST (insn, reg_last->sets);
		  SET_REGNO_REG_SET (&deps->reg_conditional_sets, i);
		}
	    }
	}
      else
	{
	  EXECUTE_IF_SET_IN_REG_SET (reg_pending_clobbers, 0, i, rsi)
	    {
	      struct deps_reg *reg_last = &deps->reg_last[i];
	      if (reg_last->uses_length > MAX_PENDING_LIST_LENGTH
		  || reg_last->clobbers_length > MAX_PENDING_LIST_LENGTH)
		{
		  add_dependence_list_and_free (deps, insn, &reg_last->sets, 0,
						REG_DEP_OUTPUT);
		  add_dependence_list_and_free (deps, insn,
						&reg_last->implicit_sets, 0,
						REG_DEP_ANTI);
		  add_dependence_list_and_free (deps, insn, &reg_last->uses, 0,
						REG_DEP_ANTI);
		  add_dependence_list_and_free
		    (deps, insn, &reg_last->clobbers, 0, REG_DEP_OUTPUT);

		  if (!deps->readonly)
		    {
		      reg_last->sets = alloc_INSN_LIST (insn, reg_last->sets);
		      reg_last->clobbers_length = 0;
		      reg_last->uses_length = 0;
		    }
		}
	      else
		{
		  add_dependence_list (insn, reg_last->sets, 0, REG_DEP_OUTPUT);
		  add_dependence_list (insn, reg_last->implicit_sets, 0,
				       REG_DEP_ANTI);
		  add_dependence_list (insn, reg_last->uses, 0, REG_DEP_ANTI);
		}

	      if (!deps->readonly)
		{
		  reg_last->clobbers_length++;
		  reg_last->clobbers
		    = alloc_INSN_LIST (insn, reg_last->clobbers);
		}
	    }
	  EXECUTE_IF_SET_IN_REG_SET (reg_pending_sets, 0, i, rsi)
	    {
	      struct deps_reg *reg_last = &deps->reg_last[i];

	      add_dependence_list_and_free (deps, insn, &reg_last->sets, 0,
					    REG_DEP_OUTPUT);
	      add_dependence_list_and_free (deps, insn,
					    &reg_last->implicit_sets,
					    0, REG_DEP_ANTI);
	      add_dependence_list_and_free (deps, insn, &reg_last->clobbers, 0,
					    REG_DEP_OUTPUT);
	      add_dependence_list_and_free (deps, insn, &reg_last->uses, 0,
					    REG_DEP_ANTI);

	      if (!deps->readonly)
		{
		  reg_last->sets = alloc_INSN_LIST (insn, reg_last->sets);
		  reg_last->uses_length = 0;
		  reg_last->clobbers_length = 0;
		  CLEAR_REGNO_REG_SET (&deps->reg_conditional_sets, i);
		}
	    }
	}
    }

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (TEST_HARD_REG_BIT (implicit_reg_pending_clobbers, i))
      {
	struct deps_reg *reg_last = &deps->reg_last[i];
	add_dependence_list (insn, reg_last->sets, 0, REG_DEP_ANTI);
	add_dependence_list (insn, reg_last->clobbers, 0, REG_DEP_ANTI);
	add_dependence_list (insn, reg_last->uses, 0, REG_DEP_ANTI);

	if (!deps->readonly)
	  reg_last->implicit_sets
	    = alloc_INSN_LIST (insn, reg_last->implicit_sets);
      }

  if (!deps->readonly)
    {
      IOR_REG_SET (&deps->reg_last_in_use, reg_pending_uses);
      IOR_REG_SET (&deps->reg_last_in_use, reg_pending_clobbers);
      IOR_REG_SET (&deps->reg_last_in_use, reg_pending_sets);
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (TEST_HARD_REG_BIT (implicit_reg_pending_uses, i)
	    || TEST_HARD_REG_BIT (implicit_reg_pending_clobbers, i))
	  SET_REGNO_REG_SET (&deps->reg_last_in_use, i);

      /* Set up the pending barrier found.  */
      deps->last_reg_pending_barrier = reg_pending_barrier;
    }

  CLEAR_REG_SET (reg_pending_uses);
  CLEAR_REG_SET (reg_pending_clobbers);
  CLEAR_REG_SET (reg_pending_sets);
  CLEAR_HARD_REG_SET (implicit_reg_pending_clobbers);
  CLEAR_HARD_REG_SET (implicit_reg_pending_uses);

  /* Add dependencies if a scheduling barrier was found.  */
  if (reg_pending_barrier)
    {
      /* In the case of barrier the most added dependencies are not
         real, so we use anti-dependence here.  */
      if (sched_has_condition_p (insn))
	{
	  EXECUTE_IF_SET_IN_REG_SET (&deps->reg_last_in_use, 0, i, rsi)
	    {
	      struct deps_reg *reg_last = &deps->reg_last[i];
	      add_dependence_list (insn, reg_last->uses, 0, REG_DEP_ANTI);
	      add_dependence_list (insn, reg_last->sets, 0,
				   reg_pending_barrier == TRUE_BARRIER
				   ? REG_DEP_TRUE : REG_DEP_ANTI);
	      add_dependence_list (insn, reg_last->implicit_sets, 0,
				   REG_DEP_ANTI);
	      add_dependence_list (insn, reg_last->clobbers, 0,
				   reg_pending_barrier == TRUE_BARRIER
				   ? REG_DEP_TRUE : REG_DEP_ANTI);
	    }
	}
      else
	{
	  EXECUTE_IF_SET_IN_REG_SET (&deps->reg_last_in_use, 0, i, rsi)
	    {
	      struct deps_reg *reg_last = &deps->reg_last[i];
	      add_dependence_list_and_free (deps, insn, &reg_last->uses, 0,
					    REG_DEP_ANTI);
	      add_dependence_list_and_free (deps, insn, &reg_last->sets, 0,
					    reg_pending_barrier == TRUE_BARRIER
					    ? REG_DEP_TRUE : REG_DEP_ANTI);
	      add_dependence_list_and_free (deps, insn,
					    &reg_last->implicit_sets, 0,
					    REG_DEP_ANTI);
	      add_dependence_list_and_free (deps, insn, &reg_last->clobbers, 0,
					    reg_pending_barrier == TRUE_BARRIER
					    ? REG_DEP_TRUE : REG_DEP_ANTI);

              if (!deps->readonly)
                {
                  reg_last->uses_length = 0;
                  reg_last->clobbers_length = 0;
                }
	    }
	}

      if (!deps->readonly)
        for (i = 0; i < (unsigned)deps->max_reg; i++)
          {
            struct deps_reg *reg_last = &deps->reg_last[i];
            reg_last->sets = alloc_INSN_LIST (insn, reg_last->sets);
            SET_REGNO_REG_SET (&deps->reg_last_in_use, i);
          }

      /* Flush pending lists on jumps, but not on speculative checks.  */
      if (JUMP_P (insn) && !(sel_sched_p ()
                             && sel_insn_is_speculation_check (insn)))
	flush_pending_lists (deps, insn, true, true);

      if (!deps->readonly)
        CLEAR_REG_SET (&deps->reg_conditional_sets);
      reg_pending_barrier = NOT_A_BARRIER;
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
	{
	  if (DEBUG_INSN_P (insn))
	    /* We don't want to mark debug insns as part of the same
	       sched group.  We know they really aren't, but if we use
	       debug insns to tell that a call group is over, we'll
	       get different code if debug insns are not there and
	       instructions that follow seem like they should be part
	       of the call group.

	       Also, if we did, fixup_sched_groups() would move the
	       deps of the debug insn to the call insn, modifying
	       non-debug post-dependency counts of the debug insn
	       dependencies and otherwise messing with the scheduling
	       order.

	       Instead, let such debug insns be scheduled freely, but
	       keep the call group open in case there are insns that
	       should be part of it afterwards.  Since we grant debug
	       insns higher priority than even sched group insns, it
	       will all turn out all right.  */
	    goto debug_dont_end_call_group;
	  else
	    goto end_call_group;
	}

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
	  if (!deps->readonly
              && deps->in_post_call_group_p == post_call_initial)
	    deps->in_post_call_group_p = post_call;

          if (!sel_sched_p () || sched_emulate_haifa_p)
            {
              SCHED_GROUP_P (insn) = 1;
              CANT_MOVE (insn) = 1;
            }
	}
      else
	{
	end_call_group:
          if (!deps->readonly)
            deps->in_post_call_group_p = not_post_call;
	}
    }

 debug_dont_end_call_group:
  if ((current_sched_info->flags & DO_SPECULATION)
      && !sched_insn_is_legitimate_for_speculation_p (insn, 0))
    /* INSN has an internal dependency (e.g. r14 = [r14]) and thus cannot
       be speculated.  */
    {
      if (sel_sched_p ())
        sel_mark_hard_insn (insn);
      else
        {
          sd_iterator_def sd_it;
          dep_t dep;

          for (sd_it = sd_iterator_start (insn, SD_LIST_SPEC_BACK);
               sd_iterator_cond (&sd_it, &dep);)
            change_spec_dep_to_hard (sd_it);
        }
    }
}

/* Return TRUE if INSN might not always return normally (e.g. call exit,
   longjmp, loop forever, ...).  */
static bool
call_may_noreturn_p (rtx insn)
{
  rtx call;

  /* const or pure calls that aren't looping will always return.  */
  if (RTL_CONST_OR_PURE_CALL_P (insn)
      && !RTL_LOOPING_CONST_OR_PURE_CALL_P (insn))
    return false;

  call = PATTERN (insn);
  if (GET_CODE (call) == PARALLEL)
    call = XVECEXP (call, 0, 0);
  if (GET_CODE (call) == SET)
    call = SET_SRC (call);
  if (GET_CODE (call) == CALL
      && MEM_P (XEXP (call, 0))
      && GET_CODE (XEXP (XEXP (call, 0), 0)) == SYMBOL_REF)
    {
      rtx symbol = XEXP (XEXP (call, 0), 0);
      if (SYMBOL_REF_DECL (symbol)
	  && TREE_CODE (SYMBOL_REF_DECL (symbol)) == FUNCTION_DECL)
	{
	  if (DECL_BUILT_IN_CLASS (SYMBOL_REF_DECL (symbol))
	      == BUILT_IN_NORMAL)
	    switch (DECL_FUNCTION_CODE (SYMBOL_REF_DECL (symbol)))
	      {
	      case BUILT_IN_BCMP:
	      case BUILT_IN_BCOPY:
	      case BUILT_IN_BZERO:
	      case BUILT_IN_INDEX:
	      case BUILT_IN_MEMCHR:
	      case BUILT_IN_MEMCMP:
	      case BUILT_IN_MEMCPY:
	      case BUILT_IN_MEMMOVE:
	      case BUILT_IN_MEMPCPY:
	      case BUILT_IN_MEMSET:
	      case BUILT_IN_RINDEX:
	      case BUILT_IN_STPCPY:
	      case BUILT_IN_STPNCPY:
	      case BUILT_IN_STRCAT:
	      case BUILT_IN_STRCHR:
	      case BUILT_IN_STRCMP:
	      case BUILT_IN_STRCPY:
	      case BUILT_IN_STRCSPN:
	      case BUILT_IN_STRLEN:
	      case BUILT_IN_STRNCAT:
	      case BUILT_IN_STRNCMP:
	      case BUILT_IN_STRNCPY:
	      case BUILT_IN_STRPBRK:
	      case BUILT_IN_STRRCHR:
	      case BUILT_IN_STRSPN:
	      case BUILT_IN_STRSTR:
		/* Assume certain string/memory builtins always return.  */
		return false;
	      default:
		break;
	      }
	}
    }

  /* For all other calls assume that they might not always return.  */
  return true;
}

/* Analyze INSN with DEPS as a context.  */
void
deps_analyze_insn (struct deps *deps, rtx insn)
{
  if (sched_deps_info->start_insn)
    sched_deps_info->start_insn (insn);

  if (NONJUMP_INSN_P (insn) || DEBUG_INSN_P (insn) || JUMP_P (insn))
    {
      /* Make each JUMP_INSN (but not a speculative check)
         a scheduling barrier for memory references.  */
      if (!deps->readonly
          && JUMP_P (insn)
          && !(sel_sched_p ()
               && sel_insn_is_speculation_check (insn)))
        {
          /* Keep the list a reasonable size.  */
          if (deps->pending_flush_length++ > MAX_PENDING_LIST_LENGTH)
            flush_pending_lists (deps, insn, true, true);
          else
            deps->last_pending_memory_flush
              = alloc_INSN_LIST (insn, deps->last_pending_memory_flush);
        }

      sched_analyze_insn (deps, PATTERN (insn), insn);
    }
  else if (CALL_P (insn))
    {
      int i;

      CANT_MOVE (insn) = 1;

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
                SET_HARD_REG_BIT (implicit_reg_pending_uses, i);
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
	      SET_HARD_REG_BIT (implicit_reg_pending_uses, i);
          /* The frame pointer is normally not used by the function
             itself, but by the debugger.  */
          /* ??? MIPS o32 is an exception.  It uses the frame pointer
             in the macro expansion of jal but does not represent this
             fact in the call_insn rtl.  */
            else if (i == FRAME_POINTER_REGNUM
                     || (i == HARD_FRAME_POINTER_REGNUM
                         && (! reload_completed || frame_pointer_needed)))
	      SET_HARD_REG_BIT (implicit_reg_pending_uses, i);
        }

      /* For each insn which shouldn't cross a call, add a dependence
         between that insn and this call insn.  */
      add_dependence_list_and_free (deps, insn,
                                    &deps->sched_before_next_call, 1,
                                    REG_DEP_ANTI);

      sched_analyze_insn (deps, PATTERN (insn), insn);

      /* If CALL would be in a sched group, then this will violate
	 convention that sched group insns have dependencies only on the
	 previous instruction.

	 Of course one can say: "Hey!  What about head of the sched group?"
	 And I will answer: "Basic principles (one dep per insn) are always
	 the same."  */
      gcc_assert (!SCHED_GROUP_P (insn));

      /* In the absence of interprocedural alias analysis, we must flush
         all pending reads and writes, and start new dependencies starting
         from here.  But only flush writes for constant calls (which may
         be passed a pointer to something we haven't written yet).  */
      flush_pending_lists (deps, insn, true, ! RTL_CONST_OR_PURE_CALL_P (insn));

      if (!deps->readonly)
        {
          /* Remember the last function call for limiting lifetimes.  */
          free_INSN_LIST_list (&deps->last_function_call);
          deps->last_function_call = alloc_INSN_LIST (insn, NULL_RTX);

	  if (call_may_noreturn_p (insn))
	    {
	      /* Remember the last function call that might not always return
		 normally for limiting moves of trapping insns.  */
	      free_INSN_LIST_list (&deps->last_function_call_may_noreturn);
	      deps->last_function_call_may_noreturn
		= alloc_INSN_LIST (insn, NULL_RTX);
	    }

          /* Before reload, begin a post-call group, so as to keep the
             lifetimes of hard registers correct.  */
          if (! reload_completed)
            deps->in_post_call_group_p = post_call;
        }
    }

  if (sched_deps_info->use_cselib)
    cselib_process_insn (insn);

  /* EH_REGION insn notes can not appear until well after we complete
     scheduling.  */
  if (NOTE_P (insn))
    gcc_assert (NOTE_KIND (insn) != NOTE_INSN_EH_REGION_BEG
		&& NOTE_KIND (insn) != NOTE_INSN_EH_REGION_END);

  if (sched_deps_info->finish_insn)
    sched_deps_info->finish_insn ();

  /* Fixup the dependencies in the sched group.  */
  if ((NONJUMP_INSN_P (insn) || JUMP_P (insn))
      && SCHED_GROUP_P (insn) && !sel_sched_p ())
    fixup_sched_groups (insn);
}

/* Initialize DEPS for the new block beginning with HEAD.  */
void
deps_start_bb (struct deps *deps, rtx head)
{
  gcc_assert (!deps->readonly);

  /* Before reload, if the previous block ended in a call, show that
     we are inside a post-call group, so as to keep the lifetimes of
     hard registers correct.  */
  if (! reload_completed && !LABEL_P (head))
    {
      rtx insn = prev_nonnote_insn (head);

      while (insn && DEBUG_INSN_P (insn))
	insn = prev_nonnote_insn (insn);
      if (insn && CALL_P (insn))
	deps->in_post_call_group_p = post_call_initial;
    }
}

/* Analyze every insn between HEAD and TAIL inclusive, creating backward
   dependencies for each insn.  */
void
sched_analyze (struct deps *deps, rtx head, rtx tail)
{
  rtx insn;

  if (sched_deps_info->use_cselib)
    cselib_init (CSELIB_RECORD_MEMORY);

  deps_start_bb (deps, head);

  for (insn = head;; insn = NEXT_INSN (insn))
    {

      if (INSN_P (insn))
	{
	  /* And initialize deps_lists.  */
	  sd_init_insn (insn);
	}

      deps_analyze_insn (deps, insn);

      if (insn == tail)
	{
	  if (sched_deps_info->use_cselib)
	    cselib_finish ();
	  return;
	}
    }
  gcc_unreachable ();
}

/* Helper for sched_free_deps ().
   Delete INSN's (RESOLVED_P) backward dependencies.  */
static void
delete_dep_nodes_in_back_deps (rtx insn, bool resolved_p)
{
  sd_iterator_def sd_it;
  dep_t dep;
  sd_list_types_def types;

  if (resolved_p)
    types = SD_LIST_RES_BACK;
  else
    types = SD_LIST_BACK;

  for (sd_it = sd_iterator_start (insn, types);
       sd_iterator_cond (&sd_it, &dep);)
    {
      dep_link_t link = *sd_it.linkp;
      dep_node_t node = DEP_LINK_NODE (link);
      deps_list_t back_list;
      deps_list_t forw_list;

      get_back_and_forw_lists (dep, resolved_p, &back_list, &forw_list);
      remove_from_deps_list (link, back_list);
      delete_dep_node (node);
    }
}

/* Delete (RESOLVED_P) dependencies between HEAD and TAIL together with
   deps_lists.  */
void
sched_free_deps (rtx head, rtx tail, bool resolved_p)
{
  rtx insn;
  rtx next_tail = NEXT_INSN (tail);

  for (insn = head; insn != next_tail; insn = NEXT_INSN (insn))
    if (INSN_P (insn) && INSN_LUID (insn) > 0)
      {
	/* Clear resolved back deps together with its dep_nodes.  */
	delete_dep_nodes_in_back_deps (insn, resolved_p);

	/* Clear forward deps and leave the dep_nodes to the
	   corresponding back_deps list.  */
	if (resolved_p)
	  clear_deps_list (INSN_RESOLVED_FORW_DEPS (insn));
	else
	  clear_deps_list (INSN_FORW_DEPS (insn));

	sd_finish_insn (insn);
      }
}

/* Initialize variables for region data dependence analysis.
   When LAZY_REG_LAST is true, do not allocate reg_last array
   of struct deps immediately.  */

void
init_deps (struct deps *deps, bool lazy_reg_last)
{
  int max_reg = (reload_completed ? FIRST_PSEUDO_REGISTER : max_reg_num ());

  deps->max_reg = max_reg;
  if (lazy_reg_last)
    deps->reg_last = NULL;
  else
    deps->reg_last = XCNEWVEC (struct deps_reg, max_reg);
  INIT_REG_SET (&deps->reg_last_in_use);
  INIT_REG_SET (&deps->reg_conditional_sets);

  deps->pending_read_insns = 0;
  deps->pending_read_mems = 0;
  deps->pending_write_insns = 0;
  deps->pending_write_mems = 0;
  deps->pending_read_list_length = 0;
  deps->pending_write_list_length = 0;
  deps->pending_flush_length = 0;
  deps->last_pending_memory_flush = 0;
  deps->last_function_call = 0;
  deps->last_function_call_may_noreturn = 0;
  deps->sched_before_next_call = 0;
  deps->in_post_call_group_p = not_post_call;
  deps->last_debug_insn = 0;
  deps->last_reg_pending_barrier = NOT_A_BARRIER;
  deps->readonly = 0;
}

/* Init only reg_last field of DEPS, which was not allocated before as
   we inited DEPS lazily.  */
void
init_deps_reg_last (struct deps *deps)
{
  gcc_assert (deps && deps->max_reg > 0);
  gcc_assert (deps->reg_last == NULL);

  deps->reg_last = XCNEWVEC (struct deps_reg, deps->max_reg);
}


/* Free insn lists found in DEPS.  */

void
free_deps (struct deps *deps)
{
  unsigned i;
  reg_set_iterator rsi;

  /* We set max_reg to 0 when this context was already freed.  */
  if (deps->max_reg == 0)
    {
      gcc_assert (deps->reg_last == NULL);
      return;
    }
  deps->max_reg = 0;

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
      if (reg_last->implicit_sets)
	free_INSN_LIST_list (&reg_last->implicit_sets);
      if (reg_last->clobbers)
	free_INSN_LIST_list (&reg_last->clobbers);
    }
  CLEAR_REG_SET (&deps->reg_last_in_use);
  CLEAR_REG_SET (&deps->reg_conditional_sets);

  /* As we initialize reg_last lazily, it is possible that we didn't allocate
     it at all.  */
  if (deps->reg_last)
    free (deps->reg_last);
  deps->reg_last = NULL;

  deps = NULL;
}

/* Remove INSN from dependence contexts DEPS.  Caution: reg_conditional_sets
   is not handled.  */
void
remove_from_deps (struct deps *deps, rtx insn)
{
  int removed;
  unsigned i;
  reg_set_iterator rsi;

  removed = remove_from_both_dependence_lists (insn, &deps->pending_read_insns,
                                               &deps->pending_read_mems);
  if (!DEBUG_INSN_P (insn))
    deps->pending_read_list_length -= removed;
  removed = remove_from_both_dependence_lists (insn, &deps->pending_write_insns,
                                               &deps->pending_write_mems);
  deps->pending_write_list_length -= removed;
  removed = remove_from_dependence_list (insn, &deps->last_pending_memory_flush);
  deps->pending_flush_length -= removed;

  EXECUTE_IF_SET_IN_REG_SET (&deps->reg_last_in_use, 0, i, rsi)
    {
      struct deps_reg *reg_last = &deps->reg_last[i];
      if (reg_last->uses)
	remove_from_dependence_list (insn, &reg_last->uses);
      if (reg_last->sets)
	remove_from_dependence_list (insn, &reg_last->sets);
      if (reg_last->implicit_sets)
	remove_from_dependence_list (insn, &reg_last->implicit_sets);
      if (reg_last->clobbers)
	remove_from_dependence_list (insn, &reg_last->clobbers);
      if (!reg_last->uses && !reg_last->sets && !reg_last->implicit_sets
	  && !reg_last->clobbers)
        CLEAR_REGNO_REG_SET (&deps->reg_last_in_use, i);
    }

  if (CALL_P (insn))
    {
      remove_from_dependence_list (insn, &deps->last_function_call);
      remove_from_dependence_list (insn,
				   &deps->last_function_call_may_noreturn);
    }
  remove_from_dependence_list (insn, &deps->sched_before_next_call);
}

/* Init deps data vector.  */
static void
init_deps_data_vector (void)
{
  int reserve = (sched_max_luid + 1
                 - VEC_length (haifa_deps_insn_data_def, h_d_i_d));
  if (reserve > 0
      && ! VEC_space (haifa_deps_insn_data_def, h_d_i_d, reserve))
    VEC_safe_grow_cleared (haifa_deps_insn_data_def, heap, h_d_i_d,
                           3 * sched_max_luid / 2);
}

/* If it is profitable to use them, initialize or extend (depending on
   GLOBAL_P) dependency data.  */
void
sched_deps_init (bool global_p)
{
  /* Average number of insns in the basic block.
     '+ 1' is used to make it nonzero.  */
  int insns_in_block = sched_max_luid / n_basic_blocks + 1;

  init_deps_data_vector ();

  /* We use another caching mechanism for selective scheduling, so
     we don't use this one.  */
  if (!sel_sched_p () && global_p && insns_in_block > 100 * 5)
    {
      /* ?!? We could save some memory by computing a per-region luid mapping
         which could reduce both the number of vectors in the cache and the
         size of each vector.  Instead we just avoid the cache entirely unless
         the average number of instructions in a basic block is very high.  See
         the comment before the declaration of true_dependency_cache for
         what we consider "very high".  */
      cache_size = 0;
      extend_dependency_caches (sched_max_luid, true);
    }

  if (global_p)
    {
      dl_pool = create_alloc_pool ("deps_list", sizeof (struct _deps_list),
                                   /* Allocate lists for one block at a time.  */
                                   insns_in_block);
      dn_pool = create_alloc_pool ("dep_node", sizeof (struct _dep_node),
                                   /* Allocate nodes for one block at a time.
                                      We assume that average insn has
                                      5 producers.  */
                                   5 * insns_in_block);
    }
}


/* Create or extend (depending on CREATE_P) dependency caches to
   size N.  */
void
extend_dependency_caches (int n, bool create_p)
{
  if (create_p || true_dependency_cache)
    {
      int i, luid = cache_size + n;

      true_dependency_cache = XRESIZEVEC (bitmap_head, true_dependency_cache,
					  luid);
      output_dependency_cache = XRESIZEVEC (bitmap_head,
					    output_dependency_cache, luid);
      anti_dependency_cache = XRESIZEVEC (bitmap_head, anti_dependency_cache,
					  luid);

      if (current_sched_info->flags & DO_SPECULATION)
        spec_dependency_cache = XRESIZEVEC (bitmap_head, spec_dependency_cache,
					    luid);

      for (i = cache_size; i < luid; i++)
	{
	  bitmap_initialize (&true_dependency_cache[i], 0);
	  bitmap_initialize (&output_dependency_cache[i], 0);
	  bitmap_initialize (&anti_dependency_cache[i], 0);

          if (current_sched_info->flags & DO_SPECULATION)
            bitmap_initialize (&spec_dependency_cache[i], 0);
	}
      cache_size = luid;
    }
}

/* Finalize dependency information for the whole function.  */
void
sched_deps_finish (void)
{
  gcc_assert (deps_pools_are_empty_p ());
  free_alloc_pool_if_empty (&dn_pool);
  free_alloc_pool_if_empty (&dl_pool);
  gcc_assert (dn_pool == NULL && dl_pool == NULL);

  VEC_free (haifa_deps_insn_data_def, heap, h_d_i_d);
  cache_size = 0;

  if (true_dependency_cache)
    {
      int i;

      for (i = 0; i < cache_size; i++)
	{
	  bitmap_clear (&true_dependency_cache[i]);
	  bitmap_clear (&output_dependency_cache[i]);
	  bitmap_clear (&anti_dependency_cache[i]);

          if (sched_deps_info->generate_spec_deps)
            bitmap_clear (&spec_dependency_cache[i]);
	}
      free (true_dependency_cache);
      true_dependency_cache = NULL;
      free (output_dependency_cache);
      output_dependency_cache = NULL;
      free (anti_dependency_cache);
      anti_dependency_cache = NULL;

      if (sched_deps_info->generate_spec_deps)
        {
          free (spec_dependency_cache);
          spec_dependency_cache = NULL;
        }

    }
}

/* Initialize some global variables needed by the dependency analysis
   code.  */

void
init_deps_global (void)
{
  CLEAR_HARD_REG_SET (implicit_reg_pending_clobbers);
  CLEAR_HARD_REG_SET (implicit_reg_pending_uses);
  reg_pending_sets = ALLOC_REG_SET (&reg_obstack);
  reg_pending_clobbers = ALLOC_REG_SET (&reg_obstack);
  reg_pending_uses = ALLOC_REG_SET (&reg_obstack);
  reg_pending_barrier = NOT_A_BARRIER;

  if (!sel_sched_p () || sched_emulate_haifa_p)
    {
      sched_deps_info->start_insn = haifa_start_insn;
      sched_deps_info->finish_insn = haifa_finish_insn;

      sched_deps_info->note_reg_set = haifa_note_reg_set;
      sched_deps_info->note_reg_clobber = haifa_note_reg_clobber;
      sched_deps_info->note_reg_use = haifa_note_reg_use;

      sched_deps_info->note_mem_dep = haifa_note_mem_dep;
      sched_deps_info->note_dep = haifa_note_dep;
   }
}

/* Free everything used by the dependency analysis code.  */

void
finish_deps_global (void)
{
  FREE_REG_SET (reg_pending_sets);
  FREE_REG_SET (reg_pending_clobbers);
  FREE_REG_SET (reg_pending_uses);
}

/* Estimate the weakness of dependence between MEM1 and MEM2.  */
dw_t
estimate_dep_weak (rtx mem1, rtx mem2)
{
  rtx r1, r2;

  if (mem1 == mem2)
    /* MEMs are the same - don't speculate.  */
    return MIN_DEP_WEAK;

  r1 = XEXP (mem1, 0);
  r2 = XEXP (mem2, 0);

  if (r1 == r2
      || (REG_P (r1) && REG_P (r2)
	  && REGNO (r1) == REGNO (r2)))
    /* Again, MEMs are the same.  */
    return MIN_DEP_WEAK;
  else if ((REG_P (r1) && !REG_P (r2))
	   || (!REG_P (r1) && REG_P (r2)))
    /* Different addressing modes - reason to be more speculative,
       than usual.  */
    return NO_DEP_WEAK - (NO_DEP_WEAK - UNCERTAIN_DEP_WEAK) / 2;
  else
    /* We can't say anything about the dependence.  */
    return UNCERTAIN_DEP_WEAK;
}

/* Add or update backward dependence between INSN and ELEM with type DEP_TYPE.
   This function can handle same INSN and ELEM (INSN == ELEM).
   It is a convenience wrapper.  */
void
add_dependence (rtx insn, rtx elem, enum reg_note dep_type)
{
  ds_t ds;
  bool internal;

  if (dep_type == REG_DEP_TRUE)
    ds = DEP_TRUE;
  else if (dep_type == REG_DEP_OUTPUT)
    ds = DEP_OUTPUT;
  else
    {
      gcc_assert (dep_type == REG_DEP_ANTI);
      ds = DEP_ANTI;
    }

  /* When add_dependence is called from inside sched-deps.c, we expect
     cur_insn to be non-null.  */
  internal = cur_insn != NULL;
  if (internal)
    gcc_assert (insn == cur_insn);
  else
    cur_insn = insn;

  note_dep (elem, ds);
  if (!internal)
    cur_insn = NULL;
}

/* Return weakness of speculative type TYPE in the dep_status DS.  */
dw_t
get_dep_weak_1 (ds_t ds, ds_t type)
{
  ds = ds & type;

  switch (type)
    {
    case BEGIN_DATA: ds >>= BEGIN_DATA_BITS_OFFSET; break;
    case BE_IN_DATA: ds >>= BE_IN_DATA_BITS_OFFSET; break;
    case BEGIN_CONTROL: ds >>= BEGIN_CONTROL_BITS_OFFSET; break;
    case BE_IN_CONTROL: ds >>= BE_IN_CONTROL_BITS_OFFSET; break;
    default: gcc_unreachable ();
    }

  return (dw_t) ds;
}

dw_t
get_dep_weak (ds_t ds, ds_t type)
{
  dw_t dw = get_dep_weak_1 (ds, type);

  gcc_assert (MIN_DEP_WEAK <= dw && dw <= MAX_DEP_WEAK);
  return dw;
}

/* Return the dep_status, which has the same parameters as DS, except for
   speculative type TYPE, that will have weakness DW.  */
ds_t
set_dep_weak (ds_t ds, ds_t type, dw_t dw)
{
  gcc_assert (MIN_DEP_WEAK <= dw && dw <= MAX_DEP_WEAK);

  ds &= ~type;
  switch (type)
    {
    case BEGIN_DATA: ds |= ((ds_t) dw) << BEGIN_DATA_BITS_OFFSET; break;
    case BE_IN_DATA: ds |= ((ds_t) dw) << BE_IN_DATA_BITS_OFFSET; break;
    case BEGIN_CONTROL: ds |= ((ds_t) dw) << BEGIN_CONTROL_BITS_OFFSET; break;
    case BE_IN_CONTROL: ds |= ((ds_t) dw) << BE_IN_CONTROL_BITS_OFFSET; break;
    default: gcc_unreachable ();
    }
  return ds;
}

/* Return the join of two dep_statuses DS1 and DS2.
   If MAX_P is true then choose the greater probability,
   otherwise multiply probabilities.
   This function assumes that both DS1 and DS2 contain speculative bits.  */
static ds_t
ds_merge_1 (ds_t ds1, ds_t ds2, bool max_p)
{
  ds_t ds, t;

  gcc_assert ((ds1 & SPECULATIVE) && (ds2 & SPECULATIVE));

  ds = (ds1 & DEP_TYPES) | (ds2 & DEP_TYPES);

  t = FIRST_SPEC_TYPE;
  do
    {
      if ((ds1 & t) && !(ds2 & t))
	ds |= ds1 & t;
      else if (!(ds1 & t) && (ds2 & t))
	ds |= ds2 & t;
      else if ((ds1 & t) && (ds2 & t))
	{
	  dw_t dw1 = get_dep_weak (ds1, t);
	  dw_t dw2 = get_dep_weak (ds2, t);
	  ds_t dw;

	  if (!max_p)
	    {
	      dw = ((ds_t) dw1) * ((ds_t) dw2);
	      dw /= MAX_DEP_WEAK;
	      if (dw < MIN_DEP_WEAK)
		dw = MIN_DEP_WEAK;
	    }
	  else
	    {
	      if (dw1 >= dw2)
		dw = dw1;
	      else
		dw = dw2;
	    }

	  ds = set_dep_weak (ds, t, (dw_t) dw);
	}

      if (t == LAST_SPEC_TYPE)
	break;
      t <<= SPEC_TYPE_SHIFT;
    }
  while (1);

  return ds;
}

/* Return the join of two dep_statuses DS1 and DS2.
   This function assumes that both DS1 and DS2 contain speculative bits.  */
ds_t
ds_merge (ds_t ds1, ds_t ds2)
{
  return ds_merge_1 (ds1, ds2, false);
}

/* Return the join of two dep_statuses DS1 and DS2.  */
ds_t
ds_full_merge (ds_t ds, ds_t ds2, rtx mem1, rtx mem2)
{
  ds_t new_status = ds | ds2;

  if (new_status & SPECULATIVE)
    {
      if ((ds && !(ds & SPECULATIVE))
	  || (ds2 && !(ds2 & SPECULATIVE)))
	/* Then this dep can't be speculative.  */
	new_status &= ~SPECULATIVE;
      else
	{
	  /* Both are speculative.  Merging probabilities.  */
	  if (mem1)
	    {
	      dw_t dw;

	      dw = estimate_dep_weak (mem1, mem2);
	      ds = set_dep_weak (ds, BEGIN_DATA, dw);
	    }

	  if (!ds)
	    new_status = ds2;
	  else if (!ds2)
	    new_status = ds;
	  else
	    new_status = ds_merge (ds2, ds);
	}
    }

  return new_status;
}

/* Return the join of DS1 and DS2.  Use maximum instead of multiplying
   probabilities.  */
ds_t
ds_max_merge (ds_t ds1, ds_t ds2)
{
  if (ds1 == 0 && ds2 == 0)
    return 0;

  if (ds1 == 0 && ds2 != 0)
    return ds2;

  if (ds1 != 0 && ds2 == 0)
    return ds1;

  return ds_merge_1 (ds1, ds2, true);
}

/* Return the probability of speculation success for the speculation
   status DS.  */
dw_t
ds_weak (ds_t ds)
{
  ds_t res = 1, dt;
  int n = 0;

  dt = FIRST_SPEC_TYPE;
  do
    {
      if (ds & dt)
	{
	  res *= (ds_t) get_dep_weak (ds, dt);
	  n++;
	}

      if (dt == LAST_SPEC_TYPE)
	break;
      dt <<= SPEC_TYPE_SHIFT;
    }
  while (1);

  gcc_assert (n);
  while (--n)
    res /= MAX_DEP_WEAK;

  if (res < MIN_DEP_WEAK)
    res = MIN_DEP_WEAK;

  gcc_assert (res <= MAX_DEP_WEAK);

  return (dw_t) res;
}

/* Return a dep status that contains all speculation types of DS.  */
ds_t
ds_get_speculation_types (ds_t ds)
{
  if (ds & BEGIN_DATA)
    ds |= BEGIN_DATA;
  if (ds & BE_IN_DATA)
    ds |= BE_IN_DATA;
  if (ds & BEGIN_CONTROL)
    ds |= BEGIN_CONTROL;
  if (ds & BE_IN_CONTROL)
    ds |= BE_IN_CONTROL;

  return ds & SPECULATIVE;
}

/* Return a dep status that contains maximal weakness for each speculation
   type present in DS.  */
ds_t
ds_get_max_dep_weak (ds_t ds)
{
  if (ds & BEGIN_DATA)
    ds = set_dep_weak (ds, BEGIN_DATA, MAX_DEP_WEAK);
  if (ds & BE_IN_DATA)
    ds = set_dep_weak (ds, BE_IN_DATA, MAX_DEP_WEAK);
  if (ds & BEGIN_CONTROL)
    ds = set_dep_weak (ds, BEGIN_CONTROL, MAX_DEP_WEAK);
  if (ds & BE_IN_CONTROL)
    ds = set_dep_weak (ds, BE_IN_CONTROL, MAX_DEP_WEAK);

  return ds;
}

/* Dump information about the dependence status S.  */
static void
dump_ds (FILE *f, ds_t s)
{
  fprintf (f, "{");

  if (s & BEGIN_DATA)
    fprintf (f, "BEGIN_DATA: %d; ", get_dep_weak_1 (s, BEGIN_DATA));
  if (s & BE_IN_DATA)
    fprintf (f, "BE_IN_DATA: %d; ", get_dep_weak_1 (s, BE_IN_DATA));
  if (s & BEGIN_CONTROL)
    fprintf (f, "BEGIN_CONTROL: %d; ", get_dep_weak_1 (s, BEGIN_CONTROL));
  if (s & BE_IN_CONTROL)
    fprintf (f, "BE_IN_CONTROL: %d; ", get_dep_weak_1 (s, BE_IN_CONTROL));

  if (s & HARD_DEP)
    fprintf (f, "HARD_DEP; ");

  if (s & DEP_TRUE)
    fprintf (f, "DEP_TRUE; ");
  if (s & DEP_ANTI)
    fprintf (f, "DEP_ANTI; ");
  if (s & DEP_OUTPUT)
    fprintf (f, "DEP_OUTPUT; ");

  fprintf (f, "}");
}

void
debug_ds (ds_t s)
{
  dump_ds (stderr, s);
  fprintf (stderr, "\n");
}

#ifdef ENABLE_CHECKING
/* Verify that dependence type and status are consistent.
   If RELAXED_P is true, then skip dep_weakness checks.  */
static void
check_dep (dep_t dep, bool relaxed_p)
{
  enum reg_note dt = DEP_TYPE (dep);
  ds_t ds = DEP_STATUS (dep);

  gcc_assert (DEP_PRO (dep) != DEP_CON (dep));

  if (!(current_sched_info->flags & USE_DEPS_LIST))
    {
      gcc_assert (ds == -1);
      return;
    }

  /* Check that dependence type contains the same bits as the status.  */
  if (dt == REG_DEP_TRUE)
    gcc_assert (ds & DEP_TRUE);
  else if (dt == REG_DEP_OUTPUT)
    gcc_assert ((ds & DEP_OUTPUT)
		&& !(ds & DEP_TRUE));
  else
    gcc_assert ((dt == REG_DEP_ANTI)
		&& (ds & DEP_ANTI)
		&& !(ds & (DEP_OUTPUT | DEP_TRUE)));

  /* HARD_DEP can not appear in dep_status of a link.  */
  gcc_assert (!(ds & HARD_DEP));

  /* Check that dependence status is set correctly when speculation is not
     supported.  */
  if (!sched_deps_info->generate_spec_deps)
    gcc_assert (!(ds & SPECULATIVE));
  else if (ds & SPECULATIVE)
    {
      if (!relaxed_p)
	{
	  ds_t type = FIRST_SPEC_TYPE;

	  /* Check that dependence weakness is in proper range.  */
	  do
	    {
	      if (ds & type)
		get_dep_weak (ds, type);

	      if (type == LAST_SPEC_TYPE)
		break;
	      type <<= SPEC_TYPE_SHIFT;
	    }
	  while (1);
	}

      if (ds & BEGIN_SPEC)
	{
	  /* Only true dependence can be data speculative.  */
	  if (ds & BEGIN_DATA)
	    gcc_assert (ds & DEP_TRUE);

	  /* Control dependencies in the insn scheduler are represented by
	     anti-dependencies, therefore only anti dependence can be
	     control speculative.  */
	  if (ds & BEGIN_CONTROL)
	    gcc_assert (ds & DEP_ANTI);
	}
      else
	{
	  /* Subsequent speculations should resolve true dependencies.  */
	  gcc_assert ((ds & DEP_TYPES) == DEP_TRUE);
	}

      /* Check that true and anti dependencies can't have other speculative
	 statuses.  */
      if (ds & DEP_TRUE)
	gcc_assert (ds & (BEGIN_DATA | BE_IN_SPEC));
      /* An output dependence can't be speculative at all.  */
      gcc_assert (!(ds & DEP_OUTPUT));
      if (ds & DEP_ANTI)
	gcc_assert (ds & BEGIN_CONTROL);
    }
}
#endif /* ENABLE_CHECKING */

#endif /* INSN_SCHEDULING */
