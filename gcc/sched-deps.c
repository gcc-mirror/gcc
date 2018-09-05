/* Instruction scheduling pass.  This file computes dependencies between
   instructions.
   Copyright (C) 1992-2018 Free Software Foundation, Inc.
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
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "df.h"
#include "insn-config.h"
#include "regs.h"
#include "memmodel.h"
#include "ira.h"
#include "ira-int.h"
#include "insn-attr.h"
#include "cfgbuild.h"
#include "sched-int.h"
#include "params.h"
#include "cselib.h"

#ifdef INSN_SCHEDULING

/* Holds current parameters for the dependency analyzer.  */
struct sched_deps_info_def *sched_deps_info;

/* The data is specific to the Haifa scheduler.  */
vec<haifa_deps_insn_data_def>
    h_d_i_d = vNULL;

/* Return the major type present in the DS.  */
enum reg_note
ds_to_dk (ds_t ds)
{
  if (ds & DEP_TRUE)
    return REG_DEP_TRUE;

  if (ds & DEP_OUTPUT)
    return REG_DEP_OUTPUT;

  if (ds & DEP_CONTROL)
    return REG_DEP_CONTROL;

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

    case REG_DEP_CONTROL:
      return DEP_CONTROL;

    default:
      gcc_assert (dk == REG_DEP_ANTI);
      return DEP_ANTI;
    }
}

/* Functions to operate with dependence information container - dep_t.  */

/* Init DEP with the arguments.  */
void
init_dep_1 (dep_t dep, rtx_insn *pro, rtx_insn *con, enum reg_note type, ds_t ds)
{
  DEP_PRO (dep) = pro;
  DEP_CON (dep) = con;
  DEP_TYPE (dep) = type;
  DEP_STATUS (dep) = ds;
  DEP_COST (dep) = UNKNOWN_DEP_COST;
  DEP_NONREG (dep) = 0;
  DEP_MULTIPLE (dep) = 0;
  DEP_REPLACE (dep) = NULL;
}

/* Init DEP with the arguments.
   While most of the scheduler (including targets) only need the major type
   of the dependency, it is convenient to hide full dep_status from them.  */
void
init_dep (dep_t dep, rtx_insn *pro, rtx_insn *con, enum reg_note kind)
{
  ds_t ds;

  if ((current_sched_info->flags & USE_DEPS_LIST))
    ds = dk_to_ds (kind);
  else
    ds = 0;

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

	case REG_DEP_CONTROL:
	  t = 'c';
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
static object_allocator<_dep_node> *dn_pool;

/* Number of dep_nodes out there.  */
static int dn_pool_diff = 0;

/* Create a dep_node.  */
static dep_node_t
create_dep_node (void)
{
  dep_node_t n = dn_pool->allocate ();
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

  XDELETE (DEP_REPLACE (DEP_NODE_DEP (n)));

  --dn_pool_diff;

  dn_pool->remove (n);
}

/* Pool to hold dependencies lists (deps_list_t).  */
static object_allocator<_deps_list> *dl_pool;

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
  deps_list_t l = dl_pool->allocate ();

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

  dl_pool->remove (l);
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

/* Decide whether a dependency should be treated as a hard or a speculative
   dependency.  */
static bool
dep_spec_p (dep_t dep)
{
  if (current_sched_info->flags & DO_SPECULATION)
    {
      if (DEP_STATUS (dep) & SPECULATIVE)
	return true;
    }
  if (current_sched_info->flags & DO_PREDICATION)
    {
      if (DEP_TYPE (dep) == REG_DEP_CONTROL)
	return true;
    }
  if (DEP_REPLACE (dep) != NULL)
    return true;
  return false;
}

static regset reg_pending_sets;
static regset reg_pending_clobbers;
static regset reg_pending_uses;
static regset reg_pending_control_uses;
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
static bitmap_head *control_dependency_cache = NULL;
static bitmap_head *spec_dependency_cache = NULL;
static int cache_size;

/* True if we should mark added dependencies as a non-register deps.  */
static bool mark_as_hard;

static int deps_may_trap_p (const_rtx);
static void add_dependence_1 (rtx_insn *, rtx_insn *, enum reg_note);
static void add_dependence_list (rtx_insn *, rtx_insn_list *, int,
				 enum reg_note, bool);
static void add_dependence_list_and_free (struct deps_desc *, rtx_insn *,
					  rtx_insn_list **, int, enum reg_note,
					  bool);
static void delete_all_dependences (rtx_insn *);
static void chain_to_prev_insn (rtx_insn *);

static void flush_pending_lists (struct deps_desc *, rtx_insn *, int, int);
static void sched_analyze_1 (struct deps_desc *, rtx, rtx_insn *);
static void sched_analyze_2 (struct deps_desc *, rtx, rtx_insn *);
static void sched_analyze_insn (struct deps_desc *, rtx, rtx_insn *);

static bool sched_has_condition_p (const rtx_insn *);
static int conditions_mutex_p (const_rtx, const_rtx, bool, bool);

static enum DEPS_ADJUST_RESULT maybe_add_or_update_dep_1 (dep_t, bool,
							  rtx, rtx);
static enum DEPS_ADJUST_RESULT add_or_update_dep_1 (dep_t, bool, rtx, rtx);

static void check_dep (dep_t, bool);


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
sched_get_condition_with_rev_uncached (const rtx_insn *insn, bool *rev)
{
  rtx pat = PATTERN (insn);
  rtx src;

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

/* Return the condition under which INSN does not execute (i.e.  the
   not-taken condition for a conditional branch), or NULL if we cannot
   find such a condition.  The caller should make a copy of the condition
   before using it.  */
rtx
sched_get_reverse_condition_uncached (const rtx_insn *insn)
{
  bool rev;
  rtx cond = sched_get_condition_with_rev_uncached (insn, &rev);
  if (cond == NULL_RTX)
    return cond;
  if (!rev)
    {
      enum rtx_code revcode = reversed_comparison_code (cond, insn);
      cond = gen_rtx_fmt_ee (revcode, GET_MODE (cond),
			     XEXP (cond, 0),
			     XEXP (cond, 1));
    }
  return cond;
}

/* Caching variant of sched_get_condition_with_rev_uncached.
   We only do actual work the first time we come here for an insn; the
   results are cached in INSN_CACHED_COND and INSN_REVERSE_COND.  */
static rtx
sched_get_condition_with_rev (const rtx_insn *insn, bool *rev)
{
  bool tmp;

  if (INSN_LUID (insn) == 0)
    return sched_get_condition_with_rev_uncached (insn, rev);

  if (INSN_CACHED_COND (insn) == const_true_rtx)
    return NULL_RTX;

  if (INSN_CACHED_COND (insn) != NULL_RTX)
    {
      if (rev)
	*rev = INSN_REVERSE_COND (insn);
      return INSN_CACHED_COND (insn);
    }

  INSN_CACHED_COND (insn) = sched_get_condition_with_rev_uncached (insn, &tmp);
  INSN_REVERSE_COND (insn) = tmp;

  if (INSN_CACHED_COND (insn) == NULL_RTX)
    {
      INSN_CACHED_COND (insn) = const_true_rtx;
      return NULL_RTX;
    }

  if (rev)
    *rev = INSN_REVERSE_COND (insn);
  return INSN_CACHED_COND (insn);
}

/* True when we can find a condition under which INSN is executed.  */
static bool
sched_has_condition_p (const rtx_insn *insn)
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
      && rtx_equal_p (XEXP (cond1, 0), XEXP (cond2, 0))
      && XEXP (cond1, 1) == XEXP (cond2, 1))
    return 1;
  return 0;
}

/* Return true if insn1 and insn2 can never depend on one another because
   the conditions under which they are executed are mutually exclusive.  */
bool
sched_insns_conditions_mutex_p (const rtx_insn *insn1, const rtx_insn *insn2)
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
sched_insn_is_legitimate_for_speculation_p (const rtx_insn *insn, ds_t ds)
{
  if (HAS_INTERNAL_DEP (insn))
    return false;

  if (!NONJUMP_INSN_P (insn))
    return false;

  if (SCHED_GROUP_P (insn))
    return false;

  if (IS_SPECULATION_CHECK_P (CONST_CAST_RTX_INSN (insn)))
    return false;

  if (side_effects_p (PATTERN (insn)))
    return false;

  if (ds & BE_IN_SPEC)
    /* The following instructions, which depend on a speculatively scheduled
       instruction, cannot be speculatively scheduled along.  */
    {
      if (may_trap_or_fault_p (PATTERN (insn)))
	/* If instruction might fault, it cannot be speculatively scheduled.
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
sd_init_insn (rtx_insn *insn)
{
  INSN_HARD_BACK_DEPS (insn) = create_deps_list ();
  INSN_SPEC_BACK_DEPS (insn) = create_deps_list ();
  INSN_RESOLVED_BACK_DEPS (insn) = create_deps_list ();
  INSN_FORW_DEPS (insn) = create_deps_list ();
  INSN_RESOLVED_FORW_DEPS (insn) = create_deps_list ();

  /* ??? It would be nice to allocate dependency caches here.  */
}

/* Free data for INSN.  */
void
sd_finish_insn (rtx_insn *insn)
{
  /* ??? It would be nice to deallocate dependency caches here.  */

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

      if (!bitmap_bit_p (&true_dependency_cache[insn_luid], elem_luid)
	  && !bitmap_bit_p (&output_dependency_cache[insn_luid], elem_luid)
	  && !bitmap_bit_p (&anti_dependency_cache[insn_luid], elem_luid)
	  && !bitmap_bit_p (&control_dependency_cache[insn_luid], elem_luid))
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
  rtx_insn *elem = DEP_PRO (dep);
  rtx_insn *insn = DEP_CON (dep);

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
	      && anti_dependency_cache != NULL
	      && control_dependency_cache != NULL);

  if (!(current_sched_info->flags & USE_DEPS_LIST))
    {
      enum reg_note present_dep_type;

      if (bitmap_bit_p (&true_dependency_cache[insn_luid], elem_luid))
	present_dep_type = REG_DEP_TRUE;
      else if (bitmap_bit_p (&output_dependency_cache[insn_luid], elem_luid))
	present_dep_type = REG_DEP_OUTPUT;
      else if (bitmap_bit_p (&anti_dependency_cache[insn_luid], elem_luid))
	present_dep_type = REG_DEP_ANTI;
      else if (bitmap_bit_p (&control_dependency_cache[insn_luid], elem_luid))
	present_dep_type = REG_DEP_CONTROL;
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
      if (bitmap_bit_p (&control_dependency_cache[insn_luid], elem_luid))
	present_dep_types |= DEP_CONTROL;

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

	case REG_DEP_CONTROL:
	  bitmap_set_bit (&control_dependency_cache[insn_luid], elem_luid);
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
      if (ds & DEP_CONTROL)
	bitmap_set_bit (&control_dependency_cache[insn_luid], elem_luid);

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

	case REG_DEP_CONTROL:
	  bitmap_clear_bit (&control_dependency_cache[insn_luid], elem_luid);
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
  rtx_insn *elem = DEP_PRO (dep);
  rtx_insn *insn = DEP_CON (dep);

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
  bool was_spec = dep_spec_p (dep);

  DEP_NONREG (dep) |= DEP_NONREG (new_dep);
  DEP_MULTIPLE (dep) = 1;

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
	{
	  /* Either existing dep or a dep we're adding or both are
	     speculative.  */
	  if (!(ds & SPECULATIVE)
	      || !(dep_status & SPECULATIVE))
	    /* The new dep can't be speculative.  */
	    new_status &= ~SPECULATIVE;
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

  if (was_spec && !dep_spec_p (dep))
    /* The old dep was speculative, but now it isn't.  */
    change_spec_dep_to_hard (sd_it);

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

  if (flag_checking)
    check_dep (new_dep, mem1 != NULL);

  if (true_dependency_cache != NULL)
    {
      switch (ask_dependency_caches (new_dep))
	{
	case DEP_PRESENT:
	  dep_t present_dep;
	  sd_iterator_def sd_it;
      
	  present_dep = sd_find_dep_between_no_cache (DEP_PRO (new_dep),
						      DEP_CON (new_dep),
						      resolved_p, &sd_it);
	  DEP_MULTIPLE (present_dep) = 1;
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
  rtx_insn *con = DEP_CON (dep);

  if (!resolved_p)
    {
      if (dep_spec_p (dep))
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
  rtx_insn *elem = DEP_PRO (dep);
  rtx_insn *insn = DEP_CON (dep);

  gcc_assert (INSN_P (insn) && INSN_P (elem) && insn != elem);

  if ((current_sched_info->flags & DO_SPECULATION) == 0
      || !sched_insn_is_legitimate_for_speculation_p (insn, DEP_STATUS (dep)))
    DEP_STATUS (dep) &= ~SPECULATIVE;

  copy_dep (DEP_NODE_DEP (n), dep);

  get_back_and_forw_lists (dep, resolved_p, &con_back_deps, &pro_forw_deps);

  add_to_deps_list (DEP_NODE_BACK (n), con_back_deps);

  if (flag_checking)
    check_dep (dep, false);

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
  rtx_insn *pro = DEP_PRO (dep);
  rtx_insn *con = DEP_CON (dep);

  if (dep_spec_p (dep))
    move_dep_link (DEP_NODE_BACK (node), INSN_SPEC_BACK_DEPS (con),
		   INSN_RESOLVED_BACK_DEPS (con));
  else
    move_dep_link (DEP_NODE_BACK (node), INSN_HARD_BACK_DEPS (con),
		   INSN_RESOLVED_BACK_DEPS (con));

  move_dep_link (DEP_NODE_FORW (node), INSN_FORW_DEPS (pro),
		 INSN_RESOLVED_FORW_DEPS (pro));
}

/* Perform the inverse operation of sd_resolve_dep.  Restore the dependence
   pointed to by SD_IT to unresolved state.  */
void
sd_unresolve_dep (sd_iterator_def sd_it)
{
  dep_node_t node = DEP_LINK_NODE (*sd_it.linkp);
  dep_t dep = DEP_NODE_DEP (node);
  rtx_insn *pro = DEP_PRO (dep);
  rtx_insn *con = DEP_CON (dep);

  if (dep_spec_p (dep))
    move_dep_link (DEP_NODE_BACK (node), INSN_RESOLVED_BACK_DEPS (con),
		   INSN_SPEC_BACK_DEPS (con));
  else
    move_dep_link (DEP_NODE_BACK (node), INSN_RESOLVED_BACK_DEPS (con),
		   INSN_HARD_BACK_DEPS (con));

  move_dep_link (DEP_NODE_FORW (node), INSN_RESOLVED_FORW_DEPS (pro),
		 INSN_FORW_DEPS (pro));
}

/* Make TO depend on all the FROM's producers.
   If RESOLVED_P is true add dependencies to the resolved lists.  */
void
sd_copy_back_deps (rtx_insn *to, rtx_insn *from, bool resolved_p)
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
  rtx_insn *pro = DEP_PRO (dep);
  rtx_insn *con = DEP_CON (dep);
  deps_list_t con_back_deps;
  deps_list_t pro_forw_deps;

  if (true_dependency_cache != NULL)
    {
      int elem_luid = INSN_LUID (pro);
      int insn_luid = INSN_LUID (con);

      bitmap_clear_bit (&true_dependency_cache[insn_luid], elem_luid);
      bitmap_clear_bit (&anti_dependency_cache[insn_luid], elem_luid);
      bitmap_clear_bit (&control_dependency_cache[insn_luid], elem_luid);
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

/* A wrapper around add_dependence_1, to add a dependence of CON on
   PRO, with type DEP_TYPE.  This function implements special handling
   for REG_DEP_CONTROL dependencies.  For these, we optionally promote
   the type to REG_DEP_ANTI if we can determine that predication is
   impossible; otherwise we add additional true dependencies on the
   INSN_COND_DEPS list of the jump (which PRO must be).  */
void
add_dependence (rtx_insn *con, rtx_insn *pro, enum reg_note dep_type)
{
  if (dep_type == REG_DEP_CONTROL
      && !(current_sched_info->flags & DO_PREDICATION))
    dep_type = REG_DEP_ANTI;

  /* A REG_DEP_CONTROL dependence may be eliminated through predication,
     so we must also make the insn dependent on the setter of the
     condition.  */
  if (dep_type == REG_DEP_CONTROL)
    {
      rtx_insn *real_pro = pro;
      rtx_insn *other = real_insn_for_shadow (real_pro);
      rtx cond;

      if (other != NULL_RTX)
	real_pro = other;
      cond = sched_get_reverse_condition_uncached (real_pro);
      /* Verify that the insn does not use a different value in
	 the condition register than the one that was present at
	 the jump.  */
      if (cond == NULL_RTX)
	dep_type = REG_DEP_ANTI;
      else if (INSN_CACHED_COND (real_pro) == const_true_rtx)
	{
	  HARD_REG_SET uses;
	  CLEAR_HARD_REG_SET (uses);
	  note_uses (&PATTERN (con), record_hard_reg_uses, &uses);
	  if (TEST_HARD_REG_BIT (uses, REGNO (XEXP (cond, 0))))
	    dep_type = REG_DEP_ANTI;
	}
      if (dep_type == REG_DEP_CONTROL)
	{
	  if (sched_verbose >= 5)
	    fprintf (sched_dump, "making DEP_CONTROL for %d\n",
		     INSN_UID (real_pro));
	  add_dependence_list (con, INSN_COND_DEPS (real_pro), 0,
			       REG_DEP_TRUE, false);
	}
    }
	  
  add_dependence_1 (con, pro, dep_type);
}

/* A convenience wrapper to operate on an entire list.  HARD should be
   true if DEP_NONREG should be set on newly created dependencies.  */

static void
add_dependence_list (rtx_insn *insn, rtx_insn_list *list, int uncond,
		     enum reg_note dep_type, bool hard)
{
  mark_as_hard = hard;
  for (; list; list = list->next ())
    {
      if (uncond || ! sched_insns_conditions_mutex_p (insn, list->insn ()))
	add_dependence (insn, list->insn (), dep_type);
    }
  mark_as_hard = false;
}

/* Similar, but free *LISTP at the same time, when the context
   is not readonly.  HARD should be true if DEP_NONREG should be set on
   newly created dependencies.  */

static void
add_dependence_list_and_free (struct deps_desc *deps, rtx_insn *insn,
			      rtx_insn_list **listp,
                              int uncond, enum reg_note dep_type, bool hard)
{
  add_dependence_list (insn, *listp, uncond, dep_type, hard);

  /* We don't want to short-circuit dependencies involving debug
     insns, because they may cause actual dependencies to be
     disregarded.  */
  if (deps->readonly || DEBUG_INSN_P (insn))
    return;

  free_INSN_LIST_list (listp);
}

/* Remove all occurrences of INSN from LIST.  Return the number of
   occurrences removed.  */

static int
remove_from_dependence_list (rtx_insn *insn, rtx_insn_list **listp)
{
  int removed = 0;

  while (*listp)
    {
      if ((*listp)->insn () == insn)
        {
          remove_free_INSN_LIST_node (listp);
          removed++;
          continue;
        }

      listp = (rtx_insn_list **)&XEXP (*listp, 1);
    }

  return removed;
}

/* Same as above, but process two lists at once.  */
static int
remove_from_both_dependence_lists (rtx_insn *insn,
				   rtx_insn_list **listp,
				   rtx_expr_list **exprp)
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

      listp = (rtx_insn_list **)&XEXP (*listp, 1);
      exprp = (rtx_expr_list **)&XEXP (*exprp, 1);
    }

  return removed;
}

/* Clear all dependencies for an insn.  */
static void
delete_all_dependences (rtx_insn *insn)
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
chain_to_prev_insn (rtx_insn *insn)
{
  sd_iterator_def sd_it;
  dep_t dep;
  rtx_insn *prev_nonnote;

  FOR_EACH_DEP (insn, SD_LIST_BACK, sd_it, dep)
    {
      rtx_insn *i = insn;
      rtx_insn *pro = DEP_PRO (dep);

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

  prev_nonnote = prev_nonnote_nondebug_insn (insn);
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
add_insn_mem_dependence (struct deps_desc *deps, bool read_p,
			 rtx_insn *insn, rtx mem)
{
  rtx_insn_list **insn_list;
  rtx_insn_list *insn_node;
  rtx_expr_list **mem_list;
  rtx_expr_list *mem_node;

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

  insn_node = alloc_INSN_LIST (insn, *insn_list);
  *insn_list = insn_node;

  if (sched_deps_info->use_cselib)
    {
      mem = shallow_copy_rtx (mem);
      XEXP (mem, 0) = cselib_subst_to_values_from_insn (XEXP (mem, 0),
							GET_MODE (mem), insn);
    }
  mem_node = alloc_EXPR_LIST (VOIDmode, canon_rtx (mem), *mem_list);
  *mem_list = mem_node;
}

/* Make a dependency between every memory reference on the pending lists
   and INSN, thus flushing the pending lists.  FOR_READ is true if emitting
   dependencies for a read operation, similarly with FOR_WRITE.  */

static void
flush_pending_lists (struct deps_desc *deps, rtx_insn *insn, int for_read,
		     int for_write)
{
  if (for_write)
    {
      add_dependence_list_and_free (deps, insn, &deps->pending_read_insns,
                                    1, REG_DEP_ANTI, true);
      if (!deps->readonly)
        {
          free_EXPR_LIST_list (&deps->pending_read_mems);
          deps->pending_read_list_length = 0;
        }
    }

  add_dependence_list_and_free (deps, insn, &deps->pending_write_insns, 1,
				for_read ? REG_DEP_ANTI : REG_DEP_OUTPUT,
				true);

  add_dependence_list_and_free (deps, insn,
                                &deps->last_pending_memory_flush, 1,
                                for_read ? REG_DEP_ANTI : REG_DEP_OUTPUT,
				true);

  add_dependence_list_and_free (deps, insn, &deps->pending_jump_insns, 1,
				REG_DEP_ANTI, true);

  if (DEBUG_INSN_P (insn))
    {
      if (for_write)
	free_INSN_LIST_list (&deps->pending_read_insns);
      free_INSN_LIST_list (&deps->pending_write_insns);
      free_INSN_LIST_list (&deps->last_pending_memory_flush);
      free_INSN_LIST_list (&deps->pending_jump_insns);
    }

  if (!deps->readonly)
    {
      free_EXPR_LIST_list (&deps->pending_write_mems);
      deps->pending_write_list_length = 0;

      deps->last_pending_memory_flush = alloc_INSN_LIST (insn, NULL_RTX);
      deps->pending_flush_length = 1;
    }
  mark_as_hard = false;
}

/* Instruction which dependencies we are analyzing.  */
static rtx_insn *cur_insn = NULL;

/* Implement hooks for haifa scheduler.  */

static void
haifa_start_insn (rtx_insn *insn)
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
haifa_note_mem_dep (rtx mem, rtx pending_mem, rtx_insn *pending_insn, ds_t ds)
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
                current_sched_info->flags & USE_DEPS_LIST ? ds : 0);
    DEP_NONREG (dep) = 1;
    maybe_add_or_update_dep_1 (dep, false, pending_mem, mem);
  }

}

static void
haifa_note_dep (rtx_insn *elem, ds_t ds)
{
  dep_def _dep;
  dep_t dep = &_dep;

  init_dep (dep, elem, cur_insn, ds_to_dt (ds));
  if (mark_as_hard)
    DEP_NONREG (dep) = 1;
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
note_mem_dep (rtx m1, rtx m2, rtx_insn *e, ds_t ds)
{
  if (sched_deps_info->note_mem_dep)
    sched_deps_info->note_mem_dep (m1, m2, e, ds);
}

static void
note_dep (rtx_insn *e, ds_t ds)
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
  else if (ds & DEP_ANTI)
    return REG_DEP_ANTI;
  else
    {
      gcc_assert (ds & DEP_CONTROL);
      return REG_DEP_CONTROL;
    }
}



/* Functions for computation of info needed for register pressure
   sensitive insn scheduling.  */


/* Allocate and return reg_use_data structure for REGNO and INSN.  */
static struct reg_use_data *
create_insn_reg_use (int regno, rtx_insn *insn)
{
  struct reg_use_data *use;

  use = (struct reg_use_data *) xmalloc (sizeof (struct reg_use_data));
  use->regno = regno;
  use->insn = insn;
  use->next_insn_use = INSN_REG_USE_LIST (insn);
  INSN_REG_USE_LIST (insn) = use;
  return use;
}

/* Allocate reg_set_data structure for REGNO and INSN.  */
static void
create_insn_reg_set (int regno, rtx insn)
{
  struct reg_set_data *set;

  set = (struct reg_set_data *) xmalloc (sizeof (struct reg_set_data));
  set->regno = regno;
  set->insn = insn;
  set->next_insn_set = INSN_REG_SET_LIST (insn);
  INSN_REG_SET_LIST (insn) = set;
}

/* Set up insn register uses for INSN and dependency context DEPS.  */
static void
setup_insn_reg_uses (struct deps_desc *deps, rtx_insn *insn)
{
  unsigned i;
  reg_set_iterator rsi;
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
      for (rtx_insn_list *list = reg_last->uses; list; list = list->next ())
	{
	  use2 = create_insn_reg_use (i, list->insn ());
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
  cl = sched_regno_pressure_class[regno];
  if (cl != NO_REGS)
    {
      incr = ira_reg_class_max_nregs[cl][PSEUDO_REGNO_MODE (regno)];
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
	  cl = sched_regno_pressure_class[regno];
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
    mark_insn_hard_regno_birth (insn, regno, REG_NREGS (reg),
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
  cl = sched_regno_pressure_class[regno];
  if (cl != NO_REGS)
    {
      incr = ira_reg_class_max_nregs[cl][PSEUDO_REGNO_MODE (regno)];
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
	  cl = sched_regno_pressure_class[regno];
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
    mark_hard_regno_death (regno, REG_NREGS (reg));
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
void
init_insn_reg_pressure_info (rtx_insn *insn)
{
  int i, len;
  enum reg_class cl;
  static struct reg_pressure_data *pressure_info;
  rtx link;

  gcc_assert (sched_pressure != SCHED_PRESSURE_NONE);

  if (! INSN_P (insn))
    return;

  for (i = 0; i < ira_pressure_classes_num; i++)
    {
      cl = ira_pressure_classes[i];
      reg_pressure_info[cl].clobber_increase = 0;
      reg_pressure_info[cl].set_increase = 0;
      reg_pressure_info[cl].unused_set_increase = 0;
      reg_pressure_info[cl].change = 0;
    }

  note_stores (PATTERN (insn), mark_insn_reg_clobber, insn);

  note_stores (PATTERN (insn), mark_insn_reg_store, insn);

  if (AUTO_INC_DEC)
    for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
      if (REG_NOTE_KIND (link) == REG_INC)
	mark_insn_reg_store (XEXP (link, 0), NULL_RTX, insn);

  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
    if (REG_NOTE_KIND (link) == REG_DEAD)
      mark_reg_death (XEXP (link, 0));

  len = sizeof (struct reg_pressure_data) * ira_pressure_classes_num;
  pressure_info
    = INSN_REG_PRESSURE (insn) = (struct reg_pressure_data *) xmalloc (len);
  if (sched_pressure == SCHED_PRESSURE_WEIGHTED)
    INSN_MAX_REG_PRESSURE (insn) = (int *) xcalloc (ira_pressure_classes_num
						    * sizeof (int), 1);
  for (i = 0; i < ira_pressure_classes_num; i++)
    {
      cl = ira_pressure_classes[i];
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
extend_deps_reg_info (struct deps_desc *deps, int regno)
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
sched_analyze_reg (struct deps_desc *deps, int regno, machine_mode mode,
		   enum rtx_code ref, rtx_insn *insn)
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
      int i = hard_regno_nregs (regno, mode);
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
      else if (ref == CLOBBER_HIGH)
	{
	  gcc_assert (i == 1);
	  /* We don't know the current state of the register, so have to treat
	     the clobber high as a full clobber.  */
	  note_reg_clobber (regno);
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
	/* For CLOBBER_HIGH, we don't know the current state of the register,
	   so have to treat it as a full clobber.  */
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
				 REG_DEP_ANTI, false);
	}
    }
}

/* Analyze a single SET, CLOBBER, PRE_DEC, POST_DEC, PRE_INC or POST_INC
   rtx, X, creating all dependencies generated by the write to the
   destination of X, and reads of everything mentioned.  */

static void
sched_analyze_1 (struct deps_desc *deps, rtx x, rtx_insn *insn)
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
      int regno = REGNO (dest);
      machine_mode mode = GET_MODE (dest);

      sched_analyze_reg (deps, regno, mode, code, insn);

#ifdef STACK_REGS
      /* Treat all writes to a stack register as modifying the TOS.  */
      if (regno >= FIRST_STACK_REG && regno <= LAST_STACK_REG)
	{
	  /* Avoid analyzing the same register twice.  */
	  if (regno != FIRST_STACK_REG)
	    sched_analyze_reg (deps, FIRST_STACK_REG, mode, code, insn);

	  add_to_hard_reg_set (&implicit_reg_pending_uses, mode,
			       FIRST_STACK_REG);
	}
#endif
    }
  else if (MEM_P (dest))
    {
      /* Writing memory.  */
      rtx t = dest;

      if (sched_deps_info->use_cselib)
	{
	  machine_mode address_mode = get_address_mode (dest);

	  t = shallow_copy_rtx (dest);
	  cselib_lookup_from_insn (XEXP (t, 0), address_mode, 1,
				   GET_MODE (t), insn);
	  XEXP (t, 0)
	    = cselib_subst_to_values_from_insn (XEXP (t, 0), GET_MODE (t),
						insn);
	}
      t = canon_rtx (t);

      /* Pending lists can't get larger with a readonly context.  */
      if (!deps->readonly
          && ((deps->pending_read_list_length + deps->pending_write_list_length)
              >= MAX_PENDING_LIST_LENGTH))
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
	  rtx_insn_list *pending;
	  rtx_expr_list *pending_mem;

	  pending = deps->pending_read_insns;
	  pending_mem = deps->pending_read_mems;
	  while (pending)
	    {
	      if (anti_dependence (pending_mem->element (), t)
		  && ! sched_insns_conditions_mutex_p (insn, pending->insn ()))
		note_mem_dep (t, pending_mem->element (), pending->insn (),
			      DEP_ANTI);

	      pending = pending->next ();
	      pending_mem = pending_mem->next ();
	    }

	  pending = deps->pending_write_insns;
	  pending_mem = deps->pending_write_mems;
	  while (pending)
	    {
	      if (output_dependence (pending_mem->element (), t)
		  && ! sched_insns_conditions_mutex_p (insn, pending->insn ()))
		note_mem_dep (t, pending_mem->element (),
			      pending->insn (),
			      DEP_OUTPUT);

	      pending = pending->next ();
	      pending_mem = pending_mem-> next ();
	    }

	  add_dependence_list (insn, deps->last_pending_memory_flush, 1,
			       REG_DEP_ANTI, true);
	  add_dependence_list (insn, deps->pending_jump_insns, 1,
			       REG_DEP_CONTROL, true);

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
sched_analyze_2 (struct deps_desc *deps, rtx x, rtx_insn *insn)
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
    CASE_CONST_ANY:
    case SYMBOL_REF:
    case CONST:
    case LABEL_REF:
      /* Ignore constants.  */
      if (cslr_p && sched_deps_info->finish_rhs)
	sched_deps_info->finish_rhs ();

      return;

    case CC0:
      if (!HAVE_cc0)
	gcc_unreachable ();

      /* User of CC0 depends on immediately preceding insn.  */
      SCHED_GROUP_P (insn) = 1;
       /* Don't move CC0 setter to another block (it can set up the
        same flag for previous CC0 users which is safe).  */
      CANT_MOVE (prev_nonnote_insn (insn)) = 1;

      if (cslr_p && sched_deps_info->finish_rhs)
	sched_deps_info->finish_rhs ();

      return;

    case REG:
      {
	int regno = REGNO (x);
	machine_mode mode = GET_MODE (x);

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
	rtx_insn_list *u;
	rtx_insn_list *pending;
	rtx_expr_list *pending_mem;
	rtx t = x;

	if (sched_deps_info->use_cselib)
	  {
	    machine_mode address_mode = get_address_mode (t);

	    t = shallow_copy_rtx (t);
	    cselib_lookup_from_insn (XEXP (t, 0), address_mode, 1,
				     GET_MODE (t), insn);
	    XEXP (t, 0)
	      = cselib_subst_to_values_from_insn (XEXP (t, 0), GET_MODE (t),
						  insn);
	  }

	if (!DEBUG_INSN_P (insn))
	  {
	    t = canon_rtx (t);
	    pending = deps->pending_read_insns;
	    pending_mem = deps->pending_read_mems;
	    while (pending)
	      {
		if (read_dependence (pending_mem->element (), t)
		    && ! sched_insns_conditions_mutex_p (insn,
							 pending->insn ()))
		  note_mem_dep (t, pending_mem->element (),
				pending->insn (),
				DEP_ANTI);

		pending = pending->next ();
		pending_mem = pending_mem->next ();
	      }

	    pending = deps->pending_write_insns;
	    pending_mem = deps->pending_write_mems;
	    while (pending)
	      {
		if (true_dependence (pending_mem->element (), VOIDmode, t)
		    && ! sched_insns_conditions_mutex_p (insn,
							 pending->insn ()))
		  note_mem_dep (t, pending_mem->element (),
				pending->insn (),
				sched_deps_info->generate_spec_deps
				? BEGIN_DATA | DEP_TRUE : DEP_TRUE);

		pending = pending->next ();
		pending_mem = pending_mem->next ();
	      }

	    for (u = deps->last_pending_memory_flush; u; u = u->next ())
	      add_dependence (insn, u->insn (), REG_DEP_ANTI);

	    for (u = deps->pending_jump_insns; u; u = u->next ())
	      if (deps_may_trap_p (x))
		{
		  if ((sched_deps_info->generate_spec_deps)
		      && sel_sched_p () && (spec_info->mask & BEGIN_CONTROL))
		    {
		      ds_t ds = set_dep_weak (DEP_ANTI, BEGIN_CONTROL,
					      MAX_DEP_WEAK);
		      
		      note_dep (u->insn (), ds);
		    }
		  else
		    add_dependence (insn, u->insn (), REG_DEP_CONTROL);
		}
	  }

	/* Always add these dependencies to pending_reads, since
	   this insn may be followed by a write.  */
	if (!deps->readonly)
	  {
	    if ((deps->pending_read_list_length
		 + deps->pending_write_list_length)
		>= MAX_PENDING_LIST_LENGTH
		&& !DEBUG_INSN_P (insn))
	      flush_pending_lists (deps, insn, true, true);
	    add_insn_mem_dependence (deps, true, insn, x);
	  }

	sched_analyze_2 (deps, XEXP (x, 0), insn);

	if (cslr_p && sched_deps_info->finish_rhs)
	  sched_deps_info->finish_rhs ();

	return;
      }

    /* Force pending stores to memory in case a trap handler needs them.
       Also force pending loads from memory; loads and stores can segfault
       and the signal handler won't be triggered if the trap insn was moved
       above load or store insn.  */
    case TRAP_IF:
      flush_pending_lists (deps, insn, true, true);
      break;

    case PREFETCH:
      if (PREFETCH_SCHEDULE_BARRIER_P (x))
	reg_pending_barrier = TRUE_BARRIER;
      /* Prefetch insn contains addresses only.  So if the prefetch
	 address has no registers, there will be no dependencies on
	 the prefetch insn.  This is wrong with result code
	 correctness point of view as such prefetch can be moved below
	 a jump insn which usually generates MOVE_BARRIER preventing
	 to move insns containing registers or memories through the
	 barrier.  It is also wrong with generated code performance
	 point of view as prefetch withouth dependecies will have a
	 tendency to be issued later instead of earlier.  It is hard
	 to generate accurate dependencies for prefetch insns as
	 prefetch has only the start address but it is better to have
	 something than nothing.  */
      if (!deps->readonly)
	{
	  rtx x = gen_rtx_MEM (Pmode, XEXP (PATTERN (insn), 0));
	  if (sched_deps_info->use_cselib)
	    cselib_lookup_from_insn (x, Pmode, true, VOIDmode, insn);
	  add_insn_mem_dependence (deps, true, insn, x);
	}
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
	if ((code != ASM_OPERANDS || MEM_VOLATILE_P (x))
	    && !DEBUG_INSN_P (insn))
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

/* Try to group two fusible insns together to prevent scheduler
   from scheduling them apart.  */

static void
sched_macro_fuse_insns (rtx_insn *insn)
{
  rtx_insn *prev;
  /* No target hook would return true for debug insn as any of the
     hook operand, and with very large sequences of only debug insns
     where on each we call sched_macro_fuse_insns it has quadratic
     compile time complexity.  */
  if (DEBUG_INSN_P (insn))
    return;
  prev = prev_nonnote_nondebug_insn (insn);
  if (!prev)
    return;

  if (any_condjump_p (insn))
    {
      unsigned int condreg1, condreg2;
      rtx cc_reg_1;
      targetm.fixed_condition_code_regs (&condreg1, &condreg2);
      cc_reg_1 = gen_rtx_REG (CCmode, condreg1);
      if (reg_referenced_p (cc_reg_1, PATTERN (insn))
	  && modified_in_p (cc_reg_1, prev))
	{
	  if (targetm.sched.macro_fusion_pair_p (prev, insn))
	    SCHED_GROUP_P (insn) = 1;
	  return;
	}
    }

  if (single_set (insn) && single_set (prev))
    {
      if (targetm.sched.macro_fusion_pair_p (prev, insn))
	SCHED_GROUP_P (insn) = 1;
    }
}

/* Get the implicit reg pending clobbers for INSN and save them in TEMP.  */
void
get_implicit_reg_pending_clobbers (HARD_REG_SET *temp, rtx_insn *insn)
{
  extract_insn (insn);
  preprocess_constraints (insn);
  alternative_mask preferred = get_preferred_alternatives (insn);
  ira_implicitly_set_insn_hard_regs (temp, preferred);
  AND_COMPL_HARD_REG_SET (*temp, ira_no_alloc_regs);
}

/* Analyze an INSN with pattern X to find all dependencies.  */
static void
sched_analyze_insn (struct deps_desc *deps, rtx x, rtx_insn *insn)
{
  RTX_CODE code = GET_CODE (x);
  rtx link;
  unsigned i;
  reg_set_iterator rsi;

  if (! reload_completed)
    {
      HARD_REG_SET temp;
      get_implicit_reg_pending_clobbers (&temp, insn);
      IOR_HARD_REG_SET (implicit_reg_pending_clobbers, temp);
    }

  can_start_lhs_rhs_p = (NONJUMP_INSN_P (insn)
			 && code == SET);

  /* Group compare and branch insns for macro-fusion.  */
  if (!deps->readonly
      && targetm.sched.macro_fusion_p
      && targetm.sched.macro_fusion_p ())
    sched_macro_fuse_insns (insn);

  if (may_trap_p (x))
    /* Avoid moving trapping instructions across function calls that might
       not always return.  */
    add_dependence_list (insn, deps->last_function_call_may_noreturn,
			 1, REG_DEP_ANTI, true);

  /* We must avoid creating a situation in which two successors of the
     current block have different unwind info after scheduling.  If at any
     point the two paths re-join this leads to incorrect unwind info.  */
  /* ??? There are certain situations involving a forced frame pointer in
     which, with extra effort, we could fix up the unwind info at a later
     CFG join.  However, it seems better to notice these cases earlier
     during prologue generation and avoid marking the frame pointer setup
     as frame-related at all.  */
  if (RTX_FRAME_RELATED_P (insn))
    {
      /* Make sure prologue insn is scheduled before next jump.  */
      deps->sched_before_next_jump
	= alloc_INSN_LIST (insn, deps->sched_before_next_jump);

      /* Make sure epilogue insn is scheduled after preceding jumps.  */
      add_dependence_list (insn, deps->last_pending_memory_flush, 1,
			   REG_DEP_ANTI, true);
      add_dependence_list (insn, deps->pending_jump_insns, 1, REG_DEP_ANTI,
			   true);
    }

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
			     REG_DEP_OUTPUT, true);
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
	  else if (code == SET || code == CLOBBER || code == CLOBBER_HIGH)
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
	  else if (GET_CODE (XEXP (link, 0)) == CLOBBER_HIGH)
	    /* We could support CLOBBER_HIGH and treat it in the same way as
	      HARD_REGNO_CALL_PART_CLOBBERED, but no port needs that yet.  */
	    gcc_unreachable ();
	  else if (GET_CODE (XEXP (link, 0)) != SET)
	    sched_analyze_2 (deps, XEXP (link, 0), insn);
	}
      /* Don't schedule anything after a tail call, tail call needs
	 to use at least all call-saved registers.  */
      if (SIBLING_CALL_P (insn))
	reg_pending_barrier = TRUE_BARRIER;
      else if (find_reg_note (insn, REG_SETJMP, NULL))
	reg_pending_barrier = MOVE_BARRIER;
    }

  if (JUMP_P (insn))
    {
      rtx_insn *next = next_nonnote_nondebug_insn (insn);
      if (next && BARRIER_P (next))
	reg_pending_barrier = MOVE_BARRIER;
      else
	{
	  rtx_insn_list *pending;
	  rtx_expr_list *pending_mem;

          if (sched_deps_info->compute_jump_reg_dependencies)
            {
              (*sched_deps_info->compute_jump_reg_dependencies)
		(insn, reg_pending_control_uses);

              /* Make latency of jump equal to 0 by using anti-dependence.  */
              EXECUTE_IF_SET_IN_REG_SET (reg_pending_control_uses, 0, i, rsi)
                {
                  struct deps_reg *reg_last = &deps->reg_last[i];
                  add_dependence_list (insn, reg_last->sets, 0, REG_DEP_ANTI,
				       false);
                  add_dependence_list (insn, reg_last->implicit_sets,
				       0, REG_DEP_ANTI, false);
                  add_dependence_list (insn, reg_last->clobbers, 0,
				       REG_DEP_ANTI, false);
                }
            }

	  /* All memory writes and volatile reads must happen before the
	     jump.  Non-volatile reads must happen before the jump iff
	     the result is needed by the above register used mask.  */

	  pending = deps->pending_write_insns;
	  pending_mem = deps->pending_write_mems;
	  while (pending)
	    {
	      if (! sched_insns_conditions_mutex_p (insn, pending->insn ()))
		add_dependence (insn, pending->insn (),
				REG_DEP_OUTPUT);
	      pending = pending->next ();
	      pending_mem = pending_mem->next ();
	    }

	  pending = deps->pending_read_insns;
	  pending_mem = deps->pending_read_mems;
	  while (pending)
	    {
	      if (MEM_VOLATILE_P (pending_mem->element ())
		  && ! sched_insns_conditions_mutex_p (insn, pending->insn ()))
		add_dependence (insn, pending->insn (),
				REG_DEP_OUTPUT);
	      pending = pending->next ();
	      pending_mem = pending_mem->next ();
	    }

	  add_dependence_list (insn, deps->last_pending_memory_flush, 1,
			       REG_DEP_ANTI, true);
	  add_dependence_list (insn, deps->pending_jump_insns, 1,
			       REG_DEP_ANTI, true);
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

  if (sched_pressure != SCHED_PRESSURE_NONE)
    {
      setup_insn_reg_uses (deps, insn);
      init_insn_reg_pressure_info (insn);
    }

  /* Add register dependencies for insn.  */
  if (DEBUG_INSN_P (insn))
    {
      rtx_insn *prev = deps->last_debug_insn;
      rtx_insn_list *u;

      if (!deps->readonly)
	deps->last_debug_insn = insn;

      if (prev)
	add_dependence (insn, prev, REG_DEP_ANTI);

      add_dependence_list (insn, deps->last_function_call, 1,
			   REG_DEP_ANTI, false);

      if (!sel_sched_p ())
	for (u = deps->last_pending_memory_flush; u; u = u->next ())
	  add_dependence (insn, u->insn (), REG_DEP_ANTI);

      EXECUTE_IF_SET_IN_REG_SET (reg_pending_uses, 0, i, rsi)
	{
	  struct deps_reg *reg_last = &deps->reg_last[i];
	  add_dependence_list (insn, reg_last->sets, 1, REG_DEP_ANTI, false);
	  /* There's no point in making REG_DEP_CONTROL dependencies for
	     debug insns.  */
	  add_dependence_list (insn, reg_last->clobbers, 1, REG_DEP_ANTI,
			       false);

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
      regset_head set_or_clobbered;

      EXECUTE_IF_SET_IN_REG_SET (reg_pending_uses, 0, i, rsi)
	{
	  struct deps_reg *reg_last = &deps->reg_last[i];
	  add_dependence_list (insn, reg_last->sets, 0, REG_DEP_TRUE, false);
	  add_dependence_list (insn, reg_last->implicit_sets, 0, REG_DEP_ANTI,
			       false);
	  add_dependence_list (insn, reg_last->clobbers, 0, REG_DEP_TRUE,
			       false);

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
	    add_dependence_list (insn, reg_last->sets, 0, REG_DEP_TRUE, false);
	    add_dependence_list (insn, reg_last->implicit_sets, 0,
				 REG_DEP_ANTI, false);
	    add_dependence_list (insn, reg_last->clobbers, 0, REG_DEP_TRUE,
				 false);

	    if (!deps->readonly)
	      {
		reg_last->uses = alloc_INSN_LIST (insn, reg_last->uses);
		reg_last->uses_length++;
	      }
	  }

      if (targetm.sched.exposed_pipeline)
	{
	  INIT_REG_SET (&set_or_clobbered);
	  bitmap_ior (&set_or_clobbered, reg_pending_clobbers,
		      reg_pending_sets);
	  EXECUTE_IF_SET_IN_REG_SET (&set_or_clobbered, 0, i, rsi)
	    {
	      struct deps_reg *reg_last = &deps->reg_last[i];
	      rtx list;
	      for (list = reg_last->uses; list; list = XEXP (list, 1))
		{
		  rtx other = XEXP (list, 0);
		  if (INSN_CACHED_COND (other) != const_true_rtx
		      && refers_to_regno_p (i, INSN_CACHED_COND (other)))
		    INSN_CACHED_COND (other) = const_true_rtx;
		}
	    }
	}

      /* If the current insn is conditional, we can't free any
	 of the lists.  */
      if (sched_has_condition_p (insn))
	{
	  EXECUTE_IF_SET_IN_REG_SET (reg_pending_clobbers, 0, i, rsi)
	    {
	      struct deps_reg *reg_last = &deps->reg_last[i];
	      add_dependence_list (insn, reg_last->sets, 0, REG_DEP_OUTPUT,
				   false);
	      add_dependence_list (insn, reg_last->implicit_sets, 0,
				   REG_DEP_ANTI, false);
	      add_dependence_list (insn, reg_last->uses, 0, REG_DEP_ANTI,
				   false);
	      add_dependence_list (insn, reg_last->control_uses, 0,
				   REG_DEP_CONTROL, false);

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
	      add_dependence_list (insn, reg_last->sets, 0, REG_DEP_OUTPUT,
				   false);
	      add_dependence_list (insn, reg_last->implicit_sets, 0,
				   REG_DEP_ANTI, false);
	      add_dependence_list (insn, reg_last->clobbers, 0, REG_DEP_OUTPUT,
				   false);
	      add_dependence_list (insn, reg_last->uses, 0, REG_DEP_ANTI,
				   false);
	      add_dependence_list (insn, reg_last->control_uses, 0,
				   REG_DEP_CONTROL, false);

	      if (!deps->readonly)
		reg_last->sets = alloc_INSN_LIST (insn, reg_last->sets);
	    }
	}
      else
	{
	  EXECUTE_IF_SET_IN_REG_SET (reg_pending_clobbers, 0, i, rsi)
	    {
	      struct deps_reg *reg_last = &deps->reg_last[i];
	      if (reg_last->uses_length >= MAX_PENDING_LIST_LENGTH
		  || reg_last->clobbers_length >= MAX_PENDING_LIST_LENGTH)
		{
		  add_dependence_list_and_free (deps, insn, &reg_last->sets, 0,
						REG_DEP_OUTPUT, false);
		  add_dependence_list_and_free (deps, insn,
						&reg_last->implicit_sets, 0,
						REG_DEP_ANTI, false);
		  add_dependence_list_and_free (deps, insn, &reg_last->uses, 0,
						REG_DEP_ANTI, false);
		  add_dependence_list_and_free (deps, insn,
						&reg_last->control_uses, 0,
						REG_DEP_ANTI, false);
		  add_dependence_list_and_free (deps, insn,
						&reg_last->clobbers, 0,
						REG_DEP_OUTPUT, false);

		  if (!deps->readonly)
		    {
		      reg_last->sets = alloc_INSN_LIST (insn, reg_last->sets);
		      reg_last->clobbers_length = 0;
		      reg_last->uses_length = 0;
		    }
		}
	      else
		{
		  add_dependence_list (insn, reg_last->sets, 0, REG_DEP_OUTPUT,
				       false);
		  add_dependence_list (insn, reg_last->implicit_sets, 0,
				       REG_DEP_ANTI, false);
		  add_dependence_list (insn, reg_last->uses, 0, REG_DEP_ANTI,
				       false);
		  add_dependence_list (insn, reg_last->control_uses, 0,
				       REG_DEP_CONTROL, false);
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
					    REG_DEP_OUTPUT, false);
	      add_dependence_list_and_free (deps, insn,
					    &reg_last->implicit_sets,
					    0, REG_DEP_ANTI, false);
	      add_dependence_list_and_free (deps, insn, &reg_last->clobbers, 0,
					    REG_DEP_OUTPUT, false);
	      add_dependence_list_and_free (deps, insn, &reg_last->uses, 0,
					    REG_DEP_ANTI, false);
	      add_dependence_list (insn, reg_last->control_uses, 0,
				   REG_DEP_CONTROL, false);

	      if (!deps->readonly)
		{
		  reg_last->sets = alloc_INSN_LIST (insn, reg_last->sets);
		  reg_last->uses_length = 0;
		  reg_last->clobbers_length = 0;
		}
	    }
	}
      if (!deps->readonly)
	{
	  EXECUTE_IF_SET_IN_REG_SET (reg_pending_control_uses, 0, i, rsi)
	    {
	      struct deps_reg *reg_last = &deps->reg_last[i];
	      reg_last->control_uses
		= alloc_INSN_LIST (insn, reg_last->control_uses);
	    }
	}
    }

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (TEST_HARD_REG_BIT (implicit_reg_pending_clobbers, i))
      {
	struct deps_reg *reg_last = &deps->reg_last[i];
	add_dependence_list (insn, reg_last->sets, 0, REG_DEP_ANTI, false);
	add_dependence_list (insn, reg_last->clobbers, 0, REG_DEP_ANTI, false);
	add_dependence_list (insn, reg_last->uses, 0, REG_DEP_ANTI, false);
	add_dependence_list (insn, reg_last->control_uses, 0, REG_DEP_ANTI,
			     false);

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
  CLEAR_REG_SET (reg_pending_control_uses);
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
	      add_dependence_list (insn, reg_last->uses, 0, REG_DEP_ANTI,
				   true);
	      add_dependence_list (insn, reg_last->sets, 0,
				   reg_pending_barrier == TRUE_BARRIER
				   ? REG_DEP_TRUE : REG_DEP_ANTI, true);
	      add_dependence_list (insn, reg_last->implicit_sets, 0,
				   REG_DEP_ANTI, true);
	      add_dependence_list (insn, reg_last->clobbers, 0,
				   reg_pending_barrier == TRUE_BARRIER
				   ? REG_DEP_TRUE : REG_DEP_ANTI, true);
	    }
	}
      else
	{
	  EXECUTE_IF_SET_IN_REG_SET (&deps->reg_last_in_use, 0, i, rsi)
	    {
	      struct deps_reg *reg_last = &deps->reg_last[i];
	      add_dependence_list_and_free (deps, insn, &reg_last->uses, 0,
					    REG_DEP_ANTI, true);
	      add_dependence_list_and_free (deps, insn,
					    &reg_last->control_uses, 0,
					    REG_DEP_CONTROL, true);
	      add_dependence_list_and_free (deps, insn, &reg_last->sets, 0,
					    reg_pending_barrier == TRUE_BARRIER
					    ? REG_DEP_TRUE : REG_DEP_ANTI,
					    true);
	      add_dependence_list_and_free (deps, insn,
					    &reg_last->implicit_sets, 0,
					    REG_DEP_ANTI, true);
	      add_dependence_list_and_free (deps, insn, &reg_last->clobbers, 0,
					    reg_pending_barrier == TRUE_BARRIER
					    ? REG_DEP_TRUE : REG_DEP_ANTI,
					    true);

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

      /* Don't flush pending lists on speculative checks for
	 selective scheduling.  */
      if (!sel_sched_p () || !sel_insn_is_speculation_check (insn))
	flush_pending_lists (deps, insn, true, true);

      reg_pending_barrier = NOT_A_BARRIER;
    }

  /* If a post-call group is still open, see if it should remain so.
     This insn must be a simple move of a hard reg to a pseudo or
     vice-versa.

     We must avoid moving these insns for correctness on targets
     with small register classes, and for special registers like
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

	       Also, if we did, chain_to_prev_insn would move the
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

  /* We do not yet have code to adjust REG_ARGS_SIZE, therefore we must
     honor their original ordering.  */
  if (find_reg_note (insn, REG_ARGS_SIZE, NULL))
    {
      if (deps->last_args_size)
	add_dependence (insn, deps->last_args_size, REG_DEP_OUTPUT);
      if (!deps->readonly)
	deps->last_args_size = insn;
    }

  /* We must not mix prologue and epilogue insns.  See PR78029.  */
  if (prologue_contains (insn))
    {
      add_dependence_list (insn, deps->last_epilogue, true, REG_DEP_ANTI, true);
      if (!deps->readonly)
	{
	  if (deps->last_logue_was_epilogue)
	    free_INSN_LIST_list (&deps->last_prologue);
	  deps->last_prologue = alloc_INSN_LIST (insn, deps->last_prologue);
	  deps->last_logue_was_epilogue = false;
	}
    }

  if (epilogue_contains (insn))
    {
      add_dependence_list (insn, deps->last_prologue, true, REG_DEP_ANTI, true);
      if (!deps->readonly)
	{
	  if (!deps->last_logue_was_epilogue)
	    free_INSN_LIST_list (&deps->last_epilogue);
	  deps->last_epilogue = alloc_INSN_LIST (insn, deps->last_epilogue);
	  deps->last_logue_was_epilogue = true;
	}
    }
}

/* Return TRUE if INSN might not always return normally (e.g. call exit,
   longjmp, loop forever, ...).  */
/* FIXME: Why can't this function just use flags_from_decl_or_type and
   test for ECF_NORETURN?  */
static bool
call_may_noreturn_p (rtx_insn *insn)
{
  rtx call;

  /* const or pure calls that aren't looping will always return.  */
  if (RTL_CONST_OR_PURE_CALL_P (insn)
      && !RTL_LOOPING_CONST_OR_PURE_CALL_P (insn))
    return false;

  call = get_call_rtx_from (insn);
  if (call && GET_CODE (XEXP (XEXP (call, 0), 0)) == SYMBOL_REF)
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

/* Return true if INSN should be made dependent on the previous instruction
   group, and if all INSN's dependencies should be moved to the first
   instruction of that group.  */

static bool
chain_to_prev_insn_p (rtx_insn *insn)
{
  /* INSN forms a group with the previous instruction.  */
  if (SCHED_GROUP_P (insn))
    return true;

  /* If the previous instruction clobbers a register R and this one sets
     part of R, the clobber was added specifically to help us track the
     liveness of R.  There's no point scheduling the clobber and leaving
     INSN behind, especially if we move the clobber to another block.  */
  rtx_insn *prev = prev_nonnote_nondebug_insn (insn);
  if (prev
      && INSN_P (prev)
      && BLOCK_FOR_INSN (prev) == BLOCK_FOR_INSN (insn)
      && GET_CODE (PATTERN (prev)) == CLOBBER)
    {
      rtx x = XEXP (PATTERN (prev), 0);
      if (set_of (x, insn))
	return true;
    }

  return false;
}

/* Analyze INSN with DEPS as a context.  */
void
deps_analyze_insn (struct deps_desc *deps, rtx_insn *insn)
{
  if (sched_deps_info->start_insn)
    sched_deps_info->start_insn (insn);

  /* Record the condition for this insn.  */
  if (NONDEBUG_INSN_P (insn))
    {
      rtx t;
      sched_get_condition_with_rev (insn, NULL);
      t = INSN_CACHED_COND (insn);
      INSN_COND_DEPS (insn) = NULL;
      if (reload_completed
	  && (current_sched_info->flags & DO_PREDICATION)
	  && COMPARISON_P (t)
	  && REG_P (XEXP (t, 0))
	  && CONSTANT_P (XEXP (t, 1)))
	{
	  unsigned int regno;
	  int nregs;
	  rtx_insn_list *cond_deps = NULL;
	  t = XEXP (t, 0);
	  regno = REGNO (t);
	  nregs = REG_NREGS (t);
	  while (nregs-- > 0)
	    {
	      struct deps_reg *reg_last = &deps->reg_last[regno + nregs];
	      cond_deps = concat_INSN_LIST (reg_last->sets, cond_deps);
	      cond_deps = concat_INSN_LIST (reg_last->clobbers, cond_deps);
	      cond_deps = concat_INSN_LIST (reg_last->implicit_sets, cond_deps);
	    }
	  INSN_COND_DEPS (insn) = cond_deps;
	}
    }

  if (JUMP_P (insn))
    {
      /* Make each JUMP_INSN (but not a speculative check)
         a scheduling barrier for memory references.  */
      if (!deps->readonly
          && !(sel_sched_p ()
               && sel_insn_is_speculation_check (insn)))
        {
          /* Keep the list a reasonable size.  */
          if (deps->pending_flush_length++ >= MAX_PENDING_LIST_LENGTH)
            flush_pending_lists (deps, insn, true, true);
          else
	    deps->pending_jump_insns
              = alloc_INSN_LIST (insn, deps->pending_jump_insns);
        }

      /* For each insn which shouldn't cross a jump, add a dependence.  */
      add_dependence_list_and_free (deps, insn,
				    &deps->sched_before_next_jump, 1,
				    REG_DEP_ANTI, true);

      sched_analyze_insn (deps, PATTERN (insn), insn);
    }
  else if (NONJUMP_INSN_P (insn) || DEBUG_INSN_P (insn))
    {
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
	    else if (targetm.hard_regno_call_part_clobbered (i,
							     reg_raw_mode[i])
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
                                    REG_DEP_ANTI, true);

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

  if (sched_deps_info->finish_insn)
    sched_deps_info->finish_insn ();

  /* Fixup the dependencies in the sched group.  */
  if ((NONJUMP_INSN_P (insn) || JUMP_P (insn))
      && chain_to_prev_insn_p (insn)
      && !sel_sched_p ())
    chain_to_prev_insn (insn);
}

/* Initialize DEPS for the new block beginning with HEAD.  */
void
deps_start_bb (struct deps_desc *deps, rtx_insn *head)
{
  gcc_assert (!deps->readonly);

  /* Before reload, if the previous block ended in a call, show that
     we are inside a post-call group, so as to keep the lifetimes of
     hard registers correct.  */
  if (! reload_completed && !LABEL_P (head))
    {
      rtx_insn *insn = prev_nonnote_nondebug_insn (head);

      if (insn && CALL_P (insn))
	deps->in_post_call_group_p = post_call_initial;
    }
}

/* Analyze every insn between HEAD and TAIL inclusive, creating backward
   dependencies for each insn.  */
void
sched_analyze (struct deps_desc *deps, rtx_insn *head, rtx_insn *tail)
{
  rtx_insn *insn;

  if (sched_deps_info->use_cselib)
    cselib_init (CSELIB_RECORD_MEMORY);

  deps_start_bb (deps, head);

  for (insn = head;; insn = NEXT_INSN (insn))
    {

      if (INSN_P (insn))
	{
	  /* And initialize deps_lists.  */
	  sd_init_insn (insn);
	  /* Clean up SCHED_GROUP_P which may be set by last
	     scheduler pass.  */
	  if (SCHED_GROUP_P (insn))
	    SCHED_GROUP_P (insn) = 0;
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
delete_dep_nodes_in_back_deps (rtx_insn *insn, bool resolved_p)
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
sched_free_deps (rtx_insn *head, rtx_insn *tail, bool resolved_p)
{
  rtx_insn *insn;
  rtx_insn *next_tail = NEXT_INSN (tail);

  /* We make two passes since some insns may be scheduled before their
     dependencies are resolved.  */
  for (insn = head; insn != next_tail; insn = NEXT_INSN (insn))
    if (INSN_P (insn) && INSN_LUID (insn) > 0)
      {
	/* Clear forward deps and leave the dep_nodes to the
	   corresponding back_deps list.  */
	if (resolved_p)
	  clear_deps_list (INSN_RESOLVED_FORW_DEPS (insn));
	else
	  clear_deps_list (INSN_FORW_DEPS (insn));
      }
  for (insn = head; insn != next_tail; insn = NEXT_INSN (insn))
    if (INSN_P (insn) && INSN_LUID (insn) > 0)
      {
	/* Clear resolved back deps together with its dep_nodes.  */
	delete_dep_nodes_in_back_deps (insn, resolved_p);

	sd_finish_insn (insn);
      }
}

/* Initialize variables for region data dependence analysis.
   When LAZY_REG_LAST is true, do not allocate reg_last array
   of struct deps_desc immediately.  */

void
init_deps (struct deps_desc *deps, bool lazy_reg_last)
{
  int max_reg = (reload_completed ? FIRST_PSEUDO_REGISTER : max_reg_num ());

  deps->max_reg = max_reg;
  if (lazy_reg_last)
    deps->reg_last = NULL;
  else
    deps->reg_last = XCNEWVEC (struct deps_reg, max_reg);
  INIT_REG_SET (&deps->reg_last_in_use);

  deps->pending_read_insns = 0;
  deps->pending_read_mems = 0;
  deps->pending_write_insns = 0;
  deps->pending_write_mems = 0;
  deps->pending_jump_insns = 0;
  deps->pending_read_list_length = 0;
  deps->pending_write_list_length = 0;
  deps->pending_flush_length = 0;
  deps->last_pending_memory_flush = 0;
  deps->last_function_call = 0;
  deps->last_function_call_may_noreturn = 0;
  deps->sched_before_next_call = 0;
  deps->sched_before_next_jump = 0;
  deps->in_post_call_group_p = not_post_call;
  deps->last_debug_insn = 0;
  deps->last_args_size = 0;
  deps->last_prologue = 0;
  deps->last_epilogue = 0;
  deps->last_logue_was_epilogue = false;
  deps->last_reg_pending_barrier = NOT_A_BARRIER;
  deps->readonly = 0;
}

/* Init only reg_last field of DEPS, which was not allocated before as
   we inited DEPS lazily.  */
void
init_deps_reg_last (struct deps_desc *deps)
{
  gcc_assert (deps && deps->max_reg > 0);
  gcc_assert (deps->reg_last == NULL);

  deps->reg_last = XCNEWVEC (struct deps_reg, deps->max_reg);
}


/* Free insn lists found in DEPS.  */

void
free_deps (struct deps_desc *deps)
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
      if (reg_last->control_uses)
	free_INSN_LIST_list (&reg_last->control_uses);
      if (reg_last->clobbers)
	free_INSN_LIST_list (&reg_last->clobbers);
    }
  CLEAR_REG_SET (&deps->reg_last_in_use);

  /* As we initialize reg_last lazily, it is possible that we didn't allocate
     it at all.  */
  free (deps->reg_last);
  deps->reg_last = NULL;

  deps = NULL;
}

/* Remove INSN from dependence contexts DEPS.  */
void
remove_from_deps (struct deps_desc *deps, rtx_insn *insn)
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

  removed = remove_from_dependence_list (insn, &deps->pending_jump_insns);
  deps->pending_flush_length -= removed;
  removed = remove_from_dependence_list (insn, &deps->last_pending_memory_flush);
  deps->pending_flush_length -= removed;

  unsigned to_clear = -1U;
  EXECUTE_IF_SET_IN_REG_SET (&deps->reg_last_in_use, 0, i, rsi)
    {
      if (to_clear != -1U)
	{
	  CLEAR_REGNO_REG_SET (&deps->reg_last_in_use, to_clear);
	  to_clear = -1U;
	}
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
	to_clear = i;
    }
  if (to_clear != -1U)
    CLEAR_REGNO_REG_SET (&deps->reg_last_in_use, to_clear);

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
  int reserve = (sched_max_luid + 1 - h_d_i_d.length ());
  if (reserve > 0 && ! h_d_i_d.space (reserve))
    h_d_i_d.safe_grow_cleared (3 * sched_max_luid / 2);
}

/* If it is profitable to use them, initialize or extend (depending on
   GLOBAL_P) dependency data.  */
void
sched_deps_init (bool global_p)
{
  /* Average number of insns in the basic block.
     '+ 1' is used to make it nonzero.  */
  int insns_in_block = sched_max_luid / n_basic_blocks_for_fn (cfun) + 1;

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
      dl_pool = new object_allocator<_deps_list> ("deps_list");
				/* Allocate lists for one block at a time.  */
      dn_pool = new object_allocator<_dep_node> ("dep_node");
				/* Allocate nodes for one block at a time.  */
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
      control_dependency_cache = XRESIZEVEC (bitmap_head, control_dependency_cache,
					  luid);

      if (current_sched_info->flags & DO_SPECULATION)
        spec_dependency_cache = XRESIZEVEC (bitmap_head, spec_dependency_cache,
					    luid);

      for (i = cache_size; i < luid; i++)
	{
	  bitmap_initialize (&true_dependency_cache[i], 0);
	  bitmap_initialize (&output_dependency_cache[i], 0);
	  bitmap_initialize (&anti_dependency_cache[i], 0);
	  bitmap_initialize (&control_dependency_cache[i], 0);

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
  delete dn_pool;
  delete dl_pool;
  dn_pool = NULL;
  dl_pool = NULL;

  h_d_i_d.release ();
  cache_size = 0;

  if (true_dependency_cache)
    {
      int i;

      for (i = 0; i < cache_size; i++)
	{
	  bitmap_clear (&true_dependency_cache[i]);
	  bitmap_clear (&output_dependency_cache[i]);
	  bitmap_clear (&anti_dependency_cache[i]);
	  bitmap_clear (&control_dependency_cache[i]);

          if (sched_deps_info->generate_spec_deps)
            bitmap_clear (&spec_dependency_cache[i]);
	}
      free (true_dependency_cache);
      true_dependency_cache = NULL;
      free (output_dependency_cache);
      output_dependency_cache = NULL;
      free (anti_dependency_cache);
      anti_dependency_cache = NULL;
      free (control_dependency_cache);
      control_dependency_cache = NULL;

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
  reg_pending_control_uses = ALLOC_REG_SET (&reg_obstack);
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
  FREE_REG_SET (reg_pending_control_uses);
}

/* Estimate the weakness of dependence between MEM1 and MEM2.  */
dw_t
estimate_dep_weak (rtx mem1, rtx mem2)
{
  if (mem1 == mem2)
    /* MEMs are the same - don't speculate.  */
    return MIN_DEP_WEAK;

  rtx r1 = XEXP (mem1, 0);
  rtx r2 = XEXP (mem2, 0);

  if (sched_deps_info->use_cselib)
    {
      /* We cannot call rtx_equal_for_cselib_p because the VALUEs might be
	 dangling at this point, since we never preserve them.  Instead we
	 canonicalize manually to get stable VALUEs out of hashing.  */
      if (GET_CODE (r1) == VALUE && CSELIB_VAL_PTR (r1))
	r1 = canonical_cselib_val (CSELIB_VAL_PTR (r1))->val_rtx;
      if (GET_CODE (r2) == VALUE && CSELIB_VAL_PTR (r2))
	r2 = canonical_cselib_val (CSELIB_VAL_PTR (r2))->val_rtx;
    }

  if (r1 == r2
      || (REG_P (r1) && REG_P (r2) && REGNO (r1) == REGNO (r2)))
    /* Again, MEMs are the same.  */
    return MIN_DEP_WEAK;
  else if ((REG_P (r1) && !REG_P (r2)) || (!REG_P (r1) && REG_P (r2)))
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
static void
add_dependence_1 (rtx_insn *insn, rtx_insn *elem, enum reg_note dep_type)
{
  ds_t ds;
  bool internal;

  if (dep_type == REG_DEP_TRUE)
    ds = DEP_TRUE;
  else if (dep_type == REG_DEP_OUTPUT)
    ds = DEP_OUTPUT;
  else if (dep_type == REG_DEP_CONTROL)
    ds = DEP_CONTROL;
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

/* Return weakness of speculative type TYPE in the dep_status DS,
   without checking to prevent ICEs on malformed input.  */
static dw_t
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

/* Return weakness of speculative type TYPE in the dep_status DS.  */
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
  if (s & DEP_OUTPUT)
    fprintf (f, "DEP_OUTPUT; ");
  if (s & DEP_ANTI)
    fprintf (f, "DEP_ANTI; ");
  if (s & DEP_CONTROL)
    fprintf (f, "DEP_CONTROL; ");

  fprintf (f, "}");
}

DEBUG_FUNCTION void
debug_ds (ds_t s)
{
  dump_ds (stderr, s);
  fprintf (stderr, "\n");
}

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
      gcc_assert (ds == 0);
      return;
    }

  /* Check that dependence type contains the same bits as the status.  */
  if (dt == REG_DEP_TRUE)
    gcc_assert (ds & DEP_TRUE);
  else if (dt == REG_DEP_OUTPUT)
    gcc_assert ((ds & DEP_OUTPUT)
		&& !(ds & DEP_TRUE));
  else if (dt == REG_DEP_ANTI)
    gcc_assert ((ds & DEP_ANTI)
		&& !(ds & (DEP_OUTPUT | DEP_TRUE)));
  else
    gcc_assert (dt == REG_DEP_CONTROL
		&& (ds & DEP_CONTROL)
		&& !(ds & (DEP_OUTPUT | DEP_ANTI | DEP_TRUE)));

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

/* The following code discovers opportunities to switch a memory reference
   and an increment by modifying the address.  We ensure that this is done
   only for dependencies that are only used to show a single register
   dependence (using DEP_NONREG and DEP_MULTIPLE), and so that every memory
   instruction involved is subject to only one dep that can cause a pattern
   change.

   When we discover a suitable dependency, we fill in the dep_replacement
   structure to show how to modify the memory reference.  */

/* Holds information about a pair of memory reference and register increment
   insns which depend on each other, but could possibly be interchanged.  */
struct mem_inc_info
{
  rtx_insn *inc_insn;
  rtx_insn *mem_insn;

  rtx *mem_loc;
  /* A register occurring in the memory address for which we wish to break
     the dependence.  This must be identical to the destination register of
     the increment.  */
  rtx mem_reg0;
  /* Any kind of index that is added to that register.  */
  rtx mem_index;
  /* The constant offset used in the memory address.  */
  HOST_WIDE_INT mem_constant;
  /* The constant added in the increment insn.  Negated if the increment is
     after the memory address.  */
  HOST_WIDE_INT inc_constant;
  /* The source register used in the increment.  May be different from mem_reg0
     if the increment occurs before the memory address.  */
  rtx inc_input;
};

/* Verify that the memory location described in MII can be replaced with
   one using NEW_ADDR.  Return the new memory reference or NULL_RTX.  The
   insn remains unchanged by this function.  */

static rtx
attempt_change (struct mem_inc_info *mii, rtx new_addr)
{
  rtx mem = *mii->mem_loc;
  rtx new_mem;

  /* Jump through a lot of hoops to keep the attributes up to date.  We
     do not want to call one of the change address variants that take
     an offset even though we know the offset in many cases.  These
     assume you are changing where the address is pointing by the
     offset.  */
  new_mem = replace_equiv_address_nv (mem, new_addr);
  if (! validate_change (mii->mem_insn, mii->mem_loc, new_mem, 0))
    {
      if (sched_verbose >= 5)
	fprintf (sched_dump, "validation failure\n");
      return NULL_RTX;
    }

  /* Put back the old one.  */
  validate_change (mii->mem_insn, mii->mem_loc, mem, 0);

  return new_mem;
}

/* Return true if INSN is of a form "a = b op c" where a and b are
   regs.  op is + if c is a reg and +|- if c is a const.  Fill in
   informantion in MII about what is found.
   BEFORE_MEM indicates whether the increment is found before or after
   a corresponding memory reference.  */

static bool
parse_add_or_inc (struct mem_inc_info *mii, rtx_insn *insn, bool before_mem)
{
  rtx pat = single_set (insn);
  rtx src, cst;
  bool regs_equal;

  if (RTX_FRAME_RELATED_P (insn) || !pat)
    return false;

  /* Do not allow breaking data dependencies for insns that are marked
     with REG_STACK_CHECK.  */
  if (find_reg_note (insn, REG_STACK_CHECK, NULL))
    return false;

  /* Result must be single reg.  */
  if (!REG_P (SET_DEST (pat)))
    return false;

  if (GET_CODE (SET_SRC (pat)) != PLUS)
    return false;

  mii->inc_insn = insn;
  src = SET_SRC (pat);
  mii->inc_input = XEXP (src, 0);

  if (!REG_P (XEXP (src, 0)))
    return false;

  if (!rtx_equal_p (SET_DEST (pat), mii->mem_reg0))
    return false;

  cst = XEXP (src, 1);
  if (!CONST_INT_P (cst))
    return false;
  mii->inc_constant = INTVAL (cst);

  regs_equal = rtx_equal_p (mii->inc_input, mii->mem_reg0);

  if (!before_mem)
    {
      mii->inc_constant = -mii->inc_constant;
      if (!regs_equal)
	return false;
    }

  if (regs_equal && REGNO (SET_DEST (pat)) == STACK_POINTER_REGNUM)
    {
      /* Note that the sign has already been reversed for !before_mem.  */
      if (STACK_GROWS_DOWNWARD)
	return mii->inc_constant > 0;
      else
	return mii->inc_constant < 0;
    }
  return true;
}

/* Once a suitable mem reference has been found and the corresponding data
   in MII has been filled in, this function is called to find a suitable
   add or inc insn involving the register we found in the memory
   reference.  */

static bool
find_inc (struct mem_inc_info *mii, bool backwards)
{
  sd_iterator_def sd_it;
  dep_t dep;

  sd_it = sd_iterator_start (mii->mem_insn,
			     backwards ? SD_LIST_HARD_BACK : SD_LIST_FORW);
  while (sd_iterator_cond (&sd_it, &dep))
    {
      dep_node_t node = DEP_LINK_NODE (*sd_it.linkp);
      rtx_insn *pro = DEP_PRO (dep);
      rtx_insn *con = DEP_CON (dep);
      rtx_insn *inc_cand = backwards ? pro : con;
      if (DEP_NONREG (dep) || DEP_MULTIPLE (dep))
	goto next;
      if (parse_add_or_inc (mii, inc_cand, backwards))
	{
	  struct dep_replacement *desc;
	  df_ref def;
	  rtx newaddr, newmem;

	  if (sched_verbose >= 5)
	    fprintf (sched_dump, "candidate mem/inc pair: %d %d\n",
		     INSN_UID (mii->mem_insn), INSN_UID (inc_cand));

	  /* Need to assure that none of the operands of the inc
	     instruction are assigned to by the mem insn.  */
	  FOR_EACH_INSN_DEF (def, mii->mem_insn)
	    if (reg_overlap_mentioned_p (DF_REF_REG (def), mii->inc_input)
		|| reg_overlap_mentioned_p (DF_REF_REG (def), mii->mem_reg0))
	      {
		if (sched_verbose >= 5)
		  fprintf (sched_dump,
			   "inc conflicts with store failure.\n");
		goto next;
	      }

	  newaddr = mii->inc_input;
	  if (mii->mem_index != NULL_RTX)
	    newaddr = gen_rtx_PLUS (GET_MODE (newaddr), newaddr,
				    mii->mem_index);
	  newaddr = plus_constant (GET_MODE (newaddr), newaddr,
				   mii->mem_constant + mii->inc_constant);
	  newmem = attempt_change (mii, newaddr);
	  if (newmem == NULL_RTX)
	    goto next;
	  if (sched_verbose >= 5)
	    fprintf (sched_dump, "successful address replacement\n");
	  desc = XCNEW (struct dep_replacement);
	  DEP_REPLACE (dep) = desc;
	  desc->loc = mii->mem_loc;
	  desc->newval = newmem;
	  desc->orig = *desc->loc;
	  desc->insn = mii->mem_insn;
	  move_dep_link (DEP_NODE_BACK (node), INSN_HARD_BACK_DEPS (con),
			 INSN_SPEC_BACK_DEPS (con));
	  if (backwards)
	    {
	      FOR_EACH_DEP (mii->inc_insn, SD_LIST_BACK, sd_it, dep)
		add_dependence_1 (mii->mem_insn, DEP_PRO (dep),
				  REG_DEP_TRUE);
	    }
	  else
	    {
	      FOR_EACH_DEP (mii->inc_insn, SD_LIST_FORW, sd_it, dep)
		add_dependence_1 (DEP_CON (dep), mii->mem_insn,
				  REG_DEP_ANTI);
	    }
	  return true;
	}
    next:
      sd_iterator_next (&sd_it);
    }
  return false;
}

/* A recursive function that walks ADDRESS_OF_X to find memory references
   which could be modified during scheduling.  We call find_inc for each
   one we find that has a recognizable form.  MII holds information about
   the pair of memory/increment instructions.
   We ensure that every instruction with a memory reference (which will be
   the location of the replacement) is assigned at most one breakable
   dependency.  */

static bool
find_mem (struct mem_inc_info *mii, rtx *address_of_x)
{
  rtx x = *address_of_x;
  enum rtx_code code = GET_CODE (x);
  const char *const fmt = GET_RTX_FORMAT (code);
  int i;

  if (code == MEM)
    {
      rtx reg0 = XEXP (x, 0);

      mii->mem_loc = address_of_x;
      mii->mem_index = NULL_RTX;
      mii->mem_constant = 0;
      if (GET_CODE (reg0) == PLUS && CONST_INT_P (XEXP (reg0, 1)))
	{
	  mii->mem_constant = INTVAL (XEXP (reg0, 1));
	  reg0 = XEXP (reg0, 0);
	}
      if (GET_CODE (reg0) == PLUS)
	{
	  mii->mem_index = XEXP (reg0, 1);
	  reg0 = XEXP (reg0, 0);
	}
      if (REG_P (reg0))
	{
	  df_ref use;
	  int occurrences = 0;

	  /* Make sure this reg appears only once in this insn.  Can't use
	     count_occurrences since that only works for pseudos.  */
	  FOR_EACH_INSN_USE (use, mii->mem_insn)
	    if (reg_overlap_mentioned_p (reg0, DF_REF_REG (use)))
	      if (++occurrences > 1)
		{
		  if (sched_verbose >= 5)
		    fprintf (sched_dump, "mem count failure\n");
		  return false;
		}

	  mii->mem_reg0 = reg0;
	  return find_inc (mii, true) || find_inc (mii, false);
	}
      return false;
    }

  if (code == SIGN_EXTRACT || code == ZERO_EXTRACT)
    {
      /* If REG occurs inside a MEM used in a bit-field reference,
	 that is unacceptable.  */
      return false;
    }

  /* Time for some deep diving.  */
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if (find_mem (mii, &XEXP (x, i)))
	    return true;
	}
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (find_mem (mii, &XVECEXP (x, i, j)))
	      return true;
	}
    }
  return false;
}


/* Examine the instructions between HEAD and TAIL and try to find
   dependencies that can be broken by modifying one of the patterns.  */

void
find_modifiable_mems (rtx_insn *head, rtx_insn *tail)
{
  rtx_insn *insn, *next_tail = NEXT_INSN (tail);
  int success_in_block = 0;

  for (insn = head; insn != next_tail; insn = NEXT_INSN (insn))
    {
      struct mem_inc_info mii;

      if (!NONDEBUG_INSN_P (insn) || RTX_FRAME_RELATED_P (insn))
	continue;

      mii.mem_insn = insn;
      if (find_mem (&mii, &PATTERN (insn)))
	success_in_block++;
    }
  if (success_in_block && sched_verbose >= 5)
    fprintf (sched_dump, "%d candidates for address modification found.\n",
	     success_in_block);
}

#endif /* INSN_SCHEDULING */
