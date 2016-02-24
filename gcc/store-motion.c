/* Store motion via Lazy Code Motion on the reverse CFG.
   Copyright (C) 1997-2016 Free Software Foundation, Inc.

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
#include "rtl.h"
#include "tree.h"
#include "predict.h"
#include "df.h"
#include "toplev.h"

#include "cfgrtl.h"
#include "cfganal.h"
#include "lcm.h"
#include "cfgcleanup.h"
#include "expr.h"
#include "tree-pass.h"
#include "dbgcnt.h"
#include "rtl-iter.h"

/* This pass implements downward store motion.
   As of May 1, 2009, the pass is not enabled by default on any target,
   but bootstrap completes on ia64 and x86_64 with the pass enabled.  */

/* TODO:
   - remove_reachable_equiv_notes is an incomprehensible pile of goo and
     a compile time hog that needs a rewrite (maybe cache st_exprs to
     invalidate REG_EQUAL/REG_EQUIV notes for?).
   - pattern_regs in st_expr should be a regset (on its own obstack).
   - antic_stores and avail_stores should be VECs instead of lists.
   - store_motion_mems should be a vec instead of a list.
   - there should be an alloc pool for struct st_expr objects.
   - investigate whether it is helpful to make the address of an st_expr
     a cselib VALUE.
   - when GIMPLE alias information is exported, the effectiveness of this
     pass should be re-evaluated.
*/

/* This is a list of store expressions (MEMs).  The structure is used
   as an expression table to track stores which look interesting, and
   might be moveable towards the exit block.  */

struct st_expr
{
  /* Pattern of this mem.  */
  rtx pattern;
  /* List of registers mentioned by the mem.  */
  rtx pattern_regs;
  /* INSN list of stores that are locally anticipatable.  */
  rtx_insn_list *antic_stores;
  /* INSN list of stores that are locally available.  */
  rtx_insn_list *avail_stores;
  /* Next in the list.  */
  struct st_expr * next;
  /* Store ID in the dataflow bitmaps.  */
  int index;
  /* Hash value for the hash table.  */
  unsigned int hash_index;
  /* Register holding the stored expression when a store is moved.
     This field is also used as a cache in find_moveable_store, see
     LAST_AVAIL_CHECK_FAILURE below.  */
  rtx reaching_reg;
};

/* Head of the list of load/store memory refs.  */
static struct st_expr * store_motion_mems = NULL;

/* These bitmaps will hold the local dataflow properties per basic block.  */
static sbitmap *st_kill, *st_avloc, *st_antloc, *st_transp;

/* Nonzero for expressions which should be inserted on a specific edge.  */
static sbitmap *st_insert_map;

/* Nonzero for expressions which should be deleted in a specific block.  */
static sbitmap *st_delete_map;

/* Global holding the number of store expressions we are dealing with.  */
static int num_stores;

/* Contains the edge_list returned by pre_edge_lcm.  */
static struct edge_list *edge_list;

/* Hashtable helpers.  */

struct st_expr_hasher : nofree_ptr_hash <st_expr>
{
  static inline hashval_t hash (const st_expr *);
  static inline bool equal (const st_expr *, const st_expr *);
};

inline hashval_t
st_expr_hasher::hash (const st_expr *x)
{
  int do_not_record_p = 0;
  return hash_rtx (x->pattern, GET_MODE (x->pattern), &do_not_record_p, NULL, false);
}

inline bool
st_expr_hasher::equal (const st_expr *ptr1, const st_expr *ptr2)
{
  return exp_equiv_p (ptr1->pattern, ptr2->pattern, 0, true);
}

/* Hashtable for the load/store memory refs.  */
static hash_table<st_expr_hasher> *store_motion_mems_table;

/* This will search the st_expr list for a matching expression. If it
   doesn't find one, we create one and initialize it.  */

static struct st_expr *
st_expr_entry (rtx x)
{
  int do_not_record_p = 0;
  struct st_expr * ptr;
  unsigned int hash;
  st_expr **slot;
  struct st_expr e;

  hash = hash_rtx (x, GET_MODE (x), &do_not_record_p,
		   NULL,  /*have_reg_qty=*/false);

  e.pattern = x;
  slot = store_motion_mems_table->find_slot_with_hash (&e, hash, INSERT);
  if (*slot)
    return *slot;

  ptr = XNEW (struct st_expr);

  ptr->next         = store_motion_mems;
  ptr->pattern      = x;
  ptr->pattern_regs = NULL_RTX;
  ptr->antic_stores = NULL;
  ptr->avail_stores = NULL;
  ptr->reaching_reg = NULL_RTX;
  ptr->index        = 0;
  ptr->hash_index   = hash;
  store_motion_mems = ptr;
  *slot = ptr;

  return ptr;
}

/* Free up an individual st_expr entry.  */

static void
free_st_expr_entry (struct st_expr * ptr)
{
  free_INSN_LIST_list (& ptr->antic_stores);
  free_INSN_LIST_list (& ptr->avail_stores);

  free (ptr);
}

/* Free up all memory associated with the st_expr list.  */

static void
free_store_motion_mems (void)
{
  delete store_motion_mems_table;
  store_motion_mems_table = NULL;

  while (store_motion_mems)
    {
      struct st_expr * tmp = store_motion_mems;
      store_motion_mems = store_motion_mems->next;
      free_st_expr_entry (tmp);
    }
  store_motion_mems = NULL;
}

/* Assign each element of the list of mems a monotonically increasing value.  */

static int
enumerate_store_motion_mems (void)
{
  struct st_expr * ptr;
  int n = 0;

  for (ptr = store_motion_mems; ptr != NULL; ptr = ptr->next)
    ptr->index = n++;

  return n;
}

/* Return first item in the list.  */

static inline struct st_expr *
first_st_expr (void)
{
  return store_motion_mems;
}

/* Return the next item in the list after the specified one.  */

static inline struct st_expr *
next_st_expr (struct st_expr * ptr)
{
  return ptr->next;
}

/* Dump debugging info about the store_motion_mems list.  */

static void
print_store_motion_mems (FILE * file)
{
  struct st_expr * ptr;

  fprintf (dump_file, "STORE_MOTION list of MEM exprs considered:\n");

  for (ptr = first_st_expr (); ptr != NULL; ptr = next_st_expr (ptr))
    {
      fprintf (file, "  Pattern (%3d): ", ptr->index);

      print_rtl (file, ptr->pattern);

      fprintf (file, "\n	 ANTIC stores : ");

      if (ptr->antic_stores)
	print_rtl (file, ptr->antic_stores);
      else
	fprintf (file, "(nil)");

      fprintf (file, "\n	 AVAIL stores : ");

      if (ptr->avail_stores)
	print_rtl (file, ptr->avail_stores);
      else
	fprintf (file, "(nil)");

      fprintf (file, "\n\n");
    }

  fprintf (file, "\n");
}

/* Return zero if some of the registers in list X are killed
   due to set of registers in bitmap REGS_SET.  */

static bool
store_ops_ok (const_rtx x, int *regs_set)
{
  const_rtx reg;

  for (; x; x = XEXP (x, 1))
    {
      reg = XEXP (x, 0);
      if (regs_set[REGNO (reg)])
	return false;
    }

  return true;
}

/* Returns a list of registers mentioned in X.
   FIXME: A regset would be prettier and less expensive.  */

static rtx_expr_list *
extract_mentioned_regs (rtx x)
{
  rtx_expr_list *mentioned_regs = NULL;
  subrtx_var_iterator::array_type array;
  FOR_EACH_SUBRTX_VAR (iter, array, x, NONCONST)
    {
      rtx x = *iter;
      if (REG_P (x))
	mentioned_regs = alloc_EXPR_LIST (0, x, mentioned_regs);
    }
  return mentioned_regs;
}

/* Check to see if the load X is aliased with STORE_PATTERN.
   AFTER is true if we are checking the case when STORE_PATTERN occurs
   after the X.  */

static bool
load_kills_store (const_rtx x, const_rtx store_pattern, int after)
{
  if (after)
    return anti_dependence (x, store_pattern);
  else
    return true_dependence (store_pattern, GET_MODE (store_pattern), x);
}

/* Go through the entire rtx X, looking for any loads which might alias
   STORE_PATTERN.  Return true if found.
   AFTER is true if we are checking the case when STORE_PATTERN occurs
   after the insn X.  */

static bool
find_loads (const_rtx x, const_rtx store_pattern, int after)
{
  const char * fmt;
  int i, j;
  int ret = false;

  if (!x)
    return false;

  if (GET_CODE (x) == SET)
    x = SET_SRC (x);

  if (MEM_P (x))
    {
      if (load_kills_store (x, store_pattern, after))
	return true;
    }

  /* Recursively process the insn.  */
  fmt = GET_RTX_FORMAT (GET_CODE (x));

  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0 && !ret; i--)
    {
      if (fmt[i] == 'e')
	ret |= find_loads (XEXP (x, i), store_pattern, after);
      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  ret |= find_loads (XVECEXP (x, i, j), store_pattern, after);
    }
  return ret;
}

/* Go through pattern PAT looking for any loads which might kill the
   store in X.  Return true if found.
   AFTER is true if we are checking the case when loads kill X occurs
   after the insn for PAT.  */

static inline bool
store_killed_in_pat (const_rtx x, const_rtx pat, int after)
{
  if (GET_CODE (pat) == SET)
    {
      rtx dest = SET_DEST (pat);

      if (GET_CODE (dest) == ZERO_EXTRACT)
	dest = XEXP (dest, 0);

      /* Check for memory stores to aliased objects.  */
      if (MEM_P (dest)
	  && !exp_equiv_p (dest, x, 0, true))
	{
	  if (after)
	    {
	      if (output_dependence (dest, x))
		return true;
	    }
	  else
	    {
	      if (output_dependence (x, dest))
		return true;
	    }
	}
    }

  if (find_loads (pat, x, after))
    return true;

  return false;
}

/* Check if INSN kills the store pattern X (is aliased with it).
   AFTER is true if we are checking the case when store X occurs
   after the insn.  Return true if it does.  */

static bool
store_killed_in_insn (const_rtx x, const_rtx x_regs, const rtx_insn *insn, int after)
{
  const_rtx reg, note, pat;

  if (! NONDEBUG_INSN_P (insn))
    return false;

  if (CALL_P (insn))
    {
      /* A normal or pure call might read from pattern,
	 but a const call will not.  */
      if (!RTL_CONST_CALL_P (insn))
	return true;

      /* But even a const call reads its parameters.  Check whether the
	 base of some of registers used in mem is stack pointer.  */
      for (reg = x_regs; reg; reg = XEXP (reg, 1))
	if (may_be_sp_based_p (XEXP (reg, 0)))
	  return true;

      return false;
    }

  pat = PATTERN (insn);
  if (GET_CODE (pat) == SET)
    {
      if (store_killed_in_pat (x, pat, after))
	return true;
    }
  else if (GET_CODE (pat) == PARALLEL)
    {
      int i;

      for (i = 0; i < XVECLEN (pat, 0); i++)
	if (store_killed_in_pat (x, XVECEXP (pat, 0, i), after))
	  return true;
    }
  else if (find_loads (PATTERN (insn), x, after))
    return true;

  /* If this insn has a REG_EQUAL or REG_EQUIV note referencing a memory
     location aliased with X, then this insn kills X.  */
  note = find_reg_equal_equiv_note (insn);
  if (! note)
    return false;
  note = XEXP (note, 0);

  /* However, if the note represents a must alias rather than a may
     alias relationship, then it does not kill X.  */
  if (exp_equiv_p (note, x, 0, true))
    return false;

  /* See if there are any aliased loads in the note.  */
  return find_loads (note, x, after);
}

/* Returns true if the expression X is loaded or clobbered on or after INSN
   within basic block BB.  REGS_SET_AFTER is bitmap of registers set in
   or after the insn.  X_REGS is list of registers mentioned in X. If the store
   is killed, return the last insn in that it occurs in FAIL_INSN.  */

static bool
store_killed_after (const_rtx x, const_rtx x_regs, const rtx_insn *insn,
		    const_basic_block bb,
		    int *regs_set_after, rtx *fail_insn)
{
  rtx_insn *last = BB_END (bb), *act;

  if (!store_ops_ok (x_regs, regs_set_after))
    {
      /* We do not know where it will happen.  */
      if (fail_insn)
	*fail_insn = NULL_RTX;
      return true;
    }

  /* Scan from the end, so that fail_insn is determined correctly.  */
  for (act = last; act != PREV_INSN (insn); act = PREV_INSN (act))
    if (store_killed_in_insn (x, x_regs, act, false))
      {
	if (fail_insn)
	  *fail_insn = act;
	return true;
      }

  return false;
}

/* Returns true if the expression X is loaded or clobbered on or before INSN
   within basic block BB. X_REGS is list of registers mentioned in X.
   REGS_SET_BEFORE is bitmap of registers set before or in this insn.  */
static bool
store_killed_before (const_rtx x, const_rtx x_regs, const rtx_insn *insn,
		     const_basic_block bb, int *regs_set_before)
{
  rtx_insn *first = BB_HEAD (bb);

  if (!store_ops_ok (x_regs, regs_set_before))
    return true;

  for ( ; insn != PREV_INSN (first); insn = PREV_INSN (insn))
    if (store_killed_in_insn (x, x_regs, insn, true))
      return true;

  return false;
}

/* The last insn in the basic block that compute_store_table is processing,
   where store_killed_after is true for X.
   Since we go through the basic block from BB_END to BB_HEAD, this is
   also the available store at the end of the basic block.  Therefore
   this is in effect a cache, to avoid calling store_killed_after for
   equivalent aliasing store expressions.
   This value is only meaningful during the computation of the store
   table.  We hi-jack the REACHING_REG field of struct st_expr to save
   a bit of memory.  */
#define LAST_AVAIL_CHECK_FAILURE(x)	((x)->reaching_reg)

/* Determine whether INSN is MEM store pattern that we will consider moving.
   REGS_SET_BEFORE is bitmap of registers set before (and including) the
   current insn, REGS_SET_AFTER is bitmap of registers set after (and
   including) the insn in this basic block.  We must be passing through BB from
   head to end, as we are using this fact to speed things up.

   The results are stored this way:

   -- the first anticipatable expression is added into ANTIC_STORES
   -- if the processed expression is not anticipatable, NULL_RTX is added
      there instead, so that we can use it as indicator that no further
      expression of this type may be anticipatable
   -- if the expression is available, it is added as head of AVAIL_STORES;
      consequently, all of them but this head are dead and may be deleted.
   -- if the expression is not available, the insn due to that it fails to be
      available is stored in REACHING_REG (via LAST_AVAIL_CHECK_FAILURE).

   The things are complicated a bit by fact that there already may be stores
   to the same MEM from other blocks; also caller must take care of the
   necessary cleanup of the temporary markers after end of the basic block.
   */

static void
find_moveable_store (rtx_insn *insn, int *regs_set_before, int *regs_set_after)
{
  struct st_expr * ptr;
  rtx dest, set;
  int check_anticipatable, check_available;
  basic_block bb = BLOCK_FOR_INSN (insn);

  set = single_set (insn);
  if (!set)
    return;

  dest = SET_DEST (set);

  if (! MEM_P (dest) || MEM_VOLATILE_P (dest)
      || GET_MODE (dest) == BLKmode)
    return;

  if (side_effects_p (dest))
    return;

  /* If we are handling exceptions, we must be careful with memory references
     that may trap.  If we are not, the behavior is undefined, so we may just
     continue.  */
  if (cfun->can_throw_non_call_exceptions && may_trap_p (dest))
    return;

  /* Even if the destination cannot trap, the source may.  In this case we'd
     need to handle updating the REG_EH_REGION note.  */
  if (find_reg_note (insn, REG_EH_REGION, NULL_RTX))
    return;

  /* Make sure that the SET_SRC of this store insns can be assigned to
     a register, or we will fail later on in replace_store_insn, which
     assumes that we can do this.  But sometimes the target machine has
     oddities like MEM read-modify-write instruction.  See for example
     PR24257.  */
  if (!can_assign_to_reg_without_clobbers_p (SET_SRC (set),
					      GET_MODE (SET_SRC (set))))
    return;

  ptr = st_expr_entry (dest);
  if (!ptr->pattern_regs)
    ptr->pattern_regs = extract_mentioned_regs (dest);

  /* Do not check for anticipatability if we either found one anticipatable
     store already, or tested for one and found out that it was killed.  */
  check_anticipatable = 0;
  if (!ptr->antic_stores)
    check_anticipatable = 1;
  else
    {
      rtx_insn *tmp = ptr->antic_stores->insn ();
      if (tmp != NULL_RTX
	  && BLOCK_FOR_INSN (tmp) != bb)
	check_anticipatable = 1;
    }
  if (check_anticipatable)
    {
      rtx_insn *tmp;
      if (store_killed_before (dest, ptr->pattern_regs, insn, bb, regs_set_before))
	tmp = NULL;
      else
	tmp = insn;
      ptr->antic_stores = alloc_INSN_LIST (tmp, ptr->antic_stores);
    }

  /* It is not necessary to check whether store is available if we did
     it successfully before; if we failed before, do not bother to check
     until we reach the insn that caused us to fail.  */
  check_available = 0;
  if (!ptr->avail_stores)
    check_available = 1;
  else
    {
      rtx_insn *tmp = ptr->avail_stores->insn ();
      if (BLOCK_FOR_INSN (tmp) != bb)
	check_available = 1;
    }
  if (check_available)
    {
      /* Check that we have already reached the insn at that the check
	 failed last time.  */
      if (LAST_AVAIL_CHECK_FAILURE (ptr))
	{
	  rtx_insn *tmp;
	  for (tmp = BB_END (bb);
	       tmp != insn && tmp != LAST_AVAIL_CHECK_FAILURE (ptr);
	       tmp = PREV_INSN (tmp))
	    continue;
	  if (tmp == insn)
	    check_available = 0;
	}
      else
	check_available = store_killed_after (dest, ptr->pattern_regs, insn,
					      bb, regs_set_after,
					      &LAST_AVAIL_CHECK_FAILURE (ptr));
    }
  if (!check_available)
    ptr->avail_stores = alloc_INSN_LIST (insn, ptr->avail_stores);
}

/* Find available and anticipatable stores.  */

static int
compute_store_table (void)
{
  int ret;
  basic_block bb;
  rtx_insn *insn;
  rtx_insn *tmp;
  df_ref def;
  int *last_set_in, *already_set;
  struct st_expr * ptr, **prev_next_ptr_ptr;
  unsigned int max_gcse_regno = max_reg_num ();

  store_motion_mems = NULL;
  store_motion_mems_table = new hash_table<st_expr_hasher> (13);
  last_set_in = XCNEWVEC (int, max_gcse_regno);
  already_set = XNEWVEC (int, max_gcse_regno);

  /* Find all the stores we care about.  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      /* First compute the registers set in this block.  */
      FOR_BB_INSNS (bb, insn)
	{

	  if (! NONDEBUG_INSN_P (insn))
	    continue;

	  FOR_EACH_INSN_DEF (def, insn)
	    last_set_in[DF_REF_REGNO (def)] = INSN_UID (insn);
	}

      /* Now find the stores.  */
      memset (already_set, 0, sizeof (int) * max_gcse_regno);
      FOR_BB_INSNS (bb, insn)
	{
	  if (! NONDEBUG_INSN_P (insn))
	    continue;

	  FOR_EACH_INSN_DEF (def, insn)
	    already_set[DF_REF_REGNO (def)] = INSN_UID (insn);

	  /* Now that we've marked regs, look for stores.  */
	  find_moveable_store (insn, already_set, last_set_in);

	  /* Unmark regs that are no longer set.  */
	  FOR_EACH_INSN_DEF (def, insn)
	    if (last_set_in[DF_REF_REGNO (def)] == INSN_UID (insn))
	      last_set_in[DF_REF_REGNO (def)] = 0;
	}

      if (flag_checking)
	{
	  /* last_set_in should now be all-zero.  */
	  for (unsigned regno = 0; regno < max_gcse_regno; regno++)
	    gcc_assert (!last_set_in[regno]);
	}

      /* Clear temporary marks.  */
      for (ptr = first_st_expr (); ptr != NULL; ptr = next_st_expr (ptr))
	{
	  LAST_AVAIL_CHECK_FAILURE (ptr) = NULL_RTX;
	  if (ptr->antic_stores
	      && (tmp = ptr->antic_stores->insn ()) == NULL_RTX)
	    ptr->antic_stores = ptr->antic_stores->next ();
	}
    }

  /* Remove the stores that are not available anywhere, as there will
     be no opportunity to optimize them.  */
  for (ptr = store_motion_mems, prev_next_ptr_ptr = &store_motion_mems;
       ptr != NULL;
       ptr = *prev_next_ptr_ptr)
    {
      if (! ptr->avail_stores)
	{
	  *prev_next_ptr_ptr = ptr->next;
	  store_motion_mems_table->remove_elt_with_hash (ptr, ptr->hash_index);
	  free_st_expr_entry (ptr);
	}
      else
	prev_next_ptr_ptr = &ptr->next;
    }

  ret = enumerate_store_motion_mems ();

  if (dump_file)
    print_store_motion_mems (dump_file);

  free (last_set_in);
  free (already_set);
  return ret;
}

/* In all code following after this, REACHING_REG has its original
   meaning again.  Avoid confusion, and undef the accessor macro for
   the temporary marks usage in compute_store_table.  */
#undef LAST_AVAIL_CHECK_FAILURE

/* Insert an instruction at the beginning of a basic block, and update
   the BB_HEAD if needed.  */

static void
insert_insn_start_basic_block (rtx_insn *insn, basic_block bb)
{
  /* Insert at start of successor block.  */
  rtx_insn *prev = PREV_INSN (BB_HEAD (bb));
  rtx_insn *before = BB_HEAD (bb);
  while (before != 0)
    {
      if (! LABEL_P (before)
	  && !NOTE_INSN_BASIC_BLOCK_P (before))
	break;
      prev = before;
      if (prev == BB_END (bb))
	break;
      before = NEXT_INSN (before);
    }

  insn = emit_insn_after_noloc (insn, prev, bb);

  if (dump_file)
    {
      fprintf (dump_file, "STORE_MOTION  insert store at start of BB %d:\n",
	       bb->index);
      print_inline_rtx (dump_file, insn, 6);
      fprintf (dump_file, "\n");
    }
}

/* This routine will insert a store on an edge. EXPR is the st_expr entry for
   the memory reference, and E is the edge to insert it on.  Returns nonzero
   if an edge insertion was performed.  */

static int
insert_store (struct st_expr * expr, edge e)
{
  rtx reg;
  rtx_insn *insn;
  basic_block bb;
  edge tmp;
  edge_iterator ei;

  /* We did all the deleted before this insert, so if we didn't delete a
     store, then we haven't set the reaching reg yet either.  */
  if (expr->reaching_reg == NULL_RTX)
    return 0;

  if (e->flags & EDGE_FAKE)
    return 0;

  reg = expr->reaching_reg;
  insn = gen_move_insn (copy_rtx (expr->pattern), reg);

  /* If we are inserting this expression on ALL predecessor edges of a BB,
     insert it at the start of the BB, and reset the insert bits on the other
     edges so we don't try to insert it on the other edges.  */
  bb = e->dest;
  FOR_EACH_EDGE (tmp, ei, e->dest->preds)
    if (!(tmp->flags & EDGE_FAKE))
      {
	int index = EDGE_INDEX (edge_list, tmp->src, tmp->dest);

	gcc_assert (index != EDGE_INDEX_NO_EDGE);
	if (! bitmap_bit_p (st_insert_map[index], expr->index))
	  break;
      }

  /* If tmp is NULL, we found an insertion on every edge, blank the
     insertion vector for these edges, and insert at the start of the BB.  */
  if (!tmp && bb != EXIT_BLOCK_PTR_FOR_FN (cfun))
    {
      FOR_EACH_EDGE (tmp, ei, e->dest->preds)
	{
	  int index = EDGE_INDEX (edge_list, tmp->src, tmp->dest);
	  bitmap_clear_bit (st_insert_map[index], expr->index);
	}
      insert_insn_start_basic_block (insn, bb);
      return 0;
    }

  /* We can't put stores in the front of blocks pointed to by abnormal
     edges since that may put a store where one didn't used to be.  */
  gcc_assert (!(e->flags & EDGE_ABNORMAL));

  insert_insn_on_edge (insn, e);

  if (dump_file)
    {
      fprintf (dump_file, "STORE_MOTION  insert insn on edge (%d, %d):\n",
	       e->src->index, e->dest->index);
      print_inline_rtx (dump_file, insn, 6);
      fprintf (dump_file, "\n");
    }

  return 1;
}

/* Remove any REG_EQUAL or REG_EQUIV notes containing a reference to the
   memory location in SMEXPR set in basic block BB.

   This could be rather expensive.  */

static void
remove_reachable_equiv_notes (basic_block bb, struct st_expr *smexpr)
{
  edge_iterator *stack, ei;
  int sp;
  edge act;
  sbitmap visited = sbitmap_alloc (last_basic_block_for_fn (cfun));
  rtx last, note;
  rtx_insn *insn;
  rtx mem = smexpr->pattern;

  stack = XNEWVEC (edge_iterator, n_basic_blocks_for_fn (cfun));
  sp = 0;
  ei = ei_start (bb->succs);

  bitmap_clear (visited);

  act = (EDGE_COUNT (ei_container (ei)) > 0 ? EDGE_I (ei_container (ei), 0) : NULL);
  while (1)
    {
      if (!act)
	{
	  if (!sp)
	    {
	      free (stack);
	      sbitmap_free (visited);
	      return;
	    }
	  act = ei_edge (stack[--sp]);
	}
      bb = act->dest;

      if (bb == EXIT_BLOCK_PTR_FOR_FN (cfun)
	  || bitmap_bit_p (visited, bb->index))
	{
	  if (!ei_end_p (ei))
	      ei_next (&ei);
	  act = (! ei_end_p (ei)) ? ei_edge (ei) : NULL;
	  continue;
	}
      bitmap_set_bit (visited, bb->index);

      if (bitmap_bit_p (st_antloc[bb->index], smexpr->index))
	{
	  for (last = smexpr->antic_stores;
	       BLOCK_FOR_INSN (XEXP (last, 0)) != bb;
	       last = XEXP (last, 1))
	    continue;
	  last = XEXP (last, 0);
	}
      else
	last = NEXT_INSN (BB_END (bb));

      for (insn = BB_HEAD (bb); insn != last; insn = NEXT_INSN (insn))
	if (NONDEBUG_INSN_P (insn))
	  {
	    note = find_reg_equal_equiv_note (insn);
	    if (!note || !exp_equiv_p (XEXP (note, 0), mem, 0, true))
	      continue;

	    if (dump_file)
	      fprintf (dump_file, "STORE_MOTION  drop REG_EQUAL note at insn %d:\n",
		       INSN_UID (insn));
	    remove_note (insn, note);
	  }

      if (!ei_end_p (ei))
	ei_next (&ei);
      act = (! ei_end_p (ei)) ? ei_edge (ei) : NULL;

      if (EDGE_COUNT (bb->succs) > 0)
	{
	  if (act)
	    stack[sp++] = ei;
	  ei = ei_start (bb->succs);
	  act = (EDGE_COUNT (ei_container (ei)) > 0 ? EDGE_I (ei_container (ei), 0) : NULL);
	}
    }
}

/* This routine will replace a store with a SET to a specified register.  */

static void
replace_store_insn (rtx reg, rtx_insn *del, basic_block bb,
		    struct st_expr *smexpr)
{
  rtx_insn *insn;
  rtx mem, note, set, ptr;

  mem = smexpr->pattern;
  insn = gen_move_insn (reg, SET_SRC (single_set (del)));

  for (ptr = smexpr->antic_stores; ptr; ptr = XEXP (ptr, 1))
    if (XEXP (ptr, 0) == del)
      {
	XEXP (ptr, 0) = insn;
	break;
      }

  /* Move the notes from the deleted insn to its replacement.  */
  REG_NOTES (insn) = REG_NOTES (del);

  /* Emit the insn AFTER all the notes are transferred.
     This is cheaper since we avoid df rescanning for the note change.  */
  insn = emit_insn_after (insn, del);

  if (dump_file)
    {
      fprintf (dump_file,
	       "STORE_MOTION  delete insn in BB %d:\n      ", bb->index);
      print_inline_rtx (dump_file, del, 6);
      fprintf (dump_file, "\nSTORE_MOTION  replaced with insn:\n      ");
      print_inline_rtx (dump_file, insn, 6);
      fprintf (dump_file, "\n");
    }

  delete_insn (del);

  /* Now we must handle REG_EQUAL notes whose contents is equal to the mem;
     they are no longer accurate provided that they are reached by this
     definition, so drop them.  */
  for (; insn != NEXT_INSN (BB_END (bb)); insn = NEXT_INSN (insn))
    if (NONDEBUG_INSN_P (insn))
      {
	set = single_set (insn);
	if (!set)
	  continue;
	if (exp_equiv_p (SET_DEST (set), mem, 0, true))
	  return;
	note = find_reg_equal_equiv_note (insn);
	if (!note || !exp_equiv_p (XEXP (note, 0), mem, 0, true))
	  continue;

	if (dump_file)
	  fprintf (dump_file, "STORE_MOTION  drop REG_EQUAL note at insn %d:\n",
		   INSN_UID (insn));
	remove_note (insn, note);
      }
  remove_reachable_equiv_notes (bb, smexpr);
}


/* Delete a store, but copy the value that would have been stored into
   the reaching_reg for later storing.  */

static void
delete_store (struct st_expr * expr, basic_block bb)
{
  rtx reg;

  if (expr->reaching_reg == NULL_RTX)
    expr->reaching_reg = gen_reg_rtx_and_attrs (expr->pattern);

  reg = expr->reaching_reg;

  for (rtx_insn_list *i = expr->avail_stores; i; i = i->next ())
    {
      rtx_insn *del = i->insn ();
      if (BLOCK_FOR_INSN (del) == bb)
	{
	  /* We know there is only one since we deleted redundant
	     ones during the available computation.  */
	  replace_store_insn (reg, del, bb, expr);
	  break;
	}
    }
}

/* Fill in available, anticipatable, transparent and kill vectors in
   STORE_DATA, based on lists of available and anticipatable stores.  */
static void
build_store_vectors (void)
{
  basic_block bb;
  int *regs_set_in_block;
  rtx_insn *insn;
  rtx_insn_list *st;
  struct st_expr * ptr;
  unsigned int max_gcse_regno = max_reg_num ();

  /* Build the gen_vector. This is any store in the table which is not killed
     by aliasing later in its block.  */
  st_avloc = sbitmap_vector_alloc (last_basic_block_for_fn (cfun),
				   num_stores);
  bitmap_vector_clear (st_avloc, last_basic_block_for_fn (cfun));

  st_antloc = sbitmap_vector_alloc (last_basic_block_for_fn (cfun),
				    num_stores);
  bitmap_vector_clear (st_antloc, last_basic_block_for_fn (cfun));

  for (ptr = first_st_expr (); ptr != NULL; ptr = next_st_expr (ptr))
    {
      for (st = ptr->avail_stores; st != NULL; st = st->next ())
	{
	  insn = st->insn ();
	  bb = BLOCK_FOR_INSN (insn);

	  /* If we've already seen an available expression in this block,
	     we can delete this one (It occurs earlier in the block). We'll
	     copy the SRC expression to an unused register in case there
	     are any side effects.  */
	  if (bitmap_bit_p (st_avloc[bb->index], ptr->index))
	    {
	      rtx r = gen_reg_rtx_and_attrs (ptr->pattern);
	      if (dump_file)
		fprintf (dump_file, "Removing redundant store:\n");
	      replace_store_insn (r, st->insn (), bb, ptr);
	      continue;
	    }
	  bitmap_set_bit (st_avloc[bb->index], ptr->index);
	}

      for (st = ptr->antic_stores; st != NULL; st = st->next ())
	{
	  insn = st->insn ();
	  bb = BLOCK_FOR_INSN (insn);
	  bitmap_set_bit (st_antloc[bb->index], ptr->index);
	}
    }

  st_kill = sbitmap_vector_alloc (last_basic_block_for_fn (cfun), num_stores);
  bitmap_vector_clear (st_kill, last_basic_block_for_fn (cfun));

  st_transp = sbitmap_vector_alloc (last_basic_block_for_fn (cfun), num_stores);
  bitmap_vector_clear (st_transp, last_basic_block_for_fn (cfun));
  regs_set_in_block = XNEWVEC (int, max_gcse_regno);

  FOR_EACH_BB_FN (bb, cfun)
    {
      memset (regs_set_in_block, 0, sizeof (int) * max_gcse_regno);

      FOR_BB_INSNS (bb, insn)
	if (NONDEBUG_INSN_P (insn))
	  {
	    df_ref def;
	    FOR_EACH_INSN_DEF (def, insn)
	      {
		unsigned int ref_regno = DF_REF_REGNO (def);
		if (ref_regno < max_gcse_regno)
		  regs_set_in_block[DF_REF_REGNO (def)] = 1;
	      }
	  }

      for (ptr = first_st_expr (); ptr != NULL; ptr = next_st_expr (ptr))
	{
	  if (store_killed_after (ptr->pattern, ptr->pattern_regs, BB_HEAD (bb),
				  bb, regs_set_in_block, NULL))
	    {
	      /* It should not be necessary to consider the expression
		 killed if it is both anticipatable and available.  */
	      if (!bitmap_bit_p (st_antloc[bb->index], ptr->index)
		  || !bitmap_bit_p (st_avloc[bb->index], ptr->index))
		bitmap_set_bit (st_kill[bb->index], ptr->index);
	    }
	  else
	    bitmap_set_bit (st_transp[bb->index], ptr->index);
	}
    }

  free (regs_set_in_block);

  if (dump_file)
    {
      dump_bitmap_vector (dump_file, "st_antloc", "", st_antloc,
			  last_basic_block_for_fn (cfun));
      dump_bitmap_vector (dump_file, "st_kill", "", st_kill,
			  last_basic_block_for_fn (cfun));
      dump_bitmap_vector (dump_file, "st_transp", "", st_transp,
			  last_basic_block_for_fn (cfun));
      dump_bitmap_vector (dump_file, "st_avloc", "", st_avloc,
			  last_basic_block_for_fn (cfun));
    }
}

/* Free memory used by store motion.  */

static void
free_store_memory (void)
{
  free_store_motion_mems ();

  if (st_avloc)
    sbitmap_vector_free (st_avloc);
  if (st_kill)
    sbitmap_vector_free (st_kill);
  if (st_transp)
    sbitmap_vector_free (st_transp);
  if (st_antloc)
    sbitmap_vector_free (st_antloc);
  if (st_insert_map)
    sbitmap_vector_free (st_insert_map);
  if (st_delete_map)
    sbitmap_vector_free (st_delete_map);

  st_avloc = st_kill = st_transp = st_antloc = NULL;
  st_insert_map = st_delete_map = NULL;
}

/* Perform store motion. Much like gcse, except we move expressions the
   other way by looking at the flowgraph in reverse.
   Return non-zero if transformations are performed by the pass.  */

static int
one_store_motion_pass (void)
{
  basic_block bb;
  int x;
  struct st_expr * ptr;
  int did_edge_inserts = 0;
  int n_stores_deleted = 0;
  int n_stores_created = 0;

  init_alias_analysis ();

  /* Find all the available and anticipatable stores.  */
  num_stores = compute_store_table ();
  if (num_stores == 0)
    {
      delete store_motion_mems_table;
      store_motion_mems_table = NULL;
      end_alias_analysis ();
      return 0;
    }

  /* Now compute kill & transp vectors.  */
  build_store_vectors ();
  add_noreturn_fake_exit_edges ();
  connect_infinite_loops_to_exit ();

  edge_list = pre_edge_rev_lcm (num_stores, st_transp, st_avloc,
				st_antloc, st_kill, &st_insert_map,
				&st_delete_map);

  /* Now we want to insert the new stores which are going to be needed.  */
  for (ptr = first_st_expr (); ptr != NULL; ptr = next_st_expr (ptr))
    {
      /* If any of the edges we have above are abnormal, we can't move this
	 store.  */
      for (x = NUM_EDGES (edge_list) - 1; x >= 0; x--)
	if (bitmap_bit_p (st_insert_map[x], ptr->index)
	    && (INDEX_EDGE (edge_list, x)->flags & EDGE_ABNORMAL))
	  break;

      if (x >= 0)
	{
	  if (dump_file != NULL)
	    fprintf (dump_file,
		     "Can't replace store %d: abnormal edge from %d to %d\n",
		     ptr->index, INDEX_EDGE (edge_list, x)->src->index,
		     INDEX_EDGE (edge_list, x)->dest->index);
	  continue;
	}

      /* Now we want to insert the new stores which are going to be needed.  */

      FOR_EACH_BB_FN (bb, cfun)
	if (bitmap_bit_p (st_delete_map[bb->index], ptr->index))
	  {
	    delete_store (ptr, bb);
	    n_stores_deleted++;
	  }

      for (x = 0; x < NUM_EDGES (edge_list); x++)
	if (bitmap_bit_p (st_insert_map[x], ptr->index))
	  {
	    did_edge_inserts |= insert_store (ptr, INDEX_EDGE (edge_list, x));
	    n_stores_created++;
	  }
    }

  if (did_edge_inserts)
    commit_edge_insertions ();

  free_store_memory ();
  free_edge_list (edge_list);
  remove_fake_exit_edges ();
  end_alias_analysis ();

  if (dump_file)
    {
      fprintf (dump_file, "STORE_MOTION of %s, %d basic blocks, ",
	       current_function_name (), n_basic_blocks_for_fn (cfun));
      fprintf (dump_file, "%d insns deleted, %d insns created\n",
	       n_stores_deleted, n_stores_created);
    }

  return (n_stores_deleted > 0 || n_stores_created > 0);
}


static unsigned int
execute_rtl_store_motion (void)
{
  delete_unreachable_blocks ();
  df_analyze ();
  flag_rerun_cse_after_global_opts |= one_store_motion_pass ();
  return 0;
}

namespace {

const pass_data pass_data_rtl_store_motion =
{
  RTL_PASS, /* type */
  "store_motion", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_LSM, /* tv_id */
  PROP_cfglayout, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_rtl_store_motion : public rtl_opt_pass
{
public:
  pass_rtl_store_motion (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_rtl_store_motion, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *);
  virtual unsigned int execute (function *)
    {
      return execute_rtl_store_motion ();
    }

}; // class pass_rtl_store_motion

bool
pass_rtl_store_motion::gate (function *fun)
{
  return optimize > 0 && flag_gcse_sm
    && !fun->calls_setjmp
    && optimize_function_for_speed_p (fun)
    && dbg_cnt (store_motion);
}

} // anon namespace

rtl_opt_pass *
make_pass_rtl_store_motion (gcc::context *ctxt)
{
  return new pass_rtl_store_motion (ctxt);
}
