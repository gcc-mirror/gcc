/* Store motion via Lazy Code Motion on the reverse CFG.
   Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
   2006, 2007, 2008, 2009 Free Software Foundation, Inc.

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
#include "tree.h"
#include "tm_p.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "real.h"
#include "insn-config.h"
#include "recog.h"
#include "basic-block.h"
#include "output.h"
#include "function.h"
#include "expr.h"
#include "except.h"
#include "ggc.h"
#include "params.h"
#include "intl.h"
#include "timevar.h"
#include "tree-pass.h"
#include "hashtab.h"
#include "df.h"
#include "dbgcnt.h"


/* This is a list of expressions which are MEMs and will be used by load
   or store motion.
   Load motion tracks MEMs which aren't killed by
   anything except itself. (i.e., loads and stores to a single location).
   We can then allow movement of these MEM refs with a little special
   allowance. (all stores copy the same value to the reaching reg used
   for the loads).  This means all values used to store into memory must have
   no side effects so we can re-issue the setter value.
   Store Motion uses this structure as an expression table to track stores
   which look interesting, and might be moveable towards the exit block.  */

struct ls_expr
{
  rtx pattern;			/* Pattern of this mem.  */
  rtx pattern_regs;		/* List of registers mentioned by the mem.  */
  rtx loads;			/* INSN list of loads seen.  */
  rtx stores;			/* INSN list of stores seen.  */
  struct ls_expr * next;	/* Next in the list.  */
  int invalid;			/* Invalid for some reason.  */
  int index;			/* If it maps to a bitmap index.  */
  unsigned int hash_index;	/* Index when in a hash table.  */
  rtx reaching_reg;		/* Register to use when re-writing.  */
};

/* Head of the list of load/store memory refs.  */
static struct ls_expr * pre_ldst_mems = NULL;

/* Hashtable for the load/store memory refs.  */
static htab_t pre_ldst_table = NULL;

/* Various variables for statistics gathering.  */

/* GCSE substitutions made.  */
static int gcse_subst_count;
/* Number of copy instructions created.  */
static int gcse_create_count;
/* For available exprs */
static sbitmap *ae_kill, *ae_gen;

/* Nonzero for expressions that are transparent in the block.  */
static sbitmap *transp;

/* Nonzero for expressions which should be inserted on a specific edge.  */
static sbitmap *pre_insert_map;

/* Nonzero for expressions which should be deleted in a specific block.  */
static sbitmap *pre_delete_map;

/* Contains the edge_list returned by pre_edge_lcm.  */
static struct edge_list *edge_list;

/*  Here we provide the things required to do store motion towards
    the exit. In order for this to be effective, PRE load motion also needed
    to be taught how to move a load when it is kill only by a store to itself.

	    int i;
	    float a[10];

	    void foo(float scale)
	    {
	      for (i=0; i<10; i++)
		a[i] *= scale;
	    }

    'i' is both loaded and stored to in the loop. Normally, gcse cannot move
    the load out since its live around the loop, and stored at the bottom
    of the loop.

      The 'Load Motion' referred to and implemented in this file is
    an enhancement to gcse which when using edge based lcm, recognizes
    this situation and allows gcse to move the load out of the loop.

      Once gcse has hoisted the load, store motion can then push this
    load towards the exit, and we end up with no loads or stores of 'i'
    in the loop.  */

static hashval_t
pre_ldst_expr_hash (const void *p)
{
  int do_not_record_p = 0;
  const struct ls_expr *const x = (const struct ls_expr *) p;
  return hash_rtx (x->pattern, GET_MODE (x->pattern), &do_not_record_p, NULL, false);
}

static int
pre_ldst_expr_eq (const void *p1, const void *p2)
{
  const struct ls_expr *const ptr1 = (const struct ls_expr *) p1,
    *const ptr2 = (const struct ls_expr *) p2;
  return exp_equiv_p (ptr1->pattern, ptr2->pattern, 0, true);
}

/* This will search the ldst list for a matching expression. If it
   doesn't find one, we create one and initialize it.  */

static struct ls_expr *
ldst_entry (rtx x)
{
  int do_not_record_p = 0;
  struct ls_expr * ptr;
  unsigned int hash;
  void **slot;
  struct ls_expr e;

  hash = hash_rtx (x, GET_MODE (x), &do_not_record_p,
		   NULL,  /*have_reg_qty=*/false);

  e.pattern = x;
  slot = htab_find_slot_with_hash (pre_ldst_table, &e, hash, INSERT);
  if (*slot)
    return (struct ls_expr *)*slot;

  ptr = XNEW (struct ls_expr);

  ptr->next         = pre_ldst_mems;
  ptr->pattern      = x;
  ptr->pattern_regs = NULL_RTX;
  ptr->loads        = NULL_RTX;
  ptr->stores       = NULL_RTX;
  ptr->reaching_reg = NULL_RTX;
  ptr->invalid      = 0;
  ptr->index        = 0;
  ptr->hash_index   = hash;
  pre_ldst_mems     = ptr;
  *slot = ptr;

  return ptr;
}

/* Free up an individual ldst entry.  */

static void
free_ldst_entry (struct ls_expr * ptr)
{
  free_INSN_LIST_list (& ptr->loads);
  free_INSN_LIST_list (& ptr->stores);

  free (ptr);
}

/* Free up all memory associated with the ldst list.  */

static void
free_ldst_mems (void)
{
  if (pre_ldst_table)
    htab_delete (pre_ldst_table);
  pre_ldst_table = NULL;

  while (pre_ldst_mems)
    {
      struct ls_expr * tmp = pre_ldst_mems;

      pre_ldst_mems = pre_ldst_mems->next;

      free_ldst_entry (tmp);
    }

  pre_ldst_mems = NULL;
}

/* Assign each element of the list of mems a monotonically increasing value.  */

static int
enumerate_ldsts (void)
{
  struct ls_expr * ptr;
  int n = 0;

  for (ptr = pre_ldst_mems; ptr != NULL; ptr = ptr->next)
    ptr->index = n++;

  return n;
}

/* Return first item in the list.  */

static inline struct ls_expr *
first_ls_expr (void)
{
  return pre_ldst_mems;
}

/* Return the next item in the list after the specified one.  */

static inline struct ls_expr *
next_ls_expr (struct ls_expr * ptr)
{
  return ptr->next;
}

/* Dump debugging info about the ldst list.  */

static void
print_ldst_list (FILE * file)
{
  struct ls_expr * ptr;

  fprintf (file, "LDST list: \n");

  for (ptr = first_ls_expr (); ptr != NULL; ptr = next_ls_expr (ptr))
    {
      fprintf (file, "  Pattern (%3d): ", ptr->index);

      print_rtl (file, ptr->pattern);

      fprintf (file, "\n	 Loads : ");

      if (ptr->loads)
	print_rtl (file, ptr->loads);
      else
	fprintf (file, "(nil)");

      fprintf (file, "\n	Stores : ");

      if (ptr->stores)
	print_rtl (file, ptr->stores);
      else
	fprintf (file, "(nil)");

      fprintf (file, "\n\n");
    }

  fprintf (file, "\n");
}

/* Store motion code.  */

#define ANTIC_STORE_LIST(x)		((x)->loads)
#define AVAIL_STORE_LIST(x)		((x)->stores)
#define LAST_AVAIL_CHECK_FAILURE(x)	((x)->reaching_reg)

/* This is used to communicate the target bitvector we want to use in the
   reg_set_info routine when called via the note_stores mechanism.  */
static int * regvec;

/* And current insn, for the same routine.  */
static rtx compute_store_table_current_insn;

/* Used in computing the reverse edge graph bit vectors.  */
static sbitmap * st_antloc;

/* Global holding the number of store expressions we are dealing with.  */
static int num_stores;

/* Checks to set if we need to mark a register set.  Called from
   note_stores.  */

static void
reg_set_info (rtx dest, const_rtx setter ATTRIBUTE_UNUSED,
	      void *data ATTRIBUTE_UNUSED)
{
  if (GET_CODE (dest) == SUBREG)
    dest = SUBREG_REG (dest);

  if (REG_P (dest))
    regvec[REGNO (dest)] = INSN_UID (compute_store_table_current_insn);
}

/* Clear any mark that says that this insn sets dest.  Called from
   note_stores.  */

static void
reg_clear_last_set (rtx dest, const_rtx setter ATTRIBUTE_UNUSED,
	      void *data)
{
  int *dead_vec = (int *) data;

  if (GET_CODE (dest) == SUBREG)
    dest = SUBREG_REG (dest);

  if (REG_P (dest) &&
      dead_vec[REGNO (dest)] == INSN_UID (compute_store_table_current_insn))
    dead_vec[REGNO (dest)] = 0;
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
      if (regs_set[REGNO(reg)])
	return false;
    }

  return true;
}

/* Helper for extract_mentioned_regs; ACCUM is used to accumulate used
   registers.  */
static rtx
extract_mentioned_regs_helper (rtx x, rtx accum)
{
  int i;
  enum rtx_code code;
  const char * fmt;

  /* Repeat is used to turn tail-recursion into iteration.  */
 repeat:

  if (x == 0)
    return accum;

  code = GET_CODE (x);
  switch (code)
    {
    case REG:
      return alloc_EXPR_LIST (0, x, accum);

    case MEM:
      x = XEXP (x, 0);
      goto repeat;

    case PRE_DEC:
    case PRE_INC:
    case PRE_MODIFY:
    case POST_DEC:
    case POST_INC:
    case POST_MODIFY:
      /* We do not run this function with arguments having side effects.  */
      gcc_unreachable ();

    case PC:
    case CC0: /*FIXME*/
    case CONST:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_FIXED:
    case CONST_VECTOR:
    case SYMBOL_REF:
    case LABEL_REF:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      return accum;

    default:
      break;
    }

  i = GET_RTX_LENGTH (code) - 1;
  fmt = GET_RTX_FORMAT (code);

  for (; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  rtx tem = XEXP (x, i);

	  /* If we are about to do the last recursive call
	     needed at this level, change it into iteration.  */
	  if (i == 0)
	    {
	      x = tem;
	      goto repeat;
	    }

	  accum = extract_mentioned_regs_helper (tem, accum);
	}
      else if (fmt[i] == 'E')
	{
	  int j;

	  for (j = 0; j < XVECLEN (x, i); j++)
	    accum = extract_mentioned_regs_helper (XVECEXP (x, i, j), accum);
	}
    }

  return accum;
}

/* Returns a list of registers mentioned in X.  */
/* ??? Reimplement with for_each_rtx?  */
static rtx
extract_mentioned_regs (rtx x)
{
  return extract_mentioned_regs_helper (x, NULL_RTX);
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
    return true_dependence (store_pattern, GET_MODE (store_pattern), x,
			    rtx_addr_varies_p);
}

/* Go through the entire insn X, looking for any loads which might alias
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
store_killed_in_insn (const_rtx x, const_rtx x_regs, const_rtx insn, int after)
{
  const_rtx reg, base, note, pat;

  if (!INSN_P (insn))
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
	{
	  base = find_base_term (XEXP (reg, 0));
	  if (!base
	      || (GET_CODE (base) == ADDRESS
		  && GET_MODE (base) == Pmode
		  && XEXP (base, 0) == stack_pointer_rtx))
	    return true;
	}

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
store_killed_after (const_rtx x, const_rtx x_regs, const_rtx insn, const_basic_block bb,
		    int *regs_set_after, rtx *fail_insn)
{
  rtx last = BB_END (bb), act;

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
store_killed_before (const_rtx x, const_rtx x_regs, const_rtx insn, const_basic_block bb,
		     int *regs_set_before)
{
  rtx first = BB_HEAD (bb);

  if (!store_ops_ok (x_regs, regs_set_before))
    return true;

  for ( ; insn != PREV_INSN (first); insn = PREV_INSN (insn))
    if (store_killed_in_insn (x, x_regs, insn, true))
      return true;

  return false;
}

/* Determine whether INSN is MEM store pattern that we will consider moving.
   REGS_SET_BEFORE is bitmap of registers set before (and including) the
   current insn, REGS_SET_AFTER is bitmap of registers set after (and
   including) the insn in this basic block.  We must be passing through BB from
   head to end, as we are using this fact to speed things up.

   The results are stored this way:

   -- the first anticipatable expression is added into ANTIC_STORE_LIST
   -- if the processed expression is not anticipatable, NULL_RTX is added
      there instead, so that we can use it as indicator that no further
      expression of this type may be anticipatable
   -- if the expression is available, it is added as head of AVAIL_STORE_LIST;
      consequently, all of them but this head are dead and may be deleted.
   -- if the expression is not available, the insn due to that it fails to be
      available is stored in reaching_reg.

   The things are complicated a bit by fact that there already may be stores
   to the same MEM from other blocks; also caller must take care of the
   necessary cleanup of the temporary markers after end of the basic block.
   */

static void
find_moveable_store (rtx insn, int *regs_set_before, int *regs_set_after)
{
  struct ls_expr * ptr;
  rtx dest, set, tmp;
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
     that may trap. If we are not, the behavior is undefined, so we may just
     continue.  */
  if (flag_non_call_exceptions && may_trap_p (dest))
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
  if (!can_assign_to_reg_without_clobbers_p (SET_SRC (set)))
    return;

  ptr = ldst_entry (dest);
  if (!ptr->pattern_regs)
    ptr->pattern_regs = extract_mentioned_regs (dest);

  /* Do not check for anticipatability if we either found one anticipatable
     store already, or tested for one and found out that it was killed.  */
  check_anticipatable = 0;
  if (!ANTIC_STORE_LIST (ptr))
    check_anticipatable = 1;
  else
    {
      tmp = XEXP (ANTIC_STORE_LIST (ptr), 0);
      if (tmp != NULL_RTX
	  && BLOCK_FOR_INSN (tmp) != bb)
	check_anticipatable = 1;
    }
  if (check_anticipatable)
    {
      if (store_killed_before (dest, ptr->pattern_regs, insn, bb, regs_set_before))
	tmp = NULL_RTX;
      else
	tmp = insn;
      ANTIC_STORE_LIST (ptr) = alloc_INSN_LIST (tmp,
						ANTIC_STORE_LIST (ptr));
    }

  /* It is not necessary to check whether store is available if we did
     it successfully before; if we failed before, do not bother to check
     until we reach the insn that caused us to fail.  */
  check_available = 0;
  if (!AVAIL_STORE_LIST (ptr))
    check_available = 1;
  else
    {
      tmp = XEXP (AVAIL_STORE_LIST (ptr), 0);
      if (BLOCK_FOR_INSN (tmp) != bb)
	check_available = 1;
    }
  if (check_available)
    {
      /* Check that we have already reached the insn at that the check
	 failed last time.  */
      if (LAST_AVAIL_CHECK_FAILURE (ptr))
	{
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
    AVAIL_STORE_LIST (ptr) = alloc_INSN_LIST (insn, AVAIL_STORE_LIST (ptr));
}

/* Find available and anticipatable stores.  */

static int
compute_store_table (void)
{
  int ret;
  basic_block bb;
  unsigned regno;
  rtx insn, pat, tmp;
  int *last_set_in, *already_set;
  struct ls_expr * ptr, **prev_next_ptr_ptr;
  unsigned int max_gcse_regno = max_reg_num ();

  pre_ldst_mems = 0;
  pre_ldst_table = htab_create (13, pre_ldst_expr_hash,
				pre_ldst_expr_eq, NULL);
  last_set_in = XCNEWVEC (int, max_gcse_regno);
  already_set = XNEWVEC (int, max_gcse_regno);

  /* Find all the stores we care about.  */
  FOR_EACH_BB (bb)
    {
      /* First compute the registers set in this block.  */
      regvec = last_set_in;

      FOR_BB_INSNS (bb, insn)
	{
	  if (! INSN_P (insn))
	    continue;

	  if (CALL_P (insn))
	    {
	      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
		if (TEST_HARD_REG_BIT (regs_invalidated_by_call, regno))
		  last_set_in[regno] = INSN_UID (insn);
	    }

	  pat = PATTERN (insn);
	  compute_store_table_current_insn = insn;
	  note_stores (pat, reg_set_info, NULL);
	}

      /* Now find the stores.  */
      memset (already_set, 0, sizeof (int) * max_gcse_regno);
      regvec = already_set;
      FOR_BB_INSNS (bb, insn)
	{
	  if (! INSN_P (insn))
	    continue;

	  if (CALL_P (insn))
	    {
	      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
		if (TEST_HARD_REG_BIT (regs_invalidated_by_call, regno))
		  already_set[regno] = 1;
	    }

	  pat = PATTERN (insn);
	  note_stores (pat, reg_set_info, NULL);

	  /* Now that we've marked regs, look for stores.  */
	  find_moveable_store (insn, already_set, last_set_in);

	  /* Unmark regs that are no longer set.  */
	  compute_store_table_current_insn = insn;
	  note_stores (pat, reg_clear_last_set, last_set_in);
	  if (CALL_P (insn))
	    {
	      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
		if (TEST_HARD_REG_BIT (regs_invalidated_by_call, regno)
		    && last_set_in[regno] == INSN_UID (insn))
		  last_set_in[regno] = 0;
	    }
	}

#ifdef ENABLE_CHECKING
      /* last_set_in should now be all-zero.  */
      for (regno = 0; regno < max_gcse_regno; regno++)
	gcc_assert (!last_set_in[regno]);
#endif

      /* Clear temporary marks.  */
      for (ptr = first_ls_expr (); ptr != NULL; ptr = next_ls_expr (ptr))
	{
	  LAST_AVAIL_CHECK_FAILURE(ptr) = NULL_RTX;
	  if (ANTIC_STORE_LIST (ptr)
	      && (tmp = XEXP (ANTIC_STORE_LIST (ptr), 0)) == NULL_RTX)
	    ANTIC_STORE_LIST (ptr) = XEXP (ANTIC_STORE_LIST (ptr), 1);
	}
    }

  /* Remove the stores that are not available anywhere, as there will
     be no opportunity to optimize them.  */
  for (ptr = pre_ldst_mems, prev_next_ptr_ptr = &pre_ldst_mems;
       ptr != NULL;
       ptr = *prev_next_ptr_ptr)
    {
      if (!AVAIL_STORE_LIST (ptr))
	{
	  *prev_next_ptr_ptr = ptr->next;
	  htab_remove_elt_with_hash (pre_ldst_table, ptr, ptr->hash_index);
	  free_ldst_entry (ptr);
	}
      else
	prev_next_ptr_ptr = &ptr->next;
    }

  ret = enumerate_ldsts ();

  if (dump_file)
    {
      fprintf (dump_file, "ST_avail and ST_antic (shown under loads..)\n");
      print_ldst_list (dump_file);
    }

  free (last_set_in);
  free (already_set);
  return ret;
}

/* Insert an instruction at the beginning of a basic block, and update
   the BB_HEAD if needed.  */

static void
insert_insn_start_basic_block (rtx insn, basic_block bb)
{
  /* Insert at start of successor block.  */
  rtx prev = PREV_INSN (BB_HEAD (bb));
  rtx before = BB_HEAD (bb);
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

/* This routine will insert a store on an edge. EXPR is the ldst entry for
   the memory reference, and E is the edge to insert it on.  Returns nonzero
   if an edge insertion was performed.  */

static int
insert_store (struct ls_expr * expr, edge e)
{
  rtx reg, insn;
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
	if (! TEST_BIT (pre_insert_map[index], expr->index))
	  break;
      }

  /* If tmp is NULL, we found an insertion on every edge, blank the
     insertion vector for these edges, and insert at the start of the BB.  */
  if (!tmp && bb != EXIT_BLOCK_PTR)
    {
      FOR_EACH_EDGE (tmp, ei, e->dest->preds)
	{
	  int index = EDGE_INDEX (edge_list, tmp->src, tmp->dest);
	  RESET_BIT (pre_insert_map[index], expr->index);
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
remove_reachable_equiv_notes (basic_block bb, struct ls_expr *smexpr)
{
  edge_iterator *stack, ei;
  int sp;
  edge act;
  sbitmap visited = sbitmap_alloc (last_basic_block);
  rtx last, insn, note;
  rtx mem = smexpr->pattern;

  stack = XNEWVEC (edge_iterator, n_basic_blocks);
  sp = 0;
  ei = ei_start (bb->succs);

  sbitmap_zero (visited);

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

      if (bb == EXIT_BLOCK_PTR
	  || TEST_BIT (visited, bb->index))
	{
	  if (!ei_end_p (ei))
	      ei_next (&ei);
	  act = (! ei_end_p (ei)) ? ei_edge (ei) : NULL;
	  continue;
	}
      SET_BIT (visited, bb->index);

      if (TEST_BIT (st_antloc[bb->index], smexpr->index))
	{
	  for (last = ANTIC_STORE_LIST (smexpr);
	       BLOCK_FOR_INSN (XEXP (last, 0)) != bb;
	       last = XEXP (last, 1))
	    continue;
	  last = XEXP (last, 0);
	}
      else
	last = NEXT_INSN (BB_END (bb));

      for (insn = BB_HEAD (bb); insn != last; insn = NEXT_INSN (insn))
	if (INSN_P (insn))
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
replace_store_insn (rtx reg, rtx del, basic_block bb, struct ls_expr *smexpr)
{
  rtx insn, mem, note, set, ptr;

  mem = smexpr->pattern;
  insn = gen_move_insn (reg, SET_SRC (single_set (del)));

  for (ptr = ANTIC_STORE_LIST (smexpr); ptr; ptr = XEXP (ptr, 1))
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
    if (INSN_P (insn))
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
delete_store (struct ls_expr * expr, basic_block bb)
{
  rtx reg, i, del;

  if (expr->reaching_reg == NULL_RTX)
    expr->reaching_reg = gen_reg_rtx_and_attrs (expr->pattern);

  reg = expr->reaching_reg;

  for (i = AVAIL_STORE_LIST (expr); i; i = XEXP (i, 1))
    {
      del = XEXP (i, 0);
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
  rtx insn, st;
  struct ls_expr * ptr;
  unsigned int max_gcse_regno = max_reg_num ();

  /* Build the gen_vector. This is any store in the table which is not killed
     by aliasing later in its block.  */
  ae_gen = sbitmap_vector_alloc (last_basic_block, num_stores);
  sbitmap_vector_zero (ae_gen, last_basic_block);

  st_antloc = sbitmap_vector_alloc (last_basic_block, num_stores);
  sbitmap_vector_zero (st_antloc, last_basic_block);

  for (ptr = first_ls_expr (); ptr != NULL; ptr = next_ls_expr (ptr))
    {
      for (st = AVAIL_STORE_LIST (ptr); st != NULL; st = XEXP (st, 1))
	{
	  insn = XEXP (st, 0);
	  bb = BLOCK_FOR_INSN (insn);

	  /* If we've already seen an available expression in this block,
	     we can delete this one (It occurs earlier in the block). We'll
	     copy the SRC expression to an unused register in case there
	     are any side effects.  */
	  if (TEST_BIT (ae_gen[bb->index], ptr->index))
	    {
	      rtx r = gen_reg_rtx_and_attrs (ptr->pattern);
	      if (dump_file)
		fprintf (dump_file, "Removing redundant store:\n");
	      replace_store_insn (r, XEXP (st, 0), bb, ptr);
	      continue;
	    }
	  SET_BIT (ae_gen[bb->index], ptr->index);
	}

      for (st = ANTIC_STORE_LIST (ptr); st != NULL; st = XEXP (st, 1))
	{
	  insn = XEXP (st, 0);
	  bb = BLOCK_FOR_INSN (insn);
	  SET_BIT (st_antloc[bb->index], ptr->index);
	}
    }

  ae_kill = sbitmap_vector_alloc (last_basic_block, num_stores);
  sbitmap_vector_zero (ae_kill, last_basic_block);

  transp = sbitmap_vector_alloc (last_basic_block, num_stores);
  sbitmap_vector_zero (transp, last_basic_block);
  regs_set_in_block = XNEWVEC (int, max_gcse_regno);

  FOR_EACH_BB (bb)
    {
      FOR_BB_INSNS (bb, insn)
	if (INSN_P (insn))
	  {
	    df_ref *def_rec;
	    for (def_rec = DF_INSN_DEFS (insn); *def_rec; def_rec++)
	      {
		unsigned int ref_regno = DF_REF_REGNO (*def_rec);
		if (ref_regno < max_gcse_regno)
		  regs_set_in_block[DF_REF_REGNO (*def_rec)] = 1;
	      }
	  }

      for (ptr = first_ls_expr (); ptr != NULL; ptr = next_ls_expr (ptr))
	{
	  if (store_killed_after (ptr->pattern, ptr->pattern_regs, BB_HEAD (bb),
				  bb, regs_set_in_block, NULL))
	    {
	      /* It should not be necessary to consider the expression
		 killed if it is both anticipatable and available.  */
	      if (!TEST_BIT (st_antloc[bb->index], ptr->index)
		  || !TEST_BIT (ae_gen[bb->index], ptr->index))
		SET_BIT (ae_kill[bb->index], ptr->index);
	    }
	  else
	    SET_BIT (transp[bb->index], ptr->index);
	}
    }

  free (regs_set_in_block);

  if (dump_file)
    {
      dump_sbitmap_vector (dump_file, "st_antloc", "", st_antloc, last_basic_block);
      dump_sbitmap_vector (dump_file, "st_kill", "", ae_kill, last_basic_block);
      dump_sbitmap_vector (dump_file, "Transpt", "", transp, last_basic_block);
      dump_sbitmap_vector (dump_file, "st_avloc", "", ae_gen, last_basic_block);
    }
}

/* Free memory used by store motion.  */

static void
free_store_memory (void)
{
  free_ldst_mems ();

  if (ae_gen)
    sbitmap_vector_free (ae_gen);
  if (ae_kill)
    sbitmap_vector_free (ae_kill);
  if (transp)
    sbitmap_vector_free (transp);
  if (st_antloc)
    sbitmap_vector_free (st_antloc);
  if (pre_insert_map)
    sbitmap_vector_free (pre_insert_map);
  if (pre_delete_map)
    sbitmap_vector_free (pre_delete_map);

  ae_gen = ae_kill = transp = st_antloc = NULL;
  pre_insert_map = pre_delete_map = NULL;
}

/* Perform store motion. Much like gcse, except we move expressions the
   other way by looking at the flowgraph in reverse.
   Return non-zero if transformations are performed by the pass.  */

static int
one_store_motion_pass (void)
{
  basic_block bb;
  int x;
  struct ls_expr * ptr;
  int update_flow = 0;

  gcse_subst_count = 0;
  gcse_create_count = 0;

  init_alias_analysis ();

  /* Find all the available and anticipatable stores.  */
  num_stores = compute_store_table ();
  if (num_stores == 0)
    {
      htab_delete (pre_ldst_table);
      pre_ldst_table = NULL;
      end_alias_analysis ();
      return 0;
    }

  /* Now compute kill & transp vectors.  */
  build_store_vectors ();
  add_noreturn_fake_exit_edges ();
  connect_infinite_loops_to_exit ();

  edge_list = pre_edge_rev_lcm (num_stores, transp, ae_gen,
				st_antloc, ae_kill, &pre_insert_map,
				&pre_delete_map);

  /* Now we want to insert the new stores which are going to be needed.  */
  for (ptr = first_ls_expr (); ptr != NULL; ptr = next_ls_expr (ptr))
    {
      /* If any of the edges we have above are abnormal, we can't move this
	 store.  */
      for (x = NUM_EDGES (edge_list) - 1; x >= 0; x--)
	if (TEST_BIT (pre_insert_map[x], ptr->index)
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

      FOR_EACH_BB (bb)
	if (TEST_BIT (pre_delete_map[bb->index], ptr->index))
	  {
	    delete_store (ptr, bb);
	    gcse_subst_count++;
	  }

      for (x = 0; x < NUM_EDGES (edge_list); x++)
	if (TEST_BIT (pre_insert_map[x], ptr->index))
	  {
	    update_flow |= insert_store (ptr, INDEX_EDGE (edge_list, x));
	    gcse_create_count++;
	  }
    }

  if (update_flow)
    commit_edge_insertions ();

  free_store_memory ();
  free_edge_list (edge_list);
  remove_fake_exit_edges ();
  end_alias_analysis ();

  if (dump_file)
    {
      fprintf (dump_file, "STORE_MOTION of %s, %d basic blocks, ",
	       current_function_name (), n_basic_blocks);
      fprintf (dump_file, "%d substs, %d insns created\n",
	       gcse_subst_count, gcse_create_count);
    }

  return (gcse_subst_count > 0 || gcse_create_count > 0);
}


static bool
gate_rtl_store_motion (void)
{
  return optimize > 0 && flag_gcse_sm
    && !cfun->calls_setjmp
    && optimize_function_for_speed_p (cfun)
    && dbg_cnt (store_motion);
}

static unsigned int
execute_rtl_store_motion (void)
{
  delete_unreachable_blocks ();
  df_note_add_problem ();
  df_analyze ();
  flag_rerun_cse_after_global_opts |= one_store_motion_pass ();
  return 0;
}

struct rtl_opt_pass pass_rtl_store_motion =
{
 {
  RTL_PASS,
  "store_motion",                       /* name */
  gate_rtl_store_motion,                /* gate */   
  execute_rtl_store_motion,		/* execute */       
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_LSM,                               /* tv_id */
  PROP_cfglayout,                       /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_df_finish | TODO_verify_rtl_sharing |
  TODO_dump_func |
  TODO_verify_flow | TODO_ggc_collect   /* todo_flags_finish */
 }
};

