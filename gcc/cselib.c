/* Common subexpression elimination library for GNU compiler.
   Copyright (C) 1987, 1988, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011,
   2012 Free Software Foundation, Inc.

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

#include "rtl.h"
#include "tm_p.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "insn-config.h"
#include "recog.h"
#include "function.h"
#include "emit-rtl.h"
#include "diagnostic-core.h"
#include "output.h"
#include "ggc.h"
#include "hashtab.h"
#include "tree-pass.h"
#include "cselib.h"
#include "params.h"
#include "alloc-pool.h"
#include "target.h"
#include "bitmap.h"

/* A list of cselib_val structures.  */
struct elt_list {
    struct elt_list *next;
    cselib_val *elt;
};

static bool cselib_record_memory;
static bool cselib_preserve_constants;
static int entry_and_rtx_equal_p (const void *, const void *);
static hashval_t get_value_hash (const void *);
static struct elt_list *new_elt_list (struct elt_list *, cselib_val *);
static void new_elt_loc_list (cselib_val *, rtx);
static void unchain_one_value (cselib_val *);
static void unchain_one_elt_list (struct elt_list **);
static void unchain_one_elt_loc_list (struct elt_loc_list **);
static int discard_useless_locs (void **, void *);
static int discard_useless_values (void **, void *);
static void remove_useless_values (void);
static int rtx_equal_for_cselib_1 (rtx, rtx, enum machine_mode);
static unsigned int cselib_hash_rtx (rtx, int, enum machine_mode);
static cselib_val *new_cselib_val (unsigned int, enum machine_mode, rtx);
static void add_mem_for_addr (cselib_val *, cselib_val *, rtx);
static cselib_val *cselib_lookup_mem (rtx, int);
static void cselib_invalidate_regno (unsigned int, enum machine_mode);
static void cselib_invalidate_mem (rtx);
static void cselib_record_set (rtx, cselib_val *, cselib_val *);
static void cselib_record_sets (rtx);

struct expand_value_data
{
  bitmap regs_active;
  cselib_expand_callback callback;
  void *callback_arg;
  bool dummy;
};

static rtx cselib_expand_value_rtx_1 (rtx, struct expand_value_data *, int);

/* There are three ways in which cselib can look up an rtx:
   - for a REG, the reg_values table (which is indexed by regno) is used
   - for a MEM, we recursively look up its address and then follow the
     addr_list of that value
   - for everything else, we compute a hash value and go through the hash
     table.  Since different rtx's can still have the same hash value,
     this involves walking the table entries for a given value and comparing
     the locations of the entries with the rtx we are looking up.  */

/* A table that enables us to look up elts by their value.  */
static htab_t cselib_hash_table;

/* This is a global so we don't have to pass this through every function.
   It is used in new_elt_loc_list to set SETTING_INSN.  */
static rtx cselib_current_insn;

/* The unique id that the next create value will take.  */
static unsigned int next_uid;

/* The number of registers we had when the varrays were last resized.  */
static unsigned int cselib_nregs;

/* Count values without known locations, or with only locations that
   wouldn't have been known except for debug insns.  Whenever this
   grows too big, we remove these useless values from the table.

   Counting values with only debug values is a bit tricky.  We don't
   want to increment n_useless_values when we create a value for a
   debug insn, for this would get n_useless_values out of sync, but we
   want increment it if all locs in the list that were ever referenced
   in nondebug insns are removed from the list.

   In the general case, once we do that, we'd have to stop accepting
   nondebug expressions in the loc list, to avoid having two values
   equivalent that, without debug insns, would have been made into
   separate values.  However, because debug insns never introduce
   equivalences themselves (no assignments), the only means for
   growing loc lists is through nondebug assignments.  If the locs
   also happen to be referenced in debug insns, it will work just fine.

   A consequence of this is that there's at most one debug-only loc in
   each loc list.  If we keep it in the first entry, testing whether
   we have a debug-only loc list takes O(1).

   Furthermore, since any additional entry in a loc list containing a
   debug loc would have to come from an assignment (nondebug) that
   references both the initial debug loc and the newly-equivalent loc,
   the initial debug loc would be promoted to a nondebug loc, and the
   loc list would not contain debug locs any more.

   So the only case we have to be careful with in order to keep
   n_useless_values in sync between debug and nondebug compilations is
   to avoid incrementing n_useless_values when removing the single loc
   from a value that turns out to not appear outside debug values.  We
   increment n_useless_debug_values instead, and leave such values
   alone until, for other reasons, we garbage-collect useless
   values.  */
static int n_useless_values;
static int n_useless_debug_values;

/* Count values whose locs have been taken exclusively from debug
   insns for the entire life of the value.  */
static int n_debug_values;

/* Number of useless values before we remove them from the hash table.  */
#define MAX_USELESS_VALUES 32

/* This table maps from register number to values.  It does not
   contain pointers to cselib_val structures, but rather elt_lists.
   The purpose is to be able to refer to the same register in
   different modes.  The first element of the list defines the mode in
   which the register was set; if the mode is unknown or the value is
   no longer valid in that mode, ELT will be NULL for the first
   element.  */
static struct elt_list **reg_values;
static unsigned int reg_values_size;
#define REG_VALUES(i) reg_values[i]

/* The largest number of hard regs used by any entry added to the
   REG_VALUES table.  Cleared on each cselib_clear_table() invocation.  */
static unsigned int max_value_regs;

/* Here the set of indices I with REG_VALUES(I) != 0 is saved.  This is used
   in cselib_clear_table() for fast emptying.  */
static unsigned int *used_regs;
static unsigned int n_used_regs;

/* We pass this to cselib_invalidate_mem to invalidate all of
   memory for a non-const call instruction.  */
static GTY(()) rtx callmem;

/* Set by discard_useless_locs if it deleted the last location of any
   value.  */
static int values_became_useless;

/* Used as stop element of the containing_mem list so we can check
   presence in the list by checking the next pointer.  */
static cselib_val dummy_val;

/* If non-NULL, value of the eliminated arg_pointer_rtx or frame_pointer_rtx
   that is constant through the whole function and should never be
   eliminated.  */
static cselib_val *cfa_base_preserved_val;
static unsigned int cfa_base_preserved_regno = INVALID_REGNUM;

/* Used to list all values that contain memory reference.
   May or may not contain the useless values - the list is compacted
   each time memory is invalidated.  */
static cselib_val *first_containing_mem = &dummy_val;
static alloc_pool elt_loc_list_pool, elt_list_pool, cselib_val_pool, value_pool;

/* If nonnull, cselib will call this function before freeing useless
   VALUEs.  A VALUE is deemed useless if its "locs" field is null.  */
void (*cselib_discard_hook) (cselib_val *);

/* If nonnull, cselib will call this function before recording sets or
   even clobbering outputs of INSN.  All the recorded sets will be
   represented in the array sets[n_sets].  new_val_min can be used to
   tell whether values present in sets are introduced by this
   instruction.  */
void (*cselib_record_sets_hook) (rtx insn, struct cselib_set *sets,
				 int n_sets);

#define PRESERVED_VALUE_P(RTX) \
  (RTL_FLAG_CHECK1("PRESERVED_VALUE_P", (RTX), VALUE)->unchanging)



/* Allocate a struct elt_list and fill in its two elements with the
   arguments.  */

static inline struct elt_list *
new_elt_list (struct elt_list *next, cselib_val *elt)
{
  struct elt_list *el;
  el = (struct elt_list *) pool_alloc (elt_list_pool);
  el->next = next;
  el->elt = elt;
  return el;
}

/* Allocate a struct elt_loc_list with LOC and prepend it to VAL's loc
   list.  */

static inline void
new_elt_loc_list (cselib_val *val, rtx loc)
{
  struct elt_loc_list *el, *next = val->locs;

  gcc_checking_assert (!next || !next->setting_insn
		       || !DEBUG_INSN_P (next->setting_insn)
		       || cselib_current_insn == next->setting_insn);

  /* If we're creating the first loc in a debug insn context, we've
     just created a debug value.  Count it.  */
  if (!next && cselib_current_insn && DEBUG_INSN_P (cselib_current_insn))
    n_debug_values++;

  val = canonical_cselib_val (val);
  next = val->locs;

  if (GET_CODE (loc) == VALUE)
    {
      loc = canonical_cselib_val (CSELIB_VAL_PTR (loc))->val_rtx;

      gcc_checking_assert (PRESERVED_VALUE_P (loc)
			   == PRESERVED_VALUE_P (val->val_rtx));

      if (val->val_rtx == loc)
	return;
      else if (val->uid > CSELIB_VAL_PTR (loc)->uid)
	{
	  /* Reverse the insertion.  */
	  new_elt_loc_list (CSELIB_VAL_PTR (loc), val->val_rtx);
	  return;
	}

      gcc_checking_assert (val->uid < CSELIB_VAL_PTR (loc)->uid);

      if (CSELIB_VAL_PTR (loc)->locs)
	{
	  /* Bring all locs from LOC to VAL.  */
	  for (el = CSELIB_VAL_PTR (loc)->locs; el->next; el = el->next)
	    {
	      /* Adjust values that have LOC as canonical so that VAL
		 becomes their canonical.  */
	      if (el->loc && GET_CODE (el->loc) == VALUE)
		{
		  gcc_checking_assert (CSELIB_VAL_PTR (el->loc)->locs->loc
				       == loc);
		  CSELIB_VAL_PTR (el->loc)->locs->loc = val->val_rtx;
		}
	    }
	  el->next = val->locs;
	  next = val->locs = CSELIB_VAL_PTR (loc)->locs;
	}

      if (CSELIB_VAL_PTR (loc)->addr_list)
	{
	  /* Bring in addr_list into canonical node.  */
	  struct elt_list *last = CSELIB_VAL_PTR (loc)->addr_list;
	  while (last->next)
	    last = last->next;
	  last->next = val->addr_list;
	  val->addr_list = CSELIB_VAL_PTR (loc)->addr_list;
	  CSELIB_VAL_PTR (loc)->addr_list = NULL;
	}

      if (CSELIB_VAL_PTR (loc)->next_containing_mem != NULL
	  && val->next_containing_mem == NULL)
	{
	  /* Add VAL to the containing_mem list after LOC.  LOC will
	     be removed when we notice it doesn't contain any
	     MEMs.  */
	  val->next_containing_mem = CSELIB_VAL_PTR (loc)->next_containing_mem;
	  CSELIB_VAL_PTR (loc)->next_containing_mem = val;
	}

      /* Chain LOC back to VAL.  */
      el = (struct elt_loc_list *) pool_alloc (elt_loc_list_pool);
      el->loc = val->val_rtx;
      el->setting_insn = cselib_current_insn;
      el->next = NULL;
      CSELIB_VAL_PTR (loc)->locs = el;
    }

  el = (struct elt_loc_list *) pool_alloc (elt_loc_list_pool);
  el->loc = loc;
  el->setting_insn = cselib_current_insn;
  el->next = next;
  val->locs = el;
}

/* Promote loc L to a nondebug cselib_current_insn if L is marked as
   originating from a debug insn, maintaining the debug values
   count.  */

static inline void
promote_debug_loc (struct elt_loc_list *l)
{
  if (l->setting_insn && DEBUG_INSN_P (l->setting_insn)
      && (!cselib_current_insn || !DEBUG_INSN_P (cselib_current_insn)))
    {
      n_debug_values--;
      l->setting_insn = cselib_current_insn;
      if (cselib_preserve_constants && l->next)
	{
	  gcc_assert (l->next->setting_insn
		      && DEBUG_INSN_P (l->next->setting_insn)
		      && !l->next->next);
	  l->next->setting_insn = cselib_current_insn;
	}
      else
	gcc_assert (!l->next);
    }
}

/* The elt_list at *PL is no longer needed.  Unchain it and free its
   storage.  */

static inline void
unchain_one_elt_list (struct elt_list **pl)
{
  struct elt_list *l = *pl;

  *pl = l->next;
  pool_free (elt_list_pool, l);
}

/* Likewise for elt_loc_lists.  */

static void
unchain_one_elt_loc_list (struct elt_loc_list **pl)
{
  struct elt_loc_list *l = *pl;

  *pl = l->next;
  pool_free (elt_loc_list_pool, l);
}

/* Likewise for cselib_vals.  This also frees the addr_list associated with
   V.  */

static void
unchain_one_value (cselib_val *v)
{
  while (v->addr_list)
    unchain_one_elt_list (&v->addr_list);

  pool_free (cselib_val_pool, v);
}

/* Remove all entries from the hash table.  Also used during
   initialization.  */

void
cselib_clear_table (void)
{
  cselib_reset_table (1);
}

/* Remove from hash table all VALUEs except constants
   and function invariants.  */

static int
preserve_only_constants (void **x, void *info ATTRIBUTE_UNUSED)
{
  cselib_val *v = (cselib_val *)*x;
  struct elt_loc_list *l;

  if (v->locs != NULL
      && v->locs->next == NULL)
    {
      if (CONSTANT_P (v->locs->loc)
	  && (GET_CODE (v->locs->loc) != CONST
	      || !references_value_p (v->locs->loc, 0)))
	return 1;
      /* Although a debug expr may be bound to different expressions,
	 we can preserve it as if it was constant, to get unification
	 and proper merging within var-tracking.  */
      if (GET_CODE (v->locs->loc) == DEBUG_EXPR
	  || GET_CODE (v->locs->loc) == DEBUG_IMPLICIT_PTR
	  || GET_CODE (v->locs->loc) == ENTRY_VALUE
	  || GET_CODE (v->locs->loc) == DEBUG_PARAMETER_REF)
	return 1;
      if (cfa_base_preserved_val)
	{
	  if (v == cfa_base_preserved_val)
	    return 1;
	  if (GET_CODE (v->locs->loc) == PLUS
	      && CONST_INT_P (XEXP (v->locs->loc, 1))
	      && XEXP (v->locs->loc, 0) == cfa_base_preserved_val->val_rtx)
	    return 1;
	}
    }

  /* Keep VALUE equivalences around.  */
  for (l = v->locs; l; l = l->next)
    if (GET_CODE (l->loc) == VALUE)
      return 1;

  htab_clear_slot (cselib_hash_table, x);
  return 1;
}

/* Remove all entries from the hash table, arranging for the next
   value to be numbered NUM.  */

void
cselib_reset_table (unsigned int num)
{
  unsigned int i;

  max_value_regs = 0;

  if (cfa_base_preserved_val)
    {
      unsigned int regno = cfa_base_preserved_regno;
      unsigned int new_used_regs = 0;
      for (i = 0; i < n_used_regs; i++)
	if (used_regs[i] == regno)
	  {
	    new_used_regs = 1;
	    continue;
	  }
	else
	  REG_VALUES (used_regs[i]) = 0;
      gcc_assert (new_used_regs == 1);
      n_used_regs = new_used_regs;
      used_regs[0] = regno;
      max_value_regs
	= hard_regno_nregs[regno][GET_MODE (cfa_base_preserved_val->locs->loc)];
    }
  else
    {
      for (i = 0; i < n_used_regs; i++)
	REG_VALUES (used_regs[i]) = 0;
      n_used_regs = 0;
    }

  if (cselib_preserve_constants)
    htab_traverse (cselib_hash_table, preserve_only_constants, NULL);
  else
    htab_empty (cselib_hash_table);

  n_useless_values = 0;
  n_useless_debug_values = 0;
  n_debug_values = 0;

  next_uid = num;

  first_containing_mem = &dummy_val;
}

/* Return the number of the next value that will be generated.  */

unsigned int
cselib_get_next_uid (void)
{
  return next_uid;
}

/* See the documentation of cselib_find_slot below.  */
static enum machine_mode find_slot_memmode;

/* Search for X, whose hashcode is HASH, in CSELIB_HASH_TABLE,
   INSERTing if requested.  When X is part of the address of a MEM,
   MEMMODE should specify the mode of the MEM.  While searching the
   table, MEMMODE is held in FIND_SLOT_MEMMODE, so that autoinc RTXs
   in X can be resolved.  */

static void **
cselib_find_slot (rtx x, hashval_t hash, enum insert_option insert,
		  enum machine_mode memmode)
{
  void **slot;
  find_slot_memmode = memmode;
  slot = htab_find_slot_with_hash (cselib_hash_table, x, hash, insert);
  find_slot_memmode = VOIDmode;
  return slot;
}

/* The equality test for our hash table.  The first argument ENTRY is a table
   element (i.e. a cselib_val), while the second arg X is an rtx.  We know
   that all callers of htab_find_slot_with_hash will wrap CONST_INTs into a
   CONST of an appropriate mode.  */

static int
entry_and_rtx_equal_p (const void *entry, const void *x_arg)
{
  struct elt_loc_list *l;
  const cselib_val *const v = (const cselib_val *) entry;
  rtx x = CONST_CAST_RTX ((const_rtx)x_arg);
  enum machine_mode mode = GET_MODE (x);

  gcc_assert (!CONST_INT_P (x) && GET_CODE (x) != CONST_FIXED
	      && (mode != VOIDmode || GET_CODE (x) != CONST_DOUBLE));

  if (mode != GET_MODE (v->val_rtx))
    return 0;

  /* Unwrap X if necessary.  */
  if (GET_CODE (x) == CONST
      && (CONST_INT_P (XEXP (x, 0))
	  || GET_CODE (XEXP (x, 0)) == CONST_FIXED
	  || GET_CODE (XEXP (x, 0)) == CONST_DOUBLE))
    x = XEXP (x, 0);

  /* We don't guarantee that distinct rtx's have different hash values,
     so we need to do a comparison.  */
  for (l = v->locs; l; l = l->next)
    if (rtx_equal_for_cselib_1 (l->loc, x, find_slot_memmode))
      {
	promote_debug_loc (l);
	return 1;
      }

  return 0;
}

/* The hash function for our hash table.  The value is always computed with
   cselib_hash_rtx when adding an element; this function just extracts the
   hash value from a cselib_val structure.  */

static hashval_t
get_value_hash (const void *entry)
{
  const cselib_val *const v = (const cselib_val *) entry;
  return v->hash;
}

/* Return true if X contains a VALUE rtx.  If ONLY_USELESS is set, we
   only return true for values which point to a cselib_val whose value
   element has been set to zero, which implies the cselib_val will be
   removed.  */

int
references_value_p (const_rtx x, int only_useless)
{
  const enum rtx_code code = GET_CODE (x);
  const char *fmt = GET_RTX_FORMAT (code);
  int i, j;

  if (GET_CODE (x) == VALUE
      && (! only_useless ||
	  (CSELIB_VAL_PTR (x)->locs == 0 && !PRESERVED_VALUE_P (x))))
    return 1;

  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e' && references_value_p (XEXP (x, i), only_useless))
	return 1;
      else if (fmt[i] == 'E')
	for (j = 0; j < XVECLEN (x, i); j++)
	  if (references_value_p (XVECEXP (x, i, j), only_useless))
	    return 1;
    }

  return 0;
}

/* For all locations found in X, delete locations that reference useless
   values (i.e. values without any location).  Called through
   htab_traverse.  */

static int
discard_useless_locs (void **x, void *info ATTRIBUTE_UNUSED)
{
  cselib_val *v = (cselib_val *)*x;
  struct elt_loc_list **p = &v->locs;
  bool had_locs = v->locs != NULL;
  rtx setting_insn = v->locs ? v->locs->setting_insn : NULL;

  while (*p)
    {
      if (references_value_p ((*p)->loc, 1))
	unchain_one_elt_loc_list (p);
      else
	p = &(*p)->next;
    }

  if (had_locs && v->locs == 0 && !PRESERVED_VALUE_P (v->val_rtx))
    {
      if (setting_insn && DEBUG_INSN_P (setting_insn))
	n_useless_debug_values++;
      else
	n_useless_values++;
      values_became_useless = 1;
    }
  return 1;
}

/* If X is a value with no locations, remove it from the hashtable.  */

static int
discard_useless_values (void **x, void *info ATTRIBUTE_UNUSED)
{
  cselib_val *v = (cselib_val *)*x;

  if (v->locs == 0 && !PRESERVED_VALUE_P (v->val_rtx))
    {
      if (cselib_discard_hook)
	cselib_discard_hook (v);

      CSELIB_VAL_PTR (v->val_rtx) = NULL;
      htab_clear_slot (cselib_hash_table, x);
      unchain_one_value (v);
      n_useless_values--;
    }

  return 1;
}

/* Clean out useless values (i.e. those which no longer have locations
   associated with them) from the hash table.  */

static void
remove_useless_values (void)
{
  cselib_val **p, *v;

  /* First pass: eliminate locations that reference the value.  That in
     turn can make more values useless.  */
  do
    {
      values_became_useless = 0;
      htab_traverse (cselib_hash_table, discard_useless_locs, 0);
    }
  while (values_became_useless);

  /* Second pass: actually remove the values.  */

  p = &first_containing_mem;
  for (v = *p; v != &dummy_val; v = v->next_containing_mem)
    if (v->locs && v == canonical_cselib_val (v))
      {
	*p = v;
	p = &(*p)->next_containing_mem;
      }
  *p = &dummy_val;

  n_useless_values += n_useless_debug_values;
  n_debug_values -= n_useless_debug_values;
  n_useless_debug_values = 0;

  htab_traverse (cselib_hash_table, discard_useless_values, 0);

  gcc_assert (!n_useless_values);
}

/* Arrange for a value to not be removed from the hash table even if
   it becomes useless.  */

void
cselib_preserve_value (cselib_val *v)
{
  PRESERVED_VALUE_P (v->val_rtx) = 1;
}

/* Test whether a value is preserved.  */

bool
cselib_preserved_value_p (cselib_val *v)
{
  return PRESERVED_VALUE_P (v->val_rtx);
}

/* Arrange for a REG value to be assumed constant through the whole function,
   never invalidated and preserved across cselib_reset_table calls.  */

void
cselib_preserve_cfa_base_value (cselib_val *v, unsigned int regno)
{
  if (cselib_preserve_constants
      && v->locs
      && REG_P (v->locs->loc))
    {
      cfa_base_preserved_val = v;
      cfa_base_preserved_regno = regno;
    }
}

/* Clean all non-constant expressions in the hash table, but retain
   their values.  */

void
cselib_preserve_only_values (void)
{
  int i;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    cselib_invalidate_regno (i, reg_raw_mode[i]);

  cselib_invalidate_mem (callmem);

  remove_useless_values ();

  gcc_assert (first_containing_mem == &dummy_val);
}

/* Return the mode in which a register was last set.  If X is not a
   register, return its mode.  If the mode in which the register was
   set is not known, or the value was already clobbered, return
   VOIDmode.  */

enum machine_mode
cselib_reg_set_mode (const_rtx x)
{
  if (!REG_P (x))
    return GET_MODE (x);

  if (REG_VALUES (REGNO (x)) == NULL
      || REG_VALUES (REGNO (x))->elt == NULL)
    return VOIDmode;

  return GET_MODE (REG_VALUES (REGNO (x))->elt->val_rtx);
}

/* Return nonzero if we can prove that X and Y contain the same value, taking
   our gathered information into account.  */

int
rtx_equal_for_cselib_p (rtx x, rtx y)
{
  return rtx_equal_for_cselib_1 (x, y, VOIDmode);
}

/* If x is a PLUS or an autoinc operation, expand the operation,
   storing the offset, if any, in *OFF.  */

static rtx
autoinc_split (rtx x, rtx *off, enum machine_mode memmode)
{
  switch (GET_CODE (x))
    {
    case PLUS:
      *off = XEXP (x, 1);
      return XEXP (x, 0);

    case PRE_DEC:
      if (memmode == VOIDmode)
	return x;

      *off = GEN_INT (-GET_MODE_SIZE (memmode));
      return XEXP (x, 0);
      break;

    case PRE_INC:
      if (memmode == VOIDmode)
	return x;

      *off = GEN_INT (GET_MODE_SIZE (memmode));
      return XEXP (x, 0);

    case PRE_MODIFY:
      return XEXP (x, 1);

    case POST_DEC:
    case POST_INC:
    case POST_MODIFY:
      return XEXP (x, 0);

    default:
      return x;
    }
}

/* Return nonzero if we can prove that X and Y contain the same value,
   taking our gathered information into account.  MEMMODE holds the
   mode of the enclosing MEM, if any, as required to deal with autoinc
   addressing modes.  If X and Y are not (known to be) part of
   addresses, MEMMODE should be VOIDmode.  */

static int
rtx_equal_for_cselib_1 (rtx x, rtx y, enum machine_mode memmode)
{
  enum rtx_code code;
  const char *fmt;
  int i;

  if (REG_P (x) || MEM_P (x))
    {
      cselib_val *e = cselib_lookup (x, GET_MODE (x), 0, memmode);

      if (e)
	x = e->val_rtx;
    }

  if (REG_P (y) || MEM_P (y))
    {
      cselib_val *e = cselib_lookup (y, GET_MODE (y), 0, memmode);

      if (e)
	y = e->val_rtx;
    }

  if (x == y)
    return 1;

  if (GET_CODE (x) == VALUE)
    {
      cselib_val *e = canonical_cselib_val (CSELIB_VAL_PTR (x));
      struct elt_loc_list *l;

      if (GET_CODE (y) == VALUE)
	return e == canonical_cselib_val (CSELIB_VAL_PTR (y));

      for (l = e->locs; l; l = l->next)
	{
	  rtx t = l->loc;

	  /* Avoid infinite recursion.  We know we have the canonical
	     value, so we can just skip any values in the equivalence
	     list.  */
	  if (REG_P (t) || MEM_P (t) || GET_CODE (t) == VALUE)
	    continue;
	  else if (rtx_equal_for_cselib_1 (t, y, memmode))
	    return 1;
	}

      return 0;
    }
  else if (GET_CODE (y) == VALUE)
    {
      cselib_val *e = canonical_cselib_val (CSELIB_VAL_PTR (y));
      struct elt_loc_list *l;

      for (l = e->locs; l; l = l->next)
	{
	  rtx t = l->loc;

	  if (REG_P (t) || MEM_P (t) || GET_CODE (t) == VALUE)
	    continue;
	  else if (rtx_equal_for_cselib_1 (x, t, memmode))
	    return 1;
	}

      return 0;
    }

  if (GET_MODE (x) != GET_MODE (y))
    return 0;

  if (GET_CODE (x) != GET_CODE (y))
    {
      rtx xorig = x, yorig = y;
      rtx xoff = NULL, yoff = NULL;

      x = autoinc_split (x, &xoff, memmode);
      y = autoinc_split (y, &yoff, memmode);

      if (!xoff != !yoff)
	return 0;

      if (xoff && !rtx_equal_for_cselib_1 (xoff, yoff, memmode))
	return 0;

      /* Don't recurse if nothing changed.  */
      if (x != xorig || y != yorig)
	return rtx_equal_for_cselib_1 (x, y, memmode);

      return 0;
    }

  /* These won't be handled correctly by the code below.  */
  switch (GET_CODE (x))
    {
    case CONST_DOUBLE:
    case CONST_FIXED:
    case DEBUG_EXPR:
      return 0;

    case DEBUG_IMPLICIT_PTR:
      return DEBUG_IMPLICIT_PTR_DECL (x)
	     == DEBUG_IMPLICIT_PTR_DECL (y);

    case DEBUG_PARAMETER_REF:
      return DEBUG_PARAMETER_REF_DECL (x)
	     == DEBUG_PARAMETER_REF_DECL (y);

    case ENTRY_VALUE:
      /* ENTRY_VALUEs are function invariant, it is thus undesirable to
	 use rtx_equal_for_cselib_1 to compare the operands.  */
      return rtx_equal_p (ENTRY_VALUE_EXP (x), ENTRY_VALUE_EXP (y));

    case LABEL_REF:
      return XEXP (x, 0) == XEXP (y, 0);

    case MEM:
      /* We have to compare any autoinc operations in the addresses
	 using this MEM's mode.  */
      return rtx_equal_for_cselib_1 (XEXP (x, 0), XEXP (y, 0), GET_MODE (x));

    default:
      break;
    }

  code = GET_CODE (x);
  fmt = GET_RTX_FORMAT (code);

  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      int j;

      switch (fmt[i])
	{
	case 'w':
	  if (XWINT (x, i) != XWINT (y, i))
	    return 0;
	  break;

	case 'n':
	case 'i':
	  if (XINT (x, i) != XINT (y, i))
	    return 0;
	  break;

	case 'V':
	case 'E':
	  /* Two vectors must have the same length.  */
	  if (XVECLEN (x, i) != XVECLEN (y, i))
	    return 0;

	  /* And the corresponding elements must match.  */
	  for (j = 0; j < XVECLEN (x, i); j++)
	    if (! rtx_equal_for_cselib_1 (XVECEXP (x, i, j),
					  XVECEXP (y, i, j), memmode))
	      return 0;
	  break;

	case 'e':
	  if (i == 1
	      && targetm.commutative_p (x, UNKNOWN)
	      && rtx_equal_for_cselib_1 (XEXP (x, 1), XEXP (y, 0), memmode)
	      && rtx_equal_for_cselib_1 (XEXP (x, 0), XEXP (y, 1), memmode))
	    return 1;
	  if (! rtx_equal_for_cselib_1 (XEXP (x, i), XEXP (y, i), memmode))
	    return 0;
	  break;

	case 'S':
	case 's':
	  if (strcmp (XSTR (x, i), XSTR (y, i)))
	    return 0;
	  break;

	case 'u':
	  /* These are just backpointers, so they don't matter.  */
	  break;

	case '0':
	case 't':
	  break;

	  /* It is believed that rtx's at this level will never
	     contain anything but integers and other rtx's,
	     except for within LABEL_REFs and SYMBOL_REFs.  */
	default:
	  gcc_unreachable ();
	}
    }
  return 1;
}

/* We need to pass down the mode of constants through the hash table
   functions.  For that purpose, wrap them in a CONST of the appropriate
   mode.  */
static rtx
wrap_constant (enum machine_mode mode, rtx x)
{
  if (!CONST_INT_P (x) && GET_CODE (x) != CONST_FIXED
      && (GET_CODE (x) != CONST_DOUBLE || GET_MODE (x) != VOIDmode))
    return x;
  gcc_assert (mode != VOIDmode);
  return gen_rtx_CONST (mode, x);
}

/* Hash an rtx.  Return 0 if we couldn't hash the rtx.
   For registers and memory locations, we look up their cselib_val structure
   and return its VALUE element.
   Possible reasons for return 0 are: the object is volatile, or we couldn't
   find a register or memory location in the table and CREATE is zero.  If
   CREATE is nonzero, table elts are created for regs and mem.
   N.B. this hash function returns the same hash value for RTXes that
   differ only in the order of operands, thus it is suitable for comparisons
   that take commutativity into account.
   If we wanted to also support associative rules, we'd have to use a different
   strategy to avoid returning spurious 0, e.g. return ~(~0U >> 1) .
   MEMMODE indicates the mode of an enclosing MEM, and it's only
   used to compute autoinc values.
   We used to have a MODE argument for hashing for CONST_INTs, but that
   didn't make sense, since it caused spurious hash differences between
    (set (reg:SI 1) (const_int))
    (plus:SI (reg:SI 2) (reg:SI 1))
   and
    (plus:SI (reg:SI 2) (const_int))
   If the mode is important in any context, it must be checked specifically
   in a comparison anyway, since relying on hash differences is unsafe.  */

static unsigned int
cselib_hash_rtx (rtx x, int create, enum machine_mode memmode)
{
  cselib_val *e;
  int i, j;
  enum rtx_code code;
  const char *fmt;
  unsigned int hash = 0;

  code = GET_CODE (x);
  hash += (unsigned) code + (unsigned) GET_MODE (x);

  switch (code)
    {
    case VALUE:
      e = CSELIB_VAL_PTR (x);
      return e->hash;

    case MEM:
    case REG:
      e = cselib_lookup (x, GET_MODE (x), create, memmode);
      if (! e)
	return 0;

      return e->hash;

    case DEBUG_EXPR:
      hash += ((unsigned) DEBUG_EXPR << 7)
	      + DEBUG_TEMP_UID (DEBUG_EXPR_TREE_DECL (x));
      return hash ? hash : (unsigned int) DEBUG_EXPR;

    case DEBUG_IMPLICIT_PTR:
      hash += ((unsigned) DEBUG_IMPLICIT_PTR << 7)
	      + DECL_UID (DEBUG_IMPLICIT_PTR_DECL (x));
      return hash ? hash : (unsigned int) DEBUG_IMPLICIT_PTR;

    case DEBUG_PARAMETER_REF:
      hash += ((unsigned) DEBUG_PARAMETER_REF << 7)
	      + DECL_UID (DEBUG_PARAMETER_REF_DECL (x));
      return hash ? hash : (unsigned int) DEBUG_PARAMETER_REF;

    case ENTRY_VALUE:
      /* ENTRY_VALUEs are function invariant, thus try to avoid
	 recursing on argument if ENTRY_VALUE is one of the
	 forms emitted by expand_debug_expr, otherwise
	 ENTRY_VALUE hash would depend on the current value
	 in some register or memory.  */
      if (REG_P (ENTRY_VALUE_EXP (x)))
	hash += (unsigned int) REG
		+ (unsigned int) GET_MODE (ENTRY_VALUE_EXP (x))
		+ (unsigned int) REGNO (ENTRY_VALUE_EXP (x));
      else if (MEM_P (ENTRY_VALUE_EXP (x))
	       && REG_P (XEXP (ENTRY_VALUE_EXP (x), 0)))
	hash += (unsigned int) MEM
		+ (unsigned int) GET_MODE (XEXP (ENTRY_VALUE_EXP (x), 0))
		+ (unsigned int) REGNO (XEXP (ENTRY_VALUE_EXP (x), 0));
      else
	hash += cselib_hash_rtx (ENTRY_VALUE_EXP (x), create, memmode);
      return hash ? hash : (unsigned int) ENTRY_VALUE;

    case CONST_INT:
      hash += ((unsigned) CONST_INT << 7) + INTVAL (x);
      return hash ? hash : (unsigned int) CONST_INT;

    case CONST_DOUBLE:
      /* This is like the general case, except that it only counts
	 the integers representing the constant.  */
      hash += (unsigned) code + (unsigned) GET_MODE (x);
      if (GET_MODE (x) != VOIDmode)
	hash += real_hash (CONST_DOUBLE_REAL_VALUE (x));
      else
	hash += ((unsigned) CONST_DOUBLE_LOW (x)
		 + (unsigned) CONST_DOUBLE_HIGH (x));
      return hash ? hash : (unsigned int) CONST_DOUBLE;

    case CONST_FIXED:
      hash += (unsigned int) code + (unsigned int) GET_MODE (x);
      hash += fixed_hash (CONST_FIXED_VALUE (x));
      return hash ? hash : (unsigned int) CONST_FIXED;

    case CONST_VECTOR:
      {
	int units;
	rtx elt;

	units = CONST_VECTOR_NUNITS (x);

	for (i = 0; i < units; ++i)
	  {
	    elt = CONST_VECTOR_ELT (x, i);
	    hash += cselib_hash_rtx (elt, 0, memmode);
	  }

	return hash;
      }

      /* Assume there is only one rtx object for any given label.  */
    case LABEL_REF:
      /* We don't hash on the address of the CODE_LABEL to avoid bootstrap
	 differences and differences between each stage's debugging dumps.  */
      hash += (((unsigned int) LABEL_REF << 7)
	       + CODE_LABEL_NUMBER (XEXP (x, 0)));
      return hash ? hash : (unsigned int) LABEL_REF;

    case SYMBOL_REF:
      {
	/* Don't hash on the symbol's address to avoid bootstrap differences.
	   Different hash values may cause expressions to be recorded in
	   different orders and thus different registers to be used in the
	   final assembler.  This also avoids differences in the dump files
	   between various stages.  */
	unsigned int h = 0;
	const unsigned char *p = (const unsigned char *) XSTR (x, 0);

	while (*p)
	  h += (h << 7) + *p++; /* ??? revisit */

	hash += ((unsigned int) SYMBOL_REF << 7) + h;
	return hash ? hash : (unsigned int) SYMBOL_REF;
      }

    case PRE_DEC:
    case PRE_INC:
      /* We can't compute these without knowing the MEM mode.  */
      gcc_assert (memmode != VOIDmode);
      i = GET_MODE_SIZE (memmode);
      if (code == PRE_DEC)
	i = -i;
      /* Adjust the hash so that (mem:MEMMODE (pre_* (reg))) hashes
	 like (mem:MEMMODE (plus (reg) (const_int I))).  */
      hash += (unsigned) PLUS - (unsigned)code
	+ cselib_hash_rtx (XEXP (x, 0), create, memmode)
	+ cselib_hash_rtx (GEN_INT (i), create, memmode);
      return hash ? hash : 1 + (unsigned) PLUS;

    case PRE_MODIFY:
      gcc_assert (memmode != VOIDmode);
      return cselib_hash_rtx (XEXP (x, 1), create, memmode);

    case POST_DEC:
    case POST_INC:
    case POST_MODIFY:
      gcc_assert (memmode != VOIDmode);
      return cselib_hash_rtx (XEXP (x, 0), create, memmode);

    case PC:
    case CC0:
    case CALL:
    case UNSPEC_VOLATILE:
      return 0;

    case ASM_OPERANDS:
      if (MEM_VOLATILE_P (x))
	return 0;

      break;

    default:
      break;
    }

  i = GET_RTX_LENGTH (code) - 1;
  fmt = GET_RTX_FORMAT (code);
  for (; i >= 0; i--)
    {
      switch (fmt[i])
	{
	case 'e':
	  {
	    rtx tem = XEXP (x, i);
	    unsigned int tem_hash = cselib_hash_rtx (tem, create, memmode);

	    if (tem_hash == 0)
	      return 0;

	    hash += tem_hash;
	  }
	  break;
	case 'E':
	  for (j = 0; j < XVECLEN (x, i); j++)
	    {
	      unsigned int tem_hash
		= cselib_hash_rtx (XVECEXP (x, i, j), create, memmode);

	      if (tem_hash == 0)
		return 0;

	      hash += tem_hash;
	    }
	  break;

	case 's':
	  {
	    const unsigned char *p = (const unsigned char *) XSTR (x, i);

	    if (p)
	      while (*p)
		hash += *p++;
	    break;
	  }

	case 'i':
	  hash += XINT (x, i);
	  break;

	case '0':
	case 't':
	  /* unused */
	  break;

	default:
	  gcc_unreachable ();
	}
    }

  return hash ? hash : 1 + (unsigned int) GET_CODE (x);
}

/* Create a new value structure for VALUE and initialize it.  The mode of the
   value is MODE.  */

static inline cselib_val *
new_cselib_val (unsigned int hash, enum machine_mode mode, rtx x)
{
  cselib_val *e = (cselib_val *) pool_alloc (cselib_val_pool);

  gcc_assert (hash);
  gcc_assert (next_uid);

  e->hash = hash;
  e->uid = next_uid++;
  /* We use an alloc pool to allocate this RTL construct because it
     accounts for about 8% of the overall memory usage.  We know
     precisely when we can have VALUE RTXen (when cselib is active)
     so we don't need to put them in garbage collected memory.
     ??? Why should a VALUE be an RTX in the first place?  */
  e->val_rtx = (rtx) pool_alloc (value_pool);
  memset (e->val_rtx, 0, RTX_HDR_SIZE);
  PUT_CODE (e->val_rtx, VALUE);
  PUT_MODE (e->val_rtx, mode);
  CSELIB_VAL_PTR (e->val_rtx) = e;
  e->addr_list = 0;
  e->locs = 0;
  e->next_containing_mem = 0;

  if (dump_file && (dump_flags & TDF_CSELIB))
    {
      fprintf (dump_file, "cselib value %u:%u ", e->uid, hash);
      if (flag_dump_noaddr || flag_dump_unnumbered)
	fputs ("# ", dump_file);
      else
	fprintf (dump_file, "%p ", (void*)e);
      print_rtl_single (dump_file, x);
      fputc ('\n', dump_file);
    }

  return e;
}

/* ADDR_ELT is a value that is used as address.  MEM_ELT is the value that
   contains the data at this address.  X is a MEM that represents the
   value.  Update the two value structures to represent this situation.  */

static void
add_mem_for_addr (cselib_val *addr_elt, cselib_val *mem_elt, rtx x)
{
  struct elt_loc_list *l;

  addr_elt = canonical_cselib_val (addr_elt);
  mem_elt = canonical_cselib_val (mem_elt);

  /* Avoid duplicates.  */
  for (l = mem_elt->locs; l; l = l->next)
    if (MEM_P (l->loc)
	&& CSELIB_VAL_PTR (XEXP (l->loc, 0)) == addr_elt)
      {
	promote_debug_loc (l);
	return;
      }

  addr_elt->addr_list = new_elt_list (addr_elt->addr_list, mem_elt);
  new_elt_loc_list (mem_elt,
		    replace_equiv_address_nv (x, addr_elt->val_rtx));
  if (mem_elt->next_containing_mem == NULL)
    {
      mem_elt->next_containing_mem = first_containing_mem;
      first_containing_mem = mem_elt;
    }
}

/* Subroutine of cselib_lookup.  Return a value for X, which is a MEM rtx.
   If CREATE, make a new one if we haven't seen it before.  */

static cselib_val *
cselib_lookup_mem (rtx x, int create)
{
  enum machine_mode mode = GET_MODE (x);
  enum machine_mode addr_mode;
  void **slot;
  cselib_val *addr;
  cselib_val *mem_elt;
  struct elt_list *l;

  if (MEM_VOLATILE_P (x) || mode == BLKmode
      || !cselib_record_memory
      || (FLOAT_MODE_P (mode) && flag_float_store))
    return 0;

  addr_mode = GET_MODE (XEXP (x, 0));
  if (addr_mode == VOIDmode)
    addr_mode = Pmode;

  /* Look up the value for the address.  */
  addr = cselib_lookup (XEXP (x, 0), addr_mode, create, mode);
  if (! addr)
    return 0;

  addr = canonical_cselib_val (addr);
  /* Find a value that describes a value of our mode at that address.  */
  for (l = addr->addr_list; l; l = l->next)
    if (GET_MODE (l->elt->val_rtx) == mode)
      {
	promote_debug_loc (l->elt->locs);
	return l->elt;
      }

  if (! create)
    return 0;

  mem_elt = new_cselib_val (next_uid, mode, x);
  add_mem_for_addr (addr, mem_elt, x);
  slot = cselib_find_slot (wrap_constant (mode, x), mem_elt->hash,
			   INSERT, mode);
  *slot = mem_elt;
  return mem_elt;
}

/* Search thru the possible substitutions in P.  We prefer a non reg
   substitution because this allows us to expand the tree further.  If
   we find, just a reg, take the lowest regno.  There may be several
   non-reg results, we just take the first one because they will all
   expand to the same place.  */

static rtx
expand_loc (struct elt_loc_list *p, struct expand_value_data *evd,
	    int max_depth)
{
  rtx reg_result = NULL;
  unsigned int regno = UINT_MAX;
  struct elt_loc_list *p_in = p;

  for (; p; p = p -> next)
    {
      /* Avoid infinite recursion trying to expand a reg into a
	 the same reg.  */
      if ((REG_P (p->loc))
	  && (REGNO (p->loc) < regno)
	  && !bitmap_bit_p (evd->regs_active, REGNO (p->loc)))
	{
	  reg_result = p->loc;
	  regno = REGNO (p->loc);
	}
      /* Avoid infinite recursion and do not try to expand the
	 value.  */
      else if (GET_CODE (p->loc) == VALUE
	       && CSELIB_VAL_PTR (p->loc)->locs == p_in)
	continue;
      else if (!REG_P (p->loc))
	{
	  rtx result, note;
	  if (dump_file && (dump_flags & TDF_CSELIB))
	    {
	      print_inline_rtx (dump_file, p->loc, 0);
	      fprintf (dump_file, "\n");
	    }
	  if (GET_CODE (p->loc) == LO_SUM
	      && GET_CODE (XEXP (p->loc, 1)) == SYMBOL_REF
	      && p->setting_insn
	      && (note = find_reg_note (p->setting_insn, REG_EQUAL, NULL_RTX))
	      && XEXP (note, 0) == XEXP (p->loc, 1))
	    return XEXP (p->loc, 1);
	  result = cselib_expand_value_rtx_1 (p->loc, evd, max_depth - 1);
	  if (result)
	    return result;
	}

    }

  if (regno != UINT_MAX)
    {
      rtx result;
      if (dump_file && (dump_flags & TDF_CSELIB))
	fprintf (dump_file, "r%d\n", regno);

      result = cselib_expand_value_rtx_1 (reg_result, evd, max_depth - 1);
      if (result)
	return result;
    }

  if (dump_file && (dump_flags & TDF_CSELIB))
    {
      if (reg_result)
	{
	  print_inline_rtx (dump_file, reg_result, 0);
	  fprintf (dump_file, "\n");
	}
      else
	fprintf (dump_file, "NULL\n");
    }
  return reg_result;
}


/* Forward substitute and expand an expression out to its roots.
   This is the opposite of common subexpression.  Because local value
   numbering is such a weak optimization, the expanded expression is
   pretty much unique (not from a pointer equals point of view but
   from a tree shape point of view.

   This function returns NULL if the expansion fails.  The expansion
   will fail if there is no value number for one of the operands or if
   one of the operands has been overwritten between the current insn
   and the beginning of the basic block.  For instance x has no
   expansion in:

   r1 <- r1 + 3
   x <- r1 + 8

   REGS_ACTIVE is a scratch bitmap that should be clear when passing in.
   It is clear on return.  */

rtx
cselib_expand_value_rtx (rtx orig, bitmap regs_active, int max_depth)
{
  struct expand_value_data evd;

  evd.regs_active = regs_active;
  evd.callback = NULL;
  evd.callback_arg = NULL;
  evd.dummy = false;

  return cselib_expand_value_rtx_1 (orig, &evd, max_depth);
}

/* Same as cselib_expand_value_rtx, but using a callback to try to
   resolve some expressions.  The CB function should return ORIG if it
   can't or does not want to deal with a certain RTX.  Any other
   return value, including NULL, will be used as the expansion for
   VALUE, without any further changes.  */

rtx
cselib_expand_value_rtx_cb (rtx orig, bitmap regs_active, int max_depth,
			    cselib_expand_callback cb, void *data)
{
  struct expand_value_data evd;

  evd.regs_active = regs_active;
  evd.callback = cb;
  evd.callback_arg = data;
  evd.dummy = false;

  return cselib_expand_value_rtx_1 (orig, &evd, max_depth);
}

/* Similar to cselib_expand_value_rtx_cb, but no rtxs are actually copied
   or simplified.  Useful to find out whether cselib_expand_value_rtx_cb
   would return NULL or non-NULL, without allocating new rtx.  */

bool
cselib_dummy_expand_value_rtx_cb (rtx orig, bitmap regs_active, int max_depth,
				  cselib_expand_callback cb, void *data)
{
  struct expand_value_data evd;

  evd.regs_active = regs_active;
  evd.callback = cb;
  evd.callback_arg = data;
  evd.dummy = true;

  return cselib_expand_value_rtx_1 (orig, &evd, max_depth) != NULL;
}

/* Internal implementation of cselib_expand_value_rtx and
   cselib_expand_value_rtx_cb.  */

static rtx
cselib_expand_value_rtx_1 (rtx orig, struct expand_value_data *evd,
			   int max_depth)
{
  rtx copy, scopy;
  int i, j;
  RTX_CODE code;
  const char *format_ptr;
  enum machine_mode mode;

  code = GET_CODE (orig);

  /* For the context of dse, if we end up expand into a huge tree, we
     will not have a useful address, so we might as well just give up
     quickly.  */
  if (max_depth <= 0)
    return NULL;

  switch (code)
    {
    case REG:
      {
	struct elt_list *l = REG_VALUES (REGNO (orig));

	if (l && l->elt == NULL)
	  l = l->next;
	for (; l; l = l->next)
	  if (GET_MODE (l->elt->val_rtx) == GET_MODE (orig))
	    {
	      rtx result;
	      unsigned regno = REGNO (orig);

	      /* The only thing that we are not willing to do (this
		 is requirement of dse and if others potential uses
		 need this function we should add a parm to control
		 it) is that we will not substitute the
		 STACK_POINTER_REGNUM, FRAME_POINTER or the
		 HARD_FRAME_POINTER.

		 These expansions confuses the code that notices that
		 stores into the frame go dead at the end of the
		 function and that the frame is not effected by calls
		 to subroutines.  If you allow the
		 STACK_POINTER_REGNUM substitution, then dse will
		 think that parameter pushing also goes dead which is
		 wrong.  If you allow the FRAME_POINTER or the
		 HARD_FRAME_POINTER then you lose the opportunity to
		 make the frame assumptions.  */
	      if (regno == STACK_POINTER_REGNUM
		  || regno == FRAME_POINTER_REGNUM
		  || regno == HARD_FRAME_POINTER_REGNUM
		  || regno == cfa_base_preserved_regno)
		return orig;

	      bitmap_set_bit (evd->regs_active, regno);

	      if (dump_file && (dump_flags & TDF_CSELIB))
		fprintf (dump_file, "expanding: r%d into: ", regno);

	      result = expand_loc (l->elt->locs, evd, max_depth);
	      bitmap_clear_bit (evd->regs_active, regno);

	      if (result)
		return result;
	      else
		return orig;
	    }
      }

    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_VECTOR:
    case SYMBOL_REF:
    case CODE_LABEL:
    case PC:
    case CC0:
    case SCRATCH:
      /* SCRATCH must be shared because they represent distinct values.  */
      return orig;
    case CLOBBER:
      if (REG_P (XEXP (orig, 0)) && HARD_REGISTER_NUM_P (REGNO (XEXP (orig, 0))))
	return orig;
      break;

    case CONST:
      if (shared_const_p (orig))
	return orig;
      break;

    case SUBREG:
      {
	rtx subreg;

	if (evd->callback)
	  {
	    subreg = evd->callback (orig, evd->regs_active, max_depth,
				    evd->callback_arg);
	    if (subreg != orig)
	      return subreg;
	  }

	subreg = cselib_expand_value_rtx_1 (SUBREG_REG (orig), evd,
					    max_depth - 1);
	if (!subreg)
	  return NULL;
	scopy = simplify_gen_subreg (GET_MODE (orig), subreg,
				     GET_MODE (SUBREG_REG (orig)),
				     SUBREG_BYTE (orig));
	if (scopy == NULL
	    || (GET_CODE (scopy) == SUBREG
		&& !REG_P (SUBREG_REG (scopy))
		&& !MEM_P (SUBREG_REG (scopy))))
	  return NULL;

	return scopy;
      }

    case VALUE:
      {
	rtx result;

	if (dump_file && (dump_flags & TDF_CSELIB))
	  {
	    fputs ("\nexpanding ", dump_file);
	    print_rtl_single (dump_file, orig);
	    fputs (" into...", dump_file);
	  }

	if (evd->callback)
	  {
	    result = evd->callback (orig, evd->regs_active, max_depth,
				    evd->callback_arg);

	    if (result != orig)
	      return result;
	  }

	result = expand_loc (CSELIB_VAL_PTR (orig)->locs, evd, max_depth);
	return result;
      }

    case DEBUG_EXPR:
      if (evd->callback)
	return evd->callback (orig, evd->regs_active, max_depth,
			      evd->callback_arg);
      return orig;

    default:
      break;
    }

  /* Copy the various flags, fields, and other information.  We assume
     that all fields need copying, and then clear the fields that should
     not be copied.  That is the sensible default behavior, and forces
     us to explicitly document why we are *not* copying a flag.  */
  if (evd->dummy)
    copy = NULL;
  else
    copy = shallow_copy_rtx (orig);

  format_ptr = GET_RTX_FORMAT (code);

  for (i = 0; i < GET_RTX_LENGTH (code); i++)
    switch (*format_ptr++)
      {
      case 'e':
	if (XEXP (orig, i) != NULL)
	  {
	    rtx result = cselib_expand_value_rtx_1 (XEXP (orig, i), evd,
						    max_depth - 1);
	    if (!result)
	      return NULL;
	    if (copy)
	      XEXP (copy, i) = result;
	  }
	break;

      case 'E':
      case 'V':
	if (XVEC (orig, i) != NULL)
	  {
	    if (copy)
	      XVEC (copy, i) = rtvec_alloc (XVECLEN (orig, i));
	    for (j = 0; j < XVECLEN (orig, i); j++)
	      {
		rtx result = cselib_expand_value_rtx_1 (XVECEXP (orig, i, j),
							evd, max_depth - 1);
		if (!result)
		  return NULL;
		if (copy)
		  XVECEXP (copy, i, j) = result;
	      }
	  }
	break;

      case 't':
      case 'w':
      case 'i':
      case 's':
      case 'S':
      case 'T':
      case 'u':
      case 'B':
      case '0':
	/* These are left unchanged.  */
	break;

      default:
	gcc_unreachable ();
      }

  if (evd->dummy)
    return orig;

  mode = GET_MODE (copy);
  /* If an operand has been simplified into CONST_INT, which doesn't
     have a mode and the mode isn't derivable from whole rtx's mode,
     try simplify_*_operation first with mode from original's operand
     and as a fallback wrap CONST_INT into gen_rtx_CONST.  */
  scopy = copy;
  switch (GET_RTX_CLASS (code))
    {
    case RTX_UNARY:
      if (CONST_INT_P (XEXP (copy, 0))
	  && GET_MODE (XEXP (orig, 0)) != VOIDmode)
	{
	  scopy = simplify_unary_operation (code, mode, XEXP (copy, 0),
					    GET_MODE (XEXP (orig, 0)));
	  if (scopy)
	    return scopy;
	}
      break;
    case RTX_COMM_ARITH:
    case RTX_BIN_ARITH:
      /* These expressions can derive operand modes from the whole rtx's mode.  */
      break;
    case RTX_TERNARY:
    case RTX_BITFIELD_OPS:
      if (CONST_INT_P (XEXP (copy, 0))
	  && GET_MODE (XEXP (orig, 0)) != VOIDmode)
	{
	  scopy = simplify_ternary_operation (code, mode,
					      GET_MODE (XEXP (orig, 0)),
					      XEXP (copy, 0), XEXP (copy, 1),
					      XEXP (copy, 2));
	  if (scopy)
	    return scopy;
	}
      break;
    case RTX_COMPARE:
    case RTX_COMM_COMPARE:
      if (CONST_INT_P (XEXP (copy, 0))
	  && GET_MODE (XEXP (copy, 1)) == VOIDmode
	  && (GET_MODE (XEXP (orig, 0)) != VOIDmode
	      || GET_MODE (XEXP (orig, 1)) != VOIDmode))
	{
	  scopy = simplify_relational_operation (code, mode,
						 (GET_MODE (XEXP (orig, 0))
						  != VOIDmode)
						 ? GET_MODE (XEXP (orig, 0))
						 : GET_MODE (XEXP (orig, 1)),
						 XEXP (copy, 0),
						 XEXP (copy, 1));
	  if (scopy)
	    return scopy;
	}
      break;
    default:
      break;
    }
  scopy = simplify_rtx (copy);
  if (scopy)
    return scopy;
  return copy;
}

/* Walk rtx X and replace all occurrences of REG and MEM subexpressions
   with VALUE expressions.  This way, it becomes independent of changes
   to registers and memory.
   X isn't actually modified; if modifications are needed, new rtl is
   allocated.  However, the return value can share rtl with X.
   If X is within a MEM, MEMMODE must be the mode of the MEM.  */

rtx
cselib_subst_to_values (rtx x, enum machine_mode memmode)
{
  enum rtx_code code = GET_CODE (x);
  const char *fmt = GET_RTX_FORMAT (code);
  cselib_val *e;
  struct elt_list *l;
  rtx copy = x;
  int i;

  switch (code)
    {
    case REG:
      l = REG_VALUES (REGNO (x));
      if (l && l->elt == NULL)
	l = l->next;
      for (; l; l = l->next)
	if (GET_MODE (l->elt->val_rtx) == GET_MODE (x))
	  return l->elt->val_rtx;

      gcc_unreachable ();

    case MEM:
      e = cselib_lookup_mem (x, 0);
      /* This used to happen for autoincrements, but we deal with them
	 properly now.  Remove the if stmt for the next release.  */
      if (! e)
	{
	  /* Assign a value that doesn't match any other.  */
	  e = new_cselib_val (next_uid, GET_MODE (x), x);
	}
      return e->val_rtx;

    case ENTRY_VALUE:
      e = cselib_lookup (x, GET_MODE (x), 0, memmode);
      if (! e)
	break;
      return e->val_rtx;

    case CONST_DOUBLE:
    case CONST_VECTOR:
    case CONST_INT:
    case CONST_FIXED:
      return x;

    case PRE_DEC:
    case PRE_INC:
      gcc_assert (memmode != VOIDmode);
      i = GET_MODE_SIZE (memmode);
      if (code == PRE_DEC)
	i = -i;
      return cselib_subst_to_values (plus_constant (XEXP (x, 0), i),
				     memmode);

    case PRE_MODIFY:
      gcc_assert (memmode != VOIDmode);
      return cselib_subst_to_values (XEXP (x, 1), memmode);

    case POST_DEC:
    case POST_INC:
    case POST_MODIFY:
      gcc_assert (memmode != VOIDmode);
      return cselib_subst_to_values (XEXP (x, 0), memmode);

    default:
      break;
    }

  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  rtx t = cselib_subst_to_values (XEXP (x, i), memmode);

	  if (t != XEXP (x, i))
	    {
	      if (x == copy)
		copy = shallow_copy_rtx (x);
	      XEXP (copy, i) = t;
	    }
	}
      else if (fmt[i] == 'E')
	{
	  int j;

	  for (j = 0; j < XVECLEN (x, i); j++)
	    {
	      rtx t = cselib_subst_to_values (XVECEXP (x, i, j), memmode);

	      if (t != XVECEXP (x, i, j))
		{
		  if (XVEC (x, i) == XVEC (copy, i))
		    {
		      if (x == copy)
			copy = shallow_copy_rtx (x);
		      XVEC (copy, i) = shallow_copy_rtvec (XVEC (x, i));
		    }
		  XVECEXP (copy, i, j) = t;
		}
	    }
	}
    }

  return copy;
}

/* Look up the rtl expression X in our tables and return the value it
   has.  If CREATE is zero, we return NULL if we don't know the value.
   Otherwise, we create a new one if possible, using mode MODE if X
   doesn't have a mode (i.e. because it's a constant).  When X is part
   of an address, MEMMODE should be the mode of the enclosing MEM if
   we're tracking autoinc expressions.  */

static cselib_val *
cselib_lookup_1 (rtx x, enum machine_mode mode,
		 int create, enum machine_mode memmode)
{
  void **slot;
  cselib_val *e;
  unsigned int hashval;

  if (GET_MODE (x) != VOIDmode)
    mode = GET_MODE (x);

  if (GET_CODE (x) == VALUE)
    return CSELIB_VAL_PTR (x);

  if (REG_P (x))
    {
      struct elt_list *l;
      unsigned int i = REGNO (x);

      l = REG_VALUES (i);
      if (l && l->elt == NULL)
	l = l->next;
      for (; l; l = l->next)
	if (mode == GET_MODE (l->elt->val_rtx))
	  {
	    promote_debug_loc (l->elt->locs);
	    return l->elt;
	  }

      if (! create)
	return 0;

      if (i < FIRST_PSEUDO_REGISTER)
	{
	  unsigned int n = hard_regno_nregs[i][mode];

	  if (n > max_value_regs)
	    max_value_regs = n;
	}

      e = new_cselib_val (next_uid, GET_MODE (x), x);
      new_elt_loc_list (e, x);
      if (REG_VALUES (i) == 0)
	{
	  /* Maintain the invariant that the first entry of
	     REG_VALUES, if present, must be the value used to set the
	     register, or NULL.  */
	  used_regs[n_used_regs++] = i;
	  REG_VALUES (i) = new_elt_list (REG_VALUES (i), NULL);
	}
      else if (cselib_preserve_constants
	       && GET_MODE_CLASS (mode) == MODE_INT)
	{
	  /* During var-tracking, try harder to find equivalences
	     for SUBREGs.  If a setter sets say a DImode register
	     and user uses that register only in SImode, add a lowpart
	     subreg location.  */
	  struct elt_list *lwider = NULL;
	  l = REG_VALUES (i);
	  if (l && l->elt == NULL)
	    l = l->next;
	  for (; l; l = l->next)
	    if (GET_MODE_CLASS (GET_MODE (l->elt->val_rtx)) == MODE_INT
		&& GET_MODE_SIZE (GET_MODE (l->elt->val_rtx))
		   > GET_MODE_SIZE (mode)
		&& (lwider == NULL
		    || GET_MODE_SIZE (GET_MODE (l->elt->val_rtx))
		       < GET_MODE_SIZE (GET_MODE (lwider->elt->val_rtx))))
	      {
		struct elt_loc_list *el;
		if (i < FIRST_PSEUDO_REGISTER
		    && hard_regno_nregs[i][GET_MODE (l->elt->val_rtx)] != 1)
		  continue;
		for (el = l->elt->locs; el; el = el->next)
		  if (!REG_P (el->loc))
		    break;
		if (el)
		  lwider = l;
	      }
	  if (lwider)
	    {
	      rtx sub = lowpart_subreg (mode, lwider->elt->val_rtx,
					GET_MODE (lwider->elt->val_rtx));
	      if (sub)
		new_elt_loc_list (e, sub);
	    }
	}
      REG_VALUES (i)->next = new_elt_list (REG_VALUES (i)->next, e);
      slot = cselib_find_slot (x, e->hash, INSERT, memmode);
      *slot = e;
      return e;
    }

  if (MEM_P (x))
    return cselib_lookup_mem (x, create);

  hashval = cselib_hash_rtx (x, create, memmode);
  /* Can't even create if hashing is not possible.  */
  if (! hashval)
    return 0;

  slot = cselib_find_slot (wrap_constant (mode, x), hashval,
			   create ? INSERT : NO_INSERT, memmode);
  if (slot == 0)
    return 0;

  e = (cselib_val *) *slot;
  if (e)
    return e;

  e = new_cselib_val (hashval, mode, x);

  /* We have to fill the slot before calling cselib_subst_to_values:
     the hash table is inconsistent until we do so, and
     cselib_subst_to_values will need to do lookups.  */
  *slot = (void *) e;
  new_elt_loc_list (e, cselib_subst_to_values (x, memmode));
  return e;
}

/* Wrapper for cselib_lookup, that indicates X is in INSN.  */

cselib_val *
cselib_lookup_from_insn (rtx x, enum machine_mode mode,
			 int create, enum machine_mode memmode, rtx insn)
{
  cselib_val *ret;

  gcc_assert (!cselib_current_insn);
  cselib_current_insn = insn;

  ret = cselib_lookup (x, mode, create, memmode);

  cselib_current_insn = NULL;

  return ret;
}

/* Wrapper for cselib_lookup_1, that logs the lookup result and
   maintains invariants related with debug insns.  */

cselib_val *
cselib_lookup (rtx x, enum machine_mode mode,
	       int create, enum machine_mode memmode)
{
  cselib_val *ret = cselib_lookup_1 (x, mode, create, memmode);

  /* ??? Should we return NULL if we're not to create an entry, the
     found loc is a debug loc and cselib_current_insn is not DEBUG?
     If so, we should also avoid converting val to non-DEBUG; probably
     easiest setting cselib_current_insn to NULL before the call
     above.  */

  if (dump_file && (dump_flags & TDF_CSELIB))
    {
      fputs ("cselib lookup ", dump_file);
      print_inline_rtx (dump_file, x, 2);
      fprintf (dump_file, " => %u:%u\n",
	       ret ? ret->uid : 0,
	       ret ? ret->hash : 0);
    }

  return ret;
}

/* Invalidate any entries in reg_values that overlap REGNO.  This is called
   if REGNO is changing.  MODE is the mode of the assignment to REGNO, which
   is used to determine how many hard registers are being changed.  If MODE
   is VOIDmode, then only REGNO is being changed; this is used when
   invalidating call clobbered registers across a call.  */

static void
cselib_invalidate_regno (unsigned int regno, enum machine_mode mode)
{
  unsigned int endregno;
  unsigned int i;

  /* If we see pseudos after reload, something is _wrong_.  */
  gcc_assert (!reload_completed || regno < FIRST_PSEUDO_REGISTER
	      || reg_renumber[regno] < 0);

  /* Determine the range of registers that must be invalidated.  For
     pseudos, only REGNO is affected.  For hard regs, we must take MODE
     into account, and we must also invalidate lower register numbers
     if they contain values that overlap REGNO.  */
  if (regno < FIRST_PSEUDO_REGISTER)
    {
      gcc_assert (mode != VOIDmode);

      if (regno < max_value_regs)
	i = 0;
      else
	i = regno - max_value_regs;

      endregno = end_hard_regno (mode, regno);
    }
  else
    {
      i = regno;
      endregno = regno + 1;
    }

  for (; i < endregno; i++)
    {
      struct elt_list **l = &REG_VALUES (i);

      /* Go through all known values for this reg; if it overlaps the range
	 we're invalidating, remove the value.  */
      while (*l)
	{
	  cselib_val *v = (*l)->elt;
	  bool had_locs;
	  rtx setting_insn;
	  struct elt_loc_list **p;
	  unsigned int this_last = i;

	  if (i < FIRST_PSEUDO_REGISTER && v != NULL)
	    this_last = end_hard_regno (GET_MODE (v->val_rtx), i) - 1;

	  if (this_last < regno || v == NULL
	      || (v == cfa_base_preserved_val
		  && i == cfa_base_preserved_regno))
	    {
	      l = &(*l)->next;
	      continue;
	    }

	  /* We have an overlap.  */
	  if (*l == REG_VALUES (i))
	    {
	      /* Maintain the invariant that the first entry of
		 REG_VALUES, if present, must be the value used to set
		 the register, or NULL.  This is also nice because
		 then we won't push the same regno onto user_regs
		 multiple times.  */
	      (*l)->elt = NULL;
	      l = &(*l)->next;
	    }
	  else
	    unchain_one_elt_list (l);

	  v = canonical_cselib_val (v);

	  had_locs = v->locs != NULL;
	  setting_insn = v->locs ? v->locs->setting_insn : NULL;

	  /* Now, we clear the mapping from value to reg.  It must exist, so
	     this code will crash intentionally if it doesn't.  */
	  for (p = &v->locs; ; p = &(*p)->next)
	    {
	      rtx x = (*p)->loc;

	      if (REG_P (x) && REGNO (x) == i)
		{
		  unchain_one_elt_loc_list (p);
		  break;
		}
	    }

	  if (had_locs && v->locs == 0 && !PRESERVED_VALUE_P (v->val_rtx))
	    {
	      if (setting_insn && DEBUG_INSN_P (setting_insn))
		n_useless_debug_values++;
	      else
		n_useless_values++;
	    }
	}
    }
}

/* Invalidate any locations in the table which are changed because of a
   store to MEM_RTX.  If this is called because of a non-const call
   instruction, MEM_RTX is (mem:BLK const0_rtx).  */

static void
cselib_invalidate_mem (rtx mem_rtx)
{
  cselib_val **vp, *v, *next;
  int num_mems = 0;
  rtx mem_addr;

  mem_addr = canon_rtx (get_addr (XEXP (mem_rtx, 0)));
  mem_rtx = canon_rtx (mem_rtx);

  vp = &first_containing_mem;
  for (v = *vp; v != &dummy_val; v = next)
    {
      bool has_mem = false;
      struct elt_loc_list **p = &v->locs;
      bool had_locs = v->locs != NULL;
      rtx setting_insn = v->locs ? v->locs->setting_insn : NULL;

      while (*p)
	{
	  rtx x = (*p)->loc;
	  cselib_val *addr;
	  struct elt_list **mem_chain;

	  /* MEMs may occur in locations only at the top level; below
	     that every MEM or REG is substituted by its VALUE.  */
	  if (!MEM_P (x))
	    {
	      p = &(*p)->next;
	      continue;
	    }
	  if (num_mems < PARAM_VALUE (PARAM_MAX_CSELIB_MEMORY_LOCATIONS)
	      && ! canon_true_dependence (mem_rtx, GET_MODE (mem_rtx),
					  mem_addr, x, NULL_RTX))
	    {
	      has_mem = true;
	      num_mems++;
	      p = &(*p)->next;
	      continue;
	    }

	  /* This one overlaps.  */
	  /* We must have a mapping from this MEM's address to the
	     value (E).  Remove that, too.  */
	  addr = cselib_lookup (XEXP (x, 0), VOIDmode, 0, GET_MODE (x));
	  addr = canonical_cselib_val (addr);
	  gcc_checking_assert (v == canonical_cselib_val (v));
	  mem_chain = &addr->addr_list;
	  for (;;)
	    {
	      cselib_val *canon = canonical_cselib_val ((*mem_chain)->elt);

	      if (canon == v)
		{
		  unchain_one_elt_list (mem_chain);
		  break;
		}

	      /* Record canonicalized elt.  */
	      (*mem_chain)->elt = canon;

	      mem_chain = &(*mem_chain)->next;
	    }

	  unchain_one_elt_loc_list (p);
	}

      if (had_locs && v->locs == 0 && !PRESERVED_VALUE_P (v->val_rtx))
	{
	  if (setting_insn && DEBUG_INSN_P (setting_insn))
	    n_useless_debug_values++;
	  else
	    n_useless_values++;
	}

      next = v->next_containing_mem;
      if (has_mem)
	{
	  *vp = v;
	  vp = &(*vp)->next_containing_mem;
	}
      else
	v->next_containing_mem = NULL;
    }
  *vp = &dummy_val;
}

/* Invalidate DEST, which is being assigned to or clobbered.  */

void
cselib_invalidate_rtx (rtx dest)
{
  while (GET_CODE (dest) == SUBREG
	 || GET_CODE (dest) == ZERO_EXTRACT
	 || GET_CODE (dest) == STRICT_LOW_PART)
    dest = XEXP (dest, 0);

  if (REG_P (dest))
    cselib_invalidate_regno (REGNO (dest), GET_MODE (dest));
  else if (MEM_P (dest))
    cselib_invalidate_mem (dest);
}

/* A wrapper for cselib_invalidate_rtx to be called via note_stores.  */

static void
cselib_invalidate_rtx_note_stores (rtx dest, const_rtx ignore ATTRIBUTE_UNUSED,
				   void *data ATTRIBUTE_UNUSED)
{
  cselib_invalidate_rtx (dest);
}

/* Record the result of a SET instruction.  DEST is being set; the source
   contains the value described by SRC_ELT.  If DEST is a MEM, DEST_ADDR_ELT
   describes its address.  */

static void
cselib_record_set (rtx dest, cselib_val *src_elt, cselib_val *dest_addr_elt)
{
  int dreg = REG_P (dest) ? (int) REGNO (dest) : -1;

  if (src_elt == 0 || side_effects_p (dest))
    return;

  if (dreg >= 0)
    {
      if (dreg < FIRST_PSEUDO_REGISTER)
	{
	  unsigned int n = hard_regno_nregs[dreg][GET_MODE (dest)];

	  if (n > max_value_regs)
	    max_value_regs = n;
	}

      if (REG_VALUES (dreg) == 0)
	{
	  used_regs[n_used_regs++] = dreg;
	  REG_VALUES (dreg) = new_elt_list (REG_VALUES (dreg), src_elt);
	}
      else
	{
	  /* The register should have been invalidated.  */
	  gcc_assert (REG_VALUES (dreg)->elt == 0);
	  REG_VALUES (dreg)->elt = src_elt;
	}

      if (src_elt->locs == 0 && !PRESERVED_VALUE_P (src_elt->val_rtx))
	n_useless_values--;
      new_elt_loc_list (src_elt, dest);
    }
  else if (MEM_P (dest) && dest_addr_elt != 0
	   && cselib_record_memory)
    {
      if (src_elt->locs == 0 && !PRESERVED_VALUE_P (src_elt->val_rtx))
	n_useless_values--;
      add_mem_for_addr (dest_addr_elt, src_elt, dest);
    }
}

/* Make ELT and X's VALUE equivalent to each other at INSN.  */

void
cselib_add_permanent_equiv (cselib_val *elt, rtx x, rtx insn)
{
  cselib_val *nelt;
  rtx save_cselib_current_insn = cselib_current_insn;

  gcc_checking_assert (elt);
  gcc_checking_assert (PRESERVED_VALUE_P (elt->val_rtx));
  gcc_checking_assert (!side_effects_p (x));

  cselib_current_insn = insn;

  nelt = cselib_lookup (x, GET_MODE (elt->val_rtx), 1, VOIDmode);

  if (nelt != elt)
    {
      if (!PRESERVED_VALUE_P (nelt->val_rtx))
	cselib_preserve_value (nelt);

      new_elt_loc_list (nelt, elt->val_rtx);
    }

  cselib_current_insn = save_cselib_current_insn;
}

/* There is no good way to determine how many elements there can be
   in a PARALLEL.  Since it's fairly cheap, use a really large number.  */
#define MAX_SETS (FIRST_PSEUDO_REGISTER * 2)

struct cselib_record_autoinc_data
{
  struct cselib_set *sets;
  int n_sets;
};

/* Callback for for_each_inc_dec.  Records in ARG the SETs implied by
   autoinc RTXs: SRC plus SRCOFF if non-NULL is stored in DEST.  */

static int
cselib_record_autoinc_cb (rtx mem ATTRIBUTE_UNUSED, rtx op ATTRIBUTE_UNUSED,
			  rtx dest, rtx src, rtx srcoff, void *arg)
{
  struct cselib_record_autoinc_data *data;
  data = (struct cselib_record_autoinc_data *)arg;

  data->sets[data->n_sets].dest = dest;

  if (srcoff)
    data->sets[data->n_sets].src = gen_rtx_PLUS (GET_MODE (src), src, srcoff);
  else
    data->sets[data->n_sets].src = src;

  data->n_sets++;

  return -1;
}

/* Record the effects of any sets and autoincs in INSN.  */
static void
cselib_record_sets (rtx insn)
{
  int n_sets = 0;
  int i;
  struct cselib_set sets[MAX_SETS];
  rtx body = PATTERN (insn);
  rtx cond = 0;
  int n_sets_before_autoinc;
  struct cselib_record_autoinc_data data;

  body = PATTERN (insn);
  if (GET_CODE (body) == COND_EXEC)
    {
      cond = COND_EXEC_TEST (body);
      body = COND_EXEC_CODE (body);
    }

  /* Find all sets.  */
  if (GET_CODE (body) == SET)
    {
      sets[0].src = SET_SRC (body);
      sets[0].dest = SET_DEST (body);
      n_sets = 1;
    }
  else if (GET_CODE (body) == PARALLEL)
    {
      /* Look through the PARALLEL and record the values being
	 set, if possible.  Also handle any CLOBBERs.  */
      for (i = XVECLEN (body, 0) - 1; i >= 0; --i)
	{
	  rtx x = XVECEXP (body, 0, i);

	  if (GET_CODE (x) == SET)
	    {
	      sets[n_sets].src = SET_SRC (x);
	      sets[n_sets].dest = SET_DEST (x);
	      n_sets++;
	    }
	}
    }

  if (n_sets == 1
      && MEM_P (sets[0].src)
      && !cselib_record_memory
      && MEM_READONLY_P (sets[0].src))
    {
      rtx note = find_reg_equal_equiv_note (insn);

      if (note && CONSTANT_P (XEXP (note, 0)))
	sets[0].src = XEXP (note, 0);
    }

  data.sets = sets;
  data.n_sets = n_sets_before_autoinc = n_sets;
  for_each_inc_dec (&insn, cselib_record_autoinc_cb, &data);
  n_sets = data.n_sets;

  /* Look up the values that are read.  Do this before invalidating the
     locations that are written.  */
  for (i = 0; i < n_sets; i++)
    {
      rtx dest = sets[i].dest;

      /* A STRICT_LOW_PART can be ignored; we'll record the equivalence for
         the low part after invalidating any knowledge about larger modes.  */
      if (GET_CODE (sets[i].dest) == STRICT_LOW_PART)
	sets[i].dest = dest = XEXP (dest, 0);

      /* We don't know how to record anything but REG or MEM.  */
      if (REG_P (dest)
	  || (MEM_P (dest) && cselib_record_memory))
        {
	  rtx src = sets[i].src;
	  if (cond)
	    src = gen_rtx_IF_THEN_ELSE (GET_MODE (dest), cond, src, dest);
	  sets[i].src_elt = cselib_lookup (src, GET_MODE (dest), 1, VOIDmode);
	  if (MEM_P (dest))
	    {
	      enum machine_mode address_mode
		= targetm.addr_space.address_mode (MEM_ADDR_SPACE (dest));

	      sets[i].dest_addr_elt = cselib_lookup (XEXP (dest, 0),
						     address_mode, 1,
						     GET_MODE (dest));
	    }
	  else
	    sets[i].dest_addr_elt = 0;
	}
    }

  if (cselib_record_sets_hook)
    cselib_record_sets_hook (insn, sets, n_sets);

  /* Invalidate all locations written by this insn.  Note that the elts we
     looked up in the previous loop aren't affected, just some of their
     locations may go away.  */
  note_stores (body, cselib_invalidate_rtx_note_stores, NULL);

  for (i = n_sets_before_autoinc; i < n_sets; i++)
    cselib_invalidate_rtx (sets[i].dest);

  /* If this is an asm, look for duplicate sets.  This can happen when the
     user uses the same value as an output multiple times.  This is valid
     if the outputs are not actually used thereafter.  Treat this case as
     if the value isn't actually set.  We do this by smashing the destination
     to pc_rtx, so that we won't record the value later.  */
  if (n_sets >= 2 && asm_noperands (body) >= 0)
    {
      for (i = 0; i < n_sets; i++)
	{
	  rtx dest = sets[i].dest;
	  if (REG_P (dest) || MEM_P (dest))
	    {
	      int j;
	      for (j = i + 1; j < n_sets; j++)
		if (rtx_equal_p (dest, sets[j].dest))
		  {
		    sets[i].dest = pc_rtx;
		    sets[j].dest = pc_rtx;
		  }
	    }
	}
    }

  /* Now enter the equivalences in our tables.  */
  for (i = 0; i < n_sets; i++)
    {
      rtx dest = sets[i].dest;
      if (REG_P (dest)
	  || (MEM_P (dest) && cselib_record_memory))
	cselib_record_set (dest, sets[i].src_elt, sets[i].dest_addr_elt);
    }
}

/* Record the effects of INSN.  */

void
cselib_process_insn (rtx insn)
{
  int i;
  rtx x;

  cselib_current_insn = insn;

  /* Forget everything at a CODE_LABEL, a volatile asm, or a setjmp.  */
  if (LABEL_P (insn)
      || (CALL_P (insn)
	  && find_reg_note (insn, REG_SETJMP, NULL))
      || (NONJUMP_INSN_P (insn)
	  && GET_CODE (PATTERN (insn)) == ASM_OPERANDS
	  && MEM_VOLATILE_P (PATTERN (insn))))
    {
      cselib_reset_table (next_uid);
      cselib_current_insn = NULL_RTX;
      return;
    }

  if (! INSN_P (insn))
    {
      cselib_current_insn = NULL_RTX;
      return;
    }

  /* If this is a call instruction, forget anything stored in a
     call clobbered register, or, if this is not a const call, in
     memory.  */
  if (CALL_P (insn))
    {
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (call_used_regs[i]
	    || (REG_VALUES (i) && REG_VALUES (i)->elt
		&& HARD_REGNO_CALL_PART_CLOBBERED (i,
		      GET_MODE (REG_VALUES (i)->elt->val_rtx))))
	  cselib_invalidate_regno (i, reg_raw_mode[i]);

      /* Since it is not clear how cselib is going to be used, be
	 conservative here and treat looping pure or const functions
	 as if they were regular functions.  */
      if (RTL_LOOPING_CONST_OR_PURE_CALL_P (insn)
	  || !(RTL_CONST_OR_PURE_CALL_P (insn)))
	cselib_invalidate_mem (callmem);
    }

  cselib_record_sets (insn);

  /* Look for any CLOBBERs in CALL_INSN_FUNCTION_USAGE, but only
     after we have processed the insn.  */
  if (CALL_P (insn))
    for (x = CALL_INSN_FUNCTION_USAGE (insn); x; x = XEXP (x, 1))
      if (GET_CODE (XEXP (x, 0)) == CLOBBER)
	cselib_invalidate_rtx (XEXP (XEXP (x, 0), 0));

  cselib_current_insn = NULL_RTX;

  if (n_useless_values > MAX_USELESS_VALUES
      /* remove_useless_values is linear in the hash table size.  Avoid
         quadratic behavior for very large hashtables with very few
	 useless elements.  */
      && ((unsigned int)n_useless_values
	  > (cselib_hash_table->n_elements
	     - cselib_hash_table->n_deleted
	     - n_debug_values) / 4))
    remove_useless_values ();
}

/* Initialize cselib for one pass.  The caller must also call
   init_alias_analysis.  */

void
cselib_init (int record_what)
{
  elt_list_pool = create_alloc_pool ("elt_list",
				     sizeof (struct elt_list), 10);
  elt_loc_list_pool = create_alloc_pool ("elt_loc_list",
				         sizeof (struct elt_loc_list), 10);
  cselib_val_pool = create_alloc_pool ("cselib_val_list",
				       sizeof (cselib_val), 10);
  value_pool = create_alloc_pool ("value", RTX_CODE_SIZE (VALUE), 100);
  cselib_record_memory = record_what & CSELIB_RECORD_MEMORY;
  cselib_preserve_constants = record_what & CSELIB_PRESERVE_CONSTANTS;

  /* (mem:BLK (scratch)) is a special mechanism to conflict with everything,
     see canon_true_dependence.  This is only created once.  */
  if (! callmem)
    callmem = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (VOIDmode));

  cselib_nregs = max_reg_num ();

  /* We preserve reg_values to allow expensive clearing of the whole thing.
     Reallocate it however if it happens to be too large.  */
  if (!reg_values || reg_values_size < cselib_nregs
      || (reg_values_size > 10 && reg_values_size > cselib_nregs * 4))
    {
      free (reg_values);
      /* Some space for newly emit instructions so we don't end up
	 reallocating in between passes.  */
      reg_values_size = cselib_nregs + (63 + cselib_nregs) / 16;
      reg_values = XCNEWVEC (struct elt_list *, reg_values_size);
    }
  used_regs = XNEWVEC (unsigned int, cselib_nregs);
  n_used_regs = 0;
  cselib_hash_table = htab_create (31, get_value_hash,
				   entry_and_rtx_equal_p, NULL);
  next_uid = 1;
}

/* Called when the current user is done with cselib.  */

void
cselib_finish (void)
{
  cselib_discard_hook = NULL;
  cselib_preserve_constants = false;
  cfa_base_preserved_val = NULL;
  cfa_base_preserved_regno = INVALID_REGNUM;
  free_alloc_pool (elt_list_pool);
  free_alloc_pool (elt_loc_list_pool);
  free_alloc_pool (cselib_val_pool);
  free_alloc_pool (value_pool);
  cselib_clear_table ();
  htab_delete (cselib_hash_table);
  free (used_regs);
  used_regs = 0;
  cselib_hash_table = 0;
  n_useless_values = 0;
  n_useless_debug_values = 0;
  n_debug_values = 0;
  next_uid = 0;
}

/* Dump the cselib_val *X to FILE *info.  */

static int
dump_cselib_val (void **x, void *info)
{
  cselib_val *v = (cselib_val *)*x;
  FILE *out = (FILE *)info;
  bool need_lf = true;

  print_inline_rtx (out, v->val_rtx, 0);

  if (v->locs)
    {
      struct elt_loc_list *l = v->locs;
      if (need_lf)
	{
	  fputc ('\n', out);
	  need_lf = false;
	}
      fputs (" locs:", out);
      do
	{
	  fprintf (out, "\n  from insn %i ",
		   INSN_UID (l->setting_insn));
	  print_inline_rtx (out, l->loc, 4);
	}
      while ((l = l->next));
      fputc ('\n', out);
    }
  else
    {
      fputs (" no locs", out);
      need_lf = true;
    }

  if (v->addr_list)
    {
      struct elt_list *e = v->addr_list;
      if (need_lf)
	{
	  fputc ('\n', out);
	  need_lf = false;
	}
      fputs (" addr list:", out);
      do
	{
	  fputs ("\n  ", out);
	  print_inline_rtx (out, e->elt->val_rtx, 2);
	}
      while ((e = e->next));
      fputc ('\n', out);
    }
  else
    {
      fputs (" no addrs", out);
      need_lf = true;
    }

  if (v->next_containing_mem == &dummy_val)
    fputs (" last mem\n", out);
  else if (v->next_containing_mem)
    {
      fputs (" next mem ", out);
      print_inline_rtx (out, v->next_containing_mem->val_rtx, 2);
      fputc ('\n', out);
    }
  else if (need_lf)
    fputc ('\n', out);

  return 1;
}

/* Dump to OUT everything in the CSELIB table.  */

void
dump_cselib_table (FILE *out)
{
  fprintf (out, "cselib hash table:\n");
  htab_traverse (cselib_hash_table, dump_cselib_val, out);
  if (first_containing_mem != &dummy_val)
    {
      fputs ("first mem ", out);
      print_inline_rtx (out, first_containing_mem->val_rtx, 2);
      fputc ('\n', out);
    }
  fprintf (out, "next uid %i\n", next_uid);
}

#include "gt-cselib.h"
