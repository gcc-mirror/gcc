/* SparseSet implementation.
   Copyright (C) 2007, 2008 Free Software Foundation, Inc.
   Contributed by Peter Bergner <bergner@vnet.ibm.com>

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
#include "sparseset.h"

/* Allocate and clear a n_elms SparseSet.  */

sparseset
sparseset_alloc (SPARSESET_ELT_TYPE n_elms)
{
  unsigned int n_bytes = sizeof (struct sparseset_def)
			 + ((n_elms - 1) * 2 * sizeof (SPARSESET_ELT_TYPE));

  /* We use xcalloc rather than xmalloc to silence some valgrind uninitialized
     read errors when accessing set->sparse[n] when "n" is not, and never has
     been, in the set.  These uninitialized reads are expected, by design and
     harmless.  If this turns into a performance problem due to some future
     additional users of sparseset, we can revisit this decision.  */
  sparseset set = (sparseset) xcalloc (1, n_bytes);
  set->dense = &(set->elms[0]);
  set->sparse = &(set->elms[n_elms]);
  set->size = n_elms;
  sparseset_clear (set);
  return set;
}

/* Low level routine not meant for use outside of sparseset.[ch].
   Assumes idx1 < s->members and idx2 < s->members.  */

static inline void
sparseset_swap (sparseset s, SPARSESET_ELT_TYPE idx1, SPARSESET_ELT_TYPE idx2)
{
  SPARSESET_ELT_TYPE tmp = s->dense[idx2];
  sparseset_insert_bit (s, s->dense[idx1], idx2);
  sparseset_insert_bit (s, tmp, idx1);
}

/* Operation: S = S - {e}
   Delete e from the set S if it is a member of S.  */

void
sparseset_clear_bit (sparseset s, SPARSESET_ELT_TYPE e)
{
  if (sparseset_bit_p (s, e))
    {
      SPARSESET_ELT_TYPE idx = s->sparse[e];
      SPARSESET_ELT_TYPE iter = s->iter;
      SPARSESET_ELT_TYPE mem = s->members - 1;

      /* If we are iterating over this set and we want to delete a
	 member we've already visited, then we swap the element we
	 want to delete with the element at the current iteration
	 index so that it plays well together with the code below
	 that actually removes the element.  */
      if (s->iterating && idx <= iter)
	{
	  if (idx < iter)
	    {
	      sparseset_swap (s, idx, iter);
	      idx = iter;
	    }
	  s->iter_inc = 0;
	}

      /* Replace the element we want to delete with the last element
	 in the dense array and then decrement s->members, effectively
	 removing the element we want to delete.  */
      sparseset_insert_bit (s, s->dense[mem], idx);
      s->members = mem;
    }
}

/* Operation: D = S
   Restrictions: none.  */

void
sparseset_copy (sparseset d, sparseset s)
{
  SPARSESET_ELT_TYPE i;

  if (d == s)
    return;

  sparseset_clear (d);
  for (i = 0; i < s->members; i++)
    sparseset_insert_bit (d, s->dense[i], i);
  d->members = s->members;
}

/* Operation: D = A & B.
   Restrictions: none.  */

void
sparseset_and (sparseset d, sparseset a, sparseset b)
{
  SPARSESET_ELT_TYPE e;

  if (a == b)
    {
      if (d != a)
	sparseset_copy (d, a);
      return;
    }

  if (d == a || d == b)
    {
      sparseset s = (d == a) ? b : a;

      EXECUTE_IF_SET_IN_SPARSESET (d, e)
	if (!sparseset_bit_p (s, e))
	  sparseset_clear_bit (d, e);
    }
  else
    {
      sparseset sml, lrg;

      if (sparseset_cardinality (a) < sparseset_cardinality (b))
	{
	  sml = a;
	  lrg = b;
	}
      else
	{
	  sml = b;
	  lrg = a;
	}

      sparseset_clear (d);
      EXECUTE_IF_SET_IN_SPARSESET (sml, e)
	if (sparseset_bit_p (lrg, e))
	  sparseset_set_bit (d, e);
    }
}

/* Operation: D = A & ~B.
   Restrictions: D != B, unless D == A == B.  */

void
sparseset_and_compl (sparseset d, sparseset a, sparseset b)
{
  SPARSESET_ELT_TYPE e;

  if (a == b)
    {
      sparseset_clear (d);
      return;
    }

  gcc_assert (d != b);

  if (d == a)
    {
      if (sparseset_cardinality (d) < sparseset_cardinality (b))
	{
	  EXECUTE_IF_SET_IN_SPARSESET (d, e)
	    if (sparseset_bit_p (b, e))
	      sparseset_clear_bit (d, e);
	}
      else
	{
	  EXECUTE_IF_SET_IN_SPARSESET (b, e)
	    sparseset_clear_bit (d, e);
	}
    }
  else
    {
      sparseset_clear (d);
      EXECUTE_IF_SET_IN_SPARSESET (a, e)
	if (!sparseset_bit_p (b, e))
	  sparseset_set_bit (d, e);
    }
}

/* Operation: D = A | B.
   Restrictions: none.  */

void
sparseset_ior (sparseset d, sparseset a, sparseset b)
{
  SPARSESET_ELT_TYPE e;

  if (a == b)
    sparseset_copy (d, a);
  else if (d == b)
    {
      EXECUTE_IF_SET_IN_SPARSESET (a, e)
	sparseset_set_bit (d, e);
    }
  else
    {
      if (d != a)
        sparseset_copy (d, a);
      EXECUTE_IF_SET_IN_SPARSESET (b, e)
	sparseset_set_bit (d, e);
    }
}

/* Operation: A == B
   Restrictions: none.  */

bool
sparseset_equal_p (sparseset a, sparseset b)
{
  SPARSESET_ELT_TYPE e;

  if (a == b)
    return true;

  if (sparseset_cardinality (a) != sparseset_cardinality (b))
    return false;

  EXECUTE_IF_SET_IN_SPARSESET (a, e)
    if (!sparseset_bit_p (b, e))
      return false;

  return true;
}

