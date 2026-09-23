/* MOID sorting routines.
   Copyright (C) 2026 James Bohl.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "a68.h"

/*
 * Routines for ordering modes.
 */

/* Forward references.  */

static int mode_ordering (MOID_T *a,MOID_T *b);
static PACK_T * sort_union_pack (PACK_T *u);

/* Returns a negative value if 'a' should be ordered after 'b'.
   Returns a positive value if 'a' should be ordered before 'b'.
   Returns zero if 'a' and 'b' are equivalent.  */

static int
packs_ordering (PACK_T *a, PACK_T *b, bool compare_names = true)
{
  for (; a != NO_PACK && b != NO_PACK; FORWARD (a), FORWARD (b))
    {
      int order = mode_ordering (MOID (a), MOID (b));
      if (order != 0)
	return order;
      if (compare_names)
	{
	  if (TEXT (a) != TEXT (b))
	    {
	      if (TEXT (a) == NO_TEXT)
		return 1;
	      if (TEXT (b) == NO_TEXT)
		return -1;

	      int cmp = strcmp (TEXT (a), TEXT (b));
	      if (cmp != 0)
		return -cmp;
	    }
	}
    }
  return 0;
}

/* Returns a negative value if 'a' should be ordered after 'b'.
   Returns a positive value if 'a' should be ordered before 'b'.
   Returns zero if 'a' and 'b' are equivalent.  */

static int
mode_ordering (MOID_T *a, MOID_T *b)
{
  if (a == b)
    return 0;
  int r = ATTRIBUTE (a) - ATTRIBUTE (b);
  if (r != 0)
    return r;
  r = DIM (a) - DIM (b);
  if (r != 0)
    return r;
  if (IS (a, STANDARD))
    return strcmp (NSYMBOL (NODE (a)), NSYMBOL (NODE (b)));
  else if (EQUIVALENT (a) == b || EQUIVALENT (b) == a)
    return 0;
  else if (a68_is_postulated_pair (A68 (top_postulate), a, b)
	    || a68_is_postulated_pair (A68 (top_postulate), b, a))
    return 0;
  else if (IS (a, INDICANT))
    {
      if (NODE (a) == NO_NODE)
       return 1;
      if (NODE (b) == NO_NODE)
       return -1;
      if (NODE (a) == NODE (b))
       return 0;
      return strcmp (NSYMBOL (NODE (a)), NSYMBOL (NODE (b)));
    }
  else if (IS (a, REF_SYMBOL))
    return mode_ordering (SUB (a), SUB (b));
  else if (IS (a, ROW_SYMBOL))
    return mode_ordering (SUB (a), SUB (b));
  else if (IS (a, FLEX_SYMBOL))
    return mode_ordering (SUB (a), SUB (b));
  else if (IS (a, STRUCT_SYMBOL))
    {
      POSTULATE_T *save = A68 (top_postulate);
      a68_make_postulate (&A68 (top_postulate), a, b);
      r = packs_ordering (PACK (a), PACK (b));
      a68_free_postulate_list (A68 (top_postulate), save);
      A68 (top_postulate) = save;
      return r;
    }
  else if (IS (a, UNION_SYMBOL))
    {
      PACK (a) = sort_union_pack (PACK (a));
      PACK (b) = sort_union_pack (PACK (b));
      return packs_ordering (PACK (a), PACK (b), false);
    }
  else if (IS (a, PROC_SYMBOL))
    {
      POSTULATE_T *save = A68 (top_postulate);
      a68_make_postulate (&A68 (top_postulate), a, b);
      r = mode_ordering (SUB (a), SUB (b));
      if (r == 0)
	r = packs_ordering (PACK (a), PACK (b), false);
      a68_free_postulate_list (A68 (top_postulate), save);
      A68 (top_postulate) = save;
      return r;
    }
  else if (IS (a, SERIES_MODE) || IS (a, STOWED_MODE))
    return packs_ordering (PACK (a), PACK (b), false);
  return 0;
}

/* Add a moid to a sorted pack, maybe with a (field) name.  */

static void
add_mode_to_pack_sorted (PACK_T **p, MOID_T *m, const char *text, NODE_T *node)
{
  PACK_T *z = a68_new_pack ();

  MOID (z) = m;
  TEXT (z) = text;
  NODE (z) = node;

  PACK_T *next = (*p);
  PACK_T *previous = NO_PACK;
  while (next != NO_PACK)
    {
      int order = mode_ordering (m,MOID (next));
      if (order > 0)
	break;
      previous = next;
      FORWARD (next);
    }
  NEXT (z) = next;
  PREVIOUS (z) = previous;

  if (previous == NO_PACK)
    *p = z;
  else
    NEXT (previous) = z;

  if (next != NO_PACK)
    PREVIOUS (next) = z;
}

/* Sort modes in a UNION pack.  */

static PACK_T *
sort_union_pack (PACK_T *u)
{
  PACK_T *z = NO_PACK;
  for (PACK_T *t = u; t != NO_PACK; FORWARD (t))
    {
      (void) add_mode_to_pack_sorted (&z, MOID (t), NO_TEXT, NODE (t));
    }
  return z;
}

/* Sort modes in UNION packs.  */

void
a68_sort_union_packs (MOID_T *m)
{
  for (; m != NO_MOID; FORWARD (m))
    {
      if (IS (m, UNION_SYMBOL))
	PACK (m) = sort_union_pack (PACK (m));
    }
}
