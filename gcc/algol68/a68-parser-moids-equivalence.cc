/* Prove equivalence of modes.
   Copyright (C) 2001-2023 J. Marcel van der Veer.
   Copyright (C) 2025 Jose E. Marchesi.

   Original implementation by J. Marcel van der Veer.
   Adapted for GCC by Jose E. Marchesi.

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

/* Routines for establishing equivalence of modes.
   After I made this mode equivalencer (in 1993), I found:

   Algol Bulletin 30.3.3 C.H.A. Koster: On infinite modes, 86-89 [1969],

   which essentially concurs with this test on mode equivalence I wrote.
   It is elementary logic anyway: prove equivalence, assuming equivalence.  */

/* Forward declarations of some of the functions defined below.  */

static bool are_modes_equivalent (MOID_T * a, MOID_T * b);

/* Whether packs are equivalent, same sequence of equivalence modes.  */

static bool
are_packs_equivalent (PACK_T *s, PACK_T *t,
		      bool compare_names = true)
{
  for (; s != NO_PACK && t != NO_PACK; s = s->next, t = t->next)
    {
      if (!are_modes_equivalent (MOID (s), MOID (t)))
	return false;
      if (compare_names)
	{
	  if (TEXT (s) != TEXT (t)
	      && TEXT (s) != NO_TEXT
	      && TEXT (t) != NO_TEXT
	      && strcmp (TEXT (s), TEXT (t)) != 0)
	    return false;
	}
    }

  return s == NO_PACK && t == NO_PACK;
}

/* Whether packs are subsets.  */

static bool
is_united_subset (PACK_T *s, PACK_T *t)
{
  /* For all modes in 's' there must be an equivalent in 't'.  */
  for (PACK_T *p = s; p != NO_PACK; p = p->next)
    {
      bool f = false;
      for (PACK_T *q = t; q != NO_PACK && !f; q = q->next)
	f = are_modes_equivalent (MOID (p), MOID (q));

      if (!f)
	return false;
    }

  return true;
}

/* Whether packs are subsets.  */

static bool
are_united_packs_equivalent (PACK_T *s, PACK_T *t)
{
  return is_united_subset (s, t) && is_united_subset (t, s);
}

/* Whether moids A and B are structurally equivalent.  */

static bool
are_modes_equivalent (MOID_T * a, MOID_T * b)
{
  /* First lets try some cheap heuristics.  */

  if (a == NO_MOID || b == NO_MOID)
    /* Modes can be NO_MOID in partial argument lists.  */
    return false;
  else if (a == M_ERROR || b == M_ERROR)
    return false;
  else if (a == b)
    return true;
  else if (ATTRIBUTE (a) != ATTRIBUTE (b))
    return false;
  else if (DIM (a) != DIM (b))
    return false;
  else if (IS (a, STANDARD))
    return (a == b);
  else if (EQUIVALENT (a) == b || EQUIVALENT (b) == a)
    return true;
  else if (a68_is_postulated_pair (A68 (top_postulate), a, b)
	   || a68_is_postulated_pair (A68 (top_postulate), b, a))
    return true;
  else if (IS (a, INDICANT))
    {
      if (NODE (a) == NO_NODE || NODE (b) == NO_NODE)
	return false;
      else
	return (NODE (a) == NODE (b)
		|| strcmp (NSYMBOL (NODE (a)), NSYMBOL (NODE (b))) == 0);
    }

  /* Investigate structure.  */

  /* We now know that 'a' and 'b' have same attribute, dimension, ...  */
  if (IS (a, REF_SYMBOL))
    /* REF MODE  */
    return are_modes_equivalent (a->sub, b->sub);
  else if (IS (a, ROW_SYMBOL))
    /* [] MODE  */
    return are_modes_equivalent (a->sub, b->sub);
  else if (IS (a, FLEX_SYMBOL))
    /* FLEX [...] MODE  */
    return are_modes_equivalent (a->sub, b->sub);
  else if (IS (a, STRUCT_SYMBOL))
    {
      /* STRUCT (...)  */
      POSTULATE_T *save = A68 (top_postulate);
      a68_make_postulate (&A68 (top_postulate), a, b);
      bool z = are_packs_equivalent (PACK (a), PACK (b));
      a68_free_postulate_list (A68 (top_postulate), save);
      A68 (top_postulate) = save;
      return z;
    }
  else if (IS (a, UNION_SYMBOL))
    /* UNION (...)  */
    return are_united_packs_equivalent (PACK (a), PACK (b));
  else if (IS (a, PROC_SYMBOL) && PACK (a) == NO_PACK && PACK (b) == NO_PACK)
    /* PROC MOID  */
    return are_modes_equivalent (a->sub, b->sub);
  else if (IS (a, PROC_SYMBOL) && PACK (a) != NO_PACK && PACK (b) != NO_PACK)
    {
      /* PROC (...) MOID  */
      POSTULATE_T *save = A68 (top_postulate);
      a68_make_postulate (&A68 (top_postulate), a, b);
      bool z = are_modes_equivalent (a->sub, b->sub);
      if (z)
	z = are_packs_equivalent (PACK (a), PACK (b),
				  false /* compare_names */);
      a68_free_postulate_list (A68 (top_postulate), save);
      A68 (top_postulate) = save;
      return z;
    }
  else if (IS (a, SERIES_MODE) || IS (a, STOWED_MODE))
    /* Modes occurring in displays.  */
    return are_packs_equivalent (PACK (a), PACK (b));

  return false;
}

//! @brief Whether two modes are structurally equivalent.

bool
a68_prove_moid_equivalence (MOID_T *p, MOID_T *q)
{
// Prove two modes to be equivalent under assumption that they indeed are.
  POSTULATE_T *save = A68 (top_postulate);
  bool z = are_modes_equivalent (p, q);
  a68_free_postulate_list (A68 (top_postulate), save);
  A68 (top_postulate) = save;
  return z;
}
