/* Postulates needed for improving equivalence of modes.
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

/* Initialise use of postulate-lists.  */

void
a68_init_postulates (void)
{
  A68 (top_postulate) = NO_POSTULATE;
  A68 (top_postulate_list) = NO_POSTULATE;
}

/* Make old postulates available for new use.  */

void
a68_free_postulate_list (POSTULATE_T *start, POSTULATE_T *stop)
{
  if (start == stop)
    return;

  POSTULATE_T *last = start;
  for (; NEXT (last) != stop; FORWARD (last))
    ;

  NEXT (last) = A68 (top_postulate_list);
  A68 (top_postulate_list) = start;
}

/* Add postulates to postulate-list.  */

void
a68_make_postulate (POSTULATE_T **p, MOID_T *a, MOID_T *b)
{
  POSTULATE_T *new_one;

  if (A68 (top_postulate_list) != NO_POSTULATE)
    {
      new_one = A68 (top_postulate_list);
      A68 (top_postulate_list) = A68 (top_postulate_list)->next;
    }
  else
    {
      new_one = (POSTULATE_T *) ggc_cleared_alloc<POSTULATE_T> ();
      A68 (new_postulates)++;
    }

  new_one->a = a;
  new_one->b = b;
  new_one->next = *p;
  *p = new_one;
}

/* Where postulates are in the list.  */

POSTULATE_T
*a68_is_postulated_pair (POSTULATE_T *p, MOID_T *a, MOID_T *b)
{
  for (; p != NO_POSTULATE; p = p->next)
    {
      if (p->a == a && p->b == b)
	return p;
    }

  return NO_POSTULATE;
}

/* Where postulate is in the list.  */

POSTULATE_T
*a68_is_postulated (POSTULATE_T *p, MOID_T *a)
{
  for (; p != NO_POSTULATE; p = p->next)
    {
      if (p->a == a)
	return p;
    }

  return NO_POSTULATE;
}
