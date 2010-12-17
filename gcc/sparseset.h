/* SparseSet implementation.
   Copyright (C) 2007, 2010 Free Software Foundation, Inc.
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

#ifndef GCC_SPARSESET_H
#define GCC_SPARSESET_H

#define SPARSESET_ELT_BITS ((unsigned) HOST_BITS_PER_WIDEST_FAST_INT)
#define SPARSESET_ELT_TYPE unsigned int

/* Data Structure used for the SparseSet representation.  */

typedef struct sparseset_def
{
  SPARSESET_ELT_TYPE *dense;	/* Dense array.  */
  SPARSESET_ELT_TYPE *sparse;	/* Sparse array.  */
  SPARSESET_ELT_TYPE members;	/* Number of elements.  */
  SPARSESET_ELT_TYPE size;	/* Maximum number of elements.  */
  SPARSESET_ELT_TYPE iter;	/* Iterator index.  */
  unsigned char iter_inc;	/* Iteration increment amount.  */
  bool iterating;
  SPARSESET_ELT_TYPE elms[2];   /* Combined dense and sparse arrays.  */
} *sparseset;

#define sparseset_free(MAP)  free(MAP)
extern sparseset sparseset_alloc (SPARSESET_ELT_TYPE n_elms);
extern void sparseset_clear_bit (sparseset, SPARSESET_ELT_TYPE);
extern void sparseset_copy (sparseset, sparseset);
extern void sparseset_and (sparseset, sparseset, sparseset);
extern void sparseset_and_compl (sparseset, sparseset, sparseset);
extern void sparseset_ior (sparseset, sparseset, sparseset);
extern bool sparseset_equal_p (sparseset, sparseset);

/* Operation: S = {}
   Clear the set of all elements.  */

static inline void
sparseset_clear (sparseset s)
{
  s->members = 0;
  s->iterating = false;
}

/* Return the number of elements currently in the set.  */

static inline SPARSESET_ELT_TYPE
sparseset_cardinality (sparseset s)
{
  return s->members;
}

/* Return the maximum number of elements this set can hold.  */

static inline SPARSESET_ELT_TYPE
sparseset_size (sparseset s)
{
  return s->size;
}

/* Return true if e is a member of the set S, otherwise return false.  */

static inline bool
sparseset_bit_p (sparseset s, SPARSESET_ELT_TYPE e)
{
  SPARSESET_ELT_TYPE idx;

  gcc_assert (e < s->size);

  idx = s->sparse[e];

  return idx < s->members && s->dense[idx] == e;
}

/* Low level insertion routine not meant for use outside of sparseset.[ch].
   Assumes E is valid and not already a member of the set S.  */

static inline void
sparseset_insert_bit (sparseset s, SPARSESET_ELT_TYPE e, SPARSESET_ELT_TYPE idx)
{
  s->sparse[e] = idx;
  s->dense[idx] = e;
}

/* Operation: S = S + {e}
   Insert E into the set S, if it isn't already a member.  */

static inline void
sparseset_set_bit (sparseset s, SPARSESET_ELT_TYPE e)
{
  if (!sparseset_bit_p (s, e))
    sparseset_insert_bit (s, e, s->members++);
}

/* Return and remove an arbitrary element from the set S.  */

static inline SPARSESET_ELT_TYPE
sparseset_pop (sparseset s)
{
  SPARSESET_ELT_TYPE mem = s->members;

  gcc_assert (mem != 0);

  s->members = mem - 1;
  return s->dense[mem];
}

static inline void
sparseset_iter_init (sparseset s)
{
  s->iter = 0;
  s->iter_inc = 1;
  s->iterating = true;
}

static inline bool
sparseset_iter_p (sparseset s)
{
  if (s->iterating && s->iter < s->members)
    return true;
  else
    return s->iterating = false;
}

static inline SPARSESET_ELT_TYPE
sparseset_iter_elm (sparseset s)
{
  return s->dense[s->iter];
}

static inline void
sparseset_iter_next (sparseset s)
{
  s->iter += s->iter_inc;
  s->iter_inc = 1;
}

#define EXECUTE_IF_SET_IN_SPARSESET(SPARSESET, ITER)			\
  for (sparseset_iter_init (SPARSESET);					\
       sparseset_iter_p (SPARSESET)					\
       && (((ITER) = sparseset_iter_elm (SPARSESET)) || 1);		\
       sparseset_iter_next (SPARSESET))

#endif /* GCC_SPARSESET_H */
