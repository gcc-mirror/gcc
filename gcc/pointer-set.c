/* Set operations on pointers
   Copyright (C) 2004-2013 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "pointer-set.h"


/* Use the multiplicative method, as described in Knuth 6.4, to obtain
   a hash code for P in the range [0, MAX).  MAX == 2^LOGMAX.

   Summary of this method: Multiply p by some number A that's
   relatively prime to 2^sizeof(size_t).  The result is two words.
   Discard the most significant word, and return the most significant
   N bits of the least significant word.  As suggested by Knuth, our
   choice for A is the integer part of (ULONG_MAX + 1.0) / phi, where phi
   is the golden ratio.

   We don't need to do anything special for full-width multiplication
   because we're only interested in the least significant word of the
   product, and unsigned arithmetic in C is modulo the word size.  */

static inline size_t
hash1 (const void *p, unsigned long max, unsigned long logmax)
{
#if HOST_BITS_PER_LONG == 32
  const unsigned long A = 0x9e3779b9u;
#elif HOST_BITS_PER_LONG == 64
  const unsigned long A = 0x9e3779b97f4a7c16ul;
#else
  const unsigned long A
    = (ULONG_MAX + 1.0L) * 0.6180339887498948482045868343656381177203L;
#endif
  const unsigned long shift = HOST_BITS_PER_LONG - logmax;

  return ((A * (uintptr_t) p) >> shift) & (max - 1);
}


/* Allocate an empty pointer set.  */
struct pointer_set_t *
pointer_set_create (void)
{
  struct pointer_set_t *result = XNEW (struct pointer_set_t);

  result->n_elements = 0;
  result->log_slots = 8;
  result->n_slots = (size_t) 1 << result->log_slots;

  result->slots = XCNEWVEC (const void *, result->n_slots);
  return result;
}

/* Reclaims all memory associated with PSET.  */
void
pointer_set_destroy (struct pointer_set_t *pset)
{
  XDELETEVEC (pset->slots);
  XDELETE (pset);
}


/* Lookup the slot for the pointer P and return true if it exists,
   otherwise return false in which case *IX points to the slot that
   would be used on insertion.  */

bool
pointer_set_lookup (const pointer_set_t *pset, const void *p, size_t *ix)
{
  size_t n = hash1 (p, pset->n_slots, pset->log_slots);

  while (true)
    {
      if (pset->slots[n] == p)
	{
	  *ix = n;
	  return true;
	}
      else if (pset->slots[n] == 0)
	{
	  *ix = n;
	  return false;
	}
      else
       {
         ++n;
         if (n == pset->n_slots)
           n = 0;
       }
    }
}

/* Returns nonzero if PSET contains P.  P must be nonnull.

   Collisions are resolved by linear probing.  */
int
pointer_set_contains (const struct pointer_set_t *pset, const void *p)
{
  size_t n;
  return pointer_set_lookup (pset, p, &n);
}

/* Inserts P into PSET if it wasn't already there.  Returns nonzero
   if it was already there. P must be nonnull.  */
int
pointer_set_insert (struct pointer_set_t *pset, const void *p)
{
  size_t n;

  /* For simplicity, expand the set even if P is already there.  This can be
     superfluous but can happen at most once.  */
  if (pset->n_elements > pset->n_slots / 4)
    {
      size_t old_n_slots = pset->n_slots;
      const void **old_slots = pset->slots;
      pset->log_slots = pset->log_slots + 1;
      pset->n_slots = pset->n_slots * 2;
      pset->slots = XCNEWVEC (const void *, pset->n_slots);
      size_t i;

      for (i = 0; i < old_n_slots; ++i)
        {
	  const void *value = old_slots[i];
	  pointer_set_lookup (pset, value, &n);
	  pset->slots[n] = value;
	}

      XDELETEVEC (old_slots);
    }

  if (pointer_set_lookup (pset, p, &n))
    return 1;

  pset->slots[n] = p;
  ++pset->n_elements;
  return 0;
}

/* Pass each pointer in PSET to the function in FN, together with the fixed
   parameter DATA.  If FN returns false, the iteration stops.  */

void pointer_set_traverse (const struct pointer_set_t *pset,
			   bool (*fn) (const void *, void *), void *data)
{
  size_t i;
  for (i = 0; i < pset->n_slots; ++i)
    if (pset->slots[i] && !fn (pset->slots[i], data))
      break;
}


/* A pointer map is represented the same way as a pointer_set, so
   the hash code is based on the address of the key, rather than
   its contents.  Null keys are a reserved value.  Deletion is not
   supported (yet).  There is no mechanism for user control of hash
   function, equality comparison, initial size, or resizing policy.  */

struct pointer_map_t
{
  pointer_set_t pset;
  void **values;
};

/* Allocate an empty pointer map.  */
struct pointer_map_t *
pointer_map_create (void)
{
  struct pointer_map_t *result = XNEW (struct pointer_map_t);

  result->pset.n_elements = 0;
  result->pset.log_slots = 8;
  result->pset.n_slots = (size_t) 1 << result->pset.log_slots;

  result->pset.slots = XCNEWVEC (const void *, result->pset.n_slots);
  result->values = XCNEWVEC (void *, result->pset.n_slots);
  return result;
}

/* Reclaims all memory associated with PMAP.  */
void pointer_map_destroy (struct pointer_map_t *pmap)
{
  XDELETEVEC (pmap->pset.slots);
  XDELETEVEC (pmap->values);
  XDELETE (pmap);
}

/* Returns a pointer to the value to which P maps, if PMAP contains P.  P
   must be nonnull.  Return NULL if PMAP does not contain P.

   Collisions are resolved by linear probing.  */
void **
pointer_map_contains (const struct pointer_map_t *pmap, const void *p)
{
  size_t n;
  if (pointer_set_lookup (&pmap->pset, p, &n))
    return &pmap->values[n];
  else
    return NULL;
}

/* Inserts P into PMAP if it wasn't already there.  Returns a pointer
   to the value.  P must be nonnull.  */
void **
pointer_map_insert (struct pointer_map_t *pmap, const void *p)
{
  size_t n;

  /* For simplicity, expand the map even if P is already there.  This can be
     superfluous but can happen at most once.  */
  if (pmap->pset.n_elements > pmap->pset.n_slots / 4)
    {
      size_t old_n_slots = pmap->pset.n_slots;
      const void **old_keys = pmap->pset.slots;
      void **old_values = pmap->values;
      pmap->pset.log_slots = pmap->pset.log_slots + 1;
      pmap->pset.n_slots = pmap->pset.n_slots * 2;
      pmap->pset.slots = XCNEWVEC (const void *, pmap->pset.n_slots);
      pmap->values = XCNEWVEC (void *, pmap->pset.n_slots);
      size_t i;

      for (i = 0; i < old_n_slots; ++i)
	if (old_keys[i])
	  {
	    const void *key = old_keys[i];
	    pointer_set_lookup (&pmap->pset, key, &n);
	    pmap->pset.slots[n] = key;
	    pmap->values[n] = old_values[i];
	  }

      XDELETEVEC (old_keys);
      XDELETEVEC (old_values);
    }

  if (!pointer_set_lookup (&pmap->pset, p, &n))
    {
      ++pmap->pset.n_elements;
      pmap->pset.slots[n] = p;
    }

  return &pmap->values[n];
}

/* Pass each pointer in PMAP to the function in FN, together with the pointer
   to the value and the fixed parameter DATA.  If FN returns false, the
   iteration stops.  */

void pointer_map_traverse (const struct pointer_map_t *pmap,
			   bool (*fn) (const void *, void **, void *), void *data)
{
  size_t i;
  for (i = 0; i < pmap->pset.n_slots; ++i)
    if (pmap->pset.slots[i]
	&& !fn (pmap->pset.slots[i], &pmap->values[i], data))
      break;
}
