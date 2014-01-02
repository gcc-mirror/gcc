/* Set operations on pointers
   Copyright (C) 2004-2014 Free Software Foundation, Inc.

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

#ifndef POINTER_SET_H
#define POINTER_SET_H


/* A pointer set is represented as a simple open-addressing hash
   table.  Simplifications: The hash code is based on the value of the
   pointer, not what it points to.  The number of buckets is always a
   power of 2.  Null pointers are a reserved value.  Deletion is not
   supported (yet).  There is no mechanism for user control of hash
   function, equality comparison, initial size, or resizing policy.  */

struct pointer_set_t
{
  size_t log_slots;
  size_t n_slots;		/* n_slots = 2^log_slots */
  size_t n_elements;
  const void **slots;
};

struct pointer_set_t *pointer_set_create (void);
void pointer_set_destroy (struct pointer_set_t *pset);
int pointer_set_contains (const struct pointer_set_t *pset, const void *p);
int pointer_set_insert (struct pointer_set_t *pset, const void *p);
void pointer_set_traverse (const struct pointer_set_t *,
			   bool (*) (const void *, void *),
			   void *);
bool pointer_set_lookup (const pointer_set_t *, const void *, size_t *);

/* A pointer map is represented the same way as a pointer_set, so
   the hash code is based on the address of the key, rather than
   its contents.  Null keys are a reserved value.  Deletion is not
   supported (yet).  There is no mechanism for user control of hash
   function, equality comparison, initial size, or resizing policy.  */

template <typename T>
class pointer_map : protected pointer_set_t
{
  T *values;

public:
  pointer_map ();
  ~pointer_map ();
  T *contains (const void *p);
  T *insert (const void *p, bool *existed_p = NULL);
  void traverse (bool (*fn) (const void *, T *, void *), void *data);
};

/* Allocate an empty pointer map.  */
template <typename T>
pointer_map<T>::pointer_map (void)
{
  n_elements = 0;
  log_slots = 8;
  n_slots = (size_t) 1 << log_slots;

  slots = XCNEWVEC (const void *, n_slots);
  values = XNEWVEC (T, n_slots);
}

/* Reclaims all memory associated with PMAP.  */
template <typename T>
pointer_map<T>::~pointer_map (void)
{
  XDELETEVEC (slots);
  XDELETEVEC (values);
}

/* Returns a pointer to the value to which P maps, if PMAP contains P.  P
   must be nonnull.  Return NULL if PMAP does not contain P.

   Collisions are resolved by linear probing.  */
template <typename T>
T *
pointer_map<T>::contains (const void *p)
{
  size_t n;
  if (!pointer_set_lookup (this, p, &n))
    return NULL;
  return &values[n];
}

/* Inserts P into PMAP if it wasn't already there.  Returns a pointer
   to the value.  P must be nonnull.  */
template <typename T>
T *
pointer_map<T>::insert (const void *p, bool *existed_p)
{
  size_t n;

  /* For simplicity, expand the map even if P is already there.  This can be
     superfluous but can happen at most once.  */
  /* ???  Fugly that we have to inline that here.  */
  if (n_elements > n_slots / 4)
    {
      size_t old_n_slots = n_slots;
      const void **old_keys = slots;
      T *old_values = values;
      log_slots = log_slots + 1;
      n_slots = n_slots * 2;
      slots = XCNEWVEC (const void *, n_slots);
      values = XNEWVEC (T, n_slots);
      for (size_t i = 0; i < old_n_slots; ++i)
	if (old_keys[i])
	  {
	    const void *key = old_keys[i];
	    pointer_set_lookup (this, key, &n);
	    slots[n] = key;
	    values[n] = old_values[i];
	  }
      XDELETEVEC (old_keys);
      XDELETEVEC (old_values);
    }

  if (!pointer_set_lookup (this, p, &n))
    {
      ++n_elements;
      slots[n] = p;
      if (existed_p)
	*existed_p = false;
    }
  else if (existed_p)
    *existed_p = true;

  return &values[n];
}

/* Pass each pointer in PMAP to the function in FN, together with the pointer
   to the value and the fixed parameter DATA.  If FN returns false, the
   iteration stops.  */

template <class T>
void
pointer_map<T>::traverse (bool (*fn) (const void *, T *, void *), void *data)
{
  for (size_t i = 0; i < n_slots; ++i)
    if (slots[i] && !fn (slots[i], &values[i], data))
      break;
}


struct pointer_map_t;
pointer_map_t *pointer_map_create (void);
void pointer_map_destroy (pointer_map_t *pmap);

void **pointer_map_contains (const pointer_map_t *pmap, const void *p);
void **pointer_map_insert (pointer_map_t *pmap, const void *p);
void pointer_map_traverse (const pointer_map_t *,
			   bool (*) (const void *, void **, void *), void *);


#endif  /* POINTER_SET_H  */
