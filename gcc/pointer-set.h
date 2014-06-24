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


struct pointer_map_t;
pointer_map_t *pointer_map_create (void);
void pointer_map_destroy (pointer_map_t *pmap);

void **pointer_map_contains (const pointer_map_t *pmap, const void *p);
void **pointer_map_insert (pointer_map_t *pmap, const void *p);
void pointer_map_traverse (const pointer_map_t *,
			   bool (*) (const void *, void **, void *), void *);


#endif  /* POINTER_SET_H  */
