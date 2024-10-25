/* objc-map.h -- Implementation of map data structures for ObjC compiler
   Copyright (C) 2011-2024 Free Software Foundation, Inc.
   Written by Nicola Pero <nicola.pero@meta-innovation.com>

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU Lesser Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser Public License for more details.

You should have received a copy of the GNU Lesser Public License
along with this program; if not, write to the Free Software
Foundation, 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1301, USA.  */

#ifndef OBJC_MAP_H
#define OBJC_MAP_H

/* A map is a data structure that maps a key to a value.  In this file
   we currently have maps that can map a GCC identifier (a tree) to
   some other GCC tree.  This is what the ObjC frontend mostly needs:
   being able to look up an identifier into an ObjC data structure.  A
   typical usage is mapping ObjC class names (as identifiers) to a
   tree representing the class.

   This implementation is fast.  :-) */

/**
 ** Private definitions.
 **/

/* We include private declaration and definitions that are required to
   provide the implementation of inline functions.  You should ignore
   these definitions (and the implementation of the inline functions)
   as they are not part of the public API and may change.  */
typedef unsigned int objc_map_private_hash_t;

/* This is used as sentinel.  */
#define OBJC_MAP_PRIVATE_EMPTY_SLOT (tree)0

struct GTY(()) objc_map_private {
  /* Total number of slots.  This is the maximum number of elements
     that can be currently stored in the map before resizing.  This is
     the number of slots in the C array.  Important: this is
     guaranteed to be a power of 2.  When we create (or resize) the
     map, we round up the size to the next power of 2.  This allows us
     to convert a hash to a position in the hashtable by simply doing
     "position = hash & mask", where mask is number_of_slots - 1
     instead of using a modulo (which requires a division).  */
  size_t number_of_slots;

  /* This is number_of_slots - 1, precomputed.  */
  size_t mask;

  /* Number of slots that are not empty (ie, that are active).  We
     keep counts using this variable which can easily be checked
     against max_number_of_non_empty_slots.  */
  size_t number_of_non_empty_slots;

  /* This is the load factor limit.  When the number of non empty
     slots equals this number, we need to resize the array.  This is
     calculated once, when the slots are resized, and then kept cached
     so it can be compared quickly when elements are added.  */
  size_t max_number_of_non_empty_slots;

  /* The maximum load factor.  */
  int maximum_load_factor;

  /* These are the keys.  */
  tree * GTY ((length ("%h.number_of_slots"))) slots;

  /* These are the values.  values[i] is the value corresponding
     to slots[i].  */
  tree * GTY ((length ("%h.number_of_slots"))) values;
};

/* Private functions used to resize the map.  They may be called by
   the inline functions when adding elements.  */
extern void
objc_map_private_grow (struct objc_map_private *map);


/**
 ** The definition of a map.
 **/
typedef struct objc_map_private *objc_map_t;


/**
 ** Creating a map.
 **/

/* objc_map_alloc_ggc() creates a new map which is under GGC.  The initial
   capacity must be specified as an argument; this is used to size the map
   when it is created.  */
objc_map_t objc_map_alloc_ggc (size_t initial_capacity);

/**
 ** Performance tuning.
 **/

/* Set a maximum load factor for the data structure.  This is the main
   tuning parameter to improve performance (at the expense of
   memory).  */
void objc_map_set_maximum_load_factor (objc_map_t map, int number_between_zero_and_one_hundred);

/* Read the maximum load factor.  */
int objc_map_maximum_load_factor (objc_map_t map);


/**
 ** Getting the value corresponding to a key.
 **/

/* This is the value returned by objc_map_get() when the value
   corresponding to a key is not found.  */
#define OBJC_MAP_NOT_FOUND (tree)1

/* objc_map_get() returns the value associated with a certain key,
   or OBJC_MAP_NOT_FOUND if there is no value associated with that key.
   Note that you can also use it to simply check if the map contains a
   pair with a certain key; just compare the result of calling
   objc_map_get() to OBJC_MAP_NOT_FOUND.

   It is essential to always check the results of the call to make
   sure it is not OBJC_MAP_NOT_FOUND.

   NULL is a valid value, so a key can be inserted into a map with
   value NULL, and objc_map_get() will return NULL in that case.
   So a result of NULL means that they key *was* found, and the value
   associated with it was NULL.  */
inline tree
objc_map_get (objc_map_t map, /* struct tree_identifier * */tree key)
{
  /* The inline implementation is private and may change without notice.  */
  objc_map_private_hash_t hash = IDENTIFIER_HASH_VALUE (key);
  size_t i = hash & map->mask;
  size_t j = 1;

  if (map->slots[i] == OBJC_MAP_PRIVATE_EMPTY_SLOT)
    return OBJC_MAP_NOT_FOUND;

  if (map->slots[i] == key)
    return map->values[i];

  while (1)
    {
      i = (i + j) & map->mask;

      if (map->slots[i] == OBJC_MAP_PRIVATE_EMPTY_SLOT)
	return OBJC_MAP_NOT_FOUND;

      if (map->slots[i] == key)
	return map->values[i];

      j++;
    }
}

/* objc_map_put() puts a key/value pair into the map.  If the map does
   not contain the key, it is added to it with the specified value.
   If the map already contains the key, the previous value is replaced
   with the new one.

   You can use any identifier as key, with the exception of NULL.

   You can use any tree as value, including NULL.  */
inline
void objc_map_put (objc_map_t map, /*struct tree_identifier * */tree key, tree value)
{
  /* The inline implementation is private and may change without notice.  */
  objc_map_private_hash_t hash = IDENTIFIER_HASH_VALUE (key);
  size_t i, j = 0;

  if (map->number_of_non_empty_slots == map->max_number_of_non_empty_slots)
    objc_map_private_grow (map);

  i = hash & map->mask;

  while (1)
    {
      if (map->slots[i] == OBJC_MAP_PRIVATE_EMPTY_SLOT)
	{
	  map->number_of_non_empty_slots++;
	  map->slots[i] = key;
	  map->values[i] = value;
	  return;
	}
      if (map->slots[i] == key)
	{
	  map->values[i] = value;
	  return;
	}

      j++;
      i = (i + j) & map->mask;
    }
}

/**
 ** Iterating over a map using an iterator.
 **/

/* When using iterators you can iterate directly on the elements in
   the map, and take an action over each one.

   Here is how you iterate over a hmap_pointer using iterators:

   objc_map_iterator_t i;

   objc_map_iterator_initialize (map, &i);

   while (objc_map_iterator_move_to_next (map, &i))
     {
       tree p = objc_map_iterator_current_key (map, i);
       tree q = objc_map_iterator_current_value (map, i);

       ... do something with p and q ...
     }

   You'll notice that the functions that modify the iterator (to
   initialize it, or move it to the next element) take a pointer to it
   as argument (as in "&i"), while the functions that only read its
   state (to read the current key/value, or remove the current
   key/value from the map) take it as a direct argument (as in "i").

   Note that all the objc_map_iterator_*() functions are inline and if
   you follow the pattern above, the compiler should be able to inline
   everything into a very efficient loop, roughly equivalent to
   hand-writing a C loop that iterates directly onto the hmap_pointer
   internal data structures.  */

/* A objc_map_iterator_t variable encapsulates the state of an
   iteration.  The fact that this is actually a size_t (pointing to
   the index of the slot that we return next) is an internal, private
   detail of the implementation and may change without notice.  */
typedef size_t objc_map_iterator_t;

/* Initialize an iterator to iterate over the specified objc_map.  You
   must use this before starting the iteration, to get a working
   iterator.  */
inline
void
objc_map_iterator_initialize (objc_map_t map ATTRIBUTE_UNUSED, objc_map_iterator_t *i)
{
  /* The inline implementation is private and may change without notice.  */
  /* This is trivial, but the same API would work to initialize more
     complicated iterators.  */
  *i = 0;
}

#define OBJC_MAP_FAILURE 0
#define OBJC_MAP_SUCCESS 1

/* Move the iterator to the next key/value pair, and return
   OBJC_MAP_SUCCESS if there is such a key/value pair, and
   OBJC_MAP_FAILURE if there are no more ones.  The iterator must have
   been initialized using objc_map_iterator_initialize().  Note that
   because this function is modifying the iterator, you need to pass a
   pointer to it.  */
inline
int
objc_map_iterator_move_to_next (objc_map_t map, objc_map_iterator_t *i)
{
  /* The inline implementation is private and may change without notice.  */
  while (1)
    {
      void *slot;
      if (*i == map->number_of_slots)
	return OBJC_MAP_FAILURE;

      slot = map->slots[*i];
      *i = *i + 1;
      if (slot != OBJC_MAP_PRIVATE_EMPTY_SLOT)
	return OBJC_MAP_SUCCESS;
    }
}

/* Return the current key.  You can only call it after you have called
   objc_map_iterator_move_to_next() at least once (to move to the
   first element), and only if the last call returned
   OBJC_MAP_SUCCESS.  The behavior is otherwise undefined, probably a
   segmentation fault.  */
inline
tree
objc_map_iterator_current_key (objc_map_t map, objc_map_iterator_t i)
{
  /* The inline implementation is private and may change without notice.  */
  return map->slots[i - 1];
}

/* Return the current value.  You can only call it after you have
   called objc_map_iterator_move_to_next() at least once (to move to
   the first element), and only if the last call returned
   OBJC_MAP_SUCCESS.  The behavior is otherwise undefined, probably a
   segmentation fault.  */
inline
tree
objc_map_iterator_current_value (objc_map_t map, objc_map_iterator_t i)
{
  /* The inline implementation is private and may change without notice.  */
  return map->values[i - 1];
}

#endif /* OBJC_MAP_H */
