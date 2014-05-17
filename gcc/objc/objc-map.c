/* objc-map.c -- Implementation of map data structures for ObjC compiler
   Copyright (C) 2011-2014 Free Software Foundation, Inc.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "ggc.h"
#include "objc-map.h"

#define OUT_OF_MEMORY { fprintf (stderr, "Out of memory\n"); abort (); }

static
size_t
ATTRIBUTE_PURE
next_power_of_two (size_t x)
{
  size_t result = 1;

  if (x < 2)
    return 2;

  /* Avoid the long calculation if x is already a power of two.  Since
     we internally always increase/shrink tables by powers of 2, the
     calculation should only be done once, when the table is first
     set up.  */
  if ((x & (x - 1)) == 0)
    return x;

  /* Calculate log_2 by counting how many times we can divide by 2
     before reaching 0.  */
  while (x > 0)
    {
      x = x >> 1;
      result = result << 1;
    }
  return result;
}

objc_map_t
objc_map_alloc_ggc (size_t initial_capacity)
{
  objc_map_t map = ggc_cleared_alloc<objc_map_private> ();
  if (map == NULL)
    OUT_OF_MEMORY;
  
  initial_capacity = next_power_of_two (initial_capacity);
  
  map->number_of_slots = initial_capacity;
  map->mask = initial_capacity - 1;
  map->maximum_load_factor = 70;
  map->max_number_of_non_empty_slots = (initial_capacity * map->maximum_load_factor) / 100;

  map->slots = ggc_cleared_vec_alloc<tree> (initial_capacity);
  map->values = ggc_cleared_vec_alloc<tree> (initial_capacity);

  if (map->slots == NULL)
    OUT_OF_MEMORY;
  
  if (map->values == NULL)
    OUT_OF_MEMORY;

  return map;
}

void
objc_map_set_maximum_load_factor (objc_map_t map, int number_between_zero_and_one_hundred)
{
  if (map->number_of_non_empty_slots != 0)
    return;

  map->maximum_load_factor = number_between_zero_and_one_hundred;
  map->max_number_of_non_empty_slots = (map->number_of_slots * number_between_zero_and_one_hundred) / 100;
}

int
objc_map_maximum_load_factor (objc_map_t map)
{
  return map->maximum_load_factor;
}

static void
objc_map_private_resize (objc_map_t map, size_t new_number_of_slots)
{
  tree *old_slots = map->slots;
  tree *old_values = map->values;
  size_t i, old_number_of_slots = map->number_of_slots;
  
  if (new_number_of_slots < (map->number_of_non_empty_slots))
    new_number_of_slots = 2 * map->number_of_non_empty_slots;

  new_number_of_slots = next_power_of_two (new_number_of_slots);
  
  map->number_of_slots = new_number_of_slots;
  map->mask = map->number_of_slots - 1;
  map->max_number_of_non_empty_slots = (map->number_of_slots * map->maximum_load_factor) / 100;


  map->slots = ggc_cleared_vec_alloc<tree> (map->number_of_slots);
  map->values = ggc_cleared_vec_alloc<tree> (map->number_of_slots);

  if (map->slots == NULL)
    OUT_OF_MEMORY;

  if (map->values == NULL)
    OUT_OF_MEMORY;

  for (i = 0; i < old_number_of_slots; i++)
    if (old_slots[i] != OBJC_MAP_PRIVATE_EMPTY_SLOT)
      {
	size_t k = IDENTIFIER_HASH_VALUE (old_slots[i]) & map->mask;
	
	if (map->slots[k] == OBJC_MAP_PRIVATE_EMPTY_SLOT)
	  {
	    map->slots[k] = old_slots[i];
	    map->values[k] = old_values[i];
	  }
	else
	  {
	    size_t j = 1;
	    while (1)
	      {
		k = (k + j) & map->mask;
		if (map->slots[k] == OBJC_MAP_PRIVATE_EMPTY_SLOT)
		  {
		    map->slots[k] = old_slots[i];
		    map->values[k] = old_values[i];
		    break;
		  }
		j++;
	      }
	  }
      }

  ggc_free (old_slots);
  ggc_free (old_values);
}

void
objc_map_private_grow (struct objc_map_private *map)
{
  objc_map_private_resize (map, map->number_of_slots * 2);
}

#include "gt-objc-objc-map.h"
