/* Header file for generic hash table support.
   Copyright (C) 1993, 1994, 1997, 1998 Free Software Foundation, Inc.
   Written by Steve Chamberlain <sac@cygnus.com>

This file was lifted from BFD, the Binary File Descriptor library.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef IN_GCC
#include <ansidecl.h>
#endif /* ! IN_GCC */

#include "obstack.h"

typedef enum {false, true} boolean;

typedef PTR hash_table_key;

/* Hash table routines.  There is no way to free up a hash table.  */

/* An element in the hash table.  Most uses will actually use a larger
   structure, and an instance of this will be the first field.  */

struct hash_entry
{
  /* Next entry for this hash code.  */
  struct hash_entry *next;
  /* The thing being hashed.  */
  hash_table_key key;
  /* Hash code.  This is the full hash code, not the index into the
     table.  */
  unsigned long hash;
};

/* A hash table.  */

struct hash_table
{
  /* The hash array.  */
  struct hash_entry **table;
  /* The number of slots in the hash table.  */
  unsigned int size;
  /* A function used to create new elements in the hash table.  The
     first entry is itself a pointer to an element.  When this
     function is first invoked, this pointer will be NULL.  However,
     having the pointer permits a hierarchy of method functions to be
     built each of which calls the function in the superclass.  Thus
     each function should be written to allocate a new block of memory
     only if the argument is NULL.  */
  struct hash_entry *(*newfunc) PARAMS ((struct hash_entry *,
					 struct hash_table *,
					 hash_table_key));
  /* A function to compute the hash code for a key in the hash table.  */
  unsigned long (*hash) PARAMS ((hash_table_key));
  /* A function to compare two keys.  */
  boolean (*comp) PARAMS ((hash_table_key, hash_table_key));
  /* An obstack for this hash table.  */
  struct obstack memory;
};

/* Initialize a hash table.  */
extern boolean hash_table_init
  PARAMS ((struct hash_table *,
	   struct hash_entry *(*) (struct hash_entry *,
				   struct hash_table *,
				   hash_table_key),
	   unsigned long (*hash) (hash_table_key),
	   boolean (*comp) (hash_table_key, hash_table_key)));

/* Initialize a hash table specifying a size.  */
extern boolean hash_table_init_n
  PARAMS ((struct hash_table *,
	   struct hash_entry *(*) (struct hash_entry *,
				   struct hash_table *,
				   hash_table_key),
	   unsigned long (*hash) (hash_table_key),
	   boolean (*comp) (hash_table_key, hash_table_key),
	   unsigned int size));

/* Free up a hash table.  */
extern void hash_table_free PARAMS ((struct hash_table *));

/* Look up KEY in a hash table.  If CREATE is true, a new entry
   will be created for this KEY if one does not already exist.  If
   COPY is non-NULL, it is used to copy the KEY before storing it in
   the hash table.  */
extern struct hash_entry *hash_lookup
  PARAMS ((struct hash_table *, hash_table_key key, boolean create,
	   hash_table_key (*copy)(struct obstack*, hash_table_key)));

/* Base method for creating a hash table entry.  */
extern struct hash_entry *hash_newfunc
  PARAMS ((struct hash_entry *, struct hash_table *, 
	   hash_table_key key));

/* Grab some space for a hash table entry.  */
extern PTR hash_allocate PARAMS ((struct hash_table *,
				  unsigned int));

/* Traverse a hash table in a random order, calling a function on each
   element.  If the function returns false, the traversal stops.  The
   INFO argument is passed to the function.  */
extern void hash_traverse PARAMS ((struct hash_table *,
				   boolean (*) (struct hash_entry *,
						hash_table_key),
				   hash_table_key info));

/* Hash a string K, which is really of type `char*'.  */
extern unsigned long string_hash PARAMS ((hash_table_key k));

/* Compare two strings K1, K2 which are really of type `char*'.  */
extern boolean string_compare PARAMS ((hash_table_key k1, 
				       hash_table_key k2));

/* Copy a string K, which is really of type `char*'.  */
extern hash_table_key string_copy PARAMS ((struct obstack* memory,
					   hash_table_key k));

