/* CYGNUS LOCAL: whole file jason */
/* Header file for generic hash table support.
   Copyright (C) 1993, 94 Free Software Foundation, Inc.
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
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifdef IN_GCC

/* Add prototype support.  */
#ifndef PROTO
#if defined (USE_PROTOTYPES) ? USE_PROTOTYPES : defined (__STDC__)
#define PROTO(ARGS) ARGS
#else
#define PROTO(ARGS) ()
#endif
#endif

#define PARAMS(ARGS) PROTO(ARGS)

#ifdef __STDC__
#define PTR void *
#else
#ifndef const
#define const
#endif
#define PTR char *
#endif

#else /* ! IN_GCC */
#include <ansidecl.h>
#endif /* IN_GCC */

#include "obstack.h"

typedef enum {false, true} boolean;

/* Hash table routines.  There is no way to free up a hash table.  */

/* An element in the hash table.  Most uses will actually use a larger
   structure, and an instance of this will be the first field.  */

struct hash_entry
{
  /* Next entry for this hash code.  */
  struct hash_entry *next;
  /* String being hashed.  */
  const char *string;
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
					 const char *));
  /* An obstack for this hash table.  */
  struct obstack memory;
};

/* Initialize a hash table.  */
extern boolean hash_table_init
  PARAMS ((struct hash_table *,
	   struct hash_entry *(*) (struct hash_entry *,
				   struct hash_table *,
				   const char *)));

/* Initialize a hash table specifying a size.  */
extern boolean hash_table_init_n
  PARAMS ((struct hash_table *,
	   struct hash_entry *(*) (struct hash_entry *,
				   struct hash_table *,
				   const char *),
	   unsigned int size));

/* Free up a hash table.  */
extern void hash_table_free PARAMS ((struct hash_table *));

/* Look up a string in a hash table.  If CREATE is true, a new entry
   will be created for this string if one does not already exist.  The
   COPY argument must be true if this routine should copy the string
   into newly allocated memory when adding an entry.  */
extern struct hash_entry *hash_lookup
  PARAMS ((struct hash_table *, const char *, boolean create,
	   boolean copy));

/* Base method for creating a hash table entry.  */
extern struct hash_entry *hash_newfunc
  PARAMS ((struct hash_entry *, struct hash_table *,
	   const char *));

/* Grab some space for a hash table entry.  */
extern PTR hash_allocate PARAMS ((struct hash_table *,
				  unsigned int));

/* Traverse a hash table in a random order, calling a function on each
   element.  If the function returns false, the traversal stops.  The
   INFO argument is passed to the function.  */
extern void hash_traverse PARAMS ((struct hash_table *,
				   boolean (*) (struct hash_entry *,
						PTR),
				   PTR info));
