/* hash.c -- hash table routines
   Copyright (C) 1993, 1994, 1998, 2001 Free Software Foundation, Inc.
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

#include "config.h"
#include "system.h"
#include "hash.h"
#include "obstack.h"
#include "toplev.h"

/* Obstack allocation and deallocation routines.  */
#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

/* The default number of entries to use when creating a hash table.  */
#define DEFAULT_SIZE 1009

/* Create a new hash table, given a number of entries.  */

void
hash_table_init_n (table, newfunc, hash, comp, size)
     struct hash_table *table;
     struct hash_entry *(*newfunc) PARAMS ((struct hash_entry *,
					    struct hash_table *,
					    hash_table_key));
     unsigned long (*hash) PARAMS ((hash_table_key));
     bool (*comp) PARAMS ((hash_table_key, hash_table_key));
     unsigned int size;
{
  unsigned int alloc;

  alloc = size * sizeof (struct hash_entry *);
  obstack_begin (&table->memory, alloc);
  table->table = ((struct hash_entry **)
		  obstack_alloc (&table->memory, alloc));
  memset ((PTR) table->table, 0, alloc);
  table->size = size;
  table->newfunc = newfunc;
  table->hash = hash;
  table->comp = comp;
}

/* Create a new hash table with the default number of entries.  */

void
hash_table_init (table, newfunc, hash, comp)
     struct hash_table *table;
     struct hash_entry *(*newfunc) PARAMS ((struct hash_entry *,
					    struct hash_table *,
					    hash_table_key));
     unsigned long (*hash) PARAMS ((hash_table_key));
     bool (*comp) PARAMS ((hash_table_key, hash_table_key));
{
  hash_table_init_n (table, newfunc, hash, comp, DEFAULT_SIZE);
}

/* Free a hash table.  */

void
hash_table_free (table)
     struct hash_table *table;
{
  obstack_free (&table->memory, (PTR) NULL);
}

/* Look up KEY in TABLE.  If CREATE is non-NULL a new entry is
   created if one does not previously exist.  */

struct hash_entry *
hash_lookup (table, key, create, copy)
     struct hash_table *table;
     hash_table_key key;
     int create;
     hash_table_key (*copy) PARAMS ((struct obstack* memory, 
				     hash_table_key key));
{
  unsigned long hash;
  struct hash_entry *hashp;
  unsigned int index;
  
  hash = (*table->hash)(key);

  index = hash % table->size;
  for (hashp = table->table[index]; hashp != 0; hashp = hashp->next)
    if (hashp->hash == hash
	&& (*table->comp)(hashp->key, key))
      return hashp;

  if (! create)
    return 0;

  hashp = (*table->newfunc) ((struct hash_entry *) NULL, table, key);
  if (hashp == 0)
    return 0;

  if (copy)
    key = (*copy) (&table->memory, key);

  hashp->key = key;
  hashp->hash = hash;
  hashp->next = table->table[index];
  table->table[index] = hashp;

  return hashp;
}

/* Base method for creating a new hash table entry.  */

struct hash_entry *
hash_newfunc (entry, table, p)
     struct hash_entry *entry;
     struct hash_table *table;
     hash_table_key p ATTRIBUTE_UNUSED;
{
  if (entry == 0)
    entry = ((struct hash_entry *)
	     hash_allocate (table, sizeof (struct hash_entry)));
  return entry;
}

/* Allocate space in a hash table.  */

PTR
hash_allocate (table, size)
     struct hash_table *table;
     unsigned int size;
{
  return obstack_alloc (&table->memory, size);
}

/* Traverse a hash table.  */

void
hash_traverse (table, func, info)
     struct hash_table *table;
     bool (*func) PARAMS ((struct hash_entry *, hash_table_key));
     PTR info;
{
  unsigned int i;
  struct hash_entry *p;

  for (i = 0; i < table->size; i++)
    for (p = table->table[i]; p != 0; p = p->next)
      if (! (*func) (p, info))
	return;
}

/* Hash a string.  Return a hash-code for the string.  */

unsigned long
string_hash (k)
     hash_table_key k;
{
  const unsigned char *s;
  unsigned long hash;
  unsigned char c;
  unsigned int len;

  s = (const unsigned char *) k;
  hash = 0;
  len = 0;

  while ((c = *s++) != '\0')
    {
      hash += c + (c << 17);
      hash ^= hash >> 2;
      ++len;
    }

  hash += len + (len << 17);
  hash ^= hash >> 2;

  return hash;
}

/* Compare two strings.  Return non-zero iff the two strings are
   the same.  */

bool
string_compare (k1, k2)
     hash_table_key k1;
     hash_table_key k2;
{
  return (strcmp ((char*) k1, (char*) k2) == 0);
}

/* Copy K to OBSTACK.  */

hash_table_key
string_copy (memory, k)
     struct obstack *memory;
     hash_table_key k;
{
  char *new;
  char *string = (char *) k;

  new = (char *) obstack_alloc (memory, strlen (string) + 1);
  strcpy (new, string);
  
  return new;
}
