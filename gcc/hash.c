/* CYGNUS LOCAL: whole file jason */
/* hash.c -- hash table routines
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

#include "config.h"
#include "hash.h"
#include "obstack.h"

extern void free PARAMS ((PTR));

/* Obstack allocation and deallocation routines.  */
#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

extern char * xmalloc ();

/* The default number of entries to use when creating a hash table.  */
#define DEFAULT_SIZE (1009)

#ifndef NULL
#define NULL 0
#endif

/* Create a new hash table, given a number of entries.  */

boolean
hash_table_init_n (table, newfunc, size)
     struct hash_table *table;
     struct hash_entry *(*newfunc) PARAMS ((struct hash_entry *,
						struct hash_table *,
						const char *));
     unsigned int size;
{
  unsigned int alloc;

  alloc = size * sizeof (struct hash_entry *);
  if (!obstack_begin (&table->memory, alloc))
    {
      error ("no memory");
      return false;
    }
  table->table = ((struct hash_entry **)
		  obstack_alloc (&table->memory, alloc));
  if (!table->table)
    {
      error ("no memory");
      return false;
    }
  memset ((PTR) table->table, 0, alloc);
  table->size = size;
  table->newfunc = newfunc;
  return true;
}

/* Create a new hash table with the default number of entries.  */

boolean
hash_table_init (table, newfunc)
     struct hash_table *table;
     struct hash_entry *(*newfunc) PARAMS ((struct hash_entry *,
						struct hash_table *,
						const char *));
{
  return hash_table_init_n (table, newfunc, DEFAULT_SIZE);
}

/* Free a hash table.  */

void
hash_table_free (table)
     struct hash_table *table;
{
  obstack_free (&table->memory, (PTR) NULL);
}

/* Look up a string in a hash table.  */

struct hash_entry *
hash_lookup (table, string, create, copy)
     struct hash_table *table;
     const char *string;
     boolean create;
     boolean copy;
{
  register const unsigned char *s;
  register unsigned long hash;
  register unsigned int c;
  struct hash_entry *hashp;
  unsigned int len;
  unsigned int index;
  
  hash = 0;
  len = 0;
  s = (const unsigned char *) string;
  while ((c = *s++) != '\0')
    {
      hash += c + (c << 17);
      hash ^= hash >> 2;
      ++len;
    }
  hash += len + (len << 17);
  hash ^= hash >> 2;

  index = hash % table->size;
  for (hashp = table->table[index];
       hashp != (struct hash_entry *) NULL;
       hashp = hashp->next)
    {
      if (hashp->hash == hash
	  && strcmp (hashp->string, string) == 0)
	return hashp;
    }

  if (! create)
    return (struct hash_entry *) NULL;

  hashp = (*table->newfunc) ((struct hash_entry *) NULL, table, string);
  if (hashp == (struct hash_entry *) NULL)
    return (struct hash_entry *) NULL;
  if (copy)
    {
      char *new;

      new = (char *) obstack_alloc (&table->memory, len + 1);
      if (!new)
	{
	  error ("no memory");
	  return (struct hash_entry *) NULL;
	}
      strcpy (new, string);
      string = new;
    }
  hashp->string = string;
  hashp->hash = hash;
  hashp->next = table->table[index];
  table->table[index] = hashp;

  return hashp;
}

/* Base method for creating a new hash table entry.  */

/*ARGSUSED*/
struct hash_entry *
hash_newfunc (entry, table, string)
     struct hash_entry *entry;
     struct hash_table *table;
     const char *string;
{
  if (entry == (struct hash_entry *) NULL)
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
  PTR ret;

  ret = obstack_alloc (&table->memory, size);
  if (ret == NULL && size != 0)
    error ("no memory");
  return ret;
}

/* Traverse a hash table.  */

void
hash_traverse (table, func, info)
     struct hash_table *table;
     boolean (*func) PARAMS ((struct hash_entry *, PTR));
     PTR info;
{
  unsigned int i;

  for (i = 0; i < table->size; i++)
    {
      struct hash_entry *p;

      for (p = table->table[i]; p != NULL; p = p->next)
	{
	  if (! (*func) (p, info))
	    return;
	}
    }
}
