/* Part of CPP library.  (Identifier and string tables.)
   Copyright (C) 1986, 1987, 1989, 1992, 1993, 1994, 1995, 1996, 1998,
   1999, 2000 Free Software Foundation, Inc.
   Written by Per Bothner, 1994.
   Based on CCCP program by Paul Rubin, June 1986
   Adapted to ANSI C, Richard Stallman, Jan 1987

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!  */

#include "config.h"
#include "system.h"
#include "cpplib.h"
#include "cpphash.h"
#include "obstack.h"

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

/* Initial hash table size.  (It can grow if necessary.)  This is the
   largest prime number smaller than 2**12. */
#define HASHSIZE 4093

/* This is the structure used for the hash table.  */
struct htab
{
  struct cpp_hashnode **entries;
  size_t size;
  size_t nelts;
};

static void expand_hash PARAMS ((struct htab *));
static unsigned long higher_prime_number PARAMS ((unsigned long));

/* Set up and tear down internal structures for macro expansion.  */
void
_cpp_init_macros (pfile)
     cpp_reader *pfile;
{
  pfile->hash_ob = xnew (struct obstack);
  obstack_init (pfile->hash_ob);

  pfile->hashtab = xobnew (pfile->hash_ob, struct htab);

  pfile->hashtab->nelts = 0;
  pfile->hashtab->size = HASHSIZE;
  pfile->hashtab->entries = xcnewvec (cpp_hashnode *, HASHSIZE);
}

void
_cpp_cleanup_macros (pfile)
     cpp_reader *pfile;
{
  cpp_hashnode **p, **limit;

  p = pfile->hashtab->entries;
  limit = p + pfile->hashtab->size;
  do
    {
      if (*p)
	{
	  _cpp_free_definition (*p);
	  (*p)->fe_value = 0;  /* expose the node to GC */
	}
    }
  while (++p < limit);

  free (pfile->hashtab->entries);
  obstack_free (pfile->hash_ob, 0);
  free (pfile->hash_ob);
}

/* The code below is a specialization of Vladimir Makarov's expandable
   hash tables (see libiberty/hashtab.c).  The abstraction penalty was
   too high to continue using the generic form.  This code knows
   intrinsically how to calculate a hash value, and how to compare an
   existing entry with a potential new one.  Also, the ability to
   delete members from the table has been removed.  */

cpp_hashnode *
cpp_lookup (pfile, name, len)
     cpp_reader *pfile;
     const U_CHAR *name;
     size_t len;
{
  size_t n = len;
  unsigned int r = 0;
  const U_CHAR *str = name;

  do
    {
      r = HASHSTEP (r, *str);
      str++;
    }
  while (--n);

  return _cpp_lookup_with_hash (pfile, name, len, r);
}

cpp_hashnode *
_cpp_lookup_with_hash (pfile, name, len, hash)
     cpp_reader *pfile;
     const U_CHAR *name;
     size_t len;
     unsigned int hash;
{
  unsigned int index;
  unsigned int hash2;
  size_t size;
  cpp_hashnode *entry;
  cpp_hashnode **entries;

  entries = pfile->hashtab->entries;
  size = pfile->hashtab->size;

  hash += len;
  index = hash % size;

  entry = entries[index];
  if (entry == NULL)
    goto insert;
  if (entry->hash == hash && entry->length == len
      && !memcmp (entry->name, name, len))
    return entry;

  hash2 = 1 + hash % (size - 2);

  for (;;)
    {
      index += hash2;
      if (index >= size)
	index -= size;
      entry = entries[index];

      if (entry == NULL)
	goto insert;
      if (entry->hash == hash && entry->length == len
	  && !memcmp (entry->name, name, len))
	return entry;
    }

 insert:
  pfile->hashtab->nelts++;

  /* Create a new hash node.  */
  {
    U_CHAR *p = obstack_alloc (pfile->hash_ob, sizeof (cpp_hashnode) + len);
    entry = (cpp_hashnode *)p;
    p += offsetof (cpp_hashnode, name);
    
    entry->type = T_VOID;
    entry->fe_value = 0;
    entry->length = len;
    entry->hash = hash;
    entry->value.expansion = NULL;
    memcpy (p, name, len);
    p[len] = 0;

    entries[index] = entry;
  }

  if (size * 3 <= pfile->hashtab->nelts * 4)
    expand_hash (pfile->hashtab);

  return entry;
}

static void
expand_hash (htab)
     struct htab *htab;
{
  cpp_hashnode **oentries;
  cpp_hashnode **olimit;
  cpp_hashnode **p;
  size_t size;

  oentries = htab->entries;
  olimit = oentries + htab->size;

  htab->size = size = higher_prime_number (htab->size * 2);
  htab->entries = xcnewvec (cpp_hashnode *, size);

  for (p = oentries; p < olimit; p++)
    {
      if (*p != NULL)
	{
	  unsigned int index;
	  unsigned int hash, hash2;
	  cpp_hashnode *entry = *p;

	  hash = entry->hash;
	  index = hash % size;

	  if (htab->entries[index] == NULL)
	    {
	    insert:
	      htab->entries[index] = entry;
	      continue;
	    }

	  hash2 = 1 + hash % (size - 2);
	  for (;;)
	    {
	      index += hash2;
	      if (index >= size)
		index -= size;

	      if (htab->entries[index] == NULL)
		goto insert;
	    }
	}
    }

  free (oentries);
}

/* The following function returns the nearest prime number which is
   greater than a given source number, N. */

static unsigned long
higher_prime_number (n)
     unsigned long n;
{
  unsigned long i;

  /* Ensure we have a larger number and then force to odd.  */
  n++;  
  n |= 0x01; 

  /* All odd numbers < 9 are prime.  */
  if (n < 9)
    return n;

  /* Otherwise find the next prime using a sieve.  */

 next:
  for (i = 3; i * i <= n; i += 2)
    if (n % i == 0)
      {
	 n += 2;
	 goto next;
       }

  return n;
}

void
cpp_forall_identifiers (pfile, cb)
     cpp_reader *pfile;
     int (*cb) PARAMS ((cpp_reader *, cpp_hashnode *));
{
    cpp_hashnode **p, **limit;

  p = pfile->hashtab->entries;
  limit = p + pfile->hashtab->size;
  do
    {
      if (*p)
	if ((*cb) (pfile, *p) == 0)
	  break;
    }
  while (++p < limit);
}
