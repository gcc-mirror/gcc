/* Hash tables for the CPP library.
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

static cpp_hashnode *alloc_node PARAMS ((hash_table *));

/* Return an identifier node for hashtable.c.  Used by cpplib except
   when integrated with the C front ends.  */

static cpp_hashnode *
alloc_node (table)
     hash_table *table;
{
  cpp_hashnode *node;
  
  node = (cpp_hashnode *) obstack_alloc (&table->pfile->hash_ob,
					 sizeof (cpp_hashnode));
  memset ((PTR) node, 0, sizeof (cpp_hashnode));
  return node;
}

/* Set up the identifier hash table.  Use TABLE if non-null, otherwise
   create our own.  */

void
_cpp_init_hashtable (pfile, table)
     cpp_reader *pfile;
     hash_table *table;
{
  if (table == NULL)
    {
      pfile->our_hashtable = 1;
      table = ht_create (13);	/* 8K (=2^13) entries.  */
      table->alloc_node = (hashnode (*) PARAMS ((hash_table *))) alloc_node;
      gcc_obstack_init (&pfile->hash_ob);
    }

  table->pfile = pfile;
  pfile->hash_table = table;
}

/* Tear down the identifier hash table.  */

void
_cpp_destroy_hashtable (pfile)
     cpp_reader *pfile;
{
  if (pfile->our_hashtable)
    {
      free (pfile->hash_table);
      obstack_free (&pfile->hash_ob, 0);
    }
}

/* Returns the hash entry for the STR of length LEN, creating one
   if necessary.  */

cpp_hashnode *
cpp_lookup (pfile, str, len)
     cpp_reader *pfile;
     const unsigned char *str;
     unsigned int len;
{
  /* ht_lookup cannot return NULL.  */
  return CPP_HASHNODE (ht_lookup (pfile->hash_table, str, len, HT_ALLOC));
}

/* Determine whether the str STR, of length LEN, is a defined macro.  */

int
cpp_defined (pfile, str, len)
     cpp_reader *pfile;
     const unsigned char *str;
     int len;
{
  cpp_hashnode *node;

  node = CPP_HASHNODE (ht_lookup (pfile->hash_table, str, len, HT_NO_INSERT));

  /* If it's of type NT_MACRO, it cannot be poisoned.  */
  return node && node->type == NT_MACRO;
}

/* For all nodes in the hashtable, callback CB with parameters PFILE,
   the node, and V.  */

void
cpp_forall_identifiers (pfile, cb, v)
     cpp_reader *pfile;
     cpp_cb cb;
     PTR v;
{
  /* We don't need a proxy since the hash table's identifier comes
     first in cpp_hashnode.  */
  ht_forall (pfile->hash_table, (ht_cb) cb, v);
}
