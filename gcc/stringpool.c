/* String pool for GCC.
   Copyright (C) 2000, 2001 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* String text, identifier text and identifier node allocator.  Strings
   allocated by ggc_alloc_string are stored in an obstack which is
   never shrunk.  Identifiers are uniquely stored in a hash table.

   We have our own private hash table implementation.  libiberty's
   hashtab.c is not used because it requires 100% average space
   overhead per string, which is unacceptable.  Also, this algorithm
   is faster.  */

#include "config.h"
#include "system.h"
#include "ggc.h"
#include "tree.h"
#include "hashtable.h"

/* The "" allocated string.  */
const char empty_string[] = "";

/* Character strings, each containing a single decimal digit.
   Written this way to save space.  */
const char digit_vector[] = {
  '0', 0, '1', 0, '2', 0, '3', 0, '4', 0,
  '5', 0, '6', 0, '7', 0, '8', 0, '9', 0
};

struct ht *ident_hash;
static struct obstack string_stack;

static hashnode alloc_node PARAMS ((hash_table *));
static int mark_ident PARAMS ((struct cpp_reader *, hashnode, const PTR));
static void mark_ident_hash PARAMS ((void *));

/* Initialize the string pool.  */
void
init_stringpool ()
{
  /* Create with 16K (2^14) entries.  */
  ident_hash = ht_create (14);
  ident_hash->alloc_node = alloc_node;
  gcc_obstack_init (&string_stack);
  ggc_add_root (&ident_hash, 1, sizeof ident_hash, mark_ident_hash);
}

/* Allocate a hash node.  */
static hashnode
alloc_node (table)
     hash_table *table ATTRIBUTE_UNUSED;
{
  return GCC_IDENT_TO_HT_IDENT (make_node (IDENTIFIER_NODE));
}

/* Allocate and return a string constant of length LENGTH, containing
   CONTENTS.  If LENGTH is -1, CONTENTS is assumed to be a
   nul-terminated string, and the length is calculated using strlen.
   If the same string constant has been allocated before, that copy is
   returned this time too.  */

const char *
ggc_alloc_string (contents, length)
     const char *contents;
     int length;
{
  if (length == -1)
    length = strlen (contents);

  if (length == 0)
    return empty_string;
  if (length == 1 && ISDIGIT (contents[0]))
    return digit_string (contents[0] - '0');

  obstack_grow0 (&string_stack, contents, length);
  return obstack_finish (&string_stack);
}

/* Return an IDENTIFIER_NODE whose name is TEXT (a null-terminated string).
   If an identifier with that name has previously been referred to,
   the same node is returned this time.  */

tree
get_identifier (text)
     const char *text;
{
  hashnode ht_node = ht_lookup (ident_hash,
				(const unsigned char *) text,
				strlen (text), HT_ALLOC);

  /* ht_node can't be NULL here.  */
  return HT_IDENT_TO_GCC_IDENT (ht_node);
}

/* Identical to get_identifier, except that the length is assumed
   known.  */
   
tree
get_identifier_with_length (text, length)
     const char *text;
     unsigned int length;
{
  hashnode ht_node = ht_lookup (ident_hash,
				(const unsigned char *) text,
				length, HT_ALLOC);

  /* ht_node can't be NULL here.  */
  return HT_IDENT_TO_GCC_IDENT (ht_node);
}

/* If an identifier with the name TEXT (a null-terminated string) has
   previously been referred to, return that node; otherwise return
   NULL_TREE.  */

tree
maybe_get_identifier (text)
     const char *text;
{
  hashnode ht_node;

  ht_node = ht_lookup (ident_hash, (const unsigned char *) text,
		       strlen (text), HT_NO_INSERT);
  if (ht_node)
    return HT_IDENT_TO_GCC_IDENT (ht_node);

  return NULL_TREE;
}

/* Report some basic statistics about the string pool.  */

void
stringpool_statistics ()
{
  ht_dump_statistics (ident_hash);
}

/* Mark an identifier for GC.  */

static int
mark_ident (pfile, h, v)
     struct cpp_reader *pfile ATTRIBUTE_UNUSED;
     hashnode h;
     const PTR v ATTRIBUTE_UNUSED;
{
  ggc_mark_nonnull_tree (HT_IDENT_TO_GCC_IDENT (h));
  return 1;
}

/* Mark all identifiers for GC.  */

static void
mark_ident_hash (arg)
     PTR arg ATTRIBUTE_UNUSED;
{
  ht_forall (ident_hash, mark_ident, NULL);
}
