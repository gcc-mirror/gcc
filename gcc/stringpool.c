/* String pool for GCC.
   Copyright (C) 2000-2014 Free Software Foundation, Inc.

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

/* String text, identifier text and identifier node allocator.
   Identifiers are uniquely stored in a hash table.

   We use cpplib's hash table implementation.  libiberty's
   hashtab.c is not used because it requires 100% average space
   overhead per string, which is unacceptable.  Also, this algorithm
   is faster.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "ggc.h"
#include "ggc-internal.h"
#include "tree.h"
#include "symtab.h"
#include "cpplib.h"

/* The "" allocated string.  */
const char empty_string[] = "";

/* Character strings, each containing a single decimal digit.
   Written this way to save space.  */
static const char digit_vector[] = {
  '0', 0, '1', 0, '2', 0, '3', 0, '4', 0,
  '5', 0, '6', 0, '7', 0, '8', 0, '9', 0
};

#define digit_string(d) (digit_vector + ((d) * 2))

struct ht *ident_hash;

static hashnode alloc_node (cpp_hash_table *);
static int mark_ident (struct cpp_reader *, hashnode, const void *);

static void *
stringpool_ggc_alloc (size_t x)
{
  return ggc_alloc_atomic (x);
}

/* Initialize the string pool.  */
void
init_stringpool (void)
{
  /* Create with 16K (2^14) entries.  */
  ident_hash = ht_create (14);
  ident_hash->alloc_node = alloc_node;
  ident_hash->alloc_subobject = stringpool_ggc_alloc;
}

/* Allocate a hash node.  */
static hashnode
alloc_node (cpp_hash_table *table ATTRIBUTE_UNUSED)
{
  return GCC_IDENT_TO_HT_IDENT (make_node (IDENTIFIER_NODE));
}

/* Allocate and return a string constant of length LENGTH, containing
   CONTENTS.  If LENGTH is -1, CONTENTS is assumed to be a
   nul-terminated string, and the length is calculated using strlen.  */

const char *
ggc_alloc_string_stat (const char *contents, int length MEM_STAT_DECL)
{
  char *result;

  if (length == -1)
    length = strlen (contents);

  if (length == 0)
    return empty_string;
  if (length == 1 && ISDIGIT (contents[0]))
    return digit_string (contents[0] - '0');

  result = (char *) ggc_alloc_atomic_stat (length + 1 PASS_MEM_STAT);
  memcpy (result, contents, length);
  result[length] = '\0';
  return (const char *) result;
}

/* Return an IDENTIFIER_NODE whose name is TEXT (a null-terminated string).
   If an identifier with that name has previously been referred to,
   the same node is returned this time.  */

#undef get_identifier

tree
get_identifier (const char *text)
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
get_identifier_with_length (const char *text, size_t length)
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
maybe_get_identifier (const char *text)
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
stringpool_statistics (void)
{
  ht_dump_statistics (ident_hash);
}

/* Mark an identifier for GC.  */

static int
mark_ident (struct cpp_reader *pfile ATTRIBUTE_UNUSED, hashnode h,
	    const void *v ATTRIBUTE_UNUSED)
{
  gt_ggc_m_9tree_node (HT_IDENT_TO_GCC_IDENT (h));
  return 1;
}

/* Return true if an identifier should be removed from the table.  */

static int
maybe_delete_ident (struct cpp_reader *pfile ATTRIBUTE_UNUSED, hashnode h,
		    const void *v ATTRIBUTE_UNUSED)
{
  return !ggc_marked_p (HT_IDENT_TO_GCC_IDENT (h));
}

/* Mark the trees hanging off the identifier node for GGC.  These are
   handled specially (not using gengtype) because identifiers are only
   roots during one part of compilation.  */

void
ggc_mark_stringpool (void)
{
  ht_forall (ident_hash, mark_ident, NULL);
}

/* Purge the identifier hash of identifiers which are no longer
   referenced.  */

void
ggc_purge_stringpool (void)
{
  ht_purge (ident_hash, maybe_delete_ident, NULL);
}

/* Pointer-walking routine for strings (not very interesting, since
   strings don't contain pointers).  */

void
gt_pch_p_S (void *obj ATTRIBUTE_UNUSED, void *x ATTRIBUTE_UNUSED,
	    gt_pointer_operator op ATTRIBUTE_UNUSED,
	    void *cookie ATTRIBUTE_UNUSED)
{
}

/* PCH pointer-walking routine for strings.  */

void
gt_pch_n_S (const void *x)
{
  gt_pch_note_object (CONST_CAST (void *, x), CONST_CAST (void *, x),
		      &gt_pch_p_S);
}


/* User-callable entry point for marking string X.  */

void
gt_pch_nx (const char *& x)
{
  gt_pch_n_S (x);
}

void
gt_pch_nx (unsigned char *& x)
{
  gt_pch_n_S (x);
}

void
gt_pch_nx (unsigned char& x ATTRIBUTE_UNUSED)
{
}

void
gt_pch_nx (unsigned char *x, gt_pointer_operator op, void *cookie)
{
  op (x, cookie);
}

/* Handle saving and restoring the string pool for PCH.  */

/* SPD is saved in the PCH file and holds the information needed
   to restore the string pool.  */

struct GTY(()) string_pool_data {
  ht_identifier_ptr *
    GTY((length ("%h.nslots"),
	 nested_ptr (union tree_node, "%h ? GCC_IDENT_TO_HT_IDENT (%h) : NULL",
		     "%h ? HT_IDENT_TO_GCC_IDENT (%h) : NULL")))
    entries;
  unsigned int nslots;
  unsigned int nelements;
};

static GTY(()) struct string_pool_data * spd;

/* Save the stringpool data in SPD.  */

void
gt_pch_save_stringpool (void)
{
  spd = ggc_alloc_string_pool_data ();
  spd->nslots = ident_hash->nslots;
  spd->nelements = ident_hash->nelements;
  spd->entries = ggc_alloc_vec_ht_identifier_ptr (spd->nslots);
  memcpy (spd->entries, ident_hash->entries,
	  spd->nslots * sizeof (spd->entries[0]));
}

/* Return the stringpool to its state before gt_pch_save_stringpool
   was called.  */

void
gt_pch_fixup_stringpool (void)
{
}

/* A PCH file has been restored, which loaded SPD; fill the real hash table
   from SPD.  */

void
gt_pch_restore_stringpool (void)
{
  ht_load (ident_hash, spd->entries, spd->nslots, spd->nelements, false);
  spd = NULL;
}

#include "gt-stringpool.h"
