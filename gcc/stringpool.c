/* String pool for GCC.
   Copyright (C) 2000 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

GNU CC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* String pool allocator.  All strings allocated by ggc_alloc_string are
   uniquified and stored in an obstack which is never shrunk.  You can
   associate a tree with a string if you wish; this is used to implement
   get_identifier.

   We have our own private hash table implementation which is similar
   to the one in cpphash.c (actually, it's a further refinement of
   that code).  libiberty's hashtab.c is not used because it requires
   100% average space overhead per string, which is unacceptable.
   Also, this algorithm is faster.  */

#include "config.h"
#include "system.h"
#include "ggc.h"
#include "tree.h"
#include "obstack.h"
#include "flags.h"
#include "toplev.h"

/* The "" allocated string.  */
const char empty_string[] = "";

/* Character strings, each containing a single decimal digit.
   Written this way to save space.  */
const char digit_vector[] = {
  '0', 0, '1', 0, '2', 0, '3', 0, '4', 0,
  '5', 0, '6', 0, '7', 0, '8', 0, '9', 0
};

static struct obstack string_stack;

/* Each hashnode is just a pointer to a TREE_IDENTIFIER.  */
typedef struct tree_identifier *sp_hashnode;

#define SP_EMPTY(NODE) ((NODE) == NULL)
#define SP_LEN(NODE) ((NODE)->length)
#define SP_TREE(NODE) ((tree) NODE)
#define SP_STR(NODE) ((NODE)->pointer)
#define SP_VALID(NODE) (TREE_CODE (SP_TREE (NODE)) == IDENTIFIER_NODE)

/* This is the hash table structure.  There's only one.  */
struct str_hash
{
  sp_hashnode *entries;
  size_t nslots;	/* total slots in the entries array */
  size_t nelements;	/* number of live elements */

  /* table usage statistics */
  unsigned int searches;
  unsigned int collisions;
};
#define INITIAL_HASHSIZE (16*1024)

static struct str_hash string_hash = { 0, INITIAL_HASHSIZE, 0, 0, 0 };

enum insert_option { INSERT, NO_INSERT };

static sp_hashnode alloc_ident PARAMS ((const char *, size_t,
					enum insert_option));
static inline unsigned int calc_hash PARAMS ((const unsigned char *, size_t));
static void mark_string_hash PARAMS ((void *));
static void expand_string_table PARAMS ((void));

/* Convenience macro for iterating over the hash table.  E is set to
   each live entry in turn.  */
#define FORALL_IDS(E) \
for (E = string_hash.entries; E < string_hash.entries+string_hash.nslots; E++) \
  if (!SP_EMPTY (*E) && SP_VALID (*E))

/* 0 while creating built-in identifiers.  */
static int do_identifier_warnings;

/* Initialize the string pool.  */
void
init_stringpool ()
{
  gcc_obstack_init (&string_stack);
  ggc_add_root (&string_hash, 1, sizeof string_hash, mark_string_hash);

  /* Strings need no alignment.  */
  obstack_alignment_mask (&string_stack) = 0;

  string_hash.entries = (sp_hashnode *)
    xcalloc (string_hash.nslots, sizeof (sp_hashnode));
}

/* Enable warnings on similar identifiers (if requested).
   Done after the built-in identifiers are created.  */
void
start_identifier_warnings ()
{
  do_identifier_warnings = 1;
}

/* Record the size of an identifier node for the language in use.
   SIZE is the total size in bytes.
   This is called by the language-specific files.  This must be
   called before allocating any identifiers.  */
void
set_identifier_size (size)
     int size;
{
  tree_code_length[(int) IDENTIFIER_NODE]
    = (size - sizeof (struct tree_common)) / sizeof (tree);
}

/* Calculate the hash of the string STR, which is of length LEN.  */
static inline unsigned int
calc_hash (str, len)
     const unsigned char *str;
     size_t len;
{
  size_t n = len;
  unsigned int r = 0;
#define HASHSTEP(r, c) ((r) * 67 + (c - 113));

  while (n--)
    r = HASHSTEP (r, *str++);

  return r + len;
#undef HASHSTEP
}

/* Internal primitive: returns the header structure for the identifier
   of length LENGTH, containing CONTENTS.  If that identifier already
   exists in the table, returns the existing entry.  If the identifier
   hasn't been seen before and the last argument is INSERT, inserts
   and returns a new entry. Otherwise returns NULL.  */
static sp_hashnode
alloc_ident (contents, length, insert)
     const char *contents;
     size_t length;
     enum insert_option insert;
{
  unsigned int hash = calc_hash ((const unsigned char *)contents, length);
  unsigned int hash2;
  unsigned int index;
  size_t sizemask;
  sp_hashnode entry;

  sizemask = string_hash.nslots - 1;
  index = hash & sizemask;

  /* hash2 must be odd, so we're guaranteed to visit every possible
     location in the table during rehashing.  */
  hash2 = ((hash * 17) & sizemask) | 1;
  string_hash.searches++;

  for (;;)
    {
      entry = string_hash.entries[index];

      if (SP_EMPTY (entry))
	break;

      if ((size_t) SP_LEN (entry) == length
	  && !memcmp (SP_STR (entry), contents, length))
	return entry;

      index = (index + hash2) & sizemask;
      string_hash.collisions++;
    }

  if (insert == NO_INSERT)
    return NULL;

  entry = (sp_hashnode) make_node (IDENTIFIER_NODE);
  string_hash.entries[index] = entry;
  SP_STR (entry) = ggc_alloc_string (contents, length);
  SP_LEN (entry) = length;
  /* This is not yet an identifier.  */
  TREE_SET_CODE (entry, ERROR_MARK);

  if (++string_hash.nelements * 4 >= string_hash.nslots * 3)
    /* Must expand the string table.  */
    expand_string_table ();

  return entry;
}

/* Subroutine of alloc_ident which doubles the size of the hash table
   and rehashes all the strings into the new table.  Returns the entry
   in the new table corresponding to ENTRY.  */
static void
expand_string_table ()
{
  sp_hashnode *nentries;
  sp_hashnode *e;
  size_t size, sizemask;

  size = string_hash.nslots * 2;
  nentries = (sp_hashnode *) xcalloc (size, sizeof (sp_hashnode));
  sizemask = size - 1;

  FORALL_IDS (e)
    {
      unsigned int index, hash, hash2;

      hash = calc_hash ((const unsigned char *) SP_STR (*e), SP_LEN (*e));
      hash2 = ((hash * 17) & sizemask) | 1;
      index = hash & sizemask;

      for (;;)
	{
	  if (SP_EMPTY (nentries[index]))
	    {
	      nentries[index] = *e;
	      break;
	    }

	  index = (index + hash2) & sizemask;
	}
    }

  free (string_hash.entries);
  string_hash.entries = nentries;
  string_hash.nslots = size;
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
  if (length == 1 && contents[0] >= '0' && contents[0] <= '9')
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
  sp_hashnode node;
  size_t length = strlen (text);

  node = alloc_ident (text, length, INSERT);
  if (!SP_VALID (node))
    {
      /* If this identifier is longer than the clash-warning length,
	 do a brute force search of the entire table for clashes.  */
      if (warn_id_clash && do_identifier_warnings && length >= (size_t) id_clash_len)
	{
	  sp_hashnode *e;
	  FORALL_IDS (e)
	    {
	      if (SP_LEN (*e) >= id_clash_len
		  && !strncmp (SP_STR (*e), text, id_clash_len))
		{
		  warning ("\"%s\" and \"%s\" identical in first %d characters",
			   text, SP_STR (*e), id_clash_len);
		  break;
		}
	    }
	}

      TREE_SET_CODE (node, IDENTIFIER_NODE);
#ifdef GATHER_STATISTICS
      id_string_size += length;
#endif
    }

  return SP_TREE (node);
}

/* If an identifier with the name TEXT (a null-terminated string) has
   previously been referred to, return that node; otherwise return
   NULL_TREE.  */

tree
maybe_get_identifier (text)
     const char *text;
{
  sp_hashnode node;
  size_t length = strlen (text);

  node = alloc_ident (text, length, NO_INSERT);
  if (!SP_EMPTY (node) && SP_VALID (node))
    return SP_TREE (node);

  return NULL_TREE;
}

/* Report some basic statistics about the string pool.  */

void
stringpool_statistics ()
{
  size_t nelts, nids, overhead, headers;
  size_t total_bytes, longest, sum_of_squares;
  double exp_len, exp_len2, exp2_len;
  sp_hashnode *e;
#define SCALE(x) ((unsigned long) ((x) < 1024*10 \
		  ? (x) \
		  : ((x) < 1024*1024*10 \
		     ? (x) / 1024 \
		     : (x) / (1024*1024))))
#define LABEL(x) ((x) < 1024*10 ? ' ' : ((x) < 1024*1024*10 ? 'k' : 'M'))

  total_bytes = longest = sum_of_squares = nids = 0;
  FORALL_IDS (e)
    {
      size_t n = SP_LEN (*e);

      total_bytes += n;
      sum_of_squares += n*n;
      if (n > longest)
	longest = n;
      if (SP_VALID (*e))
	nids++;
    }
      
  nelts = string_hash.nelements;
  overhead = obstack_memory_used (&string_stack) - total_bytes;
  headers = string_hash.nslots * sizeof (sp_hashnode);

  fprintf (stderr,
"\nString pool\n\
entries\t\t%lu\n\
identifiers\t%lu (%.2f%%)\n\
slots\t\t%lu\n\
bytes\t\t%lu%c (%lu%c overhead)\n\
table size\t%lu%c\n",
	   (unsigned long) nelts,
	   (unsigned long) nids, nids * 100.0 / nelts,
	   (unsigned long) string_hash.nslots,
	   SCALE (total_bytes), LABEL (total_bytes),
	   SCALE (overhead), LABEL (overhead),
	   SCALE (headers), LABEL (headers));

  exp_len = (double)total_bytes / (double)nelts;
  exp2_len = exp_len * exp_len;
  exp_len2 = (double)sum_of_squares / (double)nelts;

  fprintf (stderr,
"coll/search\t%.4f\n\
ins/search\t%.4f\n\
avg. entry\t%.2f bytes (+/- %.2f)\n\
longest entry\t%lu\n",
	   (double) string_hash.collisions / (double) string_hash.searches,
	   (double) nelts / (double) string_hash.searches,
	   exp_len, approx_sqrt (exp_len2 - exp2_len),
	   (unsigned long) longest);
#undef SCALE
#undef LABEL
}

/* Mark the string hash for GC.  */

static void
mark_string_hash (arg)
     void *arg ATTRIBUTE_UNUSED;
{
  sp_hashnode *h;

  FORALL_IDS (h)
    {
      ggc_mark_tree (SP_TREE (*h));
    }
}
