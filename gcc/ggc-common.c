/* Simple garbage collection for the GNU compiler.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

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

/* Generic garbage collection (GC) functions and data, not specific to
   any particular GC implementation.  */

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tree.h"
#include "tm_p.h"
#include "hash.h"
#include "varray.h"
#include "ggc.h"

/* Statistics about the allocation.  */
static ggc_statistics *ggc_stats;

static void ggc_mark_rtx_ptr PARAMS ((void *));
static void ggc_mark_tree_ptr PARAMS ((void *));
static void ggc_mark_rtx_varray_ptr PARAMS ((void *));
static void ggc_mark_tree_varray_ptr PARAMS ((void *));
static void ggc_mark_tree_hash_table_ptr PARAMS ((void *));
static void ggc_mark_string_ptr PARAMS ((void *));
static boolean ggc_mark_tree_hash_table_entry PARAMS ((struct hash_entry *,
						       hash_table_key));

/* Maintain global roots that are preserved during GC.  */

/* Global roots that are preserved during calls to gc.  */

struct ggc_root
{
  struct ggc_root *next;
  void *base;
  int nelt;
  int size;
  void (*cb) PARAMS ((void *));
};

static struct ggc_root *roots;

/* Add BASE as a new garbage collection root.  It is an array of
   length NELT with each element SIZE bytes long.  CB is a 
   function that will be called with a pointer to each element
   of the array; it is the intention that CB call the appropriate
   routine to mark gc-able memory for that element.  */

void
ggc_add_root (base, nelt, size, cb)
     void *base;
     int nelt, size;
     void (*cb) PARAMS ((void *));
{
  struct ggc_root *x = (struct ggc_root *) xmalloc (sizeof (*x));

  x->next = roots;
  x->base = base;
  x->nelt = nelt;
  x->size = size;
  x->cb = cb;

  roots = x;
}

/* Register an array of rtx as a GC root.  */

void
ggc_add_rtx_root (base, nelt)
     rtx *base;
     int nelt;
{
  ggc_add_root (base, nelt, sizeof(rtx), ggc_mark_rtx_ptr);
}

/* Register an array of trees as a GC root.  */

void
ggc_add_tree_root (base, nelt)
     tree *base;
     int nelt;
{
  ggc_add_root (base, nelt, sizeof(tree), ggc_mark_tree_ptr);
}

/* Register a varray of rtxs as a GC root.  */

void
ggc_add_rtx_varray_root (base, nelt)
     varray_type *base;
     int nelt;
{
  ggc_add_root (base, nelt, sizeof (varray_type), 
		ggc_mark_rtx_varray_ptr);
}

/* Register a varray of trees as a GC root.  */

void
ggc_add_tree_varray_root (base, nelt)
     varray_type *base;
     int nelt;
{
  ggc_add_root (base, nelt, sizeof (varray_type), 
		ggc_mark_tree_varray_ptr);
}

/* Register a hash table of trees as a GC root.  */

void
ggc_add_tree_hash_table_root (base, nelt)
     struct hash_table **base;
     int nelt;
{
  ggc_add_root (base, nelt, sizeof (struct hash_table *), 
		ggc_mark_tree_hash_table_ptr);
}

/* Register an array of strings as a GC root.  */

void
ggc_add_string_root (base, nelt)
     char **base;
     int nelt;
{
  ggc_add_root (base, nelt, sizeof (char *), ggc_mark_string_ptr);
}

/* Remove the previously registered GC root at BASE.  */

void
ggc_del_root (base)
     void *base;
{
  struct ggc_root *x, **p;

  p = &roots, x = roots;
  while (x)
    {
      if (x->base == base)
	{
	  *p = x->next;
	  free (x);
	  return;
	}
      p = &x->next;
      x = x->next;
    }

  abort();
}

/* Iterate through all registered roots and mark each element.  */

void
ggc_mark_roots ()
{
  struct ggc_root* x;
  
  for (x = roots; x != NULL; x = x->next)
    {
      char *elt = x->base;
      int s = x->size, n = x->nelt;
      void (*cb) PARAMS ((void *)) = x->cb;
      int i;

      for (i = 0; i < n; ++i, elt += s)
	(*cb)(elt);
    }
}

/* R had not been previously marked, but has now been marked via
   ggc_set_mark.  Now recurse and process the children.  */

void
ggc_mark_rtx_children (r)
     rtx r;
{
  const char *fmt;
  int i;
  rtx next_rtx;

  do 
    {
      enum rtx_code code = GET_CODE (r);
      /* This gets set to a child rtx to eliminate tail recursion.  */
      next_rtx = NULL;

      /* Collect statistics, if appropriate.  */
      if (ggc_stats)
	{
	  ++ggc_stats->num_rtxs[(int) code];
	  ggc_stats->size_rtxs[(int) code] += ggc_get_size (r);
	}

      /* ??? If (some of) these are really pass-dependant info, do we
	 have any right poking our noses in?  */
      switch (code)
	{
	case JUMP_INSN:
	  ggc_mark_rtx (JUMP_LABEL (r));
	  break;
	case CODE_LABEL:
	  ggc_mark_rtx (LABEL_REFS (r));
	  ggc_mark_string (LABEL_ALTERNATE_NAME (r));
	  break;
	case LABEL_REF:
	  ggc_mark_rtx (LABEL_NEXTREF (r));
	  ggc_mark_rtx (CONTAINING_INSN (r));
	  break;
	case ADDRESSOF:
	  ggc_mark_tree (ADDRESSOF_DECL (r));
	  break;
	case CONST_DOUBLE:
	  ggc_mark_rtx (CONST_DOUBLE_CHAIN (r));
	  break;
	case NOTE:
	  switch (NOTE_LINE_NUMBER (r))
	    {
	    case NOTE_INSN_RANGE_START:
	    case NOTE_INSN_RANGE_END:
	    case NOTE_INSN_LIVE:
	      ggc_mark_rtx (NOTE_RANGE_INFO (r));
	      break;

	    case NOTE_INSN_BLOCK_BEG:
	    case NOTE_INSN_BLOCK_END:
	      ggc_mark_tree (NOTE_BLOCK (r));
	      break;

	    default:
	      if (NOTE_LINE_NUMBER (r) >= 0)
		ggc_mark_string (NOTE_SOURCE_FILE (r));
	      break;
	    }
	  break;

	default:
	  break;
	}

      for (fmt = GET_RTX_FORMAT (GET_CODE (r)), i = 0; *fmt ; ++fmt, ++i)
	{
	  rtx exp;
	  switch (*fmt)
	    {
	    case 'e': case 'u':
	      exp = XEXP (r, i);
	      if (ggc_test_and_set_mark (exp))
		{ 
		  if (next_rtx == NULL) 
		    next_rtx = exp; 
		  else 
		    ggc_mark_rtx_children (exp);
		} 
	      break;
	    case 'V': case 'E':
	      ggc_mark_rtvec (XVEC (r, i));
	      break;
	    case 'S': case 's':
	      ggc_mark_if_gcable (XSTR (r, i));
	      break;
	    }
	}
    }
  while ((r = next_rtx) != NULL);
}

/* V had not been previously marked, but has now been marked via
   ggc_set_mark.  Now recurse and process the children.  */

void
ggc_mark_rtvec_children (v)
     rtvec v;
{
  int i;

  i = GET_NUM_ELEM (v);
  while (--i >= 0)
    ggc_mark_rtx (RTVEC_ELT (v, i));
}

/* T had not been previously marked, but has now been marked via
   ggc_set_mark.  Now recurse and process the children.  */

void
ggc_mark_tree_children (t)
     tree t;
{
  enum tree_code code = TREE_CODE (t);

  /* Collect statistics, if appropriate.  */
  if (ggc_stats)
    {
      ++ggc_stats->num_trees[(int) code];
      ggc_stats->size_trees[(int) code] += ggc_get_size (t);
    }

  /* Bits from common.  */
  ggc_mark_tree (TREE_TYPE (t));
  ggc_mark_tree (TREE_CHAIN (t));

  /* Some nodes require special handling.  */
  switch (code)
    {
    case TREE_LIST:
      ggc_mark_tree (TREE_PURPOSE (t));
      ggc_mark_tree (TREE_VALUE (t));
      return;

    case TREE_VEC:
      {
	int i = TREE_VEC_LENGTH (t);
	while (--i >= 0)
	  ggc_mark_tree (TREE_VEC_ELT (t, i));
	return;
      }

    case SAVE_EXPR:
      ggc_mark_tree (TREE_OPERAND (t, 0));
      ggc_mark_tree (SAVE_EXPR_CONTEXT (t));
      ggc_mark_rtx (SAVE_EXPR_RTL (t));
      return;

    case RTL_EXPR:
      ggc_mark_rtx (RTL_EXPR_SEQUENCE (t));
      ggc_mark_rtx (RTL_EXPR_RTL (t));
      return;

    case CALL_EXPR:
      ggc_mark_tree (TREE_OPERAND (t, 0));
      ggc_mark_tree (TREE_OPERAND (t, 1));
      ggc_mark_rtx (CALL_EXPR_RTL (t));
      return;

    case COMPLEX_CST:
      ggc_mark_tree (TREE_REALPART (t));
      ggc_mark_tree (TREE_IMAGPART (t));
      break;

    case STRING_CST:
      ggc_mark_string (TREE_STRING_POINTER (t));
      break;

    case PARM_DECL:
      ggc_mark_rtx (DECL_INCOMING_RTL (t));
      break;

    case FIELD_DECL:
      ggc_mark_tree (DECL_FIELD_BIT_OFFSET (t));
      break;

    case IDENTIFIER_NODE:
      ggc_mark_string (IDENTIFIER_POINTER (t));
      lang_mark_tree (t);
      return;

    default:
      break;
    }
  
  /* But in general we can handle them by class.  */
  switch (TREE_CODE_CLASS (code))
    {
    case 'd': /* A decl node.  */
      ggc_mark_string (DECL_SOURCE_FILE (t));
      ggc_mark_tree (DECL_SIZE (t));
      ggc_mark_tree (DECL_SIZE_UNIT (t));
      ggc_mark_tree (DECL_NAME (t));
      ggc_mark_tree (DECL_CONTEXT (t));
      ggc_mark_tree (DECL_ARGUMENTS (t));
      ggc_mark_tree (DECL_RESULT (t));
      ggc_mark_tree (DECL_INITIAL (t));
      ggc_mark_tree (DECL_ABSTRACT_ORIGIN (t));
      ggc_mark_tree (DECL_ASSEMBLER_NAME (t));
      ggc_mark_tree (DECL_SECTION_NAME (t));
      ggc_mark_tree (DECL_MACHINE_ATTRIBUTES (t));
      ggc_mark_rtx (DECL_RTL (t));
      ggc_mark_rtx (DECL_LIVE_RANGE_RTL (t));
      ggc_mark_tree (DECL_VINDEX (t));
      lang_mark_tree (t);
      break;

    case 't': /* A type node.  */
      ggc_mark_tree (TYPE_SIZE (t));
      ggc_mark_tree (TYPE_SIZE_UNIT (t));
      ggc_mark_tree (TYPE_ATTRIBUTES (t));
      ggc_mark_tree (TYPE_VALUES (t));
      ggc_mark_tree (TYPE_POINTER_TO (t));
      ggc_mark_tree (TYPE_REFERENCE_TO (t));
      ggc_mark_tree (TYPE_NAME (t));
      ggc_mark_tree (TYPE_MIN_VALUE (t));
      ggc_mark_tree (TYPE_MAX_VALUE (t));
      ggc_mark_tree (TYPE_NEXT_VARIANT (t));
      ggc_mark_tree (TYPE_MAIN_VARIANT (t));
      ggc_mark_tree (TYPE_BINFO (t));
      ggc_mark_tree (TYPE_NONCOPIED_PARTS (t));
      ggc_mark_tree (TYPE_CONTEXT (t));
      lang_mark_tree (t);
      break;

    case 'b': /* A lexical block.  */
      ggc_mark_tree (BLOCK_VARS (t));
      ggc_mark_tree (BLOCK_SUBBLOCKS (t));
      ggc_mark_tree (BLOCK_SUPERCONTEXT (t));
      ggc_mark_tree (BLOCK_ABSTRACT_ORIGIN (t));
      break;

    case 'c': /* A constant.  */
      ggc_mark_rtx (TREE_CST_RTL (t));
      break;

    case 'r': case '<': case '1':
    case '2': case 'e': case 's': /* Expressions.  */
      {
	int i = tree_code_length[TREE_CODE (t)];
	while (--i >= 0)
	  ggc_mark_tree (TREE_OPERAND (t, i));
	break;
      }

    case 'x':
      lang_mark_tree (t);
      break;
    }
}

/* Mark all the elements of the varray V, which contains rtxs.  */

void
ggc_mark_rtx_varray (v)
     varray_type v;
{
  int i;

  if (v)
    for (i = v->num_elements - 1; i >= 0; --i) 
      ggc_mark_rtx (VARRAY_RTX (v, i));
}

/* Mark all the elements of the varray V, which contains trees.  */

void
ggc_mark_tree_varray (v)
     varray_type v;
{
  int i;

  if (v)
    for (i = v->num_elements - 1; i >= 0; --i) 
      ggc_mark_tree (VARRAY_TREE (v, i));
}

/* Mark the hash table-entry HE.  It's key field is really a tree.  */

static boolean
ggc_mark_tree_hash_table_entry (he, k)
     struct hash_entry *he;
     hash_table_key k ATTRIBUTE_UNUSED;
{
  ggc_mark_tree ((tree) he->key);
  return true;
}

/* Mark all the elements of the hash-table H, which contains trees.  */

void
ggc_mark_tree_hash_table (ht)
     struct hash_table *ht;
{
  hash_traverse (ht, ggc_mark_tree_hash_table_entry, /*info=*/0);
}

/* Type-correct function to pass to ggc_add_root.  It just forwards
   *ELT (which is an rtx) to ggc_mark_rtx.  */

static void
ggc_mark_rtx_ptr (elt)
     void *elt;
{
  ggc_mark_rtx (*(rtx *) elt);
}

/* Type-correct function to pass to ggc_add_root.  It just forwards
   *ELT (which is a tree) to ggc_mark_tree.  */

static void
ggc_mark_tree_ptr (elt)
     void *elt;
{
  ggc_mark_tree (*(tree *) elt);
}

/* Type-correct function to pass to ggc_add_root.  It just forwards
   ELT (which is really a varray_type *) to ggc_mark_rtx_varray.  */

static void
ggc_mark_rtx_varray_ptr (elt)
     void *elt;
{
  ggc_mark_rtx_varray (*(varray_type *) elt);
}

/* Type-correct function to pass to ggc_add_root.  It just forwards
   ELT (which is really a varray_type *) to ggc_mark_tree_varray.  */

static void
ggc_mark_tree_varray_ptr (elt)
     void *elt;
{
  ggc_mark_tree_varray (*(varray_type *) elt);
}

/* Type-correct function to pass to ggc_add_root.  It just forwards
   ELT (which is really a struct hash_table **) to
   ggc_mark_tree_hash_table.  */

static void
ggc_mark_tree_hash_table_ptr (elt)
     void *elt;
{
  ggc_mark_tree_hash_table (*(struct hash_table **) elt);
}

/* Type-correct function to pass to ggc_add_root.  It just forwards
   ELT (which is really a char **) to ggc_mark_string.  */

static void
ggc_mark_string_ptr (elt)
     void *elt;
{
  ggc_mark_string (*(char **) elt);
}

/* Allocate a gc-able string.  If CONTENTS is null, then the memory will
   be uninitialized.  If LENGTH is -1, then CONTENTS is assumed to be a
   null-terminated string and the memory sized accordingly.  Otherwise,
   the memory is filled with LENGTH bytes from CONTENTS.  */

char *
ggc_alloc_string (contents, length)
     const char *contents;
     int length;
{
  char *string;

  if (length < 0)
    {
      if (contents == NULL)
	return NULL;
      length = strlen (contents);
    }

  string = (char *) ggc_alloc_obj (length + 1, 0);
  if (contents != NULL)
    memcpy (string, contents, length);
  string[length] = 0;

  return string;
}

/* Print statistics that are independent of the collector in use.  */

void
ggc_print_statistics (stream, stats)
     FILE *stream;
     ggc_statistics *stats;
{
  int code;

  /* Set the pointer so that during collection we will actually gather
     the statistics.  */
  ggc_stats = stats;

  /* Then do one collection to fill in the statistics.  */
  ggc_collect ();

  /* Total the statistics.  */
  for (code = 0; code < MAX_TREE_CODES; ++code)
    {
      stats->total_num_trees += stats->num_trees[code];
      stats->total_size_trees += stats->size_trees[code];
    }
  for (code = 0; code < NUM_RTX_CODE; ++code)
    {
      stats->total_num_rtxs += stats->num_rtxs[code];
      stats->total_size_rtxs += stats->size_rtxs[code];
    }

  /* Print the statistics for trees.  */
  fprintf (stream, "%-22s%-16s%-16s%-7s\n", "Code", 
	   "Number", "Bytes", "% Total");
  for (code = 0; code < MAX_TREE_CODES; ++code)
    if (ggc_stats->num_trees[code]) 
      {
	fprintf (stream, "%s%*s%-15u %-15lu %7.3f\n", 
		 tree_code_name[code],
		 22 - (int) strlen (tree_code_name[code]), "",
		 ggc_stats->num_trees[code],
		 (unsigned long) ggc_stats->size_trees[code],
		 (100 * ((double) ggc_stats->size_trees[code]) 
		  / ggc_stats->total_size_trees));
      }
  fprintf (stream,
	   "%-22s%-15u %-15lu\n", "Total",
	   ggc_stats->total_num_trees,
	   (unsigned long) ggc_stats->total_size_trees);

  /* Print the statistics for RTL.  */
  fprintf (stream, "\n%-22s%-16s%-16s%-7s\n", "Code", 
	   "Number", "Bytes", "% Total");
  for (code = 0; code < NUM_RTX_CODE; ++code)
    if (ggc_stats->num_rtxs[code]) 
      {
	fprintf (stream, "%s%*s%-15u %-15lu %7.3f\n", 
		 rtx_name[code],
		 22 - (int) strlen (rtx_name[code]), "",
		 ggc_stats->num_rtxs[code],
		 (unsigned long) ggc_stats->size_rtxs[code],
		 (100 * ((double) ggc_stats->size_rtxs[code]) 
		  / ggc_stats->total_size_rtxs));
      }
  fprintf (stream,
	   "%-22s%-15u %-15lu\n", "Total",
	   ggc_stats->total_num_rtxs,
	   (unsigned long) ggc_stats->total_size_rtxs);


  /* Don't gather statistics any more.  */
  ggc_stats = NULL;
}
