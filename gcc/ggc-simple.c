/* Simple garbage collection for the GNU compiler.
   Copyright (C) 1998 Free Software Foundation, Inc.

   This file is part of GNU CC.

   GNU CC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GNU CC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tree.h"
#include "ggc.h"
#include "flags.h"
#include "varray.h"
#include "hash.h"

/* Debugging flags.  */

/* Zap memory before freeing to catch dangling pointers.  */
#define GGC_POISON

/* Log alloc and release.  Don't enable this unless you want a
   really really lot of data.  */
#undef GGC_DUMP

/* Some magic tags for strings and anonymous memory, hoping to catch
   certain errors wrt marking memory.  */

#define IS_MARKED(X)		((X) & 1)
#define IGNORE_MARK(X)		((X) & -2)

#define GGC_STRING_MAGIC	((unsigned int)0xa1b2c3d4)
#define GGC_STRING_MAGIC_MARK	((unsigned int)0xa1b2c3d4 | 1)

#define GGC_ANY_MAGIC		((unsigned int)0xa9bacbdc)
#define GGC_ANY_MAGIC_MARK	((unsigned int)0xa9bacbdc | 1)

/* Global lists of roots, rtxs, and trees.  */

struct ggc_root
{
  struct ggc_root *next;
  void *base;
  int nelt;
  int size;
  void (*cb) PROTO ((void *));
};

static struct ggc_root *roots;

struct ggc_rtx
{
  struct ggc_rtx *chain;
  struct rtx_def rtx;
};

struct ggc_rtvec
{
  struct ggc_rtvec *chain;
  struct rtvec_def vec;
};

struct ggc_tree
{
  struct ggc_tree *chain;
  union tree_node tree;
};

struct ggc_string
{
  struct ggc_string *chain;
  unsigned int magic_mark;
  char string[1];
};

/* A generic allocation, with an external mark bit.  */

struct ggc_any
{
  struct ggc_any *chain;
  unsigned int magic_mark;

  /* Make sure the data is reasonably aligned.  */
  union {
    char c;
    HOST_WIDE_INT i;
    long double d;
  } u;
};

struct ggc_status
{
  struct ggc_status *next;
  struct ggc_rtx *rtxs;
  struct ggc_rtvec *vecs;
  struct ggc_tree *trees;
  struct ggc_string *strings;
  struct ggc_any *anys;
  size_t bytes_alloced_since_gc;
};

/* A chain of GGC contexts.  The currently active context is at the
   front of the chain.  */
static struct ggc_status *ggc_chain;

/* Some statistics.  */

static int n_rtxs_collected;
static int n_vecs_collected;
static int n_trees_collected;
static int n_strings_collected;
static int n_anys_collected;
extern int gc_time;

#ifdef GGC_DUMP
static FILE *dump;
#endif

/* Local function prototypes.  */

static void ggc_free_rtx PROTO ((struct ggc_rtx *r));
static void ggc_free_rtvec PROTO ((struct ggc_rtvec *v));
static void ggc_free_tree PROTO ((struct ggc_tree *t));
static void ggc_free_string PROTO ((struct ggc_string *s));
static void ggc_free_any PROTO ((struct ggc_any *a));

static void ggc_mark_rtx_ptr PROTO ((void *elt));
static void ggc_mark_tree_ptr PROTO ((void *elt));
static void ggc_mark_string_ptr PROTO ((void *elt));
static void ggc_mark_tree_varray_ptr PROTO ((void *elt));
static void ggc_mark_tree_hash_table_ptr PROTO ((void *elt));
static boolean ggc_mark_tree_hash_table_entry PROTO ((struct hash_entry *,
						      hash_table_key));

/* Called once to initialize the garbage collector.  */

void 
init_ggc PROTO ((void))
{
  /* Initialize the global context.  */
  ggc_push_context ();

#ifdef GGC_DUMP
  dump = fopen ("zgcdump", "w");
  setlinebuf (dump);
#endif
}

/* Start a new GGC context.  Memory allocated in previous contexts
   will not be collected while the new context is active.  */

void
ggc_push_context PROTO ((void))
{
  struct ggc_status *gs = (struct ggc_status *) xcalloc (1, sizeof (*gs));
  gs->next = ggc_chain;
  ggc_chain = gs;
}

/* Finish a GC context.  Any uncollected memory in the new context
   will be merged with the old context.  */

void 
ggc_pop_context PROTO ((void))
{
  struct ggc_rtx *r;
  struct ggc_rtvec *v;
  struct ggc_tree *t;
  struct ggc_string *s;
  struct ggc_status *gs;

  gs = ggc_chain;

  r = gs->rtxs;
  if (r)
    {
      while (r->chain)
	r = r->chain;
      r->chain = gs->next->rtxs;
      gs->next->rtxs = gs->rtxs;
    }
      
  v = gs->vecs;
  if (v)
    {
      while (v->chain)
	v = v->chain;
      v->chain = gs->next->vecs;
      gs->next->vecs = gs->vecs;
    }

  t = gs->trees;
  if (t)
    {
      while (t->chain)
	t = t->chain;
      t->chain = gs->next->trees;
      gs->next->trees = gs->trees;
    }

  s = gs->strings;
  if (s)
    {
      while (s->chain)
	s = s->chain;
      s->chain = gs->next->strings;
      gs->next->strings = gs->strings;
    }

  ggc_chain = gs->next;
  free (gs);
}

/* These allocators are dreadfully simple, with no caching whatsoever so
   that Purify-like tools that do allocation versioning can catch errors.
   This collector is never going to go fast anyway.  */

rtx
ggc_alloc_rtx (nslots)
     int nslots;
{
  struct ggc_rtx *n;
  int size = sizeof(*n) + (nslots-1) * sizeof(rtunion);

  n = (struct ggc_rtx *) xcalloc (1, size);
  n->chain = ggc_chain->rtxs;
  ggc_chain->rtxs = n;

#ifdef GGC_DUMP
  fprintf (dump, "alloc rtx %p\n", &n->rtx);
#endif

  ggc_chain->bytes_alloced_since_gc += size;

  return &n->rtx;
}

rtvec
ggc_alloc_rtvec (nelt)
     int nelt;
{
  struct ggc_rtvec *v;
  int size = sizeof (*v) + (nelt - 1) * sizeof (rtx);

  v = (struct ggc_rtvec *) xcalloc (1, size);
  v->chain = ggc_chain->vecs;
  ggc_chain->vecs = v;

#ifdef GGC_DUMP
  fprintf(dump, "alloc vec %p\n", &v->vec);
#endif

  ggc_chain->bytes_alloced_since_gc += size;

  return &v->vec;
}

tree
ggc_alloc_tree (length)
     int length;
{
  struct ggc_tree *n;
  int size = sizeof(*n) - sizeof(n->tree) + length;

  n = (struct ggc_tree *) xcalloc (1, size);
  n->chain = ggc_chain->trees;
  ggc_chain->trees = n;

#ifdef GGC_DUMP
  fprintf(dump, "alloc tree %p\n", &n->tree);
#endif

  ggc_chain->bytes_alloced_since_gc += size;

  return &n->tree;
}

char *
ggc_alloc_string (contents, length)
     const char *contents;
     int length;
{
  struct ggc_string *s;
  int size;

  if (length < 0)
    {
      if (contents == NULL)
	return NULL;
      length = strlen (contents);
    }

  size = (s->string - (char *)s) + length + 1;
  s = (struct ggc_string *) xmalloc (size);
  s->chain = ggc_chain->strings;
  s->magic_mark = GGC_STRING_MAGIC;
  ggc_chain->strings = s;

  if (contents)
    memcpy (s->string, contents, length);
  s->string[length] = 0;

#ifdef GGC_DUMP
  fprintf(dump, "alloc string %p\n", &s->string);
#endif

  ggc_chain->bytes_alloced_since_gc += size;

  return s->string;
}

/* Like xmalloc, but allocates GC-able memory.  */

void *
ggc_alloc (bytes)
     size_t bytes;
{
  struct ggc_any *a;

  if (bytes == 0)
    bytes = 1;
  bytes += (&((struct ggc_any *) 0)->u.c - (char *) 0);

  a = (struct ggc_any *) xmalloc (bytes);
  a->chain = ggc_chain->anys;
  a->magic_mark = GGC_ANY_MAGIC;
  ggc_chain->anys = a;

  ggc_chain->bytes_alloced_since_gc += bytes;

  return &a->u;
}

/* Freeing a bit of rtl is as simple as calling free.  */

static inline void 
ggc_free_rtx (r)
     struct ggc_rtx *r;
{
#ifdef GGC_DUMP
  fprintf (dump, "collect rtx %p\n", &r->rtx);
#endif
#ifdef GGC_POISON
  memset (r, 0xAA, sizeof(*r) + ((GET_RTX_LENGTH (r->rtx.code) -1)
				 * sizeof(rtunion)));
#endif

  free (r);
}

/* Freeing an rtvec is as simple as calling free.  */

static inline void
ggc_free_rtvec (v)
     struct ggc_rtvec *v;
{
#ifdef GGC_DUMP
  fprintf(dump, "collect vec %p\n", &v->vec);
#endif
#ifdef GGC_POISON
  memset (v, 0xBB, sizeof (*v) + ((GET_NUM_ELEM (&v->vec) - 1)
				  * sizeof (rtunion)));
#endif

  free (v);
}

/* Freeing a tree node is almost, but not quite, as simple as calling free.
   Mostly we need to let the language clean up its lang_specific bits.  */

static inline void
ggc_free_tree (t)
     struct ggc_tree *t;
{
  switch (TREE_CODE_CLASS (TREE_CODE (&t->tree)))
    {
    case 'd': /* A decl node.  */
    case 't': /* A type node.  */
      lang_cleanup_tree (&t->tree);
      break;
    }

#ifdef GGC_DUMP
  fprintf (dump, "collect tree %p\n", &t->tree);
#endif
#ifdef GGC_POISON
  memset(&t->tree.common, 0xCC, sizeof(t->tree.common));
#endif

  free (t);
}

/* Freeing a string is as simple as calling free.  */

static inline void
ggc_free_string (s)
     struct ggc_string *s;
{
#ifdef GGC_DUMP
  fprintf(dump, "collect string %p\n", s->string);
#endif
#ifdef GGC_POISON
  s->magic_mark = 0xDDDDDDDD;
  s->string[0] = 0xDD;
#endif

  free (s);
}

/* Freeing anonymous memory is as simple as calling free.  */

static inline void
ggc_free_any (a)
     struct ggc_any *a;
{
#ifdef GGC_DUMP
  fprintf(dump, "collect mem %p\n", &a->u);
#endif
#ifdef GGC_POISON
  a->magic_mark = 0xEEEEEEEE;
#endif

  free (a);
}

/* Mark a node.  */

void
ggc_mark_rtx (r)
     rtx r;
{
  const char *fmt;
  int i;

  if (r == NULL_RTX || r->gc_mark)
    return;
  r->gc_mark = 1;

  /* ??? If (some of) these are really pass-dependant info, do we have
     any right poking our noses in?  */
  switch (GET_CODE (r))
    {
    case JUMP_INSN:
      ggc_mark_rtx (JUMP_LABEL (r));
      break;
    case CODE_LABEL:
      ggc_mark_rtx (LABEL_REFS (r));
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
      switch (*fmt)
	{
	case 'e': case 'u':
	  ggc_mark_rtx (XEXP (r, i));
	  break;
	case 'V': case 'E':
	  ggc_mark_rtvec (XVEC (r, i));
	  break;
	case 'S': case 's':
	  ggc_mark_string (XSTR (r, i));
	  break;
	}
    }
}

void
ggc_mark_rtvec (v)
     rtvec v;
{
  int i;

  if (v == NULL || v->gc_mark)
    return;
  v->gc_mark = 1;

  i = GET_NUM_ELEM (v);
  while (--i >= 0)
    ggc_mark_rtx (RTVEC_ELT (v, i));
}

void
ggc_mark_tree (t)
     tree t;
{
  if (t == NULL_TREE || t->common.gc_mark)
    return;
  t->common.gc_mark = 1;

  /* Bits from common.  */
  ggc_mark_tree (TREE_TYPE (t));
  ggc_mark_tree (TREE_CHAIN (t));

  /* Some nodes require special handling.  */
  switch (TREE_CODE (t))
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

    case IDENTIFIER_NODE:
      ggc_mark_string (IDENTIFIER_POINTER (t));
      lang_mark_tree (t);
      return;

    default:
      break;
    }
  
  /* But in general we can handle them by class.  */
  switch (TREE_CODE_CLASS (TREE_CODE (t)))
    {
    case 'd': /* A decl node.  */
      ggc_mark_tree (DECL_SIZE (t));
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
      ggc_mark_tree (BLOCK_TYPE_TAGS (t));
      ggc_mark_tree (BLOCK_SUBBLOCKS (t));
      ggc_mark_tree (BLOCK_SUPERCONTEXT (t));
      ggc_mark_tree (BLOCK_ABSTRACT_ORIGIN (t));
      ggc_mark_rtx (BLOCK_END_NOTE (t));
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

void
ggc_mark_string (s)
     char *s;
{
  const ptrdiff_t d = (((struct ggc_string *) 0)->string - (char *) 0);
  struct ggc_string *gs;

  if (s == NULL)
    return;

  gs = (struct ggc_string *)(s - d);
  if (IGNORE_MARK (gs->magic_mark) != GGC_STRING_MAGIC)
    return;   /* abort? */
  gs->magic_mark = GGC_STRING_MAGIC_MARK;
}

/* Mark P, allocated with ggc_alloc.  */

void
ggc_mark (p)
     void *p;
{
  const ptrdiff_t d = (&((struct ggc_any *) 0)->u.c - (char *) 0);
  struct ggc_any *a;

  if (p == NULL)
    return;

  a = (struct ggc_any *) (((char*) p) - d);
  if (IGNORE_MARK (a->magic_mark) != GGC_ANY_MAGIC)
    abort ();
  a->magic_mark = GGC_ANY_MAGIC_MARK;
}

/* The top level mark-and-sweep routine.  */

void
ggc_collect ()
{
  struct ggc_rtx *r, **rp;
  struct ggc_rtvec *v, **vp;
  struct ggc_tree *t, **tp;
  struct ggc_string *s, **sp;
  struct ggc_root *x;
  struct ggc_status *gs;
  struct ggc_any *a, **ap;
  int time, n_rtxs, n_trees, n_vecs, n_strings, n_anys;

#ifndef ENABLE_CHECKING
  /* See if it's even worth our while.  */
  if (ggc_chain->bytes_alloced_since_gc < 64*1024)
    return;
#endif

  if (!quiet_flag)
    fputs (" {GC ", stderr);

  time = get_run_time ();

  /* Clean out all of the GC marks.  */
  for (gs = ggc_chain; gs; gs = gs->next)
    {
      for (r = gs->rtxs; r != NULL; r = r->chain)
	r->rtx.gc_mark = 0;
      for (v = gs->vecs; v != NULL; v = v->chain)
	v->vec.gc_mark = 0;
      for (t = gs->trees; t != NULL; t = t->chain)
	t->tree.common.gc_mark = 0;
      for (s = gs->strings; s != NULL; s = s->chain)
	s->magic_mark = GGC_STRING_MAGIC;
      for (a = gs->anys; a != NULL; a = a->chain)
	a->magic_mark = GGC_ANY_MAGIC;
    }

  /* Mark through all the roots.  */
  for (x = roots; x != NULL; x = x->next)
    {
      char *elt = x->base;
      int s = x->size, n = x->nelt;
      void (*cb) PROTO ((void *)) = x->cb;
      int i;

      for (i = 0; i < n; ++i, elt += s)
	(*cb)(elt);
    }

  /* Sweep the resulting dead nodes.  */

  /* The RTXs.  */

  rp = &ggc_chain->rtxs;
  r = ggc_chain->rtxs;
  n_rtxs = 0;
  while (r != NULL)
    {
      struct ggc_rtx *chain = r->chain;
      if (!r->rtx.gc_mark)
        {
	  ggc_free_rtx (r);
	  *rp = chain;
	  n_rtxs++;
        }
      else
	rp = &r->chain;
      r = chain;
    }
  *rp = NULL;
  n_rtxs_collected += n_rtxs;

  /* The vectors.  */

  vp = &ggc_chain->vecs;
  v = ggc_chain->vecs;
  n_vecs = 0;
  while (v != NULL)
    {
      struct ggc_rtvec *chain = v->chain;
      if (!v->vec.gc_mark)
        {
	  ggc_free_rtvec (v);
	  *vp = chain;
	  n_vecs++;
        }
      else
	vp = &v->chain;
      v = chain;
    }
  *vp = NULL;
  n_vecs_collected += n_vecs;

  /* The trees.  */

  tp = &ggc_chain->trees;
  t = ggc_chain->trees;
  n_trees = 0;
  while (t != NULL)
    {
      struct ggc_tree *chain = t->chain;
      if (!t->tree.common.gc_mark)
        {
	  ggc_free_tree (t);
	  *tp = chain;
	  n_trees++;
        }
      else
	tp = &t->chain;
      t = chain;
    }
  *tp = NULL;
  n_trees_collected += n_trees;

  /* The strings.  */

  sp = &ggc_chain->strings;
  s = ggc_chain->strings;
  n_strings = 0;
  while (s != NULL)
    {
      struct ggc_string *chain = s->chain;
      if (! IS_MARKED (s->magic_mark))
        {
	  ggc_free_string (s);
	  *sp = chain;
	  n_strings++;
        }
      else
	sp = &s->chain;
      s = chain;
    }
  *sp = NULL;
  n_strings_collected += n_strings;

  /* The generic data.  */

  ap = &ggc_chain->anys;
  a = ggc_chain->anys;
  n_anys = 0;
  while (a != NULL)
    {
      struct ggc_any *chain = a->chain;
      if (! IS_MARKED (a->magic_mark))
	{
	  ggc_free_any (a);
	  *ap = chain;
	  n_anys++;
	}
      else
	ap = &a->chain;
      a = chain;
    }
  n_anys_collected += n_anys;

  ggc_chain->bytes_alloced_since_gc = 0;

  time = get_run_time () - time;
  gc_time += time;

  if (!quiet_flag)
    {
      time = (time + 500) / 1000;
      fprintf (stderr, "%dr,%dv,%dt,%ds,%da %d.%03d}", n_rtxs, n_vecs, 
	       n_trees, n_strings, n_anys, time / 1000, time % 1000);
    }
}

/* Manipulate global roots that are needed between calls to gc.  */

void
ggc_add_root (base, nelt, size, cb)
     void *base;
     int nelt, size;
     void (*cb) PROTO ((void *));
{
  struct ggc_root *x = (struct ggc_root *) xmalloc (sizeof(*x));

  x->next = roots;
  x->base = base;
  x->nelt = nelt;
  x->size = size;
  x->cb = cb;

  roots = x;
}

void
ggc_add_rtx_root (base, nelt)
     rtx *base;
     int nelt;
{
  ggc_add_root (base, nelt, sizeof(rtx), ggc_mark_rtx_ptr);
}

void
ggc_add_tree_root (base, nelt)
     tree *base;
     int nelt;
{
  ggc_add_root (base, nelt, sizeof(tree), ggc_mark_tree_ptr);
}

void
ggc_add_string_root (base, nelt)
     char **base;
     int nelt;
{
  ggc_add_root (base, nelt, sizeof(char *), ggc_mark_string_ptr);
}

/* Add V (a varray full of trees) to the list of GC roots.  */

void
ggc_add_tree_varray_root (base, nelt)
     varray_type *base;
     int nelt;
{
  ggc_add_root (base, nelt, sizeof (varray_type), 
		ggc_mark_tree_varray_ptr);
}

/* Add HT (a hash-table where ever key is a tree) to the list of GC
   roots.  */

void
ggc_add_tree_hash_table_root (base, nelt)
     struct hash_table **base;
     int nelt;
{
  ggc_add_root (base, nelt, sizeof (struct hash_table *), 
		ggc_mark_tree_hash_table_ptr);
}

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

static void
ggc_mark_rtx_ptr (elt)
     void *elt;
{
  ggc_mark_rtx (*(rtx *)elt);
}

static void
ggc_mark_tree_ptr (elt)
     void *elt;
{
  ggc_mark_tree (*(tree *)elt);
}

/* Type-correct function to pass to ggc_add_root.  It just forwards
   ELT (which is really a char **) to ggc_mark_string.  */

static void
ggc_mark_string_ptr (elt)
     void *elt;
{
  ggc_mark_string (*(char **)elt);
}

/* Type-correct function to pass to ggc_add_root.  It just forwards
   ELT (which is really a varray_type *) to ggc_mark_tree_varray.  */

static void
ggc_mark_tree_varray_ptr (elt)
     void *elt;
{
  ggc_mark_tree_varray (*(varray_type *)elt);
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

#if 0
/* GDB really should have a memory search function.  Since this is just
   for initial debugging, I won't even pretend to get the __data_start
   to work on any but alpha-dec-linux-gnu.  */
static void **
search_data(void **start, void *target)
{
  extern void *__data_start[];
  void **_end = (void **)sbrk(0);

  if (start == NULL)
    start = __data_start;
  while (start < _end)
    {
      if (*start == target)
        return start;
      start++;
    }
  return NULL;
}
#endif
