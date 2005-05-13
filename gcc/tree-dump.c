/* Tree-dumping functionality for intermediate representation.
   Copyright (C) 1999, 2000, 2002, 2003, 2004, 2005
   Free Software Foundation, Inc.
   Written by Mark Mitchell <mark@codesourcery.com>

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "splay-tree.h"
#include "diagnostic.h"
#include "toplev.h"
#include "tree-dump.h"
#include "tree-pass.h"
#include "langhooks.h"
#include "tree-iterator.h"

static unsigned int queue (dump_info_p, tree, int);
static void dump_index (dump_info_p, unsigned int);
static void dequeue_and_dump (dump_info_p);
static void dump_new_line (dump_info_p);
static void dump_maybe_newline (dump_info_p);
static void dump_string_field (dump_info_p, const char *, const char *);
static int dump_enable_all (int, int);

/* Add T to the end of the queue of nodes to dump.  Returns the index
   assigned to T.  */

static unsigned int
queue (dump_info_p di, tree t, int flags)
{
  dump_queue_p dq;
  dump_node_info_p dni;
  unsigned int index;

  /* Assign the next available index to T.  */
  index = ++di->index;

  /* Obtain a new queue node.  */
  if (di->free_list)
    {
      dq = di->free_list;
      di->free_list = dq->next;
    }
  else
    dq = xmalloc (sizeof (struct dump_queue));

  /* Create a new entry in the splay-tree.  */
  dni = xmalloc (sizeof (struct dump_node_info));
  dni->index = index;
  dni->binfo_p = ((flags & DUMP_BINFO) != 0);
  dq->node = splay_tree_insert (di->nodes, (splay_tree_key) t,
				(splay_tree_value) dni);

  /* Add it to the end of the queue.  */
  dq->next = 0;
  if (!di->queue_end)
    di->queue = dq;
  else
    di->queue_end->next = dq;
  di->queue_end = dq;

  /* Return the index.  */
  return index;
}

static void
dump_index (dump_info_p di, unsigned int index)
{
  fprintf (di->stream, "@%-6u ", index);
  di->column += 8;
}

/* If T has not already been output, queue it for subsequent output.
   FIELD is a string to print before printing the index.  Then, the
   index of T is printed.  */

void
queue_and_dump_index (dump_info_p di, const char *field, tree t, int flags)
{
  unsigned int index;
  splay_tree_node n;

  /* If there's no node, just return.  This makes for fewer checks in
     our callers.  */
  if (!t)
    return;

  /* See if we've already queued or dumped this node.  */
  n = splay_tree_lookup (di->nodes, (splay_tree_key) t);
  if (n)
    index = ((dump_node_info_p) n->value)->index;
  else
    /* If we haven't, add it to the queue.  */
    index = queue (di, t, flags);

  /* Print the index of the node.  */
  dump_maybe_newline (di);
  fprintf (di->stream, "%-4s: ", field);
  di->column += 6;
  dump_index (di, index);
}

/* Dump the type of T.  */

void
queue_and_dump_type (dump_info_p di, tree t)
{
  queue_and_dump_index (di, "type", TREE_TYPE (t), DUMP_NONE);
}

/* Dump column control */
#define SOL_COLUMN 25		/* Start of line column.  */
#define EOL_COLUMN 55		/* End of line column.  */
#define COLUMN_ALIGNMENT 15	/* Alignment.  */

/* Insert a new line in the dump output, and indent to an appropriate
   place to start printing more fields.  */

static void
dump_new_line (dump_info_p di)
{
  fprintf (di->stream, "\n%*s", SOL_COLUMN, "");
  di->column = SOL_COLUMN;
}

/* If necessary, insert a new line.  */

static void
dump_maybe_newline (dump_info_p di)
{
  int extra;

  /* See if we need a new line.  */
  if (di->column > EOL_COLUMN)
    dump_new_line (di);
  /* See if we need any padding.  */
  else if ((extra = (di->column - SOL_COLUMN) % COLUMN_ALIGNMENT) != 0)
    {
      fprintf (di->stream, "%*s", COLUMN_ALIGNMENT - extra, "");
      di->column += COLUMN_ALIGNMENT - extra;
    }
}

/* Dump pointer PTR using FIELD to identify it.  */

void
dump_pointer (dump_info_p di, const char *field, void *ptr)
{
  dump_maybe_newline (di);
  fprintf (di->stream, "%-4s: %-8lx ", field, (long) ptr);
  di->column += 15;
}

/* Dump integer I using FIELD to identify it.  */

void
dump_int (dump_info_p di, const char *field, int i)
{
  dump_maybe_newline (di);
  fprintf (di->stream, "%-4s: %-7d ", field, i);
  di->column += 14;
}

/* Dump the string S.  */

void
dump_string (dump_info_p di, const char *string)
{
  dump_maybe_newline (di);
  fprintf (di->stream, "%-13s ", string);
  if (strlen (string) > 13)
    di->column += strlen (string) + 1;
  else
    di->column += 14;
}

/* Dump the string field S.  */

static void
dump_string_field (dump_info_p di, const char *field, const char *string)
{
  dump_maybe_newline (di);
  fprintf (di->stream, "%-4s: %-7s ", field, string);
  if (strlen (string) > 7)
    di->column += 6 + strlen (string) + 1;
  else
    di->column += 14;
}

/* Dump the next node in the queue.  */

static void
dequeue_and_dump (dump_info_p di)
{
  dump_queue_p dq;
  splay_tree_node stn;
  dump_node_info_p dni;
  tree t;
  unsigned int index;
  enum tree_code code;
  enum tree_code_class code_class;
  const char* code_name;

  /* Get the next node from the queue.  */
  dq = di->queue;
  stn = dq->node;
  t = (tree) stn->key;
  dni = (dump_node_info_p) stn->value;
  index = dni->index;

  /* Remove the node from the queue, and put it on the free list.  */
  di->queue = dq->next;
  if (!di->queue)
    di->queue_end = 0;
  dq->next = di->free_list;
  di->free_list = dq;

  /* Print the node index.  */
  dump_index (di, index);
  /* And the type of node this is.  */
  if (dni->binfo_p)
    code_name = "binfo";
  else
    code_name = tree_code_name[(int) TREE_CODE (t)];
  fprintf (di->stream, "%-16s ", code_name);
  di->column = 25;

  /* Figure out what kind of node this is.  */
  code = TREE_CODE (t);
  code_class = TREE_CODE_CLASS (code);

  /* Although BINFOs are TREE_VECs, we dump them specially so as to be
     more informative.  */
  if (dni->binfo_p)
    {
      unsigned ix;
      tree base;
      VEC(tree,gc) *accesses = BINFO_BASE_ACCESSES (t);

      dump_child ("type", BINFO_TYPE (t));

      if (BINFO_VIRTUAL_P (t))
	dump_string (di, "virt");

      dump_int (di, "bases", BINFO_N_BASE_BINFOS (t));
      for (ix = 0; BINFO_BASE_ITERATE (t, ix, base); ix++)
	{
	  tree access = (accesses ? VEC_index (tree, accesses, ix)
			 : access_public_node);
	  const char *string = NULL;

	  if (access == access_public_node)
	    string = "pub";
	  else if (access == access_protected_node)
	    string = "prot";
	  else if (access == access_private_node)
	    string = "priv";
	  else
	    gcc_unreachable ();

	  dump_string (di, string);
	  queue_and_dump_index (di, "binf", base, DUMP_BINFO);
	}

      goto done;
    }

  /* We can knock off a bunch of expression nodes in exactly the same
     way.  */
  if (IS_EXPR_CODE_CLASS (code_class))
    {
      /* If we're dumping children, dump them now.  */
      queue_and_dump_type (di, t);

      switch (code_class)
	{
	case tcc_unary:
	  dump_child ("op 0", TREE_OPERAND (t, 0));
	  break;

	case tcc_binary:
	case tcc_comparison:
	  dump_child ("op 0", TREE_OPERAND (t, 0));
	  dump_child ("op 1", TREE_OPERAND (t, 1));
	  break;

	case tcc_expression:
	case tcc_reference:
	case tcc_statement:
	  /* These nodes are handled explicitly below.  */
	  break;

	default:
	  gcc_unreachable ();
	}
    }
  else if (DECL_P (t))
    {
      expanded_location xloc;
      /* All declarations have names.  */
      if (DECL_NAME (t))
	dump_child ("name", DECL_NAME (t));
      if (DECL_ASSEMBLER_NAME_SET_P (t)
	  && DECL_ASSEMBLER_NAME (t) != DECL_NAME (t))
	dump_child ("mngl", DECL_ASSEMBLER_NAME (t));
      /* And types.  */
      queue_and_dump_type (di, t);
      dump_child ("scpe", DECL_CONTEXT (t));
      /* And a source position.  */
      xloc = expand_location (DECL_SOURCE_LOCATION (t));
      if (xloc.file)
	{
	  const char *filename = strrchr (xloc.file, '/');
	  if (!filename)
	    filename = xloc.file;
	  else
	    /* Skip the slash.  */
	    ++filename;

	  dump_maybe_newline (di);
	  fprintf (di->stream, "srcp: %s:%-6d ", filename,
		   xloc.line);
	  di->column += 6 + strlen (filename) + 8;
	}
      /* And any declaration can be compiler-generated.  */
      if (DECL_ARTIFICIAL (t))
	dump_string (di, "artificial");
      if (TREE_CHAIN (t) && !dump_flag (di, TDF_SLIM, NULL))
	dump_child ("chan", TREE_CHAIN (t));
    }
  else if (code_class == tcc_type)
    {
      /* All types have qualifiers.  */
      int quals = lang_hooks.tree_dump.type_quals (t);

      if (quals != TYPE_UNQUALIFIED)
	{
	  fprintf (di->stream, "qual: %c%c%c     ",
		   (quals & TYPE_QUAL_CONST) ? 'c' : ' ',
		   (quals & TYPE_QUAL_VOLATILE) ? 'v' : ' ',
		   (quals & TYPE_QUAL_RESTRICT) ? 'r' : ' ');
	  di->column += 14;
	}

      /* All types have associated declarations.  */
      dump_child ("name", TYPE_NAME (t));

      /* All types have a main variant.  */
      if (TYPE_MAIN_VARIANT (t) != t)
	dump_child ("unql", TYPE_MAIN_VARIANT (t));

      /* And sizes.  */
      dump_child ("size", TYPE_SIZE (t));

      /* All types have alignments.  */
      dump_int (di, "algn", TYPE_ALIGN (t));
    }
  else if (code_class == tcc_constant)
    /* All constants can have types.  */
    queue_and_dump_type (di, t);

  /* Give the language-specific code a chance to print something.  If
     it's completely taken care of things, don't bother printing
     anything more ourselves.  */
  if (lang_hooks.tree_dump.dump_tree (di, t))
    goto done;

  /* Now handle the various kinds of nodes.  */
  switch (code)
    {
      int i;

    case IDENTIFIER_NODE:
      dump_string_field (di, "strg", IDENTIFIER_POINTER (t));
      dump_int (di, "lngt", IDENTIFIER_LENGTH (t));
      break;

    case TREE_LIST:
      dump_child ("purp", TREE_PURPOSE (t));
      dump_child ("valu", TREE_VALUE (t));
      dump_child ("chan", TREE_CHAIN (t));
      break;

    case STATEMENT_LIST:
      {
	tree_stmt_iterator it;
	for (i = 0, it = tsi_start (t); !tsi_end_p (it); tsi_next (&it), i++)
	  {
	    char buffer[32];
	    sprintf (buffer, "%u", i);
	    dump_child (buffer, tsi_stmt (it));
	  }
      }
      break;

    case TREE_VEC:
      dump_int (di, "lngt", TREE_VEC_LENGTH (t));
      for (i = 0; i < TREE_VEC_LENGTH (t); ++i)
	{
	  char buffer[32];
	  sprintf (buffer, "%u", i);
	  dump_child (buffer, TREE_VEC_ELT (t, i));
	}
      break;

    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
      dump_int (di, "prec", TYPE_PRECISION (t));
      if (TYPE_UNSIGNED (t))
	dump_string (di, "unsigned");
      dump_child ("min", TYPE_MIN_VALUE (t));
      dump_child ("max", TYPE_MAX_VALUE (t));

      if (code == ENUMERAL_TYPE)
	dump_child ("csts", TYPE_VALUES (t));
      break;

    case REAL_TYPE:
      dump_int (di, "prec", TYPE_PRECISION (t));
      break;

    case POINTER_TYPE:
      dump_child ("ptd", TREE_TYPE (t));
      break;

    case REFERENCE_TYPE:
      dump_child ("refd", TREE_TYPE (t));
      break;

    case METHOD_TYPE:
      dump_child ("clas", TYPE_METHOD_BASETYPE (t));
      /* Fall through.  */

    case FUNCTION_TYPE:
      dump_child ("retn", TREE_TYPE (t));
      dump_child ("prms", TYPE_ARG_TYPES (t));
      break;

    case ARRAY_TYPE:
      dump_child ("elts", TREE_TYPE (t));
      dump_child ("domn", TYPE_DOMAIN (t));
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
      if (TREE_CODE (t) == RECORD_TYPE)
	dump_string (di, "struct");
      else
	dump_string (di, "union");

      dump_child ("flds", TYPE_FIELDS (t));
      dump_child ("fncs", TYPE_METHODS (t));
      queue_and_dump_index (di, "binf", TYPE_BINFO (t),
			    DUMP_BINFO);
      break;

    case CONST_DECL:
      dump_child ("cnst", DECL_INITIAL (t));
      break;

    case VAR_DECL:
    case PARM_DECL:
    case FIELD_DECL:
    case RESULT_DECL:
      if (TREE_CODE (t) == PARM_DECL)
	dump_child ("argt", DECL_ARG_TYPE (t));
      else
	dump_child ("init", DECL_INITIAL (t));
      dump_child ("size", DECL_SIZE (t));
      dump_int (di, "algn", DECL_ALIGN (t));

      if (TREE_CODE (t) == FIELD_DECL)
	{
	  if (DECL_FIELD_OFFSET (t))
	    dump_child ("bpos", bit_position (t));
	}
      else if (TREE_CODE (t) == VAR_DECL
	       || TREE_CODE (t) == PARM_DECL)
	{
	  dump_int (di, "used", TREE_USED (t));
	  if (DECL_REGISTER (t))
	    dump_string (di, "register");
	}
      break;

    case FUNCTION_DECL:
      dump_child ("args", DECL_ARGUMENTS (t));
      if (DECL_EXTERNAL (t))
	dump_string (di, "undefined");
      if (TREE_PUBLIC (t))
	dump_string (di, "extern");
      else
	dump_string (di, "static");
      if (DECL_LANG_SPECIFIC (t) && !dump_flag (di, TDF_SLIM, t))
	dump_child ("body", DECL_SAVED_TREE (t));
      break;

    case INTEGER_CST:
      if (TREE_INT_CST_HIGH (t))
	dump_int (di, "high", TREE_INT_CST_HIGH (t));
      dump_int (di, "low", TREE_INT_CST_LOW (t));
      break;

    case STRING_CST:
      fprintf (di->stream, "strg: %-7s ", TREE_STRING_POINTER (t));
      dump_int (di, "lngt", TREE_STRING_LENGTH (t));
      break;

    case TRUTH_NOT_EXPR:
    case ADDR_EXPR:
    case INDIRECT_REF:
    case ALIGN_INDIRECT_REF:
    case MISALIGNED_INDIRECT_REF:
    case CLEANUP_POINT_EXPR:
    case SAVE_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      /* These nodes are unary, but do not have code class `1'.  */
      dump_child ("op 0", TREE_OPERAND (t, 0));
      break;

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case INIT_EXPR:
    case MODIFY_EXPR:
    case COMPOUND_EXPR:
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
      /* These nodes are binary, but do not have code class `2'.  */
      dump_child ("op 0", TREE_OPERAND (t, 0));
      dump_child ("op 1", TREE_OPERAND (t, 1));
      break;

    case COMPONENT_REF:
      dump_child ("op 0", TREE_OPERAND (t, 0));
      dump_child ("op 1", TREE_OPERAND (t, 1));
      dump_child ("op 2", TREE_OPERAND (t, 2));
      break;

    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      dump_child ("op 0", TREE_OPERAND (t, 0));
      dump_child ("op 1", TREE_OPERAND (t, 1));
      dump_child ("op 2", TREE_OPERAND (t, 2));
      dump_child ("op 3", TREE_OPERAND (t, 3));
      break;

    case COND_EXPR:
      dump_child ("op 0", TREE_OPERAND (t, 0));
      dump_child ("op 1", TREE_OPERAND (t, 1));
      dump_child ("op 2", TREE_OPERAND (t, 2));
      break;

    case CALL_EXPR:
      dump_child ("fn", TREE_OPERAND (t, 0));
      dump_child ("args", TREE_OPERAND (t, 1));
      break;

    case CONSTRUCTOR:
      dump_child ("elts", CONSTRUCTOR_ELTS (t));
      break;

    case BIND_EXPR:
      dump_child ("vars", TREE_OPERAND (t, 0));
      dump_child ("body", TREE_OPERAND (t, 1));
      break;

    case LOOP_EXPR:
      dump_child ("body", TREE_OPERAND (t, 0));
      break;

    case EXIT_EXPR:
      dump_child ("cond", TREE_OPERAND (t, 0));
      break;

    case TARGET_EXPR:
      dump_child ("decl", TREE_OPERAND (t, 0));
      dump_child ("init", TREE_OPERAND (t, 1));
      dump_child ("clnp", TREE_OPERAND (t, 2));
      /* There really are two possible places the initializer can be.
	 After RTL expansion, the second operand is moved to the
	 position of the fourth operand, and the second operand
	 becomes NULL.  */
      dump_child ("init", TREE_OPERAND (t, 3));
      break;

    default:
      /* There are no additional fields to print.  */
      break;
    }

 done:
  if (dump_flag (di, TDF_ADDRESS, NULL))
    dump_pointer (di, "addr", (void *)t);

  /* Terminate the line.  */
  fprintf (di->stream, "\n");
}

/* Return nonzero if FLAG has been specified for the dump, and NODE
   is not the root node of the dump.  */

int dump_flag (dump_info_p di, int flag, tree node)
{
  return (di->flags & flag) && (node != di->node);
}

/* Dump T, and all its children, on STREAM.  */

void
dump_node (tree t, int flags, FILE *stream)
{
  struct dump_info di;
  dump_queue_p dq;
  dump_queue_p next_dq;

  /* Initialize the dump-information structure.  */
  di.stream = stream;
  di.index = 0;
  di.column = 0;
  di.queue = 0;
  di.queue_end = 0;
  di.free_list = 0;
  di.flags = flags;
  di.node = t;
  di.nodes = splay_tree_new (splay_tree_compare_pointers, 0,
			     (splay_tree_delete_value_fn) &free);

  /* Queue up the first node.  */
  queue (&di, t, DUMP_NONE);

  /* Until the queue is empty, keep dumping nodes.  */
  while (di.queue)
    dequeue_and_dump (&di);

  /* Now, clean up.  */
  for (dq = di.free_list; dq; dq = next_dq)
    {
      next_dq = dq->next;
      free (dq);
    }
  splay_tree_delete (di.nodes);
}


/* Table of tree dump switches. This must be consistent with the
   TREE_DUMP_INDEX enumeration in tree.h */
static struct dump_file_info dump_files[TDI_end] =
{
  {NULL, NULL, NULL, 0, 0, 0, 0},
  {".tu", "translation-unit", NULL, TDF_TREE, 0, 0, 0},
  {".class", "class-hierarchy", NULL, TDF_TREE, 0, 1, 0},
  {".original", "tree-original", NULL, TDF_TREE, 0, 2, 0},
  {".gimple", "tree-gimple", NULL, TDF_TREE, 0, 3, 0},
  {".nested", "tree-nested", NULL, TDF_TREE, 0, 4, 0},
  {".inlined", "tree-inlined", NULL, TDF_TREE, 0, 5, 0},
  {".vcg", "tree-vcg", NULL, TDF_TREE, 0, 6, 0},
  {NULL, "tree-all", NULL, TDF_TREE, 0, 0, 0},
  {NULL, "rtl-all", NULL, TDF_RTL, 0, 0, 0},
  {NULL, "ipa-all", NULL, TDF_IPA, 0, 0, 0},

  { ".cgraph", "ipa-cgraph", NULL,	TDF_IPA, 0,  1, 0},

  { ".sibling", "rtl-sibling", NULL,	TDF_RTL, 0,  1, 'i'},
  { ".eh", "rtl-eh", NULL,		TDF_RTL, 0,  2, 'h'},
  { ".jump", "rtl-jump", NULL,		TDF_RTL, 0,  3, 'j'},
  { ".cse", "rtl-cse", NULL,    	 TDF_RTL, 0,  4, 's'},
  { ".gcse", "rtl-gcse", NULL,		TDF_RTL, 0,  5, 'G'},
  { ".loop", "rtl-loop", NULL,		TDF_RTL, 0,  6, 'L'},
  { ".bypass", "rtl-bypass", NULL,		TDF_RTL, 0,  7, 'G'},
  { ".cfg", "rtl-cfg", NULL,			TDF_RTL, 0,  8, 'f'},
  { ".bp", "rtl-bp", NULL,			TDF_RTL, 0,  9, 'b'},
  { ".vpt", "rtl-vpt", NULL,			TDF_RTL, 0, 10, 'V'},
  { ".ce1", "rtl-ce1", NULL,			TDF_RTL, 0, 11, 'C'},
  { ".tracer", "rtl-tracer", NULL,		TDF_RTL, 0, 12, 'T'},
  { ".loop2", "rtl-loop2", NULL,		TDF_RTL, 0, 13, 'L'},
  { ".web", "rtl-web", NULL,			TDF_RTL, 0, 14, 'Z'},
  { ".cse2", "rtl-cse2", NULL,		TDF_RTL, 0, 15, 't'},
  { ".life", "rtl-life", NULL,		TDF_RTL, 0, 16, 'f'},
  { ".combine", "rtl-combine", NULL,		TDF_RTL, 0, 17, 'c'},
  { ".ce2", "rtl-ce2", NULL,			TDF_RTL, 0, 18, 'C'},
  { ".regmove", "rtl-regmove", NULL,		TDF_RTL, 0, 19, 'N'},
  { ".sms", "rtl-sms", NULL,			TDF_RTL, 0, 20, 'm'},
  { ".sched", "rtl-sched", NULL,		TDF_RTL, 0, 21, 'S'},
  { ".lreg", "rtl-lreg", NULL,		TDF_RTL, 0, 22, 'l'},
  { ".greg", "rtl-greg", NULL,		TDF_RTL, 0, 23, 'g'},
  { ".postreload", "rtl-postreload", NULL,	TDF_RTL, 0, 24, 'o'},
  { ".gcse2", "rtl-gcse2", NULL,		TDF_RTL, 0, 25, 'J'},
  { ".flow2", "rtl-flow2", NULL,		TDF_RTL, 0, 26, 'w'},
  { ".peephole2", "rtl-peephole2", NULL,	TDF_RTL, 0, 27, 'z'},
  { ".ce3", "rtl-ce3", NULL,			TDF_RTL, 0, 28, 'E'},
  { ".rnreg", "rtl-rnreg", NULL,		TDF_RTL, 0, 29, 'n'},
  { ".bbro", "rtl-bbro", NULL,		TDF_RTL, 0, 30, 'B'},
  { ".btl", "rtl-btl", NULL,			TDF_RTL, 0, 31, 'd'},
  { ".sched2", "rtl-sched2", NULL,		TDF_RTL, 0, 32, 'R'},
  { ".stack", "rtl-stack", NULL,		TDF_RTL, 0, 33, 'k'},
  { ".vartrack", "rtl-vartrack", NULL,	TDF_RTL, 0, 34, 'V'},
  { ".mach", "rtl-mach", NULL,		TDF_RTL, 0, 35, 'M'},
  { ".dbr", "rtl-dbr", NULL,			TDF_RTL, 0, 36, 'd'}
};

/* Dynamically registered tree dump files and switches.  */
static struct dump_file_info *extra_dump_files;
static size_t extra_dump_files_in_use;
static size_t extra_dump_files_alloced;

/* Define a name->number mapping for a dump flag value.  */
struct dump_option_value_info
{
  const char *const name;	/* the name of the value */
  const int value;		/* the value of the name */
};

/* Table of dump options. This must be consistent with the TDF_* flags
   in tree.h */
static const struct dump_option_value_info dump_options[] =
{
  {"address", TDF_ADDRESS},
  {"slim", TDF_SLIM},
  {"raw", TDF_RAW},
  {"details", TDF_DETAILS},
  {"stats", TDF_STATS},
  {"blocks", TDF_BLOCKS},
  {"vops", TDF_VOPS},
  {"lineno", TDF_LINENO},
  {"uid", TDF_UID},
  {"stmtaddr", TDF_STMTADDR},
  {"all", ~(TDF_RAW | TDF_SLIM | TDF_LINENO | TDF_TREE | TDF_RTL | TDF_IPA 
	    | TDF_STMTADDR)},
  {NULL, 0}
};

unsigned int
dump_register (const char *suffix, const char *swtch, const char *glob,
	       int flags, unsigned int num, int letter)
{
  size_t this = extra_dump_files_in_use++;

  if (this >= extra_dump_files_alloced)
    {
      if (extra_dump_files_alloced == 0)
	extra_dump_files_alloced = 32;
      else
	extra_dump_files_alloced *= 2;
      extra_dump_files = xrealloc (extra_dump_files,
				   sizeof (struct dump_file_info)
				   * extra_dump_files_alloced);
    }

  memset (&extra_dump_files[this], 0, sizeof (struct dump_file_info));
  extra_dump_files[this].suffix = suffix;
  extra_dump_files[this].swtch = swtch;
  extra_dump_files[this].glob = glob;
  extra_dump_files[this].flags = flags;
  extra_dump_files[this].num = num;
  extra_dump_files[this].letter = letter;

  return this + TDI_end;
}


/* Return the dump_file_info for the given phase.  */

struct dump_file_info *
get_dump_file_info (enum tree_dump_index phase)
{
  if (phase < TDI_end)
    return &dump_files[phase];
  else if (phase - TDI_end >= extra_dump_files_in_use)
    return NULL;
  else
    return extra_dump_files + (phase - TDI_end);
}


/* Return the name of the dump file for the given phase.
   If the dump is not enabled, returns NULL.  */

char *
get_dump_file_name (enum tree_dump_index phase)
{
  char dump_id[7];
  struct dump_file_info *dfi;

  if (phase == TDI_none)
    return NULL;

  dfi = get_dump_file_info (phase);
  if (dfi->state == 0)
    return NULL;

  if (dfi->num < 0)
    dump_id[0] = '\0';
  else
    {
      const char *template;
      if (dfi->flags & TDF_TREE)
	template = ".t%02d";
      else if (dfi->flags & TDF_IPA)
	template = ".i%02d";
      else
	template = ".%02d";

      if (snprintf (dump_id, sizeof (dump_id), template, dfi->num) < 0)
	dump_id[0] = '\0';
    }

  return concat (dump_base_name, dump_id, dfi->suffix, NULL);
}

/* Begin a tree dump for PHASE. Stores any user supplied flag in
   *FLAG_PTR and returns a stream to write to. If the dump is not
   enabled, returns NULL.
   Multiple calls will reopen and append to the dump file.  */

FILE *
dump_begin (enum tree_dump_index phase, int *flag_ptr)
{
  char *name;
  struct dump_file_info *dfi;
  FILE *stream;

  if (phase == TDI_none || !dump_enabled_p (phase))
    return NULL;

  name = get_dump_file_name (phase);
  dfi = get_dump_file_info (phase);
  stream = fopen (name, dfi->state < 0 ? "w" : "a");
  if (!stream)
    error ("could not open dump file %qs: %s", name, strerror (errno));
  else
    dfi->state = 1;
  free (name);

  if (flag_ptr)
    *flag_ptr = dfi->flags;

  return stream;
}

/* Returns nonzero if tree dump PHASE is enabled.  */

int
dump_enabled_p (enum tree_dump_index phase)
{
  struct dump_file_info *dfi = get_dump_file_info (phase);
  return dfi->state;
}

/* Returns nonzero if tree dump PHASE has been initialized.  */

int
dump_initialized_p (enum tree_dump_index phase)
{
  struct dump_file_info *dfi = get_dump_file_info (phase);
  return dfi->state > 0;
}

/* Returns the switch name of PHASE.  */

const char *
dump_flag_name (enum tree_dump_index phase)
{
  struct dump_file_info *dfi = get_dump_file_info (phase);
  return dfi->swtch;
}

/* Finish a tree dump for PHASE. STREAM is the stream created by
   dump_begin.  */

void
dump_end (enum tree_dump_index phase ATTRIBUTE_UNUSED, FILE *stream)
{
  fclose (stream);
}

/* Enable all tree dumps.  Return number of enabled tree dumps.  */

static int
dump_enable_all (int flags, int letter)
{
  int n = 0;
  size_t i;

  for (i = TDI_none + 1; i < (size_t) TDI_end; i++)
    if ((dump_files[i].flags & flags)
	&& (letter == 0 || letter == dump_files[i].letter))
      {
        dump_files[i].state = -1;
        dump_files[i].flags = flags;
        n++;
      }

  for (i = 0; i < extra_dump_files_in_use; i++)
    if ((extra_dump_files[i].flags & flags)
	&& (letter == 0 || letter == extra_dump_files[i].letter))
      {
        extra_dump_files[i].state = -1;
        extra_dump_files[i].flags = flags;
	n++;
      }

  return n;
}

/* Parse ARG as a dump switch. Return nonzero if it is, and store the
   relevant details in the dump_files array.  */

static int
dump_switch_p_1 (const char *arg, struct dump_file_info *dfi, bool doglob)
{
  const char *option_value;
  const char *ptr;
  int flags;
  
  if (doglob && !dfi->glob)
    return 0;

  option_value = skip_leading_substring (arg, doglob ? dfi->glob : dfi->swtch);
  if (!option_value)
    return 0;

  ptr = option_value;
  flags = 0;

  while (*ptr)
    {
      const struct dump_option_value_info *option_ptr;
      const char *end_ptr;
      unsigned length;

      while (*ptr == '-')
	ptr++;
      end_ptr = strchr (ptr, '-');
      if (!end_ptr)
	end_ptr = ptr + strlen (ptr);
      length = end_ptr - ptr;

      for (option_ptr = dump_options; option_ptr->name; option_ptr++)
	if (strlen (option_ptr->name) == length
	    && !memcmp (option_ptr->name, ptr, length))
	  {
	    flags |= option_ptr->value;
	    goto found;
	  }
      warning (0, "ignoring unknown option %q.*s in %<-fdump-%s%>",
	       length, ptr, dfi->swtch);
    found:;
      ptr = end_ptr;
    }

  dfi->state = -1;
  dfi->flags |= flags;

  /* Process -fdump-tree-all and -fdump-rtl-all, by enabling all the
     known dumps.  */
  if (dfi->suffix == NULL)
    dump_enable_all (dfi->flags, 0);

  return 1;
}

int
dump_switch_p (const char *arg)
{
  size_t i;
  int any = 0;

  for (i = TDI_none + 1; i != TDI_end; i++)
    any |= dump_switch_p_1 (arg, &dump_files[i], false);

  /* Don't glob if we got a hit already */
  if (!any)
    for (i = TDI_none + 1; i != TDI_end; i++)
      any |= dump_switch_p_1 (arg, &dump_files[i], true);

  for (i = 0; i < extra_dump_files_in_use; i++)
    any |= dump_switch_p_1 (arg, &extra_dump_files[i], false);
  
  if (!any)
    for (i = 0; i < extra_dump_files_in_use; i++)
      any |= dump_switch_p_1 (arg, &extra_dump_files[i], true);


  return any;
}

/* Dump FUNCTION_DECL FN as tree dump PHASE.  */

void
dump_function (enum tree_dump_index phase, tree fn)
{
  FILE *stream;
  int flags;

  stream = dump_begin (phase, &flags);
  if (stream)
    {
      dump_function_to_file (fn, stream, flags);
      dump_end (phase, stream);
    }
}

bool
enable_rtl_dump_file (int letter)
{
  if (letter == 'a')
    letter = 0;

  return dump_enable_all (TDF_RTL, letter) > 0;
}


