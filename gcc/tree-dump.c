/* Tree-dumping functionality for intermediate representation.
   Copyright (C) 1999, 2000, 2002 Free Software Foundation, Inc.
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
#include "tree.h"
#include "c-tree.h"
#include "splay-tree.h"
#include "diagnostic.h"
#include "toplev.h"
#include "tree-dump.h"
#include "langhooks.h"

static unsigned int queue PARAMS ((dump_info_p, tree, int));
static void dump_index PARAMS ((dump_info_p, unsigned int));
static void dequeue_and_dump PARAMS ((dump_info_p));
static void dump_new_line PARAMS ((dump_info_p));
static void dump_maybe_newline PARAMS ((dump_info_p));
static void dump_string_field PARAMS ((dump_info_p, const char *, const char *));

/* Add T to the end of the queue of nodes to dump.  Returns the index
   assigned to T.  */

static unsigned int
queue (di, t, flags)
     dump_info_p di;
     tree t;
     int flags;
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
    dq = (dump_queue_p) xmalloc (sizeof (struct dump_queue));

  /* Create a new entry in the splay-tree.  */
  dni = (dump_node_info_p) xmalloc (sizeof (struct dump_node_info));
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
dump_index (di, index)
     dump_info_p di;
     unsigned int index;
{
  fprintf (di->stream, "@%-6u ", index);
  di->column += 8;
}

/* If T has not already been output, queue it for subsequent output.
   FIELD is a string to print before printing the index.  Then, the
   index of T is printed.  */

void
queue_and_dump_index (di, field, t, flags)
     dump_info_p di;
     const char *field;
     tree t;
     int flags;
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
queue_and_dump_type (di, t)
     dump_info_p di;
     tree t;
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
dump_new_line (di)
     dump_info_p di;
{
  fprintf (di->stream, "\n%*s", SOL_COLUMN, "");
  di->column = SOL_COLUMN;
}

/* If necessary, insert a new line.  */

static void
dump_maybe_newline (di)
     dump_info_p di;
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
dump_pointer (di, field, ptr)
     dump_info_p di;
     const char *field;
     void *ptr;
{
  dump_maybe_newline (di);
  fprintf (di->stream, "%-4s: %-8lx ", field, (long) ptr);
  di->column += 15;
}

/* Dump integer I using FIELD to identify it.  */

void
dump_int (di, field, i)
     dump_info_p di;
     const char *field;
     int i;
{
  dump_maybe_newline (di);
  fprintf (di->stream, "%-4s: %-7d ", field, i);
  di->column += 14;
}

/* Dump the string S.  */

void
dump_string (di, string)
     dump_info_p di;
     const char *string;
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
dump_string_field (di, field, string)
     dump_info_p di;
     const char *field;
     const char *string;
{
  dump_maybe_newline (di);
  fprintf (di->stream, "%-4s: %-7s ", field, string);
  if (strlen (string) > 7)
    di->column += 6 + strlen (string) + 1;
  else
    di->column += 14;
}

/* Dump information common to statements from STMT.  */

void
dump_stmt (di, t)
     dump_info_p di;
     tree t;
{
  dump_int (di, "line", STMT_LINENO (t));
}

/* Dump the next statement after STMT.  */

void
dump_next_stmt (di, t)
     dump_info_p di;
     tree t;
{
  dump_child ("next", TREE_CHAIN (t));
}

/* Dump the next node in the queue.  */

static void 
dequeue_and_dump (di)
     dump_info_p di;
{
  dump_queue_p dq;
  splay_tree_node stn;
  dump_node_info_p dni;
  tree t;
  unsigned int index;
  enum tree_code code;
  char code_class;
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
      if (TREE_VIA_PUBLIC (t))
	dump_string (di, "pub");
      else if (TREE_VIA_PROTECTED (t))
	dump_string (di, "prot");
      else if (TREE_VIA_PRIVATE (t))
	dump_string (di, "priv");
      if (TREE_VIA_VIRTUAL (t))
	dump_string (di, "virt");
	    
      dump_child ("type", BINFO_TYPE (t));
      dump_child ("base", BINFO_BASETYPES (t));

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
	case '1':
	  dump_child ("op 0", TREE_OPERAND (t, 0));
	  break;
	      
	case '2':
	case '<':
	  dump_child ("op 0", TREE_OPERAND (t, 0));
	  dump_child ("op 1", TREE_OPERAND (t, 1));
	  break;
	      
	case 'e':
	  /* These nodes are handled explicitly below.  */
	  break;
	      
	default:
	  abort ();
	}
    }
  else if (DECL_P (t))
    {
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
      if (DECL_SOURCE_FILE (t))
	{
	  const char *filename = strrchr (DECL_SOURCE_FILE (t), '/');
	  if (!filename)
	    filename = DECL_SOURCE_FILE (t);
	  else
	    /* Skip the slash.  */
	    ++filename;

	  dump_maybe_newline (di);
	  fprintf (di->stream, "srcp: %s:%-6d ", filename, 
		   DECL_SOURCE_LINE (t));
	  di->column += 6 + strlen (filename) + 8;
	}
      /* And any declaration can be compiler-generated.  */
      if (DECL_ARTIFICIAL (t))
	dump_string (di, "artificial");
      if (TREE_CHAIN (t) && !dump_flag (di, TDF_SLIM, NULL))
	dump_child ("chan", TREE_CHAIN (t));
    }
  else if (code_class == 't')
    {
      /* All types have qualifiers.  */
      int quals = (*lang_hooks.tree_dump.type_quals) (t);
      
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
  else if (code_class == 'c')
    /* All constants can have types.  */
    queue_and_dump_type (di, t);

  /* Give the language-specific code a chance to print something.  If
     it's completely taken care of things, don't bother printing
     anything more ourselves.  */
  if ((*lang_hooks.tree_dump.dump_tree) (di, t))
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
      if (TREE_UNSIGNED (t))
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
	  if (DECL_C_BIT_FIELD (t))
	    dump_string (di, "bitfield");
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

    case ASM_STMT:
      dump_stmt (di, t);
      if (ASM_VOLATILE_P (t))
	dump_string (di, "volatile");
      dump_child ("strg", ASM_STRING (t));
      dump_child ("outs", ASM_OUTPUTS (t));
      dump_child ("ins", ASM_INPUTS (t));
      dump_child ("clbr", ASM_CLOBBERS (t));
      dump_next_stmt (di, t);
      break;

    case BREAK_STMT:
    case CONTINUE_STMT:
      dump_stmt (di, t);
      dump_next_stmt (di, t);
      break;

    case CASE_LABEL:
      /* Note that a case label is not like other statements; there is
	 no way to get the line-number of a case label.  */
      dump_child ("low", CASE_LOW (t));
      dump_child ("high", CASE_HIGH (t));
      dump_next_stmt (di, t);
      break;

    case CLEANUP_STMT:
      dump_stmt (di, t);
      dump_child ("decl", CLEANUP_DECL (t));
      dump_child ("expr", CLEANUP_EXPR (t));
      dump_next_stmt (di, t);
      break;

    case COMPOUND_STMT:
      dump_stmt (di, t);
      dump_child ("body", COMPOUND_BODY (t));
      dump_next_stmt (di, t);
      break;

    case DECL_STMT:
      dump_stmt (di, t);
      dump_child ("decl", DECL_STMT_DECL (t));
      dump_next_stmt (di, t);
      break;
      
    case DO_STMT:
      dump_stmt (di, t);
      dump_child ("body", DO_BODY (t));
      dump_child ("cond", DO_COND (t));
      dump_next_stmt (di, t);
      break;

    case EXPR_STMT:
      dump_stmt (di, t);
      dump_child ("expr", EXPR_STMT_EXPR (t));
      dump_next_stmt (di, t);
      break;

    case FOR_STMT:
      dump_stmt (di, t);
      dump_child ("init", FOR_INIT_STMT (t));
      dump_child ("cond", FOR_COND (t));
      dump_child ("expr", FOR_EXPR (t));
      dump_child ("body", FOR_BODY (t));
      dump_next_stmt (di, t);
      break;

    case GOTO_STMT:
      dump_stmt (di, t);
      dump_child ("dest", GOTO_DESTINATION (t));
      dump_next_stmt (di, t);
      break;

    case IF_STMT:
      dump_stmt (di, t);
      dump_child ("cond", IF_COND (t));
      dump_child ("then", THEN_CLAUSE (t));
      dump_child ("else", ELSE_CLAUSE (t));
      dump_next_stmt (di, t);
      break;

    case LABEL_STMT:
      dump_stmt (di, t);
      dump_child ("labl", LABEL_STMT_LABEL (t));
      dump_next_stmt (di, t);
      break;

    case RETURN_STMT:
      dump_stmt (di, t);
      dump_child ("expr", RETURN_EXPR (t));
      dump_next_stmt (di, t);
      break;

    case SWITCH_STMT:
      dump_stmt (di, t);
      dump_child ("cond", SWITCH_COND (t));
      dump_child ("body", SWITCH_BODY (t));
      dump_next_stmt (di, t);
      break;

    case WHILE_STMT:
      dump_stmt (di, t);
      dump_child ("cond", WHILE_COND (t));
      dump_child ("body", WHILE_BODY (t));
      dump_next_stmt (di, t);
      break;

    case SCOPE_STMT:
      dump_stmt (di, t);
      if (SCOPE_BEGIN_P (t))
	dump_string (di, "begn");
      else
	dump_string (di, "end");
      if (SCOPE_NULLIFIED_P (t))
	dump_string (di, "null");
      if (!SCOPE_NO_CLEANUPS_P (t))
	dump_string (di, "clnp");
      dump_next_stmt (di, t);
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
    case CLEANUP_POINT_EXPR:
    case SAVE_EXPR:
      /* These nodes are unary, but do not have code class `1'.  */
      dump_child ("op 0", TREE_OPERAND (t, 0));
      break;

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case INIT_EXPR:
    case MODIFY_EXPR:
    case COMPONENT_REF:
    case COMPOUND_EXPR:
    case ARRAY_REF:
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
      /* These nodes are binary, but do not have code class `2'.  */
      dump_child ("op 0", TREE_OPERAND (t, 0));
      dump_child ("op 1", TREE_OPERAND (t, 1));
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
      dump_child ("elts", TREE_OPERAND (t, 1));
      break;

    case STMT_EXPR:
      dump_child ("stmt", STMT_EXPR_STMT (t));
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
      
    case EXPR_WITH_FILE_LOCATION:
      dump_child ("expr", EXPR_WFL_NODE (t));
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

/* Return non-zero if FLAG has been specified for the dump, and NODE
   is not the root node of the dump.  */

int dump_flag (di, flag, node)
     dump_info_p di;
     int flag;
     tree node;
{
  return (di->flags & flag) && (node != di->node);
}

/* Dump T, and all its children, on STREAM.  */

void
dump_node (t, flags, stream)
     tree t;
     int flags;
     FILE *stream;
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

/* Define a tree dump switch.  */
struct dump_file_info
{
  const char *const suffix;	/* suffix to give output file.  */
  const char *const swtch;	/* command line switch */
  int flags;			/* user flags */
  int state;			/* state of play */
};

/* Table of tree dump switches. This must be consistent with the
   TREE_DUMP_INDEX enumeration in tree.h */
static struct dump_file_info dump_files[TDI_end] =
{
  {".tu", "dump-translation-unit", 0, 0},
  {".class", "dump-class-hierarchy", 0, 0},
  {".original", "dump-tree-original", 0, 0},
  {".optimized", "dump-tree-optimized", 0, 0},
  {".inlined", "dump-tree-inlined", 0, 0},
};

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
  {"all", ~0},
  {NULL, 0}
};

/* Begin a tree dump for PHASE. Stores any user supplied flag in
   *FLAG_PTR and returns a stream to write to. If the dump is not
   enabled, returns NULL.
   Multiple calls will reopen and append to the dump file.  */

FILE *
dump_begin (phase, flag_ptr)
     enum tree_dump_index phase;
     int *flag_ptr;
{
  FILE *stream;
  char *name;
  
  if (!dump_files[phase].state)
    return NULL;
  
  name = concat (dump_base_name, dump_files[phase].suffix, NULL);
  stream = fopen (name, dump_files[phase].state < 0 ? "w" : "a");
  if (!stream)
    error ("could not open dump file `%s'", name);
  else
    dump_files[phase].state = 1;
  free (name);
  if (flag_ptr)
    *flag_ptr = dump_files[phase].flags;
  
  return stream;
}

/* Returns non-zero if tree dump PHASE is enabled.  */

int
dump_enabled_p (phase)
     enum tree_dump_index phase;
{
  return dump_files[phase].state;
}

/* Returns the switch name of PHASE.  */

const char *
dump_flag_name (phase)
     enum tree_dump_index phase;
{
  return dump_files[phase].swtch;
}

/* Finish a tree dump for PHASE. STREAM is the stream created by
   dump_begin.  */

void
dump_end (phase, stream)
     enum tree_dump_index phase ATTRIBUTE_UNUSED;
     FILE *stream;
{
  fclose (stream);
}

/* Parse ARG as a dump switch. Return non-zero if it is, and store the
   relevant details in the dump_files array.  */

int
dump_switch_p (arg)
     const char *arg;
{
  unsigned ix;
  const char *option_value;
  
  for (ix = 0; ix != TDI_end; ix++)
    if ((option_value = skip_leading_substring (arg, dump_files[ix].swtch)))
      {
	const char *ptr = option_value;
	int flags = 0;
	
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
	    
	    for (option_ptr = dump_options; option_ptr->name;
		 option_ptr++)
	      if (strlen (option_ptr->name) == length
		  && !memcmp (option_ptr->name, ptr, length))
		{
		  flags |= option_ptr->value;
		  goto found;
		}
	    warning ("ignoring unknown option `%.*s' in `-f%s'",
		     length, ptr, dump_files[ix].swtch);
	  found:;
	    ptr = end_ptr;
	  }
	
	dump_files[ix].state = -1;
	dump_files[ix].flags = flags;
	
	return 1;
      }
  return 0;
}
