/* Tree-dumping functionality for intermediate representation.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.
   Written by Mark Mitchell <mark@codesourcery.com>

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
#include "tree.h"
#include "cp-tree.h"

/* Flags used with queue functions.  */
#define DUMP_NONE     0
#define DUMP_BINFO    1

/* Information about a node to be dumped.  */

typedef struct dump_node_info
{
  /* The index for the node.  */
  unsigned int index;
  /* Nonzero if the node is a binfo.  */
  unsigned int binfo_p : 1;
} *dump_node_info_p;

/* A dump_queue is a link in the queue of things to be dumped.  */

typedef struct dump_queue
{
  /* The queued tree node.  */
  splay_tree_node node;
  /* The next node in the queue.  */
  struct dump_queue *next;
} *dump_queue_p;

/* A dump_info gives information about how we should perform the dump 
   and about the current state of the dump.  */

typedef struct dump_info
{
  /* The stream on which to dump the information.  */
  FILE *stream;
  /* The next unused node index.  */
  unsigned int index;
  /* The next column.  */
  unsigned int column;
  /* The first node in the queue of nodes to be written out.  */
  dump_queue_p queue;
  /* The last node in the queue.  */
  dump_queue_p queue_end;
  /* Free queue nodes.  */
  dump_queue_p free_list;
  /* The tree nodes which we have already written out.  The 
     keys are the addresses of the nodes; the values are the integer
     indices we assigned them.  */
  splay_tree nodes;
} *dump_info_p;

static unsigned int queue PARAMS ((dump_info_p, tree, int));
static void dump_index PARAMS ((dump_info_p, unsigned int));
static void queue_and_dump_index PARAMS ((dump_info_p, const char *, tree, int));
static void queue_and_dump_type PARAMS ((dump_info_p, tree));
static void dequeue_and_dump PARAMS ((dump_info_p));
static void dump_new_line PARAMS ((dump_info_p));
static void dump_maybe_newline PARAMS ((dump_info_p));
static void dump_int PARAMS ((dump_info_p, const char *, int));
static void dump_string PARAMS ((dump_info_p, const char *));
static void dump_string_field PARAMS ((dump_info_p, const char *, const char *));
static void dump_node PARAMS ((tree, FILE *));
static void dump_stmt PARAMS ((dump_info_p, tree));
static void dump_next_stmt PARAMS ((dump_info_p, tree));

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

static void
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

static void
queue_and_dump_type (di, t)
     dump_info_p di;
     tree t;
{
  queue_and_dump_index (di, "type", TREE_TYPE (t), DUMP_NONE);
}

/* Insert a new line in the dump output, and indent to an appropriate
   place to start printing more fields.  */

static void
dump_new_line (di)
     dump_info_p di;
{
  fprintf (di->stream, "\n%25s", "");
  di->column = 25;
}

/* If necessary, insert a new line.  */

static void
dump_maybe_newline (di)
     dump_info_p di;
{
  /* See if we need a new line.  */
  if (di->column > 53)
    dump_new_line (di);
  /* See if we need any padding.  */
  else if ((di->column - 25) % 14 != 0)
    {
      fprintf (di->stream, "%*s", 14 - ((di->column - 25) % 14), "");
      di->column += 14 - (di->column - 25) % 14;
    }
}

/* Dump I using FIELD to identity it.  */

static void
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

static void
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

static void
dump_stmt (di, t)
     dump_info_p di;
     tree t;
{
  dump_int (di, "line", STMT_LINENO (t));
}

/* Dump the CHILD and its children.  */
#define dump_child(field, child) \
  queue_and_dump_index (di, field, child, DUMP_NONE)

/* Dump the next statement after STMT.  */

static void
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
	  my_friendly_abort (19990726);
	}
    }
  else if (code_class == 'd')
    {
      /* All declarations have names.  */
      if (DECL_NAME (t))
	dump_child ("name", DECL_NAME (t));
      /* And types.  */
      queue_and_dump_type (di, t);
      dump_child ("scpe", DECL_CONTEXT (t));
      /* And a source position.  */
      if (DECL_SOURCE_FILE (t))
	{
	  const char *filename = rindex (DECL_SOURCE_FILE (t), '/');
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
      if (TREE_CHAIN (t))
	dump_child ("chan", TREE_CHAIN (t));
    }
  else if (code_class == 't')
    {
      /* All types have qualifiers.  */
      int quals = CP_TYPE_QUALS (t);
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

  /* Now handle the various kinds of nodes.  */
  switch (code)
    {
      int i;

    case IDENTIFIER_NODE:
      if (IDENTIFIER_OPNAME_P (t))
	dump_string (di, "operator");
      else if (IDENTIFIER_TYPENAME_P (t))
	dump_child ("tynm", TREE_TYPE (t));
      else if (t == anonymous_namespace_name)
	dump_string (di, "unnamed");
      else
	{
	  dump_string_field (di, "strg", IDENTIFIER_POINTER (t));
	  dump_int (di, "lngt", IDENTIFIER_LENGTH (t));
	}
      break;

    case TREE_LIST:
      dump_child ("purp", TREE_PURPOSE (t));
      dump_child ("valu", TREE_VALUE (t));
      dump_child ("chan", TREE_CHAIN (t));
      break;

    case TREE_VEC:
      dump_int (di, "lngt", IDENTIFIER_LENGTH (t));
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
      if (TYPE_PTRMEM_P (t))
	{
	  dump_string (di, "ptrmem");
	  dump_child ("ptd", TYPE_PTRMEM_POINTED_TO_TYPE (t));
	  dump_child ("cls", TYPE_PTRMEM_CLASS_TYPE (t));
	}
      else
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
      if (TYPE_PTRMEMFUNC_P (t))
	{
	  dump_string (di, "ptrmem");
	  dump_child ("ptd", TYPE_PTRMEM_POINTED_TO_TYPE (t));
	  dump_child ("cls", TYPE_PTRMEM_CLASS_TYPE (t));
	}
      else
	{
	  if (CLASSTYPE_DECLARED_CLASS (t))
	    dump_string (di, "class");
	  else if (TREE_CODE (t) == RECORD_TYPE)
	    dump_string (di, "struct");
	  else
	    dump_string (di, "union");

	  dump_child ("flds", TYPE_FIELDS (t));
	  dump_child ("fncs", TYPE_METHODS (t));
	  dump_child ("vfld", TYPE_VFIELD (t));
	  queue_and_dump_index (di, "binf", TYPE_BINFO (t), 
				DUMP_BINFO);
	}
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
      break;

    case FUNCTION_DECL:
      dump_child ("mngl", DECL_ASSEMBLER_NAME (t));
      dump_child ("args", DECL_ARGUMENTS (t));
      if (DECL_EXTERNAL (t))
	dump_string (di, "undefined");
      if (TREE_PUBLIC (t))
	dump_string(di, "extern");
      else
	dump_string (di, "static");
      if (TREE_CODE (t) == FUNCTION_DECL)
	{
	  if (DECL_FUNCTION_MEMBER_P (t))
	    dump_string (di, "member");
	  if (DECL_CONSTRUCTOR_P (t))
	    dump_string (di, "constructor");
	  if (DECL_DESTRUCTOR_P (t))
	    dump_string (di, "destructor");
	  if (DECL_OVERLOADED_OPERATOR_P (t))
	    dump_string (di, "operator");
	  if (DECL_CONV_FN_P (t))
	    dump_string (di, "conversion");
	  if (DECL_GLOBAL_CTOR_P (t) || DECL_GLOBAL_DTOR_P (t))
	    {
	      if (DECL_GLOBAL_CTOR_P (t))
		dump_string (di, "global init");
	      if (DECL_GLOBAL_DTOR_P (t))
		dump_string (di, "global fini");
	      dump_int (di, "prio", GLOBAL_INIT_PRIORITY (t));
	    }
	  if (DECL_FRIEND_PSEUDO_TEMPLATE_INSTANTIATION (t))
	    dump_string (di, "pseudo tmpl");

	  dump_child ("body", DECL_SAVED_TREE (t));
	}
      else
	{
	  dump_int (di, "dlta", THUNK_DELTA (t));
	  dump_child ("init", DECL_INITIAL (t));
	}
      break;

    case NAMESPACE_DECL:
      /* The fake `::std' namespace does not have DECL_LANG_SPECIFIC,
	 and therefore many other macros do not work on it.  */
      if (t == std_node)
	break;
      if (DECL_NAMESPACE_ALIAS (t))
	dump_child ("alis", DECL_NAMESPACE_ALIAS (t));
      else
	dump_child ("dcls", cp_namespace_decls (t));
      break;

    case TEMPLATE_DECL:
      dump_child ("rslt", DECL_TEMPLATE_RESULT (t));
      dump_child ("inst", DECL_TEMPLATE_INSTANTIATIONS (t));
      dump_child ("spcs", DECL_TEMPLATE_SPECIALIZATIONS (t));
      break;

    case OVERLOAD:
      dump_child ("crnt", OVL_CURRENT (t));
      dump_child ("chan", OVL_CHAIN (t));
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
      /* Note that a case label is not like other statments; there is
	 no way to get the line-number of a case label.  */
      dump_child ("low", CASE_LOW (t));
      dump_child ("high", CASE_HIGH (t));
      dump_next_stmt (di, t);
      break;

    case COMPOUND_STMT:
      dump_stmt (di, t);
      dump_child ("body", COMPOUND_BODY (t));
      dump_next_stmt (di, t);
      break;

    case CTOR_STMT:
      dump_stmt (di, t);
      if (CTOR_BEGIN_P (t))
	dump_string (di, "begn");
      else
	dump_string (di, "end");
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

    case HANDLER:
      dump_stmt (di, t);
      dump_child ("body", HANDLER_BODY (t));
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

    case TRY_BLOCK:
      dump_stmt (di, t);
      if (CLEANUP_P (t))
	dump_string (di, "cleanup");
      dump_child ("body", TRY_STMTS (t));
      dump_child ("hdlr", TRY_HANDLERS (t));
      dump_next_stmt (di, t);
      break;

    case WHILE_STMT:
      dump_stmt (di, t);
      dump_child ("cond", WHILE_COND (t));
      dump_child ("body", WHILE_BODY (t));
      dump_next_stmt (di, t);
      break;

    case SUBOBJECT:
      dump_stmt (di, t);
      dump_child ("clnp", TREE_OPERAND (t, 0));
      dump_next_stmt (di, t);
      break;

    case START_CATCH_STMT:
      dump_stmt (di, t);
      queue_and_dump_type (di, t);
      dump_next_stmt (di, t);
      break;

    case CLEANUP_STMT:
      dump_stmt (di, t);
      dump_child ("decl", CLEANUP_DECL (t));
      dump_child ("expr", CLEANUP_EXPR (t));
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

    case PTRMEM_CST:
      dump_child ("clas", PTRMEM_CST_CLASS (t));
      dump_child ("mbr", PTRMEM_CST_MEMBER (t));
      break;

    case TRUTH_NOT_EXPR:
    case ADDR_EXPR:
    case INDIRECT_REF:
    case THROW_EXPR:
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
      
    case AGGR_INIT_EXPR:
      dump_int (di, "ctor", AGGR_INIT_VIA_CTOR_P (t));
      dump_child ("fn", TREE_OPERAND (t, 0));
      dump_child ("args", TREE_OPERAND (t, 1));
      dump_child ("decl", TREE_OPERAND (t, 2));
      break;
      
    case EXPR_WITH_FILE_LOCATION:
      dump_child ("expr", EXPR_WFL_NODE (t));
      break;

    default:
      /* There are no additional fields to print.  */
      break;
    }

 done:
  /* Terminate the line.  */
  fprintf (di->stream, "\n");
}

/* Dump T, and all its children, on STREAM.  */

static void
dump_node (t, stream)
     tree t;
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

/* Dump T, and all its children, to FILE.  */

void
dump_node_to_file (t, file)
     tree t;
     const char *file;
{
  FILE *f;

  f = fopen (file, "w");
  if (!f)
    cp_error ("could not open `%s'", file);
  else
    {
      dump_node (t, f);
      fclose (f);
    }
}
