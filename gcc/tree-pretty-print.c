/* Pretty formatting of GENERIC trees in C syntax.
   Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.
   Adapted from c-pretty-print.c by Diego Novillo <dnovillo@redhat.com>

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "output.h"
#include "tree-pretty-print.h"
#include "hashtab.h"
#include "tree-flow.h"
#include "langhooks.h"
#include "tree-iterator.h"
#include "tree-chrec.h"
#include "tree-pass.h"
#include "value-prof.h"
#include "predict.h"

/* Local functions, macros and variables.  */
static const char *op_symbol (const_tree);
static void pretty_print_string (pretty_printer *, const char*);
static void newline_and_indent (pretty_printer *, int);
static void maybe_init_pretty_print (FILE *);
static void print_struct_decl (pretty_printer *, const_tree, int, int);
static void do_niy (pretty_printer *, const_tree);

#define INDENT(SPACE) do { \
  int i; for (i = 0; i<SPACE; i++) pp_space (buffer); } while (0)

#define NIY do_niy(buffer,node)

static pretty_printer buffer;
static int initialized = 0;

/* Try to print something for an unknown tree code.  */

static void
do_niy (pretty_printer *buffer, const_tree node)
{
  int i, len;

  pp_string (buffer, "<<< Unknown tree: ");
  pp_string (buffer, tree_code_name[(int) TREE_CODE (node)]);

  if (EXPR_P (node))
    {
      len = TREE_OPERAND_LENGTH (node);
      for (i = 0; i < len; ++i)
	{
	  newline_and_indent (buffer, 2);
	  dump_generic_node (buffer, TREE_OPERAND (node, i), 2, 0, false);
	}
    }

  pp_string (buffer, " >>>");
}

/* Debugging function to print out a generic expression.  */

DEBUG_FUNCTION void
debug_generic_expr (tree t)
{
  print_generic_expr (stderr, t, TDF_VOPS|TDF_MEMSYMS);
  fprintf (stderr, "\n");
}

/* Debugging function to print out a generic statement.  */

DEBUG_FUNCTION void
debug_generic_stmt (tree t)
{
  print_generic_stmt (stderr, t, TDF_VOPS|TDF_MEMSYMS);
  fprintf (stderr, "\n");
}

/* Debugging function to print out a chain of trees .  */

DEBUG_FUNCTION void
debug_tree_chain (tree t)
{
  struct pointer_set_t *seen = pointer_set_create ();

  while (t)
    {
      print_generic_expr (stderr, t, TDF_VOPS|TDF_MEMSYMS|TDF_UID);
      fprintf (stderr, " ");
      t = TREE_CHAIN (t);
      if (pointer_set_insert (seen, t))
	{
	  fprintf (stderr, "... [cycled back to ");
	  print_generic_expr (stderr, t, TDF_VOPS|TDF_MEMSYMS|TDF_UID);
	  fprintf (stderr, "]");
	  break;
	}
    }
  fprintf (stderr, "\n");

  pointer_set_destroy (seen);
}

/* Prints declaration DECL to the FILE with details specified by FLAGS.  */
void
print_generic_decl (FILE *file, tree decl, int flags)
{
  maybe_init_pretty_print (file);
  print_declaration (&buffer, decl, 2, flags);
  pp_write_text_to_stream (&buffer);
}

/* Print tree T, and its successors, on file FILE.  FLAGS specifies details
   to show in the dump.  See TDF_* in tree-pass.h.  */

void
print_generic_stmt (FILE *file, tree t, int flags)
{
  maybe_init_pretty_print (file);
  dump_generic_node (&buffer, t, 0, flags, true);
  pp_flush (&buffer);
}

/* Print tree T, and its successors, on file FILE.  FLAGS specifies details
   to show in the dump.  See TDF_* in tree-pass.h.  The output is indented by
   INDENT spaces.  */

void
print_generic_stmt_indented (FILE *file, tree t, int flags, int indent)
{
  int i;

  maybe_init_pretty_print (file);

  for (i = 0; i < indent; i++)
    pp_space (&buffer);
  dump_generic_node (&buffer, t, indent, flags, true);
  pp_flush (&buffer);
}

/* Print a single expression T on file FILE.  FLAGS specifies details to show
   in the dump.  See TDF_* in tree-pass.h.  */

void
print_generic_expr (FILE *file, tree t, int flags)
{
  maybe_init_pretty_print (file);
  dump_generic_node (&buffer, t, 0, flags, false);
}

/* Dump the name of a _DECL node and its DECL_UID if TDF_UID is set
   in FLAGS.  */

static void
dump_decl_name (pretty_printer *buffer, tree node, int flags)
{
  if (DECL_NAME (node))
    {
      if ((flags & TDF_ASMNAME) && DECL_ASSEMBLER_NAME_SET_P (node))
	pp_tree_identifier (buffer, DECL_ASSEMBLER_NAME (node));
      else
	pp_tree_identifier (buffer, DECL_NAME (node));
    }
  if ((flags & TDF_UID) || DECL_NAME (node) == NULL_TREE)
    {
      if (TREE_CODE (node) == LABEL_DECL && LABEL_DECL_UID (node) != -1)
	pp_printf (buffer, "L.%d", (int) LABEL_DECL_UID (node));
      else if (TREE_CODE (node) == DEBUG_EXPR_DECL)
	{
	  if (flags & TDF_NOUID)
	    pp_string (buffer, "D#xxxx");
	  else
	    pp_printf (buffer, "D#%i", DEBUG_TEMP_UID (node));
	}
      else
	{
	  char c = TREE_CODE (node) == CONST_DECL ? 'C' : 'D';
	  if (flags & TDF_NOUID)
	    pp_printf (buffer, "%c.xxxx", c);
	  else
	    pp_printf (buffer, "%c.%u", c, DECL_UID (node));
	}
    }
  if ((flags & TDF_ALIAS) && DECL_PT_UID (node) != DECL_UID (node))
    {
      if (flags & TDF_NOUID)
	pp_printf (buffer, "ptD.xxxx");
      else
	pp_printf (buffer, "ptD.%u", DECL_PT_UID (node));
    }
}

/* Like the above, but used for pretty printing function calls.  */

static void
dump_function_name (pretty_printer *buffer, tree node, int flags)
{
  if (TREE_CODE (node) == NOP_EXPR)
    node = TREE_OPERAND (node, 0);
  if (DECL_NAME (node) && (flags & TDF_ASMNAME) == 0)
    pp_string (buffer, lang_hooks.decl_printable_name (node, 1));
  else
    dump_decl_name (buffer, node, flags);
}

/* Dump a function declaration.  NODE is the FUNCTION_TYPE.  BUFFER, SPC and
   FLAGS are as in dump_generic_node.  */

static void
dump_function_declaration (pretty_printer *buffer, tree node,
			   int spc, int flags)
{
  bool wrote_arg = false;
  tree arg;

  pp_space (buffer);
  pp_character (buffer, '(');

  /* Print the argument types.  The last element in the list is a VOID_TYPE.
     The following avoids printing the last element.  */
  arg = TYPE_ARG_TYPES (node);
  while (arg && TREE_CHAIN (arg) && arg != error_mark_node)
    {
      wrote_arg = true;
      dump_generic_node (buffer, TREE_VALUE (arg), spc, flags, false);
      arg = TREE_CHAIN (arg);
      if (TREE_CHAIN (arg) && TREE_CODE (TREE_CHAIN (arg)) == TREE_LIST)
	{
	  pp_character (buffer, ',');
	  pp_space (buffer);
	}
    }

  if (!wrote_arg)
    pp_string (buffer, "void");

  pp_character (buffer, ')');
}

/* Dump the domain associated with an array.  */

static void
dump_array_domain (pretty_printer *buffer, tree domain, int spc, int flags)
{
  pp_character (buffer, '[');
  if (domain)
    {
      tree min = TYPE_MIN_VALUE (domain);
      tree max = TYPE_MAX_VALUE (domain);

      if (min && max
	  && integer_zerop (min)
	  && host_integerp (max, 0))
	pp_wide_integer (buffer, TREE_INT_CST_LOW (max) + 1);
      else
	{
	  if (min)
	    dump_generic_node (buffer, min, spc, flags, false);
	  pp_character (buffer, ':');
	  if (max)
	    dump_generic_node (buffer, max, spc, flags, false);
	}
    }
  else
    pp_string (buffer, "<unknown>");
  pp_character (buffer, ']');
}


/* Dump OpenMP clause CLAUSE.  BUFFER, CLAUSE, SPC and FLAGS are as in
   dump_generic_node.  */

static void
dump_omp_clause (pretty_printer *buffer, tree clause, int spc, int flags)
{
  const char *name;

  switch (OMP_CLAUSE_CODE (clause))
    {
    case OMP_CLAUSE_PRIVATE:
      name = "private";
      goto print_remap;
    case OMP_CLAUSE_SHARED:
      name = "shared";
      goto print_remap;
    case OMP_CLAUSE_FIRSTPRIVATE:
      name = "firstprivate";
      goto print_remap;
    case OMP_CLAUSE_LASTPRIVATE:
      name = "lastprivate";
      goto print_remap;
    case OMP_CLAUSE_COPYIN:
      name = "copyin";
      goto print_remap;
    case OMP_CLAUSE_COPYPRIVATE:
      name = "copyprivate";
      goto print_remap;
  print_remap:
      pp_string (buffer, name);
      pp_character (buffer, '(');
      dump_generic_node (buffer, OMP_CLAUSE_DECL (clause),
	  spc, flags, false);
      pp_character (buffer, ')');
      break;

    case OMP_CLAUSE_REDUCTION:
      pp_string (buffer, "reduction(");
      pp_string (buffer, op_symbol_code (OMP_CLAUSE_REDUCTION_CODE (clause)));
      pp_character (buffer, ':');
      dump_generic_node (buffer, OMP_CLAUSE_DECL (clause),
	  spc, flags, false);
      pp_character (buffer, ')');
      break;

    case OMP_CLAUSE_IF:
      pp_string (buffer, "if(");
      dump_generic_node (buffer, OMP_CLAUSE_IF_EXPR (clause),
	  spc, flags, false);
      pp_character (buffer, ')');
      break;

    case OMP_CLAUSE_NUM_THREADS:
      pp_string (buffer, "num_threads(");
      dump_generic_node (buffer, OMP_CLAUSE_NUM_THREADS_EXPR (clause),
	  spc, flags, false);
      pp_character (buffer, ')');
      break;

    case OMP_CLAUSE_NOWAIT:
      pp_string (buffer, "nowait");
      break;
    case OMP_CLAUSE_ORDERED:
      pp_string (buffer, "ordered");
      break;

    case OMP_CLAUSE_DEFAULT:
      pp_string (buffer, "default(");
      switch (OMP_CLAUSE_DEFAULT_KIND (clause))
	{
	case OMP_CLAUSE_DEFAULT_UNSPECIFIED:
	  break;
	case OMP_CLAUSE_DEFAULT_SHARED:
	  pp_string (buffer, "shared");
	  break;
	case OMP_CLAUSE_DEFAULT_NONE:
	  pp_string (buffer, "none");
	  break;
	case OMP_CLAUSE_DEFAULT_PRIVATE:
	  pp_string (buffer, "private");
	  break;
	case OMP_CLAUSE_DEFAULT_FIRSTPRIVATE:
	  pp_string (buffer, "firstprivate");
	  break;
	default:
	  gcc_unreachable ();
	}
      pp_character (buffer, ')');
      break;

    case OMP_CLAUSE_SCHEDULE:
      pp_string (buffer, "schedule(");
      switch (OMP_CLAUSE_SCHEDULE_KIND (clause))
	{
      case OMP_CLAUSE_SCHEDULE_STATIC:
	pp_string (buffer, "static");
	break;
      case OMP_CLAUSE_SCHEDULE_DYNAMIC:
	pp_string (buffer, "dynamic");
	break;
      case OMP_CLAUSE_SCHEDULE_GUIDED:
	pp_string (buffer, "guided");
	break;
      case OMP_CLAUSE_SCHEDULE_RUNTIME:
	pp_string (buffer, "runtime");
	break;
      case OMP_CLAUSE_SCHEDULE_AUTO:
	pp_string (buffer, "auto");
	break;
      default:
	gcc_unreachable ();
	}
      if (OMP_CLAUSE_SCHEDULE_CHUNK_EXPR (clause))
	{
	  pp_character (buffer, ',');
	  dump_generic_node (buffer,
	      OMP_CLAUSE_SCHEDULE_CHUNK_EXPR (clause),
	      spc, flags, false);
	}
      pp_character (buffer, ')');
      break;

    case OMP_CLAUSE_UNTIED:
      pp_string (buffer, "untied");
      break;

    case OMP_CLAUSE_COLLAPSE:
      pp_string (buffer, "collapse(");
      dump_generic_node (buffer,
			 OMP_CLAUSE_COLLAPSE_EXPR (clause),
			 spc, flags, false);
      pp_character (buffer, ')');
      break;

    default:
      /* Should never happen.  */
      dump_generic_node (buffer, clause, spc, flags, false);
      break;
    }
}


/* Dump the list of OpenMP clauses.  BUFFER, SPC and FLAGS are as in
   dump_generic_node.  */

void
dump_omp_clauses (pretty_printer *buffer, tree clause, int spc, int flags)
{
  if (clause == NULL)
    return;

  pp_space (buffer);
  while (1)
    {
      dump_omp_clause (buffer, clause, spc, flags);
      clause = OMP_CLAUSE_CHAIN (clause);
      if (clause == NULL)
	return;
      pp_space (buffer);
    }
}


/* Dump location LOC to BUFFER.  */

static void
dump_location (pretty_printer *buffer, location_t loc)
{
  expanded_location xloc = expand_location (loc);

  pp_character (buffer, '[');
  if (xloc.file)
    {
      pp_string (buffer, xloc.file);
      pp_string (buffer, " : ");
    }
  pp_decimal_int (buffer, xloc.line);
  pp_string (buffer, "] ");
}


/* Dump lexical block BLOCK.  BUFFER, SPC and FLAGS are as in
   dump_generic_node.  */

static void
dump_block_node (pretty_printer *buffer, tree block, int spc, int flags)
{
  tree t;

  pp_printf (buffer, "BLOCK #%d ", BLOCK_NUMBER (block));

  if (flags & TDF_ADDRESS)
    pp_printf (buffer, "[%p] ", (void *) block);

  if (BLOCK_ABSTRACT (block))
    pp_string (buffer, "[abstract] ");

  if (TREE_ASM_WRITTEN (block))
    pp_string (buffer, "[written] ");

  if (flags & TDF_SLIM)
    return;

  if (BLOCK_SOURCE_LOCATION (block))
    dump_location (buffer, BLOCK_SOURCE_LOCATION (block));

  newline_and_indent (buffer, spc + 2);

  if (BLOCK_SUPERCONTEXT (block))
    {
      pp_string (buffer, "SUPERCONTEXT: ");
      dump_generic_node (buffer, BLOCK_SUPERCONTEXT (block), 0,
			 flags | TDF_SLIM, false);
      newline_and_indent (buffer, spc + 2);
    }

  if (BLOCK_SUBBLOCKS (block))
    {
      pp_string (buffer, "SUBBLOCKS: ");
      for (t = BLOCK_SUBBLOCKS (block); t; t = BLOCK_CHAIN (t))
	{
	  dump_generic_node (buffer, t, 0, flags | TDF_SLIM, false);
	  pp_string (buffer, " ");
	}
      newline_and_indent (buffer, spc + 2);
    }

  if (BLOCK_CHAIN (block))
    {
      pp_string (buffer, "SIBLINGS: ");
      for (t = BLOCK_CHAIN (block); t; t = BLOCK_CHAIN (t))
	{
	  dump_generic_node (buffer, t, 0, flags | TDF_SLIM, false);
	  pp_string (buffer, " ");
	}
      newline_and_indent (buffer, spc + 2);
    }

  if (BLOCK_VARS (block))
    {
      pp_string (buffer, "VARS: ");
      for (t = BLOCK_VARS (block); t; t = TREE_CHAIN (t))
	{
	  dump_generic_node (buffer, t, 0, flags, false);
	  pp_string (buffer, " ");
	}
      newline_and_indent (buffer, spc + 2);
    }

  if (VEC_length (tree, BLOCK_NONLOCALIZED_VARS (block)) > 0)
    {
      unsigned i;
      VEC(tree,gc) *nlv = BLOCK_NONLOCALIZED_VARS (block);

      pp_string (buffer, "NONLOCALIZED_VARS: ");
      FOR_EACH_VEC_ELT (tree, nlv, i, t)
	{
	  dump_generic_node (buffer, t, 0, flags, false);
	  pp_string (buffer, " ");
	}
      newline_and_indent (buffer, spc + 2);
    }

  if (BLOCK_ABSTRACT_ORIGIN (block))
    {
      pp_string (buffer, "ABSTRACT_ORIGIN: ");
      dump_generic_node (buffer, BLOCK_ABSTRACT_ORIGIN (block), 0,
			 flags | TDF_SLIM, false);
      newline_and_indent (buffer, spc + 2);
    }

  if (BLOCK_FRAGMENT_ORIGIN (block))
    {
      pp_string (buffer, "FRAGMENT_ORIGIN: ");
      dump_generic_node (buffer, BLOCK_FRAGMENT_ORIGIN (block), 0,
			 flags | TDF_SLIM, false);
      newline_and_indent (buffer, spc + 2);
    }

  if (BLOCK_FRAGMENT_CHAIN (block))
    {
      pp_string (buffer, "FRAGMENT_CHAIN: ");
      for (t = BLOCK_FRAGMENT_CHAIN (block); t; t = BLOCK_FRAGMENT_CHAIN (t))
	{
	  dump_generic_node (buffer, t, 0, flags | TDF_SLIM, false);
	  pp_string (buffer, " ");
	}
      newline_and_indent (buffer, spc + 2);
    }
}


/* Dump the node NODE on the pretty_printer BUFFER, SPC spaces of
   indent.  FLAGS specifies details to show in the dump (see TDF_* in
   tree-pass.h).  If IS_STMT is true, the object printed is considered
   to be a statement and it is terminated by ';' if appropriate.  */

int
dump_generic_node (pretty_printer *buffer, tree node, int spc, int flags,
		   bool is_stmt)
{
  tree type;
  tree op0, op1;
  const char *str;
  bool is_expr;

  if (node == NULL_TREE)
    return spc;

  is_expr = EXPR_P (node);

  if (is_stmt && (flags & TDF_STMTADDR))
    pp_printf (buffer, "<&%p> ", (void *)node);

  if ((flags & TDF_LINENO) && EXPR_HAS_LOCATION (node))
    dump_location (buffer, EXPR_LOCATION (node));

  switch (TREE_CODE (node))
    {
    case ERROR_MARK:
      pp_string (buffer, "<<< error >>>");
      break;

    case IDENTIFIER_NODE:
      pp_tree_identifier (buffer, node);
      break;

    case TREE_LIST:
      while (node && node != error_mark_node)
	{
	  if (TREE_PURPOSE (node))
	    {
	      dump_generic_node (buffer, TREE_PURPOSE (node), spc, flags, false);
	      pp_space (buffer);
	    }
	  dump_generic_node (buffer, TREE_VALUE (node), spc, flags, false);
	  node = TREE_CHAIN (node);
	  if (node && TREE_CODE (node) == TREE_LIST)
	    {
	      pp_character (buffer, ',');
	      pp_space (buffer);
	    }
	}
      break;

    case TREE_BINFO:
      dump_generic_node (buffer, BINFO_TYPE (node), spc, flags, false);
      break;

    case TREE_VEC:
      {
	size_t i;
	if (TREE_VEC_LENGTH (node) > 0)
	  {
	    size_t len = TREE_VEC_LENGTH (node);
	    for (i = 0; i < len - 1; i++)
	      {
		dump_generic_node (buffer, TREE_VEC_ELT (node, i), spc, flags,
				   false);
		pp_character (buffer, ',');
		pp_space (buffer);
	      }
	    dump_generic_node (buffer, TREE_VEC_ELT (node, len - 1), spc,
			       flags, false);
	  }
      }
      break;

    case VOID_TYPE:
    case INTEGER_TYPE:
    case REAL_TYPE:
    case FIXED_POINT_TYPE:
    case COMPLEX_TYPE:
    case VECTOR_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
      {
	unsigned int quals = TYPE_QUALS (node);
	enum tree_code_class tclass;

	if (quals & TYPE_QUAL_CONST)
	  pp_string (buffer, "const ");
	else if (quals & TYPE_QUAL_VOLATILE)
	  pp_string (buffer, "volatile ");
	else if (quals & TYPE_QUAL_RESTRICT)
	  pp_string (buffer, "restrict ");

	if (!ADDR_SPACE_GENERIC_P (TYPE_ADDR_SPACE (node)))
	  {
	    pp_string (buffer, "<address-space-");
	    pp_decimal_int (buffer, TYPE_ADDR_SPACE (node));
	    pp_string (buffer, "> ");
	  }

	tclass = TREE_CODE_CLASS (TREE_CODE (node));

	if (tclass == tcc_declaration)
	  {
	    if (DECL_NAME (node))
	      dump_decl_name (buffer, node, flags);
	    else
              pp_string (buffer, "<unnamed type decl>");
	  }
	else if (tclass == tcc_type)
	  {
	    if (TYPE_NAME (node))
	      {
		if (TREE_CODE (TYPE_NAME (node)) == IDENTIFIER_NODE)
		  pp_tree_identifier (buffer, TYPE_NAME (node));
		else if (TREE_CODE (TYPE_NAME (node)) == TYPE_DECL
			 && DECL_NAME (TYPE_NAME (node)))
		  dump_decl_name (buffer, TYPE_NAME (node), flags);
		else
		  pp_string (buffer, "<unnamed type>");
	      }
	    else if (TREE_CODE (node) == VECTOR_TYPE)
	      {
		pp_string (buffer, "vector");
		pp_character (buffer, '(');
		pp_wide_integer (buffer, TYPE_VECTOR_SUBPARTS (node));
		pp_string (buffer, ") ");
		dump_generic_node (buffer, TREE_TYPE (node), spc, flags, false);
	      }
	    else if (TREE_CODE (node) == INTEGER_TYPE)
	      {
		pp_string (buffer, (TYPE_UNSIGNED (node)
				    ? "<unnamed-unsigned:"
				    : "<unnamed-signed:"));
		pp_decimal_int (buffer, TYPE_PRECISION (node));
		pp_string (buffer, ">");
	      }
	    else if (TREE_CODE (node) == COMPLEX_TYPE)
	      {
		pp_string (buffer, "__complex__ ");
		dump_generic_node (buffer, TREE_TYPE (node), spc, flags, false);
	      }
	    else if (TREE_CODE (node) == REAL_TYPE)
	      {
		pp_string (buffer, "<float:");
		pp_decimal_int (buffer, TYPE_PRECISION (node));
		pp_string (buffer, ">");
	      }
	    else if (TREE_CODE (node) == FIXED_POINT_TYPE)
	      {
		pp_string (buffer, "<fixed-point-");
		pp_string (buffer, TYPE_SATURATING (node) ? "sat:" : "nonsat:");
		pp_decimal_int (buffer, TYPE_PRECISION (node));
		pp_string (buffer, ">");
	      }
	    else if (TREE_CODE (node) == VOID_TYPE)
	      pp_string (buffer, "void");
	    else
              pp_string (buffer, "<unnamed type>");
	  }
	break;
      }

    case POINTER_TYPE:
    case REFERENCE_TYPE:
      str = (TREE_CODE (node) == POINTER_TYPE ? "*" : "&");

      if (TREE_TYPE (node) == NULL)
        {
	  pp_string (buffer, str);
          pp_string (buffer, "<null type>");
        }
      else if (TREE_CODE (TREE_TYPE (node)) == FUNCTION_TYPE)
        {
	  tree fnode = TREE_TYPE (node);

	  dump_generic_node (buffer, TREE_TYPE (fnode), spc, flags, false);
	  pp_space (buffer);
	  pp_character (buffer, '(');
	  pp_string (buffer, str);
	  if (TYPE_NAME (node) && DECL_NAME (TYPE_NAME (node)))
	    dump_decl_name (buffer, TYPE_NAME (node), flags);
	  else if (flags & TDF_NOUID)
	    pp_printf (buffer, "<Txxxx>");
	  else
	    pp_printf (buffer, "<T%x>", TYPE_UID (node));

	  pp_character (buffer, ')');
	  dump_function_declaration (buffer, fnode, spc, flags);
	}
      else
        {
	  unsigned int quals = TYPE_QUALS (node);

          dump_generic_node (buffer, TREE_TYPE (node), spc, flags, false);
	  pp_space (buffer);
	  pp_string (buffer, str);

	  if (quals & TYPE_QUAL_CONST)
	    pp_string (buffer, " const");
	  if (quals & TYPE_QUAL_VOLATILE)
	    pp_string (buffer, " volatile");
	  if (quals & TYPE_QUAL_RESTRICT)
	    pp_string (buffer, " restrict");

	  if (!ADDR_SPACE_GENERIC_P (TYPE_ADDR_SPACE (node)))
	    {
	      pp_string (buffer, " <address-space-");
	      pp_decimal_int (buffer, TYPE_ADDR_SPACE (node));
	      pp_string (buffer, ">");
	    }

	  if (TYPE_REF_CAN_ALIAS_ALL (node))
	    pp_string (buffer, " {ref-all}");
	}
      break;

    case OFFSET_TYPE:
      NIY;
      break;

    case MEM_REF:
      {
	if (integer_zerop (TREE_OPERAND (node, 1))
	    /* Dump the types of INTEGER_CSTs explicitly, for we can't
	       infer them and MEM_ATTR caching will share MEM_REFs
	       with differently-typed op0s.  */
	    && TREE_CODE (TREE_OPERAND (node, 0)) != INTEGER_CST
	    /* Same pointer types, but ignoring POINTER_TYPE vs.
	       REFERENCE_TYPE.  */
	    && (TREE_TYPE (TREE_TYPE (TREE_OPERAND (node, 0)))
		== TREE_TYPE (TREE_TYPE (TREE_OPERAND (node, 1))))
	    && (TYPE_MODE (TREE_TYPE (TREE_OPERAND (node, 0)))
		== TYPE_MODE (TREE_TYPE (TREE_OPERAND (node, 1))))
	    && (TYPE_REF_CAN_ALIAS_ALL (TREE_TYPE (TREE_OPERAND (node, 0)))
		== TYPE_REF_CAN_ALIAS_ALL (TREE_TYPE (TREE_OPERAND (node, 1))))
	    /* Same value types ignoring qualifiers.  */
	    && (TYPE_MAIN_VARIANT (TREE_TYPE (node))
		== TYPE_MAIN_VARIANT
		    (TREE_TYPE (TREE_TYPE (TREE_OPERAND (node, 1))))))
	  {
	    if (TREE_CODE (TREE_OPERAND (node, 0)) != ADDR_EXPR)
	      {
		pp_string (buffer, "*");
		dump_generic_node (buffer, TREE_OPERAND (node, 0),
				   spc, flags, false);
	      }
	    else
	      dump_generic_node (buffer,
				 TREE_OPERAND (TREE_OPERAND (node, 0), 0),
				 spc, flags, false);
	  }
	else
	  {
	    tree ptype;

	    pp_string (buffer, "MEM[");
	    pp_string (buffer, "(");
	    ptype = TYPE_MAIN_VARIANT (TREE_TYPE (TREE_OPERAND (node, 1)));
	    dump_generic_node (buffer, ptype,
			       spc, flags | TDF_SLIM, false);
	    pp_string (buffer, ")");
	    dump_generic_node (buffer, TREE_OPERAND (node, 0),
			       spc, flags, false);
	    if (!integer_zerop (TREE_OPERAND (node, 1)))
	      {
		pp_string (buffer, " + ");
		dump_generic_node (buffer, TREE_OPERAND (node, 1),
				   spc, flags, false);
	      }
	    pp_string (buffer, "]");
	  }
	break;
      }

    case TARGET_MEM_REF:
      {
	const char *sep = "";
	tree tmp;

	pp_string (buffer, "MEM[");

	if (TREE_CODE (TMR_BASE (node)) == ADDR_EXPR)
	  {
	    pp_string (buffer, sep);
	    sep = ", ";
	    pp_string (buffer, "symbol: ");
	    dump_generic_node (buffer, TREE_OPERAND (TMR_BASE (node), 0),
			       spc, flags, false);
	  }
	else
	  {
	    pp_string (buffer, sep);
	    sep = ", ";
	    pp_string (buffer, "base: ");
	    dump_generic_node (buffer, TMR_BASE (node), spc, flags, false);
	  }
	tmp = TMR_INDEX2 (node);
	if (tmp)
	  {
	    pp_string (buffer, sep);
	    sep = ", ";
	    pp_string (buffer, "base: ");
	    dump_generic_node (buffer, tmp, spc, flags, false);
	  }
	tmp = TMR_INDEX (node);
	if (tmp)
	  {
	    pp_string (buffer, sep);
	    sep = ", ";
	    pp_string (buffer, "index: ");
	    dump_generic_node (buffer, tmp, spc, flags, false);
	  }
	tmp = TMR_STEP (node);
	if (tmp)
	  {
	    pp_string (buffer, sep);
	    sep = ", ";
	    pp_string (buffer, "step: ");
	    dump_generic_node (buffer, tmp, spc, flags, false);
	  }
	tmp = TMR_OFFSET (node);
	if (tmp)
	  {
	    pp_string (buffer, sep);
	    sep = ", ";
	    pp_string (buffer, "offset: ");
	    dump_generic_node (buffer, tmp, spc, flags, false);
	  }
	pp_string (buffer, "]");
      }
      break;

    case ARRAY_TYPE:
      {
	tree tmp;

	/* Print the innermost component type.  */
	for (tmp = TREE_TYPE (node); TREE_CODE (tmp) == ARRAY_TYPE;
	     tmp = TREE_TYPE (tmp))
	  ;
	dump_generic_node (buffer, tmp, spc, flags, false);

	/* Print the dimensions.  */
	for (tmp = node; TREE_CODE (tmp) == ARRAY_TYPE; tmp = TREE_TYPE (tmp))
	  dump_array_domain (buffer, TYPE_DOMAIN (tmp), spc, flags);
	break;
      }

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      {
	unsigned int quals = TYPE_QUALS (node);

	if (quals & TYPE_QUAL_CONST)
	  pp_string (buffer, "const ");
	if (quals & TYPE_QUAL_VOLATILE)
	  pp_string (buffer, "volatile ");

        /* Print the name of the structure.  */
        if (TREE_CODE (node) == RECORD_TYPE)
	  pp_string (buffer, "struct ");
        else if (TREE_CODE (node) == UNION_TYPE)
	  pp_string (buffer, "union ");

        if (TYPE_NAME (node))
	  dump_generic_node (buffer, TYPE_NAME (node), spc, flags, false);
	else if (!(flags & TDF_SLIM))
	  /* FIXME: If we eliminate the 'else' above and attempt
	     to show the fields for named types, we may get stuck
	     following a cycle of pointers to structs.  The alleged
	     self-reference check in print_struct_decl will not detect
	     cycles involving more than one pointer or struct type.  */
	  print_struct_decl (buffer, node, spc, flags);
        break;
      }

    case LANG_TYPE:
      NIY;
      break;

    case INTEGER_CST:
      if (TREE_CODE (TREE_TYPE (node)) == POINTER_TYPE)
	{
	  /* In the case of a pointer, one may want to divide by the
	     size of the pointed-to type.  Unfortunately, this not
	     straightforward.  The C front-end maps expressions

	     (int *) 5
	     int *p; (p + 5)

	     in such a way that the two INTEGER_CST nodes for "5" have
	     different values but identical types.  In the latter
	     case, the 5 is multiplied by sizeof (int) in c-common.c
	     (pointer_int_sum) to convert it to a byte address, and
	     yet the type of the node is left unchanged.  Argh.  What
	     is consistent though is that the number value corresponds
	     to bytes (UNITS) offset.

             NB: Neither of the following divisors can be trivially
             used to recover the original literal:

             TREE_INT_CST_LOW (TYPE_SIZE_UNIT (TREE_TYPE (node)))
	     TYPE_PRECISION (TREE_TYPE (TREE_TYPE (node)))  */
	  pp_wide_integer (buffer, TREE_INT_CST_LOW (node));
	  pp_string (buffer, "B"); /* pseudo-unit */
	}
      else if (! host_integerp (node, 0))
	{
	  tree val = node;
	  unsigned HOST_WIDE_INT low = TREE_INT_CST_LOW (val);
	  HOST_WIDE_INT high = TREE_INT_CST_HIGH (val);

	  if (tree_int_cst_sgn (val) < 0)
	    {
	      pp_character (buffer, '-');
	      high = ~high + !low;
	      low = -low;
	    }
	  /* Would "%x%0*x" or "%x%*0x" get zero-padding on all
	     systems?  */
	  sprintf (pp_buffer (buffer)->digit_buffer,
		   HOST_WIDE_INT_PRINT_DOUBLE_HEX,
		   (unsigned HOST_WIDE_INT) high, low);
	  pp_string (buffer, pp_buffer (buffer)->digit_buffer);
	}
      else
	pp_wide_integer (buffer, TREE_INT_CST_LOW (node));
      break;

    case REAL_CST:
      /* Code copied from print_node.  */
      {
	REAL_VALUE_TYPE d;
	if (TREE_OVERFLOW (node))
	  pp_string (buffer, " overflow");

#if !defined(REAL_IS_NOT_DOUBLE) || defined(REAL_ARITHMETIC)
	d = TREE_REAL_CST (node);
	if (REAL_VALUE_ISINF (d))
	  pp_string (buffer, REAL_VALUE_NEGATIVE (d) ? " -Inf" : " Inf");
	else if (REAL_VALUE_ISNAN (d))
	  pp_string (buffer, " Nan");
	else
	  {
	    char string[100];
	    real_to_decimal (string, &d, sizeof (string), 0, 1);
	    pp_string (buffer, string);
	  }
#else
	{
	  HOST_WIDE_INT i;
	  unsigned char *p = (unsigned char *) &TREE_REAL_CST (node);
	  pp_string (buffer, "0x");
	  for (i = 0; i < sizeof TREE_REAL_CST (node); i++)
	    output_formatted_integer (buffer, "%02x", *p++);
	}
#endif
	break;
      }

    case FIXED_CST:
      {
	char string[100];
	fixed_to_decimal (string, TREE_FIXED_CST_PTR (node), sizeof (string));
	pp_string (buffer, string);
	break;
      }

    case COMPLEX_CST:
      pp_string (buffer, "__complex__ (");
      dump_generic_node (buffer, TREE_REALPART (node), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, TREE_IMAGPART (node), spc, flags, false);
      pp_string (buffer, ")");
      break;

    case STRING_CST:
      pp_string (buffer, "\"");
      pretty_print_string (buffer, TREE_STRING_POINTER (node));
      pp_string (buffer, "\"");
      break;

    case VECTOR_CST:
      {
	tree elt;
	pp_string (buffer, "{ ");
	for (elt = TREE_VECTOR_CST_ELTS (node); elt; elt = TREE_CHAIN (elt))
	  {
	    dump_generic_node (buffer, TREE_VALUE (elt), spc, flags, false);
	    if (TREE_CHAIN (elt))
	      pp_string (buffer, ", ");
	  }
	pp_string (buffer, " }");
      }
      break;

    case FUNCTION_TYPE:
    case METHOD_TYPE:
      dump_generic_node (buffer, TREE_TYPE (node), spc, flags, false);
      pp_space (buffer);
      if (TREE_CODE (node) == METHOD_TYPE)
	{
	  if (TYPE_METHOD_BASETYPE (node))
	    dump_decl_name (buffer, TYPE_NAME (TYPE_METHOD_BASETYPE (node)),
			    flags);
	  else
	    pp_string (buffer, "<null method basetype>");
	  pp_string (buffer, "::");
	}
      if (TYPE_NAME (node) && DECL_NAME (TYPE_NAME (node)))
	dump_decl_name (buffer, TYPE_NAME (node), flags);
      else if (flags & TDF_NOUID)
	pp_printf (buffer, "<Txxxx>");
      else
	pp_printf (buffer, "<T%x>", TYPE_UID (node));
      dump_function_declaration (buffer, node, spc, flags);
      break;

    case FUNCTION_DECL:
    case CONST_DECL:
      dump_decl_name (buffer, node, flags);
      break;

    case LABEL_DECL:
      if (DECL_NAME (node))
	dump_decl_name (buffer, node, flags);
      else if (LABEL_DECL_UID (node) != -1)
	pp_printf (buffer, "<L%d>", (int) LABEL_DECL_UID (node));
      else
	{
	  if (flags & TDF_NOUID)
	    pp_string (buffer, "<D.xxxx>");
	  else
	    pp_printf (buffer, "<D.%u>", DECL_UID (node));
	}
      break;

    case TYPE_DECL:
      if (DECL_IS_BUILTIN (node))
	{
	  /* Don't print the declaration of built-in types.  */
	  break;
	}
      if (DECL_NAME (node))
	dump_decl_name (buffer, node, flags);
      else if (TYPE_NAME (TREE_TYPE (node)) != node)
	{
	  if ((TREE_CODE (TREE_TYPE (node)) == RECORD_TYPE
	       || TREE_CODE (TREE_TYPE (node)) == UNION_TYPE)
	      && TYPE_METHODS (TREE_TYPE (node)))
	    {
	      /* The type is a c++ class: all structures have at least
		 4 methods.  */
	      pp_string (buffer, "class ");
	      dump_generic_node (buffer, TREE_TYPE (node), spc, flags, false);
	    }
	  else
	    {
	      pp_string (buffer,
			 (TREE_CODE (TREE_TYPE (node)) == UNION_TYPE
			  ? "union" : "struct "));
	      dump_generic_node (buffer, TREE_TYPE (node), spc, flags, false);
	    }
	}
      else
	pp_string (buffer, "<anon>");
      break;

    case VAR_DECL:
    case PARM_DECL:
    case FIELD_DECL:
    case DEBUG_EXPR_DECL:
    case NAMESPACE_DECL:
      dump_decl_name (buffer, node, flags);
      break;

    case RESULT_DECL:
      pp_string (buffer, "<retval>");
      break;

    case COMPONENT_REF:
      op0 = TREE_OPERAND (node, 0);
      str = ".";
      if (op0
	  && (TREE_CODE (op0) == INDIRECT_REF
	      || (TREE_CODE (op0) == MEM_REF
		  && TREE_CODE (TREE_OPERAND (op0, 0)) != ADDR_EXPR
		  && integer_zerop (TREE_OPERAND (op0, 1))
		  /* Dump the types of INTEGER_CSTs explicitly, for we
		     can't infer them and MEM_ATTR caching will share
		     MEM_REFs with differently-typed op0s.  */
		  && TREE_CODE (TREE_OPERAND (op0, 0)) != INTEGER_CST
		  /* Same pointer types, but ignoring POINTER_TYPE vs.
		     REFERENCE_TYPE.  */
		  && (TREE_TYPE (TREE_TYPE (TREE_OPERAND (op0, 0)))
		      == TREE_TYPE (TREE_TYPE (TREE_OPERAND (op0, 1))))
		  && (TYPE_MODE (TREE_TYPE (TREE_OPERAND (op0, 0)))
		      == TYPE_MODE (TREE_TYPE (TREE_OPERAND (op0, 1))))
		  && (TYPE_REF_CAN_ALIAS_ALL (TREE_TYPE (TREE_OPERAND (op0, 0)))
		      == TYPE_REF_CAN_ALIAS_ALL (TREE_TYPE (TREE_OPERAND (op0, 1))))
		  /* Same value types ignoring qualifiers.  */
		  && (TYPE_MAIN_VARIANT (TREE_TYPE (op0))
		      == TYPE_MAIN_VARIANT
		          (TREE_TYPE (TREE_TYPE (TREE_OPERAND (op0, 1))))))))
	{
	  op0 = TREE_OPERAND (op0, 0);
	  str = "->";
	}
      if (op_prio (op0) < op_prio (node))
	pp_character (buffer, '(');
      dump_generic_node (buffer, op0, spc, flags, false);
      if (op_prio (op0) < op_prio (node))
	pp_character (buffer, ')');
      pp_string (buffer, str);
      dump_generic_node (buffer, TREE_OPERAND (node, 1), spc, flags, false);
      op0 = component_ref_field_offset (node);
      if (op0 && TREE_CODE (op0) != INTEGER_CST)
	{
	  pp_string (buffer, "{off: ");
	      dump_generic_node (buffer, op0, spc, flags, false);
	      pp_character (buffer, '}');
	}
      break;

    case BIT_FIELD_REF:
      pp_string (buffer, "BIT_FIELD_REF <");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, TREE_OPERAND (node, 2), spc, flags, false);
      pp_string (buffer, ">");
      break;

    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      op0 = TREE_OPERAND (node, 0);
      if (op_prio (op0) < op_prio (node))
	pp_character (buffer, '(');
      dump_generic_node (buffer, op0, spc, flags, false);
      if (op_prio (op0) < op_prio (node))
	pp_character (buffer, ')');
      pp_character (buffer, '[');
      dump_generic_node (buffer, TREE_OPERAND (node, 1), spc, flags, false);
      if (TREE_CODE (node) == ARRAY_RANGE_REF)
	pp_string (buffer, " ...");
      pp_character (buffer, ']');

      op0 = array_ref_low_bound (node);
      op1 = array_ref_element_size (node);

      if (!integer_zerop (op0)
	  || TREE_OPERAND (node, 2)
	  || TREE_OPERAND (node, 3))
	{
	  pp_string (buffer, "{lb: ");
	  dump_generic_node (buffer, op0, spc, flags, false);
	  pp_string (buffer, " sz: ");
	  dump_generic_node (buffer, op1, spc, flags, false);
	  pp_character (buffer, '}');
	}
      break;

    case CONSTRUCTOR:
      {
	unsigned HOST_WIDE_INT ix;
	tree field, val;
	bool is_struct_init = FALSE;
	pp_character (buffer, '{');
	if (TREE_CODE (TREE_TYPE (node)) == RECORD_TYPE
	    || TREE_CODE (TREE_TYPE (node)) == UNION_TYPE)
	  is_struct_init = TRUE;
	FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (node), ix, field, val)
	  {
	    if (field && is_struct_init)
	      {
		pp_character (buffer, '.');
		dump_generic_node (buffer, field, spc, flags, false);
		pp_string (buffer, "=");
	      }
	    if (val && TREE_CODE (val) == ADDR_EXPR)
	      if (TREE_CODE (TREE_OPERAND (val, 0)) == FUNCTION_DECL)
		val = TREE_OPERAND (val, 0);
	    if (val && TREE_CODE (val) == FUNCTION_DECL)
		dump_decl_name (buffer, val, flags);
	    else
		dump_generic_node (buffer, val, spc, flags, false);
	    if (ix != VEC_length (constructor_elt, CONSTRUCTOR_ELTS (node)) - 1)
	      {
		pp_character (buffer, ',');
		pp_space (buffer);
	      }
	  }
	pp_character (buffer, '}');
      }
      break;

    case COMPOUND_EXPR:
      {
	tree *tp;
	if (flags & TDF_SLIM)
	  {
	    pp_string (buffer, "<COMPOUND_EXPR>");
	    break;
	  }

	dump_generic_node (buffer, TREE_OPERAND (node, 0),
			   spc, flags, !(flags & TDF_SLIM));
	if (flags & TDF_SLIM)
	  newline_and_indent (buffer, spc);
	else
	  {
	    pp_character (buffer, ',');
	    pp_space (buffer);
	  }

	for (tp = &TREE_OPERAND (node, 1);
	     TREE_CODE (*tp) == COMPOUND_EXPR;
	     tp = &TREE_OPERAND (*tp, 1))
	  {
	    dump_generic_node (buffer, TREE_OPERAND (*tp, 0),
			       spc, flags, !(flags & TDF_SLIM));
	    if (flags & TDF_SLIM)
	      newline_and_indent (buffer, spc);
	    else
	      {
	        pp_character (buffer, ',');
	        pp_space (buffer);
	      }
	  }

	dump_generic_node (buffer, *tp, spc, flags, !(flags & TDF_SLIM));
      }
      break;

    case STATEMENT_LIST:
      {
	tree_stmt_iterator si;
	bool first = true;

	if (flags & TDF_SLIM)
	  {
	    pp_string (buffer, "<STATEMENT_LIST>");
	    break;
	  }

	for (si = tsi_start (node); !tsi_end_p (si); tsi_next (&si))
	  {
	    if (!first)
	      newline_and_indent (buffer, spc);
	    else
	      first = false;
	    dump_generic_node (buffer, tsi_stmt (si), spc, flags, true);
	  }
      }
      break;

    case MODIFY_EXPR:
    case INIT_EXPR:
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags,
	  		 false);
      pp_space (buffer);
      pp_character (buffer, '=');
      if (TREE_CODE (node) == MODIFY_EXPR
	  && MOVE_NONTEMPORAL (node))
	pp_string (buffer, "{nt}");
      pp_space (buffer);
      dump_generic_node (buffer, TREE_OPERAND (node, 1), spc, flags,
	  		 false);
      break;

    case TARGET_EXPR:
      pp_string (buffer, "TARGET_EXPR <");
      dump_generic_node (buffer, TARGET_EXPR_SLOT (node), spc, flags, false);
      pp_character (buffer, ',');
      pp_space (buffer);
      dump_generic_node (buffer, TARGET_EXPR_INITIAL (node), spc, flags, false);
      pp_character (buffer, '>');
      break;

    case DECL_EXPR:
      print_declaration (buffer, DECL_EXPR_DECL (node), spc, flags);
      is_stmt = false;
      break;

    case COND_EXPR:
      if (TREE_TYPE (node) == NULL || TREE_TYPE (node) == void_type_node)
	{
	  pp_string (buffer, "if (");
	  dump_generic_node (buffer, COND_EXPR_COND (node), spc, flags, false);
	  pp_character (buffer, ')');
	  /* The lowered cond_exprs should always be printed in full.  */
	  if (COND_EXPR_THEN (node)
	      && (IS_EMPTY_STMT (COND_EXPR_THEN (node))
		  || TREE_CODE (COND_EXPR_THEN (node)) == GOTO_EXPR)
	      && COND_EXPR_ELSE (node)
	      && (IS_EMPTY_STMT (COND_EXPR_ELSE (node))
		  || TREE_CODE (COND_EXPR_ELSE (node)) == GOTO_EXPR))
	    {
	      pp_space (buffer);
	      dump_generic_node (buffer, COND_EXPR_THEN (node),
				 0, flags, true);
	      if (!IS_EMPTY_STMT (COND_EXPR_ELSE (node)))
		{
		  pp_string (buffer, " else ");
		  dump_generic_node (buffer, COND_EXPR_ELSE (node),
				     0, flags, true);
		}
	    }
	  else if (!(flags & TDF_SLIM))
	    {
	      /* Output COND_EXPR_THEN.  */
	      if (COND_EXPR_THEN (node))
		{
		  newline_and_indent (buffer, spc+2);
		  pp_character (buffer, '{');
		  newline_and_indent (buffer, spc+4);
		  dump_generic_node (buffer, COND_EXPR_THEN (node), spc+4,
				     flags, true);
		  newline_and_indent (buffer, spc+2);
		  pp_character (buffer, '}');
		}

	      /* Output COND_EXPR_ELSE.  */
	      if (COND_EXPR_ELSE (node)
		  && !IS_EMPTY_STMT (COND_EXPR_ELSE (node)))
		{
		  newline_and_indent (buffer, spc);
		  pp_string (buffer, "else");
		  newline_and_indent (buffer, spc+2);
		  pp_character (buffer, '{');
		  newline_and_indent (buffer, spc+4);
		  dump_generic_node (buffer, COND_EXPR_ELSE (node), spc+4,
			             flags, true);
		  newline_and_indent (buffer, spc+2);
		  pp_character (buffer, '}');
		}
	    }
	  is_expr = false;
	}
      else
	{
	  dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
	  pp_space (buffer);
	  pp_character (buffer, '?');
	  pp_space (buffer);
	  dump_generic_node (buffer, TREE_OPERAND (node, 1), spc, flags, false);
	  pp_space (buffer);
	  pp_character (buffer, ':');
	  pp_space (buffer);
	  dump_generic_node (buffer, TREE_OPERAND (node, 2), spc, flags, false);
	}
      break;

    case BIND_EXPR:
      pp_character (buffer, '{');
      if (!(flags & TDF_SLIM))
	{
	  if (BIND_EXPR_VARS (node))
	    {
	      pp_newline (buffer);

	      for (op0 = BIND_EXPR_VARS (node); op0; op0 = DECL_CHAIN (op0))
		{
		  print_declaration (buffer, op0, spc+2, flags);
		  pp_newline (buffer);
		}
	    }

	  newline_and_indent (buffer, spc+2);
	  dump_generic_node (buffer, BIND_EXPR_BODY (node), spc+2, flags, true);
	  newline_and_indent (buffer, spc);
	  pp_character (buffer, '}');
	}
      is_expr = false;
      break;

    case CALL_EXPR:
      print_call_name (buffer, CALL_EXPR_FN (node), flags);

      /* Print parameters.  */
      pp_space (buffer);
      pp_character (buffer, '(');
      {
	tree arg;
	call_expr_arg_iterator iter;
	FOR_EACH_CALL_EXPR_ARG (arg, iter, node)
	  {
	    dump_generic_node (buffer, arg, spc, flags, false);
	    if (more_call_expr_args_p (&iter))
	      {
		pp_character (buffer, ',');
		pp_space (buffer);
	      }
	  }
      }
      if (CALL_EXPR_VA_ARG_PACK (node))
	{
	  if (call_expr_nargs (node) > 0)
	    {
	      pp_character (buffer, ',');
	      pp_space (buffer);
	    }
	  pp_string (buffer, "__builtin_va_arg_pack ()");
	}
      pp_character (buffer, ')');

      op1 = CALL_EXPR_STATIC_CHAIN (node);
      if (op1)
	{
	  pp_string (buffer, " [static-chain: ");
	  dump_generic_node (buffer, op1, spc, flags, false);
	  pp_character (buffer, ']');
	}

      if (CALL_EXPR_RETURN_SLOT_OPT (node))
	pp_string (buffer, " [return slot optimization]");
      if (CALL_EXPR_TAILCALL (node))
	pp_string (buffer, " [tail call]");
      break;

    case WITH_CLEANUP_EXPR:
      NIY;
      break;

    case CLEANUP_POINT_EXPR:
      pp_string (buffer, "<<cleanup_point ");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, ">>");
      break;

    case PLACEHOLDER_EXPR:
      pp_string (buffer, "<PLACEHOLDER_EXPR ");
      dump_generic_node (buffer, TREE_TYPE (node), spc, flags, false);
      pp_character (buffer, '>');
      break;

      /* Binary arithmetic and logic expressions.  */
    case WIDEN_SUM_EXPR:
    case WIDEN_MULT_EXPR:
    case MULT_EXPR:
    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
    case MINUS_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case RDIV_EXPR:
    case EXACT_DIV_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
    case VEC_LSHIFT_EXPR:
    case VEC_RSHIFT_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
    case LTGT_EXPR:
    case ORDERED_EXPR:
    case UNORDERED_EXPR:
      {
	const char *op = op_symbol (node);
	op0 = TREE_OPERAND (node, 0);
	op1 = TREE_OPERAND (node, 1);

	/* When the operands are expressions with less priority,
	   keep semantics of the tree representation.  */
	if (op_prio (op0) <= op_prio (node))
	  {
	    pp_character (buffer, '(');
	    dump_generic_node (buffer, op0, spc, flags, false);
	    pp_character (buffer, ')');
	  }
	else
	  dump_generic_node (buffer, op0, spc, flags, false);

	pp_space (buffer);
	pp_string (buffer, op);
	pp_space (buffer);

	/* When the operands are expressions with less priority,
	   keep semantics of the tree representation.  */
	if (op_prio (op1) <= op_prio (node))
	  {
	    pp_character (buffer, '(');
	    dump_generic_node (buffer, op1, spc, flags, false);
	    pp_character (buffer, ')');
	  }
	else
	  dump_generic_node (buffer, op1, spc, flags, false);
      }
      break;

      /* Unary arithmetic and logic expressions.  */
    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_NOT_EXPR:
    case ADDR_EXPR:
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case INDIRECT_REF:
      if (TREE_CODE (node) == ADDR_EXPR
	  && (TREE_CODE (TREE_OPERAND (node, 0)) == STRING_CST
	      || TREE_CODE (TREE_OPERAND (node, 0)) == FUNCTION_DECL))
	;	/* Do not output '&' for strings and function pointers.  */
      else
	pp_string (buffer, op_symbol (node));

      if (op_prio (TREE_OPERAND (node, 0)) < op_prio (node))
	{
	  pp_character (buffer, '(');
	  dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
	  pp_character (buffer, ')');
	}
      else
	dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      break;

    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
      if (op_prio (TREE_OPERAND (node, 0)) < op_prio (node))
	{
	  pp_character (buffer, '(');
	  dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
	  pp_character (buffer, ')');
	}
      else
	dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, op_symbol (node));
      break;

    case MIN_EXPR:
      pp_string (buffer, "MIN_EXPR <");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, TREE_OPERAND (node, 1), spc, flags, false);
      pp_character (buffer, '>');
      break;

    case MAX_EXPR:
      pp_string (buffer, "MAX_EXPR <");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, TREE_OPERAND (node, 1), spc, flags, false);
      pp_character (buffer, '>');
      break;

    case ABS_EXPR:
      pp_string (buffer, "ABS_EXPR <");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_character (buffer, '>');
      break;

    case RANGE_EXPR:
      NIY;
      break;

    case ADDR_SPACE_CONVERT_EXPR:
    case FIXED_CONVERT_EXPR:
    case FIX_TRUNC_EXPR:
    case FLOAT_EXPR:
    CASE_CONVERT:
      type = TREE_TYPE (node);
      op0 = TREE_OPERAND (node, 0);
      if (type != TREE_TYPE (op0))
	{
	  pp_character (buffer, '(');
	  dump_generic_node (buffer, type, spc, flags, false);
	  pp_string (buffer, ") ");
	}
      if (op_prio (op0) < op_prio (node))
	pp_character (buffer, '(');
      dump_generic_node (buffer, op0, spc, flags, false);
      if (op_prio (op0) < op_prio (node))
	pp_character (buffer, ')');
      break;

    case VIEW_CONVERT_EXPR:
      pp_string (buffer, "VIEW_CONVERT_EXPR<");
      dump_generic_node (buffer, TREE_TYPE (node), spc, flags, false);
      pp_string (buffer, ">(");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_character (buffer, ')');
      break;

    case PAREN_EXPR:
      pp_string (buffer, "((");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, "))");
      break;

    case NON_LVALUE_EXPR:
      pp_string (buffer, "NON_LVALUE_EXPR <");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_character (buffer, '>');
      break;

    case SAVE_EXPR:
      pp_string (buffer, "SAVE_EXPR <");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_character (buffer, '>');
      break;

    case COMPLEX_EXPR:
      pp_string (buffer, "COMPLEX_EXPR <");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (buffer, ">");
      break;

    case CONJ_EXPR:
      pp_string (buffer, "CONJ_EXPR <");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, ">");
      break;

    case REALPART_EXPR:
      pp_string (buffer, "REALPART_EXPR <");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, ">");
      break;

    case IMAGPART_EXPR:
      pp_string (buffer, "IMAGPART_EXPR <");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, ">");
      break;

    case VA_ARG_EXPR:
      pp_string (buffer, "VA_ARG_EXPR <");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, ">");
      break;

    case TRY_FINALLY_EXPR:
    case TRY_CATCH_EXPR:
      pp_string (buffer, "try");
      newline_and_indent (buffer, spc+2);
      pp_string (buffer, "{");
      newline_and_indent (buffer, spc+4);
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc+4, flags, true);
      newline_and_indent (buffer, spc+2);
      pp_string (buffer, "}");
      newline_and_indent (buffer, spc);
      pp_string (buffer,
			 (TREE_CODE (node) == TRY_CATCH_EXPR) ? "catch" : "finally");
      newline_and_indent (buffer, spc+2);
      pp_string (buffer, "{");
      newline_and_indent (buffer, spc+4);
      dump_generic_node (buffer, TREE_OPERAND (node, 1), spc+4, flags, true);
      newline_and_indent (buffer, spc+2);
      pp_string (buffer, "}");
      is_expr = false;
      break;

    case CATCH_EXPR:
      pp_string (buffer, "catch (");
      dump_generic_node (buffer, CATCH_TYPES (node), spc+2, flags, false);
      pp_string (buffer, ")");
      newline_and_indent (buffer, spc+2);
      pp_string (buffer, "{");
      newline_and_indent (buffer, spc+4);
      dump_generic_node (buffer, CATCH_BODY (node), spc+4, flags, true);
      newline_and_indent (buffer, spc+2);
      pp_string (buffer, "}");
      is_expr = false;
      break;

    case EH_FILTER_EXPR:
      pp_string (buffer, "<<<eh_filter (");
      dump_generic_node (buffer, EH_FILTER_TYPES (node), spc+2, flags, false);
      pp_string (buffer, ")>>>");
      newline_and_indent (buffer, spc+2);
      pp_string (buffer, "{");
      newline_and_indent (buffer, spc+4);
      dump_generic_node (buffer, EH_FILTER_FAILURE (node), spc+4, flags, true);
      newline_and_indent (buffer, spc+2);
      pp_string (buffer, "}");
      is_expr = false;
      break;

    case LABEL_EXPR:
      op0 = TREE_OPERAND (node, 0);
      /* If this is for break or continue, don't bother printing it.  */
      if (DECL_NAME (op0))
	{
	  const char *name = IDENTIFIER_POINTER (DECL_NAME (op0));
	  if (strcmp (name, "break") == 0
	      || strcmp (name, "continue") == 0)
	    break;
	}
      dump_generic_node (buffer, op0, spc, flags, false);
      pp_character (buffer, ':');
      if (DECL_NONLOCAL (op0))
	pp_string (buffer, " [non-local]");
      break;

    case LOOP_EXPR:
      pp_string (buffer, "while (1)");
      if (!(flags & TDF_SLIM))
	{
	  newline_and_indent (buffer, spc+2);
	  pp_character (buffer, '{');
	  newline_and_indent (buffer, spc+4);
	  dump_generic_node (buffer, LOOP_EXPR_BODY (node), spc+4, flags, true);
	  newline_and_indent (buffer, spc+2);
	  pp_character (buffer, '}');
	}
      is_expr = false;
      break;

    case PREDICT_EXPR:
      pp_string (buffer, "// predicted ");
      if (PREDICT_EXPR_OUTCOME (node))
        pp_string (buffer, "likely by ");
      else
        pp_string (buffer, "unlikely by ");
      pp_string (buffer, predictor_name (PREDICT_EXPR_PREDICTOR (node)));
      pp_string (buffer, " predictor.");
      break;

    case RETURN_EXPR:
      pp_string (buffer, "return");
      op0 = TREE_OPERAND (node, 0);
      if (op0)
	{
	  pp_space (buffer);
	  if (TREE_CODE (op0) == MODIFY_EXPR)
	    dump_generic_node (buffer, TREE_OPERAND (op0, 1),
			       spc, flags, false);
	  else
	    dump_generic_node (buffer, op0, spc, flags, false);
	}
      break;

    case EXIT_EXPR:
      pp_string (buffer, "if (");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, ") break");
      break;

    case SWITCH_EXPR:
      pp_string (buffer, "switch (");
      dump_generic_node (buffer, SWITCH_COND (node), spc, flags, false);
      pp_character (buffer, ')');
      if (!(flags & TDF_SLIM))
	{
	  newline_and_indent (buffer, spc+2);
	  pp_character (buffer, '{');
	  if (SWITCH_BODY (node))
	    {
	      newline_and_indent (buffer, spc+4);
	      dump_generic_node (buffer, SWITCH_BODY (node), spc+4, flags,
		                 true);
	    }
	  else
	    {
	      tree vec = SWITCH_LABELS (node);
	      size_t i, n = TREE_VEC_LENGTH (vec);
	      for (i = 0; i < n; ++i)
		{
		  tree elt = TREE_VEC_ELT (vec, i);
		  newline_and_indent (buffer, spc+4);
		  if (elt)
		    {
		      dump_generic_node (buffer, elt, spc+4, flags, false);
		      pp_string (buffer, " goto ");
		      dump_generic_node (buffer, CASE_LABEL (elt), spc+4,
					 flags, true);
		      pp_semicolon (buffer);
		    }
		  else
		    pp_string (buffer, "case ???: goto ???;");
		}
	    }
	  newline_and_indent (buffer, spc+2);
	  pp_character (buffer, '}');
	}
      is_expr = false;
      break;

    case GOTO_EXPR:
      op0 = GOTO_DESTINATION (node);
      if (TREE_CODE (op0) != SSA_NAME && DECL_P (op0) && DECL_NAME (op0))
	{
	  const char *name = IDENTIFIER_POINTER (DECL_NAME (op0));
	  if (strcmp (name, "break") == 0
	      || strcmp (name, "continue") == 0)
	    {
	      pp_string (buffer, name);
	      break;
	    }
	}
      pp_string (buffer, "goto ");
      dump_generic_node (buffer, op0, spc, flags, false);
      break;

    case ASM_EXPR:
      pp_string (buffer, "__asm__");
      if (ASM_VOLATILE_P (node))
	pp_string (buffer, " __volatile__");
      pp_character (buffer, '(');
      dump_generic_node (buffer, ASM_STRING (node), spc, flags, false);
      pp_character (buffer, ':');
      dump_generic_node (buffer, ASM_OUTPUTS (node), spc, flags, false);
      pp_character (buffer, ':');
      dump_generic_node (buffer, ASM_INPUTS (node), spc, flags, false);
      if (ASM_CLOBBERS (node))
	{
	  pp_character (buffer, ':');
	  dump_generic_node (buffer, ASM_CLOBBERS (node), spc, flags, false);
	}
      pp_string (buffer, ")");
      break;

    case CASE_LABEL_EXPR:
      if (CASE_LOW (node) && CASE_HIGH (node))
	{
	  pp_string (buffer, "case ");
	  dump_generic_node (buffer, CASE_LOW (node), spc, flags, false);
	  pp_string (buffer, " ... ");
	  dump_generic_node (buffer, CASE_HIGH (node), spc, flags, false);
	}
      else if (CASE_LOW (node))
	{
	  pp_string (buffer, "case ");
	  dump_generic_node (buffer, CASE_LOW (node), spc, flags, false);
	}
      else
	pp_string (buffer, "default");
      pp_character (buffer, ':');
      break;

    case OBJ_TYPE_REF:
      pp_string (buffer, "OBJ_TYPE_REF(");
      dump_generic_node (buffer, OBJ_TYPE_REF_EXPR (node), spc, flags, false);
      pp_character (buffer, ';');
      dump_generic_node (buffer, OBJ_TYPE_REF_OBJECT (node), spc, flags, false);
      pp_character (buffer, '-');
      pp_character (buffer, '>');
      dump_generic_node (buffer, OBJ_TYPE_REF_TOKEN (node), spc, flags, false);
      pp_character (buffer, ')');
      break;

    case SSA_NAME:
      dump_generic_node (buffer, SSA_NAME_VAR (node), spc, flags, false);
      pp_string (buffer, "_");
      pp_decimal_int (buffer, SSA_NAME_VERSION (node));
      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (node))
	pp_string (buffer, "(ab)");
      else if (SSA_NAME_IS_DEFAULT_DEF (node))
	pp_string (buffer, "(D)");
      break;

    case WITH_SIZE_EXPR:
      pp_string (buffer, "WITH_SIZE_EXPR <");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (buffer, ">");
      break;

    case ASSERT_EXPR:
      pp_string (buffer, "ASSERT_EXPR <");
      dump_generic_node (buffer, ASSERT_EXPR_VAR (node), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, ASSERT_EXPR_COND (node), spc, flags, false);
      pp_string (buffer, ">");
      break;

    case SCEV_KNOWN:
      pp_string (buffer, "scev_known");
      break;

    case SCEV_NOT_KNOWN:
      pp_string (buffer, "scev_not_known");
      break;

    case POLYNOMIAL_CHREC:
      pp_string (buffer, "{");
      dump_generic_node (buffer, CHREC_LEFT (node), spc, flags, false);
      pp_string (buffer, ", +, ");
      dump_generic_node (buffer, CHREC_RIGHT (node), spc, flags, false);
      pp_string (buffer, "}_");
      dump_generic_node (buffer, CHREC_VAR (node), spc, flags, false);
      is_stmt = false;
      break;

    case REALIGN_LOAD_EXPR:
      pp_string (buffer, "REALIGN_LOAD <");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, TREE_OPERAND (node, 2), spc, flags, false);
      pp_string (buffer, ">");
      break;

    case VEC_COND_EXPR:
      pp_string (buffer, " VEC_COND_EXPR < ");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, " , ");
      dump_generic_node (buffer, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (buffer, " , ");
      dump_generic_node (buffer, TREE_OPERAND (node, 2), spc, flags, false);
      pp_string (buffer, " > ");
      break;

    case DOT_PROD_EXPR:
      pp_string (buffer, " DOT_PROD_EXPR < ");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, TREE_OPERAND (node, 2), spc, flags, false);
      pp_string (buffer, " > ");
      break;

    case WIDEN_MULT_PLUS_EXPR:
      pp_string (buffer, " WIDEN_MULT_PLUS_EXPR < ");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, TREE_OPERAND (node, 2), spc, flags, false);
      pp_string (buffer, " > ");
      break;

    case WIDEN_MULT_MINUS_EXPR:
      pp_string (buffer, " WIDEN_MULT_MINUS_EXPR < ");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, TREE_OPERAND (node, 2), spc, flags, false);
      pp_string (buffer, " > ");
      break;

    case FMA_EXPR:
      pp_string (buffer, " FMA_EXPR < ");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, TREE_OPERAND (node, 2), spc, flags, false);
      pp_string (buffer, " > ");
      break;

    case OMP_PARALLEL:
      pp_string (buffer, "#pragma omp parallel");
      dump_omp_clauses (buffer, OMP_PARALLEL_CLAUSES (node), spc, flags);

    dump_omp_body:
      if (!(flags & TDF_SLIM) && OMP_BODY (node))
	{
	  newline_and_indent (buffer, spc + 2);
	  pp_character (buffer, '{');
	  newline_and_indent (buffer, spc + 4);
	  dump_generic_node (buffer, OMP_BODY (node), spc + 4, flags, false);
	  newline_and_indent (buffer, spc + 2);
	  pp_character (buffer, '}');
	}
      is_expr = false;
      break;

    case OMP_TASK:
      pp_string (buffer, "#pragma omp task");
      dump_omp_clauses (buffer, OMP_TASK_CLAUSES (node), spc, flags);
      goto dump_omp_body;

    case OMP_FOR:
      pp_string (buffer, "#pragma omp for");
      dump_omp_clauses (buffer, OMP_FOR_CLAUSES (node), spc, flags);

      if (!(flags & TDF_SLIM))
	{
	  int i;

	  if (OMP_FOR_PRE_BODY (node))
	    {
	      newline_and_indent (buffer, spc + 2);
	      pp_character (buffer, '{');
	      spc += 4;
	      newline_and_indent (buffer, spc);
	      dump_generic_node (buffer, OMP_FOR_PRE_BODY (node),
		  spc, flags, false);
	    }
	  spc -= 2;
	  for (i = 0; i < TREE_VEC_LENGTH (OMP_FOR_INIT (node)); i++)
	    {
	      spc += 2;
	      newline_and_indent (buffer, spc);
	      pp_string (buffer, "for (");
	      dump_generic_node (buffer, TREE_VEC_ELT (OMP_FOR_INIT (node), i),
				 spc, flags, false);
	      pp_string (buffer, "; ");
	      dump_generic_node (buffer, TREE_VEC_ELT (OMP_FOR_COND (node), i),
				 spc, flags, false);
	      pp_string (buffer, "; ");
	      dump_generic_node (buffer, TREE_VEC_ELT (OMP_FOR_INCR (node), i),
				 spc, flags, false);
	      pp_string (buffer, ")");
	    }
	  if (OMP_FOR_BODY (node))
	    {
	      newline_and_indent (buffer, spc + 2);
	      pp_character (buffer, '{');
	      newline_and_indent (buffer, spc + 4);
	      dump_generic_node (buffer, OMP_FOR_BODY (node), spc + 4, flags,
		  false);
	      newline_and_indent (buffer, spc + 2);
	      pp_character (buffer, '}');
	    }
	  spc -= 2 * TREE_VEC_LENGTH (OMP_FOR_INIT (node)) - 2;
	  if (OMP_FOR_PRE_BODY (node))
	    {
	      spc -= 4;
	      newline_and_indent (buffer, spc + 2);
	      pp_character (buffer, '}');
	    }
	}
      is_expr = false;
      break;

    case OMP_SECTIONS:
      pp_string (buffer, "#pragma omp sections");
      dump_omp_clauses (buffer, OMP_SECTIONS_CLAUSES (node), spc, flags);
      goto dump_omp_body;

    case OMP_SECTION:
      pp_string (buffer, "#pragma omp section");
      goto dump_omp_body;

    case OMP_MASTER:
      pp_string (buffer, "#pragma omp master");
      goto dump_omp_body;

    case OMP_ORDERED:
      pp_string (buffer, "#pragma omp ordered");
      goto dump_omp_body;

    case OMP_CRITICAL:
      pp_string (buffer, "#pragma omp critical");
      if (OMP_CRITICAL_NAME (node))
	{
	  pp_space (buffer);
	  pp_character (buffer, '(');
          dump_generic_node (buffer, OMP_CRITICAL_NAME (node), spc,
			     flags, false);
	  pp_character (buffer, ')');
	}
      goto dump_omp_body;

    case OMP_ATOMIC:
      pp_string (buffer, "#pragma omp atomic");
      newline_and_indent (buffer, spc + 2);
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_space (buffer);
      pp_character (buffer, '=');
      pp_space (buffer);
      dump_generic_node (buffer, TREE_OPERAND (node, 1), spc, flags, false);
      break;

    case OMP_SINGLE:
      pp_string (buffer, "#pragma omp single");
      dump_omp_clauses (buffer, OMP_SINGLE_CLAUSES (node), spc, flags);
      goto dump_omp_body;

    case OMP_CLAUSE:
      dump_omp_clause (buffer, node, spc, flags);
      is_expr = false;
      break;

    case REDUC_MAX_EXPR:
      pp_string (buffer, " REDUC_MAX_EXPR < ");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, " > ");
      break;

    case REDUC_MIN_EXPR:
      pp_string (buffer, " REDUC_MIN_EXPR < ");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, " > ");
      break;

    case REDUC_PLUS_EXPR:
      pp_string (buffer, " REDUC_PLUS_EXPR < ");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, " > ");
      break;

    case VEC_WIDEN_MULT_HI_EXPR:
      pp_string (buffer, " VEC_WIDEN_MULT_HI_EXPR < ");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (buffer, " > ");
      break;

    case VEC_WIDEN_MULT_LO_EXPR:
      pp_string (buffer, " VEC_WIDEN_MULT_LO_EXPR < ");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (buffer, " > ");
      break;

    case VEC_UNPACK_HI_EXPR:
      pp_string (buffer, " VEC_UNPACK_HI_EXPR < ");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, " > ");
      break;

    case VEC_UNPACK_LO_EXPR:
      pp_string (buffer, " VEC_UNPACK_LO_EXPR < ");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, " > ");
      break;

    case VEC_UNPACK_FLOAT_HI_EXPR:
      pp_string (buffer, " VEC_UNPACK_FLOAT_HI_EXPR < ");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, " > ");
      break;

    case VEC_UNPACK_FLOAT_LO_EXPR:
      pp_string (buffer, " VEC_UNPACK_FLOAT_LO_EXPR < ");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, " > ");
      break;

    case VEC_PACK_TRUNC_EXPR:
      pp_string (buffer, " VEC_PACK_TRUNC_EXPR < ");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (buffer, " > ");
      break;

    case VEC_PACK_SAT_EXPR:
      pp_string (buffer, " VEC_PACK_SAT_EXPR < ");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (buffer, " > ");
      break;

    case VEC_PACK_FIX_TRUNC_EXPR:
      pp_string (buffer, " VEC_PACK_FIX_TRUNC_EXPR < ");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (buffer, " > ");
      break;

    case BLOCK:
      dump_block_node (buffer, node, spc, flags);
      break;

    case VEC_EXTRACT_EVEN_EXPR:
      pp_string (buffer, " VEC_EXTRACT_EVEN_EXPR < ");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (buffer, " > ");
      break;

    case VEC_EXTRACT_ODD_EXPR:
      pp_string (buffer, " VEC_EXTRACT_ODD_EXPR < ");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (buffer, " > ");
      break;

    case VEC_INTERLEAVE_HIGH_EXPR:
      pp_string (buffer, " VEC_INTERLEAVE_HIGH_EXPR < ");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (buffer, " > ");
      break;

    case VEC_INTERLEAVE_LOW_EXPR:
      pp_string (buffer, " VEC_INTERLEAVE_LOW_EXPR < ");
      dump_generic_node (buffer, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (buffer, ", ");
      dump_generic_node (buffer, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (buffer, " > ");
      break;

    default:
      NIY;
    }

  if (is_stmt && is_expr)
    pp_semicolon (buffer);

  /* If we're building a diagnostic, the formatted text will be written
     into BUFFER's stream by the caller; otherwise, write it now.  */
  if (!(flags & TDF_DIAGNOSTIC))
    pp_write_text_to_stream (buffer);

  return spc;
}

/* Print the declaration of a variable.  */

void
print_declaration (pretty_printer *buffer, tree t, int spc, int flags)
{
  INDENT (spc);

  if (TREE_CODE (t) == TYPE_DECL)
    pp_string (buffer, "typedef ");

  if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_DECL_WRTL) && DECL_REGISTER (t))
    pp_string (buffer, "register ");

  if (TREE_PUBLIC (t) && DECL_EXTERNAL (t))
    pp_string (buffer, "extern ");
  else if (TREE_STATIC (t))
    pp_string (buffer, "static ");

  /* Print the type and name.  */
  if (TREE_TYPE (t) && TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE)
    {
      tree tmp;

      /* Print array's type.  */
      tmp = TREE_TYPE (t);
      while (TREE_CODE (TREE_TYPE (tmp)) == ARRAY_TYPE)
	tmp = TREE_TYPE (tmp);
      dump_generic_node (buffer, TREE_TYPE (tmp), spc, flags, false);

      /* Print variable's name.  */
      pp_space (buffer);
      dump_generic_node (buffer, t, spc, flags, false);

      /* Print the dimensions.  */
      tmp = TREE_TYPE (t);
      while (TREE_CODE (tmp) == ARRAY_TYPE)
	{
	  dump_array_domain (buffer, TYPE_DOMAIN (tmp), spc, flags);
	  tmp = TREE_TYPE (tmp);
	}
    }
  else if (TREE_CODE (t) == FUNCTION_DECL)
    {
      dump_generic_node (buffer, TREE_TYPE (TREE_TYPE (t)), spc, flags, false);
      pp_space (buffer);
      dump_decl_name (buffer, t, flags);
      dump_function_declaration (buffer, TREE_TYPE (t), spc, flags);
    }
  else
    {
      /* Print type declaration.  */
      dump_generic_node (buffer, TREE_TYPE (t), spc, flags, false);

      /* Print variable's name.  */
      pp_space (buffer);
      dump_generic_node (buffer, t, spc, flags, false);
    }

  if (TREE_CODE (t) == VAR_DECL && DECL_HARD_REGISTER (t))
    {
      pp_string (buffer, " __asm__ ");
      pp_character (buffer, '(');
      dump_generic_node (buffer, DECL_ASSEMBLER_NAME (t), spc, flags, false);
      pp_character (buffer, ')');
    }

  /* The initial value of a function serves to determine whether the function
     is declared or defined.  So the following does not apply to function
     nodes.  */
  if (TREE_CODE (t) != FUNCTION_DECL)
    {
      /* Print the initial value.  */
      if (DECL_INITIAL (t))
	{
	  pp_space (buffer);
	  pp_character (buffer, '=');
	  pp_space (buffer);
	  dump_generic_node (buffer, DECL_INITIAL (t), spc, flags, false);
	}
    }

  if (TREE_CODE (t) == VAR_DECL && DECL_HAS_VALUE_EXPR_P (t))
    {
      pp_string (buffer, " [value-expr: ");
      dump_generic_node (buffer, DECL_VALUE_EXPR (t), spc, flags, false);
      pp_character (buffer, ']');
    }

  pp_character (buffer, ';');
}


/* Prints a structure: name, fields, and methods.
   FIXME: Still incomplete.  */

static void
print_struct_decl (pretty_printer *buffer, const_tree node, int spc, int flags)
{
  /* Print the name of the structure.  */
  if (TYPE_NAME (node))
    {
      INDENT (spc);
      if (TREE_CODE (node) == RECORD_TYPE)
	pp_string (buffer, "struct ");
      else if ((TREE_CODE (node) == UNION_TYPE
		|| TREE_CODE (node) == QUAL_UNION_TYPE))
	pp_string (buffer, "union ");

      dump_generic_node (buffer, TYPE_NAME (node), spc, 0, false);
    }

  /* Print the contents of the structure.  */
  pp_newline (buffer);
  INDENT (spc);
  pp_character (buffer, '{');
  pp_newline (buffer);

  /* Print the fields of the structure.  */
  {
    tree tmp;
    tmp = TYPE_FIELDS (node);
    while (tmp)
      {
	/* Avoid to print recursively the structure.  */
	/* FIXME : Not implemented correctly...,
	   what about the case when we have a cycle in the contain graph? ...
	   Maybe this could be solved by looking at the scope in which the
	   structure was declared.  */
	if (TREE_TYPE (tmp) != node
	    && (TREE_CODE (TREE_TYPE (tmp)) != POINTER_TYPE
		|| TREE_TYPE (TREE_TYPE (tmp)) != node))
	  {
	    print_declaration (buffer, tmp, spc+2, flags);
	    pp_newline (buffer);
	  }
	tmp = DECL_CHAIN (tmp);
      }
  }
  INDENT (spc);
  pp_character (buffer, '}');
}

/* Return the priority of the operator CODE.

   From lowest to highest precedence with either left-to-right (L-R)
   or right-to-left (R-L) associativity]:

     1	[L-R] ,
     2	[R-L] = += -= *= /= %= &= ^= |= <<= >>=
     3	[R-L] ?:
     4	[L-R] ||
     5	[L-R] &&
     6	[L-R] |
     7	[L-R] ^
     8	[L-R] &
     9	[L-R] == !=
    10	[L-R] < <= > >=
    11	[L-R] << >>
    12	[L-R] + -
    13	[L-R] * / %
    14	[R-L] ! ~ ++ -- + - * & (type) sizeof
    15	[L-R] fn() [] -> .

   unary +, - and * have higher precedence than the corresponding binary
   operators.  */

int
op_code_prio (enum tree_code code)
{
  switch (code)
    {
    case TREE_LIST:
    case COMPOUND_EXPR:
    case BIND_EXPR:
      return 1;

    case MODIFY_EXPR:
    case INIT_EXPR:
      return 2;

    case COND_EXPR:
      return 3;

    case TRUTH_OR_EXPR:
    case TRUTH_ORIF_EXPR:
      return 4;

    case TRUTH_AND_EXPR:
    case TRUTH_ANDIF_EXPR:
      return 5;

    case BIT_IOR_EXPR:
      return 6;

    case BIT_XOR_EXPR:
    case TRUTH_XOR_EXPR:
      return 7;

    case BIT_AND_EXPR:
      return 8;

    case EQ_EXPR:
    case NE_EXPR:
      return 9;

    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
    case LTGT_EXPR:
    case ORDERED_EXPR:
    case UNORDERED_EXPR:
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
      return 10;

    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
      return 11;

    case WIDEN_SUM_EXPR:
    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
    case MINUS_EXPR:
      return 12;

    case VEC_WIDEN_MULT_HI_EXPR:
    case VEC_WIDEN_MULT_LO_EXPR:
    case WIDEN_MULT_EXPR:
    case DOT_PROD_EXPR:
    case WIDEN_MULT_PLUS_EXPR:
    case WIDEN_MULT_MINUS_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case RDIV_EXPR:
    case EXACT_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case FMA_EXPR:
      return 13;

    case TRUTH_NOT_EXPR:
    case BIT_NOT_EXPR:
    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case NEGATE_EXPR:
    case INDIRECT_REF:
    case ADDR_EXPR:
    case FLOAT_EXPR:
    CASE_CONVERT:
    case FIX_TRUNC_EXPR:
    case TARGET_EXPR:
      return 14;

    case CALL_EXPR:
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
    case COMPONENT_REF:
      return 15;

      /* Special expressions.  */
    case MIN_EXPR:
    case MAX_EXPR:
    case ABS_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case REDUC_MAX_EXPR:
    case REDUC_MIN_EXPR:
    case REDUC_PLUS_EXPR:
    case VEC_LSHIFT_EXPR:
    case VEC_RSHIFT_EXPR:
    case VEC_UNPACK_HI_EXPR:
    case VEC_UNPACK_LO_EXPR:
    case VEC_UNPACK_FLOAT_HI_EXPR:
    case VEC_UNPACK_FLOAT_LO_EXPR:
    case VEC_PACK_TRUNC_EXPR:
    case VEC_PACK_SAT_EXPR:
      return 16;

    default:
      /* Return an arbitrarily high precedence to avoid surrounding single
	 VAR_DECLs in ()s.  */
      return 9999;
    }
}

/* Return the priority of the operator OP.  */

int
op_prio (const_tree op)
{
  enum tree_code code;

  if (op == NULL)
    return 9999;

  code = TREE_CODE (op);
  if (code == SAVE_EXPR || code == NON_LVALUE_EXPR)
    return op_prio (TREE_OPERAND (op, 0));

  return op_code_prio (code);
}

/* Return the symbol associated with operator CODE.  */

const char *
op_symbol_code (enum tree_code code)
{
  switch (code)
    {
    case MODIFY_EXPR:
      return "=";

    case TRUTH_OR_EXPR:
    case TRUTH_ORIF_EXPR:
      return "||";

    case TRUTH_AND_EXPR:
    case TRUTH_ANDIF_EXPR:
      return "&&";

    case BIT_IOR_EXPR:
      return "|";

    case TRUTH_XOR_EXPR:
    case BIT_XOR_EXPR:
      return "^";

    case ADDR_EXPR:
    case BIT_AND_EXPR:
      return "&";

    case ORDERED_EXPR:
      return "ord";
    case UNORDERED_EXPR:
      return "unord";

    case EQ_EXPR:
      return "==";
    case UNEQ_EXPR:
      return "u==";

    case NE_EXPR:
      return "!=";

    case LT_EXPR:
      return "<";
    case UNLT_EXPR:
      return "u<";

    case LE_EXPR:
      return "<=";
    case UNLE_EXPR:
      return "u<=";

    case GT_EXPR:
      return ">";
    case UNGT_EXPR:
      return "u>";

    case GE_EXPR:
      return ">=";
    case UNGE_EXPR:
      return "u>=";

    case LTGT_EXPR:
      return "<>";

    case LSHIFT_EXPR:
      return "<<";

    case RSHIFT_EXPR:
      return ">>";

    case LROTATE_EXPR:
      return "r<<";

    case RROTATE_EXPR:
      return "r>>";

    case VEC_LSHIFT_EXPR:
      return "v<<";

    case VEC_RSHIFT_EXPR:
      return "v>>";

    case POINTER_PLUS_EXPR:
      return "+";

    case PLUS_EXPR:
      return "+";

    case REDUC_PLUS_EXPR:
      return "r+";

    case WIDEN_SUM_EXPR:
      return "w+";

    case WIDEN_MULT_EXPR:
      return "w*";

    case NEGATE_EXPR:
    case MINUS_EXPR:
      return "-";

    case BIT_NOT_EXPR:
      return "~";

    case TRUTH_NOT_EXPR:
      return "!";

    case MULT_EXPR:
    case INDIRECT_REF:
      return "*";

    case TRUNC_DIV_EXPR:
    case RDIV_EXPR:
      return "/";

    case CEIL_DIV_EXPR:
      return "/[cl]";

    case FLOOR_DIV_EXPR:
      return "/[fl]";

    case ROUND_DIV_EXPR:
      return "/[rd]";

    case EXACT_DIV_EXPR:
      return "/[ex]";

    case TRUNC_MOD_EXPR:
      return "%";

    case CEIL_MOD_EXPR:
      return "%[cl]";

    case FLOOR_MOD_EXPR:
      return "%[fl]";

    case ROUND_MOD_EXPR:
      return "%[rd]";

    case PREDECREMENT_EXPR:
      return " --";

    case PREINCREMENT_EXPR:
      return " ++";

    case POSTDECREMENT_EXPR:
      return "-- ";

    case POSTINCREMENT_EXPR:
      return "++ ";

    case MAX_EXPR:
      return "max";

    case MIN_EXPR:
      return "min";

    default:
      return "<<< ??? >>>";
    }
}

/* Return the symbol associated with operator OP.  */

static const char *
op_symbol (const_tree op)
{
  return op_symbol_code (TREE_CODE (op));
}

/* Prints the name of a call.  NODE is the CALL_EXPR_FN of a CALL_EXPR or
   the gimple_call_fn of a GIMPLE_CALL.  */

void
print_call_name (pretty_printer *buffer, tree node, int flags)
{
  tree op0 = node;

  if (TREE_CODE (op0) == NON_LVALUE_EXPR)
    op0 = TREE_OPERAND (op0, 0);

 again:
  switch (TREE_CODE (op0))
    {
    case VAR_DECL:
    case PARM_DECL:
    case FUNCTION_DECL:
      dump_function_name (buffer, op0, flags);
      break;

    case ADDR_EXPR:
    case INDIRECT_REF:
    case NOP_EXPR:
      op0 = TREE_OPERAND (op0, 0);
      goto again;

    case COND_EXPR:
      pp_string (buffer, "(");
      dump_generic_node (buffer, TREE_OPERAND (op0, 0), 0, flags, false);
      pp_string (buffer, ") ? ");
      dump_generic_node (buffer, TREE_OPERAND (op0, 1), 0, flags, false);
      pp_string (buffer, " : ");
      dump_generic_node (buffer, TREE_OPERAND (op0, 2), 0, flags, false);
      break;

    case ARRAY_REF:
      if (TREE_CODE (TREE_OPERAND (op0, 0)) == VAR_DECL)
	dump_function_name (buffer, TREE_OPERAND (op0, 0), flags);
      else
	dump_generic_node (buffer, op0, 0, flags, false);
      break;

    case MEM_REF:
      if (integer_zerop (TREE_OPERAND (op0, 1)))
	{
	  op0 = TREE_OPERAND (op0, 0);
	  goto again;
	}
      /* Fallthru.  */
    case COMPONENT_REF:
    case SSA_NAME:
    case OBJ_TYPE_REF:
      dump_generic_node (buffer, op0, 0, flags, false);
      break;

    default:
      NIY;
    }
}

/* Parses the string STR and replaces new-lines by '\n', tabs by '\t', ...  */

static void
pretty_print_string (pretty_printer *buffer, const char *str)
{
  if (str == NULL)
    return;

  while (*str)
    {
      switch (str[0])
	{
	case '\b':
	  pp_string (buffer, "\\b");
	  break;

	case '\f':
	  pp_string (buffer, "\\f");
	  break;

	case '\n':
	  pp_string (buffer, "\\n");
	  break;

	case '\r':
	  pp_string (buffer, "\\r");
	  break;

	case '\t':
	  pp_string (buffer, "\\t");
	  break;

	case '\v':
	  pp_string (buffer, "\\v");
	  break;

	case '\\':
	  pp_string (buffer, "\\\\");
	  break;

	case '\"':
	  pp_string (buffer, "\\\"");
	  break;

	case '\'':
	  pp_string (buffer, "\\'");
	  break;

	  /* No need to handle \0; the loop terminates on \0.  */

	case '\1':
	  pp_string (buffer, "\\1");
	  break;

	case '\2':
	  pp_string (buffer, "\\2");
	  break;

	case '\3':
	  pp_string (buffer, "\\3");
	  break;

	case '\4':
	  pp_string (buffer, "\\4");
	  break;

	case '\5':
	  pp_string (buffer, "\\5");
	  break;

	case '\6':
	  pp_string (buffer, "\\6");
	  break;

	case '\7':
	  pp_string (buffer, "\\7");
	  break;

	default:
	  pp_character (buffer, str[0]);
	  break;
	}
      str++;
    }
}

static void
maybe_init_pretty_print (FILE *file)
{
  if (!initialized)
    {
      pp_construct (&buffer, /* prefix */NULL, /* line-width */0);
      pp_needs_newline (&buffer) = true;
      pp_translate_identifiers (&buffer) = false;
      initialized = 1;
    }

  buffer.buffer->stream = file;
}

static void
newline_and_indent (pretty_printer *buffer, int spc)
{
  pp_newline (buffer);
  INDENT (spc);
}

/* Handle a %K format for TEXT.  Separate from default_tree_printer so
   it can also be used in front ends.
   %K: a statement, from which EXPR_LOCATION and TREE_BLOCK will be recorded.
*/

void
percent_K_format (text_info *text)
{
  tree t = va_arg (*text->args_ptr, tree), block;
  gcc_assert (text->locus != NULL);
  *text->locus = EXPR_LOCATION (t);
  gcc_assert (pp_ti_abstract_origin (text) != NULL);
  block = TREE_BLOCK (t);
  *pp_ti_abstract_origin (text) = NULL;
  while (block
	 && TREE_CODE (block) == BLOCK
	 && BLOCK_ABSTRACT_ORIGIN (block))
    {
      tree ao = BLOCK_ABSTRACT_ORIGIN (block);

      while (TREE_CODE (ao) == BLOCK
	     && BLOCK_ABSTRACT_ORIGIN (ao)
	     && BLOCK_ABSTRACT_ORIGIN (ao) != ao)
	ao = BLOCK_ABSTRACT_ORIGIN (ao);

      if (TREE_CODE (ao) == FUNCTION_DECL)
	{
	  *pp_ti_abstract_origin (text) = block;
	  break;
	}
      block = BLOCK_SUPERCONTEXT (block);
    }
}

/* Print the identifier ID to PRETTY-PRINTER.  */

void
pp_base_tree_identifier (pretty_printer *pp, tree id)
{
  if (pp_translate_identifiers (pp))
    {
      const char *text = identifier_to_locale (IDENTIFIER_POINTER (id));
      pp_append_text (pp, text, text + strlen (text));
    }
  else
    pp_append_text (pp, IDENTIFIER_POINTER (id),
		    IDENTIFIER_POINTER (id) + IDENTIFIER_LENGTH (id));
}
