/* Prints out trees in human readable form.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1998,
   1999 Free Software Foundation, Inc.
   Hacked by Michael Tiemann (tiemann@cygnus.com)

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

void
cxx_print_decl (file, node, indent)
     FILE *file;
     tree node;
     int indent;
{
  if (TREE_CODE (node) == FIELD_DECL)
    {
      if (DECL_MUTABLE_P (node))
	{
	  indent_to (file, indent + 3);
	  fprintf (file, " mutable ");
	}
      return;
    }

  if (!DECL_LANG_SPECIFIC (node))
    return;
  indent_to (file, indent + 3);
  if (TREE_CODE (node) == FUNCTION_DECL
      && DECL_PENDING_INLINE_INFO (node))
    {
      fprintf (file, " pending-inline-info ");
      fprintf (file, HOST_PTR_PRINTF, DECL_PENDING_INLINE_INFO (node));
    }
  if (TREE_CODE (node) == TYPE_DECL
      && DECL_SORTED_FIELDS (node))
    {
      fprintf (file, " sorted-fields ");
      fprintf (file, HOST_PTR_PRINTF, DECL_SORTED_FIELDS (node));
    }
  if ((TREE_CODE (node) == FUNCTION_DECL || TREE_CODE (node) == VAR_DECL)
      && DECL_TEMPLATE_INFO (node))
    {
      fprintf (file, " template-info ");
      fprintf (file, HOST_PTR_PRINTF,  DECL_TEMPLATE_INFO (node));
    }
}

void
cxx_print_type (file, node, indent)
     FILE *file;
     register tree node;
     int indent;
{
  switch (TREE_CODE (node))
    {
    case TEMPLATE_TYPE_PARM:
    case TEMPLATE_TEMPLATE_PARM:
    case BOUND_TEMPLATE_TEMPLATE_PARM:
      indent_to (file, indent + 3);
      fputs ("index ", file);
      fprintf (file, HOST_WIDE_INT_PRINT_DEC, TEMPLATE_TYPE_IDX (node));
      fputs (" level ", file);
      fprintf (file, HOST_WIDE_INT_PRINT_DEC, TEMPLATE_TYPE_LEVEL (node));
      fputs (" orig_level ", file);
      fprintf (file, HOST_WIDE_INT_PRINT_DEC, TEMPLATE_TYPE_ORIG_LEVEL (node));
      return;

    case FUNCTION_TYPE:
    case METHOD_TYPE:
      if (TYPE_RAISES_EXCEPTIONS (node))
	print_node (file, "throws", TYPE_RAISES_EXCEPTIONS (node), indent + 4);
      return;

    case RECORD_TYPE:
    case UNION_TYPE:
      break;

    default:
      return;
    }

  if (TYPE_PTRMEMFUNC_P (node))
    print_node (file, "ptrmemfunc fn type", TYPE_PTRMEMFUNC_FN_TYPE (node),
		indent + 4);

  if (! CLASS_TYPE_P (node))
    return;

  indent_to (file, indent + 3);

  if (TYPE_NEEDS_CONSTRUCTING (node))
    fputs ( "needs-constructor", file);
  if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (node))
    fputs (" needs-destructor", file);
  if (TYPE_HAS_DESTRUCTOR (node))
    fputs (" ~X()", file);
  if (TYPE_HAS_DEFAULT_CONSTRUCTOR (node))
    fputs (" X()", file);
  if (TYPE_HAS_CONVERSION (node))
    fputs (" has-type-conversion", file);
  if (TYPE_HAS_INIT_REF (node))
    {
      if (TYPE_HAS_CONST_INIT_REF (node))
	fputs (" X(constX&)", file);
      else
	fputs (" X(X&)", file);
    }
  if (TYPE_HAS_NEW_OPERATOR (node))
    fputs (" new", file);
  if (TYPE_HAS_ARRAY_NEW_OPERATOR (node))
    fputs (" new[]", file);
  if (TYPE_GETS_DELETE (node) & 1)
    fputs (" delete", file);
  if (TYPE_GETS_DELETE (node) & 2)
    fputs (" delete[]", file);
  if (TYPE_HAS_ASSIGN_REF (node))
    fputs (" this=(X&)", file);
  if (TYPE_OVERLOADS_CALL_EXPR (node))
    fputs (" op()", file);
  if (TYPE_OVERLOADS_ARRAY_REF (node))
    fputs (" op[]", file);
  if (TYPE_OVERLOADS_ARROW (node))
    fputs (" op->", file);
  if (TYPE_USES_MULTIPLE_INHERITANCE (node))
    fputs (" uses-multiple-inheritance", file);

  if (TREE_CODE (node) == RECORD_TYPE)
    {
      fprintf (file, " n_parents %d", CLASSTYPE_N_BASECLASSES (node));
      fprintf (file, " use_template=%d", CLASSTYPE_USE_TEMPLATE (node));
      if (CLASSTYPE_INTERFACE_ONLY (node))
	fprintf (file, " interface-only");
      if (CLASSTYPE_INTERFACE_UNKNOWN (node))
	fprintf (file, " interface-unknown");
      print_node (file, "member-functions", CLASSTYPE_METHOD_VEC (node),
		  indent + 4);
    }
}

static void
cxx_print_binding (FILE *stream, cxx_binding *binding, const char *prefix)
{
  fprintf (stream, "%s <", prefix);
  fprintf (stream, HOST_PTR_PRINTF, (char *) binding);
  fprintf (stream, ">");
}

void
cxx_print_identifier (file, node, indent)
     FILE *file;
     tree node;
     int indent;
{
  cxx_print_binding (file, IDENTIFIER_NAMESPACE_BINDINGS (node), "bindings");
  print_node (file, "class", IDENTIFIER_CLASS_VALUE (node), indent + 4);
  cxx_print_binding (file, IDENTIFIER_BINDING (node), "local bindings");
  print_node (file, "label", IDENTIFIER_LABEL_VALUE (node), indent + 4);
  print_node (file, "template", IDENTIFIER_TEMPLATE (node), indent + 4);
  print_node (file, "implicit", IDENTIFIER_IMPLICIT_DECL (node), indent + 4);
  print_node (file, "error locus", IDENTIFIER_ERROR_LOCUS (node), indent + 4);
}

void
cxx_print_xnode (file, node, indent)
     FILE *file;
     tree node;
     int indent;
{
  switch (TREE_CODE (node))
    {
    case OVERLOAD:
      print_node (file, "function", OVL_FUNCTION (node), indent+4);
      print_node (file, "chain", TREE_CHAIN (node), indent+4);
      break;
    case TEMPLATE_PARM_INDEX:
      indent_to (file, indent + 3);
      fputs ("index ", file);
      fprintf (file, HOST_WIDE_INT_PRINT_DEC, TEMPLATE_PARM_IDX (node));
      fputs (" level ", file);
      fprintf (file, HOST_WIDE_INT_PRINT_DEC, TEMPLATE_PARM_LEVEL (node));
      fputs (" orig_level ", file);
      fprintf (file, HOST_WIDE_INT_PRINT_DEC, TEMPLATE_PARM_ORIG_LEVEL (node));
      break;
    default:
      break;
    }
}
