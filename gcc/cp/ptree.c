/* Prints out trees in human readable form.
   Copyright (C) 1992, 1993 Free Software Foundation, Inc.
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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include "config.h"
#include "tree.h"
#include <stdio.h>
#include "cp-tree.h"

void
print_lang_decl (file, node, indent)
     FILE *file;
     tree node;
     int indent;
{
  if (!DECL_LANG_SPECIFIC (node))
    return;
  /* A FIELD_DECL only has the flags structure, which we aren't displaying
     anyways.  */
  if (DECL_MUTABLE_P (node))
    {
      indent_to (file, indent + 3);
      fprintf (file, " mutable ");
    }
  if (TREE_CODE (node) == FIELD_DECL)
    return;
  indent_to (file, indent + 3);
  if (DECL_MAIN_VARIANT (node))
    {
      fprintf (file, " decl-main-variant ");
      fprintf (file, HOST_PTR_PRINTF, DECL_MAIN_VARIANT (node));
    }
  if (DECL_PENDING_INLINE_INFO (node))
    {
      fprintf (file, " pending-inline-info ");
      fprintf (file, HOST_PTR_PRINTF, DECL_PENDING_INLINE_INFO (node));
    }
  if (DECL_TEMPLATE_INFO (node))
    {
      fprintf (file, " template-info ");
      fprintf (file, HOST_PTR_PRINTF,  DECL_TEMPLATE_INFO (node));
    }
}

void
print_lang_type (file, node, indent)
     FILE *file;
     register tree node;
     int indent;
{
  if (TREE_CODE (node) == TEMPLATE_TYPE_PARM)
    {
      print_node (file, "tinfo", TYPE_VALUES (node), indent + 4);
      return;
    }

  if (TREE_CODE (node) == UNINSTANTIATED_P_TYPE)
    {
      print_node (file, "template", UPT_TEMPLATE (node), indent + 4);
      print_node (file, "parameters", UPT_PARMS (node), indent + 4);
      return;
    }

  if (! (TREE_CODE (node) == RECORD_TYPE
	 || TREE_CODE (node) == UNION_TYPE))
    return;

  if (!TYPE_LANG_SPECIFIC (node))
    return;

  indent_to (file, indent + 3);

  if (TYPE_NEEDS_CONSTRUCTING (node))
    fputs ( "needs-constructor", file);
  if (TYPE_NEEDS_DESTRUCTOR (node))
    fputs (" needs-destructor", file);
  if (TYPE_HAS_DESTRUCTOR (node))
    fputs (" ~X()", file);
  if (TYPE_HAS_DEFAULT_CONSTRUCTOR (node))
    fputs (" X()", file);
  if (TYPE_HAS_CONVERSION (node))
    fputs (" has-type-conversion", file);
  if (TYPE_HAS_INT_CONVERSION (node))
    fputs (" has-int-conversion", file);
  if (TYPE_HAS_REAL_CONVERSION (node))
    fputs (" has-float-conversion", file);
  if (TYPE_HAS_INIT_REF (node))
    {
      if (TYPE_HAS_CONST_INIT_REF (node))
	fputs (" X(constX&)", file);
      else
	fputs (" X(X&)", file);
    }
  if (TYPE_GETS_NEW (node) & 1)
    fputs (" new", file);
  if (TYPE_GETS_NEW (node) & 2)
    fputs (" new[]", file);
  if (TYPE_GETS_DELETE (node) & 1)
    fputs (" delete", file);
  if (TYPE_GETS_DELETE (node) & 2)
    fputs (" delete[]", file);
  if (TYPE_HAS_ASSIGNMENT (node))
    fputs (" has=", file);
  if (TYPE_HAS_ASSIGN_REF (node))
    fputs (" this=(X&)", file);
  if (TYPE_OVERLOADS_METHOD_CALL_EXPR (node))
    fputs (" op->()", file);
  if (TYPE_GETS_INIT_AGGR (node))
    fputs (" gets X(X, ...)", file);
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
      fprintf (file, " n_parents %d n_ancestors %d",
	       CLASSTYPE_N_BASECLASSES (node),
	       CLASSTYPE_N_SUPERCLASSES (node));
      fprintf (file, " use_template=%d", CLASSTYPE_USE_TEMPLATE (node));
      if (CLASSTYPE_INTERFACE_ONLY (node))
	fprintf (file, " interface-only");
      if (CLASSTYPE_INTERFACE_UNKNOWN (node))
	fprintf (file, " interface-unknown");
      print_node (file, "member-functions", CLASSTYPE_METHOD_VEC (node),
		  indent + 4);
      print_node (file, "baselinks",
		  TYPE_BINFO_BASETYPES (node) ? CLASSTYPE_BASELINK_VEC (node) : NULL_TREE,
		  indent + 4);
    }
}

void
print_lang_identifier (file, node, indent)
     FILE *file;
     tree node;
     int indent;
{
  print_node (file, "global", IDENTIFIER_GLOBAL_VALUE (node), indent + 4);
  print_node (file, "class", IDENTIFIER_CLASS_VALUE (node), indent + 4);
  print_node (file, "local", IDENTIFIER_LOCAL_VALUE (node), indent + 4);
  print_node (file, "label", IDENTIFIER_LABEL_VALUE (node), indent + 4);
  print_node (file, "template", IDENTIFIER_TEMPLATE (node), indent + 4);
  print_node (file, "implicit", IDENTIFIER_IMPLICIT_DECL (node), indent + 4);
  print_node (file, "error locus", IDENTIFIER_ERROR_LOCUS (node), indent + 4);
}
