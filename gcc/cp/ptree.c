/* Prints out trees in human readable form.
   Copyright (C) 1992-2020 Free Software Foundation, Inc.
   Hacked by Michael Tiemann (tiemann@cygnus.com)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "cp-tree.h"
#include "print-tree.h"

void
cxx_print_decl (FILE *file, tree node, int indent)
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

  if (!CODE_CONTAINS_STRUCT (TREE_CODE (node), TS_DECL_COMMON)
      || !DECL_LANG_SPECIFIC (node))
    return;

  if (TREE_CODE (node) == FUNCTION_DECL)
    {
      int flags = TFF_DECL_SPECIFIERS|TFF_RETURN_TYPE
	|TFF_FUNCTION_DEFAULT_ARGUMENTS|TFF_EXCEPTION_SPECIFICATION ;
      indent_to (file, indent + 3);
      fprintf (file, " full-name \"%s\"", decl_as_string (node, flags));
    }
  else if (TREE_CODE (node) == TEMPLATE_DECL)
    {
      print_node (file, "parms", DECL_TEMPLATE_PARMS (node), indent + 4);
      indent_to (file, indent + 3);
      fprintf (file, " full-name \"%s\"",
	       decl_as_string (node, TFF_TEMPLATE_HEADER));
    }

  indent_to (file, indent + 3);
  if (DECL_EXTERNAL (node) && DECL_NOT_REALLY_EXTERN (node))
    fprintf (file, " not-really-extern");
  if (TREE_CODE (node) == FUNCTION_DECL
      && DECL_PENDING_INLINE_INFO (node))
    fprintf (file, " pending-inline-info %p",
	     (void *) DECL_PENDING_INLINE_INFO (node));
  if (VAR_OR_FUNCTION_DECL_P (node)
      && DECL_TEMPLATE_INFO (node))
    fprintf (file, " template-info %p",
	     (void *) DECL_TEMPLATE_INFO (node));
}

void
cxx_print_type (FILE *file, tree node, int indent)
{
  switch (TREE_CODE (node))
    {
    case BOUND_TEMPLATE_TEMPLATE_PARM:
      print_node (file, "args", TYPE_TI_ARGS (node), indent + 4);
      gcc_fallthrough ();

    case TEMPLATE_TYPE_PARM:
    case TEMPLATE_TEMPLATE_PARM:
      indent_to (file, indent + 3);
      fprintf (file, "index %d level %d orig_level %d",
	       TEMPLATE_TYPE_IDX (node), TEMPLATE_TYPE_LEVEL (node),
	       TEMPLATE_TYPE_ORIG_LEVEL (node));
      return;

    case FUNCTION_TYPE:
    case METHOD_TYPE:
      if (TYPE_RAISES_EXCEPTIONS (node))
	print_node (file, "throws", TYPE_RAISES_EXCEPTIONS (node), indent + 4);
      return;

    case RECORD_TYPE:
    case UNION_TYPE:
      break;

    case DECLTYPE_TYPE:
      print_node (file, "expr", DECLTYPE_TYPE_EXPR (node), indent + 4);
      return;

    case TYPENAME_TYPE:
      print_node (file, "fullname", TYPENAME_TYPE_FULLNAME (node),
		  indent + 4);
      return;

    case TYPEOF_TYPE:
      print_node (file, "expr", TYPEOF_TYPE_EXPR (node), indent + 4);
      return;

    case BASES:
      if (BASES_DIRECT (node))
	fputs (" direct", file);
      print_node (file, "type", BASES_TYPE (node), indent + 4);
      return;

    case TYPE_PACK_EXPANSION:
      print_node (file, "args", PACK_EXPANSION_EXTRA_ARGS (node), indent + 4);
      return;

    default:
      return;
    }

  if (TYPE_PTRMEMFUNC_P (node))
    print_node (file, "ptrmemfunc fn type", TYPE_PTRMEMFUNC_FN_TYPE (node),
		indent + 4);

  if (! CLASS_TYPE_P (node))
    return;

  indent_to (file, indent + 4);
  fprintf (file, "full-name \"%s\"",
	   type_as_string (node, TFF_CLASS_KEY_OR_ENUM));

  indent_to (file, indent + 3);

  if (TYPE_NEEDS_CONSTRUCTING (node))
    fputs ( " needs-constructor", file);
  if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (node))
    fputs (" needs-destructor", file);
  if (TYPE_HAS_DEFAULT_CONSTRUCTOR (node))
    fputs (" X()", file);
  if (TYPE_HAS_CONVERSION (node))
    fputs (" has-type-conversion", file);
  if (TYPE_HAS_COPY_CTOR (node))
    {
      if (TYPE_HAS_CONST_COPY_CTOR (node))
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
  if (TYPE_HAS_COPY_ASSIGN (node))
    fputs (" this=(X&)", file);

  if (TREE_CODE (node) == RECORD_TYPE)
    {
      if (TYPE_BINFO (node))
	fprintf (file, " n_parents=%d",
		 BINFO_N_BASE_BINFOS (TYPE_BINFO (node)));
      else
	fprintf (file, " no-binfo");

      fprintf (file, " use_template=%d", CLASSTYPE_USE_TEMPLATE (node));
      if (CLASSTYPE_INTERFACE_ONLY (node))
	fprintf (file, " interface-only");
      if (CLASSTYPE_INTERFACE_UNKNOWN (node))
	fprintf (file, " interface-unknown");
    }
}

void
cxx_print_identifier (FILE *file, tree node, int indent)
{
  if (indent == 0)
    fprintf (file, " ");
  else
    indent_to (file, indent + 4);
  fprintf (file, "%s local bindings <%p>", get_identifier_kind_name (node),
	   (void *) IDENTIFIER_BINDING (node));
}

void
cxx_print_lambda_node (FILE *file, tree node, int indent)
{
  if (LAMBDA_EXPR_MUTABLE_P (node))
    fprintf (file, " /mutable");
  fprintf (file, " default_capture_mode=[");
  switch (LAMBDA_EXPR_DEFAULT_CAPTURE_MODE (node))
    {
    case CPLD_NONE:
      fprintf (file, "NONE");
      break;
    case CPLD_COPY:
      fprintf (file, "COPY");
      break;
    case CPLD_REFERENCE:
      fprintf (file, "CPLD_REFERENCE");
      break;
    default:
      fprintf (file, "??");
      break;
    }
  fprintf (file, "] ");
  print_node (file, "capture_list", LAMBDA_EXPR_CAPTURE_LIST (node), indent + 4);
  print_node (file, "this_capture", LAMBDA_EXPR_THIS_CAPTURE (node), indent + 4);
}

void
cxx_print_xnode (FILE *file, tree node, int indent)
{
  switch (TREE_CODE (node))
    {
    case BASELINK:
      print_node (file, "functions", BASELINK_FUNCTIONS (node), indent + 4);
      print_node (file, "binfo", BASELINK_BINFO (node), indent + 4);
      print_node (file, "access_binfo", BASELINK_ACCESS_BINFO (node),
		  indent + 4);
      print_node (file, "optype", BASELINK_OPTYPE (node), indent + 4);
      break;
    case OVERLOAD:
      print_node (file, "function", OVL_FUNCTION (node), indent + 4);
      print_node (file, "next", OVL_CHAIN (node), indent + 4);
      break;
    case TEMPLATE_PARM_INDEX:
      print_node (file, "decl", TEMPLATE_PARM_DECL (node), indent+4);
      indent_to (file, indent + 3);
      fprintf (file, "index %d level %d orig_level %d",
	       TEMPLATE_PARM_IDX (node), TEMPLATE_PARM_LEVEL (node),
	       TEMPLATE_PARM_ORIG_LEVEL (node));
      break;
    case TEMPLATE_INFO:
      print_node (file, "template", TI_TEMPLATE (node), indent+4);
      print_node (file, "args", TI_ARGS (node), indent+4);
      if (TI_PENDING_TEMPLATE_FLAG (node))
	{
	  indent_to (file, indent + 3);
	  fprintf (file, "pending_template");
	}
      break;
    case CONSTRAINT_INFO:
      {
        tree_constraint_info *cinfo = (tree_constraint_info *)node;
        if (cinfo->template_reqs)
          print_node (file, "template_reqs", cinfo->template_reqs, indent+4);
        if (cinfo->declarator_reqs)
          print_node (file, "declarator_reqs", cinfo->declarator_reqs,
		      indent+4);
        print_node (file, "associated_constr",
                          cinfo->associated_constr, indent+4);
        break;
      }
    case ARGUMENT_PACK_SELECT:
      print_node (file, "pack", ARGUMENT_PACK_SELECT_FROM_PACK (node),
		  indent+4);
      indent_to (file, indent + 3);
      fprintf (file, "index %d", ARGUMENT_PACK_SELECT_INDEX (node));
      break;
    case DEFERRED_NOEXCEPT:
      print_node (file, "pattern", DEFERRED_NOEXCEPT_PATTERN (node), indent+4);
      print_node (file, "args", DEFERRED_NOEXCEPT_ARGS (node), indent+4);
      break;
    case TRAIT_EXPR:
      indent_to (file, indent+4);
      fprintf (file, "kind %d", TRAIT_EXPR_KIND (node));
      print_node (file, "type 1", TRAIT_EXPR_TYPE1 (node), indent+4);
      if (TRAIT_EXPR_TYPE2 (node))
	print_node (file, "type 2", TRAIT_EXPR_TYPE2 (node), indent+4);
      break;
    case LAMBDA_EXPR:
      cxx_print_lambda_node (file, node, indent);
      break;
    case STATIC_ASSERT:
      if (location_t loc = STATIC_ASSERT_SOURCE_LOCATION (node))
	{
	  expanded_location xloc = expand_location (loc);
	  indent_to (file, indent+4);
	  fprintf (file, "%s:%d:%d", xloc.file, xloc.line, xloc.column);
	}
      print_node (file, "condition", STATIC_ASSERT_CONDITION (node), indent+4);
      if (tree message = STATIC_ASSERT_MESSAGE (node))
	print_node (file, "message", message, indent+4);
      break;
    default:
      break;
    }
}

/* Print the node NODE on standard error, for debugging.  */

DEBUG_FUNCTION void
debug_tree (cp_expr node)
{
  debug_tree (node.get_value());
}
