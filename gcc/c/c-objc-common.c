/* Some code common to C and ObjC front ends.
   Copyright (C) 2001-2016 Free Software Foundation, Inc.

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
#include "c-tree.h"
#include "intl.h"
#include "c-family/c-pretty-print.h"
#include "tree-pretty-print.h"
#include "langhooks.h"
#include "c-objc-common.h"

static bool c_tree_printer (pretty_printer *, text_info *, const char *,
			    int, bool, bool, bool);

bool
c_missing_noreturn_ok_p (tree decl)
{
  /* A missing noreturn is not ok for freestanding implementations and
     ok for the `main' function in hosted implementations.  */
  return flag_hosted && MAIN_NAME_P (DECL_ASSEMBLER_NAME (decl));
}

/* Called from check_global_declaration.  */

bool
c_warn_unused_global_decl (const_tree decl)
{
  if (TREE_CODE (decl) == FUNCTION_DECL && DECL_DECLARED_INLINE_P (decl))
    return false;
  if (DECL_IN_SYSTEM_HEADER (decl))
    return false;

  return true;
}

/* Initialization common to C and Objective-C front ends.  */
bool
c_objc_common_init (void)
{
  c_init_decl_processing ();

  return c_common_init ();
}

/* Called during diagnostic message formatting process to print a
   source-level entity onto BUFFER.  The meaning of the format specifiers
   is as follows:
   %D: a general decl,
   %E: an identifier or expression,
   %F: a function declaration,
   %T: a type.
   %V: a list of type qualifiers from a tree.
   %v: an explicit list of type qualifiers
   %#v: an explicit list of type qualifiers of a function type.

   Please notice when called, the `%' part was already skipped by the
   diagnostic machinery.  */
static bool
c_tree_printer (pretty_printer *pp, text_info *text, const char *spec,
		int precision, bool wide, bool set_locus, bool hash)
{
  tree t = NULL_TREE;
  tree name;
  // FIXME: the next cast should be a dynamic_cast, when it is permitted.
  c_pretty_printer *cpp = (c_pretty_printer *) pp;
  pp->padding = pp_none;

  if (precision != 0 || wide)
    return false;

  if (*spec == 'K')
    {
      percent_K_format (text);
      return true;
    }

  if (*spec != 'v')
    {
      t = va_arg (*text->args_ptr, tree);
      if (set_locus)
	text->set_location (0, DECL_SOURCE_LOCATION (t), true);
    }

  switch (*spec)
    {
    case 'D':
      if (VAR_P (t) && DECL_HAS_DEBUG_EXPR_P (t))
	{
	  t = DECL_DEBUG_EXPR (t);
	  if (!DECL_P (t))
	    {
	      cpp->expression (t);
	      return true;
	    }
	}
      /* FALLTHRU */

    case 'F':
      if (DECL_NAME (t))
	{
	  pp_identifier (cpp, lang_hooks.decl_printable_name (t, 2));
	  return true;
	}
      break;

    case 'T':
      {
	gcc_assert (TYPE_P (t));
	struct obstack *ob = pp_buffer (cpp)->obstack;
	char *p = (char *) obstack_base (ob);
	/* Remember the end of the initial dump.  */
	int len = obstack_object_size (ob);

	name = TYPE_NAME (t);
	if (name && TREE_CODE (name) == TYPE_DECL && DECL_NAME (name))
	  pp_identifier (cpp, lang_hooks.decl_printable_name (name, 2));
	else
	  cpp->type_id (t);

	/* If we're printing a type that involves typedefs, also print the
	   stripped version.  But sometimes the stripped version looks
	   exactly the same, so we don't want it after all.  To avoid
	   printing it in that case, we play ugly obstack games.  */
	if (TYPE_CANONICAL (t) && t != TYPE_CANONICAL (t))
	  {
	    c_pretty_printer cpp2;
	    /* Print the stripped version into a temporary printer.  */
	    cpp2.type_id (TYPE_CANONICAL (t));
	    struct obstack *ob2 = cpp2.buffer->obstack;
	    /* Get the stripped version from the temporary printer.  */
	    const char *aka = (char *) obstack_base (ob2);
	    int aka_len = obstack_object_size (ob2);
	    int type1_len = obstack_object_size (ob) - len;

	    /* If they are identical, bail out.  */
	    if (aka_len == type1_len && memcmp (p + len, aka, aka_len) == 0)
	      return true;

	    /* They're not, print the stripped version now.  */
	    pp_c_whitespace (cpp);
	    pp_left_brace (cpp);
	    pp_c_ws_string (cpp, _("aka"));
	    pp_c_whitespace (cpp);
	    cpp->type_id (TYPE_CANONICAL (t));
	    pp_right_brace (cpp);
	  }
	return true;
      }

    case 'E':
      if (TREE_CODE (t) == IDENTIFIER_NODE)
	pp_identifier (cpp, IDENTIFIER_POINTER (t));
      else
	cpp->expression (t);
      return true;

    case 'V':
      pp_c_type_qualifier_list (cpp, t);
      return true;

    case 'v':
      pp_c_cv_qualifiers (cpp, va_arg (*text->args_ptr, int), hash);
      return true;

    default:
      return false;
    }

  pp_string (cpp, _("({anonymous})"));
  return true;
}

/* In C and ObjC, all decls have "C" linkage.  */
bool
has_c_linkage (const_tree decl ATTRIBUTE_UNUSED)
{
  return true;
}

void
c_initialize_diagnostics (diagnostic_context *context)
{
  pretty_printer *base = context->printer;
  c_pretty_printer *pp = XNEW (c_pretty_printer);
  context->printer = new (pp) c_pretty_printer ();

  /* It is safe to free this object because it was previously XNEW()'d.  */
  base->~pretty_printer ();
  XDELETE (base);

  c_common_diagnostics_set_defaults (context);
  diagnostic_format_decoder (context) = &c_tree_printer;
}

int
c_types_compatible_p (tree x, tree y)
{
  return comptypes (TYPE_MAIN_VARIANT (x), TYPE_MAIN_VARIANT (y));
}

/* Determine if the type is a vla type for the backend.  */

bool
c_vla_unspec_p (tree x, tree fn ATTRIBUTE_UNUSED)
{
  return c_vla_type_p (x);
}
