/* Some code common to C and ObjC front ends.
   Copyright (C) 2001-2013 Free Software Foundation, Inc.

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
#include "tree.h"
#include "c-tree.h"
#include "intl.h"
#include "c-family/c-pretty-print.h"
#include "flags.h"
#include "diagnostic.h"
#include "tree-pretty-print.h"
#include "langhooks.h"
#include "c-objc-common.h"

#include <new>                          // For placement new.

static bool c_tree_printer (pretty_printer *, text_info *, const char *,
			    int, bool, bool, bool);

bool
c_missing_noreturn_ok_p (tree decl)
{
  /* A missing noreturn is not ok for freestanding implementations and
     ok for the `main' function in hosted implementations.  */
  return flag_hosted && MAIN_NAME_P (DECL_ASSEMBLER_NAME (decl));
}

/* Called from check_global_declarations.  */

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

  if (c_common_init () == false)
    return false;

  /* These were not defined in the Objective-C front end, but I'm
     putting them here anyway.  The diagnostic format decoder might
     want an enhanced ObjC implementation.  */
  diagnostic_format_decoder (global_dc) = &c_tree_printer;

  return true;
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
      if (set_locus && text->locus)
	*text->locus = DECL_SOURCE_LOCATION (t);
    }

  switch (*spec)
    {
    case 'D':
      if (TREE_CODE (t) == VAR_DECL && DECL_HAS_DEBUG_EXPR_P (t))
	{
	  t = DECL_DEBUG_EXPR (t);
	  if (!DECL_P (t))
	    {
	      pp_c_expression (cpp, t);
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
      gcc_assert (TYPE_P (t));
      name = TYPE_NAME (t);

      if (name && TREE_CODE (name) == TYPE_DECL)
	{
	  if (DECL_NAME (name))
	    pp_identifier (cpp, lang_hooks.decl_printable_name (name, 2));
	  else
	    pp_type_id (cpp, t);
	  return true;
	}
      else
	{
	  pp_type_id (cpp, t);
	  return true;
	}
      break;

    case 'E':
      if (TREE_CODE (t) == IDENTIFIER_NODE)
	pp_identifier (cpp, IDENTIFIER_POINTER (t));
      else
	pp_expression (cpp, t);
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
  c_common_initialize_diagnostics (context);

  pretty_printer *base = context->printer;
  c_pretty_printer *pp = XNEW (c_pretty_printer);
  context->printer = new (pp) c_pretty_printer ();

  /* It is safe to free this object because it was previously XNEW()'d.  */
  base->~pretty_printer ();
  XDELETE (base);
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
