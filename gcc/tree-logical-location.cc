/* Subclasses of logical_location with knowledge of "tree".
   Copyright (C) 2022-2025 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

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
#include "pretty-print.h"
#include "tree-logical-location.h"
#include "langhooks.h"
#include "intl.h"

/* class compiler_logical_location : public logical_location.  */

/* Get a string for DECL suitable for use by the SARIF logicalLocation
   "name" property (SARIF v2.1.0 section 3.33.4).  */

const char *
compiler_logical_location::get_short_name_for_tree (tree decl)
{
  gcc_assert (decl);
  return identifier_to_locale (lang_hooks.decl_printable_name (decl, 0));
}

/* Get a string for DECL suitable for use by the SARIF logicalLocation
   "fullyQualifiedName" property (SARIF v2.1.0 section 3.33.5).  */

const char *
compiler_logical_location::get_name_with_scope_for_tree (tree decl)
{
  gcc_assert (decl);
  return identifier_to_locale (lang_hooks.decl_printable_name (decl, 1));
}

/* Get a string for DECL suitable for use by the SARIF logicalLocation
   "decoratedName" property (SARIF v2.1.0 section 3.33.6).  */

const char *
compiler_logical_location::get_internal_name_for_tree (tree decl)
{
  gcc_assert (decl);
  if (HAS_DECL_ASSEMBLER_NAME_P (decl))
    if (tree id = DECL_ASSEMBLER_NAME (decl))
      return IDENTIFIER_POINTER (id);
  return NULL;
}

/* Get what kind of SARIF logicalLocation DECL is (if any).  */

enum logical_location_kind
compiler_logical_location::get_kind_for_tree (tree decl)
{
  if (!decl)
    return LOGICAL_LOCATION_KIND_UNKNOWN;

  switch (TREE_CODE (decl))
    {
    default:
      return LOGICAL_LOCATION_KIND_UNKNOWN;
    case FUNCTION_DECL:
      return LOGICAL_LOCATION_KIND_FUNCTION;
    case PARM_DECL:
      return LOGICAL_LOCATION_KIND_PARAMETER;
    case VAR_DECL:
      return LOGICAL_LOCATION_KIND_VARIABLE;
    }
}

label_text
compiler_logical_location::get_name_for_tree_for_path_output (tree decl)
{
  gcc_assert (decl);
  const char *n = DECL_NAME (decl)
    ? identifier_to_locale (lang_hooks.decl_printable_name (decl, 2))
    : _("<anonymous>");
  return label_text::borrow (n);
}

/* class tree_logical_location : public compiler_logical_location.  */

/* Implementation of the logical_location vfuncs, using m_decl.  */

const char *
tree_logical_location::get_short_name () const
{
  gcc_assert (m_decl);
  return get_short_name_for_tree (m_decl);
}

const char *
tree_logical_location::get_name_with_scope () const
{
  gcc_assert (m_decl);
  return get_name_with_scope_for_tree (m_decl);
}

const char *
tree_logical_location::get_internal_name () const
{
  gcc_assert (m_decl);
  return get_internal_name_for_tree (m_decl);
}

enum logical_location_kind
tree_logical_location::get_kind () const
{
  gcc_assert (m_decl);
  return get_kind_for_tree (m_decl);
}

label_text
tree_logical_location::get_name_for_path_output () const
{
  gcc_assert (m_decl);
  return get_name_for_tree_for_path_output (m_decl);
}

/* class current_fndecl_logical_location : public compiler_logical_location.  */

/* Implementation of the logical_location vfuncs, using
   current_function_decl.  */

const char *
current_fndecl_logical_location::get_short_name () const
{
  gcc_assert (current_function_decl);
  return get_short_name_for_tree (current_function_decl);
}

const char *
current_fndecl_logical_location::get_name_with_scope () const
{
  gcc_assert (current_function_decl);
  return get_name_with_scope_for_tree (current_function_decl);
}

const char *
current_fndecl_logical_location::get_internal_name () const
{
  gcc_assert (current_function_decl);
  return get_internal_name_for_tree (current_function_decl);
}

enum logical_location_kind
current_fndecl_logical_location::get_kind () const
{
  gcc_assert (current_function_decl);
  return get_kind_for_tree (current_function_decl);
}

label_text
current_fndecl_logical_location::get_name_for_path_output () const
{
  gcc_assert (current_function_decl);
  return get_name_for_tree_for_path_output (current_function_decl);
}
