/* Subclass of logical_location_manager with knowledge of "tree".
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

static void
assert_valid_tree (const_tree node)
{
  gcc_assert (node);
  gcc_assert (DECL_P (node) || TYPE_P (node));
  gcc_assert (TREE_CODE (node) != TRANSLATION_UNIT_DECL);
}

/* class tree_logical_location_manager : public logical_location_manager.  */

const char *
tree_logical_location_manager::get_short_name (key k) const
{
  tree node = tree_from_key (k);
  assert_valid_tree (node);

  if (DECL_P (node))
    return identifier_to_locale (lang_hooks.decl_printable_name (node, 0));
  if (TYPE_P (node))
    return IDENTIFIER_POINTER (TYPE_IDENTIFIER (node));
  return nullptr;
}

const char *
tree_logical_location_manager::get_name_with_scope (key k) const
{
  tree node = tree_from_key (k);
  assert_valid_tree (node);

  if (DECL_P (node))
    return identifier_to_locale (lang_hooks.decl_printable_name (node, 1));
  if (TYPE_P (node))
    return nullptr;
  return nullptr;
}

const char *
tree_logical_location_manager::get_internal_name (key k) const
{
  tree node = tree_from_key (k);
  assert_valid_tree (node);

  if (DECL_P (node))
    {
      if (HAS_DECL_ASSEMBLER_NAME_P (node)
	  && TREE_CODE (node) != NAMESPACE_DECL) // FIXME
	if (tree id = DECL_ASSEMBLER_NAME (node))
	  return IDENTIFIER_POINTER (id);
    }
  else if (TYPE_P (node))
    return nullptr;
  return NULL;
}

enum logical_location_kind
tree_logical_location_manager::get_kind (key k) const
{
  tree node = tree_from_key (k);
  assert_valid_tree (node);

  switch (TREE_CODE (node))
    {
    default:
      return LOGICAL_LOCATION_KIND_UNKNOWN;
    case FUNCTION_DECL:
      return LOGICAL_LOCATION_KIND_FUNCTION;
    case PARM_DECL:
      return LOGICAL_LOCATION_KIND_PARAMETER;
    case VAR_DECL:
      return LOGICAL_LOCATION_KIND_VARIABLE;
    case NAMESPACE_DECL:
      return LOGICAL_LOCATION_KIND_NAMESPACE;

    case RECORD_TYPE:
      return LOGICAL_LOCATION_KIND_TYPE;
    }
}

label_text
tree_logical_location_manager::get_name_for_path_output (key k) const
{
  tree node = tree_from_key (k);
  assert_valid_tree (node);

  if (DECL_P (node))
    {
      const char *n = DECL_NAME (node)
	? identifier_to_locale (lang_hooks.decl_printable_name (node, 2))
	: _("<anonymous>");
      return label_text::borrow (n);
    }
  else if (TYPE_P (node))
    return label_text ();
  return label_text ();
}

logical_location
tree_logical_location_manager::get_parent (key k) const
{
  tree node = tree_from_key (k);
  assert_valid_tree (node);

  if (DECL_P (node))
    {
      if (!DECL_CONTEXT (node))
	return logical_location ();
      if (TREE_CODE (DECL_CONTEXT (node)) == TRANSLATION_UNIT_DECL)
	return logical_location ();
      return key_from_tree (DECL_CONTEXT (node));
    }
  else if (TYPE_P (node))
    {
      if (!TYPE_CONTEXT (node))
	return logical_location ();
      return key_from_tree (TYPE_CONTEXT (node));
    }
  return logical_location ();
}
