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

#ifndef GCC_TREE_LOGICAL_LOCATION_H
#define GCC_TREE_LOGICAL_LOCATION_H

#include "logical-location.h"

/* Abstract subclass of logical_location, with knowledge of "tree", but
   for no specific tree.  */

class compiler_logical_location : public logical_location
{
 protected:
  static const char *get_short_name_for_tree (tree);
  static const char *get_name_with_scope_for_tree (tree);
  static const char *get_internal_name_for_tree (tree);
  static enum logical_location_kind get_kind_for_tree (tree);
  static label_text get_name_for_tree_for_path_output (tree);
};

/* Concrete subclass of logical_location, with reference to a specific
   tree.  */

class tree_logical_location : public compiler_logical_location
{
public:
  tree_logical_location (tree decl) : m_decl (decl) {}

  const char *get_short_name () const final override;
  const char *get_name_with_scope () const final override;
  const char *get_internal_name () const final override;
  enum logical_location_kind get_kind () const final override;
  label_text get_name_for_path_output () const final override;

private:
  tree m_decl;
};

/* Concrete subclass of logical_location, with reference to
   current_function_decl.  */

class current_fndecl_logical_location : public compiler_logical_location
{
public:
  const char *get_short_name () const final override;
  const char *get_name_with_scope () const final override;
  const char *get_internal_name () const final override;
  enum logical_location_kind get_kind () const final override;
  label_text get_name_for_path_output () const final override;
};

#endif /* GCC_TREE_LOGICAL_LOCATION_H.  */
