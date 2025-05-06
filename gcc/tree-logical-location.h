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

#ifndef GCC_TREE_LOGICAL_LOCATION_H
#define GCC_TREE_LOGICAL_LOCATION_H

#include "logical-location.h"

/* A subclass of logical_location_manager in which the keys are
   "tree".
   Note that there is no integration with the garbage collector,
   and so logical_location instances can only be short-lived.  */
class tree_logical_location_manager : public logical_location_manager
{
public:
  const char *get_short_name (key) const final override;
  const char *get_name_with_scope (key) const final override;
  const char *get_internal_name (key) const final override;
  enum logical_location_kind get_kind (key) const final override;
  label_text get_name_for_path_output (key) const final override;
  key get_parent (key) const final override;

  static tree tree_from_key (logical_location k)
  {
    return const_cast<tree> (k.cast_to<const_tree> ());
  }
  static logical_location key_from_tree (tree node)
  {
    return logical_location::from_ptr (node);
  }
};

#endif /* GCC_TREE_LOGICAL_LOCATION_H.  */
