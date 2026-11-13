/* Simple wrapper around "tree" for types.
   Copyright (C) 2026 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_ANALYZER_TYPE_H
#define GCC_ANALYZER_TYPE_H

namespace ana {

/* Simple wrapper around "tree" for types.  */

class type
{
public:
  type (const_tree tree_node)
  : m_tree_node (tree_node)
  {
    if (tree_node)
      gcc_assert (TYPE_P (tree_node));
  }

  const_tree
  get_tree_node () const { return m_tree_node; }

private:
  const_tree m_tree_node;
};

} // namespace ana

#endif /* GCC_ANALYZER_TYPE_H */
