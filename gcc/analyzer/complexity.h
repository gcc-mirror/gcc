/* Measuring the complexity of symbolic nodes.
   Copyright (C) 2020-2026 Free Software Foundation, Inc.
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

#ifndef GCC_ANALYZER_COMPLEXITY_H
#define GCC_ANALYZER_COMPLEXITY_H

namespace ana {

/* A measurement of the complexity of a symbolic node, so that
   we can impose bounds on the growth of these tree-like structures
   and thus avoid infinite chains of analysis.  */

struct complexity
{
  complexity (unsigned num_nodes, unsigned max_depth)
  : m_num_nodes (num_nodes), m_max_depth (max_depth)
  {}

  complexity (const symbolic::node &n);
  complexity (const region *reg);
  complexity (const svalue *sval);

  static complexity
  from_pair (const complexity &c1, const complexity &c);

  static complexity
  from_vec_svalue (const vec<const svalue *> &vec);

  static complexity
  from_child_complexities (const complexity &child_complexities);

  bool
  operator== (const complexity &other) const
  {
    return (m_num_nodes == other.m_num_nodes
	    && m_max_depth == other.m_max_depth);
  }

  complexity
  operator+ (const complexity &other) const;

  complexity &
  operator+= (const complexity &other);

  /* The total number of nodes in the tree of this
     entity, including the entity itself.  */
  unsigned m_num_nodes;

  /* The maximum depth of the tree of this entity, including the
     entity itself.  */
  unsigned m_max_depth;
};

} // namespace ana

#endif /* GCC_ANALYZER_COMPLEXITY_H */
