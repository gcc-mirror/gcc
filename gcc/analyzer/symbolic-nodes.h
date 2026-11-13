/* Base class for svalues and regions.
   Copyright (C) 2023-2026 Free Software Foundation, Inc.
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

#ifndef GCC_ANALYZER_SYMBOLIC_NODES_H
#define GCC_ANALYZER_SYMBOLIC_NODES_H

#include "analyzer/complexity.h"

namespace ana {
namespace symbolic {

/* Abstract base class for the various tree-like purely-functional
   data structures we use during symbolic execution.

   Instances are owned by a manager and reused, with memoization/caching
   (compare to "hash consing" and value numbering).

   Instances have a complexity and a numeric ID.  */

class node
{
 public:
  using id_t = unsigned;

  virtual ~node () = default;

  const complexity &get_complexity () const { return m_complexity; }

  id_t get_id () const { return m_id; }
  static int cmp_ids (const symbolic::node *s1, const symbolic::node *s2);

 protected:
  node (complexity c, unsigned id)
  : m_complexity (c),
    m_id (id)
  {}

 private:
  complexity m_complexity;
  id_t m_id; // for deterministic sorting at this stage, for dumps
};

} // namespace symbolic

} // namespace ana

#endif /* GCC_ANALYZER_SYMBOLIC_NODES_H */
