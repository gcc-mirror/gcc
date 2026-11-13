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

/* A central hub for managing symbolic::node instances
   - Provides a central authority for handing out IDs for new nodes.
   - Owns the various managers of symbolic::node subclasses and provides
   a central place for them to find each other.

   We use pointers to the managers to minimize compile-time dependencies.

   This is effectively a singleton during analysis (but each unit test
   gets its own instance).  */

class node_manager
{
public:
  node_manager ();
  ~node_manager ();

  node_manager (const node_manager &) = delete;

  unsigned get_num_nodes () const { return m_next_node_id; }
  unsigned alloc_node_id () { return m_next_node_id++; }

  region_model_manager &
  get_region_model_manager () const { return *m_rmm; }

  region_model_manager &
  get_svalue_manager () const { return *m_rmm; }

  void log_stats (logger *logger, bool show_objs) const;

private:
  unsigned m_next_node_id;

  std::unique_ptr<region_model_manager> m_rmm;
};

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
