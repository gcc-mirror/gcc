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

#include "analyzer/common.h"

#include "options.h"
#include "cgraph.h"
#include "cfg.h"
#include "digraph.h"

#include "analyzer/analyzer-logging.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/complexity.h"
#include "analyzer/svalue.h"
#include "analyzer/region.h"

#include "selftest.h"

#if ENABLE_ANALYZER

namespace ana {

/* struct complexity.  */

/* Get complexity for a new node that references NODE
   (the complexity of NODE, plus one for the new node).  */

complexity::complexity (const symbolic::node &node)
: m_num_nodes (node.get_complexity ().m_num_nodes + 1),
  m_max_depth (node.get_complexity ().m_max_depth + 1)
{
}

/* Get complexity for a new node that references REG
   (the complexity of REG, plus one for the new node).  */

complexity::complexity (const region *reg)
: m_num_nodes (reg->get_complexity ().m_num_nodes + 1),
  m_max_depth (reg->get_complexity ().m_max_depth + 1)
{
}

/* Get complexity for a new node that references SVAL.
   (the complexity of SVAL, plus one for the new node).  */

complexity::complexity (const svalue *sval)
: m_num_nodes (sval->get_complexity ().m_num_nodes + 1),
  m_max_depth (sval->get_complexity ().m_max_depth + 1)
{
}

/* Get complexity for a new node that references nodes with complexity
   C1 and C2.  */

complexity
complexity::from_pair (const complexity &c1, const complexity &c2)
{
  return complexity (c1.m_num_nodes + c2.m_num_nodes + 1,
		     MAX (c1.m_max_depth, c2.m_max_depth) + 1);
}

/* Get complexity for a new node that references the svalues in VEC.  */

complexity
complexity::from_vec_svalue (const vec<const svalue *> &vec)
{
  unsigned num_nodes = 0;
  unsigned max_depth = 0;
  for (auto iter_sval : vec)
    {
      const complexity &iter_c = iter_sval->get_complexity ();
      num_nodes += iter_c.m_num_nodes;
      max_depth = MAX (max_depth, iter_c.m_max_depth);
    }
  return complexity (num_nodes + 1, max_depth + 1);
}

/* Get complexity for a new node which has children with overall
   complexity CHILD_COMPLEXITIES.  */

complexity
complexity::from_child_complexities (const complexity &child_complexities)
{
  /* One extra node for the node itself; one more level of depth.  */
  return complexity (child_complexities.m_num_nodes + 1,
		     child_complexities.m_max_depth + 1);
}

/* The sum of two complexity instances is the sum of their
   numbers of nodes and the maximum of the maxima of their depths.  */

complexity
complexity::operator+ (const complexity &other) const
{
  return complexity (m_num_nodes + other.m_num_nodes,
		     std::max (m_max_depth, other.m_max_depth));
}

complexity &
complexity::operator+= (const complexity &other)
{
  *this = *this + other;
  return *this;
}

#if CHECKING_P

namespace selftest {

void
analyzer_complexity_cc_tests ()
{
  ASSERT_EQ (complexity (1, 1) + complexity (1, 1),
	     complexity (2, 1));

  ASSERT_EQ (complexity (3, 2) + complexity (3, 1),
	     complexity (6, 2));

  ASSERT_EQ (complexity::from_child_complexities (complexity (3, 2)),
	     complexity (4, 3));
}

} // namespace selftest

#endif /* CHECKING_P */


} // namespace ana

#endif /* #if ENABLE_ANALYZER */
