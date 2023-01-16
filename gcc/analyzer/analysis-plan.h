/* A class to encapsulate decisions about how the analysis should happen.
   Copyright (C) 2019-2023 Free Software Foundation, Inc.
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

#ifndef GCC_ANALYZER_ANALYSIS_PLAN_H
#define GCC_ANALYZER_ANALYSIS_PLAN_H

namespace ana {

/* A class to encapsulate decisions about how the analysis should happen.
   Examples:
   - the order in which functions should be analyzed, so that function
     summaries are created before analysis of call sites that might use
     them
   - which callgraph edges should use call summaries
   TODO: the above is a work-in-progress.  */

class analysis_plan : public log_user
{
public:
  analysis_plan (const supergraph &sg, logger *logger);
  ~analysis_plan ();

  int cmp_function (function *fun_a, function *fun_b) const;

  bool use_summary_p (const cgraph_edge *edge) const;

private:
  DISABLE_COPY_AND_ASSIGN (analysis_plan);

  const supergraph &m_sg;

  /* Result of ipa_reverse_postorder.  */
  cgraph_node **m_cgraph_node_postorder;
  int m_num_cgraph_nodes;

  /* Index of each node within the postorder ordering,
     accessed via the "m_uid" field.  */
  auto_vec<int> m_index_by_uid;
};

} // namespace ana

#endif /* GCC_ANALYZER_ANALYSIS_PLAN_H */
