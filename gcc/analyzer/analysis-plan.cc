/* A class to encapsulate decisions about how the analysis should happen.
   Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "options.h"
#include "cgraph.h"
#include "timevar.h"
#include "ipa-utils.h"
#include "function.h"
#include "analyzer/analyzer.h"
#include "diagnostic-core.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/analysis-plan.h"
#include "ordered-hash-map.h"
#include "options.h"
#include "cgraph.h"
#include "function.h"
#include "cfg.h"
#include "basic-block.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "digraph.h"
#include "analyzer/supergraph.h"

#if ENABLE_ANALYZER

/* class analysis_plan.  */

/* analysis_plan's ctor.  */

analysis_plan::analysis_plan (const supergraph &sg, logger *logger)
: log_user (logger), m_sg (sg),
  m_cgraph_node_postorder (XCNEWVEC (struct cgraph_node *,
				     symtab->cgraph_count)),
  m_index_by_uid (symtab->cgraph_max_uid)
{
  LOG_SCOPE (logger);
  auto_timevar time (TV_ANALYZER_PLAN);

  m_num_cgraph_nodes = ipa_reverse_postorder (m_cgraph_node_postorder);
  gcc_assert (m_num_cgraph_nodes == symtab->cgraph_count);
  if (get_logger_file ())
    ipa_print_order (get_logger_file (),
		     "analysis_plan", m_cgraph_node_postorder,
		     m_num_cgraph_nodes);

  /* Populate m_index_by_uid.  */
  for (int i = 0; i < symtab->cgraph_max_uid; i++)
    m_index_by_uid.quick_push (-1);
  for (int i = 0; i < m_num_cgraph_nodes; i++)
    {
      gcc_assert (m_cgraph_node_postorder[i]->get_uid ()
		  < symtab->cgraph_max_uid);
      m_index_by_uid[m_cgraph_node_postorder[i]->get_uid ()] = i;
    }
}

/* analysis_plan's dtor.  */

analysis_plan::~analysis_plan ()
{
  free (m_cgraph_node_postorder);
}

/* Comparator for use by the exploded_graph's worklist, to order FUN_A
   and FUN_B so that functions that are to be summarized are visited
   before the summary is needed (based on a sort of the callgraph).  */

int
analysis_plan::cmp_function (function *fun_a, function *fun_b) const
{
  cgraph_node *node_a = cgraph_node::get (fun_a->decl);
  cgraph_node *node_b = cgraph_node::get (fun_b->decl);

  int idx_a = m_index_by_uid[node_a->get_uid ()];
  int idx_b = m_index_by_uid[node_b->get_uid ()];

  return idx_b - idx_a;
}

/* Return true if the call EDGE should be analyzed using a call summary.
   Return false if it should be analyzed using a full call and return.  */

bool
analysis_plan::use_summary_p (const cgraph_edge *edge) const
{
  /* Don't use call summaries if -fno-analyzer-call-summaries.  */
  if (!flag_analyzer_call_summaries)
    return false;

  /* TODO: don't count callsites each time.  */
  int num_call_sites = 0;
  const cgraph_node *callee = edge->callee;
  for (cgraph_edge *edge = callee->callers; edge; edge = edge->next_caller)
    ++num_call_sites;

  /* Don't use a call summary if there's only one call site.  */
  if (num_call_sites <= 1)
    return false;

  /* Require the callee to be sufficiently complex to be worth
     summarizing.  */
  const function *fun
    = const_cast <cgraph_node *> (callee)->ultimate_alias_target ()->get_fun ();
  /* TODO(stage1): can ultimate_alias_target be made const?  */

  if ((int)m_sg.get_num_snodes (fun)
      < param_analyzer_min_snodes_for_call_summary)
    return false;

  return true;
}

#endif /* #if ENABLE_ANALYZER */
