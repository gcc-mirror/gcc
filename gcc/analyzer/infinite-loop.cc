/* Detection of infinite loops.
   Copyright (C) 2022-2024 Free Software Foundation, Inc.
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
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "fold-const.h"
#include "gcc-rich-location.h"
#include "alloc-pool.h"
#include "fibonacci_heap.h"
#include "shortest-paths.h"
#include "diagnostic-core.h"
#include "diagnostic-event-id.h"
#include "diagnostic-path.h"
#include "function.h"
#include "pretty-print.h"
#include "sbitmap.h"
#include "bitmap.h"
#include "tristate.h"
#include "ordered-hash-map.h"
#include "selftest.h"
#include "json.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"
#include "analyzer/constraint-manager.h"
#include "analyzer/sm.h"
#include "analyzer/pending-diagnostic.h"
#include "analyzer/diagnostic-manager.h"
#include "cfg.h"
#include "basic-block.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-pretty-print.h"
#include "cgraph.h"
#include "digraph.h"
#include "analyzer/supergraph.h"
#include "analyzer/program-state.h"
#include "analyzer/exploded-graph.h"
#include "analyzer/checker-path.h"
#include "analyzer/feasible-graph.h"
#include "make-unique.h"
#include "diagnostic-format-sarif.h"

/* A bundle of data characterizing a particular infinite loop
   identified within the exploded graph.  */

struct infinite_loop
{
  infinite_loop (const exploded_node &enode,
		location_t loc,
		std::vector<const exploded_edge *> &&eedges,
		logger *logger)
  : m_enode (enode),
    m_loc (loc),
    m_eedge_vec (eedges)
  {
    LOG_SCOPE (logger);
    if (logger)
      {
	logger->start_log_line ();
	logger->log_partial ("infinite loop: EN: %i", m_enode.m_index);
	for (auto eedge : m_eedge_vec)
	  {
	    logger->log_partial (" ->");
	    if (const superedge *sedge = eedge->m_sedge)
	      {
		sedge->dump_label_to_pp (logger->get_printer (), false);
	      }
	    logger->log_partial (" EN: %i", eedge->m_dest->m_index);
	  }
	logger->end_log_line ();
      }
  }

  bool
  operator== (const infinite_loop &other) const
  {
    /* Compare underlying supernode, rather than enodes, so that
       we don't get duplicates in functions that are called from
       elsewhere.  */
    return (m_enode.get_supernode () == other.m_enode.get_supernode ()
	    && m_loc == other.m_loc);
  }

  json::object *
  to_json () const
  {
    json::object *loop_obj = new json::object ();
    loop_obj->set_integer ("enode", m_enode.m_index);
    json::array *edge_arr = new json::array ();
    for (auto eedge : m_eedge_vec)
      edge_arr->append (eedge->to_json ());
    loop_obj->set ("eedges", edge_arr);
    return loop_obj;
  }

  const exploded_node &m_enode;
  location_t m_loc;
  std::vector<const exploded_edge *> m_eedge_vec;
};

/* A custom subclass of start_cfg_edge_event that rewords the
   message to indicate that the CFG edge is *always* taken on
   subsequent iterations, assuming it's been taken once. */

class perpetual_start_cfg_edge_event : public start_cfg_edge_event
{
public:
  perpetual_start_cfg_edge_event (const exploded_edge &eedge,
				  const event_loc_info &loc_info)
  : start_cfg_edge_event (eedge, loc_info)
  {
  }

  void print_desc (pretty_printer &pp) const final override
  {
    bool user_facing = !flag_analyzer_verbose_edges;
    label_text edge_desc (m_sedge->get_description (user_facing));
    if (user_facing)
      {
	if (edge_desc.get () && strlen (edge_desc.get ()) > 0)
	  {
	    label_text cond_desc
	      = maybe_describe_condition (pp_show_color (&pp));
	    if (cond_desc.get ())
	      pp_printf (&pp,
			 "%s: always following %qs branch...",
			 cond_desc.get (), edge_desc.get ());
	    else
	      pp_printf (&pp,
			 "if it ever follows %qs branch,"
			 " it will always do so...",
			 edge_desc.get ());
	  }
      }
    else
      return start_cfg_edge_event::print_desc (pp);
  }
};

class looping_back_event : public start_cfg_edge_event
{
public:
  looping_back_event (const exploded_edge &eedge,
		      const event_loc_info &loc_info)
  : start_cfg_edge_event (eedge, loc_info)
  {
  }

  void print_desc (pretty_printer &pp) const final override
  {
    pp_string (&pp, "looping back...");
  }
};

/* A subclass of pending_diagnostic for complaining about suspected
   infinite loops.  */

class infinite_loop_diagnostic
: public pending_diagnostic_subclass<infinite_loop_diagnostic>
{
public:
  infinite_loop_diagnostic (std::unique_ptr<infinite_loop> inf_loop)
    : m_inf_loop (std::move (inf_loop))
  {
    gcc_assert (m_inf_loop != nullptr);
  }

  const char *get_kind () const final override
  {
    return "infinite_loop_diagnostic";
  }

  bool operator== (const infinite_loop_diagnostic &other) const
  {
    return *m_inf_loop == *other.m_inf_loop;
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_infinite_loop;
  }

  bool emit (diagnostic_emission_context &ctxt) final override
  {
    /* "CWE-835: Loop with Unreachable Exit Condition ('Infinite Loop')". */
    ctxt.add_cwe (835);
    return ctxt.warn ("infinite loop");
  }

  bool maybe_add_custom_events_for_superedge (const exploded_edge &,
					      checker_path *)
    final override
  {
    /* Don't add any regular events; instead we add them after pruning as
       part of the "final" warning.  */
    return true;
  }

  bool
  describe_final_event (pretty_printer &pp,
			const evdesc::final_event &) final override
  {
    pp_string (&pp, "infinite loop here");
    return true;
  }

  /* Customize the location where the warning_event appears.  */
  void add_final_event (const state_machine *,
			const exploded_node *enode,
			const event_loc_info &,
			tree,
			state_machine::state_t,
			checker_path *emission_path) final override
  {
    emission_path->add_event
      (make_unique<warning_event>
       (event_loc_info (m_inf_loop->m_loc,
			enode->get_function ()->decl,
			enode->get_stack_depth ()),
	enode,
	nullptr, nullptr, nullptr));

    logger *logger = emission_path->get_logger ();

    /* EMISSION_PATH has the path to the entry of the infinite loop.
       Add extra edges showing the loop itself.  */
    for (auto eedge : m_inf_loop->m_eedge_vec)
      {
	if (logger)
	  logger->log ("EN: %i -> EN: %i",
		       eedge->m_src->m_index,
		       eedge->m_dest->m_index);
	if (!eedge->m_sedge)
	  continue;

	const cfg_superedge *cfg_sedge
	  = eedge->m_sedge->dyn_cast_cfg_superedge ();
	if (!cfg_sedge)
	  continue;

	const exploded_node *src_node = eedge->m_src;
	const program_point &src_point = src_node->get_point ();
	const exploded_node *dst_node = eedge->m_dest;
	const program_point &dst_point = dst_node->get_point ();
	const int src_stack_depth = src_point.get_stack_depth ();
	const int dst_stack_depth = dst_point.get_stack_depth ();
	const gimple *last_stmt = src_point.get_supernode ()->get_last_stmt ();

	event_loc_info loc_info_from
	  (last_stmt ? last_stmt->location : cfg_sedge->get_goto_locus (),
	   src_point.get_fndecl (),
	   src_stack_depth);
	event_loc_info loc_info_to
	  (dst_point.get_supernode ()->get_start_location (),
	   dst_point.get_fndecl (),
	   dst_stack_depth);

	if (const switch_cfg_superedge *switch_cfg_sedge
	      = cfg_sedge->dyn_cast_switch_cfg_superedge ())
	  {
	    if (switch_cfg_sedge->implicitly_created_default_p ())
	      {
		emission_path->add_event
		  (make_unique<perpetual_start_cfg_edge_event> (*eedge,
								loc_info_from));
		emission_path->add_event
		  (make_unique<end_cfg_edge_event>
		   (*eedge,
		    loc_info_to));
	      }
	  }

	if (cfg_sedge->true_value_p ())
	  {
	    emission_path->add_event
	      (make_unique<perpetual_start_cfg_edge_event> (*eedge,
							    loc_info_from));
	    emission_path->add_event
	      (make_unique<end_cfg_edge_event>
	       (*eedge,
		loc_info_to));
	  }
	else if (cfg_sedge->false_value_p ())
	  {
	    emission_path->add_event
	      (make_unique<perpetual_start_cfg_edge_event> (*eedge,
							    loc_info_from));
	    emission_path->add_event
	      (make_unique<end_cfg_edge_event>
	       (*eedge,
		loc_info_to));
	  }
	else if (cfg_sedge->back_edge_p ())
	  {
	    emission_path->add_event
	      (make_unique<looping_back_event> (*eedge, loc_info_from));
	    emission_path->add_event
	      (make_unique<end_cfg_edge_event>
	       (*eedge,
		loc_info_to));
	  }
      }
  }

  void maybe_add_sarif_properties (sarif_object &result_obj)
    const final override
  {
    sarif_property_bag &props = result_obj.get_or_create_properties ();
#define PROPERTY_PREFIX "gcc/analyzer/infinite_loop_diagnostic/"
    props.set (PROPERTY_PREFIX "inf_loop", m_inf_loop->to_json ());
#undef PROPERTY_PREFIX
  }

private:
  std::unique_ptr<infinite_loop> m_inf_loop;
};

/* If ENODE has an in-edge corresponding to a CFG backedge, return that
   exploded in-edge.
   Otherwise, return nullptr.  */

static const exploded_edge *
get_in_edge_back_edge (const exploded_node &enode)
{
  for (auto in_edge : enode.m_preds)
    {
      const superedge *sedge = in_edge->m_sedge;
      if (!sedge)
	continue;
      const cfg_superedge *cfg_sedge = sedge->dyn_cast_cfg_superedge ();
      if (!cfg_sedge)
	continue;
      if (cfg_sedge->back_edge_p ())
	return in_edge;
    }
  return nullptr;
}

/* Subclass of region_model_context that rejects conditional branches that
   aren't known for definite.  */

class infinite_loop_checking_context : public noop_region_model_context
{
public:
  infinite_loop_checking_context () : m_unusable (false) {}

  bool checking_for_infinite_loop_p () const override { return true; }
  void on_unusable_in_infinite_loop () override { m_unusable = true; }

  bool unusable_p () const { return m_unusable; }

private:
  bool m_unusable;
};

/* Determine if an infinite loop starts at ENODE.
   Return the loop if it is found, nullptr otherwise.

   Look for cycles in the exploded graph in which:
   - no externally visible work occurs
   - no escape from the cycle
   - the program state is "sufficiently concrete" at each step:
     - no unknown activity could be occurring
     - the worklist was fully drained for each enode in the cycle
       i.e. every enode in the cycle is processed.  */

static std::unique_ptr<infinite_loop>
starts_infinite_loop_p (const exploded_node &enode,
			const exploded_graph &eg,
			logger *logger)
{
  LOG_FUNC_1 (logger, "considering EN: %i", enode.m_index);

  /* Only consider enodes that have a CFG back edge as an in-edge.  */
  if (const exploded_edge *back_edge = get_in_edge_back_edge (enode))
    {
      if (logger)
	logger->log ("got backedge from EN: %i",
		     back_edge->m_src->m_index);
    }
  else
    {
      if (logger)
	logger->log ("rejecting: no backedge in in-edges");
      return nullptr;
    }

  /* Support for dumping an .infinite-loop.dot file visualizing the
     traversal for this enode.  */
  std::unique_ptr<feasible_graph> fg;
  feasible_node *curr_fnode = nullptr;

  if (flag_dump_analyzer_infinite_loop)
    fg = ::make_unique<feasible_graph> ();

  location_t first_loc = UNKNOWN_LOCATION;
  const exploded_node *iter = &enode;
  feasibility_state state (*enode.get_state ().m_region_model,
			   eg.get_supergraph ());

  if (fg)
    curr_fnode = fg->add_node (&enode, state, 0);

  hash_set<const exploded_node *> visited;
  std::vector<const exploded_edge *> eedges;
  while (1)
    {
      if (logger)
	logger->log ("iter: EN: %i", iter->m_index);
      /* Analysis bailed out before processing this node.  */
      if (iter->get_status () == exploded_node::STATUS_WORKLIST)
	{
	  if (logger)
	    logger->log ("rejecting: EN: %i is still in worklist",
			 iter->m_index);
	  return nullptr;
	}
      if (visited.contains (iter))
	{
	  /* We've looped back on ourselves.  ENODE is in the loop
	     itself if ENODE is the first place we looped back,
	     as opposed to being on a path to a loop.  */
	  if (iter == &enode)
	    {
	      if (logger)
		logger->log ("accepting: looped back to EN: %i",
			     iter->m_index);
	      if (fg)
		{
		  auto_timevar tv (TV_ANALYZER_DUMP);
		  pretty_printer pp;
		  pp_printf (&pp, "%s.en%i.infinite-loop.dot",
			     dump_base_name, enode.m_index);
		  char *filename = xstrdup (pp_formatted_text (&pp));
		  feasible_graph::dump_args_t dump_args (eg);
		  fg->dump_dot (filename, nullptr, dump_args);
		  free (filename);
		}
	      return ::make_unique<infinite_loop> (enode,
						   first_loc,
						   std::move (eedges),
						   logger);
	    }
	  else
	    {
	      if (logger)
		logger->log ("rejecting: looped back to EN: %i, not to EN: %i",
			     iter->m_index, enode.m_index);
	      return nullptr;
	    }
	}
      visited.add (iter);
      if (first_loc == UNKNOWN_LOCATION)
	{
	  location_t enode_loc = iter->get_point ().get_location ();
	  if (enode_loc != UNKNOWN_LOCATION)
	    first_loc = enode_loc;
	}

      /* Find the out-edges that are feasible, given the
	 constraints here.  */
      typedef std::pair<feasibility_state, const exploded_edge *> pair_t;
      std::vector<pair_t> succs;
      for (auto out_edge : iter->m_succs)
	{
	  log_scope s (logger, "considering out-edge",
		       "EN:%i -> EN:%i",
		       out_edge->m_src->m_index,
		       out_edge->m_dest->m_index);
	  feasibility_state next_state (state);

	  /* Use this context to require edge conditions to be known,
	     rather than be merely "possible".  */
	  infinite_loop_checking_context ctxt;
	  if (next_state.maybe_update_for_edge (logger,
						out_edge,
						&ctxt,
						nullptr))
	    succs.push_back (pair_t (next_state, out_edge));
	  if (ctxt.unusable_p ())
	    {
	      /* If we get here, then we have e.g. a gcond where
		 the condition is UNKNOWN, or a condition
		 based on a widening_svalue.  Reject such paths.  */
	      if (logger)
		logger->log ("rejecting: unusable");
	      return nullptr;
	    }
	}

      if (succs.size () != 1)
	{
	  if (logger)
	    logger->log ("rejecting: %i feasible successors",
			 (int)succs.size ());
	  return nullptr;
	}
      const feasibility_state &next_state = succs[0].first;
      const exploded_edge *out_eedge = succs[0].second;
      if (out_eedge->could_do_work_p ())
	{
	  if (logger)
	    logger->log ("rejecting: edge could do work");
	  return nullptr;
	}
      if (fg)
	{
	  feasible_node *next_fnode = fg->add_node (out_eedge->m_dest,
						    next_state,
						    fg->m_nodes.length ());
	  fg->add_edge (new feasible_edge (curr_fnode, next_fnode, out_eedge));
	  curr_fnode = next_fnode;
	}
      state = next_state;
      eedges.push_back (out_eedge);
      if (first_loc == UNKNOWN_LOCATION)
	{
	  if (out_eedge->m_sedge)
	    if (::edge cfg_edge = out_eedge->m_sedge->get_any_cfg_edge ())
	      if (cfg_edge->goto_locus > BUILTINS_LOCATION)
		first_loc = cfg_edge->goto_locus;
	}
      iter = out_eedge->m_dest;
    }
}

/* Implementation of -Wanalyzer-infinite-loop.  */

void
exploded_graph::detect_infinite_loops ()
{
  LOG_FUNC (get_logger ());
  auto_timevar tv (TV_ANALYZER_INFINITE_LOOPS);

  /* Track all enodes we've warned for; both the loop entrypoints
     and all the enodes within those loops.  */
  hash_set<const exploded_node *> warned_for;

  for (auto enode : m_nodes)
    {
      if (get_logger ())
	get_logger ()->log ("visited: %i out of %i",
			    (int)warned_for.elements (), m_nodes.length ());

      /* Only warn about the first enode we encounter in each cycle.  */
      if (warned_for.contains(enode))
	continue;

      if (std::unique_ptr<infinite_loop> inf_loop
	    = starts_infinite_loop_p (*enode, *this, get_logger ()))
	{
	  const supernode *snode = enode->get_supernode ();

	  if (get_logger ())
	    get_logger ()->log ("EN: %i from starts_infinite_loop_p",
				enode->m_index);

	  for (auto iter : inf_loop->m_eedge_vec)
	    warned_for.add (iter->m_src);
	  gcc_assert (warned_for.contains(enode));

	  if (inf_loop->m_loc == UNKNOWN_LOCATION)
	    {
	      if (get_logger ())
		get_logger ()->log
		  ("no location available for reporting infinite loop");
	      continue;
	    }

	  pending_location ploc (enode, snode, inf_loop->m_loc);
	  auto d
	    = ::make_unique<infinite_loop_diagnostic> (std::move (inf_loop));
	  get_diagnostic_manager ().add_diagnostic (ploc, std::move (d));
	}
    }
}
