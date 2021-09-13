/* Classes for saving, deduplicating, and emitting analyzer diagnostics.
   Copyright (C) 2019-2021 Free Software Foundation, Inc.
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
#include "pretty-print.h"
#include "gcc-rich-location.h"
#include "gimple-pretty-print.h"
#include "function.h"
#include "diagnostic-core.h"
#include "diagnostic-event-id.h"
#include "diagnostic-path.h"
#include "alloc-pool.h"
#include "fibonacci_heap.h"
#include "shortest-paths.h"
#include "sbitmap.h"
#include "bitmap.h"
#include "tristate.h"
#include "selftest.h"
#include "ordered-hash-map.h"
#include "json.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/sm.h"
#include "analyzer/pending-diagnostic.h"
#include "analyzer/diagnostic-manager.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"
#include "analyzer/constraint-manager.h"
#include "cfg.h"
#include "basic-block.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "cgraph.h"
#include "digraph.h"
#include "analyzer/supergraph.h"
#include "analyzer/program-state.h"
#include "analyzer/exploded-graph.h"
#include "analyzer/trimmed-graph.h"
#include "analyzer/feasible-graph.h"
#include "analyzer/checker-path.h"
#include "analyzer/reachability.h"

#if ENABLE_ANALYZER

namespace ana {

class feasible_worklist;

/* State for finding the shortest feasible exploded_path for a
   saved_diagnostic.
   This is shared between all diagnostics, so that we avoid repeating work.  */

class epath_finder
{
public:
  epath_finder (const exploded_graph &eg)
  : m_eg (eg),
    m_sep (NULL)
  {
    /* This is shared by all diagnostics, but only needed if
       !flag_analyzer_feasibility.  */
    if (!flag_analyzer_feasibility)
      m_sep = new shortest_exploded_paths (eg, eg.get_origin (),
					   SPS_FROM_GIVEN_ORIGIN);
  }

  ~epath_finder () { delete m_sep; }

  logger *get_logger () const { return m_eg.get_logger (); }

  exploded_path *get_best_epath (const exploded_node *target_enode,
				 const char *desc, unsigned diag_idx,
				 feasibility_problem **out_problem);

private:
  DISABLE_COPY_AND_ASSIGN(epath_finder);

  exploded_path *explore_feasible_paths (const exploded_node *target_enode,
					 const char *desc, unsigned diag_idx);
  bool process_worklist_item (feasible_worklist *worklist,
			      const trimmed_graph &tg,
			      feasible_graph *fg,
			      const exploded_node *target_enode,
			      unsigned diag_idx,
			      exploded_path **out_best_path) const;
  void dump_trimmed_graph (const exploded_node *target_enode,
			   const char *desc, unsigned diag_idx,
			   const trimmed_graph &tg,
			   const shortest_paths<eg_traits, exploded_path> &sep);
  void dump_feasible_graph (const exploded_node *target_enode,
			    const char *desc, unsigned diag_idx,
			    const feasible_graph &fg);

  const exploded_graph &m_eg;
  shortest_exploded_paths *m_sep;
};

/* class epath_finder.  */

/* Get the "best" exploded_path for reaching ENODE from the origin,
   returning ownership of it to the caller.

   Ideally we want to report the shortest feasible path.
   Return NULL if we could not find a feasible path
   (when flag_analyzer_feasibility is true).

   If flag_analyzer_feasibility is false, then simply return the
   shortest path.

   Use DESC and DIAG_IDX when logging.

   Write any feasibility_problem to *OUT_PROBLEM.  */

exploded_path *
epath_finder::get_best_epath (const exploded_node *enode,
			      const char *desc, unsigned diag_idx,
			      feasibility_problem **out_problem)
{
  logger *logger = get_logger ();
  LOG_SCOPE (logger);

  unsigned snode_idx = enode->get_supernode ()->m_index;
  if (logger)
    logger->log ("considering %qs at EN: %i, SN: %i (sd: %i)",
		 desc, enode->m_index, snode_idx, diag_idx);

  /* State-merging means that not every path in the egraph corresponds
     to a feasible one w.r.t. states.

     We want to find the shortest feasible path from the origin to ENODE
     in the egraph.  */

  if (flag_analyzer_feasibility)
    {
      /* Attempt to find the shortest feasible path using feasible_graph.  */
      if (logger)
	logger->log ("trying to find shortest feasible path");
      if (exploded_path *epath = explore_feasible_paths (enode, desc, diag_idx))
	{
	  if (logger)
	    logger->log ("accepting %qs at EN: %i, SN: %i (sd: %i)"
			 " with feasible path (length: %i)",
			 desc, enode->m_index, snode_idx, diag_idx,
			 epath->length ());
	  return epath;
	}
      else
	{
	  if (logger)
	    logger->log ("rejecting %qs at EN: %i, SN: %i (sd: %i)"
			 " due to not finding feasible path",
			 desc, enode->m_index, snode_idx, diag_idx);
	  return NULL;
	}
    }
  else
    {
      /* As a crude approximation to shortest feasible path, simply find
	 the shortest path, and note whether it is feasible.
	 There could be longer feasible paths within the egraph, so this
	 approach would lead to diagnostics being falsely rejected
	 (PR analyzer/96374).  */
      if (logger)
	logger->log ("trying to find shortest path ignoring feasibility");
      gcc_assert (m_sep);
      exploded_path *epath
	= new exploded_path (m_sep->get_shortest_path (enode));
      if (epath->feasible_p (logger, out_problem, m_eg.get_engine (), &m_eg))
	{
	  if (logger)
	    logger->log ("accepting %qs at EN: %i, SN: %i (sn: %i)"
			 " with feasible path (length: %i)",
			 desc, enode->m_index, snode_idx, diag_idx,
			 epath->length ());
	}
      else
	{
	  if (logger)
	    logger->log ("accepting %qs at EN: %i, SN: %i (sn: %i) (length: %i)"
			 " despite infeasible path (due to %qs)",
			 desc, enode->m_index, snode_idx, diag_idx,
			 epath->length (),
			 "-fno-analyzer-feasibility");
	}
      return epath;
    }
}

/* A class for managing the worklist of feasible_nodes in
   epath_finder::explore_feasible_paths, prioritizing them
   so that shorter paths appear earlier in the queue.  */

class feasible_worklist
{
public:
  feasible_worklist (const shortest_paths<eg_traits, exploded_path> &sep)
  : m_queue (key_t (*this, NULL)),
    m_sep (sep)
  {
  }

  feasible_node *take_next () { return m_queue.extract_min (); }

  void add_node (feasible_node *fnode)
  {
    m_queue.insert (key_t (*this, fnode), fnode);
  }

private:
  struct key_t
  {
    key_t (const feasible_worklist &w, feasible_node *fnode)
    : m_worklist (w), m_fnode (fnode)
    {}

    bool operator< (const key_t &other) const
    {
      return cmp (*this, other) < 0;
    }

    bool operator== (const key_t &other) const
    {
      return cmp (*this, other) == 0;
    }

    bool operator> (const key_t &other) const
    {
      return !(*this == other || *this < other);
    }

  private:
    static int cmp (const key_t &ka, const key_t &kb)
    {
      /* Choose the node for which if the remaining path were feasible,
	 it would be the shortest path (summing the length of the
	 known-feasible path so far with that of the remaining
	 possibly-feasible path).  */
      int ca = ka.m_worklist.get_estimated_cost (ka.m_fnode);
      int cb = kb.m_worklist.get_estimated_cost (kb.m_fnode);
      return ca - cb;
    }

    const feasible_worklist &m_worklist;
    feasible_node *m_fnode;
  };

  /* Get the estimated length of a path involving FNODE from
     the origin to the target enode.
     Sum the length of the known-feasible path so far with
     that of the remaining possibly-feasible path.  */

  int get_estimated_cost (const feasible_node *fnode) const
  {
    unsigned length_so_far = fnode->get_path_length ();
    int shortest_remaining_path
      = m_sep.get_shortest_distance (fnode->get_inner_node ());

    gcc_assert (shortest_remaining_path >= 0);
    /* This should be true since we're only exploring nodes within
       the trimmed graph (and we anticipate it being much smaller
       than this, and thus not overflowing the sum).  */
    gcc_assert (shortest_remaining_path < INT_MAX);

    return length_so_far + shortest_remaining_path;
  }

  /* Priority queue, backed by a fibonacci_heap.  */
  typedef fibonacci_heap<key_t, feasible_node> queue_t;
  queue_t m_queue;
  const shortest_paths<eg_traits, exploded_path> &m_sep;
};

/* When we're building the exploded graph we want to simplify
   overly-complicated symbolic values down to "UNKNOWN" to try to avoid
   state explosions and unbounded chains of exploration.

   However, when we're building the feasibility graph for a diagnostic
   (actually a tree), we don't want UNKNOWN values, as conditions on them
   are also unknown: we don't want to have a contradiction such as a path
   where (VAL != 0) and then (VAL == 0) along the same path.

   Hence this is an RAII class for temporarily disabling complexity-checking
   in the region_model_manager, for use within
   epath_finder::explore_feasible_paths.  */

class auto_disable_complexity_checks
{
public:
  auto_disable_complexity_checks (region_model_manager *mgr) : m_mgr (mgr)
  {
    m_mgr->disable_complexity_check ();
  }
  ~auto_disable_complexity_checks ()
  {
    m_mgr->enable_complexity_check ();
  }
private:
  region_model_manager *m_mgr;
};

/* Attempt to find the shortest feasible path from the origin to
   TARGET_ENODE by iteratively building a feasible_graph, in which
   every path to a feasible_node is feasible by construction.

   We effectively explore the tree of feasible paths in order of shortest
   path until we either find a feasible path to TARGET_ENODE, or hit
   a limit and give up.

   Preliminaries:
   - Find the shortest path from each node to the TARGET_ENODE (without
   checking feasibility), so that we can prioritize our worklist.
   - Construct a trimmed_graph: the subset of nodes/edges that
   are on a path that eventually reaches TARGET_ENODE.  We will only need
   to consider these when considering the shortest feasible path.

   Build a feasible_graph, in which every path to a feasible_node
   is feasible by construction.
   We use a worklist to flatten the exploration into an iteration.
   Starting at the origin, find feasible out-edges within the trimmed graph.
   At each stage, choose the node for which if the remaining path were feasible,
   it would be the shortest path (summing the length of the known-feasible path
   so far with that of the remaining possibly-feasible path).
   This way, the first feasible path we find to TARGET_ENODE is the shortest.
   We start by trying the shortest possible path, but if that fails,
   we explore progressively longer paths, eventually trying iterations through
   loops.  The exploration is captured in the feasible_graph, which can be
   dumped as a .dot file to visualize the exploration.  The indices of the
   feasible_nodes show the order in which they were created.

   This is something of a brute-force approach, but the trimmed_graph
   hopefully keeps the complexity manageable.

   Terminate with failure when the number of infeasible edges exceeds
   a threshold (--param=analyzer-max-infeasible-edges=).
   This is guaranteed to eventually lead to terminatation, as
   we can't keep creating feasible nodes without eventually
   either reaching an infeasible edge, or reaching the
   TARGET_ENODE.  Specifically, there can't be a cycle of
   feasible edges that doesn't reach the target_enode without
   an out-edge that either fails feasibility or gets closer
   to the TARGET_ENODE: on each iteration we are either:
   - effectively getting closer to the TARGET_ENODE (which can't
     continue forever without reaching the target), or
   - getting monotonically closer to the termination threshold.  */

exploded_path *
epath_finder::explore_feasible_paths (const exploded_node *target_enode,
				      const char *desc, unsigned diag_idx)
{
  logger *logger = get_logger ();
  LOG_SCOPE (logger);

  region_model_manager *mgr = m_eg.get_engine ()->get_model_manager ();

  /* Determine the shortest path to TARGET_ENODE from each node in
     the exploded graph.  */
  shortest_paths<eg_traits, exploded_path> sep
    (m_eg, target_enode, SPS_TO_GIVEN_TARGET);

  /* Construct a trimmed_graph: the subset of nodes/edges that
     are on a path that eventually reaches TARGET_ENODE.
     We only need to consider these when considering the shortest
     feasible path.  */
  trimmed_graph tg (m_eg, target_enode);

  if (flag_dump_analyzer_feasibility)
    dump_trimmed_graph (target_enode, desc, diag_idx, tg, sep);

  feasible_graph fg;
  feasible_worklist worklist (sep);

  /* Populate the worklist with the origin node.  */
  {
    feasibility_state init_state (mgr, m_eg.get_supergraph ());
    feasible_node *origin = fg.add_node (m_eg.get_origin (), init_state, 0);
    worklist.add_node (origin);
  }

  /* Iteratively explore the tree of feasible paths in order of shortest
     path until we either find a feasible path to TARGET_ENODE, or hit
     a limit.  */

  /* Set this if we find a feasible path to TARGET_ENODE.  */
  exploded_path *best_path = NULL;

  {
    auto_disable_complexity_checks sentinel (mgr);

    while (process_worklist_item (&worklist, tg, &fg, target_enode, diag_idx,
				  &best_path))
      {
	/* Empty; the work is done within process_worklist_item.  */
      }
  }

  if (logger)
    {
      logger->log ("tg for sd: %i:", diag_idx);
      logger->inc_indent ();
      tg.log_stats (logger);
      logger->dec_indent ();

      logger->log ("fg for sd: %i:", diag_idx);
      logger->inc_indent ();
      fg.log_stats (logger);
      logger->dec_indent ();
    }

  /* Dump the feasible_graph.  */
  if (flag_dump_analyzer_feasibility)
    dump_feasible_graph (target_enode, desc, diag_idx, fg);

  return best_path;
}

/* Process the next item in WORKLIST, potentially adding new items
   based on feasible out-edges, and extending FG accordingly.
   Use TG to ignore out-edges that don't lead to TARGET_ENODE.
   Return true if the worklist processing should continue.
   Return false if the processing of the worklist should stop
   (either due to reaching TARGET_ENODE, or hitting a limit).
   Write to *OUT_BEST_PATH if stopping due to finding a feasible path
   to TARGET_ENODE.  */

bool
epath_finder::process_worklist_item (feasible_worklist *worklist,
				     const trimmed_graph &tg,
				     feasible_graph *fg,
				     const exploded_node *target_enode,
				     unsigned diag_idx,
				     exploded_path **out_best_path) const
{
  logger *logger = get_logger ();

  feasible_node *fnode = worklist->take_next ();
  if (!fnode)
    {
      if (logger)
	logger->log ("drained worklist for sd: %i"
		     " without finding feasible path",
		     diag_idx);
      return false;
    }

  log_scope s (logger, "fg worklist item",
	       "considering FN: %i (EN: %i) for sd: %i",
	       fnode->get_index (), fnode->get_inner_node ()->m_index,
	       diag_idx);

  /* Iterate through all out-edges from this item.  */
  unsigned i;
  exploded_edge *succ_eedge;
  FOR_EACH_VEC_ELT (fnode->get_inner_node ()->m_succs, i, succ_eedge)
    {
      log_scope s (logger, "edge", "considering edge: EN:%i -> EN:%i",
		   succ_eedge->m_src->m_index,
		   succ_eedge->m_dest->m_index);
      /* Reject edges that aren't in the trimmed graph.  */
      if (!tg.contains_p (succ_eedge))
	{
	  if (logger)
	    logger->log ("rejecting: not in trimmed graph");
	  continue;
	}

      feasibility_state succ_state (fnode->get_state ());
      rejected_constraint *rc = NULL;
      if (succ_state.maybe_update_for_edge (logger, succ_eedge, &rc))
	{
	  gcc_assert (rc == NULL);
	  feasible_node *succ_fnode
	    = fg->add_node (succ_eedge->m_dest,
			    succ_state,
			    fnode->get_path_length () + 1);
	  if (logger)
	    logger->log ("accepting as FN: %i", succ_fnode->get_index ());
	  fg->add_edge (new feasible_edge (fnode, succ_fnode, succ_eedge));

	  /* Have we reached TARGET_ENODE?  */
	  if (succ_fnode->get_inner_node () == target_enode)
	    {
	      if (logger)
		logger->log ("success: got feasible path to EN: %i (sd: %i)"
			     " (length: %i)",
			     target_enode->m_index, diag_idx,
			     succ_fnode->get_path_length ());
	      *out_best_path = fg->make_epath (succ_fnode);
	      /* Success: stop the worklist iteration.  */
	      return false;
	    }
	  else
	    worklist->add_node (succ_fnode);
	}
      else
	{
	  if (logger)
	    logger->log ("infeasible");
	  gcc_assert (rc);
	  fg->add_feasibility_problem (fnode,
				       succ_eedge,
				       rc);

	  /* Give up if there have been too many infeasible edges.  */
	  if (fg->get_num_infeasible ()
	      > (unsigned)param_analyzer_max_infeasible_edges)
	    {
	      if (logger)
		logger->log ("too many infeasible edges (%i); giving up",
			     fg->get_num_infeasible ());
	      return false;
	    }
	}
    }

  /* Continue the worklist iteration.  */
  return true;
}

/* Helper class for epath_finder::dump_trimmed_graph
   to dump extra per-node information.
   Use SEP to add the length of the shortest path from each
   node to the target node to each node's dump.  */

class dump_eg_with_shortest_path : public eg_traits::dump_args_t
{
public:
  dump_eg_with_shortest_path
    (const exploded_graph &eg,
     const shortest_paths<eg_traits, exploded_path> &sep)
  : dump_args_t (eg),
    m_sep (sep)
  {
  }

  void dump_extra_info (const exploded_node *enode,
			pretty_printer *pp) const FINAL OVERRIDE
  {
    pp_printf (pp, "sp: %i", m_sep.get_shortest_path (enode).length ());
    pp_newline (pp);
  }

private:
  const shortest_paths<eg_traits, exploded_path> &m_sep;
};

/* Dump TG to "BASE_NAME.DESC.DIAG_IDX.to-enN.tg.dot",
   annotating each node with the length of the shortest path
   from that node to TARGET_ENODE (using SEP).  */

void
epath_finder::
dump_trimmed_graph (const exploded_node *target_enode,
		    const char *desc, unsigned diag_idx,
		    const trimmed_graph &tg,
		    const shortest_paths<eg_traits, exploded_path> &sep)
{
  auto_timevar tv (TV_ANALYZER_DUMP);
  dump_eg_with_shortest_path inner_args (m_eg, sep);
  trimmed_graph::dump_args_t args (inner_args);
  pretty_printer pp;
  pp_printf (&pp, "%s.%s.%i.to-en%i.tg.dot",
	     dump_base_name, desc, diag_idx, target_enode->m_index);
  char *filename = xstrdup (pp_formatted_text (&pp));
  tg.dump_dot (filename, NULL, args);
  free (filename);
}

/* Dump FG to "BASE_NAME.DESC.DIAG_IDX.to-enN.fg.dot".  */

void
epath_finder::dump_feasible_graph (const exploded_node *target_enode,
				   const char *desc, unsigned diag_idx,
				   const feasible_graph &fg)
{
  auto_timevar tv (TV_ANALYZER_DUMP);
  exploded_graph::dump_args_t inner_args (m_eg);
  feasible_graph::dump_args_t args (inner_args);
  pretty_printer pp;
  pp_printf (&pp, "%s.%s.%i.to-en%i.fg.dot",
	     dump_base_name, desc, diag_idx, target_enode->m_index);
  char *filename = xstrdup (pp_formatted_text (&pp));
  fg.dump_dot (filename, NULL, args);
  free (filename);
}

/* class saved_diagnostic.  */

/* saved_diagnostic's ctor.
   Take ownership of D and STMT_FINDER.  */

saved_diagnostic::saved_diagnostic (const state_machine *sm,
				    const exploded_node *enode,
				    const supernode *snode, const gimple *stmt,
				    stmt_finder *stmt_finder,
				    tree var,
				    const svalue *sval,
				    state_machine::state_t state,
				    pending_diagnostic *d,
				    unsigned idx)
: m_sm (sm), m_enode (enode), m_snode (snode), m_stmt (stmt),
 /* stmt_finder could be on-stack; we want our own copy that can
    outlive that.  */
  m_stmt_finder (stmt_finder ? stmt_finder->clone () : NULL),
  m_var (var), m_sval (sval), m_state (state),
  m_d (d), m_trailing_eedge (NULL),
  m_idx (idx),
  m_best_epath (NULL), m_problem (NULL)
{
  gcc_assert (m_stmt || m_stmt_finder);

  /* We must have an enode in order to be able to look for paths
     through the exploded_graph to this diagnostic.  */
  gcc_assert (m_enode);
}

/* saved_diagnostic's dtor.  */

saved_diagnostic::~saved_diagnostic ()
{
  delete m_stmt_finder;
  delete m_d;
  delete m_best_epath;
  delete m_problem;
}

bool
saved_diagnostic::operator== (const saved_diagnostic &other) const
{
  return (m_sm == other.m_sm
	  /* We don't compare m_enode.  */
	  && m_snode == other.m_snode
	  && m_stmt == other.m_stmt
	  /* We don't compare m_stmt_finder.  */
	  && pending_diagnostic::same_tree_p (m_var, other.m_var)
	  && m_state == other.m_state
	  && m_d->equal_p (*other.m_d)
	  && m_trailing_eedge == other.m_trailing_eedge);
}

/* Return a new json::object of the form
   {"sm": optional str,
    "enode": int,
    "snode": int,
    "sval": optional str,
    "state": optional str,
    "path_length": optional int,
    "pending_diagnostic": str,
    "idx": int}.  */

json::object *
saved_diagnostic::to_json () const
{
  json::object *sd_obj = new json::object ();

  if (m_sm)
    sd_obj->set ("sm", new json::string (m_sm->get_name ()));
  sd_obj->set ("enode", new json::integer_number (m_enode->m_index));
  sd_obj->set ("snode", new json::integer_number (m_snode->m_index));
  if (m_sval)
    sd_obj->set ("sval", m_sval->to_json ());
  if (m_state)
    sd_obj->set ("state", m_state->to_json ());
  if (m_best_epath)
    sd_obj->set ("path_length", new json::integer_number (get_epath_length ()));
  sd_obj->set ("pending_diagnostic", new json::string (m_d->get_kind ()));
  sd_obj->set ("idx", new json::integer_number (m_idx));

  /* We're not yet JSONifying the following fields:
     const gimple *m_stmt;
     stmt_finder *m_stmt_finder;
     tree m_var;
     exploded_edge *m_trailing_eedge;
     enum status m_status;
     feasibility_problem *m_problem;
  */

  return sd_obj;
}

/* Use PF to find the best exploded_path for this saved_diagnostic,
   and store it in m_best_epath.
   If m_stmt is still NULL, use m_stmt_finder on the epath to populate
   m_stmt.
   Return true if a best path was found.  */

bool
saved_diagnostic::calc_best_epath (epath_finder *pf)
{
  logger *logger = pf->get_logger ();
  LOG_SCOPE (logger);
  delete m_best_epath;
  delete m_problem;
  m_problem = NULL;

  m_best_epath = pf->get_best_epath (m_enode, m_d->get_kind (), m_idx,
				     &m_problem);

  /* Handle failure to find a feasible path.  */
  if (m_best_epath == NULL)
    return false;

  gcc_assert (m_best_epath);
  if (m_stmt == NULL)
    {
      gcc_assert (m_stmt_finder);
      m_stmt = m_stmt_finder->find_stmt (*m_best_epath);
    }
  gcc_assert (m_stmt);

  return true;
}

unsigned
saved_diagnostic::get_epath_length () const
{
  gcc_assert (m_best_epath);
  return m_best_epath->length ();
}

/* Record that OTHER (and its duplicates) are duplicates
   of this saved_diagnostic.  */

void
saved_diagnostic::add_duplicate (saved_diagnostic *other)
{
  gcc_assert (other);
  m_duplicates.reserve (m_duplicates.length ()
			+ other->m_duplicates.length ()
			+ 1);
  m_duplicates.splice (other->m_duplicates);
  other->m_duplicates.truncate (0);
  m_duplicates.safe_push (other);
}

/* Return true if this diagnostic supercedes OTHER, and that OTHER should
   therefore not be emitted.  */

bool
saved_diagnostic::supercedes_p (const saved_diagnostic &other) const
{
  /* They should be at the same stmt.  */
  if (m_stmt != other.m_stmt)
    return false;
  return m_d->supercedes_p (*other.m_d);
}

/* State for building a checker_path from a particular exploded_path.
   In particular, this precomputes reachability information: the set of
   source enodes for which a path be found to the diagnostic enode.  */

class path_builder
{
public:
  path_builder (const exploded_graph &eg,
		const exploded_path &epath,
		const feasibility_problem *problem,
		const saved_diagnostic &sd)
  : m_eg (eg),
    m_diag_enode (epath.get_final_enode ()),
    m_sd (sd),
    m_reachability (eg, m_diag_enode),
    m_feasibility_problem (problem)
  {}

  const exploded_node *get_diag_node () const { return m_diag_enode; }

  pending_diagnostic *get_pending_diagnostic () const
  {
    return m_sd.m_d;
  }

  bool reachable_from_p (const exploded_node *src_enode) const
  {
    return m_reachability.reachable_from_p (src_enode);
  }

  const extrinsic_state &get_ext_state () const { return m_eg.get_ext_state (); }

  const feasibility_problem *get_feasibility_problem () const
  {
    return m_feasibility_problem;
  }

  const state_machine *get_sm () const { return m_sd.m_sm; }

private:
  typedef reachability<eg_traits> enode_reachability;

  const exploded_graph &m_eg;

  /* The enode where the diagnostic occurs.  */
  const exploded_node *m_diag_enode;

  const saved_diagnostic &m_sd;

  /* Precompute all enodes from which the diagnostic is reachable.  */
  enode_reachability m_reachability;

  const feasibility_problem *m_feasibility_problem;
};

/* class diagnostic_manager.  */

/* diagnostic_manager's ctor.  */

diagnostic_manager::diagnostic_manager (logger *logger, engine *eng,
					int verbosity)
: log_user (logger), m_eng (eng), m_verbosity (verbosity)
{
}

/* Queue pending_diagnostic D at ENODE for later emission.  */

void
diagnostic_manager::add_diagnostic (const state_machine *sm,
				    exploded_node *enode,
				    const supernode *snode, const gimple *stmt,
				    stmt_finder *finder,
				    tree var,
				    const svalue *sval,
				    state_machine::state_t state,
				    pending_diagnostic *d)
{
  LOG_FUNC (get_logger ());

  /* We must have an enode in order to be able to look for paths
     through the exploded_graph to the diagnostic.  */
  gcc_assert (enode);

  saved_diagnostic *sd
    = new saved_diagnostic (sm, enode, snode, stmt, finder, var, sval,
			    state, d, m_saved_diagnostics.length ());
  m_saved_diagnostics.safe_push (sd);
  enode->add_diagnostic (sd);
  if (get_logger ())
    log ("adding saved diagnostic %i at SN %i to EN %i: %qs",
	 sd->get_index (),
	 snode->m_index, enode->m_index, d->get_kind ());
}

/* Queue pending_diagnostic D at ENODE for later emission.  */

void
diagnostic_manager::add_diagnostic (exploded_node *enode,
				    const supernode *snode, const gimple *stmt,
				    stmt_finder *finder,
				    pending_diagnostic *d)
{
  gcc_assert (enode);
  add_diagnostic (NULL, enode, snode, stmt, finder, NULL_TREE, NULL, 0, d);
}

/* Return a new json::object of the form
   {"diagnostics"  : [obj for saved_diagnostic]}.  */

json::object *
diagnostic_manager::to_json () const
{
  json::object *dm_obj = new json::object ();

  {
    json::array *sd_arr = new json::array ();
    int i;
    saved_diagnostic *sd;
    FOR_EACH_VEC_ELT (m_saved_diagnostics, i, sd)
      sd_arr->append (sd->to_json ());
    dm_obj->set ("diagnostics", sd_arr);
  }

  return dm_obj;
}

/* A class for identifying sets of duplicated pending_diagnostic.

   We want to find the simplest saved_diagnostic amongst those that share a
   dedupe_key.  */

class dedupe_key
{
public:
  dedupe_key (const saved_diagnostic &sd)
  : m_sd (sd), m_stmt (sd.m_stmt)
  {
    gcc_assert (m_stmt);
  }

  hashval_t hash () const
  {
    inchash::hash hstate;
    hstate.add_ptr (m_stmt);
    // TODO: m_sd
    return hstate.end ();
  }
  bool operator== (const dedupe_key &other) const
  {
    return (m_sd == other.m_sd
	    && m_stmt == other.m_stmt);
  }

  location_t get_location () const
  {
    return m_stmt->location;
  }

  /* A qsort comparator for use by dedupe_winners::emit_best
     to sort them into location_t order.  */

  static int
  comparator (const void *p1, const void *p2)
  {
    const dedupe_key *pk1 = *(const dedupe_key * const *)p1;
    const dedupe_key *pk2 = *(const dedupe_key * const *)p2;

    location_t loc1 = pk1->get_location ();
    location_t loc2 = pk2->get_location ();

    if (int cmp = linemap_compare_locations (line_table, loc2, loc1))
      return cmp;
    if (int cmp = ((int)pk1->m_sd.get_epath_length ()
		   - (int)pk2->m_sd.get_epath_length ()))
      return cmp;
    if (int cmp = strcmp (pk1->m_sd.m_d->get_kind (),
			  pk2->m_sd.m_d->get_kind ()))
      return cmp;
    return 0;
  }

  const saved_diagnostic &m_sd;
  const gimple *m_stmt;
};

/* Traits for use by dedupe_winners.  */

class dedupe_hash_map_traits
{
public:
  typedef const dedupe_key *key_type;
  typedef saved_diagnostic *value_type;
  typedef saved_diagnostic *compare_type;

  static inline hashval_t hash (const key_type &v)
  {
    return v->hash ();
  }
  static inline bool equal_keys (const key_type &k1, const key_type &k2)
  {
    return *k1 == *k2;
  }
  template <typename T>
  static inline void remove (T &)
  {
    // TODO
  }
  template <typename T>
  static inline void mark_deleted (T &entry)
  {
    entry.m_key = reinterpret_cast<key_type> (1);
  }
  template <typename T>
  static inline void mark_empty (T &entry)
  {
    entry.m_key = NULL;
  }
  template <typename T>
  static inline bool is_deleted (const T &entry)
  {
    return entry.m_key == reinterpret_cast<key_type> (1);
  }
  template <typename T>
  static inline bool is_empty (const T &entry)
  {
    return entry.m_key == NULL;
  }
  static const bool empty_zero_p = true;
};

/* A class for deduplicating diagnostics and finding (and emitting) the
   best saved_diagnostic within each partition.  */

class dedupe_winners
{
public:
  ~dedupe_winners ()
  {
    /* Delete all keys, but not the saved_diagnostics.  */
    for (map_t::iterator iter = m_map.begin ();
	 iter != m_map.end ();
	 ++iter)
      delete (*iter).first;
  }

  /* Determine an exploded_path for SD using PF and, if it's feasible,
     determine if SD is the best seen so far for its dedupe_key.
     Record the winning SD for each dedupe_key.  */

  void add (logger *logger,
	    epath_finder *pf,
	    saved_diagnostic *sd)
  {
    /* Determine best epath for SD.  */
    if (!sd->calc_best_epath (pf))
      return;

    dedupe_key *key = new dedupe_key (*sd);
    if (saved_diagnostic **slot = m_map.get (key))
      {
	if (logger)
	  logger->log ("already have this dedupe_key");

	saved_diagnostic *cur_best_sd = *slot;

	if (sd->get_epath_length () < cur_best_sd->get_epath_length ())
	  {
	    /* We've got a shorter path for the key; replace
	       the current candidate, marking it as a duplicate of SD.  */
	    if (logger)
	      logger->log ("length %i is better than existing length %i;"
			   " taking over this dedupe_key",
			   sd->get_epath_length (),
			   cur_best_sd->get_epath_length ());
	    sd->add_duplicate (cur_best_sd);
	    *slot = sd;
	  }
	else
	  /* We haven't beaten the current best candidate; add SD
	     as a duplicate of it.  */
	  {
	    if (logger)
	      logger->log ("length %i isn't better than existing length %i;"
			   " dropping this candidate",
			   sd->get_epath_length (),
			   cur_best_sd->get_epath_length ());
	    cur_best_sd->add_duplicate (sd);
	  }
	delete key;
      }
    else
      {
	/* This is the first candidate for this key.  */
	m_map.put (key, sd);
	if (logger)
	  logger->log ("first candidate for this dedupe_key");
      }
  }

  /* Handle interactions between the dedupe winners, so that some
     diagnostics can supercede others (of different kinds).

     We want use-after-free to supercede use-of-unitialized-value,
     so that if we have these at the same stmt, we don't emit
     a use-of-uninitialized, just the use-after-free.  */

  void handle_interactions (diagnostic_manager *dm)
  {
    LOG_SCOPE (dm->get_logger ());
    auto_vec<const dedupe_key *> superceded;
    for (auto outer : m_map)
      {
	const saved_diagnostic *outer_sd = outer.second;
	for (auto inner : m_map)
	  {
	    const saved_diagnostic *inner_sd = inner.second;
	    if (inner_sd->supercedes_p (*outer_sd))
	      {
		superceded.safe_push (outer.first);
		if (dm->get_logger ())
		  dm->log ("sd[%i] \"%s\" superceded by sd[%i] \"%s\"",
			   outer_sd->get_index (), outer_sd->m_d->get_kind (),
			   inner_sd->get_index (), inner_sd->m_d->get_kind ());
		break;
	      }
	  }
      }
    for (auto iter : superceded)
      m_map.remove (iter);
  }

 /* Emit the simplest diagnostic within each set.  */

  void emit_best (diagnostic_manager *dm,
		  const exploded_graph &eg)
  {
    LOG_SCOPE (dm->get_logger ());

    /* Get keys into a vec for sorting.  */
    auto_vec<const dedupe_key *> keys (m_map.elements ());
    for (map_t::iterator iter = m_map.begin ();
	 iter != m_map.end ();
	 ++iter)
      keys.quick_push ((*iter).first);

    dm->log ("# keys after de-duplication: %i", keys.length ());

    /* Sort into a good emission order.  */
    keys.qsort (dedupe_key::comparator);

    /* Emit the best saved_diagnostics for each key.  */
    int i;
    const dedupe_key *key;
    FOR_EACH_VEC_ELT (keys, i, key)
      {
	saved_diagnostic **slot = m_map.get (key);
	gcc_assert (*slot);
	const saved_diagnostic *sd = *slot;
	dm->emit_saved_diagnostic (eg, *sd);
      }
  }

private:
  /* This maps from each dedupe_key to a current best saved_diagnostic.  */

  typedef hash_map<const dedupe_key *, saved_diagnostic *,
		   dedupe_hash_map_traits> map_t;
  map_t m_map;
};

/* Emit all saved diagnostics.  */

void
diagnostic_manager::emit_saved_diagnostics (const exploded_graph &eg)
{
  LOG_SCOPE (get_logger ());
  auto_timevar tv (TV_ANALYZER_DIAGNOSTICS);
  log ("# saved diagnostics: %i", m_saved_diagnostics.length ());
  if (get_logger ())
    {
      unsigned i;
      saved_diagnostic *sd;
      FOR_EACH_VEC_ELT (m_saved_diagnostics, i, sd)
	log ("[%i] sd: %qs at EN: %i, SN: %i",
	     i, sd->m_d->get_kind (), sd->m_enode->m_index,
	     sd->m_snode->m_index);
    }

  if (m_saved_diagnostics.length () == 0)
    return;

  /* Compute the shortest_paths once, sharing it between all diagnostics.  */
  epath_finder pf (eg);

  /* Iterate through all saved diagnostics, adding them to a dedupe_winners
     instance.  This partitions the saved diagnostics by dedupe_key,
     generating exploded_paths for them, and retaining the best one in each
     partition.  */
  dedupe_winners best_candidates;

  int i;
  saved_diagnostic *sd;
  FOR_EACH_VEC_ELT (m_saved_diagnostics, i, sd)
    best_candidates.add (get_logger (), &pf, sd);

  best_candidates.handle_interactions (this);

  /* For each dedupe-key, call emit_saved_diagnostic on the "best"
     saved_diagnostic.  */
  best_candidates.emit_best (this, eg);
}

/* Given a saved_diagnostic SD with m_best_epath through EG,
   create an checker_path of suitable events and use it to call
   SD's underlying pending_diagnostic "emit" vfunc to emit a diagnostic.  */

void
diagnostic_manager::emit_saved_diagnostic (const exploded_graph &eg,
					   const saved_diagnostic &sd)
{
  LOG_SCOPE (get_logger ());
  log ("sd: %qs at SN: %i", sd.m_d->get_kind (), sd.m_snode->m_index);
  log ("num dupes: %i", sd.get_num_dupes ());

  pretty_printer *pp = global_dc->printer->clone ();

  const exploded_path *epath = sd.get_best_epath ();
  gcc_assert (epath);

  /* Precompute all enodes from which the diagnostic is reachable.  */
  path_builder pb (eg, *epath, sd.get_feasibility_problem (), sd);

  /* This is the diagnostic_path subclass that will be built for
     the diagnostic.  */
  checker_path emission_path;

  /* Populate emission_path with a full description of EPATH.  */
  build_emission_path (pb, *epath, &emission_path);

  /* Now prune it to just cover the most pertinent events.  */
  prune_path (&emission_path, sd.m_sm, sd.m_sval, sd.m_state);

  /* Add a final event to the path, covering the diagnostic itself.
     We use the final enode from the epath, which might be different from
     the sd.m_enode, as the dedupe code doesn't care about enodes, just
     snodes.  */
  emission_path.add_final_event (sd.m_sm, epath->get_final_enode (), sd.m_stmt,
				 sd.m_var, sd.m_state);

  /* The "final" event might not be final; if the saved_diagnostic has a
     trailing eedge stashed, add any events for it.  This is for use
     in handling longjmp, to show where a longjmp is rewinding to.  */
  if (sd.m_trailing_eedge)
    add_events_for_eedge (pb, *sd.m_trailing_eedge, &emission_path);

  emission_path.prepare_for_emission (sd.m_d);

  location_t loc = get_stmt_location (sd.m_stmt, sd.m_snode->m_fun);

  /* Allow the pending_diagnostic to fix up the primary location
     and any locations for events.  */
  loc = sd.m_d->fixup_location (loc);
  emission_path.fixup_locations (sd.m_d);

  gcc_rich_location rich_loc (loc);
  rich_loc.set_path (&emission_path);

  auto_diagnostic_group d;
  auto_cfun sentinel (sd.m_snode->m_fun);
  if (sd.m_d->emit (&rich_loc))
    {
      unsigned num_dupes = sd.get_num_dupes ();
      if (flag_analyzer_show_duplicate_count && num_dupes > 0)
	inform_n (loc, num_dupes,
		  "%i duplicate", "%i duplicates",
		  num_dupes);
      if (flag_dump_analyzer_exploded_paths)
	{
	  auto_timevar tv (TV_ANALYZER_DUMP);
	  pretty_printer pp;
	  pp_printf (&pp, "%s.%i.%s.epath.txt",
		     dump_base_name, sd.get_index (), sd.m_d->get_kind ());
	  char *filename = xstrdup (pp_formatted_text (&pp));
	  epath->dump_to_file (filename, eg.get_ext_state ());
	  inform (loc, "exploded path written to %qs", filename);
	  free (filename);
	}
    }
  delete pp;
}

/* Emit a "path" of events to EMISSION_PATH describing the exploded path
   EPATH within EG.  */

void
diagnostic_manager::build_emission_path (const path_builder &pb,
					 const exploded_path &epath,
					 checker_path *emission_path) const
{
  LOG_SCOPE (get_logger ());
  for (unsigned i = 0; i < epath.m_edges.length (); i++)
    {
      const exploded_edge *eedge = epath.m_edges[i];
      add_events_for_eedge (pb, *eedge, emission_path);
    }
}

/* Subclass of state_change_visitor that creates state_change_event
   instances.  */

class state_change_event_creator : public state_change_visitor
{
public:
  state_change_event_creator (const path_builder &pb,
			      const exploded_edge &eedge,
			      checker_path *emission_path)
    : m_pb (pb),
      m_eedge (eedge),
      m_emission_path (emission_path)
  {}

  bool on_global_state_change (const state_machine &sm,
			       state_machine::state_t src_sm_val,
			       state_machine::state_t dst_sm_val)
    FINAL OVERRIDE
  {
    if (&sm != m_pb.get_sm ())
      return false;
    const exploded_node *src_node = m_eedge.m_src;
    const program_point &src_point = src_node->get_point ();
    const int src_stack_depth = src_point.get_stack_depth ();
    const exploded_node *dst_node = m_eedge.m_dest;
    const gimple *stmt = src_point.get_stmt ();
    const supernode *supernode = src_point.get_supernode ();
    const program_state &dst_state = dst_node->get_state ();

    int stack_depth = src_stack_depth;

    m_emission_path->add_event (new state_change_event (supernode,
							stmt,
							stack_depth,
							sm,
							NULL,
							src_sm_val,
							dst_sm_val,
							NULL,
							dst_state));
    return false;
  }

  bool on_state_change (const state_machine &sm,
			state_machine::state_t src_sm_val,
			state_machine::state_t dst_sm_val,
			const svalue *sval,
			const svalue *dst_origin_sval) FINAL OVERRIDE
  {
    if (&sm != m_pb.get_sm ())
      return false;
    const exploded_node *src_node = m_eedge.m_src;
    const program_point &src_point = src_node->get_point ();
    const int src_stack_depth = src_point.get_stack_depth ();
    const exploded_node *dst_node = m_eedge.m_dest;
    const gimple *stmt = src_point.get_stmt ();
    const supernode *supernode = src_point.get_supernode ();
    const program_state &dst_state = dst_node->get_state ();

    int stack_depth = src_stack_depth;

    if (m_eedge.m_sedge
	&& m_eedge.m_sedge->m_kind == SUPEREDGE_CFG_EDGE)
      {
	supernode = src_point.get_supernode ();
	stmt = supernode->get_last_stmt ();
	stack_depth = src_stack_depth;
      }

    /* Bulletproofing for state changes at calls/returns;
       TODO: is there a better way? */
    if (!stmt)
      return false;

    m_emission_path->add_event (new state_change_event (supernode,
							stmt,
							stack_depth,
							sm,
							sval,
							src_sm_val,
							dst_sm_val,
							dst_origin_sval,
							dst_state));
    return false;
  }

  const path_builder &m_pb;
  const exploded_edge &m_eedge;
  checker_path *m_emission_path;
};

/* Compare SRC_STATE and DST_STATE (which use EXT_STATE), and call
   VISITOR's on_state_change for every sm-state change that occurs
   to a tree, and on_global_state_change for every global state change
   that occurs.

   This determines the state changes that ought to be reported to
   the user: a combination of the effects of changes to sm_state_map
   (which maps svalues to sm-states), and of region_model changes
   (which map trees to svalues).

   Bail out early and return true if any call to on_global_state_change
   or on_state_change returns true, otherwise return false.

   This is split out to make it easier to experiment with changes to
   exploded_node granularity (so that we can observe what state changes
   lead to state_change_events being emitted).  */

bool
for_each_state_change (const program_state &src_state,
		       const program_state &dst_state,
		       const extrinsic_state &ext_state,
		       state_change_visitor *visitor)
{
  gcc_assert (src_state.m_checker_states.length ()
	      == ext_state.get_num_checkers ());
  gcc_assert (dst_state.m_checker_states.length ()
	      == ext_state.get_num_checkers ());
  for (unsigned i = 0; i < ext_state.get_num_checkers (); i++)
    {
      const state_machine &sm = ext_state.get_sm (i);
      const sm_state_map &src_smap = *src_state.m_checker_states[i];
      const sm_state_map &dst_smap = *dst_state.m_checker_states[i];

      /* Add events for any global state changes.  */
      if (src_smap.get_global_state () != dst_smap.get_global_state ())
	if (visitor->on_global_state_change (sm,
					     src_smap.get_global_state (),
					     dst_smap.get_global_state ()))
	  return true;

      /* Add events for per-svalue state changes.  */
      for (sm_state_map::iterator_t iter = dst_smap.begin ();
	   iter != dst_smap.end ();
	   ++iter)
	{
	  const svalue *sval = (*iter).first;
	  state_machine::state_t dst_sm_val = (*iter).second.m_state;
	  state_machine::state_t src_sm_val
	    = src_smap.get_state (sval, ext_state);
	  if (dst_sm_val != src_sm_val)
	    {
	      const svalue *origin_sval = (*iter).second.m_origin;
	      if (visitor->on_state_change (sm, src_sm_val, dst_sm_val,
					    sval, origin_sval))
		return true;
	    }
	}
    }
  return false;
}

/* An sm_context for adding state_change_event on assignments to NULL,
   where the default state isn't m_start.  Storing such state in the
   sm_state_map would lead to bloat of the exploded_graph, so we want
   to leave it as a default state, and inject state change events here
   when we have a diagnostic.
   Find transitions of constants, for handling on_zero_assignment.  */

struct null_assignment_sm_context : public sm_context
{
  null_assignment_sm_context (int sm_idx,
			      const state_machine &sm,
			      const program_state *old_state,
			      const program_state *new_state,
			      const gimple *stmt,
			      const program_point *point,
			      checker_path *emission_path,
			      const extrinsic_state &ext_state)
  : sm_context (sm_idx, sm), m_old_state (old_state), m_new_state (new_state),
    m_stmt (stmt), m_point (point), m_emission_path (emission_path),
    m_ext_state (ext_state)
  {
  }

  tree get_fndecl_for_call (const gcall */*call*/) FINAL OVERRIDE
  {
    return NULL_TREE;
  }

  state_machine::state_t get_state (const gimple *stmt ATTRIBUTE_UNUSED,
				    tree var) FINAL OVERRIDE
  {
    const svalue *var_old_sval
      = m_old_state->m_region_model->get_rvalue (var, NULL);
    const sm_state_map *old_smap = m_old_state->m_checker_states[m_sm_idx];

    state_machine::state_t current
      = old_smap->get_state (var_old_sval, m_ext_state);

    return current;
  }

  state_machine::state_t get_state (const gimple *stmt ATTRIBUTE_UNUSED,
				    const svalue *sval) FINAL OVERRIDE
  {
    const sm_state_map *old_smap = m_old_state->m_checker_states[m_sm_idx];
    state_machine::state_t current = old_smap->get_state (sval, m_ext_state);
    return current;
  }

  void set_next_state (const gimple *stmt,
		       tree var,
		       state_machine::state_t to,
		       tree origin ATTRIBUTE_UNUSED) FINAL OVERRIDE
  {
    state_machine::state_t from = get_state (stmt, var);
    if (from != m_sm.get_start_state ())
      return;

    const svalue *var_new_sval
      = m_new_state->m_region_model->get_rvalue (var, NULL);
    const supernode *supernode = m_point->get_supernode ();
    int stack_depth = m_point->get_stack_depth ();

    m_emission_path->add_event (new state_change_event (supernode,
							m_stmt,
							stack_depth,
							m_sm,
							var_new_sval,
							from, to,
							NULL,
							*m_new_state));
  }

  void set_next_state (const gimple *stmt,
		       const svalue *sval,
		       state_machine::state_t to,
		       tree origin ATTRIBUTE_UNUSED) FINAL OVERRIDE
  {
    state_machine::state_t from = get_state (stmt, sval);
    if (from != m_sm.get_start_state ())
      return;

    const supernode *supernode = m_point->get_supernode ();
    int stack_depth = m_point->get_stack_depth ();

    m_emission_path->add_event (new state_change_event (supernode,
							m_stmt,
							stack_depth,
							m_sm,
							sval,
							from, to,
							NULL,
							*m_new_state));
  }

  void warn (const supernode *, const gimple *,
	     tree, pending_diagnostic *d) FINAL OVERRIDE
  {
    delete d;
  }

  tree get_diagnostic_tree (tree expr) FINAL OVERRIDE
  {
    return expr;
  }

  tree get_diagnostic_tree (const svalue *sval) FINAL OVERRIDE
  {
    return m_new_state->m_region_model->get_representative_tree (sval);
  }

  state_machine::state_t get_global_state () const FINAL OVERRIDE
  {
    return 0;
  }

  void set_global_state (state_machine::state_t) FINAL OVERRIDE
  {
    /* No-op.  */
  }

  void on_custom_transition (custom_transition *) FINAL OVERRIDE
  {
  }

  tree is_zero_assignment (const gimple *stmt) FINAL OVERRIDE
  {
    const gassign *assign_stmt = dyn_cast <const gassign *> (stmt);
    if (!assign_stmt)
     return NULL_TREE;
    if (const svalue *sval
	= m_new_state->m_region_model->get_gassign_result (assign_stmt, NULL))
      if (tree cst = sval->maybe_get_constant ())
	if (::zerop(cst))
	  return gimple_assign_lhs (assign_stmt);
    return NULL_TREE;
  }

  const program_state *m_old_state;
  const program_state *m_new_state;
  const gimple *m_stmt;
  const program_point *m_point;
  checker_path *m_emission_path;
  const extrinsic_state &m_ext_state;
};

/* Subroutine of diagnostic_manager::build_emission_path.
   Add any events for EEDGE to EMISSION_PATH.  */

void
diagnostic_manager::add_events_for_eedge (const path_builder &pb,
					  const exploded_edge &eedge,
					  checker_path *emission_path) const
{
  const exploded_node *src_node = eedge.m_src;
  const program_point &src_point = src_node->get_point ();
  const exploded_node *dst_node = eedge.m_dest;
  const program_point &dst_point = dst_node->get_point ();
  const int dst_stack_depth = dst_point.get_stack_depth ();
  if (get_logger ())
    {
      get_logger ()->start_log_line ();
      pretty_printer *pp = get_logger ()->get_printer ();
      pp_printf (pp, "EN %i -> EN %i: ",
		 eedge.m_src->m_index,
		 eedge.m_dest->m_index);
      src_point.print (pp, format (false));
      pp_string (pp, "-> ");
      dst_point.print (pp, format (false));
      get_logger ()->end_log_line ();
    }
  const program_state &src_state = src_node->get_state ();
  const program_state &dst_state = dst_node->get_state ();

  /* Add state change events for the states that have changed.
     We add these before events for superedges, so that if we have a
     state_change_event due to following an edge, we'll get this sequence
     of events:

      | if (!ptr)
      |    ~
      |    |
      |    (1) assuming 'ptr' is non-NULL  (state_change_event)
      |    (2) following 'false' branch... (start_cfg_edge_event)
     ...
      | do_something (ptr);
      | ~~~~~~~~~~~~~^~~~~
      |              |
      |              (3) ...to here        (end_cfg_edge_event).  */
  state_change_event_creator visitor (pb, eedge, emission_path);
  for_each_state_change (src_state, dst_state, pb.get_ext_state (),
			 &visitor);

  /* Allow non-standard edges to add events, e.g. when rewinding from
     longjmp to a setjmp.  */
  if (eedge.m_custom_info)
    eedge.m_custom_info->add_events_to_path (emission_path, eedge);

  /* Add events for superedges, function entries, and for statements.  */
  switch (dst_point.get_kind ())
    {
    default:
      break;
    case PK_BEFORE_SUPERNODE:
      if (src_point.get_kind () == PK_AFTER_SUPERNODE)
	{
	  if (eedge.m_sedge)
	    add_events_for_superedge (pb, eedge, emission_path);
	}
      /* Add function entry events.  */
      if (dst_point.get_supernode ()->entry_p ())
	{
	  emission_path->add_event
	    (new function_entry_event
	     (dst_point.get_supernode ()->get_start_location (),
	      dst_point.get_fndecl (),
	      dst_stack_depth));
	}
      break;
    case PK_BEFORE_STMT:
      {
	const gimple *stmt = dst_point.get_stmt ();
	const gcall *call = dyn_cast <const gcall *> (stmt);
	if (call && is_setjmp_call_p (call))
	  emission_path->add_event
	    (new setjmp_event (stmt->location,
			       dst_node,
			       dst_point.get_fndecl (),
			       dst_stack_depth,
			       call));
	else
	  emission_path->add_event
	    (new statement_event (stmt,
				  dst_point.get_fndecl (),
				  dst_stack_depth, dst_state));

	/* Create state change events for assignment to NULL.
	   Iterate through the stmts in dst_enode, adding state change
	   events for them.  */
	if (dst_state.m_region_model)
	  {
	    program_state iter_state (dst_state);
	    program_point iter_point (dst_point);
	    while (1)
	      {
		const gimple *stmt = iter_point.get_stmt ();
		if (const gassign *assign = dyn_cast<const gassign *> (stmt))
		  {
		    const extrinsic_state &ext_state = pb.get_ext_state ();
		    program_state old_state (iter_state);
		    iter_state.m_region_model->on_assignment (assign, NULL);
		    for (unsigned i = 0; i < ext_state.get_num_checkers (); i++)
		      {
			const state_machine &sm = ext_state.get_sm (i);
			null_assignment_sm_context sm_ctxt (i, sm,
							    &old_state,
							    &iter_state,
							    stmt,
							    &iter_point,
							    emission_path,
							    pb.get_ext_state ());
			sm.on_stmt (&sm_ctxt, dst_point.get_supernode (), stmt);
			// TODO: what about phi nodes?
		      }
		  }
		iter_point.next_stmt ();
		if (iter_point.get_kind () == PK_AFTER_SUPERNODE
		    || (dst_node->m_succs.length () > 1
			&& (iter_point
			    == dst_node->m_succs[0]->m_dest->get_point ())))
		  break;
	      }
	  }
      }
      break;
    }

  if (pb.get_feasibility_problem ()
      && &pb.get_feasibility_problem ()->m_eedge == &eedge)
    {
      pretty_printer pp;
      pp_format_decoder (&pp) = default_tree_printer;
      pp_string (&pp,
		 "this path would have been rejected as infeasible"
		 " at this edge: ");
      pb.get_feasibility_problem ()->dump_to_pp (&pp);
      emission_path->add_event (new precanned_custom_event
				(dst_point.get_location (),
				 dst_point.get_fndecl (),
				 dst_stack_depth,
				 pp_formatted_text (&pp)));
    }
}

/* Return true if EEDGE is a significant edge in the path to the diagnostic
   for PB.

   Consider all of the sibling out-eedges from the same source enode
   as EEDGE.
   If there's no path from the destinations of those eedges to the
   diagnostic enode, then we have to take this eedge and thus it's
   significant.

   Conversely if there is a path from the destination of any other sibling
   eedge to the diagnostic enode, then this edge is insignificant.

   Example 1: redundant if-else:

     (A) if (...)            A
     (B)   ...              / \
         else              B   C
     (C)   ...              \ /
     (D) [DIAGNOSTIC]        D

     D is reachable by either B or C, so neither of these edges
     are significant.

   Example 2: pertinent if-else:

     (A) if (...)                         A
     (B)   ...                           / \
         else                           B   C
     (C)   [NECESSARY CONDITION]        |   |
     (D) [POSSIBLE DIAGNOSTIC]          D1  D2

     D becomes D1 and D2 in the exploded graph, where the diagnostic occurs
     at D2.  D2 is only reachable via C, so the A -> C edge is significant.

   Example 3: redundant loop:

     (A) while (...)          +-->A
     (B)   ...                |  / \
     (C) ...                  +-B  C
     (D) [DIAGNOSTIC]              |
                                   D

     D is reachable from both B and C, so the A->C edge is not significant.  */

bool
diagnostic_manager::significant_edge_p (const path_builder &pb,
					const exploded_edge &eedge) const
{
  int i;
  exploded_edge *sibling;
  FOR_EACH_VEC_ELT (eedge.m_src->m_succs, i, sibling)
    {
      if (sibling == &eedge)
	continue;
      if (pb.reachable_from_p (sibling->m_dest))
	{
	  if (get_logger ())
	    get_logger ()->log ("  edge EN: %i -> EN: %i is insignificant as"
				" EN: %i is also reachable via"
				" EN: %i -> EN: %i",
				eedge.m_src->m_index, eedge.m_dest->m_index,
				pb.get_diag_node ()->m_index,
				sibling->m_src->m_index,
				sibling->m_dest->m_index);
	  return false;
	}
    }

  return true;
}

/* Subroutine of diagnostic_manager::add_events_for_eedge
   where EEDGE has an underlying superedge i.e. a CFG edge,
   or an interprocedural call/return.
   Add any events for the superedge to EMISSION_PATH.  */

void
diagnostic_manager::add_events_for_superedge (const path_builder &pb,
					      const exploded_edge &eedge,
					      checker_path *emission_path)
  const
{
  gcc_assert (eedge.m_sedge);

  /* Give diagnostics an opportunity to override this function.  */
  pending_diagnostic *pd = pb.get_pending_diagnostic ();
  if (pd->maybe_add_custom_events_for_superedge (eedge, emission_path))
    return;

  /* Don't add events for insignificant edges at verbosity levels below 3.  */
  if (m_verbosity < 3)
    if (!significant_edge_p (pb, eedge))
      return;

  const exploded_node *src_node = eedge.m_src;
  const program_point &src_point = src_node->get_point ();
  const exploded_node *dst_node = eedge.m_dest;
  const program_point &dst_point = dst_node->get_point ();
  const int src_stack_depth = src_point.get_stack_depth ();
  const int dst_stack_depth = dst_point.get_stack_depth ();
  const gimple *last_stmt = src_point.get_supernode ()->get_last_stmt ();

  switch (eedge.m_sedge->m_kind)
    {
    case SUPEREDGE_CFG_EDGE:
      {
	emission_path->add_event
	  (new start_cfg_edge_event (eedge,
			       (last_stmt
				? last_stmt->location
				: UNKNOWN_LOCATION),
			       src_point.get_fndecl (),
			       src_stack_depth));
	emission_path->add_event
	  (new end_cfg_edge_event (eedge,
				   dst_point.get_supernode ()->get_start_location (),
				   dst_point.get_fndecl (),
				   dst_stack_depth));
      }
      break;

    case SUPEREDGE_CALL:
      {
	emission_path->add_event
	  (new call_event (eedge,
			   (last_stmt
			    ? last_stmt->location
			    : UNKNOWN_LOCATION),
			   src_point.get_fndecl (),
			   src_stack_depth));
      }
      break;

    case SUPEREDGE_INTRAPROCEDURAL_CALL:
      {
	/* TODO: add a subclass for this, or generate events for the
	   summary.  */
	emission_path->add_event
	  (new debug_event ((last_stmt
			     ? last_stmt->location
			     : UNKNOWN_LOCATION),
			    src_point.get_fndecl (),
			    src_stack_depth,
			    "call summary"));
      }
      break;

    case SUPEREDGE_RETURN:
      {
	const return_superedge *return_edge
	  = as_a <const return_superedge *> (eedge.m_sedge);

	const gcall *call_stmt = return_edge->get_call_stmt ();
	emission_path->add_event
	  (new return_event (eedge,
			     (call_stmt
			      ? call_stmt->location
			      : UNKNOWN_LOCATION),
			     dst_point.get_fndecl (),
			     dst_stack_depth));
      }
      break;
    }
}

/* Prune PATH, based on the verbosity level, to the most pertinent
   events for a diagnostic that involves VAR ending in state STATE
   (for state machine SM).

   PATH is updated in place, and the redundant checker_events are deleted.

   As well as deleting events, call record_critical_state on events in
   which state critical to the pending_diagnostic is being handled; see
   the comment for diagnostic_manager::prune_for_sm_diagnostic.  */

void
diagnostic_manager::prune_path (checker_path *path,
				const state_machine *sm,
				const svalue *sval,
				state_machine::state_t state) const
{
  LOG_FUNC (get_logger ());
  path->maybe_log (get_logger (), "path");
  prune_for_sm_diagnostic (path, sm, sval, state);
  prune_interproc_events (path);
  consolidate_conditions (path);
  finish_pruning (path);
  path->maybe_log (get_logger (), "pruned");
}

/* A cheap test to determine if EXPR can be the expression of interest in
   an sm-diagnostic, so that we can reject cases where we have a non-lvalue.
   We don't have always have a model when calling this, so we can't use
   tentative_region_model_context, so there can be false positives.  */

static bool
can_be_expr_of_interest_p (tree expr)
{
  if (!expr)
    return false;

  /* Reject constants.  */
  if (CONSTANT_CLASS_P (expr))
    return false;

  /* Otherwise assume that it can be an lvalue.  */
  return true;
}

/* First pass of diagnostic_manager::prune_path: apply verbosity level,
   pruning unrelated state change events.

   Iterate backwards through PATH, skipping state change events that aren't
   VAR but update the pertinent VAR when state-copying occurs.

   As well as deleting events, call record_critical_state on events in
   which state critical to the pending_diagnostic is being handled, so
   that the event's get_desc vfunc can potentially supply a more precise
   description of the event to the user.
   e.g. improving
     "calling 'foo' from 'bar'"
   to
     "passing possibly-NULL pointer 'ptr' to 'foo' from 'bar' as param 1"
   when the diagnostic relates to later dereferencing 'ptr'.  */

void
diagnostic_manager::prune_for_sm_diagnostic (checker_path *path,
					     const state_machine *sm,
					     const svalue *sval,
					     state_machine::state_t state) const
{
  int idx = path->num_events () - 1;
  while (idx >= 0 && idx < (signed)path->num_events ())
    {
      checker_event *base_event = path->get_checker_event (idx);
      if (get_logger ())
	{
	  if (sm)
	    {
	      if (sval)
		{
		  label_text sval_desc = sval->get_desc ();
		  log ("considering event %i (%s), with sval: %qs, state: %qs",
		       idx, event_kind_to_string (base_event->m_kind),
		       sval_desc.m_buffer, state->get_name ());
		}
	      else
		log ("considering event %i (%s), with global state: %qs",
		     idx, event_kind_to_string (base_event->m_kind),
		     state->get_name ());
	    }
	  else
	    log ("considering event %i", idx);
	}

      switch (base_event->m_kind)
	{
	default:
	  gcc_unreachable ();

	case EK_DEBUG:
	  if (m_verbosity < 4)
	    {
	      log ("filtering event %i: debug event", idx);
	      path->delete_event (idx);
	    }
	  break;

	case EK_CUSTOM:
	  /* Don't filter custom events.  */
	  break;

	case EK_STMT:
	  {
	    if (m_verbosity < 4)
	      {
		log ("filtering event %i: statement event", idx);
		path->delete_event (idx);
	      }
	  }
	  break;

	case EK_FUNCTION_ENTRY:
	  if (m_verbosity < 1)
	    {
	      log ("filtering event %i: function entry", idx);
	      path->delete_event (idx);
	    }
	  break;

	case EK_STATE_CHANGE:
	  {
	    state_change_event *state_change = (state_change_event *)base_event;
	    gcc_assert (state_change->m_dst_state.m_region_model);

	    if (state_change->m_sval == sval)
	      {
		if (state_change->m_origin)
		  {
		    if (get_logger ())
		      {
			label_text sval_desc = sval->get_desc ();
			label_text origin_sval_desc
			  = state_change->m_origin->get_desc ();
			log ("event %i:"
			     " switching var of interest from %qs to %qs",
			     idx, sval_desc.m_buffer,
			     origin_sval_desc.m_buffer);
		      }
		    sval = state_change->m_origin;
		  }
		log ("event %i: switching state of interest from %qs to %qs",
		     idx, state_change->m_to->get_name (),
		     state_change->m_from->get_name ());
		state = state_change->m_from;
	      }
	    else if (m_verbosity < 4)
	      {
		if (get_logger ())
		  {
		    if (state_change->m_sval)
		      {
			label_text change_sval_desc
			  = state_change->m_sval->get_desc ();
			if (sval)
			  {
			    label_text sval_desc = sval->get_desc ();
			    log ("filtering event %i:"
				 " state change to %qs unrelated to %qs",
				 idx, change_sval_desc.m_buffer,
				 sval_desc.m_buffer);
			  }
			else
			  log ("filtering event %i: state change to %qs",
			       idx, change_sval_desc.m_buffer);
		      }
		    else
		      log ("filtering event %i: global state change", idx);
		  }
		path->delete_event (idx);
	      }
	  }
	  break;

	case EK_START_CFG_EDGE:
	  {
	    cfg_edge_event *event = (cfg_edge_event *)base_event;

	    /* TODO: is this edge significant to var?
	       See if var can be in other states in the dest, but not
	       in other states in the src?
	       Must have multiple sibling edges.  */

	    if (event->should_filter_p (m_verbosity))
	      {
		log ("filtering events %i and %i: CFG edge", idx, idx + 1);
		path->delete_event (idx);
		/* Also delete the corresponding EK_END_CFG_EDGE.  */
		gcc_assert (path->get_checker_event (idx)->m_kind
			    == EK_END_CFG_EDGE);
		path->delete_event (idx);
	      }
	  }
	  break;

	case EK_END_CFG_EDGE:
	  /* These come in pairs with EK_START_CFG_EDGE events and are
	     filtered when their start event is filtered.  */
	  break;

	case EK_CALL_EDGE:
	  {
	    call_event *event = (call_event *)base_event;
	    const region_model *callee_model
	      = event->m_eedge.m_dest->get_state ().m_region_model;
	    const region_model *caller_model
	      = event->m_eedge.m_src->get_state ().m_region_model;
	    tree callee_var = callee_model->get_representative_tree (sval);
	    callsite_expr expr;

	    tree caller_var;
            if(event->m_sedge)
              {
                const callgraph_superedge& cg_superedge
                  = event->get_callgraph_superedge ();
                if (cg_superedge.m_cedge)
	          caller_var
	            = cg_superedge.map_expr_from_callee_to_caller (callee_var,
                                                                   &expr);
                else
                  caller_var = caller_model->get_representative_tree (sval);
              }
            else
	      caller_var = caller_model->get_representative_tree (sval);

	    if (caller_var)
	      {
		if (get_logger ())
		  {
		    label_text sval_desc = sval->get_desc ();
		    log ("event %i:"
			 " recording critical state for %qs at call"
			 " from %qE in callee to %qE in caller",
			 idx, sval_desc.m_buffer, callee_var, caller_var);
		  }
		if (expr.param_p ())
		  event->record_critical_state (caller_var, state);
	      }
	  }
	  break;

	case EK_RETURN_EDGE:
	  {
	    if (sval)
	      {
		return_event *event = (return_event *)base_event;
                const region_model *caller_model
                  = event->m_eedge.m_dest->get_state ().m_region_model;
                tree caller_var = caller_model->get_representative_tree (sval);
                const region_model *callee_model
                  = event->m_eedge.m_src->get_state ().m_region_model;
		callsite_expr expr;

                tree callee_var;
                if (event->m_sedge)
                  {
                    const callgraph_superedge& cg_superedge
                      = event->get_callgraph_superedge ();
                    if (cg_superedge.m_cedge)
                      callee_var
                        = cg_superedge.map_expr_from_caller_to_callee (caller_var,
                                                                       &expr);
                    else
                      callee_var = callee_model->get_representative_tree (sval);
                  }
                else
                  callee_var = callee_model->get_representative_tree (sval);

		if (callee_var)
		  {
		    if (get_logger ())
		      {
			label_text sval_desc = sval->get_desc ();
			log ("event %i:"
			     " recording critical state for %qs at return"
			     " from %qE in caller to %qE in callee",
			     idx, sval_desc.m_buffer, callee_var, callee_var);
		      }
		    if (expr.return_value_p ())
		      event->record_critical_state (callee_var, state);
		  }
	      }
	  }
	  break;

	case EK_SETJMP:
	  /* TODO: only show setjmp_events that matter i.e. those for which
	     there is a later rewind event using them.  */
	case EK_REWIND_FROM_LONGJMP:
	case EK_REWIND_TO_SETJMP:
	  break;

	case EK_WARNING:
	  /* Always show the final "warning" event in the path.  */
	  break;
	}
      idx--;
    }
}

/* Subroutine of diagnostic_manager::prune_for_sm_diagnostic.
   If *EXPR is not suitable to be the expression of interest in
   an sm-diagnostic, set *EXPR to NULL and log.  */

void
diagnostic_manager::update_for_unsuitable_sm_exprs (tree *expr) const
{
  gcc_assert (expr);
  if (*expr && !can_be_expr_of_interest_p (*expr))
    {
      log ("new var %qE is unsuitable; setting var to NULL", *expr);
      *expr = NULL_TREE;
    }
}

/* Second pass of diagnostic_manager::prune_path: remove redundant
   interprocedural information.

   For example, given:
     (1)- calling "f2" from "f1"
     (2)--- entry to "f2"
     (3)--- calling "f3" from "f2"
     (4)----- entry to "f3"
     (5)--- returning to "f2" to "f3"
     (6)- returning to "f1" to "f2"
   with no other intervening events, then none of these events are
   likely to be interesting to the user.

   Prune [..., call, function-entry, return, ...] triples repeatedly
   until nothing has changed.  For the example above, this would
   remove events (3, 4, 5), and then remove events (1, 2, 6).  */

void
diagnostic_manager::prune_interproc_events (checker_path *path) const
{
  bool changed = false;
  do
    {
      changed = false;
      int idx = (signed)path->num_events () - 1;
      while (idx >= 0)
	{
	  /* Prune [..., call, function-entry, return, ...] triples.  */
	  if (idx + 2 < (signed)path->num_events ()
	      && path->get_checker_event (idx)->is_call_p ()
	      && path->get_checker_event (idx + 1)->is_function_entry_p ()
	      && path->get_checker_event (idx + 2)->is_return_p ())
	    {
	      if (get_logger ())
		{
		  label_text desc
		    (path->get_checker_event (idx)->get_desc (false));
		  log ("filtering events %i-%i:"
		       " irrelevant call/entry/return: %s",
		       idx, idx + 2, desc.m_buffer);
		  desc.maybe_free ();
		}
	      path->delete_event (idx + 2);
	      path->delete_event (idx + 1);
	      path->delete_event (idx);
	      changed = true;
	      idx--;
	      continue;
	    }

	  /* Prune [..., call, return, ...] pairs
	     (for -fanalyzer-verbosity=0).  */
	  if (idx + 1 < (signed)path->num_events ()
	      && path->get_checker_event (idx)->is_call_p ()
	      && path->get_checker_event (idx + 1)->is_return_p ())
	    {
	      if (get_logger ())
		{
		  label_text desc
		    (path->get_checker_event (idx)->get_desc (false));
		  log ("filtering events %i-%i:"
		       " irrelevant call/return: %s",
		       idx, idx + 1, desc.m_buffer);
		  desc.maybe_free ();
		}
	      path->delete_event (idx + 1);
	      path->delete_event (idx);
	      changed = true;
	      idx--;
	      continue;
	    }

	  idx--;
	}

    }
  while (changed);
}

/* Return true iff event IDX within PATH is on the same line as REF_EXP_LOC.  */

static bool
same_line_as_p (const expanded_location &ref_exp_loc,
		checker_path *path, unsigned idx)
{
  const checker_event *ev = path->get_checker_event (idx);
  expanded_location idx_exp_loc = expand_location (ev->get_location ());
  gcc_assert (ref_exp_loc.file);
  if (idx_exp_loc.file == NULL)
    return false;
  if (strcmp (ref_exp_loc.file, idx_exp_loc.file))
    return false;
  return ref_exp_loc.line == idx_exp_loc.line;
}

/* This path-readability optimization reduces the verbosity of compound
   conditional statements (without needing to reconstruct the AST, which
   has already been lost).

   For example, it converts:

    |   61 |   if (cp[0] != '\0' && cp[0] != '#')
    |      |      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    |      |      |              |    |
    |      |      |              |    (6) ...to here
    |      |      |              (7) following ‘true’ branch...
    |      |      (5) following ‘true’ branch...
    |   62 |     {
    |   63 |       alias = cp++;
    |      |               ~~~~
    |      |                 |
    |      |                 (8) ...to here

   into:

    |   61 |   if (cp[0] != '\0' && cp[0] != '#')
    |      |      ~
    |      |      |
    |      |      (5) following ‘true’ branch...
    |   62 |     {
    |   63 |       alias = cp++;
    |      |               ~~~~
    |      |                 |
    |      |                 (6) ...to here

   by combining events 5-8 into new events 5-6.

   Find runs of consecutive (start_cfg_edge_event, end_cfg_edge_event) pairs
   in which all events apart from the final end_cfg_edge_event are on the same
   line, and for which either all the CFG edges are TRUE edges, or all are
   FALSE edges.

   Consolidate each such run into a
     (start_consolidated_cfg_edges_event, end_consolidated_cfg_edges_event)
   pair.  */

void
diagnostic_manager::consolidate_conditions (checker_path *path) const
{
  /* Don't simplify edges if we're debugging them.  */
  if (flag_analyzer_verbose_edges)
    return;

  for (int start_idx = 0;
       start_idx < (signed)path->num_events () - 1;
       start_idx++)
    {
      if (path->cfg_edge_pair_at_p (start_idx))
	{
	  const checker_event *old_start_ev
	    = path->get_checker_event (start_idx);
	  expanded_location start_exp_loc
	    = expand_location (old_start_ev->get_location ());
	  if (start_exp_loc.file == NULL)
	    continue;
	  if (!same_line_as_p (start_exp_loc, path, start_idx + 1))
	    continue;

	  /* Are we looking for a run of all TRUE edges, or all FALSE edges?  */
	  gcc_assert (old_start_ev->m_kind == EK_START_CFG_EDGE);
	  const start_cfg_edge_event *old_start_cfg_ev
	    = (const start_cfg_edge_event *)old_start_ev;
	  const cfg_superedge& first_cfg_sedge
	    = old_start_cfg_ev->get_cfg_superedge ();
	  bool edge_sense;
	  if (first_cfg_sedge.true_value_p ())
	    edge_sense = true;
	  else if (first_cfg_sedge.false_value_p ())
	    edge_sense = false;
	  else
	    continue;

	  /* Find a run of CFG start/end event pairs from
	       [start_idx, next_idx)
	     where all apart from the final event are on the same line,
	     and all are either TRUE or FALSE edges, matching the initial.  */
	  int next_idx = start_idx + 2;
	  while (path->cfg_edge_pair_at_p (next_idx)
		 && same_line_as_p (start_exp_loc, path, next_idx))
	    {
	      const checker_event *iter_ev
		= path->get_checker_event (next_idx);
	      gcc_assert (iter_ev->m_kind == EK_START_CFG_EDGE);
	      const start_cfg_edge_event *iter_cfg_ev
		= (const start_cfg_edge_event *)iter_ev;
	      const cfg_superedge& iter_cfg_sedge
		= iter_cfg_ev->get_cfg_superedge ();
	      if (edge_sense)
		{
		  if (!iter_cfg_sedge.true_value_p ())
		    break;
		}
	      else
		{
		  if (!iter_cfg_sedge.false_value_p ())
		    break;
		}
	      next_idx += 2;
	    }

	  /* If we have more than one pair in the run, consolidate.  */
	  if (next_idx > start_idx + 2)
	    {
	      const checker_event *old_end_ev
		= path->get_checker_event (next_idx - 1);
	      log ("consolidating CFG edge events %i-%i into %i-%i",
		   start_idx, next_idx - 1, start_idx, start_idx +1);
	      start_consolidated_cfg_edges_event *new_start_ev
		= new start_consolidated_cfg_edges_event
		(old_start_ev->get_location (),
		 old_start_ev->get_fndecl (),
		 old_start_ev->get_stack_depth (),
		 edge_sense);
	      checker_event *new_end_ev
		= new end_consolidated_cfg_edges_event
		(old_end_ev->get_location (),
		 old_end_ev->get_fndecl (),
		 old_end_ev->get_stack_depth ());
	      path->replace_event (start_idx, new_start_ev);
	      path->replace_event (start_idx + 1, new_end_ev);
	      path->delete_events (start_idx + 2, next_idx - (start_idx + 2));
	    }
	}
    }
}

/* Final pass of diagnostic_manager::prune_path.

   If all we're left with is in one function, then filter function entry
   events.  */

void
diagnostic_manager::finish_pruning (checker_path *path) const
{
  if (!path->interprocedural_p ())
    {
      int idx = path->num_events () - 1;
      while (idx >= 0 && idx < (signed)path->num_events ())
	{
	  checker_event *base_event = path->get_checker_event (idx);
	  if (base_event->m_kind == EK_FUNCTION_ENTRY)
	    {
	      log ("filtering event %i:"
		   " function entry for purely intraprocedural path", idx);
	      path->delete_event (idx);
	    }
	  idx--;
	}
    }
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
