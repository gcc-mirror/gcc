/* Classes for saving, deduplicating, and emitting analyzer diagnostics.
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
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/sm.h"
#include "analyzer/pending-diagnostic.h"
#include "analyzer/diagnostic-manager.h"
#include "analyzer/region-model.h"
#include "analyzer/constraint-manager.h"
#include "cfg.h"
#include "basic-block.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "cgraph.h"
#include "digraph.h"
#include "analyzer/supergraph.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/program-state.h"
#include "analyzer/exploded-graph.h"
#include "analyzer/checker-path.h"
#include "analyzer/reachability.h"

#if ENABLE_ANALYZER

namespace ana {

/* class saved_diagnostic.  */

/* saved_diagnostic's ctor.
   Take ownership of D and STMT_FINDER.  */

saved_diagnostic::saved_diagnostic (const state_machine *sm,
				    const exploded_node *enode,
				    const supernode *snode, const gimple *stmt,
				    stmt_finder *stmt_finder,
				    tree var, state_machine::state_t state,
				    pending_diagnostic *d)
: m_sm (sm), m_enode (enode), m_snode (snode), m_stmt (stmt),
 /* stmt_finder could be on-stack; we want our own copy that can
    outlive that.  */
  m_stmt_finder (stmt_finder ? stmt_finder->clone () : NULL),
  m_var (var), m_state (state),
  m_d (d), m_trailing_eedge (NULL),
  m_status (STATUS_NEW), m_epath_length (0), m_problem (NULL)
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

/* State for building a checker_path from a particular exploded_path.
   In particular, this precomputes reachability information: the set of
   source enodes for which a path be found to the diagnostic enode.  */

class path_builder
{
public:
  path_builder (const exploded_graph &eg,
		const exploded_path &epath)
  : m_eg (eg),
    m_diag_enode (epath.get_final_enode ()),
    m_reachability (eg, m_diag_enode)
  {}

  const exploded_node *get_diag_node () const { return m_diag_enode; }

  bool reachable_from_p (const exploded_node *src_enode) const
  {
    return m_reachability.reachable_from_p (src_enode);
  }

  const extrinsic_state &get_ext_state () const { return m_eg.get_ext_state (); }

private:
  typedef reachability<eg_traits> enode_reachability;

  const exploded_graph &m_eg;

  /* The enode where the diagnostic occurs.  */
  const exploded_node *m_diag_enode;

  /* Precompute all enodes from which the diagnostic is reachable.  */
  enode_reachability m_reachability;
};

/* class diagnostic_manager.  */

/* diagnostic_manager's ctor.  */

diagnostic_manager::diagnostic_manager (logger *logger, int verbosity)
: log_user (logger), m_verbosity (verbosity)
{
}

/* Queue pending_diagnostic D at ENODE for later emission.  */

void
diagnostic_manager::add_diagnostic (const state_machine *sm,
				    const exploded_node *enode,
				    const supernode *snode, const gimple *stmt,
				    stmt_finder *finder,
				    tree var, state_machine::state_t state,
				    pending_diagnostic *d)
{
  LOG_FUNC (get_logger ());

  /* We must have an enode in order to be able to look for paths
     through the exploded_graph to the diagnostic.  */
  gcc_assert (enode);

  saved_diagnostic *sd
    = new saved_diagnostic (sm, enode, snode, stmt, finder, var, state, d);
  m_saved_diagnostics.safe_push (sd);
  if (get_logger ())
    log ("adding saved diagnostic %i at SN %i: %qs",
	 m_saved_diagnostics.length () - 1,
	 snode->m_index, d->get_kind ());
}

/* Queue pending_diagnostic D at ENODE for later emission.  */

void
diagnostic_manager::add_diagnostic (const exploded_node *enode,
				    const supernode *snode, const gimple *stmt,
				    stmt_finder *finder,
				    pending_diagnostic *d)
{
  gcc_assert (enode);
  add_diagnostic (NULL, enode, snode, stmt, finder, NULL_TREE, 0, d);
}

/* A class for identifying sets of duplicated pending_diagnostic.

   We want to find the simplest dedupe_candidate amongst those that share a
   dedupe_key.  */

class dedupe_key
{
public:
  dedupe_key (const saved_diagnostic &sd,
	      const exploded_path &epath)
  : m_sd (sd), m_stmt (sd.m_stmt)
  {
    /* Support deferring the choice of stmt until after an emission path has
     been built, using an optional stmt_finder.  */
    if (m_stmt == NULL)
      {
	gcc_assert (sd.m_stmt_finder);
	m_stmt = sd.m_stmt_finder->find_stmt (epath);
      }
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

    return linemap_compare_locations (line_table, loc2, loc1);
  }

  const saved_diagnostic &m_sd;
  const gimple *m_stmt;
};

/* The value of a slot for a dedupe_key within dedupe_winners:
   the exploded_path for the best candidate for that key, and the
   number of duplicates seen so far.  */

class dedupe_candidate
{
public:
  // has the exploded_path
  dedupe_candidate (const shortest_exploded_paths &sp,
		    saved_diagnostic *sd)
  : m_epath (sp.get_shortest_path (sd->m_enode)),
    m_num_dupes (0)
  {
  }

  unsigned length () const { return m_epath.length (); }
  const exploded_path &get_path () const { return m_epath; }

  void add_duplicate () { m_num_dupes++; }
  int get_num_dupes () const { return m_num_dupes; }

private:
  exploded_path m_epath;
public:
  int m_num_dupes;
};

/* Traits for use by dedupe_winners.  */

class dedupe_hash_map_traits
{
public:
  typedef const dedupe_key *key_type;
  typedef dedupe_candidate *value_type;
  typedef dedupe_candidate *compare_type;

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
   best diagnostic within each partition.  */

class dedupe_winners
{
public:
  ~dedupe_winners ()
  {
    /* Delete all keys and candidates.  */
    for (map_t::iterator iter = m_map.begin ();
	 iter != m_map.end ();
	 ++iter)
      {
	delete (*iter).first;
	delete (*iter).second;
      }
  }

  /* Determine an exploded_path for SD using SP and, if it's feasible,
     determine if it's the best seen so far for its dedupe_key.
     Retain the winner for each dedupe_key, and discard the rest.  */

  void add (logger *logger,
	    const shortest_exploded_paths &sp,
	    saved_diagnostic *sd)
  {
    /* Build a dedupe_candidate for SD.
       This uses SP to build an exploded_path.  */
    dedupe_candidate *dc = new dedupe_candidate (sp, sd);

    sd->set_epath_length (dc->length ());

    /* Verify that the epath is feasible.
       State-merging means that not every path in the epath corresponds
       to a feasible one w.r.t. states.
       Here we simply check each duplicate saved_diagnostic's
       shortest_path, and reject any that aren't feasible.
       This could introduce false negatives, as there could be longer
       feasible paths within the egraph.  */
    if (logger)
      logger->log ("considering %qs at EN: %i, SN: %i",
		   sd->m_d->get_kind (), sd->m_enode->m_index,
		   sd->m_snode->m_index);

    feasibility_problem *p = NULL;
    if (!dc->get_path ().feasible_p (logger, &p))
      {
	if (logger)
	  logger->log ("rejecting %qs at EN: %i, SN: %i"
		       " due to infeasible path",
		       sd->m_d->get_kind (), sd->m_enode->m_index,
		       sd->m_snode->m_index);
	sd->set_infeasible (p);
	delete dc;
	return;
      }
    else
      if (logger)
	logger->log ("accepting %qs at EN: %i, SN: %i with feasible path",
		     sd->m_d->get_kind (), sd->m_enode->m_index,
		     sd->m_snode->m_index);

    sd->set_feasible ();

    dedupe_key *key = new dedupe_key (*sd, dc->get_path ());
    if (dedupe_candidate **slot = m_map.get (key))
      {
	if (logger)
	  logger->log ("already have this dedupe_key");

	(*slot)->add_duplicate ();

	if (dc->length () < (*slot)->length ())
	  {
	    /* We've got a shorter path for the key; replace
	       the current candidate.  */
	    if (logger)
	      logger->log ("length %i is better than existing length %i;"
			   " taking over this dedupe_key",
			   dc->length (), (*slot)->length ());
	    dc->m_num_dupes = (*slot)->get_num_dupes ();
	    delete *slot;
	    *slot = dc;
	  }
	else
	  /* We haven't beaten the current best candidate;
	     drop the new candidate.  */
	  {
	    if (logger)
	      logger->log ("length %i isn't better than existing length %i;"
			   " dropping this candidate",
			   dc->length (), (*slot)->length ());
	    delete dc;
	  }
	delete key;
      }
    else
      {
	/* This is the first candidate for this key.  */
	m_map.put (key, dc);
	if (logger)
	  logger->log ("first candidate for this dedupe_key");
      }
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

    /* Emit the best candidate for each key.  */
    int i;
    const dedupe_key *key;
    FOR_EACH_VEC_ELT (keys, i, key)
      {
	dedupe_candidate **slot = m_map.get (key);
	gcc_assert (*slot);
	const dedupe_candidate &dc = **slot;

	dm->emit_saved_diagnostic (eg, key->m_sd,
				   dc.get_path (), key->m_stmt,
				   dc.get_num_dupes ());
      }
  }

private:

  /* This maps from each dedupe_key to a current best dedupe_candidate.  */

  typedef hash_map<const dedupe_key *, dedupe_candidate *,
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
  shortest_exploded_paths sp (eg, eg.get_origin ());

  /* Iterate through all saved diagnostics, adding them to a dedupe_winners
     instance.  This partitions the saved diagnostics by dedupe_key,
     generating exploded_paths for them, and retaining the best one in each
     partition.  */
  dedupe_winners best_candidates;

  int i;
  saved_diagnostic *sd;
  FOR_EACH_VEC_ELT (m_saved_diagnostics, i, sd)
    best_candidates.add (get_logger (), sp, sd);

  /* For each dedupe-key, call emit_saved_diagnostic on the "best"
     saved_diagnostic.  */
  best_candidates.emit_best (this, eg);
}

/* Given a saved_diagnostic SD at STMT with feasible path EPATH through EG,
   create an checker_path of suitable events and use it to call
   SD's underlying pending_diagnostic "emit" vfunc to emit a diagnostic.  */

void
diagnostic_manager::emit_saved_diagnostic (const exploded_graph &eg,
					   const saved_diagnostic &sd,
					   const exploded_path &epath,
					   const gimple *stmt,
					   int num_dupes)
{
  LOG_SCOPE (get_logger ());
  log ("sd: %qs at SN: %i", sd.m_d->get_kind (), sd.m_snode->m_index);
  log ("num dupes: %i", num_dupes);

  pretty_printer *pp = global_dc->printer->clone ();

  /* Precompute all enodes from which the diagnostic is reachable.  */
  path_builder pb (eg, epath);

  /* This is the diagnostic_path subclass that will be built for
     the diagnostic.  */
  checker_path emission_path;

  /* Populate emission_path with a full description of EPATH.  */
  build_emission_path (pb, epath, &emission_path);

  /* Now prune it to just cover the most pertinent events.  */
  prune_path (&emission_path, sd.m_sm, sd.m_var, sd.m_state);

  /* Add a final event to the path, covering the diagnostic itself.
     We use the final enode from the epath, which might be different from
     the sd.m_enode, as the dedupe code doesn't care about enodes, just
     snodes.  */
  emission_path.add_final_event (sd.m_sm, epath.get_final_enode (), stmt,
				 sd.m_var, sd.m_state);

  /* The "final" event might not be final; if the saved_diagnostic has a
     trailing eedge stashed, add any events for it.  This is for use
     in handling longjmp, to show where a longjmp is rewinding to.  */
  if (sd.m_trailing_eedge)
    add_events_for_eedge (pb, *sd.m_trailing_eedge, &emission_path);

  emission_path.prepare_for_emission (sd.m_d);

  gcc_rich_location rich_loc (stmt->location);
  rich_loc.set_path (&emission_path);

  auto_diagnostic_group d;
  auto_cfun sentinel (sd.m_snode->m_fun);
  if (sd.m_d->emit (&rich_loc))
    {
      if (flag_analyzer_show_duplicate_count && num_dupes > 0)
	inform_n (stmt->location, num_dupes,
		  "%i duplicate", "%i duplicates",
		  num_dupes);
    }
  delete pp;
}

/* Given a state change to DST_REP, determine a tree that gives the origin
   of that state at STMT, using DST_STATE's region model, so that state
   changes based on assignments can be tracked back to their origins.

   For example, if we have

     (S1) _1 = malloc (64);
     (S2) EXPR = _1;

   then at stmt S2 we can get the origin of EXPR's state as being _1,
   and thus track the allocation back to S1.  */

static tree
get_any_origin (const gimple *stmt,
		tree dst_rep,
		const program_state &dst_state)
{
  if (!stmt)
    return NULL_TREE;

  gcc_assert (dst_rep);

  if (const gassign *assign = dyn_cast <const gassign *> (stmt))
    {
      tree lhs = gimple_assign_lhs (assign);
      /* Use region IDs to compare lhs with DST_REP, bulletproofing against
	 cases where they can't have lvalues by using
	 tentative_region_model_context.  */
      tentative_region_model_context ctxt;
      region_id lhs_rid = dst_state.m_region_model->get_lvalue (lhs, &ctxt);
      region_id dst_rep_rid
	= dst_state.m_region_model->get_lvalue (dst_rep, &ctxt);
      if (lhs_rid == dst_rep_rid && !ctxt.had_errors_p ())
	{
	  tree rhs1 = gimple_assign_rhs1 (assign);
	  enum tree_code op = gimple_assign_rhs_code (assign);
	  switch (op)
	    {
	    default:
	      //gcc_unreachable ();  // TODO
	      break;
	    case COMPONENT_REF:
	    case SSA_NAME:
	      return rhs1;
	    }
	}
    }
  return NULL_TREE;
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
  state_change_event_creator (const exploded_edge &eedge,
			      checker_path *emission_path)
    : m_eedge (eedge),
      m_emission_path (emission_path)
  {}

  bool on_global_state_change (const state_machine &sm,
			       state_machine::state_t src_sm_val,
			       state_machine::state_t dst_sm_val)
    FINAL OVERRIDE
  {
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
							NULL_TREE,
							src_sm_val,
							dst_sm_val,
							NULL_TREE,
							dst_state));
    return false;
  }

  bool on_state_change (const state_machine &sm,
			state_machine::state_t src_sm_val,
			state_machine::state_t dst_sm_val,
			tree dst_rep,
			svalue_id dst_origin_sid) FINAL OVERRIDE
  {
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

    tree origin_rep
      = dst_state.get_representative_tree (dst_origin_sid);

    if (origin_rep == NULL_TREE)
      origin_rep = get_any_origin (stmt, dst_rep, dst_state);
    m_emission_path->add_event (new state_change_event (supernode,
							stmt,
							stack_depth,
							sm,
							dst_rep,
							src_sm_val,
							dst_sm_val,
							origin_rep,
							dst_state));
    return false;
  }

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
	  /* Ideally we'd directly compare the SM state between src state
	     and dst state, but there's no guarantee that the IDs can
	     be meaningfully compared.  */
	  svalue_id dst_sid = (*iter).first;
	  state_machine::state_t dst_sm_val = (*iter).second.m_state;

	  auto_vec<path_var> dst_pvs;
	  dst_state.m_region_model->get_path_vars_for_svalue (dst_sid,
							      &dst_pvs);

	  unsigned j;
	  path_var *dst_pv;
	  FOR_EACH_VEC_ELT (dst_pvs, j, dst_pv)
	    {
	      tree dst_rep = dst_pv->m_tree;
	      gcc_assert (dst_rep);
	      if (dst_pv->m_stack_depth
		  >= src_state.m_region_model->get_stack_depth ())
		continue;
	      tentative_region_model_context ctxt;
	      svalue_id src_sid
		= src_state.m_region_model->get_rvalue (*dst_pv, &ctxt);
	      if (src_sid.null_p () || ctxt.had_errors_p ())
		continue;
	      state_machine::state_t src_sm_val = src_smap.get_state (src_sid);
	      if (dst_sm_val != src_sm_val)
		{
		  svalue_id dst_origin_sid = (*iter).second.m_origin;
		  if (visitor->on_state_change (sm, src_sm_val, dst_sm_val,
						dst_rep, dst_origin_sid))
		    return true;
		}
	    }
	}
    }
  return false;
}

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
  state_change_event_creator visitor (eedge, emission_path);
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
      }
      break;
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
				tree var,
				state_machine::state_t state) const
{
  LOG_FUNC (get_logger ());
  path->maybe_log (get_logger (), "path");
  prune_for_sm_diagnostic (path, sm, var, state);
  prune_interproc_events (path);
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
					     tree var,
					     state_machine::state_t state) const
{
  update_for_unsuitable_sm_exprs (&var);

  int idx = path->num_events () - 1;
  while (idx >= 0 && idx < (signed)path->num_events ())
    {
      checker_event *base_event = path->get_checker_event (idx);
      if (get_logger ())
	{
	  if (sm)
	    {
	      if (var)
		log ("considering event %i, with var: %qE, state: %qs",
		     idx, var, sm->get_state_name (state));
	      else
		log ("considering event %i, with global state: %qs",
		     idx, sm->get_state_name (state));
	    }
	  else
	    log ("considering event %i", idx);
	}
      gcc_assert (var == NULL || can_be_expr_of_interest_p (var));
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
	    /* If this stmt is the origin of "var", update var.  */
	    if (var)
	      {
		statement_event *stmt_event = (statement_event *)base_event;
		tree new_var = get_any_origin (stmt_event->m_stmt, var,
					       stmt_event->m_dst_state);
		if (new_var)
		  {
		    log ("event %i: switching var of interest from %qE to %qE",
			 idx, var, new_var);
		    var = new_var;
		  }
	      }
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
	    /* Use region IDs to compare var with the state_change's m_var,
	       bulletproofing against cases where they can't have lvalues by
	       using tentative_region_model_context.  */
	    tentative_region_model_context ctxt;
	    region_id state_var_rid
	      = state_change->get_lvalue (state_change->m_var, &ctxt);
	    region_id var_rid = state_change->get_lvalue (var, &ctxt);
	    if (state_var_rid == var_rid && !ctxt.had_errors_p ())
	      {
		if (state_change->m_origin)
		  {
		    log ("event %i: switching var of interest from %qE to %qE",
			 idx, var, state_change->m_origin);
		    var = state_change->m_origin;
		    update_for_unsuitable_sm_exprs (&var);
		  }
		log ("event %i: switching state of interest from %qs to %qs",
		     idx, sm->get_state_name (state_change->m_to),
		     sm->get_state_name (state_change->m_from));
		state = state_change->m_from;
	      }
	    else if (m_verbosity < 4)
	      {
		if (var)
		  log ("filtering event %i:"
		       " state change to %qE unrelated to %qE",
		       idx, state_change->m_var, var);
		else
		  log ("filtering event %i: state change to %qE",
		       idx, state_change->m_var);
		if (ctxt.had_errors_p ())
		  log ("context had errors");
		path->delete_event (idx);
	      }
	  }
	  break;

	case EK_START_CFG_EDGE:
	  {
	    cfg_edge_event *event = (cfg_edge_event *)base_event;
	    const cfg_superedge& cfg_superedge
	      = event->get_cfg_superedge ();
	    const supernode *dest = event->m_sedge->m_dest;
	    /* Do we have an SSA_NAME defined via a phi node in
	       the dest CFG node?  */
	    if (var && TREE_CODE (var) == SSA_NAME)
	      if (SSA_NAME_DEF_STMT (var)->bb == dest->m_bb)
		{
		  if (gphi *phi
		      = dyn_cast <gphi *> (SSA_NAME_DEF_STMT (var)))
		    {
		      /* Update var based on its phi node.  */
		      tree old_var = var;
		      var = cfg_superedge.get_phi_arg (phi);
		      log ("updating from %qE to %qE based on phi node",
			   old_var, var);
		      if (get_logger ())
			{
			  pretty_printer pp;
			  pp_gimple_stmt_1 (&pp, phi, 0, (dump_flags_t)0);
			  log ("  phi: %s", pp_formatted_text (&pp));
			}
		      /* If we've chosen a bad exploded_path, then the
			 phi arg might be a constant.  Fail gracefully for
			 this case.  */
		      update_for_unsuitable_sm_exprs (&var);
		    }
		}

	    /* TODO: is this edge significant to var?
	       See if var can be in other states in the dest, but not
	       in other states in the src?
	       Must have multiple sibling edges.  */

	    if (event->should_filter_p (m_verbosity))
	      {
		log ("filtering event %i: CFG edge", idx);
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
	    const callgraph_superedge& cg_superedge
	      = event->get_callgraph_superedge ();
	    callsite_expr expr;
	    tree caller_var
	      = cg_superedge.map_expr_from_callee_to_caller (var, &expr);
	    if (caller_var)
	      {
		log ("event %i:"
		     " switching var of interest"
		     " from %qE in callee to %qE in caller",
		     idx, var, caller_var);
		var = caller_var;
		if (expr.param_p ())
		  event->record_critical_state (var, state);
		update_for_unsuitable_sm_exprs (&var);
	      }
	  }
	  break;

	case EK_RETURN_EDGE:
	  // TODO: potentially update var/state based on return value,
	  // args etc
	  {
	    if (var)
	      {
		return_event *event = (return_event *)base_event;
		const callgraph_superedge& cg_superedge
		  = event->get_callgraph_superedge ();
		callsite_expr expr;
		tree callee_var
		  = cg_superedge.map_expr_from_caller_to_callee (var, &expr);
		if (callee_var)
		  {
		    log ("event %i:"
			 " switching var of interest"
			 " from %qE in caller to %qE in callee",
			 idx, var, callee_var);
		    var = callee_var;
		    if (expr.return_value_p ())
		      event->record_critical_state (var, state);
		    update_for_unsuitable_sm_exprs (&var);
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
      int idx = path->num_events () - 1;
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
