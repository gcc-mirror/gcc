/* Classes for representing the state of interest at a given path of analysis.
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
#include "diagnostic-core.h"
#include "diagnostic.h"
#include "function.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/sm.h"
#include "sbitmap.h"
#include "bitmap.h"
#include "tristate.h"
#include "ordered-hash-map.h"
#include "selftest.h"
#include "analyzer/region-model.h"
#include "analyzer/program-state.h"
#include "analyzer/constraint-manager.h"
#include "alloc-pool.h"
#include "fibonacci_heap.h"
#include "shortest-paths.h"
#include "analyzer/constraint-manager.h"
#include "diagnostic-event-id.h"
#include "analyzer/pending-diagnostic.h"
#include "analyzer/diagnostic-manager.h"
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
#include "analyzer/state-purge.h"
#include "analyzer/analyzer-selftests.h"

#if ENABLE_ANALYZER

namespace ana {

/* class extrinsic_state.  */

/* Dump a multiline representation of this state to PP.  */

void
extrinsic_state::dump_to_pp (pretty_printer *pp) const
{
  pp_printf (pp, "extrinsic_state: %i checker(s)\n", get_num_checkers ());
  unsigned i;
  state_machine *checker;
  FOR_EACH_VEC_ELT (m_checkers, i, checker)
    {
      pp_printf (pp, "m_checkers[%i]: %qs\n", i, checker->get_name ());
      checker->dump_to_pp (pp);
    }
}

/* Dump a multiline representation of this state to OUTF.  */

void
extrinsic_state::dump_to_file (FILE *outf) const
{
  pretty_printer pp;
  if (outf == stderr)
    pp_show_color (&pp) = pp_show_color (global_dc->printer);
  pp.buffer->stream = outf;
  dump_to_pp (&pp);
  pp_flush (&pp);
}

/* Dump a multiline representation of this state to stderr.  */

DEBUG_FUNCTION void
extrinsic_state::dump () const
{
  dump_to_file (stderr);
}

/* class sm_state_map.  */

/* sm_state_map's ctor.  */

sm_state_map::sm_state_map ()
: m_map (), m_global_state (0)
{
}

/* Clone the sm_state_map.  */

sm_state_map *
sm_state_map::clone () const
{
  return new sm_state_map (*this);
}

/* Clone this sm_state_map, remapping all svalue_ids within it with ID_MAP.

   Return NULL if there are any svalue_ids that have sm-state for which
   ID_MAP maps them to svalue_id::null (and thus the clone would have lost
   the sm-state information). */

sm_state_map *
sm_state_map::clone_with_remapping (const one_way_svalue_id_map &id_map) const
{
  sm_state_map *result = new sm_state_map ();
  result->m_global_state = m_global_state;
  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end ();
       ++iter)
    {
      svalue_id sid = (*iter).first;
      gcc_assert (!sid.null_p ());
      entry_t e = (*iter).second;
      /* TODO: what should we do if the origin maps from non-null to null?
	 Is that loss of information acceptable?  */
      id_map.update (&e.m_origin);

      svalue_id new_sid = id_map.get_dst_for_src (sid);
      if (new_sid.null_p ())
	{
	  delete result;
	  return NULL;
	}
      result->m_map.put (new_sid, e);
    }
  return result;
}

/* Print this sm_state_map (for SM) to PP.
   If MODEL is non-NULL, print representative tree values where
   available.  */

void
sm_state_map::print (const state_machine &sm, const region_model *model,
		     pretty_printer *pp) const
{
  bool first = true;
  pp_string (pp, "{");
  if (m_global_state != 0)
    {
      pp_printf (pp, "global: %s", sm.get_state_name (m_global_state));
      first = false;
    }
  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end ();
       ++iter)
    {
      if (!first)
	pp_string (pp, ", ");
      first = false;
      svalue_id sid = (*iter).first;
      sid.print (pp);

      entry_t e = (*iter).second;
      pp_printf (pp, ": %s", sm.get_state_name (e.m_state));
      if (model)
	if (tree rep = model->get_representative_tree (sid))
	  {
	    pp_string (pp, " (");
	    dump_quoted_tree (pp, rep);
	    pp_character (pp, ')');
	  }
      if (!e.m_origin.null_p ())
	{
	  pp_string (pp, " (origin: ");
	  e.m_origin.print (pp);
	  if (model)
	    if (tree rep = model->get_representative_tree (e.m_origin))
	      {
		pp_string (pp, " (");
		dump_quoted_tree (pp, rep);
		pp_character (pp, ')');
	      }
	  pp_string (pp, ")");
	}
    }
  pp_string (pp, "}");
}

/* Dump this object (for SM) to stderr.  */

DEBUG_FUNCTION void
sm_state_map::dump (const state_machine &sm) const
{
  pretty_printer pp;
  pp_show_color (&pp) = pp_show_color (global_dc->printer);
  pp.buffer->stream = stderr;
  print (sm, NULL, &pp);
  pp_newline (&pp);
  pp_flush (&pp);
}

/* Return true if no states have been set within this map
   (all expressions are for the start state).  */

bool
sm_state_map::is_empty_p () const
{
  return m_map.elements () == 0 && m_global_state == 0;
}

/* Generate a hash value for this sm_state_map.  */

hashval_t
sm_state_map::hash () const
{
  hashval_t result = 0;

  /* Accumulate the result by xoring a hash for each slot, so that the
     result doesn't depend on the ordering of the slots in the map.  */

  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end ();
       ++iter)
    {
      inchash::hash hstate;
      inchash::add ((*iter).first, hstate);
      entry_t e = (*iter).second;
      hstate.add_int (e.m_state);
      inchash::add (e.m_origin, hstate);
      result ^= hstate.end ();
    }
  result ^= m_global_state;

  return result;
}

/* Equality operator for sm_state_map.  */

bool
sm_state_map::operator== (const sm_state_map &other) const
{
  if (m_global_state != other.m_global_state)
    return false;

  if (m_map.elements () != other.m_map.elements ())
    return false;

  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end ();
       ++iter)
    {
      svalue_id sid = (*iter).first;
      entry_t e = (*iter).second;
      entry_t *other_slot = const_cast <map_t &> (other.m_map).get (sid);
      if (other_slot == NULL)
	return false;
      if (e != *other_slot)
	return false;
    }

  gcc_checking_assert (hash () == other.hash ());

  return true;
}

/* Get the state of SID within this object.
   States default to the start state.  */

state_machine::state_t
sm_state_map::get_state (svalue_id sid) const
{
  gcc_assert (!sid.null_p ());

  if (entry_t *slot
      = const_cast <map_t &> (m_map).get (sid))
    return slot->m_state;
  else
    return 0;
}

/* Get the "origin" svalue_id for any state of SID.  */

svalue_id
sm_state_map::get_origin (svalue_id sid) const
{
  gcc_assert (!sid.null_p ());

  entry_t *slot
    = const_cast <map_t &> (m_map).get (sid);
  if (slot)
    return slot->m_origin;
  else
    return svalue_id::null ();
}

/* Set the state of SID within MODEL to STATE, recording that
   the state came from ORIGIN.  */

void
sm_state_map::set_state (region_model *model,
			 svalue_id sid,
			 state_machine::state_t state,
			 svalue_id origin)
{
  if (model == NULL)
    return;
  equiv_class &ec = model->get_constraints ()->get_equiv_class (sid);
  if (!set_state (ec, state, origin))
    return;

  /* Also do it for all svalues that are equal via non-cm, so that
     e.g. (void *)&r and (foo *)&r transition together.  */
  for (unsigned i = 0; i < model->get_num_svalues (); i++)
    {
      svalue_id other_sid = svalue_id::from_int (i);
      if (other_sid == sid)
	continue;

      tristate eq = model->eval_condition_without_cm (sid, EQ_EXPR, other_sid);
      if (eq.is_true ())
	impl_set_state (other_sid, state, origin);
    }
}

/* Set the state of EC to STATE, recording that the state came from
   ORIGIN.
   Return true if any states of svalue_ids within EC changed.  */

bool
sm_state_map::set_state (const equiv_class &ec,
			 state_machine::state_t state,
			 svalue_id origin)
{
  int i;
  svalue_id *sid;
  bool any_changed = false;
  FOR_EACH_VEC_ELT (ec.m_vars, i, sid)
    any_changed |= impl_set_state (*sid, state, origin);
  return any_changed;
}

/* Set state of SID to STATE, bypassing equivalence classes.
   Return true if the state changed.  */

bool
sm_state_map::impl_set_state (svalue_id sid, state_machine::state_t state,
			      svalue_id origin)
{
  if (get_state (sid) == state)
    return false;

  /* Special-case state 0 as the default value.  */
  if (state == 0)
    {
      if (m_map.get (sid))
	m_map.remove (sid);
      return true;
    }
  gcc_assert (!sid.null_p ());
  m_map.put (sid, entry_t (state, origin));
  return true;
}

/* Set the "global" state within this state map to STATE.  */

void
sm_state_map::set_global_state (state_machine::state_t state)
{
  m_global_state = state;
}

/* Get the "global" state within this state map.  */

state_machine::state_t
sm_state_map::get_global_state () const
{
  return m_global_state;
}

/* Handle CALL to unknown FNDECL with an unknown function body, which
   could do anything to the states passed to it.
   Clear any state for SM for the params and any LHS.
   Note that the function might be known to other state machines, but
   not to this one.  */

void
sm_state_map::purge_for_unknown_fncall (const exploded_graph &eg,
					const state_machine &sm,
					const gcall *call,
					tree fndecl,
					region_model *new_model,
					region_model_context *ctxt)
{
  logger * const logger = eg.get_logger ();
  if (logger)
    {
      if (fndecl)
	logger->log ("function %qE is unknown to checker %qs",
		     fndecl, sm.get_name ());
      else
	logger->log ("unknown function pointer for checker %qs",
		     sm.get_name ());
    }

  /* Purge any state for parms.  */
  tree iter_param_types = NULL_TREE;
  if (fndecl)
    iter_param_types = TYPE_ARG_TYPES (TREE_TYPE (fndecl));
  for (unsigned arg_idx = 0; arg_idx < gimple_call_num_args (call); arg_idx++)
    {
      /* Track expected param type, where available.  */
      if (iter_param_types)
	{
	  tree param_type = TREE_VALUE (iter_param_types);
	  gcc_assert (param_type);
	  iter_param_types = TREE_CHAIN (iter_param_types);

	  /* Don't purge state if it was passed as a const pointer
	     e.g. for things like strlen (PTR).  */
	  if (TREE_CODE (param_type) == POINTER_TYPE)
	    if (TYPE_READONLY (TREE_TYPE (param_type)))
	      continue;
	}
      tree parm = gimple_call_arg (call, arg_idx);
      svalue_id parm_sid = new_model->get_rvalue (parm, ctxt);
      set_state (new_model, parm_sid, 0, svalue_id::null ());

      /* Also clear sm-state from svalue_ids that are passed via a
	 pointer.  */
      if (TREE_CODE (parm) == ADDR_EXPR)
	{
	  tree pointee = TREE_OPERAND (parm, 0);
	  svalue_id parm_sid = new_model->get_rvalue (pointee, ctxt);
	  set_state (new_model, parm_sid, 0, svalue_id::null ());
	}
    }

  /* Purge any state for any LHS.  */
  if (tree lhs = gimple_call_lhs (call))
    {
      svalue_id lhs_sid = new_model->get_rvalue (lhs, ctxt);
      set_state (new_model, lhs_sid, 0, svalue_id::null ());
    }
}

/* Update this map based on MAP.  */

void
sm_state_map::remap_svalue_ids (const svalue_id_map &map)
{
  map_t tmp_map;

  /* Build an intermediate map, using the new sids.  */
  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end ();
       ++iter)
    {
      svalue_id sid = (*iter).first;
      entry_t e = (*iter).second;

      map.update (&sid);
      map.update (&e.m_origin);
      tmp_map.put (sid, e);
    }

  /* Clear the existing values.  */
  m_map.empty ();

  /* Copy over from intermediate map.  */
  for (map_t::iterator iter = tmp_map.begin ();
       iter != tmp_map.end ();
       ++iter)
    {
      svalue_id sid = (*iter).first;
      entry_t e = (*iter).second;

      impl_set_state (sid, e.m_state, e.m_origin);
    }
}

/* Purge any state for svalue_ids >= FIRST_UNUSED_SID.
   If !SM::can_purge_p, then report the state as leaking,
   using SM_IDX, CTXT, and MAP.
   Return the number of states that were purged.  */

int
sm_state_map::on_svalue_purge (const state_machine &sm,
			       int sm_idx,
			       svalue_id first_unused_sid,
			       const svalue_id_map &map,
			       impl_region_model_context *ctxt)
{
  /* TODO: ideally remove the slot directly; for now
     do it in two stages.  */
  auto_vec<svalue_id> to_remove;
  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end ();
       ++iter)
    {
      svalue_id dst_sid ((*iter).first);
      if (dst_sid.as_int () >= first_unused_sid.as_int ())
	{
	  /* Complain about leaks here.  */
	  entry_t e = (*iter).second;

	  if (!sm.can_purge_p (e.m_state))
	    ctxt->on_state_leak (sm, sm_idx, dst_sid, first_unused_sid,
				 map, e.m_state);

	  to_remove.safe_push (dst_sid);
	}
      else if ((*iter).second.m_origin.as_int () >= first_unused_sid.as_int ())
	{
	  /* If the origin svalue is being purged, then reset it to null.  */
	  (*iter).second.m_origin = svalue_id::null ();
	}
    }

  int i;
  svalue_id *dst_sid;
  FOR_EACH_VEC_ELT (to_remove, i, dst_sid)
    m_map.remove (*dst_sid);

  return to_remove.length ();
}

/* Set the state of CHILD_SID to that of PARENT_SID.  */

void
sm_state_map::on_inherited_svalue (svalue_id parent_sid,
				   svalue_id child_sid)
{
  state_machine::state_t state = get_state (parent_sid);
  impl_set_state (child_sid, state, parent_sid);
}

/* Set the state of DST_SID to that of SRC_SID.  */

void
sm_state_map::on_cast (svalue_id src_sid,
		       svalue_id dst_sid)
{
  state_machine::state_t state = get_state (src_sid);
  impl_set_state (dst_sid, state, get_origin (src_sid));
}

/* Purge state from SID (in response to a call to an unknown function).  */

void
sm_state_map::on_unknown_change (svalue_id sid)
{
  impl_set_state (sid, (state_machine::state_t)0, svalue_id::null ());
}

/* Assert that this object is sane.  */

void
sm_state_map::validate (const state_machine &sm,
			int num_svalues) const
{
  /* Skip this in a release build.  */
#if !CHECKING_P
  return;
#endif

  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end ();
       ++iter)
    {
      svalue_id sid = (*iter).first;
      entry_t e = (*iter).second;

      gcc_assert (sid.as_int () < num_svalues);
      sm.validate (e.m_state);
      gcc_assert (e.m_origin.as_int () < num_svalues);
    }
}

/* class program_state.  */

/* program_state's ctor.  */

program_state::program_state (const extrinsic_state &ext_state)
: m_region_model (new region_model ()),
  m_checker_states (ext_state.get_num_checkers ()),
  m_valid (true)
{
  int num_states = ext_state.get_num_checkers ();
  for (int i = 0; i < num_states; i++)
    m_checker_states.quick_push (new sm_state_map ());
}

/* program_state's copy ctor.  */

program_state::program_state (const program_state &other)
: m_region_model (new region_model (*other.m_region_model)),
  m_checker_states (other.m_checker_states.length ()),
  m_valid (true)
{
  int i;
  sm_state_map *smap;
  FOR_EACH_VEC_ELT (other.m_checker_states, i, smap)
    m_checker_states.quick_push (smap->clone ());
}

/* program_state's assignment operator.  */

program_state&
program_state::operator= (const program_state &other)
{
  delete m_region_model;
  m_region_model = new region_model (*other.m_region_model);

  int i;
  sm_state_map *smap;
  FOR_EACH_VEC_ELT (m_checker_states, i, smap)
    delete smap;
  m_checker_states.truncate (0);
  gcc_assert (m_checker_states.space (other.m_checker_states.length ()));

  FOR_EACH_VEC_ELT (other.m_checker_states, i, smap)
    m_checker_states.quick_push (smap->clone ());

  m_valid = other.m_valid;

  return *this;
}

#if __cplusplus >= 201103
/* Move constructor for program_state (when building with C++11).  */
program_state::program_state (program_state &&other)
: m_region_model (other.m_region_model),
  m_checker_states (other.m_checker_states.length ())
{
  other.m_region_model = NULL;

  int i;
  sm_state_map *smap;
  FOR_EACH_VEC_ELT (other.m_checker_states, i, smap)
    m_checker_states.quick_push (smap);
  other.m_checker_states.truncate (0);

  m_valid = other.m_valid;
}
#endif

/* program_state's dtor.  */

program_state::~program_state ()
{
  delete m_region_model;
}

/* Generate a hash value for this program_state.  */

hashval_t
program_state::hash () const
{
  hashval_t result = m_region_model->hash ();

  int i;
  sm_state_map *smap;
  FOR_EACH_VEC_ELT (m_checker_states, i, smap)
    result ^= smap->hash ();
  return result;
}

/* Equality operator for program_state.
   All parts of the program_state (region model, checker states) must
   equal their counterparts in OTHER for the two program_states to be
   considered equal.  */

bool
program_state::operator== (const program_state &other) const
{
  if (!(*m_region_model == *other.m_region_model))
    return false;

  int i;
  sm_state_map *smap;
  FOR_EACH_VEC_ELT (m_checker_states, i, smap)
    if (!(*smap == *other.m_checker_states[i]))
      return false;

  gcc_checking_assert (hash () == other.hash ());

  return true;
}

/* Print a compact representation of this state to PP.  */

void
program_state::print (const extrinsic_state &ext_state,
		      pretty_printer *pp) const
{
  pp_printf (pp, "rmodel: ");
  m_region_model->print (pp);
  pp_newline (pp);

  int i;
  sm_state_map *smap;
  FOR_EACH_VEC_ELT (m_checker_states, i, smap)
    {
      if (!smap->is_empty_p ())
	{
	  pp_printf (pp, "%s: ", ext_state.get_name (i));
	  smap->print (ext_state.get_sm (i), m_region_model, pp);
	  pp_newline (pp);
	}
    }
  if (!m_valid)
    {
      pp_printf (pp, "invalid state");
      pp_newline (pp);
    }
}

/* Dump a representation of this state to PP.
   If SUMMARIZE is true, print a one-line summary;
   if false, print a detailed multiline representation.  */

void
program_state::dump_to_pp (const extrinsic_state &ext_state,
			   bool summarize,
			   pretty_printer *pp) const
{
  pp_printf (pp, "rmodel: ");
  m_region_model->dump_to_pp (pp, summarize);

  int i;
  sm_state_map *smap;
  FOR_EACH_VEC_ELT (m_checker_states, i, smap)
    {
      if (!smap->is_empty_p ())
	{
	  if (summarize)
	    pp_space (pp);
	  pp_printf (pp, "%s: ", ext_state.get_name (i));
	  smap->print (ext_state.get_sm (i), m_region_model, pp);
	  if (!summarize)
	    pp_newline (pp);
	}
    }

  if (!m_valid)
    {
      if (summarize)
	pp_space (pp);
      pp_printf (pp, "invalid state");
      if (!summarize)
	pp_newline (pp);
    }
}

/* Dump a multiline representation of this state to OUTF.  */

void
program_state::dump_to_file (const extrinsic_state &ext_state,
			     bool summarize,
			     FILE *outf) const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  if (outf == stderr)
    pp_show_color (&pp) = pp_show_color (global_dc->printer);
  pp.buffer->stream = outf;
  dump_to_pp (ext_state, summarize, &pp);
  pp_flush (&pp);
}

/* Dump a multiline representation of this state to stderr.  */

DEBUG_FUNCTION void
program_state::dump (const extrinsic_state &ext_state,
		     bool summarize) const
{
  dump_to_file (ext_state, summarize, stderr);
}

/* Determine if following edge SUCC from ENODE is valid within the graph EG
   and update this state accordingly in-place.

   Return true if the edge can be followed, or false otherwise.

   Check for relevant conditionals and switch-values for conditionals
   and switch statements, adding the relevant conditions to this state.
   Push/pop frames for interprocedural edges and update params/returned
   values.

   This is the "state" half of exploded_node::on_edge.  */

bool
program_state::on_edge (exploded_graph &eg,
			const exploded_node &enode,
			const superedge *succ,
			state_change *change)
{
  /* Update state.  */
  const program_point &point = enode.get_point ();
  const gimple *last_stmt = point.get_supernode ()->get_last_stmt ();

  /* For conditionals and switch statements, add the
     relevant conditions (for the specific edge) to new_state;
     skip edges for which the resulting constraints
     are impossible.
     This also updates frame information for call/return superedges.
     Adding the relevant conditions for the edge could also trigger
     sm-state transitions (e.g. transitions due to ptrs becoming known
     to be NULL or non-NULL) */

  impl_region_model_context ctxt (eg, &enode,
				  &enode.get_state (),
				  this, change,
				  last_stmt);
  if (!m_region_model->maybe_update_for_edge (*succ,
					      last_stmt,
					      &ctxt))
    {
      logger * const logger = eg.get_logger ();
      if (logger)
	logger->log ("edge to SN: %i is impossible"
		     " due to region_model constraints",
		     succ->m_dest->m_index);
      return false;
    }

  return true;
}

/* Generate a simpler version of THIS, discarding state that's no longer
   relevant at POINT.
   The idea is that we're more likely to be able to consolidate
   multiple (point, state) into single exploded_nodes if we discard
   irrelevant state (e.g. at the end of functions).

   Retain state affected by CHANGE, to make it easier to generate
   state_change_events.  */

program_state
program_state::prune_for_point (exploded_graph &eg,
				const program_point &point,
				state_change *change) const
{
  logger * const logger = eg.get_logger ();
  LOG_SCOPE (logger);

  function *fun = point.get_function ();
  if (!fun)
    return *this;

  program_state new_state (*this);

  purge_stats stats;

  const state_purge_map *pm = eg.get_purge_map ();
  if (pm)
    {
      region_id_set purgeable_ssa_regions (new_state.m_region_model);
      region_id frame_rid
	= new_state.m_region_model->get_current_frame_id ();
      frame_region *frame
	= new_state.m_region_model->get_region <frame_region>(frame_rid);

      /* TODO: maybe move to a member of region_model?  */

      auto_vec<tree> ssa_names_to_purge;
      for (frame_region::map_t::iterator iter = frame->begin ();
	   iter != frame->end ();
	   ++iter)
	{
	  tree var = (*iter).first;
	  region_id rid = (*iter).second;
	  if (TREE_CODE (var) == SSA_NAME)
	    {
	      const state_purge_per_ssa_name &per_ssa
		= pm->get_data_for_ssa_name (var);
	      if (!per_ssa.needed_at_point_p (point.get_function_point ()))
		{
		  region *region
		    = new_state.m_region_model->get_region (rid);
		  svalue_id sid = region->get_value_direct ();
		  if (!sid.null_p ())
		    {
		      if (!new_state.can_purge_p (eg.get_ext_state (), sid))
			{
			  /* (currently only state maps can keep things
			     alive).  */
			  if (logger)
			    logger->log ("not purging RID: %i for %qE"
					 " (used by state map)",
					 rid.as_int (), var);
			  continue;
			}

		      /* Don't purge regions containing svalues that
			 have a change of sm-state, to make it easier to
			 generate state_change_event messages.  */
		      if (change)
			if (change->affects_p (sid))
			  {
			    if (logger)
			      logger->log ("not purging RID: %i for %qE"
					   " (affected by change)",
					   rid.as_int (), var);
			    continue;
			  }
		    }
		  purgeable_ssa_regions.add_region (rid);
		  ssa_names_to_purge.safe_push (var);
		  if (logger)
		    logger->log ("purging RID: %i for %qE", rid.as_int (), var);
		  /* We also need to remove the region from the map.
		     We're in mid-traversal, so the removal is done in
		     unbind below.  */
		}
	    }
	}

      /* Unbind the regions from the frame's map of vars-to-regions.  */
      unsigned i;
      tree var;
      FOR_EACH_VEC_ELT (ssa_names_to_purge, i, var)
	frame->unbind (var);

      /* Purge the regions.  Nothing should point to them, and they
	 should have no children, as they are for SSA names.  */
      new_state.m_region_model->purge_regions (purgeable_ssa_regions,
					       &stats,
					       eg.get_logger ());
    }

  /* Purge unused svalues.  */
  // TODO: which enode to use, if any?
  impl_region_model_context ctxt (eg, NULL,
				  this,
				  &new_state,
				  change,
				  NULL);
  new_state.m_region_model->purge_unused_svalues (&stats, &ctxt);
  if (logger)
    {
      logger->log ("num svalues purged: %i", stats.m_num_svalues);
      logger->log ("num regions purged: %i", stats.m_num_regions);
      logger->log ("num equiv_classes purged: %i", stats.m_num_equiv_classes);
      logger->log ("num constraints purged: %i", stats.m_num_constraints);
      logger->log ("num sm map items purged: %i", stats.m_num_client_items);
    }

  new_state.m_region_model->canonicalize (&ctxt);

  return new_state;
}

/* Remap all svalue_ids in this state's m_checker_states according to MAP.
   The svalues_ids in the region_model are assumed to already have been
   remapped.  */

void
program_state::remap_svalue_ids (const svalue_id_map &map)
{
  int i;
  sm_state_map *smap;
  FOR_EACH_VEC_ELT (m_checker_states, i, smap)
    smap->remap_svalue_ids (map);
}

/* Attempt to return a tree that represents SID, or return NULL_TREE.
   Find the first region that stores the value (e.g. a local) and
   generate a representative tree for it.  */

tree
program_state::get_representative_tree (svalue_id sid) const
{
  return m_region_model->get_representative_tree (sid);
}

/* Attempt to merge this state with OTHER, both using EXT_STATE.
   Write the result to *OUT.
   If the states were merged successfully, return true.  */

bool
program_state::can_merge_with_p (const program_state &other,
				 const extrinsic_state &ext_state,
				 program_state *out) const
{
  gcc_assert (out);

  /* TODO:  initially I had an early reject here if there
     are sm-differences between the states.  However, this was
     falsely rejecting merger opportunities for states where the
     only difference was in svalue_id ordering.  */

  /* Attempt to merge the region_models.  */

  svalue_id_merger_mapping sid_mapping (*m_region_model,
					*other.m_region_model);
  if (!m_region_model->can_merge_with_p (*other.m_region_model,
					 out->m_region_model,
					 &sid_mapping))
    return false;

  /* Copy m_checker_states to result, remapping svalue_ids using
     sid_mapping.  */
  int i;
  sm_state_map *smap;
  FOR_EACH_VEC_ELT (out->m_checker_states, i, smap)
    delete smap;
  out->m_checker_states.truncate (0);

  /* Remap this and other's m_checker_states using sid_mapping.
     Only merge states that have equality between the two end-results:
     sm-state differences are likely to be interesting to end-users, and
     hence are worth exploring as separate paths in the exploded graph.  */
  FOR_EACH_VEC_ELT (m_checker_states, i, smap)
    {
      sm_state_map *other_smap = other.m_checker_states[i];

      /* If clone_with_remapping returns NULL for one of the input smaps,
	 then it has sm-state for an svalue_id where the svalue_id is
	 being mapped to svalue_id::null in its sid_mapping, meaning that
	 the svalue is to be dropped during the merger.  We don't want
	 to lose sm-state during a state merger, so return false for these
	 cases.  */
      sm_state_map *remapped_a_smap
	= smap->clone_with_remapping (sid_mapping.m_map_from_a_to_m);
      if (!remapped_a_smap)
	return false;
      sm_state_map *remapped_b_smap
	= other_smap->clone_with_remapping (sid_mapping.m_map_from_b_to_m);
      if (!remapped_b_smap)
	{
	  delete remapped_a_smap;
	  return false;
	}

      /* Both states have sm-state for the same values; now ensure that the
	 states are equal.  */
      if (*remapped_a_smap == *remapped_b_smap)
	{
	  out->m_checker_states.safe_push (remapped_a_smap);
	  delete remapped_b_smap;
	}
      else
	{
	  /* Don't merge if there are sm-state differences.  */
	  delete remapped_a_smap;
	  delete remapped_b_smap;
	  return false;
	}
    }

  impl_region_model_context ctxt (out, NULL, ext_state);
  out->m_region_model->canonicalize (&ctxt);

  return true;
}

/* Assert that this object is valid.  */

void
program_state::validate (const extrinsic_state &ext_state) const
{
  /* Skip this in a release build.  */
#if !CHECKING_P
  return;
#endif

  m_region_model->validate ();
  gcc_assert (m_checker_states.length () == ext_state.get_num_checkers ());
  int sm_idx;
  sm_state_map *smap;
  FOR_EACH_VEC_ELT (m_checker_states, sm_idx, smap)
    {
      const state_machine &sm = ext_state.get_sm (sm_idx);
      smap->validate (sm, m_region_model->get_num_svalues ());
    }
}

/* Dump this sm_change to PP.  */

void
state_change::sm_change::dump (pretty_printer *pp,
			       const extrinsic_state &ext_state) const
{
  const state_machine &sm = get_sm (ext_state);
  pp_string (pp, "(");
  m_new_sid.print (pp);
  pp_printf (pp, ": %s: %qs -> %qs)",
	     sm.get_name (),
	     sm.get_state_name (m_old_state),
	     sm.get_state_name (m_new_state));
}

/* Remap all svalue_ids in this change according to MAP.  */

void
state_change::sm_change::remap_svalue_ids (const svalue_id_map &map)
{
  map.update (&m_new_sid);
}

/* Purge any svalue_ids >= FIRST_UNUSED_SID.
   Return the number of states that were purged.  */

int
state_change::sm_change::on_svalue_purge (svalue_id first_unused_sid)
{
  if (m_new_sid.as_int () >= first_unused_sid.as_int ())
    {
      m_new_sid = svalue_id::null ();
      return 1;
    }

  return 0;
}

/* Assert that this object is sane.  */

void
state_change::sm_change::validate (const program_state &new_state,
				   const extrinsic_state &ext_state) const
{
  gcc_assert ((unsigned)m_sm_idx < ext_state.get_num_checkers ());
  const state_machine &sm = ext_state.get_sm (m_sm_idx);
  sm.validate (m_old_state);
  sm.validate (m_new_state);
  m_new_sid.validate (*new_state.m_region_model);
}

/* state_change's ctor.  */

state_change::state_change ()
{
}

/* state_change's copy ctor.  */

state_change::state_change (const state_change &other)
: m_sm_changes (other.m_sm_changes.length ())
{
  unsigned i;
  sm_change *change;
  FOR_EACH_VEC_ELT (other.m_sm_changes, i, change)
    m_sm_changes.quick_push (*change);
}

/* Record a state-machine state change.  */

void
state_change::add_sm_change (int sm_idx,
			     svalue_id new_sid,
			     state_machine::state_t old_state,
			     state_machine::state_t new_state)
{
  m_sm_changes.safe_push (sm_change (sm_idx,
				     new_sid,
				     old_state, new_state));
}

/* Return true if SID (in the new state) was affected by any
   sm-state changes.  */

bool
state_change::affects_p (svalue_id sid) const
{
  unsigned i;
  sm_change *change;
  FOR_EACH_VEC_ELT (m_sm_changes, i, change)
    {
      if (sid == change->m_new_sid)
	return true;
    }
  return false;
}

/* Dump this state_change to PP.  */

void
state_change::dump (pretty_printer *pp,
		    const extrinsic_state &ext_state) const
{
  unsigned i;
  sm_change *change;
  FOR_EACH_VEC_ELT (m_sm_changes, i, change)
    {
      if (i > 0)
	pp_string (pp, ", ");
      change->dump (pp, ext_state);
    }
}

/* Dump this state_change to stderr.  */

void
state_change::dump (const extrinsic_state &ext_state) const
{
  pretty_printer pp;
  pp_show_color (&pp) = pp_show_color (global_dc->printer);
  pp.buffer->stream = stderr;
  dump (&pp, ext_state);
  pp_newline (&pp);
  pp_flush (&pp);
}

/* Remap all svalue_ids in this state_change according to MAP.  */

void
state_change::remap_svalue_ids (const svalue_id_map &map)
{
  unsigned i;
  sm_change *change;
  FOR_EACH_VEC_ELT (m_sm_changes, i, change)
    change->remap_svalue_ids (map);
}

/* Purge any svalue_ids >= FIRST_UNUSED_SID.
   Return the number of states that were purged.  */

int
state_change::on_svalue_purge (svalue_id first_unused_sid)
{
  int result = 0;
  unsigned i;
  sm_change *change;
  FOR_EACH_VEC_ELT (m_sm_changes, i, change)
    result += change->on_svalue_purge (first_unused_sid);
  return result;
}

/* Assert that this object is sane.  */

void
state_change::validate (const program_state &new_state,
			const extrinsic_state &ext_state) const
{
  /* Skip this in a release build.  */
#if !CHECKING_P
  return;
#endif
  unsigned i;
  sm_change *change;
  FOR_EACH_VEC_ELT (m_sm_changes, i, change)
    change->validate (new_state, ext_state);
}

#if CHECKING_P

namespace selftest {

/* Implementation detail of ASSERT_DUMP_EQ.  */

static void
assert_dump_eq (const location &loc,
		const program_state &state,
		const extrinsic_state &ext_state,
		bool summarize,
		const char *expected)
{
  auto_fix_quotes sentinel;
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  state.dump_to_pp (ext_state, summarize, &pp);
  ASSERT_STREQ_AT (loc, pp_formatted_text (&pp), expected);
}

/* Assert that STATE.dump_to_pp (SUMMARIZE) is EXPECTED.  */

#define ASSERT_DUMP_EQ(STATE, EXT_STATE, SUMMARIZE, EXPECTED)		\
  SELFTEST_BEGIN_STMT							\
  assert_dump_eq ((SELFTEST_LOCATION), (STATE), (EXT_STATE), (SUMMARIZE), \
		  (EXPECTED));						\
  SELFTEST_END_STMT

/* Tests for sm_state_map.  */

static void
test_sm_state_map ()
{
  tree x = build_global_decl ("x", integer_type_node);
  tree y = build_global_decl ("y", integer_type_node);
  tree z = build_global_decl ("z", integer_type_node);

  /* Test setting states on svalue_id instances directly.  */
  {
    region_model model;
    svalue_id sid_x = model.get_rvalue (x, NULL);
    svalue_id sid_y = model.get_rvalue (y, NULL);
    svalue_id sid_z = model.get_rvalue (z, NULL);

    sm_state_map map;
    ASSERT_TRUE (map.is_empty_p ());
    ASSERT_EQ (map.get_state (sid_x), 0);

    map.impl_set_state (sid_x, 42, sid_z);
    ASSERT_EQ (map.get_state (sid_x), 42);
    ASSERT_EQ (map.get_origin (sid_x), sid_z);
    ASSERT_EQ (map.get_state (sid_y), 0);
    ASSERT_FALSE (map.is_empty_p ());

    map.impl_set_state (sid_y, 0, sid_z);
    ASSERT_EQ (map.get_state (sid_y), 0);

    map.impl_set_state (sid_x, 0, sid_z);
    ASSERT_EQ (map.get_state (sid_x), 0);
    ASSERT_TRUE (map.is_empty_p ());
  }

  /* Test setting states via equivalence classes.  */
  {
    region_model model;
    svalue_id sid_x = model.get_rvalue (x, NULL);
    svalue_id sid_y = model.get_rvalue (y, NULL);
    svalue_id sid_z = model.get_rvalue (z, NULL);

    sm_state_map map;
    ASSERT_TRUE (map.is_empty_p ());
    ASSERT_EQ (map.get_state (sid_x), 0);
    ASSERT_EQ (map.get_state (sid_y), 0);

    model.add_constraint (x, EQ_EXPR, y, NULL);

    /* Setting x to a state should also update y, as they
       are in the same equivalence class.  */
    map.set_state (&model, sid_x, 5, sid_z);
    ASSERT_EQ (map.get_state (sid_x), 5);
    ASSERT_EQ (map.get_state (sid_y), 5);
    ASSERT_EQ (map.get_origin (sid_x), sid_z);
    ASSERT_EQ (map.get_origin (sid_y), sid_z);
  }

  /* Test equality and hashing.  */
  {
    region_model model;
    svalue_id sid_y = model.get_rvalue (y, NULL);
    svalue_id sid_z = model.get_rvalue (z, NULL);

    sm_state_map map0;
    sm_state_map map1;
    sm_state_map map2;

    ASSERT_EQ (map0.hash (), map1.hash ());
    ASSERT_EQ (map0, map1);

    map1.impl_set_state (sid_y, 5, sid_z);
    ASSERT_NE (map0.hash (), map1.hash ());
    ASSERT_NE (map0, map1);

    /* Make the same change to map2.  */
    map2.impl_set_state (sid_y, 5, sid_z);
    ASSERT_EQ (map1.hash (), map2.hash ());
    ASSERT_EQ (map1, map2);
  }

  /* Equality and hashing shouldn't depend on ordering.  */
  {
    sm_state_map map0;
    sm_state_map map1;
    sm_state_map map2;

    ASSERT_EQ (map0.hash (), map1.hash ());
    ASSERT_EQ (map0, map1);

    map1.impl_set_state (svalue_id::from_int (14), 2, svalue_id::null ());
    map1.impl_set_state (svalue_id::from_int (16), 3, svalue_id::null ());
    map1.impl_set_state (svalue_id::from_int (1), 2, svalue_id::null ());
    map1.impl_set_state (svalue_id::from_int (9), 2, svalue_id::null ());

    map2.impl_set_state (svalue_id::from_int (1), 2, svalue_id::null ());
    map2.impl_set_state (svalue_id::from_int (16), 3, svalue_id::null ());
    map2.impl_set_state (svalue_id::from_int (14), 2, svalue_id::null ());
    map2.impl_set_state (svalue_id::from_int (9), 2, svalue_id::null ());

    ASSERT_EQ (map1.hash (), map2.hash ());
    ASSERT_EQ (map1, map2);
  }

  /* Test sm_state_map::remap_svalue_ids.  */
  {
    sm_state_map map;
    svalue_id sid_0 = svalue_id::from_int (0);
    svalue_id sid_1 = svalue_id::from_int (1);
    svalue_id sid_2 = svalue_id::from_int (2);

    map.impl_set_state (sid_0, 42, sid_2);
    ASSERT_EQ (map.get_state (sid_0), 42);
    ASSERT_EQ (map.get_origin (sid_0), sid_2);
    ASSERT_EQ (map.get_state (sid_1), 0);
    ASSERT_EQ (map.get_state (sid_2), 0);

    /* Apply a remapping to the IDs.  */
    svalue_id_map remapping (3);
    remapping.put (sid_0, sid_1);
    remapping.put (sid_1, sid_2);
    remapping.put (sid_2, sid_0);
    map.remap_svalue_ids (remapping);

    /* Verify that the IDs have been remapped.  */
    ASSERT_EQ (map.get_state (sid_1), 42);
    ASSERT_EQ (map.get_origin (sid_1), sid_0);
    ASSERT_EQ (map.get_state (sid_2), 0);
    ASSERT_EQ (map.get_state (sid_0), 0);
  }

  // TODO: coverage for purging
}

/* Verify that program_state::dump_to_pp works as expected.  */

static void
test_program_state_dumping ()
{
  /* Create a program_state for a global ptr "p" that has
     malloc sm-state, pointing to a region on the heap.  */
  tree p = build_global_decl ("p", ptr_type_node);

  state_machine *sm = make_malloc_state_machine (NULL);
  const state_machine::state_t UNCHECKED_STATE
    = sm->get_state_by_name ("unchecked");
  auto_delete_vec <state_machine> checkers;
  checkers.safe_push (sm);
  extrinsic_state ext_state (checkers);

  program_state s (ext_state);
  region_model *model = s.m_region_model;
  region_id new_rid = model->add_new_malloc_region ();
  svalue_id ptr_sid
      = model->get_or_create_ptr_svalue (ptr_type_node, new_rid);
  model->set_value (model->get_lvalue (p, NULL),
		    ptr_sid, NULL);
  sm_state_map *smap = s.m_checker_states[0];

  smap->impl_set_state (ptr_sid, UNCHECKED_STATE, svalue_id::null ());
  ASSERT_EQ (smap->get_state (ptr_sid), UNCHECKED_STATE);

  ASSERT_DUMP_EQ
    (s, ext_state, false,
     "rmodel: r0: {kind: `root', parent: null, sval: null}\n"
     "|-heap: r1: {kind: `heap', parent: r0, sval: null}\n"
     "|  `-r2: {kind: `symbolic', parent: r1, sval: null, possibly_null: true}\n"
     "`-globals: r3: {kind: `globals', parent: r0, sval: null, map: {`p': r4}}\n"
     "  `-`p': r4: {kind: `primitive', parent: r3, sval: sv0, type: `void *'}\n"
     "    |: sval: sv0: {type: `void *', &r2}\n"
     "    |: type: `void *'\n"
     "svalues:\n"
     "  sv0: {type: `void *', &r2}\n"
     "constraint manager:\n"
     "  equiv classes:\n"
     "  constraints:\n"
     "malloc: {sv0: unchecked (`p')}\n");

  ASSERT_DUMP_EQ (s, ext_state, true,
		  "rmodel: p: &r2 malloc: {sv0: unchecked (`p')}");
}

/* Verify that program_state::dump_to_pp works for string literals.  */

static void
test_program_state_dumping_2 ()
{
    /* Create a program_state for a global ptr "p" that points to
       a string constant.  */
  tree p = build_global_decl ("p", ptr_type_node);

  tree string_cst_ptr = build_string_literal (4, "foo");

  auto_delete_vec <state_machine> checkers;
  extrinsic_state ext_state (checkers);

  program_state s (ext_state);
  region_model *model = s.m_region_model;
  region_id p_rid = model->get_lvalue (p, NULL);
  svalue_id str_sid = model->get_rvalue (string_cst_ptr, NULL);
  model->set_value (p_rid, str_sid, NULL);

  ASSERT_DUMP_EQ
    (s, ext_state, false,
     "rmodel: r0: {kind: `root', parent: null, sval: null}\n"
     "|-globals: r1: {kind: `globals', parent: r0, sval: null, map: {`p': r2}}\n"
     "|  `-`p': r2: {kind: `primitive', parent: r1, sval: sv3, type: `void *'}\n"
     "|    |: sval: sv3: {type: `void *', &r4}\n"
     "|    |: type: `void *'\n"
     "`-r3: {kind: `array', parent: r0, sval: sv0, type: `const char[4]', array: {[0]: r4}}\n"
     "  |: sval: sv0: {type: `const char[4]', `\"foo\"'}\n"
     "  |: type: `const char[4]'\n"
     "  `-[0]: r4: {kind: `primitive', parent: r3, sval: null, type: `const char'}\n"
     "    |: type: `const char'\n"
     "svalues:\n"
     "  sv0: {type: `const char[4]', `\"foo\"'}\n"
     "  sv1: {type: `int', `0'}\n"
     "  sv2: {type: `const char *', &r4}\n"
     "  sv3: {type: `void *', &r4}\n"
     "constraint manager:\n"
     "  equiv classes:\n"
     "  constraints:\n");

  ASSERT_DUMP_EQ (s, ext_state, true,
		  "rmodel: p: &\"foo\"[0]");
}

/* Verify that program_states with identical sm-state can be merged,
   and that the merged program_state preserves the sm-state.  */

static void
test_program_state_merging ()
{
  /* Create a program_state for a global ptr "p" that has
     malloc sm-state, pointing to a region on the heap.  */
  tree p = build_global_decl ("p", ptr_type_node);

  auto_delete_vec <state_machine> checkers;
  checkers.safe_push (make_malloc_state_machine (NULL));
  extrinsic_state ext_state (checkers);

  program_state s0 (ext_state);
  impl_region_model_context ctxt (&s0, NULL, ext_state);

  region_model *model0 = s0.m_region_model;
  region_id new_rid = model0->add_new_malloc_region ();
  svalue_id ptr_sid
      = model0->get_or_create_ptr_svalue (ptr_type_node, new_rid);
  model0->set_value (model0->get_lvalue (p, &ctxt),
		     ptr_sid, &ctxt);
  sm_state_map *smap = s0.m_checker_states[0];
  const state_machine::state_t TEST_STATE = 3;
  smap->impl_set_state (ptr_sid, TEST_STATE, svalue_id::null ());
  ASSERT_EQ (smap->get_state (ptr_sid), TEST_STATE);

  model0->canonicalize (&ctxt);

  /* Verify that canonicalization preserves sm-state.  */
  ASSERT_EQ (smap->get_state (model0->get_rvalue (p, NULL)), TEST_STATE);

  /* Make a copy of the program_state.  */
  program_state s1 (s0);
  ASSERT_EQ (s0, s1);

  /* We have two identical states with "p" pointing to a heap region
     with the given sm-state.
     They ought to be mergeable, preserving the sm-state.  */
  program_state merged (ext_state);
  ASSERT_TRUE (s0.can_merge_with_p (s1, ext_state, &merged));
  merged.validate (ext_state);

  /* Verify that the merged state has the sm-state for "p".  */
  region_model *merged_model = merged.m_region_model;
  sm_state_map *merged_smap = merged.m_checker_states[0];
  ASSERT_EQ (merged_smap->get_state (merged_model->get_rvalue (p, NULL)),
	     TEST_STATE);

  /* Try canonicalizing.  */
  impl_region_model_context merged_ctxt (&merged, NULL, ext_state);
  merged.m_region_model->canonicalize (&merged_ctxt);
  merged.validate (ext_state);

  /* Verify that the merged state still has the sm-state for "p".  */
  ASSERT_EQ (merged_smap->get_state (merged_model->get_rvalue (p, NULL)),
	     TEST_STATE);

  /* After canonicalization, we ought to have equality with the inputs.  */
  ASSERT_EQ (s0, merged);
}

/* Verify that program_states with different global-state in an sm-state
   can't be merged.  */

static void
test_program_state_merging_2 ()
{
  auto_delete_vec <state_machine> checkers;
  checkers.safe_push (make_signal_state_machine (NULL));
  extrinsic_state ext_state (checkers);

  program_state s0 (ext_state);
  {
    sm_state_map *smap0 = s0.m_checker_states[0];
    const state_machine::state_t TEST_STATE_0 = 0;
    smap0->set_global_state (TEST_STATE_0);
    ASSERT_EQ (smap0->get_global_state (), TEST_STATE_0);
  }

  program_state s1 (ext_state);
  {
    sm_state_map *smap1 = s1.m_checker_states[0];
    const state_machine::state_t TEST_STATE_1 = 1;
    smap1->set_global_state (TEST_STATE_1);
    ASSERT_EQ (smap1->get_global_state (), TEST_STATE_1);
  }

  ASSERT_NE (s0, s1);

  /* They ought to not be mergeable.  */
  program_state merged (ext_state);
  ASSERT_FALSE (s0.can_merge_with_p (s1, ext_state, &merged));
}

/* Run all of the selftests within this file.  */

void
analyzer_program_state_cc_tests ()
{
  test_sm_state_map ();
  test_program_state_dumping ();
  test_program_state_dumping_2 ();
  test_program_state_merging ();
  test_program_state_merging_2 ();
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
