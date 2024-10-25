/* Classes for representing the state of interest at a given path of analysis.
   Copyright (C) 2019-2024 Free Software Foundation, Inc.
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
#define INCLUDE_MEMORY
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "diagnostic-core.h"
#include "diagnostic.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/sm.h"
#include "sbitmap.h"
#include "bitmap.h"
#include "ordered-hash-map.h"
#include "selftest.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"
#include "analyzer/program-state.h"
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
#include "analyzer/program-state.h"
#include "analyzer/exploded-graph.h"
#include "analyzer/state-purge.h"
#include "analyzer/call-summary.h"
#include "analyzer/analyzer-selftests.h"
#include "text-art/tree-widget.h"
#include "text-art/dump.h"

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
  tree_dump_pretty_printer pp (outf);
  dump_to_pp (&pp);
}

/* Dump a multiline representation of this state to stderr.  */

DEBUG_FUNCTION void
extrinsic_state::dump () const
{
  dump_to_file (stderr);
}

/* Return a new json::object of the form
   {"checkers"  : array of objects, one for each state_machine}.  */

json::object *
extrinsic_state::to_json () const
{
  json::object *ext_state_obj = new json::object ();

  {
    json::array *checkers_arr = new json::array ();
    unsigned i;
    state_machine *sm;
    FOR_EACH_VEC_ELT (m_checkers, i, sm)
      checkers_arr->append (sm->to_json ());
    ext_state_obj->set ("checkers", checkers_arr);
  }

  return ext_state_obj;
}

/* Get the region_model_manager for this extrinsic_state.  */

region_model_manager *
extrinsic_state::get_model_manager () const
{
  if (m_engine)
    return m_engine->get_model_manager ();
  else
    return NULL; /* for selftests.  */
}

/* Try to find a state machine named NAME.
   If found, return true and write its index to *OUT.
   Otherwise return false.  */

bool
extrinsic_state::get_sm_idx_by_name (const char *name, unsigned *out) const
{
  unsigned i;
  state_machine *sm;
  FOR_EACH_VEC_ELT (m_checkers, i, sm)
    if (0 == strcmp (name, sm->get_name ()))
      {
	/* Found NAME.  */
	*out = i;
	return true;
      }

  /* NAME not found.  */
  return false;
}

/* struct sm_state_map::entry_t.  */

int
sm_state_map::entry_t::cmp (const entry_t &entry_a, const entry_t &entry_b)
{
  gcc_assert (entry_a.m_state);
  gcc_assert (entry_b.m_state);
  if (int cmp_state = ((int)entry_a.m_state->get_id ()
		       - (int)entry_b.m_state->get_id ()))
    return cmp_state;
  if (entry_a.m_origin && entry_b.m_origin)
    return svalue::cmp_ptr (entry_a.m_origin, entry_b.m_origin);
  if (entry_a.m_origin)
    return 1;
  if (entry_b.m_origin)
    return -1;
  return 0;
}

/* class sm_state_map.  */

/* sm_state_map's ctor.  */

sm_state_map::sm_state_map (const state_machine &sm)
: m_sm (sm), m_map (), m_global_state (sm.get_start_state ())
{
}

/* Clone the sm_state_map.  */

sm_state_map *
sm_state_map::clone () const
{
  return new sm_state_map (*this);
}

/* Print this sm_state_map to PP.
   If MODEL is non-NULL, print representative tree values where
   available.  */

void
sm_state_map::print (const region_model *model,
		      bool simple, bool multiline,
		      pretty_printer *pp) const
{
  bool first = true;
  if (!multiline)
    pp_string (pp, "{");
  if (m_global_state != m_sm.get_start_state ())
    {
      if (multiline)
	pp_string (pp, "  ");
      pp_string (pp, "global: ");
      m_global_state->dump_to_pp (pp);
      if (multiline)
	pp_newline (pp);
      first = false;
    }
  auto_vec <const svalue *> keys (m_map.elements ());
  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end ();
       ++iter)
    keys.quick_push ((*iter).first);
  keys.qsort (svalue::cmp_ptr_ptr);
  unsigned i;
  const svalue *sval;
  FOR_EACH_VEC_ELT (keys, i, sval)
    {
      if (multiline)
	pp_string (pp, "  ");
      else if (!first)
	pp_string (pp, ", ");
      first = false;
      if (!flag_dump_noaddr)
	{
	  pp_pointer (pp, sval);
	  pp_string (pp, ": ");
	}
      sval->dump_to_pp (pp, simple);

      entry_t e = *const_cast <map_t &> (m_map).get (sval);
      pp_string (pp, ": ");
      e.m_state->dump_to_pp (pp);
      if (model)
	if (tree rep = model->get_representative_tree (sval))
	  {
	    pp_string (pp, " (");
	    dump_quoted_tree (pp, rep);
	    pp_character (pp, ')');
	  }
      if (e.m_origin)
	{
	  pp_string (pp, " (origin: ");
	  if (!flag_dump_noaddr)
	    {
	      pp_pointer (pp, e.m_origin);
	      pp_string (pp, ": ");
	    }
	  e.m_origin->dump_to_pp (pp, simple);
	  if (model)
	    if (tree rep = model->get_representative_tree (e.m_origin))
	      {
		pp_string (pp, " (");
		dump_quoted_tree (pp, rep);
		pp_character (pp, ')');
	      }
	  pp_string (pp, ")");
	}
      if (multiline)
	pp_newline (pp);
    }
  if (!multiline)
    pp_string (pp, "}");
}

/* Dump this object to stderr.  */

DEBUG_FUNCTION void
sm_state_map::dump (bool simple) const
{
  tree_dump_pretty_printer pp (stderr);
  print (NULL, simple, true, &pp);
  pp_newline (&pp);
}

/* Return a new json::object of the form
   {"global"  : (optional) value for global state,
    SVAL_DESC : value for state}.  */

json::object *
sm_state_map::to_json () const
{
  json::object *map_obj = new json::object ();

  if (m_global_state != m_sm.get_start_state ())
    map_obj->set ("global", m_global_state->to_json ());
  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end ();
       ++iter)
    {
      const svalue *sval = (*iter).first;
      entry_t e = (*iter).second;

      label_text sval_desc = sval->get_desc ();
      map_obj->set (sval_desc.get (), e.m_state->to_json ());

      /* This doesn't yet JSONify e.m_origin.  */
    }
  return map_obj;
}

/* Make a text_art::tree_widget describing this sm_state_map,
   using MODEL if non-null to describe svalues.  */

std::unique_ptr<text_art::tree_widget>
sm_state_map::make_dump_widget (const text_art::dump_widget_info &dwi,
				const region_model *model) const
{
  using text_art::styled_string;
  using text_art::tree_widget;
  std::unique_ptr<tree_widget> state_widget
    (tree_widget::from_fmt (dwi, nullptr,
			    "%qs state machine", m_sm.get_name ()));

  if (m_global_state != m_sm.get_start_state ())
    {
      pretty_printer the_pp;
      pretty_printer * const pp = &the_pp;
      pp_format_decoder (pp) = default_tree_printer;
      pp_string (pp, "Global State: ");
      m_global_state->dump_to_pp (pp);
      state_widget->add_child (tree_widget::make (dwi, pp));
    }

  auto_vec <const svalue *> keys (m_map.elements ());
  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end ();
       ++iter)
    keys.quick_push ((*iter).first);
  keys.qsort (svalue::cmp_ptr_ptr);
  unsigned i;
  const svalue *sval;
  FOR_EACH_VEC_ELT (keys, i, sval)
    {
      pretty_printer the_pp;
      pretty_printer * const pp = &the_pp;
      const bool simple = true;
      pp_format_decoder (pp) = default_tree_printer;
      if (!flag_dump_noaddr)
	{
	  pp_pointer (pp, sval);
	  pp_string (pp, ": ");
	}
      sval->dump_to_pp (pp, simple);

      entry_t e = *const_cast <map_t &> (m_map).get (sval);
      pp_string (pp, ": ");
      e.m_state->dump_to_pp (pp);
      if (model)
	if (tree rep = model->get_representative_tree (sval))
	  {
	    pp_string (pp, " (");
	    dump_quoted_tree (pp, rep);
	    pp_character (pp, ')');
	  }
      if (e.m_origin)
	{
	  pp_string (pp, " (origin: ");
	  if (!flag_dump_noaddr)
	    {
	      pp_pointer (pp, e.m_origin);
	      pp_string (pp, ": ");
	    }
	  e.m_origin->dump_to_pp (pp, simple);
	  if (model)
	    if (tree rep = model->get_representative_tree (e.m_origin))
	      {
		pp_string (pp, " (");
		dump_quoted_tree (pp, rep);
		pp_character (pp, ')');
	      }
	  pp_string (pp, ")");
	}

      state_widget->add_child (tree_widget::make (dwi, pp));
    }

  return state_widget;
}

/* Return true if no states have been set within this map
   (all expressions are for the start state).  */

bool
sm_state_map::is_empty_p () const
{
  return m_map.elements () == 0 && m_global_state == m_sm.get_start_state ();
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
      hstate.add_ptr ((*iter).first);
      entry_t e = (*iter).second;
      hstate.add_int (e.m_state->get_id ());
      hstate.add_ptr (e.m_origin);
      result ^= hstate.end ();
    }
  result ^= m_global_state->get_id ();

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
      const svalue *sval = (*iter).first;
      entry_t e = (*iter).second;
      entry_t *other_slot = const_cast <map_t &> (other.m_map).get (sval);
      if (other_slot == NULL)
	return false;
      if (e != *other_slot)
	return false;
    }

  gcc_checking_assert (hash () == other.hash ());

  return true;
}

/* Get the state of SVAL within this object.
   States default to the start state.  */

state_machine::state_t
sm_state_map::get_state (const svalue *sval,
			  const extrinsic_state &ext_state) const
{
  gcc_assert (sval);

  sval = canonicalize_svalue (sval, ext_state);

  if (entry_t *slot
      = const_cast <map_t &> (m_map).get (sval))
    return slot->m_state;

  /* SVAL has no explicit sm-state.
     If this sm allows for state inheritance, then SVAL might have implicit
     sm-state inherited via a parent.
     For example INIT_VAL(foo.field) might inherit taintedness state from
     INIT_VAL(foo).  */
  if (m_sm.inherited_state_p ())
    if (region_model_manager *mgr = ext_state.get_model_manager ())
      {
	if (const initial_svalue *init_sval = sval->dyn_cast_initial_svalue ())
	  {
	    const region *reg = init_sval->get_region ();
	    /* Try recursing upwards (up to the base region for the
	       cluster).  */
	    if (!reg->base_region_p ())
	      if (const region *parent_reg = reg->get_parent_region ())
		{
		  const svalue *parent_init_sval
		    = mgr->get_or_create_initial_value (parent_reg);
		  state_machine::state_t parent_state
		    = get_state (parent_init_sval, ext_state);
		  if (parent_state)
		    return parent_state;
		}
	  }
	else if (const sub_svalue *sub_sval = sval->dyn_cast_sub_svalue ())
	  {
	    const svalue *parent_sval = sub_sval->get_parent ();
	    if (state_machine::state_t parent_state
		  = get_state (parent_sval, ext_state))
	      return parent_state;
	  }
      }

  if (state_machine::state_t state
      = m_sm.alt_get_inherited_state (*this, sval, ext_state))
    return state;

  return m_sm.get_default_state (sval);
}

/* Get the "origin" svalue for any state of SVAL.  */

const svalue *
sm_state_map::get_origin (const svalue *sval,
			   const extrinsic_state &ext_state) const
{
  gcc_assert (sval);

  sval = canonicalize_svalue (sval, ext_state);

  entry_t *slot
    = const_cast <map_t &> (m_map).get (sval);
  if (slot)
    return slot->m_origin;
  else
    return NULL;
}

/* Set the state of SID within MODEL to STATE, recording that
   the state came from ORIGIN.  */

void
sm_state_map::set_state (region_model *model,
			 const svalue *sval,
			 state_machine::state_t state,
			 const svalue *origin,
			 const extrinsic_state &ext_state)
{
  if (model == NULL)
    return;

  /* Reject attempts to set state on UNKNOWN/POISONED.  */
  if (!sval->can_have_associated_state_p ())
    return;

  equiv_class &ec = model->get_constraints ()->get_equiv_class (sval);
  if (!set_state (ec, state, origin, ext_state))
    return;
}

/* Set the state of EC to STATE, recording that the state came from
   ORIGIN.
   Return true if any states of svalue_ids within EC changed.  */

bool
sm_state_map::set_state (const equiv_class &ec,
			 state_machine::state_t state,
			 const svalue *origin,
			 const extrinsic_state &ext_state)
{
  bool any_changed = false;
  for (const svalue *sval : ec.m_vars)
    any_changed |= impl_set_state (sval, state, origin, ext_state);
  return any_changed;
}

/* Set state of SVAL to STATE, bypassing equivalence classes.
   Return true if the state changed.  */

bool
sm_state_map::impl_set_state (const svalue *sval,
			      state_machine::state_t state,
			      const svalue *origin,
			      const extrinsic_state &ext_state)
{
  sval = canonicalize_svalue (sval, ext_state);

  if (get_state (sval, ext_state) == state)
    return false;

  gcc_assert (sval->can_have_associated_state_p ());

  if (m_sm.inherited_state_p ())
    {
      if (const compound_svalue *compound_sval
	    = sval->dyn_cast_compound_svalue ())
	for (auto iter : *compound_sval)
	  {
	    const svalue *inner_sval = iter.second;
	    if (inner_sval->can_have_associated_state_p ())
	      impl_set_state (inner_sval, state, origin, ext_state);
	  }
    }

  /* Special-case state 0 as the default value.  */
  if (state == 0)
    {
      if (m_map.get (sval))
	m_map.remove (sval);
      return true;
    }
  gcc_assert (sval);
  m_map.put (sval, entry_t (state, origin));
  return true;
}

/* Clear any state for SVAL from this state map.  */

void
sm_state_map::clear_any_state (const svalue *sval)
{
  m_map.remove (sval);
}

/* Clear all per-svalue state within this state map.  */

void
sm_state_map::clear_all_per_svalue_state ()
{
  m_map.empty ();
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

/* Purge any state for SVAL.
   If !SM::can_purge_p, then report the state as leaking,
   using CTXT.  */

void
sm_state_map::on_svalue_leak (const svalue *sval,
			      impl_region_model_context *ctxt)
{
  if (state_machine::state_t state = get_state (sval, ctxt->m_ext_state))
    {
      if (m_sm.can_purge_p (state))
	m_map.remove (sval);
      else
	ctxt->on_state_leak (m_sm, sval, state);
    }
}

/* Purge any state for svalues that aren't live with respect to LIVE_SVALUES
   and MODEL.  */

void
sm_state_map::on_liveness_change (const svalue_set &live_svalues,
				  const region_model *model,
				  const extrinsic_state &ext_state,
				  impl_region_model_context *ctxt)
{
  svalue_set svals_to_unset;
  uncertainty_t *uncertainty = ctxt->get_uncertainty ();

  auto_vec<const svalue *> leaked_svals (m_map.elements ());
  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end ();
       ++iter)
    {
      const svalue *iter_sval = (*iter).first;
      if (!iter_sval->live_p (&live_svalues, model))
	{
	  svals_to_unset.add (iter_sval);
	  entry_t e = (*iter).second;
	  if (!m_sm.can_purge_p (e.m_state))
	    leaked_svals.quick_push (iter_sval);
	}
      if (uncertainty)
	if (uncertainty->unknown_sm_state_p (iter_sval))
	  svals_to_unset.add (iter_sval);
    }

  leaked_svals.qsort (svalue::cmp_ptr_ptr);

  unsigned i;
  const svalue *sval;
  FOR_EACH_VEC_ELT (leaked_svals, i, sval)
    {
      entry_t e = *m_map.get (sval);
      ctxt->on_state_leak (m_sm, sval, e.m_state);
    }

  sm_state_map old_sm_map = *this;

  for (svalue_set::iterator iter = svals_to_unset.begin ();
       iter != svals_to_unset.end (); ++iter)
    m_map.remove (*iter);

  /* For state machines like "taint" where states can be
     alt-inherited from other svalues, ensure that state purging doesn't
     make us lose sm-state.

     Consider e.g.:

     make_tainted(foo);
     if (foo.field > 128)
       return;
     arr[foo.field].f1 = v1;

     where the last line is:

     (A): _t1 = foo.field;
     (B): _t2 = _t1 * sizeof(arr[0]);
     (C): [arr + _t2].f1 = val;

     At (A), foo is 'tainted' and foo.field is 'has_ub'.
     After (B), foo.field's value (in _t1) is no longer directly
     within LIVE_SVALUES, so with state purging enabled, we would
     erroneously purge the "has_ub" state from the svalue.

     Given that _t2's value's state comes from _t1's value's state,
     we need to preserve that information.

     Hence for all svalues that have had their explicit sm-state unset,
     having their sm-state being unset, determine if doing so has changed
     their effective state, and if so, explicitly set their state.

     For example, in the above, unsetting the "has_ub" for _t1's value means
     that _t2's effective value changes from "has_ub" (from alt-inherited
     from _t1's value) to "tainted" (inherited from "foo"'s value).

     For such cases, preserve the effective state by explicitly setting the
     new state.  In the above example, this means explicitly setting _t2's
     value to the value ("has_ub") it was previously alt-inheriting from _t1's
     value.  */
  if (m_sm.has_alt_get_inherited_state_p ())
    {
      auto_vec<const svalue *> svalues_needing_state;
      for (auto unset_sval : svals_to_unset)
	{
	  const state_machine::state_t old_state
	    = old_sm_map.get_state (unset_sval, ext_state);
	  const state_machine::state_t new_state
	    = get_state (unset_sval, ext_state);
	  if (new_state != old_state)
	    svalues_needing_state.safe_push (unset_sval);
	}
      for (auto sval : svalues_needing_state)
	{
	  const state_machine::state_t old_state
	    = old_sm_map.get_state (sval, ext_state);
	  impl_set_state (sval, old_state, nullptr, ext_state);
	}
    }
}

/* Purge state from SVAL (in response to a call to an unknown function).  */

void
sm_state_map::on_unknown_change (const svalue *sval,
				 bool is_mutable,
				 const extrinsic_state &ext_state)
{
  svalue_set svals_to_unset;

  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end ();
       ++iter)
    {
      const svalue *key = (*iter).first;
      entry_t e = (*iter).second;
      /* We only want to purge state for some states when things
	 are mutable.  For example, in sm-malloc.cc, an on-stack ptr
	 doesn't stop being stack-allocated when passed to an unknown fn.  */
      if (!m_sm.reset_when_passed_to_unknown_fn_p (e.m_state, is_mutable))
	continue;
      if (key == sval)
	svals_to_unset.add (key);
      /* If we have INIT_VAL(BASE_REG), then unset any INIT_VAL(REG)
	 for REG within BASE_REG.  */
      if (const initial_svalue *init_sval = sval->dyn_cast_initial_svalue ())
	if (const initial_svalue *init_key = key->dyn_cast_initial_svalue ())
	  {
	    const region *changed_reg = init_sval->get_region ();
	    const region *changed_key = init_key->get_region ();
	    if (changed_key->get_base_region () == changed_reg)
	      svals_to_unset.add (key);
	  }
    }

  for (svalue_set::iterator iter = svals_to_unset.begin ();
       iter != svals_to_unset.end (); ++iter)
    impl_set_state (*iter, (state_machine::state_t)0, NULL, ext_state);
}

/* Purge state for things involving SVAL.
   For use when SVAL changes meaning, at the def_stmt on an SSA_NAME.   */

void
sm_state_map::purge_state_involving (const svalue *sval,
				     const extrinsic_state &ext_state)
{
  /* Currently svalue::involves_p requires this.  */
  if (!(sval->get_kind () == SK_INITIAL
	|| sval->get_kind () == SK_CONJURED))
    return;

  svalue_set svals_to_unset;

  for (map_t::iterator iter = m_map.begin ();
       iter != m_map.end ();
       ++iter)
    {
      const svalue *key = (*iter).first;
      entry_t e = (*iter).second;
      if (!m_sm.can_purge_p (e.m_state))
	continue;
      if (key->involves_p (sval))
	svals_to_unset.add (key);
    }

  for (svalue_set::iterator iter = svals_to_unset.begin ();
       iter != svals_to_unset.end (); ++iter)
    impl_set_state (*iter, (state_machine::state_t)0, NULL, ext_state);
}

/* Comparator for imposing an order on sm_state_map instances.  */

int
sm_state_map::cmp (const sm_state_map &smap_a, const sm_state_map &smap_b)
{
  if (int cmp_count = smap_a.elements () - smap_b.elements ())
    return cmp_count;

  auto_vec <const svalue *> keys_a (smap_a.elements ());
  for (map_t::iterator iter = smap_a.begin ();
       iter != smap_a.end ();
       ++iter)
    keys_a.quick_push ((*iter).first);
  keys_a.qsort (svalue::cmp_ptr_ptr);

  auto_vec <const svalue *> keys_b (smap_b.elements ());
  for (map_t::iterator iter = smap_b.begin ();
       iter != smap_b.end ();
       ++iter)
    keys_b.quick_push ((*iter).first);
  keys_b.qsort (svalue::cmp_ptr_ptr);

  unsigned i;
  const svalue *sval_a;
  FOR_EACH_VEC_ELT (keys_a, i, sval_a)
    {
      const svalue *sval_b = keys_b[i];
      if (int cmp_sval = svalue::cmp_ptr (sval_a, sval_b))
	return cmp_sval;
      const entry_t *e_a = const_cast <map_t &> (smap_a.m_map).get (sval_a);
      const entry_t *e_b = const_cast <map_t &> (smap_b.m_map).get (sval_b);
      if (int cmp_entry = entry_t::cmp (*e_a, *e_b))
	return cmp_entry;
    }

  return 0;
}

/* Canonicalize SVAL before getting/setting it within the map.
   Convert all NULL pointers to (void *) to avoid state explosions
   involving all of the various (foo *)NULL vs (bar *)NULL.  */

const svalue *
sm_state_map::canonicalize_svalue (const svalue *sval,
				   const extrinsic_state &ext_state)
{
  region_model_manager *mgr = ext_state.get_model_manager ();
  if (mgr && sval->get_type () && POINTER_TYPE_P (sval->get_type ()))
    if (tree cst = sval->maybe_get_constant ())
      if (zerop (cst))
	return mgr->get_or_create_constant_svalue (null_pointer_node);

  return sval;
}

/* Attempt to merge this state map with OTHER, writing the result
   into *OUT.
   Return true if the merger was possible, false otherwise.

   Normally, only identical state maps can be merged, so that
   differences between state maps lead to different enodes

   However some state machines may support merging states to
   allow for discarding of less important states, and thus avoid
   blow-up of the exploded graph.  */

bool
sm_state_map::can_merge_with_p (const sm_state_map &other,
				const state_machine &sm,
				const extrinsic_state &ext_state,
				sm_state_map **out) const
{
  /* If identical, then they merge trivially, with a copy.  */
  if (*this == other)
    {
      delete *out;
      *out = clone ();
      return true;
    }

  delete *out;
  *out = new sm_state_map (sm);

  /* Otherwise, attempt to merge element by element. */

  /* Try to merge global state.  */
  if (state_machine::state_t merged_global_state
      = sm.maybe_get_merged_state (get_global_state (),
				   other.get_global_state ()))
    (*out)->set_global_state (merged_global_state);
  else
    return false;

  /* Try to merge state each svalue's state (for the union
     of svalues represented by each smap).
     Ignore the origin information.  */
  hash_set<const svalue *> svals;
  for (auto kv : *this)
    svals.add (kv.first);
  for (auto kv : other)
    svals.add (kv.first);
  for (auto sval : svals)
    {
      state_machine::state_t this_state = get_state (sval, ext_state);
      state_machine::state_t other_state = other.get_state (sval, ext_state);
      if (state_machine::state_t merged_state
	    = sm.maybe_get_merged_state (this_state, other_state))
	(*out)->impl_set_state (sval, merged_state, NULL, ext_state);
      else
	return false;
    }

  /* Successfully merged all elements.  */
  return true;
}

/* class program_state.  */

/* program_state's ctor.  */

program_state::program_state (const extrinsic_state &ext_state)
: m_region_model (NULL),
  m_checker_states (ext_state.get_num_checkers ()),
  m_valid (true)
{
  engine *eng = ext_state.get_engine ();
  region_model_manager *mgr = eng->get_model_manager ();
  m_region_model = new region_model (mgr);
  const int num_states = ext_state.get_num_checkers ();
  for (int i = 0; i < num_states; i++)
    {
      sm_state_map *sm = new sm_state_map (ext_state.get_sm (i));
      m_checker_states.quick_push (sm);
    }
}

/* Attempt to use R to replay SUMMARY into this object.
   Return true if it is possible.  */

bool
sm_state_map::replay_call_summary (call_summary_replay &r,
				   const sm_state_map &summary)
{
  for (auto kv : summary.m_map)
    {
      const svalue *summary_sval = kv.first;
      const svalue *caller_sval = r.convert_svalue_from_summary (summary_sval);
      if (!caller_sval)
	continue;
      if (!caller_sval->can_have_associated_state_p ())
	continue;
      const svalue *summary_origin = kv.second.m_origin;
      const svalue *caller_origin
	= (summary_origin
	   ? r.convert_svalue_from_summary (summary_origin)
	   : NULL);
      // caller_origin can be NULL.
      m_map.put (caller_sval, entry_t (kv.second.m_state, caller_origin));
    }
  m_global_state = summary.m_global_state;
  return true;
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
  m_region_model->dump_to_pp (pp, true, false);
  pp_newline (pp);

  int i;
  sm_state_map *smap;
  FOR_EACH_VEC_ELT (m_checker_states, i, smap)
    {
      if (!smap->is_empty_p ())
	{
	  pp_printf (pp, "%s: ", ext_state.get_name (i));
	  smap->print (m_region_model, true, false, pp);
	  pp_newline (pp);
	}
    }
  if (!m_valid)
    {
      pp_printf (pp, "invalid state");
      pp_newline (pp);
    }
}

/* Dump a representation of this state to PP.  */

void
program_state::dump_to_pp (const extrinsic_state &ext_state,
			   bool /*summarize*/, bool multiline,
			   pretty_printer *pp) const
{
  if (!multiline)
    pp_string (pp, "{");
  {
    pp_printf (pp, "rmodel:");
    if (multiline)
      pp_newline (pp);
    else
      pp_string (pp, " {");
    m_region_model->dump_to_pp (pp, true, multiline);
    if (!multiline)
      pp_string (pp, "}");
  }

  int i;
  sm_state_map *smap;
  FOR_EACH_VEC_ELT (m_checker_states, i, smap)
    {
      if (!smap->is_empty_p ())
	{
	  if (!multiline)
	    pp_string (pp, " {");
	  pp_printf (pp, "%s: ", ext_state.get_name (i));
	  if (multiline)
	    pp_newline (pp);
	  smap->print (m_region_model, true, multiline, pp);
	  if (!multiline)
	    pp_string (pp, "}");
	}
    }

  if (!m_valid)
    {
      if (!multiline)
	pp_space (pp);
      pp_printf (pp, "invalid state");
      if (multiline)
	pp_newline (pp);
    }
  if (!multiline)
    pp_string (pp, "}");
}

/* Dump a representation of this state to OUTF.  */

void
program_state::dump_to_file (const extrinsic_state &ext_state,
			     bool summarize, bool multiline,
			     FILE *outf) const
{
  tree_dump_pretty_printer pp (outf);
  dump_to_pp (ext_state, summarize, multiline, &pp);
}

/* Dump a multiline representation of this state to stderr.  */

DEBUG_FUNCTION void
program_state::dump (const extrinsic_state &ext_state,
		     bool summarize) const
{
  dump_to_file (ext_state, summarize, true, stderr);
}

/* Dump a tree-like representation of this state to stderr.  */

DEBUG_FUNCTION void
program_state::dump () const
{
  text_art::dump (*this);
}

/* Return a new json::object of the form
   {"store"  : object for store,
    "constraints" : object for constraint_manager,
    "curr_frame" : (optional) str for current frame,
    "checkers" : { STATE_NAME : object per sm_state_map },
    "valid" : true/false}.  */

json::object *
program_state::to_json (const extrinsic_state &ext_state) const
{
  json::object *state_obj = new json::object ();

  state_obj->set ("store", m_region_model->get_store ()->to_json ());
  state_obj->set ("constraints",
		  m_region_model->get_constraints ()->to_json ());
  if (m_region_model->get_current_frame ())
    state_obj->set ("curr_frame",
		    m_region_model->get_current_frame ()->to_json ());

  /* Provide m_checker_states as an object, using names as keys.  */
  {
    json::object *checkers_obj = new json::object ();

    int i;
    sm_state_map *smap;
    FOR_EACH_VEC_ELT (m_checker_states, i, smap)
      if (!smap->is_empty_p ())
	checkers_obj->set (ext_state.get_name (i), smap->to_json ());

    state_obj->set ("checkers", checkers_obj);
  }

  state_obj->set_bool ("valid", m_valid);

  return state_obj;
}


std::unique_ptr<text_art::tree_widget>
program_state::make_dump_widget (const text_art::dump_widget_info &dwi) const
{
  using text_art::tree_widget;
  std::unique_ptr<tree_widget> state_widget
    (tree_widget::from_fmt (dwi, nullptr, "State"));

  state_widget->add_child (m_region_model->make_dump_widget (dwi));

  /* Add nodes for any sm_state_maps with state.  */
  {
    int i;
    sm_state_map *smap;
    FOR_EACH_VEC_ELT (m_checker_states, i, smap)
      if (!smap->is_empty_p ())
	state_widget->add_child (smap->make_dump_widget (dwi, m_region_model));
  }

  return state_widget;
}

/* Update this program_state to reflect a top-level call to FUN.
   The params will have initial_svalues.  */

void
program_state::push_frame (const extrinsic_state &ext_state ATTRIBUTE_UNUSED,
			   const function &fun)
{
  m_region_model->push_frame (fun, NULL, NULL);
}

/* Get the current function of this state.  */

const function *
program_state::get_current_function () const
{
  return m_region_model->get_current_function ();
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
			exploded_node *enode,
			const superedge *succ,
			uncertainty_t *uncertainty)
{
  class my_path_context : public path_context
  {
  public:
    my_path_context (bool &terminated) : m_terminated (terminated) {}
    void bifurcate (std::unique_ptr<custom_edge_info>) final override
    {
      gcc_unreachable ();
    }

    void terminate_path () final override
    {
      m_terminated = true;
    }

    bool terminate_path_p () const final override
    {
      return m_terminated;
    }
    bool &m_terminated;
  };

  /* Update state.  */
  const program_point &point = enode->get_point ();
  const gimple *last_stmt = point.get_supernode ()->get_last_stmt ();

  /* For conditionals and switch statements, add the
     relevant conditions (for the specific edge) to new_state;
     skip edges for which the resulting constraints
     are impossible.
     This also updates frame information for call/return superedges.
     Adding the relevant conditions for the edge could also trigger
     sm-state transitions (e.g. transitions due to ptrs becoming known
     to be NULL or non-NULL) */
  bool terminated = false;
  my_path_context path_ctxt (terminated);
  impl_region_model_context ctxt (eg, enode,
				  &enode->get_state (),
				  this,
				  uncertainty, &path_ctxt,
				  last_stmt);
  std::unique_ptr<rejected_constraint> rc;
  logger * const logger = eg.get_logger ();
  if (!m_region_model->maybe_update_for_edge (*succ,
					      last_stmt,
					      &ctxt,
					      logger ? &rc : nullptr))
    {
      if (logger)
	{
	  logger->start_log_line ();
	  logger->log_partial ("edge to SN: %i is impossible"
			       " due to region_model constraint: ",
			       succ->m_dest->m_index);
	  rc->dump_to_pp (logger->get_printer ());
	  logger->end_log_line ();
	}
      return false;
    }
  if (terminated)
    return false;

  program_state::detect_leaks (enode->get_state (), *this,
			       NULL, eg.get_ext_state (),
			       &ctxt);

  return true;
}

/* Update this program_state to reflect a call to function
   represented by CALL_STMT.
   currently used only when the call doesn't have a superedge representing
   the call ( like call via a function pointer )  */
void
program_state::push_call (exploded_graph &eg,
                          exploded_node *enode,
                          const gcall *call_stmt,
                          uncertainty_t *uncertainty)
{
  /* Update state.  */
  const program_point &point = enode->get_point ();
  const gimple *last_stmt = point.get_supernode ()->get_last_stmt ();

  impl_region_model_context ctxt (eg, enode,
                                  &enode->get_state (),
                                  this,
                                  uncertainty,
				  NULL,
                                  last_stmt);
  m_region_model->update_for_gcall (call_stmt, &ctxt);
}

/* Update this program_state to reflect a return from function
   call to which is represented by CALL_STMT.
   currently used only when the call doesn't have a superedge representing
   the return */
void
program_state::returning_call (exploded_graph &eg,
                               exploded_node *enode,
                               const gcall *call_stmt,
                               uncertainty_t *uncertainty)
{
  /* Update state.  */
  const program_point &point = enode->get_point ();
  const gimple *last_stmt = point.get_supernode ()->get_last_stmt ();

  impl_region_model_context ctxt (eg, enode,
                                  &enode->get_state (),
                                  this,
                                  uncertainty,
				  NULL,
                                  last_stmt);
  m_region_model->update_for_return_gcall (call_stmt, &ctxt);
}

/* Generate a simpler version of THIS, discarding state that's no longer
   relevant at POINT.
   The idea is that we're more likely to be able to consolidate
   multiple (point, state) into single exploded_nodes if we discard
   irrelevant state (e.g. at the end of functions).  */

program_state
program_state::prune_for_point (exploded_graph &eg,
				const program_point &point,
				exploded_node *enode_for_diag,
				uncertainty_t *uncertainty) const
{
  logger * const logger = eg.get_logger ();
  LOG_SCOPE (logger);

  function *fun = point.get_function ();
  if (!fun)
    return *this;

  program_state new_state (*this);

  const state_purge_map *pm = eg.get_purge_map ();
  if (pm)
    {
      unsigned num_ssas_purged = 0;
      unsigned num_decls_purged = 0;
      auto_vec<const decl_region *> regs;
      new_state.m_region_model->get_regions_for_current_frame (&regs);
      regs.qsort (region::cmp_ptr_ptr);
      unsigned i;
      const decl_region *reg;
      FOR_EACH_VEC_ELT (regs, i, reg)
	{
	  const tree node = reg->get_decl ();
	  if (TREE_CODE (node) == SSA_NAME)
	    {
	      const tree ssa_name = node;
	      const state_purge_per_ssa_name &per_ssa
		= pm->get_data_for_ssa_name (node);
	      if (!per_ssa.needed_at_point_p (point.get_function_point ()))
		{
		  /* Don't purge bindings of SSA names to svalues
		     that have unpurgable sm-state, so that leaks are
		     reported at the end of the function, rather than
		     at the last place that such an SSA name is referred to.

		     But do purge them for temporaries (when SSA_NAME_VAR is
		     NULL), so that we report for cases where a leak happens when
		     a variable is overwritten with another value, so that the leak
		     is reported at the point of overwrite, rather than having
		     temporaries keep the value reachable until the frame is
		     popped.  */
		  const svalue *sval
		    = new_state.m_region_model->get_store_value (reg, NULL);
		  if (!new_state.can_purge_p (eg.get_ext_state (), sval)
		      && SSA_NAME_VAR (ssa_name))
		    {
		      /* (currently only state maps can keep things
			 alive).  */
		      if (logger)
			logger->log ("not purging binding for %qE"
				     " (used by state map)", ssa_name);
		      continue;
		    }

		  new_state.m_region_model->purge_region (reg);
		  num_ssas_purged++;
		}
	    }
	  else
	    {
	      const tree decl = node;
	      gcc_assert (TREE_CODE (node) == VAR_DECL
			  || TREE_CODE (node) == PARM_DECL
			  || TREE_CODE (node) == RESULT_DECL);
	      if (const state_purge_per_decl *per_decl
		  = pm->get_any_data_for_decl (decl))
		if (!per_decl->needed_at_point_p (point.get_function_point ()))
		  {
		    /* Don't purge bindings of decls if there are svalues
		       that have unpurgable sm-state within the decl's cluster,
		       so that leaks are reported at the end of the function,
		       rather than at the last place that such a decl is
		       referred to.  */
		    if (!new_state.can_purge_base_region_p (eg.get_ext_state (),
							    reg))
		      {
			/* (currently only state maps can keep things
			   alive).  */
			if (logger)
			  logger->log ("not purging binding for %qE"
				       " (value in binding used by state map)",
				       decl);
			continue;
		      }

		    new_state.m_region_model->purge_region (reg);
		    num_decls_purged++;
		  }
	    }
	}

      if (num_ssas_purged > 0 || num_decls_purged > 0)
	{
	  if (logger)
	    {
	      logger->log ("num_ssas_purged: %i", num_ssas_purged);
	      logger->log ("num_decl_purged: %i", num_decls_purged);
	    }
	  impl_region_model_context ctxt (eg, enode_for_diag,
					  this,
					  &new_state,
					  uncertainty, NULL,
					  point.get_stmt ());
	  detect_leaks (*this, new_state, NULL, eg.get_ext_state (), &ctxt);
	}
    }

  new_state.m_region_model->canonicalize ();

  return new_state;
}

/* Return true if there are no unpurgeable bindings within BASE_REG. */

bool
program_state::can_purge_base_region_p (const extrinsic_state &ext_state,
					const region *base_reg) const
{
  binding_cluster *cluster
    = m_region_model->get_store ()->get_cluster (base_reg);
  if (!cluster)
    return true;

  for (auto iter : *cluster)
    {
      const svalue *sval = iter.second;
      if (!can_purge_p (ext_state, sval))
	return false;
    }

  return true;
}

/* Get a representative tree to use for describing SVAL.  */

tree
program_state::get_representative_tree (const svalue *sval) const
{
  gcc_assert (m_region_model);
  return m_region_model->get_representative_tree (sval);
}

/* Attempt to merge this state with OTHER, both at POINT.
   Write the result to *OUT.
   If the states were merged successfully, return true.  */

bool
program_state::can_merge_with_p (const program_state &other,
				 const extrinsic_state &ext_state,
				 const program_point &point,
				 program_state *out) const
{
  gcc_assert (out);
  gcc_assert (m_region_model);

  /* Attempt to merge the sm-states.  */
  int i;
  sm_state_map *smap;
  FOR_EACH_VEC_ELT (out->m_checker_states, i, smap)
    if (!m_checker_states[i]->can_merge_with_p (*other.m_checker_states[i],
						ext_state.get_sm (i),
						ext_state,
						&out->m_checker_states[i]))
      return false;

  /* Attempt to merge the region_models.  */
  if (!m_region_model->can_merge_with_p (*other.m_region_model,
					  point,
					 out->m_region_model,
					 &ext_state,
					 this, &other))
    return false;

  out->m_region_model->canonicalize ();

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

  gcc_assert (m_checker_states.length () == ext_state.get_num_checkers ());
  m_region_model->validate ();
}

static void
log_set_of_svalues (logger *logger, const char *name,
		    const svalue_set &set)
{
  logger->log (name);
  logger->inc_indent ();
  auto_vec<const svalue *> sval_vecs (set.elements ());
  for (svalue_set::iterator iter = set.begin ();
       iter != set.end (); ++iter)
    sval_vecs.quick_push (*iter);
  sval_vecs.qsort (svalue::cmp_ptr_ptr);
  unsigned i;
  const svalue *sval;
  FOR_EACH_VEC_ELT (sval_vecs, i, sval)
    {
      logger->start_log_line ();
      pretty_printer *pp = logger->get_printer ();
      if (!flag_dump_noaddr)
	{
	  pp_pointer (pp, sval);
	  pp_string (pp, ": ");
	}
      sval->dump_to_pp (pp, false);
      logger->end_log_line ();
    }
  logger->dec_indent ();
}

/* Compare the sets of svalues reachable from each of SRC_STATE and DEST_STATE.
   For all svalues that are reachable in SRC_STATE and are not live in
   DEST_STATE (whether explicitly reachable in DEST_STATE, or implicitly live
   based on the former set), call CTXT->on_svalue_leak for them.

   Call on_liveness_change on both the CTXT and on the DEST_STATE's
   constraint_manager, purging dead svalues from sm-state and from
   constraints, respectively.

   This function should be called at each fine-grained state change, not
   just at exploded edges.  */

void
program_state::detect_leaks (const program_state &src_state,
			     const program_state &dest_state,
			     const svalue *extra_sval,
			     const extrinsic_state &ext_state,
			     region_model_context *ctxt)
{
  logger *logger = ext_state.get_logger ();
  LOG_SCOPE (logger);
  const uncertainty_t *uncertainty = ctxt->get_uncertainty ();
  if (logger)
    {
      pretty_printer *pp = logger->get_printer ();
      logger->start_log_line ();
      pp_string (pp, "src_state: ");
      src_state.dump_to_pp (ext_state, true, false, pp);
      logger->end_log_line ();
      logger->start_log_line ();
      pp_string (pp, "dest_state: ");
      dest_state.dump_to_pp (ext_state, true, false, pp);
      logger->end_log_line ();
      if (extra_sval)
	{
	  logger->start_log_line ();
	  pp_string (pp, "extra_sval: ");
	  extra_sval->dump_to_pp (pp, true);
	  logger->end_log_line ();
	}
      if (uncertainty)
	{
	  logger->start_log_line ();
	  pp_string (pp, "uncertainty: ");
	  uncertainty->dump_to_pp (pp, true);
	  logger->end_log_line ();
	}
    }

  /* Get svalues reachable from each of src_state and dest_state.
     Get svalues *known* to be reachable in src_state.
     Pass in uncertainty for dest_state so that we additionally get svalues that
     *might* still be reachable in dst_state.  */
  svalue_set known_src_svalues;
  src_state.m_region_model->get_reachable_svalues (&known_src_svalues,
						   NULL, NULL);
  svalue_set maybe_dest_svalues;
  dest_state.m_region_model->get_reachable_svalues (&maybe_dest_svalues,
						    extra_sval, uncertainty);

  if (logger)
    {
      log_set_of_svalues (logger, "src_state known reachable svalues:",
			  known_src_svalues);
      log_set_of_svalues (logger, "dest_state maybe reachable svalues:",
			  maybe_dest_svalues);
    }

  auto_vec <const svalue *> dead_svals (known_src_svalues.elements ());
  for (svalue_set::iterator iter = known_src_svalues.begin ();
       iter != known_src_svalues.end (); ++iter)
    {
      const svalue *sval = (*iter);
      /* For each sval reachable from SRC_STATE, determine if it is
	 live in DEST_STATE: either explicitly reachable, implicitly
	 live based on the set of explicitly reachable svalues,
	 or possibly reachable as recorded in uncertainty.
	 Record those that have ceased to be live i.e. were known
	 to be live, and are now not known to be even possibly-live.  */
      if (!sval->live_p (&maybe_dest_svalues, dest_state.m_region_model))
	dead_svals.quick_push (sval);
    }

  /* Call CTXT->on_svalue_leak on all svals in SRC_STATE  that have ceased
     to be live, sorting them first to ensure deterministic behavior.  */
  dead_svals.qsort (svalue::cmp_ptr_ptr);
  unsigned i;
  const svalue *sval;
  FOR_EACH_VEC_ELT (dead_svals, i, sval)
    ctxt->on_svalue_leak (sval);

  /* Purge dead svals from sm-state.  */
  ctxt->on_liveness_change (maybe_dest_svalues,
			    dest_state.m_region_model);

  /* Purge dead svals from constraints.  */
  dest_state.m_region_model->get_constraints ()->on_liveness_change
    (maybe_dest_svalues, dest_state.m_region_model);

  /* Purge dead heap-allocated regions from dynamic extents.  */
  for (const svalue *sval : dead_svals)
    if (const region *reg = sval->maybe_get_region ())
      if (reg->get_kind () == RK_HEAP_ALLOCATED)
	dest_state.m_region_model->unset_dynamic_extents (reg);
}

/* Attempt to use R to replay SUMMARY into this object.
   Return true if it is possible.  */

bool
program_state::replay_call_summary (call_summary_replay &r,
				    const program_state &summary)
{
  if (!m_region_model->replay_call_summary (r, *summary.m_region_model))
    return false;

  for (unsigned sm_idx = 0; sm_idx < m_checker_states.length (); sm_idx++)
    {
      const sm_state_map *summary_sm_map = summary.m_checker_states[sm_idx];
      m_checker_states[sm_idx]->replay_call_summary (r, *summary_sm_map);
    }

  if (!summary.m_valid)
    m_valid = false;

  return true;
}

/* Handle calls to "__analyzer_dump_state".  */

void
program_state::impl_call_analyzer_dump_state (const gcall *call,
					      const extrinsic_state &ext_state,
					      region_model_context *ctxt)
{
  call_details cd (call, m_region_model, ctxt);
  const char *sm_name = cd.get_arg_string_literal (0);
  if (!sm_name)
    {
      error_at (call->location, "cannot determine state machine");
      return;
    }
  unsigned sm_idx;
  if (!ext_state.get_sm_idx_by_name (sm_name, &sm_idx))
    {
      error_at (call->location, "unrecognized state machine %qs", sm_name);
      return;
    }
  const sm_state_map *smap = m_checker_states[sm_idx];

  const svalue *sval = cd.get_arg_svalue (1);

  /* Strip off cast to int (due to variadic args).  */
  if (const svalue *cast = sval->maybe_undo_cast ())
    sval = cast;

  state_machine::state_t state = smap->get_state (sval, ext_state);
  warning_at (call->location, 0, "state: %qs", state->get_name ());
}

#if CHECKING_P

namespace selftest {

/* Tests for sm_state_map.  */

static void
test_sm_state_map ()
{
  tree x = build_global_decl ("x", integer_type_node);
  tree y = build_global_decl ("y", integer_type_node);
  tree z = build_global_decl ("z", integer_type_node);

  state_machine *sm = make_malloc_state_machine (NULL);
  auto_delete_vec <state_machine> checkers;
  checkers.safe_push (sm);
  engine eng;
  extrinsic_state ext_state (checkers, &eng);
  state_machine::state_t start = sm->get_start_state ();

  /* Test setting states on svalue_id instances directly.  */
  {
    const state_machine::state test_state_42 ("test state 42", 42);
    const state_machine::state_t TEST_STATE_42 = &test_state_42;
    region_model_manager mgr;
    region_model model (&mgr);
    const svalue *x_sval = model.get_rvalue (x, NULL);
    const svalue *y_sval = model.get_rvalue (y, NULL);
    const svalue *z_sval = model.get_rvalue (z, NULL);

    sm_state_map map (*sm);
    ASSERT_TRUE (map.is_empty_p ());
    ASSERT_EQ (map.get_state (x_sval, ext_state), start);

    map.impl_set_state (x_sval, TEST_STATE_42, z_sval, ext_state);
    ASSERT_EQ (map.get_state (x_sval, ext_state), TEST_STATE_42);
    ASSERT_EQ (map.get_origin (x_sval, ext_state), z_sval);
    ASSERT_EQ (map.get_state (y_sval, ext_state), start);
    ASSERT_FALSE (map.is_empty_p ());

    map.impl_set_state (y_sval, 0, z_sval, ext_state);
    ASSERT_EQ (map.get_state (y_sval, ext_state), start);

    map.impl_set_state (x_sval, 0, z_sval, ext_state);
    ASSERT_EQ (map.get_state (x_sval, ext_state), start);
    ASSERT_TRUE (map.is_empty_p ());
  }

  const state_machine::state test_state_5 ("test state 5", 5);
  const state_machine::state_t TEST_STATE_5 = &test_state_5;

  /* Test setting states via equivalence classes.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    const svalue *x_sval = model.get_rvalue (x, NULL);
    const svalue *y_sval = model.get_rvalue (y, NULL);
    const svalue *z_sval = model.get_rvalue (z, NULL);

    sm_state_map map (*sm);
    ASSERT_TRUE (map.is_empty_p ());
    ASSERT_EQ (map.get_state (x_sval, ext_state), start);
    ASSERT_EQ (map.get_state (y_sval, ext_state), start);

    model.add_constraint (x, EQ_EXPR, y, NULL);

    /* Setting x to a state should also update y, as they
       are in the same equivalence class.  */
    map.set_state (&model, x_sval, TEST_STATE_5, z_sval, ext_state);
    ASSERT_EQ (map.get_state (x_sval, ext_state), TEST_STATE_5);
    ASSERT_EQ (map.get_state (y_sval, ext_state), TEST_STATE_5);
    ASSERT_EQ (map.get_origin (x_sval, ext_state), z_sval);
    ASSERT_EQ (map.get_origin (y_sval, ext_state), z_sval);
  }

  /* Test equality and hashing.  */
  {
    region_model_manager mgr;
    region_model model (&mgr);
    const svalue *y_sval = model.get_rvalue (y, NULL);
    const svalue *z_sval = model.get_rvalue (z, NULL);

    sm_state_map map0 (*sm);
    sm_state_map map1 (*sm);
    sm_state_map map2 (*sm);

    ASSERT_EQ (map0.hash (), map1.hash ());
    ASSERT_EQ (map0, map1);

    map1.impl_set_state (y_sval, TEST_STATE_5, z_sval, ext_state);
    ASSERT_NE (map0.hash (), map1.hash ());
    ASSERT_NE (map0, map1);

    /* Make the same change to map2.  */
    map2.impl_set_state (y_sval, TEST_STATE_5, z_sval, ext_state);
    ASSERT_EQ (map1.hash (), map2.hash ());
    ASSERT_EQ (map1, map2);
  }

  /* Equality and hashing shouldn't depend on ordering.  */
  {
    const state_machine::state test_state_2 ("test state 2", 2);
    const state_machine::state_t TEST_STATE_2 = &test_state_2;
    const state_machine::state test_state_3 ("test state 3", 3);
    const state_machine::state_t TEST_STATE_3 = &test_state_3;
    sm_state_map map0 (*sm);
    sm_state_map map1 (*sm);
    sm_state_map map2 (*sm);

    ASSERT_EQ (map0.hash (), map1.hash ());
    ASSERT_EQ (map0, map1);

    region_model_manager mgr;
    region_model model (&mgr);
    const svalue *x_sval = model.get_rvalue (x, NULL);
    const svalue *y_sval = model.get_rvalue (y, NULL);
    const svalue *z_sval = model.get_rvalue (z, NULL);

    map1.impl_set_state (x_sval, TEST_STATE_2, NULL, ext_state);
    map1.impl_set_state (y_sval, TEST_STATE_3, NULL, ext_state);
    map1.impl_set_state (z_sval, TEST_STATE_2, NULL, ext_state);

    map2.impl_set_state (z_sval, TEST_STATE_2, NULL, ext_state);
    map2.impl_set_state (y_sval, TEST_STATE_3, NULL, ext_state);
    map2.impl_set_state (x_sval, TEST_STATE_2, NULL, ext_state);

    ASSERT_EQ (map1.hash (), map2.hash ());
    ASSERT_EQ (map1, map2);
  }

  // TODO: coverage for purging
}

/* Check program_state works as expected.  */

static void
test_program_state_1 ()
{
  /* Create a program_state for a global ptr "p" that has
     malloc sm-state, pointing to a region on the heap.  */
  tree p = build_global_decl ("p", ptr_type_node);

  state_machine *sm = make_malloc_state_machine (NULL);
  const state_machine::state_t UNCHECKED_STATE
    = sm->get_state_by_name ("unchecked");
  auto_delete_vec <state_machine> checkers;
  checkers.safe_push (sm);

  engine eng;
  extrinsic_state ext_state (checkers, &eng);
  region_model_manager *mgr = eng.get_model_manager ();
  program_state s (ext_state);
  region_model *model = s.m_region_model;
  const svalue *size_in_bytes
    = mgr->get_or_create_unknown_svalue (size_type_node);
  const region *new_reg
    = model->get_or_create_region_for_heap_alloc (size_in_bytes, NULL);
  const svalue *ptr_sval = mgr->get_ptr_svalue (ptr_type_node, new_reg);
  model->set_value (model->get_lvalue (p, NULL),
		    ptr_sval, NULL);
  sm_state_map *smap = s.m_checker_states[0];

  smap->impl_set_state (ptr_sval, UNCHECKED_STATE, NULL, ext_state);
  ASSERT_EQ (smap->get_state (ptr_sval, ext_state), UNCHECKED_STATE);
}

/* Check that program_state works for string literals.  */

static void
test_program_state_2 ()
{
  /* Create a program_state for a global ptr "p" that points to
     a string constant.  */
  tree p = build_global_decl ("p", ptr_type_node);

  tree string_cst_ptr = build_string_literal (4, "foo");

  auto_delete_vec <state_machine> checkers;
  engine eng;
  extrinsic_state ext_state (checkers, &eng);

  program_state s (ext_state);
  region_model *model = s.m_region_model;
  const region *p_reg = model->get_lvalue (p, NULL);
  const svalue *str_sval = model->get_rvalue (string_cst_ptr, NULL);
  model->set_value (p_reg, str_sval, NULL);
}

/* Verify that program_states with identical sm-state can be merged,
   and that the merged program_state preserves the sm-state.  */

static void
test_program_state_merging ()
{
  /* Create a program_state for a global ptr "p" that has
     malloc sm-state, pointing to a region on the heap.  */
  tree p = build_global_decl ("p", ptr_type_node);

  engine eng;
  region_model_manager *mgr = eng.get_model_manager ();
  program_point point (program_point::origin (*mgr));
  auto_delete_vec <state_machine> checkers;
  checkers.safe_push (make_malloc_state_machine (NULL));
  extrinsic_state ext_state (checkers, &eng);

  program_state s0 (ext_state);
  uncertainty_t uncertainty;
  impl_region_model_context ctxt (&s0, ext_state, &uncertainty);

  region_model *model0 = s0.m_region_model;
  const svalue *size_in_bytes
    = mgr->get_or_create_unknown_svalue (size_type_node);
  const region *new_reg
    = model0->get_or_create_region_for_heap_alloc (size_in_bytes, NULL);
  const svalue *ptr_sval = mgr->get_ptr_svalue (ptr_type_node, new_reg);
  model0->set_value (model0->get_lvalue (p, &ctxt),
		     ptr_sval, &ctxt);
  sm_state_map *smap = s0.m_checker_states[0];
  const state_machine::state test_state ("test state", 0);
  const state_machine::state_t TEST_STATE = &test_state;
  smap->impl_set_state (ptr_sval, TEST_STATE, NULL, ext_state);
  ASSERT_EQ (smap->get_state (ptr_sval, ext_state), TEST_STATE);

  model0->canonicalize ();

  /* Verify that canonicalization preserves sm-state.  */
  ASSERT_EQ (smap->get_state (model0->get_rvalue (p, NULL), ext_state),
	     TEST_STATE);

  /* Make a copy of the program_state.  */
  program_state s1 (s0);
  ASSERT_EQ (s0, s1);

  /* We have two identical states with "p" pointing to a heap region
     with the given sm-state.
     They ought to be mergeable, preserving the sm-state.  */
  program_state merged (ext_state);
  ASSERT_TRUE (s0.can_merge_with_p (s1, ext_state, point, &merged));
  merged.validate (ext_state);

  /* Verify that the merged state has the sm-state for "p".  */
  region_model *merged_model = merged.m_region_model;
  sm_state_map *merged_smap = merged.m_checker_states[0];
  ASSERT_EQ (merged_smap->get_state (merged_model->get_rvalue (p, NULL),
				     ext_state),
	     TEST_STATE);

  /* Try canonicalizing.  */
  merged.m_region_model->canonicalize ();
  merged.validate (ext_state);

  /* Verify that the merged state still has the sm-state for "p".  */
  ASSERT_EQ (merged_smap->get_state (merged_model->get_rvalue (p, NULL),
				     ext_state),
	     TEST_STATE);

  /* After canonicalization, we ought to have equality with the inputs.  */
  ASSERT_EQ (s0, merged);
}

/* Verify that program_states with different global-state in an sm-state
   can't be merged.  */

static void
test_program_state_merging_2 ()
{
  engine eng;
  region_model_manager *mgr = eng.get_model_manager ();
  program_point point (program_point::origin (*mgr));
  auto_delete_vec <state_machine> checkers;
  checkers.safe_push (make_signal_state_machine (NULL));
  extrinsic_state ext_state (checkers, &eng);

  const state_machine::state test_state_0 ("test state 0", 0);
  const state_machine::state test_state_1 ("test state 1", 1);
  const state_machine::state_t TEST_STATE_0 = &test_state_0;
  const state_machine::state_t TEST_STATE_1 = &test_state_1;

  program_state s0 (ext_state);
  {
    sm_state_map *smap0 = s0.m_checker_states[0];
    smap0->set_global_state (TEST_STATE_0);
    ASSERT_EQ (smap0->get_global_state (), TEST_STATE_0);
  }

  program_state s1 (ext_state);
  {
    sm_state_map *smap1 = s1.m_checker_states[0];
    smap1->set_global_state (TEST_STATE_1);
    ASSERT_EQ (smap1->get_global_state (), TEST_STATE_1);
  }

  ASSERT_NE (s0, s1);

  /* They ought to not be mergeable.  */
  program_state merged (ext_state);
  ASSERT_FALSE (s0.can_merge_with_p (s1, ext_state, point, &merged));
}

/* Run all of the selftests within this file.  */

void
analyzer_program_state_cc_tests ()
{
  test_sm_state_map ();
  test_program_state_1 ();
  test_program_state_2 ();
  test_program_state_merging ();
  test_program_state_merging_2 ();
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
