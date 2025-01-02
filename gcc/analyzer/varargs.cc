/* Implementation of <stdarg.h> within analyzer.
   Copyright (C) 2022-2025 Free Software Foundation, Inc.
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
#include "make-unique.h"
#include "tree.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "diagnostic-core.h"
#include "diagnostic-path.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/sm.h"
#include "analyzer/pending-diagnostic.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"
#include "analyzer/program-state.h"
#include "analyzer/checker-path.h"
#include "analyzer/supergraph.h"
#include "analyzer/diagnostic-manager.h"
#include "analyzer/exploded-graph.h"
#include "analyzer/call-details.h"

#if ENABLE_ANALYZER

namespace ana {

/* Implementation of <stdarg.h> within analyzer.

   Objectives:
   - detection of interprocedural type errors involving va_arg
   - tracking of symbolic values interprocedurally from variadic call
     through to va_arg unpacking
   - detection of missing va_end
   - detection of va_arg outside of a va_start/va_end pair
   - detection of uses of a va_list after the frame in containing the
     va_start has returned

   The analyzer runs *before* the "stdarg" and "lower_vaarg" gimple
   passes, which have target-dependent effects.

   This file implements a state machine on svalues for tracking when
   va_start has been called, so that we can detect missing va_end,
   and misplaced va_arg, etc.
   To do this requires an svalue that can have state, so we implement va_start
   by creating a stack-allocated region, and use a pointer to that region
   as the svalue that has state.

   We call this stack-allocated region the "impl_reg".  Allocating it on
   the stack ensures that it is invalidated when the frame containing
   the va_start returns, leading to
   -Wanalyzer-use-of-pointer-in-stale-stack-frame on attempts to use such
   a va_list.

   To track svalues from variadic calls interprocedurally, we implement
   variadic arguments via new child regions of the callee's frame_region,
   var_arg_region, each one representing a storage slot for one of the
   variadic arguments, accessed by index.

   We have:

    stack frame:
      va_list: &impl_reg
      'impl_reg': pointer to next var_arg_region
      var_arg_region for arg 0
      ...
      var_arg_region for arg N-1

   Hence given test_1 in stdarg-1.c, at the call to:

     __analyzer_called_by_test_1 (int placeholder, ...);

   here:

     __analyzer_called_by_test_1 (42, "foo", 1066, '@');

   we push this frame for the called function:
     clusters within frame: ‘__analyzer_called_by_test_1’@2
       cluster for: placeholder: (int)42
       cluster for: VAR_ARG_REG(frame: ‘__analyzer_called_by_test_1’@2, arg_idx: 0): &"foo" (TOUCHED)
       cluster for: VAR_ARG_REG(frame: ‘__analyzer_called_by_test_1’@2, arg_idx: 1): (int)1066 (TOUCHED)
       cluster for: VAR_ARG_REG(frame: ‘__analyzer_called_by_test_1’@2, arg_idx: 2): (int)64 (TOUCHED)
   where the called function's frame has been populated with both the value
   of the regular argument "placeholder", and with values for 3 variadic
   arguments.

   At the call to
      va_start (ap, placeholder);
   we allocate a region ALLOCA_REGION for ap to point to, populate that
   region with the address of variadic argument 0, and set sm-state of
   &ALLOCA_REGION to "started":
    clusters within frame: ‘__analyzer_called_by_test_1’@2
      cluster for: placeholder: (int)42
      cluster for: VAR_ARG_REG(frame: ‘__analyzer_called_by_test_1’@2, arg_idx: 0): &"foo" (TOUCHED)
      cluster for: VAR_ARG_REG(frame: ‘__analyzer_called_by_test_1’@2, arg_idx: 1): (int)1066 (TOUCHED)
      cluster for: VAR_ARG_REG(frame: ‘__analyzer_called_by_test_1’@2, arg_idx: 2): (int)64 (TOUCHED)
      cluster for: ap: &ALLOCA_REGION
      cluster for: ALLOCA_REGION: &VAR_ARG_REG(frame: ‘__analyzer_called_by_test_1’@2, arg_idx: 0) (TOUCHED)
    va_list:
      0x4c83700: &ALLOCA_REGION: started

   At each call to
     va_arg (ap, TYPE);
   we can look within *ap, locate the region holding the next variadic
   argument to be extracted, extract the svalue, and advance the index
   by effectively updating *ap.

   At the va_end, we can set &ALLOCA_REGION's state to "ended".

   The various __builtin_va_* accept ap by pointer, so we have e.g.:

     __builtin_va_start (&ap, [...]);

   except for the 2nd param of __builtin_va_copy, where the type
   is already target-dependent (see the discussion of get_va_copy_arg
   below).  */

/* Get a tree for diagnostics.
   Typically we have "&ap", but it will make more sense to
   the user as just "ap", so strip off the ADDR_EXPR.  */

static tree
get_va_list_diag_arg (tree va_list_tree)
{
  if (TREE_CODE (va_list_tree) == ADDR_EXPR)
    va_list_tree = TREE_OPERAND (va_list_tree, 0);
  return va_list_tree;
}

/* Get argument ARG_IDX of va_copy.

   builtin-types.def has:
     DEF_PRIMITIVE_TYPE (BT_VALIST_ARG, va_list_arg_type_node)

   and c_common_nodes_and_builtins initializes va_list_arg_type_node
   based on whether TREE_CODE (va_list_type_node) is of ARRAY_TYPE or
   not, giving either one or zero levels of indirection.

   Alternatively we could be dealing with __builtin_ms_va_copy or
   __builtin_sysv_va_copy.

   Handle this by looking at the types of the argument in question.  */

static const svalue *
get_va_copy_arg (const region_model *model,
		 region_model_context *ctxt,
		 const gcall *call,
		 unsigned arg_idx)
{
  tree arg = gimple_call_arg (call, arg_idx);
  const svalue *arg_sval = model->get_rvalue (arg, ctxt);
  if (const svalue *cast = arg_sval->maybe_undo_cast ())
    arg_sval = cast;
  if (TREE_CODE (TREE_TYPE (arg)) == POINTER_TYPE
      && TREE_CODE (TREE_TYPE (TREE_TYPE (arg))) == ARRAY_TYPE)
    {
      /* va_list_arg_type_node is a pointer to a va_list;
	 return *ARG_SVAL.  */
      const region *src_reg = model->deref_rvalue (arg_sval, arg, ctxt);
      const svalue *src_reg_sval = model->get_store_value (src_reg, ctxt);
      if (const svalue *cast = src_reg_sval->maybe_undo_cast ())
	src_reg_sval = cast;
      return src_reg_sval;
    }
  else
    {
      /* va_list_arg_type_node is a va_list; return ARG_SVAL.  */
      return arg_sval;
    }
}

namespace {

/* A state machine for tracking the state of a va_list, so that
   we can enforce that each va_start is paired with a va_end,
   and va_arg only happens within a va_start/va_end pair.
   Specifically, this tracks the state of the &ALLOCA_BUFFER
   that va_start/va_copy allocate.  */

class va_list_state_machine : public state_machine
{
public:
  va_list_state_machine (logger *logger);

  bool inherited_state_p () const final override { return false; }

  bool on_stmt (sm_context &sm_ctxt,
		const supernode *node,
		const gimple *stmt) const final override;

  bool can_purge_p (state_t s) const final override
  {
    return s != m_started;
  }
  std::unique_ptr<pending_diagnostic> on_leak (tree var) const final override;

  /* State for a va_list that is the result of a va_start or va_copy.  */
  state_t m_started;

  /* State for a va_list that has had va_end called on it.  */
  state_t m_ended;

private:
  void on_va_start (sm_context &sm_ctxt, const supernode *node,
		    const gcall *call) const;
  void on_va_copy (sm_context &sm_ctxt, const supernode *node,
		   const gcall *call) const;
  void on_va_arg (sm_context &sm_ctxt, const supernode *node,
		  const gcall *call) const;
  void on_va_end (sm_context &sm_ctxt, const supernode *node,
		  const gcall *call) const;
  void check_for_ended_va_list (sm_context &sm_ctxt,
				const supernode *node,
				const gcall *call,
				const svalue *arg,
				const char *usage_fnname) const;
};

/* va_list_state_machine's ctor.  */

va_list_state_machine::va_list_state_machine (logger *logger)
: state_machine ("va_list", logger),
  m_started (add_state ("started")),
  m_ended (add_state ("ended"))
{
}

/* Implementation of the various "va_*" functions for
   va_list_state_machine.  */

bool
va_list_state_machine::on_stmt (sm_context &sm_ctxt,
				const supernode *node,
				const gimple *stmt) const
{
  if (const gcall *call = dyn_cast <const gcall *> (stmt))
    {
      if (gimple_call_internal_p (call)
	  && gimple_call_internal_fn (call) == IFN_VA_ARG)
	{
	  on_va_arg (sm_ctxt, node, call);
	  return false;
	}

      if (tree callee_fndecl = sm_ctxt.get_fndecl_for_call (call))
	if (fndecl_built_in_p (callee_fndecl, BUILT_IN_NORMAL)
	    && gimple_builtin_call_types_compatible_p (call, callee_fndecl))
	  switch (DECL_UNCHECKED_FUNCTION_CODE (callee_fndecl))
	    {
	    default:
	      break;

	    case BUILT_IN_VA_START:
	      on_va_start (sm_ctxt, node, call);
	      break;

	    case BUILT_IN_VA_COPY:
	      on_va_copy (sm_ctxt, node, call);
	      break;

	    case BUILT_IN_VA_END:
	      on_va_end (sm_ctxt, node, call);
	      break;
	    }
    }
  return false;
}

/* Get the svalue for which va_list_state_machine holds state on argument ARG_
   IDX to CALL.  */

static const svalue *
get_stateful_arg (sm_context &sm_ctxt, const gcall *call, unsigned arg_idx)
{
  tree ap = gimple_call_arg (call, arg_idx);
  if (ap
      && POINTER_TYPE_P (TREE_TYPE (ap)))
    {
      if (const program_state *new_state = sm_ctxt.get_new_program_state ())
	{
	  const region_model *new_model = new_state->m_region_model;
	  const svalue *ptr_sval = new_model->get_rvalue (ap, NULL);
	  const region *reg = new_model->deref_rvalue (ptr_sval, ap, NULL);
	  const svalue *impl_sval = new_model->get_store_value (reg, NULL);
	  if (const svalue *cast = impl_sval->maybe_undo_cast ())
	    impl_sval = cast;
	  return impl_sval;
	}
    }
  return NULL;
}

/* Abstract class for diagnostics relating to va_list_state_machine.  */

class va_list_sm_diagnostic : public pending_diagnostic
{
public:
  bool subclass_equal_p (const pending_diagnostic &base_other) const override
  {
    const va_list_sm_diagnostic &other
      = (const va_list_sm_diagnostic &)base_other;
    return (m_ap_sval == other.m_ap_sval
	    && same_tree_p (m_ap_tree, other.m_ap_tree));
  }

  bool
  describe_state_change (pretty_printer &pp,
			 const evdesc::state_change &change) override
  {
    if (const char *fnname = maybe_get_fnname (change))
      {
	pp_printf (&pp, "%qs called here", fnname);
	return true;
      }
    return false;
  }

  diagnostic_event::meaning
  get_meaning_for_state_change (const evdesc::state_change &change)
    const final override
  {
    if (change.m_new_state == m_sm.m_started)
      return diagnostic_event::meaning (diagnostic_event::VERB_acquire,
					diagnostic_event::NOUN_resource);
    if (change.m_new_state == m_sm.m_ended)
      return diagnostic_event::meaning (diagnostic_event::VERB_release,
					diagnostic_event::NOUN_resource);
    return diagnostic_event::meaning ();
  }

protected:
  va_list_sm_diagnostic (const va_list_state_machine &sm,
			 const svalue *ap_sval, tree ap_tree)
    : m_sm (sm), m_ap_sval (ap_sval), m_ap_tree (ap_tree)
  {}

  static const char *maybe_get_fnname (const evdesc::state_change &change)
  {
    if (change.m_event.m_stmt)
      if (const gcall *call = as_a <const gcall *> (change.m_event.m_stmt))
	if (tree callee_fndecl = gimple_call_fndecl (call))
	  {
	    if (fndecl_built_in_p (callee_fndecl, BUILT_IN_NORMAL))
	      switch (DECL_UNCHECKED_FUNCTION_CODE (callee_fndecl))
		{
		case BUILT_IN_VA_START:
		  return "va_start";
		case BUILT_IN_VA_COPY:
		  return "va_copy";
		case BUILT_IN_VA_END:
		  return "va_end";
		}
	  }
    return NULL;
  }

  const va_list_state_machine &m_sm;
  const svalue *m_ap_sval;
  tree m_ap_tree;
};

/* Concrete class for -Wanalyzer-va-list-use-after-va-end:
   complain about use of a va_list after va_end has been called on it.  */

class va_list_use_after_va_end : public va_list_sm_diagnostic
{
public:
  va_list_use_after_va_end (const va_list_state_machine &sm,
			    const svalue *ap_sval, tree ap_tree,
			    const char *usage_fnname)
  : va_list_sm_diagnostic (sm, ap_sval, ap_tree),
    m_usage_fnname (usage_fnname)
  {
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_va_list_use_after_va_end;
  }

  bool operator== (const va_list_use_after_va_end &other) const
  {
    return (va_list_sm_diagnostic::subclass_equal_p (other)
	    && 0 == strcmp (m_usage_fnname, other.m_usage_fnname));
  }

  bool emit (diagnostic_emission_context &ctxt) final override
  {
    return ctxt.warn ("%qs after %qs", m_usage_fnname, "va_end");
  }

  const char *get_kind () const final override
  {
    return "va_list_use_after_va_end";
  }

  bool
  describe_state_change (pretty_printer &pp,
			 const evdesc::state_change &change) final override
  {
    if (change.m_new_state == m_sm.m_ended)
      m_va_end_event = change.m_event_id;
    return va_list_sm_diagnostic::describe_state_change (pp, change);
  }

  bool
  describe_final_event (pretty_printer &pp,
			const evdesc::final_event &ev) final override
  {
    if (ev.m_expr)
      {
	if (m_va_end_event.known_p ())
	  pp_printf (&pp,
		     "%qs on %qE after %qs at %@",
		     m_usage_fnname, ev.m_expr, "va_end", &m_va_end_event);
	else
	  pp_printf (&pp,
		     "%qs on %qE after %qs",
		     m_usage_fnname, ev.m_expr, "va_end");
      }
    else
      {
	if (m_va_end_event.known_p ())
	  pp_printf (&pp,
		     "%qs after %qs at %@",
		     m_usage_fnname, "va_end", &m_va_end_event);
	else
	  pp_printf (&pp,
		     "%qs after %qs",
		     m_usage_fnname, "va_end");
      }
    return true;
  }

private:
  diagnostic_event_id_t m_va_end_event;
  const char *m_usage_fnname;
};

/* Concrete class for -Wanalyzer-va-list-leak:
   complain about a va_list in the "started" state that doesn't get after
   va_end called on it.  */

class va_list_leak : public va_list_sm_diagnostic
{
public:
  va_list_leak (const va_list_state_machine &sm,
		const svalue *ap_sval, tree ap_tree)
  : va_list_sm_diagnostic (sm, ap_sval, ap_tree),
    m_start_event_fnname (NULL)
  {
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_va_list_leak;
  }

  bool operator== (const va_list_leak &other) const
  {
    return va_list_sm_diagnostic::subclass_equal_p (other);
  }

  bool emit (diagnostic_emission_context &ctxt) final override
  {
    return ctxt.warn ("missing call to %qs", "va_end");
  }

  const char *get_kind () const final override { return "va_list_leak"; }

  bool
  describe_state_change (pretty_printer &pp,
			 const evdesc::state_change &change) final override
  {
    if (change.m_new_state == m_sm.m_started)
      {
	m_start_event = change.m_event_id;
	m_start_event_fnname = maybe_get_fnname (change);
      }
    return va_list_sm_diagnostic::describe_state_change (pp, change);
  }

  bool
  describe_final_event (pretty_printer &pp,
			const evdesc::final_event &ev) final override
  {
    if (ev.m_expr)
      {
	if (m_start_event.known_p () && m_start_event_fnname)
	  pp_printf (&pp,
		     "missing call to %qs on %qE to match %qs at %@",
		     "va_end", ev.m_expr, m_start_event_fnname, &m_start_event);
	else
	  pp_printf (&pp,
		     "missing call to %qs on %qE",
		     "va_end", ev.m_expr);
      }
    else
      {
	if (m_start_event.known_p () && m_start_event_fnname)
	  pp_printf (&pp,
		     "missing call to %qs to match %qs at %@",
		     "va_end", m_start_event_fnname, &m_start_event);
	else
	  pp_printf (&pp,
		     "missing call to %qs",
		     "va_end");
      }
    return true;
  }

private:
  diagnostic_event_id_t m_start_event;
  const char *m_start_event_fnname;
};

/* Update state machine for a "va_start" call.  */

void
va_list_state_machine::on_va_start (sm_context &sm_ctxt,
				    const supernode *,
				    const gcall *call) const
{
  const svalue *arg = get_stateful_arg (sm_ctxt, call, 0);
  if (arg)
    {
      /* Transition from start state to "started".  */
      if (sm_ctxt.get_state (call, arg) == m_start)
	sm_ctxt.set_next_state (call, arg, m_started);
    }
}

/* Complain if ARG is in the "ended" state.  */

void
va_list_state_machine::check_for_ended_va_list (sm_context &sm_ctxt,
						const supernode *node,
						const gcall *call,
						const svalue *arg,
						const char *usage_fnname) const
{
  if (sm_ctxt.get_state (call, arg) == m_ended)
    sm_ctxt.warn (node, call, arg,
		  make_unique<va_list_use_after_va_end>
		    (*this, arg, NULL_TREE, usage_fnname));
}

/* Get the svalue with associated va_list_state_machine state for
   ARG_IDX of CALL to va_copy, if SM_CTXT supports this,
   or NULL otherwise.  */

static const svalue *
get_stateful_va_copy_arg (sm_context &sm_ctxt,
			  const gcall *call,
			  unsigned arg_idx)
{
  if (const program_state *new_state = sm_ctxt.get_new_program_state ())
    {
      const region_model *new_model = new_state->m_region_model;
      const svalue *arg = get_va_copy_arg (new_model, NULL, call, arg_idx);
      return arg;
    }
  return NULL;
}

/* Update state machine for a "va_copy" call.  */

void
va_list_state_machine::on_va_copy (sm_context &sm_ctxt,
				   const supernode *node,
				   const gcall *call) const
{
  const svalue *src_arg = get_stateful_va_copy_arg (sm_ctxt, call, 1);
  if (src_arg)
    check_for_ended_va_list (sm_ctxt, node, call, src_arg, "va_copy");

  const svalue *dst_arg = get_stateful_arg (sm_ctxt, call, 0);
  if (dst_arg)
    {
      /* Transition from start state to "started".  */
      if (sm_ctxt.get_state (call, dst_arg) == m_start)
	sm_ctxt.set_next_state (call, dst_arg, m_started);
    }
}

/* Update state machine for a "va_arg" call.  */

void
va_list_state_machine::on_va_arg (sm_context &sm_ctxt,
				  const supernode *node,
				  const gcall *call) const
{
  const svalue *arg = get_stateful_arg (sm_ctxt, call, 0);
  if (arg)
    check_for_ended_va_list (sm_ctxt, node, call, arg, "va_arg");
}

/* Update state machine for a "va_end" call.  */

void
va_list_state_machine::on_va_end (sm_context &sm_ctxt,
				  const supernode *node,
				  const gcall *call) const
{
  const svalue *arg = get_stateful_arg (sm_ctxt, call, 0);
  if (arg)
    {
      state_t s = sm_ctxt.get_state (call, arg);
      /* Transition from "started" to "ended".  */
      if (s == m_started)
	sm_ctxt.set_next_state (call, arg, m_ended);
      else if (s == m_ended)
	check_for_ended_va_list (sm_ctxt, node, call, arg, "va_end");
    }
}

/* Implementation of state_machine::on_leak vfunc for va_list_state_machine
   (for complaining about leaks of values in state 'started').  */

std::unique_ptr<pending_diagnostic>
va_list_state_machine::on_leak (tree var) const
{
  return make_unique<va_list_leak> (*this, nullptr, var);
}

} // anonymous namespace

/* Internal interface to this file. */

state_machine *
make_va_list_state_machine (logger *logger)
{
  return new va_list_state_machine (logger);
}

/* Handler for "__builtin_va_start".  */

class kf_va_start : public known_function
{
public:
  bool matches_call_types_p (const call_details &) const final override
  {
    return true;
  }
  void impl_call_pre (const call_details &cd) const final override;
};

void
kf_va_start::impl_call_pre (const call_details &cd) const
{
  region_model *model = cd.get_model ();
  region_model_manager *mgr = cd.get_manager ();
  const svalue *out_ptr = cd.get_arg_svalue (0);
  const region *out_reg
    = model->deref_rvalue (out_ptr, cd.get_arg_tree (0), cd.get_ctxt ());
  const frame_region *frame = model->get_current_frame ();

  /* "*out_ptr = &IMPL_REGION;".  */
  const region *impl_reg = mgr->create_region_for_alloca (frame);

  /* We abuse the types here, since va_list_type isn't
     necessarily anything to do with a pointer.  */
  const svalue *ptr_to_impl_reg = mgr->get_ptr_svalue (NULL_TREE, impl_reg);
  model->set_value (out_reg, ptr_to_impl_reg, cd.get_ctxt ());

  if (model->get_stack_depth () > 1)
    {
      /* The interprocedural case: the frame containing the va_start call
	 will have been populated with any variadic aruguments.
	 Initialize IMPL_REGION with a ptr to var_arg_region 0.  */
      const region *init_var_arg_reg = mgr->get_var_arg_region (frame, 0);
      const svalue *ap_sval
	= mgr->get_ptr_svalue (NULL_TREE, init_var_arg_reg);
      model->set_value (impl_reg, ap_sval, cd.get_ctxt ());
    }
  else
    {
      /* The frame containing va_start is an entry-point to the analysis,
	 so there won't be any specific var_arg_regions populated within it.
	 Initialize IMPL_REGION as the UNKNOWN_SVALUE to avoid state
	 explosions on repeated calls to va_arg.  */
      const svalue *unknown_sval
	= mgr->get_or_create_unknown_svalue (NULL_TREE);
      model->set_value (impl_reg, unknown_sval, cd.get_ctxt ());
    }
}

/* Handler for "__builtin_va_copy".  */

class kf_va_copy : public known_function
{
public:
  bool matches_call_types_p (const call_details &) const final override
  {
    return true;
  }
  void impl_call_pre (const call_details &cd) const final override;
};

void
kf_va_copy::impl_call_pre (const call_details &cd) const
{
  region_model *model = cd.get_model ();
  region_model_manager *mgr = cd.get_manager ();
  const svalue *out_dst_ptr = cd.get_arg_svalue (0);
  const svalue *in_va_list
    = get_va_copy_arg (model, cd.get_ctxt (), cd.get_call_stmt (), 1);
  in_va_list
    = model->check_for_poison (in_va_list,
			       get_va_list_diag_arg (cd.get_arg_tree (1)),
			       NULL,
			       cd.get_ctxt ());

  const region *out_dst_reg
    = model->deref_rvalue (out_dst_ptr, cd.get_arg_tree (0), cd.get_ctxt ());

  /* "*out_dst_ptr = &NEW_IMPL_REGION;".  */
  const region *new_impl_reg
    = mgr->create_region_for_alloca (model->get_current_frame ());
  const svalue *ptr_to_new_impl_reg
    = mgr->get_ptr_svalue (NULL_TREE, new_impl_reg);
  model->set_value (out_dst_reg, ptr_to_new_impl_reg, cd.get_ctxt ());

  if (const region *old_impl_reg = in_va_list->maybe_get_region ())
    {
      /* "(NEW_IMPL_REGION) = (OLD_IMPL_REGION);".  */
      const svalue *existing_sval
	= model->get_store_value (old_impl_reg, cd.get_ctxt ());
      model->set_value (new_impl_reg, existing_sval, cd.get_ctxt ());
    }
}

/* Get the number of variadic arguments to CALLEE_FNDECL at CALL_STMT.  */

static int
get_num_variadic_arguments (tree callee_fndecl,
			    const gcall *call_stmt)
{
  int num_positional = 0;
  for (tree iter_parm = DECL_ARGUMENTS (callee_fndecl); iter_parm;
       iter_parm = DECL_CHAIN (iter_parm))
    num_positional++;
  return gimple_call_num_args (call_stmt) - num_positional;
}

/* An abstract subclass of pending_diagnostic for diagnostics relating
   to bad va_arg invocations.

   This shows the number of variadic arguments at the call of interest.
   Ideally we'd also be able to highlight individual arguments, but
   that location information isn't generally available from the middle end.  */

class va_arg_diagnostic : public pending_diagnostic
{
public:
  /* Override of pending_diagnostic::add_call_event,
     adding a custom call_event subclass.  */
  void add_call_event (const exploded_edge &eedge,
		       checker_path *emission_path) override
  {
    /* As per call_event, but show the number of variadic arguments
       in the call.  */
    class va_arg_call_event : public call_event
    {
    public:
      va_arg_call_event (const exploded_edge &eedge,
			 const event_loc_info &loc_info,
			 int num_variadic_arguments)
      : call_event (eedge, loc_info),
	m_num_variadic_arguments (num_variadic_arguments)
      {
      }

      void print_desc (pretty_printer &pp) const override
      {
	pp_printf_n (&pp,
		     m_num_variadic_arguments,
		     "calling %qE from %qE with %i variadic argument",
		     "calling %qE from %qE with %i variadic arguments",
		     get_callee_fndecl (),
		     get_caller_fndecl (),
		     m_num_variadic_arguments);
      }
    private:
      int m_num_variadic_arguments;
    };

    const frame_region *frame_reg = m_var_arg_reg->get_frame_region ();
    const exploded_node *dst_node = eedge.m_dest;
    if (dst_node->get_state ().m_region_model->get_current_frame ()
	== frame_reg)
      {
	const exploded_node *src_node = eedge.m_src;
	const program_point &src_point = src_node->get_point ();
	const int src_stack_depth = src_point.get_stack_depth ();
	const gimple *last_stmt = src_point.get_supernode ()->get_last_stmt ();
	const gcall *call_stmt = as_a <const gcall *> (last_stmt);
	int num_variadic_arguments
	  = get_num_variadic_arguments (dst_node->get_function ()->decl,
					call_stmt);
	emission_path->add_event
	  (make_unique<va_arg_call_event>
	   (eedge,
	    event_loc_info (last_stmt ? last_stmt->location : UNKNOWN_LOCATION,
			    src_point.get_fndecl (),
			    src_stack_depth),
	    num_variadic_arguments));
      }
    else
      pending_diagnostic::add_call_event (eedge, emission_path);
  }

protected:
  va_arg_diagnostic (tree va_list_tree, const var_arg_region *var_arg_reg)
  : m_va_list_tree (va_list_tree), m_var_arg_reg (var_arg_reg)
  {}

  bool subclass_equal_p (const pending_diagnostic &base_other) const override
  {
    const va_arg_diagnostic &other = (const va_arg_diagnostic &)base_other;
    return (same_tree_p (m_va_list_tree, other.m_va_list_tree)
	    && m_var_arg_reg == other.m_var_arg_reg);
  }

  /* Get the number of arguments consumed so far from the va_list
     (*before* this va_arg call).  */
  unsigned get_num_consumed () const
  {
    return m_var_arg_reg->get_index ();
  }

  /* Get a 1-based index of which variadic argument is being consumed.  */
  unsigned get_variadic_index_for_diagnostic () const
  {
    return get_num_consumed () + 1;
  }

  /* User-readable expr for the va_list argument to va_arg.  */
  tree m_va_list_tree;

  /* The region that the va_arg attempted to access.  */
  const var_arg_region *m_var_arg_reg;
};

/* A subclass of pending_diagnostic for complaining about a type mismatch
   between the result of:
     va_arg (AP);
   and the type of the argument that was passed to the variadic call.  */

class va_arg_type_mismatch : public va_arg_diagnostic
{
public:
  va_arg_type_mismatch (tree va_list_tree, const var_arg_region *var_arg_reg,
			tree expected_type, tree actual_type)
  : va_arg_diagnostic (va_list_tree, var_arg_reg),
    m_expected_type (expected_type), m_actual_type (actual_type)
  {}

  const char *get_kind () const final override
  {
    return "va_arg_type_mismatch";
  }

  bool subclass_equal_p (const pending_diagnostic &base_other)
    const final override
  {
    if (!va_arg_diagnostic::subclass_equal_p (base_other))
      return false;
    const va_arg_type_mismatch &other
      = (const va_arg_type_mismatch &)base_other;
    return (same_tree_p (m_expected_type, other.m_expected_type)
	    && same_tree_p (m_actual_type, other.m_actual_type));
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_va_arg_type_mismatch;
  }

  bool emit (diagnostic_emission_context &ctxt) final override
  {
    /* "CWE-686: Function Call With Incorrect Argument Type".  */
    ctxt.add_cwe (686);
    bool warned
      = ctxt.warn ("%<va_arg%> expected %qT but received %qT"
		   " for variadic argument %i of %qE",
		   m_expected_type, m_actual_type,
		   get_variadic_index_for_diagnostic (), m_va_list_tree);
    return warned;
  }

  bool
  describe_final_event (pretty_printer &pp,
			const evdesc::final_event &) final override
  {
    pp_printf (&pp,
	       "%<va_arg%> expected %qT but received %qT"
	       " for variadic argument %i of %qE",
	       m_expected_type, m_actual_type,
	       get_variadic_index_for_diagnostic (),
	       m_va_list_tree);
    return true;
  }

private:
  tree m_expected_type;
  tree m_actual_type;
};

/* A subclass of pending_diagnostic for complaining about a
     va_arg (AP);
   after all of the args in AP have been consumed.  */

class va_list_exhausted : public va_arg_diagnostic
{
public:
  va_list_exhausted (tree va_list_tree, const var_arg_region *var_arg_reg)
  : va_arg_diagnostic (va_list_tree, var_arg_reg)
  {}

  const char *get_kind () const final override
  {
    return "va_list_exhausted";
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_va_list_exhausted;
  }

  bool emit (diagnostic_emission_context &ctxt) final override
  {
    /* CWE-685: Function Call With Incorrect Number of Arguments.  */
    ctxt.add_cwe (685);
    bool warned = ctxt.warn ("%qE has no more arguments (%i consumed)",
			     m_va_list_tree, get_num_consumed ());
    return warned;
  }

  bool
  describe_final_event (pretty_printer &pp,
			const evdesc::final_event &) final override
  {
    pp_printf (&pp,
	       "%qE has no more arguments (%i consumed)",
	       m_va_list_tree, get_num_consumed ());
    return true;
  }
};

static bool
representable_in_integral_type_p (const svalue &sval, const_tree type)
{
  gcc_assert (INTEGRAL_TYPE_P (type));

  if (tree cst = sval.maybe_get_constant ())
    return wi::fits_to_tree_p (wi::to_wide (cst), type);

  return true;
}

/* Return true if it's OK to copy ARG_SVAL from ARG_TYPE to LHS_TYPE via
   va_arg (where argument promotion has already happened).  */

static bool
va_arg_compatible_types_p (tree lhs_type, tree arg_type, const svalue &arg_sval)
{
  if (compat_types_p (arg_type, lhs_type))
    return true;

  /* It's OK if both types are integer types, where one is signed and the
     other type the corresponding unsigned type, when the value is
     representable in both types.  */
  if (INTEGRAL_TYPE_P (lhs_type)
      && INTEGRAL_TYPE_P (arg_type)
      && TYPE_UNSIGNED (lhs_type) != TYPE_UNSIGNED (arg_type)
      && TYPE_PRECISION (lhs_type) == TYPE_PRECISION (arg_type)
      && representable_in_integral_type_p (arg_sval, lhs_type)
      && representable_in_integral_type_p (arg_sval, arg_type))
    return true;

  /* It's OK if one type is a pointer to void and the other is a
     pointer to a character type.
     This is handled by compat_types_p.  */

  /* Otherwise the types are not compatible.  */
  return false;
}

/* If AP_SVAL is a pointer to a var_arg_region, return that var_arg_region.
   Otherwise return NULL.  */

static const var_arg_region *
maybe_get_var_arg_region (const svalue *ap_sval)
{
  if (const region *reg = ap_sval->maybe_get_region ())
    return reg->dyn_cast_var_arg_region ();
  return NULL;
}

/* Handler for "__builtin_va_arg".  */

class kf_va_arg : public internal_known_function
{
public:
  void impl_call_pre (const call_details &cd) const final override;
};

void
kf_va_arg::impl_call_pre (const call_details &cd) const
{
  region_model_context *ctxt = cd.get_ctxt ();
  region_model *model = cd.get_model ();
  region_model_manager *mgr = cd.get_manager ();

  const svalue *in_ptr = cd.get_arg_svalue (0);
  const region *ap_reg
    = model->deref_rvalue (in_ptr, cd.get_arg_tree (0), ctxt);

  const svalue *ap_sval = model->get_store_value (ap_reg, ctxt);
  if (const svalue *cast = ap_sval->maybe_undo_cast ())
    ap_sval = cast;

  tree va_list_tree = get_va_list_diag_arg (cd.get_arg_tree (0));
  ap_sval = model->check_for_poison (ap_sval, va_list_tree, ap_reg, ctxt);

  cd.set_any_lhs_with_defaults ();

  if (const region *impl_reg = ap_sval->maybe_get_region ())
    {
      const svalue *old_impl_sval = model->get_store_value (impl_reg, ctxt);
      if (const var_arg_region *arg_reg
	  = maybe_get_var_arg_region (old_impl_sval))
	{
	  bool saw_problem = false;

	  const frame_region *frame_reg = arg_reg->get_frame_region ();
	  unsigned next_arg_idx = arg_reg->get_index ();

	  if (frame_reg->get_stack_depth () > 1)
	    {
	      /* The interprocedural case: the called frame will have been
		 populated with any variadic aruguments.
		 Attempt to extract arg_reg to cd's return region (which already
		 has a conjured_svalue), or warn if there's a problem
		 (incompatible types, or if we've run out of args).  */
	      if (const svalue *arg_sval
		  = model->get_store ()->get_any_binding
		      (mgr->get_store_manager (), arg_reg))
		{
		  tree lhs_type = cd.get_lhs_type ();
		  tree arg_type = arg_sval->get_type ();
		  if (va_arg_compatible_types_p (lhs_type, arg_type, *arg_sval))
		    cd.maybe_set_lhs (arg_sval);
		  else
		    {
		      if (ctxt)
			ctxt->warn (make_unique <va_arg_type_mismatch>
				      (va_list_tree,
				       arg_reg,
				       lhs_type,
				       arg_type));
		      saw_problem = true;
		    }
		}
	      else
		{
		  if (ctxt)
		    ctxt->warn (make_unique <va_list_exhausted> (va_list_tree,
								 arg_reg));
		  saw_problem = true;
		}
	    }
	  else
	    {
	      /* This frame is an entry-point to the analysis, so there won't be
		 any specific var_arg_regions populated within it.
		 We already have a conjured_svalue for the result, so leave
		 it untouched.  */
	      gcc_assert (frame_reg->get_stack_depth () == 1);
	    }

	  if (saw_problem)
	    {
	      /* Set impl_reg to UNKNOWN to suppress further warnings.  */
	      const svalue *new_ap_sval
		= mgr->get_or_create_unknown_svalue (impl_reg->get_type ());
	      model->set_value (impl_reg, new_ap_sval, ctxt);
	    }
	  else
	    {
	      /* Update impl_reg to advance to the next arg.  */
	      const region *next_var_arg_region
		= mgr->get_var_arg_region (frame_reg, next_arg_idx + 1);
	      const svalue *new_ap_sval
		= mgr->get_ptr_svalue (NULL_TREE, next_var_arg_region);
	      model->set_value (impl_reg, new_ap_sval, ctxt);
	    }
	}
    }
}

/* Handler for "__builtin_va_end".  */

class kf_va_end : public known_function
{
public:
  bool matches_call_types_p (const call_details &) const
  {
    return true;
  }
};

/* Populate KFM with instances of known functions relating to varargs.  */

void
register_varargs_builtins (known_function_manager &kfm)
{
  kfm.add (BUILT_IN_VA_START, make_unique<kf_va_start> ());
  kfm.add (BUILT_IN_VA_COPY, make_unique<kf_va_copy> ());
  kfm.add (IFN_VA_ARG, make_unique<kf_va_arg> ());
  kfm.add (BUILT_IN_VA_END, make_unique<kf_va_end> ());
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
