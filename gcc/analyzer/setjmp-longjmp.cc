/* setjmp/longjmp handling
   Copyright (C) 2019-2026 Free Software Foundation, Inc.
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

#include "analyzer/region-model.h"
#include "analyzer/checker-path.h"
#include "analyzer/checker-event.h"
#include "analyzer/exploded-graph.h"
#include "analyzer/constraint-manager.h"

#if ENABLE_ANALYZER

/* Return true if stmt is a setjmp or sigsetjmp call.  */

bool
is_setjmp_call_p (const gcall &call)
{
  if (is_special_named_call_p (call, "setjmp", 1)
      || is_special_named_call_p (call, "sigsetjmp", 2))
    /* region_model::on_setjmp requires a pointer.  */
    if (POINTER_TYPE_P (TREE_TYPE (gimple_call_arg (&call, 0))))
      return true;

  return false;
}

/* Return true if stmt is a longjmp or siglongjmp call.  */

bool
is_longjmp_call_p (const gcall &call)
{
  if (is_special_named_call_p (call, "longjmp", 2)
      || is_special_named_call_p (call, "siglongjmp", 2))
    /* exploded_node::on_longjmp requires a pointer for the initial
       argument.  */
    if (POINTER_TYPE_P (TREE_TYPE (gimple_call_arg (&call, 0))))
      return true;

  return false;
}

namespace ana {

/* struct setjmp_record.  */

int
setjmp_record::cmp (const setjmp_record &rec1, const setjmp_record &rec2)
{
  if (int cmp_enode = rec1.m_enode->m_index - rec2.m_enode->m_index)
    return cmp_enode;
  gcc_assert (&rec1 == &rec2);
  return 0;
}

/* class setjmp_svalue : public svalue.  */

bool
setjmp_svalue::key_t::operator< (const key_t &other) const
{
  if (int c = setjmp_record::cmp (m_record, other.m_record))
    return c < 0;
  return cmp_types (m_type, other.m_type) < 0;
}

/* Implementation of svalue::accept vfunc for setjmp_svalue.  */

void
setjmp_svalue::accept (visitor *v) const
{
  v->visit_setjmp_svalue (this);
}

/* Implementation of svalue::dump_to_pp vfunc for setjmp_svalue.  */

void
setjmp_svalue::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    pp_printf (pp, "SETJMP(EN: %i)", get_enode_index ());
  else
    pp_printf (pp, "setjmp_svalue(EN%i)", get_enode_index ());
}

/* Implementation of svalue::print_dump_widget_label vfunc for
   setjmp_svalue.  */

void
setjmp_svalue::print_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "setjmp_svalue(EN: %i)", get_enode_index ());
}

/* Implementation of svalue::add_dump_widget_children vfunc for
   setjmp_svalue.  */

void
setjmp_svalue::
add_dump_widget_children (text_art::tree_widget &,
			  const text_art::dump_widget_info &) const
{
  /* No children.  */
}

/* Get the index of the stored exploded_node.  */

int
setjmp_svalue::get_enode_index () const
{
  return m_setjmp_record.m_enode->m_index;
}

/* Verify that the stack at LONGJMP_POINT is still valid, given a call
   to "setjmp" at SETJMP_POINT - the stack frame that "setjmp" was
   called in must still be valid.

   Caveat: this merely checks the call_strings in the points; it doesn't
   detect the case where a frame returns and is then called again.  */

static bool
valid_longjmp_stack_p (const program_point &longjmp_point,
		       const program_point &setjmp_point)
{
  const call_string &cs_at_longjmp = longjmp_point.get_call_string ();
  const call_string &cs_at_setjmp = setjmp_point.get_call_string ();

  if (cs_at_longjmp.length () < cs_at_setjmp.length ())
    return false;

  /* Check that the call strings match, up to the depth of the
     setjmp point.  */
  for (unsigned depth = 0; depth < cs_at_setjmp.length (); depth++)
    if (cs_at_longjmp[depth] != cs_at_setjmp[depth])
      return false;

  return true;
}

/* A pending_diagnostic subclass for complaining about bad longjmps,
   where the enclosing function of the "setjmp" has returned (and thus
   the stack frame no longer exists).  */

class stale_jmp_buf : public pending_diagnostic_subclass<stale_jmp_buf>
{
public:
  stale_jmp_buf (const gcall &setjmp_call, const gcall &longjmp_call,
		 const program_point &setjmp_point)
  : m_setjmp_call (setjmp_call), m_longjmp_call (longjmp_call),
    m_setjmp_point (setjmp_point), m_stack_pop_event (nullptr)
  {}

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_stale_setjmp_buffer;
  }

  bool emit (diagnostic_emission_context &ctxt) final override
  {
    return ctxt.warn ("%qs called after enclosing function of %qs has returned",
		      get_user_facing_name (m_longjmp_call),
		      get_user_facing_name (m_setjmp_call));
  }

  const char *get_kind () const final override
  { return "stale_jmp_buf"; }

  bool operator== (const stale_jmp_buf &other) const
  {
    return (&m_setjmp_call == &other.m_setjmp_call
	    && &m_longjmp_call == &other.m_longjmp_call);
  }

  bool
  maybe_add_custom_events_for_eedge (const exploded_edge &eedge,
				     checker_path *emission_path)
    final override
  {
    /* Detect exactly when the stack first becomes invalid,
       and issue an event then.  */
    if (m_stack_pop_event)
      return false;
    const exploded_node *src_node = eedge.m_src;
    const program_point &src_point = src_node->get_point ();
    const exploded_node *dst_node = eedge.m_dest;
    const program_point &dst_point = dst_node->get_point ();
    if (valid_longjmp_stack_p (src_point, m_setjmp_point)
	&& !valid_longjmp_stack_p (dst_point, m_setjmp_point))
      {
	/* Compare with diagnostic_manager::add_events_for_superedge.  */
	const int src_stack_depth = src_point.get_stack_depth ();
	m_stack_pop_event = new precanned_custom_event
	  (event_loc_info (src_point.get_location (),
			   src_point.get_fndecl (),
			   src_stack_depth),
	   "stack frame is popped here, invalidating saved environment");
	emission_path->add_event
	  (std::unique_ptr<custom_event> (m_stack_pop_event));
	return false;
      }
    return false;
  }

  bool
  describe_final_event (pretty_printer &pp,
			const evdesc::final_event &) final override
  {
    if (m_stack_pop_event)
      pp_printf (&pp,
		 "%qs called after enclosing function of %qs returned at %@",
		 get_user_facing_name (m_longjmp_call),
		 get_user_facing_name (m_setjmp_call),
		 m_stack_pop_event->get_id_ptr ());
    else
      pp_printf (&pp,
		 "%qs called after enclosing function of %qs has returned",
		 get_user_facing_name (m_longjmp_call),
		 get_user_facing_name (m_setjmp_call));
    return true;
  }


private:
  const gcall &m_setjmp_call;
  const gcall &m_longjmp_call;
  program_point m_setjmp_point;
  custom_event *m_stack_pop_event;
};

/* Update this model for a call and return of setjmp/sigsetjmp at CALL within
   ENODE, using CTXT to report any diagnostics.

   This is for the initial direct invocation of setjmp/sigsetjmp (which returns
   0), as opposed to any second return due to longjmp/sigsetjmp.  */

void
region_model::on_setjmp (const gcall &call,
			 const exploded_node &enode,
			 const superedge &sedge,
			 region_model_context *ctxt)
{
  const svalue *buf_ptr = get_rvalue (gimple_call_arg (&call, 0), ctxt);
  const region *buf_reg = deref_rvalue (buf_ptr, gimple_call_arg (&call, 0),
					 ctxt);

  /* Create a setjmp_svalue for this call and store it in BUF_REG's
     region.  */
  if (buf_reg)
    {
      setjmp_record r (&enode, &sedge, call);
      const svalue *sval
	= m_mgr->get_or_create_setjmp_svalue (r, buf_reg->get_type ());
      set_value (buf_reg, sval, ctxt);
    }

  /* Direct calls to setjmp return 0.  */
  if (tree lhs = gimple_call_lhs (&call))
    {
      const svalue *new_sval
	= m_mgr->get_or_create_int_cst (TREE_TYPE (lhs), 0);
      const region *lhs_reg = get_lvalue (lhs, ctxt);
      set_value (lhs_reg, new_sval, ctxt);
    }
}

/* Update this region_model for rewinding from a "longjmp" at LONGJMP_CALL
   to a "setjmp" at SETJMP_CALL where the final stack depth should be
   SETJMP_STACK_DEPTH.  Pop any stack frames.  Leak detection is *not*
   done, and should be done by the caller.  */

void
region_model::on_longjmp (const gcall &longjmp_call, const gcall &setjmp_call,
			   int setjmp_stack_depth, region_model_context *ctxt)
{
  /* Evaluate the val, using the frame of the "longjmp".  */
  tree fake_retval = gimple_call_arg (&longjmp_call, 1);
  const svalue *fake_retval_sval = get_rvalue (fake_retval, ctxt);

  /* Pop any frames until we reach the stack depth of the function where
     setjmp was called.  */
  gcc_assert (get_stack_depth () >= setjmp_stack_depth);
  while (get_stack_depth () > setjmp_stack_depth)
    pop_frame (nullptr, nullptr, ctxt, nullptr, false);

  gcc_assert (get_stack_depth () == setjmp_stack_depth);

  /* Assign to LHS of "setjmp" in new_state.  */
  if (tree lhs = gimple_call_lhs (&setjmp_call))
    {
      /* Passing 0 as the val to longjmp leads to setjmp returning 1.  */
      const svalue *zero_sval
	= m_mgr->get_or_create_int_cst (TREE_TYPE (fake_retval), 0);
      tristate eq_zero = eval_condition (fake_retval_sval, EQ_EXPR, zero_sval);
      /* If we have 0, use 1.  */
      if (eq_zero.is_true ())
	{
	  const svalue *one_sval
	    = m_mgr->get_or_create_int_cst (TREE_TYPE (fake_retval), 1);
	  fake_retval_sval = one_sval;
	}
      else
	{
	  /* Otherwise note that the value is nonzero.  */
	  m_constraints->add_constraint (fake_retval_sval, NE_EXPR, zero_sval);
	}

      /* Decorate the return value from setjmp as being unmergeable,
	 so that we don't attempt to merge states with it as zero
	 with states in which it's nonzero, leading to a clean distinction
	 in the exploded_graph between the first return and the second
	 return.  */
      fake_retval_sval = m_mgr->get_or_create_unmergeable (fake_retval_sval);

      const region *lhs_reg = get_lvalue (lhs, ctxt);
      set_value (lhs_reg, fake_retval_sval, ctxt);
    }
}

/* Handle LONGJMP_CALL, a call to longjmp or siglongjmp.

   Attempt to locate where setjmp/sigsetjmp was called on the jmp_buf and build
   an exploded_node and exploded_edge to it representing a rewind to that frame,
   handling the various kinds of failure that can occur.  */

void
exploded_node::on_longjmp (exploded_graph &eg,
			   const gcall &longjmp_call,
			   program_state *new_state,
			   region_model_context *ctxt)
{
  tree buf_ptr = gimple_call_arg (&longjmp_call, 0);
  gcc_assert (POINTER_TYPE_P (TREE_TYPE (buf_ptr)));

  region_model *new_region_model = new_state->m_region_model;
  const svalue *buf_ptr_sval = new_region_model->get_rvalue (buf_ptr, ctxt);
  const region *buf = new_region_model->deref_rvalue (buf_ptr_sval, buf_ptr,
						       ctxt);

  const svalue *buf_content_sval
    = new_region_model->get_store_value (buf, ctxt);
  const setjmp_svalue *setjmp_sval
    = buf_content_sval->dyn_cast_setjmp_svalue ();
  if (!setjmp_sval)
    return;

  const setjmp_record tmp_setjmp_record = setjmp_sval->get_setjmp_record ();

  /* Build a custom enode and eedge for rewinding from the longjmp/siglongjmp
     call back to the setjmp/sigsetjmp.  */
  rewind_info_t rewind_info (tmp_setjmp_record, longjmp_call);

  const gcall &setjmp_call = rewind_info.get_setjmp_call ();
  const program_point point_before_setjmp = rewind_info.get_point_before_setjmp ();
  const program_point point_after_setjmp = rewind_info.get_point_after_setjmp ();

  const program_point &longjmp_point = get_point ();

  /* Verify that the setjmp's call_stack hasn't been popped.  */
  if (!valid_longjmp_stack_p (longjmp_point, point_after_setjmp))
    {
      ctxt->warn (std::make_unique<stale_jmp_buf> (setjmp_call,
						   longjmp_call,
						   point_before_setjmp));
      return;
    }

  gcc_assert (longjmp_point.get_stack_depth ()
	      >= point_after_setjmp.get_stack_depth ());

  /* Update the state for use by the destination node.  */

  /* Stash the current number of diagnostics so that we can update
     any that this adds to show where the longjmp is rewinding to.  */

  diagnostic_manager *dm = &eg.get_diagnostic_manager ();
  unsigned prev_num_diagnostics = dm->get_num_diagnostics ();

  new_region_model->on_longjmp (longjmp_call, setjmp_call,
				point_after_setjmp.get_stack_depth (), ctxt);

  /* Detect leaks in the new state relative to the old state.  */
  program_state::detect_leaks (get_state (), *new_state, nullptr,
				eg.get_ext_state (), ctxt);
  exploded_node *next
    = eg.get_or_create_node (point_after_setjmp, *new_state, this);

  /* Create custom exploded_edge for a longjmp.  */
  if (next)
    {
      exploded_edge *eedge
	= eg.add_edge (const_cast<exploded_node *> (this), next, nullptr, true,
		       std::make_unique<rewind_info_t> (tmp_setjmp_record,
							longjmp_call));

      /* For any diagnostics that were queued here (such as leaks) we want
	 the checker_path to show the rewinding events after the "final event"
	 so that the user sees where the longjmp is rewinding to (otherwise the
	 path is meaningless).

	 For example, we want to emit something like:
                        |   NN | {
                        |   NN |   longjmp (env, 1);
                        |      |   ~~~~~~~~~~~~~~~~
                        |      |   |
                        |      |   (10) 'ptr' leaks here; was allocated at (7)
                        |      |   (11) rewinding from 'longjmp' in 'inner'...
                        |
          <-------------+
          |
        'outer': event 12
          |
          |   NN |   i = setjmp(env);
          |      |       ^~~~~~
          |      |       |
          |      |       (12) ...to 'setjmp' in 'outer' (saved at (2))

	 where the "final" event above is event (10), but we want to append
	 events (11) and (12) afterwards.

	 Do this by setting m_trailing_eedge on any diagnostics that were
	 just saved.  */
      unsigned num_diagnostics = dm->get_num_diagnostics ();
      for (unsigned i = prev_num_diagnostics; i < num_diagnostics; i++)
	{
	  saved_diagnostic *sd = dm->get_saved_diagnostic (i);
	  sd->m_trailing_eedge = eedge;
	}
    }
}

/* class rewind_info_t : public custom_edge_info.  */

/* Implementation of custom_edge_info::update_model vfunc
   for rewind_info_t.

   Update state for the special-case of a rewind of a longjmp
   to a setjmp (which doesn't have a superedge, but does affect
   state).  */

bool
rewind_info_t::update_model (region_model *model,
			     const exploded_edge *eedge,
			     region_model_context *) const
{
  gcc_assert (eedge);
  const program_point &longjmp_point = eedge->m_src->get_point ();
  const program_point &setjmp_point = eedge->m_dest->get_point ();

  gcc_assert (longjmp_point.get_stack_depth ()
	      >= setjmp_point.get_stack_depth ());

  model->on_longjmp (get_longjmp_call (),
		     get_setjmp_call (),
		     setjmp_point.get_stack_depth (), nullptr);
  return true;
}

/* Implementation of custom_edge_info::add_events_to_path vfunc
   for rewind_info_t.  */

void
rewind_info_t::add_events_to_path (checker_path *emission_path,
				   const exploded_edge &eedge,
				   pending_diagnostic &,
				   const state_transition *) const
{
  const exploded_node *src_node = eedge.m_src;
  const program_point &src_point = src_node->get_point ();
  const int src_stack_depth = src_point.get_stack_depth ();
  const exploded_node *dst_node = eedge.m_dest;
  const program_point &dst_point = dst_node->get_point ();
  const int dst_stack_depth = dst_point.get_stack_depth ();

  emission_path->add_event
    (std::make_unique<rewind_from_longjmp_event>
       (&eedge,
	event_loc_info (get_longjmp_call ().location,
			src_point.get_fndecl (),
			src_stack_depth),
	this));
  emission_path->add_event
    (std::make_unique<rewind_to_setjmp_event>
       (&eedge,
	event_loc_info (get_setjmp_call ().location,
			dst_point.get_fndecl (),
			dst_stack_depth),
	this));
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
