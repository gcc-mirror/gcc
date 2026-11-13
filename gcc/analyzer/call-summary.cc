/* Classes for working with summaries of function calls.
   Copyright (C) 2022 David Malcolm <dmalcolm@redhat.com>.

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
#include "analyzer/call-summary.h"
#include "analyzer/exploded-graph.h"

#if ENABLE_ANALYZER

namespace ana {

/* class call_summary.  */

const program_state &
call_summary::get_state () const
{
  return m_enode->get_state ();
}

tree
call_summary::get_fndecl () const
{
  return m_enode->get_function ()->decl;
}

label_text
call_summary::get_desc () const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;

  get_user_facing_desc (&pp);
  if (flag_analyzer_verbose_edges)
    pp_printf (&pp, " (call summary; EN: %i)", m_enode->m_index);

  return label_text::take (xstrdup (pp_formatted_text (&pp)));
}

/* Generate a user-facing description of this call summary.c
   This has various heuristics for distinguishing between different
   summaries.
   This will help with debugging, too.  */

void
call_summary::get_user_facing_desc (pretty_printer *pp) const
{
  tree fndecl = get_fndecl ();

  /* If there are multiple summaries, try to use the return value to
     distinguish between them.  */
  if (m_per_fn_data->m_summaries.length () > 1)
    {
      if (tree result = DECL_RESULT (fndecl))
	{
	  const region *result_reg
	    = get_state ().m_region_model->get_lvalue (result, nullptr);
	  const svalue *result_sval
	    = get_state ().m_region_model->get_store_value (result_reg, nullptr);
	  switch (result_sval->get_kind ())
	    {
	    default:
	      break;
	    case SK_POINTER:
	      {
		const pointer_svalue *ptr_sval
		  = as_a <const pointer_svalue *> (result_sval);
		const region *base_reg = &ptr_sval->get_base_region ();
		switch (base_reg->get_kind ())
		  {
		  default:
		    break;
		  case RK_HEAP_ALLOCATED:
		    pp_printf (pp,
			       "when %qE returns pointer"
			       " to heap-allocated buffer",
			       fndecl);
		    return;
		  }
	      }
	      break;
	    case SK_CONSTANT:
	      {
		const constant_svalue *constant_sval
		  = as_a <const constant_svalue *> (result_sval);
		tree cst = constant_sval->get_constant ();
		if (POINTER_TYPE_P (TREE_TYPE (result))
		    && zerop (cst))
		  pp_printf (pp, "when %qE returns NULL", fndecl);
		else
		  pp_printf (pp, "when %qE returns %qE", fndecl, cst);
		return;
	      }
	    }
	}
    }

  /* Fallback.  */
  pp_printf (pp, "when %qE returns", fndecl);
}

void
call_summary::dump_to_logger (logger &logger) const
{
  symbolic::dump_node_tree_to_logger<call_summary> (*this, logger);
}

std::unique_ptr<text_art::tree_widget>
call_summary::make_dump_widget (symbolic::node_dump_state &nds) const
{
  using text_art::tree_widget;

  label_text desc = get_desc ();
  std::unique_ptr<tree_widget> summary_widget
    (tree_widget::from_fmt (nds, nullptr, "call_summary: %qs", desc.get ()));

  summary_widget->add_child (get_state ().make_dump_widget (nds));

  return summary_widget;
}

/* class call_summary_replay.  */

/* call_summary_replay's ctor.
   Populate the cache with params for the summary based on
   arguments at the caller. */

call_summary_replay::call_summary_replay (const call_details &cd,
					  const function &called_fn,
					  call_summary &summary,
					  const extrinsic_state &ext_state)
: m_cd (cd),
  m_summary (summary),
  m_ext_state (ext_state)
{
  region_model_manager *mgr = cd.get_manager ();

  // populate params based on args
  tree fndecl = called_fn.decl;

  /* Get a frame_region for use with respect to the summary.
     This will be a top-level frame, since that's what's in
     the summary.  */
  const frame_region *summary_frame
    = mgr->get_frame_region (nullptr, called_fn);

  unsigned idx = 0;
  for (tree iter_parm = DECL_ARGUMENTS (fndecl); iter_parm;
       iter_parm = DECL_CHAIN (iter_parm), ++idx)
    {
      /* If there's a mismatching declaration, the call stmt might
	 not have enough args.  Handle this case by leaving the
	 rest of the params as uninitialized.  */
      if (idx >= cd.num_args ())
	break;
      const svalue *caller_arg_sval = cd.get_arg_svalue (idx);
      tree parm_lval = iter_parm;
      if (tree parm_default_ssa = get_ssa_default_def (called_fn, iter_parm))
	parm_lval = parm_default_ssa;
      const region *summary_parm_reg
	= summary_frame->get_region_for_local (mgr, parm_lval, cd.get_ctxt ());
      const svalue *summary_initial_parm_reg
	= mgr->get_or_create_initial_value (summary_parm_reg);
      add_svalue_mapping (summary_initial_parm_reg, caller_arg_sval);
    }

  /* Handle any variadic args.  */
  unsigned va_arg_idx = 0;
  for (; idx < cd.num_args (); idx++, va_arg_idx++)
    {
      const svalue *caller_arg_sval = cd.get_arg_svalue (idx);
      const region *summary_var_arg_reg
	= mgr->get_var_arg_region (summary_frame, va_arg_idx);
      const svalue *summary_initial_var_arg_reg
	= mgr->get_or_create_initial_value (summary_var_arg_reg);
      add_svalue_mapping (summary_initial_var_arg_reg, caller_arg_sval);
    }
}

/* Try to convert SUMMARY_SVAL in the summary to a corresponding svalue
   in the caller, caching the result.

   Return nullptr if the conversion is not possible.  */

const svalue *
call_summary_replay::convert_svalue_from_summary (const svalue *summary_sval)
{
  gcc_assert (summary_sval);

  if (const svalue **slot
	= m_map_svalue_from_summary_to_caller.get (summary_sval))
    return *slot;

  const svalue *caller_sval = convert_svalue_from_summary_1 (summary_sval);

  if (caller_sval)
    if (summary_sval->get_type () && caller_sval->get_type ())
      gcc_assert (types_compatible_p (summary_sval->get_type (),
				      caller_sval->get_type ()));

  /* Add to cache.  */
  add_svalue_mapping (summary_sval, caller_sval);

  return caller_sval;
}

/* Implementation of call_summary_replay::convert_svalue_from_summary.  */

const svalue *
call_summary_replay::convert_svalue_from_summary_1 (const svalue *summary_sval)
{
  gcc_assert (summary_sval);

  switch (summary_sval->get_kind ())
    {
    default:
      gcc_unreachable ();
    case SK_POINTER:
      {
	const pointer_svalue *region_summary_sval
	  = as_a <const pointer_svalue *> (summary_sval);
	auto &summary_sym_ptr = region_summary_sval->get_symbolic_pointer ();

	const region *caller_base_reg
	  = convert_region_from_summary (&summary_sym_ptr.get_base_region ());
	if (!caller_base_reg)
	  return nullptr;
	auto *caller_byte_offset
	  = convert_scalar_from_summary (summary_sym_ptr.get_byte_offset ());
	if (!caller_byte_offset)
	  return nullptr;
	region_model_manager *mgr = get_manager ();
	return mgr->get_ptr_svalue (summary_sval->get_type (),
				    symbolic::pointer (*caller_base_reg,
						       *caller_byte_offset));
      }
      break;

    case SK_CONSTANT:
    case SK_PLACEHOLDER:
    case SK_POISONED:
    case SK_UNKNOWN:
      return summary_sval;

    case SK_SETJMP:
      return nullptr; // TODO

    case SK_INITIAL:
      {
	const initial_svalue *initial_summary_sval
	  = as_a <const initial_svalue *> (summary_sval);
	/* Params should already be in the cache, courtesy of the ctor.  */
	gcc_assert (!initial_summary_sval->initial_value_of_param_p ());

	/* Initial value of region within the summary is the value of the
	   region at the point of the call.  */
	const region *summary_reg = initial_summary_sval->get_region ();
	const region *caller_reg = convert_region_from_summary (summary_reg);
	if (!caller_reg)
	  return nullptr;
	const svalue *caller_sval
	  = m_cd.get_model ()->get_store_value (caller_reg, m_cd.get_ctxt ());
	return caller_sval;
      }
      break;
    case SK_UNARYOP:
      {
	const unaryop_svalue *unaryop_summary_sval
	  = as_a <const unaryop_svalue *> (summary_sval);
	const svalue *summary_arg = unaryop_summary_sval->get_arg ();
	const svalue *caller_arg = convert_svalue_from_summary (summary_arg);
	if (!caller_arg)
	  return nullptr;
	region_model_manager *mgr = get_manager ();
	return mgr->get_or_create_unaryop (summary_sval->get_type (),
					   unaryop_summary_sval->get_op (),
					   caller_arg);
      }
      break;
    case SK_BINOP:
      {
	const binop_svalue *binop_summary_sval
	  = as_a <const binop_svalue *> (summary_sval);
	const svalue *summary_arg0 = binop_summary_sval->get_arg0 ();
	const svalue *caller_arg0 = convert_svalue_from_summary (summary_arg0);
	if (!caller_arg0)
	  return nullptr;
	const svalue *summary_arg1 = binop_summary_sval->get_arg1 ();
	const svalue *caller_arg1 = convert_svalue_from_summary (summary_arg1);
	if (!caller_arg1)
	  return nullptr;
	region_model_manager *mgr = get_manager ();
	return mgr->get_or_create_binop (summary_sval->get_type (),
					 binop_summary_sval->get_op (),
					 caller_arg0,
					 caller_arg1);
      }
      break;
    case SK_BIT_VECTOR:
      {
	auto *sval_from_bv
	  = as_a<const svalue_from_bit_vector *> (summary_sval);
	auto *caller_bv
	  = convert_bit_vector_from_summary (sval_from_bv->get_bit_vector ());
	if (!caller_bv)
	  return nullptr;
	region_model_manager *mgr = get_manager ();
	return mgr->get_svalue_from_bit_vector (summary_sval->get_type (),
						*caller_bv);
      }
      break;
    case SK_UNMERGEABLE:
      {
	const unmergeable_svalue *unmergeable_summary_sval
	  = as_a <const unmergeable_svalue *> (summary_sval);
	const svalue *summary_arg_sval = unmergeable_summary_sval->get_arg ();
	const svalue *caller_arg_sval
	  = convert_svalue_from_summary (summary_arg_sval);
	if (!caller_arg_sval)
	  return nullptr;
	region_model_manager *mgr = get_manager ();
	return mgr->get_or_create_unmergeable (caller_arg_sval);
      }
      break;
    case SK_WIDENING:
      {
	const widening_svalue *widening_summary_sval
	  = as_a <const widening_svalue *> (summary_sval);
	const supernode *snode = widening_summary_sval->get_snode ();
	const svalue *summary_base_sval
	  = widening_summary_sval->get_base_svalue ();
	const svalue *caller_base_sval
	  = convert_svalue_from_summary (summary_base_sval);
	if (!(caller_base_sval
	      && caller_base_sval->can_have_associated_state_p ()))
	  return nullptr;
	const svalue *summary_iter_sval
	  = widening_summary_sval->get_iter_svalue ();
	const svalue *caller_iter_sval
	  = convert_svalue_from_summary (summary_iter_sval);
	if (!(caller_iter_sval
	      && caller_iter_sval->can_have_associated_state_p ()))
	  return nullptr;
	region_model_manager *mgr = get_manager ();
	return mgr->get_or_create_widening_svalue
	  (summary_iter_sval->get_type (),
	   snode,
	   caller_base_sval,
	   caller_iter_sval);
      }
      break;
    case SK_ASM_OUTPUT:
      {
	const asm_output_svalue *asm_output_summary_sval
	  = as_a <const asm_output_svalue *> (summary_sval);
	const char *asm_string = asm_output_summary_sval->get_asm_string ();
	unsigned output_idx = asm_output_summary_sval->get_output_idx ();
	unsigned num_inputs = asm_output_summary_sval->get_num_inputs ();
	unsigned num_outputs = asm_output_summary_sval->get_num_outputs ();
	auto_vec<const svalue *> inputs (num_inputs);
	for (unsigned idx = 0; idx < num_inputs; idx++)
	  {
	    const svalue *summary_input
	      = asm_output_summary_sval->get_input (idx);
	    const svalue *caller_input
	      = convert_svalue_from_summary (summary_input);
	    if (!caller_input)
	      return nullptr;
	    inputs.safe_push (caller_input);
	  }
	region_model_manager *mgr = get_manager ();
	return mgr->get_or_create_asm_output_svalue (summary_sval->get_type (),
						     asm_string,
						     output_idx,
						     num_outputs,
						     inputs);
      }
      break;
    case SK_CONST_FN_RESULT:
      {
	const const_fn_result_svalue *const_fn_result_summary_sval
	  = as_a <const const_fn_result_svalue *> (summary_sval);
	tree fndecl = const_fn_result_summary_sval->get_fndecl ();
	unsigned num_inputs = const_fn_result_summary_sval->get_num_inputs ();
	auto_vec<const svalue *> inputs (num_inputs);
	for (unsigned idx = 0; idx < num_inputs; idx++)
	  {
	    const svalue *summary_input
	      = const_fn_result_summary_sval->get_input (idx);
	    const svalue *caller_input
	      = convert_svalue_from_summary (summary_input);
	    if (!caller_input)
	      return nullptr;
	    inputs.safe_push (caller_input);
	  }
	region_model_manager *mgr = get_manager ();
	return mgr->get_or_create_const_fn_result_svalue
	  (summary_sval->get_type (),
	   fndecl,
	   inputs);
      }
      break;
    }
}

/* Try to convert SUMMARY_REG in the summary to a corresponding region
   in the caller, caching the result.

   Return nullptr if the conversion is not possible.  */

const region *
call_summary_replay::convert_region_from_summary (const region *summary_reg)
{
  gcc_assert (summary_reg);

  if (const region **slot
	= m_map_region_from_summary_to_caller.get (summary_reg))
    return *slot;

  const region *caller_reg = convert_region_from_summary_1 (summary_reg);

  if (caller_reg)
    if (summary_reg->get_type () && caller_reg->get_type ())
      gcc_assert (types_compatible_p (summary_reg->get_type (),
				      caller_reg->get_type ()));

  /* Add to cache.  */
  add_region_mapping (summary_reg, caller_reg);

  return caller_reg;
}

/* Implementation of call_summary_replay::convert_region_from_summary.  */

const region *
call_summary_replay::convert_region_from_summary_1 (const region *summary_reg)
{
  gcc_assert (summary_reg);

  region_model_manager *mgr = get_manager ();
  switch (summary_reg->get_kind ())
    {
    default:
      gcc_unreachable ();
      /* Top-level regions.  */
    case RK_FRAME:
    case RK_GLOBALS:
    case RK_CODE:
    case RK_STACK:
    case RK_HEAP:
    case RK_THREAD_LOCAL:
    case RK_ROOT:
      /* These should never be pointed to by a pointer_svalue.  */
      gcc_unreachable ();

    case RK_FUNCTION:
    case RK_LABEL:
    case RK_STRING:
    case RK_ERRNO:
    case RK_UNKNOWN:
    case RK_PRIVATE:
      /* We can reuse these regions directly.  */
      return summary_reg;

    case RK_SYMBOLIC:
      {
	const symbolic_region *summary_symbolic_reg
	  = as_a <const symbolic_region *> (summary_reg);
	const svalue *summary_ptr_sval = summary_symbolic_reg->get_pointer ();
	const svalue *caller_ptr_sval
	  = convert_svalue_from_summary (summary_ptr_sval);
	if (!caller_ptr_sval)
	  return nullptr;
	const region *caller_reg
	  = get_caller_model ()->deref_rvalue (caller_ptr_sval,
					       NULL_TREE,
					       get_ctxt ());
	caller_reg = mgr->get_cast_region (caller_reg,
					   summary_reg->get_type ());
	return caller_reg;
      }
      break;

    case RK_DECL:
      {
	const decl_region *summary_decl_reg
	  = as_a <const decl_region *> (summary_reg);
	tree decl = summary_decl_reg->get_decl ();
	switch (TREE_CODE (decl))
	  {
	  default:
	    gcc_unreachable ();
	  case SSA_NAME:
	    /* We don't care about writes to locals within
	       the summary.  */
	    return nullptr;
	  case VAR_DECL:
	    /* We don't care about writes to locals within
	       the summary.  */
	    if (is_global_var (decl))
	      /* If it's a global, we can reuse the region directly.  */
	      return summary_reg;
	    else
	      /* Otherwise, we don't care about locals.  */
	      return nullptr;
	  case RESULT_DECL:
	    return m_cd.get_lhs_region ();
	  case PARM_DECL:
	    /* Writes (by value) to parms should be visible to the caller.  */
	    return nullptr;
	  }
      }
      break;
    case RK_FIELD:
      {
	const field_region *summary_field_reg
	  = as_a <const field_region *> (summary_reg);
	const region *summary_parent_reg = summary_reg->get_parent_region ();
	const region *caller_parent_reg
	  = convert_region_from_summary (summary_parent_reg);
	if (!caller_parent_reg)
	  return nullptr;
	tree field = summary_field_reg->get_field ();
	return mgr->get_field_region (caller_parent_reg, field);
      }
      break;
    case RK_ELEMENT:
      {
	const element_region *summary_element_reg
	  = as_a <const element_region *> (summary_reg);
	const region *summary_parent_reg = summary_reg->get_parent_region ();
	const region *caller_parent_reg
	  = convert_region_from_summary (summary_parent_reg);
	if (!caller_parent_reg)
	  return nullptr;
	const svalue *summary_index = summary_element_reg->get_index ();
	const svalue *caller_index
	  = convert_svalue_from_summary (summary_index);
	if (!caller_index)
	  return nullptr;
	return mgr->get_element_region (caller_parent_reg,
					summary_reg->get_type (),
					caller_index);
      }
      break;
    case RK_OFFSET:
      {
	const offset_region *summary_offset_reg
	  = as_a <const offset_region *> (summary_reg);
	const region *summary_parent_reg = summary_reg->get_parent_region ();
	const region *caller_parent_reg
	  = convert_region_from_summary (summary_parent_reg);
	if (!caller_parent_reg)
	  return nullptr;
	const svalue *summary_byte_offset
	  = summary_offset_reg->get_byte_offset ();
	const svalue *caller_byte_offset
	  = convert_svalue_from_summary (summary_byte_offset);
	if (!caller_byte_offset)
	  return nullptr;
	return mgr->get_offset_region (caller_parent_reg,
				       summary_reg->get_type (),
				       caller_byte_offset);
      }
      break;
    case RK_SIZED:
      {
	const sized_region *summary_sized_reg
	  = as_a <const sized_region *> (summary_reg);
	const region *summary_parent_reg = summary_reg->get_parent_region ();
	const region *caller_parent_reg
	  = convert_region_from_summary (summary_parent_reg);
	if (!caller_parent_reg)
	  return nullptr;
	const svalue *summary_byte_size
	  = summary_sized_reg->get_byte_size_sval (mgr);
	const svalue *caller_byte_size
	  = convert_svalue_from_summary (summary_byte_size);
	if (!caller_byte_size)
	  return nullptr;
	return mgr->get_sized_region (caller_parent_reg,
				       summary_reg->get_type (),
				       caller_byte_size);
      }
      break;
    case RK_CAST:
      {
	const region *summary_parent_reg = summary_reg->get_parent_region ();
	const region *caller_parent_reg
	  = convert_region_from_summary (summary_parent_reg);
	if (!caller_parent_reg)
	  return nullptr;
	return mgr->get_cast_region (caller_parent_reg,
				     summary_reg->get_type ());
      }
      break;
    case RK_HEAP_ALLOCATED:
      {
	/* If we have a heap-allocated region in the summary, then
	   it was allocated within the callee.
	   Create a new heap-allocated region to summarize this.  */
	auto_bitmap heap_regs_in_use;
	get_caller_model ()->get_referenced_base_regions (heap_regs_in_use);
	return mgr->get_or_create_region_for_heap_alloc (heap_regs_in_use);
      }
      break;
    case RK_ALLOCA:
      return nullptr;
    case RK_BIT_RANGE:
      {
	const bit_range_region *summary_bit_range_reg
	  = as_a <const bit_range_region *> (summary_reg);
	const region *summary_parent_reg = summary_reg->get_parent_region ();
	const region *caller_parent_reg
	  = convert_region_from_summary (summary_parent_reg);
	if (!caller_parent_reg)
	  return nullptr;
	const concrete_bit_range &bits = summary_bit_range_reg->get_bits ();
	return mgr->get_bit_range (caller_parent_reg,
				   summary_reg->get_type (),
				   bits);
      }
      break;
    case RK_VAR_ARG:
      return nullptr;
    }
}

/* Try to convert SUMMARY_BC in the summary to a corresponding bit_contents
   in the caller.

   Return nullptr if the conversion is not possible.  */

const symbolic::bit_contents *
call_summary_replay::
convert_bit_contents_from_summary (const symbolic::bit_contents &summary_bc)
{
  auto &bc_mgr = summary_bc.get_bit_contents_manager ();
  switch (summary_bc.get_kind ())
    {
    default:
      gcc_unreachable ();

    case symbolic::bit_contents::kind::uninit:
    case symbolic::bit_contents::kind::zero:
      return &summary_bc;

    case symbolic::bit_contents::kind::untracked:
      {
	auto &summary_untracked
	  = (const symbolic::untracked_bit_contents &)summary_bc;
	value_set maybe_live_caller;
	for (auto iter : summary_untracked.get_maybe_live ())
	  {
	    if (auto *sval = iter->dyn_cast_svalue ())
	      {
		if (auto *caller_sval = convert_svalue_from_summary (sval))
		  maybe_live_caller.add (caller_sval);
	      }
	    else if (auto *bv = iter->dyn_cast_bit_vector ())
	      {
		if (auto *caller_bv = convert_bit_vector_from_summary (*bv))
		  maybe_live_caller.add (caller_bv);
	      }
	    else
	      gcc_unreachable ();
	  }
	return &bc_mgr.get_untracked (maybe_live_caller);
      }
      break;

    case symbolic::bit_contents::kind::baseline:
      {
	auto &summary_baseline
	  = (const symbolic::baseline_bit_contents &)summary_bc;

	auto &summary_baseline_key = summary_baseline.get_baseline_key ();
	auto *caller_reg
	  = convert_region_from_summary (summary_baseline_key.m_base_reg);
	if (!caller_reg)
	  return nullptr;

	/* Note that caller_reg might not itself be a base region;
	   get its offset within its base region.  */
	auto &rmm = summary_bc.get_node_mgr ().get_region_model_manager ();
	auto caller_mem_offset = caller_reg->get_memory_offset (rmm);

	/* We want the state of base_reg in the caller at the call,
	   rather than at its baseline.  */
	region_model *caller_model = get_caller_model ();
	auto *caller_cluster
	  = caller_model->get_store ()->get_cluster (&caller_mem_offset.get_base_region ());
	if (!caller_cluster)
	  return nullptr;
	auto &caller_bc = caller_cluster->get_bit_contents ();
	return &bc_mgr.get_offset_bit_contents
	  (caller_bc,
	   -caller_mem_offset.get_bit_offset ());
      }
      break;

    case symbolic::bit_contents::kind::conjured:
      return nullptr; // TODO

    case symbolic::bit_contents::kind::named_test:
      return nullptr; // TODO

    case symbolic::bit_contents::kind::concrete_overlay:
      {
	auto &summary_overlay
	  = (const symbolic::concrete_bit_overlay &)summary_bc;
	auto *caller_baseline
	  = convert_bit_contents_from_summary (summary_overlay.get_baseline ());
	if (!caller_baseline)
	  return nullptr;
	auto *caller_result = caller_baseline;
	for (auto iter : summary_overlay.m_concrete_range_map)
	  {
	    auto &summary_bv = iter.second;
	    auto *caller_bv
	      = convert_bit_vector_from_summary (summary_bv);
	    if (!caller_bv)
	      return nullptr;
	    caller_result
	      = &caller_result->set_bits (iter.first, *caller_bv, nullptr);
	  }
	return caller_result;
      }
      break;
    case symbolic::bit_contents::kind::symbolic_overlay:
      {
	auto &summary_overlay
	  = (const symbolic::symbolic_bit_overlay &)summary_bc;
	auto *caller_baseline
	  = convert_bit_contents_from_summary (summary_overlay.get_baseline ());
	if (!caller_baseline)
	  return nullptr;
	symbolic::symbolic_bit_overlay::key caller_key {caller_baseline, {}};
	for (auto iter : summary_overlay.m_symbolic_range_map)
	  {
	    auto &summary_bit_range = iter.first;
	    auto &summary_bv = iter.second;
	    auto *caller_start_offset = convert_scalar_from_summary
	      (summary_bit_range.get_start_offset ());
	    if (!caller_start_offset)
	      return nullptr;
	    auto *caller_size
	      = convert_scalar_from_summary (summary_bit_range.get_size ());
	    if (!caller_size)
	      return nullptr;
	    auto *caller_bv
	      = convert_bit_vector_from_summary (summary_bv);
	    if (!caller_bv)
	      return nullptr;
	    caller_key.m_symbolic_range_map.insert
	      ({symbolic_bit_range (*caller_start_offset, *caller_size),
		*caller_bv});
	  }
	return &bc_mgr.get_symbolic_bit_overlay (std::move (caller_key),
						 nullptr /* FIMXE: uncertainty */);
      }
      break;
    case symbolic::bit_contents::kind::offset:
      return nullptr; // TODO
    }
}

/* Try to convert SUMMARY_BV in the summary to a corresponding bit_vector
   in the caller.

   Return nullptr if the conversion is not possible.  */

const symbolic::bit_vector *
call_summary_replay::
convert_bit_vector_from_summary (const symbolic::bit_vector &summary_bv)
{
  auto &bv_mgr = summary_bv.get_bit_vector_manager ();
  switch (summary_bv.get_kind ())
    {
    default:
      gcc_unreachable ();

    case symbolic::bit_vector::kind::concat:
      return nullptr; // TODO

    case symbolic::bit_vector::kind::conjured:
      {
	auto &summary_conjured_bv
	  = (const symbolic::conjured_bit_vector &)summary_bv;

	auto *caller_id_reg
	  = convert_region_from_summary (summary_conjured_bv.get_id_reg ());
	if (!caller_id_reg)
	  return nullptr;

	conjured_purge p (get_caller_model (), get_ctxt ());
	auto &caller_bv
	  = bv_mgr.get_conjured_bits (summary_conjured_bv.get_stmt (),
				      caller_id_reg,
				      p,
				      summary_conjured_bv.get_idx ());
	return &caller_bv;
      }
      break;
    case symbolic::bit_vector::kind::cropped:
      {
	auto &summary_cropped_bv
	  = (const symbolic::cropped_bit_vector &)summary_bv;
	auto &summary_uncropped_bv
	  = summary_cropped_bv.get_uncropped_bit_vector ();
	auto *caller_uncropped_bv
	  = convert_bit_vector_from_summary (summary_uncropped_bv);
	if (!caller_uncropped_bv)
	  return nullptr;
	auto summary_bit_range
	  = summary_cropped_bv.get_uncropped_bits_to_use ();
	auto *caller_start_offset
	  = convert_scalar_from_summary (summary_bit_range.get_start_offset ());
	if (!caller_start_offset)
	  return nullptr;
	auto *caller_size
	  = convert_scalar_from_summary (summary_bit_range.get_size ());
	return &bv_mgr.get_cropped_bits
	  (*caller_uncropped_bv,
	   symbolic_bit_range (*caller_start_offset,
			       *caller_size));
      }
      break;

    case symbolic::bit_vector::kind::empty:
    case symbolic::bit_vector::kind::named_test:
    case symbolic::bit_vector::kind::poisoned:
      return &summary_bv;

    case symbolic::bit_vector::kind::repeated:
      return nullptr; // TODO

    case symbolic::bit_vector::kind::svalue:
      {
	auto &summary_bv_for_sval
	  = (const symbolic::bit_vector_from_svalue &)summary_bv;
	auto &summary_sval = summary_bv_for_sval.get_svalue ();
	auto *caller_sval = convert_svalue_from_summary (&summary_sval);
	if (!caller_sval)
	  return nullptr;
	return &bv_mgr.get_bit_vector_from_svalue (*caller_sval);
      }
      break;

    case symbolic::bit_vector::kind::from_ubc:
      {
	auto &summary_bv_from_ubc
	  = (const symbolic::bit_vector_from_ubc &)summary_bv;
	auto &summary_bc = summary_bv_from_ubc.get_bit_contents ();
	auto *caller_bc = convert_bit_contents_from_summary (summary_bc);
	if (!caller_bc)
	  return nullptr;

	auto &summary_sym_bit_range = summary_bv_from_ubc.get_bit_range ();
	auto *caller_start_offset
	  = convert_scalar_from_summary (summary_sym_bit_range.get_start_offset ());
	if (!caller_start_offset)
	  return nullptr;
	auto *caller_size
	  = convert_scalar_from_summary (summary_sym_bit_range.get_size ());
	if (!caller_size)
	  return nullptr;
	return &caller_bc->get_bits (symbolic_bit_range (*caller_start_offset,
							 *caller_size));
      }
      break;
    case symbolic::bit_vector::kind::tristate:
      return &summary_bv;
    }
}

template <typename ConcreteValueType>
const symbolic::scalar<ConcreteValueType> *
call_summary_replay::impl_bits_from_bytes (const symbolic::scalar<ConcreteValueType> &summary_scalar)
{
  gcc_unreachable ();
}

template <>
const symbolic_bit_size *
call_summary_replay::impl_bits_from_bytes (const symbolic_bit_size &summary_scalar)
{
  auto &summary_from_bytes
    = (const symbolic::bits_from_bytes_scalar &)summary_scalar;
  auto &summary_bytes = summary_from_bytes.get_bytes ();
  auto *caller_bytes = convert_scalar_from_summary (summary_bytes);
  if (!caller_bytes)
    return nullptr;
  auto &scalar_mgr = summary_scalar.get_mgr ();
  return &scalar_mgr.get_bits_from_bytes (*caller_bytes);
}

template <typename ConcreteValueType>
const symbolic::scalar<ConcreteValueType> *
call_summary_replay::
convert_scalar_from_summary (const symbolic::scalar<ConcreteValueType> &summary_scalar)
{
  switch (summary_scalar.get_kind ())
    {
    default:
      gcc_unreachable ();
    case symbolic::scalar<ConcreteValueType>::kind::concrete:
      return &summary_scalar;

    case symbolic::scalar<ConcreteValueType>::kind::sum:
      {
	auto &summary_sum
	  = (const symbolic::sum<ConcreteValueType> &)summary_scalar;

	auto &scalar_mgr = summary_scalar.get_mgr ();
	std::vector<const symbolic::scalar<ConcreteValueType> *> caller_addends;
	for (size_t i = 0; i < summary_sum.get_num_symbolic_addends (); ++i)
	  {
	    auto &summary_addend = summary_sum.get_symbolic_addend (i);
	    auto *caller_addend = convert_scalar_from_summary (summary_addend);
	    gcc_assert (caller_addend);
	    caller_addends.push_back (caller_addend);
	  }
	if (auto *summary_conc_addend = summary_sum.get_any_concrete_addend ())
	  caller_addends.push_back (summary_conc_addend);
	return &scalar_mgr.get_sum (std::move (caller_addends));
      }
      break;

    case symbolic::scalar<ConcreteValueType>::kind::product:
      {
	auto &summary_product
	  = (const symbolic::product<ConcreteValueType> &)summary_scalar;
	auto *caller_lhs
	  = convert_scalar_from_summary (summary_product.get_lhs ());
	if (!caller_lhs)
	  return nullptr;
	auto *caller_rhs
	  = convert_scalar_from_summary (summary_product.get_rhs ());
	if (!caller_rhs)
	  return nullptr;
	auto &scalar_mgr = summary_scalar.get_mgr ();
	return &scalar_mgr.get_product (*caller_lhs, *caller_rhs);
      }
      break;

    case symbolic::scalar<ConcreteValueType>::kind::from_svalue:
      {
	auto &summary_from_sval
	  = (const symbolic::scalar_from_svalue<ConcreteValueType> &)summary_scalar;
	auto *caller_sval
	  = convert_svalue_from_summary (&summary_from_sval.get_svalue ());
	if (!caller_sval)
	  return nullptr;
	auto &scalar_mgr = summary_scalar.get_mgr ();
	return &scalar_mgr.get_scalar_from_svalue (*caller_sval);
      }
      break;

    case symbolic::scalar<ConcreteValueType>::kind::named_test:
      return &summary_scalar;

    case symbolic::scalar<ConcreteValueType>::kind::bits_from_bytes:
      return impl_bits_from_bytes (summary_scalar);
    }
}

/* Record that SUMMARY_SVAL maps to CALLER_SVAL for this replay.  */

void
call_summary_replay::add_svalue_mapping (const svalue *summary_sval,
					 const svalue *caller_sval)
{
  gcc_assert (summary_sval);
  // CALLER_SVAL can be nullptr
  m_map_svalue_from_summary_to_caller.put (summary_sval, caller_sval);
}

/* Record that SUMMARY_REG maps to CALLER_REG for this replay.  */

void
call_summary_replay::add_region_mapping (const region *summary_reg,
					 const region *caller_reg)
{
  gcc_assert (summary_reg);
  // CALLER_REG can be nullptr
  m_map_region_from_summary_to_caller.put (summary_reg, caller_reg);
}

std::unique_ptr<text_art::tree_widget>
call_summary_replay::make_dump_widget (symbolic::node_dump_state &nds) const
{
  using text_art::tree_widget;
  std::unique_ptr<tree_widget> replay_widget
    (tree_widget::from_fmt (nds, nullptr, "call_summary_replay"));

  replay_widget->add_child (m_cd.make_dump_widget (nds));
  replay_widget->add_child (m_summary.make_dump_widget (nds));

  /* Current state of caller (could be in mid-update).  */
  {
    auto w = tree_widget::from_fmt (nds, nullptr, "Current state of caller");
    w->add_child (m_cd.get_model ()->make_dump_widget (nds));
    replay_widget->add_child (std::move (w));
  }

  {
    auto details_widget = tree_widget::from_fmt (nds, nullptr, "Replay state");

    {
      auto sval_map_widget = tree_widget::from_fmt
	(nds, nullptr, "svalue mappings from summary to caller:");

      auto_vec <const svalue *> summary_svals;
      for (auto kv : m_map_svalue_from_summary_to_caller)
	summary_svals.safe_push (kv.first);
      summary_svals.qsort (svalue::cmp_ptr_ptr);
      for (auto summary_sval : summary_svals)
	{
	  auto in_summary_widget = tree_widget::from_fmt
	    (nds, nullptr, "sval in summary:");
	  in_summary_widget->add_child (summary_sval->make_dump_widget (nds));
	  sval_map_widget->add_child (std::move (in_summary_widget));

	  const svalue *caller_sval
	    = *((const_cast<svalue_map_t &>
		 (m_map_svalue_from_summary_to_caller)).get (summary_sval));
	  auto in_caller_widget = tree_widget::from_fmt
	    (nds, nullptr, "sval in caller:");
	  in_caller_widget->add_child (caller_sval->make_dump_widget (nds));
	  sval_map_widget->add_child (std::move (in_caller_widget));
	}

      details_widget->add_child (std::move (sval_map_widget));
    }

    {
      auto reg_map_widget = tree_widget::from_fmt
	(nds, nullptr, "region mappings from summary to caller:");

      auto_vec <const region *> summary_regs;
      for (auto kv : m_map_region_from_summary_to_caller)
	summary_regs.safe_push (kv.first);
      summary_regs.qsort (region::cmp_ptr_ptr);
      for (auto summary_reg : summary_regs)
	{
	  auto in_summary_widget = tree_widget::from_fmt
	    (nds, nullptr, "reg in summary:");
	  if (summary_reg)
	    in_summary_widget->add_child (summary_reg->make_dump_widget (nds));
	  else
	    in_summary_widget->add_child
	      (tree_widget::from_fmt (nds, nullptr, "(null)"));
	  reg_map_widget->add_child (std::move (in_summary_widget));

	  const region *caller_reg
	    = *((const_cast<region_map_t &>
		 (m_map_region_from_summary_to_caller)).get (summary_reg));
	  auto in_caller_widget = tree_widget::from_fmt
	    (nds, nullptr, "reg in caller:");
	  if (caller_reg)
	    in_caller_widget->add_child (caller_reg->make_dump_widget (nds));
	  else
	    in_caller_widget->add_child
	      (tree_widget::from_fmt (nds, nullptr, "(null)"));
	  reg_map_widget->add_child (std::move (in_caller_widget));
	}

      details_widget->add_child (std::move (reg_map_widget));
    }

    replay_widget->add_child (std::move (details_widget));
  }

  return replay_widget;
}

/* Dump a tree-like representation of this object to stderr.  */

DEBUG_FUNCTION void
call_summary_replay::dump () const
{
  symbolic::dump_node_tree_to_stderr<call_summary_replay> (*this);
}

/* Dump a tree-like representation of this object to LOG.  */

void
call_summary_replay::dump_to_logger (logger &log) const
{
  symbolic::dump_node_tree_to_logger<call_summary_replay> (*this, log);
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
