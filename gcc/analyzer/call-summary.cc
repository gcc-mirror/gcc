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

#include "config.h"
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "tree-dfa.h"
#include "diagnostic-core.h"
#include "diagnostic.h"
#include "tree-diagnostic.h"
#include "analyzer/analyzer.h"
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
  return m_enode->get_point ().get_fndecl ();
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
	    = get_state ().m_region_model->get_lvalue (result, NULL);
	  const svalue *result_sval
	    = get_state ().m_region_model->get_store_value (result_reg, NULL);
	  switch (result_sval->get_kind ())
	    {
	    default:
	      break;
	    case SK_REGION:
	      {
		const region_svalue *region_sval
		  = as_a <const region_svalue *> (result_sval);
		const region *pointee_reg = region_sval->get_pointee ();
		switch (pointee_reg->get_kind ())
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

/* Dump a multiline representation of this object to PP.  */

void
call_summary::dump_to_pp (const extrinsic_state &ext_state,
			  pretty_printer *pp,
			  bool simple) const
{
  label_text desc = get_desc ();
  pp_printf (pp, "desc: %qs", desc.get ());
  pp_newline (pp);

  get_state ().dump_to_pp (ext_state, simple, true, pp);
}

/* Dump a multiline representation of this object to FILE.  */

void
call_summary::dump (const extrinsic_state &ext_state,
		    FILE *fp,
		    bool simple) const
{
  tree_dump_pretty_printer pp (fp);
  dump_to_pp (ext_state, &pp, simple);
}

/* Dump a multiline representation of this object to stderr.  */

DEBUG_FUNCTION void
call_summary::dump (const extrinsic_state &ext_state, bool simple) const
{
  dump (ext_state, stderr, simple);
}

/* class call_summary_replay.  */

/* call_summary_replay's ctor.
   Populate the cache with params for the summary based on
   arguments at the caller. */

call_summary_replay::call_summary_replay (const call_details &cd,
					  const function &called_fn,
					  call_summary *summary,
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
    = mgr->get_frame_region (NULL, called_fn);

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

   Return NULL if the conversion is not possible.  */

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
    case SK_REGION:
      {
	const region_svalue *region_summary_sval
	  = as_a <const region_svalue *> (summary_sval);
	const region *summary_reg = region_summary_sval->get_pointee ();
	const region *caller_reg = convert_region_from_summary (summary_reg);
	if (!caller_reg)
	  return NULL;
	region_model_manager *mgr = get_manager ();
	const svalue *caller_ptr
	  = mgr->get_ptr_svalue (summary_sval->get_type (),
				 caller_reg);
	return caller_ptr;
      }
      break;

    case SK_CONSTANT:
    case SK_PLACEHOLDER:
    case SK_POISONED:
    case SK_UNKNOWN:
      return summary_sval;

    case SK_SETJMP:
      return NULL; // TODO

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
	  return NULL;
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
	  return NULL;
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
	  return NULL;
	const svalue *summary_arg1 = binop_summary_sval->get_arg1 ();
	const svalue *caller_arg1 = convert_svalue_from_summary (summary_arg1);
	if (!caller_arg1)
	  return NULL;
	region_model_manager *mgr = get_manager ();
	return mgr->get_or_create_binop (summary_sval->get_type (),
					 binop_summary_sval->get_op (),
					 caller_arg0,
					 caller_arg1);
      }
      break;
    case SK_SUB:
      {
	const sub_svalue *sub_summary_sval
	  = as_a <const sub_svalue *> (summary_sval);
	region_model_manager *mgr = get_manager ();
	const svalue *summary_parent_sval = sub_summary_sval->get_parent ();
	if (!summary_parent_sval)
	  return NULL;
	const region *summary_subregion = sub_summary_sval->get_subregion ();
	if (!summary_subregion)
	  return NULL;
	return mgr->get_or_create_sub_svalue (summary_sval->get_type (),
					      summary_parent_sval,
					      summary_subregion);
      }
      break;
    case SK_REPEATED:
      {
	const repeated_svalue *repeated_summary_sval
	  = as_a <const repeated_svalue *> (summary_sval);
	const svalue *summary_outer_size
	  = repeated_summary_sval->get_outer_size ();
	const svalue *caller_outer_size
	  = convert_svalue_from_summary (summary_outer_size);
	if (!caller_outer_size)
	  return NULL;
	const svalue *summary_inner_sval
	  = repeated_summary_sval->get_inner_svalue ();
	const svalue *caller_inner_sval
	  = convert_svalue_from_summary (summary_inner_sval);
	if (!caller_inner_sval)
	  return NULL;
	region_model_manager *mgr = get_manager ();
	return mgr->get_or_create_repeated_svalue (summary_sval->get_type (),
						   caller_outer_size,
						   caller_inner_sval);
      }
      break;
    case SK_BITS_WITHIN:
      {
	const bits_within_svalue *bits_within_summary_sval
	  = as_a <const bits_within_svalue *> (summary_sval);
	const bit_range &bits = bits_within_summary_sval->get_bits ();
	const svalue *summary_inner_sval
	  = bits_within_summary_sval->get_inner_svalue ();
	const svalue *caller_inner_sval
	  = convert_svalue_from_summary (summary_inner_sval);
	if (!caller_inner_sval)
	  return NULL;
	region_model_manager *mgr = get_manager ();
	return mgr->get_or_create_bits_within (summary_sval->get_type (),
					       bits,
					       caller_inner_sval);
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
	  return NULL;
	region_model_manager *mgr = get_manager ();
	return mgr->get_or_create_unmergeable (caller_arg_sval);
      }
      break;
    case SK_WIDENING:
      {
	const widening_svalue *widening_summary_sval
	  = as_a <const widening_svalue *> (summary_sval);
	const function_point &point = widening_summary_sval->get_point ();
	const svalue *summary_base_sval
	  = widening_summary_sval->get_base_svalue ();
	const svalue *caller_base_sval
	  = convert_svalue_from_summary (summary_base_sval);
	if (!(caller_base_sval
	      && caller_base_sval->can_have_associated_state_p ()))
	  return NULL;
	const svalue *summary_iter_sval
	  = widening_summary_sval->get_iter_svalue ();
	const svalue *caller_iter_sval
	  = convert_svalue_from_summary (summary_iter_sval);
	if (!(caller_iter_sval
	      && caller_iter_sval->can_have_associated_state_p ()))
	  return NULL;
	region_model_manager *mgr = get_manager ();
	return mgr->get_or_create_widening_svalue
	  (summary_iter_sval->get_type (),
	   point,
	   caller_base_sval,
	   caller_iter_sval);
      }
      break;
    case SK_COMPOUND:
      {
	const compound_svalue *compound_summary_sval
	  = as_a <const compound_svalue *> (summary_sval);
	region_model_manager *mgr = get_manager ();
	store_manager *store_mgr = mgr->get_store_manager ();
	binding_map caller_map;
	auto_vec <const binding_key *> summary_keys;
	for (auto kv : *compound_summary_sval)
	  summary_keys.safe_push (kv.first);
	summary_keys.qsort (binding_key::cmp_ptrs);
	for (auto key : summary_keys)
	  {
	    gcc_assert (key->concrete_p ());
	    /* No remapping is needed for concrete binding keys.  */

	    const svalue *bound_summary_sval
	      = compound_summary_sval->get_map ().get (key);
	    const svalue *caller_sval
	      = convert_svalue_from_summary (bound_summary_sval);
	    if (!caller_sval)
	      caller_sval = mgr->get_or_create_unknown_svalue (NULL_TREE);

	    if (const compound_svalue *inner_compound_sval
		= caller_sval->dyn_cast_compound_svalue ())
	      {
		const concrete_binding *outer_key
		  = as_a <const concrete_binding *> (key);
		for (auto inner_kv : *inner_compound_sval)
		  {
		    // These should already be mapped to the caller.
		    const binding_key *inner_key = inner_kv.first;
		    const svalue *inner_sval = inner_kv.second;
		    gcc_assert (inner_key->concrete_p ());
		    const concrete_binding *concrete_key
		      = as_a <const concrete_binding *> (inner_key);
		    bit_offset_t effective_start
		      = (concrete_key->get_start_bit_offset ()
			 + outer_key->get_start_bit_offset ());
		    const concrete_binding *effective_concrete_key
		      = store_mgr->get_concrete_binding
			  (effective_start,
			   concrete_key->get_size_in_bits ());
		    caller_map.put (effective_concrete_key, inner_sval);
		  }
	      }
	    else
	      caller_map.put (key, caller_sval);
	  }
	return mgr->get_or_create_compound_svalue (summary_sval->get_type (),
						   caller_map);
      }
      break;
    case SK_CONJURED:
      {
	region_model_manager *mgr = get_manager ();
	return mgr->get_or_create_unknown_svalue (summary_sval->get_type ());
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
	      return NULL;
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
	      return NULL;
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

   Return NULL if the conversion is not possible.  */

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
      /* These should never be pointed to by a region_svalue.  */
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
	  return NULL;
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
	    return NULL;
	  case VAR_DECL:
	    /* We don't care about writes to locals within
	       the summary.  */
	    if (is_global_var (decl))
	      /* If it's a global, we can reuse the region directly.  */
	      return summary_reg;
	    else
	      /* Otherwise, we don't care about locals.  */
	      return NULL;
	  case RESULT_DECL:
	    return m_cd.get_lhs_region ();
	  case PARM_DECL:
	    /* Writes (by value) to parms should be visible to the caller.  */
	    return NULL;
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
	  return NULL;
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
	  return NULL;
	const svalue *summary_index = summary_element_reg->get_index ();
	const svalue *caller_index
	  = convert_svalue_from_summary (summary_index);
	if (!caller_index)
	  return NULL;
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
	  return NULL;
	const svalue *summary_byte_offset
	  = summary_offset_reg->get_byte_offset ();
	const svalue *caller_byte_offset
	  = convert_svalue_from_summary (summary_byte_offset);
	if (!caller_byte_offset)
	  return NULL;
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
	  return NULL;
	const svalue *summary_byte_size
	  = summary_sized_reg->get_byte_size_sval (mgr);
	const svalue *caller_byte_size
	  = convert_svalue_from_summary (summary_byte_size);
	if (!caller_byte_size)
	  return NULL;
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
	  return NULL;
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
      return NULL;
    case RK_BIT_RANGE:
      {
	const bit_range_region *summary_bit_range_reg
	  = as_a <const bit_range_region *> (summary_reg);
	const region *summary_parent_reg = summary_reg->get_parent_region ();
	const region *caller_parent_reg
	  = convert_region_from_summary (summary_parent_reg);
	if (!caller_parent_reg)
	  return NULL;
	const bit_range &bits = summary_bit_range_reg->get_bits ();
	return mgr->get_bit_range (caller_parent_reg,
				   summary_reg->get_type (),
				   bits);
      }
      break;
    case RK_VAR_ARG:
      return NULL;
    }
}

/* Try to convert SUMMARY_KEY in the summary to a corresponding binding key
   in the caller.

   Return NULL if the conversion is not possible.  */

const binding_key *
call_summary_replay::convert_key_from_summary (const binding_key *summary_key)
{
  if (summary_key->concrete_p ())
    return summary_key;

  const symbolic_binding *symbolic_key = (const symbolic_binding *)summary_key;
  const region *summary_reg = symbolic_key->get_region ();
  const region *caller_reg = convert_region_from_summary (summary_reg);
  if (!caller_reg)
    return NULL;
  region_model_manager *mgr = get_manager ();
  store_manager *store_mgr = mgr->get_store_manager ();
  return store_mgr->get_symbolic_binding (caller_reg);
}

/* Record that SUMMARY_SVAL maps to CALLER_SVAL for this replay.  */

void
call_summary_replay::add_svalue_mapping (const svalue *summary_sval,
					 const svalue *caller_sval)
{
  gcc_assert (summary_sval);
  // CALLER_SVAL can be NULL
  m_map_svalue_from_summary_to_caller.put (summary_sval, caller_sval);
}

/* Record that SUMMARY_REG maps to CALLER_REG for this replay.  */

void
call_summary_replay::add_region_mapping (const region *summary_reg,
					 const region *caller_reg)
{
  gcc_assert (summary_reg);
  // CALLER_REG can be NULL
  m_map_region_from_summary_to_caller.put (summary_reg, caller_reg);
}

/* Dump a multiline representation of this object to PP.  */

void
call_summary_replay::dump_to_pp (pretty_printer *pp, bool simple) const
{
  pp_newline (pp);
  pp_string (pp, "CALL DETAILS:");
  pp_newline (pp);
  m_cd.dump_to_pp (pp, simple);

  pp_newline (pp);
  pp_string (pp, "CALLEE SUMMARY:");
  pp_newline (pp);
  m_summary->dump_to_pp (m_ext_state, pp, simple);

  /* Current state of caller (could be in mid-update).  */
  pp_newline (pp);
  pp_string (pp, "CALLER:");
  pp_newline (pp);
  m_cd.get_model ()->dump_to_pp (pp, simple, true);

  pp_newline (pp);
  pp_string (pp, "REPLAY STATE:");
  pp_newline (pp);
  pp_string (pp, "svalue mappings from summary to caller:");
  pp_newline (pp);
  auto_vec <const svalue *> summary_svals;
  for (auto kv : m_map_svalue_from_summary_to_caller)
    summary_svals.safe_push (kv.first);
  summary_svals.qsort (svalue::cmp_ptr_ptr);
  for (auto summary_sval : summary_svals)
    {
      pp_string (pp, "sval in summary: ");
      summary_sval->dump_to_pp (pp, simple);
      pp_newline (pp);

      const svalue *caller_sval
	= *((const_cast<svalue_map_t &>
	     (m_map_svalue_from_summary_to_caller)).get (summary_sval));
      pp_string (pp, " sval in caller: ");
      caller_sval->dump_to_pp (pp, simple);
      pp_newline (pp);
    }

  pp_newline (pp);
  pp_string (pp, "region mappings from summary to caller:");
  pp_newline (pp);
  auto_vec <const region *> summary_regs;
  for (auto kv : m_map_region_from_summary_to_caller)
    summary_regs.safe_push (kv.first);
  summary_regs.qsort (region::cmp_ptr_ptr);
  for (auto summary_reg : summary_regs)
    {
      pp_string (pp, "reg in summary: ");
      if (summary_reg)
	summary_reg->dump_to_pp (pp, simple);
      else
	pp_string (pp, "(null)");
      pp_newline (pp);

      const region *caller_reg
	= *((const_cast<region_map_t &>
	     (m_map_region_from_summary_to_caller)).get (summary_reg));
      pp_string (pp, " reg in caller: ");
      if (caller_reg)
	caller_reg->dump_to_pp (pp, simple);
      else
	pp_string (pp, "(null)");
      pp_newline (pp);
    }
}

/* Dump a multiline representation of this object to FILE.  */

void
call_summary_replay::dump (FILE *fp, bool simple) const
{
  tree_dump_pretty_printer pp (fp);
  dump_to_pp (&pp, simple);
}

/* Dump a multiline representation of this object to stderr.  */

DEBUG_FUNCTION void
call_summary_replay::dump (bool simple) const
{
  dump (stderr, simple);
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
