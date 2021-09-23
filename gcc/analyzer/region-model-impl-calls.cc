/* Handling for the known behavior of various specific functions.
   Copyright (C) 2020-2021 Free Software Foundation, Inc.
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
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "diagnostic-core.h"
#include "graphviz.h"
#include "options.h"
#include "cgraph.h"
#include "tree-dfa.h"
#include "stringpool.h"
#include "convert.h"
#include "target.h"
#include "fold-const.h"
#include "tree-pretty-print.h"
#include "diagnostic-color.h"
#include "diagnostic-metadata.h"
#include "tristate.h"
#include "bitmap.h"
#include "selftest.h"
#include "function.h"
#include "json.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "ordered-hash-map.h"
#include "options.h"
#include "cgraph.h"
#include "cfg.h"
#include "digraph.h"
#include "analyzer/supergraph.h"
#include "sbitmap.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"
#include "analyzer/call-info.h"
#include "gimple-pretty-print.h"

#if ENABLE_ANALYZER

namespace ana {

/* class call_details.  */

/* call_details's ctor.  */

call_details::call_details (const gcall *call, region_model *model,
			    region_model_context *ctxt)
: m_call (call), m_model (model), m_ctxt (ctxt),
  m_lhs_type (NULL_TREE), m_lhs_region (NULL)
{
  m_lhs_type = NULL_TREE;
  if (tree lhs = gimple_call_lhs (call))
    {
      m_lhs_region = model->get_lvalue (lhs, ctxt);
      m_lhs_type = TREE_TYPE (lhs);
    }
}

/* Get any uncertainty_t associated with the region_model_context.  */

uncertainty_t *
call_details::get_uncertainty () const
{
  if (m_ctxt)
    return m_ctxt->get_uncertainty ();
  else
    return NULL;
}

/* If the callsite has a left-hand-side region, set it to RESULT
   and return true.
   Otherwise do nothing and return false.  */

bool
call_details::maybe_set_lhs (const svalue *result) const
{
  gcc_assert (result);
  if (m_lhs_region)
    {
      m_model->set_value (m_lhs_region, result, m_ctxt);
      return true;
    }
  else
    return false;
}

/* Return the number of arguments used by the call statement.  */

unsigned
call_details::num_args () const
{
  return gimple_call_num_args (m_call);
}

/* Get argument IDX at the callsite as a tree.  */

tree
call_details::get_arg_tree (unsigned idx) const
{
  return gimple_call_arg (m_call, idx);
}

/* Get the type of argument IDX.  */

tree
call_details::get_arg_type (unsigned idx) const
{
  return TREE_TYPE (gimple_call_arg (m_call, idx));
}

/* Get argument IDX at the callsite as an svalue.  */

const svalue *
call_details::get_arg_svalue (unsigned idx) const
{
  tree arg = get_arg_tree (idx);
  return m_model->get_rvalue (arg, m_ctxt);
}

/* Attempt to get the string literal for argument IDX, or return NULL
   otherwise.
   For use when implementing "__analyzer_*" functions that take
   string literals.  */

const char *
call_details::get_arg_string_literal (unsigned idx) const
{
  const svalue *str_arg = get_arg_svalue (idx);
  if (const region *pointee = str_arg->maybe_get_region ())
    if (const string_region *string_reg = pointee->dyn_cast_string_region ())
      {
	tree string_cst = string_reg->get_string_cst ();
	return TREE_STRING_POINTER (string_cst);
      }
  return NULL;
}

/* Attempt to get the fndecl used at this call, if known, or NULL_TREE
   otherwise.  */

tree
call_details::get_fndecl_for_call () const
{
  return m_model->get_fndecl_for_call (m_call, m_ctxt);
}

/* Dump a multiline representation of this call to PP.  */

void
call_details::dump_to_pp (pretty_printer *pp, bool simple) const
{
  pp_string (pp, "gcall: ");
  pp_gimple_stmt_1 (pp, m_call, 0 /* spc */, TDF_NONE /* flags */);
  pp_newline (pp);
  pp_string (pp, "return region: ");
  if (m_lhs_region)
    m_lhs_region->dump_to_pp (pp, simple);
  else
    pp_string (pp, "NULL");
  pp_newline (pp);
  for (unsigned i = 0; i < gimple_call_num_args (m_call); i++)
    {
      const svalue *arg_sval = get_arg_svalue (i);
      pp_printf (pp, "arg %i: ", i);
      arg_sval->dump_to_pp (pp, simple);
      pp_newline (pp);
    }
}

/* Dump a multiline representation of this call to stderr.  */

DEBUG_FUNCTION void
call_details::dump (bool simple) const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_show_color (&pp) = pp_show_color (global_dc->printer);
  pp.buffer->stream = stderr;
  dump_to_pp (&pp, simple);
  pp_flush (&pp);
}

/* Get a conjured_svalue for this call for REG.  */

const svalue *
call_details::get_or_create_conjured_svalue (const region *reg) const
{
  region_model_manager *mgr = m_model->get_manager ();
  return mgr->get_or_create_conjured_svalue (reg->get_type (), m_call, reg);
}

/* Implementations of specific functions.  */

/* Handle the on_call_pre part of "alloca".  */

void
region_model::impl_call_alloca (const call_details &cd)
{
  const svalue *size_sval = cd.get_arg_svalue (0);
  const region *new_reg = create_region_for_alloca (size_sval);
  const svalue *ptr_sval
    = m_mgr->get_ptr_svalue (cd.get_lhs_type (), new_reg);
  cd.maybe_set_lhs (ptr_sval);
}

/* Handle a call to "__analyzer_describe".

   Emit a warning describing the 2nd argument (which can be of any
   type), at the given verbosity level.  This is for use when
   debugging, and may be of use in DejaGnu tests.  */

void
region_model::impl_call_analyzer_describe (const gcall *call,
					   region_model_context *ctxt)
{
  tree t_verbosity = gimple_call_arg (call, 0);
  tree t_val = gimple_call_arg (call, 1);
  const svalue *sval = get_rvalue (t_val, ctxt);
  bool simple = zerop (t_verbosity);
  label_text desc = sval->get_desc (simple);
  warning_at (call->location, 0, "svalue: %qs", desc.m_buffer);
}

/* Handle a call to "__analyzer_dump_capacity".

   Emit a warning describing the capacity of the base region of
   the region pointed to by the 1st argument.
   This is for use when debugging, and may be of use in DejaGnu tests.  */

void
region_model::impl_call_analyzer_dump_capacity (const gcall *call,
						region_model_context *ctxt)
{
  tree t_ptr = gimple_call_arg (call, 0);
  const svalue *sval_ptr = get_rvalue (t_ptr, ctxt);
  const region *reg = deref_rvalue (sval_ptr, t_ptr, ctxt);
  const region *base_reg = reg->get_base_region ();
  const svalue *capacity = get_capacity (base_reg);
  label_text desc = capacity->get_desc (true);
  warning_at (call->location, 0, "capacity: %qs", desc.m_buffer);
}

/* Handle a call to "__analyzer_eval" by evaluating the input
   and dumping as a dummy warning, so that test cases can use
   dg-warning to validate the result (and so unexpected warnings will
   lead to DejaGnu failures).
   Broken out as a subroutine to make it easier to put a breakpoint on it
   - though typically this doesn't help, as we have an SSA name as the arg,
   and what's more interesting is usually the def stmt for that name.  */

void
region_model::impl_call_analyzer_eval (const gcall *call,
				       region_model_context *ctxt)
{
  tree t_arg = gimple_call_arg (call, 0);
  tristate t = eval_condition (t_arg, NE_EXPR, integer_zero_node, ctxt);
  warning_at (call->location, 0, "%s", t.as_string ());
}

/* Handle the on_call_pre part of "__builtin_expect" etc.  */

void
region_model::impl_call_builtin_expect (const call_details &cd)
{
  /* __builtin_expect's return value is its initial argument.  */
  const svalue *sval = cd.get_arg_svalue (0);
  cd.maybe_set_lhs (sval);
}

/* Handle the on_call_pre part of "calloc".  */

void
region_model::impl_call_calloc (const call_details &cd)
{
  const svalue *nmemb_sval = cd.get_arg_svalue (0);
  const svalue *size_sval = cd.get_arg_svalue (1);
  /* TODO: check for overflow here?  */
  const svalue *prod_sval
    = m_mgr->get_or_create_binop (size_type_node, MULT_EXPR,
				  nmemb_sval, size_sval);
  const region *new_reg = create_region_for_heap_alloc (prod_sval);
  zero_fill_region (new_reg);
  if (cd.get_lhs_type ())
    {
      const svalue *ptr_sval
	= m_mgr->get_ptr_svalue (cd.get_lhs_type (), new_reg);
      cd.maybe_set_lhs (ptr_sval);
    }
}

/* Handle the on_call_pre part of "error" and "error_at_line" from
   GNU's non-standard <error.h>.
   MIN_ARGS identifies the minimum number of expected arguments
   to be consistent with such a call (3 and 5 respectively).
   Return true if handling it as one of these functions.
   Write true to *OUT_TERMINATE_PATH if this execution path should be
   terminated (e.g. the function call terminates the process).  */

bool
region_model::impl_call_error (const call_details &cd, unsigned min_args,
			       bool *out_terminate_path)
{
  /* Bail if not enough args.  */
  if (cd.num_args () < min_args)
    return false;

  /* Initial argument ought to be of type "int".  */
  if (cd.get_arg_type (0) != integer_type_node)
    return false;

  /* The process exits if status != 0, so it only continues
     for the case where status == 0.
     Add that constraint, or terminate this analysis path.  */
  tree status = cd.get_arg_tree (0);
  if (!add_constraint (status, EQ_EXPR, integer_zero_node, cd.get_ctxt ()))
    *out_terminate_path = true;

  return true;
}

/* Handle the on_call_pre part of "fgets" and "fgets_unlocked".  */

void
region_model::impl_call_fgets (const call_details &cd)
{
  /* Ideally we would bifurcate state here between the
     error vs no error cases.  */
  const svalue *ptr_sval = cd.get_arg_svalue (0);
  if (const region *reg = ptr_sval->maybe_get_region ())
    {
      const region *base_reg = reg->get_base_region ();
      const svalue *new_sval = cd.get_or_create_conjured_svalue (base_reg);
      purge_state_involving (new_sval, cd.get_ctxt ());
      set_value (base_reg, new_sval, cd.get_ctxt ());
    }
}

/* Handle the on_call_pre part of "fread".  */

void
region_model::impl_call_fread (const call_details &cd)
{
  const svalue *ptr_sval = cd.get_arg_svalue (0);
  if (const region *reg = ptr_sval->maybe_get_region ())
    {
      const region *base_reg = reg->get_base_region ();
      const svalue *new_sval = cd.get_or_create_conjured_svalue (base_reg);
      purge_state_involving (new_sval, cd.get_ctxt ());
      set_value (base_reg, new_sval, cd.get_ctxt ());
    }
}

/* Handle the on_call_post part of "free", after sm-handling.

   If the ptr points to an underlying heap region, delete the region,
   poisoning pointers to it and regions within it.

   We delay this until after sm-state has been updated so that the
   sm-handling can transition all of the various casts of the pointer
   to a "freed" state *before* we delete the related region here.

   This has to be done here so that the sm-handling can use the fact
   that they point to the same region to establish that they are equal
   (in region_model::eval_condition_without_cm), and thus transition
   all pointers to the region to the "freed" state together, regardless
   of casts.  */

void
region_model::impl_call_free (const call_details &cd)
{
  const svalue *ptr_sval = cd.get_arg_svalue (0);
  if (const region *freed_reg = ptr_sval->maybe_get_region ())
    {
      /* If the ptr points to an underlying heap region, delete it,
	 poisoning pointers.  */
      unbind_region_and_descendents (freed_reg, POISON_KIND_FREED);
      m_dynamic_extents.remove (freed_reg);
    }
}

/* Handle the on_call_pre part of "malloc".  */

void
region_model::impl_call_malloc (const call_details &cd)
{
  const svalue *size_sval = cd.get_arg_svalue (0);
  const region *new_reg = create_region_for_heap_alloc (size_sval);
  if (cd.get_lhs_type ())
    {
      const svalue *ptr_sval
	= m_mgr->get_ptr_svalue (cd.get_lhs_type (), new_reg);
      cd.maybe_set_lhs (ptr_sval);
    }
}

/* Handle the on_call_pre part of "memcpy" and "__builtin_memcpy".  */

void
region_model::impl_call_memcpy (const call_details &cd)
{
  const svalue *dest_sval = cd.get_arg_svalue (0);
  const svalue *num_bytes_sval = cd.get_arg_svalue (2);

  const region *dest_reg = deref_rvalue (dest_sval, cd.get_arg_tree (0),
					 cd.get_ctxt ());

  cd.maybe_set_lhs (dest_sval);

  if (tree num_bytes = num_bytes_sval->maybe_get_constant ())
    {
      /* "memcpy" of zero size is a no-op.  */
      if (zerop (num_bytes))
	return;
    }

  check_region_for_write (dest_reg, cd.get_ctxt ());

  /* Otherwise, mark region's contents as unknown.  */
  mark_region_as_unknown (dest_reg, cd.get_uncertainty ());
}

/* Handle the on_call_pre part of "memset" and "__builtin_memset".  */

void
region_model::impl_call_memset (const call_details &cd)
{
  const svalue *dest_sval = cd.get_arg_svalue (0);
  const svalue *fill_value_sval = cd.get_arg_svalue (1);
  const svalue *num_bytes_sval = cd.get_arg_svalue (2);

  const region *dest_reg = deref_rvalue (dest_sval, cd.get_arg_tree (0),
					  cd.get_ctxt ());

  const svalue *fill_value_u8
    = m_mgr->get_or_create_cast (unsigned_char_type_node, fill_value_sval);

  const region *sized_dest_reg = m_mgr->get_sized_region (dest_reg,
							  NULL_TREE,
							  num_bytes_sval);
  check_region_for_write (sized_dest_reg, cd.get_ctxt ());
  fill_region (sized_dest_reg, fill_value_u8);
}

/* Handle the on_call_pre part of "operator new".  */

void
region_model::impl_call_operator_new (const call_details &cd)
{
  const svalue *size_sval = cd.get_arg_svalue (0);
  const region *new_reg = create_region_for_heap_alloc (size_sval);
  if (cd.get_lhs_type ())
    {
      const svalue *ptr_sval
	= m_mgr->get_ptr_svalue (cd.get_lhs_type (), new_reg);
      cd.maybe_set_lhs (ptr_sval);
    }
}

/* Handle the on_call_pre part of "operator delete", which comes in
   both sized and unsized variants (2 arguments and 1 argument
   respectively).  */

void
region_model::impl_call_operator_delete (const call_details &cd)
{
  const svalue *ptr_sval = cd.get_arg_svalue (0);
  if (const region *freed_reg = ptr_sval->maybe_get_region ())
    {
      /* If the ptr points to an underlying heap region, delete it,
	 poisoning pointers.  */
      unbind_region_and_descendents (freed_reg, POISON_KIND_FREED);
    }
}

/* Handle the on_call_post part of "realloc":

     void *realloc(void *ptr, size_t size);

   realloc(3) is awkward, since it has various different outcomes
   that are best modelled as separate exploded nodes/edges.

   We first check for sm-state, in
   malloc_state_machine::on_realloc_call, so that we
   can complain about issues such as realloc of a non-heap
   pointer, and terminate the path for such cases (and issue
   the complaints at the call's exploded node).

   Assuming that these checks pass, we split the path here into
   three special cases (and terminate the "standard" path):
   (A) failure, returning NULL
   (B) success, growing the buffer in-place without moving it
   (C) success, allocating a new buffer, copying the content
   of the old buffer to it, and freeing the old buffer.

   Each of these has a custom_edge_info subclass, which updates
   the region_model and sm-state of the destination state.  */

void
region_model::impl_call_realloc (const call_details &cd)
{
  /* Three custom subclasses of custom_edge_info, for handling the various
     outcomes of "realloc".  */

  /* Concrete custom_edge_info: a realloc call that fails, returning NULL.  */
  class failure : public failed_call_info
  {
  public:
    failure (const call_details &cd)
    : failed_call_info (cd)
    {
    }

    bool update_model (region_model *model,
		       const exploded_edge *,
		       region_model_context *ctxt) const FINAL OVERRIDE
    {
      /* Return NULL; everything else is unchanged.  */
      const call_details cd (get_call_details (model, ctxt));
      if (cd.get_lhs_type ())
	{
	  const svalue *zero
	    = model->m_mgr->get_or_create_int_cst (cd.get_lhs_type (), 0);
	  model->set_value (cd.get_lhs_region (),
			    zero,
			    cd.get_ctxt ());
	}
      return true;
    }
  };

  /* Concrete custom_edge_info: a realloc call that succeeds, growing
     the existing buffer without moving it.  */
  class success_no_move : public call_info
  {
  public:
    success_no_move (const call_details &cd)
    : call_info (cd)
    {
    }

    label_text get_desc (bool can_colorize) const FINAL OVERRIDE
    {
      return make_label_text (can_colorize,
			      "when %qE succeeds, without moving buffer",
			      get_fndecl ());
    }

    bool update_model (region_model *model,
		       const exploded_edge *,
		       region_model_context *ctxt) const FINAL OVERRIDE
    {
      /* Update size of buffer and return the ptr unchanged.  */
      const call_details cd (get_call_details (model, ctxt));
      const svalue *ptr_sval = cd.get_arg_svalue (0);
      const svalue *size_sval = cd.get_arg_svalue (1);
      if (const region *buffer_reg = ptr_sval->maybe_get_region ())
	if (compat_types_p (size_sval->get_type (), size_type_node))
	  model->set_dynamic_extents (buffer_reg, size_sval);
      if (cd.get_lhs_region ())
	{
	  model->set_value (cd.get_lhs_region (), ptr_sval, cd.get_ctxt ());
	  const svalue *zero
	    = model->m_mgr->get_or_create_int_cst (cd.get_lhs_type (), 0);
	  return model->add_constraint (ptr_sval, NE_EXPR, zero, cd.get_ctxt ());
	}
      else
	return true;
    }
  };

  /* Concrete custom_edge_info: a realloc call that succeeds, freeing
     the existing buffer and moving the content to a freshly allocated
     buffer.  */
  class success_with_move : public call_info
  {
  public:
    success_with_move (const call_details &cd)
    : call_info (cd)
    {
    }

    label_text get_desc (bool can_colorize) const FINAL OVERRIDE
    {
      return make_label_text (can_colorize,
			      "when %qE succeeds, moving buffer",
			      get_fndecl ());
    }
    bool update_model (region_model *model,
		       const exploded_edge *,
		       region_model_context *ctxt) const FINAL OVERRIDE
    {
      const call_details cd (get_call_details (model, ctxt));
      const svalue *old_ptr_sval = cd.get_arg_svalue (0);
      const svalue *new_size_sval = cd.get_arg_svalue (1);

      /* Create the new region.  */
      const region *new_reg
	= model->create_region_for_heap_alloc (new_size_sval);
      const svalue *new_ptr_sval
	= model->m_mgr->get_ptr_svalue (cd.get_lhs_type (), new_reg);
      if (cd.get_lhs_type ())
	cd.maybe_set_lhs (new_ptr_sval);

      if (const region *freed_reg = old_ptr_sval->maybe_get_region ())
	{
	  /* Copy the data.  */
	  const svalue *old_size_sval = model->get_dynamic_extents (freed_reg);
	  if (old_size_sval)
	    {
	      const region *sized_old_reg
		= model->m_mgr->get_sized_region (freed_reg, NULL,
						  old_size_sval);
	      const svalue *buffer_content_sval
		= model->get_store_value (sized_old_reg, cd.get_ctxt ());
	      model->set_value (new_reg, buffer_content_sval, cd.get_ctxt ());
	    }

	  /* Free the old region, so that pointers to the old buffer become
	     invalid.  */

	  /* If the ptr points to an underlying heap region, delete it,
	     poisoning pointers.  */
	  model->unbind_region_and_descendents (freed_reg, POISON_KIND_FREED);
	  model->m_dynamic_extents.remove (freed_reg);
	}

      /* Update the sm-state: mark the old_ptr_sval as "freed",
	 and the new_ptr_sval as "nonnull".  */
      model->on_realloc_with_move (cd, old_ptr_sval, new_ptr_sval);

      if (cd.get_lhs_type ())
	{
	  const svalue *zero
	    = model->m_mgr->get_or_create_int_cst (cd.get_lhs_type (), 0);
	  return model->add_constraint (new_ptr_sval, NE_EXPR, zero,
					cd.get_ctxt ());
	}
      else
	return true;
    }
  };

  /* Body of region_model::impl_call_realloc.  */

  if (cd.get_ctxt ())
    {
      cd.get_ctxt ()->bifurcate (new failure (cd));
      cd.get_ctxt ()->bifurcate (new success_no_move (cd));
      cd.get_ctxt ()->bifurcate (new success_with_move (cd));
      cd.get_ctxt ()->terminate_path ();
    }
}

/* Handle the on_call_pre part of "strcpy" and "__builtin_strcpy_chk".  */

void
region_model::impl_call_strcpy (const call_details &cd)
{
  const svalue *dest_sval = cd.get_arg_svalue (0);
  const region *dest_reg = deref_rvalue (dest_sval, cd.get_arg_tree (0),
					 cd.get_ctxt ());

  cd.maybe_set_lhs (dest_sval);

  check_region_for_write (dest_reg, cd.get_ctxt ());

  /* For now, just mark region's contents as unknown.  */
  mark_region_as_unknown (dest_reg, cd.get_uncertainty ());
}

/* Handle the on_call_pre part of "strlen".  */

void
region_model::impl_call_strlen (const call_details &cd)
{
  region_model_context *ctxt = cd.get_ctxt ();
  const svalue *arg_sval = cd.get_arg_svalue (0);
  const region *buf_reg = deref_rvalue (arg_sval, cd.get_arg_tree (0), ctxt);
  if (const string_region *str_reg
      = buf_reg->dyn_cast_string_region ())
    {
      tree str_cst = str_reg->get_string_cst ();
      /* TREE_STRING_LENGTH is sizeof, not strlen.  */
      int sizeof_cst = TREE_STRING_LENGTH (str_cst);
      int strlen_cst = sizeof_cst - 1;
      if (cd.get_lhs_type ())
	{
	  tree t_cst = build_int_cst (cd.get_lhs_type (), strlen_cst);
	  const svalue *result_sval
	    = m_mgr->get_or_create_constant_svalue (t_cst);
	  cd.maybe_set_lhs (result_sval);
	  return;
	}
    }
  /* Otherwise a conjured value.  */
}

/* Handle calls to functions referenced by
   __attribute__((malloc(FOO))).  */

void
region_model::impl_deallocation_call (const call_details &cd)
{
  impl_call_free (cd);
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
