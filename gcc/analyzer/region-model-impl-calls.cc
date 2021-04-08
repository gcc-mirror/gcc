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
  return m_ctxt->get_uncertainty ();
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

/* Implementations of specific functions.  */

/* Handle the on_call_pre part of "alloca".  */

bool
region_model::impl_call_alloca (const call_details &cd)
{
  const svalue *size_sval = cd.get_arg_svalue (0);
  const region *new_reg = create_region_for_alloca (size_sval);
  const svalue *ptr_sval
    = m_mgr->get_ptr_svalue (cd.get_lhs_type (), new_reg);
  cd.maybe_set_lhs (ptr_sval);
  return true;
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

bool
region_model::impl_call_builtin_expect (const call_details &cd)
{
  /* __builtin_expect's return value is its initial argument.  */
  const svalue *sval = cd.get_arg_svalue (0);
  cd.maybe_set_lhs (sval);
  return false;
}

/* Handle the on_call_pre part of "calloc".  */

bool
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
  return true;
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
  if (const region_svalue *ptr_to_region_sval
      = ptr_sval->dyn_cast_region_svalue ())
    {
      /* If the ptr points to an underlying heap region, delete it,
	 poisoning pointers.  */
      const region *freed_reg = ptr_to_region_sval->get_pointee ();
      unbind_region_and_descendents (freed_reg, POISON_KIND_FREED);
    }
}

/* Handle the on_call_pre part of "malloc".  */

bool
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
  return true;
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

  check_for_writable_region (dest_reg, cd.get_ctxt ());

  /* Otherwise, mark region's contents as unknown.  */
  mark_region_as_unknown (dest_reg, cd.get_uncertainty ());
}

/* Handle the on_call_pre part of "memset" and "__builtin_memset".  */

bool
region_model::impl_call_memset (const call_details &cd)
{
  const svalue *dest_sval = cd.get_arg_svalue (0);
  const svalue *fill_value_sval = cd.get_arg_svalue (1);
  const svalue *num_bytes_sval = cd.get_arg_svalue (2);

  const region *dest_reg = deref_rvalue (dest_sval, cd.get_arg_tree (0),
					  cd.get_ctxt ());

  if (tree num_bytes = num_bytes_sval->maybe_get_constant ())
    {
      /* "memset" of zero size is a no-op.  */
      if (zerop (num_bytes))
	return true;

      /* Set with known amount.  */
      byte_size_t reg_size;
      if (dest_reg->get_byte_size (&reg_size))
	{
	  /* Check for an exact size match.  */
	  if (reg_size == wi::to_offset (num_bytes))
	    {
	      if (tree cst = fill_value_sval->maybe_get_constant ())
		{
		  if (zerop (cst))
		    {
		      zero_fill_region (dest_reg);
		      return true;
		    }
		}
	    }
	}
    }

  check_for_writable_region (dest_reg, cd.get_ctxt ());

  /* Otherwise, mark region's contents as unknown.  */
  mark_region_as_unknown (dest_reg, cd.get_uncertainty ());
  return false;
}

/* Handle the on_call_pre part of "operator new".  */

bool
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
  return false;
}

/* Handle the on_call_pre part of "operator delete", which comes in
   both sized and unsized variants (2 arguments and 1 argument
   respectively).  */

bool
region_model::impl_call_operator_delete (const call_details &cd)
{
  const svalue *ptr_sval = cd.get_arg_svalue (0);
  if (const region_svalue *ptr_to_region_sval
      = ptr_sval->dyn_cast_region_svalue ())
    {
      /* If the ptr points to an underlying heap region, delete it,
	 poisoning pointers.  */
      const region *freed_reg = ptr_to_region_sval->get_pointee ();
      unbind_region_and_descendents (freed_reg, POISON_KIND_FREED);
    }
  return false;
}

/* Handle the on_call_pre part of "realloc".  */

void
region_model::impl_call_realloc (const call_details &)
{
  /* Currently we don't support bifurcating state, so there's no good
     way to implement realloc(3).
     For now, malloc_state_machine::on_realloc_call has a minimal
     implementation to suppress false positives.  */
}

/* Handle the on_call_pre part of "strcpy" and "__builtin_strcpy_chk".  */

void
region_model::impl_call_strcpy (const call_details &cd)
{
  const svalue *dest_sval = cd.get_arg_svalue (0);
  const region *dest_reg = deref_rvalue (dest_sval, cd.get_arg_tree (0),
					 cd.get_ctxt ());

  cd.maybe_set_lhs (dest_sval);

  check_for_writable_region (dest_reg, cd.get_ctxt ());

  /* For now, just mark region's contents as unknown.  */
  mark_region_as_unknown (dest_reg, cd.get_uncertainty ());
}

/* Handle the on_call_pre part of "strlen".
   Return true if the LHS is updated.  */

bool
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
	  return true;
	}
    }
  /* Otherwise an unknown value.  */
  return true;
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
