/* Handling for the known behavior of various specific functions.
   Copyright (C) 2020-2022 Free Software Foundation, Inc.
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
#include "bitmap.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "ordered-hash-map.h"
#include "options.h"
#include "analyzer/supergraph.h"
#include "sbitmap.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"
#include "analyzer/call-info.h"
#include "analyzer/sm.h"
#include "diagnostic-path.h"
#include "analyzer/pending-diagnostic.h"
#include "gimple-pretty-print.h"
#include "make-unique.h"

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

/* Get the manager from m_model.  */

region_model_manager *
call_details::get_manager () const
{
  return m_model->get_manager ();
}

/* Get any logger associated with this object.  */

logger *
call_details::get_logger () const
{
  if (m_ctxt)
    return m_ctxt->get_logger ();
  else
    return NULL;
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

/* Get a conjured_svalue for this call for REG,
   and purge any state already relating to that conjured_svalue.  */

const svalue *
call_details::get_or_create_conjured_svalue (const region *reg) const
{
  region_model_manager *mgr = m_model->get_manager ();
  return mgr->get_or_create_conjured_svalue (reg->get_type (), m_call, reg,
					     conjured_purge (m_model, m_ctxt));
}

/* Implementations of specific functions.  */

/* Handle the on_call_pre part of "alloca".  */

void
region_model::impl_call_alloca (const call_details &cd)
{
  const svalue *size_sval = cd.get_arg_svalue (0);
  const region *new_reg = create_region_for_alloca (size_sval, cd.get_ctxt ());
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
  warning_at (call->location, 0, "svalue: %qs", desc.get ());
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
  warning_at (call->location, 0, "capacity: %qs", desc.get ());
}

/* Compare D1 and D2 using their names, and then IDs to order them.  */

static int
cmp_decls (tree d1, tree d2)
{
  gcc_assert (DECL_P (d1));
  gcc_assert (DECL_P (d2));
  if (DECL_NAME (d1) && DECL_NAME (d2))
    if (int cmp = strcmp (IDENTIFIER_POINTER (DECL_NAME (d1)),
			  IDENTIFIER_POINTER (DECL_NAME (d2))))
      return cmp;
  return (int)DECL_UID (d1) - (int)DECL_UID (d2);
}

/* Comparator for use by vec<tree>::qsort,
   using their names, and then IDs to order them.  */

static int
cmp_decls_ptr_ptr (const void *p1, const void *p2)
{
  tree const *d1 = (tree const *)p1;
  tree const *d2 = (tree const *)p2;

  return cmp_decls (*d1, *d2);
}

/* Handle a call to "__analyzer_dump_escaped".

   Emit a warning giving the number of decls that have escaped, followed
   by a comma-separated list of their names, in alphabetical order.

   This is for use when debugging, and may be of use in DejaGnu tests.  */

void
region_model::impl_call_analyzer_dump_escaped (const gcall *call)
{
  auto_vec<tree> escaped_decls;
  for (auto iter : m_store)
    {
      const binding_cluster *c = iter.second;
      if (!c->escaped_p ())
	continue;
      if (tree decl = c->get_base_region ()->maybe_get_decl ())
	escaped_decls.safe_push (decl);
    }

  /* Sort them into deterministic order; alphabetical is
     probably most user-friendly.  */
  escaped_decls.qsort (cmp_decls_ptr_ptr);

  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_show_color (&pp) = pp_show_color (global_dc->printer);
  bool first = true;
  for (auto iter : escaped_decls)
    {
      if (first)
	first = false;
      else
	pp_string (&pp, ", ");
      pp_printf (&pp, "%qD", iter);
    }
  /* Print the number to make it easier to write DejaGnu tests for
     the "nothing has escaped" case.  */
  warning_at (call->location, 0, "escaped: %i: %s",
	      escaped_decls.length (),
	      pp_formatted_text (&pp));
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

/* Handle the on_call_pre part of "__analyzer_get_unknown_ptr".  */

void
region_model::impl_call_analyzer_get_unknown_ptr (const call_details &cd)
{
  const svalue *ptr_sval
    = m_mgr->get_or_create_unknown_svalue (cd.get_lhs_type ());
  cd.maybe_set_lhs (ptr_sval);
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
  const region *new_reg
    = create_region_for_heap_alloc (prod_sval, cd.get_ctxt ());
  const region *sized_reg
    = m_mgr->get_sized_region (new_reg, NULL_TREE, prod_sval);
  zero_fill_region (sized_reg);
  if (cd.get_lhs_type ())
    {
      const svalue *ptr_sval
	= m_mgr->get_ptr_svalue (cd.get_lhs_type (), new_reg);
      cd.maybe_set_lhs (ptr_sval);
    }
}

/* Handle the on_call_pre part of "__errno_location".  */

void
region_model::impl_call_errno_location (const call_details &cd)
{
  if (cd.get_lhs_region ())
    {
      const region *errno_reg = m_mgr->get_errno_region ();
      const svalue *errno_ptr = m_mgr->get_ptr_svalue (cd.get_lhs_type (),
						       errno_reg);
      cd.maybe_set_lhs (errno_ptr);
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
   (in region_model::eval_condition), and thus transition
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
  const region *new_reg
    = create_region_for_heap_alloc (size_sval, cd.get_ctxt ());
  if (cd.get_lhs_type ())
    {
      const svalue *ptr_sval
	= m_mgr->get_ptr_svalue (cd.get_lhs_type (), new_reg);
      cd.maybe_set_lhs (ptr_sval);
    }
}

/* Handle the on_call_pre part of "memcpy" and "__builtin_memcpy".  */
// TODO: complain about overlapping src and dest.

void
region_model::impl_call_memcpy (const call_details &cd)
{
  const svalue *dest_ptr_sval = cd.get_arg_svalue (0);
  const svalue *src_ptr_sval = cd.get_arg_svalue (1);
  const svalue *num_bytes_sval = cd.get_arg_svalue (2);

  const region *dest_reg = deref_rvalue (dest_ptr_sval, cd.get_arg_tree (0),
					 cd.get_ctxt ());
  const region *src_reg = deref_rvalue (src_ptr_sval, cd.get_arg_tree (1),
					cd.get_ctxt ());

  cd.maybe_set_lhs (dest_ptr_sval);

  const region *sized_src_reg
    = m_mgr->get_sized_region (src_reg, NULL_TREE, num_bytes_sval);
  const region *sized_dest_reg
    = m_mgr->get_sized_region (dest_reg, NULL_TREE, num_bytes_sval);
  const svalue *src_contents_sval
    = get_store_value (sized_src_reg, cd.get_ctxt ());
  set_value (sized_dest_reg, src_contents_sval, cd.get_ctxt ());
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

/* Handle the on_call_post part of "pipe".  */

void
region_model::impl_call_pipe (const call_details &cd)
{
  class failure : public failed_call_info
  {
  public:
    failure (const call_details &cd) : failed_call_info (cd) {}

    bool update_model (region_model *model,
		       const exploded_edge *,
		       region_model_context *ctxt) const final override
    {
      /* Return -1; everything else is unchanged.  */
      const call_details cd (get_call_details (model, ctxt));
      model->update_for_int_cst_return (cd, -1, true);
      return true;
    }
  };

  class success : public success_call_info
  {
  public:
    success (const call_details &cd) : success_call_info (cd) {}

    bool update_model (region_model *model,
		       const exploded_edge *,
		       region_model_context *ctxt) const final override
    {
      const call_details cd (get_call_details (model, ctxt));

      /* Return 0.  */
      model->update_for_zero_return (cd, true);

      /* Update fd array.  */
      region_model_manager *mgr = cd.get_manager ();
      tree arr_tree = cd.get_arg_tree (0);
      const svalue *arr_sval = cd.get_arg_svalue (0);
      for (int idx = 0; idx < 2; idx++)
	{
	  const region *arr_reg
	    = model->deref_rvalue (arr_sval, arr_tree, cd.get_ctxt ());
	  const svalue *idx_sval
	    = mgr->get_or_create_int_cst (integer_type_node, idx);
	  const region *element_reg
	    = mgr->get_element_region (arr_reg, integer_type_node, idx_sval);
	  conjured_purge p (model, cd.get_ctxt ());
	  const svalue *fd_sval
	    = mgr->get_or_create_conjured_svalue (integer_type_node,
						  cd.get_call_stmt (),
						  element_reg,
						  p);
	  model->set_value (element_reg, fd_sval, cd.get_ctxt ());
	  model->mark_as_valid_fd (fd_sval, cd.get_ctxt ());

	}
      return true;
    }
  };

  /* Body of region_model::impl_call_pipe.  */
  if (cd.get_ctxt ())
    {
      cd.get_ctxt ()->bifurcate (make_unique<failure> (cd));
      cd.get_ctxt ()->bifurcate (make_unique<success> (cd));
      cd.get_ctxt ()->terminate_path ();
    }
}

/* A subclass of pending_diagnostic for complaining about 'putenv'
   called on an auto var.  */

class putenv_of_auto_var
: public pending_diagnostic_subclass<putenv_of_auto_var>
{
public:
  putenv_of_auto_var (tree fndecl, const region *reg)
  : m_fndecl (fndecl), m_reg (reg),
    m_var_decl (reg->get_base_region ()->maybe_get_decl ())
  {
  }

  const char *get_kind () const final override
  {
    return "putenv_of_auto_var";
  }

  bool operator== (const putenv_of_auto_var &other) const
  {
    return (m_fndecl == other.m_fndecl
	    && m_reg == other.m_reg
	    && same_tree_p (m_var_decl, other.m_var_decl));
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_putenv_of_auto_var;
  }

  bool emit (rich_location *rich_loc) final override
  {
    auto_diagnostic_group d;
    diagnostic_metadata m;

    /* SEI CERT C Coding Standard: "POS34-C. Do not call putenv() with a
       pointer to an automatic variable as the argument".  */
    diagnostic_metadata::precanned_rule
      rule ("POS34-C", "https://wiki.sei.cmu.edu/confluence/x/6NYxBQ");
    m.add_rule (rule);

    bool warned;
    if (m_var_decl)
      warned = warning_meta (rich_loc, m, get_controlling_option (),
			     "%qE on a pointer to automatic variable %qE",
			     m_fndecl, m_var_decl);
    else
      warned = warning_meta (rich_loc, m, get_controlling_option (),
			     "%qE on a pointer to an on-stack buffer",
			     m_fndecl);
    if (warned)
      {
	if (m_var_decl)
	  inform (DECL_SOURCE_LOCATION (m_var_decl),
		  "%qE declared on stack here", m_var_decl);
	inform (rich_loc->get_loc (), "perhaps use %qs rather than %qE",
		"setenv", m_fndecl);
      }

    return warned;
  }

  label_text describe_final_event (const evdesc::final_event &ev) final override
  {
    if (m_var_decl)
      return ev.formatted_print ("%qE on a pointer to automatic variable %qE",
				 m_fndecl, m_var_decl);
    else
      return ev.formatted_print ("%qE on a pointer to an on-stack buffer",
				 m_fndecl);
  }

  void mark_interesting_stuff (interesting_t *interest) final override
  {
    if (!m_var_decl)
      interest->add_region_creation (m_reg->get_base_region ());
  }

private:
  tree m_fndecl; // non-NULL
  const region *m_reg; // non-NULL
  tree m_var_decl; // could be NULL
};

/* Handle the on_call_pre part of "putenv".

   In theory we could try to model the state of the environment variables
   for the process; for now we merely complain about putenv of regions
   on the stack.  */

void
region_model::impl_call_putenv (const call_details &cd)
{
  tree fndecl = cd.get_fndecl_for_call ();
  gcc_assert (fndecl);
  region_model_context *ctxt = cd.get_ctxt ();
  const svalue *ptr_sval = cd.get_arg_svalue (0);
  const region *reg = deref_rvalue (ptr_sval, cd.get_arg_tree (0), ctxt);
  m_store.mark_as_escaped (reg);
  enum memory_space mem_space = reg->get_memory_space ();
  switch (mem_space)
    {
    default:
      gcc_unreachable ();
    case MEMSPACE_UNKNOWN:
    case MEMSPACE_CODE:
    case MEMSPACE_GLOBALS:
    case MEMSPACE_HEAP:
    case MEMSPACE_READONLY_DATA:
      break;
    case MEMSPACE_STACK:
      if (ctxt)
	ctxt->warn (make_unique<putenv_of_auto_var> (fndecl, reg));
      break;
    }
}

/* Handle the on_call_pre part of "operator new".  */

void
region_model::impl_call_operator_new (const call_details &cd)
{
  const svalue *size_sval = cd.get_arg_svalue (0);
  const region *new_reg
    = create_region_for_heap_alloc (size_sval, cd.get_ctxt ());
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
		       region_model_context *ctxt) const final override
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

    label_text get_desc (bool can_colorize) const final override
    {
      return make_label_text (can_colorize,
			      "when %qE succeeds, without moving buffer",
			      get_fndecl ());
    }

    bool update_model (region_model *model,
		       const exploded_edge *,
		       region_model_context *ctxt) const final override
    {
      /* Update size of buffer and return the ptr unchanged.  */
      const call_details cd (get_call_details (model, ctxt));
      const svalue *ptr_sval = cd.get_arg_svalue (0);
      const svalue *size_sval = cd.get_arg_svalue (1);

      /* We can only grow in place with a non-NULL pointer.  */
      {
	const svalue *null_ptr
	  = model->m_mgr->get_or_create_int_cst (ptr_sval->get_type (), 0);
	if (!model->add_constraint (ptr_sval, NE_EXPR, null_ptr,
				    cd.get_ctxt ()))
	  return false;
      }

      if (const region *buffer_reg = model->deref_rvalue (ptr_sval, NULL_TREE,
							  ctxt))
	if (compat_types_p (size_sval->get_type (), size_type_node))
	  model->set_dynamic_extents (buffer_reg, size_sval, ctxt);
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

    label_text get_desc (bool can_colorize) const final override
    {
      return make_label_text (can_colorize,
			      "when %qE succeeds, moving buffer",
			      get_fndecl ());
    }
    bool update_model (region_model *model,
		       const exploded_edge *,
		       region_model_context *ctxt) const final override
    {
      const call_details cd (get_call_details (model, ctxt));
      const svalue *old_ptr_sval = cd.get_arg_svalue (0);
      const svalue *new_size_sval = cd.get_arg_svalue (1);

      /* Create the new region.  */
      const region *new_reg
	= model->create_region_for_heap_alloc (new_size_sval, ctxt);
      const svalue *new_ptr_sval
	= model->m_mgr->get_ptr_svalue (cd.get_lhs_type (), new_reg);
      if (!model->add_constraint (new_ptr_sval, NE_EXPR, old_ptr_sval,
				  cd.get_ctxt ()))
	return false;

      if (cd.get_lhs_type ())
	cd.maybe_set_lhs (new_ptr_sval);

      if (const region *freed_reg = model->deref_rvalue (old_ptr_sval,
							 NULL_TREE, ctxt))
	{
	  /* Copy the data.  */
	  const svalue *old_size_sval = model->get_dynamic_extents (freed_reg);
	  if (old_size_sval)
	    {
	      const svalue *copied_size_sval
		= get_copied_size (model, old_size_sval, new_size_sval);
	      const region *copied_old_reg
		= model->m_mgr->get_sized_region (freed_reg, NULL,
						  copied_size_sval);
	      const svalue *buffer_content_sval
		= model->get_store_value (copied_old_reg, cd.get_ctxt ());
	      const region *copied_new_reg
		= model->m_mgr->get_sized_region (new_reg, NULL,
						  copied_size_sval);
	      model->set_value (copied_new_reg, buffer_content_sval,
				cd.get_ctxt ());
	    }
	  else
	    {
	      /* We don't know how big the old region was;
		 mark the new region as having been touched to avoid uninit
		 issues.  */
	      model->mark_region_as_unknown (new_reg, cd.get_uncertainty ());
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

  private:
    /* Return the lesser of OLD_SIZE_SVAL and NEW_SIZE_SVAL.
       If unknown, OLD_SIZE_SVAL is returned.  */
    const svalue *get_copied_size (region_model *model,
				   const svalue *old_size_sval,
				   const svalue *new_size_sval) const
    {
      tristate res
	= model->eval_condition (old_size_sval, GT_EXPR, new_size_sval);
      switch (res.get_value ())
	{
	case tristate::TS_TRUE:
	  return new_size_sval;
	case tristate::TS_FALSE:
	case tristate::TS_UNKNOWN:
	  return old_size_sval;
	default:
	  gcc_unreachable ();
	}
    }
  };

  /* Body of region_model::impl_call_realloc.  */

  if (cd.get_ctxt ())
    {
      cd.get_ctxt ()->bifurcate (make_unique<failure> (cd));
      cd.get_ctxt ()->bifurcate (make_unique<success_no_move> (cd));
      cd.get_ctxt ()->bifurcate (make_unique<success_with_move> (cd));
      cd.get_ctxt ()->terminate_path ();
    }
}

/* Handle the on_call_post part of "strchr" and "__builtin_strchr".  */

void
region_model::impl_call_strchr (const call_details &cd)
{
  class strchr_call_info : public call_info
  {
  public:
    strchr_call_info (const call_details &cd, bool found)
    : call_info (cd), m_found (found)
    {
    }

    label_text get_desc (bool can_colorize) const final override
    {
      if (m_found)
	return make_label_text (can_colorize,
				"when %qE returns non-NULL",
				get_fndecl ());
      else
	return make_label_text (can_colorize,
				"when %qE returns NULL",
				get_fndecl ());
    }

    bool update_model (region_model *model,
		       const exploded_edge *,
		       region_model_context *ctxt) const final override
    {
      const call_details cd (get_call_details (model, ctxt));
      if (tree lhs_type = cd.get_lhs_type ())
	{
	  region_model_manager *mgr = model->get_manager ();
	  const svalue *result;
	  if (m_found)
	    {
	      const svalue *str_sval = cd.get_arg_svalue (0);
	      const region *str_reg
		= model->deref_rvalue (str_sval, cd.get_arg_tree (0),
				       cd.get_ctxt ());
	      /* We want str_sval + OFFSET for some unknown OFFSET.
		 Use a conjured_svalue to represent the offset,
		 using the str_reg as the id of the conjured_svalue.  */
	      const svalue *offset
		= mgr->get_or_create_conjured_svalue (size_type_node,
						      cd.get_call_stmt (),
						      str_reg,
						      conjured_purge (model,
								      ctxt));
	      result = mgr->get_or_create_binop (lhs_type, POINTER_PLUS_EXPR,
						 str_sval, offset);
	    }
	  else
	    result = mgr->get_or_create_int_cst (lhs_type, 0);
	  cd.maybe_set_lhs (result);
	}
      return true;
    }
  private:
    bool m_found;
  };

  /* Body of region_model::impl_call_strchr.  */
  if (cd.get_ctxt ())
    {
      cd.get_ctxt ()->bifurcate (make_unique<strchr_call_info> (cd, false));
      cd.get_ctxt ()->bifurcate (make_unique<strchr_call_info> (cd, true));
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
  const svalue *src_sval = cd.get_arg_svalue (1);
  const region *src_reg = deref_rvalue (src_sval, cd.get_arg_tree (1),
					cd.get_ctxt ());
  const svalue *src_contents_sval = get_store_value (src_reg,
						     cd.get_ctxt ());

  cd.maybe_set_lhs (dest_sval);

  /* Try to get the string size if SRC_REG is a string_region.  */
  const svalue *copied_bytes_sval = get_string_size (src_reg);
  /* Otherwise, check if the contents of SRC_REG is a string.  */
  if (copied_bytes_sval->get_kind () == SK_UNKNOWN)
    copied_bytes_sval = get_string_size (src_contents_sval);

  const region *sized_dest_reg
    = m_mgr->get_sized_region (dest_reg, NULL_TREE, copied_bytes_sval);
  set_value (sized_dest_reg, src_contents_sval, cd.get_ctxt ());
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
