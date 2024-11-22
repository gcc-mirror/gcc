/* Handling for the various __analyzer_* known functions.
   Copyright (C) 2020-2024 Free Software Foundation, Inc.
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
#include "tree.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "diagnostic-core.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "diagnostic.h"
#include "tree-diagnostic.h" /* for default_tree_printer.  */
#include "analyzer/region-model.h"
#include "analyzer/pending-diagnostic.h"
#include "analyzer/call-details.h"
#include "make-unique.h"
#include "pretty-print-markup.h"

#if ENABLE_ANALYZER

namespace ana {

/* Handle calls to "__analyzer_break" by triggering a breakpoint within
   the analyzer.  */

class kf_analyzer_break : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return cd.num_args () == 0;
  }
  void impl_call_pre (const call_details &) const final override
  {
    /* TODO: is there a good cross-platform way to do this?  */
    raise (SIGINT);
  }
};

/* Handler for calls to "__analyzer_describe".

   Emit a warning describing the 2nd argument (which can be of any
   type), at the given verbosity level.  This is for use when
   debugging, and may be of use in DejaGnu tests.  */

class kf_analyzer_describe : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return cd.num_args () == 2;
  }
  void impl_call_pre (const call_details &cd) const final override
  {
    if (!cd.get_ctxt ())
      return;
    tree t_verbosity = cd.get_arg_tree (0);
    const svalue *sval = cd.get_arg_svalue (1);
    bool simple = zerop (t_verbosity);
    label_text desc = sval->get_desc (simple);
    warning_at (cd.get_location (), 0, "svalue: %qs", desc.get ());
  }
};

/* Handler for calls to "__analyzer_dump_capacity".

   Emit a warning describing the capacity of the base region of
   the region pointed to by the 1st argument.
   This is for use when debugging, and may be of use in DejaGnu tests.  */

class kf_analyzer_dump_capacity : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 1
	    && cd.arg_is_pointer_p (0));
  }

  void impl_call_pre (const call_details &cd) const final override
  {
    region_model_context *ctxt = cd.get_ctxt ();
    if (!ctxt)
      return;
    region_model *model = cd.get_model ();
    tree t_ptr = cd.get_arg_tree (0);
    const svalue *sval_ptr = model->get_rvalue (t_ptr, ctxt);
    const region *reg = model->deref_rvalue (sval_ptr, t_ptr, ctxt);
    const region *base_reg = reg->get_base_region ();
    const svalue *capacity = model->get_capacity (base_reg);
    label_text desc = capacity->get_desc (true);
    warning_at (cd.get_call_stmt ()->location, 0,
		"capacity: %qs", desc.get ());
  }
};

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

/* Handler for calls to "__analyzer_dump_escaped".

   Emit a warning giving the number of decls that have escaped, followed
   by a comma-separated list of their names, in alphabetical order.

   This is for use when debugging, and may be of use in DejaGnu tests.  */

class kf_analyzer_dump_escaped : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return cd.num_args () == 0;
  }
  void impl_call_pre (const call_details &cd) const final override
  {
    region_model_context *ctxt = cd.get_ctxt ();
    if (!ctxt)
      return;
    region_model *model = cd.get_model ();

    auto_vec<tree> escaped_decls;
    for (auto iter : *model->get_store ())
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

    class escaped_list_element : public pp_element
    {
    public:
      escaped_list_element (auto_vec<tree> &escaped_decls)
      : m_escaped_decls (escaped_decls)
      {
      }

      void add_to_phase_2 (pp_markup::context &ctxt) final override
      {
	/* We can't call pp_printf directly on ctxt.m_pp from within
	   formatting.  As a workaround, work with a clone of the pp.  */
	std::unique_ptr<pretty_printer> pp (ctxt.m_pp.clone ());
	bool first = true;
	for (auto iter : m_escaped_decls)
	  {
	    if (first)
	      first = false;
	    else
	      pp_string (pp.get (), ", ");
	    pp_printf (pp.get (), "%qD", iter);
	  }
	pp_string (&ctxt.m_pp, pp_formatted_text (pp.get ()));
      }

    private:
      auto_vec<tree> &m_escaped_decls;
    } e_escaped (escaped_decls);

    /* Print the number to make it easier to write DejaGnu tests for
       the "nothing has escaped" case.  */
    warning_at (cd.get_location (), 0, "escaped: %i: %e",
		escaped_decls.length (),
		&e_escaped);
  }
};

/* Placeholder handler for calls to "__analyzer_dump_exploded_nodes".
   This is a no-op; the real implementation happens when the
   exploded_graph is postprocessed.  */

class kf_analyzer_dump_exploded_nodes : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return cd.num_args () == 1;
  }
};

/* Handler for calls to "__analyzer_dump_named_constant".

   Look up the given name, and emit a warning describing the
   state of the corresponding stashed value.

   This is for use when debugging, and for DejaGnu tests.  */

class kf_analyzer_dump_named_constant : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return cd.num_args () == 1;
  }
  void impl_call_pre (const call_details &cd) const final override
  {
    region_model_context *ctxt = cd.get_ctxt ();
    if (!ctxt)
      return;

    const char *name = cd.get_arg_string_literal (0);
    if (!name)
      {
	error_at (cd.get_location (), "cannot determine name");
	return;
      }
    tree value = get_stashed_constant_by_name (name);
    if (value)
      warning_at (cd.get_location (), 0, "named constant %qs has value %qE",
		  name, value);
    else
      warning_at (cd.get_location (), 0, "named constant %qs has unknown value",
		  name);
  }
};

/* A pending_diagnostic subclass for implementing "__analyzer_dump_path".  */

class dump_path_diagnostic
  : public pending_diagnostic_subclass<dump_path_diagnostic>
{
public:
  int get_controlling_option () const final override
  {
    return 0;
  }

  bool emit (diagnostic_emission_context &ctxt) final override
  {
    ctxt.inform ("path");
    return true;
  }

  const char *get_kind () const final override
  {
    return "dump_path_diagnostic";
  }

  bool operator== (const dump_path_diagnostic &) const
  {
    return true;
  }
};

/* Handle calls to "__analyzer_dump_path" by queuing a diagnostic at this
   exploded_node.  */

class kf_analyzer_dump_path : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return cd.num_args () == 0;
  }
  void impl_call_pre (const call_details &cd) const final override
  {
    region_model_context *ctxt = cd.get_ctxt ();
    if (!ctxt)
      return;
    ctxt->warn (make_unique<dump_path_diagnostic> ());
  }
};

/* Handle calls to "__analyzer_dump_region_model" by dumping
   the region model's state to stderr.  */

class kf_analyzer_dump_region_model : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return cd.num_args () == 0;
  }
  void impl_call_pre (const call_details &cd) const final override
  {
    region_model_context *ctxt = cd.get_ctxt ();
    if (!ctxt)
      return;
    region_model *model = cd.get_model ();
    model->dump (false);
  }
};

/* Handle a call to "__analyzer_eval" by evaluating the input
   and dumping as a dummy warning, so that test cases can use
   dg-warning to validate the result (and so unexpected warnings will
   lead to DejaGnu failures).
   Broken out as a subroutine to make it easier to put a breakpoint on it
   - though typically this doesn't help, as we have an SSA name as the arg,
   and what's more interesting is usually the def stmt for that name.  */

class kf_analyzer_eval : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return cd.num_args () == 1;
  }
  void impl_call_pre (const call_details &cd) const final override
  {
    region_model_context *ctxt = cd.get_ctxt ();
    if (!ctxt)
      return;
    region_model *model = cd.get_model ();

    tree t_arg = cd.get_arg_tree (0);
    tristate t = model->eval_condition (t_arg, NE_EXPR, integer_zero_node,
					ctxt);
    warning_at (cd.get_location (), 0, "%s", t.as_string ());
  }
};

/* Handler for "__analyzer_get_unknown_ptr".  */

class kf_analyzer_get_unknown_ptr : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return cd.num_args () == 0;
  }
  void impl_call_pre (const call_details &cd) const final override
  {
    region_model_manager *mgr = cd.get_manager ();
    const svalue *ptr_sval
      = mgr->get_or_create_unknown_svalue (cd.get_lhs_type ());
    cd.maybe_set_lhs (ptr_sval);
  }
};

/* Populate KFM with instances of known functions used for debugging the
   analyzer and for writing DejaGnu tests, all with a "__analyzer_" prefix.  */

void
register_known_analyzer_functions (known_function_manager &kfm)
{
  kfm.add ("__analyzer_break", make_unique<kf_analyzer_break> ());
  kfm.add ("__analyzer_describe", make_unique<kf_analyzer_describe> ());
  kfm.add ("__analyzer_dump_capacity",
	   make_unique<kf_analyzer_dump_capacity> ());
  kfm.add ("__analyzer_dump_escaped", make_unique<kf_analyzer_dump_escaped> ());
  kfm.add ("__analyzer_dump_exploded_nodes",
	   make_unique<kf_analyzer_dump_exploded_nodes> ());
  kfm.add ("__analyzer_dump_named_constant",
	   make_unique<kf_analyzer_dump_named_constant> ());
  kfm.add ("__analyzer_dump_path", make_unique<kf_analyzer_dump_path> ());
  kfm.add ("__analyzer_dump_region_model",
	   make_unique<kf_analyzer_dump_region_model> ());
  kfm.add ("__analyzer_eval", make_unique<kf_analyzer_eval> ());
  kfm.add ("__analyzer_get_unknown_ptr",
	   make_unique<kf_analyzer_get_unknown_ptr> ());
  kfm.add ("__analyzer_get_strlen", make_kf_strlen ());
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
