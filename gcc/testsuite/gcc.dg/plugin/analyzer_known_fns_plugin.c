/* Proof-of-concept of a -fanalyzer plugin to handle known functions.  */
/* { dg-options "-g" } */

#define INCLUDE_MEMORY
#include "gcc-plugin.h"
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
#include "analyzer/call-details.h"
#include "analyzer/call-info.h"
#include "make-unique.h"

int plugin_is_GPL_compatible;

#if ENABLE_ANALYZER

namespace ana {

/* Basic example of known fn behavior.  */

class known_function_returns_42 : public known_function
{
public:
  bool matches_call_types_p (const call_details &) const final override
  {
    return true;
  }

  void impl_call_pre (const call_details &cd) const final override
  {
    if (cd.get_lhs_type ())
      {
	const svalue *result
	  = cd.get_manager ()->get_or_create_int_cst (cd.get_lhs_type (), 42);
	cd.maybe_set_lhs (result);
      }
  }
};

/* Example of bifurcation, with a copy that can fail.  */

class known_function_attempt_to_copy : public known_function
{
public:
  class copy_success : public success_call_info
  {
  public:
    copy_success (const call_details &cd,
		  const region *sized_dest_reg,
		  const svalue *copied_sval)
    : success_call_info (cd),
      m_sized_dest_reg (sized_dest_reg),
      m_copied_sval (copied_sval)
    {}

    bool update_model (region_model *model,
		       const exploded_edge *,
		       region_model_context *ctxt) const final override
    {
      call_details cd (get_call_details (model, ctxt));
      model->update_for_zero_return (cd, true);
      model->set_value (m_sized_dest_reg, m_copied_sval, ctxt);
      return true;
    }

    const region *m_sized_dest_reg;
    const svalue *m_copied_sval;
    const region *m_sized_src_reg;
  };

  class copy_failure : public failed_call_info
  {
  public:
    copy_failure (const call_details &cd)
    : failed_call_info (cd)
    {}

    bool update_model (region_model *model,
		       const exploded_edge *,
		       region_model_context *ctxt) const final override
    {
      call_details cd (get_call_details (model, ctxt));
      model->update_for_nonzero_return (cd);
      /* Leave the destination region untouched.  */
      return true;
    }
  };

  bool matches_call_types_p (const call_details &cd) const
  {
    return cd.num_args () == 3;
  }

  void impl_call_pre (const call_details &cd) const final override
  {
    region_model_manager *mgr = cd.get_manager ();
    region_model *model = cd.get_model ();

    const svalue *dest_sval = cd.get_arg_svalue (0);
    const svalue *src_sval = cd.get_arg_svalue (1);
    const svalue *num_bytes_sval = cd.get_arg_svalue (2);

    const region *dest_reg = model->deref_rvalue (dest_sval,
						  cd.get_arg_tree (0),
						  cd.get_ctxt ());
    const region *src_reg = model->deref_rvalue (src_sval,
						 cd.get_arg_tree (1),
						 cd.get_ctxt ());
    if (const svalue * bounded_sval
	  = model->maybe_get_copy_bounds (src_reg, num_bytes_sval))
      num_bytes_sval = bounded_sval;

    if (tree cst = num_bytes_sval->maybe_get_constant ())
      if (zerop (cst))
	/* No-op.  */
	return;

    const region *sized_src_reg = mgr->get_sized_region (src_reg,
							 NULL_TREE,
							 num_bytes_sval);

    const svalue *copied_sval
      = model->get_store_value (sized_src_reg, cd.get_ctxt ());

    const region *sized_dest_reg = mgr->get_sized_region (dest_reg,
							  NULL_TREE,
							  num_bytes_sval);

    if (cd.get_ctxt ())
      {
	/* Bifurcate state, creating a "failure" out-edge.  */
	cd.get_ctxt ()->bifurcate (make_unique<copy_failure> (cd));

	/* The "unbifurcated" state is the "success" case.  */
	copy_success success (cd,
			      sized_dest_reg,
			      copied_sval);
	success.update_model (model, NULL, cd.get_ctxt ());
      }
  }
};

/* Callback handler for the PLUGIN_ANALYZER_INIT event.  */

static void
known_fn_analyzer_init_cb (void *gcc_data, void */*user_data*/)
{
  ana::plugin_analyzer_init_iface *iface
    = (ana::plugin_analyzer_init_iface *)gcc_data;
  LOG_SCOPE (iface->get_logger ());
  if (0)
    inform (input_location, "got here: known_fn_analyzer_init_cb");
  iface->register_known_function ("returns_42",
				  make_unique<known_function_returns_42> ());
  iface->register_known_function
    ("attempt_to_copy",
     make_unique<known_function_attempt_to_copy> ());
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */

int
plugin_init (struct plugin_name_args *plugin_info,
	     struct plugin_gcc_version *version)
{
#if ENABLE_ANALYZER
  const char *plugin_name = plugin_info->base_name;
  if (0)
    inform (input_location, "got here; %qs", plugin_name);
  register_callback (plugin_info->base_name,
		     PLUGIN_ANALYZER_INIT,
		     ana::known_fn_analyzer_init_cb,
		     NULL); /* void *user_data */
#else
  sorry_no_analyzer ();
#endif
  return 0;
}
