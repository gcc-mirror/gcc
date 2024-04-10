/* Proof-of-concept of a -fanalyzer plugin for the Linux kernel.  */
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

/* Implementation of "copy_from_user" and "copy_to_user".  */
  
class copy_across_boundary_fn : public known_function
{
 public:
  virtual bool untrusted_source_p () const = 0;
  virtual bool untrusted_destination_p () const = 0;

  bool matches_call_types_p (const call_details &cd) const final override
  {
    return cd.num_args () == 3;
  }

  void impl_call_pre (const call_details &cd) const final override
  {
    region_model_manager *mgr = cd.get_manager ();
    region_model *model = cd.get_model ();
    region_model_context *ctxt = cd.get_ctxt ();

    const svalue *dest_sval = cd.get_arg_svalue (0);
    const svalue *src_sval = cd.get_arg_svalue (1);
    const svalue *num_bytes_sval = cd.get_arg_svalue (2);

    const region *dest_reg = model->deref_rvalue (dest_sval,
						  cd.get_arg_tree (0),
						  ctxt);
    const region *src_reg = model->deref_rvalue (src_sval,
						 cd.get_arg_tree (1),
						 ctxt);
    if (const svalue *bounded_sval
	  = model->maybe_get_copy_bounds (src_reg, num_bytes_sval))
      num_bytes_sval = bounded_sval;

    if (tree cst = num_bytes_sval->maybe_get_constant ())
      if (zerop (cst))
	{
	  /* No-op.  */
	  model->update_for_zero_return (cd, true);
	  return;
	}

    const region *sized_src_reg = mgr->get_sized_region (src_reg,
							 NULL_TREE,
							 num_bytes_sval);

    const svalue *copied_sval
      = model->get_store_value (sized_src_reg, ctxt);
    const region *sized_dest_reg = mgr->get_sized_region (dest_reg,
							  NULL_TREE,
							  num_bytes_sval);

    if (ctxt)
      {
	/* Bifurcate state, creating a "failure" out-edge.  */
	ctxt->bifurcate (make_unique<copy_failure> (cd));

	/* The "unbifurcated" state is the "success" case.  */
	copy_success success (cd,
			      sized_dest_reg,
			      copied_sval,
			      sized_src_reg,
			      untrusted_source_p (),
			      untrusted_destination_p ());
	success.update_model (model, NULL, ctxt);
      }
  }

 private:
  class copy_success : public success_call_info
  {
  public:
    copy_success (const call_details &cd,
		  const region *sized_dest_reg,
		  const svalue *copied_sval,
		  const region *sized_src_reg,
		  bool untrusted_source,
		  bool untrusted_destination)
    : success_call_info (cd),
      m_sized_dest_reg (sized_dest_reg),
      m_copied_sval (copied_sval),
      m_sized_src_reg (sized_src_reg),
      m_untrusted_source (untrusted_source),
      m_untrusted_destination (untrusted_destination)
    {}

    bool update_model (region_model *model,
		       const exploded_edge *,
		       region_model_context *ctxt) const final override
    {
      call_details cd (get_call_details (model, ctxt));
      model->update_for_zero_return (cd, true);
      model->set_value (m_sized_dest_reg, m_copied_sval, ctxt);
      if (ctxt && m_untrusted_source)
	model->mark_as_tainted (m_copied_sval, ctxt);
      if (m_untrusted_destination)
	model->maybe_complain_about_infoleak (m_sized_dest_reg,
					      m_copied_sval,
					      m_sized_src_reg,
					      ctxt);
      return true;
    }

    const region *m_sized_dest_reg;
    const svalue *m_copied_sval;
    const region *m_sized_src_reg;
    bool m_untrusted_source;
    bool m_untrusted_destination;
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
};

/* "copy_from_user".  */

class known_function_copy_from_user : public copy_across_boundary_fn
{
public:
  bool untrusted_source_p () const final override
  {
    return true;
  }
  bool untrusted_destination_p () const final override
  {
    return false;
  }
};

/* "copy_to_user".  */

class known_function_copy_to_user : public copy_across_boundary_fn
{
public:
  bool untrusted_source_p () const final override
  {
    return false;
  }
  bool untrusted_destination_p () const final override
  {
    return true;
  }
};

/* Implementation of "__check_object_size".  */
  
class known_function___check_object_size : public known_function
{
 public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return cd.num_args () == 2;
  }

  void impl_call_pre (const call_details &) const final override
  {
    /* No-op.  */
  }
};

/* Callback handler for the PLUGIN_ANALYZER_INIT event.  */

static void
kernel_analyzer_init_cb (void *gcc_data, void */*user_data*/)
{
  ana::plugin_analyzer_init_iface *iface
    = (ana::plugin_analyzer_init_iface *)gcc_data;
  LOG_SCOPE (iface->get_logger ());
  if (0)
    inform (input_location, "got here: kernel_analyzer_init_cb");
  iface->register_known_function
    ("copy_from_user",
     make_unique<known_function_copy_from_user> ());
  iface->register_known_function ("copy_to_user",
				  make_unique<known_function_copy_to_user> ());
  iface->register_known_function ("__check_object_size",
				  make_unique<known_function___check_object_size> ());
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
		     ana::kernel_analyzer_init_cb,
		     NULL); /* void *user_data */
#else
  sorry_no_analyzer ();
#endif
  return 0;
}
