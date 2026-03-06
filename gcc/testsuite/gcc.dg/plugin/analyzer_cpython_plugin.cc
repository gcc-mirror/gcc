/* -fanalyzer plugin for CPython extension modules  */
/* { dg-options "-g" } */

#include "analyzer/common.h"
#include "gcc-plugin.h"
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
#include "diagnostics/color.h"
#include "diagnostics/metadata.h"
#include "tristate.h"
#include "bitmap.h"
#include "selftest.h"
#include "function.h"
#include "json.h"
#include "analyzer/common.h"
#include "analyzer/analyzer-language.h"
#include "analyzer/analyzer-logging.h"
#include "ordered-hash-map.h"
#include "options.h"
#include "cgraph.h"
#include "cfg.h"
#include "digraph.h"
#include "analyzer/supergraph.h"
#include "sbitmap.h"
#include "context.h"
#include "channels.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"
#include "analyzer/call-details.h"
#include "analyzer/call-info.h"
#include "analyzer/exploded-graph.h"

int plugin_is_GPL_compatible;

#if ENABLE_ANALYZER
static GTY (()) hash_map<tree, tree> *analyzer_stashed_types;
static GTY (()) hash_map<tree, tree> *analyzer_stashed_globals;

namespace ana {
namespace cpython_plugin {

static tree
get_field_by_name (tree type, const char *name, bool complain = true)
{
  gcc_assert (type);
  gcc_assert (name);
  for (tree field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
    {
      if (TREE_CODE (field) == FIELD_DECL)
	if (tree id = DECL_NAME (field))
	  {
	    const char *field_name = IDENTIFIER_POINTER (id);
	    if (strcmp (field_name, name) == 0)
	      return field;
	  }

      /* Prior to python 3.11, ob_refcnt a field of PyObject.
	 In Python 3.11 ob_refcnt was moved to an anonymous union within
	 PyObject (as part of PEP 683 "Immortal Objects, Using a
	 Fixed Refcount").  */
      if (0 == strcmp (name, "ob_refcnt"))
	if (tree subfield = get_field_by_name (TREE_TYPE (field), name, false))
	  return subfield;
    }
  if (complain)
    inform (UNKNOWN_LOCATION, "could not find field %qs of CPython type %qT",
	    name, type);
  return NULL_TREE;
}

/* Global state and utils for working with the CPython API.

   Attempt to provide some isolation against changes to the API in
   different versions of CPython.

   This only persists during the analyzer, and thus doesn't need
   to interact with the garbage collector.  */

class api
{
public:
  bool init_from_stashed_types ();

  // Get RECORD_TYPE for PyObject
  tree
  get_type_PyObject () const
  {
    gcc_assert (m_type_PyObject);
    return m_type_PyObject;
  }

  // Get FIELD_DECL for PyObject's "ob_refcnt"
  tree
  get_field_PyObject_ob_refcnt () const
  {
    gcc_assert (m_field_PyObject_ob_refcnt);
    return m_field_PyObject_ob_refcnt;
  }

  // Get FIELD_DECL for PyObject's "ob_type"
  tree
  get_field_PyObject_ob_type () const
  {
    gcc_assert (m_field_PyObject_ob_type);
    return m_field_PyObject_ob_type;
  }

  // Get POINTER_TYPE for "PyObject *"
  tree
  get_type_PyObject_ptr () const
  {
    gcc_assert (m_type_PyObject_ptr);
    return m_type_PyObject_ptr;
  }

  // Get POINTER_TYPE for "PyObject **"
  tree
  get_type_PyObject_ptr_ptr () const
  {
    gcc_assert (m_type_PyObject_ptr_ptr);
    return m_type_PyObject_ptr_ptr;
  }

  // Get RECORD_TYPE for CPython's PyVarObject
  tree
  get_type_PyVarObject () const
  {
    gcc_assert (m_type_PyVarObject);
    return m_type_PyVarObject;
  }

  // Get FIELD_DECL for PyVarObject's "ob_size"
  tree
  get_field_PyVarObject_ob_size () const
  {
    gcc_assert (m_field_PyVarObject_ob_size);
    return m_field_PyVarObject_ob_size;
  }

  // Get RECORD_TYPE for CPython's PyListObject
  tree
  get_type_PyListObject () const
  {
    gcc_assert (m_type_PyListObject);
    return m_type_PyListObject;
  }

  // Get RECORD_TYPE for CPython's PyLongObject
  tree
  get_type_PyLongObject () const
  {
    gcc_assert (m_type_PyLongObject);
    return m_type_PyLongObject;
  }

  // Get FIELD_DECL for PyListObject's "ob_item"
  tree
  get_field_PyListObject_ob_item () const
  {
    gcc_assert (m_field_PyListObject_ob_item);
    return m_field_PyListObject_ob_item;
  }

  // Get VAR_DECL for CPython's global var PyList_Type
  tree
  get_vardecl_PyList_Type () const
  {
    gcc_assert (m_vardecl_PyList_Type);
    return m_vardecl_PyList_Type;
  }

  // Get VAR_DECL for CPython's global var PyLong_Type
  tree
  get_vardecl_PyLong_Type () const
  {
    gcc_assert (m_vardecl_PyLong_Type);
    return m_vardecl_PyLong_Type;
  }

  /* Get an ana::svalue for "sizeof (PyObject *)".  */
  const svalue *
  get_sval_sizeof_PyObject_ptr (region_model_manager *mgr) const
  {
    tree size_tree = TYPE_SIZE_UNIT (m_type_PyObject_ptr);
    const svalue *sizeof_sval = mgr->get_or_create_constant_svalue (size_tree);
    return sizeof_sval;
  }

  /* Update MODEL to set OB_BASE_REGION's ob_refcnt to 1.  */
  void
  init_ob_refcnt_field (region_model *model,
			const region *ob_base_region,
			region_model_context *ctxt)
  {
    region_model_manager *mgr = model->get_manager ();
    tree ob_refcnt_tree = get_field_PyObject_ob_refcnt ();
    const region *ob_refcnt_region
      = mgr->get_field_region (ob_base_region, ob_refcnt_tree);
    const svalue *refcnt_one_sval
      = mgr->get_or_create_int_cst (size_type_node, 1);
    model->set_value (ob_refcnt_region, refcnt_one_sval, ctxt);
  }

  /* Update MODEL to set OB_BASE_REGION's ob_type to point to
     PYTYPE_VAR_DECL_PTR.  */
  void
  set_ob_type_field (region_model *model,
		     const region *ob_base_region,
		     tree pytype_var_decl_ptr,
		     region_model_context *ctxt)
  {
    region_model_manager *mgr = model->get_manager ();
    const region *pylist_type_region
      = mgr->get_region_for_global (pytype_var_decl_ptr);
    tree pytype_var_decl_ptr_type
      = build_pointer_type (TREE_TYPE (pytype_var_decl_ptr));
    const svalue *pylist_type_ptr_sval
      = mgr->get_ptr_svalue (pytype_var_decl_ptr_type, pylist_type_region);
    tree ob_type_field = get_field_PyObject_ob_type ();
    const region *ob_type_region
      = mgr->get_field_region (ob_base_region, ob_type_field);
    model->set_value (ob_type_region, pylist_type_ptr_sval, ctxt);
  }

  /* Initialize OB_BASE_REGION as a PyObject_HEAD
     i.e. set "ob_refcnt" to 1 and "ob_type" to PYTYPE_VAR_DECL_PTR.  */
  void
  init_PyObject_HEAD (region_model *model,
		      const region *ob_base_region,
		      tree pytype_var_decl_ptr,
		      region_model_context *ctxt)
  {
    // Initialize ob_refcnt field to 1.
    init_ob_refcnt_field (model, ob_base_region, ctxt);

    /* Get pointer svalue for PYTYPE_VAR_DECL_PTR then
       assign it to ob_type field.   */
    set_ob_type_field (model, ob_base_region,
		       pytype_var_decl_ptr, ctxt);
  }

  // Get subregion for ob_refcnt within a PyObject instance
  const region *
  get_region_PyObject_ob_refcnt (region_model_manager *mgr,
				 const region *pyobject_instance_reg)
  {
    const region *ob_refcnt_region
      = mgr->get_field_region (pyobject_instance_reg,
			       m_field_PyObject_ob_refcnt);
    return ob_refcnt_region;
  }

  void
  do_Py_INCREF (region_model *model,
		const region *pyobject_instance_reg,
		region_model_context *ctxt)
  {
    region_model_manager *mgr = model->get_manager ();
    const region *ob_refcnt_region
      = get_region_PyObject_ob_refcnt (mgr, pyobject_instance_reg);
    inc_field_val (model, ob_refcnt_region, size_type_node, ctxt);
  }

  // Get subregion for ob_size within a PyVarObject instance
  const region *
  get_region_PyVarObject_ob_size (region_model_manager *mgr,
				  const region *pyvarobject_instance_reg)
  {
    const region *ob_size_region
      = mgr->get_field_region (pyvarobject_instance_reg,
			       m_field_PyVarObject_ob_size);
    return ob_size_region;
  }

  /* Attempt to get the subregion for the "ob_digit" field within
     PYLONG_REGION, a PyLongObject instance.  */
  const region *
  get_region_PyLongObject_ob_digit (region_model_manager *mgr,
				    const region *pylong_region)
  {
    if (tree ob_digit_field
	  = get_field_by_name (m_type_PyLongObject, "ob_digit", false))
      {
	const region *ob_digit_region
	  = mgr->get_field_region (pylong_region, ob_digit_field);
	return ob_digit_region;
      }

    /* TODO: https://github.com/python/cpython/pull/101292
       moved "ob_digit" from struct _longobject to a new field long_value
       of a new struct _PyLongValue.  */

    return nullptr;
  }

  /* Increment the value of FIELD_REGION in the MODEL by 1. Optionally
     capture the old and new svalues if OLD_SVAL and NEW_SVAL pointers are
     provided. */
  void
  inc_field_val (region_model *model,
		 const region *field_region, const tree type_node,
		 region_model_context *ctxt,
		 const svalue **old_sval = nullptr,
		 const svalue **new_sval = nullptr)
  {
    region_model_manager *mgr = model->get_manager ();
    const svalue *tmp_old_sval = model->get_store_value (field_region, ctxt);
    const svalue *one_sval = mgr->get_or_create_int_cst (type_node, 1);
    const svalue *tmp_new_sval
      = mgr->get_or_create_binop (type_node, PLUS_EXPR, tmp_old_sval, one_sval);

    model->set_value (field_region, tmp_new_sval, ctxt);

    if (old_sval)
      *old_sval = tmp_old_sval;

    if (new_sval)
      *new_sval = tmp_new_sval;
  }

private:
  // PyObject
  tree m_type_PyObject; // RECORD_TYPE
  tree m_field_PyObject_ob_refcnt; // FIELD_DECL
  tree m_field_PyObject_ob_type; // FIELD_DECL

  // POINTER_TYPE for "PyObject *"
  tree m_type_PyObject_ptr;

  // POINTER_TYPE for "PyObject **"
  tree m_type_PyObject_ptr_ptr;

  // PyVarObject
  tree m_type_PyVarObject; // RECORD_TYPE
  tree m_field_PyVarObject_ob_size; // FIELD_DECL

  // PyListObject
  tree m_type_PyListObject; // RECORD_TYPE
  tree m_field_PyListObject_ob_item; // FIELD_DECL
  tree m_vardecl_PyList_Type; // VAR_DECL for CPython's global var PyList_Type

  // PyLongObject
  tree m_type_PyLongObject; // RECORD_TYPE
  tree m_vardecl_PyLong_Type; // VAR_DECL for CPython's global var PyLong_Type

} api;

/* Retrieve the "ob_base" field's region from OBJECT_RECORD within
   NEW_OBJECT_REGION and set its value in the MODEL to PYOBJ_SVALUE. */
static const region *
get_ob_base_region (region_model_manager *mgr, region_model *model,
                   const region *new_object_region, tree object_record,
                   const svalue *pyobj_svalue, const call_details &cd)
{
  tree ob_base_tree = get_field_by_name (object_record, "ob_base");
  const region *ob_base_region
      = mgr->get_field_region (new_object_region, ob_base_tree);
  model->set_value (ob_base_region, pyobj_svalue, cd.get_ctxt ());
  return ob_base_region;
}

/* Initialize and retrieve a region within the MODEL for a PyObject
   and set its value to OBJECT_SVALUE. */
static const region *
init_pyobject_region (region_model_manager *mgr, region_model *model,
                      const svalue *object_svalue, const call_details &cd)
{
  const region *pyobject_region = model->get_or_create_region_for_heap_alloc (
      NULL, cd.get_ctxt (), true, &cd);
  model->set_value (pyobject_region, object_svalue, cd.get_ctxt ());
  return pyobject_region;
}

class pyobj_init_fail : public failed_call_info
{
public:
  pyobj_init_fail (const call_details &cd) : failed_call_info (cd) {}

  bool
  update_model (region_model *model, const exploded_edge *,
                region_model_context *ctxt) const final override
  {
    /* Return NULL; everything else is unchanged. */
    const call_details cd (get_call_details (model, ctxt));
    region_model_manager *mgr = cd.get_manager ();
    if (cd.get_lhs_type ())
      {
        const svalue *zero
            = mgr->get_or_create_int_cst (cd.get_lhs_type (), 0);
        model->set_value (cd.get_lhs_region (), zero, cd.get_ctxt ());
      }
    return true;
  }
};

class refcnt_mismatch : public pending_diagnostic_subclass<refcnt_mismatch>
{
public:
  refcnt_mismatch (const region *base_region,
		   const svalue *ob_refcnt,
		   const svalue *actual_refcnt,
		   tree reg_tree,
		   tree expected_refcnt_tree,
		   tree actual_refcnt_tree)
  : m_base_region (base_region),
    m_ob_refcnt (ob_refcnt),
    m_actual_refcnt (actual_refcnt),
    m_reg_tree (reg_tree),
    m_expected_refcnt_tree (expected_refcnt_tree),
    m_actual_refcnt_tree (actual_refcnt_tree)
  {
  }

  const char *
  get_kind () const final override
  {
    return "refcnt_mismatch";
  }

  bool
  operator== (const refcnt_mismatch &other) const
  {
    return (m_base_region == other.m_base_region
	    && m_ob_refcnt == other.m_ob_refcnt
	    && m_actual_refcnt == other.m_actual_refcnt);
  }

  int get_controlling_option () const final override
  {
    return 0;
  }

  bool
  emit (diagnostic_emission_context &ctxt) final override
  {
    bool warned;
    warned = ctxt.warn ("expected %qE to have "
			"reference count: %qE but ob_refcnt field is: %qE",
			m_reg_tree,
			m_actual_refcnt_tree,
			m_expected_refcnt_tree);
    return warned;
  }

  void mark_interesting_stuff (interesting_t *interest) final override
  {
    if (m_base_region)
      interest->add_region_creation (m_base_region);
  }

private:
  const region *m_base_region;
  const svalue *m_ob_refcnt;
  const svalue *m_actual_refcnt;
  tree m_reg_tree;
  tree m_expected_refcnt_tree;
  tree m_actual_refcnt_tree;
};

/* Retrieves the svalue associated with the ob_refcnt field of the base region.
 */
static const svalue *
retrieve_ob_refcnt_sval (const region *base_reg, const region_model *model,
			 region_model_context *ctxt)
{
  region_model_manager *mgr = model->get_manager ();
  tree ob_refcnt_tree = api.get_field_PyObject_ob_refcnt ();
  const region *ob_refcnt_region
      = mgr->get_field_region (base_reg, ob_refcnt_tree);
  const svalue *ob_refcnt_sval
      = model->get_store_value (ob_refcnt_region, ctxt);
  return ob_refcnt_sval;
}

static void
increment_region_refcnt (hash_map<const region *, int> &map, const region *key)
{
  bool existed;
  auto &refcnt = map.get_or_insert (key, &existed);
  refcnt = existed ? refcnt + 1 : 1;
}


/* Recursively fills in region_to_refcnt with the references owned by
   pyobj_ptr_sval.  */
static void
count_pyobj_references (const region_model *model,
			hash_map<const region *, int> &region_to_refcnt,
			const svalue *pyobj_ptr_sval,
			hash_set<const region *> &seen)
{
  if (!pyobj_ptr_sval)
    return;

  const auto *pyobj_region_sval = pyobj_ptr_sval->dyn_cast_region_svalue ();
  const auto *pyobj_initial_sval = pyobj_ptr_sval->dyn_cast_initial_svalue ();
  if (!pyobj_region_sval && !pyobj_initial_sval)
    return;

  // todo: support initial sval (e.g passed in as parameter)
  if (pyobj_initial_sval)
    {
      //     increment_region_refcnt (region_to_refcnt,
      // 		       pyobj_initial_sval->get_region ());
      return;
    }

  const region *pyobj_region = pyobj_region_sval->get_pointee ();
  if (!pyobj_region || seen.contains (pyobj_region))
    return;

  seen.add (pyobj_region);

  if (pyobj_ptr_sval->get_type () == api.get_type_PyObject_ptr ())
    increment_region_refcnt (region_to_refcnt, pyobj_region);

  const auto *curr_store = model->get_store ();
  const auto *retval_cluster = curr_store->get_cluster (pyobj_region);
  if (!retval_cluster)
    return;

  const auto &retval_binding_map = retval_cluster->get_map ();

  for (const auto &binding : retval_binding_map)
    {
      const svalue *binding_sval = binding.m_sval;
      const svalue *unwrapped_sval = binding_sval->unwrap_any_unmergeable ();
      const region *pointee = unwrapped_sval->maybe_get_region ();

      if (pointee && pointee->get_kind () == RK_HEAP_ALLOCATED)
	count_pyobj_references (model, region_to_refcnt, binding_sval, seen);
    }
}

/* Compare ob_refcnt field vs the actual reference count of a region */
static void
check_refcnt (const region_model *model,
	      const region_model *old_model,
	      region_model_context *ctxt,
	      const hash_map<const ana::region *,
			     int>::iterator::reference_pair region_refcnt)
{
  region_model_manager *mgr = model->get_manager ();
  const auto &curr_region = region_refcnt.first;
  const auto &actual_refcnt = region_refcnt.second;
  const svalue *ob_refcnt_sval
      = retrieve_ob_refcnt_sval (curr_region, model, ctxt);
  const svalue *actual_refcnt_sval = mgr->get_or_create_int_cst (
      ob_refcnt_sval->get_type (), actual_refcnt);

  if (ob_refcnt_sval != actual_refcnt_sval
      && ob_refcnt_sval->get_kind () != SK_UNKNOWN
      && actual_refcnt_sval->get_kind () != SK_UNKNOWN)
    {
      const svalue *curr_reg_sval
	= mgr->get_ptr_svalue (api.get_type_PyObject_ptr (), curr_region);
      tree reg_tree = old_model->get_representative_tree (curr_reg_sval);
      if (!reg_tree)
	return;
      tree expected_refcnt_tree
	= old_model->get_representative_tree (ob_refcnt_sval);
      if (!expected_refcnt_tree)
	return;
      tree actual_refcnt_tree
	= old_model->get_representative_tree (actual_refcnt_sval);
      if (!actual_refcnt_tree)
	return;

      const auto &eg = ctxt->get_eg ();
      auto pd = std::make_unique<refcnt_mismatch> (curr_region, ob_refcnt_sval,
						   actual_refcnt_sval,
						   reg_tree,
						   expected_refcnt_tree,
						   actual_refcnt_tree);
      if (pd && eg)
	ctxt->warn (std::move (pd),
		    make_ploc_fixer_for_epath_for_leak_diagnostic (*eg,
								   NULL_TREE));
    }
}

static void
check_refcnts (const region_model *model,
	       const region_model *old_model,
	       const svalue *retval,
	       region_model_context *ctxt,
	       hash_map<const region *, int> &region_to_refcnt)
{
  for (const auto &region_refcnt : region_to_refcnt)
    {
      check_refcnt (model, old_model, ctxt, region_refcnt);
    }
}

/* Validates the reference count of all Python objects. */
void
pyobj_refcnt_checker (const region_model *model,
		      const region_model *old_model,
		      const svalue *retval,
		      region_model_context *ctxt)
{
  if (!ctxt)
    return;

  hash_map<const region *, int> region_to_refcnt;
  hash_set<const region *> seen_regions;

  count_pyobj_references (model, region_to_refcnt, retval, seen_regions);
  check_refcnts (model, old_model, retval, ctxt, region_to_refcnt);
}

/* Counts the actual pyobject references from all clusters in the model's
 * store. */
static void
count_all_references (const region_model *model,
		      hash_map<const region *, int> &region_to_refcnt)
{
  for (const auto &cluster : *model->get_store ())
    {
      auto curr_region = cluster.first;
      if (curr_region->get_kind () != RK_HEAP_ALLOCATED)
	continue;

      increment_region_refcnt (region_to_refcnt, curr_region);

      auto binding_cluster = cluster.second;
      for (const auto &binding : binding_cluster->get_map ())
	{
	  const svalue *binding_sval = binding.m_sval;

	  const svalue *unwrapped_sval
	      = binding_sval->unwrap_any_unmergeable ();
	  // if (unwrapped_sval->get_type () != api.m_type_PyObject_ptr)
	  // continue;

	  const region *pointee = unwrapped_sval->maybe_get_region ();
	  if (!pointee || pointee->get_kind () != RK_HEAP_ALLOCATED)
	    continue;

	  increment_region_refcnt (region_to_refcnt, pointee);
	}
    }
}

static void
dump_refcnt_info (const hash_map<const region *, int> &region_to_refcnt,
		  const region_model *model,
		  region_model_context *ctxt)
{
  region_model_manager *mgr = model->get_manager ();
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_show_color (&pp) = pp_show_color (global_dc->get_reference_printer ());
  pp.set_output_stream (stderr);

  for (const auto &region_refcnt : region_to_refcnt)
    {
      auto region = region_refcnt.first;
      auto actual_refcnt = region_refcnt.second;
      const svalue *ob_refcnt_sval
	  = retrieve_ob_refcnt_sval (region, model, ctxt);
      const svalue *actual_refcnt_sval = mgr->get_or_create_int_cst (
	  ob_refcnt_sval->get_type (), actual_refcnt);

      region->dump_to_pp (&pp, true);
      pp_string (&pp, " — ob_refcnt: ");
      ob_refcnt_sval->dump_to_pp (&pp, true);
      pp_string (&pp, " actual refcnt: ");
      actual_refcnt_sval->dump_to_pp (&pp, true);
      pp_newline (&pp);
    }
  pp_string (&pp, "~~~~~~~~\n");
  pp_flush (&pp);
}

class kf_analyzer_cpython_dump_refcounts : public known_function
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
    hash_map<const region *, int> region_to_refcnt;
    count_all_references(model, region_to_refcnt);
    dump_refcnt_info(region_to_refcnt, model, ctxt);
  }
};

/* Some concessions were made to
simplify the analysis process when comparing kf_PyList_Append with the
real implementation. In particular, PyList_Append performs some
optimization internally to try and avoid calls to realloc if
possible. For simplicity, we assume that realloc is called every time.
Also, we grow the size by just 1 (to ensure enough space for adding a
new element) rather than abide by the heuristics that the actual implementation
follows. */
class kf_PyList_Append : public known_function
{
public:
  bool
  matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 2 && cd.arg_is_pointer_p (0)
            && cd.arg_is_pointer_p (1));
  }
  void impl_call_pre (const call_details &cd) const final override;
  void impl_call_post (const call_details &cd) const final override;
};

void
kf_PyList_Append::impl_call_pre (const call_details &cd) const
{
  region_model_manager *mgr = cd.get_manager ();
  region_model *model = cd.get_model ();

  const svalue *pylist_sval = cd.get_arg_svalue (0);
  const region *pylist_reg
      = model->deref_rvalue (pylist_sval, cd.get_arg_tree (0), cd.get_ctxt ());

  const svalue *newitem_sval = cd.get_arg_svalue (1);
  const region *newitem_reg
      = model->deref_rvalue (pylist_sval, cd.get_arg_tree (0), cd.get_ctxt ());

  // Skip checks if unknown etc
  if (pylist_sval->get_kind () != SK_REGION
      && pylist_sval->get_kind () != SK_CONSTANT)
    return;

  // PyList_Check
  tree ob_type_field = api.get_field_PyObject_ob_type ();
  const region *ob_type_region
      = mgr->get_field_region (pylist_reg, ob_type_field);
  const svalue *stored_sval
      = model->get_store_value (ob_type_region, cd.get_ctxt ());
  const region *pylist_type_region
    = mgr->get_region_for_global (api.get_vardecl_PyList_Type ());
  tree pylisttype_vardecl_ptr
    = build_pointer_type (TREE_TYPE (api.get_vardecl_PyList_Type ()));
  const svalue *pylist_type_ptr
      = mgr->get_ptr_svalue (pylisttype_vardecl_ptr, pylist_type_region);

  if (stored_sval != pylist_type_ptr)
    {
      // TODO: emit diagnostic -Wanalyzer-type-error
      cd.get_ctxt ()->terminate_path ();
      return;
    }

  // Check that new_item is not null.
  {
    const svalue *null_ptr
        = mgr->get_or_create_int_cst (newitem_sval->get_type (), 0);
    if (!model->add_constraint (newitem_sval, NE_EXPR, null_ptr,
                                cd.get_ctxt ()))
      {
        // TODO: emit diagnostic here
        cd.get_ctxt ()->terminate_path ();
        return;
      }
  }
}

void
kf_PyList_Append::impl_call_post (const call_details &cd) const
{
  /* Three custom subclasses of custom_edge_info, for handling the various
     outcomes of "realloc".  */

  /* Concrete custom_edge_info: a realloc call that fails, returning NULL.
   */
  class realloc_failure : public failed_call_info
  {
  public:
    realloc_failure (const call_details &cd) : failed_call_info (cd) {}

    bool
    update_model (region_model *model, const exploded_edge *,
                  region_model_context *ctxt) const final override
    {
      const call_details cd (get_call_details (model, ctxt));
      region_model_manager *mgr = cd.get_manager ();

      const svalue *pylist_sval = cd.get_arg_svalue (0);
      const region *pylist_reg = model->deref_rvalue (
          pylist_sval, cd.get_arg_tree (0), cd.get_ctxt ());

      /* Identify ob_item field and set it to NULL. */
      tree ob_item_field = api.get_field_PyListObject_ob_item ();
      const region *ob_item_reg
          = mgr->get_field_region (pylist_reg, ob_item_field);
      const svalue *old_ptr_sval
          = model->get_store_value (ob_item_reg, cd.get_ctxt ());

      if (const region_svalue *old_reg
          = old_ptr_sval->dyn_cast_region_svalue ())
        {
          const region *freed_reg = old_reg->get_pointee ();
          model->unbind_region_and_descendents (freed_reg, poison_kind::freed);
          model->unset_dynamic_extents (freed_reg);
        }

      const svalue *null_sval = mgr->get_or_create_null_ptr (api.get_type_PyObject_ptr_ptr ());
      model->set_value (ob_item_reg, null_sval, cd.get_ctxt ());

      if (cd.get_lhs_type ())
        {
          const svalue *neg_one
              = mgr->get_or_create_int_cst (cd.get_lhs_type (), -1);
          cd.maybe_set_lhs(neg_one);
        }
      return true;
    }
  };

  class realloc_success_no_move : public call_info
  {
  public:
    realloc_success_no_move (const call_details &cd) : call_info (cd) {}

    void
    print_desc (pretty_printer &pp) const final override
    {
      pp_printf (&pp,
		 "when %qE succeeds, without moving underlying buffer",
		 get_fndecl ());
    }

    bool
    update_model (region_model *model, const exploded_edge *,
                  region_model_context *ctxt) const final override
    {
      const call_details cd (get_call_details (model, ctxt));
      region_model_manager *mgr = cd.get_manager ();

      const svalue *pylist_sval = cd.get_arg_svalue (0);
      const region *pylist_reg = model->deref_rvalue (
          pylist_sval, cd.get_arg_tree (0), cd.get_ctxt ());

      const svalue *newitem_sval = cd.get_arg_svalue (1);
      const region *newitem_reg = model->deref_rvalue (
          newitem_sval, cd.get_arg_tree (1), cd.get_ctxt ());

      const region *ob_size_region
	= api.get_region_PyVarObject_ob_size (mgr, pylist_reg);
      const svalue *ob_size_sval = nullptr;
      const svalue *new_size_sval = nullptr;
      api.inc_field_val (model, ob_size_region, integer_type_node, ctxt,
			 &ob_size_sval, &new_size_sval);

      const svalue *sizeof_sval = mgr->get_or_create_cast (
          ob_size_sval->get_type (), api.get_sval_sizeof_PyObject_ptr (mgr));
      const svalue *num_allocated_bytes = mgr->get_or_create_binop (
          size_type_node, MULT_EXPR, sizeof_sval, new_size_sval);

      tree ob_item_field = api.get_field_PyListObject_ob_item ();
      const region *ob_item_region
          = mgr->get_field_region (pylist_reg, ob_item_field);
      const svalue *ob_item_ptr_sval
          = model->get_store_value (ob_item_region, cd.get_ctxt ());

      /* We can only grow in place with a non-NULL pointer and no unknown
       */
      {
        const svalue *null_ptr = mgr->get_or_create_null_ptr (api.get_type_PyObject_ptr_ptr ());
        if (!model->add_constraint (ob_item_ptr_sval, NE_EXPR, null_ptr,
                                    cd.get_ctxt ()))
          {
            return false;
          }
      }

      const unmergeable_svalue *underlying_svalue
          = ob_item_ptr_sval->dyn_cast_unmergeable_svalue ();
      const svalue *target_svalue = nullptr;
      const region_svalue *target_region_svalue = nullptr;

      if (underlying_svalue)
        {
          target_svalue = underlying_svalue->get_arg ();
          if (target_svalue->get_kind () != SK_REGION)
            {
              return false;
            }
        }
      else
        {
          if (ob_item_ptr_sval->get_kind () != SK_REGION)
            {
              return false;
            }
          target_svalue = ob_item_ptr_sval;
        }

      target_region_svalue = target_svalue->dyn_cast_region_svalue ();
      const region *curr_reg = target_region_svalue->get_pointee ();

      if (compat_types_p (num_allocated_bytes->get_type (), size_type_node))
        model->set_dynamic_extents (curr_reg, num_allocated_bytes, ctxt);

      model->set_value (ob_size_region, new_size_sval, ctxt);

      const svalue *offset_sval = mgr->get_or_create_binop (
          size_type_node, MULT_EXPR, sizeof_sval, ob_size_sval);
      const region *element_region
          = mgr->get_offset_region (curr_reg, api.get_type_PyObject_ptr_ptr (), offset_sval);
      model->set_value (element_region, newitem_sval, cd.get_ctxt ());

      // Increment ob_refcnt of appended item.
      api.do_Py_INCREF (model, newitem_reg, ctxt);

      if (cd.get_lhs_type ())
        {
          const svalue *zero
              = mgr->get_or_create_int_cst (cd.get_lhs_type (), 0);
          cd.maybe_set_lhs(zero);
        }
      return true;
    }
  };

  class realloc_success_move : public call_info
  {
  public:
    realloc_success_move (const call_details &cd) : call_info (cd) {}

    void
    print_desc (pretty_printer &pp) const final override
    {
      pp_printf (&pp,
		 "when %qE succeeds, moving buffer",
		 get_fndecl ());
    }

    bool
    update_model (region_model *model, const exploded_edge *,
                  region_model_context *ctxt) const final override
    {
      const call_details cd (get_call_details (model, ctxt));
      region_model_manager *mgr = cd.get_manager ();
      const svalue *pylist_sval = cd.get_arg_svalue (0);
      const region *pylist_reg = model->deref_rvalue (
          pylist_sval, cd.get_arg_tree (0), cd.get_ctxt ());

      const svalue *newitem_sval = cd.get_arg_svalue (1);
      const region *newitem_reg = model->deref_rvalue (
          newitem_sval, cd.get_arg_tree (1), cd.get_ctxt ());

      const region *ob_size_region
	= api.get_region_PyVarObject_ob_size (mgr, pylist_reg);
      const svalue *old_ob_size_sval = nullptr;
      const svalue *new_ob_size_sval = nullptr;
      api.inc_field_val (model, ob_size_region, integer_type_node, ctxt,
			 &old_ob_size_sval, &new_ob_size_sval);

      const svalue *sizeof_sval = mgr->get_or_create_cast (
          old_ob_size_sval->get_type (), api.get_sval_sizeof_PyObject_ptr (mgr));
      const svalue *new_size_sval = mgr->get_or_create_binop (
          size_type_node, MULT_EXPR, sizeof_sval, new_ob_size_sval);

      tree ob_item_field = api.get_field_PyListObject_ob_item ();
      const region *ob_item_reg
          = mgr->get_field_region (pylist_reg, ob_item_field);
      const svalue *old_ptr_sval
          = model->get_store_value (ob_item_reg, cd.get_ctxt ());

      /* Create the new region.  */
      const region *new_reg = model->get_or_create_region_for_heap_alloc (
          new_size_sval, cd.get_ctxt ());
      const svalue *new_ptr_sval
          = mgr->get_ptr_svalue (api.get_type_PyObject_ptr_ptr (), new_reg);
      if (!model->add_constraint (new_ptr_sval, NE_EXPR, old_ptr_sval,
                                  cd.get_ctxt ()))
        return false;

      if (const region_svalue *old_reg
          = old_ptr_sval->dyn_cast_region_svalue ())
        {
          const region *freed_reg = old_reg->get_pointee ();
          const svalue *old_size_sval = model->get_dynamic_extents (freed_reg);
          if (old_size_sval)
            {
              const svalue *copied_size_sval
                  = get_copied_size (model, old_size_sval, new_size_sval);
              const region *copied_old_reg = mgr->get_sized_region (
                  freed_reg, api.get_type_PyObject_ptr_ptr (), copied_size_sval);
              const svalue *buffer_content_sval
                  = model->get_store_value (copied_old_reg, cd.get_ctxt ());
              const region *copied_new_reg = mgr->get_sized_region (
                  new_reg, api.get_type_PyObject_ptr_ptr (), copied_size_sval);
              model->set_value (copied_new_reg, buffer_content_sval,
                                cd.get_ctxt ());
            }
          else
            {
              model->mark_region_as_unknown (freed_reg, cd.get_uncertainty ());
            }

          model->unbind_region_and_descendents (freed_reg, poison_kind::freed);
          model->unset_dynamic_extents (freed_reg);
        }

      const svalue *null_ptr = mgr->get_or_create_null_ptr (api.get_type_PyObject_ptr_ptr ());
      if (!model->add_constraint (new_ptr_sval, NE_EXPR, null_ptr,
                                  cd.get_ctxt ()))
        return false;

      model->set_value (ob_size_region, new_ob_size_sval, ctxt);
      model->set_value (ob_item_reg, new_ptr_sval, cd.get_ctxt ());

      const svalue *offset_sval = mgr->get_or_create_binop (
          size_type_node, MULT_EXPR, sizeof_sval, old_ob_size_sval);
      const region *element_region
          = mgr->get_offset_region (new_reg, api.get_type_PyObject_ptr_ptr (), offset_sval);
      model->set_value (element_region, newitem_sval, cd.get_ctxt ());

      // Increment ob_refcnt of appended item.
      api.do_Py_INCREF (model, newitem_reg, ctxt);

      if (cd.get_lhs_type ())
        {
          const svalue *zero
              = mgr->get_or_create_int_cst (cd.get_lhs_type (), 0);
          cd.maybe_set_lhs(zero);
        }
      return true;
    }

  private:
    /* Return the lesser of OLD_SIZE_SVAL and NEW_SIZE_SVAL.
       If unknown, OLD_SIZE_SVAL is returned.  */
    const svalue *
    get_copied_size (region_model *model, const svalue *old_size_sval,
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

  /* Body of kf_PyList_Append::impl_call_post.  */
  if (cd.get_ctxt ())
    {
      cd.get_ctxt ()->bifurcate (std::make_unique<realloc_failure> (cd));
      cd.get_ctxt ()->bifurcate (std::make_unique<realloc_success_no_move> (cd));
      cd.get_ctxt ()->bifurcate (std::make_unique<realloc_success_move> (cd));
      cd.get_ctxt ()->terminate_path ();
    }
}

class kf_PyList_New : public known_function
{
public:
  bool
  matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 1 && cd.arg_is_integral_p (0));
  }
  void impl_call_post (const call_details &cd) const final override;
};

void
kf_PyList_New::impl_call_post (const call_details &cd) const
{
  class success : public success_call_info
  {
  public:
    success (const call_details &cd) : success_call_info (cd) {}

    bool
    update_model (region_model *model, const exploded_edge *,
                  region_model_context *ctxt) const final override
    {
      const call_details cd (get_call_details (model, ctxt));
      region_model_manager *mgr = cd.get_manager ();

      const svalue *pyobj_svalue
	= mgr->get_or_create_unknown_svalue (api.get_type_PyObject ());
      const svalue *varobj_svalue
	= mgr->get_or_create_unknown_svalue (api.get_type_PyVarObject ());
      const svalue *pylist_svalue
	= mgr->get_or_create_unknown_svalue (api.get_type_PyListObject ());

      const svalue *size_sval = cd.get_arg_svalue (0);

      const region *pylist_region
          = init_pyobject_region (mgr, model, pylist_svalue, cd);

      /*
      typedef struct
      {
        PyObject_VAR_HEAD
        PyObject **ob_item;
        Py_ssize_t allocated;
      } PyListObject;
      */
      tree varobj_field = get_field_by_name (api.get_type_PyListObject (),
					     "ob_base");
      const region *varobj_region
          = mgr->get_field_region (pylist_region, varobj_field);
      model->set_value (varobj_region, varobj_svalue, cd.get_ctxt ());

      tree ob_item_field = api.get_field_PyListObject_ob_item ();
      const region *ob_item_region
          = mgr->get_field_region (pylist_region, ob_item_field);

      const svalue *zero_sval = mgr->get_or_create_int_cst (size_type_node, 0);
      const svalue *casted_size_sval
          = mgr->get_or_create_cast (size_type_node, size_sval);
      const svalue *size_cond_sval = mgr->get_or_create_binop (
          size_type_node, LE_EXPR, casted_size_sval, zero_sval);

      // if size <= 0, ob_item = NULL

      if (tree_int_cst_equal (size_cond_sval->maybe_get_constant (),
                              integer_one_node))
        {
          const svalue *null_sval
              = mgr->get_or_create_null_ptr (api.get_type_PyObject_ptr_ptr ());
          model->set_value (ob_item_region, null_sval, cd.get_ctxt ());
        }
      else // calloc
        {
          const svalue *sizeof_sval = mgr->get_or_create_cast (
              size_sval->get_type (), api.get_sval_sizeof_PyObject_ptr (mgr));
          const svalue *prod_sval = mgr->get_or_create_binop (
              size_type_node, MULT_EXPR, sizeof_sval, size_sval);
          const region *ob_item_sized_region
              = model->get_or_create_region_for_heap_alloc (prod_sval,
                                                            cd.get_ctxt ());
          model->zero_fill_region (ob_item_sized_region, cd.get_ctxt ());
          const svalue *ob_item_ptr_sval
              = mgr->get_ptr_svalue (api.get_type_PyObject_ptr_ptr (),
				     ob_item_sized_region);
          const svalue *ob_item_unmergeable
              = mgr->get_or_create_unmergeable (ob_item_ptr_sval);
          model->set_value (ob_item_region, ob_item_unmergeable,
                            cd.get_ctxt ());
        }

      /*
      typedef struct {
      PyObject ob_base;
      Py_ssize_t ob_size; // Number of items in variable part
      } PyVarObject;
      */
      const region *ob_base_region
	= get_ob_base_region (mgr, model, varobj_region,
			      api.get_type_PyVarObject (),
			      pyobj_svalue, cd);

      const region *ob_size_region
	= api.get_region_PyVarObject_ob_size (mgr, varobj_region);
      model->set_value (ob_size_region, size_sval, cd.get_ctxt ());

      api.init_PyObject_HEAD (model, ob_base_region,
			      api.get_vardecl_PyList_Type (), ctxt);

      if (cd.get_lhs_type ())
        {
          const svalue *ptr_sval
              = mgr->get_ptr_svalue (cd.get_lhs_type (), pylist_region);
          cd.maybe_set_lhs (ptr_sval);
        }
      return true;
    }
  };

  if (cd.get_ctxt ())
    {
      cd.get_ctxt ()->bifurcate (std::make_unique<pyobj_init_fail> (cd));
      cd.get_ctxt ()->bifurcate (std::make_unique<success> (cd));
      cd.get_ctxt ()->terminate_path ();
    }
}

class kf_PyLong_FromLong : public known_function
{
public:
  bool
  matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 1 && cd.arg_is_integral_p (0));
  }
  void impl_call_post (const call_details &cd) const final override;
};

void
kf_PyLong_FromLong::impl_call_post (const call_details &cd) const
{
  class success : public success_call_info
  {
  public:
    success (const call_details &cd) : success_call_info (cd) {}

    bool
    update_model (region_model *model, const exploded_edge *,
                  region_model_context *ctxt) const final override
    {
      const call_details cd (get_call_details (model, ctxt));
      region_model_manager *mgr = cd.get_manager ();

      const svalue *pyobj_svalue
	= mgr->get_or_create_unknown_svalue (api.get_type_PyObject ());
      const svalue *pylongobj_sval
	= mgr->get_or_create_unknown_svalue (api.get_type_PyLongObject ());

      const region *pylong_region
          = init_pyobject_region (mgr, model, pylongobj_sval, cd);

      // Create a region for the base PyObject within the PyLongObject.
      const region *ob_base_region = get_ob_base_region (
          mgr, model, pylong_region, api.get_type_PyLongObject (), pyobj_svalue, cd);

      api.init_PyObject_HEAD (model, ob_base_region,
			      api.get_vardecl_PyLong_Type (), ctxt);

      // Set the PyLongObject value.
      if (const region *ob_digit_region
	  = api.get_region_PyLongObject_ob_digit (mgr, pylong_region))
	{
	  const svalue *ob_digit_sval = cd.get_arg_svalue (0);
	  model->set_value (ob_digit_region, ob_digit_sval, cd.get_ctxt ());
	}

      if (cd.get_lhs_type ())
        {
          const svalue *ptr_sval
              = mgr->get_ptr_svalue (cd.get_lhs_type (), pylong_region);
          cd.maybe_set_lhs (ptr_sval);
        }
      return true;
    }
  };

  if (cd.get_ctxt ())
    {
      cd.get_ctxt ()->bifurcate (std::make_unique<pyobj_init_fail> (cd));
      cd.get_ctxt ()->bifurcate (std::make_unique<success> (cd));
      cd.get_ctxt ()->terminate_path ();
    }
}

static void
maybe_stash_named_type (logger *logger, const translation_unit &tu,
                        const char *name)
{
  LOG_FUNC_1 (logger, "name: %qs", name);
  if (!analyzer_stashed_types)
    analyzer_stashed_types = hash_map<tree, tree>::create_ggc ();

  tree id = get_identifier (name);
  if (tree t = tu.lookup_type_by_id (id))
    {
      gcc_assert (TREE_CODE (t) == RECORD_TYPE);
      analyzer_stashed_types->put (id, t);
      if (logger)
        logger->log ("found %qs: %qE", name, t);
    }
  else
    {
      if (logger)
        logger->log ("%qs: not found", name);
    }
}

static void
maybe_stash_global_var (logger *logger, const translation_unit &tu,
                        const char *name)
{
  LOG_FUNC_1 (logger, "name: %qs", name);
  if (!analyzer_stashed_globals)
    analyzer_stashed_globals = hash_map<tree, tree>::create_ggc ();

  tree id = get_identifier (name);
  if (tree t = tu.lookup_global_var_by_id (id))
    {
      gcc_assert (TREE_CODE (t) == VAR_DECL);
      analyzer_stashed_globals->put (id, t);
      if (logger)
        logger->log ("found %qs: %qE", name, t);
    }
  else
    {
      if (logger)
        logger->log ("%qs: not found", name);
    }
}

static void
stash_named_types (logger *logger, const translation_unit &tu)
{
  LOG_SCOPE (logger);

  maybe_stash_named_type (logger, tu, "PyObject");
  maybe_stash_named_type (logger, tu, "PyListObject");
  maybe_stash_named_type (logger, tu, "PyVarObject");
  maybe_stash_named_type (logger, tu, "PyLongObject");
}

static void
stash_global_vars (logger *logger, const translation_unit &tu)
{
  LOG_SCOPE (logger);

  maybe_stash_global_var (logger, tu, "PyLong_Type");
  maybe_stash_global_var (logger, tu, "PyList_Type");
}

static tree
get_stashed_type_by_name (const char *name)
{
  if (!analyzer_stashed_types)
    return NULL_TREE;
  tree id = get_identifier (name);
  if (tree *slot = analyzer_stashed_types->get (id))
    {
      gcc_assert (TREE_CODE (*slot) == RECORD_TYPE);
      return *slot;
    }
  inform (UNKNOWN_LOCATION, "could not find CPython type %qs", name);
  return NULL_TREE;
}

static tree
get_stashed_global_var_by_name (const char *name)
{
  if (!analyzer_stashed_globals)
    return NULL_TREE;
  tree id = get_identifier (name);
  if (tree *slot = analyzer_stashed_globals->get (id))
    {
      gcc_assert (TREE_CODE (*slot) == VAR_DECL);
      return *slot;
    }
  inform (UNKNOWN_LOCATION, "could not find CPython global %qs", name);
  return NULL_TREE;
}

/* Attempt to find the various types for the CPython API, which
   we hope were stashed when the frontend ran.

   Return true if we found enough to run the plugin,
   false if it doesn't make sense to run it.  */

bool
api::init_from_stashed_types ()
{
  memset (this, 0, sizeof (*this));

  m_type_PyObject = get_stashed_type_by_name ("PyObject");
  if (!m_type_PyObject)
    return false;
  gcc_assert (TREE_CODE (m_type_PyObject) == RECORD_TYPE);

  m_field_PyObject_ob_refcnt
    = get_field_by_name (m_type_PyObject, "ob_refcnt");
  if (!m_field_PyObject_ob_refcnt)
    return false;

  m_field_PyObject_ob_type
    = get_field_by_name (m_type_PyObject, "ob_type");
  if (!m_field_PyObject_ob_type)
    return false;

  /* PyVarObject.  */
  m_type_PyVarObject = get_stashed_type_by_name ("PyVarObject");
  if (!m_type_PyVarObject)
    return false;
  gcc_assert (TREE_CODE (m_type_PyVarObject) == RECORD_TYPE);
  m_field_PyVarObject_ob_size
    = get_field_by_name (m_type_PyVarObject, "ob_size");
  if (!m_field_PyVarObject_ob_size)
    return false;

  /* PyListObject.  */
  m_type_PyListObject = get_stashed_type_by_name ("PyListObject");
  if (!m_type_PyListObject)
    return false;
  m_field_PyListObject_ob_item
    = get_field_by_name (m_type_PyListObject, "ob_item");
  if (!m_field_PyListObject_ob_item)
    return false;

  m_vardecl_PyList_Type = get_stashed_global_var_by_name ("PyList_Type");
  if (!m_vardecl_PyList_Type)
    return false;

  /* PyLongObject.  */
  m_type_PyLongObject = get_stashed_type_by_name ("PyLongObject");
  if (!m_type_PyLongObject)
    return false;

  m_vardecl_PyLong_Type = get_stashed_global_var_by_name ("PyLong_Type");
  if (!m_vardecl_PyLong_Type)
    return false;

  m_type_PyObject_ptr = build_pointer_type (m_type_PyObject);
  m_type_PyObject_ptr_ptr = build_pointer_type (m_type_PyObject_ptr);

  return true;
}

void
sorry_no_cpython_plugin ()
{
  sorry ("%qs definitions not found."
	 " Please ensure to %qs.)",
	 "Python/C API", "#include <Python.h>");
}

namespace analyzer_events = ::gcc::topics::analyzer_events;

class analyzer_events_subscriber : public analyzer_events::subscriber
{
public:
  analyzer_events_subscriber ()
  : m_init_failed (false)
  {
  }

  void
  on_message (const analyzer_events::on_tu_finished &msg) final override
  {
    LOG_SCOPE (msg.m_logger);
    stash_named_types (msg.m_logger, msg.m_tu);
    stash_global_vars (msg.m_logger, msg.m_tu);
  }

  void
  on_message (const analyzer_events::on_ana_init &m) final override
  {
    LOG_SCOPE (m.get_logger ());

    if (!api.init_from_stashed_types ())
      {
	sorry_no_cpython_plugin ();
	m_init_failed = true;
	return;
      }
    gcc_assert (api.get_type_PyObject ());

    m.register_known_function ("PyList_Append",
			       std::make_unique<kf_PyList_Append> ());
    m.register_known_function ("PyList_New",
			       std::make_unique<kf_PyList_New> ());
    m.register_known_function ("PyLong_FromLong",
			       std::make_unique<kf_PyLong_FromLong> ());
    m.register_known_function
      ("__analyzer_cpython_dump_refcounts",
       std::make_unique<kf_analyzer_cpython_dump_refcounts> ());
  }

  void
  on_message (const analyzer_events::on_frame_popped &msg) final override
  {
    if (m_init_failed)
      return;
    pyobj_refcnt_checker (msg.m_new_model,
			  msg.m_old_model,
			  msg.m_retval,
			  msg.m_ctxt);
  }

private:
  bool m_init_failed;
} event_sub;

} // namespace ana::cpython_plugin
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
  g->get_channels ().analyzer_events_channel.add_subscriber
    (ana::cpython_plugin::event_sub);
#else
  sorry_no_analyzer ();
#endif
  return 0;
}
