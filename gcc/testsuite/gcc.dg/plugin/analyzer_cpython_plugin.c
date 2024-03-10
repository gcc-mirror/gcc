/* -fanalyzer plugin for CPython extension modules  */
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
#include "analyzer/analyzer-language.h"
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
static GTY (()) hash_map<tree, tree> *analyzer_stashed_types;
static GTY (()) hash_map<tree, tree> *analyzer_stashed_globals;

namespace ana
{
static tree pyobj_record = NULL_TREE;
static tree varobj_record = NULL_TREE;
static tree pylistobj_record = NULL_TREE;
static tree pylongobj_record = NULL_TREE;
static tree pylongtype_vardecl = NULL_TREE;
static tree pylisttype_vardecl = NULL_TREE;

static tree
get_field_by_name (tree type, const char *name)
{
  for (tree field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
    {
      if (TREE_CODE (field) == FIELD_DECL)
        {
          const char *field_name = IDENTIFIER_POINTER (DECL_NAME (field));
          if (strcmp (field_name, name) == 0)
            return field;
        }
    }
  return NULL_TREE;
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
  return NULL_TREE;
}

static void
init_py_structs ()
{
  pyobj_record = get_stashed_type_by_name ("PyObject");
  varobj_record = get_stashed_type_by_name ("PyVarObject");
  pylistobj_record = get_stashed_type_by_name ("PyListObject");
  pylongobj_record = get_stashed_type_by_name ("PyLongObject");
  pylongtype_vardecl = get_stashed_global_var_by_name ("PyLong_Type");
  pylisttype_vardecl = get_stashed_global_var_by_name ("PyList_Type");
}

void
sorry_no_cpython_plugin ()
{
  sorry ("%qs definitions not found."
	 " Please ensure to %qs.)",
	 "Python/C API", "#include <Python.h>");
}

static void
cpython_analyzer_init_cb (void *gcc_data, void * /*user_data */)
{
  ana::plugin_analyzer_init_iface *iface
      = (ana::plugin_analyzer_init_iface *)gcc_data;
  LOG_SCOPE (iface->get_logger ());
  if (0)
    inform (input_location, "got here: cpython_analyzer_init_cb");

  init_py_structs ();

  if (pyobj_record == NULL_TREE)
    {
      sorry_no_cpython_plugin ();
      return;
    }
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
  ana::register_finish_translation_unit_callback (&stash_named_types);
  ana::register_finish_translation_unit_callback (&stash_global_vars);
  register_callback (plugin_info->base_name, PLUGIN_ANALYZER_INIT,
                     ana::cpython_analyzer_init_cb,
                     NULL); /* void *user_data */
#else
  sorry_no_analyzer ();
#endif
  return 0;
}