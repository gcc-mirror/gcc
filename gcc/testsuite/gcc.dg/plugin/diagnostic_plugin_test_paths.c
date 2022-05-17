/* { dg-options "-O" } */

/* This plugin exercises the path-printing code.

   The goal is to unit-test the path-printing code without needing any
   specific tests within the compiler's IR.  We can't use any real
   diagnostics for this, so we have to fake it, hence this plugin.  */

#include "gcc-plugin.h"
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "stringpool.h"
#include "toplev.h"
#include "basic-block.h"
#include "hash-table.h"
#include "vec.h"
#include "ggc.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "tree.h"
#include "tree-pass.h"
#include "intl.h"
#include "plugin-version.h"
#include "diagnostic.h"
#include "diagnostic-path.h"
#include "diagnostic-metadata.h"
#include "context.h"
#include "print-tree.h"
#include "gcc-rich-location.h"
#include "cgraph.h"

int plugin_is_GPL_compatible;

const pass_data pass_data_test_show_path =
{
  IPA_PASS, /* type */
  "test_show_path", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_test_show_path : public ipa_opt_pass_d
{
public:
  pass_test_show_path(gcc::context *ctxt)
    : ipa_opt_pass_d (pass_data_test_show_path, ctxt,
		      NULL, /* generate_summary */
		      NULL, /* write_summary */
		      NULL, /* read_summary */
		      NULL, /* write_optimization_summary */
		      NULL, /* read_optimization_summary */
		      NULL, /* stmt_fixup */
		      0, /* function_transform_todo_flags_start */
		      NULL, /* function_transform */
		      NULL) /* variable_transform */
  {}

  /* opt_pass methods: */
  bool gate (function *) { return true; }
  virtual unsigned int execute (function *);

}; // class pass_test_show_path

/* Determine if STMT is a call with NUM_ARGS arguments to a function
   named FUNCNAME.
   If so, return STMT as a gcall *.  Otherwise return NULL.  */

static gcall *
check_for_named_call (gimple *stmt,
		      const char *funcname, unsigned int num_args)
{
  gcc_assert (funcname);

  gcall *call = dyn_cast <gcall *> (stmt);
  if (!call)
    return NULL;

  tree fndecl = gimple_call_fndecl (call);
  if (!fndecl)
    return NULL;

  if (strcmp (IDENTIFIER_POINTER (DECL_NAME (fndecl)), funcname))
    return NULL;

  if (gimple_call_num_args (call) != num_args)
    {
      error_at (stmt->location, "expected number of args: %i (got %i)",
		num_args, gimple_call_num_args (call));
      return NULL;
    }

  return call;
}

/* Example 1: a purely intraprocedural path.  */

static void
example_1 ()
{
  gimple_stmt_iterator gsi;
  basic_block bb;

  gcall *call_to_PyList_Append = NULL;
  gcall *call_to_PyList_New = NULL;
  gcond *for_cond = NULL;
  function *example_a_fun = NULL;

  cgraph_node *node;
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (node)
    {
      function *fun = node->get_fun ();
      FOR_EACH_BB_FN (bb, fun)
	{
	  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      gimple *stmt = gsi_stmt (gsi);
	      if (gcall *call = check_for_named_call (stmt, "PyList_New", 1))
		{
		  call_to_PyList_New = call;
		  example_a_fun = fun;
		}
	      if (gcall *call = check_for_named_call (stmt, "PyList_Append", 2))
		call_to_PyList_Append = call;
	      if (gcond *cond = dyn_cast <gcond *> (stmt))
		for_cond = cond;
	    }
	}
    }

  if (call_to_PyList_New && for_cond && call_to_PyList_Append)
    {
      auto_diagnostic_group d;
      gcc_rich_location richloc (gimple_location (call_to_PyList_Append));
      simple_diagnostic_path path (global_dc->printer);
      diagnostic_event_id_t alloc_event_id
	= path.add_event (gimple_location (call_to_PyList_New),
			  example_a_fun->decl, 0,
			  "when %qs fails, returning NULL",
			  "PyList_New");
      path.add_event (gimple_location (for_cond),
		      example_a_fun->decl, 0,
		      "when %qs", "i < count");
      path.add_event (gimple_location (call_to_PyList_Append),
		      example_a_fun->decl, 0,
		      "when calling %qs, passing NULL from %@ as argument %i",
		      "PyList_Append", &alloc_event_id, 1);
      richloc.set_path (&path);
      error_at (&richloc,
		"passing NULL as argument %i to %qs"
		" which requires a non-NULL parameter",
		1, "PyList_Append");
    }
}

/* A (function, location_t) pair.  */

struct event_location_t
{
  event_location_t ()
  : m_fun (NULL), m_loc (UNKNOWN_LOCATION)
  {}

  event_location_t (function *fun, location_t loc)
  : m_fun (fun), m_loc (loc)
  {}

  void set (const gimple *stmt, function *fun)
  {
    m_fun = fun;
    m_loc = gimple_location (stmt);
  }

  function *m_fun;
  location_t m_loc;
};

/* If FUN's name matches FUNCNAME, write the function and its start location
   into *OUT_ENTRY.  */

static void
check_for_named_function (function *fun, const char *funcname,
			  event_location_t *out_entry)
{
  gcc_assert (fun);
  gcc_assert (funcname);

  if (strcmp (IDENTIFIER_POINTER (DECL_NAME (fun->decl)), funcname))
    return;

  *out_entry = event_location_t (fun, fun->function_start_locus);
}


/* Example 2: an interprocedural path.  */

class test_diagnostic_path : public simple_diagnostic_path
{
 public:
  test_diagnostic_path (pretty_printer *event_pp)
  : simple_diagnostic_path (event_pp)
  {
  }
  void add_entry (event_location_t evloc, int stack_depth,
		  const char *funcname)
  {
    gcc_assert (evloc.m_fun);
    add_event (evloc.m_loc, evloc.m_fun->decl, stack_depth,
	       "entering %qs", funcname);
  }

  void add_call (event_location_t call_evloc, int caller_stack_depth,
		 event_location_t callee_entry_evloc, const char *callee)
  {
    gcc_assert (call_evloc.m_fun);
    add_event (call_evloc.m_loc, call_evloc.m_fun->decl, caller_stack_depth,
	       "calling %qs", callee);
    add_entry (callee_entry_evloc, caller_stack_depth + 1, callee);
  }

  void add_leaf_call (event_location_t call_evloc, int caller_stack_depth,
		      const char *callee)
  {
    gcc_assert (call_evloc.m_fun);
    add_event (call_evloc.m_loc, call_evloc.m_fun->decl, caller_stack_depth,
	       "calling %qs", callee);
  }
};

static void
example_2 ()
{
  gimple_stmt_iterator gsi;
  basic_block bb;

  event_location_t entry_to_wrapped_malloc;
  event_location_t call_to_malloc;

  event_location_t entry_to_wrapped_free;
  event_location_t call_to_free;

  event_location_t entry_to_make_boxed_int;
  event_location_t call_to_wrapped_malloc;

  event_location_t entry_to_free_boxed_int;
  event_location_t call_to_wrapped_free;

  event_location_t entry_to_test;
  event_location_t call_to_make_boxed_int;
  event_location_t call_to_free_boxed_int;

  event_location_t call_to_missing_location;

  cgraph_node *node;
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (node)
    {
      function *fun = node->get_fun ();
      FOR_EACH_BB_FN (bb, fun)
	{
	  check_for_named_function (fun, "wrapped_malloc",
				    &entry_to_wrapped_malloc);
	  check_for_named_function (fun, "wrapped_free",
				    &entry_to_wrapped_free);
	  check_for_named_function (fun, "make_boxed_int",
				    &entry_to_make_boxed_int);
	  check_for_named_function (fun, "free_boxed_int",
				    &entry_to_free_boxed_int);
	  check_for_named_function (fun, "test",
				    &entry_to_test);

	  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      gimple *stmt = gsi_stmt (gsi);
	      if (gcall *call = check_for_named_call (stmt, "malloc", 1))
		call_to_malloc.set (call, fun);
	      if (gcall *call = check_for_named_call (stmt, "free", 1))
		call_to_free.set (call, fun);
	      if (gcall *call = check_for_named_call (stmt, "wrapped_malloc", 1))
		call_to_wrapped_malloc.set (call, fun);
	      if (gcall *call = check_for_named_call (stmt, "wrapped_free", 1))
		call_to_wrapped_free.set (call, fun);
	      if (gcall *call = check_for_named_call (stmt, "make_boxed_int", 1))
		call_to_make_boxed_int.set (call, fun);
	      if (gcall *call = check_for_named_call (stmt, "free_boxed_int", 1))
		call_to_free_boxed_int.set (call, fun);
	      if (gcall *call = check_for_named_call (stmt, "missing_location", 0))
		{
		  call_to_missing_location.set (call, fun);
		  /* Simulate an event that's missing a useful location_t.  */
		  call_to_missing_location.m_loc = UNKNOWN_LOCATION;
		}
	    }
	}
    }

  if (call_to_malloc.m_fun)
    {
      auto_diagnostic_group d;

      gcc_rich_location richloc (call_to_free.m_loc);
      test_diagnostic_path path (global_dc->printer);
      path.add_entry (entry_to_test, 0, "test");
      path.add_call (call_to_make_boxed_int, 0,
		     entry_to_make_boxed_int, "make_boxed_int");
      path.add_call (call_to_wrapped_malloc, 1,
		     entry_to_wrapped_malloc, "wrapped_malloc");
      path.add_leaf_call (call_to_malloc, 2, "malloc");

      for (int i = 0; i < 2; i++)
	{
	  path.add_call (call_to_free_boxed_int, 0,
			 entry_to_free_boxed_int, "free_boxed_int");
	  path.add_call (call_to_wrapped_free, 1,
			 entry_to_wrapped_free, "wrapped_free");
	  path.add_leaf_call (call_to_free, 2, "free");
	  if (i == 0 && call_to_missing_location.m_fun)
	    path.add_leaf_call (call_to_missing_location, 0,
				"missing_location");
	}

      richloc.set_path (&path);

      diagnostic_metadata m;
      m.add_cwe (415); /* CWE-415: Double Free.  */

      warning_meta (&richloc, m, 0,
		    "double-free of %qs", "ptr");
    }
}

/* Example 3: an interprocedural path with a callback.  */

static void
example_3 ()
{
  gimple_stmt_iterator gsi;
  basic_block bb;

  event_location_t entry_to_custom_logger;
  event_location_t call_to_fprintf;

  event_location_t entry_to_int_handler;
  event_location_t call_to_custom_logger;

  event_location_t entry_to_register_handler;
  event_location_t call_to_signal;

  event_location_t entry_to_test;
  event_location_t call_to_register_handler;

  cgraph_node *node;
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (node)
    {
      function *fun = node->get_fun ();
      FOR_EACH_BB_FN (bb, fun)
	{
	  check_for_named_function (fun, "custom_logger",
				    &entry_to_custom_logger);
	  check_for_named_function (fun, "int_handler",
				    &entry_to_int_handler);
	  check_for_named_function (fun, "register_handler",
				    &entry_to_register_handler);
	  check_for_named_function (fun, "test",
				    &entry_to_test);
	  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      gimple *stmt = gsi_stmt (gsi);
	      if (gcall *call = check_for_named_call (stmt, "fprintf", 3))
		call_to_fprintf.set (call, fun);
	      if (gcall *call = check_for_named_call (stmt, "custom_logger", 1))
		call_to_custom_logger.set (call, fun);
	      if (gcall *call = check_for_named_call (stmt, "register_handler",
						      0))
		call_to_register_handler.set (call, fun);
	      if (gcall *call = check_for_named_call (stmt, "signal", 2))
		call_to_signal.set (call, fun);
	    }
	}
    }

  if (call_to_fprintf.m_fun)
    {
      auto_diagnostic_group d;

      gcc_rich_location richloc (call_to_fprintf.m_loc);
      test_diagnostic_path path (global_dc->printer);
      path.add_entry (entry_to_test, 1, "test");
      path.add_call (call_to_register_handler, 1,
		     entry_to_register_handler, "register_handler");
      path.add_event (call_to_signal.m_loc, call_to_signal.m_fun->decl,
		      2, "registering 'int_handler' as signal handler");
      path.add_event (UNKNOWN_LOCATION, NULL_TREE, 0,
		      "later on, when the signal is delivered to the process");
      path.add_entry (entry_to_int_handler, 1, "int_handler");
      path.add_call (call_to_custom_logger, 1,
		     entry_to_custom_logger, "custom_logger");
      path.add_leaf_call (call_to_fprintf, 2, "fprintf");

      richloc.set_path (&path);

      diagnostic_metadata m;
      /* CWE-479: Signal Handler Use of a Non-reentrant Function.  */
      m.add_cwe (479);

      warning_meta (&richloc, m, 0,
		    "call to %qs from within signal handler",
		    "fprintf");
    }
}

unsigned int
pass_test_show_path::execute (function *)
{
  example_1 ();
  example_2 ();
  example_3 ();

  return 0;
}

static opt_pass *
make_pass_test_show_path (gcc::context *ctxt)
{
  return new pass_test_show_path (ctxt);
}

int
plugin_init (struct plugin_name_args *plugin_info,
	     struct plugin_gcc_version *version)
{
  struct register_pass_info pass_info;
  const char *plugin_name = plugin_info->base_name;
  int argc = plugin_info->argc;
  struct plugin_argument *argv = plugin_info->argv;

  if (!plugin_default_version_check (version, &gcc_version))
    return 1;

  global_dc->caret_max_width = 80;

  pass_info.pass = make_pass_test_show_path (g);
  pass_info.reference_pass_name = "whole-program";
  pass_info.ref_pass_instance_number = 1;
  pass_info.pos_op = PASS_POS_INSERT_BEFORE;
  register_callback (plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL,
		     &pass_info);

  return 0;
}
