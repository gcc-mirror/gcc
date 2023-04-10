/* { dg-options "-O" } */

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
#include "c-family/c-common.h"
#include "diagnostic.h"
#include "context.h"

int plugin_is_GPL_compatible;

/* A custom pass for injecting a crash in the middle-end when compiling
   certain functions.  */

const pass_data pass_data_crash_test =
{
  GIMPLE_PASS, /* type */
  "crash_test", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_crash_test : public gimple_opt_pass
{
public:
  pass_crash_test(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_crash_test, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override { return true; }
  unsigned int execute (function *) final override;

}; // class pass_test_groups

/* Determine if STMT is a call to a function named FUNCNAME.
   If so, return STMT as a gcall *.  Otherwise return NULL.  */

static gcall *
check_for_named_call (gimple *stmt, const char *funcname)
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

  return call;
}

unsigned int
pass_crash_test::execute (function *fun)
{
  gimple_stmt_iterator gsi;
  basic_block bb;

  FOR_EACH_BB_FN (bb, fun)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gimple *stmt = gsi_stmt (gsi);
	if (gcall *call = check_for_named_call (stmt, "inject_ice"))
	  {
	    input_location = stmt->location;
	    internal_error ("I'm sorry Dave, I'm afraid I can't do that");
	  }
	if (gcall *call = check_for_named_call (stmt,
						"inject_write_through_null"))
	  {
	    input_location = stmt->location;
	    int *p = NULL;
	    *p = 42;
	  }
      }

  return 0;
}

/* Entrypoint for the plugin.
   Create and register the custom pass.  */

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

  pass_info.pass = new pass_crash_test (g);
  pass_info.reference_pass_name = "*warn_function_noreturn";
  pass_info.ref_pass_instance_number = 1;
  pass_info.pos_op = PASS_POS_INSERT_AFTER;
  register_callback (plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL,
		     &pass_info);

  return 0;
}
