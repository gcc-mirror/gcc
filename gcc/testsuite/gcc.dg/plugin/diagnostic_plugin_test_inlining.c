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
#include "print-tree.h"
#include "cpplib.h"
#include "c-family/c-pragma.h"
#include "substring-locations.h"

int plugin_is_GPL_compatible;

/* A custom pass for emitting dummy warnings from the middle-end.  */

const pass_data pass_data_test_inlining =
{
  GIMPLE_PASS, /* type */
  "test_inlining", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_test_inlining : public gimple_opt_pass
{
public:
  pass_test_inlining(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_test_inlining, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) { return true; }
  virtual unsigned int execute (function *);

}; // class pass_test_inlining

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

/* Emit a warning at LOC.  */

static void
emit_warning (location_t loc)
{
  source_range src_range = get_range_from_loc (line_table, loc);
  warning_at (loc, 0, "range %i:%i-%i:%i",
	      LOCATION_LINE (src_range.m_start),
	      LOCATION_COLUMN (src_range.m_start),
	      LOCATION_LINE (src_range.m_finish),
	      LOCATION_COLUMN (src_range.m_finish));
}

/* Code for simulating the emission of a warning from the middle-end.
   Emit a warning for each call to a function named "__emit_warning".  */

static void
test_inlining (gimple *stmt)
{
  gcall *call = check_for_named_call (stmt, "__emit_warning", 1);
  if (!call)
    return;

  /* We expect an ADDR_EXPR with a STRING_CST inside it for the
     initial arg.  */
  tree t_addr_string = gimple_call_arg (call, 0);
  if (TREE_CODE (t_addr_string) != ADDR_EXPR)
    {
      error_at (call->location, "string literal required for arg 1");
      return;
    }

  tree t_string = TREE_OPERAND (t_addr_string, 0);
  if (TREE_CODE (t_string) != STRING_CST)
    {
      error_at (call->location, "string literal required for arg 1");
      return;
    }

  warning_at (call->location, 0, "%s",
	      TREE_STRING_POINTER (t_string));
}

/* Call test_inlining on every statement within FUN.  */

unsigned int
pass_test_inlining::execute (function *fun)
{
  gimple_stmt_iterator gsi;
  basic_block bb;

  FOR_EACH_BB_FN (bb, fun)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gimple *stmt = gsi_stmt (gsi);
	test_inlining (stmt);
      }

  return 0;
}

/* Entrypoint for the plugin.  Create and register the custom pass.  */

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

  pass_info.pass = new pass_test_inlining (g);
  pass_info.reference_pass_name = "*warn_function_noreturn";
  pass_info.ref_pass_instance_number = 1;
  pass_info.pos_op = PASS_POS_INSERT_AFTER;
  register_callback (plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL,
		     &pass_info);

  return 0;
}
