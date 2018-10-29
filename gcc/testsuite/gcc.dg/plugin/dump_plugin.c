/* Plugin for testing dumpfile.c.  */

#include "gcc-plugin.h"
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "tree-pass.h"
#include "intl.h"
#include "plugin-version.h"
#include "diagnostic.h"
#include "context.h"
#include "optinfo.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "cgraph.h"

int plugin_is_GPL_compatible;

const pass_data pass_data_test_dumping =
{
  GIMPLE_PASS, /* type */
  "test_dumping", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_test_dumping : public gimple_opt_pass
{
public:
  pass_test_dumping (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_test_dumping, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) { return true; }
  virtual unsigned int execute (function *);

}; // class pass_test_dumping

unsigned int
pass_test_dumping::execute (function *fun)
{
  basic_block bb;

  if (!dump_enabled_p ())
    return 0;

  FOR_ALL_BB_FN (bb, fun)
    for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
	 !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gimple *stmt = gsi_stmt (gsi);
	gcall *call = dyn_cast <gcall *> (stmt);
	if (!call)
	  continue;
	tree callee_decl = gimple_call_fndecl (call);
	if (!callee_decl)
	  continue;
	tree callee_name = DECL_NAME (callee_decl);
	if (!callee_name)
	  continue;
	const char *callee = IDENTIFIER_POINTER (callee_name);

	/* Various dumping tests, done at callsites,
	   controlled by the callee name.  */
	if (strcmp (callee, "test_string_literal") == 0)
	  dump_printf_loc (MSG_NOTE, stmt, "test of dump for %qs\n",
			   callee);
	else if (strcmp (callee, "test_tree") == 0)
	  dump_printf_loc (MSG_NOTE, stmt, "test of tree: %T\n",
			   integer_zero_node);
	else if (strcmp (callee, "test_gimple") == 0)
	  dump_printf_loc (MSG_NOTE, stmt, "test of gimple: %G", stmt);
	else if (strcmp (callee, "test_cgraph_node") == 0)
	  {
	    dump_printf_loc (MSG_NOTE, stmt, "test of callgraph node: ");
	    dump_symtab_node (MSG_NOTE, cgraph_node::get (callee_decl));
	    dump_printf (MSG_NOTE, "\n");
	  }
	else if (strcmp (callee, "test_wide_int") == 0)
	  {
	    HOST_WIDE_INT val = 0;
	    dump_printf_loc (MSG_NOTE, stmt,
			     "test of wide int: " HOST_WIDE_INT_PRINT_DEC "\n",
			     val);
	  }
	else if (strcmp (callee, "test_poly_int") == 0)
	  {
	    dump_printf_loc (MSG_NOTE, stmt, "test of poly int: ");
	    dump_dec (MSG_NOTE, poly_int64 (42));
	    dump_printf (MSG_NOTE, "\n");
	  }
	else if (strcmp (callee, "test_scopes") == 0)
	  {
	    AUTO_DUMP_SCOPE ("outer scope", stmt);
	    {
	      dump_printf_loc (MSG_NOTE, stmt, "at outer scope\n");
	      AUTO_DUMP_SCOPE ("middle scope", stmt);
	      {
		dump_printf_loc (MSG_NOTE, stmt, "at middle scope\n");
		AUTO_DUMP_SCOPE ("innermost scope", stmt);
		dump_printf_loc (MSG_NOTE, stmt, "at innermost scope\n");
	      }
	    }
	  }
      }

  return 0;
}

static gimple_opt_pass *
make_pass_test_dumping (gcc::context *ctxt)
{
  return new pass_test_dumping (ctxt);
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

  pass_info.pass = make_pass_test_dumping (g);
  pass_info.reference_pass_name = "ssa";
  pass_info.ref_pass_instance_number = 1;
  pass_info.pos_op = PASS_POS_INSERT_AFTER;
  register_callback (plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL,
		     &pass_info);

  return 0;
}
