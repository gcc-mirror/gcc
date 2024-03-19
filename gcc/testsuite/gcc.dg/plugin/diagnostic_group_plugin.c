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

/* A custom pass for emitting dummy warnings from the middle-end.  */

const pass_data pass_data_test_groups =
{
  GIMPLE_PASS, /* type */
  "test_groups", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_test_groups : public gimple_opt_pass
{
public:
  pass_test_groups(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_test_groups, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) { return true; }
  virtual unsigned int execute (function *);

}; // class pass_test_groups

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
test_groups (gimple *stmt)
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

  {
    auto_diagnostic_group d;
    if (warning_at (call->location, 0, "%s", call,
		    TREE_STRING_POINTER (t_string)))
      {
	inform (call->location, "message for note");
	inform (call->location, " some more detail");
	inform (call->location, "  yet more detail");
      }
  }
  inform (call->location, "an unrelated message");
}

/* Call test_groups on every statement within FUN.  */

unsigned int
pass_test_groups::execute (function *fun)
{
  gimple_stmt_iterator gsi;
  basic_block bb;

  FOR_EACH_BB_FN (bb, fun)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gimple *stmt = gsi_stmt (gsi);
	test_groups (stmt);
      }

  return 0;
}

/* Custom diagnostic callback, to avoid having the path in the
   expected output.  */

void
test_diagnostic_starter (diagnostic_context *context,
			 const diagnostic_info *diagnostic)
{
  pp_set_prefix (context->printer, xstrdup ("PREFIX: "));
}

/* Custom diagnostic callback, to avoid having the path in the
   expected output.  */

void
test_diagnostic_start_span_fn (diagnostic_context *context,
			       expanded_location exploc)
{
  pp_string (context->printer, "START_SPAN_FN: ");
  pp_newline (context->printer);
}

/* Custom output format subclass.  */

class test_output_format : public diagnostic_text_output_format
{
 public:
  test_output_format (diagnostic_context &context)
  : diagnostic_text_output_format (context)
  {}

  void on_begin_group () final override
  {
    /* Loudly announce a new diagnostic group.  */
    pp_string (m_context.printer,
	       "================================= BEGIN GROUP ==============================");
    pp_newline (m_context.printer);
  }
  void on_end_group () final override
  {
    /* Loudly announce the end of a diagnostic group.  */
    pp_set_prefix (m_context.printer, NULL);
    pp_string (m_context.printer,
	       "---------------------------------- END GROUP -------------------------------");
    pp_newline_and_flush (m_context.printer);
  }
};

/* Entrypoint for the plugin.
   Install custom callbacks into the global_dc.
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

  diagnostic_starter (global_dc) = test_diagnostic_starter;
  diagnostic_start_span (global_dc) = test_diagnostic_start_span_fn;
  global_dc->set_output_format (new test_output_format (*global_dc));

  pass_info.pass = new pass_test_groups (g);
  pass_info.reference_pass_name = "*warn_function_noreturn";
  pass_info.ref_pass_instance_number = 1;
  pass_info.pos_op = PASS_POS_INSERT_AFTER;
  register_callback (plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL,
		     &pass_info);

  return 0;
}
