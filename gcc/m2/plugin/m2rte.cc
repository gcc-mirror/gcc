/* m2rte.cc a plugin to detect runtime exceptions at compiletime.

Copyright (C) 2017-2024 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


#include "gcc-plugin.h"
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "options.h"
#include "tree-pass.h"
#include "diagnostic-core.h"
#include "flags.h"
#include "intl.h"
#include "plugin.h"
#include "tree.h"
#include "gimple.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "gimple-pretty-print.h"
#include "plugin-version.h"
#include "diagnostic.h"
#include "context.h"

#include "rtegraph.h"
extern bool ggc_force_collect;
extern void ggc_collect (void);

#undef DEBUG_BASICBLOCK

int plugin_is_GPL_compatible;

void debug_tree (tree);

/* All dialects of Modula-2 issue some or all of these runtime error calls.
   This plugin detects whether a runtime error will be called in the first
   basic block of a reachable function.  */

static const char *m2_runtime_error_calls[] = {
  "m2pim_M2RTS_AssignmentException",
  "m2pim_M2RTS_ReturnException",
  "m2pim_M2RTS_IncException",
  "m2pim_M2RTS_DecException",
  "m2pim_M2RTS_InclException",
  "m2pim_M2RTS_ExclException",
  "m2pim_M2RTS_ShiftException",
  "m2pim_M2RTS_RotateException",
  "m2pim_M2RTS_StaticArraySubscriptException",
  "m2pim_M2RTS_DynamicArraySubscriptException",
  "m2pim_M2RTS_ForLoopBeginException",
  "m2pim_M2RTS_ForLoopToException",
  "m2pim_M2RTS_ForLoopEndException",
  "m2pim_M2RTS_PointerNilException",
  "m2pim_M2RTS_NoReturnException",
  "m2pim_M2RTS_CaseException",
  "m2pim_M2RTS_WholeNonPosDivException",
  "m2pim_M2RTS_WholeNonPosModException",
  "m2pim_M2RTS_WholeZeroDivException",
  "m2pim_M2RTS_WholeZeroRemException",
  "m2pim_M2RTS_WholeValueException",
  "m2pim_M2RTS_RealValueException",
  "m2pim_M2RTS_ParameterException",
  "m2pim_M2RTS_NoException",

  "m2iso_M2RTS_AssignmentException",
  "m2iso_M2RTS_ReturnException",
  "m2iso_M2RTS_IncException",
  "m2iso_M2RTS_DecException",
  "m2iso_M2RTS_InclException",
  "m2iso_M2RTS_ExclException",
  "m2iso_M2RTS_ShiftException",
  "m2iso_M2RTS_RotateException",
  "m2iso_M2RTS_StaticArraySubscriptException",
  "m2iso_M2RTS_DynamicArraySubscriptException",
  "m2iso_M2RTS_ForLoopBeginException",
  "m2iso_M2RTS_ForLoopToException",
  "m2iso_M2RTS_ForLoopEndException",
  "m2iso_M2RTS_PointerNilException",
  "m2iso_M2RTS_NoReturnException",
  "m2iso_M2RTS_CaseException",
  "m2iso_M2RTS_WholeNonPosDivException",
  "m2iso_M2RTS_WholeNonPosModException",
  "m2iso_M2RTS_WholeZeroDivException",
  "m2iso_M2RTS_WholeZeroRemException",
  "m2iso_M2RTS_WholeValueException",
  "m2iso_M2RTS_RealValueException",
  "m2iso_M2RTS_ParameterException",
  "m2iso_M2RTS_NoException",
  NULL,
};


#if defined(DEBUG_BASICBLOCK)
/* pretty_function display the name of the function.  */

static void
pretty_function (tree fndecl)
{
  if (fndecl != NULL && (DECL_NAME (fndecl) != NULL))
    {
      const char *n = IDENTIFIER_POINTER (DECL_NAME (fndecl));
      fprintf (stderr, "PROCEDURE %s ;\n", n);
    }
}
#endif

void
print_rtl (FILE *outf, const_rtx rtx_first);

/* strend returns true if string name has ending.  */

static bool
strend (const char *name, const char *ending)
{
  unsigned int len = strlen (name);
  return (len > strlen (ending)
	  && (strcmp (&name[len-strlen (ending)], ending) == 0));
}

/* is_constructor returns true if the function name is that of a module
   constructor or deconstructor.  */

static bool
is_constructor (tree fndecl)
{
  const char *name = IDENTIFIER_POINTER (DECL_NAME (fndecl));
  unsigned int len = strlen (name);

  return ((len > strlen ("_M2_"))
	  && (strncmp (name, "_M2_", strlen ("_M2_")) == 0)
	  && (strend (name, "_init") || strend (name, "_finish")));
}

/* is_external returns true if the function is extern.  */

static bool
is_external (tree function)
{
  return (! DECL_EXTERNAL (function))
    && TREE_PUBLIC (function)
    && TREE_STATIC (function);
}

/* is_external returns true if the function is a call to a Modula-2
   runtime exception handler.  */

static bool
is_rte (tree fndecl)
{
  const char *n = IDENTIFIER_POINTER (DECL_NAME (fndecl));

  for (int i = 0; m2_runtime_error_calls[i] != NULL; i++)
    if (strcmp (m2_runtime_error_calls[i], n) == 0)
      return true;
  return false;
}

/* examine_call extract the function tree from the gimple call
   statement and check whether it is a call to a runtime exception.  */

static void
examine_call (gimple *stmt)
{
  tree fndecl = gimple_call_fndecl (stmt);
  rtenode *func = rtegraph_lookup (stmt, fndecl, true);
  // rtegraph_dump ();
  if (fndecl != NULL && (DECL_NAME (fndecl) != NULL))
    {
      /* Firstly check if the function is a runtime exception.  */
      if (is_rte (fndecl))
	{
	  /* Remember runtime exception call.  */
	  rtegraph_include_rtscall (func);
	  /* Add the callee to the list of candidates to be queried reachable.  */
	  rtegraph_candidates_include (func);
	  return;
	}
    }
  /* Add it to the list of calls.  */
  rtegraph_include_function_call (func);
}


/* examine_function_decl, check if the current function is a module
   constructor/deconstructor.  Also check if the current function is
   declared as external.  */

static void
examine_function_decl (rtenode *rt)
{
  tree fndecl = rtegraph_get_func (rt);
  if (fndecl != NULL && (DECL_NAME (fndecl) != NULL))
    {
      /* Check if the function is a module constructor.  */
      if (is_constructor (fndecl))
	rtegraph_constructors_include (rt);
      /* Can it be called externally?  */
      if (is_external (fndecl))
	rtegraph_externs_include (rt);
    }
}


/* Check and warn if STMT is a self-assign statement.  */

static void
runtime_exception_inevitable (gimple *stmt)
{
  if (is_gimple_call (stmt))
    examine_call (stmt);
}


namespace {

const pass_data pass_data_exception_detection =
{
  GIMPLE_PASS, /* type */
  "runtime_exception_inevitable", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_gimple_lcf , /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_warn_exception_inevitable : public gimple_opt_pass
{
public:
  pass_warn_exception_inevitable(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_exception_detection, ctxt)
  {}

  virtual unsigned int execute (function *);
};

/* execute checks the first basic block of function fun to see if it
   calls a runtime exception.  */

unsigned int
pass_warn_exception_inevitable::execute (function *fun)
{
  gimple_stmt_iterator gsi;
  basic_block bb;
  /* Record a function declaration.  */
  rtenode *fn = rtegraph_lookup (fun->gimple_body, fun->decl, false);

  rtegraph_set_current_function (fn);
  /* Check if the current function is a module constructor/deconstructor.
     Also check if the current function is declared as external.  */
  examine_function_decl (fn);

#if defined(DEBUG_BASICBLOCK)
  pretty_function (fun->decl);
  int basic_count = 0;
#endif
  FOR_EACH_BB_FN (bb, fun)
    {
#if defined(DEBUG_BASICBLOCK)
      int stmt_count = 0;
#endif
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
#if defined(DEBUG_BASICBLOCK)
	  printf ("  [%d][%d]  [basic block][statement]\n",
		  basic_count, stmt_count);
	  stmt_count++;
#endif
	  runtime_exception_inevitable (gsi_stmt (gsi));
#if defined(DEBUG_BASICBLOCK)
	  debug (gsi_stmt (gsi));
#endif
	}
      /* We only care about the first basic block in each function.
         We could continue to search if this edge falls though (top
         of a loop for example) but for now this is cautiously safe.
         --fixme--  */
      return 0;
#if defined(DEBUG_BASICBLOCK)
      basic_count++;
#endif
    }
  return 0;
}

/* analyse_graph discovers any reachable call to a runtime exception in the
   first basic block of a reachable function.  It then calls rtegraph_finish
   to tidy up and return all dynamic memory used.  */

void analyse_graph (void *gcc_data, void *user_data)
{
  rtegraph_discover ();
  rtegraph_finish ();
}

} // anon namespace


static gimple_opt_pass *
make_pass_warn_exception_inevitable (gcc::context *ctxt)
{
  return new pass_warn_exception_inevitable (ctxt);
}


/* plugin_init, check the version and register the plugin.  */

int
plugin_init (struct plugin_name_args *plugin_info,
	     struct plugin_gcc_version *version)
{
  struct register_pass_info pass_info;
  const char *plugin_name = plugin_info->base_name;

  if (!plugin_default_version_check (version, &gcc_version))
    {
      fprintf (stderr, "incorrect GCC version (%s) this plugin was built for GCC version %s\n",
	       version->basever, gcc_version.basever);
      return 1;
    }

  /* Runtime exception inevitable detection.  This plugin is most effective if
     it is run after all optimizations.  This is plugged in at the end of
     gimple range of optimizations.  */
  pass_info.pass = make_pass_warn_exception_inevitable (g);
  pass_info.reference_pass_name = "*warn_function_noreturn";

  pass_info.ref_pass_instance_number = 1;
  pass_info.pos_op = PASS_POS_INSERT_AFTER;

  rtegraph_init ();

  register_callback (plugin_name,
		     PLUGIN_PASS_MANAGER_SETUP,
		     NULL,
		     &pass_info);
  register_callback (plugin_name,
		     PLUGIN_FINISH, analyse_graph, NULL);
  return 0;
}
