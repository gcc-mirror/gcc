/* coroutine expansion and optimisation passes.

   Copyright (C) 2018-2019 Free Software Foundation, Inc.

 Contributed by Iain Sandoe <iain@sandoe.co.uk> under contract to Facebook.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

// FIXME: minimise headers ..
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "pretty-print.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "internal-fn.h"
#include "langhooks.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "gimple-walk.h"
#include "tree-cfg.h"
#include "tree-into-ssa.h"
#include "tree-nested.h"
#include "stor-layout.h"
#include "common/common-target.h"
#include "lto-section-names.h"
#include "gimple-pretty-print.h"
#include "intl.h"
#include "stringpool.h"
#include "attribs.h"
#include "cfgloop.h"

/* Iterate the function exanding the IFNs.  */

static unsigned int
execute_expand_coro_ifns (void)
{
  basic_block bb;
  gimple_stmt_iterator gsi;
  FOR_EACH_BB_FN (bb, cfun)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi);)
      {
	gimple *stmt = gsi_stmt (gsi);
	if (!is_gimple_call (stmt) || !gimple_call_internal_p (stmt))
	  {
	  gsi_next (&gsi);
	  continue;
	  }
	switch (gimple_call_internal_fn (stmt))
	  {
	  case IFN_CO_FRAME:
	  case IFN_CO_YIELD:
	  case IFN_CO_ACTOR:
	    fprintf (stderr, "saw coro IFN %s in %s\n",
		     internal_fn_name (gimple_call_internal_fn (stmt)),
		     IDENTIFIER_POINTER (DECL_NAME (current_function_decl)));
	    //stmt = gimple_build_nop ();
	    gsi_remove(&gsi, true);
	    if (gsi_end_p (gsi))
	      break;
	    continue;
	  default:
	    gsi_next (&gsi);
	    break;
	  }
      }
  return 0;
}

namespace {

const pass_data pass_data_coroutine_expand_ifns  =
{
  GIMPLE_PASS, /* type */
  "coro-expand-ifns", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  0, /* PROP_cfg, *//* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0 /* maybe TODO_update_ssa | TODO_cleanup_cfg, *//* todo_flags_finish */
};

class pass_coroutine_expand_ifns : public gimple_opt_pass
{
public:
  pass_coroutine_expand_ifns  (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_coroutine_expand_ifns , ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return flag_coroutines; };

  virtual unsigned int execute (function *f ATTRIBUTE_UNUSED)
    {
      return execute_expand_coro_ifns ();
    }

}; // class pass_coroutine_expand_ifns

} // anon namespace

gimple_opt_pass *
make_pass_coroutine_expand_ifns (gcc::context *ctxt)
{
  return new pass_coroutine_expand_ifns (ctxt);
}
