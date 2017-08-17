/* Code coverage instrumentation for fuzzing.
   Copyright (C) 2015-2017 Free Software Foundation, Inc.
   Contributed by Dmitry Vyukov <dvyukov@google.com>

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "basic-block.h"
#include "options.h"
#include "flags.h"
#include "stmt.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-pass.h"
#include "tree-iterator.h"
#include "stringpool.h"
#include "attribs.h"
#include "asan.h"

namespace {

unsigned
sancov_pass (function *fun)
{
  initialize_sanitizer_builtins ();

  /* Insert callback into beginning of every BB. */
  tree fndecl = builtin_decl_implicit (BUILT_IN_SANITIZER_COV_TRACE_PC);
  basic_block bb;
  FOR_EACH_BB_FN (bb, fun)
    {
      gimple_stmt_iterator gsi = gsi_start_nondebug_after_labels_bb (bb);
      if (gsi_end_p (gsi))
	continue;
      gimple *stmt = gsi_stmt (gsi);
      gimple *gcall = gimple_build_call (fndecl, 0);
      gimple_set_location (gcall, gimple_location (stmt));
      gsi_insert_before (&gsi, gcall, GSI_SAME_STMT);
    }
  return 0;
}

template <bool O0> class pass_sancov : public gimple_opt_pass
{
public:
  pass_sancov (gcc::context *ctxt) : gimple_opt_pass (data, ctxt) {}

  static const pass_data data;
  opt_pass *
  clone ()
  {
    return new pass_sancov<O0> (m_ctxt);
  }
  virtual bool
  gate (function *)
  {
    return flag_sanitize_coverage && (!O0 || !optimize);
  }
  virtual unsigned int
  execute (function *fun)
  {
    return sancov_pass (fun);
  }
}; // class pass_sancov

template <bool O0>
const pass_data pass_sancov<O0>::data = {
  GIMPLE_PASS,		       /* type */
  O0 ? "sancov_O0" : "sancov", /* name */
  OPTGROUP_NONE,	       /* optinfo_flags */
  TV_NONE,		       /* tv_id */
  (PROP_cfg),		       /* properties_required */
  0,			       /* properties_provided */
  0,			       /* properties_destroyed */
  0,			       /* todo_flags_start */
  TODO_update_ssa,	     /* todo_flags_finish */
};

} // anon namespace

gimple_opt_pass *
make_pass_sancov (gcc::context *ctxt)
{
  return new pass_sancov<false> (ctxt);
}

gimple_opt_pass *
make_pass_sancov_O0 (gcc::context *ctxt)
{
  return new pass_sancov<true> (ctxt);
}
