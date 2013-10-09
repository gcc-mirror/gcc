/* Top-level control of tree optimizations.
   Copyright (C) 2001-2013 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "tm_p.h"
#include "basic-block.h"
#include "flags.h"
#include "tree-ssa.h"
#include "function.h"
#include "langhooks.h"
#include "diagnostic-core.h"
#include "toplev.h"
#include "flags.h"
#include "cgraph.h"
#include "tree-inline.h"
#include "tree-pass.h"
#include "ggc.h"
#include "cgraph.h"
#include "cfgloop.h"
#include "except.h"
#include "plugin.h"


/* Pass: cleanup the CFG just before expanding trees to RTL.
   This is just a round of label cleanups and case node grouping
   because after the tree optimizers have run such cleanups may
   be necessary.  */

static unsigned int
execute_cleanup_cfg_post_optimizing (void)
{
  unsigned int todo = 0;
  if (cleanup_tree_cfg ())
    todo |= TODO_update_ssa;
  maybe_remove_unreachable_handlers ();
  cleanup_dead_labels ();
  group_case_labels ();
  if ((flag_compare_debug_opt || flag_compare_debug)
      && flag_dump_final_insns)
    {
      FILE *final_output = fopen (flag_dump_final_insns, "a");

      if (!final_output)
	{
	  error ("could not open final insn dump file %qs: %m",
		 flag_dump_final_insns);
	  flag_dump_final_insns = NULL;
	}
      else
	{
	  int save_unnumbered = flag_dump_unnumbered;
	  int save_noaddr = flag_dump_noaddr;

	  flag_dump_noaddr = flag_dump_unnumbered = 1;
	  fprintf (final_output, "\n");
	  dump_enumerated_decls (final_output, dump_flags | TDF_NOUID);
	  flag_dump_noaddr = save_noaddr;
	  flag_dump_unnumbered = save_unnumbered;
	  if (fclose (final_output))
	    {
	      error ("could not close final insn dump file %qs: %m",
		     flag_dump_final_insns);
	      flag_dump_final_insns = NULL;
	    }
	}
    }
  return todo;
}

namespace {

const pass_data pass_data_cleanup_cfg_post_optimizing =
{
  GIMPLE_PASS, /* type */
  "optimized", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  false, /* has_gate */
  true, /* has_execute */
  TV_TREE_CLEANUP_CFG, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_remove_unused_locals, /* todo_flags_finish */
};

class pass_cleanup_cfg_post_optimizing : public gimple_opt_pass
{
public:
  pass_cleanup_cfg_post_optimizing (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_cleanup_cfg_post_optimizing, ctxt)
  {}

  /* opt_pass methods: */
  unsigned int execute () {
    return execute_cleanup_cfg_post_optimizing ();
  }

}; // class pass_cleanup_cfg_post_optimizing

} // anon namespace

gimple_opt_pass *
make_pass_cleanup_cfg_post_optimizing (gcc::context *ctxt)
{
  return new pass_cleanup_cfg_post_optimizing (ctxt);
}


