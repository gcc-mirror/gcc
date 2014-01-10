/* Target-dependent globals.
   Copyright (C) 2010-2014 Free Software Foundation, Inc.

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
#include "tm.h"
#include "insn-config.h"
#include "machmode.h"
#include "tree.h"
#include "ggc.h"
#include "toplev.h"
#include "target-globals.h"
#include "flags.h"
#include "regs.h"
#include "rtl.h"
#include "hard-reg-set.h"
#include "reload.h"
#include "expmed.h"
#include "expr.h"
#include "optabs.h"
#include "libfuncs.h"
#include "cfgloop.h"
#include "ira-int.h"
#include "lra-int.h"
#include "builtins.h"
#include "gcse.h"
#include "bb-reorder.h"
#include "lower-subreg.h"

#if SWITCHABLE_TARGET
struct target_globals default_target_globals = {
  &default_target_flag_state,
  &default_target_regs,
  &default_target_rtl,
  &default_target_hard_regs,
  &default_target_reload,
  &default_target_expmed,
  &default_target_optabs,
  &default_target_libfuncs,
  &default_target_cfgloop,
  &default_target_ira,
  &default_target_ira_int,
  &default_target_lra_int,
  &default_target_builtins,
  &default_target_gcse,
  &default_target_bb_reorder,
  &default_target_lower_subreg
};

struct target_globals *
save_target_globals (void)
{
  struct target_globals *g;
  struct target_globals_extra {
    struct target_globals g;
    struct target_flag_state flag_state;
    struct target_optabs optabs;
    struct target_cfgloop cfgloop;
    struct target_builtins builtins;
    struct target_gcse gcse;
    struct target_bb_reorder bb_reorder;
    struct target_lower_subreg lower_subreg;
  } *p;
  p = (struct target_globals_extra *)
      ggc_internal_cleared_alloc_stat (sizeof (struct target_globals_extra)
				       PASS_MEM_STAT);
  g = (struct target_globals *) p;
  g->flag_state = &p->flag_state;
  g->regs = ggc_internal_cleared_alloc_stat (sizeof (struct target_regs)
					     PASS_MEM_STAT);
  g->rtl = ggc_alloc_cleared_target_rtl ();
  g->hard_regs
    = ggc_internal_cleared_alloc_stat (sizeof (struct target_hard_regs)
				       PASS_MEM_STAT);
  g->reload = ggc_internal_cleared_alloc_stat (sizeof (struct target_reload)
					       PASS_MEM_STAT);
  g->expmed =  ggc_internal_cleared_alloc_stat (sizeof (struct target_expmed)
						PASS_MEM_STAT);
  g->optabs = &p->optabs;
  g->libfuncs = ggc_alloc_cleared_target_libfuncs ();
  g->cfgloop = &p->cfgloop;
  g->ira = ggc_internal_cleared_alloc_stat (sizeof (struct target_ira)
					    PASS_MEM_STAT);
  g->ira_int = ggc_internal_cleared_alloc_stat (sizeof (struct target_ira_int)
						PASS_MEM_STAT);
  g->lra_int = ggc_internal_cleared_alloc_stat (sizeof (struct target_lra_int)
						PASS_MEM_STAT);
  g->builtins = &p->builtins;
  g->gcse = &p->gcse;
  g->bb_reorder = &p->bb_reorder;
  g->lower_subreg = &p->lower_subreg;
  restore_target_globals (g);
  init_reg_sets ();
  target_reinit ();
  return g;
}

/* Like save_target_globals() above, but set *this_target_optabs
   correctly when a previous function has changed
   *this_target_optabs.  */

struct target_globals *
save_target_globals_default_opts ()
{
  struct target_globals *globals;

  if (optimization_current_node != optimization_default_node)
    {
      tree opts = optimization_current_node;
      /* Temporarily switch to the default optimization node, so that
	 *this_target_optabs is set to the default, not reflecting
	 whatever a previous function used for the optimize
	 attribute.  */
      optimization_current_node = optimization_default_node;
      cl_optimization_restore
	(&global_options,
	 TREE_OPTIMIZATION (optimization_default_node));
      globals = save_target_globals ();
      optimization_current_node = opts;
      cl_optimization_restore (&global_options,
			       TREE_OPTIMIZATION (opts));
      return globals;
    }
  return save_target_globals ();
}

#endif
