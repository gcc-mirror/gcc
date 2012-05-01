/* Target-dependent globals.
   Copyright (C) 2010  Free Software Foundation, Inc.

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
  &default_target_builtins,
  &default_target_gcse,
  &default_target_bb_reorder,
  &default_target_lower_subreg
};

struct target_globals *
save_target_globals (void)
{
  struct target_globals *g;

  g = ggc_alloc_target_globals ();
  g->flag_state = XCNEW (struct target_flag_state);
  g->regs = XCNEW (struct target_regs);
  g->rtl = ggc_alloc_cleared_target_rtl ();
  g->hard_regs = XCNEW (struct target_hard_regs);
  g->reload = XCNEW (struct target_reload);
  g->expmed = XCNEW (struct target_expmed);
  g->optabs = XCNEW (struct target_optabs);
  g->libfuncs = ggc_alloc_cleared_target_libfuncs ();
  g->cfgloop = XCNEW (struct target_cfgloop);
  g->ira = XCNEW (struct target_ira);
  g->ira_int = XCNEW (struct target_ira_int);
  g->builtins = XCNEW (struct target_builtins);
  g->gcse = XCNEW (struct target_gcse);
  g->bb_reorder = XCNEW (struct target_bb_reorder);
  g->lower_subreg = XCNEW (struct target_lower_subreg);
  restore_target_globals (g);
  init_reg_sets ();
  target_reinit ();
  return g;
}

#endif
