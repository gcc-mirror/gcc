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

#if SWITCHABLE_TARGET
struct target_globals default_target_globals = {
  &default_target_flag_state,
  &default_target_regs
};

struct target_globals *
save_target_globals (void)
{
  struct target_globals *g;

  g = ggc_alloc_target_globals ();
  g->flag_state = XCNEW (struct target_flag_state);
  g->regs = XCNEW (struct target_regs);
  restore_target_globals (g);
  target_reinit ();
  return g;
}

#endif
