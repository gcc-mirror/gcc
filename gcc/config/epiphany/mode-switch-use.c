/* Insert USEs in instructions that require mode switching.
   This should probably be merged into mode-switching.c .
   Copyright (C) 2011-2014 Free Software Foundation, Inc.
   Contributed by Embecosm on behalf of Adapteva, Inc.

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
#include "rtl.h"
#include "function.h"
#include "emit-rtl.h"
#include "tree-pass.h"
#include "insn-attr.h"
#include "insn-config.h"
#include "recog.h"
#include "tm_p.h"
#include "df.h"

#ifndef TARGET_INSERT_MODE_SWITCH_USE
#define TARGET_INSERT_MODE_SWITCH_USE NULL
#endif

static unsigned int
insert_uses (void)
{
  static const int num_modes[] = NUM_MODES_FOR_MODE_SWITCHING;
#define N_ENTITIES ARRAY_SIZE (num_modes)
  int e;
  void (*target_insert_mode_switch_use) (rtx insn, int, int)
    = TARGET_INSERT_MODE_SWITCH_USE;

  for (e = N_ENTITIES - 1; e >= 0; e--)
    {
      int no_mode = num_modes[e];
      rtx insn;
      int mode;

      if (!OPTIMIZE_MODE_SWITCHING (e))
	continue;
      for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
	{
	  if (!INSN_P (insn))
	    continue;
	  mode = MODE_NEEDED (e, insn);
	  if (mode == no_mode)
	    continue;
	  if (target_insert_mode_switch_use)
	    {
	      target_insert_mode_switch_use (insn, e, mode);
	      df_insn_rescan (insn);
	    }
	}
    }
  return 0;
}

namespace {

const pass_data pass_data_mode_switch_use =
{
  RTL_PASS, /* type */
  "mode_switch_use", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_execute */
  TV_NONE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_mode_switch_use : public rtl_opt_pass
{
public:
  pass_mode_switch_use(gcc::context *ctxt)
    : rtl_opt_pass(pass_data_mode_switch_use, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *) { return insert_uses (); }

}; // class pass_mode_switch_use

} // anon namespace

rtl_opt_pass *
make_pass_mode_switch_use (gcc::context *ctxt)
{
  return new pass_mode_switch_use (ctxt);
}
