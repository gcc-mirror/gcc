/* Adjust alignment for local variable.
   Copyright (C) 2020-2023 Free Software Foundation, Inc.
   Contributed by Kito Cheng <kito.cheng@sifive.com>

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
#include "target.h"
#include "tree.h"
#include "tree-pass.h"
#include "memmodel.h"
#include "tm_p.h"

namespace {

const pass_data pass_data_adjust_alignment =
{
  GIMPLE_PASS, /* type */
  "adjust_alignment", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_adjust_alignment : public gimple_opt_pass
{
public:
  pass_adjust_alignment (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_adjust_alignment, ctxt)
  {}

  unsigned int execute (function *) final override;
}; // class pass_adjust_alignment

} // anon namespace

/* Entry point to adjust_alignment pass.  */
unsigned int
pass_adjust_alignment::execute (function *fun)
{
  size_t i;
  tree var;

  FOR_EACH_LOCAL_DECL (fun, i, var)
    {
      /* Don't adjust aligment for static local var and hard register var.  */
      if (is_global_var (var) || DECL_HARD_REGISTER (var))
	continue;

      unsigned align = LOCAL_DECL_ALIGNMENT (var);

      /* Make sure alignment only increase.  */
      gcc_assert (align >= DECL_ALIGN (var));

      SET_DECL_ALIGN (var, align);
    }
  return 0;
}

gimple_opt_pass *
make_pass_adjust_alignment (gcc::context *ctxt)
{
  return new pass_adjust_alignment (ctxt);
}
