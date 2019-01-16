/* run-rtl-passes.c - Run RTL passes directly from frontend
   Copyright (C) 2016-2019 Free Software Foundation, Inc.

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
#include "target.h"
#include "rtl.h"
#include "function.h"
#include "basic-block.h"
#include "tree-pass.h"
#include "context.h"
#include "pass_manager.h"
#include "bitmap.h"
#include "df.h"
#include "regs.h"
#include "output.h"
#include "debug.h" /* for debug_hooks.  */
#include "insn-attr-common.h" /* for INSN_SCHEDULING.  */
#include "insn-attr.h" /* for init_sched_attrs.  */
#include "run-rtl-passes.h"

/* Run the backend passes, starting at the given pass.
   Take ownership of INITIAL_PASS_NAME.  */

void
run_rtl_passes (char *initial_pass_name)
{
  cfun->pass_startwith = initial_pass_name;
  max_regno = max_reg_num ();

  /* cgraphunit.c normally handles this.  */
  switch_to_section (text_section);
  (*debug_hooks->assembly_start) ();

  /* Pass "expand" normally sets this up.  */
#ifdef INSN_SCHEDULING
  init_sched_attrs ();
#endif

  bitmap_obstack_initialize (NULL);
  bitmap_obstack_initialize (&reg_obstack);

  opt_pass *rest_of_compilation
    = g->get_passes ()->get_rest_of_compilation ();
  gcc_assert (rest_of_compilation);
  execute_pass_list (cfun, rest_of_compilation);

  opt_pass *clean_slate = g->get_passes ()->get_clean_slate ();
  gcc_assert (clean_slate);
  execute_pass_list (cfun, clean_slate);

  bitmap_obstack_release (&reg_obstack);

  cfun->curr_properties |= PROP_rtl;
}
