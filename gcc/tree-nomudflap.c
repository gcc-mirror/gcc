/* Mudflap: narrow-pointer bounds-checking by tree rewriting.
   Copyright (C) 2001-2013 Free Software Foundation, Inc.
   Contributed by Frank Ch. Eigler <fche@redhat.com>

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
#include "tree.h"
#include "tree-inline.h"
#include "gimple.h"
#include "hashtab.h"
#include "langhooks.h"
#include "tree-mudflap.h"
#include "tree-pass.h"
#include "ggc.h"
#include "diagnostic-core.h"



/* This file contains placeholder functions, to be used only for
   language processors that cannot handle tree-mudflap.c directly.
   (e.g. Fortran).  */

static void
nogo (void)
{
  sorry ("mudflap: this language is not supported");
}

void
mudflap_enqueue_decl (tree obj ATTRIBUTE_UNUSED)
{
  nogo ();
}

void
mudflap_enqueue_constant (tree obj ATTRIBUTE_UNUSED)
{
  nogo ();
}

void
mudflap_finish_file (void)
{
  nogo ();
}

int
mf_marked_p (tree t ATTRIBUTE_UNUSED)
{
  nogo ();
  return 0;
}

tree
mf_mark (tree t ATTRIBUTE_UNUSED)
{
  nogo ();
  return NULL;
}

/* The pass structures must exist, but need not do anything.  */

static bool
gate_mudflap (void)
{
  return flag_mudflap != 0;
}

namespace {

const pass_data pass_data_mudflap_1 =
{
  GIMPLE_PASS, /* type */
  "mudflap1", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_gate */
  false, /* has_execute */
  TV_NONE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_mudflap_1 : public gimple_opt_pass
{
public:
  pass_mudflap_1 (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_mudflap_1, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_mudflap (); }

}; // class pass_mudflap_1

} // anon namespace

gimple_opt_pass *
make_pass_mudflap_1 (gcc::context *ctxt)
{
  return new pass_mudflap_1 (ctxt);
}

namespace {

const pass_data pass_data_mudflap_2 =
{
  GIMPLE_PASS, /* type */
  "mudflap2", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_gate */
  false, /* has_execute */
  TV_NONE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_mudflap_2 : public gimple_opt_pass
{
public:
  pass_mudflap_2 (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_mudflap_2, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_mudflap (); }

}; // class pass_mudflap_2

} // anon namespace

gimple_opt_pass *
make_pass_mudflap_2 (gcc::context *ctxt)
{
  return new pass_mudflap_2 (ctxt);
}

/* Instead of:
#include "gt-tree-mudflap.h"
We prepare a little dummy struct here.
*/

EXPORTED_CONST struct ggc_root_tab gt_ggc_r_gt_tree_mudflap_h[] = {
  LAST_GGC_ROOT_TAB
};
