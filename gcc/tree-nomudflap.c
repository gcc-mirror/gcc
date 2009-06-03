/* Mudflap: narrow-pointer bounds-checking by tree rewriting.
   Copyright (C) 2001, 2002, 2003, 2007, 2008, 2009
   Free Software Foundation, Inc.
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
#include "diagnostic.h"
#include "hashtab.h"
#include "output.h"
#include "varray.h"
#include "langhooks.h"
#include "tree-mudflap.h"
#include "tree-pass.h"
#include "ggc.h"
#include "toplev.h"



/* This file contains placeholder functions, to be used only for
   language processors that cannot handle tree-mudflap.c directly.
   (e.g. Fortran).  */

static void
nogo (void)
{
  internal_error ("mudflap: this language is not supported");
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

struct gimple_opt_pass pass_mudflap_1 = 
{
 {
  GIMPLE_PASS,
  "mudflap1",				/* name */
  gate_mudflap,                         /* gate */
  NULL,					/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_NONE,				/* tv_id */
  0,					/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0                                     /* todo_flags_finish */
 }
};

struct gimple_opt_pass pass_mudflap_2 = 
{
 {
  GIMPLE_PASS,
  "mudflap2",				/* name */
  gate_mudflap,                         /* gate */
  NULL,					/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_NONE,				/* tv_id */
  0,					/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0                                     /* todo_flags_finish */
 }
};

/* Instead of:
#include "gt-tree-mudflap.h"
We prepare a little dummy struct here.
*/

EXPORTED_CONST struct ggc_root_tab gt_ggc_r_gt_tree_mudflap_h[] = {
  LAST_GGC_ROOT_TAB
};
