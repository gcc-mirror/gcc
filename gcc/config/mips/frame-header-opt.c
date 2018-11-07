/* Analyze functions to determine if callers need to allocate a frame header
   on the stack.  The frame header is used by callees to save their arguments.
   This optimization is specific to TARGET_OLDABI targets.  For TARGET_NEWABI
   targets, if a frame header is required, it is allocated by the callee.


   Copyright (C) 2015-2018 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "context.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "tree-core.h"
#include "tree-pass.h"
#include "target.h"
#include "target-globals.h"
#include "profile-count.h"
#include "cgraph.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"

static unsigned int frame_header_opt (void);

namespace {

const pass_data pass_data_ipa_frame_header_opt =
{
  IPA_PASS, /* type */
  "frame-header-opt", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_CGRAPHOPT, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_ipa_frame_header_opt : public ipa_opt_pass_d
{
public:
  pass_ipa_frame_header_opt (gcc::context *ctxt)
    : ipa_opt_pass_d (pass_data_ipa_frame_header_opt, ctxt,
                      NULL, /* generate_summary */
                      NULL, /* write_summary */
                      NULL, /* read_summary */
                      NULL, /* write_optimization_summary */
                      NULL, /* read_optimization_summary */
                      NULL, /* stmt_fixup */
                      0, /* function_transform_todo_flags_start */
                      NULL, /* function_transform */
                      NULL) /* variable_transform */
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      /* This optimization has no affect if TARGET_NEWABI.   If optimize
         is not at least 1 then the data needed for the optimization is
         not available and nothing will be done anyway.  */
      return TARGET_OLDABI && flag_frame_header_optimization && optimize > 0;
    }

  virtual unsigned int execute (function *) { return frame_header_opt (); }

}; // class pass_ipa_frame_header_opt

} // anon namespace

static ipa_opt_pass_d *
make_pass_ipa_frame_header_opt (gcc::context *ctxt)
{
  return new pass_ipa_frame_header_opt (ctxt);
}

void
mips_register_frame_header_opt (void)
{
  opt_pass *p = make_pass_ipa_frame_header_opt (g);
  struct register_pass_info f = { p, "comdats", 1, PASS_POS_INSERT_AFTER };
  register_pass (&f);
}


/* Return true if it is certain that this is a leaf function.  False if it is
   not a leaf function or if it is impossible to tell.  */

static bool
is_leaf_function (function *fn)
{
  basic_block bb;
  gimple_stmt_iterator gsi;

  /* If we do not have a cfg for this function be conservative and assume
     it is not a leaf function.  */
  if (fn->cfg == NULL)
    return false;

  FOR_EACH_BB_FN (bb, fn)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      if (is_gimple_call (gsi_stmt (gsi)))
	return false;
  return true;
}

/* Return true if this function has inline assembly code or if we cannot
   be certain that it does not.  False if we know that there is no inline
   assembly.  */

static bool
has_inlined_assembly (function *fn)
{
  basic_block bb;
  gimple_stmt_iterator gsi;

  /* If we do not have a cfg for this function be conservative and assume
     it is may have inline assembly.  */
  if (fn->cfg == NULL)
    return true;

  FOR_EACH_BB_FN (bb, fn)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      if (gimple_code (gsi_stmt (gsi)) == GIMPLE_ASM)
	return true;

  return false;
}

/* Return true if this function will use the stack space allocated by its
   caller or if we cannot determine for certain that it does not.  */

static bool
needs_frame_header_p (function *fn)
{
  tree t;

  if (fn->decl == NULL)
    return true;

  if (fn->stdarg)
    return true;

  for (t = DECL_ARGUMENTS (fn->decl); t; t = TREE_CHAIN (t))
    {
      if (!use_register_for_decl (t))
	return true;

      /* Some 64-bit types may get copied to general registers using the frame
	 header, see mips_output_64bit_xfer.  Checking for SImode only may be
         overly restrictive but it is guaranteed to be safe. */
      if (DECL_MODE (t) != SImode)
	return true;
    }

  return false;
}

/* Return true if the argument stack space allocated by function FN is used.
   Return false if the space is needed or if the need for the space cannot
   be determined.  */

static bool
callees_functions_use_frame_header (function *fn)
{
  basic_block bb;
  gimple_stmt_iterator gsi;
  gimple *stmt;
  tree called_fn_tree;
  function *called_fn;

  if (fn->cfg == NULL)
    return true;

  FOR_EACH_BB_FN (bb, fn)
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  stmt = gsi_stmt (gsi);
	  if (is_gimple_call (stmt))
	    {
	      called_fn_tree = gimple_call_fndecl (stmt);
	      if (called_fn_tree != NULL)
	        {
	          called_fn = DECL_STRUCT_FUNCTION (called_fn_tree);
		  if (called_fn == NULL
		      || DECL_WEAK (called_fn_tree) 
		      || has_inlined_assembly (called_fn)
		      || !is_leaf_function (called_fn)
		      || !called_fn->machine->does_not_use_frame_header)
		    return true;
	        }
	      else
		return true;
            }
        }
    }
  return false;
}

/* Set the callers_may_not_allocate_frame flag for any function which
   function FN calls because FN may not allocate a frame header.  */

static void
set_callers_may_not_allocate_frame (function *fn)
{
  basic_block bb;
  gimple_stmt_iterator gsi;
  gimple *stmt;
  tree called_fn_tree;
  function *called_fn;

  if (fn->cfg == NULL)
    return;

  FOR_EACH_BB_FN (bb, fn)
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  stmt = gsi_stmt (gsi);
	  if (is_gimple_call (stmt))
	    {
	      called_fn_tree = gimple_call_fndecl (stmt);
	      if (called_fn_tree != NULL)
	        {
	          called_fn = DECL_STRUCT_FUNCTION (called_fn_tree);
		  if (called_fn != NULL)
		    called_fn->machine->callers_may_not_allocate_frame = true;
	        }
            }
        }
    }
  return;
}

/* Scan each function to determine those that need its frame headers.  Perform
   a second scan to determine if the allocation can be skipped because none of
   their callees require the frame header.  */

static unsigned int
frame_header_opt ()
{
  struct cgraph_node *node;
  function *fn;

  FOR_EACH_DEFINED_FUNCTION (node)
    {
      fn = node->get_fun ();
      if (fn != NULL)
	fn->machine->does_not_use_frame_header = !needs_frame_header_p (fn);
    }

  FOR_EACH_DEFINED_FUNCTION (node)
    {
      fn = node->get_fun ();
      if (fn != NULL)
	fn->machine->optimize_call_stack
	  = !callees_functions_use_frame_header (fn) && !is_leaf_function (fn);
    }

  FOR_EACH_DEFINED_FUNCTION (node)
    {
      fn = node->get_fun ();
      if (fn != NULL && fn->machine->optimize_call_stack)
	set_callers_may_not_allocate_frame (fn);
    }

  return 0;
}
