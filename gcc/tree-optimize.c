/* Control and data flow functions for trees.
   Copyright 2001, 2002, 2003, 2004 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "toplev.h"
#include "tree.h"
#include "tree-inline.h"
#include "flags.h"
#include "langhooks.h"
#include "cgraph.h"
#include "timevar.h"
#include "tm.h"
#include "function.h"
#include "ggc.h"


/* Called to move the SAVE_EXPRs for parameter declarations in a
   nested function into the nested function.  DATA is really the
   nested FUNCTION_DECL.  */

static tree
set_save_expr_context (tree *tp,
		       int *walk_subtrees,
		       void *data)
{
  if (TREE_CODE (*tp) == SAVE_EXPR && !SAVE_EXPR_CONTEXT (*tp))
    SAVE_EXPR_CONTEXT (*tp) = (tree) data;
  /* Do not walk back into the SAVE_EXPR_CONTEXT; that will cause
     circularity.  */
  else if (DECL_P (*tp))
    *walk_subtrees = 0;

  return NULL;
}

/* Clear out the DECL_RTL for the non-static local variables in BLOCK and
   its sub-blocks.  DATA is the decl of the function being processed.  */

static tree
clear_decl_rtl (tree *tp, int *walk_subtrees ATTRIBUTE_UNUSED, void *data)
{
  bool nonstatic_p, local_p;
  tree t = *tp;

  switch (TREE_CODE (t))
    {
    case VAR_DECL:
      nonstatic_p = !TREE_STATIC (t) && !DECL_EXTERNAL (t);
      local_p = decl_function_context (t) == data;
      break;

    case PARM_DECL:
    case LABEL_DECL:
      nonstatic_p = true;
      local_p = decl_function_context (t) == data;
      break;

    case RESULT_DECL:
      nonstatic_p = local_p = true;
      break;

    default:
      nonstatic_p = local_p = false;
      break;
    }

  if (nonstatic_p && local_p)
    SET_DECL_RTL (t, NULL);

  return NULL;
}

/* For functions-as-trees languages, this performs all optimization and
   compilation for FNDECL.  */

void
tree_rest_of_compilation (tree fndecl, bool nested_p)
{
  location_t saved_loc;

  timevar_push (TV_EXPAND);

  if (flag_unit_at_a_time && !cgraph_global_info_ready)
    abort ();

  /* Initialize the RTL code for the function.  */
  current_function_decl = fndecl;
  saved_loc = input_location;
  input_location = DECL_SOURCE_LOCATION (fndecl);
  init_function_start (fndecl);

  /* This function is being processed in whole-function mode.  */
  cfun->x_whole_function_mode_p = 1;

  /* Even though we're inside a function body, we still don't want to
     call expand_expr to calculate the size of a variable-sized array.
     We haven't necessarily assigned RTL to all variables yet, so it's
     not safe to try to expand expressions involving them.  */
  immediate_size_expand = 0;
  cfun->x_dont_save_pending_sizes_p = 1;

  /* If the function has a variably modified type, there may be
     SAVE_EXPRs in the parameter types.  Their context must be set to
     refer to this function; they cannot be expanded in the containing
     function.  */
  if (decl_function_context (fndecl)
      && variably_modified_type_p (TREE_TYPE (fndecl)))
    walk_tree (&TREE_TYPE (fndecl), set_save_expr_context, fndecl,
	       NULL);

  /* Set up parameters and prepare for return, for the function.  */
  expand_function_start (fndecl, 0);

  /* Allow language dialects to perform special processing.  */
  (*lang_hooks.rtl_expand.start) ();

  /* If this function is `main', emit a call to `__main'
     to run global initializers, etc.  */
  if (DECL_NAME (fndecl)
      && MAIN_NAME_P (DECL_NAME (fndecl))
      && DECL_FILE_SCOPE_P (fndecl))
    expand_main_function ();

  /* Generate the RTL for this function.  */
  (*lang_hooks.rtl_expand.stmt) (DECL_SAVED_TREE (fndecl));

  /* We hard-wired immediate_size_expand to zero above.
     expand_function_end will decrement this variable.  So, we set the
     variable to one here, so that after the decrement it will remain
     zero.  */
  immediate_size_expand = 1;

  /* Allow language dialects to perform special processing.  */
  (*lang_hooks.rtl_expand.end) ();

  /* Generate rtl for function exit.  */
  expand_function_end ();

  /* If this is a nested function, protect the local variables in the stack
     above us from being collected while we're compiling this function.  */
  if (nested_p)
    ggc_push_context ();

  /* There's no need to defer outputting this function any more; we
     know we want to output it.  */
  DECL_DEFER_OUTPUT (fndecl) = 0;

  /* Run the optimizers and output the assembler code for this function.  */
  rest_of_compilation (fndecl);

  /* Undo the GC context switch.  */
  if (nested_p)
    ggc_pop_context ();

  /* If requested, warn about function definitions where the function will
     return a value (usually of some struct or union type) which itself will
     take up a lot of stack space.  */
  if (warn_larger_than && !DECL_EXTERNAL (fndecl) && TREE_TYPE (fndecl))
    {
      tree ret_type = TREE_TYPE (TREE_TYPE (fndecl));

      if (ret_type && TYPE_SIZE_UNIT (ret_type)
	  && TREE_CODE (TYPE_SIZE_UNIT (ret_type)) == INTEGER_CST
	  && 0 < compare_tree_int (TYPE_SIZE_UNIT (ret_type),
				   larger_than_size))
	{
	  unsigned int size_as_int
	    = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (ret_type));

	  if (compare_tree_int (TYPE_SIZE_UNIT (ret_type), size_as_int) == 0)
	    warning ("%Jsize of return value of '%D' is %u bytes",
                     fndecl, fndecl, size_as_int);
	  else
	    warning ("%Jsize of return value of '%D' is larger than %wd bytes",
                     fndecl, fndecl, larger_than_size);
	}
    }

  if (! DECL_DEFER_OUTPUT (fndecl) || !cgraph_node (fndecl)->origin)
    {
      /* Since we don't need the RTL for this function anymore, stop pointing
	 to it.  That's especially important for LABEL_DECLs, since you can
	 reach all the instructions in the function from the CODE_LABEL stored
	 in the DECL_RTL for the LABEL_DECL.  Walk the BLOCK-tree, clearing
	 DECL_RTL for LABEL_DECLs and non-static local variables.  Note that
	 we must check the context of the variables, otherwise processing a
	 nested function can kill the rtl of a variable from an outer
	 function.  */
      walk_tree_without_duplicates (&DECL_SAVED_TREE (fndecl),
				    clear_decl_rtl,
				    fndecl);
      if (!cgraph_function_possibly_inlined_p (fndecl))
	{
	  DECL_SAVED_TREE (fndecl) = NULL;
	  if (DECL_SAVED_INSNS (fndecl) == 0
	      && !cgraph_node (fndecl)->origin)
	    {
	      /* Stop pointing to the local nodes about to be freed.
		 But DECL_INITIAL must remain nonzero so we know this
		 was an actual function definition.
		 For a nested function, this is done in c_pop_function_context.
		 If rest_of_compilation set this to 0, leave it 0.  */
	      if (DECL_INITIAL (fndecl) != 0)
		DECL_INITIAL (fndecl) = error_mark_node;
	    }
	}
    }

  input_location = saved_loc;

  timevar_pop (TV_EXPAND);
}
