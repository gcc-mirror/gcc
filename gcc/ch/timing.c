/* Implement timing-related actions for CHILL.
   Copyright (C) 1992, 93, 1994, 1998 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "rtl.h"
#include "ch-tree.h"
#include "flags.h"
#include "input.h"
#include "obstack.h"
#include "lex.h"
#include "toplev.h"

#ifndef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE BITS_PER_WORD
#endif

/* set non-zero if input text is forced to lowercase */
extern int ignore_case;

/* set non-zero if special words are to be entered in uppercase */
extern int special_UC;

/* timing modes */
tree abs_timing_type_node;
tree duration_timing_type_node;

/* rts time type */
static tree rtstime_type_node = NULL_TREE;

/* the stack for AFTER primval [ DELAY ] IN 
   and has following layout

   TREE_VALUE (TREE_VALUE (after_stack)) = current time or NULL_TREE (if DELAY specified)
   TREE_PURPOSE (TREE_VALUE (after_stack)) = the duration location
   TREE_VALUE (TREE_PURPOSE (after_stack)) = label at TIMEOUT
   TREE_PURPOSE (TREE_PURPOSE (after_stack)) = label at the end of AFTER action
*/
tree after_stack = NULL_TREE;

/* in pass 1 we need a seperate list for the labels */
static tree after_stack_pass_1 = NULL_TREE;
static tree after_help;

void
timing_init ()
{
  tree ptr_ftype_durt_ptr_int;
  tree int_ftype_abst_ptr_int;
  tree void_ftype_ptr;
  tree long_ftype_int_int_int_int_int_int_int_ptr_int;
  tree void_ftype_abstime_ptr;
  tree int_ftype_ptr_durt_ptr;
  tree void_ftype_durt_ptr;
  tree void_ftype_ptr_durt_ptr_int;
  tree temp;
  tree endlink;
  tree ulong_type;
  
  ulong_type = TREE_TYPE (lookup_name (
		          get_identifier ((ignore_case || ! special_UC ) ?
					  "ulong" : "ULONG")));

  /* build modes for TIME and DURATION */
  duration_timing_type_node = make_unsigned_type (LONG_TYPE_SIZE);
  temp = pushdecl (build_decl (TYPE_DECL, ridpointers[(int)RID_DURATION],
			       duration_timing_type_node));
  SET_CH_NOVELTY_NONNIL (duration_timing_type_node, temp);
  abs_timing_type_node = make_unsigned_type (LONG_TYPE_SIZE);
  temp = pushdecl (build_decl (TYPE_DECL, ridpointers[(int)RID_TIME],
			       abs_timing_type_node));
  SET_CH_NOVELTY_NONNIL (abs_timing_type_node, temp);

  /* the mode of time the runtimesystem returns */
  if (rtstime_type_node == NULL_TREE)
  {
      tree decl1, decl2, result;

      decl1 = build_decl (FIELD_DECL,
			  get_identifier ("secs"),
			  ulong_type);
      DECL_INITIAL (decl1) = NULL_TREE;
      decl2 = build_decl (FIELD_DECL,
			  get_identifier ("nsecs"),
			  ulong_type);
      DECL_INITIAL (decl2) = NULL_TREE;
      TREE_CHAIN (decl2) = NULL_TREE;
      TREE_CHAIN (decl1) = decl2;
      
      result = build_chill_struct_type (decl1);
      pushdecl (temp = build_decl (TYPE_DECL,
	get_identifier ("__tmp_rtstime"), result));
      DECL_SOURCE_LINE (temp) = 0;
      satisfy_decl (temp, 0);
      rtstime_type_node = TREE_TYPE (temp);
  }
  
  endlink = void_list_node;
  
  ptr_ftype_durt_ptr_int
    = build_function_type (ptr_type_node,
         tree_cons (NULL_TREE, duration_timing_type_node,
             tree_cons (NULL_TREE, ptr_type_node,
                 tree_cons (NULL_TREE, integer_type_node,
                     endlink))));

  int_ftype_abst_ptr_int
    = build_function_type (integer_type_node,
         tree_cons (NULL_TREE, abs_timing_type_node,
             tree_cons (NULL_TREE, ptr_type_node,
                 tree_cons (NULL_TREE, integer_type_node,
                     endlink))));

  void_ftype_ptr
     = build_function_type (void_type_node,
	   tree_cons (NULL_TREE, ptr_type_node,
	       endlink));

  long_ftype_int_int_int_int_int_int_int_ptr_int
    = build_function_type (abs_timing_type_node,
	 tree_cons (NULL_TREE, integer_type_node,
	    tree_cons (NULL_TREE, integer_type_node,
	       tree_cons (NULL_TREE, integer_type_node,
	          tree_cons (NULL_TREE, integer_type_node,
	             tree_cons (NULL_TREE, integer_type_node,
	                tree_cons (NULL_TREE, integer_type_node,
	                   tree_cons (NULL_TREE, integer_type_node,
	                      tree_cons (NULL_TREE, ptr_type_node,
	                         tree_cons (NULL_TREE, integer_type_node,
			            endlink))))))))));

  void_ftype_abstime_ptr
    = build_function_type (void_type_node,
          tree_cons (NULL_TREE, abs_timing_type_node,
              tree_cons (NULL_TREE, ptr_type_node,
                  endlink)));

  int_ftype_ptr_durt_ptr
    = build_function_type (integer_type_node,
          tree_cons (NULL_TREE, ptr_type_node,
              tree_cons (NULL_TREE, duration_timing_type_node,
                  tree_cons (NULL_TREE, ptr_type_node,
                      endlink))));

  void_ftype_durt_ptr
    = build_function_type (void_type_node,
          tree_cons (NULL_TREE, duration_timing_type_node,
              tree_cons (NULL_TREE, ptr_type_node,
                  endlink)));

  void_ftype_ptr_durt_ptr_int
    = build_function_type (void_type_node,
        tree_cons (NULL_TREE, ptr_type_node,
          tree_cons (NULL_TREE, duration_timing_type_node,
            tree_cons (NULL_TREE, ptr_type_node,
              tree_cons (NULL_TREE, integer_type_node,
                endlink)))));

  builtin_function ("_abstime", long_ftype_int_int_int_int_int_int_int_ptr_int,
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__check_cycle", void_ftype_ptr_durt_ptr_int,
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__convert_duration_rtstime", void_ftype_durt_ptr,
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__define_timeout", ptr_ftype_durt_ptr_int,
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("_inttime", void_ftype_abstime_ptr,
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__remaintime", int_ftype_ptr_durt_ptr,
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__rtstime", void_ftype_ptr,
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__wait_until", int_ftype_abst_ptr_int,
		    NOT_BUILT_IN, NULL_PTR);
}

#if 0
 *
 * build AT action
 *
 * AT primval IN
 *  ok-actionlist
 * TIMEOUT
 *  to-actionlist
 * END;
 *
 * gets translated to
 *
 * if (__wait_until (primval) == 0)
 *   ok-actionlist
 * else
 *   to-action-list
 *
#endif

void
build_at_action (t)
     tree t;
{
  tree abstime, expr, filename, fcall;
  
  if (t == NULL_TREE || TREE_CODE (t) == ERROR_MARK)
    abstime = convert (abs_timing_type_node, build_int_2 (0, 0));
  else
    abstime = t;
  
  if (TREE_TYPE (abstime) != abs_timing_type_node)
    {
      error ("absolute time value must be of mode TIME.");
      abstime = convert (abs_timing_type_node, build_int_2 (0, 0));
    }
  filename = force_addr_of (get_chill_filename ());
  fcall = build_chill_function_call (
	    lookup_name (get_identifier ("__wait_until")),
	      tree_cons (NULL_TREE, abstime,
		tree_cons (NULL_TREE, filename,
		  tree_cons (NULL_TREE, get_chill_linenumber (), NULL_TREE))));
  expr = build (EQ_EXPR, integer_type_node, fcall, integer_zero_node);
  expand_start_cond (expr, 0);
  emit_line_note (input_filename, lineno);
}

#if 0
 *
 * build CYCLE action
 *
 * CYCLE primval IN
 *  actionlist
 * END;
 *
 * gets translated to
 *
 * {
 *    RtsTime  now; 
 *  label:
 *    __rtstime (&now); 
 *     actionlist
 *    __check_cycle (&now, primval, filename, lineno); 
 *    goto label;
 *  }
 *
#endif

tree
build_cycle_start (t)
    tree t;
{
  tree purpose = build_tree_list (NULL_TREE, NULL_TREE);
  tree toid = build_tree_list (purpose, NULL_TREE);

  /* define the label. Note: define_label needs to be called in
     pass 1 and pass 2. */
  TREE_VALUE (toid) = define_label (input_filename, lineno,
				    get_unique_identifier ("CYCLE_label"));
  if (! ignoring)
    {
      tree duration_value, now_location;
      
      if (t == NULL_TREE || TREE_CODE (t) == ERROR_MARK)
	duration_value = convert (duration_timing_type_node, build_int_2 (0,0));
      else
	duration_value = t;
      
      if (TREE_TYPE (duration_value) != duration_timing_type_node)
	{
	  error ("duration primitive value must be of mode DURATION.");
	  duration_value = convert (duration_timing_type_node, build_int_2 (0,0));
	}
      TREE_PURPOSE (TREE_PURPOSE (toid)) = duration_value;
      /* define the variable */
      now_location = decl_temp1 (get_unique_identifier ("CYCLE_var"),
				 rtstime_type_node, 0,
				 NULL_TREE, 0, 0);
      TREE_VALUE (TREE_PURPOSE (toid)) = force_addr_of (now_location);
      
      /* build the call to __rtstime */
      expand_expr_stmt (
        build_chill_function_call (lookup_name (get_identifier ("__rtstime")),
          build_tree_list (NULL_TREE, TREE_VALUE (TREE_PURPOSE (toid)))));
    }

  return toid;
}

void
build_cycle_end (toid)
     tree toid;
{
  tree filename, linenumber;
  
  /* here we call __check_cycle and then jump to beginning of this
     action */
  filename = force_addr_of (get_chill_filename ());
  linenumber = get_chill_linenumber ();
  expand_expr_stmt (
    build_chill_function_call (
      lookup_name (get_identifier ("__check_cycle")),
	tree_cons (NULL_TREE, TREE_VALUE (TREE_PURPOSE (toid)),
          tree_cons (NULL_TREE, TREE_PURPOSE (TREE_PURPOSE (toid)),
	    tree_cons (NULL_TREE, filename,
	      tree_cons (NULL_TREE, linenumber, NULL_TREE))))));
  expand_goto (TREE_VALUE (toid));
}

#if 0
 *
 * build AFTER ACTION
 *
 * AFTER primval [ DELAY ] IN
 *  action-list
 * TIMEOUT
 *  to-action-list
 * END
 *
 * gets translated to
 *
 * {
 *   struct chill_time __now; 
 *   duration dur = primval; 
 *   if (! delay_spceified)
 *     __rts_time (&__now); 
 *     .
 *     .
 *    goto end-label;
 *   to-label:
 *     .
 *     .
 *   end-label:
 * }
 *
#endif

void
build_after_start (duration, delay_flag)
    tree duration;
    int  delay_flag;
{
  tree value, purpose;
  
  if (! ignoring)
    {
      value = tree_cons (NULL_TREE, NULL_TREE, NULL_TREE);
      purpose = after_stack_pass_1;
      after_stack_pass_1 = TREE_CHAIN (after_stack_pass_1);
      after_stack = tree_cons (purpose, value, after_stack);
      
      if (TREE_TYPE (duration) != duration_timing_type_node)
        {
	  error ("duration primitive value must be of mode DURATION.");
	  duration = convert (duration_timing_type_node, build_int_2 (0,0));
        }
      TREE_PURPOSE (value) = decl_temp1 (get_identifier ("AFTER_duration"),
					 duration_timing_type_node, 0,
					 duration, 0, 0);
      
      if (! delay_flag)
        {
	  /* in this case we have to get the current time */
	  TREE_VALUE (value) = decl_temp1 (get_unique_identifier ("AFTER_now"),
					   rtstime_type_node, 0,
					   NULL_TREE, 0, 0);
	  /* build the function call to initialize the variable */
	  expand_expr_stmt (
            build_chill_function_call (lookup_name (get_identifier ("__rtstime")),
              build_tree_list (NULL_TREE, force_addr_of (TREE_VALUE (value)))));
        }
    }
  else
    {
      /* in pass 1 we just save the labels */
      after_help = tree_cons (NULL_TREE, NULL_TREE, after_help);
      after_stack_pass_1 = chainon (after_stack_pass_1, after_help);
    }
}

void
build_after_timeout_start ()
{
  tree label_name;
  
  if (! ignoring)
    {
      /* jump to the end of AFTER action */
      lookup_and_expand_goto (TREE_PURPOSE (TREE_PURPOSE (after_stack)));
      label_name = TREE_VALUE (TREE_PURPOSE (after_stack));
      /* mark we are in TIMEOUT part of AFTER action */
      TREE_VALUE (TREE_PURPOSE (after_stack)) = NULL_TREE;
    }
  else
    {
      label_name = get_unique_identifier ("AFTER_tolabel");
      TREE_VALUE (after_help) = label_name;
    }
  define_label (input_filename, lineno, label_name);
}

void
build_after_end ()
{
  tree label_name;
    
  /* define the end label */
  if (! ignoring)
    {
      label_name = TREE_PURPOSE (TREE_PURPOSE (after_stack));
      after_stack = TREE_CHAIN (after_stack);
    }
  else
    {
      label_name = get_unique_identifier ("AFTER_endlabel");
      TREE_PURPOSE (after_help) = label_name;
      after_help = TREE_CHAIN (after_help);
    }
  define_label (input_filename, lineno, label_name);
}

tree
build_timeout_preface ()
{
  tree timeout_value = null_pointer_node;
  
  if (after_stack != NULL_TREE &&
      TREE_VALUE (TREE_PURPOSE (after_stack)) != NULL_TREE)
    {
      tree to_loc;
      
      to_loc = decl_temp1 (get_unique_identifier ("TOloc"),
			   rtstime_type_node, 0, NULL_TREE, 0, 0);
      timeout_value = force_addr_of (to_loc);

      if (TREE_VALUE (TREE_VALUE (after_stack)) == NULL_TREE)
        {
	  /* DELAY specified -- just call __convert_duration_rtstime for
	     given duration value */
	  expand_expr_stmt (
            build_chill_function_call (
              lookup_name (get_identifier ("__convert_duration_rtstime")),
                tree_cons (NULL_TREE, TREE_PURPOSE (TREE_VALUE (after_stack)),
                  tree_cons (NULL_TREE, timeout_value, NULL_TREE))));
        }
      else
        {
	  /* delay not specified -- call __remaintime which returns the 
	     remaining time of duration in rtstime format and check the 
	     result */
	  tree fcall = 
            build_chill_function_call (
              lookup_name (get_identifier ("__remaintime")),
                tree_cons (NULL_TREE, force_addr_of (TREE_VALUE (TREE_VALUE (after_stack))),
		  tree_cons (NULL_TREE, TREE_PURPOSE (TREE_VALUE (after_stack)),
                    tree_cons (NULL_TREE, timeout_value, NULL_TREE))));
	  tree expr = build (NE_EXPR, integer_type_node,
			     fcall, integer_zero_node);
	  expand_start_cond (expr, 0);
	  lookup_and_expand_goto (TREE_VALUE (TREE_PURPOSE (after_stack)));
	  expand_end_cond ();
        }
    }
  return timeout_value;
}

void
build_timesupervised_call (fcall, to_loc)
    tree fcall;
    tree to_loc;
{
  if (to_loc == null_pointer_node)
    expand_expr_stmt (fcall);
  else
    {
      tree expr = build (NE_EXPR, integer_type_node, fcall, integer_zero_node);
      expand_start_cond (expr, 0);
      lookup_and_expand_goto (TREE_VALUE (TREE_PURPOSE (after_stack)));
      expand_end_cond ();
    }
}
