/* Exception support for GNU CHILL.
   WARNING:  Only works for native (needs setjmp.h)!  FIXME!
   Copyright (C) 1992, 93, 94, 98, 99, 2000 Free Software Foundation, Inc.

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

/* On Suns this can get you to the right definition if you
   set the right value for TARGET.  */
#include <setjmp.h>
#ifdef sequent
/* Can you believe they forgot this?  */
#ifndef _JBLEN
#define _JBLEN 11
#endif
#endif

#ifndef _JBLEN
#define _JBLEN (sizeof(jmp_buf)/sizeof(int))
#define _JBLEN_2 _JBLEN+20
#else
/* if we use i.e. posix threads, this buffer must be longer */
#define _JBLEN_2 _JBLEN+20
#endif

/* On Linux setjmp is __setjmp FIXME: what is for CROSS */
#ifndef SETJMP_LIBRARY_NAME
#ifdef __linux__
#define SETJMP_LIBRARY_NAME "__setjmp"
#else
#define SETJMP_LIBRARY_NAME "setjmp"
#endif
#endif

#include "tree.h"
#include "ch-tree.h"
#include "rtl.h"
#include "toplev.h"

extern int  expand_exit_needed;

static tree link_handler_decl;
static tree handler_link_pointer_type;
static tree unlink_handler_decl;
static int exceptions_initialized = 0;
static void emit_setup_handler PARAMS ((void));
static void initialize_exceptions PARAMS ((void));
static tree start_handler_array PARAMS ((void));
static void finish_handler_array PARAMS ((void));
static tree char_pointer_type_for_handler;

/* If this is 1, operations to push and pop on the __exceptionStack
   are inline.  The default is is to use a function call, to
   allow for a per-thread exception stack. */
static int inline_exception_stack_ops = 0;

struct handler_state
{
  struct handler_state *next;

  /* Starts at 0, then incremented for every <on-alternative>. */
  int prev_on_alternative;

  /* If > 0: handler number for ELSE handler. */
  int else_handler;

  int action_number;

  char do_pushlevel;

  tree on_alt_list;
  tree setjmp_expr;

  /* A decl for the static handler array (used to map exception name to int).*/
  tree handler_array_decl;

  rtx end_label;

  /* Used to pass a tree from emit_setup_handler to chill_start_on. */
  tree handler_ref;

  tree unlink_cleanup;

  tree function;

  /* flag to indicate that we are currently compiling this handler.
     is_handled will need this to determine an unhandled exception */
  int compiling;
};

/* This is incremented by one each time we start an action which
   might have an ON-handler.  It is reset between passes. */
static int action_number = 0;

int action_nesting_level = 0;

/* The global_handler_list is constructed in pass 1.  It is not sorted.
   It contains one element for each action that actually had an ON-handler.
   An element's ACTION_NUMBER matches the action_number
   of that action.  The global_handler_list is eaten up during pass 2. */
#define ACTION_NUMBER(HANDLER) ((HANDLER)->action_number)
struct handler_state *global_handler_list = NULL;

/* This is a stack of handlers, one for each nested ON-handler. */
static struct handler_state *current_handler = NULL;

static struct handler_state *free_handlers = NULL; /* freelist */

static tree handler_element_type;
static tree handler_link_type;
static tree BISJ;
static tree jbuf_ident, prev_ident, handlers_ident;
static tree exception_stack_decl = 0;

/* Chain of cleanups assocated with exception handlers.
   The TREE_PURPOSE is an INTEGER_CST whose value is the
   DECL_ACTION_NESTING_LEVEL (when the handled actions was entered).
   The TREE_VALUE is an expression to expand when we exit that action. */

static tree cleanup_chain = NULL_TREE;

#if 0
/* Merge the current sequence onto the tail of the previous one. */

void
pop_sequence ()
{
  rtx sequence_first = get_insns ();

  end_sequence ();
  emit_insns (sequence_first);
  
}
#endif

/* Things we need to do at the beginning of pass 2. */

void
except_init_pass_2 ()
{
  /* First sort the global_handler_list on ACTION_NUMBER.
     This will already be in close to reverse order (the exception being
     nested ON-handlers), so insertion sort should essentially linear. */

  register struct handler_state *old_list = global_handler_list;

  /* First add a dummy final element. */
  if (free_handlers)
    global_handler_list = free_handlers;
  else
    global_handler_list
      = (struct handler_state*) permalloc (sizeof (struct handler_state));
  /* Make the final dummy "larger" than any other element. */
  ACTION_NUMBER (global_handler_list) = action_number + 1;
  /* Now move all the elements in old_list over to global_handler_list. */
  while (old_list != NULL)
    {
      register struct handler_state **ptr = &global_handler_list;
      /* Unlink from old_list. */
      register struct handler_state *current = old_list;
      old_list = old_list->next;

      while (ACTION_NUMBER (current) > ACTION_NUMBER (*ptr))
	ptr = &(*ptr)->next;
      /* Link into proper place in global_handler_list (new list). */
      current->next = *ptr;
      *ptr = current;
    }
     
  /* Don't forget to reset action_number. */
  action_number = 0;
}

/* This function is called at the beginning of an action that might be
   followed by an ON-handler.  Chill syntax doesn't let us know if
   we actually have an ON-handler until we see the ON, so we save
   away during pass 1 that information for use during pass 2. */

void
push_handler ()
{
  register struct handler_state *hstate;

  action_number++;
  action_nesting_level++;

  if (pass == 1)
    {
      if (free_handlers)
	{
	  hstate = free_handlers;
	  free_handlers = hstate->next;
	}
      else
	{
	  hstate =
	    (struct handler_state*) permalloc (sizeof (struct handler_state));
	}

      hstate->next = current_handler;
      current_handler = hstate;
      hstate->prev_on_alternative = 0;
      hstate->else_handler = 0;
      hstate->on_alt_list = NULL_TREE;
      hstate->compiling = 0;

      ACTION_NUMBER (hstate) = action_number;
      return;
    }

  if (ACTION_NUMBER (global_handler_list) != action_number)
    return;

  /* OK.  This action actually has an ON-handler.
     Pop it from global_handler_list, and use it. */

  hstate = global_handler_list;
  global_handler_list = hstate->next;

  /* Since this is pass 2, let's generate prologue code for that. */

  hstate->next = current_handler;
  current_handler = hstate;

  hstate->prev_on_alternative = 0;
  hstate->function = current_function_decl;

  emit_setup_handler ();
}

static tree
start_handler_array ()
{
  tree handler_array_type, decl;

  push_obstacks_nochange ();
  end_temporary_allocation ();
  handler_array_type = build_array_type (handler_element_type, NULL_TREE);
  decl = build_lang_decl (VAR_DECL,
			  get_unique_identifier ("handler_table"),
			  handler_array_type);

/*  TREE_TYPE (decl) = handler_array_type;*/
  TREE_READONLY (decl) = 1;
  TREE_STATIC (decl) = 1;
  DECL_INITIAL (decl) = error_mark_node;
  
  pushdecl (decl);
  make_decl_rtl (decl, NULL_PTR, 0);
  current_handler->handler_array_decl = decl;
  return decl;
}

static void
finish_handler_array ()
{
  tree decl = current_handler->handler_array_decl;
  tree t;
  tree handler_array_init = NULL_TREE;
  int handlers_count = 1;
  int nelts;

  /* Build the table mapping exceptions to handler(-number)s.
     This is done in reverse order. */
  
  /* First push the end of the list.  This is either the ELSE
     handler (current_handler->else_handler>0) or NULL handler to indicate
     the end of the list (if current_handler->else-handler == 0).
     The following works either way. */
  handler_array_init = build_tree_list
    (NULL_TREE, chill_expand_tuple
     (handler_element_type,
      build_nt (CONSTRUCTOR, NULL_TREE,
		tree_cons (NULL_TREE,
			   null_pointer_node,
			   build_tree_list (NULL_TREE,
					    build_int_2 (current_handler->else_handler,
							     0))))));
  
  for (t = current_handler->on_alt_list; t != NULL_TREE; t = TREE_CHAIN (t))
    { tree handler_number = TREE_PURPOSE(t);
      tree elist = TREE_VALUE (t);
      for ( ; elist != NULL_TREE; elist = TREE_CHAIN (elist))
	{
	  tree ex_decl =
	    build_chill_exception_decl (IDENTIFIER_POINTER(TREE_VALUE(elist)));
	  tree ex_addr = build1 (ADDR_EXPR,
				 char_pointer_type_for_handler,
				 ex_decl);
	  tree el = build_nt (CONSTRUCTOR, NULL_TREE,
			      tree_cons (NULL_TREE,
					 ex_addr,
					 build_tree_list (NULL_TREE,
							  handler_number)));
	  mark_addressable (ex_decl);
	  TREE_CONSTANT (ex_addr) = 1;
	  handler_array_init =
	    tree_cons (NULL_TREE,
		       chill_expand_tuple (handler_element_type, el),
		       handler_array_init);
	  handlers_count++;
	}
    }

#if 1
  nelts = list_length (handler_array_init);
  TYPE_DOMAIN (TREE_TYPE (decl))
    = build_index_type (build_int_2 (nelts - 1, - (nelts == 0)));
  layout_type (TREE_TYPE (decl));
  DECL_INITIAL (decl)
    = convert (TREE_TYPE (decl),
	       build_nt (CONSTRUCTOR, NULL_TREE, handler_array_init));

  /* Pop back to the obstack that is current for this binding level.
     This is because MAXINDEX, rtl, etc. to be made below
     must go in the permanent obstack.  But don't discard the
     temporary data yet.  */
  pop_obstacks ();
  layout_decl (decl, 0);
  /* To prevent make_decl_rtl (called indiectly by rest_of_decl_compilation)
     throwing the existing RTL (which has already been used). */
  PUT_MODE (DECL_RTL (decl), DECL_MODE (decl));
  rest_of_decl_compilation (decl, (char*)0, 0, 0);
  expand_decl_init (decl);
#else
  /* To prevent make_decl_rtl (called indirectly by finish_decl)
     altering the existing RTL. */
  GET_MODE (DECL_RTL (current_handler->handler_array_decl)) =
    DECL_MODE (current_handler->handler_array_decl);

  finish_decl (current_handler->handler_array_decl,
	       build_nt (CONSTRUCTOR, NULL_TREE, handler_array_init),
	       NULL_TREE);
#endif
}


void
pop_handler (used)
     int used;
{
  action_nesting_level--;
  if (pass == 1)
    {
      struct handler_state *old = current_handler;
      if (old == NULL)
	fatal ("internal error: on stack out of sync");
      current_handler = old->next;

      if (used)
	{ /* Push unto global_handler_list. */
	  old->next = global_handler_list;
	  global_handler_list = old;
	}
      else
	{
	  /* Push onto free_handlers free list. */
	  old->next = free_handlers;
	  free_handlers = old;
	}
    }
  else if (used)
    {
      current_handler = current_handler->next;
    }
}

/* Emit code before an action that has an ON-handler. */

static void
emit_setup_handler ()
{
  tree handler_decl, handler_addr, t;

  /* Field references. */
  tree jbuf_ref, handlers_ref,prev_ref;
  if (!exceptions_initialized)
    {
      /* We temporarily reset the maximum_field_alignment to zero so the
	 compiler's exception data structures can be compatible with the
	 run-time system, even when we're compiling with -fpack. */
      extern int maximum_field_alignment;
      int save_maximum_field_alignment = maximum_field_alignment;
      maximum_field_alignment = 0;
      push_obstacks_nochange ();
      end_temporary_allocation ();
      initialize_exceptions ();
      pop_obstacks ();
      maximum_field_alignment = save_maximum_field_alignment;
    }

  push_momentary ();

  handler_decl = build_lang_decl (VAR_DECL,
				  get_unique_identifier ("handler"),
				  handler_link_type);
  push_obstacks_nochange ();
  pushdecl(handler_decl);
  expand_decl (handler_decl);
  finish_decl (handler_decl);

  jbuf_ref = build_component_ref (handler_decl, jbuf_ident);
  jbuf_ref = build_chill_arrow_expr (jbuf_ref, 1);
  handlers_ref = build_component_ref (handler_decl, handlers_ident);
  prev_ref = build_component_ref (handler_decl, prev_ident);

  /* Emit code to link in handler in __exceptionStack chain. */
  mark_addressable (handler_decl);
  handler_addr = build1 (ADDR_EXPR, handler_link_pointer_type, handler_decl);
  if (inline_exception_stack_ops)
    {
      expand_expr_stmt (build_chill_modify_expr (prev_ref,
						 exception_stack_decl));
      expand_expr_stmt (build_chill_modify_expr (exception_stack_decl,
						 handler_addr));
      current_handler->handler_ref = prev_ref;
    }
  else
    {
      expand_expr_stmt (build_chill_function_call (link_handler_decl,
					     build_tree_list (NULL_TREE,
							      handler_addr)));
      current_handler->handler_ref = handler_addr;
    }

  /* Expand:  handler->__handlers = { <<array mapping names to ints } */
  t =  build1 (NOP_EXPR, build_pointer_type (handler_element_type),
	       build_chill_arrow_expr (start_handler_array (), 1));
  expand_expr_stmt (build_chill_modify_expr (handlers_ref, t));

  /* Emit code to unlink handler. */
  if (inline_exception_stack_ops)
    current_handler->unlink_cleanup
      = build_chill_modify_expr (exception_stack_decl,
				 current_handler->handler_ref);
  else
    current_handler->unlink_cleanup
      = build_chill_function_call (unlink_handler_decl,
				   build_tree_list(NULL_TREE,
					       current_handler->handler_ref));
  cleanup_chain = tree_cons (build_int_2 (action_nesting_level, 0),
			     current_handler->unlink_cleanup,
			     cleanup_chain);

  /* Emit code for setjmp. */
  
  current_handler->setjmp_expr =
    build_chill_function_call (BISJ, build_tree_list (NULL_TREE, jbuf_ref));
  expand_start_case (1, current_handler->setjmp_expr,
		     integer_type_node, "on handler");

  chill_handle_case_label (integer_zero_node, current_handler->setjmp_expr);
}

/* Start emitting code for: <actions> ON <handlers> END.
   Assume we've parsed <actions>, and the setup needed for it. */

void
chill_start_on ()
{
  expand_expr_stmt (current_handler->unlink_cleanup);

  /* Emit code to jump past the handlers. */
  current_handler->end_label = gen_label_rtx ();
  current_handler->compiling = 1;
  emit_jump (current_handler->end_label);
}

void
chill_finish_on ()
{
  expand_end_case (current_handler->setjmp_expr);
  
  finish_handler_array ();

  emit_label (current_handler->end_label);

  pop_momentary ();

  cleanup_chain = TREE_CHAIN (cleanup_chain);
}

void
chill_handle_on_labels (labels)
     tree labels;
{
  int alternative = ++current_handler->prev_on_alternative;
  if (pass == 1)
    {
      tree handler_number = build_int_2 (alternative, 0);
      current_handler->on_alt_list =
	tree_cons (handler_number, labels, current_handler->on_alt_list);
    }
  else
    {
      /* Find handler_number saved in pass 1. */
      tree tmp = current_handler->on_alt_list;
      while (TREE_INT_CST_LOW (TREE_PURPOSE (tmp)) != alternative)
	tmp = TREE_CHAIN (tmp);
      if (expand_exit_needed)
	expand_exit_something (), expand_exit_needed = 0;
      chill_handle_case_label (TREE_PURPOSE (tmp),
			       current_handler->setjmp_expr);
    }
}

void
chill_start_default_handler ()
{
  current_handler->else_handler = ++current_handler->prev_on_alternative;
  if (!ignoring)
    {
      chill_handle_case_default ();
    }
}

void
chill_check_no_handlers ()
{
  if (current_handler != NULL)
    fatal ("internal error: on stack not empty when done");
}

static void
initialize_exceptions ()
{
  tree jmp_buf_type = build_array_type (integer_type_node,
					build_index_type (build_int_2 (_JBLEN_2-1, 0)));
  tree setjmp_fndecl, link_ftype;
  tree parmtypes
    = tree_cons (NULL_TREE, build_pointer_type (jmp_buf_type), void_list_node);

  setjmp_fndecl = builtin_function ("setjmp",
				    build_function_type (integer_type_node,
							 parmtypes),
				    0, NOT_BUILT_IN,
				    SETJMP_LIBRARY_NAME);
  BISJ = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (setjmp_fndecl)),
		 setjmp_fndecl);
 
  char_pointer_type_for_handler
    = build_pointer_type (build_type_variant (char_type_node, 1, 0));
  handler_element_type =
    build_chill_struct_type (chainon
			     (build_decl (FIELD_DECL,
					  get_identifier("__exceptid"),
					  char_pointer_type_for_handler),
			      build_decl (FIELD_DECL,
					  get_identifier("__handlerno"),
					  integer_type_node)));

  jbuf_ident = get_identifier("__jbuf");
  prev_ident = get_identifier("__prev");
  handlers_ident = get_identifier("__handlers");

  handler_link_type =
    build_chill_struct_type
      (chainon
       (build_decl (FIELD_DECL, prev_ident, ptr_type_node),
	chainon
	(build_decl (FIELD_DECL, handlers_ident,
		     build_pointer_type (handler_element_type)),
	 build_decl (FIELD_DECL, jbuf_ident, jmp_buf_type))));

  handler_link_pointer_type = build_pointer_type (handler_link_type);

  if (inline_exception_stack_ops)
    {
      exception_stack_decl =
	build_lang_decl (VAR_DECL,
			 get_identifier("__exceptionStack"),
			 handler_link_pointer_type);
      TREE_STATIC (exception_stack_decl) = 1;
      TREE_PUBLIC (exception_stack_decl) = 1;
      DECL_EXTERNAL (exception_stack_decl) = 1;
      push_obstacks_nochange ();
      pushdecl(exception_stack_decl);
      make_decl_rtl (exception_stack_decl, NULL_PTR, 1);
      finish_decl (exception_stack_decl);
    }

  link_ftype = build_function_type (void_type_node,
				    tree_cons (NULL_TREE,
					       handler_link_pointer_type,
					       void_list_node));
  link_handler_decl = builtin_function ("__ch_link_handler", link_ftype,
					0, NOT_BUILT_IN, NULL_PTR);
  unlink_handler_decl = builtin_function ("__ch_unlink_handler", link_ftype,
					  0, NOT_BUILT_IN, NULL_PTR);

  exceptions_initialized = 1;
}

/* Do the cleanup(s) needed for a GOTO label.
   We only need to do the last of the cleanups. */

void
expand_goto_except_cleanup (label_level)
     int label_level;
{
  tree list = cleanup_chain;
  tree last = NULL_TREE;
  for ( ; list != NULL_TREE; list = TREE_CHAIN (list))
    {
      if (TREE_INT_CST_LOW (TREE_PURPOSE (list)) > label_level)
	last = list;
      else
	break;
    }
  if (last)
    expand_expr_stmt (TREE_VALUE (last));
}

/* Returns true if there is a valid handler for EXCEPT_NAME
   in the current static scope.
   0 ... no handler found
   1 ... local handler available
   2 ... function may propagate this exception
*/

int
is_handled (except_name)
     tree except_name;
{
  tree t;
  struct handler_state *h = current_handler;

  /* if we are are currently compiling this handler
     we have to start at the next level */
  if (h && h->compiling)
    h = h->next;
  while (h != NULL)
    {
      if (h->function != current_function_decl)
	break;
      if (h->else_handler > 0)
	return 1;
      for (t = h->on_alt_list; t != NULL_TREE; t = TREE_CHAIN (t))
	{
	  if (value_member (except_name, TREE_VALUE (t)))
	    return 1;
	}
      h = h->next;
    }

  t = TYPE_RAISES_EXCEPTIONS (TREE_TYPE (current_function_decl));

  if (value_member (except_name, t))
    return 2;
  return 0;
}

/* function generates code to reraise exceptions
   for PROC's propagating exceptions. */

void
chill_reraise_exceptions (exceptions)
     tree exceptions;
{
  tree wrk;

  if (exceptions == NULL_TREE)
    return; /* just in case */

  if (pass == 1)
    {
      for (wrk = exceptions; wrk != NULL_TREE; wrk = TREE_CHAIN (wrk))
	chill_handle_on_labels (build_tree_list (NULL_TREE, TREE_VALUE (wrk)));
    }
  else /* pass == 2 */
    {
      chill_start_on ();
      expand_exit_needed = 0;

      for (wrk = exceptions; wrk != NULL_TREE; wrk = TREE_CHAIN (wrk))
	{
	  chill_handle_on_labels (TREE_VALUE (wrk));
	  /* do a CAUSE exception */
	  expand_expr_stmt (build_cause_exception (TREE_VALUE (wrk), 0));
	  expand_exit_needed = 1;
	}
      chill_finish_on ();
    }
  pop_handler (1);
}
