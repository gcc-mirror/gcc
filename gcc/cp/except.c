/* Handle exceptional things in C++.
   Copyright (C) 1989, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
   Contributed by Michael Tiemann <tiemann@cygnus.com>
   Rewritten by Mike Stump <mrs@cygnus.com>, based upon an
   initial re-implementation courtesy Tad Hunt.

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


/* High-level class interface. */

#include "config.h"
#include "tree.h"
#include "rtl.h"
#include "cp-tree.h"
#include "flags.h"
#include "obstack.h"
#include "expr.h"

tree protect_list;

extern void (*interim_eh_hook)	PROTO((tree));
rtx expand_builtin_return_addr	PROTO((enum built_in_function, int, rtx));

/* holds the fndecl for __builtin_return_address () */
tree builtin_return_address_fndecl;
tree throw_fndecl;

static int
doing_eh (do_warn)
     int do_warn;
{
  if (! flag_handle_exceptions)
    {
      static int warned = 0;
      if (! warned && do_warn)
	{
	  error ("exception handling disabled, use -fhandle-exceptions to enable.");
	  warned = 1;
	}
      return 0;
    }
  return 1;
}


/*
NO GNEWS IS GOOD GNEWS WITH GARRY GNUS: This version is much closer
to supporting exception handling as per ANSI C++ working draft.
It is a complete rewrite of all the EH stuff that was here before
	Shortcomings:
		1. Throw specifications of functions still don't work.
	Cool Things:
		1. Destructors are called properly :-)
		2. No overhead for the non-exception thrown case.
		3. Fixing shortcoming 1 is simple.
			-Tad Hunt	(tad@mail.csh.rit.edu)

*/

/* A couple of backend routines from m88k.c */

/* used to cache a call to __builtin_return_address () */
static tree BuiltinReturnAddress;
     

#include <stdio.h>

/* XXX - Tad: for EH */
/* output an exception table entry */

static void
output_exception_table_entry (file, start_label, end_label, eh_label)
     FILE *file;
     rtx start_label, end_label, eh_label;
{
  char label[100];

  assemble_integer (start_label, GET_MODE_SIZE (Pmode), 1);
  assemble_integer (end_label, GET_MODE_SIZE (Pmode), 1);
  assemble_integer (eh_label, GET_MODE_SIZE (Pmode), 1);
  putc ('\n', file);		/* blank line */
}
   
static void
easy_expand_asm (str)
     char *str;
{
  expand_asm (build_string (strlen (str)+1, str));
}


#if 0
/* This is the startup, and finish stuff per exception table. */

/* XXX - Tad: exception handling section */
#ifndef EXCEPT_SECTION_ASM_OP
#define EXCEPT_SECTION_ASM_OP	"section\t.gcc_except_table,\"a\",@progbits"
#endif

#ifdef EXCEPT_SECTION_ASM_OP
typedef struct {
    void *start_protect;
    void *end_protect;
    void *exception_handler;
 } exception_table;
#endif /* EXCEPT_SECTION_ASM_OP */

#ifdef EXCEPT_SECTION_ASM_OP

 /* on machines which support it, the exception table lives in another section,
	but it needs a label so we can reference it...  This sets up that
    label! */
asm (EXCEPT_SECTION_ASM_OP);
exception_table __EXCEPTION_TABLE__[1] = { (void*)0, (void*)0, (void*)0 };
asm (TEXT_SECTION_ASM_OP);

#endif /* EXCEPT_SECTION_ASM_OP */

#ifdef EXCEPT_SECTION_ASM_OP

 /* we need to know where the end of the exception table is... so this
    is how we do it! */

asm (EXCEPT_SECTION_ASM_OP);
exception_table __EXCEPTION_END__[1] = { (void*)-1, (void*)-1, (void*)-1 };
asm (TEXT_SECTION_ASM_OP);

#endif /* EXCEPT_SECTION_ASM_OP */

#endif

void
exception_section ()
{
#ifdef ASM_OUTPUT_SECTION_NAME
  named_section (NULL_TREE, ".gcc_except_table");
#else
  if (flag_pic)
    data_section ();
  else
#if defined(TARGET_POWERPC) /* are we on a __rs6000? */
    data_section ();
#else
    readonly_data_section ();
#endif
#endif
}




/* from: my-cp-except.c */

/* VI: ":set ts=4" */
#if 0
#include <stdio.h> */
#include "config.h"
#include "tree.h"
#include "rtl.h"
#include "cp-tree.h"
#endif
#include "decl.h"
#if 0
#include "flags.h"
#endif
#include "insn-flags.h"
#include "obstack.h"
#if 0
#include "expr.h"
#endif

/* ======================================================================
   Briefly the algorithm works like this:

     When a constructor or start of a try block is encountered,
     push_eh_entry (&eh_stack) is called.  Push_eh_entry () creates a
     new entry in the unwind protection stack and returns a label to
     output to start the protection for that block.

     When a destructor or end try block is encountered, pop_eh_entry
     (&eh_stack) is called.  Pop_eh_entry () returns the ehEntry it
     created when push_eh_entry () was called.  The ehEntry structure
     contains three things at this point.  The start protect label,
     the end protect label, and the exception handler label.  The end
     protect label should be output before the call to the destructor
     (if any). If it was a destructor, then its parse tree is stored
     in the finalization variable in the ehEntry structure.  Otherwise
     the finalization variable is set to NULL to reflect the fact that
     is the the end of a try block.  Next, this modified ehEntry node
     is enqueued in the finalizations queue by calling
     enqueue_eh_entry (&queue,entry).

	+---------------------------------------------------------------+
	|XXX: Will need modification to deal with partially		|
	|			constructed arrays of objects		|
	|								|
	|	Basically, this consists of keeping track of how many	|
	|	of the objects have been constructed already (this	|
	|	should be in a register though, so that shouldn't be a	|
	|	problem.						|
	+---------------------------------------------------------------+

     When a catch block is encountered, there is a lot of work to be
     done.

     Since we don't want to generate the catch block inline with the
     regular flow of the function, we need to have some way of doing
     so.  Luckily, we can use sequences to defer the catch sections.
     When the start of a catch block is encountered, we start the
     sequence.  After the catch block is generated, we end the
     sequence.

     Next we must insure that when the catch block is executed, all
     finalizations for the matching try block have been completed.  If
     any of those finalizations throw an exception, we must call
     terminate according to the ARM (section r.15.6.1).  What this
     means is that we need to dequeue and emit finalizations for each
     entry in the ehQueue until we get to an entry with a NULL
     finalization field.  For any of the finalization entries, if it
     is not a call to terminate (), we must protect it by giving it
     another start label, end label, and exception handler label,
     setting its finalization tree to be a call to terminate (), and
     enqueue'ing this new ehEntry to be output at an outer level.
     Finally, after all that is done, we can get around to outputting
     the catch block which basically wraps all the "catch (...) {...}"
     statements in a big if/then/else construct that matches the
     correct block to call.
     
     ===================================================================== */

extern rtx emit_insn		PROTO((rtx));
extern rtx gen_nop		PROTO(());

/* local globals for function calls
   ====================================================================== */

/* used to cache "terminate ()", "unexpected ()", "set_terminate ()", and
   "set_unexpected ()" after default_conversion. (lib-except.c)  */
static tree Terminate, Unexpected, SetTerminate, SetUnexpected, CatchMatch, Throw;

/* used to cache __find_first_exception_table_match ()
   for throw (lib-except.c)  */
static tree FirstExceptionMatch;

/* used to cache a call to __unwind_function () (lib-except.c)  */
static tree Unwind;

/* holds a ready to emit call to "terminate ()".  */
static tree TerminateFunctionCall;

/* ====================================================================== */



/* data structures for my various quick and dirty stacks and queues
   Eventually, most of this should go away, because I think it can be
   integrated with stuff already built into the compiler.  */

/* =================================================================== */

struct labelNode {
  rtx label;
  struct labelNode *chain;
};


/* this is the most important structure here.  Basically this is how I store
   an exception table entry internally. */
struct ehEntry {
  rtx start_label;
  rtx end_label;
  rtx exception_handler_label;

  tree finalization;
  tree context;
};

struct ehNode {
  struct ehEntry *entry;
  struct ehNode *chain;
};

struct ehStack {
  struct ehNode *top;
};

struct ehQueue {
  struct ehNode *head;
  struct ehNode *tail;
};
/* ========================================================================= */



/* local globals - these local globals are for storing data necessary for
   generating the exception table and code in the correct order.

   ========================================================================= */

/* Holds the pc for doing "throw" */
tree saved_pc;
/* Holds the type of the thing being thrown. */
tree saved_throw_type;
/* Holds the value being thrown.  */
tree saved_throw_value;

int throw_used;

static rtx catch_clauses;
static first_catch_label;

static struct ehStack ehstack;
static struct ehQueue ehqueue;
static struct ehQueue eh_table_output_queue;
static struct labelNode *false_label_stack = NULL;
static struct labelNode *caught_return_label_stack = NULL;
/* ========================================================================= */

/* function prototypes */
static struct ehEntry *pop_eh_entry	PROTO((struct ehStack *stack));
static void enqueue_eh_entry		PROTO((struct ehQueue *queue, struct ehEntry *entry));
static rtx push_eh_entry		PROTO((struct ehStack *stack));
static struct ehEntry *dequeue_eh_entry	PROTO((struct ehQueue *queue));
static void new_eh_queue		PROTO((struct ehQueue *queue));
static void new_eh_stack		PROTO((struct ehStack *stack));
static void push_label_entry		PROTO((struct labelNode **labelstack, rtx label));
static rtx pop_label_entry		PROTO((struct labelNode **labelstack));
static rtx top_label_entry		PROTO((struct labelNode **labelstack));
static struct ehEntry *copy_eh_entry	PROTO((struct ehEntry *entry));



/* All my cheesy stack/queue/misc data structure handling routines

   ========================================================================= */

static void
push_label_entry (labelstack, label)
     struct labelNode **labelstack;
     rtx label;
{
  struct labelNode *newnode=(struct labelNode*)xmalloc (sizeof (struct labelNode));

  newnode->label = label;
  newnode->chain = *labelstack;
  *labelstack = newnode;
}

static rtx
pop_label_entry (labelstack)
     struct labelNode **labelstack;
{
  rtx label;
  struct labelNode *tempnode;

  if (! *labelstack) return NULL_RTX;

  tempnode = *labelstack;
  label = tempnode->label;
  *labelstack = (*labelstack)->chain;
  free (tempnode);

  return label;
}

static rtx
top_label_entry (labelstack)
     struct labelNode **labelstack;
{
  if (! *labelstack) return NULL_RTX;

  return (*labelstack)->label;
}

/* Push to permanent obstack for rtl generation.
   One level only!  */
static struct obstack *saved_rtl_obstack;
void
push_rtl_perm ()
{
  extern struct obstack permanent_obstack;
  extern struct obstack *rtl_obstack;
  
  saved_rtl_obstack = rtl_obstack;
  rtl_obstack = &permanent_obstack;
}

/* Pop back to normal rtl handling.  */
static void
pop_rtl_from_perm ()
{
  extern struct obstack permanent_obstack;
  extern struct obstack *rtl_obstack;
  
  rtl_obstack = saved_rtl_obstack;
}

static rtx
push_eh_entry (stack)
     struct ehStack *stack;
{
  struct ehNode *node = (struct ehNode*)xmalloc (sizeof (struct ehNode));
  struct ehEntry *entry = (struct ehEntry*)xmalloc (sizeof (struct ehEntry));

  if (stack == NULL) {
    free (node);
    free (entry);
    return NULL_RTX;
  }

  /* These are saved for the exception table.  */
  push_rtl_perm ();
  entry->start_label = gen_label_rtx ();
  entry->end_label = gen_label_rtx ();
  entry->exception_handler_label = gen_label_rtx ();
  pop_rtl_from_perm ();

  LABEL_PRESERVE_P (entry->start_label) = 1;
  LABEL_PRESERVE_P (entry->end_label) = 1;
  LABEL_PRESERVE_P (entry->exception_handler_label) = 1;

  entry->finalization = NULL_TREE;
  entry->context = current_function_decl;

  node->entry = entry;
  node->chain = stack->top;
  stack->top = node;

  enqueue_eh_entry (&eh_table_output_queue, copy_eh_entry (entry));

  return entry->start_label;
}

static struct ehEntry *
pop_eh_entry (stack)
     struct ehStack *stack;
{
  struct ehNode *tempnode;
  struct ehEntry *tempentry;

  if (stack && (tempnode = stack->top)) {
    tempentry = tempnode->entry;
    stack->top = stack->top->chain;
    free (tempnode);

    return tempentry;
  }

  return NULL;
}

static struct ehEntry *
copy_eh_entry (entry)
     struct ehEntry *entry;
{
  struct ehEntry *newentry;

  newentry = (struct ehEntry*)xmalloc (sizeof (struct ehEntry));
  memcpy ((void*)newentry, (void*)entry, sizeof (struct ehEntry));

  return newentry;
}

static void
enqueue_eh_entry (queue, entry)
     struct ehQueue *queue;
     struct ehEntry *entry;
{
  struct ehNode *node = (struct ehNode*)xmalloc (sizeof (struct ehNode));

  node->entry = entry;
  node->chain = NULL;

  if (queue->head == NULL)
    {
      queue->head = node;
    }
  else
    {
      queue->tail->chain = node;
    }
  queue->tail = node;
}

static struct ehEntry *
dequeue_eh_entry (queue)
     struct ehQueue *queue;
{
  struct ehNode *tempnode;
  struct ehEntry *tempentry;

  if (queue->head == NULL)
    return NULL;

  tempnode = queue->head;
  queue->head = queue->head->chain;

  tempentry = tempnode->entry;
  free (tempnode);

  return tempentry;
}

static void
new_eh_queue (queue)
     struct ehQueue *queue;
{
  queue->head = queue->tail = NULL;
}

static void
new_eh_stack (stack)
     struct ehStack *stack;
{
  stack->top = NULL;
}

/* cheesyness to save some typing. returns the return value rtx */
rtx
do_function_call (func, params, return_type)
     tree func, params, return_type;
{
  tree func_call;
  func_call = build_function_call (func, params);
  expand_call (func_call, NULL_RTX, 0);
  if (return_type != NULL_TREE)
    return hard_function_value (return_type, func_call);
  return NULL_RTX;
}

static void
expand_internal_throw (pc)
     rtx pc;
{
  tree params;

  emit_move_insn (DECL_RTL (saved_pc), pc);
#ifdef JUMP_TO_THROW
  emit_indirect_jump (gen_rtx (SYMBOL_REF, Pmode, "__throw"));
#else
  do_function_call (Throw, NULL_TREE, NULL_TREE);
#endif
  throw_used = 1;
}

/* ========================================================================= */

void
lang_interim_eh (finalization)
     tree finalization;
{
  if (finalization)
    end_protect (finalization);
  else
    start_protect ();
}

extern tree auto_function PROTO((tree, tree, enum built_in_function));

/* sets up all the global eh stuff that needs to be initialized at the
   start of compilation.

   This includes:
		- Setting up all the function call trees
		- Initializing the ehqueue
		- Initializing the eh_table_output_queue
		- Initializing the ehstack
*/

void
init_exception_processing ()
{
  extern tree define_function ();
  tree unexpected_fndecl, terminate_fndecl;
  tree set_unexpected_fndecl, set_terminate_fndecl;
  tree catch_match_fndecl;
  tree find_first_exception_match_fndecl;
  tree unwind_fndecl;
  tree declspecs;
  tree d;

  /* void (*)() */
  tree PFV = build_pointer_type (build_function_type
				 (void_type_node, void_list_node));

  /* arg list for the build_function_type call for set_terminate () and
     set_unexpected () */
  tree pfvlist = tree_cons (NULL_TREE, PFV, void_list_node);

  /* void (*pfvtype (void (*) ()))() */
  tree pfvtype = build_function_type (PFV, pfvlist);

  /* void vtype () */
  tree vtype = build_function_type (void_type_node, void_list_node);
  
  set_terminate_fndecl = auto_function (get_identifier ("set_terminate"),
					pfvtype, NOT_BUILT_IN);
  set_unexpected_fndecl = auto_function (get_identifier ("set_unexpected"),
					 pfvtype, NOT_BUILT_IN);
  unexpected_fndecl = auto_function (get_identifier ("unexpected"),
				     vtype, NOT_BUILT_IN);
  terminate_fndecl = auto_function (get_identifier ("terminate"),
				    vtype, NOT_BUILT_IN);

  interim_eh_hook = lang_interim_eh;

  push_lang_context (lang_name_c);

  catch_match_fndecl =
    define_function (flag_rtti
		     ? "__throw_type_match_rtti"
		     : "__throw_type_match",
		     build_function_type (ptr_type_node,
					  tree_cons (NULL_TREE, ptr_type_node,
						     tree_cons (NULL_TREE, ptr_type_node,
								tree_cons (NULL_TREE, ptr_type_node,
									   void_list_node)))),
		     NOT_BUILT_IN,
		     pushdecl,
		     0);
  find_first_exception_match_fndecl =
    define_function ("__find_first_exception_table_match",
		     build_function_type (ptr_type_node,
					  tree_cons (NULL_TREE, ptr_type_node,
						     void_list_node)),
		     NOT_BUILT_IN,
		     pushdecl,
		     0);
  unwind_fndecl =
    define_function ("__unwind_function",
		     build_function_type (void_type_node,
					  tree_cons (NULL_TREE, ptr_type_node,
						     void_list_node)),
		     NOT_BUILT_IN,
		     pushdecl,
		     0);
  throw_fndecl =
    define_function ("__throw",
		     build_function_type (void_type_node, void_list_node),
		     NOT_BUILT_IN,
		     pushdecl,
		     0);
  DECL_EXTERNAL (throw_fndecl) = 0;
  TREE_PUBLIC (throw_fndecl) = 0;

  Unexpected = default_conversion (unexpected_fndecl);
  Terminate = default_conversion (terminate_fndecl);
  SetTerminate = default_conversion (set_terminate_fndecl);
  SetUnexpected = default_conversion (set_unexpected_fndecl);
  CatchMatch = default_conversion (catch_match_fndecl);
  FirstExceptionMatch = default_conversion (find_first_exception_match_fndecl);
  Unwind = default_conversion (unwind_fndecl);
  Throw = default_conversion (throw_fndecl);
  BuiltinReturnAddress = default_conversion (builtin_return_address_fndecl);

  TerminateFunctionCall = build_function_call (Terminate, NULL_TREE);

  pop_lang_context ();

  new_eh_queue (&ehqueue);
  new_eh_queue (&eh_table_output_queue);
  new_eh_stack (&ehstack);

  declspecs = tree_cons (NULL_TREE, get_identifier ("void"), NULL_TREE);
  d = build_parse_node (INDIRECT_REF, get_identifier ("__eh_pc"));
  d = start_decl (d, declspecs, 0, NULL_TREE);
  DECL_COMMON (d) = 1;
  cp_finish_decl (d, NULL_TREE, NULL_TREE, 0, 0);
  saved_pc = lookup_name (get_identifier ("__eh_pc"), 0);

  declspecs = tree_cons (NULL_TREE, get_identifier ("void"), NULL_TREE);
  d = build_parse_node (INDIRECT_REF, get_identifier ("__eh_type"));
  d = start_decl (d, declspecs, 0, NULL_TREE);
  DECL_COMMON (d) = 1;
  cp_finish_decl (d, NULL_TREE, NULL_TREE, 0, 0);
  saved_throw_type = lookup_name (get_identifier ("__eh_type"), 0);

  declspecs = tree_cons (NULL_TREE, get_identifier ("void"), NULL_TREE);
  d = build_parse_node (INDIRECT_REF, get_identifier ("__eh_value"));
  d = start_decl (d, declspecs, 0, NULL_TREE);
  DECL_COMMON (d) = 1;
  cp_finish_decl (d, NULL_TREE, NULL_TREE, 0, 0);
  saved_throw_value = lookup_name (get_identifier ("__eh_value"), 0);
}

/* call this to begin a block of unwind protection (ie: when an object is
   constructed) */
void
start_protect ()
{
  if (! doing_eh (0))
    return;

  emit_label (push_eh_entry (&ehstack));
}
   
/* call this to end a block of unwind protection.  the finalization tree is
   the finalization which needs to be run in order to cleanly unwind through
   this level of protection. (ie: call this when a scope is exited)*/
void
end_protect (finalization)
     tree finalization;
{
  struct ehEntry *entry;

  if (! doing_eh (0))
    return;

  entry = pop_eh_entry (&ehstack);

  emit_label (entry->end_label);
  /* Put in something that takes up space, as otherwise the end
     address for the EH region could have the exact same address as
     the outer region, causing us to miss the fact that resuming
     exception handling with this PC value would be inside the outer
     region.  */
  emit_insn (gen_nop ());

  entry->finalization = finalization;

  enqueue_eh_entry (&ehqueue, entry);
}

/* call this on start of a try block. */
void
expand_start_try_stmts ()
{
  if (! doing_eh (1))
    return;

  start_protect ();
}

void
expand_end_try_stmts ()
{
  end_protect (integer_zero_node);
}


/* call this to start processing of all the catch blocks. */
void
expand_start_all_catch ()
{
  struct ehEntry *entry;
  rtx label;

  if (! doing_eh (1))
    return;

  emit_line_note (input_filename, lineno);
  label = gen_label_rtx ();

  /* The label for the exception handling block we will save.  This is
     Lresume, in the documention.  */
  emit_label (label);
  
  /* Put in something that takes up space, as otherwise the end
     address for the EH region could have the exact same address as
     the outer region, causing us to miss the fact that resuming
     exception handling with this PC value would be inside the outer
     region.  */
  emit_insn (gen_nop ());

  push_label_entry (&caught_return_label_stack, label);

  /* Start a new sequence for all the catch blocks.  We will add this
     to the gloabl sequence catch_clauses, when we have completed all
     the handlers in this handler-seq.  */
  start_sequence ();

  while (1)
    {
      entry = dequeue_eh_entry (&ehqueue);
      emit_label (entry->exception_handler_label);

      expand_expr (entry->finalization, const0_rtx, VOIDmode, 0);

      /* When we get down to the matching entry, stop.  */
      if (entry->finalization == integer_zero_node)
	break;

      /* The below can be optimized away, and we could just fall into the
	 next EH handler, if we are certain they are nested.  */
      /* Code to throw out to outer context, if we fall off end of the
	 handler.  */
      expand_internal_throw (gen_rtx (LABEL_REF,
				      Pmode,
				      entry->end_label));
      free (entry);
    }
}

/* call this to end processing of all the catch blocks. */
void
expand_end_all_catch ()
{
  rtx new_catch_clause;

  if (! doing_eh (1))
    return;

  /* Code to throw out to outer context, if we fall off end of catch
     handlers.  This is rethrow (Lresume, same id, same obj); in the
     documentation.  */
  expand_internal_throw (gen_rtx (LABEL_REF,
				  Pmode,
				  top_label_entry (&caught_return_label_stack)));

  /* Now we have the complete catch sequence.  */
  new_catch_clause = get_insns ();
  end_sequence ();
  
  /* this level of catch blocks is done, so set up the successful catch jump
     label for the next layer of catch blocks. */
  pop_label_entry (&caught_return_label_stack);

  /* Add the new sequence of catchs to the main one for this
     function.  */
  push_to_sequence (catch_clauses);
  emit_insns (new_catch_clause);
  catch_clauses = get_insns ();
  end_sequence ();
  
  /* Here we fall through into the continuation code.  */
}

/* Build a type value for use at runtime for a type that is matched
   against by the exception handling system.  */
static tree
build_eh_type_type (type)
     tree type;
{
  char *typestring;
  tree exp;

  if (type == error_mark_node)
    return error_mark_node;

  /* peel back references, so they match. */
  if (TREE_CODE (type) == REFERENCE_TYPE)
    type = TREE_TYPE (type);

  /* Peel off cv qualifiers. */
  type = TYPE_MAIN_VARIANT (type);

  if (flag_rtti)
    {
      return build1 (ADDR_EXPR, ptr_type_node, get_typeid (type));
    }

  typestring = build_overload_name (type, 1, 1);
  exp = combine_strings (build_string (strlen (typestring)+1, typestring));
  return build1 (ADDR_EXPR, ptr_type_node, exp);
}

/* Build a type value for use at runtime for a exp that is thrown or
   matched against by the exception handling system.  */
static tree
build_eh_type (exp)
     tree exp;
{
  if (flag_rtti)
    {
      exp = build_typeid (exp);
      return build1 (ADDR_EXPR, ptr_type_node, exp);
    }
  return build_eh_type_type (TREE_TYPE (exp));
}

/* call this to start a catch block. Typename is the typename, and identifier
   is the variable to place the object in or NULL if the variable doesn't
   matter.  If typename is NULL, that means its a "catch (...)" or catch
   everything.  In that case we don't need to do any type checking.
   (ie: it ends up as the "else" clause rather than an "else if" clause) */
void
expand_start_catch_block (declspecs, declarator)
     tree declspecs, declarator;
{
  rtx false_label_rtx;
  rtx protect_label_rtx;
  tree decl = NULL_TREE;
  tree init;

  if (! doing_eh (1))
    return;

  /* Create a binding level for the parm.  */
  expand_start_bindings (0);

  false_label_rtx = gen_label_rtx ();
  /* This is saved for the exception table.  */
  push_rtl_perm ();
  protect_label_rtx = gen_label_rtx ();
  pop_rtl_from_perm ();
  push_label_entry (&false_label_stack, false_label_rtx);
  push_label_entry (&false_label_stack, protect_label_rtx);

  if (declspecs)
    {
      tree exp;
      rtx call_rtx, return_value_rtx;
      tree init_type;

      decl = grokdeclarator (declarator, declspecs, CATCHPARM, 1,
			     NULL_TREE, NULL_TREE);

      if (decl == NULL_TREE)
	{
	  error ("invalid catch parameter");
	  return;
	}

      /* Figure out the type that the initializer is. */
      init_type = TREE_TYPE (decl);
      if (TREE_CODE (init_type) != REFERENCE_TYPE
	  && TREE_CODE (init_type) != POINTER_TYPE)
	init_type = build_reference_type (init_type);

      exp = saved_throw_value;
      exp = tree_cons (NULL_TREE,
		       build_eh_type_type (TREE_TYPE (decl)),
		       tree_cons (NULL_TREE,
				  saved_throw_type,
				  tree_cons (NULL_TREE, exp, NULL_TREE)));
      exp = build_function_call (CatchMatch, exp);
      call_rtx = expand_call (exp, NULL_RTX, 0);
      assemble_external (TREE_OPERAND (CatchMatch, 0));

      return_value_rtx = hard_function_value (ptr_type_node, exp);

      /* did the throw type match function return TRUE? */
      emit_cmp_insn (return_value_rtx, const0_rtx, EQ, NULL_RTX,
		    GET_MODE (return_value_rtx), 0, 0);

      /* if it returned FALSE, jump over the catch block, else fall into it */
      emit_jump_insn (gen_beq (false_label_rtx));

      init = convert_from_reference (save_expr (make_tree (init_type, call_rtx)));

      /* Do we need the below two lines? */
      /* Let `cp_finish_decl' know that this initializer is ok.  */
      DECL_INITIAL (decl) = init;
      decl = pushdecl (decl);
      cp_finish_decl (decl, init, NULL_TREE, 0, LOOKUP_ONLYCONVERTING);
    }
  else
    {
      /* Fall into the catch all section. */
    }

  /* This is the starting of something to protect.  */
  emit_label (protect_label_rtx);

  emit_line_note (input_filename, lineno);
}


/* this is called from expand_exception_blocks and
   expand_end_catch_block to expand the toplevel finalizations for a
   function.  We return the first label emitted, if any, otherwise
   return NULL_RTX.  */
static rtx
expand_leftover_cleanups ()
{
  struct ehEntry *entry;
  rtx first_label = NULL_RTX;

  while ((entry = dequeue_eh_entry (&ehqueue)) != 0)
    {
      if (! first_label)
	first_label = entry->exception_handler_label;
      emit_label (entry->exception_handler_label);

      expand_expr (entry->finalization, const0_rtx, VOIDmode, 0);

      /* The below can be optimized away, and we could just fall into the
	 next EH handler, if we are certain they are nested.  */
      /* Code to throw out to outer context, if we fall off end of the
	 handler.  */
      expand_internal_throw (gen_rtx (LABEL_REF,
				      Pmode,
				      entry->end_label));

      /* leftover try block, opps.  */
      if (entry->finalization == integer_zero_node)
	abort ();

      free (entry);
    }

  return first_label;
}

/* Call this to end a catch block.  Its responsible for emitting the
   code to handle jumping back to the correct place, and for emitting
   the label to jump to if this catch block didn't match.  */
void expand_end_catch_block ()
{
  rtx start_protect_label_rtx;
  rtx end_protect_label_rtx;
  tree decls;
  struct ehEntry entry;

  if (! doing_eh (1))
    return;

  /* fall to outside the try statement when done executing handler and
     we fall off end of handler.  This is jump Lresume in the
     documentation.  */
  emit_jump (top_label_entry (&caught_return_label_stack));

  /* We end the rethrow protection region as soon as we hit a label. */
  end_protect_label_rtx = expand_leftover_cleanups ();

  /* Code to throw out to outer context, if we get a throw from within
     our catch handler. */
  /* These are saved for the exception table.  */
  push_rtl_perm ();
  entry.exception_handler_label = gen_label_rtx ();
  pop_rtl_from_perm ();
  /* This label is Lhandler in the documentation.  */
  emit_label (entry.exception_handler_label);
  expand_internal_throw (gen_rtx (LABEL_REF,
				  Pmode,
				  top_label_entry (&caught_return_label_stack)));

  /* No associated finalization.  */
  entry.finalization = NULL_TREE;
  entry.context = current_function_decl;

  if (end_protect_label_rtx == NULL_RTX)
    end_protect_label_rtx = entry.exception_handler_label;

  /* Because we are emitted out of line, we have to protect this. */
  /* label for the start of the protection region.  */
  start_protect_label_rtx = pop_label_entry (&false_label_stack);

  /* Cleanup the EH parameter.  */
  decls = getdecls ();
  expand_end_bindings (decls, decls != NULL_TREE, 0);
      
  /* label we emit to jump to if this catch block didn't match. */
  /* This the closing } in the `if (eq) {' of the documentation.  */
  emit_label (pop_label_entry (&false_label_stack));

  /* Because we are reordered out of line, we have to protect this. */
  entry.start_label = start_protect_label_rtx;
  entry.end_label = end_protect_label_rtx;

  LABEL_PRESERVE_P (entry.start_label) = 1;
  LABEL_PRESERVE_P (entry.end_label) = 1;
  LABEL_PRESERVE_P (entry.exception_handler_label) = 1;

  /* These set up a call to throw the caught exception into the outer
     context.  */
  enqueue_eh_entry (&eh_table_output_queue, copy_eh_entry (&entry));
}

/* unwind the stack. */
static void
do_unwind (inner_throw_label)
     rtx inner_throw_label;
{
#if defined(SPARC_STACK_ALIGN) /* was sparc */
  tree fcall;
  tree params;
  rtx return_val_rtx;
  rtx temp;

  /* call to  __builtin_return_address () */
  params = tree_cons (NULL_TREE, integer_zero_node, NULL_TREE);
  fcall = build_function_call (BuiltinReturnAddress, params);
  return_val_rtx = expand_expr (fcall, NULL_RTX, Pmode, 0);
  /* In the return, the new pc is pc+8, as the value coming in is
     really the address of the call insn, not the next insn.  */
  temp = gen_reg_rtx (Pmode);
  emit_move_insn (temp, inner_throw_label);
  emit_move_insn (return_val_rtx, plus_constant (temp, -8));
  easy_expand_asm ("ret");
  easy_expand_asm ("restore");
  emit_barrier ();
#endif
#if defined(ARM_FRAME_RTX)  /* was __arm */
  if (flag_omit_frame_pointer)
    sorry ("this implementation of exception handling requires a frame pointer");

  emit_move_insn (stack_pointer_rtx,
		  gen_rtx (MEM, Pmode, plus_constant (hard_frame_pointer_rtx, -8)));
  emit_move_insn (hard_frame_pointer_rtx,
		  gen_rtx (MEM, Pmode, plus_constant (hard_frame_pointer_rtx, -12)));
#endif
#if defined(TARGET_88000) /* was m88k */
  rtx temp_frame = frame_pointer_rtx;

  temp_frame = memory_address (Pmode, temp_frame);
  temp_frame = copy_to_reg (gen_rtx (MEM, Pmode, temp_frame));

  /* hopefully this will successfully pop the frame! */
  emit_move_insn (frame_pointer_rtx, temp_frame);
  emit_move_insn (stack_pointer_rtx, frame_pointer_rtx);
  emit_move_insn (arg_pointer_rtx, frame_pointer_rtx);
  emit_insn (gen_add2_insn (stack_pointer_rtx, gen_rtx (CONST_INT, VOIDmode,
						     (HOST_WIDE_INT)m88k_debugger_offset (stack_pointer_rtx, 0))));

#if 0
  emit_insn (gen_add2_insn (arg_pointer_rtx, gen_rtx (CONST_INT, VOIDmode,
						   -(HOST_WIDE_INT)m88k_debugger_offset (arg_pointer_rtx, 0))));

  emit_move_insn (stack_pointer_rtx, arg_pointer_rtx);

  emit_insn (gen_add2_insn (stack_pointer_rtx, gen_rtx (CONST_INT, VOIDmode,
						     (HOST_WIDE_INT)m88k_debugger_offset (arg_pointer_rtx, 0))));
#endif
#endif
#if !defined(TARGET_88000) && !defined(ARM_FRAME_RTX) && !defined(SPARC_STACK_ALIGN)
  tree fcall;
  tree params;
  rtx return_val_rtx;

  /* call to  __builtin_return_address () */
  params = tree_cons (NULL_TREE, integer_zero_node, NULL_TREE);
  fcall = build_function_call (BuiltinReturnAddress, params);
  return_val_rtx = expand_expr (fcall, NULL_RTX, Pmode, 0);
#if 0
  /* I would like to do this here, but doesn't seem to work. */
  emit_move_insn (return_val_rtx, inner_throw_label);
  /* So, for now, just pass throw label to stack unwinder. */
#endif
  params = tree_cons (NULL_TREE, make_tree (ptr_type_node,
					    inner_throw_label), NULL_TREE);
  
  do_function_call (Unwind, params, NULL_TREE);
  assemble_external (TREE_OPERAND (Unwind, 0));
  emit_barrier ();
#endif
}


/* is called from expand_exception_blocks () to generate the code in a function
   to "throw" if anything in the function needs to perform a throw.

   expands "throw" as the following pseudo code:

	throw:
		eh = find_first_exception_match (saved_pc);
	    if (!eh) goto gotta_rethrow_it;
		goto eh;

	gotta_rethrow_it:
		saved_pc = __builtin_return_address (0);
		pop_to_previous_level ();
		goto throw;

 */
void
expand_builtin_throw ()
{
  tree fcall;
  tree params;
  rtx return_val_rtx;
  rtx gotta_rethrow_it;
  rtx gotta_call_terminate;
  rtx unwind_and_throw;
  rtx goto_unwind_and_throw;
  rtx top_of_loop;
  rtx unwind_first;
  tree t;

  if (! doing_eh (0))
    return;

  if (! throw_used)
    return;

  params = void_list_node;
  t = build_parse_node (CALL_EXPR, get_identifier ("__throw"), params, NULL_TREE);
  start_function (decl_tree_cons (NULL_TREE, get_identifier ("static"),
				  void_list_node),
		  t, NULL_TREE, NULL_TREE, 0);
  store_parm_decls ();
  pushlevel (0);
  clear_last_expr ();
  push_momentary ();
  expand_start_bindings (0);

  gotta_rethrow_it = gen_label_rtx ();
  gotta_call_terminate = gen_label_rtx ();
  unwind_and_throw = gen_label_rtx ();
  goto_unwind_and_throw = gen_label_rtx ();
  top_of_loop = gen_label_rtx ();
  unwind_first = gen_label_rtx ();

  emit_jump (unwind_first);

  emit_label (top_of_loop);

  /* search for an exception handler for the saved_pc */
  return_val_rtx = do_function_call (FirstExceptionMatch,
				     tree_cons (NULL_TREE, saved_pc, NULL_TREE),
				     ptr_type_node);
  assemble_external (TREE_OPERAND (FirstExceptionMatch, 0));

  /* did we find one? */
  emit_cmp_insn (return_val_rtx, const0_rtx, EQ, NULL_RTX,
		 GET_MODE (return_val_rtx), 0, 0);

  /* if not, jump to gotta_rethrow_it */
  emit_jump_insn (gen_beq (gotta_rethrow_it));

  /* we found it, so jump to it */
  emit_indirect_jump (return_val_rtx);

  /* code to deal with unwinding and looking for it again */
  emit_label (gotta_rethrow_it);

  /* call to  __builtin_return_address () */
#if defined(ARM_FRAME_RTX)  /* was __arm */
/* This replaces a 'call' to __builtin_return_address */
  return_val_rtx = gen_reg_rtx (Pmode);
  emit_move_insn (return_val_rtx, gen_rtx (MEM, Pmode, plus_constant (hard_frame_pointer_rtx, -4)));
#else
  params = tree_cons (NULL_TREE, integer_zero_node, NULL_TREE);
  fcall = build_function_call (BuiltinReturnAddress, params);
  return_val_rtx = expand_expr (fcall, NULL_RTX, Pmode, 0);
#endif

  /* did __builtin_return_address () return a valid address? */
  emit_cmp_insn (return_val_rtx, const0_rtx, EQ, NULL_RTX,
		 GET_MODE (return_val_rtx), 0, 0);

  emit_jump_insn (gen_beq (gotta_call_terminate));

#if defined(ARM_FRAME_RTX)  /* was __arm */
  /* On the ARM, '__builtin_return_address',  must have 4
     subtracted from it. */
  emit_insn (gen_add2_insn (return_val_rtx, GEN_INT (-4)));

  /* If we are generating code for an ARM2/ARM3 machine or for an ARM6 in 26 bit
     mode, the condition codes must be masked out of the return value, or else
     they will confuse BuiltinReturnAddress.  This does not apply to ARM6 and
     later processors when running in 32 bit mode. */
  if (!TARGET_6)
    emit_insn (gen_rtx (SET, Pmode, return_val_rtx, gen_rtx (AND, Pmode, return_val_rtx, GEN_INT (0x03fffffc))));
#else
#if !defined(SPARC_STACK_ALIGN) /* was sparc */
  /* On the SPARC, __builtin_return_address is already -8, no need to
     subtract any more from it. */
  return_val_rtx = plus_constant (return_val_rtx, -1);
#endif
#endif

  /* yes it did */
  t = build_modify_expr (saved_pc, NOP_EXPR, make_tree (ptr_type_node, return_val_rtx));
  expand_expr (t, const0_rtx, VOIDmode, 0);

  do_unwind (gen_rtx (LABEL_REF, Pmode, top_of_loop));
  emit_jump (top_of_loop);

  /* no it didn't --> therefore we need to call terminate */
  emit_label (gotta_call_terminate);
  do_function_call (Terminate, NULL_TREE, NULL_TREE);
  assemble_external (TREE_OPERAND (Terminate, 0));

  {
    rtx ret_val, return_val_rtx;
    emit_label (unwind_first);
    ret_val = expand_builtin_return_addr (BUILT_IN_RETURN_ADDRESS,
					  0, hard_frame_pointer_rtx);

    /* Set it up so that we continue inside, at the top of the loop.  */
    emit_move_insn (ret_val, gen_rtx (LABEL_REF, Pmode, top_of_loop));
#ifdef NORMAL_RETURN_ADDR_OFFSET
  return_val_rtx = plus_constant (ret_val, -NORMAL_RETURN_ADDR_OFFSET);
    if (return_val_rtx != ret_val)
      emit_move_insn (ret_val, return_val_rtx);
#endif

    /* Fall into epilogue to unwind prologue. */
  }

  expand_end_bindings (getdecls(), 1, 0);
  poplevel (1, 0, 0);
  pop_momentary ();

  finish_function (lineno, 0, 0);
}


void
expand_start_eh_spec ()
{
  start_protect ();
}

void
expand_end_eh_spec (raises)
     tree raises;
{
  tree expr, second_try;
  rtx check = gen_label_rtx ();
  rtx cont;
  rtx ret = gen_reg_rtx (Pmode);
  rtx flag = gen_reg_rtx (TYPE_MODE (integer_type_node));
  rtx end = gen_label_rtx ();

  expr = make_node (RTL_EXPR);
  TREE_TYPE (expr) = void_type_node;
  RTL_EXPR_RTL (expr) = const0_rtx;
  TREE_SIDE_EFFECTS (expr) = 1;
  start_sequence_for_rtl_expr (expr);
  cont = gen_label_rtx ();
  emit_move_insn (ret, gen_rtx (LABEL_REF, Pmode, cont));
  emit_jump (check);
  emit_label (cont);
  jumpif (make_tree (integer_type_node, flag), end);
  do_function_call (Terminate, NULL_TREE, NULL_TREE);
  assemble_external (TREE_OPERAND (Terminate, 0));
  emit_barrier ();
  RTL_EXPR_SEQUENCE (expr) = get_insns ();
  end_sequence ();
  
  second_try = expr;

  expr = make_node (RTL_EXPR);
  TREE_TYPE (expr) = void_type_node;
  RTL_EXPR_RTL (expr) = const0_rtx;
  TREE_SIDE_EFFECTS (expr) = 1;
  start_sequence_for_rtl_expr (expr);

  cont = gen_label_rtx ();
  emit_move_insn (ret, gen_rtx (LABEL_REF, Pmode, cont));
  emit_jump (check);
  emit_label (cont);
  jumpif (make_tree (integer_type_node, flag), end);
  start_protect ();
  do_function_call (Unexpected, NULL_TREE, NULL_TREE);
  assemble_external (TREE_OPERAND (Unexpected, 0));
  emit_barrier ();
  end_protect (second_try);
  
  emit_label (check);
  emit_move_insn (flag, const1_rtx);
  cont = gen_label_rtx ();
  while (raises)
    {
      tree exp;
      tree match_type = TREE_VALUE (raises);
      
      if (match_type)
	{
	  /* check TREE_VALUE (raises) here */
	  exp = saved_throw_value;
	  exp = tree_cons (NULL_TREE,
			   build_eh_type_type (match_type),
			   tree_cons (NULL_TREE,
				      saved_throw_type,
				      tree_cons (NULL_TREE, exp, NULL_TREE)));
	  exp = build_function_call (CatchMatch, exp);
	  assemble_external (TREE_OPERAND (CatchMatch, 0));

	  jumpif (exp, cont);
	}

      raises = TREE_CHAIN (raises);
    }
  emit_move_insn (flag, const0_rtx);
  emit_label (cont);
  emit_indirect_jump (ret);
  emit_label (end);
  
  RTL_EXPR_SEQUENCE (expr) = get_insns ();
  end_sequence ();
  
  end_protect (expr);
}

/* This is called to expand all the toplevel exception handling
   finalization for a function.  It should only be called once per
   function.  */
void
expand_exception_blocks ()
{
  static rtx funcend;
  rtx insns;

  start_sequence ();

  funcend = gen_label_rtx ();
  emit_jump (funcend);
  /* expand_null_return (); */

  start_sequence ();

  /* Add all the catch clauses here.  */
  emit_insns (catch_clauses);
  catch_clauses = NULL_RTX;

  expand_leftover_cleanups ();

  insns = get_insns ();
  end_sequence ();
  
  /* Do this after we expand leftover cleanups, so that the end_protect
     that expand_end_eh_spec does will match the right start_protect,
     and make sure it comes out before the terminate protected region.  */
  if (TYPE_RAISES_EXCEPTIONS (TREE_TYPE (current_function_decl)))
    {
      expand_end_eh_spec (TYPE_RAISES_EXCEPTIONS (TREE_TYPE (current_function_decl)));
      push_to_sequence (insns);

      /* Now expand any new ones.  */
      expand_leftover_cleanups ();

      insns = get_insns ();
      end_sequence ();
    }

  if (insns)
    {
      struct ehEntry entry;

      /* These are saved for the exception table.  */
      push_rtl_perm ();
      entry.start_label = gen_label_rtx ();
      entry.end_label = gen_label_rtx ();
      entry.exception_handler_label = gen_label_rtx ();
      entry.finalization = TerminateFunctionCall;
      entry.context = current_function_decl;
      assemble_external (TREE_OPERAND (Terminate, 0));
      pop_rtl_from_perm ();

      LABEL_PRESERVE_P (entry.start_label) = 1;
      LABEL_PRESERVE_P (entry.end_label) = 1;
      LABEL_PRESERVE_P (entry.exception_handler_label) = 1;

      emit_label (entry.start_label);
      emit_insns (insns);

      enqueue_eh_entry (&eh_table_output_queue, copy_eh_entry (&entry));

      emit_label (entry.exception_handler_label);
      expand_expr (entry.finalization, const0_rtx, VOIDmode, 0);
      emit_label (entry.end_label);
      emit_barrier ();
    }

  {
    /* Mark the end of the stack unwinder.  */
    rtx unwind_insns;
    start_sequence ();
    end_eh_unwinder (funcend);
    expand_leftover_cleanups ();
    unwind_insns = get_insns ();
    end_sequence ();
    if (unwind_insns)
      {
	insns = unwind_insns;
	emit_insns (insns);
      }
  }

  emit_label (funcend);

  /* Only if we had previous insns do we want to emit the jump around
     them.  If there weren't any, then insns will remain NULL_RTX.  */
  if (insns)
    insns = get_insns ();
  end_sequence ();

  emit_insns (insns);
}


/* call this to expand a throw statement.  This follows the following
   algorithm:

	1. Allocate space to save the current PC onto the stack.
	2. Generate and emit a label and save its address into the
		newly allocated stack space since we can't save the pc directly.
	3. If this is the first call to throw in this function:
		generate a label for the throw block
	4. jump to the throw block label.  */
void
expand_throw (exp)
     tree exp;
{
  rtx label;

  if (! doing_eh (1))
    return;

  /* This is the label that represents where in the code we were, when
     we got an exception.  This needs to be updated when we rethrow an
     exception, so that the matching routine knows to search out.  */
  label = gen_label_rtx ();
  emit_label (label);

  if (exp)
    {
      tree throw_type;
      tree e;

      /* throw expression */
      /* First, decay it. */
      exp = decay_conversion (exp);

      if (TREE_CODE (TREE_TYPE (exp)) == POINTER_TYPE)
	{
	  throw_type = build_eh_type (exp);
	  exp = build_reinterpret_cast (ptr_type_node, exp);
	}
      else
	{
	  /* Make a copy of the thrown object.  WP 15.1.5  */
	  exp = build_new (NULL_TREE, TREE_TYPE (exp),
			   build_tree_list (NULL_TREE, exp),
			   0);

	  if (exp == error_mark_node)
	    error ("  in thrown expression");

	  throw_type = build_eh_type (build_indirect_ref (exp, NULL_PTR));
	}

      e = build_modify_expr (saved_throw_type, NOP_EXPR, throw_type);
      expand_expr (e, const0_rtx, VOIDmode, 0);
      e = build_modify_expr (saved_throw_value, NOP_EXPR, exp);
      e = build1 (CLEANUP_POINT_EXPR, TREE_TYPE (e), e);
      expand_expr (e, const0_rtx, VOIDmode, 0);
    }
  else
    {
      /* rethrow current exception */
      /* This part is easy, as we don't have to do anything else.  */
    }

  expand_internal_throw (gen_rtx (LABEL_REF, Pmode, label));
}

void
end_protect_partials () {
  while (protect_list)
    {
      end_protect (TREE_VALUE (protect_list));
      protect_list = TREE_CHAIN (protect_list);
    }
}

int
might_have_exceptions_p ()
{
  if (eh_table_output_queue.head)
    return 1;
  return 0;
}

/* Output the exception table.
 Return the number of handlers.  */
void
emit_exception_table ()
{
  int count = 0;
  extern FILE *asm_out_file;
  struct ehEntry *entry;
  tree eh_node_decl;

  if (! doing_eh (0))
    return;

  exception_section ();

  /* Beginning marker for table. */
  assemble_align (GET_MODE_ALIGNMENT (Pmode));
  assemble_label ("__EXCEPTION_TABLE__");
  output_exception_table_entry (asm_out_file,
				const0_rtx, const0_rtx, const0_rtx);

 while (entry = dequeue_eh_entry (&eh_table_output_queue))
   {
     tree context = entry->context;

     if (context && ! TREE_ASM_WRITTEN (context))
       continue;

     count++;
     output_exception_table_entry (asm_out_file,
				   entry->start_label, entry->end_label,
				   entry->exception_handler_label);
  }

  /* Ending marker for table. */
  assemble_label ("__EXCEPTION_END__");
  output_exception_table_entry (asm_out_file,
				constm1_rtx, constm1_rtx, constm1_rtx);
}

void
register_exception_table ()
{
  emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "__register_exceptions"), 0,
		     VOIDmode, 1,
		     gen_rtx (SYMBOL_REF, Pmode, "__EXCEPTION_TABLE__"),
		     Pmode);
}

/* Build a throw expression.  */
tree
build_throw (e)
     tree e;
{
  if (e != error_mark_node)
    {
      e = build1 (THROW_EXPR, void_type_node, e);
      TREE_SIDE_EFFECTS (e) = 1;
      TREE_USED (e) = 1;
    }
  return e;
}

start_eh_unwinder ()
{
  start_protect ();
}

end_eh_unwinder (end)
     rtx end;
{
  tree expr;
  rtx return_val_rtx, ret_val, label;

  if (! doing_eh (0))
    return;

  expr = make_node (RTL_EXPR);
  TREE_TYPE (expr) = void_type_node;
  RTL_EXPR_RTL (expr) = const0_rtx;
  TREE_SIDE_EFFECTS (expr) = 1;
  start_sequence_for_rtl_expr (expr);

  ret_val = expand_builtin_return_addr (BUILT_IN_RETURN_ADDRESS,
					0, hard_frame_pointer_rtx);
  return_val_rtx = copy_to_reg (ret_val);
#ifdef NORMAL_RETURN_ADDR_OFFSET
  return_val_rtx = plus_constant (return_val_rtx, NORMAL_RETURN_ADDR_OFFSET-1);
#else
  return_val_rtx = plus_constant (return_val_rtx, -1);
#endif
  emit_move_insn (DECL_RTL (saved_pc), return_val_rtx);
  
#ifdef JUMP_TO_THROW
  emit_move_insn (ret_val, gen_rtx (SYMBOL_REF, Pmode, "__throw"));
#else
  label = gen_label_rtx ();
  emit_move_insn (ret_val, gen_rtx (LABEL_REF, Pmode, label));
#endif

#ifdef NORMAL_RETURN_ADDR_OFFSET
  return_val_rtx = plus_constant (ret_val, -NORMAL_RETURN_ADDR_OFFSET);
  if (return_val_rtx != ret_val)
    emit_move_insn (ret_val, return_val_rtx);
#endif
  
  emit_jump (end);  

#ifndef JUMP_TO_THROW
  emit_label (label);
  do_function_call (Throw, NULL_TREE, NULL_TREE);
#endif
  
  RTL_EXPR_SEQUENCE (expr) = get_insns ();
  end_sequence ();
  end_protect (expr);
}
