/* Handle exceptional things in C++.
   Copyright (C) 1989, 1992, 1993, 1994 Free Software Foundation, Inc.
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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* High-level class interface. */

#include "config.h"
#include "tree.h"
#include "rtl.h"
#include "cp-tree.h"
#include "flags.h"
#include "obstack.h"
#include "expr.h"

extern void (*interim_eh_hook)	PROTO((tree));

/* holds the fndecl for __builtin_return_address () */
tree builtin_return_address_fndecl;

/* Define at your own risk!  */
#ifndef CROSS_COMPILE
#ifdef sun
#ifdef sparc
#define TRY_NEW_EH
#endif
#endif
#endif

#ifndef TRY_NEW_EH

static void
sorry_no_eh ()
{
  static int warned = 0;
  if (! warned)
    {
      sorry ("exception handling not supported");
      warned = 1;
    }
}

void
expand_exception_blocks ()
{
}

void
start_protect ()
{
}

void
end_protect (finalization)
     tree finalization;
{
}

void
expand_start_try_stmts ()
{
  sorry_no_eh ();
}

void
expand_end_try_stmts ()
{
}

void
expand_start_all_catch ()
{
}

void
expand_end_all_catch ()
{
}

void
expand_start_catch_block (declspecs, declarator)
     tree declspecs, declarator;
{
}

void
expand_end_catch_block ()
{
}

void
init_exception_processing ()
{
}

void
expand_throw (exp)
     tree exp;
{
  sorry_no_eh ();
}

#else

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
to supporting exception handling as per Stroustrup's 2nd edition.
It is a complete rewrite of all the EH stuff that was here before
	Shortcomings:
		1. The type of the throw and catch must still match
		   exactly (no support yet for matching base classes)
		2. Throw specifications of functions still doesnt't work.
	Cool Things:
		1. Destructors are called properly :-)
		2. No overhead for the non-exception thrown case.
		3. Fixing shortcomings 1 and 2 is simple.
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

  assemble_integer (start_label, BITS_PER_WORD/BITS_PER_UNIT, 1);
  assemble_integer (end_label, BITS_PER_WORD/BITS_PER_UNIT, 1);
  assemble_integer (eh_label, BITS_PER_WORD/BITS_PER_UNIT, 1);
  putc ('\n', file);		/* blank line */
}
   
static void
easy_expand_asm (str)
     char *str;
{
  expand_asm (build_string (strlen (str)+1, str));
}

/* unwind the stack. */
static void
do_unwind (throw_label)
     rtx throw_label;
{
#ifdef sparc
  extern FILE *asm_out_file;
  tree fcall;
  tree params;
  rtx return_val_rtx;

  /* call to  __builtin_return_address () */
  params=tree_cons (NULL_TREE, integer_zero_node, NULL_TREE);
  fcall = build_function_call (BuiltinReturnAddress, params);
  return_val_rtx = expand_expr (fcall, NULL_RTX, SImode, 0);
  /* In the return, the new pc is pc+8, as the value comming in is
     really the address of the call insn, not the next insn.  */
  emit_move_insn (return_val_rtx, plus_constant(gen_rtx (LABEL_REF,
							 Pmode,
							 throw_label), -8));
  /* We use three values, PC, type, and value */
  easy_expand_asm ("st %l0,[%fp]");
  easy_expand_asm ("st %l1,[%fp+4]");
  easy_expand_asm ("st %l2,[%fp+8]");
  easy_expand_asm ("ret");
  easy_expand_asm ("restore");
  emit_barrier ();
#endif
#if m88k
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
  named_section (".gcc_except_table");
#else
  text_section ();
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
     so.  Luckily, we have a couple of routines "get_last_insn ()" and
     "set_last_insn ()" provided.  When the start of a catch block is
     encountered, we save a pointer to the last insn generated.  After
     the catch block is generated, we save a pointer to the first
     catch block insn and the last catch block insn with the routines
     "NEXT_INSN ()" and "get_last_insn ()".  We then set the last insn
     to be the last insn generated before the catch block, and set the
     NEXT_INSN (last_insn) to zero.

     Since catch blocks might be nested inside other catch blocks, and
     we munge the chain of generated insns after the catch block is
     generated, we need to store the pointers to the last insn
     generated in a stack, so that when the end of a catch block is
     encountered, the last insn before the current catch block can be
     popped and set to be the last insn, and the first and last insns
     of the catch block just generated can be enqueue'd for output at
     a later time.
  		
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
   "set_unexpected ()" after default_conversion. (lib-except.c) */
static tree Terminate, Unexpected, SetTerminate, SetUnexpected, CatchMatch;

/* used to cache __find_first_exception_table_match ()
   for throw (lib-except.c)  */
static tree FirstExceptionMatch;

/* used to cache a call to __unwind_function () (lib-except.c) */
static tree Unwind;

/* holds a ready to emit call to "terminate ()". */
static tree TerminateFunctionCall;

/* ====================================================================== */



/* data structures for my various quick and dirty stacks and queues
   Eventually, most of this should go away, because I think it can be
   integrated with stuff already built into the compiler. */

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

struct exceptNode {
    rtx catchstart;
	rtx catchend;

	struct exceptNode *chain;
 };

struct exceptStack {
	struct exceptNode *top;
 };
/* ========================================================================= */



/* local globals - these local globals are for storing data necessary for
   generating the exception table and code in the correct order.

   ========================================================================= */

/* Holds the pc for doing "throw" */
rtx saved_pc;
/* Holds the type of the thing being thrown. */
rtx saved_throw_type;
/* Holds the value being thrown.  */
rtx saved_throw_value;

rtx throw_label;

static struct ehStack ehstack;
static struct ehQueue ehqueue;
static struct ehQueue eh_table_output_queue;
static struct exceptStack exceptstack;
static struct labelNode *false_label_stack = NULL;
static struct labelNode *caught_return_label_stack = NULL;
/* ========================================================================= */

/* function prototypes */
static struct ehEntry *pop_eh_entry	PROTO((struct ehStack *stack));
static void enqueue_eh_entry		PROTO((struct ehQueue *queue, struct ehEntry *entry));
static void push_except_stmts		PROTO((struct exceptStack *exceptstack,
					 rtx catchstart, rtx catchend));
static int pop_except_stmts		PROTO((struct exceptStack *exceptstack,
					 rtx *catchstart, rtx *catchend));
static rtx push_eh_entry		PROTO((struct ehStack *stack));
static struct ehEntry *dequeue_eh_entry	PROTO((struct ehQueue *queue));
static void new_eh_queue		PROTO((struct ehQueue *queue));
static void new_eh_stack		PROTO((struct ehStack *stack));
static void new_except_stack		PROTO((struct exceptStack *queue));
static void push_last_insn		PROTO(());
static rtx pop_last_insn		PROTO(());
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

static void
push_except_stmts (exceptstack, catchstart, catchend)
     struct exceptStack *exceptstack;
     rtx catchstart, catchend;
{
  struct exceptNode *newnode = (struct exceptNode*)
    xmalloc (sizeof (struct exceptNode));

  newnode->catchstart = catchstart;
  newnode->catchend = catchend;
  newnode->chain = exceptstack->top;

  exceptstack->top = newnode;
}

static int
pop_except_stmts (exceptstack, catchstart, catchend)
     struct exceptStack *exceptstack;
     rtx *catchstart, *catchend;
{
  struct exceptNode *tempnode;

  if (!exceptstack->top) {
    *catchstart = *catchend = NULL_RTX;
    return 0;
  }

  tempnode = exceptstack->top;
  exceptstack->top = exceptstack->top->chain;

  *catchstart = tempnode->catchstart;
  *catchend = tempnode->catchend;
  free (tempnode);

  return 1;
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

  entry->finalization = NULL_TREE;

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

static void
new_except_stack (stack)
     struct exceptStack *stack;
{
  stack->top = NULL;
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

/* sets up all the global eh stuff that needs to be initialized at the
   start of compilation.

   This includes:
		- Setting up all the function call trees
		- Initializing the ehqueue
		- Initializing the eh_table_output_queue
		- Initializing the ehstack
		- Initializing the exceptstack
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
  tree temp, PFV;

  interim_eh_hook = lang_interim_eh;

  /* void (*)() */
  PFV = build_pointer_type (build_function_type (void_type_node, void_list_node));

  /* arg list for the build_function_type call for set_terminate () and
     set_unexpected () */
  temp = tree_cons (NULL_TREE, PFV, void_list_node);

  push_lang_context (lang_name_c);

  set_terminate_fndecl =
    define_function ("set_terminate",
		     build_function_type (PFV, temp),
		     NOT_BUILT_IN,
		     pushdecl,
		     0);
  set_unexpected_fndecl =
    define_function ("set_unexpected",
		     build_function_type (PFV, temp),
		     NOT_BUILT_IN,
		     pushdecl,
		     0);

  unexpected_fndecl =
    define_function ("unexpected",
		     build_function_type (void_type_node, void_list_node),
		     NOT_BUILT_IN,
		     pushdecl,
		     0);
  terminate_fndecl =
    define_function ("terminate",
		     build_function_type (void_type_node, void_list_node),
		     NOT_BUILT_IN,
		     pushdecl,
		     0);
  catch_match_fndecl =
    define_function ("__throw_type_match",
		     build_function_type (integer_type_node,
					  tree_cons (NULL_TREE, string_type_node, tree_cons (NULL_TREE, ptr_type_node, void_list_node))),
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
					  tree_cons (NULL_TREE, ptr_type_node, void_list_node)),
		     NOT_BUILT_IN,
		     pushdecl,
		     0);

  Unexpected = default_conversion (unexpected_fndecl);
  Terminate = default_conversion (terminate_fndecl);
  SetTerminate = default_conversion (set_terminate_fndecl);
  SetUnexpected = default_conversion (set_unexpected_fndecl);
  CatchMatch = default_conversion (catch_match_fndecl);
  FirstExceptionMatch = default_conversion (find_first_exception_match_fndecl);
  Unwind = default_conversion (unwind_fndecl);
  BuiltinReturnAddress = default_conversion (builtin_return_address_fndecl);

  TerminateFunctionCall = build_function_call (Terminate, NULL_TREE);

  pop_lang_context ();
  throw_label = gen_label_rtx ();
  saved_pc = gen_rtx (REG, Pmode, 16);
  saved_throw_type = gen_rtx (REG, Pmode, 17);
  saved_throw_value = gen_rtx (REG, Pmode, 18);

  new_eh_queue (&ehqueue);
  new_eh_queue (&eh_table_output_queue);
  new_eh_stack (&ehstack);
  new_except_stack (&exceptstack);
}

/* call this to begin a block of unwind protection (ie: when an object is
   constructed) */
void
start_protect ()
{
  if (doing_eh (0))
    {
      emit_label (push_eh_entry (&ehstack));
    }
}
   
/* call this to end a block of unwind protection.  the finalization tree is
   the finalization which needs to be run in order to cleanly unwind through
   this level of protection. (ie: call this when a scope is exited)*/
void
end_protect (finalization)
     tree finalization;
{
  struct ehEntry *entry = pop_eh_entry (&ehstack);

  if (! doing_eh (0))
    return;

  emit_label (entry->end_label);

  entry->finalization = finalization;

  enqueue_eh_entry (&ehqueue, entry);
}

/* call this on start of a try block. */
void
expand_start_try_stmts ()
{
  if (doing_eh (1))
    {
      start_protect ();
    }
}

void
expand_end_try_stmts ()
{
  end_protect (integer_zero_node);
}

struct insn_save_node {
	rtx last;
	struct insn_save_node *chain;
 };

static struct insn_save_node *InsnSave = NULL;


/* Used to keep track of where the catch blocks start.  */
static void
push_last_insn ()
{
  struct insn_save_node *newnode = (struct insn_save_node*)
    xmalloc (sizeof (struct insn_save_node));

  newnode->last = get_last_insn ();
  newnode->chain = InsnSave;
  InsnSave = newnode;
}

/* Use to keep track of where the catch blocks start.  */
static rtx
pop_last_insn ()
{
  struct insn_save_node *tempnode;
  rtx temprtx;

  if (!InsnSave) return NULL_RTX;

  tempnode = InsnSave;
  temprtx = tempnode->last;
  InsnSave = InsnSave->chain;

  free (tempnode);

  return temprtx;
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
  /* The label for the exception handling block we will save.  */
  emit_label (label);
  
  push_label_entry (&caught_return_label_stack, label);

  /* Remember where we started. */
  push_last_insn ();

  emit_insn (gen_nop ());

  /* Will this help us not stomp on it? */
  emit_insn (gen_rtx (USE, VOIDmode, saved_throw_type));
  emit_insn (gen_rtx (USE, VOIDmode, saved_throw_value));

  while (1)
    {
      entry = dequeue_eh_entry (&ehqueue);
      emit_label (entry->exception_handler_label);

      expand_expr (entry->finalization, const0_rtx, VOIDmode, 0);

      /* When we get down to the matching entry, stop.  */
      if (entry->finalization == integer_zero_node)
	break;

      free (entry);
    }

  /* This goes when the below moves out of our way.  */
#if 1
  label = gen_label_rtx ();
  emit_jump (label);
#endif
  
  /* All this should be out of line, and saved back in the exception handler
     block area.  */
#if 1
  entry->start_label = entry->exception_handler_label;
  /* These are saved for the exception table.  */
  push_rtl_perm ();
  entry->end_label = gen_label_rtx ();
  entry->exception_handler_label = gen_label_rtx ();
  entry->finalization = TerminateFunctionCall;
  pop_rtl_from_perm ();
  emit_label (entry->end_label);

  enqueue_eh_entry (&eh_table_output_queue, copy_eh_entry (entry));

  /* After running the finalization, continue on out to the next
     cleanup, if we have nothing better to do.  */
  emit_move_insn (saved_pc, gen_rtx (LABEL_REF, Pmode, entry->end_label));
  /* Will this help us not stomp on it? */
  emit_insn (gen_rtx (USE, VOIDmode, saved_throw_type));
  emit_insn (gen_rtx (USE, VOIDmode, saved_throw_value));
  emit_jump (throw_label);
  emit_label (entry->exception_handler_label);
  expand_expr (entry->finalization, const0_rtx, VOIDmode, 0);
  emit_barrier ();
#endif
  emit_label (label);
}

/* call this to end processing of all the catch blocks. */
void
expand_end_all_catch ()
{
  rtx catchstart, catchend, last;
  rtx label;

  if (! doing_eh (1))
    return;

  /* Find the start of the catch block.  */
  last = pop_last_insn ();
  catchstart = NEXT_INSN (last);
  catchend = get_last_insn ();

  NEXT_INSN (last) = 0;
  set_last_insn (last);

  /* this level of catch blocks is done, so set up the successful catch jump
     label for the next layer of catch blocks. */
  pop_label_entry (&caught_return_label_stack);

  push_except_stmts (&exceptstack, catchstart, catchend);
  
  /* Here we fall through into the continuation code.  */
}


/* this is called from expand_exception_blocks () to expand the toplevel
   finalizations for a function. */
void
expand_leftover_cleanups ()
{
  struct ehEntry *entry;
  rtx first_label = NULL_RTX;

  if (! doing_eh (0))
    return;

  /* Will this help us not stomp on it? */
  emit_insn (gen_rtx (USE, VOIDmode, saved_throw_type));
  emit_insn (gen_rtx (USE, VOIDmode, saved_throw_value));

  while ((entry = dequeue_eh_entry (&ehqueue)) != 0)
    {
      if (! first_label)
	first_label = entry->exception_handler_label;
      emit_label (entry->exception_handler_label);

      expand_expr (entry->finalization, const0_rtx, VOIDmode, 0);

      /* leftover try block, opps.  */
      if (entry->finalization == integer_zero_node)
	abort ();

      free (entry);
    }
  if (first_label)
    {
      rtx label;
      struct ehEntry entry;
      /* These are saved for the exception table.  */
      push_rtl_perm ();
      label = gen_label_rtx ();
      entry.start_label = first_label;
      entry.end_label = label;
      entry.exception_handler_label = gen_label_rtx ();
      entry.finalization = TerminateFunctionCall;
      pop_rtl_from_perm ();
      emit_label (label);

      enqueue_eh_entry (&eh_table_output_queue, copy_eh_entry (&entry));

      /* After running the finalization, continue on out to the next
	 cleanup, if we have nothing better to do.  */
      emit_move_insn (saved_pc, gen_rtx (LABEL_REF, Pmode, entry.end_label));
      /* Will this help us not stomp on it? */
      emit_insn (gen_rtx (USE, VOIDmode, saved_throw_type));
      emit_insn (gen_rtx (USE, VOIDmode, saved_throw_value));
      emit_jump (throw_label);
      emit_label (entry.exception_handler_label);
      expand_expr (entry.finalization, const0_rtx, VOIDmode, 0);
      emit_barrier ();
    }
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
  tree type;
  tree decl;
  tree init;

  if (! doing_eh (1))
    return;

  /* Create a binding level for the parm.  */
  expand_start_bindings (0);

  if (declspecs)
    {
      tree init_type;
      decl = grokdeclarator (declarator, declspecs, NORMAL, 1, NULL_TREE);

      /* Figure out the type that the initializer is. */
      init_type = TREE_TYPE (decl);
      if (TREE_CODE (init_type) != REFERENCE_TYPE)
	init_type = build_reference_type (init_type);

      init = convert_from_reference (save_expr (make_tree (init_type, saved_throw_value)));
      
      /* Do we need the below two lines? */
      /* Let `finish_decl' know that this initializer is ok.  */
      DECL_INITIAL (decl) = init;
      /* This needs to be preallocated under the try block,
	 in a union of all catch variables. */
      pushdecl (decl);
      type = TREE_TYPE (decl);

      /* peel back references, so they match. */
      if (TREE_CODE (type) == REFERENCE_TYPE)
	type = TREE_TYPE (type);
    }
  else
    type = NULL_TREE;

  /* These are saved for the exception table.  */
  push_rtl_perm ();
  false_label_rtx = gen_label_rtx ();
  protect_label_rtx = gen_label_rtx ();
  pop_rtl_from_perm ();
  push_label_entry (&false_label_stack, false_label_rtx);
  push_label_entry (&false_label_stack, protect_label_rtx);

  if (type)
    {
      tree params;
      char *typestring;
      rtx call_rtx, return_value_rtx;
      tree catch_match_fcall;
      tree catchmatch_arg, argval;

      typestring = build_overload_name (type, 1, 1);

      params = tree_cons (NULL_TREE,
			 combine_strings (build_string (strlen (typestring)+1, typestring)),
			 tree_cons (NULL_TREE,
				    make_tree (ptr_type_node, saved_throw_type),
				    NULL_TREE));
      catch_match_fcall = build_function_call (CatchMatch, params);
      call_rtx = expand_call (catch_match_fcall, NULL_RTX, 0);

      return_value_rtx =
	hard_function_value (integer_type_node, catch_match_fcall);

      /* did the throw type match function return TRUE? */
      emit_cmp_insn (return_value_rtx, const0_rtx, NE, NULL_RTX,
		    GET_MODE (return_value_rtx), 0, 0);

      /* if it returned FALSE, jump over the catch block, else fall into it */
      emit_jump_insn (gen_bne (false_label_rtx));
      finish_decl (decl, init, NULL_TREE, 0);
    }
  else
    {
      /* Fall into the catch all section. */
    }

  /* This is the starting of something to protect.  */
  emit_label (protect_label_rtx);

  emit_line_note (input_filename, lineno);
}


/* Call this to end a catch block.  Its responsible for emitting the
   code to handle jumping back to the correct place, and for emitting
   the label to jump to if this catch block didn't match.  */
void expand_end_catch_block ()
{
  if (doing_eh (1))
    {
      rtx start_protect_label_rtx;
      rtx end_protect_label_rtx;
      tree decls;
      struct ehEntry entry;

      /* label we jump to if we caught the exception */
      emit_jump (top_label_entry (&caught_return_label_stack));

      /* Code to throw out to outer context, if we get an throw from within
	 our catch handler. */
      /* These are saved for the exception table.  */
      push_rtl_perm ();
      entry.exception_handler_label = gen_label_rtx ();
      pop_rtl_from_perm ();
      emit_label (entry.exception_handler_label);
      emit_move_insn (saved_pc, gen_rtx (LABEL_REF,
					 Pmode,
					 top_label_entry (&caught_return_label_stack)));
      emit_jump (throw_label);
      /* No associated finalization.  */
      entry.finalization = NULL_TREE;

      /* Because we are reordered out of line, we have to protect this. */
      /* label for the start of the protection region.  */
      start_protect_label_rtx = pop_label_entry (&false_label_stack);

      /* Cleanup the EH paramater.  */
      expand_end_bindings (decls = getdecls (), decls != NULL_TREE, 0);
      
      /* label we emit to jump to if this catch block didn't match. */
      emit_label (end_protect_label_rtx = pop_label_entry (&false_label_stack));

      /* Because we are reordered out of line, we have to protect this. */
      entry.start_label = start_protect_label_rtx;
      entry.end_label = end_protect_label_rtx;

      /* These set up a call to throw the caught exception into the outer
       context.  */
      enqueue_eh_entry (&eh_table_output_queue, copy_eh_entry (&entry));
    }
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


/* is called from expand_excpetion_blocks () to generate the code in a function
   to "throw" if anything in the function needs to preform a throw.

   expands "throw" as the following psuedo code:

	throw:
		eh = find_first_exception_match (saved_pc);
	    if (!eh) goto gotta_rethrow_it;
		goto eh;

	gotta_rethrow_it:
		saved_pc = __builtin_return_address (0);
		pop_to_previous_level ();
		goto throw;

 */
static void
expand_builtin_throw ()
{
  tree fcall;
  tree params;
  rtx return_val_rtx;
  rtx gotta_rethrow_it = gen_label_rtx ();
  rtx gotta_call_terminate = gen_label_rtx ();
  rtx unwind_and_throw = gen_label_rtx ();
  rtx goto_unwind_and_throw = gen_label_rtx ();

  emit_label (throw_label);

  /* search for an exception handler for the saved_pc */
  return_val_rtx = do_function_call (FirstExceptionMatch,
				     tree_cons (NULL_TREE, make_tree (ptr_type_node, saved_pc), NULL_TREE),
				     ptr_type_node);

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
  params=tree_cons (NULL_TREE, integer_zero_node, NULL_TREE);
  fcall = build_function_call (BuiltinReturnAddress, params);
  return_val_rtx = expand_expr (fcall, NULL_RTX, SImode, 0);

  /* did __builtin_return_address () return a valid address? */
  emit_cmp_insn (return_val_rtx, const0_rtx, EQ, NULL_RTX,
		 GET_MODE (return_val_rtx), 0, 0);

  emit_jump_insn (gen_beq (gotta_call_terminate));

  /* yes it did */
  emit_move_insn (saved_pc, return_val_rtx);
  do_unwind (throw_label);
  emit_jump (throw_label);

  /* no it didn't --> therefore we need to call terminate */
  emit_label (gotta_call_terminate);
  do_function_call (Terminate, NULL_TREE, NULL_TREE);
}


/* This is called to expand all the toplevel exception handling
   finalization for a function.  It should only be called once per
   function.  */
void
expand_exception_blocks ()
{
  rtx catchstart, catchend;
  rtx last;
  static rtx funcend;

  funcend = gen_label_rtx ();
  emit_jump (funcend);
  /* expand_null_return (); */

  while (pop_except_stmts (&exceptstack, &catchstart, &catchend)) {
    last = get_last_insn ();
    NEXT_INSN (last) = catchstart;
    PREV_INSN (catchstart) = last;
    NEXT_INSN (catchend) = 0;
    set_last_insn (catchend);
  }

  expand_leftover_cleanups ();

  {
    static int have_done = 0;
    if (! have_done && TREE_PUBLIC (current_function_decl)
	&& ! DECL_INLINE (current_function_decl))
      {
	have_done = 1;
	expand_builtin_throw ();
      }
  }
  emit_label (funcend);
}


/* call this to expand a throw statement.  This follows the following
   algorithm:

	1. Allocate space to save the current PC onto the stack.
	2. Generate and emit a label and save its address into the
		newly allocate stack space since we can't save the pc directly.
	3. If this is the first call to throw in this function:
		generate a label for the throw block
	4. jump to the throw block label.  */
void
expand_throw (exp)
     tree exp;
{
  rtx label;
  tree type;

  if (! doing_eh (1))
    return;

  /* This is the label that represents where in the code we were, when
     we got an exception.  This needs to be updated when we rethrow an
     exception, so that the matching routine knows to search out.  */
  label = gen_label_rtx ();
  emit_label (label);
  emit_move_insn (saved_pc, gen_rtx (LABEL_REF, Pmode, label));

  if (exp)
    {
      /* throw expression */
      /* First, decay it. */
      exp = default_conversion (exp);
      type = TREE_TYPE (exp);

      {
	char *typestring = build_overload_name (type, 1, 1);
	tree throw_type = build1 (ADDR_EXPR, ptr_type_node, combine_strings (build_string (strlen (typestring)+1, typestring)));
	rtx throw_type_rtx = expand_expr (throw_type, NULL_RTX, VOIDmode, 0);
	rtx throw_value_rtx;

	emit_move_insn (saved_throw_type, throw_type_rtx);
	exp = convert_to_reference (build_reference_type (build_type_variant (TREE_TYPE (exp), 1, 0)), exp, CONV_STATIC, LOOKUP_COMPLAIN, NULL_TREE);
	if (exp == error_mark_node)
	  error ("  in thrown expression");
	throw_value_rtx = expand_expr (build_unary_op (ADDR_EXPR, exp, 0), NULL_RTX, VOIDmode, 0);
	emit_move_insn (saved_throw_value, throw_value_rtx);
      }
    }
  else
    {
      /* rethrow current exception */
      /* This part is easy, as we dont' have to do anything else.  */
    }

  emit_jump (throw_label);
}

/* end of: my-cp-except.c */
#endif


/* Output the exception table.
 Return the number of handlers.  */
int
build_exception_table ()
{
  int count = 0;
#ifdef TRY_NEW_EH
  extern FILE *asm_out_file;
  struct ehEntry *entry;
  tree eh_node_decl;

  if (! doing_eh (0))
    return 0;

 while (entry = dequeue_eh_entry (&eh_table_output_queue))
   {
     if (count == 0)
       {
	 exception_section ();

	 /* Beginning marker for table. */
	 ASM_OUTPUT_ALIGN (asm_out_file, 2);
	 ASM_OUTPUT_LABEL (asm_out_file, "__EXCEPTION_TABLE__");
	 fprintf (asm_out_file, "        .word   0, 0, 0\n");
       }
     count++;
     output_exception_table_entry (asm_out_file,
				   entry->start_label, entry->end_label,
				   entry->exception_handler_label);
  }

  if (count)
    {
      /* Ending marker for table. */
      ASM_OUTPUT_LABEL (asm_out_file, "__EXCEPTION_END__");
      fprintf (asm_out_file, "        .word   -1, -1, -1\n");
    }

#endif /* TRY_NEW_EH */
  return count;
}

void
register_exception_table ()
{
#ifdef TRY_NEW_EH
  emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "__register_exceptions"), 0,
		     VOIDmode, 1,
		     gen_rtx (SYMBOL_REF, Pmode, "__EXCEPTION_TABLE__"),
		     Pmode);
#endif /* TRY_NEW_EH */
}

/* Build a throw expression.  */
tree
build_throw (e)
     tree e;
{
  e = build1 (THROW_EXPR, void_type_node, e);
  TREE_SIDE_EFFECTS (e) = 1;
  return e;
}
