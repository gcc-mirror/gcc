/* Implements exceptiom handling.
   Copyright (C) 1989, 92-95, 1996 Free Software Foundation, Inc.
   Contributed by Mike Stump <mrs@cygnus.com>.

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


/* This file contains the exception handling code for the compiler.

   Exception handling is a mechanism by which complex flows of control
   can be designated.  The central concepts are the `exception region',
   the associated `exception handler' for that region and the concept of
   throwing an exception, and the context of the throw.

   Restrictions are, the regions must be non-overlapping, they can be
   nested, and there can be zero or more per function.  For each
   region, there is one associated handler.  Regions only ever
   surround possible context points of throws.  Regions with no such
   context points can be optimized away, as they are trivial, and it
   is not possible for the associated handler to ever be used during a
   throw.

   Semantics are, when an exception is thrown, control is transferred
   to a handler, and the code of the exception handler is executed.
   As control is transferred, the machine state (stack pointer, all
   callee saved registers and possibly the frame pointer) is restored.

   The handler that is selected by a throw, is the handler associated
   with the smallest (most nested) region that contains the context of
   the throw, if such a region exists.  If no region exists, the
   search for a handler continues in the function that called the
   function that contains the current context of the throw, with the
   context of the throw then becoming that point in the code that
   contains the call instruction.


   One can add to the basic model the concepts of thrown exception
   type, and thrown exception value.  Semantics are as above, except a
   further check is done when finding a handler for the thrown
   exception to see if the given handler can handle the thrown
   exception based upon the exception object's type and possibly its
   value.  A common optimization is when two regions are identical,
   the handlers are combined into just one handler so the first check
   of the resulting handler is for the inner (nested) region's
   handler, and the second one is for the outer region's handler.  To
   separate these two notions of handlers, we can call the subhandlers
   `catch blocks', and use the name `handler' to refer to the
   combination of the two.  Currently, this layer of functionality is
   managed by the various front ends.


   To mark the start of a exception handling region,
   expand_eh_region_start () is called.  To mark the end, and
   associate a handler for the region expand_eh_region_end () is used.
   The front end can use this interface, if useful.  The back end
   creates exception regions with these routines.  Another interface
   the front end can use, is TARGET_EXPR.  TARGET_EXPR gives an
   unwind-protect style interface a la emacs.


   In this implementation, regions do not span more than one function.

   In order to help with the task of finding the associated handler for
   a region, an exception table is built which associates handlers
   with regions.  A 3-tuple, containing a reference to the start, the
   end and the handler is sufficient for the exception table.

   In order to help with the task of restoring callee saved registers
   and performing other associated function exit actions, function
   `unwinders' can be generated within those function for which a
   generic function unwinder called __unwind_function () cannot work.
   Whether the generic __unwind_function can work is machine dependent
   and possibly function dependent.  The macro DOESNT_NEEED_UNWINDER
   decides if the current function being compiled needs an unwinder or
   not.

   The default is for unwinders to be used, as the default generic
   function unwinder only calls abort ().  The compiler-generated per
   function function unwinders simply modify the context of thrown
   exception to be that of the call site, and then arrange for control
   to be transferred to __throw instead of the function's caller on
   return, and then return.  */


#include "config.h"
#include <stdio.h>
#include "rtl.h"
#include "tree.h"
#include "flags.h"
#include "except.h"
#include "function.h"
#include "insn-flags.h"
#include "expr.h"
#include "insn-codes.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "insn-config.h"
#include "recog.h"
#include "output.h"

/* List of labels use for exception handlers.  Created by
   find_exception_handler_labels for the optimization passes.  */

rtx exception_handler_labels;

/* Nonzero means that throw was used.  Used for now, because __throw
   is emitted statically in each file.  */

int throw_used;

/* A stack used for keeping track of the currectly active exception
   handling region.  As exceptions regions are started, an entry
   describing the region is pushed onto this stack.  The current
   region can be found by looking at the top of the stack, and as we
   end regions, entries are poped.  */

struct eh_stack ehstack;

/* A queue used for tracking which exception regions have closed, but
   whose handlers have not yet been expanded.  As we end regions, we
   enqueue the entry onto this queue.  Entries are dequeue from the
   queue during expand_leftover_cleanups and expand_start_all_catch,
   and the handlers for regions are expanded in groups in an effort to
   group all the handlers together in the same region of program space
   to improve page performance.  We should redo things, so that we
   either take RTL for the handler, or we expand the handler expressed
   as a tree immediately at region end time.  */

struct eh_queue ehqueue;

/* Insns for the catch clauses.  */

rtx catch_clauses;

/* A list of actions for handlers for regions that are not yet
   closed.  */

tree protect_list;

/* Stacks to keep track of various labels.  */

/* Keeps track of the label to resume to, should one want to resume
   the normal control flow out of a handler.  Also used to rethrow
   exceptions caught in handlers, as if they were physically emitted
   inline.  */

struct label_node *caught_return_label_stack = NULL;

/* A spare data area for the front end's own use.  */

struct label_node *false_label_stack = NULL;

/* The rtx and the tree for the saved PC value.  */

rtx eh_saved_pc_rtx;
tree eh_saved_pc;

rtx expand_builtin_return_addr	PROTO((enum built_in_function, int, rtx));

/* Various support routines to manipulate the various data structures
   used by the exception handling code.  */

/* Push a label entry onto the given STACK.  */

void
push_label_entry (stack, rlabel, tlabel)
     struct label_node **stack;
     rtx rlabel;
     tree tlabel;
{
  struct label_node *newnode
    = (struct label_node *) xmalloc (sizeof (struct label_node));

  if (rlabel)
    newnode->u.rlabel = rlabel;
  else
    newnode->u.tlabel = tlabel;
  newnode->chain = *stack;
  *stack = newnode;
}

/* Pop a label entry from the given STACK.  */

rtx
pop_label_entry (stack)
     struct label_node **stack;
{
  rtx label;
  struct label_node *tempnode;

  if (! *stack)
    return NULL_RTX;

  tempnode = *stack;
  label = tempnode->u.rlabel;
  *stack = (*stack)->chain;
  free (tempnode);

  return label;
}

/* Return the top element of the given STACK.  */

tree
top_label_entry (stack)
     struct label_node **stack;
{
  if (! *stack)
    return NULL_TREE;

  return (*stack)->u.tlabel;
}

/* Copy an entry.  */

static struct eh_entry *
copy_eh_entry (entry)
     struct eh_entry *entry;
{
  struct eh_entry *newentry;

  newentry = (struct eh_entry *) xmalloc (sizeof (struct eh_entry));
  bcopy ((char *) entry, (char *) newentry, sizeof (struct eh_entry));

  return newentry;
}

/* Push an entry onto the given STACK.  */

static rtx
push_eh_entry (stack)
     struct eh_stack *stack;
{
  struct eh_node *node = (struct eh_node *) xmalloc (sizeof (struct eh_node));
  struct eh_entry *entry = (struct eh_entry *) xmalloc (sizeof (struct eh_entry));

  entry->start_label = gen_label_rtx ();
  entry->end_label = gen_label_rtx ();
  entry->exception_handler_label = gen_label_rtx ();
  entry->finalization = NULL_TREE;

  node->entry = entry;
  node->chain = stack->top;
  stack->top = node;

  return entry->start_label;
}

/* Pop an entry from the given STACK.  */

static struct eh_entry *
pop_eh_entry (stack)
     struct eh_stack *stack;
{
  struct eh_node *tempnode;
  struct eh_entry *tempentry;
  
  tempnode = stack->top;
  tempentry = tempnode->entry;
  stack->top = stack->top->chain;
  free (tempnode);

  return tempentry;
}

/* Enqueue an ENTRY onto the given QUEUE.  */

static void
enqueue_eh_entry (queue, entry)
     struct eh_queue *queue;
     struct eh_entry *entry;
{
  struct eh_node *node = (struct eh_node *) xmalloc (sizeof (struct eh_node));

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

/* Dequeue an entry from the given QUEUE.  */

static struct eh_entry *
dequeue_eh_entry (queue)
     struct eh_queue *queue;
{
  struct eh_node *tempnode;
  struct eh_entry *tempentry;

  if (queue->head == NULL)
    return NULL;

  tempnode = queue->head;
  queue->head = queue->head->chain;

  tempentry = tempnode->entry;
  free (tempnode);

  return tempentry;
}

/* Routine to see if exception exception handling is turned on.
   DO_WARN is non-zero if we want to inform the user that exception
   handling is turned off.  */

int
doing_eh (do_warn)
     int do_warn;
{
  if (! flag_exceptions)
    {
      static int warned = 0;
      if (! warned && do_warn)
	{
	  error ("exception handling disabled, use -fexceptions to enable");
	  warned = 1;
	}
      return 0;
    }
  return 1;
}

/* Given the return address in ADDR, compute the new pc to throw.
   This has to work for the current frame of the current function, and
   the one above it in the case of throw.  */

rtx
eh_outer_context (addr)
     rtx addr;
{
  /* First mask out any unwanted bits.  */
#ifdef MASK_RETURN_ADDR
  emit_insn (gen_rtx (SET, Pmode,
		      addr,
		      gen_rtx (AND, Pmode,
			       addr, MASK_RETURN_ADDR)));
#endif

  /* Then subtract out enough to get into the prior region.  If this
     is defined, assume we don't need to subtract anything, as it is
     already within the region.  */
#if ! defined (RETURN_ADDR_OFFSET)
  addr = plus_constant (addr, -1);
#endif

  return addr;
}

/* Output a note marking the start of an exception handling region.  */

void
expand_eh_region_start ()
{
  rtx note;

  /* This is the old code.  */
  if (! doing_eh (0))
    return;

#if 0
  /* Maybe do this to prevent jumping in and so on...  */
  pushlevel (0);
#endif

  note = emit_note (NULL_PTR, NOTE_INSN_EH_REGION_BEG);
  emit_label (push_eh_entry (&ehstack));
  NOTE_BLOCK_NUMBER (note)
    = CODE_LABEL_NUMBER (ehstack.top->entry->exception_handler_label);
}

/* Output a note marking the end of an exception handling region.
   HANDLER is the the handler for the exception region.  */

void
expand_eh_region_end (handler)
     tree handler;
{
  rtx note;

  struct eh_entry *entry;

  if (! doing_eh (0))
    return;

  entry = pop_eh_entry (&ehstack);

  note = emit_note (NULL_PTR, NOTE_INSN_EH_REGION_END);
  NOTE_BLOCK_NUMBER (note) = CODE_LABEL_NUMBER (entry->exception_handler_label);

  emit_label (entry->end_label);

  /* Put in something that takes up space, as otherwise the end
     address for the EH region could have the exact same address as
     the outer region, causing us to miss the fact that resuming
     exception handling with this PC value would be inside the outer
     region.  */
  emit_insn (gen_nop ());

  entry->finalization = handler;

  enqueue_eh_entry (&ehqueue, entry);


#if 0
  /* Makebe do this to prevent jumping in and so on...  */
  poplevel (1, 0, 0);
#endif
}

/* Emit a call to __throw and note that we threw something.  */

static void
emit_throw ()
{
#ifdef JUMP_TO_THROW
  emit_indirect_jump (throw_libfunc);
#else
  SYMBOL_REF_USED (throw_libfunc) = 1;
  emit_library_call (throw_libfunc, 0, VOIDmode, 0);
#endif
  throw_used = 1;
  emit_barrier ();
}

/* An internal throw with an indirect CONTEXT we want to throw from.  */

void
expand_internal_throw_indirect (context)
     rtx context;
{
  assemble_external (eh_saved_pc);
  emit_move_insn (eh_saved_pc_rtx, context);
  emit_throw ();
}

/* An internal throw with a direct CONTEXT we want to throw from.  The
   context should be a label.  */

void
expand_internal_throw (context)
     rtx context;
{
  expand_internal_throw_indirect (gen_rtx (LABEL_REF, Pmode, context));
}

/* Called from expand_exception_blocks and expand_end_catch_block to
   expand any pending handlers.  */

void
expand_leftover_cleanups ()
{
  struct eh_entry *entry;

  while ((entry = dequeue_eh_entry (&ehqueue)) != 0)
    {
      rtx prev;

      emit_label (entry->exception_handler_label);

      expand_expr (entry->finalization, const0_rtx, VOIDmode, 0);

      prev = get_last_insn ();
      if (! (prev && GET_CODE (prev) == BARRIER))
	{
	  /* The below can be optimized away, and we could just fall into the
	     next EH handler, if we are certain they are nested.  */
	  /* Code to throw out to outer context, if we fall off end of the
	     handler.  */
	  expand_internal_throw (entry->end_label);
	}

      /* leftover try block, opps.  */
      if (entry->finalization == integer_zero_node)
	abort ();

      free (entry);
    }
}

/* Generate RTL for the start of all the catch blocks.  Used for
   arranging for the exception handling code to be placed farther out
   of line than normal.  */

void
expand_start_all_catch ()
{
  struct eh_entry *entry;
  tree label;

  if (! doing_eh (1))
    return;

  emit_line_note (input_filename, lineno);
  label = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);

  /* The label for the exception handling block we will save.  This is
     Lresume, in the documention.  */
  expand_label (label);
  
  /* Put in something that takes up space, as otherwise the end
     address for the EH region could have the exact same address as
     the outer region, causing us to miss the fact that resuming
     exception handling with this PC value would be inside the outer
     region.  */
  emit_insn (gen_nop ());

  push_label_entry (&caught_return_label_stack, NULL_RTX, label);

  /* Start a new sequence for all the catch blocks.  We will add this
     to the gloabl sequence catch_clauses, when we have completed all
     the handlers in this handler-seq.  */
  start_sequence ();

  while (1)
    {
      rtx prev;

      entry = dequeue_eh_entry (&ehqueue);
      emit_label (entry->exception_handler_label);

      expand_expr (entry->finalization, const0_rtx, VOIDmode, 0);

      /* When we get down to the matching entry, stop.  */
      if (entry->finalization == integer_zero_node)
	break;

      prev = get_last_insn ();
      if (! (prev && GET_CODE (prev) == BARRIER))
	{
	  /* The below can be optimized away, and we could just fall into the
	     next EH handler, if we are certain they are nested.  */
	  /* Code to throw out to outer context, if we fall off end of the
	     handler.  */
	  expand_internal_throw (entry->end_label);
	}

      free (entry);
    }
}

/* Generate RTL for the end of all the catch blocks.  */

void
expand_end_all_catch ()
{
  rtx new_catch_clause;

  if (! doing_eh (1))
    return;

  /* Code to throw out to outer context, if we fall off end of catch
     handlers.  This is rethrow (Lresume, same id, same obj); in the
     documentation.  */
  expand_internal_throw (DECL_RTL (top_label_entry (&caught_return_label_stack)));

  /* Now we have the complete catch sequence.  */
  new_catch_clause = get_insns ();
  end_sequence ();
  
  /* This level of catch blocks is done, so set up the successful
     catch jump label for the next layer of catch blocks.  */
  pop_label_entry (&caught_return_label_stack);

  /* Add the new sequence of catches to the main one for this function.  */
  push_to_sequence (catch_clauses);
  emit_insns (new_catch_clause);
  catch_clauses = get_insns ();
  end_sequence ();
  
  /* Here we fall through into the continuation code.  */
}

/* End all the pending exception regions from protect_list that have
   been started, but not yet completed.  */

void
end_protect_partials ()
{
  while (protect_list)
    {
      expand_eh_region_end (TREE_VALUE (protect_list));
      protect_list = TREE_CHAIN (protect_list);
    }
}

/* The exception table that we build that is used for looking up and
   dispatching exceptions, it's size, and it's maximum size before we
   have to extend it.  */
static int *eh_table;
static int eh_table_size;
static int eh_table_max_size;

/* Note the need for an exception table entry for region N.  If we
   don't need to output an explicit exception table, avoid all the
   extra work.  Called during final_scan_insn time.  */

void
add_eh_table_entry (n)
     int n;
{
#ifndef OMIT_EH_TABLE
  if (eh_table_size >= eh_table_max_size)
    {
      if (eh_table)
	{
	  eh_table_max_size += eh_table_max_size>>1;

	  if (eh_table_max_size < 0)
	    abort ();

	  if ((eh_table = (int *) realloc (eh_table, eh_table_max_size))
	      == 0)
	    fatal ("virtual memory exhausted");
	}
      else
	{
	  eh_table_max_size = 252;
	  eh_table = (int *) xmalloc (eh_table_max_size * sizeof (int));
	}
    }
  eh_table[eh_table_size++] = n;
#endif
}

/* Conditional to test to see if we need to output an exception table.
   Note, on some platforms, we don't have to output a table
   explicitly.  This routine doesn't mean we don't have one.  */

int
exception_table_p ()
{
  if (eh_table)
    return 1;

  return 0;
}

/* Output an entry N for the exception table to the specified FILE.  */

static void
output_exception_table_entry (file, n)
     FILE *file;
     int n;
{
  char buf[256];
  rtx sym;

  ASM_GENERATE_INTERNAL_LABEL (buf, "LEHB", n);
  sym = gen_rtx (SYMBOL_REF, Pmode, buf);
  assemble_integer (sym, POINTER_SIZE / BITS_PER_UNIT, 1);

  ASM_GENERATE_INTERNAL_LABEL (buf, "LEHE", n);
  sym = gen_rtx (SYMBOL_REF, Pmode, buf);
  assemble_integer (sym, POINTER_SIZE / BITS_PER_UNIT, 1);

  ASM_GENERATE_INTERNAL_LABEL (buf, "L", n);
  sym = gen_rtx (SYMBOL_REF, Pmode, buf);
  assemble_integer (sym, POINTER_SIZE / BITS_PER_UNIT, 1);

  putc ('\n', file);		/* blank line */
}

/* Output the exception table if we have one and need one.  */

void
output_exception_table ()
{
  int i;
  extern FILE *asm_out_file;

  if (! doing_eh (0))
    return;

  exception_section ();

  /* Beginning marker for table.  */
  assemble_align (GET_MODE_ALIGNMENT (ptr_mode));
  assemble_label ("__EXCEPTION_TABLE__");

  assemble_integer (const0_rtx, POINTER_SIZE / BITS_PER_UNIT, 1);
  assemble_integer (const0_rtx, POINTER_SIZE / BITS_PER_UNIT, 1);
  assemble_integer (const0_rtx, POINTER_SIZE / BITS_PER_UNIT, 1);
  putc ('\n', asm_out_file);		/* blank line */

  for (i = 0; i < eh_table_size; ++i)
    output_exception_table_entry (asm_out_file, eh_table[i]);

  free (eh_table);

  /* Ending marker for table.  */
  assemble_label ("__EXCEPTION_END__");
  assemble_integer (constm1_rtx, POINTER_SIZE / BITS_PER_UNIT, 1);
  assemble_integer (constm1_rtx, POINTER_SIZE / BITS_PER_UNIT, 1);
  assemble_integer (constm1_rtx, POINTER_SIZE / BITS_PER_UNIT, 1);
  putc ('\n', asm_out_file);		/* blank line */
}

/* Generate code to initialize the exception table at program startup
   time.  */

void
register_exception_table ()
{
  emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "__register_exceptions"), 0,
		     VOIDmode, 1,
		     gen_rtx (SYMBOL_REF, Pmode, "__EXCEPTION_TABLE__"),
		     Pmode);
}

/* Emit the RTL for the start of the per function unwinder for the
   current function.  */

void
start_eh_unwinder ()
{
#ifdef DOESNT_NEED_UNWINDER
  if (DOESNT_NEED_UNWINDER)
    return;
#endif

  expand_eh_region_start ();
}

/* Emit the RTL for the end of the per function unwinder for the
   current function.  */

void
end_eh_unwinder ()
{
  tree expr;
  rtx return_val_rtx, ret_val, label, end, insns;

  if (! doing_eh (0))
    return;

#ifdef DOESNT_NEED_UNWINDER
  if (DOESNT_NEED_UNWINDER)
    return;
#endif

  assemble_external (eh_saved_pc);

  expr = make_node (RTL_EXPR);
  TREE_TYPE (expr) = void_type_node;
  RTL_EXPR_RTL (expr) = const0_rtx;
  TREE_SIDE_EFFECTS (expr) = 1;
  start_sequence_for_rtl_expr (expr);

  ret_val = expand_builtin_return_addr (BUILT_IN_RETURN_ADDRESS,
					0, hard_frame_pointer_rtx);
  return_val_rtx = copy_to_reg (ret_val);

  return_val_rtx = eh_outer_context (return_val_rtx);

  emit_move_insn (eh_saved_pc_rtx, return_val_rtx);
  
#ifdef JUMP_TO_THROW
  emit_move_insn (ret_val, throw_libfunc);
#else
  label = gen_label_rtx ();
  emit_move_insn (ret_val, gen_rtx (LABEL_REF, Pmode, label));
#endif

#ifdef RETURN_ADDR_OFFSET
  return_val_rtx = plus_constant (ret_val, -RETURN_ADDR_OFFSET);
  if (return_val_rtx != ret_val)
    emit_move_insn (ret_val, return_val_rtx);
#endif
  
  end = gen_label_rtx ();
  emit_jump (end);  

  RTL_EXPR_SEQUENCE (expr) = get_insns ();
  end_sequence ();
  expand_eh_region_end (expr);

  emit_jump (end);

#ifndef JUMP_TO_THROW
  emit_label (label);
  emit_throw ();
#endif
  
  expand_leftover_cleanups ();

  emit_label (end);
}

/* Emit the RTL for the per function unwinder for the current
   function, if needed.  Called after all the code that needs unwind
   protection is output.  */

void
emit_unwinder ()
{
  rtx insns;

  start_sequence ();
  start_eh_unwinder ();
  insns = get_insns ();
  end_sequence ();

  if (insns)
    emit_insns_after (insns, get_insns ());

  end_eh_unwinder ();
}

/* Scan the current insns and build a list of handler labels.  Called
   after the last exception handling region is added to the current
   function (when the rtl is almost all built for the current
   function) and before the jump optimization pass.  */

void
find_exception_handler_labels ()
{
  rtx insn;
  int max_labelno = max_label_num ();
  int min_labelno = get_first_label_num ();
  rtx *labels;

  exception_handler_labels = NULL_RTX;

  /* If we aren't doing exception handling, there isn't much to check.  */
  if (! doing_eh (0))
    return;

  /* First we generate a handy reference to each label.  */

  labels = (rtx *) alloca ((max_labelno - min_labelno) * sizeof (rtx));
  labels -= min_labelno;

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == CODE_LABEL)
	if (CODE_LABEL_NUMBER (insn) >= min_labelno
	    && CODE_LABEL_NUMBER (insn) < max_labelno)
	  labels[CODE_LABEL_NUMBER (insn)] = insn;
    }

  /* Then for each start of a region, we add its label to the list.  */
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == NOTE
	  && NOTE_LINE_NUMBER (insn) == NOTE_INSN_EH_REGION_BEG)
	{
	  rtx label = NULL_RTX;

	  if (NOTE_BLOCK_NUMBER (insn) >= min_labelno
	      && NOTE_BLOCK_NUMBER (insn) < max_labelno)
	    {
	      label = labels[NOTE_BLOCK_NUMBER (insn)];

	      if (label)
		exception_handler_labels
		  = gen_rtx (EXPR_LIST, VOIDmode,
			     label, exception_handler_labels);
	      else
		warning ("didn't find handler for EH region %d",
			 NOTE_BLOCK_NUMBER (insn));
	    }
	  else
	    warning ("mismatched EH region %d", NOTE_BLOCK_NUMBER (insn));
	}
    }
}

/* Do some sanity checking on the exception_handler_labels list.  Can
   be called after find_exception_handler_labels is called to build
   the list of exception handlers for the current function, and before
   we finish processing the current function.  */

void
check_exception_handler_labels ()
{
  rtx insn, handler;

  /* If we aren't doing exception handling, there isn't much to check.  */
  if (! doing_eh (0))
    return;

  for (handler = exception_handler_labels;
       handler;
       handler = XEXP (handler, 1))
    {
      for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
	{
	  if (GET_CODE (insn) == CODE_LABEL)
	    {
	      if (CODE_LABEL_NUMBER (insn)
		  == CODE_LABEL_NUMBER (XEXP (handler, 0)))
		{
		  if (insn != XEXP (handler, 0))
		    warning ("mismatched handler %d",
			     CODE_LABEL_NUMBER (insn));
		  break;
		}
	    }
	}
      if (insn == NULL_RTX)
	warning ("handler not found %d",
		 CODE_LABEL_NUMBER (XEXP (handler, 0)));
    }

  /* Now go through, and make sure that for each region we have, that we
     have the corresponding label.  */
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == NOTE
	  && (NOTE_LINE_NUMBER (insn) == NOTE_INSN_EH_REGION_BEG ||
	      NOTE_LINE_NUMBER (insn) == NOTE_INSN_EH_REGION_END))
	{
	  for (handler = exception_handler_labels;
	       handler;
	       handler = XEXP (handler, 1))
	    {
	      if (CODE_LABEL_NUMBER (XEXP (handler, 0))
		  == NOTE_BLOCK_NUMBER (insn))
		break;
	    }
	  if (handler == NULL_RTX)
	    warning ("region exists, no handler %d",
		     NOTE_BLOCK_NUMBER (insn));
	}
    }
}

/* This group of functions initializes the exception handling data
   structures at the start of the compilation, initializes the data
   structures at the start of a function, saves and restores the
   exception handling data structures for the start/end of a nested
   function.  */

/* Toplevel initialization for EH things.  */ 

void
init_eh ()
{
  tree type = build_pointer_type (make_node (VOID_TYPE));

  eh_saved_pc = build_decl (VAR_DECL, get_identifier ("__eh_pc"), type);
  DECL_EXTERNAL (eh_saved_pc) = 1;
  TREE_PUBLIC (eh_saved_pc) = 1;
  make_decl_rtl (eh_saved_pc, NULL_PTR, 1);
  eh_saved_pc_rtx = DECL_RTL (eh_saved_pc);
}

/* Initialize various EH things.  */

void
init_eh_for_function ()
{
  ehstack.top = 0;
  ehqueue.head = ehqueue.tail = 0;
  catch_clauses = NULL_RTX;
  false_label_stack = 0;
  caught_return_label_stack = 0;
  protect_list = NULL_TREE;
}

/* Save various EH things for the current function into the save area
   denoted by P.  */

void
save_eh_status (p)
     struct function *p;
{
  p->ehstack = ehstack;
  p->ehqueue = ehqueue;
  p->catch_clauses = catch_clauses;
  p->false_label_stack = false_label_stack;
  p->caught_return_label_stack = caught_return_label_stack;
  p->protect_list = protect_list;

  init_eh ();
}

/* Restore various EH things for the current function from the save
   area denoted by P.  */

void
restore_eh_status (p)
     struct function *p;
{
  protect_list = p->protect_list;
  caught_return_label_stack = p->caught_return_label_stack;
  false_label_stack = p->false_label_stack;
  catch_clauses	= p->catch_clauses;
  ehqueue = p->ehqueue;
  ehstack = p->ehstack;
}

/* This section is for the exception handling specific optimization
   pass.  First are the internal routines, and then the main
   optimization pass.  */

/* Determine if the given INSN can throw an exception.  */

static int
can_throw (insn)
     rtx insn;
{
  /* The only things that can possibly throw are calls.  */
  if (GET_CODE (insn) == CALL_INSN)
    return 1;

#ifdef ASYNCH_EXCEPTIONS
  /* If we wanted asynchronous exceptions, then everything but NOTEs
     and CODE_LABELs could throw.  */
  if (GET_CODE (insn) != NOTE && GET_CODE (insn) != CODE_LABEL)
    return 1;
#endif

  return 0;
}

/* Scan a region, looking for a matching end, and decide if the region
   can be removed.  INSN is the start of the region, N is the region
   number, and DELETE_OUTER is to note if anything in this region can
   throw.  */

static rtx
scan_region (insn, n, delete_outer)
     rtx insn;
     int n;
     int *delete_outer;
{
  rtx start = insn;

  /* Assume we can delete the region.  */
  int delete = 1;

  insn = NEXT_INSN (insn);

  /* Look for the matching end.  */
  while (! (GET_CODE (insn) == NOTE
	    && NOTE_LINE_NUMBER (insn) == NOTE_INSN_EH_REGION_END))
    {
      /* If anything can throw, we can't remove the region.  */
      if (delete && can_throw (insn))
	{
	  delete = 0;
	}

      /* Watch out for and handle nested regions.  */
      if (GET_CODE (insn) == NOTE
	  && NOTE_LINE_NUMBER (insn) == NOTE_INSN_EH_REGION_BEG)
	{
	  insn = scan_region (insn, NOTE_BLOCK_NUMBER (insn), &delete);
	}

      insn = NEXT_INSN (insn);
    }

  /* The _BEG/_END NOTEs must match and nest.  */
  if (NOTE_BLOCK_NUMBER (insn) != n)
    abort ();

  /* If anything can throw, we can throw.  */
  if (! delete)
    *delete_outer = 0;
  else
    {
      /* Delete the start and end of the region.  */
      delete_insn (start);
      delete_insn (insn);

      /* Only do this part if we have built the exception handler
         labels.  */
      if (exception_handler_labels)
	{
	  rtx x, *prev = &exception_handler_labels;

	  /* Find it in the list of handlers.  */
	  for (x = exception_handler_labels; x; x = XEXP (x, 1))
	    {
	      rtx label = XEXP (x, 0);
	      if (CODE_LABEL_NUMBER (label) == n)
		{
		  /* If we are the last reference to the handler,
                     delete it.  */
		  if (--LABEL_NUSES (label) == 0)
		    delete_insn (label);

		  if (optimize)
		    {
		      /* Remove it from the list of exception handler
			 labels, if we are optimizing.  If we are not, then
			 leave it in the list, as we are not really going to
			 remove the region.  */
		      *prev = XEXP (x, 1);
		      XEXP (x, 1) = 0;
		      XEXP (x, 0) = 0;
		    }

		  break;
		}
	      prev = &XEXP (x, 1);
	    }
	}
    }
  return insn;
}

/* Perform various interesting optimizations for exception handling
   code.

   We find empty exception regions, and remove them.  The jump
   optimization code will remove the handler if nothing else uses it.  */

void
exception_optimize ()
{
  rtx insn, regions = NULL_RTX;
  int n;

  /* First remove empty regions.  */
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == NOTE
	  && NOTE_LINE_NUMBER (insn) == NOTE_INSN_EH_REGION_BEG)
	{
	  insn = scan_region (insn, NOTE_BLOCK_NUMBER (insn), &n);
	}
    }
}
