/* Exception Handling interface routines.
   Copyright (C) 1996 Free Software Foundation, Inc.
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


#ifndef GET_CODE
#define rtx int *
#endif

#ifdef TREE_CODE

/* A stack of labels. CHAIN points to the next entry in the stack.  */

struct label_node {
  union {
    rtx rlabel;
    tree tlabel;
  } u;
  struct label_node *chain;
};

/* An eh_entry is used to describe one exception handling region.

   START_LABEL is the label corresponding to the start of the region.

   END_LABEL is the label corresponding to the end of the region.

   EXCEPTION_HANDLER_LABEL is the label corresponding to the handler
   for this region.

   FINALIZATION is the tree codes for the handler, or is NULL_TREE if
   one hasn't been generated yet, or is integer_zero_node to mark the
   end of a group of try blocks.  */

struct eh_entry {
  rtx start_label;
  rtx end_label;
  rtx exception_handler_label;

  tree finalization;
};

/* A list of EH_ENTRYs. ENTRY is the entry; CHAIN points to the next
   entry in the list, or is NULL if this is the last entry.  */

struct eh_node {
  struct eh_entry *entry;
  struct eh_node *chain;
};

/* A stack of EH_ENTRYs. TOP is the topmost entry on the stack. TOP is
   NULL if the stack is empty.  */

struct eh_stack {
  struct eh_node *top;
};

/* A queue of EH_ENTRYs. HEAD is the front of the queue; TAIL is the
   end (the latest entry). HEAD and TAIL are NULL if the queue is
   empty.  */

struct eh_queue {
  struct eh_node *head;
  struct eh_node *tail;
};


extern void expand_eh_region_start		PROTO((void));

extern void expand_eh_region_end		PROTO((tree));

/* Push RLABEL or TLABEL onto LABELSTACK. Only one of RLABEL or TLABEL
   should be set; the other must be NULL.  */

extern void push_label_entry			PROTO((struct label_node **labelstack, rtx rlabel, tree tlabel));

/* Pop the topmost entry from LABELSTACK and return its value as an
   rtx node. If LABELSTACK is empty, return NULL.  */

extern rtx pop_label_entry			PROTO((struct label_node **labelstack));

/* Return the topmost entry of LABELSTACK as a tree node, or return
   NULL_TREE if LABELSTACK is empty.  */

extern tree top_label_entry			PROTO((struct label_node **labelstack));

/* The stack used to keep track of the exception region corresponding to
   the current instruction.  */

extern struct eh_stack ehstack;

/* A queue used to track closed exception regions whose handlers have
   not been emitted yet.  */

extern struct eh_queue ehqueue;

/* A set of insns for the catch clauses in the current function. They
   will be emitted at the end of the current function.  */

extern rtx catch_clauses;

#endif

struct function;

/* Toplevel initialization for EH.  */

extern void init_eh				PROTO((void));

/* Initialization for the per-function EH data.  */

extern void init_eh_for_function		PROTO((void));

/* Saves the current per-function EH data into P.  */

extern void save_eh_status			PROTO((struct function *p));

/* Restores the per-function EH data from P.  */

extern void restore_eh_status			PROTO((struct function *p));

/* Adds an EH table entry for EH entry number N. Called from
   final_scan_insn for NOTE_INSN_EH_REGION_BEG.  */

extern void add_eh_table_entry			PROTO((int n));

/* Returns a non-zero value if we need to output an exception table.  */

extern int exception_table_p			PROTO((void));

/* Outputs the exception table if we have one.  */

extern void output_exception_table		PROTO((void));

/* Given a return address in ADDR, determine the address we should use
   to find the corresponding EH region.  */

extern rtx eh_outer_context			PROTO((rtx addr));

/* Called at the start of a block of try statements for which there is
   a supplied catch handler.  */

extern void expand_start_try_stmts 		PROTO((void));

/* Called at the start of a block of catch statements. It terminates the
   previous set of try statements.  */

extern void expand_start_all_catch		PROTO((void));

/* Called at the end of a block of catch statements.  */

extern void expand_end_all_catch		PROTO((void));

#ifdef TREE_CODE
/* Create a new exception region and add the handler for the region
   onto a list. These regions will be ended (and their handlers
   emitted) when end_protect_partials is invoked.  */

extern void add_partial_entry			PROTO((tree handler));
#endif

/* End all of the pending exception regions that have handlers added with
   push_protect_entry ().  */

extern void end_protect_partials		PROTO((void));

/* An internal throw with a direct CONTEXT we want to throw
   from. CONTEXT must be a label.  */

extern void expand_internal_throw		PROTO((rtx context));

/* Called from expand_exception_blocks and expand_end_catch_block to
   expand and pending handlers.  */

extern void expand_leftover_cleanups		PROTO((void));

/* If necessary, emit insns for the start of per-function unwinder for
   the current function.  */

extern void emit_unwinder			PROTO((void));

/* If necessary, emit insns for the end of the per-function unwinder
   for the current function.  */

extern void end_eh_unwinder			PROTO((void));

/* Builds a list of handler labels and puts them in the global
   variable exception_handler_labels.  */

extern void find_exception_handler_labels	PROTO((void));

/* Performs sanity checking on the check_exception_handler_labels
   list.  */

extern void check_exception_handler_labels	PROTO((void));

/* A stack used to keep track of the label used to resume normal program
   flow out of the current exception handler region.  */

extern struct label_node *caught_return_label_stack;

/* A random area used for purposes elsewhere.  */

extern struct label_node *false_label_stack;

/* A list of labels used for exception handlers. It is created by
   find_exception_handler_labels for the optimization passes.  */

extern rtx exception_handler_labels;

/* The rtx for the saved PC value.  */

extern rtx eh_saved_pc_rtx;

/* Performs optimizations for exception handling, such as removing
   unnecessary exception regions. Invoked from jump_optimize ().  */

extern void exception_optimize			PROTO((void));
