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

struct label_node {
  union {
    rtx rlabel;
    tree tlabel;
  } u;
  struct label_node *chain;
};

/* A entry for the exception handling stack EHSTACK or the exception
   handling queue EHQUEUE.  */
struct eh_entry {
  rtx start_label;
  rtx end_label;
  rtx exception_handler_label;

  tree finalization;
};

struct eh_node {
  struct eh_entry *entry;
  struct eh_node *chain;
};

struct eh_stack {
  struct eh_node *top;
};

struct eh_queue {
  struct eh_node *head;
  struct eh_node *tail;
};


extern void push_label_entry			PROTO((struct label_node **labelstack, rtx rlabel, tree tlabel));
extern rtx pop_label_entry			PROTO((struct label_node **labelstack));
extern tree top_label_entry			PROTO((struct label_node **labelstack));

extern struct eh_stack ehstack;
extern struct eh_queue ehqueue;
extern rtx catch_clauses;
extern tree protect_list;

#endif

struct function;

extern void init_eh				PROTO((void));
extern void init_eh_for_function		PROTO((void));
extern void save_eh_status			PROTO((struct function *p));
extern void restore_eh_status			PROTO((struct function *p));
extern void add_eh_table_entry			PROTO((int));
extern int exception_table_p			PROTO((void));
extern void output_exception_table		PROTO((void));
extern rtx eh_outer_context			PROTO((rtx));
extern void emit_unwinder			PROTO((void));
extern void end_eh_unwinder			PROTO((void));
extern void find_handler_labels			PROTO((void));
extern void check_handler_labels		PROTO((void));


extern struct label_node *caught_return_label_stack;
extern struct label_node *false_label_stack;

extern rtx exception_handler_labels;

/* The rtx for the saved PC value.  */

extern rtx eh_saved_pc_rtx;

extern void exception_optimize			PROTO((void));
