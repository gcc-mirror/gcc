/* Callgraph handling code.
   Copyright (C) 2003, 2004 Free Software Foundation, Inc.
   Contributed by Jan Hubicka

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#ifndef GCC_CGRAPH_H
#define GCC_CGRAPH_H

/* Information about the function collected locally.
   Available after function is analyzed.  */

struct cgraph_local_info GTY(())
{
  /* Size of the function before inlining.  */
  int self_insns;

  /* Set when function function is visible in current compilation unit only
     and it's address is never taken.  */
  bool local;
  /* Set once it has been finalized so we consider it to be output.  */
  bool finalized;

  /* False when there something makes inlining impossible (such as va_arg).  */
  bool inlinable;
  /* True when function should be inlined independently on it's size.  */
  bool disregard_inline_limits;
  /* True when the function has been originally extern inline, but it is
     redefined now.  */
  bool redefined_extern_inline;
};

/* Information about the function that needs to be computed globally
   once compilation is finished.  Available only with -funit-at-time.  */

struct cgraph_global_info GTY(())
{
  /* Estimated size of the function after inlining.  */
  int insns;

  /* Number of times given function will be cloned during output.  */
  int cloned_times;

  /* Set when the function will be inlined exactly once.  */
  bool inline_once;

  /* Set to true for all reachable functions before inlining is decided.
     Once we inline all calls to the function and the function is local,
     it is set to false.  */
  bool will_be_output;

  /* Set iff at least one of the caller edges has inline_call flag set.  */
  bool inlined;
};

/* Information about the function that is propagated by the RTL backend.
   Available only for functions that has been already assembled.  */

struct cgraph_rtl_info GTY(())
{
   bool const_function;
   bool pure_function;
   int preferred_incoming_stack_boundary;
};


/* The cgraph data structure.
   Each function decl has assigned cgraph_node listing callees and callers.  */

struct cgraph_node GTY((chain_next ("%h.next"), chain_prev ("%h.previous")))
{
  tree decl;
  struct cgraph_edge *callees;
  struct cgraph_edge *callers;
  struct cgraph_node *next;
  struct cgraph_node *previous;
  /* For nested functions points to function the node is nested in.  */
  struct cgraph_node *origin;
  /* Points to first nested function, if any.  */
  struct cgraph_node *nested;
  /* Pointer to the next function with same origin, if any.  */
  struct cgraph_node *next_nested;
  /* Pointer to the next function in cgraph_nodes_queue.  */
  struct cgraph_node *next_needed;
  PTR GTY ((skip (""))) aux;

  struct cgraph_local_info local;
  struct cgraph_global_info global;
  struct cgraph_rtl_info rtl;
  /* Unique id of the node.  */
  int uid;
  /* Set when function must be output - it is externally visible
     or it's address is taken.  */
  bool needed;
  /* Set when function is reachable by call from other function
     that is either reachable or needed.  */
  bool reachable;
  /* Set once the function has been instantiated and its callee
     lists created.  */
  bool analyzed;
  /* Set when function is scheduled to be assembled.  */
  bool output;
};

struct cgraph_edge GTY(())
{
  struct cgraph_node *caller;
  struct cgraph_node *callee;
  struct cgraph_edge *next_caller;
  struct cgraph_edge *next_callee;
  /* When NULL, inline this call.  When non-NULL, points to the explanation
     why function was not inlined.  */
  const char *inline_failed;
};

/* The cgraph_varpool data structure.
   Each static variable decl has assigned cgraph_varpool_node.  */

struct cgraph_varpool_node GTY(())
{
  tree decl;
  /* Pointer to the next function in cgraph_varpool_nodes_queue.  */
  struct cgraph_varpool_node *next_needed;

  /* Set when function must be output - it is externally visible
     or it's address is taken.  */
  bool needed;
  /* Set once it has been finalized so we consider it to be output.  */
  bool finalized;
  /* Set when function is scheduled to be assembled.  */
  bool output;
};

extern GTY(()) struct cgraph_node *cgraph_nodes;
extern GTY(()) int cgraph_n_nodes;
extern GTY(()) int cgraph_max_uid;
extern bool cgraph_global_info_ready;
extern GTY(()) struct cgraph_node *cgraph_nodes_queue;
extern FILE *cgraph_dump_file;

extern GTY(()) int cgraph_varpool_n_nodes;
extern GTY(()) struct cgraph_varpool_node *cgraph_varpool_nodes_queue;


/* In cgraph.c  */
void dump_cgraph (FILE *);
void cgraph_remove_edge (struct cgraph_node *, struct cgraph_node *);
void cgraph_remove_call (tree, tree);
void cgraph_remove_node (struct cgraph_node *);
struct cgraph_edge *cgraph_record_call (tree, tree);
struct cgraph_node *cgraph_node (tree decl);
struct cgraph_node *cgraph_node_for_identifier (tree id);
bool cgraph_calls_p (tree, tree);
struct cgraph_local_info *cgraph_local_info (tree);
struct cgraph_global_info *cgraph_global_info (tree);
struct cgraph_rtl_info *cgraph_rtl_info (tree);
const char * cgraph_node_name (struct cgraph_node *);

struct cgraph_varpool_node *cgraph_varpool_node (tree decl);
struct cgraph_varpool_node *cgraph_varpool_node_for_identifier (tree id);
void cgraph_varpool_mark_needed_node (struct cgraph_varpool_node *);
void cgraph_varpool_finalize_decl (tree);
bool cgraph_varpool_assemble_pending_decls (void);

bool cgraph_function_possibly_inlined_p (tree);

/* In cgraphunit.c  */
bool cgraph_assemble_pending_functions (void);
void cgraph_finalize_function (tree, bool);
void cgraph_finalize_compilation_unit (void);
void cgraph_create_edges (tree, tree);
void cgraph_optimize (void);
void cgraph_mark_needed_node (struct cgraph_node *);
void cgraph_mark_reachable_node (struct cgraph_node *);
bool cgraph_inline_p (tree, tree, const char **reason);

#endif  /* GCC_CGRAPH_H  */
