/* Callgraph handling code.
   Copyright (C) 2003, 2004, 2005 Free Software Foundation, Inc.
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
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

#ifndef GCC_CGRAPH_H
#define GCC_CGRAPH_H
#include "tree.h"
#include "basic-block.h"

enum availability
{
  /* Not yet set by cgraph_function_body_availability.  */
  AVAIL_UNSET,
  /* Function body/variable initializer is unknown.  */
  AVAIL_NOT_AVAILABLE,
  /* Function body/variable initializer is known but might be replaced
     by a different one from other compilation unit and thus needs to
     be dealt with a care.  Like AVAIL_NOT_AVAILABLE it can have
     arbitrary side effects on escaping variables and functions, while
     like AVAILABLE it might access static variables.  */
  AVAIL_OVERWRITABLE,
  /* Function body/variable initializer is known and will be used in final
     program.  */
  AVAIL_AVAILABLE,
  /* Function body/variable initializer is known and all it's uses are explicitly
     visible within current unit (ie it's address is never taken and it is not
     exported to other units).
     Currently used only for functions.  */
  AVAIL_LOCAL
};

/* Information about the function collected locally.
   Available after function is analyzed.  */

struct cgraph_local_info GTY(())
{
  /* Size of the function before inlining.  */
  int self_insns;

  /* Set when function function is visible in current compilation unit only
     and its address is never taken.  */
  bool local;

  /* Set when function is visible by other units.  */
  bool externally_visible;

  /* Set once it has been finalized so we consider it to be output.  */
  bool finalized;

  /* False when there something makes inlining impossible (such as va_arg).  */
  bool inlinable;

  /* True when function should be inlined independently on its size.  */
  bool disregard_inline_limits;

  /* True when the function has been originally extern inline, but it is
     redefined now.  */
  bool redefined_extern_inline;

  /* True if statics_read_for_function and
     statics_written_for_function contain valid data.  */
  bool for_functions_valid;

  /* True if the function is going to be emitted in some other translation
     unit, referenced from vtable.  */
  bool vtable_method;
};

/* Information about the function that needs to be computed globally
   once compilation is finished.  Available only with -funit-at-time.  */

struct cgraph_global_info GTY(())
{
  /* For inline clones this points to the function they will be inlined into.  */
  struct cgraph_node *inlined_to;

  /* Estimated size of the function after inlining.  */
  int insns;

  /* Estimated growth after inlining.  INT_MIN if not computed.  */
  int estimated_growth;

  /* Set iff the function has been inlined at least once.  */
  bool inlined;
};

/* Information about the function that is propagated by the RTL backend.
   Available only for functions that has been already assembled.  */

struct cgraph_rtl_info GTY(())
{
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
  /* Pointer to the next clone.  */
  struct cgraph_node *next_clone;
  struct cgraph_node *prev_clone;
  /* Pointer to a single unique cgraph node for this function.  If the
     function is to be output, this is the copy that will survive.  */
  struct cgraph_node *master_clone;
 
  PTR GTY ((skip)) aux;

  struct cgraph_local_info local;
  struct cgraph_global_info global;
  struct cgraph_rtl_info rtl;
  
  /* Expected number of executions: calculated in profile.c.  */
  gcov_type count;
  /* Unique id of the node.  */
  int uid;
  /* Set when function must be output - it is externally visible
     or its address is taken.  */
  bool needed;
  /* Set when function is reachable by call from other function
     that is either reachable or needed.  */
  bool reachable;
  /* Set once the function is lowered (ie it's CFG is built).  */
  bool lowered;
  /* Set once the function has been instantiated and its callee
     lists created.  */
  bool analyzed;
  /* Set when function is scheduled to be assembled.  */
  bool output;
  /* Set when function is visible by other units.  */
  bool externally_visible;
  /* Set for aliases once they got through assemble_alias.  */
  bool alias;
};

struct cgraph_edge GTY((chain_next ("%h.next_caller"), chain_prev ("%h.prev_caller")))
{
  struct cgraph_node *caller;
  struct cgraph_node *callee;
  struct cgraph_edge *prev_caller;
  struct cgraph_edge *next_caller;
  struct cgraph_edge *prev_callee;
  struct cgraph_edge *next_callee;
  tree call_stmt;
  PTR GTY ((skip (""))) aux;
  /* When NULL, inline this call.  When non-NULL, points to the explanation
     why function was not inlined.  */
  const char *inline_failed;
  /* Expected number of executions: calculated in profile.c.  */
  gcov_type count;
  /* Depth of loop nest, 1 means no loop nest.  */
  int loop_nest;
};

/* The cgraph_varpool data structure.
   Each static variable decl has assigned cgraph_varpool_node.  */

struct cgraph_varpool_node GTY(())
{
  tree decl;
  /* Pointer to the next function in cgraph_varpool_nodes.  */
  struct cgraph_varpool_node *next;
  /* Pointer to the next function in cgraph_varpool_nodes_queue.  */
  struct cgraph_varpool_node *next_needed;

  /* Set when function must be output - it is externally visible
     or its address is taken.  */
  bool needed;
  /* Needed variables might become dead by optimization.  This flag
     forces the variable to be output even if it appears dead otherwise.  */
  bool force_output;
  /* Set once the variable has been instantiated and its callee
     lists created.  */
  bool analyzed;
  /* Set once it has been finalized so we consider it to be output.  */
  bool finalized;
  /* Set when variable is scheduled to be assembled.  */
  bool output;
  /* Set when function is visible by other units.  */
  bool externally_visible;
  /* Set for aliases once they got through assemble_alias.  */
  bool alias;
};

extern GTY(()) struct cgraph_node *cgraph_nodes;
extern GTY(()) int cgraph_n_nodes;
extern GTY(()) int cgraph_max_uid;
extern bool cgraph_global_info_ready;
extern bool cgraph_function_flags_ready;
extern GTY(()) struct cgraph_node *cgraph_nodes_queue;

extern GTY(()) struct cgraph_varpool_node *cgraph_varpool_first_unanalyzed_node;
extern GTY(()) struct cgraph_varpool_node *cgraph_varpool_nodes_queue;
extern GTY(()) struct cgraph_varpool_node *cgraph_varpool_nodes;

/* In cgraph.c  */
void dump_cgraph (FILE *);
void dump_cgraph_node (FILE *, struct cgraph_node *);
void dump_varpool (FILE *);
void dump_cgraph_varpool_node (FILE *, struct cgraph_varpool_node *);
void cgraph_remove_edge (struct cgraph_edge *);
void cgraph_remove_node (struct cgraph_node *);
void cgraph_node_remove_callees (struct cgraph_node *node);
struct cgraph_edge *cgraph_create_edge (struct cgraph_node *,
					struct cgraph_node *,
				        tree, gcov_type, int);
struct cgraph_node *cgraph_node (tree);
struct cgraph_node *cgraph_node_for_asm (tree asmname);
struct cgraph_edge *cgraph_edge (struct cgraph_node *, tree);
struct cgraph_local_info *cgraph_local_info (tree);
struct cgraph_global_info *cgraph_global_info (tree);
struct cgraph_rtl_info *cgraph_rtl_info (tree);
const char * cgraph_node_name (struct cgraph_node *);
struct cgraph_edge * cgraph_clone_edge (struct cgraph_edge *,
				        struct cgraph_node *,
				        tree, gcov_type, int, bool);
struct cgraph_node * cgraph_clone_node (struct cgraph_node *, gcov_type,
					int, bool);

struct cgraph_varpool_node *cgraph_varpool_node (tree);
struct cgraph_varpool_node *cgraph_varpool_node_for_asm (tree asmname);
void cgraph_varpool_mark_needed_node (struct cgraph_varpool_node *);
void cgraph_varpool_finalize_decl (tree);
void cgraph_redirect_edge_callee (struct cgraph_edge *, struct cgraph_node *);

bool cgraph_function_possibly_inlined_p (tree);
void cgraph_unnest_node (struct cgraph_node *);
void cgraph_varpool_enqueue_needed_node (struct cgraph_varpool_node *);
void cgraph_varpool_reset_queue (void);
bool decide_is_variable_needed (struct cgraph_varpool_node *, tree);

enum availability cgraph_function_body_availability (struct cgraph_node *);
enum availability cgraph_variable_initializer_availability (struct cgraph_varpool_node *);
bool cgraph_is_master_clone (struct cgraph_node *);
struct cgraph_node *cgraph_master_clone (struct cgraph_node *);

/* In cgraphunit.c  */
bool cgraph_assemble_pending_functions (void);
bool cgraph_varpool_assemble_pending_decls (void);
void cgraph_finalize_function (tree, bool);
void cgraph_lower_function (struct cgraph_node *);
void cgraph_finalize_compilation_unit (void);
void cgraph_optimize (void);
void cgraph_mark_needed_node (struct cgraph_node *);
void cgraph_mark_reachable_node (struct cgraph_node *);
bool cgraph_inline_p (struct cgraph_edge *, const char **reason);
bool cgraph_preserve_function_body_p (tree);
void verify_cgraph (void);
void verify_cgraph_node (struct cgraph_node *);
void cgraph_build_static_cdtor (char which, tree body, int priority);
void cgraph_reset_static_var_maps (void);
void init_cgraph (void);
struct cgraph_node *cgraph_function_versioning (struct cgraph_node *,
                                                varray_type, varray_type);

/* In ipa.c  */
bool cgraph_remove_unreachable_nodes (bool, FILE *);
int cgraph_postorder (struct cgraph_node **);

/* In ipa-inline.c  */
bool cgraph_decide_inlining_incrementally (struct cgraph_node *, bool);
void cgraph_clone_inlined_nodes (struct cgraph_edge *, bool, bool);
void cgraph_mark_inline_edge (struct cgraph_edge *, bool);
bool cgraph_default_inline_p (struct cgraph_node *, const char **);
#endif  /* GCC_CGRAPH_H  */
