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
#include "hashtab.h"
#include "bitmap.h"
#include "tree.h"

/* Information about the function collected locally.
   Available after function is analyzed.  */

struct cgraph_local_info GTY(())
{
  /* Size of the function before inlining.  */
  int self_insns;

  /* Set when function function is visible in current compilation unit only
     and it's address is never taken.  */
  bool local;

  /* Set when function is defined in another compilation unit.  */
  bool external;

  /* Set when this function calls a function external of the
     compilation unit.  In general, such calls are modeled as reading
     and writing all variables (both bits on) but sometime there are
     attributes on the called function so we can do better.  */
  bool calls_read_all;
  bool calls_write_all;

  /* Set once it has been finalized so we consider it to be output.  */
  bool finalized;

  /* False when there something makes inlining impossible (such as va_arg).  */
  bool inlinable;

  /* True when function should be inlined independently on it's size.  */
  bool disregard_inline_limits;

  /* True when the function has been originally extern inline, but it is
     redefined now.  */
  bool redefined_extern_inline;

  /* True if statics_read_for_function and
     statics_written_for_function contain valid data.  */
  bool for_functions_valid;
};

/* Information about the function that needs to be computed globally
   once compilation is finished.  Available only with -funit-at-time.  */

struct cgraph_global_info GTY(())
{
  /* For inline clones this points to the function they will be inlined into.  */
  struct cgraph_node *inlined_to;

  /* Estimated size of the function after inlining.  */
  int insns;

  /* Set iff the function has been inlined at least once.  */
  bool inlined;
};

/* Information about the function that is propagated by the RTL backend.
   Available only for functions that has been already assembled.  */

struct cgraph_rtl_info GTY(())
{
   int preferred_incoming_stack_boundary;
   bool const_function;
   bool pure_function;
};

/* FIXME -- PROFILE-RESTRUCTURE: When the next round of the profiling
   code gets merged in, it will contain a restructing where ssa form
   is built for every function within the compilation unit before the
   rest of the compilation continues.  When this reorgination is done,
   it will no longer be necessary to have the _decl_uid versions of
   local_static_vars_info and global_static_vars_info structures.
   Having both structures is now required because the _ann_uid values
   for static variables are reset as each function is compiled.
   Currently, the analysis is done using the _decl_uid versions and
   converted to the _var_ann versions on demand.

   Also, the var_anns_valid fields within these structures can also go
   away.
*/

/* The static variables defined within the compilation unit that are
   loaded or stored directly by function that owns this structure.  */ 

struct local_static_vars_info_d GTY(())
{
  bitmap statics_read_by_decl_uid;
  bitmap statics_written_by_decl_uid;
};

struct global_static_vars_info_d GTY(())
{
  bitmap statics_read_by_decl_uid;
  bitmap statics_written_by_decl_uid;
  bitmap statics_read_by_ann_uid;
  bitmap statics_written_by_ann_uid;
  bitmap statics_not_read_by_decl_uid;
  bitmap statics_not_written_by_decl_uid;
  bitmap statics_not_read_by_ann_uid;
  bitmap statics_not_written_by_ann_uid;

  /* var_anns_valid is reset at the start of compilation for each
     function because the indexing that the "_var_anns" is based
     on is invalidated between function compilations.  This allows for
     lazy creation of the "_var_ann" variables.  */
  bool var_anns_valid;
};

/* Statics that are read and written by some set of functions. The
   local ones are based on the loads and stores local to the function.
   The global ones are based on the local info as well as the
   transitive closure of the functions that are called.  The
   structures are separated to allow the global structures to be
   shared between several functions since every function within a
   strongly connected component will have the same information.  This
   sharing saves both time and space in the computation of the vectors
   as well as their translation from decl_uid form to ann_uid
   form.  */ 

typedef struct local_static_vars_info_d *local_static_vars_info_t;
typedef struct global_static_vars_info_d *global_static_vars_info_t;

struct static_vars_info_d GTY(()) 
{
  local_static_vars_info_t local;
  global_static_vars_info_t global;
};

typedef struct static_vars_info_d *static_vars_info_t;

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
  /* Pointer to next node in a recursive call graph cycle; */
  struct cgraph_node *next_cycle;
  PTR GTY ((skip)) aux;

  struct cgraph_local_info local;
  struct cgraph_global_info global;
  struct cgraph_rtl_info rtl;
  
  /* Pointer to the structure that contains the sets of global
     variables modified by function calls.  */
  static_vars_info_t static_vars_info;

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

struct cgraph_edge GTY((chain_next ("%h.next_caller")))
{
  struct cgraph_node *caller;
  struct cgraph_node *callee;
  struct cgraph_edge *next_caller;
  struct cgraph_edge *next_callee;
  tree call_expr;
  PTR GTY ((skip (""))) aux;
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

extern GTY(()) int cgraph_varpool_n_nodes;
extern GTY(()) struct cgraph_varpool_node *cgraph_varpool_nodes_queue;

/* In cgraph.c  */
void dump_cgraph (FILE *);
void dump_cgraph_node (FILE *, struct cgraph_node *);
void cgraph_remove_edge (struct cgraph_edge *);
void cgraph_remove_node (struct cgraph_node *);
struct cgraph_edge *cgraph_create_edge (struct cgraph_node *,
					struct cgraph_node *,
				        tree);
struct cgraph_node *cgraph_node (tree decl);
struct cgraph_edge *cgraph_edge (struct cgraph_node *, tree call_expr);
bool cgraph_calls_p (tree, tree);
struct cgraph_local_info *cgraph_local_info (tree);
struct cgraph_global_info *cgraph_global_info (tree);
struct cgraph_rtl_info *cgraph_rtl_info (tree);
const char * cgraph_node_name (struct cgraph_node *);
struct cgraph_edge * cgraph_clone_edge (struct cgraph_edge *, struct cgraph_node *, tree);
struct cgraph_node * cgraph_clone_node (struct cgraph_node *);

struct cgraph_varpool_node *cgraph_varpool_node (tree decl);
void cgraph_varpool_mark_needed_node (struct cgraph_varpool_node *);
void cgraph_varpool_finalize_decl (tree);
bool cgraph_varpool_assemble_pending_decls (void);
void cgraph_redirect_edge_callee (struct cgraph_edge *, struct cgraph_node *);

bool cgraph_function_possibly_inlined_p (tree);
void cgraph_unnest_node (struct cgraph_node *node);

/* In cgraphunit.c  */
bool cgraph_assemble_pending_functions (void);
void cgraph_finalize_function (tree, bool);
void cgraph_finalize_compilation_unit (void);
void cgraph_create_edges (struct cgraph_node *, tree);
void cgraph_optimize (void);
void cgraph_mark_needed_node (struct cgraph_node *);
void cgraph_mark_reachable_node (struct cgraph_node *);
bool cgraph_inline_p (struct cgraph_edge *, const char **reason);
bool cgraph_preserve_function_body_p (tree);
void verify_cgraph (void);
void verify_cgraph_node (struct cgraph_node *);
void cgraph_mark_inline_edge (struct cgraph_edge *e);
void cgraph_clone_inlined_nodes (struct cgraph_edge *e, bool duplicate);
void cgraph_build_static_cdtor (char which, tree body, int priority);
void cgraph_reset_static_var_maps (void);
bitmap get_global_statics_not_read (tree fn);
bitmap get_global_statics_not_written(tree fn);
void init_cgraph (void);

#endif  /* GCC_CGRAPH_H  */
