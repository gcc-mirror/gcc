/* Callgraph handling code.
   Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.
   Contributed by Jan Hubicka

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_CGRAPH_H
#define GCC_CGRAPH_H

#include "plugin-api.h"
#include "vec.h"
#include "tree.h"
#include "basic-block.h"
#include "function.h"
#include "ipa-ref.h"	/* FIXME: inappropriate dependency of cgraph on IPA.  */

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

/* This is the information that is put into the cgraph local structure
   to recover a function.  */
struct lto_file_decl_data;

extern const char * const cgraph_availability_names[];
extern const char * const ld_plugin_symbol_resolution_names[];

/* Information about thunk, used only for same body aliases.  */

struct GTY(()) cgraph_thunk_info {
  /* Information about the thunk.  */
  HOST_WIDE_INT fixed_offset;
  HOST_WIDE_INT virtual_value;
  tree alias;
  bool this_adjusting;
  bool virtual_offset_p;
  /* Set to true when alias node is thunk.  */
  bool thunk_p;
};

/* Information about the function collected locally.
   Available after function is analyzed.  */

struct GTY(()) cgraph_local_info {
  /* File stream where this node is being written to.  */
  struct lto_file_decl_data * lto_file_data;

  /* Set when function function is visible in current compilation unit only
     and its address is never taken.  */
  unsigned local : 1;

  /* Set when function is visible by other units.  */
  unsigned externally_visible : 1;

  /* Set once it has been finalized so we consider it to be output.  */
  unsigned finalized : 1;

  /* False when there is something makes versioning impossible.  */
  unsigned versionable : 1;

  /* False when function calling convention and signature can not be changed.
     This is the case when __builtin_apply_args is used.  */
  unsigned can_change_signature : 1;

  /* True when the function has been originally extern inline, but it is
     redefined now.  */
  unsigned redefined_extern_inline : 1;

  /* True if the function may enter serial irrevocable mode.  */
  unsigned tm_may_enter_irr : 1;
};

/* Information about the function that needs to be computed globally
   once compilation is finished.  Available only with -funit-at-a-time.  */

struct GTY(()) cgraph_global_info {
  /* For inline clones this points to the function they will be
     inlined into.  */
  struct cgraph_node *inlined_to;
};

/* Information about the function that is propagated by the RTL backend.
   Available only for functions that has been already assembled.  */

struct GTY(()) cgraph_rtl_info {
   unsigned int preferred_incoming_stack_boundary;
};

/* Represent which DECL tree (or reference to such tree)
   will be replaced by another tree while versioning.  */
struct GTY(()) ipa_replace_map
{
  /* The tree that will be replaced.  */
  tree old_tree;
  /* The new (replacing) tree.  */
  tree new_tree;
  /* Parameter number to replace, when old_tree is NULL.  */
  int parm_num;
  /* True when a substitution should be done, false otherwise.  */
  bool replace_p;
  /* True when we replace a reference to old_tree.  */
  bool ref_p;
};
typedef struct ipa_replace_map *ipa_replace_map_p;
DEF_VEC_P(ipa_replace_map_p);
DEF_VEC_ALLOC_P(ipa_replace_map_p,gc);

struct GTY(()) cgraph_clone_info
{
  VEC(ipa_replace_map_p,gc)* tree_map;
  bitmap args_to_skip;
  bitmap combined_args_to_skip;
};


/* The cgraph data structure.
   Each function decl has assigned cgraph_node listing callees and callers.  */

struct GTY((chain_next ("%h.next"), chain_prev ("%h.previous"))) cgraph_node {
  tree decl;
  struct cgraph_edge *callees;
  struct cgraph_edge *callers;
  struct cgraph_node *next;
  struct cgraph_node *previous;
  /* List of edges representing indirect calls with a yet undetermined
     callee.  */
  struct cgraph_edge *indirect_calls;
  /* For nested functions points to function the node is nested in.  */
  struct cgraph_node *origin;
  /* Points to first nested function, if any.  */
  struct cgraph_node *nested;
  /* Pointer to the next function with same origin, if any.  */
  struct cgraph_node *next_nested;
  /* Pointer to the next function in cgraph_nodes_queue.  */
  struct cgraph_node *next_needed;
  /* Pointer to the next clone.  */
  struct cgraph_node *next_sibling_clone;
  struct cgraph_node *prev_sibling_clone;
  struct cgraph_node *clones;
  struct cgraph_node *clone_of;
  /* Circular list of nodes in the same comdat group if non-NULL.  */
  struct cgraph_node *same_comdat_group;
  /* For functions with many calls sites it holds map from call expression
     to the edge to speed up cgraph_edge function.  */
  htab_t GTY((param_is (struct cgraph_edge))) call_site_hash;
  /* Declaration node used to be clone of. */
  tree former_clone_of;

  PTR GTY ((skip)) aux;

  /* Interprocedural passes scheduled to have their transform functions
     applied next time we execute local pass on them.  We maintain it
     per-function in order to allow IPA passes to introduce new functions.  */
  VEC(ipa_opt_pass,heap) * GTY((skip)) ipa_transforms_to_apply;

  struct ipa_ref_list ref_list;
  struct cgraph_local_info local;
  struct cgraph_global_info global;
  struct cgraph_rtl_info rtl;
  struct cgraph_clone_info clone;
  struct cgraph_thunk_info thunk;

  /* Expected number of executions: calculated in profile.c.  */
  gcov_type count;
  /* How to scale counts at materialization time; used to merge
     LTO units with different number of profile runs.  */
  int count_materialization_scale;
  /* Unique id of the node.  */
  int uid;
  /* Ordering of all cgraph nodes.  */
  int order;

  enum ld_plugin_symbol_resolution resolution;

  /* Set when function must be output for some reason.  The primary
     use of this flag is to mark functions needed to be output for
     non-standard reason.  Functions that are externally visible
     or reachable from functions needed to be output are marked
     by specialized flags.  */
  unsigned needed : 1;
  /* Set when function has address taken.
     In current implementation it imply needed flag. */
  unsigned address_taken : 1;
  /* Set when decl is an abstract function pointed to by the
     ABSTRACT_DECL_ORIGIN of a reachable function.  */
  unsigned abstract_and_needed : 1;
  /* Set when function is reachable by call from other function
     that is either reachable or needed.
     This flag is computed at original cgraph construction and then
     updated in cgraph_remove_unreachable_nodes.  Note that after
     cgraph_remove_unreachable_nodes cgraph still can contain unreachable
     nodes when they are needed for virtual clone instantiation.  */
  unsigned reachable : 1;
  /* Set when function is reachable by call from other LTRANS partition.  */
  unsigned reachable_from_other_partition : 1;
  /* Set once the function is lowered (i.e. its CFG is built).  */
  unsigned lowered : 1;
  /* Set once the function has been instantiated and its callee
     lists created.  */
  unsigned analyzed : 1;
  /* Set when function is available in the other LTRANS partition.  
     During WPA output it is used to mark nodes that are present in
     multiple partitions.  */
  unsigned in_other_partition : 1;
  /* Set when function is scheduled to be processed by local passes.  */
  unsigned process : 1;
  /* Set for aliases once they got through assemble_alias.  */
  unsigned alias : 1;
  /* Set for aliases created as C++ same body aliases.  */
  unsigned same_body_alias : 1;
  /* How commonly executed the node is.  Initialized during branch
     probabilities pass.  */
  ENUM_BITFIELD (node_frequency) frequency : 2;
  /* True when function can only be called at startup (from static ctor).  */
  unsigned only_called_at_startup : 1;
  /* True when function can only be called at startup (from static dtor).  */
  unsigned only_called_at_exit : 1;
  /* True when function is the transactional clone of a function which
     is called only from inside transactions.  */
  /* ?? We should be able to remove this.  We have enough bits in
     cgraph to calculate it.  */
  unsigned tm_clone : 1;
};

typedef struct cgraph_node *cgraph_node_ptr;

DEF_VEC_P(cgraph_node_ptr);
DEF_VEC_ALLOC_P(cgraph_node_ptr,heap);
DEF_VEC_ALLOC_P(cgraph_node_ptr,gc);

/* A cgraph node set is a collection of cgraph nodes.  A cgraph node
   can appear in multiple sets.  */
struct cgraph_node_set_def
{
  struct pointer_map_t *map;
  VEC(cgraph_node_ptr, heap) *nodes;
};

typedef struct varpool_node *varpool_node_ptr;

DEF_VEC_P(varpool_node_ptr);
DEF_VEC_ALLOC_P(varpool_node_ptr,heap);
DEF_VEC_ALLOC_P(varpool_node_ptr,gc);

/* A varpool node set is a collection of varpool nodes.  A varpool node
   can appear in multiple sets.  */
struct varpool_node_set_def
{
  struct pointer_map_t * map;
  VEC(varpool_node_ptr, heap) *nodes;
};

typedef struct cgraph_node_set_def *cgraph_node_set;

DEF_VEC_P(cgraph_node_set);
DEF_VEC_ALLOC_P(cgraph_node_set,gc);
DEF_VEC_ALLOC_P(cgraph_node_set,heap);

typedef struct varpool_node_set_def *varpool_node_set;

DEF_VEC_P(varpool_node_set);
DEF_VEC_ALLOC_P(varpool_node_set,gc);
DEF_VEC_ALLOC_P(varpool_node_set,heap);

/* Iterator structure for cgraph node sets.  */
typedef struct
{
  cgraph_node_set set;
  unsigned index;
} cgraph_node_set_iterator;

/* Iterator structure for varpool node sets.  */
typedef struct
{
  varpool_node_set set;
  unsigned index;
} varpool_node_set_iterator;

#define DEFCIFCODE(code, string)	CIF_ ## code,
/* Reasons for inlining failures.  */
typedef enum cgraph_inline_failed_enum {
#include "cif-code.def"
  CIF_N_REASONS
} cgraph_inline_failed_t;

/* Structure containing additional information about an indirect call.  */

struct GTY(()) cgraph_indirect_call_info
{
  /* Offset accumulated from ancestor jump functions of inlined call graph
     edges.  */
  HOST_WIDE_INT anc_offset;
  /* OBJ_TYPE_REF_TOKEN of a polymorphic call (if polymorphic is set).  */
  HOST_WIDE_INT otr_token;
  /* Type of the object from OBJ_TYPE_REF_OBJECT. */
  tree otr_type;
  /* Index of the parameter that is called.  */
  int param_index;
  /* ECF flags determined from the caller.  */
  int ecf_flags;

  /* Set when the call is a virtual call with the parameter being the
     associated object pointer rather than a simple direct call.  */
  unsigned polymorphic : 1;
};

struct GTY((chain_next ("%h.next_caller"), chain_prev ("%h.prev_caller"))) cgraph_edge {
  /* Expected number of executions: calculated in profile.c.  */
  gcov_type count;
  struct cgraph_node *caller;
  struct cgraph_node *callee;
  struct cgraph_edge *prev_caller;
  struct cgraph_edge *next_caller;
  struct cgraph_edge *prev_callee;
  struct cgraph_edge *next_callee;
  gimple call_stmt;
  /* Additional information about an indirect call.  Not cleared when an edge
     becomes direct.  */
  struct cgraph_indirect_call_info *indirect_info;
  PTR GTY ((skip (""))) aux;
  /* When equal to CIF_OK, inline this call.  Otherwise, points to the
     explanation why function was not inlined.  */
  cgraph_inline_failed_t inline_failed;
  /* The stmt_uid of call_stmt.  This is used by LTO to recover the call_stmt
     when the function is serialized in.  */
  unsigned int lto_stmt_uid;
  /* Expected frequency of executions within the function.
     When set to CGRAPH_FREQ_BASE, the edge is expected to be called once
     per function call.  The range is 0 to CGRAPH_FREQ_MAX.  */
  int frequency;
  /* Unique id of the edge.  */
  int uid;
  /* Whether this edge was made direct by indirect inlining.  */
  unsigned int indirect_inlining_edge : 1;
  /* Whether this edge describes an indirect call with an undetermined
     callee.  */
  unsigned int indirect_unknown_callee : 1;
  /* Whether this edge is still a dangling  */
  /* True if the corresponding CALL stmt cannot be inlined.  */
  unsigned int call_stmt_cannot_inline_p : 1;
  /* Can this call throw externally?  */
  unsigned int can_throw_external : 1;
};

#define CGRAPH_FREQ_BASE 1000
#define CGRAPH_FREQ_MAX 100000

typedef struct cgraph_edge *cgraph_edge_p;

DEF_VEC_P(cgraph_edge_p);
DEF_VEC_ALLOC_P(cgraph_edge_p,heap);

/* The varpool data structure.
   Each static variable decl has assigned varpool_node.  */

struct GTY((chain_next ("%h.next"), chain_prev ("%h.prev"))) varpool_node {
  tree decl;
  /* For aliases points to declaration DECL is alias of.  */
  tree alias_of;
  /* Pointer to the next function in varpool_nodes.  */
  struct varpool_node *next, *prev;
  /* Pointer to the next function in varpool_nodes_queue.  */
  struct varpool_node *next_needed, *prev_needed;
  /* Circular list of nodes in the same comdat group if non-NULL.  */
  struct varpool_node *same_comdat_group;
  struct ipa_ref_list ref_list;
  /* File stream where this node is being written to.  */
  struct lto_file_decl_data * lto_file_data;
  PTR GTY ((skip)) aux;
  /* Ordering of all cgraph nodes.  */
  int order;
  enum ld_plugin_symbol_resolution resolution;

  /* Set when function must be output - it is externally visible
     or its address is taken.  */
  unsigned needed : 1;
  /* Needed variables might become dead by optimization.  This flag
     forces the variable to be output even if it appears dead otherwise.  */
  unsigned force_output : 1;
  /* Set once the variable has been instantiated and its callee
     lists created.  */
  unsigned analyzed : 1;
  /* Set once it has been finalized so we consider it to be output.  */
  unsigned finalized : 1;
  /* Set when variable is scheduled to be assembled.  */
  unsigned output : 1;
  /* Set when function is visible by other units.  */
  unsigned externally_visible : 1;
  /* Set for aliases once they got through assemble_alias.  Also set for
     extra name aliases in varpool_extra_name_alias.  */
  unsigned alias : 1;
  unsigned extra_name_alias : 1;
  /* Set when variable is used from other LTRANS partition.  */
  unsigned used_from_other_partition : 1;
  /* Set when variable is available in the other LTRANS partition.
     During WPA output it is used to mark nodes that are present in
     multiple partitions.  */
  unsigned in_other_partition : 1;
};

/* Every top level asm statement is put into a cgraph_asm_node.  */

struct GTY(()) cgraph_asm_node {
  /* Next asm node.  */
  struct cgraph_asm_node *next;
  /* String for this asm node.  */
  tree asm_str;
  /* Ordering of all cgraph nodes.  */
  int order;
};

extern GTY(()) struct cgraph_node *cgraph_nodes;
extern GTY(()) int cgraph_n_nodes;
extern GTY(()) int cgraph_max_uid;
extern GTY(()) int cgraph_edge_max_uid;
extern bool cgraph_global_info_ready;
enum cgraph_state
{
  /* Callgraph is being constructed.  It is safe to add new functions.  */
  CGRAPH_STATE_CONSTRUCTION,
  /* Callgraph is built and IPA passes are being run.  */
  CGRAPH_STATE_IPA,
  /* Callgraph is built and all functions are transformed to SSA form.  */
  CGRAPH_STATE_IPA_SSA,
  /* Functions are now ordered and being passed to RTL expanders.  */
  CGRAPH_STATE_EXPANSION,
  /* All cgraph expansion is done.  */
  CGRAPH_STATE_FINISHED
};
extern enum cgraph_state cgraph_state;
extern bool cgraph_function_flags_ready;
extern GTY(()) struct cgraph_node *cgraph_nodes_queue;
extern GTY(()) struct cgraph_node *cgraph_new_nodes;

extern GTY(()) struct cgraph_asm_node *cgraph_asm_nodes;
extern GTY(()) int cgraph_order;
extern bool same_body_aliases_done;

/* In cgraph.c  */
void dump_cgraph (FILE *);
void debug_cgraph (void);
void dump_cgraph_node (FILE *, struct cgraph_node *);
void debug_cgraph_node (struct cgraph_node *);
void cgraph_insert_node_to_hashtable (struct cgraph_node *node);
void cgraph_remove_edge (struct cgraph_edge *);
void cgraph_remove_node (struct cgraph_node *);
void cgraph_add_to_same_comdat_group (struct cgraph_node *, struct cgraph_node *);
void cgraph_remove_node_and_inline_clones (struct cgraph_node *);
void cgraph_release_function_body (struct cgraph_node *);
void cgraph_node_remove_callees (struct cgraph_node *node);
struct cgraph_edge *cgraph_create_edge (struct cgraph_node *,
					struct cgraph_node *,
					gimple, gcov_type, int);
struct cgraph_edge *cgraph_create_indirect_edge (struct cgraph_node *, gimple,
						 int, gcov_type, int);
struct cgraph_indirect_call_info *cgraph_allocate_init_indirect_info (void);
struct cgraph_node * cgraph_get_node (const_tree);
struct cgraph_node * cgraph_create_node (tree);
struct cgraph_node * cgraph_get_create_node (tree);
struct cgraph_node * cgraph_same_body_alias (struct cgraph_node *, tree, tree);
struct cgraph_node * cgraph_add_thunk (struct cgraph_node *, tree, tree, bool, HOST_WIDE_INT,
				       HOST_WIDE_INT, tree, tree);
struct cgraph_node *cgraph_node_for_asm (tree);
struct cgraph_edge *cgraph_edge (struct cgraph_node *, gimple);
void cgraph_set_call_stmt (struct cgraph_edge *, gimple);
void cgraph_set_call_stmt_including_clones (struct cgraph_node *, gimple, gimple);
void cgraph_create_edge_including_clones (struct cgraph_node *,
					  struct cgraph_node *,
					  gimple, gimple, gcov_type, int,
					  cgraph_inline_failed_t);
void cgraph_update_edges_for_call_stmt (gimple, tree, gimple);
struct cgraph_local_info *cgraph_local_info (tree);
struct cgraph_global_info *cgraph_global_info (tree);
struct cgraph_rtl_info *cgraph_rtl_info (tree);
const char * cgraph_node_name (struct cgraph_node *);
struct cgraph_edge * cgraph_clone_edge (struct cgraph_edge *,
					struct cgraph_node *, gimple,
					unsigned, gcov_type, int, bool);
struct cgraph_node * cgraph_clone_node (struct cgraph_node *, tree, gcov_type,
					int, bool, VEC(cgraph_edge_p,heap) *,
					bool);
struct cgraph_node *cgraph_create_function_alias (tree, tree);

void cgraph_redirect_edge_callee (struct cgraph_edge *, struct cgraph_node *);
void cgraph_make_edge_direct (struct cgraph_edge *, struct cgraph_node *);
bool cgraph_only_called_directly_p (struct cgraph_node *);

struct cgraph_asm_node *cgraph_add_asm_node (tree);

bool cgraph_function_possibly_inlined_p (tree);
void cgraph_unnest_node (struct cgraph_node *);

enum availability cgraph_function_body_availability (struct cgraph_node *);
void cgraph_add_new_function (tree, bool);
const char* cgraph_inline_failed_string (cgraph_inline_failed_t);
struct cgraph_node * cgraph_create_virtual_clone (struct cgraph_node *old_node,
			                          VEC(cgraph_edge_p,heap)*,
			                          VEC(ipa_replace_map_p,gc)* tree_map,
			                          bitmap args_to_skip,
						  const char *clone_name);

void cgraph_set_nothrow_flag (struct cgraph_node *, bool);
void cgraph_set_const_flag (struct cgraph_node *, bool, bool);
void cgraph_set_pure_flag (struct cgraph_node *, bool, bool);
tree clone_function_name (tree decl, const char *);
bool cgraph_node_cannot_return (struct cgraph_node *);
bool cgraph_edge_cannot_lead_to_return (struct cgraph_edge *);
bool cgraph_will_be_removed_from_program_if_no_direct_calls
  (struct cgraph_node *node);
bool cgraph_can_remove_if_no_direct_calls_and_refs_p
  (struct cgraph_node *node);
bool cgraph_can_remove_if_no_direct_calls_p (struct cgraph_node *node);
bool resolution_used_from_other_file_p (enum ld_plugin_symbol_resolution);
bool cgraph_used_from_object_file_p (struct cgraph_node *);
bool varpool_used_from_object_file_p (struct varpool_node *);
bool cgraph_for_node_thunks_and_aliases (struct cgraph_node *,
			                 bool (*) (struct cgraph_node *, void *),
			                 void *,
					 bool);
bool cgraph_for_node_and_aliases (struct cgraph_node *,
		                  bool (*) (struct cgraph_node *, void *),
			          void *, bool);
VEC (cgraph_edge_p, heap) * collect_callers_of_node (struct cgraph_node *node);


/* In cgraphunit.c  */
extern FILE *cgraph_dump_file;
void cgraph_finalize_function (tree, bool);
void cgraph_mark_if_needed (tree);
void cgraph_analyze_function (struct cgraph_node *);
void cgraph_finalize_compilation_unit (void);
void cgraph_optimize (void);
void cgraph_mark_needed_node (struct cgraph_node *);
void cgraph_mark_address_taken_node (struct cgraph_node *);
void cgraph_mark_reachable_node (struct cgraph_node *);
bool cgraph_inline_p (struct cgraph_edge *, cgraph_inline_failed_t *reason);
bool cgraph_preserve_function_body_p (struct cgraph_node *);
void verify_cgraph (void);
void verify_cgraph_node (struct cgraph_node *);
void cgraph_build_static_cdtor (char which, tree body, int priority);
void cgraph_reset_static_var_maps (void);
void init_cgraph (void);
struct cgraph_node * cgraph_copy_node_for_versioning (struct cgraph_node *,
		tree, VEC(cgraph_edge_p,heap)*, bitmap);
struct cgraph_node *cgraph_function_versioning (struct cgraph_node *,
						VEC(cgraph_edge_p,heap)*,
						VEC(ipa_replace_map_p,gc)*,
						bitmap, bitmap, basic_block,
						const char *);
void tree_function_versioning (tree, tree, VEC (ipa_replace_map_p,gc)*, bool, bitmap,
			       bitmap, basic_block);
void record_references_in_initializer (tree, bool);
bool cgraph_process_new_functions (void);
void cgraph_process_same_body_aliases (void);

bool cgraph_decide_is_function_needed (struct cgraph_node *, tree);

typedef void (*cgraph_edge_hook)(struct cgraph_edge *, void *);
typedef void (*cgraph_node_hook)(struct cgraph_node *, void *);
typedef void (*cgraph_2edge_hook)(struct cgraph_edge *, struct cgraph_edge *,
				  void *);
typedef void (*cgraph_2node_hook)(struct cgraph_node *, struct cgraph_node *,
				  void *);
struct cgraph_edge_hook_list;
struct cgraph_node_hook_list;
struct cgraph_2edge_hook_list;
struct cgraph_2node_hook_list;
struct cgraph_edge_hook_list *cgraph_add_edge_removal_hook (cgraph_edge_hook, void *);
void cgraph_remove_edge_removal_hook (struct cgraph_edge_hook_list *);
struct cgraph_node_hook_list *cgraph_add_node_removal_hook (cgraph_node_hook,
							    void *);
void cgraph_remove_node_removal_hook (struct cgraph_node_hook_list *);
struct cgraph_node_hook_list *cgraph_add_function_insertion_hook (cgraph_node_hook,
							          void *);
void cgraph_remove_function_insertion_hook (struct cgraph_node_hook_list *);
void cgraph_call_function_insertion_hooks (struct cgraph_node *node);
struct cgraph_2edge_hook_list *cgraph_add_edge_duplication_hook (cgraph_2edge_hook, void *);
void cgraph_remove_edge_duplication_hook (struct cgraph_2edge_hook_list *);
struct cgraph_2node_hook_list *cgraph_add_node_duplication_hook (cgraph_2node_hook, void *);
void cgraph_remove_node_duplication_hook (struct cgraph_2node_hook_list *);
void cgraph_materialize_all_clones (void);
gimple cgraph_redirect_edge_call_stmt_to_callee (struct cgraph_edge *);
bool cgraph_propagate_frequency (struct cgraph_node *node);
/* In cgraphbuild.c  */
unsigned int rebuild_cgraph_edges (void);
void cgraph_rebuild_references (void);
void reset_inline_failed (struct cgraph_node *);
int compute_call_stmt_bb_frequency (tree, basic_block bb);

/* In ipa.c  */
bool cgraph_remove_unreachable_nodes (bool, FILE *);
cgraph_node_set cgraph_node_set_new (void);
cgraph_node_set_iterator cgraph_node_set_find (cgraph_node_set,
					       struct cgraph_node *);
void cgraph_node_set_add (cgraph_node_set, struct cgraph_node *);
void cgraph_node_set_remove (cgraph_node_set, struct cgraph_node *);
void dump_cgraph_node_set (FILE *, cgraph_node_set);
void debug_cgraph_node_set (cgraph_node_set);
void free_cgraph_node_set (cgraph_node_set);

varpool_node_set varpool_node_set_new (void);
varpool_node_set_iterator varpool_node_set_find (varpool_node_set,
					       struct varpool_node *);
void varpool_node_set_add (varpool_node_set, struct varpool_node *);
void varpool_node_set_remove (varpool_node_set, struct varpool_node *);
void dump_varpool_node_set (FILE *, varpool_node_set);
void debug_varpool_node_set (varpool_node_set);
void free_varpool_node_set (varpool_node_set);
void ipa_discover_readonly_nonaddressable_vars (void);
bool cgraph_comdat_can_be_unshared_p (struct cgraph_node *);
bool varpool_externally_visible_p (struct varpool_node *, bool);

/* In predict.c  */
bool cgraph_maybe_hot_edge_p (struct cgraph_edge *e);
bool cgraph_optimize_for_size_p (struct cgraph_node *);

/* In varpool.c  */
extern GTY(()) struct varpool_node *varpool_nodes_queue;
extern GTY(()) struct varpool_node *varpool_nodes;

struct varpool_node *varpool_node (tree);
struct varpool_node *varpool_node_for_asm (tree asmname);
void varpool_mark_needed_node (struct varpool_node *);
void debug_varpool (void);
void dump_varpool (FILE *);
void dump_varpool_node (FILE *, struct varpool_node *);

void varpool_finalize_decl (tree);
bool decide_is_variable_needed (struct varpool_node *, tree);
enum availability cgraph_variable_initializer_availability (struct varpool_node *);
void cgraph_make_decl_local (tree);
void cgraph_make_node_local (struct cgraph_node *);
bool cgraph_node_can_be_local_p (struct cgraph_node *);


struct varpool_node * varpool_get_node (const_tree decl);
void varpool_remove_node (struct varpool_node *node);
void varpool_finalize_named_section_flags (struct varpool_node *node);
bool varpool_assemble_pending_decls (void);
bool varpool_assemble_decl (struct varpool_node *node);
bool varpool_analyze_pending_decls (void);
void varpool_remove_unreferenced_decls (void);
void varpool_empty_needed_queue (void);
struct varpool_node * varpool_extra_name_alias (tree, tree);
struct varpool_node * varpool_create_variable_alias (tree, tree);
const char * varpool_node_name (struct varpool_node *node);
void varpool_reset_queue (void);
bool const_value_known_p (tree);
bool varpool_for_node_and_aliases (struct varpool_node *,
		                   bool (*) (struct varpool_node *, void *),
			           void *, bool);
void varpool_add_new_variable (tree);

/* Walk all reachable static variables.  */
#define FOR_EACH_STATIC_VARIABLE(node) \
   for ((node) = varpool_nodes_queue; (node); (node) = (node)->next_needed)

/* Return first reachable static variable with initializer.  */
static inline struct varpool_node *
varpool_first_static_initializer (void)
{
  struct varpool_node *node;
  for (node = varpool_nodes_queue; node; node = node->next_needed)
    {
      gcc_checking_assert (TREE_CODE (node->decl) == VAR_DECL);
      if (DECL_INITIAL (node->decl))
	return node;
    }
  return NULL;
}

/* Return next reachable static variable with initializer after NODE.  */
static inline struct varpool_node *
varpool_next_static_initializer (struct varpool_node *node)
{
  for (node = node->next_needed; node; node = node->next_needed)
    {
      gcc_checking_assert (TREE_CODE (node->decl) == VAR_DECL);
      if (DECL_INITIAL (node->decl))
	return node;
    }
  return NULL;
}

/* Walk all static variables with initializer set.  */
#define FOR_EACH_STATIC_INITIALIZER(node) \
   for ((node) = varpool_first_static_initializer (); (node); \
        (node) = varpool_next_static_initializer (node))

/* Return first function with body defined.  */
static inline struct cgraph_node *
cgraph_first_defined_function (void)
{
  struct cgraph_node *node;
  for (node = cgraph_nodes; node; node = node->next)
    {
      if (node->analyzed)
	return node;
    }
  return NULL;
}

/* Return next reachable static variable with initializer after NODE.  */
static inline struct cgraph_node *
cgraph_next_defined_function (struct cgraph_node *node)
{
  for (node = node->next; node; node = node->next)
    {
      if (node->analyzed)
	return node;
    }
  return NULL;
}

/* Walk all functions with body defined.  */
#define FOR_EACH_DEFINED_FUNCTION(node) \
   for ((node) = cgraph_first_defined_function (); (node); \
        (node) = cgraph_next_defined_function (node))


/* Return true when NODE is a function with Gimple body defined
   in current unit.  Functions can also be define externally or they
   can be thunks with no Gimple representation.

   Note that at WPA stage, the function body may not be present in memory.  */

static inline bool
cgraph_function_with_gimple_body_p (struct cgraph_node *node)
{
  return node->analyzed && !node->thunk.thunk_p && !node->alias;
}

/* Return first function with body defined.  */
static inline struct cgraph_node *
cgraph_first_function_with_gimple_body (void)
{
  struct cgraph_node *node;
  for (node = cgraph_nodes; node; node = node->next)
    {
      if (cgraph_function_with_gimple_body_p (node))
	return node;
    }
  return NULL;
}

/* Return next reachable static variable with initializer after NODE.  */
static inline struct cgraph_node *
cgraph_next_function_with_gimple_body (struct cgraph_node *node)
{
  for (node = node->next; node; node = node->next)
    {
      if (cgraph_function_with_gimple_body_p (node))
	return node;
    }
  return NULL;
}

/* Walk all functions with body defined.  */
#define FOR_EACH_FUNCTION_WITH_GIMPLE_BODY(node) \
   for ((node) = cgraph_first_function_with_gimple_body (); (node); \
        (node) = cgraph_next_function_with_gimple_body (node))

/* Create a new static variable of type TYPE.  */
tree add_new_static_var (tree type);

/* Return true if iterator CSI points to nothing.  */
static inline bool
csi_end_p (cgraph_node_set_iterator csi)
{
  return csi.index >= VEC_length (cgraph_node_ptr, csi.set->nodes);
}

/* Advance iterator CSI.  */
static inline void
csi_next (cgraph_node_set_iterator *csi)
{
  csi->index++;
}

/* Return the node pointed to by CSI.  */
static inline struct cgraph_node *
csi_node (cgraph_node_set_iterator csi)
{
  return VEC_index (cgraph_node_ptr, csi.set->nodes, csi.index);
}

/* Return an iterator to the first node in SET.  */
static inline cgraph_node_set_iterator
csi_start (cgraph_node_set set)
{
  cgraph_node_set_iterator csi;

  csi.set = set;
  csi.index = 0;
  return csi;
}

/* Return true if SET contains NODE.  */
static inline bool
cgraph_node_in_set_p (struct cgraph_node *node, cgraph_node_set set)
{
  cgraph_node_set_iterator csi;
  csi = cgraph_node_set_find (set, node);
  return !csi_end_p (csi);
}

/* Return number of nodes in SET.  */
static inline size_t
cgraph_node_set_size (cgraph_node_set set)
{
  return VEC_length (cgraph_node_ptr, set->nodes);
}

/* Return true if iterator VSI points to nothing.  */
static inline bool
vsi_end_p (varpool_node_set_iterator vsi)
{
  return vsi.index >= VEC_length (varpool_node_ptr, vsi.set->nodes);
}

/* Advance iterator VSI.  */
static inline void
vsi_next (varpool_node_set_iterator *vsi)
{
  vsi->index++;
}

/* Return the node pointed to by VSI.  */
static inline struct varpool_node *
vsi_node (varpool_node_set_iterator vsi)
{
  return VEC_index (varpool_node_ptr, vsi.set->nodes, vsi.index);
}

/* Return an iterator to the first node in SET.  */
static inline varpool_node_set_iterator
vsi_start (varpool_node_set set)
{
  varpool_node_set_iterator vsi;

  vsi.set = set;
  vsi.index = 0;
  return vsi;
}

/* Return true if SET contains NODE.  */
static inline bool
varpool_node_in_set_p (struct varpool_node *node, varpool_node_set set)
{
  varpool_node_set_iterator vsi;
  vsi = varpool_node_set_find (set, node);
  return !vsi_end_p (vsi);
}

/* Return number of nodes in SET.  */
static inline size_t
varpool_node_set_size (varpool_node_set set)
{
  return VEC_length (varpool_node_ptr, set->nodes);
}

/* Uniquize all constants that appear in memory.
   Each constant in memory thus far output is recorded
   in `const_desc_table'.  */

struct GTY(()) constant_descriptor_tree {
  /* A MEM for the constant.  */
  rtx rtl;

  /* The value of the constant.  */
  tree value;

  /* Hash of value.  Computing the hash from value each time
     hashfn is called can't work properly, as that means recursive
     use of the hash table during hash table expansion.  */
  hashval_t hash;
};

/* Return true if set is nonempty.  */
static inline bool
cgraph_node_set_nonempty_p (cgraph_node_set set)
{
  return !VEC_empty (cgraph_node_ptr, set->nodes);
}

/* Return true if set is nonempty.  */
static inline bool
varpool_node_set_nonempty_p (varpool_node_set set)
{
  return !VEC_empty (varpool_node_ptr, set->nodes);
}

/* Return true when function NODE is only called directly or it has alias.
   i.e. it is not externally visible, address was not taken and
   it is not used in any other non-standard way.  */

static inline bool
cgraph_only_called_directly_or_aliased_p (struct cgraph_node *node)
{
  gcc_assert (!node->global.inlined_to);
  return (!node->needed && !node->address_taken
	  && !node->reachable_from_other_partition
	  && !DECL_STATIC_CONSTRUCTOR (node->decl)
	  && !DECL_STATIC_DESTRUCTOR (node->decl)
	  && !node->local.externally_visible);
}

/* Return true when function NODE can be removed from callgraph
   if all direct calls are eliminated.  */

static inline bool
varpool_can_remove_if_no_refs (struct varpool_node *node)
{
  return (!node->force_output && !node->used_from_other_partition
	  && (flag_toplevel_reorder || DECL_COMDAT (node->decl)
	      || DECL_ARTIFICIAL (node->decl))
  	  && (DECL_COMDAT (node->decl) || !node->externally_visible));
}

/* Return true when all references to VNODE must be visible in ipa_ref_list.
   i.e. if the variable is not externally visible or not used in some magic
   way (asm statement or such).
   The magic uses are all summarized in force_output flag.  */

static inline bool
varpool_all_refs_explicit_p (struct varpool_node *vnode)
{
  return (vnode->analyzed
	  && !vnode->externally_visible
	  && !vnode->used_from_other_partition
	  && !vnode->force_output);
}

/* Constant pool accessor function.  */
htab_t constant_pool_htab (void);

/* FIXME: inappropriate dependency of cgraph on IPA.  */
#include "ipa-ref-inline.h"

/* Return node that alias N is aliasing.  */

static inline struct cgraph_node *
cgraph_alias_aliased_node (struct cgraph_node *n)
{
  struct ipa_ref *ref;

  ipa_ref_list_reference_iterate (&n->ref_list, 0, ref);
  gcc_checking_assert (ref->use == IPA_REF_ALIAS);
  if (ref->refered_type == IPA_REF_CGRAPH)
    return ipa_ref_node (ref);
  return NULL;
}

/* Return node that alias N is aliasing.  */

static inline struct varpool_node *
varpool_alias_aliased_node (struct varpool_node *n)
{
  struct ipa_ref *ref;

  ipa_ref_list_reference_iterate (&n->ref_list, 0, ref);
  gcc_checking_assert (ref->use == IPA_REF_ALIAS);
  if (ref->refered_type == IPA_REF_VARPOOL)
    return ipa_ref_varpool_node (ref);
  return NULL;
}

/* Given NODE, walk the alias chain to return the function NODE is alias of.
   Walk through thunk, too.
   When AVAILABILITY is non-NULL, get minimal availablity in the chain.  */

static inline struct cgraph_node *
cgraph_function_node (struct cgraph_node *node, enum availability *availability)
{
  if (availability)
    *availability = cgraph_function_body_availability (node);
  while (node)
    {
      if (node->alias && node->analyzed)
	node = cgraph_alias_aliased_node (node);
      else if (node->thunk.thunk_p)
	node = node->callees->callee;
      else
	return node;
      if (node && availability)
	{
	  enum availability a;
	  a = cgraph_function_body_availability (node);
	  if (a < *availability)
	    *availability = a;
	}
    }
  if (availability)
    *availability = AVAIL_NOT_AVAILABLE;
  return NULL;
}

/* Given NODE, walk the alias chain to return the function NODE is alias of.
   Do not walk through thunks.
   When AVAILABILITY is non-NULL, get minimal availablity in the chain.  */

static inline struct cgraph_node *
cgraph_function_or_thunk_node (struct cgraph_node *node, enum availability *availability)
{
  if (availability)
    *availability = cgraph_function_body_availability (node);
  while (node)
    {
      if (node->alias && node->analyzed)
	node = cgraph_alias_aliased_node (node);
      else
	return node;
      if (node && availability)
	{
	  enum availability a;
	  a = cgraph_function_body_availability (node);
	  if (a < *availability)
	    *availability = a;
	}
    }
  if (availability)
    *availability = AVAIL_NOT_AVAILABLE;
  return NULL;
}

/* Given NODE, walk the alias chain to return the function NODE is alias of.
   Do not walk through thunks.
   When AVAILABILITY is non-NULL, get minimal availablity in the chain.  */

static inline struct varpool_node *
varpool_variable_node (struct varpool_node *node, enum availability *availability)
{
  if (availability)
    *availability = cgraph_variable_initializer_availability (node);
  while (node)
    {
      if (node->alias && node->analyzed)
	node = varpool_alias_aliased_node (node);
      else
	return node;
      if (node && availability)
	{
	  enum availability a;
	  a = cgraph_variable_initializer_availability (node);
	  if (a < *availability)
	    *availability = a;
	}
    }
  if (availability)
    *availability = AVAIL_NOT_AVAILABLE;
  return NULL;
}

/* Return true when the edge E represents a direct recursion.  */
static inline bool
cgraph_edge_recursive_p (struct cgraph_edge *e)
{
  struct cgraph_node *callee = cgraph_function_or_thunk_node (e->callee, NULL);
  if (e->caller->global.inlined_to)
    return e->caller->global.inlined_to->decl == callee->decl;
  else
    return e->caller->decl == callee->decl;
}

/* Return true if the TM_CLONE bit is set for a given FNDECL.  */
static inline bool
decl_is_tm_clone (const_tree fndecl)
{
  struct cgraph_node *n = cgraph_get_node (fndecl);
  if (n)
    return n->tm_clone;
  return false;
}
#endif  /* GCC_CGRAPH_H  */
