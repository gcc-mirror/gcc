/* Data and Control Flow Analysis for Trees.
   Copyright (C) 2001, 2003, 2004 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef _TREE_FLOW_H
#define _TREE_FLOW_H 1

#include "bitmap.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "hashtab.h"
#include "tree-gimple.h"
#include "tree-ssa-operands.h"

/* Forward declare structures for the garbage collector GTY markers.  */
#ifndef GCC_BASIC_BLOCK_H
struct edge_def;
typedef struct edge_def *edge;
struct basic_block_def;
typedef struct basic_block_def *basic_block;
#endif

/*---------------------------------------------------------------------------
		   Tree annotations stored in tree_common.ann
---------------------------------------------------------------------------*/
enum tree_ann_type { TREE_ANN_COMMON, VAR_ANN, STMT_ANN };

struct tree_ann_common_d GTY(())
{
  /* Annotation type.  */
  enum tree_ann_type type;

  /* The value handle for this expression.  Used by GVN-PRE.  */
  tree GTY((skip)) value_handle;
};

/* It is advantageous to avoid things like life analysis for variables which
   do not need PHI nodes.  This enum describes whether or not a particular
   variable may need a PHI node.  */

enum need_phi_state {
  /* This is the default.  If we are still in this state after finding
     all the definition and use sites, then we will assume the variable
     needs PHI nodes.  This is probably an overly conservative assumption.  */
  NEED_PHI_STATE_UNKNOWN,

  /* This state indicates that we have seen one or more sets of the 
     variable in a single basic block and that the sets dominate all
     uses seen so far.  If after finding all definition and use sites
     we are still in this state, then the variable does not need any
     PHI nodes.  */
  NEED_PHI_STATE_NO,

  /* This state indicates that we have either seen multiple definitions of
     the variable in multiple blocks, or that we encountered a use in a
     block that was not dominated by the block containing the set(s) of
     this variable.  This variable is assumed to need PHI nodes.  */
  NEED_PHI_STATE_MAYBE
};


/* When computing aliasing information, we represent the memory pointed-to
   by pointers with artificial variables called "memory tags" (MT).  There
   are two kinds of tags: type and name.  Type tags (TMT) are used in
   type-based alias analysis, they represent all the pointed-to locations
   and variables of the same alias set class.  Name tags (NMT) are used in
   flow-sensitive points-to alias analysis, they represent the variables
   and memory locations pointed-to by a specific SSA_NAME pointer.  */
enum mem_tag_kind {
  /* This variable is not a memory tag.  */
  NOT_A_TAG,

  /* This variable is a type memory tag (TMT).  */
  TYPE_TAG,

  /* This variable is a name memory tag (NMT).  */
  NAME_TAG
};

struct var_ann_d GTY(())
{
  struct tree_ann_common_d common;

  /* Nonzero if this variable has uses which may not appear
     in the IL.  This can happen in the following cases:

       1. If the variable is used in a variable length
          array declaration.

	2. If the variable is the return value in a C++
	   function where the named return value optimization
	   has been performed.  */
  unsigned has_hidden_use : 1;

  /* Used by the out of SSA pass to determine whether this variable has
     been seen yet or not.  */
  unsigned out_of_ssa_tag : 1;

  /* Used when building root_var structures in tree_ssa_live.[ch].  */
  unsigned root_var_processed : 1;

  /* If nonzero, this variable is a memory tag.  */
  ENUM_BITFIELD (mem_tag_kind) mem_tag_kind : 2;

  /* Nonzero if this variable is an alias tag that represents references to
     other variables (i.e., this variable appears in the MAY_ALIASES array
     of other variables).  */
  unsigned is_alias_tag : 1;

  /* Nonzero if this variable was used after SSA optimizations were
     applied.  We set this when translating out of SSA form.  */
  unsigned used : 1;

  /* This field indicates whether or not the variable may need PHI nodes.
     See the enum's definition for more detailed information about the
     states.  */
  ENUM_BITFIELD (need_phi_state) need_phi_state : 2;

  /* An artificial variable representing the memory location pointed-to by
     all the pointers that TBAA (type-based alias analysis) considers
     to be aliased.  If the variable is not a pointer or if it is never
     dereferenced, this must be NULL.  */
  tree type_mem_tag;

  /* Variables that may alias this variable.  */
  varray_type may_aliases;

  /* Unique ID of this variable.  */
  size_t uid;

  /* Used when going out of SSA form to indicate which partition this
     variable represents storage for.  */
  unsigned partition;

  /* Used by the root-var object in tree-ssa-live.[ch].  */
  unsigned root_index;

  /* Default definition for this symbol.  If this field is not NULL, it
     means that the first reference to this variable in the function is a
     USE or a VUSE.  In those cases, the SSA renamer creates an SSA name
     for this variable with an empty defining statement.  */
  tree default_def;

  /* During into-ssa and the dominator optimizer, this field holds the
     current version of this variable (an SSA_NAME). 

     This was previously two varrays (one in into-ssa the other in the
     dominator optimizer).  That is wasteful, particularly since the
     dominator optimizer calls into-ssa resulting in having two varrays
     live at the same time and this can happen for each call to the
     dominator optimizer.  */
  tree current_def;

  /* The set of expressions represented by this variable if it is a
     value handle.  This is used by GVN-PRE.  */
  PTR GTY ((skip)) expr_set;
  
};


struct dataflow_d GTY(())
{
  /* Immediate uses.  This is a list of all the statements and PHI nodes
     that are immediately reached by the definitions made in this
     statement.  */
  varray_type immediate_uses;

  /* Use this array for very small numbers of uses instead of the varray.  */
  tree uses[2];

  /* Reached uses.  This is a list of all the possible program statements
     that may be reached directly or indirectly by definitions made in this
     statement.  Notice that this is a superset of IMMEDIATE_USES.
     For instance, given the following piece of code:

	    1	a1 = 10;
	    2	if (a1 > 3)
	    3	  a2 = a1 + 5;
	    4	a3 = PHI (a1, a2)
	    5	b1 = a3 - 2;

     IMMEDIATE_USES for statement #1 are all those statements that use a1
     directly (i.e., #2, #3 and #4).  REACHED_USES for statement #1 also
     includes statement #5 because 'a1' could reach 'a3' via the PHI node
     at statement #4.  The set of REACHED_USES is then the transitive
     closure over all the PHI nodes in the IMMEDIATE_USES set.  */

  /* Reaching definitions.  Similarly to REACHED_USES, the set
     REACHING_DEFS is the set of all the statements that make definitions
     that may reach this statement.  Notice that we don't need to have a
     similar entry for immediate definitions, as these are represented by
     the SSA_NAME nodes themselves (each SSA_NAME node contains a pointer
     to the statement that makes that definition).  */
};

typedef struct dataflow_d *dataflow_t;


struct stmt_ann_d GTY(())
{
  struct tree_ann_common_d common;

  /* Nonzero if the statement has been modified (meaning that the operands
     need to be scanned again).  */
  unsigned modified : 1;

  /* Nonzero if the statement is in the CCP worklist and has not been
     "cancelled".  If we ever need to use this bit outside CCP, then
     it should be renamed.  */
  unsigned in_ccp_worklist: 1;

  /* Nonzero if the statement makes aliased loads.  */
  unsigned makes_aliased_loads : 1;

  /* Nonzero if the statement makes aliased stores.  */
  unsigned makes_aliased_stores : 1;

  /* Nonzero if the statement makes references to volatile storage.  */
  unsigned has_volatile_ops : 1;

  /* Nonzero if the statement makes a function call that may clobber global
     and local addressable variables.  */
  unsigned makes_clobbering_call : 1;

  /* Basic block that contains this statement.  */
  basic_block GTY ((skip (""))) bb;

  /* Statement operands.  */
  struct def_optype_d * GTY (()) def_ops;
  struct use_optype_d * GTY (()) use_ops;

  /* Virtual operands (V_MAY_DEF, VUSE, and V_MUST_DEF).  */
  struct v_may_def_optype_d * GTY (()) v_may_def_ops;
  struct vuse_optype_d * GTY (()) vuse_ops;
  struct v_must_def_optype_d * GTY (()) v_must_def_ops;

  /* Dataflow information.  */
  dataflow_t df;

  /* Set of variables that have had their address taken in the statement.  */
  bitmap addresses_taken;

  /* Unique identifier for this statement.  These ID's are to be created
     by each pass on an as-needed basis in any order convenient for the
     pass which needs statement UIDs.  */
  unsigned int uid;
};

union tree_ann_d GTY((desc ("ann_type ((tree_ann_t)&%h)")))
{
  struct tree_ann_common_d GTY((tag ("TREE_ANN_COMMON"))) common;
  struct var_ann_d GTY((tag ("VAR_ANN"))) decl;
  struct stmt_ann_d GTY((tag ("STMT_ANN"))) stmt;
};

typedef union tree_ann_d *tree_ann_t;
typedef struct var_ann_d *var_ann_t;
typedef struct stmt_ann_d *stmt_ann_t;

static inline tree_ann_t tree_ann (tree);
static inline tree_ann_t get_tree_ann (tree);
static inline var_ann_t var_ann (tree);
static inline var_ann_t get_var_ann (tree);
static inline stmt_ann_t stmt_ann (tree);
static inline stmt_ann_t get_stmt_ann (tree);
static inline enum tree_ann_type ann_type (tree_ann_t);
static inline basic_block bb_for_stmt (tree);
extern void set_bb_for_stmt (tree, basic_block);
static inline void modify_stmt (tree);
static inline void unmodify_stmt (tree);
static inline bool stmt_modified_p (tree);
static inline varray_type may_aliases (tree);
static inline int get_lineno (tree);
static inline const char *get_filename (tree);
static inline bool is_exec_stmt (tree);
static inline bool is_label_stmt (tree);
static inline v_may_def_optype get_v_may_def_ops (stmt_ann_t);
static inline vuse_optype get_vuse_ops (stmt_ann_t);
static inline use_optype get_use_ops (stmt_ann_t);
static inline def_optype get_def_ops (stmt_ann_t);
static inline bitmap addresses_taken (tree);
static inline int num_immediate_uses (dataflow_t);
static inline tree immediate_use (dataflow_t, int);
static inline dataflow_t get_immediate_uses (tree);
static inline bool has_hidden_use (tree);
static inline void set_has_hidden_use (tree);
static inline void set_default_def (tree, tree);
static inline tree default_def (tree);
static inline bool may_be_aliased (tree);

/*---------------------------------------------------------------------------
                  Structure representing predictions in tree level.
---------------------------------------------------------------------------*/
struct edge_prediction GTY((chain_next ("%h.next")))
{
  struct edge_prediction *next;
  edge edge;
  enum br_predictor predictor;
  int probability;
};

/*---------------------------------------------------------------------------
		  Block annotations stored in basic_block.tree_annotations
---------------------------------------------------------------------------*/
struct bb_ann_d GTY(())
{
  /* Chain of PHI nodes for this block.  */
  tree phi_nodes;

  /* Chain of EPHI nodes created in this block.  */
  tree ephi_nodes;
  
  /* Number of predecessors for this block.  This is only valid during
     SSA rewriting.  It is not maintained after conversion into SSA form.  */
  int num_preds;

  /* Nonzero if this block is forwardable during cfg cleanups.  This is also
     used to detect loops during cfg cleanups.  */
  unsigned forwardable: 1;

  /* Nonzero if this block contains an escape point (see is_escape_site).  */
  unsigned has_escape_site : 1;

  struct edge_prediction *predictions;
};

typedef struct bb_ann_d *bb_ann_t;

/* Accessors for basic block annotations.  */
static inline bb_ann_t bb_ann (basic_block);
static inline tree phi_nodes (basic_block);
static inline void set_phi_nodes (basic_block, tree);

/*---------------------------------------------------------------------------
			      Global declarations
---------------------------------------------------------------------------*/
/* Array of all variables referenced in the function.  */
extern GTY(()) varray_type referenced_vars;

#define num_referenced_vars VARRAY_ACTIVE_SIZE (referenced_vars)
#define referenced_var(i) VARRAY_TREE (referenced_vars, i)

/* Array of all SSA_NAMEs used in the function.  */
extern GTY(()) varray_type ssa_names;

#define num_ssa_names VARRAY_ACTIVE_SIZE (ssa_names)
#define ssa_name(i) VARRAY_TREE (ssa_names, i)

/* Artificial variable used to model the effects of function calls.  */
extern GTY(()) tree global_var;

/* Call clobbered variables in the function.  If bit I is set, then
   REFERENCED_VARS (I) is call-clobbered.  */
extern bitmap call_clobbered_vars;

/* 'true' after aliases have been computed (see compute_may_aliases).  */
extern bool aliases_computed_p;

/* Macros for showing usage statistics.  */
#define SCALE(x) ((unsigned long) ((x) < 1024*10	\
		  ? (x)					\
		  : ((x) < 1024*1024*10			\
		     ? (x) / 1024			\
		     : (x) / (1024*1024))))

#define LABEL(x) ((x) < 1024*10 ? 'b' : ((x) < 1024*1024*10 ? 'k' : 'M'))

#define PERCENT(x,y) ((float)(x) * 100.0 / (float)(y))


/*---------------------------------------------------------------------------
			      Block iterators
---------------------------------------------------------------------------*/

typedef struct {
  tree_stmt_iterator tsi;
  basic_block bb;
} block_stmt_iterator;

static inline block_stmt_iterator bsi_start (basic_block);
static inline block_stmt_iterator bsi_last (basic_block);
static inline block_stmt_iterator bsi_after_labels (basic_block);
static inline bool bsi_end_p (block_stmt_iterator);
static inline void bsi_next (block_stmt_iterator *);
static inline void bsi_prev (block_stmt_iterator *);
static inline tree bsi_stmt (block_stmt_iterator);
static inline tree * bsi_stmt_ptr (block_stmt_iterator);

extern void bsi_remove (block_stmt_iterator *);
extern void bsi_move_before (block_stmt_iterator *, block_stmt_iterator *);
extern void bsi_move_after (block_stmt_iterator *, block_stmt_iterator *);
extern void bsi_move_to_bb_end (block_stmt_iterator *, basic_block);

enum bsi_iterator_update
{
  /* Note that these are intentionally in the same order as TSI_FOO.  They
     mean exactly the same as their TSI_* counterparts.  */
  BSI_NEW_STMT,
  BSI_SAME_STMT,
  BSI_CHAIN_START,
  BSI_CHAIN_END,
  BSI_CONTINUE_LINKING
};

extern void bsi_insert_before (block_stmt_iterator *, tree,
			       enum bsi_iterator_update);
extern void bsi_insert_after (block_stmt_iterator *, tree,
			      enum bsi_iterator_update);

extern void bsi_replace (const block_stmt_iterator *, tree, bool);

/*---------------------------------------------------------------------------
			      Function prototypes
---------------------------------------------------------------------------*/
/* In tree-cfg.c  */

/* Location to track pending stmt for edge insertion.  */
#define PENDING_STMT(e)	((e)->insns.t)

extern void delete_tree_cfg (void);
extern void disband_implicit_edges (void);
extern bool stmt_ends_bb_p (tree);
extern bool is_ctrl_stmt (tree);
extern bool is_ctrl_altering_stmt (tree);
extern bool computed_goto_p (tree);
extern bool simple_goto_p (tree);
extern void tree_dump_bb (basic_block, FILE *, int);
extern void debug_tree_bb (basic_block);
extern basic_block debug_tree_bb_n (int);
extern void dump_tree_cfg (FILE *, int);
extern void debug_tree_cfg (int);
extern void dump_cfg_stats (FILE *);
extern void debug_cfg_stats (void);
extern void debug_loop_ir (void);
extern void print_loop_ir (FILE *);
extern void cleanup_tree_cfg (void);
extern tree first_stmt (basic_block);
extern tree last_stmt (basic_block);
extern tree *last_stmt_ptr (basic_block);
extern tree last_and_only_stmt (basic_block);
extern edge find_taken_edge (basic_block, tree);
extern void cfg_remove_useless_stmts (void);
extern edge thread_edge (edge, basic_block);
extern basic_block label_to_block (tree);
extern void tree_optimize_tail_calls (bool, enum tree_dump_index);
extern edge tree_block_forwards_to (basic_block bb);
extern void bsi_insert_on_edge (edge, tree);
extern void bsi_commit_edge_inserts (int *);
extern void notice_special_calls (tree);
extern void clear_special_calls (void);
extern void compute_dominance_frontiers (bitmap *);
extern void verify_stmts (void);
extern tree tree_block_label (basic_block bb);
extern void extract_true_false_edges_from_block (basic_block, edge *, edge *);

/* In tree-pretty-print.c.  */
extern void dump_generic_bb (FILE *, basic_block, int, int);

/* In tree-dfa.c  */
extern var_ann_t create_var_ann (tree);
extern stmt_ann_t create_stmt_ann (tree);
extern tree_ann_t create_tree_ann (tree);
extern tree create_phi_node (tree, basic_block);
extern void add_phi_arg (tree *, tree, edge);
extern void remove_phi_arg (tree, basic_block);
extern void remove_phi_arg_num (tree, int);
extern void remove_phi_node (tree, tree, basic_block);
extern void remove_all_phi_nodes_for (bitmap);
extern void dump_dfa_stats (FILE *);
extern void debug_dfa_stats (void);
extern void debug_referenced_vars (void);
extern void dump_referenced_vars (FILE *);
extern void dump_variable (FILE *, tree);
extern void debug_variable (tree);
extern void dump_immediate_uses (FILE *);
extern void debug_immediate_uses (void);
extern void dump_immediate_uses_for (FILE *, tree);
extern void debug_immediate_uses_for (tree);
extern void compute_immediate_uses (int, bool (*)(tree));
extern void free_df (void);
extern tree get_virtual_var (tree);
extern void add_referenced_tmp_var (tree var);
extern void mark_new_vars_to_rename (tree, bitmap);
extern void redirect_immediate_uses (tree, tree);
extern tree make_rename_temp (tree, const char *);

/* Flags used when computing reaching definitions and reached uses.  */
#define TDFA_USE_OPS		1 << 0
#define TDFA_USE_VOPS		1 << 1

/* In gimple-low.c  */
struct lower_data;
extern void lower_stmt_body (tree, struct lower_data *);
extern void expand_used_vars (void);
extern void record_vars (tree);
extern bool block_may_fallthru (tree block);

/* In tree-ssa-alias.c  */
extern void dump_may_aliases_for (FILE *, tree);
extern void debug_may_aliases_for (tree);
extern void dump_alias_info (FILE *);
extern void debug_alias_info (void);
extern void dump_points_to_info (FILE *);
extern void debug_points_to_info (void);

/* Call-back function for walk_use_def_chains().  At each reaching
   definition, a function with this prototype is called.  */
typedef bool (*walk_use_def_chains_fn) (tree, tree, void *);

/* In tree-ssa.c  */
extern void init_tree_ssa (void);
extern void rewrite_vars_out_of_ssa (bitmap);
extern void dump_reaching_defs (FILE *);
extern void debug_reaching_defs (void);
extern void dump_tree_ssa (FILE *);
extern void debug_tree_ssa (void);
extern void debug_def_blocks (void);
extern void dump_tree_ssa_stats (FILE *);
extern void debug_tree_ssa_stats (void);
extern void ssa_remove_edge (edge);
extern edge ssa_redirect_edge (edge, basic_block);
extern void set_is_used (tree);
extern bool tree_ssa_useless_type_conversion (tree);
extern bool tree_ssa_useless_type_conversion_1 (tree, tree);
extern void verify_ssa (void);
extern void delete_tree_ssa (void);
extern void register_new_def (tree, varray_type *);
extern void walk_use_def_chains (tree, walk_use_def_chains_fn, void *);

/* In tree-into-ssa.c  */
extern void rewrite_into_ssa (void);

/* In tree-ssa-ccp.c  */
bool fold_stmt (tree *);
tree widen_bitfield (tree, tree, tree);

/* In tree-ssa-dom.c  */
extern void dump_dominator_optimization_stats (FILE *);
extern void debug_dominator_optimization_stats (void);

/* In tree-ssa-copy.c  */
extern void propagate_value (use_operand_p, tree);
extern void propagate_tree_value (tree *, tree);
extern void replace_exp (use_operand_p, tree);
extern bool cprop_into_stmt (tree, varray_type);
extern void cprop_into_successor_phis (basic_block, varray_type, bitmap);

/* In tree-flow-inline.h  */
static inline int phi_arg_from_edge (tree, edge);
static inline bool may_propagate_copy (tree, tree);
static inline bool is_call_clobbered (tree);
static inline void mark_call_clobbered (tree);

/* In tree-eh.c  */
extern void make_eh_edges (tree);
extern bool tree_could_trap_p (tree);
extern bool tree_could_throw_p (tree);
extern bool tree_can_throw_internal (tree);
extern bool tree_can_throw_external (tree);
extern void add_stmt_to_eh_region (tree, int);

/* In tree-ssa-pre.c */
tree get_value_handle (tree);
void set_value_handle (tree, tree);
void debug_value_expressions (tree);
void print_value_expressions (FILE *, tree);

#include "tree-flow-inline.h"

#endif /* _TREE_FLOW_H  */
