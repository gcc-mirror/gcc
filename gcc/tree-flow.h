/* Data and Control Flow Analysis for Trees.
   Copyright (C) 2001, 2003, 2004, 2005, 2006, 2007
   Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef _TREE_FLOW_H
#define _TREE_FLOW_H 1

#include "bitmap.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "hashtab.h"
#include "tree-gimple.h"
#include "tree-ssa-operands.h"
#include "cgraph.h"
#include "ipa-reference.h"

/* Forward declare structures for the garbage collector GTY markers.  */
#ifndef GCC_BASIC_BLOCK_H
struct edge_def;
typedef struct edge_def *edge;
struct basic_block_def;
typedef struct basic_block_def *basic_block;
#endif
struct static_var_ann_d;

/* The reasons a variable may escape a function.  */
enum escape_type 
{
  NO_ESCAPE = 0,			/* Doesn't escape.  */
  ESCAPE_STORED_IN_GLOBAL = 1 << 0,
  ESCAPE_TO_ASM = 1 << 1,		/* Passed by address to an assembly
					   statement.  */
  ESCAPE_TO_CALL = 1 << 2,		/* Escapes to a function call.  */
  ESCAPE_BAD_CAST = 1 << 3,		/* Cast from pointer to integer */
  ESCAPE_TO_RETURN = 1 << 4,		/* Returned from function.  */
  ESCAPE_TO_PURE_CONST = 1 << 5,	/* Escapes to a pure or constant
					   function call.  */
  ESCAPE_IS_GLOBAL = 1 << 6,		/* Is a global variable.  */
  ESCAPE_IS_PARM = 1 << 7,		/* Is an incoming function argument.  */
  ESCAPE_UNKNOWN = 1 << 8		/* We believe it escapes for
					   some reason not enumerated
					   above.  */
};

/* Memory reference statistics for individual memory symbols,
   collected during alias analysis.  */
struct mem_sym_stats_d GTY(())
{
  /* Memory symbol.  */
  tree var;

  /* Nonzero if this entry has been assigned a partition.  */
  unsigned int partitioned_p : 1;

  /* Nonzero if VAR is a memory partition tag that already contains
     call-clobbered variables in its partition set.  */
  unsigned int has_call_clobbered_vars : 1;

  /* Number of direct reference sites.  A direct reference to VAR is any
     reference of the form 'VAR = ' or ' = VAR'.  For GIMPLE reg
     pointers, this is the number of sites where the pointer is
     dereferenced.  */
  long num_direct_writes;
  long num_direct_reads;

  /* Number of indirect reference sites.  An indirect reference to VAR
     is any reference via a pointer that contains VAR in its points-to
     set or, in the case of call-clobbered symbols, a function call.  */
  long num_indirect_writes;
  long num_indirect_reads;

  /* Execution frequency.  This is the sum of the execution
     frequencies of all the statements that reference this object
     weighted by the number of references in each statement.  This is
     the main key used to sort the list of symbols to partition.
     Symbols with high execution frequencies are put at the bottom of
     the work list (ie, they are partitioned last).
     Execution frequencies are taken directly from each basic block,
     so compiling with PGO enabled will increase the precision of this
     estimate.  */
  long frequency_reads;
  long frequency_writes;

  /* Set of memory tags that contain VAR in their alias set.  */
  bitmap parent_tags;
};

typedef struct mem_sym_stats_d *mem_sym_stats_t;
DEF_VEC_P(mem_sym_stats_t);
DEF_VEC_ALLOC_P(mem_sym_stats_t, heap);

/* Memory reference statistics collected during alias analysis.  */
struct mem_ref_stats_d GTY(())
{
  /* Number of statements that make memory references.  */
  long num_mem_stmts;

  /* Number of statements that make function calls.  */
  long num_call_sites;

  /* Number of statements that make calls to pure/const functions.  */
  long num_pure_const_call_sites;

  /* Number of ASM statements.  */
  long num_asm_sites;

  /* Estimated number of virtual operands needed as computed by
   compute_memory_partitions.  */
  long num_vuses;
  long num_vdefs;

  /* This maps every symbol used to make "memory" references
     (pointers, arrays, structures, etc) to an instance of struct
     mem_sym_stats_d describing reference statistics for the symbol.  */
  struct pointer_map_t * GTY((skip)) mem_sym_stats;
};


/* Gimple dataflow datastructure. All publicly available fields shall have
   gimple_ accessor defined in tree-flow-inline.h, all publicly modifiable
   fields should have gimple_set accessor.  */
struct gimple_df GTY(())
{
  /* Array of all variables referenced in the function.  */
  htab_t GTY((param_is (union tree_node))) referenced_vars;

  /* A list of all the noreturn calls passed to modify_stmt.
     cleanup_control_flow uses it to detect cases where a mid-block
     indirect call has been turned into a noreturn call.  When this
     happens, all the instructions after the call are no longer
     reachable and must be deleted as dead.  */
  VEC(tree,gc) *modified_noreturn_calls;

  /* Array of all SSA_NAMEs used in the function.  */
  VEC(tree,gc) *ssa_names;

  /* Artificial variable used to model the effects of function calls.  */
  tree global_var;

  /* Artificial variable used to model the effects of nonlocal
     variables.  */
  tree nonlocal_all;

  /* Call clobbered variables in the function.  If bit I is set, then
     REFERENCED_VARS (I) is call-clobbered.  */
  bitmap call_clobbered_vars;

  /* Addressable variables in the function.  If bit I is set, then
     REFERENCED_VARS (I) has had its address taken.  Note that
     CALL_CLOBBERED_VARS and ADDRESSABLE_VARS are not related.  An
     addressable variable is not necessarily call-clobbered (e.g., a
     local addressable whose address does not escape) and not all
     call-clobbered variables are addressable (e.g., a local static
     variable).  */
  bitmap addressable_vars;

  /* Free list of SSA_NAMEs.  */
  tree free_ssanames;

  /* Hashtable holding definition for symbol.  If this field is not NULL, it
     means that the first reference to this variable in the function is a
     USE or a VUSE.  In those cases, the SSA renamer creates an SSA name
     for this variable with an empty defining statement.  */
  htab_t GTY((param_is (union tree_node))) default_defs;

  /* 'true' after aliases have been computed (see compute_may_aliases).  */
  unsigned int aliases_computed_p : 1;

  /* True if the code is in ssa form.  */
  unsigned int in_ssa_p : 1;

  struct ssa_operands ssa_operands;

  /* Memory reference statistics collected during alias analysis.
     This information is used to drive the memory partitioning
     heuristics in compute_memory_partitions.  */
  struct mem_ref_stats_d mem_ref_stats;
};

/* Accessors for internal use only.  Generic code should use abstraction
   provided by tree-flow-inline.h or specific modules.  */
#define FREE_SSANAMES(fun) (fun)->gimple_df->free_ssanames
#define SSANAMES(fun) (fun)->gimple_df->ssa_names
#define MODIFIED_NORETURN_CALLS(fun) (fun)->gimple_df->modified_noreturn_calls
#define DEFAULT_DEFS(fun) (fun)->gimple_df->default_defs

typedef struct
{
  htab_t htab;
  PTR *slot;
  PTR *limit;
} htab_iterator;

/* Iterate through the elements of hashtable HTAB, using htab_iterator ITER,
   storing each element in RESULT, which is of type TYPE.  */
#define FOR_EACH_HTAB_ELEMENT(HTAB, RESULT, TYPE, ITER) \
  for (RESULT = (TYPE) first_htab_element (&(ITER), (HTAB)); \
	!end_htab_p (&(ITER)); \
	RESULT = (TYPE) next_htab_element (&(ITER)))

/*---------------------------------------------------------------------------
		      Attributes for SSA_NAMEs.
  
  NOTE: These structures are stored in struct tree_ssa_name
  but are only used by the tree optimizers, so it makes better sense
  to declare them here to avoid recompiling unrelated files when
  making changes.
---------------------------------------------------------------------------*/

/* Aliasing information for SSA_NAMEs representing pointer variables.  */
struct ptr_info_def GTY(())
{
  /* Nonzero if points-to analysis couldn't determine where this pointer
     is pointing to.  */
  unsigned int pt_anything : 1;

  /* Nonzero if the value of this pointer escapes the current function.  */
  unsigned int value_escapes_p : 1;

  /* Nonzero if this pointer is dereferenced.  */
  unsigned int is_dereferenced : 1;

  /* Nonzero if this pointer points to a global variable.  */
  unsigned int pt_global_mem : 1;

  /* Nonzero if this pointer points to NULL.  */
  unsigned int pt_null : 1;

  /* Mask of reasons this pointer's value escapes the function  */
  ENUM_BITFIELD (escape_type) escape_mask : 9;

  /* Set of variables that this pointer may point to.  */
  bitmap pt_vars;

  /* If this pointer has been dereferenced, and points-to information is
     more precise than type-based aliasing, indirect references to this
     pointer will be represented by this memory tag, instead of the type
     tag computed by TBAA.  */
  tree name_mem_tag;
};


/*---------------------------------------------------------------------------
		   Tree annotations stored in tree_base.ann
---------------------------------------------------------------------------*/
enum tree_ann_type { TREE_ANN_COMMON, VAR_ANN, FUNCTION_ANN, STMT_ANN };

struct tree_ann_common_d GTY(())
{
  /* Annotation type.  */
  enum tree_ann_type type;

 /* Auxiliary info specific to a pass.  At all times, this
    should either point to valid data or be NULL.  */ 
  PTR GTY ((skip (""))) aux; 

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


/* The "no alias" attribute allows alias analysis to make more
   aggressive assumptions when assigning alias sets, computing
   points-to information and memory partitions.  These attributes
   are the result of user annotations or flags (e.g.,
   -fargument-noalias).  */
enum noalias_state {
    /* Default state.  No special assumptions can be made about this
       symbol.  */
    MAY_ALIAS = 0,

    /* The symbol does not alias with other symbols that have a
       NO_ALIAS* attribute.  */
    NO_ALIAS,

    /* The symbol does not alias with other symbols that have a
       NO_ALIAS*, and it may not alias with global symbols.  */
    NO_ALIAS_GLOBAL,

    /* The symbol does not alias with any other symbols.  */
    NO_ALIAS_ANYTHING
};


typedef VEC(tree,gc) *subvar_t;

struct var_ann_d GTY(())
{
  struct tree_ann_common_d common;

  /* Used by the out of SSA pass to determine whether this variable has
     been seen yet or not.  */
  unsigned out_of_ssa_tag : 1;

  /* Used when building base variable structures in a var_map.  */
  unsigned base_var_processed : 1;

  /* Nonzero if this variable was used after SSA optimizations were
     applied.  We set this when translating out of SSA form.  */
  unsigned used : 1;

  /* This field indicates whether or not the variable may need PHI nodes.
     See the enum's definition for more detailed information about the
     states.  */
  ENUM_BITFIELD (need_phi_state) need_phi_state : 2;

  /* Used during operand processing to determine if this variable is already 
     in the VUSE list.  */
  unsigned in_vuse_list : 1;

  /* Used during operand processing to determine if this variable is already 
     in the VDEF list.  */
  unsigned in_vdef_list : 1;

  /* True for HEAP artificial variables.  These variables represent
     the memory area allocated by a call to malloc.  */
  unsigned is_heapvar : 1;

  /* True if the variable is call clobbered.  */
  unsigned call_clobbered : 1;

  /* This field describes several "no alias" attributes that some
     symbols are known to have.  See the enum's definition for more
     information on each attribute.  */
  ENUM_BITFIELD (noalias_state) noalias_state : 2;

  /* Mask of values saying the reasons why this variable has escaped
     the function.  */
  ENUM_BITFIELD (escape_type) escape_mask : 9;

  /* Memory partition tag assigned to this symbol.  */
  tree mpt;

  /* If this variable is a pointer P that has been dereferenced, this
     field is an artificial variable that represents the memory
     location *P.  Every other pointer Q that is type-compatible with
     P will also have the same memory tag.  If the variable is not a
     pointer or if it is never dereferenced, this must be NULL.
     FIXME, do we really need this here?  How much slower would it be
     to convert to hash table?  */
  tree symbol_mem_tag;

  /* Used when going out of SSA form to indicate which partition this
     variable represents storage for.  */
  unsigned partition;

  /* Used by var_map for the base index of ssa base variables.  */
  unsigned base_index;

  /* During into-ssa and the dominator optimizer, this field holds the
     current version of this variable (an SSA_NAME).  */
  tree current_def;

  /* If this variable is a structure, this fields holds an array
     of symbols representing each of the fields of the structure.  */
  VEC(tree,gc) *subvars;
};

/* Container for variable annotation used by hashtable for annotations for
   static variables.  */
struct static_var_ann_d GTY(())
{
  struct var_ann_d ann;
  unsigned int uid;
};

struct function_ann_d GTY(())
{
  struct tree_ann_common_d common;

  /* Pointer to the structure that contains the sets of global
     variables modified by function calls.  This field is only used
     for FUNCTION_DECLs.  */
  ipa_reference_vars_info_t GTY ((skip)) reference_vars_info;
};

typedef struct immediate_use_iterator_d
{
  /* This is the current use the iterator is processing.  */
  ssa_use_operand_t *imm_use;
  /* This marks the last use in the list (use node from SSA_NAME)  */
  ssa_use_operand_t *end_p;
  /* This node is inserted and used to mark the end of the uses for a stmt.  */
  ssa_use_operand_t iter_node;
  /* This is the next ssa_name to visit.  IMM_USE may get removed before
     the next one is traversed to, so it must be cached early.  */
  ssa_use_operand_t *next_imm_name;
} imm_use_iterator;


/* Use this iterator when simply looking at stmts.  Adding, deleting or
   modifying stmts will cause this iterator to malfunction.  */

#define FOR_EACH_IMM_USE_FAST(DEST, ITER, SSAVAR)			\
  for ((DEST) = first_readonly_imm_use (&(ITER), (SSAVAR));	\
       !end_readonly_imm_use_p (&(ITER));			\
       (DEST) = next_readonly_imm_use (&(ITER)))
  
/* Use this iterator to visit each stmt which has a use of SSAVAR.  */

#define FOR_EACH_IMM_USE_STMT(STMT, ITER, SSAVAR)		\
  for ((STMT) = first_imm_use_stmt (&(ITER), (SSAVAR));		\
       !end_imm_use_stmt_p (&(ITER));				\
       (STMT) = next_imm_use_stmt (&(ITER)))

/* Use this to terminate the FOR_EACH_IMM_USE_STMT loop early.  Failure to 
   do so will result in leaving a iterator marker node in the immediate
   use list, and nothing good will come from that.   */
#define BREAK_FROM_IMM_USE_STMT(ITER)				\
   {								\
     end_imm_use_stmt_traverse (&(ITER));			\
     break;							\
   }


/* Use this iterator in combination with FOR_EACH_IMM_USE_STMT to 
   get access to each occurrence of ssavar on the stmt returned by
   that iterator..  for instance:

     FOR_EACH_IMM_USE_STMT (stmt, iter, var)
       {
         FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
	   {
	     SET_USE (use_p) = blah;
	   }
	 update_stmt (stmt);
       }							 */

#define FOR_EACH_IMM_USE_ON_STMT(DEST, ITER)			\
  for ((DEST) = first_imm_use_on_stmt (&(ITER));		\
       !end_imm_use_on_stmt_p (&(ITER));			\
       (DEST) = next_imm_use_on_stmt (&(ITER)))



struct stmt_ann_d GTY(())
{
  struct tree_ann_common_d common;

  /* Basic block that contains this statement.  */
  basic_block bb;

  /* Operand cache for stmt.  */
  struct stmt_operands_d GTY ((skip (""))) operands;

  /* Set of variables that have had their address taken in the statement.  */
  bitmap addresses_taken;

  /* Unique identifier for this statement.  These ID's are to be created
     by each pass on an as-needed basis in any order convenient for the
     pass which needs statement UIDs.  */
  unsigned int uid;

  /* Nonzero if the statement references memory (at least one of its
     expressions contains a non-register operand).  */
  unsigned references_memory : 1;

  /* Nonzero if the statement has been modified (meaning that the operands
     need to be scanned again).  */
  unsigned modified : 1;

  /* Nonzero if the statement makes references to volatile storage.  */
  unsigned has_volatile_ops : 1;
};

union tree_ann_d GTY((desc ("ann_type ((tree_ann_t)&%h)")))
{
  struct tree_ann_common_d GTY((tag ("TREE_ANN_COMMON"))) common;
  struct var_ann_d GTY((tag ("VAR_ANN"))) vdecl;
  struct function_ann_d GTY((tag ("FUNCTION_ANN"))) fdecl;
  struct stmt_ann_d GTY((tag ("STMT_ANN"))) stmt;
};

typedef union tree_ann_d *tree_ann_t;
typedef struct var_ann_d *var_ann_t;
typedef struct function_ann_d *function_ann_t;
typedef struct stmt_ann_d *stmt_ann_t;
typedef struct tree_ann_common_d *tree_ann_common_t;

static inline tree_ann_common_t tree_common_ann (const_tree);
static inline tree_ann_common_t get_tree_common_ann (tree);
static inline var_ann_t var_ann (const_tree);
static inline var_ann_t get_var_ann (tree);
static inline function_ann_t function_ann (const_tree);
static inline function_ann_t get_function_ann (tree);
static inline stmt_ann_t stmt_ann (tree);
static inline bool has_stmt_ann (tree);
static inline stmt_ann_t get_stmt_ann (tree);
static inline enum tree_ann_type ann_type (tree_ann_t);
static inline basic_block bb_for_stmt (tree);
extern void set_bb_for_stmt (tree, basic_block);
static inline bool noreturn_call_p (tree);
static inline void update_stmt (tree);
static inline bool stmt_modified_p (tree);
static inline bitmap may_aliases (const_tree);
static inline int get_lineno (const_tree);
static inline bitmap addresses_taken (tree);

/*---------------------------------------------------------------------------
                  Structure representing predictions in tree level.
---------------------------------------------------------------------------*/
struct edge_prediction GTY((chain_next ("%h.ep_next")))
{
  struct edge_prediction *ep_next;
  edge ep_edge;
  enum br_predictor ep_predictor;
  int ep_probability;
};

/* Accessors for basic block annotations.  */
static inline tree phi_nodes (const_basic_block);
static inline void set_phi_nodes (basic_block, tree);

/*---------------------------------------------------------------------------
			      Global declarations
---------------------------------------------------------------------------*/
struct int_tree_map GTY(())
{
  
  unsigned int uid;
  tree to;
};

extern unsigned int int_tree_map_hash (const void *);
extern int int_tree_map_eq (const void *, const void *);

extern unsigned int uid_decl_map_hash (const void *);
extern int uid_decl_map_eq (const void *, const void *);

typedef struct 
{
  htab_iterator hti;
} referenced_var_iterator;


/* This macro loops over all the referenced vars, one at a time, putting the
   current var in VAR.  Note:  You are not allowed to add referenced variables
   to the hashtable while using this macro.  Doing so may cause it to behave
   erratically.  */

#define FOR_EACH_REFERENCED_VAR(VAR, ITER) \
  for ((VAR) = first_referenced_var (&(ITER)); \
       !end_referenced_vars_p (&(ITER)); \
       (VAR) = next_referenced_var (&(ITER))) 


typedef struct
{
  int i;
} safe_referenced_var_iterator;

/* This macro loops over all the referenced vars, one at a time, putting the
   current var in VAR.  You are allowed to add referenced variables during the
   execution of this macro, however, the macro will not iterate over them.  It
   requires a temporary vector of trees, VEC, whose lifetime is controlled by
   the caller.  The purpose of the vector is to temporarily store the
   referenced_variables hashtable so that adding referenced variables does not
   affect the hashtable.  */

#define FOR_EACH_REFERENCED_VAR_SAFE(VAR, VEC, ITER) \
  for ((ITER).i = 0, fill_referenced_var_vec (&(VEC)); \
       VEC_iterate (tree, (VEC), (ITER).i, (VAR)); \
       (ITER).i++)

extern tree referenced_var_lookup (unsigned int);
extern bool referenced_var_check_and_insert (tree);
#define num_referenced_vars htab_elements (gimple_referenced_vars (cfun))
#define referenced_var(i) referenced_var_lookup (i)

#define num_ssa_names (VEC_length (tree, cfun->gimple_df->ssa_names))
#define ssa_name(i) (VEC_index (tree, cfun->gimple_df->ssa_names, (i)))

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
block_stmt_iterator bsi_for_stmt (tree);
static inline bool bsi_end_p (block_stmt_iterator);
static inline void bsi_next (block_stmt_iterator *);
static inline void bsi_prev (block_stmt_iterator *);
static inline tree bsi_stmt (block_stmt_iterator);
static inline tree * bsi_stmt_ptr (block_stmt_iterator);

extern void bsi_remove (block_stmt_iterator *, bool);
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
			      OpenMP Region Tree
---------------------------------------------------------------------------*/

/* Parallel region information.  Every parallel and workshare
   directive is enclosed between two markers, the OMP_* directive
   and a corresponding OMP_RETURN statement.  */

struct omp_region
{
  /* The enclosing region.  */
  struct omp_region *outer;

  /* First child region.  */
  struct omp_region *inner;

  /* Next peer region.  */
  struct omp_region *next;

  /* Block containing the omp directive as its last stmt.  */
  basic_block entry;

  /* Block containing the OMP_RETURN as its last stmt.  */
  basic_block exit;

  /* Block containing the OMP_CONTINUE as its last stmt.  */
  basic_block cont;

  /* If this is a combined parallel+workshare region, this is a list
     of additional arguments needed by the combined parallel+workshare
     library call.  */
  tree ws_args;

  /* The code for the omp directive of this region.  */
  enum tree_code type;

  /* Schedule kind, only used for OMP_FOR type regions.  */
  enum omp_clause_schedule_kind sched_kind;

  /* True if this is a combined parallel+workshare region.  */
  bool is_combined_parallel;
};

extern struct omp_region *root_omp_region;
extern struct omp_region *new_omp_region (basic_block, enum tree_code,
					  struct omp_region *);
extern void free_omp_regions (void);
void omp_expand_local (basic_block);
extern tree find_omp_clause (tree, enum tree_code);
tree copy_var_decl (tree, tree, tree);

/*---------------------------------------------------------------------------
			      Function prototypes
---------------------------------------------------------------------------*/
/* In tree-cfg.c  */

/* Location to track pending stmt for edge insertion.  */
#define PENDING_STMT(e)	((e)->insns.t)

extern void delete_tree_cfg_annotations (void);
extern bool stmt_ends_bb_p (const_tree);
extern bool is_ctrl_stmt (const_tree);
extern bool is_ctrl_altering_stmt (const_tree);
extern bool computed_goto_p (const_tree);
extern bool simple_goto_p (const_tree);
extern bool tree_can_make_abnormal_goto (const_tree);
extern basic_block single_noncomplex_succ (basic_block bb);
extern void tree_dump_bb (basic_block, FILE *, int);
extern void debug_tree_bb (basic_block);
extern basic_block debug_tree_bb_n (int);
extern void dump_tree_cfg (FILE *, int);
extern void debug_tree_cfg (int);
extern void dump_cfg_stats (FILE *);
extern void dot_cfg (void);
extern void debug_cfg_stats (void);
extern void debug_loops (int);
extern void debug_loop (struct loop *, int);
extern void debug_loop_num (unsigned, int);
extern void print_loops (FILE *, int);
extern void print_loops_bb (FILE *, basic_block, int, int);
extern void cleanup_dead_labels (void);
extern void group_case_labels (void);
extern tree first_stmt (basic_block);
extern tree last_stmt (basic_block);
extern tree last_and_only_stmt (basic_block);
extern edge find_taken_edge (basic_block, tree);
extern basic_block label_to_block_fn (struct function *, tree);
#define label_to_block(t) (label_to_block_fn (cfun, t))
extern void bsi_insert_on_edge (edge, tree);
extern basic_block bsi_insert_on_edge_immediate (edge, tree);
extern void bsi_commit_one_edge_insert (edge, basic_block *);
extern void bsi_commit_edge_inserts (void);
extern void notice_special_calls (tree);
extern void clear_special_calls (void);
extern void verify_stmts (void);
extern void verify_gimple (void);
extern void verify_gimple_1 (tree);
extern tree tree_block_label (basic_block);
extern void extract_true_false_edges_from_block (basic_block, edge *, edge *);
extern bool tree_duplicate_sese_region (edge, edge, basic_block *, unsigned,
					basic_block *);
extern bool tree_duplicate_sese_tail (edge, edge, basic_block *, unsigned,
				      basic_block *);
extern void add_phi_args_after_copy_bb (basic_block);
extern void add_phi_args_after_copy (basic_block *, unsigned, edge);
extern bool tree_purge_dead_abnormal_call_edges (basic_block);
extern bool tree_purge_dead_eh_edges (basic_block);
extern bool tree_purge_all_dead_eh_edges (const_bitmap);
extern tree gimplify_val (block_stmt_iterator *, tree, tree);
extern tree gimplify_build1 (block_stmt_iterator *, enum tree_code,
			     tree, tree);
extern tree gimplify_build2 (block_stmt_iterator *, enum tree_code,
			     tree, tree, tree);
extern tree gimplify_build3 (block_stmt_iterator *, enum tree_code,
			     tree, tree, tree, tree);
extern void init_empty_tree_cfg (void);
extern void fold_cond_expr_cond (void);
extern void make_abnormal_goto_edges (basic_block, bool);
extern void replace_uses_by (tree, tree);
extern void start_recording_case_labels (void);
extern void end_recording_case_labels (void);
extern basic_block move_sese_region_to_fn (struct function *, basic_block,
				           basic_block);
void remove_edge_and_dominated_blocks (edge);

/* In tree-cfgcleanup.c  */
extern bitmap cfgcleanup_altered_bbs;
extern bool cleanup_tree_cfg (void);

/* In tree-pretty-print.c.  */
extern void dump_generic_bb (FILE *, basic_block, int, int);
extern const char *op_symbol_code (enum tree_code);

/* In tree-dfa.c  */
extern var_ann_t create_var_ann (tree);
extern function_ann_t create_function_ann (tree);
extern stmt_ann_t create_stmt_ann (tree);
extern tree_ann_common_t create_tree_common_ann (tree);
extern void dump_dfa_stats (FILE *);
extern void debug_dfa_stats (void);
extern void debug_referenced_vars (void);
extern void dump_referenced_vars (FILE *);
extern void dump_variable (FILE *, tree);
extern void debug_variable (tree);
extern void dump_subvars_for (FILE *, tree);
extern void debug_subvars_for (tree);
extern tree get_virtual_var (tree);
extern void add_referenced_var (tree);
extern void remove_referenced_var (tree);
extern void mark_symbols_for_renaming (tree);
extern void find_new_referenced_vars (tree *);
extern tree make_rename_temp (tree, const char *);
extern void set_default_def (tree, tree);
extern tree gimple_default_def (struct function *, tree);
extern bool stmt_references_abnormal_ssa_name (tree);

/* In tree-phinodes.c  */
extern void reserve_phi_args_for_new_edge (basic_block);
extern tree create_phi_node (tree, basic_block);
extern void add_phi_arg (tree, tree, edge);
extern void remove_phi_args (edge);
extern void remove_phi_node (tree, tree, bool);
extern tree phi_reverse (tree);

/* In gimple-low.c  */
extern void record_vars_into (tree, tree);
extern void record_vars (tree);
extern bool block_may_fallthru (const_tree);

/* In tree-ssa-alias.c  */
extern unsigned int compute_may_aliases (void);
extern void dump_may_aliases_for (FILE *, tree);
extern void debug_may_aliases_for (tree);
extern void dump_alias_info (FILE *);
extern void debug_alias_info (void);
extern void dump_points_to_info (FILE *);
extern void debug_points_to_info (void);
extern void dump_points_to_info_for (FILE *, tree);
extern void debug_points_to_info_for (tree);
extern bool may_be_aliased (tree);
extern struct ptr_info_def *get_ptr_info (tree);
extern void new_type_alias (tree, tree, tree);
extern void count_uses_and_derefs (tree, tree, unsigned *, unsigned *,
				   unsigned *);
static inline subvar_t get_subvars_for_var (tree);
static inline tree get_subvar_at (tree, unsigned HOST_WIDE_INT);
static inline bool ref_contains_array_ref (const_tree);
static inline bool array_ref_contains_indirect_ref (const_tree);
extern tree get_ref_base_and_extent (tree, HOST_WIDE_INT *,
				     HOST_WIDE_INT *, HOST_WIDE_INT *);
static inline bool var_can_have_subvars (const_tree);
static inline bool overlap_subvar (unsigned HOST_WIDE_INT,
				   unsigned HOST_WIDE_INT,
				   const_tree, bool *);
extern tree create_tag_raw (enum tree_code, tree, const char *);
extern void delete_mem_ref_stats (struct function *);
extern void dump_mem_ref_stats (FILE *);
extern void debug_mem_ref_stats (void);
extern void debug_memory_partitions (void);
extern void debug_mem_sym_stats (tree var);
extern void dump_mem_sym_stats_for_var (FILE *, tree);
extern void debug_all_mem_sym_stats (void);

/* Call-back function for walk_use_def_chains().  At each reaching
   definition, a function with this prototype is called.  */
typedef bool (*walk_use_def_chains_fn) (tree, tree, void *);

/* In tree-ssa-alias-warnings.c  */
extern void strict_aliasing_warning_backend (void);

/* In tree-ssa.c  */
extern void init_tree_ssa (void);
extern edge ssa_redirect_edge (edge, basic_block);
extern void flush_pending_stmts (edge);
extern bool tree_ssa_useless_type_conversion (tree);
extern bool useless_type_conversion_p (tree, tree);
extern bool types_compatible_p (tree, tree);
extern void verify_ssa (bool);
extern void delete_tree_ssa (void);
extern void walk_use_def_chains (tree, walk_use_def_chains_fn, void *, bool);
extern bool stmt_references_memory_p (tree);
extern bool ssa_undefined_value_p (tree);

/* In tree-into-ssa.c  */
void update_ssa (unsigned);
void delete_update_ssa (void);
void register_new_name_mapping (tree, tree);
tree create_new_def_for (tree, tree, def_operand_p);
bool need_ssa_update_p (void);
bool name_mappings_registered_p (void);
bool name_registered_for_update_p (tree);
bitmap ssa_names_to_replace (void);
void release_ssa_name_after_update_ssa (tree);
void compute_global_livein (bitmap, bitmap);
tree duplicate_ssa_name (tree, tree);
void mark_sym_for_renaming (tree);
void mark_set_for_renaming (bitmap);
tree get_current_def (tree);
void set_current_def (tree, tree);

/* In tree-ssa-ccp.c  */
bool fold_stmt (tree *);
bool fold_stmt_inplace (tree);
tree widen_bitfield (tree, tree, tree);

/* In tree-vrp.c  */
tree vrp_evaluate_conditional (tree, tree);
void simplify_stmt_using_ranges (tree);

/* In tree-ssa-dom.c  */
extern void dump_dominator_optimization_stats (FILE *);
extern void debug_dominator_optimization_stats (void);
int loop_depth_of_name (tree);

/* In tree-ssa-copy.c  */
extern void merge_alias_info (tree, tree);
extern void propagate_value (use_operand_p, tree);
extern void propagate_tree_value (tree *, tree);
extern void replace_exp (use_operand_p, tree);
extern bool may_propagate_copy (tree, tree);
extern bool may_propagate_copy_into_asm (tree);

/* Affine iv.  */

typedef struct
{
  /* Iv = BASE + STEP * i.  */
  tree base, step;

  /* True if this iv does not overflow.  */
  bool no_overflow;
} affine_iv;

/* Description of number of iterations of a loop.  All the expressions inside
   the structure can be evaluated at the end of the loop's preheader
   (and due to ssa form, also anywhere inside the body of the loop).  */

struct tree_niter_desc
{
  tree assumptions;	/* The boolean expression.  If this expression evaluates
			   to false, then the other fields in this structure
			   should not be used; there is no guarantee that they
			   will be correct.  */
  tree may_be_zero;	/* The boolean expression.  If it evaluates to true,
			   the loop will exit in the first iteration (i.e.
			   its latch will not be executed), even if the niter
			   field says otherwise.  */
  tree niter;		/* The expression giving the number of iterations of
			   a loop (provided that assumptions == true and
			   may_be_zero == false), more precisely the number
			   of executions of the latch of the loop.  */
  double_int max;	/* The upper bound on the number of iterations of
			   the loop.  */

  /* The simplified shape of the exit condition.  The loop exits if
     CONTROL CMP BOUND is false, where CMP is one of NE_EXPR,
     LT_EXPR, or GT_EXPR, and step of CONTROL is positive if CMP is
     LE_EXPR and negative if CMP is GE_EXPR.  This information is used
     by loop unrolling.  */
  affine_iv control;
  tree bound;
  enum tree_code cmp;
};

/* In tree-vectorizer.c */
unsigned vectorize_loops (void);
extern bool vect_can_force_dr_alignment_p (const_tree, unsigned int);
extern tree get_vectype_for_scalar_type (tree);

/* In tree-ssa-phiopt.c */
bool empty_block_p (basic_block);
basic_block *blocks_in_phiopt_order (void);

/* In tree-ssa-loop*.c  */

void tree_ssa_lim (void);
unsigned int tree_ssa_unswitch_loops (void);
unsigned int canonicalize_induction_variables (void);
unsigned int tree_unroll_loops_completely (bool);
unsigned int tree_ssa_prefetch_arrays (void);
unsigned int remove_empty_loops (void);
void tree_ssa_iv_optimize (void);
unsigned tree_predictive_commoning (void);
bool parallelize_loops (void);

bool loop_only_exit_p (const struct loop *, const_edge);
bool number_of_iterations_exit (struct loop *, edge,
				struct tree_niter_desc *niter, bool);
tree find_loop_niter (struct loop *, edge *);
tree loop_niter_by_eval (struct loop *, edge);
tree find_loop_niter_by_eval (struct loop *, edge *);
void estimate_numbers_of_iterations (void);
bool scev_probably_wraps_p (tree, tree, tree, struct loop *, bool);
bool convert_affine_scev (struct loop *, tree, tree *, tree *, tree, bool);

bool nowrap_type_p (tree);
enum ev_direction {EV_DIR_GROWS, EV_DIR_DECREASES, EV_DIR_UNKNOWN};
enum ev_direction scev_direction (const_tree);

void free_numbers_of_iterations_estimates (void);
void free_numbers_of_iterations_estimates_loop (struct loop *);
void rewrite_into_loop_closed_ssa (bitmap, unsigned);
void verify_loop_closed_ssa (void);
bool for_each_index (tree *, bool (*) (tree, tree *, void *), void *);
void create_iv (tree, tree, tree, struct loop *, block_stmt_iterator *, bool,
		tree *, tree *);
basic_block split_loop_exit_edge (edge);
void standard_iv_increment_position (struct loop *, block_stmt_iterator *,
				     bool *);
basic_block ip_end_pos (struct loop *);
basic_block ip_normal_pos (struct loop *);
bool tree_duplicate_loop_to_header_edge (struct loop *, edge,
					 unsigned int, sbitmap,
					 edge, VEC (edge, heap) **,
					 int);
struct loop *tree_ssa_loop_version (struct loop *, tree,
				    basic_block *);
tree expand_simple_operations (tree);
void substitute_in_loop_info (struct loop *, tree, tree);
edge single_dom_exit (struct loop *);
bool can_unroll_loop_p (struct loop *loop, unsigned factor,
			struct tree_niter_desc *niter);
void tree_unroll_loop (struct loop *, unsigned,
		       edge, struct tree_niter_desc *);
typedef void (*transform_callback)(struct loop *, void *);
void tree_transform_and_unroll_loop (struct loop *, unsigned,
				     edge, struct tree_niter_desc *,
				     transform_callback, void *);
bool contains_abnormal_ssa_name_p (tree);
bool stmt_dominates_stmt_p (tree, tree);
void mark_virtual_ops_for_renaming (tree);

/* In tree-ssa-threadedge.c */
extern bool potentially_threadable_block (basic_block);
extern void thread_across_edge (tree, edge, bool,
				VEC(tree, heap) **, tree (*) (tree, tree));

/* In tree-ssa-loop-im.c  */
/* The possibilities of statement movement.  */

enum move_pos
  {
    MOVE_IMPOSSIBLE,		/* No movement -- side effect expression.  */
    MOVE_PRESERVE_EXECUTION,	/* Must not cause the non-executed statement
				   become executed -- memory accesses, ... */
    MOVE_POSSIBLE		/* Unlimited movement.  */
  };
extern enum move_pos movement_possibility (tree);
char *get_lsm_tmp_name (tree, unsigned);

/* In tree-flow-inline.h  */
static inline bool is_call_clobbered (const_tree);
static inline void mark_call_clobbered (tree, unsigned int);
static inline void set_is_used (tree);
static inline bool unmodifiable_var_p (const_tree);

/* In tree-eh.c  */
extern void make_eh_edges (tree);
extern bool tree_could_trap_p (tree);
extern bool tree_could_throw_p (tree);
extern bool tree_can_throw_internal (const_tree);
extern bool tree_can_throw_external (tree);
extern int lookup_stmt_eh_region (const_tree);
extern void add_stmt_to_eh_region (tree, int);
extern bool remove_stmt_from_eh_region (tree);
extern bool maybe_clean_or_replace_eh_stmt (tree, tree);

/* In tree-ssa-pre.c  */
void add_to_value (tree, tree);
void debug_value_expressions (tree);
void print_value_expressions (FILE *, tree);


/* In tree-vn.c  */
tree make_value_handle (tree);
void set_value_handle (tree, tree);
bool expressions_equal_p (tree, tree);
static inline tree get_value_handle (tree);
void sort_vuses (VEC (tree, gc) *);
void sort_vuses_heap (VEC (tree, heap) *);
tree vn_lookup_or_add (tree);
tree vn_lookup_or_add_with_stmt (tree, tree);
tree vn_lookup_or_add_with_vuses (tree, VEC (tree, gc) *);
void vn_add (tree, tree);
void vn_add_with_vuses (tree, tree, VEC (tree, gc) *);
tree vn_lookup_with_stmt (tree, tree);
tree vn_lookup (tree);
tree vn_lookup_with_vuses (tree, VEC (tree, gc) *);

/* In tree-ssa-sink.c  */
bool is_hidden_global_store (tree);

/* In tree-sra.c  */
void insert_edge_copies (tree, basic_block);
void sra_insert_before (block_stmt_iterator *, tree);
void sra_insert_after (block_stmt_iterator *, tree);
void sra_init_cache (void);
bool sra_type_can_be_decomposed_p (tree);

/* In tree-loop-linear.c  */
extern void linear_transform_loops (void);

/* In tree-data-ref.c  */
extern void tree_check_data_deps (void);

/* In tree-ssa-loop-ivopts.c  */
bool expr_invariant_in_loop_p (struct loop *, tree);
bool multiplier_allowed_in_address_p (HOST_WIDE_INT, enum machine_mode);
unsigned multiply_by_cost (HOST_WIDE_INT, enum machine_mode);

/* In tree-ssa-threadupdate.c.  */
extern bool thread_through_all_blocks (bool);
extern void register_jump_thread (edge, edge);

/* In gimplify.c  */
tree force_gimple_operand (tree, tree *, bool, tree);
tree force_gimple_operand_bsi (block_stmt_iterator *, tree, bool, tree,
			       bool, enum bsi_iterator_update);
tree gimple_fold_indirect_ref (tree);

/* In tree-ssa-structalias.c */
bool find_what_p_points_to (tree);

/* In tree-ssa-live.c */
extern void remove_unused_locals (void);

/* In tree-ssa-address.c  */

/* Description of a memory address.  */

struct mem_address
{
  tree symbol, base, index, step, offset;
};

struct affine_tree_combination;
tree create_mem_ref (block_stmt_iterator *, tree, 
		     struct affine_tree_combination *);
rtx addr_for_mem_ref (struct mem_address *, bool);
void get_address_description (tree, struct mem_address *);
tree maybe_fold_tmr (tree);

/* This structure is used during pushing fields onto the fieldstack
   to track the offset of the field, since bitpos_of_field gives it
   relative to its immediate containing type, and we want it relative
   to the ultimate containing object.  */

struct fieldoff
{
  /* Type of the field.  */
  tree type;

  /* Size, in bits, of the field.  */
  tree size;

  /* Field.  */
  tree decl;

  /* Offset from the base of the base containing object to this field.  */
  HOST_WIDE_INT offset;  

  /* Alias set for the field.  */
  alias_set_type alias_set;

  /* True, if this offset can be a base for further component accesses.  */
  unsigned base_for_components : 1;
};
typedef struct fieldoff fieldoff_s;

DEF_VEC_O(fieldoff_s);
DEF_VEC_ALLOC_O(fieldoff_s,heap);
int push_fields_onto_fieldstack (tree, VEC(fieldoff_s,heap) **,
				 HOST_WIDE_INT, bool *, tree);
void sort_fieldstack (VEC(fieldoff_s,heap) *);

void init_alias_heapvars (void);
void delete_alias_heapvars (void);
unsigned int execute_fixup_cfg (void);

#include "tree-flow-inline.h"

void swap_tree_operands (tree, tree *, tree *);

int least_common_multiple (int, int);

#endif /* _TREE_FLOW_H  */
