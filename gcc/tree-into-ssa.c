/* Rewrite a program in Normal form into SSA.
   Copyright (C) 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "flags.h"
#include "rtl.h"
#include "tm_p.h"
#include "langhooks.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "output.h"
#include "errors.h"
#include "expr.h"
#include "function.h"
#include "diagnostic.h"
#include "bitmap.h"
#include "tree-flow.h"
#include "tree-gimple.h"
#include "tree-inline.h"
#include "varray.h"
#include "timevar.h"
#include "tree-alias-common.h"
#include "hashtab.h"
#include "tree-dump.h"
#include "tree-pass.h"
#include "cfgloop.h"
#include "domwalk.h"

/* This file builds the SSA form for a function as described in:
   R. Cytron, J. Ferrante, B. Rosen, M. Wegman, and K. Zadeck. Efficiently
   Computing Static Single Assignment Form and the Control Dependence
   Graph. ACM Transactions on Programming Languages and Systems,
   13(4):451-490, October 1991.  */


/* Structure to map a variable VAR to the set of blocks that contain
   definitions for VAR.  */
struct def_blocks_d
{
  /* The variable.  */
  tree var;

  /* Blocks that contain definitions of VAR.  Bit I will be set if the
     Ith block contains a definition of VAR.  */
  bitmap def_blocks;

  /* Blocks where VAR is live-on-entry.  Similar semantics as
     DEF_BLOCKS.  */
  bitmap livein_blocks;
};

/* Each entry in DEF_BLOCKS contains an element of type STRUCT
   DEF_BLOCKS_D, mapping a variable VAR to a bitmap describing all the
   basic blocks where VAR is defined (assigned a new value).  It also
   contains a bitmap of all the blocks where VAR is live-on-entry
   (i.e., there is a use of VAR in block B without a preceding
   definition in B).  The live-on-entry information is used when
   computing PHI pruning heuristics.  */
static htab_t def_blocks;

/* Global data to attach to the main dominator walk structure.  */
struct mark_def_sites_global_data
{
  /* This sbitmap contains the variables which are set before they
     are used in a basic block.  We keep it as a global variable
     solely to avoid the overhead of allocating and deallocating
     the bitmap.  */
  sbitmap kills;
};

struct rewrite_block_data
{
  varray_type block_defs;
};


/* Local functions.  */
static void rewrite_finalize_block (struct dom_walk_data *, basic_block);
static void rewrite_initialize_block_local_data (struct dom_walk_data *,
						 basic_block, bool);
static void rewrite_initialize_block (struct dom_walk_data *, basic_block);
static void rewrite_add_phi_arguments (struct dom_walk_data *, basic_block);
static void mark_def_sites (struct dom_walk_data *walk_data,
			    basic_block bb, block_stmt_iterator);
static void mark_def_sites_initialize_block (struct dom_walk_data *walk_data,
					     basic_block bb);
static void compute_global_livein (bitmap, bitmap);
static void set_def_block (tree, basic_block);
static void set_livein_block (tree, basic_block);
static bool prepare_operand_for_rename (tree *op_p, size_t *uid_p, bool);
static void insert_phi_nodes (bitmap *);
static void rewrite_stmt (struct dom_walk_data *, basic_block,
			  block_stmt_iterator);
static inline void rewrite_operand (tree *);
static void insert_phi_nodes_for (tree, bitmap *, varray_type *);
static tree get_reaching_def (tree);
static hashval_t def_blocks_hash (const void *);
static int def_blocks_eq (const void *, const void *);
static void def_blocks_free (void *);
static int debug_def_blocks_r (void **, void *);
static inline struct def_blocks_d *get_def_blocks_for (tree);
static inline struct def_blocks_d *find_def_blocks_for (tree);
static void htab_statistics (FILE *, htab_t);

/* Compute global livein information given the set of blockx where
   an object is locally live at the start of the block (LIVEIN)
   and the set of blocks where the object is defined (DEF_BLOCKS).

   Note: This routine augments the existing local livein information
   to include global livein (i.e., it modifies the underlying bitmap
   for LIVEIN).  */

static void
compute_global_livein (bitmap livein, bitmap def_blocks)
{
  basic_block bb, *worklist, *tos;
  int i;

  tos = worklist
    = (basic_block *) xmalloc (sizeof (basic_block) * (last_basic_block + 1));

  EXECUTE_IF_SET_IN_BITMAP (livein, 0, i,
    {
	*tos++ = BASIC_BLOCK (i);
    });

  /* Iterate until the worklist is empty.  */
  while (tos != worklist)
    {
      edge e;

      /* Pull a block off the worklist.  */
      bb = *--tos;

      /* For each predecessor block.  */
      for (e = bb->pred; e; e = e->pred_next)
	{
	  basic_block pred = e->src;
	  int pred_index = pred->index;

	  /* None of this is necessary for the entry block.  */
	  if (pred != ENTRY_BLOCK_PTR
	      && ! bitmap_bit_p (livein, pred_index)
	      && ! bitmap_bit_p (def_blocks, pred_index))
	    {
	      *tos++ = pred;
	      bitmap_set_bit (livein, pred_index);
	    }
	}
    }

  free (worklist);
}


/* Block initialization routine for mark_def_sites.  Clear the 
   KILLS bitmap at the start of each block.  */

static void
mark_def_sites_initialize_block (struct dom_walk_data *walk_data,
				 basic_block bb ATTRIBUTE_UNUSED)
{
  struct mark_def_sites_global_data *gd = walk_data->global_data;
  sbitmap kills = gd->kills;

  sbitmap_zero (kills);
}


/* Call back for walk_dominator_tree used to collect definition sites
   for every variable in the function.  For every statement S in block
   BB:

   1- Variables defined by S in DEF_OPS(S) are marked in the bitmap
      WALK_DATA->GLOBAL_DATA->KILLS.

   2- If S uses a variable VAR and there is no preceding kill of VAR,
      then it is marked in marked in the LIVEIN_BLOCKS bitmap
      associated with VAR.

   This information is used to determine which variables are live
   across block boundaries to reduce the number of PHI nodes
   we create.  */

static void
mark_def_sites (struct dom_walk_data *walk_data,
		basic_block bb,
		block_stmt_iterator bsi)
{
  struct mark_def_sites_global_data *gd = walk_data->global_data;
  sbitmap kills = gd->kills;
  vdef_optype vdefs;
  vuse_optype vuses;
  def_optype defs;
  use_optype uses;
  size_t i, uid;
  tree stmt;
  stmt_ann_t ann;

  /* Mark all the blocks that have definitions for each variable in the
     VARS_TO_RENAME bitmap.  */
  stmt = bsi_stmt (bsi);
  get_stmt_operands (stmt);
  ann = stmt_ann (stmt);

  /* If a variable is used before being set, then the variable is live
     across a block boundary, so mark it live-on-entry to BB.  */
  uses = USE_OPS (ann);
  for (i = 0; i < NUM_USES (uses); i++)
    {
      tree *use_p = USE_OP_PTR (uses, i);

      if (prepare_operand_for_rename (use_p, &uid, true)
	  && !TEST_BIT (kills, uid))
	set_livein_block (*use_p, bb);
    }
	  
  /* Similarly for virtual uses.  */
  vuses = VUSE_OPS (ann);
  for (i = 0; i < NUM_VUSES (vuses); i++)
    {
      tree *use_p = VUSE_OP_PTR (vuses, i);

      if (prepare_operand_for_rename (use_p, &uid, true))
	set_livein_block (*use_p, bb);
    }

  /* Note that virtual definitions are irrelevant for computing KILLS
     because a VDEF does not constitute a killing definition of the
     variable.  However, the operand of a virtual definitions is a use
     of the variable, so it may cause the variable to be considered
     live-on-entry.  */
  vdefs = VDEF_OPS (ann);
  for (i = 0; i < NUM_VDEFS (vdefs); i++)
    {
      if (prepare_operand_for_rename (VDEF_OP_PTR (vdefs, i), &uid, true))
	{
	  /* If we do not already have an SSA_NAME for our destination,
	     then set the destination to the source.  */
	  if (TREE_CODE (VDEF_RESULT (vdefs, i)) != SSA_NAME)
	    VDEF_RESULT (vdefs, i) = VDEF_OP (vdefs, i);

	  set_livein_block (VDEF_OP (vdefs, i), bb);
	  set_def_block (VDEF_RESULT (vdefs, i), bb);
	}
    }

  /* Now process the definition made by this statement.  Mark the
     variables in KILLS.  */
  defs = DEF_OPS (ann);
  for (i = 0; i < NUM_DEFS (defs); i++)
    {
      tree *def_p = DEF_OP_PTR (defs, i);

      if (prepare_operand_for_rename (def_p, &uid, false))
	{
	  set_def_block (*def_p, bb);
	  SET_BIT (kills, uid);
	}
    }
}


/* Mark block BB as the definition site for variable VAR.  */

static void
set_def_block (tree var, basic_block bb)
{
  struct def_blocks_d *db_p;
  enum need_phi_state state;

  if (TREE_CODE (var) == SSA_NAME)
    var = SSA_NAME_VAR (var);

  state = var_ann (var)->need_phi_state;
  db_p = get_def_blocks_for (var);

  /* Set the bit corresponding to the block where VAR is defined.  */
  bitmap_set_bit (db_p->def_blocks, bb->index);

  /* Keep track of whether or not we may need to insert phi nodes.

     If we are in the UNKNOWN state, then this is the first definition
     of VAR.  Additionally, we have not seen any uses of VAR yet, so
     we do not need a phi node for this variable at this time (i.e.,
     transition to NEED_PHI_STATE_NO).

     If we are in any other state, then we either have multiple definitions
     of this variable occurring in different blocks or we saw a use of the
     variable which was not dominated by the block containing the
     definition(s).  In this case we may need a PHI node, so enter
     state NEED_PHI_STATE_MAYBE.  */
  if (state == NEED_PHI_STATE_UNKNOWN)
    var_ann (var)->need_phi_state = NEED_PHI_STATE_NO;
  else
    var_ann (var)->need_phi_state = NEED_PHI_STATE_MAYBE;
}


/* Mark block BB as having VAR live at the entry to BB.  */

static void
set_livein_block (tree var, basic_block bb)
{
  struct def_blocks_d *db_p;
  enum need_phi_state state = var_ann (var)->need_phi_state;

  db_p = get_def_blocks_for (var);

  /* Set the bit corresponding to the block where VAR is live in.  */
  bitmap_set_bit (db_p->livein_blocks, bb->index);

  /* Keep track of whether or not we may need to insert phi nodes.

     If we reach here in NEED_PHI_STATE_NO, see if this use is dominated
     by the single block containing the definition(s) of this variable.  If
     it is, then we remain in NEED_PHI_STATE_NO, otherwise we transition to
     NEED_PHI_STATE_MAYBE.  */
  if (state == NEED_PHI_STATE_NO)
    {
      int def_block_index = bitmap_first_set_bit (db_p->def_blocks);

      if (def_block_index == -1
	  || ! dominated_by_p (CDI_DOMINATORS, bb,
	                       BASIC_BLOCK (def_block_index)))
	var_ann (var)->need_phi_state = NEED_PHI_STATE_MAYBE;
    }
  else
    var_ann (var)->need_phi_state = NEED_PHI_STATE_MAYBE;
}


/* If the operand pointed to by OP_P needs to be renamed, then

     1. If OP_P is used (rather than set), then strip away any SSA_NAME
        wrapping the operand.

     2. Set *UID_P to the underlying variable's uid.

     3. Return true.

   Otherwise return false.  */

static bool
prepare_operand_for_rename (tree *op_p, size_t *uid_p, bool is_use)
{
  tree var = (TREE_CODE (*op_p) != SSA_NAME) ? *op_p : SSA_NAME_VAR (*op_p);
  *uid_p = var_ann (var)->uid;

  /* Ignore variables that don't need to be renamed.  */
  if (vars_to_rename && !bitmap_bit_p (vars_to_rename, *uid_p))
    return false;

  /* The variable needs to be renamed.  If this is a use which already
     has an SSA_NAME, then strip it off.

     By not throwing away SSA_NAMEs on assignments, we avoid a lot of 
     useless churn of SSA_NAMEs without having to overly complicate the
     renamer.  */
  if (TREE_CODE (*op_p) == SSA_NAME && is_use)
    *op_p = var;

  return true;
}


/* Helper for insert_phi_nodes.  If VAR needs PHI nodes, insert them
   at the dominance frontier (DFS) of blocks defining VAR.  */

static inline
void insert_phi_nodes_1 (tree var, bitmap *dfs, varray_type *work_stack)
{
  var_ann_t ann = var_ann (var);
  if (ann->need_phi_state != NEED_PHI_STATE_NO)
    insert_phi_nodes_for (var, dfs, work_stack);
}


/* Insert PHI nodes at the dominance frontier of blocks with variable
   definitions.  DFS contains the dominance frontier information for
   the flowgraph.  PHI nodes will only be inserted at the dominance
   frontier of definition blocks for variables whose NEED_PHI_STATE
   annotation is marked as ``maybe'' or ``unknown'' (computed by
   mark_def_sites).  */

static void
insert_phi_nodes (bitmap *dfs)
{
  size_t i;
  varray_type work_stack;

  timevar_push (TV_TREE_INSERT_PHI_NODES);

  /* Array WORK_STACK is a stack of CFG blocks.  Each block that contains
     an assignment or PHI node will be pushed to this stack.  */
  VARRAY_BB_INIT (work_stack, last_basic_block, "work_stack");

  /* Iterate over all variables in VARS_TO_RENAME.  For each variable, add
     to the work list all the blocks that have a definition for the
     variable.  PHI nodes will be added to the dominance frontier blocks of
     each definition block.  */
  if (vars_to_rename)
    EXECUTE_IF_SET_IN_BITMAP (vars_to_rename, 0, i,
	insert_phi_nodes_1 (referenced_var (i), dfs, &work_stack));
  else
    for (i = 0; i < num_referenced_vars; i++)
      insert_phi_nodes_1 (referenced_var (i), dfs, &work_stack);

  timevar_pop (TV_TREE_INSERT_PHI_NODES);
}


/* Perform a depth-first traversal of the dominator tree looking for
   variables to rename.  BB is the block where to start searching.
   Renaming is a five step process:

   1- Every definition made by PHI nodes at the start of the blocks is
      registered as the current definition for the corresponding variable.

   2- Every statement in BB is rewritten.  USE and VUSE operands are
      rewritten with their corresponding reaching definition.  DEF and
      VDEF targets are registered as new definitions.
      
   3- All the PHI nodes in successor blocks of BB are visited.  The
      argument corresponding to BB is replaced with its current reaching
      definition.

   4- Recursively rewrite every dominator child block of BB.

   5- Restore (in reverse order) the current reaching definition for every
      new definition introduced in this block.  This is done so that when
      we return from the recursive call, all the current reaching
      definitions are restored to the names that were valid in the
      dominator parent of BB.  */

/* Initialize the local stacks.
     
   BLOCK_DEFS is used to save all the existing reaching definitions for
   the new SSA names introduced in this block.  Before registering a
   new definition for a variable, the existing reaching definition is
   pushed into this stack so that we can restore it in Step 5.  */

static void
rewrite_initialize_block_local_data (struct dom_walk_data *walk_data ATTRIBUTE_UNUSED,
				     basic_block bb ATTRIBUTE_UNUSED,
				     bool recycled ATTRIBUTE_UNUSED)
{
#ifdef ENABLE_CHECKING
  struct rewrite_block_data *bd
    = (struct rewrite_block_data *)VARRAY_TOP_GENERIC_PTR (walk_data->block_data_stack);
                                                                                
  /* We get cleared memory from the allocator, so if the memory is
     not cleared, then we are re-using a previously allocated entry.  In
     that case, we can also re-use the underlying virtal arrays.  Just
     make sure we clear them before using them!  */
  if (recycled && bd->block_defs && VARRAY_ACTIVE_SIZE (bd->block_defs) > 0)
    abort ();
#endif
}


/* SSA Rewriting Step 1.  Initialization, create a block local stack
   of reaching definitions for new SSA names produced in this block
   (BLOCK_DEFS).  Register new definitions for every PHI node in the
   block.  */

static void
rewrite_initialize_block (struct dom_walk_data *walk_data, basic_block bb)
{
  tree phi;
  struct rewrite_block_data *bd
    = (struct rewrite_block_data *)VARRAY_TOP_GENERIC_PTR (walk_data->block_data_stack);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n\nRenaming block #%d\n\n", bb->index);

  /* Step 1.  Register new definitions for every PHI node in the block.
     Conceptually, all the PHI nodes are executed in parallel and each PHI
     node introduces a new version for the associated variable.  */
  for (phi = phi_nodes (bb); phi; phi = TREE_CHAIN (phi))
    {
      tree result = PHI_RESULT (phi);

      register_new_def (result, &bd->block_defs);
    }
}


/* SSA Rewriting Step 3.  Visit all the successor blocks of BB looking for
   PHI nodes.  For every PHI node found, add a new argument containing the
   current reaching definition for the variable and the edge through which
   that definition is reaching the PHI node.   */

static void
rewrite_add_phi_arguments (struct dom_walk_data *walk_data ATTRIBUTE_UNUSED,
			   basic_block bb)
{
  edge e;

  for (e = bb->succ; e; e = e->succ_next)
    {
      tree phi;

      for (phi = phi_nodes (e->dest); phi; phi = TREE_CHAIN (phi))
	{
	  tree currdef;

	  /* If this PHI node has already been rewritten, then there is
	     nothing to do for this PHI or any following PHIs since we
	     always add new PHI nodes at the start of the PHI chain.  */
	  if (PHI_REWRITTEN (phi))
	    break;

	  currdef = get_reaching_def (SSA_NAME_VAR (PHI_RESULT (phi)));
	  add_phi_arg (&phi, currdef, e);
	}
    }
}

/* SSA Rewriting Step 5.  Restore the current reaching definition for each
   variable referenced in the block (in reverse order).  */

static void
rewrite_finalize_block (struct dom_walk_data *walk_data,
			basic_block bb ATTRIBUTE_UNUSED)
{
  struct rewrite_block_data *bd
    = (struct rewrite_block_data *)VARRAY_TOP_GENERIC_PTR (walk_data->block_data_stack);

  /* Step 5.  Restore the current reaching definition for each variable
     referenced in the block (in reverse order).  */
  while (bd->block_defs && VARRAY_ACTIVE_SIZE (bd->block_defs) > 0)
    {
      tree tmp = VARRAY_TOP_TREE (bd->block_defs);
      tree saved_def, var;

      VARRAY_POP (bd->block_defs);
      if (TREE_CODE (tmp) == SSA_NAME)
	{
	  saved_def = tmp;
	  var = SSA_NAME_VAR (saved_def);
	}
      else
	{
	  saved_def = NULL;
	  var = tmp;
	}

      var_ann (var)->current_def = saved_def;
    }
}


/* Dump SSA information to FILE.  */

void
dump_tree_ssa (FILE *file)
{
  basic_block bb;
  const char *funcname
    = lang_hooks.decl_printable_name (current_function_decl, 2);

  fprintf (file, "SSA information for %s\n\n", funcname);

  FOR_EACH_BB (bb)
    {
      dump_bb (bb, file, 0);
      fputs ("    ", file);
      print_generic_stmt (file, phi_nodes (bb), dump_flags);
      fputs ("\n\n", file);
    }
}


/* Dump SSA information to stderr.  */

void
debug_tree_ssa (void)
{
  dump_tree_ssa (stderr);
}


/* Dump SSA statistics on FILE.  */

void
dump_tree_ssa_stats (FILE *file)
{
  fprintf (file, "\nHash table statistics:\n");

  fprintf (file, "    def_blocks: ");
  htab_statistics (file, def_blocks);

  fprintf (file, "\n");
}


/* Dump SSA statistics on stderr.  */

void
debug_tree_ssa_stats (void)
{
  dump_tree_ssa_stats (stderr);
}


/* Dump statistics for the hash table HTAB.  */

static void
htab_statistics (FILE *file, htab_t htab)
{
  fprintf (file, "size %ld, %ld elements, %f collision/search ratio\n",
	   (long) htab_size (htab),
	   (long) htab_elements (htab),
	   htab_collisions (htab));
}


/* Insert PHI nodes for variable VAR using the dominance frontier
   information given in DFS.  */

static void
insert_phi_nodes_for (tree var, bitmap *dfs, varray_type *work_stack)
{
  struct def_blocks_d *def_map;
  bitmap phi_insertion_points;
  int bb_index;

  def_map = find_def_blocks_for (var);
  if (def_map == NULL)
    return;

  phi_insertion_points = BITMAP_XMALLOC ();

  EXECUTE_IF_SET_IN_BITMAP (def_map->def_blocks, 0, bb_index,
    {
      VARRAY_PUSH_BB (*work_stack, BASIC_BLOCK (bb_index));
    });

  /* Pop a block off the worklist, add every block that appears in
     the original block's dfs that we have not already processed to
     the worklist.  Iterate until the worklist is empty.   Blocks
     which are added to the worklist are potential sites for
     PHI nodes. 

     The iteration step could be done during PHI insertion just as
     easily.  We do it here for historical reasons -- we used to have
     a heuristic which used the potential PHI insertion points to
     determine if fully pruned or semi pruned SSA form was appropriate.

     We now always use fully pruned SSA form.  */
  while (VARRAY_ACTIVE_SIZE (*work_stack) > 0)
    {
      basic_block bb = VARRAY_TOP_BB (*work_stack);
      int bb_index = bb->index;
      int dfs_index;

      VARRAY_POP (*work_stack);
      
      EXECUTE_IF_AND_COMPL_IN_BITMAP (dfs[bb_index],
				      phi_insertion_points,
				      0, dfs_index,
	{
	  basic_block bb = BASIC_BLOCK (dfs_index);

	  VARRAY_PUSH_BB (*work_stack, bb);
	  bitmap_set_bit (phi_insertion_points, dfs_index);
	});
    }

  /* Now compute global livein for this variable.  Note this modifies
     def_map->livein_blocks.  */
  compute_global_livein (def_map->livein_blocks, def_map->def_blocks);

  /* And insert the PHI nodes.  */
  EXECUTE_IF_AND_IN_BITMAP (phi_insertion_points, def_map->livein_blocks,
			    0, bb_index,
    {
      create_phi_node (var, BASIC_BLOCK (bb_index));
    });

  BITMAP_XFREE (phi_insertion_points);
}

/* SSA Rewriting Step 2.  Rewrite every variable used in each statement in
   the block with its immediate reaching definitions.  Update the current
   definition of a variable when a new real or virtual definition is found.  */

static void
rewrite_stmt (struct dom_walk_data *walk_data,
	      basic_block bb ATTRIBUTE_UNUSED,
	      block_stmt_iterator si)
{
  size_t i;
  stmt_ann_t ann;
  tree stmt;
  vuse_optype vuses;
  vdef_optype vdefs;
  def_optype defs;
  use_optype uses;
  struct rewrite_block_data *bd;

  bd = VARRAY_TOP_GENERIC_PTR (walk_data->block_data_stack);

  stmt = bsi_stmt (si);
  ann = stmt_ann (stmt);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Renaming statement ");
      print_generic_stmt (dump_file, stmt, TDF_SLIM);
      fprintf (dump_file, "\n");
    }

#if defined ENABLE_CHECKING
  /* We have just scanned the code for operands.  No statement should
     be modified.  */
  if (ann->modified)
    abort ();
#endif

  defs = DEF_OPS (ann);
  uses = USE_OPS (ann);
  vuses = VUSE_OPS (ann);
  vdefs = VDEF_OPS (ann);

  /* Step 1.  Rewrite USES and VUSES in the statement.  */
  for (i = 0; i < NUM_USES (uses); i++)
    rewrite_operand (USE_OP_PTR (uses, i));

  /* Rewrite virtual uses in the statement.  */
  for (i = 0; i < NUM_VUSES (vuses); i++)
    rewrite_operand (VUSE_OP_PTR (vuses, i));

  /* Step 2.  Register the statement's DEF and VDEF operands.  */
  for (i = 0; i < NUM_DEFS (defs); i++)
    {
      tree *def_p = DEF_OP_PTR (defs, i);

      if (TREE_CODE (*def_p) != SSA_NAME)
	*def_p = make_ssa_name (*def_p, stmt);

      /* FIXME: We shouldn't be registering new defs if the variable
	 doesn't need to be renamed.  */
      register_new_def (*def_p, &bd->block_defs);
    }

  /* Register new virtual definitions made by the statement.  */
  for (i = 0; i < NUM_VDEFS (vdefs); i++)
    {
      rewrite_operand (VDEF_OP_PTR (vdefs, i));

      if (TREE_CODE (VDEF_RESULT (vdefs, i)) != SSA_NAME)
	*VDEF_RESULT_PTR (vdefs, i)
	  = make_ssa_name (VDEF_RESULT (vdefs, i), stmt);

      /* FIXME: We shouldn't be registering new defs if the variable
	 doesn't need to be renamed.  */
      register_new_def (VDEF_RESULT (vdefs, i), &bd->block_defs);
    }
}


/* Replace the operand pointed by OP_P with its immediate reaching
   definition.  */

static inline void
rewrite_operand (tree *op_p)
{
  if (TREE_CODE (*op_p) != SSA_NAME)
    *op_p = get_reaching_def (*op_p);
}


/* Register DEF (an SSA_NAME) to be a new definition for its underlying
   variable (SSA_NAME_VAR (DEF)) and push VAR's current reaching definition
   into the stack pointed by BLOCK_DEFS_P.  */

void
register_new_def (tree def, varray_type *block_defs_p)
{
  tree var = SSA_NAME_VAR (def);
  tree currdef;
   
  /* If this variable is set in a single basic block and all uses are
     dominated by the set(s) in that single basic block, then there is
     no reason to record anything for this variable in the block local
     definition stacks.  Doing so just wastes time and memory.

     This is the same test to prune the set of variables which may
     need PHI nodes.  So we just use that information since it's already
     computed and available for us to use.  */
  if (var_ann (var)->need_phi_state == NEED_PHI_STATE_NO)
    {
      var_ann (var)->current_def = def;
      return;
    }

  currdef = var_ann (var)->current_def;
  if (! *block_defs_p)
    VARRAY_TREE_INIT (*block_defs_p, 20, "block_defs");

  /* Push the current reaching definition into *BLOCK_DEFS_P.  This stack is
     later used by the dominator tree callbacks to restore the reaching
     definitions for all the variables defined in the block after a recursive
     visit to all its immediately dominated blocks.  If there is no current
     reaching definition, then just record the underlying _DECL node.  */
  VARRAY_PUSH_TREE (*block_defs_p, currdef ? currdef : var);

  /* Set the current reaching definition for VAR to be DEF.  */
  var_ann (var)->current_def = def;
}


/* Return the current definition for variable VAR.  If none is found,
   create a new SSA name to act as the zeroth definition for VAR.  If VAR
   is call clobbered and there exists a more recent definition of
   GLOBAL_VAR, return the definition for GLOBAL_VAR.  This means that VAR
   has been clobbered by a function call since its last assignment.  */

static tree
get_reaching_def (tree var)
{
  tree default_d, currdef_var;
  
  /* Lookup the current reaching definition for VAR.  */
  default_d = NULL_TREE;
  currdef_var = var_ann (var)->current_def;

  /* If there is no reaching definition for VAR, create and register a
     default definition for it (if needed).  */
  if (currdef_var == NULL_TREE)
    {
      default_d = default_def (var);
      if (default_d == NULL_TREE)
	{
	  default_d = make_ssa_name (var, build_empty_stmt ());
	  set_default_def (var, default_d);
	}
      var_ann (var)->current_def = default_d;
    }

  /* Return the current reaching definition for VAR, or the default
     definition, if we had to create one.  */
  return (currdef_var) ? currdef_var : default_d;
}


/* Hashing and equality functions for DEF_BLOCKS.  */

static hashval_t
def_blocks_hash (const void *p)
{
  return htab_hash_pointer
	((const void *)((const struct def_blocks_d *)p)->var);
}

static int
def_blocks_eq (const void *p1, const void *p2)
{
  return ((const struct def_blocks_d *)p1)->var
	 == ((const struct def_blocks_d *)p2)->var;
}

/* Free memory allocated by one entry in DEF_BLOCKS.  */

static void
def_blocks_free (void *p)
{
  struct def_blocks_d *entry = p;
  BITMAP_XFREE (entry->def_blocks);
  BITMAP_XFREE (entry->livein_blocks);
  free (entry);
}


/* Dump the DEF_BLOCKS hash table on stderr.  */

void
debug_def_blocks (void)
{
  htab_traverse (def_blocks, debug_def_blocks_r, NULL);
}

/* Callback for htab_traverse to dump the DEF_BLOCKS hash table.  */

static int
debug_def_blocks_r (void **slot, void *data ATTRIBUTE_UNUSED)
{
  unsigned long i;
  struct def_blocks_d *db_p = (struct def_blocks_d *) *slot;
  
  fprintf (stderr, "VAR: ");
  print_generic_expr (stderr, db_p->var, dump_flags);
  fprintf (stderr, ", DEF_BLOCKS: { ");
  EXECUTE_IF_SET_IN_BITMAP (db_p->def_blocks, 0, i,
			    fprintf (stderr, "%ld ", i));
  fprintf (stderr, "}");
  fprintf (stderr, ", LIVEIN_BLOCKS: { ");
  EXECUTE_IF_SET_IN_BITMAP (db_p->livein_blocks, 0, i,
			    fprintf (stderr, "%ld ", i));
  fprintf (stderr, "}\n");

  return 1;
}


/* Return the set of blocks where variable VAR is defined and the blocks
   where VAR is live on entry (livein).  Return NULL, if no entry is
   found in DEF_BLOCKS.  */

static inline struct def_blocks_d *
find_def_blocks_for (tree var)
{
  struct def_blocks_d dm;
  dm.var = var;
  return (struct def_blocks_d *) htab_find (def_blocks, &dm);
}


/* Return the set of blocks where variable VAR is defined and the blocks
   where VAR is live on entry (livein).  If no entry is found in
   DEF_BLOCKS, a new one is created and returned.  */

static inline struct def_blocks_d *
get_def_blocks_for (tree var)
{
  struct def_blocks_d db, *db_p;
  void **slot;

  db.var = var;
  slot = htab_find_slot (def_blocks, (void *) &db, INSERT);
  if (*slot == NULL)
    {
      db_p = xmalloc (sizeof (*db_p));
      db_p->var = var;
      db_p->def_blocks = BITMAP_XMALLOC ();
      db_p->livein_blocks = BITMAP_XMALLOC ();
      *slot = (void *) db_p;
    }
  else
    db_p = (struct def_blocks_d *) *slot;

  return db_p;
}

/* If a variable V in VARS_TO_RENAME is a pointer, the renaming
   process will cause us to lose the name memory tags that may have
   been associated with the various SSA_NAMEs of V.  This means that
   the variables aliased to those name tags also need to be renamed
   again.

   FIXME 1- We should either have a better scheme for renaming
	    pointers that doesn't lose name tags or re-run alias
	    analysis to recover points-to information.

	 2- Currently we just invalidate *all* the name tags.  This
	    should be more selective.  */

static void
invalidate_name_tags (bitmap vars_to_rename)
{
  size_t i;
  bool rename_name_tags_p;

  rename_name_tags_p = false;
  EXECUTE_IF_SET_IN_BITMAP (vars_to_rename, 0, i,
      if (POINTER_TYPE_P (TREE_TYPE (referenced_var (i))))
	{
	  rename_name_tags_p = true;
	  break;
	});

  if (rename_name_tags_p)
    for (i = 0; i < num_referenced_vars; i++)
      {
	var_ann_t ann = var_ann (referenced_var (i));

	if (ann->mem_tag_kind == NAME_TAG)
	  {
	    size_t j;
	    varray_type may_aliases = ann->may_aliases;

	    bitmap_set_bit (vars_to_rename, ann->uid);
	    if (ann->may_aliases)
	      for (j = 0; j < VARRAY_ACTIVE_SIZE (may_aliases); j++)
		{
		  tree var = VARRAY_TREE (may_aliases, j);
		  bitmap_set_bit (vars_to_rename, var_ann (var)->uid);
		}
	  }
      }
}


/* Main entry point into the SSA builder.  The renaming process
   proceeds in five main phases:

   1- If VARS_TO_RENAME has any entries, any existing PHI nodes for
      those variables are removed from the flow graph so that they can
      be computed again.

   2- Compute dominance frontier and immediate dominators, needed to
      insert PHI nodes and rename the function in dominator tree
      order.

   3- Find and mark all the blocks that define variables
      (mark_def_sites).

   4- Insert PHI nodes at dominance frontiers (insert_phi_nodes).

   5- Rename all the blocks (rewrite_initialize_block,
      rewrite_add_phi_arguments) and statements in the program
      (rewrite_stmt).

   Steps 3 and 5 are done using the dominator tree walker
   (walk_dominator_tree).  */

void
rewrite_into_ssa (void)
{
  bitmap *dfs;
  basic_block bb;
  struct dom_walk_data walk_data;
  struct mark_def_sites_global_data mark_def_sites_global_data;
  unsigned int i;
  
  timevar_push (TV_TREE_SSA_OTHER);

  /* Initialize the array of variables to rename.  */
  if (vars_to_rename != NULL)
    {
      invalidate_name_tags (vars_to_rename);

      /* Now remove all the existing PHI nodes (if any) for the variables
	 that we are about to rename into SSA.  */
      remove_all_phi_nodes_for (vars_to_rename);
    }

  /* Allocate memory for the DEF_BLOCKS hash table.  */
  def_blocks = htab_create (VARRAY_ACTIVE_SIZE (referenced_vars),
			    def_blocks_hash, def_blocks_eq, def_blocks_free);

  /* Initialize dominance frontier and immediate dominator bitmaps. 
     Also count the number of predecessors for each block.  Doing so
     can save significant time during PHI insertion for large graphs.  */
  dfs = (bitmap *) xmalloc (last_basic_block * sizeof (bitmap *));
  FOR_EACH_BB (bb)
    {
      edge e;
      int count = 0;

      for (e = bb->pred; e; e = e->pred_next)
	count++;

      bb_ann (bb)->num_preds = count;
      dfs[bb->index] = BITMAP_XMALLOC ();
    }

  for (i = 0; i < num_referenced_vars; i++)
    var_ann (referenced_var (i))->current_def = NULL;

  /* Ensure that the dominance information is OK.  */
  calculate_dominance_info (CDI_DOMINATORS);

  /* Compute dominance frontiers.  */
  compute_dominance_frontiers (dfs);

  /* Setup callbacks for the generic dominator tree walker to find and
     mark definition sites.  */
  walk_data.walk_stmts_backward = false;
  walk_data.dom_direction = CDI_DOMINATORS;
  walk_data.initialize_block_local_data = NULL;
  walk_data.before_dom_children_before_stmts = mark_def_sites_initialize_block;
  walk_data.before_dom_children_walk_stmts = mark_def_sites;
  walk_data.before_dom_children_after_stmts = NULL; 
  walk_data.after_dom_children_before_stmts =  NULL;
  walk_data.after_dom_children_walk_stmts =  NULL;
  walk_data.after_dom_children_after_stmts =  NULL;

  /* Notice that this bitmap is indexed using variable UIDs, so it must be
     large enough to accommodate all the variables referenced in the
     function, not just the ones we are renaming.  */
  mark_def_sites_global_data.kills = sbitmap_alloc (num_referenced_vars);
  walk_data.global_data = &mark_def_sites_global_data;

  /* We do not have any local data.  */
  walk_data.block_local_data_size = 0;

  /* Initialize the dominator walker.  */
  init_walk_dominator_tree (&walk_data);

  /* Recursively walk the dominator tree.  */
  walk_dominator_tree (&walk_data, ENTRY_BLOCK_PTR);

  /* Finalize the dominator walker.  */
  fini_walk_dominator_tree (&walk_data);

  /* We no longer need this bitmap, clear and free it.  */
  sbitmap_free (mark_def_sites_global_data.kills);

  /* Insert PHI nodes at dominance frontiers of definition blocks.  */
  insert_phi_nodes (dfs);

  /* Rewrite all the basic blocks in the program.  */
  timevar_push (TV_TREE_SSA_REWRITE_BLOCKS);

  /* Setup callbacks for the generic dominator tree walker.  */
  walk_data.walk_stmts_backward = false;
  walk_data.dom_direction = CDI_DOMINATORS;
  walk_data.initialize_block_local_data = rewrite_initialize_block_local_data;
  walk_data.before_dom_children_before_stmts = rewrite_initialize_block;
  walk_data.before_dom_children_walk_stmts = rewrite_stmt;
  walk_data.before_dom_children_after_stmts = rewrite_add_phi_arguments; 
  walk_data.after_dom_children_before_stmts =  NULL;
  walk_data.after_dom_children_walk_stmts =  NULL;
  walk_data.after_dom_children_after_stmts =  rewrite_finalize_block;
  walk_data.global_data = NULL;
  walk_data.block_local_data_size = sizeof (struct rewrite_block_data);

  /* Initialize the dominator walker.  */
  init_walk_dominator_tree (&walk_data);

  /* Recursively walk the dominator tree rewriting each statement in
     each basic block.  */
  walk_dominator_tree (&walk_data, ENTRY_BLOCK_PTR);

  /* Finalize the dominator walker.  */
  fini_walk_dominator_tree (&walk_data);

  timevar_pop (TV_TREE_SSA_REWRITE_BLOCKS);

  /* Debugging dumps.  */
  if (dump_file && (dump_flags & TDF_STATS))
    {
      dump_dfa_stats (dump_file);
      dump_tree_ssa_stats (dump_file);
    }

  /* Free allocated memory.  */
  FOR_EACH_BB (bb)
    BITMAP_XFREE (dfs[bb->index]);
  free (dfs);

  htab_delete (def_blocks);

  timevar_pop (TV_TREE_SSA_OTHER);
}

struct tree_opt_pass pass_build_ssa = 
{
  "ssa",				/* name */
  NULL,					/* gate */
  rewrite_into_ssa,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  PROP_cfg | PROP_referenced_vars,	/* properties_required */
  PROP_ssa,				/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_verify_ssa	/* todo_flags_finish */
};
