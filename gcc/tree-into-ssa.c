/* Rewrite a program in Normal form into SSA.
   Copyright (C) 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
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
#include "hashtab.h"
#include "tree-dump.h"
#include "tree-pass.h"
#include "cfgloop.h"
#include "domwalk.h"
#include "ggc.h"

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

  /* Blocks that contain a PHI node for VAR.  */
  bitmap phi_blocks;

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

/* Stack of trees used to restore the global currdefs to its original
   state after completing rewriting of a block and its dominator
   children.  Its elements have the following properties:

   - An SSA_NAME indicates that the current definition of the
     underlying variable should be set to the given SSA_NAME.

   - A _DECL node indicates that the underlying variable has no
     current definition.

   - A NULL node is used to mark the last node associated with the
     current block.

   - A NULL node at the top entry is used to mark the last node
     associated with the current block.  */
static VEC(tree_on_heap) *block_defs_stack;

/* Basic block vectors used in this file ought to be allocated in the heap.  */
DEF_VEC_MALLOC_P(int);

/* Set of existing SSA names being replaced by update_ssa.  */
static sbitmap old_ssa_names;

/* Set of new SSA names being added by update_ssa.  Note that both
   NEW_SSA_NAMES and OLD_SSA_NAMES are dense bitmaps because most of
   the operations done on them are presence tests.  */
static sbitmap new_ssa_names;

/* Set of virtual SSA names to be updated.  Since virtuals are always
   in FUD chain form, these names are not used as a mapping mechanism
   like OLD_SSA_NAMES and NEW_SSA_NAMES.  Instead, the names in this
   set are used by ssa_names_to_replace to inform its caller which
   names are going to be updated.  */
static bitmap old_virtual_ssa_names;

/* Symbols whose SSA form needs to be updated or created for the first
   time.  */
static bitmap syms_to_rename;

/* Set of SSA names that have been marked to be released after they
   were registered in the replacement table.  They will be finally
   released after we finish updating the SSA web.  */
static bitmap names_to_release;

/* Growth factor for NEW_SSA_NAMES and OLD_SSA_NAMES.  These sets need
   to grow as the callers to register_new_name_mapping will typically
   create new names on the fly.  FIXME.  Currently set to 1/3 to avoid
   frequent reallocations but still need to find a reasonable growth
   strategy.  */
#define NAME_SETS_GROWTH_FACTOR	(MAX (3, num_ssa_names / 3))

/* Tuple used to represent replacement mappings.  */
struct repl_map_d
{
  tree name;
  bitmap set;
};

/* NEW -> OLD_SET replacement table.  If we are replacing several
   existing SSA names O_1, O_2, ..., O_j with a new name N_i,
   then REPL_TBL[N_i] = { O_1, O_2, ..., O_j }.  */
static htab_t repl_tbl;

/* true if register_new_name_mapping needs to initialize the data
   structures needed by update_ssa.  */
static bool need_to_initialize_update_ssa_p = true;

/* true if update_ssa needs to update virtual operands.  */
static bool need_to_update_vops_p = false;

/* true if update_ssa is replacing existing SSA names.  */
static bool need_to_replace_names_p = false;

/* Global data to attach to the main dominator walk structure.  */
struct mark_def_sites_global_data
{
  /* This bitmap contains the variables which are set before they
     are used in a basic block.  */
  bitmap kills;

  /* Bitmap of names to rename.  */
  sbitmap names_to_rename;

  /* Set of blocks that mark_def_sites deems interesting for the
     renamer to process.  */
  sbitmap interesting_blocks;
};


/* Information stored for SSA names.  */
struct ssa_name_info
{
  /* This field indicates whether or not the variable may need PHI nodes.
     See the enum's definition for more detailed information about the
     states.  */
  ENUM_BITFIELD (need_phi_state) need_phi_state : 2;

  /* The actual definition of the ssa name.  */
  tree current_def;
};


/* The main entry point to the SSA renamer (rewrite_blocks) may be
   called several times to do different, but related, tasks.
   Initially, we need it to rename the whole program into SSA form.
   At other times, we may need it to only rename into SSA newly
   exposed symbols.  Finally, we can also call it to incrementally fix
   an already built SSA web.  */
enum rewrite_mode {
    /* Convert the whole function into SSA form.  */
    REWRITE_ALL,

    /* Incrementally update the SSA web by replacing existing SSA
       names with new ones.  See update_ssa for details.  */
    REWRITE_UPDATE
};


/* Use TREE_VISITED to keep track of which statements we want to
   rename.  When renaming a subset of the variables, not all
   statements will be processed.  This is decided in mark_def_sites.  */
#define REWRITE_THIS_STMT(T)	TREE_VISITED (T)

/* Use the unsigned flag to keep track of which statements we want to
   visit when marking new definition sites.  This is slightly
   different than REWRITE_THIS_STMT: it's used by update_ssa to
   distinguish statements that need to have both uses and defs
   processed from those that only need to have their defs processed.
   Statements that define new SSA names only need to have their defs
   registered, but they don't need to have their uses renamed.  */
#define REGISTER_DEFS_IN_THIS_STMT(T)	(T)->common.unsigned_flag


/* Get the information associated with NAME.  */

static inline struct ssa_name_info *
get_ssa_name_ann (tree name)
{
  if (!SSA_NAME_AUX (name))
    SSA_NAME_AUX (name) = xcalloc (1, sizeof (struct ssa_name_info));

  return SSA_NAME_AUX (name);
}


/* Gets phi_state field for VAR.  */

static inline enum need_phi_state
get_phi_state (tree var)
{
  if (TREE_CODE (var) == SSA_NAME)
    return get_ssa_name_ann (var)->need_phi_state;
  else
    return var_ann (var)->need_phi_state;
}


/* Sets phi_state field for VAR to STATE.  */

static inline void
set_phi_state (tree var, enum need_phi_state state)
{
  if (TREE_CODE (var) == SSA_NAME)
    get_ssa_name_ann (var)->need_phi_state = state;
  else
    var_ann (var)->need_phi_state = state;
}


/* Return the current definition for VAR.  */

static inline tree
get_current_def (tree var)
{
  if (TREE_CODE (var) == SSA_NAME)
    return get_ssa_name_ann (var)->current_def;
  else
    return var_ann (var)->current_def;
}


/* Sets current definition of VAR to DEF.  */

static inline void
set_current_def (tree var, tree def)
{
  if (TREE_CODE (var) == SSA_NAME)
    get_ssa_name_ann (var)->current_def = def;
  else
    var_ann (var)->current_def = def;
}


/* Compute global livein information given the set of blockx where
   an object is locally live at the start of the block (LIVEIN)
   and the set of blocks where the object is defined (DEF_BLOCKS).

   Note: This routine augments the existing local livein information
   to include global livein (i.e., it modifies the underlying bitmap
   for LIVEIN).  */

void
compute_global_livein (bitmap livein, bitmap def_blocks)
{
  basic_block bb, *worklist, *tos;
  unsigned i;
  bitmap_iterator bi;

  tos = worklist
    = (basic_block *) xmalloc (sizeof (basic_block) * (last_basic_block + 1));

  EXECUTE_IF_SET_IN_BITMAP (livein, 0, i, bi)
    {
      *tos++ = BASIC_BLOCK (i);
    }

  /* Iterate until the worklist is empty.  */
  while (tos != worklist)
    {
      edge e;
      edge_iterator ei;

      /* Pull a block off the worklist.  */
      bb = *--tos;

      /* For each predecessor block.  */
      FOR_EACH_EDGE (e, ei, bb->preds)
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
      db_p->def_blocks = BITMAP_ALLOC (NULL);
      db_p->phi_blocks = BITMAP_ALLOC (NULL);
      db_p->livein_blocks = BITMAP_ALLOC (NULL);
      *slot = (void *) db_p;
    }
  else
    db_p = (struct def_blocks_d *) *slot;

  return db_p;
}


/* Mark block BB as the definition site for variable VAR.  PHI_P is true if
   VAR is defined by a PHI node.  */

static void
set_def_block (tree var, basic_block bb, bool phi_p)
{
  struct def_blocks_d *db_p;
  enum need_phi_state state;

  state = get_phi_state (var);
  db_p = get_def_blocks_for (var);

  /* Set the bit corresponding to the block where VAR is defined.  */
  bitmap_set_bit (db_p->def_blocks, bb->index);
  if (phi_p)
    bitmap_set_bit (db_p->phi_blocks, bb->index);

  /* Keep track of whether or not we may need to insert PHI nodes.

     If we are in the UNKNOWN state, then this is the first definition
     of VAR.  Additionally, we have not seen any uses of VAR yet, so
     we do not need a PHI node for this variable at this time (i.e.,
     transition to NEED_PHI_STATE_NO).

     If we are in any other state, then we either have multiple definitions
     of this variable occurring in different blocks or we saw a use of the
     variable which was not dominated by the block containing the
     definition(s).  In this case we may need a PHI node, so enter
     state NEED_PHI_STATE_MAYBE.  */
  if (state == NEED_PHI_STATE_UNKNOWN)
    set_phi_state (var, NEED_PHI_STATE_NO);
  else
    set_phi_state (var, NEED_PHI_STATE_MAYBE);
}


/* Mark block BB as having VAR live at the entry to BB.  */

static void
set_livein_block (tree var, basic_block bb)
{
  struct def_blocks_d *db_p;
  enum need_phi_state state = get_phi_state (var);

  db_p = get_def_blocks_for (var);

  /* Set the bit corresponding to the block where VAR is live in.  */
  bitmap_set_bit (db_p->livein_blocks, bb->index);

  /* Keep track of whether or not we may need to insert PHI nodes.

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
	set_phi_state (var, NEED_PHI_STATE_MAYBE);
    }
  else
    set_phi_state (var, NEED_PHI_STATE_MAYBE);
}


/* Return true if symbol SYM is marked for renaming.  */

static inline bool
symbol_marked_for_renaming (tree sym)
{
  gcc_assert (DECL_P (sym));
  return bitmap_bit_p (syms_to_rename, var_ann (sym)->uid);
}


/* Return true if NAME is in OLD_SSA_NAMES.  */

static inline bool
is_old_name (tree name)
{
  if (!need_to_replace_names_p)
    return false;

  return TEST_BIT (old_ssa_names, SSA_NAME_VERSION (name));
}


/* Return true if NAME is in NEW_SSA_NAMES.  */

static inline bool
is_new_name (tree name)
{
  if (!need_to_replace_names_p)
    return false;

  return TEST_BIT (new_ssa_names, SSA_NAME_VERSION (name));
}


/* Hashing and equality functions for REPL_TBL.  */

static hashval_t
repl_map_hash (const void *p)
{
  return htab_hash_pointer ((const void *)((const struct repl_map_d *)p)->name);
}

static int
repl_map_eq (const void *p1, const void *p2)
{
  return ((const struct repl_map_d *)p1)->name
	 == ((const struct repl_map_d *)p2)->name;
}

static void
repl_map_free (void *p)
{
  BITMAP_FREE (((struct repl_map_d *)p)->set);
  free (p);
}


/* Return the names replaced by NEW (i.e., REPL_TBL[NEW].SET).  */

static inline bitmap
names_replaced_by (tree new)
{
  struct repl_map_d m;
  void **slot;

  m.name = new;
  slot = htab_find_slot (repl_tbl, (void *) &m, NO_INSERT);

  /* If N was not registered in the replacement table, return NULL.  */
  if (slot == NULL || *slot == NULL)
    return NULL;

  return ((struct repl_map_d *) *slot)->set;
}


/* Add OLD to REPL_TBL[NEW].SET.  */

static inline void
add_to_repl_tbl (tree new, tree old)
{
  struct repl_map_d m, *mp;
  void **slot;

  m.name = new;
  slot = htab_find_slot (repl_tbl, (void *) &m, INSERT);
  if (*slot == NULL)
    {
      mp = xmalloc (sizeof (*mp));
      mp->name = new;
      mp->set = BITMAP_ALLOC (NULL);
      *slot = (void *) mp;
    }
  else
    mp = (struct repl_map_d *) *slot;

  bitmap_set_bit (mp->set, SSA_NAME_VERSION (old));
}


/* Add a new mapping NEW -> OLD REPL_TBL.  Every entry N_i in REPL_TBL
   represents the set of names O_1 ... O_j replaced by N_i.  This is
   used by update_ssa and its helpers to introduce new SSA names in an
   already formed SSA web.  */

static void
add_new_name_mapping (tree new, tree old)
{
  timevar_push (TV_TREE_SSA_INCREMENTAL);

  /* We may need to grow NEW_SSA_NAMES and OLD_SSA_NAMES because our
     caller may have created new names since the set was created.  */
  if (new_ssa_names->n_bits <= num_ssa_names - 1)
    {
      unsigned int new_sz = num_ssa_names + NAME_SETS_GROWTH_FACTOR;
      new_ssa_names = sbitmap_resize (new_ssa_names, new_sz, 0);
      old_ssa_names = sbitmap_resize (old_ssa_names, new_sz, 0);
    }

  /* We don't need to keep replacement mappings for virtual names.
     Since these names are kept in FUD-chain form, we need to traverse
     the CFG from ENTRY to repair FUD chains.  */
  if (!is_gimple_reg (new))
    {
      tree sym;

      gcc_assert (!is_gimple_reg (old));

      if (DECL_P (old))
	sym = new;
      else
	{
	  sym = SSA_NAME_VAR (old);
	  bitmap_set_bit (old_virtual_ssa_names, SSA_NAME_VERSION (old));
	}

      mark_sym_for_renaming (sym);
      need_to_update_vops_p = true;

      timevar_pop (TV_TREE_SSA_INCREMENTAL);

      return;
    }

  /* Assume that OLD and NEW are different GIMPLE register names.  */
  gcc_assert (new != old && is_gimple_reg (old));

  /* Update the REPL_TBL table.  */
  add_to_repl_tbl (new, old);

  /* If OLD had already been registered as a new name, then all the
     names that OLD replaces should also be replaced by NEW.  */
  if (is_new_name (old))
    bitmap_ior_into (names_replaced_by (new), names_replaced_by (old));

  /* Register NEW and OLD in NEW_SSA_NAMES and OLD_SSA_NAMES,
     respectively.  */
  SET_BIT (new_ssa_names, SSA_NAME_VERSION (new));
  SET_BIT (old_ssa_names, SSA_NAME_VERSION (old));

  /* Indicate that we are going to be replacing existing names.  */
  need_to_replace_names_p = true;

  timevar_pop (TV_TREE_SSA_INCREMENTAL);
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
  bitmap kills = gd->kills;
  tree stmt, def;
  use_operand_p use_p;
  def_operand_p def_p;
  ssa_op_iter iter;

  stmt = bsi_stmt (bsi);
  update_stmt_if_modified (stmt);

  REGISTER_DEFS_IN_THIS_STMT (stmt) = 0;
  REWRITE_THIS_STMT (stmt) = 0;

  /* If a variable is used before being set, then the variable is live
     across a block boundary, so mark it live-on-entry to BB.  */
  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter,
			    SSA_OP_USE | SSA_OP_VUSE | SSA_OP_VMUSTDEFKILL)
    {
      tree sym = USE_FROM_PTR (use_p);
      gcc_assert (DECL_P (sym));
      if (!bitmap_bit_p (kills, var_ann (sym)->uid))
	set_livein_block (sym, bb);
      REWRITE_THIS_STMT (stmt) = 1;
    }
  
  /* Note that virtual definitions are irrelevant for computing KILLS
     because a V_MAY_DEF does not constitute a killing definition of the
     variable.  However, the operand of a virtual definitions is a use
     of the variable, so it may cause the variable to be considered
     live-on-entry.  */
  FOR_EACH_SSA_MAYDEF_OPERAND (def_p, use_p, stmt, iter)
    {
      tree sym = USE_FROM_PTR (use_p);
      gcc_assert (DECL_P (sym));
      set_livein_block (sym, bb);
      set_def_block (sym, bb, false);
      REGISTER_DEFS_IN_THIS_STMT (stmt) = 1;
      REWRITE_THIS_STMT (stmt) = 1;
    }

  /* Now process the defs and must-defs made by this statement.  */
  FOR_EACH_SSA_TREE_OPERAND (def, stmt, iter, SSA_OP_DEF | SSA_OP_VMUSTDEF)
    {
      gcc_assert (DECL_P (def));
      set_def_block (def, bb, false);
      bitmap_set_bit (kills, var_ann (def)->uid);
      REGISTER_DEFS_IN_THIS_STMT (stmt) = 1;
    }

  /* If we found the statement interesting then also mark the block BB
     as interesting.  */
  if (REWRITE_THIS_STMT (stmt) || REGISTER_DEFS_IN_THIS_STMT (stmt))
    SET_BIT (gd->interesting_blocks, bb->index);
}


/* Given a set of blocks with variable definitions (DEF_BLOCKS),
   return a bitmap with all the blocks in the iterated dominance
   frontier of the blocks in DEF_BLOCKS.  DFS contains dominance
   frontier information as returned by compute_dominance_frontiers.
   
   The resulting set of blocks are the potential sites where PHI nodes
   are needed.  The caller is responsible from freeing the memory
   allocated for the return value.  */

static bitmap
find_idf (bitmap def_blocks, bitmap *dfs)
{
  bitmap_iterator bi;
  unsigned bb_index;
  VEC(int) *work_stack;
  bitmap phi_insertion_points;

  work_stack = VEC_alloc (int, n_basic_blocks);
  phi_insertion_points = BITMAP_ALLOC (NULL);

  /* Seed the work list with all the blocks in DEF_BLOCKS.  */
  EXECUTE_IF_SET_IN_BITMAP (def_blocks, 0, bb_index, bi)
    /* We use VEC_quick_push here for speed.  This is safe because we
       know that the number of definition blocks is no greater than
       the number of basic blocks, which is the initial capacity of
       WORK_STACK.  */
    VEC_quick_push (int, work_stack, bb_index);

  /* Pop a block off the worklist, add every block that appears in
     the original block's DF that we have not already processed to
     the worklist.  Iterate until the worklist is empty.   Blocks
     which are added to the worklist are potential sites for
     PHI nodes.  */
  while (VEC_length (int, work_stack) > 0)
    {
      bb_index = VEC_pop (int, work_stack);

      /* Since the registration of NEW -> OLD name mappings is done
	 separately from the call to update_ssa, when updating the SSA
	 form, the basic blocks where new and/or old names are defined
	 may have disappeared by CFG cleanup calls.  In this case,
	 we may pull a non-existing block from the work stack.  */
      gcc_assert (bb_index < (unsigned) last_basic_block);

      EXECUTE_IF_AND_COMPL_IN_BITMAP (dfs[bb_index], phi_insertion_points,
	                              0, bb_index, bi)
	{
	  /* Use a safe push because if there is a definition of VAR
	     in every basic block, then WORK_STACK may eventually have
	     more than N_BASIC_BLOCK entries.  */
	  VEC_safe_push (int, work_stack, bb_index);
	  bitmap_set_bit (phi_insertion_points, bb_index);
	}
    }

  VEC_free (int, work_stack);

  return phi_insertion_points;
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


/* Retrieve or create a default definition for symbol SYM.  */

static inline tree
get_default_def_for (tree sym)
{
  tree ddef = default_def (sym);

  if (ddef == NULL_TREE)
    {
      ddef = make_ssa_name (sym, build_empty_stmt ());
      set_default_def (sym, ddef);
    }

  return ddef;
}


/* Insert PHI nodes for variable VAR using the iterated dominance
   frontier given in PHI_INSERTION_POINTS.  If UPDATE_P is true, this
   function assumes that the caller is incrementally updating the SSA
   form, in which case (1) VAR is assumed to be an SSA name, (2) a new
   SSA name is created for VAR's symbol, and, (3) all the arguments
   for the newly created PHI node are set to VAR.

   PHI_INSERTION_POINTS is updated to reflect nodes that already had a
   PHI node for VAR.  On exit, only the nodes that received a PHI node
   for VAR will be present in PHI_INSERTION_POINTS.  */

static void
insert_phi_nodes_for (tree var, bitmap phi_insertion_points, bool update_p)
{
  unsigned bb_index;
  edge e;
  tree phi;
  basic_block bb;
  bitmap_iterator bi;
  struct def_blocks_d *def_map;

  def_map = find_def_blocks_for (var);
  gcc_assert (def_map);

  /* Remove the blocks where we already have PHI nodes for VAR.  */
  bitmap_and_compl_into (phi_insertion_points, def_map->phi_blocks);

  /* Now compute global livein for this variable.  Note this modifies
     def_map->livein_blocks.  */
  compute_global_livein (def_map->livein_blocks, def_map->def_blocks);

  /* And insert the PHI nodes.  */
  EXECUTE_IF_AND_IN_BITMAP (phi_insertion_points, def_map->livein_blocks,
			    0, bb_index, bi)
    {
      bb = BASIC_BLOCK (bb_index);
      phi = create_phi_node (var, bb);

      if (TREE_CODE (var) == SSA_NAME)
	{
	  edge_iterator ei;

	  /* FIXME.  After removing rewrite_ssa_into_ssa, change this
	     if() to gcc_assert().  */
	  if (update_p)
	    {
	      /* If we are rewriting SSA names, create the LHS of the
		 PHI node by duplicating VAR.  This is useful in the
		 case of pointers, to also duplicate pointer
		 attributes (alias information, in particular).  */
	      tree new_lhs = duplicate_ssa_name (var, phi);
	      SET_PHI_RESULT (phi, new_lhs);
	      add_new_name_mapping (new_lhs, var);
	    }

	  /* Add VAR to every argument slot of PHI.  We need VAR in
	     every argument so that rewrite_update_phi_arguments knows
	     which name is this PHI node replacing.  If VAR is a
	     symbol marked for renaming, this is not necessary, the
	     renamer will use the symbol on the LHS to get its
	     reaching definition.  */
	  FOR_EACH_EDGE (e, ei, bb->preds)
	    add_phi_arg (phi, var, e);
	}

      /* Mark this PHI node as interesting for update_ssa.  */
      REGISTER_DEFS_IN_THIS_STMT (phi) = 1;
      REWRITE_THIS_STMT (phi) = 1;
    }
}


/* Helper for insert_phi_nodes.  If VAR needs PHI nodes, insert them
   at the dominance frontier (DFS) of blocks defining VAR.  */

static inline void
insert_phi_nodes_1 (tree var, bitmap *dfs)
{
  struct def_blocks_d *def_map;
  bitmap idf;

  def_map = find_def_blocks_for (var);
  if (def_map == NULL)
    return;

  if (get_phi_state (var) != NEED_PHI_STATE_NO)
    {
      idf = find_idf (def_map->def_blocks, dfs);
      insert_phi_nodes_for (var, idf, false);
      BITMAP_FREE (idf);
    }
}


/* Insert PHI nodes at the dominance frontier of blocks with variable
   definitions.  DFS contains the dominance frontier information for
   the flowgraph.  PHI nodes will only be inserted at the dominance
   frontier of definition blocks for variables whose NEED_PHI_STATE
   annotation is marked as ``maybe'' or ``unknown'' (computed by
   mark_def_sites).  If NAMES_TO_RENAME is not NULL, do the same but
   for ssa name rewriting.  */

static void
insert_phi_nodes (bitmap *dfs, bitmap names_to_rename)
{
  unsigned i;

  timevar_push (TV_TREE_INSERT_PHI_NODES);

  if (names_to_rename)
    {
      bitmap_iterator bi;

      EXECUTE_IF_SET_IN_BITMAP (names_to_rename, 0, i, bi)
	if (ssa_name (i))
	  insert_phi_nodes_1 (ssa_name (i), dfs);
    }
  else
    {
      for (i = 0; i < num_referenced_vars; i++)
	insert_phi_nodes_1 (referenced_var (i), dfs);
    }

  timevar_pop (TV_TREE_INSERT_PHI_NODES);
}


/* Register DEF (an SSA_NAME) to be a new definition for its underlying
   variable (SSA_NAME_VAR (DEF)) and push VAR's current reaching definition
   into the stack pointed by BLOCK_DEFS_P.  */

void
register_new_def (tree def, VEC (tree_on_heap) **block_defs_p)
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
  if (get_phi_state (var) == NEED_PHI_STATE_NO)
    {
      set_current_def (var, def);
      return;
    }

  currdef = get_current_def (var);

  /* Push the current reaching definition into *BLOCK_DEFS_P.  This stack is
     later used by the dominator tree callbacks to restore the reaching
     definitions for all the variables defined in the block after a recursive
     visit to all its immediately dominated blocks.  If there is no current
     reaching definition, then just record the underlying _DECL node.  */
  VEC_safe_push (tree_on_heap, *block_defs_p, currdef ? currdef : var);

  /* Set the current reaching definition for VAR to be DEF.  */
  set_current_def (var, def);
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

/* SSA Rewriting Step 1.  Initialization, create a block local stack
   of reaching definitions for new SSA names produced in this block
   (BLOCK_DEFS).  Register new definitions for every PHI node in the
   block.  */

static void
rewrite_initialize_block (struct dom_walk_data *walk_data ATTRIBUTE_UNUSED,
			  basic_block bb)
{
  tree phi;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n\nRenaming block #%d\n\n", bb->index);

  /* Mark the unwind point for this block.  */
  VEC_safe_push (tree_on_heap, block_defs_stack, NULL_TREE);

  /* Step 1.  Register new definitions for every PHI node in the block.
     Conceptually, all the PHI nodes are executed in parallel and each PHI
     node introduces a new version for the associated variable.  */
  for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
    {
      tree result = PHI_RESULT (phi);
      register_new_def (result, &block_defs_stack);
    }
}


/* Return the current definition for variable VAR.  If none is found,
   create a new SSA name to act as the zeroth definition for VAR.  If VAR
   is call clobbered and there exists a more recent definition of
   GLOBAL_VAR, return the definition for GLOBAL_VAR.  This means that VAR
   has been clobbered by a function call since its last assignment.  */

static tree
get_reaching_def (tree var)
{
  tree currdef_var, avar;
  
  /* Lookup the current reaching definition for VAR.  */
  currdef_var = get_current_def (var);

  /* If there is no reaching definition for VAR, create and register a
     default definition for it (if needed).  */
  if (currdef_var == NULL_TREE)
    {
      avar = DECL_P (var) ? var : SSA_NAME_VAR (var);
      currdef_var = get_default_def_for (avar);
      set_current_def (var, currdef_var);
    }

  /* Return the current reaching definition for VAR, or the default
     definition, if we had to create one.  */
  return currdef_var;
}


/* SSA Rewriting Step 2.  Rewrite every variable used in each statement in
   the block with its immediate reaching definitions.  Update the current
   definition of a variable when a new real or virtual definition is found.  */

static void
rewrite_stmt (struct dom_walk_data *walk_data ATTRIBUTE_UNUSED,
	      basic_block bb ATTRIBUTE_UNUSED,
	      block_stmt_iterator si)
{
  tree stmt;
  use_operand_p use_p;
  def_operand_p def_p;
  ssa_op_iter iter;

  stmt = bsi_stmt (si);

  /* If mark_def_sites decided that we don't need to rewrite this
     statement, ignore it.  */
  if (!REWRITE_THIS_STMT (stmt) && !REGISTER_DEFS_IN_THIS_STMT (stmt))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Renaming statement ");
      print_generic_stmt (dump_file, stmt, TDF_SLIM);
      fprintf (dump_file, "\n");
    }

  /* Step 1.  Rewrite USES and VUSES in the statement.  */
  if (REWRITE_THIS_STMT (stmt))
    FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter,
	                      SSA_OP_ALL_USES|SSA_OP_ALL_KILLS)
      {
	tree var = USE_FROM_PTR (use_p);
	gcc_assert (DECL_P (var));
	SET_USE (use_p, get_reaching_def (var));
      }

  /* Step 2.  Register the statement's DEF and VDEF operands.  */
  if (REGISTER_DEFS_IN_THIS_STMT (stmt))
    FOR_EACH_SSA_DEF_OPERAND (def_p, stmt, iter, SSA_OP_ALL_DEFS)
      {
	tree var = DEF_FROM_PTR (def_p);
	gcc_assert (DECL_P (var));
	SET_DEF (def_p, make_ssa_name (var, stmt));
	register_new_def (DEF_FROM_PTR (def_p), &block_defs_stack);
      }
}


/* SSA Rewriting Step 3.  Visit all the successor blocks of BB looking for
   PHI nodes.  For every PHI node found, add a new argument containing the
   current reaching definition for the variable and the edge through which
   that definition is reaching the PHI node.  */

static void
rewrite_add_phi_arguments (struct dom_walk_data *walk_data ATTRIBUTE_UNUSED,
			   basic_block bb)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      tree phi;

      for (phi = phi_nodes (e->dest); phi; phi = PHI_CHAIN (phi))
	{
	  tree currdef;
	  currdef = get_reaching_def (SSA_NAME_VAR (PHI_RESULT (phi)));
	  add_phi_arg (phi, currdef, e);
	}
    }
}


/* Called after visiting basic block BB.  Restore CURRDEFS to its
   original value.  */

static void
rewrite_finalize_block (struct dom_walk_data *walk_data ATTRIBUTE_UNUSED,
			basic_block bb ATTRIBUTE_UNUSED)
{
  /* Restore CURRDEFS to its original state.  */
  while (VEC_length (tree_on_heap, block_defs_stack) > 0)
    {
      tree tmp = VEC_pop (tree_on_heap, block_defs_stack);
      tree saved_def, var;

      if (tmp == NULL_TREE)
	break;

      /* If we recorded an SSA_NAME, then make the SSA_NAME the current
	 definition of its underlying variable.  If we recorded anything
	 else, it must have been an _DECL node and its current reaching
	 definition must have been NULL.  */
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
                                                                                
      set_current_def (var, saved_def);
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


/* Dump statistics for the hash table HTAB.  */

static void
htab_statistics (FILE *file, htab_t htab)
{
  fprintf (file, "size %ld, %ld elements, %f collision/search ratio\n",
	   (long) htab_size (htab),
	   (long) htab_elements (htab),
	   htab_collisions (htab));
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
  BITMAP_FREE (entry->def_blocks);
  BITMAP_FREE (entry->phi_blocks);
  BITMAP_FREE (entry->livein_blocks);
  free (entry);
}


/* Callback for htab_traverse to dump the DEF_BLOCKS hash table.  */

static int
debug_def_blocks_r (void **slot, void *data ATTRIBUTE_UNUSED)
{
  struct def_blocks_d *db_p = (struct def_blocks_d *) *slot;
  
  fprintf (stderr, "VAR: ");
  print_generic_expr (stderr, db_p->var, dump_flags);
  bitmap_print (stderr, db_p->def_blocks, ", DEF_BLOCKS: { ", "}");
  bitmap_print (stderr, db_p->livein_blocks, ", LIVEIN_BLOCKS: { ", "}\n");

  return 1;
}


/* Dump the DEF_BLOCKS hash table on stderr.  */

void
debug_def_blocks (void)
{
  htab_traverse (def_blocks, debug_def_blocks_r, NULL);
}


/* Register NEW_NAME to be the new reaching definition for OLD_NAME.  */

static inline void
register_new_update_single (tree new_name, tree old_name)
{
  tree currdef = get_current_def (old_name);

  /* Push the current reaching definition into *BLOCK_DEFS_P.
     This stack is later used by the dominator tree callbacks to
     restore the reaching definitions for all the variables
     defined in the block after a recursive visit to all its
     immediately dominated blocks.  */
  VEC_safe_push (tree_on_heap, block_defs_stack, currdef);
  VEC_safe_push (tree_on_heap, block_defs_stack, old_name);

  /* Set the current reaching definition for OLD_NAME to be
     NEW_NAME.  */
  set_current_def (old_name, new_name);
}


/* Register NEW_NAME to be the new reaching definition for all the
   names in OLD_NAMES.  Used by the incremental SSA update routines to
   replace old SSA names with new ones.  */

static inline void
register_new_update_set (tree new_name, bitmap old_names)
{
  bitmap_iterator bi;
  unsigned i;

  EXECUTE_IF_SET_IN_BITMAP (old_names, 0, i, bi)
    register_new_update_single (new_name, ssa_name (i));
}


/* Initialization of block data structures for the incremental SSA
   update pass.  Create a block local stack of reaching definitions
   for new SSA names produced in this block (BLOCK_DEFS).  Register
   new definitions for every PHI node in the block.  */

static void
rewrite_update_init_block (struct dom_walk_data *walk_data ATTRIBUTE_UNUSED,
		           basic_block bb)
{
  edge e;
  edge_iterator ei;
  tree phi;
  bool is_abnormal_phi;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n\nRegistering new PHI nodes in block #%d\n\n",
	     bb->index);

  /* Mark the unwind point for this block.  */
  VEC_safe_push (tree_on_heap, block_defs_stack, NULL_TREE);

  /* Mark the LHS if any of the arguments flows through an abnormal
     edge.  */
  is_abnormal_phi = false;
  FOR_EACH_EDGE (e, ei, bb->preds)
    if (e->flags & EDGE_ABNORMAL)
      {
	is_abnormal_phi = true;
	break;
      }

  /* If any of the PHI nodes is a replacement for a name in
     OLD_SSA_NAMES or it's one of the names in NEW_SSA_NAMES, then
     register it as a new definition for its corresponding name.  Also
     register definitions for names whose underlying symbols are
     marked for renaming.  */
  for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
    {
      tree lhs, lhs_sym;

      if (!REGISTER_DEFS_IN_THIS_STMT (phi))
	continue;
      
      lhs = PHI_RESULT (phi);
      lhs_sym = SSA_NAME_VAR (lhs);

      if (symbol_marked_for_renaming (lhs_sym))
	register_new_update_single (lhs, lhs_sym);
      else
	{
	  /* If LHS is a new name, register a new definition for all
	     the names replaced by LHS.  */
	  if (is_new_name (lhs))
	    register_new_update_set (lhs, names_replaced_by (lhs));
	  
	  /* If LHS is an OLD name, register it as a new definition
	     for itself.  */
	  if (is_old_name (lhs))
	    register_new_update_single (lhs, lhs);
	}

      if (is_abnormal_phi)
	SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs) = 1;
    }
}


/* Replace the operand pointed by USE_P with USE's current reaching
   definition.  */

static inline void
replace_use (use_operand_p use_p, tree use)
{
  tree rdef = get_reaching_def (use);
  if (rdef != use)
    SET_USE (use_p, rdef);
}


/* Called after visiting block BB.  Unwind BLOCK_DEFS_STACK to restore
   the current reaching definition of every name re-written in BB to
   the original reaching definition before visiting BB.  This
   unwinding must be done in the opposite order to what is done in
   register_new_update_set.  */

static void
rewrite_update_fini_block (struct dom_walk_data *walk_data ATTRIBUTE_UNUSED,
			   basic_block bb ATTRIBUTE_UNUSED)
{
  while (VEC_length (tree_on_heap, block_defs_stack) > 0)
    {
      tree var = VEC_pop (tree_on_heap, block_defs_stack);
      tree saved_def;
      
      /* NULL indicates the unwind stop point for this block (see
	 rewrite_update_init_block).  */
      if (var == NULL)
	return;

      saved_def = VEC_pop (tree_on_heap, block_defs_stack);
      set_current_def (var, saved_def);
    }
}


/* Update every variable used in the statement pointed-to by SI.  The
   statement is assumed to be in SSA form already.  Names in
   OLD_SSA_NAMES used by SI will be updated to their current reaching
   definition.  Names in OLD_SSA_NAMES or NEW_SSA_NAMES defined by SI
   will be registered as a new definition for their corresponding name
   in OLD_SSA_NAMES.  */

static void
rewrite_update_stmt (struct dom_walk_data *walk_data ATTRIBUTE_UNUSED,
		     basic_block bb ATTRIBUTE_UNUSED,
		     block_stmt_iterator si)
{
  stmt_ann_t ann;
  tree stmt;
  use_operand_p use_p;
  def_operand_p def_p;
  ssa_op_iter iter;

  stmt = bsi_stmt (si);
  ann = stmt_ann (stmt);

  /* Only update marked statements.  */
  if (!REWRITE_THIS_STMT (stmt) && !REGISTER_DEFS_IN_THIS_STMT (stmt))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Updating SSA information for statement ");
      print_generic_stmt (dump_file, stmt, TDF_SLIM);
      fprintf (dump_file, "\n");
    }

  /* Rewrite USES included in OLD_SSA_NAMES and USES whose underlying
     symbol is marked for renaming.  */
  if (REWRITE_THIS_STMT (stmt))
    {
      FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_USE)
	{
	  tree use = USE_FROM_PTR (use_p);
	  tree sym = DECL_P (use) ? use : SSA_NAME_VAR (use);

	  if (symbol_marked_for_renaming (sym))
	    replace_use (use_p, sym);
	  else if (is_old_name (use))
	    replace_use (use_p, use);
	}

      if (need_to_update_vops_p)
	FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter,
				  SSA_OP_VIRTUAL_USES | SSA_OP_VIRTUAL_KILLS)
	  {
	    tree use = USE_FROM_PTR (use_p);
	    tree sym = DECL_P (use) ? use : SSA_NAME_VAR (use);

	    if (symbol_marked_for_renaming (sym))
	      replace_use (use_p, sym);
	  }
    }

  /* Register definitions of names in NEW_SSA_NAMES and OLD_SSA_NAMES.
     Also register definitions for names whose underlying symbol is
     marked for renaming.  */
  if (REGISTER_DEFS_IN_THIS_STMT (stmt))
    {
      FOR_EACH_SSA_DEF_OPERAND (def_p, stmt, iter, SSA_OP_DEF)
	{
	  tree def = DEF_FROM_PTR (def_p);
	  tree sym = DECL_P (def) ? def : SSA_NAME_VAR (def);

	  /* If DEF is a naked symbol that needs renaming, create a
	     new name for it.  */
	  if (symbol_marked_for_renaming (sym))
	    {
	      if (DECL_P (def))
		{
		  def = make_ssa_name (def, stmt);
		  SET_DEF (def_p, def);
		}

	      register_new_update_single (def, sym);
	    }
	  else
	    {
	      /* If DEF is a new name, register it as a new definition
		 for all the names replaced by DEF.  */
	      if (is_new_name (def))
		register_new_update_set (def, names_replaced_by (def));

	      /* If DEF is an old name, register DEF as a new
		 definition for itself.  */
	      if (is_old_name (def))
		register_new_update_single (def, def);
	    }
	}

      if (need_to_update_vops_p)
	FOR_EACH_SSA_DEF_OPERAND (def_p, stmt, iter, SSA_OP_VIRTUAL_DEFS)
	  {
	    tree def = DEF_FROM_PTR (def_p);
	    tree sym = DECL_P (def) ? def : SSA_NAME_VAR (def);

	    if (symbol_marked_for_renaming (sym))
	      {
		if (DECL_P (def))
		  {
		    def = make_ssa_name (def, stmt);
		    SET_DEF (def_p, def);
		  }

		register_new_update_single (def, sym);
	      }
	  }
    }
}


/* Visit all the successor blocks of BB looking for PHI nodes.  For
   every PHI node found, check if any of its arguments is in
   OLD_SSA_NAMES.  If so, and if the argument has a current reaching
   definition, replace it.  */

static void
rewrite_update_phi_arguments (struct dom_walk_data *walk_data ATTRIBUTE_UNUSED,
			      basic_block bb)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      tree phi;

      for (phi = phi_nodes (e->dest); phi; phi = PHI_CHAIN (phi))
	{
	  tree arg;
	  use_operand_p arg_p;

	  /* Skip PHI nodes that are not marked for rewrite.  */
	  if (!REWRITE_THIS_STMT (phi))
	    continue;

	  arg_p = PHI_ARG_DEF_PTR_FROM_EDGE (phi, e);
	  arg = USE_FROM_PTR (arg_p);

	  if (arg && !DECL_P (arg) && TREE_CODE (arg) != SSA_NAME)
	    continue;

	  if (arg == NULL_TREE)
	    {
	      /* When updating a PHI node for a recently introduced
		 symbol we may find NULL arguments.  That's why we
		 take the symbol from the LHS of the PHI node.  */
	      replace_use (arg_p, SSA_NAME_VAR (PHI_RESULT (phi)));
	    }
	  else
	    {
	      tree sym = DECL_P (arg) ? arg : SSA_NAME_VAR (arg);

	      if (symbol_marked_for_renaming (sym))
		replace_use (arg_p, sym);
	      else if (is_old_name (arg))
		replace_use (arg_p, arg);
	    }

	  if (e->flags & EDGE_ABNORMAL)
	    SSA_NAME_OCCURS_IN_ABNORMAL_PHI (USE_FROM_PTR (arg_p)) = 1;
	}
    }
}


/* Rewrite the actual blocks, statements, and PHI arguments, to be in SSA
   form.  

   ENTRY indicates the block where to start.  Every block dominated by
      ENTRY will be rewritten.

   WHAT indicates what actions will be taken by the renamer (see enum
      rewrite_mode).

   BLOCKS are the set of interesting blocks for the dominator walker
      to process.  If this set is NULL, then all the nodes dominated
      by ENTRY are walked.  Otherwise, blocks dominated by ENTRY that
      are not present in BLOCKS are ignored.  */

static void
rewrite_blocks (basic_block entry, enum rewrite_mode what, sbitmap blocks)
{
  struct dom_walk_data walk_data;
  
  /* Rewrite all the basic blocks in the program.  */
  timevar_push (TV_TREE_SSA_REWRITE_BLOCKS);

  /* Setup callbacks for the generic dominator tree walker.  */
  memset (&walk_data, 0, sizeof (walk_data));

  walk_data.dom_direction = CDI_DOMINATORS;
  walk_data.interesting_blocks = blocks;

  if (what == REWRITE_UPDATE)
    walk_data.before_dom_children_before_stmts = rewrite_update_init_block;
  else
    walk_data.before_dom_children_before_stmts = rewrite_initialize_block;

  if (what == REWRITE_ALL)
    walk_data.before_dom_children_walk_stmts = rewrite_stmt;
  else if (what == REWRITE_UPDATE)
    walk_data.before_dom_children_walk_stmts = rewrite_update_stmt;
  else
    gcc_unreachable ();

  if (what == REWRITE_ALL)
    walk_data.before_dom_children_after_stmts = rewrite_add_phi_arguments;
  else if (what == REWRITE_UPDATE)
    walk_data.before_dom_children_after_stmts = rewrite_update_phi_arguments;
  else
    gcc_unreachable ();
  
  if (what == REWRITE_ALL)
    walk_data.after_dom_children_after_stmts =  rewrite_finalize_block;
  else if (what == REWRITE_UPDATE)
    walk_data.after_dom_children_after_stmts = rewrite_update_fini_block;
  else
    gcc_unreachable ();

  block_defs_stack = VEC_alloc (tree_on_heap, 10);

  /* Initialize the dominator walker.  */
  init_walk_dominator_tree (&walk_data);

  /* Recursively walk the dominator tree rewriting each statement in
     each basic block.  */
  walk_dominator_tree (&walk_data, entry);

  /* Finalize the dominator walker.  */
  fini_walk_dominator_tree (&walk_data);

  /* Debugging dumps.  */
  if (dump_file && (dump_flags & TDF_STATS))
    {
      dump_dfa_stats (dump_file);
      if (def_blocks)
	dump_tree_ssa_stats (dump_file);
    }

  if (def_blocks)
    {
      htab_delete (def_blocks);
      def_blocks = NULL;
    }
  
  VEC_free (tree_on_heap, block_defs_stack);
  block_defs_stack = NULL;

  timevar_pop (TV_TREE_SSA_REWRITE_BLOCKS);
}


/* Block initialization routine for mark_def_sites.  Clear the 
   KILLS bitmap at the start of each block.  */

static void
mark_def_sites_initialize_block (struct dom_walk_data *walk_data,
				 basic_block bb ATTRIBUTE_UNUSED)
{
  struct mark_def_sites_global_data *gd = walk_data->global_data;
  bitmap kills = gd->kills;
  bitmap_clear (kills);
}


/* Mark the definition site blocks for each variable, so that we know
   where the variable is actually live.

   INTERESTING_BLOCKS will be filled in with all the blocks that
      should be processed by the renamer.  It is assumed to be
      initialized and zeroed by the caller.  */

static void
mark_def_site_blocks (sbitmap interesting_blocks)
{
  size_t i;
  struct dom_walk_data walk_data;
  struct mark_def_sites_global_data mark_def_sites_global_data;

  /* Allocate memory for the DEF_BLOCKS hash table.  */
  def_blocks = htab_create (VARRAY_ACTIVE_SIZE (referenced_vars),
			    def_blocks_hash, def_blocks_eq, def_blocks_free);

  for (i = 0; i < num_referenced_vars; i++)
    set_current_def (referenced_var (i), NULL_TREE);

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
  walk_data.interesting_blocks = NULL;

  /* Notice that this bitmap is indexed using variable UIDs, so it must be
     large enough to accommodate all the variables referenced in the
     function, not just the ones we are renaming.  */
  mark_def_sites_global_data.kills = BITMAP_ALLOC (NULL);

  /* Create the set of interesting blocks that will be filled by
     mark_def_sites.  */
  mark_def_sites_global_data.interesting_blocks = interesting_blocks;
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
  BITMAP_FREE (mark_def_sites_global_data.kills);
}


/* Main entry point into the SSA builder.  The renaming process
   proceeds in four main phases:

   1- Compute dominance frontier and immediate dominators, needed to
      insert PHI nodes and rename the function in dominator tree
      order.

   2- Find and mark all the blocks that define variables
      (mark_def_site_blocks).

   3- Insert PHI nodes at dominance frontiers (insert_phi_nodes).

   4- Rename all the blocks (rewrite_blocks) and statements in the program.

   Steps 3 and 5 are done using the dominator tree walker
   (walk_dominator_tree).  */

static void
rewrite_into_ssa (void)
{
  bitmap *dfs;
  basic_block bb;
  sbitmap interesting_blocks;
  
  timevar_push (TV_TREE_SSA_OTHER);

  /* Initialize operand data structures.  */
  init_ssa_operands ();

  /* Initialize the set of interesting blocks.  The callback
     mark_def_sites will add to this set those blocks that the renamer
     should process.  */
  interesting_blocks = sbitmap_alloc (last_basic_block);
  sbitmap_zero (interesting_blocks);

  /* Initialize dominance frontier.  */
  dfs = (bitmap *) xmalloc (last_basic_block * sizeof (bitmap *));
  FOR_EACH_BB (bb)
    dfs[bb->index] = BITMAP_ALLOC (NULL);

  /* 1- Compute dominance frontiers.  */
  calculate_dominance_info (CDI_DOMINATORS);
  compute_dominance_frontiers (dfs);

  /* 2- Find and mark definition sites.  */
  mark_def_site_blocks (interesting_blocks);

  /* 3- Insert PHI nodes at dominance frontiers of definition blocks.  */
  insert_phi_nodes (dfs, NULL);

  /* 4- Rename all the blocks.  */
  rewrite_blocks (ENTRY_BLOCK_PTR, REWRITE_ALL, interesting_blocks);

  /* Free allocated memory.  */
  FOR_EACH_BB (bb)
    BITMAP_FREE (dfs[bb->index]);
  free (dfs);
  sbitmap_free (interesting_blocks);

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
  TODO_dump_func | TODO_verify_ssa,	/* todo_flags_finish */
  0					/* letter */
};


/* Mark the definition of VAR at STMT and BB as interesting for the
   renamer.  BLOCKS is the set of blocks that need updating.  */

static void
mark_def_interesting (tree var, tree stmt, basic_block bb, bitmap blocks,
		      bool insert_phi_p)
{
  REGISTER_DEFS_IN_THIS_STMT (stmt) = 1;
  bitmap_set_bit (blocks, bb->index);

  if (insert_phi_p)
    {
      bool is_phi_p = TREE_CODE (stmt) == PHI_NODE;

#if defined ENABLE_CHECKING
      /* If VAR is a virtual, then it had better be a symbol.
	 Virtuals are in FUD-chain form, so we are interested in the
	 definition and use sites of the symbol, not the individual
	 SSA names.  */
      if (!is_gimple_reg (var))
	gcc_assert (DECL_P (var));
#endif

      set_def_block (var, bb, is_phi_p);

      /* If VAR is an SSA name in NEW_SSA_NAMES, this is a definition
	 site for both itself and all the old names replaced by it.  */
      if (TREE_CODE (var) == SSA_NAME && is_new_name (var))
	{
	  bitmap_iterator bi;
	  unsigned i;
	  bitmap set = names_replaced_by (var);
	  if (set)
	    EXECUTE_IF_SET_IN_BITMAP (set, 0, i, bi)
	      set_def_block (ssa_name (i), bb, is_phi_p);
	}
    }
}


/* Mark the use of VAR at STMT and BB as interesting for the
   renamer.  INSERT_PHI_P is true if we are going to insert new PHI
   nodes.  BLOCKS is the set of blocks that need updating.  */

static inline void
mark_use_interesting (tree var, tree stmt, basic_block bb, bitmap blocks,
		      bool insert_phi_p)
{
  REWRITE_THIS_STMT (stmt) = 1;
  bitmap_set_bit (blocks, bb->index);

  /* If VAR has not been defined in BB, then it is live-on-entry
     to BB.  Note that we cannot just use the block holding VAR's
     definition because if VAR is one of the names in OLD_SSA_NAMES,
     it will have several definitions (itself and all the names that
     replace it).  */
  if (insert_phi_p)
    {
      struct def_blocks_d *db_p;

#if defined ENABLE_CHECKING
      /* If VAR is a virtual, then it had better be a symbol.
	 Virtuals are in FUD-chain form, so we are interested in the
	 definition and use sites of the symbol, not the individual
	 SSA names.  */
      if (!is_gimple_reg (var))
	gcc_assert (DECL_P (var));
#endif

      db_p = get_def_blocks_for (var);
      if (!bitmap_bit_p (db_p->def_blocks, bb->index))
	set_livein_block (var, bb);
    }
}


/* If any of the arguments of PHI is in OLD_SSA_NAMES, mark PHI to
   be rewritten.  BB is the block where PHI resides, BLOCKS is the
   region to be renamed and INSERT_PHI_P is true if the updating
   process should insert new PHI nodes.  */

static void
prepare_phi_args_for_update (tree phi, basic_block bb, bitmap blocks,
                             bool insert_phi_p)
{
  int i;

  for (i = 0; i < PHI_NUM_ARGS (phi); i++)
    {
      tree arg = PHI_ARG_DEF (phi, i);

      if (TREE_CODE (arg) == SSA_NAME && is_old_name (arg))
	{
	  /* Mark this use of ARG interesting for the renamer.  Notice
	     that we explicitly call mark_use_interesting with
	     INSERT_PHI_P == false.

	     This is to avoid marking ARG as live-in in this block BB.
	     If we were to mark ARG live-in to BB, then ARG would be
	     considered live-in through ALL incoming edges to BB which
	     is not what we want.  Since we are updating the SSA form
	     for ARG, we don't really know what other names of ARG are
	     coming in through other edges into BB.

	     If we considered ARG live-in at BB, then the PHI
	     placement algorithm may try to insert PHI nodes in blocks
	     that are not only unnecessary but also the renamer would
	     not know how to fill in.  */
	  mark_use_interesting (arg, phi, bb, blocks, false);

	  /* As discussed above, we only want to mark ARG live-in
	     through the edge corresponding to its slot inside the PHI
	     argument list.  So, we look for the block BB1 where ARG is
	     flowing through.  If BB1 does not contain a definition of
	     ARG, then consider ARG live-in at BB1.  */
	  if (insert_phi_p)
	    {
	      edge e = PHI_ARG_EDGE (phi, i);
	      basic_block bb1 = e->src;
	      struct def_blocks_d *db = get_def_blocks_for (arg);

	      if (!bitmap_bit_p (db->def_blocks, bb1->index))
		set_livein_block (arg, bb1);
	    }
	}
    }
}


/* Do a dominator walk starting at BB processing statements that
   reference variables in OLD_SSA_NAMES and NEW_SSA_NAMES.

   1- Mark in BLOCKS the defining block of every name N in
      NEW_SSA_NAMES.

   2- Mark in BLOCKS the defining block of every name O in
      OLD_SSA_NAMES.

   3- For every statement or PHI node that uses a name O in
      OLD_SSA_NAMES.  If INSERT_PHI_P is true, mark those uses as live
      in the corresponding block.  This is later used by the PHI
      placement algorithm to make PHI pruning decisions.

   If VISIT_DOM_P is true, all the dominator children of BB are also
   visited.

   FIXME.  This process is slower than necessary.  Once we have
   immediate uses merged in, we should be able to just visit the
   immediate uses of all the names that we are about to replace,
   instead of visiting the whole block.  */

static void
prepare_block_for_update (basic_block bb, bool insert_phi_p,
                          bitmap blocks, bool visit_dom_p)
{
  basic_block son;
  block_stmt_iterator si;
  tree phi;

  /* Process PHI nodes marking interesting those that define or use
     the names that we are interested in.  */
  for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
    {
      tree lhs_sym, lhs = PHI_RESULT (phi);

      REWRITE_THIS_STMT (phi) = 0;
      REGISTER_DEFS_IN_THIS_STMT (phi) = 0;

      /* Ignore virtual PHIs if we are not updating virtual operands.
	 Note that even if NEED_TO_REPLACE_NAMES_P is false, we need
	 to process real PHIs because we may be rewriting GIMPLE regs
	 into SSA for the first time.  Therefore, we cannot do a
	 similar shortcut for real PHIs.  */
      if (!need_to_update_vops_p && !is_gimple_reg (lhs))
	continue;

      lhs_sym = DECL_P (lhs) ? lhs : SSA_NAME_VAR (lhs);

      if (symbol_marked_for_renaming (lhs_sym))
	{
	  /* If the LHS is a virtual symbol marked for renaming, then
	     we don't need to scan the argument list.  Since virtual
	     operands are in FUD-chain form, all the arguments of this
	     PHI must be the same symbol as the LHS.  So, we just need
	     to mark this site as both an interesting use and an
	     interesting def for the symbol.  */
	  mark_use_interesting (lhs_sym, phi, bb, blocks, insert_phi_p);
	  mark_def_interesting (lhs_sym, phi, bb, blocks, insert_phi_p);
	}
      else if (need_to_replace_names_p)
	{
	  /* If the LHS is in OLD_SSA_NAMES or NEW_SSA_NAMES, this is
	     a definition site for it.  */
	  if (is_old_name (lhs) || is_new_name (lhs))
	    mark_def_interesting (lhs, phi, bb, blocks, insert_phi_p);

	  prepare_phi_args_for_update (phi, bb, blocks, insert_phi_p);
	}
    }

  /* Process the statements.  */
  for (si = bsi_start (bb); !bsi_end_p (si); bsi_next (&si))
    {
      tree stmt;
      ssa_op_iter i;
      use_operand_p use_p;
      def_operand_p def_p;
      
      stmt = bsi_stmt (si);

      REWRITE_THIS_STMT (stmt) = 0;
      REGISTER_DEFS_IN_THIS_STMT (stmt) = 0;

      /* Note, even if NEED_TO_REPLACE_NAMES_P is false, we need to
	 scan real uses and defs, as we may be renaming a GIMPLE
	 register for the first time.  */
      FOR_EACH_SSA_USE_OPERAND (use_p, stmt, i, SSA_OP_USE)
	{
	  tree use = USE_FROM_PTR (use_p);
	  tree sym = DECL_P (use) ? use : SSA_NAME_VAR (use);
	  if (symbol_marked_for_renaming (sym) || is_old_name (use))
	    mark_use_interesting (use, stmt, bb, blocks, insert_phi_p);
	}

      FOR_EACH_SSA_DEF_OPERAND (def_p, stmt, i, SSA_OP_DEF)
	{
	  tree def = DEF_FROM_PTR (def_p);
	  tree sym = DECL_P (def) ? def : SSA_NAME_VAR (def);

	  if (symbol_marked_for_renaming (sym)
	      || is_new_name (def)
	      || is_old_name (def))
	    mark_def_interesting (def, stmt, bb, blocks, insert_phi_p);
	}

      /* If we don't need to update virtual operands, continue to the
	 next statement.  */
      if (!need_to_update_vops_p)
	continue;

      /* For every interesting N_i = V_MAY_DEF <N_j> and
	 N_i = V_MUST_DEF <N_j>, mark the statement as interesting.
	 Notice that N_j may in fact be a naked symbol (if this
	 statement is the result of basic block duplication). The
	 rename process will later fill in the appropriate reaching
	 definition for the symbol.  */
      FOR_EACH_SSA_DEF_OPERAND (def_p, stmt, i, SSA_OP_VIRTUAL_DEFS)
	{
	  tree def = DEF_FROM_PTR (def_p);
	  tree sym = DECL_P (def) ? def : SSA_NAME_VAR (def);

	  if (symbol_marked_for_renaming (sym))
	    {
	      mark_use_interesting (sym, stmt, bb, blocks, insert_phi_p);
	      mark_def_interesting (sym, stmt, bb, blocks, insert_phi_p);
	    }
	}

      /* Similarly, for V_USE <N_i>.  */
      FOR_EACH_SSA_USE_OPERAND (use_p, stmt, i, SSA_OP_VUSE)
	{
	  tree use = USE_FROM_PTR (use_p);
	  tree sym = DECL_P (use) ? use : SSA_NAME_VAR (use);

	  if (symbol_marked_for_renaming (sym))
	    mark_use_interesting (sym, stmt, bb, blocks, insert_phi_p);
	}
    }

  /* Now visit all the blocks dominated by BB.  */
  if (visit_dom_p)
    for (son = first_dom_son (CDI_DOMINATORS, bb);
	 son;
	 son = next_dom_son (CDI_DOMINATORS, son))
      prepare_block_for_update (son, insert_phi_p, blocks, true);
}


/* Helper for prepare_def_sites.  Mark the definition site for NAME as
   interesting.  BLOCKS and INSERT_PHI_P are as in prepare_def_sites.  */

static void
prepare_def_site_for (tree name, bitmap blocks, bool insert_phi_p)
{
  tree stmt;
  basic_block bb;

  gcc_assert (name && is_gimple_reg (name));
  gcc_assert (names_to_release == NULL
	      || !bitmap_bit_p (names_to_release, SSA_NAME_VERSION (name)));

  stmt = SSA_NAME_DEF_STMT (name);
  bb = bb_for_stmt (stmt);
  if (bb)
    {
      gcc_assert (bb->index < last_basic_block);
      mark_def_interesting (name, stmt, bb, blocks, insert_phi_p);
    }
}


/* Mark definition sites of names in NEW_SSA_NAMES and OLD_SSA_NAMES.
   Add each definition block to BLOCKS.  INSERT_PHI_P is true if the
   caller wants to insert PHI nodes for newly created names.  */

static void
prepare_def_sites (bitmap blocks, bool insert_phi_p)
{
  unsigned i;
  bitmap_iterator bi;

  /* If a name N from NEW_SSA_NAMES is also marked to be released,
     remove it from NEW_SSA_NAMES so that we don't try to visit its
     defining basic block (which most likely doesn't exist).  Notice
     that we cannot do the same with names in OLD_SSA_NAMES because we
     want to replace existing instances.  */
  if (names_to_release)
    EXECUTE_IF_SET_IN_BITMAP (names_to_release, 0, i, bi)
      RESET_BIT (new_ssa_names, i);

  /* If an old name is in NAMES_TO_RELEASE, we cannot remove it from
     OLD_SSA_NAMES, but we have to ignore its definition site.  */
  EXECUTE_IF_SET_IN_SBITMAP (old_ssa_names, 0, i,
    if (names_to_release == NULL || !bitmap_bit_p (names_to_release, i))
      prepare_def_site_for (ssa_name (i), blocks, insert_phi_p));

  EXECUTE_IF_SET_IN_SBITMAP (new_ssa_names, 0, i,
    prepare_def_site_for (ssa_name (i), blocks, insert_phi_p));
}


/* Dump all the names replaced by NAME to FILE.  */

void
dump_names_replaced_by (FILE *file, tree name)
{
  unsigned i;
  bitmap old_set;
  bitmap_iterator bi;

  print_generic_expr (file, name, 0);
  fprintf (file, " -> { ");

  old_set = names_replaced_by (name);
  EXECUTE_IF_SET_IN_BITMAP (old_set, 0, i, bi)
    {
      print_generic_expr (file, ssa_name (i), 0);
      fprintf (file, " ");
    }

  fprintf (file, "}\n");
}


/* Dump all the names replaced by NAME to stderr.  */

void
debug_names_replaced_by (tree name)
{
  dump_names_replaced_by (stderr, name);
}


/* Dump the SSA name replacement table to FILE.  */

void
dump_repl_tbl (FILE *file)
{
  unsigned i;
  bitmap_iterator bi;

  if (!need_ssa_update_p ())
    return;

  if (new_ssa_names && sbitmap_first_set_bit (new_ssa_names) >= 0)
    {
      fprintf (file, "\nSSA replacement table\n");
      fprintf (file, "N_i -> { O_1 ... O_j } means that N_i replaces "
	             "O_1, ..., O_j\n\n");

      EXECUTE_IF_SET_IN_SBITMAP (new_ssa_names, 0, i,
	dump_names_replaced_by (file, ssa_name (i)));
    }

  if (syms_to_rename && !bitmap_empty_p (syms_to_rename))
    {
      fprintf (file, "\n\nSymbols to be put in SSA form\n\n");
      EXECUTE_IF_SET_IN_BITMAP (syms_to_rename, 0, i, bi)
	{
	  print_generic_expr (file, referenced_var (i), 0);
	  fprintf (file, " ");
	}
    }

  if (old_virtual_ssa_names && !bitmap_empty_p (old_virtual_ssa_names))
    {
      fprintf (file, "\n\nVirtual SSA names to be updated\n\n");
      EXECUTE_IF_SET_IN_BITMAP (old_virtual_ssa_names, 0, i, bi)
	{
	  print_generic_expr (file, ssa_name (i), 0);
	  fprintf (file, " ");
	}
    }

  if (names_to_release && !bitmap_empty_p (names_to_release))
    {
      fprintf (file, "\n\nSSA names to release after updating the SSA web\n\n");
      EXECUTE_IF_SET_IN_BITMAP (names_to_release, 0, i, bi)
	{
	  print_generic_expr (file, ssa_name (i), 0);
	  fprintf (file, " ");
	}
    }

  fprintf (file, "\n\n");
}


/* Dump the SSA name replacement table to stderr.  */

void
debug_repl_tbl (void)
{
  dump_repl_tbl (stderr);
}


/* Initialize data structures used for incremental SSA updates.  */

static void
init_update_ssa (void)
{
  /* Reserve 1/3 more than the current number of names.  The calls to
     add_new_name_mapping are typically done after creating new SSA
     names, so we'll need to reallocate these arrays.  */
  old_ssa_names = sbitmap_alloc (num_ssa_names + NAME_SETS_GROWTH_FACTOR);
  sbitmap_zero (old_ssa_names);

  new_ssa_names = sbitmap_alloc (num_ssa_names + NAME_SETS_GROWTH_FACTOR);
  sbitmap_zero (new_ssa_names);

  repl_tbl = htab_create (20, repl_map_hash, repl_map_eq, repl_map_free);
  need_to_initialize_update_ssa_p = false;
  need_to_update_vops_p = false;
  need_to_replace_names_p = false;
  syms_to_rename = BITMAP_ALLOC (NULL);
  old_virtual_ssa_names = BITMAP_ALLOC (NULL);
  names_to_release = NULL;
}


/* Deallocate data structures used for incremental SSA updates.  */

static void
delete_update_ssa (void)
{
  unsigned i;
  bitmap_iterator bi;

  sbitmap_free (old_ssa_names);
  old_ssa_names = NULL;

  sbitmap_free (new_ssa_names);
  new_ssa_names = NULL;

  htab_delete (repl_tbl);
  repl_tbl = NULL;

  need_to_initialize_update_ssa_p = true;
  need_to_update_vops_p = false;
  need_to_replace_names_p = false;
  BITMAP_FREE (syms_to_rename);
  BITMAP_FREE (old_virtual_ssa_names);

  if (names_to_release)
    {
      EXECUTE_IF_SET_IN_BITMAP (names_to_release, 0, i, bi)
	release_ssa_name (ssa_name (i));
      BITMAP_FREE (names_to_release);
    }

  for (i = 1; i < num_ssa_names; i++)
    {
      tree n = ssa_name (i);

      if (n)
	{
	  free (SSA_NAME_AUX (n));
	  SSA_NAME_AUX (n) = NULL;
	}
    }

  /* Unmark all the names we may have protected from being released in
     insert_updated_phi_nodes_for.  */
  unmark_all_for_rewrite ();
}


/* Create a new name for OLD_NAME in statement STMT and replace the
   operand pointed to by DEF_P with the newly created name.  Return
   the new name and register the replacement mapping <NEW, OLD> in
   update_ssa's tables.  */

tree
create_new_def_for (tree old_name, tree stmt, def_operand_p def)
{
  tree new_name = duplicate_ssa_name (old_name, stmt);

  SET_DEF (def, new_name);

  if (TREE_CODE (stmt) == PHI_NODE)
    {
      edge e;
      edge_iterator ei;
      basic_block bb = bb_for_stmt (stmt);

      /* If needed, mark NEW_NAME as occurring in an abnormal PHI node. */
      FOR_EACH_EDGE (e, ei, bb->preds)
	if (e->flags & EDGE_ABNORMAL)
	  {
	    SSA_NAME_OCCURS_IN_ABNORMAL_PHI (new_name) = 1;
	    break;
	  }
    }

  register_new_name_mapping (new_name, old_name);

  /* For the benefit of passes that will be updating the SSA form on
     their own, set the current reaching definition of OLD_NAME to be
     NEW_NAME.  */
  set_current_def (old_name, new_name);

  return new_name;
}


/* Register name NEW to be a replacement for name OLD.  This function
   must be called for every replacement that should be performed by
   update_ssa.  */

void
register_new_name_mapping (tree new, tree old)
{
  if (need_to_initialize_update_ssa_p)
    init_update_ssa ();

  add_new_name_mapping (new, old);
}


/* Register symbol SYM to be renamed by update_ssa.  */

void
mark_sym_for_renaming (tree sym)
{
  if (need_to_initialize_update_ssa_p)
    init_update_ssa ();

  bitmap_set_bit (syms_to_rename, var_ann (sym)->uid);

  if (!is_gimple_reg (sym))
    need_to_update_vops_p = true;
}


/* Register all the symbols in SET to be renamed by update_ssa.  */

void
mark_set_for_renaming (bitmap set)
{
  bitmap_iterator bi;
  unsigned i;

  if (need_to_initialize_update_ssa_p)
    init_update_ssa ();

  bitmap_ior_into (syms_to_rename, set);

  EXECUTE_IF_SET_IN_BITMAP (set, 0, i, bi)
    if (!is_gimple_reg (referenced_var (i)))
      {
	need_to_update_vops_p = true;
	break;
      }
}


/* Return true if there is any work to be done by update_ssa.  */

bool
need_ssa_update_p (void)
{
  return syms_to_rename || old_ssa_names || new_ssa_names;
}


/* Return true if name N has been registered in the replacement table.  */

bool
name_registered_for_update_p (tree n)
{
  if (!need_ssa_update_p ())
    return false;

  return is_new_name (n)
         || is_old_name (n)
	 || symbol_marked_for_renaming (SSA_NAME_VAR (n));
}


/* Return the set of all the SSA names marked to be replaced.  */

bitmap
ssa_names_to_replace (void)
{
  unsigned i;
  bitmap ret;
  
  ret = BITMAP_ALLOC (NULL);
  EXECUTE_IF_SET_IN_SBITMAP (old_ssa_names, 0, i,
    bitmap_set_bit (ret, i));

  bitmap_ior_into (ret, old_virtual_ssa_names);

  return ret;
}


/* Mark NAME to be released after update_ssa has finished.  */

void
release_ssa_name_after_update_ssa (tree name)
{
  gcc_assert (!need_to_initialize_update_ssa_p);

  if (names_to_release == NULL)
    names_to_release = BITMAP_ALLOC (NULL);

  bitmap_set_bit (names_to_release, SSA_NAME_VERSION (name));
}


/* Insert new PHI nodes to replace VAR.  DFS contains dominance
   frontier information.  BLOCKS is the set of blocks to be updated.

   This is slightly different than the regular PHI insertion
   algorithm.  The value of UPDATE_FLAGS controls how PHI nodes for
   real names (i.e., GIMPLE registers) are inserted:
 
   - If UPDATE_FLAGS == TODO_update_ssa, we are only interested in PHI
     nodes inside the region affected by the block that defines VAR
     and the blocks that define all its replacements.  All these
     definition blocks have been gathered by prepare_block_for_update
     and they are stored in DEF_BLOCKS[VAR]->DEF_BLOCKS.

     First, we compute the entry point to the region (ENTRY).  This is
     given by the nearest common dominator to all the definition
     blocks. When computing the iterated dominance frontier (IDF), any
     block not strictly dominated by ENTRY is ignored.

     We then call the standard PHI insertion algorithm with the pruned
     IDF.

   - If UPDATE_FLAGS == TODO_update_ssa_full_phi, the IDF for real
     names is not pruned.  PHI nodes are inserted at every IDF block.  */

static void
insert_updated_phi_nodes_for (tree var, bitmap *dfs, bitmap blocks,
                              unsigned update_flags)
{
  basic_block entry;
  struct def_blocks_d *db;
  bitmap idf, pruned_idf;
  bitmap_iterator bi;
  unsigned i;

#if defined ENABLE_CHECKING
  if (TREE_CODE (var) == SSA_NAME)
    gcc_assert (is_old_name (var));
  else
    gcc_assert (symbol_marked_for_renaming (var));
#endif

  /* Get all the definition sites for VAR.  */
  db = find_def_blocks_for (var);

  /* No need to do anything if there were no definitions to VAR.  */
  if (db == NULL || bitmap_empty_p (db->def_blocks))
    return;

  /* Compute the initial iterated dominance frontier.  */
  idf = find_idf (db->def_blocks, dfs);
  pruned_idf = BITMAP_ALLOC (NULL);

  if (TREE_CODE (var) == SSA_NAME)
    {
      if (update_flags == TODO_update_ssa)
	{
	  /* If doing regular SSA updates for GIMPLE registers, we are
	     only interested in IDF blocks dominated by the nearest
	     common dominator of all the definition blocks.  */
	  entry = nearest_common_dominator_for_set (CDI_DOMINATORS,
						    db->def_blocks);

	  if (entry != ENTRY_BLOCK_PTR)
	    EXECUTE_IF_SET_IN_BITMAP (idf, 0, i, bi)
	      if (BASIC_BLOCK (i) != entry
		  && dominated_by_p (CDI_DOMINATORS, BASIC_BLOCK (i), entry))
		bitmap_set_bit (pruned_idf, i);
	}
      else
	{
	  /* Otherwise, do not prune the IDF for VAR.  */
	  gcc_assert (update_flags == TODO_update_ssa_full_phi);
	  bitmap_copy (pruned_idf, idf);
	}
    }
  else
    {
      /* Otherwise, VAR is a symbol that needs to be put into SSA form
	 for the first time, so we need to compute the full IDF for
	 it.  */
      bitmap_copy (pruned_idf, idf);

      /* There may already be PHI nodes for VAR in the flowgraph.
	 Some of them are no longer necessary.  PRUNED_IDF is
	 the set of blocks that need PHI nodes for VAR and
	 DB.PHI_BLOCKS is the set of blocks that already contain a PHI
	 node for VAR.  Therefore, the set DB.PHI_BLOCKS - PRUNED_IDF
	 gives us the set of blocks that contain PHI nodes which are
	 no longer needed.  */
      if (!bitmap_empty_p (db->phi_blocks) && !bitmap_empty_p (pruned_idf))
	EXECUTE_IF_AND_COMPL_IN_BITMAP (db->phi_blocks, pruned_idf, 0, i, bi)
	  {
	    tree phi, prev;
	    unsigned ver;

	    phi = find_phi_node_for (BASIC_BLOCK (i), var, &prev);
	    
	    /* Protect the name on PHI's LHS from being released into
	       the SSA name free list.  Since we have still not
	       updated the SSA form of the program, there may be
	       instances of PHI's LHS in the IL.  */
	    ver = SSA_NAME_VERSION (PHI_RESULT (phi));
	    mark_for_rewrite (PHI_RESULT (phi));
	    release_ssa_name_after_update_ssa (PHI_RESULT (phi));
	    remove_phi_node (phi, prev);
	  }
    }

  if (!bitmap_empty_p (pruned_idf))
    {
      /* Make sure that PRUNED_IDF blocks and all their feeding blocks
	 are included in the region to be updated.  The feeding blocks
	 are important to guarantee that the PHI arguments are renamed
	 properly.  */
      bitmap_ior_into (blocks, pruned_idf);
      EXECUTE_IF_SET_IN_BITMAP (pruned_idf, 0, i, bi)
	{
	  edge e;
	  edge_iterator ei;
	  basic_block bb = BASIC_BLOCK (i);

	  FOR_EACH_EDGE (e, ei, bb->preds)
	    if (e->src->index >= 0)
	      bitmap_set_bit (blocks, e->src->index);
	}

      insert_phi_nodes_for (var, pruned_idf, true);
    }

  BITMAP_FREE (pruned_idf);
  BITMAP_FREE (idf);
}


/* Given a set of newly created SSA names (NEW_SSA_NAMES) and a set of
   existing SSA names (OLD_SSA_NAMES), update the SSA form so that:

   1- The names in OLD_SSA_NAMES dominated by the definitions of
      NEW_SSA_NAMES are all re-written to be reached by the
      appropriate definition from NEW_SSA_NAMES.

   2- If needed, new PHI nodes are added to the iterated dominance
      frontier of the blocks where each of NEW_SSA_NAMES are defined.

   The mapping between OLD_SSA_NAMES and NEW_SSA_NAMES is setup by
   calling register_new_name_mapping for every pair of names that the
   caller wants to replace.

   The caller identifies the new names that have been inserted and the
   names that need to be replaced by calling register_new_name_mapping
   for every pair <NEW, OLD>.  Note that the function assumes that the
   new names have already been inserted in the IL.

   For instance, given the following code:

     1	L0:
     2	x_1 = PHI (0, x_5)
     3	if (x_1 < 10)
     4	  if (x_1 > 7)
     5	    y_2 = 0
     6	  else
     7	    y_3 = x_1 + x_7
     8	  endif
     9	  x_5 = x_1 + 1
     10   goto L0;
     11	endif

   Suppose that we insert new names x_10 and x_11 (lines 4 and 8).

     1	L0:
     2	x_1 = PHI (0, x_5)
     3	if (x_1 < 10)
     4	  x_10 = ...
     5	  if (x_1 > 7)
     6	    y_2 = 0
     7	  else
     8	    x_11 = ...
     9	    y_3 = x_1 + x_7
     10	  endif
     11	  x_5 = x_1 + 1
     12	  goto L0;
     13	endif

   We want to replace all the uses of x_1 with the new definitions of
   x_10 and x_11.  Note that the only uses that should be replaced are
   those at lines 5, 9 and 11.  Also, the use of x_7 at line 9 should
   *not* be replaced (this is why we cannot just mark symbol 'x' for
   renaming).

   Additionally, we may need to insert a PHI node at line 11 because
   that is a merge point for x_10 and x_11.  So the use of x_1 at line
   11 will be replaced with the new PHI node.  The insertion of PHI
   nodes is optional.  They are not strictly necessary to preserve the
   SSA form, and depending on what the caller inserted, they may not
   even be useful for the optimizers.  UPDATE_FLAGS controls various
   aspects of how update_ssa operates, see the documentation for
   TODO_update_ssa*.  */

void
update_ssa (unsigned update_flags)
{
  bitmap *dfs, blocks;
  basic_block bb, start_bb;
  bitmap_iterator bi;
  unsigned i;
  sbitmap tmp;
  bool insert_phi_p;

  if (!need_ssa_update_p ())
    return;

  timevar_push (TV_TREE_SSA_INCREMENTAL);

  /* Ensure that the dominance information is up-to-date.  */
  calculate_dominance_info (CDI_DOMINATORS);

  /* Only one update flag should be set.  */
  gcc_assert (update_flags == TODO_update_ssa
              || update_flags == TODO_update_ssa_no_phi
	      || update_flags == TODO_update_ssa_full_phi
	      || update_flags == TODO_update_ssa_only_virtuals);

  /* If we only need to update virtuals, remove all the mappings for
     real names before proceeding.  */
  if (update_flags == TODO_update_ssa_only_virtuals)
    {
      sbitmap_zero (old_ssa_names);
      sbitmap_zero (new_ssa_names);
      htab_empty (repl_tbl);
      need_to_replace_names_p = false;
    }

  if (update_flags == TODO_update_ssa
      || update_flags == TODO_update_ssa_full_phi
      || update_flags == TODO_update_ssa_only_virtuals)
    insert_phi_p = true;
  else
    insert_phi_p = false;

  if (insert_phi_p)
    {
      /* If the caller requested PHI nodes to be added, compute
	 dominance frontiers and initialize live-in information data
	 structures (DEF_BLOCKS).  */
      dfs = (bitmap *) xmalloc (last_basic_block * sizeof (bitmap *));
      FOR_EACH_BB (bb)
	dfs[bb->index] = BITMAP_ALLOC (NULL);
      compute_dominance_frontiers (dfs);

      /* For each SSA name N, the DEF_BLOCKS table describes where the
	 name is defined, which blocks have PHI nodes for N, and which
	 blocks have uses of N (i.e., N is live-on-entry in those
	 blocks).  */
      def_blocks = htab_create (num_ssa_names, def_blocks_hash,
				def_blocks_eq, def_blocks_free);
    }
  else
    {
      dfs = NULL;
      def_blocks = NULL;
    }

  blocks = BITMAP_ALLOC (NULL);

  /* Determine the CFG region that we are going to update.  First add
     all the blocks that define each of the names in NEW_SSA_NAMES
     and OLD_SSA_NAMES.  */
  prepare_def_sites (blocks, insert_phi_p);

  /* Next, determine the nearest common dominator START_BB for all the
     blocks in the region.  */
  if (!bitmap_empty_p (syms_to_rename) || bitmap_empty_p (blocks))
    {
      /* If the region to update is seemingly empty, or if we have to
	 rename some symbols from scratch, we need to start the
	 process at the root of the CFG.

	 FIXME, it should be possible to determine the nearest block
	 that had a definition for each of the symbols that are marked
	 for updating.  For now this seems more work than it's worth.  */
      start_bb = ENTRY_BLOCK_PTR;
    }
  else
    start_bb = nearest_common_dominator_for_set (CDI_DOMINATORS, blocks);

  /* Traverse all the blocks dominated by START_BB.  Mark interesting
     blocks and statements and set local live-in information for the
     PHI placement heuristics.  */
  prepare_block_for_update (start_bb, insert_phi_p, blocks, true);

  /* If are going to insert PHI nodes, blocks in the dominance
     frontier of START_BB may be affected.  Note that we don't need to
     visit the dominator children of blocks in the dominance frontier
     of START_BB.  None of the changes inside this region can affect
     blocks on the outside.  */
  if (insert_phi_p && start_bb->index >= 0)
    EXECUTE_IF_SET_IN_BITMAP (dfs[start_bb->index], 0, i, bi)
      prepare_block_for_update (BASIC_BLOCK (i), insert_phi_p,
				blocks, false);

  /* If requested, insert PHI nodes at the iterated dominance frontier
     of every block making new definitions for names in OLD_SSA_NAMES
     and for symbols in SYMS_TO_RENAME.  */
  if (insert_phi_p)
    {
      if (sbitmap_first_set_bit (old_ssa_names) >= 0)
	{
	  /* insert_updated_phi_nodes_for will call
	     add_new_name_mapping when inserting new PHI nodes, so the
	     set OLD_SSA_NAMES will grow while we are traversing it
	     (but it will not gain any new members).  Copy
	     OLD_SSA_NAMES to a temporary for traversal.  */
	  sbitmap tmp = sbitmap_alloc (old_ssa_names->n_bits);
	  sbitmap_copy (tmp, old_ssa_names);
	  EXECUTE_IF_SET_IN_SBITMAP (tmp, 0, i,
	    insert_updated_phi_nodes_for (ssa_name (i), dfs, blocks,
	                                  update_flags));
	  sbitmap_free (tmp);
	}

      EXECUTE_IF_SET_IN_BITMAP (syms_to_rename, 0, i, bi)
	insert_updated_phi_nodes_for (referenced_var (i), dfs, blocks,
	                              update_flags);

      /* Insertion of PHI nodes may have added blocks to the region.
	 We need to re-compute START_BB to include the newly added
	 blocks.  */
      if (start_bb != ENTRY_BLOCK_PTR)
	start_bb = nearest_common_dominator_for_set (CDI_DOMINATORS, blocks);
    }

  /* Reset the current definition for name and symbol before renaming
     the sub-graph.  */
  if (update_flags == TODO_update_ssa_full_phi)
    {
      /* If we are not prunning the IDF for new PHI nodes, set the
	 current name of every GIMPLE register to NULL.  This way, PHI
	 arguments coming from edges with uninitialized values will be
	 renamed to use the symbol's default definition.  */
      EXECUTE_IF_SET_IN_SBITMAP (old_ssa_names, 0, i,
	set_current_def (ssa_name (i), NULL_TREE));
    }
  else
    {
      /* Otherwise, set each old name to be its current reaching
	 definition.  */
      EXECUTE_IF_SET_IN_SBITMAP (old_ssa_names, 0, i,
	set_current_def (ssa_name (i), NULL_TREE));
    }

  EXECUTE_IF_SET_IN_BITMAP (syms_to_rename, 0, i, bi)
    set_current_def (referenced_var (i), NULL_TREE);

  /* Now start the renaming process at START_BB.  */
  tmp = sbitmap_alloc (last_basic_block);
  sbitmap_zero (tmp);
  EXECUTE_IF_SET_IN_BITMAP (blocks, 0, i, bi)
    SET_BIT (tmp, i);

  rewrite_blocks (start_bb, REWRITE_UPDATE, tmp);

  sbitmap_free (tmp);

  /* Debugging dumps.  */
  if (dump_file)
    {
      int c;
      unsigned i;

      dump_repl_tbl (dump_file);

      fprintf (dump_file, "Incremental SSA update started at block: %d\n\n",
	       start_bb->index);

      c = 0;
      EXECUTE_IF_SET_IN_BITMAP (blocks, 0, i, bi)
	c++;
      fprintf (dump_file, "Number of blocks in CFG: %d\n", last_basic_block);
      fprintf (dump_file, "Number of blocks to update: %d (%3.0f%%)\n\n",
	       c, PERCENT (c, last_basic_block));

      if (dump_flags & TDF_DETAILS)
	{
	  fprintf (dump_file, "Affected blocks: ");
	  EXECUTE_IF_SET_IN_BITMAP (blocks, 0, i, bi)
	    fprintf (dump_file, "%u ", i);
	  fprintf (dump_file, "\n");
	}

      fprintf (dump_file, "\n\n");
    }

  /* Free allocated memory.  */
  if (insert_phi_p)
    {
      FOR_EACH_BB (bb)
	BITMAP_FREE (dfs[bb->index]);
      free (dfs);
    }

  BITMAP_FREE (blocks);
  delete_update_ssa ();

  timevar_pop (TV_TREE_SSA_INCREMENTAL);
}


/*---------------------------------------------------------------------------
    Functions to fix a program in invalid SSA form into valid SSA
    form.  The main entry point here is rewrite_ssa_into_ssa.
---------------------------------------------------------------------------*/

/* Called after visiting basic block BB.  Restore CURRDEFS to its
   original value.  */

static void
ssa_rewrite_finalize_block (struct dom_walk_data *walk_data ATTRIBUTE_UNUSED,
			    basic_block bb ATTRIBUTE_UNUSED)
{

  /* Step 5.  Restore the current reaching definition for each variable
     referenced in the block (in reverse order).  */
  while (VEC_length (tree_on_heap, block_defs_stack) > 0)
    {
      tree var = VEC_pop (tree_on_heap, block_defs_stack);
      tree saved_def;
      
      if (var == NULL)
	break;

      saved_def = VEC_pop (tree_on_heap, block_defs_stack);
      set_current_def (var, saved_def);
    }
}


/* Register DEF (an SSA_NAME) to be a new definition for the original
   ssa name VAR and push VAR's current reaching definition
   into the stack pointed by BLOCK_DEFS_P.  */

static void
ssa_register_new_def (tree var, tree def)
{
  tree currdef;
   
  /* If this variable is set in a single basic block and all uses are
     dominated by the set(s) in that single basic block, then there is
     nothing to do.  TODO we should not be called at all, and just
     keep the original name.  */
  if (get_phi_state (var) == NEED_PHI_STATE_NO)
    {
      set_current_def (var, def);
      return;
    }

  currdef = get_current_def (var);

  /* Push the current reaching definition into *BLOCK_DEFS_P.  This stack is
     later used by the dominator tree callbacks to restore the reaching
     definitions for all the variables defined in the block after a recursive
     visit to all its immediately dominated blocks.  */
  VEC_safe_push (tree_on_heap, block_defs_stack, currdef);
  VEC_safe_push (tree_on_heap, block_defs_stack, var);

  /* Set the current reaching definition for VAR to be DEF.  */
  set_current_def (var, def);
}


/* Same as rewrite_stmt, for rewriting ssa names.  */

static void
ssa_rewrite_stmt (struct dom_walk_data *walk_data,
		  basic_block bb ATTRIBUTE_UNUSED,
		  block_stmt_iterator si)
{
  stmt_ann_t ann;
  tree stmt, var;
  ssa_op_iter iter;
  use_operand_p use_p;
  def_operand_p def_p;
  sbitmap names_to_rename = walk_data->global_data;

  stmt = bsi_stmt (si);
  ann = stmt_ann (stmt);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Renaming statement ");
      print_generic_stmt (dump_file, stmt, TDF_SLIM);
      fprintf (dump_file, "\n");
    }

  /* We have just scanned the code for operands.  No statement should
     be modified.  */
  gcc_assert (!ann->modified);

  /* Step 1.  Rewrite USES and VUSES in the statement.  */
  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_ALL_USES | SSA_OP_ALL_KILLS)
    {
      if (TEST_BIT (names_to_rename, SSA_NAME_VERSION (USE_FROM_PTR (use_p))))
	SET_USE (use_p, get_reaching_def (USE_FROM_PTR (use_p)));
    }

  /* Step 2.  Register the statement's DEF and VDEF operands.  */
  FOR_EACH_SSA_DEF_OPERAND (def_p, stmt, iter, SSA_OP_ALL_DEFS)
    {
      var = DEF_FROM_PTR (def_p);

      if (!TEST_BIT (names_to_rename, SSA_NAME_VERSION (var)))
	continue;

      SET_DEF (def_p, duplicate_ssa_name (var, stmt));
      ssa_register_new_def (var, DEF_FROM_PTR (def_p));
    }
}


/* Ditto, for ssa name rewriting.  */

static void
ssa_rewrite_phi_arguments (struct dom_walk_data *walk_data, basic_block bb)
{
  edge e;
  sbitmap names_to_rename = walk_data->global_data;
  use_operand_p op;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      tree phi;

      if (e->dest == EXIT_BLOCK_PTR)
	continue;

      for (phi = phi_nodes (e->dest); phi; phi = PHI_CHAIN (phi))
	{
	  op = PHI_ARG_DEF_PTR_FROM_EDGE (phi, e);
	  if (TREE_CODE (USE_FROM_PTR (op)) != SSA_NAME)
	    continue;
	  
	  if (!TEST_BIT (names_to_rename, SSA_NAME_VERSION (USE_FROM_PTR (op))))
	    continue; 

	  SET_USE (op, get_reaching_def (USE_FROM_PTR (op)));
	  if (e->flags & EDGE_ABNORMAL)
	    SSA_NAME_OCCURS_IN_ABNORMAL_PHI (USE_FROM_PTR (op)) = 1;
	}
    }
}

/* Ditto, for rewriting ssa names.  */

static void
ssa_rewrite_initialize_block (struct dom_walk_data *walk_data, basic_block bb)
{
  tree phi, new_name;
  sbitmap names_to_rename = walk_data->global_data;
  edge e;
  bool abnormal_phi;
  edge_iterator ei;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n\nRenaming block #%d\n\n", bb->index);

  /* Mark the unwind point for this block.  */
  VEC_safe_push (tree_on_heap, block_defs_stack, NULL_TREE);

  FOR_EACH_EDGE (e, ei, bb->preds)
    if (e->flags & EDGE_ABNORMAL)
      break;
  abnormal_phi = (e != NULL);

  /* Step 1.  Register new definitions for every PHI node in the block.
     Conceptually, all the PHI nodes are executed in parallel and each PHI
     node introduces a new version for the associated variable.  */
  for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
    {
      tree result = PHI_RESULT (phi);

      if (TEST_BIT (names_to_rename, SSA_NAME_VERSION (result)))
	{
	  new_name = duplicate_ssa_name (result, phi);
	  SET_PHI_RESULT (phi, new_name);

	  if (abnormal_phi)
	    SSA_NAME_OCCURS_IN_ABNORMAL_PHI (new_name) = 1;
	  ssa_register_new_def (result, new_name);
	}
    }
}


/* Same as mark_def_sites, but works over SSA names.  */

static void
ssa_mark_def_sites (struct dom_walk_data *walk_data,
		    basic_block bb,
		    block_stmt_iterator bsi)
{
  struct mark_def_sites_global_data *gd = walk_data->global_data;
  bitmap kills = gd->kills;
  size_t uid, def_uid;
  tree stmt, use, def;
  ssa_op_iter iter;

  /* Mark all the blocks that have definitions for each variable in the
     names_to_rename bitmap.  */
  stmt = bsi_stmt (bsi);
  update_stmt_if_modified (stmt);

  /* If a variable is used before being set, then the variable is live
     across a block boundary, so mark it live-on-entry to BB.  */
  FOR_EACH_SSA_TREE_OPERAND (use, stmt, iter, SSA_OP_ALL_USES | SSA_OP_ALL_KILLS)
    {
      uid = SSA_NAME_VERSION (use);

      if (TEST_BIT (gd->names_to_rename, uid)
	  && !bitmap_bit_p (kills, uid))
	set_livein_block (use, bb);
    }
	  
  /* Now process the definition made by this statement.  Mark the
     variables in KILLS.  */
  FOR_EACH_SSA_TREE_OPERAND (def, stmt, iter, SSA_OP_ALL_DEFS)
    {
      def_uid = SSA_NAME_VERSION (def);

      if (TEST_BIT (gd->names_to_rename, def_uid))
	{
	  set_def_block (def, bb, false);
	  bitmap_set_bit (kills, def_uid);
	}
    }
}


/* Block initialization routine for mark_def_sites.  Clear the 
   KILLS bitmap at the start of each block.  */

static void
ssa_mark_def_sites_initialize_block (struct dom_walk_data *walk_data,
				     basic_block bb)
{
  struct mark_def_sites_global_data *gd = walk_data->global_data;
  bitmap kills = gd->kills;
  tree phi, def;
  unsigned def_uid;

  bitmap_clear (kills);

  for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
    {
      def = PHI_RESULT (phi);
      def_uid = SSA_NAME_VERSION (def);

      if (!TEST_BIT (gd->names_to_rename, def_uid))
	continue;

      set_def_block (def, bb, true);
      bitmap_set_bit (kills, def_uid);
    }
}

/* Marks ssa names used as arguments of phis at the end of BB.  */

static void
ssa_mark_phi_uses (struct dom_walk_data *walk_data, basic_block bb)
{
  struct mark_def_sites_global_data *gd = walk_data->global_data;
  bitmap kills = gd->kills;
  edge e;
  tree phi, use;
  unsigned uid;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      if (e->dest == EXIT_BLOCK_PTR)
	continue;

      for (phi = phi_nodes (e->dest); phi; phi = PHI_CHAIN (phi))
	{
	  use = PHI_ARG_DEF_FROM_EDGE (phi, e);
	  if (TREE_CODE (use) != SSA_NAME)
	    continue;

	  uid = SSA_NAME_VERSION (use);

	  if (TEST_BIT (gd->names_to_rename, uid)
	      && !bitmap_bit_p (kills, uid))
	    set_livein_block (use, bb);
	}
    }
}
       
   
/* The marked ssa names may have more than one definition;
   add PHI nodes and rewrite them to fix this.  */

void
rewrite_ssa_into_ssa (void)
{
  bitmap *dfs;
  basic_block bb;
  struct dom_walk_data walk_data;
  struct mark_def_sites_global_data mark_def_sites_global_data;
  unsigned i;
  sbitmap snames_to_rename;
  bitmap to_rename;
  bitmap_iterator bi;
  
  if (!any_marked_for_rewrite_p ())
    return;
  to_rename = marked_ssa_names ();

  timevar_push (TV_TREE_SSA_OTHER);

  /* Allocate memory for the DEF_BLOCKS hash table.  */
  def_blocks = htab_create (num_ssa_names,
			    def_blocks_hash, def_blocks_eq, def_blocks_free);

  /* Initialize dominance frontier and immediate dominator bitmaps. 
     Also count the number of predecessors for each block.  Doing so
     can save significant time during PHI insertion for large graphs.  */
  dfs = (bitmap *) xmalloc (last_basic_block * sizeof (bitmap *));
  FOR_EACH_BB (bb)
    dfs[bb->index] = BITMAP_ALLOC (NULL);

  /* Ensure that the dominance information is OK.  */
  calculate_dominance_info (CDI_DOMINATORS);

  /* Compute dominance frontiers.  */
  compute_dominance_frontiers (dfs);

  /* Setup callbacks for the generic dominator tree walker to find and
     mark definition sites.  */
  walk_data.walk_stmts_backward = false;
  walk_data.dom_direction = CDI_DOMINATORS;
  walk_data.interesting_blocks = NULL;
  walk_data.initialize_block_local_data = NULL;
  walk_data.before_dom_children_before_stmts
	  = ssa_mark_def_sites_initialize_block;
  walk_data.before_dom_children_walk_stmts = ssa_mark_def_sites;
  walk_data.before_dom_children_after_stmts = ssa_mark_phi_uses; 
  walk_data.after_dom_children_before_stmts =  NULL;
  walk_data.after_dom_children_walk_stmts =  NULL;
  walk_data.after_dom_children_after_stmts =  NULL;

  snames_to_rename = sbitmap_alloc (num_ssa_names);
  sbitmap_zero (snames_to_rename);
  EXECUTE_IF_SET_IN_BITMAP (to_rename, 0, i, bi)
    {
      SET_BIT (snames_to_rename, i);
      set_current_def (ssa_name (i), NULL_TREE);
    }

  mark_def_sites_global_data.kills = BITMAP_ALLOC (NULL);
  mark_def_sites_global_data.names_to_rename = snames_to_rename;
  walk_data.global_data = &mark_def_sites_global_data;

  block_defs_stack = VEC_alloc (tree_on_heap, 10);

  /* We do not have any local data.  */
  walk_data.block_local_data_size = 0;

  /* Initialize the dominator walker.  */
  init_walk_dominator_tree (&walk_data);

  /* Recursively walk the dominator tree.  */
  walk_dominator_tree (&walk_data, ENTRY_BLOCK_PTR);

  /* Finalize the dominator walker.  */
  fini_walk_dominator_tree (&walk_data);

  /* We no longer need this bitmap, clear and free it.  */
  BITMAP_FREE (mark_def_sites_global_data.kills);

  /* Insert PHI nodes at dominance frontiers of definition blocks.  */
  insert_phi_nodes (dfs, to_rename);

  /* Rewrite all the basic blocks in the program.  */
  timevar_push (TV_TREE_SSA_REWRITE_BLOCKS);

  /* Setup callbacks for the generic dominator tree walker.  */
  walk_data.walk_stmts_backward = false;
  walk_data.dom_direction = CDI_DOMINATORS;
  walk_data.interesting_blocks = NULL;
  walk_data.initialize_block_local_data = NULL;
  walk_data.before_dom_children_before_stmts = ssa_rewrite_initialize_block;
  walk_data.before_dom_children_walk_stmts = ssa_rewrite_stmt;
  walk_data.before_dom_children_after_stmts = ssa_rewrite_phi_arguments;
  walk_data.after_dom_children_before_stmts = NULL;
  walk_data.after_dom_children_walk_stmts =  NULL;
  walk_data.after_dom_children_after_stmts =  ssa_rewrite_finalize_block;
  walk_data.global_data = snames_to_rename;
  walk_data.block_local_data_size = 0;

  /* Initialize the dominator walker.  */
  init_walk_dominator_tree (&walk_data);

  /* Recursively walk the dominator tree rewriting each statement in
     each basic block.  */
  walk_dominator_tree (&walk_data, ENTRY_BLOCK_PTR);

  /* Finalize the dominator walker.  */
  fini_walk_dominator_tree (&walk_data);

  unmark_all_for_rewrite ();

  EXECUTE_IF_SET_IN_BITMAP (to_rename, 0, i, bi)
    {
      /* Free SSA_NAME_AUX.  We don't have to zero it because
	 release_ssa_name will.  */
      if (SSA_NAME_AUX (ssa_name (i)))
	free (SSA_NAME_AUX (ssa_name (i)));

      release_ssa_name (ssa_name (i));
    }

  sbitmap_free (snames_to_rename);

  timevar_pop (TV_TREE_SSA_REWRITE_BLOCKS);

  /* Debugging dumps.  */
  if (dump_file && (dump_flags & TDF_STATS))
    {
      dump_dfa_stats (dump_file);
      dump_tree_ssa_stats (dump_file);
    }

  /* Free allocated memory.  */
  FOR_EACH_BB (bb)
    BITMAP_FREE (dfs[bb->index]);
  free (dfs);

  htab_delete (def_blocks);

#ifdef ENABLE_CHECKING
  for (i = 1; i < num_ssa_names; i++)
    {
      tree name = ssa_name (i);
      if (!name)
	continue;

      gcc_assert (SSA_NAME_AUX (name) == NULL);
    }
#endif

  BITMAP_FREE (to_rename);
  
  VEC_free (tree_on_heap, block_defs_stack);
  block_defs_stack = NULL;
  timevar_pop (TV_TREE_SSA_OTHER);
}
