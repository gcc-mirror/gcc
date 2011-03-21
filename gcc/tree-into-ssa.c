/* Rewrite a program in Normal form into SSA.
   Copyright (C) 2001, 2002, 2003, 2004, 2005, 2007, 2008, 2009, 2010
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "flags.h"
#include "tm_p.h"
#include "langhooks.h"
#include "basic-block.h"
#include "output.h"
#include "function.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "bitmap.h"
#include "tree-flow.h"
#include "gimple.h"
#include "tree-inline.h"
#include "timevar.h"
#include "hashtab.h"
#include "tree-dump.h"
#include "tree-pass.h"
#include "cfgloop.h"
#include "domwalk.h"
#include "params.h"
#include "vecprim.h"


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

   - An SSA_NAME (N) indicates that the current definition of the
     underlying variable should be set to the given SSA_NAME.  If the
     symbol associated with the SSA_NAME is not a GIMPLE register, the
     next slot in the stack must be a _DECL node (SYM).  In this case,
     the name N in the previous slot is the current reaching
     definition for SYM.

   - A _DECL node indicates that the underlying variable has no
     current definition.

   - A NULL node at the top entry is used to mark the last slot
     associated with the current block.  */
static VEC(tree,heap) *block_defs_stack;


/* Set of existing SSA names being replaced by update_ssa.  */
static sbitmap old_ssa_names;

/* Set of new SSA names being added by update_ssa.  Note that both
   NEW_SSA_NAMES and OLD_SSA_NAMES are dense bitmaps because most of
   the operations done on them are presence tests.  */
static sbitmap new_ssa_names;

sbitmap interesting_blocks;

/* Set of SSA names that have been marked to be released after they
   were registered in the replacement table.  They will be finally
   released after we finish updating the SSA web.  */
static bitmap names_to_release;

static VEC(gimple_vec, heap) *phis_to_rewrite;

/* The bitmap of non-NULL elements of PHIS_TO_REWRITE.  */
static bitmap blocks_with_phis_to_rewrite;

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

/* The function the SSA updating data structures have been initialized for.
   NULL if they need to be initialized by register_new_name_mapping.  */
static struct function *update_ssa_initialized_fn = NULL;

/* Statistics kept by update_ssa to use in the virtual mapping
   heuristic.  If the number of virtual mappings is beyond certain
   threshold, the updater will switch from using the mappings into
   renaming the virtual symbols from scratch.  In some cases, the
   large number of name mappings for virtual names causes significant
   slowdowns in the PHI insertion code.  */
struct update_ssa_stats_d
{
  unsigned num_virtual_mappings;
  unsigned num_total_mappings;
  bitmap virtual_symbols;
  unsigned num_virtual_symbols;
};
static struct update_ssa_stats_d update_ssa_stats;

/* Global data to attach to the main dominator walk structure.  */
struct mark_def_sites_global_data
{
  /* This bitmap contains the variables which are set before they
     are used in a basic block.  */
  bitmap kills;
};


/* Information stored for SSA names.  */
struct ssa_name_info
{
  /* The current reaching definition replacing this SSA name.  */
  tree current_def;

  /* This field indicates whether or not the variable may need PHI nodes.
     See the enum's definition for more detailed information about the
     states.  */
  ENUM_BITFIELD (need_phi_state) need_phi_state : 2;

  /* Age of this record (so that info_for_ssa_name table can be cleared
     quickly); if AGE < CURRENT_INFO_FOR_SSA_NAME_AGE, then the fields
     are assumed to be null.  */
  unsigned age;
};

/* The information associated with names.  */
typedef struct ssa_name_info *ssa_name_info_p;
DEF_VEC_P (ssa_name_info_p);
DEF_VEC_ALLOC_P (ssa_name_info_p, heap);

static VEC(ssa_name_info_p, heap) *info_for_ssa_name;
static unsigned current_info_for_ssa_name_age;

/* The set of blocks affected by update_ssa.  */
static bitmap blocks_to_update;

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




/* Prototypes for debugging functions.  */
extern void dump_tree_ssa (FILE *);
extern void debug_tree_ssa (void);
extern void debug_def_blocks (void);
extern void dump_tree_ssa_stats (FILE *);
extern void debug_tree_ssa_stats (void);
extern void dump_update_ssa (FILE *);
extern void debug_update_ssa (void);
extern void dump_names_replaced_by (FILE *, tree);
extern void debug_names_replaced_by (tree);
extern void dump_def_blocks (FILE *);
extern void debug_def_blocks (void);
extern void dump_defs_stack (FILE *, int);
extern void debug_defs_stack (int);
extern void dump_currdefs (FILE *);
extern void debug_currdefs (void);

/* Return true if STMT needs to be rewritten.  When renaming a subset
   of the variables, not all statements will be processed.  This is
   decided in mark_def_sites.  */

static inline bool
rewrite_uses_p (gimple stmt)
{
  return gimple_visited_p (stmt);
}


/* Set the rewrite marker on STMT to the value given by REWRITE_P.  */

static inline void
set_rewrite_uses (gimple stmt, bool rewrite_p)
{
  gimple_set_visited (stmt, rewrite_p);
}


/* Return true if the DEFs created by statement STMT should be
   registered when marking new definition sites.  This is slightly
   different than rewrite_uses_p: it's used by update_ssa to
   distinguish statements that need to have both uses and defs
   processed from those that only need to have their defs processed.
   Statements that define new SSA names only need to have their defs
   registered, but they don't need to have their uses renamed.  */

static inline bool
register_defs_p (gimple stmt)
{
  return gimple_plf (stmt, GF_PLF_1) != 0;
}


/* If REGISTER_DEFS_P is true, mark STMT to have its DEFs registered.  */

static inline void
set_register_defs (gimple stmt, bool register_defs_p)
{
  gimple_set_plf (stmt, GF_PLF_1, register_defs_p);
}


/* Get the information associated with NAME.  */

static inline ssa_name_info_p
get_ssa_name_ann (tree name)
{
  unsigned ver = SSA_NAME_VERSION (name);
  unsigned len = VEC_length (ssa_name_info_p, info_for_ssa_name);
  struct ssa_name_info *info;

  if (ver >= len)
    {
      unsigned new_len = num_ssa_names;

      VEC_reserve (ssa_name_info_p, heap, info_for_ssa_name, new_len);
      while (len++ < new_len)
	{
	  struct ssa_name_info *info = XCNEW (struct ssa_name_info);
	  info->age = current_info_for_ssa_name_age;
	  VEC_quick_push (ssa_name_info_p, info_for_ssa_name, info);
	}
    }

  info = VEC_index (ssa_name_info_p, info_for_ssa_name, ver);
  if (info->age < current_info_for_ssa_name_age)
    {
      info->need_phi_state = NEED_PHI_STATE_UNKNOWN;
      info->current_def = NULL_TREE;
      info->age = current_info_for_ssa_name_age;
    }

  return info;
}


/* Clears info for SSA names.  */

static void
clear_ssa_name_info (void)
{
  current_info_for_ssa_name_age++;
}


/* Get phi_state field for VAR.  */

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

tree
get_current_def (tree var)
{
  if (TREE_CODE (var) == SSA_NAME)
    return get_ssa_name_ann (var)->current_def;
  else
    return var_ann (var)->current_def;
}


/* Sets current definition of VAR to DEF.  */

void
set_current_def (tree var, tree def)
{
  if (TREE_CODE (var) == SSA_NAME)
    get_ssa_name_ann (var)->current_def = def;
  else
    var_ann (var)->current_def = def;
}


/* Compute global livein information given the set of blocks where
   an object is locally live at the start of the block (LIVEIN)
   and the set of blocks where the object is defined (DEF_BLOCKS).

   Note: This routine augments the existing local livein information
   to include global livein (i.e., it modifies the underlying bitmap
   for LIVEIN).  */

void
compute_global_livein (bitmap livein ATTRIBUTE_UNUSED, bitmap def_blocks ATTRIBUTE_UNUSED)
{
  basic_block bb, *worklist, *tos;
  unsigned i;
  bitmap_iterator bi;

  tos = worklist
    = (basic_block *) xmalloc (sizeof (basic_block) * (last_basic_block + 1));

  EXECUTE_IF_SET_IN_BITMAP (livein, 0, i, bi)
    *tos++ = BASIC_BLOCK (i);

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


/* Cleans up the REWRITE_THIS_STMT and REGISTER_DEFS_IN_THIS_STMT flags for
   all statements in basic block BB.  */

static void
initialize_flags_in_bb (basic_block bb)
{
  gimple stmt;
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple phi = gsi_stmt (gsi);
      set_rewrite_uses (phi, false);
      set_register_defs (phi, false);
    }

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      stmt = gsi_stmt (gsi);

      /* We are going to use the operand cache API, such as
	 SET_USE, SET_DEF, and FOR_EACH_IMM_USE_FAST.  The operand
	 cache for each statement should be up-to-date.  */
      gcc_assert (!gimple_modified_p (stmt));
      set_rewrite_uses (stmt, false);
      set_register_defs (stmt, false);
    }
}

/* Mark block BB as interesting for update_ssa.  */

static void
mark_block_for_update (basic_block bb)
{
  gcc_assert (blocks_to_update != NULL);
  if (!bitmap_set_bit (blocks_to_update, bb->index))
    return;
  initialize_flags_in_bb (bb);
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
      db_p = XNEW (struct def_blocks_d);
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

bool
symbol_marked_for_renaming (tree sym)
{
  return bitmap_bit_p (SYMS_TO_RENAME (cfun), DECL_UID (sym));
}


/* Return true if NAME is in OLD_SSA_NAMES.  */

static inline bool
is_old_name (tree name)
{
  unsigned ver = SSA_NAME_VERSION (name);
  if (!new_ssa_names)
    return false;
  return ver < new_ssa_names->n_bits && TEST_BIT (old_ssa_names, ver);
}


/* Return true if NAME is in NEW_SSA_NAMES.  */

static inline bool
is_new_name (tree name)
{
  unsigned ver = SSA_NAME_VERSION (name);
  if (!new_ssa_names)
    return false;
  return ver < new_ssa_names->n_bits && TEST_BIT (new_ssa_names, ver);
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


/* Return the names replaced by NEW_TREE (i.e., REPL_TBL[NEW_TREE].SET).  */

static inline bitmap
names_replaced_by (tree new_tree)
{
  struct repl_map_d m;
  void **slot;

  m.name = new_tree;
  slot = htab_find_slot (repl_tbl, (void *) &m, NO_INSERT);

  /* If N was not registered in the replacement table, return NULL.  */
  if (slot == NULL || *slot == NULL)
    return NULL;

  return ((struct repl_map_d *) *slot)->set;
}


/* Add OLD to REPL_TBL[NEW_TREE].SET.  */

static inline void
add_to_repl_tbl (tree new_tree, tree old)
{
  struct repl_map_d m, *mp;
  void **slot;

  m.name = new_tree;
  slot = htab_find_slot (repl_tbl, (void *) &m, INSERT);
  if (*slot == NULL)
    {
      mp = XNEW (struct repl_map_d);
      mp->name = new_tree;
      mp->set = BITMAP_ALLOC (NULL);
      *slot = (void *) mp;
    }
  else
    mp = (struct repl_map_d *) *slot;

  bitmap_set_bit (mp->set, SSA_NAME_VERSION (old));
}


/* Add a new mapping NEW_TREE -> OLD REPL_TBL.  Every entry N_i in REPL_TBL
   represents the set of names O_1 ... O_j replaced by N_i.  This is
   used by update_ssa and its helpers to introduce new SSA names in an
   already formed SSA web.  */

static void
add_new_name_mapping (tree new_tree, tree old)
{
  timevar_push (TV_TREE_SSA_INCREMENTAL);

  /* OLD and NEW_TREE must be different SSA names for the same symbol.  */
  gcc_assert (new_tree != old && SSA_NAME_VAR (new_tree) == SSA_NAME_VAR (old));

  /* If this mapping is for virtual names, we will need to update
     virtual operands.  If this is a mapping for .MEM, then we gather
     the symbols associated with each name.  */
  if (!is_gimple_reg (new_tree))
    {
      tree sym;

      update_ssa_stats.num_virtual_mappings++;
      update_ssa_stats.num_virtual_symbols++;

      /* Keep counts of virtual mappings and symbols to use in the
	 virtual mapping heuristic.  If we have large numbers of
	 virtual mappings for a relatively low number of symbols, it
	 will make more sense to rename the symbols from scratch.
	 Otherwise, the insertion of PHI nodes for each of the old
	 names in these mappings will be very slow.  */
      sym = SSA_NAME_VAR (new_tree);
      bitmap_set_bit (update_ssa_stats.virtual_symbols, DECL_UID (sym));
    }

  /* We may need to grow NEW_SSA_NAMES and OLD_SSA_NAMES because our
     caller may have created new names since the set was created.  */
  if (new_ssa_names->n_bits <= num_ssa_names - 1)
    {
      unsigned int new_sz = num_ssa_names + NAME_SETS_GROWTH_FACTOR;
      new_ssa_names = sbitmap_resize (new_ssa_names, new_sz, 0);
      old_ssa_names = sbitmap_resize (old_ssa_names, new_sz, 0);
    }

  /* Update the REPL_TBL table.  */
  add_to_repl_tbl (new_tree, old);

  /* If OLD had already been registered as a new name, then all the
     names that OLD replaces should also be replaced by NEW_TREE.  */
  if (is_new_name (old))
    bitmap_ior_into (names_replaced_by (new_tree), names_replaced_by (old));

  /* Register NEW_TREE and OLD in NEW_SSA_NAMES and OLD_SSA_NAMES,
     respectively.  */
  SET_BIT (new_ssa_names, SSA_NAME_VERSION (new_tree));
  SET_BIT (old_ssa_names, SSA_NAME_VERSION (old));

  /* Update mapping counter to use in the virtual mapping heuristic.  */
  update_ssa_stats.num_total_mappings++;

  timevar_pop (TV_TREE_SSA_INCREMENTAL);
}


/* Call back for walk_dominator_tree used to collect definition sites
   for every variable in the function.  For every statement S in block
   BB:

   1- Variables defined by S in the DEFS of S are marked in the bitmap
      KILLS.

   2- If S uses a variable VAR and there is no preceding kill of VAR,
      then it is marked in the LIVEIN_BLOCKS bitmap associated with VAR.

   This information is used to determine which variables are live
   across block boundaries to reduce the number of PHI nodes
   we create.  */

static void
mark_def_sites (basic_block bb, gimple stmt, bitmap kills)
{
  tree def;
  use_operand_p use_p;
  ssa_op_iter iter;

  /* Since this is the first time that we rewrite the program into SSA
     form, force an operand scan on every statement.  */
  update_stmt (stmt);

  gcc_assert (blocks_to_update == NULL);
  set_register_defs (stmt, false);
  set_rewrite_uses (stmt, false);

  if (is_gimple_debug (stmt))
    return;

  /* If a variable is used before being set, then the variable is live
     across a block boundary, so mark it live-on-entry to BB.  */
  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_USE)
    {
      tree sym = USE_FROM_PTR (use_p);
      gcc_assert (DECL_P (sym));
      if (!bitmap_bit_p (kills, DECL_UID (sym)))
	set_livein_block (sym, bb);
      set_rewrite_uses (stmt, true);
    }

  /* Now process the defs.  Mark BB as the definition block and add
     each def to the set of killed symbols.  */
  FOR_EACH_SSA_TREE_OPERAND (def, stmt, iter, SSA_OP_DEF)
    {
      gcc_assert (DECL_P (def));
      set_def_block (def, bb, false);
      bitmap_set_bit (kills, DECL_UID (def));
      set_register_defs (stmt, true);
    }

  /* If we found the statement interesting then also mark the block BB
     as interesting.  */
  if (rewrite_uses_p (stmt) || register_defs_p (stmt))
    SET_BIT (interesting_blocks, bb->index);
}

/* Structure used by prune_unused_phi_nodes to record bounds of the intervals
   in the dfs numbering of the dominance tree.  */

struct dom_dfsnum
{
  /* Basic block whose index this entry corresponds to.  */
  unsigned bb_index;

  /* The dfs number of this node.  */
  unsigned dfs_num;
};

/* Compares two entries of type struct dom_dfsnum by dfs_num field.  Callback
   for qsort.  */

static int
cmp_dfsnum (const void *a, const void *b)
{
  const struct dom_dfsnum *const da = (const struct dom_dfsnum *) a;
  const struct dom_dfsnum *const db = (const struct dom_dfsnum *) b;

  return (int) da->dfs_num - (int) db->dfs_num;
}

/* Among the intervals starting at the N points specified in DEFS, find
   the one that contains S, and return its bb_index.  */

static unsigned
find_dfsnum_interval (struct dom_dfsnum *defs, unsigned n, unsigned s)
{
  unsigned f = 0, t = n, m;

  while (t > f + 1)
    {
      m = (f + t) / 2;
      if (defs[m].dfs_num <= s)
	f = m;
      else
	t = m;
    }

  return defs[f].bb_index;
}

/* Clean bits from PHIS for phi nodes whose value cannot be used in USES.
   KILLS is a bitmap of blocks where the value is defined before any use.  */

static void
prune_unused_phi_nodes (bitmap phis, bitmap kills, bitmap uses)
{
  VEC(int, heap) *worklist;
  bitmap_iterator bi;
  unsigned i, b, p, u, top;
  bitmap live_phis;
  basic_block def_bb, use_bb;
  edge e;
  edge_iterator ei;
  bitmap to_remove;
  struct dom_dfsnum *defs;
  unsigned n_defs, adef;

  if (bitmap_empty_p (uses))
    {
      bitmap_clear (phis);
      return;
    }

  /* The phi must dominate a use, or an argument of a live phi.  Also, we
     do not create any phi nodes in def blocks, unless they are also livein.  */
  to_remove = BITMAP_ALLOC (NULL);
  bitmap_and_compl (to_remove, kills, uses);
  bitmap_and_compl_into (phis, to_remove);
  if (bitmap_empty_p (phis))
    {
      BITMAP_FREE (to_remove);
      return;
    }

  /* We want to remove the unnecessary phi nodes, but we do not want to compute
     liveness information, as that may be linear in the size of CFG, and if
     there are lot of different variables to rewrite, this may lead to quadratic
     behavior.

     Instead, we basically emulate standard dce.  We put all uses to worklist,
     then for each of them find the nearest def that dominates them.  If this
     def is a phi node, we mark it live, and if it was not live before, we
     add the predecessors of its basic block to the worklist.

     To quickly locate the nearest def that dominates use, we use dfs numbering
     of the dominance tree (that is already available in order to speed up
     queries).  For each def, we have the interval given by the dfs number on
     entry to and on exit from the corresponding subtree in the dominance tree.
     The nearest dominator for a given use is the smallest of these intervals
     that contains entry and exit dfs numbers for the basic block with the use.
     If we store the bounds for all the uses to an array and sort it, we can
     locate the nearest dominating def in logarithmic time by binary search.*/
  bitmap_ior (to_remove, kills, phis);
  n_defs = bitmap_count_bits (to_remove);
  defs = XNEWVEC (struct dom_dfsnum, 2 * n_defs + 1);
  defs[0].bb_index = 1;
  defs[0].dfs_num = 0;
  adef = 1;
  EXECUTE_IF_SET_IN_BITMAP (to_remove, 0, i, bi)
    {
      def_bb = BASIC_BLOCK (i);
      defs[adef].bb_index = i;
      defs[adef].dfs_num = bb_dom_dfs_in (CDI_DOMINATORS, def_bb);
      defs[adef + 1].bb_index = i;
      defs[adef + 1].dfs_num = bb_dom_dfs_out (CDI_DOMINATORS, def_bb);
      adef += 2;
    }
  BITMAP_FREE (to_remove);
  gcc_assert (adef == 2 * n_defs + 1);
  qsort (defs, adef, sizeof (struct dom_dfsnum), cmp_dfsnum);
  gcc_assert (defs[0].bb_index == 1);

  /* Now each DEFS entry contains the number of the basic block to that the
     dfs number corresponds.  Change them to the number of basic block that
     corresponds to the interval following the dfs number.  Also, for the
     dfs_out numbers, increase the dfs number by one (so that it corresponds
     to the start of the following interval, not to the end of the current
     one).  We use WORKLIST as a stack.  */
  worklist = VEC_alloc (int, heap, n_defs + 1);
  VEC_quick_push (int, worklist, 1);
  top = 1;
  n_defs = 1;
  for (i = 1; i < adef; i++)
    {
      b = defs[i].bb_index;
      if (b == top)
	{
	  /* This is a closing element.  Interval corresponding to the top
	     of the stack after removing it follows.  */
	  VEC_pop (int, worklist);
	  top = VEC_index (int, worklist, VEC_length (int, worklist) - 1);
	  defs[n_defs].bb_index = top;
	  defs[n_defs].dfs_num = defs[i].dfs_num + 1;
	}
      else
	{
	  /* Opening element.  Nothing to do, just push it to the stack and move
	     it to the correct position.  */
	  defs[n_defs].bb_index = defs[i].bb_index;
	  defs[n_defs].dfs_num = defs[i].dfs_num;
	  VEC_quick_push (int, worklist, b);
	  top = b;
	}

      /* If this interval starts at the same point as the previous one, cancel
	 the previous one.  */
      if (defs[n_defs].dfs_num == defs[n_defs - 1].dfs_num)
	defs[n_defs - 1].bb_index = defs[n_defs].bb_index;
      else
	n_defs++;
    }
  VEC_pop (int, worklist);
  gcc_assert (VEC_empty (int, worklist));

  /* Now process the uses.  */
  live_phis = BITMAP_ALLOC (NULL);
  EXECUTE_IF_SET_IN_BITMAP (uses, 0, i, bi)
    {
      VEC_safe_push (int, heap, worklist, i);
    }

  while (!VEC_empty (int, worklist))
    {
      b = VEC_pop (int, worklist);
      if (b == ENTRY_BLOCK)
	continue;

      /* If there is a phi node in USE_BB, it is made live.  Otherwise,
	 find the def that dominates the immediate dominator of USE_BB
	 (the kill in USE_BB does not dominate the use).  */
      if (bitmap_bit_p (phis, b))
	p = b;
      else
	{
	  use_bb = get_immediate_dominator (CDI_DOMINATORS, BASIC_BLOCK (b));
	  p = find_dfsnum_interval (defs, n_defs,
				    bb_dom_dfs_in (CDI_DOMINATORS, use_bb));
	  if (!bitmap_bit_p (phis, p))
	    continue;
	}

      /* If the phi node is already live, there is nothing to do.  */
      if (!bitmap_set_bit (live_phis, p))
	continue;

      /* Add the new uses to the worklist.  */
      def_bb = BASIC_BLOCK (p);
      FOR_EACH_EDGE (e, ei, def_bb->preds)
	{
	  u = e->src->index;
	  if (bitmap_bit_p (uses, u))
	    continue;

	  /* In case there is a kill directly in the use block, do not record
	     the use (this is also necessary for correctness, as we assume that
	     uses dominated by a def directly in their block have been filtered
	     out before).  */
	  if (bitmap_bit_p (kills, u))
	    continue;

	  bitmap_set_bit (uses, u);
	  VEC_safe_push (int, heap, worklist, u);
	}
    }

  VEC_free (int, heap, worklist);
  bitmap_copy (phis, live_phis);
  BITMAP_FREE (live_phis);
  free (defs);
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
  tree ddef = gimple_default_def (cfun, sym);

  if (ddef == NULL_TREE)
    {
      ddef = make_ssa_name (sym, gimple_build_nop ());
      set_default_def (sym, ddef);
    }

  return ddef;
}


/* Marks phi node PHI in basic block BB for rewrite.  */

static void
mark_phi_for_rewrite (basic_block bb, gimple phi)
{
  gimple_vec phis;
  unsigned i, idx = bb->index;

  if (rewrite_uses_p (phi))
    return;

  set_rewrite_uses (phi, true);

  if (!blocks_with_phis_to_rewrite)
    return;

  bitmap_set_bit (blocks_with_phis_to_rewrite, idx);
  VEC_reserve (gimple_vec, heap, phis_to_rewrite, last_basic_block + 1);
  for (i = VEC_length (gimple_vec, phis_to_rewrite); i <= idx; i++)
    VEC_quick_push (gimple_vec, phis_to_rewrite, NULL);

  phis = VEC_index (gimple_vec, phis_to_rewrite, idx);
  if (!phis)
    phis = VEC_alloc (gimple, heap, 10);

  VEC_safe_push (gimple, heap, phis, phi);
  VEC_replace (gimple_vec, phis_to_rewrite, idx, phis);
}

/* Insert PHI nodes for variable VAR using the iterated dominance
   frontier given in PHI_INSERTION_POINTS.  If UPDATE_P is true, this
   function assumes that the caller is incrementally updating the
   existing SSA form, in which case VAR may be an SSA name instead of
   a symbol.

   PHI_INSERTION_POINTS is updated to reflect nodes that already had a
   PHI node for VAR.  On exit, only the nodes that received a PHI node
   for VAR will be present in PHI_INSERTION_POINTS.  */

static void
insert_phi_nodes_for (tree var, bitmap phi_insertion_points, bool update_p)
{
  unsigned bb_index;
  edge e;
  gimple phi;
  basic_block bb;
  bitmap_iterator bi;
  struct def_blocks_d *def_map;

  def_map = find_def_blocks_for (var);
  gcc_assert (def_map);

  /* Remove the blocks where we already have PHI nodes for VAR.  */
  bitmap_and_compl_into (phi_insertion_points, def_map->phi_blocks);

  /* Remove obviously useless phi nodes.  */
  prune_unused_phi_nodes (phi_insertion_points, def_map->def_blocks,
			  def_map->livein_blocks);

  /* And insert the PHI nodes.  */
  EXECUTE_IF_SET_IN_BITMAP (phi_insertion_points, 0, bb_index, bi)
    {
      bb = BASIC_BLOCK (bb_index);
      if (update_p)
	mark_block_for_update (bb);

      phi = NULL;

      if (TREE_CODE (var) == SSA_NAME)
	{
	  /* If we are rewriting SSA names, create the LHS of the PHI
	     node by duplicating VAR.  This is useful in the case of
	     pointers, to also duplicate pointer attributes (alias
	     information, in particular).  */
	  edge_iterator ei;
	  tree new_lhs;

	  gcc_assert (update_p);
	  phi = create_phi_node (var, bb);

	  new_lhs = duplicate_ssa_name (var, phi);
	  gimple_phi_set_result (phi, new_lhs);
	  add_new_name_mapping (new_lhs, var);

	  /* Add VAR to every argument slot of PHI.  We need VAR in
	     every argument so that rewrite_update_phi_arguments knows
	     which name is this PHI node replacing.  If VAR is a
	     symbol marked for renaming, this is not necessary, the
	     renamer will use the symbol on the LHS to get its
	     reaching definition.  */
	  FOR_EACH_EDGE (e, ei, bb->preds)
	    add_phi_arg (phi, var, e, UNKNOWN_LOCATION);
	}
      else
	{
	  tree tracked_var;

	  gcc_assert (DECL_P (var));
	  phi = create_phi_node (var, bb);

	  tracked_var = target_for_debug_bind (var);
	  if (tracked_var)
	    {
	      gimple note = gimple_build_debug_bind (tracked_var,
						     PHI_RESULT (phi),
						     phi);
	      gimple_stmt_iterator si = gsi_after_labels (bb);
	      gsi_insert_before (&si, note, GSI_SAME_STMT);
	    }
	}

      /* Mark this PHI node as interesting for update_ssa.  */
      set_register_defs (phi, true);
      mark_phi_for_rewrite (bb, phi);
    }
}


/* Insert PHI nodes at the dominance frontier of blocks with variable
   definitions.  DFS contains the dominance frontier information for
   the flowgraph.  */

static void
insert_phi_nodes (bitmap_head *dfs)
{
  referenced_var_iterator rvi;
  bitmap_iterator bi;
  tree var;
  bitmap vars;
  unsigned uid;

  timevar_push (TV_TREE_INSERT_PHI_NODES);

  /* Do two stages to avoid code generation differences for UID
     differences but no UID ordering differences.  */

  vars = BITMAP_ALLOC (NULL);
  FOR_EACH_REFERENCED_VAR (cfun, var, rvi)
    {
      struct def_blocks_d *def_map;

      def_map = find_def_blocks_for (var);
      if (def_map == NULL)
	continue;

      if (get_phi_state (var) != NEED_PHI_STATE_NO)
	bitmap_set_bit (vars, DECL_UID (var));
    }

  EXECUTE_IF_SET_IN_BITMAP (vars, 0, uid, bi)
    {
      tree var = referenced_var (uid);
      struct def_blocks_d *def_map;
      bitmap idf;

      def_map = find_def_blocks_for (var);
      idf = compute_idf (def_map->def_blocks, dfs);
      insert_phi_nodes_for (var, idf, false);
      BITMAP_FREE (idf);
    }

  BITMAP_FREE (vars);

  timevar_pop (TV_TREE_INSERT_PHI_NODES);
}


/* Push SYM's current reaching definition into BLOCK_DEFS_STACK and
   register DEF (an SSA_NAME) to be a new definition for SYM.  */

static void
register_new_def (tree def, tree sym)
{
  tree currdef;

  /* If this variable is set in a single basic block and all uses are
     dominated by the set(s) in that single basic block, then there is
     no reason to record anything for this variable in the block local
     definition stacks.  Doing so just wastes time and memory.

     This is the same test to prune the set of variables which may
     need PHI nodes.  So we just use that information since it's already
     computed and available for us to use.  */
  if (get_phi_state (sym) == NEED_PHI_STATE_NO)
    {
      set_current_def (sym, def);
      return;
    }

  currdef = get_current_def (sym);

  /* If SYM is not a GIMPLE register, then CURRDEF may be a name whose
     SSA_NAME_VAR is not necessarily SYM.  In this case, also push SYM
     in the stack so that we know which symbol is being defined by
     this SSA name when we unwind the stack.  */
  if (currdef && !is_gimple_reg (sym))
    VEC_safe_push (tree, heap, block_defs_stack, sym);

  /* Push the current reaching definition into BLOCK_DEFS_STACK.  This
     stack is later used by the dominator tree callbacks to restore
     the reaching definitions for all the variables defined in the
     block after a recursive visit to all its immediately dominated
     blocks.  If there is no current reaching definition, then just
     record the underlying _DECL node.  */
  VEC_safe_push (tree, heap, block_defs_stack, currdef ? currdef : sym);

  /* Set the current reaching definition for SYM to be DEF.  */
  set_current_def (sym, def);
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

/* Return the current definition for variable VAR.  If none is found,
   create a new SSA name to act as the zeroth definition for VAR.  */

static tree
get_reaching_def (tree var)
{
  tree currdef;

  /* Lookup the current reaching definition for VAR.  */
  currdef = get_current_def (var);

  /* If there is no reaching definition for VAR, create and register a
     default definition for it (if needed).  */
  if (currdef == NULL_TREE)
    {
      tree sym = DECL_P (var) ? var : SSA_NAME_VAR (var);
      currdef = get_default_def_for (sym);
      set_current_def (var, currdef);
    }

  /* Return the current reaching definition for VAR, or the default
     definition, if we had to create one.  */
  return currdef;
}


/* SSA Rewriting Step 2.  Rewrite every variable used in each statement in
   the block with its immediate reaching definitions.  Update the current
   definition of a variable when a new real or virtual definition is found.  */

static void
rewrite_stmt (gimple_stmt_iterator si)
{
  use_operand_p use_p;
  def_operand_p def_p;
  ssa_op_iter iter;
  gimple stmt = gsi_stmt (si);

  /* If mark_def_sites decided that we don't need to rewrite this
     statement, ignore it.  */
  gcc_assert (blocks_to_update == NULL);
  if (!rewrite_uses_p (stmt) && !register_defs_p (stmt))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Renaming statement ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
      fprintf (dump_file, "\n");
    }

  /* Step 1.  Rewrite USES in the statement.  */
  if (rewrite_uses_p (stmt))
    FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_USE)
      {
	tree var = USE_FROM_PTR (use_p);
	gcc_assert (DECL_P (var));
	SET_USE (use_p, get_reaching_def (var));
      }

  /* Step 2.  Register the statement's DEF operands.  */
  if (register_defs_p (stmt))
    FOR_EACH_SSA_DEF_OPERAND (def_p, stmt, iter, SSA_OP_DEF)
      {
	tree var = DEF_FROM_PTR (def_p);
	tree name = make_ssa_name (var, stmt);
	tree tracked_var;
	gcc_assert (DECL_P (var));
	SET_DEF (def_p, name);
	register_new_def (DEF_FROM_PTR (def_p), var);

	tracked_var = target_for_debug_bind (var);
	if (tracked_var)
	  {
	    gimple note = gimple_build_debug_bind (tracked_var, name, stmt);
	    gsi_insert_after (&si, note, GSI_SAME_STMT);
	  }
      }
}


/* SSA Rewriting Step 3.  Visit all the successor blocks of BB looking for
   PHI nodes.  For every PHI node found, add a new argument containing the
   current reaching definition for the variable and the edge through which
   that definition is reaching the PHI node.  */

static void
rewrite_add_phi_arguments (basic_block bb)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      gimple phi;
      gimple_stmt_iterator gsi;

      for (gsi = gsi_start_phis (e->dest); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  tree currdef;
	  gimple stmt;

	  phi = gsi_stmt (gsi);
	  currdef = get_reaching_def (SSA_NAME_VAR (gimple_phi_result (phi)));
	  stmt = SSA_NAME_DEF_STMT (currdef);
	  add_phi_arg (phi, currdef, e, gimple_location (stmt));
	}
    }
}

/* SSA Rewriting Step 1.  Initialization, create a block local stack
   of reaching definitions for new SSA names produced in this block
   (BLOCK_DEFS).  Register new definitions for every PHI node in the
   block.  */

static void
rewrite_enter_block (struct dom_walk_data *walk_data ATTRIBUTE_UNUSED,
		     basic_block bb)
{
  gimple phi;
  gimple_stmt_iterator gsi;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n\nRenaming block #%d\n\n", bb->index);

  /* Mark the unwind point for this block.  */
  VEC_safe_push (tree, heap, block_defs_stack, NULL_TREE);

  /* Step 1.  Register new definitions for every PHI node in the block.
     Conceptually, all the PHI nodes are executed in parallel and each PHI
     node introduces a new version for the associated variable.  */
  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      tree result;

      phi = gsi_stmt (gsi);
      result = gimple_phi_result (phi);
      gcc_assert (is_gimple_reg (result));
      register_new_def (result, SSA_NAME_VAR (result));
    }

  /* Step 2.  Rewrite every variable used in each statement in the block
     with its immediate reaching definitions.  Update the current definition
     of a variable when a new real or virtual definition is found.  */
  if (TEST_BIT (interesting_blocks, bb->index))
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      rewrite_stmt (gsi);

  /* Step 3.  Visit all the successor blocks of BB looking for PHI nodes.
     For every PHI node found, add a new argument containing the current
     reaching definition for the variable and the edge through which that
     definition is reaching the PHI node.  */
  rewrite_add_phi_arguments (bb);
}



/* Called after visiting all the statements in basic block BB and all
   of its dominator children.  Restore CURRDEFS to its original value.  */

static void
rewrite_leave_block (struct dom_walk_data *walk_data ATTRIBUTE_UNUSED,
		     basic_block bb ATTRIBUTE_UNUSED)
{
  /* Restore CURRDEFS to its original state.  */
  while (VEC_length (tree, block_defs_stack) > 0)
    {
      tree tmp = VEC_pop (tree, block_defs_stack);
      tree saved_def, var;

      if (tmp == NULL_TREE)
	break;

      if (TREE_CODE (tmp) == SSA_NAME)
	{
	  /* If we recorded an SSA_NAME, then make the SSA_NAME the
	     current definition of its underlying variable.  Note that
	     if the SSA_NAME is not for a GIMPLE register, the symbol
	     being defined is stored in the next slot in the stack.
	     This mechanism is needed because an SSA name for a
	     non-register symbol may be the definition for more than
	     one symbol (e.g., SFTs, aliased variables, etc).  */
	  saved_def = tmp;
	  var = SSA_NAME_VAR (saved_def);
	  if (!is_gimple_reg (var))
	    var = VEC_pop (tree, block_defs_stack);
	}
      else
	{
	  /* If we recorded anything else, it must have been a _DECL
	     node and its current reaching definition must have been
	     NULL.  */
	  saved_def = NULL;
	  var = tmp;
	}

      set_current_def (var, saved_def);
    }
}


/* Dump bitmap SET (assumed to contain VAR_DECLs) to FILE.  */

void
dump_decl_set (FILE *file, bitmap set)
{
  if (set)
    {
      bitmap_iterator bi;
      unsigned i;

      fprintf (file, "{ ");

      EXECUTE_IF_SET_IN_BITMAP (set, 0, i, bi)
	{
	  tree var = referenced_var_lookup (cfun, i);
	  if (var)
	    print_generic_expr (file, var, 0);
	  else
	    fprintf (file, "D.%u", i);
	  fprintf (file, " ");
	}

      fprintf (file, "}");
    }
  else
    fprintf (file, "NIL");
}


/* Dump bitmap SET (assumed to contain VAR_DECLs) to FILE.  */

DEBUG_FUNCTION void
debug_decl_set (bitmap set)
{
  dump_decl_set (stderr, set);
  fprintf (stderr, "\n");
}


/* Dump the renaming stack (block_defs_stack) to FILE.  Traverse the
   stack up to a maximum of N levels.  If N is -1, the whole stack is
   dumped.  New levels are created when the dominator tree traversal
   used for renaming enters a new sub-tree.  */

void
dump_defs_stack (FILE *file, int n)
{
  int i, j;

  fprintf (file, "\n\nRenaming stack");
  if (n > 0)
    fprintf (file, " (up to %d levels)", n);
  fprintf (file, "\n\n");

  i = 1;
  fprintf (file, "Level %d (current level)\n", i);
  for (j = (int) VEC_length (tree, block_defs_stack) - 1; j >= 0; j--)
    {
      tree name, var;

      name = VEC_index (tree, block_defs_stack, j);
      if (name == NULL_TREE)
	{
	  i++;
	  if (n > 0 && i > n)
	    break;
	  fprintf (file, "\nLevel %d\n", i);
	  continue;
	}

      if (DECL_P (name))
	{
	  var = name;
	  name = NULL_TREE;
	}
      else
	{
	  var = SSA_NAME_VAR (name);
	  if (!is_gimple_reg (var))
	    {
	      j--;
	      var = VEC_index (tree, block_defs_stack, j);
	    }
	}

      fprintf (file, "    Previous CURRDEF (");
      print_generic_expr (file, var, 0);
      fprintf (file, ") = ");
      if (name)
	print_generic_expr (file, name, 0);
      else
	fprintf (file, "<NIL>");
      fprintf (file, "\n");
    }
}


/* Dump the renaming stack (block_defs_stack) to stderr.  Traverse the
   stack up to a maximum of N levels.  If N is -1, the whole stack is
   dumped.  New levels are created when the dominator tree traversal
   used for renaming enters a new sub-tree.  */

DEBUG_FUNCTION void
debug_defs_stack (int n)
{
  dump_defs_stack (stderr, n);
}


/* Dump the current reaching definition of every symbol to FILE.  */

void
dump_currdefs (FILE *file)
{
  referenced_var_iterator i;
  tree var;

  fprintf (file, "\n\nCurrent reaching definitions\n\n");
  FOR_EACH_REFERENCED_VAR (cfun, var, i)
    if (SYMS_TO_RENAME (cfun) == NULL
	|| bitmap_bit_p (SYMS_TO_RENAME (cfun), DECL_UID (var)))
      {
	fprintf (file, "CURRDEF (");
	print_generic_expr (file, var, 0);
	fprintf (file, ") = ");
	if (get_current_def (var))
	  print_generic_expr (file, get_current_def (var), 0);
	else
	  fprintf (file, "<NIL>");
	fprintf (file, "\n");
      }
}


/* Dump the current reaching definition of every symbol to stderr.  */

DEBUG_FUNCTION void
debug_currdefs (void)
{
  dump_currdefs (stderr);
}


/* Dump SSA information to FILE.  */

void
dump_tree_ssa (FILE *file)
{
  const char *funcname
    = lang_hooks.decl_printable_name (current_function_decl, 2);

  fprintf (file, "SSA renaming information for %s\n\n", funcname);

  dump_def_blocks (file);
  dump_defs_stack (file, -1);
  dump_currdefs (file);
  dump_tree_ssa_stats (file);
}


/* Dump SSA information to stderr.  */

DEBUG_FUNCTION void
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
  if (def_blocks || repl_tbl)
    fprintf (file, "\nHash table statistics:\n");

  if (def_blocks)
    {
      fprintf (file, "    def_blocks:   ");
      htab_statistics (file, def_blocks);
    }

  if (repl_tbl)
    {
      fprintf (file, "    repl_tbl:     ");
      htab_statistics (file, repl_tbl);
    }

  if (def_blocks || repl_tbl)
    fprintf (file, "\n");
}


/* Dump SSA statistics on stderr.  */

DEBUG_FUNCTION void
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
  struct def_blocks_d *entry = (struct def_blocks_d *) p;
  BITMAP_FREE (entry->def_blocks);
  BITMAP_FREE (entry->phi_blocks);
  BITMAP_FREE (entry->livein_blocks);
  free (entry);
}


/* Callback for htab_traverse to dump the DEF_BLOCKS hash table.  */

static int
debug_def_blocks_r (void **slot, void *data)
{
  FILE *file = (FILE *) data;
  struct def_blocks_d *db_p = (struct def_blocks_d *) *slot;

  fprintf (file, "VAR: ");
  print_generic_expr (file, db_p->var, dump_flags);
  bitmap_print (file, db_p->def_blocks, ", DEF_BLOCKS: { ", "}");
  bitmap_print (file, db_p->livein_blocks, ", LIVEIN_BLOCKS: { ", "}");
  bitmap_print (file, db_p->phi_blocks, ", PHI_BLOCKS: { ", "}\n");

  return 1;
}


/* Dump the DEF_BLOCKS hash table on FILE.  */

void
dump_def_blocks (FILE *file)
{
  fprintf (file, "\n\nDefinition and live-in blocks:\n\n");
  if (def_blocks)
    htab_traverse (def_blocks, debug_def_blocks_r, file);
}


/* Dump the DEF_BLOCKS hash table on stderr.  */

DEBUG_FUNCTION void
debug_def_blocks (void)
{
  dump_def_blocks (stderr);
}


/* Register NEW_NAME to be the new reaching definition for OLD_NAME.  */

static inline void
register_new_update_single (tree new_name, tree old_name)
{
  tree currdef = get_current_def (old_name);

  /* Push the current reaching definition into BLOCK_DEFS_STACK.
     This stack is later used by the dominator tree callbacks to
     restore the reaching definitions for all the variables
     defined in the block after a recursive visit to all its
     immediately dominated blocks.  */
  VEC_reserve (tree, heap, block_defs_stack, 2);
  VEC_quick_push (tree, block_defs_stack, currdef);
  VEC_quick_push (tree, block_defs_stack, old_name);

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



/* If the operand pointed to by USE_P is a name in OLD_SSA_NAMES or
   it is a symbol marked for renaming, replace it with USE_P's current
   reaching definition.  */

static inline void
maybe_replace_use (use_operand_p use_p)
{
  tree rdef = NULL_TREE;
  tree use = USE_FROM_PTR (use_p);
  tree sym = DECL_P (use) ? use : SSA_NAME_VAR (use);

  if (symbol_marked_for_renaming (sym))
    rdef = get_reaching_def (sym);
  else if (is_old_name (use))
    rdef = get_reaching_def (use);

  if (rdef && rdef != use)
    SET_USE (use_p, rdef);
}


/* Same as maybe_replace_use, but without introducing default stmts,
   returning false to indicate a need to do so.  */

static inline bool
maybe_replace_use_in_debug_stmt (use_operand_p use_p)
{
  tree rdef = NULL_TREE;
  tree use = USE_FROM_PTR (use_p);
  tree sym = DECL_P (use) ? use : SSA_NAME_VAR (use);

  if (symbol_marked_for_renaming (sym))
    rdef = get_current_def (sym);
  else if (is_old_name (use))
    {
      rdef = get_current_def (use);
      /* We can't assume that, if there's no current definition, the
	 default one should be used.  It could be the case that we've
	 rearranged blocks so that the earlier definition no longer
	 dominates the use.  */
      if (!rdef && SSA_NAME_IS_DEFAULT_DEF (use))
	rdef = use;
    }
  else
    rdef = use;

  if (rdef && rdef != use)
    SET_USE (use_p, rdef);

  return rdef != NULL_TREE;
}


/* If the operand pointed to by DEF_P is an SSA name in NEW_SSA_NAMES
   or OLD_SSA_NAMES, or if it is a symbol marked for renaming,
   register it as the current definition for the names replaced by
   DEF_P.  */

static inline void
maybe_register_def (def_operand_p def_p, gimple stmt,
		    gimple_stmt_iterator gsi)
{
  tree def = DEF_FROM_PTR (def_p);
  tree sym = DECL_P (def) ? def : SSA_NAME_VAR (def);

  /* If DEF is a naked symbol that needs renaming, create a new
     name for it.  */
  if (symbol_marked_for_renaming (sym))
    {
      if (DECL_P (def))
	{
	  tree tracked_var;

	  def = make_ssa_name (def, stmt);
	  SET_DEF (def_p, def);

	  tracked_var = target_for_debug_bind (sym);
	  if (tracked_var)
	    {
	      gimple note = gimple_build_debug_bind (tracked_var, def, stmt);
	      /* If stmt ends the bb, insert the debug stmt on the single
		 non-EH edge from the stmt.  */
	      if (gsi_one_before_end_p (gsi) && stmt_ends_bb_p (stmt))
		{
		  basic_block bb = gsi_bb (gsi);
		  edge_iterator ei;
		  edge e, ef = NULL;
		  FOR_EACH_EDGE (e, ei, bb->succs)
		    if (!(e->flags & EDGE_EH))
		      {
			gcc_assert (!ef);
			ef = e;
		      }
		  /* If there are other predecessors to ef->dest, then
		     there must be PHI nodes for the modified
		     variable, and therefore there will be debug bind
		     stmts after the PHI nodes.  The debug bind notes
		     we'd insert would force the creation of a new
		     block (diverging codegen) and be redundant with
		     the post-PHI bind stmts, so don't add them.

		     As for the exit edge, there wouldn't be redundant
		     bind stmts, but there wouldn't be a PC to bind
		     them to either, so avoid diverging the CFG.  */
		  if (ef && single_pred_p (ef->dest)
		      && ef->dest != EXIT_BLOCK_PTR)
		    {
		      /* If there were PHI nodes in the node, we'd
			 have to make sure the value we're binding
			 doesn't need rewriting.  But there shouldn't
			 be PHI nodes in a single-predecessor block,
			 so we just add the note.  */
		      gsi_insert_on_edge_immediate (ef, note);
		    }
		}
	      else
		gsi_insert_after (&gsi, note, GSI_SAME_STMT);
	    }
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


/* Update every variable used in the statement pointed-to by SI.  The
   statement is assumed to be in SSA form already.  Names in
   OLD_SSA_NAMES used by SI will be updated to their current reaching
   definition.  Names in OLD_SSA_NAMES or NEW_SSA_NAMES defined by SI
   will be registered as a new definition for their corresponding name
   in OLD_SSA_NAMES.  */

static void
rewrite_update_stmt (gimple stmt, gimple_stmt_iterator gsi)
{
  use_operand_p use_p;
  def_operand_p def_p;
  ssa_op_iter iter;

  /* Only update marked statements.  */
  if (!rewrite_uses_p (stmt) && !register_defs_p (stmt))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Updating SSA information for statement ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
      fprintf (dump_file, "\n");
    }

  /* Rewrite USES included in OLD_SSA_NAMES and USES whose underlying
     symbol is marked for renaming.  */
  if (rewrite_uses_p (stmt))
    {
      if (is_gimple_debug (stmt))
	{
	  bool failed = false;

	  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_USE)
	    if (!maybe_replace_use_in_debug_stmt (use_p))
	      {
		failed = true;
		break;
	      }

	  if (failed)
	    {
	      /* DOM sometimes threads jumps in such a way that a
		 debug stmt ends up referencing a SSA variable that no
		 longer dominates the debug stmt, but such that all
		 incoming definitions refer to the same definition in
		 an earlier dominator.  We could try to recover that
		 definition somehow, but this will have to do for now.

		 Introducing a default definition, which is what
		 maybe_replace_use() would do in such cases, may
		 modify code generation, for the otherwise-unused
		 default definition would never go away, modifying SSA
		 version numbers all over.  */
	      gimple_debug_bind_reset_value (stmt);
	      update_stmt (stmt);
	    }
	}
      else
	{
	  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_ALL_USES)
	    maybe_replace_use (use_p);
	}
    }

  /* Register definitions of names in NEW_SSA_NAMES and OLD_SSA_NAMES.
     Also register definitions for names whose underlying symbol is
     marked for renaming.  */
  if (register_defs_p (stmt))
    FOR_EACH_SSA_DEF_OPERAND (def_p, stmt, iter, SSA_OP_ALL_DEFS)
      maybe_register_def (def_p, stmt, gsi);
}


/* Visit all the successor blocks of BB looking for PHI nodes.  For
   every PHI node found, check if any of its arguments is in
   OLD_SSA_NAMES.  If so, and if the argument has a current reaching
   definition, replace it.  */

static void
rewrite_update_phi_arguments (basic_block bb)
{
  edge e;
  edge_iterator ei;
  unsigned i;

  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      gimple phi;
      gimple_vec phis;

      if (!bitmap_bit_p (blocks_with_phis_to_rewrite, e->dest->index))
	continue;

      phis = VEC_index (gimple_vec, phis_to_rewrite, e->dest->index);
      FOR_EACH_VEC_ELT (gimple, phis, i, phi)
	{
	  tree arg, lhs_sym, reaching_def = NULL;
	  use_operand_p arg_p;

  	  gcc_assert (rewrite_uses_p (phi));

	  arg_p = PHI_ARG_DEF_PTR_FROM_EDGE (phi, e);
	  arg = USE_FROM_PTR (arg_p);

	  if (arg && !DECL_P (arg) && TREE_CODE (arg) != SSA_NAME)
	    continue;

	  lhs_sym = SSA_NAME_VAR (gimple_phi_result (phi));

	  if (arg == NULL_TREE)
	    {
	      /* When updating a PHI node for a recently introduced
		 symbol we may find NULL arguments.  That's why we
		 take the symbol from the LHS of the PHI node.  */
	      reaching_def = get_reaching_def (lhs_sym);

	    }
	  else
	    {
	      tree sym = DECL_P (arg) ? arg : SSA_NAME_VAR (arg);

	      if (symbol_marked_for_renaming (sym))
		reaching_def = get_reaching_def (sym);
	      else if (is_old_name (arg))
		reaching_def = get_reaching_def (arg);
	    }

          /* Update the argument if there is a reaching def.  */
	  if (reaching_def)
	    {
	      gimple stmt;
	      source_location locus;
	      int arg_i = PHI_ARG_INDEX_FROM_USE (arg_p);

	      SET_USE (arg_p, reaching_def);
	      stmt = SSA_NAME_DEF_STMT (reaching_def);

	      /* Single element PHI nodes  behave like copies, so get the
		 location from the phi argument.  */
	      if (gimple_code (stmt) == GIMPLE_PHI &&
		  gimple_phi_num_args (stmt) == 1)
		locus = gimple_phi_arg_location (stmt, 0);
	      else
		locus = gimple_location (stmt);

	      gimple_phi_arg_set_location (phi, arg_i, locus);
	    }


	  if (e->flags & EDGE_ABNORMAL)
	    SSA_NAME_OCCURS_IN_ABNORMAL_PHI (USE_FROM_PTR (arg_p)) = 1;
	}
    }
}


/* Initialization of block data structures for the incremental SSA
   update pass.  Create a block local stack of reaching definitions
   for new SSA names produced in this block (BLOCK_DEFS).  Register
   new definitions for every PHI node in the block.  */

static void
rewrite_update_enter_block (struct dom_walk_data *walk_data ATTRIBUTE_UNUSED,
		            basic_block bb)
{
  bool is_abnormal_phi;
  gimple_stmt_iterator gsi;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n\nRegistering new PHI nodes in block #%d\n\n",
	     bb->index);

  /* Mark the unwind point for this block.  */
  VEC_safe_push (tree, heap, block_defs_stack, NULL_TREE);

  if (!bitmap_bit_p (blocks_to_update, bb->index))
    return;

  /* Mark the LHS if any of the arguments flows through an abnormal
     edge.  */
  is_abnormal_phi = bb_has_abnormal_pred (bb);

  /* If any of the PHI nodes is a replacement for a name in
     OLD_SSA_NAMES or it's one of the names in NEW_SSA_NAMES, then
     register it as a new definition for its corresponding name.  Also
     register definitions for names whose underlying symbols are
     marked for renaming.  */
  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      tree lhs, lhs_sym;
      gimple phi = gsi_stmt (gsi);

      if (!register_defs_p (phi))
	continue;

      lhs = gimple_phi_result (phi);
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

  /* Step 2.  Rewrite every variable used in each statement in the block.  */
  if (TEST_BIT (interesting_blocks, bb->index))
    {
      gcc_assert (bitmap_bit_p (blocks_to_update, bb->index));
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
        rewrite_update_stmt (gsi_stmt (gsi), gsi);
    }

  /* Step 3.  Update PHI nodes.  */
  rewrite_update_phi_arguments (bb);
}

/* Called after visiting block BB.  Unwind BLOCK_DEFS_STACK to restore
   the current reaching definition of every name re-written in BB to
   the original reaching definition before visiting BB.  This
   unwinding must be done in the opposite order to what is done in
   register_new_update_set.  */

static void
rewrite_update_leave_block (struct dom_walk_data *walk_data ATTRIBUTE_UNUSED,
			    basic_block bb ATTRIBUTE_UNUSED)
{
  while (VEC_length (tree, block_defs_stack) > 0)
    {
      tree var = VEC_pop (tree, block_defs_stack);
      tree saved_def;

      /* NULL indicates the unwind stop point for this block (see
	 rewrite_update_enter_block).  */
      if (var == NULL)
	return;

      saved_def = VEC_pop (tree, block_defs_stack);
      set_current_def (var, saved_def);
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
rewrite_blocks (basic_block entry, enum rewrite_mode what)
{
  struct dom_walk_data walk_data;

  /* Rewrite all the basic blocks in the program.  */
  timevar_push (TV_TREE_SSA_REWRITE_BLOCKS);

  /* Setup callbacks for the generic dominator tree walker.  */
  memset (&walk_data, 0, sizeof (walk_data));

  walk_data.dom_direction = CDI_DOMINATORS;

  if (what == REWRITE_ALL)
    {
      walk_data.before_dom_children = rewrite_enter_block;
      walk_data.after_dom_children = rewrite_leave_block;
    }
  else if (what == REWRITE_UPDATE)
    {
      walk_data.before_dom_children = rewrite_update_enter_block;
      walk_data.after_dom_children = rewrite_update_leave_block;
    }
  else
    gcc_unreachable ();

  block_defs_stack = VEC_alloc (tree, heap, 10);

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

  VEC_free (tree, heap, block_defs_stack);

  timevar_pop (TV_TREE_SSA_REWRITE_BLOCKS);
}


/* Block processing routine for mark_def_sites.  Clear the KILLS bitmap
   at the start of each block, and call mark_def_sites for each statement.  */

static void
mark_def_sites_block (struct dom_walk_data *walk_data, basic_block bb)
{
  struct mark_def_sites_global_data *gd;
  bitmap kills;
  gimple_stmt_iterator gsi;

  gd = (struct mark_def_sites_global_data *) walk_data->global_data;
  kills = gd->kills;

  bitmap_clear (kills);
  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    mark_def_sites (bb, gsi_stmt (gsi), kills);
}


/* Mark the definition site blocks for each variable, so that we know
   where the variable is actually live.

   The INTERESTING_BLOCKS global will be filled in with all the blocks
   that should be processed by the renamer.  It is assumed that the
   caller has already initialized and zeroed it.  */

static void
mark_def_site_blocks (void)
{
  struct dom_walk_data walk_data;
  struct mark_def_sites_global_data mark_def_sites_global_data;

  /* Setup callbacks for the generic dominator tree walker to find and
     mark definition sites.  */
  walk_data.dom_direction = CDI_DOMINATORS;
  walk_data.initialize_block_local_data = NULL;
  walk_data.before_dom_children = mark_def_sites_block;
  walk_data.after_dom_children = NULL;

  /* Notice that this bitmap is indexed using variable UIDs, so it must be
     large enough to accommodate all the variables referenced in the
     function, not just the ones we are renaming.  */
  mark_def_sites_global_data.kills = BITMAP_ALLOC (NULL);
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


/* Initialize internal data needed during renaming.  */

static void
init_ssa_renamer (void)
{
  tree var;
  referenced_var_iterator rvi;

  cfun->gimple_df->in_ssa_p = false;

  /* Allocate memory for the DEF_BLOCKS hash table.  */
  gcc_assert (def_blocks == NULL);
  def_blocks = htab_create (num_referenced_vars, def_blocks_hash,
                            def_blocks_eq, def_blocks_free);

  FOR_EACH_REFERENCED_VAR (cfun, var, rvi)
    set_current_def (var, NULL_TREE);
}


/* Deallocate internal data structures used by the renamer.  */

static void
fini_ssa_renamer (void)
{
  if (def_blocks)
    {
      htab_delete (def_blocks);
      def_blocks = NULL;
    }

  cfun->gimple_df->in_ssa_p = true;
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

   Steps 3 and 4 are done using the dominator tree walker
   (walk_dominator_tree).  */

static unsigned int
rewrite_into_ssa (void)
{
  bitmap_head *dfs;
  basic_block bb;

  /* Initialize operand data structures.  */
  init_ssa_operands ();

  /* Initialize internal data needed by the renamer.  */
  init_ssa_renamer ();

  /* Initialize the set of interesting blocks.  The callback
     mark_def_sites will add to this set those blocks that the renamer
     should process.  */
  interesting_blocks = sbitmap_alloc (last_basic_block);
  sbitmap_zero (interesting_blocks);

  /* Initialize dominance frontier.  */
  dfs = XNEWVEC (bitmap_head, last_basic_block);
  FOR_EACH_BB (bb)
    bitmap_initialize (&dfs[bb->index], &bitmap_default_obstack);

  /* 1- Compute dominance frontiers.  */
  calculate_dominance_info (CDI_DOMINATORS);
  compute_dominance_frontiers (dfs);

  /* 2- Find and mark definition sites.  */
  mark_def_site_blocks ();

  /* 3- Insert PHI nodes at dominance frontiers of definition blocks.  */
  insert_phi_nodes (dfs);

  /* 4- Rename all the blocks.  */
  rewrite_blocks (ENTRY_BLOCK_PTR, REWRITE_ALL);

  /* Free allocated memory.  */
  FOR_EACH_BB (bb)
    bitmap_clear (&dfs[bb->index]);
  free (dfs);

  sbitmap_free (interesting_blocks);

  fini_ssa_renamer ();

  return 0;
}


struct gimple_opt_pass pass_build_ssa =
{
 {
  GIMPLE_PASS,
  "ssa",				/* name */
  NULL,					/* gate */
  rewrite_into_ssa,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_SSA_OTHER,			/* tv_id */
  PROP_cfg | PROP_referenced_vars,	/* properties_required */
  PROP_ssa,				/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func
    | TODO_update_ssa_only_virtuals
    | TODO_verify_ssa
    | TODO_remove_unused_locals		/* todo_flags_finish */
 }
};


/* Mark the definition of VAR at STMT and BB as interesting for the
   renamer.  BLOCKS is the set of blocks that need updating.  */

static void
mark_def_interesting (tree var, gimple stmt, basic_block bb, bool insert_phi_p)
{
  gcc_assert (bitmap_bit_p (blocks_to_update, bb->index));
  set_register_defs (stmt, true);

  if (insert_phi_p)
    {
      bool is_phi_p = gimple_code (stmt) == GIMPLE_PHI;

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
   nodes.  */

static inline void
mark_use_interesting (tree var, gimple stmt, basic_block bb, bool insert_phi_p)
{
  basic_block def_bb = gimple_bb (stmt);

  mark_block_for_update (def_bb);
  mark_block_for_update (bb);

  if (gimple_code (stmt) == GIMPLE_PHI)
    mark_phi_for_rewrite (def_bb, stmt);
  else
    {
      set_rewrite_uses (stmt, true);

      if (is_gimple_debug (stmt))
	return;
    }

  /* If VAR has not been defined in BB, then it is live-on-entry
     to BB.  Note that we cannot just use the block holding VAR's
     definition because if VAR is one of the names in OLD_SSA_NAMES,
     it will have several definitions (itself and all the names that
     replace it).  */
  if (insert_phi_p)
    {
      struct def_blocks_d *db_p = get_def_blocks_for (var);
      if (!bitmap_bit_p (db_p->def_blocks, bb->index))
	set_livein_block (var, bb);
    }
}


/* Do a dominator walk starting at BB processing statements that
   reference symbols in SYMS_TO_RENAME.  This is very similar to
   mark_def_sites, but the scan handles statements whose operands may
   already be SSA names.

   If INSERT_PHI_P is true, mark those uses as live in the
   corresponding block.  This is later used by the PHI placement
   algorithm to make PHI pruning decisions.

   FIXME.  Most of this would be unnecessary if we could associate a
	   symbol to all the SSA names that reference it.  But that
	   sounds like it would be expensive to maintain.  Still, it
	   would be interesting to see if it makes better sense to do
	   that.  */

static void
prepare_block_for_update (basic_block bb, bool insert_phi_p)
{
  basic_block son;
  gimple_stmt_iterator si;
  edge e;
  edge_iterator ei;

  mark_block_for_update (bb);

  /* Process PHI nodes marking interesting those that define or use
     the symbols that we are interested in.  */
  for (si = gsi_start_phis (bb); !gsi_end_p (si); gsi_next (&si))
    {
      gimple phi = gsi_stmt (si);
      tree lhs_sym, lhs = gimple_phi_result (phi);

      lhs_sym = DECL_P (lhs) ? lhs : SSA_NAME_VAR (lhs);

      if (!symbol_marked_for_renaming (lhs_sym))
	continue;

      mark_def_interesting (lhs_sym, phi, bb, insert_phi_p);

      /* Mark the uses in phi nodes as interesting.  It would be more correct
	 to process the arguments of the phi nodes of the successor edges of
	 BB at the end of prepare_block_for_update, however, that turns out
	 to be significantly more expensive.  Doing it here is conservatively
	 correct -- it may only cause us to believe a value to be live in a
	 block that also contains its definition, and thus insert a few more
	 phi nodes for it.  */
      FOR_EACH_EDGE (e, ei, bb->preds)
	mark_use_interesting (lhs_sym, phi, e->src, insert_phi_p);
    }

  /* Process the statements.  */
  for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
    {
      gimple stmt;
      ssa_op_iter i;
      use_operand_p use_p;
      def_operand_p def_p;

      stmt = gsi_stmt (si);

      FOR_EACH_SSA_USE_OPERAND (use_p, stmt, i, SSA_OP_ALL_USES)
	{
	  tree use = USE_FROM_PTR (use_p);
	  tree sym = DECL_P (use) ? use : SSA_NAME_VAR (use);
	  if (symbol_marked_for_renaming (sym))
	    mark_use_interesting (sym, stmt, bb, insert_phi_p);
	}

      FOR_EACH_SSA_DEF_OPERAND (def_p, stmt, i, SSA_OP_ALL_DEFS)
	{
	  tree def = DEF_FROM_PTR (def_p);
	  tree sym = DECL_P (def) ? def : SSA_NAME_VAR (def);
	  if (symbol_marked_for_renaming (sym))
	    mark_def_interesting (sym, stmt, bb, insert_phi_p);
	}
    }

  /* Now visit all the blocks dominated by BB.  */
  for (son = first_dom_son (CDI_DOMINATORS, bb);
       son;
       son = next_dom_son (CDI_DOMINATORS, son))
    prepare_block_for_update (son, insert_phi_p);
}


/* Helper for prepare_names_to_update.  Mark all the use sites for
   NAME as interesting.  BLOCKS and INSERT_PHI_P are as in
   prepare_names_to_update.  */

static void
prepare_use_sites_for (tree name, bool insert_phi_p)
{
  use_operand_p use_p;
  imm_use_iterator iter;

  FOR_EACH_IMM_USE_FAST (use_p, iter, name)
    {
      gimple stmt = USE_STMT (use_p);
      basic_block bb = gimple_bb (stmt);

      if (gimple_code (stmt) == GIMPLE_PHI)
	{
	  int ix = PHI_ARG_INDEX_FROM_USE (use_p);
	  edge e = gimple_phi_arg_edge (stmt, ix);
	  mark_use_interesting (name, stmt, e->src, insert_phi_p);
	}
      else
	{
	  /* For regular statements, mark this as an interesting use
	     for NAME.  */
	  mark_use_interesting (name, stmt, bb, insert_phi_p);
	}
    }
}


/* Helper for prepare_names_to_update.  Mark the definition site for
   NAME as interesting.  BLOCKS and INSERT_PHI_P are as in
   prepare_names_to_update.  */

static void
prepare_def_site_for (tree name, bool insert_phi_p)
{
  gimple stmt;
  basic_block bb;

  gcc_assert (names_to_release == NULL
	      || !bitmap_bit_p (names_to_release, SSA_NAME_VERSION (name)));

  stmt = SSA_NAME_DEF_STMT (name);
  bb = gimple_bb (stmt);
  if (bb)
    {
      gcc_assert (bb->index < last_basic_block);
      mark_block_for_update (bb);
      mark_def_interesting (name, stmt, bb, insert_phi_p);
    }
}


/* Mark definition and use sites of names in NEW_SSA_NAMES and
   OLD_SSA_NAMES.  INSERT_PHI_P is true if the caller wants to insert
   PHI nodes for newly created names.  */

static void
prepare_names_to_update (bool insert_phi_p)
{
  unsigned i = 0;
  bitmap_iterator bi;
  sbitmap_iterator sbi;

  /* If a name N from NEW_SSA_NAMES is also marked to be released,
     remove it from NEW_SSA_NAMES so that we don't try to visit its
     defining basic block (which most likely doesn't exist).  Notice
     that we cannot do the same with names in OLD_SSA_NAMES because we
     want to replace existing instances.  */
  if (names_to_release)
    EXECUTE_IF_SET_IN_BITMAP (names_to_release, 0, i, bi)
      RESET_BIT (new_ssa_names, i);

  /* First process names in NEW_SSA_NAMES.  Otherwise, uses of old
     names may be considered to be live-in on blocks that contain
     definitions for their replacements.  */
  EXECUTE_IF_SET_IN_SBITMAP (new_ssa_names, 0, i, sbi)
    prepare_def_site_for (ssa_name (i), insert_phi_p);

  /* If an old name is in NAMES_TO_RELEASE, we cannot remove it from
     OLD_SSA_NAMES, but we have to ignore its definition site.  */
  EXECUTE_IF_SET_IN_SBITMAP (old_ssa_names, 0, i, sbi)
    {
      if (names_to_release == NULL || !bitmap_bit_p (names_to_release, i))
	prepare_def_site_for (ssa_name (i), insert_phi_p);
      prepare_use_sites_for (ssa_name (i), insert_phi_p);
    }
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

DEBUG_FUNCTION void
debug_names_replaced_by (tree name)
{
  dump_names_replaced_by (stderr, name);
}


/* Dump SSA update information to FILE.  */

void
dump_update_ssa (FILE *file)
{
  unsigned i = 0;
  bitmap_iterator bi;

  if (!need_ssa_update_p (cfun))
    return;

  if (new_ssa_names && sbitmap_first_set_bit (new_ssa_names) >= 0)
    {
      sbitmap_iterator sbi;

      fprintf (file, "\nSSA replacement table\n");
      fprintf (file, "N_i -> { O_1 ... O_j } means that N_i replaces "
	             "O_1, ..., O_j\n\n");

      EXECUTE_IF_SET_IN_SBITMAP (new_ssa_names, 0, i, sbi)
	dump_names_replaced_by (file, ssa_name (i));

      fprintf (file, "\n");
      fprintf (file, "Number of virtual NEW -> OLD mappings: %7u\n",
	       update_ssa_stats.num_virtual_mappings);
      fprintf (file, "Number of real NEW -> OLD mappings:    %7u\n",
	       update_ssa_stats.num_total_mappings
	       - update_ssa_stats.num_virtual_mappings);
      fprintf (file, "Number of total NEW -> OLD mappings:   %7u\n",
	       update_ssa_stats.num_total_mappings);

      fprintf (file, "\nNumber of virtual symbols: %u\n",
	       update_ssa_stats.num_virtual_symbols);
    }

  if (!bitmap_empty_p (SYMS_TO_RENAME (cfun)))
    {
      fprintf (file, "\n\nSymbols to be put in SSA form\n\n");
      dump_decl_set (file, SYMS_TO_RENAME (cfun));
      fprintf (file, "\n");
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


/* Dump SSA update information to stderr.  */

DEBUG_FUNCTION void
debug_update_ssa (void)
{
  dump_update_ssa (stderr);
}


/* Initialize data structures used for incremental SSA updates.  */

static void
init_update_ssa (struct function *fn)
{
  /* Reserve more space than the current number of names.  The calls to
     add_new_name_mapping are typically done after creating new SSA
     names, so we'll need to reallocate these arrays.  */
  old_ssa_names = sbitmap_alloc (num_ssa_names + NAME_SETS_GROWTH_FACTOR);
  sbitmap_zero (old_ssa_names);

  new_ssa_names = sbitmap_alloc (num_ssa_names + NAME_SETS_GROWTH_FACTOR);
  sbitmap_zero (new_ssa_names);

  repl_tbl = htab_create (20, repl_map_hash, repl_map_eq, repl_map_free);
  names_to_release = NULL;
  memset (&update_ssa_stats, 0, sizeof (update_ssa_stats));
  update_ssa_stats.virtual_symbols = BITMAP_ALLOC (NULL);
  update_ssa_initialized_fn = fn;
}


/* Deallocate data structures used for incremental SSA updates.  */

void
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

  bitmap_clear (SYMS_TO_RENAME (update_ssa_initialized_fn));
  BITMAP_FREE (update_ssa_stats.virtual_symbols);

  if (names_to_release)
    {
      EXECUTE_IF_SET_IN_BITMAP (names_to_release, 0, i, bi)
	release_ssa_name (ssa_name (i));
      BITMAP_FREE (names_to_release);
    }

  clear_ssa_name_info ();

  fini_ssa_renamer ();

  if (blocks_with_phis_to_rewrite)
    EXECUTE_IF_SET_IN_BITMAP (blocks_with_phis_to_rewrite, 0, i, bi)
      {
	gimple_vec phis = VEC_index (gimple_vec, phis_to_rewrite, i);

	VEC_free (gimple, heap, phis);
	VEC_replace (gimple_vec, phis_to_rewrite, i, NULL);
      }

  BITMAP_FREE (blocks_with_phis_to_rewrite);
  BITMAP_FREE (blocks_to_update);
  update_ssa_initialized_fn = NULL;
}


/* Create a new name for OLD_NAME in statement STMT and replace the
   operand pointed to by DEF_P with the newly created name.  Return
   the new name and register the replacement mapping <NEW, OLD> in
   update_ssa's tables.  */

tree
create_new_def_for (tree old_name, gimple stmt, def_operand_p def)
{
  tree new_name = duplicate_ssa_name (old_name, stmt);

  SET_DEF (def, new_name);

  if (gimple_code (stmt) == GIMPLE_PHI)
    {
      basic_block bb = gimple_bb (stmt);

      /* If needed, mark NEW_NAME as occurring in an abnormal PHI node. */
      SSA_NAME_OCCURS_IN_ABNORMAL_PHI (new_name) = bb_has_abnormal_pred (bb);
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
register_new_name_mapping (tree new_tree, tree old)
{
  if (!update_ssa_initialized_fn)
    init_update_ssa (cfun);

  gcc_assert (update_ssa_initialized_fn == cfun);

  add_new_name_mapping (new_tree, old);
}


/* Register symbol SYM to be renamed by update_ssa.  */

void
mark_sym_for_renaming (tree sym)
{
  bitmap_set_bit (SYMS_TO_RENAME (cfun), DECL_UID (sym));
}


/* Register all the symbols in SET to be renamed by update_ssa.  */

void
mark_set_for_renaming (bitmap set)
{
  bitmap_iterator bi;
  unsigned i;

  if (set == NULL || bitmap_empty_p (set))
    return;

  EXECUTE_IF_SET_IN_BITMAP (set, 0, i, bi)
    mark_sym_for_renaming (referenced_var (i));
}


/* Return true if there is any work to be done by update_ssa
   for function FN.  */

bool
need_ssa_update_p (struct function *fn)
{
  gcc_assert (fn != NULL);
  return (update_ssa_initialized_fn == fn
	  || (fn->gimple_df
	      && !bitmap_empty_p (SYMS_TO_RENAME (fn))));
}

/* Return true if SSA name mappings have been registered for SSA updating.  */

bool
name_mappings_registered_p (void)
{
  if (!update_ssa_initialized_fn)
    return false;

  gcc_assert (update_ssa_initialized_fn == cfun);

  return repl_tbl && htab_elements (repl_tbl) > 0;
}

/* Return true if name N has been registered in the replacement table.  */

bool
name_registered_for_update_p (tree n ATTRIBUTE_UNUSED)
{
  if (!update_ssa_initialized_fn)
    return false;

  gcc_assert (update_ssa_initialized_fn == cfun);

  return is_new_name (n) || is_old_name (n);
}


/* Return the set of all the SSA names marked to be replaced.  */

bitmap
ssa_names_to_replace (void)
{
  unsigned i = 0;
  bitmap ret;
  sbitmap_iterator sbi;

  gcc_assert (update_ssa_initialized_fn == NULL
	      || update_ssa_initialized_fn == cfun);

  ret = BITMAP_ALLOC (NULL);
  EXECUTE_IF_SET_IN_SBITMAP (old_ssa_names, 0, i, sbi)
    bitmap_set_bit (ret, i);

  return ret;
}


/* Mark NAME to be released after update_ssa has finished.  */

void
release_ssa_name_after_update_ssa (tree name)
{
  gcc_assert (cfun && update_ssa_initialized_fn == cfun);

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
     definition blocks are stored in DEF_BLOCKS[VAR]->DEF_BLOCKS.

     First, we compute the entry point to the region (ENTRY).  This is
     given by the nearest common dominator to all the definition
     blocks. When computing the iterated dominance frontier (IDF), any
     block not strictly dominated by ENTRY is ignored.

     We then call the standard PHI insertion algorithm with the pruned
     IDF.

   - If UPDATE_FLAGS == TODO_update_ssa_full_phi, the IDF for real
     names is not pruned.  PHI nodes are inserted at every IDF block.  */

static void
insert_updated_phi_nodes_for (tree var, bitmap_head *dfs, bitmap blocks,
                              unsigned update_flags)
{
  basic_block entry;
  struct def_blocks_d *db;
  bitmap idf, pruned_idf;
  bitmap_iterator bi;
  unsigned i;

  if (TREE_CODE (var) == SSA_NAME)
    gcc_checking_assert (is_old_name (var));
  else
    gcc_checking_assert (symbol_marked_for_renaming (var));

  /* Get all the definition sites for VAR.  */
  db = find_def_blocks_for (var);

  /* No need to do anything if there were no definitions to VAR.  */
  if (db == NULL || bitmap_empty_p (db->def_blocks))
    return;

  /* Compute the initial iterated dominance frontier.  */
  idf = compute_idf (db->def_blocks, dfs);
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
    }

  if (!bitmap_empty_p (pruned_idf))
    {
      /* Make sure that PRUNED_IDF blocks and all their feeding blocks
	 are included in the region to be updated.  The feeding blocks
	 are important to guarantee that the PHI arguments are renamed
	 properly.  */

      /* FIXME, this is not needed if we are updating symbols.  We are
	 already starting at the ENTRY block anyway.  */
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


/* Heuristic to determine whether SSA name mappings for virtual names
   should be discarded and their symbols rewritten from scratch.  When
   there is a large number of mappings for virtual names, the
   insertion of PHI nodes for the old names in the mappings takes
   considerable more time than if we inserted PHI nodes for the
   symbols instead.

   Currently the heuristic takes these stats into account:

   	- Number of mappings for virtual SSA names.
	- Number of distinct virtual symbols involved in those mappings.

   If the number of virtual mappings is much larger than the number of
   virtual symbols, then it will be faster to compute PHI insertion
   spots for the symbols.  Even if this involves traversing the whole
   CFG, which is what happens when symbols are renamed from scratch.  */

static bool
switch_virtuals_to_full_rewrite_p (void)
{
  if (update_ssa_stats.num_virtual_mappings < (unsigned) MIN_VIRTUAL_MAPPINGS)
    return false;

  if (update_ssa_stats.num_virtual_mappings
      > (unsigned) VIRTUAL_MAPPINGS_TO_SYMS_RATIO
        * update_ssa_stats.num_virtual_symbols)
    return true;

  return false;
}


/* Remove every virtual mapping and mark all the affected virtual
   symbols for renaming.  */

static void
switch_virtuals_to_full_rewrite (void)
{
  unsigned i = 0;
  sbitmap_iterator sbi;

  if (dump_file)
    {
      fprintf (dump_file, "\nEnabled virtual name mapping heuristic.\n");
      fprintf (dump_file, "\tNumber of virtual mappings:       %7u\n",
	       update_ssa_stats.num_virtual_mappings);
      fprintf (dump_file, "\tNumber of unique virtual symbols: %7u\n",
	       update_ssa_stats.num_virtual_symbols);
      fprintf (dump_file, "Updating FUD-chains from top of CFG will be "
	                  "faster than processing\nthe name mappings.\n\n");
    }

  /* Remove all virtual names from NEW_SSA_NAMES and OLD_SSA_NAMES.
     Note that it is not really necessary to remove the mappings from
     REPL_TBL, that would only waste time.  */
  EXECUTE_IF_SET_IN_SBITMAP (new_ssa_names, 0, i, sbi)
    if (!is_gimple_reg (ssa_name (i)))
      RESET_BIT (new_ssa_names, i);

  EXECUTE_IF_SET_IN_SBITMAP (old_ssa_names, 0, i, sbi)
    if (!is_gimple_reg (ssa_name (i)))
      RESET_BIT (old_ssa_names, i);

  mark_set_for_renaming (update_ssa_stats.virtual_symbols);
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
  basic_block bb, start_bb;
  bitmap_iterator bi;
  unsigned i = 0;
  bool insert_phi_p;
  sbitmap_iterator sbi;

  if (!need_ssa_update_p (cfun))
    return;

  timevar_push (TV_TREE_SSA_INCREMENTAL);

  if (!update_ssa_initialized_fn)
    init_update_ssa (cfun);
  gcc_assert (update_ssa_initialized_fn == cfun);

  blocks_with_phis_to_rewrite = BITMAP_ALLOC (NULL);
  if (!phis_to_rewrite)
    phis_to_rewrite = VEC_alloc (gimple_vec, heap, last_basic_block);
  blocks_to_update = BITMAP_ALLOC (NULL);

  /* Ensure that the dominance information is up-to-date.  */
  calculate_dominance_info (CDI_DOMINATORS);

  /* Only one update flag should be set.  */
  gcc_assert (update_flags == TODO_update_ssa
              || update_flags == TODO_update_ssa_no_phi
	      || update_flags == TODO_update_ssa_full_phi
	      || update_flags == TODO_update_ssa_only_virtuals);

  /* If we only need to update virtuals, remove all the mappings for
     real names before proceeding.  The caller is responsible for
     having dealt with the name mappings before calling update_ssa.  */
  if (update_flags == TODO_update_ssa_only_virtuals)
    {
      sbitmap_zero (old_ssa_names);
      sbitmap_zero (new_ssa_names);
      htab_empty (repl_tbl);
    }

  insert_phi_p = (update_flags != TODO_update_ssa_no_phi);

  if (insert_phi_p)
    {
      /* If the caller requested PHI nodes to be added, initialize
	 live-in information data structures (DEF_BLOCKS).  */

      /* For each SSA name N, the DEF_BLOCKS table describes where the
	 name is defined, which blocks have PHI nodes for N, and which
	 blocks have uses of N (i.e., N is live-on-entry in those
	 blocks).  */
      def_blocks = htab_create (num_ssa_names, def_blocks_hash,
				def_blocks_eq, def_blocks_free);
    }
  else
    {
      def_blocks = NULL;
    }

  /* Heuristic to avoid massive slow downs when the replacement
     mappings include lots of virtual names.  */
  if (insert_phi_p && switch_virtuals_to_full_rewrite_p ())
    switch_virtuals_to_full_rewrite ();

  /* If there are names defined in the replacement table, prepare
     definition and use sites for all the names in NEW_SSA_NAMES and
     OLD_SSA_NAMES.  */
  if (sbitmap_first_set_bit (new_ssa_names) >= 0)
    {
      prepare_names_to_update (insert_phi_p);

      /* If all the names in NEW_SSA_NAMES had been marked for
	 removal, and there are no symbols to rename, then there's
	 nothing else to do.  */
      if (sbitmap_first_set_bit (new_ssa_names) < 0
	  && bitmap_empty_p (SYMS_TO_RENAME (cfun)))
	goto done;
    }

  /* Next, determine the block at which to start the renaming process.  */
  if (!bitmap_empty_p (SYMS_TO_RENAME (cfun)))
    {
      /* If we have to rename some symbols from scratch, we need to
	 start the process at the root of the CFG.  FIXME, it should
	 be possible to determine the nearest block that had a
	 definition for each of the symbols that are marked for
	 updating.  For now this seems more work than it's worth.  */
      start_bb = ENTRY_BLOCK_PTR;

      /* Traverse the CFG looking for existing definitions and uses of
	 symbols in SYMS_TO_RENAME.  Mark interesting blocks and
	 statements and set local live-in information for the PHI
	 placement heuristics.  */
      prepare_block_for_update (start_bb, insert_phi_p);
    }
  else
    {
      /* Otherwise, the entry block to the region is the nearest
	 common dominator for the blocks in BLOCKS.  */
      start_bb = nearest_common_dominator_for_set (CDI_DOMINATORS,
						   blocks_to_update);
    }

  /* If requested, insert PHI nodes at the iterated dominance frontier
     of every block, creating new definitions for names in OLD_SSA_NAMES
     and for symbols in SYMS_TO_RENAME.  */
  if (insert_phi_p)
    {
      bitmap_head *dfs;

      /* If the caller requested PHI nodes to be added, compute
	 dominance frontiers.  */
      dfs = XNEWVEC (bitmap_head, last_basic_block);
      FOR_EACH_BB (bb)
	bitmap_initialize (&dfs[bb->index], &bitmap_default_obstack);
      compute_dominance_frontiers (dfs);

      if (sbitmap_first_set_bit (old_ssa_names) >= 0)
	{
	  sbitmap_iterator sbi;

	  /* insert_update_phi_nodes_for will call add_new_name_mapping
	     when inserting new PHI nodes, so the set OLD_SSA_NAMES
	     will grow while we are traversing it (but it will not
	     gain any new members).  Copy OLD_SSA_NAMES to a temporary
	     for traversal.  */
	  sbitmap tmp = sbitmap_alloc (old_ssa_names->n_bits);
	  sbitmap_copy (tmp, old_ssa_names);
	  EXECUTE_IF_SET_IN_SBITMAP (tmp, 0, i, sbi)
	    insert_updated_phi_nodes_for (ssa_name (i), dfs, blocks_to_update,
	                                  update_flags);
	  sbitmap_free (tmp);
	}

      EXECUTE_IF_SET_IN_BITMAP (SYMS_TO_RENAME (cfun), 0, i, bi)
	insert_updated_phi_nodes_for (referenced_var (i), dfs, blocks_to_update,
	                              update_flags);

      FOR_EACH_BB (bb)
	bitmap_clear (&dfs[bb->index]);
      free (dfs);

      /* Insertion of PHI nodes may have added blocks to the region.
	 We need to re-compute START_BB to include the newly added
	 blocks.  */
      if (start_bb != ENTRY_BLOCK_PTR)
	start_bb = nearest_common_dominator_for_set (CDI_DOMINATORS,
						     blocks_to_update);
    }

  /* Reset the current definition for name and symbol before renaming
     the sub-graph.  */
  EXECUTE_IF_SET_IN_SBITMAP (old_ssa_names, 0, i, sbi)
    set_current_def (ssa_name (i), NULL_TREE);

  EXECUTE_IF_SET_IN_BITMAP (SYMS_TO_RENAME (cfun), 0, i, bi)
    set_current_def (referenced_var (i), NULL_TREE);

  /* Now start the renaming process at START_BB.  */
  interesting_blocks = sbitmap_alloc (last_basic_block);
  sbitmap_zero (interesting_blocks);
  EXECUTE_IF_SET_IN_BITMAP (blocks_to_update, 0, i, bi)
    SET_BIT (interesting_blocks, i);

  rewrite_blocks (start_bb, REWRITE_UPDATE);

  sbitmap_free (interesting_blocks);

  /* Debugging dumps.  */
  if (dump_file)
    {
      int c;
      unsigned i;

      dump_update_ssa (dump_file);

      fprintf (dump_file, "Incremental SSA update started at block: %d\n\n",
	       start_bb->index);

      c = 0;
      EXECUTE_IF_SET_IN_BITMAP (blocks_to_update, 0, i, bi)
	c++;
      fprintf (dump_file, "Number of blocks in CFG: %d\n", last_basic_block);
      fprintf (dump_file, "Number of blocks to update: %d (%3.0f%%)\n\n",
	       c, PERCENT (c, last_basic_block));

      if (dump_flags & TDF_DETAILS)
	{
	  fprintf (dump_file, "Affected blocks: ");
	  EXECUTE_IF_SET_IN_BITMAP (blocks_to_update, 0, i, bi)
	    fprintf (dump_file, "%u ", i);
	  fprintf (dump_file, "\n");
	}

      fprintf (dump_file, "\n\n");
    }

  /* Free allocated memory.  */
done:
  delete_update_ssa ();

  timevar_pop (TV_TREE_SSA_INCREMENTAL);
}
