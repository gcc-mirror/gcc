/* Rewrite a program in Normal form into SSA.
   Copyright (C) 2001-2013 Free Software Foundation, Inc.
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
#include "function.h"
#include "gimple-pretty-print.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-ssa.h"
#include "tree-cfg.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "tree-ssanames.h"
#include "tree-into-ssa.h"
#include "tree-dfa.h"
#include "tree-ssa.h"
#include "tree-inline.h"
#include "hash-table.h"
#include "tree-pass.h"
#include "cfgloop.h"
#include "domwalk.h"
#include "params.h"
#include "diagnostic-core.h"
#include "tree-into-ssa.h"


/* This file builds the SSA form for a function as described in:
   R. Cytron, J. Ferrante, B. Rosen, M. Wegman, and K. Zadeck. Efficiently
   Computing Static Single Assignment Form and the Control Dependence
   Graph. ACM Transactions on Programming Languages and Systems,
   13(4):451-490, October 1991.  */

/* Structure to map a variable VAR to the set of blocks that contain
   definitions for VAR.  */
struct def_blocks_d
{
  /* Blocks that contain definitions of VAR.  Bit I will be set if the
     Ith block contains a definition of VAR.  */
  bitmap def_blocks;

  /* Blocks that contain a PHI node for VAR.  */
  bitmap phi_blocks;

  /* Blocks where VAR is live-on-entry.  Similar semantics as
     DEF_BLOCKS.  */
  bitmap livein_blocks;
};

typedef struct def_blocks_d *def_blocks_p;


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
static vec<tree> block_defs_stack;


/* Set of existing SSA names being replaced by update_ssa.  */
static sbitmap old_ssa_names;

/* Set of new SSA names being added by update_ssa.  Note that both
   NEW_SSA_NAMES and OLD_SSA_NAMES are dense bitmaps because most of
   the operations done on them are presence tests.  */
static sbitmap new_ssa_names;

static sbitmap interesting_blocks;

/* Set of SSA names that have been marked to be released after they
   were registered in the replacement table.  They will be finally
   released after we finish updating the SSA web.  */
static bitmap names_to_release;

/* vec of vec of PHIs to rewrite in a basic block.  Element I corresponds
   the to basic block with index I.  Allocated once per compilation, *not*
   released between different functions.  */
static vec<gimple_vec> phis_to_rewrite;

/* The bitmap of non-NULL elements of PHIS_TO_REWRITE.  */
static bitmap blocks_with_phis_to_rewrite;

/* Growth factor for NEW_SSA_NAMES and OLD_SSA_NAMES.  These sets need
   to grow as the callers to create_new_def_for will create new names on
   the fly.
   FIXME.  Currently set to 1/3 to avoid frequent reallocations but still
   need to find a reasonable growth strategy.  */
#define NAME_SETS_GROWTH_FACTOR	(MAX (3, num_ssa_names / 3))


/* The function the SSA updating data structures have been initialized for.
   NULL if they need to be initialized by create_new_def_for.  */
static struct function *update_ssa_initialized_fn = NULL;

/* Global data to attach to the main dominator walk structure.  */
struct mark_def_sites_global_data
{
  /* This bitmap contains the variables which are set before they
     are used in a basic block.  */
  bitmap kills;
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

/* Information stored for both SSA names and decls.  */
struct common_info_d
{
  /* This field indicates whether or not the variable may need PHI nodes.
     See the enum's definition for more detailed information about the
     states.  */
  ENUM_BITFIELD (need_phi_state) need_phi_state : 2;

  /* The current reaching definition replacing this var.  */
  tree current_def;

  /* Definitions for this var.  */
  struct def_blocks_d def_blocks;
};

/* The information associated with decls and SSA names.  */
typedef struct common_info_d *common_info_p;

/* Information stored for decls.  */
struct var_info_d
{
  /* The variable.  */
  tree var;

  /* Information stored for both SSA names and decls.  */
  struct common_info_d info;
};

/* The information associated with decls.  */
typedef struct var_info_d *var_info_p;


/* VAR_INFOS hashtable helpers.  */

struct var_info_hasher : typed_free_remove <var_info_d>
{
  typedef var_info_d value_type;
  typedef var_info_d compare_type;
  static inline hashval_t hash (const value_type *);
  static inline bool equal (const value_type *, const compare_type *);
};

inline hashval_t
var_info_hasher::hash (const value_type *p)
{
  return DECL_UID (p->var);
}

inline bool
var_info_hasher::equal (const value_type *p1, const compare_type *p2)
{
  return p1->var == p2->var;
}


/* Each entry in VAR_INFOS contains an element of type STRUCT 
   VAR_INFO_D.  */
static hash_table <var_info_hasher> var_infos;


/* Information stored for SSA names.  */
struct ssa_name_info
{
  /* Age of this record (so that info_for_ssa_name table can be cleared
     quickly); if AGE < CURRENT_INFO_FOR_SSA_NAME_AGE, then the fields
     are assumed to be null.  */
  unsigned age;

  /* Replacement mappings, allocated from update_ssa_obstack.  */
  bitmap repl_set;

  /* Information stored for both SSA names and decls.  */
  struct common_info_d info;
};

/* The information associated with names.  */
typedef struct ssa_name_info *ssa_name_info_p;

static vec<ssa_name_info_p> info_for_ssa_name;
static unsigned current_info_for_ssa_name_age;

static bitmap_obstack update_ssa_obstack;

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

/* The set of symbols we ought to re-write into SSA form in update_ssa.  */
static bitmap symbols_to_rename_set;
static vec<tree> symbols_to_rename;

/* Mark SYM for renaming.  */

static void
mark_for_renaming (tree sym)
{
  if (!symbols_to_rename_set)
    symbols_to_rename_set = BITMAP_ALLOC (NULL);
  if (bitmap_set_bit (symbols_to_rename_set, DECL_UID (sym)))
    symbols_to_rename.safe_push (sym);
}

/* Return true if SYM is marked for renaming.  */

static bool
marked_for_renaming (tree sym)
{
  if (!symbols_to_rename_set || sym == NULL_TREE)
    return false;
  return bitmap_bit_p (symbols_to_rename_set, DECL_UID (sym));
}


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
  unsigned len = info_for_ssa_name.length ();
  struct ssa_name_info *info;

  /* Re-allocate the vector at most once per update/into-SSA.  */
  if (ver >= len)
    info_for_ssa_name.safe_grow_cleared (num_ssa_names);

  /* But allocate infos lazily.  */
  info = info_for_ssa_name[ver];
  if (!info)
    {
      info = XCNEW (struct ssa_name_info);
      info->age = current_info_for_ssa_name_age;
      info->info.need_phi_state = NEED_PHI_STATE_UNKNOWN;
      info_for_ssa_name[ver] = info;
    }

  if (info->age < current_info_for_ssa_name_age)
    {
      info->age = current_info_for_ssa_name_age;
      info->repl_set = NULL;
      info->info.need_phi_state = NEED_PHI_STATE_UNKNOWN;
      info->info.current_def = NULL_TREE;
      info->info.def_blocks.def_blocks = NULL;
      info->info.def_blocks.phi_blocks = NULL;
      info->info.def_blocks.livein_blocks = NULL;
    }

  return info;
}

/* Return and allocate the auxiliar information for DECL.  */

static inline var_info_p
get_var_info (tree decl)
{
  struct var_info_d vi;
  var_info_d **slot;
  vi.var = decl;
  slot = var_infos.find_slot_with_hash (&vi, DECL_UID (decl), INSERT);
  if (*slot == NULL)
    {
      var_info_p v = XCNEW (struct var_info_d);
      v->var = decl;
      *slot = v;
      return v;
    }
  return *slot;
}


/* Clears info for SSA names.  */

static void
clear_ssa_name_info (void)
{
  current_info_for_ssa_name_age++;

  /* If current_info_for_ssa_name_age wraps we use stale information.
     Asser that this does not happen.  */
  gcc_assert (current_info_for_ssa_name_age != 0);
}


/* Get access to the auxiliar information stored per SSA name or decl.  */

static inline common_info_p
get_common_info (tree var)
{
  if (TREE_CODE (var) == SSA_NAME)
    return &get_ssa_name_ann (var)->info;
  else
    return &get_var_info (var)->info;
}


/* Return the current definition for VAR.  */

tree
get_current_def (tree var)
{
  return get_common_info (var)->current_def;
}


/* Sets current definition of VAR to DEF.  */

void
set_current_def (tree var, tree def)
{
  get_common_info (var)->current_def = def;
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
      gcc_checking_assert (!gimple_modified_p (stmt));
      set_rewrite_uses (stmt, false);
      set_register_defs (stmt, false);
    }
}

/* Mark block BB as interesting for update_ssa.  */

static void
mark_block_for_update (basic_block bb)
{
  gcc_checking_assert (blocks_to_update != NULL);
  if (!bitmap_set_bit (blocks_to_update, bb->index))
    return;
  initialize_flags_in_bb (bb);
}

/* Return the set of blocks where variable VAR is defined and the blocks
   where VAR is live on entry (livein).  If no entry is found in
   DEF_BLOCKS, a new one is created and returned.  */

static inline struct def_blocks_d *
get_def_blocks_for (common_info_p info)
{
  struct def_blocks_d *db_p = &info->def_blocks;
  if (!db_p->def_blocks)
    {
      db_p->def_blocks = BITMAP_ALLOC (&update_ssa_obstack);
      db_p->phi_blocks = BITMAP_ALLOC (&update_ssa_obstack);
      db_p->livein_blocks = BITMAP_ALLOC (&update_ssa_obstack);
    }

  return db_p;
}


/* Mark block BB as the definition site for variable VAR.  PHI_P is true if
   VAR is defined by a PHI node.  */

static void
set_def_block (tree var, basic_block bb, bool phi_p)
{
  struct def_blocks_d *db_p;
  common_info_p info;

  info = get_common_info (var);
  db_p = get_def_blocks_for (info);

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
  if (info->need_phi_state == NEED_PHI_STATE_UNKNOWN)
    info->need_phi_state = NEED_PHI_STATE_NO;
  else
    info->need_phi_state = NEED_PHI_STATE_MAYBE;
}


/* Mark block BB as having VAR live at the entry to BB.  */

static void
set_livein_block (tree var, basic_block bb)
{
  common_info_p info;
  struct def_blocks_d *db_p;

  info = get_common_info (var);
  db_p = get_def_blocks_for (info);

  /* Set the bit corresponding to the block where VAR is live in.  */
  bitmap_set_bit (db_p->livein_blocks, bb->index);

  /* Keep track of whether or not we may need to insert PHI nodes.

     If we reach here in NEED_PHI_STATE_NO, see if this use is dominated
     by the single block containing the definition(s) of this variable.  If
     it is, then we remain in NEED_PHI_STATE_NO, otherwise we transition to
     NEED_PHI_STATE_MAYBE.  */
  if (info->need_phi_state == NEED_PHI_STATE_NO)
    {
      int def_block_index = bitmap_first_set_bit (db_p->def_blocks);

      if (def_block_index == -1
	  || ! dominated_by_p (CDI_DOMINATORS, bb,
	                       BASIC_BLOCK (def_block_index)))
	info->need_phi_state = NEED_PHI_STATE_MAYBE;
    }
  else
    info->need_phi_state = NEED_PHI_STATE_MAYBE;
}


/* Return true if NAME is in OLD_SSA_NAMES.  */

static inline bool
is_old_name (tree name)
{
  unsigned ver = SSA_NAME_VERSION (name);
  if (!new_ssa_names)
    return false;
  return (ver < SBITMAP_SIZE (new_ssa_names)
	  && bitmap_bit_p (old_ssa_names, ver));
}


/* Return true if NAME is in NEW_SSA_NAMES.  */

static inline bool
is_new_name (tree name)
{
  unsigned ver = SSA_NAME_VERSION (name);
  if (!new_ssa_names)
    return false;
  return (ver < SBITMAP_SIZE (new_ssa_names)
	  && bitmap_bit_p (new_ssa_names, ver));
}


/* Return the names replaced by NEW_TREE (i.e., REPL_TBL[NEW_TREE].SET).  */

static inline bitmap
names_replaced_by (tree new_tree)
{
  return get_ssa_name_ann (new_tree)->repl_set;
}


/* Add OLD to REPL_TBL[NEW_TREE].SET.  */

static inline void
add_to_repl_tbl (tree new_tree, tree old)
{
  bitmap *set = &get_ssa_name_ann (new_tree)->repl_set;
  if (!*set)
    *set = BITMAP_ALLOC (&update_ssa_obstack);
  bitmap_set_bit (*set, SSA_NAME_VERSION (old));
}


/* Add a new mapping NEW_TREE -> OLD REPL_TBL.  Every entry N_i in REPL_TBL
   represents the set of names O_1 ... O_j replaced by N_i.  This is
   used by update_ssa and its helpers to introduce new SSA names in an
   already formed SSA web.  */

static void
add_new_name_mapping (tree new_tree, tree old)
{
  /* OLD and NEW_TREE must be different SSA names for the same symbol.  */
  gcc_checking_assert (new_tree != old
		       && SSA_NAME_VAR (new_tree) == SSA_NAME_VAR (old));

  /* We may need to grow NEW_SSA_NAMES and OLD_SSA_NAMES because our
     caller may have created new names since the set was created.  */
  if (SBITMAP_SIZE (new_ssa_names) <= num_ssa_names - 1)
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
  bitmap_set_bit (new_ssa_names, SSA_NAME_VERSION (new_tree));
  bitmap_set_bit (old_ssa_names, SSA_NAME_VERSION (old));
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

  gcc_checking_assert (blocks_to_update == NULL);
  set_register_defs (stmt, false);
  set_rewrite_uses (stmt, false);

  if (is_gimple_debug (stmt))
    {
      FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_USE)
	{
	  tree sym = USE_FROM_PTR (use_p);
	  gcc_checking_assert (DECL_P (sym));
	  set_rewrite_uses (stmt, true);
	}
      if (rewrite_uses_p (stmt))
	bitmap_set_bit (interesting_blocks, bb->index);
      return;
    }

  /* If a variable is used before being set, then the variable is live
     across a block boundary, so mark it live-on-entry to BB.  */
  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_ALL_USES)
    {
      tree sym = USE_FROM_PTR (use_p);
      gcc_checking_assert (DECL_P (sym));
      if (!bitmap_bit_p (kills, DECL_UID (sym)))
	set_livein_block (sym, bb);
      set_rewrite_uses (stmt, true);
    }

  /* Now process the defs.  Mark BB as the definition block and add
     each def to the set of killed symbols.  */
  FOR_EACH_SSA_TREE_OPERAND (def, stmt, iter, SSA_OP_ALL_DEFS)
    {
      gcc_checking_assert (DECL_P (def));
      set_def_block (def, bb, false);
      bitmap_set_bit (kills, DECL_UID (def));
      set_register_defs (stmt, true);
    }

  /* If we found the statement interesting then also mark the block BB
     as interesting.  */
  if (rewrite_uses_p (stmt) || register_defs_p (stmt))
    bitmap_set_bit (interesting_blocks, bb->index);
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
  vec<int> worklist;
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
  worklist.create (n_defs + 1);
  worklist.quick_push (1);
  top = 1;
  n_defs = 1;
  for (i = 1; i < adef; i++)
    {
      b = defs[i].bb_index;
      if (b == top)
	{
	  /* This is a closing element.  Interval corresponding to the top
	     of the stack after removing it follows.  */
	  worklist.pop ();
	  top = worklist[worklist.length () - 1];
	  defs[n_defs].bb_index = top;
	  defs[n_defs].dfs_num = defs[i].dfs_num + 1;
	}
      else
	{
	  /* Opening element.  Nothing to do, just push it to the stack and move
	     it to the correct position.  */
	  defs[n_defs].bb_index = defs[i].bb_index;
	  defs[n_defs].dfs_num = defs[i].dfs_num;
	  worklist.quick_push (b);
	  top = b;
	}

      /* If this interval starts at the same point as the previous one, cancel
	 the previous one.  */
      if (defs[n_defs].dfs_num == defs[n_defs - 1].dfs_num)
	defs[n_defs - 1].bb_index = defs[n_defs].bb_index;
      else
	n_defs++;
    }
  worklist.pop ();
  gcc_assert (worklist.is_empty ());

  /* Now process the uses.  */
  live_phis = BITMAP_ALLOC (NULL);
  EXECUTE_IF_SET_IN_BITMAP (uses, 0, i, bi)
    {
      worklist.safe_push (i);
    }

  while (!worklist.is_empty ())
    {
      b = worklist.pop ();
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
	  worklist.safe_push (u);
	}
    }

  worklist.release ();
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
  def_blocks_p p = &get_common_info (var)->def_blocks;
  if (!p->def_blocks)
    return NULL;
  return p;
}


/* Marks phi node PHI in basic block BB for rewrite.  */

static void
mark_phi_for_rewrite (basic_block bb, gimple phi)
{
  gimple_vec phis;
  unsigned n, idx = bb->index;

  if (rewrite_uses_p (phi))
    return;

  set_rewrite_uses (phi, true);

  if (!blocks_with_phis_to_rewrite)
    return;

  bitmap_set_bit (blocks_with_phis_to_rewrite, idx);

  n = (unsigned) last_basic_block + 1;
  if (phis_to_rewrite.length () < n)
    phis_to_rewrite.safe_grow_cleared (n);

  phis = phis_to_rewrite[idx];
  phis.reserve (10);

  phis.safe_push (phi);
  phis_to_rewrite[idx] = phis;
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
  struct def_blocks_d *def_map = find_def_blocks_for (var);

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

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "creating PHI node in block #%d for ", bb_index);
	  print_generic_expr (dump_file, var, TDF_SLIM);
	  fprintf (dump_file, "\n");
	}
      phi = NULL;

      if (TREE_CODE (var) == SSA_NAME)
	{
	  /* If we are rewriting SSA names, create the LHS of the PHI
	     node by duplicating VAR.  This is useful in the case of
	     pointers, to also duplicate pointer attributes (alias
	     information, in particular).  */
	  edge_iterator ei;
	  tree new_lhs;

	  gcc_checking_assert (update_p);
	  new_lhs = duplicate_ssa_name (var, NULL);
	  phi = create_phi_node (new_lhs, bb);
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

	  gcc_checking_assert (DECL_P (var));
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

/* Sort var_infos after DECL_UID of their var.  */

static int
insert_phi_nodes_compare_var_infos (const void *a, const void *b)
{
  const struct var_info_d *defa = *(struct var_info_d * const *)a;
  const struct var_info_d *defb = *(struct var_info_d * const *)b;
  if (DECL_UID (defa->var) < DECL_UID (defb->var))
    return -1;
  else
    return 1;
}

/* Insert PHI nodes at the dominance frontier of blocks with variable
   definitions.  DFS contains the dominance frontier information for
   the flowgraph.  */

static void
insert_phi_nodes (bitmap_head *dfs)
{
  hash_table <var_info_hasher>::iterator hi;
  unsigned i;
  var_info_p info;
  vec<var_info_p> vars;

  timevar_push (TV_TREE_INSERT_PHI_NODES);

  vars.create (var_infos.elements ());
  FOR_EACH_HASH_TABLE_ELEMENT (var_infos, info, var_info_p, hi)
    if (info->info.need_phi_state != NEED_PHI_STATE_NO)
      vars.quick_push (info);

  /* Do two stages to avoid code generation differences for UID
     differences but no UID ordering differences.  */
  vars.qsort (insert_phi_nodes_compare_var_infos);

  FOR_EACH_VEC_ELT (vars, i, info)
    {
      bitmap idf = compute_idf (info->info.def_blocks.def_blocks, dfs);
      insert_phi_nodes_for (info->var, idf, false);
      BITMAP_FREE (idf);
    }

  vars.release ();

  timevar_pop (TV_TREE_INSERT_PHI_NODES);
}


/* Push SYM's current reaching definition into BLOCK_DEFS_STACK and
   register DEF (an SSA_NAME) to be a new definition for SYM.  */

static void
register_new_def (tree def, tree sym)
{
  common_info_p info = get_common_info (sym);
  tree currdef;

  /* If this variable is set in a single basic block and all uses are
     dominated by the set(s) in that single basic block, then there is
     no reason to record anything for this variable in the block local
     definition stacks.  Doing so just wastes time and memory.

     This is the same test to prune the set of variables which may
     need PHI nodes.  So we just use that information since it's already
     computed and available for us to use.  */
  if (info->need_phi_state == NEED_PHI_STATE_NO)
    {
      info->current_def = def;
      return;
    }

  currdef = info->current_def;

  /* If SYM is not a GIMPLE register, then CURRDEF may be a name whose
     SSA_NAME_VAR is not necessarily SYM.  In this case, also push SYM
     in the stack so that we know which symbol is being defined by
     this SSA name when we unwind the stack.  */
  if (currdef && !is_gimple_reg (sym))
    block_defs_stack.safe_push (sym);

  /* Push the current reaching definition into BLOCK_DEFS_STACK.  This
     stack is later used by the dominator tree callbacks to restore
     the reaching definitions for all the variables defined in the
     block after a recursive visit to all its immediately dominated
     blocks.  If there is no current reaching definition, then just
     record the underlying _DECL node.  */
  block_defs_stack.safe_push (currdef ? currdef : sym);

  /* Set the current reaching definition for SYM to be DEF.  */
  info->current_def = def;
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
  common_info_p info = get_common_info (var);
  tree currdef;

  /* Lookup the current reaching definition for VAR.  */
  currdef = info->current_def;

  /* If there is no reaching definition for VAR, create and register a
     default definition for it (if needed).  */
  if (currdef == NULL_TREE)
    {
      tree sym = DECL_P (var) ? var : SSA_NAME_VAR (var);
      currdef = get_or_create_ssa_default_def (cfun, sym);
    }

  /* Return the current reaching definition for VAR, or the default
     definition, if we had to create one.  */
  return currdef;
}


/* Helper function for rewrite_stmt.  Rewrite uses in a debug stmt.  */

static void
rewrite_debug_stmt_uses (gimple stmt)
{
  use_operand_p use_p;
  ssa_op_iter iter;
  bool update = false;

  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_USE)
    {
      tree var = USE_FROM_PTR (use_p), def;
      common_info_p info = get_common_info (var);
      gcc_checking_assert (DECL_P (var));
      def = info->current_def;
      if (!def)
	{
	  if (TREE_CODE (var) == PARM_DECL && single_succ_p (ENTRY_BLOCK_PTR))
	    {
	      gimple_stmt_iterator gsi
		= gsi_after_labels (single_succ (ENTRY_BLOCK_PTR));
	      int lim;
	      /* Search a few source bind stmts at the start of first bb to
		 see if a DEBUG_EXPR_DECL can't be reused.  */
	      for (lim = 32;
		   !gsi_end_p (gsi) && lim > 0;
		   gsi_next (&gsi), lim--)
		{
		  gimple gstmt = gsi_stmt (gsi);
		  if (!gimple_debug_source_bind_p (gstmt))
		    break;
		  if (gimple_debug_source_bind_get_value (gstmt) == var)
		    {
		      def = gimple_debug_source_bind_get_var (gstmt);
		      if (TREE_CODE (def) == DEBUG_EXPR_DECL)
			break;
		      else
			def = NULL_TREE;
		    }
		}
	      /* If not, add a new source bind stmt.  */
	      if (def == NULL_TREE)
		{
		  gimple def_temp;
		  def = make_node (DEBUG_EXPR_DECL);
		  def_temp = gimple_build_debug_source_bind (def, var, NULL);
		  DECL_ARTIFICIAL (def) = 1;
		  TREE_TYPE (def) = TREE_TYPE (var);
		  DECL_MODE (def) = DECL_MODE (var);
		  gsi = gsi_after_labels (single_succ (ENTRY_BLOCK_PTR));
		  gsi_insert_before (&gsi, def_temp, GSI_SAME_STMT);
		}
	      update = true;
	    }
	}
      else
	{
	  /* Check if info->current_def can be trusted.  */
	  basic_block bb = gimple_bb (stmt);
	  basic_block def_bb
	      = SSA_NAME_IS_DEFAULT_DEF (def)
	      ? NULL : gimple_bb (SSA_NAME_DEF_STMT (def));

	  /* If definition is in current bb, it is fine.  */
	  if (bb == def_bb)
	    ;
	  /* If definition bb doesn't dominate the current bb,
	     it can't be used.  */
	  else if (def_bb && !dominated_by_p (CDI_DOMINATORS, bb, def_bb))
	    def = NULL;
	  /* If there is just one definition and dominates the current
	     bb, it is fine.  */
	  else if (info->need_phi_state == NEED_PHI_STATE_NO)
	    ;
	  else
	    {
	      struct def_blocks_d *db_p = get_def_blocks_for (info);

	      /* If there are some non-debug uses in the current bb,
		 it is fine.  */
	      if (bitmap_bit_p (db_p->livein_blocks, bb->index))
		;
	      /* Otherwise give up for now.  */
	      else
		def = NULL;
	    }
	}
      if (def == NULL)
	{
	  gimple_debug_bind_reset_value (stmt);
	  update_stmt (stmt);
	  return;
	}
      SET_USE (use_p, def);
    }
  if (update)
    update_stmt (stmt);
}

/* SSA Rewriting Step 2.  Rewrite every variable used in each statement in
   the block with its immediate reaching definitions.  Update the current
   definition of a variable when a new real or virtual definition is found.  */

static void
rewrite_stmt (gimple_stmt_iterator *si)
{
  use_operand_p use_p;
  def_operand_p def_p;
  ssa_op_iter iter;
  gimple stmt = gsi_stmt (*si);

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
    {
      if (is_gimple_debug (stmt))
	rewrite_debug_stmt_uses (stmt);
      else
	FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_ALL_USES)
	  {
	    tree var = USE_FROM_PTR (use_p);
	    gcc_checking_assert (DECL_P (var));
	    SET_USE (use_p, get_reaching_def (var));
	  }
    }

  /* Step 2.  Register the statement's DEF operands.  */
  if (register_defs_p (stmt))
    FOR_EACH_SSA_DEF_OPERAND (def_p, stmt, iter, SSA_OP_ALL_DEFS)
      {
	tree var = DEF_FROM_PTR (def_p);
	tree name;
	tree tracked_var;

	gcc_checking_assert (DECL_P (var));

	if (gimple_clobber_p (stmt)
	    && is_gimple_reg (var))
	  {
	    /* If we rewrite a DECL into SSA form then drop its
	       clobber stmts and replace uses with a new default def.  */
	    gcc_checking_assert (TREE_CODE (var) == VAR_DECL
				 && !gimple_vdef (stmt));
	    gsi_replace (si, gimple_build_nop (), true);
	    register_new_def (get_or_create_ssa_default_def (cfun, var), var);
	    break;
	  }

	name = make_ssa_name (var, stmt);
	SET_DEF (def_p, name);
	register_new_def (DEF_FROM_PTR (def_p), var);

	tracked_var = target_for_debug_bind (var);
	if (tracked_var)
	  {
	    gimple note = gimple_build_debug_bind (tracked_var, name, stmt);
	    gsi_insert_after (si, note, GSI_SAME_STMT);
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
	  tree currdef, res;
	  location_t loc;

	  phi = gsi_stmt (gsi);
	  res = gimple_phi_result (phi);
	  currdef = get_reaching_def (SSA_NAME_VAR (res));
	  /* Virtual operand PHI args do not need a location.  */
	  if (virtual_operand_p (res))
	    loc = UNKNOWN_LOCATION;
	  else
	    loc = gimple_location (SSA_NAME_DEF_STMT (currdef));
	  add_phi_arg (phi, currdef, e, loc);
	}
    }
}

class rewrite_dom_walker : public dom_walker
{
public:
  rewrite_dom_walker (cdi_direction direction) : dom_walker (direction) {}

  virtual void before_dom_children (basic_block);
  virtual void after_dom_children (basic_block);
};

/* SSA Rewriting Step 1.  Initialization, create a block local stack
   of reaching definitions for new SSA names produced in this block
   (BLOCK_DEFS).  Register new definitions for every PHI node in the
   block.  */

void
rewrite_dom_walker::before_dom_children (basic_block bb)
{
  gimple_stmt_iterator gsi;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n\nRenaming block #%d\n\n", bb->index);

  /* Mark the unwind point for this block.  */
  block_defs_stack.safe_push (NULL_TREE);

  /* Step 1.  Register new definitions for every PHI node in the block.
     Conceptually, all the PHI nodes are executed in parallel and each PHI
     node introduces a new version for the associated variable.  */
  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      tree result = gimple_phi_result (gsi_stmt (gsi));
      register_new_def (result, SSA_NAME_VAR (result));
    }

  /* Step 2.  Rewrite every variable used in each statement in the block
     with its immediate reaching definitions.  Update the current definition
     of a variable when a new real or virtual definition is found.  */
  if (bitmap_bit_p (interesting_blocks, bb->index))
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      rewrite_stmt (&gsi);

  /* Step 3.  Visit all the successor blocks of BB looking for PHI nodes.
     For every PHI node found, add a new argument containing the current
     reaching definition for the variable and the edge through which that
     definition is reaching the PHI node.  */
  rewrite_add_phi_arguments (bb);
}



/* Called after visiting all the statements in basic block BB and all
   of its dominator children.  Restore CURRDEFS to its original value.  */

void
rewrite_dom_walker::after_dom_children (basic_block bb ATTRIBUTE_UNUSED)
{
  /* Restore CURRDEFS to its original state.  */
  while (block_defs_stack.length () > 0)
    {
      tree tmp = block_defs_stack.pop ();
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
	    var = block_defs_stack.pop ();
	}
      else
	{
	  /* If we recorded anything else, it must have been a _DECL
	     node and its current reaching definition must have been
	     NULL.  */
	  saved_def = NULL;
	  var = tmp;
	}

      get_common_info (var)->current_def = saved_def;
    }
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
  for (j = (int) block_defs_stack.length () - 1; j >= 0; j--)
    {
      tree name, var;

      name = block_defs_stack[j];
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
	      var = block_defs_stack[j];
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
  unsigned i;
  tree var;

  if (symbols_to_rename.is_empty ())
    return;

  fprintf (file, "\n\nCurrent reaching definitions\n\n");
  FOR_EACH_VEC_ELT (symbols_to_rename, i, var)
    {
      common_info_p info = get_common_info (var);
      fprintf (file, "CURRDEF (");
      print_generic_expr (file, var, 0);
      fprintf (file, ") = ");
      if (info->current_def)
	print_generic_expr (file, info->current_def, 0);
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

  dump_var_infos (file);
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
htab_statistics (FILE *file, hash_table <var_info_hasher> htab)
{
  fprintf (file, "size %ld, %ld elements, %f collision/search ratio\n",
	   (long) htab.size (),
	   (long) htab.elements (),
	   htab.collisions ());
}


/* Dump SSA statistics on FILE.  */

void
dump_tree_ssa_stats (FILE *file)
{
  if (var_infos.is_created ())
    {
      fprintf (file, "\nHash table statistics:\n");
      fprintf (file, "    var_infos:   ");
      htab_statistics (file, var_infos);
      fprintf (file, "\n");
    }
}


/* Dump SSA statistics on stderr.  */

DEBUG_FUNCTION void
debug_tree_ssa_stats (void)
{
  dump_tree_ssa_stats (stderr);
}


/* Callback for htab_traverse to dump the VAR_INFOS hash table.  */

int
debug_var_infos_r (var_info_d **slot, FILE *file)
{
  struct var_info_d *info = *slot;

  fprintf (file, "VAR: ");
  print_generic_expr (file, info->var, dump_flags);
  bitmap_print (file, info->info.def_blocks.def_blocks,
		", DEF_BLOCKS: { ", "}");
  bitmap_print (file, info->info.def_blocks.livein_blocks,
		", LIVEIN_BLOCKS: { ", "}");
  bitmap_print (file, info->info.def_blocks.phi_blocks,
		", PHI_BLOCKS: { ", "}\n");

  return 1;
}


/* Dump the VAR_INFOS hash table on FILE.  */

void
dump_var_infos (FILE *file)
{
  fprintf (file, "\n\nDefinition and live-in blocks:\n\n");
  if (var_infos.is_created ())
    var_infos.traverse <FILE *, debug_var_infos_r> (file);
}


/* Dump the VAR_INFOS hash table on stderr.  */

DEBUG_FUNCTION void
debug_var_infos (void)
{
  dump_var_infos (stderr);
}


/* Register NEW_NAME to be the new reaching definition for OLD_NAME.  */

static inline void
register_new_update_single (tree new_name, tree old_name)
{
  common_info_p info = get_common_info (old_name);
  tree currdef = info->current_def;

  /* Push the current reaching definition into BLOCK_DEFS_STACK.
     This stack is later used by the dominator tree callbacks to
     restore the reaching definitions for all the variables
     defined in the block after a recursive visit to all its
     immediately dominated blocks.  */
  block_defs_stack.reserve (2);
  block_defs_stack.quick_push (currdef);
  block_defs_stack.quick_push (old_name);

  /* Set the current reaching definition for OLD_NAME to be
     NEW_NAME.  */
  info->current_def = new_name;
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

  if (marked_for_renaming (sym))
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

  if (marked_for_renaming (sym))
    rdef = get_var_info (sym)->info.current_def;
  else if (is_old_name (use))
    {
      rdef = get_ssa_name_ann (use)->info.current_def;
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
  if (marked_for_renaming (sym))
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
			gcc_checking_assert (!ef);
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

      phis = phis_to_rewrite[e->dest->index];
      FOR_EACH_VEC_ELT (phis, i, phi)
	{
	  tree arg, lhs_sym, reaching_def = NULL;
	  use_operand_p arg_p;

  	  gcc_checking_assert (rewrite_uses_p (phi));

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

	      if (marked_for_renaming (sym))
		reaching_def = get_reaching_def (sym);
	      else if (is_old_name (arg))
		reaching_def = get_reaching_def (arg);
	    }

          /* Update the argument if there is a reaching def.  */
	  if (reaching_def)
	    {
	      source_location locus;
	      int arg_i = PHI_ARG_INDEX_FROM_USE (arg_p);

	      SET_USE (arg_p, reaching_def);

	      /* Virtual operands do not need a location.  */
	      if (virtual_operand_p (reaching_def))
		locus = UNKNOWN_LOCATION;
	      else
		{
		  gimple stmt = SSA_NAME_DEF_STMT (reaching_def);

		  /* Single element PHI nodes  behave like copies, so get the
		     location from the phi argument.  */
		  if (gimple_code (stmt) == GIMPLE_PHI
		      && gimple_phi_num_args (stmt) == 1)
		    locus = gimple_phi_arg_location (stmt, 0);
		  else
		    locus = gimple_location (stmt);
		}

	      gimple_phi_arg_set_location (phi, arg_i, locus);
	    }


	  if (e->flags & EDGE_ABNORMAL)
	    SSA_NAME_OCCURS_IN_ABNORMAL_PHI (USE_FROM_PTR (arg_p)) = 1;
	}
    }
}

class rewrite_update_dom_walker : public dom_walker
{
public:
  rewrite_update_dom_walker (cdi_direction direction) : dom_walker (direction) {}

  virtual void before_dom_children (basic_block);
  virtual void after_dom_children (basic_block);
};

/* Initialization of block data structures for the incremental SSA
   update pass.  Create a block local stack of reaching definitions
   for new SSA names produced in this block (BLOCK_DEFS).  Register
   new definitions for every PHI node in the block.  */

void
rewrite_update_dom_walker::before_dom_children (basic_block bb)
{
  bool is_abnormal_phi;
  gimple_stmt_iterator gsi;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Registering new PHI nodes in block #%d\n",
	     bb->index);

  /* Mark the unwind point for this block.  */
  block_defs_stack.safe_push (NULL_TREE);

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

      if (marked_for_renaming (lhs_sym))
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
  if (bitmap_bit_p (interesting_blocks, bb->index))
    {
      gcc_checking_assert (bitmap_bit_p (blocks_to_update, bb->index));
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

void
rewrite_update_dom_walker::after_dom_children (basic_block bb ATTRIBUTE_UNUSED)
{
  while (block_defs_stack.length () > 0)
    {
      tree var = block_defs_stack.pop ();
      tree saved_def;

      /* NULL indicates the unwind stop point for this block (see
	 rewrite_update_enter_block).  */
      if (var == NULL)
	return;

      saved_def = block_defs_stack.pop ();
      get_common_info (var)->current_def = saved_def;
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
  /* Rewrite all the basic blocks in the program.  */
  timevar_push (TV_TREE_SSA_REWRITE_BLOCKS);

  block_defs_stack.create (10);

  /* Recursively walk the dominator tree rewriting each statement in
     each basic block.  */
  if (what == REWRITE_ALL)
      rewrite_dom_walker (CDI_DOMINATORS).walk (entry);
  else if (what == REWRITE_UPDATE)
      rewrite_update_dom_walker (CDI_DOMINATORS).walk (entry);
  else
    gcc_unreachable ();

  /* Debugging dumps.  */
  if (dump_file && (dump_flags & TDF_STATS))
    {
      dump_dfa_stats (dump_file);
      if (var_infos.is_created ())
	dump_tree_ssa_stats (dump_file);
    }

  block_defs_stack.release ();

  timevar_pop (TV_TREE_SSA_REWRITE_BLOCKS);
}

class mark_def_dom_walker : public dom_walker
{
public:
  mark_def_dom_walker (cdi_direction direction);
  ~mark_def_dom_walker ();

  virtual void before_dom_children (basic_block);

private:
  /* Notice that this bitmap is indexed using variable UIDs, so it must be
     large enough to accommodate all the variables referenced in the
     function, not just the ones we are renaming.  */
  bitmap m_kills;
};

mark_def_dom_walker::mark_def_dom_walker (cdi_direction direction)
  : dom_walker (direction), m_kills (BITMAP_ALLOC (NULL))
{
}

mark_def_dom_walker::~mark_def_dom_walker ()
{
  BITMAP_FREE (m_kills);
}

/* Block processing routine for mark_def_sites.  Clear the KILLS bitmap
   at the start of each block, and call mark_def_sites for each statement.  */

void
mark_def_dom_walker::before_dom_children (basic_block bb)
{
  gimple_stmt_iterator gsi;

  bitmap_clear (m_kills);
  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    mark_def_sites (bb, gsi_stmt (gsi), m_kills);
}

/* Initialize internal data needed during renaming.  */

static void
init_ssa_renamer (void)
{
  cfun->gimple_df->in_ssa_p = false;

  /* Allocate memory for the DEF_BLOCKS hash table.  */
  gcc_assert (!var_infos.is_created ());
  var_infos.create (vec_safe_length (cfun->local_decls));

  bitmap_obstack_initialize (&update_ssa_obstack);
}


/* Deallocate internal data structures used by the renamer.  */

static void
fini_ssa_renamer (void)
{
  if (var_infos.is_created ())
    var_infos.dispose ();

  bitmap_obstack_release (&update_ssa_obstack);

  cfun->gimple_df->ssa_renaming_needed = 0;
  cfun->gimple_df->rename_vops = 0;
  cfun->gimple_df->in_ssa_p = true;
}

/* Main entry point into the SSA builder.  The renaming process
   proceeds in four main phases:

   1- Compute dominance frontier and immediate dominators, needed to
      insert PHI nodes and rename the function in dominator tree
      order.

   2- Find and mark all the blocks that define variables.

   3- Insert PHI nodes at dominance frontiers (insert_phi_nodes).

   4- Rename all the blocks (rewrite_blocks) and statements in the program.

   Steps 3 and 4 are done using the dominator tree walker
   (walk_dominator_tree).  */

static unsigned int
rewrite_into_ssa (void)
{
  bitmap_head *dfs;
  basic_block bb;
  unsigned i;

  /* Initialize operand data structures.  */
  init_ssa_operands (cfun);

  /* Initialize internal data needed by the renamer.  */
  init_ssa_renamer ();

  /* Initialize the set of interesting blocks.  The callback
     mark_def_sites will add to this set those blocks that the renamer
     should process.  */
  interesting_blocks = sbitmap_alloc (last_basic_block);
  bitmap_clear (interesting_blocks);

  /* Initialize dominance frontier.  */
  dfs = XNEWVEC (bitmap_head, last_basic_block);
  FOR_EACH_BB (bb)
    bitmap_initialize (&dfs[bb->index], &bitmap_default_obstack);

  /* 1- Compute dominance frontiers.  */
  calculate_dominance_info (CDI_DOMINATORS);
  compute_dominance_frontiers (dfs);

  /* 2- Find and mark definition sites.  */
  mark_def_dom_walker (CDI_DOMINATORS).walk (cfun->cfg->x_entry_block_ptr);

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

  /* Try to get rid of all gimplifier generated temporaries by making
     its SSA names anonymous.  This way we can garbage collect them
     all after removing unused locals which we do in our TODO.  */
  for (i = 1; i < num_ssa_names; ++i)
    {
      tree decl, name = ssa_name (i);
      if (!name
	  || SSA_NAME_IS_DEFAULT_DEF (name))
	continue;
      decl = SSA_NAME_VAR (name);
      if (decl
	  && TREE_CODE (decl) == VAR_DECL
	  && !VAR_DECL_IS_VIRTUAL_OPERAND (decl)
	  && DECL_IGNORED_P (decl))
	SET_SSA_NAME_VAR_OR_IDENTIFIER (name, DECL_NAME (decl));
    }

  return 0;
}

/* Gate for IPCP optimization.  */

static bool
gate_into_ssa (void)
{
  /* Do nothing for funcions that was produced already in SSA form.  */
  return !(cfun->curr_properties & PROP_ssa);
}

namespace {

const pass_data pass_data_build_ssa =
{
  GIMPLE_PASS, /* type */
  "ssa", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_TREE_SSA_OTHER, /* tv_id */
  PROP_cfg, /* properties_required */
  PROP_ssa, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_verify_ssa | TODO_remove_unused_locals ), /* todo_flags_finish */
};

class pass_build_ssa : public gimple_opt_pass
{
public:
  pass_build_ssa (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_build_ssa, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_into_ssa (); }
  unsigned int execute () { return rewrite_into_ssa (); }

}; // class pass_build_ssa

} // anon namespace

gimple_opt_pass *
make_pass_build_ssa (gcc::context *ctxt)
{
  return new pass_build_ssa (ctxt);
}


/* Mark the definition of VAR at STMT and BB as interesting for the
   renamer.  BLOCKS is the set of blocks that need updating.  */

static void
mark_def_interesting (tree var, gimple stmt, basic_block bb, bool insert_phi_p)
{
  gcc_checking_assert (bitmap_bit_p (blocks_to_update, bb->index));
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
      struct def_blocks_d *db_p = get_def_blocks_for (get_common_info (var));
      if (!bitmap_bit_p (db_p->def_blocks, bb->index))
	set_livein_block (var, bb);
    }
}


/* Do a dominator walk starting at BB processing statements that
   reference symbols in SSA operands.  This is very similar to
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

      if (TREE_CODE (lhs) == SSA_NAME
	  && (! virtual_operand_p (lhs)
	      || ! cfun->gimple_df->rename_vops))
	continue;

      lhs_sym = DECL_P (lhs) ? lhs : SSA_NAME_VAR (lhs);
      mark_for_renaming (lhs_sym);
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

      if (cfun->gimple_df->rename_vops
	  && gimple_vuse (stmt))
	{
	  tree use = gimple_vuse (stmt);
	  tree sym = DECL_P (use) ? use : SSA_NAME_VAR (use);
	  mark_for_renaming (sym);
	  mark_use_interesting (sym, stmt, bb, insert_phi_p);
	}

      FOR_EACH_SSA_USE_OPERAND (use_p, stmt, i, SSA_OP_USE)
	{
	  tree use = USE_FROM_PTR (use_p);
	  if (!DECL_P (use))
	    continue;
	  mark_for_renaming (use);
	  mark_use_interesting (use, stmt, bb, insert_phi_p);
	}

      if (cfun->gimple_df->rename_vops
	  && gimple_vdef (stmt))
	{
	  tree def = gimple_vdef (stmt);
	  tree sym = DECL_P (def) ? def : SSA_NAME_VAR (def);
	  mark_for_renaming (sym);
	  mark_def_interesting (sym, stmt, bb, insert_phi_p);
	}

      FOR_EACH_SSA_DEF_OPERAND (def_p, stmt, i, SSA_OP_DEF)
	{
	  tree def = DEF_FROM_PTR (def_p);
	  if (!DECL_P (def))
	    continue;
	  mark_for_renaming (def);
	  mark_def_interesting (def, stmt, bb, insert_phi_p);
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

  gcc_checking_assert (names_to_release == NULL
		       || !bitmap_bit_p (names_to_release,
					 SSA_NAME_VERSION (name)));

  stmt = SSA_NAME_DEF_STMT (name);
  bb = gimple_bb (stmt);
  if (bb)
    {
      gcc_checking_assert (bb->index < last_basic_block);
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
      bitmap_clear_bit (new_ssa_names, i);

  /* First process names in NEW_SSA_NAMES.  Otherwise, uses of old
     names may be considered to be live-in on blocks that contain
     definitions for their replacements.  */
  EXECUTE_IF_SET_IN_BITMAP (new_ssa_names, 0, i, sbi)
    prepare_def_site_for (ssa_name (i), insert_phi_p);

  /* If an old name is in NAMES_TO_RELEASE, we cannot remove it from
     OLD_SSA_NAMES, but we have to ignore its definition site.  */
  EXECUTE_IF_SET_IN_BITMAP (old_ssa_names, 0, i, sbi)
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

  if (new_ssa_names && bitmap_first_set_bit (new_ssa_names) >= 0)
    {
      sbitmap_iterator sbi;

      fprintf (file, "\nSSA replacement table\n");
      fprintf (file, "N_i -> { O_1 ... O_j } means that N_i replaces "
	             "O_1, ..., O_j\n\n");

      EXECUTE_IF_SET_IN_BITMAP (new_ssa_names, 0, i, sbi)
	dump_names_replaced_by (file, ssa_name (i));
    }

  if (symbols_to_rename_set && !bitmap_empty_p (symbols_to_rename_set))
    {
      fprintf (file, "\nSymbols to be put in SSA form\n");
      dump_decl_set (file, symbols_to_rename_set);
      fprintf (file, "\n");
    }

  if (names_to_release && !bitmap_empty_p (names_to_release))
    {
      fprintf (file, "\nSSA names to release after updating the SSA web\n\n");
      EXECUTE_IF_SET_IN_BITMAP (names_to_release, 0, i, bi)
	{
	  print_generic_expr (file, ssa_name (i), 0);
	  fprintf (file, " ");
	}
      fprintf (file, "\n");
    }
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
  bitmap_clear (old_ssa_names);

  new_ssa_names = sbitmap_alloc (num_ssa_names + NAME_SETS_GROWTH_FACTOR);
  bitmap_clear (new_ssa_names);

  bitmap_obstack_initialize (&update_ssa_obstack);

  names_to_release = NULL;
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

  BITMAP_FREE (symbols_to_rename_set);
  symbols_to_rename_set = NULL;
  symbols_to_rename.release ();

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
	gimple_vec phis = phis_to_rewrite[i];
	phis.release ();
	phis_to_rewrite[i].create (0);
      }

  BITMAP_FREE (blocks_with_phis_to_rewrite);
  BITMAP_FREE (blocks_to_update);

  update_ssa_initialized_fn = NULL;
}


/* Create a new name for OLD_NAME in statement STMT and replace the
   operand pointed to by DEF_P with the newly created name.  If DEF_P
   is NULL then STMT should be a GIMPLE assignment.
   Return the new name and register the replacement mapping <NEW, OLD> in
   update_ssa's tables.  */

tree
create_new_def_for (tree old_name, gimple stmt, def_operand_p def)
{
  tree new_name;

  timevar_push (TV_TREE_SSA_INCREMENTAL);

  if (!update_ssa_initialized_fn)
    init_update_ssa (cfun);

  gcc_assert (update_ssa_initialized_fn == cfun);

  new_name = duplicate_ssa_name (old_name, stmt);
  if (def)
    SET_DEF (def, new_name);
  else
    gimple_assign_set_lhs (stmt, new_name);

  if (gimple_code (stmt) == GIMPLE_PHI)
    {
      basic_block bb = gimple_bb (stmt);

      /* If needed, mark NEW_NAME as occurring in an abnormal PHI node. */
      SSA_NAME_OCCURS_IN_ABNORMAL_PHI (new_name) = bb_has_abnormal_pred (bb);
    }

  add_new_name_mapping (new_name, old_name);

  /* For the benefit of passes that will be updating the SSA form on
     their own, set the current reaching definition of OLD_NAME to be
     NEW_NAME.  */
  get_ssa_name_ann (old_name)->info.current_def = new_name;

  timevar_pop (TV_TREE_SSA_INCREMENTAL);

  return new_name;
}


/* Mark virtual operands of FN for renaming by update_ssa.  */

void
mark_virtual_operands_for_renaming (struct function *fn)
{
  fn->gimple_df->ssa_renaming_needed = 1;
  fn->gimple_df->rename_vops = 1;
}

/* Replace all uses of NAME by underlying variable and mark it
   for renaming.  This assumes the defining statement of NAME is
   going to be removed.  */

void
mark_virtual_operand_for_renaming (tree name)
{
  tree name_var = SSA_NAME_VAR (name);
  bool used = false;
  imm_use_iterator iter;
  use_operand_p use_p;
  gimple stmt;

  gcc_assert (VAR_DECL_IS_VIRTUAL_OPERAND (name_var));
  FOR_EACH_IMM_USE_STMT (stmt, iter, name)
    {
      FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
        SET_USE (use_p, name_var);
      used = true;
    }
  if (used)
    mark_virtual_operands_for_renaming (cfun);
}

/* Replace all uses of the virtual PHI result by its underlying variable
   and mark it for renaming.  This assumes the PHI node is going to be
   removed.  */

void
mark_virtual_phi_result_for_renaming (gimple phi)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Marking result for renaming : ");
      print_gimple_stmt (dump_file, phi, 0, TDF_SLIM);
      fprintf (dump_file, "\n");
    }

  mark_virtual_operand_for_renaming (gimple_phi_result (phi));
}

/* Return true if there is any work to be done by update_ssa
   for function FN.  */

bool
need_ssa_update_p (struct function *fn)
{
  gcc_assert (fn != NULL);
  return (update_ssa_initialized_fn == fn
	  || (fn->gimple_df && fn->gimple_df->ssa_renaming_needed));
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
    gcc_checking_assert (marked_for_renaming (var));

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
	  gcc_checking_assert (update_flags == TODO_update_ssa_full_phi);
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

/* Sort symbols_to_rename after their DECL_UID.  */

static int
insert_updated_phi_nodes_compare_uids (const void *a, const void *b)
{
  const_tree syma = *(const const_tree *)a;
  const_tree symb = *(const const_tree *)b;
  if (DECL_UID (syma) == DECL_UID (symb))
    return 0;
  return DECL_UID (syma) < DECL_UID (symb) ? -1 : 1;
}

/* Given a set of newly created SSA names (NEW_SSA_NAMES) and a set of
   existing SSA names (OLD_SSA_NAMES), update the SSA form so that:

   1- The names in OLD_SSA_NAMES dominated by the definitions of
      NEW_SSA_NAMES are all re-written to be reached by the
      appropriate definition from NEW_SSA_NAMES.

   2- If needed, new PHI nodes are added to the iterated dominance
      frontier of the blocks where each of NEW_SSA_NAMES are defined.

   The mapping between OLD_SSA_NAMES and NEW_SSA_NAMES is setup by
   calling create_new_def_for to create new defs for names that the
   caller wants to replace.

   The caller cretaes the new names to be inserted and the names that need
   to be replaced by calling create_new_def_for for each old definition
   to be replaced.  Note that the function assumes that the
   new defining statement has already been inserted in the IL.

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
  tree sym;

  /* Only one update flag should be set.  */
  gcc_assert (update_flags == TODO_update_ssa
              || update_flags == TODO_update_ssa_no_phi
	      || update_flags == TODO_update_ssa_full_phi
	      || update_flags == TODO_update_ssa_only_virtuals);

  if (!need_ssa_update_p (cfun))
    return;

  timevar_push (TV_TREE_SSA_INCREMENTAL);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nUpdating SSA:\n");

  if (!update_ssa_initialized_fn)
    init_update_ssa (cfun);
  else if (update_flags == TODO_update_ssa_only_virtuals)
    {
      /* If we only need to update virtuals, remove all the mappings for
	 real names before proceeding.  The caller is responsible for
	 having dealt with the name mappings before calling update_ssa.  */
      bitmap_clear (old_ssa_names);
      bitmap_clear (new_ssa_names);
    }

  gcc_assert (update_ssa_initialized_fn == cfun);

  blocks_with_phis_to_rewrite = BITMAP_ALLOC (NULL);
  if (!phis_to_rewrite.exists ())
    phis_to_rewrite.create (last_basic_block + 1);
  blocks_to_update = BITMAP_ALLOC (NULL);

  /* Ensure that the dominance information is up-to-date.  */
  calculate_dominance_info (CDI_DOMINATORS);

  insert_phi_p = (update_flags != TODO_update_ssa_no_phi);

  /* If there are names defined in the replacement table, prepare
     definition and use sites for all the names in NEW_SSA_NAMES and
     OLD_SSA_NAMES.  */
  if (bitmap_first_set_bit (new_ssa_names) >= 0)
    {
      prepare_names_to_update (insert_phi_p);

      /* If all the names in NEW_SSA_NAMES had been marked for
	 removal, and there are no symbols to rename, then there's
	 nothing else to do.  */
      if (bitmap_first_set_bit (new_ssa_names) < 0
	  && !cfun->gimple_df->ssa_renaming_needed)
	goto done;
    }

  /* Next, determine the block at which to start the renaming process.  */
  if (cfun->gimple_df->ssa_renaming_needed)
    {
      /* If we rename bare symbols initialize the mapping to
         auxiliar info we need to keep track of.  */
      var_infos.create (47);

      /* If we have to rename some symbols from scratch, we need to
	 start the process at the root of the CFG.  FIXME, it should
	 be possible to determine the nearest block that had a
	 definition for each of the symbols that are marked for
	 updating.  For now this seems more work than it's worth.  */
      start_bb = ENTRY_BLOCK_PTR;

      /* Traverse the CFG looking for existing definitions and uses of
	 symbols in SSA operands.  Mark interesting blocks and
	 statements and set local live-in information for the PHI
	 placement heuristics.  */
      prepare_block_for_update (start_bb, insert_phi_p);

#ifdef ENABLE_CHECKING
      for (i = 1; i < num_ssa_names; ++i)
	{
	  tree name = ssa_name (i);
	  if (!name
	      || virtual_operand_p (name))
	    continue;

	  /* For all but virtual operands, which do not have SSA names
	     with overlapping life ranges, ensure that symbols marked
	     for renaming do not have existing SSA names associated with
	     them as we do not re-write them out-of-SSA before going
	     into SSA for the remaining symbol uses.  */
	  if (marked_for_renaming (SSA_NAME_VAR (name)))
	    {
	      fprintf (stderr, "Existing SSA name for symbol marked for "
		       "renaming: ");
	      print_generic_expr (stderr, name, TDF_SLIM);
	      fprintf (stderr, "\n");
	      internal_error ("SSA corruption");
	    }
	}
#endif
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
     and for symbols found.  */
  if (insert_phi_p)
    {
      bitmap_head *dfs;

      /* If the caller requested PHI nodes to be added, compute
	 dominance frontiers.  */
      dfs = XNEWVEC (bitmap_head, last_basic_block);
      FOR_EACH_BB (bb)
	bitmap_initialize (&dfs[bb->index], &bitmap_default_obstack);
      compute_dominance_frontiers (dfs);

      if (bitmap_first_set_bit (old_ssa_names) >= 0)
	{
	  sbitmap_iterator sbi;

	  /* insert_update_phi_nodes_for will call add_new_name_mapping
	     when inserting new PHI nodes, so the set OLD_SSA_NAMES
	     will grow while we are traversing it (but it will not
	     gain any new members).  Copy OLD_SSA_NAMES to a temporary
	     for traversal.  */
	  sbitmap tmp = sbitmap_alloc (SBITMAP_SIZE (old_ssa_names));
	  bitmap_copy (tmp, old_ssa_names);
	  EXECUTE_IF_SET_IN_BITMAP (tmp, 0, i, sbi)
	    insert_updated_phi_nodes_for (ssa_name (i), dfs, blocks_to_update,
	                                  update_flags);
	  sbitmap_free (tmp);
	}

      symbols_to_rename.qsort (insert_updated_phi_nodes_compare_uids);
      FOR_EACH_VEC_ELT (symbols_to_rename, i, sym)
	insert_updated_phi_nodes_for (sym, dfs, blocks_to_update,
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
  EXECUTE_IF_SET_IN_BITMAP (old_ssa_names, 0, i, sbi)
    get_ssa_name_ann (ssa_name (i))->info.current_def = NULL_TREE;

  FOR_EACH_VEC_ELT (symbols_to_rename, i, sym)
    get_var_info (sym)->info.current_def = NULL_TREE;

  /* Now start the renaming process at START_BB.  */
  interesting_blocks = sbitmap_alloc (last_basic_block);
  bitmap_clear (interesting_blocks);
  EXECUTE_IF_SET_IN_BITMAP (blocks_to_update, 0, i, bi)
    bitmap_set_bit (interesting_blocks, i);

  rewrite_blocks (start_bb, REWRITE_UPDATE);

  sbitmap_free (interesting_blocks);

  /* Debugging dumps.  */
  if (dump_file)
    {
      int c;
      unsigned i;

      dump_update_ssa (dump_file);

      fprintf (dump_file, "Incremental SSA update started at block: %d\n",
	       start_bb->index);

      c = 0;
      EXECUTE_IF_SET_IN_BITMAP (blocks_to_update, 0, i, bi)
	c++;
      fprintf (dump_file, "Number of blocks in CFG: %d\n", last_basic_block);
      fprintf (dump_file, "Number of blocks to update: %d (%3.0f%%)\n",
	       c, PERCENT (c, last_basic_block));

      if (dump_flags & TDF_DETAILS)
	{
	  fprintf (dump_file, "Affected blocks:");
	  EXECUTE_IF_SET_IN_BITMAP (blocks_to_update, 0, i, bi)
	    fprintf (dump_file, " %u", i);
	  fprintf (dump_file, "\n");
	}

      fprintf (dump_file, "\n\n");
    }

  /* Free allocated memory.  */
done:
  delete_update_ssa ();

  timevar_pop (TV_TREE_SSA_INCREMENTAL);
}
