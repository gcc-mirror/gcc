/* Single entry single exit control flow regions.
   Copyright (C) 2008, 2009  Free Software Foundation, Inc.
   Contributed by Jan Sjodin <jan.sjodin@amd.com> and
   Sebastian Pop <sebastian.pop@amd.com>.

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

#ifndef GCC_SESE_H
#define GCC_SESE_H

/* A Single Entry, Single Exit region is a part of the CFG delimited
   by two edges.  */
typedef struct sese_s
{
  /* Single ENTRY and single EXIT from the SESE region.  */
  edge entry, exit;

  /* Parameters used within the SCOP.  */
  VEC (tree, heap) *params;

  /* Used to quickly retrieve the index of a parameter in PARAMS.  */
  htab_t params_index;

  /* Store the names of the parameters that are passed to CLooG.  */
  char **params_names;

  /* Loops completely contained in the SCOP.  */
  bitmap loops;
  VEC (loop_p, heap) *loop_nest;

  /* Are we allowed to add more params?  This is for debugging purpose.  We
     can only add new params before generating the bb domains, otherwise they
     become invalid.  */
  bool add_params;
} *sese;

#define SESE_ENTRY(S) (S->entry)
#define SESE_ENTRY_BB(S) (S->entry->dest)
#define SESE_EXIT(S) (S->exit)
#define SESE_EXIT_BB(S) (S->exit->dest)
#define SESE_PARAMS(S) (S->params)
#define SESE_PARAMS_INDEX(S) (S->params_index)
#define SESE_PARAMS_NAMES(S) (S->params_names)
#define SESE_LOOPS(S) (S->loops)
#define SESE_LOOP_NEST(S) (S->loop_nest)
#define SESE_ADD_PARAMS(S) (S->add_params)

extern sese new_sese (edge, edge);
extern void free_sese (sese);
extern void sese_insert_phis_for_liveouts (sese, basic_block, edge, edge);
extern void sese_adjust_liveout_phis (sese, htab_t, basic_block, edge, edge);
extern void build_sese_loop_nests (sese);
extern edge copy_bb_and_scalar_dependences (basic_block, sese, edge, htab_t);
extern struct loop *outermost_loop_in_sese (sese, basic_block);
extern void insert_loop_close_phis (htab_t, loop_p);
extern void insert_guard_phis (basic_block, edge, edge, htab_t, htab_t);
extern void sese_reset_aux_in_loops (sese);
extern tree scalar_evolution_in_region (sese, loop_p, tree);

/* Check that SESE contains LOOP.  */

static inline bool
sese_contains_loop (sese sese, struct loop *loop)
{
  return bitmap_bit_p (SESE_LOOPS (sese), loop->num);
}

/* The number of parameters in REGION. */

static inline unsigned
sese_nb_params (sese region)
{
  return VEC_length (tree, SESE_PARAMS (region));
}

/* Checks whether BB is contained in the region delimited by ENTRY and
   EXIT blocks.  */

static inline bool
bb_in_region (basic_block bb, basic_block entry, basic_block exit)
{
#ifdef ENABLE_CHECKING
  {
    edge e;
    edge_iterator ei;

    /* Check that there are no edges coming in the region: all the
       predecessors of EXIT are dominated by ENTRY.  */
    FOR_EACH_EDGE (e, ei, exit->preds)
      dominated_by_p (CDI_DOMINATORS, e->src, entry);
 
    /* Check that there are no edges going out of the region: the
       entry is post-dominated by the exit.  FIXME: This cannot be
       checked right now as the CDI_POST_DOMINATORS are needed.  */
  }
#endif

  return dominated_by_p (CDI_DOMINATORS, bb, entry)
	 && !(dominated_by_p (CDI_DOMINATORS, bb, exit)
	      && !dominated_by_p (CDI_DOMINATORS, entry, exit));
}

/* Checks whether BB is contained in the region delimited by ENTRY and
   EXIT blocks.  */

static inline bool
bb_in_sese_p (basic_block bb, sese region)
{
  basic_block entry = SESE_ENTRY_BB (region);
  basic_block exit = SESE_EXIT_BB (region);

  return bb_in_region (bb, entry, exit);
}

/* Returns true when NAME is defined in REGION.  */

static inline bool
defined_in_sese_p (tree name, sese region)
{
  gimple stmt = SSA_NAME_DEF_STMT (name);
  basic_block bb = gimple_bb (stmt);

  return bb && bb_in_sese_p (bb, region);
}

/* Returns true when LOOP is in REGION.  */

static inline bool 
loop_in_sese_p (struct loop *loop, sese region)
{
  return (bb_in_sese_p (loop->header, region)
	  && bb_in_sese_p (loop->latch, region));
}

/* Returns the loop depth of LOOP in REGION.  The loop depth
   is the same as the normal loop depth, but limited by a region.

   Example:

   loop_0
     loop_1
       {
         S0 
            <- region start
         S1

         loop_2
           S2

         S3
            <- region end
       } 

    loop_0 does not exist in the region -> invalid
    loop_1 exists, but is not completely contained in the region -> depth 0
    loop_2 is completely contained -> depth 1  */

static inline unsigned int
sese_loop_depth (sese region, loop_p loop)
{
  unsigned int depth = 0;

  gcc_assert ((!loop_in_sese_p (loop, region)
	       && (SESE_ENTRY_BB (region)->loop_father == loop
	           || SESE_EXIT (region)->src->loop_father == loop))
              || loop_in_sese_p (loop, region));

  while (loop_in_sese_p (loop, region))
    {
      depth++;
      loop = loop_outer (loop);
    }

  return depth;
}

/* Splits BB to make a single entry single exit region.  */

static inline sese
split_region_for_bb (basic_block bb)
{
  edge entry, exit;

  if (single_pred_p (bb))
    entry = single_pred_edge (bb);
  else
    {
      entry = split_block_after_labels (bb);
      bb = single_succ (bb);
    }

  if (single_succ_p (bb))
    exit = single_succ_edge (bb);
  else
    {
      gimple_stmt_iterator gsi = gsi_last_bb (bb);
      gsi_prev (&gsi);
      exit = split_block (bb, gsi_stmt (gsi));
    }

  return new_sese (entry, exit);
}

/* Returns the block preceding the entry of a SESE.  */

static inline basic_block
block_before_sese (sese sese)
{
  return SESE_ENTRY (sese)->src;
}

/* Stores the INDEX in a vector for a given clast NAME.  */

typedef struct clast_name_index {
  int index;
  const char *name;
} *clast_name_index_p;

/* Returns a pointer to a new element of type clast_name_index_p built
   from NAME and INDEX.  */

static inline clast_name_index_p
new_clast_name_index (const char *name, int index)
{
  clast_name_index_p res = XNEW (struct clast_name_index);

  res->name = name;
  res->index = index;
  return res;
}

/* For a given clast NAME, returns -1 if it does not correspond to any
   parameter, or otherwise, returns the index in the PARAMS or
   SCATTERING_DIMENSIONS vector.  */

static inline int
clast_name_to_index (const char *name, htab_t index_table)
{
  struct clast_name_index tmp;
  PTR *slot;

  tmp.name = name;
  slot = htab_find_slot (index_table, &tmp, NO_INSERT);

  if (slot && *slot)
    return ((struct clast_name_index *) *slot)->index;

  return -1;
}

/* Records in INDEX_TABLE the INDEX for NAME.  */

static inline void
save_clast_name_index (htab_t index_table, const char *name, int index)
{
  struct clast_name_index tmp;
  PTR *slot;

  tmp.name = name;
  slot = htab_find_slot (index_table, &tmp, INSERT);

  if (slot)
    *slot = new_clast_name_index (name, index);
}

/* Print to stderr the element ELT.  */

static inline void
debug_clast_name_index (clast_name_index_p elt)
{
  fprintf (stderr, "(index = %d, name = %s)\n", elt->index, elt->name);
}

/* Helper function for debug_rename_map.  */

static inline int
debug_clast_name_indexes_1 (void **slot, void *s ATTRIBUTE_UNUSED)
{
  struct clast_name_index *entry = (struct clast_name_index *) *slot;
  debug_clast_name_index (entry);
  return 1;
}

/* Print to stderr all the elements of MAP.  */

static inline void
debug_clast_name_indexes (htab_t map)
{
  htab_traverse (map, debug_clast_name_indexes_1, NULL);
}

/* Computes a hash function for database element ELT.  */

static inline hashval_t
clast_name_index_elt_info (const void *elt)
{
  return htab_hash_pointer (((const struct clast_name_index *) elt)->name);
}

/* Compares database elements E1 and E2.  */

static inline int
eq_clast_name_indexes (const void *e1, const void *e2)
{
  const struct clast_name_index *elt1 = (const struct clast_name_index *) e1;
  const struct clast_name_index *elt2 = (const struct clast_name_index *) e2;

  return (elt1->name == elt2->name);
}



/* A single entry single exit specialized for conditions.  */

typedef struct ifsese_s {
  sese region;
  sese true_region;
  sese false_region;
} *ifsese;

extern void if_region_set_false_region (ifsese, sese);
extern ifsese create_if_region_on_edge (edge, tree);
extern ifsese move_sese_in_condition (sese);
extern edge get_true_edge_from_guard_bb (basic_block);
extern edge get_false_edge_from_guard_bb (basic_block);

static inline edge
if_region_entry (ifsese if_region)
{
  return SESE_ENTRY (if_region->region);
}

static inline edge
if_region_exit (ifsese if_region)
{
  return SESE_EXIT (if_region->region);
}

static inline basic_block
if_region_get_condition_block (ifsese if_region)
{
  return if_region_entry (if_region)->dest;
}

/* Structure containing the mapping between the old names and the new
   names used after block copy in the new loop context.  */
typedef struct rename_map_elt_s
{
  tree old_name, expr;
} *rename_map_elt;

DEF_VEC_P(rename_map_elt);
DEF_VEC_ALLOC_P (rename_map_elt, heap);

extern void debug_rename_map (htab_t);
extern hashval_t rename_map_elt_info (const void *);
extern int eq_rename_map_elts (const void *, const void *);
extern void set_rename (htab_t, tree, tree);

/* Constructs a new SCEV_INFO_STR structure for VAR and INSTANTIATED_BELOW.  */

static inline rename_map_elt
new_rename_map_elt (tree old_name, tree expr)
{
  rename_map_elt res;
  
  res = XNEW (struct rename_map_elt_s);
  res->old_name = old_name;
  res->expr = expr;

  return res;
}

/* Structure containing the mapping between the CLooG's induction
   variable and the type of the old induction variable.  */
typedef struct ivtype_map_elt_s
{
  tree type;
  const char *cloog_iv;
} *ivtype_map_elt;

extern void debug_ivtype_map (htab_t);
extern hashval_t ivtype_map_elt_info (const void *);
extern int eq_ivtype_map_elts (const void *, const void *);

/* Constructs a new SCEV_INFO_STR structure for VAR and INSTANTIATED_BELOW.  */

static inline ivtype_map_elt
new_ivtype_map_elt (const char *cloog_iv, tree type)
{
  ivtype_map_elt res;
  
  res = XNEW (struct ivtype_map_elt_s);
  res->cloog_iv = cloog_iv;
  res->type = type;

  return res;
}

/* Free and compute again all the dominators information.  */

static inline void
recompute_all_dominators (void)
{
  mark_irreducible_loops ();
  free_dominance_info (CDI_DOMINATORS);
  free_dominance_info (CDI_POST_DOMINATORS);
  calculate_dominance_info (CDI_DOMINATORS);
  calculate_dominance_info (CDI_POST_DOMINATORS);
}

typedef struct gimple_bb
{
  basic_block bb;

  /* Lists containing the restrictions of the conditional statements
     dominating this bb.  This bb can only be executed, if all conditions
     are true.
 
     Example:
 
     for (i = 0; i <= 20; i++)
     {
       A
 
       if (2i <= 8)
         B
     }
 
     So for B there is an additional condition (2i <= 8).
 
     List of COND_EXPR and SWITCH_EXPR.  A COND_EXPR is true only if the
     corresponding element in CONDITION_CASES is not NULL_TREE.  For a
     SWITCH_EXPR the corresponding element in CONDITION_CASES is a
     CASE_LABEL_EXPR.  */
  VEC (gimple, heap) *conditions;
  VEC (gimple, heap) *condition_cases;
  VEC (data_reference_p, heap) *data_refs;
  htab_t cloog_iv_types;
} *gimple_bb_p;

#define GBB_BB(GBB) GBB->bb
#define GBB_DATA_REFS(GBB) GBB->data_refs
#define GBB_CONDITIONS(GBB) GBB->conditions
#define GBB_CONDITION_CASES(GBB) GBB->condition_cases
#define GBB_CLOOG_IV_TYPES(GBB) GBB->cloog_iv_types

/* Return the innermost loop that contains the basic block GBB.  */

static inline struct loop *
gbb_loop (struct gimple_bb *gbb)
{
  return GBB_BB (gbb)->loop_father;
}

/* Returns the gimple loop, that corresponds to the loop_iterator_INDEX.  
   If there is no corresponding gimple loop, we return NULL.  */

static inline loop_p
gbb_loop_at_index (gimple_bb_p gbb, sese region, int index)
{
  loop_p loop = gbb_loop (gbb);
  int depth = sese_loop_depth (region, loop);

  while (--depth > index)
    loop = loop_outer (loop);

  gcc_assert (sese_contains_loop (region, loop));

  return loop;
}

/* The number of common loops in REGION for GBB1 and GBB2.  */

static inline int
nb_common_loops (sese region, gimple_bb_p gbb1, gimple_bb_p gbb2)
{
  loop_p l1 = gbb_loop (gbb1);
  loop_p l2 = gbb_loop (gbb2);
  loop_p common = find_common_loop (l1, l2);
  
  return sese_loop_depth (region, common);
}

extern void print_gimple_bb (FILE *, gimple_bb_p, int, int);
extern void debug_gbb (gimple_bb_p, int);

#endif
