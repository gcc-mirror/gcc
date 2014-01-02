/* Single entry single exit control flow regions.
   Copyright (C) 2008-2014 Free Software Foundation, Inc.
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
  vec<tree> params;

  /* Loops completely contained in the SCOP.  */
  bitmap loops;
  vec<loop_p> loop_nest;

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
#define SESE_LOOPS(S) (S->loops)
#define SESE_LOOP_NEST(S) (S->loop_nest)
#define SESE_ADD_PARAMS(S) (S->add_params)

extern sese new_sese (edge, edge);
extern void free_sese (sese);
extern void sese_insert_phis_for_liveouts (sese, basic_block, edge, edge);
extern void build_sese_loop_nests (sese);
extern edge copy_bb_and_scalar_dependences (basic_block, sese, edge,
					    vec<tree> , bool *);
extern struct loop *outermost_loop_in_sese (sese, basic_block);
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
  return SESE_PARAMS (region).length ();
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

/* Returns true when STMT is defined in REGION.  */

static inline bool
stmt_in_sese_p (gimple stmt, sese region)
{
  basic_block bb = gimple_bb (stmt);
  return bb && bb_in_sese_p (bb, region);
}

/* Returns true when NAME is defined in REGION.  */

static inline bool
defined_in_sese_p (tree name, sese region)
{
  gimple stmt = SSA_NAME_DEF_STMT (name);
  return stmt_in_sese_p (stmt, region);
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



/* A single entry single exit specialized for conditions.  */

typedef struct ifsese_s {
  sese region;
  sese true_region;
  sese false_region;
} *ifsese;

extern void if_region_set_false_region (ifsese, sese);
extern ifsese move_sese_in_condition (sese);
extern edge get_true_edge_from_guard_bb (basic_block);
extern edge get_false_edge_from_guard_bb (basic_block);
extern void set_ifsese_condition (ifsese, tree);

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


extern hashval_t rename_map_elt_info (const void *);
extern int eq_rename_map_elts (const void *, const void *);

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

/* Free and compute again all the dominators information.  */

static inline void
recompute_all_dominators (void)
{
  mark_irreducible_loops ();
  free_dominance_info (CDI_DOMINATORS);
  calculate_dominance_info (CDI_DOMINATORS);
}

typedef struct gimple_bb
{
  basic_block bb;
  struct poly_bb *pbb;

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
  vec<gimple> conditions;
  vec<gimple> condition_cases;
  vec<data_reference_p> data_refs;
} *gimple_bb_p;

#define GBB_BB(GBB) (GBB)->bb
#define GBB_PBB(GBB) (GBB)->pbb
#define GBB_DATA_REFS(GBB) (GBB)->data_refs
#define GBB_CONDITIONS(GBB) (GBB)->conditions
#define GBB_CONDITION_CASES(GBB) (GBB)->condition_cases

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

/* Return true when DEF can be analyzed in REGION by the scalar
   evolution analyzer.  */

static inline bool
scev_analyzable_p (tree def, sese region)
{
  loop_p loop;
  tree scev;
  tree type = TREE_TYPE (def);

  /* When Graphite generates code for a scev, the code generator
     expresses the scev in function of a single induction variable.
     This is unsafe for floating point computations, as it may replace
     a floating point sum reduction with a multiplication.  The
     following test returns false for non integer types to avoid such
     problems.  */
  if (!INTEGRAL_TYPE_P (type)
      && !POINTER_TYPE_P (type))
    return false;

  loop = loop_containing_stmt (SSA_NAME_DEF_STMT (def));
  scev = scalar_evolution_in_region (region, loop, def);

  return !chrec_contains_undetermined (scev)
    && (TREE_CODE (scev) != SSA_NAME
	|| !defined_in_sese_p (scev, region))
    && (tree_does_not_contain_chrecs (scev)
	|| evolution_function_is_affine_p (scev));
}

#endif
