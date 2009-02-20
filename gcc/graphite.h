/* Gimple Represented as Polyhedra.
   Copyright (C) 2006, 2007, 2008, 2009  Free Software Foundation, Inc.
   Contributed by Sebastian Pop <sebastian.pop@inria.fr>.

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

#ifndef GCC_GRAPHITE_H
#define GCC_GRAPHITE_H

#include "tree-data-ref.h"

int ref_nb_loops (data_reference_p);

typedef struct graphite_bb *graphite_bb_p;
DEF_VEC_P(graphite_bb_p);
DEF_VEC_ALLOC_P (graphite_bb_p, heap);

DEF_VEC_P(scop_p);
DEF_VEC_ALLOC_P (scop_p, heap);

static inline int scop_nb_loops (scop_p scop);
static inline unsigned scop_nb_params (scop_p scop);
static inline bool scop_contains_loop (scop_p scop, struct loop *loop);

typedef struct graphite_bb
{
  basic_block bb;
  scop_p scop;

  /* The static schedule contains the textual order for every loop layer.
    
     Example:

     S0
     for (i ...)
       {
         S1
         for (j ...)
           {
             S2
             S3
           }
         S4
       }
     S5
     for (k ...)
       {
         S6
         S7
         for (l ...)
           {
             S8
           }
         S9
       }
     S10

     Schedules:
  
        | Depth       
     BB | 0  1  2 
     ------------
     S0 | 0
     S1 | 1, 0
     S2 | 1, 1, 0
     S3 | 1, 1, 1
     S4 | 1, 2
     S5 | 2
     S6 | 3, 0
     S7 | 3, 1
     S8 | 3, 2, 0
     S9 | 3, 3
     S10| 4

   Normalization rules:
     - One SCoP can never contain two bbs with the same schedule timestamp.
     - All bbs at the same loop depth have a consecutive ordering (no gaps). */
  lambda_vector static_schedule;

  /* The iteration domain of this bb. It contains this columns:
     - In/Eq: If this line is a equation or inequation.
     - For every loop iterator one column.
     - One column for every parameter in this SCoP.
     - The constant column to add integers to the (in)equations.

     Example:

     for (i = a - 7*b + 8; i <= 3*a + 13*b + 20; i++)
       for (j = 2; j <= 2*i + 5; j++)
         for (k = 0; k <= 5; k++)
           S (i,j,k)

     Loop iterators: i, j, k 
     Parameters: a, b
      
     (I)eq   i   j   k   a   b   1
  
     1       1   0   0  -1   7   -8    #  i >=  a -  7b +  8
     1      -1   0   0   3   13  20    #  i <= 3a + 13b + 20
     1       0   1   0   0   0   -2    #  j >= 2
     1       2  -1   0   0   0    5    #  j <= 2i + 5
     1       0   0   1   0   0    0    #  k >= 0 
     1       0   0  -1   0   0    5    #  k <= 5

     The number of loop iterators may change and is not connected to the
     number of loops, that surrounded this bb in the gimple code. */
   CloogMatrix *domain;

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
 
     TODO: Add these restrictions to the domain matrix.
      
     List of COND_EXPR and SWITCH_EXPR.  A COND_EXPR is true only if the
     corresponding element in CONDITION_CASES is not NULL_TREE.  For a
     SWITCH_EXPR the corresponding element in CONDITION_CASES is a
     CASE_LABEL_EXPR.  */
  VEC (gimple, heap) *conditions;
  VEC (gimple, heap) *condition_cases;

  /* LOOPS contains for every column in the graphite domain the corresponding
     gimple loop. If there exists no corresponding gimple loop LOOPS contains
     NULL. 
  
     Example:

     Original code:

     for (i = 0; i <= 20; i++) 
       for (j = 5; j <= 10; j++)
         A

     Original domain:

     (I)eq  i  j  1
     1      1  0  0   # i >= 0
     1     -1  0  20  # i <= 20
     1      0  1  0   # j >= 0
     1      0 -1  10  # j <= 10

     Original loops vector:
     0         1 
     Loop i    Loop j

     After some changes (Exchange i and j, strip-mine i):
     
     Domain:

     (I)eq  j  ii i  k  1
     1      0  0  1  0  0   # i >= 0
     1      0  0 -1  0  20  # i <= 20
     1      1  0  0  0  0   # j >= 0
     1     -1  0  0  0  10  # j <= 10
     1      0 -1  1  0  0   # ii <= i
     1      0  1 -1  0  1   # ii + 1 >= i 
     1      0 -1  0  2  0   # ii <= 2k
     1      0  1  0 -2  0   # ii >= 2k 

     Iterator vector:
     0        1        2         3
     Loop j   NULL     Loop i    NULL
    
     Means the original loop i is now at column two of the domain and
     loop j in the original loop nest is now at column 0.  Column 1 and
     3 are emtpy.  */
  VEC (loop_p, heap) *loops;

  lambda_vector compressed_alpha_matrix;
  CloogMatrix *dynamic_schedule;
  VEC (data_reference_p, heap) *data_refs;
  htab_t cloog_iv_types;
} *gbb_p;

#define GBB_BB(GBB) GBB->bb
#define GBB_SCOP(GBB) GBB->scop
#define GBB_STATIC_SCHEDULE(GBB) GBB->static_schedule
#define GBB_DATA_REFS(GBB) GBB->data_refs
#define GBB_ALPHA(GBB) GBB->compressed_alpha_matrix
#define GBB_DYNAMIC_SCHEDULE(GBB) GBB->dynamic_schedule
#define GBB_DOMAIN(GBB) GBB->domain
#define GBB_CONDITIONS(GBB) GBB->conditions
#define GBB_CONDITION_CASES(GBB) GBB->condition_cases
#define GBB_LOOPS(GBB) GBB->loops
#define GBB_CLOOG_IV_TYPES(GBB) GBB->cloog_iv_types

/* Return the loop that contains the basic block GBB.  */

static inline struct loop *
gbb_loop (struct graphite_bb *gbb)
{
  return GBB_BB (gbb)->loop_father;
}

int nb_loops_around_gb (graphite_bb_p);

/* Calculate the number of loops around GB in the current SCOP.  Only
   works if GBB_DOMAIN is built.  */

static inline int
gbb_nb_loops (const struct graphite_bb *gb)
{
  scop_p scop = GBB_SCOP (gb);

  if (GBB_DOMAIN (gb) == NULL)
    return 0;
  
  return GBB_DOMAIN (gb)->NbColumns - scop_nb_params (scop) - 2;
}

/* Returns the gimple loop, that corresponds to the loop_iterator_INDEX.  
   If there is no corresponding gimple loop, we return NULL.  */

static inline loop_p
gbb_loop_at_index (graphite_bb_p gb, int index)
{
  return VEC_index (loop_p, GBB_LOOPS (gb), index);
}

/* Returns the index of LOOP in the loop nest around GB.  */

static inline int
gbb_loop_index (graphite_bb_p gb, loop_p loop)
{
  int i;
  loop_p l;

  for (i = 0; VEC_iterate (loop_p, GBB_LOOPS (gb), i, l); i++)
    if (loop == l)
      return i;

  gcc_unreachable();
}

struct loop_to_cloog_loop_str
{
  unsigned int loop_num;
  unsigned int loop_position; /* The column that represents this loop.  */
  CloogLoop *cloog_loop;
};

typedef struct name_tree
{
  tree t;
  const char *name;
  struct loop *loop;
} *name_tree;

DEF_VEC_P(name_tree);
DEF_VEC_ALLOC_P (name_tree, heap);

/* A Single Entry, Single Exit region is a part of the CFG delimited
   by two edges.  */
typedef struct sese
{
  /* Single ENTRY and single EXIT from the SESE region.  */
  edge entry, exit;

  /* REGION_BASIC_BLOCKS contains the set of all the basic blocks
     belonging to the SESE region.  */
  struct pointer_set_t *region_basic_blocks;

  /* An SSA_NAME version is flagged in the LIVEOUT bitmap if the
     SSA_NAME is defined inside and used outside the SESE region.  */
  bitmap liveout;

  /* The overall number of SSA_NAME versions used to index LIVEIN.  */
  int num_ver;

  /* For each SSA_NAME version VER in LIVEOUT, LIVEIN[VER] contains
     the set of basic blocks indices that contain a use of VER.  */
  bitmap *livein;
} *sese;

#define SESE_ENTRY(S) (S->entry)
#define SESE_EXIT(S) (S->exit)
#define SESE_REGION_BBS(S) (S->region_basic_blocks)
#define SESE_LIVEOUT(S) (S->liveout)
#define SESE_LIVEIN(S) (S->livein)
#define SESE_LIVEIN_VER(S, I) (S->livein[I])
#define SESE_NUM_VER(S) (S->num_ver)

extern sese new_sese (edge, edge);
extern void free_sese (sese);
extern void sese_build_livein_liveouts (sese);

/* A SCOP is a Static Control Part of the program, simple enough to be
   represented in polyhedral form.  */
struct scop
{
  /* A SCOP is defined as a SESE region.  */
  sese region;

  /* All the basic blocks in this scop that contain memory references
     and that will be represented as statements in the polyhedral
     representation.  */
  VEC (graphite_bb_p, heap) *bbs;

  lambda_vector static_schedule;

  /* Parameters used within the SCOP.  */
  VEC (name_tree, heap) *params;

  /* A collection of old induction variables*/ 
  VEC (name_tree, heap) *old_ivs;

  /* Loops completely contained in the SCOP.  */
  bitmap loops;
  VEC (loop_p, heap) *loop_nest;

  /* ???  It looks like a global mapping loop_id -> cloog_loop would work.  */
  htab_t loop2cloog_loop;

  /* Cloog representation of this scop.  */
  CloogProgram *program;

  /* Are we allowed to add more params?  This is for debugging purpose.  We
     can only add new params before generating the bb domains, otherwise they
     become invalid.  */
  bool add_params;

  /* LIVEOUT_RENAMES registers the rename mapping that has to be
     applied after code generation.  */
  htab_t liveout_renames;
};

#define SCOP_BBS(S) S->bbs
#define SCOP_REGION(S) S->region
/* SCOP_ENTRY bb dominates all the bbs of the scop.  SCOP_EXIT bb
   post-dominates all the bbs of the scop.  SCOP_EXIT potentially
   contains non affine data accesses, side effect statements or
   difficult constructs, and thus is not considered part of the scop,
   but just a boundary.  SCOP_ENTRY is considered part of the scop.  */
#define SCOP_ENTRY(S) (SESE_ENTRY (SCOP_REGION (S))->dest)
#define SCOP_EXIT(S) (SESE_EXIT (SCOP_REGION (S))->dest)
#define SCOP_REGION_BBS(S) (SESE_REGION_BBS (SCOP_REGION (S)))
#define SCOP_STATIC_SCHEDULE(S) S->static_schedule
#define SCOP_LOOPS(S) S->loops
#define SCOP_LOOP_NEST(S) S->loop_nest
#define SCOP_ADD_PARAMS(S) S->add_params
#define SCOP_PARAMS(S) S->params
#define SCOP_OLDIVS(S) S->old_ivs
#define SCOP_PROG(S) S->program
#define SCOP_LOOP2CLOOG_LOOP(S) S->loop2cloog_loop
#define SCOP_LOOPS_MAPPING(S) S->loops_mapping
#define SCOP_LIVEOUT_RENAMES(S) S->liveout_renames

extern void debug_scop (scop_p, int);
extern void debug_scops (int);
extern void print_graphite_bb (FILE *, graphite_bb_p, int, int);
extern void debug_gbb (graphite_bb_p, int);
extern void dot_scop (scop_p);
extern void dot_all_scops (void);
extern void debug_clast_stmt (struct clast_stmt *);
extern void debug_rename_map (htab_t);
extern void debug_ivtype_map (htab_t);
extern void debug_loop_vec (graphite_bb_p);
extern void debug_oldivs (scop_p);

/* Describes the type of an iv stack entry.  */
typedef enum {
  iv_stack_entry_unknown = 0,
  iv_stack_entry_iv,
  iv_stack_entry_const
} iv_stack_entry_kind;

/* Data contained in an iv stack entry.  */
typedef union iv_stack_entry_data_union
{
  name_tree iv;
  tree constant;
} iv_stack_entry_data;

/* Datatype for loop iv stack entry.  */
typedef struct iv_stack_entry_struct
{
  iv_stack_entry_kind kind;
  iv_stack_entry_data data;
} iv_stack_entry;

typedef iv_stack_entry *iv_stack_entry_p;

DEF_VEC_P(iv_stack_entry_p);
DEF_VEC_ALLOC_P(iv_stack_entry_p,heap);

typedef VEC(iv_stack_entry_p, heap) **loop_iv_stack;
extern void debug_loop_iv_stack (loop_iv_stack);

/* Return the old induction variable of the LOOP that is in normal
   form in SCOP.  */

static inline tree
oldiv_for_loop (scop_p scop, loop_p loop)
{
  int i;
  name_tree iv;

  if (!loop)
    return NULL_TREE;

  for (i = 0; VEC_iterate (name_tree, SCOP_OLDIVS (scop), i, iv); i++)
    if (iv->loop == loop)
      return iv->t;

  return NULL_TREE;
}

/* Return the number of gimple loops contained in SCOP.  */

static inline int
scop_nb_loops (scop_p scop)
{
  return VEC_length (loop_p, SCOP_LOOP_NEST (scop));
}

/* Returns the number of parameters for SCOP.  */

static inline unsigned
scop_nb_params (scop_p scop)
{
  return VEC_length (name_tree, SCOP_PARAMS (scop));
}

/* Return the dimension of the domains for SCOP.  */

static inline int
scop_dim_domain (scop_p scop)
{
  return scop_nb_loops (scop) + scop_nb_params (scop) + 1;
}

/* Return the dimension of the domains for GB.  */

static inline int
gbb_dim_domain (graphite_bb_p gb)
{
  return scop_dim_domain (GBB_SCOP (gb));
}

/* Returns the dimensionality of a loop iteration domain for a given
   loop, identified by LOOP_NUM, with respect to SCOP.  */

static inline int
loop_domain_dim (unsigned int loop_num, scop_p scop)
{
  struct loop_to_cloog_loop_str tmp, *slot; 
  htab_t tab = SCOP_LOOP2CLOOG_LOOP (scop);

  tmp.loop_num = loop_num;
  slot = (struct loop_to_cloog_loop_str *) htab_find (tab, &tmp);

  /* The loop containing the entry of the scop is not always part of
     the SCoP, and it is not registered in SCOP_LOOP2CLOOG_LOOP.  */
  if (!slot)
    return scop_nb_params (scop) + 2;

  return cloog_domain_dim (cloog_loop_domain (slot->cloog_loop)) + 2;
}

/* Returns the dimensionality of a loop iteration vector in a loop
   iteration domain for a given loop (identified by LOOP_NUM) with
   respect to SCOP.  */

static inline int
loop_iteration_vector_dim (unsigned int loop_num, scop_p scop)
{
  return loop_domain_dim (loop_num, scop) - 2 - scop_nb_params (scop);
}

/* Checks, if SCOP contains LOOP.  */

static inline bool
scop_contains_loop (scop_p scop, struct loop *loop)
{
  return bitmap_bit_p (SCOP_LOOPS (scop), loop->num);
}

/* Returns the index of LOOP in the domain matrix for the SCOP.  */

static inline int
scop_loop_index (scop_p scop, struct loop *loop)
{
  unsigned i;
  struct loop *l;

  gcc_assert (scop_contains_loop (scop, loop));

  for (i = 0; VEC_iterate (loop_p, SCOP_LOOP_NEST (scop), i, l); i++)
    if (l == loop)
      return i;

  gcc_unreachable();
}

/* Return the index of innermost loop that contains the basic block
   GBB.  */

static inline int
gbb_inner_most_loop_index (scop_p scop, graphite_bb_p gb)
{
  return scop_loop_index(scop, gbb_loop (gb));
}

/* Return the outermost loop that contains the loop LOOP.  The outer
   loops are searched until a sibling for the outer loop is found.  */

static struct loop *
outer_most_loop_1 (scop_p scop, struct loop* loop, struct loop* current_outer)
{
  return (!scop_contains_loop (scop, loop)) ? current_outer :
    (loop->next != NULL) ? loop :
    outer_most_loop_1 (scop, loop_outer (loop), loop);
}

/* Return the outermost loop that contains the loop LOOP.  */

static struct loop *
outer_most_loop (scop_p scop, struct loop *loop)
{
  return outer_most_loop_1 (scop, loop, NULL);
}

/* Return the index of the outermost loop that contains the basic
   block BB.  */

static inline int
gbb_outer_most_loop_index (scop_p scop, graphite_bb_p gb)
{
  return scop_loop_index (scop, outer_most_loop (scop, gbb_loop (gb)));
}

/* Return the loop depth of LOOP in SCOP.  */

static inline unsigned int
scop_gimple_loop_depth (scop_p scop, loop_p loop)
{
  unsigned int depth = 0;

  loop = loop_outer (loop);

  while (scop_contains_loop (scop, loop))
    {
      depth++;
      loop = loop_outer (loop);
    }

  return depth;
}

#endif  /* GCC_GRAPHITE_H  */
