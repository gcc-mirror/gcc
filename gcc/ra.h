/* Define per-register tables for data flow info and register allocation.
   Copyright (C) 2007 Free Software Foundation, Inc.

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

#ifndef GCC_RA_H
#define GCC_RA_H

#include "regs.h"

struct allocno
{
  int reg;
  /* Gives the number of consecutive hard registers needed by that
     pseudo reg.  */
  int size;

  /* Number of calls crossed by each allocno.  */
  int calls_crossed;

  /* Estimated frequency of crossing call by each allocno.  */
  int freq_calls_crossed;

  /* Number of calls that might throw crossed by each allocno.  */
  int throwing_calls_crossed;

  /* Number of refs to each allocno.  */
  int n_refs;

  /* Frequency of uses of each allocno.  */
  int freq;

  /* Guess at live length of each allocno.
     This is actually the max of the live lengths of the regs.  */
  int live_length;

  /* Set of hard regs conflicting with allocno N.  */

  HARD_REG_SET hard_reg_conflicts;

  /* Set of hard regs preferred by allocno N.
     This is used to make allocnos go into regs that are copied to or from them,
     when possible, to reduce register shuffling.  */

  HARD_REG_SET hard_reg_preferences;

  /* Similar, but just counts register preferences made in simple copy
     operations, rather than arithmetic.  These are given priority because
     we can always eliminate an insn by using these, but using a register
     in the above list won't always eliminate an insn.  */

  HARD_REG_SET hard_reg_copy_preferences;

  /* Similar to hard_reg_preferences, but includes bits for subsequent
     registers when an allocno is multi-word.  The above variable is used for
     allocation while this is used to build reg_someone_prefers, below.  */

  HARD_REG_SET hard_reg_full_preferences;

  /* Set of hard registers that some later allocno has a preference for.  */

  HARD_REG_SET regs_someone_prefers;

#ifdef EH_RETURN_DATA_REGNO
  /* Set to true if allocno can't be allocated in an eh register.  */
  unsigned int no_eh_reg:1;
#endif

#ifdef STACK_REGS
  /* Set to true if allocno can't be allocated in the stack register.  */
  unsigned int no_stack_reg:1;
#endif
};
extern struct allocno *allocno;

/* In ra-conflict.c  */

/* Number of pseudo-registers which are candidates for allocation.  */

extern int max_allocno;

/* max_allocno by max_allocno compressed triangular bit matrix,
   recording whether two allocnos conflict (can't go in the same
   hardware register).  */

extern HOST_WIDEST_FAST_INT *conflicts;

/* Indexed by (pseudo) reg number, gives the allocno, or -1
   for pseudo registers which are not to be allocated.  */

extern int *reg_allocno;

/* Precalculated partial bit number in the compressed triangular bit matrix.
   For two allocnos, the final bit number is: partial_bitnum[LOW] + HIGH.  */

extern HOST_WIDE_INT *partial_bitnum;

/* Size in bits of the compressed triangular bit matrix.  */

extern HOST_WIDE_INT max_bitnum;

/* The pool to allocate the adjacency list elements from.  */

extern alloc_pool adjacency_pool;

/* The maximum number of neighbors stored in the neighbors vector before
   we have to chain in another vector.  */

#define ADJACENCY_VEC_LENGTH 30

/* Conflict graph adjacency list.  */

typedef struct adjacency_list_d
{
  int neighbors[ADJACENCY_VEC_LENGTH];
  unsigned int index;
  struct adjacency_list_d *next;
} adjacency_t;

extern adjacency_t **adjacency;

/* Add NEIGHBOR to ALLOC_NO's adjacency list.  It is assumed the caller
   has already determined that NEIGHBOR is not already neighbor by
   checking the conflict bit matrix.  */

static inline void
add_neighbor (int alloc_no, int neighbor)
{
  adjacency_t *adjlist = adjacency[alloc_no];

  if (adjlist == NULL || adjlist->index == ADJACENCY_VEC_LENGTH)
    {
      adjacency_t *new = pool_alloc (adjacency_pool);
      new->index = 0;
      new->next = adjlist;
      adjlist = new;
      adjacency[alloc_no] = adjlist;
    }

  adjlist->neighbors[adjlist->index++] = neighbor;
}

/* Iterator for adjacency lists.  */

typedef struct adjacency_iterator_d
{
  adjacency_t *vec;
  unsigned int idx;
} adjacency_iter;

/* Initialize a single adjacency list iterator.  */

static inline int
adjacency_iter_init (adjacency_iter *ai, int allocno1)
{
  ai->vec = adjacency[allocno1];
  ai->idx = 0;
  return ai->vec != NULL;
}

/* Test whether we have visited all of the neighbors.  */

static inline int
adjacency_iter_done (adjacency_iter *ai)
{
  return ai->idx > ai->vec->index;
}

/* Advance to the next neighbor in AI.  */

static inline int
adjacency_iter_next (adjacency_iter *ai)
{
  unsigned int idx = ai->idx;
  int neighbor = ai->vec->neighbors[idx++];
  if (idx >= ai->vec->index && ai->vec->next != NULL)
    {
      ai->vec = ai->vec->next;
      ai->idx = 0;
    }
  else
    ai->idx = idx;
  return neighbor;
}

/* Return the one basic block regno is used in.  If regno is used
   in more than one basic block or if it is unknown which block it
   is used in, return 0.  */

static inline int
regno_basic_block (int regno)
{
  int block = REG_BASIC_BLOCK (regno);
  if (block < 0)
    block = 0;
  return block;
}

extern void global_conflicts (void);

/* In global.c  */

/* Macro to visit all of IN_ALLOCNO's neighbors.  Neighbors are
   returned in OUT_ALLOCNO for each iteration of the loop.  */

#define FOR_EACH_CONFLICT(IN_ALLOCNO, OUT_ALLOCNO, ITER)		\
  if (!adjacency || !adjacency_iter_init (&(ITER), (IN_ALLOCNO)))	\
    ;									\
  else									\
    for ((OUT_ALLOCNO) = adjacency_iter_next (&(ITER));			\
	 !adjacency_iter_done (&(ITER));				\
	 (OUT_ALLOCNO) = adjacency_iter_next (&(ITER)))

extern void ra_init_live_subregs (bool, sbitmap *, int *, int, rtx);
extern bool conflict_p (int, int);

#endif /* GCC_RA_H */
