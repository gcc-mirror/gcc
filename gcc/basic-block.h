/* Define control and data flow tables, and regsets.
   Copyright (C) 1987, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#ifndef GCC_BASIC_BLOCK_H
#define GCC_BASIC_BLOCK_H

#include "bitmap.h"
#include "sbitmap.h"
#include "varray.h"
#include "partition.h"
#include "hard-reg-set.h"
#include "predict.h"
#include "vec.h"
#include "errors.h"

/* Head of register set linked list.  */
typedef bitmap_head regset_head;

/* A pointer to a regset_head.  */
typedef bitmap regset;

/* Allocate a register set with oballoc.  */
#define ALLOC_REG_SET(OBSTACK) BITMAP_ALLOC (OBSTACK)

/* Do any cleanup needed on a regset when it is no longer used.  */
#define FREE_REG_SET(REGSET) BITMAP_FREE (REGSET)

/* Initialize a new regset.  */
#define INIT_REG_SET(HEAD) bitmap_initialize (HEAD, &reg_obstack)

/* Clear a register set by freeing up the linked list.  */
#define CLEAR_REG_SET(HEAD) bitmap_clear (HEAD)

/* Copy a register set to another register set.  */
#define COPY_REG_SET(TO, FROM) bitmap_copy (TO, FROM)

/* Compare two register sets.  */
#define REG_SET_EQUAL_P(A, B) bitmap_equal_p (A, B)

/* `and' a register set with a second register set.  */
#define AND_REG_SET(TO, FROM) bitmap_and_into (TO, FROM)

/* `and' the complement of a register set with a register set.  */
#define AND_COMPL_REG_SET(TO, FROM) bitmap_and_compl_into (TO, FROM)

/* Inclusive or a register set with a second register set.  */
#define IOR_REG_SET(TO, FROM) bitmap_ior_into (TO, FROM)

/* Exclusive or a register set with a second register set.  */
#define XOR_REG_SET(TO, FROM) bitmap_xor_into (TO, FROM)

/* Or into TO the register set FROM1 `and'ed with the complement of FROM2.  */
#define IOR_AND_COMPL_REG_SET(TO, FROM1, FROM2) \
  bitmap_ior_and_compl_into (TO, FROM1, FROM2)

/* Clear a single register in a register set.  */
#define CLEAR_REGNO_REG_SET(HEAD, REG) bitmap_clear_bit (HEAD, REG)

/* Set a single register in a register set.  */
#define SET_REGNO_REG_SET(HEAD, REG) bitmap_set_bit (HEAD, REG)

/* Return true if a register is set in a register set.  */
#define REGNO_REG_SET_P(TO, REG) bitmap_bit_p (TO, REG)

/* Copy the hard registers in a register set to the hard register set.  */
extern void reg_set_to_hard_reg_set (HARD_REG_SET *, bitmap);
#define REG_SET_TO_HARD_REG_SET(TO, FROM)				\
do {									\
  CLEAR_HARD_REG_SET (TO);						\
  reg_set_to_hard_reg_set (&TO, FROM);					\
} while (0)

typedef bitmap_iterator reg_set_iterator;

/* Loop over all registers in REGSET, starting with MIN, setting REGNUM to the
   register number and executing CODE for all registers that are set.  */
#define EXECUTE_IF_SET_IN_REG_SET(REGSET, MIN, REGNUM, RSI)	\
  EXECUTE_IF_SET_IN_BITMAP (REGSET, MIN, REGNUM, RSI)

/* Loop over all registers in REGSET1 and REGSET2, starting with MIN, setting
   REGNUM to the register number and executing CODE for all registers that are
   set in the first regset and not set in the second.  */
#define EXECUTE_IF_AND_COMPL_IN_REG_SET(REGSET1, REGSET2, MIN, REGNUM, RSI) \
  EXECUTE_IF_AND_COMPL_IN_BITMAP (REGSET1, REGSET2, MIN, REGNUM, RSI)

/* Loop over all registers in REGSET1 and REGSET2, starting with MIN, setting
   REGNUM to the register number and executing CODE for all registers that are
   set in both regsets.  */
#define EXECUTE_IF_AND_IN_REG_SET(REGSET1, REGSET2, MIN, REGNUM, RSI) \
  EXECUTE_IF_AND_IN_BITMAP (REGSET1, REGSET2, MIN, REGNUM, RSI)	\

/* Type we use to hold basic block counters.  Should be at least
   64bit.  Although a counter cannot be negative, we use a signed
   type, because erroneous negative counts can be generated when the
   flow graph is manipulated by various optimizations.  A signed type
   makes those easy to detect.  */
typedef HOST_WIDEST_INT gcov_type;

/* Control flow edge information.  */
struct edge_def GTY(())
{
  /* The two blocks at the ends of the edge.  */
  struct basic_block_def *src;
  struct basic_block_def *dest;

  /* Instructions queued on the edge.  */
  union edge_def_insns {
    rtx GTY ((tag ("0"))) r;
    tree GTY ((tag ("1"))) t;
  } GTY ((desc ("ir_type ()"))) insns;

  /* Auxiliary info specific to a pass.  */
  PTR GTY ((skip (""))) aux;

  /* Location of any goto implicit in the edge, during tree-ssa.  */
  source_locus goto_locus;

  int flags;			/* see EDGE_* below  */
  int probability;		/* biased by REG_BR_PROB_BASE */
  gcov_type count;		/* Expected number of executions calculated
				   in profile.c  */

  /* The index number corresponding to this edge in the edge vector
     dest->preds.  */
  unsigned int dest_idx;
};

typedef struct edge_def *edge;
DEF_VEC_GC_P(edge);

#define EDGE_FALLTHRU		1	/* 'Straight line' flow */
#define EDGE_ABNORMAL		2	/* Strange flow, like computed
					   label, or eh */
#define EDGE_ABNORMAL_CALL	4	/* Call with abnormal exit
					   like an exception, or sibcall */
#define EDGE_EH			8	/* Exception throw */
#define EDGE_FAKE		16	/* Not a real edge (profile.c) */
#define EDGE_DFS_BACK		32	/* A backwards edge */
#define EDGE_CAN_FALLTHRU	64	/* Candidate for straight line
					   flow.  */
#define EDGE_IRREDUCIBLE_LOOP	128	/* Part of irreducible loop.  */
#define EDGE_SIBCALL		256	/* Edge from sibcall to exit.  */
#define EDGE_LOOP_EXIT		512	/* Exit of a loop.  */
#define EDGE_TRUE_VALUE		1024	/* Edge taken when controlling
					   predicate is nonzero.  */
#define EDGE_FALSE_VALUE	2048	/* Edge taken when controlling
					   predicate is zero.  */
#define EDGE_EXECUTABLE		4096	/* Edge is executable.  Only
					   valid during SSA-CCP.  */
#define EDGE_CROSSING		8192    /* Edge crosses between hot
					   and cold sections, when we
					   do partitioning.  */
#define EDGE_ALL_FLAGS	       16383

#define EDGE_COMPLEX	(EDGE_ABNORMAL | EDGE_ABNORMAL_CALL | EDGE_EH)

/* Counter summary from the last set of coverage counts read by
   profile.c.  */
extern const struct gcov_ctr_summary *profile_info;

/* Declared in cfgloop.h.  */
struct loop;
struct loops;

/* Declared in tree-flow.h.  */
struct bb_ann_d;

/* A basic block is a sequence of instructions with only entry and
   only one exit.  If any one of the instructions are executed, they
   will all be executed, and in sequence from first to last.

   There may be COND_EXEC instructions in the basic block.  The
   COND_EXEC *instructions* will be executed -- but if the condition
   is false the conditionally executed *expressions* will of course
   not be executed.  We don't consider the conditionally executed
   expression (which might have side-effects) to be in a separate
   basic block because the program counter will always be at the same
   location after the COND_EXEC instruction, regardless of whether the
   condition is true or not.

   Basic blocks need not start with a label nor end with a jump insn.
   For example, a previous basic block may just "conditionally fall"
   into the succeeding basic block, and the last basic block need not
   end with a jump insn.  Block 0 is a descendant of the entry block.

   A basic block beginning with two labels cannot have notes between
   the labels.

   Data for jump tables are stored in jump_insns that occur in no
   basic block even though these insns can follow or precede insns in
   basic blocks.  */

/* Basic block information indexed by block number.  */
struct basic_block_def GTY((chain_next ("%h.next_bb"), chain_prev ("%h.prev_bb")))
{
  /* The first and last insns of the block.  */
  rtx head_;
  rtx end_;

  /* Pointers to the first and last trees of the block.  */
  tree stmt_list;

  /* The edges into and out of the block.  */
  VEC(edge) *preds;
  VEC(edge) *succs;

  /* The registers that are live on entry to this block.  */
  bitmap GTY ((skip (""))) global_live_at_start;

  /* The registers that are live on exit from this block.  */
  bitmap GTY ((skip (""))) global_live_at_end;

  /* Auxiliary info specific to a pass.  */
  PTR GTY ((skip (""))) aux;

  /* Innermost loop containing the block.  */
  struct loop * GTY ((skip (""))) loop_father;

  /* The dominance and postdominance information node.  */
  struct et_node * GTY ((skip (""))) dom[2];

  /* Previous and next blocks in the chain.  */
  struct basic_block_def *prev_bb;
  struct basic_block_def *next_bb;

  /* The data used by basic block copying and reordering functions.  */
  struct reorder_block_def * GTY ((skip (""))) rbi;

  /* Annotations used at the tree level.  */
  struct bb_ann_d *tree_annotations;

  /* Expected number of executions: calculated in profile.c.  */
  gcov_type count;

  /* The index of this block.  */
  int index;

  /* The loop depth of this block.  */
  int loop_depth;

  /* Expected frequency.  Normalized to be in range 0 to BB_FREQ_MAX.  */
  int frequency;

  /* Various flags.  See BB_* below.  */
  int flags;
};

typedef struct basic_block_def *basic_block;

/* Structure to hold information about the blocks during reordering and
   copying.  */

typedef struct reorder_block_def
{
  rtx header;
  rtx footer;
  basic_block next;
  basic_block original;
  /* Used by loop copying.  */
  basic_block copy;
  int duplicated;
  int copy_number;

  /* These fields are used by bb-reorder pass.  */
  int visited;
} *reorder_block_def_p;

#define BB_FREQ_MAX 10000

/* Masks for basic_block.flags.

   BB_VISITED should not be used by passes, it is used internally by
   dfs_enumerate_from.

   BB_HOT_PARTITION and BB_COLD_PARTITION should be preserved throughout
   the compilation, so they are never cleared.

   All other flags may be cleared by clear_bb_flags().  It is generally
   a bad idea to rely on any flags being up-to-date.  */

/* Set if insns in BB have are modified.  Used for updating liveness info.  */
#define BB_DIRTY		1

/* Only set on blocks that have just been created by create_bb.  */
#define BB_NEW			2

/* Set by find_unreachable_blocks.  Do not rely on this being set in any
   pass.  */
#define BB_REACHABLE		4

/* Used by dfs_enumerate_from to keep track of visited basic blocks.  */
#define BB_VISITED		8

/* Set for blocks in an irreducible loop by loop analysis.  */
#define BB_IRREDUCIBLE_LOOP	16

/* Set on blocks that may actually not be single-entry single-exit block.  */
#define BB_SUPERBLOCK		32

/* Set on basic blocks that the scheduler should not touch.  This is used
   by SMS to prevent other schedulers from messing with the loop schedule.  */
#define BB_DISABLE_SCHEDULE	64

/* Set on blocks that should be put in a hot section.  */
#define BB_HOT_PARTITION	128

/* Set on blocks that should be put in a cold section.  */
#define BB_COLD_PARTITION	256

/* Dummy flag for convenience in the hot/cold partitioning code.  */
#define BB_UNPARTITIONED	0

/* Partitions, to be used when partitioning hot and cold basic blocks into
   separate sections.  */
#define BB_PARTITION(bb) ((bb)->flags & (BB_HOT_PARTITION|BB_COLD_PARTITION))
#define BB_SET_PARTITION(bb, part) do {					\
  basic_block bb_ = (bb);						\
  bb_->flags = ((bb_->flags & ~(BB_HOT_PARTITION|BB_COLD_PARTITION))	\
		| (part));						\
} while (0)

#define BB_COPY_PARTITION(dstbb, srcbb) \
  BB_SET_PARTITION (dstbb, BB_PARTITION (srcbb))

/* Number of basic blocks in the current function.  */

extern int n_basic_blocks;

/* First free basic block number.  */

extern int last_basic_block;

/* Number of edges in the current function.  */

extern int n_edges;

/* Signalize the status of profile information in the CFG.  */
extern enum profile_status
{
  PROFILE_ABSENT,
  PROFILE_GUESSED,
  PROFILE_READ
} profile_status;

/* Index by basic block number, get basic block struct info.  */

extern GTY(()) varray_type basic_block_info;

#define BASIC_BLOCK(N)  (VARRAY_BB (basic_block_info, (N)))

/* For iterating over basic blocks.  */
#define FOR_BB_BETWEEN(BB, FROM, TO, DIR) \
  for (BB = FROM; BB != TO; BB = BB->DIR)

#define FOR_EACH_BB(BB) \
  FOR_BB_BETWEEN (BB, ENTRY_BLOCK_PTR->next_bb, EXIT_BLOCK_PTR, next_bb)

#define FOR_EACH_BB_REVERSE(BB) \
  FOR_BB_BETWEEN (BB, EXIT_BLOCK_PTR->prev_bb, ENTRY_BLOCK_PTR, prev_bb)

/* For iterating over insns in basic block.  */
#define FOR_BB_INSNS(BB, INSN)			\
  for ((INSN) = BB_HEAD (BB);			\
       (INSN) != NEXT_INSN (BB_END (BB));	\
       (INSN) = NEXT_INSN (INSN))

#define FOR_BB_INSNS_REVERSE(BB, INSN)		\
  for ((INSN) = BB_END (BB);			\
       (INSN) != PREV_INSN (BB_HEAD (BB));	\
       (INSN) = PREV_INSN (INSN))

/* Cycles through _all_ basic blocks, even the fake ones (entry and
   exit block).  */

#define FOR_ALL_BB(BB) \
  for (BB = ENTRY_BLOCK_PTR; BB; BB = BB->next_bb)

/* What registers are live at the setjmp call.  */

extern regset regs_live_at_setjmp;

/* Special labels found during CFG build.  */

extern GTY(()) rtx label_value_list;

extern bitmap_obstack reg_obstack;

/* Indexed by n, gives number of basic block that  (REG n) is used in.
   If the value is REG_BLOCK_GLOBAL (-2),
   it means (REG n) is used in more than one basic block.
   REG_BLOCK_UNKNOWN (-1) means it hasn't been seen yet so we don't know.
   This information remains valid for the rest of the compilation
   of the current function; it is used to control register allocation.  */

#define REG_BLOCK_UNKNOWN -1
#define REG_BLOCK_GLOBAL -2

#define REG_BASIC_BLOCK(N) (VARRAY_REG (reg_n_info, N)->basic_block)

/* Stuff for recording basic block info.  */

#define BB_HEAD(B)      (B)->head_
#define BB_END(B)       (B)->end_

/* Special block numbers [markers] for entry and exit.  */
#define ENTRY_BLOCK (-1)
#define EXIT_BLOCK (-2)

/* Special block number not valid for any block.  */
#define INVALID_BLOCK (-3)

/* Similarly, block pointers for the edge list.  */
extern GTY(()) basic_block ENTRY_BLOCK_PTR;
extern GTY(()) basic_block EXIT_BLOCK_PTR;

#define BLOCK_NUM(INSN)	      (BLOCK_FOR_INSN (INSN)->index + 0)
#define set_block_for_insn(INSN, BB)  (BLOCK_FOR_INSN (INSN) = BB)

extern void compute_bb_for_insn (void);
extern void free_bb_for_insn (void);
extern void update_bb_for_insn (basic_block);

extern void free_basic_block_vars (void);

extern void insert_insn_on_edge (rtx, edge);
bool safe_insert_insn_on_edge (rtx, edge);

extern void commit_edge_insertions (void);
extern void commit_edge_insertions_watch_calls (void);

extern void remove_fake_edges (void);
extern void remove_fake_exit_edges (void);
extern void add_noreturn_fake_exit_edges (void);
extern void connect_infinite_loops_to_exit (void);
extern edge unchecked_make_edge (basic_block, basic_block, int);
extern edge cached_make_edge (sbitmap *, basic_block, basic_block, int);
extern edge make_edge (basic_block, basic_block, int);
extern edge make_single_succ_edge (basic_block, basic_block, int);
extern void remove_edge (edge);
extern void redirect_edge_succ (edge, basic_block);
extern edge redirect_edge_succ_nodup (edge, basic_block);
extern void redirect_edge_pred (edge, basic_block);
extern basic_block create_basic_block_structure (rtx, rtx, rtx, basic_block);
extern void clear_bb_flags (void);
extern void flow_reverse_top_sort_order_compute (int *);
extern int flow_depth_first_order_compute (int *, int *);
extern int dfs_enumerate_from (basic_block, int,
			       bool (*)(basic_block, void *),
			       basic_block *, int, void *);
extern void compute_dominance_frontiers (bitmap *);
extern void dump_edge_info (FILE *, edge, int);
extern void brief_dump_cfg (FILE *);
extern void clear_edges (void);
extern rtx first_insn_after_basic_block_note (basic_block);

/* Structure to group all of the information to process IF-THEN and
   IF-THEN-ELSE blocks for the conditional execution support.  This
   needs to be in a public file in case the IFCVT macros call
   functions passing the ce_if_block data structure.  */

typedef struct ce_if_block
{
  basic_block test_bb;			/* First test block.  */
  basic_block then_bb;			/* THEN block.  */
  basic_block else_bb;			/* ELSE block or NULL.  */
  basic_block join_bb;			/* Join THEN/ELSE blocks.  */
  basic_block last_test_bb;		/* Last bb to hold && or || tests.  */
  int num_multiple_test_blocks;		/* # of && and || basic blocks.  */
  int num_and_and_blocks;		/* # of && blocks.  */
  int num_or_or_blocks;			/* # of || blocks.  */
  int num_multiple_test_insns;		/* # of insns in && and || blocks.  */
  int and_and_p;			/* Complex test is &&.  */
  int num_then_insns;			/* # of insns in THEN block.  */
  int num_else_insns;			/* # of insns in ELSE block.  */
  int pass;				/* Pass number.  */

#ifdef IFCVT_EXTRA_FIELDS
  IFCVT_EXTRA_FIELDS			/* Any machine dependent fields.  */
#endif

} ce_if_block_t;

/* This structure maintains an edge list vector.  */
struct edge_list
{
  int num_blocks;
  int num_edges;
  edge *index_to_edge;
};

/* This is the value which indicates no edge is present.  */
#define EDGE_INDEX_NO_EDGE	-1

/* EDGE_INDEX returns an integer index for an edge, or EDGE_INDEX_NO_EDGE
   if there is no edge between the 2 basic blocks.  */
#define EDGE_INDEX(el, pred, succ) (find_edge_index ((el), (pred), (succ)))

/* INDEX_EDGE_PRED_BB and INDEX_EDGE_SUCC_BB return a pointer to the basic
   block which is either the pred or succ end of the indexed edge.  */
#define INDEX_EDGE_PRED_BB(el, index)	((el)->index_to_edge[(index)]->src)
#define INDEX_EDGE_SUCC_BB(el, index)	((el)->index_to_edge[(index)]->dest)

/* INDEX_EDGE returns a pointer to the edge.  */
#define INDEX_EDGE(el, index)           ((el)->index_to_edge[(index)])

/* Number of edges in the compressed edge list.  */
#define NUM_EDGES(el)			((el)->num_edges)

/* BB is assumed to contain conditional jump.  Return the fallthru edge.  */
#define FALLTHRU_EDGE(bb)		(EDGE_SUCC ((bb), 0)->flags & EDGE_FALLTHRU \
					 ? EDGE_SUCC ((bb), 0) : EDGE_SUCC ((bb), 1))

/* BB is assumed to contain conditional jump.  Return the branch edge.  */
#define BRANCH_EDGE(bb)			(EDGE_SUCC ((bb), 0)->flags & EDGE_FALLTHRU \
					 ? EDGE_SUCC ((bb), 1) : EDGE_SUCC ((bb), 0))

/* Return expected execution frequency of the edge E.  */
#define EDGE_FREQUENCY(e)		(((e)->src->frequency \
					  * (e)->probability \
					  + REG_BR_PROB_BASE / 2) \
					 / REG_BR_PROB_BASE)

/* Return nonzero if edge is critical.  */
#define EDGE_CRITICAL_P(e)		(EDGE_COUNT ((e)->src->succs) >= 2 \
					 && EDGE_COUNT ((e)->dest->preds) >= 2)

#define EDGE_COUNT(ev)			VEC_length (edge, (ev))
#define EDGE_I(ev,i)			VEC_index  (edge, (ev), (i))
#define EDGE_PRED(bb,i)			VEC_index  (edge, (bb)->preds, (i))
#define EDGE_SUCC(bb,i)			VEC_index  (edge, (bb)->succs, (i))

/* Iterator object for edges.  */

typedef struct {
  unsigned index;
  VEC(edge) **container;
} edge_iterator;

static inline VEC(edge) *
ei_container (edge_iterator i)
{
  gcc_assert (i.container);
  return *i.container;
}

#define ei_start(iter) ei_start_1 (&(iter))
#define ei_last(iter) ei_last_1 (&(iter))

/* Return an iterator pointing to the start of an edge vector.  */
static inline edge_iterator
ei_start_1 (VEC(edge) **ev)
{
  edge_iterator i;

  i.index = 0;
  i.container = ev;

  return i;
}

/* Return an iterator pointing to the last element of an edge
   vector.  */
static inline edge_iterator
ei_last_1 (VEC(edge) **ev)
{
  edge_iterator i;

  i.index = EDGE_COUNT (*ev) - 1;
  i.container = ev;

  return i;
}

/* Is the iterator `i' at the end of the sequence?  */
static inline bool
ei_end_p (edge_iterator i)
{
  return (i.index == EDGE_COUNT (ei_container (i)));
}

/* Is the iterator `i' at one position before the end of the
   sequence?  */
static inline bool
ei_one_before_end_p (edge_iterator i)
{
  return (i.index + 1 == EDGE_COUNT (ei_container (i)));
}

/* Advance the iterator to the next element.  */
static inline void
ei_next (edge_iterator *i)
{
  gcc_assert (i->index < EDGE_COUNT (ei_container (*i)));
  i->index++;
}

/* Move the iterator to the previous element.  */
static inline void
ei_prev (edge_iterator *i)
{
  gcc_assert (i->index > 0);
  i->index--;
}

/* Return the edge pointed to by the iterator `i'.  */
static inline edge
ei_edge (edge_iterator i)
{
  return EDGE_I (ei_container (i), i.index);
}

/* Return an edge pointed to by the iterator.  Do it safely so that
   NULL is returned when the iterator is pointing at the end of the
   sequence.  */
static inline edge
ei_safe_edge (edge_iterator i)
{
  return !ei_end_p (i) ? ei_edge (i) : NULL;
}

/* This macro serves as a convenient way to iterate each edge in a
   vector of predecessor or successor edges.  It must not be used when
   an element might be removed during the traversal, otherwise
   elements will be missed.  Instead, use a for-loop like that shown
   in the following pseudo-code:
   
   FOR (ei = ei_start (bb->succs); (e = ei_safe_edge (ei)); )
     {
	IF (e != taken_edge)
	  remove_edge (e);
	ELSE
	  ei_next (&ei);
     }
*/

#define FOR_EACH_EDGE(EDGE,ITER,EDGE_VEC) \
  for ((EDGE) = NULL, (ITER) = ei_start ((EDGE_VEC)); \
       ((EDGE) = ei_safe_edge ((ITER))); \
       ei_next (&(ITER)))

struct edge_list * create_edge_list (void);
void free_edge_list (struct edge_list *);
void print_edge_list (FILE *, struct edge_list *);
void verify_edge_list (FILE *, struct edge_list *);
int find_edge_index (struct edge_list *, basic_block, basic_block);
edge find_edge (basic_block, basic_block);


enum update_life_extent
{
  UPDATE_LIFE_LOCAL = 0,
  UPDATE_LIFE_GLOBAL = 1,
  UPDATE_LIFE_GLOBAL_RM_NOTES = 2
};

/* Flags for life_analysis and update_life_info.  */

#define PROP_DEATH_NOTES	1	/* Create DEAD and UNUSED notes.  */
#define PROP_LOG_LINKS		2	/* Create LOG_LINKS.  */
#define PROP_REG_INFO		4	/* Update regs_ever_live et al.  */
#define PROP_KILL_DEAD_CODE	8	/* Remove dead code.  */
#define PROP_SCAN_DEAD_CODE	16	/* Scan for dead code.  */
#define PROP_ALLOW_CFG_CHANGES	32	/* Allow the CFG to be changed
					   by dead code removal.  */
#define PROP_AUTOINC		64	/* Create autoinc mem references.  */
#define PROP_EQUAL_NOTES	128	/* Take into account REG_EQUAL notes.  */
#define PROP_SCAN_DEAD_STORES	256	/* Scan for dead code.  */
#define PROP_ASM_SCAN		512	/* Internal flag used within flow.c
					   to flag analysis of asms.  */
#define PROP_FINAL		(PROP_DEATH_NOTES | PROP_LOG_LINKS  \
				 | PROP_REG_INFO | PROP_KILL_DEAD_CODE  \
				 | PROP_SCAN_DEAD_CODE | PROP_AUTOINC \
				 | PROP_ALLOW_CFG_CHANGES \
				 | PROP_SCAN_DEAD_STORES)
#define PROP_POSTRELOAD		(PROP_DEATH_NOTES  \
				 | PROP_KILL_DEAD_CODE  \
				 | PROP_SCAN_DEAD_CODE \
				 | PROP_SCAN_DEAD_STORES)

#define CLEANUP_EXPENSIVE	1	/* Do relatively expensive optimizations
					   except for edge forwarding */
#define CLEANUP_CROSSJUMP	2	/* Do crossjumping.  */
#define CLEANUP_POST_REGSTACK	4	/* We run after reg-stack and need
					   to care REG_DEAD notes.  */
#define CLEANUP_PRE_LOOP	8	/* Take care to preserve syntactic loop
					   notes.  */
#define CLEANUP_UPDATE_LIFE	16	/* Keep life information up to date.  */
#define CLEANUP_THREADING	32	/* Do jump threading.  */
#define CLEANUP_NO_INSN_DEL	64	/* Do not try to delete trivially dead
					   insns.  */
#define CLEANUP_CFGLAYOUT	128	/* Do cleanup in cfglayout mode.  */
#define CLEANUP_LOG_LINKS	256	/* Update log links.  */

extern void life_analysis (FILE *, int);
extern int update_life_info (sbitmap, enum update_life_extent, int);
extern int update_life_info_in_dirty_blocks (enum update_life_extent, int);
extern int count_or_remove_death_notes (sbitmap, int);
extern int propagate_block (basic_block, regset, regset, regset, int);

struct propagate_block_info;
extern rtx propagate_one_insn (struct propagate_block_info *, rtx);
extern struct propagate_block_info *init_propagate_block_info
 (basic_block, regset, regset, regset, int);
extern void free_propagate_block_info (struct propagate_block_info *);

/* In lcm.c */
extern struct edge_list *pre_edge_lcm (FILE *, int, sbitmap *, sbitmap *,
				       sbitmap *, sbitmap *, sbitmap **,
				       sbitmap **);
extern struct edge_list *pre_edge_rev_lcm (FILE *, int, sbitmap *,
					   sbitmap *, sbitmap *,
					   sbitmap *, sbitmap **,
					   sbitmap **);
extern void compute_available (sbitmap *, sbitmap *, sbitmap *, sbitmap *);
extern int optimize_mode_switching (FILE *);

/* In predict.c */
extern void estimate_probability (struct loops *);
extern void expected_value_to_br_prob (void);
extern bool maybe_hot_bb_p (basic_block);
extern bool probably_cold_bb_p (basic_block);
extern bool probably_never_executed_bb_p (basic_block);
extern bool tree_predicted_by_p (basic_block, enum br_predictor);
extern bool rtl_predicted_by_p (basic_block, enum br_predictor);
extern void tree_predict_edge (edge, enum br_predictor, int);
extern void rtl_predict_edge (edge, enum br_predictor, int);
extern void predict_edge_def (edge, enum br_predictor, enum prediction);
extern void guess_outgoing_edge_probabilities (basic_block);

/* In flow.c */
extern void init_flow (void);
extern void debug_bb (basic_block);
extern basic_block debug_bb_n (int);
extern void dump_regset (regset, FILE *);
extern void debug_regset (regset);
extern void allocate_reg_life_data (void);
extern void expunge_block (basic_block);
extern void link_block (basic_block, basic_block);
extern void unlink_block (basic_block);
extern void compact_blocks (void);
extern basic_block alloc_block (void);
extern void find_unreachable_blocks (void);
extern int delete_noop_moves (void);
extern basic_block force_nonfallthru (edge);
extern rtx block_label (basic_block);
extern bool forwarder_block_p (basic_block);
extern bool purge_all_dead_edges (int);
extern bool purge_dead_edges (basic_block);
extern void find_sub_basic_blocks (basic_block);
extern void find_many_sub_basic_blocks (sbitmap);
extern void rtl_make_eh_edge (sbitmap *, basic_block, rtx);
extern bool can_fallthru (basic_block, basic_block);
extern bool could_fall_through (basic_block, basic_block);
extern void flow_nodes_print (const char *, const sbitmap, FILE *);
extern void flow_edge_list_print (const char *, const edge *, int, FILE *);
extern void alloc_aux_for_block (basic_block, int);
extern void alloc_aux_for_blocks (int);
extern void clear_aux_for_blocks (void);
extern void free_aux_for_blocks (void);
extern void alloc_aux_for_edge (edge, int);
extern void alloc_aux_for_edges (int);
extern void clear_aux_for_edges (void);
extern void free_aux_for_edges (void);
extern void find_basic_blocks (rtx);
extern bool cleanup_cfg (int);
extern bool delete_unreachable_blocks (void);
extern bool merge_seq_blocks (void);

typedef struct conflict_graph_def *conflict_graph;

/* Callback function when enumerating conflicts.  The arguments are
   the smaller and larger regno in the conflict.  Returns zero if
   enumeration is to continue, nonzero to halt enumeration.  */
typedef int (*conflict_graph_enum_fn) (int, int, void *);


/* Prototypes of operations on conflict graphs.  */

extern conflict_graph conflict_graph_new
 (int);
extern void conflict_graph_delete (conflict_graph);
extern int conflict_graph_add (conflict_graph, int, int);
extern int conflict_graph_conflict_p (conflict_graph, int, int);
extern void conflict_graph_enum (conflict_graph, int, conflict_graph_enum_fn,
				 void *);
extern void conflict_graph_merge_regs (conflict_graph, int, int);
extern void conflict_graph_print (conflict_graph, FILE*);
extern bool mark_dfs_back_edges (void);
extern void set_edge_can_fallthru_flag (void);
extern void update_br_prob_note (basic_block);
extern void fixup_abnormal_edges (void);
extern bool inside_basic_block_p (rtx);
extern bool control_flow_insn_p (rtx);

/* In bb-reorder.c */
extern void reorder_basic_blocks (unsigned int);
extern void duplicate_computed_gotos (void);
extern void partition_hot_cold_basic_blocks (void);

/* In cfg.c */
extern void alloc_rbi_pool (void);
extern void initialize_bb_rbi (basic_block bb);
extern void free_rbi_pool (void);

/* In dominance.c */

enum cdi_direction
{
  CDI_DOMINATORS,
  CDI_POST_DOMINATORS
};

enum dom_state
{
  DOM_NONE,		/* Not computed at all.  */
  DOM_NO_FAST_QUERY,	/* The data is OK, but the fast query data are not usable.  */
  DOM_OK		/* Everything is ok.  */
};

extern enum dom_state dom_computed[2];

extern bool dom_info_available_p (enum cdi_direction);
extern void calculate_dominance_info (enum cdi_direction);
extern void free_dominance_info (enum cdi_direction);
extern basic_block nearest_common_dominator (enum cdi_direction,
					     basic_block, basic_block);
extern void set_immediate_dominator (enum cdi_direction, basic_block,
				     basic_block);
extern basic_block get_immediate_dominator (enum cdi_direction, basic_block);
extern bool dominated_by_p (enum cdi_direction, basic_block, basic_block);
extern int get_dominated_by (enum cdi_direction, basic_block, basic_block **);
extern unsigned get_dominated_by_region (enum cdi_direction, basic_block *,
					 unsigned, basic_block *);
extern void add_to_dominance_info (enum cdi_direction, basic_block);
extern void delete_from_dominance_info (enum cdi_direction, basic_block);
basic_block recount_dominator (enum cdi_direction, basic_block);
extern void redirect_immediate_dominators (enum cdi_direction, basic_block,
					   basic_block);
extern void iterate_fix_dominators (enum cdi_direction, basic_block *, int);
extern void verify_dominators (enum cdi_direction);
extern basic_block first_dom_son (enum cdi_direction, basic_block);
extern basic_block next_dom_son (enum cdi_direction, basic_block);
extern edge try_redirect_by_replacing_jump (edge, basic_block, bool);
extern void break_superblocks (void);
extern void check_bb_profile (basic_block, FILE *);
extern void update_bb_profile_for_threading (basic_block, int, gcov_type, edge);

#include "cfghooks.h"

#endif /* GCC_BASIC_BLOCK_H */
