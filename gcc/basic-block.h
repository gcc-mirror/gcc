/* Define control and data flow tables, and regsets.
   Copyright (C) 1987, 1997, 1998, 1999, 2000, 2001, 2002, 2003
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

/* Head of register set linked list.  */
typedef bitmap_head regset_head;
/* A pointer to a regset_head.  */
typedef bitmap regset;

/* Initialize a new regset.  */
#define INIT_REG_SET(HEAD) bitmap_initialize (HEAD, 1)

/* Clear a register set by freeing up the linked list.  */
#define CLEAR_REG_SET(HEAD) bitmap_clear (HEAD)

/* Copy a register set to another register set.  */
#define COPY_REG_SET(TO, FROM) bitmap_copy (TO, FROM)

/* Compare two register sets.  */
#define REG_SET_EQUAL_P(A, B) bitmap_equal_p (A, B)

/* `and' a register set with a second register set.  */
#define AND_REG_SET(TO, FROM) bitmap_operation (TO, TO, FROM, BITMAP_AND)

/* `and' the complement of a register set with a register set.  */
#define AND_COMPL_REG_SET(TO, FROM) \
  bitmap_operation (TO, TO, FROM, BITMAP_AND_COMPL)

/* Inclusive or a register set with a second register set.  */
#define IOR_REG_SET(TO, FROM) bitmap_operation (TO, TO, FROM, BITMAP_IOR)

/* Exclusive or a register set with a second register set.  */
#define XOR_REG_SET(TO, FROM) bitmap_operation (TO, TO, FROM, BITMAP_XOR)

/* Or into TO the register set FROM1 `and'ed with the complement of FROM2.  */
#define IOR_AND_COMPL_REG_SET(TO, FROM1, FROM2) \
  bitmap_ior_and_compl (TO, FROM1, FROM2)

/* Clear a single register in a register set.  */
#define CLEAR_REGNO_REG_SET(HEAD, REG) bitmap_clear_bit (HEAD, REG)

/* Set a single register in a register set.  */
#define SET_REGNO_REG_SET(HEAD, REG) bitmap_set_bit (HEAD, REG)

/* Return true if a register is set in a register set.  */
#define REGNO_REG_SET_P(TO, REG) bitmap_bit_p (TO, REG)

/* Copy the hard registers in a register set to the hard register set.  */
extern void reg_set_to_hard_reg_set PARAMS ((HARD_REG_SET *, bitmap));
#define REG_SET_TO_HARD_REG_SET(TO, FROM)				\
do {									\
  CLEAR_HARD_REG_SET (TO);						\
  reg_set_to_hard_reg_set (&TO, FROM);					\
} while (0)

/* Loop over all registers in REGSET, starting with MIN, setting REGNUM to the
   register number and executing CODE for all registers that are set.  */
#define EXECUTE_IF_SET_IN_REG_SET(REGSET, MIN, REGNUM, CODE)		\
  EXECUTE_IF_SET_IN_BITMAP (REGSET, MIN, REGNUM, CODE)

/* Loop over all registers in REGSET1 and REGSET2, starting with MIN, setting
   REGNUM to the register number and executing CODE for all registers that are
   set in the first regset and not set in the second.  */
#define EXECUTE_IF_AND_COMPL_IN_REG_SET(REGSET1, REGSET2, MIN, REGNUM, CODE) \
  EXECUTE_IF_AND_COMPL_IN_BITMAP (REGSET1, REGSET2, MIN, REGNUM, CODE)

/* Loop over all registers in REGSET1 and REGSET2, starting with MIN, setting
   REGNUM to the register number and executing CODE for all registers that are
   set in both regsets.  */
#define EXECUTE_IF_AND_IN_REG_SET(REGSET1, REGSET2, MIN, REGNUM, CODE) \
  EXECUTE_IF_AND_IN_BITMAP (REGSET1, REGSET2, MIN, REGNUM, CODE)

/* Allocate a register set with oballoc.  */
#define OBSTACK_ALLOC_REG_SET(OBSTACK) BITMAP_OBSTACK_ALLOC (OBSTACK)

/* Initialize a register set.  Returns the new register set.  */
#define INITIALIZE_REG_SET(HEAD) bitmap_initialize (&HEAD, 1)

/* Do any cleanup needed on a regset when it is no longer used.  */
#define FREE_REG_SET(REGSET) BITMAP_FREE(REGSET)

/* Do any one-time initializations needed for regsets.  */
#define INIT_ONCE_REG_SET() BITMAP_INIT_ONCE ()

/* Grow any tables needed when the number of registers is calculated
   or extended.  For the linked list allocation, nothing needs to
   be done, other than zero the statistics on the first allocation.  */
#define MAX_REGNO_REG_SET(NUM_REGS, NEW_P, RENUMBER_P)

/* Type we use to hold basic block counters.  Should be at least 64bit.  */
typedef HOST_WIDEST_INT gcov_type;

/* Control flow edge information.  */
typedef struct edge_def {
  /* Links through the predecessor and successor lists.  */
  struct edge_def *pred_next, *succ_next;

  /* The two blocks at the ends of the edge.  */
  struct basic_block_def *src, *dest;

  /* Instructions queued on the edge.  */
  rtx insns;

  /* Auxiliary info specific to a pass.  */
  void *aux;

  int flags;			/* see EDGE_* below  */
  int probability;		/* biased by REG_BR_PROB_BASE */
  gcov_type count;		/* Expected number of executions calculated
				   in profile.c  */
} *edge;

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

#define EDGE_COMPLEX	(EDGE_ABNORMAL | EDGE_ABNORMAL_CALL | EDGE_EH)


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
typedef struct basic_block_def {
  /* The first and last insns of the block.  */
  rtx head, end;

  /* The first and last trees of the block.  */
  tree head_tree;
  tree end_tree;

  /* The edges into and out of the block.  */
  edge pred, succ;

  /* Liveness info.  */

  /* The registers that are modified within this in block.  */
  regset local_set;
  /* The registers that are conditionally modified within this block.
     In other words, registers that are set only as part of a
     COND_EXEC.  */
  regset cond_local_set;
  /* The registers that are live on entry to this block.

     Note that in SSA form, global_live_at_start does not reflect the
     use of regs in phi functions, since the liveness of these regs
     may depend on which edge was taken into the block.  */
  regset global_live_at_start;
  /* The registers that are live on exit from this block.  */
  regset global_live_at_end;

  /* Auxiliary info specific to a pass.  */
  void *aux;

  /* The index of this block.  */
  int index;

  /* Previous and next blocks in the chain.  */
  struct basic_block_def *prev_bb, *next_bb;

  /* The loop depth of this block.  */
  int loop_depth;

  /* Outermost loop containing the block.  */
  struct loop *loop_father;

  /* Expected number of executions: calculated in profile.c.  */
  gcov_type count;

  /* Expected frequency.  Normalized to be in range 0 to BB_FREQ_MAX.  */
  int frequency;

  /* Various flags.  See BB_* below.  */
  int flags;
} *basic_block;

#define BB_FREQ_MAX 10000

/* Masks for basic_block.flags.  */
#define BB_DIRTY		1
#define BB_NEW			2
#define BB_REACHABLE		4
#define BB_VISITED		8

/* Number of basic blocks in the current function.  */

extern int n_basic_blocks;

/* First free basic block number.  */

extern int last_basic_block;

/* Number of edges in the current function.  */

extern int n_edges;

/* Index by basic block number, get basic block struct info.  */

extern varray_type basic_block_info;

#define BASIC_BLOCK(N)  (VARRAY_BB (basic_block_info, (N)))

/* For iterating over basic blocks.  */
#define FOR_BB_BETWEEN(BB, FROM, TO, DIR) \
  for (BB = FROM; BB != TO; BB = BB->DIR)

#define FOR_EACH_BB(BB) \
  FOR_BB_BETWEEN (BB, ENTRY_BLOCK_PTR->next_bb, EXIT_BLOCK_PTR, next_bb)

#define FOR_EACH_BB_REVERSE(BB) \
  FOR_BB_BETWEEN (BB, EXIT_BLOCK_PTR->prev_bb, ENTRY_BLOCK_PTR, prev_bb)

/* Cycles through _all_ basic blocks, even the fake ones (entry and
   exit block).  */

#define FOR_ALL_BB(BB) \
  for (BB = ENTRY_BLOCK_PTR; BB; BB = BB->next_bb)

/* What registers are live at the setjmp call.  */

extern regset regs_live_at_setjmp;

/* Special labels found during CFG build.  */

extern GTY(()) rtx label_value_list;
extern GTY(()) rtx tail_recursion_label_list;

extern struct obstack flow_obstack;

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

#define BLOCK_HEAD(B)      (BASIC_BLOCK (B)->head)
#define BLOCK_END(B)       (BASIC_BLOCK (B)->end)

#define BLOCK_HEAD_TREE(B) (BASIC_BLOCK (B)->head_tree)
#define BLOCK_END_TREE(B) (BASIC_BLOCK (B)->end_tree)

/* Special block numbers [markers] for entry and exit.  */
#define ENTRY_BLOCK (-1)
#define EXIT_BLOCK (-2)

/* Special block number not valid for any block.  */
#define INVALID_BLOCK (-3)

/* Similarly, block pointers for the edge list.  */
extern struct basic_block_def entry_exit_blocks[2];
#define ENTRY_BLOCK_PTR	(&entry_exit_blocks[0])
#define EXIT_BLOCK_PTR	(&entry_exit_blocks[1])

#define BLOCK_NUM(INSN)	      (BLOCK_FOR_INSN (INSN)->index + 0)
#define set_block_for_insn(INSN, BB)  (BLOCK_FOR_INSN (INSN) = BB)

extern void compute_bb_for_insn		PARAMS ((void));
extern void free_bb_for_insn		PARAMS ((void));
extern void update_bb_for_insn		PARAMS ((basic_block));

extern void free_basic_block_vars	PARAMS ((int));

extern edge split_block			PARAMS ((basic_block, rtx));
extern basic_block split_edge		PARAMS ((edge));
extern void insert_insn_on_edge		PARAMS ((rtx, edge));

extern void commit_edge_insertions	PARAMS ((void));
extern void commit_edge_insertions_watch_calls	PARAMS ((void));

extern void remove_fake_edges		PARAMS ((void));
extern void add_noreturn_fake_exit_edges	PARAMS ((void));
extern void connect_infinite_loops_to_exit	PARAMS ((void));
extern int flow_call_edges_add		PARAMS ((sbitmap));
extern edge cached_make_edge		PARAMS ((sbitmap *, basic_block,
						 basic_block, int));
extern edge unchecked_make_edge		PARAMS ((basic_block,
						 basic_block, int));
extern edge make_edge			PARAMS ((basic_block,
						 basic_block, int));
extern edge make_single_succ_edge	PARAMS ((basic_block,
						 basic_block, int));
extern void remove_edge			PARAMS ((edge));
extern void redirect_edge_succ		PARAMS ((edge, basic_block));
extern edge redirect_edge_succ_nodup	PARAMS ((edge, basic_block));
extern void redirect_edge_pred		PARAMS ((edge, basic_block));
extern basic_block create_basic_block_structure PARAMS ((rtx, rtx, rtx, basic_block));
extern basic_block create_basic_block	PARAMS ((rtx, rtx, basic_block));
extern int flow_delete_block		PARAMS ((basic_block));
extern int flow_delete_block_noexpunge	PARAMS ((basic_block));
extern void clear_bb_flags		PARAMS ((void));
extern void merge_blocks_nomove		PARAMS ((basic_block, basic_block));
extern void tidy_fallthru_edge		PARAMS ((edge, basic_block,
						 basic_block));
extern void tidy_fallthru_edges		PARAMS ((void));
extern void flow_reverse_top_sort_order_compute	PARAMS ((int *));
extern int flow_depth_first_order_compute	PARAMS ((int *, int *));
extern void flow_preorder_transversal_compute	PARAMS ((int *));
extern void dump_edge_info		PARAMS ((FILE *, edge, int));
extern void clear_edges			PARAMS ((void));
extern void mark_critical_edges		PARAMS ((void));
extern rtx first_insn_after_basic_block_note	PARAMS ((basic_block));

/* Dominator information for basic blocks.  */

typedef struct dominance_info *dominance_info;

/* Structure to hold information for each natural loop.  */
struct loop
{
  /* Index into loops array.  */
  int num;

  /* Basic block of loop header.  */
  basic_block header;

  /* Basic block of loop latch.  */
  basic_block latch;

  /* Basic block of loop pre-header or NULL if it does not exist.  */
  basic_block pre_header;

  /* Array of edges along the pre-header extended basic block trace.
     The source of the first edge is the root node of pre-header
     extended basic block, if it exists.  */
  edge *pre_header_edges;

  /* Number of edges along the pre_header extended basic block trace.  */
  int num_pre_header_edges;

  /* The first block in the loop.  This is not necessarily the same as
     the loop header.  */
  basic_block first;

  /* The last block in the loop.  This is not necessarily the same as
     the loop latch.  */
  basic_block last;

  /* Bitmap of blocks contained within the loop.  */
  sbitmap nodes;

  /* Number of blocks contained within the loop.  */
  int num_nodes;

  /* Array of edges that enter the loop.  */
  edge *entry_edges;

  /* Number of edges that enter the loop.  */
  int num_entries;

  /* Array of edges that exit the loop.  */
  edge *exit_edges;

  /* Number of edges that exit the loop.  */
  int num_exits;

  /* Bitmap of blocks that dominate all exits of the loop.  */
  sbitmap exits_doms;

  /* The loop nesting depth.  */
  int depth;

  /* Superloops of the loop.  */
  struct loop **pred;

  /* The height of the loop (enclosed loop levels) within the loop
     hierarchy tree.  */
  int level;

  /* The outer (parent) loop or NULL if outermost loop.  */
  struct loop *outer;

  /* The first inner (child) loop or NULL if innermost loop.  */
  struct loop *inner;

  /* Link to the next (sibling) loop.  */
  struct loop *next;

  /* Nonzero if the loop is invalid (e.g., contains setjmp.).  */
  int invalid;

  /* Auxiliary info specific to a pass.  */
  void *aux;

  /* The following are currently used by loop.c but they are likely to
     disappear as loop.c is converted to use the CFG.  */

  /* Nonzero if the loop has a NOTE_INSN_LOOP_VTOP.  */
  rtx vtop;

  /* Nonzero if the loop has a NOTE_INSN_LOOP_CONT.
     A continue statement will generate a branch to NEXT_INSN (cont).  */
  rtx cont;

  /* The NOTE_INSN_LOOP_BEG.  */
  rtx start;

  /* The NOTE_INSN_LOOP_END.  */
  rtx end;

  /* For a rotated loop that is entered near the bottom,
     this is the label at the top.  Otherwise it is zero.  */
  rtx top;

  /* Place in the loop where control enters.  */
  rtx scan_start;

  /* The position where to sink insns out of the loop.  */
  rtx sink;

  /* List of all LABEL_REFs which refer to code labels outside the
     loop.  Used by routines that need to know all loop exits, such as
     final_biv_value and final_giv_value.

     This does not include loop exits due to return instructions.
     This is because all bivs and givs are pseudos, and hence must be
     dead after a return, so the presense of a return does not affect
     any of the optimizations that use this info.  It is simpler to
     just not include return instructions on this list.  */
  rtx exit_labels;

  /* The number of LABEL_REFs on exit_labels for this loop and all
     loops nested inside it.  */
  int exit_count;
};


/* Structure to hold CFG information about natural loops within a function.  */
struct loops
{
  /* Number of natural loops in the function.  */
  int num;

  /* Maxium nested loop level in the function.  */
  int levels;

  /* Array of natural loop descriptors (scanning this array in reverse order
     will find the inner loops before their enclosing outer loops).  */
  struct loop *array;

  /* The above array is unused in new loop infrastructure and is kept only for
     purposes of the old loop optimizer.  Instead we store just pointers to
     loops here.  */
  struct loop **parray;

  /* Pointer to root of loop heirachy tree.  */
  struct loop *tree_root;

  /* Information derived from the CFG.  */
  struct cfg
  {
    /* The bitmap vector of dominators or NULL if not computed.  */
    dominance_info dom;

    /* The ordering of the basic blocks in a depth first search.  */
    int *dfs_order;

    /* The reverse completion ordering of the basic blocks found in a
       depth first search.  */
    int *rc_order;
  } cfg;

  /* Headers shared by multiple loops that should be merged.  */
  sbitmap shared_headers;
};

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

extern int flow_loops_find PARAMS ((struct loops *, int flags));
extern int flow_loops_update PARAMS ((struct loops *, int flags));
extern void flow_loops_free PARAMS ((struct loops *));
extern void flow_loops_dump PARAMS ((const struct loops *, FILE *,
				     void (*)(const struct loop *,
					      FILE *, int), int));
extern void flow_loop_dump PARAMS ((const struct loop *, FILE *,
				    void (*)(const struct loop *,
					     FILE *, int), int));
extern int flow_loop_scan PARAMS ((struct loops *, struct loop *, int));
extern void flow_loop_tree_node_add PARAMS ((struct loop *, struct loop *));
extern void flow_loop_tree_node_remove PARAMS ((struct loop *));

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
#define FALLTHRU_EDGE(bb)		((bb)->succ->flags & EDGE_FALLTHRU \
					 ? (bb)->succ : (bb)->succ->succ_next)

/* BB is assumed to contain conditional jump.  Return the branch edge.  */
#define BRANCH_EDGE(bb)			((bb)->succ->flags & EDGE_FALLTHRU \
					 ? (bb)->succ->succ_next : (bb)->succ)

/* Return expected execution frequency of the edge E.  */
#define EDGE_FREQUENCY(e)		(((e)->src->frequency \
					  * (e)->probability \
					  + REG_BR_PROB_BASE / 2) \
					 / REG_BR_PROB_BASE)

/* Return nonzero if edge is critical.  */
#define EDGE_CRITICAL_P(e)		((e)->src->succ->succ_next \
					 && (e)->dest->pred->pred_next)

struct edge_list * create_edge_list	PARAMS ((void));
void free_edge_list			PARAMS ((struct edge_list *));
void print_edge_list			PARAMS ((FILE *, struct edge_list *));
void verify_edge_list			PARAMS ((FILE *, struct edge_list *));
int find_edge_index			PARAMS ((struct edge_list *,
						 basic_block, basic_block));


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
#define PROP_FINAL		(PROP_DEATH_NOTES | PROP_LOG_LINKS  \
				 | PROP_REG_INFO | PROP_KILL_DEAD_CODE  \
				 | PROP_SCAN_DEAD_CODE | PROP_AUTOINC \
				 | PROP_ALLOW_CFG_CHANGES \
				 | PROP_SCAN_DEAD_STORES)

#define CLEANUP_EXPENSIVE	1	/* Do relativly expensive optimizations
					   except for edge forwarding */
#define CLEANUP_CROSSJUMP	2	/* Do crossjumping.  */
#define CLEANUP_POST_REGSTACK	4	/* We run after reg-stack and need
					   to care REG_DEAD notes.  */
#define CLEANUP_PRE_SIBCALL	8	/* Do not get confused by code hidden
					   inside call_placeholders..  */
#define CLEANUP_PRE_LOOP	16	/* Take care to preserve syntactic loop
					   notes.  */
#define CLEANUP_UPDATE_LIFE	32	/* Keep life information up to date.  */
#define CLEANUP_THREADING	64	/* Do jump threading.  */
#define CLEANUP_NO_INSN_DEL	128	/* Do not try to delete trivially dead
					   insns.  */
/* Flags for loop discovery.  */

#define LOOP_TREE		1	/* Build loop hierarchy tree.  */
#define LOOP_PRE_HEADER		2	/* Analyse loop pre-header.  */
#define LOOP_ENTRY_EDGES	4	/* Find entry edges.  */
#define LOOP_EXIT_EDGES		8	/* Find exit edges.  */
#define LOOP_EDGES		(LOOP_ENTRY_EDGES | LOOP_EXIT_EDGES)
#define LOOP_ALL	       15	/* All of the above  */

extern void life_analysis	PARAMS ((rtx, FILE *, int));
extern int update_life_info	PARAMS ((sbitmap, enum update_life_extent,
					 int));
extern int update_life_info_in_dirty_blocks PARAMS ((enum update_life_extent,
						      int));
extern int count_or_remove_death_notes	PARAMS ((sbitmap, int));
extern int propagate_block	PARAMS ((basic_block, regset, regset, regset,
					 int));

struct propagate_block_info;
extern rtx propagate_one_insn	PARAMS ((struct propagate_block_info *, rtx));
extern struct propagate_block_info *init_propagate_block_info
  PARAMS ((basic_block, regset, regset, regset, int));
extern void free_propagate_block_info PARAMS ((struct propagate_block_info *));

/* In lcm.c */
extern struct edge_list *pre_edge_lcm	PARAMS ((FILE *, int, sbitmap *,
						 sbitmap *, sbitmap *,
						 sbitmap *, sbitmap **,
						 sbitmap **));
extern struct edge_list *pre_edge_rev_lcm PARAMS ((FILE *, int, sbitmap *,
						   sbitmap *, sbitmap *,
						   sbitmap *, sbitmap **,
						   sbitmap **));
extern void compute_available		PARAMS ((sbitmap *, sbitmap *,
						 sbitmap *, sbitmap *));
extern int optimize_mode_switching	PARAMS ((FILE *));

/* In emit-rtl.c.  */
extern rtx emit_block_insn_after	PARAMS ((rtx, rtx, basic_block));
extern rtx emit_block_insn_before	PARAMS ((rtx, rtx, basic_block));

/* In predict.c */
extern void estimate_probability        PARAMS ((struct loops *));
extern void note_prediction_to_br_prob	PARAMS ((void));
extern void expected_value_to_br_prob	PARAMS ((void));
extern void note_prediction_to_br_prob	PARAMS ((void));
extern bool maybe_hot_bb_p		PARAMS ((basic_block));
extern bool probably_cold_bb_p		PARAMS ((basic_block));
extern bool probably_never_executed_bb_p PARAMS ((basic_block));

/* In flow.c */
extern void init_flow                   PARAMS ((void));
extern void reorder_basic_blocks	PARAMS ((void));
extern void dump_bb			PARAMS ((basic_block, FILE *));
extern void debug_bb			PARAMS ((basic_block));
extern void debug_bb_n			PARAMS ((int));
extern void dump_regset			PARAMS ((regset, FILE *));
extern void debug_regset		PARAMS ((regset));
extern void allocate_reg_life_data      PARAMS ((void));
extern void allocate_bb_life_data	PARAMS ((void));
extern void expunge_block		PARAMS ((basic_block));
extern void link_block			PARAMS ((basic_block, basic_block));
extern void unlink_block		PARAMS ((basic_block));
extern void compact_blocks		PARAMS ((void));
extern basic_block alloc_block		PARAMS ((void));
extern void find_unreachable_blocks	PARAMS ((void));
extern int delete_noop_moves		PARAMS ((rtx));
extern basic_block redirect_edge_and_branch_force PARAMS ((edge, basic_block));
extern basic_block force_nonfallthru	PARAMS ((edge));
extern bool redirect_edge_and_branch	PARAMS ((edge, basic_block));
extern rtx block_label			PARAMS ((basic_block));
extern bool forwarder_block_p		PARAMS ((basic_block));
extern bool purge_all_dead_edges	PARAMS ((int));
extern bool purge_dead_edges		PARAMS ((basic_block));
extern void find_sub_basic_blocks	PARAMS ((basic_block));
extern void find_many_sub_basic_blocks	PARAMS ((sbitmap));
extern bool can_fallthru		PARAMS ((basic_block, basic_block));
extern void flow_nodes_print		PARAMS ((const char *, const sbitmap,
						 FILE *));
extern void flow_edge_list_print	PARAMS ((const char *, const edge *,
						 int, FILE *));
extern void alloc_aux_for_block		PARAMS ((basic_block, int));
extern void alloc_aux_for_blocks	PARAMS ((int));
extern void clear_aux_for_blocks	PARAMS ((void));
extern void free_aux_for_blocks		PARAMS ((void));
extern void alloc_aux_for_edge		PARAMS ((edge, int));
extern void alloc_aux_for_edges		PARAMS ((int));
extern void clear_aux_for_edges		PARAMS ((void));
extern void free_aux_for_edges		PARAMS ((void));

/* This function is always defined so it can be called from the
   debugger, and it is declared extern so we don't get warnings about
   it being unused.  */
extern void verify_flow_info		PARAMS ((void));
extern bool flow_loop_outside_edge_p	PARAMS ((const struct loop *, edge));
extern bool flow_loop_nested_p		PARAMS ((const struct loop *,
						 const struct loop *));
extern bool flow_bb_inside_loop_p	PARAMS ((const struct loop *,
						 const basic_block));
extern basic_block *get_loop_body       PARAMS ((const struct loop *));
extern int dfs_enumerate_from           PARAMS ((basic_block, int,
				         bool (*)(basic_block, void *),
					 basic_block *, int, void *));

extern edge loop_preheader_edge PARAMS ((struct loop *));
extern edge loop_latch_edge PARAMS ((struct loop *));

extern void add_bb_to_loop PARAMS ((basic_block, struct loop *));
extern void remove_bb_from_loops PARAMS ((basic_block));
extern struct loop * find_common_loop PARAMS ((struct loop *, struct loop *));

extern void verify_loop_structure PARAMS ((struct loops *, int));
#define VLS_EXPECT_PREHEADERS 1
#define VLS_EXPECT_SIMPLE_LATCHES 2

typedef struct conflict_graph_def *conflict_graph;

/* Callback function when enumerating conflicts.  The arguments are
   the smaller and larger regno in the conflict.  Returns zero if
   enumeration is to continue, nonzero to halt enumeration.  */
typedef int (*conflict_graph_enum_fn) PARAMS ((int, int, void *));


/* Prototypes of operations on conflict graphs.  */

extern conflict_graph conflict_graph_new
                                        PARAMS ((int));
extern void conflict_graph_delete       PARAMS ((conflict_graph));
extern int conflict_graph_add           PARAMS ((conflict_graph,
						 int, int));
extern int conflict_graph_conflict_p    PARAMS ((conflict_graph,
						 int, int));
extern void conflict_graph_enum         PARAMS ((conflict_graph, int,
						 conflict_graph_enum_fn,
						 void *));
extern void conflict_graph_merge_regs   PARAMS ((conflict_graph, int,
						 int));
extern void conflict_graph_print        PARAMS ((conflict_graph, FILE*));
extern conflict_graph conflict_graph_compute
                                        PARAMS ((regset,
						 partition));
extern bool mark_dfs_back_edges		PARAMS ((void));
extern void set_edge_can_fallthru_flag	PARAMS ((void));
extern void update_br_prob_note		PARAMS ((basic_block));
extern void fixup_abnormal_edges	PARAMS ((void));
extern bool can_hoist_insn_p		PARAMS ((rtx, rtx, regset));
extern rtx hoist_insn_after		PARAMS ((rtx, rtx, rtx, rtx));
extern rtx hoist_insn_to_edge		PARAMS ((rtx, edge, rtx, rtx));
extern bool control_flow_insn_p		PARAMS ((rtx));

/* In dominance.c */

enum cdi_direction
{
  CDI_DOMINATORS,
  CDI_POST_DOMINATORS
};

extern dominance_info calculate_dominance_info	PARAMS ((enum cdi_direction));
extern void free_dominance_info			PARAMS ((dominance_info));
extern basic_block nearest_common_dominator	PARAMS ((dominance_info,
						 basic_block, basic_block));
extern void set_immediate_dominator	PARAMS ((dominance_info,
						 basic_block, basic_block));
extern basic_block get_immediate_dominator	PARAMS ((dominance_info,
						 basic_block));
extern bool dominated_by_p	PARAMS ((dominance_info, basic_block, basic_block));
extern int get_dominated_by PARAMS ((dominance_info, basic_block, basic_block **));
extern void add_to_dominance_info PARAMS ((dominance_info, basic_block));
extern void delete_from_dominance_info PARAMS ((dominance_info, basic_block));
basic_block recount_dominator PARAMS ((dominance_info, basic_block));
extern void redirect_immediate_dominators PARAMS ((dominance_info, basic_block,
						 basic_block));
void iterate_fix_dominators PARAMS ((dominance_info, basic_block *, int));
extern void verify_dominators PARAMS ((dominance_info));
#endif /* GCC_BASIC_BLOCK_H */
