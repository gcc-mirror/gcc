/* Instruction scheduling pass.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com) Enhanced by,
   and currently maintained by, Jim Wilson (wilson@cygnus.com)

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

/* This pass implements list scheduling within basic blocks.  It is
   run twice: (1) after flow analysis, but before register allocation,
   and (2) after register allocation.

   The first run performs interblock scheduling, moving insns between
   different blocks in the same "region", and the second runs only
   basic block scheduling.

   Interblock motions performed are useful motions and speculative
   motions, including speculative loads.  Motions requiring code
   duplication are not supported.  The identification of motion type
   and the check for validity of speculative motions requires
   construction and analysis of the function's control flow graph.

   The main entry point for this pass is schedule_insns(), called for
   each function.  The work of the scheduler is organized in three
   levels: (1) function level: insns are subject to splitting,
   control-flow-graph is constructed, regions are computed (after
   reload, each region is of one block), (2) region level: control
   flow graph attributes required for interblock scheduling are
   computed (dominators, reachability, etc.), data dependences and
   priorities are computed, and (3) block level: insns in the block
   are actually scheduled.  */

#include "config.h"
#include "system.h"
#include "toplev.h"
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "regs.h"
#include "function.h"
#include "flags.h"
#include "insn-config.h"
#include "insn-attr.h"
#include "except.h"
#include "toplev.h"
#include "recog.h"
#include "cfglayout.h"
#include "sched-int.h"
#include "target.h"

/* Define when we want to do count REG_DEAD notes before and after scheduling
   for sanity checking.  We can't do that when conditional execution is used,
   as REG_DEAD exist only for unconditional deaths.  */

#if !defined (HAVE_conditional_execution) && defined (ENABLE_CHECKING)
#define CHECK_DEAD_NOTES 1
#else
#define CHECK_DEAD_NOTES 0
#endif


#ifdef INSN_SCHEDULING
/* Some accessor macros for h_i_d members only used within this file.  */
#define INSN_REF_COUNT(INSN)	(h_i_d[INSN_UID (INSN)].ref_count)
#define FED_BY_SPEC_LOAD(insn)	(h_i_d[INSN_UID (insn)].fed_by_spec_load)
#define IS_LOAD_INSN(insn)	(h_i_d[INSN_UID (insn)].is_load_insn)

#define MAX_RGN_BLOCKS 10
#define MAX_RGN_INSNS 100

/* nr_inter/spec counts interblock/speculative motion for the function.  */
static int nr_inter, nr_spec;

/* Control flow graph edges are kept in circular lists.  */
typedef struct
{
  int from_block;
  int to_block;
  int next_in;
  int next_out;
}
haifa_edge;
static haifa_edge *edge_table;

#define NEXT_IN(edge) (edge_table[edge].next_in)
#define NEXT_OUT(edge) (edge_table[edge].next_out)
#define FROM_BLOCK(edge) (edge_table[edge].from_block)
#define TO_BLOCK(edge) (edge_table[edge].to_block)

/* Number of edges in the control flow graph.  (In fact, larger than
   that by 1, since edge 0 is unused.)  */
static int nr_edges;

/* Circular list of incoming/outgoing edges of a block.  */
static int *in_edges;
static int *out_edges;

#define IN_EDGES(block) (in_edges[block])
#define OUT_EDGES(block) (out_edges[block])

static int is_cfg_nonregular PARAMS ((void));
static int build_control_flow PARAMS ((struct edge_list *));
static void new_edge PARAMS ((int, int));

/* A region is the main entity for interblock scheduling: insns
   are allowed to move between blocks in the same region, along
   control flow graph edges, in the 'up' direction.  */
typedef struct
{
  int rgn_nr_blocks;		/* Number of blocks in region.  */
  int rgn_blocks;		/* cblocks in the region (actually index in rgn_bb_table).  */
}
region;

/* Number of regions in the procedure.  */
static int nr_regions;

/* Table of region descriptions.  */
static region *rgn_table;

/* Array of lists of regions' blocks.  */
static int *rgn_bb_table;

/* Topological order of blocks in the region (if b2 is reachable from
   b1, block_to_bb[b2] > block_to_bb[b1]).  Note: A basic block is
   always referred to by either block or b, while its topological
   order name (in the region) is refered to by bb.  */
static int *block_to_bb;

/* The number of the region containing a block.  */
static int *containing_rgn;

#define RGN_NR_BLOCKS(rgn) (rgn_table[rgn].rgn_nr_blocks)
#define RGN_BLOCKS(rgn) (rgn_table[rgn].rgn_blocks)
#define BLOCK_TO_BB(block) (block_to_bb[block])
#define CONTAINING_RGN(block) (containing_rgn[block])

void debug_regions PARAMS ((void));
static void find_single_block_region PARAMS ((void));
static void find_rgns PARAMS ((struct edge_list *, dominance_info));
static int too_large PARAMS ((int, int *, int *));

extern void debug_live PARAMS ((int, int));

/* Blocks of the current region being scheduled.  */
static int current_nr_blocks;
static int current_blocks;

/* The mapping from bb to block.  */
#define BB_TO_BLOCK(bb) (rgn_bb_table[current_blocks + (bb)])

typedef struct
{
  int *first_member;		/* Pointer to the list start in bitlst_table.  */
  int nr_members;		/* The number of members of the bit list.  */
}
bitlst;

static int bitlst_table_last;
static int bitlst_table_size;
static int *bitlst_table;

static void extract_bitlst PARAMS ((sbitmap, bitlst *));

/* Target info declarations.

   The block currently being scheduled is referred to as the "target" block,
   while other blocks in the region from which insns can be moved to the
   target are called "source" blocks.  The candidate structure holds info
   about such sources: are they valid?  Speculative?  Etc.  */
typedef bitlst bblst;
typedef struct
{
  char is_valid;
  char is_speculative;
  int src_prob;
  bblst split_bbs;
  bblst update_bbs;
}
candidate;

static candidate *candidate_table;

/* A speculative motion requires checking live information on the path
   from 'source' to 'target'.  The split blocks are those to be checked.
   After a speculative motion, live information should be modified in
   the 'update' blocks.

   Lists of split and update blocks for each candidate of the current
   target are in array bblst_table.  */
static int *bblst_table, bblst_size, bblst_last;

#define IS_VALID(src) ( candidate_table[src].is_valid )
#define IS_SPECULATIVE(src) ( candidate_table[src].is_speculative )
#define SRC_PROB(src) ( candidate_table[src].src_prob )

/* The bb being currently scheduled.  */
static int target_bb;

/* List of edges.  */
typedef bitlst edgelst;

/* Target info functions.  */
static void split_edges PARAMS ((int, int, edgelst *));
static void compute_trg_info PARAMS ((int));
void debug_candidate PARAMS ((int));
void debug_candidates PARAMS ((int));

/* Dominators array: dom[i] contains the sbitmap of dominators of
   bb i in the region.  */
static sbitmap *dom;

/* bb 0 is the only region entry.  */
#define IS_RGN_ENTRY(bb) (!bb)

/* Is bb_src dominated by bb_trg.  */
#define IS_DOMINATED(bb_src, bb_trg)                                 \
( TEST_BIT (dom[bb_src], bb_trg) )

/* Probability: Prob[i] is a float in [0, 1] which is the probability
   of bb i relative to the region entry.  */
static float *prob;

/* The probability of bb_src, relative to bb_trg.  Note, that while the
   'prob[bb]' is a float in [0, 1], this macro returns an integer
   in [0, 100].  */
#define GET_SRC_PROB(bb_src, bb_trg) ((int) (100.0 * (prob[bb_src] / \
						      prob[bb_trg])))

/* Bit-set of edges, where bit i stands for edge i.  */
typedef sbitmap edgeset;

/* Number of edges in the region.  */
static int rgn_nr_edges;

/* Array of size rgn_nr_edges.  */
static int *rgn_edges;


/* Mapping from each edge in the graph to its number in the rgn.  */
static int *edge_to_bit;
#define EDGE_TO_BIT(edge) (edge_to_bit[edge])

/* The split edges of a source bb is different for each target
   bb.  In order to compute this efficiently, the 'potential-split edges'
   are computed for each bb prior to scheduling a region.  This is actually
   the split edges of each bb relative to the region entry.

   pot_split[bb] is the set of potential split edges of bb.  */
static edgeset *pot_split;

/* For every bb, a set of its ancestor edges.  */
static edgeset *ancestor_edges;

static void compute_dom_prob_ps PARAMS ((int));

#define INSN_PROBABILITY(INSN) (SRC_PROB (BLOCK_TO_BB (BLOCK_NUM (INSN))))
#define IS_SPECULATIVE_INSN(INSN) (IS_SPECULATIVE (BLOCK_TO_BB (BLOCK_NUM (INSN))))
#define INSN_BB(INSN) (BLOCK_TO_BB (BLOCK_NUM (INSN)))

/* Parameters affecting the decision of rank_for_schedule().
   ??? Nope.  But MIN_PROBABILITY is used in copmute_trg_info.  */
#define MIN_PROBABILITY 40

/* Speculative scheduling functions.  */
static int check_live_1 PARAMS ((int, rtx));
static void update_live_1 PARAMS ((int, rtx));
static int check_live PARAMS ((rtx, int));
static void update_live PARAMS ((rtx, int));
static void set_spec_fed PARAMS ((rtx));
static int is_pfree PARAMS ((rtx, int, int));
static int find_conditional_protection PARAMS ((rtx, int));
static int is_conditionally_protected PARAMS ((rtx, int, int));
static int may_trap_exp PARAMS ((rtx, int));
static int haifa_classify_insn PARAMS ((rtx));
static int is_prisky PARAMS ((rtx, int, int));
static int is_exception_free PARAMS ((rtx, int, int));

static bool sets_likely_spilled PARAMS ((rtx));
static void sets_likely_spilled_1 PARAMS ((rtx, rtx, void *));
static void add_branch_dependences PARAMS ((rtx, rtx));
static void compute_block_backward_dependences PARAMS ((int));
void debug_dependencies PARAMS ((void));

static void init_regions PARAMS ((void));
static void schedule_region PARAMS ((int));
static rtx concat_INSN_LIST PARAMS ((rtx, rtx));
static void concat_insn_mem_list PARAMS ((rtx, rtx, rtx *, rtx *));
static void propagate_deps PARAMS ((int, struct deps *));
static void free_pending_lists PARAMS ((void));

/* Functions for construction of the control flow graph.  */

/* Return 1 if control flow graph should not be constructed, 0 otherwise.

   We decide not to build the control flow graph if there is possibly more
   than one entry to the function, if computed branches exist, of if we
   have nonlocal gotos.  */

static int
is_cfg_nonregular ()
{
  basic_block b;
  rtx insn;
  RTX_CODE code;

  /* If we have a label that could be the target of a nonlocal goto, then
     the cfg is not well structured.  */
  if (nonlocal_goto_handler_labels)
    return 1;

  /* If we have any forced labels, then the cfg is not well structured.  */
  if (forced_labels)
    return 1;

  /* If this function has a computed jump, then we consider the cfg
     not well structured.  */
  if (current_function_has_computed_jump)
    return 1;

  /* If we have exception handlers, then we consider the cfg not well
     structured.  ?!?  We should be able to handle this now that flow.c
     computes an accurate cfg for EH.  */
  if (current_function_has_exception_handlers ())
    return 1;

  /* If we have non-jumping insns which refer to labels, then we consider
     the cfg not well structured.  */
  /* Check for labels referred to other thn by jumps.  */
  FOR_EACH_BB (b)
    for (insn = b->head;; insn = NEXT_INSN (insn))
      {
	code = GET_CODE (insn);
	if (GET_RTX_CLASS (code) == 'i' && code != JUMP_INSN)
	  {
	    rtx note = find_reg_note (insn, REG_LABEL, NULL_RTX);

	    if (note
		&& ! (GET_CODE (NEXT_INSN (insn)) == JUMP_INSN
		      && find_reg_note (NEXT_INSN (insn), REG_LABEL,
					XEXP (note, 0))))
	      return 1;
	  }

	if (insn == b->end)
	  break;
      }

  /* All the tests passed.  Consider the cfg well structured.  */
  return 0;
}

/* Build the control flow graph and set nr_edges.

   Instead of trying to build a cfg ourselves, we rely on flow to
   do it for us.  Stamp out useless code (and bug) duplication.

   Return nonzero if an irregularity in the cfg is found which would
   prevent cross block scheduling.  */

static int
build_control_flow (edge_list)
     struct edge_list *edge_list;
{
  int i, unreachable, num_edges;
  basic_block b;

  /* This already accounts for entry/exit edges.  */
  num_edges = NUM_EDGES (edge_list);

  /* Unreachable loops with more than one basic block are detected
     during the DFS traversal in find_rgns.

     Unreachable loops with a single block are detected here.  This
     test is redundant with the one in find_rgns, but it's much
    cheaper to go ahead and catch the trivial case here.  */
  unreachable = 0;
  FOR_EACH_BB (b)
    {
      if (b->pred == NULL
	  || (b->pred->src == b
	      && b->pred->pred_next == NULL))
	unreachable = 1;
    }

  /* ??? We can kill these soon.  */
  in_edges = (int *) xcalloc (last_basic_block, sizeof (int));
  out_edges = (int *) xcalloc (last_basic_block, sizeof (int));
  edge_table = (haifa_edge *) xcalloc (num_edges, sizeof (haifa_edge));

  nr_edges = 0;
  for (i = 0; i < num_edges; i++)
    {
      edge e = INDEX_EDGE (edge_list, i);

      if (e->dest != EXIT_BLOCK_PTR
	  && e->src != ENTRY_BLOCK_PTR)
	new_edge (e->src->index, e->dest->index);
    }

  /* Increment by 1, since edge 0 is unused.  */
  nr_edges++;

  return unreachable;
}

/* Record an edge in the control flow graph from SOURCE to TARGET.

   In theory, this is redundant with the s_succs computed above, but
   we have not converted all of haifa to use information from the
   integer lists.  */

static void
new_edge (source, target)
     int source, target;
{
  int e, next_edge;
  int curr_edge, fst_edge;

  /* Check for duplicates.  */
  fst_edge = curr_edge = OUT_EDGES (source);
  while (curr_edge)
    {
      if (FROM_BLOCK (curr_edge) == source
	  && TO_BLOCK (curr_edge) == target)
	{
	  return;
	}

      curr_edge = NEXT_OUT (curr_edge);

      if (fst_edge == curr_edge)
	break;
    }

  e = ++nr_edges;

  FROM_BLOCK (e) = source;
  TO_BLOCK (e) = target;

  if (OUT_EDGES (source))
    {
      next_edge = NEXT_OUT (OUT_EDGES (source));
      NEXT_OUT (OUT_EDGES (source)) = e;
      NEXT_OUT (e) = next_edge;
    }
  else
    {
      OUT_EDGES (source) = e;
      NEXT_OUT (e) = e;
    }

  if (IN_EDGES (target))
    {
      next_edge = NEXT_IN (IN_EDGES (target));
      NEXT_IN (IN_EDGES (target)) = e;
      NEXT_IN (e) = next_edge;
    }
  else
    {
      IN_EDGES (target) = e;
      NEXT_IN (e) = e;
    }
}

/* Translate a bit-set SET to a list BL of the bit-set members.  */

static void
extract_bitlst (set, bl)
     sbitmap set;
     bitlst *bl;
{
  int i;

  /* bblst table space is reused in each call to extract_bitlst.  */
  bitlst_table_last = 0;

  bl->first_member = &bitlst_table[bitlst_table_last];
  bl->nr_members = 0;

  /* Iterate over each word in the bitset.  */
  EXECUTE_IF_SET_IN_SBITMAP (set, 0, i,
  {
    bitlst_table[bitlst_table_last++] = i;
    (bl->nr_members)++;
  });

}

/* Functions for the construction of regions.  */

/* Print the regions, for debugging purposes.  Callable from debugger.  */

void
debug_regions ()
{
  int rgn, bb;

  fprintf (sched_dump, "\n;;   ------------ REGIONS ----------\n\n");
  for (rgn = 0; rgn < nr_regions; rgn++)
    {
      fprintf (sched_dump, ";;\trgn %d nr_blocks %d:\n", rgn,
	       rgn_table[rgn].rgn_nr_blocks);
      fprintf (sched_dump, ";;\tbb/block: ");

      for (bb = 0; bb < rgn_table[rgn].rgn_nr_blocks; bb++)
	{
	  current_blocks = RGN_BLOCKS (rgn);

	  if (bb != BLOCK_TO_BB (BB_TO_BLOCK (bb)))
	    abort ();

	  fprintf (sched_dump, " %d/%d ", bb, BB_TO_BLOCK (bb));
	}

      fprintf (sched_dump, "\n\n");
    }
}

/* Build a single block region for each basic block in the function.
   This allows for using the same code for interblock and basic block
   scheduling.  */

static void
find_single_block_region ()
{
  basic_block bb;

  nr_regions = 0;

  FOR_EACH_BB (bb)
    {
      rgn_bb_table[nr_regions] = bb->index;
      RGN_NR_BLOCKS (nr_regions) = 1;
      RGN_BLOCKS (nr_regions) = nr_regions;
      CONTAINING_RGN (bb->index) = nr_regions;
      BLOCK_TO_BB (bb->index) = 0;
      nr_regions++;
    }
}

/* Update number of blocks and the estimate for number of insns
   in the region.  Return 1 if the region is "too large" for interblock
   scheduling (compile time considerations), otherwise return 0.  */

static int
too_large (block, num_bbs, num_insns)
     int block, *num_bbs, *num_insns;
{
  (*num_bbs)++;
  (*num_insns) += (INSN_LUID (BLOCK_END (block)) -
		   INSN_LUID (BLOCK_HEAD (block)));
  if ((*num_bbs > MAX_RGN_BLOCKS) || (*num_insns > MAX_RGN_INSNS))
    return 1;
  else
    return 0;
}

/* Update_loop_relations(blk, hdr): Check if the loop headed by max_hdr[blk]
   is still an inner loop.  Put in max_hdr[blk] the header of the most inner
   loop containing blk.  */
#define UPDATE_LOOP_RELATIONS(blk, hdr)		\
{						\
  if (max_hdr[blk] == -1)			\
    max_hdr[blk] = hdr;				\
  else if (dfs_nr[max_hdr[blk]] > dfs_nr[hdr])	\
    RESET_BIT (inner, hdr);			\
  else if (dfs_nr[max_hdr[blk]] < dfs_nr[hdr])	\
    {						\
      RESET_BIT (inner,max_hdr[blk]);		\
      max_hdr[blk] = hdr;			\
    }						\
}

/* Find regions for interblock scheduling.

   A region for scheduling can be:

     * A loop-free procedure, or

     * A reducible inner loop, or

     * A basic block not contained in any other region.

   ?!? In theory we could build other regions based on extended basic
   blocks or reverse extended basic blocks.  Is it worth the trouble?

   Loop blocks that form a region are put into the region's block list
   in topological order.

   This procedure stores its results into the following global (ick) variables

     * rgn_nr
     * rgn_table
     * rgn_bb_table
     * block_to_bb
     * containing region

   We use dominator relationships to avoid making regions out of non-reducible
   loops.

   This procedure needs to be converted to work on pred/succ lists instead
   of edge tables.  That would simplify it somewhat.  */

static void
find_rgns (edge_list, dom)
     struct edge_list *edge_list;
     dominance_info dom;
{
  int *max_hdr, *dfs_nr, *stack, *degree;
  char no_loops = 1;
  int node, child, loop_head, i, head, tail;
  int count = 0, sp, idx = 0, current_edge = out_edges[0];
  int num_bbs, num_insns, unreachable;
  int too_large_failure;
  basic_block bb;

  /* Note if an edge has been passed.  */
  sbitmap passed;

  /* Note if a block is a natural loop header.  */
  sbitmap header;

  /* Note if a block is a natural inner loop header.  */
  sbitmap inner;

  /* Note if a block is in the block queue.  */
  sbitmap in_queue;

  /* Note if a block is in the block queue.  */
  sbitmap in_stack;

  int num_edges = NUM_EDGES (edge_list);

  /* Perform a DFS traversal of the cfg.  Identify loop headers, inner loops
     and a mapping from block to its loop header (if the block is contained
     in a loop, else -1).

     Store results in HEADER, INNER, and MAX_HDR respectively, these will
     be used as inputs to the second traversal.

     STACK, SP and DFS_NR are only used during the first traversal.  */

  /* Allocate and initialize variables for the first traversal.  */
  max_hdr = (int *) xmalloc (last_basic_block * sizeof (int));
  dfs_nr = (int *) xcalloc (last_basic_block, sizeof (int));
  stack = (int *) xmalloc (nr_edges * sizeof (int));

  inner = sbitmap_alloc (last_basic_block);
  sbitmap_ones (inner);

  header = sbitmap_alloc (last_basic_block);
  sbitmap_zero (header);

  passed = sbitmap_alloc (nr_edges);
  sbitmap_zero (passed);

  in_queue = sbitmap_alloc (last_basic_block);
  sbitmap_zero (in_queue);

  in_stack = sbitmap_alloc (last_basic_block);
  sbitmap_zero (in_stack);

  for (i = 0; i < last_basic_block; i++)
    max_hdr[i] = -1;

  /* DFS traversal to find inner loops in the cfg.  */

  sp = -1;
  while (1)
    {
      if (current_edge == 0 || TEST_BIT (passed, current_edge))
	{
	  /* We have reached a leaf node or a node that was already
	     processed.  Pop edges off the stack until we find
	     an edge that has not yet been processed.  */
	  while (sp >= 0
		 && (current_edge == 0 || TEST_BIT (passed, current_edge)))
	    {
	      /* Pop entry off the stack.  */
	      current_edge = stack[sp--];
	      node = FROM_BLOCK (current_edge);
	      child = TO_BLOCK (current_edge);
	      RESET_BIT (in_stack, child);
	      if (max_hdr[child] >= 0 && TEST_BIT (in_stack, max_hdr[child]))
		UPDATE_LOOP_RELATIONS (node, max_hdr[child]);
	      current_edge = NEXT_OUT (current_edge);
	    }

	  /* See if have finished the DFS tree traversal.  */
	  if (sp < 0 && TEST_BIT (passed, current_edge))
	    break;

	  /* Nope, continue the traversal with the popped node.  */
	  continue;
	}

      /* Process a node.  */
      node = FROM_BLOCK (current_edge);
      child = TO_BLOCK (current_edge);
      SET_BIT (in_stack, node);
      dfs_nr[node] = ++count;

      /* If the successor is in the stack, then we've found a loop.
	 Mark the loop, if it is not a natural loop, then it will
	 be rejected during the second traversal.  */
      if (TEST_BIT (in_stack, child))
	{
	  no_loops = 0;
	  SET_BIT (header, child);
	  UPDATE_LOOP_RELATIONS (node, child);
	  SET_BIT (passed, current_edge);
	  current_edge = NEXT_OUT (current_edge);
	  continue;
	}

      /* If the child was already visited, then there is no need to visit
	 it again.  Just update the loop relationships and restart
	 with a new edge.  */
      if (dfs_nr[child])
	{
	  if (max_hdr[child] >= 0 && TEST_BIT (in_stack, max_hdr[child]))
	    UPDATE_LOOP_RELATIONS (node, max_hdr[child]);
	  SET_BIT (passed, current_edge);
	  current_edge = NEXT_OUT (current_edge);
	  continue;
	}

      /* Push an entry on the stack and continue DFS traversal.  */
      stack[++sp] = current_edge;
      SET_BIT (passed, current_edge);
      current_edge = OUT_EDGES (child);

      /* This is temporary until haifa is converted to use rth's new
	 cfg routines which have true entry/exit blocks and the
	 appropriate edges from/to those blocks.

	 Generally we update dfs_nr for a node when we process its
	 out edge.  However, if the node has no out edge then we will
	 not set dfs_nr for that node.  This can confuse the scheduler
	 into thinking that we have unreachable blocks, which in turn
	 disables cross block scheduling.

	 So, if we have a node with no out edges, go ahead and mark it
	 as reachable now.  */
      if (current_edge == 0)
	dfs_nr[child] = ++count;
    }

  /* Another check for unreachable blocks.  The earlier test in
     is_cfg_nonregular only finds unreachable blocks that do not
     form a loop.

     The DFS traversal will mark every block that is reachable from
     the entry node by placing a nonzero value in dfs_nr.  Thus if
     dfs_nr is zero for any block, then it must be unreachable.  */
  unreachable = 0;
  FOR_EACH_BB (bb)
    if (dfs_nr[bb->index] == 0)
      {
	unreachable = 1;
	break;
      }

  /* Gross.  To avoid wasting memory, the second pass uses the dfs_nr array
     to hold degree counts.  */
  degree = dfs_nr;

  FOR_EACH_BB (bb)
    degree[bb->index] = 0;
  for (i = 0; i < num_edges; i++)
    {
      edge e = INDEX_EDGE (edge_list, i);

      if (e->dest != EXIT_BLOCK_PTR)
	degree[e->dest->index]++;
    }

  /* Do not perform region scheduling if there are any unreachable
     blocks.  */
  if (!unreachable)
    {
      int *queue;

      if (no_loops)
	SET_BIT (header, 0);

      /* Second travsersal:find reducible inner loops and topologically sort
	 block of each region.  */

      queue = (int *) xmalloc (n_basic_blocks * sizeof (int));

      /* Find blocks which are inner loop headers.  We still have non-reducible
	 loops to consider at this point.  */
      FOR_EACH_BB (bb)
	{
	  if (TEST_BIT (header, bb->index) && TEST_BIT (inner, bb->index))
	    {
	      edge e;
	      basic_block jbb;

	      /* Now check that the loop is reducible.  We do this separate
		 from finding inner loops so that we do not find a reducible
		 loop which contains an inner non-reducible loop.

		 A simple way to find reducible/natural loops is to verify
		 that each block in the loop is dominated by the loop
		 header.

		 If there exists a block that is not dominated by the loop
		 header, then the block is reachable from outside the loop
		 and thus the loop is not a natural loop.  */
	      FOR_EACH_BB (jbb)
		{
		  /* First identify blocks in the loop, except for the loop
		     entry block.  */
		  if (bb->index == max_hdr[jbb->index] && bb != jbb)
		    {
		      /* Now verify that the block is dominated by the loop
			 header.  */
		      if (!dominated_by_p (dom, jbb, bb))
			break;
		    }
		}

	      /* If we exited the loop early, then I is the header of
		 a non-reducible loop and we should quit processing it
		 now.  */
	      if (jbb != EXIT_BLOCK_PTR)
		continue;

	      /* I is a header of an inner loop, or block 0 in a subroutine
		 with no loops at all.  */
	      head = tail = -1;
	      too_large_failure = 0;
	      loop_head = max_hdr[bb->index];

	      /* Decrease degree of all I's successors for topological
		 ordering.  */
	      for (e = bb->succ; e; e = e->succ_next)
		if (e->dest != EXIT_BLOCK_PTR)
		  --degree[e->dest->index];

	      /* Estimate # insns, and count # blocks in the region.  */
	      num_bbs = 1;
	      num_insns = (INSN_LUID (bb->end)
			   - INSN_LUID (bb->head));

	      /* Find all loop latches (blocks with back edges to the loop
		 header) or all the leaf blocks in the cfg has no loops.

		 Place those blocks into the queue.  */
	      if (no_loops)
		{
		  FOR_EACH_BB (jbb)
		    /* Leaf nodes have only a single successor which must
		       be EXIT_BLOCK.  */
		    if (jbb->succ
			&& jbb->succ->dest == EXIT_BLOCK_PTR
			&& jbb->succ->succ_next == NULL)
		      {
			queue[++tail] = jbb->index;
			SET_BIT (in_queue, jbb->index);

			if (too_large (jbb->index, &num_bbs, &num_insns))
			  {
			    too_large_failure = 1;
			    break;
			  }
		      }
		}
	      else
		{
		  edge e;

		  for (e = bb->pred; e; e = e->pred_next)
		    {
		      if (e->src == ENTRY_BLOCK_PTR)
			continue;

		      node = e->src->index;

		      if (max_hdr[node] == loop_head && node != bb->index)
			{
			  /* This is a loop latch.  */
			  queue[++tail] = node;
			  SET_BIT (in_queue, node);

			  if (too_large (node, &num_bbs, &num_insns))
			    {
			      too_large_failure = 1;
			      break;
			    }
			}
		    }
		}

	      /* Now add all the blocks in the loop to the queue.

	     We know the loop is a natural loop; however the algorithm
	     above will not always mark certain blocks as being in the
	     loop.  Consider:
		node   children
		 a	  b,c
		 b	  c
		 c	  a,d
		 d	  b

	     The algorithm in the DFS traversal may not mark B & D as part
	     of the loop (ie they will not have max_hdr set to A).

	     We know they can not be loop latches (else they would have
	     had max_hdr set since they'd have a backedge to a dominator
	     block).  So we don't need them on the initial queue.

	     We know they are part of the loop because they are dominated
	     by the loop header and can be reached by a backwards walk of
	     the edges starting with nodes on the initial queue.

	     It is safe and desirable to include those nodes in the
	     loop/scheduling region.  To do so we would need to decrease
	     the degree of a node if it is the target of a backedge
	     within the loop itself as the node is placed in the queue.

	     We do not do this because I'm not sure that the actual
	     scheduling code will properly handle this case. ?!? */

	      while (head < tail && !too_large_failure)
		{
		  edge e;
		  child = queue[++head];

		  for (e = BASIC_BLOCK (child)->pred; e; e = e->pred_next)
		    {
		      node = e->src->index;

		      /* See discussion above about nodes not marked as in
			 this loop during the initial DFS traversal.  */
		      if (e->src == ENTRY_BLOCK_PTR
			  || max_hdr[node] != loop_head)
			{
			  tail = -1;
			  break;
			}
		      else if (!TEST_BIT (in_queue, node) && node != bb->index)
			{
			  queue[++tail] = node;
			  SET_BIT (in_queue, node);

			  if (too_large (node, &num_bbs, &num_insns))
			    {
			      too_large_failure = 1;
			      break;
			    }
			}
		    }
		}

	      if (tail >= 0 && !too_large_failure)
		{
		  /* Place the loop header into list of region blocks.  */
		  degree[bb->index] = -1;
		  rgn_bb_table[idx] = bb->index;
		  RGN_NR_BLOCKS (nr_regions) = num_bbs;
		  RGN_BLOCKS (nr_regions) = idx++;
		  CONTAINING_RGN (bb->index) = nr_regions;
		  BLOCK_TO_BB (bb->index) = count = 0;

		  /* Remove blocks from queue[] when their in degree
		     becomes zero.  Repeat until no blocks are left on the
		     list.  This produces a topological list of blocks in
		     the region.  */
		  while (tail >= 0)
		    {
		      if (head < 0)
			head = tail;
		      child = queue[head];
		      if (degree[child] == 0)
			{
			  edge e;

			  degree[child] = -1;
			  rgn_bb_table[idx++] = child;
			  BLOCK_TO_BB (child) = ++count;
			  CONTAINING_RGN (child) = nr_regions;
			  queue[head] = queue[tail--];

			  for (e = BASIC_BLOCK (child)->succ;
			       e;
			       e = e->succ_next)
			    if (e->dest != EXIT_BLOCK_PTR)
			      --degree[e->dest->index];
			}
		      else
			--head;
		    }
		  ++nr_regions;
		}
	    }
	}
      free (queue);
    }

  /* Any block that did not end up in a region is placed into a region
     by itself.  */
  FOR_EACH_BB (bb)
    if (degree[bb->index] >= 0)
      {
	rgn_bb_table[idx] = bb->index;
	RGN_NR_BLOCKS (nr_regions) = 1;
	RGN_BLOCKS (nr_regions) = idx++;
	CONTAINING_RGN (bb->index) = nr_regions++;
	BLOCK_TO_BB (bb->index) = 0;
      }

  free (max_hdr);
  free (dfs_nr);
  free (stack);
  sbitmap_free (passed);
  sbitmap_free (header);
  sbitmap_free (inner);
  sbitmap_free (in_queue);
  sbitmap_free (in_stack);
}

/* Functions for regions scheduling information.  */

/* Compute dominators, probability, and potential-split-edges of bb.
   Assume that these values were already computed for bb's predecessors.  */

static void
compute_dom_prob_ps (bb)
     int bb;
{
  int nxt_in_edge, fst_in_edge, pred;
  int fst_out_edge, nxt_out_edge, nr_out_edges, nr_rgn_out_edges;

  prob[bb] = 0.0;
  if (IS_RGN_ENTRY (bb))
    {
      SET_BIT (dom[bb], 0);
      prob[bb] = 1.0;
      return;
    }

  fst_in_edge = nxt_in_edge = IN_EDGES (BB_TO_BLOCK (bb));

  /* Initialize dom[bb] to '111..1'.  */
  sbitmap_ones (dom[bb]);

  do
    {
      pred = FROM_BLOCK (nxt_in_edge);
      sbitmap_a_and_b (dom[bb], dom[bb], dom[BLOCK_TO_BB (pred)]);
      sbitmap_a_or_b (ancestor_edges[bb], ancestor_edges[bb], ancestor_edges[BLOCK_TO_BB (pred)]);

      SET_BIT (ancestor_edges[bb], EDGE_TO_BIT (nxt_in_edge));

      nr_out_edges = 1;
      nr_rgn_out_edges = 0;
      fst_out_edge = OUT_EDGES (pred);
      nxt_out_edge = NEXT_OUT (fst_out_edge);

      sbitmap_a_or_b (pot_split[bb], pot_split[bb], pot_split[BLOCK_TO_BB (pred)]);

      SET_BIT (pot_split[bb], EDGE_TO_BIT (fst_out_edge));

      /* The successor doesn't belong in the region?  */
      if (CONTAINING_RGN (TO_BLOCK (fst_out_edge)) !=
	  CONTAINING_RGN (BB_TO_BLOCK (bb)))
	++nr_rgn_out_edges;

      while (fst_out_edge != nxt_out_edge)
	{
	  ++nr_out_edges;
	  /* The successor doesn't belong in the region?  */
	  if (CONTAINING_RGN (TO_BLOCK (nxt_out_edge)) !=
	      CONTAINING_RGN (BB_TO_BLOCK (bb)))
	    ++nr_rgn_out_edges;
	  SET_BIT (pot_split[bb], EDGE_TO_BIT (nxt_out_edge));
	  nxt_out_edge = NEXT_OUT (nxt_out_edge);

	}

      /* Now nr_rgn_out_edges is the number of region-exit edges from
         pred, and nr_out_edges will be the number of pred out edges
         not leaving the region.  */
      nr_out_edges -= nr_rgn_out_edges;
      if (nr_rgn_out_edges > 0)
	prob[bb] += 0.9 * prob[BLOCK_TO_BB (pred)] / nr_out_edges;
      else
	prob[bb] += prob[BLOCK_TO_BB (pred)] / nr_out_edges;
      nxt_in_edge = NEXT_IN (nxt_in_edge);
    }
  while (fst_in_edge != nxt_in_edge);

  SET_BIT (dom[bb], bb);
  sbitmap_difference (pot_split[bb], pot_split[bb], ancestor_edges[bb]);

  if (sched_verbose >= 2)
    fprintf (sched_dump, ";;  bb_prob(%d, %d) = %3d\n", bb, BB_TO_BLOCK (bb),
	     (int) (100.0 * prob[bb]));
}

/* Functions for target info.  */

/* Compute in BL the list of split-edges of bb_src relatively to bb_trg.
   Note that bb_trg dominates bb_src.  */

static void
split_edges (bb_src, bb_trg, bl)
     int bb_src;
     int bb_trg;
     edgelst *bl;
{
  sbitmap src = (edgeset) sbitmap_alloc (pot_split[bb_src]->n_bits);
  sbitmap_copy (src, pot_split[bb_src]);

  sbitmap_difference (src, src, pot_split[bb_trg]);
  extract_bitlst (src, bl);
  sbitmap_free (src);
}

/* Find the valid candidate-source-blocks for the target block TRG, compute
   their probability, and check if they are speculative or not.
   For speculative sources, compute their update-blocks and split-blocks.  */

static void
compute_trg_info (trg)
     int trg;
{
  candidate *sp;
  edgelst el;
  int check_block, update_idx;
  int i, j, k, fst_edge, nxt_edge;

  /* Define some of the fields for the target bb as well.  */
  sp = candidate_table + trg;
  sp->is_valid = 1;
  sp->is_speculative = 0;
  sp->src_prob = 100;

  for (i = trg + 1; i < current_nr_blocks; i++)
    {
      sp = candidate_table + i;

      sp->is_valid = IS_DOMINATED (i, trg);
      if (sp->is_valid)
	{
	  sp->src_prob = GET_SRC_PROB (i, trg);
	  sp->is_valid = (sp->src_prob >= MIN_PROBABILITY);
	}

      if (sp->is_valid)
	{
	  split_edges (i, trg, &el);
	  sp->is_speculative = (el.nr_members) ? 1 : 0;
	  if (sp->is_speculative && !flag_schedule_speculative)
	    sp->is_valid = 0;
	}

      if (sp->is_valid)
	{
	  char *update_blocks;

	  /* Compute split blocks and store them in bblst_table.
	     The TO block of every split edge is a split block.  */
	  sp->split_bbs.first_member = &bblst_table[bblst_last];
	  sp->split_bbs.nr_members = el.nr_members;
	  for (j = 0; j < el.nr_members; bblst_last++, j++)
	    bblst_table[bblst_last] =
	      TO_BLOCK (rgn_edges[el.first_member[j]]);
	  sp->update_bbs.first_member = &bblst_table[bblst_last];

	  /* Compute update blocks and store them in bblst_table.
	     For every split edge, look at the FROM block, and check
	     all out edges.  For each out edge that is not a split edge,
	     add the TO block to the update block list.  This list can end
	     up with a lot of duplicates.  We need to weed them out to avoid
	     overrunning the end of the bblst_table.  */
	  update_blocks = (char *) alloca (last_basic_block);
	  memset (update_blocks, 0, last_basic_block);

	  update_idx = 0;
	  for (j = 0; j < el.nr_members; j++)
	    {
	      check_block = FROM_BLOCK (rgn_edges[el.first_member[j]]);
	      fst_edge = nxt_edge = OUT_EDGES (check_block);
	      do
		{
		  if (! update_blocks[TO_BLOCK (nxt_edge)])
		    {
		      for (k = 0; k < el.nr_members; k++)
			if (EDGE_TO_BIT (nxt_edge) == el.first_member[k])
			  break;

		      if (k >= el.nr_members)
			{
			  bblst_table[bblst_last++] = TO_BLOCK (nxt_edge);
			  update_blocks[TO_BLOCK (nxt_edge)] = 1;
			  update_idx++;
			}
		    }

		  nxt_edge = NEXT_OUT (nxt_edge);
		}
	      while (fst_edge != nxt_edge);
	    }
	  sp->update_bbs.nr_members = update_idx;

	  /* Make sure we didn't overrun the end of bblst_table.  */
	  if (bblst_last > bblst_size)
	    abort ();
	}
      else
	{
	  sp->split_bbs.nr_members = sp->update_bbs.nr_members = 0;

	  sp->is_speculative = 0;
	  sp->src_prob = 0;
	}
    }
}

/* Print candidates info, for debugging purposes.  Callable from debugger.  */

void
debug_candidate (i)
     int i;
{
  if (!candidate_table[i].is_valid)
    return;

  if (candidate_table[i].is_speculative)
    {
      int j;
      fprintf (sched_dump, "src b %d bb %d speculative \n", BB_TO_BLOCK (i), i);

      fprintf (sched_dump, "split path: ");
      for (j = 0; j < candidate_table[i].split_bbs.nr_members; j++)
	{
	  int b = candidate_table[i].split_bbs.first_member[j];

	  fprintf (sched_dump, " %d ", b);
	}
      fprintf (sched_dump, "\n");

      fprintf (sched_dump, "update path: ");
      for (j = 0; j < candidate_table[i].update_bbs.nr_members; j++)
	{
	  int b = candidate_table[i].update_bbs.first_member[j];

	  fprintf (sched_dump, " %d ", b);
	}
      fprintf (sched_dump, "\n");
    }
  else
    {
      fprintf (sched_dump, " src %d equivalent\n", BB_TO_BLOCK (i));
    }
}

/* Print candidates info, for debugging purposes.  Callable from debugger.  */

void
debug_candidates (trg)
     int trg;
{
  int i;

  fprintf (sched_dump, "----------- candidate table: target: b=%d bb=%d ---\n",
	   BB_TO_BLOCK (trg), trg);
  for (i = trg + 1; i < current_nr_blocks; i++)
    debug_candidate (i);
}

/* Functions for speculative scheduing.  */

/* Return 0 if x is a set of a register alive in the beginning of one
   of the split-blocks of src, otherwise return 1.  */

static int
check_live_1 (src, x)
     int src;
     rtx x;
{
  int i;
  int regno;
  rtx reg = SET_DEST (x);

  if (reg == 0)
    return 1;

  while (GET_CODE (reg) == SUBREG || GET_CODE (reg) == ZERO_EXTRACT
	 || GET_CODE (reg) == SIGN_EXTRACT
	 || GET_CODE (reg) == STRICT_LOW_PART)
    reg = XEXP (reg, 0);

  if (GET_CODE (reg) == PARALLEL)
    {
      int i;

      for (i = XVECLEN (reg, 0) - 1; i >= 0; i--)
	if (XEXP (XVECEXP (reg, 0, i), 0) != 0)
	  if (check_live_1 (src, XEXP (XVECEXP (reg, 0, i), 0)))
	    return 1;

      return 0;
    }

  if (GET_CODE (reg) != REG)
    return 1;

  regno = REGNO (reg);

  if (regno < FIRST_PSEUDO_REGISTER && global_regs[regno])
    {
      /* Global registers are assumed live.  */
      return 0;
    }
  else
    {
      if (regno < FIRST_PSEUDO_REGISTER)
	{
	  /* Check for hard registers.  */
	  int j = HARD_REGNO_NREGS (regno, GET_MODE (reg));
	  while (--j >= 0)
	    {
	      for (i = 0; i < candidate_table[src].split_bbs.nr_members; i++)
		{
		  int b = candidate_table[src].split_bbs.first_member[i];

		  if (REGNO_REG_SET_P (BASIC_BLOCK (b)->global_live_at_start,
				       regno + j))
		    {
		      return 0;
		    }
		}
	    }
	}
      else
	{
	  /* Check for psuedo registers.  */
	  for (i = 0; i < candidate_table[src].split_bbs.nr_members; i++)
	    {
	      int b = candidate_table[src].split_bbs.first_member[i];

	      if (REGNO_REG_SET_P (BASIC_BLOCK (b)->global_live_at_start, regno))
		{
		  return 0;
		}
	    }
	}
    }

  return 1;
}

/* If x is a set of a register R, mark that R is alive in the beginning
   of every update-block of src.  */

static void
update_live_1 (src, x)
     int src;
     rtx x;
{
  int i;
  int regno;
  rtx reg = SET_DEST (x);

  if (reg == 0)
    return;

  while (GET_CODE (reg) == SUBREG || GET_CODE (reg) == ZERO_EXTRACT
	 || GET_CODE (reg) == SIGN_EXTRACT
	 || GET_CODE (reg) == STRICT_LOW_PART)
    reg = XEXP (reg, 0);

  if (GET_CODE (reg) == PARALLEL)
    {
      int i;

      for (i = XVECLEN (reg, 0) - 1; i >= 0; i--)
	if (XEXP (XVECEXP (reg, 0, i), 0) != 0)
	  update_live_1 (src, XEXP (XVECEXP (reg, 0, i), 0));

      return;
    }

  if (GET_CODE (reg) != REG)
    return;

  /* Global registers are always live, so the code below does not apply
     to them.  */

  regno = REGNO (reg);

  if (regno >= FIRST_PSEUDO_REGISTER || !global_regs[regno])
    {
      if (regno < FIRST_PSEUDO_REGISTER)
	{
	  int j = HARD_REGNO_NREGS (regno, GET_MODE (reg));
	  while (--j >= 0)
	    {
	      for (i = 0; i < candidate_table[src].update_bbs.nr_members; i++)
		{
		  int b = candidate_table[src].update_bbs.first_member[i];

		  SET_REGNO_REG_SET (BASIC_BLOCK (b)->global_live_at_start,
				     regno + j);
		}
	    }
	}
      else
	{
	  for (i = 0; i < candidate_table[src].update_bbs.nr_members; i++)
	    {
	      int b = candidate_table[src].update_bbs.first_member[i];

	      SET_REGNO_REG_SET (BASIC_BLOCK (b)->global_live_at_start, regno);
	    }
	}
    }
}

/* Return 1 if insn can be speculatively moved from block src to trg,
   otherwise return 0.  Called before first insertion of insn to
   ready-list or before the scheduling.  */

static int
check_live (insn, src)
     rtx insn;
     int src;
{
  /* Find the registers set by instruction.  */
  if (GET_CODE (PATTERN (insn)) == SET
      || GET_CODE (PATTERN (insn)) == CLOBBER)
    return check_live_1 (src, PATTERN (insn));
  else if (GET_CODE (PATTERN (insn)) == PARALLEL)
    {
      int j;
      for (j = XVECLEN (PATTERN (insn), 0) - 1; j >= 0; j--)
	if ((GET_CODE (XVECEXP (PATTERN (insn), 0, j)) == SET
	     || GET_CODE (XVECEXP (PATTERN (insn), 0, j)) == CLOBBER)
	    && !check_live_1 (src, XVECEXP (PATTERN (insn), 0, j)))
	  return 0;

      return 1;
    }

  return 1;
}

/* Update the live registers info after insn was moved speculatively from
   block src to trg.  */

static void
update_live (insn, src)
     rtx insn;
     int src;
{
  /* Find the registers set by instruction.  */
  if (GET_CODE (PATTERN (insn)) == SET
      || GET_CODE (PATTERN (insn)) == CLOBBER)
    update_live_1 (src, PATTERN (insn));
  else if (GET_CODE (PATTERN (insn)) == PARALLEL)
    {
      int j;
      for (j = XVECLEN (PATTERN (insn), 0) - 1; j >= 0; j--)
	if (GET_CODE (XVECEXP (PATTERN (insn), 0, j)) == SET
	    || GET_CODE (XVECEXP (PATTERN (insn), 0, j)) == CLOBBER)
	  update_live_1 (src, XVECEXP (PATTERN (insn), 0, j));
    }
}

/* Exception Free Loads:

   We define five classes of speculative loads: IFREE, IRISKY,
   PFREE, PRISKY, and MFREE.

   IFREE loads are loads that are proved to be exception-free, just
   by examining the load insn.  Examples for such loads are loads
   from TOC and loads of global data.

   IRISKY loads are loads that are proved to be exception-risky,
   just by examining the load insn.  Examples for such loads are
   volatile loads and loads from shared memory.

   PFREE loads are loads for which we can prove, by examining other
   insns, that they are exception-free.  Currently, this class consists
   of loads for which we are able to find a "similar load", either in
   the target block, or, if only one split-block exists, in that split
   block.  Load2 is similar to load1 if both have same single base
   register.  We identify only part of the similar loads, by finding
   an insn upon which both load1 and load2 have a DEF-USE dependence.

   PRISKY loads are loads for which we can prove, by examining other
   insns, that they are exception-risky.  Currently we have two proofs for
   such loads.  The first proof detects loads that are probably guarded by a
   test on the memory address.  This proof is based on the
   backward and forward data dependence information for the region.
   Let load-insn be the examined load.
   Load-insn is PRISKY iff ALL the following hold:

   - insn1 is not in the same block as load-insn
   - there is a DEF-USE dependence chain (insn1, ..., load-insn)
   - test-insn is either a compare or a branch, not in the same block
     as load-insn
   - load-insn is reachable from test-insn
   - there is a DEF-USE dependence chain (insn1, ..., test-insn)

   This proof might fail when the compare and the load are fed
   by an insn not in the region.  To solve this, we will add to this
   group all loads that have no input DEF-USE dependence.

   The second proof detects loads that are directly or indirectly
   fed by a speculative load.  This proof is affected by the
   scheduling process.  We will use the flag  fed_by_spec_load.
   Initially, all insns have this flag reset.  After a speculative
   motion of an insn, if insn is either a load, or marked as
   fed_by_spec_load, we will also mark as fed_by_spec_load every
   insn1 for which a DEF-USE dependence (insn, insn1) exists.  A
   load which is fed_by_spec_load is also PRISKY.

   MFREE (maybe-free) loads are all the remaining loads. They may be
   exception-free, but we cannot prove it.

   Now, all loads in IFREE and PFREE classes are considered
   exception-free, while all loads in IRISKY and PRISKY classes are
   considered exception-risky.  As for loads in the MFREE class,
   these are considered either exception-free or exception-risky,
   depending on whether we are pessimistic or optimistic.  We have
   to take the pessimistic approach to assure the safety of
   speculative scheduling, but we can take the optimistic approach
   by invoking the -fsched_spec_load_dangerous option.  */

enum INSN_TRAP_CLASS
{
  TRAP_FREE = 0, IFREE = 1, PFREE_CANDIDATE = 2,
  PRISKY_CANDIDATE = 3, IRISKY = 4, TRAP_RISKY = 5
};

#define WORST_CLASS(class1, class2) \
((class1 > class2) ? class1 : class2)

/* Non-zero if block bb_to is equal to, or reachable from block bb_from.  */
#define IS_REACHABLE(bb_from, bb_to)					\
  (bb_from == bb_to							\
   || IS_RGN_ENTRY (bb_from)						\
   || (TEST_BIT (ancestor_edges[bb_to],					\
		 EDGE_TO_BIT (IN_EDGES (BB_TO_BLOCK (bb_from))))))

/* Non-zero iff the address is comprised from at most 1 register.  */
#define CONST_BASED_ADDRESS_P(x)			\
  (GET_CODE (x) == REG					\
   || ((GET_CODE (x) == PLUS || GET_CODE (x) == MINUS	\
	|| (GET_CODE (x) == LO_SUM))			\
       && (CONSTANT_P (XEXP (x, 0))			\
	   || CONSTANT_P (XEXP (x, 1)))))

/* Turns on the fed_by_spec_load flag for insns fed by load_insn.  */

static void
set_spec_fed (load_insn)
     rtx load_insn;
{
  rtx link;

  for (link = INSN_DEPEND (load_insn); link; link = XEXP (link, 1))
    if (GET_MODE (link) == VOIDmode)
      FED_BY_SPEC_LOAD (XEXP (link, 0)) = 1;
}				/* set_spec_fed */

/* On the path from the insn to load_insn_bb, find a conditional
branch depending on insn, that guards the speculative load.  */

static int
find_conditional_protection (insn, load_insn_bb)
     rtx insn;
     int load_insn_bb;
{
  rtx link;

  /* Iterate through DEF-USE forward dependences.  */
  for (link = INSN_DEPEND (insn); link; link = XEXP (link, 1))
    {
      rtx next = XEXP (link, 0);
      if ((CONTAINING_RGN (BLOCK_NUM (next)) ==
	   CONTAINING_RGN (BB_TO_BLOCK (load_insn_bb)))
	  && IS_REACHABLE (INSN_BB (next), load_insn_bb)
	  && load_insn_bb != INSN_BB (next)
	  && GET_MODE (link) == VOIDmode
	  && (GET_CODE (next) == JUMP_INSN
	      || find_conditional_protection (next, load_insn_bb)))
	return 1;
    }
  return 0;
}				/* find_conditional_protection */

/* Returns 1 if the same insn1 that participates in the computation
   of load_insn's address is feeding a conditional branch that is
   guarding on load_insn. This is true if we find a the two DEF-USE
   chains:
   insn1 -> ... -> conditional-branch
   insn1 -> ... -> load_insn,
   and if a flow path exist:
   insn1 -> ... -> conditional-branch -> ... -> load_insn,
   and if insn1 is on the path
   region-entry -> ... -> bb_trg -> ... load_insn.

   Locate insn1 by climbing on LOG_LINKS from load_insn.
   Locate the branch by following INSN_DEPEND from insn1.  */

static int
is_conditionally_protected (load_insn, bb_src, bb_trg)
     rtx load_insn;
     int bb_src, bb_trg;
{
  rtx link;

  for (link = LOG_LINKS (load_insn); link; link = XEXP (link, 1))
    {
      rtx insn1 = XEXP (link, 0);

      /* Must be a DEF-USE dependence upon non-branch.  */
      if (GET_MODE (link) != VOIDmode
	  || GET_CODE (insn1) == JUMP_INSN)
	continue;

      /* Must exist a path: region-entry -> ... -> bb_trg -> ... load_insn.  */
      if (INSN_BB (insn1) == bb_src
	  || (CONTAINING_RGN (BLOCK_NUM (insn1))
	      != CONTAINING_RGN (BB_TO_BLOCK (bb_src)))
	  || (!IS_REACHABLE (bb_trg, INSN_BB (insn1))
	      && !IS_REACHABLE (INSN_BB (insn1), bb_trg)))
	continue;

      /* Now search for the conditional-branch.  */
      if (find_conditional_protection (insn1, bb_src))
	return 1;

      /* Recursive step: search another insn1, "above" current insn1.  */
      return is_conditionally_protected (insn1, bb_src, bb_trg);
    }

  /* The chain does not exist.  */
  return 0;
}				/* is_conditionally_protected */

/* Returns 1 if a clue for "similar load" 'insn2' is found, and hence
   load_insn can move speculatively from bb_src to bb_trg.  All the
   following must hold:

   (1) both loads have 1 base register (PFREE_CANDIDATEs).
   (2) load_insn and load1 have a def-use dependence upon
   the same insn 'insn1'.
   (3) either load2 is in bb_trg, or:
   - there's only one split-block, and
   - load1 is on the escape path, and

   From all these we can conclude that the two loads access memory
   addresses that differ at most by a constant, and hence if moving
   load_insn would cause an exception, it would have been caused by
   load2 anyhow.  */

static int
is_pfree (load_insn, bb_src, bb_trg)
     rtx load_insn;
     int bb_src, bb_trg;
{
  rtx back_link;
  candidate *candp = candidate_table + bb_src;

  if (candp->split_bbs.nr_members != 1)
    /* Must have exactly one escape block.  */
    return 0;

  for (back_link = LOG_LINKS (load_insn);
       back_link; back_link = XEXP (back_link, 1))
    {
      rtx insn1 = XEXP (back_link, 0);

      if (GET_MODE (back_link) == VOIDmode)
	{
	  /* Found a DEF-USE dependence (insn1, load_insn).  */
	  rtx fore_link;

	  for (fore_link = INSN_DEPEND (insn1);
	       fore_link; fore_link = XEXP (fore_link, 1))
	    {
	      rtx insn2 = XEXP (fore_link, 0);
	      if (GET_MODE (fore_link) == VOIDmode)
		{
		  /* Found a DEF-USE dependence (insn1, insn2).  */
		  if (haifa_classify_insn (insn2) != PFREE_CANDIDATE)
		    /* insn2 not guaranteed to be a 1 base reg load.  */
		    continue;

		  if (INSN_BB (insn2) == bb_trg)
		    /* insn2 is the similar load, in the target block.  */
		    return 1;

		  if (*(candp->split_bbs.first_member) == BLOCK_NUM (insn2))
		    /* insn2 is a similar load, in a split-block.  */
		    return 1;
		}
	    }
	}
    }

  /* Couldn't find a similar load.  */
  return 0;
}				/* is_pfree */

/* Returns a class that insn with GET_DEST(insn)=x may belong to,
   as found by analyzing insn's expression.  */

static int
may_trap_exp (x, is_store)
     rtx x;
     int is_store;
{
  enum rtx_code code;

  if (x == 0)
    return TRAP_FREE;
  code = GET_CODE (x);
  if (is_store)
    {
      if (code == MEM && may_trap_p (x))
	return TRAP_RISKY;
      else
	return TRAP_FREE;
    }
  if (code == MEM)
    {
      /* The insn uses memory:  a volatile load.  */
      if (MEM_VOLATILE_P (x))
	return IRISKY;
      /* An exception-free load.  */
      if (!may_trap_p (x))
	return IFREE;
      /* A load with 1 base register, to be further checked.  */
      if (CONST_BASED_ADDRESS_P (XEXP (x, 0)))
	return PFREE_CANDIDATE;
      /* No info on the load, to be further checked.  */
      return PRISKY_CANDIDATE;
    }
  else
    {
      const char *fmt;
      int i, insn_class = TRAP_FREE;

      /* Neither store nor load, check if it may cause a trap.  */
      if (may_trap_p (x))
	return TRAP_RISKY;
      /* Recursive step: walk the insn...  */
      fmt = GET_RTX_FORMAT (code);
      for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
	{
	  if (fmt[i] == 'e')
	    {
	      int tmp_class = may_trap_exp (XEXP (x, i), is_store);
	      insn_class = WORST_CLASS (insn_class, tmp_class);
	    }
	  else if (fmt[i] == 'E')
	    {
	      int j;
	      for (j = 0; j < XVECLEN (x, i); j++)
		{
		  int tmp_class = may_trap_exp (XVECEXP (x, i, j), is_store);
		  insn_class = WORST_CLASS (insn_class, tmp_class);
		  if (insn_class == TRAP_RISKY || insn_class == IRISKY)
		    break;
		}
	    }
	  if (insn_class == TRAP_RISKY || insn_class == IRISKY)
	    break;
	}
      return insn_class;
    }
}

/* Classifies insn for the purpose of verifying that it can be
   moved speculatively, by examining it's patterns, returning:
   TRAP_RISKY: store, or risky non-load insn (e.g. division by variable).
   TRAP_FREE: non-load insn.
   IFREE: load from a globaly safe location.
   IRISKY: volatile load.
   PFREE_CANDIDATE, PRISKY_CANDIDATE: load that need to be checked for
   being either PFREE or PRISKY.  */

static int
haifa_classify_insn (insn)
     rtx insn;
{
  rtx pat = PATTERN (insn);
  int tmp_class = TRAP_FREE;
  int insn_class = TRAP_FREE;
  enum rtx_code code;

  if (GET_CODE (pat) == PARALLEL)
    {
      int i, len = XVECLEN (pat, 0);

      for (i = len - 1; i >= 0; i--)
	{
	  code = GET_CODE (XVECEXP (pat, 0, i));
	  switch (code)
	    {
	    case CLOBBER:
	      /* Test if it is a 'store'.  */
	      tmp_class = may_trap_exp (XEXP (XVECEXP (pat, 0, i), 0), 1);
	      break;
	    case SET:
	      /* Test if it is a store.  */
	      tmp_class = may_trap_exp (SET_DEST (XVECEXP (pat, 0, i)), 1);
	      if (tmp_class == TRAP_RISKY)
		break;
	      /* Test if it is a load.  */
	      tmp_class
		= WORST_CLASS (tmp_class,
			       may_trap_exp (SET_SRC (XVECEXP (pat, 0, i)),
					     0));
	      break;
	    case COND_EXEC:
	    case TRAP_IF:
	      tmp_class = TRAP_RISKY;
	      break;
	    default:
	      ;
	    }
	  insn_class = WORST_CLASS (insn_class, tmp_class);
	  if (insn_class == TRAP_RISKY || insn_class == IRISKY)
	    break;
	}
    }
  else
    {
      code = GET_CODE (pat);
      switch (code)
	{
	case CLOBBER:
	  /* Test if it is a 'store'.  */
	  tmp_class = may_trap_exp (XEXP (pat, 0), 1);
	  break;
	case SET:
	  /* Test if it is a store.  */
	  tmp_class = may_trap_exp (SET_DEST (pat), 1);
	  if (tmp_class == TRAP_RISKY)
	    break;
	  /* Test if it is a load.  */
	  tmp_class =
	    WORST_CLASS (tmp_class,
			 may_trap_exp (SET_SRC (pat), 0));
	  break;
	case COND_EXEC:
	case TRAP_IF:
	  tmp_class = TRAP_RISKY;
	  break;
	default:;
	}
      insn_class = tmp_class;
    }

  return insn_class;
}

/* Return 1 if load_insn is prisky (i.e. if load_insn is fed by
   a load moved speculatively, or if load_insn is protected by
   a compare on load_insn's address).  */

static int
is_prisky (load_insn, bb_src, bb_trg)
     rtx load_insn;
     int bb_src, bb_trg;
{
  if (FED_BY_SPEC_LOAD (load_insn))
    return 1;

  if (LOG_LINKS (load_insn) == NULL)
    /* Dependence may 'hide' out of the region.  */
    return 1;

  if (is_conditionally_protected (load_insn, bb_src, bb_trg))
    return 1;

  return 0;
}

/* Insn is a candidate to be moved speculatively from bb_src to bb_trg.
   Return 1 if insn is exception-free (and the motion is valid)
   and 0 otherwise.  */

static int
is_exception_free (insn, bb_src, bb_trg)
     rtx insn;
     int bb_src, bb_trg;
{
  int insn_class = haifa_classify_insn (insn);

  /* Handle non-load insns.  */
  switch (insn_class)
    {
    case TRAP_FREE:
      return 1;
    case TRAP_RISKY:
      return 0;
    default:;
    }

  /* Handle loads.  */
  if (!flag_schedule_speculative_load)
    return 0;
  IS_LOAD_INSN (insn) = 1;
  switch (insn_class)
    {
    case IFREE:
      return (1);
    case IRISKY:
      return 0;
    case PFREE_CANDIDATE:
      if (is_pfree (insn, bb_src, bb_trg))
	return 1;
      /* Don't 'break' here: PFREE-candidate is also PRISKY-candidate.  */
    case PRISKY_CANDIDATE:
      if (!flag_schedule_speculative_load_dangerous
	  || is_prisky (insn, bb_src, bb_trg))
	return 0;
      break;
    default:;
    }

  return flag_schedule_speculative_load_dangerous;
}

/* The number of insns from the current block scheduled so far.  */
static int sched_target_n_insns;
/* The number of insns from the current block to be scheduled in total.  */
static int target_n_insns;
/* The number of insns from the entire region scheduled so far.  */
static int sched_n_insns;
/* Nonzero if the last scheduled insn was a jump.  */
static int last_was_jump;

/* Implementations of the sched_info functions for region scheduling.  */
static void init_ready_list PARAMS ((struct ready_list *));
static int can_schedule_ready_p PARAMS ((rtx));
static int new_ready PARAMS ((rtx));
static int schedule_more_p PARAMS ((void));
static const char *rgn_print_insn PARAMS ((rtx, int));
static int rgn_rank PARAMS ((rtx, rtx));
static int contributes_to_priority PARAMS ((rtx, rtx));
static void compute_jump_reg_dependencies PARAMS ((rtx, regset, regset,
						   regset));

/* Return nonzero if there are more insns that should be scheduled.  */

static int
schedule_more_p ()
{
  return ! last_was_jump && sched_target_n_insns < target_n_insns;
}

/* Add all insns that are initially ready to the ready list READY.  Called
   once before scheduling a set of insns.  */

static void
init_ready_list (ready)
     struct ready_list *ready;
{
  rtx prev_head = current_sched_info->prev_head;
  rtx next_tail = current_sched_info->next_tail;
  int bb_src;
  rtx insn;

  target_n_insns = 0;
  sched_target_n_insns = 0;
  sched_n_insns = 0;
  last_was_jump = 0;

  /* Print debugging information.  */
  if (sched_verbose >= 5)
    debug_dependencies ();

  /* Prepare current target block info.  */
  if (current_nr_blocks > 1)
    {
      candidate_table = (candidate *) xmalloc (current_nr_blocks
					       * sizeof (candidate));

      bblst_last = 0;
      /* bblst_table holds split blocks and update blocks for each block after
	 the current one in the region.  split blocks and update blocks are
	 the TO blocks of region edges, so there can be at most rgn_nr_edges
	 of them.  */
      bblst_size = (current_nr_blocks - target_bb) * rgn_nr_edges;
      bblst_table = (int *) xmalloc (bblst_size * sizeof (int));

      bitlst_table_last = 0;
      bitlst_table_size = rgn_nr_edges;
      bitlst_table = (int *) xmalloc (rgn_nr_edges * sizeof (int));

      compute_trg_info (target_bb);
    }

  /* Initialize ready list with all 'ready' insns in target block.
     Count number of insns in the target block being scheduled.  */
  for (insn = NEXT_INSN (prev_head); insn != next_tail; insn = NEXT_INSN (insn))
    {
      rtx next;

      if (! INSN_P (insn))
	continue;
      next = NEXT_INSN (insn);

      if (INSN_DEP_COUNT (insn) == 0
	  && (! INSN_P (next) || SCHED_GROUP_P (next) == 0))
	ready_add (ready, insn);
      if (!(SCHED_GROUP_P (insn)))
	target_n_insns++;
    }

  /* Add to ready list all 'ready' insns in valid source blocks.
     For speculative insns, check-live, exception-free, and
     issue-delay.  */
  for (bb_src = target_bb + 1; bb_src < current_nr_blocks; bb_src++)
    if (IS_VALID (bb_src))
      {
	rtx src_head;
	rtx src_next_tail;
	rtx tail, head;

	get_block_head_tail (BB_TO_BLOCK (bb_src), &head, &tail);
	src_next_tail = NEXT_INSN (tail);
	src_head = head;

	for (insn = src_head; insn != src_next_tail; insn = NEXT_INSN (insn))
	  {
	    if (! INSN_P (insn))
	      continue;

	    if (!CANT_MOVE (insn)
		&& (!IS_SPECULATIVE_INSN (insn)
		    || ((((!targetm.sched.use_dfa_pipeline_interface
			   || !(*targetm.sched.use_dfa_pipeline_interface) ())
			  && insn_issue_delay (insn) <= 3)
			 || (targetm.sched.use_dfa_pipeline_interface
			     && (*targetm.sched.use_dfa_pipeline_interface) ()
			     && (recog_memoized (insn) < 0
			         || min_insn_conflict_delay (curr_state,
							     insn, insn) <= 3)))
			&& check_live (insn, bb_src)
			&& is_exception_free (insn, bb_src, target_bb))))
	      {
		rtx next;

		/* Note that we haven't squirreled away the notes for
		   blocks other than the current.  So if this is a
		   speculative insn, NEXT might otherwise be a note.  */
		next = next_nonnote_insn (insn);
		if (INSN_DEP_COUNT (insn) == 0
		    && (! next
			|| ! INSN_P (next)
			|| SCHED_GROUP_P (next) == 0))
		  ready_add (ready, insn);
	      }
	  }
      }
}

/* Called after taking INSN from the ready list.  Returns nonzero if this
   insn can be scheduled, nonzero if we should silently discard it.  */

static int
can_schedule_ready_p (insn)
     rtx insn;
{
  if (GET_CODE (insn) == JUMP_INSN)
    last_was_jump = 1;

  /* An interblock motion?  */
  if (INSN_BB (insn) != target_bb)
    {
      rtx temp;
      basic_block b1;

      if (IS_SPECULATIVE_INSN (insn))
	{
	  if (!check_live (insn, INSN_BB (insn)))
	    return 0;
	  update_live (insn, INSN_BB (insn));

	  /* For speculative load, mark insns fed by it.  */
	  if (IS_LOAD_INSN (insn) || FED_BY_SPEC_LOAD (insn))
	    set_spec_fed (insn);

	  nr_spec++;
	}
      nr_inter++;

      /* Find the beginning of the scheduling group.  */
      /* ??? Ought to update basic block here, but later bits of
	 schedule_block assumes the original insn block is
	 still intact.  */

      temp = insn;
      while (SCHED_GROUP_P (temp))
	temp = PREV_INSN (temp);

      /* Update source block boundaries.  */
      b1 = BLOCK_FOR_INSN (temp);
      if (temp == b1->head && insn == b1->end)
	{
	  /* We moved all the insns in the basic block.
	     Emit a note after the last insn and update the
	     begin/end boundaries to point to the note.  */
	  rtx note = emit_note_after (NOTE_INSN_DELETED, insn);
	  b1->head = note;
	  b1->end = note;
	}
      else if (insn == b1->end)
	{
	  /* We took insns from the end of the basic block,
	     so update the end of block boundary so that it
	     points to the first insn we did not move.  */
	  b1->end = PREV_INSN (temp);
	}
      else if (temp == b1->head)
	{
	  /* We took insns from the start of the basic block,
	     so update the start of block boundary so that
	     it points to the first insn we did not move.  */
	  b1->head = NEXT_INSN (insn);
	}
    }
  else
    {
      /* In block motion.  */
      sched_target_n_insns++;
    }
  sched_n_insns++;

  return 1;
}

/* Called after INSN has all its dependencies resolved.  Return nonzero
   if it should be moved to the ready list or the queue, or zero if we
   should silently discard it.  */
static int
new_ready (next)
     rtx next;
{
  /* For speculative insns, before inserting to ready/queue,
     check live, exception-free, and issue-delay.  */
  if (INSN_BB (next) != target_bb
      && (!IS_VALID (INSN_BB (next))
	  || CANT_MOVE (next)
	  || (IS_SPECULATIVE_INSN (next)
	      && (0
		  || (targetm.sched.use_dfa_pipeline_interface
		      && (*targetm.sched.use_dfa_pipeline_interface) ()
		      && recog_memoized (next) >= 0
		      && min_insn_conflict_delay (curr_state, next,
						  next) > 3)
		  || ((!targetm.sched.use_dfa_pipeline_interface
		       || !(*targetm.sched.use_dfa_pipeline_interface) ())
		      && insn_issue_delay (next) > 3)
		  || !check_live (next, INSN_BB (next))
		  || !is_exception_free (next, INSN_BB (next), target_bb)))))
    return 0;
  return 1;
}

/* Return a string that contains the insn uid and optionally anything else
   necessary to identify this insn in an output.  It's valid to use a
   static buffer for this.  The ALIGNED parameter should cause the string
   to be formatted so that multiple output lines will line up nicely.  */

static const char *
rgn_print_insn (insn, aligned)
     rtx insn;
     int aligned;
{
  static char tmp[80];

  if (aligned)
    sprintf (tmp, "b%3d: i%4d", INSN_BB (insn), INSN_UID (insn));
  else
    {
      if (current_nr_blocks > 1 && INSN_BB (insn) != target_bb)
	sprintf (tmp, "%d/b%d", INSN_UID (insn), INSN_BB (insn));
      else
	sprintf (tmp, "%d", INSN_UID (insn));
    }
  return tmp;
}

/* Compare priority of two insns.  Return a positive number if the second
   insn is to be preferred for scheduling, and a negative one if the first
   is to be preferred.  Zero if they are equally good.  */

static int
rgn_rank (insn1, insn2)
     rtx insn1, insn2;
{
  /* Some comparison make sense in interblock scheduling only.  */
  if (INSN_BB (insn1) != INSN_BB (insn2))
    {
      int spec_val, prob_val;

      /* Prefer an inblock motion on an interblock motion.  */
      if ((INSN_BB (insn2) == target_bb) && (INSN_BB (insn1) != target_bb))
	return 1;
      if ((INSN_BB (insn1) == target_bb) && (INSN_BB (insn2) != target_bb))
	return -1;

      /* Prefer a useful motion on a speculative one.  */
      spec_val = IS_SPECULATIVE_INSN (insn1) - IS_SPECULATIVE_INSN (insn2);
      if (spec_val)
	return spec_val;

      /* Prefer a more probable (speculative) insn.  */
      prob_val = INSN_PROBABILITY (insn2) - INSN_PROBABILITY (insn1);
      if (prob_val)
	return prob_val;
    }
  return 0;
}

/* NEXT is an instruction that depends on INSN (a backward dependence);
   return nonzero if we should include this dependence in priority
   calculations.  */

static int
contributes_to_priority (next, insn)
     rtx next, insn;
{
  return BLOCK_NUM (next) == BLOCK_NUM (insn);
}

/* INSN is a JUMP_INSN, COND_SET is the set of registers that are
   conditionally set before INSN.  Store the set of registers that
   must be considered as used by this jump in USED and that of
   registers that must be considered as set in SET.  */

static void
compute_jump_reg_dependencies (insn, cond_set, used, set)
     rtx insn ATTRIBUTE_UNUSED;
     regset cond_set ATTRIBUTE_UNUSED;
     regset used ATTRIBUTE_UNUSED;
     regset set ATTRIBUTE_UNUSED;
{
  /* Nothing to do here, since we postprocess jumps in
     add_branch_dependences.  */
}

/* Used in schedule_insns to initialize current_sched_info for scheduling
   regions (or single basic blocks).  */

static struct sched_info region_sched_info =
{
  init_ready_list,
  can_schedule_ready_p,
  schedule_more_p,
  new_ready,
  rgn_rank,
  rgn_print_insn,
  contributes_to_priority,
  compute_jump_reg_dependencies,

  NULL, NULL,
  NULL, NULL,
  0, 0
};

/* Determine if PAT sets a CLASS_LIKELY_SPILLED_P register.  */

static bool
sets_likely_spilled (pat)
     rtx pat;
{
  bool ret = false;
  note_stores (pat, sets_likely_spilled_1, &ret);
  return ret;
}

static void
sets_likely_spilled_1 (x, pat, data)
     rtx x, pat;
     void *data;
{
  bool *ret = (bool *) data;

  if (GET_CODE (pat) == SET
      && REG_P (x)
      && REGNO (x) < FIRST_PSEUDO_REGISTER
      && CLASS_LIKELY_SPILLED_P (REGNO_REG_CLASS (REGNO (x))))
    *ret = true;
}

/* Add dependences so that branches are scheduled to run last in their
   block.  */

static void
add_branch_dependences (head, tail)
     rtx head, tail;
{
  rtx insn, last;

  /* For all branches, calls, uses, clobbers, cc0 setters, and instructions
     that can throw exceptions, force them to remain in order at the end of
     the block by adding dependencies and giving the last a high priority.
     There may be notes present, and prev_head may also be a note.

     Branches must obviously remain at the end.  Calls should remain at the
     end since moving them results in worse register allocation.  Uses remain
     at the end to ensure proper register allocation.

     cc0 setters remaim at the end because they can't be moved away from
     their cc0 user.

     Insns setting CLASS_LIKELY_SPILLED_P registers (usually return values)
     are not moved before reload because we can wind up with register
     allocation failures.  */

  insn = tail;
  last = 0;
  while (GET_CODE (insn) == CALL_INSN
	 || GET_CODE (insn) == JUMP_INSN
	 || (GET_CODE (insn) == INSN
	     && (GET_CODE (PATTERN (insn)) == USE
		 || GET_CODE (PATTERN (insn)) == CLOBBER
		 || can_throw_internal (insn)
#ifdef HAVE_cc0
		 || sets_cc0_p (PATTERN (insn))
#endif
		 || (!reload_completed
		     && sets_likely_spilled (PATTERN (insn)))))
	 || GET_CODE (insn) == NOTE)
    {
      if (GET_CODE (insn) != NOTE)
	{
	  if (last != 0 && !find_insn_list (insn, LOG_LINKS (last)))
	    {
	      add_dependence (last, insn, REG_DEP_ANTI);
	      INSN_REF_COUNT (insn)++;
	    }

	  CANT_MOVE (insn) = 1;

	  last = insn;
	  /* Skip over insns that are part of a group.
	     Make each insn explicitly depend on the previous insn.
	     This ensures that only the group header will ever enter
	     the ready queue (and, when scheduled, will automatically
	     schedule the SCHED_GROUP_P block).  */
	  while (SCHED_GROUP_P (insn))
	    {
	      rtx temp = prev_nonnote_insn (insn);
	      add_dependence (insn, temp, REG_DEP_ANTI);
	      insn = temp;
	    }
	}

      /* Don't overrun the bounds of the basic block.  */
      if (insn == head)
	break;

      insn = PREV_INSN (insn);
    }

  /* Make sure these insns are scheduled last in their block.  */
  insn = last;
  if (insn != 0)
    while (insn != head)
      {
	insn = prev_nonnote_insn (insn);

	if (INSN_REF_COUNT (insn) != 0)
	  continue;

	add_dependence (last, insn, REG_DEP_ANTI);
	INSN_REF_COUNT (insn) = 1;

	/* Skip over insns that are part of a group.  */
	while (SCHED_GROUP_P (insn))
	  insn = prev_nonnote_insn (insn);
      }
}

/* Data structures for the computation of data dependences in a regions.  We
   keep one `deps' structure for every basic block.  Before analyzing the
   data dependences for a bb, its variables are initialized as a function of
   the variables of its predecessors.  When the analysis for a bb completes,
   we save the contents to the corresponding bb_deps[bb] variable.  */

static struct deps *bb_deps;

/* Duplicate the INSN_LIST elements of COPY and prepend them to OLD.  */

static rtx
concat_INSN_LIST (copy, old)
     rtx copy, old;
{
  rtx new = old;
  for (; copy ; copy = XEXP (copy, 1))
    new = alloc_INSN_LIST (XEXP (copy, 0), new);
  return new;
}

static void
concat_insn_mem_list (copy_insns, copy_mems, old_insns_p, old_mems_p)
     rtx copy_insns, copy_mems;
     rtx *old_insns_p, *old_mems_p;
{
  rtx new_insns = *old_insns_p;
  rtx new_mems = *old_mems_p;

  while (copy_insns)
    {
      new_insns = alloc_INSN_LIST (XEXP (copy_insns, 0), new_insns);
      new_mems = alloc_EXPR_LIST (VOIDmode, XEXP (copy_mems, 0), new_mems);
      copy_insns = XEXP (copy_insns, 1);
      copy_mems = XEXP (copy_mems, 1);
    }

  *old_insns_p = new_insns;
  *old_mems_p = new_mems;
}

/* After computing the dependencies for block BB, propagate the dependencies
   found in TMP_DEPS to the successors of the block.  */
static void
propagate_deps (bb, pred_deps)
     int bb;
     struct deps *pred_deps;
{
  int b = BB_TO_BLOCK (bb);
  int e, first_edge;

  /* bb's structures are inherited by its successors.  */
  first_edge = e = OUT_EDGES (b);
  if (e > 0)
    do
      {
	int b_succ = TO_BLOCK (e);
	int bb_succ = BLOCK_TO_BB (b_succ);
	struct deps *succ_deps = bb_deps + bb_succ;
	int reg;

	/* Only bbs "below" bb, in the same region, are interesting.  */
	if (CONTAINING_RGN (b) != CONTAINING_RGN (b_succ)
	    || bb_succ <= bb)
	  {
	    e = NEXT_OUT (e);
	    continue;
	  }

	/* The reg_last lists are inherited by bb_succ.  */
	EXECUTE_IF_SET_IN_REG_SET (&pred_deps->reg_last_in_use, 0, reg,
	  {
	    struct deps_reg *pred_rl = &pred_deps->reg_last[reg];
	    struct deps_reg *succ_rl = &succ_deps->reg_last[reg];

	    succ_rl->uses = concat_INSN_LIST (pred_rl->uses, succ_rl->uses);
	    succ_rl->sets = concat_INSN_LIST (pred_rl->sets, succ_rl->sets);
	    succ_rl->clobbers = concat_INSN_LIST (pred_rl->clobbers,
						  succ_rl->clobbers);
	    succ_rl->uses_length += pred_rl->uses_length;
	    succ_rl->clobbers_length += pred_rl->clobbers_length;
	  });
	IOR_REG_SET (&succ_deps->reg_last_in_use, &pred_deps->reg_last_in_use);

	/* Mem read/write lists are inherited by bb_succ.  */
	concat_insn_mem_list (pred_deps->pending_read_insns,
			      pred_deps->pending_read_mems,
			      &succ_deps->pending_read_insns,
			      &succ_deps->pending_read_mems);
	concat_insn_mem_list (pred_deps->pending_write_insns,
			      pred_deps->pending_write_mems,
			      &succ_deps->pending_write_insns,
			      &succ_deps->pending_write_mems);

	succ_deps->last_pending_memory_flush
	  = concat_INSN_LIST (pred_deps->last_pending_memory_flush,
			      succ_deps->last_pending_memory_flush);

	succ_deps->pending_lists_length += pred_deps->pending_lists_length;
	succ_deps->pending_flush_length += pred_deps->pending_flush_length;

	/* last_function_call is inherited by bb_succ.  */
	succ_deps->last_function_call
	  = concat_INSN_LIST (pred_deps->last_function_call,
			      succ_deps->last_function_call);

	/* sched_before_next_call is inherited by bb_succ.  */
	succ_deps->sched_before_next_call
	  = concat_INSN_LIST (pred_deps->sched_before_next_call,
			      succ_deps->sched_before_next_call);

	e = NEXT_OUT (e);
      }
    while (e != first_edge);

  /* These lists should point to the right place, for correct
     freeing later.  */
  bb_deps[bb].pending_read_insns = pred_deps->pending_read_insns;
  bb_deps[bb].pending_read_mems = pred_deps->pending_read_mems;
  bb_deps[bb].pending_write_insns = pred_deps->pending_write_insns;
  bb_deps[bb].pending_write_mems = pred_deps->pending_write_mems;

  /* Can't allow these to be freed twice.  */
  pred_deps->pending_read_insns = 0;
  pred_deps->pending_read_mems = 0;
  pred_deps->pending_write_insns = 0;
  pred_deps->pending_write_mems = 0;
}

/* Compute backward dependences inside bb.  In a multiple blocks region:
   (1) a bb is analyzed after its predecessors, and (2) the lists in
   effect at the end of bb (after analyzing for bb) are inherited by
   bb's successrs.

   Specifically for reg-reg data dependences, the block insns are
   scanned by sched_analyze () top-to-bottom.  Two lists are
   maintained by sched_analyze (): reg_last[].sets for register DEFs,
   and reg_last[].uses for register USEs.

   When analysis is completed for bb, we update for its successors:
   ;  - DEFS[succ] = Union (DEFS [succ], DEFS [bb])
   ;  - USES[succ] = Union (USES [succ], DEFS [bb])

   The mechanism for computing mem-mem data dependence is very
   similar, and the result is interblock dependences in the region.  */

static void
compute_block_backward_dependences (bb)
     int bb;
{
  rtx head, tail;
  struct deps tmp_deps;

  tmp_deps = bb_deps[bb];

  /* Do the analysis for this block.  */
  get_block_head_tail (BB_TO_BLOCK (bb), &head, &tail);
  sched_analyze (&tmp_deps, head, tail);
  add_branch_dependences (head, tail);

  if (current_nr_blocks > 1)
    propagate_deps (bb, &tmp_deps);

  /* Free up the INSN_LISTs.  */
  free_deps (&tmp_deps);
}

/* Remove all INSN_LISTs and EXPR_LISTs from the pending lists and add
   them to the unused_*_list variables, so that they can be reused.  */

static void
free_pending_lists ()
{
  int bb;

  for (bb = 0; bb < current_nr_blocks; bb++)
    {
      free_INSN_LIST_list (&bb_deps[bb].pending_read_insns);
      free_INSN_LIST_list (&bb_deps[bb].pending_write_insns);
      free_EXPR_LIST_list (&bb_deps[bb].pending_read_mems);
      free_EXPR_LIST_list (&bb_deps[bb].pending_write_mems);
    }
}

/* Print dependences for debugging, callable from debugger.  */

void
debug_dependencies ()
{
  int bb;

  fprintf (sched_dump, ";;   --------------- forward dependences: ------------ \n");
  for (bb = 0; bb < current_nr_blocks; bb++)
    {
      if (1)
	{
	  rtx head, tail;
	  rtx next_tail;
	  rtx insn;

	  get_block_head_tail (BB_TO_BLOCK (bb), &head, &tail);
	  next_tail = NEXT_INSN (tail);
	  fprintf (sched_dump, "\n;;   --- Region Dependences --- b %d bb %d \n",
		   BB_TO_BLOCK (bb), bb);

	  if (targetm.sched.use_dfa_pipeline_interface
	      && (*targetm.sched.use_dfa_pipeline_interface) ())
	    {
	      fprintf (sched_dump, ";;   %7s%6s%6s%6s%6s%6s%14s\n",
		       "insn", "code", "bb", "dep", "prio", "cost",
		       "reservation");
	      fprintf (sched_dump, ";;   %7s%6s%6s%6s%6s%6s%14s\n",
		       "----", "----", "--", "---", "----", "----",
		       "-----------");
	    }
	  else
	    {
	      fprintf (sched_dump, ";;   %7s%6s%6s%6s%6s%6s%11s%6s\n",
	      "insn", "code", "bb", "dep", "prio", "cost", "blockage", "units");
	      fprintf (sched_dump, ";;   %7s%6s%6s%6s%6s%6s%11s%6s\n",
	      "----", "----", "--", "---", "----", "----", "--------", "-----");
	    }

	  for (insn = head; insn != next_tail; insn = NEXT_INSN (insn))
	    {
	      rtx link;

	      if (! INSN_P (insn))
		{
		  int n;
		  fprintf (sched_dump, ";;   %6d ", INSN_UID (insn));
		  if (GET_CODE (insn) == NOTE)
		    {
		      n = NOTE_LINE_NUMBER (insn);
		      if (n < 0)
			fprintf (sched_dump, "%s\n", GET_NOTE_INSN_NAME (n));
		      else
			fprintf (sched_dump, "line %d, file %s\n", n,
				 NOTE_SOURCE_FILE (insn));
		    }
		  else
		    fprintf (sched_dump, " {%s}\n", GET_RTX_NAME (GET_CODE (insn)));
		  continue;
		}

	      if (targetm.sched.use_dfa_pipeline_interface
		  && (*targetm.sched.use_dfa_pipeline_interface) ())
		{
		  fprintf (sched_dump,
			   ";;   %s%5d%6d%6d%6d%6d%6d   ",
			   (SCHED_GROUP_P (insn) ? "+" : " "),
			   INSN_UID (insn),
			   INSN_CODE (insn),
			   INSN_BB (insn),
			   INSN_DEP_COUNT (insn),
			   INSN_PRIORITY (insn),
			   insn_cost (insn, 0, 0));

		  if (recog_memoized (insn) < 0)
		    fprintf (sched_dump, "nothing");
		  else
		    print_reservation (sched_dump, insn);
		}
	      else
		{
		  int unit = insn_unit (insn);
		  int range
		    = (unit < 0
		       || function_units[unit].blockage_range_function == 0
		       ? 0
		       : function_units[unit].blockage_range_function (insn));
		  fprintf (sched_dump,
			   ";;   %s%5d%6d%6d%6d%6d%6d  %3d -%3d   ",
			   (SCHED_GROUP_P (insn) ? "+" : " "),
			   INSN_UID (insn),
			   INSN_CODE (insn),
			   INSN_BB (insn),
			   INSN_DEP_COUNT (insn),
			   INSN_PRIORITY (insn),
			   insn_cost (insn, 0, 0),
			   (int) MIN_BLOCKAGE_COST (range),
			   (int) MAX_BLOCKAGE_COST (range));
		  insn_print_units (insn);
		}

	      fprintf (sched_dump, "\t: ");
	      for (link = INSN_DEPEND (insn); link; link = XEXP (link, 1))
		fprintf (sched_dump, "%d ", INSN_UID (XEXP (link, 0)));
	      fprintf (sched_dump, "\n");
	    }
	}
    }
  fprintf (sched_dump, "\n");
}

/* Schedule a region.  A region is either an inner loop, a loop-free
   subroutine, or a single basic block.  Each bb in the region is
   scheduled after its flow predecessors.  */

static void
schedule_region (rgn)
     int rgn;
{
  int bb;
  int rgn_n_insns = 0;
  int sched_rgn_n_insns = 0;

  /* Set variables for the current region.  */
  current_nr_blocks = RGN_NR_BLOCKS (rgn);
  current_blocks = RGN_BLOCKS (rgn);

  init_deps_global ();

  /* Initializations for region data dependence analyisis.  */
  bb_deps = (struct deps *) xmalloc (sizeof (struct deps) * current_nr_blocks);
  for (bb = 0; bb < current_nr_blocks; bb++)
    init_deps (bb_deps + bb);

  /* Compute LOG_LINKS.  */
  for (bb = 0; bb < current_nr_blocks; bb++)
    compute_block_backward_dependences (bb);

  /* Compute INSN_DEPEND.  */
  for (bb = current_nr_blocks - 1; bb >= 0; bb--)
    {
      rtx head, tail;
      get_block_head_tail (BB_TO_BLOCK (bb), &head, &tail);

      compute_forward_dependences (head, tail);
    }

  /* Set priorities.  */
  for (bb = 0; bb < current_nr_blocks; bb++)
    {
      rtx head, tail;
      get_block_head_tail (BB_TO_BLOCK (bb), &head, &tail);

      rgn_n_insns += set_priorities (head, tail);
    }

  /* Compute interblock info: probabilities, split-edges, dominators, etc.  */
  if (current_nr_blocks > 1)
    {
      int i;

      prob = (float *) xmalloc ((current_nr_blocks) * sizeof (float));

      dom = sbitmap_vector_alloc (current_nr_blocks, current_nr_blocks);
      sbitmap_vector_zero (dom, current_nr_blocks);
      /* Edge to bit.  */
      rgn_nr_edges = 0;
      edge_to_bit = (int *) xmalloc (nr_edges * sizeof (int));
      for (i = 1; i < nr_edges; i++)
	if (CONTAINING_RGN (FROM_BLOCK (i)) == rgn)
	  EDGE_TO_BIT (i) = rgn_nr_edges++;
      rgn_edges = (int *) xmalloc (rgn_nr_edges * sizeof (int));

      rgn_nr_edges = 0;
      for (i = 1; i < nr_edges; i++)
	if (CONTAINING_RGN (FROM_BLOCK (i)) == (rgn))
	  rgn_edges[rgn_nr_edges++] = i;

      /* Split edges.  */
      pot_split = sbitmap_vector_alloc (current_nr_blocks, rgn_nr_edges);
      sbitmap_vector_zero (pot_split, current_nr_blocks);
      ancestor_edges = sbitmap_vector_alloc (current_nr_blocks, rgn_nr_edges);
      sbitmap_vector_zero (ancestor_edges, current_nr_blocks);

      /* Compute probabilities, dominators, split_edges.  */
      for (bb = 0; bb < current_nr_blocks; bb++)
	compute_dom_prob_ps (bb);
    }

  /* Now we can schedule all blocks.  */
  for (bb = 0; bb < current_nr_blocks; bb++)
    {
      rtx head, tail;
      int b = BB_TO_BLOCK (bb);

      get_block_head_tail (b, &head, &tail);

      if (no_real_insns_p (head, tail))
	continue;

      current_sched_info->prev_head = PREV_INSN (head);
      current_sched_info->next_tail = NEXT_INSN (tail);

      if (write_symbols != NO_DEBUG)
	{
	  save_line_notes (b, head, tail);
	  rm_line_notes (head, tail);
	}

      /* rm_other_notes only removes notes which are _inside_ the
	 block---that is, it won't remove notes before the first real insn
 	 or after the last real insn of the block.  So if the first insn
	 has a REG_SAVE_NOTE which would otherwise be emitted before the
	 insn, it is redundant with the note before the start of the
	 block, and so we have to take it out.  */
      if (INSN_P (head))
	{
	  rtx note;

	  for (note = REG_NOTES (head); note; note = XEXP (note, 1))
	    if (REG_NOTE_KIND (note) == REG_SAVE_NOTE)
	      {
		remove_note (head, note);
		note = XEXP (note, 1);
		remove_note (head, note);
	      }
	}

      /* Remove remaining note insns from the block, save them in
	 note_list.  These notes are restored at the end of
	 schedule_block ().  */
      rm_other_notes (head, tail);

      target_bb = bb;

      current_sched_info->queue_must_finish_empty
	= current_nr_blocks > 1 && !flag_schedule_interblock;

      schedule_block (b, rgn_n_insns);
      sched_rgn_n_insns += sched_n_insns;

      /* Update target block boundaries.  */
      if (head == BLOCK_HEAD (b))
	BLOCK_HEAD (b) = current_sched_info->head;
      if (tail == BLOCK_END (b))
	BLOCK_END (b) = current_sched_info->tail;

      /* Clean up.  */
      if (current_nr_blocks > 1)
	{
	  free (candidate_table);
	  free (bblst_table);
	  free (bitlst_table);
	}
    }

  /* Sanity check: verify that all region insns were scheduled.  */
  if (sched_rgn_n_insns != rgn_n_insns)
    abort ();

  /* Restore line notes.  */
  if (write_symbols != NO_DEBUG)
    {
      for (bb = 0; bb < current_nr_blocks; bb++)
	{
	  rtx head, tail;
	  get_block_head_tail (BB_TO_BLOCK (bb), &head, &tail);
	  restore_line_notes (head, tail);
	}
    }

  /* Done with this region.  */
  free_pending_lists ();

  finish_deps_global ();

  free (bb_deps);

  if (current_nr_blocks > 1)
    {
      free (prob);
      sbitmap_vector_free (dom);
      sbitmap_vector_free (pot_split);
      sbitmap_vector_free (ancestor_edges);
      free (edge_to_bit);
      free (rgn_edges);
    }
}

/* Indexed by region, holds the number of death notes found in that region.
   Used for consistency checks.  */
static int *deaths_in_region;

/* Initialize data structures for region scheduling.  */

static void
init_regions ()
{
  sbitmap blocks;
  int rgn;

  nr_regions = 0;
  rgn_table = (region *) xmalloc ((n_basic_blocks) * sizeof (region));
  rgn_bb_table = (int *) xmalloc ((n_basic_blocks) * sizeof (int));
  block_to_bb = (int *) xmalloc ((last_basic_block) * sizeof (int));
  containing_rgn = (int *) xmalloc ((last_basic_block) * sizeof (int));

  /* Compute regions for scheduling.  */
  if (reload_completed
      || n_basic_blocks == 1
      || !flag_schedule_interblock)
    {
      find_single_block_region ();
    }
  else
    {
      /* Verify that a 'good' control flow graph can be built.  */
      if (is_cfg_nonregular ())
	{
	  find_single_block_region ();
	}
      else
	{
	  dominance_info dom;
	  struct edge_list *edge_list;

	  /* The scheduler runs after flow; therefore, we can't blindly call
	     back into find_basic_blocks since doing so could invalidate the
	     info in global_live_at_start.

	     Consider a block consisting entirely of dead stores; after life
	     analysis it would be a block of NOTE_INSN_DELETED notes.  If
	     we call find_basic_blocks again, then the block would be removed
	     entirely and invalidate our the register live information.

	     We could (should?) recompute register live information.  Doing
	     so may even be beneficial.  */
	  edge_list = create_edge_list ();

	  /* Compute the dominators and post dominators.  */
	  dom = calculate_dominance_info (CDI_DOMINATORS);

	  /* build_control_flow will return nonzero if it detects unreachable
	     blocks or any other irregularity with the cfg which prevents
	     cross block scheduling.  */
	  if (build_control_flow (edge_list) != 0)
	    find_single_block_region ();
	  else
	    find_rgns (edge_list, dom);

	  if (sched_verbose >= 3)
	    debug_regions ();

	  /* We are done with flow's edge list.  */
	  free_edge_list (edge_list);

	  /* For now.  This will move as more and more of haifa is converted
	     to using the cfg code in flow.c.  */
	  free_dominance_info (dom);
	}
    }


  if (CHECK_DEAD_NOTES)
    {
      blocks = sbitmap_alloc (last_basic_block);
      deaths_in_region = (int *) xmalloc (sizeof (int) * nr_regions);
      /* Remove all death notes from the subroutine.  */
      for (rgn = 0; rgn < nr_regions; rgn++)
	{
	  int b;

	  sbitmap_zero (blocks);
	  for (b = RGN_NR_BLOCKS (rgn) - 1; b >= 0; --b)
	    SET_BIT (blocks, rgn_bb_table[RGN_BLOCKS (rgn) + b]);

	  deaths_in_region[rgn] = count_or_remove_death_notes (blocks, 1);
	}
      sbitmap_free (blocks);
    }
  else
    count_or_remove_death_notes (NULL, 1);
}

/* The one entry point in this file.  DUMP_FILE is the dump file for
   this pass.  */

void
schedule_insns (dump_file)
     FILE *dump_file;
{
  sbitmap large_region_blocks, blocks;
  int rgn;
  int any_large_regions;
  basic_block bb;

  /* Taking care of this degenerate case makes the rest of
     this code simpler.  */
  if (n_basic_blocks == 0)
    return;

  nr_inter = 0;
  nr_spec = 0;

  sched_init (dump_file);

  init_regions ();

  current_sched_info = &region_sched_info;

  /* Schedule every region in the subroutine.  */
  for (rgn = 0; rgn < nr_regions; rgn++)
    schedule_region (rgn);

  /* Update life analysis for the subroutine.  Do single block regions
     first so that we can verify that live_at_start didn't change.  Then
     do all other blocks.  */
  /* ??? There is an outside possibility that update_life_info, or more
     to the point propagate_block, could get called with nonzero flags
     more than once for one basic block.  This would be kinda bad if it
     were to happen, since REG_INFO would be accumulated twice for the
     block, and we'd have twice the REG_DEAD notes.

     I'm fairly certain that this _shouldn't_ happen, since I don't think
     that live_at_start should change at region heads.  Not sure what the
     best way to test for this kind of thing...  */

  allocate_reg_life_data ();
  compute_bb_for_insn ();

  any_large_regions = 0;
  large_region_blocks = sbitmap_alloc (last_basic_block);
  sbitmap_zero (large_region_blocks);
  FOR_EACH_BB (bb)
    SET_BIT (large_region_blocks, bb->index);

  blocks = sbitmap_alloc (last_basic_block);
  sbitmap_zero (blocks);

  /* Update life information.  For regions consisting of multiple blocks
     we've possibly done interblock scheduling that affects global liveness.
     For regions consisting of single blocks we need to do only local
     liveness.  */
  for (rgn = 0; rgn < nr_regions; rgn++)
    if (RGN_NR_BLOCKS (rgn) > 1)
      any_large_regions = 1;
    else
      {
	SET_BIT (blocks, rgn_bb_table[RGN_BLOCKS (rgn)]);
	RESET_BIT (large_region_blocks, rgn_bb_table[RGN_BLOCKS (rgn)]);
      }

  /* Don't update reg info after reload, since that affects
     regs_ever_live, which should not change after reload.  */
  update_life_info (blocks, UPDATE_LIFE_LOCAL,
		    (reload_completed ? PROP_DEATH_NOTES
		     : PROP_DEATH_NOTES | PROP_REG_INFO));
  if (any_large_regions)
    {
      update_life_info (large_region_blocks, UPDATE_LIFE_GLOBAL,
			PROP_DEATH_NOTES | PROP_REG_INFO);
    }

  if (CHECK_DEAD_NOTES)
    {
      /* Verify the counts of basic block notes in single the basic block
         regions.  */
      for (rgn = 0; rgn < nr_regions; rgn++)
	if (RGN_NR_BLOCKS (rgn) == 1)
	  {
	    sbitmap_zero (blocks);
	    SET_BIT (blocks, rgn_bb_table[RGN_BLOCKS (rgn)]);

	    if (deaths_in_region[rgn]
		!= count_or_remove_death_notes (blocks, 0))
	      abort ();
	  }
      free (deaths_in_region);
    }

  /* Reposition the prologue and epilogue notes in case we moved the
     prologue/epilogue insns.  */
  if (reload_completed)
    reposition_prologue_and_epilogue_notes (get_insns ());

  /* Delete redundant line notes.  */
  if (write_symbols != NO_DEBUG)
    rm_redundant_line_notes ();

  if (sched_verbose)
    {
      if (reload_completed == 0 && flag_schedule_interblock)
	{
	  fprintf (sched_dump,
		   "\n;; Procedure interblock/speculative motions == %d/%d \n",
		   nr_inter, nr_spec);
	}
      else
	{
	  if (nr_inter > 0)
	    abort ();
	}
      fprintf (sched_dump, "\n\n");
    }

  /* Clean up.  */
  free (rgn_table);
  free (rgn_bb_table);
  free (block_to_bb);
  free (containing_rgn);

  sched_finish ();

  if (edge_table)
    {
      free (edge_table);
      edge_table = NULL;
    }

  if (in_edges)
    {
      free (in_edges);
      in_edges = NULL;
    }
  if (out_edges)
    {
      free (out_edges);
      out_edges = NULL;
    }

  sbitmap_free (blocks);
  sbitmap_free (large_region_blocks);
}
#endif
