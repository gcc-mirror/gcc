/* Instruction scheduling pass.
   Copyright (C) 1992, 93-98, 1999 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com) Enhanced by,
   and currently maintained by, Jim Wilson (wilson@cygnus.com)

   This file is part of GNU CC.

   GNU CC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GNU CC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to the Free
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */


/* Instruction scheduling pass.

   This pass implements list scheduling within basic blocks.  It is
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
   The scheduler works as follows:

   We compute insn priorities based on data dependencies.  Flow
   analysis only creates a fraction of the data-dependencies we must
   observe: namely, only those dependencies which the combiner can be
   expected to use.  For this pass, we must therefore create the
   remaining dependencies we need to observe: register dependencies,
   memory dependencies, dependencies to keep function calls in order,
   and the dependence between a conditional branch and the setting of
   condition codes are all dealt with here.

   The scheduler first traverses the data flow graph, starting with
   the last instruction, and proceeding to the first, assigning values
   to insn_priority as it goes.  This sorts the instructions
   topologically by data dependence.

   Once priorities have been established, we order the insns using
   list scheduling.  This works as follows: starting with a list of
   all the ready insns, and sorted according to priority number, we
   schedule the insn from the end of the list by placing its
   predecessors in the list according to their priority order.  We
   consider this insn scheduled by setting the pointer to the "end" of
   the list to point to the previous insn.  When an insn has no
   predecessors, we either queue it until sufficient time has elapsed
   or add it to the ready list.  As the instructions are scheduled or
   when stalls are introduced, the queue advances and dumps insns into
   the ready list.  When all insns down to the lowest priority have
   been scheduled, the critical path of the basic block has been made
   as short as possible.  The remaining insns are then scheduled in
   remaining slots.

   Function unit conflicts are resolved during forward list scheduling
   by tracking the time when each insn is committed to the schedule
   and from that, the time the function units it uses must be free.
   As insns on the ready list are considered for scheduling, those
   that would result in a blockage of the already committed insns are
   queued until no blockage will result.

   The following list shows the order in which we want to break ties
   among insns in the ready list:

   1.  choose insn with the longest path to end of bb, ties
   broken by
   2.  choose insn with least contribution to register pressure,
   ties broken by
   3.  prefer in-block upon interblock motion, ties broken by
   4.  prefer useful upon speculative motion, ties broken by
   5.  choose insn with largest control flow probability, ties
   broken by
   6.  choose insn with the least dependences upon the previously
   scheduled insn, or finally
   7   choose the insn which has the most insns dependent on it.
   8.  choose insn with lowest UID.

   Memory references complicate matters.  Only if we can be certain
   that memory references are not part of the data dependency graph
   (via true, anti, or output dependence), can we move operations past
   memory references.  To first approximation, reads can be done
   independently, while writes introduce dependencies.  Better
   approximations will yield fewer dependencies.

   Before reload, an extended analysis of interblock data dependences
   is required for interblock scheduling.  This is performed in
   compute_block_backward_dependences ().

   Dependencies set up by memory references are treated in exactly the
   same way as other dependencies, by using LOG_LINKS backward
   dependences.  LOG_LINKS are translated into INSN_DEPEND forward
   dependences for the purpose of forward list scheduling.

   Having optimized the critical path, we may have also unduly
   extended the lifetimes of some registers.  If an operation requires
   that constants be loaded into registers, it is certainly desirable
   to load those constants as early as necessary, but no earlier.
   I.e., it will not do to load up a bunch of registers at the
   beginning of a basic block only to use them at the end, if they
   could be loaded later, since this may result in excessive register
   utilization.

   Note that since branches are never in basic blocks, but only end
   basic blocks, this pass will not move branches.  But that is ok,
   since we can use GNU's delayed branch scheduling pass to take care
   of this case.

   Also note that no further optimizations based on algebraic
   identities are performed, so this pass would be a good one to
   perform instruction splitting, such as breaking up a multiply
   instruction into shifts and adds where that is profitable.

   Given the memory aliasing analysis that this pass should perform,
   it should be possible to remove redundant stores to memory, and to
   load values from registers instead of hitting memory.

   Before reload, speculative insns are moved only if a 'proof' exists
   that no exception will be caused by this, and if no live registers
   exist that inhibit the motion (live registers constraints are not
   represented by data dependence edges).

   This pass must update information that subsequent passes expect to
   be correct.  Namely: reg_n_refs, reg_n_sets, reg_n_deaths,
   reg_n_calls_crossed, and reg_live_length.  Also, BLOCK_HEAD,
   BLOCK_END.

   The information in the line number notes is carefully retained by
   this pass.  Notes that refer to the starting and ending of
   exception regions are also carefully retained by this pass.  All
   other NOTE insns are grouped in their same relative order at the
   beginning of basic blocks and regions that have been scheduled.

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
#include "basic-block.h"
#include "regs.h"
#include "function.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "insn-config.h"
#include "insn-attr.h"
#include "except.h"
#include "toplev.h"
#include "recog.h"

extern char *reg_known_equiv_p;
extern rtx *reg_known_value;

#ifdef INSN_SCHEDULING

/* target_units bitmask has 1 for each unit in the cpu.  It should be
   possible to compute this variable from the machine description.
   But currently it is computed by examining the insn list.  Since
   this is only needed for visualization, it seems an acceptable
   solution.  (For understanding the mapping of bits to units, see
   definition of function_units[] in "insn-attrtab.c".)  */

static int target_units = 0;

/* issue_rate is the number of insns that can be scheduled in the same
   machine cycle.  It can be defined in the config/mach/mach.h file,
   otherwise we set it to 1.  */

static int issue_rate;

#ifndef ISSUE_RATE
#define ISSUE_RATE 1
#endif

/* sched-verbose controls the amount of debugging output the
   scheduler prints.  It is controlled by -fsched-verbose-N:
   N>0 and no -DSR : the output is directed to stderr.
   N>=10 will direct the printouts to stderr (regardless of -dSR).
   N=1: same as -dSR.
   N=2: bb's probabilities, detailed ready list info, unit/insn info.
   N=3: rtl at abort point, control-flow, regions info.
   N=5: dependences info.  */

#define MAX_RGN_BLOCKS 10
#define MAX_RGN_INSNS 100

static int sched_verbose_param = 0;
static int sched_verbose = 0;

/* nr_inter/spec counts interblock/speculative motion for the function.  */
static int nr_inter, nr_spec;


/* Debugging file.  All printouts are sent to dump, which is always set,
   either to stderr, or to the dump listing file (-dRS).  */
static FILE *dump = 0;

/* fix_sched_param() is called from toplev.c upon detection
   of the -fsched-***-N options.  */

void
fix_sched_param (param, val)
     const char *param, *val;
{
  if (!strcmp (param, "verbose"))
    sched_verbose_param = atoi (val);
  else
    warning ("fix_sched_param: unknown param: %s", param);
}

/* Describe state of dependencies used during sched_analyze phase.  */
struct deps
{
  /* The *_insns and *_mems are paired lists.  Each pending memory operation
     will have a pointer to the MEM rtx on one list and a pointer to the
     containing insn on the other list in the same place in the list.  */

  /* We can't use add_dependence like the old code did, because a single insn
     may have multiple memory accesses, and hence needs to be on the list
     once for each memory access.  Add_dependence won't let you add an insn
     to a list more than once.  */

  /* An INSN_LIST containing all insns with pending read operations.  */
  rtx pending_read_insns;

  /* An EXPR_LIST containing all MEM rtx's which are pending reads.  */
  rtx pending_read_mems;

  /* An INSN_LIST containing all insns with pending write operations.  */
  rtx pending_write_insns;

  /* An EXPR_LIST containing all MEM rtx's which are pending writes.  */
  rtx pending_write_mems;

  /* Indicates the combined length of the two pending lists.  We must prevent
     these lists from ever growing too large since the number of dependencies
     produced is at least O(N*N), and execution time is at least O(4*N*N), as
     a function of the length of these pending lists.  */
  int pending_lists_length;

  /* The last insn upon which all memory references must depend.
     This is an insn which flushed the pending lists, creating a dependency
     between it and all previously pending memory references.  This creates
     a barrier (or a checkpoint) which no memory reference is allowed to cross.

     This includes all non constant CALL_INSNs.  When we do interprocedural
     alias analysis, this restriction can be relaxed.
     This may also be an INSN that writes memory if the pending lists grow
     too large.  */
  rtx last_pending_memory_flush;

  /* The last function call we have seen.  All hard regs, and, of course,
     the last function call, must depend on this.  */
  rtx last_function_call;

  /* The LOG_LINKS field of this is a list of insns which use a pseudo register
     that does not already cross a call.  We create dependencies between each
     of those insn and the next call insn, to ensure that they won't cross a call
     after scheduling is done.  */
  rtx sched_before_next_call;

  /* Element N is the next insn that sets (hard or pseudo) register
     N within the current basic block; or zero, if there is no
     such insn.  Needed for new registers which may be introduced
     by splitting insns.  */
  rtx *reg_last_uses;
  rtx *reg_last_sets;
  rtx *reg_last_clobbers;
};

static regset reg_pending_sets;
static regset reg_pending_clobbers;
static int reg_pending_sets_all;

/* To speed up the test for duplicate dependency links we keep a record
   of true dependencies created by add_dependence when the average number
   of instructions in a basic block is very large.

   Studies have shown that there is typically around 5 instructions between
   branches for typical C code.  So we can make a guess that the average
   basic block is approximately 5 instructions long; we will choose 100X
   the average size as a very large basic block.
  
   Each insn has an associated bitmap for its dependencies.  Each bitmap
   has enough entries to represent a dependency on any other insn in the
   insn chain.  */
static sbitmap *true_dependency_cache;

/* Indexed by INSN_UID, the collection of all data associated with
   a single instruction.  */

struct haifa_insn_data
{
  /* A list of insns which depend on the instruction.  Unlike LOG_LINKS,
     it represents forward dependancies.  */
  rtx depend;

  /* The line number note in effect for each insn.  For line number 
     notes, this indicates whether the note may be reused.  */
  rtx line_note;

  /* Logical uid gives the original ordering of the insns.  */
  int luid;

  /* A priority for each insn.  */
  int priority;

  /* The number of incoming edges in the forward dependency graph.
     As scheduling proceds, counts are decreased.  An insn moves to
     the ready queue when its counter reaches zero.  */
  int dep_count;

  /* An encoding of the blockage range function.  Both unit and range
     are coded.  */
  unsigned int blockage;

  /* Number of instructions referring to this insn.  */
  int ref_count;

  /* The minimum clock tick at which the insn becomes ready.  This is
     used to note timing constraints for the insns in the pending list.  */
  int tick;

  short cost;

  /* An encoding of the function units used.  */
  short units;

  /* This weight is an estimation of the insn's contribution to
     register pressure.  */
  short reg_weight;

  /* Some insns (e.g. call) are not allowed to move across blocks.  */
  unsigned int cant_move : 1;

  /* Set if there's DEF-USE dependance between some speculatively
     moved load insn and this one.  */
  unsigned int fed_by_spec_load : 1;
  unsigned int is_load_insn : 1;
};

static struct haifa_insn_data *h_i_d;

#define INSN_DEPEND(INSN)	(h_i_d[INSN_UID (INSN)].depend)
#define INSN_LUID(INSN)		(h_i_d[INSN_UID (INSN)].luid)
#define INSN_PRIORITY(INSN)	(h_i_d[INSN_UID (INSN)].priority)
#define INSN_DEP_COUNT(INSN)	(h_i_d[INSN_UID (INSN)].dep_count)
#define INSN_COST(INSN)		(h_i_d[INSN_UID (INSN)].cost)
#define INSN_UNIT(INSN)		(h_i_d[INSN_UID (INSN)].units)
#define INSN_REG_WEIGHT(INSN)	(h_i_d[INSN_UID (INSN)].reg_weight)

#define INSN_BLOCKAGE(INSN)	(h_i_d[INSN_UID (INSN)].blockage)
#define UNIT_BITS		5
#define BLOCKAGE_MASK		((1 << BLOCKAGE_BITS) - 1)
#define ENCODE_BLOCKAGE(U, R)			\
  (((U) << BLOCKAGE_BITS			\
    | MIN_BLOCKAGE_COST (R)) << BLOCKAGE_BITS	\
   | MAX_BLOCKAGE_COST (R))
#define UNIT_BLOCKED(B)		((B) >> (2 * BLOCKAGE_BITS))
#define BLOCKAGE_RANGE(B)                                                \
  (((((B) >> BLOCKAGE_BITS) & BLOCKAGE_MASK) << (HOST_BITS_PER_INT / 2)) \
   | ((B) & BLOCKAGE_MASK))

/* Encodings of the `<name>_unit_blockage_range' function.  */
#define MIN_BLOCKAGE_COST(R) ((R) >> (HOST_BITS_PER_INT / 2))
#define MAX_BLOCKAGE_COST(R) ((R) & ((1 << (HOST_BITS_PER_INT / 2)) - 1))

#define DONE_PRIORITY	-1
#define MAX_PRIORITY	0x7fffffff
#define TAIL_PRIORITY	0x7ffffffe
#define LAUNCH_PRIORITY	0x7f000001
#define DONE_PRIORITY_P(INSN) (INSN_PRIORITY (INSN) < 0)
#define LOW_PRIORITY_P(INSN) ((INSN_PRIORITY (INSN) & 0x7f000000) == 0)

#define INSN_REF_COUNT(INSN)	(h_i_d[INSN_UID (INSN)].ref_count)
#define LINE_NOTE(INSN)		(h_i_d[INSN_UID (INSN)].line_note)
#define INSN_TICK(INSN)		(h_i_d[INSN_UID (INSN)].tick)
#define CANT_MOVE(insn)		(h_i_d[INSN_UID (insn)].cant_move)
#define FED_BY_SPEC_LOAD(insn)	(h_i_d[INSN_UID (insn)].fed_by_spec_load)
#define IS_LOAD_INSN(insn)	(h_i_d[INSN_UID (insn)].is_load_insn)

/* Vector indexed by basic block number giving the starting line-number
   for each basic block.  */
static rtx *line_note_head;

/* List of important notes we must keep around.  This is a pointer to the
   last element in the list.  */
static rtx note_list;

/* Queues, etc.  */

/* An instruction is ready to be scheduled when all insns preceding it
   have already been scheduled.  It is important to ensure that all
   insns which use its result will not be executed until its result
   has been computed.  An insn is maintained in one of four structures:

   (P) the "Pending" set of insns which cannot be scheduled until
   their dependencies have been satisfied.
   (Q) the "Queued" set of insns that can be scheduled when sufficient
   time has passed.
   (R) the "Ready" list of unscheduled, uncommitted insns.
   (S) the "Scheduled" list of insns.

   Initially, all insns are either "Pending" or "Ready" depending on
   whether their dependencies are satisfied.

   Insns move from the "Ready" list to the "Scheduled" list as they
   are committed to the schedule.  As this occurs, the insns in the
   "Pending" list have their dependencies satisfied and move to either
   the "Ready" list or the "Queued" set depending on whether
   sufficient time has passed to make them ready.  As time passes,
   insns move from the "Queued" set to the "Ready" list.  Insns may
   move from the "Ready" list to the "Queued" set if they are blocked
   due to a function unit conflict.

   The "Pending" list (P) are the insns in the INSN_DEPEND of the unscheduled
   insns, i.e., those that are ready, queued, and pending.
   The "Queued" set (Q) is implemented by the variable `insn_queue'.
   The "Ready" list (R) is implemented by the variables `ready' and
   `n_ready'.
   The "Scheduled" list (S) is the new insn chain built by this pass.

   The transition (R->S) is implemented in the scheduling loop in
   `schedule_block' when the best insn to schedule is chosen.
   The transition (R->Q) is implemented in `queue_insn' when an
   insn is found to have a function unit conflict with the already
   committed insns.
   The transitions (P->R and P->Q) are implemented in `schedule_insn' as
   insns move from the ready list to the scheduled list.
   The transition (Q->R) is implemented in 'queue_to_insn' as time
   passes or stalls are introduced.  */

/* Implement a circular buffer to delay instructions until sufficient
   time has passed.  INSN_QUEUE_SIZE is a power of two larger than
   MAX_BLOCKAGE and MAX_READY_COST computed by genattr.c.  This is the
   longest time an isnsn may be queued.  */
static rtx insn_queue[INSN_QUEUE_SIZE];
static int q_ptr = 0;
static int q_size = 0;
#define NEXT_Q(X) (((X)+1) & (INSN_QUEUE_SIZE-1))
#define NEXT_Q_AFTER(X, C) (((X)+C) & (INSN_QUEUE_SIZE-1))

/* Forward declarations.  */
static void add_dependence PROTO ((rtx, rtx, enum reg_note));
#ifdef HAVE_cc0
static void remove_dependence PROTO ((rtx, rtx));
#endif
static rtx find_insn_list PROTO ((rtx, rtx));
static int insn_unit PROTO ((rtx));
static unsigned int blockage_range PROTO ((int, rtx));
static void clear_units PROTO ((void));
static int actual_hazard_this_instance PROTO ((int, int, rtx, int, int));
static void schedule_unit PROTO ((int, rtx, int));
static int actual_hazard PROTO ((int, rtx, int, int));
static int potential_hazard PROTO ((int, rtx, int));
static int insn_cost PROTO ((rtx, rtx, rtx));
static int priority PROTO ((rtx));
static void free_pending_lists PROTO ((void));
static void add_insn_mem_dependence PROTO ((struct deps *, rtx *, rtx *, rtx,
					    rtx));
static void flush_pending_lists PROTO ((struct deps *, rtx, int));
static void sched_analyze_1 PROTO ((struct deps *, rtx, rtx));
static void sched_analyze_2 PROTO ((struct deps *, rtx, rtx));
static void sched_analyze_insn PROTO ((struct deps *, rtx, rtx, rtx));
static void sched_analyze PROTO ((struct deps *, rtx, rtx));
static int rank_for_schedule PROTO ((const PTR, const PTR));
static void swap_sort PROTO ((rtx *, int));
static void queue_insn PROTO ((rtx, int));
static int schedule_insn PROTO ((rtx, rtx *, int, int));
static void find_insn_reg_weight PROTO ((int));
static int schedule_block PROTO ((int, int));
static char *safe_concat PROTO ((char *, char *, const char *));
static int insn_issue_delay PROTO ((rtx));
static void adjust_priority PROTO ((rtx));

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



static int is_cfg_nonregular PROTO ((void));
static int build_control_flow PROTO ((struct edge_list *));
static void new_edge PROTO ((int, int));


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

void debug_regions PROTO ((void));
static void find_single_block_region PROTO ((void));
static void find_rgns PROTO ((struct edge_list *, sbitmap *));
static int too_large PROTO ((int, int *, int *));

extern void debug_live PROTO ((int, int));

/* Blocks of the current region being scheduled.  */
static int current_nr_blocks;
static int current_blocks;

/* The mapping from bb to block.  */
#define BB_TO_BLOCK(bb) (rgn_bb_table[current_blocks + (bb)])


/* Bit vectors and bitset operations are needed for computations on
   the control flow graph.  */

typedef unsigned HOST_WIDE_INT *bitset;
typedef struct
  {
    int *first_member;		/* Pointer to the list start in bitlst_table.  */
    int nr_members;		/* The number of members of the bit list.  */
  }
bitlst;

static int bitlst_table_last;
static int bitlst_table_size;
static int *bitlst_table;

static char bitset_member PROTO ((bitset, int, int));
static void extract_bitlst PROTO ((bitset, int, bitlst *));

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
static void split_edges PROTO ((int, int, edgelst *));
static void compute_trg_info PROTO ((int));
void debug_candidate PROTO ((int));
void debug_candidates PROTO ((int));


/* Bit-set of bbs, where bit 'i' stands for bb 'i'.  */
typedef bitset bbset;

/* Number of words of the bbset.  */
static int bbset_size;

/* Dominators array: dom[i] contains the bbset of dominators of
   bb i in the region.  */
static bbset *dom;

/* bb 0 is the only region entry.  */
#define IS_RGN_ENTRY(bb) (!bb)

/* Is bb_src dominated by bb_trg.  */
#define IS_DOMINATED(bb_src, bb_trg)                                 \
( bitset_member (dom[bb_src], bb_trg, bbset_size) )

/* Probability: Prob[i] is a float in [0, 1] which is the probability
   of bb i relative to the region entry.  */
static float *prob;

/* The probability of bb_src, relative to bb_trg.  Note, that while the
   'prob[bb]' is a float in [0, 1], this macro returns an integer
   in [0, 100].  */
#define GET_SRC_PROB(bb_src, bb_trg) ((int) (100.0 * (prob[bb_src] / \
						      prob[bb_trg])))

/* Bit-set of edges, where bit i stands for edge i.  */
typedef bitset edgeset;

/* Number of edges in the region.  */
static int rgn_nr_edges;

/* Array of size rgn_nr_edges.  */
static int *rgn_edges;

/* Number of words in an edgeset.  */
static int edgeset_size;

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

static void compute_dom_prob_ps PROTO ((int));

#define ABS_VALUE(x) (((x)<0)?(-(x)):(x))
#define INSN_PROBABILITY(INSN) (SRC_PROB (BLOCK_TO_BB (BLOCK_NUM (INSN))))
#define IS_SPECULATIVE_INSN(INSN) (IS_SPECULATIVE (BLOCK_TO_BB (BLOCK_NUM (INSN))))
#define INSN_BB(INSN) (BLOCK_TO_BB (BLOCK_NUM (INSN)))

/* Parameters affecting the decision of rank_for_schedule().  */
#define MIN_DIFF_PRIORITY 2
#define MIN_PROBABILITY 40
#define MIN_PROB_DIFF 10

/* Speculative scheduling functions.  */
static int check_live_1 PROTO ((int, rtx));
static void update_live_1 PROTO ((int, rtx));
static int check_live PROTO ((rtx, int));
static void update_live PROTO ((rtx, int));
static void set_spec_fed PROTO ((rtx));
static int is_pfree PROTO ((rtx, int, int));
static int find_conditional_protection PROTO ((rtx, int));
static int is_conditionally_protected PROTO ((rtx, int, int));
static int may_trap_exp PROTO ((rtx, int));
static int haifa_classify_insn PROTO ((rtx));
static int is_prisky PROTO ((rtx, int, int));
static int is_exception_free PROTO ((rtx, int, int));

static char find_insn_mem_list PROTO ((rtx, rtx, rtx, rtx));
static void compute_block_forward_dependences PROTO ((int));
static void add_branch_dependences PROTO ((rtx, rtx));
static void compute_block_backward_dependences PROTO ((int));
void debug_dependencies PROTO ((void));

/* Notes handling mechanism:
   =========================
   Generally, NOTES are saved before scheduling and restored after scheduling.
   The scheduler distinguishes between three types of notes:

   (1) LINE_NUMBER notes, generated and used for debugging.  Here,
   before scheduling a region, a pointer to the LINE_NUMBER note is
   added to the insn following it (in save_line_notes()), and the note
   is removed (in rm_line_notes() and unlink_line_notes()).  After
   scheduling the region, this pointer is used for regeneration of
   the LINE_NUMBER note (in restore_line_notes()).

   (2) LOOP_BEGIN, LOOP_END, SETJMP, EHREGION_BEG, EHREGION_END notes:
   Before scheduling a region, a pointer to the note is added to the insn
   that follows or precedes it.  (This happens as part of the data dependence
   computation).  After scheduling an insn, the pointer contained in it is
   used for regenerating the corresponding note (in reemit_notes).

   (3) All other notes (e.g. INSN_DELETED):  Before scheduling a block,
   these notes are put in a list (in rm_other_notes() and
   unlink_other_notes ()).  After scheduling the block, these notes are
   inserted at the beginning of the block (in schedule_block()).  */

static rtx unlink_other_notes PROTO ((rtx, rtx));
static rtx unlink_line_notes PROTO ((rtx, rtx));
static void rm_line_notes PROTO ((int));
static void save_line_notes PROTO ((int));
static void restore_line_notes PROTO ((int));
static void rm_redundant_line_notes PROTO ((void));
static void rm_other_notes PROTO ((rtx, rtx));
static rtx reemit_notes PROTO ((rtx, rtx));

static void get_block_head_tail PROTO ((int, rtx *, rtx *));
static void get_bb_head_tail PROTO ((int, rtx *, rtx *));

static int queue_to_ready PROTO ((rtx [], int));

static void debug_ready_list PROTO ((rtx[], int));
static void init_target_units PROTO ((void));
static void insn_print_units PROTO ((rtx));
static int get_visual_tbl_length PROTO ((void));
static void init_block_visualization PROTO ((void));
static void print_block_visualization PROTO ((int, const char *));
static void visualize_scheduled_insns PROTO ((int, int));
static void visualize_no_unit PROTO ((rtx));
static void visualize_stall_cycles PROTO ((int, int));
static void print_exp PROTO ((char *, rtx, int));
static void print_value PROTO ((char *, rtx, int));
static void print_pattern PROTO ((char *, rtx, int));
static void print_insn PROTO ((char *, rtx, int));
void debug_reg_vector PROTO ((regset));

static rtx move_insn1 PROTO ((rtx, rtx));
static rtx move_insn PROTO ((rtx, rtx));
static rtx group_leader PROTO ((rtx));
static int set_priorities PROTO ((int));
static void init_deps PROTO ((struct deps *));
static void schedule_region PROTO ((int));

#endif /* INSN_SCHEDULING */

#define SIZE_FOR_MODE(X) (GET_MODE_SIZE (GET_MODE (X)))

/* Add ELEM wrapped in an INSN_LIST with reg note kind DEP_TYPE to the
   LOG_LINKS of INSN, if not already there.  DEP_TYPE indicates the type
   of dependence that this link represents.  */

static void
add_dependence (insn, elem, dep_type)
     rtx insn;
     rtx elem;
     enum reg_note dep_type;
{
  rtx link, next;

  /* Don't depend an insn on itself.  */
  if (insn == elem)
    return;

  /* We can get a dependency on deleted insns due to optimizations in
     the register allocation and reloading or due to splitting.  Any
     such dependency is useless and can be ignored.  */
  if (GET_CODE (elem) == NOTE)
    return;
	
  /* If elem is part of a sequence that must be scheduled together, then
     make the dependence point to the last insn of the sequence.
     When HAVE_cc0, it is possible for NOTEs to exist between users and
     setters of the condition codes, so we must skip past notes here.
     Otherwise, NOTEs are impossible here.  */

  next = NEXT_INSN (elem);

#ifdef HAVE_cc0
  while (next && GET_CODE (next) == NOTE)
    next = NEXT_INSN (next);
#endif

  if (next && SCHED_GROUP_P (next)
      && GET_CODE (next) != CODE_LABEL)
    {
      /* Notes will never intervene here though, so don't bother checking
         for them.  */
      /* We must reject CODE_LABELs, so that we don't get confused by one
         that has LABEL_PRESERVE_P set, which is represented by the same
         bit in the rtl as SCHED_GROUP_P.  A CODE_LABEL can never be
         SCHED_GROUP_P.  */
      while (NEXT_INSN (next) && SCHED_GROUP_P (NEXT_INSN (next))
	     && GET_CODE (NEXT_INSN (next)) != CODE_LABEL)
	next = NEXT_INSN (next);

      /* Again, don't depend an insn on itself.  */
      if (insn == next)
	return;

      /* Make the dependence to NEXT, the last insn of the group, instead
         of the original ELEM.  */
      elem = next;
    }

#ifdef INSN_SCHEDULING
  /* (This code is guarded by INSN_SCHEDULING, otherwise INSN_BB is undefined.)
     No need for interblock dependences with calls, since
     calls are not moved between blocks.   Note: the edge where
     elem is a CALL is still required.  */
  if (GET_CODE (insn) == CALL_INSN
      && (INSN_BB (elem) != INSN_BB (insn)))
    return;


  /* If we already have a true dependency for ELEM, then we do not
     need to do anything.  Avoiding the list walk below can cut
     compile times dramatically for some code.  */
  if (true_dependency_cache
      && TEST_BIT (true_dependency_cache[INSN_LUID (insn)], INSN_LUID (elem)))
    return;
#endif

  /* Check that we don't already have this dependence.  */
  for (link = LOG_LINKS (insn); link; link = XEXP (link, 1))
    if (XEXP (link, 0) == elem)
      {
	/* If this is a more restrictive type of dependence than the existing
	   one, then change the existing dependence to this type.  */
	if ((int) dep_type < (int) REG_NOTE_KIND (link))
	  PUT_REG_NOTE_KIND (link, dep_type);

#ifdef INSN_SCHEDULING
	/* If we are adding a true dependency to INSN's LOG_LINKs, then
	   note that in the bitmap cache of true dependency information.  */
	if ((int)dep_type == 0 && true_dependency_cache)
	  SET_BIT (true_dependency_cache[INSN_LUID (insn)], INSN_LUID (elem));
#endif
	return;
      }
  /* Might want to check one level of transitivity to save conses.  */

  link = alloc_INSN_LIST (elem, LOG_LINKS (insn));
  LOG_LINKS (insn) = link;

  /* Insn dependency, not data dependency.  */
  PUT_REG_NOTE_KIND (link, dep_type);

#ifdef INSN_SCHEDULING
  /* If we are adding a true dependency to INSN's LOG_LINKs, then
     note that in the bitmap cache of true dependency information.  */
  if ((int)dep_type == 0 && true_dependency_cache)
    SET_BIT (true_dependency_cache[INSN_LUID (insn)], INSN_LUID (elem));
#endif
}

#ifdef HAVE_cc0
/* Remove ELEM wrapped in an INSN_LIST from the LOG_LINKS
   of INSN.  Abort if not found.  */

static void
remove_dependence (insn, elem)
     rtx insn;
     rtx elem;
{
  rtx prev, link, next;
  int found = 0;

  for (prev = 0, link = LOG_LINKS (insn); link; link = next)
    {
      next = XEXP (link, 1);
      if (XEXP (link, 0) == elem)
	{
	  if (prev)
	    XEXP (prev, 1) = next;
	  else
	    LOG_LINKS (insn) = next;

#ifdef INSN_SCHEDULING
	  /* If we are removing a true dependency from the LOG_LINKS list,
	     make sure to remove it from the cache too.  */
	  if (REG_NOTE_KIND (link) == 0 && true_dependency_cache)
	    RESET_BIT (true_dependency_cache[INSN_LUID (insn)],
		       INSN_LUID (elem));
#endif

	  free_INSN_LIST_node (link);

	  found = 1;
	}
      else
	prev = link;
    }

  if (!found)
    abort ();
  return;
}
#endif /* HAVE_cc0 */

#ifndef INSN_SCHEDULING
void
schedule_insns (dump_file)
     FILE *dump_file;
{
}
#else
#ifndef __GNUC__
#define __inline
#endif

#ifndef HAIFA_INLINE
#define HAIFA_INLINE __inline
#endif

/* Computation of memory dependencies.  */

/* Data structures for the computation of data dependences in a regions.  We
   keep one mem_deps structure for every basic block.  Before analyzing the
   data dependences for a bb, its variables are initialized as a function of
   the variables of its predecessors.  When the analysis for a bb completes,
   we save the contents to the corresponding bb_mem_deps[bb] variable.  */

static struct deps *bb_deps;

/* Pointer to the last instruction scheduled.  Used by rank_for_schedule,
   so that insns independent of the last scheduled insn will be preferred
   over dependent instructions.  */

static rtx last_scheduled_insn;

/* Functions for construction of the control flow graph.  */

/* Return 1 if control flow graph should not be constructed, 0 otherwise.

   We decide not to build the control flow graph if there is possibly more
   than one entry to the function, if computed branches exist, of if we
   have nonlocal gotos.  */

static int
is_cfg_nonregular ()
{
  int b;
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
  if (exception_handler_labels)
    return 1;

  /* If we have non-jumping insns which refer to labels, then we consider
     the cfg not well structured.  */
  /* Check for labels referred to other thn by jumps.  */
  for (b = 0; b < n_basic_blocks; b++)
    for (insn = BLOCK_HEAD (b);; insn = NEXT_INSN (insn))
      {
	code = GET_CODE (insn);
	if (GET_RTX_CLASS (code) == 'i')
	  {
	    rtx note;

	    for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
	      if (REG_NOTE_KIND (note) == REG_LABEL)
		return 1;
	  }

	if (insn == BLOCK_END (b))
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

  /* This already accounts for entry/exit edges.  */
  num_edges = NUM_EDGES (edge_list);

  /* Unreachable loops with more than one basic block are detected
     during the DFS traversal in find_rgns.

     Unreachable loops with a single block are detected here.  This
     test is redundant with the one in find_rgns, but it's much
    cheaper to go ahead and catch the trivial case here.  */
  unreachable = 0;
  for (i = 0; i < n_basic_blocks; i++)
    {
      basic_block b = BASIC_BLOCK (i);

      if (b->pred == NULL
	  || (b->pred->src == b
	      && b->pred->pred_next == NULL))
	unreachable = 1;
    }

  /* ??? We can kill these soon.  */
  in_edges = (int *) xcalloc (n_basic_blocks, sizeof (int));
  out_edges = (int *) xcalloc (n_basic_blocks, sizeof (int));
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


/* BITSET macros for operations on the control flow graph.  */

/* Compute bitwise union of two bitsets.  */
#define BITSET_UNION(set1, set2, len)                                \
do { register bitset tp = set1, sp = set2;                           \
     register int i;                                                 \
     for (i = 0; i < len; i++)                                       \
       *(tp++) |= *(sp++); } while (0)

/* Compute bitwise intersection of two bitsets.  */
#define BITSET_INTER(set1, set2, len)                                \
do { register bitset tp = set1, sp = set2;                           \
     register int i;                                                 \
     for (i = 0; i < len; i++)                                       \
       *(tp++) &= *(sp++); } while (0)

/* Compute bitwise difference of two bitsets.  */
#define BITSET_DIFFER(set1, set2, len)                               \
do { register bitset tp = set1, sp = set2;                           \
     register int i;                                                 \
     for (i = 0; i < len; i++)                                       \
       *(tp++) &= ~*(sp++); } while (0)

/* Inverts every bit of bitset 'set'.  */
#define BITSET_INVERT(set, len)                                      \
do { register bitset tmpset = set;                                   \
     register int i;                                                 \
     for (i = 0; i < len; i++, tmpset++)                             \
       *tmpset = ~*tmpset; } while (0)

/* Turn on the index'th bit in bitset set.  */
#define BITSET_ADD(set, index, len)                                  \
{                                                                    \
  if (index >= HOST_BITS_PER_WIDE_INT * len)                         \
    abort ();                                                        \
  else                                                               \
    set[index/HOST_BITS_PER_WIDE_INT] |=			     \
      1 << (index % HOST_BITS_PER_WIDE_INT);                         \
}

/* Turn off the index'th bit in set.  */
#define BITSET_REMOVE(set, index, len)                               \
{                                                                    \
  if (index >= HOST_BITS_PER_WIDE_INT * len)                         \
    abort ();                                                        \
  else                                                               \
    set[index/HOST_BITS_PER_WIDE_INT] &=			     \
      ~(1 << (index%HOST_BITS_PER_WIDE_INT));                        \
}


/* Check if the index'th bit in bitset set is on.  */

static char
bitset_member (set, index, len)
     bitset set;
     int index, len;
{
  if (index >= HOST_BITS_PER_WIDE_INT * len)
    abort ();
  return (set[index / HOST_BITS_PER_WIDE_INT] &
	  1 << (index % HOST_BITS_PER_WIDE_INT)) ? 1 : 0;
}


/* Translate a bit-set SET to a list BL of the bit-set members.  */

static void
extract_bitlst (set, len, bl)
     bitset set;
     int len;
     bitlst *bl;
{
  int i, j, offset;
  unsigned HOST_WIDE_INT word;

  /* bblst table space is reused in each call to extract_bitlst.  */
  bitlst_table_last = 0;

  bl->first_member = &bitlst_table[bitlst_table_last];
  bl->nr_members = 0;

  for (i = 0; i < len; i++)
    {
      word = set[i];
      offset = i * HOST_BITS_PER_WIDE_INT;
      for (j = 0; word; j++)
	{
	  if (word & 1)
	    {
	      bitlst_table[bitlst_table_last++] = offset;
	      (bl->nr_members)++;
	    }
	  word >>= 1;
	  ++offset;
	}
    }

}


/* Functions for the construction of regions.  */

/* Print the regions, for debugging purposes.  Callable from debugger.  */

void
debug_regions ()
{
  int rgn, bb;

  fprintf (dump, "\n;;   ------------ REGIONS ----------\n\n");
  for (rgn = 0; rgn < nr_regions; rgn++)
    {
      fprintf (dump, ";;\trgn %d nr_blocks %d:\n", rgn,
	       rgn_table[rgn].rgn_nr_blocks);
      fprintf (dump, ";;\tbb/block: ");

      for (bb = 0; bb < rgn_table[rgn].rgn_nr_blocks; bb++)
	{
	  current_blocks = RGN_BLOCKS (rgn);

	  if (bb != BLOCK_TO_BB (BB_TO_BLOCK (bb)))
	    abort ();

	  fprintf (dump, " %d/%d ", bb, BB_TO_BLOCK (bb));
	}

      fprintf (dump, "\n\n");
    }
}


/* Build a single block region for each basic block in the function.
   This allows for using the same code for interblock and basic block
   scheduling.  */

static void
find_single_block_region ()
{
  int i;

  for (i = 0; i < n_basic_blocks; i++)
    {
      rgn_bb_table[i] = i;
      RGN_NR_BLOCKS (i) = 1;
      RGN_BLOCKS (i) = i;
      CONTAINING_RGN (i) = i;
      BLOCK_TO_BB (i) = 0;
    }
  nr_regions = n_basic_blocks;
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
#define UPDATE_LOOP_RELATIONS(blk, hdr)                              \
{                                                                    \
  if (max_hdr[blk] == -1)                                            \
    max_hdr[blk] = hdr;                                              \
  else if (dfs_nr[max_hdr[blk]] > dfs_nr[hdr])                       \
         RESET_BIT (inner, hdr);                                     \
  else if (dfs_nr[max_hdr[blk]] < dfs_nr[hdr])                       \
         {                                                           \
            RESET_BIT (inner,max_hdr[blk]);			     \
            max_hdr[blk] = hdr;                                      \
         }                                                           \
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
     sbitmap *dom;
{
  int *max_hdr, *dfs_nr, *stack, *degree;
  char no_loops = 1;
  int node, child, loop_head, i, head, tail;
  int count = 0, sp, idx = 0, current_edge = out_edges[0];
  int num_bbs, num_insns, unreachable;
  int too_large_failure;

  /* Note if an edge has been passed.  */
  sbitmap passed;

  /* Note if a block is a natural loop header.  */
  sbitmap header;

  /* Note if a block is an natural inner loop header.  */
  sbitmap inner;

  /* Note if a block is in the block queue. */
  sbitmap in_queue;

  /* Note if a block is in the block queue. */
  sbitmap in_stack;

  int num_edges = NUM_EDGES (edge_list);

  /* Perform a DFS traversal of the cfg.  Identify loop headers, inner loops
     and a mapping from block to its loop header (if the block is contained
     in a loop, else -1).

     Store results in HEADER, INNER, and MAX_HDR respectively, these will
     be used as inputs to the second traversal.

     STACK, SP and DFS_NR are only used during the first traversal.  */

  /* Allocate and initialize variables for the first traversal.  */
  max_hdr = (int *) xmalloc (n_basic_blocks * sizeof (int));
  dfs_nr = (int *) xcalloc (n_basic_blocks, sizeof (int));
  stack = (int *) xmalloc (nr_edges * sizeof (int));

  inner = sbitmap_alloc (n_basic_blocks);
  sbitmap_ones (inner);

  header = sbitmap_alloc (n_basic_blocks);
  sbitmap_zero (header);

  passed = sbitmap_alloc (nr_edges);
  sbitmap_zero (passed);

  in_queue = sbitmap_alloc (n_basic_blocks);
  sbitmap_zero (in_queue);

  in_stack = sbitmap_alloc (n_basic_blocks);
  sbitmap_zero (in_stack);

  for (i = 0; i < n_basic_blocks; i++)
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
  for (i = 0; i < n_basic_blocks; i++)
    if (dfs_nr[i] == 0)
      {
	unreachable = 1;
	break;
      }

  /* Gross.  To avoid wasting memory, the second pass uses the dfs_nr array
     to hold degree counts.  */
  degree = dfs_nr;

  for (i = 0; i < n_basic_blocks; i++)
    degree[i] = 0;
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
      for (i = 0; i < n_basic_blocks; i++)
	{
	  if (TEST_BIT (header, i) && TEST_BIT (inner, i))
	    {
	      edge e;
	      int j;

	      /* Now check that the loop is reducible.  We do this separate
		 from finding inner loops so that we do not find a reducible
		 loop which contains an inner non-reducible loop.

		 A simple way to find reducible/natural loops is to verify
		 that each block in the loop is dominated by the loop
		 header.

		 If there exists a block that is not dominated by the loop
		 header, then the block is reachable from outside the loop
		 and thus the loop is not a natural loop.  */
	      for (j = 0; j < n_basic_blocks; j++)	
		{
		  /* First identify blocks in the loop, except for the loop
		     entry block.  */
		  if (i == max_hdr[j] && i != j)
		    {
		      /* Now verify that the block is dominated by the loop
			 header.  */
		      if (!TEST_BIT (dom[j], i))
			break;
		    }
		}

	      /* If we exited the loop early, then I is the header of
		 a non-reducible loop and we should quit processing it
		 now.  */
	      if (j != n_basic_blocks)
		continue;

	      /* I is a header of an inner loop, or block 0 in a subroutine
		 with no loops at all.  */
	      head = tail = -1;
	      too_large_failure = 0;
	      loop_head = max_hdr[i];

	      /* Decrease degree of all I's successors for topological
		 ordering.  */
	      for (e = BASIC_BLOCK (i)->succ; e; e = e->succ_next)
		if (e->dest != EXIT_BLOCK_PTR)
		  --degree[e->dest->index];

	      /* Estimate # insns, and count # blocks in the region.  */
	      num_bbs = 1;
	      num_insns	= (INSN_LUID (BLOCK_END (i))
			   - INSN_LUID (BLOCK_HEAD (i)));


	      /* Find all loop latches (blocks with back edges to the loop
		 header) or all the leaf blocks in the cfg has no loops.

		 Place those blocks into the queue.  */
	      if (no_loops)
		{
		  for (j = 0; j < n_basic_blocks; j++)
		    /* Leaf nodes have only a single successor which must
		       be EXIT_BLOCK.  */
		    if (BASIC_BLOCK (j)->succ
			&& BASIC_BLOCK (j)->succ->dest == EXIT_BLOCK_PTR
			&& BASIC_BLOCK (j)->succ->succ_next == NULL)
		      {
			queue[++tail] = j;
			SET_BIT (in_queue, j);

			if (too_large (j, &num_bbs, &num_insns))
			  {
			    too_large_failure = 1;
			    break;
			  }
		      }
		}
	      else
		{
		  edge e;

		  for (e = BASIC_BLOCK (i)->pred; e; e = e->pred_next)
		    {
		      if (e->src == ENTRY_BLOCK_PTR)
			continue;

		      node = e->src->index;

		      if (max_hdr[node] == loop_head && node != i)
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
		      else if (!TEST_BIT (in_queue, node) && node != i)
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
		  degree[i] = -1;
		  rgn_bb_table[idx] = i;
		  RGN_NR_BLOCKS (nr_regions) = num_bbs;
		  RGN_BLOCKS (nr_regions) = idx++;
		  CONTAINING_RGN (i) = nr_regions;
		  BLOCK_TO_BB (i) = count = 0;

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
  for (i = 0; i < n_basic_blocks; i++)
    if (degree[i] >= 0)
      {
	rgn_bb_table[idx] = i;
	RGN_NR_BLOCKS (nr_regions) = 1;
	RGN_BLOCKS (nr_regions) = idx++;
	CONTAINING_RGN (i) = nr_regions++;
	BLOCK_TO_BB (i) = 0;
      }

  free (max_hdr);
  free (dfs_nr);
  free (stack);
  free (passed);
  free (header);
  free (inner);
  free (in_queue);
  free (in_stack);
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
      BITSET_ADD (dom[bb], 0, bbset_size);
      prob[bb] = 1.0;
      return;
    }

  fst_in_edge = nxt_in_edge = IN_EDGES (BB_TO_BLOCK (bb));

  /* Intialize dom[bb] to '111..1'.  */
  BITSET_INVERT (dom[bb], bbset_size);

  do
    {
      pred = FROM_BLOCK (nxt_in_edge);
      BITSET_INTER (dom[bb], dom[BLOCK_TO_BB (pred)], bbset_size);

      BITSET_UNION (ancestor_edges[bb], ancestor_edges[BLOCK_TO_BB (pred)],
		    edgeset_size);

      BITSET_ADD (ancestor_edges[bb], EDGE_TO_BIT (nxt_in_edge), edgeset_size);

      nr_out_edges = 1;
      nr_rgn_out_edges = 0;
      fst_out_edge = OUT_EDGES (pred);
      nxt_out_edge = NEXT_OUT (fst_out_edge);
      BITSET_UNION (pot_split[bb], pot_split[BLOCK_TO_BB (pred)],
		    edgeset_size);

      BITSET_ADD (pot_split[bb], EDGE_TO_BIT (fst_out_edge), edgeset_size);

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
	  BITSET_ADD (pot_split[bb], EDGE_TO_BIT (nxt_out_edge), edgeset_size);
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

  BITSET_ADD (dom[bb], bb, bbset_size);
  BITSET_DIFFER (pot_split[bb], ancestor_edges[bb], edgeset_size);

  if (sched_verbose >= 2)
    fprintf (dump, ";;  bb_prob(%d, %d) = %3d\n", bb, BB_TO_BLOCK (bb), (int) (100.0 * prob[bb]));
}				/* compute_dom_prob_ps */

/* Functions for target info.  */

/* Compute in BL the list of split-edges of bb_src relatively to bb_trg.
   Note that bb_trg dominates bb_src.  */

static void
split_edges (bb_src, bb_trg, bl)
     int bb_src;
     int bb_trg;
     edgelst *bl;
{
  int es = edgeset_size;
  edgeset src = (edgeset) xmalloc (es * sizeof (HOST_WIDE_INT));

  while (es--)
    src[es] = (pot_split[bb_src])[es];
  BITSET_DIFFER (src, pot_split[bb_trg], edgeset_size);
  extract_bitlst (src, edgeset_size, bl);
  free (src);
}


/* Find the valid candidate-source-blocks for the target block TRG, compute
   their probability, and check if they are speculative or not.
   For speculative sources, compute their update-blocks and split-blocks.  */

static void
compute_trg_info (trg)
     int trg;
{
  register candidate *sp;
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
	  sp->split_bbs.first_member = &bblst_table[bblst_last];
	  sp->split_bbs.nr_members = el.nr_members;
	  for (j = 0; j < el.nr_members; bblst_last++, j++)
	    bblst_table[bblst_last] =
	      TO_BLOCK (rgn_edges[el.first_member[j]]);
	  sp->update_bbs.first_member = &bblst_table[bblst_last];
	  update_idx = 0;
	  for (j = 0; j < el.nr_members; j++)
	    {
	      check_block = FROM_BLOCK (rgn_edges[el.first_member[j]]);
	      fst_edge = nxt_edge = OUT_EDGES (check_block);
	      do
		{
		  for (k = 0; k < el.nr_members; k++)
		    if (EDGE_TO_BIT (nxt_edge) == el.first_member[k])
		      break;

		  if (k >= el.nr_members)
		    {
		      bblst_table[bblst_last++] = TO_BLOCK (nxt_edge);
		      update_idx++;
		    }

		  nxt_edge = NEXT_OUT (nxt_edge);
		}
	      while (fst_edge != nxt_edge);
	    }
	  sp->update_bbs.nr_members = update_idx;

	}
      else
	{
	  sp->split_bbs.nr_members = sp->update_bbs.nr_members = 0;

	  sp->is_speculative = 0;
	  sp->src_prob = 0;
	}
    }
}				/* compute_trg_info */


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
      fprintf (dump, "src b %d bb %d speculative \n", BB_TO_BLOCK (i), i);

      fprintf (dump, "split path: ");
      for (j = 0; j < candidate_table[i].split_bbs.nr_members; j++)
	{
	  int b = candidate_table[i].split_bbs.first_member[j];

	  fprintf (dump, " %d ", b);
	}
      fprintf (dump, "\n");

      fprintf (dump, "update path: ");
      for (j = 0; j < candidate_table[i].update_bbs.nr_members; j++)
	{
	  int b = candidate_table[i].update_bbs.first_member[j];

	  fprintf (dump, " %d ", b);
	}
      fprintf (dump, "\n");
    }
  else
    {
      fprintf (dump, " src %d equivalent\n", BB_TO_BLOCK (i));
    }
}


/* Print candidates info, for debugging purposes.  Callable from debugger.  */

void
debug_candidates (trg)
     int trg;
{
  int i;

  fprintf (dump, "----------- candidate table: target: b=%d bb=%d ---\n",
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
  register int i;
  register int regno;
  register rtx reg = SET_DEST (x);

  if (reg == 0)
    return 1;

  while (GET_CODE (reg) == SUBREG || GET_CODE (reg) == ZERO_EXTRACT
	 || GET_CODE (reg) == SIGN_EXTRACT
	 || GET_CODE (reg) == STRICT_LOW_PART)
    reg = XEXP (reg, 0);

  if (GET_CODE (reg) == PARALLEL
      && GET_MODE (reg) == BLKmode)
    {
      register int i;
      for (i = XVECLEN (reg, 0) - 1; i >= 0; i--)
	if (check_live_1 (src, XVECEXP (reg, 0, i)))
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
  register int i;
  register int regno;
  register rtx reg = SET_DEST (x);

  if (reg == 0)
    return;

  while (GET_CODE (reg) == SUBREG || GET_CODE (reg) == ZERO_EXTRACT
	 || GET_CODE (reg) == SIGN_EXTRACT
	 || GET_CODE (reg) == STRICT_LOW_PART)
    reg = XEXP (reg, 0);

  if (GET_CODE (reg) == PARALLEL
      && GET_MODE (reg) == BLKmode)
    {
      register int i;
      for (i = XVECLEN (reg, 0) - 1; i >= 0; i--)
	update_live_1 (src, XVECEXP (reg, 0, i));
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
(bb_from == bb_to                                                       \
   || IS_RGN_ENTRY (bb_from)						\
   || (bitset_member (ancestor_edges[bb_to],				\
		      EDGE_TO_BIT (IN_EDGES (BB_TO_BLOCK (bb_from))),	\
		      edgeset_size)))

/* Non-zero iff the address is comprised from at most 1 register.  */
#define CONST_BASED_ADDRESS_P(x)			\
  (GET_CODE (x) == REG					\
   || ((GET_CODE (x) == PLUS || GET_CODE (x) == MINUS   \
	|| (GET_CODE (x) == LO_SUM))	                \
       && (GET_CODE (XEXP (x, 0)) == CONST_INT		\
	   || GET_CODE (XEXP (x, 1)) == CONST_INT)))

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
  register candidate *candp = candidate_table + bb_src;

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
      if (code == MEM)
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
}				/* may_trap_exp */


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
	      tmp_class =
		WORST_CLASS (tmp_class,
			   may_trap_exp (SET_SRC (XVECEXP (pat, 0, i)), 0));
	      break;
	    case TRAP_IF:
	      tmp_class = TRAP_RISKY;
	      break;
	    default:;
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
	case TRAP_IF:
	  tmp_class = TRAP_RISKY;
	  break;
	default:;
	}
      insn_class = tmp_class;
    }

  return insn_class;

}				/* haifa_classify_insn */

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
}				/* is_prisky */

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
}				/* is_exception_free */


/* Process an insn's memory dependencies.  There are four kinds of
   dependencies:

   (0) read dependence: read follows read
   (1) true dependence: read follows write
   (2) anti dependence: write follows read
   (3) output dependence: write follows write

   We are careful to build only dependencies which actually exist, and
   use transitivity to avoid building too many links.  */

/* Return the INSN_LIST containing INSN in LIST, or NULL
   if LIST does not contain INSN.  */

HAIFA_INLINE static rtx
find_insn_list (insn, list)
     rtx insn;
     rtx list;
{
  while (list)
    {
      if (XEXP (list, 0) == insn)
	return list;
      list = XEXP (list, 1);
    }
  return 0;
}


/* Return 1 if the pair (insn, x) is found in (LIST, LIST1), or 0
   otherwise.  */

HAIFA_INLINE static char
find_insn_mem_list (insn, x, list, list1)
     rtx insn, x;
     rtx list, list1;
{
  while (list)
    {
      if (XEXP (list, 0) == insn
	  && XEXP (list1, 0) == x)
	return 1;
      list = XEXP (list, 1);
      list1 = XEXP (list1, 1);
    }
  return 0;
}


/* Compute the function units used by INSN.  This caches the value
   returned by function_units_used.  A function unit is encoded as the
   unit number if the value is non-negative and the compliment of a
   mask if the value is negative.  A function unit index is the
   non-negative encoding.  */

HAIFA_INLINE static int
insn_unit (insn)
     rtx insn;
{
  register int unit = INSN_UNIT (insn);

  if (unit == 0)
    {
      recog_memoized (insn);

      /* A USE insn, or something else we don't need to understand.
         We can't pass these directly to function_units_used because it will
         trigger a fatal error for unrecognizable insns.  */
      if (INSN_CODE (insn) < 0)
	unit = -1;
      else
	{
	  unit = function_units_used (insn);
	  /* Increment non-negative values so we can cache zero.  */
	  if (unit >= 0)
	    unit++;
	}
      /* We only cache 16 bits of the result, so if the value is out of
         range, don't cache it.  */
      if (FUNCTION_UNITS_SIZE < HOST_BITS_PER_SHORT
	  || unit >= 0
	  || (unit & ~((1 << (HOST_BITS_PER_SHORT - 1)) - 1)) == 0)
	INSN_UNIT (insn) = unit;
    }
  return (unit > 0 ? unit - 1 : unit);
}

/* Compute the blockage range for executing INSN on UNIT.  This caches
   the value returned by the blockage_range_function for the unit.
   These values are encoded in an int where the upper half gives the
   minimum value and the lower half gives the maximum value.  */

HAIFA_INLINE static unsigned int
blockage_range (unit, insn)
     int unit;
     rtx insn;
{
  unsigned int blockage = INSN_BLOCKAGE (insn);
  unsigned int range;

  if ((int) UNIT_BLOCKED (blockage) != unit + 1)
    {
      range = function_units[unit].blockage_range_function (insn);
      /* We only cache the blockage range for one unit and then only if
         the values fit.  */
      if (HOST_BITS_PER_INT >= UNIT_BITS + 2 * BLOCKAGE_BITS)
	INSN_BLOCKAGE (insn) = ENCODE_BLOCKAGE (unit + 1, range);
    }
  else
    range = BLOCKAGE_RANGE (blockage);

  return range;
}

/* A vector indexed by function unit instance giving the last insn to use
   the unit.  The value of the function unit instance index for unit U
   instance I is (U + I * FUNCTION_UNITS_SIZE).  */
static rtx unit_last_insn[FUNCTION_UNITS_SIZE * MAX_MULTIPLICITY];

/* A vector indexed by function unit instance giving the minimum time when
   the unit will unblock based on the maximum blockage cost.  */
static int unit_tick[FUNCTION_UNITS_SIZE * MAX_MULTIPLICITY];

/* A vector indexed by function unit number giving the number of insns
   that remain to use the unit.  */
static int unit_n_insns[FUNCTION_UNITS_SIZE];

/* Reset the function unit state to the null state.  */

static void
clear_units ()
{
  bzero ((char *) unit_last_insn, sizeof (unit_last_insn));
  bzero ((char *) unit_tick, sizeof (unit_tick));
  bzero ((char *) unit_n_insns, sizeof (unit_n_insns));
}

/* Return the issue-delay of an insn.  */

HAIFA_INLINE static int
insn_issue_delay (insn)
     rtx insn;
{
  int i, delay = 0;
  int unit = insn_unit (insn);

  /* Efficiency note: in fact, we are working 'hard' to compute a
     value that was available in md file, and is not available in
     function_units[] structure.  It would be nice to have this
     value there, too.  */
  if (unit >= 0)
    {
      if (function_units[unit].blockage_range_function &&
	  function_units[unit].blockage_function)
	delay = function_units[unit].blockage_function (insn, insn);
    }
  else
    for (i = 0, unit = ~unit; unit; i++, unit >>= 1)
      if ((unit & 1) != 0 && function_units[i].blockage_range_function
	  && function_units[i].blockage_function)
	delay = MAX (delay, function_units[i].blockage_function (insn, insn));

  return delay;
}

/* Return the actual hazard cost of executing INSN on the unit UNIT,
   instance INSTANCE at time CLOCK if the previous actual hazard cost
   was COST.  */

HAIFA_INLINE static int
actual_hazard_this_instance (unit, instance, insn, clock, cost)
     int unit, instance, clock, cost;
     rtx insn;
{
  int tick = unit_tick[instance]; /* Issue time of the last issued insn.  */

  if (tick - clock > cost)
    {
      /* The scheduler is operating forward, so unit's last insn is the
         executing insn and INSN is the candidate insn.  We want a
         more exact measure of the blockage if we execute INSN at CLOCK
         given when we committed the execution of the unit's last insn.

         The blockage value is given by either the unit's max blockage
         constant, blockage range function, or blockage function.  Use
         the most exact form for the given unit.  */

      if (function_units[unit].blockage_range_function)
	{
	  if (function_units[unit].blockage_function)
	    tick += (function_units[unit].blockage_function
		     (unit_last_insn[instance], insn)
		     - function_units[unit].max_blockage);
	  else
	    tick += ((int) MAX_BLOCKAGE_COST (blockage_range (unit, insn))
		     - function_units[unit].max_blockage);
	}
      if (tick - clock > cost)
	cost = tick - clock;
    }
  return cost;
}

/* Record INSN as having begun execution on the units encoded by UNIT at
   time CLOCK.  */

HAIFA_INLINE static void
schedule_unit (unit, insn, clock)
     int unit, clock;
     rtx insn;
{
  int i;

  if (unit >= 0)
    {
      int instance = unit;
#if MAX_MULTIPLICITY > 1
      /* Find the first free instance of the function unit and use that
         one.  We assume that one is free.  */
      for (i = function_units[unit].multiplicity - 1; i > 0; i--)
	{
	  if (!actual_hazard_this_instance (unit, instance, insn, clock, 0))
	    break;
	  instance += FUNCTION_UNITS_SIZE;
	}
#endif
      unit_last_insn[instance] = insn;
      unit_tick[instance] = (clock + function_units[unit].max_blockage);
    }
  else
    for (i = 0, unit = ~unit; unit; i++, unit >>= 1)
      if ((unit & 1) != 0)
	schedule_unit (i, insn, clock);
}

/* Return the actual hazard cost of executing INSN on the units encoded by
   UNIT at time CLOCK if the previous actual hazard cost was COST.  */

HAIFA_INLINE static int
actual_hazard (unit, insn, clock, cost)
     int unit, clock, cost;
     rtx insn;
{
  int i;

  if (unit >= 0)
    {
      /* Find the instance of the function unit with the minimum hazard.  */
      int instance = unit;
      int best_cost = actual_hazard_this_instance (unit, instance, insn,
						   clock, cost);
#if MAX_MULTIPLICITY > 1
      int this_cost;

      if (best_cost > cost)
	{
	  for (i = function_units[unit].multiplicity - 1; i > 0; i--)
	    {
	      instance += FUNCTION_UNITS_SIZE;
	      this_cost = actual_hazard_this_instance (unit, instance, insn,
						       clock, cost);
	      if (this_cost < best_cost)
		{
		  best_cost = this_cost;
		  if (this_cost <= cost)
		    break;
		}
	    }
	}
#endif
      cost = MAX (cost, best_cost);
    }
  else
    for (i = 0, unit = ~unit; unit; i++, unit >>= 1)
      if ((unit & 1) != 0)
	cost = actual_hazard (i, insn, clock, cost);

  return cost;
}

/* Return the potential hazard cost of executing an instruction on the
   units encoded by UNIT if the previous potential hazard cost was COST.
   An insn with a large blockage time is chosen in preference to one
   with a smaller time; an insn that uses a unit that is more likely
   to be used is chosen in preference to one with a unit that is less
   used.  We are trying to minimize a subsequent actual hazard.  */

HAIFA_INLINE static int
potential_hazard (unit, insn, cost)
     int unit, cost;
     rtx insn;
{
  int i, ncost;
  unsigned int minb, maxb;

  if (unit >= 0)
    {
      minb = maxb = function_units[unit].max_blockage;
      if (maxb > 1)
	{
	  if (function_units[unit].blockage_range_function)
	    {
	      maxb = minb = blockage_range (unit, insn);
	      maxb = MAX_BLOCKAGE_COST (maxb);
	      minb = MIN_BLOCKAGE_COST (minb);
	    }

	  if (maxb > 1)
	    {
	      /* Make the number of instructions left dominate.  Make the
	         minimum delay dominate the maximum delay.  If all these
	         are the same, use the unit number to add an arbitrary
	         ordering.  Other terms can be added.  */
	      ncost = minb * 0x40 + maxb;
	      ncost *= (unit_n_insns[unit] - 1) * 0x1000 + unit;
	      if (ncost > cost)
		cost = ncost;
	    }
	}
    }
  else
    for (i = 0, unit = ~unit; unit; i++, unit >>= 1)
      if ((unit & 1) != 0)
	cost = potential_hazard (i, insn, cost);

  return cost;
}

/* Compute cost of executing INSN given the dependence LINK on the insn USED.
   This is the number of cycles between instruction issue and
   instruction results.  */

HAIFA_INLINE static int
insn_cost (insn, link, used)
     rtx insn, link, used;
{
  register int cost = INSN_COST (insn);

  if (cost == 0)
    {
      recog_memoized (insn);

      /* A USE insn, or something else we don't need to understand.
         We can't pass these directly to result_ready_cost because it will
         trigger a fatal error for unrecognizable insns.  */
      if (INSN_CODE (insn) < 0)
	{
	  INSN_COST (insn) = 1;
	  return 1;
	}
      else
	{
	  cost = result_ready_cost (insn);

	  if (cost < 1)
	    cost = 1;

	  INSN_COST (insn) = cost;
	}
    }

  /* In this case estimate cost without caring how insn is used.  */
  if (link == 0 && used == 0)
    return cost;

  /* A USE insn should never require the value used to be computed.  This
     allows the computation of a function's result and parameter values to
     overlap the return and call.  */
  recog_memoized (used);
  if (INSN_CODE (used) < 0)
    LINK_COST_FREE (link) = 1;

  /* If some dependencies vary the cost, compute the adjustment.  Most
     commonly, the adjustment is complete: either the cost is ignored
     (in the case of an output- or anti-dependence), or the cost is
     unchanged.  These values are cached in the link as LINK_COST_FREE
     and LINK_COST_ZERO.  */

  if (LINK_COST_FREE (link))
    cost = 0;
#ifdef ADJUST_COST
  else if (!LINK_COST_ZERO (link))
    {
      int ncost = cost;

      ADJUST_COST (used, link, insn, ncost);
      if (ncost < 1)
	{
	  LINK_COST_FREE (link) = 1;
	  ncost = 0;
	}
      if (cost == ncost)
	LINK_COST_ZERO (link) = 1;
      cost = ncost;
    }
#endif
  return cost;
}

/* Compute the priority number for INSN.  */

static int
priority (insn)
     rtx insn;
{
  int this_priority;
  rtx link;

  if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
    return 0;

  if ((this_priority = INSN_PRIORITY (insn)) == 0)
    {
      if (INSN_DEPEND (insn) == 0)
	this_priority = insn_cost (insn, 0, 0);
      else
	for (link = INSN_DEPEND (insn); link; link = XEXP (link, 1))
	  {
	    rtx next;
	    int next_priority;

	    if (RTX_INTEGRATED_P (link))
	      continue;

	    next = XEXP (link, 0);

	    /* Critical path is meaningful in block boundaries only.  */
	    if (BLOCK_NUM (next) != BLOCK_NUM (insn))
	      continue;

	    next_priority = insn_cost (insn, link, next) + priority (next);
	    if (next_priority > this_priority)
	      this_priority = next_priority;
	  }
      INSN_PRIORITY (insn) = this_priority;
    }
  return this_priority;
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

/* Add an INSN and MEM reference pair to a pending INSN_LIST and MEM_LIST.
   The MEM is a memory reference contained within INSN, which we are saving
   so that we can do memory aliasing on it.  */

static void
add_insn_mem_dependence (deps, insn_list, mem_list, insn, mem)
     struct deps *deps;
     rtx *insn_list, *mem_list, insn, mem;
{
  register rtx link;

  link = alloc_INSN_LIST (insn, *insn_list);
  *insn_list = link;

  link = alloc_EXPR_LIST (VOIDmode, mem, *mem_list);
  *mem_list = link;

  deps->pending_lists_length++;
}

/* Make a dependency between every memory reference on the pending lists
   and INSN, thus flushing the pending lists.  If ONLY_WRITE, don't flush
   the read list.  */

static void
flush_pending_lists (deps, insn, only_write)
     struct deps *deps;
     rtx insn;
     int only_write;
{
  rtx u;
  rtx link;

  while (deps->pending_read_insns && ! only_write)
    {
      add_dependence (insn, XEXP (deps->pending_read_insns, 0),
		      REG_DEP_ANTI);

      link = deps->pending_read_insns;
      deps->pending_read_insns = XEXP (deps->pending_read_insns, 1);
      free_INSN_LIST_node (link);

      link = deps->pending_read_mems;
      deps->pending_read_mems = XEXP (deps->pending_read_mems, 1);
      free_EXPR_LIST_node (link);
    }
  while (deps->pending_write_insns)
    {
      add_dependence (insn, XEXP (deps->pending_write_insns, 0),
		      REG_DEP_ANTI);

      link = deps->pending_write_insns;
      deps->pending_write_insns = XEXP (deps->pending_write_insns, 1);
      free_INSN_LIST_node (link);

      link = deps->pending_write_mems;
      deps->pending_write_mems = XEXP (deps->pending_write_mems, 1);
      free_EXPR_LIST_node (link);
    }
  deps->pending_lists_length = 0;

  /* last_pending_memory_flush is now a list of insns.  */
  for (u = deps->last_pending_memory_flush; u; u = XEXP (u, 1))
    add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);

  free_INSN_LIST_list (&deps->last_pending_memory_flush);
  deps->last_pending_memory_flush = alloc_INSN_LIST (insn, NULL_RTX);
}

/* Analyze a single SET, CLOBBER, PRE_DEC, POST_DEC, PRE_INC or POST_INC
   rtx, X, creating all dependencies generated by the write to the
   destination of X, and reads of everything mentioned.  */

static void
sched_analyze_1 (deps, x, insn)
     struct deps *deps;
     rtx x;
     rtx insn;
{
  register int regno;
  register rtx dest = XEXP (x, 0);
  enum rtx_code code = GET_CODE (x);

  if (dest == 0)
    return;

  if (GET_CODE (dest) == PARALLEL
      && GET_MODE (dest) == BLKmode)
    {
      register int i;
      for (i = XVECLEN (dest, 0) - 1; i >= 0; i--)
	sched_analyze_1 (deps, XVECEXP (dest, 0, i), insn);
      if (GET_CODE (x) == SET)
	sched_analyze_2 (deps, SET_SRC (x), insn);
      return;
    }

  while (GET_CODE (dest) == STRICT_LOW_PART || GET_CODE (dest) == SUBREG
      || GET_CODE (dest) == ZERO_EXTRACT || GET_CODE (dest) == SIGN_EXTRACT)
    {
      if (GET_CODE (dest) == ZERO_EXTRACT || GET_CODE (dest) == SIGN_EXTRACT)
	{
	  /* The second and third arguments are values read by this insn.  */
	  sched_analyze_2 (deps, XEXP (dest, 1), insn);
	  sched_analyze_2 (deps, XEXP (dest, 2), insn);
	}
      dest = XEXP (dest, 0);
    }

  if (GET_CODE (dest) == REG)
    {
      register int i;

      regno = REGNO (dest);

      /* A hard reg in a wide mode may really be multiple registers.
         If so, mark all of them just like the first.  */
      if (regno < FIRST_PSEUDO_REGISTER)
	{
	  i = HARD_REGNO_NREGS (regno, GET_MODE (dest));
	  while (--i >= 0)
	    {
	      int r = regno + i;
	      rtx u;

	      for (u = deps->reg_last_uses[r]; u; u = XEXP (u, 1))
		add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);

	      for (u = deps->reg_last_sets[r]; u; u = XEXP (u, 1))
		add_dependence (insn, XEXP (u, 0), REG_DEP_OUTPUT);

	      /* Clobbers need not be ordered with respect to one
		 another, but sets must be ordered with respect to a
		 pending clobber.  */
	      if (code == SET)
		{
		  free_INSN_LIST_list (&deps->reg_last_uses[r]);
	          for (u = deps->reg_last_clobbers[r]; u; u = XEXP (u, 1))
		    add_dependence (insn, XEXP (u, 0), REG_DEP_OUTPUT);
	          SET_REGNO_REG_SET (reg_pending_sets, r);
		}
	      else
		SET_REGNO_REG_SET (reg_pending_clobbers, r);

	      /* Function calls clobber all call_used regs.  */
	      if (global_regs[r] || (code == SET && call_used_regs[r]))
		for (u = deps->last_function_call; u; u = XEXP (u, 1))
		  add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);
	    }
	}
      else
	{
	  rtx u;

	  for (u = deps->reg_last_uses[regno]; u; u = XEXP (u, 1))
	    add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);

	  for (u = deps->reg_last_sets[regno]; u; u = XEXP (u, 1))
	    add_dependence (insn, XEXP (u, 0), REG_DEP_OUTPUT);

	  if (code == SET)
	    {
	      free_INSN_LIST_list (&deps->reg_last_uses[regno]);
	      for (u = deps->reg_last_clobbers[regno]; u; u = XEXP (u, 1))
		add_dependence (insn, XEXP (u, 0), REG_DEP_OUTPUT);
	      SET_REGNO_REG_SET (reg_pending_sets, regno);
	    }
	  else
	    SET_REGNO_REG_SET (reg_pending_clobbers, regno);

	  /* Pseudos that are REG_EQUIV to something may be replaced
	     by that during reloading.  We need only add dependencies for
	     the address in the REG_EQUIV note.  */
	  if (!reload_completed
	      && reg_known_equiv_p[regno]
	      && GET_CODE (reg_known_value[regno]) == MEM)
	    sched_analyze_2 (deps, XEXP (reg_known_value[regno], 0), insn);

	  /* Don't let it cross a call after scheduling if it doesn't
	     already cross one.  */

	  if (REG_N_CALLS_CROSSED (regno) == 0)
	    for (u = deps->last_function_call; u; u = XEXP (u, 1))
	      add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);
	}
    }
  else if (GET_CODE (dest) == MEM)
    {
      /* Writing memory.  */

      if (deps->pending_lists_length > 32)
	{
	  /* Flush all pending reads and writes to prevent the pending lists
	     from getting any larger.  Insn scheduling runs too slowly when
	     these lists get long.  The number 32 was chosen because it
	     seems like a reasonable number.  When compiling GCC with itself,
	     this flush occurs 8 times for sparc, and 10 times for m88k using
	     the number 32.  */
	  flush_pending_lists (deps, insn, 0);
	}
      else
	{
	  rtx u;
	  rtx pending, pending_mem;

	  pending = deps->pending_read_insns;
	  pending_mem = deps->pending_read_mems;
	  while (pending)
	    {
	      if (anti_dependence (XEXP (pending_mem, 0), dest))
		add_dependence (insn, XEXP (pending, 0), REG_DEP_ANTI);

	      pending = XEXP (pending, 1);
	      pending_mem = XEXP (pending_mem, 1);
	    }

	  pending = deps->pending_write_insns;
	  pending_mem = deps->pending_write_mems;
	  while (pending)
	    {
	      if (output_dependence (XEXP (pending_mem, 0), dest))
		add_dependence (insn, XEXP (pending, 0), REG_DEP_OUTPUT);

	      pending = XEXP (pending, 1);
	      pending_mem = XEXP (pending_mem, 1);
	    }

	  for (u = deps->last_pending_memory_flush; u; u = XEXP (u, 1))
	    add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);

	  add_insn_mem_dependence (deps, &deps->pending_write_insns,
				   &deps->pending_write_mems, insn, dest);
	}
      sched_analyze_2 (deps, XEXP (dest, 0), insn);
    }

  /* Analyze reads.  */
  if (GET_CODE (x) == SET)
    sched_analyze_2 (deps, SET_SRC (x), insn);
}

/* Analyze the uses of memory and registers in rtx X in INSN.  */

static void
sched_analyze_2 (deps, x, insn)
     struct deps *deps;
     rtx x;
     rtx insn;
{
  register int i;
  register int j;
  register enum rtx_code code;
  register const char *fmt;

  if (x == 0)
    return;

  code = GET_CODE (x);

  switch (code)
    {
    case CONST_INT:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case CONST:
    case LABEL_REF:
      /* Ignore constants.  Note that we must handle CONST_DOUBLE here
         because it may have a cc0_rtx in its CONST_DOUBLE_CHAIN field, but
         this does not mean that this insn is using cc0.  */
      return;

#ifdef HAVE_cc0
    case CC0:
      {
	rtx link, prev;

	/* User of CC0 depends on immediately preceding insn.  */
	SCHED_GROUP_P (insn) = 1;

	/* There may be a note before this insn now, but all notes will
	   be removed before we actually try to schedule the insns, so
	   it won't cause a problem later.  We must avoid it here though.  */
	prev = prev_nonnote_insn (insn);

	/* Make a copy of all dependencies on the immediately previous insn,
	   and add to this insn.  This is so that all the dependencies will
	   apply to the group.  Remove an explicit dependence on this insn
	   as SCHED_GROUP_P now represents it.  */

	if (find_insn_list (prev, LOG_LINKS (insn)))
	  remove_dependence (insn, prev);

	for (link = LOG_LINKS (prev); link; link = XEXP (link, 1))
	  add_dependence (insn, XEXP (link, 0), REG_NOTE_KIND (link));

	return;
      }
#endif

    case REG:
      {
	rtx u;
	int regno = REGNO (x);
	if (regno < FIRST_PSEUDO_REGISTER)
	  {
	    int i;

	    i = HARD_REGNO_NREGS (regno, GET_MODE (x));
	    while (--i >= 0)
	      {
		int r = regno + i;
		deps->reg_last_uses[r]
		  = alloc_INSN_LIST (insn, deps->reg_last_uses[r]);

		for (u = deps->reg_last_sets[r]; u; u = XEXP (u, 1))
		  add_dependence (insn, XEXP (u, 0), 0);

		/* ??? This should never happen.  */
		for (u = deps->reg_last_clobbers[r]; u; u = XEXP (u, 1))
		  add_dependence (insn, XEXP (u, 0), 0);

		if (call_used_regs[r] || global_regs[r])
		  /* Function calls clobber all call_used regs.  */
		  for (u = deps->last_function_call; u; u = XEXP (u, 1))
		    add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);
	      }
	  }
	else
	  {
	    deps->reg_last_uses[regno]
	      = alloc_INSN_LIST (insn, deps->reg_last_uses[regno]);

	    for (u = deps->reg_last_sets[regno]; u; u = XEXP (u, 1))
	      add_dependence (insn, XEXP (u, 0), 0);

	    /* ??? This should never happen.  */
	    for (u = deps->reg_last_clobbers[regno]; u; u = XEXP (u, 1))
	      add_dependence (insn, XEXP (u, 0), 0);

	    /* Pseudos that are REG_EQUIV to something may be replaced
	       by that during reloading.  We need only add dependencies for
	       the address in the REG_EQUIV note.  */
	    if (!reload_completed
		&& reg_known_equiv_p[regno]
		&& GET_CODE (reg_known_value[regno]) == MEM)
	      sched_analyze_2 (deps, XEXP (reg_known_value[regno], 0), insn);

	    /* If the register does not already cross any calls, then add this
	       insn to the sched_before_next_call list so that it will still
	       not cross calls after scheduling.  */
	    if (REG_N_CALLS_CROSSED (regno) == 0)
	      add_dependence (deps->sched_before_next_call, insn,
			      REG_DEP_ANTI);
	  }
	return;
      }

    case MEM:
      {
	/* Reading memory.  */
	rtx u;
	rtx pending, pending_mem;

	pending = deps->pending_read_insns;
	pending_mem = deps->pending_read_mems;
	while (pending)
	  {
	    if (read_dependence (XEXP (pending_mem, 0), x))
	      add_dependence (insn, XEXP (pending, 0), REG_DEP_ANTI);

	    pending = XEXP (pending, 1);
	    pending_mem = XEXP (pending_mem, 1);
	  }

	pending = deps->pending_write_insns;
	pending_mem = deps->pending_write_mems;
	while (pending)
	  {
	    if (true_dependence (XEXP (pending_mem, 0), VOIDmode,
		x, rtx_varies_p))
	      add_dependence (insn, XEXP (pending, 0), 0);

	    pending = XEXP (pending, 1);
	    pending_mem = XEXP (pending_mem, 1);
	  }

	for (u = deps->last_pending_memory_flush; u; u = XEXP (u, 1))
	  add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);

	/* Always add these dependencies to pending_reads, since
	   this insn may be followed by a write.  */
	add_insn_mem_dependence (deps, &deps->pending_read_insns,
				 &deps->pending_read_mems, insn, x);

	/* Take advantage of tail recursion here.  */
	sched_analyze_2 (deps, XEXP (x, 0), insn);
	return;
      }

    /* Force pending stores to memory in case a trap handler needs them.  */
    case TRAP_IF:
      flush_pending_lists (deps, insn, 1);
      break;

    case ASM_OPERANDS:
    case ASM_INPUT:
    case UNSPEC_VOLATILE:
      {
	rtx u;

	/* Traditional and volatile asm instructions must be considered to use
	   and clobber all hard registers, all pseudo-registers and all of
	   memory.  So must TRAP_IF and UNSPEC_VOLATILE operations.

	   Consider for instance a volatile asm that changes the fpu rounding
	   mode.  An insn should not be moved across this even if it only uses
	   pseudo-regs because it might give an incorrectly rounded result.  */
	if (code != ASM_OPERANDS || MEM_VOLATILE_P (x))
	  {
	    int max_reg = max_reg_num ();
	    for (i = 0; i < max_reg; i++)
	      {
		for (u = deps->reg_last_uses[i]; u; u = XEXP (u, 1))
		  add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);
		free_INSN_LIST_list (&deps->reg_last_uses[i]);

		for (u = deps->reg_last_sets[i]; u; u = XEXP (u, 1))
		  add_dependence (insn, XEXP (u, 0), 0);

		for (u = deps->reg_last_clobbers[i]; u; u = XEXP (u, 1))
		  add_dependence (insn, XEXP (u, 0), 0);
	      }
	    reg_pending_sets_all = 1;

	    flush_pending_lists (deps, insn, 0);
	  }

	/* For all ASM_OPERANDS, we must traverse the vector of input operands.
	   We can not just fall through here since then we would be confused
	   by the ASM_INPUT rtx inside ASM_OPERANDS, which do not indicate
	   traditional asms unlike their normal usage.  */

	if (code == ASM_OPERANDS)
	  {
	    for (j = 0; j < ASM_OPERANDS_INPUT_LENGTH (x); j++)
	      sched_analyze_2 (deps, ASM_OPERANDS_INPUT (x, j), insn);
	    return;
	  }
	break;
      }

    case PRE_DEC:
    case POST_DEC:
    case PRE_INC:
    case POST_INC:
      /* These both read and modify the result.  We must handle them as writes
         to get proper dependencies for following instructions.  We must handle
         them as reads to get proper dependencies from this to previous
         instructions.  Thus we need to pass them to both sched_analyze_1
         and sched_analyze_2.  We must call sched_analyze_2 first in order
         to get the proper antecedent for the read.  */
      sched_analyze_2 (deps, XEXP (x, 0), insn);
      sched_analyze_1 (deps, x, insn);
      return;

    default:
      break;
    }

  /* Other cases: walk the insn.  */
  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	sched_analyze_2 (deps, XEXP (x, i), insn);
      else if (fmt[i] == 'E')
	for (j = 0; j < XVECLEN (x, i); j++)
	  sched_analyze_2 (deps, XVECEXP (x, i, j), insn);
    }
}

/* Analyze an INSN with pattern X to find all dependencies.  */

static void
sched_analyze_insn (deps, x, insn, loop_notes)
     struct deps *deps;
     rtx x, insn;
     rtx loop_notes;
{
  register RTX_CODE code = GET_CODE (x);
  rtx link;
  int maxreg = max_reg_num ();
  int i;

  if (code == SET || code == CLOBBER)
    sched_analyze_1 (deps, x, insn);
  else if (code == PARALLEL)
    {
      register int i;
      for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
	{
	  code = GET_CODE (XVECEXP (x, 0, i));
	  if (code == SET || code == CLOBBER)
	    sched_analyze_1 (deps, XVECEXP (x, 0, i), insn);
	  else
	    sched_analyze_2 (deps, XVECEXP (x, 0, i), insn);
	}
    }
  else
    sched_analyze_2 (deps, x, insn);

  /* Mark registers CLOBBERED or used by called function.  */
  if (GET_CODE (insn) == CALL_INSN)
    for (link = CALL_INSN_FUNCTION_USAGE (insn); link; link = XEXP (link, 1))
      {
	if (GET_CODE (XEXP (link, 0)) == CLOBBER)
	  sched_analyze_1 (deps, XEXP (link, 0), insn);
	else
	  sched_analyze_2 (deps, XEXP (link, 0), insn);
      }

  /* If there is a {LOOP,EHREGION}_{BEG,END} note in the middle of a basic
     block, then we must be sure that no instructions are scheduled across it.
     Otherwise, the reg_n_refs info (which depends on loop_depth) would
     become incorrect.  */

  if (loop_notes)
    {
      int max_reg = max_reg_num ();
      int schedule_barrier_found = 0;
      rtx link;

      /* Update loop_notes with any notes from this insn.  Also determine
	 if any of the notes on the list correspond to instruction scheduling
	 barriers (loop, eh & setjmp notes, but not range notes.  */
      link = loop_notes;
      while (XEXP (link, 1))
	{
	  if (INTVAL (XEXP (link, 0)) == NOTE_INSN_LOOP_BEG
	      || INTVAL (XEXP (link, 0)) == NOTE_INSN_LOOP_END
	      || INTVAL (XEXP (link, 0)) == NOTE_INSN_EH_REGION_BEG
	      || INTVAL (XEXP (link, 0)) == NOTE_INSN_EH_REGION_END
	      || INTVAL (XEXP (link, 0)) == NOTE_INSN_SETJMP)
	    schedule_barrier_found = 1;

	  link = XEXP (link, 1);
	}
      XEXP (link, 1) = REG_NOTES (insn);
      REG_NOTES (insn) = loop_notes;

      /* Add dependencies if a scheduling barrier was found.  */
      if (schedule_barrier_found)
	{
	  for (i = 0; i < max_reg; i++)
	    {
	      rtx u;
	      for (u = deps->reg_last_uses[i]; u; u = XEXP (u, 1))
		add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);
	      free_INSN_LIST_list (&deps->reg_last_uses[i]);

	      for (u = deps->reg_last_sets[i]; u; u = XEXP (u, 1))
		add_dependence (insn, XEXP (u, 0), 0);

	      for (u = deps->reg_last_clobbers[i]; u; u = XEXP (u, 1))
		add_dependence (insn, XEXP (u, 0), 0);
	    }
	  reg_pending_sets_all = 1;

	  flush_pending_lists (deps, insn, 0);
	}

    }

  /* Accumulate clobbers until the next set so that it will be output dependent
     on all of them.  At the next set we can clear the clobber list, since
     subsequent sets will be output dependent on it.  */
  EXECUTE_IF_SET_IN_REG_SET
    (reg_pending_sets, 0, i,
     {
       free_INSN_LIST_list (&deps->reg_last_sets[i]);
       free_INSN_LIST_list (&deps->reg_last_clobbers[i]);
       deps->reg_last_sets[i] = alloc_INSN_LIST (insn, NULL_RTX);
     });
  EXECUTE_IF_SET_IN_REG_SET
    (reg_pending_clobbers, 0, i,
     {
       deps->reg_last_clobbers[i]
	 = alloc_INSN_LIST (insn, deps->reg_last_clobbers[i]);
     });
  CLEAR_REG_SET (reg_pending_sets);
  CLEAR_REG_SET (reg_pending_clobbers);

  if (reg_pending_sets_all)
    {
      for (i = 0; i < maxreg; i++)
	{
	  free_INSN_LIST_list (&deps->reg_last_sets[i]);
	  free_INSN_LIST_list (&deps->reg_last_clobbers[i]);
	  deps->reg_last_sets[i] = alloc_INSN_LIST (insn, NULL_RTX);
	}

      reg_pending_sets_all = 0;
    }

  /* Handle function calls and function returns created by the epilogue
     threading code.  */
  if (GET_CODE (insn) == CALL_INSN || GET_CODE (insn) == JUMP_INSN)
    {
      rtx dep_insn;
      rtx prev_dep_insn;

      /* When scheduling instructions, we make sure calls don't lose their
         accompanying USE insns by depending them one on another in order.

         Also, we must do the same thing for returns created by the epilogue
         threading code.  Note this code works only in this special case,
         because other passes make no guarantee that they will never emit
         an instruction between a USE and a RETURN.  There is such a guarantee
         for USE instructions immediately before a call.  */

      prev_dep_insn = insn;
      dep_insn = PREV_INSN (insn);
      while (GET_CODE (dep_insn) == INSN
	     && GET_CODE (PATTERN (dep_insn)) == USE
	     && GET_CODE (XEXP (PATTERN (dep_insn), 0)) == REG)
	{
	  SCHED_GROUP_P (prev_dep_insn) = 1;

	  /* Make a copy of all dependencies on dep_insn, and add to insn.
	     This is so that all of the dependencies will apply to the
	     group.  */

	  for (link = LOG_LINKS (dep_insn); link; link = XEXP (link, 1))
	    add_dependence (insn, XEXP (link, 0), REG_NOTE_KIND (link));

	  prev_dep_insn = dep_insn;
	  dep_insn = PREV_INSN (dep_insn);
	}
    }
}

/* Analyze every insn between HEAD and TAIL inclusive, creating LOG_LINKS
   for every dependency.  */

static void
sched_analyze (deps, head, tail)
     struct deps *deps;
     rtx head, tail;
{
  register rtx insn;
  register rtx u;
  rtx loop_notes = 0;

  for (insn = head;; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == INSN || GET_CODE (insn) == JUMP_INSN)
	{
	  /* Clear out the stale LOG_LINKS from flow.  */
	  free_INSN_LIST_list (&LOG_LINKS (insn));

	  /* Make each JUMP_INSN a scheduling barrier for memory
             references.  */
	  if (GET_CODE (insn) == JUMP_INSN)
	    deps->last_pending_memory_flush
	      = alloc_INSN_LIST (insn, deps->last_pending_memory_flush);
	  sched_analyze_insn (deps, PATTERN (insn), insn, loop_notes);
	  loop_notes = 0;
	}
      else if (GET_CODE (insn) == CALL_INSN)
	{
	  rtx x;
	  register int i;

	  CANT_MOVE (insn) = 1;

	  /* Clear out the stale LOG_LINKS from flow.  */
	  free_INSN_LIST_list (&LOG_LINKS (insn));

	  /* Any instruction using a hard register which may get clobbered
	     by a call needs to be marked as dependent on this call.
	     This prevents a use of a hard return reg from being moved
	     past a void call (i.e. it does not explicitly set the hard
	     return reg).  */

	  /* If this call is followed by a NOTE_INSN_SETJMP, then assume that
	     all registers, not just hard registers, may be clobbered by this
	     call.  */

	  /* Insn, being a CALL_INSN, magically depends on
	     `last_function_call' already.  */

	  if (NEXT_INSN (insn) && GET_CODE (NEXT_INSN (insn)) == NOTE
	      && NOTE_LINE_NUMBER (NEXT_INSN (insn)) == NOTE_INSN_SETJMP)
	    {
	      int max_reg = max_reg_num ();
	      for (i = 0; i < max_reg; i++)
		{
		  for (u = deps->reg_last_uses[i]; u; u = XEXP (u, 1))
		    add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);
		  free_INSN_LIST_list (&deps->reg_last_uses[i]);

		  for (u = deps->reg_last_sets[i]; u; u = XEXP (u, 1))
		    add_dependence (insn, XEXP (u, 0), 0);

		  for (u = deps->reg_last_clobbers[i]; u; u = XEXP (u, 1))
		    add_dependence (insn, XEXP (u, 0), 0);
		}
	      reg_pending_sets_all = 1;

	      /* Add a pair of REG_SAVE_NOTEs which we will later
		 convert back into a NOTE_INSN_SETJMP note.  See
		 reemit_notes for why we use a pair of NOTEs.  */
	      REG_NOTES (insn) = alloc_EXPR_LIST (REG_SAVE_NOTE,
						  GEN_INT (0),
						  REG_NOTES (insn));
	      REG_NOTES (insn) = alloc_EXPR_LIST (REG_SAVE_NOTE,
						  GEN_INT (NOTE_INSN_SETJMP),
						  REG_NOTES (insn));
	    }
	  else
	    {
	      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
		if (call_used_regs[i] || global_regs[i])
		  {
		    for (u = deps->reg_last_uses[i]; u; u = XEXP (u, 1))
		      add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);

		    for (u = deps->reg_last_sets[i]; u; u = XEXP (u, 1))
		      add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);

		    SET_REGNO_REG_SET (reg_pending_clobbers, i);
		  }
	    }

	  /* For each insn which shouldn't cross a call, add a dependence
	     between that insn and this call insn.  */
	  x = LOG_LINKS (deps->sched_before_next_call);
	  while (x)
	    {
	      add_dependence (insn, XEXP (x, 0), REG_DEP_ANTI);
	      x = XEXP (x, 1);
	    }
	  free_INSN_LIST_list (&LOG_LINKS (deps->sched_before_next_call));

	  sched_analyze_insn (deps, PATTERN (insn), insn, loop_notes);
	  loop_notes = 0;

	  /* In the absence of interprocedural alias analysis, we must flush
	     all pending reads and writes, and start new dependencies starting
	     from here.  But only flush writes for constant calls (which may
	     be passed a pointer to something we haven't written yet).  */
	  flush_pending_lists (deps, insn, CONST_CALL_P (insn));

	  /* Depend this function call (actually, the user of this
	     function call) on all hard register clobberage.  */

	  /* last_function_call is now a list of insns.  */
	  free_INSN_LIST_list (&deps->last_function_call);
	  deps->last_function_call = alloc_INSN_LIST (insn, NULL_RTX);
	}

      /* See comments on reemit_notes as to why we do this.  
	 ??? Actually, the reemit_notes just say what is done, not why.  */

      else if (GET_CODE (insn) == NOTE
	       && (NOTE_LINE_NUMBER (insn) == NOTE_INSN_RANGE_START
		   || NOTE_LINE_NUMBER (insn) == NOTE_INSN_RANGE_END))
	{
	  loop_notes = alloc_EXPR_LIST (REG_SAVE_NOTE, NOTE_RANGE_INFO (insn),
					loop_notes);
	  loop_notes = alloc_EXPR_LIST (REG_SAVE_NOTE,
					GEN_INT (NOTE_LINE_NUMBER (insn)),
					loop_notes);
	}
      else if (GET_CODE (insn) == NOTE
	       && (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_BEG
		   || NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_END
		   || NOTE_LINE_NUMBER (insn) == NOTE_INSN_EH_REGION_BEG
		   || NOTE_LINE_NUMBER (insn) == NOTE_INSN_EH_REGION_END
		   || (NOTE_LINE_NUMBER (insn) == NOTE_INSN_SETJMP
		       && GET_CODE (PREV_INSN (insn)) != CALL_INSN)))
	{
	  rtx rtx_region;

	  if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_EH_REGION_BEG
	      || NOTE_LINE_NUMBER (insn) == NOTE_INSN_EH_REGION_END)
	    rtx_region = GEN_INT (NOTE_EH_HANDLER (insn));
	  else
	    rtx_region = GEN_INT (0);

	  loop_notes = alloc_EXPR_LIST (REG_SAVE_NOTE,
					rtx_region,
					loop_notes);
	  loop_notes = alloc_EXPR_LIST (REG_SAVE_NOTE,
					GEN_INT (NOTE_LINE_NUMBER (insn)),
					loop_notes);
	  CONST_CALL_P (loop_notes) = CONST_CALL_P (insn);
	}

      if (insn == tail)
	return;
    }
  abort ();
}

/* Macros and functions for keeping the priority queue sorted, and
   dealing with queueing and dequeueing of instructions.  */

#define SCHED_SORT(READY, N_READY)                                   \
do { if ((N_READY) == 2)				             \
       swap_sort (READY, N_READY);			             \
     else if ((N_READY) > 2)                                         \
         qsort (READY, N_READY, sizeof (rtx), rank_for_schedule); }  \
while (0)

/* Returns a positive value if x is preferred; returns a negative value if
   y is preferred.  Should never return 0, since that will make the sort
   unstable.  */

static int
rank_for_schedule (x, y)
     const PTR x;
     const PTR y;
{
  rtx tmp = *(rtx *)y;
  rtx tmp2 = *(rtx *)x;
  rtx link;
  int tmp_class, tmp2_class, depend_count1, depend_count2;
  int val, priority_val, spec_val, prob_val, weight_val;


  /* Prefer insn with higher priority.  */
  priority_val = INSN_PRIORITY (tmp2) - INSN_PRIORITY (tmp);
  if (priority_val)
    return priority_val;

  /* Prefer an insn with smaller contribution to registers-pressure.  */
  if (!reload_completed &&
      (weight_val = INSN_REG_WEIGHT (tmp) - INSN_REG_WEIGHT (tmp2)))
    return (weight_val);

  /* Some comparison make sense in interblock scheduling only.  */
  if (INSN_BB (tmp) != INSN_BB (tmp2))
    {
      /* Prefer an inblock motion on an interblock motion.  */
      if ((INSN_BB (tmp2) == target_bb) && (INSN_BB (tmp) != target_bb))
	return 1;
      if ((INSN_BB (tmp) == target_bb) && (INSN_BB (tmp2) != target_bb))
	return -1;

      /* Prefer a useful motion on a speculative one.  */
      if ((spec_val = IS_SPECULATIVE_INSN (tmp) - IS_SPECULATIVE_INSN (tmp2)))
	return (spec_val);

      /* Prefer a more probable (speculative) insn.  */
      prob_val = INSN_PROBABILITY (tmp2) - INSN_PROBABILITY (tmp);
      if (prob_val)
	return (prob_val);
    }

  /* Compare insns based on their relation to the last-scheduled-insn.  */
  if (last_scheduled_insn)
    {
      /* Classify the instructions into three classes:
         1) Data dependent on last schedule insn.
         2) Anti/Output dependent on last scheduled insn.
         3) Independent of last scheduled insn, or has latency of one.
         Choose the insn from the highest numbered class if different.  */
      link = find_insn_list (tmp, INSN_DEPEND (last_scheduled_insn));
      if (link == 0 || insn_cost (last_scheduled_insn, link, tmp) == 1)
	tmp_class = 3;
      else if (REG_NOTE_KIND (link) == 0)	/* Data dependence.  */
	tmp_class = 1;
      else
	tmp_class = 2;

      link = find_insn_list (tmp2, INSN_DEPEND (last_scheduled_insn));
      if (link == 0 || insn_cost (last_scheduled_insn, link, tmp2) == 1)
	tmp2_class = 3;
      else if (REG_NOTE_KIND (link) == 0)	/* Data dependence.  */
	tmp2_class = 1;
      else
	tmp2_class = 2;

      if ((val = tmp2_class - tmp_class))
	return val;
    }

  /* Prefer the insn which has more later insns that depend on it. 
     This gives the scheduler more freedom when scheduling later
     instructions at the expense of added register pressure.  */
  depend_count1 = 0;
  for (link = INSN_DEPEND (tmp); link; link = XEXP (link, 1))
    depend_count1++;

  depend_count2 = 0;
  for (link = INSN_DEPEND (tmp2); link; link = XEXP (link, 1))
    depend_count2++;

  val = depend_count2 - depend_count1;
  if (val)
    return val;
  
  /* If insns are equally good, sort by INSN_LUID (original insn order),
     so that we make the sort stable.  This minimizes instruction movement,
     thus minimizing sched's effect on debugging and cross-jumping.  */
  return INSN_LUID (tmp) - INSN_LUID (tmp2);
}

/* Resort the array A in which only element at index N may be out of order.  */

HAIFA_INLINE static void
swap_sort (a, n)
     rtx *a;
     int n;
{
  rtx insn = a[n - 1];
  int i = n - 2;

  while (i >= 0 && rank_for_schedule (a + i, &insn) >= 0)
    {
      a[i + 1] = a[i];
      i -= 1;
    }
  a[i + 1] = insn;
}

static int max_priority;

/* Add INSN to the insn queue so that it can be executed at least
   N_CYCLES after the currently executing insn.  Preserve insns
   chain for debugging purposes.  */

HAIFA_INLINE static void
queue_insn (insn, n_cycles)
     rtx insn;
     int n_cycles;
{
  int next_q = NEXT_Q_AFTER (q_ptr, n_cycles);
  rtx link = alloc_INSN_LIST (insn, insn_queue[next_q]);
  insn_queue[next_q] = link;
  q_size += 1;

  if (sched_verbose >= 2)
    {
      fprintf (dump, ";;\t\tReady-->Q: insn %d: ", INSN_UID (insn));

      if (INSN_BB (insn) != target_bb)
	fprintf (dump, "(b%d) ", BLOCK_NUM (insn));

      fprintf (dump, "queued for %d cycles.\n", n_cycles);
    }

}

/* PREV is an insn that is ready to execute.  Adjust its priority if that
   will help shorten or lengthen register lifetimes as appropriate.  Also
   provide a hook for the target to tweek itself.  */

HAIFA_INLINE static void
adjust_priority (prev)
     rtx prev ATTRIBUTE_UNUSED;
{
  /* ??? There used to be code here to try and estimate how an insn
     affected register lifetimes, but it did it by looking at REG_DEAD
     notes, which we removed in schedule_region.  Nor did it try to 
     take into account register pressure or anything useful like that.

     Revisit when we have a machine model to work with and not before.  */

#ifdef ADJUST_PRIORITY
  ADJUST_PRIORITY (prev);
#endif
}

/* Clock at which the previous instruction was issued.  */
static int last_clock_var;

/* INSN is the "currently executing insn".  Launch each insn which was
   waiting on INSN.  READY is a vector of insns which are ready to fire.
   N_READY is the number of elements in READY.  CLOCK is the current
   cycle.  */

static int
schedule_insn (insn, ready, n_ready, clock)
     rtx insn;
     rtx *ready;
     int n_ready;
     int clock;
{
  rtx link;
  int unit;

  unit = insn_unit (insn);

  if (sched_verbose >= 2)
    {
      fprintf (dump, ";;\t\t--> scheduling insn <<<%d>>> on unit ",
	       INSN_UID (insn));
      insn_print_units (insn);
      fprintf (dump, "\n");
    }

  if (sched_verbose && unit == -1)
    visualize_no_unit (insn);

  if (MAX_BLOCKAGE > 1 || issue_rate > 1 || sched_verbose)
    schedule_unit (unit, insn, clock);

  if (INSN_DEPEND (insn) == 0)
    return n_ready;

  /* This is used by the function adjust_priority above.  */
  if (n_ready > 0)
    max_priority = MAX (INSN_PRIORITY (ready[0]), INSN_PRIORITY (insn));
  else
    max_priority = INSN_PRIORITY (insn);

  for (link = INSN_DEPEND (insn); link != 0; link = XEXP (link, 1))
    {
      rtx next = XEXP (link, 0);
      int cost = insn_cost (insn, link, next);

      INSN_TICK (next) = MAX (INSN_TICK (next), clock + cost);

      if ((INSN_DEP_COUNT (next) -= 1) == 0)
	{
	  int effective_cost = INSN_TICK (next) - clock;

	  /* For speculative insns, before inserting to ready/queue,
	     check live, exception-free, and issue-delay.  */
	  if (INSN_BB (next) != target_bb
	      && (!IS_VALID (INSN_BB (next))
		  || CANT_MOVE (next)
		  || (IS_SPECULATIVE_INSN (next)
		      && (insn_issue_delay (next) > 3
			  || !check_live (next, INSN_BB (next))
		 || !is_exception_free (next, INSN_BB (next), target_bb)))))
	    continue;

	  if (sched_verbose >= 2)
	    {
	      fprintf (dump, ";;\t\tdependences resolved: insn %d ", 
		       INSN_UID (next));

	      if (current_nr_blocks > 1 && INSN_BB (next) != target_bb)
		fprintf (dump, "/b%d ", BLOCK_NUM (next));

	      if (effective_cost < 1)
		fprintf (dump, "into ready\n");
	      else
		fprintf (dump, "into queue with cost=%d\n", effective_cost);
	    }

	  /* Adjust the priority of NEXT and either put it on the ready
	     list or queue it.  */
	  adjust_priority (next);
	  if (effective_cost < 1)
	    ready[n_ready++] = next;
	  else
	    queue_insn (next, effective_cost);
	}
    }

  /* Annotate the instruction with issue information -- TImode 
     indicates that the instruction is expected not to be able
     to issue on the same cycle as the previous insn.  A machine
     may use this information to decide how the instruction should
     be aligned.  */
  if (reload_completed && issue_rate > 1)
    {
      PUT_MODE (insn, clock > last_clock_var ? TImode : VOIDmode);
      last_clock_var = clock;
    }

  return n_ready;
}

/* Functions for handling of notes.  */

/* Delete notes beginning with INSN and put them in the chain
   of notes ended by NOTE_LIST.
   Returns the insn following the notes.  */

static rtx
unlink_other_notes (insn, tail)
     rtx insn, tail;
{
  rtx prev = PREV_INSN (insn);

  while (insn != tail && GET_CODE (insn) == NOTE)
    {
      rtx next = NEXT_INSN (insn);
      /* Delete the note from its current position.  */
      if (prev)
	NEXT_INSN (prev) = next;
      if (next)
	PREV_INSN (next) = prev;

      /* See sched_analyze to see how these are handled.  */
      if (NOTE_LINE_NUMBER (insn) != NOTE_INSN_SETJMP
	  && NOTE_LINE_NUMBER (insn) != NOTE_INSN_LOOP_BEG
	  && NOTE_LINE_NUMBER (insn) != NOTE_INSN_LOOP_END
	  && NOTE_LINE_NUMBER (insn) != NOTE_INSN_RANGE_START
	  && NOTE_LINE_NUMBER (insn) != NOTE_INSN_RANGE_END
	  && NOTE_LINE_NUMBER (insn) != NOTE_INSN_EH_REGION_BEG
	  && NOTE_LINE_NUMBER (insn) != NOTE_INSN_EH_REGION_END)
	{
	  /* Insert the note at the end of the notes list.  */
	  PREV_INSN (insn) = note_list;
	  if (note_list)
	    NEXT_INSN (note_list) = insn;
	  note_list = insn;
	}

      insn = next;
    }
  return insn;
}

/* Delete line notes beginning with INSN. Record line-number notes so
   they can be reused.  Returns the insn following the notes.  */

static rtx
unlink_line_notes (insn, tail)
     rtx insn, tail;
{
  rtx prev = PREV_INSN (insn);

  while (insn != tail && GET_CODE (insn) == NOTE)
    {
      rtx next = NEXT_INSN (insn);

      if (write_symbols != NO_DEBUG && NOTE_LINE_NUMBER (insn) > 0)
	{
	  /* Delete the note from its current position.  */
	  if (prev)
	    NEXT_INSN (prev) = next;
	  if (next)
	    PREV_INSN (next) = prev;

	  /* Record line-number notes so they can be reused.  */
	  LINE_NOTE (insn) = insn;
	}
      else
	prev = insn;

      insn = next;
    }
  return insn;
}

/* Return the head and tail pointers of BB.  */

HAIFA_INLINE static void
get_block_head_tail (b, headp, tailp)
     int b;
     rtx *headp;
     rtx *tailp;
{

  rtx head;
  rtx tail;

  /* HEAD and TAIL delimit the basic block being scheduled.  */
  head = BLOCK_HEAD (b);
  tail = BLOCK_END (b);

  /* Don't include any notes or labels at the beginning of the
     basic block, or notes at the ends of basic blocks.  */
  while (head != tail)
    {
      if (GET_CODE (head) == NOTE)
	head = NEXT_INSN (head);
      else if (GET_CODE (tail) == NOTE)
	tail = PREV_INSN (tail);
      else if (GET_CODE (head) == CODE_LABEL)
	head = NEXT_INSN (head);
      else
	break;
    }

  *headp = head;
  *tailp = tail;
}

HAIFA_INLINE static void
get_bb_head_tail (bb, headp, tailp)
     int bb;
     rtx *headp;
     rtx *tailp;
{
  get_block_head_tail (BB_TO_BLOCK (bb), headp, tailp);
}

/* Delete line notes from bb. Save them so they can be later restored
   (in restore_line_notes ()).  */

static void
rm_line_notes (bb)
     int bb;
{
  rtx next_tail;
  rtx tail;
  rtx head;
  rtx insn;

  get_bb_head_tail (bb, &head, &tail);

  if (head == tail
      && (GET_RTX_CLASS (GET_CODE (head)) != 'i'))
    return;

  next_tail = NEXT_INSN (tail);
  for (insn = head; insn != next_tail; insn = NEXT_INSN (insn))
    {
      rtx prev;

      /* Farm out notes, and maybe save them in NOTE_LIST.
         This is needed to keep the debugger from
         getting completely deranged.  */
      if (GET_CODE (insn) == NOTE)
	{
	  prev = insn;
	  insn = unlink_line_notes (insn, next_tail);

	  if (prev == tail)
	    abort ();
	  if (prev == head)
	    abort ();
	  if (insn == next_tail)
	    abort ();
	}
    }
}

/* Save line number notes for each insn in bb.  */

static void
save_line_notes (bb)
     int bb;
{
  rtx head, tail;
  rtx next_tail;

  /* We must use the true line number for the first insn in the block
     that was computed and saved at the start of this pass.  We can't
     use the current line number, because scheduling of the previous
     block may have changed the current line number.  */

  rtx line = line_note_head[BB_TO_BLOCK (bb)];
  rtx insn;

  get_bb_head_tail (bb, &head, &tail);
  next_tail = NEXT_INSN (tail);

  for (insn = BLOCK_HEAD (BB_TO_BLOCK (bb));
       insn != next_tail;
       insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == NOTE && NOTE_LINE_NUMBER (insn) > 0)
      line = insn;
    else
      LINE_NOTE (insn) = line;
}


/* After bb was scheduled, insert line notes into the insns list.  */

static void
restore_line_notes (bb)
     int bb;
{
  rtx line, note, prev, new;
  int added_notes = 0;
  int b;
  rtx head, next_tail, insn;

  b = BB_TO_BLOCK (bb);

  head = BLOCK_HEAD (b);
  next_tail = NEXT_INSN (BLOCK_END (b));

  /* Determine the current line-number.  We want to know the current
     line number of the first insn of the block here, in case it is
     different from the true line number that was saved earlier.  If
     different, then we need a line number note before the first insn
     of this block.  If it happens to be the same, then we don't want to
     emit another line number note here.  */
  for (line = head; line; line = PREV_INSN (line))
    if (GET_CODE (line) == NOTE && NOTE_LINE_NUMBER (line) > 0)
      break;

  /* Walk the insns keeping track of the current line-number and inserting
     the line-number notes as needed.  */
  for (insn = head; insn != next_tail; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == NOTE && NOTE_LINE_NUMBER (insn) > 0)
      line = insn;
  /* This used to emit line number notes before every non-deleted note.
     However, this confuses a debugger, because line notes not separated
     by real instructions all end up at the same address.  I can find no
     use for line number notes before other notes, so none are emitted.  */
    else if (GET_CODE (insn) != NOTE
	     && (note = LINE_NOTE (insn)) != 0
	     && note != line
	     && (line == 0
		 || NOTE_LINE_NUMBER (note) != NOTE_LINE_NUMBER (line)
		 || NOTE_SOURCE_FILE (note) != NOTE_SOURCE_FILE (line)))
      {
	line = note;
	prev = PREV_INSN (insn);
	if (LINE_NOTE (note))
	  {
	    /* Re-use the original line-number note.  */
	    LINE_NOTE (note) = 0;
	    PREV_INSN (note) = prev;
	    NEXT_INSN (prev) = note;
	    PREV_INSN (insn) = note;
	    NEXT_INSN (note) = insn;
	  }
	else
	  {
	    added_notes++;
	    new = emit_note_after (NOTE_LINE_NUMBER (note), prev);
	    NOTE_SOURCE_FILE (new) = NOTE_SOURCE_FILE (note);
	    RTX_INTEGRATED_P (new) = RTX_INTEGRATED_P (note);
	  }
      }
  if (sched_verbose && added_notes)
    fprintf (dump, ";; added %d line-number notes\n", added_notes);
}

/* After scheduling the function, delete redundant line notes from the
   insns list.  */

static void
rm_redundant_line_notes ()
{
  rtx line = 0;
  rtx insn = get_insns ();
  int active_insn = 0;
  int notes = 0;

  /* Walk the insns deleting redundant line-number notes.  Many of these
     are already present.  The remainder tend to occur at basic
     block boundaries.  */
  for (insn = get_last_insn (); insn; insn = PREV_INSN (insn))
    if (GET_CODE (insn) == NOTE && NOTE_LINE_NUMBER (insn) > 0)
      {
	/* If there are no active insns following, INSN is redundant.  */
	if (active_insn == 0)
	  {
	    notes++;
	    NOTE_SOURCE_FILE (insn) = 0;
	    NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
	  }
	/* If the line number is unchanged, LINE is redundant.  */
	else if (line
		 && NOTE_LINE_NUMBER (line) == NOTE_LINE_NUMBER (insn)
		 && NOTE_SOURCE_FILE (line) == NOTE_SOURCE_FILE (insn))
	  {
	    notes++;
	    NOTE_SOURCE_FILE (line) = 0;
	    NOTE_LINE_NUMBER (line) = NOTE_INSN_DELETED;
	    line = insn;
	  }
	else
	  line = insn;
	active_insn = 0;
      }
    else if (!((GET_CODE (insn) == NOTE
		&& NOTE_LINE_NUMBER (insn) == NOTE_INSN_DELETED)
	       || (GET_CODE (insn) == INSN
		   && (GET_CODE (PATTERN (insn)) == USE
		       || GET_CODE (PATTERN (insn)) == CLOBBER))))
      active_insn++;

  if (sched_verbose && notes)
    fprintf (dump, ";; deleted %d line-number notes\n", notes);
}

/* Delete notes between head and tail and put them in the chain
   of notes ended by NOTE_LIST.  */

static void
rm_other_notes (head, tail)
     rtx head;
     rtx tail;
{
  rtx next_tail;
  rtx insn;

  if (head == tail
      && (GET_RTX_CLASS (GET_CODE (head)) != 'i'))
    return;

  next_tail = NEXT_INSN (tail);
  for (insn = head; insn != next_tail; insn = NEXT_INSN (insn))
    {
      rtx prev;

      /* Farm out notes, and maybe save them in NOTE_LIST.
         This is needed to keep the debugger from
         getting completely deranged.  */
      if (GET_CODE (insn) == NOTE)
	{
	  prev = insn;

	  insn = unlink_other_notes (insn, next_tail);

	  if (prev == tail)
	    abort ();
	  if (prev == head)
	    abort ();
	  if (insn == next_tail)
	    abort ();
	}
    }
}

/* Functions for computation of registers live/usage info.  */

/* Calculate INSN_REG_WEIGHT for all insns of a block.  */

static void
find_insn_reg_weight (b)
    int b;
{
  rtx insn, next_tail, head, tail;

  get_block_head_tail (b, &head, &tail);
  next_tail = NEXT_INSN (tail);

  for (insn = head; insn != next_tail; insn = NEXT_INSN (insn))
    {
      int reg_weight = 0;
      rtx x;

      /* Handle register life information.  */
      if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
	continue;

      /* Increment weight for each register born here.  */
      x = PATTERN (insn);
      if ((GET_CODE (x) == SET || GET_CODE (x) == CLOBBER)
	  && register_operand (SET_DEST (x), VOIDmode))
	reg_weight++;
      else if (GET_CODE (x) == PARALLEL)
	{
	  int j;
	  for (j = XVECLEN (x, 0) - 1; j >= 0; j--)
	    {
	      x = XVECEXP (PATTERN (insn), 0, j);
	      if ((GET_CODE (x) == SET || GET_CODE (x) == CLOBBER)
		  && register_operand (SET_DEST (x), VOIDmode))
		reg_weight++;
	    }
	}

      /* Decrement weight for each register that dies here.  */
      for (x = REG_NOTES (insn); x; x = XEXP (x, 1))
	{
	  if (REG_NOTE_KIND (x) == REG_DEAD
	      || REG_NOTE_KIND (x) == REG_UNUSED)
	    reg_weight--;
	}

      INSN_REG_WEIGHT (insn) = reg_weight;
    }
}

/* Scheduling clock, modified in schedule_block() and queue_to_ready ().  */
static int clock_var;

/* Move insns that became ready to fire from queue to ready list.  */

static int
queue_to_ready (ready, n_ready)
     rtx ready[];
     int n_ready;
{
  rtx insn;
  rtx link;

  q_ptr = NEXT_Q (q_ptr);

  /* Add all pending insns that can be scheduled without stalls to the
     ready list.  */
  for (link = insn_queue[q_ptr]; link; link = XEXP (link, 1))
    {

      insn = XEXP (link, 0);
      q_size -= 1;

      if (sched_verbose >= 2)
	fprintf (dump, ";;\t\tQ-->Ready: insn %d: ", INSN_UID (insn));

      if (sched_verbose >= 2 && INSN_BB (insn) != target_bb)
	fprintf (dump, "(b%d) ", BLOCK_NUM (insn));

      ready[n_ready++] = insn;
      if (sched_verbose >= 2)
	fprintf (dump, "moving to ready without stalls\n");
    }
  insn_queue[q_ptr] = 0;

  /* If there are no ready insns, stall until one is ready and add all
     of the pending insns at that point to the ready list.  */
  if (n_ready == 0)
    {
      register int stalls;

      for (stalls = 1; stalls < INSN_QUEUE_SIZE; stalls++)
	{
	  if ((link = insn_queue[NEXT_Q_AFTER (q_ptr, stalls)]))
	    {
	      for (; link; link = XEXP (link, 1))
		{
		  insn = XEXP (link, 0);
		  q_size -= 1;

		  if (sched_verbose >= 2)
		    fprintf (dump, ";;\t\tQ-->Ready: insn %d: ", INSN_UID (insn));

		  if (sched_verbose >= 2 && INSN_BB (insn) != target_bb)
		    fprintf (dump, "(b%d) ", BLOCK_NUM (insn));

		  ready[n_ready++] = insn;
		  if (sched_verbose >= 2)
		    fprintf (dump, "moving to ready with %d stalls\n", stalls);
		}
	      insn_queue[NEXT_Q_AFTER (q_ptr, stalls)] = 0;

	      if (n_ready)
		break;
	    }
	}

      if (sched_verbose && stalls)
	visualize_stall_cycles (BB_TO_BLOCK (target_bb), stalls);
      q_ptr = NEXT_Q_AFTER (q_ptr, stalls);
      clock_var += stalls;
    }
  return n_ready;
}

/* Print the ready list for debugging purposes.  Callable from debugger.  */

static void
debug_ready_list (ready, n_ready)
     rtx ready[];
     int n_ready;
{
  int i;

  for (i = 0; i < n_ready; i++)
    {
      fprintf (dump, "  %d", INSN_UID (ready[i]));
      if (current_nr_blocks > 1 && INSN_BB (ready[i]) != target_bb)
	fprintf (dump, "/b%d", BLOCK_NUM (ready[i]));
    }
  fprintf (dump, "\n");
}

/* Print names of units on which insn can/should execute, for debugging.  */

static void
insn_print_units (insn)
     rtx insn;
{
  int i;
  int unit = insn_unit (insn);

  if (unit == -1)
    fprintf (dump, "none");
  else if (unit >= 0)
    fprintf (dump, "%s", function_units[unit].name);
  else
    {
      fprintf (dump, "[");
      for (i = 0, unit = ~unit; unit; i++, unit >>= 1)
	if (unit & 1)
	  {
	    fprintf (dump, "%s", function_units[i].name);
	    if (unit != 1)
	      fprintf (dump, " ");
	  }
      fprintf (dump, "]");
    }
}

/* MAX_VISUAL_LINES is the maximum number of lines in visualization table
   of a basic block.  If more lines are needed, table is splitted to two.
   n_visual_lines is the number of lines printed so far for a block.
   visual_tbl contains the block visualization info.
   vis_no_unit holds insns in a cycle that are not mapped to any unit.  */
#define MAX_VISUAL_LINES 100
#define INSN_LEN 30
int n_visual_lines;
char *visual_tbl;
int n_vis_no_unit;
rtx vis_no_unit[10];

/* Finds units that are in use in this fuction.  Required only
   for visualization.  */

static void
init_target_units ()
{
  rtx insn;
  int unit;

  for (insn = get_last_insn (); insn; insn = PREV_INSN (insn))
    {
      if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
	continue;

      unit = insn_unit (insn);

      if (unit < 0)
	target_units |= ~unit;
      else
	target_units |= (1 << unit);
    }
}

/* Return the length of the visualization table.  */

static int
get_visual_tbl_length ()
{
  int unit, i;
  int n, n1;
  char *s;

  /* Compute length of one field in line.  */
  s = (char *) alloca (INSN_LEN + 6);
  sprintf (s, "  %33s", "uname");
  n1 = strlen (s);

  /* Compute length of one line.  */
  n = strlen (";; ");
  n += n1;
  for (unit = 0; unit < FUNCTION_UNITS_SIZE; unit++)
    if (function_units[unit].bitmask & target_units)
      for (i = 0; i < function_units[unit].multiplicity; i++)
	n += n1;
  n += n1;
  n += strlen ("\n") + 2;

  /* Compute length of visualization string.  */
  return (MAX_VISUAL_LINES * n);
}

/* Init block visualization debugging info.  */

static void
init_block_visualization ()
{
  strcpy (visual_tbl, "");
  n_visual_lines = 0;
  n_vis_no_unit = 0;
}

#define BUF_LEN 256

static char *
safe_concat (buf, cur, str)
     char *buf;
     char *cur;
     const char *str;
{
  char *end = buf + BUF_LEN - 2;	/* Leave room for null.  */
  int c;

  if (cur > end)
    {
      *end = '\0';
      return end;
    }

  while (cur < end && (c = *str++) != '\0')
    *cur++ = c;

  *cur = '\0';
  return cur;
}

/* This recognizes rtx, I classified as expressions.  These are always
   represent some action on values or results of other expression, that
   may be stored in objects representing values.  */

static void
print_exp (buf, x, verbose)
     char *buf;
     rtx x;
     int verbose;
{
  char tmp[BUF_LEN];
  const char *st[4];
  char *cur = buf;
  const char *fun = (char *)0;
  const char *sep;
  rtx op[4];
  int i;

  for (i = 0; i < 4; i++)
    {
      st[i] = (char *)0;
      op[i] = NULL_RTX;
    }

  switch (GET_CODE (x))
    {
    case PLUS:
      op[0] = XEXP (x, 0);
      if (GET_CODE (XEXP (x, 1)) == CONST_INT
	  && INTVAL (XEXP (x, 1)) < 0)
	{
	  st[1] = "-";
	  op[1] = GEN_INT (-INTVAL (XEXP (x, 1)));
	}
      else
	{
	  st[1] = "+";
	  op[1] = XEXP (x, 1);
	}
      break;
    case LO_SUM:
      op[0] = XEXP (x, 0);
      st[1] = "+low(";
      op[1] = XEXP (x, 1);
      st[2] = ")";
      break;
    case MINUS:
      op[0] = XEXP (x, 0);
      st[1] = "-";
      op[1] = XEXP (x, 1);
      break;
    case COMPARE:
      fun = "cmp";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      break;
    case NEG:
      st[0] = "-";
      op[0] = XEXP (x, 0);
      break;
    case MULT:
      op[0] = XEXP (x, 0);
      st[1] = "*";
      op[1] = XEXP (x, 1);
      break;
    case DIV:
      op[0] = XEXP (x, 0);
      st[1] = "/";
      op[1] = XEXP (x, 1);
      break;
    case UDIV:
      fun = "udiv";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      break;
    case MOD:
      op[0] = XEXP (x, 0);
      st[1] = "%";
      op[1] = XEXP (x, 1);
      break;
    case UMOD:
      fun = "umod";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      break;
    case SMIN:
      fun = "smin";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      break;
    case SMAX:
      fun = "smax";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      break;
    case UMIN:
      fun = "umin";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      break;
    case UMAX:
      fun = "umax";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      break;
    case NOT:
      st[0] = "!";
      op[0] = XEXP (x, 0);
      break;
    case AND:
      op[0] = XEXP (x, 0);
      st[1] = "&";
      op[1] = XEXP (x, 1);
      break;
    case IOR:
      op[0] = XEXP (x, 0);
      st[1] = "|";
      op[1] = XEXP (x, 1);
      break;
    case XOR:
      op[0] = XEXP (x, 0);
      st[1] = "^";
      op[1] = XEXP (x, 1);
      break;
    case ASHIFT:
      op[0] = XEXP (x, 0);
      st[1] = "<<";
      op[1] = XEXP (x, 1);
      break;
    case LSHIFTRT:
      op[0] = XEXP (x, 0);
      st[1] = " 0>>";
      op[1] = XEXP (x, 1);
      break;
    case ASHIFTRT:
      op[0] = XEXP (x, 0);
      st[1] = ">>";
      op[1] = XEXP (x, 1);
      break;
    case ROTATE:
      op[0] = XEXP (x, 0);
      st[1] = "<-<";
      op[1] = XEXP (x, 1);
      break;
    case ROTATERT:
      op[0] = XEXP (x, 0);
      st[1] = ">->";
      op[1] = XEXP (x, 1);
      break;
    case ABS:
      fun = "abs";
      op[0] = XEXP (x, 0);
      break;
    case SQRT:
      fun = "sqrt";
      op[0] = XEXP (x, 0);
      break;
    case FFS:
      fun = "ffs";
      op[0] = XEXP (x, 0);
      break;
    case EQ:
      op[0] = XEXP (x, 0);
      st[1] = "==";
      op[1] = XEXP (x, 1);
      break;
    case NE:
      op[0] = XEXP (x, 0);
      st[1] = "!=";
      op[1] = XEXP (x, 1);
      break;
    case GT:
      op[0] = XEXP (x, 0);
      st[1] = ">";
      op[1] = XEXP (x, 1);
      break;
    case GTU:
      fun = "gtu";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      break;
    case LT:
      op[0] = XEXP (x, 0);
      st[1] = "<";
      op[1] = XEXP (x, 1);
      break;
    case LTU:
      fun = "ltu";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      break;
    case GE:
      op[0] = XEXP (x, 0);
      st[1] = ">=";
      op[1] = XEXP (x, 1);
      break;
    case GEU:
      fun = "geu";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      break;
    case LE:
      op[0] = XEXP (x, 0);
      st[1] = "<=";
      op[1] = XEXP (x, 1);
      break;
    case LEU:
      fun = "leu";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      break;
    case SIGN_EXTRACT:
      fun = (verbose) ? "sign_extract" : "sxt";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      op[2] = XEXP (x, 2);
      break;
    case ZERO_EXTRACT:
      fun = (verbose) ? "zero_extract" : "zxt";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      op[2] = XEXP (x, 2);
      break;
    case SIGN_EXTEND:
      fun = (verbose) ? "sign_extend" : "sxn";
      op[0] = XEXP (x, 0);
      break;
    case ZERO_EXTEND:
      fun = (verbose) ? "zero_extend" : "zxn";
      op[0] = XEXP (x, 0);
      break;
    case FLOAT_EXTEND:
      fun = (verbose) ? "float_extend" : "fxn";
      op[0] = XEXP (x, 0);
      break;
    case TRUNCATE:
      fun = (verbose) ? "trunc" : "trn";
      op[0] = XEXP (x, 0);
      break;
    case FLOAT_TRUNCATE:
      fun = (verbose) ? "float_trunc" : "ftr";
      op[0] = XEXP (x, 0);
      break;
    case FLOAT:
      fun = (verbose) ? "float" : "flt";
      op[0] = XEXP (x, 0);
      break;
    case UNSIGNED_FLOAT:
      fun = (verbose) ? "uns_float" : "ufl";
      op[0] = XEXP (x, 0);
      break;
    case FIX:
      fun = "fix";
      op[0] = XEXP (x, 0);
      break;
    case UNSIGNED_FIX:
      fun = (verbose) ? "uns_fix" : "ufx";
      op[0] = XEXP (x, 0);
      break;
    case PRE_DEC:
      st[0] = "--";
      op[0] = XEXP (x, 0);
      break;
    case PRE_INC:
      st[0] = "++";
      op[0] = XEXP (x, 0);
      break;
    case POST_DEC:
      op[0] = XEXP (x, 0);
      st[1] = "--";
      break;
    case POST_INC:
      op[0] = XEXP (x, 0);
      st[1] = "++";
      break;
    case CALL:
      st[0] = "call ";
      op[0] = XEXP (x, 0);
      if (verbose)
	{
	  st[1] = " argc:";
	  op[1] = XEXP (x, 1);
	}
      break;
    case IF_THEN_ELSE:
      st[0] = "{(";
      op[0] = XEXP (x, 0);
      st[1] = ")?";
      op[1] = XEXP (x, 1);
      st[2] = ":";
      op[2] = XEXP (x, 2);
      st[3] = "}";
      break;
    case TRAP_IF:
      fun = "trap_if";
      op[0] = TRAP_CONDITION (x);
      break;
    case UNSPEC:
    case UNSPEC_VOLATILE:
      {
	cur = safe_concat (buf, cur, "unspec");
	if (GET_CODE (x) == UNSPEC_VOLATILE)
	  cur = safe_concat (buf, cur, "/v");
	cur = safe_concat (buf, cur, "[");
	sep = "";
	for (i = 0; i < XVECLEN (x, 0); i++)
	  {
	    print_pattern (tmp, XVECEXP (x, 0, i), verbose);
	    cur = safe_concat (buf, cur, sep);
	    cur = safe_concat (buf, cur, tmp);
	    sep = ",";
	  }
	cur = safe_concat (buf, cur, "] ");
	sprintf (tmp, "%d", XINT (x, 1));
	cur = safe_concat (buf, cur, tmp);
      }
      break;
    default:
      /* If (verbose) debug_rtx (x);  */
      st[0] = GET_RTX_NAME (GET_CODE (x));
      break;
    }

  /* Print this as a function?  */
  if (fun)
    {
      cur = safe_concat (buf, cur, fun);
      cur = safe_concat (buf, cur, "(");
    }

  for (i = 0; i < 4; i++)
    {
      if (st[i])
	cur = safe_concat (buf, cur, st[i]);

      if (op[i])
	{
	  if (fun && i != 0)
	    cur = safe_concat (buf, cur, ",");

	  print_value (tmp, op[i], verbose);
	  cur = safe_concat (buf, cur, tmp);
	}
    }

  if (fun)
    cur = safe_concat (buf, cur, ")");
}		/* print_exp */

/* Prints rtxes, I customly classified as values.  They're constants,
   registers, labels, symbols and memory accesses.  */

static void
print_value (buf, x, verbose)
     char *buf;
     rtx x;
     int verbose;
{
  char t[BUF_LEN];
  char *cur = buf;

  switch (GET_CODE (x))
    {
    case CONST_INT:
      sprintf (t, HOST_WIDE_INT_PRINT_HEX, INTVAL (x));
      cur = safe_concat (buf, cur, t);
      break;
    case CONST_DOUBLE:
      sprintf (t, "<0x%lx,0x%lx>", (long)XWINT (x, 2), (long)XWINT (x, 3));
      cur = safe_concat (buf, cur, t);
      break;
    case CONST_STRING:
      cur = safe_concat (buf, cur, "\"");
      cur = safe_concat (buf, cur, XSTR (x, 0));
      cur = safe_concat (buf, cur, "\"");
      break;
    case SYMBOL_REF:
      cur = safe_concat (buf, cur, "`");
      cur = safe_concat (buf, cur, XSTR (x, 0));
      cur = safe_concat (buf, cur, "'");
      break;
    case LABEL_REF:
      sprintf (t, "L%d", INSN_UID (XEXP (x, 0)));
      cur = safe_concat (buf, cur, t);
      break;
    case CONST:
      print_value (t, XEXP (x, 0), verbose);
      cur = safe_concat (buf, cur, "const(");
      cur = safe_concat (buf, cur, t);
      cur = safe_concat (buf, cur, ")");
      break;
    case HIGH:
      print_value (t, XEXP (x, 0), verbose);
      cur = safe_concat (buf, cur, "high(");
      cur = safe_concat (buf, cur, t);
      cur = safe_concat (buf, cur, ")");
      break;
    case REG:
      if (REGNO (x) < FIRST_PSEUDO_REGISTER)
	{
	  int c = reg_names[ REGNO (x) ][0];
	  if (c >= '0' && c <= '9')
	    cur = safe_concat (buf, cur, "%");

	  cur = safe_concat (buf, cur, reg_names[ REGNO (x) ]);
	}
      else
	{
	  sprintf (t, "r%d", REGNO (x));
	  cur = safe_concat (buf, cur, t);
	}
      break;
    case SUBREG:
      print_value (t, SUBREG_REG (x), verbose);
      cur = safe_concat (buf, cur, t);
      sprintf (t, "#%d", SUBREG_WORD (x));
      cur = safe_concat (buf, cur, t);
      break;
    case SCRATCH:
      cur = safe_concat (buf, cur, "scratch");
      break;
    case CC0:
      cur = safe_concat (buf, cur, "cc0");
      break;
    case PC:
      cur = safe_concat (buf, cur, "pc");
      break;
    case MEM:
      print_value (t, XEXP (x, 0), verbose);
      cur = safe_concat (buf, cur, "[");
      cur = safe_concat (buf, cur, t);
      cur = safe_concat (buf, cur, "]");
      break;
    default:
      print_exp (t, x, verbose);
      cur = safe_concat (buf, cur, t);
      break;
    }
}				/* print_value */

/* The next step in insn detalization, its pattern recognition.  */

static void
print_pattern (buf, x, verbose)
     char *buf;
     rtx x;
     int verbose;
{
  char t1[BUF_LEN], t2[BUF_LEN], t3[BUF_LEN];

  switch (GET_CODE (x))
    {
    case SET:
      print_value (t1, SET_DEST (x), verbose);
      print_value (t2, SET_SRC (x), verbose);
      sprintf (buf, "%s=%s", t1, t2);
      break;
    case RETURN:
      sprintf (buf, "return");
      break;
    case CALL:
      print_exp (buf, x, verbose);
      break;
    case CLOBBER:
      print_value (t1, XEXP (x, 0), verbose);
      sprintf (buf, "clobber %s", t1);
      break;
    case USE:
      print_value (t1, XEXP (x, 0), verbose);
      sprintf (buf, "use %s", t1);
      break;
    case PARALLEL:
      {
	int i;

	sprintf (t1, "{");
	for (i = 0; i < XVECLEN (x, 0); i++)
	  {
	    print_pattern (t2, XVECEXP (x, 0, i), verbose);
	    sprintf (t3, "%s%s;", t1, t2);
	    strcpy (t1, t3);
	  }
	sprintf (buf, "%s}", t1);
      }
      break;
    case SEQUENCE:
      {
	int i;

	sprintf (t1, "%%{");
	for (i = 0; i < XVECLEN (x, 0); i++)
	  {
	    print_insn (t2, XVECEXP (x, 0, i), verbose);
	    sprintf (t3, "%s%s;", t1, t2);
	    strcpy (t1, t3);
	  }
	sprintf (buf, "%s%%}", t1);
      }
      break;
    case ASM_INPUT:
      sprintf (buf, "asm {%s}", XSTR (x, 0));
      break;
    case ADDR_VEC:
      break;
    case ADDR_DIFF_VEC:
      print_value (buf, XEXP (x, 0), verbose);
      break;
    case TRAP_IF:
      print_value (t1, TRAP_CONDITION (x), verbose);
      sprintf (buf, "trap_if %s", t1);
      break;
    case UNSPEC:
      {
	int i;

	sprintf (t1, "unspec{");
	for (i = 0; i < XVECLEN (x, 0); i++)
	  {
	    print_pattern (t2, XVECEXP (x, 0, i), verbose);
	    sprintf (t3, "%s%s;", t1, t2);
	    strcpy (t1, t3);
	  }
	sprintf (buf, "%s}", t1);
      }
      break;
    case UNSPEC_VOLATILE:
      {
	int i;

	sprintf (t1, "unspec/v{");
	for (i = 0; i < XVECLEN (x, 0); i++)
	  {
	    print_pattern (t2, XVECEXP (x, 0, i), verbose);
	    sprintf (t3, "%s%s;", t1, t2);
	    strcpy (t1, t3);
	  }
	sprintf (buf, "%s}", t1);
      }
      break;
    default:
      print_value (buf, x, verbose);
    }
}				/* print_pattern */

/* This is the main function in rtl visualization mechanism. It
   accepts an rtx and tries to recognize it as an insn, then prints it
   properly in human readable form, resembling assembler mnemonics.
   For every insn it prints its UID and BB the insn belongs too.
   (Probably the last "option" should be extended somehow, since it
   depends now on sched.c inner variables ...)  */

static void
print_insn (buf, x, verbose)
     char *buf;
     rtx x;
     int verbose;
{
  char t[BUF_LEN];
  rtx insn = x;

  switch (GET_CODE (x))
    {
    case INSN:
      print_pattern (t, PATTERN (x), verbose);
      if (verbose)
	sprintf (buf, "b%d: i% 4d: %s", INSN_BB (x),
		 INSN_UID (x), t);
      else
	sprintf (buf, "%-4d %s", INSN_UID (x), t);
      break;
    case JUMP_INSN:
      print_pattern (t, PATTERN (x), verbose);
      if (verbose)
	sprintf (buf, "b%d: i% 4d: jump %s", INSN_BB (x),
		 INSN_UID (x), t);
      else
	sprintf (buf, "%-4d %s", INSN_UID (x), t);
      break;
    case CALL_INSN:
      x = PATTERN (insn);
      if (GET_CODE (x) == PARALLEL)
	{
	  x = XVECEXP (x, 0, 0);
	  print_pattern (t, x, verbose);
	}
      else
	strcpy (t, "call <...>");
      if (verbose)
	sprintf (buf, "b%d: i% 4d: %s", INSN_BB (insn),
		 INSN_UID (insn), t);
      else
	sprintf (buf, "%-4d %s", INSN_UID (insn), t);
      break;
    case CODE_LABEL:
      sprintf (buf, "L%d:", INSN_UID (x));
      break;
    case BARRIER:
      sprintf (buf, "i% 4d: barrier", INSN_UID (x));
      break;
    case NOTE:
      if (NOTE_LINE_NUMBER (x) > 0)
	sprintf (buf, "%4d note \"%s\" %d", INSN_UID (x),
		 NOTE_SOURCE_FILE (x), NOTE_LINE_NUMBER (x));
      else
	sprintf (buf, "%4d %s", INSN_UID (x),
		 GET_NOTE_INSN_NAME (NOTE_LINE_NUMBER (x)));
      break;
    default:
      if (verbose)
	{
	  sprintf (buf, "Not an INSN at all\n");
	  debug_rtx (x);
	}
      else
	sprintf (buf, "i%-4d  <What?>", INSN_UID (x));
    }
}				/* print_insn */

/* Print visualization debugging info.  */

static void
print_block_visualization (b, s)
     int b;
     const char *s;
{
  int unit, i;

  /* Print header.  */
  fprintf (dump, "\n;;   ==================== scheduling visualization for block %d %s \n", b, s);

  /* Print names of units.  */
  fprintf (dump, ";;   %-8s", "clock");
  for (unit = 0; unit < FUNCTION_UNITS_SIZE; unit++)
    if (function_units[unit].bitmask & target_units)
      for (i = 0; i < function_units[unit].multiplicity; i++)
	fprintf (dump, "  %-33s", function_units[unit].name);
  fprintf (dump, "  %-8s\n", "no-unit");

  fprintf (dump, ";;   %-8s", "=====");
  for (unit = 0; unit < FUNCTION_UNITS_SIZE; unit++)
    if (function_units[unit].bitmask & target_units)
      for (i = 0; i < function_units[unit].multiplicity; i++)
	fprintf (dump, "  %-33s", "==============================");
  fprintf (dump, "  %-8s\n", "=======");

  /* Print insns in each cycle.  */
  fprintf (dump, "%s\n", visual_tbl);
}

/* Print insns in the 'no_unit' column of visualization.  */

static void
visualize_no_unit (insn)
     rtx insn;
{
  vis_no_unit[n_vis_no_unit] = insn;
  n_vis_no_unit++;
}

/* Print insns scheduled in clock, for visualization.  */

static void
visualize_scheduled_insns (b, clock)
     int b, clock;
{
  int i, unit;

  /* If no more room, split table into two.  */
  if (n_visual_lines >= MAX_VISUAL_LINES)
    {
      print_block_visualization (b, "(incomplete)");
      init_block_visualization ();
    }

  n_visual_lines++;

  sprintf (visual_tbl + strlen (visual_tbl), ";;   %-8d", clock);
  for (unit = 0; unit < FUNCTION_UNITS_SIZE; unit++)
    if (function_units[unit].bitmask & target_units)
      for (i = 0; i < function_units[unit].multiplicity; i++)
	{
	  int instance = unit + i * FUNCTION_UNITS_SIZE;
	  rtx insn = unit_last_insn[instance];

	  /* Print insns that still keep the unit busy.  */
	  if (insn &&
	      actual_hazard_this_instance (unit, instance, insn, clock, 0))
	    {
	      char str[BUF_LEN];
	      print_insn (str, insn, 0);
	      str[INSN_LEN] = '\0';
	      sprintf (visual_tbl + strlen (visual_tbl), "  %-33s", str);
	    }
	  else
	    sprintf (visual_tbl + strlen (visual_tbl), "  %-33s", "------------------------------");
	}

  /* Print insns that are not assigned to any unit.  */
  for (i = 0; i < n_vis_no_unit; i++)
    sprintf (visual_tbl + strlen (visual_tbl), "  %-8d",
	     INSN_UID (vis_no_unit[i]));
  n_vis_no_unit = 0;

  sprintf (visual_tbl + strlen (visual_tbl), "\n");
}

/* Print stalled cycles.  */

static void
visualize_stall_cycles (b, stalls)
     int b, stalls;
{
  int i;

  /* If no more room, split table into two.  */
  if (n_visual_lines >= MAX_VISUAL_LINES)
    {
      print_block_visualization (b, "(incomplete)");
      init_block_visualization ();
    }

  n_visual_lines++;

  sprintf (visual_tbl + strlen (visual_tbl), ";;       ");
  for (i = 0; i < stalls; i++)
    sprintf (visual_tbl + strlen (visual_tbl), ".");
  sprintf (visual_tbl + strlen (visual_tbl), "\n");
}

/* move_insn1: Remove INSN from insn chain, and link it after LAST insn.  */

static rtx
move_insn1 (insn, last)
     rtx insn, last;
{
  NEXT_INSN (PREV_INSN (insn)) = NEXT_INSN (insn);
  PREV_INSN (NEXT_INSN (insn)) = PREV_INSN (insn);

  NEXT_INSN (insn) = NEXT_INSN (last);
  PREV_INSN (NEXT_INSN (last)) = insn;

  NEXT_INSN (last) = insn;
  PREV_INSN (insn) = last;

  return insn;
}

/* Search INSN for REG_SAVE_NOTE note pairs for NOTE_INSN_SETJMP,
   NOTE_INSN_{LOOP,EHREGION}_{BEG,END}; and convert them back into
   NOTEs.  The REG_SAVE_NOTE note following first one is contains the
   saved value for NOTE_BLOCK_NUMBER which is useful for
   NOTE_INSN_EH_REGION_{BEG,END} NOTEs.  LAST is the last instruction
   output by the instruction scheduler.  Return the new value of LAST.  */

static rtx
reemit_notes (insn, last)
     rtx insn;
     rtx last;
{
  rtx note, retval;

  retval = last;
  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
    {
      if (REG_NOTE_KIND (note) == REG_SAVE_NOTE)
	{
	  int note_type = INTVAL (XEXP (note, 0));
	  if (note_type == NOTE_INSN_SETJMP)
	    {
	      retval = emit_note_after (NOTE_INSN_SETJMP, insn);
	      CONST_CALL_P (retval) = CONST_CALL_P (note);
	      remove_note (insn, note);
	      note = XEXP (note, 1);
	    }
	  else if (note_type == NOTE_INSN_RANGE_START
                   || note_type == NOTE_INSN_RANGE_END)
	    {
	      last = emit_note_before (note_type, last);
	      remove_note (insn, note);
	      note = XEXP (note, 1);
	      NOTE_RANGE_INFO (last) = XEXP (note, 0);
	    }
	  else
	    {
	      last = emit_note_before (note_type, last);
	      remove_note (insn, note);
	      note = XEXP (note, 1);
	      if (note_type == NOTE_INSN_EH_REGION_BEG
		  || note_type == NOTE_INSN_EH_REGION_END)
		NOTE_EH_HANDLER (last) = INTVAL (XEXP (note, 0));
	    }
	  remove_note (insn, note);
	}
    }
  return retval;
}

/* Move INSN, and all insns which should be issued before it,
   due to SCHED_GROUP_P flag.  Reemit notes if needed.

   Return the last insn emitted by the scheduler, which is the
   return value from the first call to reemit_notes.  */

static rtx
move_insn (insn, last)
     rtx insn, last;
{
  rtx retval = NULL;

  /* If INSN has SCHED_GROUP_P set, then issue it and any other
     insns with SCHED_GROUP_P set first.  */
  while (SCHED_GROUP_P (insn))
    {
      rtx prev = PREV_INSN (insn);

      /* Move a SCHED_GROUP_P insn.  */
      move_insn1 (insn, last);
      /* If this is the first call to reemit_notes, then record
	 its return value.  */
      if (retval == NULL_RTX)
	retval = reemit_notes (insn, insn);
      else
	reemit_notes (insn, insn);
      insn = prev;
    }

  /* Now move the first non SCHED_GROUP_P insn.  */
  move_insn1 (insn, last);

  /* If this is the first call to reemit_notes, then record
     its return value.  */
  if (retval == NULL_RTX)
    retval = reemit_notes (insn, insn);
  else
    reemit_notes (insn, insn);

  return retval;
}

/* Return an insn which represents a SCHED_GROUP, which is
   the last insn in the group.  */

static rtx
group_leader (insn)
     rtx insn;
{
  rtx prev;

  do
    {
      prev = insn;
      insn = next_nonnote_insn (insn);
    }
  while (insn && SCHED_GROUP_P (insn) && (GET_CODE (insn) != CODE_LABEL));

  return prev;
}

/* Use forward list scheduling to rearrange insns of block BB in region RGN,
   possibly bringing insns from subsequent blocks in the same region.
   Return number of insns scheduled.  */

static int
schedule_block (bb, rgn_n_insns)
     int bb;
     int rgn_n_insns;
{
  /* Local variables.  */
  rtx insn, last;
  rtx *ready;
  int n_ready = 0;
  int can_issue_more;

  /* Flow block of this bb.  */
  int b = BB_TO_BLOCK (bb);

  /* target_n_insns == number of insns in b before scheduling starts.
     sched_target_n_insns == how many of b's insns were scheduled.
     sched_n_insns == how many insns were scheduled in b.  */
  int target_n_insns = 0;
  int sched_target_n_insns = 0;
  int sched_n_insns = 0;

#define NEED_NOTHING	0
#define NEED_HEAD	1
#define NEED_TAIL	2
  int new_needs;

  /* Head/tail info for this block.  */
  rtx prev_head;
  rtx next_tail;
  rtx head;
  rtx tail;
  int bb_src;

  /* We used to have code to avoid getting parameters moved from hard
     argument registers into pseudos.

     However, it was removed when it proved to be of marginal benefit
     and caused problems because schedule_block and compute_forward_dependences
     had different notions of what the "head" insn was.  */
  get_bb_head_tail (bb, &head, &tail);

  /* Interblock scheduling could have moved the original head insn from this
     block into a proceeding block.  This may also cause schedule_block and
     compute_forward_dependences to have different notions of what the
     "head" insn was.

     If the interblock movement happened to make this block start with
     some notes (LOOP, EH or SETJMP) before the first real insn, then
     HEAD will have various special notes attached to it which must be
     removed so that we don't end up with extra copies of the notes.  */
  if (GET_RTX_CLASS (GET_CODE (head)) == 'i')
    {
      rtx note;

      for (note = REG_NOTES (head); note; note = XEXP (note, 1))
	if (REG_NOTE_KIND (note) == REG_SAVE_NOTE)
	  remove_note (head, note);
    }

  next_tail = NEXT_INSN (tail);
  prev_head = PREV_INSN (head);

  /* If the only insn left is a NOTE or a CODE_LABEL, then there is no need
     to schedule this block.  */
  if (head == tail
      && (GET_RTX_CLASS (GET_CODE (head)) != 'i'))
    return (sched_n_insns);

  /* Debug info.  */
  if (sched_verbose)
    {
      fprintf (dump, ";;   ======================================================\n");
      fprintf (dump,
	       ";;   -- basic block %d from %d to %d -- %s reload\n",
	       b, INSN_UID (BLOCK_HEAD (b)), INSN_UID (BLOCK_END (b)),
	       (reload_completed ? "after" : "before"));
      fprintf (dump, ";;   ======================================================\n");
      fprintf (dump, "\n");

      visual_tbl = (char *) alloca (get_visual_tbl_length ());
      init_block_visualization ();
    }

  /* Remove remaining note insns from the block, save them in
     note_list.  These notes are restored at the end of
     schedule_block ().  */
  note_list = 0;
  rm_other_notes (head, tail);

  target_bb = bb;

  /* Prepare current target block info.  */
  if (current_nr_blocks > 1)
    {
      candidate_table = (candidate *) xmalloc (current_nr_blocks 
					       * sizeof (candidate));

      bblst_last = 0;
      /* ??? It is not clear why bblst_size is computed this way.  The original
	 number was clearly too small as it resulted in compiler failures.
	 Multiplying by the original number by 2 (to account for update_bbs
	 members) seems to be a reasonable solution.  */
      /* ??? Or perhaps there is a bug somewhere else in this file?  */
      bblst_size = (current_nr_blocks - bb) * rgn_nr_edges * 2;
      bblst_table = (int *) xmalloc (bblst_size * sizeof (int));

      bitlst_table_last = 0;
      bitlst_table_size = rgn_nr_edges;
      bitlst_table = (int *) xmalloc (rgn_nr_edges * sizeof (int));

      compute_trg_info (bb);
    }

  clear_units ();

  /* Allocate the ready list.  */
  ready = (rtx *) xmalloc ((rgn_n_insns + 1) * sizeof (rtx));

  /* Print debugging information.  */
  if (sched_verbose >= 5)
    debug_dependencies ();


  /* Initialize ready list with all 'ready' insns in target block.
     Count number of insns in the target block being scheduled.  */
  n_ready = 0;
  for (insn = head; insn != next_tail; insn = NEXT_INSN (insn))
    {
      rtx next;

      if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
	continue;
      next = NEXT_INSN (insn);

      if (INSN_DEP_COUNT (insn) == 0
	  && (SCHED_GROUP_P (next) == 0 || GET_RTX_CLASS (GET_CODE (next)) != 'i'))
	ready[n_ready++] = insn;
      if (!(SCHED_GROUP_P (insn)))
	target_n_insns++;
    }

  /* Add to ready list all 'ready' insns in valid source blocks.
     For speculative insns, check-live, exception-free, and
     issue-delay.  */
  for (bb_src = bb + 1; bb_src < current_nr_blocks; bb_src++)
    if (IS_VALID (bb_src))
      {
	rtx src_head;
	rtx src_next_tail;
	rtx tail, head;

	get_bb_head_tail (bb_src, &head, &tail);
	src_next_tail = NEXT_INSN (tail);
	src_head = head;

	if (head == tail
	    && (GET_RTX_CLASS (GET_CODE (head)) != 'i'))
	  continue;

	for (insn = src_head; insn != src_next_tail; insn = NEXT_INSN (insn))
	  {
	    if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
	      continue;

	    if (!CANT_MOVE (insn)
		&& (!IS_SPECULATIVE_INSN (insn)
		    || (insn_issue_delay (insn) <= 3
			&& check_live (insn, bb_src)
			&& is_exception_free (insn, bb_src, target_bb))))
	      {
		rtx next;

		/* Note that we havn't squirrled away the notes for 
		   blocks other than the current.  So if this is a
		   speculative insn, NEXT might otherwise be a note.  */
		next = next_nonnote_insn (insn);
		if (INSN_DEP_COUNT (insn) == 0
		    && (! next
			|| SCHED_GROUP_P (next) == 0
			|| GET_RTX_CLASS (GET_CODE (next)) != 'i'))
		  ready[n_ready++] = insn;
	      }
	  }
      }

#ifdef MD_SCHED_INIT
  MD_SCHED_INIT (dump, sched_verbose);
#endif

  /* No insns scheduled in this block yet.  */
  last_scheduled_insn = 0;

  /* Q_SIZE is the total number of insns in the queue.  */
  q_ptr = 0;
  q_size = 0;
  last_clock_var = 0;
  bzero ((char *) insn_queue, sizeof (insn_queue));

  /* Start just before the beginning of time.  */
  clock_var = -1;

  /* We start inserting insns after PREV_HEAD.  */
  last = prev_head;

  /* Initialize INSN_QUEUE, LIST and NEW_NEEDS.  */
  new_needs = (NEXT_INSN (prev_head) == BLOCK_HEAD (b)
	       ? NEED_HEAD : NEED_NOTHING);
  if (PREV_INSN (next_tail) == BLOCK_END (b))
    new_needs |= NEED_TAIL;

  /* Loop until all the insns in BB are scheduled.  */
  while (sched_target_n_insns < target_n_insns)
    {
      clock_var++;

      /* Add to the ready list all pending insns that can be issued now.
         If there are no ready insns, increment clock until one
         is ready and add all pending insns at that point to the ready
         list.  */
      n_ready = queue_to_ready (ready, n_ready);

      if (n_ready == 0)
	abort ();

      if (sched_verbose >= 2)
	{
	  fprintf (dump, ";;\t\tReady list after queue_to_ready:  ");
	  debug_ready_list (ready, n_ready);
	}

      /* Sort the ready list based on priority.  */
      SCHED_SORT (ready, n_ready);

      /* Allow the target to reorder the list, typically for 
	 better instruction bundling.  */
#ifdef MD_SCHED_REORDER
      MD_SCHED_REORDER (dump, sched_verbose, ready, n_ready, clock_var,
			can_issue_more);
#else
      can_issue_more = issue_rate;
#endif

      if (sched_verbose)
	{
	  fprintf (dump, "\n;;\tReady list (t =%3d):  ", clock_var);
	  debug_ready_list (ready, n_ready);
	}

      /* Issue insns from ready list.  */
      while (n_ready != 0 && can_issue_more)
	{
	  /* Select and remove the insn from the ready list.  */
	  rtx insn = ready[--n_ready];
	  int cost = actual_hazard (insn_unit (insn), insn, clock_var, 0);

	  if (cost >= 1)
	    {
	      queue_insn (insn, cost);
	      continue;
	    }

	  /* An interblock motion?  */
	  if (INSN_BB (insn) != target_bb)
	    {
	      rtx temp;
	      basic_block b1;

	      if (IS_SPECULATIVE_INSN (insn))
		{
		  if (!check_live (insn, INSN_BB (insn)))
		    continue;
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

	      /* Update source block boundaries.   */
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

	  last_scheduled_insn = insn;
	  last = move_insn (insn, last);
	  sched_n_insns++;

#ifdef MD_SCHED_VARIABLE_ISSUE
	  MD_SCHED_VARIABLE_ISSUE (dump, sched_verbose, insn,
				   can_issue_more);
#else
	  can_issue_more--;
#endif

	  n_ready = schedule_insn (insn, ready, n_ready, clock_var);

	  /* Close this block after scheduling its jump.  */
	  if (GET_CODE (last_scheduled_insn) == JUMP_INSN)
	    break;
	}

      /* Debug info.  */
      if (sched_verbose)
	visualize_scheduled_insns (b, clock_var);
    }

  /* Debug info.  */
  if (sched_verbose)
    {
      fprintf (dump, ";;\tReady list (final):  ");
      debug_ready_list (ready, n_ready);
      print_block_visualization (b, "");
    }

  /* Sanity check -- queue must be empty now.  Meaningless if region has
     multiple bbs.  */
  if (current_nr_blocks > 1)
    if (!flag_schedule_interblock && q_size != 0)
      abort ();

  /* Update head/tail boundaries.  */
  head = NEXT_INSN (prev_head);
  tail = last;

  /* Restore-other-notes: NOTE_LIST is the end of a chain of notes
     previously found among the insns.  Insert them at the beginning
     of the insns.  */
  if (note_list != 0)
    {
      rtx note_head = note_list;

      while (PREV_INSN (note_head))
	{
	  note_head = PREV_INSN (note_head);
	}

      PREV_INSN (note_head) = PREV_INSN (head);
      NEXT_INSN (PREV_INSN (head)) = note_head;
      PREV_INSN (head) = note_list;
      NEXT_INSN (note_list) = head;
      head = note_head;
    }

  /* Update target block boundaries.  */
  if (new_needs & NEED_HEAD)
    BLOCK_HEAD (b) = head;

  if (new_needs & NEED_TAIL)
    BLOCK_END (b) = tail;

  /* Debugging.  */
  if (sched_verbose)
    {
      fprintf (dump, ";;   total time = %d\n;;   new basic block head = %d\n",
	       clock_var, INSN_UID (BLOCK_HEAD (b)));
      fprintf (dump, ";;   new basic block end = %d\n\n",
	       INSN_UID (BLOCK_END (b)));
    }

  /* Clean up.  */
  if (current_nr_blocks > 1)
    {
      free (candidate_table);
      free (bblst_table);
      free (bitlst_table);
    }
  free (ready);

  return (sched_n_insns);
}				/* schedule_block () */


/* Print the bit-set of registers, S, callable from debugger.  */

extern void
debug_reg_vector (s)
     regset s;
{
  int regno;

  EXECUTE_IF_SET_IN_REG_SET (s, 0, regno,
			     {
			       fprintf (dump, " %d", regno);
			     });

  fprintf (dump, "\n");
}

/* Use the backward dependences from LOG_LINKS to build
   forward dependences in INSN_DEPEND.  */

static void
compute_block_forward_dependences (bb)
     int bb;
{
  rtx insn, link;
  rtx tail, head;
  rtx next_tail;
  enum reg_note dep_type;

  get_bb_head_tail (bb, &head, &tail);
  next_tail = NEXT_INSN (tail);
  for (insn = head; insn != next_tail; insn = NEXT_INSN (insn))
    {
      if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
	continue;

      insn = group_leader (insn);

      for (link = LOG_LINKS (insn); link; link = XEXP (link, 1))
	{
	  rtx x = group_leader (XEXP (link, 0));
	  rtx new_link;

	  if (x != XEXP (link, 0))
	    continue;

#ifdef ENABLE_CHECKING
	  /* If add_dependence is working properly there should never
	     be notes, deleted insns or duplicates in the backward
	     links.  Thus we need not check for them here.

	     However, if we have enabled checking we might as well go
	     ahead and verify that add_dependence worked properly.  */
	  if (GET_CODE (x) == NOTE
	      || INSN_DELETED_P (x)
	      || find_insn_list (insn, INSN_DEPEND (x)))
	    abort ();
#endif

	  new_link = alloc_INSN_LIST (insn, INSN_DEPEND (x));

	  dep_type = REG_NOTE_KIND (link);
	  PUT_REG_NOTE_KIND (new_link, dep_type);

	  INSN_DEPEND (x) = new_link;
	  INSN_DEP_COUNT (insn) += 1;
	}
    }
}

/* Initialize variables for region data dependence analysis.
   n_bbs is the number of region blocks.  */

static void
init_deps (deps)
     struct deps *deps;
{
  int maxreg = max_reg_num ();
  deps->reg_last_uses = (rtx *) xcalloc (maxreg, sizeof (rtx));
  deps->reg_last_sets = (rtx *) xcalloc (maxreg, sizeof (rtx));
  deps->reg_last_clobbers = (rtx *) xcalloc (maxreg, sizeof (rtx));

  deps->pending_read_insns = 0;
  deps->pending_read_mems = 0;
  deps->pending_write_insns = 0;
  deps->pending_write_mems = 0;
  deps->pending_lists_length = 0;
  deps->last_pending_memory_flush = 0;
  deps->last_function_call = 0;

  deps->sched_before_next_call
    = gen_rtx_INSN (VOIDmode, 0, NULL_RTX, NULL_RTX,
		    NULL_RTX, 0, NULL_RTX, NULL_RTX);
  LOG_LINKS (deps->sched_before_next_call) = 0;
}

/* Add dependences so that branches are scheduled to run last in their
   block.  */

static void
add_branch_dependences (head, tail)
     rtx head, tail;
{
  rtx insn, last;

  /* For all branches, calls, uses, clobbers, and cc0 setters, force them
     to remain in order at the end of the block by adding dependencies and
     giving the last a high priority.  There may be notes present, and
     prev_head may also be a note.

     Branches must obviously remain at the end.  Calls should remain at the
     end since moving them results in worse register allocation.  Uses remain
     at the end to ensure proper register allocation.  cc0 setters remaim
     at the end because they can't be moved away from their cc0 user.  */
  insn = tail;
  last = 0;
  while (GET_CODE (insn) == CALL_INSN
	 || GET_CODE (insn) == JUMP_INSN
	 || (GET_CODE (insn) == INSN
	     && (GET_CODE (PATTERN (insn)) == USE
		 || GET_CODE (PATTERN (insn)) == CLOBBER
#ifdef HAVE_cc0
		 || sets_cc0_p (PATTERN (insn))
#endif
	     ))
	 || GET_CODE (insn) == NOTE)
    {
      if (GET_CODE (insn) != NOTE)
	{
	  if (last != 0
	      && !find_insn_list (insn, LOG_LINKS (last)))
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

/* After computing the dependencies for block BB, propagate the dependencies
   found in TMP_DEPS to the successors of the block.  MAX_REG is the number
   of registers.  */
static void
propagate_deps (bb, tmp_deps, max_reg)
     int bb;
     struct deps *tmp_deps;
     int max_reg;
{
  int b = BB_TO_BLOCK (bb);
  int e, first_edge;
  int reg;
  rtx link_insn, link_mem;
  rtx u;

  /* These lists should point to the right place, for correct
     freeing later.  */
  bb_deps[bb].pending_read_insns = tmp_deps->pending_read_insns;
  bb_deps[bb].pending_read_mems = tmp_deps->pending_read_mems;
  bb_deps[bb].pending_write_insns = tmp_deps->pending_write_insns;
  bb_deps[bb].pending_write_mems = tmp_deps->pending_write_mems;

  /* bb's structures are inherited by its successors.  */
  first_edge = e = OUT_EDGES (b);
  if (e <= 0)
    return;

  do
    {
      rtx x;
      int b_succ = TO_BLOCK (e);
      int bb_succ = BLOCK_TO_BB (b_succ);
      struct deps *succ_deps = bb_deps + bb_succ;

      /* Only bbs "below" bb, in the same region, are interesting.  */
      if (CONTAINING_RGN (b) != CONTAINING_RGN (b_succ)
	  || bb_succ <= bb)
	{
	  e = NEXT_OUT (e);
	  continue;
	}

      for (reg = 0; reg < max_reg; reg++)
	{
	  /* reg-last-uses lists are inherited by bb_succ.  */
	  for (u = tmp_deps->reg_last_uses[reg]; u; u = XEXP (u, 1))
	    {
	      if (find_insn_list (XEXP (u, 0),
				  succ_deps->reg_last_uses[reg]))
		continue;

	      succ_deps->reg_last_uses[reg]
		= alloc_INSN_LIST (XEXP (u, 0),
				   succ_deps->reg_last_uses[reg]);
	    }

	  /* reg-last-defs lists are inherited by bb_succ.  */
	  for (u = tmp_deps->reg_last_sets[reg]; u; u = XEXP (u, 1))
	    {
	      if (find_insn_list (XEXP (u, 0),
				  succ_deps->reg_last_sets[reg]))
		continue;

	      succ_deps->reg_last_sets[reg]
		= alloc_INSN_LIST (XEXP (u, 0),
				   succ_deps->reg_last_sets[reg]);
	    }

	  for (u = tmp_deps->reg_last_clobbers[reg]; u; u = XEXP (u, 1))
	    {
	      if (find_insn_list (XEXP (u, 0),
				  succ_deps->reg_last_clobbers[reg]))
		continue;

	      succ_deps->reg_last_clobbers[reg]
		= alloc_INSN_LIST (XEXP (u, 0),
				   succ_deps->reg_last_clobbers[reg]);
	    }
	}

      /* Mem read/write lists are inherited by bb_succ.  */
      link_insn = tmp_deps->pending_read_insns;
      link_mem = tmp_deps->pending_read_mems;
      while (link_insn)
	{
	  if (!(find_insn_mem_list (XEXP (link_insn, 0),
				    XEXP (link_mem, 0),
				    succ_deps->pending_read_insns,
				    succ_deps->pending_read_mems)))
	    add_insn_mem_dependence (succ_deps, &succ_deps->pending_read_insns,
				     &succ_deps->pending_read_mems,
				     XEXP (link_insn, 0), XEXP (link_mem, 0));
	  link_insn = XEXP (link_insn, 1);
	  link_mem = XEXP (link_mem, 1);
	}

      link_insn = tmp_deps->pending_write_insns;
      link_mem = tmp_deps->pending_write_mems;
      while (link_insn)
	{
	  if (!(find_insn_mem_list (XEXP (link_insn, 0),
				    XEXP (link_mem, 0),
				    succ_deps->pending_write_insns,
				    succ_deps->pending_write_mems)))
	    add_insn_mem_dependence (succ_deps,
				     &succ_deps->pending_write_insns,
				     &succ_deps->pending_write_mems,
				     XEXP (link_insn, 0), XEXP (link_mem, 0));

	  link_insn = XEXP (link_insn, 1);
	  link_mem = XEXP (link_mem, 1);
	}

      /* last_function_call is inherited by bb_succ.  */
      for (u = tmp_deps->last_function_call; u; u = XEXP (u, 1))
	{
	  if (find_insn_list (XEXP (u, 0),
			      succ_deps->last_function_call))
	    continue;

	  succ_deps->last_function_call
	    = alloc_INSN_LIST (XEXP (u, 0),
			       succ_deps->last_function_call);
	}

      /* last_pending_memory_flush is inherited by bb_succ.  */
      for (u = tmp_deps->last_pending_memory_flush; u; u = XEXP (u, 1))
	{
	  if (find_insn_list (XEXP (u, 0), 
			      succ_deps->last_pending_memory_flush))
	    continue;

	  succ_deps->last_pending_memory_flush
	    = alloc_INSN_LIST (XEXP (u, 0),
			       succ_deps->last_pending_memory_flush);
	}

      /* sched_before_next_call is inherited by bb_succ.  */
      x = LOG_LINKS (tmp_deps->sched_before_next_call);
      for (; x; x = XEXP (x, 1))
	add_dependence (succ_deps->sched_before_next_call,
			XEXP (x, 0), REG_DEP_ANTI);

      e = NEXT_OUT (e);
    }
  while (e != first_edge);
}

/* Compute backward dependences inside bb.  In a multiple blocks region:
   (1) a bb is analyzed after its predecessors, and (2) the lists in
   effect at the end of bb (after analyzing for bb) are inherited by
   bb's successrs.

   Specifically for reg-reg data dependences, the block insns are
   scanned by sched_analyze () top-to-bottom.  Two lists are
   maintained by sched_analyze (): reg_last_sets[] for register DEFs,
   and reg_last_uses[] for register USEs.

   When analysis is completed for bb, we update for its successors:
   ;  - DEFS[succ] = Union (DEFS [succ], DEFS [bb])
   ;  - USES[succ] = Union (USES [succ], DEFS [bb])

   The mechanism for computing mem-mem data dependence is very
   similar, and the result is interblock dependences in the region.  */

static void
compute_block_backward_dependences (bb)
     int bb;
{
  int i;
  rtx head, tail;
  int max_reg = max_reg_num ();
  struct deps tmp_deps;

  tmp_deps = bb_deps[bb];

  /* Do the analysis for this block.  */
  get_bb_head_tail (bb, &head, &tail);
  sched_analyze (&tmp_deps, head, tail);
  add_branch_dependences (head, tail);

  if (current_nr_blocks > 1)
    propagate_deps (bb, &tmp_deps, max_reg);

  /* Free up the INSN_LISTs.

     Note this loop is executed max_reg * nr_regions times.  It's first 
     implementation accounted for over 90% of the calls to free_INSN_LIST_list.
     The list was empty for the vast majority of those calls.  On the PA, not 
     calling free_INSN_LIST_list in those cases improves -O2 compile times by
     3-5% on average.  */
  for (i = 0; i < max_reg; ++i)
    {
      if (tmp_deps.reg_last_clobbers[i])
	free_INSN_LIST_list (&tmp_deps.reg_last_clobbers[i]);
      if (tmp_deps.reg_last_sets[i])
	free_INSN_LIST_list (&tmp_deps.reg_last_sets[i]);
      if (tmp_deps.reg_last_uses[i])
	free_INSN_LIST_list (&tmp_deps.reg_last_uses[i]);
    }

  /* Assert that we won't need bb_reg_last_* for this block anymore.  */
  free (bb_deps[bb].reg_last_uses);
  free (bb_deps[bb].reg_last_sets);
  free (bb_deps[bb].reg_last_clobbers);
  bb_deps[bb].reg_last_uses = 0;
  bb_deps[bb].reg_last_sets = 0;
  bb_deps[bb].reg_last_clobbers = 0;
}

/* Print dependences for debugging, callable from debugger.  */

void
debug_dependencies ()
{
  int bb;

  fprintf (dump, ";;   --------------- forward dependences: ------------ \n");
  for (bb = 0; bb < current_nr_blocks; bb++)
    {
      if (1)
	{
	  rtx head, tail;
	  rtx next_tail;
	  rtx insn;

	  get_bb_head_tail (bb, &head, &tail);
	  next_tail = NEXT_INSN (tail);
	  fprintf (dump, "\n;;   --- Region Dependences --- b %d bb %d \n",
		   BB_TO_BLOCK (bb), bb);

	  fprintf (dump, ";;   %7s%6s%6s%6s%6s%6s%11s%6s\n",
	  "insn", "code", "bb", "dep", "prio", "cost", "blockage", "units");
	  fprintf (dump, ";;   %7s%6s%6s%6s%6s%6s%11s%6s\n",
	  "----", "----", "--", "---", "----", "----", "--------", "-----");
	  for (insn = head; insn != next_tail; insn = NEXT_INSN (insn))
	    {
	      rtx link;
	      int unit, range;

	      if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
		{
		  int n;
		  fprintf (dump, ";;   %6d ", INSN_UID (insn));
		  if (GET_CODE (insn) == NOTE)
		    {
		      n = NOTE_LINE_NUMBER (insn);
		      if (n < 0)
			fprintf (dump, "%s\n", GET_NOTE_INSN_NAME (n));
		      else
			fprintf (dump, "line %d, file %s\n", n,
				 NOTE_SOURCE_FILE (insn));
		    }
		  else
		    fprintf (dump, " {%s}\n", GET_RTX_NAME (GET_CODE (insn)));
		  continue;
		}

	      unit = insn_unit (insn);
	      range = (unit < 0
		 || function_units[unit].blockage_range_function == 0) ? 0 :
		function_units[unit].blockage_range_function (insn);
	      fprintf (dump,
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
	      fprintf (dump, "\t: ");
	      for (link = INSN_DEPEND (insn); link; link = XEXP (link, 1))
		fprintf (dump, "%d ", INSN_UID (XEXP (link, 0)));
	      fprintf (dump, "\n");
	    }
	}
    }
  fprintf (dump, "\n");
}

/* Set_priorities: compute priority of each insn in the block.  */

static int
set_priorities (bb)
     int bb;
{
  rtx insn;
  int n_insn;

  rtx tail;
  rtx prev_head;
  rtx head;

  get_bb_head_tail (bb, &head, &tail);
  prev_head = PREV_INSN (head);

  if (head == tail
      && (GET_RTX_CLASS (GET_CODE (head)) != 'i'))
    return 0;

  n_insn = 0;
  for (insn = tail; insn != prev_head; insn = PREV_INSN (insn))
    {

      if (GET_CODE (insn) == NOTE)
	continue;

      if (!(SCHED_GROUP_P (insn)))
	n_insn++;
      (void) priority (insn);
    }

  return n_insn;
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

  reg_pending_sets = ALLOCA_REG_SET ();
  reg_pending_clobbers = ALLOCA_REG_SET ();
  reg_pending_sets_all = 0;

  /* Initializations for region data dependence analyisis.  */
  bb_deps = (struct deps *) xmalloc (sizeof (struct deps) * current_nr_blocks);
  for (bb = 0; bb < current_nr_blocks; bb++)
    init_deps (bb_deps + bb);

  /* Compute LOG_LINKS.  */
  for (bb = 0; bb < current_nr_blocks; bb++)
    compute_block_backward_dependences (bb);

  /* Compute INSN_DEPEND.  */
  for (bb = current_nr_blocks - 1; bb >= 0; bb--)
    compute_block_forward_dependences (bb);

  /* Delete line notes and set priorities.  */
  for (bb = 0; bb < current_nr_blocks; bb++)
    {
      if (write_symbols != NO_DEBUG)
	{
	  save_line_notes (bb);
	  rm_line_notes (bb);
	}

      rgn_n_insns += set_priorities (bb);
    }

  /* Compute interblock info: probabilities, split-edges, dominators, etc.  */
  if (current_nr_blocks > 1)
    {
      int i;

      prob = (float *) xmalloc ((current_nr_blocks) * sizeof (float));

      bbset_size = current_nr_blocks / HOST_BITS_PER_WIDE_INT + 1;
      dom = (bbset *) xmalloc (current_nr_blocks * sizeof (bbset));
      for (i = 0; i < current_nr_blocks; i++)
	dom[i] = (bbset) xcalloc (bbset_size, sizeof (HOST_WIDE_INT));

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
      edgeset_size = rgn_nr_edges / HOST_BITS_PER_WIDE_INT + 1;
      pot_split = (edgeset *) xmalloc (current_nr_blocks * sizeof (edgeset));
      ancestor_edges 
	= (edgeset *) xmalloc (current_nr_blocks * sizeof (edgeset));
      for (i = 0; i < current_nr_blocks; i++)
	{
	  pot_split[i] =
	    (edgeset) xcalloc (edgeset_size, sizeof (HOST_WIDE_INT));
	  ancestor_edges[i] =
	    (edgeset) xcalloc (edgeset_size, sizeof (HOST_WIDE_INT));
	}

      /* Compute probabilities, dominators, split_edges.  */
      for (bb = 0; bb < current_nr_blocks; bb++)
	compute_dom_prob_ps (bb);
    }

  /* Now we can schedule all blocks.  */
  for (bb = 0; bb < current_nr_blocks; bb++)
    sched_rgn_n_insns += schedule_block (bb, rgn_n_insns);

  /* Sanity check: verify that all region insns were scheduled.  */
  if (sched_rgn_n_insns != rgn_n_insns)
    abort ();

  /* Restore line notes.  */
  if (write_symbols != NO_DEBUG)
    {
      for (bb = 0; bb < current_nr_blocks; bb++)
	restore_line_notes (bb);
    }

  /* Done with this region.  */
  free_pending_lists ();

  FREE_REG_SET (reg_pending_sets);
  FREE_REG_SET (reg_pending_clobbers);

  free (bb_deps);

  if (current_nr_blocks > 1)
    {
      int i;

      free (prob);
      for (i = 0; i < current_nr_blocks; ++i)
	{
	  free (dom[i]);
	  free (pot_split[i]);
	  free (ancestor_edges[i]);
	}
      free (dom);
      free (edge_to_bit);
      free (rgn_edges);
      free (pot_split);
      free (ancestor_edges);
    }
}

/* The one entry point in this file.  DUMP_FILE is the dump file for
   this pass.  */

void
schedule_insns (dump_file)
     FILE *dump_file;
{
  int *deaths_in_region;
  sbitmap blocks, large_region_blocks;
  int max_uid;
  int b;
  rtx insn;
  int rgn;
  int luid;
  int any_large_regions;

  /* Disable speculative loads in their presence if cc0 defined.  */
#ifdef HAVE_cc0
  flag_schedule_speculative_load = 0;
#endif

  /* Taking care of this degenerate case makes the rest of
     this code simpler.  */
  if (n_basic_blocks == 0)
    return;

  /* Set dump and sched_verbose for the desired debugging output.  If no
     dump-file was specified, but -fsched-verbose-N (any N), print to stderr.
     For -fsched-verbose-N, N>=10, print everything to stderr.  */
  sched_verbose = sched_verbose_param;
  if (sched_verbose_param == 0 && dump_file)
    sched_verbose = 1;
  dump = ((sched_verbose_param >= 10 || !dump_file) ? stderr : dump_file);

  nr_inter = 0;
  nr_spec = 0;

  /* Initialize issue_rate.  */
  issue_rate = ISSUE_RATE;

  split_all_insns (1);

  /* We use LUID 0 for the fake insn (UID 0) which holds dependencies for
     pseudos which do not cross calls.  */
  max_uid = get_max_uid () + 1;

  h_i_d = (struct haifa_insn_data *) xcalloc (max_uid, sizeof (*h_i_d));

  h_i_d[0].luid = 0;
  luid = 1;
  for (b = 0; b < n_basic_blocks; b++)
    for (insn = BLOCK_HEAD (b);; insn = NEXT_INSN (insn))
      {
	INSN_LUID (insn) = luid;

	/* Increment the next luid, unless this is a note.  We don't
	   really need separate IDs for notes and we don't want to
	   schedule differently depending on whether or not there are
	   line-number notes, i.e., depending on whether or not we're
	   generating debugging information.  */
	if (GET_CODE (insn) != NOTE)
	  ++luid;

	if (insn == BLOCK_END (b))
	  break;
      }
  
  /* ?!? We could save some memory by computing a per-region luid mapping
     which could reduce both the number of vectors in the cache and the size
     of each vector.  Instead we just avoid the cache entirely unless the
     average number of instructions in a basic block is very high.  See
     the comment before the declaration of true_dependency_cache for
     what we consider "very high".  */
  if (luid / n_basic_blocks > 100 * 5)
    {
      true_dependency_cache = sbitmap_vector_alloc (luid, luid);
      sbitmap_vector_zero (true_dependency_cache, luid);
    }

  nr_regions = 0;
  rgn_table = (region *) xmalloc ((n_basic_blocks) * sizeof (region));
  rgn_bb_table = (int *) xmalloc ((n_basic_blocks) * sizeof (int));
  block_to_bb = (int *) xmalloc ((n_basic_blocks) * sizeof (int));
  containing_rgn = (int *) xmalloc ((n_basic_blocks) * sizeof (int));

  blocks = sbitmap_alloc (n_basic_blocks);
  large_region_blocks = sbitmap_alloc (n_basic_blocks);

  compute_bb_for_insn (max_uid);

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
	  sbitmap *dom;
	  struct edge_list *edge_list;

	  dom = sbitmap_vector_alloc (n_basic_blocks, n_basic_blocks);

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

	  /* Compute the dominators and post dominators.  We don't
	     currently use post dominators, but we should for
	     speculative motion analysis.  */
	  compute_flow_dominators (dom, NULL);

	  /* build_control_flow will return nonzero if it detects unreachable
	     blocks or any other irregularity with the cfg which prevents
	     cross block scheduling.  */
	  if (build_control_flow (edge_list) != 0)
	    find_single_block_region ();
	  else
	    find_rgns (edge_list, dom);

	  if (sched_verbose >= 3)
	    debug_regions ();

	  /* For now.  This will move as more and more of haifa is converted
	     to using the cfg code in flow.c.  */
	  free (dom);
	}
    }

  deaths_in_region = (int *) xmalloc (sizeof(int) * nr_regions);

  init_alias_analysis ();

  if (write_symbols != NO_DEBUG)
    {
      rtx line;

      line_note_head = (rtx *) xcalloc (n_basic_blocks, sizeof (rtx));

      /* Save-line-note-head:
         Determine the line-number at the start of each basic block.
         This must be computed and saved now, because after a basic block's
         predecessor has been scheduled, it is impossible to accurately
         determine the correct line number for the first insn of the block.  */

      for (b = 0; b < n_basic_blocks; b++)
	for (line = BLOCK_HEAD (b); line; line = PREV_INSN (line))
	  if (GET_CODE (line) == NOTE && NOTE_LINE_NUMBER (line) > 0)
	    {
	      line_note_head[b] = line;
	      break;
	    }
    }

  /* Find units used in this fuction, for visualization.  */
  if (sched_verbose)
    init_target_units ();

  /* ??? Add a NOTE after the last insn of the last basic block.  It is not
     known why this is done.  */

  insn = BLOCK_END (n_basic_blocks - 1);
  if (NEXT_INSN (insn) == 0
      || (GET_CODE (insn) != NOTE
	  && GET_CODE (insn) != CODE_LABEL
	  /* Don't emit a NOTE if it would end up between an unconditional
	     jump and a BARRIER.  */
	  && !(GET_CODE (insn) == JUMP_INSN
	       && GET_CODE (NEXT_INSN (insn)) == BARRIER)))
    emit_note_after (NOTE_INSN_DELETED, BLOCK_END (n_basic_blocks - 1));

  /* Compute INSN_REG_WEIGHT for all blocks.  We must do this before
     removing death notes.  */
  for (b = n_basic_blocks - 1; b >= 0; b--)
    find_insn_reg_weight (b);

  /* Remove all death notes from the subroutine.  */
  for (rgn = 0; rgn < nr_regions; rgn++)
    {
      sbitmap_zero (blocks);
      for (b = RGN_NR_BLOCKS (rgn) - 1; b >= 0; --b)
	SET_BIT (blocks, rgn_bb_table [RGN_BLOCKS (rgn) + b]);

      deaths_in_region[rgn] = count_or_remove_death_notes (blocks, 1);
    }

  /* Schedule every region in the subroutine.  */
  for (rgn = 0; rgn < nr_regions; rgn++)
    schedule_region (rgn);

  /* Update life analysis for the subroutine.  Do single block regions
     first so that we can verify that live_at_start didn't change.  Then
     do all other blocks.   */
  /* ??? There is an outside possibility that update_life_info, or more
     to the point propagate_block, could get called with non-zero flags
     more than once for one basic block.  This would be kinda bad if it
     were to happen, since REG_INFO would be accumulated twice for the
     block, and we'd have twice the REG_DEAD notes.

     I'm fairly certain that this _shouldn't_ happen, since I don't think
     that live_at_start should change at region heads.  Not sure what the
     best way to test for this kind of thing... */

  allocate_reg_life_data ();
  compute_bb_for_insn (max_uid);

  any_large_regions = 0;
  sbitmap_ones (large_region_blocks);

  for (rgn = 0; rgn < nr_regions; rgn++)
    if (RGN_NR_BLOCKS (rgn) > 1)
      any_large_regions = 1;
    else
      {
	sbitmap_zero (blocks);
	SET_BIT (blocks, rgn_bb_table[RGN_BLOCKS (rgn)]);
	RESET_BIT (large_region_blocks, rgn_bb_table[RGN_BLOCKS (rgn)]);

	update_life_info (blocks, UPDATE_LIFE_LOCAL,
			  PROP_DEATH_NOTES | PROP_REG_INFO);

	/* In the single block case, the count of registers that died should
	   not have changed during the schedule.  */
	if (count_or_remove_death_notes (blocks, 0) != deaths_in_region[rgn])
          abort (); 
      }

  if (any_large_regions)
    {
      update_life_info (large_region_blocks, UPDATE_LIFE_GLOBAL,
		        PROP_DEATH_NOTES | PROP_REG_INFO);
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
	  fprintf (dump, "\n;; Procedure interblock/speculative motions == %d/%d \n",
		   nr_inter, nr_spec);
	}
      else
	{
	  if (nr_inter > 0)
	    abort ();
	}
      fprintf (dump, "\n\n");
    }

  /* Clean up.  */
  end_alias_analysis ();

  if (true_dependency_cache)
    {
      free (true_dependency_cache);
      true_dependency_cache = NULL;
    }
  free (rgn_table);
  free (rgn_bb_table);
  free (block_to_bb);
  free (containing_rgn);

  free (h_i_d);

  if (write_symbols != NO_DEBUG)
    free (line_note_head);

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

  free (deaths_in_region);
}

#endif /* INSN_SCHEDULING */
