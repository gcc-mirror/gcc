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
#include "basic-block.h"
#include "regs.h"
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
   But currently it is computed by examinning the insn list.  Since
   this is only needed for visualization, it seems an acceptable
   solution.  (For understanding the mapping of bits to units, see
   definition of function_units[] in "insn-attrtab.c") */

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

/* nr_inter/spec counts interblock/speculative motion for the function */
static int nr_inter, nr_spec;


/* debugging file. all printouts are sent to dump, which is always set,
   either to stderr, or to the dump listing file (-dRS).  */
static FILE *dump = 0;

/* fix_sched_param() is called from toplev.c upon detection
   of the -fsched-***-N options.  */

void
fix_sched_param (param, val)
     char *param, *val;
{
  if (!strcmp (param, "verbose"))
    sched_verbose_param = atoi (val);
  else
    warning ("fix_sched_param: unknown param: %s", param);
}


/* Arrays set up by scheduling for the same respective purposes as
   similar-named arrays set up by flow analysis.  We work with these
   arrays during the scheduling pass so we can compare values against
   unscheduled code.

   Values of these arrays are copied at the end of this pass into the
   arrays set up by flow analysis.  */
static int *sched_reg_n_calls_crossed;
static int *sched_reg_live_length;
static int *sched_reg_basic_block;

/* We need to know the current block number during the post scheduling
   update of live register information so that we can also update
   REG_BASIC_BLOCK if a register changes blocks.  */
static int current_block_num;

/* Element N is the next insn that sets (hard or pseudo) register
   N within the current basic block; or zero, if there is no
   such insn.  Needed for new registers which may be introduced
   by splitting insns.  */
static rtx *reg_last_uses;
static rtx *reg_last_sets;
static rtx *reg_last_clobbers;
static regset reg_pending_sets;
static regset reg_pending_clobbers;
static int reg_pending_sets_all;

/* Vector indexed by INSN_UID giving the original ordering of the insns.  */
static int *insn_luid;
#define INSN_LUID(INSN) (insn_luid[INSN_UID (INSN)])

/* Vector indexed by INSN_UID giving each instruction a priority.  */
static int *insn_priority;
#define INSN_PRIORITY(INSN) (insn_priority[INSN_UID (INSN)])

static short *insn_costs;
#define INSN_COST(INSN)	insn_costs[INSN_UID (INSN)]

/* Vector indexed by INSN_UID giving an encoding of the function units
   used.  */
static short *insn_units;
#define INSN_UNIT(INSN)	insn_units[INSN_UID (INSN)]

/* Vector indexed by INSN_UID giving each instruction a register-weight.
   This weight is an estimation of the insn contribution to registers pressure.  */
static int *insn_reg_weight;
#define INSN_REG_WEIGHT(INSN) (insn_reg_weight[INSN_UID (INSN)])

/* Vector indexed by INSN_UID giving list of insns which
   depend upon INSN.  Unlike LOG_LINKS, it represents forward dependences.  */
static rtx *insn_depend;
#define INSN_DEPEND(INSN) insn_depend[INSN_UID (INSN)]

/* Vector indexed by INSN_UID. Initialized to the number of incoming
   edges in forward dependence graph (= number of LOG_LINKS).  As
   scheduling procedes, dependence counts are decreased.  An
   instruction moves to the ready list when its counter is zero.  */
static int *insn_dep_count;
#define INSN_DEP_COUNT(INSN) (insn_dep_count[INSN_UID (INSN)])

/* Vector indexed by INSN_UID giving an encoding of the blockage range
   function.  The unit and the range are encoded.  */
static unsigned int *insn_blockage;
#define INSN_BLOCKAGE(INSN) insn_blockage[INSN_UID (INSN)]
#define UNIT_BITS 5
#define BLOCKAGE_MASK ((1 << BLOCKAGE_BITS) - 1)
#define ENCODE_BLOCKAGE(U, R)				\
(((U) << BLOCKAGE_BITS					\
  | MIN_BLOCKAGE_COST (R)) << BLOCKAGE_BITS		\
 | MAX_BLOCKAGE_COST (R))
#define UNIT_BLOCKED(B) ((B) >> (2 * BLOCKAGE_BITS))
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

/* Vector indexed by INSN_UID giving number of insns referring to this insn.  */
static int *insn_ref_count;
#define INSN_REF_COUNT(INSN) (insn_ref_count[INSN_UID (INSN)])

/* Vector indexed by INSN_UID giving line-number note in effect for each
   insn.  For line-number notes, this indicates whether the note may be
   reused.  */
static rtx *line_note;
#define LINE_NOTE(INSN) (line_note[INSN_UID (INSN)])

/* Vector indexed by basic block number giving the starting line-number
   for each basic block.  */
static rtx *line_note_head;

/* List of important notes we must keep around.  This is a pointer to the
   last element in the list.  */
static rtx note_list;

/* Regsets telling whether a given register is live or dead before the last
   scheduled insn.  Must scan the instructions once before scheduling to
   determine what registers are live or dead at the end of the block.  */
static regset bb_live_regs;

/* Regset telling whether a given register is live after the insn currently
   being scheduled.  Before processing an insn, this is equal to bb_live_regs
   above.  This is used so that we can find registers that are newly born/dead
   after processing an insn.  */
static regset old_live_regs;

/* The chain of REG_DEAD notes.  REG_DEAD notes are removed from all insns
   during the initial scan and reused later.  If there are not exactly as
   many REG_DEAD notes in the post scheduled code as there were in the
   prescheduled code then we trigger an abort because this indicates a bug.  */
static rtx dead_notes;

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

/* Vector indexed by INSN_UID giving the minimum clock tick at which
   the insn becomes ready.  This is used to note timing constraints for
   insns in the pending list.  */
static int *insn_tick;
#define INSN_TICK(INSN) (insn_tick[INSN_UID (INSN)])

/* Data structure for keeping track of register information
   during that register's life.  */

struct sometimes
  {
    int regno;
    int live_length;
    int calls_crossed;
  };

/* Forward declarations.  */
static void add_dependence PROTO ((rtx, rtx, enum reg_note));
static void remove_dependence PROTO ((rtx, rtx));
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
static void add_insn_mem_dependence PROTO ((rtx *, rtx *, rtx, rtx));
static void flush_pending_lists PROTO ((rtx, int));
static void sched_analyze_1 PROTO ((rtx, rtx));
static void sched_analyze_2 PROTO ((rtx, rtx));
static void sched_analyze_insn PROTO ((rtx, rtx, rtx));
static void sched_analyze PROTO ((rtx, rtx));
static void sched_note_set PROTO ((rtx, int));
static int rank_for_schedule PROTO ((const GENERIC_PTR, const GENERIC_PTR));
static void swap_sort PROTO ((rtx *, int));
static void queue_insn PROTO ((rtx, int));
static int schedule_insn PROTO ((rtx, rtx *, int, int));
static void create_reg_dead_note PROTO ((rtx, rtx));
static void attach_deaths PROTO ((rtx, rtx, int));
static void attach_deaths_insn PROTO ((rtx));
static int new_sometimes_live PROTO ((struct sometimes *, int, int));
static void finish_sometimes_live PROTO ((struct sometimes *, int));
static int schedule_block PROTO ((int, int));
static void split_hard_reg_notes PROTO ((rtx, rtx, rtx));
static void new_insn_dead_notes PROTO ((rtx, rtx, rtx, rtx));
static void update_n_sets PROTO ((rtx, int));
static char *safe_concat PROTO ((char *, char *, char *));
static int insn_issue_delay PROTO ((rtx));
static int birthing_insn_p PROTO ((rtx));
static void adjust_priority PROTO ((rtx));

/* Mapping of insns to their original block prior to scheduling.  */
static int *insn_orig_block;
#define INSN_BLOCK(insn) (insn_orig_block[INSN_UID (insn)])

/* Some insns (e.g. call) are not allowed to move across blocks.  */
static char *cant_move;
#define CANT_MOVE(insn) (cant_move[INSN_UID (insn)])

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

/* Number of edges in the control flow graph.  (in fact larger than
   that by 1, since edge 0 is unused.) */
static int nr_edges;

/* Circular list of incoming/outgoing edges of a block */
static int *in_edges;
static int *out_edges;

#define IN_EDGES(block) (in_edges[block])
#define OUT_EDGES(block) (out_edges[block])

/* List of labels which cannot be deleted, needed for control
   flow graph construction.  */
extern rtx forced_labels;


static int is_cfg_nonregular PROTO ((void));
static int build_control_flow PROTO ((int_list_ptr *, int_list_ptr *,
				      int *, int *));
static void new_edge PROTO ((int, int));


/* A region is the main entity for interblock scheduling: insns
   are allowed to move between blocks in the same region, along
   control flow graph edges, in the 'up' direction.  */
typedef struct
  {
    int rgn_nr_blocks;		/* number of blocks in region */
    int rgn_blocks;		/* blocks in the region (actually index in rgn_bb_table) */
  }
region;

/* Number of regions in the procedure */
static int nr_regions;

/* Table of region descriptions */
static region *rgn_table;

/* Array of lists of regions' blocks */
static int *rgn_bb_table;

/* Topological order of blocks in the region (if b2 is reachable from
   b1, block_to_bb[b2] > block_to_bb[b1]).
   Note: A basic block is always referred to by either block or b,
   while its topological order name (in the region) is refered to by
   bb.
 */
static int *block_to_bb;

/* The number of the region containing a block.  */
static int *containing_rgn;

#define RGN_NR_BLOCKS(rgn) (rgn_table[rgn].rgn_nr_blocks)
#define RGN_BLOCKS(rgn) (rgn_table[rgn].rgn_blocks)
#define BLOCK_TO_BB(block) (block_to_bb[block])
#define CONTAINING_RGN(block) (containing_rgn[block])

void debug_regions PROTO ((void));
static void find_single_block_region PROTO ((void));
static void find_rgns PROTO ((int_list_ptr *, int_list_ptr *,
			      int *, int *, sbitmap *));
static int too_large PROTO ((int, int *, int *));

extern void debug_live PROTO ((int, int));

/* Blocks of the current region being scheduled.  */
static int current_nr_blocks;
static int current_blocks;

/* The mapping from bb to block */
#define BB_TO_BLOCK(bb) (rgn_bb_table[current_blocks + (bb)])


/* Bit vectors and bitset operations are needed for computations on
   the control flow graph.  */

typedef unsigned HOST_WIDE_INT *bitset;
typedef struct
  {
    int *first_member;		/* pointer to the list start in bitlst_table.  */
    int nr_members;		/* the number of members of the bit list.     */
  }
bitlst;

static int bitlst_table_last;
static int bitlst_table_size;
static int *bitlst_table;

static char bitset_member PROTO ((bitset, int, int));
static void extract_bitlst PROTO ((bitset, int, bitlst *));

/* target info declarations.

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

   Lists of split and update blocks for  each candidate of the current
   target  are  in  array bblst_table */
static int *bblst_table, bblst_size, bblst_last;

#define IS_VALID(src) ( candidate_table[src].is_valid )
#define IS_SPECULATIVE(src) ( candidate_table[src].is_speculative )
#define SRC_PROB(src) ( candidate_table[src].src_prob )

/* The bb being currently scheduled.  */
static int target_bb;

/* List of edges.  */
typedef bitlst edgelst;

/* target info functions */
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

/* bb 0 is the only region entry */
#define IS_RGN_ENTRY(bb) (!bb)

/* Is bb_src dominated by bb_trg.  */
#define IS_DOMINATED(bb_src, bb_trg)                                 \
( bitset_member (dom[bb_src], bb_trg, bbset_size) )

/* Probability: Prob[i] is a float in [0, 1] which is the probability
   of bb i relative to the region entry.  */
static float *prob;

/*  The probability of bb_src, relative to bb_trg.  Note, that while the
   'prob[bb]' is a float in [0, 1], this macro returns an integer
   in [0, 100].  */
#define GET_SRC_PROB(bb_src, bb_trg) ((int) (100.0 * (prob[bb_src] / \
						      prob[bb_trg])))

/* Bit-set of edges, where bit i stands for edge i.  */
typedef bitset edgeset;

/* Number of edges in the region.  */
static int rgn_nr_edges;

/* Array of size rgn_nr_edges.    */
static int *rgn_edges;

/* Number of words in an edgeset.    */
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
#define INSN_PROBABILITY(INSN) (SRC_PROB (BLOCK_TO_BB (INSN_BLOCK (INSN))))
#define IS_SPECULATIVE_INSN(INSN) (IS_SPECULATIVE (BLOCK_TO_BB (INSN_BLOCK (INSN))))
#define INSN_BB(INSN) (BLOCK_TO_BB (INSN_BLOCK (INSN)))

/* parameters affecting the decision of rank_for_schedule() */
#define MIN_DIFF_PRIORITY 2
#define MIN_PROBABILITY 40
#define MIN_PROB_DIFF 10

/* speculative scheduling functions */
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
static void init_rgn_data_dependences PROTO ((int));
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

static void find_pre_sched_live PROTO ((int));
static void find_post_sched_live PROTO ((int));
static void update_reg_usage PROTO ((void));
static int queue_to_ready PROTO ((rtx [], int));

static void debug_ready_list PROTO ((rtx[], int));
static void init_target_units PROTO ((void));
static void insn_print_units PROTO ((rtx));
static int get_visual_tbl_length PROTO ((void));
static void init_block_visualization PROTO ((void));
static void print_block_visualization PROTO ((int, char *));
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
static void init_rtx_vector PROTO ((rtx **, rtx *, int, int));
static void schedule_region PROTO ((int));

#endif /* INSN_SCHEDULING */

#define SIZE_FOR_MODE(X) (GET_MODE_SIZE (GET_MODE (X)))

/* Helper functions for instruction scheduling.  */

/* An INSN_LIST containing all INSN_LISTs allocated but currently unused.  */
static rtx unused_insn_list;

/* An EXPR_LIST containing all EXPR_LISTs allocated but currently unused.  */
static rtx unused_expr_list;

static void free_list PROTO ((rtx *, rtx *));
static rtx alloc_INSN_LIST PROTO ((rtx, rtx));
static rtx alloc_EXPR_LIST PROTO ((int, rtx, rtx));

static void
free_list (listp, unused_listp)
     rtx *listp, *unused_listp;
{
  register rtx link, prev_link;

  if (*listp == 0)
    return;

  prev_link = *listp;
  link = XEXP (prev_link, 1);

  while (link)
    {
      prev_link = link;
      link = XEXP (link, 1);
    }

  XEXP (prev_link, 1) = *unused_listp;
  *unused_listp = *listp;
  *listp = 0;
}

static rtx
alloc_INSN_LIST (val, next)
     rtx val, next;
{
  rtx r;

  if (unused_insn_list)
    {
      r = unused_insn_list;
      unused_insn_list = XEXP (r, 1);
      XEXP (r, 0) = val;
      XEXP (r, 1) = next;
      PUT_REG_NOTE_KIND (r, VOIDmode);
    }
  else
    r = gen_rtx_INSN_LIST (VOIDmode, val, next);

  return r;
}

static rtx
alloc_EXPR_LIST (kind, val, next)
     int kind;
     rtx val, next;
{
  rtx r;

  if (unused_expr_list)
    {
      r = unused_expr_list;
      unused_expr_list = XEXP (r, 1);
      XEXP (r, 0) = val;
      XEXP (r, 1) = next;
      PUT_REG_NOTE_KIND (r, kind);
    }
  else
    r = gen_rtx_EXPR_LIST (kind, val, next);

  return r;
}

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

#endif

  /* Check that we don't already have this dependence.  */
  for (link = LOG_LINKS (insn); link; link = XEXP (link, 1))
    if (XEXP (link, 0) == elem)
      {
	/* If this is a more restrictive type of dependence than the existing
	   one, then change the existing dependence to this type.  */
	if ((int) dep_type < (int) REG_NOTE_KIND (link))
	  PUT_REG_NOTE_KIND (link, dep_type);
	return;
      }
  /* Might want to check one level of transitivity to save conses.  */

  link = alloc_INSN_LIST (elem, LOG_LINKS (insn));
  LOG_LINKS (insn) = link;

  /* Insn dependency, not data dependency.  */
  PUT_REG_NOTE_KIND (link, dep_type);
}

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

	  XEXP (link, 1) = unused_insn_list;
	  unused_insn_list = link;

	  found = 1;
	}
      else
	prev = link;
    }

  if (!found)
    abort ();
  return;
}

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

/* The *_insns and *_mems are paired lists.  Each pending memory operation
   will have a pointer to the MEM rtx on one list and a pointer to the
   containing insn on the other list in the same place in the list.  */

/* We can't use add_dependence like the old code did, because a single insn
   may have multiple memory accesses, and hence needs to be on the list
   once for each memory access.  Add_dependence won't let you add an insn
   to a list more than once.  */

/* An INSN_LIST containing all insns with pending read operations.  */
static rtx pending_read_insns;

/* An EXPR_LIST containing all MEM rtx's which are pending reads.  */
static rtx pending_read_mems;

/* An INSN_LIST containing all insns with pending write operations.  */
static rtx pending_write_insns;

/* An EXPR_LIST containing all MEM rtx's which are pending writes.  */
static rtx pending_write_mems;

/* Indicates the combined length of the two pending lists.  We must prevent
   these lists from ever growing too large since the number of dependencies
   produced is at least O(N*N), and execution time is at least O(4*N*N), as
   a function of the length of these pending lists.  */

static int pending_lists_length;

/* The last insn upon which all memory references must depend.
   This is an insn which flushed the pending lists, creating a dependency
   between it and all previously pending memory references.  This creates
   a barrier (or a checkpoint) which no memory reference is allowed to cross.

   This includes all non constant CALL_INSNs.  When we do interprocedural
   alias analysis, this restriction can be relaxed.
   This may also be an INSN that writes memory if the pending lists grow
   too large.  */

static rtx last_pending_memory_flush;

/* The last function call we have seen.  All hard regs, and, of course,
   the last function call, must depend on this.  */

static rtx last_function_call;

/* The LOG_LINKS field of this is a list of insns which use a pseudo register
   that does not already cross a call.  We create dependencies between each
   of those insn and the next call insn, to ensure that they won't cross a call
   after scheduling is done.  */

static rtx sched_before_next_call;

/* Pointer to the last instruction scheduled.  Used by rank_for_schedule,
   so that insns independent of the last scheduled insn will be preferred
   over dependent instructions.  */

static rtx last_scheduled_insn;

/* Data structures for the computation of data dependences in a regions.  We
   keep one copy of each of the declared above variables for each bb in the
   region.  Before analyzing the data dependences for a bb, its variables
   are initialized as a function of the variables of its predecessors.  When
   the analysis for a bb completes, we save the contents of each variable X
   to a corresponding bb_X[bb] variable.  For example, pending_read_insns is
   copied to bb_pending_read_insns[bb].  Another change is that few
   variables are now a list of insns rather than a single insn:
   last_pending_memory_flash, last_function_call, reg_last_sets.  The
   manipulation of these variables was changed appropriately.  */

static rtx **bb_reg_last_uses;
static rtx **bb_reg_last_sets;
static rtx **bb_reg_last_clobbers;

static rtx *bb_pending_read_insns;
static rtx *bb_pending_read_mems;
static rtx *bb_pending_write_insns;
static rtx *bb_pending_write_mems;
static int *bb_pending_lists_length;

static rtx *bb_last_pending_memory_flush;
static rtx *bb_last_function_call;
static rtx *bb_sched_before_next_call;

/* functions for construction of the control flow graph.  */

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
  /* check for labels referred to other thn by jumps */
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
build_control_flow (s_preds, s_succs, num_preds, num_succs)
     int_list_ptr *s_preds;
     int_list_ptr *s_succs;
     int *num_preds;
     int *num_succs;
{
  int i;
  int_list_ptr succ;
  int unreachable;

  /* Count the number of edges in the cfg.  */
  nr_edges = 0;
  unreachable = 0;
  for (i = 0; i < n_basic_blocks; i++)
    {
      nr_edges += num_succs[i];

      /* Unreachable loops with more than one basic block are detected
	 during the DFS traversal in find_rgns.

	 Unreachable loops with a single block are detected here.  This
	 test is redundant with the one in find_rgns, but it's much
	 cheaper to go ahead and catch the trivial case here.  */
      if (num_preds[i] == 0
	  || (num_preds[i] == 1 && INT_LIST_VAL (s_preds[i]) == i))
	unreachable = 1;
    }

  /* Account for entry/exit edges.  */
  nr_edges += 2;

  in_edges = (int *) xmalloc (n_basic_blocks * sizeof (int));
  out_edges = (int *) xmalloc (n_basic_blocks * sizeof (int));
  bzero ((char *) in_edges, n_basic_blocks * sizeof (int));
  bzero ((char *) out_edges, n_basic_blocks * sizeof (int));

  edge_table = (haifa_edge *) xmalloc ((nr_edges) * sizeof (haifa_edge));
  bzero ((char *) edge_table, ((nr_edges) * sizeof (haifa_edge)));

  nr_edges = 0;
  for (i = 0; i < n_basic_blocks; i++)
    for (succ = s_succs[i]; succ; succ = succ->next)
      {
	if (INT_LIST_VAL (succ) != EXIT_BLOCK)
	  new_edge (i, INT_LIST_VAL (succ));
      }

  /* increment by 1, since edge 0 is unused.  */
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

  /* check for duplicates */
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

/* Compute  bitwise union  of two  bitsets.  */
#define BITSET_UNION(set1, set2, len)                                \
do { register bitset tp = set1, sp = set2;                           \
     register int i;                                                 \
     for (i = 0; i < len; i++)                                       \
       *(tp++) |= *(sp++); } while (0)

/* Compute  bitwise intersection  of two  bitsets.  */
#define BITSET_INTER(set1, set2, len)                                \
do { register bitset tp = set1, sp = set2;                           \
     register int i;                                                 \
     for (i = 0; i < len; i++)                                       \
       *(tp++) &= *(sp++); } while (0)

/* Compute bitwise   difference of  two bitsets.  */
#define BITSET_DIFFER(set1, set2, len)                               \
do { register bitset tp = set1, sp = set2;                           \
     register int i;                                                 \
     for (i = 0; i < len; i++)                                       \
       *(tp++) &= ~*(sp++); } while (0)

/* Inverts every bit of bitset 'set' */
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


/* Check if the index'th bit in bitset  set is on.  */

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

  /* bblst table space is reused in each call to extract_bitlst */
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


/* functions for the construction of regions */

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
find_rgns (s_preds, s_succs, num_preds, num_succs, dom)
     int_list_ptr *s_preds;
     int_list_ptr *s_succs;
     int *num_preds;
     int *num_succs;
     sbitmap *dom;
{
  int *max_hdr, *dfs_nr, *stack, *queue, *degree;
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

  /* Perform a DFS traversal of the cfg.  Identify loop headers, inner loops
     and a mapping from block to its loop header (if the block is contained
     in a loop, else -1).

     Store results in HEADER, INNER, and MAX_HDR respectively, these will
     be used as inputs to the second traversal.

     STACK, SP and DFS_NR are only used during the first traversal.  */

  /* Allocate and initialize variables for the first traversal.  */
  max_hdr = (int *) alloca (n_basic_blocks * sizeof (int));
  dfs_nr = (int *) alloca (n_basic_blocks * sizeof (int));
  bzero ((char *) dfs_nr, n_basic_blocks * sizeof (int));
  stack = (int *) alloca (nr_edges * sizeof (int));

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

  /* Compute the in-degree of every block in the graph */
  for (i = 0; i < n_basic_blocks; i++)
    degree[i] = num_preds[i];

  /* Do not perform region scheduling if there are any unreachable
     blocks.  */
  if (!unreachable)
    {
      if (no_loops)
	SET_BIT (header, 0);

      /* Second travsersal:find reducible inner loops and topologically sort
	 block of each region.  */

      queue = (int *) alloca (n_basic_blocks * sizeof (int));

      /* Find blocks which are inner loop headers.  We still have non-reducible
	 loops to consider at this point.  */
      for (i = 0; i < n_basic_blocks; i++)
	{
	  if (TEST_BIT (header, i) && TEST_BIT (inner, i))
	    {
	      int_list_ptr ps;
	      int j;

	      /* Now check that the loop is reducible.  We do this separate
		 from finding inner loops so that we do not find a reducible
		 loop which contains an inner  non-reducible loop.

		 A simple way to find reducible/natrual loops is to verify
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

	      /* If we exited the loop early, then I is the header of a non
		 reducible loop and we should quit processing it now.  */
	      if (j != n_basic_blocks)
		continue;

	      /* I is a header of an inner loop, or block 0 in a subroutine
		 with no loops at all.  */
	      head = tail = -1;
	      too_large_failure = 0;
	      loop_head = max_hdr[i];

	      /* Decrease degree of all I's successors for topological
		 ordering.  */
	      for (ps = s_succs[i]; ps; ps = ps->next)
		if (INT_LIST_VAL (ps) != EXIT_BLOCK
		    && INT_LIST_VAL (ps) != ENTRY_BLOCK)
		  --degree[INT_LIST_VAL(ps)];

	      /* Estimate # insns, and count # blocks in the region.  */
	      num_bbs = 1;
	      num_insns	= (INSN_LUID (BLOCK_END (i))
			   - INSN_LUID (BLOCK_HEAD (i)));


	      /* Find all loop latches (blocks which back edges to the loop
		 header) or all the leaf blocks in the cfg has no loops.

		 Place those blocks into the queue.  */
	      if (no_loops)
		{
		  for (j = 0; j < n_basic_blocks; j++)
		    /* Leaf nodes have only a single successor which must
		       be EXIT_BLOCK.  */
		    if (num_succs[j] == 1
			&& INT_LIST_VAL (s_succs[j]) == EXIT_BLOCK)
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
		  int_list_ptr ps;

		  for (ps = s_preds[i]; ps; ps = ps->next)
		    {
		      node = INT_LIST_VAL (ps);

		      if (node == ENTRY_BLOCK || node == EXIT_BLOCK)
			continue;
 
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
		  int_list_ptr ps;
		  child = queue[++head];

		  for (ps = s_preds[child]; ps; ps = ps->next)
		    {
		      node = INT_LIST_VAL (ps);

		      /* See discussion above about nodes not marked as in
			 this loop during the initial DFS traversal.  */
		      if (node == ENTRY_BLOCK || node == EXIT_BLOCK
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

		  /* Remove blocks from queue[] when their in degree becomes
		 zero.  Repeat until no blocks are left on the list.  This
		 produces a topological list of blocks in the region.  */
		  while (tail >= 0)
		    {
		      int_list_ptr ps;

		      if (head < 0)
			head = tail;
		      child = queue[head];
		      if (degree[child] == 0)
			{
			  degree[child] = -1;
			  rgn_bb_table[idx++] = child;
			  BLOCK_TO_BB (child) = ++count;
			  CONTAINING_RGN (child) = nr_regions;
			  queue[head] = queue[tail--];

			  for (ps = s_succs[child]; ps; ps = ps->next)
			    if (INT_LIST_VAL (ps) != ENTRY_BLOCK
				&& INT_LIST_VAL (ps) != EXIT_BLOCK)
			      --degree[INT_LIST_VAL (ps)];
			}
		      else
			--head;
		    }
		  ++nr_regions;
		}
	    }
	}
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

  free (passed);
  free (header);
  free (inner);
  free (in_queue);
  free (in_stack);
}


/* functions for regions scheduling information */

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

  /* intialize dom[bb] to '111..1' */
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

      /* the successor doesn't belong the region? */
      if (CONTAINING_RGN (TO_BLOCK (fst_out_edge)) !=
	  CONTAINING_RGN (BB_TO_BLOCK (bb)))
	++nr_rgn_out_edges;

      while (fst_out_edge != nxt_out_edge)
	{
	  ++nr_out_edges;
	  /* the successor doesn't belong the region? */
	  if (CONTAINING_RGN (TO_BLOCK (nxt_out_edge)) !=
	      CONTAINING_RGN (BB_TO_BLOCK (bb)))
	    ++nr_rgn_out_edges;
	  BITSET_ADD (pot_split[bb], EDGE_TO_BIT (nxt_out_edge), edgeset_size);
	  nxt_out_edge = NEXT_OUT (nxt_out_edge);

	}

      /* now nr_rgn_out_edges is the number of region-exit edges from pred,
         and nr_out_edges will be the number of pred out edges not leaving
         the region.  */
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

/* functions for target info */

/* Compute in BL the list of split-edges of bb_src relatively to bb_trg.
   Note that bb_trg dominates bb_src.  */

static void
split_edges (bb_src, bb_trg, bl)
     int bb_src;
     int bb_trg;
     edgelst *bl;
{
  int es = edgeset_size;
  edgeset src = (edgeset) alloca (es * sizeof (HOST_WIDE_INT));

  while (es--)
    src[es] = (pot_split[bb_src])[es];
  BITSET_DIFFER (src, pot_split[bb_trg], edgeset_size);
  extract_bitlst (src, edgeset_size, bl);
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

  /* define some of the fields for the target bb as well */
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


/* functions for speculative scheduing */

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
      /* Global registers are assumed live */
      return 0;
    }
  else
    {
      if (regno < FIRST_PSEUDO_REGISTER)
	{
	  /* check for hard registers */
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
	  /* check for psuedo registers */
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
  /* find the registers set by instruction */
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
  /* find the registers set by instruction */
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
   - test-insn is either a compare or a branch, not in the same block as load-insn
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

/* Indexed by INSN_UID, and set if there's DEF-USE dependence between */
/* some speculatively moved load insn and this one.  */
char *fed_by_spec_load;
char *is_load_insn;

/* Non-zero if block bb_to is equal to, or reachable from block bb_from.  */
#define IS_REACHABLE(bb_from, bb_to)					\
(bb_from == bb_to                                                       \
   || IS_RGN_ENTRY (bb_from)						\
   || (bitset_member (ancestor_edges[bb_to],				\
		      EDGE_TO_BIT (IN_EDGES (BB_TO_BLOCK (bb_from))),	\
		      edgeset_size)))
#define FED_BY_SPEC_LOAD(insn) (fed_by_spec_load[INSN_UID (insn)])
#define IS_LOAD_INSN(insn) (is_load_insn[INSN_UID (insn)])

/* Non-zero iff the address is comprised from at most 1 register */
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

/* On the path from the insn to load_insn_bb, find a conditional branch */
/* depending on insn, that guards the speculative load.  */

static int
find_conditional_protection (insn, load_insn_bb)
     rtx insn;
     int load_insn_bb;
{
  rtx link;

  /* iterate through DEF-USE forward dependences */
  for (link = INSN_DEPEND (insn); link; link = XEXP (link, 1))
    {
      rtx next = XEXP (link, 0);
      if ((CONTAINING_RGN (INSN_BLOCK (next)) ==
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

      /* must be a DEF-USE dependence upon non-branch */
      if (GET_MODE (link) != VOIDmode
	  || GET_CODE (insn1) == JUMP_INSN)
	continue;

      /* must exist a path: region-entry -> ... -> bb_trg -> ... load_insn */
      if (INSN_BB (insn1) == bb_src
	  || (CONTAINING_RGN (INSN_BLOCK (insn1))
	      != CONTAINING_RGN (BB_TO_BLOCK (bb_src)))
	  || (!IS_REACHABLE (bb_trg, INSN_BB (insn1))
	      && !IS_REACHABLE (INSN_BB (insn1), bb_trg)))
	continue;

      /* now search for the conditional-branch */
      if (find_conditional_protection (insn1, bb_src))
	return 1;

      /* recursive step: search another insn1, "above" current insn1.  */
      return is_conditionally_protected (insn1, bb_src, bb_trg);
    }

  /* the chain does not exsist */
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
    /* must have exactly one escape block */
    return 0;

  for (back_link = LOG_LINKS (load_insn);
       back_link; back_link = XEXP (back_link, 1))
    {
      rtx insn1 = XEXP (back_link, 0);

      if (GET_MODE (back_link) == VOIDmode)
	{
	  /* found a DEF-USE dependence (insn1, load_insn) */
	  rtx fore_link;

	  for (fore_link = INSN_DEPEND (insn1);
	       fore_link; fore_link = XEXP (fore_link, 1))
	    {
	      rtx insn2 = XEXP (fore_link, 0);
	      if (GET_MODE (fore_link) == VOIDmode)
		{
		  /* found a DEF-USE dependence (insn1, insn2) */
		  if (haifa_classify_insn (insn2) != PFREE_CANDIDATE)
		    /* insn2 not guaranteed to be a 1 base reg load */
		    continue;

		  if (INSN_BB (insn2) == bb_trg)
		    /* insn2 is the similar load, in the target block */
		    return 1;

		  if (*(candp->split_bbs.first_member) == INSN_BLOCK (insn2))
		    /* insn2 is a similar load, in a split-block */
		    return 1;
		}
	    }
	}
    }

  /* couldn't find a similar load */
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
      /* The insn uses memory */
      /* a volatile load */
      if (MEM_VOLATILE_P (x))
	return IRISKY;
      /* an exception-free load */
      if (!may_trap_p (x))
	return IFREE;
      /* a load with 1 base register, to be further checked */
      if (CONST_BASED_ADDRESS_P (XEXP (x, 0)))
	return PFREE_CANDIDATE;
      /* no info on the load, to be further checked */
      return PRISKY_CANDIDATE;
    }
  else
    {
      char *fmt;
      int i, insn_class = TRAP_FREE;

      /* neither store nor load, check if it may cause a trap */
      if (may_trap_p (x))
	return TRAP_RISKY;
      /* recursive step: walk the insn...  */
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
	      /* test if it is a 'store' */
	      tmp_class = may_trap_exp (XEXP (XVECEXP (pat, 0, i), 0), 1);
	      break;
	    case SET:
	      /* test if it is a store */
	      tmp_class = may_trap_exp (SET_DEST (XVECEXP (pat, 0, i)), 1);
	      if (tmp_class == TRAP_RISKY)
		break;
	      /* test if it is a load  */
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
	  /* test if it is a 'store' */
	  tmp_class = may_trap_exp (XEXP (pat, 0), 1);
	  break;
	case SET:
	  /* test if it is a store */
	  tmp_class = may_trap_exp (SET_DEST (pat), 1);
	  if (tmp_class == TRAP_RISKY)
	    break;
	  /* test if it is a load  */
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
    /* dependence may 'hide' out of the region.  */
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

  /* handle non-load insns */
  switch (insn_class)
    {
    case TRAP_FREE:
      return 1;
    case TRAP_RISKY:
      return 0;
    default:;
    }

  /* handle loads */
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
      /* don't 'break' here: PFREE-candidate is also PRISKY-candidate */
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


/* Return 1 if the pair (insn, x) is found in (LIST, LIST1), or 0 otherwise.  */

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

/* Return the issue-delay of an insn */

HAIFA_INLINE static int
insn_issue_delay (insn)
     rtx insn;
{
  int i, delay = 0;
  int unit = insn_unit (insn);

  /* efficiency note: in fact, we are working 'hard' to compute a
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
  int tick = unit_tick[instance];	/* issue time of the last issued insn */

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
      int this_cost;

#if MAX_MULTIPLICITY > 1
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

  /* in this case estimate cost without caring how insn is used.  */
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
    cost = 1;
#ifdef ADJUST_COST
  else if (!LINK_COST_ZERO (link))
    {
      int ncost = cost;

      ADJUST_COST (used, link, insn, ncost);
      if (ncost <= 1)
	LINK_COST_FREE (link) = ncost = 1;
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

	    /* critical path is meaningful in block boundaries only */
	    if (INSN_BLOCK (next) != INSN_BLOCK (insn))
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
  if (current_nr_blocks <= 1)
    {
      free_list (&pending_read_insns, &unused_insn_list);
      free_list (&pending_write_insns, &unused_insn_list);
      free_list (&pending_read_mems, &unused_expr_list);
      free_list (&pending_write_mems, &unused_expr_list);
    }
  else
    {
      /* interblock scheduling */
      int bb;

      for (bb = 0; bb < current_nr_blocks; bb++)
	{
	  free_list (&bb_pending_read_insns[bb], &unused_insn_list);
	  free_list (&bb_pending_write_insns[bb], &unused_insn_list);
	  free_list (&bb_pending_read_mems[bb], &unused_expr_list);
	  free_list (&bb_pending_write_mems[bb], &unused_expr_list);
	}
    }
}

/* Add an INSN and MEM reference pair to a pending INSN_LIST and MEM_LIST.
   The MEM is a memory reference contained within INSN, which we are saving
   so that we can do memory aliasing on it.  */

static void
add_insn_mem_dependence (insn_list, mem_list, insn, mem)
     rtx *insn_list, *mem_list, insn, mem;
{
  register rtx link;

  link = alloc_INSN_LIST (insn, *insn_list);
  *insn_list = link;

  link = alloc_EXPR_LIST (VOIDmode, mem, *mem_list);
  *mem_list = link;

  pending_lists_length++;
}


/* Make a dependency between every memory reference on the pending lists
   and INSN, thus flushing the pending lists.  If ONLY_WRITE, don't flush
   the read list.  */

static void
flush_pending_lists (insn, only_write)
     rtx insn;
     int only_write;
{
  rtx u;
  rtx link;

  while (pending_read_insns && ! only_write)
    {
      add_dependence (insn, XEXP (pending_read_insns, 0), REG_DEP_ANTI);

      link = pending_read_insns;
      pending_read_insns = XEXP (pending_read_insns, 1);
      XEXP (link, 1) = unused_insn_list;
      unused_insn_list = link;

      link = pending_read_mems;
      pending_read_mems = XEXP (pending_read_mems, 1);
      XEXP (link, 1) = unused_expr_list;
      unused_expr_list = link;
    }
  while (pending_write_insns)
    {
      add_dependence (insn, XEXP (pending_write_insns, 0), REG_DEP_ANTI);

      link = pending_write_insns;
      pending_write_insns = XEXP (pending_write_insns, 1);
      XEXP (link, 1) = unused_insn_list;
      unused_insn_list = link;

      link = pending_write_mems;
      pending_write_mems = XEXP (pending_write_mems, 1);
      XEXP (link, 1) = unused_expr_list;
      unused_expr_list = link;
    }
  pending_lists_length = 0;

  /* last_pending_memory_flush is now a list of insns */
  for (u = last_pending_memory_flush; u; u = XEXP (u, 1))
    add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);

  free_list (&last_pending_memory_flush, &unused_insn_list);
  last_pending_memory_flush = alloc_INSN_LIST (insn, NULL_RTX);
}

/* Analyze a single SET or CLOBBER rtx, X, creating all dependencies generated
   by the write to the destination of X, and reads of everything mentioned.  */

static void
sched_analyze_1 (x, insn)
     rtx x;
     rtx insn;
{
  register int regno;
  register rtx dest = SET_DEST (x);
  enum rtx_code code = GET_CODE (x);

  if (dest == 0)
    return;

  if (GET_CODE (dest) == PARALLEL
      && GET_MODE (dest) == BLKmode)
    {
      register int i;
      for (i = XVECLEN (dest, 0) - 1; i >= 0; i--)
	sched_analyze_1 (XVECEXP (dest, 0, i), insn);
      if (GET_CODE (x) == SET)
	sched_analyze_2 (SET_SRC (x), insn);
      return;
    }

  while (GET_CODE (dest) == STRICT_LOW_PART || GET_CODE (dest) == SUBREG
      || GET_CODE (dest) == ZERO_EXTRACT || GET_CODE (dest) == SIGN_EXTRACT)
    {
      if (GET_CODE (dest) == ZERO_EXTRACT || GET_CODE (dest) == SIGN_EXTRACT)
	{
	  /* The second and third arguments are values read by this insn.  */
	  sched_analyze_2 (XEXP (dest, 1), insn);
	  sched_analyze_2 (XEXP (dest, 2), insn);
	}
      dest = SUBREG_REG (dest);
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
	      rtx u;

	      for (u = reg_last_uses[regno + i]; u; u = XEXP (u, 1))
		add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);

	      for (u = reg_last_sets[regno + i]; u; u = XEXP (u, 1))
		add_dependence (insn, XEXP (u, 0), REG_DEP_OUTPUT);

	      /* Clobbers need not be ordered with respect to one another,
		 but sets must be ordered with respect to a pending clobber. */
	      if (code == SET)
		{
	          reg_last_uses[regno + i] = 0;
	          for (u = reg_last_clobbers[regno + i]; u; u = XEXP (u, 1))
		    add_dependence (insn, XEXP (u, 0), REG_DEP_OUTPUT);
	          SET_REGNO_REG_SET (reg_pending_sets, regno + i);
		}
	      else
		SET_REGNO_REG_SET (reg_pending_clobbers, regno + i);

	      /* Function calls clobber all call_used regs.  */
	      if (global_regs[regno + i]
		  || (code == SET && call_used_regs[regno + i]))
		for (u = last_function_call; u; u = XEXP (u, 1))
		  add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);
	    }
	}
      else
	{
	  rtx u;

	  for (u = reg_last_uses[regno]; u; u = XEXP (u, 1))
	    add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);

	  for (u = reg_last_sets[regno]; u; u = XEXP (u, 1))
	    add_dependence (insn, XEXP (u, 0), REG_DEP_OUTPUT);

	  if (code == SET)
	    {
	      reg_last_uses[regno] = 0;
	      for (u = reg_last_clobbers[regno]; u; u = XEXP (u, 1))
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
	    sched_analyze_2 (XEXP (reg_known_value[regno], 0), insn);

	  /* Don't let it cross a call after scheduling if it doesn't
	     already cross one.  */

	  if (REG_N_CALLS_CROSSED (regno) == 0)
	    for (u = last_function_call; u; u = XEXP (u, 1))
	      add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);
	}
    }
  else if (GET_CODE (dest) == MEM)
    {
      /* Writing memory.  */

      if (pending_lists_length > 32)
	{
	  /* Flush all pending reads and writes to prevent the pending lists
	     from getting any larger.  Insn scheduling runs too slowly when
	     these lists get long.  The number 32 was chosen because it
	     seems like a reasonable number.  When compiling GCC with itself,
	     this flush occurs 8 times for sparc, and 10 times for m88k using
	     the number 32.  */
	  flush_pending_lists (insn, 0);
	}
      else
	{
	  rtx u;
	  rtx pending, pending_mem;

	  pending = pending_read_insns;
	  pending_mem = pending_read_mems;
	  while (pending)
	    {
	      /* If a dependency already exists, don't create a new one.  */
	      if (!find_insn_list (XEXP (pending, 0), LOG_LINKS (insn)))
		if (anti_dependence (XEXP (pending_mem, 0), dest))
		  add_dependence (insn, XEXP (pending, 0), REG_DEP_ANTI);

	      pending = XEXP (pending, 1);
	      pending_mem = XEXP (pending_mem, 1);
	    }

	  pending = pending_write_insns;
	  pending_mem = pending_write_mems;
	  while (pending)
	    {
	      /* If a dependency already exists, don't create a new one.  */
	      if (!find_insn_list (XEXP (pending, 0), LOG_LINKS (insn)))
		if (output_dependence (XEXP (pending_mem, 0), dest))
		  add_dependence (insn, XEXP (pending, 0), REG_DEP_OUTPUT);

	      pending = XEXP (pending, 1);
	      pending_mem = XEXP (pending_mem, 1);
	    }

	  for (u = last_pending_memory_flush; u; u = XEXP (u, 1))
	    add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);

	  add_insn_mem_dependence (&pending_write_insns, &pending_write_mems,
				   insn, dest);
	}
      sched_analyze_2 (XEXP (dest, 0), insn);
    }

  /* Analyze reads.  */
  if (GET_CODE (x) == SET)
    sched_analyze_2 (SET_SRC (x), insn);
}

/* Analyze the uses of memory and registers in rtx X in INSN.  */

static void
sched_analyze_2 (x, insn)
     rtx x;
     rtx insn;
{
  register int i;
  register int j;
  register enum rtx_code code;
  register char *fmt;

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
		reg_last_uses[regno + i]
		  = alloc_INSN_LIST (insn, reg_last_uses[regno + i]);

		for (u = reg_last_sets[regno + i]; u; u = XEXP (u, 1))
		  add_dependence (insn, XEXP (u, 0), 0);

		/* ??? This should never happen.  */
		for (u = reg_last_clobbers[regno + i]; u; u = XEXP (u, 1))
		  add_dependence (insn, XEXP (u, 0), 0);

		if ((call_used_regs[regno + i] || global_regs[regno + i]))
		  /* Function calls clobber all call_used regs.  */
		  for (u = last_function_call; u; u = XEXP (u, 1))
		    add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);
	      }
	  }
	else
	  {
	    reg_last_uses[regno] = alloc_INSN_LIST (insn, reg_last_uses[regno]);

	    for (u = reg_last_sets[regno]; u; u = XEXP (u, 1))
	      add_dependence (insn, XEXP (u, 0), 0);

	    /* ??? This should never happen.  */
	    for (u = reg_last_clobbers[regno]; u; u = XEXP (u, 1))
	      add_dependence (insn, XEXP (u, 0), 0);

	    /* Pseudos that are REG_EQUIV to something may be replaced
	       by that during reloading.  We need only add dependencies for
	       the address in the REG_EQUIV note.  */
	    if (!reload_completed
		&& reg_known_equiv_p[regno]
		&& GET_CODE (reg_known_value[regno]) == MEM)
	      sched_analyze_2 (XEXP (reg_known_value[regno], 0), insn);

	    /* If the register does not already cross any calls, then add this
	       insn to the sched_before_next_call list so that it will still
	       not cross calls after scheduling.  */
	    if (REG_N_CALLS_CROSSED (regno) == 0)
	      add_dependence (sched_before_next_call, insn, REG_DEP_ANTI);
	  }
	return;
      }

    case MEM:
      {
	/* Reading memory.  */
	rtx u;
	rtx pending, pending_mem;

	pending = pending_read_insns;
	pending_mem = pending_read_mems;
	while (pending)
	  {
	    /* If a dependency already exists, don't create a new one.  */
	    if (!find_insn_list (XEXP (pending, 0), LOG_LINKS (insn)))
	      if (read_dependence (XEXP (pending_mem, 0), x))
		add_dependence (insn, XEXP (pending, 0), REG_DEP_ANTI);

	    pending = XEXP (pending, 1);
	    pending_mem = XEXP (pending_mem, 1);
	  }

	pending = pending_write_insns;
	pending_mem = pending_write_mems;
	while (pending)
	  {
	    /* If a dependency already exists, don't create a new one.  */
	    if (!find_insn_list (XEXP (pending, 0), LOG_LINKS (insn)))
	      if (true_dependence (XEXP (pending_mem, 0), VOIDmode,
		  x, rtx_varies_p))
		add_dependence (insn, XEXP (pending, 0), 0);

	    pending = XEXP (pending, 1);
	    pending_mem = XEXP (pending_mem, 1);
	  }

	for (u = last_pending_memory_flush; u; u = XEXP (u, 1))
	  add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);

	/* Always add these dependencies to pending_reads, since
	   this insn may be followed by a write.  */
	add_insn_mem_dependence (&pending_read_insns, &pending_read_mems,
				 insn, x);

	/* Take advantage of tail recursion here.  */
	sched_analyze_2 (XEXP (x, 0), insn);
	return;
      }

    /* Force pending stores to memory in case a trap handler needs them.  */
    case TRAP_IF:
      flush_pending_lists (insn, 1);
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
		for (u = reg_last_uses[i]; u; u = XEXP (u, 1))
		  add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);
		reg_last_uses[i] = 0;

		for (u = reg_last_sets[i]; u; u = XEXP (u, 1))
		  add_dependence (insn, XEXP (u, 0), 0);

		for (u = reg_last_clobbers[i]; u; u = XEXP (u, 1))
		  add_dependence (insn, XEXP (u, 0), 0);
	      }
	    reg_pending_sets_all = 1;

	    flush_pending_lists (insn, 0);
	  }

	/* For all ASM_OPERANDS, we must traverse the vector of input operands.
	   We can not just fall through here since then we would be confused
	   by the ASM_INPUT rtx inside ASM_OPERANDS, which do not indicate
	   traditional asms unlike their normal usage.  */

	if (code == ASM_OPERANDS)
	  {
	    for (j = 0; j < ASM_OPERANDS_INPUT_LENGTH (x); j++)
	      sched_analyze_2 (ASM_OPERANDS_INPUT (x, j), insn);
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
      sched_analyze_2 (XEXP (x, 0), insn);
      sched_analyze_1 (x, insn);
      return;

    default:
      break;
    }

  /* Other cases: walk the insn.  */
  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	sched_analyze_2 (XEXP (x, i), insn);
      else if (fmt[i] == 'E')
	for (j = 0; j < XVECLEN (x, i); j++)
	  sched_analyze_2 (XVECEXP (x, i, j), insn);
    }
}

/* Analyze an INSN with pattern X to find all dependencies.  */

static void
sched_analyze_insn (x, insn, loop_notes)
     rtx x, insn;
     rtx loop_notes;
{
  register RTX_CODE code = GET_CODE (x);
  rtx link;
  int maxreg = max_reg_num ();
  int i;

  if (code == SET || code == CLOBBER)
    sched_analyze_1 (x, insn);
  else if (code == PARALLEL)
    {
      register int i;
      for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
	{
	  code = GET_CODE (XVECEXP (x, 0, i));
	  if (code == SET || code == CLOBBER)
	    sched_analyze_1 (XVECEXP (x, 0, i), insn);
	  else
	    sched_analyze_2 (XVECEXP (x, 0, i), insn);
	}
    }
  else
    sched_analyze_2 (x, insn);

  /* Mark registers CLOBBERED or used by called function.  */
  if (GET_CODE (insn) == CALL_INSN)
    for (link = CALL_INSN_FUNCTION_USAGE (insn); link; link = XEXP (link, 1))
      {
	if (GET_CODE (XEXP (link, 0)) == CLOBBER)
	  sched_analyze_1 (XEXP (link, 0), insn);
	else
	  sched_analyze_2 (XEXP (link, 0), insn);
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
	      for (u = reg_last_uses[i]; u; u = XEXP (u, 1))
		add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);
	      reg_last_uses[i] = 0;

	      for (u = reg_last_sets[i]; u; u = XEXP (u, 1))
		add_dependence (insn, XEXP (u, 0), 0);

	      for (u = reg_last_clobbers[i]; u; u = XEXP (u, 1))
		add_dependence (insn, XEXP (u, 0), 0);
	    }
	  reg_pending_sets_all = 1;

	  flush_pending_lists (insn, 0);
	}

    }

  /* Accumulate clobbers until the next set so that it will be output dependant
     on all of them.  At the next set we can clear the clobber list, since
     subsequent sets will be output dependant on it.  */
  EXECUTE_IF_SET_IN_REG_SET (reg_pending_sets, 0, i,
			     {
			       free_list (&reg_last_sets[i], &unused_insn_list);
			       free_list (&reg_last_clobbers[i],
					  &unused_insn_list);
			       reg_last_sets[i]
				 = alloc_INSN_LIST (insn, NULL_RTX);
			     });
  EXECUTE_IF_SET_IN_REG_SET (reg_pending_clobbers, 0, i,
			     {
			       reg_last_clobbers[i]
				 = alloc_INSN_LIST (insn, reg_last_clobbers[i]);
			     });
  CLEAR_REG_SET (reg_pending_sets);
  CLEAR_REG_SET (reg_pending_clobbers);

  if (reg_pending_sets_all)
    {
      for (i = 0; i < maxreg; i++)
	{
	  free_list (&reg_last_sets[i], &unused_insn_list);
	  reg_last_sets[i] = alloc_INSN_LIST (insn, NULL_RTX);
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
sched_analyze (head, tail)
     rtx head, tail;
{
  register rtx insn;
  register rtx u;
  rtx loop_notes = 0;

  for (insn = head;; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == INSN || GET_CODE (insn) == JUMP_INSN)
	{
	  /* Make each JUMP_INSN a scheduling barrier for memory references.  */
	  if (GET_CODE (insn) == JUMP_INSN)
	    last_pending_memory_flush
	      = alloc_INSN_LIST (insn, last_pending_memory_flush);
	  sched_analyze_insn (PATTERN (insn), insn, loop_notes);
	  loop_notes = 0;
	}
      else if (GET_CODE (insn) == CALL_INSN)
	{
	  rtx x;
	  register int i;

	  CANT_MOVE (insn) = 1;

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
		  for (u = reg_last_uses[i]; u; u = XEXP (u, 1))
		    add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);

		  reg_last_uses[i] = 0;

		  for (u = reg_last_sets[i]; u; u = XEXP (u, 1))
		    add_dependence (insn, XEXP (u, 0), 0);

		  for (u = reg_last_clobbers[i]; u; u = XEXP (u, 1))
		    add_dependence (insn, XEXP (u, 0), 0);
		}
	      reg_pending_sets_all = 1;

	      /* Add a pair of fake REG_NOTE which we will later
		 convert back into a NOTE_INSN_SETJMP note.  See
		 reemit_notes for why we use a pair of NOTEs.  */
	      REG_NOTES (insn) = alloc_EXPR_LIST (REG_DEAD,
						  GEN_INT (0),
						  REG_NOTES (insn));
	      REG_NOTES (insn) = alloc_EXPR_LIST (REG_DEAD,
						  GEN_INT (NOTE_INSN_SETJMP),
						  REG_NOTES (insn));
	    }
	  else
	    {
	      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
		if (call_used_regs[i] || global_regs[i])
		  {
		    for (u = reg_last_uses[i]; u; u = XEXP (u, 1))
		      add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);

		    for (u = reg_last_sets[i]; u; u = XEXP (u, 1))
		      add_dependence (insn, XEXP (u, 0), REG_DEP_ANTI);

		    SET_REGNO_REG_SET (reg_pending_clobbers, i);
		  }
	    }

	  /* For each insn which shouldn't cross a call, add a dependence
	     between that insn and this call insn.  */
	  x = LOG_LINKS (sched_before_next_call);
	  while (x)
	    {
	      add_dependence (insn, XEXP (x, 0), REG_DEP_ANTI);
	      x = XEXP (x, 1);
	    }
	  LOG_LINKS (sched_before_next_call) = 0;

	  sched_analyze_insn (PATTERN (insn), insn, loop_notes);
	  loop_notes = 0;

	  /* In the absence of interprocedural alias analysis, we must flush
	     all pending reads and writes, and start new dependencies starting
	     from here.  But only flush writes for constant calls (which may
	     be passed a pointer to something we haven't written yet).  */
	  flush_pending_lists (insn, CONST_CALL_P (insn));

	  /* Depend this function call (actually, the user of this
	     function call) on all hard register clobberage.  */

	  /* last_function_call is now a list of insns */
	  free_list(&last_function_call, &unused_insn_list);
	  last_function_call = alloc_INSN_LIST (insn, NULL_RTX);
	}

      /* See comments on reemit_notes as to why we do this.  */
      /* ??? Actually, the reemit_notes just say what is done, not why.  */

      else if (GET_CODE (insn) == NOTE
	       && (NOTE_LINE_NUMBER (insn) == NOTE_INSN_RANGE_START
		   || NOTE_LINE_NUMBER (insn) == NOTE_INSN_RANGE_END))
	{
	  loop_notes = alloc_EXPR_LIST (REG_DEAD, NOTE_RANGE_INFO (insn),
					loop_notes);
	  loop_notes = alloc_EXPR_LIST (REG_DEAD,
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
	  loop_notes = alloc_EXPR_LIST (REG_DEAD,
					GEN_INT (NOTE_BLOCK_NUMBER (insn)),
					loop_notes);
	  loop_notes = alloc_EXPR_LIST (REG_DEAD,
					GEN_INT (NOTE_LINE_NUMBER (insn)),
					loop_notes);
	  CONST_CALL_P (loop_notes) = CONST_CALL_P (insn);
	}

      if (insn == tail)
	return;
    }
  abort ();
}

/* Called when we see a set of a register.  If death is true, then we are
   scanning backwards.  Mark that register as unborn.  If nobody says
   otherwise, that is how things will remain.  If death is false, then we
   are scanning forwards.  Mark that register as being born.  */

static void
sched_note_set (x, death)
     rtx x;
     int death;
{
  register int regno;
  register rtx reg = SET_DEST (x);
  int subreg_p = 0;

  if (reg == 0)
    return;

  if (GET_CODE (reg) == PARALLEL
      && GET_MODE (reg) == BLKmode)
    {
      register int i;
      for (i = XVECLEN (reg, 0) - 1; i >= 0; i--)
	sched_note_set (XVECEXP (reg, 0, i), death);
      return;
    }

  while (GET_CODE (reg) == SUBREG || GET_CODE (reg) == STRICT_LOW_PART
	 || GET_CODE (reg) == SIGN_EXTRACT || GET_CODE (reg) == ZERO_EXTRACT)
    {
      /* Must treat modification of just one hardware register of a multi-reg
         value or just a byte field of a register exactly the same way that
         mark_set_1 in flow.c does, i.e. anything except a paradoxical subreg
         does not kill the entire register.  */
      if (GET_CODE (reg) != SUBREG
	  || REG_SIZE (SUBREG_REG (reg)) > REG_SIZE (reg))
	subreg_p = 1;

      reg = SUBREG_REG (reg);
    }

  if (GET_CODE (reg) != REG)
    return;

  /* Global registers are always live, so the code below does not apply
     to them.  */

  regno = REGNO (reg);
  if (regno >= FIRST_PSEUDO_REGISTER || !global_regs[regno])
    {
      if (death)
	{
	  /* If we only set part of the register, then this set does not
	     kill it.  */
	  if (subreg_p)
	    return;

	  /* Try killing this register.  */
	  if (regno < FIRST_PSEUDO_REGISTER)
	    {
	      int j = HARD_REGNO_NREGS (regno, GET_MODE (reg));
	      while (--j >= 0)
		{
		  CLEAR_REGNO_REG_SET (bb_live_regs, regno + j);
		}
	    }
	  else
	    {
	      /* Recompute REG_BASIC_BLOCK as we update all the other
		 dataflow information.  */
	      if (sched_reg_basic_block[regno] == REG_BLOCK_UNKNOWN)
		sched_reg_basic_block[regno] = current_block_num;
	      else if (sched_reg_basic_block[regno] != current_block_num)
		sched_reg_basic_block[regno] = REG_BLOCK_GLOBAL;

	      CLEAR_REGNO_REG_SET (bb_live_regs, regno);
	    }
	}
      else
	{
	  /* Make the register live again.  */
	  if (regno < FIRST_PSEUDO_REGISTER)
	    {
	      int j = HARD_REGNO_NREGS (regno, GET_MODE (reg));
	      while (--j >= 0)
		{
		  SET_REGNO_REG_SET (bb_live_regs, regno + j);
		}
	    }
	  else
	    {
	      SET_REGNO_REG_SET (bb_live_regs, regno);
	    }
	}
    }
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
     const GENERIC_PTR x;
     const GENERIC_PTR y;
{
  rtx tmp = *(rtx *)y;
  rtx tmp2 = *(rtx *)x;
  rtx link;
  int tmp_class, tmp2_class, depend_count1, depend_count2;
  int val, priority_val, spec_val, prob_val, weight_val;


  /* prefer insn with higher priority */
  priority_val = INSN_PRIORITY (tmp2) - INSN_PRIORITY (tmp);
  if (priority_val)
    return priority_val;

  /* prefer an insn with smaller contribution to registers-pressure */
  if (!reload_completed &&
      (weight_val = INSN_REG_WEIGHT (tmp) - INSN_REG_WEIGHT (tmp2)))
    return (weight_val);

  /* some comparison make sense in interblock scheduling only */
  if (INSN_BB (tmp) != INSN_BB (tmp2))
    {
      /* prefer an inblock motion on an interblock motion */
      if ((INSN_BB (tmp2) == target_bb) && (INSN_BB (tmp) != target_bb))
	return 1;
      if ((INSN_BB (tmp) == target_bb) && (INSN_BB (tmp2) != target_bb))
	return -1;

      /* prefer a useful motion on a speculative one */
      if ((spec_val = IS_SPECULATIVE_INSN (tmp) - IS_SPECULATIVE_INSN (tmp2)))
	return (spec_val);

      /* prefer a more probable (speculative) insn */
      prob_val = INSN_PROBABILITY (tmp2) - INSN_PROBABILITY (tmp);
      if (prob_val)
	return (prob_val);
    }

  /* compare insns based on their relation to the last-scheduled-insn */
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
	fprintf (dump, "(b%d) ", INSN_BLOCK (insn));

      fprintf (dump, "queued for %d cycles.\n", n_cycles);
    }

}

/* Return nonzero if PAT is the pattern of an insn which makes a
   register live.  */

HAIFA_INLINE static int
birthing_insn_p (pat)
     rtx pat;
{
  int j;

  if (reload_completed == 1)
    return 0;

  if (GET_CODE (pat) == SET
      && (GET_CODE (SET_DEST (pat)) == REG
	  || (GET_CODE (SET_DEST (pat)) == PARALLEL
	      && GET_MODE (SET_DEST (pat)) == BLKmode)))
    {
      rtx dest = SET_DEST (pat);
      int i;

      /* It would be more accurate to use refers_to_regno_p or
	 reg_mentioned_p to determine when the dest is not live before this
	 insn.  */
      if (GET_CODE (dest) == REG)
	{
	  i = REGNO (dest);
	  if (REGNO_REG_SET_P (bb_live_regs, i))
	    return (REG_N_SETS (i) == 1);
	}
      else
	{
	  for (i = XVECLEN (dest, 0) - 1; i >= 0; i--)
	    {
	      int regno = REGNO (SET_DEST (XVECEXP (dest, 0, i)));
	      if (REGNO_REG_SET_P (bb_live_regs, regno))
		return (REG_N_SETS (regno) == 1);
	    }
	}
      return 0;
    }
  if (GET_CODE (pat) == PARALLEL)
    {
      for (j = 0; j < XVECLEN (pat, 0); j++)
	if (birthing_insn_p (XVECEXP (pat, 0, j)))
	  return 1;
    }
  return 0;
}

/* PREV is an insn that is ready to execute.  Adjust its priority if that
   will help shorten register lifetimes.  */

HAIFA_INLINE static void
adjust_priority (prev)
     rtx prev;
{
  /* Trying to shorten register lives after reload has completed
     is useless and wrong.  It gives inaccurate schedules.  */
  if (reload_completed == 0)
    {
      rtx note;
      int n_deaths = 0;

      /* ??? This code has no effect, because REG_DEAD notes are removed
	 before we ever get here.  */
      for (note = REG_NOTES (prev); note; note = XEXP (note, 1))
	if (REG_NOTE_KIND (note) == REG_DEAD)
	  n_deaths += 1;

      /* Defer scheduling insns which kill registers, since that
	 shortens register lives.  Prefer scheduling insns which
	 make registers live for the same reason.  */
      switch (n_deaths)
	{
	default:
	  INSN_PRIORITY (prev) >>= 3;
	  break;
	case 3:
	  INSN_PRIORITY (prev) >>= 2;
	  break;
	case 2:
	case 1:
	  INSN_PRIORITY (prev) >>= 1;
	  break;
	case 0:
	  if (birthing_insn_p (PATTERN (prev)))
	    {
	      int max = max_priority;

	      if (max > INSN_PRIORITY (prev))
		INSN_PRIORITY (prev) = max;
	    }
	  break;
	}
#ifdef ADJUST_PRIORITY
      ADJUST_PRIORITY (prev);
#endif
    }
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
      fprintf (dump, ";;\t\t--> scheduling insn <<<%d>>> on unit ", INSN_UID (insn));
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
	     check live, exception-free, and issue-delay */
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
	      fprintf (dump, ";;\t\tdependences resolved: insn %d ", INSN_UID (next));

	      if (current_nr_blocks > 1 && INSN_BB (next) != target_bb)
		fprintf (dump, "/b%d ", INSN_BLOCK (next));

	      if (effective_cost <= 1)
		fprintf (dump, "into ready\n");
	      else
		fprintf (dump, "into queue with cost=%d\n", effective_cost);
	    }

	  /* Adjust the priority of NEXT and either put it on the ready
	     list or queue it.  */
	  adjust_priority (next);
	  if (effective_cost <= 1)
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


/* Add a REG_DEAD note for REG to INSN, reusing a REG_DEAD note from the
   dead_notes list.  */

static void
create_reg_dead_note (reg, insn)
     rtx reg, insn;
{
  rtx link;

  /* The number of registers killed after scheduling must be the same as the
     number of registers killed before scheduling.  The number of REG_DEAD
     notes may not be conserved, i.e. two SImode hard register REG_DEAD notes
     might become one DImode hard register REG_DEAD note, but the number of
     registers killed will be conserved.

     We carefully remove REG_DEAD notes from the dead_notes list, so that
     there will be none left at the end.  If we run out early, then there
     is a bug somewhere in flow, combine and/or sched.  */

  if (dead_notes == 0)
    {
      if (current_nr_blocks <= 1)
	abort ();
      else
	link = alloc_EXPR_LIST (REG_DEAD, NULL_RTX, NULL_RTX);
    }
  else
    {
      /* Number of regs killed by REG.  */
      int regs_killed = (REGNO (reg) >= FIRST_PSEUDO_REGISTER ? 1
			 : HARD_REGNO_NREGS (REGNO (reg), GET_MODE (reg)));
      /* Number of regs killed by REG_DEAD notes taken off the list.  */
      int reg_note_regs;

      link = dead_notes;
      reg_note_regs = (REGNO (XEXP (link, 0)) >= FIRST_PSEUDO_REGISTER ? 1
		       : HARD_REGNO_NREGS (REGNO (XEXP (link, 0)),
					   GET_MODE (XEXP (link, 0))));
      while (reg_note_regs < regs_killed)
	{
	  link = XEXP (link, 1);

	  /* LINK might be zero if we killed more registers after scheduling
	     than before, and the last hard register we kill is actually
	     multiple hard regs. 

	     This is normal for interblock scheduling, so deal with it in
	     that case, else abort.  */
	  if (link == NULL_RTX && current_nr_blocks <= 1)
	    abort ();
	  else if (link == NULL_RTX)
	    link = alloc_EXPR_LIST (REG_DEAD, gen_rtx_REG (word_mode, 0),
				    NULL_RTX);
	     
	  reg_note_regs += (REGNO (XEXP (link, 0)) >= FIRST_PSEUDO_REGISTER ? 1
			    : HARD_REGNO_NREGS (REGNO (XEXP (link, 0)),
						GET_MODE (XEXP (link, 0))));
	}
      dead_notes = XEXP (link, 1);

      /* If we took too many regs kills off, put the extra ones back.  */
      while (reg_note_regs > regs_killed)
	{
	  rtx temp_reg, temp_link;

	  temp_reg = gen_rtx_REG (word_mode, 0);
	  temp_link = alloc_EXPR_LIST (REG_DEAD, temp_reg, dead_notes);
	  dead_notes = temp_link;
	  reg_note_regs--;
	}
    }

  XEXP (link, 0) = reg;
  XEXP (link, 1) = REG_NOTES (insn);
  REG_NOTES (insn) = link;
}

/* Subroutine on attach_deaths_insn--handles the recursive search
   through INSN.  If SET_P is true, then x is being modified by the insn.  */

static void
attach_deaths (x, insn, set_p)
     rtx x;
     rtx insn;
     int set_p;
{
  register int i;
  register int j;
  register enum rtx_code code;
  register char *fmt;

  if (x == 0)
    return;

  code = GET_CODE (x);

  switch (code)
    {
    case CONST_INT:
    case CONST_DOUBLE:
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST:
    case CODE_LABEL:
    case PC:
    case CC0:
      /* Get rid of the easy cases first.  */
      return;

    case REG:
      {
	/* If the register dies in this insn, queue that note, and mark
	   this register as needing to die.  */
	/* This code is very similar to mark_used_1 (if set_p is false)
	   and mark_set_1 (if set_p is true) in flow.c.  */

	register int regno;
	int some_needed;
	int all_needed;

	if (set_p)
	  return;

	regno = REGNO (x);
	all_needed = some_needed = REGNO_REG_SET_P (old_live_regs, regno);
	if (regno < FIRST_PSEUDO_REGISTER)
	  {
	    int n;

	    n = HARD_REGNO_NREGS (regno, GET_MODE (x));
	    while (--n > 0)
	      {
		int needed = (REGNO_REG_SET_P (old_live_regs, regno + n));
		some_needed |= needed;
		all_needed &= needed;
	      }
	  }

	/* If it wasn't live before we started, then add a REG_DEAD note.
	   We must check the previous lifetime info not the current info,
	   because we may have to execute this code several times, e.g.
	   once for a clobber (which doesn't add a note) and later
	   for a use (which does add a note).

	   Always make the register live.  We must do this even if it was
	   live before, because this may be an insn which sets and uses
	   the same register, in which case the register has already been
	   killed, so we must make it live again.

	   Global registers are always live, and should never have a REG_DEAD
	   note added for them, so none of the code below applies to them.  */

	if (regno >= FIRST_PSEUDO_REGISTER || ! global_regs[regno])
	  {
	    /* Never add REG_DEAD notes for STACK_POINTER_REGNUM
	       since it's always considered to be live.  Similarly
	       for FRAME_POINTER_REGNUM if a frame pointer is needed
	       and for ARG_POINTER_REGNUM if it is fixed.  */
	    if (! (regno == FRAME_POINTER_REGNUM
		   && (! reload_completed || frame_pointer_needed))
#if HARD_FRAME_POINTER_REGNUM != FRAME_POINTER_REGNUM
		&& ! (regno == HARD_FRAME_POINTER_REGNUM
		      && (! reload_completed || frame_pointer_needed))
#endif
#if ARG_POINTER_REGNUM != FRAME_POINTER_REGNUM
		&& ! (regno == ARG_POINTER_REGNUM && fixed_regs[regno])
#endif
		&& regno != STACK_POINTER_REGNUM)
	      {
		if (! all_needed && ! dead_or_set_p (insn, x))
		  {
		    /* Check for the case where the register dying partially
		       overlaps the register set by this insn.  */
		    if (regno < FIRST_PSEUDO_REGISTER
			&& HARD_REGNO_NREGS (regno, GET_MODE (x)) > 1)
		      {
			int n = HARD_REGNO_NREGS (regno, GET_MODE (x));
			while (--n >= 0)
			  some_needed |= dead_or_set_regno_p (insn, regno + n);
		      }

		    /* If none of the words in X is needed, make a REG_DEAD
		       note.  Otherwise, we must make partial REG_DEAD
		       notes.  */
		    if (! some_needed)
		      create_reg_dead_note (x, insn);
		    else
		      {
			int i;

			/* Don't make a REG_DEAD note for a part of a
			   register that is set in the insn.  */
			for (i = HARD_REGNO_NREGS (regno, GET_MODE (x)) - 1;
			     i >= 0; i--)
			  if (! REGNO_REG_SET_P (old_live_regs, regno+i)
			      && ! dead_or_set_regno_p (insn, regno + i))
			    create_reg_dead_note (gen_rtx_REG (reg_raw_mode[regno + i],
							       regno + i),
						  insn);
		      }
		  }
	      }

	    if (regno < FIRST_PSEUDO_REGISTER)
	      {
		int j = HARD_REGNO_NREGS (regno, GET_MODE (x));
		while (--j >= 0)
		  {
		    SET_REGNO_REG_SET (bb_live_regs, regno + j);
		  }
	      }
	    else
	      {
		/* Recompute REG_BASIC_BLOCK as we update all the other
		   dataflow information.  */
		if (sched_reg_basic_block[regno] == REG_BLOCK_UNKNOWN)
		  sched_reg_basic_block[regno] = current_block_num;
		else if (sched_reg_basic_block[regno] != current_block_num)
		  sched_reg_basic_block[regno] = REG_BLOCK_GLOBAL;

		SET_REGNO_REG_SET (bb_live_regs, regno);
	      }
	  }
	return;
      }

    case MEM:
      /* Handle tail-recursive case.  */
      attach_deaths (XEXP (x, 0), insn, 0);
      return;

    case SUBREG:
      attach_deaths (SUBREG_REG (x), insn,
		     set_p && ((GET_MODE_SIZE (GET_MODE (SUBREG_REG (x)))
				<= UNITS_PER_WORD)
			       || (GET_MODE_SIZE (GET_MODE (SUBREG_REG (x)))
				   == GET_MODE_SIZE (GET_MODE ((x))))));
      return;

    case STRICT_LOW_PART:
      attach_deaths (XEXP (x, 0), insn, 0);
      return;

    case ZERO_EXTRACT:
    case SIGN_EXTRACT:
      attach_deaths (XEXP (x, 0), insn, 0);
      attach_deaths (XEXP (x, 1), insn, 0);
      attach_deaths (XEXP (x, 2), insn, 0);
      return;

    case PARALLEL:
      if (set_p
	  && GET_MODE (x) == BLKmode)
	{
	  for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
	    attach_deaths (SET_DEST (XVECEXP (x, 0, i)), insn, 1);
	  return;
	}

      /* fallthrough */
    default:
      /* Other cases: walk the insn.  */
      fmt = GET_RTX_FORMAT (code);
      for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
	{
	  if (fmt[i] == 'e')
	    attach_deaths (XEXP (x, i), insn, 0);
	  else if (fmt[i] == 'E')
	    for (j = 0; j < XVECLEN (x, i); j++)
	      attach_deaths (XVECEXP (x, i, j), insn, 0);
	}
    }
}

/* After INSN has executed, add register death notes for each register
   that is dead after INSN.  */

static void
attach_deaths_insn (insn)
     rtx insn;
{
  rtx x = PATTERN (insn);
  register RTX_CODE code = GET_CODE (x);
  rtx link;

  if (code == SET)
    {
      attach_deaths (SET_SRC (x), insn, 0);

      /* A register might die here even if it is the destination, e.g.
         it is the target of a volatile read and is otherwise unused.
         Hence we must always call attach_deaths for the SET_DEST.  */
      attach_deaths (SET_DEST (x), insn, 1);
    }
  else if (code == PARALLEL)
    {
      register int i;
      for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
	{
	  code = GET_CODE (XVECEXP (x, 0, i));
	  if (code == SET)
	    {
	      attach_deaths (SET_SRC (XVECEXP (x, 0, i)), insn, 0);

	      attach_deaths (SET_DEST (XVECEXP (x, 0, i)), insn, 1);
	    }
	  /* Flow does not add REG_DEAD notes to registers that die in
	     clobbers, so we can't either.  */
	  else if (code != CLOBBER)
	    attach_deaths (XVECEXP (x, 0, i), insn, 0);
	}
    }
  /* If this is a CLOBBER, only add REG_DEAD notes to registers inside a
     MEM being clobbered, just like flow.  */
  else if (code == CLOBBER && GET_CODE (XEXP (x, 0)) == MEM)
    attach_deaths (XEXP (XEXP (x, 0), 0), insn, 0);
  /* Otherwise don't add a death note to things being clobbered.  */
  else if (code != CLOBBER)
    attach_deaths (x, insn, 0);

  /* Make death notes for things used in the called function.  */
  if (GET_CODE (insn) == CALL_INSN)
    for (link = CALL_INSN_FUNCTION_USAGE (insn); link; link = XEXP (link, 1))
      attach_deaths (XEXP (XEXP (link, 0), 0), insn,
		     GET_CODE (XEXP (link, 0)) == CLOBBER);
}

/* functions for handlnig of notes */

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

      /* Don't save away NOTE_INSN_SETJMPs, because they must remain
         immediately after the call they follow.  We use a fake
         (REG_DEAD (const_int -1)) note to remember them.
         Likewise with NOTE_INSN_{LOOP,EHREGION}_{BEG, END}.  */
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
get_block_head_tail (bb, headp, tailp)
     int bb;
     rtx *headp;
     rtx *tailp;
{

  rtx head;
  rtx tail;
  int b;

  b = BB_TO_BLOCK (bb);

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

  get_block_head_tail (bb, &head, &tail);

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

  get_block_head_tail (bb, &head, &tail);
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

/* Constructor for `sometimes' data structure.  */

static int
new_sometimes_live (regs_sometimes_live, regno, sometimes_max)
     struct sometimes *regs_sometimes_live;
     int regno;
     int sometimes_max;
{
  register struct sometimes *p;

  /* There should never be a register greater than max_regno here.  If there
     is, it means that a define_split has created a new pseudo reg.  This
     is not allowed, since there will not be flow info available for any
     new register, so catch the error here.  */
  if (regno >= max_regno)
    abort ();

  p = &regs_sometimes_live[sometimes_max];
  p->regno = regno;
  p->live_length = 0;
  p->calls_crossed = 0;
  sometimes_max++;
  return sometimes_max;
}

/* Count lengths of all regs we are currently tracking,
   and find new registers no longer live.  */

static void
finish_sometimes_live (regs_sometimes_live, sometimes_max)
     struct sometimes *regs_sometimes_live;
     int sometimes_max;
{
  int i;

  for (i = 0; i < sometimes_max; i++)
    {
      register struct sometimes *p = &regs_sometimes_live[i];
      int regno = p->regno;

      sched_reg_live_length[regno] += p->live_length;
      sched_reg_n_calls_crossed[regno] += p->calls_crossed;
    }
}

/* functions for computation of registers live/usage info */

/* It is assumed that prior to scheduling BASIC_BLOCK (b)->global_live_at_start
   contains the registers that are alive at the entry to b.

   Two passes follow: The first pass is performed before the scheduling
   of a region. It scans each block of the region forward, computing
   the set of registers alive at the end of the basic block and
   discard REG_DEAD notes (done by find_pre_sched_live ()).

   The second path is invoked after scheduling all region blocks.
   It scans each block of the region backward, a block being traversed
   only after its succesors in the region. When the set of registers
   live at the end of a basic block may be changed by the scheduling
   (this may happen for multiple blocks region), it is computed as
   the union of the registers live at the start of its succesors.
   The last-use information is updated by inserting REG_DEAD notes.
   (done by find_post_sched_live ()) */

/* Scan all the insns to be scheduled, removing register death notes.
   Register death notes end up in DEAD_NOTES.
   Recreate the register life information for the end of this basic
   block.  */

static void
find_pre_sched_live (bb)
     int bb;
{
  rtx insn, next_tail, head, tail;
  int b = BB_TO_BLOCK (bb);

  get_block_head_tail (bb, &head, &tail);
  COPY_REG_SET (bb_live_regs, BASIC_BLOCK (b)->global_live_at_start);
  next_tail = NEXT_INSN (tail);

  for (insn = head; insn != next_tail; insn = NEXT_INSN (insn))
    {
      rtx prev, next, link;
      int reg_weight = 0;

      /* Handle register life information.  */
      if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	{
	  /* See if the register gets born here.  */
	  /* We must check for registers being born before we check for
	     registers dying.  It is possible for a register to be born and
	     die in the same insn, e.g. reading from a volatile memory
	     location into an otherwise unused register.  Such a register
	     must be marked as dead after this insn.  */
	  if (GET_CODE (PATTERN (insn)) == SET
	      || GET_CODE (PATTERN (insn)) == CLOBBER)
	    {
	      sched_note_set (PATTERN (insn), 0);
	      reg_weight++;
	    }

	  else if (GET_CODE (PATTERN (insn)) == PARALLEL)
	    {
	      int j;
	      for (j = XVECLEN (PATTERN (insn), 0) - 1; j >= 0; j--)
		if (GET_CODE (XVECEXP (PATTERN (insn), 0, j)) == SET
		    || GET_CODE (XVECEXP (PATTERN (insn), 0, j)) == CLOBBER)
		  {
		    sched_note_set (XVECEXP (PATTERN (insn), 0, j), 0);
		    reg_weight++;
		  }

	      /* ??? This code is obsolete and should be deleted.  It
	         is harmless though, so we will leave it in for now.  */
	      for (j = XVECLEN (PATTERN (insn), 0) - 1; j >= 0; j--)
		if (GET_CODE (XVECEXP (PATTERN (insn), 0, j)) == USE)
		  sched_note_set (XVECEXP (PATTERN (insn), 0, j), 0);
	    }

	  /* Each call cobbers (makes live) all call-clobbered regs
	     that are not global or fixed.  Note that the function-value
	     reg is a call_clobbered reg.  */
	  if (GET_CODE (insn) == CALL_INSN)
	    {
	      int j;
	      for (j = 0; j < FIRST_PSEUDO_REGISTER; j++)
		if (call_used_regs[j] && !global_regs[j]
		    && ! fixed_regs[j])
		  {
		    SET_REGNO_REG_SET (bb_live_regs, j);
		  }
	    }

	  /* Need to know what registers this insn kills.  */
	  for (prev = 0, link = REG_NOTES (insn); link; link = next)
	    {
	      next = XEXP (link, 1);
	      if ((REG_NOTE_KIND (link) == REG_DEAD
		   || REG_NOTE_KIND (link) == REG_UNUSED)
	      /* Verify that the REG_NOTE has a valid value.  */
		  && GET_CODE (XEXP (link, 0)) == REG)
		{
		  register int regno = REGNO (XEXP (link, 0));

		  reg_weight--;

		  /* Only unlink REG_DEAD notes; leave REG_UNUSED notes
		     alone.  */
		  if (REG_NOTE_KIND (link) == REG_DEAD)
		    {
		      if (prev)
			XEXP (prev, 1) = next;
		      else
			REG_NOTES (insn) = next;
		      XEXP (link, 1) = dead_notes;
		      dead_notes = link;
		    }
		  else
		    prev = link;

		  if (regno < FIRST_PSEUDO_REGISTER)
		    {
		      int j = HARD_REGNO_NREGS (regno,
						GET_MODE (XEXP (link, 0)));
		      while (--j >= 0)
			{
			  CLEAR_REGNO_REG_SET (bb_live_regs, regno+j);
			}
		    }
		  else
		    {
		      CLEAR_REGNO_REG_SET (bb_live_regs, regno);
		    }
		}
	      else
		prev = link;
	    }
	}

      INSN_REG_WEIGHT (insn) = reg_weight;
    }
}

/* Update register life and usage information for block bb
   after scheduling.  Put register dead notes back in the code.  */

static void
find_post_sched_live (bb)
     int bb;
{
  int sometimes_max;
  int j, i;
  int b;
  rtx insn;
  rtx head, tail, prev_head, next_tail;

  register struct sometimes *regs_sometimes_live;

  b = BB_TO_BLOCK (bb);

  /* compute live regs at the end of bb as a function of its successors.  */
  if (current_nr_blocks > 1)
    {
      int e;
      int first_edge;

      first_edge = e = OUT_EDGES (b);
      CLEAR_REG_SET (bb_live_regs);

      if (e)
	do
	  {
	    int b_succ;

	    b_succ = TO_BLOCK (e);
	    IOR_REG_SET (bb_live_regs,
			 BASIC_BLOCK (b_succ)->global_live_at_start);
	    e = NEXT_OUT (e);
	  }
	while (e != first_edge);
    }

  get_block_head_tail (bb, &head, &tail);
  next_tail = NEXT_INSN (tail);
  prev_head = PREV_INSN (head);

  EXECUTE_IF_SET_IN_REG_SET (bb_live_regs, FIRST_PSEUDO_REGISTER, i,
			     {
			       sched_reg_basic_block[i] = REG_BLOCK_GLOBAL;
			     });

  /* if the block is empty, same regs are alive at its end and its start.
     since this is not guaranteed after interblock scheduling, make sure they
     are truly identical.  */
  if (NEXT_INSN (prev_head) == tail
      && (GET_RTX_CLASS (GET_CODE (tail)) != 'i'))
    {
      if (current_nr_blocks > 1)
	COPY_REG_SET (BASIC_BLOCK (b)->global_live_at_start, bb_live_regs);

      return;
    }

  b = BB_TO_BLOCK (bb);
  current_block_num = b;

  /* Keep track of register lives.  */
  old_live_regs = ALLOCA_REG_SET ();
  regs_sometimes_live
    = (struct sometimes *) alloca (max_regno * sizeof (struct sometimes));
  sometimes_max = 0;

  /* initiate "sometimes" data, starting with registers live at end */
  sometimes_max = 0;
  COPY_REG_SET (old_live_regs, bb_live_regs);
  EXECUTE_IF_SET_IN_REG_SET (bb_live_regs, 0, j,
			     {
			       sometimes_max
				 = new_sometimes_live (regs_sometimes_live,
						       j, sometimes_max);
			     });

  /* scan insns back, computing regs live info */
  for (insn = tail; insn != prev_head; insn = PREV_INSN (insn))
    {
      /* First we kill registers set by this insn, and then we
         make registers used by this insn live.  This is the opposite
         order used above because we are traversing the instructions
         backwards.  */

      /* Strictly speaking, we should scan REG_UNUSED notes and make
         every register mentioned there live, however, we will just
         kill them again immediately below, so there doesn't seem to
         be any reason why we bother to do this.  */

      /* See if this is the last notice we must take of a register.  */
      if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
	continue;

      if (GET_CODE (PATTERN (insn)) == SET
	  || GET_CODE (PATTERN (insn)) == CLOBBER)
	sched_note_set (PATTERN (insn), 1);
      else if (GET_CODE (PATTERN (insn)) == PARALLEL)
	{
	  for (j = XVECLEN (PATTERN (insn), 0) - 1; j >= 0; j--)
	    if (GET_CODE (XVECEXP (PATTERN (insn), 0, j)) == SET
		|| GET_CODE (XVECEXP (PATTERN (insn), 0, j)) == CLOBBER)
	      sched_note_set (XVECEXP (PATTERN (insn), 0, j), 1);
	}

      /* This code keeps life analysis information up to date.  */
      if (GET_CODE (insn) == CALL_INSN)
	{
	  register struct sometimes *p;

	  /* A call kills all call used registers that are not
	     global or fixed, except for those mentioned in the call
	     pattern which will be made live again later.  */
	  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	    if (call_used_regs[i] && ! global_regs[i]
		&& ! fixed_regs[i])
	      {
		CLEAR_REGNO_REG_SET (bb_live_regs, i);
	      }

	  /* Regs live at the time of a call instruction must not
	     go in a register clobbered by calls.  Record this for
	     all regs now live.  Note that insns which are born or
	     die in a call do not cross a call, so this must be done
	     after the killings (above) and before the births
	     (below).  */
	  p = regs_sometimes_live;
	  for (i = 0; i < sometimes_max; i++, p++)
	    if (REGNO_REG_SET_P (bb_live_regs, p->regno))
	      p->calls_crossed += 1;
	}

      /* Make every register used live, and add REG_DEAD notes for
         registers which were not live before we started.  */
      attach_deaths_insn (insn);

      /* Find registers now made live by that instruction.  */
      EXECUTE_IF_AND_COMPL_IN_REG_SET (bb_live_regs, old_live_regs, 0, j,
				 {
				   sometimes_max
				     = new_sometimes_live (regs_sometimes_live,
							   j, sometimes_max);
				 });
      IOR_REG_SET (old_live_regs, bb_live_regs);

      /* Count lengths of all regs we are worrying about now,
         and handle registers no longer live.  */

      for (i = 0; i < sometimes_max; i++)
	{
	  register struct sometimes *p = &regs_sometimes_live[i];
	  int regno = p->regno;

	  p->live_length += 1;

	  if (!REGNO_REG_SET_P (bb_live_regs, regno))
	    {
	      /* This is the end of one of this register's lifetime
	         segments.  Save the lifetime info collected so far,
	         and clear its bit in the old_live_regs entry.  */
	      sched_reg_live_length[regno] += p->live_length;
	      sched_reg_n_calls_crossed[regno] += p->calls_crossed;
	      CLEAR_REGNO_REG_SET (old_live_regs, p->regno);

	      /* Delete the reg_sometimes_live entry for this reg by
	         copying the last entry over top of it.  */
	      *p = regs_sometimes_live[--sometimes_max];
	      /* ...and decrement i so that this newly copied entry
	         will be processed.  */
	      i--;
	    }
	}
    }

  finish_sometimes_live (regs_sometimes_live, sometimes_max);

  /* In interblock scheduling, global_live_at_start may have changed.  */
  if (current_nr_blocks > 1)
    COPY_REG_SET (BASIC_BLOCK (b)->global_live_at_start, bb_live_regs);


  FREE_REG_SET (old_live_regs);
}				/* find_post_sched_live */

/* After scheduling the subroutine, restore information about uses of
   registers.  */

static void
update_reg_usage ()
{
  int regno;

  if (n_basic_blocks > 0)
    EXECUTE_IF_SET_IN_REG_SET (bb_live_regs, FIRST_PSEUDO_REGISTER, regno,
			       {
				 sched_reg_basic_block[regno]
				   = REG_BLOCK_GLOBAL;
			       });

  for (regno = 0; regno < max_regno; regno++)
    if (sched_reg_live_length[regno])
      {
	if (sched_verbose)
	  {
	    if (REG_LIVE_LENGTH (regno) > sched_reg_live_length[regno])
	      fprintf (dump,
		       ";; register %d life shortened from %d to %d\n",
		       regno, REG_LIVE_LENGTH (regno),
		       sched_reg_live_length[regno]);
	    /* Negative values are special; don't overwrite the current
	       reg_live_length value if it is negative.  */
	    else if (REG_LIVE_LENGTH (regno) < sched_reg_live_length[regno]
		     && REG_LIVE_LENGTH (regno) >= 0)
	      fprintf (dump,
		       ";; register %d life extended from %d to %d\n",
		       regno, REG_LIVE_LENGTH (regno),
		       sched_reg_live_length[regno]);

	    if (!REG_N_CALLS_CROSSED (regno)
		&& sched_reg_n_calls_crossed[regno])
	      fprintf (dump,
		       ";; register %d now crosses calls\n", regno);
	    else if (REG_N_CALLS_CROSSED (regno)
		     && !sched_reg_n_calls_crossed[regno]
		     && REG_BASIC_BLOCK (regno) != REG_BLOCK_GLOBAL)
	      fprintf (dump,
		       ";; register %d no longer crosses calls\n", regno);

	    if (REG_BASIC_BLOCK (regno) != sched_reg_basic_block[regno]
		&& sched_reg_basic_block[regno] != REG_BLOCK_UNKNOWN
		&& REG_BASIC_BLOCK(regno) != REG_BLOCK_UNKNOWN)
	      fprintf (dump,
		       ";; register %d changed basic block from %d to %d\n",
			regno, REG_BASIC_BLOCK(regno),
			sched_reg_basic_block[regno]);

	  }
	/* Negative values are special; don't overwrite the current
	   reg_live_length value if it is negative.  */
	if (REG_LIVE_LENGTH (regno) >= 0)
	  REG_LIVE_LENGTH (regno) = sched_reg_live_length[regno];

	if (sched_reg_basic_block[regno] != REG_BLOCK_UNKNOWN
	    && REG_BASIC_BLOCK(regno) != REG_BLOCK_UNKNOWN)
	  REG_BASIC_BLOCK(regno) = sched_reg_basic_block[regno];

	/* We can't change the value of reg_n_calls_crossed to zero for
	   pseudos which are live in more than one block.

	   This is because combine might have made an optimization which
	   invalidated global_live_at_start and reg_n_calls_crossed,
	   but it does not update them.  If we update reg_n_calls_crossed
	   here, the two variables are now inconsistent, and this might
	   confuse the caller-save code into saving a register that doesn't
	   need to be saved.  This is only a problem when we zero calls
	   crossed for a pseudo live in multiple basic blocks.

	   Alternatively, we could try to correctly update basic block live
	   at start here in sched, but that seems complicated.

	   Note: it is possible that a global register became local, as result
	   of interblock motion, but will remain marked as a global register.  */
	if (sched_reg_n_calls_crossed[regno]
	    || REG_BASIC_BLOCK (regno) != REG_BLOCK_GLOBAL)
	  REG_N_CALLS_CROSSED (regno) = sched_reg_n_calls_crossed[regno];

      }
}

/* Scheduling clock, modified in schedule_block() and queue_to_ready () */
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
	fprintf (dump, "(b%d) ", INSN_BLOCK (insn));

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
		    fprintf (dump, "(b%d) ", INSN_BLOCK (insn));

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

/* Print the ready list for debugging purposes. Callable from debugger.  */

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
	fprintf (dump, "/b%d", INSN_BLOCK (ready[i]));
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

/* Finds units that are in use in this fuction. Required only
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

/* Return the length of the visualization table */

static int
get_visual_tbl_length ()
{
  int unit, i;
  int n, n1;
  char *s;

  /* compute length of one field in line */
  s = (char *) alloca (INSN_LEN + 5);
  sprintf (s, "  %33s", "uname");
  n1 = strlen (s);

  /* compute length of one line */
  n = strlen (";; ");
  n += n1;
  for (unit = 0; unit < FUNCTION_UNITS_SIZE; unit++)
    if (function_units[unit].bitmask & target_units)
      for (i = 0; i < function_units[unit].multiplicity; i++)
	n += n1;
  n += n1;
  n += strlen ("\n") + 2;

  /* compute length of visualization string */
  return (MAX_VISUAL_LINES * n);
}

/* Init block visualization debugging info */

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
     char *str;
{
  char *end = buf + BUF_LEN - 2;	/* leave room for null */
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

/* This recognizes rtx, I classified as expressions. These are always */
/* represent some action on values or results of other expression, */
/* that may be stored in objects representing values.  */

static void
print_exp (buf, x, verbose)
     char *buf;
     rtx x;
     int verbose;
{
  char tmp[BUF_LEN];
  char *st[4];
  char *cur = buf;
  char *fun = (char *)0;
  char *sep;
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
      /* if (verbose) debug_rtx (x); */
      st[0] = GET_RTX_NAME (GET_CODE (x));
      break;
    }

  /* Print this as a function? */
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

/* Prints rtxes, i customly classified as values. They're constants, */
/* registers, labels, symbols and memory accesses.  */

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

/* The next step in insn detalization, its pattern recognition */

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
   properly in human readable form, resembling assembler mnemonics.  */
/* For every insn it prints its UID and BB the insn belongs */
/* too. (probably the last "option" should be extended somehow, since */
/* it depends now on sched.c inner variables ...) */

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

/* Print visualization debugging info */

static void
print_block_visualization (b, s)
     int b;
     char *s;
{
  int unit, i;

  /* print header */
  fprintf (dump, "\n;;   ==================== scheduling visualization for block %d %s \n", b, s);

  /* Print names of units */
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

  /* Print insns in each cycle */
  fprintf (dump, "%s\n", visual_tbl);
}

/* Print insns in the 'no_unit' column of visualization */

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

  /* if no more room, split table into two */
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

	  /* print insns that still keep the unit busy */
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

  /* print insns that are not assigned to any unit */
  for (i = 0; i < n_vis_no_unit; i++)
    sprintf (visual_tbl + strlen (visual_tbl), "  %-8d",
	     INSN_UID (vis_no_unit[i]));
  n_vis_no_unit = 0;

  sprintf (visual_tbl + strlen (visual_tbl), "\n");
}

/* Print stalled cycles */

static void
visualize_stall_cycles (b, stalls)
     int b, stalls;
{
  int i;

  /* if no more room, split table into two */
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

/* move_insn1: Remove INSN from insn chain, and link it after LAST insn */

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

/* Search INSN for fake REG_DEAD note pairs for NOTE_INSN_SETJMP,
   NOTE_INSN_{LOOP,EHREGION}_{BEG,END}; and convert them back into
   NOTEs.  The REG_DEAD note following first one is contains the saved
   value for NOTE_BLOCK_NUMBER which is useful for
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
      if (REG_NOTE_KIND (note) == REG_DEAD
	  && GET_CODE (XEXP (note, 0)) == CONST_INT)
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
	      last = emit_note_before (INTVAL (XEXP (note, 0)), last);
	      remove_note (insn, note);
	      note = XEXP (note, 1);
	      NOTE_BLOCK_NUMBER (last) = INTVAL (XEXP (note, 0));
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
  int i;
  int n_ready = 0;
  int can_issue_more;

  /* flow block of this bb */
  int b = BB_TO_BLOCK (bb);

  /* target_n_insns == number of insns in b before scheduling starts.
     sched_target_n_insns == how many of b's insns were scheduled.
     sched_n_insns == how many insns were scheduled in b */
  int target_n_insns = 0;
  int sched_target_n_insns = 0;
  int sched_n_insns = 0;

#define NEED_NOTHING	0
#define NEED_HEAD	1
#define NEED_TAIL	2
  int new_needs;

  /* head/tail info for this block */
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
  get_block_head_tail (bb, &head, &tail);

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
	if (REG_NOTE_KIND (note) == REG_DEAD
	    && GET_CODE (XEXP (note, 0)) == CONST_INT)
	  remove_note (head, note);
    }

  next_tail = NEXT_INSN (tail);
  prev_head = PREV_INSN (head);

  /* If the only insn left is a NOTE or a CODE_LABEL, then there is no need
     to schedule this block.  */
  if (head == tail
      && (GET_RTX_CLASS (GET_CODE (head)) != 'i'))
    return (sched_n_insns);

  /* debug info */
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

  /* remove remaining note insns from the block, save them in
     note_list.  These notes are restored at the end of
     schedule_block ().  */
  note_list = 0;
  rm_other_notes (head, tail);

  target_bb = bb;

  /* prepare current target block info */
  if (current_nr_blocks > 1)
    {
      candidate_table = (candidate *) alloca (current_nr_blocks * sizeof (candidate));

      bblst_last = 0;
      /* ??? It is not clear why bblst_size is computed this way.  The original
	 number was clearly too small as it resulted in compiler failures.
	 Multiplying by the original number by 2 (to account for update_bbs
	 members) seems to be a reasonable solution.  */
      /* ??? Or perhaps there is a bug somewhere else in this file?  */
      bblst_size = (current_nr_blocks - bb) * rgn_nr_edges * 2;
      bblst_table = (int *) alloca (bblst_size * sizeof (int));

      bitlst_table_last = 0;
      bitlst_table_size = rgn_nr_edges;
      bitlst_table = (int *) alloca (rgn_nr_edges * sizeof (int));

      compute_trg_info (bb);
    }

  clear_units ();

  /* Allocate the ready list */
  ready = (rtx *) alloca ((rgn_n_insns + 1) * sizeof (rtx));

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

	get_block_head_tail (bb_src, &head, &tail);
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

		next = NEXT_INSN (insn);
		if (INSN_DEP_COUNT (insn) == 0
		    && (SCHED_GROUP_P (next) == 0
			|| GET_RTX_CLASS (GET_CODE (next)) != 'i'))
		  ready[n_ready++] = insn;
	      }
	  }
      }

#ifdef MD_SCHED_INIT
  MD_SCHED_INIT (dump, sched_verbose);
#endif

  /* no insns scheduled in this block yet */
  last_scheduled_insn = 0;

  /* Sort the ready list */
  SCHED_SORT (ready, n_ready);
#ifdef MD_SCHED_REORDER
  MD_SCHED_REORDER (dump, sched_verbose, ready, n_ready);
#endif

  if (sched_verbose >= 2)
    {
      fprintf (dump, ";;\t\tReady list initially:             ");
      debug_ready_list (ready, n_ready);
    }

  /* Q_SIZE is the total number of insns in the queue.  */
  q_ptr = 0;
  q_size = 0;
  clock_var = 0;
  last_clock_var = 0;
  bzero ((char *) insn_queue, sizeof (insn_queue));

  /* We start inserting insns after PREV_HEAD.  */
  last = prev_head;

  /* Initialize INSN_QUEUE, LIST and NEW_NEEDS.  */
  new_needs = (NEXT_INSN (prev_head) == BLOCK_HEAD (b)
	       ? NEED_HEAD : NEED_NOTHING);
  if (PREV_INSN (next_tail) == BLOCK_END (b))
    new_needs |= NEED_TAIL;

  /* loop until all the insns in BB are scheduled.  */
  while (sched_target_n_insns < target_n_insns)
    {
      int b1;

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

      /* Sort the ready list.  */
      SCHED_SORT (ready, n_ready);
#ifdef MD_SCHED_REORDER
      MD_SCHED_REORDER (dump, sched_verbose, ready, n_ready);
#endif

      if (sched_verbose)
	{
	  fprintf (dump, "\n;;\tReady list (t =%3d):  ", clock_var);
	  debug_ready_list (ready, n_ready);
	}

      /* Issue insns from ready list.
         It is important to count down from n_ready, because n_ready may change
         as insns are issued.  */
      can_issue_more = issue_rate;
      for (i = n_ready - 1; i >= 0 && can_issue_more; i--)
	{
	  rtx insn = ready[i];
	  int cost = actual_hazard (insn_unit (insn), insn, clock_var, 0);

	  if (cost > 1)
	    {
	      queue_insn (insn, cost);
	      ready[i] = ready[--n_ready];	/* remove insn from ready list */
	    }
	  else if (cost == 0)
	    {
	      /* an interblock motion? */
	      if (INSN_BB (insn) != target_bb)
		{
		  rtx temp;

		  if (IS_SPECULATIVE_INSN (insn))
		    {

		      if (!check_live (insn, INSN_BB (insn)))
			{
			  /* speculative motion, live check failed, remove
			     insn from ready list */
			  ready[i] = ready[--n_ready];
			  continue;
			}
		      update_live (insn, INSN_BB (insn));

		      /* for speculative load, mark insns fed by it.  */
		      if (IS_LOAD_INSN (insn) || FED_BY_SPEC_LOAD (insn))
			set_spec_fed (insn);

		      nr_spec++;
		    }
		  nr_inter++;

		  temp = insn;
		  while (SCHED_GROUP_P (temp))
		    temp = PREV_INSN (temp);

		  /* Update source block boundaries.   */
		  b1 = INSN_BLOCK (temp);
		  if (temp == BLOCK_HEAD (b1)
		      && insn == BLOCK_END (b1))
		    {
		      /* We moved all the insns in the basic block.
			 Emit a note after the last insn and update the
			 begin/end boundaries to point to the note.  */
		      emit_note_after (NOTE_INSN_DELETED, insn);
		      BLOCK_END (b1) = NEXT_INSN (insn);
		      BLOCK_HEAD (b1) = NEXT_INSN (insn);
		    }
		  else if (insn == BLOCK_END (b1))
		    {
		      /* We took insns from the end of the basic block,
			 so update the end of block boundary so that it
			 points to the first insn we did not move.  */
		      BLOCK_END (b1) = PREV_INSN (temp);
		    }
		  else if (temp == BLOCK_HEAD (b1))
		    {
		      /* We took insns from the start of the basic block,
			 so update the start of block boundary so that
			 it points to the first insn we did not move.  */
		      BLOCK_HEAD (b1) = NEXT_INSN (insn);
		    }
		}
	      else
		{
		  /* in block motion */
		  sched_target_n_insns++;
		}

	      last_scheduled_insn = insn;
	      last = move_insn (insn, last);
	      sched_n_insns++;

#ifdef MD_SCHED_VARIABLE_ISSUE
	      MD_SCHED_VARIABLE_ISSUE (dump, sched_verbose, insn, can_issue_more);
#else
	      can_issue_more--;
#endif

	      n_ready = schedule_insn (insn, ready, n_ready, clock_var);

	      /* remove insn from ready list */
	      ready[i] = ready[--n_ready];

	      /* close this block after scheduling its jump */
	      if (GET_CODE (last_scheduled_insn) == JUMP_INSN)
		break;
	    }
	}

      /* debug info */
      if (sched_verbose)
	{
	  visualize_scheduled_insns (b, clock_var);
	}
    }

  /* debug info */
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

  /* update head/tail boundaries.  */
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

  /* update target block boundaries.  */
  if (new_needs & NEED_HEAD)
    BLOCK_HEAD (b) = head;

  if (new_needs & NEED_TAIL)
    BLOCK_END (b) = tail;

  /* debugging */
  if (sched_verbose)
    {
      fprintf (dump, ";;   total time = %d\n;;   new basic block head = %d\n",
	       clock_var, INSN_UID (BLOCK_HEAD (b)));
      fprintf (dump, ";;   new basic block end = %d\n\n",
	       INSN_UID (BLOCK_END (b)));
    }

  return (sched_n_insns);
}				/* schedule_block () */


/* print the bit-set of registers, S.  callable from debugger */

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

  get_block_head_tail (bb, &head, &tail);
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

	  /* Ignore dependences upon deleted insn */
	  if (GET_CODE (x) == NOTE || INSN_DELETED_P (x))
	    continue;
	  if (find_insn_list (insn, INSN_DEPEND (x)))
	    continue;

	  new_link = alloc_INSN_LIST (insn, INSN_DEPEND (x));

	  dep_type = REG_NOTE_KIND (link);
	  PUT_REG_NOTE_KIND (new_link, dep_type);

	  INSN_DEPEND (x) = new_link;
	  INSN_DEP_COUNT (insn) += 1;
	}
    }
}

/* Initialize variables for region data dependence analysis.
   n_bbs is the number of region blocks */

__inline static void
init_rgn_data_dependences (n_bbs)
     int n_bbs;
{
  int bb;

  /* variables for which one copy exists for each block */
  bzero ((char *) bb_pending_read_insns, n_bbs * sizeof (rtx));
  bzero ((char *) bb_pending_read_mems, n_bbs * sizeof (rtx));
  bzero ((char *) bb_pending_write_insns, n_bbs * sizeof (rtx));
  bzero ((char *) bb_pending_write_mems, n_bbs * sizeof (rtx));
  bzero ((char *) bb_pending_lists_length, n_bbs * sizeof (rtx));
  bzero ((char *) bb_last_pending_memory_flush, n_bbs * sizeof (rtx));
  bzero ((char *) bb_last_function_call, n_bbs * sizeof (rtx));
  bzero ((char *) bb_sched_before_next_call, n_bbs * sizeof (rtx));

  /* Create an insn here so that we can hang dependencies off of it later.  */
  for (bb = 0; bb < n_bbs; bb++)
    {
      bb_sched_before_next_call[bb] =
	gen_rtx_INSN (VOIDmode, 0, NULL_RTX, NULL_RTX,
		      NULL_RTX, 0, NULL_RTX, NULL_RTX);
      LOG_LINKS (bb_sched_before_next_call[bb]) = 0;
    }
}

/* Add dependences so that branches are scheduled to run last in their block */

static void
add_branch_dependences (head, tail)
     rtx head, tail;
{

  rtx insn, last;

  /* For all branches, calls, uses, and cc0 setters, force them to remain
     in order at the end of the block by adding dependencies and giving
     the last a high priority.  There may be notes present, and prev_head
     may also be a note.

     Branches must obviously remain at the end.  Calls should remain at the
     end since moving them results in worse register allocation.  Uses remain
     at the end to ensure proper register allocation.  cc0 setters remaim
     at the end because they can't be moved away from their cc0 user.  */
  insn = tail;
  last = 0;
  while (GET_CODE (insn) == CALL_INSN || GET_CODE (insn) == JUMP_INSN
	 || (GET_CODE (insn) == INSN
	     && (GET_CODE (PATTERN (insn)) == USE
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

  /* make sure these insns are scheduled last in their block */
  insn = last;
  if (insn != 0)
    while (insn != head)
      {
	insn = prev_nonnote_insn (insn);

	if (INSN_REF_COUNT (insn) != 0)
	  continue;

	if (!find_insn_list (last, LOG_LINKS (insn)))
	  add_dependence (last, insn, REG_DEP_ANTI);
	INSN_REF_COUNT (insn) = 1;

	/* Skip over insns that are part of a group.  */
	while (SCHED_GROUP_P (insn))
	  insn = prev_nonnote_insn (insn);
      }
}

/* Compute bacward dependences inside BB.  In a multiple blocks region:
   (1) a bb is analyzed after its predecessors, and (2) the lists in
   effect at the end of bb (after analyzing for bb) are inherited by
   bb's successrs.

   Specifically for reg-reg data dependences, the block insns are
   scanned by sched_analyze () top-to-bottom.  Two lists are
   naintained by sched_analyze (): reg_last_defs[] for register DEFs,
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
  int b;
  rtx x;
  rtx head, tail;
  int max_reg = max_reg_num ();

  b = BB_TO_BLOCK (bb);

  if (current_nr_blocks == 1)
    {
      reg_last_uses = (rtx *) alloca (max_reg * sizeof (rtx));
      reg_last_sets = (rtx *) alloca (max_reg * sizeof (rtx));
      reg_last_clobbers = (rtx *) alloca (max_reg * sizeof (rtx));

      bzero ((char *) reg_last_uses, max_reg * sizeof (rtx));
      bzero ((char *) reg_last_sets, max_reg * sizeof (rtx));
      bzero ((char *) reg_last_clobbers, max_reg * sizeof (rtx));

      pending_read_insns = 0;
      pending_read_mems = 0;
      pending_write_insns = 0;
      pending_write_mems = 0;
      pending_lists_length = 0;
      last_function_call = 0;
      last_pending_memory_flush = 0;
      sched_before_next_call
	= gen_rtx_INSN (VOIDmode, 0, NULL_RTX, NULL_RTX,
			NULL_RTX, 0, NULL_RTX, NULL_RTX);
      LOG_LINKS (sched_before_next_call) = 0;
    }
  else
    {
      reg_last_uses = bb_reg_last_uses[bb];
      reg_last_sets = bb_reg_last_sets[bb];
      reg_last_clobbers = bb_reg_last_clobbers[bb];

      pending_read_insns = bb_pending_read_insns[bb];
      pending_read_mems = bb_pending_read_mems[bb];
      pending_write_insns = bb_pending_write_insns[bb];
      pending_write_mems = bb_pending_write_mems[bb];
      pending_lists_length = bb_pending_lists_length[bb];
      last_function_call = bb_last_function_call[bb];
      last_pending_memory_flush = bb_last_pending_memory_flush[bb];

      sched_before_next_call = bb_sched_before_next_call[bb];
    }

  /* do the analysis for this block */
  get_block_head_tail (bb, &head, &tail);
  sched_analyze (head, tail);
  add_branch_dependences (head, tail);

  if (current_nr_blocks > 1)
    {
      int e, first_edge;
      int b_succ, bb_succ;
      int reg;
      rtx link_insn, link_mem;
      rtx u;

      /* these lists should point to the right place, for correct freeing later.  */
      bb_pending_read_insns[bb] = pending_read_insns;
      bb_pending_read_mems[bb] = pending_read_mems;
      bb_pending_write_insns[bb] = pending_write_insns;
      bb_pending_write_mems[bb] = pending_write_mems;

      /* bb's structures are inherited by it's successors */
      first_edge = e = OUT_EDGES (b);
      if (e > 0)
	do
	  {
	    b_succ = TO_BLOCK (e);
	    bb_succ = BLOCK_TO_BB (b_succ);

	    /* only bbs "below" bb, in the same region, are interesting */
	    if (CONTAINING_RGN (b) != CONTAINING_RGN (b_succ)
		|| bb_succ <= bb)
	      {
		e = NEXT_OUT (e);
		continue;
	      }

	    for (reg = 0; reg < max_reg; reg++)
	      {

		/* reg-last-uses lists are inherited by bb_succ */
		for (u = reg_last_uses[reg]; u; u = XEXP (u, 1))
		  {
		    if (find_insn_list (XEXP (u, 0), (bb_reg_last_uses[bb_succ])[reg]))
		      continue;

		    (bb_reg_last_uses[bb_succ])[reg]
		      = alloc_INSN_LIST (XEXP (u, 0),
					 (bb_reg_last_uses[bb_succ])[reg]);
		  }

		/* reg-last-defs lists are inherited by bb_succ */
		for (u = reg_last_sets[reg]; u; u = XEXP (u, 1))
		  {
		    if (find_insn_list (XEXP (u, 0), (bb_reg_last_sets[bb_succ])[reg]))
		      continue;

		    (bb_reg_last_sets[bb_succ])[reg]
		      = alloc_INSN_LIST (XEXP (u, 0),
					 (bb_reg_last_sets[bb_succ])[reg]);
		  }

		for (u = reg_last_clobbers[reg]; u; u = XEXP (u, 1))
		  {
		    if (find_insn_list (XEXP (u, 0), (bb_reg_last_clobbers[bb_succ])[reg]))
		      continue;

		    (bb_reg_last_clobbers[bb_succ])[reg]
		      = alloc_INSN_LIST (XEXP (u, 0),
					 (bb_reg_last_clobbers[bb_succ])[reg]);
		  }
	      }

	    /* mem read/write lists are inherited by bb_succ */
	    link_insn = pending_read_insns;
	    link_mem = pending_read_mems;
	    while (link_insn)
	      {
		if (!(find_insn_mem_list (XEXP (link_insn, 0), XEXP (link_mem, 0),
					  bb_pending_read_insns[bb_succ],
					  bb_pending_read_mems[bb_succ])))
		  add_insn_mem_dependence (&bb_pending_read_insns[bb_succ],
					   &bb_pending_read_mems[bb_succ],
				   XEXP (link_insn, 0), XEXP (link_mem, 0));
		link_insn = XEXP (link_insn, 1);
		link_mem = XEXP (link_mem, 1);
	      }

	    link_insn = pending_write_insns;
	    link_mem = pending_write_mems;
	    while (link_insn)
	      {
		if (!(find_insn_mem_list (XEXP (link_insn, 0), XEXP (link_mem, 0),
					  bb_pending_write_insns[bb_succ],
					  bb_pending_write_mems[bb_succ])))
		  add_insn_mem_dependence (&bb_pending_write_insns[bb_succ],
					   &bb_pending_write_mems[bb_succ],
				   XEXP (link_insn, 0), XEXP (link_mem, 0));

		link_insn = XEXP (link_insn, 1);
		link_mem = XEXP (link_mem, 1);
	      }

	    /* last_function_call is inherited by bb_succ */
	    for (u = last_function_call; u; u = XEXP (u, 1))
	      {
		if (find_insn_list (XEXP (u, 0), bb_last_function_call[bb_succ]))
		  continue;

		bb_last_function_call[bb_succ]
		  = alloc_INSN_LIST (XEXP (u, 0),
				     bb_last_function_call[bb_succ]);
	      }

	    /* last_pending_memory_flush is inherited by bb_succ */
	    for (u = last_pending_memory_flush; u; u = XEXP (u, 1))
	      {
		if (find_insn_list (XEXP (u, 0), bb_last_pending_memory_flush[bb_succ]))
		  continue;

		bb_last_pending_memory_flush[bb_succ]
		  = alloc_INSN_LIST (XEXP (u, 0),
				     bb_last_pending_memory_flush[bb_succ]);
	      }

	    /* sched_before_next_call is inherited by bb_succ */
	    x = LOG_LINKS (sched_before_next_call);
	    for (; x; x = XEXP (x, 1))
	      add_dependence (bb_sched_before_next_call[bb_succ],
			      XEXP (x, 0), REG_DEP_ANTI);

	    e = NEXT_OUT (e);
	  }
	while (e != first_edge);
    }

  /* Free up the INSN_LISTs 

     Note this loop is executed max_reg * nr_regions times.  It's first 
     implementation accounted for over 90% of the calls to free_list.
     The list was empty for the vast majority of those calls.  On the PA,
     not calling free_list in those cases improves -O2 compile times by
     3-5% on average.  */
  for (b = 0; b < max_reg; ++b)
    {
      if (reg_last_clobbers[b])
	free_list (&reg_last_clobbers[b], &unused_insn_list);
      if (reg_last_sets[b])
	free_list (&reg_last_sets[b], &unused_insn_list);
      if (reg_last_uses[b])
	free_list (&reg_last_uses[b], &unused_insn_list);
    }

  /* Assert that we won't need bb_reg_last_* for this block anymore.  */
  if (current_nr_blocks > 1)
    {
      bb_reg_last_uses[bb] = (rtx *) NULL_RTX;
      bb_reg_last_sets[bb] = (rtx *) NULL_RTX;
      bb_reg_last_clobbers[bb] = (rtx *) NULL_RTX;
    }
}

/* Print dependences for debugging, callable from debugger */

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

	  get_block_head_tail (bb, &head, &tail);
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

/* Set_priorities: compute priority of each insn in the block */

static int
set_priorities (bb)
     int bb;
{
  rtx insn;
  int n_insn;

  rtx tail;
  rtx prev_head;
  rtx head;

  get_block_head_tail (bb, &head, &tail);
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

/* Make each element of VECTOR point at an rtx-vector,
   taking the space for all those rtx-vectors from SPACE.
   SPACE is of type (rtx *), but it is really as long as NELTS rtx-vectors.
   BYTES_PER_ELT is the number of bytes in one rtx-vector.
   (this is the same as init_regset_vector () in flow.c) */

static void
init_rtx_vector (vector, space, nelts, bytes_per_elt)
     rtx **vector;
     rtx *space;
     int nelts;
     int bytes_per_elt;
{
  register int i;
  register rtx *p = space;

  for (i = 0; i < nelts; i++)
    {
      vector[i] = p;
      p += bytes_per_elt / sizeof (*p);
    }
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

  /* set variables for the current region */
  current_nr_blocks = RGN_NR_BLOCKS (rgn);
  current_blocks = RGN_BLOCKS (rgn);

  reg_pending_sets = ALLOCA_REG_SET ();
  reg_pending_clobbers = ALLOCA_REG_SET ();
  reg_pending_sets_all = 0;

  /* initializations for region data dependence analyisis */
  if (current_nr_blocks > 1)
    {
      rtx *space;
      int maxreg = max_reg_num ();

      bb_reg_last_uses = (rtx **) alloca (current_nr_blocks * sizeof (rtx *));
      space = (rtx *) alloca (current_nr_blocks * maxreg * sizeof (rtx));
      bzero ((char *) space, current_nr_blocks * maxreg * sizeof (rtx));
      init_rtx_vector (bb_reg_last_uses, space, current_nr_blocks,
		       maxreg * sizeof (rtx *));

      bb_reg_last_sets = (rtx **) alloca (current_nr_blocks * sizeof (rtx *));
      space = (rtx *) alloca (current_nr_blocks * maxreg * sizeof (rtx));
      bzero ((char *) space, current_nr_blocks * maxreg * sizeof (rtx));
      init_rtx_vector (bb_reg_last_sets, space, current_nr_blocks,
		       maxreg * sizeof (rtx *));

      bb_reg_last_clobbers =
	(rtx **) alloca (current_nr_blocks * sizeof (rtx *));
      space = (rtx *) alloca (current_nr_blocks * maxreg * sizeof (rtx));
      bzero ((char *) space, current_nr_blocks * maxreg * sizeof (rtx));
      init_rtx_vector (bb_reg_last_clobbers, space, current_nr_blocks,
		       maxreg * sizeof (rtx *));

      bb_pending_read_insns = (rtx *) alloca (current_nr_blocks * sizeof (rtx));
      bb_pending_read_mems = (rtx *) alloca (current_nr_blocks * sizeof (rtx));
      bb_pending_write_insns =
	(rtx *) alloca (current_nr_blocks * sizeof (rtx));
      bb_pending_write_mems = (rtx *) alloca (current_nr_blocks * sizeof (rtx));
      bb_pending_lists_length =
	(int *) alloca (current_nr_blocks * sizeof (int));
      bb_last_pending_memory_flush =
	(rtx *) alloca (current_nr_blocks * sizeof (rtx));
      bb_last_function_call = (rtx *) alloca (current_nr_blocks * sizeof (rtx));
      bb_sched_before_next_call =
	(rtx *) alloca (current_nr_blocks * sizeof (rtx));

      init_rgn_data_dependences (current_nr_blocks);
    }

  /* compute LOG_LINKS */
  for (bb = 0; bb < current_nr_blocks; bb++)
    compute_block_backward_dependences (bb);

  /* compute INSN_DEPEND */
  for (bb = current_nr_blocks - 1; bb >= 0; bb--)
    compute_block_forward_dependences (bb);

  /* Delete line notes, compute live-regs at block end, and set priorities.  */
  dead_notes = 0;
  for (bb = 0; bb < current_nr_blocks; bb++)
    {
      if (reload_completed == 0)
	find_pre_sched_live (bb);

      if (write_symbols != NO_DEBUG)
	{
	  save_line_notes (bb);
	  rm_line_notes (bb);
	}

      rgn_n_insns += set_priorities (bb);
    }

  /* compute interblock info: probabilities, split-edges, dominators, etc.  */
  if (current_nr_blocks > 1)
    {
      int i;

      prob = (float *) alloca ((current_nr_blocks) * sizeof (float));

      bbset_size = current_nr_blocks / HOST_BITS_PER_WIDE_INT + 1;
      dom = (bbset *) alloca (current_nr_blocks * sizeof (bbset));
      for (i = 0; i < current_nr_blocks; i++)
	{
	  dom[i] = (bbset) alloca (bbset_size * sizeof (HOST_WIDE_INT));
	  bzero ((char *) dom[i], bbset_size * sizeof (HOST_WIDE_INT));
	}

      /* edge to bit */
      rgn_nr_edges = 0;
      edge_to_bit = (int *) alloca (nr_edges * sizeof (int));
      for (i = 1; i < nr_edges; i++)
	if (CONTAINING_RGN (FROM_BLOCK (i)) == rgn)
	  EDGE_TO_BIT (i) = rgn_nr_edges++;
      rgn_edges = (int *) alloca (rgn_nr_edges * sizeof (int));

      rgn_nr_edges = 0;
      for (i = 1; i < nr_edges; i++)
	if (CONTAINING_RGN (FROM_BLOCK (i)) == (rgn))
	  rgn_edges[rgn_nr_edges++] = i;

      /* split edges */
      edgeset_size = rgn_nr_edges / HOST_BITS_PER_WIDE_INT + 1;
      pot_split = (edgeset *) alloca (current_nr_blocks * sizeof (edgeset));
      ancestor_edges = (edgeset *) alloca (current_nr_blocks * sizeof (edgeset));
      for (i = 0; i < current_nr_blocks; i++)
	{
	  pot_split[i] =
	    (edgeset) alloca (edgeset_size * sizeof (HOST_WIDE_INT));
	  bzero ((char *) pot_split[i],
		 edgeset_size * sizeof (HOST_WIDE_INT));
	  ancestor_edges[i] =
	    (edgeset) alloca (edgeset_size * sizeof (HOST_WIDE_INT));
	  bzero ((char *) ancestor_edges[i],
		 edgeset_size * sizeof (HOST_WIDE_INT));
	}

      /* compute probabilities, dominators, split_edges */
      for (bb = 0; bb < current_nr_blocks; bb++)
	compute_dom_prob_ps (bb);
    }

  /* now we can schedule all blocks */
  for (bb = 0; bb < current_nr_blocks; bb++)
    {
      sched_rgn_n_insns += schedule_block (bb, rgn_n_insns);

#ifdef USE_C_ALLOCA
      alloca (0);
#endif
    }

  /* sanity check: verify that all region insns were scheduled */
  if (sched_rgn_n_insns != rgn_n_insns)
    abort ();

  /* update register life and usage information */
  if (reload_completed == 0)
    {
      for (bb = current_nr_blocks - 1; bb >= 0; bb--)
	find_post_sched_live (bb);

      if (current_nr_blocks <= 1)
	/* Sanity check.  There should be no REG_DEAD notes leftover at the end.
	   In practice, this can occur as the result of bugs in flow, combine.c,
	   and/or sched.c.  The values of the REG_DEAD notes remaining are
	   meaningless, because dead_notes is just used as a free list.  */
	if (dead_notes != 0)
	  abort ();
    }

  /* restore line notes.  */
  if (write_symbols != NO_DEBUG)
    {
      for (bb = 0; bb < current_nr_blocks; bb++)
	restore_line_notes (bb);
    }

  /* Done with this region */
  free_pending_lists ();

  FREE_REG_SET (reg_pending_sets);
  FREE_REG_SET (reg_pending_clobbers);
}

/* Subroutine of update_flow_info.  Determines whether any new REG_NOTEs are
   needed for the hard register mentioned in the note.  This can happen
   if the reference to the hard register in the original insn was split into
   several smaller hard register references in the split insns.  */

static void
split_hard_reg_notes (note, first, last)
     rtx note, first, last;
{
  rtx reg, temp, link;
  int n_regs, i, new_reg;
  rtx insn;

  /* Assume that this is a REG_DEAD note.  */
  if (REG_NOTE_KIND (note) != REG_DEAD)
    abort ();

  reg = XEXP (note, 0);

  n_regs = HARD_REGNO_NREGS (REGNO (reg), GET_MODE (reg));

  for (i = 0; i < n_regs; i++)
    {
      new_reg = REGNO (reg) + i;

      /* Check for references to new_reg in the split insns.  */
      for (insn = last;; insn = PREV_INSN (insn))
	{
	  if (GET_RTX_CLASS (GET_CODE (insn)) == 'i'
	      && (temp = regno_use_in (new_reg, PATTERN (insn))))
	    {
	      /* Create a new reg dead note ere.  */
	      link = alloc_EXPR_LIST (REG_DEAD, temp, REG_NOTES (insn));
	      REG_NOTES (insn) = link;

	      /* If killed multiple registers here, then add in the excess.  */
	      i += HARD_REGNO_NREGS (REGNO (temp), GET_MODE (temp)) - 1;

	      break;
	    }
	  /* It isn't mentioned anywhere, so no new reg note is needed for
	     this register.  */
	  if (insn == first)
	    break;
	}
    }
}

/* Subroutine of update_flow_info.  Determines whether a SET or CLOBBER in an
   insn created by splitting needs a REG_DEAD or REG_UNUSED note added.  */

static void
new_insn_dead_notes (pat, insn, last, orig_insn)
     rtx pat, insn, last, orig_insn;
{
  rtx dest, tem, set;

  /* PAT is either a CLOBBER or a SET here.  */
  dest = XEXP (pat, 0);

  while (GET_CODE (dest) == ZERO_EXTRACT || GET_CODE (dest) == SUBREG
	 || GET_CODE (dest) == STRICT_LOW_PART
	 || GET_CODE (dest) == SIGN_EXTRACT)
    dest = XEXP (dest, 0);

  if (GET_CODE (dest) == REG)
    {
      /* If the original insn already used this register, we may not add new
         notes for it.  One example for a split that needs this test is
	 when a multi-word memory access with register-indirect addressing
	 is split into multiple memory accesses with auto-increment and
	 one adjusting add instruction for the address register.  */
      if (reg_referenced_p (dest, PATTERN (orig_insn)))
	return;
      for (tem = last; tem != insn; tem = PREV_INSN (tem))
	{
	  if (GET_RTX_CLASS (GET_CODE (tem)) == 'i'
	      && reg_overlap_mentioned_p (dest, PATTERN (tem))
	      && (set = single_set (tem)))
	    {
	      rtx tem_dest = SET_DEST (set);

	      while (GET_CODE (tem_dest) == ZERO_EXTRACT
		     || GET_CODE (tem_dest) == SUBREG
		     || GET_CODE (tem_dest) == STRICT_LOW_PART
		     || GET_CODE (tem_dest) == SIGN_EXTRACT)
		tem_dest = XEXP (tem_dest, 0);

	      if (!rtx_equal_p (tem_dest, dest))
		{
		  /* Use the same scheme as combine.c, don't put both REG_DEAD
		     and REG_UNUSED notes on the same insn.  */
		  if (!find_regno_note (tem, REG_UNUSED, REGNO (dest))
		      && !find_regno_note (tem, REG_DEAD, REGNO (dest)))
		    {
		      rtx note = alloc_EXPR_LIST (REG_DEAD, dest,
						  REG_NOTES (tem));
		      REG_NOTES (tem) = note;
		    }
		  /* The reg only dies in one insn, the last one that uses
		     it.  */
		  break;
		}
	      else if (reg_overlap_mentioned_p (dest, SET_SRC (set)))
		/* We found an instruction that both uses the register,
		   and sets it, so no new REG_NOTE is needed for this set.  */
		break;
	    }
	}
      /* If this is a set, it must die somewhere, unless it is the dest of
         the original insn, and hence is live after the original insn.  Abort
         if it isn't supposed to be live after the original insn.

         If this is a clobber, then just add a REG_UNUSED note.  */
      if (tem == insn)
	{
	  int live_after_orig_insn = 0;
	  rtx pattern = PATTERN (orig_insn);
	  int i;

	  if (GET_CODE (pat) == CLOBBER)
	    {
	      rtx note = alloc_EXPR_LIST (REG_UNUSED, dest, REG_NOTES (insn));
	      REG_NOTES (insn) = note;
	      return;
	    }

	  /* The original insn could have multiple sets, so search the
	     insn for all sets.  */
	  if (GET_CODE (pattern) == SET)
	    {
	      if (reg_overlap_mentioned_p (dest, SET_DEST (pattern)))
		live_after_orig_insn = 1;
	    }
	  else if (GET_CODE (pattern) == PARALLEL)
	    {
	      for (i = 0; i < XVECLEN (pattern, 0); i++)
		if (GET_CODE (XVECEXP (pattern, 0, i)) == SET
		    && reg_overlap_mentioned_p (dest,
						SET_DEST (XVECEXP (pattern,
								   0, i))))
		  live_after_orig_insn = 1;
	    }

	  if (!live_after_orig_insn)
	    abort ();
	}
    }
}

/* Subroutine of update_flow_info.  Update the value of reg_n_sets for all
   registers modified by X.  INC is -1 if the containing insn is being deleted,
   and is 1 if the containing insn is a newly generated insn.  */

static void
update_n_sets (x, inc)
     rtx x;
     int inc;
{
  rtx dest = SET_DEST (x);

  while (GET_CODE (dest) == STRICT_LOW_PART || GET_CODE (dest) == SUBREG
      || GET_CODE (dest) == ZERO_EXTRACT || GET_CODE (dest) == SIGN_EXTRACT)
    dest = SUBREG_REG (dest);

  if (GET_CODE (dest) == REG)
    {
      int regno = REGNO (dest);

      if (regno < FIRST_PSEUDO_REGISTER)
	{
	  register int i;
	  int endregno = regno + HARD_REGNO_NREGS (regno, GET_MODE (dest));

	  for (i = regno; i < endregno; i++)
	    REG_N_SETS (i) += inc;
	}
      else
	REG_N_SETS (regno) += inc;
    }
}

/* Updates all flow-analysis related quantities (including REG_NOTES) for
   the insns from FIRST to LAST inclusive that were created by splitting
   ORIG_INSN.  NOTES are the original REG_NOTES.  */

void
update_flow_info (notes, first, last, orig_insn)
     rtx notes;
     rtx first, last;
     rtx orig_insn;
{
  rtx insn, note;
  rtx next;
  rtx orig_dest, temp;
  rtx set;

  /* Get and save the destination set by the original insn.  */

  orig_dest = single_set (orig_insn);
  if (orig_dest)
    orig_dest = SET_DEST (orig_dest);

  /* Move REG_NOTES from the original insn to where they now belong.  */

  for (note = notes; note; note = next)
    {
      next = XEXP (note, 1);
      switch (REG_NOTE_KIND (note))
	{
	case REG_DEAD:
	case REG_UNUSED:
	  /* Move these notes from the original insn to the last new insn where
	     the register is now set.  */

	  for (insn = last;; insn = PREV_INSN (insn))
	    {
	      if (GET_RTX_CLASS (GET_CODE (insn)) == 'i'
		  && reg_mentioned_p (XEXP (note, 0), PATTERN (insn)))
		{
		  /* If this note refers to a multiple word hard register, it
		     may have been split into several smaller hard register
		     references, so handle it specially.  */
		  temp = XEXP (note, 0);
		  if (REG_NOTE_KIND (note) == REG_DEAD
		      && GET_CODE (temp) == REG
		      && REGNO (temp) < FIRST_PSEUDO_REGISTER
		      && HARD_REGNO_NREGS (REGNO (temp), GET_MODE (temp)) > 1)
		    split_hard_reg_notes (note, first, last);
		  else
		    {
		      XEXP (note, 1) = REG_NOTES (insn);
		      REG_NOTES (insn) = note;
		    }

		  /* Sometimes need to convert REG_UNUSED notes to REG_DEAD
		     notes.  */
		  /* ??? This won't handle multiple word registers correctly,
		     but should be good enough for now.  */
		  if (REG_NOTE_KIND (note) == REG_UNUSED
		      && GET_CODE (XEXP (note, 0)) != SCRATCH
		      && !dead_or_set_p (insn, XEXP (note, 0)))
		    PUT_REG_NOTE_KIND (note, REG_DEAD);

		  /* The reg only dies in one insn, the last one that uses
		     it.  */
		  break;
		}
	      /* It must die somewhere, fail it we couldn't find where it died.

	         If this is a REG_UNUSED note, then it must be a temporary
	         register that was not needed by this instantiation of the
	         pattern, so we can safely ignore it.  */
	      if (insn == first)
		{			
		  if (REG_NOTE_KIND (note) != REG_UNUSED)
		    abort ();

		  break;
		}
	    }
	  break;

	case REG_WAS_0:
	  /* If the insn that set the register to 0 was deleted, this
	     note cannot be relied on any longer.  The destination might
	     even have been moved to memory.
             This was observed for SH4 with execute/920501-6.c compilation,
	     -O2 -fomit-frame-pointer -finline-functions .  */
	  if (GET_CODE (XEXP (note, 0)) == NOTE
	      || INSN_DELETED_P (XEXP (note, 0)))
	    break;
	  /* This note applies to the dest of the original insn.  Find the
	     first new insn that now has the same dest, and move the note
	     there.  */

	  if (!orig_dest)
	    abort ();

	  for (insn = first;; insn = NEXT_INSN (insn))
	    {
	      if (GET_RTX_CLASS (GET_CODE (insn)) == 'i'
		  && (temp = single_set (insn))
		  && rtx_equal_p (SET_DEST (temp), orig_dest))
		{
		  XEXP (note, 1) = REG_NOTES (insn);
		  REG_NOTES (insn) = note;
		  /* The reg is only zero before one insn, the first that
		     uses it.  */
		  break;
		}
	      /* If this note refers to a multiple word hard
		 register, it may have been split into several smaller
		 hard register references.  We could split the notes,
		 but simply dropping them is good enough.  */
	      if (GET_CODE (orig_dest) == REG
		  && REGNO (orig_dest) < FIRST_PSEUDO_REGISTER
		  && HARD_REGNO_NREGS (REGNO (orig_dest),
				       GET_MODE (orig_dest)) > 1)
		    break;
	      /* It must be set somewhere, fail if we couldn't find where it
	         was set.  */
	      if (insn == last)
		abort ();
	    }
	  break;

	case REG_EQUAL:
	case REG_EQUIV:
	  /* A REG_EQUIV or REG_EQUAL note on an insn with more than one
	     set is meaningless.  Just drop the note.  */
	  if (!orig_dest)
	    break;

	case REG_NO_CONFLICT:
	  /* These notes apply to the dest of the original insn.  Find the last
	     new insn that now has the same dest, and move the note there.  */

	  if (!orig_dest)
	    abort ();

	  for (insn = last;; insn = PREV_INSN (insn))
	    {
	      if (GET_RTX_CLASS (GET_CODE (insn)) == 'i'
		  && (temp = single_set (insn))
		  && rtx_equal_p (SET_DEST (temp), orig_dest))
		{
		  XEXP (note, 1) = REG_NOTES (insn);
		  REG_NOTES (insn) = note;
		  /* Only put this note on one of the new insns.  */
		  break;
		}

	      /* The original dest must still be set someplace.  Abort if we
	         couldn't find it.  */
	      if (insn == first)
		{
		  /* However, if this note refers to a multiple word hard
		     register, it may have been split into several smaller
		     hard register references.  We could split the notes,
		     but simply dropping them is good enough.  */
		  if (GET_CODE (orig_dest) == REG
		      && REGNO (orig_dest) < FIRST_PSEUDO_REGISTER
		      && HARD_REGNO_NREGS (REGNO (orig_dest),
					   GET_MODE (orig_dest)) > 1)
		    break;
		  /* Likewise for multi-word memory references.  */
		  if (GET_CODE (orig_dest) == MEM
		      && SIZE_FOR_MODE (orig_dest) > UNITS_PER_WORD)
		    break;
		  abort ();
		}
	    }
	  break;

	case REG_LIBCALL:
	  /* Move a REG_LIBCALL note to the first insn created, and update
	     the corresponding REG_RETVAL note.  */
	  XEXP (note, 1) = REG_NOTES (first);
	  REG_NOTES (first) = note;

	  insn = XEXP (note, 0);
	  note = find_reg_note (insn, REG_RETVAL, NULL_RTX);
	  if (note)
	    XEXP (note, 0) = first;
	  break;

	case REG_EXEC_COUNT:
	  /* Move a REG_EXEC_COUNT note to the first insn created.  */
	  XEXP (note, 1) = REG_NOTES (first);
	  REG_NOTES (first) = note;
	  break;

	case REG_RETVAL:
	  /* Move a REG_RETVAL note to the last insn created, and update
	     the corresponding REG_LIBCALL note.  */
	  XEXP (note, 1) = REG_NOTES (last);
	  REG_NOTES (last) = note;

	  insn = XEXP (note, 0);
	  note = find_reg_note (insn, REG_LIBCALL, NULL_RTX);
	  if (note)
	    XEXP (note, 0) = last;
	  break;

	case REG_NONNEG:
	case REG_BR_PROB:
	  /* This should be moved to whichever instruction is a JUMP_INSN.  */

	  for (insn = last;; insn = PREV_INSN (insn))
	    {
	      if (GET_CODE (insn) == JUMP_INSN)
		{
		  XEXP (note, 1) = REG_NOTES (insn);
		  REG_NOTES (insn) = note;
		  /* Only put this note on one of the new insns.  */
		  break;
		}
	      /* Fail if we couldn't find a JUMP_INSN.  */
	      if (insn == first)
		abort ();
	    }
	  break;

	case REG_INC:
	  /* reload sometimes leaves obsolete REG_INC notes around.  */
	  if (reload_completed)
	    break;
	  /* This should be moved to whichever instruction now has the
	     increment operation.  */
	  abort ();

	case REG_LABEL:
	  /* Should be moved to the new insn(s) which use the label.  */
	  for (insn = first; insn != NEXT_INSN (last); insn = NEXT_INSN (insn))
	    if (GET_RTX_CLASS (GET_CODE (insn)) == 'i'
		&& reg_mentioned_p (XEXP (note, 0), PATTERN (insn)))
	      {
	        REG_NOTES (insn) = alloc_EXPR_LIST (REG_LABEL,
						    XEXP (note, 0),
						    REG_NOTES (insn));
	      }
	  break;

	case REG_CC_SETTER:
	case REG_CC_USER:
	  /* These two notes will never appear until after reorg, so we don't
	     have to handle them here.  */
	default:
	  abort ();
	}
    }

  /* Each new insn created, except the last, has a new set.  If the destination
     is a register, then this reg is now live across several insns, whereas
     previously the dest reg was born and died within the same insn.  To
     reflect this, we now need a REG_DEAD note on the insn where this
     dest reg dies.

     Similarly, the new insns may have clobbers that need REG_UNUSED notes.  */

  for (insn = first; insn != last; insn = NEXT_INSN (insn))
    {
      rtx pat;
      int i;

      pat = PATTERN (insn);
      if (GET_CODE (pat) == SET || GET_CODE (pat) == CLOBBER)
	new_insn_dead_notes (pat, insn, last, orig_insn);
      else if (GET_CODE (pat) == PARALLEL)
	{
	  for (i = 0; i < XVECLEN (pat, 0); i++)
	    if (GET_CODE (XVECEXP (pat, 0, i)) == SET
		|| GET_CODE (XVECEXP (pat, 0, i)) == CLOBBER)
	      new_insn_dead_notes (XVECEXP (pat, 0, i), insn, last, orig_insn);
	}
    }

  /* If any insn, except the last, uses the register set by the last insn,
     then we need a new REG_DEAD note on that insn.  In this case, there
     would not have been a REG_DEAD note for this register in the original
     insn because it was used and set within one insn.  */

  set = single_set (last);
  if (set)
    {
      rtx dest = SET_DEST (set);

      while (GET_CODE (dest) == ZERO_EXTRACT || GET_CODE (dest) == SUBREG
	     || GET_CODE (dest) == STRICT_LOW_PART
	     || GET_CODE (dest) == SIGN_EXTRACT)
	dest = XEXP (dest, 0);

      if (GET_CODE (dest) == REG
	  /* Global registers are always live, so the code below does not
	     apply to them.  */
	  && (REGNO (dest) >= FIRST_PSEUDO_REGISTER
	      || ! global_regs[REGNO (dest)]))
	{
	  rtx stop_insn = PREV_INSN (first);

	  /* If the last insn uses the register that it is setting, then
	     we don't want to put a REG_DEAD note there.  Search backwards
	     to find the first insn that sets but does not use DEST.  */

	  insn = last;
	  if (reg_overlap_mentioned_p (dest, SET_SRC (set)))
	    {
	      for (insn = PREV_INSN (insn); insn != first;
		   insn = PREV_INSN (insn))
		{
		  if ((set = single_set (insn))
		      && reg_mentioned_p (dest, SET_DEST (set))
		      && ! reg_overlap_mentioned_p (dest, SET_SRC (set)))
		    break;
		}
	    }

	  /* Now find the first insn that uses but does not set DEST.  */

	  for (insn = PREV_INSN (insn); insn != stop_insn;
	       insn = PREV_INSN (insn))
	    {
	      if (GET_RTX_CLASS (GET_CODE (insn)) == 'i'
		  && reg_mentioned_p (dest, PATTERN (insn))
		  && (set = single_set (insn)))
		{
		  rtx insn_dest = SET_DEST (set);

		  while (GET_CODE (insn_dest) == ZERO_EXTRACT
			 || GET_CODE (insn_dest) == SUBREG
			 || GET_CODE (insn_dest) == STRICT_LOW_PART
			 || GET_CODE (insn_dest) == SIGN_EXTRACT)
		    insn_dest = XEXP (insn_dest, 0);

		  if (insn_dest != dest)
		    {
		      note = alloc_EXPR_LIST (REG_DEAD, dest, REG_NOTES (insn));
		      REG_NOTES (insn) = note;
		      /* The reg only dies in one insn, the last one
			 that uses it.  */
		      break;
		    }
		}
	    }
	}
    }

  /* If the original dest is modifying a multiple register target, and the
     original instruction was split such that the original dest is now set
     by two or more SUBREG sets, then the split insns no longer kill the
     destination of the original insn.

     In this case, if there exists an instruction in the same basic block,
     before the split insn, which uses the original dest, and this use is
     killed by the original insn, then we must remove the REG_DEAD note on
     this insn, because it is now superfluous.

     This does not apply when a hard register gets split, because the code
     knows how to handle overlapping hard registers properly.  */
  if (orig_dest && GET_CODE (orig_dest) == REG)
    {
      int found_orig_dest = 0;
      int found_split_dest = 0;

      for (insn = first;; insn = NEXT_INSN (insn))
	{
	  rtx pat;
	  int i;

	  /* I'm not sure if this can happen, but let's be safe.  */
	  if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
	    continue;

	  pat = PATTERN (insn);
	  i = GET_CODE (pat) == PARALLEL ? XVECLEN (pat, 0) : 0;
	  set = pat;

	  for (;;)
	    {
	      if (GET_CODE (set) == SET)
		{
		  if (GET_CODE (SET_DEST (set)) == REG
		      && REGNO (SET_DEST (set)) == REGNO (orig_dest))
		    {
		      found_orig_dest = 1;
		      break;
		    }
		  else if (GET_CODE (SET_DEST (set)) == SUBREG
			   && SUBREG_REG (SET_DEST (set)) == orig_dest)
		    {
		      found_split_dest = 1;
		      break;
		    }
		}
	      if (--i < 0)
		break;
	      set = XVECEXP (pat, 0, i);
	    }

	  if (insn == last)
	    break;
	}

      if (found_split_dest)
	{
	  /* Search backwards from FIRST, looking for the first insn that uses
	     the original dest.  Stop if we pass a CODE_LABEL or a JUMP_INSN.
	     If we find an insn, and it has a REG_DEAD note, then delete the
	     note.  */

	  for (insn = first; insn; insn = PREV_INSN (insn))
	    {
	      if (GET_CODE (insn) == CODE_LABEL
		  || GET_CODE (insn) == JUMP_INSN)
		break;
	      else if (GET_RTX_CLASS (GET_CODE (insn)) == 'i'
		       && reg_mentioned_p (orig_dest, insn))
		{
		  note = find_regno_note (insn, REG_DEAD, REGNO (orig_dest));
		  if (note)
		    remove_note (insn, note);
		}
	    }
	}
      else if (!found_orig_dest)
	{
	  int i, regno;

	  /* Should never reach here for a pseudo reg.  */
	  if (REGNO (orig_dest) >= FIRST_PSEUDO_REGISTER)
	    abort ();

	  /* This can happen for a hard register, if the splitter
	     does not bother to emit instructions which would be no-ops.
	     We try to verify that this is the case by checking to see if
	     the original instruction uses all of the registers that it
	     set.  This case is OK, because deleting a no-op can not affect
	     REG_DEAD notes on other insns.  If this is not the case, then
	     abort.  */
	  
	  regno = REGNO (orig_dest);
	  for (i = HARD_REGNO_NREGS (regno, GET_MODE (orig_dest)) - 1;
	       i >= 0; i--)
	    if (! refers_to_regno_p (regno + i, regno + i + 1, orig_insn,
				     NULL_PTR))
	      break;
	  if (i >= 0)
	    abort ();
	}
    }

  /* Update reg_n_sets.  This is necessary to prevent local alloc from
     converting REG_EQUAL notes to REG_EQUIV when splitting has modified
     a reg from set once to set multiple times.  */

  {
    rtx x = PATTERN (orig_insn);
    RTX_CODE code = GET_CODE (x);

    if (code == SET || code == CLOBBER)
      update_n_sets (x, -1);
    else if (code == PARALLEL)
      {
	int i;
	for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
	  {
	    code = GET_CODE (XVECEXP (x, 0, i));
	    if (code == SET || code == CLOBBER)
	      update_n_sets (XVECEXP (x, 0, i), -1);
	  }
      }

    for (insn = first;; insn = NEXT_INSN (insn))
      {
	x = PATTERN (insn);
	code = GET_CODE (x);

	if (code == SET || code == CLOBBER)
	  update_n_sets (x, 1);
	else if (code == PARALLEL)
	  {
	    int i;
	    for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
	      {
		code = GET_CODE (XVECEXP (x, 0, i));
		if (code == SET || code == CLOBBER)
		  update_n_sets (XVECEXP (x, 0, i), 1);
	      }
	  }

	if (insn == last)
	  break;
      }
  }
}

/* The one entry point in this file.  DUMP_FILE is the dump file for
   this pass.  */

void
schedule_insns (dump_file)
     FILE *dump_file;
{

  int max_uid;
  int b;
  rtx insn;
  int rgn;

  int luid;

  /* disable speculative loads in their presence if cc0 defined */
#ifdef HAVE_cc0
  flag_schedule_speculative_load = 0;
#endif

  /* Taking care of this degenerate case makes the rest of
     this code simpler.  */
  if (n_basic_blocks == 0)
    return;

  /* set dump and sched_verbose for the desired debugging output.  If no
     dump-file was specified, but -fsched-verbose-N (any N), print to stderr.
     For -fsched-verbose-N, N>=10, print everything to stderr.  */
  sched_verbose = sched_verbose_param;
  if (sched_verbose_param == 0 && dump_file)
    sched_verbose = 1;
  dump = ((sched_verbose_param >= 10 || !dump_file) ? stderr : dump_file);

  nr_inter = 0;
  nr_spec = 0;

  /* Initialize the unused_*_lists.  We can't use the ones left over from
     the previous function, because gcc has freed that memory.  We can use
     the ones left over from the first sched pass in the second pass however,
     so only clear them on the first sched pass.  The first pass is before
     reload if flag_schedule_insns is set, otherwise it is afterwards.  */

  if (reload_completed == 0 || !flag_schedule_insns)
    {
      unused_insn_list = 0;
      unused_expr_list = 0;
    }

  /* initialize issue_rate */
  issue_rate = ISSUE_RATE;

  /* do the splitting first for all blocks */
  for (b = 0; b < n_basic_blocks; b++)
    split_block_insns (b, 1);

  max_uid = (get_max_uid () + 1);

  cant_move = (char *) xmalloc (max_uid * sizeof (char));
  bzero ((char *) cant_move, max_uid * sizeof (char));

  fed_by_spec_load = (char *) xmalloc (max_uid * sizeof (char));
  bzero ((char *) fed_by_spec_load, max_uid * sizeof (char));

  is_load_insn = (char *) xmalloc (max_uid * sizeof (char));
  bzero ((char *) is_load_insn, max_uid * sizeof (char));

  insn_orig_block = (int *) xmalloc (max_uid * sizeof (int));
  insn_luid = (int *) xmalloc (max_uid * sizeof (int));

  luid = 0;
  for (b = 0; b < n_basic_blocks; b++)
    for (insn = BLOCK_HEAD (b);; insn = NEXT_INSN (insn))
      {
	INSN_BLOCK (insn) = b;
	INSN_LUID (insn) = luid++;

	if (insn == BLOCK_END (b))
	  break;
      }

  /* after reload, remove inter-blocks dependences computed before reload.  */
  if (reload_completed)
    {
      int b;
      rtx insn;

      for (b = 0; b < n_basic_blocks; b++)
	for (insn = BLOCK_HEAD (b);; insn = NEXT_INSN (insn))
	  {
	    rtx link, prev;

	    if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	      {
		prev = NULL_RTX;
		link = LOG_LINKS (insn);
		while (link)
		  {
		    rtx x = XEXP (link, 0);

		    if (INSN_BLOCK (x) != b)
		      {
		        remove_dependence (insn, x);
			link = prev ? XEXP (prev, 1) : LOG_LINKS (insn);
		      }
		    else
		      prev = link, link = XEXP (prev, 1);
		  }
	      }

	    if (insn == BLOCK_END (b))
	      break;
	  }
    }

  nr_regions = 0;
  rgn_table = (region *) alloca ((n_basic_blocks) * sizeof (region));
  rgn_bb_table = (int *) alloca ((n_basic_blocks) * sizeof (int));
  block_to_bb = (int *) alloca ((n_basic_blocks) * sizeof (int));
  containing_rgn = (int *) alloca ((n_basic_blocks) * sizeof (int));

  /* compute regions for scheduling */
  if (reload_completed
      || n_basic_blocks == 1
      || !flag_schedule_interblock)
    {
      find_single_block_region ();
    }
  else
    {
      /* verify that a 'good' control flow graph can be built */
      if (is_cfg_nonregular ())
	{
	  find_single_block_region ();
	}
      else
	{
	  int_list_ptr *s_preds, *s_succs;
	  int *num_preds, *num_succs;
	  sbitmap *dom, *pdom;

	  s_preds = (int_list_ptr *) alloca (n_basic_blocks
					     * sizeof (int_list_ptr));
	  s_succs = (int_list_ptr *) alloca (n_basic_blocks
					     * sizeof (int_list_ptr));
	  num_preds = (int *) alloca (n_basic_blocks * sizeof (int));
	  num_succs = (int *) alloca (n_basic_blocks * sizeof (int));
	  dom = sbitmap_vector_alloc (n_basic_blocks, n_basic_blocks);
	  pdom = sbitmap_vector_alloc (n_basic_blocks, n_basic_blocks);

	  /* The scheduler runs after flow; therefore, we can't blindly call
	     back into find_basic_blocks since doing so could invalidate the
	     info in global_live_at_start.

	     Consider a block consisting entirely of dead stores; after life
	     analysis it would be a block of NOTE_INSN_DELETED notes.  If
	     we call find_basic_blocks again, then the block would be removed
	     entirely and invalidate our the register live information.

	     We could (should?) recompute register live information.  Doing
	     so may even be beneficial.  */

	  compute_preds_succs (s_preds, s_succs, num_preds, num_succs);

	  /* Compute the dominators and post dominators.  We don't currently use
	     post dominators, but we should for speculative motion analysis.  */
	  compute_dominators (dom, pdom, s_preds, s_succs);

	  /* build_control_flow will return nonzero if it detects unreachable
	     blocks or any other irregularity with the cfg which prevents
	     cross block scheduling.  */
	  if (build_control_flow (s_preds, s_succs, num_preds, num_succs) != 0)
	    find_single_block_region ();
	  else
	    find_rgns (s_preds, s_succs, num_preds, num_succs, dom);

	  if (sched_verbose >= 3)
	    debug_regions ();

	  /* For now.  This will move as more and more of haifa is converted
	     to using the cfg code in flow.c  */
	  free_bb_mem ();
	  free (dom);
	  free (pdom);
	}
    }

  /* Allocate data for this pass.  See comments, above,
     for what these vectors do.

     We use xmalloc instead of alloca, because max_uid can be very large
     when there is a lot of function inlining.  If we used alloca, we could
     exceed stack limits on some hosts for some inputs.  */
  insn_priority = (int *) xmalloc (max_uid * sizeof (int));
  insn_reg_weight = (int *) xmalloc (max_uid * sizeof (int));
  insn_tick = (int *) xmalloc (max_uid * sizeof (int));
  insn_costs = (short *) xmalloc (max_uid * sizeof (short));
  insn_units = (short *) xmalloc (max_uid * sizeof (short));
  insn_blockage = (unsigned int *) xmalloc (max_uid * sizeof (unsigned int));
  insn_ref_count = (int *) xmalloc (max_uid * sizeof (int));

  /* Allocate for forward dependencies */
  insn_dep_count = (int *) xmalloc (max_uid * sizeof (int));
  insn_depend = (rtx *) xmalloc (max_uid * sizeof (rtx));

  if (reload_completed == 0)
    {
      int i;

      sched_reg_n_calls_crossed = (int *) alloca (max_regno * sizeof (int));
      sched_reg_live_length = (int *) alloca (max_regno * sizeof (int));
      sched_reg_basic_block = (int *) alloca (max_regno * sizeof (int));
      bb_live_regs = ALLOCA_REG_SET ();
      bzero ((char *) sched_reg_n_calls_crossed, max_regno * sizeof (int));
      bzero ((char *) sched_reg_live_length, max_regno * sizeof (int));

      for (i = 0; i < max_regno; i++)
	sched_reg_basic_block[i] = REG_BLOCK_UNKNOWN;
    }
  else
    {
      sched_reg_n_calls_crossed = 0;
      sched_reg_live_length = 0;
      bb_live_regs = 0;
    }
  init_alias_analysis ();

  if (write_symbols != NO_DEBUG)
    {
      rtx line;

      line_note = (rtx *) xmalloc (max_uid * sizeof (rtx));
      bzero ((char *) line_note, max_uid * sizeof (rtx));
      line_note_head = (rtx *) alloca (n_basic_blocks * sizeof (rtx));
      bzero ((char *) line_note_head, n_basic_blocks * sizeof (rtx));

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

  bzero ((char *) insn_priority, max_uid * sizeof (int));
  bzero ((char *) insn_reg_weight, max_uid * sizeof (int));
  bzero ((char *) insn_tick, max_uid * sizeof (int));
  bzero ((char *) insn_costs, max_uid * sizeof (short));
  bzero ((char *) insn_units, max_uid * sizeof (short));
  bzero ((char *) insn_blockage, max_uid * sizeof (unsigned int));
  bzero ((char *) insn_ref_count, max_uid * sizeof (int));

  /* Initialize for forward dependencies */
  bzero ((char *) insn_depend, max_uid * sizeof (rtx));
  bzero ((char *) insn_dep_count, max_uid * sizeof (int));

  /* Find units used in this fuction, for visualization */
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

  /* Schedule every region in the subroutine */
  for (rgn = 0; rgn < nr_regions; rgn++)
    {
      schedule_region (rgn);

#ifdef USE_C_ALLOCA
      alloca (0);
#endif
    }

  /* Reposition the prologue and epilogue notes in case we moved the
     prologue/epilogue insns.  */
  if (reload_completed)
    reposition_prologue_and_epilogue_notes (get_insns ());

  /* delete redundant line notes.  */
  if (write_symbols != NO_DEBUG)
    rm_redundant_line_notes ();

  /* Update information about uses of registers in the subroutine.  */
  if (reload_completed == 0)
    update_reg_usage ();

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

  free (cant_move);
  free (fed_by_spec_load);
  free (is_load_insn);
  free (insn_orig_block);
  free (insn_luid);

  free (insn_priority);
  free (insn_reg_weight);
  free (insn_tick);
  free (insn_costs);
  free (insn_units);
  free (insn_blockage);
  free (insn_ref_count);

  free (insn_dep_count);
  free (insn_depend);

  if (write_symbols != NO_DEBUG)
    free (line_note);

  if (bb_live_regs)
    FREE_REG_SET (bb_live_regs);

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
}
#endif /* INSN_SCHEDULING */
