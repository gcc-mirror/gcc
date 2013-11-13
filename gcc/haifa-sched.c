/* Instruction scheduling pass.
   Copyright (C) 1992-2013 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com) Enhanced by,
   and currently maintained by, Jim Wilson (wilson@cygnus.com)

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

/* Instruction scheduling pass.  This file, along with sched-deps.c,
   contains the generic parts.  The actual entry point is found for
   the normal instruction scheduling pass is found in sched-rgn.c.

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
   same way as other dependencies, by using insn backward dependences
   INSN_BACK_DEPS.  INSN_BACK_DEPS are translated into forward dependences
   INSN_FORW_DEPS the purpose of forward list scheduling.

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
   reg_n_calls_crossed, and reg_live_length.  Also, BB_HEAD, BB_END.

   The information in the line number notes is carefully retained by
   this pass.  Notes that refer to the starting and ending of
   exception regions are also carefully retained by this pass.  All
   other NOTE insns are grouped in their same relative order at the
   beginning of basic blocks and regions that have been scheduled.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "diagnostic-core.h"
#include "hard-reg-set.h"
#include "rtl.h"
#include "tm_p.h"
#include "regs.h"
#include "function.h"
#include "flags.h"
#include "insn-config.h"
#include "insn-attr.h"
#include "except.h"
#include "recog.h"
#include "sched-int.h"
#include "target.h"
#include "common/common-target.h"
#include "params.h"
#include "dbgcnt.h"
#include "cfgloop.h"
#include "ira.h"
#include "emit-rtl.h"  /* FIXME: Can go away once crtl is moved to rtl.h.  */
#include "hash-table.h"
#include "dumpfile.h"

#ifdef INSN_SCHEDULING

/* True if we do register pressure relief through live-range
   shrinkage.  */
static bool live_range_shrinkage_p;

/* Switch on live range shrinkage.  */
void
initialize_live_range_shrinkage (void)
{
  live_range_shrinkage_p = true;
}

/* Switch off live range shrinkage.  */
void
finish_live_range_shrinkage (void)
{
  live_range_shrinkage_p = false;
}

/* issue_rate is the number of insns that can be scheduled in the same
   machine cycle.  It can be defined in the config/mach/mach.h file,
   otherwise we set it to 1.  */

int issue_rate;

/* This can be set to true by a backend if the scheduler should not
   enable a DCE pass.  */
bool sched_no_dce;

/* The current initiation interval used when modulo scheduling.  */
static int modulo_ii;

/* The maximum number of stages we are prepared to handle.  */
static int modulo_max_stages;

/* The number of insns that exist in each iteration of the loop.  We use this
   to detect when we've scheduled all insns from the first iteration.  */
static int modulo_n_insns;

/* The current count of insns in the first iteration of the loop that have
   already been scheduled.  */
static int modulo_insns_scheduled;

/* The maximum uid of insns from the first iteration of the loop.  */
static int modulo_iter0_max_uid;

/* The number of times we should attempt to backtrack when modulo scheduling.
   Decreased each time we have to backtrack.  */
static int modulo_backtracks_left;

/* The stage in which the last insn from the original loop was
   scheduled.  */
static int modulo_last_stage;

/* sched-verbose controls the amount of debugging output the
   scheduler prints.  It is controlled by -fsched-verbose=N:
   N>0 and no -DSR : the output is directed to stderr.
   N>=10 will direct the printouts to stderr (regardless of -dSR).
   N=1: same as -dSR.
   N=2: bb's probabilities, detailed ready list info, unit/insn info.
   N=3: rtl at abort point, control-flow, regions info.
   N=5: dependences info.  */

int sched_verbose = 0;

/* Debugging file.  All printouts are sent to dump, which is always set,
   either to stderr, or to the dump listing file (-dRS).  */
FILE *sched_dump = 0;

/* This is a placeholder for the scheduler parameters common
   to all schedulers.  */
struct common_sched_info_def *common_sched_info;

#define INSN_TICK(INSN)	(HID (INSN)->tick)
#define INSN_EXACT_TICK(INSN) (HID (INSN)->exact_tick)
#define INSN_TICK_ESTIMATE(INSN) (HID (INSN)->tick_estimate)
#define INTER_TICK(INSN) (HID (INSN)->inter_tick)
#define FEEDS_BACKTRACK_INSN(INSN) (HID (INSN)->feeds_backtrack_insn)
#define SHADOW_P(INSN) (HID (INSN)->shadow_p)
#define MUST_RECOMPUTE_SPEC_P(INSN) (HID (INSN)->must_recompute_spec)
/* Cached cost of the instruction.  Use insn_cost to get cost of the
   insn.  -1 here means that the field is not initialized.  */
#define INSN_COST(INSN)	(HID (INSN)->cost)

/* If INSN_TICK of an instruction is equal to INVALID_TICK,
   then it should be recalculated from scratch.  */
#define INVALID_TICK (-(max_insn_queue_index + 1))
/* The minimal value of the INSN_TICK of an instruction.  */
#define MIN_TICK (-max_insn_queue_index)

/* List of important notes we must keep around.  This is a pointer to the
   last element in the list.  */
rtx note_list;

static struct spec_info_def spec_info_var;
/* Description of the speculative part of the scheduling.
   If NULL - no speculation.  */
spec_info_t spec_info = NULL;

/* True, if recovery block was added during scheduling of current block.
   Used to determine, if we need to fix INSN_TICKs.  */
static bool haifa_recovery_bb_recently_added_p;

/* True, if recovery block was added during this scheduling pass.
   Used to determine if we should have empty memory pools of dependencies
   after finishing current region.  */
bool haifa_recovery_bb_ever_added_p;

/* Counters of different types of speculative instructions.  */
static int nr_begin_data, nr_be_in_data, nr_begin_control, nr_be_in_control;

/* Array used in {unlink, restore}_bb_notes.  */
static rtx *bb_header = 0;

/* Basic block after which recovery blocks will be created.  */
static basic_block before_recovery;

/* Basic block just before the EXIT_BLOCK and after recovery, if we have
   created it.  */
basic_block after_recovery;

/* FALSE if we add bb to another region, so we don't need to initialize it.  */
bool adding_bb_to_current_region_p = true;

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
   insns move from the "Queued" set to the "Ready" list.

   The "Pending" list (P) are the insns in the INSN_FORW_DEPS of the
   unscheduled insns, i.e., those that are ready, queued, and pending.
   The "Queued" set (Q) is implemented by the variable `insn_queue'.
   The "Ready" list (R) is implemented by the variables `ready' and
   `n_ready'.
   The "Scheduled" list (S) is the new insn chain built by this pass.

   The transition (R->S) is implemented in the scheduling loop in
   `schedule_block' when the best insn to schedule is chosen.
   The transitions (P->R and P->Q) are implemented in `schedule_insn' as
   insns move from the ready list to the scheduled list.
   The transition (Q->R) is implemented in 'queue_to_insn' as time
   passes or stalls are introduced.  */

/* Implement a circular buffer to delay instructions until sufficient
   time has passed.  For the new pipeline description interface,
   MAX_INSN_QUEUE_INDEX is a power of two minus one which is not less
   than maximal time of instruction execution computed by genattr.c on
   the base maximal time of functional unit reservations and getting a
   result.  This is the longest time an insn may be queued.  */

static rtx *insn_queue;
static int q_ptr = 0;
static int q_size = 0;
#define NEXT_Q(X) (((X)+1) & max_insn_queue_index)
#define NEXT_Q_AFTER(X, C) (((X)+C) & max_insn_queue_index)

#define QUEUE_SCHEDULED (-3)
#define QUEUE_NOWHERE   (-2)
#define QUEUE_READY     (-1)
/* QUEUE_SCHEDULED - INSN is scheduled.
   QUEUE_NOWHERE   - INSN isn't scheduled yet and is neither in
   queue or ready list.
   QUEUE_READY     - INSN is in ready list.
   N >= 0 - INSN queued for X [where NEXT_Q_AFTER (q_ptr, X) == N] cycles.  */

#define QUEUE_INDEX(INSN) (HID (INSN)->queue_index)

/* The following variable value refers for all current and future
   reservations of the processor units.  */
state_t curr_state;

/* The following variable value is size of memory representing all
   current and future reservations of the processor units.  */
size_t dfa_state_size;

/* The following array is used to find the best insn from ready when
   the automaton pipeline interface is used.  */
char *ready_try = NULL;

/* The ready list.  */
struct ready_list ready = {NULL, 0, 0, 0, 0};

/* The pointer to the ready list (to be removed).  */
static struct ready_list *readyp = &ready;

/* Scheduling clock.  */
static int clock_var;

/* Clock at which the previous instruction was issued.  */
static int last_clock_var;

/* Set to true if, when queuing a shadow insn, we discover that it would be
   scheduled too late.  */
static bool must_backtrack;

/* The following variable value is number of essential insns issued on
   the current cycle.  An insn is essential one if it changes the
   processors state.  */
int cycle_issued_insns;

/* This records the actual schedule.  It is built up during the main phase
   of schedule_block, and afterwards used to reorder the insns in the RTL.  */
static vec<rtx> scheduled_insns;

static int may_trap_exp (const_rtx, int);

/* Nonzero iff the address is comprised from at most 1 register.  */
#define CONST_BASED_ADDRESS_P(x)			\
  (REG_P (x)					\
   || ((GET_CODE (x) == PLUS || GET_CODE (x) == MINUS	\
	|| (GET_CODE (x) == LO_SUM))			\
       && (CONSTANT_P (XEXP (x, 0))			\
	   || CONSTANT_P (XEXP (x, 1)))))

/* Returns a class that insn with GET_DEST(insn)=x may belong to,
   as found by analyzing insn's expression.  */


static int haifa_luid_for_non_insn (rtx x);

/* Haifa version of sched_info hooks common to all headers.  */
const struct common_sched_info_def haifa_common_sched_info =
  {
    NULL, /* fix_recovery_cfg */
    NULL, /* add_block */
    NULL, /* estimate_number_of_insns */
    haifa_luid_for_non_insn, /* luid_for_non_insn */
    SCHED_PASS_UNKNOWN /* sched_pass_id */
  };

/* Mapping from instruction UID to its Logical UID.  */
vec<int> sched_luids = vNULL;

/* Next LUID to assign to an instruction.  */
int sched_max_luid = 1;

/* Haifa Instruction Data.  */
vec<haifa_insn_data_def> h_i_d = vNULL;

void (* sched_init_only_bb) (basic_block, basic_block);

/* Split block function.  Different schedulers might use different functions
   to handle their internal data consistent.  */
basic_block (* sched_split_block) (basic_block, rtx);

/* Create empty basic block after the specified block.  */
basic_block (* sched_create_empty_bb) (basic_block);

/* Return the number of cycles until INSN is expected to be ready.
   Return zero if it already is.  */
static int
insn_delay (rtx insn)
{
  return MAX (INSN_TICK (insn) - clock_var, 0);
}

static int
may_trap_exp (const_rtx x, int is_store)
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

/* Classifies rtx X of an insn for the purpose of verifying that X can be
   executed speculatively (and consequently the insn can be moved
   speculatively), by examining X, returning:
   TRAP_RISKY: store, or risky non-load insn (e.g. division by variable).
   TRAP_FREE: non-load insn.
   IFREE: load from a globally safe location.
   IRISKY: volatile load.
   PFREE_CANDIDATE, PRISKY_CANDIDATE: load that need to be checked for
   being either PFREE or PRISKY.  */

static int
haifa_classify_rtx (const_rtx x)
{
  int tmp_class = TRAP_FREE;
  int insn_class = TRAP_FREE;
  enum rtx_code code;

  if (GET_CODE (x) == PARALLEL)
    {
      int i, len = XVECLEN (x, 0);

      for (i = len - 1; i >= 0; i--)
	{
	  tmp_class = haifa_classify_rtx (XVECEXP (x, 0, i));
	  insn_class = WORST_CLASS (insn_class, tmp_class);
	  if (insn_class == TRAP_RISKY || insn_class == IRISKY)
	    break;
	}
    }
  else
    {
      code = GET_CODE (x);
      switch (code)
	{
	case CLOBBER:
	  /* Test if it is a 'store'.  */
	  tmp_class = may_trap_exp (XEXP (x, 0), 1);
	  break;
	case SET:
	  /* Test if it is a store.  */
	  tmp_class = may_trap_exp (SET_DEST (x), 1);
	  if (tmp_class == TRAP_RISKY)
	    break;
	  /* Test if it is a load.  */
	  tmp_class =
	    WORST_CLASS (tmp_class,
			 may_trap_exp (SET_SRC (x), 0));
	  break;
	case COND_EXEC:
	  tmp_class = haifa_classify_rtx (COND_EXEC_CODE (x));
	  if (tmp_class == TRAP_RISKY)
	    break;
	  tmp_class = WORST_CLASS (tmp_class,
				   may_trap_exp (COND_EXEC_TEST (x), 0));
	  break;
	case TRAP_IF:
	  tmp_class = TRAP_RISKY;
	  break;
	default:;
	}
      insn_class = tmp_class;
    }

  return insn_class;
}

int
haifa_classify_insn (const_rtx insn)
{
  return haifa_classify_rtx (PATTERN (insn));
}

/* After the scheduler initialization function has been called, this function
   can be called to enable modulo scheduling.  II is the initiation interval
   we should use, it affects the delays for delay_pairs that were recorded as
   separated by a given number of stages.

   MAX_STAGES provides us with a limit
   after which we give up scheduling; the caller must have unrolled at least
   as many copies of the loop body and recorded delay_pairs for them.
   
   INSNS is the number of real (non-debug) insns in one iteration of
   the loop.  MAX_UID can be used to test whether an insn belongs to
   the first iteration of the loop; all of them have a uid lower than
   MAX_UID.  */
void
set_modulo_params (int ii, int max_stages, int insns, int max_uid)
{
  modulo_ii = ii;
  modulo_max_stages = max_stages;
  modulo_n_insns = insns;
  modulo_iter0_max_uid = max_uid;
  modulo_backtracks_left = PARAM_VALUE (PARAM_MAX_MODULO_BACKTRACK_ATTEMPTS);
}

/* A structure to record a pair of insns where the first one is a real
   insn that has delay slots, and the second is its delayed shadow.
   I1 is scheduled normally and will emit an assembly instruction,
   while I2 describes the side effect that takes place at the
   transition between cycles CYCLES and (CYCLES + 1) after I1.  */
struct delay_pair
{
  struct delay_pair *next_same_i1;
  rtx i1, i2;
  int cycles;
  /* When doing modulo scheduling, we a delay_pair can also be used to
     show that I1 and I2 are the same insn in a different stage.  If that
     is the case, STAGES will be nonzero.  */
  int stages;
};

/* Helpers for delay hashing.  */

struct delay_i1_hasher : typed_noop_remove <delay_pair>
{
  typedef delay_pair value_type;
  typedef void compare_type;
  static inline hashval_t hash (const value_type *);
  static inline bool equal (const value_type *, const compare_type *);
};

/* Returns a hash value for X, based on hashing just I1.  */

inline hashval_t
delay_i1_hasher::hash (const value_type *x)
{
  return htab_hash_pointer (x->i1);
}

/* Return true if I1 of pair X is the same as that of pair Y.  */

inline bool
delay_i1_hasher::equal (const value_type *x, const compare_type *y)
{
  return x->i1 == y;
}

struct delay_i2_hasher : typed_free_remove <delay_pair>
{
  typedef delay_pair value_type;
  typedef void compare_type;
  static inline hashval_t hash (const value_type *);
  static inline bool equal (const value_type *, const compare_type *);
};

/* Returns a hash value for X, based on hashing just I2.  */

inline hashval_t
delay_i2_hasher::hash (const value_type *x)
{
  return htab_hash_pointer (x->i2);
}

/* Return true if I2 of pair X is the same as that of pair Y.  */

inline bool
delay_i2_hasher::equal (const value_type *x, const compare_type *y)
{
  return x->i2 == y;
}

/* Two hash tables to record delay_pairs, one indexed by I1 and the other
   indexed by I2.  */
static hash_table <delay_i1_hasher> delay_htab;
static hash_table <delay_i2_hasher> delay_htab_i2;

/* Called through htab_traverse.  Walk the hashtable using I2 as
   index, and delete all elements involving an UID higher than
   that pointed to by *DATA.  */
int
haifa_htab_i2_traverse (delay_pair **slot, int *data)
{
  int maxuid = *data;
  struct delay_pair *p = *slot;
  if (INSN_UID (p->i2) >= maxuid || INSN_UID (p->i1) >= maxuid)
    {
      delay_htab_i2.clear_slot (slot);
    }
  return 1;
}

/* Called through htab_traverse.  Walk the hashtable using I2 as
   index, and delete all elements involving an UID higher than
   that pointed to by *DATA.  */
int
haifa_htab_i1_traverse (delay_pair **pslot, int *data)
{
  int maxuid = *data;
  struct delay_pair *p, *first, **pprev;

  if (INSN_UID ((*pslot)->i1) >= maxuid)
    {
      delay_htab.clear_slot (pslot);
      return 1;
    }
  pprev = &first;
  for (p = *pslot; p; p = p->next_same_i1)
    {
      if (INSN_UID (p->i2) < maxuid)
	{
	  *pprev = p;
	  pprev = &p->next_same_i1;
	}
    }
  *pprev = NULL;
  if (first == NULL)
    delay_htab.clear_slot (pslot);
  else
    *pslot = first;
  return 1;
}

/* Discard all delay pairs which involve an insn with an UID higher
   than MAX_UID.  */
void
discard_delay_pairs_above (int max_uid)
{
  delay_htab.traverse <int *, haifa_htab_i1_traverse> (&max_uid);
  delay_htab_i2.traverse <int *, haifa_htab_i2_traverse> (&max_uid);
}

/* This function can be called by a port just before it starts the final
   scheduling pass.  It records the fact that an instruction with delay
   slots has been split into two insns, I1 and I2.  The first one will be
   scheduled normally and initiates the operation.  The second one is a
   shadow which must follow a specific number of cycles after I1; its only
   purpose is to show the side effect that occurs at that cycle in the RTL.
   If a JUMP_INSN or a CALL_INSN has been split, I1 should be a normal INSN,
   while I2 retains the original insn type.

   There are two ways in which the number of cycles can be specified,
   involving the CYCLES and STAGES arguments to this function.  If STAGES
   is zero, we just use the value of CYCLES.  Otherwise, STAGES is a factor
   which is multiplied by MODULO_II to give the number of cycles.  This is
   only useful if the caller also calls set_modulo_params to enable modulo
   scheduling.  */

void
record_delay_slot_pair (rtx i1, rtx i2, int cycles, int stages)
{
  struct delay_pair *p = XNEW (struct delay_pair);
  struct delay_pair **slot;

  p->i1 = i1;
  p->i2 = i2;
  p->cycles = cycles;
  p->stages = stages;

  if (!delay_htab.is_created ())
    {
      delay_htab.create (10);
      delay_htab_i2.create (10);
    }
  slot = delay_htab.find_slot_with_hash (i1, htab_hash_pointer (i1), INSERT);
  p->next_same_i1 = *slot;
  *slot = p;
  slot = delay_htab_i2.find_slot_with_hash (i2, htab_hash_pointer (i2), INSERT);
  *slot = p;
}

/* Examine the delay pair hashtable to see if INSN is a shadow for another,
   and return the other insn if so.  Return NULL otherwise.  */
rtx
real_insn_for_shadow (rtx insn)
{
  struct delay_pair *pair;

  if (!delay_htab.is_created ())
    return NULL_RTX;

  pair = delay_htab_i2.find_with_hash (insn, htab_hash_pointer (insn));
  if (!pair || pair->stages > 0)
    return NULL_RTX;
  return pair->i1;
}

/* For a pair P of insns, return the fixed distance in cycles from the first
   insn after which the second must be scheduled.  */
static int
pair_delay (struct delay_pair *p)
{
  if (p->stages == 0)
    return p->cycles;
  else
    return p->stages * modulo_ii;
}

/* Given an insn INSN, add a dependence on its delayed shadow if it
   has one.  Also try to find situations where shadows depend on each other
   and add dependencies to the real insns to limit the amount of backtracking
   needed.  */
void
add_delay_dependencies (rtx insn)
{
  struct delay_pair *pair;
  sd_iterator_def sd_it;
  dep_t dep;

  if (!delay_htab.is_created ())
    return;

  pair = delay_htab_i2.find_with_hash (insn, htab_hash_pointer (insn));
  if (!pair)
    return;
  add_dependence (insn, pair->i1, REG_DEP_ANTI);
  if (pair->stages)
    return;

  FOR_EACH_DEP (pair->i2, SD_LIST_BACK, sd_it, dep)
    {
      rtx pro = DEP_PRO (dep);
      struct delay_pair *other_pair
	= delay_htab_i2.find_with_hash (pro, htab_hash_pointer (pro));
      if (!other_pair || other_pair->stages)
	continue;
      if (pair_delay (other_pair) >= pair_delay (pair))
	{
	  if (sched_verbose >= 4)
	    {
	      fprintf (sched_dump, ";;\tadding dependence %d <- %d\n",
		       INSN_UID (other_pair->i1),
		       INSN_UID (pair->i1));
	      fprintf (sched_dump, ";;\tpair1 %d <- %d, cost %d\n",
		       INSN_UID (pair->i1),
		       INSN_UID (pair->i2),
		       pair_delay (pair));
	      fprintf (sched_dump, ";;\tpair2 %d <- %d, cost %d\n",
		       INSN_UID (other_pair->i1),
		       INSN_UID (other_pair->i2),
		       pair_delay (other_pair));
	    }
	  add_dependence (pair->i1, other_pair->i1, REG_DEP_ANTI);
	}
    }
}

/* Forward declarations.  */

static int priority (rtx);
static int rank_for_schedule (const void *, const void *);
static void swap_sort (rtx *, int);
static void queue_insn (rtx, int, const char *);
static int schedule_insn (rtx);
static void adjust_priority (rtx);
static void advance_one_cycle (void);
static void extend_h_i_d (void);


/* Notes handling mechanism:
   =========================
   Generally, NOTES are saved before scheduling and restored after scheduling.
   The scheduler distinguishes between two types of notes:

   (1) LOOP_BEGIN, LOOP_END, SETJMP, EHREGION_BEG, EHREGION_END notes:
   Before scheduling a region, a pointer to the note is added to the insn
   that follows or precedes it.  (This happens as part of the data dependence
   computation).  After scheduling an insn, the pointer contained in it is
   used for regenerating the corresponding note (in reemit_notes).

   (2) All other notes (e.g. INSN_DELETED):  Before scheduling a block,
   these notes are put in a list (in rm_other_notes() and
   unlink_other_notes ()).  After scheduling the block, these notes are
   inserted at the beginning of the block (in schedule_block()).  */

static void ready_add (struct ready_list *, rtx, bool);
static rtx ready_remove_first (struct ready_list *);
static rtx ready_remove_first_dispatch (struct ready_list *ready);

static void queue_to_ready (struct ready_list *);
static int early_queue_to_ready (state_t, struct ready_list *);

static void debug_ready_list (struct ready_list *);

/* The following functions are used to implement multi-pass scheduling
   on the first cycle.  */
static rtx ready_remove (struct ready_list *, int);
static void ready_remove_insn (rtx);

static void fix_inter_tick (rtx, rtx);
static int fix_tick_ready (rtx);
static void change_queue_index (rtx, int);

/* The following functions are used to implement scheduling of data/control
   speculative instructions.  */

static void extend_h_i_d (void);
static void init_h_i_d (rtx);
static int haifa_speculate_insn (rtx, ds_t, rtx *);
static void generate_recovery_code (rtx);
static void process_insn_forw_deps_be_in_spec (rtx, rtx, ds_t);
static void begin_speculative_block (rtx);
static void add_to_speculative_block (rtx);
static void init_before_recovery (basic_block *);
static void create_check_block_twin (rtx, bool);
static void fix_recovery_deps (basic_block);
static bool haifa_change_pattern (rtx, rtx);
static void dump_new_block_header (int, basic_block, rtx, rtx);
static void restore_bb_notes (basic_block);
static void fix_jump_move (rtx);
static void move_block_after_check (rtx);
static void move_succs (vec<edge, va_gc> **, basic_block);
static void sched_remove_insn (rtx);
static void clear_priorities (rtx, rtx_vec_t *);
static void calc_priorities (rtx_vec_t);
static void add_jump_dependencies (rtx, rtx);

#endif /* INSN_SCHEDULING */

/* Point to state used for the current scheduling pass.  */
struct haifa_sched_info *current_sched_info;

#ifndef INSN_SCHEDULING
void
schedule_insns (void)
{
}
#else

/* Do register pressure sensitive insn scheduling if the flag is set
   up.  */
enum sched_pressure_algorithm sched_pressure;

/* Map regno -> its pressure class.  The map defined only when
   SCHED_PRESSURE != SCHED_PRESSURE_NONE.  */
enum reg_class *sched_regno_pressure_class;

/* The current register pressure.  Only elements corresponding pressure
   classes are defined.  */
static int curr_reg_pressure[N_REG_CLASSES];

/* Saved value of the previous array.  */
static int saved_reg_pressure[N_REG_CLASSES];

/* Register living at given scheduling point.  */
static bitmap curr_reg_live;

/* Saved value of the previous array.  */
static bitmap saved_reg_live;

/* Registers mentioned in the current region.  */
static bitmap region_ref_regs;

/* Initiate register pressure relative info for scheduling the current
   region.  Currently it is only clearing register mentioned in the
   current region.  */
void
sched_init_region_reg_pressure_info (void)
{
  bitmap_clear (region_ref_regs);
}

/* PRESSURE[CL] describes the pressure on register class CL.  Update it
   for the birth (if BIRTH_P) or death (if !BIRTH_P) of register REGNO.
   LIVE tracks the set of live registers; if it is null, assume that
   every birth or death is genuine.  */
static inline void
mark_regno_birth_or_death (bitmap live, int *pressure, int regno, bool birth_p)
{
  enum reg_class pressure_class;

  pressure_class = sched_regno_pressure_class[regno];
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      if (pressure_class != NO_REGS)
	{
	  if (birth_p)
	    {
	      if (!live || bitmap_set_bit (live, regno))
		pressure[pressure_class]
		  += (ira_reg_class_max_nregs
		      [pressure_class][PSEUDO_REGNO_MODE (regno)]);
	    }
	  else
	    {
	      if (!live || bitmap_clear_bit (live, regno))
		pressure[pressure_class]
		  -= (ira_reg_class_max_nregs
		      [pressure_class][PSEUDO_REGNO_MODE (regno)]);
	    }
	}
    }
  else if (pressure_class != NO_REGS
	   && ! TEST_HARD_REG_BIT (ira_no_alloc_regs, regno))
    {
      if (birth_p)
	{
	  if (!live || bitmap_set_bit (live, regno))
	    pressure[pressure_class]++;
	}
      else
	{
	  if (!live || bitmap_clear_bit (live, regno))
	    pressure[pressure_class]--;
	}
    }
}

/* Initiate current register pressure related info from living
   registers given by LIVE.  */
static void
initiate_reg_pressure_info (bitmap live)
{
  int i;
  unsigned int j;
  bitmap_iterator bi;

  for (i = 0; i < ira_pressure_classes_num; i++)
    curr_reg_pressure[ira_pressure_classes[i]] = 0;
  bitmap_clear (curr_reg_live);
  EXECUTE_IF_SET_IN_BITMAP (live, 0, j, bi)
    if (sched_pressure == SCHED_PRESSURE_MODEL
	|| current_nr_blocks == 1
	|| bitmap_bit_p (region_ref_regs, j))
      mark_regno_birth_or_death (curr_reg_live, curr_reg_pressure, j, true);
}

/* Mark registers in X as mentioned in the current region.  */
static void
setup_ref_regs (rtx x)
{
  int i, j, regno;
  const RTX_CODE code = GET_CODE (x);
  const char *fmt;

  if (REG_P (x))
    {
      regno = REGNO (x);
      if (HARD_REGISTER_NUM_P (regno))
	bitmap_set_range (region_ref_regs, regno,
			  hard_regno_nregs[regno][GET_MODE (x)]);
      else
	bitmap_set_bit (region_ref_regs, REGNO (x));
      return;
    }
  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    if (fmt[i] == 'e')
      setup_ref_regs (XEXP (x, i));
    else if (fmt[i] == 'E')
      {
	for (j = 0; j < XVECLEN (x, i); j++)
	  setup_ref_regs (XVECEXP (x, i, j));
      }
}

/* Initiate current register pressure related info at the start of
   basic block BB.  */
static void
initiate_bb_reg_pressure_info (basic_block bb)
{
  unsigned int i ATTRIBUTE_UNUSED;
  rtx insn;

  if (current_nr_blocks > 1)
    FOR_BB_INSNS (bb, insn)
      if (NONDEBUG_INSN_P (insn))
	setup_ref_regs (PATTERN (insn));
  initiate_reg_pressure_info (df_get_live_in (bb));
#ifdef EH_RETURN_DATA_REGNO
  if (bb_has_eh_pred (bb))
    for (i = 0; ; ++i)
      {
	unsigned int regno = EH_RETURN_DATA_REGNO (i);

	if (regno == INVALID_REGNUM)
	  break;
	if (! bitmap_bit_p (df_get_live_in (bb), regno))
	  mark_regno_birth_or_death (curr_reg_live, curr_reg_pressure,
				     regno, true);
      }
#endif
}

/* Save current register pressure related info.  */
static void
save_reg_pressure (void)
{
  int i;

  for (i = 0; i < ira_pressure_classes_num; i++)
    saved_reg_pressure[ira_pressure_classes[i]]
      = curr_reg_pressure[ira_pressure_classes[i]];
  bitmap_copy (saved_reg_live, curr_reg_live);
}

/* Restore saved register pressure related info.  */
static void
restore_reg_pressure (void)
{
  int i;

  for (i = 0; i < ira_pressure_classes_num; i++)
    curr_reg_pressure[ira_pressure_classes[i]]
      = saved_reg_pressure[ira_pressure_classes[i]];
  bitmap_copy (curr_reg_live, saved_reg_live);
}

/* Return TRUE if the register is dying after its USE.  */
static bool
dying_use_p (struct reg_use_data *use)
{
  struct reg_use_data *next;

  for (next = use->next_regno_use; next != use; next = next->next_regno_use)
    if (NONDEBUG_INSN_P (next->insn)
	&& QUEUE_INDEX (next->insn) != QUEUE_SCHEDULED)
      return false;
  return true;
}

/* Print info about the current register pressure and its excess for
   each pressure class.  */
static void
print_curr_reg_pressure (void)
{
  int i;
  enum reg_class cl;

  fprintf (sched_dump, ";;\t");
  for (i = 0; i < ira_pressure_classes_num; i++)
    {
      cl = ira_pressure_classes[i];
      gcc_assert (curr_reg_pressure[cl] >= 0);
      fprintf (sched_dump, "  %s:%d(%d)", reg_class_names[cl],
	       curr_reg_pressure[cl],
	       curr_reg_pressure[cl] - ira_class_hard_regs_num[cl]);
    }
  fprintf (sched_dump, "\n");
}

/* Determine if INSN has a condition that is clobbered if a register
   in SET_REGS is modified.  */
static bool
cond_clobbered_p (rtx insn, HARD_REG_SET set_regs)
{
  rtx pat = PATTERN (insn);
  gcc_assert (GET_CODE (pat) == COND_EXEC);
  if (TEST_HARD_REG_BIT (set_regs, REGNO (XEXP (COND_EXEC_TEST (pat), 0))))
    {
      sd_iterator_def sd_it;
      dep_t dep;
      haifa_change_pattern (insn, ORIG_PAT (insn));
      FOR_EACH_DEP (insn, SD_LIST_BACK, sd_it, dep)
	DEP_STATUS (dep) &= ~DEP_CANCELLED;
      TODO_SPEC (insn) = HARD_DEP;
      if (sched_verbose >= 2)
	fprintf (sched_dump,
		 ";;\t\tdequeue insn %s because of clobbered condition\n",
		 (*current_sched_info->print_insn) (insn, 0));
      return true;
    }

  return false;
}

/* This function should be called after modifying the pattern of INSN,
   to update scheduler data structures as needed.  */
static void
update_insn_after_change (rtx insn)
{
  sd_iterator_def sd_it;
  dep_t dep;

  dfa_clear_single_insn_cache (insn);

  sd_it = sd_iterator_start (insn,
			     SD_LIST_FORW | SD_LIST_BACK | SD_LIST_RES_BACK);
  while (sd_iterator_cond (&sd_it, &dep))
    {
      DEP_COST (dep) = UNKNOWN_DEP_COST;
      sd_iterator_next (&sd_it);
    }

  /* Invalidate INSN_COST, so it'll be recalculated.  */
  INSN_COST (insn) = -1;
  /* Invalidate INSN_TICK, so it'll be recalculated.  */
  INSN_TICK (insn) = INVALID_TICK;
}


/* Two VECs, one to hold dependencies for which pattern replacements
   need to be applied or restored at the start of the next cycle, and
   another to hold an integer that is either one, to apply the
   corresponding replacement, or zero to restore it.  */
static vec<dep_t> next_cycle_replace_deps;
static vec<int> next_cycle_apply;

static void apply_replacement (dep_t, bool);
static void restore_pattern (dep_t, bool);

/* Look at the remaining dependencies for insn NEXT, and compute and return
   the TODO_SPEC value we should use for it.  This is called after one of
   NEXT's dependencies has been resolved.
   We also perform pattern replacements for predication, and for broken
   replacement dependencies.  The latter is only done if FOR_BACKTRACK is
   false.  */

static ds_t
recompute_todo_spec (rtx next, bool for_backtrack)
{
  ds_t new_ds;
  sd_iterator_def sd_it;
  dep_t dep, modify_dep = NULL;
  int n_spec = 0;
  int n_control = 0;
  int n_replace = 0;
  bool first_p = true;

  if (sd_lists_empty_p (next, SD_LIST_BACK))
    /* NEXT has all its dependencies resolved.  */
    return 0;

  if (!sd_lists_empty_p (next, SD_LIST_HARD_BACK))
    return HARD_DEP;

  /* Now we've got NEXT with speculative deps only.
     1. Look at the deps to see what we have to do.
     2. Check if we can do 'todo'.  */
  new_ds = 0;

  FOR_EACH_DEP (next, SD_LIST_BACK, sd_it, dep)
    {
      rtx pro = DEP_PRO (dep);
      ds_t ds = DEP_STATUS (dep) & SPECULATIVE;

      if (DEBUG_INSN_P (pro) && !DEBUG_INSN_P (next))
	continue;

      if (ds)
	{
	  n_spec++;
	  if (first_p)
	    {
	      first_p = false;

	      new_ds = ds;
	    }
	  else
	    new_ds = ds_merge (new_ds, ds);
	}
      else if (DEP_TYPE (dep) == REG_DEP_CONTROL)
	{
	  if (QUEUE_INDEX (pro) != QUEUE_SCHEDULED)
	    {
	      n_control++;
	      modify_dep = dep;
	    }
	  DEP_STATUS (dep) &= ~DEP_CANCELLED;
	}
      else if (DEP_REPLACE (dep) != NULL)
	{
	  if (QUEUE_INDEX (pro) != QUEUE_SCHEDULED)
	    {
	      n_replace++;
	      modify_dep = dep;
	    }
	  DEP_STATUS (dep) &= ~DEP_CANCELLED;
	}
    }

  if (n_replace > 0 && n_control == 0 && n_spec == 0)
    {
      if (!dbg_cnt (sched_breakdep))
	return HARD_DEP;
      FOR_EACH_DEP (next, SD_LIST_BACK, sd_it, dep)
	{
	  struct dep_replacement *desc = DEP_REPLACE (dep);
	  if (desc != NULL)
	    {
	      if (desc->insn == next && !for_backtrack)
		{
		  gcc_assert (n_replace == 1);
		  apply_replacement (dep, true);
		}
	      DEP_STATUS (dep) |= DEP_CANCELLED;
	    }
	}
      return 0;
    }
  
  else if (n_control == 1 && n_replace == 0 && n_spec == 0)
    {
      rtx pro, other, new_pat;
      rtx cond = NULL_RTX;
      bool success;
      rtx prev = NULL_RTX;
      int i;
      unsigned regno;
  
      if ((current_sched_info->flags & DO_PREDICATION) == 0
	  || (ORIG_PAT (next) != NULL_RTX
	      && PREDICATED_PAT (next) == NULL_RTX))
	return HARD_DEP;

      pro = DEP_PRO (modify_dep);
      other = real_insn_for_shadow (pro);
      if (other != NULL_RTX)
	pro = other;

      cond = sched_get_reverse_condition_uncached (pro);
      regno = REGNO (XEXP (cond, 0));

      /* Find the last scheduled insn that modifies the condition register.
	 We can stop looking once we find the insn we depend on through the
	 REG_DEP_CONTROL; if the condition register isn't modified after it,
	 we know that it still has the right value.  */
      if (QUEUE_INDEX (pro) == QUEUE_SCHEDULED)
	FOR_EACH_VEC_ELT_REVERSE (scheduled_insns, i, prev)
	  {
	    HARD_REG_SET t;

	    find_all_hard_reg_sets (prev, &t);
	    if (TEST_HARD_REG_BIT (t, regno))
	      return HARD_DEP;
	    if (prev == pro)
	      break;
	  }
      if (ORIG_PAT (next) == NULL_RTX)
	{
	  ORIG_PAT (next) = PATTERN (next);

	  new_pat = gen_rtx_COND_EXEC (VOIDmode, cond, PATTERN (next));
	  success = haifa_change_pattern (next, new_pat);
	  if (!success)
	    return HARD_DEP;
	  PREDICATED_PAT (next) = new_pat;
	}
      else if (PATTERN (next) != PREDICATED_PAT (next))
	{
	  bool success = haifa_change_pattern (next,
					       PREDICATED_PAT (next));
	  gcc_assert (success);
	}
      DEP_STATUS (modify_dep) |= DEP_CANCELLED;
      return DEP_CONTROL;
    }

  if (PREDICATED_PAT (next) != NULL_RTX)
    {
      int tick = INSN_TICK (next);
      bool success = haifa_change_pattern (next,
					   ORIG_PAT (next));
      INSN_TICK (next) = tick;
      gcc_assert (success);
    }

  /* We can't handle the case where there are both speculative and control
     dependencies, so we return HARD_DEP in such a case.  Also fail if
     we have speculative dependencies with not enough points, or more than
     one control dependency.  */
  if ((n_spec > 0 && (n_control > 0 || n_replace > 0))
      || (n_spec > 0
	  /* Too few points?  */
	  && ds_weak (new_ds) < spec_info->data_weakness_cutoff)
      || n_control > 0
      || n_replace > 0)
    return HARD_DEP;

  return new_ds;
}

/* Pointer to the last instruction scheduled.  */
static rtx last_scheduled_insn;

/* Pointer to the last nondebug instruction scheduled within the
   block, or the prev_head of the scheduling block.  Used by
   rank_for_schedule, so that insns independent of the last scheduled
   insn will be preferred over dependent instructions.  */
static rtx last_nondebug_scheduled_insn;

/* Pointer that iterates through the list of unscheduled insns if we
   have a dbg_cnt enabled.  It always points at an insn prior to the
   first unscheduled one.  */
static rtx nonscheduled_insns_begin;

/* Compute cost of executing INSN.
   This is the number of cycles between instruction issue and
   instruction results.  */
int
insn_cost (rtx insn)
{
  int cost;

  if (sel_sched_p ())
    {
      if (recog_memoized (insn) < 0)
	return 0;

      cost = insn_default_latency (insn);
      if (cost < 0)
	cost = 0;

      return cost;
    }

  cost = INSN_COST (insn);

  if (cost < 0)
    {
      /* A USE insn, or something else we don't need to
	 understand.  We can't pass these directly to
	 result_ready_cost or insn_default_latency because it will
	 trigger a fatal error for unrecognizable insns.  */
      if (recog_memoized (insn) < 0)
	{
	  INSN_COST (insn) = 0;
	  return 0;
	}
      else
	{
	  cost = insn_default_latency (insn);
	  if (cost < 0)
	    cost = 0;

	  INSN_COST (insn) = cost;
	}
    }

  return cost;
}

/* Compute cost of dependence LINK.
   This is the number of cycles between instruction issue and
   instruction results.
   ??? We also use this function to call recog_memoized on all insns.  */
int
dep_cost_1 (dep_t link, dw_t dw)
{
  rtx insn = DEP_PRO (link);
  rtx used = DEP_CON (link);
  int cost;

  if (DEP_COST (link) != UNKNOWN_DEP_COST)
    return DEP_COST (link);

  if (delay_htab.is_created ())
    {
      struct delay_pair *delay_entry;
      delay_entry
	= delay_htab_i2.find_with_hash (used, htab_hash_pointer (used));
      if (delay_entry)
	{
	  if (delay_entry->i1 == insn)
	    {
	      DEP_COST (link) = pair_delay (delay_entry);
	      return DEP_COST (link);
	    }
	}
    }

  /* A USE insn should never require the value used to be computed.
     This allows the computation of a function's result and parameter
     values to overlap the return and call.  We don't care about the
     dependence cost when only decreasing register pressure.  */
  if (recog_memoized (used) < 0)
    {
      cost = 0;
      recog_memoized (insn);
    }
  else
    {
      enum reg_note dep_type = DEP_TYPE (link);

      cost = insn_cost (insn);

      if (INSN_CODE (insn) >= 0)
	{
	  if (dep_type == REG_DEP_ANTI)
	    cost = 0;
	  else if (dep_type == REG_DEP_OUTPUT)
	    {
	      cost = (insn_default_latency (insn)
		      - insn_default_latency (used));
	      if (cost <= 0)
		cost = 1;
	    }
	  else if (bypass_p (insn))
	    cost = insn_latency (insn, used);
	}


      if (targetm.sched.adjust_cost_2)
	cost = targetm.sched.adjust_cost_2 (used, (int) dep_type, insn, cost,
					    dw);
      else if (targetm.sched.adjust_cost != NULL)
	{
	  /* This variable is used for backward compatibility with the
	     targets.  */
	  rtx dep_cost_rtx_link = alloc_INSN_LIST (NULL_RTX, NULL_RTX);

	  /* Make it self-cycled, so that if some tries to walk over this
	     incomplete list he/she will be caught in an endless loop.  */
	  XEXP (dep_cost_rtx_link, 1) = dep_cost_rtx_link;

	  /* Targets use only REG_NOTE_KIND of the link.  */
	  PUT_REG_NOTE_KIND (dep_cost_rtx_link, DEP_TYPE (link));

	  cost = targetm.sched.adjust_cost (used, dep_cost_rtx_link,
					    insn, cost);

	  free_INSN_LIST_node (dep_cost_rtx_link);
	}

      if (cost < 0)
	cost = 0;
    }

  DEP_COST (link) = cost;
  return cost;
}

/* Compute cost of dependence LINK.
   This is the number of cycles between instruction issue and
   instruction results.  */
int
dep_cost (dep_t link)
{
  return dep_cost_1 (link, 0);
}

/* Use this sel-sched.c friendly function in reorder2 instead of increasing
   INSN_PRIORITY explicitly.  */
void
increase_insn_priority (rtx insn, int amount)
{
  if (!sel_sched_p ())
    {
      /* We're dealing with haifa-sched.c INSN_PRIORITY.  */
      if (INSN_PRIORITY_KNOWN (insn))
	  INSN_PRIORITY (insn) += amount;
    }
  else
    {
      /* In sel-sched.c INSN_PRIORITY is not kept up to date.
	 Use EXPR_PRIORITY instead. */
      sel_add_to_insn_priority (insn, amount);
    }
}

/* Return 'true' if DEP should be included in priority calculations.  */
static bool
contributes_to_priority_p (dep_t dep)
{
  if (DEBUG_INSN_P (DEP_CON (dep))
      || DEBUG_INSN_P (DEP_PRO (dep)))
    return false;

  /* Critical path is meaningful in block boundaries only.  */
  if (!current_sched_info->contributes_to_priority (DEP_CON (dep),
						    DEP_PRO (dep)))
    return false;

  if (DEP_REPLACE (dep) != NULL)
    return false;

  /* If flag COUNT_SPEC_IN_CRITICAL_PATH is set,
     then speculative instructions will less likely be
     scheduled.  That is because the priority of
     their producers will increase, and, thus, the
     producers will more likely be scheduled, thus,
     resolving the dependence.  */
  if (sched_deps_info->generate_spec_deps
      && !(spec_info->flags & COUNT_SPEC_IN_CRITICAL_PATH)
      && (DEP_STATUS (dep) & SPECULATIVE))
    return false;

  return true;
}

/* Compute the number of nondebug deps in list LIST for INSN.  */

static int
dep_list_size (rtx insn, sd_list_types_def list)
{
  sd_iterator_def sd_it;
  dep_t dep;
  int dbgcount = 0, nodbgcount = 0;

  if (!MAY_HAVE_DEBUG_INSNS)
    return sd_lists_size (insn, list);

  FOR_EACH_DEP (insn, list, sd_it, dep)
    {
      if (DEBUG_INSN_P (DEP_CON (dep)))
	dbgcount++;
      else if (!DEBUG_INSN_P (DEP_PRO (dep)))
	nodbgcount++;
    }

  gcc_assert (dbgcount + nodbgcount == sd_lists_size (insn, list));

  return nodbgcount;
}

/* Compute the priority number for INSN.  */
static int
priority (rtx insn)
{
  if (! INSN_P (insn))
    return 0;

  /* We should not be interested in priority of an already scheduled insn.  */
  gcc_assert (QUEUE_INDEX (insn) != QUEUE_SCHEDULED);

  if (!INSN_PRIORITY_KNOWN (insn))
    {
      int this_priority = -1;

      if (dep_list_size (insn, SD_LIST_FORW) == 0)
	/* ??? We should set INSN_PRIORITY to insn_cost when and insn has
	   some forward deps but all of them are ignored by
	   contributes_to_priority hook.  At the moment we set priority of
	   such insn to 0.  */
	this_priority = insn_cost (insn);
      else
	{
	  rtx prev_first, twin;
	  basic_block rec;

	  /* For recovery check instructions we calculate priority slightly
	     different than that of normal instructions.  Instead of walking
	     through INSN_FORW_DEPS (check) list, we walk through
	     INSN_FORW_DEPS list of each instruction in the corresponding
	     recovery block.  */

          /* Selective scheduling does not define RECOVERY_BLOCK macro.  */
	  rec = sel_sched_p () ? NULL : RECOVERY_BLOCK (insn);
	  if (!rec || rec == EXIT_BLOCK_PTR)
	    {
	      prev_first = PREV_INSN (insn);
	      twin = insn;
	    }
	  else
	    {
	      prev_first = NEXT_INSN (BB_HEAD (rec));
	      twin = PREV_INSN (BB_END (rec));
	    }

	  do
	    {
	      sd_iterator_def sd_it;
	      dep_t dep;

	      FOR_EACH_DEP (twin, SD_LIST_FORW, sd_it, dep)
		{
		  rtx next;
		  int next_priority;

		  next = DEP_CON (dep);

		  if (BLOCK_FOR_INSN (next) != rec)
		    {
		      int cost;

		      if (!contributes_to_priority_p (dep))
			continue;

		      if (twin == insn)
			cost = dep_cost (dep);
		      else
			{
			  struct _dep _dep1, *dep1 = &_dep1;

			  init_dep (dep1, insn, next, REG_DEP_ANTI);

			  cost = dep_cost (dep1);
			}

		      next_priority = cost + priority (next);

		      if (next_priority > this_priority)
			this_priority = next_priority;
		    }
		}

	      twin = PREV_INSN (twin);
	    }
	  while (twin != prev_first);
	}

      if (this_priority < 0)
	{
	  gcc_assert (this_priority == -1);

	  this_priority = insn_cost (insn);
	}

      INSN_PRIORITY (insn) = this_priority;
      INSN_PRIORITY_STATUS (insn) = 1;
    }

  return INSN_PRIORITY (insn);
}

/* Macros and functions for keeping the priority queue sorted, and
   dealing with queuing and dequeuing of instructions.  */

#define SCHED_SORT(READY, N_READY)                                   \
do { if ((N_READY) == 2)				             \
       swap_sort (READY, N_READY);			             \
     else if ((N_READY) > 2)                                         \
         qsort (READY, N_READY, sizeof (rtx), rank_for_schedule); }  \
while (0)

/* For each pressure class CL, set DEATH[CL] to the number of registers
   in that class that die in INSN.  */

static void
calculate_reg_deaths (rtx insn, int *death)
{
  int i;
  struct reg_use_data *use;

  for (i = 0; i < ira_pressure_classes_num; i++)
    death[ira_pressure_classes[i]] = 0;
  for (use = INSN_REG_USE_LIST (insn); use != NULL; use = use->next_insn_use)
    if (dying_use_p (use))
      mark_regno_birth_or_death (0, death, use->regno, true);
}

/* Setup info about the current register pressure impact of scheduling
   INSN at the current scheduling point.  */
static void
setup_insn_reg_pressure_info (rtx insn)
{
  int i, change, before, after, hard_regno;
  int excess_cost_change;
  enum machine_mode mode;
  enum reg_class cl;
  struct reg_pressure_data *pressure_info;
  int *max_reg_pressure;
  static int death[N_REG_CLASSES];

  gcc_checking_assert (!DEBUG_INSN_P (insn));

  excess_cost_change = 0;
  calculate_reg_deaths (insn, death);
  pressure_info = INSN_REG_PRESSURE (insn);
  max_reg_pressure = INSN_MAX_REG_PRESSURE (insn);
  gcc_assert (pressure_info != NULL && max_reg_pressure != NULL);
  for (i = 0; i < ira_pressure_classes_num; i++)
    {
      cl = ira_pressure_classes[i];
      gcc_assert (curr_reg_pressure[cl] >= 0);
      change = (int) pressure_info[i].set_increase - death[cl];
      before = MAX (0, max_reg_pressure[i] - ira_class_hard_regs_num[cl]);
      after = MAX (0, max_reg_pressure[i] + change
		   - ira_class_hard_regs_num[cl]);
      hard_regno = ira_class_hard_regs[cl][0];
      gcc_assert (hard_regno >= 0);
      mode = reg_raw_mode[hard_regno];
      excess_cost_change += ((after - before)
			     * (ira_memory_move_cost[mode][cl][0]
				+ ira_memory_move_cost[mode][cl][1]));
    }
  INSN_REG_PRESSURE_EXCESS_COST_CHANGE (insn) = excess_cost_change;
}

/* This is the first page of code related to SCHED_PRESSURE_MODEL.
   It tries to make the scheduler take register pressure into account
   without introducing too many unnecessary stalls.  It hooks into the
   main scheduling algorithm at several points:

    - Before scheduling starts, model_start_schedule constructs a
      "model schedule" for the current block.  This model schedule is
      chosen solely to keep register pressure down.  It does not take the
      target's pipeline or the original instruction order into account,
      except as a tie-breaker.  It also doesn't work to a particular
      pressure limit.

      This model schedule gives us an idea of what pressure can be
      achieved for the block and gives us an example of a schedule that
      keeps to that pressure.  It also makes the final schedule less
      dependent on the original instruction order.  This is important
      because the original order can either be "wide" (many values live
      at once, such as in user-scheduled code) or "narrow" (few values
      live at once, such as after loop unrolling, where several
      iterations are executed sequentially).

      We do not apply this model schedule to the rtx stream.  We simply
      record it in model_schedule.  We also compute the maximum pressure,
      MP, that was seen during this schedule.

    - Instructions are added to the ready queue even if they require
      a stall.  The length of the stall is instead computed as:

	 MAX (INSN_TICK (INSN) - clock_var, 0)

      (= insn_delay).  This allows rank_for_schedule to choose between
      introducing a deliberate stall or increasing pressure.

    - Before sorting the ready queue, model_set_excess_costs assigns
      a pressure-based cost to each ready instruction in the queue.
      This is the instruction's INSN_REG_PRESSURE_EXCESS_COST_CHANGE
      (ECC for short) and is effectively measured in cycles.

    - rank_for_schedule ranks instructions based on:

	ECC (insn) + insn_delay (insn)

      then as:

	insn_delay (insn)

      So, for example, an instruction X1 with an ECC of 1 that can issue
      now will win over an instruction X0 with an ECC of zero that would
      introduce a stall of one cycle.  However, an instruction X2 with an
      ECC of 2 that can issue now will lose to both X0 and X1.

    - When an instruction is scheduled, model_recompute updates the model
      schedule with the new pressures (some of which might now exceed the
      original maximum pressure MP).  model_update_limit_points then searches
      for the new point of maximum pressure, if not already known.  */

/* Used to separate high-verbosity debug information for SCHED_PRESSURE_MODEL
   from surrounding debug information.  */
#define MODEL_BAR \
  ";;\t\t+------------------------------------------------------\n"

/* Information about the pressure on a particular register class at a
   particular point of the model schedule.  */
struct model_pressure_data {
  /* The pressure at this point of the model schedule, or -1 if the
     point is associated with an instruction that has already been
     scheduled.  */
  int ref_pressure;

  /* The maximum pressure during or after this point of the model schedule.  */
  int max_pressure;
};

/* Per-instruction information that is used while building the model
   schedule.  Here, "schedule" refers to the model schedule rather
   than the main schedule.  */
struct model_insn_info {
  /* The instruction itself.  */
  rtx insn;

  /* If this instruction is in model_worklist, these fields link to the
     previous (higher-priority) and next (lower-priority) instructions
     in the list.  */
  struct model_insn_info *prev;
  struct model_insn_info *next;

  /* While constructing the schedule, QUEUE_INDEX describes whether an
     instruction has already been added to the schedule (QUEUE_SCHEDULED),
     is in model_worklist (QUEUE_READY), or neither (QUEUE_NOWHERE).
     old_queue records the value that QUEUE_INDEX had before scheduling
     started, so that we can restore it once the schedule is complete.  */
  int old_queue;

  /* The relative importance of an unscheduled instruction.  Higher
     values indicate greater importance.  */
  unsigned int model_priority;

  /* The length of the longest path of satisfied true dependencies
     that leads to this instruction.  */
  unsigned int depth;

  /* The length of the longest path of dependencies of any kind
     that leads from this instruction.  */
  unsigned int alap;

  /* The number of predecessor nodes that must still be scheduled.  */
  int unscheduled_preds;
};

/* Information about the pressure limit for a particular register class.
   This structure is used when applying a model schedule to the main
   schedule.  */
struct model_pressure_limit {
  /* The maximum register pressure seen in the original model schedule.  */
  int orig_pressure;

  /* The maximum register pressure seen in the current model schedule
     (which excludes instructions that have already been scheduled).  */
  int pressure;

  /* The point of the current model schedule at which PRESSURE is first
     reached.  It is set to -1 if the value needs to be recomputed.  */
  int point;
};

/* Describes a particular way of measuring register pressure.  */
struct model_pressure_group {
  /* Index PCI describes the maximum pressure on ira_pressure_classes[PCI].  */
  struct model_pressure_limit limits[N_REG_CLASSES];

  /* Index (POINT * ira_num_pressure_classes + PCI) describes the pressure
     on register class ira_pressure_classes[PCI] at point POINT of the
     current model schedule.  A POINT of model_num_insns describes the
     pressure at the end of the schedule.  */
  struct model_pressure_data *model;
};

/* Index POINT gives the instruction at point POINT of the model schedule.
   This array doesn't change during main scheduling.  */
static vec<rtx> model_schedule;

/* The list of instructions in the model worklist, sorted in order of
   decreasing priority.  */
static struct model_insn_info *model_worklist;

/* Index I describes the instruction with INSN_LUID I.  */
static struct model_insn_info *model_insns;

/* The number of instructions in the model schedule.  */
static int model_num_insns;

/* The index of the first instruction in model_schedule that hasn't yet been
   added to the main schedule, or model_num_insns if all of them have.  */
static int model_curr_point;

/* Describes the pressure before each instruction in the model schedule.  */
static struct model_pressure_group model_before_pressure;

/* The first unused model_priority value (as used in model_insn_info).  */
static unsigned int model_next_priority;


/* The model_pressure_data for ira_pressure_classes[PCI] in GROUP
   at point POINT of the model schedule.  */
#define MODEL_PRESSURE_DATA(GROUP, POINT, PCI) \
  (&(GROUP)->model[(POINT) * ira_pressure_classes_num + (PCI)])

/* The maximum pressure on ira_pressure_classes[PCI] in GROUP at or
   after point POINT of the model schedule.  */
#define MODEL_MAX_PRESSURE(GROUP, POINT, PCI) \
  (MODEL_PRESSURE_DATA (GROUP, POINT, PCI)->max_pressure)

/* The pressure on ira_pressure_classes[PCI] in GROUP at point POINT
   of the model schedule.  */
#define MODEL_REF_PRESSURE(GROUP, POINT, PCI) \
  (MODEL_PRESSURE_DATA (GROUP, POINT, PCI)->ref_pressure)

/* Information about INSN that is used when creating the model schedule.  */
#define MODEL_INSN_INFO(INSN) \
  (&model_insns[INSN_LUID (INSN)])

/* The instruction at point POINT of the model schedule.  */
#define MODEL_INSN(POINT) \
  (model_schedule[POINT])


/* Return INSN's index in the model schedule, or model_num_insns if it
   doesn't belong to that schedule.  */

static int
model_index (rtx insn)
{
  if (INSN_MODEL_INDEX (insn) == 0)
    return model_num_insns;
  return INSN_MODEL_INDEX (insn) - 1;
}

/* Make sure that GROUP->limits is up-to-date for the current point
   of the model schedule.  */

static void
model_update_limit_points_in_group (struct model_pressure_group *group)
{
  int pci, max_pressure, point;

  for (pci = 0; pci < ira_pressure_classes_num; pci++)
    {
      /* We may have passed the final point at which the pressure in
	 group->limits[pci].pressure was reached.  Update the limit if so.  */
      max_pressure = MODEL_MAX_PRESSURE (group, model_curr_point, pci);
      group->limits[pci].pressure = max_pressure;

      /* Find the point at which MAX_PRESSURE is first reached.  We need
	 to search in three cases:

	 - We've already moved past the previous pressure point.
	   In this case we search forward from model_curr_point.

	 - We scheduled the previous point of maximum pressure ahead of
	   its position in the model schedule, but doing so didn't bring
	   the pressure point earlier.  In this case we search forward
	   from that previous pressure point.

	 - Scheduling an instruction early caused the maximum pressure
	   to decrease.  In this case we will have set the pressure
	   point to -1, and we search forward from model_curr_point.  */
      point = MAX (group->limits[pci].point, model_curr_point);
      while (point < model_num_insns
	     && MODEL_REF_PRESSURE (group, point, pci) < max_pressure)
	point++;
      group->limits[pci].point = point;

      gcc_assert (MODEL_REF_PRESSURE (group, point, pci) == max_pressure);
      gcc_assert (MODEL_MAX_PRESSURE (group, point, pci) == max_pressure);
    }
}

/* Make sure that all register-pressure limits are up-to-date for the
   current position in the model schedule.  */

static void
model_update_limit_points (void)
{
  model_update_limit_points_in_group (&model_before_pressure);
}

/* Return the model_index of the last unscheduled use in chain USE
   outside of USE's instruction.  Return -1 if there are no other uses,
   or model_num_insns if the register is live at the end of the block.  */

static int
model_last_use_except (struct reg_use_data *use)
{
  struct reg_use_data *next;
  int last, index;

  last = -1;
  for (next = use->next_regno_use; next != use; next = next->next_regno_use)
    if (NONDEBUG_INSN_P (next->insn)
	&& QUEUE_INDEX (next->insn) != QUEUE_SCHEDULED)
      {
	index = model_index (next->insn);
	if (index == model_num_insns)
	  return model_num_insns;
	if (last < index)
	  last = index;
      }
  return last;
}

/* An instruction with model_index POINT has just been scheduled, and it
   adds DELTA to the pressure on ira_pressure_classes[PCI] after POINT - 1.
   Update MODEL_REF_PRESSURE (GROUP, POINT, PCI) and
   MODEL_MAX_PRESSURE (GROUP, POINT, PCI) accordingly.  */

static void
model_start_update_pressure (struct model_pressure_group *group,
			     int point, int pci, int delta)
{
  int next_max_pressure;

  if (point == model_num_insns)
    {
      /* The instruction wasn't part of the model schedule; it was moved
	 from a different block.  Update the pressure for the end of
	 the model schedule.  */
      MODEL_REF_PRESSURE (group, point, pci) += delta;
      MODEL_MAX_PRESSURE (group, point, pci) += delta;
    }
  else
    {
      /* Record that this instruction has been scheduled.  Nothing now
	 changes between POINT and POINT + 1, so get the maximum pressure
	 from the latter.  If the maximum pressure decreases, the new
	 pressure point may be before POINT.  */
      MODEL_REF_PRESSURE (group, point, pci) = -1;
      next_max_pressure = MODEL_MAX_PRESSURE (group, point + 1, pci);
      if (MODEL_MAX_PRESSURE (group, point, pci) > next_max_pressure)
	{
	  MODEL_MAX_PRESSURE (group, point, pci) = next_max_pressure;
	  if (group->limits[pci].point == point)
	    group->limits[pci].point = -1;
	}
    }
}

/* Record that scheduling a later instruction has changed the pressure
   at point POINT of the model schedule by DELTA (which might be 0).
   Update GROUP accordingly.  Return nonzero if these changes might
   trigger changes to previous points as well.  */

static int
model_update_pressure (struct model_pressure_group *group,
		       int point, int pci, int delta)
{
  int ref_pressure, max_pressure, next_max_pressure;

  /* If POINT hasn't yet been scheduled, update its pressure.  */
  ref_pressure = MODEL_REF_PRESSURE (group, point, pci);
  if (ref_pressure >= 0 && delta != 0)
    {
      ref_pressure += delta;
      MODEL_REF_PRESSURE (group, point, pci) = ref_pressure;

      /* Check whether the maximum pressure in the overall schedule
	 has increased.  (This means that the MODEL_MAX_PRESSURE of
	 every point <= POINT will need to increae too; see below.)  */
      if (group->limits[pci].pressure < ref_pressure)
	group->limits[pci].pressure = ref_pressure;

      /* If we are at maximum pressure, and the maximum pressure
	 point was previously unknown or later than POINT,
	 bring it forward.  */
      if (group->limits[pci].pressure == ref_pressure
	  && !IN_RANGE (group->limits[pci].point, 0, point))
	group->limits[pci].point = point;

      /* If POINT used to be the point of maximum pressure, but isn't
	 any longer, we need to recalculate it using a forward walk.  */
      if (group->limits[pci].pressure > ref_pressure
	  && group->limits[pci].point == point)
	group->limits[pci].point = -1;
    }

  /* Update the maximum pressure at POINT.  Changes here might also
     affect the maximum pressure at POINT - 1.  */
  next_max_pressure = MODEL_MAX_PRESSURE (group, point + 1, pci);
  max_pressure = MAX (ref_pressure, next_max_pressure);
  if (MODEL_MAX_PRESSURE (group, point, pci) != max_pressure)
    {
      MODEL_MAX_PRESSURE (group, point, pci) = max_pressure;
      return 1;
    }
  return 0;
}

/* INSN has just been scheduled.  Update the model schedule accordingly.  */

static void
model_recompute (rtx insn)
{
  struct {
    int last_use;
    int regno;
  } uses[FIRST_PSEUDO_REGISTER + MAX_RECOG_OPERANDS];
  struct reg_use_data *use;
  struct reg_pressure_data *reg_pressure;
  int delta[N_REG_CLASSES];
  int pci, point, mix, new_last, cl, ref_pressure, queue;
  unsigned int i, num_uses, num_pending_births;
  bool print_p;

  /* The destinations of INSN were previously live from POINT onwards, but are
     now live from model_curr_point onwards.  Set up DELTA accordingly.  */
  point = model_index (insn);
  reg_pressure = INSN_REG_PRESSURE (insn);
  for (pci = 0; pci < ira_pressure_classes_num; pci++)
    {
      cl = ira_pressure_classes[pci];
      delta[cl] = reg_pressure[pci].set_increase;
    }

  /* Record which registers previously died at POINT, but which now die
     before POINT.  Adjust DELTA so that it represents the effect of
     this change after POINT - 1.  Set NUM_PENDING_BIRTHS to the number of
     registers that will be born in the range [model_curr_point, POINT).  */
  num_uses = 0;
  num_pending_births = 0;
  for (use = INSN_REG_USE_LIST (insn); use != NULL; use = use->next_insn_use)
    {
      new_last = model_last_use_except (use);
      if (new_last < point)
	{
	  gcc_assert (num_uses < ARRAY_SIZE (uses));
	  uses[num_uses].last_use = new_last;
	  uses[num_uses].regno = use->regno;
	  /* This register is no longer live after POINT - 1.  */
	  mark_regno_birth_or_death (NULL, delta, use->regno, false);
	  num_uses++;
	  if (new_last >= 0)
	    num_pending_births++;
	}
    }

  /* Update the MODEL_REF_PRESSURE and MODEL_MAX_PRESSURE for POINT.
     Also set each group pressure limit for POINT.  */
  for (pci = 0; pci < ira_pressure_classes_num; pci++)
    {
      cl = ira_pressure_classes[pci];
      model_start_update_pressure (&model_before_pressure,
				   point, pci, delta[cl]);
    }

  /* Walk the model schedule backwards, starting immediately before POINT.  */
  print_p = false;
  if (point != model_curr_point)
    do
      {
	point--;
	insn = MODEL_INSN (point);
	queue = QUEUE_INDEX (insn);

	if (queue != QUEUE_SCHEDULED)
	  {
	    /* DELTA describes the effect of the move on the register pressure
	       after POINT.  Make it describe the effect on the pressure
	       before POINT.  */
	    i = 0;
	    while (i < num_uses)
	      {
		if (uses[i].last_use == point)
		  {
		    /* This register is now live again.  */
		    mark_regno_birth_or_death (NULL, delta,
					       uses[i].regno, true);

		    /* Remove this use from the array.  */
		    uses[i] = uses[num_uses - 1];
		    num_uses--;
		    num_pending_births--;
		  }
		else
		  i++;
	      }

	    if (sched_verbose >= 5)
	      {
		if (!print_p)
		  {
		    fprintf (sched_dump, MODEL_BAR);
		    fprintf (sched_dump, ";;\t\t| New pressure for model"
			     " schedule\n");
		    fprintf (sched_dump, MODEL_BAR);
		    print_p = true;
		  }

		fprintf (sched_dump, ";;\t\t| %3d %4d %-30s ",
			 point, INSN_UID (insn),
			 str_pattern_slim (PATTERN (insn)));
		for (pci = 0; pci < ira_pressure_classes_num; pci++)
		  {
		    cl = ira_pressure_classes[pci];
		    ref_pressure = MODEL_REF_PRESSURE (&model_before_pressure,
						       point, pci);
		    fprintf (sched_dump, " %s:[%d->%d]",
			     reg_class_names[ira_pressure_classes[pci]],
			     ref_pressure, ref_pressure + delta[cl]);
		  }
		fprintf (sched_dump, "\n");
	      }
	  }

	/* Adjust the pressure at POINT.  Set MIX to nonzero if POINT - 1
	   might have changed as well.  */
	mix = num_pending_births;
	for (pci = 0; pci < ira_pressure_classes_num; pci++)
	  {
	    cl = ira_pressure_classes[pci];
	    mix |= delta[cl];
	    mix |= model_update_pressure (&model_before_pressure,
					  point, pci, delta[cl]);
	  }
      }
    while (mix && point > model_curr_point);

  if (print_p)
    fprintf (sched_dump, MODEL_BAR);
}

/* After DEP, which was cancelled, has been resolved for insn NEXT,
   check whether the insn's pattern needs restoring.  */
static bool
must_restore_pattern_p (rtx next, dep_t dep)
{
  if (QUEUE_INDEX (next) == QUEUE_SCHEDULED)
    return false;

  if (DEP_TYPE (dep) == REG_DEP_CONTROL)
    {
      gcc_assert (ORIG_PAT (next) != NULL_RTX);
      gcc_assert (next == DEP_CON (dep));
    }
  else
    {
      struct dep_replacement *desc = DEP_REPLACE (dep);
      if (desc->insn != next)
	{
	  gcc_assert (*desc->loc == desc->orig);
	  return false;
	}
    }
  return true;
}

/* model_spill_cost (CL, P, P') returns the cost of increasing the
   pressure on CL from P to P'.  We use this to calculate a "base ECC",
   baseECC (CL, X), for each pressure class CL and each instruction X.
   Supposing X changes the pressure on CL from P to P', and that the
   maximum pressure on CL in the current model schedule is MP', then:

   * if X occurs before or at the next point of maximum pressure in
     the model schedule and P' > MP', then:

       baseECC (CL, X) = model_spill_cost (CL, MP, P')

     The idea is that the pressure after scheduling a fixed set of
     instructions -- in this case, the set up to and including the
     next maximum pressure point -- is going to be the same regardless
     of the order; we simply want to keep the intermediate pressure
     under control.  Thus X has a cost of zero unless scheduling it
     now would exceed MP'.

     If all increases in the set are by the same amount, no zero-cost
     instruction will ever cause the pressure to exceed MP'.  However,
     if X is instead moved past an instruction X' with pressure in the
     range (MP' - (P' - P), MP'), the pressure at X' will increase
     beyond MP'.  Since baseECC is very much a heuristic anyway,
     it doesn't seem worth the overhead of tracking cases like these.

     The cost of exceeding MP' is always based on the original maximum
     pressure MP.  This is so that going 2 registers over the original
     limit has the same cost regardless of whether it comes from two
     separate +1 deltas or from a single +2 delta.

   * if X occurs after the next point of maximum pressure in the model
     schedule and P' > P, then:

       baseECC (CL, X) = model_spill_cost (CL, MP, MP' + (P' - P))

     That is, if we move X forward across a point of maximum pressure,
     and if X increases the pressure by P' - P, then we conservatively
     assume that scheduling X next would increase the maximum pressure
     by P' - P.  Again, the cost of doing this is based on the original
     maximum pressure MP, for the same reason as above.

   * if P' < P, P > MP, and X occurs at or after the next point of
     maximum pressure, then:

       baseECC (CL, X) = -model_spill_cost (CL, MAX (MP, P'), P)

     That is, if we have already exceeded the original maximum pressure MP,
     and if X might reduce the maximum pressure again -- or at least push
     it further back, and thus allow more scheduling freedom -- it is given
     a negative cost to reflect the improvement.

   * otherwise,

       baseECC (CL, X) = 0

     In this case, X is not expected to affect the maximum pressure MP',
     so it has zero cost.

   We then create a combined value baseECC (X) that is the sum of
   baseECC (CL, X) for each pressure class CL.

   baseECC (X) could itself be used as the ECC value described above.
   However, this is often too conservative, in the sense that it
   tends to make high-priority instructions that increase pressure
   wait too long in cases where introducing a spill would be better.
   For this reason the final ECC is a priority-adjusted form of
   baseECC (X).  Specifically, we calculate:

     P (X) = INSN_PRIORITY (X) - insn_delay (X) - baseECC (X)
     baseP = MAX { P (X) | baseECC (X) <= 0 }

   Then:

     ECC (X) = MAX (MIN (baseP - P (X), baseECC (X)), 0)

   Thus an instruction's effect on pressure is ignored if it has a high
   enough priority relative to the ones that don't increase pressure.
   Negative values of baseECC (X) do not increase the priority of X
   itself, but they do make it harder for other instructions to
   increase the pressure further.

   This pressure cost is deliberately timid.  The intention has been
   to choose a heuristic that rarely interferes with the normal list
   scheduler in cases where that scheduler would produce good code.
   We simply want to curb some of its worst excesses.  */

/* Return the cost of increasing the pressure in class CL from FROM to TO.

   Here we use the very simplistic cost model that every register above
   ira_class_hard_regs_num[CL] has a spill cost of 1.  We could use other
   measures instead, such as one based on MEMORY_MOVE_COST.  However:

      (1) In order for an instruction to be scheduled, the higher cost
	  would need to be justified in a single saving of that many stalls.
	  This is overly pessimistic, because the benefit of spilling is
	  often to avoid a sequence of several short stalls rather than
	  a single long one.

      (2) The cost is still arbitrary.  Because we are not allocating
	  registers during scheduling, we have no way of knowing for
	  sure how many memory accesses will be required by each spill,
	  where the spills will be placed within the block, or even
	  which block(s) will contain the spills.

   So a higher cost than 1 is often too conservative in practice,
   forcing blocks to contain unnecessary stalls instead of spill code.
   The simple cost below seems to be the best compromise.  It reduces
   the interference with the normal list scheduler, which helps make
   it more suitable for a default-on option.  */

static int
model_spill_cost (int cl, int from, int to)
{
  from = MAX (from, ira_class_hard_regs_num[cl]);
  return MAX (to, from) - from;
}

/* Return baseECC (ira_pressure_classes[PCI], POINT), given that
   P = curr_reg_pressure[ira_pressure_classes[PCI]] and that
   P' = P + DELTA.  */

static int
model_excess_group_cost (struct model_pressure_group *group,
			 int point, int pci, int delta)
{
  int pressure, cl;

  cl = ira_pressure_classes[pci];
  if (delta < 0 && point >= group->limits[pci].point)
    {
      pressure = MAX (group->limits[pci].orig_pressure,
		      curr_reg_pressure[cl] + delta);
      return -model_spill_cost (cl, pressure, curr_reg_pressure[cl]);
    }

  if (delta > 0)
    {
      if (point > group->limits[pci].point)
	pressure = group->limits[pci].pressure + delta;
      else
	pressure = curr_reg_pressure[cl] + delta;

      if (pressure > group->limits[pci].pressure)
	return model_spill_cost (cl, group->limits[pci].orig_pressure,
				 pressure);
    }

  return 0;
}

/* Return baseECC (MODEL_INSN (INSN)).  Dump the costs to sched_dump
   if PRINT_P.  */

static int
model_excess_cost (rtx insn, bool print_p)
{
  int point, pci, cl, cost, this_cost, delta;
  struct reg_pressure_data *insn_reg_pressure;
  int insn_death[N_REG_CLASSES];

  calculate_reg_deaths (insn, insn_death);
  point = model_index (insn);
  insn_reg_pressure = INSN_REG_PRESSURE (insn);
  cost = 0;

  if (print_p)
    fprintf (sched_dump, ";;\t\t| %3d %4d | %4d %+3d |", point,
	     INSN_UID (insn), INSN_PRIORITY (insn), insn_delay (insn));

  /* Sum up the individual costs for each register class.  */
  for (pci = 0; pci < ira_pressure_classes_num; pci++)
    {
      cl = ira_pressure_classes[pci];
      delta = insn_reg_pressure[pci].set_increase - insn_death[cl];
      this_cost = model_excess_group_cost (&model_before_pressure,
					   point, pci, delta);
      cost += this_cost;
      if (print_p)
	fprintf (sched_dump, " %s:[%d base cost %d]",
		 reg_class_names[cl], delta, this_cost);
    }

  if (print_p)
    fprintf (sched_dump, "\n");

  return cost;
}

/* Dump the next points of maximum pressure for GROUP.  */

static void
model_dump_pressure_points (struct model_pressure_group *group)
{
  int pci, cl;

  fprintf (sched_dump, ";;\t\t|  pressure points");
  for (pci = 0; pci < ira_pressure_classes_num; pci++)
    {
      cl = ira_pressure_classes[pci];
      fprintf (sched_dump, " %s:[%d->%d at ", reg_class_names[cl],
	       curr_reg_pressure[cl], group->limits[pci].pressure);
      if (group->limits[pci].point < model_num_insns)
	fprintf (sched_dump, "%d:%d]", group->limits[pci].point,
		 INSN_UID (MODEL_INSN (group->limits[pci].point)));
      else
	fprintf (sched_dump, "end]");
    }
  fprintf (sched_dump, "\n");
}

/* Set INSN_REG_PRESSURE_EXCESS_COST_CHANGE for INSNS[0...COUNT-1].  */

static void
model_set_excess_costs (rtx *insns, int count)
{
  int i, cost, priority_base, priority;
  bool print_p;

  /* Record the baseECC value for each instruction in the model schedule,
     except that negative costs are converted to zero ones now rather thatn
     later.  Do not assign a cost to debug instructions, since they must
     not change code-generation decisions.  Experiments suggest we also
     get better results by not assigning a cost to instructions from
     a different block.

     Set PRIORITY_BASE to baseP in the block comment above.  This is the
     maximum priority of the "cheap" instructions, which should always
     include the next model instruction.  */
  priority_base = 0;
  print_p = false;
  for (i = 0; i < count; i++)
    if (INSN_MODEL_INDEX (insns[i]))
      {
	if (sched_verbose >= 6 && !print_p)
	  {
	    fprintf (sched_dump, MODEL_BAR);
	    fprintf (sched_dump, ";;\t\t| Pressure costs for ready queue\n");
	    model_dump_pressure_points (&model_before_pressure);
	    fprintf (sched_dump, MODEL_BAR);
	    print_p = true;
	  }
	cost = model_excess_cost (insns[i], print_p);
	if (cost <= 0)
	  {
	    priority = INSN_PRIORITY (insns[i]) - insn_delay (insns[i]) - cost;
	    priority_base = MAX (priority_base, priority);
	    cost = 0;
	  }
	INSN_REG_PRESSURE_EXCESS_COST_CHANGE (insns[i]) = cost;
      }
  if (print_p)
    fprintf (sched_dump, MODEL_BAR);

  /* Use MAX (baseECC, 0) and baseP to calculcate ECC for each
     instruction.  */
  for (i = 0; i < count; i++)
    {
      cost = INSN_REG_PRESSURE_EXCESS_COST_CHANGE (insns[i]);
      priority = INSN_PRIORITY (insns[i]) - insn_delay (insns[i]);
      if (cost > 0 && priority > priority_base)
	{
	  cost += priority_base - priority;
	  INSN_REG_PRESSURE_EXCESS_COST_CHANGE (insns[i]) = MAX (cost, 0);
	}
    }
}

/* Returns a positive value if x is preferred; returns a negative value if
   y is preferred.  Should never return 0, since that will make the sort
   unstable.  */

static int
rank_for_schedule (const void *x, const void *y)
{
  rtx tmp = *(const rtx *) y;
  rtx tmp2 = *(const rtx *) x;
  int tmp_class, tmp2_class;
  int val, priority_val, info_val, diff;

  if (MAY_HAVE_DEBUG_INSNS)
    {
      /* Schedule debug insns as early as possible.  */
      if (DEBUG_INSN_P (tmp) && !DEBUG_INSN_P (tmp2))
	return -1;
      else if (!DEBUG_INSN_P (tmp) && DEBUG_INSN_P (tmp2))
	return 1;
      else if (DEBUG_INSN_P (tmp) && DEBUG_INSN_P (tmp2))
	return INSN_LUID (tmp) - INSN_LUID (tmp2);
    }

  if (live_range_shrinkage_p)
    {
      /* Don't use SCHED_PRESSURE_MODEL -- it results in much worse
	 code.  */
      gcc_assert (sched_pressure == SCHED_PRESSURE_WEIGHTED);
      if ((INSN_REG_PRESSURE_EXCESS_COST_CHANGE (tmp) < 0
	   || INSN_REG_PRESSURE_EXCESS_COST_CHANGE (tmp2) < 0)
	  && (diff = (INSN_REG_PRESSURE_EXCESS_COST_CHANGE (tmp)
		      - INSN_REG_PRESSURE_EXCESS_COST_CHANGE (tmp2))) != 0)
	return diff;
      /* Sort by INSN_LUID (original insn order), so that we make the
	 sort stable.  This minimizes instruction movement, thus
	 minimizing sched's effect on debugging and cross-jumping.  */
      return INSN_LUID (tmp) - INSN_LUID (tmp2);
    }

  /* The insn in a schedule group should be issued the first.  */
  if (flag_sched_group_heuristic &&
      SCHED_GROUP_P (tmp) != SCHED_GROUP_P (tmp2))
    return SCHED_GROUP_P (tmp2) ? 1 : -1;

  /* Make sure that priority of TMP and TMP2 are initialized.  */
  gcc_assert (INSN_PRIORITY_KNOWN (tmp) && INSN_PRIORITY_KNOWN (tmp2));

  if (sched_pressure != SCHED_PRESSURE_NONE)
    {
      /* Prefer insn whose scheduling results in the smallest register
	 pressure excess.  */
      if ((diff = (INSN_REG_PRESSURE_EXCESS_COST_CHANGE (tmp)
		   + insn_delay (tmp)
		   - INSN_REG_PRESSURE_EXCESS_COST_CHANGE (tmp2)
		   - insn_delay (tmp2))))
	return diff;
    }

  if (sched_pressure != SCHED_PRESSURE_NONE
      && (INSN_TICK (tmp2) > clock_var || INSN_TICK (tmp) > clock_var))
    {
      if (INSN_TICK (tmp) <= clock_var)
	return -1;
      else if (INSN_TICK (tmp2) <= clock_var)
	return 1;
      else
	return INSN_TICK (tmp) - INSN_TICK (tmp2);
    }

  /* If we are doing backtracking in this schedule, prefer insns that
     have forward dependencies with negative cost against an insn that
     was already scheduled.  */
  if (current_sched_info->flags & DO_BACKTRACKING)
    {
      priority_val = FEEDS_BACKTRACK_INSN (tmp2) - FEEDS_BACKTRACK_INSN (tmp);
      if (priority_val)
	return priority_val;
    }

  /* Prefer insn with higher priority.  */
  priority_val = INSN_PRIORITY (tmp2) - INSN_PRIORITY (tmp);

  if (flag_sched_critical_path_heuristic && priority_val)
    return priority_val;

  /* Prefer speculative insn with greater dependencies weakness.  */
  if (flag_sched_spec_insn_heuristic && spec_info)
    {
      ds_t ds1, ds2;
      dw_t dw1, dw2;
      int dw;

      ds1 = TODO_SPEC (tmp) & SPECULATIVE;
      if (ds1)
	dw1 = ds_weak (ds1);
      else
	dw1 = NO_DEP_WEAK;

      ds2 = TODO_SPEC (tmp2) & SPECULATIVE;
      if (ds2)
	dw2 = ds_weak (ds2);
      else
	dw2 = NO_DEP_WEAK;

      dw = dw2 - dw1;
      if (dw > (NO_DEP_WEAK / 8) || dw < -(NO_DEP_WEAK / 8))
	return dw;
    }

  info_val = (*current_sched_info->rank) (tmp, tmp2);
  if (flag_sched_rank_heuristic && info_val)
    return info_val;

  /* Compare insns based on their relation to the last scheduled
     non-debug insn.  */
  if (flag_sched_last_insn_heuristic && last_nondebug_scheduled_insn)
    {
      dep_t dep1;
      dep_t dep2;
      rtx last = last_nondebug_scheduled_insn;

      /* Classify the instructions into three classes:
         1) Data dependent on last schedule insn.
         2) Anti/Output dependent on last scheduled insn.
         3) Independent of last scheduled insn, or has latency of one.
         Choose the insn from the highest numbered class if different.  */
      dep1 = sd_find_dep_between (last, tmp, true);

      if (dep1 == NULL || dep_cost (dep1) == 1)
	tmp_class = 3;
      else if (/* Data dependence.  */
	       DEP_TYPE (dep1) == REG_DEP_TRUE)
	tmp_class = 1;
      else
	tmp_class = 2;

      dep2 = sd_find_dep_between (last, tmp2, true);

      if (dep2 == NULL || dep_cost (dep2)  == 1)
	tmp2_class = 3;
      else if (/* Data dependence.  */
	       DEP_TYPE (dep2) == REG_DEP_TRUE)
	tmp2_class = 1;
      else
	tmp2_class = 2;

      if ((val = tmp2_class - tmp_class))
	return val;
    }

  /* Prefer instructions that occur earlier in the model schedule.  */
  if (sched_pressure == SCHED_PRESSURE_MODEL)
    {
      int diff;

      diff = model_index (tmp) - model_index (tmp2);
      if (diff != 0)
	return diff;
    }

  /* Prefer the insn which has more later insns that depend on it.
     This gives the scheduler more freedom when scheduling later
     instructions at the expense of added register pressure.  */

  val = (dep_list_size (tmp2, SD_LIST_FORW)
	 - dep_list_size (tmp, SD_LIST_FORW));

  if (flag_sched_dep_count_heuristic && val != 0)
    return val;

  /* If insns are equally good, sort by INSN_LUID (original insn order),
     so that we make the sort stable.  This minimizes instruction movement,
     thus minimizing sched's effect on debugging and cross-jumping.  */
  return INSN_LUID (tmp) - INSN_LUID (tmp2);
}

/* Resort the array A in which only element at index N may be out of order.  */

HAIFA_INLINE static void
swap_sort (rtx *a, int n)
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

/* Add INSN to the insn queue so that it can be executed at least
   N_CYCLES after the currently executing insn.  Preserve insns
   chain for debugging purposes.  REASON will be printed in debugging
   output.  */

HAIFA_INLINE static void
queue_insn (rtx insn, int n_cycles, const char *reason)
{
  int next_q = NEXT_Q_AFTER (q_ptr, n_cycles);
  rtx link = alloc_INSN_LIST (insn, insn_queue[next_q]);
  int new_tick;

  gcc_assert (n_cycles <= max_insn_queue_index);
  gcc_assert (!DEBUG_INSN_P (insn));

  insn_queue[next_q] = link;
  q_size += 1;

  if (sched_verbose >= 2)
    {
      fprintf (sched_dump, ";;\t\tReady-->Q: insn %s: ",
	       (*current_sched_info->print_insn) (insn, 0));

      fprintf (sched_dump, "queued for %d cycles (%s).\n", n_cycles, reason);
    }

  QUEUE_INDEX (insn) = next_q;

  if (current_sched_info->flags & DO_BACKTRACKING)
    {
      new_tick = clock_var + n_cycles;
      if (INSN_TICK (insn) == INVALID_TICK || INSN_TICK (insn) < new_tick)
	INSN_TICK (insn) = new_tick;

      if (INSN_EXACT_TICK (insn) != INVALID_TICK
	  && INSN_EXACT_TICK (insn) < clock_var + n_cycles)
	{
	  must_backtrack = true;
	  if (sched_verbose >= 2)
	    fprintf (sched_dump, ";;\t\tcausing a backtrack.\n");
	}
    }
}

/* Remove INSN from queue.  */
static void
queue_remove (rtx insn)
{
  gcc_assert (QUEUE_INDEX (insn) >= 0);
  remove_free_INSN_LIST_elem (insn, &insn_queue[QUEUE_INDEX (insn)]);
  q_size--;
  QUEUE_INDEX (insn) = QUEUE_NOWHERE;
}

/* Return a pointer to the bottom of the ready list, i.e. the insn
   with the lowest priority.  */

rtx *
ready_lastpos (struct ready_list *ready)
{
  gcc_assert (ready->n_ready >= 1);
  return ready->vec + ready->first - ready->n_ready + 1;
}

/* Add an element INSN to the ready list so that it ends up with the
   lowest/highest priority depending on FIRST_P.  */

HAIFA_INLINE static void
ready_add (struct ready_list *ready, rtx insn, bool first_p)
{
  if (!first_p)
    {
      if (ready->first == ready->n_ready)
	{
	  memmove (ready->vec + ready->veclen - ready->n_ready,
		   ready_lastpos (ready),
		   ready->n_ready * sizeof (rtx));
	  ready->first = ready->veclen - 1;
	}
      ready->vec[ready->first - ready->n_ready] = insn;
    }
  else
    {
      if (ready->first == ready->veclen - 1)
	{
	  if (ready->n_ready)
	    /* ready_lastpos() fails when called with (ready->n_ready == 0).  */
	    memmove (ready->vec + ready->veclen - ready->n_ready - 1,
		     ready_lastpos (ready),
		     ready->n_ready * sizeof (rtx));
	  ready->first = ready->veclen - 2;
	}
      ready->vec[++(ready->first)] = insn;
    }

  ready->n_ready++;
  if (DEBUG_INSN_P (insn))
    ready->n_debug++;

  gcc_assert (QUEUE_INDEX (insn) != QUEUE_READY);
  QUEUE_INDEX (insn) = QUEUE_READY;

  if (INSN_EXACT_TICK (insn) != INVALID_TICK
      && INSN_EXACT_TICK (insn) < clock_var)
    {
      must_backtrack = true;
    }
}

/* Remove the element with the highest priority from the ready list and
   return it.  */

HAIFA_INLINE static rtx
ready_remove_first (struct ready_list *ready)
{
  rtx t;

  gcc_assert (ready->n_ready);
  t = ready->vec[ready->first--];
  ready->n_ready--;
  if (DEBUG_INSN_P (t))
    ready->n_debug--;
  /* If the queue becomes empty, reset it.  */
  if (ready->n_ready == 0)
    ready->first = ready->veclen - 1;

  gcc_assert (QUEUE_INDEX (t) == QUEUE_READY);
  QUEUE_INDEX (t) = QUEUE_NOWHERE;

  return t;
}

/* The following code implements multi-pass scheduling for the first
   cycle.  In other words, we will try to choose ready insn which
   permits to start maximum number of insns on the same cycle.  */

/* Return a pointer to the element INDEX from the ready.  INDEX for
   insn with the highest priority is 0, and the lowest priority has
   N_READY - 1.  */

rtx
ready_element (struct ready_list *ready, int index)
{
  gcc_assert (ready->n_ready && index < ready->n_ready);

  return ready->vec[ready->first - index];
}

/* Remove the element INDEX from the ready list and return it.  INDEX
   for insn with the highest priority is 0, and the lowest priority
   has N_READY - 1.  */

HAIFA_INLINE static rtx
ready_remove (struct ready_list *ready, int index)
{
  rtx t;
  int i;

  if (index == 0)
    return ready_remove_first (ready);
  gcc_assert (ready->n_ready && index < ready->n_ready);
  t = ready->vec[ready->first - index];
  ready->n_ready--;
  if (DEBUG_INSN_P (t))
    ready->n_debug--;
  for (i = index; i < ready->n_ready; i++)
    ready->vec[ready->first - i] = ready->vec[ready->first - i - 1];
  QUEUE_INDEX (t) = QUEUE_NOWHERE;
  return t;
}

/* Remove INSN from the ready list.  */
static void
ready_remove_insn (rtx insn)
{
  int i;

  for (i = 0; i < readyp->n_ready; i++)
    if (ready_element (readyp, i) == insn)
      {
        ready_remove (readyp, i);
        return;
      }
  gcc_unreachable ();
}

/* Sort the ready list READY by ascending priority, using the SCHED_SORT
   macro.  */

void
ready_sort (struct ready_list *ready)
{
  int i;
  rtx *first = ready_lastpos (ready);

  if (sched_pressure == SCHED_PRESSURE_WEIGHTED)
    {
      for (i = 0; i < ready->n_ready; i++)
	if (!DEBUG_INSN_P (first[i]))
	  setup_insn_reg_pressure_info (first[i]);
    }
  if (sched_pressure == SCHED_PRESSURE_MODEL
      && model_curr_point < model_num_insns)
    model_set_excess_costs (first, ready->n_ready);
  SCHED_SORT (first, ready->n_ready);
}

/* PREV is an insn that is ready to execute.  Adjust its priority if that
   will help shorten or lengthen register lifetimes as appropriate.  Also
   provide a hook for the target to tweak itself.  */

HAIFA_INLINE static void
adjust_priority (rtx prev)
{
  /* ??? There used to be code here to try and estimate how an insn
     affected register lifetimes, but it did it by looking at REG_DEAD
     notes, which we removed in schedule_region.  Nor did it try to
     take into account register pressure or anything useful like that.

     Revisit when we have a machine model to work with and not before.  */

  if (targetm.sched.adjust_priority)
    INSN_PRIORITY (prev) =
      targetm.sched.adjust_priority (prev, INSN_PRIORITY (prev));
}

/* Advance DFA state STATE on one cycle.  */
void
advance_state (state_t state)
{
  if (targetm.sched.dfa_pre_advance_cycle)
    targetm.sched.dfa_pre_advance_cycle ();

  if (targetm.sched.dfa_pre_cycle_insn)
    state_transition (state,
		      targetm.sched.dfa_pre_cycle_insn ());

  state_transition (state, NULL);

  if (targetm.sched.dfa_post_cycle_insn)
    state_transition (state,
		      targetm.sched.dfa_post_cycle_insn ());

  if (targetm.sched.dfa_post_advance_cycle)
    targetm.sched.dfa_post_advance_cycle ();
}

/* Advance time on one cycle.  */
HAIFA_INLINE static void
advance_one_cycle (void)
{
  advance_state (curr_state);
  if (sched_verbose >= 6)
    fprintf (sched_dump, ";;\tAdvanced a state.\n");
}

/* Update register pressure after scheduling INSN.  */
static void
update_register_pressure (rtx insn)
{
  struct reg_use_data *use;
  struct reg_set_data *set;

  gcc_checking_assert (!DEBUG_INSN_P (insn));

  for (use = INSN_REG_USE_LIST (insn); use != NULL; use = use->next_insn_use)
    if (dying_use_p (use))
      mark_regno_birth_or_death (curr_reg_live, curr_reg_pressure,
				 use->regno, false);
  for (set = INSN_REG_SET_LIST (insn); set != NULL; set = set->next_insn_set)
    mark_regno_birth_or_death (curr_reg_live, curr_reg_pressure,
			       set->regno, true);
}

/* Set up or update (if UPDATE_P) max register pressure (see its
   meaning in sched-int.h::_haifa_insn_data) for all current BB insns
   after insn AFTER.  */
static void
setup_insn_max_reg_pressure (rtx after, bool update_p)
{
  int i, p;
  bool eq_p;
  rtx insn;
  static int max_reg_pressure[N_REG_CLASSES];

  save_reg_pressure ();
  for (i = 0; i < ira_pressure_classes_num; i++)
    max_reg_pressure[ira_pressure_classes[i]]
      = curr_reg_pressure[ira_pressure_classes[i]];
  for (insn = NEXT_INSN (after);
       insn != NULL_RTX && ! BARRIER_P (insn)
	 && BLOCK_FOR_INSN (insn) == BLOCK_FOR_INSN (after);
       insn = NEXT_INSN (insn))
    if (NONDEBUG_INSN_P (insn))
      {
	eq_p = true;
	for (i = 0; i < ira_pressure_classes_num; i++)
	  {
	    p = max_reg_pressure[ira_pressure_classes[i]];
	    if (INSN_MAX_REG_PRESSURE (insn)[i] != p)
	      {
		eq_p = false;
		INSN_MAX_REG_PRESSURE (insn)[i]
		  = max_reg_pressure[ira_pressure_classes[i]];
	      }
	  }
	if (update_p && eq_p)
	  break;
	update_register_pressure (insn);
	for (i = 0; i < ira_pressure_classes_num; i++)
	  if (max_reg_pressure[ira_pressure_classes[i]]
	      < curr_reg_pressure[ira_pressure_classes[i]])
	    max_reg_pressure[ira_pressure_classes[i]]
	      = curr_reg_pressure[ira_pressure_classes[i]];
      }
  restore_reg_pressure ();
}

/* Update the current register pressure after scheduling INSN.  Update
   also max register pressure for unscheduled insns of the current
   BB.  */
static void
update_reg_and_insn_max_reg_pressure (rtx insn)
{
  int i;
  int before[N_REG_CLASSES];

  for (i = 0; i < ira_pressure_classes_num; i++)
    before[i] = curr_reg_pressure[ira_pressure_classes[i]];
  update_register_pressure (insn);
  for (i = 0; i < ira_pressure_classes_num; i++)
    if (curr_reg_pressure[ira_pressure_classes[i]] != before[i])
      break;
  if (i < ira_pressure_classes_num)
    setup_insn_max_reg_pressure (insn, true);
}

/* Set up register pressure at the beginning of basic block BB whose
   insns starting after insn AFTER.  Set up also max register pressure
   for all insns of the basic block.  */
void
sched_setup_bb_reg_pressure_info (basic_block bb, rtx after)
{
  gcc_assert (sched_pressure == SCHED_PRESSURE_WEIGHTED);
  initiate_bb_reg_pressure_info (bb);
  setup_insn_max_reg_pressure (after, false);
}

/* If doing predication while scheduling, verify whether INSN, which
   has just been scheduled, clobbers the conditions of any
   instructions that must be predicated in order to break their
   dependencies.  If so, remove them from the queues so that they will
   only be scheduled once their control dependency is resolved.  */

static void
check_clobbered_conditions (rtx insn)
{
  HARD_REG_SET t;
  int i;

  if ((current_sched_info->flags & DO_PREDICATION) == 0)
    return;

  find_all_hard_reg_sets (insn, &t);

 restart:
  for (i = 0; i < ready.n_ready; i++)
    {
      rtx x = ready_element (&ready, i);
      if (TODO_SPEC (x) == DEP_CONTROL && cond_clobbered_p (x, t))
	{
	  ready_remove_insn (x);
	  goto restart;
	}
    }
  for (i = 0; i <= max_insn_queue_index; i++)
    {
      rtx link;
      int q = NEXT_Q_AFTER (q_ptr, i);

    restart_queue:
      for (link = insn_queue[q]; link; link = XEXP (link, 1))
	{
	  rtx x = XEXP (link, 0);
	  if (TODO_SPEC (x) == DEP_CONTROL && cond_clobbered_p (x, t))
	    {
	      queue_remove (x);
	      goto restart_queue;
	    }
	}
    }
}

/* Return (in order):

   - positive if INSN adversely affects the pressure on one
     register class

   - negative if INSN reduces the pressure on one register class

   - 0 if INSN doesn't affect the pressure on any register class.  */

static int
model_classify_pressure (struct model_insn_info *insn)
{
  struct reg_pressure_data *reg_pressure;
  int death[N_REG_CLASSES];
  int pci, cl, sum;

  calculate_reg_deaths (insn->insn, death);
  reg_pressure = INSN_REG_PRESSURE (insn->insn);
  sum = 0;
  for (pci = 0; pci < ira_pressure_classes_num; pci++)
    {
      cl = ira_pressure_classes[pci];
      if (death[cl] < reg_pressure[pci].set_increase)
	return 1;
      sum += reg_pressure[pci].set_increase - death[cl];
    }
  return sum;
}

/* Return true if INSN1 should come before INSN2 in the model schedule.  */

static int
model_order_p (struct model_insn_info *insn1, struct model_insn_info *insn2)
{
  unsigned int height1, height2;
  unsigned int priority1, priority2;

  /* Prefer instructions with a higher model priority.  */
  if (insn1->model_priority != insn2->model_priority)
    return insn1->model_priority > insn2->model_priority;

  /* Combine the length of the longest path of satisfied true dependencies
     that leads to each instruction (depth) with the length of the longest
     path of any dependencies that leads from the instruction (alap).
     Prefer instructions with the greatest combined length.  If the combined
     lengths are equal, prefer instructions with the greatest depth.

     The idea is that, if we have a set S of "equal" instructions that each
     have ALAP value X, and we pick one such instruction I, any true-dependent
     successors of I that have ALAP value X - 1 should be preferred over S.
     This encourages the schedule to be "narrow" rather than "wide".
     However, if I is a low-priority instruction that we decided to
     schedule because of its model_classify_pressure, and if there
     is a set of higher-priority instructions T, the aforementioned
     successors of I should not have the edge over T.  */
  height1 = insn1->depth + insn1->alap;
  height2 = insn2->depth + insn2->alap;
  if (height1 != height2)
    return height1 > height2;
  if (insn1->depth != insn2->depth)
    return insn1->depth > insn2->depth;

  /* We have no real preference between INSN1 an INSN2 as far as attempts
     to reduce pressure go.  Prefer instructions with higher priorities.  */
  priority1 = INSN_PRIORITY (insn1->insn);
  priority2 = INSN_PRIORITY (insn2->insn);
  if (priority1 != priority2)
    return priority1 > priority2;

  /* Use the original rtl sequence as a tie-breaker.  */
  return insn1 < insn2;
}

/* Add INSN to the model worklist immediately after PREV.  Add it to the
   beginning of the list if PREV is null.  */

static void
model_add_to_worklist_at (struct model_insn_info *insn,
			  struct model_insn_info *prev)
{
  gcc_assert (QUEUE_INDEX (insn->insn) == QUEUE_NOWHERE);
  QUEUE_INDEX (insn->insn) = QUEUE_READY;

  insn->prev = prev;
  if (prev)
    {
      insn->next = prev->next;
      prev->next = insn;
    }
  else
    {
      insn->next = model_worklist;
      model_worklist = insn;
    }
  if (insn->next)
    insn->next->prev = insn;
}

/* Remove INSN from the model worklist.  */

static void
model_remove_from_worklist (struct model_insn_info *insn)
{
  gcc_assert (QUEUE_INDEX (insn->insn) == QUEUE_READY);
  QUEUE_INDEX (insn->insn) = QUEUE_NOWHERE;

  if (insn->prev)
    insn->prev->next = insn->next;
  else
    model_worklist = insn->next;
  if (insn->next)
    insn->next->prev = insn->prev;
}

/* Add INSN to the model worklist.  Start looking for a suitable position
   between neighbors PREV and NEXT, testing at most MAX_SCHED_READY_INSNS
   insns either side.  A null PREV indicates the beginning of the list and
   a null NEXT indicates the end.  */

static void
model_add_to_worklist (struct model_insn_info *insn,
		       struct model_insn_info *prev,
		       struct model_insn_info *next)
{
  int count;

  count = MAX_SCHED_READY_INSNS;
  if (count > 0 && prev && model_order_p (insn, prev))
    do
      {
	count--;
	prev = prev->prev;
      }
    while (count > 0 && prev && model_order_p (insn, prev));
  else
    while (count > 0 && next && model_order_p (next, insn))
      {
	count--;
	prev = next;
	next = next->next;
      }
  model_add_to_worklist_at (insn, prev);
}

/* INSN may now have a higher priority (in the model_order_p sense)
   than before.  Move it up the worklist if necessary.  */

static void
model_promote_insn (struct model_insn_info *insn)
{
  struct model_insn_info *prev;
  int count;

  prev = insn->prev;
  count = MAX_SCHED_READY_INSNS;
  while (count > 0 && prev && model_order_p (insn, prev))
    {
      count--;
      prev = prev->prev;
    }
  if (prev != insn->prev)
    {
      model_remove_from_worklist (insn);
      model_add_to_worklist_at (insn, prev);
    }
}

/* Add INSN to the end of the model schedule.  */

static void
model_add_to_schedule (rtx insn)
{
  unsigned int point;

  gcc_assert (QUEUE_INDEX (insn) == QUEUE_NOWHERE);
  QUEUE_INDEX (insn) = QUEUE_SCHEDULED;

  point = model_schedule.length ();
  model_schedule.quick_push (insn);
  INSN_MODEL_INDEX (insn) = point + 1;
}

/* Analyze the instructions that are to be scheduled, setting up
   MODEL_INSN_INFO (...) and model_num_insns accordingly.  Add ready
   instructions to model_worklist.  */

static void
model_analyze_insns (void)
{
  rtx start, end, iter;
  sd_iterator_def sd_it;
  dep_t dep;
  struct model_insn_info *insn, *con;

  model_num_insns = 0;
  start = PREV_INSN (current_sched_info->next_tail);
  end = current_sched_info->prev_head;
  for (iter = start; iter != end; iter = PREV_INSN (iter))
    if (NONDEBUG_INSN_P (iter))
      {
	insn = MODEL_INSN_INFO (iter);
	insn->insn = iter;
	FOR_EACH_DEP (iter, SD_LIST_FORW, sd_it, dep)
	  {
	    con = MODEL_INSN_INFO (DEP_CON (dep));
	    if (con->insn && insn->alap < con->alap + 1)
	      insn->alap = con->alap + 1;
	  }

	insn->old_queue = QUEUE_INDEX (iter);
	QUEUE_INDEX (iter) = QUEUE_NOWHERE;

	insn->unscheduled_preds = dep_list_size (iter, SD_LIST_HARD_BACK);
	if (insn->unscheduled_preds == 0)
	  model_add_to_worklist (insn, NULL, model_worklist);

	model_num_insns++;
      }
}

/* The global state describes the register pressure at the start of the
   model schedule.  Initialize GROUP accordingly.  */

static void
model_init_pressure_group (struct model_pressure_group *group)
{
  int pci, cl;

  for (pci = 0; pci < ira_pressure_classes_num; pci++)
    {
      cl = ira_pressure_classes[pci];
      group->limits[pci].pressure = curr_reg_pressure[cl];
      group->limits[pci].point = 0;
    }
  /* Use index model_num_insns to record the state after the last
     instruction in the model schedule.  */
  group->model = XNEWVEC (struct model_pressure_data,
			  (model_num_insns + 1) * ira_pressure_classes_num);
}

/* Record that MODEL_REF_PRESSURE (GROUP, POINT, PCI) is PRESSURE.
   Update the maximum pressure for the whole schedule.  */

static void
model_record_pressure (struct model_pressure_group *group,
		       int point, int pci, int pressure)
{
  MODEL_REF_PRESSURE (group, point, pci) = pressure;
  if (group->limits[pci].pressure < pressure)
    {
      group->limits[pci].pressure = pressure;
      group->limits[pci].point = point;
    }
}

/* INSN has just been added to the end of the model schedule.  Record its
   register-pressure information.  */

static void
model_record_pressures (struct model_insn_info *insn)
{
  struct reg_pressure_data *reg_pressure;
  int point, pci, cl, delta;
  int death[N_REG_CLASSES];

  point = model_index (insn->insn);
  if (sched_verbose >= 2)
    {
      if (point == 0)
	{
	  fprintf (sched_dump, "\n;;\tModel schedule:\n;;\n");
	  fprintf (sched_dump, ";;\t| idx insn | mpri hght dpth prio |\n");
	}
      fprintf (sched_dump, ";;\t| %3d %4d | %4d %4d %4d %4d | %-30s ",
	       point, INSN_UID (insn->insn), insn->model_priority,
	       insn->depth + insn->alap, insn->depth,
	       INSN_PRIORITY (insn->insn),
	       str_pattern_slim (PATTERN (insn->insn)));
    }
  calculate_reg_deaths (insn->insn, death);
  reg_pressure = INSN_REG_PRESSURE (insn->insn);
  for (pci = 0; pci < ira_pressure_classes_num; pci++)
    {
      cl = ira_pressure_classes[pci];
      delta = reg_pressure[pci].set_increase - death[cl];
      if (sched_verbose >= 2)
	fprintf (sched_dump, " %s:[%d,%+d]", reg_class_names[cl],
		 curr_reg_pressure[cl], delta);
      model_record_pressure (&model_before_pressure, point, pci,
			     curr_reg_pressure[cl]);
    }
  if (sched_verbose >= 2)
    fprintf (sched_dump, "\n");
}

/* All instructions have been added to the model schedule.  Record the
   final register pressure in GROUP and set up all MODEL_MAX_PRESSUREs.  */

static void
model_record_final_pressures (struct model_pressure_group *group)
{
  int point, pci, max_pressure, ref_pressure, cl;

  for (pci = 0; pci < ira_pressure_classes_num; pci++)
    {
      /* Record the final pressure for this class.  */
      cl = ira_pressure_classes[pci];
      point = model_num_insns;
      ref_pressure = curr_reg_pressure[cl];
      model_record_pressure (group, point, pci, ref_pressure);

      /* Record the original maximum pressure.  */
      group->limits[pci].orig_pressure = group->limits[pci].pressure;

      /* Update the MODEL_MAX_PRESSURE for every point of the schedule.  */
      max_pressure = ref_pressure;
      MODEL_MAX_PRESSURE (group, point, pci) = max_pressure;
      while (point > 0)
	{
	  point--;
	  ref_pressure = MODEL_REF_PRESSURE (group, point, pci);
	  max_pressure = MAX (max_pressure, ref_pressure);
	  MODEL_MAX_PRESSURE (group, point, pci) = max_pressure;
	}
    }
}

/* Update all successors of INSN, given that INSN has just been scheduled.  */

static void
model_add_successors_to_worklist (struct model_insn_info *insn)
{
  sd_iterator_def sd_it;
  struct model_insn_info *con;
  dep_t dep;

  FOR_EACH_DEP (insn->insn, SD_LIST_FORW, sd_it, dep)
    {
      con = MODEL_INSN_INFO (DEP_CON (dep));
      /* Ignore debug instructions, and instructions from other blocks.  */
      if (con->insn)
	{
	  con->unscheduled_preds--;

	  /* Update the depth field of each true-dependent successor.
	     Increasing the depth gives them a higher priority than
	     before.  */
	  if (DEP_TYPE (dep) == REG_DEP_TRUE && con->depth < insn->depth + 1)
	    {
	      con->depth = insn->depth + 1;
	      if (QUEUE_INDEX (con->insn) == QUEUE_READY)
		model_promote_insn (con);
	    }

	  /* If this is a true dependency, or if there are no remaining
	     dependencies for CON (meaning that CON only had non-true
	     dependencies), make sure that CON is on the worklist.
	     We don't bother otherwise because it would tend to fill the
	     worklist with a lot of low-priority instructions that are not
	     yet ready to issue.  */
	  if ((con->depth > 0 || con->unscheduled_preds == 0)
	      && QUEUE_INDEX (con->insn) == QUEUE_NOWHERE)
	    model_add_to_worklist (con, insn, insn->next);
	}
    }
}

/* Give INSN a higher priority than any current instruction, then give
   unscheduled predecessors of INSN a higher priority still.  If any of
   those predecessors are not on the model worklist, do the same for its
   predecessors, and so on.  */

static void
model_promote_predecessors (struct model_insn_info *insn)
{
  struct model_insn_info *pro, *first;
  sd_iterator_def sd_it;
  dep_t dep;

  if (sched_verbose >= 7)
    fprintf (sched_dump, ";;\t+--- priority of %d = %d, priority of",
	     INSN_UID (insn->insn), model_next_priority);
  insn->model_priority = model_next_priority++;
  model_remove_from_worklist (insn);
  model_add_to_worklist_at (insn, NULL);

  first = NULL;
  for (;;)
    {
      FOR_EACH_DEP (insn->insn, SD_LIST_HARD_BACK, sd_it, dep)
	{
	  pro = MODEL_INSN_INFO (DEP_PRO (dep));
	  /* The first test is to ignore debug instructions, and instructions
	     from other blocks.  */
	  if (pro->insn
	      && pro->model_priority != model_next_priority
	      && QUEUE_INDEX (pro->insn) != QUEUE_SCHEDULED)
	    {
	      pro->model_priority = model_next_priority;
	      if (sched_verbose >= 7)
		fprintf (sched_dump, " %d", INSN_UID (pro->insn));
	      if (QUEUE_INDEX (pro->insn) == QUEUE_READY)
		{
		  /* PRO is already in the worklist, but it now has
		     a higher priority than before.  Move it at the
		     appropriate place.  */
		  model_remove_from_worklist (pro);
		  model_add_to_worklist (pro, NULL, model_worklist);
		}
	      else
		{
		  /* PRO isn't in the worklist.  Recursively process
		     its predecessors until we find one that is.  */
		  pro->next = first;
		  first = pro;
		}
	    }
	}
      if (!first)
	break;
      insn = first;
      first = insn->next;
    }
  if (sched_verbose >= 7)
    fprintf (sched_dump, " = %d\n", model_next_priority);
  model_next_priority++;
}

/* Pick one instruction from model_worklist and process it.  */

static void
model_choose_insn (void)
{
  struct model_insn_info *insn, *fallback;
  int count;

  if (sched_verbose >= 7)
    {
      fprintf (sched_dump, ";;\t+--- worklist:\n");
      insn = model_worklist;
      count = MAX_SCHED_READY_INSNS;
      while (count > 0 && insn)
	{
	  fprintf (sched_dump, ";;\t+---   %d [%d, %d, %d, %d]\n",
		   INSN_UID (insn->insn), insn->model_priority,
		   insn->depth + insn->alap, insn->depth,
		   INSN_PRIORITY (insn->insn));
	  count--;
	  insn = insn->next;
	}
    }

  /* Look for a ready instruction whose model_classify_priority is zero
     or negative, picking the highest-priority one.  Adding such an
     instruction to the schedule now should do no harm, and may actually
     do some good.

     Failing that, see whether there is an instruction with the highest
     extant model_priority that is not yet ready, but which would reduce
     pressure if it became ready.  This is designed to catch cases like:

       (set (mem (reg R1)) (reg R2))

     where the instruction is the last remaining use of R1 and where the
     value of R2 is not yet available (or vice versa).  The death of R1
     means that this instruction already reduces pressure.  It is of
     course possible that the computation of R2 involves other registers
     that are hard to kill, but such cases are rare enough for this
     heuristic to be a win in general.

     Failing that, just pick the highest-priority instruction in the
     worklist.  */
  count = MAX_SCHED_READY_INSNS;
  insn = model_worklist;
  fallback = 0;
  for (;;)
    {
      if (count == 0 || !insn)
	{
	  insn = fallback ? fallback : model_worklist;
	  break;
	}
      if (insn->unscheduled_preds)
	{
	  if (model_worklist->model_priority == insn->model_priority
	      && !fallback
	      && model_classify_pressure (insn) < 0)
	    fallback = insn;
	}
      else
	{
	  if (model_classify_pressure (insn) <= 0)
	    break;
	}
      count--;
      insn = insn->next;
    }

  if (sched_verbose >= 7 && insn != model_worklist)
    {
      if (insn->unscheduled_preds)
	fprintf (sched_dump, ";;\t+--- promoting insn %d, with dependencies\n",
		 INSN_UID (insn->insn));
      else
	fprintf (sched_dump, ";;\t+--- promoting insn %d, which is ready\n",
		 INSN_UID (insn->insn));
    }
  if (insn->unscheduled_preds)
    /* INSN isn't yet ready to issue.  Give all its predecessors the
       highest priority.  */
    model_promote_predecessors (insn);
  else
    {
      /* INSN is ready.  Add it to the end of model_schedule and
	 process its successors.  */
      model_add_successors_to_worklist (insn);
      model_remove_from_worklist (insn);
      model_add_to_schedule (insn->insn);
      model_record_pressures (insn);
      update_register_pressure (insn->insn);
    }
}

/* Restore all QUEUE_INDEXs to the values that they had before
   model_start_schedule was called.  */

static void
model_reset_queue_indices (void)
{
  unsigned int i;
  rtx insn;

  FOR_EACH_VEC_ELT (model_schedule, i, insn)
    QUEUE_INDEX (insn) = MODEL_INSN_INFO (insn)->old_queue;
}

/* We have calculated the model schedule and spill costs.  Print a summary
   to sched_dump.  */

static void
model_dump_pressure_summary (void)
{
  int pci, cl;

  fprintf (sched_dump, ";; Pressure summary:");
  for (pci = 0; pci < ira_pressure_classes_num; pci++)
    {
      cl = ira_pressure_classes[pci];
      fprintf (sched_dump, " %s:%d", reg_class_names[cl],
	       model_before_pressure.limits[pci].pressure);
    }
  fprintf (sched_dump, "\n\n");
}

/* Initialize the SCHED_PRESSURE_MODEL information for the current
   scheduling region.  */

static void
model_start_schedule (void)
{
  basic_block bb;

  model_next_priority = 1;
  model_schedule.create (sched_max_luid);
  model_insns = XCNEWVEC (struct model_insn_info, sched_max_luid);

  bb = BLOCK_FOR_INSN (NEXT_INSN (current_sched_info->prev_head));
  initiate_reg_pressure_info (df_get_live_in (bb));

  model_analyze_insns ();
  model_init_pressure_group (&model_before_pressure);
  while (model_worklist)
    model_choose_insn ();
  gcc_assert (model_num_insns == (int) model_schedule.length ());
  if (sched_verbose >= 2)
    fprintf (sched_dump, "\n");

  model_record_final_pressures (&model_before_pressure);
  model_reset_queue_indices ();

  XDELETEVEC (model_insns);

  model_curr_point = 0;
  initiate_reg_pressure_info (df_get_live_in (bb));
  if (sched_verbose >= 1)
    model_dump_pressure_summary ();
}

/* Free the information associated with GROUP.  */

static void
model_finalize_pressure_group (struct model_pressure_group *group)
{
  XDELETEVEC (group->model);
}

/* Free the information created by model_start_schedule.  */

static void
model_end_schedule (void)
{
  model_finalize_pressure_group (&model_before_pressure);
  model_schedule.release ();
}

/* A structure that holds local state for the loop in schedule_block.  */
struct sched_block_state
{
  /* True if no real insns have been scheduled in the current cycle.  */
  bool first_cycle_insn_p;
  /* True if a shadow insn has been scheduled in the current cycle, which
     means that no more normal insns can be issued.  */
  bool shadows_only_p;
  /* True if we're winding down a modulo schedule, which means that we only
     issue insns with INSN_EXACT_TICK set.  */
  bool modulo_epilogue;
  /* Initialized with the machine's issue rate every cycle, and updated
     by calls to the variable_issue hook.  */
  int can_issue_more;
};

/* INSN is the "currently executing insn".  Launch each insn which was
   waiting on INSN.  READY is the ready list which contains the insns
   that are ready to fire.  CLOCK is the current cycle.  The function
   returns necessary cycle advance after issuing the insn (it is not
   zero for insns in a schedule group).  */

static int
schedule_insn (rtx insn)
{
  sd_iterator_def sd_it;
  dep_t dep;
  int i;
  int advance = 0;

  if (sched_verbose >= 1)
    {
      struct reg_pressure_data *pressure_info;
      fprintf (sched_dump, ";;\t%3i--> %s%-40s:",
	       clock_var, (*current_sched_info->print_insn) (insn, 1),
	       str_pattern_slim (PATTERN (insn)));

      if (recog_memoized (insn) < 0)
	fprintf (sched_dump, "nothing");
      else
	print_reservation (sched_dump, insn);
      pressure_info = INSN_REG_PRESSURE (insn);
      if (pressure_info != NULL)
	{
	  fputc (':', sched_dump);
	  for (i = 0; i < ira_pressure_classes_num; i++)
	    fprintf (sched_dump, "%s%s%+d(%d)",
		     scheduled_insns.length () > 1
		     && INSN_LUID (insn)
		     < INSN_LUID (scheduled_insns[scheduled_insns.length () - 2]) ? "@" : "",
		     reg_class_names[ira_pressure_classes[i]],
		     pressure_info[i].set_increase, pressure_info[i].change);
	}
      if (sched_pressure == SCHED_PRESSURE_MODEL
	  && model_curr_point < model_num_insns
	  && model_index (insn) == model_curr_point)
	fprintf (sched_dump, ":model %d", model_curr_point);
      fputc ('\n', sched_dump);
    }

  if (sched_pressure == SCHED_PRESSURE_WEIGHTED && !DEBUG_INSN_P (insn))
    update_reg_and_insn_max_reg_pressure (insn);

  /* Scheduling instruction should have all its dependencies resolved and
     should have been removed from the ready list.  */
  gcc_assert (sd_lists_empty_p (insn, SD_LIST_HARD_BACK));

  /* Reset debug insns invalidated by moving this insn.  */
  if (MAY_HAVE_DEBUG_INSNS && !DEBUG_INSN_P (insn))
    for (sd_it = sd_iterator_start (insn, SD_LIST_BACK);
	 sd_iterator_cond (&sd_it, &dep);)
      {
	rtx dbg = DEP_PRO (dep);
	struct reg_use_data *use, *next;

	if (DEP_STATUS (dep) & DEP_CANCELLED)
	  {
	    sd_iterator_next (&sd_it);
	    continue;
	  }

	gcc_assert (DEBUG_INSN_P (dbg));

	if (sched_verbose >= 6)
	  fprintf (sched_dump, ";;\t\tresetting: debug insn %d\n",
		   INSN_UID (dbg));

	/* ??? Rather than resetting the debug insn, we might be able
	   to emit a debug temp before the just-scheduled insn, but
	   this would involve checking that the expression at the
	   point of the debug insn is equivalent to the expression
	   before the just-scheduled insn.  They might not be: the
	   expression in the debug insn may depend on other insns not
	   yet scheduled that set MEMs, REGs or even other debug
	   insns.  It's not clear that attempting to preserve debug
	   information in these cases is worth the effort, given how
	   uncommon these resets are and the likelihood that the debug
	   temps introduced won't survive the schedule change.  */
	INSN_VAR_LOCATION_LOC (dbg) = gen_rtx_UNKNOWN_VAR_LOC ();
	df_insn_rescan (dbg);

	/* Unknown location doesn't use any registers.  */
	for (use = INSN_REG_USE_LIST (dbg); use != NULL; use = next)
	  {
	    struct reg_use_data *prev = use;

	    /* Remove use from the cyclic next_regno_use chain first.  */
	    while (prev->next_regno_use != use)
	      prev = prev->next_regno_use;
	    prev->next_regno_use = use->next_regno_use;
	    next = use->next_insn_use;
	    free (use);
	  }
	INSN_REG_USE_LIST (dbg) = NULL;

	/* We delete rather than resolve these deps, otherwise we
	   crash in sched_free_deps(), because forward deps are
	   expected to be released before backward deps.  */
	sd_delete_dep (sd_it);
      }

  gcc_assert (QUEUE_INDEX (insn) == QUEUE_NOWHERE);
  QUEUE_INDEX (insn) = QUEUE_SCHEDULED;

  if (sched_pressure == SCHED_PRESSURE_MODEL
      && model_curr_point < model_num_insns
      && NONDEBUG_INSN_P (insn))
    {
      if (model_index (insn) == model_curr_point)
	do
	  model_curr_point++;
	while (model_curr_point < model_num_insns
	       && (QUEUE_INDEX (MODEL_INSN (model_curr_point))
		   == QUEUE_SCHEDULED));
      else
	model_recompute (insn);
      model_update_limit_points ();
      update_register_pressure (insn);
      if (sched_verbose >= 2)
	print_curr_reg_pressure ();
    }

  gcc_assert (INSN_TICK (insn) >= MIN_TICK);
  if (INSN_TICK (insn) > clock_var)
    /* INSN has been prematurely moved from the queue to the ready list.
       This is possible only if following flag is set.  */
    gcc_assert (flag_sched_stalled_insns);

  /* ??? Probably, if INSN is scheduled prematurely, we should leave
     INSN_TICK untouched.  This is a machine-dependent issue, actually.  */
  INSN_TICK (insn) = clock_var;

  check_clobbered_conditions (insn);

  /* Update dependent instructions.  First, see if by scheduling this insn
     now we broke a dependence in a way that requires us to change another
     insn.  */
  for (sd_it = sd_iterator_start (insn, SD_LIST_SPEC_BACK);
       sd_iterator_cond (&sd_it, &dep); sd_iterator_next (&sd_it))
    {
      struct dep_replacement *desc = DEP_REPLACE (dep);
      rtx pro = DEP_PRO (dep);
      if (QUEUE_INDEX (pro) != QUEUE_SCHEDULED
	  && desc != NULL && desc->insn == pro)
	apply_replacement (dep, false);
    }

  /* Go through and resolve forward dependencies.  */
  for (sd_it = sd_iterator_start (insn, SD_LIST_FORW);
       sd_iterator_cond (&sd_it, &dep);)
    {
      rtx next = DEP_CON (dep);
      bool cancelled = (DEP_STATUS (dep) & DEP_CANCELLED) != 0;

      /* Resolve the dependence between INSN and NEXT.
	 sd_resolve_dep () moves current dep to another list thus
	 advancing the iterator.  */
      sd_resolve_dep (sd_it);

      if (cancelled)
	{
	  if (must_restore_pattern_p (next, dep))
	    restore_pattern (dep, false);
	  continue;
	}

      /* Don't bother trying to mark next as ready if insn is a debug
	 insn.  If insn is the last hard dependency, it will have
	 already been discounted.  */
      if (DEBUG_INSN_P (insn) && !DEBUG_INSN_P (next))
	continue;

      if (!IS_SPECULATION_BRANCHY_CHECK_P (insn))
	{
	  int effective_cost;

	  effective_cost = try_ready (next);

	  if (effective_cost >= 0
	      && SCHED_GROUP_P (next)
	      && advance < effective_cost)
	    advance = effective_cost;
	}
      else
	/* Check always has only one forward dependence (to the first insn in
	   the recovery block), therefore, this will be executed only once.  */
	{
	  gcc_assert (sd_lists_empty_p (insn, SD_LIST_FORW));
	  fix_recovery_deps (RECOVERY_BLOCK (insn));
	}
    }

  /* Annotate the instruction with issue information -- TImode
     indicates that the instruction is expected not to be able
     to issue on the same cycle as the previous insn.  A machine
     may use this information to decide how the instruction should
     be aligned.  */
  if (issue_rate > 1
      && GET_CODE (PATTERN (insn)) != USE
      && GET_CODE (PATTERN (insn)) != CLOBBER
      && !DEBUG_INSN_P (insn))
    {
      if (reload_completed)
	PUT_MODE (insn, clock_var > last_clock_var ? TImode : VOIDmode);
      last_clock_var = clock_var;
    }

  return advance;
}

/* Functions for handling of notes.  */

/* Add note list that ends on FROM_END to the end of TO_ENDP.  */
void
concat_note_lists (rtx from_end, rtx *to_endp)
{
  rtx from_start;

  /* It's easy when have nothing to concat.  */
  if (from_end == NULL)
    return;

  /* It's also easy when destination is empty.  */
  if (*to_endp == NULL)
    {
      *to_endp = from_end;
      return;
    }

  from_start = from_end;
  while (PREV_INSN (from_start) != NULL)
    from_start = PREV_INSN (from_start);

  PREV_INSN (from_start) = *to_endp;
  NEXT_INSN (*to_endp) = from_start;
  *to_endp = from_end;
}

/* Delete notes between HEAD and TAIL and put them in the chain
   of notes ended by NOTE_LIST.  */
void
remove_notes (rtx head, rtx tail)
{
  rtx next_tail, insn, next;

  note_list = 0;
  if (head == tail && !INSN_P (head))
    return;

  next_tail = NEXT_INSN (tail);
  for (insn = head; insn != next_tail; insn = next)
    {
      next = NEXT_INSN (insn);
      if (!NOTE_P (insn))
	continue;

      switch (NOTE_KIND (insn))
	{
	case NOTE_INSN_BASIC_BLOCK:
	  continue;

	case NOTE_INSN_EPILOGUE_BEG:
	  if (insn != tail)
	    {
	      remove_insn (insn);
	      add_reg_note (next, REG_SAVE_NOTE,
			    GEN_INT (NOTE_INSN_EPILOGUE_BEG));
	      break;
	    }
	  /* FALLTHRU */

	default:
	  remove_insn (insn);

	  /* Add the note to list that ends at NOTE_LIST.  */
	  PREV_INSN (insn) = note_list;
	  NEXT_INSN (insn) = NULL_RTX;
	  if (note_list)
	    NEXT_INSN (note_list) = insn;
	  note_list = insn;
	  break;
	}

      gcc_assert ((sel_sched_p () || insn != tail) && insn != head);
    }
}

/* A structure to record enough data to allow us to backtrack the scheduler to
   a previous state.  */
struct haifa_saved_data
{
  /* Next entry on the list.  */
  struct haifa_saved_data *next;

  /* Backtracking is associated with scheduling insns that have delay slots.
     DELAY_PAIR points to the structure that contains the insns involved, and
     the number of cycles between them.  */
  struct delay_pair *delay_pair;

  /* Data used by the frontend (e.g. sched-ebb or sched-rgn).  */
  void *fe_saved_data;
  /* Data used by the backend.  */
  void *be_saved_data;

  /* Copies of global state.  */
  int clock_var, last_clock_var;
  struct ready_list ready;
  state_t curr_state;

  rtx last_scheduled_insn;
  rtx last_nondebug_scheduled_insn;
  int cycle_issued_insns;

  /* Copies of state used in the inner loop of schedule_block.  */
  struct sched_block_state sched_block;

  /* We don't need to save q_ptr, as its value is arbitrary and we can set it
     to 0 when restoring.  */
  int q_size;
  rtx *insn_queue;

  /* Describe pattern replacements that occurred since this backtrack point
     was queued.  */
  vec<dep_t> replacement_deps;
  vec<int> replace_apply;

  /* A copy of the next-cycle replacement vectors at the time of the backtrack
     point.  */
  vec<dep_t> next_cycle_deps;
  vec<int> next_cycle_apply;
};

/* A record, in reverse order, of all scheduled insns which have delay slots
   and may require backtracking.  */
static struct haifa_saved_data *backtrack_queue;

/* For every dependency of INSN, set the FEEDS_BACKTRACK_INSN bit according
   to SET_P.  */
static void
mark_backtrack_feeds (rtx insn, int set_p)
{
  sd_iterator_def sd_it;
  dep_t dep;
  FOR_EACH_DEP (insn, SD_LIST_HARD_BACK, sd_it, dep)
    {
      FEEDS_BACKTRACK_INSN (DEP_PRO (dep)) = set_p;
    }
}

/* Save the current scheduler state so that we can backtrack to it
   later if necessary.  PAIR gives the insns that make it necessary to
   save this point.  SCHED_BLOCK is the local state of schedule_block
   that need to be saved.  */
static void
save_backtrack_point (struct delay_pair *pair,
		      struct sched_block_state sched_block)
{
  int i;
  struct haifa_saved_data *save = XNEW (struct haifa_saved_data);

  save->curr_state = xmalloc (dfa_state_size);
  memcpy (save->curr_state, curr_state, dfa_state_size);

  save->ready.first = ready.first;
  save->ready.n_ready = ready.n_ready;
  save->ready.n_debug = ready.n_debug;
  save->ready.veclen = ready.veclen;
  save->ready.vec = XNEWVEC (rtx, ready.veclen);
  memcpy (save->ready.vec, ready.vec, ready.veclen * sizeof (rtx));

  save->insn_queue = XNEWVEC (rtx, max_insn_queue_index + 1);
  save->q_size = q_size;
  for (i = 0; i <= max_insn_queue_index; i++)
    {
      int q = NEXT_Q_AFTER (q_ptr, i);
      save->insn_queue[i] = copy_INSN_LIST (insn_queue[q]);
    }

  save->clock_var = clock_var;
  save->last_clock_var = last_clock_var;
  save->cycle_issued_insns = cycle_issued_insns;
  save->last_scheduled_insn = last_scheduled_insn;
  save->last_nondebug_scheduled_insn = last_nondebug_scheduled_insn;

  save->sched_block = sched_block;

  save->replacement_deps.create (0);
  save->replace_apply.create (0);
  save->next_cycle_deps = next_cycle_replace_deps.copy ();
  save->next_cycle_apply = next_cycle_apply.copy ();

  if (current_sched_info->save_state)
    save->fe_saved_data = (*current_sched_info->save_state) ();

  if (targetm.sched.alloc_sched_context)
    {
      save->be_saved_data = targetm.sched.alloc_sched_context ();
      targetm.sched.init_sched_context (save->be_saved_data, false);
    }
  else
    save->be_saved_data = NULL;

  save->delay_pair = pair;

  save->next = backtrack_queue;
  backtrack_queue = save;

  while (pair)
    {
      mark_backtrack_feeds (pair->i2, 1);
      INSN_TICK (pair->i2) = INVALID_TICK;
      INSN_EXACT_TICK (pair->i2) = clock_var + pair_delay (pair);
      SHADOW_P (pair->i2) = pair->stages == 0;
      pair = pair->next_same_i1;
    }
}

/* Walk the ready list and all queues. If any insns have unresolved backwards
   dependencies, these must be cancelled deps, broken by predication.  Set or
   clear (depending on SET) the DEP_CANCELLED bit in DEP_STATUS.  */

static void
toggle_cancelled_flags (bool set)
{
  int i;
  sd_iterator_def sd_it;
  dep_t dep;

  if (ready.n_ready > 0)
    {
      rtx *first = ready_lastpos (&ready);
      for (i = 0; i < ready.n_ready; i++)
	FOR_EACH_DEP (first[i], SD_LIST_BACK, sd_it, dep)
	  if (!DEBUG_INSN_P (DEP_PRO (dep)))
	    {
	      if (set)
		DEP_STATUS (dep) |= DEP_CANCELLED;
	      else
		DEP_STATUS (dep) &= ~DEP_CANCELLED;
	    }
    }
  for (i = 0; i <= max_insn_queue_index; i++)
    {
      int q = NEXT_Q_AFTER (q_ptr, i);
      rtx link;
      for (link = insn_queue[q]; link; link = XEXP (link, 1))
	{
	  rtx insn = XEXP (link, 0);
	  FOR_EACH_DEP (insn, SD_LIST_BACK, sd_it, dep)
	    if (!DEBUG_INSN_P (DEP_PRO (dep)))
	      {
		if (set)
		  DEP_STATUS (dep) |= DEP_CANCELLED;
		else
		  DEP_STATUS (dep) &= ~DEP_CANCELLED;
	      }
	}
    }
}

/* Undo the replacements that have occurred after backtrack point SAVE
   was placed.  */
static void
undo_replacements_for_backtrack (struct haifa_saved_data *save)
{
  while (!save->replacement_deps.is_empty ())
    {
      dep_t dep = save->replacement_deps.pop ();
      int apply_p = save->replace_apply.pop ();

      if (apply_p)
	restore_pattern (dep, true);
      else
	apply_replacement (dep, true);
    }
  save->replacement_deps.release ();
  save->replace_apply.release ();
}

/* Pop entries from the SCHEDULED_INSNS vector up to and including INSN.
   Restore their dependencies to an unresolved state, and mark them as
   queued nowhere.  */

static void
unschedule_insns_until (rtx insn)
{
  vec<rtx> recompute_vec = vNULL;

  /* Make two passes over the insns to be unscheduled.  First, we clear out
     dependencies and other trivial bookkeeping.  */
  for (;;)
    {
      rtx last;
      sd_iterator_def sd_it;
      dep_t dep;

      last = scheduled_insns.pop ();

      /* This will be changed by restore_backtrack_point if the insn is in
	 any queue.  */
      QUEUE_INDEX (last) = QUEUE_NOWHERE;
      if (last != insn)
	INSN_TICK (last) = INVALID_TICK;

      if (modulo_ii > 0 && INSN_UID (last) < modulo_iter0_max_uid)
	modulo_insns_scheduled--;

      for (sd_it = sd_iterator_start (last, SD_LIST_RES_FORW);
	   sd_iterator_cond (&sd_it, &dep);)
	{
	  rtx con = DEP_CON (dep);
	  sd_unresolve_dep (sd_it);
	  if (!MUST_RECOMPUTE_SPEC_P (con))
	    {
	      MUST_RECOMPUTE_SPEC_P (con) = 1;
	      recompute_vec.safe_push (con);
	    }
	}

      if (last == insn)
	break;
    }

  /* A second pass, to update ready and speculation status for insns
     depending on the unscheduled ones.  The first pass must have
     popped the scheduled_insns vector up to the point where we
     restart scheduling, as recompute_todo_spec requires it to be
     up-to-date.  */
  while (!recompute_vec.is_empty ())
    {
      rtx con;

      con = recompute_vec.pop ();
      MUST_RECOMPUTE_SPEC_P (con) = 0;
      if (!sd_lists_empty_p (con, SD_LIST_HARD_BACK))
	{
	  TODO_SPEC (con) = HARD_DEP;
	  INSN_TICK (con) = INVALID_TICK;
	  if (PREDICATED_PAT (con) != NULL_RTX)
	    haifa_change_pattern (con, ORIG_PAT (con));
	}
      else if (QUEUE_INDEX (con) != QUEUE_SCHEDULED)
	TODO_SPEC (con) = recompute_todo_spec (con, true);
    }
  recompute_vec.release ();
}

/* Restore scheduler state from the topmost entry on the backtracking queue.
   PSCHED_BLOCK_P points to the local data of schedule_block that we must
   overwrite with the saved data.
   The caller must already have called unschedule_insns_until.  */

static void
restore_last_backtrack_point (struct sched_block_state *psched_block)
{
  rtx link;
  int i;
  struct haifa_saved_data *save = backtrack_queue;

  backtrack_queue = save->next;

  if (current_sched_info->restore_state)
    (*current_sched_info->restore_state) (save->fe_saved_data);

  if (targetm.sched.alloc_sched_context)
    {
      targetm.sched.set_sched_context (save->be_saved_data);
      targetm.sched.free_sched_context (save->be_saved_data);
    }

  /* Do this first since it clobbers INSN_TICK of the involved
     instructions.  */
  undo_replacements_for_backtrack (save);

  /* Clear the QUEUE_INDEX of everything in the ready list or one
     of the queues.  */
  if (ready.n_ready > 0)
    {
      rtx *first = ready_lastpos (&ready);
      for (i = 0; i < ready.n_ready; i++)
	{
	  rtx insn = first[i];
	  QUEUE_INDEX (insn) = QUEUE_NOWHERE;
	  INSN_TICK (insn) = INVALID_TICK;
	}
    }
  for (i = 0; i <= max_insn_queue_index; i++)
    {
      int q = NEXT_Q_AFTER (q_ptr, i);

      for (link = insn_queue[q]; link; link = XEXP (link, 1))
	{
	  rtx x = XEXP (link, 0);
	  QUEUE_INDEX (x) = QUEUE_NOWHERE;
	  INSN_TICK (x) = INVALID_TICK;
	}
      free_INSN_LIST_list (&insn_queue[q]);
    }

  free (ready.vec);
  ready = save->ready;

  if (ready.n_ready > 0)
    {
      rtx *first = ready_lastpos (&ready);
      for (i = 0; i < ready.n_ready; i++)
	{
	  rtx insn = first[i];
	  QUEUE_INDEX (insn) = QUEUE_READY;
	  TODO_SPEC (insn) = recompute_todo_spec (insn, true);
	  INSN_TICK (insn) = save->clock_var;
	}
    }

  q_ptr = 0;
  q_size = save->q_size;
  for (i = 0; i <= max_insn_queue_index; i++)
    {
      int q = NEXT_Q_AFTER (q_ptr, i);

      insn_queue[q] = save->insn_queue[q];

      for (link = insn_queue[q]; link; link = XEXP (link, 1))
	{
	  rtx x = XEXP (link, 0);
	  QUEUE_INDEX (x) = i;
	  TODO_SPEC (x) = recompute_todo_spec (x, true);
	  INSN_TICK (x) = save->clock_var + i;
	}
    }
  free (save->insn_queue);

  toggle_cancelled_flags (true);

  clock_var = save->clock_var;
  last_clock_var = save->last_clock_var;
  cycle_issued_insns = save->cycle_issued_insns;
  last_scheduled_insn = save->last_scheduled_insn;
  last_nondebug_scheduled_insn = save->last_nondebug_scheduled_insn;

  *psched_block = save->sched_block;

  memcpy (curr_state, save->curr_state, dfa_state_size);
  free (save->curr_state);

  mark_backtrack_feeds (save->delay_pair->i2, 0);

  gcc_assert (next_cycle_replace_deps.is_empty ());
  next_cycle_replace_deps = save->next_cycle_deps.copy ();
  next_cycle_apply = save->next_cycle_apply.copy ();

  free (save);

  for (save = backtrack_queue; save; save = save->next)
    {
      mark_backtrack_feeds (save->delay_pair->i2, 1);
    }
}

/* Discard all data associated with the topmost entry in the backtrack
   queue.  If RESET_TICK is false, we just want to free the data.  If true,
   we are doing this because we discovered a reason to backtrack.  In the
   latter case, also reset the INSN_TICK for the shadow insn.  */
static void
free_topmost_backtrack_point (bool reset_tick)
{
  struct haifa_saved_data *save = backtrack_queue;
  int i;

  backtrack_queue = save->next;

  if (reset_tick)
    {
      struct delay_pair *pair = save->delay_pair;
      while (pair)
	{
	  INSN_TICK (pair->i2) = INVALID_TICK;
	  INSN_EXACT_TICK (pair->i2) = INVALID_TICK;
	  pair = pair->next_same_i1;
	}
      undo_replacements_for_backtrack (save);
    }
  else
    {
      save->replacement_deps.release ();
      save->replace_apply.release ();
    }

  if (targetm.sched.free_sched_context)
    targetm.sched.free_sched_context (save->be_saved_data);
  if (current_sched_info->restore_state)
    free (save->fe_saved_data);
  for (i = 0; i <= max_insn_queue_index; i++)
    free_INSN_LIST_list (&save->insn_queue[i]);
  free (save->insn_queue);
  free (save->curr_state);
  free (save->ready.vec);
  free (save);
}

/* Free the entire backtrack queue.  */
static void
free_backtrack_queue (void)
{
  while (backtrack_queue)
    free_topmost_backtrack_point (false);
}

/* Apply a replacement described by DESC.  If IMMEDIATELY is false, we
   may have to postpone the replacement until the start of the next cycle,
   at which point we will be called again with IMMEDIATELY true.  This is
   only done for machines which have instruction packets with explicit
   parallelism however.  */
static void
apply_replacement (dep_t dep, bool immediately)
{
  struct dep_replacement *desc = DEP_REPLACE (dep);
  if (!immediately && targetm.sched.exposed_pipeline && reload_completed)
    {
      next_cycle_replace_deps.safe_push (dep);
      next_cycle_apply.safe_push (1);
    }
  else
    {
      bool success;

      if (QUEUE_INDEX (desc->insn) == QUEUE_SCHEDULED)
	return;

      if (sched_verbose >= 5)
	fprintf (sched_dump, "applying replacement for insn %d\n",
		 INSN_UID (desc->insn));

      success = validate_change (desc->insn, desc->loc, desc->newval, 0);
      gcc_assert (success);

      update_insn_after_change (desc->insn);
      if ((TODO_SPEC (desc->insn) & (HARD_DEP | DEP_POSTPONED)) == 0)
	fix_tick_ready (desc->insn);

      if (backtrack_queue != NULL)
	{
	  backtrack_queue->replacement_deps.safe_push (dep);
	  backtrack_queue->replace_apply.safe_push (1);
	}
    }
}

/* We have determined that a pattern involved in DEP must be restored.
   If IMMEDIATELY is false, we may have to postpone the replacement
   until the start of the next cycle, at which point we will be called
   again with IMMEDIATELY true.  */
static void
restore_pattern (dep_t dep, bool immediately)
{
  rtx next = DEP_CON (dep);
  int tick = INSN_TICK (next);

  /* If we already scheduled the insn, the modified version is
     correct.  */
  if (QUEUE_INDEX (next) == QUEUE_SCHEDULED)
    return;

  if (!immediately && targetm.sched.exposed_pipeline && reload_completed)
    {
      next_cycle_replace_deps.safe_push (dep);
      next_cycle_apply.safe_push (0);
      return;
    }


  if (DEP_TYPE (dep) == REG_DEP_CONTROL)
    {
      if (sched_verbose >= 5)
	fprintf (sched_dump, "restoring pattern for insn %d\n",
		 INSN_UID (next));
      haifa_change_pattern (next, ORIG_PAT (next));
    }
  else
    {
      struct dep_replacement *desc = DEP_REPLACE (dep);
      bool success;

      if (sched_verbose >= 5)
	fprintf (sched_dump, "restoring pattern for insn %d\n",
		 INSN_UID (desc->insn));
      tick = INSN_TICK (desc->insn);

      success = validate_change (desc->insn, desc->loc, desc->orig, 0);
      gcc_assert (success);
      update_insn_after_change (desc->insn);
      if (backtrack_queue != NULL)
	{
	  backtrack_queue->replacement_deps.safe_push (dep);
	  backtrack_queue->replace_apply.safe_push (0);
	}
    }
  INSN_TICK (next) = tick;
  if (TODO_SPEC (next) == DEP_POSTPONED)
    return;

  if (sd_lists_empty_p (next, SD_LIST_BACK))
    TODO_SPEC (next) = 0;
  else if (!sd_lists_empty_p (next, SD_LIST_HARD_BACK))
    TODO_SPEC (next) = HARD_DEP;
}

/* Perform pattern replacements that were queued up until the next
   cycle.  */
static void
perform_replacements_new_cycle (void)
{
  int i;
  dep_t dep;
  FOR_EACH_VEC_ELT (next_cycle_replace_deps, i, dep)
    {
      int apply_p = next_cycle_apply[i];
      if (apply_p)
	apply_replacement (dep, true);
      else
	restore_pattern (dep, true);
    }
  next_cycle_replace_deps.truncate (0);
  next_cycle_apply.truncate (0);
}

/* Compute INSN_TICK_ESTIMATE for INSN.  PROCESSED is a bitmap of
   instructions we've previously encountered, a set bit prevents
   recursion.  BUDGET is a limit on how far ahead we look, it is
   reduced on recursive calls.  Return true if we produced a good
   estimate, or false if we exceeded the budget.  */
static bool
estimate_insn_tick (bitmap processed, rtx insn, int budget)
{
  sd_iterator_def sd_it;
  dep_t dep;
  int earliest = INSN_TICK (insn);

  FOR_EACH_DEP (insn, SD_LIST_BACK, sd_it, dep)
    {
      rtx pro = DEP_PRO (dep);
      int t;

      if (DEP_STATUS (dep) & DEP_CANCELLED)
	continue;

      if (QUEUE_INDEX (pro) == QUEUE_SCHEDULED)
	gcc_assert (INSN_TICK (pro) + dep_cost (dep) <= INSN_TICK (insn));
      else
	{
	  int cost = dep_cost (dep);
	  if (cost >= budget)
	    return false;
	  if (!bitmap_bit_p (processed, INSN_LUID (pro)))
	    {
	      if (!estimate_insn_tick (processed, pro, budget - cost))
		return false;
	    }
	  gcc_assert (INSN_TICK_ESTIMATE (pro) != INVALID_TICK);
	  t = INSN_TICK_ESTIMATE (pro) + cost;
	  if (earliest == INVALID_TICK || t > earliest)
	    earliest = t;
	}
    }
  bitmap_set_bit (processed, INSN_LUID (insn));
  INSN_TICK_ESTIMATE (insn) = earliest;
  return true;
}

/* Examine the pair of insns in P, and estimate (optimistically, assuming
   infinite resources) the cycle in which the delayed shadow can be issued.
   Return the number of cycles that must pass before the real insn can be
   issued in order to meet this constraint.  */
static int
estimate_shadow_tick (struct delay_pair *p)
{
  bitmap_head processed;
  int t;
  bool cutoff;
  bitmap_initialize (&processed, 0);

  cutoff = !estimate_insn_tick (&processed, p->i2,
				max_insn_queue_index + pair_delay (p));
  bitmap_clear (&processed);
  if (cutoff)
    return max_insn_queue_index;
  t = INSN_TICK_ESTIMATE (p->i2) - (clock_var + pair_delay (p) + 1);
  if (t > 0)
    return t;
  return 0;
}

/* If INSN has no unresolved backwards dependencies, add it to the schedule and
   recursively resolve all its forward dependencies.  */
static void
resolve_dependencies (rtx insn)
{
  sd_iterator_def sd_it;
  dep_t dep;

  /* Don't use sd_lists_empty_p; it ignores debug insns.  */
  if (DEPS_LIST_FIRST (INSN_HARD_BACK_DEPS (insn)) != NULL
      || DEPS_LIST_FIRST (INSN_SPEC_BACK_DEPS (insn)) != NULL)
    return;

  if (sched_verbose >= 4)
    fprintf (sched_dump, ";;\tquickly resolving %d\n", INSN_UID (insn));

  if (QUEUE_INDEX (insn) >= 0)
    queue_remove (insn);

  scheduled_insns.safe_push (insn);

  /* Update dependent instructions.  */
  for (sd_it = sd_iterator_start (insn, SD_LIST_FORW);
       sd_iterator_cond (&sd_it, &dep);)
    {
      rtx next = DEP_CON (dep);

      if (sched_verbose >= 4)
	fprintf (sched_dump, ";;\t\tdep %d against %d\n", INSN_UID (insn),
		 INSN_UID (next));

      /* Resolve the dependence between INSN and NEXT.
	 sd_resolve_dep () moves current dep to another list thus
	 advancing the iterator.  */
      sd_resolve_dep (sd_it);

      if (!IS_SPECULATION_BRANCHY_CHECK_P (insn))
	{
	  resolve_dependencies (next);
	}
      else
	/* Check always has only one forward dependence (to the first insn in
	   the recovery block), therefore, this will be executed only once.  */
	{
	  gcc_assert (sd_lists_empty_p (insn, SD_LIST_FORW));
	}
    }
}


/* Return the head and tail pointers of ebb starting at BEG and ending
   at END.  */
void
get_ebb_head_tail (basic_block beg, basic_block end, rtx *headp, rtx *tailp)
{
  rtx beg_head = BB_HEAD (beg);
  rtx beg_tail = BB_END (beg);
  rtx end_head = BB_HEAD (end);
  rtx end_tail = BB_END (end);

  /* Don't include any notes or labels at the beginning of the BEG
     basic block, or notes at the end of the END basic blocks.  */

  if (LABEL_P (beg_head))
    beg_head = NEXT_INSN (beg_head);

  while (beg_head != beg_tail)
    if (NOTE_P (beg_head))
      beg_head = NEXT_INSN (beg_head);
    else if (DEBUG_INSN_P (beg_head))
      {
	rtx note, next;

	for (note = NEXT_INSN (beg_head);
	     note != beg_tail;
	     note = next)
	  {
	    next = NEXT_INSN (note);
	    if (NOTE_P (note))
	      {
		if (sched_verbose >= 9)
		  fprintf (sched_dump, "reorder %i\n", INSN_UID (note));

		reorder_insns_nobb (note, note, PREV_INSN (beg_head));

		if (BLOCK_FOR_INSN (note) != beg)
		  df_insn_change_bb (note, beg);
	      }
	    else if (!DEBUG_INSN_P (note))
	      break;
	  }

	break;
      }
    else
      break;

  *headp = beg_head;

  if (beg == end)
    end_head = beg_head;
  else if (LABEL_P (end_head))
    end_head = NEXT_INSN (end_head);

  while (end_head != end_tail)
    if (NOTE_P (end_tail))
      end_tail = PREV_INSN (end_tail);
    else if (DEBUG_INSN_P (end_tail))
      {
	rtx note, prev;

	for (note = PREV_INSN (end_tail);
	     note != end_head;
	     note = prev)
	  {
	    prev = PREV_INSN (note);
	    if (NOTE_P (note))
	      {
		if (sched_verbose >= 9)
		  fprintf (sched_dump, "reorder %i\n", INSN_UID (note));

		reorder_insns_nobb (note, note, end_tail);

		if (end_tail == BB_END (end))
		  BB_END (end) = note;

		if (BLOCK_FOR_INSN (note) != end)
		  df_insn_change_bb (note, end);
	      }
	    else if (!DEBUG_INSN_P (note))
	      break;
	  }

	break;
      }
    else
      break;

  *tailp = end_tail;
}

/* Return nonzero if there are no real insns in the range [ HEAD, TAIL ].  */

int
no_real_insns_p (const_rtx head, const_rtx tail)
{
  while (head != NEXT_INSN (tail))
    {
      if (!NOTE_P (head) && !LABEL_P (head))
	return 0;
      head = NEXT_INSN (head);
    }
  return 1;
}

/* Restore-other-notes: NOTE_LIST is the end of a chain of notes
   previously found among the insns.  Insert them just before HEAD.  */
rtx
restore_other_notes (rtx head, basic_block head_bb)
{
  if (note_list != 0)
    {
      rtx note_head = note_list;

      if (head)
	head_bb = BLOCK_FOR_INSN (head);
      else
	head = NEXT_INSN (bb_note (head_bb));

      while (PREV_INSN (note_head))
	{
	  set_block_for_insn (note_head, head_bb);
	  note_head = PREV_INSN (note_head);
	}
      /* In the above cycle we've missed this note.  */
      set_block_for_insn (note_head, head_bb);

      PREV_INSN (note_head) = PREV_INSN (head);
      NEXT_INSN (PREV_INSN (head)) = note_head;
      PREV_INSN (head) = note_list;
      NEXT_INSN (note_list) = head;

      if (BLOCK_FOR_INSN (head) != head_bb)
	BB_END (head_bb) = note_list;

      head = note_head;
    }

  return head;
}

/* When we know we are going to discard the schedule due to a failed attempt
   at modulo scheduling, undo all replacements.  */
static void
undo_all_replacements (void)
{
  rtx insn;
  int i;

  FOR_EACH_VEC_ELT (scheduled_insns, i, insn)
    {
      sd_iterator_def sd_it;
      dep_t dep;

      /* See if we must undo a replacement.  */
      for (sd_it = sd_iterator_start (insn, SD_LIST_RES_FORW);
	   sd_iterator_cond (&sd_it, &dep); sd_iterator_next (&sd_it))
	{
	  struct dep_replacement *desc = DEP_REPLACE (dep);
	  if (desc != NULL)
	    validate_change (desc->insn, desc->loc, desc->orig, 0);
	}
    }
}

/* Move insns that became ready to fire from queue to ready list.  */

static void
queue_to_ready (struct ready_list *ready)
{
  rtx insn;
  rtx link;
  rtx skip_insn;

  q_ptr = NEXT_Q (q_ptr);

  if (dbg_cnt (sched_insn) == false)
    {
      /* If debug counter is activated do not requeue the first
	 nonscheduled insn.  */
      skip_insn = nonscheduled_insns_begin;
      do
	{
	  skip_insn = next_nonnote_nondebug_insn (skip_insn);
	}
      while (QUEUE_INDEX (skip_insn) == QUEUE_SCHEDULED);
    }
  else
    skip_insn = NULL_RTX;

  /* Add all pending insns that can be scheduled without stalls to the
     ready list.  */
  for (link = insn_queue[q_ptr]; link; link = XEXP (link, 1))
    {
      insn = XEXP (link, 0);
      q_size -= 1;

      if (sched_verbose >= 2)
	fprintf (sched_dump, ";;\t\tQ-->Ready: insn %s: ",
		 (*current_sched_info->print_insn) (insn, 0));

      /* If the ready list is full, delay the insn for 1 cycle.
	 See the comment in schedule_block for the rationale.  */
      if (!reload_completed
	  && (ready->n_ready - ready->n_debug > MAX_SCHED_READY_INSNS
	      || (sched_pressure == SCHED_PRESSURE_MODEL
		  /* Limit pressure recalculations to MAX_SCHED_READY_INSNS
		     instructions too.  */
		  && model_index (insn) > (model_curr_point
					   + MAX_SCHED_READY_INSNS)))
	  && !(sched_pressure == SCHED_PRESSURE_MODEL
	       && model_curr_point < model_num_insns
	       /* Always allow the next model instruction to issue.  */
	       && model_index (insn) == model_curr_point)
	  && !SCHED_GROUP_P (insn)
	  && insn != skip_insn)
	queue_insn (insn, 1, "ready full");
      else
	{
	  ready_add (ready, insn, false);
	  if (sched_verbose >= 2)
	    fprintf (sched_dump, "moving to ready without stalls\n");
        }
    }
  free_INSN_LIST_list (&insn_queue[q_ptr]);

  /* If there are no ready insns, stall until one is ready and add all
     of the pending insns at that point to the ready list.  */
  if (ready->n_ready == 0)
    {
      int stalls;

      for (stalls = 1; stalls <= max_insn_queue_index; stalls++)
	{
	  if ((link = insn_queue[NEXT_Q_AFTER (q_ptr, stalls)]))
	    {
	      for (; link; link = XEXP (link, 1))
		{
		  insn = XEXP (link, 0);
		  q_size -= 1;

		  if (sched_verbose >= 2)
		    fprintf (sched_dump, ";;\t\tQ-->Ready: insn %s: ",
			     (*current_sched_info->print_insn) (insn, 0));

		  ready_add (ready, insn, false);
		  if (sched_verbose >= 2)
		    fprintf (sched_dump, "moving to ready with %d stalls\n", stalls);
		}
	      free_INSN_LIST_list (&insn_queue[NEXT_Q_AFTER (q_ptr, stalls)]);

	      advance_one_cycle ();

	      break;
	    }

	  advance_one_cycle ();
	}

      q_ptr = NEXT_Q_AFTER (q_ptr, stalls);
      clock_var += stalls;
    }
}

/* Used by early_queue_to_ready.  Determines whether it is "ok" to
   prematurely move INSN from the queue to the ready list.  Currently,
   if a target defines the hook 'is_costly_dependence', this function
   uses the hook to check whether there exist any dependences which are
   considered costly by the target, between INSN and other insns that
   have already been scheduled.  Dependences are checked up to Y cycles
   back, with default Y=1; The flag -fsched-stalled-insns-dep=Y allows
   controlling this value.
   (Other considerations could be taken into account instead (or in
   addition) depending on user flags and target hooks.  */

static bool
ok_for_early_queue_removal (rtx insn)
{
  if (targetm.sched.is_costly_dependence)
    {
      rtx prev_insn;
      int n_cycles;
      int i = scheduled_insns.length ();
      for (n_cycles = flag_sched_stalled_insns_dep; n_cycles; n_cycles--)
	{
	  while (i-- > 0)
	    {
	      int cost;

	      prev_insn = scheduled_insns[i];

	      if (!NOTE_P (prev_insn))
		{
		  dep_t dep;

		  dep = sd_find_dep_between (prev_insn, insn, true);

		  if (dep != NULL)
		    {
		      cost = dep_cost (dep);

		      if (targetm.sched.is_costly_dependence (dep, cost,
				flag_sched_stalled_insns_dep - n_cycles))
			return false;
		    }
		}

	      if (GET_MODE (prev_insn) == TImode) /* end of dispatch group */
		break;
	    }

	  if (i == 0)
	    break;
	}
    }

  return true;
}


/* Remove insns from the queue, before they become "ready" with respect
   to FU latency considerations.  */

static int
early_queue_to_ready (state_t state, struct ready_list *ready)
{
  rtx insn;
  rtx link;
  rtx next_link;
  rtx prev_link;
  bool move_to_ready;
  int cost;
  state_t temp_state = alloca (dfa_state_size);
  int stalls;
  int insns_removed = 0;

  /*
     Flag '-fsched-stalled-insns=X' determines the aggressiveness of this
     function:

     X == 0: There is no limit on how many queued insns can be removed
             prematurely.  (flag_sched_stalled_insns = -1).

     X >= 1: Only X queued insns can be removed prematurely in each
	     invocation.  (flag_sched_stalled_insns = X).

     Otherwise: Early queue removal is disabled.
         (flag_sched_stalled_insns = 0)
  */

  if (! flag_sched_stalled_insns)
    return 0;

  for (stalls = 0; stalls <= max_insn_queue_index; stalls++)
    {
      if ((link = insn_queue[NEXT_Q_AFTER (q_ptr, stalls)]))
	{
	  if (sched_verbose > 6)
	    fprintf (sched_dump, ";; look at index %d + %d\n", q_ptr, stalls);

	  prev_link = 0;
	  while (link)
	    {
	      next_link = XEXP (link, 1);
	      insn = XEXP (link, 0);
	      if (insn && sched_verbose > 6)
		print_rtl_single (sched_dump, insn);

	      memcpy (temp_state, state, dfa_state_size);
	      if (recog_memoized (insn) < 0)
		/* non-negative to indicate that it's not ready
		   to avoid infinite Q->R->Q->R... */
		cost = 0;
	      else
		cost = state_transition (temp_state, insn);

	      if (sched_verbose >= 6)
		fprintf (sched_dump, "transition cost = %d\n", cost);

	      move_to_ready = false;
	      if (cost < 0)
		{
		  move_to_ready = ok_for_early_queue_removal (insn);
		  if (move_to_ready == true)
		    {
		      /* move from Q to R */
		      q_size -= 1;
		      ready_add (ready, insn, false);

		      if (prev_link)
			XEXP (prev_link, 1) = next_link;
		      else
			insn_queue[NEXT_Q_AFTER (q_ptr, stalls)] = next_link;

		      free_INSN_LIST_node (link);

		      if (sched_verbose >= 2)
			fprintf (sched_dump, ";;\t\tEarly Q-->Ready: insn %s\n",
				 (*current_sched_info->print_insn) (insn, 0));

		      insns_removed++;
		      if (insns_removed == flag_sched_stalled_insns)
			/* Remove no more than flag_sched_stalled_insns insns
			   from Q at a time.  */
			return insns_removed;
		    }
		}

	      if (move_to_ready == false)
		prev_link = link;

	      link = next_link;
	    } /* while link */
	} /* if link */

    } /* for stalls.. */

  return insns_removed;
}


/* Print the ready list for debugging purposes.  Callable from debugger.  */

static void
debug_ready_list (struct ready_list *ready)
{
  rtx *p;
  int i;

  if (ready->n_ready == 0)
    {
      fprintf (sched_dump, "\n");
      return;
    }

  p = ready_lastpos (ready);
  for (i = 0; i < ready->n_ready; i++)
    {
      fprintf (sched_dump, "  %s:%d",
	       (*current_sched_info->print_insn) (p[i], 0),
	       INSN_LUID (p[i]));
      if (sched_pressure != SCHED_PRESSURE_NONE)
	fprintf (sched_dump, "(cost=%d",
		 INSN_REG_PRESSURE_EXCESS_COST_CHANGE (p[i]));
      if (INSN_TICK (p[i]) > clock_var)
	fprintf (sched_dump, ":delay=%d", INSN_TICK (p[i]) - clock_var);
      if (sched_pressure != SCHED_PRESSURE_NONE)
	fprintf (sched_dump, ")");
    }
  fprintf (sched_dump, "\n");
}

/* Search INSN for REG_SAVE_NOTE notes and convert them back into insn
   NOTEs.  This is used for NOTE_INSN_EPILOGUE_BEG, so that sched-ebb
   replaces the epilogue note in the correct basic block.  */
void
reemit_notes (rtx insn)
{
  rtx note, last = insn;

  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
    {
      if (REG_NOTE_KIND (note) == REG_SAVE_NOTE)
	{
	  enum insn_note note_type = (enum insn_note) INTVAL (XEXP (note, 0));

	  last = emit_note_before (note_type, last);
	  remove_note (insn, note);
	}
    }
}

/* Move INSN.  Reemit notes if needed.  Update CFG, if needed.  */
static void
move_insn (rtx insn, rtx last, rtx nt)
{
  if (PREV_INSN (insn) != last)
    {
      basic_block bb;
      rtx note;
      int jump_p = 0;

      bb = BLOCK_FOR_INSN (insn);

      /* BB_HEAD is either LABEL or NOTE.  */
      gcc_assert (BB_HEAD (bb) != insn);

      if (BB_END (bb) == insn)
	/* If this is last instruction in BB, move end marker one
	   instruction up.  */
	{
	  /* Jumps are always placed at the end of basic block.  */
	  jump_p = control_flow_insn_p (insn);

	  gcc_assert (!jump_p
		      || ((common_sched_info->sched_pass_id == SCHED_RGN_PASS)
			  && IS_SPECULATION_BRANCHY_CHECK_P (insn))
		      || (common_sched_info->sched_pass_id
			  == SCHED_EBB_PASS));

	  gcc_assert (BLOCK_FOR_INSN (PREV_INSN (insn)) == bb);

	  BB_END (bb) = PREV_INSN (insn);
	}

      gcc_assert (BB_END (bb) != last);

      if (jump_p)
	/* We move the block note along with jump.  */
	{
	  gcc_assert (nt);

	  note = NEXT_INSN (insn);
	  while (NOTE_NOT_BB_P (note) && note != nt)
	    note = NEXT_INSN (note);

	  if (note != nt
	      && (LABEL_P (note)
		  || BARRIER_P (note)))
	    note = NEXT_INSN (note);

	  gcc_assert (NOTE_INSN_BASIC_BLOCK_P (note));
	}
      else
	note = insn;

      NEXT_INSN (PREV_INSN (insn)) = NEXT_INSN (note);
      PREV_INSN (NEXT_INSN (note)) = PREV_INSN (insn);

      NEXT_INSN (note) = NEXT_INSN (last);
      PREV_INSN (NEXT_INSN (last)) = note;

      NEXT_INSN (last) = insn;
      PREV_INSN (insn) = last;

      bb = BLOCK_FOR_INSN (last);

      if (jump_p)
	{
	  fix_jump_move (insn);

	  if (BLOCK_FOR_INSN (insn) != bb)
	    move_block_after_check (insn);

	  gcc_assert (BB_END (bb) == last);
	}

      df_insn_change_bb (insn, bb);

      /* Update BB_END, if needed.  */
      if (BB_END (bb) == last)
	BB_END (bb) = insn;
    }

  SCHED_GROUP_P (insn) = 0;
}

/* Return true if scheduling INSN will finish current clock cycle.  */
static bool
insn_finishes_cycle_p (rtx insn)
{
  if (SCHED_GROUP_P (insn))
    /* After issuing INSN, rest of the sched_group will be forced to issue
       in order.  Don't make any plans for the rest of cycle.  */
    return true;

  /* Finishing the block will, apparently, finish the cycle.  */
  if (current_sched_info->insn_finishes_block_p
      && current_sched_info->insn_finishes_block_p (insn))
    return true;

  return false;
}

/* Define type for target data used in multipass scheduling.  */
#ifndef TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DATA_T
# define TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DATA_T int
#endif
typedef TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DATA_T first_cycle_multipass_data_t;

/* The following structure describe an entry of the stack of choices.  */
struct choice_entry
{
  /* Ordinal number of the issued insn in the ready queue.  */
  int index;
  /* The number of the rest insns whose issues we should try.  */
  int rest;
  /* The number of issued essential insns.  */
  int n;
  /* State after issuing the insn.  */
  state_t state;
  /* Target-specific data.  */
  first_cycle_multipass_data_t target_data;
};

/* The following array is used to implement a stack of choices used in
   function max_issue.  */
static struct choice_entry *choice_stack;

/* This holds the value of the target dfa_lookahead hook.  */
int dfa_lookahead;

/* The following variable value is maximal number of tries of issuing
   insns for the first cycle multipass insn scheduling.  We define
   this value as constant*(DFA_LOOKAHEAD**ISSUE_RATE).  We would not
   need this constraint if all real insns (with non-negative codes)
   had reservations because in this case the algorithm complexity is
   O(DFA_LOOKAHEAD**ISSUE_RATE).  Unfortunately, the dfa descriptions
   might be incomplete and such insn might occur.  For such
   descriptions, the complexity of algorithm (without the constraint)
   could achieve DFA_LOOKAHEAD ** N , where N is the queue length.  */
static int max_lookahead_tries;

/* The following value is value of hook
   `first_cycle_multipass_dfa_lookahead' at the last call of
   `max_issue'.  */
static int cached_first_cycle_multipass_dfa_lookahead = 0;

/* The following value is value of `issue_rate' at the last call of
   `sched_init'.  */
static int cached_issue_rate = 0;

/* The following function returns maximal (or close to maximal) number
   of insns which can be issued on the same cycle and one of which
   insns is insns with the best rank (the first insn in READY).  To
   make this function tries different samples of ready insns.  READY
   is current queue `ready'.  Global array READY_TRY reflects what
   insns are already issued in this try.  The function stops immediately,
   if it reached the such a solution, that all instruction can be issued.
   INDEX will contain index of the best insn in READY.  The following
   function is used only for first cycle multipass scheduling.

   PRIVILEGED_N >= 0

   This function expects recognized insns only.  All USEs,
   CLOBBERs, etc must be filtered elsewhere.  */
int
max_issue (struct ready_list *ready, int privileged_n, state_t state,
	   bool first_cycle_insn_p, int *index)
{
  int n, i, all, n_ready, best, delay, tries_num;
  int more_issue;
  struct choice_entry *top;
  rtx insn;

  n_ready = ready->n_ready;
  gcc_assert (dfa_lookahead >= 1 && privileged_n >= 0
	      && privileged_n <= n_ready);

  /* Init MAX_LOOKAHEAD_TRIES.  */
  if (cached_first_cycle_multipass_dfa_lookahead != dfa_lookahead)
    {
      cached_first_cycle_multipass_dfa_lookahead = dfa_lookahead;
      max_lookahead_tries = 100;
      for (i = 0; i < issue_rate; i++)
	max_lookahead_tries *= dfa_lookahead;
    }

  /* Init max_points.  */
  more_issue = issue_rate - cycle_issued_insns;
  gcc_assert (more_issue >= 0);

  /* The number of the issued insns in the best solution.  */
  best = 0;

  top = choice_stack;

  /* Set initial state of the search.  */
  memcpy (top->state, state, dfa_state_size);
  top->rest = dfa_lookahead;
  top->n = 0;
  if (targetm.sched.first_cycle_multipass_begin)
    targetm.sched.first_cycle_multipass_begin (&top->target_data,
					       ready_try, n_ready,
					       first_cycle_insn_p);

  /* Count the number of the insns to search among.  */
  for (all = i = 0; i < n_ready; i++)
    if (!ready_try [i])
      all++;

  /* I is the index of the insn to try next.  */
  i = 0;
  tries_num = 0;
  for (;;)
    {
      if (/* If we've reached a dead end or searched enough of what we have
	     been asked...  */
	  top->rest == 0
	  /* or have nothing else to try...  */
	  || i >= n_ready
	  /* or should not issue more.  */
	  || top->n >= more_issue)
	{
	  /* ??? (... || i == n_ready).  */
	  gcc_assert (i <= n_ready);

	  /* We should not issue more than issue_rate instructions.  */
	  gcc_assert (top->n <= more_issue);

	  if (top == choice_stack)
	    break;

	  if (best < top - choice_stack)
	    {
	      if (privileged_n)
		{
		  n = privileged_n;
		  /* Try to find issued privileged insn.  */
		  while (n && !ready_try[--n])
		    ;
		}

	      if (/* If all insns are equally good...  */
		  privileged_n == 0
		  /* Or a privileged insn will be issued.  */
		  || ready_try[n])
		/* Then we have a solution.  */
		{
		  best = top - choice_stack;
		  /* This is the index of the insn issued first in this
		     solution.  */
		  *index = choice_stack [1].index;
		  if (top->n == more_issue || best == all)
		    break;
		}
	    }

	  /* Set ready-list index to point to the last insn
	     ('i++' below will advance it to the next insn).  */
	  i = top->index;

	  /* Backtrack.  */
	  ready_try [i] = 0;

	  if (targetm.sched.first_cycle_multipass_backtrack)
	    targetm.sched.first_cycle_multipass_backtrack (&top->target_data,
							   ready_try, n_ready);

	  top--;
	  memcpy (state, top->state, dfa_state_size);
	}
      else if (!ready_try [i])
	{
	  tries_num++;
	  if (tries_num > max_lookahead_tries)
	    break;
	  insn = ready_element (ready, i);
	  delay = state_transition (state, insn);
	  if (delay < 0)
	    {
	      if (state_dead_lock_p (state)
		  || insn_finishes_cycle_p (insn))
		/* We won't issue any more instructions in the next
		   choice_state.  */
		top->rest = 0;
	      else
		top->rest--;

	      n = top->n;
	      if (memcmp (top->state, state, dfa_state_size) != 0)
		n++;

	      /* Advance to the next choice_entry.  */
	      top++;
	      /* Initialize it.  */
	      top->rest = dfa_lookahead;
	      top->index = i;
	      top->n = n;
	      memcpy (top->state, state, dfa_state_size);
	      ready_try [i] = 1;

	      if (targetm.sched.first_cycle_multipass_issue)
		targetm.sched.first_cycle_multipass_issue (&top->target_data,
							   ready_try, n_ready,
							   insn,
							   &((top - 1)
							     ->target_data));

	      i = -1;
	    }
	}

      /* Increase ready-list index.  */
      i++;
    }

  if (targetm.sched.first_cycle_multipass_end)
    targetm.sched.first_cycle_multipass_end (best != 0
					     ? &choice_stack[1].target_data
					     : NULL);

  /* Restore the original state of the DFA.  */
  memcpy (state, choice_stack->state, dfa_state_size);

  return best;
}

/* The following function chooses insn from READY and modifies
   READY.  The following function is used only for first
   cycle multipass scheduling.
   Return:
   -1 if cycle should be advanced,
   0 if INSN_PTR is set to point to the desirable insn,
   1 if choose_ready () should be restarted without advancing the cycle.  */
static int
choose_ready (struct ready_list *ready, bool first_cycle_insn_p,
	      rtx *insn_ptr)
{
  int lookahead;

  if (dbg_cnt (sched_insn) == false)
    {
      rtx insn = nonscheduled_insns_begin;
      do
	{
	  insn = next_nonnote_insn (insn);
	}
      while (QUEUE_INDEX (insn) == QUEUE_SCHEDULED);

      if (QUEUE_INDEX (insn) == QUEUE_READY)
	/* INSN is in the ready_list.  */
	{
	  nonscheduled_insns_begin = insn;
	  ready_remove_insn (insn);
	  *insn_ptr = insn;
	  return 0;
	}

      /* INSN is in the queue.  Advance cycle to move it to the ready list.  */
      return -1;
    }

  lookahead = 0;

  if (targetm.sched.first_cycle_multipass_dfa_lookahead)
    lookahead = targetm.sched.first_cycle_multipass_dfa_lookahead ();
  if (lookahead <= 0 || SCHED_GROUP_P (ready_element (ready, 0))
      || DEBUG_INSN_P (ready_element (ready, 0)))
    {
      if (targetm.sched.dispatch (NULL_RTX, IS_DISPATCH_ON))
	*insn_ptr = ready_remove_first_dispatch (ready);
      else
	*insn_ptr = ready_remove_first (ready);

      return 0;
    }
  else
    {
      /* Try to choose the better insn.  */
      int index = 0, i, n;
      rtx insn;
      int try_data = 1, try_control = 1;
      ds_t ts;

      insn = ready_element (ready, 0);
      if (INSN_CODE (insn) < 0)
	{
	  *insn_ptr = ready_remove_first (ready);
	  return 0;
	}

      if (spec_info
	  && spec_info->flags & (PREFER_NON_DATA_SPEC
				 | PREFER_NON_CONTROL_SPEC))
	{
	  for (i = 0, n = ready->n_ready; i < n; i++)
	    {
	      rtx x;
	      ds_t s;

	      x = ready_element (ready, i);
	      s = TODO_SPEC (x);

	      if (spec_info->flags & PREFER_NON_DATA_SPEC
		  && !(s & DATA_SPEC))
		{
		  try_data = 0;
		  if (!(spec_info->flags & PREFER_NON_CONTROL_SPEC)
		      || !try_control)
		    break;
		}

	      if (spec_info->flags & PREFER_NON_CONTROL_SPEC
		  && !(s & CONTROL_SPEC))
		{
		  try_control = 0;
		  if (!(spec_info->flags & PREFER_NON_DATA_SPEC) || !try_data)
		    break;
		}
	    }
	}

      ts = TODO_SPEC (insn);
      if ((ts & SPECULATIVE)
	  && (((!try_data && (ts & DATA_SPEC))
	       || (!try_control && (ts & CONTROL_SPEC)))
	      || (targetm.sched.first_cycle_multipass_dfa_lookahead_guard_spec
		  && !targetm.sched
		  .first_cycle_multipass_dfa_lookahead_guard_spec (insn))))
	/* Discard speculative instruction that stands first in the ready
	   list.  */
	{
	  change_queue_index (insn, 1);
	  return 1;
	}

      ready_try[0] = 0;

      for (i = 1; i < ready->n_ready; i++)
	{
	  insn = ready_element (ready, i);

	  ready_try [i]
	    = ((!try_data && (TODO_SPEC (insn) & DATA_SPEC))
               || (!try_control && (TODO_SPEC (insn) & CONTROL_SPEC)));
	}

      /* Let the target filter the search space.  */
      for (i = 1; i < ready->n_ready; i++)
	if (!ready_try[i])
	  {
	    insn = ready_element (ready, i);

	    /* If this insn is recognizable we should have already
	       recognized it earlier.
	       ??? Not very clear where this is supposed to be done.
	       See dep_cost_1.  */
	    gcc_checking_assert (INSN_CODE (insn) >= 0
				 || recog_memoized (insn) < 0);

	    ready_try [i]
	      = (/* INSN_CODE check can be omitted here as it is also done later
		    in max_issue ().  */
		 INSN_CODE (insn) < 0
		 || (targetm.sched.first_cycle_multipass_dfa_lookahead_guard
		     && !targetm.sched.first_cycle_multipass_dfa_lookahead_guard
		     (insn)));
	  }

      if (max_issue (ready, 1, curr_state, first_cycle_insn_p, &index) == 0)
	{
	  *insn_ptr = ready_remove_first (ready);
	  if (sched_verbose >= 4)
	    fprintf (sched_dump, ";;\t\tChosen insn (but can't issue) : %s \n",
                     (*current_sched_info->print_insn) (*insn_ptr, 0));
	  return 0;
	}
      else
	{
	  if (sched_verbose >= 4)
	    fprintf (sched_dump, ";;\t\tChosen insn : %s\n",
		     (*current_sched_info->print_insn)
		     (ready_element (ready, index), 0));

	  *insn_ptr = ready_remove (ready, index);
	  return 0;
	}
    }
}

/* This function is called when we have successfully scheduled a
   block.  It uses the schedule stored in the scheduled_insns vector
   to rearrange the RTL.  PREV_HEAD is used as the anchor to which we
   append the scheduled insns; TAIL is the insn after the scheduled
   block.  TARGET_BB is the argument passed to schedule_block.  */

static void
commit_schedule (rtx prev_head, rtx tail, basic_block *target_bb)
{
  unsigned int i;
  rtx insn;

  last_scheduled_insn = prev_head;
  for (i = 0;
       scheduled_insns.iterate (i, &insn);
       i++)
    {
      if (control_flow_insn_p (last_scheduled_insn)
	  || current_sched_info->advance_target_bb (*target_bb, insn))
	{
	  *target_bb = current_sched_info->advance_target_bb (*target_bb, 0);

	  if (sched_verbose)
	    {
	      rtx x;

	      x = next_real_insn (last_scheduled_insn);
	      gcc_assert (x);
	      dump_new_block_header (1, *target_bb, x, tail);
	    }

	  last_scheduled_insn = bb_note (*target_bb);
	}

      if (current_sched_info->begin_move_insn)
	(*current_sched_info->begin_move_insn) (insn, last_scheduled_insn);
      move_insn (insn, last_scheduled_insn,
		 current_sched_info->next_tail);
      if (!DEBUG_INSN_P (insn))
	reemit_notes (insn);
      last_scheduled_insn = insn;
    }

  scheduled_insns.truncate (0);
}

/* Examine all insns on the ready list and queue those which can't be
   issued in this cycle.  TEMP_STATE is temporary scheduler state we
   can use as scratch space.  If FIRST_CYCLE_INSN_P is true, no insns
   have been issued for the current cycle, which means it is valid to
   issue an asm statement.

   If SHADOWS_ONLY_P is true, we eliminate all real insns and only
   leave those for which SHADOW_P is true.  If MODULO_EPILOGUE is true,
   we only leave insns which have an INSN_EXACT_TICK.  */

static void
prune_ready_list (state_t temp_state, bool first_cycle_insn_p,
		  bool shadows_only_p, bool modulo_epilogue_p)
{
  int i, pass;
  bool sched_group_found = false;
  int min_cost_group = 1;

  for (i = 0; i < ready.n_ready; i++)
    {
      rtx insn = ready_element (&ready, i);
      if (SCHED_GROUP_P (insn))
	{
	  sched_group_found = true;
	  break;
	}
    }

  /* Make two passes if there's a SCHED_GROUP_P insn; make sure to handle
     such an insn first and note its cost, then schedule all other insns
     for one cycle later.  */
  for (pass = sched_group_found ? 0 : 1; pass < 2; )
    {
      int n = ready.n_ready;
      for (i = 0; i < n; i++)
	{
	  rtx insn = ready_element (&ready, i);
	  int cost = 0;
	  const char *reason = "resource conflict";

	  if (DEBUG_INSN_P (insn))
	    continue;

	  if (sched_group_found && !SCHED_GROUP_P (insn))
	    {
	      if (pass == 0)
		continue;
	      cost = min_cost_group;
	      reason = "not in sched group";
	    }
	  else if (modulo_epilogue_p
		   && INSN_EXACT_TICK (insn) == INVALID_TICK)
	    {
	      cost = max_insn_queue_index;
	      reason = "not an epilogue insn";
	    }
	  else if (shadows_only_p && !SHADOW_P (insn))
	    {
	      cost = 1;
	      reason = "not a shadow";
	    }
	  else if (recog_memoized (insn) < 0)
	    {
	      if (!first_cycle_insn_p
		  && (GET_CODE (PATTERN (insn)) == ASM_INPUT
		      || asm_noperands (PATTERN (insn)) >= 0))
		cost = 1;
	      reason = "asm";
	    }
	  else if (sched_pressure != SCHED_PRESSURE_NONE)
	    {
	      if (sched_pressure == SCHED_PRESSURE_MODEL
		  && INSN_TICK (insn) <= clock_var)
		{
		  memcpy (temp_state, curr_state, dfa_state_size);
		  if (state_transition (temp_state, insn) >= 0)
		    INSN_TICK (insn) = clock_var + 1;
		}
	      cost = 0;
	    }
	  else
	    {
	      int delay_cost = 0;

	      if (delay_htab.is_created ())
		{
		  struct delay_pair *delay_entry;
		  delay_entry
		    = delay_htab.find_with_hash (insn,
						 htab_hash_pointer (insn));
		  while (delay_entry && delay_cost == 0)
		    {
		      delay_cost = estimate_shadow_tick (delay_entry);
		      if (delay_cost > max_insn_queue_index)
			delay_cost = max_insn_queue_index;
		      delay_entry = delay_entry->next_same_i1;
		    }
		}

	      memcpy (temp_state, curr_state, dfa_state_size);
	      cost = state_transition (temp_state, insn);
	      if (cost < 0)
		cost = 0;
	      else if (cost == 0)
		cost = 1;
	      if (cost < delay_cost)
		{
		  cost = delay_cost;
		  reason = "shadow tick";
		}
	    }
	  if (cost >= 1)
	    {
	      if (SCHED_GROUP_P (insn) && cost > min_cost_group)
		min_cost_group = cost;
	      ready_remove (&ready, i);
	      queue_insn (insn, cost, reason);
	      if (i + 1 < n)
		break;
	    }
	}
      if (i == n)
	pass++;
    }
}

/* Called when we detect that the schedule is impossible.  We examine the
   backtrack queue to find the earliest insn that caused this condition.  */

static struct haifa_saved_data *
verify_shadows (void)
{
  struct haifa_saved_data *save, *earliest_fail = NULL;
  for (save = backtrack_queue; save; save = save->next)
    {
      int t;
      struct delay_pair *pair = save->delay_pair;
      rtx i1 = pair->i1;

      for (; pair; pair = pair->next_same_i1)
	{
	  rtx i2 = pair->i2;

	  if (QUEUE_INDEX (i2) == QUEUE_SCHEDULED)
	    continue;

	  t = INSN_TICK (i1) + pair_delay (pair);
	  if (t < clock_var)
	    {
	      if (sched_verbose >= 2)
		fprintf (sched_dump,
			 ";;\t\tfailed delay requirements for %d/%d (%d->%d)"
			 ", not ready\n",
			 INSN_UID (pair->i1), INSN_UID (pair->i2),
			 INSN_TICK (pair->i1), INSN_EXACT_TICK (pair->i2));
	      earliest_fail = save;
	      break;
	    }
	  if (QUEUE_INDEX (i2) >= 0)
	    {
	      int queued_for = INSN_TICK (i2);

	      if (t < queued_for)
		{
		  if (sched_verbose >= 2)
		    fprintf (sched_dump,
			     ";;\t\tfailed delay requirements for %d/%d"
			     " (%d->%d), queued too late\n",
			     INSN_UID (pair->i1), INSN_UID (pair->i2),
			     INSN_TICK (pair->i1), INSN_EXACT_TICK (pair->i2));
		  earliest_fail = save;
		  break;
		}
	    }
	}
    }

  return earliest_fail;
}

/* Use forward list scheduling to rearrange insns of block pointed to by
   TARGET_BB, possibly bringing insns from subsequent blocks in the same
   region.  */

bool
schedule_block (basic_block *target_bb, state_t init_state)
{
  int i;
  bool success = modulo_ii == 0;
  struct sched_block_state ls;
  state_t temp_state = NULL;  /* It is used for multipass scheduling.  */
  int sort_p, advance, start_clock_var;

  /* Head/tail info for this block.  */
  rtx prev_head = current_sched_info->prev_head;
  rtx next_tail = current_sched_info->next_tail;
  rtx head = NEXT_INSN (prev_head);
  rtx tail = PREV_INSN (next_tail);

  if ((current_sched_info->flags & DONT_BREAK_DEPENDENCIES) == 0
      && sched_pressure != SCHED_PRESSURE_MODEL)
    find_modifiable_mems (head, tail);

  /* We used to have code to avoid getting parameters moved from hard
     argument registers into pseudos.

     However, it was removed when it proved to be of marginal benefit
     and caused problems because schedule_block and compute_forward_dependences
     had different notions of what the "head" insn was.  */

  gcc_assert (head != tail || INSN_P (head));

  haifa_recovery_bb_recently_added_p = false;

  backtrack_queue = NULL;

  /* Debug info.  */
  if (sched_verbose)
    dump_new_block_header (0, *target_bb, head, tail);

  if (init_state == NULL)
    state_reset (curr_state);
  else
    memcpy (curr_state, init_state, dfa_state_size);

  /* Clear the ready list.  */
  ready.first = ready.veclen - 1;
  ready.n_ready = 0;
  ready.n_debug = 0;

  /* It is used for first cycle multipass scheduling.  */
  temp_state = alloca (dfa_state_size);

  if (targetm.sched.init)
    targetm.sched.init (sched_dump, sched_verbose, ready.veclen);

  /* We start inserting insns after PREV_HEAD.  */
  last_scheduled_insn = nonscheduled_insns_begin = prev_head;
  last_nondebug_scheduled_insn = NULL_RTX;

  gcc_assert ((NOTE_P (last_scheduled_insn)
	       || DEBUG_INSN_P (last_scheduled_insn))
	      && BLOCK_FOR_INSN (last_scheduled_insn) == *target_bb);

  /* Initialize INSN_QUEUE.  Q_SIZE is the total number of insns in the
     queue.  */
  q_ptr = 0;
  q_size = 0;

  insn_queue = XALLOCAVEC (rtx, max_insn_queue_index + 1);
  memset (insn_queue, 0, (max_insn_queue_index + 1) * sizeof (rtx));

  /* Start just before the beginning of time.  */
  clock_var = -1;

  /* We need queue and ready lists and clock_var be initialized
     in try_ready () (which is called through init_ready_list ()).  */
  (*current_sched_info->init_ready_list) ();

  if (sched_pressure == SCHED_PRESSURE_MODEL)
    model_start_schedule ();

  /* The algorithm is O(n^2) in the number of ready insns at any given
     time in the worst case.  Before reload we are more likely to have
     big lists so truncate them to a reasonable size.  */
  if (!reload_completed
      && ready.n_ready - ready.n_debug > MAX_SCHED_READY_INSNS)
    {
      ready_sort (&ready);

      /* Find first free-standing insn past MAX_SCHED_READY_INSNS.
         If there are debug insns, we know they're first.  */
      for (i = MAX_SCHED_READY_INSNS + ready.n_debug; i < ready.n_ready; i++)
	if (!SCHED_GROUP_P (ready_element (&ready, i)))
	  break;

      if (sched_verbose >= 2)
	{
	  fprintf (sched_dump,
		   ";;\t\tReady list on entry: %d insns\n", ready.n_ready);
	  fprintf (sched_dump,
		   ";;\t\t before reload => truncated to %d insns\n", i);
	}

      /* Delay all insns past it for 1 cycle.  If debug counter is
	 activated make an exception for the insn right after
	 nonscheduled_insns_begin.  */
      {
	rtx skip_insn;

	if (dbg_cnt (sched_insn) == false)
	  skip_insn = next_nonnote_insn (nonscheduled_insns_begin);
	else
	  skip_insn = NULL_RTX;

	while (i < ready.n_ready)
	  {
	    rtx insn;

	    insn = ready_remove (&ready, i);

	    if (insn != skip_insn)
	      queue_insn (insn, 1, "list truncated");
	  }
	if (skip_insn)
	  ready_add (&ready, skip_insn, true);
      }
    }

  /* Now we can restore basic block notes and maintain precise cfg.  */
  restore_bb_notes (*target_bb);

  last_clock_var = -1;

  advance = 0;

  gcc_assert (scheduled_insns.length () == 0);
  sort_p = TRUE;
  must_backtrack = false;
  modulo_insns_scheduled = 0;

  ls.modulo_epilogue = false;

  /* Loop until all the insns in BB are scheduled.  */
  while ((*current_sched_info->schedule_more_p) ())
    {
      perform_replacements_new_cycle ();
      do
	{
	  start_clock_var = clock_var;

	  clock_var++;

	  advance_one_cycle ();

	  /* Add to the ready list all pending insns that can be issued now.
	     If there are no ready insns, increment clock until one
	     is ready and add all pending insns at that point to the ready
	     list.  */
	  queue_to_ready (&ready);

	  gcc_assert (ready.n_ready);

	  if (sched_verbose >= 2)
	    {
	      fprintf (sched_dump, ";;\t\tReady list after queue_to_ready:  ");
	      debug_ready_list (&ready);
	    }
	  advance -= clock_var - start_clock_var;
	}
      while (advance > 0);

      if (ls.modulo_epilogue)
	{
	  int stage = clock_var / modulo_ii;
	  if (stage > modulo_last_stage * 2 + 2)
	    {
	      if (sched_verbose >= 2)
		fprintf (sched_dump,
			 ";;\t\tmodulo scheduled succeeded at II %d\n",
			 modulo_ii);
	      success = true;
	      goto end_schedule;
	    }
	}
      else if (modulo_ii > 0)
	{
	  int stage = clock_var / modulo_ii;
	  if (stage > modulo_max_stages)
	    {
	      if (sched_verbose >= 2)
		fprintf (sched_dump,
			 ";;\t\tfailing schedule due to excessive stages\n");
	      goto end_schedule;
	    }
	  if (modulo_n_insns == modulo_insns_scheduled
	      && stage > modulo_last_stage)
	    {
	      if (sched_verbose >= 2)
		fprintf (sched_dump,
			 ";;\t\tfound kernel after %d stages, II %d\n",
			 stage, modulo_ii);
	      ls.modulo_epilogue = true;
	    }
	}

      prune_ready_list (temp_state, true, false, ls.modulo_epilogue);
      if (ready.n_ready == 0)
	continue;
      if (must_backtrack)
	goto do_backtrack;

      ls.first_cycle_insn_p = true;
      ls.shadows_only_p = false;
      cycle_issued_insns = 0;
      ls.can_issue_more = issue_rate;
      for (;;)
	{
	  rtx insn;
	  int cost;
	  bool asm_p;

	  if (sort_p && ready.n_ready > 0)
	    {
	      /* Sort the ready list based on priority.  This must be
		 done every iteration through the loop, as schedule_insn
		 may have readied additional insns that will not be
		 sorted correctly.  */
	      ready_sort (&ready);

	      if (sched_verbose >= 2)
		{
		  fprintf (sched_dump, ";;\t\tReady list after ready_sort:  ");
		  debug_ready_list (&ready);
		}
	    }

	  /* We don't want md sched reorder to even see debug isns, so put
	     them out right away.  */
	  if (ready.n_ready && DEBUG_INSN_P (ready_element (&ready, 0))
	      && (*current_sched_info->schedule_more_p) ())
	    {
	      while (ready.n_ready && DEBUG_INSN_P (ready_element (&ready, 0)))
		{
		  rtx insn = ready_remove_first (&ready);
		  gcc_assert (DEBUG_INSN_P (insn));
		  (*current_sched_info->begin_schedule_ready) (insn);
		  scheduled_insns.safe_push (insn);
		  last_scheduled_insn = insn;
		  advance = schedule_insn (insn);
		  gcc_assert (advance == 0);
		  if (ready.n_ready > 0)
		    ready_sort (&ready);
		}
	    }

	  if (ls.first_cycle_insn_p && !ready.n_ready)
	    break;

	resume_after_backtrack:
	  /* Allow the target to reorder the list, typically for
	     better instruction bundling.  */
	  if (sort_p
	      && (ready.n_ready == 0
		  || !SCHED_GROUP_P (ready_element (&ready, 0))))
	    {
	      if (ls.first_cycle_insn_p && targetm.sched.reorder)
		ls.can_issue_more
		  = targetm.sched.reorder (sched_dump, sched_verbose,
					   ready_lastpos (&ready),
					   &ready.n_ready, clock_var);
	      else if (!ls.first_cycle_insn_p && targetm.sched.reorder2)
		ls.can_issue_more
		  = targetm.sched.reorder2 (sched_dump, sched_verbose,
					    ready.n_ready
					    ? ready_lastpos (&ready) : NULL,
					    &ready.n_ready, clock_var);
	    }

	restart_choose_ready:
	  if (sched_verbose >= 2)
	    {
	      fprintf (sched_dump, ";;\tReady list (t = %3d):  ",
		       clock_var);
	      debug_ready_list (&ready);
	      if (sched_pressure == SCHED_PRESSURE_WEIGHTED)
		print_curr_reg_pressure ();
	    }

	  if (ready.n_ready == 0
	      && ls.can_issue_more
	      && reload_completed)
	    {
	      /* Allow scheduling insns directly from the queue in case
		 there's nothing better to do (ready list is empty) but
		 there are still vacant dispatch slots in the current cycle.  */
	      if (sched_verbose >= 6)
		fprintf (sched_dump,";;\t\tSecond chance\n");
	      memcpy (temp_state, curr_state, dfa_state_size);
	      if (early_queue_to_ready (temp_state, &ready))
		ready_sort (&ready);
	    }

	  if (ready.n_ready == 0
	      || !ls.can_issue_more
	      || state_dead_lock_p (curr_state)
	      || !(*current_sched_info->schedule_more_p) ())
	    break;

	  /* Select and remove the insn from the ready list.  */
	  if (sort_p)
	    {
	      int res;

	      insn = NULL_RTX;
	      res = choose_ready (&ready, ls.first_cycle_insn_p, &insn);

	      if (res < 0)
		/* Finish cycle.  */
		break;
	      if (res > 0)
		goto restart_choose_ready;

	      gcc_assert (insn != NULL_RTX);
	    }
	  else
	    insn = ready_remove_first (&ready);

	  if (sched_pressure != SCHED_PRESSURE_NONE
	      && INSN_TICK (insn) > clock_var)
	    {
	      ready_add (&ready, insn, true);
	      advance = 1;
	      break;
	    }

	  if (targetm.sched.dfa_new_cycle
	      && targetm.sched.dfa_new_cycle (sched_dump, sched_verbose,
					      insn, last_clock_var,
					      clock_var, &sort_p))
	    /* SORT_P is used by the target to override sorting
	       of the ready list.  This is needed when the target
	       has modified its internal structures expecting that
	       the insn will be issued next.  As we need the insn
	       to have the highest priority (so it will be returned by
	       the ready_remove_first call above), we invoke
	       ready_add (&ready, insn, true).
	       But, still, there is one issue: INSN can be later
	       discarded by scheduler's front end through
	       current_sched_info->can_schedule_ready_p, hence, won't
	       be issued next.  */
	    {
	      ready_add (&ready, insn, true);
              break;
	    }

	  sort_p = TRUE;

	  if (current_sched_info->can_schedule_ready_p
	      && ! (*current_sched_info->can_schedule_ready_p) (insn))
	    /* We normally get here only if we don't want to move
	       insn from the split block.  */
	    {
	      TODO_SPEC (insn) = DEP_POSTPONED;
	      goto restart_choose_ready;
	    }

	  if (delay_htab.is_created ())
	    {
	      /* If this insn is the first part of a delay-slot pair, record a
		 backtrack point.  */
	      struct delay_pair *delay_entry;
	      delay_entry
		= delay_htab.find_with_hash (insn, htab_hash_pointer (insn));
	      if (delay_entry)
		{
		  save_backtrack_point (delay_entry, ls);
		  if (sched_verbose >= 2)
		    fprintf (sched_dump, ";;\t\tsaving backtrack point\n");
		}
	    }

	  /* DECISION is made.  */

	  if (modulo_ii > 0 && INSN_UID (insn) < modulo_iter0_max_uid)
	    {
	      modulo_insns_scheduled++;
	      modulo_last_stage = clock_var / modulo_ii;
	    }
          if (TODO_SPEC (insn) & SPECULATIVE)
            generate_recovery_code (insn);

	  if (targetm.sched.dispatch (NULL_RTX, IS_DISPATCH_ON))
	    targetm.sched.dispatch_do (insn, ADD_TO_DISPATCH_WINDOW);

	  /* Update counters, etc in the scheduler's front end.  */
	  (*current_sched_info->begin_schedule_ready) (insn);
	  scheduled_insns.safe_push (insn);
	  gcc_assert (NONDEBUG_INSN_P (insn));
	  last_nondebug_scheduled_insn = last_scheduled_insn = insn;

	  if (recog_memoized (insn) >= 0)
	    {
	      memcpy (temp_state, curr_state, dfa_state_size);
	      cost = state_transition (curr_state, insn);
	      if (sched_pressure != SCHED_PRESSURE_WEIGHTED)
		gcc_assert (cost < 0);
	      if (memcmp (temp_state, curr_state, dfa_state_size) != 0)
		cycle_issued_insns++;
	      asm_p = false;
	    }
	  else
	    asm_p = (GET_CODE (PATTERN (insn)) == ASM_INPUT
		     || asm_noperands (PATTERN (insn)) >= 0);

	  if (targetm.sched.variable_issue)
	    ls.can_issue_more =
	      targetm.sched.variable_issue (sched_dump, sched_verbose,
					    insn, ls.can_issue_more);
	  /* A naked CLOBBER or USE generates no instruction, so do
	     not count them against the issue rate.  */
	  else if (GET_CODE (PATTERN (insn)) != USE
		   && GET_CODE (PATTERN (insn)) != CLOBBER)
	    ls.can_issue_more--;
	  advance = schedule_insn (insn);

	  if (SHADOW_P (insn))
	    ls.shadows_only_p = true;

	  /* After issuing an asm insn we should start a new cycle.  */
	  if (advance == 0 && asm_p)
	    advance = 1;

	  if (must_backtrack)
	    break;

	  if (advance != 0)
	    break;

	  ls.first_cycle_insn_p = false;
	  if (ready.n_ready > 0)
	    prune_ready_list (temp_state, false, ls.shadows_only_p,
			      ls.modulo_epilogue);
	}

    do_backtrack:
      if (!must_backtrack)
	for (i = 0; i < ready.n_ready; i++)
	  {
	    rtx insn = ready_element (&ready, i);
	    if (INSN_EXACT_TICK (insn) == clock_var)
	      {
		must_backtrack = true;
		clock_var++;
		break;
	      }
	  }
      if (must_backtrack && modulo_ii > 0)
	{
	  if (modulo_backtracks_left == 0)
	    goto end_schedule;
	  modulo_backtracks_left--;
	}
      while (must_backtrack)
	{
	  struct haifa_saved_data *failed;
	  rtx failed_insn;

	  must_backtrack = false;
	  failed = verify_shadows ();
	  gcc_assert (failed);

	  failed_insn = failed->delay_pair->i1;
	  /* Clear these queues.  */
	  perform_replacements_new_cycle ();
	  toggle_cancelled_flags (false);
	  unschedule_insns_until (failed_insn);
	  while (failed != backtrack_queue)
	    free_topmost_backtrack_point (true);
	  restore_last_backtrack_point (&ls);
	  if (sched_verbose >= 2)
	    fprintf (sched_dump, ";;\t\trewind to cycle %d\n", clock_var);
	  /* Delay by at least a cycle.  This could cause additional
	     backtracking.  */
	  queue_insn (failed_insn, 1, "backtracked");
	  advance = 0;
	  if (must_backtrack)
	    continue;
	  if (ready.n_ready > 0)
	    goto resume_after_backtrack;
	  else
	    {
	      if (clock_var == 0 && ls.first_cycle_insn_p)
		goto end_schedule;
	      advance = 1;
	      break;
	    }
	}
    }
  if (ls.modulo_epilogue)
    success = true;
 end_schedule:
  advance_one_cycle ();
  perform_replacements_new_cycle ();
  if (modulo_ii > 0)
    {
      /* Once again, debug insn suckiness: they can be on the ready list
	 even if they have unresolved dependencies.  To make our view
	 of the world consistent, remove such "ready" insns.  */
    restart_debug_insn_loop:
      for (i = ready.n_ready - 1; i >= 0; i--)
	{
	  rtx x;

	  x = ready_element (&ready, i);
	  if (DEPS_LIST_FIRST (INSN_HARD_BACK_DEPS (x)) != NULL
	      || DEPS_LIST_FIRST (INSN_SPEC_BACK_DEPS (x)) != NULL)
	    {
	      ready_remove (&ready, i);
	      goto restart_debug_insn_loop;
	    }
	}
      for (i = ready.n_ready - 1; i >= 0; i--)
	{
	  rtx x;

	  x = ready_element (&ready, i);
	  resolve_dependencies (x);
	}
      for (i = 0; i <= max_insn_queue_index; i++)
	{
	  rtx link;
	  while ((link = insn_queue[i]) != NULL)
	    {
	      rtx x = XEXP (link, 0);
	      insn_queue[i] = XEXP (link, 1);
	      QUEUE_INDEX (x) = QUEUE_NOWHERE;
	      free_INSN_LIST_node (link);
	      resolve_dependencies (x);
	    }
	}
    }

  if (!success)
    undo_all_replacements ();

  /* Debug info.  */
  if (sched_verbose)
    {
      fprintf (sched_dump, ";;\tReady list (final):  ");
      debug_ready_list (&ready);
    }

  if (modulo_ii == 0 && current_sched_info->queue_must_finish_empty)
    /* Sanity check -- queue must be empty now.  Meaningless if region has
       multiple bbs.  */
    gcc_assert (!q_size && !ready.n_ready && !ready.n_debug);
  else if (modulo_ii == 0)
    {
      /* We must maintain QUEUE_INDEX between blocks in region.  */
      for (i = ready.n_ready - 1; i >= 0; i--)
	{
	  rtx x;

	  x = ready_element (&ready, i);
	  QUEUE_INDEX (x) = QUEUE_NOWHERE;
	  TODO_SPEC (x) = HARD_DEP;
	}

      if (q_size)
	for (i = 0; i <= max_insn_queue_index; i++)
	  {
	    rtx link;
	    for (link = insn_queue[i]; link; link = XEXP (link, 1))
	      {
		rtx x;

		x = XEXP (link, 0);
		QUEUE_INDEX (x) = QUEUE_NOWHERE;
		TODO_SPEC (x) = HARD_DEP;
	      }
	    free_INSN_LIST_list (&insn_queue[i]);
	  }
    }

  if (sched_pressure == SCHED_PRESSURE_MODEL)
    model_end_schedule ();

  if (success)
    {
      commit_schedule (prev_head, tail, target_bb);
      if (sched_verbose)
	fprintf (sched_dump, ";;   total time = %d\n", clock_var);
    }
  else
    last_scheduled_insn = tail;

  scheduled_insns.truncate (0);

  if (!current_sched_info->queue_must_finish_empty
      || haifa_recovery_bb_recently_added_p)
    {
      /* INSN_TICK (minimum clock tick at which the insn becomes
         ready) may be not correct for the insn in the subsequent
         blocks of the region.  We should use a correct value of
         `clock_var' or modify INSN_TICK.  It is better to keep
         clock_var value equal to 0 at the start of a basic block.
         Therefore we modify INSN_TICK here.  */
      fix_inter_tick (NEXT_INSN (prev_head), last_scheduled_insn);
    }

  if (targetm.sched.finish)
    {
      targetm.sched.finish (sched_dump, sched_verbose);
      /* Target might have added some instructions to the scheduled block
	 in its md_finish () hook.  These new insns don't have any data
	 initialized and to identify them we extend h_i_d so that they'll
	 get zero luids.  */
      sched_extend_luids ();
    }

  if (sched_verbose)
    fprintf (sched_dump, ";;   new head = %d\n;;   new tail = %d\n\n",
	     INSN_UID (head), INSN_UID (tail));

  /* Update head/tail boundaries.  */
  head = NEXT_INSN (prev_head);
  tail = last_scheduled_insn;

  head = restore_other_notes (head, NULL);

  current_sched_info->head = head;
  current_sched_info->tail = tail;

  free_backtrack_queue ();

  return success;
}

/* Set_priorities: compute priority of each insn in the block.  */

int
set_priorities (rtx head, rtx tail)
{
  rtx insn;
  int n_insn;
  int sched_max_insns_priority =
	current_sched_info->sched_max_insns_priority;
  rtx prev_head;

  if (head == tail && ! INSN_P (head))
    gcc_unreachable ();

  n_insn = 0;

  prev_head = PREV_INSN (head);
  for (insn = tail; insn != prev_head; insn = PREV_INSN (insn))
    {
      if (!INSN_P (insn))
	continue;

      n_insn++;
      (void) priority (insn);

      gcc_assert (INSN_PRIORITY_KNOWN (insn));

      sched_max_insns_priority = MAX (sched_max_insns_priority,
				      INSN_PRIORITY (insn));
    }

  current_sched_info->sched_max_insns_priority = sched_max_insns_priority;

  return n_insn;
}

/* Set dump and sched_verbose for the desired debugging output.  If no
   dump-file was specified, but -fsched-verbose=N (any N), print to stderr.
   For -fsched-verbose=N, N>=10, print everything to stderr.  */
void
setup_sched_dump (void)
{
  sched_verbose = sched_verbose_param;
  if (sched_verbose_param == 0 && dump_file)
    sched_verbose = 1;
  sched_dump = ((sched_verbose_param >= 10 || !dump_file)
		? stderr : dump_file);
}

/* Try to group comparison and the following conditional jump INSN if
   they're already adjacent. This is to prevent scheduler from scheduling
   them apart.  */

static void
try_group_insn (rtx insn)
{
  unsigned int condreg1, condreg2;
  rtx cc_reg_1;
  rtx prev;

  if (!any_condjump_p (insn))
    return;

  targetm.fixed_condition_code_regs (&condreg1, &condreg2);
  cc_reg_1 = gen_rtx_REG (CCmode, condreg1);
  prev = prev_nonnote_nondebug_insn (insn);
  if (!reg_referenced_p (cc_reg_1, PATTERN (insn))
      || !prev
      || !modified_in_p (cc_reg_1, prev))
    return;

  /* Different microarchitectures support macro fusions for different
     combinations of insn pairs.  */
  if (!targetm.sched.macro_fusion_pair_p
      || !targetm.sched.macro_fusion_pair_p (prev, insn))
    return;

  SCHED_GROUP_P (insn) = 1;
}

/* If the last cond jump and the cond register defining insn are consecutive
   before scheduling, we want them to be in a schedule group. This is good
   for performance on microarchitectures supporting macro-fusion.  */

static void
group_insns_for_macro_fusion ()
{
  basic_block bb;

  FOR_EACH_BB (bb)
    try_group_insn (BB_END (bb));
}

/* Initialize some global state for the scheduler.  This function works
   with the common data shared between all the schedulers.  It is called
   from the scheduler specific initialization routine.  */

void
sched_init (void)
{
  /* Disable speculative loads in their presence if cc0 defined.  */
#ifdef HAVE_cc0
  flag_schedule_speculative_load = 0;
#endif

  if (targetm.sched.dispatch (NULL_RTX, IS_DISPATCH_ON))
    targetm.sched.dispatch_do (NULL_RTX, DISPATCH_INIT);

  if (live_range_shrinkage_p)
    sched_pressure = SCHED_PRESSURE_WEIGHTED;
  else if (flag_sched_pressure
	   && !reload_completed
	   && common_sched_info->sched_pass_id == SCHED_RGN_PASS)
    sched_pressure = ((enum sched_pressure_algorithm)
		      PARAM_VALUE (PARAM_SCHED_PRESSURE_ALGORITHM));
  else
    sched_pressure = SCHED_PRESSURE_NONE;

  if (sched_pressure != SCHED_PRESSURE_NONE)
    ira_setup_eliminable_regset (false);

  /* Initialize SPEC_INFO.  */
  if (targetm.sched.set_sched_flags)
    {
      spec_info = &spec_info_var;
      targetm.sched.set_sched_flags (spec_info);

      if (spec_info->mask != 0)
        {
          spec_info->data_weakness_cutoff =
            (PARAM_VALUE (PARAM_SCHED_SPEC_PROB_CUTOFF) * MAX_DEP_WEAK) / 100;
          spec_info->control_weakness_cutoff =
            (PARAM_VALUE (PARAM_SCHED_SPEC_PROB_CUTOFF)
             * REG_BR_PROB_BASE) / 100;
        }
      else
	/* So we won't read anything accidentally.  */
	spec_info = NULL;

    }
  else
    /* So we won't read anything accidentally.  */
    spec_info = 0;

  /* Initialize issue_rate.  */
  if (targetm.sched.issue_rate)
    issue_rate = targetm.sched.issue_rate ();
  else
    issue_rate = 1;

  if (cached_issue_rate != issue_rate)
    {
      cached_issue_rate = issue_rate;
      /* To invalidate max_lookahead_tries:  */
      cached_first_cycle_multipass_dfa_lookahead = 0;
    }

  if (targetm.sched.first_cycle_multipass_dfa_lookahead)
    dfa_lookahead = targetm.sched.first_cycle_multipass_dfa_lookahead ();
  else
    dfa_lookahead = 0;

  if (targetm.sched.init_dfa_pre_cycle_insn)
    targetm.sched.init_dfa_pre_cycle_insn ();

  if (targetm.sched.init_dfa_post_cycle_insn)
    targetm.sched.init_dfa_post_cycle_insn ();

  dfa_start ();
  dfa_state_size = state_size ();

  init_alias_analysis ();

  if (!sched_no_dce)
    df_set_flags (DF_LR_RUN_DCE);
  df_note_add_problem ();

  /* More problems needed for interloop dep calculation in SMS.  */
  if (common_sched_info->sched_pass_id == SCHED_SMS_PASS)
    {
      df_rd_add_problem ();
      df_chain_add_problem (DF_DU_CHAIN + DF_UD_CHAIN);
    }

  df_analyze ();

  /* Do not run DCE after reload, as this can kill nops inserted
     by bundling.  */
  if (reload_completed)
    df_clear_flags (DF_LR_RUN_DCE);

  regstat_compute_calls_crossed ();

  if (targetm.sched.init_global)
    targetm.sched.init_global (sched_dump, sched_verbose, get_max_uid () + 1);

  if (sched_pressure != SCHED_PRESSURE_NONE)
    {
      int i, max_regno = max_reg_num ();

      if (sched_dump != NULL)
	/* We need info about pseudos for rtl dumps about pseudo
	   classes and costs.  */
	regstat_init_n_sets_and_refs ();
      ira_set_pseudo_classes (true, sched_verbose ? sched_dump : NULL);
      sched_regno_pressure_class
	= (enum reg_class *) xmalloc (max_regno * sizeof (enum reg_class));
      for (i = 0; i < max_regno; i++)
	sched_regno_pressure_class[i]
	  = (i < FIRST_PSEUDO_REGISTER
	     ? ira_pressure_class_translate[REGNO_REG_CLASS (i)]
	     : ira_pressure_class_translate[reg_allocno_class (i)]);
      curr_reg_live = BITMAP_ALLOC (NULL);
      if (sched_pressure == SCHED_PRESSURE_WEIGHTED)
	{
	  saved_reg_live = BITMAP_ALLOC (NULL);
	  region_ref_regs = BITMAP_ALLOC (NULL);
	}
    }

  curr_state = xmalloc (dfa_state_size);

  /* Group compare and branch insns for macro-fusion.  */
  if (targetm.sched.macro_fusion_p
      && targetm.sched.macro_fusion_p ())
    group_insns_for_macro_fusion ();
}

static void haifa_init_only_bb (basic_block, basic_block);

/* Initialize data structures specific to the Haifa scheduler.  */
void
haifa_sched_init (void)
{
  setup_sched_dump ();
  sched_init ();

  scheduled_insns.create (0);

  if (spec_info != NULL)
    {
      sched_deps_info->use_deps_list = 1;
      sched_deps_info->generate_spec_deps = 1;
    }

  /* Initialize luids, dependency caches, target and h_i_d for the
     whole function.  */
  {
    bb_vec_t bbs;
    bbs.create (n_basic_blocks);
    basic_block bb;

    sched_init_bbs ();

    FOR_EACH_BB (bb)
      bbs.quick_push (bb);
    sched_init_luids (bbs);
    sched_deps_init (true);
    sched_extend_target ();
    haifa_init_h_i_d (bbs);

    bbs.release ();
  }

  sched_init_only_bb = haifa_init_only_bb;
  sched_split_block = sched_split_block_1;
  sched_create_empty_bb = sched_create_empty_bb_1;
  haifa_recovery_bb_ever_added_p = false;

  nr_begin_data = nr_begin_control = nr_be_in_data = nr_be_in_control = 0;
  before_recovery = 0;
  after_recovery = 0;

  modulo_ii = 0;
}

/* Finish work with the data specific to the Haifa scheduler.  */
void
haifa_sched_finish (void)
{
  sched_create_empty_bb = NULL;
  sched_split_block = NULL;
  sched_init_only_bb = NULL;

  if (spec_info && spec_info->dump)
    {
      char c = reload_completed ? 'a' : 'b';

      fprintf (spec_info->dump,
	       ";; %s:\n", current_function_name ());

      fprintf (spec_info->dump,
               ";; Procedure %cr-begin-data-spec motions == %d\n",
               c, nr_begin_data);
      fprintf (spec_info->dump,
               ";; Procedure %cr-be-in-data-spec motions == %d\n",
               c, nr_be_in_data);
      fprintf (spec_info->dump,
               ";; Procedure %cr-begin-control-spec motions == %d\n",
               c, nr_begin_control);
      fprintf (spec_info->dump,
               ";; Procedure %cr-be-in-control-spec motions == %d\n",
               c, nr_be_in_control);
    }

  scheduled_insns.release ();

  /* Finalize h_i_d, dependency caches, and luids for the whole
     function.  Target will be finalized in md_global_finish ().  */
  sched_deps_finish ();
  sched_finish_luids ();
  current_sched_info = NULL;
  sched_finish ();
}

/* Free global data used during insn scheduling.  This function works with
   the common data shared between the schedulers.  */

void
sched_finish (void)
{
  haifa_finish_h_i_d ();
  if (sched_pressure != SCHED_PRESSURE_NONE)
    {
      if (regstat_n_sets_and_refs != NULL)
	regstat_free_n_sets_and_refs ();
      if (sched_pressure == SCHED_PRESSURE_WEIGHTED)
	{
	  BITMAP_FREE (region_ref_regs);
	  BITMAP_FREE (saved_reg_live);
	}
      BITMAP_FREE (curr_reg_live);
      free (sched_regno_pressure_class);
    }
  free (curr_state);

  if (targetm.sched.finish_global)
    targetm.sched.finish_global (sched_dump, sched_verbose);

  end_alias_analysis ();

  regstat_free_calls_crossed ();

  dfa_finish ();
}

/* Free all delay_pair structures that were recorded.  */
void
free_delay_pairs (void)
{
  if (delay_htab.is_created ())
    {
      delay_htab.empty ();
      delay_htab_i2.empty ();
    }
}

/* Fix INSN_TICKs of the instructions in the current block as well as
   INSN_TICKs of their dependents.
   HEAD and TAIL are the begin and the end of the current scheduled block.  */
static void
fix_inter_tick (rtx head, rtx tail)
{
  /* Set of instructions with corrected INSN_TICK.  */
  bitmap_head processed;
  /* ??? It is doubtful if we should assume that cycle advance happens on
     basic block boundaries.  Basically insns that are unconditionally ready
     on the start of the block are more preferable then those which have
     a one cycle dependency over insn from the previous block.  */
  int next_clock = clock_var + 1;

  bitmap_initialize (&processed, 0);

  /* Iterates over scheduled instructions and fix their INSN_TICKs and
     INSN_TICKs of dependent instructions, so that INSN_TICKs are consistent
     across different blocks.  */
  for (tail = NEXT_INSN (tail); head != tail; head = NEXT_INSN (head))
    {
      if (INSN_P (head))
	{
	  int tick;
	  sd_iterator_def sd_it;
	  dep_t dep;

	  tick = INSN_TICK (head);
	  gcc_assert (tick >= MIN_TICK);

	  /* Fix INSN_TICK of instruction from just scheduled block.  */
	  if (bitmap_set_bit (&processed, INSN_LUID (head)))
	    {
	      tick -= next_clock;

	      if (tick < MIN_TICK)
		tick = MIN_TICK;

	      INSN_TICK (head) = tick;
	    }

	  if (DEBUG_INSN_P (head))
	    continue;

	  FOR_EACH_DEP (head, SD_LIST_RES_FORW, sd_it, dep)
	    {
	      rtx next;

	      next = DEP_CON (dep);
	      tick = INSN_TICK (next);

	      if (tick != INVALID_TICK
		  /* If NEXT has its INSN_TICK calculated, fix it.
		     If not - it will be properly calculated from
		     scratch later in fix_tick_ready.  */
		  && bitmap_set_bit (&processed, INSN_LUID (next)))
		{
		  tick -= next_clock;

		  if (tick < MIN_TICK)
		    tick = MIN_TICK;

		  if (tick > INTER_TICK (next))
		    INTER_TICK (next) = tick;
		  else
		    tick = INTER_TICK (next);

		  INSN_TICK (next) = tick;
		}
	    }
	}
    }
  bitmap_clear (&processed);
}

/* Check if NEXT is ready to be added to the ready or queue list.
   If "yes", add it to the proper list.
   Returns:
      -1 - is not ready yet,
       0 - added to the ready list,
   0 < N - queued for N cycles.  */
int
try_ready (rtx next)
{
  ds_t old_ts, new_ts;

  old_ts = TODO_SPEC (next);

  gcc_assert (!(old_ts & ~(SPECULATIVE | HARD_DEP | DEP_CONTROL | DEP_POSTPONED))
	      && (old_ts == HARD_DEP
		  || old_ts == DEP_POSTPONED
		  || (old_ts & SPECULATIVE)
		  || old_ts == DEP_CONTROL));

  new_ts = recompute_todo_spec (next, false);

  if (new_ts & (HARD_DEP | DEP_POSTPONED))
    gcc_assert (new_ts == old_ts
		&& QUEUE_INDEX (next) == QUEUE_NOWHERE);
  else if (current_sched_info->new_ready)
    new_ts = current_sched_info->new_ready (next, new_ts);

  /* * if !(old_ts & SPECULATIVE) (e.g. HARD_DEP or 0), then insn might
     have its original pattern or changed (speculative) one.  This is due
     to changing ebb in region scheduling.
     * But if (old_ts & SPECULATIVE), then we are pretty sure that insn
     has speculative pattern.

     We can't assert (!(new_ts & HARD_DEP) || new_ts == old_ts) here because
     control-speculative NEXT could have been discarded by sched-rgn.c
     (the same case as when discarded by can_schedule_ready_p ()).  */

  if ((new_ts & SPECULATIVE)
      /* If (old_ts == new_ts), then (old_ts & SPECULATIVE) and we don't
	 need to change anything.  */
      && new_ts != old_ts)
    {
      int res;
      rtx new_pat;

      gcc_assert ((new_ts & SPECULATIVE) && !(new_ts & ~SPECULATIVE));

      res = haifa_speculate_insn (next, new_ts, &new_pat);

      switch (res)
	{
	case -1:
	  /* It would be nice to change DEP_STATUS of all dependences,
	     which have ((DEP_STATUS & SPECULATIVE) == new_ts) to HARD_DEP,
	     so we won't reanalyze anything.  */
	  new_ts = HARD_DEP;
	  break;

	case 0:
	  /* We follow the rule, that every speculative insn
	     has non-null ORIG_PAT.  */
	  if (!ORIG_PAT (next))
	    ORIG_PAT (next) = PATTERN (next);
	  break;

	case 1:
	  if (!ORIG_PAT (next))
	    /* If we gonna to overwrite the original pattern of insn,
	       save it.  */
	    ORIG_PAT (next) = PATTERN (next);

	  res = haifa_change_pattern (next, new_pat);
	  gcc_assert (res);
	  break;

	default:
	  gcc_unreachable ();
	}
    }

  /* We need to restore pattern only if (new_ts == 0), because otherwise it is
     either correct (new_ts & SPECULATIVE),
     or we simply don't care (new_ts & HARD_DEP).  */

  gcc_assert (!ORIG_PAT (next)
	      || !IS_SPECULATION_BRANCHY_CHECK_P (next));

  TODO_SPEC (next) = new_ts;

  if (new_ts & (HARD_DEP | DEP_POSTPONED))
    {
      /* We can't assert (QUEUE_INDEX (next) == QUEUE_NOWHERE) here because
	 control-speculative NEXT could have been discarded by sched-rgn.c
	 (the same case as when discarded by can_schedule_ready_p ()).  */
      /*gcc_assert (QUEUE_INDEX (next) == QUEUE_NOWHERE);*/

      change_queue_index (next, QUEUE_NOWHERE);

      return -1;
    }
  else if (!(new_ts & BEGIN_SPEC)
	   && ORIG_PAT (next) && PREDICATED_PAT (next) == NULL_RTX
	   && !IS_SPECULATION_CHECK_P (next))
    /* We should change pattern of every previously speculative
       instruction - and we determine if NEXT was speculative by using
       ORIG_PAT field.  Except one case - speculation checks have ORIG_PAT
       pat too, so skip them.  */
    {
      bool success = haifa_change_pattern (next, ORIG_PAT (next));
      gcc_assert (success);
      ORIG_PAT (next) = 0;
    }

  if (sched_verbose >= 2)
    {
      fprintf (sched_dump, ";;\t\tdependencies resolved: insn %s",
               (*current_sched_info->print_insn) (next, 0));

      if (spec_info && spec_info->dump)
        {
          if (new_ts & BEGIN_DATA)
            fprintf (spec_info->dump, "; data-spec;");
          if (new_ts & BEGIN_CONTROL)
            fprintf (spec_info->dump, "; control-spec;");
          if (new_ts & BE_IN_CONTROL)
            fprintf (spec_info->dump, "; in-control-spec;");
        }
      if (TODO_SPEC (next) & DEP_CONTROL)
	fprintf (sched_dump, " predicated");
      fprintf (sched_dump, "\n");
    }

  adjust_priority (next);

  return fix_tick_ready (next);
}

/* Calculate INSN_TICK of NEXT and add it to either ready or queue list.  */
static int
fix_tick_ready (rtx next)
{
  int tick, delay;

  if (!DEBUG_INSN_P (next) && !sd_lists_empty_p (next, SD_LIST_RES_BACK))
    {
      int full_p;
      sd_iterator_def sd_it;
      dep_t dep;

      tick = INSN_TICK (next);
      /* if tick is not equal to INVALID_TICK, then update
	 INSN_TICK of NEXT with the most recent resolved dependence
	 cost.  Otherwise, recalculate from scratch.  */
      full_p = (tick == INVALID_TICK);

      FOR_EACH_DEP (next, SD_LIST_RES_BACK, sd_it, dep)
        {
          rtx pro = DEP_PRO (dep);
          int tick1;

	  gcc_assert (INSN_TICK (pro) >= MIN_TICK);

          tick1 = INSN_TICK (pro) + dep_cost (dep);
          if (tick1 > tick)
            tick = tick1;

	  if (!full_p)
	    break;
        }
    }
  else
    tick = -1;

  INSN_TICK (next) = tick;

  delay = tick - clock_var;
  if (delay <= 0 || sched_pressure != SCHED_PRESSURE_NONE)
    delay = QUEUE_READY;

  change_queue_index (next, delay);

  return delay;
}

/* Move NEXT to the proper queue list with (DELAY >= 1),
   or add it to the ready list (DELAY == QUEUE_READY),
   or remove it from ready and queue lists at all (DELAY == QUEUE_NOWHERE).  */
static void
change_queue_index (rtx next, int delay)
{
  int i = QUEUE_INDEX (next);

  gcc_assert (QUEUE_NOWHERE <= delay && delay <= max_insn_queue_index
	      && delay != 0);
  gcc_assert (i != QUEUE_SCHEDULED);

  if ((delay > 0 && NEXT_Q_AFTER (q_ptr, delay) == i)
      || (delay < 0 && delay == i))
    /* We have nothing to do.  */
    return;

  /* Remove NEXT from wherever it is now.  */
  if (i == QUEUE_READY)
    ready_remove_insn (next);
  else if (i >= 0)
    queue_remove (next);

  /* Add it to the proper place.  */
  if (delay == QUEUE_READY)
    ready_add (readyp, next, false);
  else if (delay >= 1)
    queue_insn (next, delay, "change queue index");

  if (sched_verbose >= 2)
    {
      fprintf (sched_dump, ";;\t\ttick updated: insn %s",
	       (*current_sched_info->print_insn) (next, 0));

      if (delay == QUEUE_READY)
	fprintf (sched_dump, " into ready\n");
      else if (delay >= 1)
	fprintf (sched_dump, " into queue with cost=%d\n", delay);
      else
	fprintf (sched_dump, " removed from ready or queue lists\n");
    }
}

static int sched_ready_n_insns = -1;

/* Initialize per region data structures.  */
void
sched_extend_ready_list (int new_sched_ready_n_insns)
{
  int i;

  if (sched_ready_n_insns == -1)
    /* At the first call we need to initialize one more choice_stack
       entry.  */
    {
      i = 0;
      sched_ready_n_insns = 0;
      scheduled_insns.reserve (new_sched_ready_n_insns);
    }
  else
    i = sched_ready_n_insns + 1;

  ready.veclen = new_sched_ready_n_insns + issue_rate;
  ready.vec = XRESIZEVEC (rtx, ready.vec, ready.veclen);

  gcc_assert (new_sched_ready_n_insns >= sched_ready_n_insns);

  ready_try = (char *) xrecalloc (ready_try, new_sched_ready_n_insns,
                                  sched_ready_n_insns, sizeof (*ready_try));

  /* We allocate +1 element to save initial state in the choice_stack[0]
     entry.  */
  choice_stack = XRESIZEVEC (struct choice_entry, choice_stack,
			     new_sched_ready_n_insns + 1);

  for (; i <= new_sched_ready_n_insns; i++)
    {
      choice_stack[i].state = xmalloc (dfa_state_size);

      if (targetm.sched.first_cycle_multipass_init)
	targetm.sched.first_cycle_multipass_init (&(choice_stack[i]
						    .target_data));
    }

  sched_ready_n_insns = new_sched_ready_n_insns;
}

/* Free per region data structures.  */
void
sched_finish_ready_list (void)
{
  int i;

  free (ready.vec);
  ready.vec = NULL;
  ready.veclen = 0;

  free (ready_try);
  ready_try = NULL;

  for (i = 0; i <= sched_ready_n_insns; i++)
    {
      if (targetm.sched.first_cycle_multipass_fini)
	targetm.sched.first_cycle_multipass_fini (&(choice_stack[i]
						    .target_data));

      free (choice_stack [i].state);
    }
  free (choice_stack);
  choice_stack = NULL;

  sched_ready_n_insns = -1;
}

static int
haifa_luid_for_non_insn (rtx x)
{
  gcc_assert (NOTE_P (x) || LABEL_P (x));

  return 0;
}

/* Generates recovery code for INSN.  */
static void
generate_recovery_code (rtx insn)
{
  if (TODO_SPEC (insn) & BEGIN_SPEC)
    begin_speculative_block (insn);

  /* Here we have insn with no dependencies to
     instructions other then CHECK_SPEC ones.  */

  if (TODO_SPEC (insn) & BE_IN_SPEC)
    add_to_speculative_block (insn);
}

/* Helper function.
   Tries to add speculative dependencies of type FS between instructions
   in deps_list L and TWIN.  */
static void
process_insn_forw_deps_be_in_spec (rtx insn, rtx twin, ds_t fs)
{
  sd_iterator_def sd_it;
  dep_t dep;

  FOR_EACH_DEP (insn, SD_LIST_FORW, sd_it, dep)
    {
      ds_t ds;
      rtx consumer;

      consumer = DEP_CON (dep);

      ds = DEP_STATUS (dep);

      if (/* If we want to create speculative dep.  */
	  fs
	  /* And we can do that because this is a true dep.  */
	  && (ds & DEP_TYPES) == DEP_TRUE)
	{
	  gcc_assert (!(ds & BE_IN_SPEC));

	  if (/* If this dep can be overcome with 'begin speculation'.  */
	      ds & BEGIN_SPEC)
	    /* Then we have a choice: keep the dep 'begin speculative'
	       or transform it into 'be in speculative'.  */
	    {
	      if (/* In try_ready we assert that if insn once became ready
		     it can be removed from the ready (or queue) list only
		     due to backend decision.  Hence we can't let the
		     probability of the speculative dep to decrease.  */
		  ds_weak (ds) <= ds_weak (fs))
		{
		  ds_t new_ds;

		  new_ds = (ds & ~BEGIN_SPEC) | fs;

		  if (/* consumer can 'be in speculative'.  */
		      sched_insn_is_legitimate_for_speculation_p (consumer,
								  new_ds))
		    /* Transform it to be in speculative.  */
		    ds = new_ds;
		}
	    }
	  else
	    /* Mark the dep as 'be in speculative'.  */
	    ds |= fs;
	}

      {
	dep_def _new_dep, *new_dep = &_new_dep;

	init_dep_1 (new_dep, twin, consumer, DEP_TYPE (dep), ds);
	sd_add_dep (new_dep, false);
      }
    }
}

/* Generates recovery code for BEGIN speculative INSN.  */
static void
begin_speculative_block (rtx insn)
{
  if (TODO_SPEC (insn) & BEGIN_DATA)
    nr_begin_data++;
  if (TODO_SPEC (insn) & BEGIN_CONTROL)
    nr_begin_control++;

  create_check_block_twin (insn, false);

  TODO_SPEC (insn) &= ~BEGIN_SPEC;
}

static void haifa_init_insn (rtx);

/* Generates recovery code for BE_IN speculative INSN.  */
static void
add_to_speculative_block (rtx insn)
{
  ds_t ts;
  sd_iterator_def sd_it;
  dep_t dep;
  rtx twins = NULL;
  rtx_vec_t priorities_roots;

  ts = TODO_SPEC (insn);
  gcc_assert (!(ts & ~BE_IN_SPEC));

  if (ts & BE_IN_DATA)
    nr_be_in_data++;
  if (ts & BE_IN_CONTROL)
    nr_be_in_control++;

  TODO_SPEC (insn) &= ~BE_IN_SPEC;
  gcc_assert (!TODO_SPEC (insn));

  DONE_SPEC (insn) |= ts;

  /* First we convert all simple checks to branchy.  */
  for (sd_it = sd_iterator_start (insn, SD_LIST_SPEC_BACK);
       sd_iterator_cond (&sd_it, &dep);)
    {
      rtx check = DEP_PRO (dep);

      if (IS_SPECULATION_SIMPLE_CHECK_P (check))
	{
	  create_check_block_twin (check, true);

	  /* Restart search.  */
	  sd_it = sd_iterator_start (insn, SD_LIST_SPEC_BACK);
	}
      else
	/* Continue search.  */
	sd_iterator_next (&sd_it);
    }

  priorities_roots.create (0);
  clear_priorities (insn, &priorities_roots);

  while (1)
    {
      rtx check, twin;
      basic_block rec;

      /* Get the first backward dependency of INSN.  */
      sd_it = sd_iterator_start (insn, SD_LIST_SPEC_BACK);
      if (!sd_iterator_cond (&sd_it, &dep))
	/* INSN has no backward dependencies left.  */
	break;

      gcc_assert ((DEP_STATUS (dep) & BEGIN_SPEC) == 0
		  && (DEP_STATUS (dep) & BE_IN_SPEC) != 0
		  && (DEP_STATUS (dep) & DEP_TYPES) == DEP_TRUE);

      check = DEP_PRO (dep);

      gcc_assert (!IS_SPECULATION_CHECK_P (check) && !ORIG_PAT (check)
		  && QUEUE_INDEX (check) == QUEUE_NOWHERE);

      rec = BLOCK_FOR_INSN (check);

      twin = emit_insn_before (copy_insn (PATTERN (insn)), BB_END (rec));
      haifa_init_insn (twin);

      sd_copy_back_deps (twin, insn, true);

      if (sched_verbose && spec_info->dump)
        /* INSN_BB (insn) isn't determined for twin insns yet.
           So we can't use current_sched_info->print_insn.  */
        fprintf (spec_info->dump, ";;\t\tGenerated twin insn : %d/rec%d\n",
                 INSN_UID (twin), rec->index);

      twins = alloc_INSN_LIST (twin, twins);

      /* Add dependences between TWIN and all appropriate
	 instructions from REC.  */
      FOR_EACH_DEP (insn, SD_LIST_SPEC_BACK, sd_it, dep)
	{
	  rtx pro = DEP_PRO (dep);

	  gcc_assert (DEP_TYPE (dep) == REG_DEP_TRUE);

	  /* INSN might have dependencies from the instructions from
	     several recovery blocks.  At this iteration we process those
	     producers that reside in REC.  */
	  if (BLOCK_FOR_INSN (pro) == rec)
	    {
	      dep_def _new_dep, *new_dep = &_new_dep;

	      init_dep (new_dep, pro, twin, REG_DEP_TRUE);
	      sd_add_dep (new_dep, false);
	    }
	}

      process_insn_forw_deps_be_in_spec (insn, twin, ts);

      /* Remove all dependencies between INSN and insns in REC.  */
      for (sd_it = sd_iterator_start (insn, SD_LIST_SPEC_BACK);
	   sd_iterator_cond (&sd_it, &dep);)
	{
	  rtx pro = DEP_PRO (dep);

	  if (BLOCK_FOR_INSN (pro) == rec)
	    sd_delete_dep (sd_it);
	  else
	    sd_iterator_next (&sd_it);
	}
    }

  /* We couldn't have added the dependencies between INSN and TWINS earlier
     because that would make TWINS appear in the INSN_BACK_DEPS (INSN).  */
  while (twins)
    {
      rtx twin;

      twin = XEXP (twins, 0);

      {
	dep_def _new_dep, *new_dep = &_new_dep;

	init_dep (new_dep, insn, twin, REG_DEP_OUTPUT);
	sd_add_dep (new_dep, false);
      }

      twin = XEXP (twins, 1);
      free_INSN_LIST_node (twins);
      twins = twin;
    }

  calc_priorities (priorities_roots);
  priorities_roots.release ();
}

/* Extends and fills with zeros (only the new part) array pointed to by P.  */
void *
xrecalloc (void *p, size_t new_nmemb, size_t old_nmemb, size_t size)
{
  gcc_assert (new_nmemb >= old_nmemb);
  p = XRESIZEVAR (void, p, new_nmemb * size);
  memset (((char *) p) + old_nmemb * size, 0, (new_nmemb - old_nmemb) * size);
  return p;
}

/* Helper function.
   Find fallthru edge from PRED.  */
edge
find_fallthru_edge_from (basic_block pred)
{
  edge e;
  basic_block succ;

  succ = pred->next_bb;
  gcc_assert (succ->prev_bb == pred);

  if (EDGE_COUNT (pred->succs) <= EDGE_COUNT (succ->preds))
    {
      e = find_fallthru_edge (pred->succs);

      if (e)
	{
	  gcc_assert (e->dest == succ);
	  return e;
	}
    }
  else
    {
      e = find_fallthru_edge (succ->preds);

      if (e)
	{
	  gcc_assert (e->src == pred);
	  return e;
	}
    }

  return NULL;
}

/* Extend per basic block data structures.  */
static void
sched_extend_bb (void)
{
  /* The following is done to keep current_sched_info->next_tail non null.  */
  rtx end = BB_END (EXIT_BLOCK_PTR->prev_bb);
  rtx insn = DEBUG_INSN_P (end) ? prev_nondebug_insn (end) : end;
  if (NEXT_INSN (end) == 0
      || (!NOTE_P (insn)
	  && !LABEL_P (insn)
	  /* Don't emit a NOTE if it would end up before a BARRIER.  */
	  && !BARRIER_P (NEXT_INSN (end))))
    {
      rtx note = emit_note_after (NOTE_INSN_DELETED, end);
      /* Make note appear outside BB.  */
      set_block_for_insn (note, NULL);
      BB_END (EXIT_BLOCK_PTR->prev_bb) = end;
    }
}

/* Init per basic block data structures.  */
void
sched_init_bbs (void)
{
  sched_extend_bb ();
}

/* Initialize BEFORE_RECOVERY variable.  */
static void
init_before_recovery (basic_block *before_recovery_ptr)
{
  basic_block last;
  edge e;

  last = EXIT_BLOCK_PTR->prev_bb;
  e = find_fallthru_edge_from (last);

  if (e)
    {
      /* We create two basic blocks:
         1. Single instruction block is inserted right after E->SRC
         and has jump to
         2. Empty block right before EXIT_BLOCK.
         Between these two blocks recovery blocks will be emitted.  */

      basic_block single, empty;
      rtx x, label;

      /* If the fallthrough edge to exit we've found is from the block we've
	 created before, don't do anything more.  */
      if (last == after_recovery)
	return;

      adding_bb_to_current_region_p = false;

      single = sched_create_empty_bb (last);
      empty = sched_create_empty_bb (single);

      /* Add new blocks to the root loop.  */
      if (current_loops != NULL)
	{
	  add_bb_to_loop (single, (*current_loops->larray)[0]);
	  add_bb_to_loop (empty, (*current_loops->larray)[0]);
	}

      single->count = last->count;
      empty->count = last->count;
      single->frequency = last->frequency;
      empty->frequency = last->frequency;
      BB_COPY_PARTITION (single, last);
      BB_COPY_PARTITION (empty, last);

      redirect_edge_succ (e, single);
      make_single_succ_edge (single, empty, 0);
      make_single_succ_edge (empty, EXIT_BLOCK_PTR, EDGE_FALLTHRU);

      label = block_label (empty);
      x = emit_jump_insn_after (gen_jump (label), BB_END (single));
      JUMP_LABEL (x) = label;
      LABEL_NUSES (label)++;
      haifa_init_insn (x);

      emit_barrier_after (x);

      sched_init_only_bb (empty, NULL);
      sched_init_only_bb (single, NULL);
      sched_extend_bb ();

      adding_bb_to_current_region_p = true;
      before_recovery = single;
      after_recovery = empty;

      if (before_recovery_ptr)
        *before_recovery_ptr = before_recovery;

      if (sched_verbose >= 2 && spec_info->dump)
        fprintf (spec_info->dump,
		 ";;\t\tFixed fallthru to EXIT : %d->>%d->%d->>EXIT\n",
                 last->index, single->index, empty->index);
    }
  else
    before_recovery = last;
}

/* Returns new recovery block.  */
basic_block
sched_create_recovery_block (basic_block *before_recovery_ptr)
{
  rtx label;
  rtx barrier;
  basic_block rec;

  haifa_recovery_bb_recently_added_p = true;
  haifa_recovery_bb_ever_added_p = true;

  init_before_recovery (before_recovery_ptr);

  barrier = get_last_bb_insn (before_recovery);
  gcc_assert (BARRIER_P (barrier));

  label = emit_label_after (gen_label_rtx (), barrier);

  rec = create_basic_block (label, label, before_recovery);

  /* A recovery block always ends with an unconditional jump.  */
  emit_barrier_after (BB_END (rec));

  if (BB_PARTITION (before_recovery) != BB_UNPARTITIONED)
    BB_SET_PARTITION (rec, BB_COLD_PARTITION);

  if (sched_verbose && spec_info->dump)
    fprintf (spec_info->dump, ";;\t\tGenerated recovery block rec%d\n",
             rec->index);

  return rec;
}

/* Create edges: FIRST_BB -> REC; FIRST_BB -> SECOND_BB; REC -> SECOND_BB
   and emit necessary jumps.  */
void
sched_create_recovery_edges (basic_block first_bb, basic_block rec,
			     basic_block second_bb)
{
  rtx label;
  rtx jump;
  int edge_flags;

  /* This is fixing of incoming edge.  */
  /* ??? Which other flags should be specified?  */
  if (BB_PARTITION (first_bb) != BB_PARTITION (rec))
    /* Partition type is the same, if it is "unpartitioned".  */
    edge_flags = EDGE_CROSSING;
  else
    edge_flags = 0;

  make_edge (first_bb, rec, edge_flags);
  label = block_label (second_bb);
  jump = emit_jump_insn_after (gen_jump (label), BB_END (rec));
  JUMP_LABEL (jump) = label;
  LABEL_NUSES (label)++;

  if (BB_PARTITION (second_bb) != BB_PARTITION (rec))
    /* Partition type is the same, if it is "unpartitioned".  */
    {
      /* Rewritten from cfgrtl.c.  */
      if (flag_reorder_blocks_and_partition
	  && targetm_common.have_named_sections)
	{
	  /* We don't need the same note for the check because
	     any_condjump_p (check) == true.  */
	  add_reg_note (jump, REG_CROSSING_JUMP, NULL_RTX);
	}
      edge_flags = EDGE_CROSSING;
    }
  else
    edge_flags = 0;

  make_single_succ_edge (rec, second_bb, edge_flags);
  if (dom_info_available_p (CDI_DOMINATORS))
    set_immediate_dominator (CDI_DOMINATORS, rec, first_bb);
}

/* This function creates recovery code for INSN.  If MUTATE_P is nonzero,
   INSN is a simple check, that should be converted to branchy one.  */
static void
create_check_block_twin (rtx insn, bool mutate_p)
{
  basic_block rec;
  rtx label, check, twin;
  ds_t fs;
  sd_iterator_def sd_it;
  dep_t dep;
  dep_def _new_dep, *new_dep = &_new_dep;
  ds_t todo_spec;

  gcc_assert (ORIG_PAT (insn) != NULL_RTX);

  if (!mutate_p)
    todo_spec = TODO_SPEC (insn);
  else
    {
      gcc_assert (IS_SPECULATION_SIMPLE_CHECK_P (insn)
		  && (TODO_SPEC (insn) & SPECULATIVE) == 0);

      todo_spec = CHECK_SPEC (insn);
    }

  todo_spec &= SPECULATIVE;

  /* Create recovery block.  */
  if (mutate_p || targetm.sched.needs_block_p (todo_spec))
    {
      rec = sched_create_recovery_block (NULL);
      label = BB_HEAD (rec);
    }
  else
    {
      rec = EXIT_BLOCK_PTR;
      label = NULL_RTX;
    }

  /* Emit CHECK.  */
  check = targetm.sched.gen_spec_check (insn, label, todo_spec);

  if (rec != EXIT_BLOCK_PTR)
    {
      /* To have mem_reg alive at the beginning of second_bb,
	 we emit check BEFORE insn, so insn after splitting
	 insn will be at the beginning of second_bb, which will
	 provide us with the correct life information.  */
      check = emit_jump_insn_before (check, insn);
      JUMP_LABEL (check) = label;
      LABEL_NUSES (label)++;
    }
  else
    check = emit_insn_before (check, insn);

  /* Extend data structures.  */
  haifa_init_insn (check);

  /* CHECK is being added to current region.  Extend ready list.  */
  gcc_assert (sched_ready_n_insns != -1);
  sched_extend_ready_list (sched_ready_n_insns + 1);

  if (current_sched_info->add_remove_insn)
    current_sched_info->add_remove_insn (insn, 0);

  RECOVERY_BLOCK (check) = rec;

  if (sched_verbose && spec_info->dump)
    fprintf (spec_info->dump, ";;\t\tGenerated check insn : %s\n",
             (*current_sched_info->print_insn) (check, 0));

  gcc_assert (ORIG_PAT (insn));

  /* Initialize TWIN (twin is a duplicate of original instruction
     in the recovery block).  */
  if (rec != EXIT_BLOCK_PTR)
    {
      sd_iterator_def sd_it;
      dep_t dep;

      FOR_EACH_DEP (insn, SD_LIST_RES_BACK, sd_it, dep)
	if ((DEP_STATUS (dep) & DEP_OUTPUT) != 0)
	  {
	    struct _dep _dep2, *dep2 = &_dep2;

	    init_dep (dep2, DEP_PRO (dep), check, REG_DEP_TRUE);

	    sd_add_dep (dep2, true);
	  }

      twin = emit_insn_after (ORIG_PAT (insn), BB_END (rec));
      haifa_init_insn (twin);

      if (sched_verbose && spec_info->dump)
	/* INSN_BB (insn) isn't determined for twin insns yet.
	   So we can't use current_sched_info->print_insn.  */
	fprintf (spec_info->dump, ";;\t\tGenerated twin insn : %d/rec%d\n",
		 INSN_UID (twin), rec->index);
    }
  else
    {
      ORIG_PAT (check) = ORIG_PAT (insn);
      HAS_INTERNAL_DEP (check) = 1;
      twin = check;
      /* ??? We probably should change all OUTPUT dependencies to
	 (TRUE | OUTPUT).  */
    }

  /* Copy all resolved back dependencies of INSN to TWIN.  This will
     provide correct value for INSN_TICK (TWIN).  */
  sd_copy_back_deps (twin, insn, true);

  if (rec != EXIT_BLOCK_PTR)
    /* In case of branchy check, fix CFG.  */
    {
      basic_block first_bb, second_bb;
      rtx jump;

      first_bb = BLOCK_FOR_INSN (check);
      second_bb = sched_split_block (first_bb, check);

      sched_create_recovery_edges (first_bb, rec, second_bb);

      sched_init_only_bb (second_bb, first_bb);
      sched_init_only_bb (rec, EXIT_BLOCK_PTR);

      jump = BB_END (rec);
      haifa_init_insn (jump);
    }

  /* Move backward dependences from INSN to CHECK and
     move forward dependences from INSN to TWIN.  */

  /* First, create dependencies between INSN's producers and CHECK & TWIN.  */
  FOR_EACH_DEP (insn, SD_LIST_BACK, sd_it, dep)
    {
      rtx pro = DEP_PRO (dep);
      ds_t ds;

      /* If BEGIN_DATA: [insn ~~TRUE~~> producer]:
	 check --TRUE--> producer  ??? or ANTI ???
	 twin  --TRUE--> producer
	 twin  --ANTI--> check

	 If BEGIN_CONTROL: [insn ~~ANTI~~> producer]:
	 check --ANTI--> producer
	 twin  --ANTI--> producer
	 twin  --ANTI--> check

	 If BE_IN_SPEC: [insn ~~TRUE~~> producer]:
	 check ~~TRUE~~> producer
	 twin  ~~TRUE~~> producer
	 twin  --ANTI--> check  */

      ds = DEP_STATUS (dep);

      if (ds & BEGIN_SPEC)
	{
	  gcc_assert (!mutate_p);
	  ds &= ~BEGIN_SPEC;
	}

      init_dep_1 (new_dep, pro, check, DEP_TYPE (dep), ds);
      sd_add_dep (new_dep, false);

      if (rec != EXIT_BLOCK_PTR)
	{
	  DEP_CON (new_dep) = twin;
	  sd_add_dep (new_dep, false);
	}
    }

  /* Second, remove backward dependencies of INSN.  */
  for (sd_it = sd_iterator_start (insn, SD_LIST_SPEC_BACK);
       sd_iterator_cond (&sd_it, &dep);)
    {
      if ((DEP_STATUS (dep) & BEGIN_SPEC)
	  || mutate_p)
	/* We can delete this dep because we overcome it with
	   BEGIN_SPECULATION.  */
	sd_delete_dep (sd_it);
      else
	sd_iterator_next (&sd_it);
    }

  /* Future Speculations.  Determine what BE_IN speculations will be like.  */
  fs = 0;

  /* Fields (DONE_SPEC (x) & BEGIN_SPEC) and CHECK_SPEC (x) are set only
     here.  */

  gcc_assert (!DONE_SPEC (insn));

  if (!mutate_p)
    {
      ds_t ts = TODO_SPEC (insn);

      DONE_SPEC (insn) = ts & BEGIN_SPEC;
      CHECK_SPEC (check) = ts & BEGIN_SPEC;

      /* Luckiness of future speculations solely depends upon initial
	 BEGIN speculation.  */
      if (ts & BEGIN_DATA)
	fs = set_dep_weak (fs, BE_IN_DATA, get_dep_weak (ts, BEGIN_DATA));
      if (ts & BEGIN_CONTROL)
	fs = set_dep_weak (fs, BE_IN_CONTROL,
			   get_dep_weak (ts, BEGIN_CONTROL));
    }
  else
    CHECK_SPEC (check) = CHECK_SPEC (insn);

  /* Future speculations: call the helper.  */
  process_insn_forw_deps_be_in_spec (insn, twin, fs);

  if (rec != EXIT_BLOCK_PTR)
    {
      /* Which types of dependencies should we use here is,
	 generally, machine-dependent question...  But, for now,
	 it is not.  */

      if (!mutate_p)
	{
	  init_dep (new_dep, insn, check, REG_DEP_TRUE);
	  sd_add_dep (new_dep, false);

	  init_dep (new_dep, insn, twin, REG_DEP_OUTPUT);
	  sd_add_dep (new_dep, false);
	}
      else
	{
	  if (spec_info->dump)
	    fprintf (spec_info->dump, ";;\t\tRemoved simple check : %s\n",
		     (*current_sched_info->print_insn) (insn, 0));

	  /* Remove all dependencies of the INSN.  */
	  {
	    sd_it = sd_iterator_start (insn, (SD_LIST_FORW
					      | SD_LIST_BACK
					      | SD_LIST_RES_BACK));
	    while (sd_iterator_cond (&sd_it, &dep))
	      sd_delete_dep (sd_it);
	  }

	  /* If former check (INSN) already was moved to the ready (or queue)
	     list, add new check (CHECK) there too.  */
	  if (QUEUE_INDEX (insn) != QUEUE_NOWHERE)
	    try_ready (check);

	  /* Remove old check from instruction stream and free its
	     data.  */
	  sched_remove_insn (insn);
	}

      init_dep (new_dep, check, twin, REG_DEP_ANTI);
      sd_add_dep (new_dep, false);
    }
  else
    {
      init_dep_1 (new_dep, insn, check, REG_DEP_TRUE, DEP_TRUE | DEP_OUTPUT);
      sd_add_dep (new_dep, false);
    }

  if (!mutate_p)
    /* Fix priorities.  If MUTATE_P is nonzero, this is not necessary,
       because it'll be done later in add_to_speculative_block.  */
    {
      rtx_vec_t priorities_roots = rtx_vec_t ();

      clear_priorities (twin, &priorities_roots);
      calc_priorities (priorities_roots);
      priorities_roots.release ();
    }
}

/* Removes dependency between instructions in the recovery block REC
   and usual region instructions.  It keeps inner dependences so it
   won't be necessary to recompute them.  */
static void
fix_recovery_deps (basic_block rec)
{
  rtx note, insn, jump, ready_list = 0;
  bitmap_head in_ready;
  rtx link;

  bitmap_initialize (&in_ready, 0);

  /* NOTE - a basic block note.  */
  note = NEXT_INSN (BB_HEAD (rec));
  gcc_assert (NOTE_INSN_BASIC_BLOCK_P (note));
  insn = BB_END (rec);
  gcc_assert (JUMP_P (insn));
  insn = PREV_INSN (insn);

  do
    {
      sd_iterator_def sd_it;
      dep_t dep;

      for (sd_it = sd_iterator_start (insn, SD_LIST_FORW);
	   sd_iterator_cond (&sd_it, &dep);)
	{
	  rtx consumer = DEP_CON (dep);

	  if (BLOCK_FOR_INSN (consumer) != rec)
	    {
	      sd_delete_dep (sd_it);

	      if (bitmap_set_bit (&in_ready, INSN_LUID (consumer)))
		ready_list = alloc_INSN_LIST (consumer, ready_list);
	    }
	  else
	    {
	      gcc_assert ((DEP_STATUS (dep) & DEP_TYPES) == DEP_TRUE);

	      sd_iterator_next (&sd_it);
	    }
	}

      insn = PREV_INSN (insn);
    }
  while (insn != note);

  bitmap_clear (&in_ready);

  /* Try to add instructions to the ready or queue list.  */
  for (link = ready_list; link; link = XEXP (link, 1))
    try_ready (XEXP (link, 0));
  free_INSN_LIST_list (&ready_list);

  /* Fixing jump's dependences.  */
  insn = BB_HEAD (rec);
  jump = BB_END (rec);

  gcc_assert (LABEL_P (insn));
  insn = NEXT_INSN (insn);

  gcc_assert (NOTE_INSN_BASIC_BLOCK_P (insn));
  add_jump_dependencies (insn, jump);
}

/* Change pattern of INSN to NEW_PAT.  Invalidate cached haifa
   instruction data.  */
static bool
haifa_change_pattern (rtx insn, rtx new_pat)
{
  int t;

  t = validate_change (insn, &PATTERN (insn), new_pat, 0);
  if (!t)
    return false;

  update_insn_after_change (insn);
  return true;
}

/* -1 - can't speculate,
   0 - for speculation with REQUEST mode it is OK to use
   current instruction pattern,
   1 - need to change pattern for *NEW_PAT to be speculative.  */
int
sched_speculate_insn (rtx insn, ds_t request, rtx *new_pat)
{
  gcc_assert (current_sched_info->flags & DO_SPECULATION
              && (request & SPECULATIVE)
	      && sched_insn_is_legitimate_for_speculation_p (insn, request));

  if ((request & spec_info->mask) != request)
    return -1;

  if (request & BE_IN_SPEC
      && !(request & BEGIN_SPEC))
    return 0;

  return targetm.sched.speculate_insn (insn, request, new_pat);
}

static int
haifa_speculate_insn (rtx insn, ds_t request, rtx *new_pat)
{
  gcc_assert (sched_deps_info->generate_spec_deps
	      && !IS_SPECULATION_CHECK_P (insn));

  if (HAS_INTERNAL_DEP (insn)
      || SCHED_GROUP_P (insn))
    return -1;

  return sched_speculate_insn (insn, request, new_pat);
}

/* Print some information about block BB, which starts with HEAD and
   ends with TAIL, before scheduling it.
   I is zero, if scheduler is about to start with the fresh ebb.  */
static void
dump_new_block_header (int i, basic_block bb, rtx head, rtx tail)
{
  if (!i)
    fprintf (sched_dump,
	     ";;   ======================================================\n");
  else
    fprintf (sched_dump,
	     ";;   =====================ADVANCING TO=====================\n");
  fprintf (sched_dump,
	   ";;   -- basic block %d from %d to %d -- %s reload\n",
	   bb->index, INSN_UID (head), INSN_UID (tail),
	   (reload_completed ? "after" : "before"));
  fprintf (sched_dump,
	   ";;   ======================================================\n");
  fprintf (sched_dump, "\n");
}

/* Unlink basic block notes and labels and saves them, so they
   can be easily restored.  We unlink basic block notes in EBB to
   provide back-compatibility with the previous code, as target backends
   assume, that there'll be only instructions between
   current_sched_info->{head and tail}.  We restore these notes as soon
   as we can.
   FIRST (LAST) is the first (last) basic block in the ebb.
   NB: In usual case (FIRST == LAST) nothing is really done.  */
void
unlink_bb_notes (basic_block first, basic_block last)
{
  /* We DON'T unlink basic block notes of the first block in the ebb.  */
  if (first == last)
    return;

  bb_header = XNEWVEC (rtx, last_basic_block);

  /* Make a sentinel.  */
  if (last->next_bb != EXIT_BLOCK_PTR)
    bb_header[last->next_bb->index] = 0;

  first = first->next_bb;
  do
    {
      rtx prev, label, note, next;

      label = BB_HEAD (last);
      if (LABEL_P (label))
	note = NEXT_INSN (label);
      else
	note = label;
      gcc_assert (NOTE_INSN_BASIC_BLOCK_P (note));

      prev = PREV_INSN (label);
      next = NEXT_INSN (note);
      gcc_assert (prev && next);

      NEXT_INSN (prev) = next;
      PREV_INSN (next) = prev;

      bb_header[last->index] = label;

      if (last == first)
	break;

      last = last->prev_bb;
    }
  while (1);
}

/* Restore basic block notes.
   FIRST is the first basic block in the ebb.  */
static void
restore_bb_notes (basic_block first)
{
  if (!bb_header)
    return;

  /* We DON'T unlink basic block notes of the first block in the ebb.  */
  first = first->next_bb;
  /* Remember: FIRST is actually a second basic block in the ebb.  */

  while (first != EXIT_BLOCK_PTR
	 && bb_header[first->index])
    {
      rtx prev, label, note, next;

      label = bb_header[first->index];
      prev = PREV_INSN (label);
      next = NEXT_INSN (prev);

      if (LABEL_P (label))
	note = NEXT_INSN (label);
      else
	note = label;
      gcc_assert (NOTE_INSN_BASIC_BLOCK_P (note));

      bb_header[first->index] = 0;

      NEXT_INSN (prev) = label;
      NEXT_INSN (note) = next;
      PREV_INSN (next) = note;

      first = first->next_bb;
    }

  free (bb_header);
  bb_header = 0;
}

/* Helper function.
   Fix CFG after both in- and inter-block movement of
   control_flow_insn_p JUMP.  */
static void
fix_jump_move (rtx jump)
{
  basic_block bb, jump_bb, jump_bb_next;

  bb = BLOCK_FOR_INSN (PREV_INSN (jump));
  jump_bb = BLOCK_FOR_INSN (jump);
  jump_bb_next = jump_bb->next_bb;

  gcc_assert (common_sched_info->sched_pass_id == SCHED_EBB_PASS
	      || IS_SPECULATION_BRANCHY_CHECK_P (jump));

  if (!NOTE_INSN_BASIC_BLOCK_P (BB_END (jump_bb_next)))
    /* if jump_bb_next is not empty.  */
    BB_END (jump_bb) = BB_END (jump_bb_next);

  if (BB_END (bb) != PREV_INSN (jump))
    /* Then there are instruction after jump that should be placed
       to jump_bb_next.  */
    BB_END (jump_bb_next) = BB_END (bb);
  else
    /* Otherwise jump_bb_next is empty.  */
    BB_END (jump_bb_next) = NEXT_INSN (BB_HEAD (jump_bb_next));

  /* To make assertion in move_insn happy.  */
  BB_END (bb) = PREV_INSN (jump);

  update_bb_for_insn (jump_bb_next);
}

/* Fix CFG after interblock movement of control_flow_insn_p JUMP.  */
static void
move_block_after_check (rtx jump)
{
  basic_block bb, jump_bb, jump_bb_next;
  vec<edge, va_gc> *t;

  bb = BLOCK_FOR_INSN (PREV_INSN (jump));
  jump_bb = BLOCK_FOR_INSN (jump);
  jump_bb_next = jump_bb->next_bb;

  update_bb_for_insn (jump_bb);

  gcc_assert (IS_SPECULATION_CHECK_P (jump)
	      || IS_SPECULATION_CHECK_P (BB_END (jump_bb_next)));

  unlink_block (jump_bb_next);
  link_block (jump_bb_next, bb);

  t = bb->succs;
  bb->succs = 0;
  move_succs (&(jump_bb->succs), bb);
  move_succs (&(jump_bb_next->succs), jump_bb);
  move_succs (&t, jump_bb_next);

  df_mark_solutions_dirty ();

  common_sched_info->fix_recovery_cfg
    (bb->index, jump_bb->index, jump_bb_next->index);
}

/* Helper function for move_block_after_check.
   This functions attaches edge vector pointed to by SUCCSP to
   block TO.  */
static void
move_succs (vec<edge, va_gc> **succsp, basic_block to)
{
  edge e;
  edge_iterator ei;

  gcc_assert (to->succs == 0);

  to->succs = *succsp;

  FOR_EACH_EDGE (e, ei, to->succs)
    e->src = to;

  *succsp = 0;
}

/* Remove INSN from the instruction stream.
   INSN should have any dependencies.  */
static void
sched_remove_insn (rtx insn)
{
  sd_finish_insn (insn);

  change_queue_index (insn, QUEUE_NOWHERE);
  current_sched_info->add_remove_insn (insn, 1);
  delete_insn (insn);
}

/* Clear priorities of all instructions, that are forward dependent on INSN.
   Store in vector pointed to by ROOTS_PTR insns on which priority () should
   be invoked to initialize all cleared priorities.  */
static void
clear_priorities (rtx insn, rtx_vec_t *roots_ptr)
{
  sd_iterator_def sd_it;
  dep_t dep;
  bool insn_is_root_p = true;

  gcc_assert (QUEUE_INDEX (insn) != QUEUE_SCHEDULED);

  FOR_EACH_DEP (insn, SD_LIST_BACK, sd_it, dep)
    {
      rtx pro = DEP_PRO (dep);

      if (INSN_PRIORITY_STATUS (pro) >= 0
	  && QUEUE_INDEX (insn) != QUEUE_SCHEDULED)
	{
	  /* If DEP doesn't contribute to priority then INSN itself should
	     be added to priority roots.  */
	  if (contributes_to_priority_p (dep))
	    insn_is_root_p = false;

	  INSN_PRIORITY_STATUS (pro) = -1;
	  clear_priorities (pro, roots_ptr);
	}
    }

  if (insn_is_root_p)
    roots_ptr->safe_push (insn);
}

/* Recompute priorities of instructions, whose priorities might have been
   changed.  ROOTS is a vector of instructions whose priority computation will
   trigger initialization of all cleared priorities.  */
static void
calc_priorities (rtx_vec_t roots)
{
  int i;
  rtx insn;

  FOR_EACH_VEC_ELT (roots, i, insn)
    priority (insn);
}


/* Add dependences between JUMP and other instructions in the recovery
   block.  INSN is the first insn the recovery block.  */
static void
add_jump_dependencies (rtx insn, rtx jump)
{
  do
    {
      insn = NEXT_INSN (insn);
      if (insn == jump)
	break;

      if (dep_list_size (insn, SD_LIST_FORW) == 0)
	{
	  dep_def _new_dep, *new_dep = &_new_dep;

	  init_dep (new_dep, insn, jump, REG_DEP_ANTI);
	  sd_add_dep (new_dep, false);
	}
    }
  while (1);

  gcc_assert (!sd_lists_empty_p (jump, SD_LIST_BACK));
}

/* Extend data structures for logical insn UID.  */
void
sched_extend_luids (void)
{
  int new_luids_max_uid = get_max_uid () + 1;

  sched_luids.safe_grow_cleared (new_luids_max_uid);
}

/* Initialize LUID for INSN.  */
void
sched_init_insn_luid (rtx insn)
{
  int i = INSN_P (insn) ? 1 : common_sched_info->luid_for_non_insn (insn);
  int luid;

  if (i >= 0)
    {
      luid = sched_max_luid;
      sched_max_luid += i;
    }
  else
    luid = -1;

  SET_INSN_LUID (insn, luid);
}

/* Initialize luids for BBS.
   The hook common_sched_info->luid_for_non_insn () is used to determine
   if notes, labels, etc. need luids.  */
void
sched_init_luids (bb_vec_t bbs)
{
  int i;
  basic_block bb;

  sched_extend_luids ();
  FOR_EACH_VEC_ELT (bbs, i, bb)
    {
      rtx insn;

      FOR_BB_INSNS (bb, insn)
	sched_init_insn_luid (insn);
    }
}

/* Free LUIDs.  */
void
sched_finish_luids (void)
{
  sched_luids.release ();
  sched_max_luid = 1;
}

/* Return logical uid of INSN.  Helpful while debugging.  */
int
insn_luid (rtx insn)
{
  return INSN_LUID (insn);
}

/* Extend per insn data in the target.  */
void
sched_extend_target (void)
{
  if (targetm.sched.h_i_d_extended)
    targetm.sched.h_i_d_extended ();
}

/* Extend global scheduler structures (those, that live across calls to
   schedule_block) to include information about just emitted INSN.  */
static void
extend_h_i_d (void)
{
  int reserve = (get_max_uid () + 1 - h_i_d.length ());
  if (reserve > 0
      && ! h_i_d.space (reserve))
    {
      h_i_d.safe_grow_cleared (3 * get_max_uid () / 2);
      sched_extend_target ();
    }
}

/* Initialize h_i_d entry of the INSN with default values.
   Values, that are not explicitly initialized here, hold zero.  */
static void
init_h_i_d (rtx insn)
{
  if (INSN_LUID (insn) > 0)
    {
      INSN_COST (insn) = -1;
      QUEUE_INDEX (insn) = QUEUE_NOWHERE;
      INSN_TICK (insn) = INVALID_TICK;
      INSN_EXACT_TICK (insn) = INVALID_TICK;
      INTER_TICK (insn) = INVALID_TICK;
      TODO_SPEC (insn) = HARD_DEP;
    }
}

/* Initialize haifa_insn_data for BBS.  */
void
haifa_init_h_i_d (bb_vec_t bbs)
{
  int i;
  basic_block bb;

  extend_h_i_d ();
  FOR_EACH_VEC_ELT (bbs, i, bb)
    {
      rtx insn;

      FOR_BB_INSNS (bb, insn)
	init_h_i_d (insn);
    }
}

/* Finalize haifa_insn_data.  */
void
haifa_finish_h_i_d (void)
{
  int i;
  haifa_insn_data_t data;
  struct reg_use_data *use, *next;

  FOR_EACH_VEC_ELT (h_i_d, i, data)
    {
      free (data->max_reg_pressure);
      free (data->reg_pressure);
      for (use = data->reg_use_list; use != NULL; use = next)
	{
	  next = use->next_insn_use;
	  free (use);
	}
    }
  h_i_d.release ();
}

/* Init data for the new insn INSN.  */
static void
haifa_init_insn (rtx insn)
{
  gcc_assert (insn != NULL);

  sched_extend_luids ();
  sched_init_insn_luid (insn);
  sched_extend_target ();
  sched_deps_init (false);
  extend_h_i_d ();
  init_h_i_d (insn);

  if (adding_bb_to_current_region_p)
    {
      sd_init_insn (insn);

      /* Extend dependency caches by one element.  */
      extend_dependency_caches (1, false);
    }
  if (sched_pressure != SCHED_PRESSURE_NONE)
    init_insn_reg_pressure_info (insn);
}

/* Init data for the new basic block BB which comes after AFTER.  */
static void
haifa_init_only_bb (basic_block bb, basic_block after)
{
  gcc_assert (bb != NULL);

  sched_init_bbs ();

  if (common_sched_info->add_block)
    /* This changes only data structures of the front-end.  */
    common_sched_info->add_block (bb, after);
}

/* A generic version of sched_split_block ().  */
basic_block
sched_split_block_1 (basic_block first_bb, rtx after)
{
  edge e;

  e = split_block (first_bb, after);
  gcc_assert (e->src == first_bb);

  /* sched_split_block emits note if *check == BB_END.  Probably it
     is better to rip that note off.  */

  return e->dest;
}

/* A generic version of sched_create_empty_bb ().  */
basic_block
sched_create_empty_bb_1 (basic_block after)
{
  return create_empty_bb (after);
}

/* Insert PAT as an INSN into the schedule and update the necessary data
   structures to account for it. */
rtx
sched_emit_insn (rtx pat)
{
  rtx insn = emit_insn_before (pat, nonscheduled_insns_begin);
  haifa_init_insn (insn);

  if (current_sched_info->add_remove_insn)
    current_sched_info->add_remove_insn (insn, 0);

  (*current_sched_info->begin_schedule_ready) (insn);
  scheduled_insns.safe_push (insn);

  last_scheduled_insn = insn;
  return insn;
}

/* This function returns a candidate satisfying dispatch constraints from
   the ready list.  */

static rtx
ready_remove_first_dispatch (struct ready_list *ready)
{
  int i;
  rtx insn = ready_element (ready, 0);

  if (ready->n_ready == 1
      || !INSN_P (insn)
      || INSN_CODE (insn) < 0
      || !active_insn_p (insn)
      || targetm.sched.dispatch (insn, FITS_DISPATCH_WINDOW))
    return ready_remove_first (ready);

  for (i = 1; i < ready->n_ready; i++)
    {
      insn = ready_element (ready, i);

      if (!INSN_P (insn)
	  || INSN_CODE (insn) < 0
	  || !active_insn_p (insn))
	continue;

      if (targetm.sched.dispatch (insn, FITS_DISPATCH_WINDOW))
	{
	  /* Return ith element of ready.  */
	  insn = ready_remove (ready, i);
	  return insn;
	}
    }

  if (targetm.sched.dispatch (NULL_RTX, DISPATCH_VIOLATION))
    return ready_remove_first (ready);

  for (i = 1; i < ready->n_ready; i++)
    {
      insn = ready_element (ready, i);

      if (! INSN_P (insn)
	  || INSN_CODE (insn) < 0
	  || !active_insn_p (insn))
	continue;

      /* Return i-th element of ready.  */
      if (targetm.sched.dispatch (insn, IS_CMP))
	return ready_remove (ready, i);
    }

  return ready_remove_first (ready);
}

/* Get number of ready insn in the ready list.  */

int
number_in_ready (void)
{
  return ready.n_ready;
}

/* Get number of ready's in the ready list.  */

rtx
get_ready_element (int i)
{
  return ready_element (&ready, i);
}

#endif /* INSN_SCHEDULING */
