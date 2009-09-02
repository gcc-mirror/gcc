/* Instruction scheduling pass.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
   2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.
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
#include "toplev.h"
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "regs.h"
#include "function.h"
#include "flags.h"
#include "insn-config.h"
#include "insn-attr.h"
#include "except.h"
#include "toplev.h"
#include "recog.h"
#include "sched-int.h"
#include "target.h"
#include "output.h"
#include "params.h"
#include "vecprim.h"
#include "dbgcnt.h"
#include "cfgloop.h"

#ifdef INSN_SCHEDULING

/* issue_rate is the number of insns that can be scheduled in the same
   machine cycle.  It can be defined in the config/mach/mach.h file,
   otherwise we set it to 1.  */

int issue_rate;

/* sched-verbose controls the amount of debugging output the
   scheduler prints.  It is controlled by -fsched-verbose=N:
   N>0 and no -DSR : the output is directed to stderr.
   N>=10 will direct the printouts to stderr (regardless of -dSR).
   N=1: same as -dSR.
   N=2: bb's probabilities, detailed ready list info, unit/insn info.
   N=3: rtl at abort point, control-flow, regions info.
   N=5: dependences info.  */

static int sched_verbose_param = 0;
int sched_verbose = 0;

/* Debugging file.  All printouts are sent to dump, which is always set,
   either to stderr, or to the dump listing file (-dRS).  */
FILE *sched_dump = 0;

/* fix_sched_param() is called from toplev.c upon detection
   of the -fsched-verbose=N option.  */

void
fix_sched_param (const char *param, const char *val)
{
  if (!strcmp (param, "verbose"))
    sched_verbose_param = atoi (val);
  else
    warning (0, "fix_sched_param: unknown param: %s", param);
}

/* This is a placeholder for the scheduler parameters common 
   to all schedulers.  */
struct common_sched_info_def *common_sched_info;

#define INSN_TICK(INSN)	(HID (INSN)->tick)
#define INTER_TICK(INSN) (HID (INSN)->inter_tick)

/* If INSN_TICK of an instruction is equal to INVALID_TICK,
   then it should be recalculated from scratch.  */
#define INVALID_TICK (-(max_insn_queue_index + 1))
/* The minimal value of the INSN_TICK of an instruction.  */
#define MIN_TICK (-max_insn_queue_index)

/* Issue points are used to distinguish between instructions in max_issue ().
   For now, all instructions are equally good.  */
#define ISSUE_POINTS(INSN) 1

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

const struct sched_scan_info_def *sched_scan_info;

/* Mapping from instruction UID to its Logical UID.  */
VEC (int, heap) *sched_luids = NULL;

/* Next LUID to assign to an instruction.  */
int sched_max_luid = 1;

/* Haifa Instruction Data.  */
VEC (haifa_insn_data_def, heap) *h_i_d = NULL;

void (* sched_init_only_bb) (basic_block, basic_block);

/* Split block function.  Different schedulers might use different functions
   to handle their internal data consistent.  */
basic_block (* sched_split_block) (basic_block, rtx);

/* Create empty basic block after the specified block.  */
basic_block (* sched_create_empty_bb) (basic_block);

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

/* Forward declarations.  */

static int priority (rtx);
static int rank_for_schedule (const void *, const void *);
static void swap_sort (rtx *, int);
static void queue_insn (rtx, int);
static int schedule_insn (rtx);
static int find_set_reg_weight (const_rtx);
static void find_insn_reg_weight (const_rtx);
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

static void queue_to_ready (struct ready_list *);
static int early_queue_to_ready (state_t, struct ready_list *);

static void debug_ready_list (struct ready_list *);

/* The following functions are used to implement multi-pass scheduling
   on the first cycle.  */
static rtx ready_remove (struct ready_list *, int);
static void ready_remove_insn (rtx);

static int choose_ready (struct ready_list *, rtx *);

static void fix_inter_tick (rtx, rtx);
static int fix_tick_ready (rtx);
static void change_queue_index (rtx, int);

/* The following functions are used to implement scheduling of data/control
   speculative instructions.  */

static void extend_h_i_d (void);
static void init_h_i_d (rtx);
static void generate_recovery_code (rtx);
static void process_insn_forw_deps_be_in_spec (rtx, rtx, ds_t);
static void begin_speculative_block (rtx);
static void add_to_speculative_block (rtx);
static void init_before_recovery (basic_block *);
static void create_check_block_twin (rtx, bool);
static void fix_recovery_deps (basic_block);
static void haifa_change_pattern (rtx, rtx);
static void dump_new_block_header (int, basic_block, rtx, rtx);
static void restore_bb_notes (basic_block);
static void fix_jump_move (rtx);
static void move_block_after_check (rtx);
static void move_succs (VEC(edge,gc) **, basic_block);
static void sched_remove_insn (rtx);
static void clear_priorities (rtx, rtx_vec_t *);
static void calc_priorities (rtx_vec_t);
static void add_jump_dependencies (rtx, rtx);
#ifdef ENABLE_CHECKING
static int has_edge_p (VEC(edge,gc) *, int);
static void check_cfg (rtx, rtx);
#endif

#endif /* INSN_SCHEDULING */

/* Point to state used for the current scheduling pass.  */
struct haifa_sched_info *current_sched_info;

#ifndef INSN_SCHEDULING
void
schedule_insns (void)
{
}
#else

/* Pointer to the last instruction scheduled.  Used by rank_for_schedule,
   so that insns independent of the last scheduled insn will be preferred
   over dependent instructions.  */

static rtx last_scheduled_insn;

/* Cached cost of the instruction.  Use below function to get cost of the
   insn.  -1 here means that the field is not initialized.  */
#define INSN_COST(INSN)	(HID (INSN)->cost)

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

  /* A USE insn should never require the value used to be computed.
     This allows the computation of a function's result and parameter
     values to overlap the return and call.  */
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
	{
	  cost = targetm.sched.adjust_cost_2 (used, (int) dep_type, insn, cost,
					      dw);
	}
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

/* Compute the number of nondebug forward deps of an insn.  */

static int
dep_list_size (rtx insn)
{
  sd_iterator_def sd_it;
  dep_t dep;
  int dbgcount = 0, nodbgcount = 0;

  if (!MAY_HAVE_DEBUG_INSNS)
    return sd_lists_size (insn, SD_LIST_FORW);

  FOR_EACH_DEP (insn, SD_LIST_FORW, sd_it, dep)
    {
      if (DEBUG_INSN_P (DEP_CON (dep)))
	dbgcount++;
      else
	nodbgcount++;
    }

  gcc_assert (dbgcount + nodbgcount == sd_lists_size (insn, SD_LIST_FORW));

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

      if (dep_list_size (insn) == 0)
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

/* Returns a positive value if x is preferred; returns a negative value if
   y is preferred.  Should never return 0, since that will make the sort
   unstable.  */

static int
rank_for_schedule (const void *x, const void *y)
{
  rtx tmp = *(const rtx *) y;
  rtx tmp2 = *(const rtx *) x;
  rtx last;
  int tmp_class, tmp2_class;
  int val, priority_val, weight_val, info_val;

  if (MAY_HAVE_DEBUG_INSNS)
    {
      /* Schedule debug insns as early as possible.  */
      if (DEBUG_INSN_P (tmp) && !DEBUG_INSN_P (tmp2))
	return -1;
      else if (DEBUG_INSN_P (tmp2))
	return 1;
    }

  /* The insn in a schedule group should be issued the first.  */
  if (flag_sched_group_heuristic && 
      SCHED_GROUP_P (tmp) != SCHED_GROUP_P (tmp2))
    return SCHED_GROUP_P (tmp2) ? 1 : -1;

  /* Make sure that priority of TMP and TMP2 are initialized.  */
  gcc_assert (INSN_PRIORITY_KNOWN (tmp) && INSN_PRIORITY_KNOWN (tmp2));

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

  /* Prefer an insn with smaller contribution to registers-pressure.  */
  if (flag_sched_reg_pressure_heuristic && !reload_completed &&
      (weight_val = INSN_REG_WEIGHT (tmp) - INSN_REG_WEIGHT (tmp2)))
    return weight_val;

  info_val = (*current_sched_info->rank) (tmp, tmp2);
  if(flag_sched_rank_heuristic && info_val)
    return info_val;

  if (flag_sched_last_insn_heuristic)
    {
      last = last_scheduled_insn;

      if (DEBUG_INSN_P (last) && last != current_sched_info->prev_head)
	do
	  last = PREV_INSN (last);
	while (!NONDEBUG_INSN_P (last)
	       && last != current_sched_info->prev_head);
    }

  /* Compare insns based on their relation to the last scheduled
     non-debug insn.  */
  if (flag_sched_last_insn_heuristic && NONDEBUG_INSN_P (last))
    {
      dep_t dep1;
      dep_t dep2;

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

  /* Prefer the insn which has more later insns that depend on it.
     This gives the scheduler more freedom when scheduling later
     instructions at the expense of added register pressure.  */

  val = (dep_list_size (tmp2) - dep_list_size (tmp));

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
   chain for debugging purposes.  */

HAIFA_INLINE static void
queue_insn (rtx insn, int n_cycles)
{
  int next_q = NEXT_Q_AFTER (q_ptr, n_cycles);
  rtx link = alloc_INSN_LIST (insn, insn_queue[next_q]);

  gcc_assert (n_cycles <= max_insn_queue_index);
  gcc_assert (!DEBUG_INSN_P (insn));

  insn_queue[next_q] = link;
  q_size += 1;

  if (sched_verbose >= 2)
    {
      fprintf (sched_dump, ";;\t\tReady-->Q: insn %s: ",
	       (*current_sched_info->print_insn) (insn, 0));

      fprintf (sched_dump, "queued for %d cycles.\n", n_cycles);
    }

  QUEUE_INDEX (insn) = next_q;
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
  rtx *first = ready_lastpos (ready);
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

/* Clock at which the previous instruction was issued.  */
static int last_clock_var;

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
  int advance = 0;

  if (sched_verbose >= 1)
    {
      char buf[2048];

      print_insn (buf, insn, 0);
      buf[40] = 0;
      fprintf (sched_dump, ";;\t%3i--> %-40s:", clock_var, buf);

      if (recog_memoized (insn) < 0)
	fprintf (sched_dump, "nothing");
      else
	print_reservation (sched_dump, insn);
      fputc ('\n', sched_dump);
    }

  /* Scheduling instruction should have all its dependencies resolved and
     should have been removed from the ready list.  */
  gcc_assert (sd_lists_empty_p (insn, SD_LIST_BACK));

  gcc_assert (QUEUE_INDEX (insn) == QUEUE_NOWHERE);
  QUEUE_INDEX (insn) = QUEUE_SCHEDULED;

  gcc_assert (INSN_TICK (insn) >= MIN_TICK);
  if (INSN_TICK (insn) > clock_var)
    /* INSN has been prematurely moved from the queue to the ready list.
       This is possible only if following flag is set.  */
    gcc_assert (flag_sched_stalled_insns);    

  /* ??? Probably, if INSN is scheduled prematurely, we should leave
     INSN_TICK untouched.  This is a machine-dependent issue, actually.  */
  INSN_TICK (insn) = clock_var;

  /* Update dependent instructions.  */
  for (sd_it = sd_iterator_start (insn, SD_LIST_FORW);
       sd_iterator_cond (&sd_it, &dep);)
    {
      rtx next = DEP_CON (dep);

      /* Resolve the dependence between INSN and NEXT.
	 sd_resolve_dep () moves current dep to another list thus
	 advancing the iterator.  */
      sd_resolve_dep (sd_it);

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

  /* This is the place where scheduler doesn't *basically* need backward and
     forward dependencies for INSN anymore.  Nevertheless they are used in
     heuristics in rank_for_schedule (), early_queue_to_ready () and in
     some targets (e.g. rs6000).  Thus the earliest place where we *can*
     remove dependencies is after targetm.sched.md_finish () call in
     schedule_block ().  But, on the other side, the safest place to remove
     dependencies is when we are finishing scheduling entire region.  As we
     don't generate [many] dependencies during scheduling itself, we won't
     need memory until beginning of next region.
     Bottom line: Dependencies are removed for all insns in the end of
     scheduling the region.  */

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

/* Insert the INSN note at the end of the notes list.  */
static void 
add_to_note_list (rtx insn, rtx *note_list_end_p)
{
  PREV_INSN (insn) = *note_list_end_p;
  if (*note_list_end_p)
    NEXT_INSN (*note_list_end_p) = insn;
  *note_list_end_p = insn;
}

/* Add note list that ends on FROM_END to the end of TO_ENDP.  */
void
concat_note_lists (rtx from_end, rtx *to_endp)
{
  rtx from_start;

  if (from_end == NULL)
    /* It's easy when have nothing to concat.  */
    return;

  if (*to_endp == NULL)
    /* It's also easy when destination is empty.  */
    {
      *to_endp = from_end;
      return;
    }

  from_start = from_end;
  /* A note list should be traversed via PREV_INSN.  */
  while (PREV_INSN (from_start) != NULL) 
    from_start = PREV_INSN (from_start);

  add_to_note_list (from_start, to_endp);
  *to_endp = from_end;
}

/* Delete notes beginning with INSN and put them in the chain
   of notes ended by NOTE_LIST.
   Returns the insn following the notes.  */
static rtx
unlink_other_notes (rtx insn, rtx tail)
{
  rtx prev = PREV_INSN (insn);

  while (insn != tail && NOTE_NOT_BB_P (insn))
    {
      rtx next = NEXT_INSN (insn);
      basic_block bb = BLOCK_FOR_INSN (insn);

      /* Delete the note from its current position.  */
      if (prev)
	NEXT_INSN (prev) = next;
      if (next)
	PREV_INSN (next) = prev;

      if (bb)
        {
          /* Basic block can begin with either LABEL or
             NOTE_INSN_BASIC_BLOCK.  */
          gcc_assert (BB_HEAD (bb) != insn);

          /* Check if we are removing last insn in the BB.  */
          if (BB_END (bb) == insn)
            BB_END (bb) = prev;
        }

      /* See sched_analyze to see how these are handled.  */
      if (NOTE_KIND (insn) != NOTE_INSN_EH_REGION_BEG
	  && NOTE_KIND (insn) != NOTE_INSN_EH_REGION_END)
        add_to_note_list (insn, &note_list);

      insn = next;
    }

  if (insn == tail)
    {
      gcc_assert (sel_sched_p ());
      return prev;
    }

  return insn;
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
    if (NOTE_P (beg_head) || BOUNDARY_DEBUG_INSN_P (beg_head))
      beg_head = NEXT_INSN (beg_head);
    else
      break;

  *headp = beg_head;

  if (beg == end)
    end_head = beg_head;
  else if (LABEL_P (end_head))
    end_head = NEXT_INSN (end_head);

  while (end_head != end_tail)
    if (NOTE_P (end_tail) || BOUNDARY_DEBUG_INSN_P (end_tail))
      end_tail = PREV_INSN (end_tail);
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
      if (!NOTE_P (head) && !LABEL_P (head)
	  && !BOUNDARY_DEBUG_INSN_P (head))
	return 0;
      head = NEXT_INSN (head);
    }
  return 1;
}

/* Delete notes between HEAD and TAIL and put them in the chain
   of notes ended by NOTE_LIST.  */
static void
rm_other_notes (rtx head, rtx tail)
{
  rtx next_tail;
  rtx insn;

  note_list = 0;
  if (head == tail && (! INSN_P (head)))
    return;

  next_tail = NEXT_INSN (tail);
  for (insn = head; insn != next_tail; insn = NEXT_INSN (insn))
    {
      rtx prev;

      /* Farm out notes, and maybe save them in NOTE_LIST.
         This is needed to keep the debugger from
         getting completely deranged.  */
      if (NOTE_NOT_BB_P (insn))
	{
	  prev = insn;
	  insn = unlink_other_notes (insn, next_tail);

	  gcc_assert ((sel_sched_p ()
		       || prev != tail) && prev != head && insn != next_tail);
	}
    }
}

/* Same as above, but also process REG_SAVE_NOTEs of HEAD.  */
void
remove_notes (rtx head, rtx tail)
{
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
	  remove_note (head, note);
    }

  /* Remove remaining note insns from the block, save them in
     note_list.  These notes are restored at the end of
     schedule_block ().  */
  rm_other_notes (head, tail);
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

/* Functions for computation of registers live/usage info.  */

/* This function looks for a new register being defined.
   If the destination register is already used by the source,
   a new register is not needed.  */
static int
find_set_reg_weight (const_rtx x)
{
  if (GET_CODE (x) == CLOBBER
      && register_operand (SET_DEST (x), VOIDmode))
    return 1;
  if (GET_CODE (x) == SET
      && register_operand (SET_DEST (x), VOIDmode))
    {
      if (REG_P (SET_DEST (x)))
	{
	  if (!reg_mentioned_p (SET_DEST (x), SET_SRC (x)))
	    return 1;
	  else
	    return 0;
	}
      return 1;
    }
  return 0;
}

/* Calculate INSN_REG_WEIGHT for INSN.  */
static void
find_insn_reg_weight (const_rtx insn)
{
  int reg_weight = 0;
  rtx x;
  
  /* Handle register life information.  */
  if (! INSN_P (insn))
    return;
  
  /* Increment weight for each register born here.  */
  x = PATTERN (insn);
  reg_weight += find_set_reg_weight (x);
  if (GET_CODE (x) == PARALLEL)
    {
      int j;
      for (j = XVECLEN (x, 0) - 1; j >= 0; j--)
	{
	  x = XVECEXP (PATTERN (insn), 0, j);
	  reg_weight += find_set_reg_weight (x);
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
      /* If debug counter is activated do not requeue insn next after
	 last_scheduled_insn.  */
      skip_insn = next_nonnote_insn (last_scheduled_insn);
      while (skip_insn && DEBUG_INSN_P (skip_insn))
	skip_insn = next_nonnote_insn (skip_insn);
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
	  && ready->n_ready - ready->n_debug > MAX_SCHED_READY_INSNS
	  && !SCHED_GROUP_P (insn)
	  && insn != skip_insn)
	{
	  if (sched_verbose >= 2)
	    fprintf (sched_dump, "requeued because ready full\n");
	  queue_insn (insn, 1);
	}
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
  int n_cycles;
  rtx prev_insn = last_scheduled_insn;

  if (targetm.sched.is_costly_dependence)
    {
      for (n_cycles = flag_sched_stalled_insns_dep; n_cycles; n_cycles--)
	{
	  for ( ; prev_insn; prev_insn = PREV_INSN (prev_insn))
	    {
	      int cost;

	      if (prev_insn == current_sched_info->prev_head)
		{
		  prev_insn = NULL;
		  break;
		}

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

	  if (!prev_insn) 
	    break;
	  prev_insn = PREV_INSN (prev_insn);     
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
    fprintf (sched_dump, "  %s", (*current_sched_info->print_insn) (p[i], 0));
  fprintf (sched_dump, "\n");
}

/* Search INSN for REG_SAVE_NOTE note pairs for
   NOTE_INSN_EHREGION_{BEG,END}; and convert them back into
   NOTEs.  The REG_SAVE_NOTE note following first one is contains the
   saved value for NOTE_BLOCK_NUMBER which is useful for
   NOTE_INSN_EH_REGION_{BEG,END} NOTEs.  */
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
};

/* The following array is used to implement a stack of choices used in
   function max_issue.  */
static struct choice_entry *choice_stack;

/* The following variable value is number of essential insns issued on
   the current cycle.  An insn is essential one if it changes the
   processors state.  */
int cycle_issued_insns;

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
   insns are already issued in this try.  MAX_POINTS is the sum of points
   of all instructions in READY.  The function stops immediately,
   if it reached the such a solution, that all instruction can be issued.
   INDEX will contain index of the best insn in READY.  The following
   function is used only for first cycle multipass scheduling.

   PRIVILEGED_N >= 0

   This function expects recognized insns only.  All USEs,
   CLOBBERs, etc must be filtered elsewhere.  */
int
max_issue (struct ready_list *ready, int privileged_n, state_t state,
	   int *index)
{
  int n, i, all, n_ready, best, delay, tries_num, points = -1, max_points;
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
  max_points = 0;
  more_issue = issue_rate - cycle_issued_insns;

  /* ??? We used to assert here that we never issue more insns than issue_rate.
     However, some targets (e.g. MIPS/SB1) claim lower issue rate than can be
     achieved to get better performance.  Until these targets are fixed to use
     scheduler hooks to manipulate insns priority instead, the assert should 
     be disabled.  

     gcc_assert (more_issue >= 0);  */

  for (i = 0; i < n_ready; i++)
    if (!ready_try [i])
      {
	if (more_issue-- > 0)
	  max_points += ISSUE_POINTS (ready_element (ready, i));
	else
	  break;
      }

  /* The number of the issued insns in the best solution.  */
  best = 0;

  top = choice_stack;

  /* Set initial state of the search.  */
  memcpy (top->state, state, dfa_state_size);
  top->rest = dfa_lookahead;
  top->n = 0;

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
	  /* Or have nothing else to try.  */
	  || i >= n_ready)
	{
	  /* ??? (... || i == n_ready).  */
	  gcc_assert (i <= n_ready);

	  if (top == choice_stack)
	    break;

	  if (best < top - choice_stack)
	    {
	      if (privileged_n)
		{
		  n = privileged_n;
		  /* Try to find issued privileged insn.  */
		  while (n && !ready_try[--n]);
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
		  points = top->n;
		  if (top->n == max_points || best == all)
		    break;
		}
	    }

	  /* Set ready-list index to point to the last insn
	     ('i++' below will advance it to the next insn).  */
	  i = top->index;

	  /* Backtrack.  */
	  ready_try [i] = 0;
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
		n += ISSUE_POINTS (insn);

	      /* Advance to the next choice_entry.  */
	      top++;
	      /* Initialize it.  */
	      top->rest = dfa_lookahead;
	      top->index = i;
	      top->n = n;
	      memcpy (top->state, state, dfa_state_size);

	      ready_try [i] = 1;
	      i = -1;
	    }
	}

      /* Increase ready-list index.  */
      i++;
    }

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
choose_ready (struct ready_list *ready, rtx *insn_ptr)
{
  int lookahead;

  if (dbg_cnt (sched_insn) == false)
    {
      rtx insn;

      insn = next_nonnote_insn (last_scheduled_insn);

      if (QUEUE_INDEX (insn) == QUEUE_READY)
	/* INSN is in the ready_list.  */
	{
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

#ifdef ENABLE_CHECKING
	    /* If this insn is recognizable we should have already
	       recognized it earlier.
	       ??? Not very clear where this is supposed to be done.
	       See dep_cost_1.  */
	    gcc_assert (INSN_CODE (insn) >= 0
			|| recog_memoized (insn) < 0);
#endif

	    ready_try [i]
	      = (/* INSN_CODE check can be omitted here as it is also done later
		    in max_issue ().  */
		 INSN_CODE (insn) < 0
		 || (targetm.sched.first_cycle_multipass_dfa_lookahead_guard
		     && !targetm.sched.first_cycle_multipass_dfa_lookahead_guard
		     (insn)));
	  }

      if (max_issue (ready, 1, curr_state, &index) == 0)
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

/* Use forward list scheduling to rearrange insns of block pointed to by
   TARGET_BB, possibly bringing insns from subsequent blocks in the same
   region.  */

void
schedule_block (basic_block *target_bb)
{
  int i, first_cycle_insn_p;
  int can_issue_more;
  state_t temp_state = NULL;  /* It is used for multipass scheduling.  */
  int sort_p, advance, start_clock_var;

  /* Head/tail info for this block.  */
  rtx prev_head = current_sched_info->prev_head;
  rtx next_tail = current_sched_info->next_tail;
  rtx head = NEXT_INSN (prev_head);
  rtx tail = PREV_INSN (next_tail);

  /* We used to have code to avoid getting parameters moved from hard
     argument registers into pseudos.

     However, it was removed when it proved to be of marginal benefit
     and caused problems because schedule_block and compute_forward_dependences
     had different notions of what the "head" insn was.  */

  gcc_assert (head != tail || INSN_P (head));

  haifa_recovery_bb_recently_added_p = false;

  /* Debug info.  */
  if (sched_verbose)
    dump_new_block_header (0, *target_bb, head, tail);

  state_reset (curr_state);

  /* Clear the ready list.  */
  ready.first = ready.veclen - 1;
  ready.n_ready = 0;
  ready.n_debug = 0;

  /* It is used for first cycle multipass scheduling.  */
  temp_state = alloca (dfa_state_size);

  if (targetm.sched.md_init)
    targetm.sched.md_init (sched_dump, sched_verbose, ready.veclen);

  /* We start inserting insns after PREV_HEAD.  */
  last_scheduled_insn = prev_head;

  gcc_assert ((NOTE_P (last_scheduled_insn)
	       || BOUNDARY_DEBUG_INSN_P (last_scheduled_insn))
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
	 last_scheduled_insn.  */
      {
	rtx skip_insn;

	if (dbg_cnt (sched_insn) == false)
	  skip_insn = next_nonnote_insn (last_scheduled_insn);
	else
	  skip_insn = NULL_RTX;

	while (i < ready.n_ready)
	  {
	    rtx insn;

	    insn = ready_remove (&ready, i);

	    if (insn != skip_insn)
	      queue_insn (insn, 1);
	  }
      }
    }

  /* Now we can restore basic block notes and maintain precise cfg.  */
  restore_bb_notes (*target_bb);

  last_clock_var = -1;

  advance = 0;

  sort_p = TRUE;
  /* Loop until all the insns in BB are scheduled.  */
  while ((*current_sched_info->schedule_more_p) ())
    {
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

      if (sort_p)
	{
	  /* Sort the ready list based on priority.  */
	  ready_sort (&ready);

	  if (sched_verbose >= 2)
	    {
	      fprintf (sched_dump, ";;\t\tReady list after ready_sort:  ");
	      debug_ready_list (&ready);
	    }
	}

      /* We don't want md sched reorder to even see debug isns, so put
	 them out right away.  */
      if (ready.n_ready && DEBUG_INSN_P (ready_element (&ready, 0)))
	{
	  if (control_flow_insn_p (last_scheduled_insn))
	    {
	      *target_bb = current_sched_info->advance_target_bb
		(*target_bb, 0);

	      if (sched_verbose)
		{
		  rtx x;

		  x = next_real_insn (last_scheduled_insn);
		  gcc_assert (x);
		  dump_new_block_header (1, *target_bb, x, tail);
		}

	      last_scheduled_insn = bb_note (*target_bb);
	    }

	  while (ready.n_ready && DEBUG_INSN_P (ready_element (&ready, 0)))
	    {
	      rtx insn = ready_remove_first (&ready);
	      gcc_assert (DEBUG_INSN_P (insn));
	      (*current_sched_info->begin_schedule_ready) (insn,
							   last_scheduled_insn);
	      move_insn (insn, last_scheduled_insn,
			 current_sched_info->next_tail);
	      last_scheduled_insn = insn;
	      advance = schedule_insn (insn);
	      gcc_assert (advance == 0);
	      if (ready.n_ready > 0)
		ready_sort (&ready);
	    }

	  if (!ready.n_ready)
	    continue;
	}

      /* Allow the target to reorder the list, typically for
	 better instruction bundling.  */
      if (sort_p && targetm.sched.reorder
	  && (ready.n_ready == 0
	      || !SCHED_GROUP_P (ready_element (&ready, 0))))
	can_issue_more =
	  targetm.sched.reorder (sched_dump, sched_verbose,
				 ready_lastpos (&ready),
				 &ready.n_ready, clock_var);
      else
	can_issue_more = issue_rate;

      first_cycle_insn_p = 1;
      cycle_issued_insns = 0;
      for (;;)
	{
	  rtx insn;
	  int cost;
	  bool asm_p = false;

	  if (sched_verbose >= 2)
	    {
	      fprintf (sched_dump, ";;\tReady list (t = %3d):  ",
		       clock_var);
	      debug_ready_list (&ready);
	    }

	  if (ready.n_ready == 0 
	      && can_issue_more 
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
	      || !can_issue_more
	      || state_dead_lock_p (curr_state)
	      || !(*current_sched_info->schedule_more_p) ())
	    break;

	  /* Select and remove the insn from the ready list.  */
	  if (sort_p)
	    {
	      int res;

	      insn = NULL_RTX;
	      res = choose_ready (&ready, &insn);

	      if (res < 0)
		/* Finish cycle.  */
		break;
	      if (res > 0)
		/* Restart choose_ready ().  */
		continue;

	      gcc_assert (insn != NULL_RTX);
	    }
	  else
	    insn = ready_remove_first (&ready);

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
	  memcpy (temp_state, curr_state, dfa_state_size);
	  if (recog_memoized (insn) < 0)
	    {
	      asm_p = (GET_CODE (PATTERN (insn)) == ASM_INPUT
		       || asm_noperands (PATTERN (insn)) >= 0);
	      if (!first_cycle_insn_p && asm_p)
		/* This is asm insn which is tried to be issued on the
		   cycle not first.  Issue it on the next cycle.  */
		cost = 1;
	      else
		/* A USE insn, or something else we don't need to
		   understand.  We can't pass these directly to
		   state_transition because it will trigger a
		   fatal error for unrecognizable insns.  */
		cost = 0;
	    }
	  else
	    {
	      cost = state_transition (temp_state, insn);
	      if (cost < 0)
		cost = 0;
	      else if (cost == 0)
		cost = 1;
	    }

	  if (cost >= 1)
	    {
	      queue_insn (insn, cost);
 	      if (SCHED_GROUP_P (insn))
 		{
 		  advance = cost;
 		  break;
 		}
 
	      continue;
	    }

	  if (current_sched_info->can_schedule_ready_p
	      && ! (*current_sched_info->can_schedule_ready_p) (insn))
	    /* We normally get here only if we don't want to move
	       insn from the split block.  */
	    {
	      TODO_SPEC (insn) = (TODO_SPEC (insn) & ~SPECULATIVE) | HARD_DEP;
	      continue;
	    }

	  /* DECISION is made.  */	
  
          if (TODO_SPEC (insn) & SPECULATIVE)
            generate_recovery_code (insn);

	  if (control_flow_insn_p (last_scheduled_insn)	     
	      /* This is used to switch basic blocks by request
		 from scheduler front-end (actually, sched-ebb.c only).
		 This is used to process blocks with single fallthru
		 edge.  If succeeding block has jump, it [jump] will try
		 move at the end of current bb, thus corrupting CFG.  */
	      || current_sched_info->advance_target_bb (*target_bb, insn))
	    {
	      *target_bb = current_sched_info->advance_target_bb
		(*target_bb, 0);
	      
	      if (sched_verbose)
		{
		  rtx x;

		  x = next_real_insn (last_scheduled_insn);
		  gcc_assert (x);
		  dump_new_block_header (1, *target_bb, x, tail);
		}

	      last_scheduled_insn = bb_note (*target_bb);
	    }
 
	  /* Update counters, etc in the scheduler's front end.  */
	  (*current_sched_info->begin_schedule_ready) (insn,
						       last_scheduled_insn);
 
	  move_insn (insn, last_scheduled_insn, current_sched_info->next_tail);
	  reemit_notes (insn);
	  last_scheduled_insn = insn;
	  
	  if (memcmp (curr_state, temp_state, dfa_state_size) != 0)
            {
              cycle_issued_insns++;
              memcpy (curr_state, temp_state, dfa_state_size);
            }

	  if (targetm.sched.variable_issue)
	    can_issue_more =
	      targetm.sched.variable_issue (sched_dump, sched_verbose,
					    insn, can_issue_more);
	  /* A naked CLOBBER or USE generates no instruction, so do
	     not count them against the issue rate.  */
	  else if (GET_CODE (PATTERN (insn)) != USE
		   && GET_CODE (PATTERN (insn)) != CLOBBER)
	    can_issue_more--;

	  advance = schedule_insn (insn);

	  /* After issuing an asm insn we should start a new cycle.  */
	  if (advance == 0 && asm_p)
	    advance = 1;
	  if (advance != 0)
	    break;

	  first_cycle_insn_p = 0;

	  /* Sort the ready list based on priority.  This must be
	     redone here, as schedule_insn may have readied additional
	     insns that will not be sorted correctly.  */
	  if (ready.n_ready > 0)
	    ready_sort (&ready);

	  /* Quickly go through debug insns such that md sched
	     reorder2 doesn't have to deal with debug insns.  */
	  if (ready.n_ready && DEBUG_INSN_P (ready_element (&ready, 0))
	      && (*current_sched_info->schedule_more_p) ())
	    {
	      if (control_flow_insn_p (last_scheduled_insn))
		{
		  *target_bb = current_sched_info->advance_target_bb
		    (*target_bb, 0);

		  if (sched_verbose)
		    {
		      rtx x;

		      x = next_real_insn (last_scheduled_insn);
		      gcc_assert (x);
		      dump_new_block_header (1, *target_bb, x, tail);
		    }

		  last_scheduled_insn = bb_note (*target_bb);
		}

 	      while (ready.n_ready && DEBUG_INSN_P (ready_element (&ready, 0)))
		{
		  insn = ready_remove_first (&ready);
		  gcc_assert (DEBUG_INSN_P (insn));
		  (*current_sched_info->begin_schedule_ready)
		    (insn, last_scheduled_insn);
		  move_insn (insn, last_scheduled_insn,
			     current_sched_info->next_tail);
		  advance = schedule_insn (insn);
		  last_scheduled_insn = insn;
		  gcc_assert (advance == 0);
		  if (ready.n_ready > 0)
		    ready_sort (&ready);
		}
	    }

	  if (targetm.sched.reorder2
	      && (ready.n_ready == 0
		  || !SCHED_GROUP_P (ready_element (&ready, 0))))
	    {
	      can_issue_more =
		targetm.sched.reorder2 (sched_dump, sched_verbose,
					ready.n_ready
					? ready_lastpos (&ready) : NULL,
					&ready.n_ready, clock_var);
	    }
	}
    }

  /* Debug info.  */
  if (sched_verbose)
    {
      fprintf (sched_dump, ";;\tReady list (final):  ");
      debug_ready_list (&ready);
    }

  if (current_sched_info->queue_must_finish_empty)
    /* Sanity check -- queue must be empty now.  Meaningless if region has
       multiple bbs.  */
    gcc_assert (!q_size && !ready.n_ready && !ready.n_debug);
  else 
    {
      /* We must maintain QUEUE_INDEX between blocks in region.  */
      for (i = ready.n_ready - 1; i >= 0; i--)
	{
	  rtx x;
	  
	  x = ready_element (&ready, i);
	  QUEUE_INDEX (x) = QUEUE_NOWHERE;
	  TODO_SPEC (x) = (TODO_SPEC (x) & ~SPECULATIVE) | HARD_DEP;
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
		TODO_SPEC (x) = (TODO_SPEC (x) & ~SPECULATIVE) | HARD_DEP;
	      }
	    free_INSN_LIST_list (&insn_queue[i]);
	  }
    }

  if (sched_verbose)
    fprintf (sched_dump, ";;   total time = %d\n", clock_var);

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

  if (targetm.sched.md_finish)
    {
      targetm.sched.md_finish (sched_dump, sched_verbose);
      /* Target might have added some instructions to the scheduled block
	 in its md_finish () hook.  These new insns don't have any data
	 initialized and to identify them we extend h_i_d so that they'll
	 get zero luids.  */
      sched_init_luids (NULL, NULL, NULL, NULL);
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

  if (head == tail && (! INSN_P (head) || BOUNDARY_DEBUG_INSN_P (head)))
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

  if (targetm.sched.md_init_global)
    targetm.sched.md_init_global (sched_dump, sched_verbose,
				  get_max_uid () + 1);

  curr_state = xmalloc (dfa_state_size);
}

static void haifa_init_only_bb (basic_block, basic_block);

/* Initialize data structures specific to the Haifa scheduler.  */
void
haifa_sched_init (void)
{
  setup_sched_dump ();
  sched_init ();

  if (spec_info != NULL)
    {
      sched_deps_info->use_deps_list = 1;
      sched_deps_info->generate_spec_deps = 1;
    }

  /* Initialize luids, dependency caches, target and h_i_d for the
     whole function.  */
  {
    bb_vec_t bbs = VEC_alloc (basic_block, heap, n_basic_blocks);
    basic_block bb;

    sched_init_bbs ();

    FOR_EACH_BB (bb)
      VEC_quick_push (basic_block, bbs, bb);
    sched_init_luids (bbs, NULL, NULL, NULL);
    sched_deps_init (true);
    sched_extend_target ();
    haifa_init_h_i_d (bbs, NULL, NULL, NULL);

    VEC_free (basic_block, heap, bbs);
  }

  sched_init_only_bb = haifa_init_only_bb;
  sched_split_block = sched_split_block_1;
  sched_create_empty_bb = sched_create_empty_bb_1;
  haifa_recovery_bb_ever_added_p = false;

#ifdef ENABLE_CHECKING
  /* This is used preferably for finding bugs in check_cfg () itself.
     We must call sched_bbs_init () before check_cfg () because check_cfg ()
     assumes that the last insn in the last bb has a non-null successor.  */
  check_cfg (0, 0);
#endif

  nr_begin_data = nr_begin_control = nr_be_in_data = nr_be_in_control = 0;
  before_recovery = 0;
  after_recovery = 0;
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
  free (curr_state);

  if (targetm.sched.md_finish_global)
    targetm.sched.md_finish_global (sched_dump, sched_verbose);

  end_alias_analysis ();

  regstat_free_calls_crossed ();

  dfa_finish ();

#ifdef ENABLE_CHECKING
  /* After reload ia64 backend clobbers CFG, so can't check anything.  */
  if (!reload_completed)
    check_cfg (0, 0);
#endif
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
	  if (!bitmap_bit_p (&processed, INSN_LUID (head)))
	    {
	      bitmap_set_bit (&processed, INSN_LUID (head));
	      tick -= next_clock;
	      
	      if (tick < MIN_TICK)
		tick = MIN_TICK;
	      
	      INSN_TICK (head) = tick;		 
	    }
	  
	  FOR_EACH_DEP (head, SD_LIST_RES_FORW, sd_it, dep)
	    {
	      rtx next;
	      
	      next = DEP_CON (dep);
	      tick = INSN_TICK (next);

	      if (tick != INVALID_TICK
		  /* If NEXT has its INSN_TICK calculated, fix it.
		     If not - it will be properly calculated from
		     scratch later in fix_tick_ready.  */
		  && !bitmap_bit_p (&processed, INSN_LUID (next)))
		{
		  bitmap_set_bit (&processed, INSN_LUID (next));
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

static int haifa_speculate_insn (rtx, ds_t, rtx *);
  
/* Check if NEXT is ready to be added to the ready or queue list.
   If "yes", add it to the proper list.
   Returns:
      -1 - is not ready yet,
       0 - added to the ready list,
   0 < N - queued for N cycles.  */
int
try_ready (rtx next)
{  
  ds_t old_ts, *ts;

  ts = &TODO_SPEC (next);
  old_ts = *ts;

  gcc_assert (!(old_ts & ~(SPECULATIVE | HARD_DEP))
	      && ((old_ts & HARD_DEP)
		  || (old_ts & SPECULATIVE)));
  
  if (sd_lists_empty_p (next, SD_LIST_BACK))
    /* NEXT has all its dependencies resolved.  */
    {
      /* Remove HARD_DEP bit from NEXT's status.  */
      *ts &= ~HARD_DEP;

      if (current_sched_info->flags & DO_SPECULATION)
	/* Remove all speculative bits from NEXT's status.  */
	*ts &= ~SPECULATIVE;
    }
  else
    {
      /* One of the NEXT's dependencies has been resolved.
	 Recalculate NEXT's status.  */

      *ts &= ~SPECULATIVE & ~HARD_DEP;

      if (sd_lists_empty_p (next, SD_LIST_HARD_BACK))
	/* Now we've got NEXT with speculative deps only.
	   1. Look at the deps to see what we have to do.
	   2. Check if we can do 'todo'.  */
	{
	  sd_iterator_def sd_it;
	  dep_t dep;
	  bool first_p = true;

	  FOR_EACH_DEP (next, SD_LIST_BACK, sd_it, dep)
	    {
	      ds_t ds = DEP_STATUS (dep) & SPECULATIVE;

	      if (first_p)
		{
		  first_p = false;

		  *ts = ds;
		}
	      else
		*ts = ds_merge (*ts, ds);
	    }

	  if (ds_weak (*ts) < spec_info->data_weakness_cutoff)
	    /* Too few points.  */
	    *ts = (*ts & ~SPECULATIVE) | HARD_DEP;
	}
      else
	*ts |= HARD_DEP;
    }

  if (*ts & HARD_DEP)
    gcc_assert (*ts == old_ts
		&& QUEUE_INDEX (next) == QUEUE_NOWHERE);
  else if (current_sched_info->new_ready)
    *ts = current_sched_info->new_ready (next, *ts);

  /* * if !(old_ts & SPECULATIVE) (e.g. HARD_DEP or 0), then insn might
     have its original pattern or changed (speculative) one.  This is due
     to changing ebb in region scheduling.
     * But if (old_ts & SPECULATIVE), then we are pretty sure that insn
     has speculative pattern.

     We can't assert (!(*ts & HARD_DEP) || *ts == old_ts) here because
     control-speculative NEXT could have been discarded by sched-rgn.c
     (the same case as when discarded by can_schedule_ready_p ()).  */

  if ((*ts & SPECULATIVE)
      /* If (old_ts == *ts), then (old_ts & SPECULATIVE) and we don't
	 need to change anything.  */
      && *ts != old_ts)
    {
      int res;
      rtx new_pat;
      
      gcc_assert ((*ts & SPECULATIVE) && !(*ts & ~SPECULATIVE));
      
      res = haifa_speculate_insn (next, *ts, &new_pat);
	
      switch (res)
	{
	case -1:
	  /* It would be nice to change DEP_STATUS of all dependences,
	     which have ((DEP_STATUS & SPECULATIVE) == *ts) to HARD_DEP,
	     so we won't reanalyze anything.  */
	  *ts = (*ts & ~SPECULATIVE) | HARD_DEP;
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
	  
	  haifa_change_pattern (next, new_pat);
	  break;
	  
	default:
	  gcc_unreachable ();
	}
    }
  
  /* We need to restore pattern only if (*ts == 0), because otherwise it is
     either correct (*ts & SPECULATIVE),
     or we simply don't care (*ts & HARD_DEP).  */
  
  gcc_assert (!ORIG_PAT (next)
	      || !IS_SPECULATION_BRANCHY_CHECK_P (next));
  
  if (*ts & HARD_DEP)
    {
      /* We can't assert (QUEUE_INDEX (next) == QUEUE_NOWHERE) here because
	 control-speculative NEXT could have been discarded by sched-rgn.c
	 (the same case as when discarded by can_schedule_ready_p ()).  */
      /*gcc_assert (QUEUE_INDEX (next) == QUEUE_NOWHERE);*/
      
      change_queue_index (next, QUEUE_NOWHERE);
      return -1;
    }
  else if (!(*ts & BEGIN_SPEC) && ORIG_PAT (next) && !IS_SPECULATION_CHECK_P (next))
    /* We should change pattern of every previously speculative 
       instruction - and we determine if NEXT was speculative by using
       ORIG_PAT field.  Except one case - speculation checks have ORIG_PAT
       pat too, so skip them.  */
    {
      haifa_change_pattern (next, ORIG_PAT (next));
      ORIG_PAT (next) = 0;
    }

  if (sched_verbose >= 2)
    {	      
      int s = TODO_SPEC (next);
          
      fprintf (sched_dump, ";;\t\tdependencies resolved: insn %s",
               (*current_sched_info->print_insn) (next, 0));
          
      if (spec_info && spec_info->dump)
        {
          if (s & BEGIN_DATA)
            fprintf (spec_info->dump, "; data-spec;");
          if (s & BEGIN_CONTROL)
            fprintf (spec_info->dump, "; control-spec;");
          if (s & BE_IN_CONTROL)
            fprintf (spec_info->dump, "; in-control-spec;");
        }

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

  if (!sd_lists_empty_p (next, SD_LIST_RES_BACK))
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
  if (delay <= 0)
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
    queue_insn (next, delay);
    
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
    choice_stack[i].state = xmalloc (dfa_state_size);

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
    free (choice_stack [i].state);
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

  priorities_roots = NULL;
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
  VEC_free (rtx, heap, priorities_roots);
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
find_fallthru_edge (basic_block pred)
{
  edge e;
  edge_iterator ei;
  basic_block succ;

  succ = pred->next_bb;
  gcc_assert (succ->prev_bb == pred);

  if (EDGE_COUNT (pred->succs) <= EDGE_COUNT (succ->preds))
    {
      FOR_EACH_EDGE (e, ei, pred->succs)
	if (e->flags & EDGE_FALLTHRU)
	  {
	    gcc_assert (e->dest == succ);
	    return e;
	  }
    }
  else
    {
      FOR_EACH_EDGE (e, ei, succ->preds)
	if (e->flags & EDGE_FALLTHRU)
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
  rtx insn;

  /* The following is done to keep current_sched_info->next_tail non null.  */
  insn = BB_END (EXIT_BLOCK_PTR->prev_bb);
  if (NEXT_INSN (insn) == 0
      || (!NOTE_P (insn)
	  && !LABEL_P (insn)
	  /* Don't emit a NOTE if it would end up before a BARRIER.  */
	  && !BARRIER_P (NEXT_INSN (insn))))
    {
      rtx note = emit_note_after (NOTE_INSN_DELETED, insn);
      /* Make insn appear outside BB.  */
      set_block_for_insn (note, NULL);
      BB_END (EXIT_BLOCK_PTR->prev_bb) = insn;
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
  e = find_fallthru_edge (last);

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
	  add_bb_to_loop (single, VEC_index (loop_p, current_loops->larray, 0));
	  add_bb_to_loop (empty, VEC_index (loop_p, current_loops->larray, 0));
	}

      single->count = last->count;
      empty->count = last->count;
      single->frequency = last->frequency;
      empty->frequency = last->frequency;
      BB_COPY_PARTITION (single, last);
      BB_COPY_PARTITION (empty, last);

      redirect_edge_succ (e, single);
      make_single_succ_edge (single, empty, 0);
      make_single_succ_edge (empty, EXIT_BLOCK_PTR,
			     EDGE_FALLTHRU | EDGE_CAN_FALLTHRU);

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
  edge e;
  int edge_flags;

  /* This is fixing of incoming edge.  */
  /* ??? Which other flags should be specified?  */      
  if (BB_PARTITION (first_bb) != BB_PARTITION (rec))
    /* Partition type is the same, if it is "unpartitioned".  */
    edge_flags = EDGE_CROSSING;
  else
    edge_flags = 0;
      
  e = make_edge (first_bb, rec, edge_flags);
  label = block_label (second_bb);
  jump = emit_jump_insn_after (gen_jump (label), BB_END (rec));
  JUMP_LABEL (jump) = label;
  LABEL_NUSES (label)++;

  if (BB_PARTITION (second_bb) != BB_PARTITION (rec))
    /* Partition type is the same, if it is "unpartitioned".  */
    {
      /* Rewritten from cfgrtl.c.  */
      if (flag_reorder_blocks_and_partition
	  && targetm.have_named_sections)
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
      rtx_vec_t priorities_roots = NULL;

      clear_priorities (twin, &priorities_roots);
      calc_priorities (priorities_roots);
      VEC_free (rtx, heap, priorities_roots);
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

	      if (!bitmap_bit_p (&in_ready, INSN_LUID (consumer)))
		{
		  ready_list = alloc_INSN_LIST (consumer, ready_list);
		  bitmap_set_bit (&in_ready, INSN_LUID (consumer));
		}
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

/* Change pattern of INSN to NEW_PAT.  */
void
sched_change_pattern (rtx insn, rtx new_pat)
{
  int t;

  t = validate_change (insn, &PATTERN (insn), new_pat, 0);
  gcc_assert (t);
  dfa_clear_single_insn_cache (insn);
}

/* Change pattern of INSN to NEW_PAT.  Invalidate cached haifa
   instruction data.  */
static void
haifa_change_pattern (rtx insn, rtx new_pat)
{
  sched_change_pattern (insn, new_pat);

  /* Invalidate INSN_COST, so it'll be recalculated.  */
  INSN_COST (insn) = -1;
  /* Invalidate INSN_TICK, so it'll be recalculated.  */
  INSN_TICK (insn) = INVALID_TICK;
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
  VEC(edge,gc) *t;

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
move_succs (VEC(edge,gc) **succsp, basic_block to)
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
  remove_insn (insn);
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
    VEC_safe_push (rtx, heap, *roots_ptr, insn);
}

/* Recompute priorities of instructions, whose priorities might have been
   changed.  ROOTS is a vector of instructions whose priority computation will
   trigger initialization of all cleared priorities.  */
static void
calc_priorities (rtx_vec_t roots)
{
  int i;
  rtx insn;

  for (i = 0; VEC_iterate (rtx, roots, i, insn); i++)
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
      
      if (dep_list_size (insn) == 0)
	{
	  dep_def _new_dep, *new_dep = &_new_dep;

	  init_dep (new_dep, insn, jump, REG_DEP_ANTI);
	  sd_add_dep (new_dep, false);
	}
    }
  while (1);

  gcc_assert (!sd_lists_empty_p (jump, SD_LIST_BACK));
}

/* Return the NOTE_INSN_BASIC_BLOCK of BB.  */
rtx
bb_note (basic_block bb)
{
  rtx note;

  note = BB_HEAD (bb);
  if (LABEL_P (note))
    note = NEXT_INSN (note);

  gcc_assert (NOTE_INSN_BASIC_BLOCK_P (note));
  return note;
}

#ifdef ENABLE_CHECKING
/* Helper function for check_cfg.
   Return nonzero, if edge vector pointed to by EL has edge with TYPE in
   its flags.  */
static int
has_edge_p (VEC(edge,gc) *el, int type)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, el)
    if (e->flags & type)
      return 1;
  return 0;
}

/* Search back, starting at INSN, for an insn that is not a
   NOTE_INSN_VAR_LOCATION.  Don't search beyond HEAD, and return it if
   no such insn can be found.  */
static inline rtx
prev_non_location_insn (rtx insn, rtx head)
{
  while (insn != head && NOTE_P (insn)
	 && NOTE_KIND (insn) == NOTE_INSN_VAR_LOCATION)
    insn = PREV_INSN (insn);

  return insn;
}

/* Check few properties of CFG between HEAD and TAIL.
   If HEAD (TAIL) is NULL check from the beginning (till the end) of the
   instruction stream.  */
static void
check_cfg (rtx head, rtx tail)
{
  rtx next_tail;
  basic_block bb = 0;
  int not_first = 0, not_last;

  if (head == NULL)
    head = get_insns ();
  if (tail == NULL)
    tail = get_last_insn ();
  next_tail = NEXT_INSN (tail);

  do
    {      
      not_last = head != tail;        

      if (not_first)
	gcc_assert (NEXT_INSN (PREV_INSN (head)) == head);
      if (not_last)
	gcc_assert (PREV_INSN (NEXT_INSN (head)) == head);

      if (LABEL_P (head) 
	  || (NOTE_INSN_BASIC_BLOCK_P (head)
	      && (!not_first
		  || (not_first && !LABEL_P (PREV_INSN (head))))))
	{
	  gcc_assert (bb == 0);	  
	  bb = BLOCK_FOR_INSN (head);
	  if (bb != 0)
	    gcc_assert (BB_HEAD (bb) == head);      
	  else
	    /* This is the case of jump table.  See inside_basic_block_p ().  */
	    gcc_assert (LABEL_P (head) && !inside_basic_block_p (head));
	}

      if (bb == 0)
	{
	  gcc_assert (!inside_basic_block_p (head));
	  head = NEXT_INSN (head);
	}
      else
	{
	  gcc_assert (inside_basic_block_p (head)
		      || NOTE_P (head));
	  gcc_assert (BLOCK_FOR_INSN (head) == bb);
	
	  if (LABEL_P (head))
	    {
	      head = NEXT_INSN (head);
	      gcc_assert (NOTE_INSN_BASIC_BLOCK_P (head));
	    }
	  else
	    {
	      if (control_flow_insn_p (head))
		{
		  gcc_assert (prev_non_location_insn (BB_END (bb), head)
			      == head);

		  if (any_uncondjump_p (head))
		    gcc_assert (EDGE_COUNT (bb->succs) == 1
				&& BARRIER_P (NEXT_INSN (head)));
		  else if (any_condjump_p (head))
		    gcc_assert (/* Usual case.  */
                                (EDGE_COUNT (bb->succs) > 1
                                 && !BARRIER_P (NEXT_INSN (head)))
                                /* Or jump to the next instruction.  */
                                || (EDGE_COUNT (bb->succs) == 1
                                    && (BB_HEAD (EDGE_I (bb->succs, 0)->dest)
                                        == JUMP_LABEL (head))));
		}
	      if (BB_END (bb) == head)
		{
		  if (EDGE_COUNT (bb->succs) > 1)
		    gcc_assert (control_flow_insn_p (prev_non_location_insn
						     (head, BB_HEAD (bb)))
				|| has_edge_p (bb->succs, EDGE_COMPLEX));
		  bb = 0;
		}

	      head = NEXT_INSN (head);
	    }
	}

      not_first = 1;
    }
  while (head != next_tail);

  gcc_assert (bb == 0);
}

#endif /* ENABLE_CHECKING */

/* Extend per basic block data structures.  */
static void
extend_bb (void)
{
  if (sched_scan_info->extend_bb)
    sched_scan_info->extend_bb ();
}

/* Init data for BB.  */
static void
init_bb (basic_block bb)
{
  if (sched_scan_info->init_bb)
    sched_scan_info->init_bb (bb);
}

/* Extend per insn data structures.  */
static void
extend_insn (void)
{
  if (sched_scan_info->extend_insn)
    sched_scan_info->extend_insn ();
}

/* Init data structures for INSN.  */
static void
init_insn (rtx insn)
{
  if (sched_scan_info->init_insn)
    sched_scan_info->init_insn (insn);
}

/* Init all insns in BB.  */
static void
init_insns_in_bb (basic_block bb)
{
  rtx insn;

  FOR_BB_INSNS (bb, insn)
    init_insn (insn);
}

/* A driver function to add a set of basic blocks (BBS),
   a single basic block (BB), a set of insns (INSNS) or a single insn (INSN)
   to the scheduling region.  */
void
sched_scan (const struct sched_scan_info_def *ssi,
	    bb_vec_t bbs, basic_block bb, insn_vec_t insns, rtx insn)
{
  sched_scan_info = ssi;

  if (bbs != NULL || bb != NULL)
    {
      extend_bb ();

      if (bbs != NULL)
	{
	  unsigned i;
	  basic_block x;

	  for (i = 0; VEC_iterate (basic_block, bbs, i, x); i++)
	    init_bb (x);
	}

      if (bb != NULL)
	init_bb (bb);
    }

  extend_insn ();

  if (bbs != NULL)
    {      
      unsigned i;
      basic_block x;

      for (i = 0; VEC_iterate (basic_block, bbs, i, x); i++)
	init_insns_in_bb (x);
    }

  if (bb != NULL)
    init_insns_in_bb (bb);

  if (insns != NULL)
    {
      unsigned i;
      rtx x;

      for (i = 0; VEC_iterate (rtx, insns, i, x); i++)
	init_insn (x);
    }

  if (insn != NULL)
    init_insn (insn);
}


/* Extend data structures for logical insn UID.  */
static void
luids_extend_insn (void)
{
  int new_luids_max_uid = get_max_uid () + 1;

  VEC_safe_grow_cleared (int, heap, sched_luids, new_luids_max_uid);
}

/* Initialize LUID for INSN.  */
static void
luids_init_insn (rtx insn)
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

/* Initialize luids for BBS, BB, INSNS and INSN.
   The hook common_sched_info->luid_for_non_insn () is used to determine
   if notes, labels, etc. need luids.  */
void
sched_init_luids (bb_vec_t bbs, basic_block bb, insn_vec_t insns, rtx insn)
{
  const struct sched_scan_info_def ssi =
    {
      NULL, /* extend_bb */
      NULL, /* init_bb */
      luids_extend_insn, /* extend_insn */
      luids_init_insn /* init_insn */
    };

  sched_scan (&ssi, bbs, bb, insns, insn);
}

/* Free LUIDs.  */
void
sched_finish_luids (void)
{
  VEC_free (int, heap, sched_luids);
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
  int reserve = (get_max_uid () + 1 
                 - VEC_length (haifa_insn_data_def, h_i_d));
  if (reserve > 0 
      && ! VEC_space (haifa_insn_data_def, h_i_d, reserve))
    {
      VEC_safe_grow_cleared (haifa_insn_data_def, heap, h_i_d, 
                             3 * get_max_uid () / 2);
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
      find_insn_reg_weight (insn);
      QUEUE_INDEX (insn) = QUEUE_NOWHERE;
      INSN_TICK (insn) = INVALID_TICK;
      INTER_TICK (insn) = INVALID_TICK;
      TODO_SPEC (insn) = HARD_DEP;
    }
}

/* Initialize haifa_insn_data for BBS, BB, INSNS and INSN.  */
void
haifa_init_h_i_d (bb_vec_t bbs, basic_block bb, insn_vec_t insns, rtx insn)
{
  const struct sched_scan_info_def ssi =
    {
      NULL, /* extend_bb */
      NULL, /* init_bb */
      extend_h_i_d, /* extend_insn */
      init_h_i_d /* init_insn */
    };

  sched_scan (&ssi, bbs, bb, insns, insn);
}

/* Finalize haifa_insn_data.  */
void
haifa_finish_h_i_d (void)
{
  VEC_free (haifa_insn_data_def, heap, h_i_d);
}

/* Init data for the new insn INSN.  */
static void
haifa_init_insn (rtx insn)
{
  gcc_assert (insn != NULL);

  sched_init_luids (NULL, NULL, NULL, insn);
  sched_extend_target ();
  sched_deps_init (false);
  haifa_init_h_i_d (NULL, NULL, NULL, insn);

  if (adding_bb_to_current_region_p)
    {
      sd_init_insn (insn);

      /* Extend dependency caches by one element.  */
      extend_dependency_caches (1, false);
    }
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
  rtx insn = emit_insn_after (pat, last_scheduled_insn);
  last_scheduled_insn = insn;
  haifa_init_insn (insn);
  return insn;
}

#endif /* INSN_SCHEDULING */
