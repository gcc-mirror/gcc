/* Instruction scheduling pass.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
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
#include "basic-block.h"
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

#ifdef INSN_SCHEDULING

/* issue_rate is the number of insns that can be scheduled in the same
   machine cycle.  It can be defined in the config/mach/mach.h file,
   otherwise we set it to 1.  */

static int issue_rate;

/* If the following variable value is nonzero, the scheduler inserts
   bubbles (nop insns).  The value of variable affects on scheduler
   behavior only if automaton pipeline interface with multipass
   scheduling is used and hook dfa_bubble is defined.  */
int insert_schedule_bubbles_p = 0;

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

/* Highest uid before scheduling.  */
static int old_max_uid;

/* fix_sched_param() is called from toplev.c upon detection
   of the -fsched-verbose=N option.  */

void
fix_sched_param (const char *param, const char *val)
{
  if (!strcmp (param, "verbose"))
    sched_verbose_param = atoi (val);
  else
    warning ("fix_sched_param: unknown param: %s", param);
}

struct haifa_insn_data *h_i_d;

#define LINE_NOTE(INSN)		(h_i_d[INSN_UID (INSN)].line_note)
#define INSN_TICK(INSN)		(h_i_d[INSN_UID (INSN)].tick)

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
   time has passed.  For the old pipeline description interface,
   INSN_QUEUE_SIZE is a power of two larger than MAX_BLOCKAGE and
   MAX_READY_COST computed by genattr.c.  For the new pipeline
   description interface, MAX_INSN_QUEUE_INDEX is a power of two minus
   one which is larger than maximal time of instruction execution
   computed by genattr.c on the base maximal time of functional unit
   reservations and geting a result.  This is the longest time an
   insn may be queued.  */

#define MAX_INSN_QUEUE_INDEX max_insn_queue_index_macro_value

static rtx *insn_queue;
static int q_ptr = 0;
static int q_size = 0;
#define NEXT_Q(X) (((X)+1) & MAX_INSN_QUEUE_INDEX)
#define NEXT_Q_AFTER(X, C) (((X)+C) & MAX_INSN_QUEUE_INDEX)

/* The following variable defines value for macro
   MAX_INSN_QUEUE_INDEX.  */
static int max_insn_queue_index_macro_value;

/* The following variable value refers for all current and future
   reservations of the processor units.  */
state_t curr_state;

/* The following variable value is size of memory representing all
   current and future reservations of the processor units.  It is used
   only by DFA based scheduler.  */
static size_t dfa_state_size;

/* The following array is used to find the best insn from ready when
   the automaton pipeline interface is used.  */
static char *ready_try;

/* Describe the ready list of the scheduler.
   VEC holds space enough for all insns in the current region.  VECLEN
   says how many exactly.
   FIRST is the index of the element with the highest priority; i.e. the
   last one in the ready list, since elements are ordered by ascending
   priority.
   N_READY determines how many insns are on the ready list.  */

struct ready_list
{
  rtx *vec;
  int veclen;
  int first;
  int n_ready;
};

static int may_trap_exp (rtx, int);

/* Nonzero iff the address is comprised from at most 1 register.  */
#define CONST_BASED_ADDRESS_P(x)			\
  (GET_CODE (x) == REG					\
   || ((GET_CODE (x) == PLUS || GET_CODE (x) == MINUS	\
	|| (GET_CODE (x) == LO_SUM))			\
       && (CONSTANT_P (XEXP (x, 0))			\
	   || CONSTANT_P (XEXP (x, 1)))))

/* Returns a class that insn with GET_DEST(insn)=x may belong to,
   as found by analyzing insn's expression.  */

static int
may_trap_exp (rtx x, int is_store)
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
   IFREE: load from a globally safe location.
   IRISKY: volatile load.
   PFREE_CANDIDATE, PRISKY_CANDIDATE: load that need to be checked for
   being either PFREE or PRISKY.  */

int
haifa_classify_insn (rtx insn)
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

/* Forward declarations.  */

/* The scheduler using only DFA description should never use the
   following five functions:  */
static unsigned int blockage_range (int, rtx);
static void clear_units (void);
static void schedule_unit (int, rtx, int);
static int actual_hazard (int, rtx, int, int);
static int potential_hazard (int, rtx, int);

static int priority (rtx);
static int rank_for_schedule (const void *, const void *);
static void swap_sort (rtx *, int);
static void queue_insn (rtx, int);
static int schedule_insn (rtx, struct ready_list *, int);
static int find_set_reg_weight (rtx);
static void find_insn_reg_weight (int);
static void adjust_priority (rtx);
static void advance_one_cycle (void);

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

static rtx unlink_other_notes (rtx, rtx);
static rtx unlink_line_notes (rtx, rtx);
static rtx reemit_notes (rtx, rtx);

static rtx *ready_lastpos (struct ready_list *);
static void ready_sort (struct ready_list *);
static rtx ready_remove_first (struct ready_list *);

static void queue_to_ready (struct ready_list *);
static int early_queue_to_ready (state_t, struct ready_list *);

static void debug_ready_list (struct ready_list *);

static rtx move_insn1 (rtx, rtx);
static rtx move_insn (rtx, rtx);

/* The following functions are used to implement multi-pass scheduling
   on the first cycle.  It is used only for DFA based scheduler.  */
static rtx ready_element (struct ready_list *, int);
static rtx ready_remove (struct ready_list *, int);
static int max_issue (struct ready_list *, int *);

static rtx choose_ready (struct ready_list *);

#endif /* INSN_SCHEDULING */

/* Point to state used for the current scheduling pass.  */
struct sched_info *current_sched_info;

#ifndef INSN_SCHEDULING
void
schedule_insns (FILE *dump_file ATTRIBUTE_UNUSED)
{
}
#else

/* Pointer to the last instruction scheduled.  Used by rank_for_schedule,
   so that insns independent of the last scheduled insn will be preferred
   over dependent instructions.  */

static rtx last_scheduled_insn;

/* Compute the function units used by INSN.  This caches the value
   returned by function_units_used.  A function unit is encoded as the
   unit number if the value is non-negative and the complement of a
   mask if the value is negative.  A function unit index is the
   non-negative encoding.  The scheduler using only DFA description
   should never use the following function.  */

HAIFA_INLINE int
insn_unit (rtx insn)
{
  int unit = INSN_UNIT (insn);

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
   minimum value and the lower half gives the maximum value.  The
   scheduler using only DFA description should never use the following
   function.  */

HAIFA_INLINE static unsigned int
blockage_range (int unit, rtx insn)
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

/* A vector indexed by function unit instance giving the last insn to
   use the unit.  The value of the function unit instance index for
   unit U instance I is (U + I * FUNCTION_UNITS_SIZE).  The scheduler
   using only DFA description should never use the following variable.  */
#if FUNCTION_UNITS_SIZE
static rtx unit_last_insn[FUNCTION_UNITS_SIZE * MAX_MULTIPLICITY];
#else
static rtx unit_last_insn[1];
#endif

/* A vector indexed by function unit instance giving the minimum time
   when the unit will unblock based on the maximum blockage cost.  The
   scheduler using only DFA description should never use the following
   variable.  */
#if FUNCTION_UNITS_SIZE
static int unit_tick[FUNCTION_UNITS_SIZE * MAX_MULTIPLICITY];
#else
static int unit_tick[1];
#endif

/* A vector indexed by function unit number giving the number of insns
   that remain to use the unit.  The scheduler using only DFA
   description should never use the following variable.  */
#if FUNCTION_UNITS_SIZE
static int unit_n_insns[FUNCTION_UNITS_SIZE];
#else
static int unit_n_insns[1];
#endif

/* Access the unit_last_insn array.  Used by the visualization code.
   The scheduler using only DFA description should never use the
   following function.  */

rtx
get_unit_last_insn (int instance)
{
  return unit_last_insn[instance];
}

/* Reset the function unit state to the null state.  */

static void
clear_units (void)
{
  memset (unit_last_insn, 0, sizeof (unit_last_insn));
  memset (unit_tick, 0, sizeof (unit_tick));
  memset (unit_n_insns, 0, sizeof (unit_n_insns));
}

/* Return the issue-delay of an insn.  The scheduler using only DFA
   description should never use the following function.  */

HAIFA_INLINE int
insn_issue_delay (rtx insn)
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
   was COST.  The scheduler using only DFA description should never
   use the following function.  */

HAIFA_INLINE int
actual_hazard_this_instance (int unit, int instance, rtx insn, int clock, int cost)
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

/* Record INSN as having begun execution on the units encoded by UNIT
   at time CLOCK.  The scheduler using only DFA description should
   never use the following function.  */

static void
schedule_unit (int unit, rtx insn, int clock)
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

/* Return the actual hazard cost of executing INSN on the units
   encoded by UNIT at time CLOCK if the previous actual hazard cost
   was COST.  The scheduler using only DFA description should never
   use the following function.  */

static int
actual_hazard (int unit, rtx insn, int clock, int cost)
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
   units encoded by UNIT if the previous potential hazard cost was
   COST.  An insn with a large blockage time is chosen in preference
   to one with a smaller time; an insn that uses a unit that is more
   likely to be used is chosen in preference to one with a unit that
   is less used.  We are trying to minimize a subsequent actual
   hazard.  The scheduler using only DFA description should never use
   the following function.  */

HAIFA_INLINE static int
potential_hazard (int unit, rtx insn, int cost)
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

HAIFA_INLINE int
insn_cost (rtx insn, rtx link, rtx used)
{
  int cost = INSN_COST (insn);

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
	  if (targetm.sched.use_dfa_pipeline_interface
	      && (*targetm.sched.use_dfa_pipeline_interface) ())
	    cost = insn_default_latency (insn);
	  else
	    cost = result_ready_cost (insn);

	  if (cost < 0)
	    cost = 0;

	  INSN_COST (insn) = cost;
	}
    }

  /* In this case estimate cost without caring how insn is used.  */
  if (link == 0 || used == 0)
    return cost;

  /* A USE insn should never require the value used to be computed.
     This allows the computation of a function's result and parameter
     values to overlap the return and call.  */
  if (recog_memoized (used) < 0)
    cost = 0;
  else
    {
      if (targetm.sched.use_dfa_pipeline_interface
	  && (*targetm.sched.use_dfa_pipeline_interface) ())
	{
	  if (INSN_CODE (insn) >= 0)
	    {
	      if (REG_NOTE_KIND (link) == REG_DEP_ANTI)
		cost = 0;
	      else if (REG_NOTE_KIND (link) == REG_DEP_OUTPUT)
		{
		  cost = (insn_default_latency (insn)
			  - insn_default_latency (used));
		  if (cost <= 0)
		    cost = 1;
		}
	      else if (bypass_p (insn))
		cost = insn_latency (insn, used);
	    }
	}

      if (targetm.sched.adjust_cost)
	cost = (*targetm.sched.adjust_cost) (used, link, insn, cost);

      if (cost < 0)
	cost = 0;
    }

  return cost;
}

/* Compute the priority number for INSN.  */

static int
priority (rtx insn)
{
  rtx link;

  if (! INSN_P (insn))
    return 0;

  if (! INSN_PRIORITY_KNOWN (insn))
    {
      int this_priority = 0;

      if (INSN_DEPEND (insn) == 0)
	this_priority = insn_cost (insn, 0, 0);
      else
	{
	  for (link = INSN_DEPEND (insn); link; link = XEXP (link, 1))
	    {
	      rtx next;
	      int next_priority;

	      if (RTX_INTEGRATED_P (link))
		continue;

	      next = XEXP (link, 0);

	      /* Critical path is meaningful in block boundaries only.  */
	      if (! (*current_sched_info->contributes_to_priority) (next, insn))
		continue;

	      next_priority = insn_cost (insn, link, next) + priority (next);
	      if (next_priority > this_priority)
		this_priority = next_priority;
	    }
	}
      INSN_PRIORITY (insn) = this_priority;
      INSN_PRIORITY_KNOWN (insn) = 1;
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
  rtx link;
  int tmp_class, tmp2_class, depend_count1, depend_count2;
  int val, priority_val, weight_val, info_val;

  /* The insn in a schedule group should be issued the first.  */
  if (SCHED_GROUP_P (tmp) != SCHED_GROUP_P (tmp2))
    return SCHED_GROUP_P (tmp2) ? 1 : -1;

  /* Prefer insn with higher priority.  */
  priority_val = INSN_PRIORITY (tmp2) - INSN_PRIORITY (tmp);

  if (priority_val)
    return priority_val;

  /* Prefer an insn with smaller contribution to registers-pressure.  */
  if (!reload_completed &&
      (weight_val = INSN_REG_WEIGHT (tmp) - INSN_REG_WEIGHT (tmp2)))
    return weight_val;

  info_val = (*current_sched_info->rank) (tmp, tmp2);
  if (info_val)
    return info_val;

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
  insn_queue[next_q] = link;
  q_size += 1;

  if (sched_verbose >= 2)
    {
      fprintf (sched_dump, ";;\t\tReady-->Q: insn %s: ",
	       (*current_sched_info->print_insn) (insn, 0));

      fprintf (sched_dump, "queued for %d cycles.\n", n_cycles);
    }
}

/* Return a pointer to the bottom of the ready list, i.e. the insn
   with the lowest priority.  */

HAIFA_INLINE static rtx *
ready_lastpos (struct ready_list *ready)
{
  if (ready->n_ready == 0)
    abort ();
  return ready->vec + ready->first - ready->n_ready + 1;
}

/* Add an element INSN to the ready list so that it ends up with the lowest
   priority.  */

HAIFA_INLINE void
ready_add (struct ready_list *ready, rtx insn)
{
  if (ready->first == ready->n_ready)
    {
      memmove (ready->vec + ready->veclen - ready->n_ready,
	       ready_lastpos (ready),
	       ready->n_ready * sizeof (rtx));
      ready->first = ready->veclen - 1;
    }
  ready->vec[ready->first - ready->n_ready] = insn;
  ready->n_ready++;
}

/* Remove the element with the highest priority from the ready list and
   return it.  */

HAIFA_INLINE static rtx
ready_remove_first (struct ready_list *ready)
{
  rtx t;
  if (ready->n_ready == 0)
    abort ();
  t = ready->vec[ready->first--];
  ready->n_ready--;
  /* If the queue becomes empty, reset it.  */
  if (ready->n_ready == 0)
    ready->first = ready->veclen - 1;
  return t;
}

/* The following code implements multi-pass scheduling for the first
   cycle.  In other words, we will try to choose ready insn which
   permits to start maximum number of insns on the same cycle.  */

/* Return a pointer to the element INDEX from the ready.  INDEX for
   insn with the highest priority is 0, and the lowest priority has
   N_READY - 1.  */

HAIFA_INLINE static rtx
ready_element (struct ready_list *ready, int index)
{
#ifdef ENABLE_CHECKING
  if (ready->n_ready == 0 || index >= ready->n_ready)
    abort ();
#endif
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
  if (ready->n_ready == 0 || index >= ready->n_ready)
    abort ();
  t = ready->vec[ready->first - index];
  ready->n_ready--;
  for (i = index; i < ready->n_ready; i++)
    ready->vec[ready->first - i] = ready->vec[ready->first - i - 1];
  return t;
}


/* Sort the ready list READY by ascending priority, using the SCHED_SORT
   macro.  */

HAIFA_INLINE static void
ready_sort (struct ready_list *ready)
{
  rtx *first = ready_lastpos (ready);
  SCHED_SORT (first, ready->n_ready);
}

/* PREV is an insn that is ready to execute.  Adjust its priority if that
   will help shorten or lengthen register lifetimes as appropriate.  Also
   provide a hook for the target to tweek itself.  */

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
      (*targetm.sched.adjust_priority) (prev, INSN_PRIORITY (prev));
}

/* Advance time on one cycle.  */
HAIFA_INLINE static void
advance_one_cycle (void)
{
  if (targetm.sched.use_dfa_pipeline_interface
      && (*targetm.sched.use_dfa_pipeline_interface) ())
    {
      if (targetm.sched.dfa_pre_cycle_insn)
	state_transition (curr_state,
			  (*targetm.sched.dfa_pre_cycle_insn) ());

      state_transition (curr_state, NULL);

      if (targetm.sched.dfa_post_cycle_insn)
	state_transition (curr_state,
			  (*targetm.sched.dfa_post_cycle_insn) ());
    }
}

/* Clock at which the previous instruction was issued.  */
static int last_clock_var;

/* INSN is the "currently executing insn".  Launch each insn which was
   waiting on INSN.  READY is the ready list which contains the insns
   that are ready to fire.  CLOCK is the current cycle.  The function
   returns necessary cycle advance after issuing the insn (it is not
   zero for insns in a schedule group).  */

static int
schedule_insn (rtx insn, struct ready_list *ready, int clock)
{
  rtx link;
  int advance = 0;
  int unit = 0;
  int premature_issue = 0;

  if (!targetm.sched.use_dfa_pipeline_interface
      || !(*targetm.sched.use_dfa_pipeline_interface) ())
    unit = insn_unit (insn);

  if (targetm.sched.use_dfa_pipeline_interface
      && (*targetm.sched.use_dfa_pipeline_interface) ()
      && sched_verbose >= 1)
    {
      char buf[2048];

      print_insn (buf, insn, 0);
      buf[40] = 0;
      fprintf (sched_dump, ";;\t%3i--> %-40s:", clock, buf);

      if (recog_memoized (insn) < 0)
	fprintf (sched_dump, "nothing");
      else
	print_reservation (sched_dump, insn);
      fputc ('\n', sched_dump);
    }
  else if (sched_verbose >= 2)
    {
      fprintf (sched_dump, ";;\t\t--> scheduling insn <<<%d>>> on unit ",
	       INSN_UID (insn));
      insn_print_units (insn);
      fputc ('\n', sched_dump);
    }

  if (!targetm.sched.use_dfa_pipeline_interface
      || !(*targetm.sched.use_dfa_pipeline_interface) ())
    {
      if (sched_verbose && unit == -1)
	visualize_no_unit (insn);


      if (MAX_BLOCKAGE > 1 || issue_rate > 1 || sched_verbose)
	schedule_unit (unit, insn, clock);

      if (INSN_DEPEND (insn) == 0)
	return 0;
    }

  if (INSN_TICK (insn) > clock)
    {
      /* 'insn' has been prematurely moved from the queue to the
	 ready list.  */
      premature_issue = INSN_TICK (insn) - clock;
    }

  for (link = INSN_DEPEND (insn); link != 0; link = XEXP (link, 1))
    {
      rtx next = XEXP (link, 0);
      int cost = insn_cost (insn, link, next);

      INSN_TICK (next) = MAX (INSN_TICK (next), clock + cost + premature_issue);

      if ((INSN_DEP_COUNT (next) -= 1) == 0)
	{
	  int effective_cost = INSN_TICK (next) - clock;

	  if (! (*current_sched_info->new_ready) (next))
	    continue;

	  if (sched_verbose >= 2)
	    {
	      fprintf (sched_dump, ";;\t\tdependences resolved: insn %s ",
		       (*current_sched_info->print_insn) (next, 0));

	      if (effective_cost < 1)
		fprintf (sched_dump, "into ready\n");
	      else
		fprintf (sched_dump, "into queue with cost=%d\n",
			 effective_cost);
	    }

	  /* Adjust the priority of NEXT and either put it on the ready
	     list or queue it.  */
	  adjust_priority (next);
	  if (effective_cost < 1)
	    ready_add (ready, next);
	  else
	    {
	      queue_insn (next, effective_cost);

	      if (SCHED_GROUP_P (next) && advance < effective_cost)
		advance = effective_cost;
	    }
	}
    }

  /* Annotate the instruction with issue information -- TImode
     indicates that the instruction is expected not to be able
     to issue on the same cycle as the previous insn.  A machine
     may use this information to decide how the instruction should
     be aligned.  */
  if (issue_rate > 1
      && GET_CODE (PATTERN (insn)) != USE
      && GET_CODE (PATTERN (insn)) != CLOBBER)
    {
      if (reload_completed)
	PUT_MODE (insn, clock > last_clock_var ? TImode : VOIDmode);
      last_clock_var = clock;
    }
  return advance;
}

/* Functions for handling of notes.  */

/* Delete notes beginning with INSN and put them in the chain
   of notes ended by NOTE_LIST.
   Returns the insn following the notes.  */

static rtx
unlink_other_notes (rtx insn, rtx tail)
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
      if (NOTE_LINE_NUMBER (insn) != NOTE_INSN_LOOP_BEG
	  && NOTE_LINE_NUMBER (insn) != NOTE_INSN_LOOP_END
	  && NOTE_LINE_NUMBER (insn) != NOTE_INSN_BASIC_BLOCK
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
unlink_line_notes (rtx insn, rtx tail)
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

void
get_block_head_tail (int b, rtx *headp, rtx *tailp)
{
  /* HEAD and TAIL delimit the basic block being scheduled.  */
  rtx head = BB_HEAD (BASIC_BLOCK (b));
  rtx tail = BB_END (BASIC_BLOCK (b));

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

/* Return nonzero if there are no real insns in the range [ HEAD, TAIL ].  */

int
no_real_insns_p (rtx head, rtx tail)
{
  while (head != NEXT_INSN (tail))
    {
      if (GET_CODE (head) != NOTE && GET_CODE (head) != CODE_LABEL)
	return 0;
      head = NEXT_INSN (head);
    }
  return 1;
}

/* Delete line notes from one block. Save them so they can be later restored
   (in restore_line_notes).  HEAD and TAIL are the boundaries of the
   block in which notes should be processed.  */

void
rm_line_notes (rtx head, rtx tail)
{
  rtx next_tail;
  rtx insn;

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

/* Save line number notes for each insn in block B.  HEAD and TAIL are
   the boundaries of the block in which notes should be processed.  */

void
save_line_notes (int b, rtx head, rtx tail)
{
  rtx next_tail;

  /* We must use the true line number for the first insn in the block
     that was computed and saved at the start of this pass.  We can't
     use the current line number, because scheduling of the previous
     block may have changed the current line number.  */

  rtx line = line_note_head[b];
  rtx insn;

  next_tail = NEXT_INSN (tail);

  for (insn = head; insn != next_tail; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == NOTE && NOTE_LINE_NUMBER (insn) > 0)
      line = insn;
    else
      LINE_NOTE (insn) = line;
}

/* After a block was scheduled, insert line notes into the insns list.
   HEAD and TAIL are the boundaries of the block in which notes should
   be processed.  */

void
restore_line_notes (rtx head, rtx tail)
{
  rtx line, note, prev, new;
  int added_notes = 0;
  rtx next_tail, insn;

  head = head;
  next_tail = NEXT_INSN (tail);

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
	     && INSN_UID (insn) < old_max_uid
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
    fprintf (sched_dump, ";; added %d line-number notes\n", added_notes);
}

/* After scheduling the function, delete redundant line notes from the
   insns list.  */

void
rm_redundant_line_notes (void)
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
    fprintf (sched_dump, ";; deleted %d line-number notes\n", notes);
}

/* Delete notes between HEAD and TAIL and put them in the chain
   of notes ended by NOTE_LIST.  */

void
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

/* This function looks for a new register being defined.
   If the destination register is already used by the source,
   a new register is not needed.  */

static int
find_set_reg_weight (rtx x)
{
  if (GET_CODE (x) == CLOBBER
      && register_operand (SET_DEST (x), VOIDmode))
    return 1;
  if (GET_CODE (x) == SET
      && register_operand (SET_DEST (x), VOIDmode))
    {
      if (GET_CODE (SET_DEST (x)) == REG)
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

/* Calculate INSN_REG_WEIGHT for all insns of a block.  */

static void
find_insn_reg_weight (int b)
{
  rtx insn, next_tail, head, tail;

  get_block_head_tail (b, &head, &tail);
  next_tail = NEXT_INSN (tail);

  for (insn = head; insn != next_tail; insn = NEXT_INSN (insn))
    {
      int reg_weight = 0;
      rtx x;

      /* Handle register life information.  */
      if (! INSN_P (insn))
	continue;

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
}

/* Scheduling clock, modified in schedule_block() and queue_to_ready ().  */
static int clock_var;

/* Move insns that became ready to fire from queue to ready list.  */

static void
queue_to_ready (struct ready_list *ready)
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
	fprintf (sched_dump, ";;\t\tQ-->Ready: insn %s: ",
		 (*current_sched_info->print_insn) (insn, 0));

      ready_add (ready, insn);
      if (sched_verbose >= 2)
	fprintf (sched_dump, "moving to ready without stalls\n");
    }
  insn_queue[q_ptr] = 0;

  /* If there are no ready insns, stall until one is ready and add all
     of the pending insns at that point to the ready list.  */
  if (ready->n_ready == 0)
    {
      int stalls;

      for (stalls = 1; stalls <= MAX_INSN_QUEUE_INDEX; stalls++)
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

		  ready_add (ready, insn);
		  if (sched_verbose >= 2)
		    fprintf (sched_dump, "moving to ready with %d stalls\n", stalls);
		}
	      insn_queue[NEXT_Q_AFTER (q_ptr, stalls)] = 0;

	      advance_one_cycle ();

	      break;
	    }

	  advance_one_cycle ();
	}

      if ((!targetm.sched.use_dfa_pipeline_interface
	   || !(*targetm.sched.use_dfa_pipeline_interface) ())
	  && sched_verbose && stalls)
	visualize_stall_cycles (stalls);

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
	      rtx dep_link = 0;
	      int dep_cost;

	      if (GET_CODE (prev_insn) != NOTE)
		{
		  dep_link = find_insn_list (insn, INSN_DEPEND (prev_insn));
		  if (dep_link)
		    {
		      dep_cost = insn_cost (prev_insn, dep_link, insn) ;
		      if (targetm.sched.is_costly_dependence (prev_insn, insn, 
				dep_link, dep_cost, 
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

  for (stalls = 0; stalls <= MAX_INSN_QUEUE_INDEX; stalls++)
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
		      ready_add (ready, insn);

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
			/* remove only one insn from Q at a time */
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

/* move_insn1: Remove INSN from insn chain, and link it after LAST insn.  */

static rtx
move_insn1 (rtx insn, rtx last)
{
  NEXT_INSN (PREV_INSN (insn)) = NEXT_INSN (insn);
  PREV_INSN (NEXT_INSN (insn)) = PREV_INSN (insn);

  NEXT_INSN (insn) = NEXT_INSN (last);
  PREV_INSN (NEXT_INSN (last)) = insn;

  NEXT_INSN (last) = insn;
  PREV_INSN (insn) = last;

  return insn;
}

/* Search INSN for REG_SAVE_NOTE note pairs for
   NOTE_INSN_{LOOP,EHREGION}_{BEG,END}; and convert them back into
   NOTEs.  The REG_SAVE_NOTE note following first one is contains the
   saved value for NOTE_BLOCK_NUMBER which is useful for
   NOTE_INSN_EH_REGION_{BEG,END} NOTEs.  LAST is the last instruction
   output by the instruction scheduler.  Return the new value of LAST.  */

static rtx
reemit_notes (rtx insn, rtx last)
{
  rtx note, retval;

  retval = last;
  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
    {
      if (REG_NOTE_KIND (note) == REG_SAVE_NOTE)
	{
	  enum insn_note note_type = INTVAL (XEXP (note, 0));

	  last = emit_note_before (note_type, last);
	  remove_note (insn, note);
	  note = XEXP (note, 1);
	  if (note_type == NOTE_INSN_EH_REGION_BEG
	      || note_type == NOTE_INSN_EH_REGION_END)
	    NOTE_EH_HANDLER (last) = INTVAL (XEXP (note, 0));
	  remove_note (insn, note);
	}
    }
  return retval;
}

/* Move INSN.  Reemit notes if needed.

   Return the last insn emitted by the scheduler, which is the
   return value from the first call to reemit_notes.  */

static rtx
move_insn (rtx insn, rtx last)
{
  rtx retval = NULL;

  move_insn1 (insn, last);

  /* If this is the first call to reemit_notes, then record
     its return value.  */
  if (retval == NULL_RTX)
    retval = reemit_notes (insn, insn);
  else
    reemit_notes (insn, insn);

  SCHED_GROUP_P (insn) = 0;

  return retval;
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
static int cycle_issued_insns;

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
   insns are already issued in this try.  INDEX will contain index
   of the best insn in READY.  The following function is used only for
   first cycle multipass scheduling.  */
static int
max_issue (struct ready_list *ready, int *index)
{
  int n, i, all, n_ready, best, delay, tries_num;
  struct choice_entry *top;
  rtx insn;

  best = 0;
  memcpy (choice_stack->state, curr_state, dfa_state_size);
  top = choice_stack;
  top->rest = cached_first_cycle_multipass_dfa_lookahead;
  top->n = 0;
  n_ready = ready->n_ready;
  for (all = i = 0; i < n_ready; i++)
    if (!ready_try [i])
      all++;
  i = 0;
  tries_num = 0;
  for (;;)
    {
      if (top->rest == 0 || i >= n_ready)
	{
	  if (top == choice_stack)
	    break;
	  if (best < top - choice_stack && ready_try [0])
	    {
	      best = top - choice_stack;
	      *index = choice_stack [1].index;
	      if (top->n == issue_rate - cycle_issued_insns || best == all)
		break;
	    }
	  i = top->index;
	  ready_try [i] = 0;
	  top--;
	  memcpy (curr_state, top->state, dfa_state_size);
	}
      else if (!ready_try [i])
	{
	  tries_num++;
	  if (tries_num > max_lookahead_tries)
	    break;
	  insn = ready_element (ready, i);
	  delay = state_transition (curr_state, insn);
	  if (delay < 0)
	    {
	      if (state_dead_lock_p (curr_state))
		top->rest = 0;
	      else
		top->rest--;
	      n = top->n;
	      if (memcmp (top->state, curr_state, dfa_state_size) != 0)
		n++;
	      top++;
	      top->rest = cached_first_cycle_multipass_dfa_lookahead;
	      top->index = i;
	      top->n = n;
	      memcpy (top->state, curr_state, dfa_state_size);
	      ready_try [i] = 1;
	      i = -1;
	    }
	}
      i++;
    }
  while (top != choice_stack)
    {
      ready_try [top->index] = 0;
      top--;
    }
  memcpy (curr_state, choice_stack->state, dfa_state_size);
  return best;
}

/* The following function chooses insn from READY and modifies
   *N_READY and READY.  The following function is used only for first
   cycle multipass scheduling.  */

static rtx
choose_ready (struct ready_list *ready)
{
  int lookahead = 0;

  if (targetm.sched.first_cycle_multipass_dfa_lookahead)
    lookahead = (*targetm.sched.first_cycle_multipass_dfa_lookahead) ();
  if (lookahead <= 0 || SCHED_GROUP_P (ready_element (ready, 0)))
    return ready_remove_first (ready);
  else
    {
      /* Try to choose the better insn.  */
      int index = 0, i;
      rtx insn;

      if (cached_first_cycle_multipass_dfa_lookahead != lookahead)
	{
	  cached_first_cycle_multipass_dfa_lookahead = lookahead;
	  max_lookahead_tries = 100;
	  for (i = 0; i < issue_rate; i++)
	    max_lookahead_tries *= lookahead;
	}
      insn = ready_element (ready, 0);
      if (INSN_CODE (insn) < 0)
	return ready_remove_first (ready);
      for (i = 1; i < ready->n_ready; i++)
	{
	  insn = ready_element (ready, i);
	  ready_try [i]
	    = (INSN_CODE (insn) < 0
	       || (targetm.sched.first_cycle_multipass_dfa_lookahead_guard
		   && !(*targetm.sched.first_cycle_multipass_dfa_lookahead_guard) (insn)));
	}
      if (max_issue (ready, &index) == 0)
	return ready_remove_first (ready);
      else
	return ready_remove (ready, index);
    }
}

/* Called from backends from targetm.sched.reorder to emit stuff into
   the instruction stream.  */

rtx
sched_emit_insn (rtx pat)
{
  rtx insn = emit_insn_after (pat, last_scheduled_insn);
  last_scheduled_insn = insn;
  return insn;
}

/* Use forward list scheduling to rearrange insns of block B in region RGN,
   possibly bringing insns from subsequent blocks in the same region.  */

void
schedule_block (int b, int rgn_n_insns)
{
  struct ready_list ready;
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

  if (head == tail && (! INSN_P (head)))
    abort ();

  /* Debug info.  */
  if (sched_verbose)
    {
      fprintf (sched_dump, ";;   ======================================================\n");
      fprintf (sched_dump,
	       ";;   -- basic block %d from %d to %d -- %s reload\n",
	       b, INSN_UID (head), INSN_UID (tail),
	       (reload_completed ? "after" : "before"));
      fprintf (sched_dump, ";;   ======================================================\n");
      fprintf (sched_dump, "\n");

      visualize_alloc ();
      init_block_visualization ();
    }

  if (targetm.sched.use_dfa_pipeline_interface
      && (*targetm.sched.use_dfa_pipeline_interface) ())
    state_reset (curr_state);
  else
    clear_units ();

  /* Allocate the ready list.  */
  ready.veclen = rgn_n_insns + 1 + issue_rate;
  ready.first = ready.veclen - 1;
  ready.vec = xmalloc (ready.veclen * sizeof (rtx));
  ready.n_ready = 0;

  if (targetm.sched.use_dfa_pipeline_interface
      && (*targetm.sched.use_dfa_pipeline_interface) ())
    {
      /* It is used for first cycle multipass scheduling.  */
      temp_state = alloca (dfa_state_size);
      ready_try = xcalloc ((rgn_n_insns + 1), sizeof (char));
      choice_stack = xmalloc ((rgn_n_insns + 1)
			      * sizeof (struct choice_entry));
      for (i = 0; i <= rgn_n_insns; i++)
	choice_stack[i].state = xmalloc (dfa_state_size);
    }

  (*current_sched_info->init_ready_list) (&ready);

  if (targetm.sched.md_init)
    (*targetm.sched.md_init) (sched_dump, sched_verbose, ready.veclen);

  /* We start inserting insns after PREV_HEAD.  */
  last_scheduled_insn = prev_head;

  /* Initialize INSN_QUEUE.  Q_SIZE is the total number of insns in the
     queue.  */
  q_ptr = 0;
  q_size = 0;

  if (!targetm.sched.use_dfa_pipeline_interface
      || !(*targetm.sched.use_dfa_pipeline_interface) ())
    max_insn_queue_index_macro_value = INSN_QUEUE_SIZE - 1;
  else
    max_insn_queue_index_macro_value = max_insn_queue_index;

  insn_queue = alloca ((MAX_INSN_QUEUE_INDEX + 1) * sizeof (rtx));
  memset (insn_queue, 0, (MAX_INSN_QUEUE_INDEX + 1) * sizeof (rtx));
  last_clock_var = -1;

  /* Start just before the beginning of time.  */
  clock_var = -1;
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

	  if (ready.n_ready == 0)
	    abort ();

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

      /* Allow the target to reorder the list, typically for
	 better instruction bundling.  */
      if (sort_p && targetm.sched.reorder
	  && (ready.n_ready == 0
	      || !SCHED_GROUP_P (ready_element (&ready, 0))))
	can_issue_more =
	  (*targetm.sched.reorder) (sched_dump, sched_verbose,
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
	      fprintf (sched_dump, ";;\tReady list (t =%3d):  ",
		       clock_var);
	      debug_ready_list (&ready);
	    }

	  if (!targetm.sched.use_dfa_pipeline_interface
	      || !(*targetm.sched.use_dfa_pipeline_interface) ())
	    {
	      if (ready.n_ready == 0 || !can_issue_more
		  || !(*current_sched_info->schedule_more_p) ())
		break;
	      insn = ready_remove_first (&ready);
	      cost = actual_hazard (insn_unit (insn), insn, clock_var, 0);
	    }
	  else
	    {
	      if (ready.n_ready == 0 
		  && can_issue_more 
		  && reload_completed) 
		{
		  /* Allow scheduling insns directly from the queue in case
		     there's nothing better to do (ready list is empty) but
		     there are still vacant dispatch slots in the current cycle.  */
		  if (sched_verbose >= 6)
		    fprintf(sched_dump,";;\t\tSecond chance\n");
		  memcpy (temp_state, curr_state, dfa_state_size);
		  if (early_queue_to_ready (temp_state, &ready))
		    ready_sort (&ready);
		}

	      if (ready.n_ready == 0 || !can_issue_more
		  || state_dead_lock_p (curr_state)
		  || !(*current_sched_info->schedule_more_p) ())
		break;

	      /* Select and remove the insn from the ready list.  */
	      if (sort_p)
		insn = choose_ready (&ready);
	      else
		insn = ready_remove_first (&ready);

	      if (targetm.sched.dfa_new_cycle
		  && (*targetm.sched.dfa_new_cycle) (sched_dump, sched_verbose,
						     insn, last_clock_var,
						     clock_var, &sort_p))
		{
		  ready_add (&ready, insn);
		  break;
		}

	      sort_p = TRUE;
	      memcpy (temp_state, curr_state, dfa_state_size);
	      if (recog_memoized (insn) < 0)
		{
		  asm_p = (GET_CODE (PATTERN (insn)) == ASM_INPUT
			   || asm_noperands (PATTERN (insn)) >= 0);
		  if (!first_cycle_insn_p && asm_p)
		    /* This is asm insn which is tryed to be issued on the
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

		  if (targetm.sched.first_cycle_multipass_dfa_lookahead
		      && targetm.sched.dfa_bubble)
		    {
		      if (cost == 0)
			{
			  int j;
			  rtx bubble;

			  for (j = 0;
			       (bubble = (*targetm.sched.dfa_bubble) (j))
				 != NULL_RTX;
			       j++)
			    {
			      memcpy (temp_state, curr_state, dfa_state_size);

			      if (state_transition (temp_state, bubble) < 0
				  && state_transition (temp_state, insn) < 0)
				break;
			    }

			  if (bubble != NULL_RTX)
			    {
			      if (insert_schedule_bubbles_p)
				{
				  rtx copy;

				  copy = copy_rtx (PATTERN (bubble));
				  emit_insn_after (copy, last_scheduled_insn);
				  last_scheduled_insn
				    = NEXT_INSN (last_scheduled_insn);
				  INSN_CODE (last_scheduled_insn)
				    = INSN_CODE (bubble);

				  /* Annotate the same for the first insns
				     scheduling by using mode.  */
				  PUT_MODE (last_scheduled_insn,
					    (clock_var > last_clock_var
					     ? clock_var - last_clock_var
					     : VOIDmode));
				  last_clock_var = clock_var;

				  if (sched_verbose >= 2)
				    {
				      fprintf (sched_dump,
					       ";;\t\t--> scheduling bubble insn <<<%d>>>:reservation ",
					       INSN_UID (last_scheduled_insn));

				      if (recog_memoized (last_scheduled_insn)
					  < 0)
					fprintf (sched_dump, "nothing");
				      else
					print_reservation
					  (sched_dump, last_scheduled_insn);

				      fprintf (sched_dump, "\n");
				    }
				}
			      cost = -1;
			    }
			}
		    }

		  if (cost < 0)
		    cost = 0;
		  else if (cost == 0)
		    cost = 1;
		}
	    }


	  if (cost >= 1)
	    {
	      queue_insn (insn, cost);
	      continue;
	    }

	  if (! (*current_sched_info->can_schedule_ready_p) (insn))
	    goto next;

	  last_scheduled_insn = move_insn (insn, last_scheduled_insn);

	  if (targetm.sched.use_dfa_pipeline_interface
	      && (*targetm.sched.use_dfa_pipeline_interface) ())
	    {
	      if (memcmp (curr_state, temp_state, dfa_state_size) != 0)
		cycle_issued_insns++;
	      memcpy (curr_state, temp_state, dfa_state_size);
	    }

	  if (targetm.sched.variable_issue)
	    can_issue_more =
	      (*targetm.sched.variable_issue) (sched_dump, sched_verbose,
					       insn, can_issue_more);
	  /* A naked CLOBBER or USE generates no instruction, so do
	     not count them against the issue rate.  */
	  else if (GET_CODE (PATTERN (insn)) != USE
		   && GET_CODE (PATTERN (insn)) != CLOBBER)
	    can_issue_more--;

	  advance = schedule_insn (insn, &ready, clock_var);

	  /* After issuing an asm insn we should start a new cycle.  */
	  if (advance == 0 && asm_p)
	    advance = 1;
	  if (advance != 0)
	    break;

	next:
	  first_cycle_insn_p = 0;

	  /* Sort the ready list based on priority.  This must be
	     redone here, as schedule_insn may have readied additional
	     insns that will not be sorted correctly.  */
	  if (ready.n_ready > 0)
	    ready_sort (&ready);

	  if (targetm.sched.reorder2
	      && (ready.n_ready == 0
		  || !SCHED_GROUP_P (ready_element (&ready, 0))))
	    {
	      can_issue_more =
		(*targetm.sched.reorder2) (sched_dump, sched_verbose,
					   ready.n_ready
					   ? ready_lastpos (&ready) : NULL,
					   &ready.n_ready, clock_var);
	    }
	}

      if ((!targetm.sched.use_dfa_pipeline_interface
	   || !(*targetm.sched.use_dfa_pipeline_interface) ())
	  && sched_verbose)
	/* Debug info.  */
	visualize_scheduled_insns (clock_var);
    }

  if (targetm.sched.md_finish)
    (*targetm.sched.md_finish) (sched_dump, sched_verbose);

  /* Debug info.  */
  if (sched_verbose)
    {
      fprintf (sched_dump, ";;\tReady list (final):  ");
      debug_ready_list (&ready);
      if (!targetm.sched.use_dfa_pipeline_interface
	  || !(*targetm.sched.use_dfa_pipeline_interface) ())
	print_block_visualization ("");
    }

  /* Sanity check -- queue must be empty now.  Meaningless if region has
     multiple bbs.  */
  if (current_sched_info->queue_must_finish_empty && q_size != 0)
      abort ();

  /* Update head/tail boundaries.  */
  head = NEXT_INSN (prev_head);
  tail = last_scheduled_insn;

  if (!reload_completed)
    {
      rtx insn, link, next;

      /* INSN_TICK (minimum clock tick at which the insn becomes
         ready) may be not correct for the insn in the subsequent
         blocks of the region.  We should use a correct value of
         `clock_var' or modify INSN_TICK.  It is better to keep
         clock_var value equal to 0 at the start of a basic block.
         Therefore we modify INSN_TICK here.  */
      for (insn = head; insn != tail; insn = NEXT_INSN (insn))
	if (INSN_P (insn))
	  {
	    for (link = INSN_DEPEND (insn); link != 0; link = XEXP (link, 1))
	      {
		next = XEXP (link, 0);
		INSN_TICK (next) -= clock_var;
	      }
	  }
    }

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

  /* Debugging.  */
  if (sched_verbose)
    {
      fprintf (sched_dump, ";;   total time = %d\n;;   new head = %d\n",
	       clock_var, INSN_UID (head));
      fprintf (sched_dump, ";;   new tail = %d\n\n",
	       INSN_UID (tail));
      visualize_free ();
    }

  current_sched_info->head = head;
  current_sched_info->tail = tail;

  free (ready.vec);

  if (targetm.sched.use_dfa_pipeline_interface
      && (*targetm.sched.use_dfa_pipeline_interface) ())
    {
      free (ready_try);
      for (i = 0; i <= rgn_n_insns; i++)
	free (choice_stack [i].state);
      free (choice_stack);
    }
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

  prev_head = PREV_INSN (head);

  if (head == tail && (! INSN_P (head)))
    return 0;

  n_insn = 0;
  sched_max_insns_priority = 0;
  for (insn = tail; insn != prev_head; insn = PREV_INSN (insn))
    {
      if (GET_CODE (insn) == NOTE)
	continue;

      n_insn++;
      (void) priority (insn);

      if (INSN_PRIORITY_KNOWN (insn))
	sched_max_insns_priority =
	  MAX (sched_max_insns_priority, INSN_PRIORITY (insn)); 
    }
  sched_max_insns_priority += 1;
  current_sched_info->sched_max_insns_priority =
	sched_max_insns_priority;

  return n_insn;
}

/* Initialize some global state for the scheduler.  DUMP_FILE is to be used
   for debugging output.  */

void
sched_init (FILE *dump_file)
{
  int luid;
  basic_block b;
  rtx insn;
  int i;

  /* Disable speculative loads in their presence if cc0 defined.  */
#ifdef HAVE_cc0
  flag_schedule_speculative_load = 0;
#endif

  /* Set dump and sched_verbose for the desired debugging output.  If no
     dump-file was specified, but -fsched-verbose=N (any N), print to stderr.
     For -fsched-verbose=N, N>=10, print everything to stderr.  */
  sched_verbose = sched_verbose_param;
  if (sched_verbose_param == 0 && dump_file)
    sched_verbose = 1;
  sched_dump = ((sched_verbose_param >= 10 || !dump_file)
		? stderr : dump_file);

  /* Initialize issue_rate.  */
  if (targetm.sched.issue_rate)
    issue_rate = (*targetm.sched.issue_rate) ();
  else
    issue_rate = 1;

  if (cached_issue_rate != issue_rate)
    {
      cached_issue_rate = issue_rate;
      /* To invalidate max_lookahead_tries:  */
      cached_first_cycle_multipass_dfa_lookahead = 0;
    }

  /* We use LUID 0 for the fake insn (UID 0) which holds dependencies for
     pseudos which do not cross calls.  */
  old_max_uid = get_max_uid () + 1;

  h_i_d = xcalloc (old_max_uid, sizeof (*h_i_d));

  for (i = 0; i < old_max_uid; i++)
    h_i_d [i].cost = -1;

  if (targetm.sched.use_dfa_pipeline_interface
      && (*targetm.sched.use_dfa_pipeline_interface) ())
    {
      if (targetm.sched.init_dfa_pre_cycle_insn)
	(*targetm.sched.init_dfa_pre_cycle_insn) ();

      if (targetm.sched.init_dfa_post_cycle_insn)
	(*targetm.sched.init_dfa_post_cycle_insn) ();

      if (targetm.sched.first_cycle_multipass_dfa_lookahead
	  && targetm.sched.init_dfa_bubbles)
	(*targetm.sched.init_dfa_bubbles) ();

      dfa_start ();
      dfa_state_size = state_size ();
      curr_state = xmalloc (dfa_state_size);
    }

  h_i_d[0].luid = 0;
  luid = 1;
  FOR_EACH_BB (b)
    for (insn = BB_HEAD (b); ; insn = NEXT_INSN (insn))
      {
	INSN_LUID (insn) = luid;

	/* Increment the next luid, unless this is a note.  We don't
	   really need separate IDs for notes and we don't want to
	   schedule differently depending on whether or not there are
	   line-number notes, i.e., depending on whether or not we're
	   generating debugging information.  */
	if (GET_CODE (insn) != NOTE)
	  ++luid;

	if (insn == BB_END (b))
	  break;
      }

  init_dependency_caches (luid);

  init_alias_analysis ();

  if (write_symbols != NO_DEBUG)
    {
      rtx line;

      line_note_head = xcalloc (last_basic_block, sizeof (rtx));

      /* Save-line-note-head:
         Determine the line-number at the start of each basic block.
         This must be computed and saved now, because after a basic block's
         predecessor has been scheduled, it is impossible to accurately
         determine the correct line number for the first insn of the block.  */

      FOR_EACH_BB (b)
	{
	  for (line = BB_HEAD (b); line; line = PREV_INSN (line))
	    if (GET_CODE (line) == NOTE && NOTE_LINE_NUMBER (line) > 0)
	      {
		line_note_head[b->index] = line;
		break;
	      }
	  /* Do a forward search as well, since we won't get to see the first
	     notes in a basic block.  */
	  for (line = BB_HEAD (b); line; line = NEXT_INSN (line))
	    {
	      if (INSN_P (line))
		break;
	      if (GET_CODE (line) == NOTE && NOTE_LINE_NUMBER (line) > 0)
		line_note_head[b->index] = line;
	    }
	}
    }

  if ((!targetm.sched.use_dfa_pipeline_interface
       || !(*targetm.sched.use_dfa_pipeline_interface) ())
      && sched_verbose)
    /* Find units used in this function, for visualization.  */
    init_target_units ();

  /* ??? Add a NOTE after the last insn of the last basic block.  It is not
     known why this is done.  */

  insn = BB_END (EXIT_BLOCK_PTR->prev_bb);
  if (NEXT_INSN (insn) == 0
      || (GET_CODE (insn) != NOTE
	  && GET_CODE (insn) != CODE_LABEL
	  /* Don't emit a NOTE if it would end up before a BARRIER.  */
	  && GET_CODE (NEXT_INSN (insn)) != BARRIER))
    {
      emit_note_after (NOTE_INSN_DELETED, BB_END (EXIT_BLOCK_PTR->prev_bb));
      /* Make insn to appear outside BB.  */
      BB_END (EXIT_BLOCK_PTR->prev_bb) = PREV_INSN (BB_END (EXIT_BLOCK_PTR->prev_bb));
    }

  /* Compute INSN_REG_WEIGHT for all blocks.  We must do this before
     removing death notes.  */
  FOR_EACH_BB_REVERSE (b)
    find_insn_reg_weight (b->index);
}

/* Free global data used during insn scheduling.  */

void
sched_finish (void)
{
  free (h_i_d);

  if (targetm.sched.use_dfa_pipeline_interface
      && (*targetm.sched.use_dfa_pipeline_interface) ())
    {
      free (curr_state);
      dfa_finish ();
    }
  free_dependency_caches ();
  end_alias_analysis ();
  if (write_symbols != NO_DEBUG)
    free (line_note_head);
}
#endif /* INSN_SCHEDULING */
