/* Swing Modulo Scheduling implementation.
   Copyright (C) 2004-2014 Free Software Foundation, Inc.
   Contributed by Ayal Zaks and Mustafa Hagog <zaks,mustafa@il.ibm.com>

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


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "diagnostic-core.h"
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "regs.h"
#include "function.h"
#include "flags.h"
#include "insn-config.h"
#include "insn-attr.h"
#include "except.h"
#include "recog.h"
#include "sched-int.h"
#include "target.h"
#include "cfgloop.h"
#include "expr.h"
#include "params.h"
#include "gcov-io.h"
#include "ddg.h"
#include "tree-pass.h"
#include "dbgcnt.h"
#include "df.h"

#ifdef INSN_SCHEDULING

/* This file contains the implementation of the Swing Modulo Scheduler,
   described in the following references:
   [1] J. Llosa, A. Gonzalez, E. Ayguade, M. Valero., and J. Eckhardt.
       Lifetime--sensitive modulo scheduling in a production environment.
       IEEE Trans. on Comps., 50(3), March 2001
   [2] J. Llosa, A. Gonzalez, E. Ayguade, and M. Valero.
       Swing Modulo Scheduling: A Lifetime Sensitive Approach.
       PACT '96 , pages 80-87, October 1996 (Boston - Massachusetts - USA).

   The basic structure is:
   1. Build a data-dependence graph (DDG) for each loop.
   2. Use the DDG to order the insns of a loop (not in topological order
      necessarily, but rather) trying to place each insn after all its
      predecessors _or_ after all its successors.
   3. Compute MII: a lower bound on the number of cycles to schedule the loop.
   4. Use the ordering to perform list-scheduling of the loop:
      1. Set II = MII.  We will try to schedule the loop within II cycles.
      2. Try to schedule the insns one by one according to the ordering.
	 For each insn compute an interval of cycles by considering already-
	 scheduled preds and succs (and associated latencies); try to place
	 the insn in the cycles of this window checking for potential
	 resource conflicts (using the DFA interface).
	 Note: this is different from the cycle-scheduling of schedule_insns;
	 here the insns are not scheduled monotonically top-down (nor bottom-
	 up).
      3. If failed in scheduling all insns - bump II++ and try again, unless
	 II reaches an upper bound MaxII, in which case report failure.
   5. If we succeeded in scheduling the loop within II cycles, we now
      generate prolog and epilog, decrease the counter of the loop, and
      perform modulo variable expansion for live ranges that span more than
      II cycles (i.e. use register copies to prevent a def from overwriting
      itself before reaching the use).

    SMS works with countable loops (1) whose control part can be easily
    decoupled from the rest of the loop and (2) whose loop count can
    be easily adjusted.  This is because we peel a constant number of
    iterations into a prologue and epilogue for which we want to avoid
    emitting the control part, and a kernel which is to iterate that
    constant number of iterations less than the original loop.  So the
    control part should be a set of insns clearly identified and having
    its own iv, not otherwise used in the loop (at-least for now), which
    initializes a register before the loop to the number of iterations.
    Currently SMS relies on the do-loop pattern to recognize such loops,
    where (1) the control part comprises of all insns defining and/or
    using a certain 'count' register and (2) the loop count can be
    adjusted by modifying this register prior to the loop.
    TODO: Rely on cfgloop analysis instead.  */

/* This page defines partial-schedule structures and functions for
   modulo scheduling.  */

typedef struct partial_schedule *partial_schedule_ptr;
typedef struct ps_insn *ps_insn_ptr;

/* The minimum (absolute) cycle that a node of ps was scheduled in.  */
#define PS_MIN_CYCLE(ps) (((partial_schedule_ptr)(ps))->min_cycle)

/* The maximum (absolute) cycle that a node of ps was scheduled in.  */
#define PS_MAX_CYCLE(ps) (((partial_schedule_ptr)(ps))->max_cycle)

/* Perform signed modulo, always returning a non-negative value.  */
#define SMODULO(x,y) ((x) % (y) < 0 ? ((x) % (y) + (y)) : (x) % (y))

/* The number of different iterations the nodes in ps span, assuming
   the stage boundaries are placed efficiently.  */
#define CALC_STAGE_COUNT(max_cycle,min_cycle,ii) ((max_cycle - min_cycle \
                         + 1 + ii - 1) / ii)
/* The stage count of ps.  */
#define PS_STAGE_COUNT(ps) (((partial_schedule_ptr)(ps))->stage_count)

/* A single instruction in the partial schedule.  */
struct ps_insn
{
  /* Identifies the instruction to be scheduled.  Values smaller than
     the ddg's num_nodes refer directly to ddg nodes.  A value of
     X - num_nodes refers to register move X.  */
  int id;

  /* The (absolute) cycle in which the PS instruction is scheduled.
     Same as SCHED_TIME (node).  */
  int cycle;

  /* The next/prev PS_INSN in the same row.  */
  ps_insn_ptr next_in_row,
	      prev_in_row;

};

/* Information about a register move that has been added to a partial
   schedule.  */
struct ps_reg_move_info
{
  /* The source of the move is defined by the ps_insn with id DEF.
     The destination is used by the ps_insns with the ids in USES.  */
  int def;
  sbitmap uses;

  /* The original form of USES' instructions used OLD_REG, but they
     should now use NEW_REG.  */
  rtx old_reg;
  rtx new_reg;

  /* The number of consecutive stages that the move occupies.  */
  int num_consecutive_stages;

  /* An instruction that sets NEW_REG to the correct value.  The first
     move associated with DEF will have an rhs of OLD_REG; later moves
     use the result of the previous move.  */
  rtx insn;
};

typedef struct ps_reg_move_info ps_reg_move_info;

/* Holds the partial schedule as an array of II rows.  Each entry of the
   array points to a linked list of PS_INSNs, which represents the
   instructions that are scheduled for that row.  */
struct partial_schedule
{
  int ii;	/* Number of rows in the partial schedule.  */
  int history;  /* Threshold for conflict checking using DFA.  */

  /* rows[i] points to linked list of insns scheduled in row i (0<=i<ii).  */
  ps_insn_ptr *rows;

  /* All the moves added for this partial schedule.  Index X has
     a ps_insn id of X + g->num_nodes.  */
  vec<ps_reg_move_info> reg_moves;

  /*  rows_length[i] holds the number of instructions in the row.
      It is used only (as an optimization) to back off quickly from
      trying to schedule a node in a full row; that is, to avoid running
      through futile DFA state transitions.  */
  int *rows_length;
  
  /* The earliest absolute cycle of an insn in the partial schedule.  */
  int min_cycle;

  /* The latest absolute cycle of an insn in the partial schedule.  */
  int max_cycle;

  ddg_ptr g;	/* The DDG of the insns in the partial schedule.  */

  int stage_count;  /* The stage count of the partial schedule.  */
};


static partial_schedule_ptr create_partial_schedule (int ii, ddg_ptr, int history);
static void free_partial_schedule (partial_schedule_ptr);
static void reset_partial_schedule (partial_schedule_ptr, int new_ii);
void print_partial_schedule (partial_schedule_ptr, FILE *);
static void verify_partial_schedule (partial_schedule_ptr, sbitmap);
static ps_insn_ptr ps_add_node_check_conflicts (partial_schedule_ptr,
						int, int, sbitmap, sbitmap);
static void rotate_partial_schedule (partial_schedule_ptr, int);
void set_row_column_for_ps (partial_schedule_ptr);
static void ps_insert_empty_row (partial_schedule_ptr, int, sbitmap);
static int compute_split_row (sbitmap, int, int, int, ddg_node_ptr);


/* This page defines constants and structures for the modulo scheduling
   driver.  */

static int sms_order_nodes (ddg_ptr, int, int *, int *);
static void set_node_sched_params (ddg_ptr);
static partial_schedule_ptr sms_schedule_by_order (ddg_ptr, int, int, int *);
static void permute_partial_schedule (partial_schedule_ptr, rtx);
static void generate_prolog_epilog (partial_schedule_ptr, struct loop *,
                                    rtx, rtx);
static int calculate_stage_count (partial_schedule_ptr, int);
static void calculate_must_precede_follow (ddg_node_ptr, int, int,
					   int, int, sbitmap, sbitmap, sbitmap);
static int get_sched_window (partial_schedule_ptr, ddg_node_ptr, 
			     sbitmap, int, int *, int *, int *);
static bool try_scheduling_node_in_cycle (partial_schedule_ptr, int, int,
					  sbitmap, int *, sbitmap, sbitmap);
static void remove_node_from_ps (partial_schedule_ptr, ps_insn_ptr);

#define NODE_ASAP(node) ((node)->aux.count)

#define SCHED_PARAMS(x) (&node_sched_param_vec[x])
#define SCHED_TIME(x) (SCHED_PARAMS (x)->time)
#define SCHED_ROW(x) (SCHED_PARAMS (x)->row)
#define SCHED_STAGE(x) (SCHED_PARAMS (x)->stage)
#define SCHED_COLUMN(x) (SCHED_PARAMS (x)->column)

/* The scheduling parameters held for each node.  */
typedef struct node_sched_params
{
  int time;	/* The absolute scheduling cycle.  */

  int row;    /* Holds time % ii.  */
  int stage;  /* Holds time / ii.  */

  /* The column of a node inside the ps.  If nodes u, v are on the same row,
     u will precede v if column (u) < column (v).  */
  int column;
} *node_sched_params_ptr;

typedef struct node_sched_params node_sched_params;

/* The following three functions are copied from the current scheduler
   code in order to use sched_analyze() for computing the dependencies.
   They are used when initializing the sched_info structure.  */
static const char *
sms_print_insn (const_rtx insn, int aligned ATTRIBUTE_UNUSED)
{
  static char tmp[80];

  sprintf (tmp, "i%4d", INSN_UID (insn));
  return tmp;
}

static void
compute_jump_reg_dependencies (rtx insn ATTRIBUTE_UNUSED,
			       regset used ATTRIBUTE_UNUSED)
{
}

static struct common_sched_info_def sms_common_sched_info;

static struct sched_deps_info_def sms_sched_deps_info =
  {
    compute_jump_reg_dependencies,
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
    NULL,
    0, 0, 0
  };

static struct haifa_sched_info sms_sched_info =
{
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  sms_print_insn,
  NULL,
  NULL, /* insn_finishes_block_p */
  NULL, NULL,
  NULL, NULL,
  0, 0,

  NULL, NULL, NULL, NULL,
  NULL, NULL,
  0
};

/* Partial schedule instruction ID in PS is a register move.  Return
   information about it.  */
static struct ps_reg_move_info *
ps_reg_move (partial_schedule_ptr ps, int id)
{
  gcc_checking_assert (id >= ps->g->num_nodes);
  return &ps->reg_moves[id - ps->g->num_nodes];
}

/* Return the rtl instruction that is being scheduled by partial schedule
   instruction ID, which belongs to schedule PS.  */
static rtx
ps_rtl_insn (partial_schedule_ptr ps, int id)
{
  if (id < ps->g->num_nodes)
    return ps->g->nodes[id].insn;
  else
    return ps_reg_move (ps, id)->insn;
}

/* Partial schedule instruction ID, which belongs to PS, occurred in
   the original (unscheduled) loop.  Return the first instruction
   in the loop that was associated with ps_rtl_insn (PS, ID).
   If the instruction had some notes before it, this is the first
   of those notes.  */
static rtx
ps_first_note (partial_schedule_ptr ps, int id)
{
  gcc_assert (id < ps->g->num_nodes);
  return ps->g->nodes[id].first_note;
}

/* Return the number of consecutive stages that are occupied by
   partial schedule instruction ID in PS.  */
static int
ps_num_consecutive_stages (partial_schedule_ptr ps, int id)
{
  if (id < ps->g->num_nodes)
    return 1;
  else
    return ps_reg_move (ps, id)->num_consecutive_stages;
}

/* Given HEAD and TAIL which are the first and last insns in a loop;
   return the register which controls the loop.  Return zero if it has
   more than one occurrence in the loop besides the control part or the
   do-loop pattern is not of the form we expect.  */
static rtx
doloop_register_get (rtx head ATTRIBUTE_UNUSED, rtx tail ATTRIBUTE_UNUSED)
{
#ifdef HAVE_doloop_end
  rtx reg, condition, insn, first_insn_not_to_check;

  if (!JUMP_P (tail))
    return NULL_RTX;

  /* TODO: Free SMS's dependence on doloop_condition_get.  */
  condition = doloop_condition_get (tail);
  if (! condition)
    return NULL_RTX;

  if (REG_P (XEXP (condition, 0)))
    reg = XEXP (condition, 0);
  else if (GET_CODE (XEXP (condition, 0)) == PLUS
	   && REG_P (XEXP (XEXP (condition, 0), 0)))
    reg = XEXP (XEXP (condition, 0), 0);
  else
    gcc_unreachable ();

  /* Check that the COUNT_REG has no other occurrences in the loop
     until the decrement.  We assume the control part consists of
     either a single (parallel) branch-on-count or a (non-parallel)
     branch immediately preceded by a single (decrement) insn.  */
  first_insn_not_to_check = (GET_CODE (PATTERN (tail)) == PARALLEL ? tail
                             : prev_nondebug_insn (tail));

  for (insn = head; insn != first_insn_not_to_check; insn = NEXT_INSN (insn))
    if (!DEBUG_INSN_P (insn) && reg_mentioned_p (reg, insn))
      {
        if (dump_file)
        {
          fprintf (dump_file, "SMS count_reg found ");
          print_rtl_single (dump_file, reg);
          fprintf (dump_file, " outside control in insn:\n");
          print_rtl_single (dump_file, insn);
        }

        return NULL_RTX;
      }

  return reg;
#else
  return NULL_RTX;
#endif
}

/* Check if COUNT_REG is set to a constant in the PRE_HEADER block, so
   that the number of iterations is a compile-time constant.  If so,
   return the rtx that sets COUNT_REG to a constant, and set COUNT to
   this constant.  Otherwise return 0.  */
static rtx
const_iteration_count (rtx count_reg, basic_block pre_header,
		       HOST_WIDEST_INT * count)
{
  rtx insn;
  rtx head, tail;

  if (! pre_header)
    return NULL_RTX;

  get_ebb_head_tail (pre_header, pre_header, &head, &tail);

  for (insn = tail; insn != PREV_INSN (head); insn = PREV_INSN (insn))
    if (NONDEBUG_INSN_P (insn) && single_set (insn) &&
	rtx_equal_p (count_reg, SET_DEST (single_set (insn))))
      {
	rtx pat = single_set (insn);

	if (CONST_INT_P (SET_SRC (pat)))
	  {
	    *count = INTVAL (SET_SRC (pat));
	    return insn;
	  }

	return NULL_RTX;
      }

  return NULL_RTX;
}

/* A very simple resource-based lower bound on the initiation interval.
   ??? Improve the accuracy of this bound by considering the
   utilization of various units.  */
static int
res_MII (ddg_ptr g)
{
  if (targetm.sched.sms_res_mii)
    return targetm.sched.sms_res_mii (g);

  return ((g->num_nodes - g->num_debug) / issue_rate);
}


/* A vector that contains the sched data for each ps_insn.  */
static vec<node_sched_params> node_sched_param_vec;

/* Allocate sched_params for each node and initialize it.  */
static void
set_node_sched_params (ddg_ptr g)
{
  node_sched_param_vec.truncate (0);
  node_sched_param_vec.safe_grow_cleared (g->num_nodes);
}

/* Make sure that node_sched_param_vec has an entry for every move in PS.  */
static void
extend_node_sched_params (partial_schedule_ptr ps)
{
  node_sched_param_vec.safe_grow_cleared (ps->g->num_nodes
					  + ps->reg_moves.length ());
}

/* Update the sched_params (time, row and stage) for node U using the II,
   the CYCLE of U and MIN_CYCLE.
   We're not simply taking the following
   SCHED_STAGE (u) = CALC_STAGE_COUNT (SCHED_TIME (u), min_cycle, ii);
   because the stages may not be aligned on cycle 0.  */
static void
update_node_sched_params (int u, int ii, int cycle, int min_cycle)
{
  int sc_until_cycle_zero;
  int stage;

  SCHED_TIME (u) = cycle;
  SCHED_ROW (u) = SMODULO (cycle, ii);

  /* The calculation of stage count is done adding the number
     of stages before cycle zero and after cycle zero.  */
  sc_until_cycle_zero = CALC_STAGE_COUNT (-1, min_cycle, ii);

  if (SCHED_TIME (u) < 0)
    {
      stage = CALC_STAGE_COUNT (-1, SCHED_TIME (u), ii);
      SCHED_STAGE (u) = sc_until_cycle_zero - stage;
    }
  else
    {
      stage = CALC_STAGE_COUNT (SCHED_TIME (u), 0, ii);
      SCHED_STAGE (u) = sc_until_cycle_zero + stage - 1;
    }
}

static void
print_node_sched_params (FILE *file, int num_nodes, partial_schedule_ptr ps)
{
  int i;

  if (! file)
    return;
  for (i = 0; i < num_nodes; i++)
    {
      node_sched_params_ptr nsp = SCHED_PARAMS (i);

      fprintf (file, "Node = %d; INSN = %d\n", i,
	       INSN_UID (ps_rtl_insn (ps, i)));
      fprintf (file, " asap = %d:\n", NODE_ASAP (&ps->g->nodes[i]));
      fprintf (file, " time = %d:\n", nsp->time);
      fprintf (file, " stage = %d:\n", nsp->stage);
    }
}

/* Set SCHED_COLUMN for each instruction in row ROW of PS.  */
static void
set_columns_for_row (partial_schedule_ptr ps, int row)
{
  ps_insn_ptr cur_insn;
  int column;

  column = 0;
  for (cur_insn = ps->rows[row]; cur_insn; cur_insn = cur_insn->next_in_row)
    SCHED_COLUMN (cur_insn->id) = column++;
}

/* Set SCHED_COLUMN for each instruction in PS.  */
static void
set_columns_for_ps (partial_schedule_ptr ps)
{
  int row;

  for (row = 0; row < ps->ii; row++)
    set_columns_for_row (ps, row);
}

/* Try to schedule the move with ps_insn identifier I_REG_MOVE in PS.
   Its single predecessor has already been scheduled, as has its
   ddg node successors.  (The move may have also another move as its
   successor, in which case that successor will be scheduled later.)

   The move is part of a chain that satisfies register dependencies
   between a producing ddg node and various consuming ddg nodes.
   If some of these dependencies have a distance of 1 (meaning that
   the use is upward-exposed) then DISTANCE1_USES is nonnull and
   contains the set of uses with distance-1 dependencies.
   DISTANCE1_USES is null otherwise.

   MUST_FOLLOW is a scratch bitmap that is big enough to hold
   all current ps_insn ids.

   Return true on success.  */
static bool
schedule_reg_move (partial_schedule_ptr ps, int i_reg_move,
		   sbitmap distance1_uses, sbitmap must_follow)
{
  unsigned int u;
  int this_time, this_distance, this_start, this_end, this_latency;
  int start, end, c, ii;
  sbitmap_iterator sbi;
  ps_reg_move_info *move;
  rtx this_insn;
  ps_insn_ptr psi;

  move = ps_reg_move (ps, i_reg_move);
  ii = ps->ii;
  if (dump_file)
    {
      fprintf (dump_file, "Scheduling register move INSN %d; ii = %d"
	       ", min cycle = %d\n\n", INSN_UID (move->insn), ii,
	       PS_MIN_CYCLE (ps));
      print_rtl_single (dump_file, move->insn);
      fprintf (dump_file, "\n%11s %11s %5s\n", "start", "end", "time");
      fprintf (dump_file, "=========== =========== =====\n");
    }

  start = INT_MIN;
  end = INT_MAX;

  /* For dependencies of distance 1 between a producer ddg node A
     and consumer ddg node B, we have a chain of dependencies:

        A --(T,L1,1)--> M1 --(T,L2,0)--> M2 ... --(T,Ln,0)--> B

     where Mi is the ith move.  For dependencies of distance 0 between
     a producer ddg node A and consumer ddg node C, we have a chain of
     dependencies:

        A --(T,L1',0)--> M1' --(T,L2',0)--> M2' ... --(T,Ln',0)--> C

     where Mi' occupies the same position as Mi but occurs a stage later.
     We can only schedule each move once, so if we have both types of
     chain, we model the second as:

        A --(T,L1',1)--> M1 --(T,L2',0)--> M2 ... --(T,Ln',-1)--> C

     First handle the dependencies between the previously-scheduled
     predecessor and the move.  */
  this_insn = ps_rtl_insn (ps, move->def);
  this_latency = insn_latency (this_insn, move->insn);
  this_distance = distance1_uses && move->def < ps->g->num_nodes ? 1 : 0;
  this_time = SCHED_TIME (move->def) - this_distance * ii;
  this_start = this_time + this_latency;
  this_end = this_time + ii;
  if (dump_file)
    fprintf (dump_file, "%11d %11d %5d %d --(T,%d,%d)--> %d\n",
	     this_start, this_end, SCHED_TIME (move->def),
	     INSN_UID (this_insn), this_latency, this_distance,
	     INSN_UID (move->insn));

  if (start < this_start)
    start = this_start;
  if (end > this_end)
    end = this_end;

  /* Handle the dependencies between the move and previously-scheduled
     successors.  */
  EXECUTE_IF_SET_IN_BITMAP (move->uses, 0, u, sbi)
    {
      this_insn = ps_rtl_insn (ps, u);
      this_latency = insn_latency (move->insn, this_insn);
      if (distance1_uses && !bitmap_bit_p (distance1_uses, u))
	this_distance = -1;
      else
	this_distance = 0;
      this_time = SCHED_TIME (u) + this_distance * ii;
      this_start = this_time - ii;
      this_end = this_time - this_latency;
      if (dump_file)
	fprintf (dump_file, "%11d %11d %5d %d --(T,%d,%d)--> %d\n",
		 this_start, this_end, SCHED_TIME (u), INSN_UID (move->insn),
		 this_latency, this_distance, INSN_UID (this_insn));

      if (start < this_start)
	start = this_start;
      if (end > this_end)
	end = this_end;
    }

  if (dump_file)
    {
      fprintf (dump_file, "----------- ----------- -----\n");
      fprintf (dump_file, "%11d %11d %5s %s\n", start, end, "", "(max, min)");
    }

  bitmap_clear (must_follow);
  bitmap_set_bit (must_follow, move->def);

  start = MAX (start, end - (ii - 1));
  for (c = end; c >= start; c--)
    {
      psi = ps_add_node_check_conflicts (ps, i_reg_move, c,
					 move->uses, must_follow);
      if (psi)
	{
	  update_node_sched_params (i_reg_move, ii, c, PS_MIN_CYCLE (ps));
	  if (dump_file)
	    fprintf (dump_file, "\nScheduled register move INSN %d at"
		     " time %d, row %d\n\n", INSN_UID (move->insn), c,
		     SCHED_ROW (i_reg_move));
	  return true;
	}
    }

  if (dump_file)
    fprintf (dump_file, "\nNo available slot\n\n");

  return false;
}

/*
   Breaking intra-loop register anti-dependences:
   Each intra-loop register anti-dependence implies a cross-iteration true
   dependence of distance 1. Therefore, we can remove such false dependencies
   and figure out if the partial schedule broke them by checking if (for a
   true-dependence of distance 1): SCHED_TIME (def) < SCHED_TIME (use) and
   if so generate a register move.   The number of such moves is equal to:
              SCHED_TIME (use) - SCHED_TIME (def)       { 0 broken
   nreg_moves = ----------------------------------- + 1 - {   dependence.
                            ii                          { 1 if not.
*/
static bool
schedule_reg_moves (partial_schedule_ptr ps)
{
  ddg_ptr g = ps->g;
  int ii = ps->ii;
  int i;

  for (i = 0; i < g->num_nodes; i++)
    {
      ddg_node_ptr u = &g->nodes[i];
      ddg_edge_ptr e;
      int nreg_moves = 0, i_reg_move;
      rtx prev_reg, old_reg;
      int first_move;
      int distances[2];
      sbitmap must_follow;
      sbitmap distance1_uses;
      rtx set = single_set (u->insn);
      
      /* Skip instructions that do not set a register.  */
      if ((set && !REG_P (SET_DEST (set))))
        continue;
 
      /* Compute the number of reg_moves needed for u, by looking at life
	 ranges started at u (excluding self-loops).  */
      distances[0] = distances[1] = false;
      for (e = u->out; e; e = e->next_out)
	if (e->type == TRUE_DEP && e->dest != e->src)
	  {
	    int nreg_moves4e = (SCHED_TIME (e->dest->cuid)
				- SCHED_TIME (e->src->cuid)) / ii;

            if (e->distance == 1)
              nreg_moves4e = (SCHED_TIME (e->dest->cuid)
			      - SCHED_TIME (e->src->cuid) + ii) / ii;

	    /* If dest precedes src in the schedule of the kernel, then dest
	       will read before src writes and we can save one reg_copy.  */
	    if (SCHED_ROW (e->dest->cuid) == SCHED_ROW (e->src->cuid)
		&& SCHED_COLUMN (e->dest->cuid) < SCHED_COLUMN (e->src->cuid))
	      nreg_moves4e--;

            if (nreg_moves4e >= 1)
	      {
		/* !single_set instructions are not supported yet and
		   thus we do not except to encounter them in the loop
		   except from the doloop part.  For the latter case
		   we assume no regmoves are generated as the doloop
		   instructions are tied to the branch with an edge.  */
		gcc_assert (set);
		/* If the instruction contains auto-inc register then
		   validate that the regmov is being generated for the
		   target regsiter rather then the inc'ed register.	*/
		gcc_assert (!autoinc_var_is_used_p (u->insn, e->dest->insn));
	      }
	    
	    if (nreg_moves4e)
	      {
		gcc_assert (e->distance < 2);
		distances[e->distance] = true;
	      }
	    nreg_moves = MAX (nreg_moves, nreg_moves4e);
	  }

      if (nreg_moves == 0)
	continue;

      /* Create NREG_MOVES register moves.  */
      first_move = ps->reg_moves.length ();
      ps->reg_moves.safe_grow_cleared (first_move + nreg_moves);
      extend_node_sched_params (ps);

      /* Record the moves associated with this node.  */
      first_move += ps->g->num_nodes;

      /* Generate each move.  */
      old_reg = prev_reg = SET_DEST (single_set (u->insn));
      for (i_reg_move = 0; i_reg_move < nreg_moves; i_reg_move++)
	{
	  ps_reg_move_info *move = ps_reg_move (ps, first_move + i_reg_move);

	  move->def = i_reg_move > 0 ? first_move + i_reg_move - 1 : i;
	  move->uses = sbitmap_alloc (first_move + nreg_moves);
	  move->old_reg = old_reg;
	  move->new_reg = gen_reg_rtx (GET_MODE (prev_reg));
	  move->num_consecutive_stages = distances[0] && distances[1] ? 2 : 1;
	  move->insn = gen_move_insn (move->new_reg, copy_rtx (prev_reg));
	  bitmap_clear (move->uses);

	  prev_reg = move->new_reg;
	}

      distance1_uses = distances[1] ? sbitmap_alloc (g->num_nodes) : NULL;

      /* Every use of the register defined by node may require a different
	 copy of this register, depending on the time the use is scheduled.
	 Record which uses require which move results.  */
      for (e = u->out; e; e = e->next_out)
	if (e->type == TRUE_DEP && e->dest != e->src)
	  {
	    int dest_copy = (SCHED_TIME (e->dest->cuid)
			     - SCHED_TIME (e->src->cuid)) / ii;

	    if (e->distance == 1)
	      dest_copy = (SCHED_TIME (e->dest->cuid)
			   - SCHED_TIME (e->src->cuid) + ii) / ii;

	    if (SCHED_ROW (e->dest->cuid) == SCHED_ROW (e->src->cuid)
		&& SCHED_COLUMN (e->dest->cuid) < SCHED_COLUMN (e->src->cuid))
	      dest_copy--;

	    if (dest_copy)
	      {
		ps_reg_move_info *move;

		move = ps_reg_move (ps, first_move + dest_copy - 1);
		bitmap_set_bit (move->uses, e->dest->cuid);
		if (e->distance == 1)
		  bitmap_set_bit (distance1_uses, e->dest->cuid);
	      }
	  }

      must_follow = sbitmap_alloc (first_move + nreg_moves);
      for (i_reg_move = 0; i_reg_move < nreg_moves; i_reg_move++)
	if (!schedule_reg_move (ps, first_move + i_reg_move,
				distance1_uses, must_follow))
	  break;
      sbitmap_free (must_follow);
      if (distance1_uses)
	sbitmap_free (distance1_uses);
      if (i_reg_move < nreg_moves)
	return false;
    }
  return true;
}

/* Emit the moves associatied with PS.  Apply the substitutions
   associated with them.  */
static void
apply_reg_moves (partial_schedule_ptr ps)
{
  ps_reg_move_info *move;
  int i;

  FOR_EACH_VEC_ELT (ps->reg_moves, i, move)
    {
      unsigned int i_use;
      sbitmap_iterator sbi;

      EXECUTE_IF_SET_IN_BITMAP (move->uses, 0, i_use, sbi)
	{
	  replace_rtx (ps->g->nodes[i_use].insn, move->old_reg, move->new_reg);
	  df_insn_rescan (ps->g->nodes[i_use].insn);
	}
    }
}

/* Bump the SCHED_TIMEs of all nodes by AMOUNT.  Set the values of
   SCHED_ROW and SCHED_STAGE.  Instruction scheduled on cycle AMOUNT
   will move to cycle zero.  */
static void
reset_sched_times (partial_schedule_ptr ps, int amount)
{
  int row;
  int ii = ps->ii;
  ps_insn_ptr crr_insn;

  for (row = 0; row < ii; row++)
    for (crr_insn = ps->rows[row]; crr_insn; crr_insn = crr_insn->next_in_row)
      {
	int u = crr_insn->id;
	int normalized_time = SCHED_TIME (u) - amount;
	int new_min_cycle = PS_MIN_CYCLE (ps) - amount;

        if (dump_file)
          {
            /* Print the scheduling times after the rotation.  */
	    rtx insn = ps_rtl_insn (ps, u);

            fprintf (dump_file, "crr_insn->node=%d (insn id %d), "
                     "crr_insn->cycle=%d, min_cycle=%d", u,
                     INSN_UID (insn), normalized_time, new_min_cycle);
            if (JUMP_P (insn))
              fprintf (dump_file, " (branch)");
            fprintf (dump_file, "\n");
          }
	
	gcc_assert (SCHED_TIME (u) >= ps->min_cycle);
	gcc_assert (SCHED_TIME (u) <= ps->max_cycle);

	crr_insn->cycle = normalized_time;
	update_node_sched_params (u, ii, normalized_time, new_min_cycle);
      }
}
 
/* Permute the insns according to their order in PS, from row 0 to
   row ii-1, and position them right before LAST.  This schedules
   the insns of the loop kernel.  */
static void
permute_partial_schedule (partial_schedule_ptr ps, rtx last)
{
  int ii = ps->ii;
  int row;
  ps_insn_ptr ps_ij;

  for (row = 0; row < ii ; row++)
    for (ps_ij = ps->rows[row]; ps_ij; ps_ij = ps_ij->next_in_row)
      {
	rtx insn = ps_rtl_insn (ps, ps_ij->id);

	if (PREV_INSN (last) != insn)
	  {
	    if (ps_ij->id < ps->g->num_nodes)
	      reorder_insns_nobb (ps_first_note (ps, ps_ij->id), insn,
				  PREV_INSN (last));
	    else
	      add_insn_before (insn, last, NULL);
	  }
      }
}

/* Set bitmaps TMP_FOLLOW and TMP_PRECEDE to MUST_FOLLOW and MUST_PRECEDE
   respectively only if cycle C falls on the border of the scheduling
   window boundaries marked by START and END cycles.  STEP is the
   direction of the window.  */
static inline void
set_must_precede_follow (sbitmap *tmp_follow, sbitmap must_follow,
			 sbitmap *tmp_precede, sbitmap must_precede, int c,
			 int start, int end, int step)
{
  *tmp_precede = NULL;
  *tmp_follow = NULL;

  if (c == start)
    {
      if (step == 1)
	*tmp_precede = must_precede;
      else			/* step == -1.  */
	*tmp_follow = must_follow;
    }
  if (c == end - step)
    {
      if (step == 1)
	*tmp_follow = must_follow;
      else			/* step == -1.  */
	*tmp_precede = must_precede;
    }

}

/* Return True if the branch can be moved to row ii-1 while
   normalizing the partial schedule PS to start from cycle zero and thus
   optimize the SC.  Otherwise return False.  */
static bool
optimize_sc (partial_schedule_ptr ps, ddg_ptr g)
{
  int amount = PS_MIN_CYCLE (ps);
  sbitmap sched_nodes = sbitmap_alloc (g->num_nodes);
  int start, end, step;
  int ii = ps->ii;
  bool ok = false;
  int stage_count, stage_count_curr;

  /* Compare the SC after normalization and SC after bringing the branch
     to row ii-1.  If they are equal just bail out.  */
  stage_count = calculate_stage_count (ps, amount);
  stage_count_curr =
    calculate_stage_count (ps, SCHED_TIME (g->closing_branch->cuid) - (ii - 1));

  if (stage_count == stage_count_curr)
    {
      if (dump_file)
	fprintf (dump_file, "SMS SC already optimized.\n");

      ok = false;
      goto clear;
    }

  if (dump_file)
    {
      fprintf (dump_file, "SMS Trying to optimize branch location\n");
      fprintf (dump_file, "SMS partial schedule before trial:\n");
      print_partial_schedule (ps, dump_file);
    }

  /* First, normalize the partial scheduling.  */
  reset_sched_times (ps, amount);
  rotate_partial_schedule (ps, amount);
  if (dump_file)
    {
      fprintf (dump_file,
	       "SMS partial schedule after normalization (ii, %d, SC %d):\n",
	       ii, stage_count);
      print_partial_schedule (ps, dump_file);
    }

  if (SMODULO (SCHED_TIME (g->closing_branch->cuid), ii) == ii - 1)
    {
      ok = true;
      goto clear;
    }

  bitmap_ones (sched_nodes);

  /* Calculate the new placement of the branch.  It should be in row
     ii-1 and fall into it's scheduling window.  */
  if (get_sched_window (ps, g->closing_branch, sched_nodes, ii, &start,
			&step, &end) == 0)
    {
      bool success;
      ps_insn_ptr next_ps_i;
      int branch_cycle = SCHED_TIME (g->closing_branch->cuid);
      int row = SMODULO (branch_cycle, ps->ii);
      int num_splits = 0;
      sbitmap must_precede, must_follow, tmp_precede, tmp_follow;
      int c;

      if (dump_file)
	fprintf (dump_file, "\nTrying to schedule node %d "
		 "INSN = %d  in (%d .. %d) step %d\n",
		 g->closing_branch->cuid,
		 (INSN_UID (g->closing_branch->insn)), start, end, step);

      gcc_assert ((step > 0 && start < end) || (step < 0 && start > end));
      if (step == 1)
	{
	  c = start + ii - SMODULO (start, ii) - 1;
	  gcc_assert (c >= start);
	  if (c >= end)
	    {
	      ok = false;
	      if (dump_file)
		fprintf (dump_file,
			 "SMS failed to schedule branch at cycle: %d\n", c);
	      goto clear;
	    }
	}
      else
	{
	  c = start - SMODULO (start, ii) - 1;
	  gcc_assert (c <= start);

	  if (c <= end)
	    {
	      if (dump_file)
		fprintf (dump_file,
			 "SMS failed to schedule branch at cycle: %d\n", c);
	      ok = false;
	      goto clear;
	    }
	}

      must_precede = sbitmap_alloc (g->num_nodes);
      must_follow = sbitmap_alloc (g->num_nodes);

      /* Try to schedule the branch is it's new cycle.  */
      calculate_must_precede_follow (g->closing_branch, start, end,
				     step, ii, sched_nodes,
				     must_precede, must_follow);

      set_must_precede_follow (&tmp_follow, must_follow, &tmp_precede,
			       must_precede, c, start, end, step);

      /* Find the element in the partial schedule related to the closing
         branch so we can remove it from it's current cycle.  */
      for (next_ps_i = ps->rows[row];
	   next_ps_i; next_ps_i = next_ps_i->next_in_row)
	if (next_ps_i->id == g->closing_branch->cuid)
	  break;

      remove_node_from_ps (ps, next_ps_i);
      success =
	try_scheduling_node_in_cycle (ps, g->closing_branch->cuid, c,
				      sched_nodes, &num_splits,
				      tmp_precede, tmp_follow);
      gcc_assert (num_splits == 0);
      if (!success)
	{
	  if (dump_file)
	    fprintf (dump_file,
		     "SMS failed to schedule branch at cycle: %d, "
		     "bringing it back to cycle %d\n", c, branch_cycle);

	  /* The branch was failed to be placed in row ii - 1.
	     Put it back in it's original place in the partial
	     schedualing.  */
	  set_must_precede_follow (&tmp_follow, must_follow, &tmp_precede,
				   must_precede, branch_cycle, start, end,
				   step);
	  success =
	    try_scheduling_node_in_cycle (ps, g->closing_branch->cuid,
					  branch_cycle, sched_nodes,
					  &num_splits, tmp_precede,
					  tmp_follow);
	  gcc_assert (success && (num_splits == 0));
	  ok = false;
	}
      else
	{
	  /* The branch is placed in row ii - 1.  */
	  if (dump_file)
	    fprintf (dump_file,
		     "SMS success in moving branch to cycle %d\n", c);

	  update_node_sched_params (g->closing_branch->cuid, ii, c,
				    PS_MIN_CYCLE (ps));
	  ok = true;
	}

      free (must_precede);
      free (must_follow);
    }

clear:
  free (sched_nodes);
  return ok;
}

static void
duplicate_insns_of_cycles (partial_schedule_ptr ps, int from_stage,
			   int to_stage, rtx count_reg)
{
  int row;
  ps_insn_ptr ps_ij;

  for (row = 0; row < ps->ii; row++)
    for (ps_ij = ps->rows[row]; ps_ij; ps_ij = ps_ij->next_in_row)
      {
	int u = ps_ij->id;
	int first_u, last_u;
	rtx u_insn;

        /* Do not duplicate any insn which refers to count_reg as it
           belongs to the control part.
           The closing branch is scheduled as well and thus should
           be ignored.
           TODO: This should be done by analyzing the control part of
           the loop.  */
	u_insn = ps_rtl_insn (ps, u);
        if (reg_mentioned_p (count_reg, u_insn)
            || JUMP_P (u_insn))
          continue;

	first_u = SCHED_STAGE (u);
	last_u = first_u + ps_num_consecutive_stages (ps, u) - 1;
	if (from_stage <= last_u && to_stage >= first_u)
	  {
	    if (u < ps->g->num_nodes)
	      duplicate_insn_chain (ps_first_note (ps, u), u_insn);
	    else
	      emit_insn (copy_rtx (PATTERN (u_insn)));
	  }
      }
}


/* Generate the instructions (including reg_moves) for prolog & epilog.  */
static void
generate_prolog_epilog (partial_schedule_ptr ps, struct loop *loop,
                        rtx count_reg, rtx count_init)
{
  int i;
  int last_stage = PS_STAGE_COUNT (ps) - 1;
  edge e;

  /* Generate the prolog, inserting its insns on the loop-entry edge.  */
  start_sequence ();

  if (!count_init)
    {
      /* Generate instructions at the beginning of the prolog to
         adjust the loop count by STAGE_COUNT.  If loop count is constant
         (count_init), this constant is adjusted by STAGE_COUNT in
         generate_prolog_epilog function.  */
      rtx sub_reg = NULL_RTX;

      sub_reg = expand_simple_binop (GET_MODE (count_reg), MINUS, count_reg,
				     gen_int_mode (last_stage,
						   GET_MODE (count_reg)),
                                     count_reg, 1, OPTAB_DIRECT);
      gcc_assert (REG_P (sub_reg));
      if (REGNO (sub_reg) != REGNO (count_reg))
        emit_move_insn (count_reg, sub_reg);
    }

  for (i = 0; i < last_stage; i++)
    duplicate_insns_of_cycles (ps, 0, i, count_reg);

  /* Put the prolog on the entry edge.  */
  e = loop_preheader_edge (loop);
  split_edge_and_insert (e, get_insns ());
  if (!flag_resched_modulo_sched)
    e->dest->flags |= BB_DISABLE_SCHEDULE;

  end_sequence ();

  /* Generate the epilog, inserting its insns on the loop-exit edge.  */
  start_sequence ();

  for (i = 0; i < last_stage; i++)
    duplicate_insns_of_cycles (ps, i + 1, last_stage, count_reg);

  /* Put the epilogue on the exit edge.  */
  gcc_assert (single_exit (loop));
  e = single_exit (loop);
  split_edge_and_insert (e, get_insns ());
  if (!flag_resched_modulo_sched)
    e->dest->flags |= BB_DISABLE_SCHEDULE;

  end_sequence ();
}

/* Mark LOOP as software pipelined so the later
   scheduling passes don't touch it.  */
static void
mark_loop_unsched (struct loop *loop)
{
  unsigned i;
  basic_block *bbs = get_loop_body (loop);

  for (i = 0; i < loop->num_nodes; i++)
    bbs[i]->flags |= BB_DISABLE_SCHEDULE;

  free (bbs);
}

/* Return true if all the BBs of the loop are empty except the
   loop header.  */
static bool
loop_single_full_bb_p (struct loop *loop)
{
  unsigned i;
  basic_block *bbs = get_loop_body (loop);

  for (i = 0; i < loop->num_nodes ; i++)
    {
      rtx head, tail;
      bool empty_bb = true;

      if (bbs[i] == loop->header)
        continue;

      /* Make sure that basic blocks other than the header
         have only notes labels or jumps.  */
      get_ebb_head_tail (bbs[i], bbs[i], &head, &tail);
      for (; head != NEXT_INSN (tail); head = NEXT_INSN (head))
        {
          if (NOTE_P (head) || LABEL_P (head)
 	      || (INSN_P (head) && (DEBUG_INSN_P (head) || JUMP_P (head))))
 	    continue;
 	  empty_bb = false;
 	  break;
        }

      if (! empty_bb)
        {
          free (bbs);
          return false;
        }
    }
  free (bbs);
  return true;
}

/* Dump file:line from INSN's location info to dump_file.  */

static void
dump_insn_location (rtx insn)
{
  if (dump_file && INSN_LOCATION (insn))
    {
      const char *file = insn_file (insn);
      if (file)
	fprintf (dump_file, " %s:%i", file, insn_line (insn));
    }
}

/* A simple loop from SMS point of view; it is a loop that is composed of
   either a single basic block or two BBs - a header and a latch.  */
#define SIMPLE_SMS_LOOP_P(loop) ((loop->num_nodes < 3 ) 		    \
				  && (EDGE_COUNT (loop->latch->preds) == 1) \
                                  && (EDGE_COUNT (loop->latch->succs) == 1))

/* Return true if the loop is in its canonical form and false if not.
   i.e. SIMPLE_SMS_LOOP_P and have one preheader block, and single exit.  */
static bool
loop_canon_p (struct loop *loop)
{

  if (loop->inner || !loop_outer (loop))
  {
    if (dump_file)
      fprintf (dump_file, "SMS loop inner or !loop_outer\n");
    return false;
  }

  if (!single_exit (loop))
    {
      if (dump_file)
	{
	  rtx insn = BB_END (loop->header);

	  fprintf (dump_file, "SMS loop many exits");
	  dump_insn_location (insn);
	  fprintf (dump_file, "\n");
	}
      return false;
    }

  if (! SIMPLE_SMS_LOOP_P (loop) && ! loop_single_full_bb_p (loop))
    {
      if (dump_file)
	{
	  rtx insn = BB_END (loop->header);

	  fprintf (dump_file, "SMS loop many BBs.");
	  dump_insn_location (insn);
	  fprintf (dump_file, "\n");
	}
      return false;
    }

    return true;
}

/* If there are more than one entry for the loop,
   make it one by splitting the first entry edge and
   redirecting the others to the new BB.  */
static void
canon_loop (struct loop *loop)
{
  edge e;
  edge_iterator i;

  /* Avoid annoying special cases of edges going to exit
     block.  */
  FOR_EACH_EDGE (e, i, EXIT_BLOCK_PTR_FOR_FN (cfun)->preds)
    if ((e->flags & EDGE_FALLTHRU) && (EDGE_COUNT (e->src->succs) > 1))
      split_edge (e);

  if (loop->latch == loop->header
      || EDGE_COUNT (loop->latch->succs) > 1)
    {
      FOR_EACH_EDGE (e, i, loop->header->preds)
        if (e->src == loop->latch)
          break;
      split_edge (e);
    }
}

/* Setup infos.  */
static void
setup_sched_infos (void)
{
  memcpy (&sms_common_sched_info, &haifa_common_sched_info,
	  sizeof (sms_common_sched_info));
  sms_common_sched_info.sched_pass_id = SCHED_SMS_PASS;
  common_sched_info = &sms_common_sched_info;

  sched_deps_info = &sms_sched_deps_info;
  current_sched_info = &sms_sched_info;
}

/* Probability in % that the sms-ed loop rolls enough so that optimized
   version may be entered.  Just a guess.  */
#define PROB_SMS_ENOUGH_ITERATIONS 80

/* Used to calculate the upper bound of ii.  */
#define MAXII_FACTOR 2

/* Main entry point, perform SMS scheduling on the loops of the function
   that consist of single basic blocks.  */
static void
sms_schedule (void)
{
  rtx insn;
  ddg_ptr *g_arr, g;
  int * node_order;
  int maxii, max_asap;
  partial_schedule_ptr ps;
  basic_block bb = NULL;
  struct loop *loop;
  basic_block condition_bb = NULL;
  edge latch_edge;
  gcov_type trip_count = 0;

  loop_optimizer_init (LOOPS_HAVE_PREHEADERS
		       | LOOPS_HAVE_RECORDED_EXITS);
  if (number_of_loops (cfun) <= 1)
    {
      loop_optimizer_finalize ();
      return;  /* There are no loops to schedule.  */
    }

  /* Initialize issue_rate.  */
  if (targetm.sched.issue_rate)
    {
      int temp = reload_completed;

      reload_completed = 1;
      issue_rate = targetm.sched.issue_rate ();
      reload_completed = temp;
    }
  else
    issue_rate = 1;

  /* Initialize the scheduler.  */
  setup_sched_infos ();
  haifa_sched_init ();

  /* Allocate memory to hold the DDG array one entry for each loop.
     We use loop->num as index into this array.  */
  g_arr = XCNEWVEC (ddg_ptr, number_of_loops (cfun));

  if (dump_file)
  {
    fprintf (dump_file, "\n\nSMS analysis phase\n");
    fprintf (dump_file, "===================\n\n");
  }

  /* Build DDGs for all the relevant loops and hold them in G_ARR
     indexed by the loop index.  */
  FOR_EACH_LOOP (loop, 0)
    {
      rtx head, tail;
      rtx count_reg;

      /* For debugging.  */
      if (dbg_cnt (sms_sched_loop) == false)
        {
          if (dump_file)
            fprintf (dump_file, "SMS reached max limit... \n");

	  break;
        }

      if (dump_file)
	{
	  rtx insn = BB_END (loop->header);

	  fprintf (dump_file, "SMS loop num: %d", loop->num);
	  dump_insn_location (insn);
	  fprintf (dump_file, "\n");
	}

      if (! loop_canon_p (loop))
        continue;

      if (! loop_single_full_bb_p (loop))
      {
        if (dump_file)
          fprintf (dump_file, "SMS not loop_single_full_bb_p\n");
	continue;
      }

      bb = loop->header;

      get_ebb_head_tail (bb, bb, &head, &tail);
      latch_edge = loop_latch_edge (loop);
      gcc_assert (single_exit (loop));
      if (single_exit (loop)->count)
	trip_count = latch_edge->count / single_exit (loop)->count;

      /* Perform SMS only on loops that their average count is above threshold.  */

      if ( latch_edge->count
          && (latch_edge->count < single_exit (loop)->count * SMS_LOOP_AVERAGE_COUNT_THRESHOLD))
	{
	  if (dump_file)
	    {
	      dump_insn_location (tail);
	      fprintf (dump_file, "\nSMS single-bb-loop\n");
	      if (profile_info && flag_branch_probabilities)
	    	{
	      	  fprintf (dump_file, "SMS loop-count ");
	      	  fprintf (dump_file, HOST_WIDEST_INT_PRINT_DEC,
	             	   (HOST_WIDEST_INT) bb->count);
	      	  fprintf (dump_file, "\n");
                  fprintf (dump_file, "SMS trip-count ");
                  fprintf (dump_file, HOST_WIDEST_INT_PRINT_DEC,
                           (HOST_WIDEST_INT) trip_count);
                  fprintf (dump_file, "\n");
	      	  fprintf (dump_file, "SMS profile-sum-max ");
	      	  fprintf (dump_file, HOST_WIDEST_INT_PRINT_DEC,
	          	   (HOST_WIDEST_INT) profile_info->sum_max);
	      	  fprintf (dump_file, "\n");
	    	}
	    }
          continue;
        }

      /* Make sure this is a doloop.  */
      if ( !(count_reg = doloop_register_get (head, tail)))
      {
        if (dump_file)
          fprintf (dump_file, "SMS doloop_register_get failed\n");
	continue;
      }

      /* Don't handle BBs with calls or barriers
	 or !single_set with the exception of instructions that include
	 count_reg---these instructions are part of the control part
	 that do-loop recognizes.
         ??? Should handle insns defining subregs.  */
     for (insn = head; insn != NEXT_INSN (tail); insn = NEXT_INSN (insn))
      {
         rtx set;

        if (CALL_P (insn)
            || BARRIER_P (insn)
            || (NONDEBUG_INSN_P (insn) && !JUMP_P (insn)
                && !single_set (insn) && GET_CODE (PATTERN (insn)) != USE
                && !reg_mentioned_p (count_reg, insn))
            || (INSN_P (insn) && (set = single_set (insn))
                && GET_CODE (SET_DEST (set)) == SUBREG))
        break;
      }

      if (insn != NEXT_INSN (tail))
	{
	  if (dump_file)
	    {
	      if (CALL_P (insn))
		fprintf (dump_file, "SMS loop-with-call\n");
	      else if (BARRIER_P (insn))
		fprintf (dump_file, "SMS loop-with-barrier\n");
              else if ((NONDEBUG_INSN_P (insn) && !JUMP_P (insn)
                && !single_set (insn) && GET_CODE (PATTERN (insn)) != USE))
                fprintf (dump_file, "SMS loop-with-not-single-set\n");
              else
               fprintf (dump_file, "SMS loop with subreg in lhs\n");
	      print_rtl_single (dump_file, insn);
	    }

	  continue;
	}

      /* Always schedule the closing branch with the rest of the
         instructions. The branch is rotated to be in row ii-1 at the
         end of the scheduling procedure to make sure it's the last
         instruction in the iteration.  */
      if (! (g = create_ddg (bb, 1)))
        {
          if (dump_file)
	    fprintf (dump_file, "SMS create_ddg failed\n");
	  continue;
        }

      g_arr[loop->num] = g;
      if (dump_file)
        fprintf (dump_file, "...OK\n");

    }
  if (dump_file)
  {
    fprintf (dump_file, "\nSMS transformation phase\n");
    fprintf (dump_file, "=========================\n\n");
  }

  /* We don't want to perform SMS on new loops - created by versioning.  */
  FOR_EACH_LOOP (loop, 0)
    {
      rtx head, tail;
      rtx count_reg, count_init;
      int mii, rec_mii, stage_count, min_cycle;
      HOST_WIDEST_INT loop_count = 0;
      bool opt_sc_p;

      if (! (g = g_arr[loop->num]))
        continue;

      if (dump_file)
	{
	  rtx insn = BB_END (loop->header);

	  fprintf (dump_file, "SMS loop num: %d", loop->num);
	  dump_insn_location (insn);
	  fprintf (dump_file, "\n");

	  print_ddg (dump_file, g);
	}

      get_ebb_head_tail (loop->header, loop->header, &head, &tail);

      latch_edge = loop_latch_edge (loop);
      gcc_assert (single_exit (loop));
      if (single_exit (loop)->count)
	trip_count = latch_edge->count / single_exit (loop)->count;

      if (dump_file)
	{
	  dump_insn_location (tail);
	  fprintf (dump_file, "\nSMS single-bb-loop\n");
	  if (profile_info && flag_branch_probabilities)
	    {
	      fprintf (dump_file, "SMS loop-count ");
	      fprintf (dump_file, HOST_WIDEST_INT_PRINT_DEC,
	               (HOST_WIDEST_INT) bb->count);
	      fprintf (dump_file, "\n");
	      fprintf (dump_file, "SMS profile-sum-max ");
	      fprintf (dump_file, HOST_WIDEST_INT_PRINT_DEC,
	               (HOST_WIDEST_INT) profile_info->sum_max);
	      fprintf (dump_file, "\n");
	    }
	  fprintf (dump_file, "SMS doloop\n");
	  fprintf (dump_file, "SMS built-ddg %d\n", g->num_nodes);
          fprintf (dump_file, "SMS num-loads %d\n", g->num_loads);
          fprintf (dump_file, "SMS num-stores %d\n", g->num_stores);
	}


      /* In case of th loop have doloop register it gets special
	 handling.  */
      count_init = NULL_RTX;
      if ((count_reg = doloop_register_get (head, tail)))
	{
	  basic_block pre_header;

	  pre_header = loop_preheader_edge (loop)->src;
	  count_init = const_iteration_count (count_reg, pre_header,
					      &loop_count);
	}
      gcc_assert (count_reg);

      if (dump_file && count_init)
        {
          fprintf (dump_file, "SMS const-doloop ");
          fprintf (dump_file, HOST_WIDEST_INT_PRINT_DEC,
		     loop_count);
          fprintf (dump_file, "\n");
        }

      node_order = XNEWVEC (int, g->num_nodes);

      mii = 1; /* Need to pass some estimate of mii.  */
      rec_mii = sms_order_nodes (g, mii, node_order, &max_asap);
      mii = MAX (res_MII (g), rec_mii);
      maxii = MAX (max_asap, MAXII_FACTOR * mii);

      if (dump_file)
	fprintf (dump_file, "SMS iis %d %d %d (rec_mii, mii, maxii)\n",
		 rec_mii, mii, maxii);

      for (;;)
	{
	  set_node_sched_params (g);

	  stage_count = 0;
	  opt_sc_p = false;
	  ps = sms_schedule_by_order (g, mii, maxii, node_order);

	  if (ps)
	    {
	      /* Try to achieve optimized SC by normalizing the partial
		 schedule (having the cycles start from cycle zero).
		 The branch location must be placed in row ii-1 in the
		 final scheduling.	If failed, shift all instructions to
		 position the branch in row ii-1.  */
	      opt_sc_p = optimize_sc (ps, g);
	      if (opt_sc_p)
		stage_count = calculate_stage_count (ps, 0);
	      else
		{
		  /* Bring the branch to cycle ii-1.  */
		  int amount = (SCHED_TIME (g->closing_branch->cuid)
				- (ps->ii - 1));

		  if (dump_file)
		    fprintf (dump_file, "SMS schedule branch at cycle ii-1\n");

		  stage_count = calculate_stage_count (ps, amount);
		}

	      gcc_assert (stage_count >= 1);
	    }

	  /* The default value of PARAM_SMS_MIN_SC is 2 as stage count of
	     1 means that there is no interleaving between iterations thus
	     we let the scheduling passes do the job in this case.  */
	  if (stage_count < PARAM_VALUE (PARAM_SMS_MIN_SC)
	      || (count_init && (loop_count <= stage_count))
	      || (flag_branch_probabilities && (trip_count <= stage_count)))
	    {
	      if (dump_file)
		{
		  fprintf (dump_file, "SMS failed... \n");
		  fprintf (dump_file, "SMS sched-failed (stage-count=%d,"
			   " loop-count=", stage_count);
		  fprintf (dump_file, HOST_WIDEST_INT_PRINT_DEC, loop_count);
		  fprintf (dump_file, ", trip-count=");
		  fprintf (dump_file, HOST_WIDEST_INT_PRINT_DEC, trip_count);
		  fprintf (dump_file, ")\n");
		}
	      break;
	    }

          if (!opt_sc_p)
            {
	      /* Rotate the partial schedule to have the branch in row ii-1.  */
              int amount = SCHED_TIME (g->closing_branch->cuid) - (ps->ii - 1);
	      
              reset_sched_times (ps, amount);
              rotate_partial_schedule (ps, amount);
            }
	  
	  set_columns_for_ps (ps);

	  min_cycle = PS_MIN_CYCLE (ps) - SMODULO (PS_MIN_CYCLE (ps), ps->ii);
	  if (!schedule_reg_moves (ps))
	    {
	      mii = ps->ii + 1;
	      free_partial_schedule (ps);
	      continue;
	    }

	  /* Moves that handle incoming values might have been added
	     to a new first stage.  Bump the stage count if so.

	     ??? Perhaps we could consider rotating the schedule here
	     instead?  */
	  if (PS_MIN_CYCLE (ps) < min_cycle)
	    {
	      reset_sched_times (ps, 0);
	      stage_count++;
	    }

	  /* The stage count should now be correct without rotation.  */
	  gcc_checking_assert (stage_count == calculate_stage_count (ps, 0));
	  PS_STAGE_COUNT (ps) = stage_count;

	  canon_loop (loop);

          if (dump_file)
            {
	      dump_insn_location (tail);
	      fprintf (dump_file, " SMS succeeded %d %d (with ii, sc)\n",
		       ps->ii, stage_count);
	      print_partial_schedule (ps, dump_file);
	    }
 
          /* case the BCT count is not known , Do loop-versioning */
	  if (count_reg && ! count_init)
            {
	      rtx comp_rtx = gen_rtx_GT (VOIDmode, count_reg,
					 gen_int_mode (stage_count,
						       GET_MODE (count_reg)));
	      unsigned prob = (PROB_SMS_ENOUGH_ITERATIONS
			       * REG_BR_PROB_BASE) / 100;

	      loop_version (loop, comp_rtx, &condition_bb,
	  		    prob, prob, REG_BR_PROB_BASE - prob,
			    true);
	     }

	  /* Set new iteration count of loop kernel.  */
          if (count_reg && count_init)
	    SET_SRC (single_set (count_init)) = GEN_INT (loop_count
						     - stage_count + 1);

	  /* Now apply the scheduled kernel to the RTL of the loop.  */
	  permute_partial_schedule (ps, g->closing_branch->first_note);

          /* Mark this loop as software pipelined so the later
	     scheduling passes don't touch it.  */
	  if (! flag_resched_modulo_sched)
	    mark_loop_unsched (loop);
	  
	  /* The life-info is not valid any more.  */
	  df_set_bb_dirty (g->bb);

	  apply_reg_moves (ps);
	  if (dump_file)
	    print_node_sched_params (dump_file, g->num_nodes, ps);
	  /* Generate prolog and epilog.  */
          generate_prolog_epilog (ps, loop, count_reg, count_init);
	  break;
	}

      free_partial_schedule (ps);
      node_sched_param_vec.release ();
      free (node_order);
      free_ddg (g);
    }

  free (g_arr);

  /* Release scheduler data, needed until now because of DFA.  */
  haifa_sched_finish ();
  loop_optimizer_finalize ();
}

/* The SMS scheduling algorithm itself
   -----------------------------------
   Input: 'O' an ordered list of insns of a loop.
   Output: A scheduling of the loop - kernel, prolog, and epilogue.

   'Q' is the empty Set
   'PS' is the partial schedule; it holds the currently scheduled nodes with
	their cycle/slot.
   'PSP' previously scheduled predecessors.
   'PSS' previously scheduled successors.
   't(u)' the cycle where u is scheduled.
   'l(u)' is the latency of u.
   'd(v,u)' is the dependence distance from v to u.
   'ASAP(u)' the earliest time at which u could be scheduled as computed in
	     the node ordering phase.
   'check_hardware_resources_conflicts(u, PS, c)'
			     run a trace around cycle/slot through DFA model
			     to check resource conflicts involving instruction u
			     at cycle c given the partial schedule PS.
   'add_to_partial_schedule_at_time(u, PS, c)'
			     Add the node/instruction u to the partial schedule
			     PS at time c.
   'calculate_register_pressure(PS)'
			     Given a schedule of instructions, calculate the register
			     pressure it implies.  One implementation could be the
			     maximum number of overlapping live ranges.
   'maxRP' The maximum allowed register pressure, it is usually derived from the number
	   registers available in the hardware.

   1. II = MII.
   2. PS = empty list
   3. for each node u in O in pre-computed order
   4.   if (PSP(u) != Q && PSS(u) == Q) then
   5.     Early_start(u) = max ( t(v) + l(v) - d(v,u)*II ) over all every v in PSP(u).
   6.     start = Early_start; end = Early_start + II - 1; step = 1
   11.  else if (PSP(u) == Q && PSS(u) != Q) then
   12.      Late_start(u) = min ( t(v) - l(v) + d(v,u)*II ) over all every v in PSS(u).
   13.     start = Late_start; end = Late_start - II + 1; step = -1
   14.  else if (PSP(u) != Q && PSS(u) != Q) then
   15.     Early_start(u) = max ( t(v) + l(v) - d(v,u)*II ) over all every v in PSP(u).
   16.     Late_start(u) = min ( t(v) - l(v) + d(v,u)*II ) over all every v in PSS(u).
   17.     start = Early_start;
   18.     end = min(Early_start + II - 1 , Late_start);
   19.     step = 1
   20.     else "if (PSP(u) == Q && PSS(u) == Q)"
   21.	  start = ASAP(u); end = start + II - 1; step = 1
   22.  endif

   23.  success = false
   24.  for (c = start ; c != end ; c += step)
   25.     if check_hardware_resources_conflicts(u, PS, c) then
   26.       add_to_partial_schedule_at_time(u, PS, c)
   27.       success = true
   28.       break
   29.     endif
   30.  endfor
   31.  if (success == false) then
   32.    II = II + 1
   33.    if (II > maxII) then
   34.       finish - failed to schedule
   35.	 endif
   36.    goto 2.
   37.  endif
   38. endfor
   39. if (calculate_register_pressure(PS) > maxRP) then
   40.    goto 32.
   41. endif
   42. compute epilogue & prologue
   43. finish - succeeded to schedule

   ??? The algorithm restricts the scheduling window to II cycles.
   In rare cases, it may be better to allow windows of II+1 cycles.
   The window would then start and end on the same row, but with
   different "must precede" and "must follow" requirements.  */

/* A limit on the number of cycles that resource conflicts can span.  ??? Should
   be provided by DFA, and be dependent on the type of insn scheduled.  Currently
   set to 0 to save compile time.  */
#define DFA_HISTORY SMS_DFA_HISTORY

/* A threshold for the number of repeated unsuccessful attempts to insert
   an empty row, before we flush the partial schedule and start over.  */
#define MAX_SPLIT_NUM 10
/* Given the partial schedule PS, this function calculates and returns the
   cycles in which we can schedule the node with the given index I.
   NOTE: Here we do the backtracking in SMS, in some special cases. We have
   noticed that there are several cases in which we fail    to SMS the loop
   because the sched window of a node is empty    due to tight data-deps. In
   such cases we want to unschedule    some of the predecessors/successors
   until we get non-empty    scheduling window.  It returns -1 if the
   scheduling window is empty and zero otherwise.  */

static int
get_sched_window (partial_schedule_ptr ps, ddg_node_ptr u_node,
		  sbitmap sched_nodes, int ii, int *start_p, int *step_p,
		  int *end_p)
{
  int start, step, end;
  int early_start, late_start;
  ddg_edge_ptr e;
  sbitmap psp = sbitmap_alloc (ps->g->num_nodes);
  sbitmap pss = sbitmap_alloc (ps->g->num_nodes);
  sbitmap u_node_preds = NODE_PREDECESSORS (u_node);
  sbitmap u_node_succs = NODE_SUCCESSORS (u_node);
  int psp_not_empty;
  int pss_not_empty;
  int count_preds;
  int count_succs;

  /* 1. compute sched window for u (start, end, step).  */
  bitmap_clear (psp);
  bitmap_clear (pss);
  psp_not_empty = bitmap_and (psp, u_node_preds, sched_nodes);
  pss_not_empty = bitmap_and (pss, u_node_succs, sched_nodes);

  /* We first compute a forward range (start <= end), then decide whether
     to reverse it.  */
  early_start = INT_MIN;
  late_start = INT_MAX;
  start = INT_MIN;
  end = INT_MAX;
  step = 1;

  count_preds = 0;
  count_succs = 0;

  if (dump_file && (psp_not_empty || pss_not_empty))
    {
      fprintf (dump_file, "\nAnalyzing dependencies for node %d (INSN %d)"
	       "; ii = %d\n\n", u_node->cuid, INSN_UID (u_node->insn), ii);
      fprintf (dump_file, "%11s %11s %11s %11s %5s\n",
	       "start", "early start", "late start", "end", "time");
      fprintf (dump_file, "=========== =========== =========== ==========="
	       " =====\n");
    }
  /* Calculate early_start and limit end.  Both bounds are inclusive.  */
  if (psp_not_empty)
    for (e = u_node->in; e != 0; e = e->next_in)
      {
	int v = e->src->cuid;

	if (bitmap_bit_p (sched_nodes, v))
	  {
	    int p_st = SCHED_TIME (v);
	    int earliest = p_st + e->latency - (e->distance * ii);
	    int latest = (e->data_type == MEM_DEP ? p_st + ii - 1 : INT_MAX);

	    if (dump_file)
	      {
		fprintf (dump_file, "%11s %11d %11s %11d %5d",
			 "", earliest, "", latest, p_st);
		print_ddg_edge (dump_file, e);
		fprintf (dump_file, "\n");
	      }

	    early_start = MAX (early_start, earliest);
	    end = MIN (end, latest);

	    if (e->type == TRUE_DEP && e->data_type == REG_DEP)
	      count_preds++;
	  }
      }

  /* Calculate late_start and limit start.  Both bounds are inclusive.  */
  if (pss_not_empty)
    for (e = u_node->out; e != 0; e = e->next_out)
      {
	int v = e->dest->cuid;

	if (bitmap_bit_p (sched_nodes, v))
	  {
	    int s_st = SCHED_TIME (v);
	    int earliest = (e->data_type == MEM_DEP ? s_st - ii + 1 : INT_MIN);
	    int latest = s_st - e->latency + (e->distance * ii);

	    if (dump_file)
	      {
		fprintf (dump_file, "%11d %11s %11d %11s %5d",
			 earliest, "", latest, "", s_st);
		print_ddg_edge (dump_file, e);
		fprintf (dump_file, "\n");
	      }

	    start = MAX (start, earliest);
	    late_start = MIN (late_start, latest);

	    if (e->type == TRUE_DEP && e->data_type == REG_DEP)
	      count_succs++;
	  }
      }

  if (dump_file && (psp_not_empty || pss_not_empty))
    {
      fprintf (dump_file, "----------- ----------- ----------- -----------"
	       " -----\n");
      fprintf (dump_file, "%11d %11d %11d %11d %5s %s\n",
	       start, early_start, late_start, end, "",
	       "(max, max, min, min)");
    }

  /* Get a target scheduling window no bigger than ii.  */
  if (early_start == INT_MIN && late_start == INT_MAX)
    early_start = NODE_ASAP (u_node);
  else if (early_start == INT_MIN)
    early_start = late_start - (ii - 1);
  late_start = MIN (late_start, early_start + (ii - 1));

  /* Apply memory dependence limits.  */
  start = MAX (start, early_start);
  end = MIN (end, late_start);

  if (dump_file && (psp_not_empty || pss_not_empty))
    fprintf (dump_file, "%11s %11d %11d %11s %5s final window\n",
	     "", start, end, "", "");

  /* If there are at least as many successors as predecessors, schedule the
     node close to its successors.  */
  if (pss_not_empty && count_succs >= count_preds)
    {
      int tmp = end;
      end = start;
      start = tmp;
      step = -1;
    }

  /* Now that we've finalized the window, make END an exclusive rather
     than an inclusive bound.  */
  end += step;

  *start_p = start;
  *step_p = step;
  *end_p = end;
  sbitmap_free (psp);
  sbitmap_free (pss);

  if ((start >= end && step == 1) || (start <= end && step == -1))
    {
      if (dump_file)
	fprintf (dump_file, "\nEmpty window: start=%d, end=%d, step=%d\n",
		 start, end, step);
      return -1;
    }

  return 0;
}

/* Calculate MUST_PRECEDE/MUST_FOLLOW bitmaps of U_NODE; which is the
   node currently been scheduled.  At the end of the calculation
   MUST_PRECEDE/MUST_FOLLOW contains all predecessors/successors of
   U_NODE which are (1) already scheduled in the first/last row of
   U_NODE's scheduling window, (2) whose dependence inequality with U
   becomes an equality when U is scheduled in this same row, and (3)
   whose dependence latency is zero.

   The first and last rows are calculated using the following parameters:
   START/END rows - The cycles that begins/ends the traversal on the window;
   searching for an empty cycle to schedule U_NODE.
   STEP - The direction in which we traverse the window.
   II - The initiation interval.  */

static void
calculate_must_precede_follow (ddg_node_ptr u_node, int start, int end,
			       int step, int ii, sbitmap sched_nodes,
			       sbitmap must_precede, sbitmap must_follow)
{
  ddg_edge_ptr e;
  int first_cycle_in_window, last_cycle_in_window;

  gcc_assert (must_precede && must_follow);

  /* Consider the following scheduling window:
     {first_cycle_in_window, first_cycle_in_window+1, ...,
     last_cycle_in_window}.  If step is 1 then the following will be
     the order we traverse the window: {start=first_cycle_in_window,
     first_cycle_in_window+1, ..., end=last_cycle_in_window+1},
     or {start=last_cycle_in_window, last_cycle_in_window-1, ...,
     end=first_cycle_in_window-1} if step is -1.  */
  first_cycle_in_window = (step == 1) ? start : end - step;
  last_cycle_in_window = (step == 1) ? end - step : start;

  bitmap_clear (must_precede);
  bitmap_clear (must_follow);

  if (dump_file)
    fprintf (dump_file, "\nmust_precede: ");

  /* Instead of checking if:
      (SMODULO (SCHED_TIME (e->src), ii) == first_row_in_window)
      && ((SCHED_TIME (e->src) + e->latency - (e->distance * ii)) ==
             first_cycle_in_window)
      && e->latency == 0
     we use the fact that latency is non-negative:
      SCHED_TIME (e->src) - (e->distance * ii) <=
      SCHED_TIME (e->src) + e->latency - (e->distance * ii)) <=
      first_cycle_in_window
     and check only if
      SCHED_TIME (e->src) - (e->distance * ii) == first_cycle_in_window  */
  for (e = u_node->in; e != 0; e = e->next_in)
    if (bitmap_bit_p (sched_nodes, e->src->cuid)
	&& ((SCHED_TIME (e->src->cuid) - (e->distance * ii)) ==
             first_cycle_in_window))
      {
	if (dump_file)
	  fprintf (dump_file, "%d ", e->src->cuid);

	bitmap_set_bit (must_precede, e->src->cuid);
      }

  if (dump_file)
    fprintf (dump_file, "\nmust_follow: ");

  /* Instead of checking if:
      (SMODULO (SCHED_TIME (e->dest), ii) == last_row_in_window)
      && ((SCHED_TIME (e->dest) - e->latency + (e->distance * ii)) ==
             last_cycle_in_window)
      && e->latency == 0
     we use the fact that latency is non-negative:
      SCHED_TIME (e->dest) + (e->distance * ii) >=
      SCHED_TIME (e->dest) - e->latency + (e->distance * ii)) >=
      last_cycle_in_window
     and check only if
      SCHED_TIME (e->dest) + (e->distance * ii) == last_cycle_in_window  */
  for (e = u_node->out; e != 0; e = e->next_out)
    if (bitmap_bit_p (sched_nodes, e->dest->cuid)
	&& ((SCHED_TIME (e->dest->cuid) + (e->distance * ii)) ==
             last_cycle_in_window))
      {
	if (dump_file)
	  fprintf (dump_file, "%d ", e->dest->cuid);

	bitmap_set_bit (must_follow, e->dest->cuid);
      }

  if (dump_file)
    fprintf (dump_file, "\n");
}

/* Return 1 if U_NODE can be scheduled in CYCLE.  Use the following
   parameters to decide if that's possible:
   PS - The partial schedule.
   U - The serial number of U_NODE.
   NUM_SPLITS - The number of row splits made so far.
   MUST_PRECEDE - The nodes that must precede U_NODE. (only valid at
   the first row of the scheduling window)
   MUST_FOLLOW - The nodes that must follow U_NODE. (only valid at the
   last row of the scheduling window)  */

static bool
try_scheduling_node_in_cycle (partial_schedule_ptr ps,
			      int u, int cycle, sbitmap sched_nodes,
			      int *num_splits, sbitmap must_precede,
			      sbitmap must_follow)
{
  ps_insn_ptr psi;
  bool success = 0;

  verify_partial_schedule (ps, sched_nodes);
  psi = ps_add_node_check_conflicts (ps, u, cycle, must_precede, must_follow);
  if (psi)
    {
      SCHED_TIME (u) = cycle;
      bitmap_set_bit (sched_nodes, u);
      success = 1;
      *num_splits = 0;
      if (dump_file)
	fprintf (dump_file, "Scheduled w/o split in %d\n", cycle);

    }

  return success;
}

/* This function implements the scheduling algorithm for SMS according to the
   above algorithm.  */
static partial_schedule_ptr
sms_schedule_by_order (ddg_ptr g, int mii, int maxii, int *nodes_order)
{
  int ii = mii;
  int i, c, success, num_splits = 0;
  int flush_and_start_over = true;
  int num_nodes = g->num_nodes;
  int start, end, step; /* Place together into one struct?  */
  sbitmap sched_nodes = sbitmap_alloc (num_nodes);
  sbitmap must_precede = sbitmap_alloc (num_nodes);
  sbitmap must_follow = sbitmap_alloc (num_nodes);
  sbitmap tobe_scheduled = sbitmap_alloc (num_nodes);

  partial_schedule_ptr ps = create_partial_schedule (ii, g, DFA_HISTORY);

  bitmap_ones (tobe_scheduled);
  bitmap_clear (sched_nodes);

  while (flush_and_start_over && (ii < maxii))
    {

      if (dump_file)
	fprintf (dump_file, "Starting with ii=%d\n", ii);
      flush_and_start_over = false;
      bitmap_clear (sched_nodes);

      for (i = 0; i < num_nodes; i++)
	{
	  int u = nodes_order[i];
  	  ddg_node_ptr u_node = &ps->g->nodes[u];
	  rtx insn = u_node->insn;

	  if (!NONDEBUG_INSN_P (insn))
	    {
	      bitmap_clear_bit (tobe_scheduled, u);
	      continue;
	    }

	  if (bitmap_bit_p (sched_nodes, u))
	    continue;

	  /* Try to get non-empty scheduling window.  */
	 success = 0;
         if (get_sched_window (ps, u_node, sched_nodes, ii, &start,
                                &step, &end) == 0)
            {
              if (dump_file)
                fprintf (dump_file, "\nTrying to schedule node %d "
			 "INSN = %d  in (%d .. %d) step %d\n", u, (INSN_UID
                        (g->nodes[u].insn)), start, end, step);

              gcc_assert ((step > 0 && start < end)
                          || (step < 0 && start > end));

              calculate_must_precede_follow (u_node, start, end, step, ii,
                                             sched_nodes, must_precede,
                                             must_follow);

              for (c = start; c != end; c += step)
                {
		  sbitmap tmp_precede, tmp_follow;

                  set_must_precede_follow (&tmp_follow, must_follow, 
		                           &tmp_precede, must_precede, 
                                           c, start, end, step);
                  success =
                    try_scheduling_node_in_cycle (ps, u, c,
                                                  sched_nodes,
                                                  &num_splits, tmp_precede,
                                                  tmp_follow);
                  if (success)
                    break;
                }

              verify_partial_schedule (ps, sched_nodes);
            }
            if (!success)
            {
              int split_row;

              if (ii++ == maxii)
                break;

              if (num_splits >= MAX_SPLIT_NUM)
                {
                  num_splits = 0;
                  flush_and_start_over = true;
                  verify_partial_schedule (ps, sched_nodes);
                  reset_partial_schedule (ps, ii);
                  verify_partial_schedule (ps, sched_nodes);
                  break;
                }

              num_splits++;
              /* The scheduling window is exclusive of 'end'
                 whereas compute_split_window() expects an inclusive,
                 ordered range.  */
              if (step == 1)
                split_row = compute_split_row (sched_nodes, start, end - 1,
                                               ps->ii, u_node);
              else
                split_row = compute_split_row (sched_nodes, end + 1, start,
                                               ps->ii, u_node);

              ps_insert_empty_row (ps, split_row, sched_nodes);
              i--;              /* Go back and retry node i.  */

              if (dump_file)
                fprintf (dump_file, "num_splits=%d\n", num_splits);
            }

          /* ??? If (success), check register pressure estimates.  */
        }                       /* Continue with next node.  */
    }                           /* While flush_and_start_over.  */
  if (ii >= maxii)
    {
      free_partial_schedule (ps);
      ps = NULL;
    }
  else
    gcc_assert (bitmap_equal_p (tobe_scheduled, sched_nodes));

  sbitmap_free (sched_nodes);
  sbitmap_free (must_precede);
  sbitmap_free (must_follow);
  sbitmap_free (tobe_scheduled);

  return ps;
}

/* This function inserts a new empty row into PS at the position
   according to SPLITROW, keeping all already scheduled instructions
   intact and updating their SCHED_TIME and cycle accordingly.  */
static void
ps_insert_empty_row (partial_schedule_ptr ps, int split_row,
		     sbitmap sched_nodes)
{
  ps_insn_ptr crr_insn;
  ps_insn_ptr *rows_new;
  int ii = ps->ii;
  int new_ii = ii + 1;
  int row;
  int *rows_length_new;

  verify_partial_schedule (ps, sched_nodes);

  /* We normalize sched_time and rotate ps to have only non-negative sched
     times, for simplicity of updating cycles after inserting new row.  */
  split_row -= ps->min_cycle;
  split_row = SMODULO (split_row, ii);
  if (dump_file)
    fprintf (dump_file, "split_row=%d\n", split_row);

  reset_sched_times (ps, PS_MIN_CYCLE (ps));
  rotate_partial_schedule (ps, PS_MIN_CYCLE (ps));

  rows_new = (ps_insn_ptr *) xcalloc (new_ii, sizeof (ps_insn_ptr));
  rows_length_new = (int *) xcalloc (new_ii, sizeof (int));
  for (row = 0; row < split_row; row++)
    {
      rows_new[row] = ps->rows[row];
      rows_length_new[row] = ps->rows_length[row];
      ps->rows[row] = NULL;
      for (crr_insn = rows_new[row];
	   crr_insn; crr_insn = crr_insn->next_in_row)
	{
	  int u = crr_insn->id;
	  int new_time = SCHED_TIME (u) + (SCHED_TIME (u) / ii);

	  SCHED_TIME (u) = new_time;
	  crr_insn->cycle = new_time;
	  SCHED_ROW (u) = new_time % new_ii;
	  SCHED_STAGE (u) = new_time / new_ii;
	}

    }

  rows_new[split_row] = NULL;

  for (row = split_row; row < ii; row++)
    {
      rows_new[row + 1] = ps->rows[row];
      rows_length_new[row + 1] = ps->rows_length[row];
      ps->rows[row] = NULL;
      for (crr_insn = rows_new[row + 1];
	   crr_insn; crr_insn = crr_insn->next_in_row)
	{
	  int u = crr_insn->id;
	  int new_time = SCHED_TIME (u) + (SCHED_TIME (u) / ii) + 1;

	  SCHED_TIME (u) = new_time;
	  crr_insn->cycle = new_time;
	  SCHED_ROW (u) = new_time % new_ii;
	  SCHED_STAGE (u) = new_time / new_ii;
	}
    }

  /* Updating ps.  */
  ps->min_cycle = ps->min_cycle + ps->min_cycle / ii
    + (SMODULO (ps->min_cycle, ii) >= split_row ? 1 : 0);
  ps->max_cycle = ps->max_cycle + ps->max_cycle / ii
    + (SMODULO (ps->max_cycle, ii) >= split_row ? 1 : 0);
  free (ps->rows);
  ps->rows = rows_new;
  free (ps->rows_length);
  ps->rows_length = rows_length_new;
  ps->ii = new_ii;
  gcc_assert (ps->min_cycle >= 0);

  verify_partial_schedule (ps, sched_nodes);

  if (dump_file)
    fprintf (dump_file, "min_cycle=%d, max_cycle=%d\n", ps->min_cycle,
	     ps->max_cycle);
}

/* Given U_NODE which is the node that failed to be scheduled; LOW and
   UP which are the boundaries of it's scheduling window; compute using
   SCHED_NODES and II a row in the partial schedule that can be split
   which will separate a critical predecessor from a critical successor
   thereby expanding the window, and return it.  */
static int
compute_split_row (sbitmap sched_nodes, int low, int up, int ii,
		   ddg_node_ptr u_node)
{
  ddg_edge_ptr e;
  int lower = INT_MIN, upper = INT_MAX;
  int crit_pred = -1;
  int crit_succ = -1;
  int crit_cycle;

  for (e = u_node->in; e != 0; e = e->next_in)
    {
      int v = e->src->cuid;

      if (bitmap_bit_p (sched_nodes, v)
	  && (low == SCHED_TIME (v) + e->latency - (e->distance * ii)))
	if (SCHED_TIME (v) > lower)
	  {
	    crit_pred = v;
	    lower = SCHED_TIME (v);
	  }
    }

  if (crit_pred >= 0)
    {
      crit_cycle = SCHED_TIME (crit_pred) + 1;
      return SMODULO (crit_cycle, ii);
    }

  for (e = u_node->out; e != 0; e = e->next_out)
    {
      int v = e->dest->cuid;

      if (bitmap_bit_p (sched_nodes, v)
	  && (up == SCHED_TIME (v) - e->latency + (e->distance * ii)))
	if (SCHED_TIME (v) < upper)
	  {
	    crit_succ = v;
	    upper = SCHED_TIME (v);
	  }
    }

  if (crit_succ >= 0)
    {
      crit_cycle = SCHED_TIME (crit_succ);
      return SMODULO (crit_cycle, ii);
    }

  if (dump_file)
    fprintf (dump_file, "Both crit_pred and crit_succ are NULL\n");

  return SMODULO ((low + up + 1) / 2, ii);
}

static void
verify_partial_schedule (partial_schedule_ptr ps, sbitmap sched_nodes)
{
  int row;
  ps_insn_ptr crr_insn;

  for (row = 0; row < ps->ii; row++)
    {
      int length = 0;
      
      for (crr_insn = ps->rows[row]; crr_insn; crr_insn = crr_insn->next_in_row)
	{
	  int u = crr_insn->id;
	  
	  length++;
	  gcc_assert (bitmap_bit_p (sched_nodes, u));
	  /* ??? Test also that all nodes of sched_nodes are in ps, perhaps by
	     popcount (sched_nodes) == number of insns in ps.  */
	  gcc_assert (SCHED_TIME (u) >= ps->min_cycle);
	  gcc_assert (SCHED_TIME (u) <= ps->max_cycle);
	}
      
      gcc_assert (ps->rows_length[row] == length);
    }
}


/* This page implements the algorithm for ordering the nodes of a DDG
   for modulo scheduling, activated through the
   "int sms_order_nodes (ddg_ptr, int mii, int * result)" API.  */

#define ORDER_PARAMS(x) ((struct node_order_params *) (x)->aux.info)
#define ASAP(x) (ORDER_PARAMS ((x))->asap)
#define ALAP(x) (ORDER_PARAMS ((x))->alap)
#define HEIGHT(x) (ORDER_PARAMS ((x))->height)
#define MOB(x) (ALAP ((x)) - ASAP ((x)))
#define DEPTH(x) (ASAP ((x)))

typedef struct node_order_params * nopa;

static void order_nodes_of_sccs (ddg_all_sccs_ptr, int * result);
static int order_nodes_in_scc (ddg_ptr, sbitmap, sbitmap, int*, int);
static nopa  calculate_order_params (ddg_ptr, int, int *);
static int find_max_asap (ddg_ptr, sbitmap);
static int find_max_hv_min_mob (ddg_ptr, sbitmap);
static int find_max_dv_min_mob (ddg_ptr, sbitmap);

enum sms_direction {BOTTOMUP, TOPDOWN};

struct node_order_params
{
  int asap;
  int alap;
  int height;
};

/* Check if NODE_ORDER contains a permutation of 0 .. NUM_NODES-1.  */
static void
check_nodes_order (int *node_order, int num_nodes)
{
  int i;
  sbitmap tmp = sbitmap_alloc (num_nodes);

  bitmap_clear (tmp);

  if (dump_file)
    fprintf (dump_file, "SMS final nodes order: \n");

  for (i = 0; i < num_nodes; i++)
    {
      int u = node_order[i];

      if (dump_file)
        fprintf (dump_file, "%d ", u);
      gcc_assert (u < num_nodes && u >= 0 && !bitmap_bit_p (tmp, u));

      bitmap_set_bit (tmp, u);
    }

  if (dump_file)
    fprintf (dump_file, "\n");

  sbitmap_free (tmp);
}

/* Order the nodes of G for scheduling and pass the result in
   NODE_ORDER.  Also set aux.count of each node to ASAP.
   Put maximal ASAP to PMAX_ASAP.  Return the recMII for the given DDG.  */
static int
sms_order_nodes (ddg_ptr g, int mii, int * node_order, int *pmax_asap)
{
  int i;
  int rec_mii = 0;
  ddg_all_sccs_ptr sccs = create_ddg_all_sccs (g);

  nopa nops = calculate_order_params (g, mii, pmax_asap);

  if (dump_file)
    print_sccs (dump_file, sccs, g);

  order_nodes_of_sccs (sccs, node_order);

  if (sccs->num_sccs > 0)
    /* First SCC has the largest recurrence_length.  */
    rec_mii = sccs->sccs[0]->recurrence_length;

  /* Save ASAP before destroying node_order_params.  */
  for (i = 0; i < g->num_nodes; i++)
    {
      ddg_node_ptr v = &g->nodes[i];
      v->aux.count = ASAP (v);
    }

  free (nops);
  free_ddg_all_sccs (sccs);
  check_nodes_order (node_order, g->num_nodes);

  return rec_mii;
}

static void
order_nodes_of_sccs (ddg_all_sccs_ptr all_sccs, int * node_order)
{
  int i, pos = 0;
  ddg_ptr g = all_sccs->ddg;
  int num_nodes = g->num_nodes;
  sbitmap prev_sccs = sbitmap_alloc (num_nodes);
  sbitmap on_path = sbitmap_alloc (num_nodes);
  sbitmap tmp = sbitmap_alloc (num_nodes);
  sbitmap ones = sbitmap_alloc (num_nodes);

  bitmap_clear (prev_sccs);
  bitmap_ones (ones);

  /* Perform the node ordering starting from the SCC with the highest recMII.
     For each SCC order the nodes according to their ASAP/ALAP/HEIGHT etc.  */
  for (i = 0; i < all_sccs->num_sccs; i++)
    {
      ddg_scc_ptr scc = all_sccs->sccs[i];

      /* Add nodes on paths from previous SCCs to the current SCC.  */
      find_nodes_on_paths (on_path, g, prev_sccs, scc->nodes);
      bitmap_ior (tmp, scc->nodes, on_path);

      /* Add nodes on paths from the current SCC to previous SCCs.  */
      find_nodes_on_paths (on_path, g, scc->nodes, prev_sccs);
      bitmap_ior (tmp, tmp, on_path);

      /* Remove nodes of previous SCCs from current extended SCC.  */
      bitmap_and_compl (tmp, tmp, prev_sccs);

      pos = order_nodes_in_scc (g, prev_sccs, tmp, node_order, pos);
      /* Above call to order_nodes_in_scc updated prev_sccs |= tmp.  */
    }

  /* Handle the remaining nodes that do not belong to any scc.  Each call
     to order_nodes_in_scc handles a single connected component.  */
  while (pos < g->num_nodes)
    {
      bitmap_and_compl (tmp, ones, prev_sccs);
      pos = order_nodes_in_scc (g, prev_sccs, tmp, node_order, pos);
    }
  sbitmap_free (prev_sccs);
  sbitmap_free (on_path);
  sbitmap_free (tmp);
  sbitmap_free (ones);
}

/* MII is needed if we consider backarcs (that do not close recursive cycles).  */
static struct node_order_params *
calculate_order_params (ddg_ptr g, int mii ATTRIBUTE_UNUSED, int *pmax_asap)
{
  int u;
  int max_asap;
  int num_nodes = g->num_nodes;
  ddg_edge_ptr e;
  /* Allocate a place to hold ordering params for each node in the DDG.  */
  nopa node_order_params_arr;

  /* Initialize of ASAP/ALAP/HEIGHT to zero.  */
  node_order_params_arr = (nopa) xcalloc (num_nodes,
					  sizeof (struct node_order_params));

  /* Set the aux pointer of each node to point to its order_params structure.  */
  for (u = 0; u < num_nodes; u++)
    g->nodes[u].aux.info = &node_order_params_arr[u];

  /* Disregarding a backarc from each recursive cycle to obtain a DAG,
     calculate ASAP, ALAP, mobility, distance, and height for each node
     in the dependence (direct acyclic) graph.  */

  /* We assume that the nodes in the array are in topological order.  */

  max_asap = 0;
  for (u = 0; u < num_nodes; u++)
    {
      ddg_node_ptr u_node = &g->nodes[u];

      ASAP (u_node) = 0;
      for (e = u_node->in; e; e = e->next_in)
	if (e->distance == 0)
	  ASAP (u_node) = MAX (ASAP (u_node),
			       ASAP (e->src) + e->latency);
      max_asap = MAX (max_asap, ASAP (u_node));
    }

  for (u = num_nodes - 1; u > -1; u--)
    {
      ddg_node_ptr u_node = &g->nodes[u];

      ALAP (u_node) = max_asap;
      HEIGHT (u_node) = 0;
      for (e = u_node->out; e; e = e->next_out)
	if (e->distance == 0)
	  {
	    ALAP (u_node) = MIN (ALAP (u_node),
				 ALAP (e->dest) - e->latency);
	    HEIGHT (u_node) = MAX (HEIGHT (u_node),
				   HEIGHT (e->dest) + e->latency);
	  }
    }
  if (dump_file)
  {
    fprintf (dump_file, "\nOrder params\n");
    for (u = 0; u < num_nodes; u++)
      {
        ddg_node_ptr u_node = &g->nodes[u];

        fprintf (dump_file, "node %d, ASAP: %d, ALAP: %d, HEIGHT: %d\n", u,
                 ASAP (u_node), ALAP (u_node), HEIGHT (u_node));
      }
  }

  *pmax_asap = max_asap;
  return node_order_params_arr;
}

static int
find_max_asap (ddg_ptr g, sbitmap nodes)
{
  unsigned int u = 0;
  int max_asap = -1;
  int result = -1;
  sbitmap_iterator sbi;

  EXECUTE_IF_SET_IN_BITMAP (nodes, 0, u, sbi)
    {
      ddg_node_ptr u_node = &g->nodes[u];

      if (max_asap < ASAP (u_node))
	{
	  max_asap = ASAP (u_node);
	  result = u;
	}
    }
  return result;
}

static int
find_max_hv_min_mob (ddg_ptr g, sbitmap nodes)
{
  unsigned int u = 0;
  int max_hv = -1;
  int min_mob = INT_MAX;
  int result = -1;
  sbitmap_iterator sbi;

  EXECUTE_IF_SET_IN_BITMAP (nodes, 0, u, sbi)
    {
      ddg_node_ptr u_node = &g->nodes[u];

      if (max_hv < HEIGHT (u_node))
	{
	  max_hv = HEIGHT (u_node);
	  min_mob = MOB (u_node);
	  result = u;
	}
      else if ((max_hv == HEIGHT (u_node))
	       && (min_mob > MOB (u_node)))
	{
	  min_mob = MOB (u_node);
	  result = u;
	}
    }
  return result;
}

static int
find_max_dv_min_mob (ddg_ptr g, sbitmap nodes)
{
  unsigned int u = 0;
  int max_dv = -1;
  int min_mob = INT_MAX;
  int result = -1;
  sbitmap_iterator sbi;

  EXECUTE_IF_SET_IN_BITMAP (nodes, 0, u, sbi)
    {
      ddg_node_ptr u_node = &g->nodes[u];

      if (max_dv < DEPTH (u_node))
	{
	  max_dv = DEPTH (u_node);
	  min_mob = MOB (u_node);
	  result = u;
	}
      else if ((max_dv == DEPTH (u_node))
	       && (min_mob > MOB (u_node)))
	{
	  min_mob = MOB (u_node);
	  result = u;
	}
    }
  return result;
}

/* Places the nodes of SCC into the NODE_ORDER array starting
   at position POS, according to the SMS ordering algorithm.
   NODES_ORDERED (in&out parameter) holds the bitset of all nodes in
   the NODE_ORDER array, starting from position zero.  */
static int
order_nodes_in_scc (ddg_ptr g, sbitmap nodes_ordered, sbitmap scc,
		    int * node_order, int pos)
{
  enum sms_direction dir;
  int num_nodes = g->num_nodes;
  sbitmap workset = sbitmap_alloc (num_nodes);
  sbitmap tmp = sbitmap_alloc (num_nodes);
  sbitmap zero_bitmap = sbitmap_alloc (num_nodes);
  sbitmap predecessors = sbitmap_alloc (num_nodes);
  sbitmap successors = sbitmap_alloc (num_nodes);

  bitmap_clear (predecessors);
  find_predecessors (predecessors, g, nodes_ordered);

  bitmap_clear (successors);
  find_successors (successors, g, nodes_ordered);

  bitmap_clear (tmp);
  if (bitmap_and (tmp, predecessors, scc))
    {
      bitmap_copy (workset, tmp);
      dir = BOTTOMUP;
    }
  else if (bitmap_and (tmp, successors, scc))
    {
      bitmap_copy (workset, tmp);
      dir = TOPDOWN;
    }
  else
    {
      int u;

      bitmap_clear (workset);
      if ((u = find_max_asap (g, scc)) >= 0)
	bitmap_set_bit (workset, u);
      dir = BOTTOMUP;
    }

  bitmap_clear (zero_bitmap);
  while (!bitmap_equal_p (workset, zero_bitmap))
    {
      int v;
      ddg_node_ptr v_node;
      sbitmap v_node_preds;
      sbitmap v_node_succs;

      if (dir == TOPDOWN)
	{
	  while (!bitmap_equal_p (workset, zero_bitmap))
	    {
	      v = find_max_hv_min_mob (g, workset);
	      v_node = &g->nodes[v];
	      node_order[pos++] = v;
	      v_node_succs = NODE_SUCCESSORS (v_node);
	      bitmap_and (tmp, v_node_succs, scc);

	      /* Don't consider the already ordered successors again.  */
	      bitmap_and_compl (tmp, tmp, nodes_ordered);
	      bitmap_ior (workset, workset, tmp);
	      bitmap_clear_bit (workset, v);
	      bitmap_set_bit (nodes_ordered, v);
	    }
	  dir = BOTTOMUP;
	  bitmap_clear (predecessors);
	  find_predecessors (predecessors, g, nodes_ordered);
	  bitmap_and (workset, predecessors, scc);
	}
      else
	{
	  while (!bitmap_equal_p (workset, zero_bitmap))
	    {
	      v = find_max_dv_min_mob (g, workset);
	      v_node = &g->nodes[v];
	      node_order[pos++] = v;
	      v_node_preds = NODE_PREDECESSORS (v_node);
	      bitmap_and (tmp, v_node_preds, scc);

	      /* Don't consider the already ordered predecessors again.  */
	      bitmap_and_compl (tmp, tmp, nodes_ordered);
	      bitmap_ior (workset, workset, tmp);
	      bitmap_clear_bit (workset, v);
	      bitmap_set_bit (nodes_ordered, v);
	    }
	  dir = TOPDOWN;
	  bitmap_clear (successors);
	  find_successors (successors, g, nodes_ordered);
	  bitmap_and (workset, successors, scc);
	}
    }
  sbitmap_free (tmp);
  sbitmap_free (workset);
  sbitmap_free (zero_bitmap);
  sbitmap_free (predecessors);
  sbitmap_free (successors);
  return pos;
}


/* This page contains functions for manipulating partial-schedules during
   modulo scheduling.  */

/* Create a partial schedule and allocate a memory to hold II rows.  */

static partial_schedule_ptr
create_partial_schedule (int ii, ddg_ptr g, int history)
{
  partial_schedule_ptr ps = XNEW (struct partial_schedule);
  ps->rows = (ps_insn_ptr *) xcalloc (ii, sizeof (ps_insn_ptr));
  ps->rows_length = (int *) xcalloc (ii, sizeof (int));
  ps->reg_moves.create (0);
  ps->ii = ii;
  ps->history = history;
  ps->min_cycle = INT_MAX;
  ps->max_cycle = INT_MIN;
  ps->g = g;

  return ps;
}

/* Free the PS_INSNs in rows array of the given partial schedule.
   ??? Consider caching the PS_INSN's.  */
static void
free_ps_insns (partial_schedule_ptr ps)
{
  int i;

  for (i = 0; i < ps->ii; i++)
    {
      while (ps->rows[i])
	{
	  ps_insn_ptr ps_insn = ps->rows[i]->next_in_row;

	  free (ps->rows[i]);
	  ps->rows[i] = ps_insn;
	}
      ps->rows[i] = NULL;
    }
}

/* Free all the memory allocated to the partial schedule.  */

static void
free_partial_schedule (partial_schedule_ptr ps)
{
  ps_reg_move_info *move;
  unsigned int i;

  if (!ps)
    return;

  FOR_EACH_VEC_ELT (ps->reg_moves, i, move)
    sbitmap_free (move->uses);
  ps->reg_moves.release ();

  free_ps_insns (ps);
  free (ps->rows);
  free (ps->rows_length);
  free (ps);
}

/* Clear the rows array with its PS_INSNs, and create a new one with
   NEW_II rows.  */

static void
reset_partial_schedule (partial_schedule_ptr ps, int new_ii)
{
  if (!ps)
    return;
  free_ps_insns (ps);
  if (new_ii == ps->ii)
    return;
  ps->rows = (ps_insn_ptr *) xrealloc (ps->rows, new_ii
						 * sizeof (ps_insn_ptr));
  memset (ps->rows, 0, new_ii * sizeof (ps_insn_ptr));
  ps->rows_length = (int *) xrealloc (ps->rows_length, new_ii * sizeof (int));
  memset (ps->rows_length, 0, new_ii * sizeof (int));
  ps->ii = new_ii;
  ps->min_cycle = INT_MAX;
  ps->max_cycle = INT_MIN;
}

/* Prints the partial schedule as an ii rows array, for each rows
   print the ids of the insns in it.  */
void
print_partial_schedule (partial_schedule_ptr ps, FILE *dump)
{
  int i;

  for (i = 0; i < ps->ii; i++)
    {
      ps_insn_ptr ps_i = ps->rows[i];

      fprintf (dump, "\n[ROW %d ]: ", i);
      while (ps_i)
	{
	  rtx insn = ps_rtl_insn (ps, ps_i->id);

	  if (JUMP_P (insn))
	    fprintf (dump, "%d (branch), ", INSN_UID (insn));
	  else
	    fprintf (dump, "%d, ", INSN_UID (insn));
	
	  ps_i = ps_i->next_in_row;
	}
    }
}

/* Creates an object of PS_INSN and initializes it to the given parameters.  */
static ps_insn_ptr
create_ps_insn (int id, int cycle)
{
  ps_insn_ptr ps_i = XNEW (struct ps_insn);

  ps_i->id = id;
  ps_i->next_in_row = NULL;
  ps_i->prev_in_row = NULL;
  ps_i->cycle = cycle;

  return ps_i;
}


/* Removes the given PS_INSN from the partial schedule.  */  
static void 
remove_node_from_ps (partial_schedule_ptr ps, ps_insn_ptr ps_i)
{
  int row;

  gcc_assert (ps && ps_i);
  
  row = SMODULO (ps_i->cycle, ps->ii);
  if (! ps_i->prev_in_row)
    {
      gcc_assert (ps_i == ps->rows[row]);
      ps->rows[row] = ps_i->next_in_row;
      if (ps->rows[row])
	ps->rows[row]->prev_in_row = NULL;
    }
  else
    {
      ps_i->prev_in_row->next_in_row = ps_i->next_in_row;
      if (ps_i->next_in_row)
	ps_i->next_in_row->prev_in_row = ps_i->prev_in_row;
    }
   
  ps->rows_length[row] -= 1; 
  free (ps_i);
  return;
}

/* Unlike what literature describes for modulo scheduling (which focuses
   on VLIW machines) the order of the instructions inside a cycle is
   important.  Given the bitmaps MUST_FOLLOW and MUST_PRECEDE we know
   where the current instruction should go relative to the already
   scheduled instructions in the given cycle.  Go over these
   instructions and find the first possible column to put it in.  */
static bool
ps_insn_find_column (partial_schedule_ptr ps, ps_insn_ptr ps_i,
		     sbitmap must_precede, sbitmap must_follow)
{
  ps_insn_ptr next_ps_i;
  ps_insn_ptr first_must_follow = NULL;
  ps_insn_ptr last_must_precede = NULL;
  ps_insn_ptr last_in_row = NULL;
  int row;

  if (! ps_i)
    return false;

  row = SMODULO (ps_i->cycle, ps->ii);

  /* Find the first must follow and the last must precede
     and insert the node immediately after the must precede
     but make sure that it there is no must follow after it.  */
  for (next_ps_i = ps->rows[row];
       next_ps_i;
       next_ps_i = next_ps_i->next_in_row)
    {
      if (must_follow
	  && bitmap_bit_p (must_follow, next_ps_i->id)
	  && ! first_must_follow)
        first_must_follow = next_ps_i;
      if (must_precede && bitmap_bit_p (must_precede, next_ps_i->id))
        {
          /* If we have already met a node that must follow, then
	     there is no possible column.  */
  	  if (first_must_follow)
            return false;
	  else
            last_must_precede = next_ps_i;
        }
      /* The closing branch must be the last in the row.  */
      if (must_precede 
	  && bitmap_bit_p (must_precede, next_ps_i->id)
	  && JUMP_P (ps_rtl_insn (ps, next_ps_i->id)))
	return false;
             
       last_in_row = next_ps_i;
    }

  /* The closing branch is scheduled as well.  Make sure there is no
     dependent instruction after it as the branch should be the last
     instruction in the row.  */
  if (JUMP_P (ps_rtl_insn (ps, ps_i->id)))
    {
      if (first_must_follow)
	return false;
      if (last_in_row)
	{
	  /* Make the branch the last in the row.  New instructions
	     will be inserted at the beginning of the row or after the
	     last must_precede instruction thus the branch is guaranteed
	     to remain the last instruction in the row.  */
	  last_in_row->next_in_row = ps_i;
	  ps_i->prev_in_row = last_in_row;
	  ps_i->next_in_row = NULL;
	}
      else
	ps->rows[row] = ps_i;
      return true;
    }
  
  /* Now insert the node after INSERT_AFTER_PSI.  */

  if (! last_must_precede)
    {
      ps_i->next_in_row = ps->rows[row];
      ps_i->prev_in_row = NULL;
      if (ps_i->next_in_row)
    	ps_i->next_in_row->prev_in_row = ps_i;
      ps->rows[row] = ps_i;
    }
  else
    {
      ps_i->next_in_row = last_must_precede->next_in_row;
      last_must_precede->next_in_row = ps_i;
      ps_i->prev_in_row = last_must_precede;
      if (ps_i->next_in_row)
        ps_i->next_in_row->prev_in_row = ps_i;
    }

  return true;
}

/* Advances the PS_INSN one column in its current row; returns false
   in failure and true in success.  Bit N is set in MUST_FOLLOW if
   the node with cuid N must be come after the node pointed to by
   PS_I when scheduled in the same cycle.  */
static int
ps_insn_advance_column (partial_schedule_ptr ps, ps_insn_ptr ps_i,
			sbitmap must_follow)
{
  ps_insn_ptr prev, next;
  int row;

  if (!ps || !ps_i)
    return false;

  row = SMODULO (ps_i->cycle, ps->ii);

  if (! ps_i->next_in_row)
    return false;

  /* Check if next_in_row is dependent on ps_i, both having same sched
     times (typically ANTI_DEP).  If so, ps_i cannot skip over it.  */
  if (must_follow && bitmap_bit_p (must_follow, ps_i->next_in_row->id))
    return false;

  /* Advance PS_I over its next_in_row in the doubly linked list.  */
  prev = ps_i->prev_in_row;
  next = ps_i->next_in_row;

  if (ps_i == ps->rows[row])
    ps->rows[row] = next;

  ps_i->next_in_row = next->next_in_row;

  if (next->next_in_row)
    next->next_in_row->prev_in_row = ps_i;

  next->next_in_row = ps_i;
  ps_i->prev_in_row = next;

  next->prev_in_row = prev;
  if (prev)
    prev->next_in_row = next;

  return true;
}

/* Inserts a DDG_NODE to the given partial schedule at the given cycle.
   Returns 0 if this is not possible and a PS_INSN otherwise.  Bit N is
   set in MUST_PRECEDE/MUST_FOLLOW if the node with cuid N must be come
   before/after (respectively) the node pointed to by PS_I when scheduled
   in the same cycle.  */
static ps_insn_ptr
add_node_to_ps (partial_schedule_ptr ps, int id, int cycle,
		sbitmap must_precede, sbitmap must_follow)
{
  ps_insn_ptr ps_i;
  int row = SMODULO (cycle, ps->ii);

  if (ps->rows_length[row] >= issue_rate)
    return NULL;

  ps_i = create_ps_insn (id, cycle);

  /* Finds and inserts PS_I according to MUST_FOLLOW and
     MUST_PRECEDE.  */
  if (! ps_insn_find_column (ps, ps_i, must_precede, must_follow))
    {
      free (ps_i);
      return NULL;
    }

  ps->rows_length[row] += 1;
  return ps_i;
}

/* Advance time one cycle.  Assumes DFA is being used.  */
static void
advance_one_cycle (void)
{
  if (targetm.sched.dfa_pre_cycle_insn)
    state_transition (curr_state,
		      targetm.sched.dfa_pre_cycle_insn ());

  state_transition (curr_state, NULL);

  if (targetm.sched.dfa_post_cycle_insn)
    state_transition (curr_state,
		      targetm.sched.dfa_post_cycle_insn ());
}



/* Checks if PS has resource conflicts according to DFA, starting from
   FROM cycle to TO cycle; returns true if there are conflicts and false
   if there are no conflicts.  Assumes DFA is being used.  */
static int
ps_has_conflicts (partial_schedule_ptr ps, int from, int to)
{
  int cycle;

  state_reset (curr_state);

  for (cycle = from; cycle <= to; cycle++)
    {
      ps_insn_ptr crr_insn;
      /* Holds the remaining issue slots in the current row.  */
      int can_issue_more = issue_rate;

      /* Walk through the DFA for the current row.  */
      for (crr_insn = ps->rows[SMODULO (cycle, ps->ii)];
	   crr_insn;
	   crr_insn = crr_insn->next_in_row)
	{
	  rtx insn = ps_rtl_insn (ps, crr_insn->id);

	  if (!NONDEBUG_INSN_P (insn))
	    continue;

	  /* Check if there is room for the current insn.  */
	  if (!can_issue_more || state_dead_lock_p (curr_state))
	    return true;

	  /* Update the DFA state and return with failure if the DFA found
	     resource conflicts.  */
	  if (state_transition (curr_state, insn) >= 0)
	    return true;

	  if (targetm.sched.variable_issue)
	    can_issue_more =
	      targetm.sched.variable_issue (sched_dump, sched_verbose,
					    insn, can_issue_more);
	  /* A naked CLOBBER or USE generates no instruction, so don't
	     let them consume issue slots.  */
	  else if (GET_CODE (PATTERN (insn)) != USE
		   && GET_CODE (PATTERN (insn)) != CLOBBER)
	    can_issue_more--;
	}

      /* Advance the DFA to the next cycle.  */
      advance_one_cycle ();
    }
  return false;
}

/* Checks if the given node causes resource conflicts when added to PS at
   cycle C.  If not the node is added to PS and returned; otherwise zero
   is returned.  Bit N is set in MUST_PRECEDE/MUST_FOLLOW if the node with
   cuid N must be come before/after (respectively) the node pointed to by
   PS_I when scheduled in the same cycle.  */
ps_insn_ptr
ps_add_node_check_conflicts (partial_schedule_ptr ps, int n,
   			     int c, sbitmap must_precede,
			     sbitmap must_follow)
{
  int has_conflicts = 0;
  ps_insn_ptr ps_i;

  /* First add the node to the PS, if this succeeds check for
     conflicts, trying different issue slots in the same row.  */
  if (! (ps_i = add_node_to_ps (ps, n, c, must_precede, must_follow)))
    return NULL; /* Failed to insert the node at the given cycle.  */

  has_conflicts = ps_has_conflicts (ps, c, c)
		  || (ps->history > 0
		      && ps_has_conflicts (ps,
					   c - ps->history,
					   c + ps->history));

  /* Try different issue slots to find one that the given node can be
     scheduled in without conflicts.  */
  while (has_conflicts)
    {
      if (! ps_insn_advance_column (ps, ps_i, must_follow))
	break;
      has_conflicts = ps_has_conflicts (ps, c, c)
		      || (ps->history > 0
			  && ps_has_conflicts (ps,
					       c - ps->history,
					       c + ps->history));
    }

  if (has_conflicts)
    {
      remove_node_from_ps (ps, ps_i);
      return NULL;
    }

  ps->min_cycle = MIN (ps->min_cycle, c);
  ps->max_cycle = MAX (ps->max_cycle, c);
  return ps_i;
}

/* Calculate the stage count of the partial schedule PS.  The calculation
   takes into account the rotation amount passed in ROTATION_AMOUNT.  */
int
calculate_stage_count (partial_schedule_ptr ps, int rotation_amount)
{
  int new_min_cycle = PS_MIN_CYCLE (ps) - rotation_amount;
  int new_max_cycle = PS_MAX_CYCLE (ps) - rotation_amount;
  int stage_count = CALC_STAGE_COUNT (-1, new_min_cycle, ps->ii);

  /* The calculation of stage count is done adding the number of stages
     before cycle zero and after cycle zero.  */ 
  stage_count += CALC_STAGE_COUNT (new_max_cycle, 0, ps->ii);

  return stage_count;
}

/* Rotate the rows of PS such that insns scheduled at time
   START_CYCLE will appear in row 0.  Updates max/min_cycles.  */
void
rotate_partial_schedule (partial_schedule_ptr ps, int start_cycle)
{
  int i, row, backward_rotates;
  int last_row = ps->ii - 1;

  if (start_cycle == 0)
    return;

  backward_rotates = SMODULO (start_cycle, ps->ii);

  /* Revisit later and optimize this into a single loop.  */
  for (i = 0; i < backward_rotates; i++)
    {
      ps_insn_ptr first_row = ps->rows[0];
      int first_row_length = ps->rows_length[0];

      for (row = 0; row < last_row; row++)
	{
	  ps->rows[row] = ps->rows[row + 1];
	  ps->rows_length[row] = ps->rows_length[row + 1]; 
	}

      ps->rows[last_row] = first_row;
      ps->rows_length[last_row] = first_row_length;
    }

  ps->max_cycle -= start_cycle;
  ps->min_cycle -= start_cycle;
}

#endif /* INSN_SCHEDULING */

static bool
gate_handle_sms (void)
{
  return (optimize > 0 && flag_modulo_sched);
}


/* Run instruction scheduler.  */
/* Perform SMS module scheduling.  */
static unsigned int
rest_of_handle_sms (void)
{
#ifdef INSN_SCHEDULING
  basic_block bb;

  /* Collect loop information to be used in SMS.  */
  cfg_layout_initialize (0);
  sms_schedule ();

  /* Update the life information, because we add pseudos.  */
  max_regno = max_reg_num ();

  /* Finalize layout changes.  */
  FOR_EACH_BB_FN (bb, cfun)
    if (bb->next_bb != EXIT_BLOCK_PTR_FOR_FN (cfun))
      bb->aux = bb->next_bb;
  free_dominance_info (CDI_DOMINATORS);
  cfg_layout_finalize ();
#endif /* INSN_SCHEDULING */
  return 0;
}

namespace {

const pass_data pass_data_sms =
{
  RTL_PASS, /* type */
  "sms", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_SMS, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_df_finish | TODO_verify_flow
    | TODO_verify_rtl_sharing ), /* todo_flags_finish */
};

class pass_sms : public rtl_opt_pass
{
public:
  pass_sms (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_sms, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_handle_sms (); }
  unsigned int execute () { return rest_of_handle_sms (); }

}; // class pass_sms

} // anon namespace

rtl_opt_pass *
make_pass_sms (gcc::context *ctxt)
{
  return new pass_sms (ctxt);
}
