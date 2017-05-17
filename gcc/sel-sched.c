/* Instruction scheduling pass.  Selective scheduler and pipeliner.
   Copyright (C) 2006-2016 Free Software Foundation, Inc.

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
#include "backend.h"
#include "tree.h"
#include "rtl.h"
#include "df.h"
#include "tm_p.h"
#include "regs.h"
#include "cfgbuild.h"
#include "insn-config.h"
#include "insn-attr.h"
#include "params.h"
#include "target.h"
#include "sched-int.h"
#include "rtlhooks-def.h"
#include "ira.h"
#include "ira-int.h"
#include "rtl-iter.h"

#ifdef INSN_SCHEDULING
#include "regset.h"
#include "cfgloop.h"
#include "sel-sched-ir.h"
#include "sel-sched-dump.h"
#include "sel-sched.h"
#include "dbgcnt.h"

/* Implementation of selective scheduling approach.
   The below implementation follows the original approach with the following
   changes:

   o the scheduler works after register allocation (but can be also tuned
   to work before RA);
   o some instructions are not copied or register renamed;
   o conditional jumps are not moved with code duplication;
   o several jumps in one parallel group are not supported;
   o when pipelining outer loops, code motion through inner loops
   is not supported;
   o control and data speculation are supported;
   o some improvements for better compile time/performance were made.

   Terminology
   ===========

   A vinsn, or virtual insn, is an insn with additional data characterizing
   insn pattern, such as LHS, RHS, register sets used/set/clobbered, etc.
   Vinsns also act as smart pointers to save memory by reusing them in
   different expressions.  A vinsn is described by vinsn_t type.

   An expression is a vinsn with additional data characterizing its properties
   at some point in the control flow graph.  The data may be its usefulness,
   priority, speculative status, whether it was renamed/subsituted, etc.
   An expression is described by expr_t type.

   Availability set (av_set) is a set of expressions at a given control flow
   point. It is represented as av_set_t.  The expressions in av sets are kept
   sorted in the terms of expr_greater_p function.  It allows to truncate
   the set while leaving the best expressions.

   A fence is a point through which code motion is prohibited.  On each step,
   we gather a parallel group of insns at a fence.  It is possible to have
   multiple fences. A fence is represented via fence_t.

   A boundary is the border between the fence group and the rest of the code.
   Currently, we never have more than one boundary per fence, as we finalize
   the fence group when a jump is scheduled. A boundary is represented
   via bnd_t.

   High-level overview
   ===================

   The scheduler finds regions to schedule, schedules each one, and finalizes.
   The regions are formed starting from innermost loops, so that when the inner
   loop is pipelined, its prologue can be scheduled together with yet unprocessed
   outer loop. The rest of acyclic regions are found using extend_rgns:
   the blocks that are not yet allocated to any regions are traversed in top-down
   order, and a block is added to a region to which all its predecessors belong;
   otherwise, the block starts its own region.

   The main scheduling loop (sel_sched_region_2) consists of just
   scheduling on each fence and updating fences.  For each fence,
   we fill a parallel group of insns (fill_insns) until some insns can be added.
   First, we compute available exprs (av-set) at the boundary of the current
   group.  Second, we choose the best expression from it.  If the stall is
   required to schedule any of the expressions, we advance the current cycle
   appropriately.  So, the final group does not exactly correspond to a VLIW
   word.  Third, we move the chosen expression to the boundary (move_op)
   and update the intermediate av sets and liveness sets.  We quit fill_insns
   when either no insns left for scheduling or we have scheduled enough insns
   so we feel like advancing a scheduling point.

   Computing available expressions
   ===============================

   The computation (compute_av_set) is a bottom-up traversal.  At each insn,
   we're moving the union of its successors' sets through it via
   moveup_expr_set.  The dependent expressions are removed.  Local
   transformations (substitution, speculation) are applied to move more
   exprs.  Then the expr corresponding to the current insn is added.
   The result is saved on each basic block header.

   When traversing the CFG, we're moving down for no more than max_ws insns.
   Also, we do not move down to ineligible successors (is_ineligible_successor),
   which include moving along a back-edge, moving to already scheduled code,
   and moving to another fence.  The first two restrictions are lifted during
   pipelining, which allows us to move insns along a back-edge.  We always have
   an acyclic region for scheduling because we forbid motion through fences.

   Choosing the best expression
   ============================

   We sort the final availability set via sel_rank_for_schedule, then we remove
   expressions which are not yet ready (tick_check_p) or which dest registers
   cannot be used.  For some of them, we choose another register via
   find_best_reg.  To do this, we run find_used_regs to calculate the set of
   registers which cannot be used.  The find_used_regs function performs
   a traversal of code motion paths for an expr.  We consider for renaming
   only registers which are from the same regclass as the original one and
   using which does not interfere with any live ranges.  Finally, we convert
   the resulting set to the ready list format and use max_issue and reorder*
   hooks similarly to the Haifa scheduler.

   Scheduling the best expression
   ==============================

   We run the move_op routine to perform the same type of code motion paths
   traversal as in find_used_regs.  (These are working via the same driver,
   code_motion_path_driver.)  When moving down the CFG, we look for original
   instruction that gave birth to a chosen expression.  We undo
   the transformations performed on an expression via the history saved in it.
   When found, we remove the instruction or leave a reg-reg copy/speculation
   check if needed.  On a way up, we insert bookkeeping copies at each join
   point.  If a copy is not needed, it will be removed later during this
   traversal.  We update the saved av sets and liveness sets on the way up, too.

   Finalizing the schedule
   =======================

   When pipelining, we reschedule the blocks from which insns were pipelined
   to get a tighter schedule.  On Itanium, we also perform bundling via
   the same routine from ia64.c.

   Dependence analysis changes
   ===========================

   We augmented the sched-deps.c with hooks that get called when a particular
   dependence is found in a particular part of an insn.  Using these hooks, we
   can do several actions such as: determine whether an insn can be moved through
   another (has_dependence_p, moveup_expr); find out whether an insn can be
   scheduled on the current cycle (tick_check_p); find out registers that
   are set/used/clobbered by an insn and find out all the strange stuff that
   restrict its movement, like SCHED_GROUP_P or CANT_MOVE (done in
   init_global_and_expr_for_insn).

   Initialization changes
   ======================

   There are parts of haifa-sched.c, sched-deps.c, and sched-rgn.c that are
   reused in all of the schedulers.  We have split up the initialization of data
   of such parts into different functions prefixed with scheduler type and
   postfixed with the type of data initialized: {,sel_,haifa_}sched_{init,finish},
   sched_rgn_init/finish, sched_deps_init/finish, sched_init_{luids/bbs}, etc.
   The same splitting is done with current_sched_info structure:
   dependence-related parts are in sched_deps_info, common part is in
   common_sched_info, and haifa/sel/etc part is in current_sched_info.

   Target contexts
   ===============

   As we now have multiple-point scheduling, this would not work with backends
   which save some of the scheduler state to use it in the target hooks.
   For this purpose, we introduce a concept of target contexts, which
   encapsulate such information.  The backend should implement simple routines
   of allocating/freeing/setting such a context.  The scheduler calls these
   as target hooks and handles the target context as an opaque pointer (similar
   to the DFA state type, state_t).

   Various speedups
   ================

   As the correct data dependence graph is not supported during scheduling (which
   is to be changed in mid-term), we cache as much of the dependence analysis
   results as possible to avoid reanalyzing.  This includes: bitmap caches on
   each insn in stream of the region saying yes/no for a query with a pair of
   UIDs; hashtables with the previously done transformations on each insn in
   stream; a vector keeping a history of transformations on each expr.

   Also, we try to minimize the dependence context used on each fence to check
   whether the given expression is ready for scheduling by removing from it
   insns that are definitely completed the execution.  The results of
   tick_check_p checks are also cached in a vector on each fence.

   We keep a valid liveness set on each insn in a region to avoid the high
   cost of recomputation on large basic blocks.

   Finally, we try to minimize the number of needed updates to the availability
   sets.  The updates happen in two cases: when fill_insns terminates,
   we advance all fences and increase the stage number to show that the region
   has changed and the sets are to be recomputed; and when the next iteration
   of a loop in fill_insns happens (but this one reuses the saved av sets
   on bb headers.)  Thus, we try to break the fill_insns loop only when
   "significant" number of insns from the current scheduling window was
   scheduled.  This should be made a target param.


   TODO: correctly support the data dependence graph at all stages and get rid
   of all caches.  This should speed up the scheduler.
   TODO: implement moving cond jumps with bookkeeping copies on both targets.
   TODO: tune the scheduler before RA so it does not create too much pseudos.


   References:
   S.-M. Moon and K. Ebcioglu. Parallelizing nonnumerical code with
   selective scheduling and software pipelining.
   ACM TOPLAS, Vol 19, No. 6, pages 853--898, Nov. 1997.

   Andrey Belevantsev, Maxim Kuvyrkov, Vladimir Makarov, Dmitry Melnik,
   and Dmitry Zhurikhin.  An interblock VLIW-targeted instruction scheduler
   for GCC. In Proceedings of GCC Developers' Summit 2006.

   Arutyun Avetisyan, Andrey Belevantsev, and Dmitry Melnik.  GCC Instruction
   Scheduler and Software Pipeliner on the Itanium Platform.   EPIC-7 Workshop.
   http://rogue.colorado.edu/EPIC7/.

*/

/* True when pipelining is enabled.  */
bool pipelining_p;

/* True if bookkeeping is enabled.  */
bool bookkeeping_p;

/* Maximum number of insns that are eligible for renaming.  */
int max_insns_to_rename;


/* Definitions of local types and macros.  */

/* Represents possible outcomes of moving an expression through an insn.  */
enum MOVEUP_EXPR_CODE
  {
    /* The expression is not changed.  */
    MOVEUP_EXPR_SAME,

    /* Not changed, but requires a new destination register.  */
    MOVEUP_EXPR_AS_RHS,

    /* Cannot be moved.  */
    MOVEUP_EXPR_NULL,

    /* Changed (substituted or speculated).  */
    MOVEUP_EXPR_CHANGED
  };

/* The container to be passed into rtx search & replace functions.  */
struct rtx_search_arg
{
  /* What we are searching for.  */
  rtx x;

  /* The occurrence counter.  */
  int n;
};

typedef struct rtx_search_arg *rtx_search_arg_p;

/* This struct contains precomputed hard reg sets that are needed when
   computing registers available for renaming.  */
struct hard_regs_data
{
  /* For every mode, this stores registers available for use with
     that mode.  */
  HARD_REG_SET regs_for_mode[NUM_MACHINE_MODES];

  /* True when regs_for_mode[mode] is initialized.  */
  bool regs_for_mode_ok[NUM_MACHINE_MODES];

  /* For every register, it has regs that are ok to rename into it.
     The register in question is always set.  If not, this means
     that the whole set is not computed yet.  */
  HARD_REG_SET regs_for_rename[FIRST_PSEUDO_REGISTER];

  /* For every mode, this stores registers not available due to
     call clobbering.  */
  HARD_REG_SET regs_for_call_clobbered[NUM_MACHINE_MODES];

  /* All registers that are used or call used.  */
  HARD_REG_SET regs_ever_used;

#ifdef STACK_REGS
  /* Stack registers.  */
  HARD_REG_SET stack_regs;
#endif
};

/* Holds the results of computation of available for renaming and
   unavailable hard registers.  */
struct reg_rename
{
  /* These are unavailable due to calls crossing, globalness, etc.  */
  HARD_REG_SET unavailable_hard_regs;

  /* These are *available* for renaming.  */
  HARD_REG_SET available_for_renaming;

  /* Whether this code motion path crosses a call.  */
  bool crosses_call;
};

/* A global structure that contains the needed information about harg
   regs.  */
static struct hard_regs_data sel_hrd;


/* This structure holds local data used in code_motion_path_driver hooks on
   the same or adjacent levels of recursion.  Here we keep those parameters
   that are not used in code_motion_path_driver routine itself, but only in
   its hooks.  Moreover, all parameters that can be modified in hooks are
   in this structure, so all other parameters passed explicitly to hooks are
   read-only.  */
struct cmpd_local_params
{
  /* Local params used in move_op_* functions.  */

  /* Edges for bookkeeping generation.  */
  edge e1, e2;

  /* C_EXPR merged from all successors and locally allocated temporary C_EXPR.  */
  expr_t c_expr_merged, c_expr_local;

  /* Local params used in fur_* functions.  */
  /* Copy of the ORIGINAL_INSN list, stores the original insns already
     found before entering the current level of code_motion_path_driver.  */
  def_list_t old_original_insns;

  /* Local params used in move_op_* functions.  */
  /* True when we have removed last insn in the block which was
     also a boundary.  Do not update anything or create bookkeeping copies.  */
  BOOL_BITFIELD removed_last_insn : 1;
};

/* Stores the static parameters for move_op_* calls.  */
struct moveop_static_params
{
  /* Destination register.  */
  rtx dest;

  /* Current C_EXPR.  */
  expr_t c_expr;

  /* An UID of expr_vliw which is to be moved up.  If we find other exprs,
     they are to be removed.  */
  int uid;

  /* This is initialized to the insn on which the driver stopped its traversal.  */
  insn_t failed_insn;

  /* True if we scheduled an insn with different register.  */
  bool was_renamed;
};

/* Stores the static parameters for fur_* calls.  */
struct fur_static_params
{
  /* Set of registers unavailable on the code motion path.  */
  regset used_regs;

  /* Pointer to the list of original insns definitions.  */
  def_list_t *original_insns;

  /* True if a code motion path contains a CALL insn.  */
  bool crosses_call;
};

typedef struct fur_static_params *fur_static_params_p;
typedef struct cmpd_local_params *cmpd_local_params_p;
typedef struct moveop_static_params *moveop_static_params_p;

/* Set of hooks and parameters that determine behavior specific to
   move_op or find_used_regs functions.  */
struct code_motion_path_driver_info_def
{
  /* Called on enter to the basic block.  */
  int (*on_enter) (insn_t, cmpd_local_params_p, void *, bool);

  /* Called when original expr is found.  */
  void (*orig_expr_found) (insn_t, expr_t, cmpd_local_params_p, void *);

  /* Called while descending current basic block if current insn is not
     the original EXPR we're searching for.  */
  bool (*orig_expr_not_found) (insn_t, av_set_t, void *);

  /* Function to merge C_EXPRes from different successors.  */
  void (*merge_succs) (insn_t, insn_t, int, cmpd_local_params_p, void *);

  /* Function to finalize merge from different successors and possibly
     deallocate temporary data structures used for merging.  */
  void (*after_merge_succs) (cmpd_local_params_p, void *);

  /* Called on the backward stage of recursion to do moveup_expr.
     Used only with move_op_*.  */
  void (*ascend) (insn_t, void *);

  /* Called on the ascending pass, before returning from the current basic
     block or from the whole traversal.  */
  void (*at_first_insn) (insn_t, cmpd_local_params_p, void *);

  /* When processing successors in move_op we need only descend into
     SUCCS_NORMAL successors, while in find_used_regs we need SUCCS_ALL.  */
  int succ_flags;

  /* The routine name to print in dumps ("move_op" of "find_used_regs").  */
  const char *routine_name;
};

/* Global pointer to current hooks, either points to MOVE_OP_HOOKS or
   FUR_HOOKS.  */
struct code_motion_path_driver_info_def *code_motion_path_driver_info;

/* Set of hooks for performing move_op and find_used_regs routines with
   code_motion_path_driver.  */
extern struct code_motion_path_driver_info_def move_op_hooks, fur_hooks;

/* True if/when we want to emulate Haifa scheduler in the common code.
   This is used in sched_rgn_local_init and in various places in
   sched-deps.c.  */
int sched_emulate_haifa_p;

/* GLOBAL_LEVEL is used to discard information stored in basic block headers
   av_sets.  Av_set of bb header is valid if its (bb header's) level is equal
   to GLOBAL_LEVEL.  And invalid if lesser.  This is primarily used to advance
   scheduling window.  */
int global_level;

/* Current fences.  */
flist_t fences;

/* True when separable insns should be scheduled as RHSes.  */
static bool enable_schedule_as_rhs_p;

/* Used in verify_target_availability to assert that target reg is reported
   unavailabile by both TARGET_UNAVAILABLE and find_used_regs only if
   we haven't scheduled anything on the previous fence.
   if scheduled_something_on_previous_fence is true, TARGET_UNAVAILABLE can
   have more conservative value than the one returned by the
   find_used_regs, thus we shouldn't assert that these values are equal.  */
static bool scheduled_something_on_previous_fence;

/* All newly emitted insns will have their uids greater than this value.  */
static int first_emitted_uid;

/* Set of basic blocks that are forced to start new ebbs.  This is a subset
   of all the ebb heads.  */
static bitmap_head _forced_ebb_heads;
bitmap_head *forced_ebb_heads = &_forced_ebb_heads;

/* Blocks that need to be rescheduled after pipelining.  */
bitmap blocks_to_reschedule = NULL;

/* True when the first lv set should be ignored when updating liveness.  */
static bool ignore_first = false;

/* Number of insns max_issue has initialized data structures for.  */
static int max_issue_size = 0;

/* Whether we can issue more instructions.  */
static int can_issue_more;

/* Maximum software lookahead window size, reduced when rescheduling after
   pipelining.  */
static int max_ws;

/* Number of insns scheduled in current region.  */
static int num_insns_scheduled;

/* A vector of expressions is used to be able to sort them.  */
static vec<expr_t> vec_av_set = vNULL;

/* A vector of vinsns is used to hold temporary lists of vinsns.  */
typedef vec<vinsn_t> vinsn_vec_t;

/* This vector has the exprs which may still present in av_sets, but actually
   can't be moved up due to bookkeeping created during code motion to another
   fence.  See comment near the call to update_and_record_unavailable_insns
   for the detailed explanations.  */
static vinsn_vec_t vec_bookkeeping_blocked_vinsns = vinsn_vec_t ();

/* This vector has vinsns which are scheduled with renaming on the first fence
   and then seen on the second.  For expressions with such vinsns, target
   availability information may be wrong.  */
static vinsn_vec_t vec_target_unavailable_vinsns = vinsn_vec_t ();

/* Vector to store temporary nops inserted in move_op to prevent removal
   of empty bbs.  */
static vec<insn_t> vec_temp_moveop_nops = vNULL;

/* These bitmaps record original instructions scheduled on the current
   iteration and bookkeeping copies created by them.  */
static bitmap current_originators = NULL;
static bitmap current_copies = NULL;

/* This bitmap marks the blocks visited by code_motion_path_driver so we don't
   visit them afterwards.  */
static bitmap code_motion_visited_blocks = NULL;

/* Variables to accumulate different statistics.  */

/* The number of bookkeeping copies created.  */
static int stat_bookkeeping_copies;

/* The number of insns that required bookkeeiping for their scheduling.  */
static int stat_insns_needed_bookkeeping;

/* The number of insns that got renamed.  */
static int stat_renamed_scheduled;

/* The number of substitutions made during scheduling.  */
static int stat_substitutions_total;


/* Forward declarations of static functions.  */
static bool rtx_ok_for_substitution_p (rtx, rtx);
static int sel_rank_for_schedule (const void *, const void *);
static av_set_t find_sequential_best_exprs (bnd_t, expr_t, bool);
static basic_block find_block_for_bookkeeping (edge e1, edge e2, bool lax);

static rtx get_dest_from_orig_ops (av_set_t);
static basic_block generate_bookkeeping_insn (expr_t, edge, edge);
static bool find_used_regs (insn_t, av_set_t, regset, struct reg_rename *,
                            def_list_t *);
static bool move_op (insn_t, av_set_t, expr_t, rtx, expr_t, bool*);
static int code_motion_path_driver (insn_t, av_set_t, ilist_t,
                                    cmpd_local_params_p, void *);
static void sel_sched_region_1 (void);
static void sel_sched_region_2 (int);
static av_set_t compute_av_set_inside_bb (insn_t, ilist_t, int, bool);

static void debug_state (state_t);


/* Functions that work with fences.  */

/* Advance one cycle on FENCE.  */
static void
advance_one_cycle (fence_t fence)
{
  unsigned i;
  int cycle;
  rtx_insn *insn;

  advance_state (FENCE_STATE (fence));
  cycle = ++FENCE_CYCLE (fence);
  FENCE_ISSUED_INSNS (fence) = 0;
  FENCE_STARTS_CYCLE_P (fence) = 1;
  can_issue_more = issue_rate;
  FENCE_ISSUE_MORE (fence) = can_issue_more;

  for (i = 0; vec_safe_iterate (FENCE_EXECUTING_INSNS (fence), i, &insn); )
    {
      if (INSN_READY_CYCLE (insn) < cycle)
        {
          remove_from_deps (FENCE_DC (fence), insn);
          FENCE_EXECUTING_INSNS (fence)->unordered_remove (i);
          continue;
        }
      i++;
    }
  if (sched_verbose >= 2)
    {
      sel_print ("Finished a cycle.  Current cycle = %d\n", FENCE_CYCLE (fence));
      debug_state (FENCE_STATE (fence));
    }
}

/* Returns true when SUCC in a fallthru bb of INSN, possibly
   skipping empty basic blocks.  */
static bool
in_fallthru_bb_p (rtx_insn *insn, rtx succ)
{
  basic_block bb = BLOCK_FOR_INSN (insn);
  edge e;

  if (bb == BLOCK_FOR_INSN (succ))
    return true;

  e = find_fallthru_edge_from (bb);
  if (e)
    bb = e->dest;
  else
    return false;

  while (sel_bb_empty_p (bb))
    bb = bb->next_bb;

  return bb == BLOCK_FOR_INSN (succ);
}

/* Construct successor fences from OLD_FENCEs and put them in NEW_FENCES.
   When a successor will continue a ebb, transfer all parameters of a fence
   to the new fence.  ORIG_MAX_SEQNO is the maximal seqno before this round
   of scheduling helping to distinguish between the old and the new code.  */
static void
extract_new_fences_from (flist_t old_fences, flist_tail_t new_fences,
			 int orig_max_seqno)
{
  bool was_here_p = false;
  insn_t insn = NULL;
  insn_t succ;
  succ_iterator si;
  ilist_iterator ii;
  fence_t fence = FLIST_FENCE (old_fences);
  basic_block bb;

  /* Get the only element of FENCE_BNDS (fence).  */
  FOR_EACH_INSN (insn, ii, FENCE_BNDS (fence))
    {
      gcc_assert (!was_here_p);
      was_here_p = true;
    }
  gcc_assert (was_here_p && insn != NULL_RTX);

  /* When in the "middle" of the block, just move this fence
     to the new list.  */
  bb = BLOCK_FOR_INSN (insn);
  if (! sel_bb_end_p (insn)
      || (single_succ_p (bb)
          && single_pred_p (single_succ (bb))))
    {
      insn_t succ;

      succ = (sel_bb_end_p (insn)
              ? sel_bb_head (single_succ (bb))
              : NEXT_INSN (insn));

      if (INSN_SEQNO (succ) > 0
          && INSN_SEQNO (succ) <= orig_max_seqno
          && INSN_SCHED_TIMES (succ) <= 0)
        {
          FENCE_INSN (fence) = succ;
          move_fence_to_fences (old_fences, new_fences);

          if (sched_verbose >= 1)
            sel_print ("Fence %d continues as %d[%d] (state continue)\n",
                       INSN_UID (insn), INSN_UID (succ), BLOCK_NUM (succ));
        }
      return;
    }

  /* Otherwise copy fence's structures to (possibly) multiple successors.  */
  FOR_EACH_SUCC_1 (succ, si, insn, SUCCS_NORMAL | SUCCS_SKIP_TO_LOOP_EXITS)
    {
      int seqno = INSN_SEQNO (succ);

      if (0 < seqno && seqno <= orig_max_seqno
          && (pipelining_p || INSN_SCHED_TIMES (succ) <= 0))
        {
          bool b = (in_same_ebb_p (insn, succ)
                    || in_fallthru_bb_p (insn, succ));

          if (sched_verbose >= 1)
            sel_print ("Fence %d continues as %d[%d] (state %s)\n",
                       INSN_UID (insn), INSN_UID (succ),
                       BLOCK_NUM (succ), b ? "continue" : "reset");

          if (b)
            add_dirty_fence_to_fences (new_fences, succ, fence);
          else
            {
              /* Mark block of the SUCC as head of the new ebb.  */
              bitmap_set_bit (forced_ebb_heads, BLOCK_NUM (succ));
              add_clean_fence_to_fences (new_fences, succ, fence);
            }
        }
    }
}


/* Functions to support substitution.  */

/* Returns whether INSN with dependence status DS is eligible for
   substitution, i.e. it's a copy operation x := y, and RHS that is
   moved up through this insn should be substituted.  */
static bool
can_substitute_through_p (insn_t insn, ds_t ds)
{
  /* We can substitute only true dependencies.  */
  if ((ds & DEP_OUTPUT)
      || (ds & DEP_ANTI)
      || ! INSN_RHS (insn)
      || ! INSN_LHS (insn))
    return false;

  /* Now we just need to make sure the INSN_RHS consists of only one
     simple REG rtx.  */
  if (REG_P (INSN_LHS (insn))
      && REG_P (INSN_RHS (insn)))
    return true;
  return false;
}

/* Substitute all occurrences of INSN's destination in EXPR' vinsn with INSN's
   source (if INSN is eligible for substitution).  Returns TRUE if
   substitution was actually performed, FALSE otherwise.  Substitution might
   be not performed because it's either EXPR' vinsn doesn't contain INSN's
   destination or the resulting insn is invalid for the target machine.
   When UNDO is true, perform unsubstitution instead (the difference is in
   the part of rtx on which validate_replace_rtx is called).  */
static bool
substitute_reg_in_expr (expr_t expr, insn_t insn, bool undo)
{
  rtx *where;
  bool new_insn_valid;
  vinsn_t *vi = &EXPR_VINSN (expr);
  bool has_rhs = VINSN_RHS (*vi) != NULL;
  rtx old, new_rtx;

  /* Do not try to replace in SET_DEST.  Although we'll choose new
     register for the RHS, we don't want to change RHS' original reg.
     If the insn is not SET, we may still be able to substitute something
     in it, and if we're here (don't have deps), it doesn't write INSN's
     dest.  */
  where = (has_rhs
	   ? &VINSN_RHS (*vi)
	   : &PATTERN (VINSN_INSN_RTX (*vi)));
  old = undo ? INSN_RHS (insn) : INSN_LHS (insn);

  /* Substitute if INSN has a form of x:=y and LHS(INSN) occurs in *VI.  */
  if (rtx_ok_for_substitution_p (old, *where))
    {
      rtx_insn *new_insn;
      rtx *where_replace;

      /* We should copy these rtxes before substitution.  */
      new_rtx = copy_rtx (undo ? INSN_LHS (insn) : INSN_RHS (insn));
      new_insn = create_copy_of_insn_rtx (VINSN_INSN_RTX (*vi));

      /* Where we'll replace.
         WHERE_REPLACE should point inside NEW_INSN, so INSN_RHS couldn't be
	 used instead of SET_SRC.  */
      where_replace = (has_rhs
		       ? &SET_SRC (PATTERN (new_insn))
		       : &PATTERN (new_insn));

      new_insn_valid
        = validate_replace_rtx_part_nosimplify (old, new_rtx, where_replace,
                                                new_insn);

      /* ??? Actually, constrain_operands result depends upon choice of
         destination register.  E.g. if we allow single register to be an rhs,
	 and if we try to move dx=ax(as rhs) through ax=dx, we'll result
	 in invalid insn dx=dx, so we'll loose this rhs here.
	 Just can't come up with significant testcase for this, so just
	 leaving it for now.  */
      if (new_insn_valid)
	{
	  change_vinsn_in_expr (expr,
				create_vinsn_from_insn_rtx (new_insn, false));

	  /* Do not allow clobbering the address register of speculative
             insns.  */
	  if ((EXPR_SPEC_DONE_DS (expr) & SPECULATIVE)
              && register_unavailable_p (VINSN_REG_USES (EXPR_VINSN (expr)),
					 expr_dest_reg (expr)))
	    EXPR_TARGET_AVAILABLE (expr) = false;

	  return true;
	}
      else
        return false;
    }
  else
    return false;
}

/* Return the number of places WHAT appears within WHERE.
   Bail out when we found a reference occupying several hard registers.  */
static int
count_occurrences_equiv (const_rtx what, const_rtx where)
{
  int count = 0;
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, where, NONCONST)
    {
      const_rtx x = *iter;
      if (REG_P (x) && REGNO (x) == REGNO (what))
	{
	  /* Bail out if mode is different or more than one register is
	     used.  */
	  if (GET_MODE (x) != GET_MODE (what) || REG_NREGS (x) > 1)
	    return 0;
	  count += 1;
	}
      else if (GET_CODE (x) == SUBREG
	       && (!REG_P (SUBREG_REG (x))
		   || REGNO (SUBREG_REG (x)) == REGNO (what)))
	/* ??? Do not support substituting regs inside subregs.  In that case,
	   simplify_subreg will be called by validate_replace_rtx, and
	   unsubstitution will fail later.  */
	return 0;
    }
  return count;
}

/* Returns TRUE if WHAT is found in WHERE rtx tree.  */
static bool
rtx_ok_for_substitution_p (rtx what, rtx where)
{
  return (count_occurrences_equiv (what, where) > 0);
}


/* Functions to support register renaming.  */

/* Substitute VI's set source with REGNO.  Returns newly created pattern
   that has REGNO as its source.  */
static rtx_insn *
create_insn_rtx_with_rhs (vinsn_t vi, rtx rhs_rtx)
{
  rtx lhs_rtx;
  rtx pattern;
  rtx_insn *insn_rtx;

  lhs_rtx = copy_rtx (VINSN_LHS (vi));

  pattern = gen_rtx_SET (lhs_rtx, rhs_rtx);
  insn_rtx = create_insn_rtx_from_pattern (pattern, NULL_RTX);

  return insn_rtx;
}

/* Returns whether INSN's src can be replaced with register number
   NEW_SRC_REG. E.g. the following insn is valid for i386:

    (insn:HI 2205 6585 2207 727 ../../gcc/libiberty/regex.c:3337
      (set (mem/s:QI (plus:SI (plus:SI (reg/f:SI 7 sp)
			(reg:SI 0 ax [orig:770 c1 ] [770]))
		    (const_int 288 [0x120])) [0 str S1 A8])
	    (const_int 0 [0x0])) 43 {*movqi_1} (nil)
	(nil))

  But if we change (const_int 0 [0x0]) to (reg:QI 4 si), it will be invalid
  because of operand constraints:

    (define_insn "*movqi_1"
      [(set (match_operand:QI 0 "nonimmediate_operand" "=q,q ,q ,r,r ,?r,m")
	    (match_operand:QI 1 "general_operand"      " q,qn,qm,q,rn,qm,qn")
	    )]

  So do constrain_operands here, before choosing NEW_SRC_REG as best
  reg for rhs.  */

static bool
replace_src_with_reg_ok_p (insn_t insn, rtx new_src_reg)
{
  vinsn_t vi = INSN_VINSN (insn);
  machine_mode mode;
  rtx dst_loc;
  bool res;

  gcc_assert (VINSN_SEPARABLE_P (vi));

  get_dest_and_mode (insn, &dst_loc, &mode);
  gcc_assert (mode == GET_MODE (new_src_reg));

  if (REG_P (dst_loc) && REGNO (new_src_reg) == REGNO (dst_loc))
    return true;

  /* See whether SET_SRC can be replaced with this register.  */
  validate_change (insn, &SET_SRC (PATTERN (insn)), new_src_reg, 1);
  res = verify_changes (0);
  cancel_changes (0);

  return res;
}

/* Returns whether INSN still be valid after replacing it's DEST with
   register NEW_REG.  */
static bool
replace_dest_with_reg_ok_p (insn_t insn, rtx new_reg)
{
  vinsn_t vi = INSN_VINSN (insn);
  bool res;

  /* We should deal here only with separable insns.  */
  gcc_assert (VINSN_SEPARABLE_P (vi));
  gcc_assert (GET_MODE (VINSN_LHS (vi)) == GET_MODE (new_reg));

  /* See whether SET_DEST can be replaced with this register.  */
  validate_change (insn, &SET_DEST (PATTERN (insn)), new_reg, 1);
  res = verify_changes (0);
  cancel_changes (0);

  return res;
}

/* Create a pattern with rhs of VI and lhs of LHS_RTX.  */
static rtx_insn *
create_insn_rtx_with_lhs (vinsn_t vi, rtx lhs_rtx)
{
  rtx rhs_rtx;
  rtx pattern;
  rtx_insn *insn_rtx;

  rhs_rtx = copy_rtx (VINSN_RHS (vi));

  pattern = gen_rtx_SET (lhs_rtx, rhs_rtx);
  insn_rtx = create_insn_rtx_from_pattern (pattern, NULL_RTX);

  return insn_rtx;
}

/* Substitute lhs in the given expression EXPR for the register with number
   NEW_REGNO.  SET_DEST may be arbitrary rtx, not only register.  */
static void
replace_dest_with_reg_in_expr (expr_t expr, rtx new_reg)
{
  rtx_insn *insn_rtx;
  vinsn_t vinsn;

  insn_rtx = create_insn_rtx_with_lhs (EXPR_VINSN (expr), new_reg);
  vinsn = create_vinsn_from_insn_rtx (insn_rtx, false);

  change_vinsn_in_expr (expr, vinsn);
  EXPR_WAS_RENAMED (expr) = 1;
  EXPR_TARGET_AVAILABLE (expr) = 1;
}

/* Returns whether VI writes either one of the USED_REGS registers or,
   if a register is a hard one, one of the UNAVAILABLE_HARD_REGS registers.  */
static bool
vinsn_writes_one_of_regs_p (vinsn_t vi, regset used_regs,
                            HARD_REG_SET unavailable_hard_regs)
{
  unsigned regno;
  reg_set_iterator rsi;

  EXECUTE_IF_SET_IN_REG_SET (VINSN_REG_SETS (vi), 0, regno, rsi)
    {
      if (REGNO_REG_SET_P (used_regs, regno))
        return true;
      if (HARD_REGISTER_NUM_P (regno)
          && TEST_HARD_REG_BIT (unavailable_hard_regs, regno))
	return true;
    }

  EXECUTE_IF_SET_IN_REG_SET (VINSN_REG_CLOBBERS (vi), 0, regno, rsi)
    {
      if (REGNO_REG_SET_P (used_regs, regno))
        return true;
      if (HARD_REGISTER_NUM_P (regno)
          && TEST_HARD_REG_BIT (unavailable_hard_regs, regno))
	return true;
    }

  return false;
}

/* Returns register class of the output register in INSN.
   Returns NO_REGS for call insns because some targets have constraints on
   destination register of a call insn.

   Code adopted from regrename.c::build_def_use.  */
static enum reg_class
get_reg_class (rtx_insn *insn)
{
  int i, n_ops;

  extract_constrain_insn (insn);
  preprocess_constraints (insn);
  n_ops = recog_data.n_operands;

  const operand_alternative *op_alt = which_op_alt ();
  if (asm_noperands (PATTERN (insn)) > 0)
    {
      for (i = 0; i < n_ops; i++)
	if (recog_data.operand_type[i] == OP_OUT)
	  {
	    rtx *loc = recog_data.operand_loc[i];
	    rtx op = *loc;
	    enum reg_class cl = alternative_class (op_alt, i);

	    if (REG_P (op)
		&& REGNO (op) == ORIGINAL_REGNO (op))
	      continue;

	    return cl;
	  }
    }
  else if (!CALL_P (insn))
    {
      for (i = 0; i < n_ops + recog_data.n_dups; i++)
       {
	 int opn = i < n_ops ? i : recog_data.dup_num[i - n_ops];
	 enum reg_class cl = alternative_class (op_alt, opn);

	 if (recog_data.operand_type[opn] == OP_OUT ||
	     recog_data.operand_type[opn] == OP_INOUT)
	   return cl;
       }
    }

/*  Insns like
    (insn (set (reg:CCZ 17 flags) (compare:CCZ ...)))
    may result in returning NO_REGS, cause flags is written implicitly through
    CMP insn, which has no OP_OUT | OP_INOUT operands.  */
  return NO_REGS;
}

/* Calculate HARD_REGNO_RENAME_OK data for REGNO.  */
static void
init_hard_regno_rename (int regno)
{
  int cur_reg;

  SET_HARD_REG_BIT (sel_hrd.regs_for_rename[regno], regno);

  for (cur_reg = 0; cur_reg < FIRST_PSEUDO_REGISTER; cur_reg++)
    {
      /* We are not interested in renaming in other regs.  */
      if (!TEST_HARD_REG_BIT (sel_hrd.regs_ever_used, cur_reg))
        continue;

      if (HARD_REGNO_RENAME_OK (regno, cur_reg))
        SET_HARD_REG_BIT (sel_hrd.regs_for_rename[regno], cur_reg);
    }
}

/* A wrapper around HARD_REGNO_RENAME_OK that will look into the hard regs
   data first.  */
static inline bool
sel_hard_regno_rename_ok (int from ATTRIBUTE_UNUSED, int to ATTRIBUTE_UNUSED)
{
  /* Check whether this is all calculated.  */
  if (TEST_HARD_REG_BIT (sel_hrd.regs_for_rename[from], from))
    return TEST_HARD_REG_BIT (sel_hrd.regs_for_rename[from], to);

  init_hard_regno_rename (from);

  return TEST_HARD_REG_BIT (sel_hrd.regs_for_rename[from], to);
}

/* Calculate set of registers that are capable of holding MODE.  */
static void
init_regs_for_mode (machine_mode mode)
{
  int cur_reg;

  CLEAR_HARD_REG_SET (sel_hrd.regs_for_mode[mode]);
  CLEAR_HARD_REG_SET (sel_hrd.regs_for_call_clobbered[mode]);

  for (cur_reg = 0; cur_reg < FIRST_PSEUDO_REGISTER; cur_reg++)
    {
      int nregs;
      int i;

      /* See whether it accepts all modes that occur in
         original insns.  */
      if (! HARD_REGNO_MODE_OK (cur_reg, mode))
        continue;

      nregs = hard_regno_nregs[cur_reg][mode];

      for (i = nregs - 1; i >= 0; --i)
        if (fixed_regs[cur_reg + i]
                || global_regs[cur_reg + i]
            /* Can't use regs which aren't saved by
               the prologue.  */
            || !TEST_HARD_REG_BIT (sel_hrd.regs_ever_used, cur_reg + i)
	    /* Can't use regs with non-null REG_BASE_VALUE, because adjusting
	       it affects aliasing globally and invalidates all AV sets.  */
	    || get_reg_base_value (cur_reg + i)
#ifdef LEAF_REGISTERS
            /* We can't use a non-leaf register if we're in a
               leaf function.  */
            || (crtl->is_leaf
                && !LEAF_REGISTERS[cur_reg + i])
#endif
            )
          break;

      if (i >= 0)
        continue;

      if (HARD_REGNO_CALL_PART_CLOBBERED (cur_reg, mode))
        SET_HARD_REG_BIT (sel_hrd.regs_for_call_clobbered[mode],
                          cur_reg);

      /* If the CUR_REG passed all the checks above,
         then it's ok.  */
      SET_HARD_REG_BIT (sel_hrd.regs_for_mode[mode], cur_reg);
    }

  sel_hrd.regs_for_mode_ok[mode] = true;
}

/* Init all register sets gathered in HRD.  */
static void
init_hard_regs_data (void)
{
  int cur_reg = 0;
  int cur_mode = 0;

  CLEAR_HARD_REG_SET (sel_hrd.regs_ever_used);
  for (cur_reg = 0; cur_reg < FIRST_PSEUDO_REGISTER; cur_reg++)
    if (df_regs_ever_live_p (cur_reg) || call_used_regs[cur_reg])
      SET_HARD_REG_BIT (sel_hrd.regs_ever_used, cur_reg);

  /* Initialize registers that are valid based on mode when this is
     really needed.  */
  for (cur_mode = 0; cur_mode < NUM_MACHINE_MODES; cur_mode++)
    sel_hrd.regs_for_mode_ok[cur_mode] = false;

  /* Mark that all HARD_REGNO_RENAME_OK is not calculated.  */
  for (cur_reg = 0; cur_reg < FIRST_PSEUDO_REGISTER; cur_reg++)
    CLEAR_HARD_REG_SET (sel_hrd.regs_for_rename[cur_reg]);

#ifdef STACK_REGS
  CLEAR_HARD_REG_SET (sel_hrd.stack_regs);

  for (cur_reg = FIRST_STACK_REG; cur_reg <= LAST_STACK_REG; cur_reg++)
    SET_HARD_REG_BIT (sel_hrd.stack_regs, cur_reg);
#endif
}

/* Mark hardware regs in REG_RENAME_P that are not suitable
   for renaming rhs in INSN due to hardware restrictions (register class,
   modes compatibility etc).  This doesn't affect original insn's dest reg,
   if it isn't in USED_REGS.  DEF is a definition insn of rhs for which the
   destination register is sought.  LHS (DEF->ORIG_INSN) may be REG or MEM.
   Registers that are in used_regs are always marked in
   unavailable_hard_regs as well.  */

static void
mark_unavailable_hard_regs (def_t def, struct reg_rename *reg_rename_p,
                            regset used_regs ATTRIBUTE_UNUSED)
{
  machine_mode mode;
  enum reg_class cl = NO_REGS;
  rtx orig_dest;
  unsigned cur_reg, regno;
  hard_reg_set_iterator hrsi;

  gcc_assert (GET_CODE (PATTERN (def->orig_insn)) == SET);
  gcc_assert (reg_rename_p);

  orig_dest = SET_DEST (PATTERN (def->orig_insn));

  /* We have decided not to rename 'mem = something;' insns, as 'something'
     is usually a register.  */
  if (!REG_P (orig_dest))
    return;

  regno = REGNO (orig_dest);

  /* If before reload, don't try to work with pseudos.  */
  if (!reload_completed && !HARD_REGISTER_NUM_P (regno))
    return;

  if (reload_completed)
    cl = get_reg_class (def->orig_insn);

  /* Stop if the original register is one of the fixed_regs, global_regs or
     frame pointer, or we could not discover its class.  */
  if (fixed_regs[regno]
      || global_regs[regno]
      || (!HARD_FRAME_POINTER_IS_FRAME_POINTER && frame_pointer_needed
	  && regno == HARD_FRAME_POINTER_REGNUM)
      || (HARD_FRAME_POINTER_REGNUM && frame_pointer_needed
	  && regno == FRAME_POINTER_REGNUM)
      || (reload_completed && cl == NO_REGS))
    {
      SET_HARD_REG_SET (reg_rename_p->unavailable_hard_regs);

      /* Give a chance for original register, if it isn't in used_regs.  */
      if (!def->crosses_call)
        CLEAR_HARD_REG_BIT (reg_rename_p->unavailable_hard_regs, regno);

      return;
    }

  /* If something allocated on stack in this function, mark frame pointer
     register unavailable, considering also modes.
     FIXME: it is enough to do this once per all original defs.  */
  if (frame_pointer_needed)
    {
      add_to_hard_reg_set (&reg_rename_p->unavailable_hard_regs,
			   Pmode, FRAME_POINTER_REGNUM);

      if (!HARD_FRAME_POINTER_IS_FRAME_POINTER)
        add_to_hard_reg_set (&reg_rename_p->unavailable_hard_regs, 
			     Pmode, HARD_FRAME_POINTER_REGNUM);
    }

#ifdef STACK_REGS
  /* For the stack registers the presence of FIRST_STACK_REG in USED_REGS
     is equivalent to as if all stack regs were in this set.
     I.e. no stack register can be renamed, and even if it's an original
     register here we make sure it won't be lifted over it's previous def
     (it's previous def will appear as if it's a FIRST_STACK_REG def.
     The HARD_REGNO_RENAME_OK covers other cases in condition below.  */
  if (IN_RANGE (REGNO (orig_dest), FIRST_STACK_REG, LAST_STACK_REG)
      && REGNO_REG_SET_P (used_regs, FIRST_STACK_REG))
    IOR_HARD_REG_SET (reg_rename_p->unavailable_hard_regs,
                      sel_hrd.stack_regs);
#endif

  /* If there's a call on this path, make regs from call_used_reg_set
     unavailable.  */
  if (def->crosses_call)
    IOR_HARD_REG_SET (reg_rename_p->unavailable_hard_regs,
                      call_used_reg_set);

  /* Stop here before reload: we need FRAME_REGS, STACK_REGS, and crosses_call,
     but not register classes.  */
  if (!reload_completed)
    return;

  /* Leave regs as 'available' only from the current
     register class.  */
  COPY_HARD_REG_SET (reg_rename_p->available_for_renaming,
                     reg_class_contents[cl]);

  mode = GET_MODE (orig_dest);

  /* Leave only registers available for this mode.  */
  if (!sel_hrd.regs_for_mode_ok[mode])
    init_regs_for_mode (mode);
  AND_HARD_REG_SET (reg_rename_p->available_for_renaming,
                    sel_hrd.regs_for_mode[mode]);

  /* Exclude registers that are partially call clobbered.  */
  if (def->crosses_call
      && ! HARD_REGNO_CALL_PART_CLOBBERED (regno, mode))
    AND_COMPL_HARD_REG_SET (reg_rename_p->available_for_renaming,
                            sel_hrd.regs_for_call_clobbered[mode]);

  /* Leave only those that are ok to rename.  */
  EXECUTE_IF_SET_IN_HARD_REG_SET (reg_rename_p->available_for_renaming,
                                  0, cur_reg, hrsi)
    {
      int nregs;
      int i;

      nregs = hard_regno_nregs[cur_reg][mode];
      gcc_assert (nregs > 0);

      for (i = nregs - 1; i >= 0; --i)
        if (! sel_hard_regno_rename_ok (regno + i, cur_reg + i))
          break;

      if (i >= 0)
        CLEAR_HARD_REG_BIT (reg_rename_p->available_for_renaming,
                            cur_reg);
    }

  AND_COMPL_HARD_REG_SET (reg_rename_p->available_for_renaming,
                          reg_rename_p->unavailable_hard_regs);

  /* Regno is always ok from the renaming part of view, but it really
     could be in *unavailable_hard_regs already, so set it here instead
     of there.  */
  SET_HARD_REG_BIT (reg_rename_p->available_for_renaming, regno);
}

/* reg_rename_tick[REG1] > reg_rename_tick[REG2] if REG1 was chosen as the
   best register more recently than REG2.  */
static int reg_rename_tick[FIRST_PSEUDO_REGISTER];

/* Indicates the number of times renaming happened before the current one.  */
static int reg_rename_this_tick;

/* Choose the register among free, that is suitable for storing
   the rhs value.

   ORIGINAL_INSNS is the list of insns where the operation (rhs)
   originally appears.  There could be multiple original operations
   for single rhs since we moving it up and merging along different
   paths.

   Some code is adapted from regrename.c (regrename_optimize).
   If original register is available, function returns it.
   Otherwise it performs the checks, so the new register should
   comply with the following:
    - it should not violate any live ranges (such registers are in
      REG_RENAME_P->available_for_renaming set);
    - it should not be in the HARD_REGS_USED regset;
    - it should be in the class compatible with original uses;
    - it should not be clobbered through reference with different mode;
    - if we're in the leaf function, then the new register should
      not be in the LEAF_REGISTERS;
    - etc.

   If several registers meet the conditions, the register with smallest
   tick is returned to achieve more even register allocation.

   If original register seems to be ok, we set *IS_ORIG_REG_P_PTR to true.

   If no register satisfies the above conditions, NULL_RTX is returned.  */
static rtx
choose_best_reg_1 (HARD_REG_SET hard_regs_used,
                   struct reg_rename *reg_rename_p,
                   def_list_t original_insns, bool *is_orig_reg_p_ptr)
{
  int best_new_reg;
  unsigned cur_reg;
  machine_mode mode = VOIDmode;
  unsigned regno, i, n;
  hard_reg_set_iterator hrsi;
  def_list_iterator di;
  def_t def;

  /* If original register is available, return it.  */
  *is_orig_reg_p_ptr = true;

  FOR_EACH_DEF (def, di, original_insns)
    {
      rtx orig_dest = SET_DEST (PATTERN (def->orig_insn));

      gcc_assert (REG_P (orig_dest));

      /* Check that all original operations have the same mode.
         This is done for the next loop; if we'd return from this
         loop, we'd check only part of them, but in this case
         it doesn't matter.  */
      if (mode == VOIDmode)
        mode = GET_MODE (orig_dest);
      gcc_assert (mode == GET_MODE (orig_dest));

      regno = REGNO (orig_dest);
      for (i = 0, n = hard_regno_nregs[regno][mode]; i < n; i++)
        if (TEST_HARD_REG_BIT (hard_regs_used, regno + i))
          break;

      /* All hard registers are available.  */
      if (i == n)
        {
          gcc_assert (mode != VOIDmode);

          /* Hard registers should not be shared.  */
          return gen_rtx_REG (mode, regno);
        }
    }

  *is_orig_reg_p_ptr = false;
  best_new_reg = -1;

  /* Among all available regs choose the register that was
     allocated earliest.  */
  EXECUTE_IF_SET_IN_HARD_REG_SET (reg_rename_p->available_for_renaming,
                                  0, cur_reg, hrsi)
    if (! TEST_HARD_REG_BIT (hard_regs_used, cur_reg))
      {
	/* Check that all hard regs for mode are available.  */
	for (i = 1, n = hard_regno_nregs[cur_reg][mode]; i < n; i++)
	  if (TEST_HARD_REG_BIT (hard_regs_used, cur_reg + i)
	      || !TEST_HARD_REG_BIT (reg_rename_p->available_for_renaming,
				     cur_reg + i))
	    break;

	if (i < n)
	  continue;

        /* All hard registers are available.  */
        if (best_new_reg < 0
            || reg_rename_tick[cur_reg] < reg_rename_tick[best_new_reg])
          {
            best_new_reg = cur_reg;

            /* Return immediately when we know there's no better reg.  */
            if (! reg_rename_tick[best_new_reg])
              break;
          }
      }

  if (best_new_reg >= 0)
    {
      /* Use the check from the above loop.  */
      gcc_assert (mode != VOIDmode);
      return gen_rtx_REG (mode, best_new_reg);
    }

  return NULL_RTX;
}

/* A wrapper around choose_best_reg_1 () to verify that we make correct
   assumptions about available registers in the function.  */
static rtx
choose_best_reg (HARD_REG_SET hard_regs_used, struct reg_rename *reg_rename_p,
                 def_list_t original_insns, bool *is_orig_reg_p_ptr)
{
  rtx best_reg = choose_best_reg_1 (hard_regs_used, reg_rename_p,
                                    original_insns, is_orig_reg_p_ptr);

  /* FIXME loop over hard_regno_nregs here.  */
  gcc_assert (best_reg == NULL_RTX
	      || TEST_HARD_REG_BIT (sel_hrd.regs_ever_used, REGNO (best_reg)));

  return best_reg;
}

/* Choose the pseudo register for storing rhs value.  As this is supposed
   to work before reload, we return either the original register or make
   the new one.  The parameters are the same that in choose_nest_reg_1
   functions, except that USED_REGS may contain pseudos.
   If we work with hard regs, check also REG_RENAME_P->UNAVAILABLE_HARD_REGS.

   TODO: take into account register pressure while doing this.  Up to this
   moment, this function would never return NULL for pseudos, but we should
   not rely on this.  */
static rtx
choose_best_pseudo_reg (regset used_regs,
                        struct reg_rename *reg_rename_p,
                        def_list_t original_insns, bool *is_orig_reg_p_ptr)
{
  def_list_iterator i;
  def_t def;
  machine_mode mode = VOIDmode;
  bool bad_hard_regs = false;

  /* We should not use this after reload.  */
  gcc_assert (!reload_completed);

  /* If original register is available, return it.  */
  *is_orig_reg_p_ptr = true;

  FOR_EACH_DEF (def, i, original_insns)
    {
      rtx dest = SET_DEST (PATTERN (def->orig_insn));
      int orig_regno;

      gcc_assert (REG_P (dest));

      /* Check that all original operations have the same mode.  */
      if (mode == VOIDmode)
        mode = GET_MODE (dest);
      else
        gcc_assert (mode == GET_MODE (dest));
      orig_regno = REGNO (dest);

      /* Check that nothing in used_regs intersects with orig_regno.  When
	 we have a hard reg here, still loop over hard_regno_nregs.  */
      if (HARD_REGISTER_NUM_P (orig_regno))
	{
	  int j, n;
	  for (j = 0, n = hard_regno_nregs[orig_regno][mode]; j < n; j++)
	    if (REGNO_REG_SET_P (used_regs, orig_regno + j))
	      break;
	  if (j < n)
	    continue;
	}
      else
	{
	  if (REGNO_REG_SET_P (used_regs, orig_regno))
	    continue;
	}
      if (HARD_REGISTER_NUM_P (orig_regno))
	{
	  gcc_assert (df_regs_ever_live_p (orig_regno));

	  /* For hard registers, we have to check hardware imposed
	     limitations (frame/stack registers, calls crossed).  */
	  if (!TEST_HARD_REG_BIT (reg_rename_p->unavailable_hard_regs,
				  orig_regno))
	    {
	      /* Don't let register cross a call if it doesn't already
		 cross one.  This condition is written in accordance with
		 that in sched-deps.c sched_analyze_reg().  */
	      if (!reg_rename_p->crosses_call
		  || REG_N_CALLS_CROSSED (orig_regno) > 0)
		return gen_rtx_REG (mode, orig_regno);
	    }

	  bad_hard_regs = true;
	}
      else
	return dest;
    }

  *is_orig_reg_p_ptr = false;

  /* We had some original hard registers that couldn't be used.
     Those were likely special.  Don't try to create a pseudo.  */
  if (bad_hard_regs)
    return NULL_RTX;

  /* We haven't found a register from original operations.  Get a new one.
     FIXME: control register pressure somehow.  */
  {
    rtx new_reg = gen_reg_rtx (mode);

    gcc_assert (mode != VOIDmode);

    max_regno = max_reg_num ();
    maybe_extend_reg_info_p ();
    REG_N_CALLS_CROSSED (REGNO (new_reg)) = reg_rename_p->crosses_call ? 1 : 0;

    return new_reg;
  }
}

/* True when target of EXPR is available due to EXPR_TARGET_AVAILABLE,
   USED_REGS and REG_RENAME_P->UNAVAILABLE_HARD_REGS.  */
static void
verify_target_availability (expr_t expr, regset used_regs,
			    struct reg_rename *reg_rename_p)
{
  unsigned n, i, regno;
  machine_mode mode;
  bool target_available, live_available, hard_available;

  if (!REG_P (EXPR_LHS (expr)) || EXPR_TARGET_AVAILABLE (expr) < 0)
    return;

  regno = expr_dest_regno (expr);
  mode = GET_MODE (EXPR_LHS (expr));
  target_available = EXPR_TARGET_AVAILABLE (expr) == 1;
  n = HARD_REGISTER_NUM_P (regno) ? hard_regno_nregs[regno][mode] : 1;

  live_available = hard_available = true;
  for (i = 0; i < n; i++)
    {
      if (bitmap_bit_p (used_regs, regno + i))
        live_available = false;
      if (TEST_HARD_REG_BIT (reg_rename_p->unavailable_hard_regs, regno + i))
        hard_available = false;
    }

  /* When target is not available, it may be due to hard register
     restrictions, e.g. crosses calls, so we check hard_available too.  */
  if (target_available)
    gcc_assert (live_available);
  else
    /* Check only if we haven't scheduled something on the previous fence,
       cause due to MAX_SOFTWARE_LOOKAHEAD_WINDOW_SIZE issues
       and having more than one fence, we may end having targ_un in a block
       in which successors target register is actually available.

       The last condition handles the case when a dependence from a call insn
       was created in sched-deps.c for insns with destination registers that
       never crossed a call before, but do cross one after our code motion.

       FIXME: in the latter case, we just uselessly called find_used_regs,
       because we can't move this expression with any other register
       as well.  */
    gcc_assert (scheduled_something_on_previous_fence || !live_available
		|| !hard_available
		|| (!reload_completed && reg_rename_p->crosses_call
		    && REG_N_CALLS_CROSSED (regno) == 0));
}

/* Collect unavailable registers due to liveness for EXPR from BNDS
   into USED_REGS.  Save additional information about available
   registers and unavailable due to hardware restriction registers
   into REG_RENAME_P structure.  Save original insns into ORIGINAL_INSNS
   list.  */
static void
collect_unavailable_regs_from_bnds (expr_t expr, blist_t bnds, regset used_regs,
				    struct reg_rename *reg_rename_p,
				    def_list_t *original_insns)
{
  for (; bnds; bnds = BLIST_NEXT (bnds))
    {
      bool res;
      av_set_t orig_ops = NULL;
      bnd_t bnd = BLIST_BND (bnds);

      /* If the chosen best expr doesn't belong to current boundary,
	 skip it.  */
      if (!av_set_is_in_p (BND_AV1 (bnd), EXPR_VINSN (expr)))
	continue;

      /* Put in ORIG_OPS all exprs from this boundary that became
	 RES on top.  */
      orig_ops = find_sequential_best_exprs (bnd, expr, false);

      /* Compute used regs and OR it into the USED_REGS.  */
      res = find_used_regs (BND_TO (bnd), orig_ops, used_regs,
			    reg_rename_p, original_insns);

      /* FIXME: the assert is true until we'd have several boundaries.  */
      gcc_assert (res);
      av_set_clear (&orig_ops);
    }
}

/* Return TRUE if it is possible to replace LHSes of ORIG_INSNS with BEST_REG.
   If BEST_REG is valid, replace LHS of EXPR with it.  */
static bool
try_replace_dest_reg (ilist_t orig_insns, rtx best_reg, expr_t expr)
{
  /* Try whether we'll be able to generate the insn
     'dest := best_reg' at the place of the original operation.  */
  for (; orig_insns; orig_insns = ILIST_NEXT (orig_insns))
    {
      insn_t orig_insn = DEF_LIST_DEF (orig_insns)->orig_insn;

      gcc_assert (EXPR_SEPARABLE_P (INSN_EXPR (orig_insn)));

      if (REGNO (best_reg) != REGNO (INSN_LHS (orig_insn))
	  && (! replace_src_with_reg_ok_p (orig_insn, best_reg)
	      || ! replace_dest_with_reg_ok_p (orig_insn, best_reg)))
	return false;
    }

  /* Make sure that EXPR has the right destination
     register.  */
  if (expr_dest_regno (expr) != REGNO (best_reg))
    replace_dest_with_reg_in_expr (expr, best_reg);
  else
    EXPR_TARGET_AVAILABLE (expr) = 1;

  return true;
}

/* Select and assign best register to EXPR searching from BNDS.
   Set *IS_ORIG_REG_P to TRUE if original register was selected.
   Return FALSE if no register can be chosen, which could happen when:
   * EXPR_SEPARABLE_P is true but we were unable to find suitable register;
   * EXPR_SEPARABLE_P is false but the insn sets/clobbers one of the registers
     that are used on the moving path.  */
static bool
find_best_reg_for_expr (expr_t expr, blist_t bnds, bool *is_orig_reg_p)
{
  static struct reg_rename reg_rename_data;

  regset used_regs;
  def_list_t original_insns = NULL;
  bool reg_ok;

  *is_orig_reg_p = false;

  /* Don't bother to do anything if this insn doesn't set any registers.  */
  if (bitmap_empty_p (VINSN_REG_SETS (EXPR_VINSN (expr)))
      && bitmap_empty_p (VINSN_REG_CLOBBERS (EXPR_VINSN (expr))))
    return true;

  used_regs = get_clear_regset_from_pool ();
  CLEAR_HARD_REG_SET (reg_rename_data.unavailable_hard_regs);

  collect_unavailable_regs_from_bnds (expr, bnds, used_regs, &reg_rename_data,
				      &original_insns);

  /* If after reload, make sure we're working with hard regs here.  */
  if (flag_checking && reload_completed)
    {
      reg_set_iterator rsi;
      unsigned i;

      EXECUTE_IF_SET_IN_REG_SET (used_regs, FIRST_PSEUDO_REGISTER, i, rsi)
        gcc_unreachable ();
    }

  if (EXPR_SEPARABLE_P (expr))
    {
      rtx best_reg = NULL_RTX;
      /* Check that we have computed availability of a target register
	 correctly.  */
      verify_target_availability (expr, used_regs, &reg_rename_data);

      /* Turn everything in hard regs after reload.  */
      if (reload_completed)
	{
	  HARD_REG_SET hard_regs_used;
	  REG_SET_TO_HARD_REG_SET (hard_regs_used, used_regs);

	  /* Join hard registers unavailable due to register class
	     restrictions and live range intersection.  */
	  IOR_HARD_REG_SET (hard_regs_used,
			    reg_rename_data.unavailable_hard_regs);

	  best_reg = choose_best_reg (hard_regs_used, &reg_rename_data,
				      original_insns, is_orig_reg_p);
	}
      else
	best_reg = choose_best_pseudo_reg (used_regs, &reg_rename_data,
					   original_insns, is_orig_reg_p);

      if (!best_reg)
	reg_ok = false;
      else if (*is_orig_reg_p)
	{
	  /* In case of unification BEST_REG may be different from EXPR's LHS
	     when EXPR's LHS is unavailable, and there is another LHS among
	     ORIGINAL_INSNS.  */
	  reg_ok = try_replace_dest_reg (original_insns, best_reg, expr);
	}
      else
	{
	  /* Forbid renaming of low-cost insns.  */
	  if (sel_vinsn_cost (EXPR_VINSN (expr)) < 2)
	    reg_ok = false;
	  else
	    reg_ok = try_replace_dest_reg (original_insns, best_reg, expr);
	}
    }
  else
    {
      /* If !EXPR_SCHEDULE_AS_RHS (EXPR), just make sure INSN doesn't set
	 any of the HARD_REGS_USED set.  */
      if (vinsn_writes_one_of_regs_p (EXPR_VINSN (expr), used_regs,
				      reg_rename_data.unavailable_hard_regs))
	{
	  reg_ok = false;
	  gcc_assert (EXPR_TARGET_AVAILABLE (expr) <= 0);
	}
      else
	{
	  reg_ok = true;
	  gcc_assert (EXPR_TARGET_AVAILABLE (expr) != 0);
	}
    }

  ilist_clear (&original_insns);
  return_regset_to_pool (used_regs);

  return reg_ok;
}


/* Return true if dependence described by DS can be overcomed.  */
static bool
can_speculate_dep_p (ds_t ds)
{
  if (spec_info == NULL)
    return false;

  /* Leave only speculative data.  */
  ds &= SPECULATIVE;

  if (ds == 0)
    return false;

  {
    /* FIXME: make sched-deps.c produce only those non-hard dependencies,
       that we can overcome.  */
    ds_t spec_mask = spec_info->mask;

    if ((ds & spec_mask) != ds)
      return false;
  }

  if (ds_weak (ds) < spec_info->data_weakness_cutoff)
    return false;

  return true;
}

/* Get a speculation check instruction.
   C_EXPR is a speculative expression,
   CHECK_DS describes speculations that should be checked,
   ORIG_INSN is the original non-speculative insn in the stream.  */
static insn_t
create_speculation_check (expr_t c_expr, ds_t check_ds, insn_t orig_insn)
{
  rtx check_pattern;
  rtx_insn *insn_rtx;
  insn_t insn;
  basic_block recovery_block;
  rtx_insn *label;

  /* Create a recovery block if target is going to emit branchy check, or if
     ORIG_INSN was speculative already.  */
  if (targetm.sched.needs_block_p (check_ds)
      || EXPR_SPEC_DONE_DS (INSN_EXPR (orig_insn)) != 0)
    {
      recovery_block = sel_create_recovery_block (orig_insn);
      label = BB_HEAD (recovery_block);
    }
  else
    {
      recovery_block = NULL;
      label = NULL;
    }

  /* Get pattern of the check.  */
  check_pattern = targetm.sched.gen_spec_check (EXPR_INSN_RTX (c_expr), label,
						check_ds);

  gcc_assert (check_pattern != NULL);

  /* Emit check.  */
  insn_rtx = create_insn_rtx_from_pattern (check_pattern, label);

  insn = sel_gen_insn_from_rtx_after (insn_rtx, INSN_EXPR (orig_insn),
				      INSN_SEQNO (orig_insn), orig_insn);

  /* Make check to be non-speculative.  */
  EXPR_SPEC_DONE_DS (INSN_EXPR (insn)) = 0;
  INSN_SPEC_CHECKED_DS (insn) = check_ds;

  /* Decrease priority of check by difference of load/check instruction
     latencies.  */
  EXPR_PRIORITY (INSN_EXPR (insn)) -= (sel_vinsn_cost (INSN_VINSN (orig_insn))
				       - sel_vinsn_cost (INSN_VINSN (insn)));

  /* Emit copy of original insn (though with replaced target register,
     if needed) to the recovery block.  */
  if (recovery_block != NULL)
    {
      rtx twin_rtx;

      twin_rtx = copy_rtx (PATTERN (EXPR_INSN_RTX (c_expr)));
      twin_rtx = create_insn_rtx_from_pattern (twin_rtx, NULL_RTX);
      sel_gen_recovery_insn_from_rtx_after (twin_rtx,
					    INSN_EXPR (orig_insn),
					    INSN_SEQNO (insn),
					    bb_note (recovery_block));
    }

  /* If we've generated a data speculation check, make sure
     that all the bookkeeping instruction we'll create during
     this move_op () will allocate an ALAT entry so that the
     check won't fail.
     In case of control speculation we must convert C_EXPR to control
     speculative mode, because failing to do so will bring us an exception
     thrown by the non-control-speculative load.  */
  check_ds = ds_get_max_dep_weak (check_ds);
  speculate_expr (c_expr, check_ds);

  return insn;
}

/* True when INSN is a "regN = regN" copy.  */
static bool
identical_copy_p (rtx_insn *insn)
{
  rtx lhs, rhs, pat;

  pat = PATTERN (insn);

  if (GET_CODE (pat) != SET)
    return false;

  lhs = SET_DEST (pat);
  if (!REG_P (lhs))
    return false;

  rhs = SET_SRC (pat);
  if (!REG_P (rhs))
    return false;

  return REGNO (lhs) == REGNO (rhs);
}

/* Undo all transformations on *AV_PTR that were done when
   moving through INSN.  */
static void
undo_transformations (av_set_t *av_ptr, rtx_insn *insn)
{
  av_set_iterator av_iter;
  expr_t expr;
  av_set_t new_set = NULL;

  /* First, kill any EXPR that uses registers set by an insn.  This is
     required for correctness.  */
  FOR_EACH_EXPR_1 (expr, av_iter, av_ptr)
    if (!sched_insns_conditions_mutex_p (insn, EXPR_INSN_RTX (expr))
        && bitmap_intersect_p (INSN_REG_SETS (insn),
                               VINSN_REG_USES (EXPR_VINSN (expr)))
        /* When an insn looks like 'r1 = r1', we could substitute through
           it, but the above condition will still hold.  This happened with
           gcc.c-torture/execute/961125-1.c.  */
        && !identical_copy_p (insn))
      {
        if (sched_verbose >= 6)
          sel_print ("Expr %d removed due to use/set conflict\n",
                     INSN_UID (EXPR_INSN_RTX (expr)));
        av_set_iter_remove (&av_iter);
      }

  /* Undo transformations looking at the history vector.  */
  FOR_EACH_EXPR (expr, av_iter, *av_ptr)
    {
      int index = find_in_history_vect (EXPR_HISTORY_OF_CHANGES (expr),
                                        insn, EXPR_VINSN (expr), true);

      if (index >= 0)
        {
          expr_history_def *phist;

          phist = &EXPR_HISTORY_OF_CHANGES (expr)[index];

          switch (phist->type)
            {
            case TRANS_SPECULATION:
              {
                ds_t old_ds, new_ds;

                /* Compute the difference between old and new speculative
                   statuses: that's what we need to check.
                   Earlier we used to assert that the status will really
                   change.  This no longer works because only the probability
                   bits in the status may have changed during compute_av_set,
                   and in the case of merging different probabilities of the
                   same speculative status along different paths we do not
                   record this in the history vector.  */
                old_ds = phist->spec_ds;
                new_ds = EXPR_SPEC_DONE_DS (expr);

                old_ds &= SPECULATIVE;
                new_ds &= SPECULATIVE;
                new_ds &= ~old_ds;

                EXPR_SPEC_TO_CHECK_DS (expr) |= new_ds;
                break;
              }
            case TRANS_SUBSTITUTION:
              {
                expr_def _tmp_expr, *tmp_expr = &_tmp_expr;
                vinsn_t new_vi;
                bool add = true;

                new_vi = phist->old_expr_vinsn;

                gcc_assert (VINSN_SEPARABLE_P (new_vi)
                            == EXPR_SEPARABLE_P (expr));
                copy_expr (tmp_expr, expr);

                if (vinsn_equal_p (phist->new_expr_vinsn,
                                   EXPR_VINSN (tmp_expr)))
                  change_vinsn_in_expr (tmp_expr, new_vi);
                else
                  /* This happens when we're unsubstituting on a bookkeeping
                     copy, which was in turn substituted.  The history is wrong
                     in this case.  Do it the hard way.  */
                  add = substitute_reg_in_expr (tmp_expr, insn, true);
                if (add)
                  av_set_add (&new_set, tmp_expr);
                clear_expr (tmp_expr);
                break;
              }
            default:
              gcc_unreachable ();
            }
        }

    }

  av_set_union_and_clear (av_ptr, &new_set, NULL);
}


/* Moveup_* helpers for code motion and computing av sets.  */

/* Propagates EXPR inside an insn group through THROUGH_INSN.
   The difference from the below function is that only substitution is
   performed.  */
static enum MOVEUP_EXPR_CODE
moveup_expr_inside_insn_group (expr_t expr, insn_t through_insn)
{
  vinsn_t vi = EXPR_VINSN (expr);
  ds_t *has_dep_p;
  ds_t full_ds;

  /* Do this only inside insn group.  */
  gcc_assert (INSN_SCHED_CYCLE (through_insn) > 0);

  full_ds = has_dependence_p (expr, through_insn, &has_dep_p);
  if (full_ds == 0)
    return MOVEUP_EXPR_SAME;

  /* Substitution is the possible choice in this case.  */
  if (has_dep_p[DEPS_IN_RHS])
    {
      /* Can't substitute UNIQUE VINSNs.  */
      gcc_assert (!VINSN_UNIQUE_P (vi));

      if (can_substitute_through_p (through_insn,
                                    has_dep_p[DEPS_IN_RHS])
          && substitute_reg_in_expr (expr, through_insn, false))
        {
          EXPR_WAS_SUBSTITUTED (expr) = true;
          return MOVEUP_EXPR_CHANGED;
        }

      /* Don't care about this, as even true dependencies may be allowed
         in an insn group.  */
      return MOVEUP_EXPR_SAME;
    }

  /* This can catch output dependencies in COND_EXECs.  */
  if (has_dep_p[DEPS_IN_INSN])
    return MOVEUP_EXPR_NULL;

  /* This is either an output or an anti dependence, which usually have
     a zero latency.  Allow this here, if we'd be wrong, tick_check_p
     will fix this.  */
  gcc_assert (has_dep_p[DEPS_IN_LHS]);
  return MOVEUP_EXPR_AS_RHS;
}

/* True when a trapping EXPR cannot be moved through THROUGH_INSN.  */
#define CANT_MOVE_TRAPPING(expr, through_insn)                \
  (VINSN_MAY_TRAP_P (EXPR_VINSN (expr))                       \
   && !sel_insn_has_single_succ_p ((through_insn), SUCCS_ALL) \
   && !sel_insn_is_speculation_check (through_insn))

/* True when a conflict on a target register was found during moveup_expr.  */
static bool was_target_conflict = false;

/* Return true when moving a debug INSN across THROUGH_INSN will
   create a bookkeeping block.  We don't want to create such blocks,
   for they would cause codegen differences between compilations with
   and without debug info.  */

static bool
moving_insn_creates_bookkeeping_block_p (insn_t insn,
					 insn_t through_insn)
{
  basic_block bbi, bbt;
  edge e1, e2;
  edge_iterator ei1, ei2;

  if (!bookkeeping_can_be_created_if_moved_through_p (through_insn))
    {
      if (sched_verbose >= 9)
	sel_print ("no bookkeeping required: ");
      return FALSE;
    }

  bbi = BLOCK_FOR_INSN (insn);

  if (EDGE_COUNT (bbi->preds) == 1)
    {
      if (sched_verbose >= 9)
	sel_print ("only one pred edge: ");
      return TRUE;
    }

  bbt = BLOCK_FOR_INSN (through_insn);

  FOR_EACH_EDGE (e1, ei1, bbt->succs)
    {
      FOR_EACH_EDGE (e2, ei2, bbi->preds)
	{
	  if (find_block_for_bookkeeping (e1, e2, TRUE))
	    {
	      if (sched_verbose >= 9)
		sel_print ("found existing block: ");
	      return FALSE;
	    }
	}
    }

  if (sched_verbose >= 9)
    sel_print ("would create bookkeeping block: ");

  return TRUE;
}

/* Return true when the conflict with newly created implicit clobbers
   between EXPR and THROUGH_INSN is found because of renaming.  */
static bool
implicit_clobber_conflict_p (insn_t through_insn, expr_t expr)
{
  HARD_REG_SET temp;
  rtx_insn *insn;
  rtx reg, rhs, pat;
  hard_reg_set_iterator hrsi;
  unsigned regno;
  bool valid;

  /* Make a new pseudo register.  */
  reg = gen_reg_rtx (GET_MODE (EXPR_LHS (expr)));
  max_regno = max_reg_num ();
  maybe_extend_reg_info_p ();

  /* Validate a change and bail out early.  */
  insn = EXPR_INSN_RTX (expr);
  validate_change (insn, &SET_DEST (PATTERN (insn)), reg, true);
  valid = verify_changes (0);
  cancel_changes (0);
  if (!valid)
    {
      if (sched_verbose >= 6)
	sel_print ("implicit clobbers failed validation, ");
      return true;
    }

  /* Make a new insn with it.  */
  rhs = copy_rtx (VINSN_RHS (EXPR_VINSN (expr)));
  pat = gen_rtx_SET (reg, rhs);
  start_sequence ();
  insn = emit_insn (pat);
  end_sequence ();

  /* Calculate implicit clobbers.  */
  extract_insn (insn);
  preprocess_constraints (insn);
  alternative_mask prefrred = get_preferred_alternatives (insn);
  ira_implicitly_set_insn_hard_regs (&temp, prefrred);
  AND_COMPL_HARD_REG_SET (temp, ira_no_alloc_regs);

  /* If any implicit clobber registers intersect with regular ones in
     through_insn, we have a dependency and thus bail out.  */
  EXECUTE_IF_SET_IN_HARD_REG_SET (temp, 0, regno, hrsi)
    {
      vinsn_t vi = INSN_VINSN (through_insn);
      if (bitmap_bit_p (VINSN_REG_SETS (vi), regno)
	  || bitmap_bit_p (VINSN_REG_CLOBBERS (vi), regno)
	  || bitmap_bit_p (VINSN_REG_USES (vi), regno))
	return true;
    }

  return false;
}

/* Modifies EXPR so it can be moved through the THROUGH_INSN,
   performing necessary transformations.  Record the type of transformation
   made in PTRANS_TYPE, when it is not NULL.  When INSIDE_INSN_GROUP,
   permit all dependencies except true ones, and try to remove those
   too via forward substitution.  All cases when a non-eliminable
   non-zero cost dependency exists inside an insn group will be fixed
   in tick_check_p instead.  */
static enum MOVEUP_EXPR_CODE
moveup_expr (expr_t expr, insn_t through_insn, bool inside_insn_group,
            enum local_trans_type *ptrans_type)
{
  vinsn_t vi = EXPR_VINSN (expr);
  insn_t insn = VINSN_INSN_RTX (vi);
  bool was_changed = false;
  bool as_rhs = false;
  ds_t *has_dep_p;
  ds_t full_ds;

  /* ??? We use dependencies of non-debug insns on debug insns to
     indicate that the debug insns need to be reset if the non-debug
     insn is pulled ahead of it.  It's hard to figure out how to
     introduce such a notion in sel-sched, but it already fails to
     support debug insns in other ways, so we just go ahead and
     let the deug insns go corrupt for now.  */
  if (DEBUG_INSN_P (through_insn) && !DEBUG_INSN_P (insn))
    return MOVEUP_EXPR_SAME;

  /* When inside_insn_group, delegate to the helper.  */
  if (inside_insn_group)
    return moveup_expr_inside_insn_group (expr, through_insn);

  /* Deal with unique insns and control dependencies.  */
  if (VINSN_UNIQUE_P (vi))
    {
      /* We can move jumps without side-effects or jumps that are
	 mutually exclusive with instruction THROUGH_INSN (all in cases
	 dependencies allow to do so and jump is not speculative).  */
      if (control_flow_insn_p (insn))
        {
          basic_block fallthru_bb;

          /* Do not move checks and do not move jumps through other
             jumps.  */
          if (control_flow_insn_p (through_insn)
              || sel_insn_is_speculation_check (insn))
            return MOVEUP_EXPR_NULL;

          /* Don't move jumps through CFG joins.  */
          if (bookkeeping_can_be_created_if_moved_through_p (through_insn))
            return MOVEUP_EXPR_NULL;

          /* The jump should have a clear fallthru block, and
             this block should be in the current region.  */
          if ((fallthru_bb = fallthru_bb_of_jump (insn)) == NULL
              || ! in_current_region_p (fallthru_bb))
            return MOVEUP_EXPR_NULL;

          /* And it should be mutually exclusive with through_insn.  */
          if (! sched_insns_conditions_mutex_p (insn, through_insn)
	      && ! DEBUG_INSN_P (through_insn))
            return MOVEUP_EXPR_NULL;
        }

      /* Don't move what we can't move.  */
      if (EXPR_CANT_MOVE (expr)
	  && BLOCK_FOR_INSN (through_insn) != BLOCK_FOR_INSN (insn))
	return MOVEUP_EXPR_NULL;

      /* Don't move SCHED_GROUP instruction through anything.
         If we don't force this, then it will be possible to start
         scheduling a sched_group before all its dependencies are
         resolved.
         ??? Haifa deals with this issue by delaying the SCHED_GROUP
         as late as possible through rank_for_schedule.  */
      if (SCHED_GROUP_P (insn))
	return MOVEUP_EXPR_NULL;
    }
  else
    gcc_assert (!control_flow_insn_p (insn));

  /* Don't move debug insns if this would require bookkeeping.  */
  if (DEBUG_INSN_P (insn)
      && BLOCK_FOR_INSN (through_insn) != BLOCK_FOR_INSN (insn)
      && moving_insn_creates_bookkeeping_block_p (insn, through_insn))
    return MOVEUP_EXPR_NULL;

  /* Deal with data dependencies.  */
  was_target_conflict = false;
  full_ds = has_dependence_p (expr, through_insn, &has_dep_p);
  if (full_ds == 0)
    {
      if (!CANT_MOVE_TRAPPING (expr, through_insn))
	return MOVEUP_EXPR_SAME;
    }
  else
    {
      /* We can move UNIQUE insn up only as a whole and unchanged,
         so it shouldn't have any dependencies.  */
      if (VINSN_UNIQUE_P (vi))
	return MOVEUP_EXPR_NULL;
    }

  if (full_ds != 0 && can_speculate_dep_p (full_ds))
    {
      int res;

      res = speculate_expr (expr, full_ds);
      if (res >= 0)
	{
          /* Speculation was successful.  */
          full_ds = 0;
          was_changed = (res > 0);
          if (res == 2)
            was_target_conflict = true;
          if (ptrans_type)
            *ptrans_type = TRANS_SPECULATION;
	  sel_clear_has_dependence ();
	}
    }

  if (has_dep_p[DEPS_IN_INSN])
    /* We have some dependency that cannot be discarded.  */
    return MOVEUP_EXPR_NULL;

  if (has_dep_p[DEPS_IN_LHS])
    {
      /* Only separable insns can be moved up with the new register.
         Anyways, we should mark that the original register is
         unavailable.  */
      if (!enable_schedule_as_rhs_p || !EXPR_SEPARABLE_P (expr))
        return MOVEUP_EXPR_NULL;

      /* When renaming a hard register to a pseudo before reload, extra
	 dependencies can occur from the implicit clobbers of the insn.
	 Filter out such cases here.  */
      if (!reload_completed && REG_P (EXPR_LHS (expr))
	  && HARD_REGISTER_P (EXPR_LHS (expr))
	  && implicit_clobber_conflict_p (through_insn, expr))
	{
	  if (sched_verbose >= 6)
	    sel_print ("implicit clobbers conflict detected, ");
	  return MOVEUP_EXPR_NULL;
	}
      EXPR_TARGET_AVAILABLE (expr) = false;
      was_target_conflict = true;
      as_rhs = true;
    }

  /* At this point we have either separable insns, that will be lifted
     up only as RHSes, or non-separable insns with no dependency in lhs.
     If dependency is in RHS, then try to perform substitution and move up
     substituted RHS:

      Ex. 1:				  Ex.2
	y = x;				    y = x;
	z = y*2;			    y = y*2;

    In Ex.1 y*2 can be substituted for x*2 and the whole operation can be
    moved above y=x assignment as z=x*2.

    In Ex.2 y*2 also can be substituted for x*2, but only the right hand
    side can be moved because of the output dependency.  The operation was
    cropped to its rhs above.  */
  if (has_dep_p[DEPS_IN_RHS])
    {
      ds_t *rhs_dsp = &has_dep_p[DEPS_IN_RHS];

      /* Can't substitute UNIQUE VINSNs.  */
      gcc_assert (!VINSN_UNIQUE_P (vi));

      if (can_speculate_dep_p (*rhs_dsp))
	{
          int res;

          res = speculate_expr (expr, *rhs_dsp);
          if (res >= 0)
            {
              /* Speculation was successful.  */
              *rhs_dsp = 0;
              was_changed = (res > 0);
              if (res == 2)
                was_target_conflict = true;
              if (ptrans_type)
                *ptrans_type = TRANS_SPECULATION;
            }
	  else
	    return MOVEUP_EXPR_NULL;
	}
      else if (can_substitute_through_p (through_insn,
                                         *rhs_dsp)
               && substitute_reg_in_expr (expr, through_insn, false))
	{
          /* ??? We cannot perform substitution AND speculation on the same
             insn.  */
          gcc_assert (!was_changed);
          was_changed = true;
          if (ptrans_type)
            *ptrans_type = TRANS_SUBSTITUTION;
          EXPR_WAS_SUBSTITUTED (expr) = true;
	}
      else
	return MOVEUP_EXPR_NULL;
    }

  /* Don't move trapping insns through jumps.
     This check should be at the end to give a chance to control speculation
     to perform its duties.  */
  if (CANT_MOVE_TRAPPING (expr, through_insn))
    return MOVEUP_EXPR_NULL;

  return (was_changed
          ? MOVEUP_EXPR_CHANGED
          : (as_rhs
             ? MOVEUP_EXPR_AS_RHS
             : MOVEUP_EXPR_SAME));
}

/* Try to look at bitmap caches for EXPR and INSN pair, return true
   if successful.  When INSIDE_INSN_GROUP, also try ignore dependencies
   that can exist within a parallel group.  Write to RES the resulting
   code for moveup_expr.  */
static bool
try_bitmap_cache (expr_t expr, insn_t insn,
                  bool inside_insn_group,
                  enum MOVEUP_EXPR_CODE *res)
{
  int expr_uid = INSN_UID (EXPR_INSN_RTX (expr));

  /* First check whether we've analyzed this situation already.  */
  if (bitmap_bit_p (INSN_ANALYZED_DEPS (insn), expr_uid))
    {
      if (bitmap_bit_p (INSN_FOUND_DEPS (insn), expr_uid))
        {
          if (sched_verbose >= 6)
            sel_print ("removed (cached)\n");
          *res = MOVEUP_EXPR_NULL;
          return true;
        }
      else
        {
          if (sched_verbose >= 6)
            sel_print ("unchanged (cached)\n");
          *res = MOVEUP_EXPR_SAME;
          return true;
        }
    }
  else if (bitmap_bit_p (INSN_FOUND_DEPS (insn), expr_uid))
    {
      if (inside_insn_group)
        {
          if (sched_verbose >= 6)
            sel_print ("unchanged (as RHS, cached, inside insn group)\n");
          *res = MOVEUP_EXPR_SAME;
          return true;

        }
      else
        EXPR_TARGET_AVAILABLE (expr) = false;

      /* This is the only case when propagation result can change over time,
         as we can dynamically switch off scheduling as RHS.  In this case,
         just check the flag to reach the correct decision.  */
      if (enable_schedule_as_rhs_p)
        {
          if (sched_verbose >= 6)
            sel_print ("unchanged (as RHS, cached)\n");
          *res = MOVEUP_EXPR_AS_RHS;
          return true;
        }
      else
        {
          if (sched_verbose >= 6)
            sel_print ("removed (cached as RHS, but renaming"
                       " is now disabled)\n");
          *res = MOVEUP_EXPR_NULL;
          return true;
        }
    }

  return false;
}

/* Try to look at bitmap caches for EXPR and INSN pair, return true
   if successful.  Write to RES the resulting code for moveup_expr.  */
static bool
try_transformation_cache (expr_t expr, insn_t insn,
                          enum MOVEUP_EXPR_CODE *res)
{
  struct transformed_insns *pti
    = (struct transformed_insns *)
    htab_find_with_hash (INSN_TRANSFORMED_INSNS (insn),
                         &EXPR_VINSN (expr),
                         VINSN_HASH_RTX (EXPR_VINSN (expr)));
  if (pti)
    {
      /* This EXPR was already moved through this insn and was
         changed as a result.  Fetch the proper data from
         the hashtable.  */
      insert_in_history_vect (&EXPR_HISTORY_OF_CHANGES (expr),
                              INSN_UID (insn), pti->type,
                              pti->vinsn_old, pti->vinsn_new,
                              EXPR_SPEC_DONE_DS (expr));

      if (INSN_IN_STREAM_P (VINSN_INSN_RTX (pti->vinsn_new)))
        pti->vinsn_new = vinsn_copy (pti->vinsn_new, true);
      change_vinsn_in_expr (expr, pti->vinsn_new);
      if (pti->was_target_conflict)
        EXPR_TARGET_AVAILABLE (expr) = false;
      if (pti->type == TRANS_SPECULATION)
        {
          EXPR_SPEC_DONE_DS (expr) = pti->ds;
          EXPR_NEEDS_SPEC_CHECK_P (expr) |= pti->needs_check;
        }

      if (sched_verbose >= 6)
        {
          sel_print ("changed (cached): ");
          dump_expr (expr);
          sel_print ("\n");
        }

      *res = MOVEUP_EXPR_CHANGED;
      return true;
    }

  return false;
}

/* Update bitmap caches on INSN with result RES of propagating EXPR.  */
static void
update_bitmap_cache (expr_t expr, insn_t insn, bool inside_insn_group,
                     enum MOVEUP_EXPR_CODE res)
{
  int expr_uid = INSN_UID (EXPR_INSN_RTX (expr));

  /* Do not cache result of propagating jumps through an insn group,
     as it is always true, which is not useful outside the group.  */
  if (inside_insn_group)
    return;

  if (res == MOVEUP_EXPR_NULL)
    {
      bitmap_set_bit (INSN_ANALYZED_DEPS (insn), expr_uid);
      bitmap_set_bit (INSN_FOUND_DEPS (insn), expr_uid);
    }
  else if (res == MOVEUP_EXPR_SAME)
    {
      bitmap_set_bit (INSN_ANALYZED_DEPS (insn), expr_uid);
      bitmap_clear_bit (INSN_FOUND_DEPS (insn), expr_uid);
    }
  else if (res == MOVEUP_EXPR_AS_RHS)
    {
      bitmap_clear_bit (INSN_ANALYZED_DEPS (insn), expr_uid);
      bitmap_set_bit (INSN_FOUND_DEPS (insn), expr_uid);
    }
  else
    gcc_unreachable ();
}

/* Update hashtable on INSN with changed EXPR, old EXPR_OLD_VINSN
   and transformation type TRANS_TYPE.  */
static void
update_transformation_cache (expr_t expr, insn_t insn,
                             bool inside_insn_group,
                             enum local_trans_type trans_type,
                             vinsn_t expr_old_vinsn)
{
  struct transformed_insns *pti;

  if (inside_insn_group)
    return;

  pti = XNEW (struct transformed_insns);
  pti->vinsn_old = expr_old_vinsn;
  pti->vinsn_new = EXPR_VINSN (expr);
  pti->type = trans_type;
  pti->was_target_conflict = was_target_conflict;
  pti->ds = EXPR_SPEC_DONE_DS (expr);
  pti->needs_check = EXPR_NEEDS_SPEC_CHECK_P (expr);
  vinsn_attach (pti->vinsn_old);
  vinsn_attach (pti->vinsn_new);
  *((struct transformed_insns **)
    htab_find_slot_with_hash (INSN_TRANSFORMED_INSNS (insn),
                              pti, VINSN_HASH_RTX (expr_old_vinsn),
                              INSERT)) = pti;
}

/* Same as moveup_expr, but first looks up the result of
   transformation in caches.  */
static enum MOVEUP_EXPR_CODE
moveup_expr_cached (expr_t expr, insn_t insn, bool inside_insn_group)
{
  enum MOVEUP_EXPR_CODE res;
  bool got_answer = false;

  if (sched_verbose >= 6)
    {
      sel_print ("Moving ");
      dump_expr (expr);
      sel_print (" through %d: ", INSN_UID (insn));
    }

  if (DEBUG_INSN_P (EXPR_INSN_RTX (expr))
      && BLOCK_FOR_INSN (EXPR_INSN_RTX (expr))
      && (sel_bb_head (BLOCK_FOR_INSN (EXPR_INSN_RTX (expr)))
	  == EXPR_INSN_RTX (expr)))
    /* Don't use cached information for debug insns that are heads of
       basic blocks.  */;
  else if (try_bitmap_cache (expr, insn, inside_insn_group, &res))
    /* When inside insn group, we do not want remove stores conflicting
       with previosly issued loads.  */
    got_answer = ! inside_insn_group || res != MOVEUP_EXPR_NULL;
  else if (try_transformation_cache (expr, insn, &res))
    got_answer = true;

  if (! got_answer)
    {
      /* Invoke moveup_expr and record the results.  */
      vinsn_t expr_old_vinsn = EXPR_VINSN (expr);
      ds_t expr_old_spec_ds = EXPR_SPEC_DONE_DS (expr);
      int expr_uid = INSN_UID (VINSN_INSN_RTX (expr_old_vinsn));
      bool unique_p = VINSN_UNIQUE_P (expr_old_vinsn);
      enum local_trans_type trans_type = TRANS_SUBSTITUTION;

      /* ??? Invent something better than this.  We can't allow old_vinsn
         to go, we need it for the history vector.  */
      vinsn_attach (expr_old_vinsn);

      res = moveup_expr (expr, insn, inside_insn_group,
                         &trans_type);
      switch (res)
        {
        case MOVEUP_EXPR_NULL:
          update_bitmap_cache (expr, insn, inside_insn_group, res);
	  if (sched_verbose >= 6)
            sel_print ("removed\n");
	  break;

	case MOVEUP_EXPR_SAME:
          update_bitmap_cache (expr, insn, inside_insn_group, res);
          if (sched_verbose >= 6)
            sel_print ("unchanged\n");
	  break;

        case MOVEUP_EXPR_AS_RHS:
          gcc_assert (!unique_p || inside_insn_group);
          update_bitmap_cache (expr, insn, inside_insn_group, res);
	  if (sched_verbose >= 6)
            sel_print ("unchanged (as RHS)\n");
	  break;

	case MOVEUP_EXPR_CHANGED:
          gcc_assert (INSN_UID (EXPR_INSN_RTX (expr)) != expr_uid
                      || EXPR_SPEC_DONE_DS (expr) != expr_old_spec_ds);
          insert_in_history_vect (&EXPR_HISTORY_OF_CHANGES (expr),
                                  INSN_UID (insn), trans_type,
                                  expr_old_vinsn, EXPR_VINSN (expr),
                                  expr_old_spec_ds);
          update_transformation_cache (expr, insn, inside_insn_group,
                                       trans_type, expr_old_vinsn);
          if (sched_verbose >= 6)
            {
              sel_print ("changed: ");
              dump_expr (expr);
              sel_print ("\n");
            }
	  break;
	default:
	  gcc_unreachable ();
        }

      vinsn_detach (expr_old_vinsn);
    }

  return res;
}

/* Moves an av set AVP up through INSN, performing necessary
   transformations.  */
static void
moveup_set_expr (av_set_t *avp, insn_t insn, bool inside_insn_group)
{
  av_set_iterator i;
  expr_t expr;

  FOR_EACH_EXPR_1 (expr, i, avp)
    {

      switch (moveup_expr_cached (expr, insn, inside_insn_group))
	{
	case MOVEUP_EXPR_SAME:
        case MOVEUP_EXPR_AS_RHS:
	  break;

	case MOVEUP_EXPR_NULL:
	  av_set_iter_remove (&i);
	  break;

	case MOVEUP_EXPR_CHANGED:
          expr = merge_with_other_exprs (avp, &i, expr);
	  break;

	default:
	  gcc_unreachable ();
	}
    }
}

/* Moves AVP set along PATH.  */
static void
moveup_set_inside_insn_group (av_set_t *avp, ilist_t path)
{
  int last_cycle;

  if (sched_verbose >= 6)
    sel_print ("Moving expressions up in the insn group...\n");
  if (! path)
    return;
  last_cycle = INSN_SCHED_CYCLE (ILIST_INSN (path));
  while (path
         && INSN_SCHED_CYCLE (ILIST_INSN (path)) == last_cycle)
    {
      moveup_set_expr (avp, ILIST_INSN (path), true);
      path = ILIST_NEXT (path);
    }
}

/* Returns true if after moving EXPR along PATH it equals to EXPR_VLIW.  */
static bool
equal_after_moveup_path_p (expr_t expr, ilist_t path, expr_t expr_vliw)
{
  expr_def _tmp, *tmp = &_tmp;
  int last_cycle;
  bool res = true;

  copy_expr_onside (tmp, expr);
  last_cycle = path ? INSN_SCHED_CYCLE (ILIST_INSN (path)) : 0;
  while (path
         && res
         && INSN_SCHED_CYCLE (ILIST_INSN (path)) == last_cycle)
    {
      res = (moveup_expr_cached (tmp, ILIST_INSN (path), true)
             != MOVEUP_EXPR_NULL);
      path = ILIST_NEXT (path);
    }

  if (res)
    {
      vinsn_t tmp_vinsn = EXPR_VINSN (tmp);
      vinsn_t expr_vliw_vinsn = EXPR_VINSN (expr_vliw);

      if (tmp_vinsn != expr_vliw_vinsn)
	res = vinsn_equal_p (tmp_vinsn, expr_vliw_vinsn);
    }

  clear_expr (tmp);
  return res;
}


/* Functions that compute av and lv sets.  */

/* Returns true if INSN is not a downward continuation of the given path P in
   the current stage.  */
static bool
is_ineligible_successor (insn_t insn, ilist_t p)
{
  insn_t prev_insn;

  /* Check if insn is not deleted.  */
  if (PREV_INSN (insn) && NEXT_INSN (PREV_INSN (insn)) != insn)
    gcc_unreachable ();
  else if (NEXT_INSN (insn) && PREV_INSN (NEXT_INSN (insn)) != insn)
    gcc_unreachable ();

  /* If it's the first insn visited, then the successor is ok.  */
  if (!p)
    return false;

  prev_insn = ILIST_INSN (p);

  if (/* a backward edge.  */
      INSN_SEQNO (insn) < INSN_SEQNO (prev_insn)
      /* is already visited.  */
      || (INSN_SEQNO (insn) == INSN_SEQNO (prev_insn)
	  && (ilist_is_in_p (p, insn)
              /* We can reach another fence here and still seqno of insn
                 would be equal to seqno of prev_insn.  This is possible
                 when prev_insn is a previously created bookkeeping copy.
                 In that case it'd get a seqno of insn.  Thus, check here
                 whether insn is in current fence too.  */
              || IN_CURRENT_FENCE_P (insn)))
      /* Was already scheduled on this round.  */
      || (INSN_SEQNO (insn) > INSN_SEQNO (prev_insn)
	  && IN_CURRENT_FENCE_P (insn))
      /* An insn from another fence could also be
	 scheduled earlier even if this insn is not in
	 a fence list right now.  Check INSN_SCHED_CYCLE instead.  */
      || (!pipelining_p
          && INSN_SCHED_TIMES (insn) > 0))
    return true;
  else
    return false;
}

/* Computes the av_set below the last bb insn INSN, doing all the 'dirty work'
   of handling multiple successors and properly merging its av_sets.  P is
   the current path traversed.  WS is the size of lookahead window.
   Return the av set computed.  */
static av_set_t
compute_av_set_at_bb_end (insn_t insn, ilist_t p, int ws)
{
  struct succs_info *sinfo;
  av_set_t expr_in_all_succ_branches = NULL;
  int is;
  insn_t succ, zero_succ = NULL;
  av_set_t av1 = NULL;

  gcc_assert (sel_bb_end_p (insn));

  /* Find different kind of successors needed for correct computing of
     SPEC and TARGET_AVAILABLE attributes.  */
  sinfo = compute_succs_info (insn, SUCCS_NORMAL);

  /* Debug output.  */
  if (sched_verbose >= 6)
    {
      sel_print ("successors of bb end (%d): ", INSN_UID (insn));
      dump_insn_vector (sinfo->succs_ok);
      sel_print ("\n");
      if (sinfo->succs_ok_n != sinfo->all_succs_n)
        sel_print ("real successors num: %d\n", sinfo->all_succs_n);
    }

  /* Add insn to the tail of current path.  */
  ilist_add (&p, insn);

  FOR_EACH_VEC_ELT (sinfo->succs_ok, is, succ)
    {
      av_set_t succ_set;

      /* We will edit SUCC_SET and EXPR_SPEC field of its elements.  */
      succ_set = compute_av_set_inside_bb (succ, p, ws, true);

      av_set_split_usefulness (succ_set,
                               sinfo->probs_ok[is],
                               sinfo->all_prob);

      if (sinfo->all_succs_n > 1)
	{
          /* Find EXPR'es that came from *all* successors and save them
             into expr_in_all_succ_branches.  This set will be used later
             for calculating speculation attributes of EXPR'es.  */
          if (is == 0)
            {
              expr_in_all_succ_branches = av_set_copy (succ_set);

              /* Remember the first successor for later. */
              zero_succ = succ;
            }
          else
            {
              av_set_iterator i;
              expr_t expr;

              FOR_EACH_EXPR_1 (expr, i, &expr_in_all_succ_branches)
                if (!av_set_is_in_p (succ_set, EXPR_VINSN (expr)))
                  av_set_iter_remove (&i);
            }
	}

      /* Union the av_sets.  Check liveness restrictions on target registers
         in special case of two successors.  */
      if (sinfo->succs_ok_n == 2 && is == 1)
        {
          basic_block bb0 = BLOCK_FOR_INSN (zero_succ);
          basic_block bb1 = BLOCK_FOR_INSN (succ);

          gcc_assert (BB_LV_SET_VALID_P (bb0) && BB_LV_SET_VALID_P (bb1));
          av_set_union_and_live (&av1, &succ_set,
                                 BB_LV_SET (bb0),
                                 BB_LV_SET (bb1),
                                 insn);
        }
      else
        av_set_union_and_clear (&av1, &succ_set, insn);
    }

  /* Check liveness restrictions via hard way when there are more than
     two successors.  */
  if (sinfo->succs_ok_n > 2)
    FOR_EACH_VEC_ELT (sinfo->succs_ok, is, succ)
      {
        basic_block succ_bb = BLOCK_FOR_INSN (succ);

        gcc_assert (BB_LV_SET_VALID_P (succ_bb));
        mark_unavailable_targets (av1, BB_AV_SET (succ_bb),
                                  BB_LV_SET (succ_bb));
      }

  /* Finally, check liveness restrictions on paths leaving the region.  */
  if (sinfo->all_succs_n > sinfo->succs_ok_n)
    FOR_EACH_VEC_ELT (sinfo->succs_other, is, succ)
      mark_unavailable_targets
        (av1, NULL, BB_LV_SET (BLOCK_FOR_INSN (succ)));

  if (sinfo->all_succs_n > 1)
    {
      av_set_iterator i;
      expr_t expr;

      /* Increase the spec attribute of all EXPR'es that didn't come
	 from all successors.  */
      FOR_EACH_EXPR (expr, i, av1)
	if (!av_set_is_in_p (expr_in_all_succ_branches, EXPR_VINSN (expr)))
	  EXPR_SPEC (expr)++;

      av_set_clear (&expr_in_all_succ_branches);

      /* Do not move conditional branches through other
	 conditional branches.  So, remove all conditional
	 branches from av_set if current operator is a conditional
	 branch.  */
      av_set_substract_cond_branches (&av1);
    }

  ilist_remove (&p);
  free_succs_info (sinfo);

  if (sched_verbose >= 6)
    {
      sel_print ("av_succs (%d): ", INSN_UID (insn));
      dump_av_set (av1);
      sel_print ("\n");
    }

  return av1;
}

/* This function computes av_set for the FIRST_INSN by dragging valid
   av_set through all basic block insns either from the end of basic block
   (computed using compute_av_set_at_bb_end) or from the insn on which
   MAX_WS was exceeded.  It uses compute_av_set_at_bb_end to compute av_set
   below the basic block and handling conditional branches.
   FIRST_INSN - the basic block head, P - path consisting of the insns
   traversed on the way to the FIRST_INSN (the path is sparse, only bb heads
   and bb ends are added to the path), WS - current window size,
   NEED_COPY_P - true if we'll make a copy of av_set before returning it.  */
static av_set_t
compute_av_set_inside_bb (insn_t first_insn, ilist_t p, int ws,
			  bool need_copy_p)
{
  insn_t cur_insn;
  int end_ws = ws;
  insn_t bb_end = sel_bb_end (BLOCK_FOR_INSN (first_insn));
  insn_t after_bb_end = NEXT_INSN (bb_end);
  insn_t last_insn;
  av_set_t av = NULL;
  basic_block cur_bb = BLOCK_FOR_INSN (first_insn);

  /* Return NULL if insn is not on the legitimate downward path.  */
  if (is_ineligible_successor (first_insn, p))
    {
      if (sched_verbose >= 6)
        sel_print ("Insn %d is ineligible_successor\n", INSN_UID (first_insn));

      return NULL;
    }

  /* If insn already has valid av(insn) computed, just return it.  */
  if (AV_SET_VALID_P (first_insn))
    {
      av_set_t av_set;

      if (sel_bb_head_p (first_insn))
	av_set = BB_AV_SET (BLOCK_FOR_INSN (first_insn));
      else
	av_set = NULL;

      if (sched_verbose >= 6)
        {
          sel_print ("Insn %d has a valid av set: ", INSN_UID (first_insn));
          dump_av_set (av_set);
          sel_print ("\n");
        }

      return need_copy_p ? av_set_copy (av_set) : av_set;
    }

  ilist_add (&p, first_insn);

  /* As the result after this loop have completed, in LAST_INSN we'll
     have the insn which has valid av_set to start backward computation
     from: it either will be NULL because on it the window size was exceeded
     or other valid av_set as returned by compute_av_set for the last insn
     of the basic block.  */
  for (last_insn = first_insn; last_insn != after_bb_end;
       last_insn = NEXT_INSN (last_insn))
    {
      /* We may encounter valid av_set not only on bb_head, but also on
	 those insns on which previously MAX_WS was exceeded.  */
      if (AV_SET_VALID_P (last_insn))
	{
          if (sched_verbose >= 6)
            sel_print ("Insn %d has a valid empty av set\n", INSN_UID (last_insn));
	  break;
	}

      /* The special case: the last insn of the BB may be an
         ineligible_successor due to its SEQ_NO that was set on
	 it as a bookkeeping.  */
      if (last_insn != first_insn
          && is_ineligible_successor (last_insn, p))
	{
          if (sched_verbose >= 6)
            sel_print ("Insn %d is ineligible_successor\n", INSN_UID (last_insn));
	  break;
	}

      if (DEBUG_INSN_P (last_insn))
	continue;

      if (end_ws > max_ws)
	{
	  /* We can reach max lookahead size at bb_header, so clean av_set
	     first.  */
	  INSN_WS_LEVEL (last_insn) = global_level;

	  if (sched_verbose >= 6)
            sel_print ("Insn %d is beyond the software lookahead window size\n",
                       INSN_UID (last_insn));
	  break;
	}

      end_ws++;
    }

  /* Get the valid av_set into AV above the LAST_INSN to start backward
     computation from.  It either will be empty av_set or av_set computed from
     the successors on the last insn of the current bb.  */
  if (last_insn != after_bb_end)
    {
      av = NULL;

      /* This is needed only to obtain av_sets that are identical to
         those computed by the old compute_av_set version.  */
      if (last_insn == first_insn && !INSN_NOP_P (last_insn))
        av_set_add (&av, INSN_EXPR (last_insn));
    }
  else
    /* END_WS is always already increased by 1 if LAST_INSN == AFTER_BB_END.  */
    av = compute_av_set_at_bb_end (bb_end, p, end_ws);

  /* Compute av_set in AV starting from below the LAST_INSN up to
     location above the FIRST_INSN.  */
  for (cur_insn = PREV_INSN (last_insn); cur_insn != PREV_INSN (first_insn);
       cur_insn = PREV_INSN (cur_insn))
    if (!INSN_NOP_P (cur_insn))
      {
        expr_t expr;

        moveup_set_expr (&av, cur_insn, false);

        /* If the expression for CUR_INSN is already in the set,
           replace it by the new one.  */
        expr = av_set_lookup (av, INSN_VINSN (cur_insn));
        if (expr != NULL)
          {
            clear_expr (expr);
            copy_expr (expr, INSN_EXPR (cur_insn));
          }
        else
          av_set_add (&av, INSN_EXPR (cur_insn));
      }

  /* Clear stale bb_av_set.  */
  if (sel_bb_head_p (first_insn))
    {
      av_set_clear (&BB_AV_SET (cur_bb));
      BB_AV_SET (cur_bb) = need_copy_p ? av_set_copy (av) : av;
      BB_AV_LEVEL (cur_bb) = global_level;
    }

  if (sched_verbose >= 6)
    {
      sel_print ("Computed av set for insn %d: ", INSN_UID (first_insn));
      dump_av_set (av);
      sel_print ("\n");
    }

  ilist_remove (&p);
  return av;
}

/* Compute av set before INSN.
   INSN - the current operation (actual rtx INSN)
   P - the current path, which is list of insns visited so far
   WS - software lookahead window size.
   UNIQUE_P - TRUE, if returned av_set will be changed, hence
   if we want to save computed av_set in s_i_d, we should make a copy of it.

   In the resulting set we will have only expressions that don't have delay
   stalls and nonsubstitutable dependences.  */
static av_set_t
compute_av_set (insn_t insn, ilist_t p, int ws, bool unique_p)
{
  return compute_av_set_inside_bb (insn, p, ws, unique_p);
}

/* Propagate a liveness set LV through INSN.  */
static void
propagate_lv_set (regset lv, insn_t insn)
{
  gcc_assert (INSN_P (insn));

  if (INSN_NOP_P (insn))
    return;

  df_simulate_one_insn_backwards (BLOCK_FOR_INSN (insn), insn, lv);
}

/* Return livness set at the end of BB.  */
static regset
compute_live_after_bb (basic_block bb)
{
  edge e;
  edge_iterator ei;
  regset lv = get_clear_regset_from_pool ();

  gcc_assert (!ignore_first);

  FOR_EACH_EDGE (e, ei, bb->succs)
    if (sel_bb_empty_p (e->dest))
      {
        if (! BB_LV_SET_VALID_P (e->dest))
          {
            gcc_unreachable ();
            gcc_assert (BB_LV_SET (e->dest) == NULL);
            BB_LV_SET (e->dest) = compute_live_after_bb (e->dest);
            BB_LV_SET_VALID_P (e->dest) = true;
          }
        IOR_REG_SET (lv, BB_LV_SET (e->dest));
      }
    else
      IOR_REG_SET (lv, compute_live (sel_bb_head (e->dest)));

  return lv;
}

/* Compute the set of all live registers at the point before INSN and save
   it at INSN if INSN is bb header.  */
regset
compute_live (insn_t insn)
{
  basic_block bb = BLOCK_FOR_INSN (insn);
  insn_t final, temp;
  regset lv;

  /* Return the valid set if we're already on it.  */
  if (!ignore_first)
    {
      regset src = NULL;

      if (sel_bb_head_p (insn) && BB_LV_SET_VALID_P (bb))
        src = BB_LV_SET (bb);
      else
        {
          gcc_assert (in_current_region_p (bb));
          if (INSN_LIVE_VALID_P (insn))
            src = INSN_LIVE (insn);
        }

      if (src)
	{
	  lv = get_regset_from_pool ();
	  COPY_REG_SET (lv, src);

          if (sel_bb_head_p (insn) && ! BB_LV_SET_VALID_P (bb))
            {
              COPY_REG_SET (BB_LV_SET (bb), lv);
              BB_LV_SET_VALID_P (bb) = true;
            }

	  return_regset_to_pool (lv);
	  return lv;
	}
    }

  /* We've skipped the wrong lv_set.  Don't skip the right one.  */
  ignore_first = false;
  gcc_assert (in_current_region_p (bb));

  /* Find a valid LV set in this block or below, if needed.
     Start searching from the next insn: either ignore_first is true, or
     INSN doesn't have a correct live set.  */
  temp = NEXT_INSN (insn);
  final = NEXT_INSN (BB_END (bb));
  while (temp != final && ! INSN_LIVE_VALID_P (temp))
    temp = NEXT_INSN (temp);
  if (temp == final)
    {
      lv = compute_live_after_bb (bb);
      temp = PREV_INSN (temp);
    }
  else
    {
      lv = get_regset_from_pool ();
      COPY_REG_SET (lv, INSN_LIVE (temp));
    }

  /* Put correct lv sets on the insns which have bad sets.  */
  final = PREV_INSN (insn);
  while (temp != final)
    {
      propagate_lv_set (lv, temp);
      COPY_REG_SET (INSN_LIVE (temp), lv);
      INSN_LIVE_VALID_P (temp) = true;
      temp = PREV_INSN (temp);
    }

  /* Also put it in a BB.  */
  if (sel_bb_head_p (insn))
    {
      basic_block bb = BLOCK_FOR_INSN (insn);

      COPY_REG_SET (BB_LV_SET (bb), lv);
      BB_LV_SET_VALID_P (bb) = true;
    }

  /* We return LV to the pool, but will not clear it there.  Thus we can
     legimatelly use LV till the next use of regset_pool_get ().  */
  return_regset_to_pool (lv);
  return lv;
}

/* Update liveness sets for INSN.  */
static inline void
update_liveness_on_insn (rtx_insn *insn)
{
  ignore_first = true;
  compute_live (insn);
}

/* Compute liveness below INSN and write it into REGS.  */
static inline void
compute_live_below_insn (rtx_insn *insn, regset regs)
{
  rtx_insn *succ;
  succ_iterator si;

  FOR_EACH_SUCC_1 (succ, si, insn, SUCCS_ALL)
    IOR_REG_SET (regs, compute_live (succ));
}

/* Update the data gathered in av and lv sets starting from INSN.  */
static void
update_data_sets (rtx_insn *insn)
{
  update_liveness_on_insn (insn);
  if (sel_bb_head_p (insn))
    {
      gcc_assert (AV_LEVEL (insn) != 0);
      BB_AV_LEVEL (BLOCK_FOR_INSN (insn)) = -1;
      compute_av_set (insn, NULL, 0, 0);
    }
}


/* Helper for move_op () and find_used_regs ().
   Return speculation type for which a check should be created on the place
   of INSN.  EXPR is one of the original ops we are searching for.  */
static ds_t
get_spec_check_type_for_insn (insn_t insn, expr_t expr)
{
  ds_t to_check_ds;
  ds_t already_checked_ds = EXPR_SPEC_DONE_DS (INSN_EXPR (insn));

  to_check_ds = EXPR_SPEC_TO_CHECK_DS (expr);

  if (targetm.sched.get_insn_checked_ds)
    already_checked_ds |= targetm.sched.get_insn_checked_ds (insn);

  if (spec_info != NULL
      && (spec_info->flags & SEL_SCHED_SPEC_DONT_CHECK_CONTROL))
    already_checked_ds |= BEGIN_CONTROL;

  already_checked_ds = ds_get_speculation_types (already_checked_ds);

  to_check_ds &= ~already_checked_ds;

  return to_check_ds;
}

/* Find the set of registers that are unavailable for storing expres
   while moving ORIG_OPS up on the path starting from INSN due to
   liveness (USED_REGS) or hardware restrictions (REG_RENAME_P).

   All the original operations found during the traversal are saved in the
   ORIGINAL_INSNS list.

   REG_RENAME_P denotes the set of hardware registers that
   can not be used with renaming due to the register class restrictions,
   mode restrictions and other (the register we'll choose should be
   compatible class with the original uses, shouldn't be in call_used_regs,
   should be HARD_REGNO_RENAME_OK etc).

   Returns TRUE if we've found all original insns, FALSE otherwise.

   This function utilizes code_motion_path_driver (formerly find_used_regs_1)
   to traverse the code motion paths.  This helper function finds registers
   that are not available for storing expres while moving ORIG_OPS up on the
   path starting from INSN.  A register considered as used on the moving path,
   if one of the following conditions is not satisfied:

      (1) a register not set or read on any path from xi to an instance of
	  the original operation,
      (2) not among the live registers of the point immediately following the
          first original operation on a given downward path, except for the
	  original target register of the operation,
      (3) not live on the other path of any conditional branch that is passed
	  by the operation, in case original operations are not present on
	  both paths of the conditional branch.

   All the original operations found during the traversal are saved in the
   ORIGINAL_INSNS list.

   REG_RENAME_P->CROSSES_CALL is true, if there is a call insn on the path
   from INSN to original insn. In this case CALL_USED_REG_SET will be added
   to unavailable hard regs at the point original operation is found.  */

static bool
find_used_regs (insn_t insn, av_set_t orig_ops, regset used_regs,
		struct reg_rename  *reg_rename_p, def_list_t *original_insns)
{
  def_list_iterator i;
  def_t def;
  int res;
  bool needs_spec_check_p = false;
  expr_t expr;
  av_set_iterator expr_iter;
  struct fur_static_params sparams;
  struct cmpd_local_params lparams;

  /* We haven't visited any blocks yet.  */
  bitmap_clear (code_motion_visited_blocks);

  /* Init parameters for code_motion_path_driver.  */
  sparams.crosses_call = false;
  sparams.original_insns = original_insns;
  sparams.used_regs = used_regs;

  /* Set the appropriate hooks and data.  */
  code_motion_path_driver_info = &fur_hooks;

  res = code_motion_path_driver (insn, orig_ops, NULL, &lparams, &sparams);

  reg_rename_p->crosses_call |= sparams.crosses_call;

  gcc_assert (res == 1);
  gcc_assert (original_insns && *original_insns);

  /* ??? We calculate whether an expression needs a check when computing
     av sets.  This information is not as precise as it could be due to
     merging this bit in merge_expr.  We can do better in find_used_regs,
     but we want to avoid multiple traversals of the same code motion
     paths.  */
  FOR_EACH_EXPR (expr, expr_iter, orig_ops)
    needs_spec_check_p |= EXPR_NEEDS_SPEC_CHECK_P (expr);

  /* Mark hardware regs in REG_RENAME_P that are not suitable
     for renaming expr in INSN due to hardware restrictions (register class,
     modes compatibility etc).  */
  FOR_EACH_DEF (def, i, *original_insns)
    {
      vinsn_t vinsn = INSN_VINSN (def->orig_insn);

      if (VINSN_SEPARABLE_P (vinsn))
	mark_unavailable_hard_regs (def, reg_rename_p, used_regs);

      /* Do not allow clobbering of ld.[sa] address in case some of the
         original operations need a check.  */
      if (needs_spec_check_p)
	IOR_REG_SET (used_regs, VINSN_REG_USES (vinsn));
    }

  return true;
}


/* Functions to choose the best insn from available ones.  */

/* Adjusts the priority for EXPR using the backend *_adjust_priority hook.  */
static int
sel_target_adjust_priority (expr_t expr)
{
  int priority = EXPR_PRIORITY (expr);
  int new_priority;

  if (targetm.sched.adjust_priority)
    new_priority = targetm.sched.adjust_priority (EXPR_INSN_RTX (expr), priority);
  else
    new_priority = priority;

  /* If the priority has changed, adjust EXPR_PRIORITY_ADJ accordingly.  */
  EXPR_PRIORITY_ADJ (expr) = new_priority - EXPR_PRIORITY (expr);

  gcc_assert (EXPR_PRIORITY_ADJ (expr) >= 0);

  if (sched_verbose >= 4)
    sel_print ("sel_target_adjust_priority: insn %d,  %d+%d = %d.\n",
	       INSN_UID (EXPR_INSN_RTX (expr)), EXPR_PRIORITY (expr),
	       EXPR_PRIORITY_ADJ (expr), new_priority);

  return new_priority;
}

/* Rank two available exprs for schedule.  Never return 0 here.  */
static int
sel_rank_for_schedule (const void *x, const void *y)
{
  expr_t tmp = *(const expr_t *) y;
  expr_t tmp2 = *(const expr_t *) x;
  insn_t tmp_insn, tmp2_insn;
  vinsn_t tmp_vinsn, tmp2_vinsn;
  int val;

  tmp_vinsn = EXPR_VINSN (tmp);
  tmp2_vinsn = EXPR_VINSN (tmp2);
  tmp_insn = EXPR_INSN_RTX (tmp);
  tmp2_insn = EXPR_INSN_RTX (tmp2);

  /* Schedule debug insns as early as possible.  */
  if (DEBUG_INSN_P (tmp_insn) && !DEBUG_INSN_P (tmp2_insn))
    return -1;
  else if (DEBUG_INSN_P (tmp2_insn))
    return 1;

  /* Prefer SCHED_GROUP_P insns to any others.  */
  if (SCHED_GROUP_P (tmp_insn) != SCHED_GROUP_P (tmp2_insn))
    {
      if (VINSN_UNIQUE_P (tmp_vinsn) && VINSN_UNIQUE_P (tmp2_vinsn))
        return SCHED_GROUP_P (tmp2_insn) ? 1 : -1;

      /* Now uniqueness means SCHED_GROUP_P is set, because schedule groups
         cannot be cloned.  */
      if (VINSN_UNIQUE_P (tmp2_vinsn))
        return 1;
      return -1;
    }

  /* Discourage scheduling of speculative checks.  */
  val = (sel_insn_is_speculation_check (tmp_insn)
	 - sel_insn_is_speculation_check (tmp2_insn));
  if (val)
    return val;

  /* Prefer not scheduled insn over scheduled one.  */
  if (EXPR_SCHED_TIMES (tmp) > 0 || EXPR_SCHED_TIMES (tmp2) > 0)
    {
      val = EXPR_SCHED_TIMES (tmp) - EXPR_SCHED_TIMES (tmp2);
      if (val)
	return val;
    }

  /* Prefer jump over non-jump instruction.  */
  if (control_flow_insn_p (tmp_insn) && !control_flow_insn_p (tmp2_insn))
    return -1;
  else if (control_flow_insn_p (tmp2_insn) && !control_flow_insn_p (tmp_insn))
    return 1;

  /* Prefer an expr with greater priority.  */
  if (EXPR_USEFULNESS (tmp) != 0 && EXPR_USEFULNESS (tmp2) != 0)
    {
      int p2 = EXPR_PRIORITY (tmp2) + EXPR_PRIORITY_ADJ (tmp2),
          p1 = EXPR_PRIORITY (tmp) + EXPR_PRIORITY_ADJ (tmp);

      val = p2 * EXPR_USEFULNESS (tmp2) - p1 * EXPR_USEFULNESS (tmp);
    }
  else
    val = EXPR_PRIORITY (tmp2) - EXPR_PRIORITY (tmp)
	  + EXPR_PRIORITY_ADJ (tmp2) - EXPR_PRIORITY_ADJ (tmp);
  if (val)
    return val;

  if (spec_info != NULL && spec_info->mask != 0)
    /* This code was taken from haifa-sched.c: rank_for_schedule ().  */
    {
      ds_t ds1, ds2;
      dw_t dw1, dw2;
      int dw;

      ds1 = EXPR_SPEC_DONE_DS (tmp);
      if (ds1)
	dw1 = ds_weak (ds1);
      else
	dw1 = NO_DEP_WEAK;

      ds2 = EXPR_SPEC_DONE_DS (tmp2);
      if (ds2)
	dw2 = ds_weak (ds2);
      else
	dw2 = NO_DEP_WEAK;

      dw = dw2 - dw1;
      if (dw > (NO_DEP_WEAK / 8) || dw < -(NO_DEP_WEAK / 8))
	return dw;
    }

  /* Prefer an old insn to a bookkeeping insn.  */
  if (INSN_UID (tmp_insn) < first_emitted_uid
      && INSN_UID (tmp2_insn) >= first_emitted_uid)
    return -1;
  if (INSN_UID (tmp_insn) >= first_emitted_uid
      && INSN_UID (tmp2_insn) < first_emitted_uid)
    return 1;

  /* Prefer an insn with smaller UID, as a last resort.
     We can't safely use INSN_LUID as it is defined only for those insns
     that are in the stream.  */
  return INSN_UID (tmp_insn) - INSN_UID (tmp2_insn);
}

/* Filter out expressions from av set pointed to by AV_PTR
   that are pipelined too many times.  */
static void
process_pipelined_exprs (av_set_t *av_ptr)
{
  expr_t expr;
  av_set_iterator si;

  /* Don't pipeline already pipelined code as that would increase
     number of unnecessary register moves.  */
  FOR_EACH_EXPR_1 (expr, si, av_ptr)
    {
      if (EXPR_SCHED_TIMES (expr)
	  >= PARAM_VALUE (PARAM_SELSCHED_MAX_SCHED_TIMES))
	av_set_iter_remove (&si);
    }
}

/* Filter speculative insns from AV_PTR if we don't want them.  */
static void
process_spec_exprs (av_set_t *av_ptr)
{
  expr_t expr;
  av_set_iterator si;

  if (spec_info == NULL)
    return;

  /* Scan *AV_PTR to find out if we want to consider speculative
     instructions for scheduling.  */
  FOR_EACH_EXPR_1 (expr, si, av_ptr)
    {
      ds_t ds;

      ds = EXPR_SPEC_DONE_DS (expr);

      /* The probability of a success is too low - don't speculate.  */
      if ((ds & SPECULATIVE)
          && (ds_weak (ds) < spec_info->data_weakness_cutoff
              || EXPR_USEFULNESS (expr) < spec_info->control_weakness_cutoff
	      || (pipelining_p && false
		  && (ds & DATA_SPEC)
		  && (ds & CONTROL_SPEC))))
        {
          av_set_iter_remove (&si);
          continue;
        }
    }
}

/* Search for any use-like insns in AV_PTR and decide on scheduling
   them.  Return one when found, and NULL otherwise.
   Note that we check here whether a USE could be scheduled to avoid
   an infinite loop later.  */
static expr_t
process_use_exprs (av_set_t *av_ptr)
{
  expr_t expr;
  av_set_iterator si;
  bool uses_present_p = false;
  bool try_uses_p = true;

  FOR_EACH_EXPR_1 (expr, si, av_ptr)
    {
      /* This will also initialize INSN_CODE for later use.  */
      if (recog_memoized (EXPR_INSN_RTX (expr)) < 0)
        {
          /* If we have a USE in *AV_PTR that was not scheduled yet,
             do so because it will do good only.  */
          if (EXPR_SCHED_TIMES (expr) <= 0)
            {
              if (EXPR_TARGET_AVAILABLE (expr) == 1)
                return expr;

              av_set_iter_remove (&si);
            }
          else
            {
              gcc_assert (pipelining_p);

              uses_present_p = true;
            }
        }
      else
        try_uses_p = false;
    }

  if (uses_present_p)
    {
      /* If we don't want to schedule any USEs right now and we have some
           in *AV_PTR, remove them, else just return the first one found.  */
      if (!try_uses_p)
        {
          FOR_EACH_EXPR_1 (expr, si, av_ptr)
            if (INSN_CODE (EXPR_INSN_RTX (expr)) < 0)
              av_set_iter_remove (&si);
        }
      else
        {
          FOR_EACH_EXPR_1 (expr, si, av_ptr)
            {
              gcc_assert (INSN_CODE (EXPR_INSN_RTX (expr)) < 0);

              if (EXPR_TARGET_AVAILABLE (expr) == 1)
                return expr;

              av_set_iter_remove (&si);
            }
        }
    }

  return NULL;
}

/* Lookup EXPR in VINSN_VEC and return TRUE if found.  Also check patterns from
   EXPR's history of changes.  */
static bool
vinsn_vec_has_expr_p (vinsn_vec_t vinsn_vec, expr_t expr)
{
  vinsn_t vinsn, expr_vinsn;
  int n;
  unsigned i;

  /* Start with checking expr itself and then proceed with all the old forms
     of expr taken from its history vector.  */
  for (i = 0, expr_vinsn = EXPR_VINSN (expr);
       expr_vinsn;
       expr_vinsn = (i < EXPR_HISTORY_OF_CHANGES (expr).length ()
		     ? EXPR_HISTORY_OF_CHANGES (expr)[i++].old_expr_vinsn
		     : NULL))
    FOR_EACH_VEC_ELT (vinsn_vec, n, vinsn)
      if (VINSN_SEPARABLE_P (vinsn))
	{
	  if (vinsn_equal_p (vinsn, expr_vinsn))
	    return true;
	}
      else
	{
	  /* For non-separable instructions, the blocking insn can have
	     another pattern due to substitution, and we can't choose
	     different register as in the above case.  Check all registers
	     being written instead.  */
	  if (bitmap_intersect_p (VINSN_REG_SETS (vinsn),
				  VINSN_REG_SETS (expr_vinsn)))
	    return true;
	}

  return false;
}

/* Return true if either of expressions from ORIG_OPS can be blocked
   by previously created bookkeeping code.  STATIC_PARAMS points to static
   parameters of move_op.  */
static bool
av_set_could_be_blocked_by_bookkeeping_p (av_set_t orig_ops, void *static_params)
{
  expr_t expr;
  av_set_iterator iter;
  moveop_static_params_p sparams;

  /* This checks that expressions in ORIG_OPS are not blocked by bookkeeping
     created while scheduling on another fence.  */
  FOR_EACH_EXPR (expr, iter, orig_ops)
    if (vinsn_vec_has_expr_p (vec_bookkeeping_blocked_vinsns, expr))
      return true;

  gcc_assert (code_motion_path_driver_info == &move_op_hooks);
  sparams = (moveop_static_params_p) static_params;

  /* Expressions can be also blocked by bookkeeping created during current
     move_op.  */
  if (bitmap_bit_p (current_copies, INSN_UID (sparams->failed_insn)))
    FOR_EACH_EXPR (expr, iter, orig_ops)
      if (moveup_expr_cached (expr, sparams->failed_insn, false) != MOVEUP_EXPR_NULL)
        return true;

  /* Expressions in ORIG_OPS may have wrong destination register due to
     renaming.  Check with the right register instead.  */
  if (sparams->dest && REG_P (sparams->dest))
    {
      rtx reg = sparams->dest;
      vinsn_t failed_vinsn = INSN_VINSN (sparams->failed_insn);

      if (register_unavailable_p (VINSN_REG_SETS (failed_vinsn), reg)
	  || register_unavailable_p (VINSN_REG_USES (failed_vinsn), reg)
	  || register_unavailable_p (VINSN_REG_CLOBBERS (failed_vinsn), reg))
	return true;
    }

  return false;
}

/* Clear VINSN_VEC and detach vinsns.  */
static void
vinsn_vec_clear (vinsn_vec_t *vinsn_vec)
{
  unsigned len = vinsn_vec->length ();
  if (len > 0)
    {
      vinsn_t vinsn;
      int n;

      FOR_EACH_VEC_ELT (*vinsn_vec, n, vinsn)
        vinsn_detach (vinsn);
      vinsn_vec->block_remove (0, len);
    }
}

/* Add the vinsn of EXPR to the VINSN_VEC.  */
static void
vinsn_vec_add (vinsn_vec_t *vinsn_vec, expr_t expr)
{
  vinsn_attach (EXPR_VINSN (expr));
  vinsn_vec->safe_push (EXPR_VINSN (expr));
}

/* Free the vector representing blocked expressions.  */
static void
vinsn_vec_free (vinsn_vec_t &vinsn_vec)
{
  vinsn_vec.release ();
}

/* Increase EXPR_PRIORITY_ADJ for INSN by AMOUNT.  */

void sel_add_to_insn_priority (rtx insn, int amount)
{
  EXPR_PRIORITY_ADJ (INSN_EXPR (insn)) += amount;

  if (sched_verbose >= 2)
    sel_print ("sel_add_to_insn_priority: insn %d, by %d (now %d+%d).\n",
	       INSN_UID (insn), amount, EXPR_PRIORITY (INSN_EXPR (insn)),
	       EXPR_PRIORITY_ADJ (INSN_EXPR (insn)));
}

/* Turn AV into a vector, filter inappropriate insns and sort it.  Return
   true if there is something to schedule.  BNDS and FENCE are current
   boundaries and fence, respectively.  If we need to stall for some cycles
   before an expr from AV would become available, write this number to
   *PNEED_STALL.  */
static bool
fill_vec_av_set (av_set_t av, blist_t bnds, fence_t fence,
                 int *pneed_stall)
{
  av_set_iterator si;
  expr_t expr;
  int sched_next_worked = 0, stalled, n;
  static int av_max_prio, est_ticks_till_branch;
  int min_need_stall = -1;
  deps_t dc = BND_DC (BLIST_BND (bnds));

  /* Bail out early when the ready list contained only USEs/CLOBBERs that are
     already scheduled.  */
  if (av == NULL)
    return false;

  /* Empty vector from the previous stuff.  */
  if (vec_av_set.length () > 0)
    vec_av_set.block_remove (0, vec_av_set.length ());

  /* Turn the set into a vector for sorting and call sel_target_adjust_priority
     for each insn.  */
  gcc_assert (vec_av_set.is_empty ());
  FOR_EACH_EXPR (expr, si, av)
    {
      vec_av_set.safe_push (expr);

      gcc_assert (EXPR_PRIORITY_ADJ (expr) == 0 || *pneed_stall);

      /* Adjust priority using target backend hook.  */
      sel_target_adjust_priority (expr);
    }

  /* Sort the vector.  */
  vec_av_set.qsort (sel_rank_for_schedule);

  /* We record maximal priority of insns in av set for current instruction
     group.  */
  if (FENCE_STARTS_CYCLE_P (fence))
    av_max_prio = est_ticks_till_branch = INT_MIN;

  /* Filter out inappropriate expressions.  Loop's direction is reversed to
     visit "best" instructions first.  We assume that vec::unordered_remove
     moves last element in place of one being deleted.  */
  for (n = vec_av_set.length () - 1, stalled = 0; n >= 0; n--)
    {
      expr_t expr = vec_av_set[n];
      insn_t insn = EXPR_INSN_RTX (expr);
      signed char target_available;
      bool is_orig_reg_p = true;
      int need_cycles, new_prio;
      bool fence_insn_p = INSN_UID (insn) == INSN_UID (FENCE_INSN (fence));

      /* Don't allow any insns other than from SCHED_GROUP if we have one.  */
      if (FENCE_SCHED_NEXT (fence) && insn != FENCE_SCHED_NEXT (fence))
        {
          vec_av_set.unordered_remove (n);
          continue;
        }

      /* Set number of sched_next insns (just in case there
         could be several).  */
      if (FENCE_SCHED_NEXT (fence))
        sched_next_worked++;

      /* Check all liveness requirements and try renaming.
         FIXME: try to minimize calls to this.  */
      target_available = EXPR_TARGET_AVAILABLE (expr);

      /* If insn was already scheduled on the current fence,
	 set TARGET_AVAILABLE to -1 no matter what expr's attribute says.  */
      if (vinsn_vec_has_expr_p (vec_target_unavailable_vinsns, expr)
	  && !fence_insn_p)
	target_available = -1;

      /* If the availability of the EXPR is invalidated by the insertion of
	 bookkeeping earlier, make sure that we won't choose this expr for
	 scheduling if it's not separable, and if it is separable, then
	 we have to recompute the set of available registers for it.  */
      if (vinsn_vec_has_expr_p (vec_bookkeeping_blocked_vinsns, expr))
	{
          vec_av_set.unordered_remove (n);
          if (sched_verbose >= 4)
            sel_print ("Expr %d is blocked by bookkeeping inserted earlier\n",
                       INSN_UID (insn));
          continue;
        }

      if (target_available == true)
	{
          /* Do nothing -- we can use an existing register.  */
	  is_orig_reg_p = EXPR_SEPARABLE_P (expr);
        }
      else if (/* Non-separable instruction will never
                  get another register. */
               (target_available == false
                && !EXPR_SEPARABLE_P (expr))
               /* Don't try to find a register for low-priority expression.  */
               || (int) vec_av_set.length () - 1 - n >= max_insns_to_rename
               /* ??? FIXME: Don't try to rename data speculation.  */
               || (EXPR_SPEC_DONE_DS (expr) & BEGIN_DATA)
               || ! find_best_reg_for_expr (expr, bnds, &is_orig_reg_p))
        {
          vec_av_set.unordered_remove (n);
          if (sched_verbose >= 4)
            sel_print ("Expr %d has no suitable target register\n",
                       INSN_UID (insn));

	  /* A fence insn should not get here.  */
	  gcc_assert (!fence_insn_p);
	  continue;
        }

      /* At this point a fence insn should always be available.  */
      gcc_assert (!fence_insn_p
		  || INSN_UID (FENCE_INSN (fence)) == INSN_UID (EXPR_INSN_RTX (expr)));

      /* Filter expressions that need to be renamed or speculated when
	 pipelining, because compensating register copies or speculation
	 checks are likely to be placed near the beginning of the loop,
	 causing a stall.  */
      if (pipelining_p && EXPR_ORIG_SCHED_CYCLE (expr) > 0
	  && (!is_orig_reg_p || EXPR_SPEC_DONE_DS (expr) != 0))
	{
	  /* Estimation of number of cycles until loop branch for
	     renaming/speculation to be successful.  */
	  int need_n_ticks_till_branch = sel_vinsn_cost (EXPR_VINSN (expr));

	  if ((int) current_loop_nest->ninsns < 9)
	    {
	      vec_av_set.unordered_remove (n);
	      if (sched_verbose >= 4)
		sel_print ("Pipelining expr %d will likely cause stall\n",
			   INSN_UID (insn));
	      continue;
	    }

	  if ((int) current_loop_nest->ninsns - num_insns_scheduled
	      < need_n_ticks_till_branch * issue_rate / 2
	      && est_ticks_till_branch < need_n_ticks_till_branch)
	     {
	       vec_av_set.unordered_remove (n);
	       if (sched_verbose >= 4)
		 sel_print ("Pipelining expr %d will likely cause stall\n",
			    INSN_UID (insn));
	       continue;
	     }
	}

      /* We want to schedule speculation checks as late as possible.  Discard
	 them from av set if there are instructions with higher priority.  */
      if (sel_insn_is_speculation_check (insn)
	  && EXPR_PRIORITY (expr) < av_max_prio)
	{
          stalled++;
          min_need_stall = min_need_stall < 0 ? 1 : MIN (min_need_stall, 1);
          vec_av_set.unordered_remove (n);
	  if (sched_verbose >= 4)
	    sel_print ("Delaying speculation check %d until its first use\n",
		       INSN_UID (insn));
	  continue;
	}

      /* Ignore EXPRs available from pipelining to update AV_MAX_PRIO.  */
      if (EXPR_ORIG_SCHED_CYCLE (expr) <= 0)
	av_max_prio = MAX (av_max_prio, EXPR_PRIORITY (expr));

      /* Don't allow any insns whose data is not yet ready.
         Check first whether we've already tried them and failed.  */
      if (INSN_UID (insn) < FENCE_READY_TICKS_SIZE (fence))
	{
          need_cycles = (FENCE_READY_TICKS (fence)[INSN_UID (insn)]
			 - FENCE_CYCLE (fence));
	  if (EXPR_ORIG_SCHED_CYCLE (expr) <= 0)
	    est_ticks_till_branch = MAX (est_ticks_till_branch,
					 EXPR_PRIORITY (expr) + need_cycles);

	  if (need_cycles > 0)
	    {
	      stalled++;
	      min_need_stall = (min_need_stall < 0
				? need_cycles
				: MIN (min_need_stall, need_cycles));
	      vec_av_set.unordered_remove (n);

	      if (sched_verbose >= 4)
		sel_print ("Expr %d is not ready until cycle %d (cached)\n",
			   INSN_UID (insn),
			   FENCE_READY_TICKS (fence)[INSN_UID (insn)]);
	      continue;
	    }
	}

      /* Now resort to dependence analysis to find whether EXPR might be
         stalled due to dependencies from FENCE's context.  */
      need_cycles = tick_check_p (expr, dc, fence);
      new_prio = EXPR_PRIORITY (expr) + EXPR_PRIORITY_ADJ (expr) + need_cycles;

      if (EXPR_ORIG_SCHED_CYCLE (expr) <= 0)
	est_ticks_till_branch = MAX (est_ticks_till_branch,
				     new_prio);

      if (need_cycles > 0)
        {
          if (INSN_UID (insn) >= FENCE_READY_TICKS_SIZE (fence))
            {
              int new_size = INSN_UID (insn) * 3 / 2;

              FENCE_READY_TICKS (fence)
                = (int *) xrecalloc (FENCE_READY_TICKS (fence),
                                     new_size, FENCE_READY_TICKS_SIZE (fence),
                                     sizeof (int));
            }
          FENCE_READY_TICKS (fence)[INSN_UID (insn)]
            = FENCE_CYCLE (fence) + need_cycles;

          stalled++;
          min_need_stall = (min_need_stall < 0
                            ? need_cycles
                            : MIN (min_need_stall, need_cycles));

          vec_av_set.unordered_remove (n);

          if (sched_verbose >= 4)
            sel_print ("Expr %d is not ready yet until cycle %d\n",
                       INSN_UID (insn),
                       FENCE_READY_TICKS (fence)[INSN_UID (insn)]);
          continue;
        }

      if (sched_verbose >= 4)
        sel_print ("Expr %d is ok\n", INSN_UID (insn));
      min_need_stall = 0;
    }

  /* Clear SCHED_NEXT.  */
  if (FENCE_SCHED_NEXT (fence))
    {
      gcc_assert (sched_next_worked == 1);
      FENCE_SCHED_NEXT (fence) = NULL;
    }

  /* No need to stall if this variable was not initialized.  */
  if (min_need_stall < 0)
    min_need_stall = 0;

  if (vec_av_set.is_empty ())
    {
      /* We need to set *pneed_stall here, because later we skip this code
         when ready list is empty.  */
      *pneed_stall = min_need_stall;
      return false;
    }
  else
    gcc_assert (min_need_stall == 0);

  /* Sort the vector.  */
  vec_av_set.qsort (sel_rank_for_schedule);

  if (sched_verbose >= 4)
    {
      sel_print ("Total ready exprs: %d, stalled: %d\n",
                 vec_av_set.length (), stalled);
      sel_print ("Sorted av set (%d): ", vec_av_set.length ());
      FOR_EACH_VEC_ELT (vec_av_set, n, expr)
        dump_expr (expr);
      sel_print ("\n");
    }

  *pneed_stall = 0;
  return true;
}

/* Convert a vectored and sorted av set to the ready list that
   the rest of the backend wants to see.  */
static void
convert_vec_av_set_to_ready (void)
{
  int n;
  expr_t expr;

  /* Allocate and fill the ready list from the sorted vector.  */
  ready.n_ready = vec_av_set.length ();
  ready.first = ready.n_ready - 1;

  gcc_assert (ready.n_ready > 0);

  if (ready.n_ready > max_issue_size)
    {
      max_issue_size = ready.n_ready;
      sched_extend_ready_list (ready.n_ready);
    }

  FOR_EACH_VEC_ELT (vec_av_set, n, expr)
    {
      vinsn_t vi = EXPR_VINSN (expr);
      insn_t insn = VINSN_INSN_RTX (vi);

      ready_try[n] = 0;
      ready.vec[n] = insn;
    }
}

/* Initialize ready list from *AV_PTR for the max_issue () call.
   If any unrecognizable insn found in *AV_PTR, return it (and skip
   max_issue).  BND and FENCE are current boundary and fence,
   respectively.  If we need to stall for some cycles before an expr
   from *AV_PTR would become available, write this number to *PNEED_STALL.  */
static expr_t
fill_ready_list (av_set_t *av_ptr, blist_t bnds, fence_t fence,
                 int *pneed_stall)
{
  expr_t expr;

  /* We do not support multiple boundaries per fence.  */
  gcc_assert (BLIST_NEXT (bnds) == NULL);

  /* Process expressions required special handling, i.e.  pipelined,
     speculative and recog() < 0 expressions first.  */
  process_pipelined_exprs (av_ptr);
  process_spec_exprs (av_ptr);

  /* A USE could be scheduled immediately.  */
  expr = process_use_exprs (av_ptr);
  if (expr)
    {
      *pneed_stall = 0;
      return expr;
    }

  /* Turn the av set to a vector for sorting.  */
  if (! fill_vec_av_set (*av_ptr, bnds, fence, pneed_stall))
    {
      ready.n_ready = 0;
      return NULL;
    }

  /* Build the final ready list.  */
  convert_vec_av_set_to_ready ();
  return NULL;
}

/* Wrapper for dfa_new_cycle ().  Returns TRUE if cycle was advanced.  */
static bool
sel_dfa_new_cycle (insn_t insn, fence_t fence)
{
  int last_scheduled_cycle = FENCE_LAST_SCHEDULED_INSN (fence)
                             ? INSN_SCHED_CYCLE (FENCE_LAST_SCHEDULED_INSN (fence))
                             : FENCE_CYCLE (fence) - 1;
  bool res = false;
  int sort_p = 0;

  if (!targetm.sched.dfa_new_cycle)
    return false;

  memcpy (curr_state, FENCE_STATE (fence), dfa_state_size);

  while (!sort_p && targetm.sched.dfa_new_cycle (sched_dump, sched_verbose,
                                                 insn, last_scheduled_cycle,
                                                 FENCE_CYCLE (fence), &sort_p))
    {
      memcpy (FENCE_STATE (fence), curr_state, dfa_state_size);
      advance_one_cycle (fence);
      memcpy (curr_state, FENCE_STATE (fence), dfa_state_size);
      res = true;
    }

  return res;
}

/* Invoke reorder* target hooks on the ready list.  Return the number of insns
   we can issue.  FENCE is the current fence.  */
static int
invoke_reorder_hooks (fence_t fence)
{
  int issue_more;
  bool ran_hook = false;

  /* Call the reorder hook at the beginning of the cycle, and call
     the reorder2 hook in the middle of the cycle.  */
  if (FENCE_ISSUED_INSNS (fence) == 0)
    {
      if (targetm.sched.reorder
          && !SCHED_GROUP_P (ready_element (&ready, 0))
          && ready.n_ready > 1)
        {
          /* Don't give reorder the most prioritized insn as it can break
             pipelining.  */
          if (pipelining_p)
            --ready.n_ready;

          issue_more
            = targetm.sched.reorder (sched_dump, sched_verbose,
                                     ready_lastpos (&ready),
                                     &ready.n_ready, FENCE_CYCLE (fence));

          if (pipelining_p)
            ++ready.n_ready;

          ran_hook = true;
        }
      else
        /* Initialize can_issue_more for variable_issue.  */
        issue_more = issue_rate;
    }
  else if (targetm.sched.reorder2
           && !SCHED_GROUP_P (ready_element (&ready, 0)))
    {
      if (ready.n_ready == 1)
        issue_more =
          targetm.sched.reorder2 (sched_dump, sched_verbose,
                                  ready_lastpos (&ready),
                                  &ready.n_ready, FENCE_CYCLE (fence));
      else
        {
          if (pipelining_p)
            --ready.n_ready;

          issue_more =
            targetm.sched.reorder2 (sched_dump, sched_verbose,
                                    ready.n_ready
                                    ? ready_lastpos (&ready) : NULL,
                                    &ready.n_ready, FENCE_CYCLE (fence));

          if (pipelining_p)
            ++ready.n_ready;
        }

      ran_hook = true;
    }
  else
    issue_more = FENCE_ISSUE_MORE (fence);

  /* Ensure that ready list and vec_av_set are in line with each other,
     i.e. vec_av_set[i] == ready_element (&ready, i).  */
  if (issue_more && ran_hook)
    {
      int i, j, n;
      rtx_insn **arr = ready.vec;
      expr_t *vec = vec_av_set.address ();

      for (i = 0, n = ready.n_ready; i < n; i++)
        if (EXPR_INSN_RTX (vec[i]) != arr[i])
          {
            for (j = i; j < n; j++)
              if (EXPR_INSN_RTX (vec[j]) == arr[i])
                break;
            gcc_assert (j < n);

	    std::swap (vec[i], vec[j]);
          }
    }

  return issue_more;
}

/* Return an EXPR corresponding to INDEX element of ready list, if
   FOLLOW_READY_ELEMENT is true (i.e., an expr of
   ready_element (&ready, INDEX) will be returned), and to INDEX element of
   ready.vec otherwise.  */
static inline expr_t
find_expr_for_ready (int index, bool follow_ready_element)
{
  expr_t expr;
  int real_index;

  real_index = follow_ready_element ? ready.first - index : index;

  expr = vec_av_set[real_index];
  gcc_assert (ready.vec[real_index] == EXPR_INSN_RTX (expr));

  return expr;
}

/* Calculate insns worth trying via lookahead_guard hook.  Return a number
   of such insns found.  */
static int
invoke_dfa_lookahead_guard (void)
{
  int i, n;
  bool have_hook
    = targetm.sched.first_cycle_multipass_dfa_lookahead_guard != NULL;

  if (sched_verbose >= 2)
    sel_print ("ready after reorder: ");

  for (i = 0, n = 0; i < ready.n_ready; i++)
    {
      expr_t expr;
      insn_t insn;
      int r;

      /* In this loop insn is Ith element of the ready list given by
         ready_element, not Ith element of ready.vec.  */
      insn = ready_element (&ready, i);

      if (! have_hook || i == 0)
        r = 0;
      else
        r = targetm.sched.first_cycle_multipass_dfa_lookahead_guard (insn, i);

      gcc_assert (INSN_CODE (insn) >= 0);

      /* Only insns with ready_try = 0 can get here
         from fill_ready_list.  */
      gcc_assert (ready_try [i] == 0);
      ready_try[i] = r;
      if (!r)
        n++;

      expr = find_expr_for_ready (i, true);

      if (sched_verbose >= 2)
        {
          dump_vinsn (EXPR_VINSN (expr));
          sel_print (":%d; ", ready_try[i]);
        }
    }

  if (sched_verbose >= 2)
    sel_print ("\n");
  return n;
}

/* Calculate the number of privileged insns and return it.  */
static int
calculate_privileged_insns (void)
{
  expr_t cur_expr, min_spec_expr = NULL;
  int privileged_n = 0, i;

  for (i = 0; i < ready.n_ready; i++)
    {
      if (ready_try[i])
        continue;

      if (! min_spec_expr)
	min_spec_expr = find_expr_for_ready (i, true);

      cur_expr = find_expr_for_ready (i, true);

      if (EXPR_SPEC (cur_expr) > EXPR_SPEC (min_spec_expr))
        break;

      ++privileged_n;
    }

  if (i == ready.n_ready)
    privileged_n = 0;

  if (sched_verbose >= 2)
    sel_print ("privileged_n: %d insns with SPEC %d\n",
               privileged_n, privileged_n ? EXPR_SPEC (min_spec_expr) : -1);
  return privileged_n;
}

/* Call the rest of the hooks after the choice was made.  Return
   the number of insns that still can be issued given that the current
   number is ISSUE_MORE.  FENCE and BEST_INSN are the current fence
   and the insn chosen for scheduling, respectively.  */
static int
invoke_aftermath_hooks (fence_t fence, rtx_insn *best_insn, int issue_more)
{
  gcc_assert (INSN_P (best_insn));

  /* First, call dfa_new_cycle, and then variable_issue, if available.  */
  sel_dfa_new_cycle (best_insn, fence);

  if (targetm.sched.variable_issue)
    {
      memcpy (curr_state, FENCE_STATE (fence), dfa_state_size);
      issue_more =
        targetm.sched.variable_issue (sched_dump, sched_verbose, best_insn,
                                      issue_more);
      memcpy (FENCE_STATE (fence), curr_state, dfa_state_size);
    }
  else if (!DEBUG_INSN_P (best_insn)
	   && GET_CODE (PATTERN (best_insn)) != USE
	   && GET_CODE (PATTERN (best_insn)) != CLOBBER)
    issue_more--;

  return issue_more;
}

/* Estimate the cost of issuing INSN on DFA state STATE.  */
static int
estimate_insn_cost (rtx_insn *insn, state_t state)
{
  static state_t temp = NULL;
  int cost;

  if (!temp)
    temp = xmalloc (dfa_state_size);

  memcpy (temp, state, dfa_state_size);
  cost = state_transition (temp, insn);

  if (cost < 0)
    return 0;
  else if (cost == 0)
    return 1;
  return cost;
}

/* Return the cost of issuing EXPR on the FENCE as estimated by DFA.
   This function properly handles ASMs, USEs etc.  */
static int
get_expr_cost (expr_t expr, fence_t fence)
{
  rtx_insn *insn = EXPR_INSN_RTX (expr);

  if (recog_memoized (insn) < 0)
    {
      if (!FENCE_STARTS_CYCLE_P (fence)
	  && INSN_ASM_P (insn))
	/* This is asm insn which is tryed to be issued on the
	   cycle not first.  Issue it on the next cycle.  */
	return 1;
      else
	/* A USE insn, or something else we don't need to
	   understand.  We can't pass these directly to
	   state_transition because it will trigger a
	   fatal error for unrecognizable insns.  */
	return 0;
    }
  else
    return estimate_insn_cost (insn, FENCE_STATE (fence));
}

/* Find the best insn for scheduling, either via max_issue or just take
   the most prioritized available.  */
static int
choose_best_insn (fence_t fence, int privileged_n, int *index)
{
  int can_issue = 0;

  if (dfa_lookahead > 0)
    {
      cycle_issued_insns = FENCE_ISSUED_INSNS (fence);
      /* TODO: pass equivalent of first_cycle_insn_p to max_issue ().  */
      can_issue = max_issue (&ready, privileged_n,
                             FENCE_STATE (fence), true, index);
      if (sched_verbose >= 2)
        sel_print ("max_issue: we can issue %d insns, already did %d insns\n",
                   can_issue, FENCE_ISSUED_INSNS (fence));
    }
  else
    {
      /* We can't use max_issue; just return the first available element.  */
      int i;

      for (i = 0; i < ready.n_ready; i++)
	{
	  expr_t expr = find_expr_for_ready (i, true);

	  if (get_expr_cost (expr, fence) < 1)
	    {
	      can_issue = can_issue_more;
	      *index = i;

	      if (sched_verbose >= 2)
		sel_print ("using %dth insn from the ready list\n", i + 1);

	      break;
	    }
	}

      if (i == ready.n_ready)
	{
	  can_issue = 0;
	  *index = -1;
	}
    }

  return can_issue;
}

/* Choose the best expr from *AV_VLIW_PTR and a suitable register for it.
   BNDS and FENCE are current boundaries and scheduling fence respectively.
   Return the expr found and NULL if nothing can be issued atm.
   Write to PNEED_STALL the number of cycles to stall if no expr was found.  */
static expr_t
find_best_expr (av_set_t *av_vliw_ptr, blist_t bnds, fence_t fence,
                int *pneed_stall)
{
  expr_t best;

  /* Choose the best insn for scheduling via:
     1) sorting the ready list based on priority;
     2) calling the reorder hook;
     3) calling max_issue.  */
  best = fill_ready_list (av_vliw_ptr, bnds, fence, pneed_stall);
  if (best == NULL && ready.n_ready > 0)
    {
      int privileged_n, index;

      can_issue_more = invoke_reorder_hooks (fence);
      if (can_issue_more > 0)
        {
          /* Try choosing the best insn until we find one that is could be
             scheduled due to liveness restrictions on its destination register.
             In the future, we'd like to choose once and then just probe insns
             in the order of their priority.  */
          invoke_dfa_lookahead_guard ();
          privileged_n = calculate_privileged_insns ();
          can_issue_more = choose_best_insn (fence, privileged_n, &index);
          if (can_issue_more)
            best = find_expr_for_ready (index, true);
        }
      /* We had some available insns, so if we can't issue them,
         we have a stall.  */
      if (can_issue_more == 0)
        {
          best = NULL;
          *pneed_stall = 1;
        }
    }

  if (best != NULL)
    {
      can_issue_more = invoke_aftermath_hooks (fence, EXPR_INSN_RTX (best),
                                               can_issue_more);
      if (targetm.sched.variable_issue
	  && can_issue_more == 0)
        *pneed_stall = 1;
    }

  if (sched_verbose >= 2)
    {
      if (best != NULL)
        {
          sel_print ("Best expression (vliw form): ");
          dump_expr (best);
          sel_print ("; cycle %d\n", FENCE_CYCLE (fence));
        }
      else
        sel_print ("No best expr found!\n");
    }

  return best;
}


/* Functions that implement the core of the scheduler.  */


/* Emit an instruction from EXPR with SEQNO and VINSN after
   PLACE_TO_INSERT.  */
static insn_t
emit_insn_from_expr_after (expr_t expr, vinsn_t vinsn, int seqno,
                           insn_t place_to_insert)
{
  /* This assert fails when we have identical instructions
     one of which dominates the other.  In this case move_op ()
     finds the first instruction and doesn't search for second one.
     The solution would be to compute av_set after the first found
     insn and, if insn present in that set, continue searching.
     For now we workaround this issue in move_op.  */
  gcc_assert (!INSN_IN_STREAM_P (EXPR_INSN_RTX (expr)));

  if (EXPR_WAS_RENAMED (expr))
    {
      unsigned regno = expr_dest_regno (expr);

      if (HARD_REGISTER_NUM_P (regno))
	{
	  df_set_regs_ever_live (regno, true);
	  reg_rename_tick[regno] = ++reg_rename_this_tick;
	}
    }

  return sel_gen_insn_from_expr_after (expr, vinsn, seqno,
                                       place_to_insert);
}

/* Return TRUE if BB can hold bookkeeping code.  */
static bool
block_valid_for_bookkeeping_p (basic_block bb)
{
  insn_t bb_end = BB_END (bb);

  if (!in_current_region_p (bb) || EDGE_COUNT (bb->succs) > 1)
    return false;

  if (INSN_P (bb_end))
    {
      if (INSN_SCHED_TIMES (bb_end) > 0)
	return false;
    }
  else
    gcc_assert (NOTE_INSN_BASIC_BLOCK_P (bb_end));

  return true;
}

/* Attempt to find a block that can hold bookkeeping code for path(s) incoming
   into E2->dest, except from E1->src (there may be a sequence of empty basic
   blocks between E1->src and E2->dest).  Return found block, or NULL if new
   one must be created.  If LAX holds, don't assume there is a simple path
   from E1->src to E2->dest.  */
static basic_block
find_block_for_bookkeeping (edge e1, edge e2, bool lax)
{
  basic_block candidate_block = NULL;
  edge e;

  /* Loop over edges from E1 to E2, inclusive.  */
  for (e = e1; !lax || e->dest != EXIT_BLOCK_PTR_FOR_FN (cfun); e =
       EDGE_SUCC (e->dest, 0))
    {
      if (EDGE_COUNT (e->dest->preds) == 2)
	{
	  if (candidate_block == NULL)
	    candidate_block = (EDGE_PRED (e->dest, 0) == e
			       ? EDGE_PRED (e->dest, 1)->src
			       : EDGE_PRED (e->dest, 0)->src);
	  else
	    /* Found additional edge leading to path from e1 to e2
	       from aside.  */
	    return NULL;
	}
      else if (EDGE_COUNT (e->dest->preds) > 2)
	/* Several edges leading to path from e1 to e2 from aside.  */
	return NULL;

      if (e == e2)
	return ((!lax || candidate_block)
		&& block_valid_for_bookkeeping_p (candidate_block)
		? candidate_block
		: NULL);

      if (lax && EDGE_COUNT (e->dest->succs) != 1)
	return NULL;
    }

  if (lax)
    return NULL;

  gcc_unreachable ();
}

/* Create new basic block for bookkeeping code for path(s) incoming into
   E2->dest, except from E1->src.  Return created block.  */
static basic_block
create_block_for_bookkeeping (edge e1, edge e2)
{
  basic_block new_bb, bb = e2->dest;

  /* Check that we don't spoil the loop structure.  */
  if (current_loop_nest)
    {
      basic_block latch = current_loop_nest->latch;

      /* We do not split header.  */
      gcc_assert (e2->dest != current_loop_nest->header);

      /* We do not redirect the only edge to the latch block.  */
      gcc_assert (e1->dest != latch
		  || !single_pred_p (latch)
		  || e1 != single_pred_edge (latch));
    }

  /* Split BB to insert BOOK_INSN there.  */
  new_bb = sched_split_block (bb, NULL);

  /* Move note_list from the upper bb.  */
  gcc_assert (BB_NOTE_LIST (new_bb) == NULL_RTX);
  BB_NOTE_LIST (new_bb) = BB_NOTE_LIST (bb);
  BB_NOTE_LIST (bb) = NULL;

  gcc_assert (e2->dest == bb);

  /* Skip block for bookkeeping copy when leaving E1->src.  */
  if (e1->flags & EDGE_FALLTHRU)
    sel_redirect_edge_and_branch_force (e1, new_bb);
  else
    sel_redirect_edge_and_branch (e1, new_bb);

  gcc_assert (e1->dest == new_bb);
  gcc_assert (sel_bb_empty_p (bb));

  /* To keep basic block numbers in sync between debug and non-debug
     compilations, we have to rotate blocks here.  Consider that we
     started from (a,b)->d, (c,d)->e, and d contained only debug
     insns.  It would have been removed before if the debug insns
     weren't there, so we'd have split e rather than d.  So what we do
     now is to swap the block numbers of new_bb and
     single_succ(new_bb) == e, so that the insns that were in e before
     get the new block number.  */

  if (MAY_HAVE_DEBUG_INSNS)
    {
      basic_block succ;
      insn_t insn = sel_bb_head (new_bb);
      insn_t last;

      if (DEBUG_INSN_P (insn)
	  && single_succ_p (new_bb)
	  && (succ = single_succ (new_bb))
	  && succ != EXIT_BLOCK_PTR_FOR_FN (cfun)
	  && DEBUG_INSN_P ((last = sel_bb_end (new_bb))))
	{
	  while (insn != last && (DEBUG_INSN_P (insn) || NOTE_P (insn)))
	    insn = NEXT_INSN (insn);

	  if (insn == last)
	    {
	      sel_global_bb_info_def gbi;
	      sel_region_bb_info_def rbi;

	      if (sched_verbose >= 2)
		sel_print ("Swapping block ids %i and %i\n",
			   new_bb->index, succ->index);

	      std::swap (new_bb->index, succ->index);

	      SET_BASIC_BLOCK_FOR_FN (cfun, new_bb->index, new_bb);
	      SET_BASIC_BLOCK_FOR_FN (cfun, succ->index, succ);

	      memcpy (&gbi, SEL_GLOBAL_BB_INFO (new_bb), sizeof (gbi));
	      memcpy (SEL_GLOBAL_BB_INFO (new_bb), SEL_GLOBAL_BB_INFO (succ),
		      sizeof (gbi));
	      memcpy (SEL_GLOBAL_BB_INFO (succ), &gbi, sizeof (gbi));

	      memcpy (&rbi, SEL_REGION_BB_INFO (new_bb), sizeof (rbi));
	      memcpy (SEL_REGION_BB_INFO (new_bb), SEL_REGION_BB_INFO (succ),
		      sizeof (rbi));
	      memcpy (SEL_REGION_BB_INFO (succ), &rbi, sizeof (rbi));

	      std::swap (BLOCK_TO_BB (new_bb->index),
			 BLOCK_TO_BB (succ->index));

	      std::swap (CONTAINING_RGN (new_bb->index),
			 CONTAINING_RGN (succ->index));

	      for (int i = 0; i < current_nr_blocks; i++)
		if (BB_TO_BLOCK (i) == succ->index)
		  BB_TO_BLOCK (i) = new_bb->index;
		else if (BB_TO_BLOCK (i) == new_bb->index)
		  BB_TO_BLOCK (i) = succ->index;

	      FOR_BB_INSNS (new_bb, insn)
		if (INSN_P (insn))
		  EXPR_ORIG_BB_INDEX (INSN_EXPR (insn)) = new_bb->index;

	      FOR_BB_INSNS (succ, insn)
		if (INSN_P (insn))
		  EXPR_ORIG_BB_INDEX (INSN_EXPR (insn)) = succ->index;

	      if (bitmap_clear_bit (code_motion_visited_blocks, new_bb->index))
		bitmap_set_bit (code_motion_visited_blocks, succ->index);

	      gcc_assert (LABEL_P (BB_HEAD (new_bb))
			  && LABEL_P (BB_HEAD (succ)));

	      if (sched_verbose >= 4)
		sel_print ("Swapping code labels %i and %i\n",
			   CODE_LABEL_NUMBER (BB_HEAD (new_bb)),
			   CODE_LABEL_NUMBER (BB_HEAD (succ)));

	      std::swap (CODE_LABEL_NUMBER (BB_HEAD (new_bb)),
			 CODE_LABEL_NUMBER (BB_HEAD (succ)));
	    }
	}
    }

  return bb;
}

/* Return insn after which we must insert bookkeeping code for path(s) incoming
   into E2->dest, except from E1->src.  If the returned insn immediately
   precedes a fence, assign that fence to *FENCE_TO_REWIND.  */
static insn_t
find_place_for_bookkeeping (edge e1, edge e2, fence_t *fence_to_rewind)
{
  insn_t place_to_insert;
  /* Find a basic block that can hold bookkeeping.  If it can be found, do not
     create new basic block, but insert bookkeeping there.  */
  basic_block book_block = find_block_for_bookkeeping (e1, e2, FALSE);

  if (book_block)
    {
      place_to_insert = BB_END (book_block);

      /* Don't use a block containing only debug insns for
	 bookkeeping, this causes scheduling differences between debug
	 and non-debug compilations, for the block would have been
	 removed already.  */
      if (DEBUG_INSN_P (place_to_insert))
	{
	  rtx_insn *insn = sel_bb_head (book_block);

	  while (insn != place_to_insert &&
		 (DEBUG_INSN_P (insn) || NOTE_P (insn)))
	    insn = NEXT_INSN (insn);

	  if (insn == place_to_insert)
	    book_block = NULL;
	}
    }

  if (!book_block)
    {
      book_block = create_block_for_bookkeeping (e1, e2);
      place_to_insert = BB_END (book_block);
      if (sched_verbose >= 9)
	sel_print ("New block is %i, split from bookkeeping block %i\n",
		   EDGE_SUCC (book_block, 0)->dest->index, book_block->index);
    }
  else
    {
      if (sched_verbose >= 9)
	sel_print ("Pre-existing bookkeeping block is %i\n", book_block->index);
    }

  *fence_to_rewind = NULL;
  /* If basic block ends with a jump, insert bookkeeping code right before it.
     Notice if we are crossing a fence when taking PREV_INSN.  */
  if (INSN_P (place_to_insert) && control_flow_insn_p (place_to_insert))
    {
      *fence_to_rewind = flist_lookup (fences, place_to_insert);
      place_to_insert = PREV_INSN (place_to_insert);
    }

  return place_to_insert;
}

/* Find a proper seqno for bookkeeing insn inserted at PLACE_TO_INSERT
   for JOIN_POINT.   */
static int
find_seqno_for_bookkeeping (insn_t place_to_insert, insn_t join_point)
{
  int seqno;

  /* Check if we are about to insert bookkeeping copy before a jump, and use
     jump's seqno for the copy; otherwise, use JOIN_POINT's seqno.  */
  rtx_insn *next = NEXT_INSN (place_to_insert);
  if (INSN_P (next)
      && JUMP_P (next)
      && BLOCK_FOR_INSN (next) == BLOCK_FOR_INSN (place_to_insert))
    {
      gcc_assert (INSN_SCHED_TIMES (next) == 0);
      seqno = INSN_SEQNO (next);
    }
  else if (INSN_SEQNO (join_point) > 0)
    seqno = INSN_SEQNO (join_point);
  else
    {
      seqno = get_seqno_by_preds (place_to_insert);

      /* Sometimes the fences can move in such a way that there will be
         no instructions with positive seqno around this bookkeeping.
         This means that there will be no way to get to it by a regular
         fence movement.  Never mind because we pick up such pieces for
         rescheduling anyways, so any positive value will do for now.  */
      if (seqno < 0)
        {
          gcc_assert (pipelining_p);
          seqno = 1;
        }
    }

  gcc_assert (seqno > 0);
  return seqno;
}

/* Insert bookkeeping copy of C_EXPS's insn after PLACE_TO_INSERT, assigning
   NEW_SEQNO to it.  Return created insn.  */
static insn_t
emit_bookkeeping_insn (insn_t place_to_insert, expr_t c_expr, int new_seqno)
{
  rtx_insn *new_insn_rtx = create_copy_of_insn_rtx (EXPR_INSN_RTX (c_expr));

  vinsn_t new_vinsn
    = create_vinsn_from_insn_rtx (new_insn_rtx,
				  VINSN_UNIQUE_P (EXPR_VINSN (c_expr)));

  insn_t new_insn = emit_insn_from_expr_after (c_expr, new_vinsn, new_seqno,
					       place_to_insert);

  INSN_SCHED_TIMES (new_insn) = 0;
  bitmap_set_bit (current_copies, INSN_UID (new_insn));

  return new_insn;
}

/* Generate a bookkeeping copy of C_EXPR's insn for path(s) incoming into to
   E2->dest, except from E1->src (there may be a sequence of empty blocks
   between E1->src and E2->dest).  Return block containing the copy.
   All scheduler data is initialized for the newly created insn.  */
static basic_block
generate_bookkeeping_insn (expr_t c_expr, edge e1, edge e2)
{
  insn_t join_point, place_to_insert, new_insn;
  int new_seqno;
  bool need_to_exchange_data_sets;
  fence_t fence_to_rewind;

  if (sched_verbose >= 4)
    sel_print ("Generating bookkeeping insn (%d->%d)\n", e1->src->index,
	       e2->dest->index);

  join_point = sel_bb_head (e2->dest);
  place_to_insert = find_place_for_bookkeeping (e1, e2, &fence_to_rewind);
  new_seqno = find_seqno_for_bookkeeping (place_to_insert, join_point);
  need_to_exchange_data_sets
    = sel_bb_empty_p (BLOCK_FOR_INSN (place_to_insert));

  new_insn = emit_bookkeeping_insn (place_to_insert, c_expr, new_seqno);

  if (fence_to_rewind)
    FENCE_INSN (fence_to_rewind) = new_insn;

  /* When inserting bookkeeping insn in new block, av sets should be
     following: old basic block (that now holds bookkeeping) data sets are
     the same as was before generation of bookkeeping, and new basic block
     (that now hold all other insns of old basic block) data sets are
     invalid.  So exchange data sets for these basic blocks as sel_split_block
     mistakenly exchanges them in this case.  Cannot do it earlier because
     when single instruction is added to new basic block it should hold NULL
     lv_set.  */
  if (need_to_exchange_data_sets)
    exchange_data_sets (BLOCK_FOR_INSN (new_insn),
			BLOCK_FOR_INSN (join_point));

  stat_bookkeeping_copies++;
  return BLOCK_FOR_INSN (new_insn);
}

/* Remove from AV_PTR all insns that may need bookkeeping when scheduling
   on FENCE, but we are unable to copy them.  */
static void
remove_insns_that_need_bookkeeping (fence_t fence, av_set_t *av_ptr)
{
  expr_t expr;
  av_set_iterator i;

  /*  An expression does not need bookkeeping if it is available on all paths
      from current block to original block and current block dominates
      original block.  We check availability on all paths by examining
      EXPR_SPEC; this is not equivalent, because it may be positive even
      if expr is available on all paths (but if expr is not available on
      any path, EXPR_SPEC will be positive).  */

  FOR_EACH_EXPR_1 (expr, i, av_ptr)
    {
      if (!control_flow_insn_p (EXPR_INSN_RTX (expr))
	  && (!bookkeeping_p || VINSN_UNIQUE_P (EXPR_VINSN (expr)))
	  && (EXPR_SPEC (expr)
	      || !EXPR_ORIG_BB_INDEX (expr)
	      || !dominated_by_p (CDI_DOMINATORS,
				  BASIC_BLOCK_FOR_FN (cfun,
						      EXPR_ORIG_BB_INDEX (expr)),
				  BLOCK_FOR_INSN (FENCE_INSN (fence)))))
	{
          if (sched_verbose >= 4)
            sel_print ("Expr %d removed because it would need bookkeeping, which "
                       "cannot be created\n", INSN_UID (EXPR_INSN_RTX (expr)));
	  av_set_iter_remove (&i);
	}
    }
}

/* Moving conditional jump through some instructions.

   Consider example:

       ...                     <- current scheduling point
       NOTE BASIC BLOCK:       <- bb header
       (p8)  add r14=r14+0x9;;
       (p8)  mov [r14]=r23
       (!p8) jump L1;;
       NOTE BASIC BLOCK:
       ...

   We can schedule jump one cycle earlier, than mov, because they cannot be
   executed together as their predicates are mutually exclusive.

   This is done in this way: first, new fallthrough basic block is created
   after jump (it is always can be done, because there already should be a
   fallthrough block, where control flow goes in case of predicate being true -
   in our example; otherwise there should be a dependence between those
   instructions and jump and we cannot schedule jump right now);
   next, all instructions between jump and current scheduling point are moved
   to this new block.  And the result is this:

      NOTE BASIC BLOCK:
      (!p8) jump L1           <- current scheduling point
      NOTE BASIC BLOCK:       <- bb header
      (p8)  add r14=r14+0x9;;
      (p8)  mov [r14]=r23
      NOTE BASIC BLOCK:
      ...
*/
static void
move_cond_jump (rtx_insn *insn, bnd_t bnd)
{
  edge ft_edge;
  basic_block block_from, block_next, block_new, block_bnd, bb;
  rtx_insn *next, *prev, *link, *head;

  block_from = BLOCK_FOR_INSN (insn);
  block_bnd = BLOCK_FOR_INSN (BND_TO (bnd));
  prev = BND_TO (bnd);

  /* Moving of jump should not cross any other jumps or beginnings of new
     basic blocks.  The only exception is when we move a jump through
     mutually exclusive insns along fallthru edges.  */
  if (flag_checking && block_from != block_bnd)
    {
      bb = block_from;
      for (link = PREV_INSN (insn); link != PREV_INSN (prev);
           link = PREV_INSN (link))
        {
          if (INSN_P (link))
            gcc_assert (sched_insns_conditions_mutex_p (insn, link));
          if (BLOCK_FOR_INSN (link) && BLOCK_FOR_INSN (link) != bb)
            {
              gcc_assert (single_pred (bb) == BLOCK_FOR_INSN (link));
              bb = BLOCK_FOR_INSN (link);
            }
        }
    }

  /* Jump is moved to the boundary.  */
  next = PREV_INSN (insn);
  BND_TO (bnd) = insn;

  ft_edge = find_fallthru_edge_from (block_from);
  block_next = ft_edge->dest;
  /* There must be a fallthrough block (or where should go
  control flow in case of false jump predicate otherwise?).  */
  gcc_assert (block_next);

  /* Create new empty basic block after source block.  */
  block_new = sel_split_edge (ft_edge);
  gcc_assert (block_new->next_bb == block_next
              && block_from->next_bb == block_new);

  /* Move all instructions except INSN to BLOCK_NEW.  */
  bb = block_bnd;
  head = BB_HEAD (block_new);
  while (bb != block_from->next_bb)
    {
      rtx_insn *from, *to;
      from = bb == block_bnd ? prev : sel_bb_head (bb);
      to = bb == block_from ? next : sel_bb_end (bb);

      /* The jump being moved can be the first insn in the block.
         In this case we don't have to move anything in this block.  */
      if (NEXT_INSN (to) != from)
        {
          reorder_insns (from, to, head);

          for (link = to; link != head; link = PREV_INSN (link))
            EXPR_ORIG_BB_INDEX (INSN_EXPR (link)) = block_new->index;
          head = to;
        }

      /* Cleanup possibly empty blocks left.  */
      block_next = bb->next_bb;
      if (bb != block_from)
	tidy_control_flow (bb, false);
      bb = block_next;
    }

  /* Assert there is no jump to BLOCK_NEW, only fallthrough edge.  */
  gcc_assert (NOTE_INSN_BASIC_BLOCK_P (BB_HEAD (block_new)));

  gcc_assert (!sel_bb_empty_p (block_from)
              && !sel_bb_empty_p (block_new));

  /* Update data sets for BLOCK_NEW to represent that INSN and
     instructions from the other branch of INSN is no longer
     available at BLOCK_NEW.  */
  BB_AV_LEVEL (block_new) = global_level;
  gcc_assert (BB_LV_SET (block_new) == NULL);
  BB_LV_SET (block_new) = get_clear_regset_from_pool ();
  update_data_sets (sel_bb_head (block_new));

  /* INSN is a new basic block header - so prepare its data
     structures and update availability and liveness sets.  */
  update_data_sets (insn);

  if (sched_verbose >= 4)
    sel_print ("Moving jump %d\n", INSN_UID (insn));
}

/* Remove nops generated during move_op for preventing removal of empty
   basic blocks.  */
static void
remove_temp_moveop_nops (bool full_tidying)
{
  int i;
  insn_t insn;

  FOR_EACH_VEC_ELT (vec_temp_moveop_nops, i, insn)
    {
      gcc_assert (INSN_NOP_P (insn));
      return_nop_to_pool (insn, full_tidying);
    }

  /* Empty the vector.  */
  if (vec_temp_moveop_nops.length () > 0)
    vec_temp_moveop_nops.block_remove (0, vec_temp_moveop_nops.length ());
}

/* Records the maximal UID before moving up an instruction.  Used for
   distinguishing between bookkeeping copies and original insns.  */
static int max_uid_before_move_op = 0;

/* Remove from AV_VLIW_P all instructions but next when debug counter
   tells us so.  Next instruction is fetched from BNDS.  */
static void
remove_insns_for_debug (blist_t bnds, av_set_t *av_vliw_p)
{
  if (! dbg_cnt (sel_sched_insn_cnt))
    /* Leave only the next insn in av_vliw.  */
    {
      av_set_iterator av_it;
      expr_t expr;
      bnd_t bnd = BLIST_BND (bnds);
      insn_t next = BND_TO (bnd);

      gcc_assert (BLIST_NEXT (bnds) == NULL);

      FOR_EACH_EXPR_1 (expr, av_it, av_vliw_p)
        if (EXPR_INSN_RTX (expr) != next)
          av_set_iter_remove (&av_it);
    }
}

/* Compute available instructions on BNDS.  FENCE is the current fence.  Write
   the computed set to *AV_VLIW_P.  */
static void
compute_av_set_on_boundaries (fence_t fence, blist_t bnds, av_set_t *av_vliw_p)
{
  if (sched_verbose >= 2)
    {
      sel_print ("Boundaries: ");
      dump_blist (bnds);
      sel_print ("\n");
    }

  for (; bnds; bnds = BLIST_NEXT (bnds))
    {
      bnd_t bnd = BLIST_BND (bnds);
      av_set_t av1_copy;
      insn_t bnd_to = BND_TO (bnd);

      /* Rewind BND->TO to the basic block header in case some bookkeeping
         instructions were inserted before BND->TO and it needs to be
         adjusted.  */
      if (sel_bb_head_p (bnd_to))
        gcc_assert (INSN_SCHED_TIMES (bnd_to) == 0);
      else
        while (INSN_SCHED_TIMES (PREV_INSN (bnd_to)) == 0)
          {
            bnd_to = PREV_INSN (bnd_to);
            if (sel_bb_head_p (bnd_to))
              break;
          }

      if (BND_TO (bnd) != bnd_to)
	{
  	  gcc_assert (FENCE_INSN (fence) == BND_TO (bnd));
	  FENCE_INSN (fence) = bnd_to;
	  BND_TO (bnd) = bnd_to;
	}

      av_set_clear (&BND_AV (bnd));
      BND_AV (bnd) = compute_av_set (BND_TO (bnd), NULL, 0, true);

      av_set_clear (&BND_AV1 (bnd));
      BND_AV1 (bnd) = av_set_copy (BND_AV (bnd));

      moveup_set_inside_insn_group (&BND_AV1 (bnd), NULL);

      av1_copy = av_set_copy (BND_AV1 (bnd));
      av_set_union_and_clear (av_vliw_p, &av1_copy, NULL);
    }

  if (sched_verbose >= 2)
    {
      sel_print ("Available exprs (vliw form): ");
      dump_av_set (*av_vliw_p);
      sel_print ("\n");
    }
}

/* Calculate the sequential av set on BND corresponding to the EXPR_VLIW
   expression.  When FOR_MOVEOP is true, also replace the register of
   expressions found with the register from EXPR_VLIW.  */
static av_set_t
find_sequential_best_exprs (bnd_t bnd, expr_t expr_vliw, bool for_moveop)
{
  av_set_t expr_seq = NULL;
  expr_t expr;
  av_set_iterator i;

  FOR_EACH_EXPR (expr, i, BND_AV (bnd))
    {
      if (equal_after_moveup_path_p (expr, NULL, expr_vliw))
        {
          if (for_moveop)
            {
              /* The sequential expression has the right form to pass
                 to move_op except when renaming happened.  Put the
                 correct register in EXPR then.  */
              if (EXPR_SEPARABLE_P (expr) && REG_P (EXPR_LHS (expr)))
		{
                  if (expr_dest_regno (expr) != expr_dest_regno (expr_vliw))
		    {
		      replace_dest_with_reg_in_expr (expr, EXPR_LHS (expr_vliw));
		      stat_renamed_scheduled++;
		    }
		  /* Also put the correct TARGET_AVAILABLE bit on the expr.
                     This is needed when renaming came up with original
                     register.  */
                  else if (EXPR_TARGET_AVAILABLE (expr)
                           != EXPR_TARGET_AVAILABLE (expr_vliw))
		    {
		      gcc_assert (EXPR_TARGET_AVAILABLE (expr_vliw) == 1);
		      EXPR_TARGET_AVAILABLE (expr) = 1;
		    }
		}
              if (EXPR_WAS_SUBSTITUTED (expr))
                stat_substitutions_total++;
            }

          av_set_add (&expr_seq, expr);

          /* With substitution inside insn group, it is possible
             that more than one expression in expr_seq will correspond
             to expr_vliw.  In this case, choose one as the attempt to
             move both leads to miscompiles.  */
          break;
        }
    }

  if (for_moveop && sched_verbose >= 2)
    {
      sel_print ("Best expression(s) (sequential form): ");
      dump_av_set (expr_seq);
      sel_print ("\n");
    }

  return expr_seq;
}


/* Move nop to previous block.  */
static void ATTRIBUTE_UNUSED
move_nop_to_previous_block (insn_t nop, basic_block prev_bb)
{
  insn_t prev_insn, next_insn;

  gcc_assert (sel_bb_head_p (nop)
              && prev_bb == BLOCK_FOR_INSN (nop)->prev_bb);
  rtx_note *note = bb_note (BLOCK_FOR_INSN (nop));
  prev_insn = sel_bb_end (prev_bb);
  next_insn = NEXT_INSN (nop);
  gcc_assert (prev_insn != NULL_RTX
              && PREV_INSN (note) == prev_insn);

  SET_NEXT_INSN (prev_insn) = nop;
  SET_PREV_INSN (nop) = prev_insn;

  SET_PREV_INSN (note) = nop;
  SET_NEXT_INSN (note) = next_insn;

  SET_NEXT_INSN (nop) = note;
  SET_PREV_INSN (next_insn) = note;

  BB_END (prev_bb) = nop;
  BLOCK_FOR_INSN (nop) = prev_bb;
}

/* Prepare a place to insert the chosen expression on BND.  */
static insn_t
prepare_place_to_insert (bnd_t bnd)
{
  insn_t place_to_insert;

  /* Init place_to_insert before calling move_op, as the later
     can possibly remove BND_TO (bnd).  */
  if (/* If this is not the first insn scheduled.  */
      BND_PTR (bnd))
    {
      /* Add it after last scheduled.  */
      place_to_insert = ILIST_INSN (BND_PTR (bnd));
      if (DEBUG_INSN_P (place_to_insert))
	{
	  ilist_t l = BND_PTR (bnd);
	  while ((l = ILIST_NEXT (l)) &&
		 DEBUG_INSN_P (ILIST_INSN (l)))
	    ;
	  if (!l)
	    place_to_insert = NULL;
	}
    }
  else
    place_to_insert = NULL;

  if (!place_to_insert)
    {
      /* Add it before BND_TO.  The difference is in the
         basic block, where INSN will be added.  */
      place_to_insert = get_nop_from_pool (BND_TO (bnd));
      gcc_assert (BLOCK_FOR_INSN (place_to_insert)
                  == BLOCK_FOR_INSN (BND_TO (bnd)));
    }

  return place_to_insert;
}

/* Find original instructions for EXPR_SEQ and move it to BND boundary.
   Return the expression to emit in C_EXPR.  */
static bool
move_exprs_to_boundary (bnd_t bnd, expr_t expr_vliw,
                        av_set_t expr_seq, expr_t c_expr)
{
  bool b, should_move;
  unsigned book_uid;
  bitmap_iterator bi;
  int n_bookkeeping_copies_before_moveop;

  /* Make a move.  This call will remove the original operation,
     insert all necessary bookkeeping instructions and update the
     data sets.  After that all we have to do is add the operation
     at before BND_TO (BND).  */
  n_bookkeeping_copies_before_moveop = stat_bookkeeping_copies;
  max_uid_before_move_op = get_max_uid ();
  bitmap_clear (current_copies);
  bitmap_clear (current_originators);

  b = move_op (BND_TO (bnd), expr_seq, expr_vliw,
               get_dest_from_orig_ops (expr_seq), c_expr, &should_move);

  /* We should be able to find the expression we've chosen for
     scheduling.  */
  gcc_assert (b);

  if (stat_bookkeeping_copies > n_bookkeeping_copies_before_moveop)
    stat_insns_needed_bookkeeping++;

  EXECUTE_IF_SET_IN_BITMAP (current_copies, 0, book_uid, bi)
    {
      unsigned uid;
      bitmap_iterator bi;

      /* We allocate these bitmaps lazily.  */
      if (! INSN_ORIGINATORS_BY_UID (book_uid))
        INSN_ORIGINATORS_BY_UID (book_uid) = BITMAP_ALLOC (NULL);

      bitmap_copy (INSN_ORIGINATORS_BY_UID (book_uid),
                   current_originators);

      /* Transitively add all originators' originators.  */
      EXECUTE_IF_SET_IN_BITMAP (current_originators, 0, uid, bi)
       if (INSN_ORIGINATORS_BY_UID (uid))
	 bitmap_ior_into (INSN_ORIGINATORS_BY_UID (book_uid),
			  INSN_ORIGINATORS_BY_UID (uid));
    }

  return should_move;
}


/* Debug a DFA state as an array of bytes.  */
static void
debug_state (state_t state)
{
  unsigned char *p;
  unsigned int i, size = dfa_state_size;

  sel_print ("state (%u):", size);
  for (i = 0, p = (unsigned char *) state; i < size; i++)
    sel_print (" %d", p[i]);
  sel_print ("\n");
}

/* Advance state on FENCE with INSN.  Return true if INSN is
   an ASM, and we should advance state once more.  */
static bool
advance_state_on_fence (fence_t fence, insn_t insn)
{
  bool asm_p;

  if (recog_memoized (insn) >= 0)
    {
      int res;
      state_t temp_state = alloca (dfa_state_size);

      gcc_assert (!INSN_ASM_P (insn));
      asm_p = false;

      memcpy (temp_state, FENCE_STATE (fence), dfa_state_size);
      res = state_transition (FENCE_STATE (fence), insn);
      gcc_assert (res < 0);

      if (memcmp (temp_state, FENCE_STATE (fence), dfa_state_size))
        {
          FENCE_ISSUED_INSNS (fence)++;

          /* We should never issue more than issue_rate insns.  */
          if (FENCE_ISSUED_INSNS (fence) > issue_rate)
            gcc_unreachable ();
        }
    }
  else
    {
      /* This could be an ASM insn which we'd like to schedule
         on the next cycle.  */
      asm_p = INSN_ASM_P (insn);
      if (!FENCE_STARTS_CYCLE_P (fence) && asm_p)
        advance_one_cycle (fence);
    }

  if (sched_verbose >= 2)
    debug_state (FENCE_STATE (fence));
  if (!DEBUG_INSN_P (insn))
    FENCE_STARTS_CYCLE_P (fence) = 0;
  FENCE_ISSUE_MORE (fence) = can_issue_more;
  return asm_p;
}

/* Update FENCE on which INSN was scheduled and this INSN, too.  NEED_STALL
   is nonzero if we need to stall after issuing INSN.  */
static void
update_fence_and_insn (fence_t fence, insn_t insn, int need_stall)
{
  bool asm_p;

  /* First, reflect that something is scheduled on this fence.  */
  asm_p = advance_state_on_fence (fence, insn);
  FENCE_LAST_SCHEDULED_INSN (fence) = insn;
  vec_safe_push (FENCE_EXECUTING_INSNS (fence), insn);
  if (SCHED_GROUP_P (insn))
    {
      FENCE_SCHED_NEXT (fence) = INSN_SCHED_NEXT (insn);
      SCHED_GROUP_P (insn) = 0;
    }
  else
    FENCE_SCHED_NEXT (fence) = NULL;
  if (INSN_UID (insn) < FENCE_READY_TICKS_SIZE (fence))
    FENCE_READY_TICKS (fence) [INSN_UID (insn)] = 0;

  /* Set instruction scheduling info.  This will be used in bundling,
     pipelining, tick computations etc.  */
  ++INSN_SCHED_TIMES (insn);
  EXPR_TARGET_AVAILABLE (INSN_EXPR (insn)) = true;
  EXPR_ORIG_SCHED_CYCLE (INSN_EXPR (insn)) = FENCE_CYCLE (fence);
  INSN_AFTER_STALL_P (insn) = FENCE_AFTER_STALL_P (fence);
  INSN_SCHED_CYCLE (insn) = FENCE_CYCLE (fence);

  /* This does not account for adjust_cost hooks, just add the biggest
     constant the hook may add to the latency.  TODO: make this
     a target dependent constant.  */
  INSN_READY_CYCLE (insn)
    = INSN_SCHED_CYCLE (insn) + (INSN_CODE (insn) < 0
                                 ? 1
                                 : maximal_insn_latency (insn) + 1);

  /* Change these fields last, as they're used above.  */
  FENCE_AFTER_STALL_P (fence) = 0;
  if (asm_p || need_stall)
    advance_one_cycle (fence);

  /* Indicate that we've scheduled something on this fence.  */
  FENCE_SCHEDULED_P (fence) = true;
  scheduled_something_on_previous_fence = true;

  /* Print debug information when insn's fields are updated.  */
  if (sched_verbose >= 2)
    {
      sel_print ("Scheduling insn: ");
      dump_insn_1 (insn, 1);
      sel_print ("\n");
    }
}

/* Update boundary BND (and, if needed, FENCE) with INSN, remove the
   old boundary from BNDSP, add new boundaries to BNDS_TAIL_P and
   return it.  */
static blist_t *
update_boundaries (fence_t fence, bnd_t bnd, insn_t insn, blist_t *bndsp,
                   blist_t *bnds_tailp)
{
  succ_iterator si;
  insn_t succ;

  advance_deps_context (BND_DC (bnd), insn);
  FOR_EACH_SUCC_1 (succ, si, insn,
                   SUCCS_NORMAL | SUCCS_SKIP_TO_LOOP_EXITS)
    {
      ilist_t ptr = ilist_copy (BND_PTR (bnd));

      ilist_add (&ptr, insn);

      if (DEBUG_INSN_P (insn) && sel_bb_end_p (insn)
	  && is_ineligible_successor (succ, ptr))
	{
	  ilist_clear (&ptr);
	  continue;
	}

      if (FENCE_INSN (fence) == insn && !sel_bb_end_p (insn))
	{
	  if (sched_verbose >= 9)
	    sel_print ("Updating fence insn from %i to %i\n",
		       INSN_UID (insn), INSN_UID (succ));
	  FENCE_INSN (fence) = succ;
	}
      blist_add (bnds_tailp, succ, ptr, BND_DC (bnd));
      bnds_tailp = &BLIST_NEXT (*bnds_tailp);
    }

  blist_remove (bndsp);
  return bnds_tailp;
}

/* Schedule EXPR_VLIW on BND.  Return the insn emitted.  */
static insn_t
schedule_expr_on_boundary (bnd_t bnd, expr_t expr_vliw, int seqno)
{
  av_set_t expr_seq;
  expr_t c_expr = XALLOCA (expr_def);
  insn_t place_to_insert;
  insn_t insn;
  bool should_move;

  expr_seq = find_sequential_best_exprs (bnd, expr_vliw, true);

  /* In case of scheduling a jump skipping some other instructions,
     prepare CFG.  After this, jump is at the boundary and can be
     scheduled as usual insn by MOVE_OP.  */
  if (vinsn_cond_branch_p (EXPR_VINSN (expr_vliw)))
    {
      insn = EXPR_INSN_RTX (expr_vliw);

      /* Speculative jumps are not handled.  */
      if (insn != BND_TO (bnd)
          && !sel_insn_is_speculation_check (insn))
        move_cond_jump (insn, bnd);
    }

  /* Find a place for C_EXPR to schedule.  */
  place_to_insert = prepare_place_to_insert (bnd);
  should_move = move_exprs_to_boundary (bnd, expr_vliw, expr_seq, c_expr);
  clear_expr (c_expr);

  /* Add the instruction.  The corner case to care about is when
     the expr_seq set has more than one expr, and we chose the one that
     is not equal to expr_vliw.  Then expr_vliw may be insn in stream, and
     we can't use it.  Generate the new vinsn.  */
  if (INSN_IN_STREAM_P (EXPR_INSN_RTX (expr_vliw)))
    {
      vinsn_t vinsn_new;

      vinsn_new = vinsn_copy (EXPR_VINSN (expr_vliw), false);
      change_vinsn_in_expr (expr_vliw, vinsn_new);
      should_move = false;
    }
  if (should_move)
    insn = sel_move_insn (expr_vliw, seqno, place_to_insert);
  else
    insn = emit_insn_from_expr_after (expr_vliw, NULL, seqno,
                                      place_to_insert);

  /* Return the nops generated for preserving of data sets back
     into pool.  */
  if (INSN_NOP_P (place_to_insert))
    return_nop_to_pool (place_to_insert, !DEBUG_INSN_P (insn));
  remove_temp_moveop_nops (!DEBUG_INSN_P (insn));

  av_set_clear (&expr_seq);

  /* Save the expression scheduled so to reset target availability if we'll
     meet it later on the same fence.  */
  if (EXPR_WAS_RENAMED (expr_vliw))
    vinsn_vec_add (&vec_target_unavailable_vinsns, INSN_EXPR (insn));

  /* Check that the recent movement didn't destroyed loop
     structure.  */
  gcc_assert (!pipelining_p
              || current_loop_nest == NULL
              || loop_latch_edge (current_loop_nest));
  return insn;
}

/* Stall for N cycles on FENCE.  */
static void
stall_for_cycles (fence_t fence, int n)
{
  int could_more;

  could_more = n > 1 || FENCE_ISSUED_INSNS (fence) < issue_rate;
  while (n--)
    advance_one_cycle (fence);
  if (could_more)
    FENCE_AFTER_STALL_P (fence) = 1;
}

/* Gather a parallel group of insns at FENCE and assign their seqno
   to SEQNO.  All scheduled insns are gathered in SCHEDULED_INSNS_TAILPP
   list for later recalculation of seqnos.  */
static void
fill_insns (fence_t fence, int seqno, ilist_t **scheduled_insns_tailpp)
{
  blist_t bnds = NULL, *bnds_tailp;
  av_set_t av_vliw = NULL;
  insn_t insn = FENCE_INSN (fence);

  if (sched_verbose >= 2)
    sel_print ("Starting fill_insns for insn %d, cycle %d\n",
               INSN_UID (insn), FENCE_CYCLE (fence));

  blist_add (&bnds, insn, NULL, FENCE_DC (fence));
  bnds_tailp = &BLIST_NEXT (bnds);
  set_target_context (FENCE_TC (fence));
  can_issue_more = FENCE_ISSUE_MORE (fence);
  target_bb = INSN_BB (insn);

  /* Do while we can add any operation to the current group.  */
  do
    {
      blist_t *bnds_tailp1, *bndsp;
      expr_t expr_vliw;
      int need_stall = false;
      int was_stall = 0, scheduled_insns = 0;
      int max_insns = pipelining_p ? issue_rate : 2 * issue_rate;
      int max_stall = pipelining_p ? 1 : 3;
      bool last_insn_was_debug = false;
      bool was_debug_bb_end_p = false;

      compute_av_set_on_boundaries (fence, bnds, &av_vliw);
      remove_insns_that_need_bookkeeping (fence, &av_vliw);
      remove_insns_for_debug (bnds, &av_vliw);

      /* Return early if we have nothing to schedule.  */
      if (av_vliw == NULL)
        break;

      /* Choose the best expression and, if needed, destination register
	 for it.  */
      do
        {
          expr_vliw = find_best_expr (&av_vliw, bnds, fence, &need_stall);
          if (! expr_vliw && need_stall)
            {
              /* All expressions required a stall.  Do not recompute av sets
                 as we'll get the same answer (modulo the insns between
                 the fence and its boundary, which will not be available for
                 pipelining).
		 If we are going to stall for too long, break to recompute av
		 sets and bring more insns for pipelining.  */
              was_stall++;
	      if (need_stall <= 3)
		stall_for_cycles (fence, need_stall);
	      else
		{
		  stall_for_cycles (fence, 1);
		  break;
		}
            }
        }
      while (! expr_vliw && need_stall);

      /* Now either we've selected expr_vliw or we have nothing to schedule.  */
      if (!expr_vliw)
        {
	  av_set_clear (&av_vliw);
          break;
        }

      bndsp = &bnds;
      bnds_tailp1 = bnds_tailp;

      do
	/* This code will be executed only once until we'd have several
           boundaries per fence.  */
        {
	  bnd_t bnd = BLIST_BND (*bndsp);

	  if (!av_set_is_in_p (BND_AV1 (bnd), EXPR_VINSN (expr_vliw)))
	    {
	      bndsp = &BLIST_NEXT (*bndsp);
	      continue;
	    }

          insn = schedule_expr_on_boundary (bnd, expr_vliw, seqno);
	  last_insn_was_debug = DEBUG_INSN_P (insn);
	  if (last_insn_was_debug)
	    was_debug_bb_end_p = (insn == BND_TO (bnd) && sel_bb_end_p (insn));
          update_fence_and_insn (fence, insn, need_stall);
          bnds_tailp = update_boundaries (fence, bnd, insn, bndsp, bnds_tailp);

	  /* Add insn to the list of scheduled on this cycle instructions.  */
	  ilist_add (*scheduled_insns_tailpp, insn);
	  *scheduled_insns_tailpp = &ILIST_NEXT (**scheduled_insns_tailpp);
        }
      while (*bndsp != *bnds_tailp1);

      av_set_clear (&av_vliw);
      if (!last_insn_was_debug)
	scheduled_insns++;

      /* We currently support information about candidate blocks only for
	 one 'target_bb' block.  Hence we can't schedule after jump insn,
	 as this will bring two boundaries and, hence, necessity to handle
	 information for two or more blocks concurrently.  */
      if ((last_insn_was_debug ? was_debug_bb_end_p : sel_bb_end_p (insn))
          || (was_stall
              && (was_stall >= max_stall
                  || scheduled_insns >= max_insns)))
        break;
    }
  while (bnds);

  gcc_assert (!FENCE_BNDS (fence));

  /* Update boundaries of the FENCE.  */
  while (bnds)
    {
      ilist_t ptr = BND_PTR (BLIST_BND (bnds));

      if (ptr)
	{
	  insn = ILIST_INSN (ptr);

	  if (!ilist_is_in_p (FENCE_BNDS (fence), insn))
	    ilist_add (&FENCE_BNDS (fence), insn);
	}

      blist_remove (&bnds);
    }

  /* Update target context on the fence.  */
  reset_target_context (FENCE_TC (fence), false);
}

/* All exprs in ORIG_OPS must have the same destination register or memory.
   Return that destination.  */
static rtx
get_dest_from_orig_ops (av_set_t orig_ops)
{
  rtx dest = NULL_RTX;
  av_set_iterator av_it;
  expr_t expr;
  bool first_p = true;

  FOR_EACH_EXPR (expr, av_it, orig_ops)
    {
      rtx x = EXPR_LHS (expr);

      if (first_p)
	{
	  first_p = false;
	  dest = x;
	}
      else
	gcc_assert (dest == x
		    || (dest != NULL_RTX && x != NULL_RTX
			&& rtx_equal_p (dest, x)));
    }

  return dest;
}

/* Update data sets for the bookkeeping block and record those expressions
   which become no longer available after inserting this bookkeeping.  */
static void
update_and_record_unavailable_insns (basic_block book_block)
{
  av_set_iterator i;
  av_set_t old_av_set = NULL;
  expr_t cur_expr;
  rtx_insn *bb_end = sel_bb_end (book_block);

  /* First, get correct liveness in the bookkeeping block.  The problem is
     the range between the bookeeping insn and the end of block.  */
  update_liveness_on_insn (bb_end);
  if (control_flow_insn_p (bb_end))
    update_liveness_on_insn (PREV_INSN (bb_end));

  /* If there's valid av_set on BOOK_BLOCK, then there might exist another
     fence above, where we may choose to schedule an insn which is
     actually blocked from moving up with the bookkeeping we create here.  */
  if (AV_SET_VALID_P (sel_bb_head (book_block)))
    {
      old_av_set = av_set_copy (BB_AV_SET (book_block));
      update_data_sets (sel_bb_head (book_block));

      /* Traverse all the expressions in the old av_set and check whether
	 CUR_EXPR is in new AV_SET.  */
      FOR_EACH_EXPR (cur_expr, i, old_av_set)
        {
          expr_t new_expr = av_set_lookup (BB_AV_SET (book_block),
					   EXPR_VINSN (cur_expr));

          if (! new_expr
              /* In this case, we can just turn off the E_T_A bit, but we can't
                 represent this information with the current vector.  */
              || EXPR_TARGET_AVAILABLE (new_expr)
		 != EXPR_TARGET_AVAILABLE (cur_expr))
	    /* Unfortunately, the below code could be also fired up on
	       separable insns, e.g. when moving insns through the new
	       speculation check as in PR 53701.  */
            vinsn_vec_add (&vec_bookkeeping_blocked_vinsns, cur_expr);
        }

      av_set_clear (&old_av_set);
    }
}

/* The main effect of this function is that sparams->c_expr is merged
   with (or copied to) lparams->c_expr_merged.  If there's only one successor,
   we avoid merging anything by copying sparams->c_expr to lparams->c_expr_merged.
   lparams->c_expr_merged is copied back to sparams->c_expr after all
   successors has been traversed.  lparams->c_expr_local is an expr allocated
   on stack in the caller function, and is used if there is more than one
   successor.

   SUCC is one of the SUCCS_NORMAL successors of INSN,
   MOVEOP_DRV_CALL_RES is the result of call code_motion_path_driver on succ,
   LPARAMS and STATIC_PARAMS contain the parameters described above.  */
static void
move_op_merge_succs (insn_t insn ATTRIBUTE_UNUSED,
                     insn_t succ ATTRIBUTE_UNUSED,
		     int moveop_drv_call_res,
		     cmpd_local_params_p lparams, void *static_params)
{
  moveop_static_params_p sparams = (moveop_static_params_p) static_params;

  /* Nothing to do, if original expr wasn't found below.  */
  if (moveop_drv_call_res != 1)
    return;

  /* If this is a first successor.  */
  if (!lparams->c_expr_merged)
    {
      lparams->c_expr_merged = sparams->c_expr;
      sparams->c_expr = lparams->c_expr_local;
    }
  else
    {
      /* We must merge all found expressions to get reasonable
	 EXPR_SPEC_DONE_DS for the resulting insn.  If we don't
	 do so then we can first find the expr with epsilon
	 speculation success probability and only then with the
	 good probability.  As a result the insn will get epsilon
	 probability and will never be scheduled because of
	 weakness_cutoff in find_best_expr.

	 We call merge_expr_data here instead of merge_expr
	 because due to speculation C_EXPR and X may have the
	 same insns with different speculation types.  And as of
	 now such insns are considered non-equal.

	 However, EXPR_SCHED_TIMES is different -- we must get
	 SCHED_TIMES from a real insn, not a bookkeeping copy.
	 We force this here.  Instead, we may consider merging
	 SCHED_TIMES to the maximum instead of minimum in the
	 below function.  */
      int old_times = EXPR_SCHED_TIMES (lparams->c_expr_merged);

      merge_expr_data (lparams->c_expr_merged, sparams->c_expr, NULL);
      if (EXPR_SCHED_TIMES (sparams->c_expr) == 0)
	EXPR_SCHED_TIMES (lparams->c_expr_merged) = old_times;

      clear_expr (sparams->c_expr);
    }
}

/*  Add used regs for the successor SUCC into SPARAMS->USED_REGS.

   SUCC is one of the SUCCS_NORMAL successors of INSN,
   MOVEOP_DRV_CALL_RES is the result of call code_motion_path_driver on succ or 0,
     if SUCC is one of SUCCS_BACK or SUCCS_OUT.
   STATIC_PARAMS contain USED_REGS set.  */
static void
fur_merge_succs (insn_t insn ATTRIBUTE_UNUSED, insn_t succ,
		 int moveop_drv_call_res,
		 cmpd_local_params_p lparams ATTRIBUTE_UNUSED,
		 void *static_params)
{
  regset succ_live;
  fur_static_params_p sparams = (fur_static_params_p) static_params;

  /* Here we compute live regsets only for branches that do not lie
     on the code motion paths.  These branches correspond to value
     MOVEOP_DRV_CALL_RES==0 and include SUCCS_BACK and SUCCS_OUT, though
     for such branches code_motion_path_driver is not called.  */
  if (moveop_drv_call_res != 0)
    return;

  /* Mark all registers that do not meet the following condition:
     (3) not live on the other path of any conditional branch
     that is passed by the operation, in case original
     operations are not present on both paths of the
     conditional branch.  */
  succ_live = compute_live (succ);
  IOR_REG_SET (sparams->used_regs, succ_live);
}

/* This function is called after the last successor.  Copies LP->C_EXPR_MERGED
   into SP->CEXPR.  */
static void
move_op_after_merge_succs (cmpd_local_params_p lp, void *sparams)
{
  moveop_static_params_p sp = (moveop_static_params_p) sparams;

  sp->c_expr = lp->c_expr_merged;
}

/* Track bookkeeping copies created, insns scheduled, and blocks for
   rescheduling when INSN is found by move_op.  */
static void
track_scheduled_insns_and_blocks (rtx_insn *insn)
{
  /* Even if this insn can be a copy that will be removed during current move_op,
     we still need to count it as an originator.  */
  bitmap_set_bit (current_originators, INSN_UID (insn));

  if (!bitmap_clear_bit (current_copies, INSN_UID (insn)))
    {
      /* Note that original block needs to be rescheduled, as we pulled an
	 instruction out of it.  */
      if (INSN_SCHED_TIMES (insn) > 0)
	bitmap_set_bit (blocks_to_reschedule, BLOCK_FOR_INSN (insn)->index);
      else if (INSN_UID (insn) < first_emitted_uid && !DEBUG_INSN_P (insn))
	num_insns_scheduled++;
    }

  /* For instructions we must immediately remove insn from the
     stream, so subsequent update_data_sets () won't include this
     insn into av_set.
     For expr we must make insn look like "INSN_REG (insn) := c_expr".  */
  if (INSN_UID (insn) > max_uid_before_move_op)
    stat_bookkeeping_copies--;
}

/* Emit a register-register copy for INSN if needed.  Return true if
   emitted one.  PARAMS is the move_op static parameters.  */
static bool
maybe_emit_renaming_copy (rtx_insn *insn,
                          moveop_static_params_p params)
{
  bool insn_emitted  = false;
  rtx cur_reg;

  /* Bail out early when expression can not be renamed at all.  */
  if (!EXPR_SEPARABLE_P (params->c_expr))
    return false;

  cur_reg = expr_dest_reg (params->c_expr);
  gcc_assert (cur_reg && params->dest && REG_P (params->dest));

  /* If original operation has expr and the register chosen for
     that expr is not original operation's dest reg, substitute
     operation's right hand side with the register chosen.  */
  if (REGNO (params->dest) != REGNO (cur_reg))
    {
      insn_t reg_move_insn, reg_move_insn_rtx;

      reg_move_insn_rtx = create_insn_rtx_with_rhs (INSN_VINSN (insn),
                                                    params->dest);
      reg_move_insn = sel_gen_insn_from_rtx_after (reg_move_insn_rtx,
                                                   INSN_EXPR (insn),
                                                   INSN_SEQNO (insn),
                                                   insn);
      EXPR_SPEC_DONE_DS (INSN_EXPR (reg_move_insn)) = 0;
      replace_dest_with_reg_in_expr (params->c_expr, params->dest);

      insn_emitted = true;
      params->was_renamed = true;
    }

  return insn_emitted;
}

/* Emit a speculative check for INSN speculated as EXPR if needed.
   Return true if we've  emitted one.  PARAMS is the move_op static
   parameters.  */
static bool
maybe_emit_speculative_check (rtx_insn *insn, expr_t expr,
                              moveop_static_params_p params)
{
  bool insn_emitted = false;
  insn_t x;
  ds_t check_ds;

  check_ds = get_spec_check_type_for_insn (insn, expr);
  if (check_ds != 0)
    {
      /* A speculation check should be inserted.  */
      x = create_speculation_check (params->c_expr, check_ds, insn);
      insn_emitted = true;
    }
  else
    {
      EXPR_SPEC_DONE_DS (INSN_EXPR (insn)) = 0;
      x = insn;
    }

  gcc_assert (EXPR_SPEC_DONE_DS (INSN_EXPR (x)) == 0
              && EXPR_SPEC_TO_CHECK_DS (INSN_EXPR (x)) == 0);
  return insn_emitted;
}

/* Handle transformations that leave an insn in place of original
   insn such as renaming/speculation.  Return true if one of such
   transformations actually happened, and we have emitted this insn.  */
static bool
handle_emitting_transformations (rtx_insn *insn, expr_t expr,
                                 moveop_static_params_p params)
{
  bool insn_emitted = false;

  insn_emitted = maybe_emit_renaming_copy (insn, params);
  insn_emitted |= maybe_emit_speculative_check (insn, expr, params);

  return insn_emitted;
}

/* If INSN is the only insn in the basic block (not counting JUMP,
   which may be a jump to next insn, and DEBUG_INSNs), we want to
   leave a NOP there till the return to fill_insns.  */

static bool
need_nop_to_preserve_insn_bb (rtx_insn *insn)
{
  insn_t bb_head, bb_end, bb_next, in_next;
  basic_block bb = BLOCK_FOR_INSN (insn);

  bb_head = sel_bb_head (bb);
  bb_end = sel_bb_end (bb);

  if (bb_head == bb_end)
    return true;

  while (bb_head != bb_end && DEBUG_INSN_P (bb_head))
    bb_head = NEXT_INSN (bb_head);

  if (bb_head == bb_end)
    return true;

  while (bb_head != bb_end && DEBUG_INSN_P (bb_end))
    bb_end = PREV_INSN (bb_end);

  if (bb_head == bb_end)
    return true;

  bb_next = NEXT_INSN (bb_head);
  while (bb_next != bb_end && DEBUG_INSN_P (bb_next))
    bb_next = NEXT_INSN (bb_next);

  if (bb_next == bb_end && JUMP_P (bb_end))
    return true;

  in_next = NEXT_INSN (insn);
  while (DEBUG_INSN_P (in_next))
    in_next = NEXT_INSN (in_next);

  if (IN_CURRENT_FENCE_P (in_next))
    return true;

  return false;
}

/* Remove INSN from stream.  When ONLY_DISCONNECT is true, its data
   is not removed but reused when INSN is re-emitted.  */
static void
remove_insn_from_stream (rtx_insn *insn, bool only_disconnect)
{
  /* If there's only one insn in the BB, make sure that a nop is
     inserted into it, so the basic block won't disappear when we'll
     delete INSN below with sel_remove_insn. It should also survive
     till the return to fill_insns.  */
  if (need_nop_to_preserve_insn_bb (insn))
    {
      insn_t nop = get_nop_from_pool (insn);
      gcc_assert (INSN_NOP_P (nop));
      vec_temp_moveop_nops.safe_push (nop);
    }

  sel_remove_insn (insn, only_disconnect, false);
}

/* This function is called when original expr is found.
   INSN - current insn traversed, EXPR - the corresponding expr found.
   LPARAMS is the local parameters of code modion driver, STATIC_PARAMS
   is static parameters of move_op.  */
static void
move_op_orig_expr_found (insn_t insn, expr_t expr,
                         cmpd_local_params_p lparams ATTRIBUTE_UNUSED,
                         void *static_params)
{
  bool only_disconnect;
  moveop_static_params_p params = (moveop_static_params_p) static_params;

  copy_expr_onside (params->c_expr, INSN_EXPR (insn));
  track_scheduled_insns_and_blocks (insn);
  handle_emitting_transformations (insn, expr, params);
  only_disconnect = params->uid == INSN_UID (insn);

  /* Mark that we've disconnected an insn.  */
  if (only_disconnect)
    params->uid = -1;
  remove_insn_from_stream (insn, only_disconnect);
}

/* The function is called when original expr is found.
   INSN - current insn traversed, EXPR - the corresponding expr found,
   crosses_call and original_insns in STATIC_PARAMS are updated.  */
static void
fur_orig_expr_found (insn_t insn, expr_t expr ATTRIBUTE_UNUSED,
                     cmpd_local_params_p lparams ATTRIBUTE_UNUSED,
                     void *static_params)
{
  fur_static_params_p params = (fur_static_params_p) static_params;
  regset tmp;

  if (CALL_P (insn))
    params->crosses_call = true;

  def_list_add (params->original_insns, insn, params->crosses_call);

  /* Mark the registers that do not meet the following condition:
    (2) not among the live registers of the point
	immediately following the first original operation on
	a given downward path, except for the original target
	register of the operation.  */
  tmp = get_clear_regset_from_pool ();
  compute_live_below_insn (insn, tmp);
  AND_COMPL_REG_SET (tmp, INSN_REG_SETS (insn));
  AND_COMPL_REG_SET (tmp, INSN_REG_CLOBBERS (insn));
  IOR_REG_SET (params->used_regs, tmp);
  return_regset_to_pool (tmp);

  /* (*1) We need to add to USED_REGS registers that are read by
     INSN's lhs. This may lead to choosing wrong src register.
     E.g. (scheduling const expr enabled):

	429: ax=0x0	<- Can't use AX for this expr (0x0)
	433: dx=[bp-0x18]
	427: [ax+dx+0x1]=ax
	  REG_DEAD: ax
	168: di=dx
	  REG_DEAD: dx
     */
  /* FIXME: see comment above and enable MEM_P
     in vinsn_separable_p.  */
  gcc_assert (!VINSN_SEPARABLE_P (INSN_VINSN (insn))
	      || !MEM_P (INSN_LHS (insn)));
}

/* This function is called on the ascending pass, before returning from
   current basic block.  */
static void
move_op_at_first_insn (insn_t insn, cmpd_local_params_p lparams,
                       void *static_params)
{
  moveop_static_params_p sparams = (moveop_static_params_p) static_params;
  basic_block book_block = NULL;

  /* When we have removed the boundary insn for scheduling, which also
     happened to be the end insn in its bb, we don't need to update sets.  */
  if (!lparams->removed_last_insn
      && lparams->e1
      && sel_bb_head_p (insn))
    {
      /* We should generate bookkeeping code only if we are not at the
         top level of the move_op.  */
      if (sel_num_cfg_preds_gt_1 (insn))
        book_block = generate_bookkeeping_insn (sparams->c_expr,
                                                lparams->e1, lparams->e2);
      /* Update data sets for the current insn.  */
      update_data_sets (insn);
    }

  /* If bookkeeping code was inserted, we need to update av sets of basic
     block that received bookkeeping.  After generation of bookkeeping insn,
     bookkeeping block does not contain valid av set because we are not following
     the original algorithm in every detail with regards to e.g. renaming
     simple reg-reg copies.  Consider example:

     bookkeeping block           scheduling fence
     \            /
      \    join  /
       ----------
       |        |
       ----------
      /           \
     /             \
     r1 := r2          r1 := r3

     We try to schedule insn "r1 := r3" on the current
     scheduling fence.  Also, note that av set of bookkeeping block
     contain both insns "r1 := r2" and "r1 := r3".  When the insn has
     been scheduled, the CFG is as follows:

     r1 := r3               r1 := r3
     bookkeeping block           scheduling fence
     \            /
      \    join  /
       ----------
       |        |
       ----------
      /          \
     /            \
     r1 := r2

     Here, insn "r1 := r3" was scheduled at the current scheduling point
     and bookkeeping code was generated at the bookeeping block.  This
     way insn "r1 := r2" is no longer available as a whole instruction
     (but only as expr) ahead of insn "r1 := r3" in bookkeeping block.
     This situation is handled by calling update_data_sets.

     Since update_data_sets is called only on the bookkeeping block, and
     it also may have predecessors with av_sets, containing instructions that
     are no longer available, we save all such expressions that become
     unavailable during data sets update on the bookkeeping block in
     VEC_BOOKKEEPING_BLOCKED_VINSNS.  Later we avoid selecting such
     expressions for scheduling.  This allows us to avoid recomputation of
     av_sets outside the code motion path.  */

  if (book_block)
    update_and_record_unavailable_insns (book_block);

  /* If INSN was previously marked for deletion, it's time to do it.  */
  if (lparams->removed_last_insn)
    insn = PREV_INSN (insn);

  /* Do not tidy control flow at the topmost moveop, as we can erroneously
     kill a block with a single nop in which the insn should be emitted.  */
  if (lparams->e1)
    tidy_control_flow (BLOCK_FOR_INSN (insn), true);
}

/* This function is called on the ascending pass, before returning from the
   current basic block.  */
static void
fur_at_first_insn (insn_t insn,
                   cmpd_local_params_p lparams ATTRIBUTE_UNUSED,
                   void *static_params ATTRIBUTE_UNUSED)
{
  gcc_assert (!sel_bb_head_p (insn) || AV_SET_VALID_P (insn)
	      || AV_LEVEL (insn) == -1);
}

/* Called on the backward stage of recursion to call moveup_expr for insn
   and sparams->c_expr.  */
static void
move_op_ascend (insn_t insn, void *static_params)
{
  enum MOVEUP_EXPR_CODE res;
  moveop_static_params_p sparams = (moveop_static_params_p) static_params;

  if (! INSN_NOP_P (insn))
    {
      res = moveup_expr_cached (sparams->c_expr, insn, false);
      gcc_assert (res != MOVEUP_EXPR_NULL);
    }

  /* Update liveness for this insn as it was invalidated.  */
  update_liveness_on_insn (insn);
}

/* This function is called on enter to the basic block.
   Returns TRUE if this block already have been visited and
   code_motion_path_driver should return 1, FALSE otherwise.  */
static int
fur_on_enter (insn_t insn ATTRIBUTE_UNUSED, cmpd_local_params_p local_params,
	      void *static_params, bool visited_p)
{
  fur_static_params_p sparams = (fur_static_params_p) static_params;

  if (visited_p)
    {
      /* If we have found something below this block, there should be at
	 least one insn in ORIGINAL_INSNS.  */
      gcc_assert (*sparams->original_insns);

      /* Adjust CROSSES_CALL, since we may have come to this block along
	 different path.  */
      DEF_LIST_DEF (*sparams->original_insns)->crosses_call
	  |= sparams->crosses_call;
    }
  else
    local_params->old_original_insns = *sparams->original_insns;

  return 1;
}

/* Same as above but for move_op.   */
static int
move_op_on_enter (insn_t insn ATTRIBUTE_UNUSED,
                  cmpd_local_params_p local_params ATTRIBUTE_UNUSED,
                  void *static_params ATTRIBUTE_UNUSED, bool visited_p)
{
  if (visited_p)
    return -1;
  return 1;
}

/* This function is called while descending current basic block if current
   insn is not the original EXPR we're searching for.

   Return value: FALSE, if code_motion_path_driver should perform a local
			cleanup and return 0 itself;
		 TRUE, if code_motion_path_driver should continue.  */
static bool
move_op_orig_expr_not_found (insn_t insn, av_set_t orig_ops ATTRIBUTE_UNUSED,
			    void *static_params)
{
  moveop_static_params_p sparams = (moveop_static_params_p) static_params;

  sparams->failed_insn = insn;

  /* If we're scheduling separate expr, in order to generate correct code
     we need to stop the search at bookkeeping code generated with the
     same destination register or memory.  */
  if (lhs_of_insn_equals_to_dest_p (insn, sparams->dest))
    return false;
  return true;
}

/* This function is called while descending current basic block if current
   insn is not the original EXPR we're searching for.

   Return value: TRUE (code_motion_path_driver should continue).  */
static bool
fur_orig_expr_not_found (insn_t insn, av_set_t orig_ops, void *static_params)
{
  bool mutexed;
  expr_t r;
  av_set_iterator avi;
  fur_static_params_p sparams = (fur_static_params_p) static_params;

  if (CALL_P (insn))
    sparams->crosses_call = true;
  else if (DEBUG_INSN_P (insn))
    return true;

  /* If current insn we are looking at cannot be executed together
     with original insn, then we can skip it safely.

     Example: ORIG_OPS = { (p6) r14 = sign_extend (r15); }
	      INSN = (!p6) r14 = r14 + 1;

     Here we can schedule ORIG_OP with lhs = r14, though only
     looking at the set of used and set registers of INSN we must
     forbid it.  So, add set/used in INSN registers to the
     untouchable set only if there is an insn in ORIG_OPS that can
     affect INSN.  */
  mutexed = true;
  FOR_EACH_EXPR (r, avi, orig_ops)
    if (!sched_insns_conditions_mutex_p (insn, EXPR_INSN_RTX (r)))
      {
	mutexed = false;
	break;
      }

  /* Mark all registers that do not meet the following condition:
     (1) Not set or read on any path from xi to an instance of the
	 original operation.  */
  if (!mutexed)
    {
      IOR_REG_SET (sparams->used_regs, INSN_REG_SETS (insn));
      IOR_REG_SET (sparams->used_regs, INSN_REG_USES (insn));
      IOR_REG_SET (sparams->used_regs, INSN_REG_CLOBBERS (insn));
    }

  return true;
}

/* Hooks and data to perform move_op operations with code_motion_path_driver.  */
struct code_motion_path_driver_info_def move_op_hooks = {
  move_op_on_enter,
  move_op_orig_expr_found,
  move_op_orig_expr_not_found,
  move_op_merge_succs,
  move_op_after_merge_succs,
  move_op_ascend,
  move_op_at_first_insn,
  SUCCS_NORMAL,
  "move_op"
};

/* Hooks and data to perform find_used_regs operations
   with code_motion_path_driver.  */
struct code_motion_path_driver_info_def fur_hooks = {
  fur_on_enter,
  fur_orig_expr_found,
  fur_orig_expr_not_found,
  fur_merge_succs,
  NULL, /* fur_after_merge_succs */
  NULL, /* fur_ascend */
  fur_at_first_insn,
  SUCCS_ALL,
  "find_used_regs"
};

/* Traverse all successors of INSN.  For each successor that is SUCCS_NORMAL
   code_motion_path_driver is called recursively.  Original operation
   was found at least on one path that is starting with one of INSN's
   successors (this fact is asserted).  ORIG_OPS is expressions we're looking
   for, PATH is the path we've traversed, STATIC_PARAMS is the parameters
   of either move_op or find_used_regs depending on the caller.

   Return 0 if we haven't found expression, 1 if we found it, -1 if we don't
   know for sure at this point.  */
static int
code_motion_process_successors (insn_t insn, av_set_t orig_ops,
                                ilist_t path, void *static_params)
{
  int res = 0;
  succ_iterator succ_i;
  insn_t succ;
  basic_block bb;
  int old_index;
  unsigned old_succs;

  struct cmpd_local_params lparams;
  expr_def _x;

  lparams.c_expr_local = &_x;
  lparams.c_expr_merged = NULL;

  /* We need to process only NORMAL succs for move_op, and collect live
     registers from ALL branches (including those leading out of the
     region) for find_used_regs.

     In move_op, there can be a case when insn's bb number has changed
     due to created bookkeeping.  This happens very rare, as we need to
     move expression from the beginning to the end of the same block.
     Rescan successors in this case.  */

 rescan:
  bb = BLOCK_FOR_INSN (insn);
  old_index = bb->index;
  old_succs = EDGE_COUNT (bb->succs);

  FOR_EACH_SUCC_1 (succ, succ_i, insn, code_motion_path_driver_info->succ_flags)
    {
      int b;

      lparams.e1 = succ_i.e1;
      lparams.e2 = succ_i.e2;

      /* Go deep into recursion only for NORMAL edges (non-backedges within the
	 current region).  */
      if (succ_i.current_flags == SUCCS_NORMAL)
	b = code_motion_path_driver (succ, orig_ops, path, &lparams,
				     static_params);
      else
	b = 0;

      /* Merge c_expres found or unify live register sets from different
	 successors.  */
      code_motion_path_driver_info->merge_succs (insn, succ, b, &lparams,
						 static_params);
      if (b == 1)
        res = b;
      else if (b == -1 && res != 1)
        res = b;

      /* We have simplified the control flow below this point.  In this case,
         the iterator becomes invalid.  We need to try again.
	 If we have removed the insn itself, it could be only an
	 unconditional jump.  Thus, do not rescan but break immediately --
	 we have already visited the only successor block.  */
      if (!BLOCK_FOR_INSN (insn))
	{
	  if (sched_verbose >= 6)
	    sel_print ("Not doing rescan: already visited the only successor"
		       " of block %d\n", old_index);
	  break;
	}
      if (BLOCK_FOR_INSN (insn)->index != old_index
          || EDGE_COUNT (bb->succs) != old_succs)
        {
	  if (sched_verbose >= 6)
	    sel_print ("Rescan: CFG was simplified below insn %d, block %d\n",
		       INSN_UID (insn), BLOCK_FOR_INSN (insn)->index);
          insn = sel_bb_end (BLOCK_FOR_INSN (insn));
          goto rescan;
        }
    }

  /* Here, RES==1 if original expr was found at least for one of the
     successors.  After the loop, RES may happen to have zero value
     only if at some point the expr searched is present in av_set, but is
     not found below.  In most cases, this situation is an error.
     The exception is when the original operation is blocked by
     bookkeeping generated for another fence or for another path in current
     move_op.  */
  gcc_checking_assert (res == 1
		       || (res == 0
			    && av_set_could_be_blocked_by_bookkeeping_p (orig_ops, static_params))
		       || res == -1);

  /* Merge data, clean up, etc.  */
  if (res != -1 && code_motion_path_driver_info->after_merge_succs)
    code_motion_path_driver_info->after_merge_succs (&lparams, static_params);

  return res;
}


/* Perform a cleanup when the driver is about to terminate.  ORIG_OPS_P
   is the pointer to the av set with expressions we were looking for,
   PATH_P is the pointer to the traversed path.  */
static inline void
code_motion_path_driver_cleanup (av_set_t *orig_ops_p, ilist_t *path_p)
{
  ilist_remove (path_p);
  av_set_clear (orig_ops_p);
}

/* The driver function that implements move_op or find_used_regs
   functionality dependent whether code_motion_path_driver_INFO is set to
   &MOVE_OP_HOOKS or &FUR_HOOKS.  This function implements the common parts
   of code (CFG traversal etc) that are shared among both functions.  INSN
   is the insn we're starting the search from, ORIG_OPS are the expressions
   we're searching for, PATH is traversed path, LOCAL_PARAMS_IN are local
   parameters of the driver, and STATIC_PARAMS are static parameters of
   the caller.

   Returns whether original instructions were found.  Note that top-level
   code_motion_path_driver always returns true.  */
static int
code_motion_path_driver (insn_t insn, av_set_t orig_ops, ilist_t path,
			 cmpd_local_params_p local_params_in,
			 void *static_params)
{
  expr_t expr = NULL;
  basic_block bb = BLOCK_FOR_INSN (insn);
  insn_t first_insn, bb_tail, before_first;
  bool removed_last_insn = false;

  if (sched_verbose >= 6)
    {
      sel_print ("%s (", code_motion_path_driver_info->routine_name);
      dump_insn (insn);
      sel_print (",");
      dump_av_set (orig_ops);
      sel_print (")\n");
    }

  gcc_assert (orig_ops);

  /* If no original operations exist below this insn, return immediately.  */
  if (is_ineligible_successor (insn, path))
    {
      if (sched_verbose >= 6)
        sel_print ("Insn %d is ineligible successor\n", INSN_UID (insn));
      return false;
    }

  /* The block can have invalid av set, in which case it was created earlier
     during move_op.  Return immediately.  */
  if (sel_bb_head_p (insn))
    {
      if (! AV_SET_VALID_P (insn))
        {
          if (sched_verbose >= 6)
            sel_print ("Returned from block %d as it had invalid av set\n",
                       bb->index);
          return false;
        }

      if (bitmap_bit_p (code_motion_visited_blocks, bb->index))
        {
          /* We have already found an original operation on this branch, do not
             go any further and just return TRUE here.  If we don't stop here,
             function can have exponential behavior even on the small code
             with many different paths (e.g. with data speculation and
             recovery blocks).  */
          if (sched_verbose >= 6)
            sel_print ("Block %d already visited in this traversal\n", bb->index);
          if (code_motion_path_driver_info->on_enter)
            return code_motion_path_driver_info->on_enter (insn,
                                                           local_params_in,
                                                           static_params,
                                                           true);
        }
    }

  if (code_motion_path_driver_info->on_enter)
    code_motion_path_driver_info->on_enter (insn, local_params_in,
                                            static_params, false);
  orig_ops = av_set_copy (orig_ops);

  /* Filter the orig_ops set.  */
  if (AV_SET_VALID_P (insn))
    av_set_code_motion_filter (&orig_ops, AV_SET (insn));

  /* If no more original ops, return immediately.  */
  if (!orig_ops)
    {
      if (sched_verbose >= 6)
        sel_print ("No intersection with av set of block %d\n", bb->index);
      return false;
    }

  /* For non-speculative insns we have to leave only one form of the
     original operation, because if we don't, we may end up with
     different C_EXPRes and, consequently, with bookkeepings for different
     expression forms along the same code motion path.  That may lead to
     generation of incorrect code.  So for each code motion we stick to
     the single form of the instruction,  except for speculative insns
     which we need to keep in different forms with all speculation
     types.  */
  av_set_leave_one_nonspec (&orig_ops);

  /* It is not possible that all ORIG_OPS are filtered out.  */
  gcc_assert (orig_ops);

  /* It is enough to place only heads and tails of visited basic blocks into
     the PATH.  */
  ilist_add (&path, insn);
  first_insn = insn;
  bb_tail = sel_bb_end (bb);

  /* Descend the basic block in search of the original expr; this part
     corresponds to the part of the original move_op procedure executed
     before the recursive call.  */
  for (;;)
    {
      /* Look at the insn and decide if it could be an ancestor of currently
	 scheduling operation.  If it is so, then the insn "dest = op" could
	 either be replaced with "dest = reg", because REG now holds the result
	 of OP, or just removed, if we've scheduled the insn as a whole.

	 If this insn doesn't contain currently scheduling OP, then proceed
	 with searching and look at its successors.  Operations we're searching
	 for could have changed when moving up through this insn via
	 substituting.  In this case, perform unsubstitution on them first.

	 When traversing the DAG below this insn is finished, insert
	 bookkeeping code, if the insn is a joint point, and remove
	 leftovers.  */

      expr = av_set_lookup (orig_ops, INSN_VINSN (insn));
      if (expr)
	{
	  insn_t last_insn = PREV_INSN (insn);

	  /* We have found the original operation.   */
          if (sched_verbose >= 6)
            sel_print ("Found original operation at insn %d\n", INSN_UID (insn));

	  code_motion_path_driver_info->orig_expr_found
            (insn, expr, local_params_in, static_params);

	  /* Step back, so on the way back we'll start traversing from the
	     previous insn (or we'll see that it's bb_note and skip that
	     loop).  */
          if (insn == first_insn)
            {
              first_insn = NEXT_INSN (last_insn);
              removed_last_insn = sel_bb_end_p (last_insn);
            }
	  insn = last_insn;
	  break;
	}
      else
	{
	  /* We haven't found the original expr, continue descending the basic
	     block.  */
	  if (code_motion_path_driver_info->orig_expr_not_found
              (insn, orig_ops, static_params))
	    {
	      /* Av set ops could have been changed when moving through this
	         insn.  To find them below it, we have to un-substitute them.  */
	      undo_transformations (&orig_ops, insn);
	    }
	  else
	    {
	      /* Clean up and return, if the hook tells us to do so.  It may
		 happen if we've encountered the previously created
		 bookkeeping.  */
	      code_motion_path_driver_cleanup (&orig_ops, &path);
	      return -1;
	    }

	  gcc_assert (orig_ops);
        }

      /* Stop at insn if we got to the end of BB.  */
      if (insn == bb_tail)
	break;

      insn = NEXT_INSN (insn);
    }

  /* Here INSN either points to the insn before the original insn (may be
     bb_note, if original insn was a bb_head) or to the bb_end.  */
  if (!expr)
    {
      int res;
      rtx_insn *last_insn = PREV_INSN (insn);
      bool added_to_path;

      gcc_assert (insn == sel_bb_end (bb));

      /* Add bb tail to PATH (but it doesn't make any sense if it's a bb_head -
	 it's already in PATH then).  */
      if (insn != first_insn)
	{
	  ilist_add (&path, insn);
	  added_to_path = true;
	}
      else
        added_to_path = false;

      /* Process_successors should be able to find at least one
	 successor for which code_motion_path_driver returns TRUE.  */
      res = code_motion_process_successors (insn, orig_ops,
                                            path, static_params);

      /* Jump in the end of basic block could have been removed or replaced
         during code_motion_process_successors, so recompute insn as the
         last insn in bb.  */
      if (NEXT_INSN (last_insn) != insn)
        {
          insn = sel_bb_end (bb);
          first_insn = sel_bb_head (bb);
        }

      /* Remove bb tail from path.  */
      if (added_to_path)
	ilist_remove (&path);

      if (res != 1)
	{
	  /* This is the case when one of the original expr is no longer available
	     due to bookkeeping created on this branch with the same register.
	     In the original algorithm, which doesn't have update_data_sets call
	     on a bookkeeping block, it would simply result in returning
	     FALSE when we've encountered a previously generated bookkeeping
	     insn in moveop_orig_expr_not_found.  */
	  code_motion_path_driver_cleanup (&orig_ops, &path);
	  return res;
	}
    }

  /* Don't need it any more.  */
  av_set_clear (&orig_ops);

  /* Backward pass: now, when we have C_EXPR computed, we'll drag it to
     the beginning of the basic block.  */
  before_first = PREV_INSN (first_insn);
  while (insn != before_first)
    {
      if (code_motion_path_driver_info->ascend)
	code_motion_path_driver_info->ascend (insn, static_params);

      insn = PREV_INSN (insn);
    }

  /* Now we're at the bb head.  */
  insn = first_insn;
  ilist_remove (&path);
  local_params_in->removed_last_insn = removed_last_insn;
  code_motion_path_driver_info->at_first_insn (insn, local_params_in, static_params);

  /* This should be the very last operation as at bb head we could change
     the numbering by creating bookkeeping blocks.  */
  if (removed_last_insn)
    insn = PREV_INSN (insn);

  /* If we have simplified the control flow and removed the first jump insn,
     there's no point in marking this block in the visited blocks bitmap.  */
  if (BLOCK_FOR_INSN (insn))
    bitmap_set_bit (code_motion_visited_blocks, BLOCK_FOR_INSN (insn)->index);
  return true;
}

/* Move up the operations from ORIG_OPS set traversing the dag starting
   from INSN.  PATH represents the edges traversed so far.
   DEST is the register chosen for scheduling the current expr.  Insert
   bookkeeping code in the join points.  EXPR_VLIW is the chosen expression,
   C_EXPR is how it looks like at the given cfg point.
   Set *SHOULD_MOVE to indicate whether we have only disconnected
   one of the insns found.

   Returns whether original instructions were found, which is asserted
   to be true in the caller.  */
static bool
move_op (insn_t insn, av_set_t orig_ops, expr_t expr_vliw,
         rtx dest, expr_t c_expr, bool *should_move)
{
  struct moveop_static_params sparams;
  struct cmpd_local_params lparams;
  int res;

  /* Init params for code_motion_path_driver.  */
  sparams.dest = dest;
  sparams.c_expr = c_expr;
  sparams.uid = INSN_UID (EXPR_INSN_RTX (expr_vliw));
  sparams.failed_insn = NULL;
  sparams.was_renamed = false;
  lparams.e1 = NULL;

  /* We haven't visited any blocks yet.  */
  bitmap_clear (code_motion_visited_blocks);

  /* Set appropriate hooks and data.  */
  code_motion_path_driver_info = &move_op_hooks;
  res = code_motion_path_driver (insn, orig_ops, NULL, &lparams, &sparams);

  gcc_assert (res != -1);

  if (sparams.was_renamed)
    EXPR_WAS_RENAMED (expr_vliw) = true;

  *should_move = (sparams.uid == -1);

  return res;
}


/* Functions that work with regions.  */

/* Current number of seqno used in init_seqno and init_seqno_1.  */
static int cur_seqno;

/* A helper for init_seqno.  Traverse the region starting from BB and
   compute seqnos for visited insns, marking visited bbs in VISITED_BBS.
   Clear visited blocks from BLOCKS_TO_RESCHEDULE.  */
static void
init_seqno_1 (basic_block bb, sbitmap visited_bbs, bitmap blocks_to_reschedule)
{
  int bbi = BLOCK_TO_BB (bb->index);
  insn_t insn;
  insn_t succ_insn;
  succ_iterator si;

  rtx_note *note = bb_note (bb);
  bitmap_set_bit (visited_bbs, bbi);
  if (blocks_to_reschedule)
    bitmap_clear_bit (blocks_to_reschedule, bb->index);

  FOR_EACH_SUCC_1 (succ_insn, si, BB_END (bb),
		   SUCCS_NORMAL | SUCCS_SKIP_TO_LOOP_EXITS)
    {
      basic_block succ = BLOCK_FOR_INSN (succ_insn);
      int succ_bbi = BLOCK_TO_BB (succ->index);

      gcc_assert (in_current_region_p (succ));

      if (!bitmap_bit_p (visited_bbs, succ_bbi))
	{
	  gcc_assert (succ_bbi > bbi);

	  init_seqno_1 (succ, visited_bbs, blocks_to_reschedule);
	}
      else if (blocks_to_reschedule)
        bitmap_set_bit (forced_ebb_heads, succ->index);
    }

  for (insn = BB_END (bb); insn != note; insn = PREV_INSN (insn))
    INSN_SEQNO (insn) = cur_seqno--;
}

/* Initialize seqnos for the current region.  BLOCKS_TO_RESCHEDULE contains
   blocks on which we're rescheduling when pipelining, FROM is the block where
   traversing region begins (it may not be the head of the region when
   pipelining, but the head of the loop instead).

   Returns the maximal seqno found.  */
static int
init_seqno (bitmap blocks_to_reschedule, basic_block from)
{
  sbitmap visited_bbs;
  bitmap_iterator bi;
  unsigned bbi;

  visited_bbs = sbitmap_alloc (current_nr_blocks);

  if (blocks_to_reschedule)
    {
      bitmap_ones (visited_bbs);
      EXECUTE_IF_SET_IN_BITMAP (blocks_to_reschedule, 0, bbi, bi)
        {
	  gcc_assert (BLOCK_TO_BB (bbi) < current_nr_blocks);
          bitmap_clear_bit (visited_bbs, BLOCK_TO_BB (bbi));
	}
    }
  else
    {
      bitmap_clear (visited_bbs);
      from = EBB_FIRST_BB (0);
    }

  cur_seqno = sched_max_luid - 1;
  init_seqno_1 (from, visited_bbs, blocks_to_reschedule);

  /* cur_seqno may be positive if the number of instructions is less than
     sched_max_luid - 1 (when rescheduling or if some instructions have been
     removed by the call to purge_empty_blocks in sel_sched_region_1).  */
  gcc_assert (cur_seqno >= 0);

  sbitmap_free (visited_bbs);
  return sched_max_luid - 1;
}

/* Initialize scheduling parameters for current region.  */
static void
sel_setup_region_sched_flags (void)
{
  enable_schedule_as_rhs_p = 1;
  bookkeeping_p = 1;
  pipelining_p = (bookkeeping_p
                  && (flag_sel_sched_pipelining != 0)
		  && current_loop_nest != NULL
		  && loop_has_exit_edges (current_loop_nest));
  max_insns_to_rename = PARAM_VALUE (PARAM_SELSCHED_INSNS_TO_RENAME);
  max_ws = MAX_WS;
}

/* Return true if all basic blocks of current region are empty.  */
static bool
current_region_empty_p (void)
{
  int i;
  for (i = 0; i < current_nr_blocks; i++)
    if (! sel_bb_empty_p (BASIC_BLOCK_FOR_FN (cfun, BB_TO_BLOCK (i))))
      return false;

  return true;
}

/* Prepare and verify loop nest for pipelining.  */
static void
setup_current_loop_nest (int rgn, bb_vec_t *bbs)
{
  current_loop_nest = get_loop_nest_for_rgn (rgn);

  if (!current_loop_nest)
    return;

  /* If this loop has any saved loop preheaders from nested loops,
     add these basic blocks to the current region.  */
  sel_add_loop_preheaders (bbs);

  /* Check that we're starting with a valid information.  */
  gcc_assert (loop_latch_edge (current_loop_nest));
  gcc_assert (LOOP_MARKED_FOR_PIPELINING_P (current_loop_nest));
}

/* Compute instruction priorities for current region.  */
static void
sel_compute_priorities (int rgn)
{
  sched_rgn_compute_dependencies (rgn);

  /* Compute insn priorities in haifa style.  Then free haifa style
     dependencies that we've calculated for this.  */
  compute_priorities ();

  if (sched_verbose >= 5)
    debug_rgn_dependencies (0);

  free_rgn_deps ();
}

/* Init scheduling data for RGN.  Returns true when this region should not
   be scheduled.  */
static bool
sel_region_init (int rgn)
{
  int i;
  bb_vec_t bbs;

  rgn_setup_region (rgn);

  /* Even if sched_is_disabled_for_current_region_p() is true, we still
     do region initialization here so the region can be bundled correctly,
     but we'll skip the scheduling in sel_sched_region ().  */
  if (current_region_empty_p ())
    return true;

  bbs.create (current_nr_blocks);

  for (i = 0; i < current_nr_blocks; i++)
    bbs.quick_push (BASIC_BLOCK_FOR_FN (cfun, BB_TO_BLOCK (i)));

  sel_init_bbs (bbs);

  if (flag_sel_sched_pipelining)
    setup_current_loop_nest (rgn, &bbs);

  sel_setup_region_sched_flags ();

  /* Initialize luids and dependence analysis which both sel-sched and haifa
     need.  */
  sched_init_luids (bbs);
  sched_deps_init (false);

  /* Initialize haifa data.  */
  rgn_setup_sched_infos ();
  sel_set_sched_flags ();
  haifa_init_h_i_d (bbs);

  sel_compute_priorities (rgn);
  init_deps_global ();

  /* Main initialization.  */
  sel_setup_sched_infos ();
  sel_init_global_and_expr (bbs);

  bbs.release ();

  blocks_to_reschedule = BITMAP_ALLOC (NULL);

  /* Init correct liveness sets on each instruction of a single-block loop.
     This is the only situation when we can't update liveness when calling
     compute_live for the first insn of the loop.  */
  if (current_loop_nest)
    {
      int header =
	(sel_is_loop_preheader_p (BASIC_BLOCK_FOR_FN (cfun, BB_TO_BLOCK (0)))
	 ? 1
	 : 0);

      if (current_nr_blocks == header + 1)
        update_liveness_on_insn
          (sel_bb_head (BASIC_BLOCK_FOR_FN (cfun, BB_TO_BLOCK (header))));
    }

  /* Set hooks so that no newly generated insn will go out unnoticed.  */
  sel_register_cfg_hooks ();

  /* !!! We call target.sched.init () for the whole region, but we invoke
     targetm.sched.finish () for every ebb.  */
  if (targetm.sched.init)
    /* None of the arguments are actually used in any target.  */
    targetm.sched.init (sched_dump, sched_verbose, -1);

  first_emitted_uid = get_max_uid () + 1;
  preheader_removed = false;

  /* Reset register allocation ticks array.  */
  memset (reg_rename_tick, 0, sizeof reg_rename_tick);
  reg_rename_this_tick = 0;

  bitmap_initialize (forced_ebb_heads, 0);
  bitmap_clear (forced_ebb_heads);

  setup_nop_vinsn ();
  current_copies = BITMAP_ALLOC (NULL);
  current_originators = BITMAP_ALLOC (NULL);
  code_motion_visited_blocks = BITMAP_ALLOC (NULL);

  return false;
}

/* Simplify insns after the scheduling.  */
static void
simplify_changed_insns (void)
{
  int i;

  for (i = 0; i < current_nr_blocks; i++)
    {
      basic_block bb = BASIC_BLOCK_FOR_FN (cfun, BB_TO_BLOCK (i));
      rtx_insn *insn;

      FOR_BB_INSNS (bb, insn)
	if (INSN_P (insn))
	  {
	    expr_t expr = INSN_EXPR (insn);

	    if (EXPR_WAS_SUBSTITUTED (expr))
	      validate_simplify_insn (insn);
	  }
    }
}

/* Find boundaries of the EBB starting from basic block BB, marking blocks of
   this EBB in SCHEDULED_BLOCKS and appropriately filling in HEAD, TAIL,
   PREV_HEAD, and NEXT_TAIL fields of CURRENT_SCHED_INFO structure.  */
static void
find_ebb_boundaries (basic_block bb, bitmap scheduled_blocks)
{
  rtx_insn *head, *tail;
  basic_block bb1 = bb;
  if (sched_verbose >= 2)
    sel_print ("Finishing schedule in bbs: ");

  do
    {
      bitmap_set_bit (scheduled_blocks, BLOCK_TO_BB (bb1->index));

      if (sched_verbose >= 2)
	sel_print ("%d; ", bb1->index);
    }
  while (!bb_ends_ebb_p (bb1) && (bb1 = bb_next_bb (bb1)));

  if (sched_verbose >= 2)
    sel_print ("\n");

  get_ebb_head_tail (bb, bb1, &head, &tail);

  current_sched_info->head = head;
  current_sched_info->tail = tail;
  current_sched_info->prev_head = PREV_INSN (head);
  current_sched_info->next_tail = NEXT_INSN (tail);
}

/* Regenerate INSN_SCHED_CYCLEs for insns of current EBB.  */
static void
reset_sched_cycles_in_current_ebb (void)
{
  int last_clock = 0;
  int haifa_last_clock = -1;
  int haifa_clock = 0;
  int issued_insns = 0;
  insn_t insn;

  if (targetm.sched.init)
    {
      /* None of the arguments are actually used in any target.
	 NB: We should have md_reset () hook for cases like this.  */
      targetm.sched.init (sched_dump, sched_verbose, -1);
    }

  state_reset (curr_state);
  advance_state (curr_state);

  for (insn = current_sched_info->head;
       insn != current_sched_info->next_tail;
       insn = NEXT_INSN (insn))
    {
      int cost, haifa_cost;
      int sort_p;
      bool asm_p, real_insn, after_stall, all_issued;
      int clock;

      if (!INSN_P (insn))
	continue;

      asm_p = false;
      real_insn = recog_memoized (insn) >= 0;
      clock = INSN_SCHED_CYCLE (insn);

      cost = clock - last_clock;

      /* Initialize HAIFA_COST.  */
      if (! real_insn)
	{
	  asm_p = INSN_ASM_P (insn);

	  if (asm_p)
	    /* This is asm insn which *had* to be scheduled first
	       on the cycle.  */
	    haifa_cost = 1;
	  else
	    /* This is a use/clobber insn.  It should not change
	       cost.  */
	    haifa_cost = 0;
	}
      else
        haifa_cost = estimate_insn_cost (insn, curr_state);

      /* Stall for whatever cycles we've stalled before.  */
      after_stall = 0;
      if (INSN_AFTER_STALL_P (insn) && cost > haifa_cost)
        {
          haifa_cost = cost;
          after_stall = 1;
        }
      all_issued = issued_insns == issue_rate;
      if (haifa_cost == 0 && all_issued)
	haifa_cost = 1;
      if (haifa_cost > 0)
	{
	  int i = 0;

	  while (haifa_cost--)
	    {
	      advance_state (curr_state);
	      issued_insns = 0;
              i++;

	      if (sched_verbose >= 2)
                {
                  sel_print ("advance_state (state_transition)\n");
                  debug_state (curr_state);
                }

              /* The DFA may report that e.g. insn requires 2 cycles to be
                 issued, but on the next cycle it says that insn is ready
                 to go.  Check this here.  */
              if (!after_stall
                  && real_insn
                  && haifa_cost > 0
                  && estimate_insn_cost (insn, curr_state) == 0)
                break;

              /* When the data dependency stall is longer than the DFA stall,
                 and when we have issued exactly issue_rate insns and stalled,
                 it could be that after this longer stall the insn will again
                 become unavailable  to the DFA restrictions.  Looks strange
                 but happens e.g. on x86-64.  So recheck DFA on the last
                 iteration.  */
              if ((after_stall || all_issued)
                  && real_insn
                  && haifa_cost == 0)
                haifa_cost = estimate_insn_cost (insn, curr_state);
            }

	  haifa_clock += i;
          if (sched_verbose >= 2)
            sel_print ("haifa clock: %d\n", haifa_clock);
	}
      else
	gcc_assert (haifa_cost == 0);

      if (sched_verbose >= 2)
	sel_print ("Haifa cost for insn %d: %d\n", INSN_UID (insn), haifa_cost);

      if (targetm.sched.dfa_new_cycle)
	while (targetm.sched.dfa_new_cycle (sched_dump, sched_verbose, insn,
					    haifa_last_clock, haifa_clock,
					    &sort_p))
	  {
	    advance_state (curr_state);
	    issued_insns = 0;
	    haifa_clock++;
	    if (sched_verbose >= 2)
              {
                sel_print ("advance_state (dfa_new_cycle)\n");
                debug_state (curr_state);
		sel_print ("haifa clock: %d\n", haifa_clock + 1);
              }
          }

      if (real_insn)
	{
	  static state_t temp = NULL;

	  if (!temp)
	    temp = xmalloc (dfa_state_size);
	  memcpy (temp, curr_state, dfa_state_size);

	  cost = state_transition (curr_state, insn);
	  if (memcmp (temp, curr_state, dfa_state_size))
	    issued_insns++;

          if (sched_verbose >= 2)
	    {
	      sel_print ("scheduled insn %d, clock %d\n", INSN_UID (insn),
			 haifa_clock + 1);
              debug_state (curr_state);
	    }
	  gcc_assert (cost < 0);
	}

      if (targetm.sched.variable_issue)
	targetm.sched.variable_issue (sched_dump, sched_verbose, insn, 0);

      INSN_SCHED_CYCLE (insn) = haifa_clock;

      last_clock = clock;
      haifa_last_clock = haifa_clock;
    }
}

/* Put TImode markers on insns starting a new issue group.  */
static void
put_TImodes (void)
{
  int last_clock = -1;
  insn_t insn;

  for (insn = current_sched_info->head; insn != current_sched_info->next_tail;
       insn = NEXT_INSN (insn))
    {
      int cost, clock;

      if (!INSN_P (insn))
	continue;

      clock = INSN_SCHED_CYCLE (insn);
      cost = (last_clock == -1) ? 1 : clock - last_clock;

      gcc_assert (cost >= 0);

      if (issue_rate > 1
	  && GET_CODE (PATTERN (insn)) != USE
	  && GET_CODE (PATTERN (insn)) != CLOBBER)
	{
	  if (reload_completed && cost > 0)
	    PUT_MODE (insn, TImode);

	  last_clock = clock;
	}

      if (sched_verbose >= 2)
	sel_print ("Cost for insn %d is %d\n", INSN_UID (insn), cost);
    }
}

/* Perform MD_FINISH on EBBs comprising current region.  When
   RESET_SCHED_CYCLES_P is true, run a pass emulating the scheduler
   to produce correct sched cycles on insns.  */
static void
sel_region_target_finish (bool reset_sched_cycles_p)
{
  int i;
  bitmap scheduled_blocks = BITMAP_ALLOC (NULL);

  for (i = 0; i < current_nr_blocks; i++)
    {
      if (bitmap_bit_p (scheduled_blocks, i))
	continue;

      /* While pipelining outer loops, skip bundling for loop
	 preheaders.  Those will be rescheduled in the outer loop.  */
      if (sel_is_loop_preheader_p (EBB_FIRST_BB (i)))
	continue;

      find_ebb_boundaries (EBB_FIRST_BB (i), scheduled_blocks);

      if (no_real_insns_p (current_sched_info->head, current_sched_info->tail))
	continue;

      if (reset_sched_cycles_p)
	reset_sched_cycles_in_current_ebb ();

      if (targetm.sched.init)
	targetm.sched.init (sched_dump, sched_verbose, -1);

      put_TImodes ();

      if (targetm.sched.finish)
	{
	  targetm.sched.finish (sched_dump, sched_verbose);

	  /* Extend luids so that insns generated by the target will
	     get zero luid.  */
	  sched_extend_luids ();
	}
    }

  BITMAP_FREE (scheduled_blocks);
}

/* Free the scheduling data for the current region.  When RESET_SCHED_CYCLES_P
   is true, make an additional pass emulating scheduler to get correct insn
   cycles for md_finish calls.  */
static void
sel_region_finish (bool reset_sched_cycles_p)
{
  simplify_changed_insns ();
  sched_finish_ready_list ();
  free_nop_pool ();

  /* Free the vectors.  */
  vec_av_set.release ();
  BITMAP_FREE (current_copies);
  BITMAP_FREE (current_originators);
  BITMAP_FREE (code_motion_visited_blocks);
  vinsn_vec_free (vec_bookkeeping_blocked_vinsns);
  vinsn_vec_free (vec_target_unavailable_vinsns);

  /* If LV_SET of the region head should be updated, do it now because
     there will be no other chance.  */
  {
    succ_iterator si;
    insn_t insn;

    FOR_EACH_SUCC_1 (insn, si, bb_note (EBB_FIRST_BB (0)),
                     SUCCS_NORMAL | SUCCS_SKIP_TO_LOOP_EXITS)
      {
	basic_block bb = BLOCK_FOR_INSN (insn);

	if (!BB_LV_SET_VALID_P (bb))
	  compute_live (insn);
      }
  }

  /* Emulate the Haifa scheduler for bundling.  */
  if (reload_completed)
    sel_region_target_finish (reset_sched_cycles_p);

  sel_finish_global_and_expr ();

  bitmap_clear (forced_ebb_heads);

  free_nop_vinsn ();

  finish_deps_global ();
  sched_finish_luids ();
  h_d_i_d.release ();

  sel_finish_bbs ();
  BITMAP_FREE (blocks_to_reschedule);

  sel_unregister_cfg_hooks ();

  max_issue_size = 0;
}


/* Functions that implement the scheduler driver.  */

/* Schedule a parallel instruction group on each of FENCES.  MAX_SEQNO
   is the current maximum seqno.  SCHEDULED_INSNS_TAILPP is the list
   of insns scheduled -- these would be postprocessed later.  */
static void
schedule_on_fences (flist_t fences, int max_seqno,
                    ilist_t **scheduled_insns_tailpp)
{
  flist_t old_fences = fences;

  if (sched_verbose >= 1)
    {
      sel_print ("\nScheduling on fences: ");
      dump_flist (fences);
      sel_print ("\n");
    }

  scheduled_something_on_previous_fence = false;
  for (; fences; fences = FLIST_NEXT (fences))
    {
      fence_t fence = NULL;
      int seqno = 0;
      flist_t fences2;
      bool first_p = true;

      /* Choose the next fence group to schedule.
         The fact that insn can be scheduled only once
         on the cycle is guaranteed by two properties:
         1. seqnos of parallel groups decrease with each iteration.
         2. If is_ineligible_successor () sees the larger seqno, it
         checks if candidate insn is_in_current_fence_p ().  */
      for (fences2 = old_fences; fences2; fences2 = FLIST_NEXT (fences2))
        {
          fence_t f = FLIST_FENCE (fences2);

          if (!FENCE_PROCESSED_P (f))
            {
              int i = INSN_SEQNO (FENCE_INSN (f));

              if (first_p || i > seqno)
                {
                  seqno = i;
                  fence = f;
                  first_p = false;
                }
              else
                /* ??? Seqnos of different groups should be different.  */
                gcc_assert (1 || i != seqno);
            }
        }

      gcc_assert (fence);

      /* As FENCE is nonnull, SEQNO is initialized.  */
      seqno -= max_seqno + 1;
      fill_insns (fence, seqno, scheduled_insns_tailpp);
      FENCE_PROCESSED_P (fence) = true;
    }

  /* All av_sets are invalidated by GLOBAL_LEVEL increase, thus we
     don't need to keep bookkeeping-invalidated and target-unavailable
     vinsns any more.  */
  vinsn_vec_clear (&vec_bookkeeping_blocked_vinsns);
  vinsn_vec_clear (&vec_target_unavailable_vinsns);
}

/* Calculate MIN_SEQNO and MAX_SEQNO.  */
static void
find_min_max_seqno (flist_t fences, int *min_seqno, int *max_seqno)
{
  *min_seqno = *max_seqno = INSN_SEQNO (FENCE_INSN (FLIST_FENCE (fences)));

  /* The first element is already processed.  */
  while ((fences = FLIST_NEXT (fences)))
    {
      int seqno = INSN_SEQNO (FENCE_INSN (FLIST_FENCE (fences)));

      if (*min_seqno > seqno)
        *min_seqno = seqno;
      else if (*max_seqno < seqno)
        *max_seqno = seqno;
    }
}

/* Calculate new fences from FENCES.  Write the current time to PTIME.  */
static flist_t
calculate_new_fences (flist_t fences, int orig_max_seqno, int *ptime)
{
  flist_t old_fences = fences;
  struct flist_tail_def _new_fences, *new_fences = &_new_fences;
  int max_time = 0;

  flist_tail_init (new_fences);
  for (; fences; fences = FLIST_NEXT (fences))
    {
      fence_t fence = FLIST_FENCE (fences);
      insn_t insn;

      if (!FENCE_BNDS (fence))
        {
          /* This fence doesn't have any successors.  */
          if (!FENCE_SCHEDULED_P (fence))
            {
              /* Nothing was scheduled on this fence.  */
              int seqno;

              insn = FENCE_INSN (fence);
              seqno = INSN_SEQNO (insn);
              gcc_assert (seqno > 0 && seqno <= orig_max_seqno);

              if (sched_verbose >= 1)
                sel_print ("Fence %d[%d] has not changed\n",
                           INSN_UID (insn),
                           BLOCK_NUM (insn));
              move_fence_to_fences (fences, new_fences);
            }
        }
      else
        extract_new_fences_from (fences, new_fences, orig_max_seqno);
      max_time = MAX (max_time, FENCE_CYCLE (fence));
    }

  flist_clear (&old_fences);
  *ptime = max_time;
  return FLIST_TAIL_HEAD (new_fences);
}

/* Update seqnos of insns given by PSCHEDULED_INSNS.  MIN_SEQNO and MAX_SEQNO
   are the miminum and maximum seqnos of the group, HIGHEST_SEQNO_IN_USE is
   the highest seqno used in a region.  Return the updated highest seqno.  */
static int
update_seqnos_and_stage (int min_seqno, int max_seqno,
                         int highest_seqno_in_use,
                         ilist_t *pscheduled_insns)
{
  int new_hs;
  ilist_iterator ii;
  insn_t insn;

  /* Actually, new_hs is the seqno of the instruction, that was
     scheduled first (i.e. it is the first one in SCHEDULED_INSNS).  */
  if (*pscheduled_insns)
    {
      new_hs = (INSN_SEQNO (ILIST_INSN (*pscheduled_insns))
                + highest_seqno_in_use + max_seqno - min_seqno + 2);
      gcc_assert (new_hs > highest_seqno_in_use);
    }
  else
    new_hs = highest_seqno_in_use;

  FOR_EACH_INSN (insn, ii, *pscheduled_insns)
    {
      gcc_assert (INSN_SEQNO (insn) < 0);
      INSN_SEQNO (insn) += highest_seqno_in_use + max_seqno - min_seqno + 2;
      gcc_assert (INSN_SEQNO (insn) <= new_hs);

      /* When not pipelining, purge unneeded insn info on the scheduled insns.
         For example, having reg_last array of INSN_DEPS_CONTEXT in memory may
         require > 1GB of memory e.g. on limit-fnargs.c.  */
      if (! pipelining_p)
        free_data_for_scheduled_insn (insn);
    }

  ilist_clear (pscheduled_insns);
  global_level++;

  return new_hs;
}

/* The main driver for scheduling a region.  This function is responsible
   for correct propagation of fences (i.e. scheduling points) and creating
   a group of parallel insns at each of them.  It also supports
   pipelining.  ORIG_MAX_SEQNO is the maximal seqno before this pass
   of scheduling.  */
static void
sel_sched_region_2 (int orig_max_seqno)
{
  int highest_seqno_in_use = orig_max_seqno;
  int max_time = 0;

  stat_bookkeeping_copies = 0;
  stat_insns_needed_bookkeeping = 0;
  stat_renamed_scheduled = 0;
  stat_substitutions_total = 0;
  num_insns_scheduled = 0;

  while (fences)
    {
      int min_seqno, max_seqno;
      ilist_t scheduled_insns = NULL;
      ilist_t *scheduled_insns_tailp = &scheduled_insns;

      find_min_max_seqno (fences, &min_seqno, &max_seqno);
      schedule_on_fences (fences, max_seqno, &scheduled_insns_tailp);
      fences = calculate_new_fences (fences, orig_max_seqno, &max_time);
      highest_seqno_in_use = update_seqnos_and_stage (min_seqno, max_seqno,
                                                      highest_seqno_in_use,
                                                      &scheduled_insns);
    }

  if (sched_verbose >= 1)
    {
      sel_print ("Total scheduling time: %d cycles\n", max_time);
      sel_print ("Scheduled %d bookkeeping copies, %d insns needed "
		 "bookkeeping, %d insns renamed, %d insns substituted\n",
		 stat_bookkeeping_copies,
		 stat_insns_needed_bookkeeping,
		 stat_renamed_scheduled,
		 stat_substitutions_total);
    }
}

/* Schedule a region.  When pipelining, search for possibly never scheduled
   bookkeeping code and schedule it.  Reschedule pipelined code without
   pipelining after.  */
static void
sel_sched_region_1 (void)
{
  int orig_max_seqno;

  /* Remove empty blocks that might be in the region from the beginning.  */
  purge_empty_blocks ();

  orig_max_seqno = init_seqno (NULL, NULL);
  gcc_assert (orig_max_seqno >= 1);

  /* When pipelining outer loops, create fences on the loop header,
     not preheader.  */
  fences = NULL;
  if (current_loop_nest)
    init_fences (BB_END (EBB_FIRST_BB (0)));
  else
    init_fences (bb_note (EBB_FIRST_BB (0)));
  global_level = 1;

  sel_sched_region_2 (orig_max_seqno);

  gcc_assert (fences == NULL);

  if (pipelining_p)
    {
      int i;
      basic_block bb;
      struct flist_tail_def _new_fences;
      flist_tail_t new_fences = &_new_fences;
      bool do_p = true;

      pipelining_p = false;
      max_ws = MIN (max_ws, issue_rate * 3 / 2);
      bookkeeping_p = false;
      enable_schedule_as_rhs_p = false;

      /* Schedule newly created code, that has not been scheduled yet.  */
      do_p = true;

      while (do_p)
        {
          do_p = false;

          for (i = 0; i < current_nr_blocks; i++)
            {
              basic_block bb = EBB_FIRST_BB (i);

              if (bitmap_bit_p (blocks_to_reschedule, bb->index))
                {
                  if (! bb_ends_ebb_p (bb))
                    bitmap_set_bit (blocks_to_reschedule, bb_next_bb (bb)->index);
                  if (sel_bb_empty_p (bb))
                    {
                      bitmap_clear_bit (blocks_to_reschedule, bb->index);
                      continue;
                    }
                  clear_outdated_rtx_info (bb);
                  if (sel_insn_is_speculation_check (BB_END (bb))
                      && JUMP_P (BB_END (bb)))
                    bitmap_set_bit (blocks_to_reschedule,
                                    BRANCH_EDGE (bb)->dest->index);
                }
              else if (! sel_bb_empty_p (bb)
                       && INSN_SCHED_TIMES (sel_bb_head (bb)) <= 0)
                bitmap_set_bit (blocks_to_reschedule, bb->index);
            }

          for (i = 0; i < current_nr_blocks; i++)
            {
              bb = EBB_FIRST_BB (i);

              /* While pipelining outer loops, skip bundling for loop
                 preheaders.  Those will be rescheduled in the outer
                 loop.  */
              if (sel_is_loop_preheader_p (bb))
                {
                  clear_outdated_rtx_info (bb);
                  continue;
                }

              if (bitmap_bit_p (blocks_to_reschedule, bb->index))
                {
                  flist_tail_init (new_fences);

                  orig_max_seqno = init_seqno (blocks_to_reschedule, bb);

                  /* Mark BB as head of the new ebb.  */
                  bitmap_set_bit (forced_ebb_heads, bb->index);

                  gcc_assert (fences == NULL);

                  init_fences (bb_note (bb));

                  sel_sched_region_2 (orig_max_seqno);

                  do_p = true;
                  break;
                }
            }
        }
    }
}

/* Schedule the RGN region.  */
void
sel_sched_region (int rgn)
{
  bool schedule_p;
  bool reset_sched_cycles_p;

  if (sel_region_init (rgn))
    return;

  if (sched_verbose >= 1)
    sel_print ("Scheduling region %d\n", rgn);

  schedule_p = (!sched_is_disabled_for_current_region_p ()
                && dbg_cnt (sel_sched_region_cnt));
  reset_sched_cycles_p = pipelining_p;
  if (schedule_p)
    sel_sched_region_1 ();
  else
    /* Force initialization of INSN_SCHED_CYCLEs for correct bundling.  */
    reset_sched_cycles_p = true;

  sel_region_finish (reset_sched_cycles_p);
}

/* Perform global init for the scheduler.  */
static void
sel_global_init (void)
{
  calculate_dominance_info (CDI_DOMINATORS);
  alloc_sched_pools ();

  /* Setup the infos for sched_init.  */
  sel_setup_sched_infos ();
  setup_sched_dump ();

  sched_rgn_init (false);
  sched_init ();

  sched_init_bbs ();
  /* Reset AFTER_RECOVERY if it has been set by the 1st scheduler pass.  */
  after_recovery = 0;
  can_issue_more = issue_rate;

  sched_extend_target ();
  sched_deps_init (true);
  setup_nop_and_exit_insns ();
  sel_extend_global_bb_info ();
  init_lv_sets ();
  init_hard_regs_data ();
}

/* Free the global data of the scheduler.  */
static void
sel_global_finish (void)
{
  free_bb_note_pool ();
  free_lv_sets ();
  sel_finish_global_bb_info ();

  free_regset_pool ();
  free_nop_and_exit_insns ();

  sched_rgn_finish ();
  sched_deps_finish ();
  sched_finish ();

  if (current_loops)
    sel_finish_pipelining ();

  free_sched_pools ();
  free_dominance_info (CDI_DOMINATORS);
}

/* Return true when we need to skip selective scheduling.  Used for debugging.  */
bool
maybe_skip_selective_scheduling (void)
{
  return ! dbg_cnt (sel_sched_cnt);
}

/* The entry point.  */
void
run_selective_scheduling (void)
{
  int rgn;

  if (n_basic_blocks_for_fn (cfun) == NUM_FIXED_BLOCKS)
    return;

  sel_global_init ();

  for (rgn = 0; rgn < nr_regions; rgn++)
    sel_sched_region (rgn);

  sel_global_finish ();
}

#endif
