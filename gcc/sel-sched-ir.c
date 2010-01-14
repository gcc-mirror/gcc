/* Instruction scheduling pass.  Selective scheduler and pipeliner.
   Copyright (C) 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc.

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
#include "params.h"
#include "target.h"
#include "timevar.h"
#include "tree-pass.h"
#include "sched-int.h"
#include "ggc.h"
#include "tree.h"
#include "vec.h"
#include "langhooks.h"
#include "rtlhooks-def.h"

#ifdef INSN_SCHEDULING
#include "sel-sched-ir.h"
/* We don't have to use it except for sel_print_insn.  */
#include "sel-sched-dump.h"

/* A vector holding bb info for whole scheduling pass.  */
VEC(sel_global_bb_info_def, heap) *sel_global_bb_info = NULL;

/* A vector holding bb info.  */
VEC(sel_region_bb_info_def, heap) *sel_region_bb_info = NULL;

/* A pool for allocating all lists.  */
alloc_pool sched_lists_pool;

/* This contains information about successors for compute_av_set.  */
struct succs_info current_succs;

/* Data structure to describe interaction with the generic scheduler utils.  */
static struct common_sched_info_def sel_common_sched_info;

/* The loop nest being pipelined.  */
struct loop *current_loop_nest;

/* LOOP_NESTS is a vector containing the corresponding loop nest for
   each region.  */
static VEC(loop_p, heap) *loop_nests = NULL;

/* Saves blocks already in loop regions, indexed by bb->index.  */
static sbitmap bbs_in_loop_rgns = NULL;

/* CFG hooks that are saved before changing create_basic_block hook.  */
static struct cfg_hooks orig_cfg_hooks;


/* Array containing reverse topological index of function basic blocks,
   indexed by BB->INDEX.  */
static int *rev_top_order_index = NULL;

/* Length of the above array.  */
static int rev_top_order_index_len = -1;

/* A regset pool structure.  */
static struct
{
  /* The stack to which regsets are returned.  */
  regset *v;

  /* Its pointer.  */
  int n;

  /* Its size.  */
  int s;

  /* In VV we save all generated regsets so that, when destructing the
     pool, we can compare it with V and check that every regset was returned
     back to pool.  */
  regset *vv;

  /* The pointer of VV stack.  */
  int nn;

  /* Its size.  */
  int ss;

  /* The difference between allocated and returned regsets.  */
  int diff;
} regset_pool = { NULL, 0, 0, NULL, 0, 0, 0 };

/* This represents the nop pool.  */
static struct
{
  /* The vector which holds previously emitted nops.  */
  insn_t *v;

  /* Its pointer.  */
  int n;

  /* Its size.  */
  int s;
} nop_pool = { NULL, 0, 0 };

/* The pool for basic block notes.  */
static rtx_vec_t bb_note_pool;

/* A NOP pattern used to emit placeholder insns.  */
rtx nop_pattern = NULL_RTX;
/* A special instruction that resides in EXIT_BLOCK.
   EXIT_INSN is successor of the insns that lead to EXIT_BLOCK.  */
rtx exit_insn = NULL_RTX;

/* TRUE if while scheduling current region, which is loop, its preheader
   was removed.  */
bool preheader_removed = false;


/* Forward static declarations.  */
static void fence_clear (fence_t);

static void deps_init_id (idata_t, insn_t, bool);
static void init_id_from_df (idata_t, insn_t, bool);
static expr_t set_insn_init (expr_t, vinsn_t, int);

static void cfg_preds (basic_block, insn_t **, int *);
static void prepare_insn_expr (insn_t, int);
static void free_history_vect (VEC (expr_history_def, heap) **);

static void move_bb_info (basic_block, basic_block);
static void remove_empty_bb (basic_block, bool);
static void sel_remove_loop_preheader (void);

static bool insn_is_the_only_one_in_bb_p (insn_t);
static void create_initial_data_sets (basic_block);

static void free_av_set (basic_block);
static void invalidate_av_set (basic_block);
static void extend_insn_data (void);
static void sel_init_new_insn (insn_t, int);
static void finish_insns (void);

/* Various list functions.  */

/* Copy an instruction list L.  */
ilist_t
ilist_copy (ilist_t l)
{
  ilist_t head = NULL, *tailp = &head;

  while (l)
    {
      ilist_add (tailp, ILIST_INSN (l));
      tailp = &ILIST_NEXT (*tailp);
      l = ILIST_NEXT (l);
    }

  return head;
}

/* Invert an instruction list L.  */
ilist_t
ilist_invert (ilist_t l)
{
  ilist_t res = NULL;

  while (l)
    {
      ilist_add (&res, ILIST_INSN (l));
      l = ILIST_NEXT (l);
    }

  return res;
}

/* Add a new boundary to the LP list with parameters TO, PTR, and DC.  */
void
blist_add (blist_t *lp, insn_t to, ilist_t ptr, deps_t dc)
{
  bnd_t bnd;

  _list_add (lp);
  bnd = BLIST_BND (*lp);

  BND_TO (bnd) = to;
  BND_PTR (bnd) = ptr;
  BND_AV (bnd) = NULL;
  BND_AV1 (bnd) = NULL;
  BND_DC (bnd) = dc;
}

/* Remove the list note pointed to by LP.  */
void
blist_remove (blist_t *lp)
{
  bnd_t b = BLIST_BND (*lp);

  av_set_clear (&BND_AV (b));
  av_set_clear (&BND_AV1 (b));
  ilist_clear (&BND_PTR (b));

  _list_remove (lp);
}

/* Init a fence tail L.  */
void
flist_tail_init (flist_tail_t l)
{
  FLIST_TAIL_HEAD (l) = NULL;
  FLIST_TAIL_TAILP (l) = &FLIST_TAIL_HEAD (l);
}

/* Try to find fence corresponding to INSN in L.  */
fence_t
flist_lookup (flist_t l, insn_t insn)
{
  while (l)
    {
      if (FENCE_INSN (FLIST_FENCE (l)) == insn)
	return FLIST_FENCE (l);

      l = FLIST_NEXT (l);
    }

  return NULL;
}

/* Init the fields of F before running fill_insns.  */
static void
init_fence_for_scheduling (fence_t f)
{
  FENCE_BNDS (f) = NULL;
  FENCE_PROCESSED_P (f) = false;
  FENCE_SCHEDULED_P (f) = false;
}

/* Add new fence consisting of INSN and STATE to the list pointed to by LP.  */
static void
flist_add (flist_t *lp, insn_t insn, state_t state, deps_t dc, void *tc,
           insn_t last_scheduled_insn, VEC(rtx,gc) *executing_insns,
           int *ready_ticks, int ready_ticks_size, insn_t sched_next,
           int cycle, int cycle_issued_insns, int issue_more,
           bool starts_cycle_p, bool after_stall_p)
{
  fence_t f;

  _list_add (lp);
  f = FLIST_FENCE (*lp);

  FENCE_INSN (f) = insn;

  gcc_assert (state != NULL);
  FENCE_STATE (f) = state;

  FENCE_CYCLE (f) = cycle;
  FENCE_ISSUED_INSNS (f) = cycle_issued_insns;
  FENCE_STARTS_CYCLE_P (f) = starts_cycle_p;
  FENCE_AFTER_STALL_P (f) = after_stall_p;

  gcc_assert (dc != NULL);
  FENCE_DC (f) = dc;

  gcc_assert (tc != NULL || targetm.sched.alloc_sched_context == NULL);
  FENCE_TC (f) = tc;

  FENCE_LAST_SCHEDULED_INSN (f) = last_scheduled_insn;
  FENCE_ISSUE_MORE (f) = issue_more;
  FENCE_EXECUTING_INSNS (f) = executing_insns;
  FENCE_READY_TICKS (f) = ready_ticks;
  FENCE_READY_TICKS_SIZE (f) = ready_ticks_size;
  FENCE_SCHED_NEXT (f) = sched_next;

  init_fence_for_scheduling (f);
}

/* Remove the head node of the list pointed to by LP.  */
static void
flist_remove (flist_t *lp)
{
  if (FENCE_INSN (FLIST_FENCE (*lp)))
    fence_clear (FLIST_FENCE (*lp));
  _list_remove (lp);
}

/* Clear the fence list pointed to by LP.  */
void
flist_clear (flist_t *lp)
{
  while (*lp)
    flist_remove (lp);
}

/* Add ORIGINAL_INSN the def list DL honoring CROSSES_CALL.  */
void
def_list_add (def_list_t *dl, insn_t original_insn, bool crosses_call)
{
  def_t d;

  _list_add (dl);
  d = DEF_LIST_DEF (*dl);

  d->orig_insn = original_insn;
  d->crosses_call = crosses_call;
}


/* Functions to work with target contexts.  */

/* Bulk target context.  It is convenient for debugging purposes to ensure
   that there are no uninitialized (null) target contexts.  */
static tc_t bulk_tc = (tc_t) 1;

/* Target hooks wrappers.  In the future we can provide some default
   implementations for them.  */

/* Allocate a store for the target context.  */
static tc_t
alloc_target_context (void)
{
  return (targetm.sched.alloc_sched_context
	  ? targetm.sched.alloc_sched_context () : bulk_tc);
}

/* Init target context TC.
   If CLEAN_P is true, then make TC as it is beginning of the scheduler.
   Overwise, copy current backend context to TC.  */
static void
init_target_context (tc_t tc, bool clean_p)
{
  if (targetm.sched.init_sched_context)
    targetm.sched.init_sched_context (tc, clean_p);
}

/* Allocate and initialize a target context.  Meaning of CLEAN_P is the same as
   int init_target_context ().  */
tc_t
create_target_context (bool clean_p)
{
  tc_t tc = alloc_target_context ();

  init_target_context (tc, clean_p);
  return tc;
}

/* Copy TC to the current backend context.  */
void
set_target_context (tc_t tc)
{
  if (targetm.sched.set_sched_context)
    targetm.sched.set_sched_context (tc);
}

/* TC is about to be destroyed.  Free any internal data.  */
static void
clear_target_context (tc_t tc)
{
  if (targetm.sched.clear_sched_context)
    targetm.sched.clear_sched_context (tc);
}

/*  Clear and free it.  */
static void
delete_target_context (tc_t tc)
{
  clear_target_context (tc);

  if (targetm.sched.free_sched_context)
    targetm.sched.free_sched_context (tc);
}

/* Make a copy of FROM in TO.
   NB: May be this should be a hook.  */
static void
copy_target_context (tc_t to, tc_t from)
{
  tc_t tmp = create_target_context (false);

  set_target_context (from);
  init_target_context (to, false);

  set_target_context (tmp);
  delete_target_context (tmp);
}

/* Create a copy of TC.  */
static tc_t
create_copy_of_target_context (tc_t tc)
{
  tc_t copy = alloc_target_context ();

  copy_target_context (copy, tc);

  return copy;
}

/* Clear TC and initialize it according to CLEAN_P.  The meaning of CLEAN_P
   is the same as in init_target_context ().  */
void
reset_target_context (tc_t tc, bool clean_p)
{
  clear_target_context (tc);
  init_target_context (tc, clean_p);
}

/* Functions to work with dependence contexts.
   Dc (aka deps context, aka deps_t, aka struct deps *) is short for dependence
   context.  It accumulates information about processed insns to decide if
   current insn is dependent on the processed ones.  */

/* Make a copy of FROM in TO.  */
static void
copy_deps_context (deps_t to, deps_t from)
{
  init_deps (to, false);
  deps_join (to, from);
}

/* Allocate store for dep context.  */
static deps_t
alloc_deps_context (void)
{
  return XNEW (struct deps);
}

/* Allocate and initialize dep context.  */
static deps_t
create_deps_context (void)
{
  deps_t dc = alloc_deps_context ();

  init_deps (dc, false);
  return dc;
}

/* Create a copy of FROM.  */
static deps_t
create_copy_of_deps_context (deps_t from)
{
  deps_t to = alloc_deps_context ();

  copy_deps_context (to, from);
  return to;
}

/* Clean up internal data of DC.  */
static void
clear_deps_context (deps_t dc)
{
  free_deps (dc);
}

/* Clear and free DC.  */
static void
delete_deps_context (deps_t dc)
{
  clear_deps_context (dc);
  free (dc);
}

/* Clear and init DC.  */
static void
reset_deps_context (deps_t dc)
{
  clear_deps_context (dc);
  init_deps (dc, false);
}

/* This structure describes the dependence analysis hooks for advancing
   dependence context.  */
static struct sched_deps_info_def advance_deps_context_sched_deps_info =
  {
    NULL,

    NULL, /* start_insn */
    NULL, /* finish_insn */
    NULL, /* start_lhs */
    NULL, /* finish_lhs */
    NULL, /* start_rhs */
    NULL, /* finish_rhs */
    haifa_note_reg_set,
    haifa_note_reg_clobber,
    haifa_note_reg_use,
    NULL, /* note_mem_dep */
    NULL, /* note_dep */

    0, 0, 0
  };

/* Process INSN and add its impact on DC.  */
void
advance_deps_context (deps_t dc, insn_t insn)
{
  sched_deps_info = &advance_deps_context_sched_deps_info;
  deps_analyze_insn (dc, insn);
}


/* Functions to work with DFA states.  */

/* Allocate store for a DFA state.  */
static state_t
state_alloc (void)
{
  return xmalloc (dfa_state_size);
}

/* Allocate and initialize DFA state.  */
static state_t
state_create (void)
{
  state_t state = state_alloc ();

  state_reset (state);
  advance_state (state);
  return state;
}

/* Free DFA state.  */
static void
state_free (state_t state)
{
  free (state);
}

/* Make a copy of FROM in TO.  */
static void
state_copy (state_t to, state_t from)
{
  memcpy (to, from, dfa_state_size);
}

/* Create a copy of FROM.  */
static state_t
state_create_copy (state_t from)
{
  state_t to = state_alloc ();

  state_copy (to, from);
  return to;
}


/* Functions to work with fences.  */

/* Clear the fence.  */
static void
fence_clear (fence_t f)
{
  state_t s = FENCE_STATE (f);
  deps_t dc = FENCE_DC (f);
  void *tc = FENCE_TC (f);

  ilist_clear (&FENCE_BNDS (f));

  gcc_assert ((s != NULL && dc != NULL && tc != NULL)
	      || (s == NULL && dc == NULL && tc == NULL));

  if (s != NULL)
    free (s);

  if (dc != NULL)
    delete_deps_context (dc);

  if (tc != NULL)
    delete_target_context (tc);
  VEC_free (rtx, gc, FENCE_EXECUTING_INSNS (f));
  free (FENCE_READY_TICKS (f));
  FENCE_READY_TICKS (f) = NULL;
}

/* Init a list of fences with successors of OLD_FENCE.  */
void
init_fences (insn_t old_fence)
{
  insn_t succ;
  succ_iterator si;
  bool first = true;
  int ready_ticks_size = get_max_uid () + 1;

  FOR_EACH_SUCC_1 (succ, si, old_fence,
                   SUCCS_NORMAL | SUCCS_SKIP_TO_LOOP_EXITS)
    {

      if (first)
        first = false;
      else
        gcc_assert (flag_sel_sched_pipelining_outer_loops);

      flist_add (&fences, succ,
		 state_create (),
		 create_deps_context () /* dc */,
		 create_target_context (true) /* tc */,
		 NULL_RTX /* last_scheduled_insn */,
                 NULL, /* executing_insns */
                 XCNEWVEC (int, ready_ticks_size), /* ready_ticks */
                 ready_ticks_size,
                 NULL_RTX /* sched_next */,
		 1 /* cycle */, 0 /* cycle_issued_insns */,
		 issue_rate, /* issue_more */
		 1 /* starts_cycle_p */, 0 /* after_stall_p */);
    }
}

/* Merges two fences (filling fields of fence F with resulting values) by
   following rules: 1) state, target context and last scheduled insn are
   propagated from fallthrough edge if it is available;
   2) deps context and cycle is propagated from more probable edge;
   3) all other fields are set to corresponding constant values.

   INSN, STATE, DC, TC, LAST_SCHEDULED_INSN, EXECUTING_INSNS,
   READY_TICKS, READY_TICKS_SIZE, SCHED_NEXT, CYCLE, ISSUE_MORE
   and AFTER_STALL_P are the corresponding fields of the second fence.  */
static void
merge_fences (fence_t f, insn_t insn,
	      state_t state, deps_t dc, void *tc,
              rtx last_scheduled_insn, VEC(rtx, gc) *executing_insns,
              int *ready_ticks, int ready_ticks_size,
	      rtx sched_next, int cycle, int issue_more, bool after_stall_p)
{
  insn_t last_scheduled_insn_old = FENCE_LAST_SCHEDULED_INSN (f);

  gcc_assert (sel_bb_head_p (FENCE_INSN (f))
              && !sched_next && !FENCE_SCHED_NEXT (f));

  /* Check if we can decide which path fences came.
     If we can't (or don't want to) - reset all.  */
  if (last_scheduled_insn == NULL
      || last_scheduled_insn_old == NULL
      /* This is a case when INSN is reachable on several paths from
         one insn (this can happen when pipelining of outer loops is on and
         there are two edges: one going around of inner loop and the other -
         right through it; in such case just reset everything).  */
      || last_scheduled_insn == last_scheduled_insn_old)
    {
      state_reset (FENCE_STATE (f));
      state_free (state);

      reset_deps_context (FENCE_DC (f));
      delete_deps_context (dc);

      reset_target_context (FENCE_TC (f), true);
      delete_target_context (tc);

      if (cycle > FENCE_CYCLE (f))
        FENCE_CYCLE (f) = cycle;

      FENCE_LAST_SCHEDULED_INSN (f) = NULL;
      FENCE_ISSUE_MORE (f) = issue_rate;
      VEC_free (rtx, gc, executing_insns);
      free (ready_ticks);
      if (FENCE_EXECUTING_INSNS (f))
        VEC_block_remove (rtx, FENCE_EXECUTING_INSNS (f), 0,
                          VEC_length (rtx, FENCE_EXECUTING_INSNS (f)));
      if (FENCE_READY_TICKS (f))
        memset (FENCE_READY_TICKS (f), 0, FENCE_READY_TICKS_SIZE (f));
    }
  else
    {
      edge edge_old = NULL, edge_new = NULL;
      edge candidate;
      succ_iterator si;
      insn_t succ;

      /* Find fallthrough edge.  */
      gcc_assert (BLOCK_FOR_INSN (insn)->prev_bb);
      candidate = find_fallthru_edge (BLOCK_FOR_INSN (insn)->prev_bb);

      if (!candidate
          || (candidate->src != BLOCK_FOR_INSN (last_scheduled_insn)
              && candidate->src != BLOCK_FOR_INSN (last_scheduled_insn_old)))
        {
          /* No fallthrough edge leading to basic block of INSN.  */
          state_reset (FENCE_STATE (f));
          state_free (state);

          reset_target_context (FENCE_TC (f), true);
          delete_target_context (tc);

          FENCE_LAST_SCHEDULED_INSN (f) = NULL;
	  FENCE_ISSUE_MORE (f) = issue_rate;
        }
      else
        if (candidate->src == BLOCK_FOR_INSN (last_scheduled_insn))
          {
            /* Would be weird if same insn is successor of several fallthrough
               edges.  */
            gcc_assert (BLOCK_FOR_INSN (insn)->prev_bb
                        != BLOCK_FOR_INSN (last_scheduled_insn_old));

            state_free (FENCE_STATE (f));
            FENCE_STATE (f) = state;

            delete_target_context (FENCE_TC (f));
            FENCE_TC (f) = tc;

            FENCE_LAST_SCHEDULED_INSN (f) = last_scheduled_insn;
	    FENCE_ISSUE_MORE (f) = issue_more;
          }
        else
          {
            /* Leave STATE, TC and LAST_SCHEDULED_INSN fields untouched.  */
            state_free (state);
            delete_target_context (tc);

            gcc_assert (BLOCK_FOR_INSN (insn)->prev_bb
                        != BLOCK_FOR_INSN (last_scheduled_insn));
          }

        /* Find edge of first predecessor (last_scheduled_insn_old->insn).  */
        FOR_EACH_SUCC_1 (succ, si, last_scheduled_insn_old,
                         SUCCS_NORMAL | SUCCS_SKIP_TO_LOOP_EXITS)
          {
            if (succ == insn)
              {
                /* No same successor allowed from several edges.  */
                gcc_assert (!edge_old);
                edge_old = si.e1;
              }
          }
        /* Find edge of second predecessor (last_scheduled_insn->insn).  */
        FOR_EACH_SUCC_1 (succ, si, last_scheduled_insn,
                         SUCCS_NORMAL | SUCCS_SKIP_TO_LOOP_EXITS)
          {
            if (succ == insn)
              {
                /* No same successor allowed from several edges.  */
                gcc_assert (!edge_new);
                edge_new = si.e1;
              }
          }

        /* Check if we can choose most probable predecessor.  */
        if (edge_old == NULL || edge_new == NULL)
          {
            reset_deps_context (FENCE_DC (f));
            delete_deps_context (dc);
            VEC_free (rtx, gc, executing_insns);
            free (ready_ticks);

            FENCE_CYCLE (f) = MAX (FENCE_CYCLE (f), cycle);
            if (FENCE_EXECUTING_INSNS (f))
              VEC_block_remove (rtx, FENCE_EXECUTING_INSNS (f), 0,
                                VEC_length (rtx, FENCE_EXECUTING_INSNS (f)));
            if (FENCE_READY_TICKS (f))
              memset (FENCE_READY_TICKS (f), 0, FENCE_READY_TICKS_SIZE (f));
          }
        else
          if (edge_new->probability > edge_old->probability)
            {
              delete_deps_context (FENCE_DC (f));
              FENCE_DC (f) = dc;
              VEC_free (rtx, gc, FENCE_EXECUTING_INSNS (f));
              FENCE_EXECUTING_INSNS (f) = executing_insns;
              free (FENCE_READY_TICKS (f));
              FENCE_READY_TICKS (f) = ready_ticks;
              FENCE_READY_TICKS_SIZE (f) = ready_ticks_size;
              FENCE_CYCLE (f) = cycle;
            }
          else
            {
              /* Leave DC and CYCLE untouched.  */
              delete_deps_context (dc);
              VEC_free (rtx, gc, executing_insns);
              free (ready_ticks);
            }
    }

  /* Fill remaining invariant fields.  */
  if (after_stall_p)
    FENCE_AFTER_STALL_P (f) = 1;

  FENCE_ISSUED_INSNS (f) = 0;
  FENCE_STARTS_CYCLE_P (f) = 1;
  FENCE_SCHED_NEXT (f) = NULL;
}

/* Add a new fence to NEW_FENCES list, initializing it from all
   other parameters.  */
static void
add_to_fences (flist_tail_t new_fences, insn_t insn,
               state_t state, deps_t dc, void *tc, rtx last_scheduled_insn,
               VEC(rtx, gc) *executing_insns, int *ready_ticks,
               int ready_ticks_size, rtx sched_next, int cycle,
               int cycle_issued_insns, int issue_rate,
	       bool starts_cycle_p, bool after_stall_p)
{
  fence_t f = flist_lookup (FLIST_TAIL_HEAD (new_fences), insn);

  if (! f)
    {
      flist_add (FLIST_TAIL_TAILP (new_fences), insn, state, dc, tc,
		 last_scheduled_insn, executing_insns, ready_ticks,
                 ready_ticks_size, sched_next, cycle, cycle_issued_insns,
		 issue_rate, starts_cycle_p, after_stall_p);

      FLIST_TAIL_TAILP (new_fences)
	= &FLIST_NEXT (*FLIST_TAIL_TAILP (new_fences));
    }
  else
    {
      merge_fences (f, insn, state, dc, tc, last_scheduled_insn,
                    executing_insns, ready_ticks, ready_ticks_size,
                    sched_next, cycle, issue_rate, after_stall_p);
    }
}

/* Move the first fence in the OLD_FENCES list to NEW_FENCES.  */
void
move_fence_to_fences (flist_t old_fences, flist_tail_t new_fences)
{
  fence_t f, old;
  flist_t *tailp = FLIST_TAIL_TAILP (new_fences);

  old = FLIST_FENCE (old_fences);
  f = flist_lookup (FLIST_TAIL_HEAD (new_fences),
                    FENCE_INSN (FLIST_FENCE (old_fences)));
  if (f)
    {
      merge_fences (f, old->insn, old->state, old->dc, old->tc,
                    old->last_scheduled_insn, old->executing_insns,
                    old->ready_ticks, old->ready_ticks_size,
                    old->sched_next, old->cycle, old->issue_more,
                    old->after_stall_p);
    }
  else
    {
      _list_add (tailp);
      FLIST_TAIL_TAILP (new_fences) = &FLIST_NEXT (*tailp);
      *FLIST_FENCE (*tailp) = *old;
      init_fence_for_scheduling (FLIST_FENCE (*tailp));
    }
  FENCE_INSN (old) = NULL;
}

/* Add a new fence to NEW_FENCES list and initialize most of its data
   as a clean one.  */
void
add_clean_fence_to_fences (flist_tail_t new_fences, insn_t succ, fence_t fence)
{
  int ready_ticks_size = get_max_uid () + 1;

  add_to_fences (new_fences,
                 succ, state_create (), create_deps_context (),
                 create_target_context (true),
                 NULL_RTX, NULL,
                 XCNEWVEC (int, ready_ticks_size), ready_ticks_size,
                 NULL_RTX, FENCE_CYCLE (fence) + 1,
                 0, issue_rate, 1, FENCE_AFTER_STALL_P (fence));
}

/* Add a new fence to NEW_FENCES list and initialize all of its data
   from FENCE and SUCC.  */
void
add_dirty_fence_to_fences (flist_tail_t new_fences, insn_t succ, fence_t fence)
{
  int * new_ready_ticks
    = XNEWVEC (int, FENCE_READY_TICKS_SIZE (fence));

  memcpy (new_ready_ticks, FENCE_READY_TICKS (fence),
          FENCE_READY_TICKS_SIZE (fence) * sizeof (int));
  add_to_fences (new_fences,
                 succ, state_create_copy (FENCE_STATE (fence)),
                 create_copy_of_deps_context (FENCE_DC (fence)),
                 create_copy_of_target_context (FENCE_TC (fence)),
                 FENCE_LAST_SCHEDULED_INSN (fence),
                 VEC_copy (rtx, gc, FENCE_EXECUTING_INSNS (fence)),
                 new_ready_ticks,
                 FENCE_READY_TICKS_SIZE (fence),
                 FENCE_SCHED_NEXT (fence),
                 FENCE_CYCLE (fence),
                 FENCE_ISSUED_INSNS (fence),
		 FENCE_ISSUE_MORE (fence),
                 FENCE_STARTS_CYCLE_P (fence),
                 FENCE_AFTER_STALL_P (fence));
}


/* Functions to work with regset and nop pools.  */

/* Returns the new regset from pool.  It might have some of the bits set
   from the previous usage.  */
regset
get_regset_from_pool (void)
{
  regset rs;

  if (regset_pool.n != 0)
    rs = regset_pool.v[--regset_pool.n];
  else
    /* We need to create the regset.  */
    {
      rs = ALLOC_REG_SET (&reg_obstack);

      if (regset_pool.nn == regset_pool.ss)
	regset_pool.vv = XRESIZEVEC (regset, regset_pool.vv,
                                     (regset_pool.ss = 2 * regset_pool.ss + 1));
      regset_pool.vv[regset_pool.nn++] = rs;
    }

  regset_pool.diff++;

  return rs;
}

/* Same as above, but returns the empty regset.  */
regset
get_clear_regset_from_pool (void)
{
  regset rs = get_regset_from_pool ();

  CLEAR_REG_SET (rs);
  return rs;
}

/* Return regset RS to the pool for future use.  */
void
return_regset_to_pool (regset rs)
{
  regset_pool.diff--;

  if (regset_pool.n == regset_pool.s)
    regset_pool.v = XRESIZEVEC (regset, regset_pool.v,
                                (regset_pool.s = 2 * regset_pool.s + 1));
  regset_pool.v[regset_pool.n++] = rs;
}

#ifdef ENABLE_CHECKING
/* This is used as a qsort callback for sorting regset pool stacks.
   X and XX are addresses of two regsets.  They are never equal.  */
static int
cmp_v_in_regset_pool (const void *x, const void *xx)
{
  return *((const regset *) x) - *((const regset *) xx);
}
#endif

/*  Free the regset pool possibly checking for memory leaks.  */
void
free_regset_pool (void)
{
#ifdef ENABLE_CHECKING
  {
    regset *v = regset_pool.v;
    int i = 0;
    int n = regset_pool.n;

    regset *vv = regset_pool.vv;
    int ii = 0;
    int nn = regset_pool.nn;

    int diff = 0;

    gcc_assert (n <= nn);

    /* Sort both vectors so it will be possible to compare them.  */
    qsort (v, n, sizeof (*v), cmp_v_in_regset_pool);
    qsort (vv, nn, sizeof (*vv), cmp_v_in_regset_pool);

    while (ii < nn)
      {
        if (v[i] == vv[ii])
          i++;
        else
          /* VV[II] was lost.  */
          diff++;

        ii++;
      }

    gcc_assert (diff == regset_pool.diff);
  }
#endif

  /* If not true - we have a memory leak.  */
  gcc_assert (regset_pool.diff == 0);

  while (regset_pool.n)
    {
      --regset_pool.n;
      FREE_REG_SET (regset_pool.v[regset_pool.n]);
    }

  free (regset_pool.v);
  regset_pool.v = NULL;
  regset_pool.s = 0;

  free (regset_pool.vv);
  regset_pool.vv = NULL;
  regset_pool.nn = 0;
  regset_pool.ss = 0;

  regset_pool.diff = 0;
}


/* Functions to work with nop pools.  NOP insns are used as temporary
   placeholders of the insns being scheduled to allow correct update of
   the data sets.  When update is finished, NOPs are deleted.  */

/* A vinsn that is used to represent a nop.  This vinsn is shared among all
   nops sel-sched generates.  */
static vinsn_t nop_vinsn = NULL;

/* Emit a nop before INSN, taking it from pool.  */
insn_t
get_nop_from_pool (insn_t insn)
{
  insn_t nop;
  bool old_p = nop_pool.n != 0;
  int flags;

  if (old_p)
    nop = nop_pool.v[--nop_pool.n];
  else
    nop = nop_pattern;

  nop = emit_insn_before (nop, insn);

  if (old_p)
    flags = INSN_INIT_TODO_SSID;
  else
    flags = INSN_INIT_TODO_LUID | INSN_INIT_TODO_SSID;

  set_insn_init (INSN_EXPR (insn), nop_vinsn, INSN_SEQNO (insn));
  sel_init_new_insn (nop, flags);

  return nop;
}

/* Remove NOP from the instruction stream and return it to the pool.  */
void
return_nop_to_pool (insn_t nop, bool full_tidying)
{
  gcc_assert (INSN_IN_STREAM_P (nop));
  sel_remove_insn (nop, false, full_tidying);

  if (nop_pool.n == nop_pool.s)
    nop_pool.v = XRESIZEVEC (rtx, nop_pool.v,
                             (nop_pool.s = 2 * nop_pool.s + 1));
  nop_pool.v[nop_pool.n++] = nop;
}

/* Free the nop pool.  */
void
free_nop_pool (void)
{
  nop_pool.n = 0;
  nop_pool.s = 0;
  free (nop_pool.v);
  nop_pool.v = NULL;
}


/* Skip unspec to support ia64 speculation. Called from rtx_equal_p_cb.
   The callback is given two rtxes XX and YY and writes the new rtxes
   to NX and NY in case some needs to be skipped.  */
static int
skip_unspecs_callback (const_rtx *xx, const_rtx *yy, rtx *nx, rtx* ny)
{
  const_rtx x = *xx;
  const_rtx y = *yy;

  if (GET_CODE (x) == UNSPEC
      && (targetm.sched.skip_rtx_p == NULL
          || targetm.sched.skip_rtx_p (x)))
    {
      *nx = XVECEXP (x, 0, 0);
      *ny = CONST_CAST_RTX (y);
      return 1;
    }

  if (GET_CODE (y) == UNSPEC
      && (targetm.sched.skip_rtx_p == NULL
          || targetm.sched.skip_rtx_p (y)))
    {
      *nx = CONST_CAST_RTX (x);
      *ny = XVECEXP (y, 0, 0);
      return 1;
    }

  return 0;
}

/* Callback, called from hash_rtx_cb.  Helps to hash UNSPEC rtx X in a correct way
   to support ia64 speculation.  When changes are needed, new rtx X and new mode
   NMODE are written, and the callback returns true.  */
static int
hash_with_unspec_callback (const_rtx x, enum machine_mode mode ATTRIBUTE_UNUSED,
                           rtx *nx, enum machine_mode* nmode)
{
  if (GET_CODE (x) == UNSPEC
      && targetm.sched.skip_rtx_p
      && targetm.sched.skip_rtx_p (x))
    {
      *nx = XVECEXP (x, 0 ,0);
      *nmode = VOIDmode;
      return 1;
    }

  return 0;
}

/* Returns LHS and RHS are ok to be scheduled separately.  */
static bool
lhs_and_rhs_separable_p (rtx lhs, rtx rhs)
{
  if (lhs == NULL || rhs == NULL)
    return false;

  /* Do not schedule CONST, CONST_INT and CONST_DOUBLE etc as rhs: no point
     to use reg, if const can be used.  Moreover, scheduling const as rhs may
     lead to mode mismatch cause consts don't have modes but they could be
     merged from branches where the same const used in different modes.  */
  if (CONSTANT_P (rhs))
    return false;

  /* ??? Do not rename predicate registers to avoid ICEs in bundling.  */
  if (COMPARISON_P (rhs))
      return false;

  /* Do not allow single REG to be an rhs.  */
  if (REG_P (rhs))
    return false;

  /* See comment at find_used_regs_1 (*1) for explanation of this
     restriction.  */
  /* FIXME: remove this later.  */
  if (MEM_P (lhs))
    return false;

  /* This will filter all tricky things like ZERO_EXTRACT etc.
     For now we don't handle it.  */
  if (!REG_P (lhs) && !MEM_P (lhs))
    return false;

  return true;
}

/* Initialize vinsn VI for INSN.  Only for use from vinsn_create ().  When
   FORCE_UNIQUE_P is true, the resulting vinsn will not be clonable.  This is
   used e.g. for insns from recovery blocks.  */
static void
vinsn_init (vinsn_t vi, insn_t insn, bool force_unique_p)
{
  hash_rtx_callback_function hrcf;
  int insn_class;

  VINSN_INSN_RTX (vi) = insn;
  VINSN_COUNT (vi) = 0;
  vi->cost = -1;

  if (DF_INSN_UID_SAFE_GET (INSN_UID (insn)) != NULL)
    init_id_from_df (VINSN_ID (vi), insn, force_unique_p);
  else
    deps_init_id (VINSN_ID (vi), insn, force_unique_p);

  /* Hash vinsn depending on whether it is separable or not.  */
  hrcf = targetm.sched.skip_rtx_p ? hash_with_unspec_callback : NULL;
  if (VINSN_SEPARABLE_P (vi))
    {
      rtx rhs = VINSN_RHS (vi);

      VINSN_HASH (vi) = hash_rtx_cb (rhs, GET_MODE (rhs),
                                     NULL, NULL, false, hrcf);
      VINSN_HASH_RTX (vi) = hash_rtx_cb (VINSN_PATTERN (vi),
                                         VOIDmode, NULL, NULL,
                                         false, hrcf);
    }
  else
    {
      VINSN_HASH (vi) = hash_rtx_cb (VINSN_PATTERN (vi), VOIDmode,
                                     NULL, NULL, false, hrcf);
      VINSN_HASH_RTX (vi) = VINSN_HASH (vi);
    }

  insn_class = haifa_classify_insn (insn);
  if (insn_class >= 2
      && (!targetm.sched.get_insn_spec_ds
          || ((targetm.sched.get_insn_spec_ds (insn) & BEGIN_CONTROL)
              == 0)))
    VINSN_MAY_TRAP_P (vi) = true;
  else
    VINSN_MAY_TRAP_P (vi) = false;
}

/* Indicate that VI has become the part of an rtx object.  */
void
vinsn_attach (vinsn_t vi)
{
  /* Assert that VI is not pending for deletion.  */
  gcc_assert (VINSN_INSN_RTX (vi));

  VINSN_COUNT (vi)++;
}

/* Create and init VI from the INSN.  Use UNIQUE_P for determining the correct
   VINSN_TYPE (VI).  */
static vinsn_t
vinsn_create (insn_t insn, bool force_unique_p)
{
  vinsn_t vi = XCNEW (struct vinsn_def);

  vinsn_init (vi, insn, force_unique_p);
  return vi;
}

/* Return a copy of VI.  When REATTACH_P is true, detach VI and attach
   the copy.  */
vinsn_t
vinsn_copy (vinsn_t vi, bool reattach_p)
{
  rtx copy;
  bool unique = VINSN_UNIQUE_P (vi);
  vinsn_t new_vi;

  copy = create_copy_of_insn_rtx (VINSN_INSN_RTX (vi));
  new_vi = create_vinsn_from_insn_rtx (copy, unique);
  if (reattach_p)
    {
      vinsn_detach (vi);
      vinsn_attach (new_vi);
    }

  return new_vi;
}

/* Delete the VI vinsn and free its data.  */
static void
vinsn_delete (vinsn_t vi)
{
  gcc_assert (VINSN_COUNT (vi) == 0);

  return_regset_to_pool (VINSN_REG_SETS (vi));
  return_regset_to_pool (VINSN_REG_USES (vi));
  return_regset_to_pool (VINSN_REG_CLOBBERS (vi));

  free (vi);
}

/* Indicate that VI is no longer a part of some rtx object.
   Remove VI if it is no longer needed.  */
void
vinsn_detach (vinsn_t vi)
{
  gcc_assert (VINSN_COUNT (vi) > 0);

  if (--VINSN_COUNT (vi) == 0)
    vinsn_delete (vi);
}

/* Returns TRUE if VI is a branch.  */
bool
vinsn_cond_branch_p (vinsn_t vi)
{
  insn_t insn;

  if (!VINSN_UNIQUE_P (vi))
    return false;

  insn = VINSN_INSN_RTX (vi);
  if (BB_END (BLOCK_FOR_INSN (insn)) != insn)
    return false;

  return control_flow_insn_p (insn);
}

/* Return latency of INSN.  */
static int
sel_insn_rtx_cost (rtx insn)
{
  int cost;

  /* A USE insn, or something else we don't need to
     understand.  We can't pass these directly to
     result_ready_cost or insn_default_latency because it will
     trigger a fatal error for unrecognizable insns.  */
  if (recog_memoized (insn) < 0)
    cost = 0;
  else
    {
      cost = insn_default_latency (insn);

      if (cost < 0)
	cost = 0;
    }

  return cost;
}

/* Return the cost of the VI.
   !!! FIXME: Unify with haifa-sched.c: insn_cost ().  */
int
sel_vinsn_cost (vinsn_t vi)
{
  int cost = vi->cost;

  if (cost < 0)
    {
      cost = sel_insn_rtx_cost (VINSN_INSN_RTX (vi));
      vi->cost = cost;
    }

  return cost;
}


/* Functions for insn emitting.  */

/* Emit new insn after AFTER based on PATTERN and initialize its data from
   EXPR and SEQNO.  */
insn_t
sel_gen_insn_from_rtx_after (rtx pattern, expr_t expr, int seqno, insn_t after)
{
  insn_t new_insn;

  gcc_assert (EXPR_TARGET_AVAILABLE (expr) == true);

  new_insn = emit_insn_after (pattern, after);
  set_insn_init (expr, NULL, seqno);
  sel_init_new_insn (new_insn, INSN_INIT_TODO_LUID | INSN_INIT_TODO_SSID);

  return new_insn;
}

/* Force newly generated vinsns to be unique.  */
static bool init_insn_force_unique_p = false;

/* Emit new speculation recovery insn after AFTER based on PATTERN and
   initialize its data from EXPR and SEQNO.  */
insn_t
sel_gen_recovery_insn_from_rtx_after (rtx pattern, expr_t expr, int seqno,
				      insn_t after)
{
  insn_t insn;

  gcc_assert (!init_insn_force_unique_p);

  init_insn_force_unique_p = true;
  insn = sel_gen_insn_from_rtx_after (pattern, expr, seqno, after);
  CANT_MOVE (insn) = 1;
  init_insn_force_unique_p = false;

  return insn;
}

/* Emit new insn after AFTER based on EXPR and SEQNO.  If VINSN is not NULL,
   take it as a new vinsn instead of EXPR's vinsn.
   We simplify insns later, after scheduling region in
   simplify_changed_insns.  */
insn_t
sel_gen_insn_from_expr_after (expr_t expr, vinsn_t vinsn, int seqno,
                              insn_t after)
{
  expr_t emit_expr;
  insn_t insn;
  int flags;

  emit_expr = set_insn_init (expr, vinsn ? vinsn : EXPR_VINSN (expr),
                             seqno);
  insn = EXPR_INSN_RTX (emit_expr);
  add_insn_after (insn, after, BLOCK_FOR_INSN (insn));

  flags = INSN_INIT_TODO_SSID;
  if (INSN_LUID (insn) == 0)
    flags |= INSN_INIT_TODO_LUID;
  sel_init_new_insn (insn, flags);

  return insn;
}

/* Move insn from EXPR after AFTER.  */
insn_t
sel_move_insn (expr_t expr, int seqno, insn_t after)
{
  insn_t insn = EXPR_INSN_RTX (expr);
  basic_block bb = BLOCK_FOR_INSN (after);
  insn_t next = NEXT_INSN (after);

  /* Assert that in move_op we disconnected this insn properly.  */
  gcc_assert (EXPR_VINSN (INSN_EXPR (insn)) != NULL);
  PREV_INSN (insn) = after;
  NEXT_INSN (insn) = next;

  NEXT_INSN (after) = insn;
  PREV_INSN (next) = insn;

  /* Update links from insn to bb and vice versa.  */
  df_insn_change_bb (insn, bb);
  if (BB_END (bb) == after)
    BB_END (bb) = insn;

  prepare_insn_expr (insn, seqno);
  return insn;
}


/* Functions to work with right-hand sides.  */

/* Search for a hash value determined by UID/NEW_VINSN in a sorted vector
   VECT and return true when found.  Use NEW_VINSN for comparison only when
   COMPARE_VINSNS is true.  Write to INDP the index on which
   the search has stopped, such that inserting the new element at INDP will
   retain VECT's sort order.  */
static bool
find_in_history_vect_1 (VEC(expr_history_def, heap) *vect,
                        unsigned uid, vinsn_t new_vinsn,
                        bool compare_vinsns, int *indp)
{
  expr_history_def *arr;
  int i, j, len = VEC_length (expr_history_def, vect);

  if (len == 0)
    {
      *indp = 0;
      return false;
    }

  arr = VEC_address (expr_history_def, vect);
  i = 0, j = len - 1;

  while (i <= j)
    {
      unsigned auid = arr[i].uid;
      vinsn_t avinsn = arr[i].new_expr_vinsn;

      if (auid == uid
          /* When undoing transformation on a bookkeeping copy, the new vinsn
             may not be exactly equal to the one that is saved in the vector.
             This is because the insn whose copy we're checking was possibly
             substituted itself.  */
          && (! compare_vinsns
              || vinsn_equal_p (avinsn, new_vinsn)))
        {
          *indp = i;
          return true;
        }
      else if (auid > uid)
        break;
      i++;
    }

  *indp = i;
  return false;
}

/* Search for a uid of INSN and NEW_VINSN in a sorted vector VECT.  Return
   the position found or -1, if no such value is in vector.
   Search also for UIDs of insn's originators, if ORIGINATORS_P is true.  */
int
find_in_history_vect (VEC(expr_history_def, heap) *vect, rtx insn,
                      vinsn_t new_vinsn, bool originators_p)
{
  int ind;

  if (find_in_history_vect_1 (vect, INSN_UID (insn), new_vinsn,
                              false, &ind))
    return ind;

  if (INSN_ORIGINATORS (insn) && originators_p)
    {
      unsigned uid;
      bitmap_iterator bi;

      EXECUTE_IF_SET_IN_BITMAP (INSN_ORIGINATORS (insn), 0, uid, bi)
        if (find_in_history_vect_1 (vect, uid, new_vinsn, false, &ind))
          return ind;
    }

  return -1;
}

/* Insert new element in a sorted history vector pointed to by PVECT,
   if it is not there already.  The element is searched using
   UID/NEW_EXPR_VINSN pair.  TYPE, OLD_EXPR_VINSN and SPEC_DS save
   the history of a transformation.  */
void
insert_in_history_vect (VEC (expr_history_def, heap) **pvect,
                        unsigned uid, enum local_trans_type type,
                        vinsn_t old_expr_vinsn, vinsn_t new_expr_vinsn,
                        ds_t spec_ds)
{
  VEC(expr_history_def, heap) *vect = *pvect;
  expr_history_def temp;
  bool res;
  int ind;

  res = find_in_history_vect_1 (vect, uid, new_expr_vinsn, true, &ind);

  if (res)
    {
      expr_history_def *phist = VEC_index (expr_history_def, vect, ind);

      /* It is possible that speculation types of expressions that were
         propagated through different paths will be different here.  In this
         case, merge the status to get the correct check later.  */
      if (phist->spec_ds != spec_ds)
        phist->spec_ds = ds_max_merge (phist->spec_ds, spec_ds);
      return;
    }

  temp.uid = uid;
  temp.old_expr_vinsn = old_expr_vinsn;
  temp.new_expr_vinsn = new_expr_vinsn;
  temp.spec_ds = spec_ds;
  temp.type = type;

  vinsn_attach (old_expr_vinsn);
  vinsn_attach (new_expr_vinsn);
  VEC_safe_insert (expr_history_def, heap, vect, ind, &temp);
  *pvect = vect;
}

/* Free history vector PVECT.  */
static void
free_history_vect (VEC (expr_history_def, heap) **pvect)
{
  unsigned i;
  expr_history_def *phist;

  if (! *pvect)
    return;

  for (i = 0;
       VEC_iterate (expr_history_def, *pvect, i, phist);
       i++)
    {
      vinsn_detach (phist->old_expr_vinsn);
      vinsn_detach (phist->new_expr_vinsn);
    }

  VEC_free (expr_history_def, heap, *pvect);
  *pvect = NULL;
}


/* Compare two vinsns as rhses if possible and as vinsns otherwise.  */
bool
vinsn_equal_p (vinsn_t x, vinsn_t y)
{
  rtx_equal_p_callback_function repcf;

  if (x == y)
    return true;

  if (VINSN_TYPE (x) != VINSN_TYPE (y))
    return false;

  if (VINSN_HASH (x) != VINSN_HASH (y))
    return false;

  repcf = targetm.sched.skip_rtx_p ? skip_unspecs_callback : NULL;
  if (VINSN_SEPARABLE_P (x))
    {
      /* Compare RHSes of VINSNs.  */
      gcc_assert (VINSN_RHS (x));
      gcc_assert (VINSN_RHS (y));

      return rtx_equal_p_cb (VINSN_RHS (x), VINSN_RHS (y), repcf);
    }

  return rtx_equal_p_cb (VINSN_PATTERN (x), VINSN_PATTERN (y), repcf);
}


/* Functions for working with expressions.  */

/* Initialize EXPR.  */
static void
init_expr (expr_t expr, vinsn_t vi, int spec, int use, int priority,
	   int sched_times, int orig_bb_index, ds_t spec_done_ds,
	   ds_t spec_to_check_ds, int orig_sched_cycle,
	   VEC(expr_history_def, heap) *history, bool target_available,
           bool was_substituted, bool was_renamed, bool needs_spec_check_p,
           bool cant_move)
{
  vinsn_attach (vi);

  EXPR_VINSN (expr) = vi;
  EXPR_SPEC (expr) = spec;
  EXPR_USEFULNESS (expr) = use;
  EXPR_PRIORITY (expr) = priority;
  EXPR_PRIORITY_ADJ (expr) = 0;
  EXPR_SCHED_TIMES (expr) = sched_times;
  EXPR_ORIG_BB_INDEX (expr) = orig_bb_index;
  EXPR_ORIG_SCHED_CYCLE (expr) = orig_sched_cycle;
  EXPR_SPEC_DONE_DS (expr) = spec_done_ds;
  EXPR_SPEC_TO_CHECK_DS (expr) = spec_to_check_ds;

  if (history)
    EXPR_HISTORY_OF_CHANGES (expr) = history;
  else
    EXPR_HISTORY_OF_CHANGES (expr) = NULL;

  EXPR_TARGET_AVAILABLE (expr) = target_available;
  EXPR_WAS_SUBSTITUTED (expr) = was_substituted;
  EXPR_WAS_RENAMED (expr) = was_renamed;
  EXPR_NEEDS_SPEC_CHECK_P (expr) = needs_spec_check_p;
  EXPR_CANT_MOVE (expr) = cant_move;
}

/* Make a copy of the expr FROM into the expr TO.  */
void
copy_expr (expr_t to, expr_t from)
{
  VEC(expr_history_def, heap) *temp = NULL;

  if (EXPR_HISTORY_OF_CHANGES (from))
    {
      unsigned i;
      expr_history_def *phist;

      temp = VEC_copy (expr_history_def, heap, EXPR_HISTORY_OF_CHANGES (from));
      for (i = 0;
           VEC_iterate (expr_history_def, temp, i, phist);
           i++)
        {
          vinsn_attach (phist->old_expr_vinsn);
          vinsn_attach (phist->new_expr_vinsn);
        }
    }

  init_expr (to, EXPR_VINSN (from), EXPR_SPEC (from),
             EXPR_USEFULNESS (from), EXPR_PRIORITY (from),
	     EXPR_SCHED_TIMES (from), EXPR_ORIG_BB_INDEX (from),
	     EXPR_SPEC_DONE_DS (from), EXPR_SPEC_TO_CHECK_DS (from),
	     EXPR_ORIG_SCHED_CYCLE (from), temp,
             EXPR_TARGET_AVAILABLE (from), EXPR_WAS_SUBSTITUTED (from),
             EXPR_WAS_RENAMED (from), EXPR_NEEDS_SPEC_CHECK_P (from),
             EXPR_CANT_MOVE (from));
}

/* Same, but the final expr will not ever be in av sets, so don't copy
   "uninteresting" data such as bitmap cache.  */
void
copy_expr_onside (expr_t to, expr_t from)
{
  init_expr (to, EXPR_VINSN (from), EXPR_SPEC (from), EXPR_USEFULNESS (from),
	     EXPR_PRIORITY (from), EXPR_SCHED_TIMES (from), 0,
	     EXPR_SPEC_DONE_DS (from), EXPR_SPEC_TO_CHECK_DS (from), 0, NULL,
	     EXPR_TARGET_AVAILABLE (from), EXPR_WAS_SUBSTITUTED (from),
	     EXPR_WAS_RENAMED (from), EXPR_NEEDS_SPEC_CHECK_P (from),
             EXPR_CANT_MOVE (from));
}

/* Prepare the expr of INSN for scheduling.  Used when moving insn and when
   initializing new insns.  */
static void
prepare_insn_expr (insn_t insn, int seqno)
{
  expr_t expr = INSN_EXPR (insn);
  ds_t ds;

  INSN_SEQNO (insn) = seqno;
  EXPR_ORIG_BB_INDEX (expr) = BLOCK_NUM (insn);
  EXPR_SPEC (expr) = 0;
  EXPR_ORIG_SCHED_CYCLE (expr) = 0;
  EXPR_WAS_SUBSTITUTED (expr) = 0;
  EXPR_WAS_RENAMED (expr) = 0;
  EXPR_TARGET_AVAILABLE (expr) = 1;
  INSN_LIVE_VALID_P (insn) = false;

  /* ??? If this expression is speculative, make its dependence
     as weak as possible.  We can filter this expression later
     in process_spec_exprs, because we do not distinguish
     between the status we got during compute_av_set and the
     existing status.  To be fixed.  */
  ds = EXPR_SPEC_DONE_DS (expr);
  if (ds)
    EXPR_SPEC_DONE_DS (expr) = ds_get_max_dep_weak (ds);

  free_history_vect (&EXPR_HISTORY_OF_CHANGES (expr));
}

/* Update target_available bits when merging exprs TO and FROM.  SPLIT_POINT
   is non-null when expressions are merged from different successors at
   a split point.  */
static void
update_target_availability (expr_t to, expr_t from, insn_t split_point)
{
  if (EXPR_TARGET_AVAILABLE (to) < 0
      || EXPR_TARGET_AVAILABLE (from) < 0)
    EXPR_TARGET_AVAILABLE (to) = -1;
  else
    {
      /* We try to detect the case when one of the expressions
         can only be reached through another one.  In this case,
         we can do better.  */
      if (split_point == NULL)
        {
          int toind, fromind;

          toind = EXPR_ORIG_BB_INDEX (to);
          fromind = EXPR_ORIG_BB_INDEX (from);

          if (toind && toind == fromind)
            /* Do nothing -- everything is done in
               merge_with_other_exprs.  */
            ;
          else
            EXPR_TARGET_AVAILABLE (to) = -1;
        }
      else
        EXPR_TARGET_AVAILABLE (to) &= EXPR_TARGET_AVAILABLE (from);
    }
}

/* Update speculation bits when merging exprs TO and FROM.  SPLIT_POINT
   is non-null when expressions are merged from different successors at
   a split point.  */
static void
update_speculative_bits (expr_t to, expr_t from, insn_t split_point)
{
  ds_t old_to_ds, old_from_ds;

  old_to_ds = EXPR_SPEC_DONE_DS (to);
  old_from_ds = EXPR_SPEC_DONE_DS (from);

  EXPR_SPEC_DONE_DS (to) = ds_max_merge (old_to_ds, old_from_ds);
  EXPR_SPEC_TO_CHECK_DS (to) |= EXPR_SPEC_TO_CHECK_DS (from);
  EXPR_NEEDS_SPEC_CHECK_P (to) |= EXPR_NEEDS_SPEC_CHECK_P (from);

  /* When merging e.g. control & data speculative exprs, or a control
     speculative with a control&data speculative one, we really have
     to change vinsn too.  Also, when speculative status is changed,
     we also need to record this as a transformation in expr's history.  */
  if ((old_to_ds & SPECULATIVE) || (old_from_ds & SPECULATIVE))
    {
      old_to_ds = ds_get_speculation_types (old_to_ds);
      old_from_ds = ds_get_speculation_types (old_from_ds);

      if (old_to_ds != old_from_ds)
        {
          ds_t record_ds;

          /* When both expressions are speculative, we need to change
             the vinsn first.  */
          if ((old_to_ds & SPECULATIVE) && (old_from_ds & SPECULATIVE))
            {
              int res;

              res = speculate_expr (to, EXPR_SPEC_DONE_DS (to));
              gcc_assert (res >= 0);
            }

          if (split_point != NULL)
            {
              /* Record the change with proper status.  */
              record_ds = EXPR_SPEC_DONE_DS (to) & SPECULATIVE;
              record_ds &= ~(old_to_ds & SPECULATIVE);
              record_ds &= ~(old_from_ds & SPECULATIVE);

              insert_in_history_vect (&EXPR_HISTORY_OF_CHANGES (to),
                                      INSN_UID (split_point), TRANS_SPECULATION,
                                      EXPR_VINSN (from), EXPR_VINSN (to),
                                      record_ds);
            }
        }
    }
}


/* Merge bits of FROM expr to TO expr.  When SPLIT_POINT is not NULL,
   this is done along different paths.  */
void
merge_expr_data (expr_t to, expr_t from, insn_t split_point)
{
  int i;
  expr_history_def *phist;

  /* For now, we just set the spec of resulting expr to be minimum of the specs
     of merged exprs.  */
  if (EXPR_SPEC (to) > EXPR_SPEC (from))
    EXPR_SPEC (to) = EXPR_SPEC (from);

  if (split_point)
    EXPR_USEFULNESS (to) += EXPR_USEFULNESS (from);
  else
    EXPR_USEFULNESS (to) = MAX (EXPR_USEFULNESS (to),
                                EXPR_USEFULNESS (from));

  if (EXPR_PRIORITY (to) < EXPR_PRIORITY (from))
    EXPR_PRIORITY (to) = EXPR_PRIORITY (from);

  if (EXPR_SCHED_TIMES (to) > EXPR_SCHED_TIMES (from))
    EXPR_SCHED_TIMES (to) = EXPR_SCHED_TIMES (from);

  if (EXPR_ORIG_BB_INDEX (to) != EXPR_ORIG_BB_INDEX (from))
    EXPR_ORIG_BB_INDEX (to) = 0;

  EXPR_ORIG_SCHED_CYCLE (to) = MIN (EXPR_ORIG_SCHED_CYCLE (to),
                                    EXPR_ORIG_SCHED_CYCLE (from));

  /* We keep this vector sorted.  */
  for (i = 0;
       VEC_iterate (expr_history_def, EXPR_HISTORY_OF_CHANGES (from),
                    i, phist);
       i++)
    insert_in_history_vect (&EXPR_HISTORY_OF_CHANGES (to),
                            phist->uid, phist->type,
                            phist->old_expr_vinsn, phist->new_expr_vinsn,
                            phist->spec_ds);

  EXPR_WAS_SUBSTITUTED (to) |= EXPR_WAS_SUBSTITUTED (from);
  EXPR_WAS_RENAMED (to) |= EXPR_WAS_RENAMED (from);
  EXPR_CANT_MOVE (to) |= EXPR_CANT_MOVE (from);

  update_target_availability (to, from, split_point);
  update_speculative_bits (to, from, split_point);
}

/* Merge bits of FROM expr to TO expr.  Vinsns in the exprs should be equal
   in terms of vinsn_equal_p.  SPLIT_POINT is non-null when expressions
   are merged from different successors at a split point.  */
void
merge_expr (expr_t to, expr_t from, insn_t split_point)
{
  vinsn_t to_vi = EXPR_VINSN (to);
  vinsn_t from_vi = EXPR_VINSN (from);

  gcc_assert (vinsn_equal_p (to_vi, from_vi));

  /* Make sure that speculative pattern is propagated into exprs that
     have non-speculative one.  This will provide us with consistent
     speculative bits and speculative patterns inside expr.  */
  if (EXPR_SPEC_DONE_DS (to) == 0
      && EXPR_SPEC_DONE_DS (from) != 0)
    change_vinsn_in_expr (to, EXPR_VINSN (from));

  merge_expr_data (to, from, split_point);
  gcc_assert (EXPR_USEFULNESS (to) <= REG_BR_PROB_BASE);
}

/* Clear the information of this EXPR.  */
void
clear_expr (expr_t expr)
{

  vinsn_detach (EXPR_VINSN (expr));
  EXPR_VINSN (expr) = NULL;

  free_history_vect (&EXPR_HISTORY_OF_CHANGES (expr));
}

/* For a given LV_SET, mark EXPR having unavailable target register.  */
static void
set_unavailable_target_for_expr (expr_t expr, regset lv_set)
{
  if (EXPR_SEPARABLE_P (expr))
    {
      if (REG_P (EXPR_LHS (expr))
          && bitmap_bit_p (lv_set, REGNO (EXPR_LHS (expr))))
	{
	  /* If it's an insn like r1 = use (r1, ...), and it exists in
	     different forms in each of the av_sets being merged, we can't say
	     whether original destination register is available or not.
	     However, this still works if destination register is not used
	     in the original expression: if the branch at which LV_SET we're
	     looking here is not actually 'other branch' in sense that same
	     expression is available through it (but it can't be determined
	     at computation stage because of transformations on one of the
	     branches), it still won't affect the availability.
	     Liveness of a register somewhere on a code motion path means
	     it's either read somewhere on a codemotion path, live on
	     'other' branch, live at the point immediately following
	     the original operation, or is read by the original operation.
	     The latter case is filtered out in the condition below.
	     It still doesn't cover the case when register is defined and used
	     somewhere within the code motion path, and in this case we could
	     miss a unifying code motion along both branches using a renamed
	     register, but it won't affect a code correctness since upon
	     an actual code motion a bookkeeping code would be generated.  */
	  if (bitmap_bit_p (VINSN_REG_USES (EXPR_VINSN (expr)),
			    REGNO (EXPR_LHS (expr))))
	    EXPR_TARGET_AVAILABLE (expr) = -1;
	  else
	    EXPR_TARGET_AVAILABLE (expr) = false;
	}
    }
  else
    {
      unsigned regno;
      reg_set_iterator rsi;

      EXECUTE_IF_SET_IN_REG_SET (VINSN_REG_SETS (EXPR_VINSN (expr)),
                                 0, regno, rsi)
        if (bitmap_bit_p (lv_set, regno))
          {
            EXPR_TARGET_AVAILABLE (expr) = false;
            break;
          }

      EXECUTE_IF_SET_IN_REG_SET (VINSN_REG_CLOBBERS (EXPR_VINSN (expr)),
                                 0, regno, rsi)
        if (bitmap_bit_p (lv_set, regno))
          {
            EXPR_TARGET_AVAILABLE (expr) = false;
            break;
          }
    }
}

/* Try to make EXPR speculative.  Return 1 when EXPR's pattern
   or dependence status have changed, 2 when also the target register
   became unavailable, 0 if nothing had to be changed.  */
int
speculate_expr (expr_t expr, ds_t ds)
{
  int res;
  rtx orig_insn_rtx;
  rtx spec_pat;
  ds_t target_ds, current_ds;

  /* Obtain the status we need to put on EXPR.   */
  target_ds = (ds & SPECULATIVE);
  current_ds = EXPR_SPEC_DONE_DS (expr);
  ds = ds_full_merge (current_ds, target_ds, NULL_RTX, NULL_RTX);

  orig_insn_rtx = EXPR_INSN_RTX (expr);

  res = sched_speculate_insn (orig_insn_rtx, ds, &spec_pat);

  switch (res)
    {
    case 0:
      EXPR_SPEC_DONE_DS (expr) = ds;
      return current_ds != ds ? 1 : 0;

    case 1:
      {
	rtx spec_insn_rtx = create_insn_rtx_from_pattern (spec_pat, NULL_RTX);
	vinsn_t spec_vinsn = create_vinsn_from_insn_rtx (spec_insn_rtx, false);

	change_vinsn_in_expr (expr, spec_vinsn);
	EXPR_SPEC_DONE_DS (expr) = ds;
        EXPR_NEEDS_SPEC_CHECK_P (expr) = true;

        /* Do not allow clobbering the address register of speculative
           insns.  */
        if (bitmap_bit_p (VINSN_REG_USES (EXPR_VINSN (expr)),
                          expr_dest_regno (expr)))
          {
            EXPR_TARGET_AVAILABLE (expr) = false;
            return 2;
          }

	return 1;
      }

    case -1:
      return -1;

    default:
      gcc_unreachable ();
      return -1;
    }
}

/* Return a destination register, if any, of EXPR.  */
rtx
expr_dest_reg (expr_t expr)
{
  rtx dest = VINSN_LHS (EXPR_VINSN (expr));

  if (dest != NULL_RTX && REG_P (dest))
    return dest;

  return NULL_RTX;
}

/* Returns the REGNO of the R's destination.  */
unsigned
expr_dest_regno (expr_t expr)
{
  rtx dest = expr_dest_reg (expr);

  gcc_assert (dest != NULL_RTX);
  return REGNO (dest);
}

/* For a given LV_SET, mark all expressions in JOIN_SET, but not present in
   AV_SET having unavailable target register.  */
void
mark_unavailable_targets (av_set_t join_set, av_set_t av_set, regset lv_set)
{
  expr_t expr;
  av_set_iterator avi;

  FOR_EACH_EXPR (expr, avi, join_set)
    if (av_set_lookup (av_set, EXPR_VINSN (expr)) == NULL)
      set_unavailable_target_for_expr (expr, lv_set);
}


/* Av set functions.  */

/* Add a new element to av set SETP.
   Return the element added.  */
static av_set_t
av_set_add_element (av_set_t *setp)
{
  /* Insert at the beginning of the list.  */
  _list_add (setp);
  return *setp;
}

/* Add EXPR to SETP.  */
void
av_set_add (av_set_t *setp, expr_t expr)
{
  av_set_t elem;

  gcc_assert (!INSN_NOP_P (EXPR_INSN_RTX (expr)));
  elem = av_set_add_element (setp);
  copy_expr (_AV_SET_EXPR (elem), expr);
}

/* Same, but do not copy EXPR.  */
static void
av_set_add_nocopy (av_set_t *setp, expr_t expr)
{
  av_set_t elem;

  elem = av_set_add_element (setp);
  *_AV_SET_EXPR (elem) = *expr;
}

/* Remove expr pointed to by IP from the av_set.  */
void
av_set_iter_remove (av_set_iterator *ip)
{
  clear_expr (_AV_SET_EXPR (*ip->lp));
  _list_iter_remove (ip);
}

/* Search for an expr in SET, such that it's equivalent to SOUGHT_VINSN in the
   sense of vinsn_equal_p function. Return NULL if no such expr is
   in SET was found.  */
expr_t
av_set_lookup (av_set_t set, vinsn_t sought_vinsn)
{
  expr_t expr;
  av_set_iterator i;

  FOR_EACH_EXPR (expr, i, set)
    if (vinsn_equal_p (EXPR_VINSN (expr), sought_vinsn))
      return expr;
  return NULL;
}

/* Same, but also remove the EXPR found.   */
static expr_t
av_set_lookup_and_remove (av_set_t *setp, vinsn_t sought_vinsn)
{
  expr_t expr;
  av_set_iterator i;

  FOR_EACH_EXPR_1 (expr, i, setp)
    if (vinsn_equal_p (EXPR_VINSN (expr), sought_vinsn))
      {
        _list_iter_remove_nofree (&i);
        return expr;
      }
  return NULL;
}

/* Search for an expr in SET, such that it's equivalent to EXPR in the
   sense of vinsn_equal_p function of their vinsns, but not EXPR itself.
   Returns NULL if no such expr is in SET was found.  */
static expr_t
av_set_lookup_other_equiv_expr (av_set_t set, expr_t expr)
{
  expr_t cur_expr;
  av_set_iterator i;

  FOR_EACH_EXPR (cur_expr, i, set)
    {
      if (cur_expr == expr)
        continue;
      if (vinsn_equal_p (EXPR_VINSN (cur_expr), EXPR_VINSN (expr)))
        return cur_expr;
    }

  return NULL;
}

/* If other expression is already in AVP, remove one of them.  */
expr_t
merge_with_other_exprs (av_set_t *avp, av_set_iterator *ip, expr_t expr)
{
  expr_t expr2;

  expr2 = av_set_lookup_other_equiv_expr (*avp, expr);
  if (expr2 != NULL)
    {
      /* Reset target availability on merge, since taking it only from one
	 of the exprs would be controversial for different code.  */
      EXPR_TARGET_AVAILABLE (expr2) = -1;
      EXPR_USEFULNESS (expr2) = 0;

      merge_expr (expr2, expr, NULL);

      /* Fix usefulness as it should be now REG_BR_PROB_BASE.  */
      EXPR_USEFULNESS (expr2) = REG_BR_PROB_BASE;

      av_set_iter_remove (ip);
      return expr2;
    }

  return expr;
}

/* Return true if there is an expr that correlates to VI in SET.  */
bool
av_set_is_in_p (av_set_t set, vinsn_t vi)
{
  return av_set_lookup (set, vi) != NULL;
}

/* Return a copy of SET.  */
av_set_t
av_set_copy (av_set_t set)
{
  expr_t expr;
  av_set_iterator i;
  av_set_t res = NULL;

  FOR_EACH_EXPR (expr, i, set)
    av_set_add (&res, expr);

  return res;
}

/* Join two av sets that do not have common elements by attaching second set
   (pointed to by FROMP) to the end of first set (TO_TAILP must point to
   _AV_SET_NEXT of first set's last element).  */
static void
join_distinct_sets (av_set_t *to_tailp, av_set_t *fromp)
{
  gcc_assert (*to_tailp == NULL);
  *to_tailp = *fromp;
  *fromp = NULL;
}

/* Makes set pointed to by TO to be the union of TO and FROM.  Clear av_set
   pointed to by FROMP afterwards.  */
void
av_set_union_and_clear (av_set_t *top, av_set_t *fromp, insn_t insn)
{
  expr_t expr1;
  av_set_iterator i;

  /* Delete from TOP all exprs, that present in FROMP.  */
  FOR_EACH_EXPR_1 (expr1, i, top)
    {
      expr_t expr2 = av_set_lookup (*fromp, EXPR_VINSN (expr1));

      if (expr2)
	{
          merge_expr (expr2, expr1, insn);
	  av_set_iter_remove (&i);
	}
    }

  join_distinct_sets (i.lp, fromp);
}

/* Same as above, but also update availability of target register in
   TOP judging by TO_LV_SET and FROM_LV_SET.  */
void
av_set_union_and_live (av_set_t *top, av_set_t *fromp, regset to_lv_set,
                       regset from_lv_set, insn_t insn)
{
  expr_t expr1;
  av_set_iterator i;
  av_set_t *to_tailp, in_both_set = NULL;

  /* Delete from TOP all expres, that present in FROMP.  */
  FOR_EACH_EXPR_1 (expr1, i, top)
    {
      expr_t expr2 = av_set_lookup_and_remove (fromp, EXPR_VINSN (expr1));

      if (expr2)
	{
          /* It may be that the expressions have different destination
             registers, in which case we need to check liveness here.  */
          if (EXPR_SEPARABLE_P (expr1))
            {
              int regno1 = (REG_P (EXPR_LHS (expr1))
                            ? (int) expr_dest_regno (expr1) : -1);
              int regno2 = (REG_P (EXPR_LHS (expr2))
                            ? (int) expr_dest_regno (expr2) : -1);

              /* ??? We don't have a way to check restrictions for
               *other* register on the current path, we did it only
               for the current target register.  Give up.  */
              if (regno1 != regno2)
                EXPR_TARGET_AVAILABLE (expr2) = -1;
            }
          else if (EXPR_INSN_RTX (expr1) != EXPR_INSN_RTX (expr2))
            EXPR_TARGET_AVAILABLE (expr2) = -1;

          merge_expr (expr2, expr1, insn);
          av_set_add_nocopy (&in_both_set, expr2);
	  av_set_iter_remove (&i);
	}
      else
        /* EXPR1 is present in TOP, but not in FROMP.  Check it on
           FROM_LV_SET.  */
        set_unavailable_target_for_expr (expr1, from_lv_set);
    }
  to_tailp = i.lp;

  /* These expressions are not present in TOP.  Check liveness
     restrictions on TO_LV_SET.  */
  FOR_EACH_EXPR (expr1, i, *fromp)
    set_unavailable_target_for_expr (expr1, to_lv_set);

  join_distinct_sets (i.lp, &in_both_set);
  join_distinct_sets (to_tailp, fromp);
}

/* Clear av_set pointed to by SETP.  */
void
av_set_clear (av_set_t *setp)
{
  expr_t expr;
  av_set_iterator i;

  FOR_EACH_EXPR_1 (expr, i, setp)
    av_set_iter_remove (&i);

  gcc_assert (*setp == NULL);
}

/* Leave only one non-speculative element in the SETP.  */
void
av_set_leave_one_nonspec (av_set_t *setp)
{
  expr_t expr;
  av_set_iterator i;
  bool has_one_nonspec = false;

  /* Keep all speculative exprs, and leave one non-speculative
     (the first one).  */
  FOR_EACH_EXPR_1 (expr, i, setp)
    {
      if (!EXPR_SPEC_DONE_DS (expr))
	{
  	  if (has_one_nonspec)
	    av_set_iter_remove (&i);
	  else
	    has_one_nonspec = true;
	}
    }
}

/* Return the N'th element of the SET.  */
expr_t
av_set_element (av_set_t set, int n)
{
  expr_t expr;
  av_set_iterator i;

  FOR_EACH_EXPR (expr, i, set)
    if (n-- == 0)
      return expr;

  gcc_unreachable ();
  return NULL;
}

/* Deletes all expressions from AVP that are conditional branches (IFs).  */
void
av_set_substract_cond_branches (av_set_t *avp)
{
  av_set_iterator i;
  expr_t expr;

  FOR_EACH_EXPR_1 (expr, i, avp)
    if (vinsn_cond_branch_p (EXPR_VINSN (expr)))
      av_set_iter_remove (&i);
}

/* Multiplies usefulness attribute of each member of av-set *AVP by
   value PROB / ALL_PROB.  */
void
av_set_split_usefulness (av_set_t av, int prob, int all_prob)
{
  av_set_iterator i;
  expr_t expr;

  FOR_EACH_EXPR (expr, i, av)
    EXPR_USEFULNESS (expr) = (all_prob
                              ? (EXPR_USEFULNESS (expr) * prob) / all_prob
                              : 0);
}

/* Leave in AVP only those expressions, which are present in AV,
   and return it.  */
void
av_set_intersect (av_set_t *avp, av_set_t av)
{
  av_set_iterator i;
  expr_t expr;

  FOR_EACH_EXPR_1 (expr, i, avp)
    if (av_set_lookup (av, EXPR_VINSN (expr)) == NULL)
      av_set_iter_remove (&i);
}



/* Dependence hooks to initialize insn data.  */

/* This is used in hooks callable from dependence analysis when initializing
   instruction's data.  */
static struct
{
  /* Where the dependence was found (lhs/rhs).  */
  deps_where_t where;

  /* The actual data object to initialize.  */
  idata_t id;

  /* True when the insn should not be made clonable.  */
  bool force_unique_p;

  /* True when insn should be treated as of type USE, i.e. never renamed.  */
  bool force_use_p;
} deps_init_id_data;


/* Setup ID for INSN.  FORCE_UNIQUE_P is true when INSN should not be
   clonable.  */
static void
setup_id_for_insn (idata_t id, insn_t insn, bool force_unique_p)
{
  int type;

  /* Determine whether INSN could be cloned and return appropriate vinsn type.
     That clonable insns which can be separated into lhs and rhs have type SET.
     Other clonable insns have type USE.  */
  type = GET_CODE (insn);

  /* Only regular insns could be cloned.  */
  if (type == INSN && !force_unique_p)
    type = SET;
  else if (type == JUMP_INSN && simplejump_p (insn))
    type = PC;
  else if (type == DEBUG_INSN)
    type = !force_unique_p ? USE : INSN;

  IDATA_TYPE (id) = type;
  IDATA_REG_SETS (id) = get_clear_regset_from_pool ();
  IDATA_REG_USES (id) = get_clear_regset_from_pool ();
  IDATA_REG_CLOBBERS (id) = get_clear_regset_from_pool ();
}

/* Start initializing insn data.  */
static void
deps_init_id_start_insn (insn_t insn)
{
  gcc_assert (deps_init_id_data.where == DEPS_IN_NOWHERE);

  setup_id_for_insn (deps_init_id_data.id, insn,
                     deps_init_id_data.force_unique_p);
  deps_init_id_data.where = DEPS_IN_INSN;
}

/* Start initializing lhs data.  */
static void
deps_init_id_start_lhs (rtx lhs)
{
  gcc_assert (deps_init_id_data.where == DEPS_IN_INSN);
  gcc_assert (IDATA_LHS (deps_init_id_data.id) == NULL);

  if (IDATA_TYPE (deps_init_id_data.id) == SET)
    {
      IDATA_LHS (deps_init_id_data.id) = lhs;
      deps_init_id_data.where = DEPS_IN_LHS;
    }
}

/* Finish initializing lhs data.  */
static void
deps_init_id_finish_lhs (void)
{
  deps_init_id_data.where = DEPS_IN_INSN;
}

/* Note a set of REGNO.  */
static void
deps_init_id_note_reg_set (int regno)
{
  haifa_note_reg_set (regno);

  if (deps_init_id_data.where == DEPS_IN_RHS)
    deps_init_id_data.force_use_p = true;

  if (IDATA_TYPE (deps_init_id_data.id) != PC)
    SET_REGNO_REG_SET (IDATA_REG_SETS (deps_init_id_data.id), regno);

#ifdef STACK_REGS
  /* Make instructions that set stack registers to be ineligible for
     renaming to avoid issues with find_used_regs.  */
  if (IN_RANGE (regno, FIRST_STACK_REG, LAST_STACK_REG))
    deps_init_id_data.force_use_p = true;
#endif
}

/* Note a clobber of REGNO.  */
static void
deps_init_id_note_reg_clobber (int regno)
{
  haifa_note_reg_clobber (regno);

  if (deps_init_id_data.where == DEPS_IN_RHS)
    deps_init_id_data.force_use_p = true;

  if (IDATA_TYPE (deps_init_id_data.id) != PC)
    SET_REGNO_REG_SET (IDATA_REG_CLOBBERS (deps_init_id_data.id), regno);
}

/* Note a use of REGNO.  */
static void
deps_init_id_note_reg_use (int regno)
{
  haifa_note_reg_use (regno);

  if (IDATA_TYPE (deps_init_id_data.id) != PC)
    SET_REGNO_REG_SET (IDATA_REG_USES (deps_init_id_data.id), regno);
}

/* Start initializing rhs data.  */
static void
deps_init_id_start_rhs (rtx rhs)
{
  gcc_assert (deps_init_id_data.where == DEPS_IN_INSN);

  /* And there was no sel_deps_reset_to_insn ().  */
  if (IDATA_LHS (deps_init_id_data.id) != NULL)
    {
      IDATA_RHS (deps_init_id_data.id) = rhs;
      deps_init_id_data.where = DEPS_IN_RHS;
    }
}

/* Finish initializing rhs data.  */
static void
deps_init_id_finish_rhs (void)
{
  gcc_assert (deps_init_id_data.where == DEPS_IN_RHS
	      || deps_init_id_data.where == DEPS_IN_INSN);
  deps_init_id_data.where = DEPS_IN_INSN;
}

/* Finish initializing insn data.  */
static void
deps_init_id_finish_insn (void)
{
  gcc_assert (deps_init_id_data.where == DEPS_IN_INSN);

  if (IDATA_TYPE (deps_init_id_data.id) == SET)
    {
      rtx lhs = IDATA_LHS (deps_init_id_data.id);
      rtx rhs = IDATA_RHS (deps_init_id_data.id);

      if (lhs == NULL || rhs == NULL || !lhs_and_rhs_separable_p (lhs, rhs)
	  || deps_init_id_data.force_use_p)
	{
          /* This should be a USE, as we don't want to schedule its RHS
             separately.  However, we still want to have them recorded
             for the purposes of substitution.  That's why we don't
             simply call downgrade_to_use () here.  */
	  gcc_assert (IDATA_TYPE (deps_init_id_data.id) == SET);
	  gcc_assert (!lhs == !rhs);

	  IDATA_TYPE (deps_init_id_data.id) = USE;
	}
    }

  deps_init_id_data.where = DEPS_IN_NOWHERE;
}

/* This is dependence info used for initializing insn's data.  */
static struct sched_deps_info_def deps_init_id_sched_deps_info;

/* This initializes most of the static part of the above structure.  */
static const struct sched_deps_info_def const_deps_init_id_sched_deps_info =
  {
    NULL,

    deps_init_id_start_insn,
    deps_init_id_finish_insn,
    deps_init_id_start_lhs,
    deps_init_id_finish_lhs,
    deps_init_id_start_rhs,
    deps_init_id_finish_rhs,
    deps_init_id_note_reg_set,
    deps_init_id_note_reg_clobber,
    deps_init_id_note_reg_use,
    NULL, /* note_mem_dep */
    NULL, /* note_dep */

    0, /* use_cselib */
    0, /* use_deps_list */
    0 /* generate_spec_deps */
  };

/* Initialize INSN's lhs and rhs in ID.  When FORCE_UNIQUE_P is true,
   we don't actually need information about lhs and rhs.  */
static void
setup_id_lhs_rhs (idata_t id, insn_t insn, bool force_unique_p)
{
  rtx pat = PATTERN (insn);

  if (NONJUMP_INSN_P (insn)
      && GET_CODE (pat) == SET
      && !force_unique_p)
    {
      IDATA_RHS (id) = SET_SRC (pat);
      IDATA_LHS (id) = SET_DEST (pat);
    }
  else
    IDATA_LHS (id) = IDATA_RHS (id) = NULL;
}

/* Possibly downgrade INSN to USE.  */
static void
maybe_downgrade_id_to_use (idata_t id, insn_t insn)
{
  bool must_be_use = false;
  unsigned uid = INSN_UID (insn);
  df_ref *rec;
  rtx lhs = IDATA_LHS (id);
  rtx rhs = IDATA_RHS (id);

  /* We downgrade only SETs.  */
  if (IDATA_TYPE (id) != SET)
    return;

  if (!lhs || !lhs_and_rhs_separable_p (lhs, rhs))
    {
      IDATA_TYPE (id) = USE;
      return;
    }

  for (rec = DF_INSN_UID_DEFS (uid); *rec; rec++)
    {
      df_ref def = *rec;

      if (DF_REF_INSN (def)
          && DF_REF_FLAGS_IS_SET (def, DF_REF_PRE_POST_MODIFY)
          && loc_mentioned_in_p (DF_REF_LOC (def), IDATA_RHS (id)))
        {
          must_be_use = true;
          break;
        }

#ifdef STACK_REGS
      /* Make instructions that set stack registers to be ineligible for
	 renaming to avoid issues with find_used_regs.  */
      if (IN_RANGE (DF_REF_REGNO (def), FIRST_STACK_REG, LAST_STACK_REG))
	{
	  must_be_use = true;
	  break;
	}
#endif
    }

  if (must_be_use)
    IDATA_TYPE (id) = USE;
}

/* Setup register sets describing INSN in ID.  */
static void
setup_id_reg_sets (idata_t id, insn_t insn)
{
  unsigned uid = INSN_UID (insn);
  df_ref *rec;
  regset tmp = get_clear_regset_from_pool ();

  for (rec = DF_INSN_UID_DEFS (uid); *rec; rec++)
    {
      df_ref def = *rec;
      unsigned int regno = DF_REF_REGNO (def);

      /* Post modifies are treated like clobbers by sched-deps.c.  */
      if (DF_REF_FLAGS_IS_SET (def, (DF_REF_MUST_CLOBBER
                                     | DF_REF_PRE_POST_MODIFY)))
        SET_REGNO_REG_SET (IDATA_REG_CLOBBERS (id), regno);
      else if (! DF_REF_FLAGS_IS_SET (def, DF_REF_MAY_CLOBBER))
        {
	  SET_REGNO_REG_SET (IDATA_REG_SETS (id), regno);

#ifdef STACK_REGS
	  /* For stack registers, treat writes to them as writes
	     to the first one to be consistent with sched-deps.c.  */
	  if (IN_RANGE (regno, FIRST_STACK_REG, LAST_STACK_REG))
	    SET_REGNO_REG_SET (IDATA_REG_SETS (id), FIRST_STACK_REG);
#endif
	}
      /* Mark special refs that generate read/write def pair.  */
      if (DF_REF_FLAGS_IS_SET (def, DF_REF_CONDITIONAL)
          || regno == STACK_POINTER_REGNUM)
        bitmap_set_bit (tmp, regno);
    }

  for (rec = DF_INSN_UID_USES (uid); *rec; rec++)
    {
      df_ref use = *rec;
      unsigned int regno = DF_REF_REGNO (use);

      /* When these refs are met for the first time, skip them, as
         these uses are just counterparts of some defs.  */
      if (bitmap_bit_p (tmp, regno))
        bitmap_clear_bit (tmp, regno);
      else if (! DF_REF_FLAGS_IS_SET (use, DF_REF_CALL_STACK_USAGE))
	{
	  SET_REGNO_REG_SET (IDATA_REG_USES (id), regno);

#ifdef STACK_REGS
	  /* For stack registers, treat reads from them as reads from
	     the first one to be consistent with sched-deps.c.  */
	  if (IN_RANGE (regno, FIRST_STACK_REG, LAST_STACK_REG))
	    SET_REGNO_REG_SET (IDATA_REG_USES (id), FIRST_STACK_REG);
#endif
	}
    }

  return_regset_to_pool (tmp);
}

/* Initialize instruction data for INSN in ID using DF's data.  */
static void
init_id_from_df (idata_t id, insn_t insn, bool force_unique_p)
{
  gcc_assert (DF_INSN_UID_SAFE_GET (INSN_UID (insn)) != NULL);

  setup_id_for_insn (id, insn, force_unique_p);
  setup_id_lhs_rhs (id, insn, force_unique_p);

  if (INSN_NOP_P (insn))
    return;

  maybe_downgrade_id_to_use (id, insn);
  setup_id_reg_sets (id, insn);
}

/* Initialize instruction data for INSN in ID.  */
static void
deps_init_id (idata_t id, insn_t insn, bool force_unique_p)
{
  struct deps _dc, *dc = &_dc;

  deps_init_id_data.where = DEPS_IN_NOWHERE;
  deps_init_id_data.id = id;
  deps_init_id_data.force_unique_p = force_unique_p;
  deps_init_id_data.force_use_p = false;

  init_deps (dc, false);

  memcpy (&deps_init_id_sched_deps_info,
	  &const_deps_init_id_sched_deps_info,
	  sizeof (deps_init_id_sched_deps_info));

  if (spec_info != NULL)
    deps_init_id_sched_deps_info.generate_spec_deps = 1;

  sched_deps_info = &deps_init_id_sched_deps_info;

  deps_analyze_insn (dc, insn);

  free_deps (dc);

  deps_init_id_data.id = NULL;
}



/* Implement hooks for collecting fundamental insn properties like if insn is
   an ASM or is within a SCHED_GROUP.  */

/* True when a "one-time init" data for INSN was already inited.  */
static bool
first_time_insn_init (insn_t insn)
{
  return INSN_LIVE (insn) == NULL;
}

/* Hash an entry in a transformed_insns hashtable.  */
static hashval_t
hash_transformed_insns (const void *p)
{
  return VINSN_HASH_RTX (((const struct transformed_insns *) p)->vinsn_old);
}

/* Compare the entries in a transformed_insns hashtable.  */
static int
eq_transformed_insns (const void *p, const void *q)
{
  rtx i1 = VINSN_INSN_RTX (((const struct transformed_insns *) p)->vinsn_old);
  rtx i2 = VINSN_INSN_RTX (((const struct transformed_insns *) q)->vinsn_old);

  if (INSN_UID (i1) == INSN_UID (i2))
    return 1;
  return rtx_equal_p (PATTERN (i1), PATTERN (i2));
}

/* Free an entry in a transformed_insns hashtable.  */
static void
free_transformed_insns (void *p)
{
  struct transformed_insns *pti = (struct transformed_insns *) p;

  vinsn_detach (pti->vinsn_old);
  vinsn_detach (pti->vinsn_new);
  free (pti);
}

/* Init the s_i_d data for INSN which should be inited just once, when
   we first see the insn.  */
static void
init_first_time_insn_data (insn_t insn)
{
  /* This should not be set if this is the first time we init data for
     insn.  */
  gcc_assert (first_time_insn_init (insn));

  /* These are needed for nops too.  */
  INSN_LIVE (insn) = get_regset_from_pool ();
  INSN_LIVE_VALID_P (insn) = false;

  if (!INSN_NOP_P (insn))
    {
      INSN_ANALYZED_DEPS (insn) = BITMAP_ALLOC (NULL);
      INSN_FOUND_DEPS (insn) = BITMAP_ALLOC (NULL);
      INSN_TRANSFORMED_INSNS (insn)
        = htab_create (16, hash_transformed_insns,
                       eq_transformed_insns, free_transformed_insns);
      init_deps (&INSN_DEPS_CONTEXT (insn), true);
    }
}

/* Free almost all above data for INSN that is scheduled already.
   Used for extra-large basic blocks.  */
void
free_data_for_scheduled_insn (insn_t insn)
{
  gcc_assert (! first_time_insn_init (insn));

  if (! INSN_ANALYZED_DEPS (insn))
    return;

  BITMAP_FREE (INSN_ANALYZED_DEPS (insn));
  BITMAP_FREE (INSN_FOUND_DEPS (insn));
  htab_delete (INSN_TRANSFORMED_INSNS (insn));

  /* This is allocated only for bookkeeping insns.  */
  if (INSN_ORIGINATORS (insn))
    BITMAP_FREE (INSN_ORIGINATORS (insn));
  free_deps (&INSN_DEPS_CONTEXT (insn));

  INSN_ANALYZED_DEPS (insn) = NULL;

  /* Clear the readonly flag so we would ICE when trying to recalculate
     the deps context (as we believe that it should not happen).  */
  (&INSN_DEPS_CONTEXT (insn))->readonly = 0;
}

/* Free the same data as above for INSN.  */
static void
free_first_time_insn_data (insn_t insn)
{
  gcc_assert (! first_time_insn_init (insn));

  free_data_for_scheduled_insn (insn);
  return_regset_to_pool (INSN_LIVE (insn));
  INSN_LIVE (insn) = NULL;
  INSN_LIVE_VALID_P (insn) = false;
}

/* Initialize region-scope data structures for basic blocks.  */
static void
init_global_and_expr_for_bb (basic_block bb)
{
  if (sel_bb_empty_p (bb))
    return;

  invalidate_av_set (bb);
}

/* Data for global dependency analysis (to initialize CANT_MOVE and
   SCHED_GROUP_P).  */
static struct
{
  /* Previous insn.  */
  insn_t prev_insn;
} init_global_data;

/* Determine if INSN is in the sched_group, is an asm or should not be
   cloned.  After that initialize its expr.  */
static void
init_global_and_expr_for_insn (insn_t insn)
{
  if (LABEL_P (insn))
    return;

  if (NOTE_INSN_BASIC_BLOCK_P (insn))
    {
      init_global_data.prev_insn = NULL_RTX;
      return;
    }

  gcc_assert (INSN_P (insn));

  if (SCHED_GROUP_P (insn))
    /* Setup a sched_group.  */
    {
      insn_t prev_insn = init_global_data.prev_insn;

      if (prev_insn)
	INSN_SCHED_NEXT (prev_insn) = insn;

      init_global_data.prev_insn = insn;
    }
  else
    init_global_data.prev_insn = NULL_RTX;

  if (GET_CODE (PATTERN (insn)) == ASM_INPUT
      || asm_noperands (PATTERN (insn)) >= 0)
    /* Mark INSN as an asm.  */
    INSN_ASM_P (insn) = true;

  {
    bool force_unique_p;
    ds_t spec_done_ds;

    /* Certain instructions cannot be cloned.  */
    if (CANT_MOVE (insn)
	|| INSN_ASM_P (insn)
	|| SCHED_GROUP_P (insn)
	|| prologue_epilogue_contains (insn)
	/* Exception handling insns are always unique.  */
	|| (flag_non_call_exceptions && can_throw_internal (insn))
	/* TRAP_IF though have an INSN code is control_flow_insn_p ().  */
	|| control_flow_insn_p (insn))
      force_unique_p = true;
    else
      force_unique_p = false;

    if (targetm.sched.get_insn_spec_ds)
      {
	spec_done_ds = targetm.sched.get_insn_spec_ds (insn);
	spec_done_ds = ds_get_max_dep_weak (spec_done_ds);
      }
    else
      spec_done_ds = 0;

    /* Initialize INSN's expr.  */
    init_expr (INSN_EXPR (insn), vinsn_create (insn, force_unique_p), 0,
	       REG_BR_PROB_BASE, INSN_PRIORITY (insn), 0, BLOCK_NUM (insn),
	       spec_done_ds, 0, 0, NULL, true, false, false, false,
               CANT_MOVE (insn));
  }

  init_first_time_insn_data (insn);
}

/* Scan the region and initialize instruction data for basic blocks BBS.  */
void
sel_init_global_and_expr (bb_vec_t bbs)
{
  /* ??? It would be nice to implement push / pop scheme for sched_infos.  */
  const struct sched_scan_info_def ssi =
    {
      NULL, /* extend_bb */
      init_global_and_expr_for_bb, /* init_bb */
      extend_insn_data, /* extend_insn */
      init_global_and_expr_for_insn /* init_insn */
    };

  sched_scan (&ssi, bbs, NULL, NULL, NULL);
}

/* Finalize region-scope data structures for basic blocks.  */
static void
finish_global_and_expr_for_bb (basic_block bb)
{
  av_set_clear (&BB_AV_SET (bb));
  BB_AV_LEVEL (bb) = 0;
}

/* Finalize INSN's data.  */
static void
finish_global_and_expr_insn (insn_t insn)
{
  if (LABEL_P (insn) || NOTE_INSN_BASIC_BLOCK_P (insn))
    return;

  gcc_assert (INSN_P (insn));

  if (INSN_LUID (insn) > 0)
    {
      free_first_time_insn_data (insn);
      INSN_WS_LEVEL (insn) = 0;
      CANT_MOVE (insn) = 0;

      /* We can no longer assert this, as vinsns of this insn could be
         easily live in other insn's caches.  This should be changed to
         a counter-like approach among all vinsns.  */
      gcc_assert (true || VINSN_COUNT (INSN_VINSN (insn)) == 1);
      clear_expr (INSN_EXPR (insn));
    }
}

/* Finalize per instruction data for the whole region.  */
void
sel_finish_global_and_expr (void)
{
  {
    bb_vec_t bbs;
    int i;

    bbs = VEC_alloc (basic_block, heap, current_nr_blocks);

    for (i = 0; i < current_nr_blocks; i++)
      VEC_quick_push (basic_block, bbs, BASIC_BLOCK (BB_TO_BLOCK (i)));

    /* Clear AV_SETs and INSN_EXPRs.  */
    {
      const struct sched_scan_info_def ssi =
	{
	  NULL, /* extend_bb */
	  finish_global_and_expr_for_bb, /* init_bb */
	  NULL, /* extend_insn */
	  finish_global_and_expr_insn /* init_insn */
	};

      sched_scan (&ssi, bbs, NULL, NULL, NULL);
    }

    VEC_free (basic_block, heap, bbs);
  }

  finish_insns ();
}


/* In the below hooks, we merely calculate whether or not a dependence
   exists, and in what part of insn.  However, we will need more data
   when we'll start caching dependence requests.  */

/* Container to hold information for dependency analysis.  */
static struct
{
  deps_t dc;

  /* A variable to track which part of rtx we are scanning in
     sched-deps.c: sched_analyze_insn ().  */
  deps_where_t where;

  /* Current producer.  */
  insn_t pro;

  /* Current consumer.  */
  vinsn_t con;

  /* Is SEL_DEPS_HAS_DEP_P[DEPS_IN_X] is true, then X has a dependence.
     X is from { INSN, LHS, RHS }.  */
  ds_t has_dep_p[DEPS_IN_NOWHERE];
} has_dependence_data;

/* Start analyzing dependencies of INSN.  */
static void
has_dependence_start_insn (insn_t insn ATTRIBUTE_UNUSED)
{
  gcc_assert (has_dependence_data.where == DEPS_IN_NOWHERE);

  has_dependence_data.where = DEPS_IN_INSN;
}

/* Finish analyzing dependencies of an insn.  */
static void
has_dependence_finish_insn (void)
{
  gcc_assert (has_dependence_data.where == DEPS_IN_INSN);

  has_dependence_data.where = DEPS_IN_NOWHERE;
}

/* Start analyzing dependencies of LHS.  */
static void
has_dependence_start_lhs (rtx lhs ATTRIBUTE_UNUSED)
{
  gcc_assert (has_dependence_data.where == DEPS_IN_INSN);

  if (VINSN_LHS (has_dependence_data.con) != NULL)
    has_dependence_data.where = DEPS_IN_LHS;
}

/* Finish analyzing dependencies of an lhs.  */
static void
has_dependence_finish_lhs (void)
{
  has_dependence_data.where = DEPS_IN_INSN;
}

/* Start analyzing dependencies of RHS.  */
static void
has_dependence_start_rhs (rtx rhs ATTRIBUTE_UNUSED)
{
  gcc_assert (has_dependence_data.where == DEPS_IN_INSN);

  if (VINSN_RHS (has_dependence_data.con) != NULL)
    has_dependence_data.where = DEPS_IN_RHS;
}

/* Start analyzing dependencies of an rhs.  */
static void
has_dependence_finish_rhs (void)
{
  gcc_assert (has_dependence_data.where == DEPS_IN_RHS
	      || has_dependence_data.where == DEPS_IN_INSN);

  has_dependence_data.where = DEPS_IN_INSN;
}

/* Note a set of REGNO.  */
static void
has_dependence_note_reg_set (int regno)
{
  struct deps_reg *reg_last = &has_dependence_data.dc->reg_last[regno];

  if (!sched_insns_conditions_mutex_p (has_dependence_data.pro,
				       VINSN_INSN_RTX
				       (has_dependence_data.con)))
    {
      ds_t *dsp = &has_dependence_data.has_dep_p[has_dependence_data.where];

      if (reg_last->sets != NULL
	  || reg_last->clobbers != NULL)
	*dsp = (*dsp & ~SPECULATIVE) | DEP_OUTPUT;

      if (reg_last->uses)
	*dsp = (*dsp & ~SPECULATIVE) | DEP_ANTI;
    }
}

/* Note a clobber of REGNO.  */
static void
has_dependence_note_reg_clobber (int regno)
{
  struct deps_reg *reg_last = &has_dependence_data.dc->reg_last[regno];

  if (!sched_insns_conditions_mutex_p (has_dependence_data.pro,
				       VINSN_INSN_RTX
				       (has_dependence_data.con)))
    {
      ds_t *dsp = &has_dependence_data.has_dep_p[has_dependence_data.where];

      if (reg_last->sets)
	*dsp = (*dsp & ~SPECULATIVE) | DEP_OUTPUT;

      if (reg_last->uses)
	*dsp = (*dsp & ~SPECULATIVE) | DEP_ANTI;
    }
}

/* Note a use of REGNO.  */
static void
has_dependence_note_reg_use (int regno)
{
  struct deps_reg *reg_last = &has_dependence_data.dc->reg_last[regno];

  if (!sched_insns_conditions_mutex_p (has_dependence_data.pro,
				       VINSN_INSN_RTX
				       (has_dependence_data.con)))
    {
      ds_t *dsp = &has_dependence_data.has_dep_p[has_dependence_data.where];

      if (reg_last->sets)
	*dsp = (*dsp & ~SPECULATIVE) | DEP_TRUE;

      if (reg_last->clobbers)
	*dsp = (*dsp & ~SPECULATIVE) | DEP_ANTI;

      /* Handle BE_IN_SPEC.  */
      if (reg_last->uses)
	{
	  ds_t pro_spec_checked_ds;

	  pro_spec_checked_ds = INSN_SPEC_CHECKED_DS (has_dependence_data.pro);
	  pro_spec_checked_ds = ds_get_max_dep_weak (pro_spec_checked_ds);

	  if (pro_spec_checked_ds != 0)
	    /* Merge BE_IN_SPEC bits into *DSP.  */
	    *dsp = ds_full_merge (*dsp, pro_spec_checked_ds,
				  NULL_RTX, NULL_RTX);
	}
    }
}

/* Note a memory dependence.  */
static void
has_dependence_note_mem_dep (rtx mem ATTRIBUTE_UNUSED,
			     rtx pending_mem ATTRIBUTE_UNUSED,
			     insn_t pending_insn ATTRIBUTE_UNUSED,
			     ds_t ds ATTRIBUTE_UNUSED)
{
  if (!sched_insns_conditions_mutex_p (has_dependence_data.pro,
				       VINSN_INSN_RTX (has_dependence_data.con)))
    {
      ds_t *dsp = &has_dependence_data.has_dep_p[has_dependence_data.where];

      *dsp = ds_full_merge (ds, *dsp, pending_mem, mem);
    }
}

/* Note a dependence.  */
static void
has_dependence_note_dep (insn_t pro ATTRIBUTE_UNUSED,
			 ds_t ds ATTRIBUTE_UNUSED)
{
  if (!sched_insns_conditions_mutex_p (has_dependence_data.pro,
				       VINSN_INSN_RTX (has_dependence_data.con)))
    {
      ds_t *dsp = &has_dependence_data.has_dep_p[has_dependence_data.where];

      *dsp = ds_full_merge (ds, *dsp, NULL_RTX, NULL_RTX);
    }
}

/* Mark the insn as having a hard dependence that prevents speculation.  */
void
sel_mark_hard_insn (rtx insn)
{
  int i;

  /* Only work when we're in has_dependence_p mode.
     ??? This is a hack, this should actually be a hook.  */
  if (!has_dependence_data.dc || !has_dependence_data.pro)
    return;

  gcc_assert (insn == VINSN_INSN_RTX (has_dependence_data.con));
  gcc_assert (has_dependence_data.where == DEPS_IN_INSN);

  for (i = 0; i < DEPS_IN_NOWHERE; i++)
    has_dependence_data.has_dep_p[i] &= ~SPECULATIVE;
}

/* This structure holds the hooks for the dependency analysis used when
   actually processing dependencies in the scheduler.  */
static struct sched_deps_info_def has_dependence_sched_deps_info;

/* This initializes most of the fields of the above structure.  */
static const struct sched_deps_info_def const_has_dependence_sched_deps_info =
  {
    NULL,

    has_dependence_start_insn,
    has_dependence_finish_insn,
    has_dependence_start_lhs,
    has_dependence_finish_lhs,
    has_dependence_start_rhs,
    has_dependence_finish_rhs,
    has_dependence_note_reg_set,
    has_dependence_note_reg_clobber,
    has_dependence_note_reg_use,
    has_dependence_note_mem_dep,
    has_dependence_note_dep,

    0, /* use_cselib */
    0, /* use_deps_list */
    0 /* generate_spec_deps */
  };

/* Initialize has_dependence_sched_deps_info with extra spec field.  */
static void
setup_has_dependence_sched_deps_info (void)
{
  memcpy (&has_dependence_sched_deps_info,
	  &const_has_dependence_sched_deps_info,
	  sizeof (has_dependence_sched_deps_info));

  if (spec_info != NULL)
    has_dependence_sched_deps_info.generate_spec_deps = 1;

  sched_deps_info = &has_dependence_sched_deps_info;
}

/* Remove all dependences found and recorded in has_dependence_data array.  */
void
sel_clear_has_dependence (void)
{
  int i;

  for (i = 0; i < DEPS_IN_NOWHERE; i++)
    has_dependence_data.has_dep_p[i] = 0;
}

/* Return nonzero if EXPR has is dependent upon PRED.  Return the pointer
   to the dependence information array in HAS_DEP_PP.  */
ds_t
has_dependence_p (expr_t expr, insn_t pred, ds_t **has_dep_pp)
{
  int i;
  ds_t ds;
  struct deps *dc;

  if (INSN_SIMPLEJUMP_P (pred))
    /* Unconditional jump is just a transfer of control flow.
       Ignore it.  */
    return false;

  dc = &INSN_DEPS_CONTEXT (pred);

  /* We init this field lazily.  */
  if (dc->reg_last == NULL)
    init_deps_reg_last (dc);

  if (!dc->readonly)
    {
      has_dependence_data.pro = NULL;
      /* Initialize empty dep context with information about PRED.  */
      advance_deps_context (dc, pred);
      dc->readonly = 1;
    }

  has_dependence_data.where = DEPS_IN_NOWHERE;
  has_dependence_data.pro = pred;
  has_dependence_data.con = EXPR_VINSN (expr);
  has_dependence_data.dc = dc;

  sel_clear_has_dependence ();

  /* Now catch all dependencies that would be generated between PRED and
     INSN.  */
  setup_has_dependence_sched_deps_info ();
  deps_analyze_insn (dc, EXPR_INSN_RTX (expr));
  has_dependence_data.dc = NULL;

  /* When a barrier was found, set DEPS_IN_INSN bits.  */
  if (dc->last_reg_pending_barrier == TRUE_BARRIER)
    has_dependence_data.has_dep_p[DEPS_IN_INSN] = DEP_TRUE;
  else if (dc->last_reg_pending_barrier == MOVE_BARRIER)
    has_dependence_data.has_dep_p[DEPS_IN_INSN] = DEP_ANTI;

  /* Do not allow stores to memory to move through checks.  Currently
     we don't move this to sched-deps.c as the check doesn't have
     obvious places to which this dependence can be attached.
     FIMXE: this should go to a hook.  */
  if (EXPR_LHS (expr)
      && MEM_P (EXPR_LHS (expr))
      && sel_insn_is_speculation_check (pred))
    has_dependence_data.has_dep_p[DEPS_IN_INSN] = DEP_ANTI;

  *has_dep_pp = has_dependence_data.has_dep_p;
  ds = 0;
  for (i = 0; i < DEPS_IN_NOWHERE; i++)
    ds = ds_full_merge (ds, has_dependence_data.has_dep_p[i],
			NULL_RTX, NULL_RTX);

  return ds;
}


/* Dependence hooks implementation that checks dependence latency constraints
   on the insns being scheduled.  The entry point for these routines is
   tick_check_p predicate.  */

static struct
{
  /* An expr we are currently checking.  */
  expr_t expr;

  /* A minimal cycle for its scheduling.  */
  int cycle;

  /* Whether we have seen a true dependence while checking.  */
  bool seen_true_dep_p;
} tick_check_data;

/* Update minimal scheduling cycle for tick_check_insn given that it depends
   on PRO with status DS and weight DW.  */
static void
tick_check_dep_with_dw (insn_t pro_insn, ds_t ds, dw_t dw)
{
  expr_t con_expr = tick_check_data.expr;
  insn_t con_insn = EXPR_INSN_RTX (con_expr);

  if (con_insn != pro_insn)
    {
      enum reg_note dt;
      int tick;

      if (/* PROducer was removed from above due to pipelining.  */
	  !INSN_IN_STREAM_P (pro_insn)
	  /* Or PROducer was originally on the next iteration regarding the
	     CONsumer.  */
	  || (INSN_SCHED_TIMES (pro_insn)
	      - EXPR_SCHED_TIMES (con_expr)) > 1)
	/* Don't count this dependence.  */
        return;

      dt = ds_to_dt (ds);
      if (dt == REG_DEP_TRUE)
        tick_check_data.seen_true_dep_p = true;

      gcc_assert (INSN_SCHED_CYCLE (pro_insn) > 0);

      {
	dep_def _dep, *dep = &_dep;

	init_dep (dep, pro_insn, con_insn, dt);

	tick = INSN_SCHED_CYCLE (pro_insn) + dep_cost_1 (dep, dw);
      }

      /* When there are several kinds of dependencies between pro and con,
         only REG_DEP_TRUE should be taken into account.  */
      if (tick > tick_check_data.cycle
	  && (dt == REG_DEP_TRUE || !tick_check_data.seen_true_dep_p))
	tick_check_data.cycle = tick;
    }
}

/* An implementation of note_dep hook.  */
static void
tick_check_note_dep (insn_t pro, ds_t ds)
{
  tick_check_dep_with_dw (pro, ds, 0);
}

/* An implementation of note_mem_dep hook.  */
static void
tick_check_note_mem_dep (rtx mem1, rtx mem2, insn_t pro, ds_t ds)
{
  dw_t dw;

  dw = (ds_to_dt (ds) == REG_DEP_TRUE
        ? estimate_dep_weak (mem1, mem2)
        : 0);

  tick_check_dep_with_dw (pro, ds, dw);
}

/* This structure contains hooks for dependence analysis used when determining
   whether an insn is ready for scheduling.  */
static struct sched_deps_info_def tick_check_sched_deps_info =
  {
    NULL,

    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    haifa_note_reg_set,
    haifa_note_reg_clobber,
    haifa_note_reg_use,
    tick_check_note_mem_dep,
    tick_check_note_dep,

    0, 0, 0
  };

/* Estimate number of cycles from the current cycle of FENCE until EXPR can be
   scheduled.  Return 0 if all data from producers in DC is ready.  */
int
tick_check_p (expr_t expr, deps_t dc, fence_t fence)
{
  int cycles_left;
  /* Initialize variables.  */
  tick_check_data.expr = expr;
  tick_check_data.cycle = 0;
  tick_check_data.seen_true_dep_p = false;
  sched_deps_info = &tick_check_sched_deps_info;

  gcc_assert (!dc->readonly);
  dc->readonly = 1;
  deps_analyze_insn (dc, EXPR_INSN_RTX (expr));
  dc->readonly = 0;

  cycles_left = tick_check_data.cycle - FENCE_CYCLE (fence);

  return cycles_left >= 0 ? cycles_left : 0;
}


/* Functions to work with insns.  */

/* Returns true if LHS of INSN is the same as DEST of an insn
   being moved.  */
bool
lhs_of_insn_equals_to_dest_p (insn_t insn, rtx dest)
{
  rtx lhs = INSN_LHS (insn);

  if (lhs == NULL || dest == NULL)
    return false;

  return rtx_equal_p (lhs, dest);
}

/* Return s_i_d entry of INSN.  Callable from debugger.  */
sel_insn_data_def
insn_sid (insn_t insn)
{
  return *SID (insn);
}

/* True when INSN is a speculative check.  We can tell this by looking
   at the data structures of the selective scheduler, not by examining
   the pattern.  */
bool
sel_insn_is_speculation_check (rtx insn)
{
  return s_i_d && !! INSN_SPEC_CHECKED_DS (insn);
}

/* Extracts machine mode MODE and destination location DST_LOC
   for given INSN.  */
void
get_dest_and_mode (rtx insn, rtx *dst_loc, enum machine_mode *mode)
{
  rtx pat = PATTERN (insn);

  gcc_assert (dst_loc);
  gcc_assert (GET_CODE (pat) == SET);

  *dst_loc = SET_DEST (pat);

  gcc_assert (*dst_loc);
  gcc_assert (MEM_P (*dst_loc) || REG_P (*dst_loc));

  if (mode)
    *mode = GET_MODE (*dst_loc);
}

/* Returns true when moving through JUMP will result in bookkeeping
   creation.  */
bool
bookkeeping_can_be_created_if_moved_through_p (insn_t jump)
{
  insn_t succ;
  succ_iterator si;

  FOR_EACH_SUCC (succ, si, jump)
    if (sel_num_cfg_preds_gt_1 (succ))
      return true;

  return false;
}

/* Return 'true' if INSN is the only one in its basic block.  */
static bool
insn_is_the_only_one_in_bb_p (insn_t insn)
{
  return sel_bb_head_p (insn) && sel_bb_end_p (insn);
}

#ifdef ENABLE_CHECKING
/* Check that the region we're scheduling still has at most one
   backedge.  */
static void
verify_backedges (void)
{
  if (pipelining_p)
    {
      int i, n = 0;
      edge e;
      edge_iterator ei;

      for (i = 0; i < current_nr_blocks; i++)
        FOR_EACH_EDGE (e, ei, BASIC_BLOCK (BB_TO_BLOCK (i))->succs)
          if (in_current_region_p (e->dest)
              && BLOCK_TO_BB (e->dest->index) < i)
            n++;

      gcc_assert (n <= 1);
    }
}
#endif


/* Functions to work with control flow.  */

/* Recompute BLOCK_TO_BB and BB_FOR_BLOCK for current region so that blocks
   are sorted in topological order (it might have been invalidated by
   redirecting an edge).  */
static void
sel_recompute_toporder (void)
{
  int i, n, rgn;
  int *postorder, n_blocks;

  postorder = XALLOCAVEC (int, n_basic_blocks);
  n_blocks = post_order_compute (postorder, false, false);

  rgn = CONTAINING_RGN (BB_TO_BLOCK (0));
  for (n = 0, i = n_blocks - 1; i >= 0; i--)
    if (CONTAINING_RGN (postorder[i]) == rgn)
      {
	BLOCK_TO_BB (postorder[i]) = n;
	BB_TO_BLOCK (n) = postorder[i];
	n++;
      }

  /* Assert that we updated info for all blocks.  We may miss some blocks if
     this function is called when redirecting an edge made a block
     unreachable, but that block is not deleted yet.  */
  gcc_assert (n == RGN_NR_BLOCKS (rgn));
}

/* Tidy the possibly empty block BB.  */
static bool
maybe_tidy_empty_bb (basic_block bb, bool recompute_toporder_p)
{
  basic_block succ_bb, pred_bb;
  edge e;
  edge_iterator ei;
  bool rescan_p;

  /* Keep empty bb only if this block immediately precedes EXIT and
     has incoming non-fallthrough edge, or it has no predecessors or
     successors.  Otherwise remove it.  */
  if (!sel_bb_empty_p (bb)
      || (single_succ_p (bb)
          && single_succ (bb) == EXIT_BLOCK_PTR
          && (!single_pred_p (bb)
              || !(single_pred_edge (bb)->flags & EDGE_FALLTHRU)))
      || EDGE_COUNT (bb->preds) == 0
      || EDGE_COUNT (bb->succs) == 0)
    return false;

  /* Do not attempt to redirect complex edges.  */
  FOR_EACH_EDGE (e, ei, bb->preds)
    if (e->flags & EDGE_COMPLEX)
      return false;

  free_data_sets (bb);

  /* Do not delete BB if it has more than one successor.
     That can occur when we moving a jump.  */
  if (!single_succ_p (bb))
    {
      gcc_assert (can_merge_blocks_p (bb->prev_bb, bb));
      sel_merge_blocks (bb->prev_bb, bb);
      return true;
    }

  succ_bb = single_succ (bb);
  rescan_p = true;
  pred_bb = NULL;

  /* Redirect all non-fallthru edges to the next bb.  */
  while (rescan_p)
    {
      rescan_p = false;

      FOR_EACH_EDGE (e, ei, bb->preds)
        {
          pred_bb = e->src;

          if (!(e->flags & EDGE_FALLTHRU))
            {
              recompute_toporder_p |= sel_redirect_edge_and_branch (e, succ_bb);
              rescan_p = true;
              break;
            }
        }
    }

  /* If it is possible - merge BB with its predecessor.  */
  if (can_merge_blocks_p (bb->prev_bb, bb))
    sel_merge_blocks (bb->prev_bb, bb);
  else
    /* Otherwise this is a block without fallthru predecessor.
       Just delete it.  */
    {
      gcc_assert (pred_bb != NULL);

      if (in_current_region_p (pred_bb))
	move_bb_info (pred_bb, bb);
      remove_empty_bb (bb, true);
    }

  if (recompute_toporder_p)
    sel_recompute_toporder ();

#ifdef ENABLE_CHECKING
  verify_backedges ();
#endif

  return true;
}

/* Tidy the control flow after we have removed original insn from
   XBB.  Return true if we have removed some blocks.  When FULL_TIDYING
   is true, also try to optimize control flow on non-empty blocks.  */
bool
tidy_control_flow (basic_block xbb, bool full_tidying)
{
  bool changed = true;
  insn_t first, last;

  /* First check whether XBB is empty.  */
  changed = maybe_tidy_empty_bb (xbb, false);
  if (changed || !full_tidying)
    return changed;

  /* Check if there is a unnecessary jump after insn left.  */
  if (jump_leads_only_to_bb_p (BB_END (xbb), xbb->next_bb)
      && INSN_SCHED_TIMES (BB_END (xbb)) == 0
      && !IN_CURRENT_FENCE_P (BB_END (xbb)))
    {
      if (sel_remove_insn (BB_END (xbb), false, false))
        return true;
      tidy_fallthru_edge (EDGE_SUCC (xbb, 0));
    }

  first = sel_bb_head (xbb);
  last = sel_bb_end (xbb);
  if (MAY_HAVE_DEBUG_INSNS)
    {
      if (first != last && DEBUG_INSN_P (first))
	do
	  first = NEXT_INSN (first);
	while (first != last && (DEBUG_INSN_P (first) || NOTE_P (first)));

      if (first != last && DEBUG_INSN_P (last))
	do
	  last = PREV_INSN (last);
	while (first != last && (DEBUG_INSN_P (last) || NOTE_P (last)));
    }
  /* Check if there is an unnecessary jump in previous basic block leading
     to next basic block left after removing INSN from stream.
     If it is so, remove that jump and redirect edge to current
     basic block (where there was INSN before deletion).  This way
     when NOP will be deleted several instructions later with its
     basic block we will not get a jump to next instruction, which
     can be harmful.  */
  if (first == last
      && !sel_bb_empty_p (xbb)
      && INSN_NOP_P (last)
      /* Flow goes fallthru from current block to the next.  */
      && EDGE_COUNT (xbb->succs) == 1
      && (EDGE_SUCC (xbb, 0)->flags & EDGE_FALLTHRU)
      /* When successor is an EXIT block, it may not be the next block.  */
      && single_succ (xbb) != EXIT_BLOCK_PTR
      /* And unconditional jump in previous basic block leads to
         next basic block of XBB and this jump can be safely removed.  */
      && in_current_region_p (xbb->prev_bb)
      && jump_leads_only_to_bb_p (BB_END (xbb->prev_bb), xbb->next_bb)
      && INSN_SCHED_TIMES (BB_END (xbb->prev_bb)) == 0
      /* Also this jump is not at the scheduling boundary.  */
      && !IN_CURRENT_FENCE_P (BB_END (xbb->prev_bb)))
    {
      bool recompute_toporder_p;
      /* Clear data structures of jump - jump itself will be removed
         by sel_redirect_edge_and_branch.  */
      clear_expr (INSN_EXPR (BB_END (xbb->prev_bb)));
      recompute_toporder_p
        = sel_redirect_edge_and_branch (EDGE_SUCC (xbb->prev_bb, 0), xbb);

      gcc_assert (EDGE_SUCC (xbb->prev_bb, 0)->flags & EDGE_FALLTHRU);

      /* It can turn out that after removing unused jump, basic block
         that contained that jump, becomes empty too.  In such case
         remove it too.  */
      if (sel_bb_empty_p (xbb->prev_bb))
        changed = maybe_tidy_empty_bb (xbb->prev_bb, recompute_toporder_p);
      else if (recompute_toporder_p)
	sel_recompute_toporder ();
    }

  return changed;
}

/* Purge meaningless empty blocks in the middle of a region.  */
void
purge_empty_blocks (void)
{
  /* Do not attempt to delete preheader.  */
  int i = sel_is_loop_preheader_p (BASIC_BLOCK (BB_TO_BLOCK (0))) ? 1 : 0;

  while (i < current_nr_blocks)
    {
      basic_block b = BASIC_BLOCK (BB_TO_BLOCK (i));

      if (maybe_tidy_empty_bb (b, false))
	continue;

      i++;
    }
}

/* Rip-off INSN from the insn stream.  When ONLY_DISCONNECT is true,
   do not delete insn's data, because it will be later re-emitted.
   Return true if we have removed some blocks afterwards.  */
bool
sel_remove_insn (insn_t insn, bool only_disconnect, bool full_tidying)
{
  basic_block bb = BLOCK_FOR_INSN (insn);

  gcc_assert (INSN_IN_STREAM_P (insn));

  if (DEBUG_INSN_P (insn) && BB_AV_SET_VALID_P (bb))
    {
      expr_t expr;
      av_set_iterator i;

      /* When we remove a debug insn that is head of a BB, it remains
	 in the AV_SET of the block, but it shouldn't.  */
      FOR_EACH_EXPR_1 (expr, i, &BB_AV_SET (bb))
	if (EXPR_INSN_RTX (expr) == insn)
	  {
	    av_set_iter_remove (&i);
	    break;
	  }
    }

  if (only_disconnect)
    {
      insn_t prev = PREV_INSN (insn);
      insn_t next = NEXT_INSN (insn);
      basic_block bb = BLOCK_FOR_INSN (insn);

      NEXT_INSN (prev) = next;
      PREV_INSN (next) = prev;

      if (BB_HEAD (bb) == insn)
        {
          gcc_assert (BLOCK_FOR_INSN (prev) == bb);
          BB_HEAD (bb) = prev;
        }
      if (BB_END (bb) == insn)
        BB_END (bb) = prev;
    }
  else
    {
      remove_insn (insn);
      clear_expr (INSN_EXPR (insn));
    }

  /* It is necessary to null this fields before calling add_insn ().  */
  PREV_INSN (insn) = NULL_RTX;
  NEXT_INSN (insn) = NULL_RTX;

  return tidy_control_flow (bb, full_tidying);
}

/* Estimate number of the insns in BB.  */
static int
sel_estimate_number_of_insns (basic_block bb)
{
  int res = 0;
  insn_t insn = NEXT_INSN (BB_HEAD (bb)), next_tail = NEXT_INSN (BB_END (bb));

  for (; insn != next_tail; insn = NEXT_INSN (insn))
    if (NONDEBUG_INSN_P (insn))
      res++;

  return res;
}

/* We don't need separate luids for notes or labels.  */
static int
sel_luid_for_non_insn (rtx x)
{
  gcc_assert (NOTE_P (x) || LABEL_P (x));

  return -1;
}

/* Return seqno of the only predecessor of INSN.  */
static int
get_seqno_of_a_pred (insn_t insn)
{
  int seqno;

  gcc_assert (INSN_SIMPLEJUMP_P (insn));

  if (!sel_bb_head_p (insn))
    seqno = INSN_SEQNO (PREV_INSN (insn));
  else
    {
      basic_block bb = BLOCK_FOR_INSN (insn);

      if (single_pred_p (bb)
	  && !in_current_region_p (single_pred (bb)))
	{
          /* We can have preds outside a region when splitting edges
             for pipelining of an outer loop.  Use succ instead.
             There should be only one of them.  */
	  insn_t succ = NULL;
          succ_iterator si;
          bool first = true;

	  gcc_assert (flag_sel_sched_pipelining_outer_loops
		      && current_loop_nest);
          FOR_EACH_SUCC_1 (succ, si, insn,
                           SUCCS_NORMAL | SUCCS_SKIP_TO_LOOP_EXITS)
            {
              gcc_assert (first);
              first = false;
            }

	  gcc_assert (succ != NULL);
	  seqno = INSN_SEQNO (succ);
	}
      else
	{
	  insn_t *preds;
	  int n;

	  cfg_preds (BLOCK_FOR_INSN (insn), &preds, &n);
	  gcc_assert (n == 1);

	  seqno = INSN_SEQNO (preds[0]);

	  free (preds);
	}
    }

  return seqno;
}

/*  Find the proper seqno for inserting at INSN.  Returns -1 if no predecessors
    with positive seqno exist.  */
int
get_seqno_by_preds (rtx insn)
{
  basic_block bb = BLOCK_FOR_INSN (insn);
  rtx tmp = insn, head = BB_HEAD (bb);
  insn_t *preds;
  int n, i, seqno;

  while (tmp != head)
    if (INSN_P (tmp))
      return INSN_SEQNO (tmp);
    else
      tmp = PREV_INSN (tmp);

  cfg_preds (bb, &preds, &n);
  for (i = 0, seqno = -1; i < n; i++)
    seqno = MAX (seqno, INSN_SEQNO (preds[i]));

  return seqno;
}



/* Extend pass-scope data structures for basic blocks.  */
void
sel_extend_global_bb_info (void)
{
  VEC_safe_grow_cleared (sel_global_bb_info_def, heap, sel_global_bb_info,
			 last_basic_block);
}

/* Extend region-scope data structures for basic blocks.  */
static void
extend_region_bb_info (void)
{
  VEC_safe_grow_cleared (sel_region_bb_info_def, heap, sel_region_bb_info,
			 last_basic_block);
}

/* Extend all data structures to fit for all basic blocks.  */
static void
extend_bb_info (void)
{
  sel_extend_global_bb_info ();
  extend_region_bb_info ();
}

/* Finalize pass-scope data structures for basic blocks.  */
void
sel_finish_global_bb_info (void)
{
  VEC_free (sel_global_bb_info_def, heap, sel_global_bb_info);
}

/* Finalize region-scope data structures for basic blocks.  */
static void
finish_region_bb_info (void)
{
  VEC_free (sel_region_bb_info_def, heap, sel_region_bb_info);
}


/* Data for each insn in current region.  */
VEC (sel_insn_data_def, heap) *s_i_d = NULL;

/* A vector for the insns we've emitted.  */
static insn_vec_t new_insns = NULL;

/* Extend data structures for insns from current region.  */
static void
extend_insn_data (void)
{
  int reserve;

  sched_extend_target ();
  sched_deps_init (false);

  /* Extend data structures for insns from current region.  */
  reserve = (sched_max_luid + 1
             - VEC_length (sel_insn_data_def, s_i_d));
  if (reserve > 0
      && ! VEC_space (sel_insn_data_def, s_i_d, reserve))
    {
      int size;

      if (sched_max_luid / 2 > 1024)
        size = sched_max_luid + 1024;
      else
        size = 3 * sched_max_luid / 2;


      VEC_safe_grow_cleared (sel_insn_data_def, heap, s_i_d, size);
    }
}

/* Finalize data structures for insns from current region.  */
static void
finish_insns (void)
{
  unsigned i;

  /* Clear here all dependence contexts that may have left from insns that were
     removed during the scheduling.  */
  for (i = 0; i < VEC_length (sel_insn_data_def, s_i_d); i++)
    {
      sel_insn_data_def *sid_entry = VEC_index (sel_insn_data_def, s_i_d, i);

      if (sid_entry->live)
        return_regset_to_pool (sid_entry->live);
      if (sid_entry->analyzed_deps)
	{
	  BITMAP_FREE (sid_entry->analyzed_deps);
	  BITMAP_FREE (sid_entry->found_deps);
          htab_delete (sid_entry->transformed_insns);
	  free_deps (&sid_entry->deps_context);
	}
      if (EXPR_VINSN (&sid_entry->expr))
        {
          clear_expr (&sid_entry->expr);

          /* Also, clear CANT_MOVE bit here, because we really don't want it
             to be passed to the next region.  */
          CANT_MOVE_BY_LUID (i) = 0;
        }
    }

  VEC_free (sel_insn_data_def, heap, s_i_d);
}

/* A proxy to pass initialization data to init_insn ().  */
static sel_insn_data_def _insn_init_ssid;
static sel_insn_data_t insn_init_ssid = &_insn_init_ssid;

/* If true create a new vinsn.  Otherwise use the one from EXPR.  */
static bool insn_init_create_new_vinsn_p;

/* Set all necessary data for initialization of the new insn[s].  */
static expr_t
set_insn_init (expr_t expr, vinsn_t vi, int seqno)
{
  expr_t x = &insn_init_ssid->expr;

  copy_expr_onside (x, expr);
  if (vi != NULL)
    {
      insn_init_create_new_vinsn_p = false;
      change_vinsn_in_expr (x, vi);
    }
  else
    insn_init_create_new_vinsn_p = true;

  insn_init_ssid->seqno = seqno;
  return x;
}

/* Init data for INSN.  */
static void
init_insn_data (insn_t insn)
{
  expr_t expr;
  sel_insn_data_t ssid = insn_init_ssid;

  /* The fields mentioned below are special and hence are not being
     propagated to the new insns.  */
  gcc_assert (!ssid->asm_p && ssid->sched_next == NULL
	      && !ssid->after_stall_p && ssid->sched_cycle == 0);
  gcc_assert (INSN_P (insn) && INSN_LUID (insn) > 0);

  expr = INSN_EXPR (insn);
  copy_expr (expr, &ssid->expr);
  prepare_insn_expr (insn, ssid->seqno);

  if (insn_init_create_new_vinsn_p)
    change_vinsn_in_expr (expr, vinsn_create (insn, init_insn_force_unique_p));

  if (first_time_insn_init (insn))
    init_first_time_insn_data (insn);
}

/* This is used to initialize spurious jumps generated by
   sel_redirect_edge ().  */
static void
init_simplejump_data (insn_t insn)
{
  init_expr (INSN_EXPR (insn), vinsn_create (insn, false), 0,
	     REG_BR_PROB_BASE, 0, 0, 0, 0, 0, 0, NULL, true, false, false,
	     false, true);
  INSN_SEQNO (insn) = get_seqno_of_a_pred (insn);
  init_first_time_insn_data (insn);
}

/* Perform deferred initialization of insns.  This is used to process
   a new jump that may be created by redirect_edge.  */
void
sel_init_new_insn (insn_t insn, int flags)
{
  /* We create data structures for bb when the first insn is emitted in it.  */
  if (INSN_P (insn)
      && INSN_IN_STREAM_P (insn)
      && insn_is_the_only_one_in_bb_p (insn))
    {
      extend_bb_info ();
      create_initial_data_sets (BLOCK_FOR_INSN (insn));
    }

  if (flags & INSN_INIT_TODO_LUID)
    sched_init_luids (NULL, NULL, NULL, insn);

  if (flags & INSN_INIT_TODO_SSID)
    {
      extend_insn_data ();
      init_insn_data (insn);
      clear_expr (&insn_init_ssid->expr);
    }

  if (flags & INSN_INIT_TODO_SIMPLEJUMP)
    {
      extend_insn_data ();
      init_simplejump_data (insn);
    }

  gcc_assert (CONTAINING_RGN (BLOCK_NUM (insn))
              == CONTAINING_RGN (BB_TO_BLOCK (0)));
}


/* Functions to init/finish work with lv sets.  */

/* Init BB_LV_SET of BB from DF_LR_IN set of BB.  */
static void
init_lv_set (basic_block bb)
{
  gcc_assert (!BB_LV_SET_VALID_P (bb));

  BB_LV_SET (bb) = get_regset_from_pool ();
  COPY_REG_SET (BB_LV_SET (bb), DF_LR_IN (bb));
  BB_LV_SET_VALID_P (bb) = true;
}

/* Copy liveness information to BB from FROM_BB.  */
static void
copy_lv_set_from (basic_block bb, basic_block from_bb)
{
  gcc_assert (!BB_LV_SET_VALID_P (bb));

  COPY_REG_SET (BB_LV_SET (bb), BB_LV_SET (from_bb));
  BB_LV_SET_VALID_P (bb) = true;
}

/* Initialize lv set of all bb headers.  */
void
init_lv_sets (void)
{
  basic_block bb;

  /* Initialize of LV sets.  */
  FOR_EACH_BB (bb)
    init_lv_set (bb);

  /* Don't forget EXIT_BLOCK.  */
  init_lv_set (EXIT_BLOCK_PTR);
}

/* Release lv set of HEAD.  */
static void
free_lv_set (basic_block bb)
{
  gcc_assert (BB_LV_SET (bb) != NULL);

  return_regset_to_pool (BB_LV_SET (bb));
  BB_LV_SET (bb) = NULL;
  BB_LV_SET_VALID_P (bb) = false;
}

/* Finalize lv sets of all bb headers.  */
void
free_lv_sets (void)
{
  basic_block bb;

  /* Don't forget EXIT_BLOCK.  */
  free_lv_set (EXIT_BLOCK_PTR);

  /* Free LV sets.  */
  FOR_EACH_BB (bb)
    if (BB_LV_SET (bb))
      free_lv_set (bb);
}

/* Initialize an invalid AV_SET for BB.
   This set will be updated next time compute_av () process BB.  */
static void
invalidate_av_set (basic_block bb)
{
  gcc_assert (BB_AV_LEVEL (bb) <= 0
	      && BB_AV_SET (bb) == NULL);

  BB_AV_LEVEL (bb) = -1;
}

/* Create initial data sets for BB (they will be invalid).  */
static void
create_initial_data_sets (basic_block bb)
{
  if (BB_LV_SET (bb))
    BB_LV_SET_VALID_P (bb) = false;
  else
    BB_LV_SET (bb) = get_regset_from_pool ();
  invalidate_av_set (bb);
}

/* Free av set of BB.  */
static void
free_av_set (basic_block bb)
{
  av_set_clear (&BB_AV_SET (bb));
  BB_AV_LEVEL (bb) = 0;
}

/* Free data sets of BB.  */
void
free_data_sets (basic_block bb)
{
  free_lv_set (bb);
  free_av_set (bb);
}

/* Exchange lv sets of TO and FROM.  */
static void
exchange_lv_sets (basic_block to, basic_block from)
{
  {
    regset to_lv_set = BB_LV_SET (to);

    BB_LV_SET (to) = BB_LV_SET (from);
    BB_LV_SET (from) = to_lv_set;
  }

  {
    bool to_lv_set_valid_p = BB_LV_SET_VALID_P (to);

    BB_LV_SET_VALID_P (to) = BB_LV_SET_VALID_P (from);
    BB_LV_SET_VALID_P (from) = to_lv_set_valid_p;
  }
}


/* Exchange av sets of TO and FROM.  */
static void
exchange_av_sets (basic_block to, basic_block from)
{
  {
    av_set_t to_av_set = BB_AV_SET (to);

    BB_AV_SET (to) = BB_AV_SET (from);
    BB_AV_SET (from) = to_av_set;
  }

  {
    int to_av_level = BB_AV_LEVEL (to);

    BB_AV_LEVEL (to) = BB_AV_LEVEL (from);
    BB_AV_LEVEL (from) = to_av_level;
  }
}

/* Exchange data sets of TO and FROM.  */
void
exchange_data_sets (basic_block to, basic_block from)
{
  exchange_lv_sets (to, from);
  exchange_av_sets (to, from);
}

/* Copy data sets of FROM to TO.  */
void
copy_data_sets (basic_block to, basic_block from)
{
  gcc_assert (!BB_LV_SET_VALID_P (to) && !BB_AV_SET_VALID_P (to));
  gcc_assert (BB_AV_SET (to) == NULL);

  BB_AV_LEVEL (to) = BB_AV_LEVEL (from);
  BB_LV_SET_VALID_P (to) = BB_LV_SET_VALID_P (from);

  if (BB_AV_SET_VALID_P (from))
    {
      BB_AV_SET (to) = av_set_copy (BB_AV_SET (from));
    }
  if (BB_LV_SET_VALID_P (from))
    {
      gcc_assert (BB_LV_SET (to) != NULL);
      COPY_REG_SET (BB_LV_SET (to), BB_LV_SET (from));
    }
}

/* Return an av set for INSN, if any.  */
av_set_t
get_av_set (insn_t insn)
{
  av_set_t av_set;

  gcc_assert (AV_SET_VALID_P (insn));

  if (sel_bb_head_p (insn))
    av_set = BB_AV_SET (BLOCK_FOR_INSN (insn));
  else
    av_set = NULL;

  return av_set;
}

/* Implementation of AV_LEVEL () macro.  Return AV_LEVEL () of INSN.  */
int
get_av_level (insn_t insn)
{
  int av_level;

  gcc_assert (INSN_P (insn));

  if (sel_bb_head_p (insn))
    av_level = BB_AV_LEVEL (BLOCK_FOR_INSN (insn));
  else
    av_level = INSN_WS_LEVEL (insn);

  return av_level;
}



/* Variables to work with control-flow graph.  */

/* The basic block that already has been processed by the sched_data_update (),
   but hasn't been in sel_add_bb () yet.  */
static VEC (basic_block, heap) *last_added_blocks = NULL;

/* A pool for allocating successor infos.  */
static struct
{
  /* A stack for saving succs_info structures.  */
  struct succs_info *stack;

  /* Its size.  */
  int size;

  /* Top of the stack.  */
  int top;

  /* Maximal value of the top.  */
  int max_top;
}  succs_info_pool;

/* Functions to work with control-flow graph.  */

/* Return basic block note of BB.  */
insn_t
sel_bb_head (basic_block bb)
{
  insn_t head;

  if (bb == EXIT_BLOCK_PTR)
    {
      gcc_assert (exit_insn != NULL_RTX);
      head = exit_insn;
    }
  else
    {
      insn_t note;

      note = bb_note (bb);
      head = next_nonnote_insn (note);

      if (head && BLOCK_FOR_INSN (head) != bb)
	head = NULL_RTX;
    }

  return head;
}

/* Return true if INSN is a basic block header.  */
bool
sel_bb_head_p (insn_t insn)
{
  return sel_bb_head (BLOCK_FOR_INSN (insn)) == insn;
}

/* Return last insn of BB.  */
insn_t
sel_bb_end (basic_block bb)
{
  if (sel_bb_empty_p (bb))
    return NULL_RTX;

  gcc_assert (bb != EXIT_BLOCK_PTR);

  return BB_END (bb);
}

/* Return true if INSN is the last insn in its basic block.  */
bool
sel_bb_end_p (insn_t insn)
{
  return insn == sel_bb_end (BLOCK_FOR_INSN (insn));
}

/* Return true if BB consist of single NOTE_INSN_BASIC_BLOCK.  */
bool
sel_bb_empty_p (basic_block bb)
{
  return sel_bb_head (bb) == NULL;
}

/* True when BB belongs to the current scheduling region.  */
bool
in_current_region_p (basic_block bb)
{
  if (bb->index < NUM_FIXED_BLOCKS)
    return false;

  return CONTAINING_RGN (bb->index) == CONTAINING_RGN (BB_TO_BLOCK (0));
}

/* Return the block which is a fallthru bb of a conditional jump JUMP.  */
basic_block
fallthru_bb_of_jump (rtx jump)
{
  if (!JUMP_P (jump))
    return NULL;

  if (any_uncondjump_p (jump))
    return single_succ (BLOCK_FOR_INSN (jump));

  if (!any_condjump_p (jump))
    return NULL;

  /* A basic block that ends with a conditional jump may still have one successor
     (and be followed by a barrier), we are not interested.  */
  if (single_succ_p (BLOCK_FOR_INSN (jump)))
    return NULL;

  return FALLTHRU_EDGE (BLOCK_FOR_INSN (jump))->dest;
}

/* Remove all notes from BB.  */
static void
init_bb (basic_block bb)
{
  remove_notes (bb_note (bb), BB_END (bb));
  BB_NOTE_LIST (bb) = note_list;
}

void
sel_init_bbs (bb_vec_t bbs, basic_block bb)
{
  const struct sched_scan_info_def ssi =
    {
      extend_bb_info, /* extend_bb */
      init_bb, /* init_bb */
      NULL, /* extend_insn */
      NULL /* init_insn */
    };

  sched_scan (&ssi, bbs, bb, new_insns, NULL);
}

/* Restore notes for the whole region.  */
static void
sel_restore_notes (void)
{
  int bb;
  insn_t insn;

  for (bb = 0; bb < current_nr_blocks; bb++)
    {
      basic_block first, last;

      first = EBB_FIRST_BB (bb);
      last = EBB_LAST_BB (bb)->next_bb;

      do
	{
	  note_list = BB_NOTE_LIST (first);
	  restore_other_notes (NULL, first);
	  BB_NOTE_LIST (first) = NULL_RTX;

	  FOR_BB_INSNS (first, insn)
	    if (NONDEBUG_INSN_P (insn))
	      reemit_notes (insn);

          first = first->next_bb;
	}
      while (first != last);
    }
}

/* Free per-bb data structures.  */
void
sel_finish_bbs (void)
{
  sel_restore_notes ();

  /* Remove current loop preheader from this loop.  */
  if (current_loop_nest)
    sel_remove_loop_preheader ();

  finish_region_bb_info ();
}

/* Return true if INSN has a single successor of type FLAGS.  */
bool
sel_insn_has_single_succ_p (insn_t insn, int flags)
{
  insn_t succ;
  succ_iterator si;
  bool first_p = true;

  FOR_EACH_SUCC_1 (succ, si, insn, flags)
    {
      if (first_p)
	first_p = false;
      else
	return false;
    }

  return true;
}

/* Allocate successor's info.  */
static struct succs_info *
alloc_succs_info (void)
{
  if (succs_info_pool.top == succs_info_pool.max_top)
    {
      int i;

      if (++succs_info_pool.max_top >= succs_info_pool.size)
        gcc_unreachable ();

      i = ++succs_info_pool.top;
      succs_info_pool.stack[i].succs_ok = VEC_alloc (rtx, heap, 10);
      succs_info_pool.stack[i].succs_other = VEC_alloc (rtx, heap, 10);
      succs_info_pool.stack[i].probs_ok = VEC_alloc (int, heap, 10);
    }
  else
    succs_info_pool.top++;

  return &succs_info_pool.stack[succs_info_pool.top];
}

/* Free successor's info.  */
void
free_succs_info (struct succs_info * sinfo)
{
  gcc_assert (succs_info_pool.top >= 0
              && &succs_info_pool.stack[succs_info_pool.top] == sinfo);
  succs_info_pool.top--;

  /* Clear stale info.  */
  VEC_block_remove (rtx, sinfo->succs_ok,
                    0, VEC_length (rtx, sinfo->succs_ok));
  VEC_block_remove (rtx, sinfo->succs_other,
                    0, VEC_length (rtx, sinfo->succs_other));
  VEC_block_remove (int, sinfo->probs_ok,
                    0, VEC_length (int, sinfo->probs_ok));
  sinfo->all_prob = 0;
  sinfo->succs_ok_n = 0;
  sinfo->all_succs_n = 0;
}

/* Compute successor info for INSN.  FLAGS are the flags passed
   to the FOR_EACH_SUCC_1 iterator.  */
struct succs_info *
compute_succs_info (insn_t insn, short flags)
{
  succ_iterator si;
  insn_t succ;
  struct succs_info *sinfo = alloc_succs_info ();

  /* Traverse *all* successors and decide what to do with each.  */
  FOR_EACH_SUCC_1 (succ, si, insn, SUCCS_ALL)
    {
      /* FIXME: this doesn't work for skipping to loop exits, as we don't
         perform code motion through inner loops.  */
      short current_flags = si.current_flags & ~SUCCS_SKIP_TO_LOOP_EXITS;

      if (current_flags & flags)
        {
          VEC_safe_push (rtx, heap, sinfo->succs_ok, succ);
          VEC_safe_push (int, heap, sinfo->probs_ok,
                         /* FIXME: Improve calculation when skipping
                            inner loop to exits.  */
                         (si.bb_end
                          ? si.e1->probability
                          : REG_BR_PROB_BASE));
          sinfo->succs_ok_n++;
        }
      else
        VEC_safe_push (rtx, heap, sinfo->succs_other, succ);

      /* Compute all_prob.  */
      if (!si.bb_end)
        sinfo->all_prob = REG_BR_PROB_BASE;
      else
        sinfo->all_prob += si.e1->probability;

      sinfo->all_succs_n++;
    }

  return sinfo;
}

/* Return the predecessors of BB in PREDS and their number in N.
   Empty blocks are skipped.  SIZE is used to allocate PREDS.  */
static void
cfg_preds_1 (basic_block bb, insn_t **preds, int *n, int *size)
{
  edge e;
  edge_iterator ei;

  gcc_assert (BLOCK_TO_BB (bb->index) != 0);

  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      basic_block pred_bb = e->src;
      insn_t bb_end = BB_END (pred_bb);

      /* ??? This code is not supposed to walk out of a region.  */
      gcc_assert (in_current_region_p (pred_bb));

      if (sel_bb_empty_p (pred_bb))
	cfg_preds_1 (pred_bb, preds, n, size);
      else
	{
	  if (*n == *size)
	    *preds = XRESIZEVEC (insn_t, *preds,
                                 (*size = 2 * *size + 1));
	  (*preds)[(*n)++] = bb_end;
	}
    }

  gcc_assert (*n != 0);
}

/* Find all predecessors of BB and record them in PREDS and their number
   in N.  Empty blocks are skipped, and only normal (forward in-region)
   edges are processed.  */
static void
cfg_preds (basic_block bb, insn_t **preds, int *n)
{
  int size = 0;

  *preds = NULL;
  *n = 0;
  cfg_preds_1 (bb, preds, n, &size);
}

/* Returns true if we are moving INSN through join point.  */
bool
sel_num_cfg_preds_gt_1 (insn_t insn)
{
  basic_block bb;

  if (!sel_bb_head_p (insn) || INSN_BB (insn) == 0)
    return false;

  bb = BLOCK_FOR_INSN (insn);

  while (1)
    {
      if (EDGE_COUNT (bb->preds) > 1)
	return true;

      gcc_assert (EDGE_PRED (bb, 0)->dest == bb);
      bb = EDGE_PRED (bb, 0)->src;

      if (!sel_bb_empty_p (bb))
	break;
    }

  return false;
}

/* Returns true when BB should be the end of an ebb.  Adapted from the
   code in sched-ebb.c.  */
bool
bb_ends_ebb_p (basic_block bb)
{
  basic_block next_bb = bb_next_bb (bb);
  edge e;
  edge_iterator ei;

  if (next_bb == EXIT_BLOCK_PTR
      || bitmap_bit_p (forced_ebb_heads, next_bb->index)
      || (LABEL_P (BB_HEAD (next_bb))
	  /* NB: LABEL_NUSES () is not maintained outside of jump.c.
	     Work around that.  */
	  && !single_pred_p (next_bb)))
    return true;

  if (!in_current_region_p (next_bb))
    return true;

  FOR_EACH_EDGE (e, ei, bb->succs)
    if ((e->flags & EDGE_FALLTHRU) != 0)
      {
	gcc_assert (e->dest == next_bb);

	return false;
      }

  return true;
}

/* Returns true when INSN and SUCC are in the same EBB, given that SUCC is a
   successor of INSN.  */
bool
in_same_ebb_p (insn_t insn, insn_t succ)
{
  basic_block ptr = BLOCK_FOR_INSN (insn);

  for(;;)
    {
      if (ptr == BLOCK_FOR_INSN (succ))
        return true;

      if (bb_ends_ebb_p (ptr))
        return false;

      ptr = bb_next_bb (ptr);
    }

  gcc_unreachable ();
  return false;
}

/* Recomputes the reverse topological order for the function and
   saves it in REV_TOP_ORDER_INDEX.  REV_TOP_ORDER_INDEX_LEN is also
   modified appropriately.  */
static void
recompute_rev_top_order (void)
{
  int *postorder;
  int n_blocks, i;

  if (!rev_top_order_index || rev_top_order_index_len < last_basic_block)
    {
      rev_top_order_index_len = last_basic_block;
      rev_top_order_index = XRESIZEVEC (int, rev_top_order_index,
                                        rev_top_order_index_len);
    }

  postorder = XNEWVEC (int, n_basic_blocks);

  n_blocks = post_order_compute (postorder, true, false);
  gcc_assert (n_basic_blocks == n_blocks);

  /* Build reverse function: for each basic block with BB->INDEX == K
     rev_top_order_index[K] is it's reverse topological sort number.  */
  for (i = 0; i < n_blocks; i++)
    {
      gcc_assert (postorder[i] < rev_top_order_index_len);
      rev_top_order_index[postorder[i]] = i;
    }

  free (postorder);
}

/* Clear all flags from insns in BB that could spoil its rescheduling.  */
void
clear_outdated_rtx_info (basic_block bb)
{
  rtx insn;

  FOR_BB_INSNS (bb, insn)
    if (INSN_P (insn))
      {
	SCHED_GROUP_P (insn) = 0;
	INSN_AFTER_STALL_P (insn) = 0;
	INSN_SCHED_TIMES (insn) = 0;
	EXPR_PRIORITY_ADJ (INSN_EXPR (insn)) = 0;

        /* We cannot use the changed caches, as previously we could ignore
           the LHS dependence due to enabled renaming and transform
           the expression, and currently we'll be unable to do this.  */
        htab_empty (INSN_TRANSFORMED_INSNS (insn));
      }
}

/* Add BB_NOTE to the pool of available basic block notes.  */
static void
return_bb_to_pool (basic_block bb)
{
  rtx note = bb_note (bb);

  gcc_assert (NOTE_BASIC_BLOCK (note) == bb
	      && bb->aux == NULL);

  /* It turns out that current cfg infrastructure does not support
     reuse of basic blocks.  Don't bother for now.  */
  /*VEC_safe_push (rtx, heap, bb_note_pool, note);*/
}

/* Get a bb_note from pool or return NULL_RTX if pool is empty.  */
static rtx
get_bb_note_from_pool (void)
{
  if (VEC_empty (rtx, bb_note_pool))
    return NULL_RTX;
  else
    {
      rtx note = VEC_pop (rtx, bb_note_pool);

      PREV_INSN (note) = NULL_RTX;
      NEXT_INSN (note) = NULL_RTX;

      return note;
    }
}

/* Free bb_note_pool.  */
void
free_bb_note_pool (void)
{
  VEC_free (rtx, heap, bb_note_pool);
}

/* Setup scheduler pool and successor structure.  */
void
alloc_sched_pools (void)
{
  int succs_size;

  succs_size = MAX_WS + 1;
  succs_info_pool.stack = XCNEWVEC (struct succs_info, succs_size);
  succs_info_pool.size = succs_size;
  succs_info_pool.top = -1;
  succs_info_pool.max_top = -1;

  sched_lists_pool = create_alloc_pool ("sel-sched-lists",
                                        sizeof (struct _list_node), 500);
}

/* Free the pools.  */
void
free_sched_pools (void)
{
  int i;

  free_alloc_pool (sched_lists_pool);
  gcc_assert (succs_info_pool.top == -1);
  for (i = 0; i < succs_info_pool.max_top; i++)
    {
      VEC_free (rtx, heap, succs_info_pool.stack[i].succs_ok);
      VEC_free (rtx, heap, succs_info_pool.stack[i].succs_other);
      VEC_free (int, heap, succs_info_pool.stack[i].probs_ok);
    }
  free (succs_info_pool.stack);
}


/* Returns a position in RGN where BB can be inserted retaining
   topological order.  */
static int
find_place_to_insert_bb (basic_block bb, int rgn)
{
  bool has_preds_outside_rgn = false;
  edge e;
  edge_iterator ei;

  /* Find whether we have preds outside the region.  */
  FOR_EACH_EDGE (e, ei, bb->preds)
    if (!in_current_region_p (e->src))
      {
        has_preds_outside_rgn = true;
        break;
      }

  /* Recompute the top order -- needed when we have > 1 pred
     and in case we don't have preds outside.  */
  if (flag_sel_sched_pipelining_outer_loops
      && (has_preds_outside_rgn || EDGE_COUNT (bb->preds) > 1))
    {
      int i, bbi = bb->index, cur_bbi;

      recompute_rev_top_order ();
      for (i = RGN_NR_BLOCKS (rgn) - 1; i >= 0; i--)
        {
          cur_bbi = BB_TO_BLOCK (i);
          if (rev_top_order_index[bbi]
              < rev_top_order_index[cur_bbi])
            break;
        }

      /* We skipped the right block, so we increase i.  We accomodate
         it for increasing by step later, so we decrease i.  */
      return (i + 1) - 1;
    }
  else if (has_preds_outside_rgn)
    {
      /* This is the case when we generate an extra empty block
         to serve as region head during pipelining.  */
      e = EDGE_SUCC (bb, 0);
      gcc_assert (EDGE_COUNT (bb->succs) == 1
                  && in_current_region_p (EDGE_SUCC (bb, 0)->dest)
                  && (BLOCK_TO_BB (e->dest->index) == 0));
      return -1;
    }

  /* We don't have preds outside the region.  We should have
     the only pred, because the multiple preds case comes from
     the pipelining of outer loops, and that is handled above.
     Just take the bbi of this single pred.  */
  if (EDGE_COUNT (bb->succs) > 0)
    {
      int pred_bbi;

      gcc_assert (EDGE_COUNT (bb->preds) == 1);

      pred_bbi = EDGE_PRED (bb, 0)->src->index;
      return BLOCK_TO_BB (pred_bbi);
    }
  else
    /* BB has no successors.  It is safe to put it in the end.  */
    return current_nr_blocks - 1;
}

/* Deletes an empty basic block freeing its data.  */
static void
delete_and_free_basic_block (basic_block bb)
{
  gcc_assert (sel_bb_empty_p (bb));

  if (BB_LV_SET (bb))
    free_lv_set (bb);

  bitmap_clear_bit (blocks_to_reschedule, bb->index);

  /* Can't assert av_set properties because we use sel_aremove_bb
     when removing loop preheader from the region.  At the point of
     removing the preheader we already have deallocated sel_region_bb_info.  */
  gcc_assert (BB_LV_SET (bb) == NULL
              && !BB_LV_SET_VALID_P (bb)
              && BB_AV_LEVEL (bb) == 0
              && BB_AV_SET (bb) == NULL);

  delete_basic_block (bb);
}

/* Add BB to the current region and update the region data.  */
static void
add_block_to_current_region (basic_block bb)
{
  int i, pos, bbi = -2, rgn;

  rgn = CONTAINING_RGN (BB_TO_BLOCK (0));
  bbi = find_place_to_insert_bb (bb, rgn);
  bbi += 1;
  pos = RGN_BLOCKS (rgn) + bbi;

  gcc_assert (RGN_HAS_REAL_EBB (rgn) == 0
              && ebb_head[bbi] == pos);

  /* Make a place for the new block.  */
  extend_regions ();

  for (i = RGN_BLOCKS (rgn + 1) - 1; i >= pos; i--)
    BLOCK_TO_BB (rgn_bb_table[i])++;

  memmove (rgn_bb_table + pos + 1,
           rgn_bb_table + pos,
           (RGN_BLOCKS (nr_regions) - pos) * sizeof (*rgn_bb_table));

  /* Initialize data for BB.  */
  rgn_bb_table[pos] = bb->index;
  BLOCK_TO_BB (bb->index) = bbi;
  CONTAINING_RGN (bb->index) = rgn;

  RGN_NR_BLOCKS (rgn)++;

  for (i = rgn + 1; i <= nr_regions; i++)
    RGN_BLOCKS (i)++;
}

/* Remove BB from the current region and update the region data.  */
static void
remove_bb_from_region (basic_block bb)
{
  int i, pos, bbi = -2, rgn;

  rgn = CONTAINING_RGN (BB_TO_BLOCK (0));
  bbi = BLOCK_TO_BB (bb->index);
  pos = RGN_BLOCKS (rgn) + bbi;

  gcc_assert (RGN_HAS_REAL_EBB (rgn) == 0
              && ebb_head[bbi] == pos);

  for (i = RGN_BLOCKS (rgn + 1) - 1; i >= pos; i--)
    BLOCK_TO_BB (rgn_bb_table[i])--;

  memmove (rgn_bb_table + pos,
           rgn_bb_table + pos + 1,
           (RGN_BLOCKS (nr_regions) - pos) * sizeof (*rgn_bb_table));

  RGN_NR_BLOCKS (rgn)--;
  for (i = rgn + 1; i <= nr_regions; i++)
    RGN_BLOCKS (i)--;
}

/* Add BB to the current region  and update all data.  If BB is NULL, add all
   blocks from last_added_blocks vector.  */
static void
sel_add_bb (basic_block bb)
{
  /* Extend luids so that new notes will receive zero luids.  */
  sched_init_luids (NULL, NULL, NULL, NULL);
  sched_init_bbs ();
  sel_init_bbs (last_added_blocks, NULL);

  /* When bb is passed explicitly, the vector should contain
     the only element that equals to bb; otherwise, the vector
     should not be NULL.  */
  gcc_assert (last_added_blocks != NULL);

  if (bb != NULL)
    {
      gcc_assert (VEC_length (basic_block, last_added_blocks) == 1
                  && VEC_index (basic_block,
                                last_added_blocks, 0) == bb);
      add_block_to_current_region (bb);

      /* We associate creating/deleting data sets with the first insn
         appearing / disappearing in the bb.  */
      if (!sel_bb_empty_p (bb) && BB_LV_SET (bb) == NULL)
	create_initial_data_sets (bb);

      VEC_free (basic_block, heap, last_added_blocks);
    }
  else
    /* BB is NULL - process LAST_ADDED_BLOCKS instead.  */
    {
      int i;
      basic_block temp_bb = NULL;

      for (i = 0;
           VEC_iterate (basic_block, last_added_blocks, i, bb); i++)
        {
          add_block_to_current_region (bb);
          temp_bb = bb;
        }

      /* We need to fetch at least one bb so we know the region
         to update.  */
      gcc_assert (temp_bb != NULL);
      bb = temp_bb;

      VEC_free (basic_block, heap, last_added_blocks);
    }

  rgn_setup_region (CONTAINING_RGN (bb->index));
}

/* Remove BB from the current region and update all data.
   If REMOVE_FROM_CFG_PBB is true, also remove the block cfom cfg.  */
static void
sel_remove_bb (basic_block bb, bool remove_from_cfg_p)
{
  gcc_assert (bb != NULL && BB_NOTE_LIST (bb) == NULL_RTX);

  remove_bb_from_region (bb);
  return_bb_to_pool (bb);
  bitmap_clear_bit (blocks_to_reschedule, bb->index);

  if (remove_from_cfg_p)
    delete_and_free_basic_block (bb);

  rgn_setup_region (CONTAINING_RGN (bb->index));
}

/* Concatenate info of EMPTY_BB to info of MERGE_BB.  */
static void
move_bb_info (basic_block merge_bb, basic_block empty_bb)
{
  gcc_assert (in_current_region_p (merge_bb));

  concat_note_lists (BB_NOTE_LIST (empty_bb),
		     &BB_NOTE_LIST (merge_bb));
  BB_NOTE_LIST (empty_bb) = NULL_RTX;

}

/* Remove an empty basic block EMPTY_BB.  When MERGE_UP_P is true, we put
   EMPTY_BB's note lists into its predecessor instead of putting them
   into the successor.  When REMOVE_FROM_CFG_P is true, also remove
   the empty block.  */
void
sel_remove_empty_bb (basic_block empty_bb, bool merge_up_p,
		     bool remove_from_cfg_p)
{
  basic_block merge_bb;

  gcc_assert (sel_bb_empty_p (empty_bb));

  if (merge_up_p)
    {
      merge_bb = empty_bb->prev_bb;
      gcc_assert (EDGE_COUNT (empty_bb->preds) == 1
		  && EDGE_PRED (empty_bb, 0)->src == merge_bb);
    }
  else
    {
      edge e;
      edge_iterator ei;

      merge_bb = bb_next_bb (empty_bb);

      /* Redirect incoming edges (except fallthrough one) of EMPTY_BB to its
         successor block.  */
      for (ei = ei_start (empty_bb->preds);
           (e = ei_safe_edge (ei)); )
        {
          if (! (e->flags & EDGE_FALLTHRU))
            sel_redirect_edge_and_branch (e, merge_bb);
          else
            ei_next (&ei);
        }

      gcc_assert (EDGE_COUNT (empty_bb->succs) == 1
		  && EDGE_SUCC (empty_bb, 0)->dest == merge_bb);
    }

  move_bb_info (merge_bb, empty_bb);
  remove_empty_bb (empty_bb, remove_from_cfg_p);
}

/* Remove EMPTY_BB.  If REMOVE_FROM_CFG_P is false, remove EMPTY_BB from
   region, but keep it in CFG.  */
static void
remove_empty_bb (basic_block empty_bb, bool remove_from_cfg_p)
{
  /* The block should contain just a note or a label.
     We try to check whether it is unused below.  */
  gcc_assert (BB_HEAD (empty_bb) == BB_END (empty_bb)
              || LABEL_P (BB_HEAD (empty_bb)));

  /* If basic block has predecessors or successors, redirect them.  */
  if (remove_from_cfg_p
      && (EDGE_COUNT (empty_bb->preds) > 0
	  || EDGE_COUNT (empty_bb->succs) > 0))
    {
      basic_block pred;
      basic_block succ;

      /* We need to init PRED and SUCC before redirecting edges.  */
      if (EDGE_COUNT (empty_bb->preds) > 0)
	{
	  edge e;

	  gcc_assert (EDGE_COUNT (empty_bb->preds) == 1);

	  e = EDGE_PRED (empty_bb, 0);
          gcc_assert (e->src == empty_bb->prev_bb
		      && (e->flags & EDGE_FALLTHRU));

	  pred = empty_bb->prev_bb;
	}
      else
	pred = NULL;

      if (EDGE_COUNT (empty_bb->succs) > 0)
	{
          /* We do not check fallthruness here as above, because
             after removing a jump the edge may actually be not fallthru.  */
	  gcc_assert (EDGE_COUNT (empty_bb->succs) == 1);
	  succ = EDGE_SUCC (empty_bb, 0)->dest;
	}
      else
	succ = NULL;

      if (EDGE_COUNT (empty_bb->preds) > 0 && succ != NULL)
        {
          edge e = EDGE_PRED (empty_bb, 0);

          if (e->flags & EDGE_FALLTHRU)
            redirect_edge_succ_nodup (e, succ);
          else
            sel_redirect_edge_and_branch (EDGE_PRED (empty_bb, 0), succ);
        }

      if (EDGE_COUNT (empty_bb->succs) > 0 && pred != NULL)
	{
	  edge e = EDGE_SUCC (empty_bb, 0);

	  if (find_edge (pred, e->dest) == NULL)
	    redirect_edge_pred (e, pred);
	}
    }

  /* Finish removing.  */
  sel_remove_bb (empty_bb, remove_from_cfg_p);
}

/* An implementation of create_basic_block hook, which additionally updates
   per-bb data structures.  */
static basic_block
sel_create_basic_block (void *headp, void *endp, basic_block after)
{
  basic_block new_bb;
  insn_t new_bb_note;

  gcc_assert (flag_sel_sched_pipelining_outer_loops
              || last_added_blocks == NULL);

  new_bb_note = get_bb_note_from_pool ();

  if (new_bb_note == NULL_RTX)
    new_bb = orig_cfg_hooks.create_basic_block (headp, endp, after);
  else
    {
      new_bb = create_basic_block_structure ((rtx) headp, (rtx) endp,
					     new_bb_note, after);
      new_bb->aux = NULL;
    }

  VEC_safe_push (basic_block, heap, last_added_blocks, new_bb);

  return new_bb;
}

/* Implement sched_init_only_bb ().  */
static void
sel_init_only_bb (basic_block bb, basic_block after)
{
  gcc_assert (after == NULL);

  extend_regions ();
  rgn_make_new_region_out_of_new_block (bb);
}

/* Update the latch when we've splitted or merged it from FROM block to TO.
   This should be checked for all outer loops, too.  */
static void
change_loops_latches (basic_block from, basic_block to)
{
  gcc_assert (from != to);

  if (current_loop_nest)
    {
      struct loop *loop;

      for (loop = current_loop_nest; loop; loop = loop_outer (loop))
        if (considered_for_pipelining_p (loop) && loop->latch == from)
          {
            gcc_assert (loop == current_loop_nest);
            loop->latch = to;
            gcc_assert (loop_latch_edge (loop));
          }
    }
}

/* Splits BB on two basic blocks, adding it to the region and extending
   per-bb data structures.  Returns the newly created bb.  */
static basic_block
sel_split_block (basic_block bb, rtx after)
{
  basic_block new_bb;
  insn_t insn;

  new_bb = sched_split_block_1 (bb, after);
  sel_add_bb (new_bb);

  /* This should be called after sel_add_bb, because this uses
     CONTAINING_RGN for the new block, which is not yet initialized.
     FIXME: this function may be a no-op now.  */
  change_loops_latches (bb, new_bb);

  /* Update ORIG_BB_INDEX for insns moved into the new block.  */
  FOR_BB_INSNS (new_bb, insn)
   if (INSN_P (insn))
     EXPR_ORIG_BB_INDEX (INSN_EXPR (insn)) = new_bb->index;

  if (sel_bb_empty_p (bb))
    {
      gcc_assert (!sel_bb_empty_p (new_bb));

      /* NEW_BB has data sets that need to be updated and BB holds
	 data sets that should be removed.  Exchange these data sets
	 so that we won't lose BB's valid data sets.  */
      exchange_data_sets (new_bb, bb);
      free_data_sets (bb);
    }

  if (!sel_bb_empty_p (new_bb)
      && bitmap_bit_p (blocks_to_reschedule, bb->index))
    bitmap_set_bit (blocks_to_reschedule, new_bb->index);

  return new_bb;
}

/* If BB ends with a jump insn whose ID is bigger then PREV_MAX_UID, return it.
   Otherwise returns NULL.  */
static rtx
check_for_new_jump (basic_block bb, int prev_max_uid)
{
  rtx end;

  end = sel_bb_end (bb);
  if (end && INSN_UID (end) >= prev_max_uid)
    return end;
  return NULL;
}

/* Look for a new jump either in FROM_BB block or in newly created JUMP_BB block.
   New means having UID at least equal to PREV_MAX_UID.  */
static rtx
find_new_jump (basic_block from, basic_block jump_bb, int prev_max_uid)
{
  rtx jump;

  /* Return immediately if no new insns were emitted.  */
  if (get_max_uid () == prev_max_uid)
    return NULL;

  /* Now check both blocks for new jumps.  It will ever be only one.  */
  if ((jump = check_for_new_jump (from, prev_max_uid)))
    return jump;

  if (jump_bb != NULL
      && (jump = check_for_new_jump (jump_bb, prev_max_uid)))
    return jump;
  return NULL;
}

/* Splits E and adds the newly created basic block to the current region.
   Returns this basic block.  */
basic_block
sel_split_edge (edge e)
{
  basic_block new_bb, src, other_bb = NULL;
  int prev_max_uid;
  rtx jump;

  src = e->src;
  prev_max_uid = get_max_uid ();
  new_bb = split_edge (e);

  if (flag_sel_sched_pipelining_outer_loops
      && current_loop_nest)
    {
      int i;
      basic_block bb;

      /* Some of the basic blocks might not have been added to the loop.
         Add them here, until this is fixed in force_fallthru.  */
      for (i = 0;
           VEC_iterate (basic_block, last_added_blocks, i, bb); i++)
        if (!bb->loop_father)
          {
            add_bb_to_loop (bb, e->dest->loop_father);

            gcc_assert (!other_bb && (new_bb->index != bb->index));
            other_bb = bb;
          }
    }

  /* Add all last_added_blocks to the region.  */
  sel_add_bb (NULL);

  jump = find_new_jump (src, new_bb, prev_max_uid);
  if (jump)
    sel_init_new_insn (jump, INSN_INIT_TODO_LUID | INSN_INIT_TODO_SIMPLEJUMP);

  /* Put the correct lv set on this block.  */
  if (other_bb && !sel_bb_empty_p (other_bb))
    compute_live (sel_bb_head (other_bb));

  return new_bb;
}

/* Implement sched_create_empty_bb ().  */
static basic_block
sel_create_empty_bb (basic_block after)
{
  basic_block new_bb;

  new_bb = sched_create_empty_bb_1 (after);

  /* We'll explicitly initialize NEW_BB via sel_init_only_bb () a bit
     later.  */
  gcc_assert (VEC_length (basic_block, last_added_blocks) == 1
	      && VEC_index (basic_block, last_added_blocks, 0) == new_bb);

  VEC_free (basic_block, heap, last_added_blocks);
  return new_bb;
}

/* Implement sched_create_recovery_block.  ORIG_INSN is where block
   will be splitted to insert a check.  */
basic_block
sel_create_recovery_block (insn_t orig_insn)
{
  basic_block first_bb, second_bb, recovery_block;
  basic_block before_recovery = NULL;
  rtx jump;

  first_bb = BLOCK_FOR_INSN (orig_insn);
  if (sel_bb_end_p (orig_insn))
    {
      /* Avoid introducing an empty block while splitting.  */
      gcc_assert (single_succ_p (first_bb));
      second_bb = single_succ (first_bb);
    }
  else
    second_bb = sched_split_block (first_bb, orig_insn);

  recovery_block = sched_create_recovery_block (&before_recovery);
  if (before_recovery)
    copy_lv_set_from (before_recovery, EXIT_BLOCK_PTR);

  gcc_assert (sel_bb_empty_p (recovery_block));
  sched_create_recovery_edges (first_bb, recovery_block, second_bb);
  if (current_loops != NULL)
    add_bb_to_loop (recovery_block, first_bb->loop_father);

  sel_add_bb (recovery_block);

  jump = BB_END (recovery_block);
  gcc_assert (sel_bb_head (recovery_block) == jump);
  sel_init_new_insn (jump, INSN_INIT_TODO_LUID | INSN_INIT_TODO_SIMPLEJUMP);

  return recovery_block;
}

/* Merge basic block B into basic block A.  */
void
sel_merge_blocks (basic_block a, basic_block b)
{
  sel_remove_empty_bb (b, true, false);
  merge_blocks (a, b);

  change_loops_latches (b, a);
}

/* A wrapper for redirect_edge_and_branch_force, which also initializes
   data structures for possibly created bb and insns.  Returns the newly
   added bb or NULL, when a bb was not needed.  */
void
sel_redirect_edge_and_branch_force (edge e, basic_block to)
{
  basic_block jump_bb, src;
  int prev_max_uid;
  rtx jump;

  gcc_assert (!sel_bb_empty_p (e->src));

  src = e->src;
  prev_max_uid = get_max_uid ();
  jump_bb = redirect_edge_and_branch_force (e, to);

  if (jump_bb != NULL)
    sel_add_bb (jump_bb);

  /* This function could not be used to spoil the loop structure by now,
     thus we don't care to update anything.  But check it to be sure.  */
  if (current_loop_nest
      && pipelining_p)
    gcc_assert (loop_latch_edge (current_loop_nest));

  jump = find_new_jump (src, jump_bb, prev_max_uid);
  if (jump)
    sel_init_new_insn (jump, INSN_INIT_TODO_LUID | INSN_INIT_TODO_SIMPLEJUMP);
}

/* A wrapper for redirect_edge_and_branch.  Return TRUE if blocks connected by
   redirected edge are in reverse topological order.  */
bool
sel_redirect_edge_and_branch (edge e, basic_block to)
{
  bool latch_edge_p;
  basic_block src;
  int prev_max_uid;
  rtx jump;
  edge redirected;
  bool recompute_toporder_p = false;

  latch_edge_p = (pipelining_p
                  && current_loop_nest
                  && e == loop_latch_edge (current_loop_nest));

  src = e->src;
  prev_max_uid = get_max_uid ();

  redirected = redirect_edge_and_branch (e, to);

  gcc_assert (redirected && last_added_blocks == NULL);

  /* When we've redirected a latch edge, update the header.  */
  if (latch_edge_p)
    {
      current_loop_nest->header = to;
      gcc_assert (loop_latch_edge (current_loop_nest));
    }

  /* In rare situations, the topological relation between the blocks connected
     by the redirected edge can change (see PR42245 for an example).  Update
     block_to_bb/bb_to_block.  */
  if (CONTAINING_RGN (e->src->index) == CONTAINING_RGN (to->index)
      && BLOCK_TO_BB (e->src->index) > BLOCK_TO_BB (to->index))
    recompute_toporder_p = true;

  jump = find_new_jump (src, NULL, prev_max_uid);
  if (jump)
    sel_init_new_insn (jump, INSN_INIT_TODO_LUID | INSN_INIT_TODO_SIMPLEJUMP);

  return recompute_toporder_p;
}

/* This variable holds the cfg hooks used by the selective scheduler.  */
static struct cfg_hooks sel_cfg_hooks;

/* Register sel-sched cfg hooks.  */
void
sel_register_cfg_hooks (void)
{
  sched_split_block = sel_split_block;

  orig_cfg_hooks = get_cfg_hooks ();
  sel_cfg_hooks = orig_cfg_hooks;

  sel_cfg_hooks.create_basic_block = sel_create_basic_block;

  set_cfg_hooks (sel_cfg_hooks);

  sched_init_only_bb = sel_init_only_bb;
  sched_split_block = sel_split_block;
  sched_create_empty_bb = sel_create_empty_bb;
}

/* Unregister sel-sched cfg hooks.  */
void
sel_unregister_cfg_hooks (void)
{
  sched_create_empty_bb = NULL;
  sched_split_block = NULL;
  sched_init_only_bb = NULL;

  set_cfg_hooks (orig_cfg_hooks);
}


/* Emit an insn rtx based on PATTERN.  If a jump insn is wanted,
   LABEL is where this jump should be directed.  */
rtx
create_insn_rtx_from_pattern (rtx pattern, rtx label)
{
  rtx insn_rtx;

  gcc_assert (!INSN_P (pattern));

  start_sequence ();

  if (label == NULL_RTX)
    insn_rtx = emit_insn (pattern);
  else if (DEBUG_INSN_P (label))
    insn_rtx = emit_debug_insn (pattern);
  else
    {
      insn_rtx = emit_jump_insn (pattern);
      JUMP_LABEL (insn_rtx) = label;
      ++LABEL_NUSES (label);
    }

  end_sequence ();

  sched_init_luids (NULL, NULL, NULL, NULL);
  sched_extend_target ();
  sched_deps_init (false);

  /* Initialize INSN_CODE now.  */
  recog_memoized (insn_rtx);
  return insn_rtx;
}

/* Create a new vinsn for INSN_RTX.  FORCE_UNIQUE_P is true when the vinsn
   must not be clonable.  */
vinsn_t
create_vinsn_from_insn_rtx (rtx insn_rtx, bool force_unique_p)
{
  gcc_assert (INSN_P (insn_rtx) && !INSN_IN_STREAM_P (insn_rtx));

  /* If VINSN_TYPE is not USE, retain its uniqueness.  */
  return vinsn_create (insn_rtx, force_unique_p);
}

/* Create a copy of INSN_RTX.  */
rtx
create_copy_of_insn_rtx (rtx insn_rtx)
{
  rtx res;

  if (DEBUG_INSN_P (insn_rtx))
    return create_insn_rtx_from_pattern (copy_rtx (PATTERN (insn_rtx)),
					 insn_rtx);

  gcc_assert (NONJUMP_INSN_P (insn_rtx));

  res = create_insn_rtx_from_pattern (copy_rtx (PATTERN (insn_rtx)),
                                      NULL_RTX);
  return res;
}

/* Change vinsn field of EXPR to hold NEW_VINSN.  */
void
change_vinsn_in_expr (expr_t expr, vinsn_t new_vinsn)
{
  vinsn_detach (EXPR_VINSN (expr));

  EXPR_VINSN (expr) = new_vinsn;
  vinsn_attach (new_vinsn);
}

/* Helpers for global init.  */
/* This structure is used to be able to call existing bundling mechanism
   and calculate insn priorities.  */
static struct haifa_sched_info sched_sel_haifa_sched_info =
{
  NULL, /* init_ready_list */
  NULL, /* can_schedule_ready_p */
  NULL, /* schedule_more_p */
  NULL, /* new_ready */
  NULL, /* rgn_rank */
  sel_print_insn, /* rgn_print_insn */
  contributes_to_priority,
  NULL, /* insn_finishes_block_p */

  NULL, NULL,
  NULL, NULL,
  0, 0,

  NULL, /* add_remove_insn */
  NULL, /* begin_schedule_ready */
  NULL, /* advance_target_bb */
  SEL_SCHED | NEW_BBS
};

/* Setup special insns used in the scheduler.  */
void
setup_nop_and_exit_insns (void)
{
  gcc_assert (nop_pattern == NULL_RTX
	      && exit_insn == NULL_RTX);

  nop_pattern = gen_nop ();

  start_sequence ();
  emit_insn (nop_pattern);
  exit_insn = get_insns ();
  end_sequence ();
  set_block_for_insn (exit_insn, EXIT_BLOCK_PTR);
}

/* Free special insns used in the scheduler.  */
void
free_nop_and_exit_insns (void)
{
  exit_insn = NULL_RTX;
  nop_pattern = NULL_RTX;
}

/* Setup a special vinsn used in new insns initialization.  */
void
setup_nop_vinsn (void)
{
  nop_vinsn = vinsn_create (exit_insn, false);
  vinsn_attach (nop_vinsn);
}

/* Free a special vinsn used in new insns initialization.  */
void
free_nop_vinsn (void)
{
  gcc_assert (VINSN_COUNT (nop_vinsn) == 1);
  vinsn_detach (nop_vinsn);
  nop_vinsn = NULL;
}

/* Call a set_sched_flags hook.  */
void
sel_set_sched_flags (void)
{
  /* ??? This means that set_sched_flags were called, and we decided to
     support speculation.  However, set_sched_flags also modifies flags
     on current_sched_info, doing this only at global init.  And we
     sometimes change c_s_i later.  So put the correct flags again.  */
  if (spec_info && targetm.sched.set_sched_flags)
    targetm.sched.set_sched_flags (spec_info);
}

/* Setup pointers to global sched info structures.  */
void
sel_setup_sched_infos (void)
{
  rgn_setup_common_sched_info ();

  memcpy (&sel_common_sched_info, common_sched_info,
	  sizeof (sel_common_sched_info));

  sel_common_sched_info.fix_recovery_cfg = NULL;
  sel_common_sched_info.add_block = NULL;
  sel_common_sched_info.estimate_number_of_insns
    = sel_estimate_number_of_insns;
  sel_common_sched_info.luid_for_non_insn = sel_luid_for_non_insn;
  sel_common_sched_info.sched_pass_id = SCHED_SEL_PASS;

  common_sched_info = &sel_common_sched_info;

  current_sched_info = &sched_sel_haifa_sched_info;
  current_sched_info->sched_max_insns_priority =
    get_rgn_sched_max_insns_priority ();

  sel_set_sched_flags ();
}


/* Adds basic block BB to region RGN at the position *BB_ORD_INDEX,
   *BB_ORD_INDEX after that is increased.  */
static void
sel_add_block_to_region (basic_block bb, int *bb_ord_index, int rgn)
{
  RGN_NR_BLOCKS (rgn) += 1;
  RGN_DONT_CALC_DEPS (rgn) = 0;
  RGN_HAS_REAL_EBB (rgn) = 0;
  CONTAINING_RGN (bb->index) = rgn;
  BLOCK_TO_BB (bb->index) = *bb_ord_index;
  rgn_bb_table[RGN_BLOCKS (rgn) + *bb_ord_index] = bb->index;
  (*bb_ord_index)++;

  /* FIXME: it is true only when not scheduling ebbs.  */
  RGN_BLOCKS (rgn + 1) = RGN_BLOCKS (rgn) + RGN_NR_BLOCKS (rgn);
}

/* Functions to support pipelining of outer loops.  */

/* Creates a new empty region and returns it's number.  */
static int
sel_create_new_region (void)
{
  int new_rgn_number = nr_regions;

  RGN_NR_BLOCKS (new_rgn_number) = 0;

  /* FIXME: This will work only when EBBs are not created.  */
  if (new_rgn_number != 0)
    RGN_BLOCKS (new_rgn_number) = RGN_BLOCKS (new_rgn_number - 1) +
      RGN_NR_BLOCKS (new_rgn_number - 1);
  else
    RGN_BLOCKS (new_rgn_number) = 0;

  /* Set the blocks of the next region so the other functions may
     calculate the number of blocks in the region.  */
  RGN_BLOCKS (new_rgn_number + 1) = RGN_BLOCKS (new_rgn_number) +
    RGN_NR_BLOCKS (new_rgn_number);

  nr_regions++;

  return new_rgn_number;
}

/* If X has a smaller topological sort number than Y, returns -1;
   if greater, returns 1.  */
static int
bb_top_order_comparator (const void *x, const void *y)
{
  basic_block bb1 = *(const basic_block *) x;
  basic_block bb2 = *(const basic_block *) y;

  gcc_assert (bb1 == bb2
	      || rev_top_order_index[bb1->index]
		 != rev_top_order_index[bb2->index]);

  /* It's a reverse topological order in REV_TOP_ORDER_INDEX, so
     bbs with greater number should go earlier.  */
  if (rev_top_order_index[bb1->index] > rev_top_order_index[bb2->index])
    return -1;
  else
    return 1;
}

/* Create a region for LOOP and return its number.  If we don't want
   to pipeline LOOP, return -1.  */
static int
make_region_from_loop (struct loop *loop)
{
  unsigned int i;
  int new_rgn_number = -1;
  struct loop *inner;

  /* Basic block index, to be assigned to BLOCK_TO_BB.  */
  int bb_ord_index = 0;
  basic_block *loop_blocks;
  basic_block preheader_block;

  if (loop->num_nodes
      > (unsigned) PARAM_VALUE (PARAM_MAX_PIPELINE_REGION_BLOCKS))
    return -1;

  /* Don't pipeline loops whose latch belongs to some of its inner loops.  */
  for (inner = loop->inner; inner; inner = inner->inner)
    if (flow_bb_inside_loop_p (inner, loop->latch))
      return -1;

  loop->ninsns = num_loop_insns (loop);
  if ((int) loop->ninsns > PARAM_VALUE (PARAM_MAX_PIPELINE_REGION_INSNS))
    return -1;

  loop_blocks = get_loop_body_in_custom_order (loop, bb_top_order_comparator);

  for (i = 0; i < loop->num_nodes; i++)
    if (loop_blocks[i]->flags & BB_IRREDUCIBLE_LOOP)
      {
	free (loop_blocks);
	return -1;
      }

  preheader_block = loop_preheader_edge (loop)->src;
  gcc_assert (preheader_block);
  gcc_assert (loop_blocks[0] == loop->header);

  new_rgn_number = sel_create_new_region ();

  sel_add_block_to_region (preheader_block, &bb_ord_index, new_rgn_number);
  SET_BIT (bbs_in_loop_rgns, preheader_block->index);

  for (i = 0; i < loop->num_nodes; i++)
    {
      /* Add only those blocks that haven't been scheduled in the inner loop.
	 The exception is the basic blocks with bookkeeping code - they should
	 be added to the region (and they actually don't belong to the loop
	 body, but to the region containing that loop body).  */

      gcc_assert (new_rgn_number >= 0);

      if (! TEST_BIT (bbs_in_loop_rgns, loop_blocks[i]->index))
	{
	  sel_add_block_to_region (loop_blocks[i], &bb_ord_index,
                                   new_rgn_number);
	  SET_BIT (bbs_in_loop_rgns, loop_blocks[i]->index);
	}
    }

  free (loop_blocks);
  MARK_LOOP_FOR_PIPELINING (loop);

  return new_rgn_number;
}

/* Create a new region from preheader blocks LOOP_BLOCKS.  */
void
make_region_from_loop_preheader (VEC(basic_block, heap) **loop_blocks)
{
  unsigned int i;
  int new_rgn_number = -1;
  basic_block bb;

  /* Basic block index, to be assigned to BLOCK_TO_BB.  */
  int bb_ord_index = 0;

  new_rgn_number = sel_create_new_region ();

  for (i = 0; VEC_iterate (basic_block, *loop_blocks, i, bb); i++)
    {
      gcc_assert (new_rgn_number >= 0);

      sel_add_block_to_region (bb, &bb_ord_index, new_rgn_number);
    }

  VEC_free (basic_block, heap, *loop_blocks);
  gcc_assert (*loop_blocks == NULL);
}


/* Create region(s) from loop nest LOOP, such that inner loops will be
   pipelined before outer loops.  Returns true when a region for LOOP
   is created.  */
static bool
make_regions_from_loop_nest (struct loop *loop)
{
  struct loop *cur_loop;
  int rgn_number;

  /* Traverse all inner nodes of the loop.  */
  for (cur_loop = loop->inner; cur_loop; cur_loop = cur_loop->next)
    if (! TEST_BIT (bbs_in_loop_rgns, cur_loop->header->index))
      return false;

  /* At this moment all regular inner loops should have been pipelined.
     Try to create a region from this loop.  */
  rgn_number = make_region_from_loop (loop);

  if (rgn_number < 0)
    return false;

  VEC_safe_push (loop_p, heap, loop_nests, loop);
  return true;
}

/* Initalize data structures needed.  */
void
sel_init_pipelining (void)
{
  /* Collect loop information to be used in outer loops pipelining.  */
  loop_optimizer_init (LOOPS_HAVE_PREHEADERS
                       | LOOPS_HAVE_FALLTHRU_PREHEADERS
		       | LOOPS_HAVE_RECORDED_EXITS
		       | LOOPS_HAVE_MARKED_IRREDUCIBLE_REGIONS);
  current_loop_nest = NULL;

  bbs_in_loop_rgns = sbitmap_alloc (last_basic_block);
  sbitmap_zero (bbs_in_loop_rgns);

  recompute_rev_top_order ();
}

/* Returns a struct loop for region RGN.  */
loop_p
get_loop_nest_for_rgn (unsigned int rgn)
{
  /* Regions created with extend_rgns don't have corresponding loop nests,
     because they don't represent loops.  */
  if (rgn < VEC_length (loop_p, loop_nests))
    return VEC_index (loop_p, loop_nests, rgn);
  else
    return NULL;
}

/* True when LOOP was included into pipelining regions.   */
bool
considered_for_pipelining_p (struct loop *loop)
{
  if (loop_depth (loop) == 0)
    return false;

  /* Now, the loop could be too large or irreducible.  Check whether its
     region is in LOOP_NESTS.
     We determine the region number of LOOP as the region number of its
     latch.  We can't use header here, because this header could be
     just removed preheader and it will give us the wrong region number.
     Latch can't be used because it could be in the inner loop too.  */
  if (LOOP_MARKED_FOR_PIPELINING_P (loop))
    {
      int rgn = CONTAINING_RGN (loop->latch->index);

      gcc_assert ((unsigned) rgn < VEC_length (loop_p, loop_nests));
      return true;
    }

  return false;
}

/* Makes regions from the rest of the blocks, after loops are chosen
   for pipelining.  */
static void
make_regions_from_the_rest (void)
{
  int cur_rgn_blocks;
  int *loop_hdr;
  int i;

  basic_block bb;
  edge e;
  edge_iterator ei;
  int *degree;

  /* Index in rgn_bb_table where to start allocating new regions.  */
  cur_rgn_blocks = nr_regions ? RGN_BLOCKS (nr_regions) : 0;

  /* Make regions from all the rest basic blocks - those that don't belong to
     any loop or belong to irreducible loops.  Prepare the data structures
     for extend_rgns.  */

  /* LOOP_HDR[I] == -1 if I-th bb doesn't belong to any loop,
     LOOP_HDR[I] == LOOP_HDR[J] iff basic blocks I and J reside within the same
     loop.  */
  loop_hdr = XNEWVEC (int, last_basic_block);
  degree = XCNEWVEC (int, last_basic_block);


  /* For each basic block that belongs to some loop assign the number
     of innermost loop it belongs to.  */
  for (i = 0; i < last_basic_block; i++)
    loop_hdr[i] = -1;

  FOR_EACH_BB (bb)
    {
      if (bb->loop_father && !bb->loop_father->num == 0
	  && !(bb->flags & BB_IRREDUCIBLE_LOOP))
	loop_hdr[bb->index] = bb->loop_father->num;
    }

  /* For each basic block degree is calculated as the number of incoming
     edges, that are going out of bbs that are not yet scheduled.
     The basic blocks that are scheduled have degree value of zero.  */
  FOR_EACH_BB (bb)
    {
      degree[bb->index] = 0;

      if (!TEST_BIT (bbs_in_loop_rgns, bb->index))
	{
	  FOR_EACH_EDGE (e, ei, bb->preds)
	    if (!TEST_BIT (bbs_in_loop_rgns, e->src->index))
	      degree[bb->index]++;
	}
      else
	degree[bb->index] = -1;
    }

  extend_rgns (degree, &cur_rgn_blocks, bbs_in_loop_rgns, loop_hdr);

  /* Any block that did not end up in a region is placed into a region
     by itself.  */
  FOR_EACH_BB (bb)
    if (degree[bb->index] >= 0)
      {
	rgn_bb_table[cur_rgn_blocks] = bb->index;
	RGN_NR_BLOCKS (nr_regions) = 1;
	RGN_BLOCKS (nr_regions) = cur_rgn_blocks++;
        RGN_DONT_CALC_DEPS (nr_regions) = 0;
	RGN_HAS_REAL_EBB (nr_regions) = 0;
	CONTAINING_RGN (bb->index) = nr_regions++;
	BLOCK_TO_BB (bb->index) = 0;
      }

  free (degree);
  free (loop_hdr);
}

/* Free data structures used in pipelining of loops.  */
void sel_finish_pipelining (void)
{
  loop_iterator li;
  struct loop *loop;

  /* Release aux fields so we don't free them later by mistake.  */
  FOR_EACH_LOOP (li, loop, 0)
    loop->aux = NULL;

  loop_optimizer_finalize ();

  VEC_free (loop_p, heap, loop_nests);

  free (rev_top_order_index);
  rev_top_order_index = NULL;
}

/* This function replaces the find_rgns when
   FLAG_SEL_SCHED_PIPELINING_OUTER_LOOPS is set.  */
void
sel_find_rgns (void)
{
  sel_init_pipelining ();
  extend_regions ();

  if (current_loops)
    {
      loop_p loop;
      loop_iterator li;

      FOR_EACH_LOOP (li, loop, (flag_sel_sched_pipelining_outer_loops
				? LI_FROM_INNERMOST
				: LI_ONLY_INNERMOST))
	make_regions_from_loop_nest (loop);
    }

  /* Make regions from all the rest basic blocks and schedule them.
     These blocks include blocks that don't belong to any loop or belong
     to irreducible loops.  */
  make_regions_from_the_rest ();

  /* We don't need bbs_in_loop_rgns anymore.  */
  sbitmap_free (bbs_in_loop_rgns);
  bbs_in_loop_rgns = NULL;
}

/* Adds the preheader blocks from previous loop to current region taking
   it from LOOP_PREHEADER_BLOCKS (current_loop_nest).
   This function is only used with -fsel-sched-pipelining-outer-loops.  */
void
sel_add_loop_preheaders (void)
{
  int i;
  basic_block bb;
  VEC(basic_block, heap) *preheader_blocks
    = LOOP_PREHEADER_BLOCKS (current_loop_nest);

  for (i = 0;
       VEC_iterate (basic_block, preheader_blocks, i, bb);
       i++)
    {
      VEC_safe_push (basic_block, heap, last_added_blocks, bb);
      sel_add_bb (bb);
    }

  VEC_free (basic_block, heap, preheader_blocks);
}

/* While pipelining outer loops, returns TRUE if BB is a loop preheader.
   Please note that the function should also work when pipelining_p is
   false, because it is used when deciding whether we should or should
   not reschedule pipelined code.  */
bool
sel_is_loop_preheader_p (basic_block bb)
{
  if (current_loop_nest)
    {
      struct loop *outer;

      if (preheader_removed)
        return false;

      /* Preheader is the first block in the region.  */
      if (BLOCK_TO_BB (bb->index) == 0)
        return true;

      /* We used to find a preheader with the topological information.
         Check that the above code is equivalent to what we did before.  */

      if (in_current_region_p (current_loop_nest->header))
	gcc_assert (!(BLOCK_TO_BB (bb->index)
		      < BLOCK_TO_BB (current_loop_nest->header->index)));

      /* Support the situation when the latch block of outer loop
         could be from here.  */
      for (outer = loop_outer (current_loop_nest);
	   outer;
	   outer = loop_outer (outer))
        if (considered_for_pipelining_p (outer) && outer->latch == bb)
          gcc_unreachable ();
    }

  return false;
}

/* Checks whether JUMP leads to basic block DEST_BB and no other blocks.  */
bool
jump_leads_only_to_bb_p (insn_t jump, basic_block dest_bb)
{
  basic_block jump_bb = BLOCK_FOR_INSN (jump);

  /* It is not jump, jump with side-effects or jump can lead to several
     basic blocks.  */
  if (!onlyjump_p (jump)
      || !any_uncondjump_p (jump))
    return false;

  /* Several outgoing edges, abnormal edge or destination of jump is
     not DEST_BB.  */
  if (EDGE_COUNT (jump_bb->succs) != 1
      || EDGE_SUCC (jump_bb, 0)->flags & EDGE_ABNORMAL
      || EDGE_SUCC (jump_bb, 0)->dest != dest_bb)
    return false;

  /* If not anything of the upper.  */
  return true;
}

/* Removes the loop preheader from the current region and saves it in
   PREHEADER_BLOCKS of the father loop, so they will be added later to
   region that represents an outer loop.  */
static void
sel_remove_loop_preheader (void)
{
  int i, old_len;
  int cur_rgn = CONTAINING_RGN (BB_TO_BLOCK (0));
  basic_block bb;
  bool all_empty_p = true;
  VEC(basic_block, heap) *preheader_blocks
    = LOOP_PREHEADER_BLOCKS (loop_outer (current_loop_nest));

  gcc_assert (current_loop_nest);
  old_len = VEC_length (basic_block, preheader_blocks);

  /* Add blocks that aren't within the current loop to PREHEADER_BLOCKS.  */
  for (i = 0; i < RGN_NR_BLOCKS (cur_rgn); i++)
    {
      bb = BASIC_BLOCK (BB_TO_BLOCK (i));

      /* If the basic block belongs to region, but doesn't belong to
	 corresponding loop, then it should be a preheader.  */
      if (sel_is_loop_preheader_p (bb))
        {
          VEC_safe_push (basic_block, heap, preheader_blocks, bb);
          if (BB_END (bb) != bb_note (bb))
            all_empty_p = false;
        }
    }

  /* Remove these blocks only after iterating over the whole region.  */
  for (i = VEC_length (basic_block, preheader_blocks) - 1;
       i >= old_len;
       i--)
    {
      bb =  VEC_index (basic_block, preheader_blocks, i);
      sel_remove_bb (bb, false);
    }

  if (!considered_for_pipelining_p (loop_outer (current_loop_nest)))
    {
      if (!all_empty_p)
        /* Immediately create new region from preheader.  */
        make_region_from_loop_preheader (&preheader_blocks);
      else
        {
          /* If all preheader blocks are empty - dont create new empty region.
             Instead, remove them completely.  */
          for (i = 0; VEC_iterate (basic_block, preheader_blocks, i, bb); i++)
            {
              edge e;
              edge_iterator ei;
              basic_block prev_bb = bb->prev_bb, next_bb = bb->next_bb;

              /* Redirect all incoming edges to next basic block.  */
              for (ei = ei_start (bb->preds); (e = ei_safe_edge (ei)); )
                {
                  if (! (e->flags & EDGE_FALLTHRU))
                    redirect_edge_and_branch (e, bb->next_bb);
                  else
                    redirect_edge_succ (e, bb->next_bb);
                }
              gcc_assert (BB_NOTE_LIST (bb) == NULL);
              delete_and_free_basic_block (bb);

              /* Check if after deleting preheader there is a nonconditional
                 jump in PREV_BB that leads to the next basic block NEXT_BB.
                 If it is so - delete this jump and clear data sets of its
                 basic block if it becomes empty.  */
	      if (next_bb->prev_bb == prev_bb
                  && prev_bb != ENTRY_BLOCK_PTR
                  && jump_leads_only_to_bb_p (BB_END (prev_bb), next_bb))
                {
                  redirect_edge_and_branch (EDGE_SUCC (prev_bb, 0), next_bb);
                  if (BB_END (prev_bb) == bb_note (prev_bb))
                    free_data_sets (prev_bb);
                }
            }
        }
      VEC_free (basic_block, heap, preheader_blocks);
    }
  else
    /* Store preheader within the father's loop structure.  */
    SET_LOOP_PREHEADER_BLOCKS (loop_outer (current_loop_nest),
			       preheader_blocks);
}
#endif
