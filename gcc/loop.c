/* Perform various loop optimizations, including strength reduction.
   Copyright (C) 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995, 1996, 1997,
   1998, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.

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

/* This is the loop optimization pass of the compiler.
   It finds invariant computations within loops and moves them
   to the beginning of the loop.  Then it identifies basic and
   general induction variables.  Strength reduction is applied to the general
   induction variables, and induction variable elimination is applied to
   the basic induction variables.

   It also finds cases where
   a register is set within the loop by zero-extending a narrower value
   and changes these to zero the entire register once before the loop
   and merely copy the low part within the loop.

   Most of the complexity is in heuristics to decide when it is worth
   while to do these things.  */

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tm_p.h"
#include "obstack.h"
#include "function.h"
#include "expr.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "insn-config.h"
#include "regs.h"
#include "recog.h"
#include "flags.h"
#include "real.h"
#include "loop.h"
#include "cselib.h"
#include "except.h"
#include "toplev.h"
#include "predict.h"
#include "insn-flags.h"
#include "optabs.h"

/* Not really meaningful values, but at least something.  */
#ifndef SIMULTANEOUS_PREFETCHES
#define SIMULTANEOUS_PREFETCHES 3
#endif
#ifndef PREFETCH_BLOCK
#define PREFETCH_BLOCK 32
#endif
#ifndef HAVE_prefetch
#define HAVE_prefetch 0
#define CODE_FOR_prefetch 0
#define gen_prefetch(a,b,c) (abort(), NULL_RTX)
#endif

/* Give up the prefetch optimizations once we exceed a given threshhold.
   It is unlikely that we would be able to optimize something in a loop
   with so many detected prefetches.  */
#define MAX_PREFETCHES 100
/* The number of prefetch blocks that are beneficial to fetch at once before
   a loop with a known (and low) iteration count.  */
#define PREFETCH_BLOCKS_BEFORE_LOOP_MAX  6
/* For very tiny loops it is not worthwhile to prefetch even before the loop,
   since it is likely that the data are already in the cache.  */
#define PREFETCH_BLOCKS_BEFORE_LOOP_MIN  2
/* The minimal number of prefetch blocks that a loop must consume to make
   the emitting of prefetch instruction in the body of loop worthwhile.  */
#define PREFETCH_BLOCKS_IN_LOOP_MIN  6

/* Parameterize some prefetch heuristics so they can be turned on and off
   easily for performance testing on new architecures.  These can be
   defined in target-dependent files.  */

/* Prefetch is worthwhile only when loads/stores are dense.  */
#ifndef PREFETCH_ONLY_DENSE_MEM
#define PREFETCH_ONLY_DENSE_MEM 1
#endif

/* Define what we mean by "dense" loads and stores; This value divided by 256
   is the minimum percentage of memory references that worth prefetching.  */
#ifndef PREFETCH_DENSE_MEM
#define PREFETCH_DENSE_MEM 220
#endif

/* Do not prefetch for a loop whose iteration count is known to be low.  */
#ifndef PREFETCH_NO_LOW_LOOPCNT
#define PREFETCH_NO_LOW_LOOPCNT 1
#endif

/* Define what we mean by a "low" iteration count.  */
#ifndef PREFETCH_LOW_LOOPCNT
#define PREFETCH_LOW_LOOPCNT 32
#endif

/* Do not prefetch for a loop that contains a function call; such a loop is
   probably not an internal loop.  */
#ifndef PREFETCH_NO_CALL
#define PREFETCH_NO_CALL 1
#endif

/* Do not prefetch accesses with an extreme stride.  */
#ifndef PREFETCH_NO_EXTREME_STRIDE
#define PREFETCH_NO_EXTREME_STRIDE 1
#endif

/* Define what we mean by an "extreme" stride.  */
#ifndef PREFETCH_EXTREME_STRIDE
#define PREFETCH_EXTREME_STRIDE 4096
#endif

/* Do not handle reversed order prefetches (negative stride).  */
#ifndef PREFETCH_NO_REVERSE_ORDER
#define PREFETCH_NO_REVERSE_ORDER 1
#endif

/* Prefetch even if the GIV is not always executed.  */
#ifndef PREFETCH_NOT_ALWAYS
#define PREFETCH_NOT_ALWAYS 0
#endif

/* If the loop requires more prefetches than the target can process in
   parallel then don't prefetch anything in that loop.  */
#ifndef PREFETCH_LIMIT_TO_SIMULTANEOUS
#define PREFETCH_LIMIT_TO_SIMULTANEOUS 1
#endif

#define LOOP_REG_LIFETIME(LOOP, REGNO) \
((REGNO_LAST_LUID (REGNO) - REGNO_FIRST_LUID (REGNO)))

#define LOOP_REG_GLOBAL_P(LOOP, REGNO) \
((REGNO_LAST_LUID (REGNO) > INSN_LUID ((LOOP)->end) \
 || REGNO_FIRST_LUID (REGNO) < INSN_LUID ((LOOP)->start)))

#define LOOP_REGNO_NREGS(REGNO, SET_DEST) \
((REGNO) < FIRST_PSEUDO_REGISTER \
 ? HARD_REGNO_NREGS ((REGNO), GET_MODE (SET_DEST)) : 1)


/* Vector mapping INSN_UIDs to luids.
   The luids are like uids but increase monotonically always.
   We use them to see whether a jump comes from outside a given loop.  */

int *uid_luid;

/* Indexed by INSN_UID, contains the ordinal giving the (innermost) loop
   number the insn is contained in.  */

struct loop **uid_loop;

/* 1 + largest uid of any insn.  */

int max_uid_for_loop;

/* 1 + luid of last insn.  */

static int max_luid;

/* Number of loops detected in current function.  Used as index to the
   next few tables.  */

static int max_loop_num;

/* Bound on pseudo register number before loop optimization.
   A pseudo has valid regscan info if its number is < max_reg_before_loop.  */
unsigned int max_reg_before_loop;

/* The value to pass to the next call of reg_scan_update.  */
static int loop_max_reg;

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

/* During the analysis of a loop, a chain of `struct movable's
   is made to record all the movable insns found.
   Then the entire chain can be scanned to decide which to move.  */

struct movable
{
  rtx insn;			/* A movable insn */
  rtx set_src;			/* The expression this reg is set from.  */
  rtx set_dest;			/* The destination of this SET.  */
  rtx dependencies;		/* When INSN is libcall, this is an EXPR_LIST
				   of any registers used within the LIBCALL.  */
  int consec;			/* Number of consecutive following insns
				   that must be moved with this one.  */
  unsigned int regno;		/* The register it sets */
  short lifetime;		/* lifetime of that register;
				   may be adjusted when matching movables
				   that load the same value are found.  */
  short savings;		/* Number of insns we can move for this reg,
				   including other movables that force this
				   or match this one.  */
  unsigned int cond : 1;	/* 1 if only conditionally movable */
  unsigned int force : 1;	/* 1 means MUST move this insn */
  unsigned int global : 1;	/* 1 means reg is live outside this loop */
		/* If PARTIAL is 1, GLOBAL means something different:
		   that the reg is live outside the range from where it is set
		   to the following label.  */
  unsigned int done : 1;	/* 1 inhibits further processing of this */

  unsigned int partial : 1;	/* 1 means this reg is used for zero-extending.
				   In particular, moving it does not make it
				   invariant.  */
  unsigned int move_insn : 1;	/* 1 means that we call emit_move_insn to
				   load SRC, rather than copying INSN.  */
  unsigned int move_insn_first:1;/* Same as above, if this is necessary for the
				    first insn of a consecutive sets group.  */
  unsigned int is_equiv : 1;	/* 1 means a REG_EQUIV is present on INSN.  */
  enum machine_mode savemode;   /* Nonzero means it is a mode for a low part
				   that we should avoid changing when clearing
				   the rest of the reg.  */
  struct movable *match;	/* First entry for same value */
  struct movable *forces;	/* An insn that must be moved if this is */
  struct movable *next;
};


FILE *loop_dump_stream;

/* Forward declarations.  */

static void invalidate_loops_containing_label PARAMS ((rtx));
static void find_and_verify_loops PARAMS ((rtx, struct loops *));
static void mark_loop_jump PARAMS ((rtx, struct loop *));
static void prescan_loop PARAMS ((struct loop *));
static int reg_in_basic_block_p PARAMS ((rtx, rtx));
static int consec_sets_invariant_p PARAMS ((const struct loop *,
					    rtx, int, rtx));
static int labels_in_range_p PARAMS ((rtx, int));
static void count_one_set PARAMS ((struct loop_regs *, rtx, rtx, rtx *));
static void note_addr_stored PARAMS ((rtx, rtx, void *));
static void note_set_pseudo_multiple_uses PARAMS ((rtx, rtx, void *));
static int loop_reg_used_before_p PARAMS ((const struct loop *, rtx, rtx));
static void scan_loop PARAMS ((struct loop*, int));
#if 0
static void replace_call_address PARAMS ((rtx, rtx, rtx));
#endif
static rtx skip_consec_insns PARAMS ((rtx, int));
static int libcall_benefit PARAMS ((rtx));
static void ignore_some_movables PARAMS ((struct loop_movables *));
static void force_movables PARAMS ((struct loop_movables *));
static void combine_movables PARAMS ((struct loop_movables *,
				      struct loop_regs *));
static int num_unmoved_movables PARAMS ((const struct loop *));
static int regs_match_p PARAMS ((rtx, rtx, struct loop_movables *));
static int rtx_equal_for_loop_p PARAMS ((rtx, rtx, struct loop_movables *,
					 struct loop_regs *));
static void add_label_notes PARAMS ((rtx, rtx));
static void move_movables PARAMS ((struct loop *loop, struct loop_movables *,
				   int, int));
static void loop_movables_add PARAMS((struct loop_movables *,
				      struct movable *));
static void loop_movables_free PARAMS((struct loop_movables *));
static int count_nonfixed_reads PARAMS ((const struct loop *, rtx));
static void loop_bivs_find PARAMS((struct loop *));
static void loop_bivs_init_find PARAMS((struct loop *));
static void loop_bivs_check PARAMS((struct loop *));
static void loop_givs_find PARAMS((struct loop *));
static void loop_givs_check PARAMS((struct loop *));
static int loop_biv_eliminable_p PARAMS((struct loop *, struct iv_class *,
					 int, int));
static int loop_giv_reduce_benefit PARAMS((struct loop *, struct iv_class *,
					   struct induction *, rtx));
static void loop_givs_dead_check PARAMS((struct loop *, struct iv_class *));
static void loop_givs_reduce PARAMS((struct loop *, struct iv_class *));
static void loop_givs_rescan PARAMS((struct loop *, struct iv_class *,
				     rtx *));
static void loop_ivs_free PARAMS((struct loop *));
static void strength_reduce PARAMS ((struct loop *, int));
static void find_single_use_in_loop PARAMS ((struct loop_regs *, rtx, rtx));
static int valid_initial_value_p PARAMS ((rtx, rtx, int, rtx));
static void find_mem_givs PARAMS ((const struct loop *, rtx, rtx, int, int));
static void record_biv PARAMS ((struct loop *, struct induction *,
				rtx, rtx, rtx, rtx, rtx *,
				int, int));
static void check_final_value PARAMS ((const struct loop *,
				       struct induction *));
static void loop_ivs_dump PARAMS((const struct loop *, FILE *, int));
static void loop_iv_class_dump PARAMS((const struct iv_class *, FILE *, int));
static void loop_biv_dump PARAMS((const struct induction *, FILE *, int));
static void loop_giv_dump PARAMS((const struct induction *, FILE *, int));
static void record_giv PARAMS ((const struct loop *, struct induction *,
				rtx, rtx, rtx, rtx, rtx, rtx, int,
				enum g_types, int, int, rtx *));
static void update_giv_derive PARAMS ((const struct loop *, rtx));
static void check_ext_dependent_givs PARAMS ((struct iv_class *,
					      struct loop_info *));
static int basic_induction_var PARAMS ((const struct loop *, rtx,
					enum machine_mode, rtx, rtx,
					rtx *, rtx *, rtx **));
static rtx simplify_giv_expr PARAMS ((const struct loop *, rtx, rtx *, int *));
static int general_induction_var PARAMS ((const struct loop *loop, rtx, rtx *,
					  rtx *, rtx *, rtx *, int, int *,
					  enum machine_mode));
static int consec_sets_giv PARAMS ((const struct loop *, int, rtx,
				    rtx, rtx, rtx *, rtx *, rtx *, rtx *));
static int check_dbra_loop PARAMS ((struct loop *, int));
static rtx express_from_1 PARAMS ((rtx, rtx, rtx));
static rtx combine_givs_p PARAMS ((struct induction *, struct induction *));
static int cmp_combine_givs_stats PARAMS ((const PTR, const PTR));
static void combine_givs PARAMS ((struct loop_regs *, struct iv_class *));
static int product_cheap_p PARAMS ((rtx, rtx));
static int maybe_eliminate_biv PARAMS ((const struct loop *, struct iv_class *,
					int, int, int));
static int maybe_eliminate_biv_1 PARAMS ((const struct loop *, rtx, rtx,
					  struct iv_class *, int,
					  basic_block, rtx));
static int last_use_this_basic_block PARAMS ((rtx, rtx));
static void record_initial PARAMS ((rtx, rtx, void *));
static void update_reg_last_use PARAMS ((rtx, rtx));
static rtx next_insn_in_loop PARAMS ((const struct loop *, rtx));
static void loop_regs_scan PARAMS ((const struct loop *, int));
static int count_insns_in_loop PARAMS ((const struct loop *));
static void load_mems PARAMS ((const struct loop *));
static int insert_loop_mem PARAMS ((rtx *, void *));
static int replace_loop_mem PARAMS ((rtx *, void *));
static void replace_loop_mems PARAMS ((rtx, rtx, rtx));
static int replace_loop_reg PARAMS ((rtx *, void *));
static void replace_loop_regs PARAMS ((rtx insn, rtx, rtx));
static void note_reg_stored PARAMS ((rtx, rtx, void *));
static void try_copy_prop PARAMS ((const struct loop *, rtx, unsigned int));
static void try_swap_copy_prop PARAMS ((const struct loop *, rtx,
					 unsigned int));
static int replace_label PARAMS ((rtx *, void *));
static rtx check_insn_for_givs PARAMS((struct loop *, rtx, int, int));
static rtx check_insn_for_bivs PARAMS((struct loop *, rtx, int, int));
static rtx gen_add_mult PARAMS ((rtx, rtx, rtx, rtx));
static void loop_regs_update PARAMS ((const struct loop *, rtx));
static int iv_add_mult_cost PARAMS ((rtx, rtx, rtx, rtx));

static rtx loop_insn_emit_after PARAMS((const struct loop *, basic_block,
					rtx, rtx));
static rtx loop_call_insn_emit_before PARAMS((const struct loop *,
					      basic_block, rtx, rtx));
static rtx loop_call_insn_hoist PARAMS((const struct loop *, rtx));
static rtx loop_insn_sink_or_swim PARAMS((const struct loop *, rtx));

static void loop_dump_aux PARAMS ((const struct loop *, FILE *, int));
static void loop_delete_insns PARAMS ((rtx, rtx));
static HOST_WIDE_INT remove_constant_addition PARAMS ((rtx *));
void debug_ivs PARAMS ((const struct loop *));
void debug_iv_class PARAMS ((const struct iv_class *));
void debug_biv PARAMS ((const struct induction *));
void debug_giv PARAMS ((const struct induction *));
void debug_loop PARAMS ((const struct loop *));
void debug_loops PARAMS ((const struct loops *));

typedef struct rtx_pair
{
  rtx r1;
  rtx r2;
} rtx_pair;

typedef struct loop_replace_args
{
  rtx match;
  rtx replacement;
  rtx insn;
} loop_replace_args;

/* Nonzero iff INSN is between START and END, inclusive.  */
#define INSN_IN_RANGE_P(INSN, START, END)	\
  (INSN_UID (INSN) < max_uid_for_loop		\
   && INSN_LUID (INSN) >= INSN_LUID (START)	\
   && INSN_LUID (INSN) <= INSN_LUID (END))

/* Indirect_jump_in_function is computed once per function.  */
static int indirect_jump_in_function;
static int indirect_jump_in_function_p PARAMS ((rtx));

static int compute_luids PARAMS ((rtx, rtx, int));

static int biv_elimination_giv_has_0_offset PARAMS ((struct induction *,
						     struct induction *,
						     rtx));

/* Benefit penalty, if a giv is not replaceable, i.e. must emit an insn to
   copy the value of the strength reduced giv to its original register.  */
static int copy_cost;

/* Cost of using a register, to normalize the benefits of a giv.  */
static int reg_address_cost;

void
init_loop ()
{
  rtx reg = gen_rtx_REG (word_mode, LAST_VIRTUAL_REGISTER + 1);

  reg_address_cost = address_cost (reg, SImode);

  copy_cost = COSTS_N_INSNS (1);
}

/* Compute the mapping from uids to luids.
   LUIDs are numbers assigned to insns, like uids,
   except that luids increase monotonically through the code.
   Start at insn START and stop just before END.  Assign LUIDs
   starting with PREV_LUID + 1.  Return the last assigned LUID + 1.  */
static int
compute_luids (start, end, prev_luid)
     rtx start, end;
     int prev_luid;
{
  int i;
  rtx insn;

  for (insn = start, i = prev_luid; insn != end; insn = NEXT_INSN (insn))
    {
      if (INSN_UID (insn) >= max_uid_for_loop)
	continue;
      /* Don't assign luids to line-number NOTEs, so that the distance in
	 luids between two insns is not affected by -g.  */
      if (GET_CODE (insn) != NOTE
	  || NOTE_LINE_NUMBER (insn) <= 0)
	uid_luid[INSN_UID (insn)] = ++i;
      else
	/* Give a line number note the same luid as preceding insn.  */
	uid_luid[INSN_UID (insn)] = i;
    }
  return i + 1;
}

/* Entry point of this file.  Perform loop optimization
   on the current function.  F is the first insn of the function
   and DUMPFILE is a stream for output of a trace of actions taken
   (or 0 if none should be output).  */

void
loop_optimize (f, dumpfile, flags)
     /* f is the first instruction of a chain of insns for one function */
     rtx f;
     FILE *dumpfile;
     int flags;
{
  rtx insn;
  int i;
  struct loops loops_data;
  struct loops *loops = &loops_data;
  struct loop_info *loops_info;

  loop_dump_stream = dumpfile;

  init_recog_no_volatile ();

  max_reg_before_loop = max_reg_num ();
  loop_max_reg = max_reg_before_loop;

  regs_may_share = 0;

  /* Count the number of loops.  */

  max_loop_num = 0;
  for (insn = f; insn; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == NOTE
	  && NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_BEG)
	max_loop_num++;
    }

  /* Don't waste time if no loops.  */
  if (max_loop_num == 0)
    return;

  loops->num = max_loop_num;

  /* Get size to use for tables indexed by uids.
     Leave some space for labels allocated by find_and_verify_loops.  */
  max_uid_for_loop = get_max_uid () + 1 + max_loop_num * 32;

  uid_luid = (int *) xcalloc (max_uid_for_loop, sizeof (int));
  uid_loop = (struct loop **) xcalloc (max_uid_for_loop,
				       sizeof (struct loop *));

  /* Allocate storage for array of loops.  */
  loops->array = (struct loop *)
    xcalloc (loops->num, sizeof (struct loop));

  /* Find and process each loop.
     First, find them, and record them in order of their beginnings.  */
  find_and_verify_loops (f, loops);

  /* Allocate and initialize auxiliary loop information.  */
  loops_info = xcalloc (loops->num, sizeof (struct loop_info));
  for (i = 0; i < loops->num; i++)
    loops->array[i].aux = loops_info + i;

  /* Now find all register lifetimes.  This must be done after
     find_and_verify_loops, because it might reorder the insns in the
     function.  */
  reg_scan (f, max_reg_before_loop, 1);

  /* This must occur after reg_scan so that registers created by gcse
     will have entries in the register tables.

     We could have added a call to reg_scan after gcse_main in toplev.c,
     but moving this call to init_alias_analysis is more efficient.  */
  init_alias_analysis ();

  /* See if we went too far.  Note that get_max_uid already returns
     one more that the maximum uid of all insn.  */
  if (get_max_uid () > max_uid_for_loop)
    abort ();
  /* Now reset it to the actual size we need.  See above.  */
  max_uid_for_loop = get_max_uid ();

  /* find_and_verify_loops has already called compute_luids, but it
     might have rearranged code afterwards, so we need to recompute
     the luids now.  */
  max_luid = compute_luids (f, NULL_RTX, 0);

  /* Don't leave gaps in uid_luid for insns that have been
     deleted.  It is possible that the first or last insn
     using some register has been deleted by cross-jumping.
     Make sure that uid_luid for that former insn's uid
     points to the general area where that insn used to be.  */
  for (i = 0; i < max_uid_for_loop; i++)
    {
      uid_luid[0] = uid_luid[i];
      if (uid_luid[0] != 0)
	break;
    }
  for (i = 0; i < max_uid_for_loop; i++)
    if (uid_luid[i] == 0)
      uid_luid[i] = uid_luid[i - 1];

  /* Determine if the function has indirect jump.  On some systems
     this prevents low overhead loop instructions from being used.  */
  indirect_jump_in_function = indirect_jump_in_function_p (f);

  /* Now scan the loops, last ones first, since this means inner ones are done
     before outer ones.  */
  for (i = max_loop_num - 1; i >= 0; i--)
    {
      struct loop *loop = &loops->array[i];

      if (! loop->invalid && loop->end)
	scan_loop (loop, flags);
    }

  /* If there were lexical blocks inside the loop, they have been
     replicated.  We will now have more than one NOTE_INSN_BLOCK_BEG
     and NOTE_INSN_BLOCK_END for each such block.  We must duplicate
     the BLOCKs as well.  */
  if (write_symbols != NO_DEBUG)
    reorder_blocks ();

  end_alias_analysis ();

  /* Clean up.  */
  free (uid_luid);
  free (uid_loop);
  free (loops_info);
  free (loops->array);
}

/* Returns the next insn, in execution order, after INSN.  START and
   END are the NOTE_INSN_LOOP_BEG and NOTE_INSN_LOOP_END for the loop,
   respectively.  LOOP->TOP, if non-NULL, is the top of the loop in the
   insn-stream; it is used with loops that are entered near the
   bottom.  */

static rtx
next_insn_in_loop (loop, insn)
     const struct loop *loop;
     rtx insn;
{
  insn = NEXT_INSN (insn);

  if (insn == loop->end)
    {
      if (loop->top)
	/* Go to the top of the loop, and continue there.  */
	insn = loop->top;
      else
	/* We're done.  */
	insn = NULL_RTX;
    }

  if (insn == loop->scan_start)
    /* We're done.  */
    insn = NULL_RTX;

  return insn;
}

/* Optimize one loop described by LOOP.  */

/* ??? Could also move memory writes out of loops if the destination address
   is invariant, the source is invariant, the memory write is not volatile,
   and if we can prove that no read inside the loop can read this address
   before the write occurs.  If there is a read of this address after the
   write, then we can also mark the memory read as invariant.  */

static void
scan_loop (loop, flags)
     struct loop *loop;
     int flags;
{
  struct loop_info *loop_info = LOOP_INFO (loop);
  struct loop_regs *regs = LOOP_REGS (loop);
  int i;
  rtx loop_start = loop->start;
  rtx loop_end = loop->end;
  rtx p;
  /* 1 if we are scanning insns that could be executed zero times.  */
  int maybe_never = 0;
  /* 1 if we are scanning insns that might never be executed
     due to a subroutine call which might exit before they are reached.  */
  int call_passed = 0;
  /* Jump insn that enters the loop, or 0 if control drops in.  */
  rtx loop_entry_jump = 0;
  /* Number of insns in the loop.  */
  int insn_count;
  int tem;
  rtx temp, update_start, update_end;
  /* The SET from an insn, if it is the only SET in the insn.  */
  rtx set, set1;
  /* Chain describing insns movable in current loop.  */
  struct loop_movables *movables = LOOP_MOVABLES (loop);
  /* Ratio of extra register life span we can justify
     for saving an instruction.  More if loop doesn't call subroutines
     since in that case saving an insn makes more difference
     and more registers are available.  */
  int threshold;
  /* Nonzero if we are scanning instructions in a sub-loop.  */
  int loop_depth = 0;
  int in_libcall;

  loop->top = 0;

  movables->head = 0;
  movables->last = 0;

  /* Determine whether this loop starts with a jump down to a test at
     the end.  This will occur for a small number of loops with a test
     that is too complex to duplicate in front of the loop.

     We search for the first insn or label in the loop, skipping NOTEs.
     However, we must be careful not to skip past a NOTE_INSN_LOOP_BEG
     (because we might have a loop executed only once that contains a
     loop which starts with a jump to its exit test) or a NOTE_INSN_LOOP_END
     (in case we have a degenerate loop).

     Note that if we mistakenly think that a loop is entered at the top
     when, in fact, it is entered at the exit test, the only effect will be
     slightly poorer optimization.  Making the opposite error can generate
     incorrect code.  Since very few loops now start with a jump to the
     exit test, the code here to detect that case is very conservative.  */

  for (p = NEXT_INSN (loop_start);
       p != loop_end
	 && GET_CODE (p) != CODE_LABEL && ! INSN_P (p)
	 && (GET_CODE (p) != NOTE
	     || (NOTE_LINE_NUMBER (p) != NOTE_INSN_LOOP_BEG
		 && NOTE_LINE_NUMBER (p) != NOTE_INSN_LOOP_END));
       p = NEXT_INSN (p))
    ;

  loop->scan_start = p;

  /* If loop end is the end of the current function, then emit a
     NOTE_INSN_DELETED after loop_end and set loop->sink to the dummy
     note insn.  This is the position we use when sinking insns out of
     the loop.  */
  if (NEXT_INSN (loop->end) != 0)
    loop->sink = NEXT_INSN (loop->end);
  else
    loop->sink = emit_note_after (NOTE_INSN_DELETED, loop->end);

  /* Set up variables describing this loop.  */
  prescan_loop (loop);
  threshold = (loop_info->has_call ? 1 : 2) * (1 + n_non_fixed_regs);

  /* If loop has a jump before the first label,
     the true entry is the target of that jump.
     Start scan from there.
     But record in LOOP->TOP the place where the end-test jumps
     back to so we can scan that after the end of the loop.  */
  if (GET_CODE (p) == JUMP_INSN)
    {
      loop_entry_jump = p;

      /* Loop entry must be unconditional jump (and not a RETURN)  */
      if (any_uncondjump_p (p)
	  && JUMP_LABEL (p) != 0
	  /* Check to see whether the jump actually
	     jumps out of the loop (meaning it's no loop).
	     This case can happen for things like
	     do {..} while (0).  If this label was generated previously
	     by loop, we can't tell anything about it and have to reject
	     the loop.  */
	  && INSN_IN_RANGE_P (JUMP_LABEL (p), loop_start, loop_end))
	{
	  loop->top = next_label (loop->scan_start);
	  loop->scan_start = JUMP_LABEL (p);
	}
    }

  /* If LOOP->SCAN_START was an insn created by loop, we don't know its luid
     as required by loop_reg_used_before_p.  So skip such loops.  (This
     test may never be true, but it's best to play it safe.)

     Also, skip loops where we do not start scanning at a label.  This
     test also rejects loops starting with a JUMP_INSN that failed the
     test above.  */

  if (INSN_UID (loop->scan_start) >= max_uid_for_loop
      || GET_CODE (loop->scan_start) != CODE_LABEL)
    {
      if (loop_dump_stream)
	fprintf (loop_dump_stream, "\nLoop from %d to %d is phony.\n\n",
		 INSN_UID (loop_start), INSN_UID (loop_end));
      return;
    }

  /* Allocate extra space for REGs that might be created by load_mems.
     We allocate a little extra slop as well, in the hopes that we
     won't have to reallocate the regs array.  */
  loop_regs_scan (loop, loop_info->mems_idx + 16);
  insn_count = count_insns_in_loop (loop);

  if (loop_dump_stream)
    {
      fprintf (loop_dump_stream, "\nLoop from %d to %d: %d real insns.\n",
	       INSN_UID (loop_start), INSN_UID (loop_end), insn_count);
      if (loop->cont)
	fprintf (loop_dump_stream, "Continue at insn %d.\n",
		 INSN_UID (loop->cont));
    }

  /* Scan through the loop finding insns that are safe to move.
     Set REGS->ARRAY[I].SET_IN_LOOP negative for the reg I being set, so that
     this reg will be considered invariant for subsequent insns.
     We consider whether subsequent insns use the reg
     in deciding whether it is worth actually moving.

     MAYBE_NEVER is nonzero if we have passed a conditional jump insn
     and therefore it is possible that the insns we are scanning
     would never be executed.  At such times, we must make sure
     that it is safe to execute the insn once instead of zero times.
     When MAYBE_NEVER is 0, all insns will be executed at least once
     so that is not a problem.  */

  for (in_libcall = 0, p = next_insn_in_loop (loop, loop->scan_start);
       p != NULL_RTX;
       p = next_insn_in_loop (loop, p))
    {
      if (in_libcall && INSN_P (p) && find_reg_note (p, REG_RETVAL, NULL_RTX))
	in_libcall--;
      if (GET_CODE (p) == INSN)
	{
	  temp = find_reg_note (p, REG_LIBCALL, NULL_RTX);
	  if (temp)
	    in_libcall++;
	  if (! in_libcall
	      && (set = single_set (p))
	      && GET_CODE (SET_DEST (set)) == REG
#ifdef PIC_OFFSET_TABLE_REG_CALL_CLOBBERED
	      && SET_DEST (set) != pic_offset_table_rtx
#endif
	      && ! regs->array[REGNO (SET_DEST (set))].may_not_optimize)
	    {
	      int tem1 = 0;
	      int tem2 = 0;
	      int move_insn = 0;
	      rtx src = SET_SRC (set);
	      rtx dependencies = 0;

	      /* Figure out what to use as a source of this insn.  If a
		 REG_EQUIV note is given or if a REG_EQUAL note with a
		 constant operand is specified, use it as the source and
		 mark that we should move this insn by calling
		 emit_move_insn rather that duplicating the insn.

		 Otherwise, only use the REG_EQUAL contents if a REG_RETVAL
		 note is present.  */
	      temp = find_reg_note (p, REG_EQUIV, NULL_RTX);
	      if (temp)
		src = XEXP (temp, 0), move_insn = 1;
	      else
		{
		  temp = find_reg_note (p, REG_EQUAL, NULL_RTX);
		  if (temp && CONSTANT_P (XEXP (temp, 0)))
		    src = XEXP (temp, 0), move_insn = 1;
		  if (temp && find_reg_note (p, REG_RETVAL, NULL_RTX))
		    {
		      src = XEXP (temp, 0);
		      /* A libcall block can use regs that don't appear in
			 the equivalent expression.  To move the libcall,
			 we must move those regs too.  */
		      dependencies = libcall_other_reg (p, src);
		    }
		}

	      /* For parallels, add any possible uses to the depencies, as
		 we can't move the insn without resolving them first.  */
	      if (GET_CODE (PATTERN (p)) == PARALLEL)
		{
		  for (i = 0; i < XVECLEN (PATTERN (p), 0); i++)
		    {
		      rtx x = XVECEXP (PATTERN (p), 0, i);
		      if (GET_CODE (x) == USE)
			dependencies
			  = gen_rtx_EXPR_LIST (VOIDmode, XEXP (x, 0),
					       dependencies);
		    }
		}

	      /* Don't try to optimize a register that was made
		 by loop-optimization for an inner loop.
		 We don't know its life-span, so we can't compute
		 the benefit.  */
	      if (REGNO (SET_DEST (set)) >= max_reg_before_loop)
		;
	      else if (/* The register is used in basic blocks other
			  than the one where it is set (meaning that
			  something after this point in the loop might
			  depend on its value before the set).  */
		       ! reg_in_basic_block_p (p, SET_DEST (set))
		       /* And the set is not guaranteed to be executed once
			  the loop starts, or the value before the set is
			  needed before the set occurs...

			  ??? Note we have quadratic behaviour here, mitigated
			  by the fact that the previous test will often fail for
			  large loops.  Rather than re-scanning the entire loop
			  each time for register usage, we should build tables
			  of the register usage and use them here instead.  */
		       && (maybe_never
			   || loop_reg_used_before_p (loop, set, p)))
		/* It is unsafe to move the set.

		   This code used to consider it OK to move a set of a variable
		   which was not created by the user and not used in an exit
		   test.
		   That behavior is incorrect and was removed.  */
		;
	      else if ((tem = loop_invariant_p (loop, src))
		       && (dependencies == 0
			   || (tem2
			       = loop_invariant_p (loop, dependencies)) != 0)
		       && (regs->array[REGNO (SET_DEST (set))].set_in_loop == 1
			   || (tem1
			       = consec_sets_invariant_p
			       (loop, SET_DEST (set),
				regs->array[REGNO (SET_DEST (set))].set_in_loop,
				p)))
		       /* If the insn can cause a trap (such as divide by zero),
			  can't move it unless it's guaranteed to be executed
			  once loop is entered.  Even a function call might
			  prevent the trap insn from being reached
			  (since it might exit!)  */
		       && ! ((maybe_never || call_passed)
			     && may_trap_p (src)))
		{
		  struct movable *m;
		  int regno = REGNO (SET_DEST (set));

		  /* A potential lossage is where we have a case where two insns
		     can be combined as long as they are both in the loop, but
		     we move one of them outside the loop.  For large loops,
		     this can lose.  The most common case of this is the address
		     of a function being called.

		     Therefore, if this register is marked as being used
		     exactly once if we are in a loop with calls
		     (a "large loop"), see if we can replace the usage of
		     this register with the source of this SET.  If we can,
		     delete this insn.

		     Don't do this if P has a REG_RETVAL note or if we have
		     SMALL_REGISTER_CLASSES and SET_SRC is a hard register.  */

		  if (loop_info->has_call
		      && regs->array[regno].single_usage != 0
		      && regs->array[regno].single_usage != const0_rtx
		      && REGNO_FIRST_UID (regno) == INSN_UID (p)
		      && (REGNO_LAST_UID (regno)
			  == INSN_UID (regs->array[regno].single_usage))
		      && regs->array[regno].set_in_loop == 1
		      && GET_CODE (SET_SRC (set)) != ASM_OPERANDS
		      && ! side_effects_p (SET_SRC (set))
		      && ! find_reg_note (p, REG_RETVAL, NULL_RTX)
		      && (! SMALL_REGISTER_CLASSES
			  || (! (GET_CODE (SET_SRC (set)) == REG
				 && (REGNO (SET_SRC (set))
				     < FIRST_PSEUDO_REGISTER))))
		      /* This test is not redundant; SET_SRC (set) might be
			 a call-clobbered register and the life of REGNO
			 might span a call.  */
		      && ! modified_between_p (SET_SRC (set), p,
					       regs->array[regno].single_usage)
		      && no_labels_between_p (p,
					      regs->array[regno].single_usage)
		      && validate_replace_rtx (SET_DEST (set), SET_SRC (set),
					       regs->array[regno].single_usage))
		    {
		      /* Replace any usage in a REG_EQUAL note.  Must copy
			 the new source, so that we don't get rtx sharing
			 between the SET_SOURCE and REG_NOTES of insn p.  */
		      REG_NOTES (regs->array[regno].single_usage)
			= (replace_rtx
			   (REG_NOTES (regs->array[regno].single_usage),
			    SET_DEST (set), copy_rtx (SET_SRC (set))));

		      delete_insn (p);
		      for (i = 0; i < LOOP_REGNO_NREGS (regno, SET_DEST (set));
			   i++)
			regs->array[regno+i].set_in_loop = 0;
		      continue;
		    }

		  m = (struct movable *) xmalloc (sizeof (struct movable));
		  m->next = 0;
		  m->insn = p;
		  m->set_src = src;
		  m->dependencies = dependencies;
		  m->set_dest = SET_DEST (set);
		  m->force = 0;
		  m->consec
		    = regs->array[REGNO (SET_DEST (set))].set_in_loop - 1;
		  m->done = 0;
		  m->forces = 0;
		  m->partial = 0;
		  m->move_insn = move_insn;
		  m->move_insn_first = 0;
		  m->is_equiv = (find_reg_note (p, REG_EQUIV, NULL_RTX) != 0);
		  m->savemode = VOIDmode;
		  m->regno = regno;
		  /* Set M->cond if either loop_invariant_p
		     or consec_sets_invariant_p returned 2
		     (only conditionally invariant).  */
		  m->cond = ((tem | tem1 | tem2) > 1);
		  m->global =  LOOP_REG_GLOBAL_P (loop, regno);
		  m->match = 0;
		  m->lifetime = LOOP_REG_LIFETIME (loop, regno);
		  m->savings = regs->array[regno].n_times_set;
		  if (find_reg_note (p, REG_RETVAL, NULL_RTX))
		    m->savings += libcall_benefit (p);
		  for (i = 0; i < LOOP_REGNO_NREGS (regno, SET_DEST (set)); i++)
		    regs->array[regno+i].set_in_loop = move_insn ? -2 : -1;
		  /* Add M to the end of the chain MOVABLES.  */
		  loop_movables_add (movables, m);

		  if (m->consec > 0)
		    {
		      /* It is possible for the first instruction to have a
			 REG_EQUAL note but a non-invariant SET_SRC, so we must
			 remember the status of the first instruction in case
			 the last instruction doesn't have a REG_EQUAL note.  */
		      m->move_insn_first = m->move_insn;

		      /* Skip this insn, not checking REG_LIBCALL notes.  */
		      p = next_nonnote_insn (p);
		      /* Skip the consecutive insns, if there are any.  */
		      p = skip_consec_insns (p, m->consec);
		      /* Back up to the last insn of the consecutive group.  */
		      p = prev_nonnote_insn (p);

		      /* We must now reset m->move_insn, m->is_equiv, and
			 possibly m->set_src to correspond to the effects of
			 all the insns.  */
		      temp = find_reg_note (p, REG_EQUIV, NULL_RTX);
		      if (temp)
			m->set_src = XEXP (temp, 0), m->move_insn = 1;
		      else
			{
			  temp = find_reg_note (p, REG_EQUAL, NULL_RTX);
			  if (temp && CONSTANT_P (XEXP (temp, 0)))
			    m->set_src = XEXP (temp, 0), m->move_insn = 1;
			  else
			    m->move_insn = 0;

			}
		      m->is_equiv
			= (find_reg_note (p, REG_EQUIV, NULL_RTX) != 0);
		    }
		}
	      /* If this register is always set within a STRICT_LOW_PART
		 or set to zero, then its high bytes are constant.
		 So clear them outside the loop and within the loop
		 just load the low bytes.
		 We must check that the machine has an instruction to do so.
		 Also, if the value loaded into the register
		 depends on the same register, this cannot be done.  */
	      else if (SET_SRC (set) == const0_rtx
		       && GET_CODE (NEXT_INSN (p)) == INSN
		       && (set1 = single_set (NEXT_INSN (p)))
		       && GET_CODE (set1) == SET
		       && (GET_CODE (SET_DEST (set1)) == STRICT_LOW_PART)
		       && (GET_CODE (XEXP (SET_DEST (set1), 0)) == SUBREG)
		       && (SUBREG_REG (XEXP (SET_DEST (set1), 0))
			   == SET_DEST (set))
		       && !reg_mentioned_p (SET_DEST (set), SET_SRC (set1)))
		{
		  int regno = REGNO (SET_DEST (set));
		  if (regs->array[regno].set_in_loop == 2)
		    {
		      struct movable *m;
		      m = (struct movable *) xmalloc (sizeof (struct movable));
		      m->next = 0;
		      m->insn = p;
		      m->set_dest = SET_DEST (set);
		      m->dependencies = 0;
		      m->force = 0;
		      m->consec = 0;
		      m->done = 0;
		      m->forces = 0;
		      m->move_insn = 0;
		      m->move_insn_first = 0;
		      m->partial = 1;
		      /* If the insn may not be executed on some cycles,
			 we can't clear the whole reg; clear just high part.
			 Not even if the reg is used only within this loop.
			 Consider this:
			 while (1)
			   while (s != t) {
			     if (foo ()) x = *s;
			     use (x);
			   }
			 Clearing x before the inner loop could clobber a value
			 being saved from the last time around the outer loop.
			 However, if the reg is not used outside this loop
			 and all uses of the register are in the same
			 basic block as the store, there is no problem.

			 If this insn was made by loop, we don't know its
			 INSN_LUID and hence must make a conservative
			 assumption.  */
		      m->global = (INSN_UID (p) >= max_uid_for_loop
				   || LOOP_REG_GLOBAL_P (loop, regno)
				   || (labels_in_range_p
				       (p, REGNO_FIRST_LUID (regno))));
		      if (maybe_never && m->global)
			m->savemode = GET_MODE (SET_SRC (set1));
		      else
			m->savemode = VOIDmode;
		      m->regno = regno;
		      m->cond = 0;
		      m->match = 0;
		      m->lifetime = LOOP_REG_LIFETIME (loop, regno);
		      m->savings = 1;
		      for (i = 0; i < LOOP_REGNO_NREGS (regno, SET_DEST (set));
			   i++)
			regs->array[regno+i].set_in_loop = -1;
		      /* Add M to the end of the chain MOVABLES.  */
		      loop_movables_add (movables, m);
		    }
		}
	    }
	}
      /* Past a call insn, we get to insns which might not be executed
	 because the call might exit.  This matters for insns that trap.
	 Constant and pure call insns always return, so they don't count.  */
      else if (GET_CODE (p) == CALL_INSN && ! CONST_OR_PURE_CALL_P (p))
	call_passed = 1;
      /* Past a label or a jump, we get to insns for which we
	 can't count on whether or how many times they will be
	 executed during each iteration.  Therefore, we can
	 only move out sets of trivial variables
	 (those not used after the loop).  */
      /* Similar code appears twice in strength_reduce.  */
      else if ((GET_CODE (p) == CODE_LABEL || GET_CODE (p) == JUMP_INSN)
	       /* If we enter the loop in the middle, and scan around to the
		  beginning, don't set maybe_never for that.  This must be an
		  unconditional jump, otherwise the code at the top of the
		  loop might never be executed.  Unconditional jumps are
		  followed by a barrier then the loop_end.  */
               && ! (GET_CODE (p) == JUMP_INSN && JUMP_LABEL (p) == loop->top
		     && NEXT_INSN (NEXT_INSN (p)) == loop_end
		     && any_uncondjump_p (p)))
	maybe_never = 1;
      else if (GET_CODE (p) == NOTE)
	{
	  /* At the virtual top of a converted loop, insns are again known to
	     be executed: logically, the loop begins here even though the exit
	     code has been duplicated.  */
	  if (NOTE_LINE_NUMBER (p) == NOTE_INSN_LOOP_VTOP && loop_depth == 0)
	    maybe_never = call_passed = 0;
	  else if (NOTE_LINE_NUMBER (p) == NOTE_INSN_LOOP_BEG)
	    loop_depth++;
	  else if (NOTE_LINE_NUMBER (p) == NOTE_INSN_LOOP_END)
	    loop_depth--;
	}
    }

  /* If one movable subsumes another, ignore that other.  */

  ignore_some_movables (movables);

  /* For each movable insn, see if the reg that it loads
     leads when it dies right into another conditionally movable insn.
     If so, record that the second insn "forces" the first one,
     since the second can be moved only if the first is.  */

  force_movables (movables);

  /* See if there are multiple movable insns that load the same value.
     If there are, make all but the first point at the first one
     through the `match' field, and add the priorities of them
     all together as the priority of the first.  */

  combine_movables (movables, regs);

  /* Now consider each movable insn to decide whether it is worth moving.
     Store 0 in regs->array[I].set_in_loop for each reg I that is moved.

     Generally this increases code size, so do not move moveables when
     optimizing for code size.  */

  if (! optimize_size)
    {
      move_movables (loop, movables, threshold, insn_count);

      /* Recalculate regs->array if move_movables has created new
	 registers.  */
      if (max_reg_num () > regs->num)
	{
	  loop_regs_scan (loop, 0);
	  for (update_start = loop_start;
	       PREV_INSN (update_start)
	       && GET_CODE (PREV_INSN (update_start)) != CODE_LABEL;
	       update_start = PREV_INSN (update_start))
	    ;
	  update_end = NEXT_INSN (loop_end);

	  reg_scan_update (update_start, update_end, loop_max_reg);
	  loop_max_reg = max_reg_num ();
	}
    }

  /* Now candidates that still are negative are those not moved.
     Change regs->array[I].set_in_loop to indicate that those are not actually
     invariant.  */
  for (i = 0; i < regs->num; i++)
    if (regs->array[i].set_in_loop < 0)
      regs->array[i].set_in_loop = regs->array[i].n_times_set;

  /* Now that we've moved some things out of the loop, we might be able to
     hoist even more memory references.  */
  load_mems (loop);

  /* Recalculate regs->array if load_mems has created new registers.  */
  if (max_reg_num () > regs->num)
    loop_regs_scan (loop, 0);

  for (update_start = loop_start;
       PREV_INSN (update_start)
	 && GET_CODE (PREV_INSN (update_start)) != CODE_LABEL;
       update_start = PREV_INSN (update_start))
    ;
  update_end = NEXT_INSN (loop_end);

  reg_scan_update (update_start, update_end, loop_max_reg);
  loop_max_reg = max_reg_num ();

  if (flag_strength_reduce)
    {
      if (update_end && GET_CODE (update_end) == CODE_LABEL)
	/* Ensure our label doesn't go away.  */
	LABEL_NUSES (update_end)++;

      strength_reduce (loop, flags);

      reg_scan_update (update_start, update_end, loop_max_reg);
      loop_max_reg = max_reg_num ();

      if (update_end && GET_CODE (update_end) == CODE_LABEL
	  && --LABEL_NUSES (update_end) == 0)
	delete_related_insns (update_end);
    }


  /* The movable information is required for strength reduction.  */
  loop_movables_free (movables);

  free (regs->array);
  regs->array = 0;
  regs->num = 0;
}

/* Add elements to *OUTPUT to record all the pseudo-regs
   mentioned in IN_THIS but not mentioned in NOT_IN_THIS.  */

void
record_excess_regs (in_this, not_in_this, output)
     rtx in_this, not_in_this;
     rtx *output;
{
  enum rtx_code code;
  const char *fmt;
  int i;

  code = GET_CODE (in_this);

  switch (code)
    {
    case PC:
    case CC0:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      return;

    case REG:
      if (REGNO (in_this) >= FIRST_PSEUDO_REGISTER
	  && ! reg_mentioned_p (in_this, not_in_this))
	*output = gen_rtx_EXPR_LIST (VOIDmode, in_this, *output);
      return;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      int j;

      switch (fmt[i])
	{
	case 'E':
	  for (j = 0; j < XVECLEN (in_this, i); j++)
	    record_excess_regs (XVECEXP (in_this, i, j), not_in_this, output);
	  break;

	case 'e':
	  record_excess_regs (XEXP (in_this, i), not_in_this, output);
	  break;
	}
    }
}

/* Check what regs are referred to in the libcall block ending with INSN,
   aside from those mentioned in the equivalent value.
   If there are none, return 0.
   If there are one or more, return an EXPR_LIST containing all of them.  */

rtx
libcall_other_reg (insn, equiv)
     rtx insn, equiv;
{
  rtx note = find_reg_note (insn, REG_RETVAL, NULL_RTX);
  rtx p = XEXP (note, 0);
  rtx output = 0;

  /* First, find all the regs used in the libcall block
     that are not mentioned as inputs to the result.  */

  while (p != insn)
    {
      if (GET_CODE (p) == INSN || GET_CODE (p) == JUMP_INSN
	  || GET_CODE (p) == CALL_INSN)
	record_excess_regs (PATTERN (p), equiv, &output);
      p = NEXT_INSN (p);
    }

  return output;
}

/* Return 1 if all uses of REG
   are between INSN and the end of the basic block.  */

static int
reg_in_basic_block_p (insn, reg)
     rtx insn, reg;
{
  int regno = REGNO (reg);
  rtx p;

  if (REGNO_FIRST_UID (regno) != INSN_UID (insn))
    return 0;

  /* Search this basic block for the already recorded last use of the reg.  */
  for (p = insn; p; p = NEXT_INSN (p))
    {
      switch (GET_CODE (p))
	{
	case NOTE:
	  break;

	case INSN:
	case CALL_INSN:
	  /* Ordinary insn: if this is the last use, we win.  */
	  if (REGNO_LAST_UID (regno) == INSN_UID (p))
	    return 1;
	  break;

	case JUMP_INSN:
	  /* Jump insn: if this is the last use, we win.  */
	  if (REGNO_LAST_UID (regno) == INSN_UID (p))
	    return 1;
	  /* Otherwise, it's the end of the basic block, so we lose.  */
	  return 0;

	case CODE_LABEL:
	case BARRIER:
	  /* It's the end of the basic block, so we lose.  */
	  return 0;

	default:
	  break;
	}
    }

  /* The "last use" that was recorded can't be found after the first
     use.  This can happen when the last use was deleted while
     processing an inner loop, this inner loop was then completely
     unrolled, and the outer loop is always exited after the inner loop,
     so that everything after the first use becomes a single basic block.  */
  return 1;
}

/* Compute the benefit of eliminating the insns in the block whose
   last insn is LAST.  This may be a group of insns used to compute a
   value directly or can contain a library call.  */

static int
libcall_benefit (last)
     rtx last;
{
  rtx insn;
  int benefit = 0;

  for (insn = XEXP (find_reg_note (last, REG_RETVAL, NULL_RTX), 0);
       insn != last; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == CALL_INSN)
	benefit += 10;		/* Assume at least this many insns in a library
				   routine.  */
      else if (GET_CODE (insn) == INSN
	       && GET_CODE (PATTERN (insn)) != USE
	       && GET_CODE (PATTERN (insn)) != CLOBBER)
	benefit++;
    }

  return benefit;
}

/* Skip COUNT insns from INSN, counting library calls as 1 insn.  */

static rtx
skip_consec_insns (insn, count)
     rtx insn;
     int count;
{
  for (; count > 0; count--)
    {
      rtx temp;

      /* If first insn of libcall sequence, skip to end.  */
      /* Do this at start of loop, since INSN is guaranteed to
	 be an insn here.  */
      if (GET_CODE (insn) != NOTE
	  && (temp = find_reg_note (insn, REG_LIBCALL, NULL_RTX)))
	insn = XEXP (temp, 0);

      do
	insn = NEXT_INSN (insn);
      while (GET_CODE (insn) == NOTE);
    }

  return insn;
}

/* Ignore any movable whose insn falls within a libcall
   which is part of another movable.
   We make use of the fact that the movable for the libcall value
   was made later and so appears later on the chain.  */

static void
ignore_some_movables (movables)
     struct loop_movables *movables;
{
  struct movable *m, *m1;

  for (m = movables->head; m; m = m->next)
    {
      /* Is this a movable for the value of a libcall?  */
      rtx note = find_reg_note (m->insn, REG_RETVAL, NULL_RTX);
      if (note)
	{
	  rtx insn;
	  /* Check for earlier movables inside that range,
	     and mark them invalid.  We cannot use LUIDs here because
	     insns created by loop.c for prior loops don't have LUIDs.
	     Rather than reject all such insns from movables, we just
	     explicitly check each insn in the libcall (since invariant
	     libcalls aren't that common).  */
	  for (insn = XEXP (note, 0); insn != m->insn; insn = NEXT_INSN (insn))
	    for (m1 = movables->head; m1 != m; m1 = m1->next)
	      if (m1->insn == insn)
		m1->done = 1;
	}
    }
}

/* For each movable insn, see if the reg that it loads
   leads when it dies right into another conditionally movable insn.
   If so, record that the second insn "forces" the first one,
   since the second can be moved only if the first is.  */

static void
force_movables (movables)
     struct loop_movables *movables;
{
  struct movable *m, *m1;

  for (m1 = movables->head; m1; m1 = m1->next)
    /* Omit this if moving just the (SET (REG) 0) of a zero-extend.  */
    if (!m1->partial && !m1->done)
      {
	int regno = m1->regno;
	for (m = m1->next; m; m = m->next)
	  /* ??? Could this be a bug?  What if CSE caused the
	     register of M1 to be used after this insn?
	     Since CSE does not update regno_last_uid,
	     this insn M->insn might not be where it dies.
	     But very likely this doesn't matter; what matters is
	     that M's reg is computed from M1's reg.  */
	  if (INSN_UID (m->insn) == REGNO_LAST_UID (regno)
	      && !m->done)
	    break;
	if (m != 0 && m->set_src == m1->set_dest
	    /* If m->consec, m->set_src isn't valid.  */
	    && m->consec == 0)
	  m = 0;

	/* Increase the priority of the moving the first insn
	   since it permits the second to be moved as well.  */
	if (m != 0)
	  {
	    m->forces = m1;
	    m1->lifetime += m->lifetime;
	    m1->savings += m->savings;
	  }
      }
}

/* Find invariant expressions that are equal and can be combined into
   one register.  */

static void
combine_movables (movables, regs)
     struct loop_movables *movables;
     struct loop_regs *regs;
{
  struct movable *m;
  char *matched_regs = (char *) xmalloc (regs->num);
  enum machine_mode mode;

  /* Regs that are set more than once are not allowed to match
     or be matched.  I'm no longer sure why not.  */
  /* Perhaps testing m->consec_sets would be more appropriate here?  */

  for (m = movables->head; m; m = m->next)
    if (m->match == 0 && regs->array[m->regno].n_times_set == 1
	&& !m->partial)
      {
	struct movable *m1;
	int regno = m->regno;

	memset (matched_regs, 0, regs->num);
	matched_regs[regno] = 1;

	/* We want later insns to match the first one.  Don't make the first
	   one match any later ones.  So start this loop at m->next.  */
	for (m1 = m->next; m1; m1 = m1->next)
	  /* ??? HACK!  move_movables does not verify that the replacement
	     is valid, which can have disasterous effects with hard regs
	     and match_dup.  Turn combination off for now.  */
	  if (0 && m != m1 && m1->match == 0
	      && regs->array[m1->regno].n_times_set == 1
	      /* A reg used outside the loop mustn't be eliminated.  */
	      && !m1->global
	      /* A reg used for zero-extending mustn't be eliminated.  */
	      && !m1->partial
	      && (matched_regs[m1->regno]
		  ||
		  (
		   /* Can combine regs with different modes loaded from the
		      same constant only if the modes are the same or
		      if both are integer modes with M wider or the same
		      width as M1.  The check for integer is redundant, but
		      safe, since the only case of differing destination
		      modes with equal sources is when both sources are
		      VOIDmode, i.e., CONST_INT.  */
		   (GET_MODE (m->set_dest) == GET_MODE (m1->set_dest)
		    || (GET_MODE_CLASS (GET_MODE (m->set_dest)) == MODE_INT
			&& GET_MODE_CLASS (GET_MODE (m1->set_dest)) == MODE_INT
			&& (GET_MODE_BITSIZE (GET_MODE (m->set_dest))
			    >= GET_MODE_BITSIZE (GET_MODE (m1->set_dest)))))
		   /* See if the source of M1 says it matches M.  */
		   && ((GET_CODE (m1->set_src) == REG
			&& matched_regs[REGNO (m1->set_src)])
		       || rtx_equal_for_loop_p (m->set_src, m1->set_src,
						movables, regs))))
	      && ((m->dependencies == m1->dependencies)
		  || rtx_equal_p (m->dependencies, m1->dependencies)))
	    {
	      m->lifetime += m1->lifetime;
	      m->savings += m1->savings;
	      m1->done = 1;
	      m1->match = m;
	      matched_regs[m1->regno] = 1;
	    }
      }

  /* Now combine the regs used for zero-extension.
     This can be done for those not marked `global'
     provided their lives don't overlap.  */

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_INT); mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    {
      struct movable *m0 = 0;

      /* Combine all the registers for extension from mode MODE.
	 Don't combine any that are used outside this loop.  */
      for (m = movables->head; m; m = m->next)
	if (m->partial && ! m->global
	    && mode == GET_MODE (SET_SRC (PATTERN (NEXT_INSN (m->insn)))))
	  {
	    struct movable *m1;

	    int first = REGNO_FIRST_LUID (m->regno);
	    int last = REGNO_LAST_LUID (m->regno);

	    if (m0 == 0)
	      {
		/* First one: don't check for overlap, just record it.  */
		m0 = m;
		continue;
	      }

	    /* Make sure they extend to the same mode.
	       (Almost always true.)  */
	    if (GET_MODE (m->set_dest) != GET_MODE (m0->set_dest))
	      continue;

	    /* We already have one: check for overlap with those
	       already combined together.  */
	    for (m1 = movables->head; m1 != m; m1 = m1->next)
	      if (m1 == m0 || (m1->partial && m1->match == m0))
		if (! (REGNO_FIRST_LUID (m1->regno) > last
		       || REGNO_LAST_LUID (m1->regno) < first))
		  goto overlap;

	    /* No overlap: we can combine this with the others.  */
	    m0->lifetime += m->lifetime;
	    m0->savings += m->savings;
	    m->done = 1;
	    m->match = m0;

	  overlap:
	    ;
	  }
    }

  /* Clean up.  */
  free (matched_regs);
}

/* Returns the number of movable instructions in LOOP that were not
   moved outside the loop.  */

static int
num_unmoved_movables (loop)
     const struct loop *loop;
{
  int num = 0;
  struct movable *m;

  for (m = LOOP_MOVABLES (loop)->head; m; m = m->next)
    if (!m->done)
      ++num;

  return num;
}


/* Return 1 if regs X and Y will become the same if moved.  */

static int
regs_match_p (x, y, movables)
     rtx x, y;
     struct loop_movables *movables;
{
  unsigned int xn = REGNO (x);
  unsigned int yn = REGNO (y);
  struct movable *mx, *my;

  for (mx = movables->head; mx; mx = mx->next)
    if (mx->regno == xn)
      break;

  for (my = movables->head; my; my = my->next)
    if (my->regno == yn)
      break;

  return (mx && my
	  && ((mx->match == my->match && mx->match != 0)
	      || mx->match == my
	      || mx == my->match));
}

/* Return 1 if X and Y are identical-looking rtx's.
   This is the Lisp function EQUAL for rtx arguments.

   If two registers are matching movables or a movable register and an
   equivalent constant, consider them equal.  */

static int
rtx_equal_for_loop_p (x, y, movables, regs)
     rtx x, y;
     struct loop_movables *movables;
     struct loop_regs *regs;
{
  int i;
  int j;
  struct movable *m;
  enum rtx_code code;
  const char *fmt;

  if (x == y)
    return 1;
  if (x == 0 || y == 0)
    return 0;

  code = GET_CODE (x);

  /* If we have a register and a constant, they may sometimes be
     equal.  */
  if (GET_CODE (x) == REG && regs->array[REGNO (x)].set_in_loop == -2
      && CONSTANT_P (y))
    {
      for (m = movables->head; m; m = m->next)
	if (m->move_insn && m->regno == REGNO (x)
	    && rtx_equal_p (m->set_src, y))
	  return 1;
    }
  else if (GET_CODE (y) == REG && regs->array[REGNO (y)].set_in_loop == -2
	   && CONSTANT_P (x))
    {
      for (m = movables->head; m; m = m->next)
	if (m->move_insn && m->regno == REGNO (y)
	    && rtx_equal_p (m->set_src, x))
	  return 1;
    }

  /* Otherwise, rtx's of different codes cannot be equal.  */
  if (code != GET_CODE (y))
    return 0;

  /* (MULT:SI x y) and (MULT:HI x y) are NOT equivalent.
     (REG:SI x) and (REG:HI x) are NOT equivalent.  */

  if (GET_MODE (x) != GET_MODE (y))
    return 0;

  /* These three types of rtx's can be compared nonrecursively.  */
  if (code == REG)
    return (REGNO (x) == REGNO (y) || regs_match_p (x, y, movables));

  if (code == LABEL_REF)
    return XEXP (x, 0) == XEXP (y, 0);
  if (code == SYMBOL_REF)
    return XSTR (x, 0) == XSTR (y, 0);

  /* Compare the elements.  If any pair of corresponding elements
     fail to match, return 0 for the whole things.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      switch (fmt[i])
	{
	case 'w':
	  if (XWINT (x, i) != XWINT (y, i))
	    return 0;
	  break;

	case 'i':
	  if (XINT (x, i) != XINT (y, i))
	    return 0;
	  break;

	case 'E':
	  /* Two vectors must have the same length.  */
	  if (XVECLEN (x, i) != XVECLEN (y, i))
	    return 0;

	  /* And the corresponding elements must match.  */
	  for (j = 0; j < XVECLEN (x, i); j++)
	    if (rtx_equal_for_loop_p (XVECEXP (x, i, j), XVECEXP (y, i, j),
				      movables, regs) == 0)
	      return 0;
	  break;

	case 'e':
	  if (rtx_equal_for_loop_p (XEXP (x, i), XEXP (y, i), movables, regs)
	      == 0)
	    return 0;
	  break;

	case 's':
	  if (strcmp (XSTR (x, i), XSTR (y, i)))
	    return 0;
	  break;

	case 'u':
	  /* These are just backpointers, so they don't matter.  */
	  break;

	case '0':
	  break;

	  /* It is believed that rtx's at this level will never
	     contain anything but integers and other rtx's,
	     except for within LABEL_REFs and SYMBOL_REFs.  */
	default:
	  abort ();
	}
    }
  return 1;
}

/* If X contains any LABEL_REF's, add REG_LABEL notes for them to all
   insns in INSNS which use the reference.  LABEL_NUSES for CODE_LABEL
   references is incremented once for each added note.  */

static void
add_label_notes (x, insns)
     rtx x;
     rtx insns;
{
  enum rtx_code code = GET_CODE (x);
  int i, j;
  const char *fmt;
  rtx insn;

  if (code == LABEL_REF && !LABEL_REF_NONLOCAL_P (x))
    {
      /* This code used to ignore labels that referred to dispatch tables to
         avoid flow generating (slighly) worse code.

         We no longer ignore such label references (see LABEL_REF handling in
         mark_jump_label for additional information).  */
      for (insn = insns; insn; insn = NEXT_INSN (insn))
	if (reg_mentioned_p (XEXP (x, 0), insn))
	  {
	    REG_NOTES (insn) = gen_rtx_INSN_LIST (REG_LABEL, XEXP (x, 0),
						  REG_NOTES (insn));
	    if (LABEL_P (XEXP (x, 0)))
	      LABEL_NUSES (XEXP (x, 0))++;
	  }
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	add_label_notes (XEXP (x, i), insns);
      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  add_label_notes (XVECEXP (x, i, j), insns);
    }
}

/* Scan MOVABLES, and move the insns that deserve to be moved.
   If two matching movables are combined, replace one reg with the
   other throughout.  */

static void
move_movables (loop, movables, threshold, insn_count)
     struct loop *loop;
     struct loop_movables *movables;
     int threshold;
     int insn_count;
{
  struct loop_regs *regs = LOOP_REGS (loop);
  int nregs = regs->num;
  rtx new_start = 0;
  struct movable *m;
  rtx p;
  rtx loop_start = loop->start;
  rtx loop_end = loop->end;
  /* Map of pseudo-register replacements to handle combining
     when we move several insns that load the same value
     into different pseudo-registers.  */
  rtx *reg_map = (rtx *) xcalloc (nregs, sizeof (rtx));
  char *already_moved = (char *) xcalloc (nregs, sizeof (char));

  for (m = movables->head; m; m = m->next)
    {
      /* Describe this movable insn.  */

      if (loop_dump_stream)
	{
	  fprintf (loop_dump_stream, "Insn %d: regno %d (life %d), ",
		   INSN_UID (m->insn), m->regno, m->lifetime);
	  if (m->consec > 0)
	    fprintf (loop_dump_stream, "consec %d, ", m->consec);
	  if (m->cond)
	    fprintf (loop_dump_stream, "cond ");
	  if (m->force)
	    fprintf (loop_dump_stream, "force ");
	  if (m->global)
	    fprintf (loop_dump_stream, "global ");
	  if (m->done)
	    fprintf (loop_dump_stream, "done ");
	  if (m->move_insn)
	    fprintf (loop_dump_stream, "move-insn ");
	  if (m->match)
	    fprintf (loop_dump_stream, "matches %d ",
		     INSN_UID (m->match->insn));
	  if (m->forces)
	    fprintf (loop_dump_stream, "forces %d ",
		     INSN_UID (m->forces->insn));
	}

      /* Ignore the insn if it's already done (it matched something else).
	 Otherwise, see if it is now safe to move.  */

      if (!m->done
	  && (! m->cond
	      || (1 == loop_invariant_p (loop, m->set_src)
		  && (m->dependencies == 0
		      || 1 == loop_invariant_p (loop, m->dependencies))
		  && (m->consec == 0
		      || 1 == consec_sets_invariant_p (loop, m->set_dest,
						       m->consec + 1,
						       m->insn))))
	  && (! m->forces || m->forces->done))
	{
	  int regno;
	  rtx p;
	  int savings = m->savings;

	  /* We have an insn that is safe to move.
	     Compute its desirability.  */

	  p = m->insn;
	  regno = m->regno;

	  if (loop_dump_stream)
	    fprintf (loop_dump_stream, "savings %d ", savings);

	  if (regs->array[regno].moved_once && loop_dump_stream)
	    fprintf (loop_dump_stream, "halved since already moved ");

	  /* An insn MUST be moved if we already moved something else
	     which is safe only if this one is moved too: that is,
	     if already_moved[REGNO] is nonzero.  */

	  /* An insn is desirable to move if the new lifetime of the
	     register is no more than THRESHOLD times the old lifetime.
	     If it's not desirable, it means the loop is so big
	     that moving won't speed things up much,
	     and it is liable to make register usage worse.  */

	  /* It is also desirable to move if it can be moved at no
	     extra cost because something else was already moved.  */

	  if (already_moved[regno]
	      || flag_move_all_movables
	      || (threshold * savings * m->lifetime) >=
		 (regs->array[regno].moved_once ? insn_count * 2 : insn_count)
	      || (m->forces && m->forces->done
		  && regs->array[m->forces->regno].n_times_set == 1))
	    {
	      int count;
	      struct movable *m1;
	      rtx first = NULL_RTX;

	      /* Now move the insns that set the reg.  */

	      if (m->partial && m->match)
		{
		  rtx newpat, i1;
		  rtx r1, r2;
		  /* Find the end of this chain of matching regs.
		     Thus, we load each reg in the chain from that one reg.
		     And that reg is loaded with 0 directly,
		     since it has ->match == 0.  */
		  for (m1 = m; m1->match; m1 = m1->match);
		  newpat = gen_move_insn (SET_DEST (PATTERN (m->insn)),
					  SET_DEST (PATTERN (m1->insn)));
		  i1 = loop_insn_hoist (loop, newpat);

		  /* Mark the moved, invariant reg as being allowed to
		     share a hard reg with the other matching invariant.  */
		  REG_NOTES (i1) = REG_NOTES (m->insn);
		  r1 = SET_DEST (PATTERN (m->insn));
		  r2 = SET_DEST (PATTERN (m1->insn));
		  regs_may_share
		    = gen_rtx_EXPR_LIST (VOIDmode, r1,
					 gen_rtx_EXPR_LIST (VOIDmode, r2,
							    regs_may_share));
		  delete_insn (m->insn);

		  if (new_start == 0)
		    new_start = i1;

		  if (loop_dump_stream)
		    fprintf (loop_dump_stream, " moved to %d", INSN_UID (i1));
		}
	      /* If we are to re-generate the item being moved with a
		 new move insn, first delete what we have and then emit
		 the move insn before the loop.  */
	      else if (m->move_insn)
		{
		  rtx i1, temp, seq;

		  for (count = m->consec; count >= 0; count--)
		    {
		      /* If this is the first insn of a library call sequence,
			 something is very wrong.  */
		      if (GET_CODE (p) != NOTE
			  && (temp = find_reg_note (p, REG_LIBCALL, NULL_RTX)))
			abort ();

		      /* If this is the last insn of a libcall sequence, then
			 delete every insn in the sequence except the last.
			 The last insn is handled in the normal manner.  */
		      if (GET_CODE (p) != NOTE
			  && (temp = find_reg_note (p, REG_RETVAL, NULL_RTX)))
			{
			  temp = XEXP (temp, 0);
			  while (temp != p)
			    temp = delete_insn (temp);
			}

		      temp = p;
		      p = delete_insn (p);

		      /* simplify_giv_expr expects that it can walk the insns
			 at m->insn forwards and see this old sequence we are
			 tossing here.  delete_insn does preserve the next
			 pointers, but when we skip over a NOTE we must fix
			 it up.  Otherwise that code walks into the non-deleted
			 insn stream.  */
		      while (p && GET_CODE (p) == NOTE)
			p = NEXT_INSN (temp) = NEXT_INSN (p);
		    }

		  start_sequence ();
		  emit_move_insn (m->set_dest, m->set_src);
		  temp = get_insns ();
		  seq = gen_sequence ();
		  end_sequence ();

		  add_label_notes (m->set_src, temp);

		  i1 = loop_insn_hoist (loop, seq);
		  if (! find_reg_note (i1, REG_EQUAL, NULL_RTX))
		    set_unique_reg_note (i1,
					 m->is_equiv ? REG_EQUIV : REG_EQUAL,
					 m->set_src);

		  if (loop_dump_stream)
		    fprintf (loop_dump_stream, " moved to %d", INSN_UID (i1));

		  /* The more regs we move, the less we like moving them.  */
		  threshold -= 3;
		}
	      else
		{
		  for (count = m->consec; count >= 0; count--)
		    {
		      rtx i1, temp;

		      /* If first insn of libcall sequence, skip to end.  */
		      /* Do this at start of loop, since p is guaranteed to
			 be an insn here.  */
		      if (GET_CODE (p) != NOTE
			  && (temp = find_reg_note (p, REG_LIBCALL, NULL_RTX)))
			p = XEXP (temp, 0);

		      /* If last insn of libcall sequence, move all
			 insns except the last before the loop.  The last
			 insn is handled in the normal manner.  */
		      if (GET_CODE (p) != NOTE
			  && (temp = find_reg_note (p, REG_RETVAL, NULL_RTX)))
			{
			  rtx fn_address = 0;
			  rtx fn_reg = 0;
			  rtx fn_address_insn = 0;

			  first = 0;
			  for (temp = XEXP (temp, 0); temp != p;
			       temp = NEXT_INSN (temp))
			    {
			      rtx body;
			      rtx n;
			      rtx next;

			      if (GET_CODE (temp) == NOTE)
				continue;

			      body = PATTERN (temp);

			      /* Find the next insn after TEMP,
				 not counting USE or NOTE insns.  */
			      for (next = NEXT_INSN (temp); next != p;
				   next = NEXT_INSN (next))
				if (! (GET_CODE (next) == INSN
				       && GET_CODE (PATTERN (next)) == USE)
				    && GET_CODE (next) != NOTE)
				  break;

			      /* If that is the call, this may be the insn
				 that loads the function address.

				 Extract the function address from the insn
				 that loads it into a register.
				 If this insn was cse'd, we get incorrect code.

				 So emit a new move insn that copies the
				 function address into the register that the
				 call insn will use.  flow.c will delete any
				 redundant stores that we have created.  */
			      if (GET_CODE (next) == CALL_INSN
				  && GET_CODE (body) == SET
				  && GET_CODE (SET_DEST (body)) == REG
				  && (n = find_reg_note (temp, REG_EQUAL,
							 NULL_RTX)))
				{
				  fn_reg = SET_SRC (body);
				  if (GET_CODE (fn_reg) != REG)
				    fn_reg = SET_DEST (body);
				  fn_address = XEXP (n, 0);
				  fn_address_insn = temp;
				}
			      /* We have the call insn.
				 If it uses the register we suspect it might,
				 load it with the correct address directly.  */
			      if (GET_CODE (temp) == CALL_INSN
				  && fn_address != 0
				  && reg_referenced_p (fn_reg, body))
				loop_insn_emit_after (loop, 0, fn_address_insn,
						      gen_move_insn
						      (fn_reg, fn_address));

			      if (GET_CODE (temp) == CALL_INSN)
				{
				  i1 = loop_call_insn_hoist (loop, body);
				  /* Because the USAGE information potentially
				     contains objects other than hard registers
				     we need to copy it.  */
				  if (CALL_INSN_FUNCTION_USAGE (temp))
				    CALL_INSN_FUNCTION_USAGE (i1)
				      = copy_rtx (CALL_INSN_FUNCTION_USAGE (temp));
				}
			      else
				i1 = loop_insn_hoist (loop, body);
			      if (first == 0)
				first = i1;
			      if (temp == fn_address_insn)
				fn_address_insn = i1;
			      REG_NOTES (i1) = REG_NOTES (temp);
			      REG_NOTES (temp) = NULL;
			      delete_insn (temp);
			    }
			  if (new_start == 0)
			    new_start = first;
			}
		      if (m->savemode != VOIDmode)
			{
			  /* P sets REG to zero; but we should clear only
			     the bits that are not covered by the mode
			     m->savemode.  */
			  rtx reg = m->set_dest;
			  rtx sequence;
			  rtx tem;

			  start_sequence ();
			  tem = expand_simple_binop
			    (GET_MODE (reg), AND, reg,
			     GEN_INT ((((HOST_WIDE_INT) 1
					<< GET_MODE_BITSIZE (m->savemode)))
				      - 1),
			     reg, 1, OPTAB_LIB_WIDEN);
			  if (tem == 0)
			    abort ();
			  if (tem != reg)
			    emit_move_insn (reg, tem);
			  sequence = gen_sequence ();
			  end_sequence ();
			  i1 = loop_insn_hoist (loop, sequence);
			}
		      else if (GET_CODE (p) == CALL_INSN)
			{
			  i1 = loop_call_insn_hoist (loop, PATTERN (p));
			  /* Because the USAGE information potentially
			     contains objects other than hard registers
			     we need to copy it.  */
			  if (CALL_INSN_FUNCTION_USAGE (p))
			    CALL_INSN_FUNCTION_USAGE (i1)
			      = copy_rtx (CALL_INSN_FUNCTION_USAGE (p));
			}
		      else if (count == m->consec && m->move_insn_first)
			{
			  rtx seq;
			  /* The SET_SRC might not be invariant, so we must
			     use the REG_EQUAL note.  */
			  start_sequence ();
			  emit_move_insn (m->set_dest, m->set_src);
			  temp = get_insns ();
			  seq = gen_sequence ();
			  end_sequence ();

			  add_label_notes (m->set_src, temp);

			  i1 = loop_insn_hoist (loop, seq);
			  if (! find_reg_note (i1, REG_EQUAL, NULL_RTX))
			    set_unique_reg_note (i1, m->is_equiv ? REG_EQUIV
						     : REG_EQUAL, m->set_src);
			}
		      else
			i1 = loop_insn_hoist (loop, PATTERN (p));

		      if (REG_NOTES (i1) == 0)
			{
			  REG_NOTES (i1) = REG_NOTES (p);
			  REG_NOTES (p) = NULL;

			  /* If there is a REG_EQUAL note present whose value
			     is not loop invariant, then delete it, since it
			     may cause problems with later optimization passes.
			     It is possible for cse to create such notes
			     like this as a result of record_jump_cond.  */

			  if ((temp = find_reg_note (i1, REG_EQUAL, NULL_RTX))
			      && ! loop_invariant_p (loop, XEXP (temp, 0)))
			    remove_note (i1, temp);
			}

		      if (new_start == 0)
			new_start = i1;

		      if (loop_dump_stream)
			fprintf (loop_dump_stream, " moved to %d",
				 INSN_UID (i1));

		      /* If library call, now fix the REG_NOTES that contain
			 insn pointers, namely REG_LIBCALL on FIRST
			 and REG_RETVAL on I1.  */
		      if ((temp = find_reg_note (i1, REG_RETVAL, NULL_RTX)))
			{
			  XEXP (temp, 0) = first;
			  temp = find_reg_note (first, REG_LIBCALL, NULL_RTX);
			  XEXP (temp, 0) = i1;
			}

		      temp = p;
		      delete_insn (p);
		      p = NEXT_INSN (p);

		      /* simplify_giv_expr expects that it can walk the insns
			 at m->insn forwards and see this old sequence we are
			 tossing here.  delete_insn does preserve the next
			 pointers, but when we skip over a NOTE we must fix
			 it up.  Otherwise that code walks into the non-deleted
			 insn stream.  */
		      while (p && GET_CODE (p) == NOTE)
			p = NEXT_INSN (temp) = NEXT_INSN (p);
		    }

		  /* The more regs we move, the less we like moving them.  */
		  threshold -= 3;
		}

	      /* Any other movable that loads the same register
		 MUST be moved.  */
	      already_moved[regno] = 1;

	      /* This reg has been moved out of one loop.  */
	      regs->array[regno].moved_once = 1;

	      /* The reg set here is now invariant.  */
	      if (! m->partial)
		{
		  int i;
		  for (i = 0; i < LOOP_REGNO_NREGS (regno, m->set_dest); i++)
		    regs->array[regno+i].set_in_loop = 0;
		}

	      m->done = 1;

	      /* Change the length-of-life info for the register
		 to say it lives at least the full length of this loop.
		 This will help guide optimizations in outer loops.  */

	      if (REGNO_FIRST_LUID (regno) > INSN_LUID (loop_start))
		/* This is the old insn before all the moved insns.
		   We can't use the moved insn because it is out of range
		   in uid_luid.  Only the old insns have luids.  */
		REGNO_FIRST_UID (regno) = INSN_UID (loop_start);
	      if (REGNO_LAST_LUID (regno) < INSN_LUID (loop_end))
		REGNO_LAST_UID (regno) = INSN_UID (loop_end);

	      /* Combine with this moved insn any other matching movables.  */

	      if (! m->partial)
		for (m1 = movables->head; m1; m1 = m1->next)
		  if (m1->match == m)
		    {
		      rtx temp;

		      /* Schedule the reg loaded by M1
			 for replacement so that shares the reg of M.
			 If the modes differ (only possible in restricted
			 circumstances, make a SUBREG.

			 Note this assumes that the target dependent files
			 treat REG and SUBREG equally, including within
			 GO_IF_LEGITIMATE_ADDRESS and in all the
			 predicates since we never verify that replacing the
			 original register with a SUBREG results in a
			 recognizable insn.  */
		      if (GET_MODE (m->set_dest) == GET_MODE (m1->set_dest))
			reg_map[m1->regno] = m->set_dest;
		      else
			reg_map[m1->regno]
			  = gen_lowpart_common (GET_MODE (m1->set_dest),
						m->set_dest);

		      /* Get rid of the matching insn
			 and prevent further processing of it.  */
		      m1->done = 1;

		      /* if library call, delete all insns.  */
		      if ((temp = find_reg_note (m1->insn, REG_RETVAL,
						 NULL_RTX)))
			delete_insn_chain (XEXP (temp, 0), m1->insn);
		      else
		        delete_insn (m1->insn);

		      /* Any other movable that loads the same register
			 MUST be moved.  */
		      already_moved[m1->regno] = 1;

		      /* The reg merged here is now invariant,
			 if the reg it matches is invariant.  */
		      if (! m->partial)
			{
			  int i;
			  for (i = 0;
			       i < LOOP_REGNO_NREGS (regno, m1->set_dest);
			       i++)
			    regs->array[m1->regno+i].set_in_loop = 0;
			}
		    }
	    }
	  else if (loop_dump_stream)
	    fprintf (loop_dump_stream, "not desirable");
	}
      else if (loop_dump_stream && !m->match)
	fprintf (loop_dump_stream, "not safe");

      if (loop_dump_stream)
	fprintf (loop_dump_stream, "\n");
    }

  if (new_start == 0)
    new_start = loop_start;

  /* Go through all the instructions in the loop, making
     all the register substitutions scheduled in REG_MAP.  */
  for (p = new_start; p != loop_end; p = NEXT_INSN (p))
    if (GET_CODE (p) == INSN || GET_CODE (p) == JUMP_INSN
	|| GET_CODE (p) == CALL_INSN)
      {
	replace_regs (PATTERN (p), reg_map, nregs, 0);
	replace_regs (REG_NOTES (p), reg_map, nregs, 0);
	INSN_CODE (p) = -1;
      }

  /* Clean up.  */
  free (reg_map);
  free (already_moved);
}


static void
loop_movables_add (movables, m)
     struct loop_movables *movables;
     struct movable *m;
{
  if (movables->head == 0)
    movables->head = m;
  else
    movables->last->next = m;
  movables->last = m;
}


static void
loop_movables_free (movables)
     struct loop_movables *movables;
{
  struct movable *m;
  struct movable *m_next;

  for (m = movables->head; m; m = m_next)
    {
      m_next = m->next;
      free (m);
    }
}

#if 0
/* Scan X and replace the address of any MEM in it with ADDR.
   REG is the address that MEM should have before the replacement.  */

static void
replace_call_address (x, reg, addr)
     rtx x, reg, addr;
{
  enum rtx_code code;
  int i;
  const char *fmt;

  if (x == 0)
    return;
  code = GET_CODE (x);
  switch (code)
    {
    case PC:
    case CC0:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
    case REG:
      return;

    case SET:
      /* Short cut for very common case.  */
      replace_call_address (XEXP (x, 1), reg, addr);
      return;

    case CALL:
      /* Short cut for very common case.  */
      replace_call_address (XEXP (x, 0), reg, addr);
      return;

    case MEM:
      /* If this MEM uses a reg other than the one we expected,
	 something is wrong.  */
      if (XEXP (x, 0) != reg)
	abort ();
      XEXP (x, 0) = addr;
      return;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	replace_call_address (XEXP (x, i), reg, addr);
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    replace_call_address (XVECEXP (x, i, j), reg, addr);
	}
    }
}
#endif

/* Return the number of memory refs to addresses that vary
   in the rtx X.  */

static int
count_nonfixed_reads (loop, x)
     const struct loop *loop;
     rtx x;
{
  enum rtx_code code;
  int i;
  const char *fmt;
  int value;

  if (x == 0)
    return 0;

  code = GET_CODE (x);
  switch (code)
    {
    case PC:
    case CC0:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
    case REG:
      return 0;

    case MEM:
      return ((loop_invariant_p (loop, XEXP (x, 0)) != 1)
	      + count_nonfixed_reads (loop, XEXP (x, 0)));

    default:
      break;
    }

  value = 0;
  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	value += count_nonfixed_reads (loop, XEXP (x, i));
      if (fmt[i] == 'E')
	{
	  int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    value += count_nonfixed_reads (loop, XVECEXP (x, i, j));
	}
    }
  return value;
}

/* Scan a loop setting the elements `cont', `vtop', `loops_enclosed',
   `has_call', `has_nonconst_call', `has_volatile', `has_tablejump',
   `unknown_address_altered', `unknown_constant_address_altered', and
   `num_mem_sets' in LOOP.  Also, fill in the array `mems' and the
   list `store_mems' in LOOP.  */

static void
prescan_loop (loop)
     struct loop *loop;
{
  int level = 1;
  rtx insn;
  struct loop_info *loop_info = LOOP_INFO (loop);
  rtx start = loop->start;
  rtx end = loop->end;
  /* The label after END.  Jumping here is just like falling off the
     end of the loop.  We use next_nonnote_insn instead of next_label
     as a hedge against the (pathological) case where some actual insn
     might end up between the two.  */
  rtx exit_target = next_nonnote_insn (end);

  loop_info->has_indirect_jump = indirect_jump_in_function;
  loop_info->pre_header_has_call = 0;
  loop_info->has_call = 0;
  loop_info->has_nonconst_call = 0;
  loop_info->has_volatile = 0;
  loop_info->has_tablejump = 0;
  loop_info->has_multiple_exit_targets = 0;
  loop->level = 1;

  loop_info->unknown_address_altered = 0;
  loop_info->unknown_constant_address_altered = 0;
  loop_info->store_mems = NULL_RTX;
  loop_info->first_loop_store_insn = NULL_RTX;
  loop_info->mems_idx = 0;
  loop_info->num_mem_sets = 0;


  for (insn = start; insn && GET_CODE (insn) != CODE_LABEL;
       insn = PREV_INSN (insn))
    {
      if (GET_CODE (insn) == CALL_INSN)
	{
	  loop_info->pre_header_has_call = 1;
	  break;
	}
    }

  for (insn = NEXT_INSN (start); insn != NEXT_INSN (end);
       insn = NEXT_INSN (insn))
    {
      switch (GET_CODE (insn))
	{
	case NOTE:
	  if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_BEG)
	    {
	      ++level;
	      /* Count number of loops contained in this one.  */
	      loop->level++;
	    }
	  else if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_END)
	    --level;
	  break;

	case CALL_INSN:
	  if (! CONST_OR_PURE_CALL_P (insn))
	    {
	      loop_info->unknown_address_altered = 1;
	      loop_info->has_nonconst_call = 1;
	    }
	  else if (pure_call_p (insn))
	    loop_info->has_nonconst_call = 1;
	  loop_info->has_call = 1;
	  if (can_throw_internal (insn))
	    loop_info->has_multiple_exit_targets = 1;
	  break;

	case JUMP_INSN:
	  if (! loop_info->has_multiple_exit_targets)
	    {
	      rtx set = pc_set (insn);

	      if (set)
		{
		  rtx src = SET_SRC (set);
		  rtx label1, label2;

		  if (GET_CODE (src) == IF_THEN_ELSE)
		    {
		      label1 = XEXP (src, 1);
		      label2 = XEXP (src, 2);
		    }
		  else
		    {
		      label1 = src;
		      label2 = NULL_RTX;
		    }

		  do
		    {
		      if (label1 && label1 != pc_rtx)
			{
			  if (GET_CODE (label1) != LABEL_REF)
			    {
			      /* Something tricky.  */
			      loop_info->has_multiple_exit_targets = 1;
			      break;
			    }
			  else if (XEXP (label1, 0) != exit_target
				   && LABEL_OUTSIDE_LOOP_P (label1))
			    {
			      /* A jump outside the current loop.  */
			      loop_info->has_multiple_exit_targets = 1;
			      break;
			    }
			}

		      label1 = label2;
		      label2 = NULL_RTX;
		    }
		  while (label1);
		}
	      else
		{
		  /* A return, or something tricky.  */
		  loop_info->has_multiple_exit_targets = 1;
		}
	    }
	  /* FALLTHRU */

	case INSN:
	  if (volatile_refs_p (PATTERN (insn)))
	    loop_info->has_volatile = 1;

	  if (GET_CODE (insn) == JUMP_INSN
	      && (GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC
		  || GET_CODE (PATTERN (insn)) == ADDR_VEC))
	    loop_info->has_tablejump = 1;

	  note_stores (PATTERN (insn), note_addr_stored, loop_info);
	  if (! loop_info->first_loop_store_insn && loop_info->store_mems)
	    loop_info->first_loop_store_insn = insn;

	  if (flag_non_call_exceptions && can_throw_internal (insn))
	    loop_info->has_multiple_exit_targets = 1;
	  break;

	default:
	  break;
	}
    }

  /* Now, rescan the loop, setting up the LOOP_MEMS array.  */
  if (/* An exception thrown by a called function might land us
	 anywhere.  */
      ! loop_info->has_nonconst_call
      /* We don't want loads for MEMs moved to a location before the
	 one at which their stack memory becomes allocated.  (Note
	 that this is not a problem for malloc, etc., since those
	 require actual function calls.  */
      && ! current_function_calls_alloca
      /* There are ways to leave the loop other than falling off the
	 end.  */
      && ! loop_info->has_multiple_exit_targets)
    for (insn = NEXT_INSN (start); insn != NEXT_INSN (end);
	 insn = NEXT_INSN (insn))
      for_each_rtx (&insn, insert_loop_mem, loop_info);

  /* BLKmode MEMs are added to LOOP_STORE_MEM as necessary so
     that loop_invariant_p and load_mems can use true_dependence
     to determine what is really clobbered.  */
  if (loop_info->unknown_address_altered)
    {
      rtx mem = gen_rtx_MEM (BLKmode, const0_rtx);

      loop_info->store_mems
	= gen_rtx_EXPR_LIST (VOIDmode, mem, loop_info->store_mems);
    }
  if (loop_info->unknown_constant_address_altered)
    {
      rtx mem = gen_rtx_MEM (BLKmode, const0_rtx);

      RTX_UNCHANGING_P (mem) = 1;
      loop_info->store_mems
	= gen_rtx_EXPR_LIST (VOIDmode, mem, loop_info->store_mems);
    }
}

/* Invalidate all loops containing LABEL.  */

static void
invalidate_loops_containing_label (label)
     rtx label;
{
  struct loop *loop;
  for (loop = uid_loop[INSN_UID (label)]; loop; loop = loop->outer)
    loop->invalid = 1;
}

/* Scan the function looking for loops.  Record the start and end of each loop.
   Also mark as invalid loops any loops that contain a setjmp or are branched
   to from outside the loop.  */

static void
find_and_verify_loops (f, loops)
     rtx f;
     struct loops *loops;
{
  rtx insn;
  rtx label;
  int num_loops;
  struct loop *current_loop;
  struct loop *next_loop;
  struct loop *loop;

  num_loops = loops->num;

  compute_luids (f, NULL_RTX, 0);

  /* If there are jumps to undefined labels,
     treat them as jumps out of any/all loops.
     This also avoids writing past end of tables when there are no loops.  */
  uid_loop[0] = NULL;

  /* Find boundaries of loops, mark which loops are contained within
     loops, and invalidate loops that have setjmp.  */

  num_loops = 0;
  current_loop = NULL;
  for (insn = f; insn; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == NOTE)
	switch (NOTE_LINE_NUMBER (insn))
	  {
	  case NOTE_INSN_LOOP_BEG:
	    next_loop = loops->array + num_loops;
	    next_loop->num = num_loops;
	    num_loops++;
	    next_loop->start = insn;
	    next_loop->outer = current_loop;
	    current_loop = next_loop;
	    break;

	  case NOTE_INSN_LOOP_CONT:
	    current_loop->cont = insn;
	    break;

	  case NOTE_INSN_LOOP_VTOP:
	    current_loop->vtop = insn;
	    break;

	  case NOTE_INSN_LOOP_END:
	    if (! current_loop)
	      abort ();

	    current_loop->end = insn;
	    current_loop = current_loop->outer;
	    break;

	  default:
	    break;
	  }

      if (GET_CODE (insn) == CALL_INSN
	  && find_reg_note (insn, REG_SETJMP, NULL))
	{
	  /* In this case, we must invalidate our current loop and any
	     enclosing loop.  */
	  for (loop = current_loop; loop; loop = loop->outer)
	    {
	      loop->invalid = 1;
	      if (loop_dump_stream)
		fprintf (loop_dump_stream,
			 "\nLoop at %d ignored due to setjmp.\n",
			 INSN_UID (loop->start));
	    }
	}

      /* Note that this will mark the NOTE_INSN_LOOP_END note as being in the
	 enclosing loop, but this doesn't matter.  */
      uid_loop[INSN_UID (insn)] = current_loop;
    }

  /* Any loop containing a label used in an initializer must be invalidated,
     because it can be jumped into from anywhere.  */
  for (label = forced_labels; label; label = XEXP (label, 1))
    invalidate_loops_containing_label (XEXP (label, 0));

  /* Any loop containing a label used for an exception handler must be
     invalidated, because it can be jumped into from anywhere.  */
  for_each_eh_label (invalidate_loops_containing_label);

  /* Now scan all insn's in the function.  If any JUMP_INSN branches into a
     loop that it is not contained within, that loop is marked invalid.
     If any INSN or CALL_INSN uses a label's address, then the loop containing
     that label is marked invalid, because it could be jumped into from
     anywhere.

     Also look for blocks of code ending in an unconditional branch that
     exits the loop.  If such a block is surrounded by a conditional
     branch around the block, move the block elsewhere (see below) and
     invert the jump to point to the code block.  This may eliminate a
     label in our loop and will simplify processing by both us and a
     possible second cse pass.  */

  for (insn = f; insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn))
      {
	struct loop *this_loop = uid_loop[INSN_UID (insn)];

	if (GET_CODE (insn) == INSN || GET_CODE (insn) == CALL_INSN)
	  {
	    rtx note = find_reg_note (insn, REG_LABEL, NULL_RTX);
	    if (note)
	      invalidate_loops_containing_label (XEXP (note, 0));
	  }

	if (GET_CODE (insn) != JUMP_INSN)
	  continue;

	mark_loop_jump (PATTERN (insn), this_loop);

	/* See if this is an unconditional branch outside the loop.  */
	if (this_loop
	    && (GET_CODE (PATTERN (insn)) == RETURN
		|| (any_uncondjump_p (insn)
		    && onlyjump_p (insn)
		    && (uid_loop[INSN_UID (JUMP_LABEL (insn))]
			!= this_loop)))
	    && get_max_uid () < max_uid_for_loop)
	  {
	    rtx p;
	    rtx our_next = next_real_insn (insn);
	    rtx last_insn_to_move = NEXT_INSN (insn);
	    struct loop *dest_loop;
	    struct loop *outer_loop = NULL;

	    /* Go backwards until we reach the start of the loop, a label,
	       or a JUMP_INSN.  */
	    for (p = PREV_INSN (insn);
		 GET_CODE (p) != CODE_LABEL
		 && ! (GET_CODE (p) == NOTE
		       && NOTE_LINE_NUMBER (p) == NOTE_INSN_LOOP_BEG)
		 && GET_CODE (p) != JUMP_INSN;
		 p = PREV_INSN (p))
	      ;

	    /* Check for the case where we have a jump to an inner nested
	       loop, and do not perform the optimization in that case.  */

	    if (JUMP_LABEL (insn))
	      {
		dest_loop = uid_loop[INSN_UID (JUMP_LABEL (insn))];
		if (dest_loop)
		  {
		    for (outer_loop = dest_loop; outer_loop;
			 outer_loop = outer_loop->outer)
		      if (outer_loop == this_loop)
			break;
		  }
	      }

	    /* Make sure that the target of P is within the current loop.  */

	    if (GET_CODE (p) == JUMP_INSN && JUMP_LABEL (p)
		&& uid_loop[INSN_UID (JUMP_LABEL (p))] != this_loop)
	      outer_loop = this_loop;

	    /* If we stopped on a JUMP_INSN to the next insn after INSN,
	       we have a block of code to try to move.

	       We look backward and then forward from the target of INSN
	       to find a BARRIER at the same loop depth as the target.
	       If we find such a BARRIER, we make a new label for the start
	       of the block, invert the jump in P and point it to that label,
	       and move the block of code to the spot we found.  */

	    if (! outer_loop
		&& GET_CODE (p) == JUMP_INSN
		&& JUMP_LABEL (p) != 0
		/* Just ignore jumps to labels that were never emitted.
		   These always indicate compilation errors.  */
		&& INSN_UID (JUMP_LABEL (p)) != 0
		&& any_condjump_p (p) && onlyjump_p (p)
		&& next_real_insn (JUMP_LABEL (p)) == our_next
		/* If it's not safe to move the sequence, then we
		   mustn't try.  */
		&& insns_safe_to_move_p (p, NEXT_INSN (insn),
					 &last_insn_to_move))
	      {
		rtx target
		  = JUMP_LABEL (insn) ? JUMP_LABEL (insn) : get_last_insn ();
		struct loop *target_loop = uid_loop[INSN_UID (target)];
		rtx loc, loc2;
		rtx tmp;

		/* Search for possible garbage past the conditional jumps
		   and look for the last barrier.  */
		for (tmp = last_insn_to_move;
		     tmp && GET_CODE (tmp) != CODE_LABEL; tmp = NEXT_INSN (tmp))
		  if (GET_CODE (tmp) == BARRIER)
		    last_insn_to_move = tmp;

		for (loc = target; loc; loc = PREV_INSN (loc))
		  if (GET_CODE (loc) == BARRIER
		      /* Don't move things inside a tablejump.  */
		      && ((loc2 = next_nonnote_insn (loc)) == 0
			  || GET_CODE (loc2) != CODE_LABEL
			  || (loc2 = next_nonnote_insn (loc2)) == 0
			  || GET_CODE (loc2) != JUMP_INSN
			  || (GET_CODE (PATTERN (loc2)) != ADDR_VEC
			      && GET_CODE (PATTERN (loc2)) != ADDR_DIFF_VEC))
		      && uid_loop[INSN_UID (loc)] == target_loop)
		    break;

		if (loc == 0)
		  for (loc = target; loc; loc = NEXT_INSN (loc))
		    if (GET_CODE (loc) == BARRIER
			/* Don't move things inside a tablejump.  */
			&& ((loc2 = next_nonnote_insn (loc)) == 0
			    || GET_CODE (loc2) != CODE_LABEL
			    || (loc2 = next_nonnote_insn (loc2)) == 0
			    || GET_CODE (loc2) != JUMP_INSN
			    || (GET_CODE (PATTERN (loc2)) != ADDR_VEC
				&& GET_CODE (PATTERN (loc2)) != ADDR_DIFF_VEC))
			&& uid_loop[INSN_UID (loc)] == target_loop)
		      break;

		if (loc)
		  {
		    rtx cond_label = JUMP_LABEL (p);
		    rtx new_label = get_label_after (p);

		    /* Ensure our label doesn't go away.  */
		    LABEL_NUSES (cond_label)++;

		    /* Verify that uid_loop is large enough and that
		       we can invert P.  */
		    if (invert_jump (p, new_label, 1))
		      {
			rtx q, r;

			/* If no suitable BARRIER was found, create a suitable
			   one before TARGET.  Since TARGET is a fall through
			   path, we'll need to insert an jump around our block
			   and add a BARRIER before TARGET.

			   This creates an extra unconditional jump outside
			   the loop.  However, the benefits of removing rarely
			   executed instructions from inside the loop usually
			   outweighs the cost of the extra unconditional jump
			   outside the loop.  */
			if (loc == 0)
			  {
			    rtx temp;

			    temp = gen_jump (JUMP_LABEL (insn));
			    temp = emit_jump_insn_before (temp, target);
			    JUMP_LABEL (temp) = JUMP_LABEL (insn);
			    LABEL_NUSES (JUMP_LABEL (insn))++;
			    loc = emit_barrier_before (target);
			  }

			/* Include the BARRIER after INSN and copy the
			   block after LOC.  */
			if (squeeze_notes (&new_label, &last_insn_to_move))
			  abort ();
			reorder_insns (new_label, last_insn_to_move, loc);

			/* All those insns are now in TARGET_LOOP.  */
			for (q = new_label;
			     q != NEXT_INSN (last_insn_to_move);
			     q = NEXT_INSN (q))
			  uid_loop[INSN_UID (q)] = target_loop;

			/* The label jumped to by INSN is no longer a loop
			   exit.  Unless INSN does not have a label (e.g.,
			   it is a RETURN insn), search loop->exit_labels
			   to find its label_ref, and remove it.  Also turn
			   off LABEL_OUTSIDE_LOOP_P bit.  */
			if (JUMP_LABEL (insn))
			  {
			    for (q = 0, r = this_loop->exit_labels;
				 r;
				 q = r, r = LABEL_NEXTREF (r))
			      if (XEXP (r, 0) == JUMP_LABEL (insn))
				{
				  LABEL_OUTSIDE_LOOP_P (r) = 0;
				  if (q)
				    LABEL_NEXTREF (q) = LABEL_NEXTREF (r);
				  else
				    this_loop->exit_labels = LABEL_NEXTREF (r);
				  break;
				}

			    for (loop = this_loop; loop && loop != target_loop;
				 loop = loop->outer)
			      loop->exit_count--;

			    /* If we didn't find it, then something is
			       wrong.  */
			    if (! r)
			      abort ();
			  }

			/* P is now a jump outside the loop, so it must be put
			   in loop->exit_labels, and marked as such.
			   The easiest way to do this is to just call
			   mark_loop_jump again for P.  */
			mark_loop_jump (PATTERN (p), this_loop);

			/* If INSN now jumps to the insn after it,
			   delete INSN.  */
			if (JUMP_LABEL (insn) != 0
			    && (next_real_insn (JUMP_LABEL (insn))
				== next_real_insn (insn)))
			  delete_related_insns (insn);
		      }

		    /* Continue the loop after where the conditional
		       branch used to jump, since the only branch insn
		       in the block (if it still remains) is an inter-loop
		       branch and hence needs no processing.  */
		    insn = NEXT_INSN (cond_label);

		    if (--LABEL_NUSES (cond_label) == 0)
		      delete_related_insns (cond_label);

		    /* This loop will be continued with NEXT_INSN (insn).  */
		    insn = PREV_INSN (insn);
		  }
	      }
	  }
      }
}

/* If any label in X jumps to a loop different from LOOP_NUM and any of the
   loops it is contained in, mark the target loop invalid.

   For speed, we assume that X is part of a pattern of a JUMP_INSN.  */

static void
mark_loop_jump (x, loop)
     rtx x;
     struct loop *loop;
{
  struct loop *dest_loop;
  struct loop *outer_loop;
  int i;

  switch (GET_CODE (x))
    {
    case PC:
    case USE:
    case CLOBBER:
    case REG:
    case MEM:
    case CONST_INT:
    case CONST_DOUBLE:
    case RETURN:
      return;

    case CONST:
      /* There could be a label reference in here.  */
      mark_loop_jump (XEXP (x, 0), loop);
      return;

    case PLUS:
    case MINUS:
    case MULT:
      mark_loop_jump (XEXP (x, 0), loop);
      mark_loop_jump (XEXP (x, 1), loop);
      return;

    case LO_SUM:
      /* This may refer to a LABEL_REF or SYMBOL_REF.  */
      mark_loop_jump (XEXP (x, 1), loop);
      return;

    case SIGN_EXTEND:
    case ZERO_EXTEND:
      mark_loop_jump (XEXP (x, 0), loop);
      return;

    case LABEL_REF:
      dest_loop = uid_loop[INSN_UID (XEXP (x, 0))];

      /* Link together all labels that branch outside the loop.  This
	 is used by final_[bg]iv_value and the loop unrolling code.  Also
	 mark this LABEL_REF so we know that this branch should predict
	 false.  */

      /* A check to make sure the label is not in an inner nested loop,
	 since this does not count as a loop exit.  */
      if (dest_loop)
	{
	  for (outer_loop = dest_loop; outer_loop;
	       outer_loop = outer_loop->outer)
	    if (outer_loop == loop)
	      break;
	}
      else
	outer_loop = NULL;

      if (loop && ! outer_loop)
	{
	  LABEL_OUTSIDE_LOOP_P (x) = 1;
	  LABEL_NEXTREF (x) = loop->exit_labels;
	  loop->exit_labels = x;

	  for (outer_loop = loop;
	       outer_loop && outer_loop != dest_loop;
	       outer_loop = outer_loop->outer)
	    outer_loop->exit_count++;
	}

      /* If this is inside a loop, but not in the current loop or one enclosed
	 by it, it invalidates at least one loop.  */

      if (! dest_loop)
	return;

      /* We must invalidate every nested loop containing the target of this
	 label, except those that also contain the jump insn.  */

      for (; dest_loop; dest_loop = dest_loop->outer)
	{
	  /* Stop when we reach a loop that also contains the jump insn.  */
	  for (outer_loop = loop; outer_loop; outer_loop = outer_loop->outer)
	    if (dest_loop == outer_loop)
	      return;

	  /* If we get here, we know we need to invalidate a loop.  */
	  if (loop_dump_stream && ! dest_loop->invalid)
	    fprintf (loop_dump_stream,
		     "\nLoop at %d ignored due to multiple entry points.\n",
		     INSN_UID (dest_loop->start));

	  dest_loop->invalid = 1;
	}
      return;

    case SET:
      /* If this is not setting pc, ignore.  */
      if (SET_DEST (x) == pc_rtx)
	mark_loop_jump (SET_SRC (x), loop);
      return;

    case IF_THEN_ELSE:
      mark_loop_jump (XEXP (x, 1), loop);
      mark_loop_jump (XEXP (x, 2), loop);
      return;

    case PARALLEL:
    case ADDR_VEC:
      for (i = 0; i < XVECLEN (x, 0); i++)
	mark_loop_jump (XVECEXP (x, 0, i), loop);
      return;

    case ADDR_DIFF_VEC:
      for (i = 0; i < XVECLEN (x, 1); i++)
	mark_loop_jump (XVECEXP (x, 1, i), loop);
      return;

    default:
      /* Strictly speaking this is not a jump into the loop, only a possible
	 jump out of the loop.  However, we have no way to link the destination
	 of this jump onto the list of exit labels.  To be safe we mark this
	 loop and any containing loops as invalid.  */
      if (loop)
	{
	  for (outer_loop = loop; outer_loop; outer_loop = outer_loop->outer)
	    {
	      if (loop_dump_stream && ! outer_loop->invalid)
		fprintf (loop_dump_stream,
			 "\nLoop at %d ignored due to unknown exit jump.\n",
			 INSN_UID (outer_loop->start));
	      outer_loop->invalid = 1;
	    }
	}
      return;
    }
}

/* Return nonzero if there is a label in the range from
   insn INSN to and including the insn whose luid is END
   INSN must have an assigned luid (i.e., it must not have
   been previously created by loop.c).  */

static int
labels_in_range_p (insn, end)
     rtx insn;
     int end;
{
  while (insn && INSN_LUID (insn) <= end)
    {
      if (GET_CODE (insn) == CODE_LABEL)
	return 1;
      insn = NEXT_INSN (insn);
    }

  return 0;
}

/* Record that a memory reference X is being set.  */

static void
note_addr_stored (x, y, data)
     rtx x;
     rtx y ATTRIBUTE_UNUSED;
     void *data ATTRIBUTE_UNUSED;
{
  struct loop_info *loop_info = data;

  if (x == 0 || GET_CODE (x) != MEM)
    return;

  /* Count number of memory writes.
     This affects heuristics in strength_reduce.  */
  loop_info->num_mem_sets++;

  /* BLKmode MEM means all memory is clobbered.  */
  if (GET_MODE (x) == BLKmode)
    {
      if (RTX_UNCHANGING_P (x))
	loop_info->unknown_constant_address_altered = 1;
      else
	loop_info->unknown_address_altered = 1;

      return;
    }

  loop_info->store_mems = gen_rtx_EXPR_LIST (VOIDmode, x,
					     loop_info->store_mems);
}

/* X is a value modified by an INSN that references a biv inside a loop
   exit test (ie, X is somehow related to the value of the biv).  If X
   is a pseudo that is used more than once, then the biv is (effectively)
   used more than once.  DATA is a pointer to a loop_regs structure.  */

static void
note_set_pseudo_multiple_uses (x, y, data)
     rtx x;
     rtx y ATTRIBUTE_UNUSED;
     void *data;
{
  struct loop_regs *regs = (struct loop_regs *) data;

  if (x == 0)
    return;

  while (GET_CODE (x) == STRICT_LOW_PART
	 || GET_CODE (x) == SIGN_EXTRACT
	 || GET_CODE (x) == ZERO_EXTRACT
	 || GET_CODE (x) == SUBREG)
    x = XEXP (x, 0);

  if (GET_CODE (x) != REG || REGNO (x) < FIRST_PSEUDO_REGISTER)
    return;

  /* If we do not have usage information, or if we know the register
     is used more than once, note that fact for check_dbra_loop.  */
  if (REGNO (x) >= max_reg_before_loop
      || ! regs->array[REGNO (x)].single_usage
      || regs->array[REGNO (x)].single_usage == const0_rtx)
    regs->multiple_uses = 1;
}

/* Return nonzero if the rtx X is invariant over the current loop.

   The value is 2 if we refer to something only conditionally invariant.

   A memory ref is invariant if it is not volatile and does not conflict
   with anything stored in `loop_info->store_mems'.  */

int
loop_invariant_p (loop, x)
     const struct loop *loop;
     rtx x;
{
  struct loop_info *loop_info = LOOP_INFO (loop);
  struct loop_regs *regs = LOOP_REGS (loop);
  int i;
  enum rtx_code code;
  const char *fmt;
  int conditional = 0;
  rtx mem_list_entry;

  if (x == 0)
    return 1;
  code = GET_CODE (x);
  switch (code)
    {
    case CONST_INT:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case CONST:
      return 1;

    case LABEL_REF:
      /* A LABEL_REF is normally invariant, however, if we are unrolling
	 loops, and this label is inside the loop, then it isn't invariant.
	 This is because each unrolled copy of the loop body will have
	 a copy of this label.  If this was invariant, then an insn loading
	 the address of this label into a register might get moved outside
	 the loop, and then each loop body would end up using the same label.

	 We don't know the loop bounds here though, so just fail for all
	 labels.  */
      if (flag_unroll_loops)
	return 0;
      else
	return 1;

    case PC:
    case CC0:
    case UNSPEC_VOLATILE:
      return 0;

    case REG:
      /* We used to check RTX_UNCHANGING_P (x) here, but that is invalid
	 since the reg might be set by initialization within the loop.  */

      if ((x == frame_pointer_rtx || x == hard_frame_pointer_rtx
	   || x == arg_pointer_rtx || x == pic_offset_table_rtx)
	  && ! current_function_has_nonlocal_goto)
	return 1;

      if (LOOP_INFO (loop)->has_call
	  && REGNO (x) < FIRST_PSEUDO_REGISTER && call_used_regs[REGNO (x)])
	return 0;

      if (regs->array[REGNO (x)].set_in_loop < 0)
	return 2;

      return regs->array[REGNO (x)].set_in_loop == 0;

    case MEM:
      /* Volatile memory references must be rejected.  Do this before
	 checking for read-only items, so that volatile read-only items
	 will be rejected also.  */
      if (MEM_VOLATILE_P (x))
	return 0;

      /* See if there is any dependence between a store and this load.  */
      mem_list_entry = loop_info->store_mems;
      while (mem_list_entry)
	{
	  if (true_dependence (XEXP (mem_list_entry, 0), VOIDmode,
			       x, rtx_varies_p))
	    return 0;

	  mem_list_entry = XEXP (mem_list_entry, 1);
	}

      /* It's not invalidated by a store in memory
	 but we must still verify the address is invariant.  */
      break;

    case ASM_OPERANDS:
      /* Don't mess with insns declared volatile.  */
      if (MEM_VOLATILE_P (x))
	return 0;
      break;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  int tem = loop_invariant_p (loop, XEXP (x, i));
	  if (tem == 0)
	    return 0;
	  if (tem == 2)
	    conditional = 1;
	}
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    {
	      int tem = loop_invariant_p (loop, XVECEXP (x, i, j));
	      if (tem == 0)
		return 0;
	      if (tem == 2)
		conditional = 1;
	    }

	}
    }

  return 1 + conditional;
}

/* Return nonzero if all the insns in the loop that set REG
   are INSN and the immediately following insns,
   and if each of those insns sets REG in an invariant way
   (not counting uses of REG in them).

   The value is 2 if some of these insns are only conditionally invariant.

   We assume that INSN itself is the first set of REG
   and that its source is invariant.  */

static int
consec_sets_invariant_p (loop, reg, n_sets, insn)
     const struct loop *loop;
     int n_sets;
     rtx reg, insn;
{
  struct loop_regs *regs = LOOP_REGS (loop);
  rtx p = insn;
  unsigned int regno = REGNO (reg);
  rtx temp;
  /* Number of sets we have to insist on finding after INSN.  */
  int count = n_sets - 1;
  int old = regs->array[regno].set_in_loop;
  int value = 0;
  int this;

  /* If N_SETS hit the limit, we can't rely on its value.  */
  if (n_sets == 127)
    return 0;

  regs->array[regno].set_in_loop = 0;

  while (count > 0)
    {
      enum rtx_code code;
      rtx set;

      p = NEXT_INSN (p);
      code = GET_CODE (p);

      /* If library call, skip to end of it.  */
      if (code == INSN && (temp = find_reg_note (p, REG_LIBCALL, NULL_RTX)))
	p = XEXP (temp, 0);

      this = 0;
      if (code == INSN
	  && (set = single_set (p))
	  && GET_CODE (SET_DEST (set)) == REG
	  && REGNO (SET_DEST (set)) == regno)
	{
	  this = loop_invariant_p (loop, SET_SRC (set));
	  if (this != 0)
	    value |= this;
	  else if ((temp = find_reg_note (p, REG_EQUAL, NULL_RTX)))
	    {
	      /* If this is a libcall, then any invariant REG_EQUAL note is OK.
		 If this is an ordinary insn, then only CONSTANT_P REG_EQUAL
		 notes are OK.  */
	      this = (CONSTANT_P (XEXP (temp, 0))
		      || (find_reg_note (p, REG_RETVAL, NULL_RTX)
			  && loop_invariant_p (loop, XEXP (temp, 0))));
	      if (this != 0)
		value |= this;
	    }
	}
      if (this != 0)
	count--;
      else if (code != NOTE)
	{
	  regs->array[regno].set_in_loop = old;
	  return 0;
	}
    }

  regs->array[regno].set_in_loop = old;
  /* If loop_invariant_p ever returned 2, we return 2.  */
  return 1 + (value & 2);
}

#if 0
/* I don't think this condition is sufficient to allow INSN
   to be moved, so we no longer test it.  */

/* Return 1 if all insns in the basic block of INSN and following INSN
   that set REG are invariant according to TABLE.  */

static int
all_sets_invariant_p (reg, insn, table)
     rtx reg, insn;
     short *table;
{
  rtx p = insn;
  int regno = REGNO (reg);

  while (1)
    {
      enum rtx_code code;
      p = NEXT_INSN (p);
      code = GET_CODE (p);
      if (code == CODE_LABEL || code == JUMP_INSN)
	return 1;
      if (code == INSN && GET_CODE (PATTERN (p)) == SET
	  && GET_CODE (SET_DEST (PATTERN (p))) == REG
	  && REGNO (SET_DEST (PATTERN (p))) == regno)
	{
	  if (! loop_invariant_p (loop, SET_SRC (PATTERN (p)), table))
	    return 0;
	}
    }
}
#endif /* 0 */

/* Look at all uses (not sets) of registers in X.  For each, if it is
   the single use, set USAGE[REGNO] to INSN; if there was a previous use in
   a different insn, set USAGE[REGNO] to const0_rtx.  */

static void
find_single_use_in_loop (regs, insn, x)
     struct loop_regs *regs;
     rtx insn;
     rtx x;
{
  enum rtx_code code = GET_CODE (x);
  const char *fmt = GET_RTX_FORMAT (code);
  int i, j;

  if (code == REG)
    regs->array[REGNO (x)].single_usage
      = (regs->array[REGNO (x)].single_usage != 0
	 && regs->array[REGNO (x)].single_usage != insn)
	? const0_rtx : insn;

  else if (code == SET)
    {
      /* Don't count SET_DEST if it is a REG; otherwise count things
	 in SET_DEST because if a register is partially modified, it won't
	 show up as a potential movable so we don't care how USAGE is set
	 for it.  */
      if (GET_CODE (SET_DEST (x)) != REG)
	find_single_use_in_loop (regs, insn, SET_DEST (x));
      find_single_use_in_loop (regs, insn, SET_SRC (x));
    }
  else
    for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
      {
	if (fmt[i] == 'e' && XEXP (x, i) != 0)
	  find_single_use_in_loop (regs, insn, XEXP (x, i));
	else if (fmt[i] == 'E')
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    find_single_use_in_loop (regs, insn, XVECEXP (x, i, j));
      }
}

/* Count and record any set in X which is contained in INSN.  Update
   REGS->array[I].MAY_NOT_OPTIMIZE and LAST_SET for any register I set
   in X.  */

static void
count_one_set (regs, insn, x, last_set)
     struct loop_regs *regs;
     rtx insn, x;
     rtx *last_set;
{
  if (GET_CODE (x) == CLOBBER && GET_CODE (XEXP (x, 0)) == REG)
    /* Don't move a reg that has an explicit clobber.
       It's not worth the pain to try to do it correctly.  */
    regs->array[REGNO (XEXP (x, 0))].may_not_optimize = 1;

  if (GET_CODE (x) == SET || GET_CODE (x) == CLOBBER)
    {
      rtx dest = SET_DEST (x);
      while (GET_CODE (dest) == SUBREG
	     || GET_CODE (dest) == ZERO_EXTRACT
	     || GET_CODE (dest) == SIGN_EXTRACT
	     || GET_CODE (dest) == STRICT_LOW_PART)
	dest = XEXP (dest, 0);
      if (GET_CODE (dest) == REG)
	{
	  int i;
	  int regno = REGNO (dest);
	  for (i = 0; i < LOOP_REGNO_NREGS (regno, dest); i++)
	    {
	      /* If this is the first setting of this reg
		 in current basic block, and it was set before,
		 it must be set in two basic blocks, so it cannot
		 be moved out of the loop.  */
	      if (regs->array[regno].set_in_loop > 0
		  && last_set == 0)
		regs->array[regno+i].may_not_optimize = 1;
	      /* If this is not first setting in current basic block,
		 see if reg was used in between previous one and this.
		 If so, neither one can be moved.  */
	      if (last_set[regno] != 0
		  && reg_used_between_p (dest, last_set[regno], insn))
		regs->array[regno+i].may_not_optimize = 1;
	      if (regs->array[regno+i].set_in_loop < 127)
		++regs->array[regno+i].set_in_loop;
	      last_set[regno+i] = insn;
	    }
	}
    }
}

/* Given a loop that is bounded by LOOP->START and LOOP->END and that
   is entered at LOOP->SCAN_START, return 1 if the register set in SET
   contained in insn INSN is used by any insn that precedes INSN in
   cyclic order starting from the loop entry point.

   We don't want to use INSN_LUID here because if we restrict INSN to those
   that have a valid INSN_LUID, it means we cannot move an invariant out
   from an inner loop past two loops.  */

static int
loop_reg_used_before_p (loop, set, insn)
     const struct loop *loop;
     rtx set, insn;
{
  rtx reg = SET_DEST (set);
  rtx p;

  /* Scan forward checking for register usage.  If we hit INSN, we
     are done.  Otherwise, if we hit LOOP->END, wrap around to LOOP->START.  */
  for (p = loop->scan_start; p != insn; p = NEXT_INSN (p))
    {
      if (INSN_P (p) && reg_overlap_mentioned_p (reg, PATTERN (p)))
	return 1;

      if (p == loop->end)
	p = loop->start;
    }

  return 0;
}


/* Information we collect about arrays that we might want to prefetch.  */
struct prefetch_info
{
  struct iv_class *class;	/* Class this prefetch is based on.  */
  struct induction *giv;	/* GIV this prefetch is based on.  */
  rtx base_address;		/* Start prefetching from this address plus
				   index.  */
  HOST_WIDE_INT index;
  HOST_WIDE_INT stride;		/* Prefetch stride in bytes in each
				   iteration.  */
  unsigned int bytes_accesed;	/* Sum of sizes of all acceses to this
				   prefetch area in one iteration.  */
  unsigned int total_bytes;	/* Total bytes loop will access in this block.
				   This is set only for loops with known
				   iteration counts and is 0xffffffff
				   otherwise.  */
  unsigned int write : 1;	/* 1 for read/write prefetches.  */
  unsigned int prefetch_in_loop : 1;
  				/* 1 for those chosen for prefetching.  */
  unsigned int prefetch_before_loop : 1;
  				/* 1 for those chosen for prefetching.  */
};

/* Data used by check_store function.  */
struct check_store_data
{
  rtx mem_address;
  int mem_write;
};

static void check_store PARAMS ((rtx, rtx, void *));
static void emit_prefetch_instructions PARAMS ((struct loop *));
static int rtx_equal_for_prefetch_p PARAMS ((rtx, rtx));

/* Set mem_write when mem_address is found.  Used as callback to
   note_stores.  */
static void
check_store (x, pat, data)
     rtx x, pat ATTRIBUTE_UNUSED;
     void *data;
{
  struct check_store_data *d = (struct check_store_data *) data;

  if ((GET_CODE (x) == MEM) && rtx_equal_p (d->mem_address, XEXP (x, 0)))
    d->mem_write = 1;
}

/* Like rtx_equal_p, but attempts to swap commutative operands.  This is
   important to get some addresses combined.  Later more sophisticated
   transformations can be added when necesary.

   ??? Same trick with swapping operand is done at several other places.
   It can be nice to develop some common way to handle this.  */

static int
rtx_equal_for_prefetch_p (x, y)
     rtx x, y;
{
  int i;
  int j;
  enum rtx_code code = GET_CODE (x);
  const char *fmt;

  if (x == y)
    return 1;
  if (code != GET_CODE (y))
    return 0;

  code = GET_CODE (x);

  if (GET_RTX_CLASS (code) == 'c')
    {
      return ((rtx_equal_for_prefetch_p (XEXP (x, 0), XEXP (y, 0))
	       && rtx_equal_for_prefetch_p (XEXP (x, 1), XEXP (y, 1)))
	      || (rtx_equal_for_prefetch_p (XEXP (x, 0), XEXP (y, 1))
	          && rtx_equal_for_prefetch_p (XEXP (x, 1), XEXP (y, 0))));
    }
  /* Compare the elements.  If any pair of corresponding elements fails to
     match, return 0 for the whole thing.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      switch (fmt[i])
	{
	case 'w':
	  if (XWINT (x, i) != XWINT (y, i))
	    return 0;
	  break;

	case 'i':
	  if (XINT (x, i) != XINT (y, i))
	    return 0;
	  break;

	case 'E':
	  /* Two vectors must have the same length.  */
	  if (XVECLEN (x, i) != XVECLEN (y, i))
	    return 0;

	  /* And the corresponding elements must match.  */
	  for (j = 0; j < XVECLEN (x, i); j++)
	    if (rtx_equal_for_prefetch_p (XVECEXP (x, i, j),
					  XVECEXP (y, i, j)) == 0)
	      return 0;
	  break;

	case 'e':
	  if (rtx_equal_for_prefetch_p (XEXP (x, i), XEXP (y, i)) == 0)
	    return 0;
	  break;

	case 's':
	  if (strcmp (XSTR (x, i), XSTR (y, i)))
	    return 0;
	  break;

	case 'u':
	  /* These are just backpointers, so they don't matter.  */
	  break;

	case '0':
	  break;

	  /* It is believed that rtx's at this level will never
	     contain anything but integers and other rtx's,
	     except for within LABEL_REFs and SYMBOL_REFs.  */
	default:
	  abort ();
	}
    }
  return 1;
}

/* Remove constant addition value from the expression X (when present)
   and return it.  */

static HOST_WIDE_INT
remove_constant_addition (x)
     rtx *x;
{
  HOST_WIDE_INT addval = 0;
  rtx exp = *x;

  /* Avoid clobbering a shared CONST expression.  */
  if (GET_CODE (exp) == CONST)
    {
      if (GET_CODE (XEXP (exp, 0)) == PLUS
	  && GET_CODE (XEXP (XEXP (exp, 0), 0)) == SYMBOL_REF
	  && GET_CODE (XEXP (XEXP (exp, 0), 1)) == CONST_INT)
	{
	  *x = XEXP (XEXP (exp, 0), 0);
	  return INTVAL (XEXP (XEXP (exp, 0), 1));
	}
      return 0;
    }

  if (GET_CODE (exp) == CONST_INT)
    {
      addval = INTVAL (exp);
      *x = const0_rtx;
    }

  /* For plus expression recurse on ourself.  */
  else if (GET_CODE (exp) == PLUS)
    {
      addval += remove_constant_addition (&XEXP (exp, 0));
      addval += remove_constant_addition (&XEXP (exp, 1));

      /* In case our parameter was constant, remove extra zero from the
	 expression.  */
      if (XEXP (exp, 0) == const0_rtx)
        *x = XEXP (exp, 1);
      else if (XEXP (exp, 1) == const0_rtx)
        *x = XEXP (exp, 0);
    }

  return addval;
}

/* Attempt to identify accesses to arrays that are most likely to cause cache
   misses, and emit prefetch instructions a few prefetch blocks forward.

   To detect the arrays we use the GIV information that was collected by the
   strength reduction pass.

   The prefetch instructions are generated after the GIV information is done
   and before the strength reduction process. The new GIVs are injected into
   the strength reduction tables, so the prefetch addresses are optimized as
   well.

   GIVs are split into base address, stride, and constant addition values.
   GIVs with the same address, stride and close addition values are combined
   into a single prefetch.  Also writes to GIVs are detected, so that prefetch
   for write instructions can be used for the block we write to, on machines
   that support write prefetches.

   Several heuristics are used to determine when to prefetch.  They are
   controlled by defined symbols that can be overridden for each target.  */

static void
emit_prefetch_instructions (loop)
     struct loop *loop;
{
  int num_prefetches = 0;
  int num_real_prefetches = 0;
  int num_real_write_prefetches = 0;
  int ahead;
  int i;
  struct iv_class *bl;
  struct induction *iv;
  struct prefetch_info info[MAX_PREFETCHES];
  struct loop_ivs *ivs = LOOP_IVS (loop);

  if (!HAVE_prefetch)
    return;

  /* Consider only loops w/o calls.  When a call is done, the loop is probably
     slow enough to read the memory.  */
  if (PREFETCH_NO_CALL && LOOP_INFO (loop)->has_call)
    {
      if (loop_dump_stream)
	fprintf (loop_dump_stream, "Prefetch: ignoring loop - has call.\n");

      return;
    }

  if (PREFETCH_NO_LOW_LOOPCNT
      && LOOP_INFO (loop)->n_iterations
      && LOOP_INFO (loop)->n_iterations <= PREFETCH_LOW_LOOPCNT)
    {
      if (loop_dump_stream)
	fprintf (loop_dump_stream,
		 "Prefetch: ignoring loop - not enought iterations.\n");
      return;
    }

  /* Search all induction variables and pick those interesting for the prefetch
     machinery.  */
  for (bl = ivs->list; bl; bl = bl->next)
    {
      struct induction *biv = bl->biv, *biv1;
      int basestride = 0;

      biv1 = biv;

      /* Expect all BIVs to be executed in each iteration.  This makes our
	 analysis more conservative.  */
      while (biv1)
	{
	  /* Discard non-constant additions that we can't handle well yet, and
	     BIVs that are executed multiple times; such BIVs ought to be
	     handled in the nested loop.  We accept not_every_iteration BIVs,
	     since these only result in larger strides and make our
	     heuristics more conservative.
	     ??? What does the last sentence mean?  */
	  if (GET_CODE (biv->add_val) != CONST_INT)
	    {
	      if (loop_dump_stream)
		{
		  fprintf (loop_dump_stream,
			   "Prefetch: biv %i ignored: non-constant addition at insn %i:",
			   REGNO (biv->src_reg), INSN_UID (biv->insn));
		  print_rtl (loop_dump_stream, biv->add_val);
		  fprintf (loop_dump_stream, "\n");
		}
	      break;
	    }

	  if (biv->maybe_multiple)
	    {
	      if (loop_dump_stream)
		{
		  fprintf (loop_dump_stream,
			   "Prefetch: biv %i ignored: maybe_multiple at insn %i:",
			   REGNO (biv->src_reg), INSN_UID (biv->insn));
		  print_rtl (loop_dump_stream, biv->add_val);
		  fprintf (loop_dump_stream, "\n");
		}
	      break;
	    }

	  basestride += INTVAL (biv1->add_val);
	  biv1 = biv1->next_iv;
	}

      if (biv1 || !basestride)
	continue;

      for (iv = bl->giv; iv; iv = iv->next_iv)
	{
	  rtx address;
	  rtx temp;
	  HOST_WIDE_INT index = 0;
	  int add = 1;
	  HOST_WIDE_INT stride;
	  struct check_store_data d;
	  int size = GET_MODE_SIZE (GET_MODE (iv));

	  /* There are several reasons why an induction variable is not
	     interesting to us.  */
	  if (iv->giv_type != DEST_ADDR
	      /* We are interested only in constant stride memory references
		 in order to be able to compute density easily.  */
	      || GET_CODE (iv->mult_val) != CONST_INT
	      /* Don't handle reversed order prefetches, since they are usually
		 ineffective.  Later we may be able to reverse such BIVs.  */
	      || (PREFETCH_NO_REVERSE_ORDER
		  && (stride = INTVAL (iv->mult_val) * basestride) < 0)
	      /* Prefetching of accesses with such an extreme stride is probably
		 not worthwhile, either.  */
	      || (PREFETCH_NO_EXTREME_STRIDE
		  && stride > PREFETCH_EXTREME_STRIDE)
	      /* Ignore GIVs with varying add values; we can't predict the
		 value for the next iteration.  */
	      || !loop_invariant_p (loop, iv->add_val)
	      /* Ignore GIVs in the nested loops; they ought to have been
		 handled already.  */
	      || iv->maybe_multiple)
	    {
	      if (loop_dump_stream)
		fprintf (loop_dump_stream, "Prefetch: Ignoring giv at %i\n",
			 INSN_UID (iv->insn));
	      continue;
	    }

	  /* Determine the pointer to the basic array we are examining.  It is
	     the sum of the BIV's initial value and the GIV's add_val.  */
	  index = 0;

	  address = copy_rtx (iv->add_val);
	  temp = copy_rtx (bl->initial_value);

	  address = simplify_gen_binary (PLUS, Pmode, temp, address);
	  index = remove_constant_addition (&address);

	  index += size;
	  d.mem_write = 0;
	  d.mem_address = *iv->location;

	  /* When the GIV is not always executed, we might be better off by
	     not dirtying the cache pages.  */
	  if (PREFETCH_NOT_ALWAYS || iv->always_executed)
	    note_stores (PATTERN (iv->insn), check_store, &d);

	  /* Attempt to find another prefetch to the same array and see if we
	     can merge this one.  */
	  for (i = 0; i < num_prefetches; i++)
	    if (rtx_equal_for_prefetch_p (address, info[i].base_address)
		&& stride == info[i].stride)
	      {
		/* In case both access same array (same location
		   just with small difference in constant indexes), merge
		   the prefetches.  Just do the later and the earlier will
		   get prefetched from previous iteration.
		   4096 is artificial threshold.  It should not be too small,
		   but also not bigger than small portion of memory usually
		   traversed by single loop.  */
		if (index >= info[i].index && index - info[i].index < 4096)
		  {
		    info[i].write |= d.mem_write;
		    info[i].bytes_accesed += size;
		    info[i].index = index;
		    info[i].giv = iv;
		    info[i].class = bl;
		    info[num_prefetches].base_address = address;
		    add = 0;
		    break;
		  }

		if (index < info[i].index && info[i].index - index < 4096)
		  {
		    info[i].write |= d.mem_write;
		    info[i].bytes_accesed += size;
		    add = 0;
		    break;
		  }
	      }

	  /* Merging failed.  */
	  if (add)
	    {
	      info[num_prefetches].giv = iv;
	      info[num_prefetches].class = bl;
	      info[num_prefetches].index = index;
	      info[num_prefetches].stride = stride;
	      info[num_prefetches].base_address = address;
	      info[num_prefetches].write = d.mem_write;
	      info[num_prefetches].bytes_accesed = size;
	      num_prefetches++;
	      if (num_prefetches >= MAX_PREFETCHES)
		{
		  if (loop_dump_stream)
		    fprintf (loop_dump_stream,
			     "Maximal number of prefetches exceeded.\n");
		  return;
		}
	    }
	}
    }

  for (i = 0; i < num_prefetches; i++)
    {
      /* Attempt to calculate the number of bytes fetched by the loop.
	 Avoid overflow.  */
      if (LOOP_INFO (loop)->n_iterations
          && ((unsigned HOST_WIDE_INT) (0xffffffff / info[i].stride)
	      >= LOOP_INFO (loop)->n_iterations))
	info[i].total_bytes = info[i].stride * LOOP_INFO (loop)->n_iterations;
      else
	info[i].total_bytes = 0xffffffff;

      /* Prefetch is worthwhile only when the loads/stores are dense.  */
      if (PREFETCH_ONLY_DENSE_MEM
	  && info[i].bytes_accesed * 256 / info[i].stride > PREFETCH_DENSE_MEM
	  && (info[i].total_bytes / PREFETCH_BLOCK
	      >= PREFETCH_BLOCKS_BEFORE_LOOP_MIN))
	{
	  info[i].prefetch_before_loop = 1;
	  info[i].prefetch_in_loop
	    = (info[i].total_bytes / PREFETCH_BLOCK
	       > PREFETCH_BLOCKS_BEFORE_LOOP_MAX);
	}
      else
        info[i].prefetch_in_loop = 0, info[i].prefetch_before_loop = 0;

      if (info[i].prefetch_in_loop)
	{
	  num_real_prefetches += ((info[i].stride + PREFETCH_BLOCK - 1)
				  / PREFETCH_BLOCK);
	  if (info[i].write)
	    num_real_write_prefetches
	      += (info[i].stride + PREFETCH_BLOCK - 1) / PREFETCH_BLOCK;
	}
    }

  if (loop_dump_stream)
    {
      for (i = 0; i < num_prefetches; i++)
	{
	  fprintf (loop_dump_stream, "Prefetch insn %i address: ",
		   INSN_UID (info[i].giv->insn));
	  print_rtl (loop_dump_stream, info[i].base_address);
	  fprintf (loop_dump_stream, " Index: ");
	  fprintf (loop_dump_stream, HOST_WIDE_INT_PRINT_DEC, info[i].index);
	  fprintf (loop_dump_stream, " stride: ");
	  fprintf (loop_dump_stream, HOST_WIDE_INT_PRINT_DEC, info[i].stride);
	  fprintf (loop_dump_stream,
		   " density: %i%% total_bytes: %u%sin loop: %s before: %s\n",
		   (int) (info[i].bytes_accesed * 100 / info[i].stride),
		   info[i].total_bytes,
		   info[i].write ? " read/write " : " read only ",
		   info[i].prefetch_in_loop ? "yes" : "no",
		   info[i].prefetch_before_loop ? "yes" : "no");
	}

      fprintf (loop_dump_stream, "Real prefetches needed: %i (write: %i)\n",
	       num_real_prefetches, num_real_write_prefetches);
    }

  if (!num_real_prefetches)
    return;

  ahead = SIMULTANEOUS_PREFETCHES / num_real_prefetches;

  if (!ahead)
    return;

  for (i = 0; i < num_prefetches; i++)
    {
      if (info[i].prefetch_in_loop)
	{
	  int y;

	  for (y = 0; y < ((info[i].stride + PREFETCH_BLOCK - 1)
			   / PREFETCH_BLOCK); y++)
	    {
	      rtx loc = copy_rtx (*info[i].giv->location);
	      rtx insn;
	      int bytes_ahead = PREFETCH_BLOCK * (ahead + y);
	      rtx before_insn = info[i].giv->insn;
	      rtx prev_insn = PREV_INSN (info[i].giv->insn);
	      rtx seq;

	      /* We can save some effort by offsetting the address on
		 architectures with offsettable memory references.  */
	      if (offsettable_address_p (0, VOIDmode, loc))
		loc = plus_constant (loc, bytes_ahead);
	      else
		{
		  rtx reg = gen_reg_rtx (Pmode);
		  loop_iv_add_mult_emit_before (loop, loc, const1_rtx,
		      				GEN_INT (bytes_ahead), reg,
				  		0, before_insn);
		  loc = reg;
		}

	      start_sequence ();
	      /* Make sure the address operand is valid for prefetch.  */
	      if (! (*insn_data[(int)CODE_FOR_prefetch].operand[0].predicate)
		    (loc,
		     insn_data[(int)CODE_FOR_prefetch].operand[0].mode))
		loc = force_reg (Pmode, loc);
	      emit_insn (gen_prefetch (loc, GEN_INT (info[i].write),
		                       GEN_INT (3)));
	      seq = gen_sequence ();
	      end_sequence ();
	      emit_insn_before (seq, before_insn);

	      /* Check all insns emitted and record the new GIV
		 information.  */
	      insn = NEXT_INSN (prev_insn);
	      while (insn != before_insn)
		{
		  insn = check_insn_for_givs (loop, insn,
					      info[i].giv->always_executed,
					      info[i].giv->maybe_multiple);
		  insn = NEXT_INSN (insn);
		}
	    }
	}

      if (info[i].prefetch_before_loop)
	{
	  int y;

	  /* Emit INSNs before the loop to fetch the first cache lines.  */
	  for (y = 0;
	       (!info[i].prefetch_in_loop || y < ahead)
	       && y * PREFETCH_BLOCK < (int) info[i].total_bytes; y ++)
	    {
	      rtx reg = gen_reg_rtx (Pmode);
	      rtx loop_start = loop->start;
	      rtx init_val = info[i].class->initial_value;
	      rtx add_val = simplify_gen_binary (PLUS, Pmode,
						 info[i].giv->add_val,
						 GEN_INT (y * PREFETCH_BLOCK));

	      /* Functions called by LOOP_IV_ADD_EMIT_BEFORE expect a
		 non-constant INIT_VAL to have the same mode as REG, which
		 in this case we know to be Pmode.  */
	      if (GET_MODE (init_val) != Pmode && !CONSTANT_P (init_val))
		init_val = convert_to_mode (Pmode, init_val, 0);
	      loop_iv_add_mult_emit_before (loop, init_val,
					    info[i].giv->mult_val,
				            add_val, reg, 0, loop_start);
	      emit_insn_before (gen_prefetch (reg, GEN_INT (info[i].write),
					      GEN_INT (3)),
				loop_start);
	    }
	}
    }

  return;
}

/* A "basic induction variable" or biv is a pseudo reg that is set
   (within this loop) only by incrementing or decrementing it.  */
/* A "general induction variable" or giv is a pseudo reg whose
   value is a linear function of a biv.  */

/* Bivs are recognized by `basic_induction_var';
   Givs by `general_induction_var'.  */

/* Communication with routines called via `note_stores'.  */

static rtx note_insn;

/* Dummy register to have non-zero DEST_REG for DEST_ADDR type givs.  */

static rtx addr_placeholder;

/* ??? Unfinished optimizations, and possible future optimizations,
   for the strength reduction code.  */

/* ??? The interaction of biv elimination, and recognition of 'constant'
   bivs, may cause problems.  */

/* ??? Add heuristics so that DEST_ADDR strength reduction does not cause
   performance problems.

   Perhaps don't eliminate things that can be combined with an addressing
   mode.  Find all givs that have the same biv, mult_val, and add_val;
   then for each giv, check to see if its only use dies in a following
   memory address.  If so, generate a new memory address and check to see
   if it is valid.   If it is valid, then store the modified memory address,
   otherwise, mark the giv as not done so that it will get its own iv.  */

/* ??? Could try to optimize branches when it is known that a biv is always
   positive.  */

/* ??? When replace a biv in a compare insn, we should replace with closest
   giv so that an optimized branch can still be recognized by the combiner,
   e.g. the VAX acb insn.  */

/* ??? Many of the checks involving uid_luid could be simplified if regscan
   was rerun in loop_optimize whenever a register was added or moved.
   Also, some of the optimizations could be a little less conservative.  */

/* Scan the loop body and call FNCALL for each insn.  In the addition to the
   LOOP and INSN parameters pass MAYBE_MULTIPLE and NOT_EVERY_ITERATION to the
   callback.

   NOT_EVERY_ITERATION if current insn is not executed at least once for every
   loop iteration except for the last one.

   MAYBE_MULTIPLE is 1 if current insn may be executed more than once for every
   loop iteration.
 */
void
for_each_insn_in_loop (loop, fncall)
     struct loop *loop;
     loop_insn_callback fncall;
{
  /* This is 1 if current insn is not executed at least once for every loop
     iteration.  */
  int not_every_iteration = 0;
  int maybe_multiple = 0;
  int past_loop_latch = 0;
  int loop_depth = 0;
  rtx p;

  /* If loop_scan_start points to the loop exit test, we have to be wary of
     subversive use of gotos inside expression statements.  */
  if (prev_nonnote_insn (loop->scan_start) != prev_nonnote_insn (loop->start))
    maybe_multiple = back_branch_in_range_p (loop, loop->scan_start);

  /* Scan through loop to find all possible bivs.  */

  for (p = next_insn_in_loop (loop, loop->scan_start);
       p != NULL_RTX;
       p = next_insn_in_loop (loop, p))
    {
      p = fncall (loop, p, not_every_iteration, maybe_multiple);

      /* Past CODE_LABEL, we get to insns that may be executed multiple
         times.  The only way we can be sure that they can't is if every
         jump insn between here and the end of the loop either
         returns, exits the loop, is a jump to a location that is still
         behind the label, or is a jump to the loop start.  */

      if (GET_CODE (p) == CODE_LABEL)
	{
	  rtx insn = p;

	  maybe_multiple = 0;

	  while (1)
	    {
	      insn = NEXT_INSN (insn);
	      if (insn == loop->scan_start)
		break;
	      if (insn == loop->end)
		{
		  if (loop->top != 0)
		    insn = loop->top;
		  else
		    break;
		  if (insn == loop->scan_start)
		    break;
		}

	      if (GET_CODE (insn) == JUMP_INSN
		  && GET_CODE (PATTERN (insn)) != RETURN
		  && (!any_condjump_p (insn)
		      || (JUMP_LABEL (insn) != 0
			  && JUMP_LABEL (insn) != loop->scan_start
			  && !loop_insn_first_p (p, JUMP_LABEL (insn)))))
		{
		  maybe_multiple = 1;
		  break;
		}
	    }
	}

      /* Past a jump, we get to insns for which we can't count
         on whether they will be executed during each iteration.  */
      /* This code appears twice in strength_reduce.  There is also similar
         code in scan_loop.  */
      if (GET_CODE (p) == JUMP_INSN
      /* If we enter the loop in the middle, and scan around to the
         beginning, don't set not_every_iteration for that.
         This can be any kind of jump, since we want to know if insns
         will be executed if the loop is executed.  */
	  && !(JUMP_LABEL (p) == loop->top
	     && ((NEXT_INSN (NEXT_INSN (p)) == loop->end
		  && any_uncondjump_p (p))
		 || (NEXT_INSN (p) == loop->end && any_condjump_p (p)))))
	{
	  rtx label = 0;

	  /* If this is a jump outside the loop, then it also doesn't
	     matter.  Check to see if the target of this branch is on the
	     loop->exits_labels list.  */

	  for (label = loop->exit_labels; label; label = LABEL_NEXTREF (label))
	    if (XEXP (label, 0) == JUMP_LABEL (p))
	      break;

	  if (!label)
	    not_every_iteration = 1;
	}

      else if (GET_CODE (p) == NOTE)
	{
	  /* At the virtual top of a converted loop, insns are again known to
	     be executed each iteration: logically, the loop begins here
	     even though the exit code has been duplicated.

	     Insns are also again known to be executed each iteration at
	     the LOOP_CONT note.  */
	  if ((NOTE_LINE_NUMBER (p) == NOTE_INSN_LOOP_VTOP
	       || NOTE_LINE_NUMBER (p) == NOTE_INSN_LOOP_CONT)
	      && loop_depth == 0)
	    not_every_iteration = 0;
	  else if (NOTE_LINE_NUMBER (p) == NOTE_INSN_LOOP_BEG)
	    loop_depth++;
	  else if (NOTE_LINE_NUMBER (p) == NOTE_INSN_LOOP_END)
	    loop_depth--;
	}

      /* Note if we pass a loop latch.  If we do, then we can not clear
         NOT_EVERY_ITERATION below when we pass the last CODE_LABEL in
         a loop since a jump before the last CODE_LABEL may have started
         a new loop iteration.

         Note that LOOP_TOP is only set for rotated loops and we need
         this check for all loops, so compare against the CODE_LABEL
         which immediately follows LOOP_START.  */
      if (GET_CODE (p) == JUMP_INSN
	  && JUMP_LABEL (p) == NEXT_INSN (loop->start))
	past_loop_latch = 1;

      /* Unlike in the code motion pass where MAYBE_NEVER indicates that
         an insn may never be executed, NOT_EVERY_ITERATION indicates whether
         or not an insn is known to be executed each iteration of the
         loop, whether or not any iterations are known to occur.

         Therefore, if we have just passed a label and have no more labels
         between here and the test insn of the loop, and we have not passed
         a jump to the top of the loop, then we know these insns will be
         executed each iteration.  */

      if (not_every_iteration
	  && !past_loop_latch
	  && GET_CODE (p) == CODE_LABEL
	  && no_labels_between_p (p, loop->end)
	  && loop_insn_first_p (p, loop->cont))
	not_every_iteration = 0;
    }
}

static void
loop_bivs_find (loop)
     struct loop *loop;
{
  struct loop_regs *regs = LOOP_REGS (loop);
  struct loop_ivs *ivs = LOOP_IVS (loop);
  /* Temporary list pointers for traversing ivs->list.  */
  struct iv_class *bl, **backbl;

  ivs->list = 0;

  for_each_insn_in_loop (loop, check_insn_for_bivs);

  /* Scan ivs->list to remove all regs that proved not to be bivs.
     Make a sanity check against regs->n_times_set.  */
  for (backbl = &ivs->list, bl = *backbl; bl; bl = bl->next)
    {
      if (REG_IV_TYPE (ivs, bl->regno) != BASIC_INDUCT
	  /* Above happens if register modified by subreg, etc.  */
	  /* Make sure it is not recognized as a basic induction var: */
	  || regs->array[bl->regno].n_times_set != bl->biv_count
	  /* If never incremented, it is invariant that we decided not to
	     move.  So leave it alone.  */
	  || ! bl->incremented)
	{
	  if (loop_dump_stream)
	    fprintf (loop_dump_stream, "Biv %d: discarded, %s\n",
		     bl->regno,
		     (REG_IV_TYPE (ivs, bl->regno) != BASIC_INDUCT
		      ? "not induction variable"
		      : (! bl->incremented ? "never incremented"
			 : "count error")));

	  REG_IV_TYPE (ivs, bl->regno) = NOT_BASIC_INDUCT;
	  *backbl = bl->next;
	}
      else
	{
	  backbl = &bl->next;

	  if (loop_dump_stream)
	    fprintf (loop_dump_stream, "Biv %d: verified\n", bl->regno);
	}
    }
}


/* Determine how BIVS are initialised by looking through pre-header
   extended basic block.  */
static void
loop_bivs_init_find (loop)
     struct loop *loop;
{
  struct loop_ivs *ivs = LOOP_IVS (loop);
  /* Temporary list pointers for traversing ivs->list.  */
  struct iv_class *bl;
  int call_seen;
  rtx p;

  /* Find initial value for each biv by searching backwards from loop_start,
     halting at first label.  Also record any test condition.  */

  call_seen = 0;
  for (p = loop->start; p && GET_CODE (p) != CODE_LABEL; p = PREV_INSN (p))
    {
      rtx test;

      note_insn = p;

      if (GET_CODE (p) == CALL_INSN)
	call_seen = 1;

      if (INSN_P (p))
	note_stores (PATTERN (p), record_initial, ivs);

      /* Record any test of a biv that branches around the loop if no store
	 between it and the start of loop.  We only care about tests with
	 constants and registers and only certain of those.  */
      if (GET_CODE (p) == JUMP_INSN
	  && JUMP_LABEL (p) != 0
	  && next_real_insn (JUMP_LABEL (p)) == next_real_insn (loop->end)
	  && (test = get_condition_for_loop (loop, p)) != 0
	  && GET_CODE (XEXP (test, 0)) == REG
	  && REGNO (XEXP (test, 0)) < max_reg_before_loop
	  && (bl = REG_IV_CLASS (ivs, REGNO (XEXP (test, 0)))) != 0
	  && valid_initial_value_p (XEXP (test, 1), p, call_seen, loop->start)
	  && bl->init_insn == 0)
	{
	  /* If an NE test, we have an initial value!  */
	  if (GET_CODE (test) == NE)
	    {
	      bl->init_insn = p;
	      bl->init_set = gen_rtx_SET (VOIDmode,
					  XEXP (test, 0), XEXP (test, 1));
	    }
	  else
	    bl->initial_test = test;
	}
    }
}


/* Look at the each biv and see if we can say anything better about its
   initial value from any initializing insns set up above.  (This is done
   in two passes to avoid missing SETs in a PARALLEL.)  */
static void
loop_bivs_check (loop)
     struct loop *loop;
{
  struct loop_ivs *ivs = LOOP_IVS (loop);
  /* Temporary list pointers for traversing ivs->list.  */
  struct iv_class *bl;
  struct iv_class **backbl;

  for (backbl = &ivs->list; (bl = *backbl); backbl = &bl->next)
    {
      rtx src;
      rtx note;

      if (! bl->init_insn)
	continue;

      /* IF INIT_INSN has a REG_EQUAL or REG_EQUIV note and the value
	 is a constant, use the value of that.  */
      if (((note = find_reg_note (bl->init_insn, REG_EQUAL, 0)) != NULL
	   && CONSTANT_P (XEXP (note, 0)))
	  || ((note = find_reg_note (bl->init_insn, REG_EQUIV, 0)) != NULL
	      && CONSTANT_P (XEXP (note, 0))))
	src = XEXP (note, 0);
      else
	src = SET_SRC (bl->init_set);

      if (loop_dump_stream)
	fprintf (loop_dump_stream,
		 "Biv %d: initialized at insn %d: initial value ",
		 bl->regno, INSN_UID (bl->init_insn));

      if ((GET_MODE (src) == GET_MODE (regno_reg_rtx[bl->regno])
	   || GET_MODE (src) == VOIDmode)
	  && valid_initial_value_p (src, bl->init_insn,
				    LOOP_INFO (loop)->pre_header_has_call,
				    loop->start))
	{
	  bl->initial_value = src;

	  if (loop_dump_stream)
	    {
	      print_simple_rtl (loop_dump_stream, src);
	      fputc ('\n', loop_dump_stream);
	    }
	}
      /* If we can't make it a giv,
	 let biv keep initial value of "itself".  */
      else if (loop_dump_stream)
	fprintf (loop_dump_stream, "is complex\n");
    }
}


/* Search the loop for general induction variables.  */

static void
loop_givs_find (loop)
     struct loop* loop;
{
  for_each_insn_in_loop (loop, check_insn_for_givs);
}


/* For each giv for which we still don't know whether or not it is
   replaceable, check to see if it is replaceable because its final value
   can be calculated.  */

static void
loop_givs_check (loop)
     struct loop *loop;
{
  struct loop_ivs *ivs = LOOP_IVS (loop);
  struct iv_class *bl;

  for (bl = ivs->list; bl; bl = bl->next)
    {
      struct induction *v;

      for (v = bl->giv; v; v = v->next_iv)
	if (! v->replaceable && ! v->not_replaceable)
	  check_final_value (loop, v);
    }
}


/* Return non-zero if it is possible to eliminate the biv BL provided
   all givs are reduced.  This is possible if either the reg is not
   used outside the loop, or we can compute what its final value will
   be.  */

static int
loop_biv_eliminable_p (loop, bl, threshold, insn_count)
     struct loop *loop;
     struct iv_class *bl;
     int threshold;
     int insn_count;
{
  /* For architectures with a decrement_and_branch_until_zero insn,
     don't do this if we put a REG_NONNEG note on the endtest for this
     biv.  */

#ifdef HAVE_decrement_and_branch_until_zero
  if (bl->nonneg)
    {
      if (loop_dump_stream)
	fprintf (loop_dump_stream,
		 "Cannot eliminate nonneg biv %d.\n", bl->regno);
      return 0;
    }
#endif

  /* Check that biv is used outside loop or if it has a final value.
     Compare against bl->init_insn rather than loop->start.  We aren't
     concerned with any uses of the biv between init_insn and
     loop->start since these won't be affected by the value of the biv
     elsewhere in the function, so long as init_insn doesn't use the
     biv itself.  */

  if ((REGNO_LAST_LUID (bl->regno) < INSN_LUID (loop->end)
       && bl->init_insn
       && INSN_UID (bl->init_insn) < max_uid_for_loop
       && REGNO_FIRST_LUID (bl->regno) >= INSN_LUID (bl->init_insn)
       && ! reg_mentioned_p (bl->biv->dest_reg, SET_SRC (bl->init_set)))
      || (bl->final_value = final_biv_value (loop, bl)))
    return maybe_eliminate_biv (loop, bl, 0, threshold,	insn_count);

  if (loop_dump_stream)
    {
      fprintf (loop_dump_stream,
	       "Cannot eliminate biv %d.\n",
	       bl->regno);
      fprintf (loop_dump_stream,
	       "First use: insn %d, last use: insn %d.\n",
	       REGNO_FIRST_UID (bl->regno),
	       REGNO_LAST_UID (bl->regno));
    }
  return 0;
}


/* Reduce each giv of BL that we have decided to reduce.  */

static void
loop_givs_reduce (loop, bl)
     struct loop *loop;
     struct iv_class *bl;
{
  struct induction *v;

  for (v = bl->giv; v; v = v->next_iv)
    {
      struct induction *tv;
      if (! v->ignore && v->same == 0)
	{
	  int auto_inc_opt = 0;

	  /* If the code for derived givs immediately below has already
	     allocated a new_reg, we must keep it.  */
	  if (! v->new_reg)
	    v->new_reg = gen_reg_rtx (v->mode);

#ifdef AUTO_INC_DEC
	  /* If the target has auto-increment addressing modes, and
	     this is an address giv, then try to put the increment
	     immediately after its use, so that flow can create an
	     auto-increment addressing mode.  */
	  if (v->giv_type == DEST_ADDR && bl->biv_count == 1
	      && bl->biv->always_executed && ! bl->biv->maybe_multiple
	      /* We don't handle reversed biv's because bl->biv->insn
		 does not have a valid INSN_LUID.  */
	      && ! bl->reversed
	      && v->always_executed && ! v->maybe_multiple
	      && INSN_UID (v->insn) < max_uid_for_loop)
	    {
	      /* If other giv's have been combined with this one, then
		 this will work only if all uses of the other giv's occur
		 before this giv's insn.  This is difficult to check.

		 We simplify this by looking for the common case where
		 there is one DEST_REG giv, and this giv's insn is the
		 last use of the dest_reg of that DEST_REG giv.  If the
		 increment occurs after the address giv, then we can
		 perform the optimization.  (Otherwise, the increment
		 would have to go before other_giv, and we would not be
		 able to combine it with the address giv to get an
		 auto-inc address.)  */
	      if (v->combined_with)
		{
		  struct induction *other_giv = 0;

		  for (tv = bl->giv; tv; tv = tv->next_iv)
		    if (tv->same == v)
		      {
			if (other_giv)
			  break;
			else
			  other_giv = tv;
		      }
		  if (! tv && other_giv
		      && REGNO (other_giv->dest_reg) < max_reg_before_loop
		      && (REGNO_LAST_UID (REGNO (other_giv->dest_reg))
			  == INSN_UID (v->insn))
		      && INSN_LUID (v->insn) < INSN_LUID (bl->biv->insn))
		    auto_inc_opt = 1;
		}
	      /* Check for case where increment is before the address
		 giv.  Do this test in "loop order".  */
	      else if ((INSN_LUID (v->insn) > INSN_LUID (bl->biv->insn)
			&& (INSN_LUID (v->insn) < INSN_LUID (loop->scan_start)
			    || (INSN_LUID (bl->biv->insn)
				> INSN_LUID (loop->scan_start))))
		       || (INSN_LUID (v->insn) < INSN_LUID (loop->scan_start)
			   && (INSN_LUID (loop->scan_start)
			       < INSN_LUID (bl->biv->insn))))
		auto_inc_opt = -1;
	      else
		auto_inc_opt = 1;

#ifdef HAVE_cc0
	      {
		rtx prev;

		/* We can't put an insn immediately after one setting
		   cc0, or immediately before one using cc0.  */
		if ((auto_inc_opt == 1 && sets_cc0_p (PATTERN (v->insn)))
		    || (auto_inc_opt == -1
			&& (prev = prev_nonnote_insn (v->insn)) != 0
			&& INSN_P (prev)
			&& sets_cc0_p (PATTERN (prev))))
		  auto_inc_opt = 0;
	      }
#endif

	      if (auto_inc_opt)
		v->auto_inc_opt = 1;
	    }
#endif

	  /* For each place where the biv is incremented, add an insn
	     to increment the new, reduced reg for the giv.  */
	  for (tv = bl->biv; tv; tv = tv->next_iv)
	    {
	      rtx insert_before;

	      if (! auto_inc_opt)
		insert_before = tv->insn;
	      else if (auto_inc_opt == 1)
		insert_before = NEXT_INSN (v->insn);
	      else
		insert_before = v->insn;

	      if (tv->mult_val == const1_rtx)
		loop_iv_add_mult_emit_before (loop, tv->add_val, v->mult_val,
					      v->new_reg, v->new_reg,
					      0, insert_before);
	      else /* tv->mult_val == const0_rtx */
		/* A multiply is acceptable here
		   since this is presumed to be seldom executed.  */
		loop_iv_add_mult_emit_before (loop, tv->add_val, v->mult_val,
					      v->add_val, v->new_reg,
					      0, insert_before);
	    }

	  /* Add code at loop start to initialize giv's reduced reg.  */

	  loop_iv_add_mult_hoist (loop,
				  extend_value_for_giv (v, bl->initial_value),
				  v->mult_val, v->add_val, v->new_reg);
	}
    }
}


/* Check for givs whose first use is their definition and whose
   last use is the definition of another giv.  If so, it is likely
   dead and should not be used to derive another giv nor to
   eliminate a biv.  */

static void
loop_givs_dead_check (loop, bl)
     struct loop *loop ATTRIBUTE_UNUSED;
     struct iv_class *bl;
{
  struct induction *v;

  for (v = bl->giv; v; v = v->next_iv)
    {
      if (v->ignore
	  || (v->same && v->same->ignore))
	continue;

      if (v->giv_type == DEST_REG
	  && REGNO_FIRST_UID (REGNO (v->dest_reg)) == INSN_UID (v->insn))
	{
	  struct induction *v1;

	  for (v1 = bl->giv; v1; v1 = v1->next_iv)
	    if (REGNO_LAST_UID (REGNO (v->dest_reg)) == INSN_UID (v1->insn))
	      v->maybe_dead = 1;
	}
    }
}


static void
loop_givs_rescan (loop, bl, reg_map)
     struct loop *loop;
     struct iv_class *bl;
     rtx *reg_map;
{
  struct induction *v;

  for (v = bl->giv; v; v = v->next_iv)
    {
      if (v->same && v->same->ignore)
	v->ignore = 1;

      if (v->ignore)
	continue;

      /* Update expression if this was combined, in case other giv was
	 replaced.  */
      if (v->same)
	v->new_reg = replace_rtx (v->new_reg,
				  v->same->dest_reg, v->same->new_reg);

      /* See if this register is known to be a pointer to something.  If
	 so, see if we can find the alignment.  First see if there is a
	 destination register that is a pointer.  If so, this shares the
	 alignment too.  Next see if we can deduce anything from the
	 computational information.  If not, and this is a DEST_ADDR
	 giv, at least we know that it's a pointer, though we don't know
	 the alignment.  */
      if (GET_CODE (v->new_reg) == REG
	  && v->giv_type == DEST_REG
	  && REG_POINTER (v->dest_reg))
	mark_reg_pointer (v->new_reg,
			  REGNO_POINTER_ALIGN (REGNO (v->dest_reg)));
      else if (GET_CODE (v->new_reg) == REG
	       && REG_POINTER (v->src_reg))
	{
	  unsigned int align = REGNO_POINTER_ALIGN (REGNO (v->src_reg));

	  if (align == 0
	      || GET_CODE (v->add_val) != CONST_INT
	      || INTVAL (v->add_val) % (align / BITS_PER_UNIT) != 0)
	    align = 0;

	  mark_reg_pointer (v->new_reg, align);
	}
      else if (GET_CODE (v->new_reg) == REG
	       && GET_CODE (v->add_val) == REG
	       && REG_POINTER (v->add_val))
	{
	  unsigned int align = REGNO_POINTER_ALIGN (REGNO (v->add_val));

	  if (align == 0 || GET_CODE (v->mult_val) != CONST_INT
	      || INTVAL (v->mult_val) % (align / BITS_PER_UNIT) != 0)
	    align = 0;

	  mark_reg_pointer (v->new_reg, align);
	}
      else if (GET_CODE (v->new_reg) == REG && v->giv_type == DEST_ADDR)
	mark_reg_pointer (v->new_reg, 0);

      if (v->giv_type == DEST_ADDR)
	/* Store reduced reg as the address in the memref where we found
	   this giv.  */
	validate_change (v->insn, v->location, v->new_reg, 0);
      else if (v->replaceable)
	{
	  reg_map[REGNO (v->dest_reg)] = v->new_reg;
	}
      else
	{
	  /* Not replaceable; emit an insn to set the original giv reg from
	     the reduced giv, same as above.  */
	  loop_insn_emit_after (loop, 0, v->insn,
				gen_move_insn (v->dest_reg, v->new_reg));
	}

      /* When a loop is reversed, givs which depend on the reversed
	 biv, and which are live outside the loop, must be set to their
	 correct final value.  This insn is only needed if the giv is
	 not replaceable.  The correct final value is the same as the
	 value that the giv starts the reversed loop with.  */
      if (bl->reversed && ! v->replaceable)
	loop_iv_add_mult_sink (loop,
			       extend_value_for_giv (v, bl->initial_value),
			       v->mult_val, v->add_val, v->dest_reg);
      else if (v->final_value)
	loop_insn_sink_or_swim (loop,
				gen_move_insn (v->dest_reg, v->final_value));

      if (loop_dump_stream)
	{
	  fprintf (loop_dump_stream, "giv at %d reduced to ",
		   INSN_UID (v->insn));
	  print_simple_rtl (loop_dump_stream, v->new_reg);
	  fprintf (loop_dump_stream, "\n");
	}
    }
}


static int
loop_giv_reduce_benefit (loop, bl, v, test_reg)
     struct loop *loop ATTRIBUTE_UNUSED;
     struct iv_class *bl;
     struct induction *v;
     rtx test_reg;
{
  int add_cost;
  int benefit;

  benefit = v->benefit;
  PUT_MODE (test_reg, v->mode);
  add_cost = iv_add_mult_cost (bl->biv->add_val, v->mult_val,
			       test_reg, test_reg);

  /* Reduce benefit if not replaceable, since we will insert a
     move-insn to replace the insn that calculates this giv.  Don't do
     this unless the giv is a user variable, since it will often be
     marked non-replaceable because of the duplication of the exit
     code outside the loop.  In such a case, the copies we insert are
     dead and will be deleted.  So they don't have a cost.  Similar
     situations exist.  */
  /* ??? The new final_[bg]iv_value code does a much better job of
     finding replaceable giv's, and hence this code may no longer be
     necessary.  */
  if (! v->replaceable && ! bl->eliminable
      && REG_USERVAR_P (v->dest_reg))
    benefit -= copy_cost;

  /* Decrease the benefit to count the add-insns that we will insert
     to increment the reduced reg for the giv.  ??? This can
     overestimate the run-time cost of the additional insns, e.g. if
     there are multiple basic blocks that increment the biv, but only
     one of these blocks is executed during each iteration.  There is
     no good way to detect cases like this with the current structure
     of the loop optimizer.  This code is more accurate for
     determining code size than run-time benefits.  */
  benefit -= add_cost * bl->biv_count;

  /* Decide whether to strength-reduce this giv or to leave the code
     unchanged (recompute it from the biv each time it is used).  This
     decision can be made independently for each giv.  */

#ifdef AUTO_INC_DEC
  /* Attempt to guess whether autoincrement will handle some of the
     new add insns; if so, increase BENEFIT (undo the subtraction of
     add_cost that was done above).  */
  if (v->giv_type == DEST_ADDR
      /* Increasing the benefit is risky, since this is only a guess.
	 Avoid increasing register pressure in cases where there would
	 be no other benefit from reducing this giv.  */
      && benefit > 0
      && GET_CODE (v->mult_val) == CONST_INT)
    {
      int size = GET_MODE_SIZE (GET_MODE (v->mem));

      if (HAVE_POST_INCREMENT
	  && INTVAL (v->mult_val) == size)
	benefit += add_cost * bl->biv_count;
      else if (HAVE_PRE_INCREMENT
	       && INTVAL (v->mult_val) == size)
	benefit += add_cost * bl->biv_count;
      else if (HAVE_POST_DECREMENT
	       && -INTVAL (v->mult_val) == size)
	benefit += add_cost * bl->biv_count;
      else if (HAVE_PRE_DECREMENT
	       && -INTVAL (v->mult_val) == size)
	benefit += add_cost * bl->biv_count;
    }
#endif

  return benefit;
}


/* Free IV structures for LOOP.  */

static void
loop_ivs_free (loop)
     struct loop *loop;
{
  struct loop_ivs *ivs = LOOP_IVS (loop);
  struct iv_class *iv = ivs->list;

  free (ivs->regs);

  while (iv)
    {
      struct iv_class *next = iv->next;
      struct induction *induction;
      struct induction *next_induction;

      for (induction = iv->biv; induction; induction = next_induction)
	{
	  next_induction = induction->next_iv;
	  free (induction);
	}
      for (induction = iv->giv; induction; induction = next_induction)
	{
	  next_induction = induction->next_iv;
	  free (induction);
	}

      free (iv);
      iv = next;
    }
}


/* Perform strength reduction and induction variable elimination.

   Pseudo registers created during this function will be beyond the
   last valid index in several tables including
   REGS->ARRAY[I].N_TIMES_SET and REGNO_LAST_UID.  This does not cause a
   problem here, because the added registers cannot be givs outside of
   their loop, and hence will never be reconsidered.  But scan_loop
   must check regnos to make sure they are in bounds.  */

static void
strength_reduce (loop, flags)
     struct loop *loop;
     int flags;
{
  struct loop_info *loop_info = LOOP_INFO (loop);
  struct loop_regs *regs = LOOP_REGS (loop);
  struct loop_ivs *ivs = LOOP_IVS (loop);
  rtx p;
  /* Temporary list pointer for traversing ivs->list.  */
  struct iv_class *bl;
  /* Ratio of extra register life span we can justify
     for saving an instruction.  More if loop doesn't call subroutines
     since in that case saving an insn makes more difference
     and more registers are available.  */
  /* ??? could set this to last value of threshold in move_movables */
  int threshold = (loop_info->has_call ? 1 : 2) * (3 + n_non_fixed_regs);
  /* Map of pseudo-register replacements.  */
  rtx *reg_map = NULL;
  int reg_map_size;
  int unrolled_insn_copies = 0;
  rtx test_reg = gen_rtx_REG (word_mode, LAST_VIRTUAL_REGISTER + 1);
  int insn_count = count_insns_in_loop (loop);

  addr_placeholder = gen_reg_rtx (Pmode);

  ivs->n_regs = max_reg_before_loop;
  ivs->regs = (struct iv *) xcalloc (ivs->n_regs, sizeof (struct iv));

  /* Find all BIVs in loop.  */
  loop_bivs_find (loop);

  /* Exit if there are no bivs.  */
  if (! ivs->list)
    {
      /* Can still unroll the loop anyways, but indicate that there is no
	 strength reduction info available.  */
      if (flags & LOOP_UNROLL)
	unroll_loop (loop, insn_count, 0);

      loop_ivs_free (loop);
      return;
    }

  /* Determine how BIVS are initialised by looking through pre-header
     extended basic block.  */
  loop_bivs_init_find (loop);

  /* Look at the each biv and see if we can say anything better about its
     initial value from any initializing insns set up above.  */
  loop_bivs_check (loop);

  /* Search the loop for general induction variables.  */
  loop_givs_find (loop);

  /* Try to calculate and save the number of loop iterations.  This is
     set to zero if the actual number can not be calculated.  This must
     be called after all giv's have been identified, since otherwise it may
     fail if the iteration variable is a giv.  */
  loop_iterations (loop);

#ifdef HAVE_prefetch
  if (flags & LOOP_PREFETCH)
    emit_prefetch_instructions (loop);
#endif

  /* Now for each giv for which we still don't know whether or not it is
     replaceable, check to see if it is replaceable because its final value
     can be calculated.  This must be done after loop_iterations is called,
     so that final_giv_value will work correctly.  */
  loop_givs_check (loop);

  /* Try to prove that the loop counter variable (if any) is always
     nonnegative; if so, record that fact with a REG_NONNEG note
     so that "decrement and branch until zero" insn can be used.  */
  check_dbra_loop (loop, insn_count);

  /* Create reg_map to hold substitutions for replaceable giv regs.
     Some givs might have been made from biv increments, so look at
     ivs->reg_iv_type for a suitable size.  */
  reg_map_size = ivs->n_regs;
  reg_map = (rtx *) xcalloc (reg_map_size, sizeof (rtx));

  /* Examine each iv class for feasibility of strength reduction/induction
     variable elimination.  */

  for (bl = ivs->list; bl; bl = bl->next)
    {
      struct induction *v;
      int benefit;

      /* Test whether it will be possible to eliminate this biv
	 provided all givs are reduced.  */
      bl->eliminable = loop_biv_eliminable_p (loop, bl, threshold, insn_count);

      /* This will be true at the end, if all givs which depend on this
	 biv have been strength reduced.
	 We can't (currently) eliminate the biv unless this is so.  */
      bl->all_reduced = 1;

      /* Check each extension dependent giv in this class to see if its
	 root biv is safe from wrapping in the interior mode.  */
      check_ext_dependent_givs (bl, loop_info);

      /* Combine all giv's for this iv_class.  */
      combine_givs (regs, bl);

      for (v = bl->giv; v; v = v->next_iv)
	{
	  struct induction *tv;

	  if (v->ignore || v->same)
	    continue;

	  benefit = loop_giv_reduce_benefit (loop, bl, v, test_reg);

	  /* If an insn is not to be strength reduced, then set its ignore
	     flag, and clear bl->all_reduced.  */

	  /* A giv that depends on a reversed biv must be reduced if it is
	     used after the loop exit, otherwise, it would have the wrong
	     value after the loop exit.  To make it simple, just reduce all
	     of such giv's whether or not we know they are used after the loop
	     exit.  */

	  if (! flag_reduce_all_givs
	      && v->lifetime * threshold * benefit < insn_count
	      && ! bl->reversed)
	    {
	      if (loop_dump_stream)
		fprintf (loop_dump_stream,
			 "giv of insn %d not worth while, %d vs %d.\n",
			 INSN_UID (v->insn),
			 v->lifetime * threshold * benefit, insn_count);
	      v->ignore = 1;
	      bl->all_reduced = 0;
	    }
	  else
	    {
	      /* Check that we can increment the reduced giv without a
		 multiply insn.  If not, reject it.  */

	      for (tv = bl->biv; tv; tv = tv->next_iv)
		if (tv->mult_val == const1_rtx
		    && ! product_cheap_p (tv->add_val, v->mult_val))
		  {
		    if (loop_dump_stream)
		      fprintf (loop_dump_stream,
			       "giv of insn %d: would need a multiply.\n",
			       INSN_UID (v->insn));
		    v->ignore = 1;
		    bl->all_reduced = 0;
		    break;
		  }
	    }
	}

      /* Check for givs whose first use is their definition and whose
	 last use is the definition of another giv.  If so, it is likely
	 dead and should not be used to derive another giv nor to
	 eliminate a biv.  */
      loop_givs_dead_check (loop, bl);

      /* Reduce each giv that we decided to reduce.  */
      loop_givs_reduce (loop, bl);

      /* Rescan all givs.  If a giv is the same as a giv not reduced, mark it
	 as not reduced.

	 For each giv register that can be reduced now: if replaceable,
	 substitute reduced reg wherever the old giv occurs;
	 else add new move insn "giv_reg = reduced_reg".  */
      loop_givs_rescan (loop, bl, reg_map);

      /* All the givs based on the biv bl have been reduced if they
	 merit it.  */

      /* For each giv not marked as maybe dead that has been combined with a
	 second giv, clear any "maybe dead" mark on that second giv.
	 v->new_reg will either be or refer to the register of the giv it
	 combined with.

	 Doing this clearing avoids problems in biv elimination where
	 a giv's new_reg is a complex value that can't be put in the
	 insn but the giv combined with (with a reg as new_reg) is
	 marked maybe_dead.  Since the register will be used in either
	 case, we'd prefer it be used from the simpler giv.  */

      for (v = bl->giv; v; v = v->next_iv)
	if (! v->maybe_dead && v->same)
	  v->same->maybe_dead = 0;

      /* Try to eliminate the biv, if it is a candidate.
	 This won't work if ! bl->all_reduced,
	 since the givs we planned to use might not have been reduced.

	 We have to be careful that we didn't initially think we could
	 eliminate this biv because of a giv that we now think may be
	 dead and shouldn't be used as a biv replacement.

	 Also, there is the possibility that we may have a giv that looks
	 like it can be used to eliminate a biv, but the resulting insn
	 isn't valid.  This can happen, for example, on the 88k, where a
	 JUMP_INSN can compare a register only with zero.  Attempts to
	 replace it with a compare with a constant will fail.

	 Note that in cases where this call fails, we may have replaced some
	 of the occurrences of the biv with a giv, but no harm was done in
	 doing so in the rare cases where it can occur.  */

      if (bl->all_reduced == 1 && bl->eliminable
	  && maybe_eliminate_biv (loop, bl, 1, threshold, insn_count))
	{
	  /* ?? If we created a new test to bypass the loop entirely,
	     or otherwise drop straight in, based on this test, then
	     we might want to rewrite it also.  This way some later
	     pass has more hope of removing the initialization of this
	     biv entirely.  */

	  /* If final_value != 0, then the biv may be used after loop end
	     and we must emit an insn to set it just in case.

	     Reversed bivs already have an insn after the loop setting their
	     value, so we don't need another one.  We can't calculate the
	     proper final value for such a biv here anyways.  */
	  if (bl->final_value && ! bl->reversed)
	      loop_insn_sink_or_swim (loop, gen_move_insn
				      (bl->biv->dest_reg, bl->final_value));

	  if (loop_dump_stream)
	    fprintf (loop_dump_stream, "Reg %d: biv eliminated\n",
		     bl->regno);
	}
      /* See above note wrt final_value.  But since we couldn't eliminate
	 the biv, we must set the value after the loop instead of before.  */
      else if (bl->final_value && ! bl->reversed)
	loop_insn_sink (loop, gen_move_insn (bl->biv->dest_reg,
					     bl->final_value));
    }

  /* Go through all the instructions in the loop, making all the
     register substitutions scheduled in REG_MAP.  */

  for (p = loop->start; p != loop->end; p = NEXT_INSN (p))
    if (GET_CODE (p) == INSN || GET_CODE (p) == JUMP_INSN
	|| GET_CODE (p) == CALL_INSN)
      {
	replace_regs (PATTERN (p), reg_map, reg_map_size, 0);
	replace_regs (REG_NOTES (p), reg_map, reg_map_size, 0);
	INSN_CODE (p) = -1;
      }

  if (loop_info->n_iterations > 0)
    {
      /* When we completely unroll a loop we will likely not need the increment
	 of the loop BIV and we will not need the conditional branch at the
	 end of the loop.  */
      unrolled_insn_copies = insn_count - 2;

#ifdef HAVE_cc0
      /* When we completely unroll a loop on a HAVE_cc0 machine we will not
	 need the comparison before the conditional branch at the end of the
	 loop.  */
      unrolled_insn_copies -= 1;
#endif

      /* We'll need one copy for each loop iteration.  */
      unrolled_insn_copies *= loop_info->n_iterations;

      /* A little slop to account for the ability to remove initialization
	 code, better CSE, and other secondary benefits of completely
	 unrolling some loops.  */
      unrolled_insn_copies -= 1;

      /* Clamp the value.  */
      if (unrolled_insn_copies < 0)
	unrolled_insn_copies = 0;
    }

  /* Unroll loops from within strength reduction so that we can use the
     induction variable information that strength_reduce has already
     collected.  Always unroll loops that would be as small or smaller
     unrolled than when rolled.  */
  if ((flags & LOOP_UNROLL)
      || (!(flags & LOOP_FIRST_PASS)
	  && loop_info->n_iterations > 0
	  && unrolled_insn_copies <= insn_count))
    unroll_loop (loop, insn_count, 1);

#ifdef HAVE_doloop_end
  if (HAVE_doloop_end && (flags & LOOP_BCT) && flag_branch_on_count_reg)
    doloop_optimize (loop);
#endif  /* HAVE_doloop_end  */

  /* In case number of iterations is known, drop branch prediction note
     in the branch.  Do that only in second loop pass, as loop unrolling
     may change the number of iterations performed.  */
  if ((flags & LOOP_BCT)
      && loop_info->n_iterations / loop_info->unroll_number > 1)
    {
      int n = loop_info->n_iterations / loop_info->unroll_number;
      predict_insn (PREV_INSN (loop->end),
		    PRED_LOOP_ITERATIONS,
		    REG_BR_PROB_BASE - REG_BR_PROB_BASE / n);
    }

  if (loop_dump_stream)
    fprintf (loop_dump_stream, "\n");

  loop_ivs_free (loop);
  if (reg_map)
    free (reg_map);
}

/*Record all basic induction variables calculated in the insn.  */
static rtx
check_insn_for_bivs (loop, p, not_every_iteration, maybe_multiple)
     struct loop *loop;
     rtx p;
     int not_every_iteration;
     int maybe_multiple;
{
  struct loop_ivs *ivs = LOOP_IVS (loop);
  rtx set;
  rtx dest_reg;
  rtx inc_val;
  rtx mult_val;
  rtx *location;

  if (GET_CODE (p) == INSN
      && (set = single_set (p))
      && GET_CODE (SET_DEST (set)) == REG)
    {
      dest_reg = SET_DEST (set);
      if (REGNO (dest_reg) < max_reg_before_loop
	  && REGNO (dest_reg) >= FIRST_PSEUDO_REGISTER
	  && REG_IV_TYPE (ivs, REGNO (dest_reg)) != NOT_BASIC_INDUCT)
	{
	  if (basic_induction_var (loop, SET_SRC (set),
				   GET_MODE (SET_SRC (set)),
				   dest_reg, p, &inc_val, &mult_val,
				   &location))
	    {
	      /* It is a possible basic induction variable.
	         Create and initialize an induction structure for it.  */

	      struct induction *v
		= (struct induction *) xmalloc (sizeof (struct induction));

	      record_biv (loop, v, p, dest_reg, inc_val, mult_val, location,
			  not_every_iteration, maybe_multiple);
	      REG_IV_TYPE (ivs, REGNO (dest_reg)) = BASIC_INDUCT;
	    }
	  else if (REGNO (dest_reg) < ivs->n_regs)
	    REG_IV_TYPE (ivs, REGNO (dest_reg)) = NOT_BASIC_INDUCT;
	}
    }
  return p;
}

/* Record all givs calculated in the insn.
   A register is a giv if: it is only set once, it is a function of a
   biv and a constant (or invariant), and it is not a biv.  */
static rtx
check_insn_for_givs (loop, p, not_every_iteration, maybe_multiple)
     struct loop *loop;
     rtx p;
     int not_every_iteration;
     int maybe_multiple;
{
  struct loop_regs *regs = LOOP_REGS (loop);

  rtx set;
  /* Look for a general induction variable in a register.  */
  if (GET_CODE (p) == INSN
      && (set = single_set (p))
      && GET_CODE (SET_DEST (set)) == REG
      && ! regs->array[REGNO (SET_DEST (set))].may_not_optimize)
    {
      rtx src_reg;
      rtx dest_reg;
      rtx add_val;
      rtx mult_val;
      rtx ext_val;
      int benefit;
      rtx regnote = 0;
      rtx last_consec_insn;

      dest_reg = SET_DEST (set);
      if (REGNO (dest_reg) < FIRST_PSEUDO_REGISTER)
	return p;

      if (/* SET_SRC is a giv.  */
	  (general_induction_var (loop, SET_SRC (set), &src_reg, &add_val,
				  &mult_val, &ext_val, 0, &benefit, VOIDmode)
	   /* Equivalent expression is a giv.  */
	   || ((regnote = find_reg_note (p, REG_EQUAL, NULL_RTX))
	       && general_induction_var (loop, XEXP (regnote, 0), &src_reg,
					 &add_val, &mult_val, &ext_val, 0,
					 &benefit, VOIDmode)))
	  /* Don't try to handle any regs made by loop optimization.
	     We have nothing on them in regno_first_uid, etc.  */
	  && REGNO (dest_reg) < max_reg_before_loop
	  /* Don't recognize a BASIC_INDUCT_VAR here.  */
	  && dest_reg != src_reg
	  /* This must be the only place where the register is set.  */
	  && (regs->array[REGNO (dest_reg)].n_times_set == 1
	      /* or all sets must be consecutive and make a giv.  */
	      || (benefit = consec_sets_giv (loop, benefit, p,
					     src_reg, dest_reg,
					     &add_val, &mult_val, &ext_val,
					     &last_consec_insn))))
	{
	  struct induction *v
	    = (struct induction *) xmalloc (sizeof (struct induction));

	  /* If this is a library call, increase benefit.  */
	  if (find_reg_note (p, REG_RETVAL, NULL_RTX))
	    benefit += libcall_benefit (p);

	  /* Skip the consecutive insns, if there are any.  */
	  if (regs->array[REGNO (dest_reg)].n_times_set != 1)
	    p = last_consec_insn;

	  record_giv (loop, v, p, src_reg, dest_reg, mult_val, add_val,
		      ext_val, benefit, DEST_REG, not_every_iteration,
		      maybe_multiple, (rtx*) 0);

	}
    }

#ifndef DONT_REDUCE_ADDR
  /* Look for givs which are memory addresses.  */
  /* This resulted in worse code on a VAX 8600.  I wonder if it
     still does.  */
  if (GET_CODE (p) == INSN)
    find_mem_givs (loop, PATTERN (p), p, not_every_iteration,
		   maybe_multiple);
#endif

  /* Update the status of whether giv can derive other givs.  This can
     change when we pass a label or an insn that updates a biv.  */
  if (GET_CODE (p) == INSN || GET_CODE (p) == JUMP_INSN
      || GET_CODE (p) == CODE_LABEL)
    update_giv_derive (loop, p);
  return p;
}

/* Return 1 if X is a valid source for an initial value (or as value being
   compared against in an initial test).

   X must be either a register or constant and must not be clobbered between
   the current insn and the start of the loop.

   INSN is the insn containing X.  */

static int
valid_initial_value_p (x, insn, call_seen, loop_start)
     rtx x;
     rtx insn;
     int call_seen;
     rtx loop_start;
{
  if (CONSTANT_P (x))
    return 1;

  /* Only consider pseudos we know about initialized in insns whose luids
     we know.  */
  if (GET_CODE (x) != REG
      || REGNO (x) >= max_reg_before_loop)
    return 0;

  /* Don't use call-clobbered registers across a call which clobbers it.  On
     some machines, don't use any hard registers at all.  */
  if (REGNO (x) < FIRST_PSEUDO_REGISTER
      && (SMALL_REGISTER_CLASSES
	  || (call_used_regs[REGNO (x)] && call_seen)))
    return 0;

  /* Don't use registers that have been clobbered before the start of the
     loop.  */
  if (reg_set_between_p (x, insn, loop_start))
    return 0;

  return 1;
}

/* Scan X for memory refs and check each memory address
   as a possible giv.  INSN is the insn whose pattern X comes from.
   NOT_EVERY_ITERATION is 1 if the insn might not be executed during
   every loop iteration.  MAYBE_MULTIPLE is 1 if the insn might be executed
   more thanonce in each loop iteration.  */

static void
find_mem_givs (loop, x, insn, not_every_iteration, maybe_multiple)
     const struct loop *loop;
     rtx x;
     rtx insn;
     int not_every_iteration, maybe_multiple;
{
  int i, j;
  enum rtx_code code;
  const char *fmt;

  if (x == 0)
    return;

  code = GET_CODE (x);
  switch (code)
    {
    case REG:
    case CONST_INT:
    case CONST:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case LABEL_REF:
    case PC:
    case CC0:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
    case USE:
    case CLOBBER:
      return;

    case MEM:
      {
	rtx src_reg;
	rtx add_val;
	rtx mult_val;
	rtx ext_val;
	int benefit;

	/* This code used to disable creating GIVs with mult_val == 1 and
	   add_val == 0.  However, this leads to lost optimizations when
	   it comes time to combine a set of related DEST_ADDR GIVs, since
	   this one would not be seen.  */

	if (general_induction_var (loop, XEXP (x, 0), &src_reg, &add_val,
				   &mult_val, &ext_val, 1, &benefit,
				   GET_MODE (x)))
	  {
	    /* Found one; record it.  */
	    struct induction *v
	      = (struct induction *) xmalloc (sizeof (struct induction));

	    record_giv (loop, v, insn, src_reg, addr_placeholder, mult_val,
			add_val, ext_val, benefit, DEST_ADDR,
			not_every_iteration, maybe_multiple, &XEXP (x, 0));

	    v->mem = x;
	  }
      }
      return;

    default:
      break;
    }

  /* Recursively scan the subexpressions for other mem refs.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    if (fmt[i] == 'e')
      find_mem_givs (loop, XEXP (x, i), insn, not_every_iteration,
		     maybe_multiple);
    else if (fmt[i] == 'E')
      for (j = 0; j < XVECLEN (x, i); j++)
	find_mem_givs (loop, XVECEXP (x, i, j), insn, not_every_iteration,
		       maybe_multiple);
}

/* Fill in the data about one biv update.
   V is the `struct induction' in which we record the biv.  (It is
   allocated by the caller, with alloca.)
   INSN is the insn that sets it.
   DEST_REG is the biv's reg.

   MULT_VAL is const1_rtx if the biv is being incremented here, in which case
   INC_VAL is the increment.  Otherwise, MULT_VAL is const0_rtx and the biv is
   being set to INC_VAL.

   NOT_EVERY_ITERATION is nonzero if this biv update is not know to be
   executed every iteration; MAYBE_MULTIPLE is nonzero if this biv update
   can be executed more than once per iteration.  If MAYBE_MULTIPLE
   and NOT_EVERY_ITERATION are both zero, we know that the biv update is
   executed exactly once per iteration.  */

static void
record_biv (loop, v, insn, dest_reg, inc_val, mult_val, location,
	    not_every_iteration, maybe_multiple)
     struct loop *loop;
     struct induction *v;
     rtx insn;
     rtx dest_reg;
     rtx inc_val;
     rtx mult_val;
     rtx *location;
     int not_every_iteration;
     int maybe_multiple;
{
  struct loop_ivs *ivs = LOOP_IVS (loop);
  struct iv_class *bl;

  v->insn = insn;
  v->src_reg = dest_reg;
  v->dest_reg = dest_reg;
  v->mult_val = mult_val;
  v->add_val = inc_val;
  v->ext_dependent = NULL_RTX;
  v->location = location;
  v->mode = GET_MODE (dest_reg);
  v->always_computable = ! not_every_iteration;
  v->always_executed = ! not_every_iteration;
  v->maybe_multiple = maybe_multiple;

  /* Add this to the reg's iv_class, creating a class
     if this is the first incrementation of the reg.  */

  bl = REG_IV_CLASS (ivs, REGNO (dest_reg));
  if (bl == 0)
    {
      /* Create and initialize new iv_class.  */

      bl = (struct iv_class *) xmalloc (sizeof (struct iv_class));

      bl->regno = REGNO (dest_reg);
      bl->biv = 0;
      bl->giv = 0;
      bl->biv_count = 0;
      bl->giv_count = 0;

      /* Set initial value to the reg itself.  */
      bl->initial_value = dest_reg;
      bl->final_value = 0;
      /* We haven't seen the initializing insn yet */
      bl->init_insn = 0;
      bl->init_set = 0;
      bl->initial_test = 0;
      bl->incremented = 0;
      bl->eliminable = 0;
      bl->nonneg = 0;
      bl->reversed = 0;
      bl->total_benefit = 0;

      /* Add this class to ivs->list.  */
      bl->next = ivs->list;
      ivs->list = bl;

      /* Put it in the array of biv register classes.  */
      REG_IV_CLASS (ivs, REGNO (dest_reg)) = bl;
    }

  /* Update IV_CLASS entry for this biv.  */
  v->next_iv = bl->biv;
  bl->biv = v;
  bl->biv_count++;
  if (mult_val == const1_rtx)
    bl->incremented = 1;

  if (loop_dump_stream)
    loop_biv_dump (v, loop_dump_stream, 0);
}

/* Fill in the data about one giv.
   V is the `struct induction' in which we record the giv.  (It is
   allocated by the caller, with alloca.)
   INSN is the insn that sets it.
   BENEFIT estimates the savings from deleting this insn.
   TYPE is DEST_REG or DEST_ADDR; it says whether the giv is computed
   into a register or is used as a memory address.

   SRC_REG is the biv reg which the giv is computed from.
   DEST_REG is the giv's reg (if the giv is stored in a reg).
   MULT_VAL and ADD_VAL are the coefficients used to compute the giv.
   LOCATION points to the place where this giv's value appears in INSN.  */

static void
record_giv (loop, v, insn, src_reg, dest_reg, mult_val, add_val, ext_val,
	    benefit, type, not_every_iteration, maybe_multiple, location)
     const struct loop *loop;
     struct induction *v;
     rtx insn;
     rtx src_reg;
     rtx dest_reg;
     rtx mult_val, add_val, ext_val;
     int benefit;
     enum g_types type;
     int not_every_iteration, maybe_multiple;
     rtx *location;
{
  struct loop_ivs *ivs = LOOP_IVS (loop);
  struct induction *b;
  struct iv_class *bl;
  rtx set = single_set (insn);
  rtx temp;

  /* Attempt to prove constantness of the values.  Don't let simplity_rtx
     undo the MULT canonicalization that we performed earlier.  */
  temp = simplify_rtx (add_val);
  if (temp
      && ! (GET_CODE (add_val) == MULT
	    && GET_CODE (temp) == ASHIFT))
    add_val = temp;

  v->insn = insn;
  v->src_reg = src_reg;
  v->giv_type = type;
  v->dest_reg = dest_reg;
  v->mult_val = mult_val;
  v->add_val = add_val;
  v->ext_dependent = ext_val;
  v->benefit = benefit;
  v->location = location;
  v->cant_derive = 0;
  v->combined_with = 0;
  v->maybe_multiple = maybe_multiple;
  v->maybe_dead = 0;
  v->derive_adjustment = 0;
  v->same = 0;
  v->ignore = 0;
  v->new_reg = 0;
  v->final_value = 0;
  v->same_insn = 0;
  v->auto_inc_opt = 0;
  v->unrolled = 0;
  v->shared = 0;

  /* The v->always_computable field is used in update_giv_derive, to
     determine whether a giv can be used to derive another giv.  For a
     DEST_REG giv, INSN computes a new value for the giv, so its value
     isn't computable if INSN insn't executed every iteration.
     However, for a DEST_ADDR giv, INSN merely uses the value of the giv;
     it does not compute a new value.  Hence the value is always computable
     regardless of whether INSN is executed each iteration.  */

  if (type == DEST_ADDR)
    v->always_computable = 1;
  else
    v->always_computable = ! not_every_iteration;

  v->always_executed = ! not_every_iteration;

  if (type == DEST_ADDR)
    {
      v->mode = GET_MODE (*location);
      v->lifetime = 1;
    }
  else /* type == DEST_REG */
    {
      v->mode = GET_MODE (SET_DEST (set));

      v->lifetime = LOOP_REG_LIFETIME (loop, REGNO (dest_reg));

      /* If the lifetime is zero, it means that this register is
	 really a dead store.  So mark this as a giv that can be
	 ignored.  This will not prevent the biv from being eliminated.  */
      if (v->lifetime == 0)
	v->ignore = 1;

      REG_IV_TYPE (ivs, REGNO (dest_reg)) = GENERAL_INDUCT;
      REG_IV_INFO (ivs, REGNO (dest_reg)) = v;
    }

  /* Add the giv to the class of givs computed from one biv.  */

  bl = REG_IV_CLASS (ivs, REGNO (src_reg));
  if (bl)
    {
      v->next_iv = bl->giv;
      bl->giv = v;
      /* Don't count DEST_ADDR.  This is supposed to count the number of
	 insns that calculate givs.  */
      if (type == DEST_REG)
	bl->giv_count++;
      bl->total_benefit += benefit;
    }
  else
    /* Fatal error, biv missing for this giv?  */
    abort ();

  if (type == DEST_ADDR)
    v->replaceable = 1;
  else
    {
      /* The giv can be replaced outright by the reduced register only if all
	 of the following conditions are true:
	 - the insn that sets the giv is always executed on any iteration
	   on which the giv is used at all
	   (there are two ways to deduce this:
	    either the insn is executed on every iteration,
	    or all uses follow that insn in the same basic block),
	 - the giv is not used outside the loop
	 - no assignments to the biv occur during the giv's lifetime.  */

      if (REGNO_FIRST_UID (REGNO (dest_reg)) == INSN_UID (insn)
	  /* Previous line always fails if INSN was moved by loop opt.  */
	  && REGNO_LAST_LUID (REGNO (dest_reg))
	  < INSN_LUID (loop->end)
	  && (! not_every_iteration
	      || last_use_this_basic_block (dest_reg, insn)))
	{
	  /* Now check that there are no assignments to the biv within the
	     giv's lifetime.  This requires two separate checks.  */

	  /* Check each biv update, and fail if any are between the first
	     and last use of the giv.

	     If this loop contains an inner loop that was unrolled, then
	     the insn modifying the biv may have been emitted by the loop
	     unrolling code, and hence does not have a valid luid.  Just
	     mark the biv as not replaceable in this case.  It is not very
	     useful as a biv, because it is used in two different loops.
	     It is very unlikely that we would be able to optimize the giv
	     using this biv anyways.  */

	  v->replaceable = 1;
	  for (b = bl->biv; b; b = b->next_iv)
	    {
	      if (INSN_UID (b->insn) >= max_uid_for_loop
		  || ((INSN_LUID (b->insn)
		       >= REGNO_FIRST_LUID (REGNO (dest_reg)))
		      && (INSN_LUID (b->insn)
			  <= REGNO_LAST_LUID (REGNO (dest_reg)))))
		{
		  v->replaceable = 0;
		  v->not_replaceable = 1;
		  break;
		}
	    }

	  /* If there are any backwards branches that go from after the
	     biv update to before it, then this giv is not replaceable.  */
	  if (v->replaceable)
	    for (b = bl->biv; b; b = b->next_iv)
	      if (back_branch_in_range_p (loop, b->insn))
		{
		  v->replaceable = 0;
		  v->not_replaceable = 1;
		  break;
		}
	}
      else
	{
	  /* May still be replaceable, we don't have enough info here to
	     decide.  */
	  v->replaceable = 0;
	  v->not_replaceable = 0;
	}
    }

  /* Record whether the add_val contains a const_int, for later use by
     combine_givs.  */
  {
    rtx tem = add_val;

    v->no_const_addval = 1;
    if (tem == const0_rtx)
      ;
    else if (CONSTANT_P (add_val))
      v->no_const_addval = 0;
    if (GET_CODE (tem) == PLUS)
      {
	while (1)
	  {
	    if (GET_CODE (XEXP (tem, 0)) == PLUS)
	      tem = XEXP (tem, 0);
	    else if (GET_CODE (XEXP (tem, 1)) == PLUS)
	      tem = XEXP (tem, 1);
	    else
	      break;
	  }
	if (CONSTANT_P (XEXP (tem, 1)))
	  v->no_const_addval = 0;
      }
  }

  if (loop_dump_stream)
    loop_giv_dump (v, loop_dump_stream, 0);
}

/* All this does is determine whether a giv can be made replaceable because
   its final value can be calculated.  This code can not be part of record_giv
   above, because final_giv_value requires that the number of loop iterations
   be known, and that can not be accurately calculated until after all givs
   have been identified.  */

static void
check_final_value (loop, v)
     const struct loop *loop;
     struct induction *v;
{
  struct loop_ivs *ivs = LOOP_IVS (loop);
  struct iv_class *bl;
  rtx final_value = 0;

  bl = REG_IV_CLASS (ivs, REGNO (v->src_reg));

  /* DEST_ADDR givs will never reach here, because they are always marked
     replaceable above in record_giv.  */

  /* The giv can be replaced outright by the reduced register only if all
     of the following conditions are true:
     - the insn that sets the giv is always executed on any iteration
       on which the giv is used at all
       (there are two ways to deduce this:
        either the insn is executed on every iteration,
        or all uses follow that insn in the same basic block),
     - its final value can be calculated (this condition is different
       than the one above in record_giv)
     - it's not used before the it's set
     - no assignments to the biv occur during the giv's lifetime.  */

#if 0
  /* This is only called now when replaceable is known to be false.  */
  /* Clear replaceable, so that it won't confuse final_giv_value.  */
  v->replaceable = 0;
#endif

  if ((final_value = final_giv_value (loop, v))
      && (v->always_executed || last_use_this_basic_block (v->dest_reg, v->insn)))
    {
      int biv_increment_seen = 0, before_giv_insn = 0;
      rtx p = v->insn;
      rtx last_giv_use;

      v->replaceable = 1;

      /* When trying to determine whether or not a biv increment occurs
	 during the lifetime of the giv, we can ignore uses of the variable
	 outside the loop because final_value is true.  Hence we can not
	 use regno_last_uid and regno_first_uid as above in record_giv.  */

      /* Search the loop to determine whether any assignments to the
	 biv occur during the giv's lifetime.  Start with the insn
	 that sets the giv, and search around the loop until we come
	 back to that insn again.

	 Also fail if there is a jump within the giv's lifetime that jumps
	 to somewhere outside the lifetime but still within the loop.  This
	 catches spaghetti code where the execution order is not linear, and
	 hence the above test fails.  Here we assume that the giv lifetime
	 does not extend from one iteration of the loop to the next, so as
	 to make the test easier.  Since the lifetime isn't known yet,
	 this requires two loops.  See also record_giv above.  */

      last_giv_use = v->insn;

      while (1)
	{
	  p = NEXT_INSN (p);
	  if (p == loop->end)
	    {
	      before_giv_insn = 1;
	      p = NEXT_INSN (loop->start);
	    }
	  if (p == v->insn)
	    break;

	  if (GET_CODE (p) == INSN || GET_CODE (p) == JUMP_INSN
	      || GET_CODE (p) == CALL_INSN)
	    {
	      /* It is possible for the BIV increment to use the GIV if we
		 have a cycle.  Thus we must be sure to check each insn for
		 both BIV and GIV uses, and we must check for BIV uses
		 first.  */

	      if (! biv_increment_seen
		  && reg_set_p (v->src_reg, PATTERN (p)))
		biv_increment_seen = 1;

	      if (reg_mentioned_p (v->dest_reg, PATTERN (p)))
		{
		  if (biv_increment_seen || before_giv_insn)
		    {
		      v->replaceable = 0;
		      v->not_replaceable = 1;
		      break;
		    }
		  last_giv_use = p;
		}
	    }
	}

      /* Now that the lifetime of the giv is known, check for branches
	 from within the lifetime to outside the lifetime if it is still
	 replaceable.  */

      if (v->replaceable)
	{
	  p = v->insn;
	  while (1)
	    {
	      p = NEXT_INSN (p);
	      if (p == loop->end)
		p = NEXT_INSN (loop->start);
	      if (p == last_giv_use)
		break;

	      if (GET_CODE (p) == JUMP_INSN && JUMP_LABEL (p)
		  && LABEL_NAME (JUMP_LABEL (p))
		  && ((loop_insn_first_p (JUMP_LABEL (p), v->insn)
		       && loop_insn_first_p (loop->start, JUMP_LABEL (p)))
		      || (loop_insn_first_p (last_giv_use, JUMP_LABEL (p))
			  && loop_insn_first_p (JUMP_LABEL (p), loop->end))))
		{
		  v->replaceable = 0;
		  v->not_replaceable = 1;

		  if (loop_dump_stream)
		    fprintf (loop_dump_stream,
			     "Found branch outside giv lifetime.\n");

		  break;
		}
	    }
	}

      /* If it is replaceable, then save the final value.  */
      if (v->replaceable)
	v->final_value = final_value;
    }

  if (loop_dump_stream && v->replaceable)
    fprintf (loop_dump_stream, "Insn %d: giv reg %d final_value replaceable\n",
	     INSN_UID (v->insn), REGNO (v->dest_reg));
}

/* Update the status of whether a giv can derive other givs.

   We need to do something special if there is or may be an update to the biv
   between the time the giv is defined and the time it is used to derive
   another giv.

   In addition, a giv that is only conditionally set is not allowed to
   derive another giv once a label has been passed.

   The cases we look at are when a label or an update to a biv is passed.  */

static void
update_giv_derive (loop, p)
     const struct loop *loop;
     rtx p;
{
  struct loop_ivs *ivs = LOOP_IVS (loop);
  struct iv_class *bl;
  struct induction *biv, *giv;
  rtx tem;
  int dummy;

  /* Search all IV classes, then all bivs, and finally all givs.

     There are three cases we are concerned with.  First we have the situation
     of a giv that is only updated conditionally.  In that case, it may not
     derive any givs after a label is passed.

     The second case is when a biv update occurs, or may occur, after the
     definition of a giv.  For certain biv updates (see below) that are
     known to occur between the giv definition and use, we can adjust the
     giv definition.  For others, or when the biv update is conditional,
     we must prevent the giv from deriving any other givs.  There are two
     sub-cases within this case.

     If this is a label, we are concerned with any biv update that is done
     conditionally, since it may be done after the giv is defined followed by
     a branch here (actually, we need to pass both a jump and a label, but
     this extra tracking doesn't seem worth it).

     If this is a jump, we are concerned about any biv update that may be
     executed multiple times.  We are actually only concerned about
     backward jumps, but it is probably not worth performing the test
     on the jump again here.

     If this is a biv update, we must adjust the giv status to show that a
     subsequent biv update was performed.  If this adjustment cannot be done,
     the giv cannot derive further givs.  */

  for (bl = ivs->list; bl; bl = bl->next)
    for (biv = bl->biv; biv; biv = biv->next_iv)
      if (GET_CODE (p) == CODE_LABEL || GET_CODE (p) == JUMP_INSN
	  || biv->insn == p)
	{
	  for (giv = bl->giv; giv; giv = giv->next_iv)
	    {
	      /* If cant_derive is already true, there is no point in
		 checking all of these conditions again.  */
	      if (giv->cant_derive)
		continue;

	      /* If this giv is conditionally set and we have passed a label,
		 it cannot derive anything.  */
	      if (GET_CODE (p) == CODE_LABEL && ! giv->always_computable)
		giv->cant_derive = 1;

	      /* Skip givs that have mult_val == 0, since
		 they are really invariants.  Also skip those that are
		 replaceable, since we know their lifetime doesn't contain
		 any biv update.  */
	      else if (giv->mult_val == const0_rtx || giv->replaceable)
		continue;

	      /* The only way we can allow this giv to derive another
		 is if this is a biv increment and we can form the product
		 of biv->add_val and giv->mult_val.  In this case, we will
		 be able to compute a compensation.  */
	      else if (biv->insn == p)
		{
		  rtx ext_val_dummy;

		  tem = 0;
		  if (biv->mult_val == const1_rtx)
		    tem = simplify_giv_expr (loop,
					     gen_rtx_MULT (giv->mode,
							   biv->add_val,
							   giv->mult_val),
					     &ext_val_dummy, &dummy);

		  if (tem && giv->derive_adjustment)
		    tem = simplify_giv_expr
		      (loop,
		       gen_rtx_PLUS (giv->mode, tem, giv->derive_adjustment),
		       &ext_val_dummy, &dummy);

		  if (tem)
		    giv->derive_adjustment = tem;
		  else
		    giv->cant_derive = 1;
		}
	      else if ((GET_CODE (p) == CODE_LABEL && ! biv->always_computable)
		       || (GET_CODE (p) == JUMP_INSN && biv->maybe_multiple))
		giv->cant_derive = 1;
	    }
	}
}

/* Check whether an insn is an increment legitimate for a basic induction var.
   X is the source of insn P, or a part of it.
   MODE is the mode in which X should be interpreted.

   DEST_REG is the putative biv, also the destination of the insn.
   We accept patterns of these forms:
     REG = REG + INVARIANT (includes REG = REG - CONSTANT)
     REG = INVARIANT + REG

   If X is suitable, we return 1, set *MULT_VAL to CONST1_RTX,
   store the additive term into *INC_VAL, and store the place where
   we found the additive term into *LOCATION.

   If X is an assignment of an invariant into DEST_REG, we set
   *MULT_VAL to CONST0_RTX, and store the invariant into *INC_VAL.

   We also want to detect a BIV when it corresponds to a variable
   whose mode was promoted via PROMOTED_MODE.  In that case, an increment
   of the variable may be a PLUS that adds a SUBREG of that variable to
   an invariant and then sign- or zero-extends the result of the PLUS
   into the variable.

   Most GIVs in such cases will be in the promoted mode, since that is the
   probably the natural computation mode (and almost certainly the mode
   used for addresses) on the machine.  So we view the pseudo-reg containing
   the variable as the BIV, as if it were simply incremented.

   Note that treating the entire pseudo as a BIV will result in making
   simple increments to any GIVs based on it.  However, if the variable
   overflows in its declared mode but not its promoted mode, the result will
   be incorrect.  This is acceptable if the variable is signed, since
   overflows in such cases are undefined, but not if it is unsigned, since
   those overflows are defined.  So we only check for SIGN_EXTEND and
   not ZERO_EXTEND.

   If we cannot find a biv, we return 0.  */

static int
basic_induction_var (loop, x, mode, dest_reg, p, inc_val, mult_val, location)
     const struct loop *loop;
     rtx x;
     enum machine_mode mode;
     rtx dest_reg;
     rtx p;
     rtx *inc_val;
     rtx *mult_val;
     rtx **location;
{
  enum rtx_code code;
  rtx *argp, arg;
  rtx insn, set = 0;

  code = GET_CODE (x);
  *location = NULL;
  switch (code)
    {
    case PLUS:
      if (rtx_equal_p (XEXP (x, 0), dest_reg)
	  || (GET_CODE (XEXP (x, 0)) == SUBREG
	      && SUBREG_PROMOTED_VAR_P (XEXP (x, 0))
	      && SUBREG_REG (XEXP (x, 0)) == dest_reg))
	{
	  argp = &XEXP (x, 1);
	}
      else if (rtx_equal_p (XEXP (x, 1), dest_reg)
	       || (GET_CODE (XEXP (x, 1)) == SUBREG
		   && SUBREG_PROMOTED_VAR_P (XEXP (x, 1))
		   && SUBREG_REG (XEXP (x, 1)) == dest_reg))
	{
	  argp = &XEXP (x, 0);
	}
      else
	return 0;

      arg = *argp;
      if (loop_invariant_p (loop, arg) != 1)
	return 0;

      *inc_val = convert_modes (GET_MODE (dest_reg), GET_MODE (x), arg, 0);
      *mult_val = const1_rtx;
      *location = argp;
      return 1;

    case SUBREG:
      /* If what's inside the SUBREG is a BIV, then the SUBREG.  This will
	 handle addition of promoted variables.
	 ??? The comment at the start of this function is wrong: promoted
	 variable increments don't look like it says they do.  */
      return basic_induction_var (loop, SUBREG_REG (x),
				  GET_MODE (SUBREG_REG (x)),
				  dest_reg, p, inc_val, mult_val, location);

    case REG:
      /* If this register is assigned in a previous insn, look at its
	 source, but don't go outside the loop or past a label.  */

      /* If this sets a register to itself, we would repeat any previous
	 biv increment if we applied this strategy blindly.  */
      if (rtx_equal_p (dest_reg, x))
	return 0;

      insn = p;
      while (1)
	{
	  rtx dest;
	  do
	    {
	      insn = PREV_INSN (insn);
	    }
	  while (insn && GET_CODE (insn) == NOTE
		 && NOTE_LINE_NUMBER (insn) != NOTE_INSN_LOOP_BEG);

	  if (!insn)
	    break;
	  set = single_set (insn);
	  if (set == 0)
	    break;
	  dest = SET_DEST (set);
	  if (dest == x
	      || (GET_CODE (dest) == SUBREG
		  && (GET_MODE_SIZE (GET_MODE (dest)) <= UNITS_PER_WORD)
		  && (GET_MODE_CLASS (GET_MODE (dest)) == MODE_INT)
		  && SUBREG_REG (dest) == x))
	    return basic_induction_var (loop, SET_SRC (set),
					(GET_MODE (SET_SRC (set)) == VOIDmode
					 ? GET_MODE (x)
					 : GET_MODE (SET_SRC (set))),
					dest_reg, insn,
					inc_val, mult_val, location);

	  while (GET_CODE (dest) == SIGN_EXTRACT
		 || GET_CODE (dest) == ZERO_EXTRACT
		 || GET_CODE (dest) == SUBREG
		 || GET_CODE (dest) == STRICT_LOW_PART)
	    dest = XEXP (dest, 0);
	  if (dest == x)
	    break;
	}
      /* Fall through.  */

      /* Can accept constant setting of biv only when inside inner most loop.
	 Otherwise, a biv of an inner loop may be incorrectly recognized
	 as a biv of the outer loop,
	 causing code to be moved INTO the inner loop.  */
    case MEM:
      if (loop_invariant_p (loop, x) != 1)
	return 0;
    case CONST_INT:
    case SYMBOL_REF:
    case CONST:
      /* convert_modes aborts if we try to convert to or from CCmode, so just
         exclude that case.  It is very unlikely that a condition code value
	 would be a useful iterator anyways.  convert_modes aborts if we try to
	 convert a float mode to non-float or vice versa too.  */
      if (loop->level == 1
	  && GET_MODE_CLASS (mode) == GET_MODE_CLASS (GET_MODE (dest_reg))
	  && GET_MODE_CLASS (mode) != MODE_CC)
	{
	  /* Possible bug here?  Perhaps we don't know the mode of X.  */
	  *inc_val = convert_modes (GET_MODE (dest_reg), mode, x, 0);
	  *mult_val = const0_rtx;
	  return 1;
	}
      else
	return 0;

    case SIGN_EXTEND:
      return basic_induction_var (loop, XEXP (x, 0), GET_MODE (XEXP (x, 0)),
				  dest_reg, p, inc_val, mult_val, location);

    case ASHIFTRT:
      /* Similar, since this can be a sign extension.  */
      for (insn = PREV_INSN (p);
	   (insn && GET_CODE (insn) == NOTE
	    && NOTE_LINE_NUMBER (insn) != NOTE_INSN_LOOP_BEG);
	   insn = PREV_INSN (insn))
	;

      if (insn)
	set = single_set (insn);

      if (! rtx_equal_p (dest_reg, XEXP (x, 0))
	  && set && SET_DEST (set) == XEXP (x, 0)
	  && GET_CODE (XEXP (x, 1)) == CONST_INT
	  && INTVAL (XEXP (x, 1)) >= 0
	  && GET_CODE (SET_SRC (set)) == ASHIFT
	  && XEXP (x, 1) == XEXP (SET_SRC (set), 1))
	return basic_induction_var (loop, XEXP (SET_SRC (set), 0),
				    GET_MODE (XEXP (x, 0)),
				    dest_reg, insn, inc_val, mult_val,
				    location);
      return 0;

    default:
      return 0;
    }
}

/* A general induction variable (giv) is any quantity that is a linear
   function   of a basic induction variable,
   i.e. giv = biv * mult_val + add_val.
   The coefficients can be any loop invariant quantity.
   A giv need not be computed directly from the biv;
   it can be computed by way of other givs.  */

/* Determine whether X computes a giv.
   If it does, return a nonzero value
     which is the benefit from eliminating the computation of X;
   set *SRC_REG to the register of the biv that it is computed from;
   set *ADD_VAL and *MULT_VAL to the coefficients,
     such that the value of X is biv * mult + add;  */

static int
general_induction_var (loop, x, src_reg, add_val, mult_val, ext_val,
		       is_addr, pbenefit, addr_mode)
     const struct loop *loop;
     rtx x;
     rtx *src_reg;
     rtx *add_val;
     rtx *mult_val;
     rtx *ext_val;
     int is_addr;
     int *pbenefit;
     enum machine_mode addr_mode;
{
  struct loop_ivs *ivs = LOOP_IVS (loop);
  rtx orig_x = x;

  /* If this is an invariant, forget it, it isn't a giv.  */
  if (loop_invariant_p (loop, x) == 1)
    return 0;

  *pbenefit = 0;
  *ext_val = NULL_RTX;
  x = simplify_giv_expr (loop, x, ext_val, pbenefit);
  if (x == 0)
    return 0;

  switch (GET_CODE (x))
    {
    case USE:
    case CONST_INT:
      /* Since this is now an invariant and wasn't before, it must be a giv
	 with MULT_VAL == 0.  It doesn't matter which BIV we associate this
	 with.  */
      *src_reg = ivs->list->biv->dest_reg;
      *mult_val = const0_rtx;
      *add_val = x;
      break;

    case REG:
      /* This is equivalent to a BIV.  */
      *src_reg = x;
      *mult_val = const1_rtx;
      *add_val = const0_rtx;
      break;

    case PLUS:
      /* Either (plus (biv) (invar)) or
	 (plus (mult (biv) (invar_1)) (invar_2)).  */
      if (GET_CODE (XEXP (x, 0)) == MULT)
	{
	  *src_reg = XEXP (XEXP (x, 0), 0);
	  *mult_val = XEXP (XEXP (x, 0), 1);
	}
      else
	{
	  *src_reg = XEXP (x, 0);
	  *mult_val = const1_rtx;
	}
      *add_val = XEXP (x, 1);
      break;

    case MULT:
      /* ADD_VAL is zero.  */
      *src_reg = XEXP (x, 0);
      *mult_val = XEXP (x, 1);
      *add_val = const0_rtx;
      break;

    default:
      abort ();
    }

  /* Remove any enclosing USE from ADD_VAL and MULT_VAL (there will be
     unless they are CONST_INT).  */
  if (GET_CODE (*add_val) == USE)
    *add_val = XEXP (*add_val, 0);
  if (GET_CODE (*mult_val) == USE)
    *mult_val = XEXP (*mult_val, 0);

  if (is_addr)
    *pbenefit += address_cost (orig_x, addr_mode) - reg_address_cost;
  else
    *pbenefit += rtx_cost (orig_x, SET);

  /* Always return true if this is a giv so it will be detected as such,
     even if the benefit is zero or negative.  This allows elimination
     of bivs that might otherwise not be eliminated.  */
  return 1;
}

/* Given an expression, X, try to form it as a linear function of a biv.
   We will canonicalize it to be of the form
	(plus (mult (BIV) (invar_1))
	      (invar_2))
   with possible degeneracies.

   The invariant expressions must each be of a form that can be used as a
   machine operand.  We surround then with a USE rtx (a hack, but localized
   and certainly unambiguous!) if not a CONST_INT for simplicity in this
   routine; it is the caller's responsibility to strip them.

   If no such canonicalization is possible (i.e., two biv's are used or an
   expression that is neither invariant nor a biv or giv), this routine
   returns 0.

   For a non-zero return, the result will have a code of CONST_INT, USE,
   REG (for a BIV), PLUS, or MULT.  No other codes will occur.

   *BENEFIT will be incremented by the benefit of any sub-giv encountered.  */

static rtx sge_plus PARAMS ((enum machine_mode, rtx, rtx));
static rtx sge_plus_constant PARAMS ((rtx, rtx));

static rtx
simplify_giv_expr (loop, x, ext_val, benefit)
     const struct loop *loop;
     rtx x;
     rtx *ext_val;
     int *benefit;
{
  struct loop_ivs *ivs = LOOP_IVS (loop);
  struct loop_regs *regs = LOOP_REGS (loop);
  enum machine_mode mode = GET_MODE (x);
  rtx arg0, arg1;
  rtx tem;

  /* If this is not an integer mode, or if we cannot do arithmetic in this
     mode, this can't be a giv.  */
  if (mode != VOIDmode
      && (GET_MODE_CLASS (mode) != MODE_INT
	  || GET_MODE_BITSIZE (mode) > HOST_BITS_PER_WIDE_INT))
    return NULL_RTX;

  switch (GET_CODE (x))
    {
    case PLUS:
      arg0 = simplify_giv_expr (loop, XEXP (x, 0), ext_val, benefit);
      arg1 = simplify_giv_expr (loop, XEXP (x, 1), ext_val, benefit);
      if (arg0 == 0 || arg1 == 0)
	return NULL_RTX;

      /* Put constant last, CONST_INT last if both constant.  */
      if ((GET_CODE (arg0) == USE
	   || GET_CODE (arg0) == CONST_INT)
	  && ! ((GET_CODE (arg0) == USE
		 && GET_CODE (arg1) == USE)
		|| GET_CODE (arg1) == CONST_INT))
	tem = arg0, arg0 = arg1, arg1 = tem;

      /* Handle addition of zero, then addition of an invariant.  */
      if (arg1 == const0_rtx)
	return arg0;
      else if (GET_CODE (arg1) == CONST_INT || GET_CODE (arg1) == USE)
	switch (GET_CODE (arg0))
	  {
	  case CONST_INT:
	  case USE:
	    /* Adding two invariants must result in an invariant, so enclose
	       addition operation inside a USE and return it.  */
	    if (GET_CODE (arg0) == USE)
	      arg0 = XEXP (arg0, 0);
	    if (GET_CODE (arg1) == USE)
	      arg1 = XEXP (arg1, 0);

	    if (GET_CODE (arg0) == CONST_INT)
	      tem = arg0, arg0 = arg1, arg1 = tem;
	    if (GET_CODE (arg1) == CONST_INT)
	      tem = sge_plus_constant (arg0, arg1);
	    else
	      tem = sge_plus (mode, arg0, arg1);

	    if (GET_CODE (tem) != CONST_INT)
	      tem = gen_rtx_USE (mode, tem);
	    return tem;

	  case REG:
	  case MULT:
	    /* biv + invar or mult + invar.  Return sum.  */
	    return gen_rtx_PLUS (mode, arg0, arg1);

	  case PLUS:
	    /* (a + invar_1) + invar_2.  Associate.  */
	    return
	      simplify_giv_expr (loop,
				 gen_rtx_PLUS (mode,
					       XEXP (arg0, 0),
					       gen_rtx_PLUS (mode,
							     XEXP (arg0, 1),
							     arg1)),
				 ext_val, benefit);

	  default:
	    abort ();
	  }

      /* Each argument must be either REG, PLUS, or MULT.  Convert REG to
	 MULT to reduce cases.  */
      if (GET_CODE (arg0) == REG)
	arg0 = gen_rtx_MULT (mode, arg0, const1_rtx);
      if (GET_CODE (arg1) == REG)
	arg1 = gen_rtx_MULT (mode, arg1, const1_rtx);

      /* Now have PLUS + PLUS, PLUS + MULT, MULT + PLUS, or MULT + MULT.
	 Put a MULT first, leaving PLUS + PLUS, MULT + PLUS, or MULT + MULT.
	 Recurse to associate the second PLUS.  */
      if (GET_CODE (arg1) == MULT)
	tem = arg0, arg0 = arg1, arg1 = tem;

      if (GET_CODE (arg1) == PLUS)
	return
	  simplify_giv_expr (loop,
			     gen_rtx_PLUS (mode,
					   gen_rtx_PLUS (mode, arg0,
							 XEXP (arg1, 0)),
					   XEXP (arg1, 1)),
			     ext_val, benefit);

      /* Now must have MULT + MULT.  Distribute if same biv, else not giv.  */
      if (GET_CODE (arg0) != MULT || GET_CODE (arg1) != MULT)
	return NULL_RTX;

      if (!rtx_equal_p (arg0, arg1))
	return NULL_RTX;

      return simplify_giv_expr (loop,
				gen_rtx_MULT (mode,
					      XEXP (arg0, 0),
					      gen_rtx_PLUS (mode,
							    XEXP (arg0, 1),
							    XEXP (arg1, 1))),
				ext_val, benefit);

    case MINUS:
      /* Handle "a - b" as "a + b * (-1)".  */
      return simplify_giv_expr (loop,
				gen_rtx_PLUS (mode,
					      XEXP (x, 0),
					      gen_rtx_MULT (mode,
							    XEXP (x, 1),
							    constm1_rtx)),
				ext_val, benefit);

    case MULT:
      arg0 = simplify_giv_expr (loop, XEXP (x, 0), ext_val, benefit);
      arg1 = simplify_giv_expr (loop, XEXP (x, 1), ext_val, benefit);
      if (arg0 == 0 || arg1 == 0)
	return NULL_RTX;

      /* Put constant last, CONST_INT last if both constant.  */
      if ((GET_CODE (arg0) == USE || GET_CODE (arg0) == CONST_INT)
	  && GET_CODE (arg1) != CONST_INT)
	tem = arg0, arg0 = arg1, arg1 = tem;

      /* If second argument is not now constant, not giv.  */
      if (GET_CODE (arg1) != USE && GET_CODE (arg1) != CONST_INT)
	return NULL_RTX;

      /* Handle multiply by 0 or 1.  */
      if (arg1 == const0_rtx)
	return const0_rtx;

      else if (arg1 == const1_rtx)
	return arg0;

      switch (GET_CODE (arg0))
	{
	case REG:
	  /* biv * invar.  Done.  */
	  return gen_rtx_MULT (mode, arg0, arg1);

	case CONST_INT:
	  /* Product of two constants.  */
	  return GEN_INT (INTVAL (arg0) * INTVAL (arg1));

	case USE:
	  /* invar * invar is a giv, but attempt to simplify it somehow.  */
	  if (GET_CODE (arg1) != CONST_INT)
	    return NULL_RTX;

	  arg0 = XEXP (arg0, 0);
	  if (GET_CODE (arg0) == MULT)
	    {
	      /* (invar_0 * invar_1) * invar_2.  Associate.  */
	      return simplify_giv_expr (loop,
					gen_rtx_MULT (mode,
						      XEXP (arg0, 0),
						      gen_rtx_MULT (mode,
								    XEXP (arg0,
									  1),
								    arg1)),
					ext_val, benefit);
	    }
	  /* Porpagate the MULT expressions to the intermost nodes.  */
	  else if (GET_CODE (arg0) == PLUS)
	    {
	      /* (invar_0 + invar_1) * invar_2.  Distribute.  */
	      return simplify_giv_expr (loop,
					gen_rtx_PLUS (mode,
						      gen_rtx_MULT (mode,
								    XEXP (arg0,
									  0),
								    arg1),
						      gen_rtx_MULT (mode,
								    XEXP (arg0,
									  1),
								    arg1)),
					ext_val, benefit);
	    }
	  return gen_rtx_USE (mode, gen_rtx_MULT (mode, arg0, arg1));

	case MULT:
	  /* (a * invar_1) * invar_2.  Associate.  */
	  return simplify_giv_expr (loop,
				    gen_rtx_MULT (mode,
						  XEXP (arg0, 0),
						  gen_rtx_MULT (mode,
								XEXP (arg0, 1),
								arg1)),
				    ext_val, benefit);

	case PLUS:
	  /* (a + invar_1) * invar_2.  Distribute.  */
	  return simplify_giv_expr (loop,
				    gen_rtx_PLUS (mode,
						  gen_rtx_MULT (mode,
								XEXP (arg0, 0),
								arg1),
						  gen_rtx_MULT (mode,
								XEXP (arg0, 1),
								arg1)),
				    ext_val, benefit);

	default:
	  abort ();
	}

    case ASHIFT:
      /* Shift by constant is multiply by power of two.  */
      if (GET_CODE (XEXP (x, 1)) != CONST_INT)
	return 0;

      return
	simplify_giv_expr (loop,
			   gen_rtx_MULT (mode,
					 XEXP (x, 0),
					 GEN_INT ((HOST_WIDE_INT) 1
						  << INTVAL (XEXP (x, 1)))),
			   ext_val, benefit);

    case NEG:
      /* "-a" is "a * (-1)" */
      return simplify_giv_expr (loop,
				gen_rtx_MULT (mode, XEXP (x, 0), constm1_rtx),
				ext_val, benefit);

    case NOT:
      /* "~a" is "-a - 1". Silly, but easy.  */
      return simplify_giv_expr (loop,
				gen_rtx_MINUS (mode,
					       gen_rtx_NEG (mode, XEXP (x, 0)),
					       const1_rtx),
				ext_val, benefit);

    case USE:
      /* Already in proper form for invariant.  */
      return x;

    case SIGN_EXTEND:
    case ZERO_EXTEND:
    case TRUNCATE:
      /* Conditionally recognize extensions of simple IVs.  After we've
	 computed loop traversal counts and verified the range of the
	 source IV, we'll reevaluate this as a GIV.  */
      if (*ext_val == NULL_RTX)
	{
	  arg0 = simplify_giv_expr (loop, XEXP (x, 0), ext_val, benefit);
	  if (arg0 && *ext_val == NULL_RTX && GET_CODE (arg0) == REG)
	    {
	      *ext_val = gen_rtx_fmt_e (GET_CODE (x), mode, arg0);
	      return arg0;
	    }
	}
      goto do_default;

    case REG:
      /* If this is a new register, we can't deal with it.  */
      if (REGNO (x) >= max_reg_before_loop)
	return 0;

      /* Check for biv or giv.  */
      switch (REG_IV_TYPE (ivs, REGNO (x)))
	{
	case BASIC_INDUCT:
	  return x;
	case GENERAL_INDUCT:
	  {
	    struct induction *v = REG_IV_INFO (ivs, REGNO (x));

	    /* Form expression from giv and add benefit.  Ensure this giv
	       can derive another and subtract any needed adjustment if so.  */

	    /* Increasing the benefit here is risky.  The only case in which it
	       is arguably correct is if this is the only use of V.  In other
	       cases, this will artificially inflate the benefit of the current
	       giv, and lead to suboptimal code.  Thus, it is disabled, since
	       potentially not reducing an only marginally beneficial giv is
	       less harmful than reducing many givs that are not really
	       beneficial.  */
	    {
	      rtx single_use = regs->array[REGNO (x)].single_usage;
	      if (single_use && single_use != const0_rtx)
		*benefit += v->benefit;
	    }

	    if (v->cant_derive)
	      return 0;

	    tem = gen_rtx_PLUS (mode, gen_rtx_MULT (mode,
						    v->src_reg, v->mult_val),
				v->add_val);

	    if (v->derive_adjustment)
	      tem = gen_rtx_MINUS (mode, tem, v->derive_adjustment);
	    arg0 = simplify_giv_expr (loop, tem, ext_val, benefit);
	    if (*ext_val)
	      {
		if (!v->ext_dependent)
		  return arg0;
	      }
	    else
	      {
		*ext_val = v->ext_dependent;
		return arg0;
	      }
	    return 0;
	  }

	default:
	do_default:
	  /* If it isn't an induction variable, and it is invariant, we
	     may be able to simplify things further by looking through
	     the bits we just moved outside the loop.  */
	  if (loop_invariant_p (loop, x) == 1)
	    {
	      struct movable *m;
	      struct loop_movables *movables = LOOP_MOVABLES (loop);

	      for (m = movables->head; m; m = m->next)
		if (rtx_equal_p (x, m->set_dest))
		  {
		    /* Ok, we found a match.  Substitute and simplify.  */

		    /* If we match another movable, we must use that, as
		       this one is going away.  */
		    if (m->match)
		      return simplify_giv_expr (loop, m->match->set_dest,
						ext_val, benefit);

		    /* If consec is non-zero, this is a member of a group of
		       instructions that were moved together.  We handle this
		       case only to the point of seeking to the last insn and
		       looking for a REG_EQUAL.  Fail if we don't find one.  */
		    if (m->consec != 0)
		      {
			int i = m->consec;
			tem = m->insn;
			do
			  {
			    tem = NEXT_INSN (tem);
			  }
			while (--i > 0);

			tem = find_reg_note (tem, REG_EQUAL, NULL_RTX);
			if (tem)
			  tem = XEXP (tem, 0);
		      }
		    else
		      {
			tem = single_set (m->insn);
			if (tem)
			  tem = SET_SRC (tem);
		      }

		    if (tem)
		      {
			/* What we are most interested in is pointer
			   arithmetic on invariants -- only take
			   patterns we may be able to do something with.  */
			if (GET_CODE (tem) == PLUS
			    || GET_CODE (tem) == MULT
			    || GET_CODE (tem) == ASHIFT
			    || GET_CODE (tem) == CONST_INT
			    || GET_CODE (tem) == SYMBOL_REF)
			  {
			    tem = simplify_giv_expr (loop, tem, ext_val,
						     benefit);
			    if (tem)
			      return tem;
			  }
			else if (GET_CODE (tem) == CONST
				 && GET_CODE (XEXP (tem, 0)) == PLUS
				 && GET_CODE (XEXP (XEXP (tem, 0), 0)) == SYMBOL_REF
				 && GET_CODE (XEXP (XEXP (tem, 0), 1)) == CONST_INT)
			  {
			    tem = simplify_giv_expr (loop, XEXP (tem, 0),
						     ext_val, benefit);
			    if (tem)
			      return tem;
			  }
		      }
		    break;
		  }
	    }
	  break;
	}

      /* Fall through to general case.  */
    default:
      /* If invariant, return as USE (unless CONST_INT).
	 Otherwise, not giv.  */
      if (GET_CODE (x) == USE)
	x = XEXP (x, 0);

      if (loop_invariant_p (loop, x) == 1)
	{
	  if (GET_CODE (x) == CONST_INT)
	    return x;
	  if (GET_CODE (x) == CONST
	      && GET_CODE (XEXP (x, 0)) == PLUS
	      && GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF
	      && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT)
	    x = XEXP (x, 0);
	  return gen_rtx_USE (mode, x);
	}
      else
	return 0;
    }
}

/* This routine folds invariants such that there is only ever one
   CONST_INT in the summation.  It is only used by simplify_giv_expr.  */

static rtx
sge_plus_constant (x, c)
     rtx x, c;
{
  if (GET_CODE (x) == CONST_INT)
    return GEN_INT (INTVAL (x) + INTVAL (c));
  else if (GET_CODE (x) != PLUS)
    return gen_rtx_PLUS (GET_MODE (x), x, c);
  else if (GET_CODE (XEXP (x, 1)) == CONST_INT)
    {
      return gen_rtx_PLUS (GET_MODE (x), XEXP (x, 0),
			   GEN_INT (INTVAL (XEXP (x, 1)) + INTVAL (c)));
    }
  else if (GET_CODE (XEXP (x, 0)) == PLUS
	   || GET_CODE (XEXP (x, 1)) != PLUS)
    {
      return gen_rtx_PLUS (GET_MODE (x),
			   sge_plus_constant (XEXP (x, 0), c), XEXP (x, 1));
    }
  else
    {
      return gen_rtx_PLUS (GET_MODE (x),
			   sge_plus_constant (XEXP (x, 1), c), XEXP (x, 0));
    }
}

static rtx
sge_plus (mode, x, y)
     enum machine_mode mode;
     rtx x, y;
{
  while (GET_CODE (y) == PLUS)
    {
      rtx a = XEXP (y, 0);
      if (GET_CODE (a) == CONST_INT)
	x = sge_plus_constant (x, a);
      else
	x = gen_rtx_PLUS (mode, x, a);
      y = XEXP (y, 1);
    }
  if (GET_CODE (y) == CONST_INT)
    x = sge_plus_constant (x, y);
  else
    x = gen_rtx_PLUS (mode, x, y);
  return x;
}

/* Help detect a giv that is calculated by several consecutive insns;
   for example,
      giv = biv * M
      giv = giv + A
   The caller has already identified the first insn P as having a giv as dest;
   we check that all other insns that set the same register follow
   immediately after P, that they alter nothing else,
   and that the result of the last is still a giv.

   The value is 0 if the reg set in P is not really a giv.
   Otherwise, the value is the amount gained by eliminating
   all the consecutive insns that compute the value.

   FIRST_BENEFIT is the amount gained by eliminating the first insn, P.
   SRC_REG is the reg of the biv; DEST_REG is the reg of the giv.

   The coefficients of the ultimate giv value are stored in
   *MULT_VAL and *ADD_VAL.  */

static int
consec_sets_giv (loop, first_benefit, p, src_reg, dest_reg,
		 add_val, mult_val, ext_val, last_consec_insn)
     const struct loop *loop;
     int first_benefit;
     rtx p;
     rtx src_reg;
     rtx dest_reg;
     rtx *add_val;
     rtx *mult_val;
     rtx *ext_val;
     rtx *last_consec_insn;
{
  struct loop_ivs *ivs = LOOP_IVS (loop);
  struct loop_regs *regs = LOOP_REGS (loop);
  int count;
  enum rtx_code code;
  int benefit;
  rtx temp;
  rtx set;

  /* Indicate that this is a giv so that we can update the value produced in
     each insn of the multi-insn sequence.

     This induction structure will be used only by the call to
     general_induction_var below, so we can allocate it on our stack.
     If this is a giv, our caller will replace the induct var entry with
     a new induction structure.  */
  struct induction *v;

  if (REG_IV_TYPE (ivs, REGNO (dest_reg)) != UNKNOWN_INDUCT)
    return 0;

  v = (struct induction *) alloca (sizeof (struct induction));
  v->src_reg = src_reg;
  v->mult_val = *mult_val;
  v->add_val = *add_val;
  v->benefit = first_benefit;
  v->cant_derive = 0;
  v->derive_adjustment = 0;
  v->ext_dependent = NULL_RTX;

  REG_IV_TYPE (ivs, REGNO (dest_reg)) = GENERAL_INDUCT;
  REG_IV_INFO (ivs, REGNO (dest_reg)) = v;

  count = regs->array[REGNO (dest_reg)].n_times_set - 1;

  while (count > 0)
    {
      p = NEXT_INSN (p);
      code = GET_CODE (p);

      /* If libcall, skip to end of call sequence.  */
      if (code == INSN && (temp = find_reg_note (p, REG_LIBCALL, NULL_RTX)))
	p = XEXP (temp, 0);

      if (code == INSN
	  && (set = single_set (p))
	  && GET_CODE (SET_DEST (set)) == REG
	  && SET_DEST (set) == dest_reg
	  && (general_induction_var (loop, SET_SRC (set), &src_reg,
				     add_val, mult_val, ext_val, 0,
				     &benefit, VOIDmode)
	      /* Giv created by equivalent expression.  */
	      || ((temp = find_reg_note (p, REG_EQUAL, NULL_RTX))
		  && general_induction_var (loop, XEXP (temp, 0), &src_reg,
					    add_val, mult_val, ext_val, 0,
					    &benefit, VOIDmode)))
	  && src_reg == v->src_reg)
	{
	  if (find_reg_note (p, REG_RETVAL, NULL_RTX))
	    benefit += libcall_benefit (p);

	  count--;
	  v->mult_val = *mult_val;
	  v->add_val = *add_val;
	  v->benefit += benefit;
	}
      else if (code != NOTE)
	{
	  /* Allow insns that set something other than this giv to a
	     constant.  Such insns are needed on machines which cannot
	     include long constants and should not disqualify a giv.  */
	  if (code == INSN
	      && (set = single_set (p))
	      && SET_DEST (set) != dest_reg
	      && CONSTANT_P (SET_SRC (set)))
	    continue;

	  REG_IV_TYPE (ivs, REGNO (dest_reg)) = UNKNOWN_INDUCT;
	  return 0;
	}
    }

  REG_IV_TYPE (ivs, REGNO (dest_reg)) = UNKNOWN_INDUCT;
  *last_consec_insn = p;
  return v->benefit;
}

/* Return an rtx, if any, that expresses giv G2 as a function of the register
   represented by G1.  If no such expression can be found, or it is clear that
   it cannot possibly be a valid address, 0 is returned.

   To perform the computation, we note that
	G1 = x * v + a		and
	G2 = y * v + b
   where `v' is the biv.

   So G2 = (y/b) * G1 + (b - a*y/x).

   Note that MULT = y/x.

   Update: A and B are now allowed to be additive expressions such that
   B contains all variables in A.  That is, computing B-A will not require
   subtracting variables.  */

static rtx
express_from_1 (a, b, mult)
     rtx a, b, mult;
{
  /* If MULT is zero, then A*MULT is zero, and our expression is B.  */

  if (mult == const0_rtx)
    return b;

  /* If MULT is not 1, we cannot handle A with non-constants, since we
     would then be required to subtract multiples of the registers in A.
     This is theoretically possible, and may even apply to some Fortran
     constructs, but it is a lot of work and we do not attempt it here.  */

  if (mult != const1_rtx && GET_CODE (a) != CONST_INT)
    return NULL_RTX;

  /* In general these structures are sorted top to bottom (down the PLUS
     chain), but not left to right across the PLUS.  If B is a higher
     order giv than A, we can strip one level and recurse.  If A is higher
     order, we'll eventually bail out, but won't know that until the end.
     If they are the same, we'll strip one level around this loop.  */

  while (GET_CODE (a) == PLUS && GET_CODE (b) == PLUS)
    {
      rtx ra, rb, oa, ob, tmp;

      ra = XEXP (a, 0), oa = XEXP (a, 1);
      if (GET_CODE (ra) == PLUS)
	tmp = ra, ra = oa, oa = tmp;

      rb = XEXP (b, 0), ob = XEXP (b, 1);
      if (GET_CODE (rb) == PLUS)
	tmp = rb, rb = ob, ob = tmp;

      if (rtx_equal_p (ra, rb))
	/* We matched: remove one reg completely.  */
	a = oa, b = ob;
      else if (GET_CODE (ob) != PLUS && rtx_equal_p (ra, ob))
	/* An alternate match.  */
	a = oa, b = rb;
      else if (GET_CODE (oa) != PLUS && rtx_equal_p (oa, rb))
	/* An alternate match.  */
	a = ra, b = ob;
      else
	{
	  /* Indicates an extra register in B.  Strip one level from B and
	     recurse, hoping B was the higher order expression.  */
	  ob = express_from_1 (a, ob, mult);
	  if (ob == NULL_RTX)
	    return NULL_RTX;
	  return gen_rtx_PLUS (GET_MODE (b), rb, ob);
	}
    }

  /* Here we are at the last level of A, go through the cases hoping to
     get rid of everything but a constant.  */

  if (GET_CODE (a) == PLUS)
    {
      rtx ra, oa;

      ra = XEXP (a, 0), oa = XEXP (a, 1);
      if (rtx_equal_p (oa, b))
	oa = ra;
      else if (!rtx_equal_p (ra, b))
	return NULL_RTX;

      if (GET_CODE (oa) != CONST_INT)
	return NULL_RTX;

      return GEN_INT (-INTVAL (oa) * INTVAL (mult));
    }
  else if (GET_CODE (a) == CONST_INT)
    {
      return plus_constant (b, -INTVAL (a) * INTVAL (mult));
    }
  else if (CONSTANT_P (a))
    {
      enum machine_mode mode_a = GET_MODE (a);
      enum machine_mode mode_b = GET_MODE (b);
      enum machine_mode mode = mode_b == VOIDmode ? mode_a : mode_b;
      return simplify_gen_binary (MINUS, mode, b, a);
    }
  else if (GET_CODE (b) == PLUS)
    {
      if (rtx_equal_p (a, XEXP (b, 0)))
	return XEXP (b, 1);
      else if (rtx_equal_p (a, XEXP (b, 1)))
	return XEXP (b, 0);
      else
	return NULL_RTX;
    }
  else if (rtx_equal_p (a, b))
    return const0_rtx;

  return NULL_RTX;
}

rtx
express_from (g1, g2)
     struct induction *g1, *g2;
{
  rtx mult, add;

  /* The value that G1 will be multiplied by must be a constant integer.  Also,
     the only chance we have of getting a valid address is if b*c/a (see above
     for notation) is also an integer.  */
  if (GET_CODE (g1->mult_val) == CONST_INT
      && GET_CODE (g2->mult_val) == CONST_INT)
    {
      if (g1->mult_val == const0_rtx
	  || INTVAL (g2->mult_val) % INTVAL (g1->mult_val) != 0)
	return NULL_RTX;
      mult = GEN_INT (INTVAL (g2->mult_val) / INTVAL (g1->mult_val));
    }
  else if (rtx_equal_p (g1->mult_val, g2->mult_val))
    mult = const1_rtx;
  else
    {
      /* ??? Find out if the one is a multiple of the other?  */
      return NULL_RTX;
    }

  add = express_from_1 (g1->add_val, g2->add_val, mult);
  if (add == NULL_RTX)
    {
      /* Failed.  If we've got a multiplication factor between G1 and G2,
	 scale G1's addend and try again.  */
      if (INTVAL (mult) > 1)
	{
	  rtx g1_add_val = g1->add_val;
	  if (GET_CODE (g1_add_val) == MULT
	      && GET_CODE (XEXP (g1_add_val, 1)) == CONST_INT)
	    {
	      HOST_WIDE_INT m;
	      m = INTVAL (mult) * INTVAL (XEXP (g1_add_val, 1));
	      g1_add_val = gen_rtx_MULT (GET_MODE (g1_add_val),
					 XEXP (g1_add_val, 0), GEN_INT (m));
	    }
	  else
	    {
	      g1_add_val = gen_rtx_MULT (GET_MODE (g1_add_val), g1_add_val,
					 mult);
	    }

	  add = express_from_1 (g1_add_val, g2->add_val, const1_rtx);
	}
    }
  if (add == NULL_RTX)
    return NULL_RTX;

  /* Form simplified final result.  */
  if (mult == const0_rtx)
    return add;
  else if (mult == const1_rtx)
    mult = g1->dest_reg;
  else
    mult = gen_rtx_MULT (g2->mode, g1->dest_reg, mult);

  if (add == const0_rtx)
    return mult;
  else
    {
      if (GET_CODE (add) == PLUS
	  && CONSTANT_P (XEXP (add, 1)))
	{
	  rtx tem = XEXP (add, 1);
	  mult = gen_rtx_PLUS (g2->mode, mult, XEXP (add, 0));
	  add = tem;
	}

      return gen_rtx_PLUS (g2->mode, mult, add);
    }
}

/* Return an rtx, if any, that expresses giv G2 as a function of the register
   represented by G1.  This indicates that G2 should be combined with G1 and
   that G2 can use (either directly or via an address expression) a register
   used to represent G1.  */

static rtx
combine_givs_p (g1, g2)
     struct induction *g1, *g2;
{
  rtx comb, ret;

  /* With the introduction of ext dependent givs, we must care for modes.
     G2 must not use a wider mode than G1.  */
  if (GET_MODE_SIZE (g1->mode) < GET_MODE_SIZE (g2->mode))
    return NULL_RTX;

  ret = comb = express_from (g1, g2);
  if (comb == NULL_RTX)
    return NULL_RTX;
  if (g1->mode != g2->mode)
    ret = gen_lowpart (g2->mode, comb);

  /* If these givs are identical, they can be combined.  We use the results
     of express_from because the addends are not in a canonical form, so
     rtx_equal_p is a weaker test.  */
  /* But don't combine a DEST_REG giv with a DEST_ADDR giv; we want the
     combination to be the other way round.  */
  if (comb == g1->dest_reg
      && (g1->giv_type == DEST_REG || g2->giv_type == DEST_ADDR))
    {
      return ret;
    }

  /* If G2 can be expressed as a function of G1 and that function is valid
     as an address and no more expensive than using a register for G2,
     the expression of G2 in terms of G1 can be used.  */
  if (ret != NULL_RTX
      && g2->giv_type == DEST_ADDR
      && memory_address_p (GET_MODE (g2->mem), ret)
      /* ??? Looses, especially with -fforce-addr, where *g2->location
	 will always be a register, and so anything more complicated
	 gets discarded.  */
#if 0
#ifdef ADDRESS_COST
      && ADDRESS_COST (tem) <= ADDRESS_COST (*g2->location)
#else
      && rtx_cost (tem, MEM) <= rtx_cost (*g2->location, MEM)
#endif
#endif
      )
    {
      return ret;
    }

  return NULL_RTX;
}

/* Check each extension dependent giv in this class to see if its
   root biv is safe from wrapping in the interior mode, which would
   make the giv illegal.  */

static void
check_ext_dependent_givs (bl, loop_info)
     struct iv_class *bl;
     struct loop_info *loop_info;
{
  int ze_ok = 0, se_ok = 0, info_ok = 0;
  enum machine_mode biv_mode = GET_MODE (bl->biv->src_reg);
  HOST_WIDE_INT start_val;
  unsigned HOST_WIDE_INT u_end_val = 0;
  unsigned HOST_WIDE_INT u_start_val = 0;
  rtx incr = pc_rtx;
  struct induction *v;

  /* Make sure the iteration data is available.  We must have
     constants in order to be certain of no overflow.  */
  /* ??? An unknown iteration count with an increment of +-1
     combined with friendly exit tests of against an invariant
     value is also ameanable to optimization.  Not implemented.  */
  if (loop_info->n_iterations > 0
      && bl->initial_value
      && GET_CODE (bl->initial_value) == CONST_INT
      && (incr = biv_total_increment (bl))
      && GET_CODE (incr) == CONST_INT
      /* Make sure the host can represent the arithmetic.  */
      && HOST_BITS_PER_WIDE_INT >= GET_MODE_BITSIZE (biv_mode))
    {
      unsigned HOST_WIDE_INT abs_incr, total_incr;
      HOST_WIDE_INT s_end_val;
      int neg_incr;

      info_ok = 1;
      start_val = INTVAL (bl->initial_value);
      u_start_val = start_val;

      neg_incr = 0, abs_incr = INTVAL (incr);
      if (INTVAL (incr) < 0)
	neg_incr = 1, abs_incr = -abs_incr;
      total_incr = abs_incr * loop_info->n_iterations;

      /* Check for host arithmatic overflow.  */
      if (total_incr / loop_info->n_iterations == abs_incr)
	{
	  unsigned HOST_WIDE_INT u_max;
	  HOST_WIDE_INT s_max;

	  u_end_val = start_val + (neg_incr ? -total_incr : total_incr);
	  s_end_val = u_end_val;
	  u_max = GET_MODE_MASK (biv_mode);
	  s_max = u_max >> 1;

	  /* Check zero extension of biv ok.  */
	  if (start_val >= 0
	      /* Check for host arithmatic overflow.  */
	      && (neg_incr
		  ? u_end_val < u_start_val
		  : u_end_val > u_start_val)
	      /* Check for target arithmetic overflow.  */
	      && (neg_incr
		  ? 1 /* taken care of with host overflow */
		  : u_end_val <= u_max))
	    {
	      ze_ok = 1;
	    }

	  /* Check sign extension of biv ok.  */
	  /* ??? While it is true that overflow with signed and pointer
	     arithmetic is undefined, I fear too many programmers don't
	     keep this fact in mind -- myself included on occasion.
	     So leave alone with the signed overflow optimizations.  */
	  if (start_val >= -s_max - 1
	      /* Check for host arithmatic overflow.  */
	      && (neg_incr
		  ? s_end_val < start_val
		  : s_end_val > start_val)
	      /* Check for target arithmetic overflow.  */
	      && (neg_incr
		  ? s_end_val >= -s_max - 1
		  : s_end_val <= s_max))
	    {
	      se_ok = 1;
	    }
	}
    }

  /* Invalidate givs that fail the tests.  */
  for (v = bl->giv; v; v = v->next_iv)
    if (v->ext_dependent)
      {
	enum rtx_code code = GET_CODE (v->ext_dependent);
	int ok = 0;

	switch (code)
	  {
	  case SIGN_EXTEND:
	    ok = se_ok;
	    break;
	  case ZERO_EXTEND:
	    ok = ze_ok;
	    break;

	  case TRUNCATE:
	    /* We don't know whether this value is being used as either
	       signed or unsigned, so to safely truncate we must satisfy
	       both.  The initial check here verifies the BIV itself;
	       once that is successful we may check its range wrt the
	       derived GIV.  */
	    if (se_ok && ze_ok)
	      {
		enum machine_mode outer_mode = GET_MODE (v->ext_dependent);
		unsigned HOST_WIDE_INT max = GET_MODE_MASK (outer_mode) >> 1;

		/* We know from the above that both endpoints are nonnegative,
		   and that there is no wrapping.  Verify that both endpoints
		   are within the (signed) range of the outer mode.  */
		if (u_start_val <= max && u_end_val <= max)
		  ok = 1;
	      }
	    break;

	  default:
	    abort ();
	  }

	if (ok)
	  {
	    if (loop_dump_stream)
	      {
		fprintf (loop_dump_stream,
			 "Verified ext dependent giv at %d of reg %d\n",
			 INSN_UID (v->insn), bl->regno);
	      }
	  }
	else
	  {
	    if (loop_dump_stream)
	      {
		const char *why;

		if (info_ok)
		  why = "biv iteration values overflowed";
		else
		  {
		    if (incr == pc_rtx)
		      incr = biv_total_increment (bl);
		    if (incr == const1_rtx)
		      why = "biv iteration info incomplete; incr by 1";
		    else
		      why = "biv iteration info incomplete";
		  }

		fprintf (loop_dump_stream,
			 "Failed ext dependent giv at %d, %s\n",
			 INSN_UID (v->insn), why);
	      }
	    v->ignore = 1;
	    bl->all_reduced = 0;
	  }
      }
}

/* Generate a version of VALUE in a mode appropriate for initializing V.  */

rtx
extend_value_for_giv (v, value)
     struct induction *v;
     rtx value;
{
  rtx ext_dep = v->ext_dependent;

  if (! ext_dep)
    return value;

  /* Recall that check_ext_dependent_givs verified that the known bounds
     of a biv did not overflow or wrap with respect to the extension for
     the giv.  Therefore, constants need no additional adjustment.  */
  if (CONSTANT_P (value) && GET_MODE (value) == VOIDmode)
    return value;

  /* Otherwise, we must adjust the value to compensate for the
     differing modes of the biv and the giv.  */
  return gen_rtx_fmt_e (GET_CODE (ext_dep), GET_MODE (ext_dep), value);
}

struct combine_givs_stats
{
  int giv_number;
  int total_benefit;
};

static int
cmp_combine_givs_stats (xp, yp)
     const PTR xp;
     const PTR yp;
{
  const struct combine_givs_stats * const x =
    (const struct combine_givs_stats *) xp;
  const struct combine_givs_stats * const y =
    (const struct combine_givs_stats *) yp;
  int d;
  d = y->total_benefit - x->total_benefit;
  /* Stabilize the sort.  */
  if (!d)
    d = x->giv_number - y->giv_number;
  return d;
}

/* Check all pairs of givs for iv_class BL and see if any can be combined with
   any other.  If so, point SAME to the giv combined with and set NEW_REG to
   be an expression (in terms of the other giv's DEST_REG) equivalent to the
   giv.  Also, update BENEFIT and related fields for cost/benefit analysis.  */

static void
combine_givs (regs, bl)
     struct loop_regs *regs;
     struct iv_class *bl;
{
  /* Additional benefit to add for being combined multiple times.  */
  const int extra_benefit = 3;

  struct induction *g1, *g2, **giv_array;
  int i, j, k, giv_count;
  struct combine_givs_stats *stats;
  rtx *can_combine;

  /* Count givs, because bl->giv_count is incorrect here.  */
  giv_count = 0;
  for (g1 = bl->giv; g1; g1 = g1->next_iv)
    if (!g1->ignore)
      giv_count++;

  giv_array
    = (struct induction **) alloca (giv_count * sizeof (struct induction *));
  i = 0;
  for (g1 = bl->giv; g1; g1 = g1->next_iv)
    if (!g1->ignore)
      giv_array[i++] = g1;

  stats = (struct combine_givs_stats *) xcalloc (giv_count, sizeof (*stats));
  can_combine = (rtx *) xcalloc (giv_count, giv_count * sizeof (rtx));

  for (i = 0; i < giv_count; i++)
    {
      int this_benefit;
      rtx single_use;

      g1 = giv_array[i];
      stats[i].giv_number = i;

      /* If a DEST_REG GIV is used only once, do not allow it to combine
	 with anything, for in doing so we will gain nothing that cannot
	 be had by simply letting the GIV with which we would have combined
	 to be reduced on its own.  The losage shows up in particular with
	 DEST_ADDR targets on hosts with reg+reg addressing, though it can
	 be seen elsewhere as well.  */
      if (g1->giv_type == DEST_REG
	  && (single_use = regs->array[REGNO (g1->dest_reg)].single_usage)
	  && single_use != const0_rtx)
	continue;

      this_benefit = g1->benefit;
      /* Add an additional weight for zero addends.  */
      if (g1->no_const_addval)
	this_benefit += 1;

      for (j = 0; j < giv_count; j++)
	{
	  rtx this_combine;

	  g2 = giv_array[j];
	  if (g1 != g2
	      && (this_combine = combine_givs_p (g1, g2)) != NULL_RTX)
	    {
	      can_combine[i * giv_count + j] = this_combine;
	      this_benefit += g2->benefit + extra_benefit;
	    }
	}
      stats[i].total_benefit = this_benefit;
    }

  /* Iterate, combining until we can't.  */
restart:
  qsort (stats, giv_count, sizeof (*stats), cmp_combine_givs_stats);

  if (loop_dump_stream)
    {
      fprintf (loop_dump_stream, "Sorted combine statistics:\n");
      for (k = 0; k < giv_count; k++)
	{
	  g1 = giv_array[stats[k].giv_number];
	  if (!g1->combined_with && !g1->same)
	    fprintf (loop_dump_stream, " {%d, %d}",
		     INSN_UID (giv_array[stats[k].giv_number]->insn),
		     stats[k].total_benefit);
	}
      putc ('\n', loop_dump_stream);
    }

  for (k = 0; k < giv_count; k++)
    {
      int g1_add_benefit = 0;

      i = stats[k].giv_number;
      g1 = giv_array[i];

      /* If it has already been combined, skip.  */
      if (g1->combined_with || g1->same)
	continue;

      for (j = 0; j < giv_count; j++)
	{
	  g2 = giv_array[j];
	  if (g1 != g2 && can_combine[i * giv_count + j]
	      /* If it has already been combined, skip.  */
	      && ! g2->same && ! g2->combined_with)
	    {
	      int l;

	      g2->new_reg = can_combine[i * giv_count + j];
	      g2->same = g1;
	      /* For destination, we now may replace by mem expression instead
		 of register.  This changes the costs considerably, so add the
		 compensation.  */
	      if (g2->giv_type == DEST_ADDR)
		g2->benefit = (g2->benefit + reg_address_cost
			       - address_cost (g2->new_reg,
			       GET_MODE (g2->mem)));
	      g1->combined_with++;
	      g1->lifetime += g2->lifetime;

	      g1_add_benefit += g2->benefit;

	      /* ??? The new final_[bg]iv_value code does a much better job
		 of finding replaceable giv's, and hence this code may no
		 longer be necessary.  */
	      if (! g2->replaceable && REG_USERVAR_P (g2->dest_reg))
		g1_add_benefit -= copy_cost;

	      /* To help optimize the next set of combinations, remove
		 this giv from the benefits of other potential mates.  */
	      for (l = 0; l < giv_count; ++l)
		{
		  int m = stats[l].giv_number;
		  if (can_combine[m * giv_count + j])
		    stats[l].total_benefit -= g2->benefit + extra_benefit;
		}

	      if (loop_dump_stream)
		fprintf (loop_dump_stream,
			 "giv at %d combined with giv at %d; new benefit %d + %d, lifetime %d\n",
			 INSN_UID (g2->insn), INSN_UID (g1->insn),
			 g1->benefit, g1_add_benefit, g1->lifetime);
	    }
	}

      /* To help optimize the next set of combinations, remove
	 this giv from the benefits of other potential mates.  */
      if (g1->combined_with)
	{
	  for (j = 0; j < giv_count; ++j)
	    {
	      int m = stats[j].giv_number;
	      if (can_combine[m * giv_count + i])
		stats[j].total_benefit -= g1->benefit + extra_benefit;
	    }

	  g1->benefit += g1_add_benefit;

	  /* We've finished with this giv, and everything it touched.
	     Restart the combination so that proper weights for the
	     rest of the givs are properly taken into account.  */
	  /* ??? Ideally we would compact the arrays at this point, so
	     as to not cover old ground.  But sanely compacting
	     can_combine is tricky.  */
	  goto restart;
	}
    }

  /* Clean up.  */
  free (stats);
  free (can_combine);
}

/* Generate sequence for REG = B * M + A.  */

static rtx
gen_add_mult (b, m, a, reg)
     rtx b;          /* initial value of basic induction variable */
     rtx m;          /* multiplicative constant */
     rtx a;          /* additive constant */
     rtx reg;        /* destination register */
{
  rtx seq;
  rtx result;

  start_sequence ();
  /* Use unsigned arithmetic.  */
  result = expand_mult_add (b, reg, m, a, GET_MODE (reg), 1);
  if (reg != result)
    emit_move_insn (reg, result);
  seq = gen_sequence ();
  end_sequence ();

  return seq;
}


/* Update registers created in insn sequence SEQ.  */

static void
loop_regs_update (loop, seq)
     const struct loop *loop ATTRIBUTE_UNUSED;
     rtx seq;
{
  /* Update register info for alias analysis.  */

  if (GET_CODE (seq) == SEQUENCE)
    {
      int i;
      for (i = 0; i < XVECLEN (seq, 0); ++i)
	{
	  rtx set = single_set (XVECEXP (seq, 0, i));
	  if (set && GET_CODE (SET_DEST (set)) == REG)
	    record_base_value (REGNO (SET_DEST (set)), SET_SRC (set), 0);
	}
    }
  else
    {
      if (GET_CODE (seq) == SET
	  && GET_CODE (SET_DEST (seq)) == REG)
	record_base_value (REGNO (SET_DEST (seq)), SET_SRC (seq), 0);
    }
}


/* EMIT code before BEFORE_BB/BEFORE_INSN to set REG = B * M + A.  */

void
loop_iv_add_mult_emit_before (loop, b, m, a, reg, before_bb, before_insn)
     const struct loop *loop;
     rtx b;          /* initial value of basic induction variable */
     rtx m;          /* multiplicative constant */
     rtx a;          /* additive constant */
     rtx reg;        /* destination register */
     basic_block before_bb;
     rtx before_insn;
{
  rtx seq;

  if (! before_insn)
    {
      loop_iv_add_mult_hoist (loop, b, m, a, reg);
      return;
    }

  /* Use copy_rtx to prevent unexpected sharing of these rtx.  */
  seq = gen_add_mult (copy_rtx (b), copy_rtx (m), copy_rtx (a), reg);

  /* Increase the lifetime of any invariants moved further in code.  */
  update_reg_last_use (a, before_insn);
  update_reg_last_use (b, before_insn);
  update_reg_last_use (m, before_insn);

  loop_insn_emit_before (loop, before_bb, before_insn, seq);

  /* It is possible that the expansion created lots of new registers.
     Iterate over the sequence we just created and record them all.  */
  loop_regs_update (loop, seq);
}


/* Emit insns in loop pre-header to set REG = B * M + A.  */

void
loop_iv_add_mult_sink (loop, b, m, a, reg)
     const struct loop *loop;
     rtx b;          /* initial value of basic induction variable */
     rtx m;          /* multiplicative constant */
     rtx a;          /* additive constant */
     rtx reg;        /* destination register */
{
  rtx seq;

  /* Use copy_rtx to prevent unexpected sharing of these rtx.  */
  seq = gen_add_mult (copy_rtx (b), copy_rtx (m), copy_rtx (a), reg);

  /* Increase the lifetime of any invariants moved further in code.
     ???? Is this really necessary?  */
  update_reg_last_use (a, loop->sink);
  update_reg_last_use (b, loop->sink);
  update_reg_last_use (m, loop->sink);

  loop_insn_sink (loop, seq);

  /* It is possible that the expansion created lots of new registers.
     Iterate over the sequence we just created and record them all.  */
  loop_regs_update (loop, seq);
}


/* Emit insns after loop to set REG = B * M + A.  */

void
loop_iv_add_mult_hoist (loop, b, m, a, reg)
     const struct loop *loop;
     rtx b;          /* initial value of basic induction variable */
     rtx m;          /* multiplicative constant */
     rtx a;          /* additive constant */
     rtx reg;        /* destination register */
{
  rtx seq;

  /* Use copy_rtx to prevent unexpected sharing of these rtx.  */
  seq = gen_add_mult (copy_rtx (b), copy_rtx (m), copy_rtx (a), reg);

  loop_insn_hoist (loop, seq);

  /* It is possible that the expansion created lots of new registers.
     Iterate over the sequence we just created and record them all.  */
  loop_regs_update (loop, seq);
}



/* Similar to gen_add_mult, but compute cost rather than generating
   sequence.  */

static int
iv_add_mult_cost (b, m, a, reg)
     rtx b;          /* initial value of basic induction variable */
     rtx m;          /* multiplicative constant */
     rtx a;          /* additive constant */
     rtx reg;        /* destination register */
{
  int cost = 0;
  rtx last, result;

  start_sequence ();
  result = expand_mult_add (b, reg, m, a, GET_MODE (reg), 1);
  if (reg != result)
    emit_move_insn (reg, result);
  last = get_last_insn ();
  while (last)
    {
      rtx t = single_set (last);
      if (t)
	cost += rtx_cost (SET_SRC (t), SET);
      last = PREV_INSN (last);
    }
  end_sequence ();
  return cost;
}

/* Test whether A * B can be computed without
   an actual multiply insn.  Value is 1 if so.  */

static int
product_cheap_p (a, b)
     rtx a;
     rtx b;
{
  int i;
  rtx tmp;
  int win = 1;

  /* If only one is constant, make it B.  */
  if (GET_CODE (a) == CONST_INT)
    tmp = a, a = b, b = tmp;

  /* If first constant, both constant, so don't need multiply.  */
  if (GET_CODE (a) == CONST_INT)
    return 1;

  /* If second not constant, neither is constant, so would need multiply.  */
  if (GET_CODE (b) != CONST_INT)
    return 0;

  /* One operand is constant, so might not need multiply insn.  Generate the
     code for the multiply and see if a call or multiply, or long sequence
     of insns is generated.  */

  start_sequence ();
  expand_mult (GET_MODE (a), a, b, NULL_RTX, 1);
  tmp = gen_sequence ();
  end_sequence ();

  if (GET_CODE (tmp) == SEQUENCE)
    {
      if (XVEC (tmp, 0) == 0)
	win = 1;
      else if (XVECLEN (tmp, 0) > 3)
	win = 0;
      else
	for (i = 0; i < XVECLEN (tmp, 0); i++)
	  {
	    rtx insn = XVECEXP (tmp, 0, i);

	    if (GET_CODE (insn) != INSN
		|| (GET_CODE (PATTERN (insn)) == SET
		    && GET_CODE (SET_SRC (PATTERN (insn))) == MULT)
		|| (GET_CODE (PATTERN (insn)) == PARALLEL
		    && GET_CODE (XVECEXP (PATTERN (insn), 0, 0)) == SET
		    && GET_CODE (SET_SRC (XVECEXP (PATTERN (insn), 0, 0))) == MULT))
	      {
		win = 0;
		break;
	      }
	  }
    }
  else if (GET_CODE (tmp) == SET
	   && GET_CODE (SET_SRC (tmp)) == MULT)
    win = 0;
  else if (GET_CODE (tmp) == PARALLEL
	   && GET_CODE (XVECEXP (tmp, 0, 0)) == SET
	   && GET_CODE (SET_SRC (XVECEXP (tmp, 0, 0))) == MULT)
    win = 0;

  return win;
}

/* Check to see if loop can be terminated by a "decrement and branch until
   zero" instruction.  If so, add a REG_NONNEG note to the branch insn if so.
   Also try reversing an increment loop to a decrement loop
   to see if the optimization can be performed.
   Value is nonzero if optimization was performed.  */

/* This is useful even if the architecture doesn't have such an insn,
   because it might change a loops which increments from 0 to n to a loop
   which decrements from n to 0.  A loop that decrements to zero is usually
   faster than one that increments from zero.  */

/* ??? This could be rewritten to use some of the loop unrolling procedures,
   such as approx_final_value, biv_total_increment, loop_iterations, and
   final_[bg]iv_value.  */

static int
check_dbra_loop (loop, insn_count)
     struct loop *loop;
     int insn_count;
{
  struct loop_info *loop_info = LOOP_INFO (loop);
  struct loop_regs *regs = LOOP_REGS (loop);
  struct loop_ivs *ivs = LOOP_IVS (loop);
  struct iv_class *bl;
  rtx reg;
  rtx jump_label;
  rtx final_value;
  rtx start_value;
  rtx new_add_val;
  rtx comparison;
  rtx before_comparison;
  rtx p;
  rtx jump;
  rtx first_compare;
  int compare_and_branch;
  rtx loop_start = loop->start;
  rtx loop_end = loop->end;

  /* If last insn is a conditional branch, and the insn before tests a
     register value, try to optimize it.  Otherwise, we can't do anything.  */

  jump = PREV_INSN (loop_end);
  comparison = get_condition_for_loop (loop, jump);
  if (comparison == 0)
    return 0;
  if (!onlyjump_p (jump))
    return 0;

  /* Try to compute whether the compare/branch at the loop end is one or
     two instructions.  */
  get_condition (jump, &first_compare);
  if (first_compare == jump)
    compare_and_branch = 1;
  else if (first_compare == prev_nonnote_insn (jump))
    compare_and_branch = 2;
  else
    return 0;

  {
    /* If more than one condition is present to control the loop, then
       do not proceed, as this function does not know how to rewrite
       loop tests with more than one condition.

       Look backwards from the first insn in the last comparison
       sequence and see if we've got another comparison sequence.  */

    rtx jump1;
    if ((jump1 = prev_nonnote_insn (first_compare)) != loop->cont)
      if (GET_CODE (jump1) == JUMP_INSN)
	return 0;
  }

  /* Check all of the bivs to see if the compare uses one of them.
     Skip biv's set more than once because we can't guarantee that
     it will be zero on the last iteration.  Also skip if the biv is
     used between its update and the test insn.  */

  for (bl = ivs->list; bl; bl = bl->next)
    {
      if (bl->biv_count == 1
	  && ! bl->biv->maybe_multiple
	  && bl->biv->dest_reg == XEXP (comparison, 0)
	  && ! reg_used_between_p (regno_reg_rtx[bl->regno], bl->biv->insn,
				   first_compare))
	break;
    }

  if (! bl)
    return 0;

  /* Look for the case where the basic induction variable is always
     nonnegative, and equals zero on the last iteration.
     In this case, add a reg_note REG_NONNEG, which allows the
     m68k DBRA instruction to be used.  */

  if (((GET_CODE (comparison) == GT
	&& GET_CODE (XEXP (comparison, 1)) == CONST_INT
	&& INTVAL (XEXP (comparison, 1)) == -1)
       || (GET_CODE (comparison) == NE && XEXP (comparison, 1) == const0_rtx))
      && GET_CODE (bl->biv->add_val) == CONST_INT
      && INTVAL (bl->biv->add_val) < 0)
    {
      /* Initial value must be greater than 0,
	 init_val % -dec_value == 0 to ensure that it equals zero on
	 the last iteration */

      if (GET_CODE (bl->initial_value) == CONST_INT
	  && INTVAL (bl->initial_value) > 0
	  && (INTVAL (bl->initial_value)
	      % (-INTVAL (bl->biv->add_val))) == 0)
	{
	  /* register always nonnegative, add REG_NOTE to branch */
	  if (! find_reg_note (jump, REG_NONNEG, NULL_RTX))
	    REG_NOTES (jump)
	      = gen_rtx_EXPR_LIST (REG_NONNEG, bl->biv->dest_reg,
				   REG_NOTES (jump));
	  bl->nonneg = 1;

	  return 1;
	}

      /* If the decrement is 1 and the value was tested as >= 0 before
	 the loop, then we can safely optimize.  */
      for (p = loop_start; p; p = PREV_INSN (p))
	{
	  if (GET_CODE (p) == CODE_LABEL)
	    break;
	  if (GET_CODE (p) != JUMP_INSN)
	    continue;

	  before_comparison = get_condition_for_loop (loop, p);
	  if (before_comparison
	      && XEXP (before_comparison, 0) == bl->biv->dest_reg
	      && GET_CODE (before_comparison) == LT
	      && XEXP (before_comparison, 1) == const0_rtx
	      && ! reg_set_between_p (bl->biv->dest_reg, p, loop_start)
	      && INTVAL (bl->biv->add_val) == -1)
	    {
	      if (! find_reg_note (jump, REG_NONNEG, NULL_RTX))
		REG_NOTES (jump)
		  = gen_rtx_EXPR_LIST (REG_NONNEG, bl->biv->dest_reg,
				       REG_NOTES (jump));
	      bl->nonneg = 1;

	      return 1;
	    }
	}
    }
  else if (GET_CODE (bl->biv->add_val) == CONST_INT
	   && INTVAL (bl->biv->add_val) > 0)
    {
      /* Try to change inc to dec, so can apply above optimization.  */
      /* Can do this if:
	 all registers modified are induction variables or invariant,
	 all memory references have non-overlapping addresses
	 (obviously true if only one write)
	 allow 2 insns for the compare/jump at the end of the loop.  */
      /* Also, we must avoid any instructions which use both the reversed
	 biv and another biv.  Such instructions will fail if the loop is
	 reversed.  We meet this condition by requiring that either
	 no_use_except_counting is true, or else that there is only
	 one biv.  */
      int num_nonfixed_reads = 0;
      /* 1 if the iteration var is used only to count iterations.  */
      int no_use_except_counting = 0;
      /* 1 if the loop has no memory store, or it has a single memory store
	 which is reversible.  */
      int reversible_mem_store = 1;

      if (bl->giv_count == 0
	  && !loop->exit_count
	  && !loop_info->has_multiple_exit_targets)
	{
	  rtx bivreg = regno_reg_rtx[bl->regno];
	  struct iv_class *blt;

	  /* If there are no givs for this biv, and the only exit is the
	     fall through at the end of the loop, then
	     see if perhaps there are no uses except to count.  */
	  no_use_except_counting = 1;
	  for (p = loop_start; p != loop_end; p = NEXT_INSN (p))
	    if (INSN_P (p))
	      {
		rtx set = single_set (p);

		if (set && GET_CODE (SET_DEST (set)) == REG
		    && REGNO (SET_DEST (set)) == bl->regno)
		  /* An insn that sets the biv is okay.  */
		  ;
		else if ((p == prev_nonnote_insn (prev_nonnote_insn (loop_end))
			  || p == prev_nonnote_insn (loop_end))
			 && reg_mentioned_p (bivreg, PATTERN (p)))
		  {
		    /* If either of these insns uses the biv and sets a pseudo
		       that has more than one usage, then the biv has uses
		       other than counting since it's used to derive a value
		       that is used more than one time.  */
		    note_stores (PATTERN (p), note_set_pseudo_multiple_uses,
				 regs);
		    if (regs->multiple_uses)
		      {
			no_use_except_counting = 0;
			break;
		      }
		  }
		else if (reg_mentioned_p (bivreg, PATTERN (p)))
		  {
		    no_use_except_counting = 0;
		    break;
		  }
	      }

	  /* A biv has uses besides counting if it is used to set
	     another biv.  */
	  for (blt = ivs->list; blt; blt = blt->next)
	    if (blt->init_set
		&& reg_mentioned_p (bivreg, SET_SRC (blt->init_set)))
	      {
		no_use_except_counting = 0;
		break;
	      }
	}

      if (no_use_except_counting)
	/* No need to worry about MEMs.  */
	;
      else if (loop_info->num_mem_sets <= 1)
	{
	  for (p = loop_start; p != loop_end; p = NEXT_INSN (p))
	    if (INSN_P (p))
	      num_nonfixed_reads += count_nonfixed_reads (loop, PATTERN (p));

	  /* If the loop has a single store, and the destination address is
	     invariant, then we can't reverse the loop, because this address
	     might then have the wrong value at loop exit.
	     This would work if the source was invariant also, however, in that
	     case, the insn should have been moved out of the loop.  */

	  if (loop_info->num_mem_sets == 1)
	    {
	      struct induction *v;

	      /* If we could prove that each of the memory locations
		 written to was different, then we could reverse the
		 store -- but we don't presently have any way of
		 knowing that.  */
	      reversible_mem_store = 0;

	      /* If the store depends on a register that is set after the
		 store, it depends on the initial value, and is thus not
		 reversible.  */
	      for (v = bl->giv; reversible_mem_store && v; v = v->next_iv)
		{
		  if (v->giv_type == DEST_REG
		      && reg_mentioned_p (v->dest_reg,
					  PATTERN (loop_info->first_loop_store_insn))
		      && loop_insn_first_p (loop_info->first_loop_store_insn,
					    v->insn))
		    reversible_mem_store = 0;
		}
	    }
	}
      else
	return 0;

      /* This code only acts for innermost loops.  Also it simplifies
	 the memory address check by only reversing loops with
	 zero or one memory access.
	 Two memory accesses could involve parts of the same array,
	 and that can't be reversed.
	 If the biv is used only for counting, than we don't need to worry
	 about all these things.  */

      if ((num_nonfixed_reads <= 1
	   && ! loop_info->has_nonconst_call
	   && ! loop_info->has_volatile
	   && reversible_mem_store
	   && (bl->giv_count + bl->biv_count + loop_info->num_mem_sets
	       + num_unmoved_movables (loop) + compare_and_branch == insn_count)
	   && (bl == ivs->list && bl->next == 0))
	  || no_use_except_counting)
	{
	  rtx tem;

	  /* Loop can be reversed.  */
	  if (loop_dump_stream)
	    fprintf (loop_dump_stream, "Can reverse loop\n");

	  /* Now check other conditions:

	     The increment must be a constant, as must the initial value,
	     and the comparison code must be LT.

	     This test can probably be improved since +/- 1 in the constant
	     can be obtained by changing LT to LE and vice versa; this is
	     confusing.  */

	  if (comparison
	      /* for constants, LE gets turned into LT */
	      && (GET_CODE (comparison) == LT
		  || (GET_CODE (comparison) == LE
		      && no_use_except_counting)))
	    {
	      HOST_WIDE_INT add_val, add_adjust, comparison_val = 0;
	      rtx initial_value, comparison_value;
	      int nonneg = 0;
	      enum rtx_code cmp_code;
	      int comparison_const_width;
	      unsigned HOST_WIDE_INT comparison_sign_mask;

	      add_val = INTVAL (bl->biv->add_val);
	      comparison_value = XEXP (comparison, 1);
	      if (GET_MODE (comparison_value) == VOIDmode)
		comparison_const_width
		  = GET_MODE_BITSIZE (GET_MODE (XEXP (comparison, 0)));
	      else
		comparison_const_width
		  = GET_MODE_BITSIZE (GET_MODE (comparison_value));
	      if (comparison_const_width > HOST_BITS_PER_WIDE_INT)
		comparison_const_width = HOST_BITS_PER_WIDE_INT;
	      comparison_sign_mask
		= (unsigned HOST_WIDE_INT) 1 << (comparison_const_width - 1);

	      /* If the comparison value is not a loop invariant, then we
		 can not reverse this loop.

		 ??? If the insns which initialize the comparison value as
		 a whole compute an invariant result, then we could move
		 them out of the loop and proceed with loop reversal.  */
	      if (! loop_invariant_p (loop, comparison_value))
		return 0;

	      if (GET_CODE (comparison_value) == CONST_INT)
		comparison_val = INTVAL (comparison_value);
	      initial_value = bl->initial_value;

	      /* Normalize the initial value if it is an integer and
		 has no other use except as a counter.  This will allow
		 a few more loops to be reversed.  */
	      if (no_use_except_counting
		  && GET_CODE (comparison_value) == CONST_INT
		  && GET_CODE (initial_value) == CONST_INT)
		{
		  comparison_val = comparison_val - INTVAL (bl->initial_value);
		  /* The code below requires comparison_val to be a multiple
		     of add_val in order to do the loop reversal, so
		     round up comparison_val to a multiple of add_val.
		     Since comparison_value is constant, we know that the
		     current comparison code is LT.  */
		  comparison_val = comparison_val + add_val - 1;
		  comparison_val
		    -= (unsigned HOST_WIDE_INT) comparison_val % add_val;
		  /* We postpone overflow checks for COMPARISON_VAL here;
		     even if there is an overflow, we might still be able to
		     reverse the loop, if converting the loop exit test to
		     NE is possible.  */
		  initial_value = const0_rtx;
		}

	      /* First check if we can do a vanilla loop reversal.  */
	      if (initial_value == const0_rtx
		  /* If we have a decrement_and_branch_on_count,
		     prefer the NE test, since this will allow that
		     instruction to be generated.  Note that we must
		     use a vanilla loop reversal if the biv is used to
		     calculate a giv or has a non-counting use.  */
#if ! defined (HAVE_decrement_and_branch_until_zero) \
&& defined (HAVE_decrement_and_branch_on_count)
		  && (! (add_val == 1 && loop->vtop
		         && (bl->biv_count == 0
			     || no_use_except_counting)))
#endif
		  && GET_CODE (comparison_value) == CONST_INT
		     /* Now do postponed overflow checks on COMPARISON_VAL.  */
		  && ! (((comparison_val - add_val) ^ INTVAL (comparison_value))
			& comparison_sign_mask))
		{
		  /* Register will always be nonnegative, with value
		     0 on last iteration */
		  add_adjust = add_val;
		  nonneg = 1;
		  cmp_code = GE;
		}
	      else if (add_val == 1 && loop->vtop
		       && (bl->biv_count == 0
			   || no_use_except_counting))
		{
		  add_adjust = 0;
		  cmp_code = NE;
		}
	      else
		return 0;

	      if (GET_CODE (comparison) == LE)
		add_adjust -= add_val;

	      /* If the initial value is not zero, or if the comparison
		 value is not an exact multiple of the increment, then we
		 can not reverse this loop.  */
	      if (initial_value == const0_rtx
		  && GET_CODE (comparison_value) == CONST_INT)
		{
		  if (((unsigned HOST_WIDE_INT) comparison_val % add_val) != 0)
		    return 0;
		}
	      else
		{
		  if (! no_use_except_counting || add_val != 1)
		    return 0;
		}

	      final_value = comparison_value;

	      /* Reset these in case we normalized the initial value
		 and comparison value above.  */
	      if (GET_CODE (comparison_value) == CONST_INT
		  && GET_CODE (initial_value) == CONST_INT)
		{
		  comparison_value = GEN_INT (comparison_val);
		  final_value
		    = GEN_INT (comparison_val + INTVAL (bl->initial_value));
		}
	      bl->initial_value = initial_value;

	      /* Save some info needed to produce the new insns.  */
	      reg = bl->biv->dest_reg;
	      jump_label = condjump_label (PREV_INSN (loop_end));
	      new_add_val = GEN_INT (-INTVAL (bl->biv->add_val));

	      /* Set start_value; if this is not a CONST_INT, we need
		 to generate a SUB.
		 Initialize biv to start_value before loop start.
		 The old initializing insn will be deleted as a
		 dead store by flow.c.  */
	      if (initial_value == const0_rtx
		  && GET_CODE (comparison_value) == CONST_INT)
		{
		  start_value = GEN_INT (comparison_val - add_adjust);
		  loop_insn_hoist (loop, gen_move_insn (reg, start_value));
		}
	      else if (GET_CODE (initial_value) == CONST_INT)
		{
		  enum machine_mode mode = GET_MODE (reg);
		  rtx offset = GEN_INT (-INTVAL (initial_value) - add_adjust);
		  rtx add_insn = gen_add3_insn (reg, comparison_value, offset);

		  if (add_insn == 0)
		    return 0;

		  start_value
		    = gen_rtx_PLUS (mode, comparison_value, offset);
		  loop_insn_hoist (loop, add_insn);
		  if (GET_CODE (comparison) == LE)
		    final_value = gen_rtx_PLUS (mode, comparison_value,
						GEN_INT (add_val));
		}
	      else if (! add_adjust)
		{
		  enum machine_mode mode = GET_MODE (reg);
		  rtx sub_insn = gen_sub3_insn (reg, comparison_value,
						initial_value);

		  if (sub_insn == 0)
		    return 0;
		  start_value
		    = gen_rtx_MINUS (mode, comparison_value, initial_value);
		  loop_insn_hoist (loop, sub_insn);
		}
	      else
		/* We could handle the other cases too, but it'll be
		   better to have a testcase first.  */
		return 0;

	      /* We may not have a single insn which can increment a reg, so
		 create a sequence to hold all the insns from expand_inc.  */
	      start_sequence ();
	      expand_inc (reg, new_add_val);
	      tem = gen_sequence ();
	      end_sequence ();

	      p = loop_insn_emit_before (loop, 0, bl->biv->insn, tem);
	      delete_insn (bl->biv->insn);

	      /* Update biv info to reflect its new status.  */
	      bl->biv->insn = p;
	      bl->initial_value = start_value;
	      bl->biv->add_val = new_add_val;

	      /* Update loop info.  */
	      loop_info->initial_value = reg;
	      loop_info->initial_equiv_value = reg;
	      loop_info->final_value = const0_rtx;
	      loop_info->final_equiv_value = const0_rtx;
	      loop_info->comparison_value = const0_rtx;
	      loop_info->comparison_code = cmp_code;
	      loop_info->increment = new_add_val;

	      /* Inc LABEL_NUSES so that delete_insn will
		 not delete the label.  */
	      LABEL_NUSES (XEXP (jump_label, 0))++;

	      /* Emit an insn after the end of the loop to set the biv's
		 proper exit value if it is used anywhere outside the loop.  */
	      if ((REGNO_LAST_UID (bl->regno) != INSN_UID (first_compare))
		  || ! bl->init_insn
		  || REGNO_FIRST_UID (bl->regno) != INSN_UID (bl->init_insn))
		loop_insn_sink (loop, gen_move_insn (reg, final_value));

	      /* Delete compare/branch at end of loop.  */
	      delete_related_insns (PREV_INSN (loop_end));
	      if (compare_and_branch == 2)
		delete_related_insns (first_compare);

	      /* Add new compare/branch insn at end of loop.  */
	      start_sequence ();
	      emit_cmp_and_jump_insns (reg, const0_rtx, cmp_code, NULL_RTX,
				       GET_MODE (reg), 0,
				       XEXP (jump_label, 0));
	      tem = gen_sequence ();
	      end_sequence ();
	      emit_jump_insn_before (tem, loop_end);

	      for (tem = PREV_INSN (loop_end);
		   tem && GET_CODE (tem) != JUMP_INSN;
		   tem = PREV_INSN (tem))
		;

	      if (tem)
		JUMP_LABEL (tem) = XEXP (jump_label, 0);

	      if (nonneg)
		{
		  if (tem)
		    {
		      /* Increment of LABEL_NUSES done above.  */
		      /* Register is now always nonnegative,
			 so add REG_NONNEG note to the branch.  */
		      REG_NOTES (tem) = gen_rtx_EXPR_LIST (REG_NONNEG, reg,
							   REG_NOTES (tem));
		    }
		  bl->nonneg = 1;
		}

	      /* No insn may reference both the reversed and another biv or it
		 will fail (see comment near the top of the loop reversal
		 code).
		 Earlier on, we have verified that the biv has no use except
		 counting, or it is the only biv in this function.
		 However, the code that computes no_use_except_counting does
		 not verify reg notes.  It's possible to have an insn that
		 references another biv, and has a REG_EQUAL note with an
		 expression based on the reversed biv.  To avoid this case,
		 remove all REG_EQUAL notes based on the reversed biv
		 here.  */
	      for (p = loop_start; p != loop_end; p = NEXT_INSN (p))
		if (INSN_P (p))
		  {
		    rtx *pnote;
		    rtx set = single_set (p);
		    /* If this is a set of a GIV based on the reversed biv, any
		       REG_EQUAL notes should still be correct.  */
		    if (! set
			|| GET_CODE (SET_DEST (set)) != REG
			|| (size_t) REGNO (SET_DEST (set)) >= ivs->n_regs
			|| REG_IV_TYPE (ivs, REGNO (SET_DEST (set))) != GENERAL_INDUCT
			|| REG_IV_INFO (ivs, REGNO (SET_DEST (set)))->src_reg != bl->biv->src_reg)
		      for (pnote = &REG_NOTES (p); *pnote;)
			{
			  if (REG_NOTE_KIND (*pnote) == REG_EQUAL
			      && reg_mentioned_p (regno_reg_rtx[bl->regno],
						  XEXP (*pnote, 0)))
			    *pnote = XEXP (*pnote, 1);
			  else
			    pnote = &XEXP (*pnote, 1);
			}
		  }

	      /* Mark that this biv has been reversed.  Each giv which depends
		 on this biv, and which is also live past the end of the loop
		 will have to be fixed up.  */

	      bl->reversed = 1;

	      if (loop_dump_stream)
		{
		  fprintf (loop_dump_stream, "Reversed loop");
		  if (bl->nonneg)
		    fprintf (loop_dump_stream, " and added reg_nonneg\n");
		  else
		    fprintf (loop_dump_stream, "\n");
		}

	      return 1;
	    }
	}
    }

  return 0;
}

/* Verify whether the biv BL appears to be eliminable,
   based on the insns in the loop that refer to it.

   If ELIMINATE_P is non-zero, actually do the elimination.

   THRESHOLD and INSN_COUNT are from loop_optimize and are used to
   determine whether invariant insns should be placed inside or at the
   start of the loop.  */

static int
maybe_eliminate_biv (loop, bl, eliminate_p, threshold, insn_count)
     const struct loop *loop;
     struct iv_class *bl;
     int eliminate_p;
     int threshold, insn_count;
{
  struct loop_ivs *ivs = LOOP_IVS (loop);
  rtx reg = bl->biv->dest_reg;
  rtx p;

  /* Scan all insns in the loop, stopping if we find one that uses the
     biv in a way that we cannot eliminate.  */

  for (p = loop->start; p != loop->end; p = NEXT_INSN (p))
    {
      enum rtx_code code = GET_CODE (p);
      basic_block where_bb = 0;
      rtx where_insn = threshold >= insn_count ? 0 : p;

      /* If this is a libcall that sets a giv, skip ahead to its end.  */
      if (GET_RTX_CLASS (code) == 'i')
	{
	  rtx note = find_reg_note (p, REG_LIBCALL, NULL_RTX);

	  if (note)
	    {
	      rtx last = XEXP (note, 0);
	      rtx set = single_set (last);

	      if (set && GET_CODE (SET_DEST (set)) == REG)
		{
		  unsigned int regno = REGNO (SET_DEST (set));

		  if (regno < ivs->n_regs
		      && REG_IV_TYPE (ivs, regno) == GENERAL_INDUCT
		      && REG_IV_INFO (ivs, regno)->src_reg == bl->biv->src_reg)
		    p = last;
		}
	    }
	}
      if ((code == INSN || code == JUMP_INSN || code == CALL_INSN)
	  && reg_mentioned_p (reg, PATTERN (p))
	  && ! maybe_eliminate_biv_1 (loop, PATTERN (p), p, bl,
				      eliminate_p, where_bb, where_insn))
	{
	  if (loop_dump_stream)
	    fprintf (loop_dump_stream,
		     "Cannot eliminate biv %d: biv used in insn %d.\n",
		     bl->regno, INSN_UID (p));
	  break;
	}
    }

  if (p == loop->end)
    {
      if (loop_dump_stream)
	fprintf (loop_dump_stream, "biv %d %s eliminated.\n",
		 bl->regno, eliminate_p ? "was" : "can be");
      return 1;
    }

  return 0;
}

/* INSN and REFERENCE are instructions in the same insn chain.
   Return non-zero if INSN is first.  */

int
loop_insn_first_p (insn, reference)
     rtx insn, reference;
{
  rtx p, q;

  for (p = insn, q = reference;;)
    {
      /* Start with test for not first so that INSN == REFERENCE yields not
         first.  */
      if (q == insn || ! p)
        return 0;
      if (p == reference || ! q)
        return 1;

      /* Either of P or Q might be a NOTE.  Notes have the same LUID as the
         previous insn, hence the <= comparison below does not work if
	 P is a note.  */
      if (INSN_UID (p) < max_uid_for_loop
	  && INSN_UID (q) < max_uid_for_loop
	  && GET_CODE (p) != NOTE)
	return INSN_LUID (p) <= INSN_LUID (q);

      if (INSN_UID (p) >= max_uid_for_loop
	  || GET_CODE (p) == NOTE)
	p = NEXT_INSN (p);
      if (INSN_UID (q) >= max_uid_for_loop)
	q = NEXT_INSN (q);
    }
}

/* We are trying to eliminate BIV in INSN using GIV.  Return non-zero if
   the offset that we have to take into account due to auto-increment /
   div derivation is zero.  */
static int
biv_elimination_giv_has_0_offset (biv, giv, insn)
     struct induction *biv, *giv;
     rtx insn;
{
  /* If the giv V had the auto-inc address optimization applied
     to it, and INSN occurs between the giv insn and the biv
     insn, then we'd have to adjust the value used here.
     This is rare, so we don't bother to make this possible.  */
  if (giv->auto_inc_opt
      && ((loop_insn_first_p (giv->insn, insn)
	   && loop_insn_first_p (insn, biv->insn))
	  || (loop_insn_first_p (biv->insn, insn)
	      && loop_insn_first_p (insn, giv->insn))))
    return 0;

  return 1;
}

/* If BL appears in X (part of the pattern of INSN), see if we can
   eliminate its use.  If so, return 1.  If not, return 0.

   If BIV does not appear in X, return 1.

   If ELIMINATE_P is non-zero, actually do the elimination.
   WHERE_INSN/WHERE_BB indicate where extra insns should be added.
   Depending on how many items have been moved out of the loop, it
   will either be before INSN (when WHERE_INSN is non-zero) or at the
   start of the loop (when WHERE_INSN is zero).  */

static int
maybe_eliminate_biv_1 (loop, x, insn, bl, eliminate_p, where_bb, where_insn)
     const struct loop *loop;
     rtx x, insn;
     struct iv_class *bl;
     int eliminate_p;
     basic_block where_bb;
     rtx where_insn;
{
  enum rtx_code code = GET_CODE (x);
  rtx reg = bl->biv->dest_reg;
  enum machine_mode mode = GET_MODE (reg);
  struct induction *v;
  rtx arg, tem;
#ifdef HAVE_cc0
  rtx new;
#endif
  int arg_operand;
  const char *fmt;
  int i, j;

  switch (code)
    {
    case REG:
      /* If we haven't already been able to do something with this BIV,
	 we can't eliminate it.  */
      if (x == reg)
	return 0;
      return 1;

    case SET:
      /* If this sets the BIV, it is not a problem.  */
      if (SET_DEST (x) == reg)
	return 1;

      /* If this is an insn that defines a giv, it is also ok because
	 it will go away when the giv is reduced.  */
      for (v = bl->giv; v; v = v->next_iv)
	if (v->giv_type == DEST_REG && SET_DEST (x) == v->dest_reg)
	  return 1;

#ifdef HAVE_cc0
      if (SET_DEST (x) == cc0_rtx && SET_SRC (x) == reg)
	{
	  /* Can replace with any giv that was reduced and
	     that has (MULT_VAL != 0) and (ADD_VAL == 0).
	     Require a constant for MULT_VAL, so we know it's nonzero.
	     ??? We disable this optimization to avoid potential
	     overflows.  */

	  for (v = bl->giv; v; v = v->next_iv)
	    if (GET_CODE (v->mult_val) == CONST_INT && v->mult_val != const0_rtx
		&& v->add_val == const0_rtx
		&& ! v->ignore && ! v->maybe_dead && v->always_computable
		&& v->mode == mode
		&& 0)
	      {
		if (! biv_elimination_giv_has_0_offset (bl->biv, v, insn))
		  continue;

		if (! eliminate_p)
		  return 1;

		/* If the giv has the opposite direction of change,
		   then reverse the comparison.  */
		if (INTVAL (v->mult_val) < 0)
		  new = gen_rtx_COMPARE (GET_MODE (v->new_reg),
					 const0_rtx, v->new_reg);
		else
		  new = v->new_reg;

		/* We can probably test that giv's reduced reg.  */
		if (validate_change (insn, &SET_SRC (x), new, 0))
		  return 1;
	      }

	  /* Look for a giv with (MULT_VAL != 0) and (ADD_VAL != 0);
	     replace test insn with a compare insn (cmp REDUCED_GIV ADD_VAL).
	     Require a constant for MULT_VAL, so we know it's nonzero.
	     ??? Do this only if ADD_VAL is a pointer to avoid a potential
	     overflow problem.  */

	  for (v = bl->giv; v; v = v->next_iv)
	    if (GET_CODE (v->mult_val) == CONST_INT
		&& v->mult_val != const0_rtx
		&& ! v->ignore && ! v->maybe_dead && v->always_computable
		&& v->mode == mode
		&& (GET_CODE (v->add_val) == SYMBOL_REF
		    || GET_CODE (v->add_val) == LABEL_REF
		    || GET_CODE (v->add_val) == CONST
		    || (GET_CODE (v->add_val) == REG
			&& REG_POINTER (v->add_val))))
	      {
		if (! biv_elimination_giv_has_0_offset (bl->biv, v, insn))
		  continue;

		if (! eliminate_p)
		  return 1;

		/* If the giv has the opposite direction of change,
		   then reverse the comparison.  */
		if (INTVAL (v->mult_val) < 0)
		  new = gen_rtx_COMPARE (VOIDmode, copy_rtx (v->add_val),
					 v->new_reg);
		else
		  new = gen_rtx_COMPARE (VOIDmode, v->new_reg,
					 copy_rtx (v->add_val));

		/* Replace biv with the giv's reduced register.  */
		update_reg_last_use (v->add_val, insn);
		if (validate_change (insn, &SET_SRC (PATTERN (insn)), new, 0))
		  return 1;

		/* Insn doesn't support that constant or invariant.  Copy it
		   into a register (it will be a loop invariant.)  */
		tem = gen_reg_rtx (GET_MODE (v->new_reg));

		loop_insn_emit_before (loop, 0, where_insn,
				       gen_move_insn (tem,
						      copy_rtx (v->add_val)));

		/* Substitute the new register for its invariant value in
		   the compare expression.  */
		XEXP (new, (INTVAL (v->mult_val) < 0) ? 0 : 1) = tem;
		if (validate_change (insn, &SET_SRC (PATTERN (insn)), new, 0))
		  return 1;
	      }
	}
#endif
      break;

    case COMPARE:
    case EQ:  case NE:
    case GT:  case GE:  case GTU:  case GEU:
    case LT:  case LE:  case LTU:  case LEU:
      /* See if either argument is the biv.  */
      if (XEXP (x, 0) == reg)
	arg = XEXP (x, 1), arg_operand = 1;
      else if (XEXP (x, 1) == reg)
	arg = XEXP (x, 0), arg_operand = 0;
      else
	break;

      if (CONSTANT_P (arg))
	{
	  /* First try to replace with any giv that has constant positive
	     mult_val and constant add_val.  We might be able to support
	     negative mult_val, but it seems complex to do it in general.  */

	  for (v = bl->giv; v; v = v->next_iv)
	    if (GET_CODE (v->mult_val) == CONST_INT
		&& INTVAL (v->mult_val) > 0
		&& (GET_CODE (v->add_val) == SYMBOL_REF
		    || GET_CODE (v->add_val) == LABEL_REF
		    || GET_CODE (v->add_val) == CONST
		    || (GET_CODE (v->add_val) == REG
			&& REG_POINTER (v->add_val)))
		&& ! v->ignore && ! v->maybe_dead && v->always_computable
		&& v->mode == mode)
	      {
		if (! biv_elimination_giv_has_0_offset (bl->biv, v, insn))
		  continue;

		if (! eliminate_p)
		  return 1;

		/* Replace biv with the giv's reduced reg.  */
		validate_change (insn, &XEXP (x, 1 - arg_operand), v->new_reg, 1);

		/* If all constants are actually constant integers and
		   the derived constant can be directly placed in the COMPARE,
		   do so.  */
		if (GET_CODE (arg) == CONST_INT
		    && GET_CODE (v->mult_val) == CONST_INT
		    && GET_CODE (v->add_val) == CONST_INT)
		  {
		    validate_change (insn, &XEXP (x, arg_operand),
				     GEN_INT (INTVAL (arg)
					      * INTVAL (v->mult_val)
					      + INTVAL (v->add_val)), 1);
		  }
		else
		  {
		    /* Otherwise, load it into a register.  */
		    tem = gen_reg_rtx (mode);
		    loop_iv_add_mult_emit_before (loop, arg,
						  v->mult_val, v->add_val,
						  tem, where_bb, where_insn);
		    validate_change (insn, &XEXP (x, arg_operand), tem, 1);
		  }
		if (apply_change_group ())
		  return 1;
	      }

	  /* Look for giv with positive constant mult_val and nonconst add_val.
	     Insert insns to calculate new compare value.
	     ??? Turn this off due to possible overflow.  */

	  for (v = bl->giv; v; v = v->next_iv)
	    if (GET_CODE (v->mult_val) == CONST_INT
		&& INTVAL (v->mult_val) > 0
		&& ! v->ignore && ! v->maybe_dead && v->always_computable
		&& v->mode == mode
		&& 0)
	      {
		rtx tem;

		if (! biv_elimination_giv_has_0_offset (bl->biv, v, insn))
		  continue;

		if (! eliminate_p)
		  return 1;

		tem = gen_reg_rtx (mode);

		/* Replace biv with giv's reduced register.  */
		validate_change (insn, &XEXP (x, 1 - arg_operand),
				 v->new_reg, 1);

		/* Compute value to compare against.  */
		loop_iv_add_mult_emit_before (loop, arg,
					      v->mult_val, v->add_val,
					      tem, where_bb, where_insn);
		/* Use it in this insn.  */
		validate_change (insn, &XEXP (x, arg_operand), tem, 1);
		if (apply_change_group ())
		  return 1;
	      }
	}
      else if (GET_CODE (arg) == REG || GET_CODE (arg) == MEM)
	{
	  if (loop_invariant_p (loop, arg) == 1)
	    {
	      /* Look for giv with constant positive mult_val and nonconst
		 add_val. Insert insns to compute new compare value.
		 ??? Turn this off due to possible overflow.  */

	      for (v = bl->giv; v; v = v->next_iv)
		if (GET_CODE (v->mult_val) == CONST_INT && INTVAL (v->mult_val) > 0
		    && ! v->ignore && ! v->maybe_dead && v->always_computable
		    && v->mode == mode
		    && 0)
		  {
		    rtx tem;

		    if (! biv_elimination_giv_has_0_offset (bl->biv, v, insn))
		      continue;

		    if (! eliminate_p)
		      return 1;

		    tem = gen_reg_rtx (mode);

		    /* Replace biv with giv's reduced register.  */
		    validate_change (insn, &XEXP (x, 1 - arg_operand),
				     v->new_reg, 1);

		    /* Compute value to compare against.  */
		    loop_iv_add_mult_emit_before (loop, arg,
						  v->mult_val, v->add_val,
						  tem, where_bb, where_insn);
		    validate_change (insn, &XEXP (x, arg_operand), tem, 1);
		    if (apply_change_group ())
		      return 1;
		  }
	    }

	  /* This code has problems.  Basically, you can't know when
	     seeing if we will eliminate BL, whether a particular giv
	     of ARG will be reduced.  If it isn't going to be reduced,
	     we can't eliminate BL.  We can try forcing it to be reduced,
	     but that can generate poor code.

	     The problem is that the benefit of reducing TV, below should
	     be increased if BL can actually be eliminated, but this means
	     we might have to do a topological sort of the order in which
	     we try to process biv.  It doesn't seem worthwhile to do
	     this sort of thing now.  */

#if 0
	  /* Otherwise the reg compared with had better be a biv.  */
	  if (GET_CODE (arg) != REG
	      || REG_IV_TYPE (ivs, REGNO (arg)) != BASIC_INDUCT)
	    return 0;

	  /* Look for a pair of givs, one for each biv,
	     with identical coefficients.  */
	  for (v = bl->giv; v; v = v->next_iv)
	    {
	      struct induction *tv;

	      if (v->ignore || v->maybe_dead || v->mode != mode)
		continue;

	      for (tv = REG_IV_CLASS (ivs, REGNO (arg))->giv; tv;
		   tv = tv->next_iv)
		if (! tv->ignore && ! tv->maybe_dead
		    && rtx_equal_p (tv->mult_val, v->mult_val)
		    && rtx_equal_p (tv->add_val, v->add_val)
		    && tv->mode == mode)
		  {
		    if (! biv_elimination_giv_has_0_offset (bl->biv, v, insn))
		      continue;

		    if (! eliminate_p)
		      return 1;

		    /* Replace biv with its giv's reduced reg.  */
		    XEXP (x, 1 - arg_operand) = v->new_reg;
		    /* Replace other operand with the other giv's
		       reduced reg.  */
		    XEXP (x, arg_operand) = tv->new_reg;
		    return 1;
		  }
	    }
#endif
	}

      /* If we get here, the biv can't be eliminated.  */
      return 0;

    case MEM:
      /* If this address is a DEST_ADDR giv, it doesn't matter if the
	 biv is used in it, since it will be replaced.  */
      for (v = bl->giv; v; v = v->next_iv)
	if (v->giv_type == DEST_ADDR && v->location == &XEXP (x, 0))
	  return 1;
      break;

    default:
      break;
    }

  /* See if any subexpression fails elimination.  */
  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      switch (fmt[i])
	{
	case 'e':
	  if (! maybe_eliminate_biv_1 (loop, XEXP (x, i), insn, bl,
				       eliminate_p, where_bb, where_insn))
	    return 0;
	  break;

	case 'E':
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (! maybe_eliminate_biv_1 (loop, XVECEXP (x, i, j), insn, bl,
					 eliminate_p, where_bb, where_insn))
	      return 0;
	  break;
	}
    }

  return 1;
}

/* Return nonzero if the last use of REG
   is in an insn following INSN in the same basic block.  */

static int
last_use_this_basic_block (reg, insn)
     rtx reg;
     rtx insn;
{
  rtx n;
  for (n = insn;
       n && GET_CODE (n) != CODE_LABEL && GET_CODE (n) != JUMP_INSN;
       n = NEXT_INSN (n))
    {
      if (REGNO_LAST_UID (REGNO (reg)) == INSN_UID (n))
	return 1;
    }
  return 0;
}

/* Called via `note_stores' to record the initial value of a biv.  Here we
   just record the location of the set and process it later.  */

static void
record_initial (dest, set, data)
     rtx dest;
     rtx set;
     void *data ATTRIBUTE_UNUSED;
{
  struct loop_ivs *ivs = (struct loop_ivs *) data;
  struct iv_class *bl;

  if (GET_CODE (dest) != REG
      || REGNO (dest) >= ivs->n_regs
      || REG_IV_TYPE (ivs, REGNO (dest)) != BASIC_INDUCT)
    return;

  bl = REG_IV_CLASS (ivs, REGNO (dest));

  /* If this is the first set found, record it.  */
  if (bl->init_insn == 0)
    {
      bl->init_insn = note_insn;
      bl->init_set = set;
    }
}

/* If any of the registers in X are "old" and currently have a last use earlier
   than INSN, update them to have a last use of INSN.  Their actual last use
   will be the previous insn but it will not have a valid uid_luid so we can't
   use it.  X must be a source expression only.  */

static void
update_reg_last_use (x, insn)
     rtx x;
     rtx insn;
{
  /* Check for the case where INSN does not have a valid luid.  In this case,
     there is no need to modify the regno_last_uid, as this can only happen
     when code is inserted after the loop_end to set a pseudo's final value,
     and hence this insn will never be the last use of x.
     ???? This comment is not correct.  See for example loop_givs_reduce.
     This may insert an insn before another new insn.  */
  if (GET_CODE (x) == REG && REGNO (x) < max_reg_before_loop
      && INSN_UID (insn) < max_uid_for_loop
      && REGNO_LAST_LUID (REGNO (x)) < INSN_LUID (insn))
    {
      REGNO_LAST_UID (REGNO (x)) = INSN_UID (insn);
    }
  else
    {
      int i, j;
      const char *fmt = GET_RTX_FORMAT (GET_CODE (x));
      for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
	{
	  if (fmt[i] == 'e')
	    update_reg_last_use (XEXP (x, i), insn);
	  else if (fmt[i] == 'E')
	    for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	      update_reg_last_use (XVECEXP (x, i, j), insn);
	}
    }
}

/* Given an insn INSN and condition COND, return the condition in a
   canonical form to simplify testing by callers.  Specifically:

   (1) The code will always be a comparison operation (EQ, NE, GT, etc.).
   (2) Both operands will be machine operands; (cc0) will have been replaced.
   (3) If an operand is a constant, it will be the second operand.
   (4) (LE x const) will be replaced with (LT x <const+1>) and similarly
       for GE, GEU, and LEU.

   If the condition cannot be understood, or is an inequality floating-point
   comparison which needs to be reversed, 0 will be returned.

   If REVERSE is non-zero, then reverse the condition prior to canonizing it.

   If EARLIEST is non-zero, it is a pointer to a place where the earliest
   insn used in locating the condition was found.  If a replacement test
   of the condition is desired, it should be placed in front of that
   insn and we will be sure that the inputs are still valid.

   If WANT_REG is non-zero, we wish the condition to be relative to that
   register, if possible.  Therefore, do not canonicalize the condition
   further.  */

rtx
canonicalize_condition (insn, cond, reverse, earliest, want_reg)
     rtx insn;
     rtx cond;
     int reverse;
     rtx *earliest;
     rtx want_reg;
{
  enum rtx_code code;
  rtx prev = insn;
  rtx set;
  rtx tem;
  rtx op0, op1;
  int reverse_code = 0;
  enum machine_mode mode;

  code = GET_CODE (cond);
  mode = GET_MODE (cond);
  op0 = XEXP (cond, 0);
  op1 = XEXP (cond, 1);

  if (reverse)
    code = reversed_comparison_code (cond, insn);
  if (code == UNKNOWN)
    return 0;

  if (earliest)
    *earliest = insn;

  /* If we are comparing a register with zero, see if the register is set
     in the previous insn to a COMPARE or a comparison operation.  Perform
     the same tests as a function of STORE_FLAG_VALUE as find_comparison_args
     in cse.c  */

  while (GET_RTX_CLASS (code) == '<'
	 && op1 == CONST0_RTX (GET_MODE (op0))
	 && op0 != want_reg)
    {
      /* Set non-zero when we find something of interest.  */
      rtx x = 0;

#ifdef HAVE_cc0
      /* If comparison with cc0, import actual comparison from compare
	 insn.  */
      if (op0 == cc0_rtx)
	{
	  if ((prev = prev_nonnote_insn (prev)) == 0
	      || GET_CODE (prev) != INSN
	      || (set = single_set (prev)) == 0
	      || SET_DEST (set) != cc0_rtx)
	    return 0;

	  op0 = SET_SRC (set);
	  op1 = CONST0_RTX (GET_MODE (op0));
	  if (earliest)
	    *earliest = prev;
	}
#endif

      /* If this is a COMPARE, pick up the two things being compared.  */
      if (GET_CODE (op0) == COMPARE)
	{
	  op1 = XEXP (op0, 1);
	  op0 = XEXP (op0, 0);
	  continue;
	}
      else if (GET_CODE (op0) != REG)
	break;

      /* Go back to the previous insn.  Stop if it is not an INSN.  We also
	 stop if it isn't a single set or if it has a REG_INC note because
	 we don't want to bother dealing with it.  */

      if ((prev = prev_nonnote_insn (prev)) == 0
	  || GET_CODE (prev) != INSN
	  || FIND_REG_INC_NOTE (prev, NULL_RTX))
	break;

      set = set_of (op0, prev);

      if (set
	  && (GET_CODE (set) != SET
	      || !rtx_equal_p (SET_DEST (set), op0)))
	break;

      /* If this is setting OP0, get what it sets it to if it looks
	 relevant.  */
      if (set)
	{
	  enum machine_mode inner_mode = GET_MODE (SET_DEST (set));

	  /* ??? We may not combine comparisons done in a CCmode with
	     comparisons not done in a CCmode.  This is to aid targets
	     like Alpha that have an IEEE compliant EQ instruction, and
	     a non-IEEE compliant BEQ instruction.  The use of CCmode is
	     actually artificial, simply to prevent the combination, but
	     should not affect other platforms.

	     However, we must allow VOIDmode comparisons to match either
	     CCmode or non-CCmode comparison, because some ports have
	     modeless comparisons inside branch patterns.

	     ??? This mode check should perhaps look more like the mode check
	     in simplify_comparison in combine.  */

	  if ((GET_CODE (SET_SRC (set)) == COMPARE
	       || (((code == NE
		     || (code == LT
			 && GET_MODE_CLASS (inner_mode) == MODE_INT
			 && (GET_MODE_BITSIZE (inner_mode)
			     <= HOST_BITS_PER_WIDE_INT)
			 && (STORE_FLAG_VALUE
			     & ((HOST_WIDE_INT) 1
				<< (GET_MODE_BITSIZE (inner_mode) - 1))))
#ifdef FLOAT_STORE_FLAG_VALUE
		     || (code == LT
			 && GET_MODE_CLASS (inner_mode) == MODE_FLOAT
			 && (REAL_VALUE_NEGATIVE
			     (FLOAT_STORE_FLAG_VALUE (inner_mode))))
#endif
		     ))
		   && GET_RTX_CLASS (GET_CODE (SET_SRC (set))) == '<'))
	      && (((GET_MODE_CLASS (mode) == MODE_CC)
		   == (GET_MODE_CLASS (inner_mode) == MODE_CC))
		  || mode == VOIDmode || inner_mode == VOIDmode))
	    x = SET_SRC (set);
	  else if (((code == EQ
		     || (code == GE
			 && (GET_MODE_BITSIZE (inner_mode)
			     <= HOST_BITS_PER_WIDE_INT)
			 && GET_MODE_CLASS (inner_mode) == MODE_INT
			 && (STORE_FLAG_VALUE
			     & ((HOST_WIDE_INT) 1
				<< (GET_MODE_BITSIZE (inner_mode) - 1))))
#ifdef FLOAT_STORE_FLAG_VALUE
		     || (code == GE
			 && GET_MODE_CLASS (inner_mode) == MODE_FLOAT
			 && (REAL_VALUE_NEGATIVE
			     (FLOAT_STORE_FLAG_VALUE (inner_mode))))
#endif
		     ))
		   && GET_RTX_CLASS (GET_CODE (SET_SRC (set))) == '<'
		   && (((GET_MODE_CLASS (mode) == MODE_CC)
			== (GET_MODE_CLASS (inner_mode) == MODE_CC))
		       || mode == VOIDmode || inner_mode == VOIDmode))

	    {
	      reverse_code = 1;
	      x = SET_SRC (set);
	    }
	  else
	    break;
	}

      else if (reg_set_p (op0, prev))
	/* If this sets OP0, but not directly, we have to give up.  */
	break;

      if (x)
	{
	  if (GET_RTX_CLASS (GET_CODE (x)) == '<')
	    code = GET_CODE (x);
	  if (reverse_code)
	    {
	      code = reversed_comparison_code (x, prev);
	      if (code == UNKNOWN)
		return 0;
	      reverse_code = 0;
	    }

	  op0 = XEXP (x, 0), op1 = XEXP (x, 1);
	  if (earliest)
	    *earliest = prev;
	}
    }

  /* If constant is first, put it last.  */
  if (CONSTANT_P (op0))
    code = swap_condition (code), tem = op0, op0 = op1, op1 = tem;

  /* If OP0 is the result of a comparison, we weren't able to find what
     was really being compared, so fail.  */
  if (GET_MODE_CLASS (GET_MODE (op0)) == MODE_CC)
    return 0;

  /* Canonicalize any ordered comparison with integers involving equality
     if we can do computations in the relevant mode and we do not
     overflow.  */

  if (GET_CODE (op1) == CONST_INT
      && GET_MODE (op0) != VOIDmode
      && GET_MODE_BITSIZE (GET_MODE (op0)) <= HOST_BITS_PER_WIDE_INT)
    {
      HOST_WIDE_INT const_val = INTVAL (op1);
      unsigned HOST_WIDE_INT uconst_val = const_val;
      unsigned HOST_WIDE_INT max_val
	= (unsigned HOST_WIDE_INT) GET_MODE_MASK (GET_MODE (op0));

      switch (code)
	{
	case LE:
	  if ((unsigned HOST_WIDE_INT) const_val != max_val >> 1)
	    code = LT, op1 = gen_int_mode (const_val + 1, GET_MODE (op0));
	  break;

	/* When cross-compiling, const_val might be sign-extended from
	   BITS_PER_WORD to HOST_BITS_PER_WIDE_INT */
	case GE:
	  if ((HOST_WIDE_INT) (const_val & max_val)
	      != (((HOST_WIDE_INT) 1
		   << (GET_MODE_BITSIZE (GET_MODE (op0)) - 1))))
	    code = GT, op1 = gen_int_mode (const_val - 1, GET_MODE (op0));
	  break;

	case LEU:
	  if (uconst_val < max_val)
	    code = LTU, op1 = gen_int_mode (uconst_val + 1, GET_MODE (op0));
	  break;

	case GEU:
	  if (uconst_val != 0)
	    code = GTU, op1 = gen_int_mode (uconst_val - 1, GET_MODE (op0));
	  break;

	default:
	  break;
	}
    }

#ifdef HAVE_cc0
  /* Never return CC0; return zero instead.  */
  if (op0 == cc0_rtx)
    return 0;
#endif

  return gen_rtx_fmt_ee (code, VOIDmode, op0, op1);
}

/* Given a jump insn JUMP, return the condition that will cause it to branch
   to its JUMP_LABEL.  If the condition cannot be understood, or is an
   inequality floating-point comparison which needs to be reversed, 0 will
   be returned.

   If EARLIEST is non-zero, it is a pointer to a place where the earliest
   insn used in locating the condition was found.  If a replacement test
   of the condition is desired, it should be placed in front of that
   insn and we will be sure that the inputs are still valid.  */

rtx
get_condition (jump, earliest)
     rtx jump;
     rtx *earliest;
{
  rtx cond;
  int reverse;
  rtx set;

  /* If this is not a standard conditional jump, we can't parse it.  */
  if (GET_CODE (jump) != JUMP_INSN
      || ! any_condjump_p (jump))
    return 0;
  set = pc_set (jump);

  cond = XEXP (SET_SRC (set), 0);

  /* If this branches to JUMP_LABEL when the condition is false, reverse
     the condition.  */
  reverse
    = GET_CODE (XEXP (SET_SRC (set), 2)) == LABEL_REF
      && XEXP (XEXP (SET_SRC (set), 2), 0) == JUMP_LABEL (jump);

  return canonicalize_condition (jump, cond, reverse, earliest, NULL_RTX);
}

/* Similar to above routine, except that we also put an invariant last
   unless both operands are invariants.  */

rtx
get_condition_for_loop (loop, x)
     const struct loop *loop;
     rtx x;
{
  rtx comparison = get_condition (x, (rtx*) 0);

  if (comparison == 0
      || ! loop_invariant_p (loop, XEXP (comparison, 0))
      || loop_invariant_p (loop, XEXP (comparison, 1)))
    return comparison;

  return gen_rtx_fmt_ee (swap_condition (GET_CODE (comparison)), VOIDmode,
			 XEXP (comparison, 1), XEXP (comparison, 0));
}

/* Scan the function and determine whether it has indirect (computed) jumps.

   This is taken mostly from flow.c; similar code exists elsewhere
   in the compiler.  It may be useful to put this into rtlanal.c.  */
static int
indirect_jump_in_function_p (start)
     rtx start;
{
  rtx insn;

  for (insn = start; insn; insn = NEXT_INSN (insn))
    if (computed_jump_p (insn))
      return 1;

  return 0;
}

/* Add MEM to the LOOP_MEMS array, if appropriate.  See the
   documentation for LOOP_MEMS for the definition of `appropriate'.
   This function is called from prescan_loop via for_each_rtx.  */

static int
insert_loop_mem (mem, data)
     rtx *mem;
     void *data ATTRIBUTE_UNUSED;
{
  struct loop_info *loop_info = data;
  int i;
  rtx m = *mem;

  if (m == NULL_RTX)
    return 0;

  switch (GET_CODE (m))
    {
    case MEM:
      break;

    case CLOBBER:
      /* We're not interested in MEMs that are only clobbered.  */
      return -1;

    case CONST_DOUBLE:
      /* We're not interested in the MEM associated with a
	 CONST_DOUBLE, so there's no need to traverse into this.  */
      return -1;

    case EXPR_LIST:
      /* We're not interested in any MEMs that only appear in notes.  */
      return -1;

    default:
      /* This is not a MEM.  */
      return 0;
    }

  /* See if we've already seen this MEM.  */
  for (i = 0; i < loop_info->mems_idx; ++i)
    if (rtx_equal_p (m, loop_info->mems[i].mem))
      {
	if (GET_MODE (m) != GET_MODE (loop_info->mems[i].mem))
	  /* The modes of the two memory accesses are different.  If
	     this happens, something tricky is going on, and we just
	     don't optimize accesses to this MEM.  */
	  loop_info->mems[i].optimize = 0;

	return 0;
      }

  /* Resize the array, if necessary.  */
  if (loop_info->mems_idx == loop_info->mems_allocated)
    {
      if (loop_info->mems_allocated != 0)
	loop_info->mems_allocated *= 2;
      else
	loop_info->mems_allocated = 32;

      loop_info->mems = (loop_mem_info *)
	xrealloc (loop_info->mems,
		  loop_info->mems_allocated * sizeof (loop_mem_info));
    }

  /* Actually insert the MEM.  */
  loop_info->mems[loop_info->mems_idx].mem = m;
  /* We can't hoist this MEM out of the loop if it's a BLKmode MEM
     because we can't put it in a register.  We still store it in the
     table, though, so that if we see the same address later, but in a
     non-BLK mode, we'll not think we can optimize it at that point.  */
  loop_info->mems[loop_info->mems_idx].optimize = (GET_MODE (m) != BLKmode);
  loop_info->mems[loop_info->mems_idx].reg = NULL_RTX;
  ++loop_info->mems_idx;

  return 0;
}


/* Allocate REGS->ARRAY or reallocate it if it is too small.

   Increment REGS->ARRAY[I].SET_IN_LOOP at the index I of each
   register that is modified by an insn between FROM and TO.  If the
   value of an element of REGS->array[I].SET_IN_LOOP becomes 127 or
   more, stop incrementing it, to avoid overflow.

   Store in REGS->ARRAY[I].SINGLE_USAGE the single insn in which
   register I is used, if it is only used once.  Otherwise, it is set
   to 0 (for no uses) or const0_rtx for more than one use.  This
   parameter may be zero, in which case this processing is not done.

   Set REGS->ARRAY[I].MAY_NOT_OPTIMIZE nonzero if we should not
   optimize register I.  */

static void
loop_regs_scan (loop, extra_size)
     const struct loop *loop;
     int extra_size;
{
  struct loop_regs *regs = LOOP_REGS (loop);
  int old_nregs;
  /* last_set[n] is nonzero iff reg n has been set in the current
   basic block.  In that case, it is the insn that last set reg n.  */
  rtx *last_set;
  rtx insn;
  int i;

  old_nregs = regs->num;
  regs->num = max_reg_num ();

  /* Grow the regs array if not allocated or too small.  */
  if (regs->num >= regs->size)
    {
      regs->size = regs->num + extra_size;

      regs->array = (struct loop_reg *)
	xrealloc (regs->array, regs->size * sizeof (*regs->array));

      /* Zero the new elements.  */
      memset (regs->array + old_nregs, 0,
	      (regs->size - old_nregs) * sizeof (*regs->array));
    }

  /* Clear previously scanned fields but do not clear n_times_set.  */
  for (i = 0; i < old_nregs; i++)
    {
      regs->array[i].set_in_loop = 0;
      regs->array[i].may_not_optimize = 0;
      regs->array[i].single_usage = NULL_RTX;
    }

  last_set = (rtx *) xcalloc (regs->num, sizeof (rtx));

  /* Scan the loop, recording register usage.  */
  for (insn = loop->top ? loop->top : loop->start; insn != loop->end;
       insn = NEXT_INSN (insn))
    {
      if (INSN_P (insn))
	{
	  /* Record registers that have exactly one use.  */
	  find_single_use_in_loop (regs, insn, PATTERN (insn));

	  /* Include uses in REG_EQUAL notes.  */
	  if (REG_NOTES (insn))
	    find_single_use_in_loop (regs, insn, REG_NOTES (insn));

	  if (GET_CODE (PATTERN (insn)) == SET
	      || GET_CODE (PATTERN (insn)) == CLOBBER)
	    count_one_set (regs, insn, PATTERN (insn), last_set);
	  else if (GET_CODE (PATTERN (insn)) == PARALLEL)
	    {
	      int i;
	      for (i = XVECLEN (PATTERN (insn), 0) - 1; i >= 0; i--)
		count_one_set (regs, insn, XVECEXP (PATTERN (insn), 0, i),
			       last_set);
	    }
	}

      if (GET_CODE (insn) == CODE_LABEL || GET_CODE (insn) == JUMP_INSN)
	memset (last_set, 0, regs->num * sizeof (rtx));
    }

  /* Invalidate all hard registers clobbered by calls.  With one exception:
     a call-clobbered PIC register is still function-invariant for our
     purposes, since we can hoist any PIC calculations out of the loop.
     Thus the call to rtx_varies_p.  */
  if (LOOP_INFO (loop)->has_call)
    for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
      if (TEST_HARD_REG_BIT (regs_invalidated_by_call, i)
          && rtx_varies_p (gen_rtx_REG (Pmode, i), /*for_alias=*/1))
        {
          regs->array[i].may_not_optimize = 1;
          regs->array[i].set_in_loop = 1;
        }

#ifdef AVOID_CCMODE_COPIES
  /* Don't try to move insns which set CC registers if we should not
     create CCmode register copies.  */
  for (i = regs->num - 1; i >= FIRST_PSEUDO_REGISTER; i--)
    if (GET_MODE_CLASS (GET_MODE (regno_reg_rtx[i])) == MODE_CC)
      regs->array[i].may_not_optimize = 1;
#endif

  /* Set regs->array[I].n_times_set for the new registers.  */
  for (i = old_nregs; i < regs->num; i++)
    regs->array[i].n_times_set = regs->array[i].set_in_loop;

  free (last_set);
}

/* Returns the number of real INSNs in the LOOP.  */

static int
count_insns_in_loop (loop)
     const struct loop *loop;
{
  int count = 0;
  rtx insn;

  for (insn = loop->top ? loop->top : loop->start; insn != loop->end;
       insn = NEXT_INSN (insn))
    if (INSN_P (insn))
      ++count;

  return count;
}

/* Move MEMs into registers for the duration of the loop.  */

static void
load_mems (loop)
     const struct loop *loop;
{
  struct loop_info *loop_info = LOOP_INFO (loop);
  struct loop_regs *regs = LOOP_REGS (loop);
  int maybe_never = 0;
  int i;
  rtx p, prev_ebb_head;
  rtx label = NULL_RTX;
  rtx end_label;
  /* Nonzero if the next instruction may never be executed.  */
  int next_maybe_never = 0;
  unsigned int last_max_reg = max_reg_num ();

  if (loop_info->mems_idx == 0)
    return;

  /* We cannot use next_label here because it skips over normal insns.  */
  end_label = next_nonnote_insn (loop->end);
  if (end_label && GET_CODE (end_label) != CODE_LABEL)
    end_label = NULL_RTX;

  /* Check to see if it's possible that some instructions in the loop are
     never executed.  Also check if there is a goto out of the loop other
     than right after the end of the loop.  */
  for (p = next_insn_in_loop (loop, loop->scan_start);
       p != NULL_RTX;
       p = next_insn_in_loop (loop, p))
    {
      if (GET_CODE (p) == CODE_LABEL)
	maybe_never = 1;
      else if (GET_CODE (p) == JUMP_INSN
	       /* If we enter the loop in the middle, and scan
		  around to the beginning, don't set maybe_never
		  for that.  This must be an unconditional jump,
		  otherwise the code at the top of the loop might
		  never be executed.  Unconditional jumps are
		  followed a by barrier then loop end.  */
	       && ! (GET_CODE (p) == JUMP_INSN
		     && JUMP_LABEL (p) == loop->top
		     && NEXT_INSN (NEXT_INSN (p)) == loop->end
		     && any_uncondjump_p (p)))
	{
	  /* If this is a jump outside of the loop but not right
	     after the end of the loop, we would have to emit new fixup
	     sequences for each such label.  */
	  if (/* If we can't tell where control might go when this
		 JUMP_INSN is executed, we must be conservative.  */
	      !JUMP_LABEL (p)
	      || (JUMP_LABEL (p) != end_label
		  && (INSN_UID (JUMP_LABEL (p)) >= max_uid_for_loop
		      || INSN_LUID (JUMP_LABEL (p)) < INSN_LUID (loop->start)
		      || INSN_LUID (JUMP_LABEL (p)) > INSN_LUID (loop->end))))
	    return;

	  if (!any_condjump_p (p))
	    /* Something complicated.  */
	    maybe_never = 1;
	  else
	    /* If there are any more instructions in the loop, they
	       might not be reached.  */
	    next_maybe_never = 1;
	}
      else if (next_maybe_never)
	maybe_never = 1;
    }

  /* Find start of the extended basic block that enters the loop.  */
  for (p = loop->start;
       PREV_INSN (p) && GET_CODE (p) != CODE_LABEL;
       p = PREV_INSN (p))
    ;
  prev_ebb_head = p;

  cselib_init ();

  /* Build table of mems that get set to constant values before the
     loop.  */
  for (; p != loop->start; p = NEXT_INSN (p))
    cselib_process_insn (p);

  /* Actually move the MEMs.  */
  for (i = 0; i < loop_info->mems_idx; ++i)
    {
      regset_head load_copies;
      regset_head store_copies;
      int written = 0;
      rtx reg;
      rtx mem = loop_info->mems[i].mem;
      rtx mem_list_entry;

      if (MEM_VOLATILE_P (mem)
	  || loop_invariant_p (loop, XEXP (mem, 0)) != 1)
	/* There's no telling whether or not MEM is modified.  */
	loop_info->mems[i].optimize = 0;

      /* Go through the MEMs written to in the loop to see if this
	 one is aliased by one of them.  */
      mem_list_entry = loop_info->store_mems;
      while (mem_list_entry)
	{
	  if (rtx_equal_p (mem, XEXP (mem_list_entry, 0)))
	    written = 1;
	  else if (true_dependence (XEXP (mem_list_entry, 0), VOIDmode,
				    mem, rtx_varies_p))
	    {
	      /* MEM is indeed aliased by this store.  */
	      loop_info->mems[i].optimize = 0;
	      break;
	    }
	  mem_list_entry = XEXP (mem_list_entry, 1);
	}

      if (flag_float_store && written
	  && GET_MODE_CLASS (GET_MODE (mem)) == MODE_FLOAT)
	loop_info->mems[i].optimize = 0;

      /* If this MEM is written to, we must be sure that there
	 are no reads from another MEM that aliases this one.  */
      if (loop_info->mems[i].optimize && written)
	{
	  int j;

	  for (j = 0; j < loop_info->mems_idx; ++j)
	    {
	      if (j == i)
		continue;
	      else if (true_dependence (mem,
					VOIDmode,
					loop_info->mems[j].mem,
					rtx_varies_p))
		{
		  /* It's not safe to hoist loop_info->mems[i] out of
		     the loop because writes to it might not be
		     seen by reads from loop_info->mems[j].  */
		  loop_info->mems[i].optimize = 0;
		  break;
		}
	    }
	}

      if (maybe_never && may_trap_p (mem))
	/* We can't access the MEM outside the loop; it might
	   cause a trap that wouldn't have happened otherwise.  */
	loop_info->mems[i].optimize = 0;

      if (!loop_info->mems[i].optimize)
	/* We thought we were going to lift this MEM out of the
	   loop, but later discovered that we could not.  */
	continue;

      INIT_REG_SET (&load_copies);
      INIT_REG_SET (&store_copies);

      /* Allocate a pseudo for this MEM.  We set REG_USERVAR_P in
	 order to keep scan_loop from moving stores to this MEM
	 out of the loop just because this REG is neither a
	 user-variable nor used in the loop test.  */
      reg = gen_reg_rtx (GET_MODE (mem));
      REG_USERVAR_P (reg) = 1;
      loop_info->mems[i].reg = reg;

      /* Now, replace all references to the MEM with the
	 corresponding pseudos.  */
      maybe_never = 0;
      for (p = next_insn_in_loop (loop, loop->scan_start);
	   p != NULL_RTX;
	   p = next_insn_in_loop (loop, p))
	{
	  if (INSN_P (p))
	    {
	      rtx set;

	      set = single_set (p);

	      /* See if this copies the mem into a register that isn't
		 modified afterwards.  We'll try to do copy propagation
		 a little further on.  */
	      if (set
		  /* @@@ This test is _way_ too conservative.  */
		  && ! maybe_never
		  && GET_CODE (SET_DEST (set)) == REG
		  && REGNO (SET_DEST (set)) >= FIRST_PSEUDO_REGISTER
		  && REGNO (SET_DEST (set)) < last_max_reg
		  && regs->array[REGNO (SET_DEST (set))].n_times_set == 1
		  && rtx_equal_p (SET_SRC (set), mem))
		SET_REGNO_REG_SET (&load_copies, REGNO (SET_DEST (set)));

	      /* See if this copies the mem from a register that isn't
		 modified afterwards.  We'll try to remove the
		 redundant copy later on by doing a little register
		 renaming and copy propagation.   This will help
		 to untangle things for the BIV detection code.  */
	      if (set
		  && ! maybe_never
		  && GET_CODE (SET_SRC (set)) == REG
		  && REGNO (SET_SRC (set)) >= FIRST_PSEUDO_REGISTER
		  && REGNO (SET_SRC (set)) < last_max_reg
		  && regs->array[REGNO (SET_SRC (set))].n_times_set == 1
		  && rtx_equal_p (SET_DEST (set), mem))
		SET_REGNO_REG_SET (&store_copies, REGNO (SET_SRC (set)));

	      /* Replace the memory reference with the shadow register.  */
	      replace_loop_mems (p, loop_info->mems[i].mem,
				 loop_info->mems[i].reg);
	    }

	  if (GET_CODE (p) == CODE_LABEL
	      || GET_CODE (p) == JUMP_INSN)
	    maybe_never = 1;
	}

      if (! apply_change_group ())
	/* We couldn't replace all occurrences of the MEM.  */
	loop_info->mems[i].optimize = 0;
      else
	{
	  /* Load the memory immediately before LOOP->START, which is
	     the NOTE_LOOP_BEG.  */
	  cselib_val *e = cselib_lookup (mem, VOIDmode, 0);
	  rtx set;
	  rtx best = mem;
	  int j;
	  struct elt_loc_list *const_equiv = 0;

	  if (e)
	    {
	      struct elt_loc_list *equiv;
	      struct elt_loc_list *best_equiv = 0;
	      for (equiv = e->locs; equiv; equiv = equiv->next)
		{
		  if (CONSTANT_P (equiv->loc))
		    const_equiv = equiv;
		  else if (GET_CODE (equiv->loc) == REG
			   /* Extending hard register lifetimes causes crash
			      on SRC targets.  Doing so on non-SRC is
			      probably also not good idea, since we most
			      probably have pseudoregister equivalence as
			      well.  */
			   && REGNO (equiv->loc) >= FIRST_PSEUDO_REGISTER)
		    best_equiv = equiv;
		}
	      /* Use the constant equivalence if that is cheap enough.  */
	      if (! best_equiv)
		best_equiv = const_equiv;
	      else if (const_equiv
		       && (rtx_cost (const_equiv->loc, SET)
			   <= rtx_cost (best_equiv->loc, SET)))
		{
		  best_equiv = const_equiv;
		  const_equiv = 0;
		}

	      /* If best_equiv is nonzero, we know that MEM is set to a
		 constant or register before the loop.  We will use this
		 knowledge to initialize the shadow register with that
		 constant or reg rather than by loading from MEM.  */
	      if (best_equiv)
		best = copy_rtx (best_equiv->loc);
	    }

	  set = gen_move_insn (reg, best);
	  set = loop_insn_hoist (loop, set);
	  if (REG_P (best))
	    {
	      for (p = prev_ebb_head; p != loop->start; p = NEXT_INSN (p))
		if (REGNO_LAST_UID (REGNO (best)) == INSN_UID (p))
		  {
		    REGNO_LAST_UID (REGNO (best)) = INSN_UID (set);
		    break;
		  }
	    }

	  if (const_equiv)
	    set_unique_reg_note (set, REG_EQUAL, copy_rtx (const_equiv->loc));

	  if (written)
	    {
	      if (label == NULL_RTX)
		{
		  label = gen_label_rtx ();
		  emit_label_after (label, loop->end);
		}

	      /* Store the memory immediately after END, which is
		 the NOTE_LOOP_END.  */
	      set = gen_move_insn (copy_rtx (mem), reg);
	      loop_insn_emit_after (loop, 0, label, set);
	    }

	  if (loop_dump_stream)
	    {
	      fprintf (loop_dump_stream, "Hoisted regno %d %s from ",
		       REGNO (reg), (written ? "r/w" : "r/o"));
	      print_rtl (loop_dump_stream, mem);
	      fputc ('\n', loop_dump_stream);
	    }

	  /* Attempt a bit of copy propagation.  This helps untangle the
	     data flow, and enables {basic,general}_induction_var to find
	     more bivs/givs.  */
	  EXECUTE_IF_SET_IN_REG_SET
	    (&load_copies, FIRST_PSEUDO_REGISTER, j,
	     {
	       try_copy_prop (loop, reg, j);
	     });
	  CLEAR_REG_SET (&load_copies);

	  EXECUTE_IF_SET_IN_REG_SET
	    (&store_copies, FIRST_PSEUDO_REGISTER, j,
	     {
	       try_swap_copy_prop (loop, reg, j);
	     });
	  CLEAR_REG_SET (&store_copies);
	}
    }

  if (label != NULL_RTX && end_label != NULL_RTX)
    {
      /* Now, we need to replace all references to the previous exit
	 label with the new one.  */
      rtx_pair rr;
      rr.r1 = end_label;
      rr.r2 = label;

      for (p = loop->start; p != loop->end; p = NEXT_INSN (p))
	{
	  for_each_rtx (&p, replace_label, &rr);

	  /* If this is a JUMP_INSN, then we also need to fix the JUMP_LABEL
	     field.  This is not handled by for_each_rtx because it doesn't
	     handle unprinted ('0') fields.  We need to update JUMP_LABEL
	     because the immediately following unroll pass will use it.
	     replace_label would not work anyways, because that only handles
	     LABEL_REFs.  */
	  if (GET_CODE (p) == JUMP_INSN && JUMP_LABEL (p) == end_label)
	    JUMP_LABEL (p) = label;
	}
    }

  cselib_finish ();
}

/* For communication between note_reg_stored and its caller.  */
struct note_reg_stored_arg
{
  int set_seen;
  rtx reg;
};

/* Called via note_stores, record in SET_SEEN whether X, which is written,
   is equal to ARG.  */
static void
note_reg_stored (x, setter, arg)
     rtx x, setter ATTRIBUTE_UNUSED;
     void *arg;
{
  struct note_reg_stored_arg *t = (struct note_reg_stored_arg *) arg;
  if (t->reg == x)
    t->set_seen = 1;
}

/* Try to replace every occurrence of pseudo REGNO with REPLACEMENT.
   There must be exactly one insn that sets this pseudo; it will be
   deleted if all replacements succeed and we can prove that the register
   is not used after the loop.  */

static void
try_copy_prop (loop, replacement, regno)
     const struct loop *loop;
     rtx replacement;
     unsigned int regno;
{
  /* This is the reg that we are copying from.  */
  rtx reg_rtx = regno_reg_rtx[regno];
  rtx init_insn = 0;
  rtx insn;
  /* These help keep track of whether we replaced all uses of the reg.  */
  int replaced_last = 0;
  int store_is_first = 0;

  for (insn = next_insn_in_loop (loop, loop->scan_start);
       insn != NULL_RTX;
       insn = next_insn_in_loop (loop, insn))
    {
      rtx set;

      /* Only substitute within one extended basic block from the initializing
         insn.  */
      if (GET_CODE (insn) == CODE_LABEL && init_insn)
	break;

      if (! INSN_P (insn))
	continue;

      /* Is this the initializing insn?  */
      set = single_set (insn);
      if (set
	  && GET_CODE (SET_DEST (set)) == REG
	  && REGNO (SET_DEST (set)) == regno)
	{
	  if (init_insn)
	    abort ();

	  init_insn = insn;
	  if (REGNO_FIRST_UID (regno) == INSN_UID (insn))
	    store_is_first = 1;
	}

      /* Only substitute after seeing the initializing insn.  */
      if (init_insn && insn != init_insn)
	{
	  struct note_reg_stored_arg arg;

	  replace_loop_regs (insn, reg_rtx, replacement);
	  if (REGNO_LAST_UID (regno) == INSN_UID (insn))
	    replaced_last = 1;

	  /* Stop replacing when REPLACEMENT is modified.  */
	  arg.reg = replacement;
	  arg.set_seen = 0;
	  note_stores (PATTERN (insn), note_reg_stored, &arg);
	  if (arg.set_seen)
	    {
	      rtx note = find_reg_note (insn, REG_EQUAL, NULL);

	      /* It is possible that we've turned previously valid REG_EQUAL to
	         invalid, as we change the REGNO to REPLACEMENT and unlike REGNO,
	         REPLACEMENT is modified, we get different meaning.  */
	      if (note && reg_mentioned_p (replacement, XEXP (note, 0)))
		remove_note (insn, note);
	      break;
	    }
	}
    }
  if (! init_insn)
    abort ();
  if (apply_change_group ())
    {
      if (loop_dump_stream)
	fprintf (loop_dump_stream, "  Replaced reg %d", regno);
      if (store_is_first && replaced_last)
	{
	  rtx first;
	  rtx retval_note;

	  /* Assume we're just deleting INIT_INSN.  */
	  first = init_insn;
	  /* Look for REG_RETVAL note.  If we're deleting the end of
	     the libcall sequence, the whole sequence can go.  */
	  retval_note = find_reg_note (init_insn, REG_RETVAL, NULL_RTX);
	  /* If we found a REG_RETVAL note, find the first instruction
	     in the sequence.  */
	  if (retval_note)
	    first = XEXP (retval_note, 0);

	  /* Delete the instructions.  */
	  loop_delete_insns (first, init_insn);
	}
      if (loop_dump_stream)
	fprintf (loop_dump_stream, ".\n");
    }
}

/* Replace all the instructions from FIRST up to and including LAST
   with NOTE_INSN_DELETED notes.  */

static void
loop_delete_insns (first, last)
     rtx first;
     rtx last;
{
  while (1)
    {
      if (loop_dump_stream)
	fprintf (loop_dump_stream, ", deleting init_insn (%d)",
		 INSN_UID (first));
      delete_insn (first);

      /* If this was the LAST instructions we're supposed to delete,
	 we're done.  */
      if (first == last)
	break;

      first = NEXT_INSN (first);
    }
}

/* Try to replace occurrences of pseudo REGNO with REPLACEMENT within
   loop LOOP if the order of the sets of these registers can be
   swapped.  There must be exactly one insn within the loop that sets
   this pseudo followed immediately by a move insn that sets
   REPLACEMENT with REGNO.  */
static void
try_swap_copy_prop (loop, replacement, regno)
     const struct loop *loop;
     rtx replacement;
     unsigned int regno;
{
  rtx insn;
  rtx set = NULL_RTX;
  unsigned int new_regno;

  new_regno = REGNO (replacement);

  for (insn = next_insn_in_loop (loop, loop->scan_start);
       insn != NULL_RTX;
       insn = next_insn_in_loop (loop, insn))
    {
      /* Search for the insn that copies REGNO to NEW_REGNO?  */
      if (INSN_P (insn)
	  && (set = single_set (insn))
	  && GET_CODE (SET_DEST (set)) == REG
	  && REGNO (SET_DEST (set)) == new_regno
	  && GET_CODE (SET_SRC (set)) == REG
	  && REGNO (SET_SRC (set)) == regno)
	break;
    }

  if (insn != NULL_RTX)
    {
      rtx prev_insn;
      rtx prev_set;

      /* Some DEF-USE info would come in handy here to make this
	 function more general.  For now, just check the previous insn
	 which is the most likely candidate for setting REGNO.  */

      prev_insn = PREV_INSN (insn);

      if (INSN_P (insn)
	  && (prev_set = single_set (prev_insn))
	  && GET_CODE (SET_DEST (prev_set)) == REG
	  && REGNO (SET_DEST (prev_set)) == regno)
	{
	  /* We have:
	     (set (reg regno) (expr))
	     (set (reg new_regno) (reg regno))

	     so try converting this to:
	     (set (reg new_regno) (expr))
	     (set (reg regno) (reg new_regno))

	     The former construct is often generated when a global
	     variable used for an induction variable is shadowed by a
	     register (NEW_REGNO).  The latter construct improves the
	     chances of GIV replacement and BIV elimination.  */

	  validate_change (prev_insn, &SET_DEST (prev_set),
			   replacement, 1);
	  validate_change (insn, &SET_DEST (set),
			   SET_SRC (set), 1);
	  validate_change (insn, &SET_SRC (set),
			   replacement, 1);

	  if (apply_change_group ())
	    {
	      if (loop_dump_stream)
		fprintf (loop_dump_stream,
			 "  Swapped set of reg %d at %d with reg %d at %d.\n",
			 regno, INSN_UID (insn),
			 new_regno, INSN_UID (prev_insn));

	      /* Update first use of REGNO.  */
	      if (REGNO_FIRST_UID (regno) == INSN_UID (prev_insn))
		REGNO_FIRST_UID (regno) = INSN_UID (insn);

	      /* Now perform copy propagation to hopefully
		 remove all uses of REGNO within the loop.  */
	      try_copy_prop (loop, replacement, regno);
	    }
	}
    }
}

/* Replace MEM with its associated pseudo register.  This function is
   called from load_mems via for_each_rtx.  DATA is actually a pointer
   to a structure describing the instruction currently being scanned
   and the MEM we are currently replacing.  */

static int
replace_loop_mem (mem, data)
     rtx *mem;
     void *data;
{
  loop_replace_args *args = (loop_replace_args *) data;
  rtx m = *mem;

  if (m == NULL_RTX)
    return 0;

  switch (GET_CODE (m))
    {
    case MEM:
      break;

    case CONST_DOUBLE:
      /* We're not interested in the MEM associated with a
	 CONST_DOUBLE, so there's no need to traverse into one.  */
      return -1;

    default:
      /* This is not a MEM.  */
      return 0;
    }

  if (!rtx_equal_p (args->match, m))
    /* This is not the MEM we are currently replacing.  */
    return 0;

  /* Actually replace the MEM.  */
  validate_change (args->insn, mem, args->replacement, 1);

  return 0;
}

static void
replace_loop_mems (insn, mem, reg)
     rtx insn;
     rtx mem;
     rtx reg;
{
  loop_replace_args args;

  args.insn = insn;
  args.match = mem;
  args.replacement = reg;

  for_each_rtx (&insn, replace_loop_mem, &args);
}

/* Replace one register with another.  Called through for_each_rtx; PX points
   to the rtx being scanned.  DATA is actually a pointer to
   a structure of arguments.  */

static int
replace_loop_reg (px, data)
     rtx *px;
     void *data;
{
  rtx x = *px;
  loop_replace_args *args = (loop_replace_args *) data;

  if (x == NULL_RTX)
    return 0;

  if (x == args->match)
    validate_change (args->insn, px, args->replacement, 1);

  return 0;
}

static void
replace_loop_regs (insn, reg, replacement)
     rtx insn;
     rtx reg;
     rtx replacement;
{
  loop_replace_args args;

  args.insn = insn;
  args.match = reg;
  args.replacement = replacement;

  for_each_rtx (&insn, replace_loop_reg, &args);
}

/* Replace occurrences of the old exit label for the loop with the new
   one.  DATA is an rtx_pair containing the old and new labels,
   respectively.  */

static int
replace_label (x, data)
     rtx *x;
     void *data;
{
  rtx l = *x;
  rtx old_label = ((rtx_pair *) data)->r1;
  rtx new_label = ((rtx_pair *) data)->r2;

  if (l == NULL_RTX)
    return 0;

  if (GET_CODE (l) != LABEL_REF)
    return 0;

  if (XEXP (l, 0) != old_label)
    return 0;

  XEXP (l, 0) = new_label;
  ++LABEL_NUSES (new_label);
  --LABEL_NUSES (old_label);

  return 0;
}

/* Emit insn for PATTERN after WHERE_INSN in basic block WHERE_BB
   (ignored in the interim).  */

static rtx
loop_insn_emit_after (loop, where_bb, where_insn, pattern)
     const struct loop *loop ATTRIBUTE_UNUSED;
     basic_block where_bb ATTRIBUTE_UNUSED;
     rtx where_insn;
     rtx pattern;
{
  return emit_insn_after (pattern, where_insn);
}


/* If WHERE_INSN is non-zero emit insn for PATTERN before WHERE_INSN
   in basic block WHERE_BB (ignored in the interim) within the loop
   otherwise hoist PATTERN into the loop pre-header.  */

rtx
loop_insn_emit_before (loop, where_bb, where_insn, pattern)
     const struct loop *loop;
     basic_block where_bb ATTRIBUTE_UNUSED;
     rtx where_insn;
     rtx pattern;
{
  if (! where_insn)
    return loop_insn_hoist (loop, pattern);
  return emit_insn_before (pattern, where_insn);
}


/* Emit call insn for PATTERN before WHERE_INSN in basic block
   WHERE_BB (ignored in the interim) within the loop.  */

static rtx
loop_call_insn_emit_before (loop, where_bb, where_insn, pattern)
     const struct loop *loop ATTRIBUTE_UNUSED;
     basic_block where_bb ATTRIBUTE_UNUSED;
     rtx where_insn;
     rtx pattern;
{
  return emit_call_insn_before (pattern, where_insn);
}


/* Hoist insn for PATTERN into the loop pre-header.  */

rtx
loop_insn_hoist (loop, pattern)
     const struct loop *loop;
     rtx pattern;
{
  return loop_insn_emit_before (loop, 0, loop->start, pattern);
}


/* Hoist call insn for PATTERN into the loop pre-header.  */

static rtx
loop_call_insn_hoist (loop, pattern)
     const struct loop *loop;
     rtx pattern;
{
  return loop_call_insn_emit_before (loop, 0, loop->start, pattern);
}


/* Sink insn for PATTERN after the loop end.  */

rtx
loop_insn_sink (loop, pattern)
     const struct loop *loop;
     rtx pattern;
{
  return loop_insn_emit_before (loop, 0, loop->sink, pattern);
}


/* If the loop has multiple exits, emit insn for PATTERN before the
   loop to ensure that it will always be executed no matter how the
   loop exits.  Otherwise, emit the insn for PATTERN after the loop,
   since this is slightly more efficient.  */

static rtx
loop_insn_sink_or_swim (loop, pattern)
     const struct loop *loop;
     rtx pattern;
{
  if (loop->exit_count)
    return loop_insn_hoist (loop, pattern);
  else
    return loop_insn_sink (loop, pattern);
}

static void
loop_ivs_dump (loop, file, verbose)
     const struct loop *loop;
     FILE *file;
     int verbose;
{
  struct iv_class *bl;
  int iv_num = 0;

  if (! loop || ! file)
    return;

  for (bl = LOOP_IVS (loop)->list; bl; bl = bl->next)
    iv_num++;

  fprintf (file, "Loop %d: %d IV classes\n", loop->num, iv_num);

  for (bl = LOOP_IVS (loop)->list; bl; bl = bl->next)
    {
      loop_iv_class_dump (bl, file, verbose);
      fputc ('\n', file);
    }
}


static void
loop_iv_class_dump (bl, file, verbose)
     const struct iv_class *bl;
     FILE *file;
     int verbose ATTRIBUTE_UNUSED;
{
  struct induction *v;
  rtx incr;
  int i;

  if (! bl || ! file)
    return;

  fprintf (file, "IV class for reg %d, benefit %d\n",
	   bl->regno, bl->total_benefit);

  fprintf (file, " Init insn %d", INSN_UID (bl->init_insn));
  if (bl->initial_value)
    {
      fprintf (file, ", init val: ");
      print_simple_rtl (file, bl->initial_value);
    }
  if (bl->initial_test)
    {
      fprintf (file, ", init test: ");
      print_simple_rtl (file, bl->initial_test);
    }
  fputc ('\n', file);

  if (bl->final_value)
    {
      fprintf (file, " Final val: ");
      print_simple_rtl (file, bl->final_value);
      fputc ('\n', file);
    }

  if ((incr = biv_total_increment (bl)))
    {
      fprintf (file, " Total increment: ");
      print_simple_rtl (file, incr);
      fputc ('\n', file);
    }

  /* List the increments.  */
  for (i = 0, v = bl->biv; v; v = v->next_iv, i++)
    {
      fprintf (file, " Inc%d: insn %d, incr: ", i, INSN_UID (v->insn));
      print_simple_rtl (file, v->add_val);
      fputc ('\n', file);
    }

  /* List the givs.  */
  for (i = 0, v = bl->giv; v; v = v->next_iv, i++)
    {
      fprintf (file, " Giv%d: insn %d, benefit %d, ",
	       i, INSN_UID (v->insn), v->benefit);
      if (v->giv_type == DEST_ADDR)
	  print_simple_rtl (file, v->mem);
      else
	  print_simple_rtl (file, single_set (v->insn));
      fputc ('\n', file);
    }
}


static void
loop_biv_dump (v, file, verbose)
     const struct induction *v;
     FILE *file;
     int verbose;
{
  if (! v || ! file)
    return;

  fprintf (file,
	   "Biv %d: insn %d",
	   REGNO (v->dest_reg), INSN_UID (v->insn));
  fprintf (file, " const ");
  print_simple_rtl (file, v->add_val);

  if (verbose && v->final_value)
    {
      fputc ('\n', file);
      fprintf (file, " final ");
      print_simple_rtl (file, v->final_value);
    }

  fputc ('\n', file);
}


static void
loop_giv_dump (v, file, verbose)
     const struct induction *v;
     FILE *file;
     int verbose;
{
  if (! v || ! file)
    return;

  if (v->giv_type == DEST_REG)
    fprintf (file, "Giv %d: insn %d",
	     REGNO (v->dest_reg),  INSN_UID (v->insn));
  else
    fprintf (file, "Dest address: insn %d",
	     INSN_UID (v->insn));

  fprintf (file, " src reg %d benefit %d",
	   REGNO (v->src_reg), v->benefit);
  fprintf (file, " lifetime %d",
	   v->lifetime);

  if (v->replaceable)
    fprintf (file, " replaceable");

  if (v->no_const_addval)
    fprintf (file, " ncav");

  if (v->ext_dependent)
    {
      switch (GET_CODE (v->ext_dependent))
	{
	case SIGN_EXTEND:
	  fprintf (file, " ext se");
	  break;
	case ZERO_EXTEND:
	  fprintf (file, " ext ze");
	  break;
	case TRUNCATE:
	  fprintf (file, " ext tr");
	  break;
	default:
	  abort ();
	}
    }

  fputc ('\n', file);
  fprintf (file, " mult ");
  print_simple_rtl (file, v->mult_val);

  fputc ('\n', file);
  fprintf (file, " add  ");
  print_simple_rtl (file, v->add_val);

  if (verbose && v->final_value)
    {
      fputc ('\n', file);
      fprintf (file, " final ");
      print_simple_rtl (file, v->final_value);
    }

  fputc ('\n', file);
}


void
debug_ivs (loop)
     const struct loop *loop;
{
  loop_ivs_dump (loop, stderr, 1);
}


void
debug_iv_class (bl)
     const struct iv_class *bl;
{
  loop_iv_class_dump (bl, stderr, 1);
}


void
debug_biv (v)
     const struct induction *v;
{
  loop_biv_dump (v, stderr, 1);
}


void
debug_giv (v)
     const struct induction *v;
{
  loop_giv_dump (v, stderr, 1);
}


#define LOOP_BLOCK_NUM_1(INSN) \
((INSN) ? (BLOCK_FOR_INSN (INSN) ? BLOCK_NUM (INSN) : - 1) : -1)

/* The notes do not have an assigned block, so look at the next insn.  */
#define LOOP_BLOCK_NUM(INSN) \
((INSN) ? (GET_CODE (INSN) == NOTE \
            ? LOOP_BLOCK_NUM_1 (next_nonnote_insn (INSN)) \
            : LOOP_BLOCK_NUM_1 (INSN)) \
        : -1)

#define LOOP_INSN_UID(INSN) ((INSN) ? INSN_UID (INSN) : -1)

static void
loop_dump_aux (loop, file, verbose)
     const struct loop *loop;
     FILE *file;
     int verbose ATTRIBUTE_UNUSED;
{
  rtx label;

  if (! loop || ! file)
    return;

  /* Print diagnostics to compare our concept of a loop with
     what the loop notes say.  */
  if (! PREV_INSN (loop->first->head)
      || GET_CODE (PREV_INSN (loop->first->head)) != NOTE
      || NOTE_LINE_NUMBER (PREV_INSN (loop->first->head))
      != NOTE_INSN_LOOP_BEG)
    fprintf (file, ";;  No NOTE_INSN_LOOP_BEG at %d\n",
	     INSN_UID (PREV_INSN (loop->first->head)));
  if (! NEXT_INSN (loop->last->end)
      || GET_CODE (NEXT_INSN (loop->last->end)) != NOTE
      || NOTE_LINE_NUMBER (NEXT_INSN (loop->last->end))
      != NOTE_INSN_LOOP_END)
    fprintf (file, ";;  No NOTE_INSN_LOOP_END at %d\n",
	     INSN_UID (NEXT_INSN (loop->last->end)));

  if (loop->start)
    {
      fprintf (file,
	       ";;  start %d (%d), cont dom %d (%d), cont %d (%d), vtop %d (%d), end %d (%d)\n",
	       LOOP_BLOCK_NUM (loop->start),
	       LOOP_INSN_UID (loop->start),
	       LOOP_BLOCK_NUM (loop->cont),
	       LOOP_INSN_UID (loop->cont),
	       LOOP_BLOCK_NUM (loop->cont),
	       LOOP_INSN_UID (loop->cont),
	       LOOP_BLOCK_NUM (loop->vtop),
	       LOOP_INSN_UID (loop->vtop),
	       LOOP_BLOCK_NUM (loop->end),
	       LOOP_INSN_UID (loop->end));
      fprintf (file, ";;  top %d (%d), scan start %d (%d)\n",
	       LOOP_BLOCK_NUM (loop->top),
	       LOOP_INSN_UID (loop->top),
	       LOOP_BLOCK_NUM (loop->scan_start),
	       LOOP_INSN_UID (loop->scan_start));
      fprintf (file, ";;  exit_count %d", loop->exit_count);
      if (loop->exit_count)
	{
	  fputs (", labels:", file);
	  for (label = loop->exit_labels; label; label = LABEL_NEXTREF (label))
	    {
	      fprintf (file, " %d ",
		       LOOP_INSN_UID (XEXP (label, 0)));
	    }
	}
      fputs ("\n", file);

      /* This can happen when a marked loop appears as two nested loops,
	 say from while (a || b) {}.  The inner loop won't match
	 the loop markers but the outer one will.  */
      if (LOOP_BLOCK_NUM (loop->cont) != loop->latch->index)
	fprintf (file, ";;  NOTE_INSN_LOOP_CONT not in loop latch\n");
    }
}

/* Call this function from the debugger to dump LOOP.  */

void
debug_loop (loop)
     const struct loop *loop;
{
  flow_loop_dump (loop, stderr, loop_dump_aux, 1);
}

/* Call this function from the debugger to dump LOOPS.  */

void
debug_loops (loops)
     const struct loops *loops;
{
  flow_loops_dump (loops, stderr, loop_dump_aux, 1);
}
