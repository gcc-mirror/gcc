/* Perform various loop optimizations, including strength reduction.
   Copyright (C) 1987, 88, 89, 91-4, 1995 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


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

#include <stdio.h>
#include "config.h"
#include "rtl.h"
#include "obstack.h"
#include "expr.h"
#include "insn-config.h"
#include "insn-flags.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "recog.h"
#include "flags.h"
#include "real.h"
#include "loop.h"

/* Vector mapping INSN_UIDs to luids.
   The luids are like uids but increase monotonically always.
   We use them to see whether a jump comes from outside a given loop.  */

int *uid_luid;

/* Indexed by INSN_UID, contains the ordinal giving the (innermost) loop
   number the insn is contained in.  */

int *uid_loop_num;

/* 1 + largest uid of any insn.  */

int max_uid_for_loop;

/* 1 + luid of last insn.  */

static int max_luid;

/* Number of loops detected in current function.  Used as index to the
   next few tables.  */

static int max_loop_num;

/* Indexed by loop number, contains the first and last insn of each loop.  */

static rtx *loop_number_loop_starts, *loop_number_loop_ends;

/* For each loop, gives the containing loop number, -1 if none.  */

int *loop_outer_loop;

/* Indexed by loop number, contains a nonzero value if the "loop" isn't
   really a loop (an insn outside the loop branches into it).  */

static char *loop_invalid;

/* Indexed by loop number, links together all LABEL_REFs which refer to
   code labels outside the loop.  Used by routines that need to know all
   loop exits, such as final_biv_value and final_giv_value.

   This does not include loop exits due to return instructions.  This is
   because all bivs and givs are pseudos, and hence must be dead after a
   return, so the presense of a return does not affect any of the
   optimizations that use this info.  It is simpler to just not include return
   instructions on this list.  */

rtx *loop_number_exit_labels;

/* Indexed by loop number, counts the number of LABEL_REFs on
   loop_number_exit_labels for this loop and all loops nested inside it.  */

int *loop_number_exit_count;

/* Holds the number of loop iterations.  It is zero if the number could not be
   calculated.  Must be unsigned since the number of iterations can
   be as high as 2^wordsize-1.  For loops with a wider iterator, this number
   will will be zero if the number of loop iterations is too large for an
   unsigned integer to hold.  */

unsigned HOST_WIDE_INT loop_n_iterations;

/* Nonzero if there is a subroutine call in the current loop.
   (unknown_address_altered is also nonzero in this case.)  */

static int loop_has_call;

/* Nonzero if there is a volatile memory reference in the current
   loop.  */

static int loop_has_volatile;

/* Added loop_continue which is the NOTE_INSN_LOOP_CONT of the
   current loop.  A continue statement will generate a branch to
   NEXT_INSN (loop_continue).  */

static rtx loop_continue;

/* Indexed by register number, contains the number of times the reg
   is set during the loop being scanned.
   During code motion, a negative value indicates a reg that has been
   made a candidate; in particular -2 means that it is an candidate that
   we know is equal to a constant and -1 means that it is an candidate
   not known equal to a constant.
   After code motion, regs moved have 0 (which is accurate now)
   while the failed candidates have the original number of times set.

   Therefore, at all times, == 0 indicates an invariant register;
   < 0 a conditionally invariant one.  */

static short *n_times_set;

/* Original value of n_times_set; same except that this value
   is not set negative for a reg whose sets have been made candidates
   and not set to 0 for a reg that is moved.  */

static short *n_times_used;

/* Index by register number, 1 indicates that the register
   cannot be moved or strength reduced.  */

static char *may_not_optimize;

/* Nonzero means reg N has already been moved out of one loop.
   This reduces the desire to move it out of another.  */

static char *moved_once;

/* Array of MEMs that are stored in this loop. If there are too many to fit
   here, we just turn on unknown_address_altered.  */

#define NUM_STORES 20
static rtx loop_store_mems[NUM_STORES];

/* Index of first available slot in above array.  */
static int loop_store_mems_idx;

/* Nonzero if we don't know what MEMs were changed in the current loop.
   This happens if the loop contains a call (in which case `loop_has_call'
   will also be set) or if we store into more than NUM_STORES MEMs.  */

static int unknown_address_altered;

/* Count of movable (i.e. invariant) instructions discovered in the loop.  */
static int num_movables;

/* Count of memory write instructions discovered in the loop.  */
static int num_mem_sets;

/* Number of loops contained within the current one, including itself.  */
static int loops_enclosed;

/* Bound on pseudo register number before loop optimization.
   A pseudo has valid regscan info if its number is < max_reg_before_loop.  */
int max_reg_before_loop;

/* This obstack is used in product_cheap_p to allocate its rtl.  It
   may call gen_reg_rtx which, in turn, may reallocate regno_reg_rtx.
   If we used the same obstack that it did, we would be deallocating
   that array.  */

static struct obstack temp_obstack;

/* This is where the pointer to the obstack being used for RTL is stored.  */

extern struct obstack *rtl_obstack;

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

extern char *oballoc ();

/* During the analysis of a loop, a chain of `struct movable's
   is made to record all the movable insns found.
   Then the entire chain can be scanned to decide which to move.  */

struct movable
{
  rtx insn;			/* A movable insn */
  rtx set_src;			/* The expression this reg is set from. */
  rtx set_dest;			/* The destination of this SET. */
  rtx dependencies;		/* When INSN is libcall, this is an EXPR_LIST
				   of any registers used within the LIBCALL. */
  int consec;			/* Number of consecutive following insns 
				   that must be moved with this one.  */
  int regno;			/* The register it sets */
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
  unsigned int is_equiv : 1;	/* 1 means a REG_EQUIV is present on INSN. */
  enum machine_mode savemode;   /* Nonzero means it is a mode for a low part
				   that we should avoid changing when clearing
				   the rest of the reg.  */
  struct movable *match;	/* First entry for same value */
  struct movable *forces;	/* An insn that must be moved if this is */
  struct movable *next;
};

FILE *loop_dump_stream;

/* Forward declarations.  */

static void find_and_verify_loops ();
static void mark_loop_jump ();
static void prescan_loop ();
static int reg_in_basic_block_p ();
static int consec_sets_invariant_p ();
static rtx libcall_other_reg ();
static int labels_in_range_p ();
static void count_loop_regs_set ();
static void note_addr_stored ();
static int loop_reg_used_before_p ();
static void scan_loop ();
static void replace_call_address ();
static rtx skip_consec_insns ();
static int libcall_benefit ();
static void ignore_some_movables ();
static void force_movables ();
static void combine_movables ();
static int rtx_equal_for_loop_p ();
static void move_movables ();
static void strength_reduce ();
static int valid_initial_value_p ();
static void find_mem_givs ();
static void record_biv ();
static void check_final_value ();
static void record_giv ();
static void update_giv_derive ();
static int basic_induction_var ();
static rtx simplify_giv_expr ();
static int general_induction_var ();
static int consec_sets_giv ();
static int check_dbra_loop ();
static rtx express_from ();
static int combine_givs_p ();
static void combine_givs ();
static int product_cheap_p ();
static int maybe_eliminate_biv ();
static int maybe_eliminate_biv_1 ();
static int last_use_this_basic_block ();
static void record_initial ();
static void update_reg_last_use ();

/* Relative gain of eliminating various kinds of operations.  */
int add_cost;
#if 0
int shift_cost;
int mult_cost;
#endif

/* Benefit penalty, if a giv is not replaceable, i.e. must emit an insn to
   copy the value of the strength reduced giv to its original register.  */
int copy_cost;

void
init_loop ()
{
  char *free_point = (char *) oballoc (1);
  rtx reg = gen_rtx (REG, word_mode, 0);

  add_cost = rtx_cost (gen_rtx (PLUS, word_mode, reg, reg), SET);

  /* We multiply by 2 to reconcile the difference in scale between
     these two ways of computing costs.  Otherwise the cost of a copy
     will be far less than the cost of an add.  */

  copy_cost = 2 * 2;

  /* Free the objects we just allocated.  */
  obfree (free_point);

  /* Initialize the obstack used for rtl in product_cheap_p.  */
  gcc_obstack_init (&temp_obstack);
}

/* Entry point of this file.  Perform loop optimization
   on the current function.  F is the first insn of the function
   and DUMPFILE is a stream for output of a trace of actions taken
   (or 0 if none should be output).  */

void
loop_optimize (f, dumpfile)
     /* f is the first instruction of a chain of insns for one function */
     rtx f;
     FILE *dumpfile;
{
  register rtx insn;
  register int i;
  rtx last_insn;

  loop_dump_stream = dumpfile;

  init_recog_no_volatile ();
  init_alias_analysis ();

  max_reg_before_loop = max_reg_num ();

  moved_once = (char *) alloca (max_reg_before_loop);
  bzero (moved_once, max_reg_before_loop);

  regs_may_share = 0;

  /* Count the number of loops. */

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

  /* Get size to use for tables indexed by uids.
     Leave some space for labels allocated by find_and_verify_loops.  */
  max_uid_for_loop = get_max_uid () + 1 + max_loop_num * 32;

  uid_luid = (int *) alloca (max_uid_for_loop * sizeof (int));
  uid_loop_num = (int *) alloca (max_uid_for_loop * sizeof (int));

  bzero ((char *) uid_luid, max_uid_for_loop * sizeof (int));
  bzero ((char *) uid_loop_num, max_uid_for_loop * sizeof (int));

  /* Allocate tables for recording each loop.  We set each entry, so they need
     not be zeroed.  */
  loop_number_loop_starts = (rtx *) alloca (max_loop_num * sizeof (rtx));
  loop_number_loop_ends = (rtx *) alloca (max_loop_num * sizeof (rtx));
  loop_outer_loop = (int *) alloca (max_loop_num * sizeof (int));
  loop_invalid = (char *) alloca (max_loop_num * sizeof (char));
  loop_number_exit_labels = (rtx *) alloca (max_loop_num * sizeof (rtx));
  loop_number_exit_count = (int *) alloca (max_loop_num * sizeof (int));

  /* Find and process each loop.
     First, find them, and record them in order of their beginnings.  */
  find_and_verify_loops (f);

  /* Now find all register lifetimes.  This must be done after
     find_and_verify_loops, because it might reorder the insns in the
     function.  */
  reg_scan (f, max_reg_num (), 1);

  /* See if we went too far.  */
  if (get_max_uid () > max_uid_for_loop)
    abort ();

  /* Compute the mapping from uids to luids.
     LUIDs are numbers assigned to insns, like uids,
     except that luids increase monotonically through the code.
     Don't assign luids to line-number NOTEs, so that the distance in luids
     between two insns is not affected by -g.  */

  for (insn = f, i = 0; insn; insn = NEXT_INSN (insn))
    {
      last_insn = insn;
      if (GET_CODE (insn) != NOTE
	  || NOTE_LINE_NUMBER (insn) <= 0)
	uid_luid[INSN_UID (insn)] = ++i;
      else
	/* Give a line number note the same luid as preceding insn.  */
	uid_luid[INSN_UID (insn)] = i;
    }

  max_luid = i + 1;

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

  /* Create a mapping from loops to BLOCK tree nodes.  */
  if (flag_unroll_loops && write_symbols != NO_DEBUG)
    find_loop_tree_blocks ();

  /* Now scan the loops, last ones first, since this means inner ones are done
     before outer ones.  */
  for (i = max_loop_num-1; i >= 0; i--)
    if (! loop_invalid[i] && loop_number_loop_ends[i])
      scan_loop (loop_number_loop_starts[i], loop_number_loop_ends[i],
		 max_reg_num ());

  /* If debugging and unrolling loops, we must replicate the tree nodes
     corresponding to the blocks inside the loop, so that the original one
     to one mapping will remain.  */
  if (flag_unroll_loops && write_symbols != NO_DEBUG)
    unroll_block_trees ();
}

/* Optimize one loop whose start is LOOP_START and end is END.
   LOOP_START is the NOTE_INSN_LOOP_BEG and END is the matching
   NOTE_INSN_LOOP_END.  */

/* ??? Could also move memory writes out of loops if the destination address
   is invariant, the source is invariant, the memory write is not volatile,
   and if we can prove that no read inside the loop can read this address
   before the write occurs.  If there is a read of this address after the
   write, then we can also mark the memory read as invariant.  */

static void
scan_loop (loop_start, end, nregs)
     rtx loop_start, end;
     int nregs;
{
  register int i;
  register rtx p;
  /* 1 if we are scanning insns that could be executed zero times.  */
  int maybe_never = 0;
  /* 1 if we are scanning insns that might never be executed
     due to a subroutine call which might exit before they are reached.  */
  int call_passed = 0;
  /* For a rotated loop that is entered near the bottom,
     this is the label at the top.  Otherwise it is zero.  */
  rtx loop_top = 0;
  /* Jump insn that enters the loop, or 0 if control drops in.  */
  rtx loop_entry_jump = 0;
  /* Place in the loop where control enters.  */
  rtx scan_start;
  /* Number of insns in the loop.  */
  int insn_count;
  int in_libcall = 0;
  int tem;
  rtx temp;
  /* The SET from an insn, if it is the only SET in the insn.  */
  rtx set, set1;
  /* Chain describing insns movable in current loop.  */
  struct movable *movables = 0;
  /* Last element in `movables' -- so we can add elements at the end.  */
  struct movable *last_movable = 0;
  /* Ratio of extra register life span we can justify
     for saving an instruction.  More if loop doesn't call subroutines
     since in that case saving an insn makes more difference
     and more registers are available.  */
  int threshold;
  /* If we have calls, contains the insn in which a register was used
     if it was used exactly once; contains const0_rtx if it was used more
     than once.  */
  rtx *reg_single_usage = 0;
  /* Nonzero if we are scanning instructions in a sub-loop.  */
  int loop_depth = 0;

  n_times_set = (short *) alloca (nregs * sizeof (short));
  n_times_used = (short *) alloca (nregs * sizeof (short));
  may_not_optimize = (char *) alloca (nregs);

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
       p != end
	 && GET_CODE (p) != CODE_LABEL && GET_RTX_CLASS (GET_CODE (p)) != 'i'
	 && (GET_CODE (p) != NOTE
	     || (NOTE_LINE_NUMBER (p) != NOTE_INSN_LOOP_BEG
		 && NOTE_LINE_NUMBER (p) != NOTE_INSN_LOOP_END));
       p = NEXT_INSN (p))
    ;

  scan_start = p;

  /* Set up variables describing this loop.  */
  prescan_loop (loop_start, end);
  threshold = (loop_has_call ? 1 : 2) * (1 + n_non_fixed_regs);

  /* If loop has a jump before the first label,
     the true entry is the target of that jump.
     Start scan from there.
     But record in LOOP_TOP the place where the end-test jumps
     back to so we can scan that after the end of the loop.  */
  if (GET_CODE (p) == JUMP_INSN)
    {
      loop_entry_jump = p;

      /* Loop entry must be unconditional jump (and not a RETURN)  */
      if (simplejump_p (p)
	  && JUMP_LABEL (p) != 0
	  /* Check to see whether the jump actually
	     jumps out of the loop (meaning it's no loop).
	     This case can happen for things like
	     do {..} while (0).  If this label was generated previously
	     by loop, we can't tell anything about it and have to reject
	     the loop.  */
	  && INSN_UID (JUMP_LABEL (p)) < max_uid_for_loop
	  && INSN_LUID (JUMP_LABEL (p)) >= INSN_LUID (loop_start)
	  && INSN_LUID (JUMP_LABEL (p)) < INSN_LUID (end))
	{
	  loop_top = next_label (scan_start);
	  scan_start = JUMP_LABEL (p);
	}
    }

  /* If SCAN_START was an insn created by loop, we don't know its luid
     as required by loop_reg_used_before_p.  So skip such loops.  (This
     test may never be true, but it's best to play it safe.) 

     Also, skip loops where we do not start scanning at a label.  This
     test also rejects loops starting with a JUMP_INSN that failed the
     test above.  */

  if (INSN_UID (scan_start) >= max_uid_for_loop
      || GET_CODE (scan_start) != CODE_LABEL)
    {
      if (loop_dump_stream)
	fprintf (loop_dump_stream, "\nLoop from %d to %d is phony.\n\n",
		 INSN_UID (loop_start), INSN_UID (end));
      return;
    }

  /* Count number of times each reg is set during this loop.
     Set may_not_optimize[I] if it is not safe to move out
     the setting of register I.  If this loop has calls, set
     reg_single_usage[I].  */

  bzero ((char *) n_times_set, nregs * sizeof (short));
  bzero (may_not_optimize, nregs);

  if (loop_has_call)
    {
      reg_single_usage = (rtx *) alloca (nregs * sizeof (rtx));
      bzero ((char *) reg_single_usage, nregs * sizeof (rtx));
    }

  count_loop_regs_set (loop_top ? loop_top : loop_start, end,
		       may_not_optimize, reg_single_usage, &insn_count, nregs);

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    may_not_optimize[i] = 1, n_times_set[i] = 1;
  bcopy ((char *) n_times_set, (char *) n_times_used, nregs * sizeof (short));

  if (loop_dump_stream)
    {
      fprintf (loop_dump_stream, "\nLoop from %d to %d: %d real insns.\n",
	       INSN_UID (loop_start), INSN_UID (end), insn_count);
      if (loop_continue)
	fprintf (loop_dump_stream, "Continue at insn %d.\n",
		 INSN_UID (loop_continue));
    }

  /* Scan through the loop finding insns that are safe to move.
     Set n_times_set negative for the reg being set, so that
     this reg will be considered invariant for subsequent insns.
     We consider whether subsequent insns use the reg
     in deciding whether it is worth actually moving.

     MAYBE_NEVER is nonzero if we have passed a conditional jump insn
     and therefore it is possible that the insns we are scanning
     would never be executed.  At such times, we must make sure
     that it is safe to execute the insn once instead of zero times.
     When MAYBE_NEVER is 0, all insns will be executed at least once
     so that is not a problem.  */

  p = scan_start;
  while (1)
    {
      p = NEXT_INSN (p);
      /* At end of a straight-in loop, we are done.
	 At end of a loop entered at the bottom, scan the top.  */
      if (p == scan_start)
	break;
      if (p == end)
	{
	  if (loop_top != 0)
	    p = loop_top;
	  else
	    break;
	  if (p == scan_start)
	    break;
	}

      if (GET_RTX_CLASS (GET_CODE (p)) == 'i'
	  && find_reg_note (p, REG_LIBCALL, NULL_RTX))
	in_libcall = 1;
      else if (GET_RTX_CLASS (GET_CODE (p)) == 'i'
	       && find_reg_note (p, REG_RETVAL, NULL_RTX))
	in_libcall = 0;

      if (GET_CODE (p) == INSN
	  && (set = single_set (p))
	  && GET_CODE (SET_DEST (set)) == REG
	  && ! may_not_optimize[REGNO (SET_DEST (set))])
	{
	  int tem1 = 0;
	  int tem2 = 0;
	  int move_insn = 0;
	  rtx src = SET_SRC (set);
	  rtx dependencies = 0;

	  /* Figure out what to use as a source of this insn.  If a REG_EQUIV
	     note is given or if a REG_EQUAL note with a constant operand is
	     specified, use it as the source and mark that we should move
	     this insn by calling emit_move_insn rather that duplicating the
	     insn.

	     Otherwise, only use the REG_EQUAL contents if a REG_RETVAL note
	     is present.  */
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

	  /* Don't try to optimize a register that was made
	     by loop-optimization for an inner loop.
	     We don't know its life-span, so we can't compute the benefit.  */
	  if (REGNO (SET_DEST (set)) >= max_reg_before_loop)
	    ;
	  /* In order to move a register, we need to have one of three cases:
	     (1) it is used only in the same basic block as the set
	     (2) it is not a user variable and it is not used in the
	         exit test (this can cause the variable to be used
		 before it is set just like a user-variable).
	     (3) the set is guaranteed to be executed once the loop starts,
	         and the reg is not used until after that.  */
	  else if (! ((! maybe_never
		       && ! loop_reg_used_before_p (set, p, loop_start,
						    scan_start, end))
		      || (! REG_USERVAR_P (SET_DEST (set))
			  && ! REG_LOOP_TEST_P (SET_DEST (set)))
		      || reg_in_basic_block_p (p, SET_DEST (set))))
	    ;
	  else if ((tem = invariant_p (src))
		   && (dependencies == 0
		       || (tem2 = invariant_p (dependencies)) != 0)
		   && (n_times_set[REGNO (SET_DEST (set))] == 1
		       || (tem1
			   = consec_sets_invariant_p (SET_DEST (set),
						      n_times_set[REGNO (SET_DEST (set))],
						      p)))
		   /* If the insn can cause a trap (such as divide by zero),
		      can't move it unless it's guaranteed to be executed
		      once loop is entered.  Even a function call might
		      prevent the trap insn from being reached
		      (since it might exit!)  */
		   && ! ((maybe_never || call_passed)
			 && may_trap_p (src)))
	    {
	      register struct movable *m;
	      register int regno = REGNO (SET_DEST (set));

	      /* A potential lossage is where we have a case where two insns
		 can be combined as long as they are both in the loop, but
		 we move one of them outside the loop.  For large loops,
		 this can lose.  The most common case of this is the address
		 of a function being called.  

		 Therefore, if this register is marked as being used exactly
		 once if we are in a loop with calls (a "large loop"), see if
		 we can replace the usage of this register with the source
		 of this SET.  If we can, delete this insn. 

		 Don't do this if P has a REG_RETVAL note or if we have
		 SMALL_REGISTER_CLASSES and SET_SRC is a hard register.  */

	      if (reg_single_usage && reg_single_usage[regno] != 0
		  && reg_single_usage[regno] != const0_rtx
		  && regno_first_uid[regno] == INSN_UID (p)
		  && (regno_last_uid[regno]
		      == INSN_UID (reg_single_usage[regno]))
		  && n_times_set[REGNO (SET_DEST (set))] == 1
		  && ! side_effects_p (SET_SRC (set))
		  && ! find_reg_note (p, REG_RETVAL, NULL_RTX)
#ifdef SMALL_REGISTER_CLASSES
		  && ! (GET_CODE (SET_SRC (set)) == REG
			&& REGNO (SET_SRC (set)) < FIRST_PSEUDO_REGISTER)
#endif
		  /* This test is not redundant; SET_SRC (set) might be
		     a call-clobbered register and the life of REGNO
		     might span a call.  */
		  && ! modified_between_p (SET_SRC (set), p,
					   reg_single_usage[regno])
		  && no_labels_between_p (p, reg_single_usage[regno])
		  && validate_replace_rtx (SET_DEST (set), SET_SRC (set),
					   reg_single_usage[regno]))
		{
		  /* Replace any usage in a REG_EQUAL note.  Must copy the
		     new source, so that we don't get rtx sharing between the
		     SET_SOURCE and REG_NOTES of insn p.  */
		  REG_NOTES (reg_single_usage[regno])
		    = replace_rtx (REG_NOTES (reg_single_usage[regno]),
				   SET_DEST (set), copy_rtx (SET_SRC (set)));
				   
		  PUT_CODE (p, NOTE);
		  NOTE_LINE_NUMBER (p) = NOTE_INSN_DELETED;
		  NOTE_SOURCE_FILE (p) = 0;
		  n_times_set[regno] = 0;
		  continue;
		}

	      m = (struct movable *) alloca (sizeof (struct movable));
	      m->next = 0;
	      m->insn = p;
	      m->set_src = src;
	      m->dependencies = dependencies;
	      m->set_dest = SET_DEST (set);
	      m->force = 0;
	      m->consec = n_times_set[REGNO (SET_DEST (set))] - 1;
	      m->done = 0;
	      m->forces = 0;
	      m->partial = 0;
	      m->move_insn = move_insn;
	      m->is_equiv = (find_reg_note (p, REG_EQUIV, NULL_RTX) != 0);
	      m->savemode = VOIDmode;
	      m->regno = regno;
	      /* Set M->cond if either invariant_p or consec_sets_invariant_p
		 returned 2 (only conditionally invariant).  */
	      m->cond = ((tem | tem1 | tem2) > 1);
	      m->global = (uid_luid[regno_last_uid[regno]] > INSN_LUID (end)
			   || uid_luid[regno_first_uid[regno]] < INSN_LUID (loop_start));
	      m->match = 0;
	      m->lifetime = (uid_luid[regno_last_uid[regno]]
			     - uid_luid[regno_first_uid[regno]]);
	      m->savings = n_times_used[regno];
	      if (find_reg_note (p, REG_RETVAL, NULL_RTX))
		m->savings += libcall_benefit (p);
	      n_times_set[regno] = move_insn ? -2 : -1;
	      /* Add M to the end of the chain MOVABLES.  */
	      if (movables == 0)
		movables = m;
	      else
		last_movable->next = m;
	      last_movable = m;

	      if (m->consec > 0)
		{
		  /* Skip this insn, not checking REG_LIBCALL notes.  */
		  p = next_nonnote_insn (p);
		  /* Skip the consecutive insns, if there are any.  */
		  p = skip_consec_insns (p, m->consec);
		  /* Back up to the last insn of the consecutive group.  */
		  p = prev_nonnote_insn (p);

		  /* We must now reset m->move_insn, m->is_equiv, and possibly
		     m->set_src to correspond to the effects of all the
		     insns.  */
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
		  m->is_equiv = (find_reg_note (p, REG_EQUIV, NULL_RTX) != 0);
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
	      register int regno = REGNO (SET_DEST (set));
	      if (n_times_set[regno] == 2)
		{
		  register struct movable *m;
		  m = (struct movable *) alloca (sizeof (struct movable));
		  m->next = 0;
		  m->insn = p;
		  m->set_dest = SET_DEST (set);
		  m->dependencies = 0;
		  m->force = 0;
		  m->consec = 0;
		  m->done = 0;
		  m->forces = 0;
		  m->move_insn = 0;
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
		     assumption. */
		  m->global = (INSN_UID (p) >= max_uid_for_loop
			       || (uid_luid[regno_last_uid[regno]]
				   > INSN_LUID (end))
			       || (uid_luid[regno_first_uid[regno]]
				   < INSN_LUID (p))
			       || (labels_in_range_p
				   (p, uid_luid[regno_first_uid[regno]])));
		  if (maybe_never && m->global)
		    m->savemode = GET_MODE (SET_SRC (set1));
		  else
		    m->savemode = VOIDmode;
		  m->regno = regno;
		  m->cond = 0;
		  m->match = 0;
		  m->lifetime = (uid_luid[regno_last_uid[regno]]
				 - uid_luid[regno_first_uid[regno]]);
		  m->savings = 1;
		  n_times_set[regno] = -1;
		  /* Add M to the end of the chain MOVABLES.  */
		  if (movables == 0)
		    movables = m;
		  else
		    last_movable->next = m;
		  last_movable = m;
		}
	    }
	}
      /* Past a call insn, we get to insns which might not be executed
	 because the call might exit.  This matters for insns that trap.
	 Call insns inside a REG_LIBCALL/REG_RETVAL block always return,
	 so they don't count.  */
      else if (GET_CODE (p) == CALL_INSN && ! in_libcall)
	call_passed = 1;
      /* Past a label or a jump, we get to insns for which we
	 can't count on whether or how many times they will be
	 executed during each iteration.  Therefore, we can
	 only move out sets of trivial variables
	 (those not used after the loop).  */
      /* This code appears in three places, once in scan_loop, and twice
	 in strength_reduce.  */
      else if ((GET_CODE (p) == CODE_LABEL || GET_CODE (p) == JUMP_INSN)
	       /* If we enter the loop in the middle, and scan around to the
		  beginning, don't set maybe_never for that.  This must be an
		  unconditional jump, otherwise the code at the top of the
		  loop might never be executed.  Unconditional jumps are
		  followed a by barrier then loop end.  */
               && ! (GET_CODE (p) == JUMP_INSN && JUMP_LABEL (p) == loop_top
		     && NEXT_INSN (NEXT_INSN (p)) == end
		     && simplejump_p (p)))
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

  combine_movables (movables, nregs);
	
  /* Now consider each movable insn to decide whether it is worth moving.
     Store 0 in n_times_set for each reg that is moved.  */

  move_movables (movables, threshold,
		 insn_count, loop_start, end, nregs);

  /* Now candidates that still are negative are those not moved.
     Change n_times_set to indicate that those are not actually invariant.  */
  for (i = 0; i < nregs; i++)
    if (n_times_set[i] < 0)
      n_times_set[i] = n_times_used[i];

  if (flag_strength_reduce)
    strength_reduce (scan_start, end, loop_top,
		     insn_count, loop_start, end);
}

/* Add elements to *OUTPUT to record all the pseudo-regs
   mentioned in IN_THIS but not mentioned in NOT_IN_THIS.  */

void
record_excess_regs (in_this, not_in_this, output)
     rtx in_this, not_in_this;
     rtx *output;
{
  enum rtx_code code;
  char *fmt;
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
	*output = gen_rtx (EXPR_LIST, VOIDmode, in_this, *output);
      return;
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

static rtx
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

  if (regno_first_uid[regno] != INSN_UID (insn))
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
	  if (regno_last_uid[regno] == INSN_UID (p))
	    return 1;
	  break;

	case JUMP_INSN:
	  /* Jump insn: if this is the last use, we win.  */
	  if (regno_last_uid[regno] == INSN_UID (p))
	    return 1;
	  /* Otherwise, it's the end of the basic block, so we lose.  */
	  return 0;

	case CODE_LABEL:
	case BARRIER:
	  /* It's the end of the basic block, so we lose.  */
	  return 0;
	}
    }

  /* The "last use" doesn't follow the "first use"??  */
  abort ();
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
				   routine. */
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

      do insn = NEXT_INSN (insn);
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
     struct movable *movables;
{
  register struct movable *m, *m1;

  for (m = movables; m; m = m->next)
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
	    for (m1 = movables; m1 != m; m1 = m1->next)
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
     struct movable *movables;
{
  register struct movable *m, *m1;
  for (m1 = movables; m1; m1 = m1->next)
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
	  if (INSN_UID (m->insn) == regno_last_uid[regno]
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
	    m1->savings += m1->savings;
	  }
      }
}

/* Find invariant expressions that are equal and can be combined into
   one register.  */

static void
combine_movables (movables, nregs)
     struct movable *movables;
     int nregs;
{
  register struct movable *m;
  char *matched_regs = (char *) alloca (nregs);
  enum machine_mode mode;

  /* Regs that are set more than once are not allowed to match
     or be matched.  I'm no longer sure why not.  */
  /* Perhaps testing m->consec_sets would be more appropriate here?  */

  for (m = movables; m; m = m->next)
    if (m->match == 0 && n_times_used[m->regno] == 1 && !m->partial)
      {
	register struct movable *m1;
	int regno = m->regno;

	bzero (matched_regs, nregs);
	matched_regs[regno] = 1;

	for (m1 = movables; m1; m1 = m1->next)
	  if (m != m1 && m1->match == 0 && n_times_used[m1->regno] == 1
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
						movables))))
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
      register struct movable *m0 = 0;

      /* Combine all the registers for extension from mode MODE.
	 Don't combine any that are used outside this loop.  */
      for (m = movables; m; m = m->next)
	if (m->partial && ! m->global
	    && mode == GET_MODE (SET_SRC (PATTERN (NEXT_INSN (m->insn)))))
	  {
	    register struct movable *m1;
	    int first = uid_luid[regno_first_uid[m->regno]];
	    int last = uid_luid[regno_last_uid[m->regno]];

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
	    for (m1 = movables; m1 != m; m1 = m1->next)
	      if (m1 == m0 || (m1->partial && m1->match == m0))
		if (! (uid_luid[regno_first_uid[m1->regno]] > last
		       || uid_luid[regno_last_uid[m1->regno]] < first))
		  goto overlap;

	    /* No overlap: we can combine this with the others.  */
	    m0->lifetime += m->lifetime;
	    m0->savings += m->savings;
	    m->done = 1;
	    m->match = m0;

	  overlap: ;
	  }
    }
}

/* Return 1 if regs X and Y will become the same if moved.  */

static int
regs_match_p (x, y, movables)
     rtx x, y;
     struct movable *movables;
{
  int xn = REGNO (x);
  int yn = REGNO (y);
  struct movable *mx, *my;

  for (mx = movables; mx; mx = mx->next)
    if (mx->regno == xn)
      break;

  for (my = movables; my; my = my->next)
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
rtx_equal_for_loop_p (x, y, movables)
     rtx x, y;
     struct movable *movables;
{
  register int i;
  register int j;
  register struct movable *m;
  register enum rtx_code code;
  register char *fmt;

  if (x == y)
    return 1;
  if (x == 0 || y == 0)
    return 0;

  code = GET_CODE (x);

  /* If we have a register and a constant, they may sometimes be
     equal.  */
  if (GET_CODE (x) == REG && n_times_set[REGNO (x)] == -2
      && CONSTANT_P (y))
    for (m = movables; m; m = m->next)
      if (m->move_insn && m->regno == REGNO (x)
	  && rtx_equal_p (m->set_src, y))
	return 1;

  else if (GET_CODE (y) == REG && n_times_set[REGNO (y)] == -2
	   && CONSTANT_P (x))
    for (m = movables; m; m = m->next)
      if (m->move_insn && m->regno == REGNO (y)
	  && rtx_equal_p (m->set_src, x))
	return 1;

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
	    if (rtx_equal_for_loop_p (XVECEXP (x, i, j), XVECEXP (y, i, j), movables) == 0)
	      return 0;
	  break;

	case 'e':
	  if (rtx_equal_for_loop_p (XEXP (x, i), XEXP (y, i), movables) == 0)
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
  insns in INSNS which use thet reference.  */

static void
add_label_notes (x, insns)
     rtx x;
     rtx insns;
{
  enum rtx_code code = GET_CODE (x);
  int i, j;
  char *fmt;
  rtx insn;

  if (code == LABEL_REF && !LABEL_REF_NONLOCAL_P (x))
    {
      rtx next = next_real_insn (XEXP (x, 0));

      /* Don't record labels that refer to dispatch tables.
	 This is not necessary, since the tablejump references the same label.
	 And if we did record them, flow.c would make worse code.  */
      if (next == 0
	  || ! (GET_CODE (next) == JUMP_INSN
		&& (GET_CODE (PATTERN (next)) == ADDR_VEC
		    || GET_CODE (PATTERN (next)) == ADDR_DIFF_VEC)))
	{
	  for (insn = insns; insn; insn = NEXT_INSN (insn))
	    if (reg_mentioned_p (XEXP (x, 0), insn))
	      REG_NOTES (insn) = gen_rtx (EXPR_LIST, REG_LABEL, XEXP (x, 0),
					  REG_NOTES (insn));
	}
      return;
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
move_movables (movables, threshold, insn_count, loop_start, end, nregs)
     struct movable *movables;
     int threshold;
     int insn_count;
     rtx loop_start;
     rtx end;
     int nregs;
{
  rtx new_start = 0;
  register struct movable *m;
  register rtx p;
  /* Map of pseudo-register replacements to handle combining
     when we move several insns that load the same value
     into different pseudo-registers.  */
  rtx *reg_map = (rtx *) alloca (nregs * sizeof (rtx));
  char *already_moved = (char *) alloca (nregs);

  bzero (already_moved, nregs);
  bzero ((char *) reg_map, nregs * sizeof (rtx));

  num_movables = 0;

  for (m = movables; m; m = m->next)
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

      /* Count movables.  Value used in heuristics in strength_reduce.  */
      num_movables++;

      /* Ignore the insn if it's already done (it matched something else).
	 Otherwise, see if it is now safe to move.  */

      if (!m->done
	  && (! m->cond
	      || (1 == invariant_p (m->set_src)
		  && (m->dependencies == 0
		      || 1 == invariant_p (m->dependencies))
		  && (m->consec == 0
		      || 1 == consec_sets_invariant_p (m->set_dest,
						       m->consec + 1,
						       m->insn))))
	  && (! m->forces || m->forces->done))
	{
	  register int regno;
	  register rtx p;
	  int savings = m->savings;

	  /* We have an insn that is safe to move.
	     Compute its desirability.  */

	  p = m->insn;
	  regno = m->regno;

	  if (loop_dump_stream)
	    fprintf (loop_dump_stream, "savings %d ", savings);

	  if (moved_once[regno])
	    {
	      insn_count *= 2;

	      if (loop_dump_stream)
		fprintf (loop_dump_stream, "halved since already moved ");
	    }

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
	      || (threshold * savings * m->lifetime) >= insn_count
	      || (m->forces && m->forces->done
		  && n_times_used[m->forces->regno] == 1))
	    {
	      int count;
	      register struct movable *m1;
	      rtx first;

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
		  i1 = emit_insn_before (newpat, loop_start);

		  /* Mark the moved, invariant reg as being allowed to
		     share a hard reg with the other matching invariant.  */
		  REG_NOTES (i1) = REG_NOTES (m->insn);
		  r1 = SET_DEST (PATTERN (m->insn));
		  r2 = SET_DEST (PATTERN (m1->insn));
		  regs_may_share = gen_rtx (EXPR_LIST, VOIDmode, r1,
					    gen_rtx (EXPR_LIST, VOIDmode, r2,
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
		  rtx i1, temp;

		  for (count = m->consec; count >= 0; count--)
		    {
		      /* If this is the first insn of a library call sequence,
			 skip to the end.  */
		      if (GET_CODE (p) != NOTE
			  && (temp = find_reg_note (p, REG_LIBCALL, NULL_RTX)))
			p = XEXP (temp, 0);

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

		      p = delete_insn (p);
		      while (p && GET_CODE (p) == NOTE)
			p = NEXT_INSN (p);
		    }

		  start_sequence ();
		  emit_move_insn (m->set_dest, m->set_src);
		  temp = get_insns ();
		  end_sequence ();

		  add_label_notes (m->set_src, temp);

		  i1 = emit_insns_before (temp, loop_start);
		  if (! find_reg_note (i1, REG_EQUAL, NULL_RTX))
		    REG_NOTES (i1)
		      = gen_rtx (EXPR_LIST,
				 m->is_equiv ? REG_EQUIV : REG_EQUAL,
				 m->set_src, REG_NOTES (i1));

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

		      /* If first insn of libcall sequence, skip to end. */
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
				emit_insn_after (gen_move_insn (fn_reg,
								fn_address),
						 fn_address_insn);

			      if (GET_CODE (temp) == CALL_INSN)
				{
				  i1 = emit_call_insn_before (body, loop_start);
				  /* Because the USAGE information potentially
				     contains objects other than hard registers
				     we need to copy it.  */
				  if (CALL_INSN_FUNCTION_USAGE (temp))
				    CALL_INSN_FUNCTION_USAGE (i1) =
				      copy_rtx (CALL_INSN_FUNCTION_USAGE (temp));
				}
			      else
				i1 = emit_insn_before (body, loop_start);
			      if (first == 0)
				first = i1;
			      if (temp == fn_address_insn)
				fn_address_insn = i1;
			      REG_NOTES (i1) = REG_NOTES (temp);
			      delete_insn (temp);
			    }
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
			  tem = expand_binop
			    (GET_MODE (reg), and_optab, reg,
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
			  i1 = emit_insn_before (sequence, loop_start);
			}
		      else if (GET_CODE (p) == CALL_INSN)
			{
			  i1 = emit_call_insn_before (PATTERN (p), loop_start);
			  /* Because the USAGE information potentially
			     contains objects other than hard registers
			     we need to copy it.  */
			  if (CALL_INSN_FUNCTION_USAGE (p))
			    CALL_INSN_FUNCTION_USAGE (i1) =
			      copy_rtx (CALL_INSN_FUNCTION_USAGE (p));
			}
		      else
			i1 = emit_insn_before (PATTERN (p), loop_start);

		      REG_NOTES (i1) = REG_NOTES (p);

		      /* If there is a REG_EQUAL note present whose value is
			 not loop invariant, then delete it, since it may
			 cause problems with later optimization passes.
			 It is possible for cse to create such notes
			 like this as a result of record_jump_cond.  */
		      
		      if ((temp = find_reg_note (i1, REG_EQUAL, NULL_RTX))
			  && ! invariant_p (XEXP (temp, 0)))
			remove_note (i1, temp);

		      if (new_start == 0)
			new_start = i1;

		      if (loop_dump_stream)
			fprintf (loop_dump_stream, " moved to %d",
				 INSN_UID (i1));

#if 0
		      /* This isn't needed because REG_NOTES is copied
			 below and is wrong since P might be a PARALLEL.  */
		      if (REG_NOTES (i1) == 0
			  && ! m->partial /* But not if it's a zero-extend clr. */
			  && ! m->global /* and not if used outside the loop
					    (since it might get set outside).  */
			  && CONSTANT_P (SET_SRC (PATTERN (p))))
			REG_NOTES (i1)
			  = gen_rtx (EXPR_LIST, REG_EQUAL,
				     SET_SRC (PATTERN (p)), REG_NOTES (i1));
#endif

		      /* If library call, now fix the REG_NOTES that contain
			 insn pointers, namely REG_LIBCALL on FIRST
			 and REG_RETVAL on I1.  */
		      if (temp = find_reg_note (i1, REG_RETVAL, NULL_RTX))
			{
			  XEXP (temp, 0) = first;
			  temp = find_reg_note (first, REG_LIBCALL, NULL_RTX);
			  XEXP (temp, 0) = i1;
			}

		      delete_insn (p);
		      do p = NEXT_INSN (p);
		      while (p && GET_CODE (p) == NOTE);
		    }

		  /* The more regs we move, the less we like moving them.  */
		  threshold -= 3;
		}

	      /* Any other movable that loads the same register
		 MUST be moved.  */
	      already_moved[regno] = 1;

	      /* This reg has been moved out of one loop.  */
	      moved_once[regno] = 1;

	      /* The reg set here is now invariant.  */
	      if (! m->partial)
		n_times_set[regno] = 0;

	      m->done = 1;

	      /* Change the length-of-life info for the register
		 to say it lives at least the full length of this loop.
		 This will help guide optimizations in outer loops.  */

	      if (uid_luid[regno_first_uid[regno]] > INSN_LUID (loop_start))
		/* This is the old insn before all the moved insns.
		   We can't use the moved insn because it is out of range
		   in uid_luid.  Only the old insns have luids.  */
		regno_first_uid[regno] = INSN_UID (loop_start);
	      if (uid_luid[regno_last_uid[regno]] < INSN_LUID (end))
		regno_last_uid[regno] = INSN_UID (end);

	      /* Combine with this moved insn any other matching movables.  */

	      if (! m->partial)
		for (m1 = movables; m1; m1 = m1->next)
		  if (m1->match == m)
		    {
		      rtx temp;

		      /* Schedule the reg loaded by M1
			 for replacement so that shares the reg of M.
			 If the modes differ (only possible in restricted
			 circumstances, make a SUBREG.  */
		      if (GET_MODE (m->set_dest) == GET_MODE (m1->set_dest))
			reg_map[m1->regno] = m->set_dest;
		      else
			reg_map[m1->regno]
			  = gen_lowpart_common (GET_MODE (m1->set_dest),
						m->set_dest);
		    
		      /* Get rid of the matching insn
			 and prevent further processing of it.  */
		      m1->done = 1;

		      /* if library call, delete all insn except last, which
			 is deleted below */
		      if (temp = find_reg_note (m1->insn, REG_RETVAL,
						NULL_RTX))
			{
			  for (temp = XEXP (temp, 0); temp != m1->insn;
			       temp = NEXT_INSN (temp))
			    delete_insn (temp);
			}
		      delete_insn (m1->insn);

		      /* Any other movable that loads the same register
			 MUST be moved.  */
		      already_moved[m1->regno] = 1;

		      /* The reg merged here is now invariant,
			 if the reg it matches is invariant.  */
		      if (! m->partial)
			n_times_set[m1->regno] = 0;
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
  for (p = new_start; p != end; p = NEXT_INSN (p))
    if (GET_CODE (p) == INSN || GET_CODE (p) == JUMP_INSN
	|| GET_CODE (p) == CALL_INSN)
      {
	replace_regs (PATTERN (p), reg_map, nregs, 0);
	replace_regs (REG_NOTES (p), reg_map, nregs, 0);
	INSN_CODE (p) = -1;
      }
}

#if 0
/* Scan X and replace the address of any MEM in it with ADDR.
   REG is the address that MEM should have before the replacement.  */

static void
replace_call_address (x, reg, addr)
     rtx x, reg, addr;
{
  register enum rtx_code code;
  register int i;
  register char *fmt;

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
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	replace_call_address (XEXP (x, i), reg, addr);
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    replace_call_address (XVECEXP (x, i, j), reg, addr);
	}
    }
}
#endif

/* Return the number of memory refs to addresses that vary
   in the rtx X.  */

static int
count_nonfixed_reads (x)
     rtx x;
{
  register enum rtx_code code;
  register int i;
  register char *fmt;
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
      return ((invariant_p (XEXP (x, 0)) != 1)
	      + count_nonfixed_reads (XEXP (x, 0)));
    }

  value = 0;
  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	value += count_nonfixed_reads (XEXP (x, i));
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    value += count_nonfixed_reads (XVECEXP (x, i, j));
	}
    }
  return value;
}


#if 0
/* P is an instruction that sets a register to the result of a ZERO_EXTEND.
   Replace it with an instruction to load just the low bytes
   if the machine supports such an instruction,
   and insert above LOOP_START an instruction to clear the register.  */

static void
constant_high_bytes (p, loop_start)
     rtx p, loop_start;
{
  register rtx new;
  register int insn_code_number;

  /* Try to change (SET (REG ...) (ZERO_EXTEND (..:B ...)))
     to (SET (STRICT_LOW_PART (SUBREG:B (REG...))) ...).  */

  new = gen_rtx (SET, VOIDmode,
		 gen_rtx (STRICT_LOW_PART, VOIDmode,
			  gen_rtx (SUBREG, GET_MODE (XEXP (SET_SRC (PATTERN (p)), 0)),
				   SET_DEST (PATTERN (p)),
				   0)),
		 XEXP (SET_SRC (PATTERN (p)), 0));
  insn_code_number = recog (new, p);

  if (insn_code_number)
    {
      register int i;

      /* Clear destination register before the loop.  */
      emit_insn_before (gen_rtx (SET, VOIDmode,
				 SET_DEST (PATTERN (p)),
				 const0_rtx),
			loop_start);

      /* Inside the loop, just load the low part.  */
      PATTERN (p) = new;
    }
}
#endif

/* Scan a loop setting the variables `unknown_address_altered',
   `num_mem_sets', `loop_continue', loops_enclosed', `loop_has_call',
   and `loop_has_volatile'.
   Also, fill in the array `loop_store_mems'.  */

static void
prescan_loop (start, end)
     rtx start, end;
{
  register int level = 1;
  register rtx insn;

  unknown_address_altered = 0;
  loop_has_call = 0;
  loop_has_volatile = 0;
  loop_store_mems_idx = 0;

  num_mem_sets = 0;
  loops_enclosed = 1;
  loop_continue = 0;

  for (insn = NEXT_INSN (start); insn != NEXT_INSN (end);
       insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == NOTE)
	{
	  if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_BEG)
	    {
	      ++level;
	      /* Count number of loops contained in this one.  */
	      loops_enclosed++;
	    }
	  else if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_END)
	    {
	      --level;
	      if (level == 0)
		{
		  end = insn;
		  break;
		}
	    }
	  else if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_CONT)
	    {
	      if (level == 1)
		loop_continue = insn;
	    }
	}
      else if (GET_CODE (insn) == CALL_INSN)
	{
	  unknown_address_altered = 1;
	  loop_has_call = 1;
	}
      else
	{
	  if (GET_CODE (insn) == INSN || GET_CODE (insn) == JUMP_INSN)
	    {
	      if (volatile_refs_p (PATTERN (insn)))
		loop_has_volatile = 1;

	      note_stores (PATTERN (insn), note_addr_stored);
	    }
	}
    }
}

/* Scan the function looking for loops.  Record the start and end of each loop.
   Also mark as invalid loops any loops that contain a setjmp or are branched
   to from outside the loop.  */

static void
find_and_verify_loops (f)
     rtx f;
{
  rtx insn, label;
  int current_loop = -1;
  int next_loop = -1;
  int loop;

  /* If there are jumps to undefined labels,
     treat them as jumps out of any/all loops.
     This also avoids writing past end of tables when there are no loops.  */
  uid_loop_num[0] = -1;

  /* Find boundaries of loops, mark which loops are contained within
     loops, and invalidate loops that have setjmp.  */

  for (insn = f; insn; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == NOTE)
	switch (NOTE_LINE_NUMBER (insn))
	  {
	  case NOTE_INSN_LOOP_BEG:
	    loop_number_loop_starts[++next_loop] =  insn;
	    loop_number_loop_ends[next_loop] = 0;
	    loop_outer_loop[next_loop] = current_loop;
	    loop_invalid[next_loop] = 0;
	    loop_number_exit_labels[next_loop] = 0;
	    loop_number_exit_count[next_loop] = 0;
	    current_loop = next_loop;
	    break;

	  case NOTE_INSN_SETJMP:
	    /* In this case, we must invalidate our current loop and any
	       enclosing loop.  */
	    for (loop = current_loop; loop != -1; loop = loop_outer_loop[loop])
	      {
		loop_invalid[loop] = 1;
		if (loop_dump_stream)
		  fprintf (loop_dump_stream,
			   "\nLoop at %d ignored due to setjmp.\n",
			   INSN_UID (loop_number_loop_starts[loop]));
	      }
	    break;

	  case NOTE_INSN_LOOP_END:
	    if (current_loop == -1)
	      abort ();

	    loop_number_loop_ends[current_loop] = insn;
	    current_loop = loop_outer_loop[current_loop];
	    break;

	  }

      /* Note that this will mark the NOTE_INSN_LOOP_END note as being in the
	 enclosing loop, but this doesn't matter.  */
      uid_loop_num[INSN_UID (insn)] = current_loop;
    }

  /* Any loop containing a label used in an initializer must be invalidated,
     because it can be jumped into from anywhere.  */

  for (label = forced_labels; label; label = XEXP (label, 1))
    {
      int loop_num;

      for (loop_num = uid_loop_num[INSN_UID (XEXP (label, 0))];
	   loop_num != -1;
	   loop_num = loop_outer_loop[loop_num])
	loop_invalid[loop_num] = 1;
    }

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
    if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
      {
	int this_loop_num = uid_loop_num[INSN_UID (insn)];

	if (GET_CODE (insn) == INSN || GET_CODE (insn) == CALL_INSN)
	  {
	    rtx note = find_reg_note (insn, REG_LABEL, NULL_RTX);
	    if (note)
	      {
		int loop_num;

		for (loop_num = uid_loop_num[INSN_UID (XEXP (note, 0))];
		     loop_num != -1;
		     loop_num = loop_outer_loop[loop_num])
		  loop_invalid[loop_num] = 1;
	      }
	  }

	if (GET_CODE (insn) != JUMP_INSN)
	  continue;

	mark_loop_jump (PATTERN (insn), this_loop_num);

	/* See if this is an unconditional branch outside the loop.  */
	if (this_loop_num != -1
	    && (GET_CODE (PATTERN (insn)) == RETURN
		|| (simplejump_p (insn)
		    && (uid_loop_num[INSN_UID (JUMP_LABEL (insn))]
			!= this_loop_num)))
	    && get_max_uid () < max_uid_for_loop)
	  {
	    rtx p;
	    rtx our_next = next_real_insn (insn);
	    int dest_loop;
	    int outer_loop = -1;

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
		dest_loop = uid_loop_num[INSN_UID (JUMP_LABEL (insn))];
		if (dest_loop != -1)
		  {
		    for (outer_loop = dest_loop; outer_loop != -1;
			 outer_loop = loop_outer_loop[outer_loop])
		      if (outer_loop == this_loop_num)
			break;
		  }
	      }

	    /* Make sure that the target of P is within the current loop.  */

	    if (GET_CODE (p) == JUMP_INSN && JUMP_LABEL (p)
		&& uid_loop_num[INSN_UID (JUMP_LABEL (p))] != this_loop_num)
	      outer_loop = this_loop_num;

	    /* If we stopped on a JUMP_INSN to the next insn after INSN,
	       we have a block of code to try to move.

	       We look backward and then forward from the target of INSN
	       to find a BARRIER at the same loop depth as the target.
	       If we find such a BARRIER, we make a new label for the start
	       of the block, invert the jump in P and point it to that label,
	       and move the block of code to the spot we found.  */

	    if (outer_loop == -1
		&& GET_CODE (p) == JUMP_INSN
		&& JUMP_LABEL (p) != 0
		/* Just ignore jumps to labels that were never emitted.
		   These always indicate compilation errors.  */
		&& INSN_UID (JUMP_LABEL (p)) != 0
		&& condjump_p (p)
		&& ! simplejump_p (p)
		&& next_real_insn (JUMP_LABEL (p)) == our_next)
	      {
		rtx target
		  = JUMP_LABEL (insn) ? JUMP_LABEL (insn) : get_last_insn ();
		int target_loop_num = uid_loop_num[INSN_UID (target)];
		rtx loc;

		for (loc = target; loc; loc = PREV_INSN (loc))
		  if (GET_CODE (loc) == BARRIER
		      && uid_loop_num[INSN_UID (loc)] == target_loop_num)
		    break;

		if (loc == 0)
		  for (loc = target; loc; loc = NEXT_INSN (loc))
		    if (GET_CODE (loc) == BARRIER
			&& uid_loop_num[INSN_UID (loc)] == target_loop_num)
		      break;

		if (loc)
		  {
		    rtx cond_label = JUMP_LABEL (p);
		    rtx new_label = get_label_after (p);

		    /* Ensure our label doesn't go away.  */
		    LABEL_NUSES (cond_label)++;

		    /* Verify that uid_loop_num is large enough and that
		       we can invert P. */
		   if (invert_jump (p, new_label))
		     {
		       rtx q, r;

		       /* Include the BARRIER after INSN and copy the
			  block after LOC.  */
		       new_label = squeeze_notes (new_label, NEXT_INSN (insn));
		       reorder_insns (new_label, NEXT_INSN (insn), loc);

		       /* All those insns are now in TARGET_LOOP_NUM.  */
		       for (q = new_label; q != NEXT_INSN (NEXT_INSN (insn));
			    q = NEXT_INSN (q))
			 uid_loop_num[INSN_UID (q)] = target_loop_num;

		       /* The label jumped to by INSN is no longer a loop exit.
			  Unless INSN does not have a label (e.g., it is a
			  RETURN insn), search loop_number_exit_labels to find
			  its label_ref, and remove it.  Also turn off
			  LABEL_OUTSIDE_LOOP_P bit.  */
		       if (JUMP_LABEL (insn))
			 {
			   int loop_num;

			   for (q = 0,
				r = loop_number_exit_labels[this_loop_num];
				r; q = r, r = LABEL_NEXTREF (r))
			     if (XEXP (r, 0) == JUMP_LABEL (insn))
			       {
				 LABEL_OUTSIDE_LOOP_P (r) = 0;
				 if (q)
				   LABEL_NEXTREF (q) = LABEL_NEXTREF (r);
				 else
				   loop_number_exit_labels[this_loop_num]
				     = LABEL_NEXTREF (r);
				 break;
			       }

			   for (loop_num = this_loop_num;
				loop_num != -1 && loop_num != target_loop_num;
				loop_num = loop_outer_loop[loop_num])
			     loop_number_exit_count[loop_num]--;

			   /* If we didn't find it, then something is wrong. */
			   if (! r)
			     abort ();
			 }

		       /* P is now a jump outside the loop, so it must be put
			  in loop_number_exit_labels, and marked as such.
			  The easiest way to do this is to just call
			  mark_loop_jump again for P.  */
		       mark_loop_jump (PATTERN (p), this_loop_num);

		       /* If INSN now jumps to the insn after it,
			  delete INSN.  */
		       if (JUMP_LABEL (insn) != 0
			   && (next_real_insn (JUMP_LABEL (insn))
			       == next_real_insn (insn)))
			 delete_insn (insn);
		     }

		    /* Continue the loop after where the conditional
		       branch used to jump, since the only branch insn
		       in the block (if it still remains) is an inter-loop
		       branch and hence needs no processing.  */
		    insn = NEXT_INSN (cond_label);

		    if (--LABEL_NUSES (cond_label) == 0)
		      delete_insn (cond_label);

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
mark_loop_jump (x, loop_num)
     rtx x;
     int loop_num;
{
  int dest_loop;
  int outer_loop;
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
      mark_loop_jump (XEXP (x, 0), loop_num);
      return;

    case PLUS:
    case MINUS:
    case MULT:
      mark_loop_jump (XEXP (x, 0), loop_num);
      mark_loop_jump (XEXP (x, 1), loop_num);
      return;

    case SIGN_EXTEND:
    case ZERO_EXTEND:
      mark_loop_jump (XEXP (x, 0), loop_num);
      return;

    case LABEL_REF:
      dest_loop = uid_loop_num[INSN_UID (XEXP (x, 0))];

      /* Link together all labels that branch outside the loop.  This
	 is used by final_[bg]iv_value and the loop unrolling code.  Also
	 mark this LABEL_REF so we know that this branch should predict
	 false.  */

      /* A check to make sure the label is not in an inner nested loop,
	 since this does not count as a loop exit.  */
      if (dest_loop != -1)
	{
	  for (outer_loop = dest_loop; outer_loop != -1;
	       outer_loop = loop_outer_loop[outer_loop])
	    if (outer_loop == loop_num)
	      break;
	}
      else
	outer_loop = -1;

      if (loop_num != -1 && outer_loop == -1)
	{
	  LABEL_OUTSIDE_LOOP_P (x) = 1;
	  LABEL_NEXTREF (x) = loop_number_exit_labels[loop_num];
	  loop_number_exit_labels[loop_num] = x;

	  for (outer_loop = loop_num;
	       outer_loop != -1 && outer_loop != dest_loop;
	       outer_loop = loop_outer_loop[outer_loop])
	    loop_number_exit_count[outer_loop]++;
	}

      /* If this is inside a loop, but not in the current loop or one enclosed
	 by it, it invalidates at least one loop.  */

      if (dest_loop == -1)
	return;

      /* We must invalidate every nested loop containing the target of this
	 label, except those that also contain the jump insn.  */

      for (; dest_loop != -1; dest_loop = loop_outer_loop[dest_loop])
	{
	  /* Stop when we reach a loop that also contains the jump insn.  */
	  for (outer_loop = loop_num; outer_loop != -1;
	       outer_loop = loop_outer_loop[outer_loop])
	    if (dest_loop == outer_loop)
	      return;

	  /* If we get here, we know we need to invalidate a loop.  */
	  if (loop_dump_stream && ! loop_invalid[dest_loop])
	    fprintf (loop_dump_stream,
		     "\nLoop at %d ignored due to multiple entry points.\n",
		     INSN_UID (loop_number_loop_starts[dest_loop]));
	  
	  loop_invalid[dest_loop] = 1;
	}
      return;

    case SET:
      /* If this is not setting pc, ignore.  */
      if (SET_DEST (x) == pc_rtx)
	mark_loop_jump (SET_SRC (x), loop_num);
      return;

    case IF_THEN_ELSE:
      mark_loop_jump (XEXP (x, 1), loop_num);
      mark_loop_jump (XEXP (x, 2), loop_num);
      return;

    case PARALLEL:
    case ADDR_VEC:
      for (i = 0; i < XVECLEN (x, 0); i++)
	mark_loop_jump (XVECEXP (x, 0, i), loop_num);
      return;

    case ADDR_DIFF_VEC:
      for (i = 0; i < XVECLEN (x, 1); i++)
	mark_loop_jump (XVECEXP (x, 1, i), loop_num);
      return;

    default:
      /* Treat anything else (such as a symbol_ref)
	 as a branch out of this loop, but not into any loop.  */

      if (loop_num != -1)
	{
	  loop_number_exit_labels[loop_num] = x;

	  for (outer_loop = loop_num; outer_loop != -1;
	       outer_loop = loop_outer_loop[outer_loop])
	    loop_number_exit_count[outer_loop]++;
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
note_addr_stored (x)
     rtx x;
{
  register int i;

  if (x == 0 || GET_CODE (x) != MEM)
    return;

  /* Count number of memory writes.
     This affects heuristics in strength_reduce.  */
  num_mem_sets++;

  /* BLKmode MEM means all memory is clobbered.  */
  if (GET_MODE (x) == BLKmode)
    unknown_address_altered = 1;

  if (unknown_address_altered)
    return;

  for (i = 0; i < loop_store_mems_idx; i++)
    if (rtx_equal_p (XEXP (loop_store_mems[i], 0), XEXP (x, 0))
	&& MEM_IN_STRUCT_P (x) == MEM_IN_STRUCT_P (loop_store_mems[i]))
      {
	/* We are storing at the same address as previously noted.  Save the
	   wider reference.  */
	if (GET_MODE_SIZE (GET_MODE (x))
	    > GET_MODE_SIZE (GET_MODE (loop_store_mems[i])))
	  loop_store_mems[i] = x;
	break;
      }

  if (i == NUM_STORES)
    unknown_address_altered = 1;

  else if (i == loop_store_mems_idx)
    loop_store_mems[loop_store_mems_idx++] = x;
}

/* Return nonzero if the rtx X is invariant over the current loop.

   The value is 2 if we refer to something only conditionally invariant.

   If `unknown_address_altered' is nonzero, no memory ref is invariant.
   Otherwise, a memory ref is invariant if it does not conflict with
   anything stored in `loop_store_mems'.  */

int
invariant_p (x)
     register rtx x;
{
  register int i;
  register enum rtx_code code;
  register char *fmt;
  int conditional = 0;

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
      if (x == frame_pointer_rtx || x == hard_frame_pointer_rtx
	  || x == arg_pointer_rtx)
	return 1;
      if (loop_has_call
	  && REGNO (x) < FIRST_PSEUDO_REGISTER && call_used_regs[REGNO (x)])
	return 0;
      if (n_times_set[REGNO (x)] < 0)
	return 2;
      return n_times_set[REGNO (x)] == 0;

    case MEM:
      /* Volatile memory references must be rejected.  Do this before
	 checking for read-only items, so that volatile read-only items
	 will be rejected also.  */
      if (MEM_VOLATILE_P (x))
	return 0;

      /* Read-only items (such as constants in a constant pool) are
	 invariant if their address is.  */
      if (RTX_UNCHANGING_P (x))
	break;

      /* If we filled the table (or had a subroutine call), any location
	 in memory could have been clobbered.  */
      if (unknown_address_altered)
	return 0;

      /* See if there is any dependence between a store and this load.  */
      for (i = loop_store_mems_idx - 1; i >= 0; i--)
	if (true_dependence (loop_store_mems[i], x))
	  return 0;

      /* It's not invalidated by a store in memory
	 but we must still verify the address is invariant.  */
      break;

    case ASM_OPERANDS:
      /* Don't mess with insns declared volatile.  */
      if (MEM_VOLATILE_P (x))
	return 0;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  int tem = invariant_p (XEXP (x, i));
	  if (tem == 0)
	    return 0;
	  if (tem == 2)
	    conditional = 1;
	}
      else if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    {
	      int tem = invariant_p (XVECEXP (x, i, j));
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
consec_sets_invariant_p (reg, n_sets, insn)
     int n_sets;
     rtx reg, insn;
{
  register rtx p = insn;
  register int regno = REGNO (reg);
  rtx temp;
  /* Number of sets we have to insist on finding after INSN.  */
  int count = n_sets - 1;
  int old = n_times_set[regno];
  int value = 0;
  int this;

  /* If N_SETS hit the limit, we can't rely on its value.  */
  if (n_sets == 127)
    return 0;

  n_times_set[regno] = 0;

  while (count > 0)
    {
      register enum rtx_code code;
      rtx set;

      p = NEXT_INSN (p);
      code = GET_CODE (p);

      /* If library call, skip to end of of it.  */
      if (code == INSN && (temp = find_reg_note (p, REG_LIBCALL, NULL_RTX)))
	p = XEXP (temp, 0);

      this = 0;
      if (code == INSN
	  && (set = single_set (p))
	  && GET_CODE (SET_DEST (set)) == REG
	  && REGNO (SET_DEST (set)) == regno)
	{
	  this = invariant_p (SET_SRC (set));
	  if (this != 0)
	    value |= this;
	  else if (temp = find_reg_note (p, REG_EQUAL, NULL_RTX))
	    {
	      /* If this is a libcall, then any invariant REG_EQUAL note is OK.
		 If this is an ordinary insn, then only CONSTANT_P REG_EQUAL
		 notes are OK.  */
	      this = (CONSTANT_P (XEXP (temp, 0))
		      || (find_reg_note (p, REG_RETVAL, NULL_RTX)
			  && invariant_p (XEXP (temp, 0))));
	      if (this != 0)
		value |= this;
	    }
	}
      if (this != 0)
	count--;
      else if (code != NOTE)
	{
	  n_times_set[regno] = old;
	  return 0;
	}
    }

  n_times_set[regno] = old;
  /* If invariant_p ever returned 2, we return 2.  */
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
  register rtx p = insn;
  register int regno = REGNO (reg);

  while (1)
    {
      register enum rtx_code code;
      p = NEXT_INSN (p);
      code = GET_CODE (p);
      if (code == CODE_LABEL || code == JUMP_INSN)
	return 1;
      if (code == INSN && GET_CODE (PATTERN (p)) == SET
	  && GET_CODE (SET_DEST (PATTERN (p))) == REG
	  && REGNO (SET_DEST (PATTERN (p))) == regno)
	{
	  if (!invariant_p (SET_SRC (PATTERN (p)), table))
	    return 0;
	}
    }
}
#endif /* 0 */

/* Look at all uses (not sets) of registers in X.  For each, if it is
   the single use, set USAGE[REGNO] to INSN; if there was a previous use in
   a different insn, set USAGE[REGNO] to const0_rtx.  */

static void
find_single_use_in_loop (insn, x, usage)
     rtx insn;
     rtx x;
     rtx *usage;
{
  enum rtx_code code = GET_CODE (x);
  char *fmt = GET_RTX_FORMAT (code);
  int i, j;

  if (code == REG)
    usage[REGNO (x)]
      = (usage[REGNO (x)] != 0 && usage[REGNO (x)] != insn)
	? const0_rtx : insn;

  else if (code == SET)
    {
      /* Don't count SET_DEST if it is a REG; otherwise count things
	 in SET_DEST because if a register is partially modified, it won't
	 show up as a potential movable so we don't care how USAGE is set 
	 for it.  */
      if (GET_CODE (SET_DEST (x)) != REG)
	find_single_use_in_loop (insn, SET_DEST (x), usage);
      find_single_use_in_loop (insn, SET_SRC (x), usage);
    }
  else
    for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
      {
	if (fmt[i] == 'e' && XEXP (x, i) != 0)
	  find_single_use_in_loop (insn, XEXP (x, i), usage);
	else if (fmt[i] == 'E')
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    find_single_use_in_loop (insn, XVECEXP (x, i, j), usage);
      }
}

/* Increment N_TIMES_SET at the index of each register
   that is modified by an insn between FROM and TO.
   If the value of an element of N_TIMES_SET becomes 127 or more,
   stop incrementing it, to avoid overflow.

   Store in SINGLE_USAGE[I] the single insn in which register I is
   used, if it is only used once.  Otherwise, it is set to 0 (for no
   uses) or const0_rtx for more than one use.  This parameter may be zero,
   in which case this processing is not done.

   Store in *COUNT_PTR the number of actual instruction
   in the loop.  We use this to decide what is worth moving out.  */

/* last_set[n] is nonzero iff reg n has been set in the current basic block.
   In that case, it is the insn that last set reg n.  */

static void
count_loop_regs_set (from, to, may_not_move, single_usage, count_ptr, nregs)
     register rtx from, to;
     char *may_not_move;
     rtx *single_usage;
     int *count_ptr;
     int nregs;
{
  register rtx *last_set = (rtx *) alloca (nregs * sizeof (rtx));
  register rtx insn;
  register int count = 0;
  register rtx dest;

  bzero ((char *) last_set, nregs * sizeof (rtx));
  for (insn = from; insn != to; insn = NEXT_INSN (insn))
    {
      if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	{
	  ++count;

	  /* If requested, record registers that have exactly one use.  */
	  if (single_usage)
	    {
	      find_single_use_in_loop (insn, PATTERN (insn), single_usage);

	      /* Include uses in REG_EQUAL notes.  */
	      if (REG_NOTES (insn))
		find_single_use_in_loop (insn, REG_NOTES (insn), single_usage);
	    }

	  if (GET_CODE (PATTERN (insn)) == CLOBBER
	      && GET_CODE (XEXP (PATTERN (insn), 0)) == REG)
	    /* Don't move a reg that has an explicit clobber.
	       We might do so sometimes, but it's not worth the pain.  */
	    may_not_move[REGNO (XEXP (PATTERN (insn), 0))] = 1;

	  if (GET_CODE (PATTERN (insn)) == SET
	      || GET_CODE (PATTERN (insn)) == CLOBBER)
	    {
	      dest = SET_DEST (PATTERN (insn));
	      while (GET_CODE (dest) == SUBREG
		     || GET_CODE (dest) == ZERO_EXTRACT
		     || GET_CODE (dest) == SIGN_EXTRACT
		     || GET_CODE (dest) == STRICT_LOW_PART)
		dest = XEXP (dest, 0);
	      if (GET_CODE (dest) == REG)
		{
		  register int regno = REGNO (dest);
		  /* If this is the first setting of this reg
		     in current basic block, and it was set before,
		     it must be set in two basic blocks, so it cannot
		     be moved out of the loop.  */
		  if (n_times_set[regno] > 0 && last_set[regno] == 0)
		    may_not_move[regno] = 1;
		  /* If this is not first setting in current basic block,
		     see if reg was used in between previous one and this.
		     If so, neither one can be moved.  */
		  if (last_set[regno] != 0
		      && reg_used_between_p (dest, last_set[regno], insn))
		    may_not_move[regno] = 1;
		  if (n_times_set[regno] < 127)
		    ++n_times_set[regno];
		  last_set[regno] = insn;
		}
	    }
	  else if (GET_CODE (PATTERN (insn)) == PARALLEL)
	    {
	      register int i;
	      for (i = XVECLEN (PATTERN (insn), 0) - 1; i >= 0; i--)
		{
		  register rtx x = XVECEXP (PATTERN (insn), 0, i);
		  if (GET_CODE (x) == CLOBBER && GET_CODE (XEXP (x, 0)) == REG)
		    /* Don't move a reg that has an explicit clobber.
		       It's not worth the pain to try to do it correctly.  */
		    may_not_move[REGNO (XEXP (x, 0))] = 1;

		  if (GET_CODE (x) == SET || GET_CODE (x) == CLOBBER)
		    {
		      dest = SET_DEST (x);
		      while (GET_CODE (dest) == SUBREG
			     || GET_CODE (dest) == ZERO_EXTRACT
			     || GET_CODE (dest) == SIGN_EXTRACT
			     || GET_CODE (dest) == STRICT_LOW_PART)
			dest = XEXP (dest, 0);
		      if (GET_CODE (dest) == REG)
			{
			  register int regno = REGNO (dest);
			  if (n_times_set[regno] > 0 && last_set[regno] == 0)
			    may_not_move[regno] = 1;
			  if (last_set[regno] != 0
			      && reg_used_between_p (dest, last_set[regno], insn))
			    may_not_move[regno] = 1;
			  if (n_times_set[regno] < 127)
			    ++n_times_set[regno];
			  last_set[regno] = insn;
			}
		    }
		}
	    }
	}

      if (GET_CODE (insn) == CODE_LABEL || GET_CODE (insn) == JUMP_INSN)
	bzero ((char *) last_set, nregs * sizeof (rtx));
    }
  *count_ptr = count;
}

/* Given a loop that is bounded by LOOP_START and LOOP_END
   and that is entered at SCAN_START,
   return 1 if the register set in SET contained in insn INSN is used by
   any insn that precedes INSN in cyclic order starting
   from the loop entry point.

   We don't want to use INSN_LUID here because if we restrict INSN to those
   that have a valid INSN_LUID, it means we cannot move an invariant out
   from an inner loop past two loops.  */

static int
loop_reg_used_before_p (set, insn, loop_start, scan_start, loop_end)
     rtx set, insn, loop_start, scan_start, loop_end;
{
  rtx reg = SET_DEST (set);
  rtx p;

  /* Scan forward checking for register usage.  If we hit INSN, we
     are done.  Otherwise, if we hit LOOP_END, wrap around to LOOP_START.  */
  for (p = scan_start; p != insn; p = NEXT_INSN (p))
    {
      if (GET_RTX_CLASS (GET_CODE (p)) == 'i'
	  && reg_overlap_mentioned_p (reg, PATTERN (p)))
	return 1;

      if (p == loop_end)
	p = loop_start;
    }

  return 0;
}

/* A "basic induction variable" or biv is a pseudo reg that is set
   (within this loop) only by incrementing or decrementing it.  */
/* A "general induction variable" or giv is a pseudo reg whose
   value is a linear function of a biv.  */

/* Bivs are recognized by `basic_induction_var';
   Givs by `general_induct_var'.  */

/* Indexed by register number, indicates whether or not register is an
   induction variable, and if so what type.  */

enum iv_mode *reg_iv_type;

/* Indexed by register number, contains pointer to `struct induction'
   if register is an induction variable.  This holds general info for
   all induction variables.  */

struct induction **reg_iv_info;

/* Indexed by register number, contains pointer to `struct iv_class'
   if register is a basic induction variable.  This holds info describing
   the class (a related group) of induction variables that the biv belongs
   to.  */

struct iv_class **reg_biv_class;

/* The head of a list which links together (via the next field)
   every iv class for the current loop.  */

struct iv_class *loop_iv_list;

/* Communication with routines called via `note_stores'.  */

static rtx note_insn;

/* Dummy register to have non-zero DEST_REG for DEST_ADDR type givs.  */

static rtx addr_placeholder;

/* ??? Unfinished optimizations, and possible future optimizations,
   for the strength reduction code.  */

/* ??? There is one more optimization you might be interested in doing: to
   allocate pseudo registers for frequently-accessed memory locations.
   If the same memory location is referenced each time around, it might
   be possible to copy it into a register before and out after.
   This is especially useful when the memory location is a variable which
   is in a stack slot because somewhere its address is taken.  If the
   loop doesn't contain a function call and the variable isn't volatile,
   it is safe to keep the value in a register for the duration of the
   loop. One tricky thing is that the copying of the value back from the
   register has to be done on all exits from the loop.  You need to check that
   all the exits from the loop go to the same place. */

/* ??? The interaction of biv elimination, and recognition of 'constant'
   bivs, may cause problems. */

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

/* Perform strength reduction and induction variable elimination.  */

/* Pseudo registers created during this function will be beyond the last
   valid index in several tables including n_times_set and regno_last_uid.
   This does not cause a problem here, because the added registers cannot be
   givs outside of their loop, and hence will never be reconsidered.
   But scan_loop must check regnos to make sure they are in bounds.  */

static void
strength_reduce (scan_start, end, loop_top, insn_count,
		 loop_start, loop_end)
     rtx scan_start;
     rtx end;
     rtx loop_top;
     int insn_count;
     rtx loop_start;
     rtx loop_end;
{
  rtx p;
  rtx set;
  rtx inc_val;
  rtx mult_val;
  rtx dest_reg;
  /* This is 1 if current insn is not executed at least once for every loop
     iteration.  */
  int not_every_iteration = 0;
  /* This is 1 if current insn may be executed more than once for every
     loop iteration.  */
  int maybe_multiple = 0;
  /* Temporary list pointers for traversing loop_iv_list.  */
  struct iv_class *bl, **backbl;
  /* Ratio of extra register life span we can justify
     for saving an instruction.  More if loop doesn't call subroutines
     since in that case saving an insn makes more difference
     and more registers are available.  */
  /* ??? could set this to last value of threshold in move_movables */
  int threshold = (loop_has_call ? 1 : 2) * (3 + n_non_fixed_regs);
  /* Map of pseudo-register replacements.  */
  rtx *reg_map;
  int call_seen;
  rtx test;
  rtx end_insert_before;
  int loop_depth = 0;

  reg_iv_type = (enum iv_mode *) alloca (max_reg_before_loop
					 * sizeof (enum iv_mode *));
  bzero ((char *) reg_iv_type, max_reg_before_loop * sizeof (enum iv_mode *));
  reg_iv_info = (struct induction **)
    alloca (max_reg_before_loop * sizeof (struct induction *));
  bzero ((char *) reg_iv_info, (max_reg_before_loop
				* sizeof (struct induction *)));
  reg_biv_class = (struct iv_class **)
    alloca (max_reg_before_loop * sizeof (struct iv_class *));
  bzero ((char *) reg_biv_class, (max_reg_before_loop
				  * sizeof (struct iv_class *)));

  loop_iv_list = 0;
  addr_placeholder = gen_reg_rtx (Pmode);

  /* Save insn immediately after the loop_end.  Insns inserted after loop_end
     must be put before this insn, so that they will appear in the right
     order (i.e. loop order). 

     If loop_end is the end of the current function, then emit a 
     NOTE_INSN_DELETED after loop_end and set end_insert_before to the
     dummy note insn.  */
  if (NEXT_INSN (loop_end) != 0)
    end_insert_before = NEXT_INSN (loop_end);
  else
    end_insert_before = emit_note_after (NOTE_INSN_DELETED, loop_end);

  /* Scan through loop to find all possible bivs.  */

  p = scan_start;
  while (1)
    {
      p = NEXT_INSN (p);
      /* At end of a straight-in loop, we are done.
	 At end of a loop entered at the bottom, scan the top.  */
      if (p == scan_start)
	break;
      if (p == end)
	{
	  if (loop_top != 0)
	    p = loop_top;
	  else
	    break;
	  if (p == scan_start)
	    break;
	}

      if (GET_CODE (p) == INSN
	  && (set = single_set (p))
	  && GET_CODE (SET_DEST (set)) == REG)
	{
	  dest_reg = SET_DEST (set);
	  if (REGNO (dest_reg) < max_reg_before_loop
	      && REGNO (dest_reg) >= FIRST_PSEUDO_REGISTER
	      && reg_iv_type[REGNO (dest_reg)] != NOT_BASIC_INDUCT)
	    {
	      if (basic_induction_var (SET_SRC (set), GET_MODE (SET_SRC (set)),
				       dest_reg, p, &inc_val, &mult_val))
		{
		  /* It is a possible basic induction variable.
		     Create and initialize an induction structure for it.  */

		  struct induction *v
		    = (struct induction *) alloca (sizeof (struct induction));

		  record_biv (v, p, dest_reg, inc_val, mult_val,
			      not_every_iteration, maybe_multiple);
		  reg_iv_type[REGNO (dest_reg)] = BASIC_INDUCT;
		}
	      else if (REGNO (dest_reg) < max_reg_before_loop)
		reg_iv_type[REGNO (dest_reg)] = NOT_BASIC_INDUCT;
	    }
	}

      /* Past CODE_LABEL, we get to insns that may be executed multiple
	 times.  The only way we can be sure that they can't is if every
	 every jump insn between here and the end of the loop either
	 returns, exits the loop, or is a forward jump.  */

      if (GET_CODE (p) == CODE_LABEL)
	{
	  rtx insn = p;

	  maybe_multiple = 0;

	  while (1)
	    {
	      insn = NEXT_INSN (insn);
	      if (insn == scan_start)
		break;
	      if (insn == end)
		{
		  if (loop_top != 0)
		    insn = loop_top;
		  else
		    break;
		  if (insn == scan_start)
		    break;
		}

	      if (GET_CODE (insn) == JUMP_INSN
		  && GET_CODE (PATTERN (insn)) != RETURN
		  && (! condjump_p (insn)
		      || (JUMP_LABEL (insn) != 0
			  && (INSN_UID (JUMP_LABEL (insn)) >= max_uid_for_loop
			      || INSN_UID (insn) >= max_uid_for_loop
			      || (INSN_LUID (JUMP_LABEL (insn))
				  < INSN_LUID (insn))))))
	      {
		maybe_multiple = 1;
		break;
	      }
	    }
	}

      /* Past a label or a jump, we get to insns for which we can't count
	 on whether or how many times they will be executed during each
	 iteration.  */
      /* This code appears in three places, once in scan_loop, and twice
	 in strength_reduce.  */
      if ((GET_CODE (p) == CODE_LABEL || GET_CODE (p) == JUMP_INSN)
	  /* If we enter the loop in the middle, and scan around to the
	     beginning, don't set not_every_iteration for that.
	     This can be any kind of jump, since we want to know if insns
	     will be executed if the loop is executed.  */
	  && ! (GET_CODE (p) == JUMP_INSN && JUMP_LABEL (p) == loop_top
		&& ((NEXT_INSN (NEXT_INSN (p)) == loop_end && simplejump_p (p))
		    || (NEXT_INSN (p) == loop_end && condjump_p (p)))))
	not_every_iteration = 1;

      else if (GET_CODE (p) == NOTE)
	{
	  /* At the virtual top of a converted loop, insns are again known to
	     be executed each iteration: logically, the loop begins here
	     even though the exit code has been duplicated.  */
	  if (NOTE_LINE_NUMBER (p) == NOTE_INSN_LOOP_VTOP && loop_depth == 0)
	    not_every_iteration = 0;
	  else if (NOTE_LINE_NUMBER (p) == NOTE_INSN_LOOP_BEG)
	    loop_depth++;
	  else if (NOTE_LINE_NUMBER (p) == NOTE_INSN_LOOP_END)
	    loop_depth--;
	}

      /* Unlike in the code motion pass where MAYBE_NEVER indicates that
	 an insn may never be executed, NOT_EVERY_ITERATION indicates whether
	 or not an insn is known to be executed each iteration of the
	 loop, whether or not any iterations are known to occur.

	 Therefore, if we have just passed a label and have no more labels
	 between here and the test insn of the loop, we know these insns
	 will be executed each iteration.  This can also happen if we
	 have just passed a jump, for example, when there are nested loops.  */

      if (not_every_iteration && GET_CODE (p) == CODE_LABEL
	  && no_labels_between_p (p, loop_end))
	not_every_iteration = 0;
    }

  /* Scan loop_iv_list to remove all regs that proved not to be bivs.
     Make a sanity check against n_times_set.  */
  for (backbl = &loop_iv_list, bl = *backbl; bl; bl = bl->next)
    {
      if (reg_iv_type[bl->regno] != BASIC_INDUCT
	  /* Above happens if register modified by subreg, etc.  */
	  /* Make sure it is not recognized as a basic induction var: */
	  || n_times_set[bl->regno] != bl->biv_count
	  /* If never incremented, it is invariant that we decided not to
	     move.  So leave it alone.  */
	  || ! bl->incremented)
	{
	  if (loop_dump_stream)
	    fprintf (loop_dump_stream, "Reg %d: biv discarded, %s\n",
		     bl->regno,
		     (reg_iv_type[bl->regno] != BASIC_INDUCT
		      ? "not induction variable"
		      : (! bl->incremented ? "never incremented"
			 : "count error")));
	  
	  reg_iv_type[bl->regno] = NOT_BASIC_INDUCT;
	  *backbl = bl->next;
	}
      else
	{
	  backbl = &bl->next;

	  if (loop_dump_stream)
	    fprintf (loop_dump_stream, "Reg %d: biv verified\n", bl->regno);
	}
    }

  /* Exit if there are no bivs.  */
  if (! loop_iv_list)
    {
      /* Can still unroll the loop anyways, but indicate that there is no
	 strength reduction info available.  */
      if (flag_unroll_loops)
	unroll_loop (loop_end, insn_count, loop_start, end_insert_before, 0);

      return;
    }

  /* Find initial value for each biv by searching backwards from loop_start,
     halting at first label.  Also record any test condition.  */

  call_seen = 0;
  for (p = loop_start; p && GET_CODE (p) != CODE_LABEL; p = PREV_INSN (p))
    {
      note_insn = p;

      if (GET_CODE (p) == CALL_INSN)
	call_seen = 1;

      if (GET_CODE (p) == INSN || GET_CODE (p) == JUMP_INSN
	  || GET_CODE (p) == CALL_INSN)
	note_stores (PATTERN (p), record_initial);

      /* Record any test of a biv that branches around the loop if no store
	 between it and the start of loop.  We only care about tests with
	 constants and registers and only certain of those.  */
      if (GET_CODE (p) == JUMP_INSN
	  && JUMP_LABEL (p) != 0
	  && next_real_insn (JUMP_LABEL (p)) == next_real_insn (loop_end)
	  && (test = get_condition_for_loop (p)) != 0
	  && GET_CODE (XEXP (test, 0)) == REG
	  && REGNO (XEXP (test, 0)) < max_reg_before_loop
	  && (bl = reg_biv_class[REGNO (XEXP (test, 0))]) != 0
	  && valid_initial_value_p (XEXP (test, 1), p, call_seen, loop_start)
	  && bl->init_insn == 0)
	{
	  /* If an NE test, we have an initial value!  */
	  if (GET_CODE (test) == NE)
	    {
	      bl->init_insn = p;
	      bl->init_set = gen_rtx (SET, VOIDmode,
				      XEXP (test, 0), XEXP (test, 1));
	    }
	  else
	    bl->initial_test = test;
	}
    }

  /* Look at the each biv and see if we can say anything better about its
     initial value from any initializing insns set up above.  (This is done
     in two passes to avoid missing SETs in a PARALLEL.)  */
  for (bl = loop_iv_list; bl; bl = bl->next)
    {
      rtx src;

      if (! bl->init_insn)
	continue;

      src = SET_SRC (bl->init_set);

      if (loop_dump_stream)
	fprintf (loop_dump_stream,
		 "Biv %d initialized at insn %d: initial value ",
		 bl->regno, INSN_UID (bl->init_insn));

      if ((GET_MODE (src) == GET_MODE (regno_reg_rtx[bl->regno])
	   || GET_MODE (src) == VOIDmode)
	  && valid_initial_value_p (src, bl->init_insn, call_seen, loop_start))
	{
	  bl->initial_value = src;

	  if (loop_dump_stream)
	    {
	      if (GET_CODE (src) == CONST_INT)
		fprintf (loop_dump_stream, "%d\n", INTVAL (src));
	      else
		{
		  print_rtl (loop_dump_stream, src);
		  fprintf (loop_dump_stream, "\n");
		}
	    }
	}
      else
	{
	  /* Biv initial value is not simple move,
	     so let it keep initial value of "itself".  */

	  if (loop_dump_stream)
	    fprintf (loop_dump_stream, "is complex\n");
	}
    }

  /* Search the loop for general induction variables.  */

  /* A register is a giv if: it is only set once, it is a function of a
     biv and a constant (or invariant), and it is not a biv.  */

  not_every_iteration = 0;
  loop_depth = 0;
  p = scan_start;
  while (1)
    {
      p = NEXT_INSN (p);
      /* At end of a straight-in loop, we are done.
	 At end of a loop entered at the bottom, scan the top.  */
      if (p == scan_start)
	break;
      if (p == end)
	{
	  if (loop_top != 0)
	    p = loop_top;
	  else
	    break;
	  if (p == scan_start)
	    break;
	}

      /* Look for a general induction variable in a register.  */
      if (GET_CODE (p) == INSN
	  && (set = single_set (p))
	  && GET_CODE (SET_DEST (set)) == REG
	  && ! may_not_optimize[REGNO (SET_DEST (set))])
	{
	  rtx src_reg;
	  rtx add_val;
	  rtx mult_val;
	  int benefit;
	  rtx regnote = 0;

	  dest_reg = SET_DEST (set);
	  if (REGNO (dest_reg) < FIRST_PSEUDO_REGISTER)
	    continue;

	  if (/* SET_SRC is a giv.  */
	      ((benefit = general_induction_var (SET_SRC (set),
						 &src_reg, &add_val,
						 &mult_val))
	       /* Equivalent expression is a giv. */
	       || ((regnote = find_reg_note (p, REG_EQUAL, NULL_RTX))
		   && (benefit = general_induction_var (XEXP (regnote, 0),
							&src_reg,
							&add_val, &mult_val))))
	      /* Don't try to handle any regs made by loop optimization.
		 We have nothing on them in regno_first_uid, etc.  */
	      && REGNO (dest_reg) < max_reg_before_loop
	      /* Don't recognize a BASIC_INDUCT_VAR here.  */
	      && dest_reg != src_reg
	      /* This must be the only place where the register is set.  */
	      && (n_times_set[REGNO (dest_reg)] == 1
		  /* or all sets must be consecutive and make a giv. */
		  || (benefit = consec_sets_giv (benefit, p,
						 src_reg, dest_reg,
						 &add_val, &mult_val))))
	    {
	      int count;
	      struct induction *v
		= (struct induction *) alloca (sizeof (struct induction));
	      rtx temp;

	      /* If this is a library call, increase benefit.  */
	      if (find_reg_note (p, REG_RETVAL, NULL_RTX))
		benefit += libcall_benefit (p);

	      /* Skip the consecutive insns, if there are any.  */
	      for (count = n_times_set[REGNO (dest_reg)] - 1;
		   count > 0; count--)
		{
		  /* If first insn of libcall sequence, skip to end.
		     Do this at start of loop, since INSN is guaranteed to
		     be an insn here.  */
		  if (GET_CODE (p) != NOTE
		      && (temp = find_reg_note (p, REG_LIBCALL, NULL_RTX)))
		    p = XEXP (temp, 0);

		  do p = NEXT_INSN (p);
		  while (GET_CODE (p) == NOTE);
		}

	      record_giv (v, p, src_reg, dest_reg, mult_val, add_val, benefit,
			  DEST_REG, not_every_iteration, NULL_PTR, loop_start,
			  loop_end);

	    }
	}

#ifndef DONT_REDUCE_ADDR
      /* Look for givs which are memory addresses.  */
      /* This resulted in worse code on a VAX 8600.  I wonder if it
	 still does.  */
      if (GET_CODE (p) == INSN)
	find_mem_givs (PATTERN (p), p, not_every_iteration, loop_start,
		       loop_end);
#endif

      /* Update the status of whether giv can derive other givs.  This can
	 change when we pass a label or an insn that updates a biv.  */
      if (GET_CODE (p) == INSN || GET_CODE (p) == JUMP_INSN
	|| GET_CODE (p) == CODE_LABEL)
	update_giv_derive (p);

      /* Past a label or a jump, we get to insns for which we can't count
	 on whether or how many times they will be executed during each
	 iteration.  */
      /* This code appears in three places, once in scan_loop, and twice
	 in strength_reduce.  */
      if ((GET_CODE (p) == CODE_LABEL || GET_CODE (p) == JUMP_INSN)
	  /* If we enter the loop in the middle, and scan around
	     to the beginning, don't set not_every_iteration for that.
	     This can be any kind of jump, since we want to know if insns
	     will be executed if the loop is executed.  */
	  && ! (GET_CODE (p) == JUMP_INSN && JUMP_LABEL (p) == loop_top
		&& ((NEXT_INSN (NEXT_INSN (p)) == loop_end && simplejump_p (p))
		    || (NEXT_INSN (p) == loop_end && condjump_p (p)))))
	not_every_iteration = 1;

      else if (GET_CODE (p) == NOTE)
	{
	  /* At the virtual top of a converted loop, insns are again known to
	     be executed each iteration: logically, the loop begins here
	     even though the exit code has been duplicated.  */
	  if (NOTE_LINE_NUMBER (p) == NOTE_INSN_LOOP_VTOP && loop_depth == 0)
	    not_every_iteration = 0;
	  else if (NOTE_LINE_NUMBER (p) == NOTE_INSN_LOOP_BEG)
	    loop_depth++;
	  else if (NOTE_LINE_NUMBER (p) == NOTE_INSN_LOOP_END)
	    loop_depth--;
	}

      /* Unlike in the code motion pass where MAYBE_NEVER indicates that
	 an insn may never be executed, NOT_EVERY_ITERATION indicates whether
	 or not an insn is known to be executed each iteration of the
	 loop, whether or not any iterations are known to occur.

	 Therefore, if we have just passed a label and have no more labels
	 between here and the test insn of the loop, we know these insns
	 will be executed each iteration.  */

      if (not_every_iteration && GET_CODE (p) == CODE_LABEL
	  && no_labels_between_p (p, loop_end))
	not_every_iteration = 0;
    }

  /* Try to calculate and save the number of loop iterations.  This is
     set to zero if the actual number can not be calculated.  This must
     be called after all giv's have been identified, since otherwise it may
     fail if the iteration variable is a giv.  */

  loop_n_iterations = loop_iterations (loop_start, loop_end);

  /* Now for each giv for which we still don't know whether or not it is
     replaceable, check to see if it is replaceable because its final value
     can be calculated.  This must be done after loop_iterations is called,
     so that final_giv_value will work correctly.  */

  for (bl = loop_iv_list; bl; bl = bl->next)
    {
      struct induction *v;

      for (v = bl->giv; v; v = v->next_iv)
	if (! v->replaceable && ! v->not_replaceable)
	  check_final_value (v, loop_start, loop_end);
    }

  /* Try to prove that the loop counter variable (if any) is always
     nonnegative; if so, record that fact with a REG_NONNEG note
     so that "decrement and branch until zero" insn can be used.  */
  check_dbra_loop (loop_end, insn_count, loop_start);

  /* Create reg_map to hold substitutions for replaceable giv regs.  */
  reg_map = (rtx *) alloca (max_reg_before_loop * sizeof (rtx));
  bzero ((char *) reg_map, max_reg_before_loop * sizeof (rtx));

  /* Examine each iv class for feasibility of strength reduction/induction
     variable elimination.  */

  for (bl = loop_iv_list; bl; bl = bl->next)
    {
      struct induction *v;
      int benefit;
      int all_reduced;
      rtx final_value = 0;

      /* Test whether it will be possible to eliminate this biv
	 provided all givs are reduced.  This is possible if either
	 the reg is not used outside the loop, or we can compute
	 what its final value will be.

	 For architectures with a decrement_and_branch_until_zero insn,
	 don't do this if we put a REG_NONNEG note on the endtest for
	 this biv.  */

      /* Compare against bl->init_insn rather than loop_start.
	 We aren't concerned with any uses of the biv between
	 init_insn and loop_start since these won't be affected
	 by the value of the biv elsewhere in the function, so
	 long as init_insn doesn't use the biv itself.
	 March 14, 1989 -- self@bayes.arc.nasa.gov */

      if ((uid_luid[regno_last_uid[bl->regno]] < INSN_LUID (loop_end)
	   && bl->init_insn
	   && INSN_UID (bl->init_insn) < max_uid_for_loop
	   && uid_luid[regno_first_uid[bl->regno]] >= INSN_LUID (bl->init_insn)
#ifdef HAVE_decrement_and_branch_until_zero
	   && ! bl->nonneg
#endif
	   && ! reg_mentioned_p (bl->biv->dest_reg, SET_SRC (bl->init_set)))
	  || ((final_value = final_biv_value (bl, loop_start, loop_end))
#ifdef HAVE_decrement_and_branch_until_zero
	      && ! bl->nonneg
#endif
	      ))
	bl->eliminable = maybe_eliminate_biv (bl, loop_start, end, 0,
					      threshold, insn_count);
      else
	{
	  if (loop_dump_stream)
	    {
	      fprintf (loop_dump_stream,
		       "Cannot eliminate biv %d.\n",
		       bl->regno);
	      fprintf (loop_dump_stream,
		       "First use: insn %d, last use: insn %d.\n",
		       regno_first_uid[bl->regno],
		       regno_last_uid[bl->regno]);
	    }
	}

      /* Combine all giv's for this iv_class.  */
      combine_givs (bl);

      /* This will be true at the end, if all givs which depend on this
	 biv have been strength reduced.
	 We can't (currently) eliminate the biv unless this is so.  */
      all_reduced = 1;

      /* Check each giv in this class to see if we will benefit by reducing
	 it.  Skip giv's combined with others.  */
      for (v = bl->giv; v; v = v->next_iv)
	{
	  struct induction *tv;

	  if (v->ignore || v->same)
	    continue;

	  benefit = v->benefit;

	  /* Reduce benefit if not replaceable, since we will insert
	     a move-insn to replace the insn that calculates this giv.
	     Don't do this unless the giv is a user variable, since it
	     will often be marked non-replaceable because of the duplication
	     of the exit code outside the loop.  In such a case, the copies
	     we insert are dead and will be deleted.  So they don't have
	     a cost.  Similar situations exist.  */
	  /* ??? The new final_[bg]iv_value code does a much better job
	     of finding replaceable giv's, and hence this code may no longer
	     be necessary.  */
	  if (! v->replaceable && ! bl->eliminable
	      && REG_USERVAR_P (v->dest_reg))
	    benefit -= copy_cost;

	  /* Decrease the benefit to count the add-insns that we will
	     insert to increment the reduced reg for the giv.  */
	  benefit -= add_cost * bl->biv_count;

	  /* Decide whether to strength-reduce this giv or to leave the code
	     unchanged (recompute it from the biv each time it is used).
	     This decision can be made independently for each giv.  */

	  /* ??? Perhaps attempt to guess whether autoincrement will handle
	     some of the new add insns; if so, can increase BENEFIT
	     (undo the subtraction of add_cost that was done above).  */

	  /* If an insn is not to be strength reduced, then set its ignore
	     flag, and clear all_reduced.  */

	  /* A giv that depends on a reversed biv must be reduced if it is
	     used after the loop exit, otherwise, it would have the wrong
	     value after the loop exit.  To make it simple, just reduce all
	     of such giv's whether or not we know they are used after the loop
	     exit.  */

	  if (v->lifetime * threshold * benefit < insn_count
	      && ! bl->reversed)
	    {
	      if (loop_dump_stream)
		fprintf (loop_dump_stream,
			 "giv of insn %d not worth while, %d vs %d.\n",
			 INSN_UID (v->insn),
			 v->lifetime * threshold * benefit, insn_count);
	      v->ignore = 1;
	      all_reduced = 0;
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
		    all_reduced = 0;
		    break;
		  }
	    }
	}

      /* Reduce each giv that we decided to reduce.  */

      for (v = bl->giv; v; v = v->next_iv)
	{
	  struct induction *tv;
	  if (! v->ignore && v->same == 0)
	    {
	      v->new_reg = gen_reg_rtx (v->mode);

	      /* For each place where the biv is incremented,
		 add an insn to increment the new, reduced reg for the giv.  */
	      for (tv = bl->biv; tv; tv = tv->next_iv)
		{
		  if (tv->mult_val == const1_rtx)
		    emit_iv_add_mult (tv->add_val, v->mult_val,
				      v->new_reg, v->new_reg, tv->insn);
		  else /* tv->mult_val == const0_rtx */
		    /* A multiply is acceptable here
		       since this is presumed to be seldom executed.  */
		    emit_iv_add_mult (tv->add_val, v->mult_val,
				      v->add_val, v->new_reg, tv->insn);
		}

	      /* Add code at loop start to initialize giv's reduced reg.  */

	      emit_iv_add_mult (bl->initial_value, v->mult_val,
				v->add_val, v->new_reg, loop_start);
	    }
	}

      /* Rescan all givs.  If a giv is the same as a giv not reduced, mark it
	 as not reduced.
	 
	 For each giv register that can be reduced now: if replaceable,
	 substitute reduced reg wherever the old giv occurs;
	 else add new move insn "giv_reg = reduced_reg".

	 Also check for givs whose first use is their definition and whose
	 last use is the definition of another giv.  If so, it is likely
	 dead and should not be used to eliminate a biv.  */
      for (v = bl->giv; v; v = v->next_iv)
	{
	  if (v->same && v->same->ignore)
	    v->ignore = 1;

	  if (v->ignore)
	    continue;

	  if (v->giv_type == DEST_REG
	      && regno_first_uid[REGNO (v->dest_reg)] == INSN_UID (v->insn))
	    {
	      struct induction *v1;

	      for (v1 = bl->giv; v1; v1 = v1->next_iv)
		if (regno_last_uid[REGNO (v->dest_reg)] == INSN_UID (v1->insn))
		  v->maybe_dead = 1;
	    }

	  /* Update expression if this was combined, in case other giv was
	     replaced.  */
	  if (v->same)
	    v->new_reg = replace_rtx (v->new_reg,
				      v->same->dest_reg, v->same->new_reg);

	  if (v->giv_type == DEST_ADDR)
	    /* Store reduced reg as the address in the memref where we found
	       this giv.  */
	    validate_change (v->insn, v->location, v->new_reg, 0);
	  else if (v->replaceable)
	    {
	      reg_map[REGNO (v->dest_reg)] = v->new_reg;

#if 0
	      /* I can no longer duplicate the original problem.  Perhaps
		 this is unnecessary now?  */

	      /* Replaceable; it isn't strictly necessary to delete the old
		 insn and emit a new one, because v->dest_reg is now dead.

		 However, especially when unrolling loops, the special
		 handling for (set REG0 REG1) in the second cse pass may
		 make v->dest_reg live again.  To avoid this problem, emit
		 an insn to set the original giv reg from the reduced giv.
		 We can not delete the original insn, since it may be part
		 of a LIBCALL, and the code in flow that eliminates dead
		 libcalls will fail if it is deleted.  */
	      emit_insn_after (gen_move_insn (v->dest_reg, v->new_reg),
			       v->insn);
#endif
	    }
	  else
	    {
	      /* Not replaceable; emit an insn to set the original giv reg from
		 the reduced giv, same as above.  */
	      emit_insn_after (gen_move_insn (v->dest_reg, v->new_reg),
			       v->insn);
	    }

	  /* When a loop is reversed, givs which depend on the reversed
	     biv, and which are live outside the loop, must be set to their
	     correct final value.  This insn is only needed if the giv is
	     not replaceable.  The correct final value is the same as the
	     value that the giv starts the reversed loop with.  */
	  if (bl->reversed && ! v->replaceable)
	    emit_iv_add_mult (bl->initial_value, v->mult_val,
			      v->add_val, v->dest_reg, end_insert_before);
	  else if (v->final_value)
	    {
	      rtx insert_before;

	      /* If the loop has multiple exits, emit the insn before the
		 loop to ensure that it will always be executed no matter
		 how the loop exits.  Otherwise, emit the insn after the loop,
		 since this is slightly more efficient.  */
	      if (loop_number_exit_count[uid_loop_num[INSN_UID (loop_start)]])
		insert_before = loop_start;
	      else
		insert_before = end_insert_before;
	      emit_insn_before (gen_move_insn (v->dest_reg, v->final_value),
				insert_before);

#if 0
	      /* If the insn to set the final value of the giv was emitted
		 before the loop, then we must delete the insn inside the loop
		 that sets it.  If this is a LIBCALL, then we must delete
		 every insn in the libcall.  Note, however, that
		 final_giv_value will only succeed when there are multiple
		 exits if the giv is dead at each exit, hence it does not
		 matter that the original insn remains because it is dead
		 anyways.  */
	      /* Delete the insn inside the loop that sets the giv since
		 the giv is now set before (or after) the loop.  */
	      delete_insn (v->insn);
#endif
	    }

	  if (loop_dump_stream)
	    {
	      fprintf (loop_dump_stream, "giv at %d reduced to ",
		       INSN_UID (v->insn));
	      print_rtl (loop_dump_stream, v->new_reg);
	      fprintf (loop_dump_stream, "\n");
	    }
	}

      /* All the givs based on the biv bl have been reduced if they
	 merit it.  */

      /* For each giv not marked as maybe dead that has been combined with a
	 second giv, clear any "maybe dead" mark on that second giv.
	 v->new_reg will either be or refer to the register of the giv it
	 combined with.

	 Doing this clearing avoids problems in biv elimination where a
	 giv's new_reg is a complex value that can't be put in the insn but
	 the giv combined with (with a reg as new_reg) is marked maybe_dead.
	 Since the register will be used in either case, we'd prefer it be
	 used from the simpler giv.  */

      for (v = bl->giv; v; v = v->next_iv)
	if (! v->maybe_dead && v->same)
	  v->same->maybe_dead = 0;

      /* Try to eliminate the biv, if it is a candidate.
	 This won't work if ! all_reduced,
	 since the givs we planned to use might not have been reduced.

	 We have to be careful that we didn't initially think we could eliminate
	 this biv because of a giv that we now think may be dead and shouldn't
	 be used as a biv replacement.  

	 Also, there is the possibility that we may have a giv that looks
	 like it can be used to eliminate a biv, but the resulting insn
	 isn't valid.  This can happen, for example, on the 88k, where a 
	 JUMP_INSN can compare a register only with zero.  Attempts to
	 replace it with a compare with a constant will fail.

	 Note that in cases where this call fails, we may have replaced some
	 of the occurrences of the biv with a giv, but no harm was done in
	 doing so in the rare cases where it can occur.  */

      if (all_reduced == 1 && bl->eliminable
	  && maybe_eliminate_biv (bl, loop_start, end, 1,
				  threshold, insn_count))

	{
	  /* ?? If we created a new test to bypass the loop entirely,
	     or otherwise drop straight in, based on this test, then
	     we might want to rewrite it also.  This way some later
	     pass has more hope of removing the initialization of this
	     biv entirely. */

	  /* If final_value != 0, then the biv may be used after loop end
	     and we must emit an insn to set it just in case.

	     Reversed bivs already have an insn after the loop setting their
	     value, so we don't need another one.  We can't calculate the
	     proper final value for such a biv here anyways. */
	  if (final_value != 0 && ! bl->reversed)
	    {
	      rtx insert_before;

	      /* If the loop has multiple exits, emit the insn before the
		 loop to ensure that it will always be executed no matter
		 how the loop exits.  Otherwise, emit the insn after the
		 loop, since this is slightly more efficient.  */
	      if (loop_number_exit_count[uid_loop_num[INSN_UID (loop_start)]])
		insert_before = loop_start;
	      else
		insert_before = end_insert_before;

	      emit_insn_before (gen_move_insn (bl->biv->dest_reg, final_value),
				end_insert_before);
	    }

#if 0
	  /* Delete all of the instructions inside the loop which set
	     the biv, as they are all dead.  If is safe to delete them,
	     because an insn setting a biv will never be part of a libcall.  */
	  /* However, deleting them will invalidate the regno_last_uid info,
	     so keeping them around is more convenient.  Final_biv_value
	     will only succeed when there are multiple exits if the biv
	     is dead at each exit, hence it does not matter that the original
	     insn remains, because it is dead anyways.  */
	  for (v = bl->biv; v; v = v->next_iv)
	    delete_insn (v->insn);
#endif

	  if (loop_dump_stream)
	    fprintf (loop_dump_stream, "Reg %d: biv eliminated\n",
		     bl->regno);
	}
    }

  /* Go through all the instructions in the loop, making all the
     register substitutions scheduled in REG_MAP.  */

  for (p = loop_start; p != end; p = NEXT_INSN (p))
    if (GET_CODE (p) == INSN || GET_CODE (p) == JUMP_INSN
 	|| GET_CODE (p) == CALL_INSN)
      {
	replace_regs (PATTERN (p), reg_map, max_reg_before_loop, 0);
	replace_regs (REG_NOTES (p), reg_map, max_reg_before_loop, 0);
	INSN_CODE (p) = -1;
      }

  /* Unroll loops from within strength reduction so that we can use the
     induction variable information that strength_reduce has already
     collected.  */
  
  if (flag_unroll_loops)
    unroll_loop (loop_end, insn_count, loop_start, end_insert_before, 1);

  if (loop_dump_stream)
    fprintf (loop_dump_stream, "\n");
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
#ifndef SMALL_REGISTER_CLASSES
      && call_used_regs[REGNO (x)] && call_seen
#endif
      )
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
   every loop iteration.  */

static void
find_mem_givs (x, insn, not_every_iteration, loop_start, loop_end)
     rtx x;
     rtx insn;
     int not_every_iteration;
     rtx loop_start, loop_end;
{
  register int i, j;
  register enum rtx_code code;
  register char *fmt;

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
	int benefit;

	benefit = general_induction_var (XEXP (x, 0),
					 &src_reg, &add_val, &mult_val);

	/* Don't make a DEST_ADDR giv with mult_val == 1 && add_val == 0.
	   Such a giv isn't useful.  */
	if (benefit > 0 && (mult_val != const1_rtx || add_val != const0_rtx))
	  {
	    /* Found one; record it.  */
	    struct induction *v
	      = (struct induction *) oballoc (sizeof (struct induction));

	    record_giv (v, insn, src_reg, addr_placeholder, mult_val,
			add_val, benefit, DEST_ADDR, not_every_iteration,
			&XEXP (x, 0), loop_start, loop_end);

	    v->mem_mode = GET_MODE (x);
	  }
	return;
      }
    }

  /* Recursively scan the subexpressions for other mem refs.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    if (fmt[i] == 'e')
      find_mem_givs (XEXP (x, i), insn, not_every_iteration, loop_start,
		     loop_end);
    else if (fmt[i] == 'E')
      for (j = 0; j < XVECLEN (x, i); j++)
	find_mem_givs (XVECEXP (x, i, j), insn, not_every_iteration,
		       loop_start, loop_end);
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
record_biv (v, insn, dest_reg, inc_val, mult_val,
	    not_every_iteration, maybe_multiple)
     struct induction *v;
     rtx insn;
     rtx dest_reg;
     rtx inc_val;
     rtx mult_val;
     int not_every_iteration;
     int maybe_multiple;
{
  struct iv_class *bl;

  v->insn = insn;
  v->src_reg = dest_reg;
  v->dest_reg = dest_reg;
  v->mult_val = mult_val;
  v->add_val = inc_val;
  v->mode = GET_MODE (dest_reg);
  v->always_computable = ! not_every_iteration;
  v->maybe_multiple = maybe_multiple;

  /* Add this to the reg's iv_class, creating a class
     if this is the first incrementation of the reg.  */

  bl = reg_biv_class[REGNO (dest_reg)];
  if (bl == 0)
    {
      /* Create and initialize new iv_class.  */

      bl = (struct iv_class *) oballoc (sizeof (struct iv_class));

      bl->regno = REGNO (dest_reg);
      bl->biv = 0;
      bl->giv = 0;
      bl->biv_count = 0;
      bl->giv_count = 0;

      /* Set initial value to the reg itself.  */
      bl->initial_value = dest_reg;
      /* We haven't seen the initializing insn yet */
      bl->init_insn = 0;
      bl->init_set = 0;
      bl->initial_test = 0;
      bl->incremented = 0;
      bl->eliminable = 0;
      bl->nonneg = 0;
      bl->reversed = 0;
      bl->total_benefit = 0;

      /* Add this class to loop_iv_list.  */
      bl->next = loop_iv_list;
      loop_iv_list = bl;

      /* Put it in the array of biv register classes.  */
      reg_biv_class[REGNO (dest_reg)] = bl;
    }

  /* Update IV_CLASS entry for this biv.  */
  v->next_iv = bl->biv;
  bl->biv = v;
  bl->biv_count++;
  if (mult_val == const1_rtx)
    bl->incremented = 1;

  if (loop_dump_stream)
    {
      fprintf (loop_dump_stream,
	       "Insn %d: possible biv, reg %d,",
	       INSN_UID (insn), REGNO (dest_reg));
      if (GET_CODE (inc_val) == CONST_INT)
	fprintf (loop_dump_stream, " const = %d\n",
		 INTVAL (inc_val));
      else
	{
	  fprintf (loop_dump_stream, " const = ");
	  print_rtl (loop_dump_stream, inc_val);
	  fprintf (loop_dump_stream, "\n");
	}
    }
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
record_giv (v, insn, src_reg, dest_reg, mult_val, add_val, benefit,
	    type, not_every_iteration, location, loop_start, loop_end)
     struct induction *v;
     rtx insn;
     rtx src_reg;
     rtx dest_reg;
     rtx mult_val, add_val;
     int benefit;
     enum g_types type;
     int not_every_iteration;
     rtx *location;
     rtx loop_start, loop_end;
{
  struct induction *b;
  struct iv_class *bl;
  rtx set = single_set (insn);
  rtx p;

  v->insn = insn;
  v->src_reg = src_reg;
  v->giv_type = type;
  v->dest_reg = dest_reg;
  v->mult_val = mult_val;
  v->add_val = add_val;
  v->benefit = benefit;
  v->location = location;
  v->cant_derive = 0;
  v->combined_with = 0;
  v->maybe_multiple = 0;
  v->maybe_dead = 0;
  v->derive_adjustment = 0;
  v->same = 0;
  v->ignore = 0;
  v->new_reg = 0;
  v->final_value = 0;
  v->same_insn = 0;

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

  if (type == DEST_ADDR)
    {
      v->mode = GET_MODE (*location);
      v->lifetime = 1;
      v->times_used = 1;
    }
  else /* type == DEST_REG */
    {
      v->mode = GET_MODE (SET_DEST (set));

      v->lifetime = (uid_luid[regno_last_uid[REGNO (dest_reg)]]
		     - uid_luid[regno_first_uid[REGNO (dest_reg)]]);

      v->times_used = n_times_used[REGNO (dest_reg)];

      /* If the lifetime is zero, it means that this register is
	 really a dead store.  So mark this as a giv that can be
	 ignored.  This will not prevent the biv from being eliminated. */
      if (v->lifetime == 0)
	v->ignore = 1;

      reg_iv_type[REGNO (dest_reg)] = GENERAL_INDUCT;
      reg_iv_info[REGNO (dest_reg)] = v;
    }

  /* Add the giv to the class of givs computed from one biv.  */

  bl = reg_biv_class[REGNO (src_reg)];
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

      if (regno_first_uid[REGNO (dest_reg)] == INSN_UID (insn)
	  /* Previous line always fails if INSN was moved by loop opt.  */
	  && uid_luid[regno_last_uid[REGNO (dest_reg)]] < INSN_LUID (loop_end)
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
		  || ((uid_luid[INSN_UID (b->insn)]
		       >= uid_luid[regno_first_uid[REGNO (dest_reg)]])
		      && (uid_luid[INSN_UID (b->insn)]
			  <= uid_luid[regno_last_uid[REGNO (dest_reg)]])))
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
	      if (back_branch_in_range_p (b->insn, loop_start, loop_end))
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

  if (loop_dump_stream)
    {
      if (type == DEST_REG)
 	fprintf (loop_dump_stream, "Insn %d: giv reg %d",
		 INSN_UID (insn), REGNO (dest_reg));
      else
 	fprintf (loop_dump_stream, "Insn %d: dest address",
 		 INSN_UID (insn));

      fprintf (loop_dump_stream, " src reg %d benefit %d",
	       REGNO (src_reg), v->benefit);
      fprintf (loop_dump_stream, " used %d lifetime %d",
	       v->times_used, v->lifetime);

      if (v->replaceable)
 	fprintf (loop_dump_stream, " replaceable");

      if (GET_CODE (mult_val) == CONST_INT)
	fprintf (loop_dump_stream, " mult %d",
 		 INTVAL (mult_val));
      else
	{
	  fprintf (loop_dump_stream, " mult ");
	  print_rtl (loop_dump_stream, mult_val);
	}

      if (GET_CODE (add_val) == CONST_INT)
	fprintf (loop_dump_stream, " add %d",
		 INTVAL (add_val));
      else
	{
	  fprintf (loop_dump_stream, " add ");
	  print_rtl (loop_dump_stream, add_val);
	}
    }

  if (loop_dump_stream)
    fprintf (loop_dump_stream, "\n");

}


/* All this does is determine whether a giv can be made replaceable because
   its final value can be calculated.  This code can not be part of record_giv
   above, because final_giv_value requires that the number of loop iterations
   be known, and that can not be accurately calculated until after all givs
   have been identified.  */

static void
check_final_value (v, loop_start, loop_end)
     struct induction *v;
     rtx loop_start, loop_end;
{
  struct iv_class *bl;
  rtx final_value = 0;

  bl = reg_biv_class[REGNO (v->src_reg)];

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
     - no assignments to the biv occur during the giv's lifetime.  */

#if 0
  /* This is only called now when replaceable is known to be false.  */
  /* Clear replaceable, so that it won't confuse final_giv_value.  */
  v->replaceable = 0;
#endif

  if ((final_value = final_giv_value (v, loop_start, loop_end))
      && (v->always_computable || last_use_this_basic_block (v->dest_reg, v->insn)))
    {
      int biv_increment_seen = 0;
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
	  if (p == loop_end)
	    p = NEXT_INSN (loop_start);
	  if (p == v->insn)
	    break;

	  if (GET_CODE (p) == INSN || GET_CODE (p) == JUMP_INSN
	      || GET_CODE (p) == CALL_INSN)
	    {
	      if (biv_increment_seen)
		{
		  if (reg_mentioned_p (v->dest_reg, PATTERN (p)))
		    {
		      v->replaceable = 0;
		      v->not_replaceable = 1;
		      break;
		    }
		}
	      else if (GET_CODE (PATTERN (p)) == SET
		       && SET_DEST (PATTERN (p)) == v->src_reg)
		biv_increment_seen = 1;
	      else if (reg_mentioned_p (v->dest_reg, PATTERN (p)))
		last_giv_use = p;
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
	      if (p == loop_end)
		p = NEXT_INSN (loop_start);
	      if (p == last_giv_use)
		break;

	      if (GET_CODE (p) == JUMP_INSN && JUMP_LABEL (p)
		  && LABEL_NAME (JUMP_LABEL (p))
		  && ((INSN_LUID (JUMP_LABEL (p)) < INSN_LUID (v->insn)
		       && INSN_LUID (JUMP_LABEL (p)) > INSN_LUID (loop_start))
		      || (INSN_LUID (JUMP_LABEL (p)) > INSN_LUID (last_giv_use)
			  && INSN_LUID (JUMP_LABEL (p)) < INSN_LUID (loop_end))))
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
update_giv_derive (p)
     rtx p;
{
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

  for (bl = loop_iv_list; bl; bl = bl->next)
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
		  tem = 0;

		  if (biv->mult_val == const1_rtx)
		    tem = simplify_giv_expr (gen_rtx (MULT, giv->mode,
						      biv->add_val,
						      giv->mult_val),
					     &dummy);

		  if (tem && giv->derive_adjustment)
		    tem = simplify_giv_expr (gen_rtx (PLUS, giv->mode, tem,
						      giv->derive_adjustment),
					     &dummy);
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
   and store the additive term into *INC_VAL.

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
basic_induction_var (x, mode, dest_reg, p, inc_val, mult_val)
     register rtx x;
     enum machine_mode mode;
     rtx p;
     rtx dest_reg;
     rtx *inc_val;
     rtx *mult_val;
{
  register enum rtx_code code;
  rtx arg;
  rtx insn, set = 0;

  code = GET_CODE (x);
  switch (code)
    {
    case PLUS:
      if (XEXP (x, 0) == dest_reg
	  || (GET_CODE (XEXP (x, 0)) == SUBREG
	      && SUBREG_PROMOTED_VAR_P (XEXP (x, 0))
	      && SUBREG_REG (XEXP (x, 0)) == dest_reg))
 	arg = XEXP (x, 1);
      else if (XEXP (x, 1) == dest_reg
	       || (GET_CODE (XEXP (x, 1)) == SUBREG
		   && SUBREG_PROMOTED_VAR_P (XEXP (x, 1))
		   && SUBREG_REG (XEXP (x, 1)) == dest_reg))
	arg = XEXP (x, 0);
      else
 	return 0;

      if (invariant_p (arg) != 1)
	return 0;

      *inc_val = convert_modes (GET_MODE (dest_reg), GET_MODE (x), arg, 0);
      *mult_val = const1_rtx;
      return 1;

    case SUBREG:
      /* If this is a SUBREG for a promoted variable, check the inner
	 value.  */
      if (SUBREG_PROMOTED_VAR_P (x))
	return basic_induction_var (SUBREG_REG (x), GET_MODE (SUBREG_REG (x)),
				    dest_reg, p, inc_val, mult_val);

    case REG:
      /* If this register is assigned in the previous insn, look at its
	 source, but don't go outside the loop or past a label.  */

      for (insn = PREV_INSN (p);
	   (insn && GET_CODE (insn) == NOTE
	    && NOTE_LINE_NUMBER (insn) != NOTE_INSN_LOOP_BEG);
	   insn = PREV_INSN (insn))
	;

      if (insn)
	set = single_set (insn);

      if (set != 0
	  && (SET_DEST (set) == x
	      || (GET_CODE (SET_DEST (set)) == SUBREG
		  && (GET_MODE_SIZE (GET_MODE (SET_DEST (set)))
		      <= UNITS_PER_WORD)
		  && SUBREG_REG (SET_DEST (set)) == x)))
	return basic_induction_var (SET_SRC (set),
				    (GET_MODE (SET_SRC (set)) == VOIDmode
				     ? GET_MODE (x)
				     : GET_MODE (SET_SRC (set))),
				    dest_reg, insn,
				    inc_val, mult_val);
      /* ... fall through ... */

      /* Can accept constant setting of biv only when inside inner most loop.
  	 Otherwise, a biv of an inner loop may be incorrectly recognized
	 as a biv of the outer loop,
	 causing code to be moved INTO the inner loop.  */
    case MEM:
      if (invariant_p (x) != 1)
	return 0;
    case CONST_INT:
    case SYMBOL_REF:
    case CONST:
      if (loops_enclosed == 1)
 	{
	  /* Possible bug here?  Perhaps we don't know the mode of X.  */
	  *inc_val = convert_modes (GET_MODE (dest_reg), mode, x, 0);
 	  *mult_val = const0_rtx;
 	  return 1;
 	}
      else
 	return 0;

    case SIGN_EXTEND:
      return basic_induction_var (XEXP (x, 0), GET_MODE (XEXP (x, 0)),
				  dest_reg, p, inc_val, mult_val);
    case ASHIFTRT:
      /* Similar, since this can be a sign extension.  */
      for (insn = PREV_INSN (p);
	   (insn && GET_CODE (insn) == NOTE
	    && NOTE_LINE_NUMBER (insn) != NOTE_INSN_LOOP_BEG);
	   insn = PREV_INSN (insn))
	;

      if (insn)
	set = single_set (insn);

      if (set && SET_DEST (set) == XEXP (x, 0)
	  && GET_CODE (XEXP (x, 1)) == CONST_INT
	  && INTVAL (XEXP (x, 1)) >= 0
	  && GET_CODE (SET_SRC (set)) == ASHIFT
	  && XEXP (x, 1) == XEXP (SET_SRC (set), 1))
	return basic_induction_var (XEXP (SET_SRC (set), 0),
				    GET_MODE (XEXP (x, 0)),
				    dest_reg, insn, inc_val, mult_val);
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
general_induction_var (x, src_reg, add_val, mult_val)
     rtx x;
     rtx *src_reg;
     rtx *add_val;
     rtx *mult_val;
{
  rtx orig_x = x;
  int benefit = 0;
  char *storage;

  /* If this is an invariant, forget it, it isn't a giv.  */
  if (invariant_p (x) == 1)
    return 0;

  /* See if the expression could be a giv and get its form.
     Mark our place on the obstack in case we don't find a giv.  */
  storage = (char *) oballoc (0);
  x = simplify_giv_expr (x, &benefit);
  if (x == 0)
    {
      obfree (storage);
      return 0;
    }

  switch (GET_CODE (x))
    {
    case USE:
    case CONST_INT:
      /* Since this is now an invariant and wasn't before, it must be a giv
	 with MULT_VAL == 0.  It doesn't matter which BIV we associate this
	 with.  */
      *src_reg = loop_iv_list->biv->dest_reg;
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

  benefit += rtx_cost (orig_x, SET);

  /* Always return some benefit if this is a giv so it will be detected
     as such.  This allows elimination of bivs that might otherwise
     not be eliminated.  */
  return benefit == 0 ? 1 : benefit;
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

static rtx
simplify_giv_expr (x, benefit)
     rtx x;
     int *benefit;
{
  enum machine_mode mode = GET_MODE (x);
  rtx arg0, arg1;
  rtx tem;

  /* If this is not an integer mode, or if we cannot do arithmetic in this
     mode, this can't be a giv.  */
  if (mode != VOIDmode
      && (GET_MODE_CLASS (mode) != MODE_INT
	  || GET_MODE_BITSIZE (mode) > HOST_BITS_PER_WIDE_INT))
    return 0;

  switch (GET_CODE (x))
    {
    case PLUS:
      arg0 = simplify_giv_expr (XEXP (x, 0), benefit);
      arg1 = simplify_giv_expr (XEXP (x, 1), benefit);
      if (arg0 == 0 || arg1 == 0)
	return 0;

      /* Put constant last, CONST_INT last if both constant.  */
      if ((GET_CODE (arg0) == USE
	   || GET_CODE (arg0) == CONST_INT)
	  && GET_CODE (arg1) != CONST_INT)
	tem = arg0, arg0 = arg1, arg1 = tem;

      /* Handle addition of zero, then addition of an invariant.  */
      if (arg1 == const0_rtx)
	return arg0;
      else if (GET_CODE (arg1) == CONST_INT || GET_CODE (arg1) == USE)
	switch (GET_CODE (arg0))
	  {
	  case CONST_INT:
	  case USE:
	    /* Both invariant.  Only valid if sum is machine operand.
	       First strip off possible USE on first operand.  */
	    if (GET_CODE (arg0) == USE)
	      arg0 = XEXP (arg0, 0);

	    tem = 0;
	    if (CONSTANT_P (arg0) && GET_CODE (arg1) == CONST_INT)
	      {
		tem = plus_constant (arg0, INTVAL (arg1));
		if (GET_CODE (tem) != CONST_INT)
		  tem = gen_rtx (USE, mode, tem);
	      }

	    return tem;

	  case REG:
	  case MULT:
	    /* biv + invar or mult + invar.  Return sum.  */
	    return gen_rtx (PLUS, mode, arg0, arg1);

	  case PLUS:
	    /* (a + invar_1) + invar_2.  Associate.  */
	    return simplify_giv_expr (gen_rtx (PLUS, mode,
					       XEXP (arg0, 0),
					       gen_rtx (PLUS, mode,
							XEXP (arg0, 1), arg1)),
				      benefit);

	  default:
	    abort ();
	  }

      /* Each argument must be either REG, PLUS, or MULT.  Convert REG to
	 MULT to reduce cases.  */
      if (GET_CODE (arg0) == REG)
	arg0 = gen_rtx (MULT, mode, arg0, const1_rtx);
      if (GET_CODE (arg1) == REG)
	arg1 = gen_rtx (MULT, mode, arg1, const1_rtx);

      /* Now have PLUS + PLUS, PLUS + MULT, MULT + PLUS, or MULT + MULT.
	 Put a MULT first, leaving PLUS + PLUS, MULT + PLUS, or MULT + MULT.
	 Recurse to associate the second PLUS.  */
      if (GET_CODE (arg1) == MULT)
	tem = arg0, arg0 = arg1, arg1 = tem;

      if (GET_CODE (arg1) == PLUS)
	  return simplify_giv_expr (gen_rtx (PLUS, mode,
					     gen_rtx (PLUS, mode,
						      arg0, XEXP (arg1, 0)),
					     XEXP (arg1, 1)),
				    benefit);

      /* Now must have MULT + MULT.  Distribute if same biv, else not giv.  */
      if (GET_CODE (arg0) != MULT || GET_CODE (arg1) != MULT)
	abort ();

      if (XEXP (arg0, 0) != XEXP (arg1, 0))
	return 0;

      return simplify_giv_expr (gen_rtx (MULT, mode,
					 XEXP (arg0, 0),
					 gen_rtx (PLUS, mode,
						  XEXP (arg0, 1),
						  XEXP (arg1, 1))),
				benefit);

    case MINUS:
      /* Handle "a - b" as "a + b * (-1)". */
      return simplify_giv_expr (gen_rtx (PLUS, mode,
					 XEXP (x, 0),
					 gen_rtx (MULT, mode,
						  XEXP (x, 1), constm1_rtx)),
				benefit);

    case MULT:
      arg0 = simplify_giv_expr (XEXP (x, 0), benefit);
      arg1 = simplify_giv_expr (XEXP (x, 1), benefit);
      if (arg0 == 0 || arg1 == 0)
	return 0;

      /* Put constant last, CONST_INT last if both constant.  */
      if ((GET_CODE (arg0) == USE || GET_CODE (arg0) == CONST_INT)
	  && GET_CODE (arg1) != CONST_INT)
	tem = arg0, arg0 = arg1, arg1 = tem;

      /* If second argument is not now constant, not giv.  */
      if (GET_CODE (arg1) != USE && GET_CODE (arg1) != CONST_INT)
	return 0;

      /* Handle multiply by 0 or 1.  */
      if (arg1 == const0_rtx)
	return const0_rtx;

      else if (arg1 == const1_rtx)
	return arg0;

      switch (GET_CODE (arg0))
	{
	case REG:
	  /* biv * invar.  Done.  */
	  return gen_rtx (MULT, mode, arg0, arg1);

	case CONST_INT:
	  /* Product of two constants.  */
	  return GEN_INT (INTVAL (arg0) * INTVAL (arg1));

	case USE:
	  /* invar * invar.  Not giv. */
	  return 0;

	case MULT:
	  /* (a * invar_1) * invar_2.  Associate.  */
	  return simplify_giv_expr (gen_rtx (MULT, mode,
					     XEXP (arg0, 0),
					     gen_rtx (MULT, mode,
						      XEXP (arg0, 1), arg1)),
				    benefit);

	case PLUS:
	  /* (a + invar_1) * invar_2.  Distribute.  */
	  return simplify_giv_expr (gen_rtx (PLUS, mode,
					     gen_rtx (MULT, mode,
						      XEXP (arg0, 0), arg1),
					     gen_rtx (MULT, mode,
						      XEXP (arg0, 1), arg1)),
				    benefit);

	default:
	  abort ();
	}

    case ASHIFT:
      /* Shift by constant is multiply by power of two.  */
      if (GET_CODE (XEXP (x, 1)) != CONST_INT)
	return 0;

      return simplify_giv_expr (gen_rtx (MULT, mode,
					 XEXP (x, 0),
					 GEN_INT ((HOST_WIDE_INT) 1
						  << INTVAL (XEXP (x, 1)))),
				benefit);

    case NEG:
      /* "-a" is "a * (-1)" */
      return simplify_giv_expr (gen_rtx (MULT, mode, XEXP (x, 0), constm1_rtx),
				benefit);

    case NOT:
      /* "~a" is "-a - 1". Silly, but easy.  */
      return simplify_giv_expr (gen_rtx (MINUS, mode,
					 gen_rtx (NEG, mode, XEXP (x, 0)),
					 const1_rtx),
				benefit);

    case USE:
      /* Already in proper form for invariant.  */
      return x;

    case REG:
      /* If this is a new register, we can't deal with it.  */
      if (REGNO (x) >= max_reg_before_loop)
	return 0;

      /* Check for biv or giv.  */
      switch (reg_iv_type[REGNO (x)])
	{
	case BASIC_INDUCT:
	  return x;
	case GENERAL_INDUCT:
	  {
	    struct induction *v = reg_iv_info[REGNO (x)];

	    /* Form expression from giv and add benefit.  Ensure this giv
	       can derive another and subtract any needed adjustment if so.  */
	    *benefit += v->benefit;
	    if (v->cant_derive)
	      return 0;

	    tem = gen_rtx (PLUS, mode, gen_rtx (MULT, mode,
						v->src_reg, v->mult_val),
			   v->add_val);
	    if (v->derive_adjustment)
	      tem = gen_rtx (MINUS, mode, tem, v->derive_adjustment);
	    return simplify_giv_expr (tem, benefit);
	  }
	}

      /* Fall through to general case.  */
    default:
      /* If invariant, return as USE (unless CONST_INT).
	 Otherwise, not giv.  */
      if (GET_CODE (x) == USE)
	x = XEXP (x, 0);

      if (invariant_p (x) == 1)
	{
	  if (GET_CODE (x) == CONST_INT)
	    return x;
	  else
	    return gen_rtx (USE, mode, x);
	}
      else
	return 0;
    }
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
consec_sets_giv (first_benefit, p, src_reg, dest_reg,
		 add_val, mult_val)
     int first_benefit;
     rtx p;
     rtx src_reg;
     rtx dest_reg;
     rtx *add_val;
     rtx *mult_val;
{
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
  struct induction *v
    = (struct induction *) alloca (sizeof (struct induction));
  v->src_reg = src_reg;
  v->mult_val = *mult_val;
  v->add_val = *add_val;
  v->benefit = first_benefit;
  v->cant_derive = 0;
  v->derive_adjustment = 0;

  reg_iv_type[REGNO (dest_reg)] = GENERAL_INDUCT;
  reg_iv_info[REGNO (dest_reg)] = v;

  count = n_times_set[REGNO (dest_reg)] - 1;

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
	  && ((benefit = general_induction_var (SET_SRC (set), &src_reg,
						add_val, mult_val))
	      /* Giv created by equivalent expression.  */
	      || ((temp = find_reg_note (p, REG_EQUAL, NULL_RTX))
		  && (benefit = general_induction_var (XEXP (temp, 0), &src_reg,
						       add_val, mult_val))))
	  && src_reg == v->src_reg)
	{
	  if (find_reg_note (p, REG_RETVAL, NULL_RTX))
	    benefit += libcall_benefit (p);

	  count--;
	  v->mult_val = *mult_val;
	  v->add_val = *add_val;
	  v->benefit = benefit;
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

	  reg_iv_type[REGNO (dest_reg)] = UNKNOWN_INDUCT;
	  return 0;
	}
    }

  return v->benefit;
}

/* Return an rtx, if any, that expresses giv G2 as a function of the register
   represented by G1.  If no such expression can be found, or it is clear that
   it cannot possibly be a valid address, 0 is returned. 

   To perform the computation, we note that
   	G1 = a * v + b		and
	G2 = c * v + d
   where `v' is the biv.

   So G2 = (c/a) * G1 + (d - b*c/a)  */

#ifdef ADDRESS_COST
static rtx
express_from (g1, g2)
     struct induction *g1, *g2;
{
  rtx mult, add;

  /* The value that G1 will be multiplied by must be a constant integer.  Also,
     the only chance we have of getting a valid address is if b*c/a (see above
     for notation) is also an integer.  */
  if (GET_CODE (g1->mult_val) != CONST_INT
      || GET_CODE (g2->mult_val) != CONST_INT
      || GET_CODE (g1->add_val) != CONST_INT
      || g1->mult_val == const0_rtx
      || INTVAL (g2->mult_val) % INTVAL (g1->mult_val) != 0)
    return 0;

  mult = GEN_INT (INTVAL (g2->mult_val) / INTVAL (g1->mult_val));
  add = plus_constant (g2->add_val, - INTVAL (g1->add_val) * INTVAL (mult));

  /* Form simplified final result.  */
  if (mult == const0_rtx)
    return add;
  else if (mult == const1_rtx)
    mult = g1->dest_reg;
  else
    mult = gen_rtx (MULT, g2->mode, g1->dest_reg, mult);

  if (add == const0_rtx)
    return mult;
  else
    return gen_rtx (PLUS, g2->mode, mult, add);
}
#endif

/* Return 1 if giv G2 can be combined with G1.  This means that G2 can use
   (either directly or via an address expression) a register used to represent
   G1.  Set g2->new_reg to a represtation of G1 (normally just
   g1->dest_reg).  */

static int
combine_givs_p (g1, g2)
     struct induction *g1, *g2;
{
  rtx tem;

  /* If these givs are identical, they can be combined.  */
  if (rtx_equal_p (g1->mult_val, g2->mult_val)
      && rtx_equal_p (g1->add_val, g2->add_val))
    {
      g2->new_reg = g1->dest_reg;
      return 1;
    }

#ifdef ADDRESS_COST
  /* If G2 can be expressed as a function of G1 and that function is valid
     as an address and no more expensive than using a register for G2,
     the expression of G2 in terms of G1 can be used.  */
  if (g2->giv_type == DEST_ADDR
      && (tem = express_from (g1, g2)) != 0
      && memory_address_p (g2->mem_mode, tem)
      && ADDRESS_COST (tem) <= ADDRESS_COST (*g2->location))
    {
      g2->new_reg = tem;
      return 1;
    }
#endif

  return 0;
}

/* Check all pairs of givs for iv_class BL and see if any can be combined with
   any other.  If so, point SAME to the giv combined with and set NEW_REG to
   be an expression (in terms of the other giv's DEST_REG) equivalent to the
   giv.  Also, update BENEFIT and related fields for cost/benefit analysis.  */

static void
combine_givs (bl)
     struct iv_class *bl;
{
  struct induction *g1, *g2;
  int pass;

  for (g1 = bl->giv; g1; g1 = g1->next_iv)
    for (pass = 0; pass <= 1; pass++)
      for (g2 = bl->giv; g2; g2 = g2->next_iv)
	if (g1 != g2
	    /* First try to combine with replaceable givs, then all givs. */
	    && (g1->replaceable || pass == 1)
	    /* If either has already been combined or is to be ignored, can't
	       combine.  */
	    && ! g1->ignore && ! g2->ignore && ! g1->same && ! g2->same
	    /* If something has been based on G2, G2 cannot itself be based
	       on something else.  */
	    && ! g2->combined_with
	    && combine_givs_p (g1, g2))
	  {
	    /* g2->new_reg set by `combine_givs_p'  */
	    g2->same = g1;
	    g1->combined_with = 1;
	    g1->benefit += g2->benefit;
	    /* ??? The new final_[bg]iv_value code does a much better job
	       of finding replaceable giv's, and hence this code may no
	       longer be necessary.  */
	    if (! g2->replaceable && REG_USERVAR_P (g2->dest_reg))
	      g1->benefit -= copy_cost;
	    g1->lifetime += g2->lifetime;
	    g1->times_used += g2->times_used;

	    if (loop_dump_stream)
	      fprintf (loop_dump_stream, "giv at %d combined with giv at %d\n",
		       INSN_UID (g2->insn), INSN_UID (g1->insn));
	  }
}

/* EMIT code before INSERT_BEFORE to set REG = B * M + A.  */

void
emit_iv_add_mult (b, m, a, reg, insert_before)
     rtx b;          /* initial value of basic induction variable */
     rtx m;          /* multiplicative constant */
     rtx a;          /* additive constant */
     rtx reg;        /* destination register */
     rtx insert_before;
{
  rtx seq;
  rtx result;

  /* Prevent unexpected sharing of these rtx.  */
  a = copy_rtx (a);
  b = copy_rtx (b);

  /* Increase the lifetime of any invariants moved further in code. */
  update_reg_last_use (a, insert_before);
  update_reg_last_use (b, insert_before);
  update_reg_last_use (m, insert_before);

  start_sequence ();
  result = expand_mult_add (b, reg, m, a, GET_MODE (reg), 0);
  if (reg != result)
    emit_move_insn (reg, result);
  seq = gen_sequence ();
  end_sequence ();

  emit_insn_before (seq, insert_before);
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
  struct obstack *old_rtl_obstack = rtl_obstack;
  char *storage = (char *) obstack_alloc (&temp_obstack, 0);
  int win = 1;

  /* If only one is constant, make it B. */
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

  rtl_obstack = &temp_obstack;
  start_sequence ();
  expand_mult (GET_MODE (a), a, b, NULL_RTX, 0);
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

  /* Free any storage we obtained in generating this multiply and restore rtl
     allocation to its normal obstack.  */
  obstack_free (&temp_obstack, storage);
  rtl_obstack = old_rtl_obstack;

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
check_dbra_loop (loop_end, insn_count, loop_start)
     rtx loop_end;
     int insn_count;
     rtx loop_start;
{
  struct iv_class *bl;
  rtx reg;
  rtx jump_label;
  rtx final_value;
  rtx start_value;
  rtx new_add_val;
  rtx comparison;
  rtx before_comparison;
  rtx p;

  /* If last insn is a conditional branch, and the insn before tests a
     register value, try to optimize it.  Otherwise, we can't do anything.  */

  comparison = get_condition_for_loop (PREV_INSN (loop_end));
  if (comparison == 0)
    return 0;

  /* Check all of the bivs to see if the compare uses one of them.
     Skip biv's set more than once because we can't guarantee that
     it will be zero on the last iteration.  Also skip if the biv is
     used between its update and the test insn.  */

  for (bl = loop_iv_list; bl; bl = bl->next)
    {
      if (bl->biv_count == 1
	  && bl->biv->dest_reg == XEXP (comparison, 0)
	  && ! reg_used_between_p (regno_reg_rtx[bl->regno], bl->biv->insn,
				   PREV_INSN (PREV_INSN (loop_end))))
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
	  && (INTVAL (bl->initial_value) %
	      (-INTVAL (bl->biv->add_val))) == 0)
	{
	  /* register always nonnegative, add REG_NOTE to branch */
	  REG_NOTES (PREV_INSN (loop_end))
	    = gen_rtx (EXPR_LIST, REG_NONNEG, NULL_RTX,
		       REG_NOTES (PREV_INSN (loop_end)));
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

	  before_comparison = get_condition_for_loop (p);
	  if (before_comparison
	      && XEXP (before_comparison, 0) == bl->biv->dest_reg
	      && GET_CODE (before_comparison) == LT
	      && XEXP (before_comparison, 1) == const0_rtx
	      && ! reg_set_between_p (bl->biv->dest_reg, p, loop_start)
	      && INTVAL (bl->biv->add_val) == -1)
	    {
	      REG_NOTES (PREV_INSN (loop_end))
		= gen_rtx (EXPR_LIST, REG_NONNEG, NULL_RTX,
			   REG_NOTES (PREV_INSN (loop_end)));
	      bl->nonneg = 1;

	      return 1;
	    }
	}
    }
  else if (num_mem_sets <= 1)
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

      for (p = loop_start; p != loop_end; p = NEXT_INSN (p))
	if (GET_RTX_CLASS (GET_CODE (p)) == 'i')
	  num_nonfixed_reads += count_nonfixed_reads (PATTERN (p));

      if (bl->giv_count == 0
	  && ! loop_number_exit_count[uid_loop_num[INSN_UID (loop_start)]])
	{
	  rtx bivreg = regno_reg_rtx[bl->regno];

	  /* If there are no givs for this biv, and the only exit is the
	     fall through at the end of the the loop, then
	     see if perhaps there are no uses except to count.  */
	  no_use_except_counting = 1;
	  for (p = loop_start; p != loop_end; p = NEXT_INSN (p))
	    if (GET_RTX_CLASS (GET_CODE (p)) == 'i')
	      {
		rtx set = single_set (p);

		if (set && GET_CODE (SET_DEST (set)) == REG
		    && REGNO (SET_DEST (set)) == bl->regno)
		  /* An insn that sets the biv is okay.  */
		  ;
		else if (p == prev_nonnote_insn (prev_nonnote_insn (loop_end))
			 || p == prev_nonnote_insn (loop_end))
		  /* Don't bother about the end test.  */
		  ;
		else if (reg_mentioned_p (bivreg, PATTERN (p)))
		  /* Any other use of the biv is no good.  */
		  {
		    no_use_except_counting = 0;
		    break;
		  }
	      }
	}

      /* If the loop has a single store, and the destination address is
	 invariant, then we can't reverse the loop, because this address
	 might then have the wrong value at loop exit.
	 This would work if the source was invariant also, however, in that
	 case, the insn should have been moved out of the loop.  */

      if (num_mem_sets == 1)
	reversible_mem_store
	  = (! unknown_address_altered
	     && ! invariant_p (XEXP (loop_store_mems[0], 0)));

      /* This code only acts for innermost loops.  Also it simplifies
	 the memory address check by only reversing loops with
	 zero or one memory access.
	 Two memory accesses could involve parts of the same array,
	 and that can't be reversed.  */

      if (num_nonfixed_reads <= 1
	  && !loop_has_call
	  && !loop_has_volatile
	  && reversible_mem_store
	  && (no_use_except_counting
	      || ((bl->giv_count + bl->biv_count + num_mem_sets
		   + num_movables + 2 == insn_count)
		  && (bl == loop_iv_list && bl->next == 0))))
	{
	  rtx tem;

	  /* Loop can be reversed.  */
	  if (loop_dump_stream)
	    fprintf (loop_dump_stream, "Can reverse loop\n");

	  /* Now check other conditions:
	     initial_value must be zero,
	     final_value % add_val == 0, so that when reversed, the
	     biv will be zero on the last iteration.

	     This test can probably be improved since +/- 1 in the constant
	     can be obtained by changing LT to LE and vice versa; this is
	     confusing.  */

	  if (comparison && bl->initial_value == const0_rtx
	      && GET_CODE (XEXP (comparison, 1)) == CONST_INT
	      /* LE gets turned into LT */
	      && GET_CODE (comparison) == LT
	      && (INTVAL (XEXP (comparison, 1))
		  % INTVAL (bl->biv->add_val)) == 0)
	    {
	      /* Register will always be nonnegative, with value
		 0 on last iteration if loop reversed */

	      /* Save some info needed to produce the new insns.  */
	      reg = bl->biv->dest_reg;
	      jump_label = XEXP (SET_SRC (PATTERN (PREV_INSN (loop_end))), 1);
	      new_add_val = GEN_INT (- INTVAL (bl->biv->add_val));

	      final_value = XEXP (comparison, 1);
	      start_value = GEN_INT (INTVAL (XEXP (comparison, 1))
				     - INTVAL (bl->biv->add_val));

	      /* Initialize biv to start_value before loop start.
		 The old initializing insn will be deleted as a
		 dead store by flow.c.  */
	      emit_insn_before (gen_move_insn (reg, start_value), loop_start);

	      /* Add insn to decrement register, and delete insn
		 that incremented the register.  */
	      p = emit_insn_before (gen_add2_insn (reg, new_add_val),
				    bl->biv->insn);
	      delete_insn (bl->biv->insn);
		      
	      /* Update biv info to reflect its new status.  */
	      bl->biv->insn = p;
	      bl->initial_value = start_value;
	      bl->biv->add_val = new_add_val;

	      /* Inc LABEL_NUSES so that delete_insn will
		 not delete the label.  */
	      LABEL_NUSES (XEXP (jump_label, 0)) ++;

	      /* Emit an insn after the end of the loop to set the biv's
		 proper exit value if it is used anywhere outside the loop.  */
	      if ((regno_last_uid[bl->regno]
		   != INSN_UID (PREV_INSN (PREV_INSN (loop_end))))
		  || ! bl->init_insn
		  || regno_first_uid[bl->regno] != INSN_UID (bl->init_insn))
		emit_insn_after (gen_move_insn (reg, final_value),
				 loop_end);

	      /* Delete compare/branch at end of loop.  */
	      delete_insn (PREV_INSN (loop_end));
	      delete_insn (PREV_INSN (loop_end));

	      /* Add new compare/branch insn at end of loop.  */
	      start_sequence ();
	      emit_cmp_insn (reg, const0_rtx, GE, NULL_RTX,
			     GET_MODE (reg), 0, 0);
	      emit_jump_insn (gen_bge (XEXP (jump_label, 0)));
	      tem = gen_sequence ();
	      end_sequence ();
	      emit_jump_insn_before (tem, loop_end);

	      for (tem = PREV_INSN (loop_end);
		   tem && GET_CODE (tem) != JUMP_INSN; tem = PREV_INSN (tem))
		;
	      if (tem)
		{
		  JUMP_LABEL (tem) = XEXP (jump_label, 0);

		  /* Increment of LABEL_NUSES done above. */
		  /* Register is now always nonnegative,
		     so add REG_NONNEG note to the branch.  */
		  REG_NOTES (tem) = gen_rtx (EXPR_LIST, REG_NONNEG, NULL_RTX,
					     REG_NOTES (tem));
		}

	      bl->nonneg = 1;

	      /* Mark that this biv has been reversed.  Each giv which depends
		 on this biv, and which is also live past the end of the loop
		 will have to be fixed up.  */

	      bl->reversed = 1;

	      if (loop_dump_stream)
		fprintf (loop_dump_stream,
			 "Reversed loop and added reg_nonneg\n");

	      return 1;
	    }
	}
    }

  return 0;
}

/* Verify whether the biv BL appears to be eliminable,
   based on the insns in the loop that refer to it.
   LOOP_START is the first insn of the loop, and END is the end insn.

   If ELIMINATE_P is non-zero, actually do the elimination.

   THRESHOLD and INSN_COUNT are from loop_optimize and are used to
   determine whether invariant insns should be placed inside or at the
   start of the loop.  */

static int
maybe_eliminate_biv (bl, loop_start, end, eliminate_p, threshold, insn_count)
     struct iv_class *bl;
     rtx loop_start;
     rtx end;
     int eliminate_p;
     int threshold, insn_count;
{
  rtx reg = bl->biv->dest_reg;
  rtx p;

  /* Scan all insns in the loop, stopping if we find one that uses the
     biv in a way that we cannot eliminate.  */

  for (p = loop_start; p != end; p = NEXT_INSN (p))
    {
      enum rtx_code code = GET_CODE (p);
      rtx where = threshold >= insn_count ? loop_start : p;

      if ((code == INSN || code == JUMP_INSN || code == CALL_INSN)
	  && reg_mentioned_p (reg, PATTERN (p))
	  && ! maybe_eliminate_biv_1 (PATTERN (p), p, bl, eliminate_p, where))
	{
	  if (loop_dump_stream)
	    fprintf (loop_dump_stream,
		     "Cannot eliminate biv %d: biv used in insn %d.\n",
		     bl->regno, INSN_UID (p));
	  break;
	}
    }

  if (p == end)
    {
      if (loop_dump_stream)
	fprintf (loop_dump_stream, "biv %d %s eliminated.\n",
		 bl->regno, eliminate_p ? "was" : "can be");
      return 1;
    }

  return 0;
}

/* If BL appears in X (part of the pattern of INSN), see if we can
   eliminate its use.  If so, return 1.  If not, return 0.

   If BIV does not appear in X, return 1.

   If ELIMINATE_P is non-zero, actually do the elimination.  WHERE indicates
   where extra insns should be added.  Depending on how many items have been
   moved out of the loop, it will either be before INSN or at the start of
   the loop.  */

static int
maybe_eliminate_biv_1 (x, insn, bl, eliminate_p, where)
     rtx x, insn;
     struct iv_class *bl;
     int eliminate_p;
     rtx where;
{
  enum rtx_code code = GET_CODE (x);
  rtx reg = bl->biv->dest_reg;
  enum machine_mode mode = GET_MODE (reg);
  struct induction *v;
  rtx arg, new, tem;
  int arg_operand;
  char *fmt;
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
	     Require a constant for MULT_VAL, so we know it's nonzero.  */

	  for (v = bl->giv; v; v = v->next_iv)
	    if (CONSTANT_P (v->mult_val) && v->mult_val != const0_rtx
		&& v->add_val == const0_rtx
		&& ! v->ignore && ! v->maybe_dead && v->always_computable
		&& v->mode == mode)
	      {
		if (! eliminate_p)
		  return 1;

		/* If the giv has the opposite direction of change,
		   then reverse the comparison.  */
		if (INTVAL (v->mult_val) < 0)
		  new = gen_rtx (COMPARE, GET_MODE (v->new_reg),
				 const0_rtx, v->new_reg);
		else
		  new = v->new_reg;

		/* We can probably test that giv's reduced reg.  */
		if (validate_change (insn, &SET_SRC (x), new, 0))
		  return 1;
	      }

	  /* Look for a giv with (MULT_VAL != 0) and (ADD_VAL != 0);
	     replace test insn with a compare insn (cmp REDUCED_GIV ADD_VAL).
	     Require a constant for MULT_VAL, so we know it's nonzero.  */

	  for (v = bl->giv; v; v = v->next_iv)
	    if (CONSTANT_P (v->mult_val) && v->mult_val != const0_rtx
		&& ! v->ignore && ! v->maybe_dead && v->always_computable
		&& v->mode == mode)
	      {
		if (! eliminate_p)
		  return 1;

		/* If the giv has the opposite direction of change,
		   then reverse the comparison.  */
		if (INTVAL (v->mult_val) < 0)
		  new = gen_rtx (COMPARE, VOIDmode, copy_rtx (v->add_val),
				 v->new_reg);
		else
		  new = gen_rtx (COMPARE, VOIDmode, v->new_reg,
				 copy_rtx (v->add_val));

		/* Replace biv with the giv's reduced register.  */
		update_reg_last_use (v->add_val, insn);
		if (validate_change (insn, &SET_SRC (PATTERN (insn)), new, 0))
		  return 1;

		/* Insn doesn't support that constant or invariant.  Copy it
		   into a register (it will be a loop invariant.)  */
		tem = gen_reg_rtx (GET_MODE (v->new_reg));

		emit_insn_before (gen_move_insn (tem, copy_rtx (v->add_val)),
				  where);

		if (validate_change (insn, &SET_SRC (PATTERN (insn)),
				     gen_rtx (COMPARE, VOIDmode,
					      v->new_reg, tem), 0))
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
	    if (CONSTANT_P (v->mult_val) && INTVAL (v->mult_val) > 0
		&& CONSTANT_P (v->add_val)
		&& ! v->ignore && ! v->maybe_dead && v->always_computable
		&& v->mode == mode)
	      {
		if (! eliminate_p)
		  return 1;

		/* Replace biv with the giv's reduced reg.  */
		XEXP (x, 1-arg_operand) = v->new_reg;

		/* If all constants are actually constant integers and
		   the derived constant can be directly placed in the COMPARE,
		   do so.  */
		if (GET_CODE (arg) == CONST_INT
		    && GET_CODE (v->mult_val) == CONST_INT
		    && GET_CODE (v->add_val) == CONST_INT
		    && validate_change (insn, &XEXP (x, arg_operand),
					GEN_INT (INTVAL (arg)
						 * INTVAL (v->mult_val)
						 + INTVAL (v->add_val)), 0))
		  return 1;

		/* Otherwise, load it into a register.  */
		tem = gen_reg_rtx (mode);
		emit_iv_add_mult (arg, v->mult_val, v->add_val, tem, where);
		if (validate_change (insn, &XEXP (x, arg_operand), tem, 0))
		  return 1;

		/* If that failed, put back the change we made above.  */
		XEXP (x, 1-arg_operand) = reg;
	      }
	  
	  /* Look for giv with positive constant mult_val and nonconst add_val.
	     Insert insns to calculate new compare value.  */

	  for (v = bl->giv; v; v = v->next_iv)
	    if (CONSTANT_P (v->mult_val) && INTVAL (v->mult_val) > 0
		&& ! v->ignore && ! v->maybe_dead && v->always_computable
		&& v->mode == mode)
	      {
		rtx tem;

		if (! eliminate_p)
		  return 1;

		tem = gen_reg_rtx (mode);

		/* Replace biv with giv's reduced register.  */
		validate_change (insn, &XEXP (x, 1 - arg_operand),
				 v->new_reg, 1);

		/* Compute value to compare against.  */
		emit_iv_add_mult (arg, v->mult_val, v->add_val, tem, where);
		/* Use it in this insn.  */
		validate_change (insn, &XEXP (x, arg_operand), tem, 1);
		if (apply_change_group ())
		  return 1;
	      }
	}
      else if (GET_CODE (arg) == REG || GET_CODE (arg) == MEM)
	{
	  if (invariant_p (arg) == 1)
	    {
	      /* Look for giv with constant positive mult_val and nonconst
		 add_val. Insert insns to compute new compare value.  */

	      for (v = bl->giv; v; v = v->next_iv)
		if (CONSTANT_P (v->mult_val) && INTVAL (v->mult_val) > 0
		    && ! v->ignore && ! v->maybe_dead && v->always_computable
		    && v->mode == mode)
		  {
		    rtx tem;

		    if (! eliminate_p)
		      return 1;

		    tem = gen_reg_rtx (mode);

		    /* Replace biv with giv's reduced register.  */
		    validate_change (insn, &XEXP (x, 1 - arg_operand),
				     v->new_reg, 1);

		    /* Compute value to compare against.  */
		    emit_iv_add_mult (arg, v->mult_val, v->add_val,
				      tem, where);
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
	      || reg_iv_type[REGNO (arg)] != BASIC_INDUCT)
	    return 0;

	  /* Look for a pair of givs, one for each biv,
	     with identical coefficients.  */
	  for (v = bl->giv; v; v = v->next_iv)
	    {
	      struct induction *tv;

	      if (v->ignore || v->maybe_dead || v->mode != mode)
		continue;

	      for (tv = reg_biv_class[REGNO (arg)]->giv; tv; tv = tv->next_iv)
		if (! tv->ignore && ! tv->maybe_dead
		    && rtx_equal_p (tv->mult_val, v->mult_val)
		    && rtx_equal_p (tv->add_val, v->add_val)
		    && tv->mode == mode)
		  {
		    if (! eliminate_p)
		      return 1;

		    /* Replace biv with its giv's reduced reg.  */
		    XEXP (x, 1-arg_operand) = v->new_reg;
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
    }

  /* See if any subexpression fails elimination.  */
  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      switch (fmt[i])
	{
	case 'e':
	  if (! maybe_eliminate_biv_1 (XEXP (x, i), insn, bl, 
				       eliminate_p, where))
	    return 0;
	  break;

	case 'E':
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (! maybe_eliminate_biv_1 (XVECEXP (x, i, j), insn, bl,
					 eliminate_p, where))
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
      if (regno_last_uid[REGNO (reg)] == INSN_UID (n))
	return 1;
    }
  return 0;
}

/* Called via `note_stores' to record the initial value of a biv.  Here we
   just record the location of the set and process it later.  */

static void
record_initial (dest, set)
     rtx dest;
     rtx set;
{
  struct iv_class *bl;

  if (GET_CODE (dest) != REG
      || REGNO (dest) >= max_reg_before_loop
      || reg_iv_type[REGNO (dest)] != BASIC_INDUCT)
    return;

  bl = reg_biv_class[REGNO (dest)];

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
   use it.  */

static void
update_reg_last_use (x, insn)
     rtx x;
     rtx insn;
{
  /* Check for the case where INSN does not have a valid luid.  In this case,
     there is no need to modify the regno_last_uid, as this can only happen
     when code is inserted after the loop_end to set a pseudo's final value,
     and hence this insn will never be the last use of x.  */
  if (GET_CODE (x) == REG && REGNO (x) < max_reg_before_loop
      && INSN_UID (insn) < max_uid_for_loop
      && uid_luid[regno_last_uid[REGNO (x)]] < uid_luid[INSN_UID (insn)])
    regno_last_uid[REGNO (x)] = INSN_UID (insn);
  else
    {
      register int i, j;
      register char *fmt = GET_RTX_FORMAT (GET_CODE (x));
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

/* Given a jump insn JUMP, return the condition that will cause it to branch
   to its JUMP_LABEL.  If the condition cannot be understood, or is an
   inequality floating-point comparison which needs to be reversed, 0 will
   be returned.

   If EARLIEST is non-zero, it is a pointer to a place where the earliest
   insn used in locating the condition was found.  If a replacement test
   of the condition is desired, it should be placed in front of that
   insn and we will be sure that the inputs are still valid.

   The condition will be returned in a canonical form to simplify testing by
   callers.  Specifically:

   (1) The code will always be a comparison operation (EQ, NE, GT, etc.).
   (2) Both operands will be machine operands; (cc0) will have been replaced.
   (3) If an operand is a constant, it will be the second operand.
   (4) (LE x const) will be replaced with (LT x <const+1>) and similarly
       for GE, GEU, and LEU.  */

rtx
get_condition (jump, earliest)
     rtx jump;
     rtx *earliest;
{
  enum rtx_code code;
  rtx prev = jump;
  rtx set;
  rtx tem;
  rtx op0, op1;
  int reverse_code = 0;
  int did_reverse_condition = 0;

  /* If this is not a standard conditional jump, we can't parse it.  */
  if (GET_CODE (jump) != JUMP_INSN
      || ! condjump_p (jump) || simplejump_p (jump))
    return 0;

  code = GET_CODE (XEXP (SET_SRC (PATTERN (jump)), 0));
  op0 = XEXP (XEXP (SET_SRC (PATTERN (jump)), 0), 0);
  op1 = XEXP (XEXP (SET_SRC (PATTERN (jump)), 0), 1);

  if (earliest)
    *earliest = jump;

  /* If this branches to JUMP_LABEL when the condition is false, reverse
     the condition.  */
  if (GET_CODE (XEXP (SET_SRC (PATTERN (jump)), 2)) == LABEL_REF
      && XEXP (XEXP (SET_SRC (PATTERN (jump)), 2), 0) == JUMP_LABEL (jump))
    code = reverse_condition (code), did_reverse_condition ^= 1;

  /* If we are comparing a register with zero, see if the register is set
     in the previous insn to a COMPARE or a comparison operation.  Perform
     the same tests as a function of STORE_FLAG_VALUE as find_comparison_args
     in cse.c  */

  while (GET_RTX_CLASS (code) == '<' && op1 == CONST0_RTX (GET_MODE (op0)))
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
	  || FIND_REG_INC_NOTE (prev, 0)
	  || (set = single_set (prev)) == 0)
	break;

      /* If this is setting OP0, get what it sets it to if it looks
	 relevant.  */
      if (SET_DEST (set) == op0)
	{
	  enum machine_mode inner_mode = GET_MODE (SET_SRC (set));

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
			 && FLOAT_STORE_FLAG_VALUE < 0)
#endif
		     ))
		   && GET_RTX_CLASS (GET_CODE (SET_SRC (set))) == '<')))
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
			 && FLOAT_STORE_FLAG_VALUE < 0)
#endif
		     ))
		   && GET_RTX_CLASS (GET_CODE (SET_SRC (set))) == '<')
	    {
	      /* We might have reversed a LT to get a GE here.  But this wasn't
		 actually the comparison of data, so we don't flag that we
		 have had to reverse the condition.  */
	      did_reverse_condition ^= 1;
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
	      code = reverse_condition (code);
	      did_reverse_condition ^= 1;
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
	  if (const_val != max_val >> 1)
	    code = LT,	op1 = GEN_INT (const_val + 1);
	  break;

	case GE:
	  if (const_val
	      != (((HOST_WIDE_INT) 1
		   << (GET_MODE_BITSIZE (GET_MODE (op0)) - 1))))
	    code = GT, op1 = GEN_INT (const_val - 1);
	  break;

	case LEU:
	  if (uconst_val != max_val)
	    code = LTU, op1 = GEN_INT (uconst_val + 1);
	  break;

	case GEU:
	  if (uconst_val != 0)
	    code = GTU, op1 = GEN_INT (uconst_val - 1);
	  break;
	}
    }

  /* If this was floating-point and we reversed anything other than an
     EQ or NE, return zero.  */
  if (TARGET_FLOAT_FORMAT == IEEE_FLOAT_FORMAT
      && did_reverse_condition && code != NE && code != EQ
      && ! flag_fast_math
      && GET_MODE_CLASS (GET_MODE (op0)) == MODE_FLOAT)
    return 0;

#ifdef HAVE_cc0
  /* Never return CC0; return zero instead.  */
  if (op0 == cc0_rtx)
    return 0;
#endif

  return gen_rtx (code, VOIDmode, op0, op1);
}

/* Similar to above routine, except that we also put an invariant last
   unless both operands are invariants.  */

rtx
get_condition_for_loop (x)
     rtx x;
{
  rtx comparison = get_condition (x, NULL_PTR);

  if (comparison == 0
      || ! invariant_p (XEXP (comparison, 0))
      || invariant_p (XEXP (comparison, 1)))
    return comparison;

  return gen_rtx (swap_condition (GET_CODE (comparison)), VOIDmode,
		  XEXP (comparison, 1), XEXP (comparison, 0));
}
