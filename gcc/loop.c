/* Perform various loop optimizations, including strength reduction.
   Copyright (C) 1987, 88, 89, 91-99, 2000 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tm_p.h"
#include "obstack.h"
#include "function.h"
#include "expr.h"
#include "basic-block.h"
#include "insn-config.h"
#include "insn-flags.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "recog.h"
#include "flags.h"
#include "real.h"
#include "loop.h"
#include "except.h"
#include "toplev.h"

/* Information about the current loop being processed used to compute
   the number of loop iterations for loop unrolling and doloop
   optimization.  */
static struct loop_info *current_loop_info;

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

static varray_type set_in_loop;

/* Original value of set_in_loop; same except that this value
   is not set negative for a reg whose sets have been made candidates
   and not set to 0 for a reg that is moved.  */

static varray_type n_times_set;

/* Index by register number, 1 indicates that the register
   cannot be moved or strength reduced.  */

static varray_type may_not_optimize;

/* Contains the insn in which a register was used if it was used
   exactly once; contains const0_rtx if it was used more than once.  */

static varray_type reg_single_usage;

/* Nonzero means reg N has already been moved out of one loop.
   This reduces the desire to move it out of another.  */

static char *moved_once;

/* List of MEMs that are stored in this loop.  */

static rtx loop_store_mems;

/* The insn where the first of these was found.  */
static rtx first_loop_store_insn;

typedef struct loop_mem_info {
  rtx mem;      /* The MEM itself.  */
  rtx reg;      /* Corresponding pseudo, if any.  */
  int optimize; /* Nonzero if we can optimize access to this MEM.  */
} loop_mem_info;

/* Array of MEMs that are used (read or written) in this loop, but
   cannot be aliased by anything in this loop, except perhaps
   themselves.  In other words, if loop_mems[i] is altered during the
   loop, it is altered by an expression that is rtx_equal_p to it.  */

static loop_mem_info *loop_mems;

/* The index of the next available slot in LOOP_MEMS.  */

static int loop_mems_idx;

/* The number of elements allocated in LOOP_MEMs.  */

static int loop_mems_allocated;

/* Nonzero if we don't know what MEMs were changed in the current
   loop.  This happens if the loop contains a call (in which case
   `loop_info->has_call' will also be set) or if we store into more
   than NUM_STORES MEMs.  */

static int unknown_address_altered;

/* The above doesn't count any readonly memory locations that are stored.
   This does.  */

static int unknown_constant_address_altered;

/* Count of movable (i.e. invariant) instructions discovered in the loop.  */
static int num_movables;

/* Count of memory write instructions discovered in the loop.  */
static int num_mem_sets;

/* Bound on pseudo register number before loop optimization.
   A pseudo has valid regscan info if its number is < max_reg_before_loop.  */
int max_reg_before_loop;

/* The value to pass to the next call of reg_scan_update.  */
static int loop_max_reg;

/* This obstack is used in product_cheap_p to allocate its rtl.  It
   may call gen_reg_rtx which, in turn, may reallocate regno_reg_rtx.
   If we used the same obstack that it did, we would be deallocating
   that array.  */

static struct obstack temp_obstack;

/* This is where the pointer to the obstack being used for RTL is stored.  */

extern struct obstack *rtl_obstack;

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

static struct movable *the_movables;

FILE *loop_dump_stream;

/* Forward declarations.  */

static void verify_dominator PARAMS ((struct loop *));
static void find_and_verify_loops PARAMS ((rtx, struct loops *));
static void mark_loop_jump PARAMS ((rtx, struct loop *));
static void prescan_loop PARAMS ((struct loop *));
static int reg_in_basic_block_p PARAMS ((rtx, rtx));
static int consec_sets_invariant_p PARAMS ((rtx, int, rtx));
static int labels_in_range_p PARAMS ((rtx, int));
static void count_one_set PARAMS ((rtx, rtx, varray_type, rtx *));

static void count_loop_regs_set PARAMS ((rtx, rtx, varray_type, varray_type,
				       int *, int)); 
static void note_addr_stored PARAMS ((rtx, rtx, void *));
static void note_set_pseudo_multiple_uses PARAMS ((rtx, rtx, void *));
static int loop_reg_used_before_p PARAMS ((const struct loop *, rtx, rtx));
static void scan_loop PARAMS ((struct loop*, int, int));
#if 0
static void replace_call_address PARAMS ((rtx, rtx, rtx));
#endif
static rtx skip_consec_insns PARAMS ((rtx, int));
static int libcall_benefit PARAMS ((rtx));
static void ignore_some_movables PARAMS ((struct movable *));
static void force_movables PARAMS ((struct movable *));
static void combine_movables PARAMS ((struct movable *, int));
static int regs_match_p PARAMS ((rtx, rtx, struct movable *));
static int rtx_equal_for_loop_p PARAMS ((rtx, rtx, struct movable *));
static void add_label_notes PARAMS ((rtx, rtx));
static void move_movables PARAMS ((struct movable *, int, int, rtx, rtx, int));
static int count_nonfixed_reads PARAMS ((rtx));
static void strength_reduce PARAMS ((struct loop *, int, int, int));
static void find_single_use_in_loop PARAMS ((rtx, rtx, varray_type));
static int valid_initial_value_p PARAMS ((rtx, rtx, int, rtx));
static void find_mem_givs PARAMS ((rtx, rtx, int, int, rtx, rtx));
static void record_biv PARAMS ((struct induction *, rtx, rtx, rtx, rtx, rtx *, int, int, int));
static void check_final_value PARAMS ((struct induction *, rtx, rtx, 
				     unsigned HOST_WIDE_INT));
static void record_giv PARAMS ((struct induction *, rtx, rtx, rtx, rtx, rtx, int, enum g_types, int, int, rtx *, rtx, rtx));
static void update_giv_derive PARAMS ((rtx));
static int basic_induction_var PARAMS ((rtx, enum machine_mode, rtx, rtx, int, rtx *, rtx *, rtx **, int *));
static rtx simplify_giv_expr PARAMS ((rtx, int *));
static int general_induction_var PARAMS ((rtx, rtx *, rtx *, rtx *, int, int *));
static int consec_sets_giv PARAMS ((int, rtx, rtx, rtx, rtx *, rtx *, rtx *));
static int check_dbra_loop PARAMS ((struct loop *, int));
static rtx express_from_1 PARAMS ((rtx, rtx, rtx));
static rtx combine_givs_p PARAMS ((struct induction *, struct induction *));
static void combine_givs PARAMS ((struct iv_class *));
struct recombine_givs_stats;
static int find_life_end PARAMS ((rtx, struct recombine_givs_stats *, rtx, rtx));
static void recombine_givs PARAMS ((struct iv_class *, rtx, rtx, int));
static int product_cheap_p PARAMS ((rtx, rtx));
static int maybe_eliminate_biv PARAMS ((struct iv_class *, rtx, rtx, int, int, int));
static int maybe_eliminate_biv_1 PARAMS ((rtx, rtx, struct iv_class *, int, rtx));
static int last_use_this_basic_block PARAMS ((rtx, rtx));
static void record_initial PARAMS ((rtx, rtx, void *));
static void update_reg_last_use PARAMS ((rtx, rtx));
static rtx next_insn_in_loop PARAMS ((const struct loop *, rtx));
static void load_mems_and_recount_loop_regs_set PARAMS ((const struct loop*,
						       int *));
static void load_mems PARAMS ((const struct loop *));
static int insert_loop_mem PARAMS ((rtx *, void *));
static int replace_loop_mem PARAMS ((rtx *, void *));
static int replace_loop_reg PARAMS ((rtx *, void *));
static void note_reg_stored PARAMS ((rtx, rtx, void *));
static void try_copy_prop PARAMS ((const struct loop *, rtx, int));
static int replace_label PARAMS ((rtx *, void *));

typedef struct rtx_and_int {
  rtx r;
  int i;
} rtx_and_int;

typedef struct rtx_pair {
  rtx r1;
  rtx r2;
} rtx_pair;

/* Nonzero iff INSN is between START and END, inclusive.  */
#define INSN_IN_RANGE_P(INSN, START, END) 	\
  (INSN_UID (INSN) < max_uid_for_loop 		\
   && INSN_LUID (INSN) >= INSN_LUID (START)	\
   && INSN_LUID (INSN) <= INSN_LUID (END))

#ifdef HAVE_decrement_and_branch_on_count
/* Test whether BCT applicable and safe.  */
static void insert_bct PARAMS ((struct loop *));

/* Auxiliary function that inserts the BCT pattern into the loop.  */
static void instrument_loop_bct PARAMS ((rtx, rtx, rtx));
#endif /* HAVE_decrement_and_branch_on_count */

/* Indirect_jump_in_function is computed once per function.  */
int indirect_jump_in_function = 0;
static int indirect_jump_in_function_p PARAMS ((rtx));

static int compute_luids PARAMS ((rtx, rtx, int));

static int biv_elimination_giv_has_0_offset PARAMS ((struct induction *,
						   struct induction *, rtx));

/* Relative gain of eliminating various kinds of operations.  */
static int add_cost;
#if 0
static int shift_cost;
static int mult_cost;
#endif

/* Benefit penalty, if a giv is not replaceable, i.e. must emit an insn to
   copy the value of the strength reduced giv to its original register.  */
static int copy_cost;

/* Cost of using a register, to normalize the benefits of a giv.  */
static int reg_address_cost;


void
init_loop ()
{
  char *free_point = (char *) oballoc (1);
  rtx reg = gen_rtx_REG (word_mode, LAST_VIRTUAL_REGISTER + 1);

  add_cost = rtx_cost (gen_rtx_PLUS (word_mode, reg, reg), SET);

#ifdef ADDRESS_COST
  reg_address_cost = ADDRESS_COST (reg);
#else
  reg_address_cost = rtx_cost (reg, MEM);
#endif

  /* We multiply by 2 to reconcile the difference in scale between
     these two ways of computing costs.  Otherwise the cost of a copy
     will be far less than the cost of an add.  */

  copy_cost = 2 * 2;

  /* Free the objects we just allocated.  */
  obfree (free_point);

  /* Initialize the obstack used for rtl in product_cheap_p.  */
  gcc_obstack_init (&temp_obstack);
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
loop_optimize (f, dumpfile, unroll_p, bct_p)
     /* f is the first instruction of a chain of insns for one function */
     rtx f;
     FILE *dumpfile;
     int unroll_p, bct_p;
{
  register rtx insn;
  register int i;
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

  moved_once = (char *) xcalloc (max_reg_before_loop, sizeof (char));

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
    loops->array[i].info = loops_info + i;

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

  /* If debugging and unrolling loops, we must replicate the tree
     nodes corresponding to the BLOCKs inside the loop, so that the
     original one to one mapping will remain.  We sometimes unroll
     loops even when unroll_p is false, so we must always do this when
     debugging.  */
  if (write_symbols != NO_DEBUG)
    find_loop_tree_blocks ();

  /* Determine if the function has indirect jump.  On some systems
     this prevents low overhead loop instructions from being used.  */
  indirect_jump_in_function = indirect_jump_in_function_p (f);

  /* Now scan the loops, last ones first, since this means inner ones are done
     before outer ones.  */
  for (i = max_loop_num - 1; i >= 0; i--)
    {
      struct loop *loop = &loops->array[i];

      if (! loop->invalid && loop->end)
	scan_loop (loop, unroll_p, bct_p);
    }

  /* Replicate the BLOCKs.  */
  if (write_symbols != NO_DEBUG)
    unroll_block_trees ();

  end_alias_analysis ();

  /* Clean up.  */
  free (moved_once);
  free (uid_luid);
  free (uid_loop);
  free (loops_info);
  free (loops->array);
}

/* Returns the next insn, in execution order, after INSN.  START and
   END are the NOTE_INSN_LOOP_BEG and NOTE_INSN_LOOP_END for the loop,
   respectively.  LOOP_TOP, if non-NULL, is the top of the loop in the
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
scan_loop (loop, unroll_p, bct_p)
     struct loop *loop;
     int unroll_p, bct_p;
{
  register int i;
  rtx loop_start = loop->start;
  rtx loop_end = loop->end;
  struct loop_info *loop_info = loop->info;
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
  int in_libcall = 0;
  int tem;
  rtx temp, update_start, update_end;
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
  /* Nonzero if we are scanning instructions in a sub-loop.  */
  int loop_depth = 0;
  int nregs;

  current_loop_info = loop_info;
  loop->top = 0;

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
	 && GET_CODE (p) != CODE_LABEL && GET_RTX_CLASS (GET_CODE (p)) != 'i'
	 && (GET_CODE (p) != NOTE
	     || (NOTE_LINE_NUMBER (p) != NOTE_INSN_LOOP_BEG
		 && NOTE_LINE_NUMBER (p) != NOTE_INSN_LOOP_END));
       p = NEXT_INSN (p))
    ;

  loop->scan_start = p;

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
      if (simplejump_p (p)
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

  /* Count number of times each reg is set during this loop.
     Set VARRAY_CHAR (may_not_optimize, I) if it is not safe to move out
     the setting of register I.  Set VARRAY_RTX (reg_single_usage, I).  */
  
  /* Allocate extra space for REGS that might be created by
     load_mems.  We allocate a little extra slop as well, in the hopes
     that even after the moving of movables creates some new registers
     we won't have to reallocate these arrays.  However, we do grow
     the arrays, if necessary, in load_mems_recount_loop_regs_set.  */
  nregs = max_reg_num () + loop_mems_idx + 16;
  VARRAY_INT_INIT (set_in_loop, nregs, "set_in_loop");
  VARRAY_INT_INIT (n_times_set, nregs, "n_times_set");
  VARRAY_CHAR_INIT (may_not_optimize, nregs, "may_not_optimize");
  VARRAY_RTX_INIT (reg_single_usage, nregs, "reg_single_usage");

  count_loop_regs_set (loop->top ? loop->top : loop->start, loop->end,
		       may_not_optimize, reg_single_usage, &insn_count, nregs);

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      VARRAY_CHAR (may_not_optimize, i) = 1;
      VARRAY_INT (set_in_loop, i) = 1;
    }

#ifdef AVOID_CCMODE_COPIES
  /* Don't try to move insns which set CC registers if we should not
     create CCmode register copies.  */
  for (i = max_reg_num () - 1; i >= FIRST_PSEUDO_REGISTER; i--)
    if (GET_MODE_CLASS (GET_MODE (regno_reg_rtx[i])) == MODE_CC)
      VARRAY_CHAR (may_not_optimize, i) = 1;
#endif

  bcopy ((char *) &set_in_loop->data, 
	 (char *) &n_times_set->data, nregs * sizeof (int));

  if (loop_dump_stream)
    {
      fprintf (loop_dump_stream, "\nLoop from %d to %d: %d real insns.\n",
	       INSN_UID (loop_start), INSN_UID (loop_end), insn_count);
      if (loop->cont)
	fprintf (loop_dump_stream, "Continue at insn %d.\n",
		 INSN_UID (loop->cont));
    }

  /* Scan through the loop finding insns that are safe to move.
     Set set_in_loop negative for the reg being set, so that
     this reg will be considered invariant for subsequent insns.
     We consider whether subsequent insns use the reg
     in deciding whether it is worth actually moving.

     MAYBE_NEVER is nonzero if we have passed a conditional jump insn
     and therefore it is possible that the insns we are scanning
     would never be executed.  At such times, we must make sure
     that it is safe to execute the insn once instead of zero times.
     When MAYBE_NEVER is 0, all insns will be executed at least once
     so that is not a problem.  */

  for (p = next_insn_in_loop (loop, loop->scan_start); 
       p != NULL_RTX;
       p = next_insn_in_loop (loop, p))
    {
      if (GET_RTX_CLASS (GET_CODE (p)) == 'i'
	  && find_reg_note (p, REG_LIBCALL, NULL_RTX))
	in_libcall = 1;
      else if (GET_RTX_CLASS (GET_CODE (p)) == 'i'
	       && find_reg_note (p, REG_RETVAL, NULL_RTX))
	in_libcall = 0;

      if (GET_CODE (p) == INSN
	  && (set = single_set (p))
	  && GET_CODE (SET_DEST (set)) == REG
	  && ! VARRAY_CHAR (may_not_optimize, REGNO (SET_DEST (set))))
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
	  else if (/* The register is used in basic blocks other
		      than the one where it is set (meaning that
		      something after this point in the loop might
		      depend on its value before the set).  */
		   ! reg_in_basic_block_p (p, SET_DEST (set))
		   /* And the set is not guaranteed to be executed one
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
	       which was not created by the user and not used in an exit test.
	       That behavior is incorrect and was removed.  */
	    ;
	  else if ((tem = invariant_p (src))
		   && (dependencies == 0
		       || (tem2 = invariant_p (dependencies)) != 0)
		   && (VARRAY_INT (set_in_loop, 
				   REGNO (SET_DEST (set))) == 1
		       || (tem1
			   = consec_sets_invariant_p 
			   (SET_DEST (set),
			    VARRAY_INT (set_in_loop, REGNO (SET_DEST (set))),
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

	      if (loop_info->has_call
		  && VARRAY_RTX (reg_single_usage, regno) != 0
		  && VARRAY_RTX (reg_single_usage, regno) != const0_rtx
		  && REGNO_FIRST_UID (regno) == INSN_UID (p)
		  && (REGNO_LAST_UID (regno)
		      == INSN_UID (VARRAY_RTX (reg_single_usage, regno)))
		  && VARRAY_INT (set_in_loop, regno) == 1
		  && ! side_effects_p (SET_SRC (set))
		  && ! find_reg_note (p, REG_RETVAL, NULL_RTX)
		  && (! SMALL_REGISTER_CLASSES
		      || (! (GET_CODE (SET_SRC (set)) == REG
			     && REGNO (SET_SRC (set)) < FIRST_PSEUDO_REGISTER)))
		  /* This test is not redundant; SET_SRC (set) might be
		     a call-clobbered register and the life of REGNO
		     might span a call.  */
		  && ! modified_between_p (SET_SRC (set), p,
					   VARRAY_RTX
					   (reg_single_usage, regno)) 
		  && no_labels_between_p (p, VARRAY_RTX (reg_single_usage, regno))
		  && validate_replace_rtx (SET_DEST (set), SET_SRC (set),
					   VARRAY_RTX
					   (reg_single_usage, regno))) 
		{
		  /* Replace any usage in a REG_EQUAL note.  Must copy the
		     new source, so that we don't get rtx sharing between the
		     SET_SOURCE and REG_NOTES of insn p.  */
		  REG_NOTES (VARRAY_RTX (reg_single_usage, regno))
		    = replace_rtx (REG_NOTES (VARRAY_RTX
					      (reg_single_usage, regno)), 
				   SET_DEST (set), copy_rtx (SET_SRC (set)));
				   
		  PUT_CODE (p, NOTE);
		  NOTE_LINE_NUMBER (p) = NOTE_INSN_DELETED;
		  NOTE_SOURCE_FILE (p) = 0;
		  VARRAY_INT (set_in_loop, regno) = 0;
		  continue;
		}

	      m = (struct movable *) alloca (sizeof (struct movable));
	      m->next = 0;
	      m->insn = p;
	      m->set_src = src;
	      m->dependencies = dependencies;
	      m->set_dest = SET_DEST (set);
	      m->force = 0;
	      m->consec = VARRAY_INT (set_in_loop, 
				      REGNO (SET_DEST (set))) - 1;
	      m->done = 0;
	      m->forces = 0;
	      m->partial = 0;
	      m->move_insn = move_insn;
	      m->move_insn_first = 0;
	      m->is_equiv = (find_reg_note (p, REG_EQUIV, NULL_RTX) != 0);
	      m->savemode = VOIDmode;
	      m->regno = regno;
	      /* Set M->cond if either invariant_p or consec_sets_invariant_p
		 returned 2 (only conditionally invariant).  */
	      m->cond = ((tem | tem1 | tem2) > 1);
	      m->global = (uid_luid[REGNO_LAST_UID (regno)] 
			   > INSN_LUID (loop_end)
			   || uid_luid[REGNO_FIRST_UID (regno)] < INSN_LUID (loop_start));
	      m->match = 0;
	      m->lifetime = (uid_luid[REGNO_LAST_UID (regno)]
			     - uid_luid[REGNO_FIRST_UID (regno)]);
	      m->savings = VARRAY_INT (n_times_set, regno);
	      if (find_reg_note (p, REG_RETVAL, NULL_RTX))
		m->savings += libcall_benefit (p);
	      VARRAY_INT (set_in_loop, regno) = move_insn ? -2 : -1;
	      /* Add M to the end of the chain MOVABLES.  */
	      if (movables == 0)
		movables = m;
	      else
		last_movable->next = m;
	      last_movable = m;

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
	      if (VARRAY_INT (set_in_loop, regno) == 2)
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
			       || (uid_luid[REGNO_LAST_UID (regno)]
				   > INSN_LUID (loop_end))
			       || (uid_luid[REGNO_FIRST_UID (regno)]
				   < INSN_LUID (p))
			       || (labels_in_range_p
				   (p, uid_luid[REGNO_FIRST_UID (regno)])));
		  if (maybe_never && m->global)
		    m->savemode = GET_MODE (SET_SRC (set1));
		  else
		    m->savemode = VOIDmode;
		  m->regno = regno;
		  m->cond = 0;
		  m->match = 0;
		  m->lifetime = (uid_luid[REGNO_LAST_UID (regno)]
				 - uid_luid[REGNO_FIRST_UID (regno)]);
		  m->savings = 1;
		  VARRAY_INT (set_in_loop, regno) = -1;
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
      /* Similar code appears twice in strength_reduce.  */
      else if ((GET_CODE (p) == CODE_LABEL || GET_CODE (p) == JUMP_INSN)
	       /* If we enter the loop in the middle, and scan around to the
		  beginning, don't set maybe_never for that.  This must be an
		  unconditional jump, otherwise the code at the top of the
		  loop might never be executed.  Unconditional jumps are
		  followed a by barrier then loop end.  */
               && ! (GET_CODE (p) == JUMP_INSN && JUMP_LABEL (p) == loop->top
		     && NEXT_INSN (NEXT_INSN (p)) == loop_end
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
     Store 0 in set_in_loop for each reg that is moved.

     Generally this increases code size, so do not move moveables when
     optimizing for code size.  */

  if (! optimize_size)
    move_movables (movables, threshold,
		   insn_count, loop_start, loop_end, nregs);

  /* Now candidates that still are negative are those not moved.
     Change set_in_loop to indicate that those are not actually invariant.  */
  for (i = 0; i < nregs; i++)
    if (VARRAY_INT (set_in_loop, i) < 0)
      VARRAY_INT (set_in_loop, i) = VARRAY_INT (n_times_set, i);

  /* Now that we've moved some things out of the loop, we might be able to
     hoist even more memory references.  */
  load_mems_and_recount_loop_regs_set (loop, &insn_count);

  for (update_start = loop_start;
       PREV_INSN (update_start) && GET_CODE (PREV_INSN (update_start)) != CODE_LABEL;
       update_start = PREV_INSN (update_start))
    ;
  update_end = NEXT_INSN (loop_end);

  reg_scan_update (update_start, update_end, loop_max_reg);
  loop_max_reg = max_reg_num ();

  if (flag_strength_reduce)
    {
      the_movables = movables;
      strength_reduce (loop, insn_count, unroll_p, bct_p);

      reg_scan_update (update_start, update_end, loop_max_reg);
      loop_max_reg = max_reg_num ();
    }

  VARRAY_FREE (reg_single_usage);
  VARRAY_FREE (set_in_loop);
  VARRAY_FREE (n_times_set);
  VARRAY_FREE (may_not_optimize);
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
combine_movables (movables, nregs)
     struct movable *movables;
     int nregs;
{
  register struct movable *m;
  char *matched_regs = (char *) xmalloc (nregs);
  enum machine_mode mode;

  /* Regs that are set more than once are not allowed to match
     or be matched.  I'm no longer sure why not.  */
  /* Perhaps testing m->consec_sets would be more appropriate here?  */

  for (m = movables; m; m = m->next)
    if (m->match == 0 && VARRAY_INT (n_times_set, m->regno) == 1 && !m->partial)
      {
	register struct movable *m1;
	int regno = m->regno;

	bzero (matched_regs, nregs);
	matched_regs[regno] = 1;

	/* We want later insns to match the first one.  Don't make the first
	   one match any later ones.  So start this loop at m->next.  */
	for (m1 = m->next; m1; m1 = m1->next)
	  if (m != m1 && m1->match == 0 && VARRAY_INT (n_times_set, m1->regno) == 1
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
	    int first = uid_luid[REGNO_FIRST_UID (m->regno)];
	    int last = uid_luid[REGNO_LAST_UID (m->regno)];

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
		if (! (uid_luid[REGNO_FIRST_UID (m1->regno)] > last
		       || uid_luid[REGNO_LAST_UID (m1->regno)] < first))
		  goto overlap;

	    /* No overlap: we can combine this with the others.  */
	    m0->lifetime += m->lifetime;
	    m0->savings += m->savings;
	    m->done = 1;
	    m->match = m0;

	  overlap: ;
	  }
    }

  /* Clean up.  */
  free (matched_regs);
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
  register const char *fmt;

  if (x == y)
    return 1;
  if (x == 0 || y == 0)
    return 0;

  code = GET_CODE (x);

  /* If we have a register and a constant, they may sometimes be
     equal.  */
  if (GET_CODE (x) == REG && VARRAY_INT (set_in_loop, REGNO (x)) == -2
      && CONSTANT_P (y))
    {
      for (m = movables; m; m = m->next)
	if (m->move_insn && m->regno == REGNO (x)
	    && rtx_equal_p (m->set_src, y))
	  return 1;
    }
  else if (GET_CODE (y) == REG && VARRAY_INT (set_in_loop, REGNO (y)) == -2
	   && CONSTANT_P (x))
    {
      for (m = movables; m; m = m->next)
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
  insns in INSNS which use the reference.  */

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
	  REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_LABEL, XEXP (x, 0),
						REG_NOTES (insn));
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
  rtx *reg_map = (rtx *) xcalloc (nregs, sizeof (rtx));
  char *already_moved = (char *) xcalloc (nregs, sizeof (char));

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

	  if (moved_once[regno] && loop_dump_stream)
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
		 (moved_once[regno] ? insn_count * 2 : insn_count)
	      || (m->forces && m->forces->done
		  && VARRAY_INT (n_times_set, m->forces->regno) == 1))
	    {
	      int count;
	      register struct movable *m1;
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
		  i1 = emit_insn_before (newpat, loop_start);

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
		  end_sequence ();

		  add_label_notes (m->set_src, temp);

		  i1 = emit_insns_before (temp, loop_start);
		  if (! find_reg_note (i1, REG_EQUAL, NULL_RTX))
		    REG_NOTES (i1)
		      = gen_rtx_EXPR_LIST (m->is_equiv ? REG_EQUIV : REG_EQUAL,
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
				    CALL_INSN_FUNCTION_USAGE (i1)
				      = copy_rtx (CALL_INSN_FUNCTION_USAGE (temp));
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
			    CALL_INSN_FUNCTION_USAGE (i1)
			      = copy_rtx (CALL_INSN_FUNCTION_USAGE (p));
			}
		      else if (count == m->consec && m->move_insn_first)
			{
			  /* The SET_SRC might not be invariant, so we must
			     use the REG_EQUAL note.  */
			  start_sequence ();
			  emit_move_insn (m->set_dest, m->set_src);
			  temp = get_insns ();
			  end_sequence ();

			  add_label_notes (m->set_src, temp);

			  i1 = emit_insns_before (temp, loop_start);
			  if (! find_reg_note (i1, REG_EQUAL, NULL_RTX))
			    REG_NOTES (i1)
			      = gen_rtx_EXPR_LIST ((m->is_equiv ? REG_EQUIV
						    : REG_EQUAL),
						   m->set_src, REG_NOTES (i1));
			}
		      else
			i1 = emit_insn_before (PATTERN (p), loop_start);

		      if (REG_NOTES (i1) == 0)
			{
			  REG_NOTES (i1) = REG_NOTES (p);

			  /* If there is a REG_EQUAL note present whose value
			     is not loop invariant, then delete it, since it
			     may cause problems with later optimization passes.
			     It is possible for cse to create such notes
			     like this as a result of record_jump_cond.  */
		      
			  if ((temp = find_reg_note (i1, REG_EQUAL, NULL_RTX))
			      && ! invariant_p (XEXP (temp, 0)))
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
	      moved_once[regno] = 1;

	      /* The reg set here is now invariant.  */
	      if (! m->partial)
		VARRAY_INT (set_in_loop, regno) = 0;

	      m->done = 1;

	      /* Change the length-of-life info for the register
		 to say it lives at least the full length of this loop.
		 This will help guide optimizations in outer loops.  */

	      if (uid_luid[REGNO_FIRST_UID (regno)] > INSN_LUID (loop_start))
		/* This is the old insn before all the moved insns.
		   We can't use the moved insn because it is out of range
		   in uid_luid.  Only the old insns have luids.  */
		REGNO_FIRST_UID (regno) = INSN_UID (loop_start);
	      if (uid_luid[REGNO_LAST_UID (regno)] < INSN_LUID (end))
		REGNO_LAST_UID (regno) = INSN_UID (end);

	      /* Combine with this moved insn any other matching movables.  */

	      if (! m->partial)
		for (m1 = movables; m1; m1 = m1->next)
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

		      /* if library call, delete all insn except last, which
			 is deleted below */
		      if ((temp = find_reg_note (m1->insn, REG_RETVAL,
						 NULL_RTX)))
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
			VARRAY_INT (set_in_loop, m1->regno) = 0;
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

  /* Clean up.  */
  free (reg_map);
  free (already_moved);
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
  register const char *fmt;

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
  register const char *fmt;
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
      
    default:
      break;
    }

  value = 0;
  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	value += count_nonfixed_reads (XEXP (x, i));
      else if (fmt[i] == 'E')
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

  new
    = gen_rtx_SET
      (VOIDmode,
       gen_rtx_STRICT_LOW_PART
       (VOIDmode,
	gen_rtx_SUBREG (GET_MODE (XEXP (SET_SRC (PATTERN (p)), 0)),
			SET_DEST (PATTERN (p)), 0)),
       XEXP (SET_SRC (PATTERN (p)), 0));

  insn_code_number = recog (new, p);

  if (insn_code_number)
    {
      register int i;

      /* Clear destination register before the loop.  */
      emit_insn_before (gen_rtx_SET (VOIDmode,
				     SET_DEST (PATTERN (p)), const0_rtx),
			loop_start);

      /* Inside the loop, just load the low part.  */
      PATTERN (p) = new;
    }
}
#endif

/* Scan a loop setting the elements `cont', `vtop', `loops_enclosed',
   `has_call', `has_volatile', and `has_tablejump' within LOOP_INFO.
   Set the global variables `unknown_address_altered',
   `unknown_constant_address_altered', and `num_mem_sets'.  Also, fill
   in the array `loop_mems' and the list `loop_store_mems'.  */

static void
prescan_loop (loop)
     struct loop *loop;
{
  register int level = 1;
  rtx insn;
  struct loop_info *loop_info = loop->info;
  rtx start = loop->start;
  rtx end = loop->end;
  /* The label after END.  Jumping here is just like falling off the
     end of the loop.  We use next_nonnote_insn instead of next_label
     as a hedge against the (pathological) case where some actual insn
     might end up between the two.  */
  rtx exit_target = next_nonnote_insn (end);

  loop_info->has_indirect_jump = indirect_jump_in_function;
  loop_info->has_call = 0;
  loop_info->has_volatile = 0;
  loop_info->has_tablejump = 0;
  loop_info->has_multiple_exit_targets = 0;
  loop->cont = 0;
  loop->vtop = 0;
  loop->level = 1;

  unknown_address_altered = 0;
  unknown_constant_address_altered = 0;
  loop_store_mems = NULL_RTX;
  first_loop_store_insn = NULL_RTX;
  loop_mems_idx = 0;
  num_mem_sets = 0;

  for (insn = NEXT_INSN (start); insn != NEXT_INSN (end);
       insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == NOTE)
	{
	  if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_BEG)
	    {
	      ++level;
	      /* Count number of loops contained in this one.  */
	      loop->level++;
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
		loop->cont = insn;
	    }
	  else if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_VTOP)
	    {
	      /* If there is a NOTE_INSN_LOOP_VTOP, then this is a for
		 or while style loop, with a loop exit test at the
		 start.  Thus, we can assume that the loop condition
		 was true when the loop was entered.  */
	      if (level == 1)
		loop->vtop = insn;
	    }
	}
      else if (GET_CODE (insn) == CALL_INSN)
	{
	  if (! CONST_CALL_P (insn))
	    unknown_address_altered = 1;
	  loop_info->has_call = 1;
	}
      else if (GET_CODE (insn) == INSN || GET_CODE (insn) == JUMP_INSN)
	{
	  rtx label1 = NULL_RTX;
	  rtx label2 = NULL_RTX;

	  if (volatile_refs_p (PATTERN (insn)))
	    loop_info->has_volatile = 1;

	  if (GET_CODE (insn) == JUMP_INSN
	      && (GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC
		  || GET_CODE (PATTERN (insn)) == ADDR_VEC))
	    loop_info->has_tablejump = 1;
	  
	  note_stores (PATTERN (insn), note_addr_stored, NULL);
	  if (! first_loop_store_insn && loop_store_mems)
	    first_loop_store_insn = insn;

	  if (! loop_info->has_multiple_exit_targets
	      && GET_CODE (insn) == JUMP_INSN
	      && GET_CODE (PATTERN (insn)) == SET
	      && SET_DEST (PATTERN (insn)) == pc_rtx)
	    {
	      if (GET_CODE (SET_SRC (PATTERN (insn))) == IF_THEN_ELSE)
		{
		  label1 = XEXP (SET_SRC (PATTERN (insn)), 1);
		  label2 = XEXP (SET_SRC (PATTERN (insn)), 2);
		}
	      else
		{
		  label1 = SET_SRC (PATTERN (insn));
		}

	      do {
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
	      } while (label1);
	    }
	}
      else if (GET_CODE (insn) == RETURN)
	loop_info->has_multiple_exit_targets = 1;
    }

  /* Now, rescan the loop, setting up the LOOP_MEMS array.  */
  if (/* We can't tell what MEMs are aliased by what.  */
      ! unknown_address_altered 
      /* An exception thrown by a called function might land us
	 anywhere.  */
      && ! loop_info->has_call
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
      for_each_rtx (&insn, insert_loop_mem, 0);
}

/* LOOP->CONT_DOMINATOR is now the last label between the loop start
   and the continue note that is a the destination of a (cond)jump after
   the continue note.  If there is any (cond)jump between the loop start
   and what we have so far as LOOP->CONT_DOMINATOR that has a
   target between LOOP->DOMINATOR and the continue note, move
   LOOP->CONT_DOMINATOR forward to that label; if a jump's
   destination cannot be determined, clear LOOP->CONT_DOMINATOR.  */

static void
verify_dominator (loop)
     struct loop *loop;
{
  rtx insn;

  if (! loop->cont_dominator)
    /* This can happen for an empty loop, e.g. in
       gcc.c-torture/compile/920410-2.c  */
    return;
  if (loop->cont_dominator == const0_rtx)
    {
      loop->cont_dominator = 0;
      return;
    }
  for (insn = loop->start; insn != loop->cont_dominator;
       insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == JUMP_INSN
	  && GET_CODE (PATTERN (insn)) != RETURN)
	{
	  rtx label = JUMP_LABEL (insn);
	  int label_luid;

	  /* If it is not a jump we can easily understand or for
	     which we do not have jump target information in the JUMP_LABEL
	     field (consider ADDR_VEC and ADDR_DIFF_VEC insns), then clear
	     LOOP->CONT_DOMINATOR.  */
	  if ((! condjump_p (insn)
	       && ! condjump_in_parallel_p (insn))
	      || label == NULL_RTX)
	    {
	      loop->cont_dominator = NULL_RTX;
	      return;
	    }

	  label_luid = INSN_LUID (label);
	  if (label_luid < INSN_LUID (loop->cont)
	      && (label_luid
		  > INSN_LUID (loop->cont)))
	    loop->cont_dominator = label;
	}
    }
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

	  case NOTE_INSN_SETJMP:
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
	    break;

	  case NOTE_INSN_LOOP_CONT:
	    current_loop->cont = insn;
	    break;
	  case NOTE_INSN_LOOP_END:
	    if (! current_loop)
	      abort ();

	    current_loop->end = insn;
	    verify_dominator (current_loop);
	    current_loop = current_loop->outer;
	    break;

	  default:
	    break;
	  }
      /* If for any loop, this is a jump insn between the NOTE_INSN_LOOP_CONT
	 and NOTE_INSN_LOOP_END notes, update loop->dominator.  */
      else if (GET_CODE (insn) == JUMP_INSN
	       && GET_CODE (PATTERN (insn)) != RETURN
	       && current_loop)
	{
	  rtx label = JUMP_LABEL (insn);

	  if (! condjump_p (insn) && ! condjump_in_parallel_p (insn))
	    label = NULL_RTX;

	  loop = current_loop;
	  do
	    {
	      /* First see if we care about this loop.  */
	      if (loop->cont && loop->cont_dominator != const0_rtx)
		{
		  /* If the jump destination is not known, invalidate
		     loop->const_dominator.  */
		  if (! label)
		    loop->cont_dominator = const0_rtx;
		  else
		    /* Check if the destination is between loop start and
		       cont.  */
		    if ((INSN_LUID (label)
			 < INSN_LUID (loop->cont))
			&& (INSN_LUID (label)
			    > INSN_LUID (loop->start))
			/* And if there is no later destination already
			   recorded.  */
			&& (! loop->cont_dominator
			    || (INSN_LUID (label)
				> INSN_LUID (loop->cont_dominator))))
		      loop->cont_dominator = label;
		}
	      loop = loop->outer;
	    }
	  while (loop);
	}

      /* Note that this will mark the NOTE_INSN_LOOP_END note as being in the
	 enclosing loop, but this doesn't matter.  */
      uid_loop[INSN_UID (insn)] = current_loop;
    }

  /* Any loop containing a label used in an initializer must be invalidated,
     because it can be jumped into from anywhere.  */

  for (label = forced_labels; label; label = XEXP (label, 1))
    {
      for (loop = uid_loop[INSN_UID (XEXP (label, 0))];
	   loop; loop = loop->outer)
	loop->invalid = 1;
    }

  /* Any loop containing a label used for an exception handler must be
     invalidated, because it can be jumped into from anywhere.  */

  for (label = exception_handler_labels; label; label = XEXP (label, 1))
    {
      for (loop = uid_loop[INSN_UID (XEXP (label, 0))];
	   loop; loop = loop->outer)
	loop->invalid = 1;
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
	struct loop *this_loop = uid_loop[INSN_UID (insn)];

	if (GET_CODE (insn) == INSN || GET_CODE (insn) == CALL_INSN)
	  {
	    rtx note = find_reg_note (insn, REG_LABEL, NULL_RTX);
	    if (note)
	      {
		for (loop = uid_loop[INSN_UID (XEXP (note, 0))];
		     loop; loop = loop->outer)
		  loop->invalid = 1;
	      }
	  }

	if (GET_CODE (insn) != JUMP_INSN)
	  continue;

	mark_loop_jump (PATTERN (insn), this_loop);

	/* See if this is an unconditional branch outside the loop.  */
	if (this_loop
	    && (GET_CODE (PATTERN (insn)) == RETURN
		|| (simplejump_p (insn)
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
		&& condjump_p (p)
		&& ! simplejump_p (p)
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
		   if (invert_jump (p, new_label))
		     {
		       rtx q, r;

		       /* If no suitable BARRIER was found, create a suitable
			  one before TARGET.  Since TARGET is a fall through
			  path, we'll need to insert an jump around our block
			  and a add a BARRIER before TARGET.

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
		       new_label = squeeze_notes (new_label, 
						  last_insn_to_move);
		       reorder_insns (new_label, last_insn_to_move, loc);

		       /* All those insns are now in TARGET_LOOP.  */
		       for (q = new_label; 
			    q != NEXT_INSN (last_insn_to_move);
			    q = NEXT_INSN (q))
			 uid_loop[INSN_UID (q)] = target_loop;

		       /* The label jumped to by INSN is no longer a loop exit.
			  Unless INSN does not have a label (e.g., it is a
			  RETURN insn), search loop->exit_labels to find
			  its label_ref, and remove it.  Also turn off
			  LABEL_OUTSIDE_LOOP_P bit.  */
		       if (JUMP_LABEL (insn))
			 {
			   for (q = 0,
				r = this_loop->exit_labels;
				r; q = r, r = LABEL_NEXTREF (r))
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
  if (x == 0 || GET_CODE (x) != MEM)
    return;

  /* Count number of memory writes.
     This affects heuristics in strength_reduce.  */
  num_mem_sets++;

  /* BLKmode MEM means all memory is clobbered.  */
    if (GET_MODE (x) == BLKmode)
    {
      if (RTX_UNCHANGING_P (x))
	unknown_constant_address_altered = 1;
      else
	unknown_address_altered = 1;

      return;
    }

  loop_store_mems = gen_rtx_EXPR_LIST (VOIDmode, x, loop_store_mems);
}

/* X is a value modified by an INSN that references a biv inside a loop
   exit test (ie, X is somehow related to the value of the biv).  If X
   is a pseudo that is used more than once, then the biv is (effectively)
   used more than once.  DATA is really an `int *', and is set if the
   biv is used more than once.  */

static void
note_set_pseudo_multiple_uses (x, y, data)
     rtx x;
     rtx y ATTRIBUTE_UNUSED;
     void *data;
{
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
      || ! VARRAY_RTX (reg_single_usage, REGNO (x))
      || VARRAY_RTX (reg_single_usage, REGNO (x)) == const0_rtx)
    *((int *) data) = 1;
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
  register const char *fmt;
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
	   || x == arg_pointer_rtx)
	  && ! current_function_has_nonlocal_goto)
	return 1;

      if (current_loop_info->has_call
	  && REGNO (x) < FIRST_PSEUDO_REGISTER && call_used_regs[REGNO (x)])
	return 0;

      if (VARRAY_INT (set_in_loop, REGNO (x)) < 0)
	return 2;

      return VARRAY_INT (set_in_loop, REGNO (x)) == 0;

    case MEM:
      /* Volatile memory references must be rejected.  Do this before
	 checking for read-only items, so that volatile read-only items
	 will be rejected also.  */
      if (MEM_VOLATILE_P (x))
	return 0;

      /* If we had a subroutine call, any location in memory could
	 have been clobbered.  We used to test here for volatile and
	 readonly, but true_dependence knows how to do that better
	 than we do. */
      if (RTX_UNCHANGING_P (x)
	  ? unknown_constant_address_altered : unknown_address_altered)
	return 0;

      /* See if there is any dependence between a store and this load.  */
      mem_list_entry = loop_store_mems;
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
  int old = VARRAY_INT (set_in_loop, regno);
  int value = 0;
  int this;

  /* If N_SETS hit the limit, we can't rely on its value.  */
  if (n_sets == 127)
    return 0;

  VARRAY_INT (set_in_loop, regno) = 0;

  while (count > 0)
    {
      register enum rtx_code code;
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
	  this = invariant_p (SET_SRC (set));
	  if (this != 0)
	    value |= this;
	  else if ((temp = find_reg_note (p, REG_EQUAL, NULL_RTX)))
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
	  VARRAY_INT (set_in_loop, regno) = old;
	  return 0;
	}
    }

  VARRAY_INT (set_in_loop, regno) = old;
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
     varray_type usage;
{
  enum rtx_code code = GET_CODE (x);
  const char *fmt = GET_RTX_FORMAT (code);
  int i, j;

  if (code == REG)
    VARRAY_RTX (usage, REGNO (x))
      = (VARRAY_RTX (usage, REGNO (x)) != 0 
	 && VARRAY_RTX (usage, REGNO (x)) != insn)
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

/* Count and record any set in X which is contained in INSN.  Update
   MAY_NOT_MOVE and LAST_SET for any register set in X.  */

static void
count_one_set (insn, x, may_not_move, last_set)
     rtx insn, x;
     varray_type may_not_move;
     rtx *last_set;
{
  if (GET_CODE (x) == CLOBBER && GET_CODE (XEXP (x, 0)) == REG)
    /* Don't move a reg that has an explicit clobber.
       It's not worth the pain to try to do it correctly.  */
    VARRAY_CHAR (may_not_move, REGNO (XEXP (x, 0))) = 1;

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
	  register int regno = REGNO (dest);
	  /* If this is the first setting of this reg
	     in current basic block, and it was set before,
	     it must be set in two basic blocks, so it cannot
	     be moved out of the loop.  */
	  if (VARRAY_INT (set_in_loop, regno) > 0 
	      && last_set[regno] == 0)
	    VARRAY_CHAR (may_not_move, regno) = 1;
	  /* If this is not first setting in current basic block,
	     see if reg was used in between previous one and this.
	     If so, neither one can be moved.  */
	  if (last_set[regno] != 0
	      && reg_used_between_p (dest, last_set[regno], insn))
	    VARRAY_CHAR (may_not_move, regno) = 1;
	  if (VARRAY_INT (set_in_loop, regno) < 127)
	    ++VARRAY_INT (set_in_loop, regno);
	  last_set[regno] = insn;
	}
    }
}

/* Increment SET_IN_LOOP at the index of each register
   that is modified by an insn between FROM and TO.
   If the value of an element of SET_IN_LOOP becomes 127 or more,
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
     varray_type may_not_move;
     varray_type single_usage;
     int *count_ptr;
     int nregs;
{
  register rtx *last_set = (rtx *) xcalloc (nregs, sizeof (rtx));
  register rtx insn;
  register int count = 0;

  for (insn = from; insn != to; insn = NEXT_INSN (insn))
    {
      if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	{
	  ++count;

	  /* Record registers that have exactly one use.  */
	  find_single_use_in_loop (insn, PATTERN (insn), single_usage);

	  /* Include uses in REG_EQUAL notes.  */
	  if (REG_NOTES (insn))
	    find_single_use_in_loop (insn, REG_NOTES (insn), single_usage);

	  if (GET_CODE (PATTERN (insn)) == SET
	      || GET_CODE (PATTERN (insn)) == CLOBBER)
	    count_one_set (insn, PATTERN (insn), may_not_move, last_set);
	  else if (GET_CODE (PATTERN (insn)) == PARALLEL)
	    {
	      register int i;
	      for (i = XVECLEN (PATTERN (insn), 0) - 1; i >= 0; i--)
		count_one_set (insn, XVECEXP (PATTERN (insn), 0, i),
			       may_not_move, last_set);
	    }
	}

      if (GET_CODE (insn) == CODE_LABEL || GET_CODE (insn) == JUMP_INSN)
	bzero ((char *) last_set, nregs * sizeof (rtx));
    }
  *count_ptr = count;

  /* Clean up.  */
  free (last_set);
}

/* Given a loop that is bounded by LOOP_START and LOOP_END
   and that is entered at LOOP_SCAN_START,
   return 1 if the register set in SET contained in insn INSN is used by
   any insn that precedes INSN in cyclic order starting
   from the loop entry point.

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
      if (GET_RTX_CLASS (GET_CODE (p)) == 'i'
	  && reg_overlap_mentioned_p (reg, PATTERN (p)))
	return 1;

      if (p == loop->end)
	p = loop->start;
    }

  return 0;
}

/* A "basic induction variable" or biv is a pseudo reg that is set
   (within this loop) only by incrementing or decrementing it.  */
/* A "general induction variable" or giv is a pseudo reg whose
   value is a linear function of a biv.  */

/* Bivs are recognized by `basic_induction_var';
   Givs by `general_induction_var'.  */

/* Indexed by register number, indicates whether or not register is an
   induction variable, and if so what type.  */

varray_type reg_iv_type;

/* Indexed by register number, contains pointer to `struct induction'
   if register is an induction variable.  This holds general info for
   all induction variables.  */

varray_type reg_iv_info;

/* Indexed by register number, contains pointer to `struct iv_class'
   if register is a basic induction variable.  This holds info describing
   the class (a related group) of induction variables that the biv belongs
   to.  */

struct iv_class **reg_biv_class;

/* The head of a list which links together (via the next field)
   every iv class for the current loop.  */

struct iv_class *loop_iv_list;

/* Givs made from biv increments are always splittable for loop unrolling.
   Since there is no regscan info for them, we have to keep track of them
   separately.  */
int first_increment_giv, last_increment_giv;

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

/* Perform strength reduction and induction variable elimination.  

   Pseudo registers created during this function will be beyond the last
   valid index in several tables including n_times_set and regno_last_uid.
   This does not cause a problem here, because the added registers cannot be
   givs outside of their loop, and hence will never be reconsidered.
   But scan_loop must check regnos to make sure they are in bounds. 
   
   LOOP_SCAN_START is the first instruction in the loop, as the loop would
   actually be executed.  END is the NOTE_INSN_LOOP_END.  LOOP_TOP is
   the first instruction in the loop, as it is layed out in the
   instruction stream.  LOOP_START is the NOTE_INSN_LOOP_BEG.
   LOOP_CONT is the NOTE_INSN_LOOP_CONT.  */

static void
strength_reduce (loop, insn_count, unroll_p, bct_p)
     struct loop *loop;
     int insn_count;
     int unroll_p, bct_p ATTRIBUTE_UNUSED;
{
  rtx p;
  rtx set;
  rtx inc_val;
  rtx mult_val;
  rtx dest_reg;
  rtx *location;
  /* This is 1 if current insn is not executed at least once for every loop
     iteration.  */
  int not_every_iteration = 0;
  /* This is 1 if current insn may be executed more than once for every
     loop iteration.  */
  int maybe_multiple = 0;
  /* This is 1 if we have past a branch back to the top of the loop
     (aka a loop latch).  */
  int past_loop_latch = 0;
  /* Temporary list pointers for traversing loop_iv_list.  */
  struct iv_class *bl, **backbl;
  struct loop_info *loop_info = loop->info;
  /* Ratio of extra register life span we can justify
     for saving an instruction.  More if loop doesn't call subroutines
     since in that case saving an insn makes more difference
     and more registers are available.  */
  /* ??? could set this to last value of threshold in move_movables */
  int threshold = (loop_info->has_call ? 1 : 2) * (3 + n_non_fixed_regs);
  /* Map of pseudo-register replacements.  */
  rtx *reg_map = NULL;
  int reg_map_size;
  int call_seen;
  rtx test;
  rtx end_insert_before;
  int loop_depth = 0;
  int n_extra_increment;
  int unrolled_insn_copies = 0;
  rtx loop_start = loop->start;
  rtx loop_end = loop->end;
  rtx loop_scan_start = loop->scan_start;
  rtx loop_top = loop->top;
  rtx loop_cont = loop->cont;

  /* If loop_scan_start points to the loop exit test, we have to be wary of
     subversive use of gotos inside expression statements.  */
  if (prev_nonnote_insn (loop_scan_start) != prev_nonnote_insn (loop_start))
    maybe_multiple = back_branch_in_range_p (loop_scan_start, loop_start, loop_end);

  VARRAY_INT_INIT (reg_iv_type, max_reg_before_loop, "reg_iv_type");
  VARRAY_GENERIC_PTR_INIT (reg_iv_info, max_reg_before_loop, "reg_iv_info");
  reg_biv_class = (struct iv_class **)
    xcalloc (max_reg_before_loop, sizeof (struct iv_class *));

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

  for (p = next_insn_in_loop (loop, loop_scan_start);
       p != NULL_RTX;
       p = next_insn_in_loop (loop, p))
    {
      if (GET_CODE (p) == INSN
	  && (set = single_set (p))
	  && GET_CODE (SET_DEST (set)) == REG)
	{
	  dest_reg = SET_DEST (set);
	  if (REGNO (dest_reg) < max_reg_before_loop
	      && REGNO (dest_reg) >= FIRST_PSEUDO_REGISTER
	      && REG_IV_TYPE (REGNO (dest_reg)) != NOT_BASIC_INDUCT)
	    {
	      int multi_insn_incr = 0;

	      if (basic_induction_var (SET_SRC (set), GET_MODE (SET_SRC (set)),
				       dest_reg, p, loop->level,
				       &inc_val, &mult_val,
				       &location, &multi_insn_incr))
		{
		  /* It is a possible basic induction variable.
		     Create and initialize an induction structure for it.  */

		  struct induction *v
		    = (struct induction *) alloca (sizeof (struct induction));

		  record_biv (v, p, dest_reg, inc_val, mult_val, location,
			      not_every_iteration, maybe_multiple, 
			      multi_insn_incr);
		  REG_IV_TYPE (REGNO (dest_reg)) = BASIC_INDUCT;
		}
	      else if (REGNO (dest_reg) < max_reg_before_loop)
		REG_IV_TYPE (REGNO (dest_reg)) = NOT_BASIC_INDUCT;
	    }
	}

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
	      if (insn == loop_scan_start)
		break;
	      if (insn == loop_end)
		{
		  if (loop_top != 0)
		    insn = loop_top;
		  else
		    break;
		  if (insn == loop_scan_start)
		    break;
		}

	      if (GET_CODE (insn) == JUMP_INSN
		  && GET_CODE (PATTERN (insn)) != RETURN
		  && (! condjump_p (insn)
		      || (JUMP_LABEL (insn) != 0
			  && JUMP_LABEL (insn) != loop_scan_start
			  && ! loop_insn_first_p (p, JUMP_LABEL (insn)))))
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
	  && ! (JUMP_LABEL (p) == loop_top
		&& ((NEXT_INSN (NEXT_INSN (p)) == loop_end && simplejump_p (p))
		    || (NEXT_INSN (p) == loop_end && condjump_p (p)))))
	{
	  rtx label = 0;

	  /* If this is a jump outside the loop, then it also doesn't
	     matter.  Check to see if the target of this branch is on the
	     loop->exits_labels list.  */
	     
	  for (label = uid_loop[INSN_UID (loop_start)]->exit_labels;
	       label;
	       label = LABEL_NEXTREF (label))
	    if (XEXP (label, 0) == JUMP_LABEL (p))
	      break;

	  if (! label)
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
	  && JUMP_LABEL (p) == NEXT_INSN (loop_start))
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
	  && ! past_loop_latch
	  && GET_CODE (p) == CODE_LABEL
	  && no_labels_between_p (p, loop_end)
	  && loop_insn_first_p (p, loop_cont))
	not_every_iteration = 0;
    }

  /* Scan loop_iv_list to remove all regs that proved not to be bivs.
     Make a sanity check against n_times_set.  */
  for (backbl = &loop_iv_list, bl = *backbl; bl; bl = bl->next)
    {
      int fail = 0;

      if (REG_IV_TYPE (bl->regno) != BASIC_INDUCT
	  /* Above happens if register modified by subreg, etc.  */
	  /* Make sure it is not recognized as a basic induction var: */
	  || VARRAY_INT (n_times_set, bl->regno) != bl->biv_count
	  /* If never incremented, it is invariant that we decided not to
	     move.  So leave it alone.  */
	  || ! bl->incremented)
	fail = 1;
      else if (bl->biv_count > 1)
	{
	  /* ??? If we have multiple increments for this BIV, and any of
	     them take multiple insns to perform the increment, drop the
	     BIV, since the bit below that converts the extra increments
	     into GIVs can't handle the multiple insn increment.  */
	  
	  struct induction *v;
	  for (v = bl->biv; v ; v = v->next_iv)
	    if (v->multi_insn_incr)
	      fail = 1;
	}

      if (fail)
	{
	  if (loop_dump_stream)
	    fprintf (loop_dump_stream, "Reg %d: biv discarded, %s\n",
		     bl->regno,
		     (REG_IV_TYPE (bl->regno) != BASIC_INDUCT
		      ? "not induction variable"
		      : (! bl->incremented ? "never incremented"
			 : "count error")));
	  
	  REG_IV_TYPE (bl->regno) = NOT_BASIC_INDUCT;
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
      if (unroll_p)
	unroll_loop (loop, insn_count, end_insert_before, 0);

      goto egress;
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
	note_stores (PATTERN (p), record_initial, NULL);

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
	      bl->init_set = gen_rtx_SET (VOIDmode,
					  XEXP (test, 0), XEXP (test, 1));
	    }
	  else
	    bl->initial_test = test;
	}
    }

  /* Look at the each biv and see if we can say anything better about its
     initial value from any initializing insns set up above.  (This is done
     in two passes to avoid missing SETs in a PARALLEL.)  */
  for (backbl = &loop_iv_list; (bl = *backbl); backbl = &bl->next)
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
		{
		  fprintf (loop_dump_stream, HOST_WIDE_INT_PRINT_DEC, INTVAL (src));
		  fputc ('\n', loop_dump_stream);
		}
	      else
		{
		  print_rtl (loop_dump_stream, src);
		  fprintf (loop_dump_stream, "\n");
		}
	    }
	}
      else
	{
	  struct iv_class *bl2 = 0;
	  rtx increment = NULL_RTX;

	  /* Biv initial value is not a simple move.  If it is the sum of
	     another biv and a constant, check if both bivs are incremented
	     in lockstep.  Then we are actually looking at a giv.
	     For simplicity, we only handle the case where there is but a
	     single increment, and the register is not used elsewhere.  */
	  if (bl->biv_count == 1
	      && bl->regno < max_reg_before_loop
	      && uid_luid[REGNO_LAST_UID (bl->regno)] < INSN_LUID (loop_end)
	      && GET_CODE (src) == PLUS
	      && GET_CODE (XEXP (src, 0)) == REG
	      && CONSTANT_P (XEXP (src, 1))
	      && ((increment = biv_total_increment (bl, loop_start, loop_end))
		  != NULL_RTX))
	    {
	      int regno = REGNO (XEXP (src, 0));

	      for (bl2 = loop_iv_list; bl2; bl2 = bl2->next)
		if (bl2->regno == regno)
		  break;
	    }
	
	  /* Now, can we transform this biv into a giv?  */
	  if (bl2
	      && bl2->biv_count == 1
	      && rtx_equal_p (increment,
			      biv_total_increment (bl2, loop_start, loop_end))
	      /* init_insn is only set to insns that are before loop_start
		 without any intervening labels.  */
	      && ! reg_set_between_p (bl2->biv->src_reg,
				      PREV_INSN (bl->init_insn), loop_start)
	      /* The register from BL2 must be set before the register from
		 BL is set, or we must be able to move the latter set after
		 the former set.  Currently there can't be any labels
	         in-between when biv_total_increment returns nonzero both times
		 but we test it here in case some day some real cfg analysis
		 gets used to set always_computable.  */
	      && (loop_insn_first_p (bl2->biv->insn, bl->biv->insn)
		  ? no_labels_between_p (bl2->biv->insn, bl->biv->insn)
		  : (! reg_used_between_p (bl->biv->src_reg, bl->biv->insn,
					   bl2->biv->insn)
		     && no_jumps_between_p (bl->biv->insn, bl2->biv->insn)))
	      && validate_change (bl->biv->insn,
				  &SET_SRC (single_set (bl->biv->insn)),
				  copy_rtx (src), 0))
	    {
	      rtx dominator = uid_loop[INSN_UID (loop_start)]->cont_dominator;
	      rtx giv = bl->biv->src_reg;
	      rtx giv_insn = bl->biv->insn;
	      rtx after_giv = NEXT_INSN (giv_insn);

	      if (loop_dump_stream)
		fprintf (loop_dump_stream, "is giv of biv %d\n", bl2->regno);
	      /* Let this giv be discovered by the generic code.  */
	      REG_IV_TYPE (bl->regno) = UNKNOWN_INDUCT;
	      reg_biv_class[bl->regno] = (struct iv_class *) NULL_PTR;
	      /* We can get better optimization if we can move the giv setting
		 before the first giv use.  */
	      if (dominator
		  && ! loop_insn_first_p (dominator, loop_scan_start)
		  && ! reg_set_between_p (bl2->biv->src_reg, loop_start,
					  dominator)
		  && ! reg_used_between_p (giv, loop_start, dominator)
		  && ! reg_used_between_p (giv, giv_insn, loop_end))
		{
		  rtx p;
		  rtx next;

		  for (next = NEXT_INSN (dominator); ; next = NEXT_INSN (next))
		    {
		      if ((GET_RTX_CLASS (GET_CODE (next)) == 'i'
			   && (reg_mentioned_p (giv, PATTERN (next))
			       || reg_set_p (bl2->biv->src_reg, next)))
			  || GET_CODE (next) == JUMP_INSN)
			break;
#ifdef HAVE_cc0
		      if (GET_RTX_CLASS (GET_CODE (next)) != 'i'
			  || ! sets_cc0_p (PATTERN (next)))
#endif
			dominator = next;
		    }
		  if (loop_dump_stream)
		    fprintf (loop_dump_stream, "move after insn %d\n",
			     INSN_UID (dominator));
		  /* Avoid problems with luids by actually moving the insn
		     and adjusting all luids in the range.  */
		  reorder_insns (giv_insn, giv_insn, dominator);
		  for (p = dominator; INSN_UID (p) >= max_uid_for_loop; )
		    p = PREV_INSN (p);
		  compute_luids (giv_insn, after_giv, INSN_LUID (p));
		  /* If the only purpose of the init insn is to initialize
		     this giv, delete it.  */
		  if (single_set (bl->init_insn)
		      && ! reg_used_between_p (giv, bl->init_insn, loop_start))
		    delete_insn (bl->init_insn);
		}
	      else if (! loop_insn_first_p (bl2->biv->insn, bl->biv->insn))
		{
		  rtx p = PREV_INSN (giv_insn);
		  while (INSN_UID (p) >= max_uid_for_loop)
		    p = PREV_INSN (p);
		  reorder_insns (giv_insn, giv_insn, bl2->biv->insn);
		  compute_luids (after_giv, NEXT_INSN (giv_insn),
				 INSN_LUID (p));
		}
	      /* Remove this biv from the chain.  */
	      if (bl->next)
		{
		  /* We move the following giv from *bl->next into *bl.
		     We have to update reg_biv_class for that moved biv
		     to point to its new address.  */
		  *bl = *bl->next;
		  reg_biv_class[bl->regno] = bl;
		}
	      else
		{
		  *backbl = 0;
		  break;
		}
	    }

	  /* If we can't make it a giv,
	     let biv keep initial value of "itself".  */
	  else if (loop_dump_stream)
	    fprintf (loop_dump_stream, "is complex\n");
	}
    }

  /* If a biv is unconditionally incremented several times in a row, convert
     all but the last increment into a giv.  */

  /* Get an upper bound for the number of registers
     we might have after all bivs have been processed.  */
  first_increment_giv = max_reg_num ();
  for (n_extra_increment = 0, bl = loop_iv_list; bl; bl = bl->next)
    n_extra_increment += bl->biv_count - 1;

  /* If the loop contains volatile memory references do not allow any
     replacements to take place, since this could loose the volatile
     markers.  */
  if (n_extra_increment  && ! loop_info->has_volatile)
    {
      int nregs = first_increment_giv + n_extra_increment;

      /* Reallocate reg_iv_type and reg_iv_info.  */
      VARRAY_GROW (reg_iv_type, nregs);
      VARRAY_GROW (reg_iv_info, nregs);

      for (bl = loop_iv_list; bl; bl = bl->next)
	{
	  struct induction **vp, *v, *next;
	  int biv_dead_after_loop = 0;

	  /* The biv increments lists are in reverse order.  Fix this
             first.  */
	  for (v = bl->biv, bl->biv = 0; v; v = next)
	    {
	      next = v->next_iv;
	      v->next_iv = bl->biv;
	      bl->biv = v;
	    }

	  /* We must guard against the case that an early exit between v->insn
	     and next->insn leaves the biv live after the loop, since that
	     would mean that we'd be missing an increment for the final
	     value.  The following test to set biv_dead_after_loop is like
	     the first part of the test to set bl->eliminable.
	     We don't check here if we can calculate the final value, since
	     this can't succeed if we already know that there is a jump
	     between v->insn and next->insn, yet next->always_executed is
	     set and next->maybe_multiple is cleared.  Such a combination
	     implies that the jump destination is outside the loop.
	     If we want to make this check more sophisticated, we should
	     check each branch between v->insn and next->insn individually
	     to see if the biv is dead at its destination.  */

	  if (uid_luid[REGNO_LAST_UID (bl->regno)] < INSN_LUID (loop_end)
	      && bl->init_insn
	      && INSN_UID (bl->init_insn) < max_uid_for_loop
	      && (uid_luid[REGNO_FIRST_UID (bl->regno)]
		  >= INSN_LUID (bl->init_insn))
#ifdef HAVE_decrement_and_branch_until_zero
	      && ! bl->nonneg
#endif
	      && ! reg_mentioned_p (bl->biv->dest_reg, SET_SRC (bl->init_set)))
	    biv_dead_after_loop = 1;

	  for (vp = &bl->biv, next = *vp; v = next, next = v->next_iv;)
	    {
	      HOST_WIDE_INT offset;
	      rtx set, add_val, old_reg, dest_reg, last_use_insn, note;
	      int old_regno, new_regno;

	      if (! v->always_executed
		  || v->maybe_multiple
		  || GET_CODE (v->add_val) != CONST_INT
		  || ! next->always_executed
		  || next->maybe_multiple
		  || ! CONSTANT_P (next->add_val)
		  || v->mult_val != const1_rtx
		  || next->mult_val != const1_rtx
		  || ! (biv_dead_after_loop
			|| no_jumps_between_p (v->insn, next->insn)))
		{
		  vp = &v->next_iv;
		  continue;
		}
	      offset = INTVAL (v->add_val);
	      set = single_set (v->insn);
	      add_val = plus_constant (next->add_val, offset);
	      old_reg = v->dest_reg;
	      dest_reg = gen_reg_rtx (v->mode);
    
	      /* Unlike reg_iv_type / reg_iv_info, the other three arrays
		 have been allocated with some slop space, so we may not
		 actually need to reallocate them.  If we do, the following
		 if statement will be executed just once in this loop.  */
	      if ((unsigned) max_reg_num () > n_times_set->num_elements)
		{
		  /* Grow all the remaining arrays.  */
		  VARRAY_GROW (set_in_loop, nregs);
		  VARRAY_GROW (n_times_set, nregs);
		  VARRAY_GROW (may_not_optimize, nregs);
		  VARRAY_GROW (reg_single_usage, nregs);
		}
    
	      if (! validate_change (next->insn, next->location, add_val, 0))
		{
		  vp = &v->next_iv;
		  continue;
		}

	      /* Here we can try to eliminate the increment by combining
		 it into the uses.  */

	      /* Set last_use_insn so that we can check against it.  */

	      for (last_use_insn = v->insn, p = NEXT_INSN (v->insn);
		   p != next->insn;
		   p = next_insn_in_loop (loop, p))
		{
		  if (GET_RTX_CLASS (GET_CODE (p)) != 'i')
		    continue;
		  if (reg_mentioned_p (old_reg, PATTERN (p)))
		    {
		      last_use_insn = p;
		    }
		}

	      /* If we can't get the LUIDs for the insns, we can't
		 calculate the lifetime.  This is likely from unrolling
		 of an inner loop, so there is little point in making this
		 a DEST_REG giv anyways.  */
	      if (INSN_UID (v->insn) >= max_uid_for_loop
		  || INSN_UID (last_use_insn) >= max_uid_for_loop
		  || ! validate_change (v->insn, &SET_DEST (set), dest_reg, 0))
		{
		  /* Change the increment at NEXT back to what it was.  */
		  if (! validate_change (next->insn, next->location,
		      next->add_val, 0))
		    abort ();
		  vp = &v->next_iv;
		  continue;
		}
	      next->add_val = add_val;
	      v->dest_reg = dest_reg;
	      v->giv_type = DEST_REG;
	      v->location = &SET_SRC (set);
	      v->cant_derive = 0;
	      v->combined_with = 0;
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
	      v->derived_from = 0;
	      v->always_computable = 1;
	      v->always_executed = 1;
	      v->replaceable = 1;
	      v->no_const_addval = 0;
    
	      old_regno = REGNO (old_reg);
	      new_regno = REGNO (dest_reg);
	      VARRAY_INT (set_in_loop, old_regno)--;
	      VARRAY_INT (set_in_loop, new_regno) = 1;
	      VARRAY_INT (n_times_set, old_regno)--;
	      VARRAY_INT (n_times_set, new_regno) = 1;
	      VARRAY_CHAR (may_not_optimize, new_regno) = 0;
    
	      REG_IV_TYPE (new_regno) = GENERAL_INDUCT;
	      REG_IV_INFO (new_regno) = v;

	      /* If next_insn has a REG_EQUAL note that mentiones OLD_REG,
		 it must be replaced.  */
	      note = find_reg_note (next->insn, REG_EQUAL, NULL_RTX);
	      if (note && reg_mentioned_p (old_reg, XEXP (note, 0)))
		XEXP (note, 0) = copy_rtx (SET_SRC (single_set (next->insn)));

	      /* Remove the increment from the list of biv increments,
		 and record it as a giv.  */
	      *vp = next;
	      bl->biv_count--;
	      v->next_iv = bl->giv;
	      bl->giv = v;
	      bl->giv_count++;
	      v->benefit = rtx_cost (SET_SRC (set), SET);
	      bl->total_benefit += v->benefit;
    
	      /* Now replace the biv with DEST_REG in all insns between
		 the replaced increment and the next increment, and
		 remember the last insn that needed a replacement.  */
	      for (last_use_insn = v->insn, p = NEXT_INSN (v->insn);
		   p != next->insn;
		   p = next_insn_in_loop (loop, p))
		{
		  rtx note;
    
		  if (GET_RTX_CLASS (GET_CODE (p)) != 'i')
		    continue;
		  if (reg_mentioned_p (old_reg, PATTERN (p)))
		    {
		      last_use_insn = p;
		      if (! validate_replace_rtx (old_reg, dest_reg, p))
			abort ();
		    }
		  for (note = REG_NOTES (p); note; note = XEXP (note, 1))
		    {
		      if (GET_CODE (note) == EXPR_LIST)
			XEXP (note, 0)
			  = replace_rtx (XEXP (note, 0), old_reg, dest_reg);
		    }
		}
    
	      v->last_use = last_use_insn;
	      v->lifetime = INSN_LUID (last_use_insn) - INSN_LUID (v->insn);
	      /* If the lifetime is zero, it means that this register is really
		 a dead store.  So mark this as a giv that can be ignored.
		 This will not prevent the biv from being eliminated.  */
	      if (v->lifetime == 0)
		v->ignore = 1;

	      if (loop_dump_stream)
		fprintf (loop_dump_stream,
			 "Increment %d of biv %d converted to giv %d.\n",
			 INSN_UID (v->insn), old_regno, new_regno);
	    }
	}
    }
  last_increment_giv = max_reg_num () - 1;

  /* Search the loop for general induction variables.  */

  /* A register is a giv if: it is only set once, it is a function of a
     biv and a constant (or invariant), and it is not a biv.  */

  not_every_iteration = 0;
  loop_depth = 0;
  maybe_multiple = 0;
  p = loop_scan_start;
  while (1)
    {
      p = NEXT_INSN (p);
      /* At end of a straight-in loop, we are done.
	 At end of a loop entered at the bottom, scan the top.  */
      if (p == loop_scan_start)
	break;
      if (p == loop_end)
	{
	  if (loop_top != 0)
	    p = loop_top;
	  else
	    break;
	  if (p == loop_scan_start)
	    break;
	}

      /* Look for a general induction variable in a register.  */
      if (GET_CODE (p) == INSN
	  && (set = single_set (p))
	  && GET_CODE (SET_DEST (set)) == REG
	  && ! VARRAY_CHAR (may_not_optimize, REGNO (SET_DEST (set))))
	{
	  rtx src_reg;
	  rtx add_val;
	  rtx mult_val;
	  int benefit;
	  rtx regnote = 0;
	  rtx last_consec_insn;

	  dest_reg = SET_DEST (set);
	  if (REGNO (dest_reg) < FIRST_PSEUDO_REGISTER)
	    continue;

	  if (/* SET_SRC is a giv.  */
	      (general_induction_var (SET_SRC (set), &src_reg, &add_val,
				      &mult_val, 0, &benefit)
	       /* Equivalent expression is a giv.  */
	       || ((regnote = find_reg_note (p, REG_EQUAL, NULL_RTX))
		   && general_induction_var (XEXP (regnote, 0), &src_reg,
					     &add_val, &mult_val, 0,
					     &benefit)))
	      /* Don't try to handle any regs made by loop optimization.
		 We have nothing on them in regno_first_uid, etc.  */
	      && REGNO (dest_reg) < max_reg_before_loop
	      /* Don't recognize a BASIC_INDUCT_VAR here.  */
	      && dest_reg != src_reg
	      /* This must be the only place where the register is set.  */
	      && (VARRAY_INT (n_times_set, REGNO (dest_reg)) == 1
		  /* or all sets must be consecutive and make a giv.  */
		  || (benefit = consec_sets_giv (benefit, p,
						 src_reg, dest_reg,
						 &add_val, &mult_val,
						 &last_consec_insn))))
	    {
	      struct induction *v
		= (struct induction *) alloca (sizeof (struct induction));

	      /* If this is a library call, increase benefit.  */
	      if (find_reg_note (p, REG_RETVAL, NULL_RTX))
		benefit += libcall_benefit (p);

	      /* Skip the consecutive insns, if there are any.  */
	      if (VARRAY_INT (n_times_set, REGNO (dest_reg)) != 1)
		p = last_consec_insn;

	      record_giv (v, p, src_reg, dest_reg, mult_val, add_val, benefit,
			  DEST_REG, not_every_iteration, maybe_multiple,
			  NULL_PTR, loop_start, loop_end);

	    }
	}

#ifndef DONT_REDUCE_ADDR
      /* Look for givs which are memory addresses.  */
      /* This resulted in worse code on a VAX 8600.  I wonder if it
	 still does.  */
      if (GET_CODE (p) == INSN)
	find_mem_givs (PATTERN (p), p, not_every_iteration, maybe_multiple,
		       loop_start, loop_end);
#endif

      /* Update the status of whether giv can derive other givs.  This can
	 change when we pass a label or an insn that updates a biv.  */
      if (GET_CODE (p) == INSN || GET_CODE (p) == JUMP_INSN
	|| GET_CODE (p) == CODE_LABEL)
	update_giv_derive (p);

      /* Past CODE_LABEL, we get to insns that may be executed multiple
	 times.  The only way we can be sure that they can't is if every
	 every jump insn between here and the end of the loop either
	 returns, exits the loop, is a forward jump, or is a jump
	 to the loop start.  */

      if (GET_CODE (p) == CODE_LABEL)
	{
	  rtx insn = p;

	  maybe_multiple = 0;

	  while (1)
	    {
	      insn = NEXT_INSN (insn);
	      if (insn == loop_scan_start)
		break;
	      if (insn == loop_end)
		{
		  if (loop_top != 0)
		    insn = loop_top;
		  else
		    break;
		  if (insn == loop_scan_start)
		    break;
		}

	      if (GET_CODE (insn) == JUMP_INSN
		  && GET_CODE (PATTERN (insn)) != RETURN
		  && (! condjump_p (insn)
		      || (JUMP_LABEL (insn) != 0
			  && JUMP_LABEL (insn) != loop_scan_start
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

      /* Past a jump, we get to insns for which we can't count
	 on whether they will be executed during each iteration.  */
      /* This code appears twice in strength_reduce.  There is also similar
	 code in scan_loop.  */
      if (GET_CODE (p) == JUMP_INSN
	  /* If we enter the loop in the middle, and scan around to the
	     beginning, don't set not_every_iteration for that.
	     This can be any kind of jump, since we want to know if insns
	     will be executed if the loop is executed.  */
	  && ! (JUMP_LABEL (p) == loop_top
		&& ((NEXT_INSN (NEXT_INSN (p)) == loop_end && simplejump_p (p))
		    || (NEXT_INSN (p) == loop_end && condjump_p (p)))))
	{
	  rtx label = 0;

	  /* If this is a jump outside the loop, then it also doesn't
	     matter.  Check to see if the target of this branch is on the
	     loop->exits_labels list.  */
	     
	  for (label = uid_loop[INSN_UID (loop_start)]->exit_labels;
	       label;
	       label = LABEL_NEXTREF (label))
	    if (XEXP (label, 0) == JUMP_LABEL (p))
	      break;

	  if (! label)
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

      /* Unlike in the code motion pass where MAYBE_NEVER indicates that
	 an insn may never be executed, NOT_EVERY_ITERATION indicates whether
	 or not an insn is known to be executed each iteration of the
	 loop, whether or not any iterations are known to occur.

	 Therefore, if we have just passed a label and have no more labels
	 between here and the test insn of the loop, we know these insns
	 will be executed each iteration.  */

      if (not_every_iteration && GET_CODE (p) == CODE_LABEL
	  && no_labels_between_p (p, loop_end)
	  && loop_insn_first_p (p, loop_cont))
	not_every_iteration = 0;
    }

  /* Try to calculate and save the number of loop iterations.  This is
     set to zero if the actual number can not be calculated.  This must
     be called after all giv's have been identified, since otherwise it may
     fail if the iteration variable is a giv.  */

  loop_iterations (loop);

  /* Now for each giv for which we still don't know whether or not it is
     replaceable, check to see if it is replaceable because its final value
     can be calculated.  This must be done after loop_iterations is called,
     so that final_giv_value will work correctly.  */

  for (bl = loop_iv_list; bl; bl = bl->next)
    {
      struct induction *v;

      for (v = bl->giv; v; v = v->next_iv)
	if (! v->replaceable && ! v->not_replaceable)
	  check_final_value (v, loop_start, loop_end, loop_info->n_iterations);
    }

  /* Try to prove that the loop counter variable (if any) is always
     nonnegative; if so, record that fact with a REG_NONNEG note
     so that "decrement and branch until zero" insn can be used.  */
  check_dbra_loop (loop, insn_count);

  /* Create reg_map to hold substitutions for replaceable giv regs.
     Some givs might have been made from biv increments, so look at
     reg_iv_type for a suitable size.  */
  reg_map_size = reg_iv_type->num_elements;
  reg_map = (rtx *) xcalloc (reg_map_size, sizeof (rtx));

  /* Examine each iv class for feasibility of strength reduction/induction
     variable elimination.  */

  for (bl = loop_iv_list; bl; bl = bl->next)
    {
      struct induction *v;
      int benefit;
      int all_reduced;
      rtx final_value = 0;
      unsigned int nregs;

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

      if ((uid_luid[REGNO_LAST_UID (bl->regno)] < INSN_LUID (loop_end)
	   && bl->init_insn
	   && INSN_UID (bl->init_insn) < max_uid_for_loop
	   && uid_luid[REGNO_FIRST_UID (bl->regno)] >= INSN_LUID (bl->init_insn)
#ifdef HAVE_decrement_and_branch_until_zero
	   && ! bl->nonneg
#endif
	   && ! reg_mentioned_p (bl->biv->dest_reg, SET_SRC (bl->init_set)))
	  || ((final_value = final_biv_value (bl, loop_start, loop_end, 
					      loop_info->n_iterations))
#ifdef HAVE_decrement_and_branch_until_zero
	      && ! bl->nonneg
#endif
	      ))
	bl->eliminable = maybe_eliminate_biv (bl, loop_start, loop_end, 0,
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
		       REGNO_FIRST_UID (bl->regno),
		       REGNO_LAST_UID (bl->regno));
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

#ifdef AUTO_INC_DEC
	  /* Attempt to guess whether autoincrement will handle some of the
	     new add insns; if so, increase BENEFIT (undo the subtraction of
	     add_cost that was done above).  */
	  if (v->giv_type == DEST_ADDR
	      && GET_CODE (v->mult_val) == CONST_INT)
	    {
	      if (HAVE_POST_INCREMENT
		  && INTVAL (v->mult_val) == GET_MODE_SIZE (v->mem_mode))
		benefit += add_cost * bl->biv_count;
	      else if (HAVE_PRE_INCREMENT
		       && INTVAL (v->mult_val) == GET_MODE_SIZE (v->mem_mode))
		benefit += add_cost * bl->biv_count;
	      else if (HAVE_POST_DECREMENT
		       && -INTVAL (v->mult_val) == GET_MODE_SIZE (v->mem_mode))
		benefit += add_cost * bl->biv_count;
	      else if (HAVE_PRE_DECREMENT
		       && -INTVAL (v->mult_val) == GET_MODE_SIZE (v->mem_mode))
		benefit += add_cost * bl->biv_count;
	    }
#endif

	  /* If an insn is not to be strength reduced, then set its ignore
	     flag, and clear all_reduced.  */

	  /* A giv that depends on a reversed biv must be reduced if it is
	     used after the loop exit, otherwise, it would have the wrong
	     value after the loop exit.  To make it simple, just reduce all
	     of such giv's whether or not we know they are used after the loop
	     exit.  */

	  if ( ! flag_reduce_all_givs && v->lifetime * threshold * benefit < insn_count
	      && ! bl->reversed )
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

      /* Check for givs whose first use is their definition and whose
	 last use is the definition of another giv.  If so, it is likely
	 dead and should not be used to derive another giv nor to
	 eliminate a biv.  */
      for (v = bl->giv; v; v = v->next_iv)
	{
	  if (v->ignore
	      || (v->same && v->same->ignore))
	    continue;

	  if (v->last_use)
	    {
	      struct induction *v1;

	      for (v1 = bl->giv; v1; v1 = v1->next_iv)
		if (v->last_use == v1->insn)
		  v->maybe_dead = 1;
	    }
	  else if (v->giv_type == DEST_REG
	      && REGNO_FIRST_UID (REGNO (v->dest_reg)) == INSN_UID (v->insn))
	    {
	      struct induction *v1;

	      for (v1 = bl->giv; v1; v1 = v1->next_iv)
		if (REGNO_LAST_UID (REGNO (v->dest_reg)) == INSN_UID (v1->insn))
		  v->maybe_dead = 1;
	    }
	}

      /* Now that we know which givs will be reduced, try to rearrange the
         combinations to reduce register pressure.
         recombine_givs calls find_life_end, which needs reg_iv_type and
	 reg_iv_info to be valid for all pseudos.  We do the necessary
	 reallocation here since it allows to check if there are still
	 more bivs to process.  */
      nregs = max_reg_num ();
      if (nregs > reg_iv_type->num_elements)
	{
	  /* If there are still more bivs to process, allocate some slack
	     space so that we're not constantly reallocating these arrays.  */
	  if (bl->next)
	    nregs += nregs / 4;
	  /* Reallocate reg_iv_type and reg_iv_info.  */
	  VARRAY_GROW (reg_iv_type, nregs);
	  VARRAY_GROW (reg_iv_info, nregs);
	}
      recombine_givs (bl, loop_start, loop_end, unroll_p);

      /* Reduce each giv that we decided to reduce.  */

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

	      if (v->derived_from)
		{
		  struct induction *d = v->derived_from;

		  /* In case d->dest_reg is not replaceable, we have
		     to replace it in v->insn now.  */
		  if (! d->new_reg)
		    d->new_reg = gen_reg_rtx (d->mode);
		  PATTERN (v->insn)
		    = replace_rtx (PATTERN (v->insn), d->dest_reg, d->new_reg);
		  PATTERN (v->insn)
		    = replace_rtx (PATTERN (v->insn), v->dest_reg, v->new_reg);
		  /* For each place where the biv is incremented, add an
		     insn to set the new, reduced reg for the giv.
		     We used to do this only for biv_count != 1, but
		     this fails when there is a giv after a single biv
		     increment, e.g. when the last giv was expressed as
		     pre-decrement.  */
		  for (tv = bl->biv; tv; tv = tv->next_iv)
		    {
		      /* We always emit reduced giv increments before the
			 biv increment when bl->biv_count != 1.  So by
			 emitting the add insns for derived givs after the
			 biv increment, they pick up the updated value of
			 the reduced giv.
			 If the reduced giv is processed with
			 auto_inc_opt == 1, then it is incremented earlier
			 than the biv, hence we'll still pick up the right
			 value.
			 If it's processed with auto_inc_opt == -1,
			 that implies that the biv increment is before the
			 first reduced giv's use.  The derived giv's lifetime
			 is after the reduced giv's lifetime, hence in this
			 case, the biv increment doesn't matter.  */
		      emit_insn_after (copy_rtx (PATTERN (v->insn)), tv->insn);
		    }
		  continue;
		}

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
			    && (INSN_LUID (v->insn) < INSN_LUID (loop_scan_start)
				|| (INSN_LUID (bl->biv->insn)
				    > INSN_LUID (loop_scan_start))))
			   || (INSN_LUID (v->insn) < INSN_LUID (loop_scan_start)
			       && (INSN_LUID (loop_scan_start)
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
			    && GET_RTX_CLASS (GET_CODE (prev)) == 'i'
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
		    emit_iv_add_mult (tv->add_val, v->mult_val,
				      v->new_reg, v->new_reg, insert_before);
		  else /* tv->mult_val == const0_rtx */
		    /* A multiply is acceptable here
		       since this is presumed to be seldom executed.  */
		    emit_iv_add_mult (tv->add_val, v->mult_val,
				      v->add_val, v->new_reg, insert_before);
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
	 else add new move insn "giv_reg = reduced_reg".  */

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
	      if (uid_loop[INSN_UID (loop_start)]->exit_count)
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
	  && maybe_eliminate_biv (bl, loop_start, loop_end, 1,
				  threshold, insn_count))

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
	  if (final_value != 0 && ! bl->reversed)
	    {
	      rtx insert_before;

	      /* If the loop has multiple exits, emit the insn before the
		 loop to ensure that it will always be executed no matter
		 how the loop exits.  Otherwise, emit the insn after the
		 loop, since this is slightly more efficient.  */
	      if (uid_loop[INSN_UID (loop_start)]->exit_count)
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

  for (p = loop_start; p != loop_end; p = NEXT_INSN (p))
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
  if (unroll_p
      || (loop_info->n_iterations > 0
	  && unrolled_insn_copies <= insn_count))
    unroll_loop (loop, insn_count, end_insert_before, 1);

#ifdef HAVE_decrement_and_branch_on_count
  /* Instrument the loop with BCT insn.  */
  if (HAVE_decrement_and_branch_on_count && bct_p
      && flag_branch_on_count_reg)
    insert_bct (loop);
#endif  /* HAVE_decrement_and_branch_on_count */

  if (loop_dump_stream)
    fprintf (loop_dump_stream, "\n");

egress:
  VARRAY_FREE (reg_iv_type);
  VARRAY_FREE (reg_iv_info);
  free (reg_biv_class);
  if (reg_map)
    free (reg_map);
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
find_mem_givs (x, insn, not_every_iteration, maybe_multiple, loop_start,
	       loop_end)
     rtx x;
     rtx insn;
     int not_every_iteration, maybe_multiple;
     rtx loop_start, loop_end;
{
  register int i, j;
  register enum rtx_code code;
  register const char *fmt;

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

	/* This code used to disable creating GIVs with mult_val == 1 and
	   add_val == 0.  However, this leads to lost optimizations when 
	   it comes time to combine a set of related DEST_ADDR GIVs, since
	   this one would not be seen.   */

	if (general_induction_var (XEXP (x, 0), &src_reg, &add_val,
				   &mult_val, 1, &benefit))
	  {
	    /* Found one; record it.  */
	    struct induction *v
	      = (struct induction *) oballoc (sizeof (struct induction));

	    record_giv (v, insn, src_reg, addr_placeholder, mult_val,
			add_val, benefit, DEST_ADDR, not_every_iteration,
			maybe_multiple, &XEXP (x, 0), loop_start, loop_end);

	    v->mem_mode = GET_MODE (x);
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
      find_mem_givs (XEXP (x, i), insn, not_every_iteration, maybe_multiple,
		     loop_start, loop_end);
    else if (fmt[i] == 'E')
      for (j = 0; j < XVECLEN (x, i); j++)
	find_mem_givs (XVECEXP (x, i, j), insn, not_every_iteration,
		       maybe_multiple, loop_start, loop_end);
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
record_biv (v, insn, dest_reg, inc_val, mult_val, location,
	    not_every_iteration, maybe_multiple, multi_insn_incr)
     struct induction *v;
     rtx insn;
     rtx dest_reg;
     rtx inc_val;
     rtx mult_val;
     rtx *location;
     int not_every_iteration;
     int maybe_multiple;
     int multi_insn_incr;
{
  struct iv_class *bl;

  v->insn = insn;
  v->src_reg = dest_reg;
  v->dest_reg = dest_reg;
  v->mult_val = mult_val;
  v->add_val = inc_val;
  v->location = location;
  v->mode = GET_MODE (dest_reg);
  v->always_computable = ! not_every_iteration;
  v->always_executed = ! not_every_iteration;
  v->maybe_multiple = maybe_multiple;
  v->multi_insn_incr = multi_insn_incr;

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
	{
	  fprintf (loop_dump_stream, " const =");
	  fprintf (loop_dump_stream, HOST_WIDE_INT_PRINT_DEC, INTVAL (inc_val));
	  fputc ('\n', loop_dump_stream);
	}
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
	    type, not_every_iteration, maybe_multiple, location, loop_start,
	    loop_end)
     struct induction *v;
     rtx insn;
     rtx src_reg;
     rtx dest_reg;
     rtx mult_val, add_val;
     int benefit;
     enum g_types type;
     int not_every_iteration, maybe_multiple;
     rtx *location;
     rtx loop_start, loop_end;
{
  struct induction *b;
  struct iv_class *bl;
  rtx set = single_set (insn);

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
  v->maybe_multiple = maybe_multiple;
  v->multi_insn_incr = 0;
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
  v->derived_from = 0;
  v->last_use = 0;

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

      v->lifetime = (uid_luid[REGNO_LAST_UID (REGNO (dest_reg))]
		     - uid_luid[REGNO_FIRST_UID (REGNO (dest_reg))]);

      /* If the lifetime is zero, it means that this register is
	 really a dead store.  So mark this as a giv that can be
	 ignored.  This will not prevent the biv from being eliminated.  */
      if (v->lifetime == 0)
	v->ignore = 1;

      REG_IV_TYPE (REGNO (dest_reg)) = GENERAL_INDUCT;
      REG_IV_INFO (REGNO (dest_reg)) = v;
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

      if (REGNO_FIRST_UID (REGNO (dest_reg)) == INSN_UID (insn)
	  /* Previous line always fails if INSN was moved by loop opt.  */
	  && uid_luid[REGNO_LAST_UID (REGNO (dest_reg))] < INSN_LUID (loop_end)
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
		       >= uid_luid[REGNO_FIRST_UID (REGNO (dest_reg))])
		      && (uid_luid[INSN_UID (b->insn)]
			  <= uid_luid[REGNO_LAST_UID (REGNO (dest_reg))])))
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

  /* Record whether the add_val contains a const_int, for later use by
     combine_givs.  */
  {
    rtx tem = add_val;

    v->no_const_addval = 1;
    if (tem == const0_rtx)
      ;
    else if (GET_CODE (tem) == CONST_INT)
      v->no_const_addval = 0;
    else if (GET_CODE (tem) == PLUS)
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
        if (GET_CODE (XEXP (tem, 1)) == CONST_INT)
          v->no_const_addval = 0;
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
      fprintf (loop_dump_stream, " lifetime %d",
	       v->lifetime);

      if (v->replaceable)
 	fprintf (loop_dump_stream, " replaceable");

      if (v->no_const_addval)
	fprintf (loop_dump_stream, " ncav");

      if (GET_CODE (mult_val) == CONST_INT)
	{
	  fprintf (loop_dump_stream, " mult ");
	  fprintf (loop_dump_stream, HOST_WIDE_INT_PRINT_DEC, INTVAL (mult_val));
	}
      else
	{
	  fprintf (loop_dump_stream, " mult ");
	  print_rtl (loop_dump_stream, mult_val);
	}

      if (GET_CODE (add_val) == CONST_INT)
	{
	  fprintf (loop_dump_stream, " add ");
	  fprintf (loop_dump_stream, HOST_WIDE_INT_PRINT_DEC, INTVAL (add_val));
	}
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
check_final_value (v, loop_start, loop_end, n_iterations)
     struct induction *v;
     rtx loop_start, loop_end;
     unsigned HOST_WIDE_INT n_iterations;
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

  if ((final_value = final_giv_value (v, loop_start, loop_end, n_iterations))
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
	      else if (reg_set_p (v->src_reg, PATTERN (p)))
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
		  && ((loop_insn_first_p (JUMP_LABEL (p), v->insn)
		       && loop_insn_first_p (loop_start, JUMP_LABEL (p)))
		      || (loop_insn_first_p (last_giv_use, JUMP_LABEL (p))
			  && loop_insn_first_p (JUMP_LABEL (p), loop_end))))
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
		    tem = simplify_giv_expr (gen_rtx_MULT (giv->mode,
							   biv->add_val,
							   giv->mult_val),
					     &dummy);

		  if (tem && giv->derive_adjustment)
		    tem = simplify_giv_expr
		      (gen_rtx_PLUS (giv->mode, tem, giv->derive_adjustment),
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
basic_induction_var (x, mode, dest_reg, p, level, inc_val, mult_val,
		     location, multi_insn_incr)
     register rtx x;
     enum machine_mode mode;
     rtx dest_reg;
     rtx p;
     int level;
     rtx *inc_val;
     rtx *mult_val;
     rtx **location;
     int *multi_insn_incr;
{
  register enum rtx_code code;
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
      if (invariant_p (arg) != 1)
	return 0;

      *inc_val = convert_modes (GET_MODE (dest_reg), GET_MODE (x), arg, 0);
      *mult_val = const1_rtx;
      *location = argp;
      return 1;

    case SUBREG:
      /* If this is a SUBREG for a promoted variable, check the inner
	 value.  */
      if (SUBREG_PROMOTED_VAR_P (x))
	return basic_induction_var (SUBREG_REG (x), GET_MODE (SUBREG_REG (x)),
				    dest_reg, p, level, 
				    inc_val, mult_val, location,
				    multi_insn_incr);
      return 0;

    case REG:
      /* If this register is assigned in a previous insn, look at its
	 source, but don't go outside the loop or past a label.  */

      insn = p;
      while (1)
	{
	  do {
	    insn = PREV_INSN (insn);
	  } while (insn && GET_CODE (insn) == NOTE
	           && NOTE_LINE_NUMBER (insn) != NOTE_INSN_LOOP_BEG);

          if (!insn)
	    break;
	  set = single_set (insn);
	  if (set == 0)
	    break;

	  if ((SET_DEST (set) == x
	       || (GET_CODE (SET_DEST (set)) == SUBREG
		   && (GET_MODE_SIZE (GET_MODE (SET_DEST (set)))
		       <= UNITS_PER_WORD)
		   && (GET_MODE_CLASS (GET_MODE (SET_DEST (set)))
		       == MODE_INT)
		   && SUBREG_REG (SET_DEST (set)) == x))
	      && basic_induction_var (SET_SRC (set),
				      (GET_MODE (SET_SRC (set)) == VOIDmode
				       ? GET_MODE (x)
				       : GET_MODE (SET_SRC (set))),
				      dest_reg, insn, level,
				      inc_val, mult_val, location,
				      multi_insn_incr))
	    {
	      *multi_insn_incr = 1;
	      return 1;
	    }
	}
      /* ... fall through ...  */

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
      /* convert_modes aborts if we try to convert to or from CCmode, so just
         exclude that case.  It is very unlikely that a condition code value
	 would be a useful iterator anyways.  */
      if (level == 0
	  && GET_MODE_CLASS (mode) != MODE_CC
	  && GET_MODE_CLASS (GET_MODE (dest_reg)) != MODE_CC)
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
				  dest_reg, p, level, inc_val, mult_val,
				  location, multi_insn_incr);

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
	  && XEXP (x, 1) == XEXP (SET_SRC (set), 1)
	  && basic_induction_var (XEXP (SET_SRC (set), 0),
				  GET_MODE (XEXP (x, 0)),
				  dest_reg, insn, level, inc_val, mult_val,
				  location, multi_insn_incr))
	{
	  *multi_insn_incr = 1;
	  return 1;
	}
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
general_induction_var (x, src_reg, add_val, mult_val, is_addr, pbenefit)
     rtx x;
     rtx *src_reg;
     rtx *add_val;
     rtx *mult_val;
     int is_addr;
     int *pbenefit;
{
  rtx orig_x = x;
  char *storage;

  /* If this is an invariant, forget it, it isn't a giv.  */
  if (invariant_p (x) == 1)
    return 0;

  /* See if the expression could be a giv and get its form.
     Mark our place on the obstack in case we don't find a giv.  */
  storage = (char *) oballoc (0);
  *pbenefit = 0;
  x = simplify_giv_expr (x, pbenefit);
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

  if (is_addr)
    {
#ifdef ADDRESS_COST
      *pbenefit += ADDRESS_COST (orig_x) - reg_address_cost;
#else
      *pbenefit += rtx_cost (orig_x, MEM) - reg_address_cost;
#endif
    }
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
static int cmp_combine_givs_stats PARAMS ((const PTR, const PTR));
static int cmp_recombine_givs_stats PARAMS ((const PTR, const PTR));

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
    return NULL_RTX;

  switch (GET_CODE (x))
    {
    case PLUS:
      arg0 = simplify_giv_expr (XEXP (x, 0), benefit);
      arg1 = simplify_giv_expr (XEXP (x, 1), benefit);
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
	      simplify_giv_expr (gen_rtx_PLUS (mode,
					       XEXP (arg0, 0),
					       gen_rtx_PLUS (mode,
							     XEXP (arg0, 1),
							     arg1)),
				 benefit);

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
	    simplify_giv_expr (gen_rtx_PLUS (mode,
					     gen_rtx_PLUS (mode, arg0,
							   XEXP (arg1, 0)),
					     XEXP (arg1, 1)),
			       benefit);

      /* Now must have MULT + MULT.  Distribute if same biv, else not giv.  */
      if (GET_CODE (arg0) != MULT || GET_CODE (arg1) != MULT)
	return NULL_RTX;

      if (!rtx_equal_p (arg0, arg1))
	return NULL_RTX;

      return simplify_giv_expr (gen_rtx_MULT (mode,
					      XEXP (arg0, 0),
					      gen_rtx_PLUS (mode,
							    XEXP (arg0, 1),
							    XEXP (arg1, 1))),
				benefit);

    case MINUS:
      /* Handle "a - b" as "a + b * (-1)".  */
      return simplify_giv_expr (gen_rtx_PLUS (mode,
					      XEXP (x, 0),
					      gen_rtx_MULT (mode,
							    XEXP (x, 1),
							    constm1_rtx)),
				benefit);

    case MULT:
      arg0 = simplify_giv_expr (XEXP (x, 0), benefit);
      arg1 = simplify_giv_expr (XEXP (x, 1), benefit);
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
	  /* invar * invar.  It is a giv, but very few of these will 
	     actually pay off, so limit to simple registers.  */
	  if (GET_CODE (arg1) != CONST_INT)
	    return NULL_RTX;

	  arg0 = XEXP (arg0, 0);
	  if (GET_CODE (arg0) == REG)
	    tem = gen_rtx_MULT (mode, arg0, arg1);
	  else if (GET_CODE (arg0) == MULT
		   && GET_CODE (XEXP (arg0, 0)) == REG
		   && GET_CODE (XEXP (arg0, 1)) == CONST_INT)
	    {
	      tem = gen_rtx_MULT (mode, XEXP (arg0, 0), 
				  GEN_INT (INTVAL (XEXP (arg0, 1))
					   * INTVAL (arg1)));
	    }
	  else
	    return NULL_RTX;
	  return gen_rtx_USE (mode, tem);

	case MULT:
	  /* (a * invar_1) * invar_2.  Associate.  */
	  return simplify_giv_expr (gen_rtx_MULT (mode,
						  XEXP (arg0, 0),
						  gen_rtx_MULT (mode,
								XEXP (arg0, 1),
								arg1)),
				    benefit);

	case PLUS:
	  /* (a + invar_1) * invar_2.  Distribute.  */
	  return simplify_giv_expr (gen_rtx_PLUS (mode,
						  gen_rtx_MULT (mode,
								XEXP (arg0, 0),
								arg1),
						  gen_rtx_MULT (mode,
								XEXP (arg0, 1),
								arg1)),
				    benefit);

	default:
	  abort ();
	}

    case ASHIFT:
      /* Shift by constant is multiply by power of two.  */
      if (GET_CODE (XEXP (x, 1)) != CONST_INT)
	return 0;

      return
	simplify_giv_expr (gen_rtx_MULT (mode,
					 XEXP (x, 0),
					 GEN_INT ((HOST_WIDE_INT) 1
						  << INTVAL (XEXP (x, 1)))),
			   benefit);

    case NEG:
      /* "-a" is "a * (-1)" */
      return simplify_giv_expr (gen_rtx_MULT (mode, XEXP (x, 0), constm1_rtx),
				benefit);

    case NOT:
      /* "~a" is "-a - 1". Silly, but easy.  */
      return simplify_giv_expr (gen_rtx_MINUS (mode,
					       gen_rtx_NEG (mode, XEXP (x, 0)),
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
      switch (REG_IV_TYPE (REGNO (x)))
	{
	case BASIC_INDUCT:
	  return x;
	case GENERAL_INDUCT:
	  {
	    struct induction *v = REG_IV_INFO (REGNO (x));

	    /* Form expression from giv and add benefit.  Ensure this giv
	       can derive another and subtract any needed adjustment if so.  */
	    *benefit += v->benefit;
	    if (v->cant_derive)
	      return 0;

	    tem = gen_rtx_PLUS (mode, gen_rtx_MULT (mode,
						    v->src_reg, v->mult_val),
				v->add_val);

	    if (v->derive_adjustment)
	      tem = gen_rtx_MINUS (mode, tem, v->derive_adjustment);
	    return simplify_giv_expr (tem, benefit);
	  }

	default:
	  /* If it isn't an induction variable, and it is invariant, we
	     may be able to simplify things further by looking through
	     the bits we just moved outside the loop.  */
	  if (invariant_p (x) == 1)
	    {
	      struct movable *m;

	      for (m = the_movables; m ; m = m->next)
		if (rtx_equal_p (x, m->set_dest))
		  {
		    /* Ok, we found a match.  Substitute and simplify.  */

		    /* If we match another movable, we must use that, as 
		       this one is going away.  */
		    if (m->match)
		      return simplify_giv_expr (m->match->set_dest, benefit);

		    /* If consec is non-zero, this is a member of a group of
		       instructions that were moved together.  We handle this
		       case only to the point of seeking to the last insn and
		       looking for a REG_EQUAL.  Fail if we don't find one.  */
		    if (m->consec != 0)
		      {
			int i = m->consec;
			tem = m->insn;
			do { tem = NEXT_INSN (tem); } while (--i > 0);

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
			    tem = simplify_giv_expr (tem, benefit);
			    if (tem)
			      return tem;
			  }
			else if (GET_CODE (tem) == CONST
			    && GET_CODE (XEXP (tem, 0)) == PLUS
			    && GET_CODE (XEXP (XEXP (tem, 0), 0)) == SYMBOL_REF
			    && GET_CODE (XEXP (XEXP (tem, 0), 1)) == CONST_INT)
			  {
			    tem = simplify_giv_expr (XEXP (tem, 0), benefit);
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

      if (invariant_p (x) == 1)
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
consec_sets_giv (first_benefit, p, src_reg, dest_reg,
		 add_val, mult_val, last_consec_insn)
     int first_benefit;
     rtx p;
     rtx src_reg;
     rtx dest_reg;
     rtx *add_val;
     rtx *mult_val;
     rtx *last_consec_insn;
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

  REG_IV_TYPE (REGNO (dest_reg)) = GENERAL_INDUCT;
  REG_IV_INFO (REGNO (dest_reg)) = v;

  count = VARRAY_INT (n_times_set, REGNO (dest_reg)) - 1;

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
	  && (general_induction_var (SET_SRC (set), &src_reg,
				     add_val, mult_val, 0, &benefit)
	      /* Giv created by equivalent expression.  */
	      || ((temp = find_reg_note (p, REG_EQUAL, NULL_RTX))
		  && general_induction_var (XEXP (temp, 0), &src_reg,
					    add_val, mult_val, 0, &benefit)))
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

	  REG_IV_TYPE (REGNO (dest_reg)) = UNKNOWN_INDUCT;
	  return 0;
	}
    }

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
  rtx tem = express_from (g1, g2);

  /* If these givs are identical, they can be combined.  We use the results
     of express_from because the addends are not in a canonical form, so
     rtx_equal_p is a weaker test.  */
  /* But don't combine a DEST_REG giv with a DEST_ADDR giv; we want the
     combination to be the other way round.  */
  if (tem == g1->dest_reg
      && (g1->giv_type == DEST_REG || g2->giv_type == DEST_ADDR))
    {
      return g1->dest_reg;
    }

  /* If G2 can be expressed as a function of G1 and that function is valid
     as an address and no more expensive than using a register for G2,
     the expression of G2 in terms of G1 can be used.  */
  if (tem != NULL_RTX
      && g2->giv_type == DEST_ADDR
      && memory_address_p (g2->mem_mode, tem)
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
      return tem;
    }

  return NULL_RTX;
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
combine_givs (bl)
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
  can_combine = (rtx *) xcalloc (giv_count, giv_count * sizeof(rtx));

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
	  && (single_use = VARRAY_RTX (reg_single_usage, REGNO (g1->dest_reg)))
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
	      can_combine[i*giv_count + j] = this_combine;
	      this_benefit += g2->benefit + extra_benefit;
	    }
	}
      stats[i].total_benefit = this_benefit;
    }

  /* Iterate, combining until we can't.  */
restart:
  qsort (stats, giv_count, sizeof(*stats), cmp_combine_givs_stats);

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
	  if (g1 != g2 && can_combine[i*giv_count + j]
	      /* If it has already been combined, skip.  */
	      && ! g2->same && ! g2->combined_with)
	    {
	      int l;

	      g2->new_reg = can_combine[i*giv_count + j];
	      g2->same = g1;
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
		  if (can_combine[m*giv_count + j])
		    stats[l].total_benefit -= g2->benefit + extra_benefit;
		}

	      if (loop_dump_stream)
		fprintf (loop_dump_stream,
			 "giv at %d combined with giv at %d\n",
			 INSN_UID (g2->insn), INSN_UID (g1->insn));
	    }
	}

      /* To help optimize the next set of combinations, remove
	 this giv from the benefits of other potential mates.  */
      if (g1->combined_with)
	{
	  for (j = 0; j < giv_count; ++j)
	    {
	      int m = stats[j].giv_number;
	      if (can_combine[m*giv_count + i])
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

struct recombine_givs_stats
{
  int giv_number;
  int start_luid, end_luid;
};

/* Used below as comparison function for qsort.  We want a ascending luid
   when scanning the array starting at the end, thus the arguments are
   used in reverse.  */
static int
cmp_recombine_givs_stats (xp, yp)
     const PTR xp;
     const PTR yp;
{
  const struct recombine_givs_stats * const x =
    (const struct recombine_givs_stats *) xp;
  const struct recombine_givs_stats * const y =
    (const struct recombine_givs_stats *) yp;
  int d;
  d = y->start_luid - x->start_luid;
  /* Stabilize the sort.  */
  if (!d)
    d = y->giv_number - x->giv_number;
  return d;
}

/* Scan X, which is a part of INSN, for the end of life of a giv.  Also
   look for the start of life of a giv where the start has not been seen
   yet to unlock the search for the end of its life.
   Only consider givs that belong to BIV.
   Return the total number of lifetime ends that have been found.  */
static int
find_life_end (x, stats, insn, biv)
     rtx x, insn, biv;
     struct recombine_givs_stats *stats;
{
  enum rtx_code code;
  const char *fmt;
  int i, j;
  int retval;

  code = GET_CODE (x);
  switch (code)
    {
    case SET:
      {
	rtx reg = SET_DEST (x);
	if (GET_CODE (reg) == REG)
	  {
	    int regno = REGNO (reg);
	    struct induction *v = REG_IV_INFO (regno);

	    if (REG_IV_TYPE (regno) == GENERAL_INDUCT
		&& ! v->ignore
		&& v->src_reg == biv
		&& stats[v->ix].end_luid <= 0)
	      {
		/* If we see a 0 here for end_luid, it means that we have
		   scanned the entire loop without finding any use at all.
		   We must not predicate this code on a start_luid match
		   since that would make the test fail for givs that have
		   been hoisted out of inner loops.  */
		if (stats[v->ix].end_luid == 0)
		  {
		    stats[v->ix].end_luid = stats[v->ix].start_luid;
		    return 1 + find_life_end (SET_SRC (x), stats, insn, biv);
		  }
		else if (stats[v->ix].start_luid == INSN_LUID (insn))
		  stats[v->ix].end_luid = 0;
	      }
	    return find_life_end (SET_SRC (x), stats, insn, biv);
	  }
	break;
      }
    case REG:
      {
	int regno = REGNO (x);
	struct induction *v = REG_IV_INFO (regno);

	if (REG_IV_TYPE (regno) == GENERAL_INDUCT
	    && ! v->ignore
	    && v->src_reg == biv
	    && stats[v->ix].end_luid == 0)
	  {
	    while (INSN_UID (insn) >= max_uid_for_loop)
	      insn = NEXT_INSN (insn);
	    stats[v->ix].end_luid = INSN_LUID (insn);
	    return 1;
	  }
	return 0;
      }
    case LABEL_REF:
    case CONST_DOUBLE:
    case CONST_INT:
    case CONST:
      return 0;
    default:
      break;
    }
  fmt = GET_RTX_FORMAT (code);
  retval = 0;
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	retval += find_life_end (XEXP (x, i), stats, insn, biv);

      else if (fmt[i] == 'E')
        for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  retval += find_life_end (XVECEXP (x, i, j), stats, insn, biv);
    }
  return retval;
}

/* For each giv that has been combined with another, look if
   we can combine it with the most recently used one instead.
   This tends to shorten giv lifetimes, and helps the next step:
   try to derive givs from other givs.  */
static void
recombine_givs (bl, loop_start, loop_end, unroll_p)
     struct iv_class *bl;
     rtx loop_start, loop_end;
     int unroll_p;
{
  struct induction *v, **giv_array, *last_giv;
  struct recombine_givs_stats *stats;
  int giv_count;
  int i, rescan;
  int ends_need_computing;

  for (giv_count = 0, v = bl->giv; v; v = v->next_iv)
    {
      if (! v->ignore)
	giv_count++;
    }
  giv_array
    = (struct induction **) xmalloc (giv_count * sizeof (struct induction *));
  stats = (struct recombine_givs_stats *) xmalloc (giv_count * sizeof *stats);

  /* Initialize stats and set up the ix field for each giv in stats to name
     the corresponding index into stats.  */
  for (i = 0, v = bl->giv; v; v = v->next_iv)
    {
      rtx p;

      if (v->ignore)
	continue;
      giv_array[i] = v;
      stats[i].giv_number = i;
      /* If this giv has been hoisted out of an inner loop, use the luid of
	 the previous insn.  */
      for (p = v->insn; INSN_UID (p) >= max_uid_for_loop; )
	p = PREV_INSN (p);
      stats[i].start_luid = INSN_LUID (p);
      i++;
    }

  qsort (stats, giv_count, sizeof(*stats), cmp_recombine_givs_stats);

  /* Set up the ix field for each giv in stats to name
     the corresponding index into stats, and
     do the actual most-recently-used recombination.  */
  for (last_giv = 0, i = giv_count - 1; i >= 0; i--)
    {
      v = giv_array[stats[i].giv_number];
      v->ix = i;
      if (v->same)
	{
	  struct induction *old_same = v->same;
	  rtx new_combine;

	  /* combine_givs_p actually says if we can make this transformation.
	     The other tests are here only to avoid keeping a giv alive
	     that could otherwise be eliminated.  */
	  if (last_giv
	      && ((old_same->maybe_dead && ! old_same->combined_with)
		  || ! last_giv->maybe_dead
		  || last_giv->combined_with)
	      && (new_combine = combine_givs_p (last_giv, v)))
	    {
	      old_same->combined_with--;
	      v->new_reg = new_combine;
	      v->same = last_giv;
	      last_giv->combined_with++;
	      /* No need to update lifetimes / benefits here since we have
		 already decided what to reduce.  */

	      if (loop_dump_stream)
		{
		  fprintf (loop_dump_stream,
			   "giv at %d recombined with giv at %d as ",
			   INSN_UID (v->insn), INSN_UID (last_giv->insn));
		  print_rtl (loop_dump_stream, v->new_reg);
		  putc ('\n', loop_dump_stream);
		}
	      continue;
	    }
	  v = v->same;
	}
      else if (v->giv_type != DEST_REG)
	continue;
      if (! last_giv
	  || (last_giv->maybe_dead && ! last_giv->combined_with)
	  || ! v->maybe_dead
	  || v->combined_with)
	last_giv = v;
    }

  ends_need_computing = 0;
  /* For each DEST_REG giv, compute lifetime starts, and try to compute
     lifetime ends from regscan info.  */
  for (i = giv_count - 1; i >= 0; i--)
    {
      v = giv_array[stats[i].giv_number];
      if (v->ignore)
	continue;
      if (v->giv_type == DEST_ADDR)
	{
	  /* Loop unrolling of an inner loop can even create new DEST_REG
	     givs.  */
	  rtx p;
	  for (p = v->insn; INSN_UID (p) >= max_uid_for_loop; )
	    p = PREV_INSN (p);
	  stats[i].start_luid = stats[i].end_luid = INSN_LUID (p);
	  if (p != v->insn)
	    stats[i].end_luid++;
	}
      else /* v->giv_type == DEST_REG */
	{
	  if (v->last_use)
	    {
	      stats[i].start_luid = INSN_LUID (v->insn);
	      stats[i].end_luid = INSN_LUID (v->last_use);
	    }
	  else if (INSN_UID (v->insn) >= max_uid_for_loop)
	    {
	      rtx p;
	      /* This insn has been created by loop optimization on an inner
		 loop.  We don't have a proper start_luid that will match
		 when we see the first set.  But we do know that there will
		 be no use before the set, so we can set end_luid to 0 so that
		 we'll start looking for the last use right away.  */
	      for (p = PREV_INSN (v->insn); INSN_UID (p) >= max_uid_for_loop; )
		p = PREV_INSN (p);
	      stats[i].start_luid = INSN_LUID (p);
	      stats[i].end_luid = 0;
	      ends_need_computing++;
	    }
	  else
	    {
	      int regno = REGNO (v->dest_reg);
	      int count = VARRAY_INT (n_times_set, regno) - 1;
	      rtx p = v->insn;

	      /* Find the first insn that sets the giv, so that we can verify
		 if this giv's lifetime wraps around the loop.  We also need
		 the luid of the first setting insn in order to detect the
		 last use properly.  */
	      while (count)
		{
		  p = prev_nonnote_insn (p);
		  if (reg_set_p (v->dest_reg, p))
		  count--;
		}

	      stats[i].start_luid = INSN_LUID (p);
	      if (stats[i].start_luid > uid_luid[REGNO_FIRST_UID (regno)])
		{
		  stats[i].end_luid = -1;
		  ends_need_computing++;
		}
	      else
		{
		  stats[i].end_luid = uid_luid[REGNO_LAST_UID (regno)];
		  if (stats[i].end_luid > INSN_LUID (loop_end))
		    {
		      stats[i].end_luid = -1;
		      ends_need_computing++;
		    }
		}
	    }
	}
    }

  /* If the regscan information was unconclusive for one or more DEST_REG
     givs, scan the all insn in the loop to find out lifetime ends.  */
  if (ends_need_computing)
    {
      rtx biv = bl->biv->src_reg;
      rtx p = loop_end;

      do
	{
	  if (p == loop_start)
	    p = loop_end;
	  p = PREV_INSN (p);
	  if (GET_RTX_CLASS (GET_CODE (p)) != 'i')
	    continue;
	  ends_need_computing -= find_life_end (PATTERN (p), stats, p, biv);
	}
      while (ends_need_computing);
    }

  /* Set start_luid back to the last insn that sets the giv.  This allows
     more combinations.  */
  for (i = giv_count - 1; i >= 0; i--)
    {
      v = giv_array[stats[i].giv_number];
      if (v->ignore)
	continue;
      if (INSN_UID (v->insn) < max_uid_for_loop)
	stats[i].start_luid = INSN_LUID (v->insn);
    }

  /* Now adjust lifetime ends by taking combined givs into account.  */
  for (i = giv_count - 1; i >= 0; i--)
    {
      unsigned luid;
      int j;

      v = giv_array[stats[i].giv_number];
      if (v->ignore)
	continue;
      if (v->same && ! v->same->ignore)
	{
	  j = v->same->ix;
	  luid = stats[i].start_luid;
	  /* Use unsigned arithmetic to model loop wrap-around.  */
	  if (luid - stats[j].start_luid
	      > (unsigned) stats[j].end_luid - stats[j].start_luid)
	    stats[j].end_luid = luid;
	}
    }

  qsort (stats, giv_count, sizeof(*stats), cmp_recombine_givs_stats);

  /* Try to derive DEST_REG givs from previous DEST_REG givs with the
     same mult_val and non-overlapping lifetime.  This reduces register
     pressure.
     Once we find a DEST_REG giv that is suitable to derive others from,
     we set last_giv to this giv, and try to derive as many other DEST_REG
     givs from it without joining overlapping lifetimes.  If we then
     encounter a DEST_REG giv that we can't derive, we set rescan to the
     index for this giv (unless rescan is already set).
     When we are finished with the current LAST_GIV (i.e. the inner loop
     terminates), we start again with rescan, which then becomes the new
     LAST_GIV.  */
  for (i = giv_count - 1; i >= 0; i = rescan)
    {
      int life_start = 0, life_end = 0;

      for (last_giv = 0, rescan = -1; i >= 0; i--)
	{
	  rtx sum;

	  v = giv_array[stats[i].giv_number];
	  if (v->giv_type != DEST_REG || v->derived_from || v->same)
	    continue;
	  if (! last_giv)
	    {
	      /* Don't use a giv that's likely to be dead to derive
		 others - that would be likely to keep that giv alive.  */
	      if (! v->maybe_dead || v->combined_with)
		{
		  last_giv = v;
		  life_start = stats[i].start_luid;
		  life_end = stats[i].end_luid;
		}
	      continue;
	    }
	  /* Use unsigned arithmetic to model loop wrap around.  */
	  if (((unsigned) stats[i].start_luid - life_start
	       >= (unsigned) life_end - life_start)
	      && ((unsigned) stats[i].end_luid - life_start
		  > (unsigned) life_end - life_start)
	      /*  Check that the giv insn we're about to use for deriving
		  precedes all uses of that giv.  Note that initializing the
		  derived giv would defeat the purpose of reducing register
		  pressure.
		  ??? We could arrange to move the insn.  */
	      && ((unsigned) stats[i].end_luid - INSN_LUID (loop_start)
                  > (unsigned) stats[i].start_luid - INSN_LUID (loop_start))
	      && rtx_equal_p (last_giv->mult_val, v->mult_val)
	      /* ??? Could handle libcalls, but would need more logic.  */
	      && ! find_reg_note (v->insn, REG_RETVAL, NULL_RTX)
	      /* We would really like to know if for any giv that v
		 is combined with, v->insn or any intervening biv increment
		 dominates that combined giv.  However, we
		 don't have this detailed control flow information.
		 N.B. since last_giv will be reduced, it is valid
		 anywhere in the loop, so we don't need to check the
		 validity of last_giv.
		 We rely here on the fact that v->always_executed implies that
		 there is no jump to someplace else in the loop before the
		 giv insn, and hence any insn that is executed before the
		 giv insn in the loop will have a lower luid.  */
	      && (v->always_executed || ! v->combined_with)
	      && (sum = express_from (last_giv, v))
	      /* Make sure we don't make the add more expensive.  ADD_COST
		 doesn't take different costs of registers and constants into
		 account, so compare the cost of the actual SET_SRCs.  */
	      && (rtx_cost (sum, SET)
		  <= rtx_cost (SET_SRC (single_set (v->insn)), SET))
	      /* ??? unroll can't understand anything but reg + const_int
		 sums.  It would be cleaner to fix unroll.  */
	      && ((GET_CODE (sum) == PLUS
		   && GET_CODE (XEXP (sum, 0)) == REG
		   && GET_CODE (XEXP (sum, 1)) == CONST_INT)
		  || ! unroll_p)
	      && validate_change (v->insn, &PATTERN (v->insn),
				  gen_rtx_SET (VOIDmode, v->dest_reg, sum), 0))
	    {
	      v->derived_from = last_giv;
	      life_end = stats[i].end_luid;

	      if (loop_dump_stream)
		{
		  fprintf (loop_dump_stream,
			   "giv at %d derived from %d as ",
			   INSN_UID (v->insn), INSN_UID (last_giv->insn));
		  print_rtl (loop_dump_stream, sum);
		  putc ('\n', loop_dump_stream);
		}
	    }
	  else if (rescan < 0)
	    rescan = i;
	}
    }

  /* Clean up.  */
  free (giv_array);
  free (stats);
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

  /* Increase the lifetime of any invariants moved further in code.  */
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

  /* It is entirely possible that the expansion created lots of new 
     registers.  Iterate over the sequence we just created and 
     record them all.  */

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
  else if (GET_CODE (seq) == SET
	   && GET_CODE (SET_DEST (seq)) == REG)
    record_base_value (REGNO (SET_DEST (seq)), SET_SRC (seq), 0);
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
check_dbra_loop (loop, insn_count)
     struct loop *loop;
     int insn_count;
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
  rtx jump;
  rtx first_compare;
  int compare_and_branch;
  rtx loop_start = loop->start;
  rtx loop_end = loop->end;
  struct loop_info *loop_info = loop->info;

  /* If last insn is a conditional branch, and the insn before tests a
     register value, try to optimize it.  Otherwise, we can't do anything.  */

  jump = PREV_INSN (loop_end);
  comparison = get_condition_for_loop (jump);
  if (comparison == 0)
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

  /* Check all of the bivs to see if the compare uses one of them.
     Skip biv's set more than once because we can't guarantee that
     it will be zero on the last iteration.  Also skip if the biv is
     used between its update and the test insn.  */

  for (bl = loop_iv_list; bl; bl = bl->next)
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
	  REG_NOTES (PREV_INSN (loop_end))
	    = gen_rtx_EXPR_LIST (REG_NONNEG, NULL_RTX,
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
		= gen_rtx_EXPR_LIST (REG_NONNEG, NULL_RTX,
				     REG_NOTES (PREV_INSN (loop_end)));
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
	  && ! uid_loop[INSN_UID (loop_start)]->exit_count)
	{
	  rtx bivreg = regno_reg_rtx[bl->regno];

	  /* If there are no givs for this biv, and the only exit is the
	     fall through at the end of the loop, then
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
		else if ((p == prev_nonnote_insn (prev_nonnote_insn (loop_end))
			  || p == prev_nonnote_insn (loop_end))
			 && reg_mentioned_p (bivreg, PATTERN (p)))
		  {
		    /* If either of these insns uses the biv and sets a pseudo
		       that has more than one usage, then the biv has uses
		       other than counting since it's used to derive a value
		       that is used more than one time.  */
		    int note_set_pseudo_multiple_uses_retval = 0;
		    note_stores (PATTERN (p), note_set_pseudo_multiple_uses,
				 &note_set_pseudo_multiple_uses_retval);
		    if (note_set_pseudo_multiple_uses_retval)
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
	}

      if (no_use_except_counting)
	; /* no need to worry about MEMs.  */
      else if (num_mem_sets <= 1)
	{
	  for (p = loop_start; p != loop_end; p = NEXT_INSN (p))
	    if (GET_RTX_CLASS (GET_CODE (p)) == 'i')
	      num_nonfixed_reads += count_nonfixed_reads (PATTERN (p));

	  /* If the loop has a single store, and the destination address is
	     invariant, then we can't reverse the loop, because this address
	     might then have the wrong value at loop exit.
	     This would work if the source was invariant also, however, in that
	     case, the insn should have been moved out of the loop.  */

	  if (num_mem_sets == 1)
	    {
	      struct induction *v;

	      reversible_mem_store
		= (! unknown_address_altered
		   && ! unknown_constant_address_altered
		   && ! invariant_p (XEXP (XEXP (loop_store_mems, 0), 0)));

	      /* If the store depends on a register that is set after the
		 store, it depends on the initial value, and is thus not
		 reversible.  */
	      for (v = bl->giv; reversible_mem_store && v; v = v->next_iv)
		{
		  if (v->giv_type == DEST_REG
		      && reg_mentioned_p (v->dest_reg,
					 PATTERN (first_loop_store_insn)) 
		      && loop_insn_first_p (first_loop_store_insn, v->insn))
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
	   && ! loop_info->has_call
	   && ! loop_info->has_volatile
	   && reversible_mem_store
	   && (bl->giv_count + bl->biv_count + num_mem_sets
	      + num_movables + compare_and_branch == insn_count)
	   && (bl == loop_iv_list && bl->next == 0))
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
		= (unsigned HOST_WIDE_INT)1 << (comparison_const_width - 1);

	      /* If the comparison value is not a loop invariant, then we
		 can not reverse this loop.

		 ??? If the insns which initialize the comparison value as
		 a whole compute an invariant result, then we could move
		 them out of the loop and proceed with loop reversal.  */
	      if (!invariant_p (comparison_value))
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
	      jump_label = XEXP (SET_SRC (PATTERN (PREV_INSN (loop_end))), 1);
	      if (jump_label == pc_rtx)
		jump_label = XEXP (SET_SRC (PATTERN (PREV_INSN (loop_end))), 2);
	      new_add_val = GEN_INT (- INTVAL (bl->biv->add_val));

	      /* Set start_value; if this is not a CONST_INT, we need
		 to generate a SUB.
		 Initialize biv to start_value before loop start.
		 The old initializing insn will be deleted as a
		 dead store by flow.c.  */
	      if (initial_value == const0_rtx
		  && GET_CODE (comparison_value) == CONST_INT)
		{
		  start_value = GEN_INT (comparison_val - add_adjust);
		  emit_insn_before (gen_move_insn (reg, start_value),
				    loop_start);
		}
	      else if (GET_CODE (initial_value) == CONST_INT)
		{
		  rtx offset = GEN_INT (-INTVAL (initial_value) - add_adjust);
		  enum machine_mode mode = GET_MODE (reg);
		  enum insn_code icode
		    = add_optab->handlers[(int) mode].insn_code;

		  if (! (*insn_data[icode].operand[0].predicate) (reg, mode)
		      || ! ((*insn_data[icode].operand[1].predicate)
			    (comparison_value, mode))
		      || ! ((*insn_data[icode].operand[2].predicate)
			    (offset, mode)))
		    return 0;
		  start_value
		    = gen_rtx_PLUS (mode, comparison_value, offset);
		  emit_insn_before ((GEN_FCN (icode)
				     (reg, comparison_value, offset)),
				    loop_start);
		  if (GET_CODE (comparison) == LE)
		    final_value = gen_rtx_PLUS (mode, comparison_value,
						GEN_INT (add_val));
		}
	      else if (! add_adjust)
		{
		  enum machine_mode mode = GET_MODE (reg);
		  enum insn_code icode
		    = sub_optab->handlers[(int) mode].insn_code;
		  if (! (*insn_data[icode].operand[0].predicate) (reg, mode)
		      || ! ((*insn_data[icode].operand[1].predicate)
			    (comparison_value, mode))
		      || ! ((*insn_data[icode].operand[2].predicate)
			    (initial_value, mode)))
		    return 0;
		  start_value
		    = gen_rtx_MINUS (mode, comparison_value, initial_value);
		  emit_insn_before ((GEN_FCN (icode)
				     (reg, comparison_value, initial_value)),
				    loop_start);
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

	      p = emit_insn_before (tem, bl->biv->insn);
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
	      LABEL_NUSES (XEXP (jump_label, 0)) ++;

	      /* Emit an insn after the end of the loop to set the biv's
		 proper exit value if it is used anywhere outside the loop.  */
	      if ((REGNO_LAST_UID (bl->regno) != INSN_UID (first_compare))
		  || ! bl->init_insn
		  || REGNO_FIRST_UID (bl->regno) != INSN_UID (bl->init_insn))
		emit_insn_after (gen_move_insn (reg, final_value),
				 loop_end);

	      /* Delete compare/branch at end of loop.  */
	      delete_insn (PREV_INSN (loop_end));
	      if (compare_and_branch == 2)
		delete_insn (first_compare);

	      /* Add new compare/branch insn at end of loop.  */
	      start_sequence ();
	      emit_cmp_and_jump_insns (reg, const0_rtx, cmp_code, NULL_RTX,
				       GET_MODE (reg), 0, 0, 
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
		      REG_NOTES (tem) = gen_rtx_EXPR_LIST (REG_NONNEG, NULL_RTX,
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
		if (GET_RTX_CLASS (GET_CODE (p)) == 'i')
		  {
		    rtx *pnote;
		    rtx set = single_set (p);
		    /* If this is a set of a GIV based on the reversed biv, any
		       REG_EQUAL notes should still be correct.  */
		    if (! set
			|| GET_CODE (SET_DEST (set)) != REG
			|| (size_t) REGNO (SET_DEST (set)) >= reg_iv_type->num_elements
			|| REG_IV_TYPE (REGNO (SET_DEST (set))) != GENERAL_INDUCT
			|| REG_IV_INFO (REGNO (SET_DEST (set)))->src_reg != bl->biv->src_reg)
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
   LOOP_START is the first insn of the loop, and END is the end insn.

   If ELIMINATE_P is non-zero, actually do the elimination.

   THRESHOLD and INSN_COUNT are from loop_optimize and are used to
   determine whether invariant insns should be placed inside or at the
   start of the loop.  */

static int
maybe_eliminate_biv (bl, loop_start, loop_end, eliminate_p, threshold,
		     insn_count)
     struct iv_class *bl;
     rtx loop_start;
     rtx loop_end;
     int eliminate_p;
     int threshold, insn_count;
{
  rtx reg = bl->biv->dest_reg;
  rtx p;

  /* Scan all insns in the loop, stopping if we find one that uses the
     biv in a way that we cannot eliminate.  */

  for (p = loop_start; p != loop_end; p = NEXT_INSN (p))
    {
      enum rtx_code code = GET_CODE (p);
      rtx where = threshold >= insn_count ? loop_start : p;

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
		  int regno = REGNO (SET_DEST (set));

		  if (regno < max_reg_before_loop
		      && REG_IV_TYPE (regno) == GENERAL_INDUCT
		      && REG_IV_INFO (regno)->src_reg == bl->biv->src_reg)
		    p = last;
		}
	    }
	}
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

  if (p == loop_end)
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

  for (p = insn, q = reference; ;)
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

  /* If the giv V was derived from another giv, and INSN does
     not occur between the giv insn and the biv insn, then we'd
     have to adjust the value used here.  This is rare, so we don't
     bother to make this possible.  */
  if (giv->derived_from
      && ! (giv->always_executed
	    && loop_insn_first_p (giv->insn, insn)
	    && loop_insn_first_p (insn, biv->insn)))
    return 0;
  if (giv->same
      && giv->same->derived_from
      && ! (giv->same->always_executed
	    && loop_insn_first_p (giv->same->insn, insn)
	    && loop_insn_first_p (insn, biv->insn)))
    return 0;

  return 1;
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
	    if (CONSTANT_P (v->mult_val) && v->mult_val != const0_rtx
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
	    if (CONSTANT_P (v->mult_val) && v->mult_val != const0_rtx
		&& ! v->ignore && ! v->maybe_dead && v->always_computable
		&& v->mode == mode
		&& (GET_CODE (v->add_val) == SYMBOL_REF
		    || GET_CODE (v->add_val) == LABEL_REF
		    || GET_CODE (v->add_val) == CONST
		    || (GET_CODE (v->add_val) == REG
			&& REGNO_POINTER_FLAG (REGNO (v->add_val)))))
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

		emit_insn_before (gen_move_insn (tem, copy_rtx (v->add_val)),
				  where);

		/* Substitute the new register for its invariant value in
		   the compare expression. */
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
	    if (CONSTANT_P (v->mult_val) && INTVAL (v->mult_val) > 0
		&& (GET_CODE (v->add_val) == SYMBOL_REF
		    || GET_CODE (v->add_val) == LABEL_REF
		    || GET_CODE (v->add_val) == CONST
		    || (GET_CODE (v->add_val) == REG
			&& REGNO_POINTER_FLAG (REGNO (v->add_val))))
		&& ! v->ignore && ! v->maybe_dead && v->always_computable
		&& v->mode == mode)
	      {
		if (! biv_elimination_giv_has_0_offset (bl->biv, v, insn))
		  continue;

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
	     Insert insns to calculate new compare value.  
	     ??? Turn this off due to possible overflow.  */

	  for (v = bl->giv; v; v = v->next_iv)
	    if (CONSTANT_P (v->mult_val) && INTVAL (v->mult_val) > 0
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
		 add_val. Insert insns to compute new compare value. 
		 ??? Turn this off due to possible overflow.  */

	      for (v = bl->giv; v; v = v->next_iv)
		if (CONSTANT_P (v->mult_val) && INTVAL (v->mult_val) > 0
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
	      || REG_IV_TYPE (REGNO (arg)) != BASIC_INDUCT)
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
		    if (! biv_elimination_giv_has_0_offset (bl->biv, v, insn))
		      continue;

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
  struct iv_class *bl;

  if (GET_CODE (dest) != REG
      || REGNO (dest) >= max_reg_before_loop
      || REG_IV_TYPE (REGNO (dest)) != BASIC_INDUCT)
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
      && uid_luid[REGNO_LAST_UID (REGNO (x))] < uid_luid[INSN_UID (insn)])
    REGNO_LAST_UID (REGNO (x)) = INSN_UID (insn);
  else
    {
      register int i, j;
      register const char *fmt = GET_RTX_FORMAT (GET_CODE (x));
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
  enum machine_mode mode;

  /* If this is not a standard conditional jump, we can't parse it.  */
  if (GET_CODE (jump) != JUMP_INSN
      || ! condjump_p (jump) || simplejump_p (jump))
    return 0;

  code = GET_CODE (XEXP (SET_SRC (PATTERN (jump)), 0));
  mode = GET_MODE (XEXP (SET_SRC (PATTERN (jump)), 0));
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
      if (rtx_equal_p (SET_DEST (set), op0))
	{
	  enum machine_mode inner_mode = GET_MODE (SET_SRC (set));

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
	      if (code == UNKNOWN)
		return 0;
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
	  if ((unsigned HOST_WIDE_INT) const_val != max_val >> 1)
	    code = LT,	op1 = GEN_INT (const_val + 1);
	  break;

	/* When cross-compiling, const_val might be sign-extended from
	   BITS_PER_WORD to HOST_BITS_PER_WIDE_INT */
	case GE:
	  if ((HOST_WIDE_INT) (const_val & max_val)
	      != (((HOST_WIDE_INT) 1
		   << (GET_MODE_BITSIZE (GET_MODE (op0)) - 1))))
	    code = GT, op1 = GEN_INT (const_val - 1);
	  break;

	case LEU:
	  if (uconst_val < max_val)
	    code = LTU, op1 = GEN_INT (uconst_val + 1);
	  break;

	case GEU:
	  if (uconst_val != 0)
	    code = GTU, op1 = GEN_INT (uconst_val - 1);
	  break;

	default:
	  break;
	}
    }

  /* If this was floating-point and we reversed anything other than an
     EQ or NE or (UN)ORDERED, return zero.  */
  if (TARGET_FLOAT_FORMAT == IEEE_FLOAT_FORMAT
      && did_reverse_condition
      && code != NE && code != EQ && code != UNORDERED && code != ORDERED
      && ! flag_fast_math
      && GET_MODE_CLASS (GET_MODE (op0)) == MODE_FLOAT)
    return 0;

#ifdef HAVE_cc0
  /* Never return CC0; return zero instead.  */
  if (op0 == cc0_rtx)
    return 0;
#endif

  return gen_rtx_fmt_ee (code, VOIDmode, op0, op1);
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

  return gen_rtx_fmt_ee (swap_condition (GET_CODE (comparison)), VOIDmode,
			 XEXP (comparison, 1), XEXP (comparison, 0));
}

#ifdef HAVE_decrement_and_branch_on_count
/* Instrument loop for insertion of bct instruction.  We distinguish between
   loops with compile-time bounds and those with run-time bounds. 
   Information from loop_iterations() is used to compute compile-time bounds.
   Run-time bounds should use loop preconditioning, but currently ignored.
 */

static void
insert_bct (loop)
     struct loop *loop;
{
  unsigned HOST_WIDE_INT n_iterations;
  rtx loop_start = loop->start;
  rtx loop_end = loop->end;
  struct loop_info *loop_info = loop->info;  
  int loop_num = loop->num;

#if 0
  int increment_direction, compare_direction;
  /* If the loop condition is <= or >=, the number of iteration
      is 1 more than the range of the bounds of the loop.  */
  int add_iteration = 0;
  enum machine_mode loop_var_mode = word_mode;
#endif

  /* It's impossible to instrument a competely unrolled loop.  */
  if (loop_info->unroll_number == loop_info->n_iterations)
    return;

  /* Make sure that the count register is not in use.  */
  if (loop_info->used_count_register)
    {
      if (loop_dump_stream)
	fprintf (loop_dump_stream,
		 "insert_bct %d: BCT instrumentation failed: count register already in use\n",
		 loop_num);
      return;
    }

  /* Make sure that the function has no indirect jumps.  */
  if (indirect_jump_in_function)
    {
      if (loop_dump_stream)
	fprintf (loop_dump_stream,
		 "insert_bct %d: BCT instrumentation failed: indirect jump in function\n",
		 loop_num);
      return;
    }

  /* Make sure that the last loop insn is a conditional jump.  */
  if (GET_CODE (PREV_INSN (loop_end)) != JUMP_INSN
      || ! condjump_p (PREV_INSN (loop_end))
      || simplejump_p (PREV_INSN (loop_end)))
    {
      if (loop_dump_stream)
	fprintf (loop_dump_stream,
		 "insert_bct %d: BCT instrumentation failed: invalid jump at loop end\n",
		 loop_num);
      return;
    }

  /* Make sure that the loop does not contain a function call
     (the count register might be altered by the called function).  */
  if (loop_info->has_call)
    {
      if (loop_dump_stream)
	fprintf (loop_dump_stream,
		 "insert_bct %d: BCT instrumentation failed: function call in loop\n",
		 loop_num);
      return;
    }

  /* Make sure that the loop does not jump via a table.
     (the count register might be used to perform the branch on table).  */
  if (loop_info->has_tablejump)
    {
      if (loop_dump_stream)
	fprintf (loop_dump_stream,
		 "insert_bct %d: BCT instrumentation failed: computed branch in the loop\n",
		 loop_num);
      return;
    }

  /* Account for loop unrolling in instrumented iteration count.  */
  if (loop_info->unroll_number > 1)
    n_iterations = loop_info->n_iterations / loop_info->unroll_number;
  else
    n_iterations = loop_info->n_iterations;

  if (n_iterations != 0 && n_iterations < 3)
    {
      /* Allow an enclosing outer loop to benefit if possible.  */
      if (loop_dump_stream)
	fprintf (loop_dump_stream,
		 "insert_bct %d: Too few iterations to benefit from BCT optimization\n",
		 loop_num);
      return;
    }

  /* Try to instrument the loop.  */

  /* Handle the simpler case, where the bounds are known at compile time.  */
  if (n_iterations > 0)
    {
      struct loop *outer_loop;
      struct loop_info *outer_loop_info;

      /* Mark all enclosing loops that they cannot use count register.  */
      for (outer_loop = loop; outer_loop; outer_loop = outer_loop->outer)
	{
	  outer_loop_info = outer_loop->info;
	  outer_loop_info->used_count_register = 1;
	}
      instrument_loop_bct (loop_start, loop_end, GEN_INT (n_iterations));
      return;
    }

  /* Handle the more complex case, that the bounds are NOT known
     at compile time.  In this case we generate run_time calculation
     of the number of iterations.  */

  if (loop_info->iteration_var == 0)
    {
      if (loop_dump_stream)
	fprintf (loop_dump_stream,
		 "insert_bct %d: BCT Runtime Instrumentation failed: no loop iteration variable found\n",
		 loop_num);
      return;
    }

  if (GET_MODE_CLASS (GET_MODE (loop_info->iteration_var)) != MODE_INT
      || GET_MODE_SIZE (GET_MODE (loop_info->iteration_var)) != UNITS_PER_WORD)
    {
      if (loop_dump_stream)
	fprintf (loop_dump_stream,
		 "insert_bct %d: BCT Runtime Instrumentation failed: loop variable not integer\n",
		 loop_num);
      return;
    }

  /* With runtime bounds, if the compare is of the form '!=' we give up */
  if (loop_info->comparison_code == NE)
    {
      if (loop_dump_stream)
	fprintf (loop_dump_stream,
		 "insert_bct %d: BCT Runtime Instrumentation failed: runtime bounds with != comparison\n",
		 loop_num);
      return;
    }
/* Use common loop preconditioning code instead.  */
#if 0
  else
    {
      /* We rely on the existence of run-time guard to ensure that the
	 loop executes at least once.  */
      rtx sequence;
      rtx iterations_num_reg;

      unsigned HOST_WIDE_INT increment_value_abs
	= INTVAL (increment) * increment_direction;

      /* make sure that the increment is a power of two, otherwise (an
	 expensive) divide is needed.  */
      if (exact_log2 (increment_value_abs) == -1)
	{
	  if (loop_dump_stream)
	    fprintf (loop_dump_stream,
		     "insert_bct: not instrumenting BCT because the increment is not power of 2\n");
	  return;
	}

      /* compute the number of iterations */
      start_sequence ();
      {
	rtx temp_reg;

	/* Again, the number of iterations is calculated by:
	   ;
	   ;                  compare-val - initial-val + (increment -1) + additional-iteration
	   ; num_iterations = -----------------------------------------------------------------
	   ;                                           increment
	 */
	/* ??? Do we have to call copy_rtx here before passing rtx to
	   expand_binop?  */
	if (compare_direction > 0)
	  {
	    /* <, <= :the loop variable is increasing */
	    temp_reg = expand_binop (loop_var_mode, sub_optab,
				     comparison_value, initial_value,
				     NULL_RTX, 0, OPTAB_LIB_WIDEN);
	  }
	else
	  {
	    temp_reg = expand_binop (loop_var_mode, sub_optab,
				     initial_value, comparison_value,
				     NULL_RTX, 0, OPTAB_LIB_WIDEN);
	  }

	if (increment_value_abs - 1 + add_iteration != 0)
	  temp_reg = expand_binop (loop_var_mode, add_optab, temp_reg,
				   GEN_INT (increment_value_abs - 1
					    + add_iteration),
				   NULL_RTX, 0, OPTAB_LIB_WIDEN);

	if (increment_value_abs != 1)
	  iterations_num_reg = expand_binop (loop_var_mode, asr_optab,
					     temp_reg,
					     GEN_INT (exact_log2 (increment_value_abs)),
					     NULL_RTX, 0, OPTAB_LIB_WIDEN);
	else
	  iterations_num_reg = temp_reg;
      }
      sequence = gen_sequence ();
      end_sequence ();
      emit_insn_before (sequence, loop_start);
      instrument_loop_bct (loop_start, loop_end, iterations_num_reg);
    }

  return;
#endif /* Complex case */
}

/* Instrument loop by inserting a bct in it as follows:
   1. A new counter register is created.
   2. In the head of the loop the new variable is initialized to the value
   passed in the loop_num_iterations parameter.
   3. At the end of the loop, comparison of the register with 0 is generated.
   The created comparison follows the pattern defined for the
   decrement_and_branch_on_count insn, so this insn will be generated.
   4. The branch on the old variable are deleted.  The compare must remain
   because it might be used elsewhere.  If the loop-variable or condition
   register are used elsewhere, they will be eliminated by flow.  */

static void
instrument_loop_bct (loop_start, loop_end, loop_num_iterations)
     rtx loop_start, loop_end;
     rtx loop_num_iterations;
{
  rtx counter_reg;
  rtx start_label;
  rtx sequence;

  if (HAVE_decrement_and_branch_on_count)
    {
      if (loop_dump_stream)
	{
	  fputs ("instrument_bct: Inserting BCT (", loop_dump_stream);
	  if (GET_CODE (loop_num_iterations) == CONST_INT)
	    fprintf (loop_dump_stream, HOST_WIDE_INT_PRINT_DEC,
		     INTVAL (loop_num_iterations));
	  else
	    fputs ("runtime", loop_dump_stream);
	  fputs (" iterations)", loop_dump_stream);
	}

      /* Discard original jump to continue loop.  Original compare result
	 may still be live, so it cannot be discarded explicitly.  */
      delete_insn (PREV_INSN (loop_end));

      /* Insert the label which will delimit the start of the loop.  */
      start_label = gen_label_rtx ();
      emit_label_after (start_label, loop_start);

      /* Insert initialization of the count register into the loop header.  */
      start_sequence ();
      counter_reg = gen_reg_rtx (word_mode);
      emit_insn (gen_move_insn (counter_reg, loop_num_iterations));
      sequence = gen_sequence ();
      end_sequence ();
      emit_insn_before (sequence, loop_start);

      /* Insert new comparison on the count register instead of the
	 old one, generating the needed BCT pattern (that will be
	 later recognized by assembly generation phase).  */
      emit_jump_insn_before (gen_decrement_and_branch_on_count (counter_reg,
								start_label),
			     loop_end);
      LABEL_NUSES (start_label)++;
    }

}
#endif /* HAVE_decrement_and_branch_on_count */

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
  for (i = 0; i < loop_mems_idx; ++i)
    if (rtx_equal_p (m, loop_mems[i].mem)) 
      {
	if (GET_MODE (m) != GET_MODE (loop_mems[i].mem))
	  /* The modes of the two memory accesses are different.  If
	     this happens, something tricky is going on, and we just
	     don't optimize accesses to this MEM.  */
	  loop_mems[i].optimize = 0;

	return 0;
      }

  /* Resize the array, if necessary.  */
  if (loop_mems_idx == loop_mems_allocated) 
    {
      if (loop_mems_allocated != 0)
	loop_mems_allocated *= 2;
      else
	loop_mems_allocated = 32;

      loop_mems = (loop_mem_info*) 
	xrealloc (loop_mems,
		  loop_mems_allocated * sizeof (loop_mem_info)); 
    }

  /* Actually insert the MEM.  */
  loop_mems[loop_mems_idx].mem = m;
  /* We can't hoist this MEM out of the loop if it's a BLKmode MEM
     because we can't put it in a register.  We still store it in the
     table, though, so that if we see the same address later, but in a
     non-BLK mode, we'll not think we can optimize it at that point.  */
  loop_mems[loop_mems_idx].optimize = (GET_MODE (m) != BLKmode);
  loop_mems[loop_mems_idx].reg = NULL_RTX;
  ++loop_mems_idx;

  return 0;
}

/* Like load_mems, but also ensures that SET_IN_LOOP,
   MAY_NOT_OPTIMIZE, REG_SINGLE_USAGE, and INSN_COUNT have the correct
   values after load_mems.  */

static void
load_mems_and_recount_loop_regs_set (loop, insn_count)
     const struct loop *loop;
     int *insn_count;
{
  int nregs = max_reg_num ();

  load_mems (loop);
  
  /* Recalculate set_in_loop and friends since load_mems may have
     created new registers.  */
  if (max_reg_num () > nregs)
    {
      int i;
      int old_nregs;

      old_nregs = nregs;
      nregs = max_reg_num ();

      if ((unsigned) nregs > set_in_loop->num_elements)
	{
	  /* Grow all the arrays.  */
	  VARRAY_GROW (set_in_loop, nregs);
	  VARRAY_GROW (n_times_set, nregs);
	  VARRAY_GROW (may_not_optimize, nregs);
	  VARRAY_GROW (reg_single_usage, nregs);
	}
      /* Clear the arrays */
      bzero ((char *) &set_in_loop->data, nregs * sizeof (int));
      bzero ((char *) &may_not_optimize->data, nregs * sizeof (char));
      bzero ((char *) &reg_single_usage->data, nregs * sizeof (rtx));

      count_loop_regs_set (loop->top ? loop->top : loop->start, loop->end,
			   may_not_optimize, reg_single_usage,
			   insn_count, nregs); 

      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	{
	  VARRAY_CHAR (may_not_optimize, i) = 1;
	  VARRAY_INT (set_in_loop, i) = 1;
	}
      
#ifdef AVOID_CCMODE_COPIES
      /* Don't try to move insns which set CC registers if we should not
	 create CCmode register copies.  */
      for (i = max_reg_num () - 1; i >= FIRST_PSEUDO_REGISTER; i--)
	if (GET_MODE_CLASS (GET_MODE (regno_reg_rtx[i])) == MODE_CC)
	  VARRAY_CHAR (may_not_optimize, i) = 1;
#endif

      /* Set n_times_set for the new registers.  */
      bcopy ((char *) (&set_in_loop->data.i[0] + old_nregs),
	     (char *) (&n_times_set->data.i[0] + old_nregs),
	     (nregs - old_nregs) * sizeof (int));
    }
}

/* Move MEMs into registers for the duration of the loop.  */

static void
load_mems (loop)
     const struct loop *loop;
{
  int maybe_never = 0;
  int i;
  rtx p;
  rtx label = NULL_RTX;
  rtx end_label = NULL_RTX;
  /* Nonzero if the next instruction may never be executed.  */
  int next_maybe_never = 0;
  int last_max_reg = max_reg_num ();

  if (loop_mems_idx == 0)
    return;

  /* Check to see if it's possible that some instructions in the
     loop are never executed.  */
  for (p = next_insn_in_loop (loop, loop->scan_start); 
       p != NULL_RTX && ! maybe_never; 
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
		     && simplejump_p (p)))
	{
	  if (!condjump_p (p))
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

  /* Actually move the MEMs.  */
  for (i = 0; i < loop_mems_idx; ++i) 
    {
      regset_head copies;
      int written = 0;
      rtx reg;
      rtx mem = loop_mems[i].mem;
      rtx mem_list_entry;

      if (MEM_VOLATILE_P (mem) 
	  || invariant_p (XEXP (mem, 0)) != 1)
	/* There's no telling whether or not MEM is modified.  */
	loop_mems[i].optimize = 0;

      /* Go through the MEMs written to in the loop to see if this
	 one is aliased by one of them.  */
      mem_list_entry = loop_store_mems;
      while (mem_list_entry)
	{
	  if (rtx_equal_p (mem, XEXP (mem_list_entry, 0)))
	    written = 1;
	  else if (true_dependence (XEXP (mem_list_entry, 0), VOIDmode,
				    mem, rtx_varies_p))
	    {
	      /* MEM is indeed aliased by this store.  */
	      loop_mems[i].optimize = 0;
	      break;
	    }
	  mem_list_entry = XEXP (mem_list_entry, 1);
	}

      if (flag_float_store && written
	  && GET_MODE_CLASS (GET_MODE (mem)) == MODE_FLOAT)
	loop_mems[i].optimize = 0;
  
      /* If this MEM is written to, we must be sure that there
	 are no reads from another MEM that aliases this one.  */ 
      if (loop_mems[i].optimize && written)
	{
	  int j;

	  for (j = 0; j < loop_mems_idx; ++j)
	    {
	      if (j == i)
		continue;
	      else if (true_dependence (mem,
					VOIDmode,
					loop_mems[j].mem,
					rtx_varies_p))
		{
		  /* It's not safe to hoist loop_mems[i] out of
		     the loop because writes to it might not be
		     seen by reads from loop_mems[j].  */
		  loop_mems[i].optimize = 0;
		  break;
		}
	    }
	}

      if (maybe_never && may_trap_p (mem))
	/* We can't access the MEM outside the loop; it might
	   cause a trap that wouldn't have happened otherwise.  */
	loop_mems[i].optimize = 0;
	  
      if (!loop_mems[i].optimize)
	/* We thought we were going to lift this MEM out of the
	   loop, but later discovered that we could not.  */
	continue;

      INIT_REG_SET (&copies);

      /* Allocate a pseudo for this MEM.  We set REG_USERVAR_P in
	 order to keep scan_loop from moving stores to this MEM
	 out of the loop just because this REG is neither a
	 user-variable nor used in the loop test.  */
      reg = gen_reg_rtx (GET_MODE (mem));
      REG_USERVAR_P (reg) = 1;
      loop_mems[i].reg = reg;

      /* Now, replace all references to the MEM with the
	 corresponding pesudos.  */
      maybe_never = 0;
      for (p = next_insn_in_loop (loop, loop->scan_start);
	   p != NULL_RTX;
	   p = next_insn_in_loop (loop, p))
	{
	  rtx_and_int ri;
	  rtx set;

	  if (GET_RTX_CLASS (GET_CODE (p)) == 'i')
	    {
	      /* See if this copies the mem into a register that isn't
		 modified afterwards.  We'll try to do copy propagation
		 a little further on.  */
	      set = single_set (p);
	      if (set
		  /* @@@ This test is _way_ too conservative.  */
		  && ! maybe_never
		  && GET_CODE (SET_DEST (set)) == REG
		  && REGNO (SET_DEST (set)) >= FIRST_PSEUDO_REGISTER
		  && REGNO (SET_DEST (set)) < last_max_reg
		  && VARRAY_INT (n_times_set, REGNO (SET_DEST (set))) == 1
		  && rtx_equal_p (SET_SRC (set), loop_mems[i].mem))
		SET_REGNO_REG_SET (&copies, REGNO (SET_DEST (set)));
	      ri.r = p;
	      ri.i = i;
	      for_each_rtx (&p, replace_loop_mem, &ri);
	    }

	  if (GET_CODE (p) == CODE_LABEL
	      || GET_CODE (p) == JUMP_INSN)
	    maybe_never = 1;
	}

      if (! apply_change_group ())
	/* We couldn't replace all occurrences of the MEM.  */
	loop_mems[i].optimize = 0;
      else
	{
	  int j;
	  rtx set;

	  /* Load the memory immediately before START, which is
	     the NOTE_LOOP_BEG.  */
	  set = gen_move_insn (reg, mem);
	  emit_insn_before (set, loop->start);

	  if (written)
	    {
	      if (label == NULL_RTX)
		{
		  /* We must compute the former
		     right-after-the-end label before we insert
		     the new one.  */
		  end_label = next_label (loop->end);
		  label = gen_label_rtx ();
		  emit_label_after (label, loop->end);
		}

	      /* Store the memory immediately after END, which is
		 the NOTE_LOOP_END.  */
	      set = gen_move_insn (copy_rtx (mem), reg); 
	      emit_insn_after (set, label);
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
	    (&copies, FIRST_PSEUDO_REGISTER, j,
	     {
	       try_copy_prop (loop, loop_mems[i].reg, j);
	     });
	  CLEAR_REG_SET (&copies);
	}
    }

  if (label != NULL_RTX)
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
  struct note_reg_stored_arg *t = (struct note_reg_stored_arg *)arg;
  if (t->reg == x)
    t->set_seen = 1;
}

/* Try to replace every occurrence of pseudo REGNO with REPLACEMENT.
   There must be exactly one insn that sets this pseudo; it will be
   deleted if all replacements succeed and we can prove that the register
   is not used after the loop.
   The arguments SCAN_START, LOOP_TOP and END are as in load_mems.  */
static void
try_copy_prop (loop, replacement, regno)
     const struct loop *loop;
     rtx replacement;
     int regno;
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

      if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
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
	  rtx array[3];
	  array[0] = reg_rtx;
	  array[1] = replacement;
	  array[2] = insn;

	  for_each_rtx (&insn, replace_loop_reg, array);
	  if (REGNO_LAST_UID (regno) == INSN_UID (insn))
	    replaced_last = 1;

	  /* Stop replacing when REPLACEMENT is modified.  */
	  arg.reg = replacement;
	  arg.set_seen = 0;
	  note_stores (PATTERN (insn), note_reg_stored, &arg);
	  if (arg.set_seen)
	    break;
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
	  PUT_CODE (init_insn, NOTE);
	  NOTE_LINE_NUMBER (init_insn) = NOTE_INSN_DELETED;
	  if (loop_dump_stream)
	    fprintf (loop_dump_stream, ", deleting init_insn (%d)",
		     INSN_UID (init_insn));
	}
      if (loop_dump_stream)
	fprintf (loop_dump_stream, ".\n");
    }
}

/* Replace MEM with its associated pseudo register.  This function is
   called from load_mems via for_each_rtx.  DATA is actually an
   rtx_and_int * describing the instruction currently being scanned
   and the MEM we are currently replacing.  */

static int
replace_loop_mem (mem, data)
     rtx *mem;
     void *data;
{
  rtx_and_int *ri; 
  rtx insn;
  int i;
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

  ri = (rtx_and_int*) data;
  i = ri->i;

  if (!rtx_equal_p (loop_mems[i].mem, m))
    /* This is not the MEM we are currently replacing.  */
    return 0;

  insn = ri->r;

  /* Actually replace the MEM.  */
  validate_change (insn, mem, loop_mems[i].reg, 1);

  return 0;
}

/* Replace one register with another.  Called through for_each_rtx; PX points
   to the rtx being scanned.  DATA is actually an array of three rtx's; the
   first one is the one to be replaced, and the second one the replacement.
   The third one is the current insn.  */

static int
replace_loop_reg (px, data)
     rtx *px;
     void *data;
{
  rtx x = *px;
  rtx *array = (rtx *)data;

  if (x == NULL_RTX)
    return 0;

  if (x == array[0])
    validate_change (array[2], px, array[1], 1);

  return 0;
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
  rtx old_label = ((rtx_pair*) data)->r1;
  rtx new_label = ((rtx_pair*) data)->r2;

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
