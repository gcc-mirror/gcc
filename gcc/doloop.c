/* Perform doloop optimizations
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.
   Contributed by Michael P. Hayes (m.hayes@elec.canterbury.ac.nz)

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

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "flags.h"
#include "expr.h"
#include "loop.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "toplev.h"
#include "tm_p.h"


/* This module is used to modify loops with a determinable number of
   iterations to use special low-overhead looping instructions.

   It first validates whether the loop is well behaved and has a
   determinable number of iterations (either at compile or run-time).
   It then modifies the loop to use a low-overhead looping pattern as
   follows:

   1. A pseudo register is allocated as the loop iteration counter.

   2. The number of loop iterations is calculated and is stored
      in the loop counter.

   3. At the end of the loop, the jump insn is replaced by the
      doloop_end pattern.  The compare must remain because it might be
      used elsewhere.  If the loop-variable or condition register are
      used elsewhere, they will be eliminated by flow.

   4. An optional doloop_begin pattern is inserted at the top of the
      loop.
*/


#ifdef HAVE_doloop_end

static rtx doloop_condition_get
  PARAMS ((rtx));
static unsigned HOST_WIDE_INT doloop_iterations_max
  PARAMS ((const struct loop_info *, enum machine_mode, int));
static int doloop_valid_p
  PARAMS ((const struct loop *, rtx));
static int doloop_modify
  PARAMS ((const struct loop *, rtx, rtx, rtx, rtx, rtx));
static int doloop_modify_runtime
  PARAMS ((const struct loop *, rtx, rtx, rtx, enum machine_mode, rtx));


/* Return the loop termination condition for PATTERN or zero
   if it is not a decrement and branch jump insn.  */
static rtx
doloop_condition_get (pattern)
     rtx pattern;
{
  rtx cmp;
  rtx inc;
  rtx reg;
  rtx condition;

  /* The canonical doloop pattern we expect is:

     (parallel [(set (pc) (if_then_else (condition)
                                        (label_ref (label))
                                        (pc)))
                (set (reg) (plus (reg) (const_int -1)))
                (additional clobbers and uses)])

     Some machines (IA-64) make the decrement conditional on
     the condition as well, so we don't bother verifying the
     actual decrement.  In summary, the branch must be the
     first entry of the parallel (also required by jump.c),
     and the second entry of the parallel must be a set of
     the loop counter register.  */

  if (GET_CODE (pattern) != PARALLEL)
    return 0;

  cmp = XVECEXP (pattern, 0, 0);
  inc = XVECEXP (pattern, 0, 1);

  /* Check for (set (reg) (something)).  */
  if (GET_CODE (inc) != SET || ! REG_P (SET_DEST (inc)))
    return 0;

  /* Extract loop counter register.  */
  reg = SET_DEST (inc);

  /* Check for (set (pc) (if_then_else (condition)
                                       (label_ref (label))
                                       (pc))).  */
  if (GET_CODE (cmp) != SET
      || SET_DEST (cmp) != pc_rtx
      || GET_CODE (SET_SRC (cmp)) != IF_THEN_ELSE
      || GET_CODE (XEXP (SET_SRC (cmp), 1)) != LABEL_REF
      || XEXP (SET_SRC (cmp), 2) != pc_rtx)
    return 0;

  /* Extract loop termination condition.  */
  condition = XEXP (SET_SRC (cmp), 0);

  if ((GET_CODE (condition) != GE && GET_CODE (condition) != NE)
      || GET_CODE (XEXP (condition, 1)) != CONST_INT)
    return 0;

  if (XEXP (condition, 0) == reg)
    return condition;

  if (GET_CODE (XEXP (condition, 0)) == PLUS
      && XEXP (XEXP (condition, 0), 0) == reg)
    return condition;

  /* ??? If a machine uses a funny comparison, we could return a
     canonicalised form here.  */

  return 0;
}


/* Return an estimate of the maximum number of loop iterations for the
   loop specified by LOOP or zero if the loop is not normal.
   MODE is the mode of the iteration count and NONNEG is non-zero if
   the the iteration count has been proved to be non-negative.  */
static unsigned HOST_WIDE_INT
doloop_iterations_max (loop_info, mode, nonneg)
     const struct loop_info *loop_info;
     enum machine_mode mode;
     int nonneg;
{
  unsigned HOST_WIDE_INT n_iterations_max;
  enum rtx_code code;
  rtx min_value;
  rtx max_value;
  HOST_WIDE_INT abs_inc;
  int neg_inc;

  neg_inc = 0;
  abs_inc = INTVAL (loop_info->increment);
  if (abs_inc < 0)
    {
      abs_inc = -abs_inc;
      neg_inc = 1;
    }

  if (neg_inc)
    {
      code = swap_condition (loop_info->comparison_code);
      min_value = loop_info->final_equiv_value;
      max_value = loop_info->initial_equiv_value;
    }
  else
    {
      code = loop_info->comparison_code;
      min_value = loop_info->initial_equiv_value;
      max_value = loop_info->final_equiv_value;
    }

  /* Since the loop has a VTOP, we know that the initial test will be
     true and thus the value of max_value should be greater than the
     value of min_value.  Thus the difference should always be positive
     and the code must be LT, LE, LTU, LEU, or NE.  Otherwise the loop is
     not normal, e.g., `for (i = 0; i < 10; i--)'.  */
  switch (code)
    {
    case LTU:
    case LEU:
      {
	unsigned HOST_WIDE_INT umax;
	unsigned HOST_WIDE_INT umin;

	if (GET_CODE (min_value) == CONST_INT)
	  umin = INTVAL (min_value);
	else
	  umin = 0;

	if (GET_CODE (max_value) == CONST_INT)
	  umax = INTVAL (max_value);
	else
	  umax = ((unsigned)2 << (GET_MODE_BITSIZE (mode) - 1)) - 1;

	n_iterations_max = umax - umin;
	break;
      }

    case LT:
    case LE:
      {
	HOST_WIDE_INT smax;
	HOST_WIDE_INT smin;

	if (GET_CODE (min_value) == CONST_INT)
	  smin = INTVAL (min_value);
	else
	  smin = -((unsigned)1 << (GET_MODE_BITSIZE (mode) - 1));

	if (GET_CODE (max_value) == CONST_INT)
	  smax = INTVAL (max_value);
	else
	  smax = ((unsigned)1 << (GET_MODE_BITSIZE (mode) - 1)) - 1;

	n_iterations_max = smax - smin;
	break;
      }

    case NE:
      if (GET_CODE (min_value) == CONST_INT
	  && GET_CODE (max_value) == CONST_INT)
	n_iterations_max = INTVAL (max_value) - INTVAL (min_value);
      else
	/* We need to conservatively assume that we might have the maximum
	   number of iterations without any additional knowledge.  */
	n_iterations_max = ((unsigned)2 << (GET_MODE_BITSIZE (mode) - 1)) - 1;
      break;

    default:
      return 0;
    }

  n_iterations_max /= abs_inc;

  /* If we know that the iteration count is non-negative then adjust
     n_iterations_max if it is so large that it appears negative.  */
  if (nonneg
      && n_iterations_max > ((unsigned)1 << (GET_MODE_BITSIZE (mode) - 1)))
    n_iterations_max = ((unsigned)1 << (GET_MODE_BITSIZE (mode) - 1)) - 1;

  return n_iterations_max;
}


/* Return non-zero if the loop specified by LOOP is suitable for
   the use of special low-overhead looping instructions.  */
static int
doloop_valid_p (loop, jump_insn)
     const struct loop *loop;
     rtx jump_insn;
{
  const struct loop_info *loop_info = LOOP_INFO (loop);

  /* The loop must have a conditional jump at the end.  */
  if (! any_condjump_p (jump_insn)
      || ! onlyjump_p (jump_insn))
    {
      if (loop_dump_stream)
  	fprintf (loop_dump_stream,
		 "Doloop: Invalid jump at loop end.\n");
      return 0;
    }

  /* Give up if a loop has been completely unrolled.  */
  if (loop_info->n_iterations == loop_info->unroll_number)
    {
      if (loop_dump_stream)
	fprintf (loop_dump_stream,
		 "Doloop: Loop completely unrolled.\n");
      return 0;
    }

  /* The loop must have a single exit target.  A break or return
     statement within a loop will generate multiple loop exits.
     Another example of a loop that currently generates multiple exit
     targets is for (i = 0; i < (foo ? 8 : 4); i++) { }.  */
  if (loop_info->has_multiple_exit_targets || loop->exit_count)
    {
      if (loop_dump_stream)
	fprintf (loop_dump_stream,
		 "Doloop: Loop has multiple exit targets.\n");
      return 0;
    }

  /* An indirect jump may jump out of the loop.  */
  if (loop_info->has_indirect_jump)
    {
      if (loop_dump_stream)
	fprintf (loop_dump_stream,
		 "Doloop: Indirect jump in function.\n");
      return 0;
    }

  /* A called function may clobber any special registers required for
     low-overhead looping.  */
  if (loop_info->has_call)
    {
      if (loop_dump_stream)
	fprintf (loop_dump_stream,
		 "Doloop: Function call in loop.\n");
      return 0;
    }

 /* Some targets (eg, PPC) use the count register for branch on table
    instructions.  ??? This should be a target specific check.  */
  if (loop_info->has_tablejump)
    {
      if (loop_dump_stream)
	fprintf (loop_dump_stream,
		 "Doloop: Computed branch in the loop.\n");
      return 0;
    }

  if (! loop_info->increment)
    {
      if (loop_dump_stream)
	fprintf (loop_dump_stream,
		 "Doloop: Could not determine iteration info.\n");
      return 0;
    }

  if (GET_CODE (loop_info->increment) != CONST_INT)
    {
      if (loop_dump_stream)
	fprintf (loop_dump_stream,
		 "Doloop: Increment not an integer constant.\n");
      return 0;
    }

  /* There is no guarantee that a NE loop will terminate if the
     absolute increment is not unity.  ??? We could compute this
     condition at run-time and have a additional jump around the loop
     to ensure an infinite loop.  */
  if (loop_info->comparison_code == NE
      && INTVAL (loop_info->increment) != -1
      && INTVAL (loop_info->increment) != 1)
    {
      if (loop_dump_stream)
	fprintf (loop_dump_stream,
		 "Doloop: NE loop with non-unity increment.\n");
      return 0;
    }

  /* Check for loops that may not terminate under special conditions.  */
  if (! loop_info->n_iterations
      && ((loop_info->comparison_code == LEU
	   && INTVAL (loop_info->increment) > 0)
	  || (loop_info->comparison_code == GEU
	      && INTVAL (loop_info->increment) < 0)))
    {
      /* If the comparison is LEU and the comparison value is UINT_MAX
	 then the loop will not terminate.  Similarly, if the
	 comparison code is GEU and the initial value is 0, the loop
	 will not terminate.

	 Note that with LE and GE, the loop behaviour can be
	 implementation dependent if an overflow occurs, say between
	 INT_MAX and INT_MAX + 1.  We thus don't have to worry about
	 these two cases.

	 ??? We could compute these conditions at run-time and have a
	 additional jump around the loop to ensure an infinite loop.
	 However, it is very unlikely that this is the intended
	 behaviour of the loop and checking for these rare boundary
	 conditions would pessimize all other code.  */
      if (loop_dump_stream)
	fprintf (loop_dump_stream,
		 "Doloop: Possible infinite iteration case ignored.\n");
    }

  return 1;
}


/* Modify the loop to use the low-overhead looping insn where LOOP
   describes the loop, ITERATIONS is an RTX containing the desired
   number of loop iterations, ITERATIONS_MAX is a CONST_INT specifying
   the maximum number of loop iterations, and DOLOOP_INSN is the
   low-overhead looping insn to emit at the end of the loop.  This
   returns non-zero if it was successful.  */
static int
doloop_modify (loop, iterations, iterations_max,
	       doloop_seq, start_label, condition)
     const struct loop *loop;
     rtx iterations;
     rtx iterations_max;
     rtx doloop_seq;
     rtx start_label;
     rtx condition;
{
  rtx counter_reg;
  rtx count;
  rtx sequence;
  rtx jump_insn;
  int nonneg = 0;
  int decrement_count;

  jump_insn = prev_nonnote_insn (loop->end);

  if (loop_dump_stream)
    {
      fprintf (loop_dump_stream, "Doloop: Inserting doloop pattern (");
      if (GET_CODE (iterations) == CONST_INT)
	fprintf (loop_dump_stream, HOST_WIDE_INT_PRINT_DEC,
		 INTVAL (iterations));
      else
	fputs ("runtime", loop_dump_stream);
      fputs (" iterations).", loop_dump_stream);
    }

  /* Emit the label that will delimit the top of the loop.
     This has to be done before the delete_insn call below, to prevent
     delete_insn from deleting too much.  */
  emit_label_after (start_label, loop->top ? loop->top : loop->start);
  LABEL_NUSES (start_label)++;

  /* Discard original jump to continue loop.  The original compare
     result may still be live, so it cannot be discarded explicitly.  */
  delete_insn (jump_insn);

  counter_reg = XEXP (condition, 0);
  if (GET_CODE (counter_reg) == PLUS)
    counter_reg = XEXP (counter_reg, 0);

  start_sequence ();

  count = iterations;
  decrement_count = 0;
  switch (GET_CODE (condition))
    {
    case NE:
      /* Currently only NE tests against zero and one are supported.  */
      if (XEXP (condition, 1) == const0_rtx)
	decrement_count = 1;
      else if (XEXP (condition, 1) != const1_rtx)
	abort ();
      break;

    case GE:
      /* Currently only GE tests against zero are supported.  */
      if (XEXP (condition, 1) != const0_rtx)
	abort ();

      /* The iteration count needs decrementing for a GE test.  */
      decrement_count = 1;

      /* Determine if the iteration counter will be non-negative.
	 Note that the maximum value loaded is iterations_max - 1.  */
      if ((unsigned HOST_WIDE_INT) INTVAL (iterations_max)
	  <= ((unsigned)1 << (GET_MODE_BITSIZE (GET_MODE (counter_reg)) - 1)))
	nonneg = 1;
      break;

      /* Abort if an invalid doloop pattern has been generated.  */
    default:
      abort();
    }

  if (decrement_count)
    {
      if (GET_CODE (count) == CONST_INT)
	count = GEN_INT (INTVAL (count) - 1);
      else
	count = expand_binop (GET_MODE (counter_reg), sub_optab,
			      count, GEN_INT (1),
			      0, 0, OPTAB_LIB_WIDEN);
    }

  /* Insert initialization of the count register into the loop header.  */
  convert_move (counter_reg, count, 1);
  sequence = gen_sequence ();
  end_sequence ();
  emit_insn_before (sequence, loop->start);

  /* Some targets (eg, C4x) need to initialize special looping
     registers.  */
#ifdef HAVE_doloop_begin
  {
    rtx init;

    init = gen_doloop_begin (counter_reg,
			     GET_CODE (iterations) == CONST_INT
			     ? iterations : const0_rtx, iterations_max,
			     GEN_INT (loop->level));
    if (init)
      {
	start_sequence ();
	emit_insn (init);
	sequence = gen_sequence ();
	end_sequence ();
	emit_insn_after (sequence, loop->start);
      }
  }
#endif

  /* Insert the new low-overhead looping insn.  */
  emit_jump_insn_before (doloop_seq, loop->end);
  jump_insn = prev_nonnote_insn (loop->end);
  JUMP_LABEL (jump_insn) = start_label;

  /* Add a REG_NONNEG note if the actual or estimated maximum number
     of iterations is non-negative.  */
  if (nonneg)
    {
      REG_NOTES (jump_insn)
	= gen_rtx_EXPR_LIST (REG_NONNEG, NULL_RTX, REG_NOTES (jump_insn));
    }
  return 1;
}


/* Handle the more complex case, where the bounds are not known at
   compile time.  In this case we generate a run_time calculation of
   the number of iterations.  We rely on the existence of a run-time
   guard to ensure that the loop executes at least once, i.e.,
   initial_value obeys the loop comparison condition.  If a guard is
   not present, we emit one.  The loop to modify is described by LOOP.
   ITERATIONS_MAX is a CONST_INT specifying the estimated maximum
   number of loop iterations.  DOLOOP_INSN is the low-overhead looping
   insn to insert.  Returns non-zero if loop successfully modified.  */
static int
doloop_modify_runtime (loop, iterations_max,
		       doloop_seq, start_label, mode, condition)
     const struct loop *loop;
     rtx iterations_max;
     rtx doloop_seq;
     rtx start_label;
     enum machine_mode mode;
     rtx condition;
{
  const struct loop_info *loop_info = LOOP_INFO (loop);
  HOST_WIDE_INT abs_inc;
  int neg_inc;
  rtx diff;
  rtx sequence;
  rtx iterations;
  rtx initial_value;
  rtx final_value;
  rtx increment;
  int unsigned_p;
  enum rtx_code comparison_code;

  increment = loop_info->increment;
  initial_value = loop_info->initial_value;
  final_value = loop_info->final_value;

  neg_inc = 0;
  abs_inc = INTVAL (increment);
  if (abs_inc < 0)
    {
      abs_inc = -abs_inc;
      neg_inc = 1;
    }

  comparison_code = loop_info->comparison_code;
  unsigned_p = (comparison_code == LTU
		|| comparison_code == LEU
		|| comparison_code == GTU
		|| comparison_code == GEU
		|| comparison_code == NE);

  /* The number of iterations (prior to any loop unrolling) is given by:
     (abs (final - initial) + abs_inc - 1) / abs_inc.

     However, it is possible for the summation to overflow, and a
     safer method is:

     abs (final - initial) / abs_inc + (abs (final - initial) % abs_inc) != 0

     If the loop has been unrolled, then the loop body has been
     preconditioned to iterate a multiple of unroll_number times.
     The number of iterations of the loop body is simply:
     abs (final - initial) / (abs_inc * unroll_number).

     The division and modulo operations can be avoided by requiring
     that the increment is a power of 2 (precondition_loop_p enforces
     this requirement).  Nevertheless, the RTX_COSTS should be checked
     to see if a fast divmod is available.  */

  start_sequence ();
  /* abs (final - initial)  */
  diff = expand_binop (mode, sub_optab,
		       copy_rtx (neg_inc ? initial_value : final_value),
		       copy_rtx (neg_inc ? final_value : initial_value),
		       NULL_RTX, unsigned_p, OPTAB_LIB_WIDEN);

  if (loop_info->unroll_number == 1)
    {
      if (abs_inc != 1)
	{
	  int shift_count;
	  rtx extra;
	  rtx label;

	  shift_count = exact_log2 (abs_inc);
	  if (shift_count < 0)
	    abort ();

	  /* abs (final - initial) / abs_inc  */
	  iterations = expand_binop (GET_MODE (diff), lshr_optab,
				     diff, GEN_INT (shift_count),
				     NULL_RTX, 1,
				     OPTAB_LIB_WIDEN);

	  /* abs (final - initial) % abs_inc  */
	  extra = expand_binop (GET_MODE (iterations), and_optab,
				diff, GEN_INT (abs_inc - 1),
				NULL_RTX, 1,
				OPTAB_LIB_WIDEN);

	  /* If (abs (final - initial) % abs_inc == 0) jump past
	     following increment instruction.  */
	  label = gen_label_rtx();
	  emit_cmp_and_jump_insns (extra, const0_rtx, EQ, NULL_RTX,
				   GET_MODE (extra), 0, 0, label);
	  JUMP_LABEL (get_last_insn ()) = label;
	  LABEL_NUSES (label)++;

	  /* Increment the iteration count by one.  */
	  iterations = expand_binop (GET_MODE (iterations), add_optab,
				     iterations, GEN_INT (1),
				     iterations, 1,
				     OPTAB_LIB_WIDEN);

	  emit_label (label);
	}
      else
	iterations = diff;
    }
  else
    {
      int shift_count;

      /* precondition_loop_p has preconditioned the loop so that the
	 iteration count of the loop body is always a power of 2.
	 Since we won't get an overflow calculating the loop count,
	 the code we emit is simpler.  */
      shift_count = exact_log2 (loop_info->unroll_number * abs_inc);
      if (shift_count < 0)
	abort ();

      iterations = expand_binop (GET_MODE (diff), lshr_optab,
				 diff, GEN_INT (shift_count),
				 NULL_RTX, 1,
				 OPTAB_LIB_WIDEN);
    }


  /* If there is a NOTE_INSN_LOOP_VTOP, we have a `for' or `while'
     style loop, with a loop exit test at the start.  Thus, we can
     assume that the loop condition was true when the loop was
     entered.

     `do-while' loops require special treatment since the exit test is
     not executed before the start of the loop.  We need to determine
     if the loop will terminate after the first pass and to limit the
     iteration count to one if necessary.  */
  if (! loop->vtop)
    {
      rtx label;

      if (loop_dump_stream)
	fprintf (loop_dump_stream, "Doloop: Do-while loop.\n");

      /* A `do-while' loop must iterate at least once.  If the
	 iteration count is bogus, we set the iteration count to 1.
	 Note that if the loop has been unrolled, then the loop body
	 is guaranteed to execute at least once.  */
      if (loop_info->unroll_number == 1)
	{
	  /*  Emit insns to test if the loop will immediately
	      terminate and to set the iteration count to 1 if true.  */
	  label = gen_label_rtx();
	  emit_cmp_and_jump_insns (copy_rtx (initial_value),
				   copy_rtx (loop_info->comparison_value),
				   comparison_code, NULL_RTX, mode, 0, 0,
				   label);
	  JUMP_LABEL (get_last_insn ()) = label;
	  LABEL_NUSES (label)++;
	  emit_move_insn (iterations, const1_rtx);
	  emit_label (label);
	}
    }

  sequence = gen_sequence ();
  end_sequence ();
  emit_insn_before (sequence, loop->start);

  return doloop_modify (loop, iterations, iterations_max, doloop_seq,
			start_label, condition);
}


/* This is the main entry point.  Process loop described by LOOP
   validating that the loop is suitable for conversion to use a low
   overhead looping instruction, replacing the jump insn where
   suitable.  We distinguish between loops with compile-time bounds
   and those with run-time bounds.  Information from LOOP is used to
   compute the number of iterations and to determine whether the loop
   is a candidate for this optimization.  Returns non-zero if loop
   successfully modified.  */
int
doloop_optimize (loop)
     const struct loop *loop;
{
  struct loop_info *loop_info = LOOP_INFO (loop);
  rtx initial_value;
  rtx final_value;
  rtx increment;
  rtx jump_insn;
  enum machine_mode mode;
  unsigned HOST_WIDE_INT n_iterations;
  unsigned HOST_WIDE_INT n_iterations_max;
  rtx doloop_seq, doloop_pat, doloop_reg;
  rtx iterations;
  rtx iterations_max;
  rtx start_label;
  rtx condition;

  if (loop_dump_stream)
    fprintf (loop_dump_stream,
	     "Doloop: Processing loop %d, enclosed levels %d.\n",
	     loop->num, loop->level);

  jump_insn = prev_nonnote_insn (loop->end);

  /* Check that loop is a candidate for a low-overhead looping insn.  */
  if (! doloop_valid_p (loop, jump_insn))
    return 0;

  /* Determine if the loop can be safely, and profitably,
     preconditioned.  While we don't precondition the loop in a loop
     unrolling sense, this test ensures that the loop is well behaved
     and that the increment is a constant integer.  */
  if (! precondition_loop_p (loop, &initial_value, &final_value,
			     &increment, &mode))
    {
      if (loop_dump_stream)
      	fprintf (loop_dump_stream,
		 "Doloop: Cannot precondition loop.\n");
      return 0;
    }

  /* Determine or estimate the maximum number of loop iterations.  */
  n_iterations = loop_info->n_iterations;
  if (n_iterations)
    {
      /* This is the simple case where the initial and final loop
	 values are constants.  */
      n_iterations_max = n_iterations;
    }
  else
    {
      int nonneg = find_reg_note (jump_insn, REG_NONNEG, 0) != 0;

      /* This is the harder case where the initial and final loop
	 values may not be constants.  */
      n_iterations_max = doloop_iterations_max (loop_info, mode, nonneg);

      if (! n_iterations_max)
	{
	  /* We have something like `for (i = 0; i < 10; i--)'.  */
	  if (loop_dump_stream)
	    fprintf (loop_dump_stream,
		     "Doloop: Not normal loop.\n");
	  return 0;
	}
    }

  /* Account for loop unrolling in the iteration count.  This will
     have no effect if loop_iterations could not determine the number
     of iterations.  */
  n_iterations /= loop_info->unroll_number;
  n_iterations_max /= loop_info->unroll_number;

  if (n_iterations && n_iterations < 3)
    {
      if (loop_dump_stream)
	fprintf (loop_dump_stream,
		 "Doloop: Too few iterations (%ld) to be profitable.\n",
		 (long int) n_iterations);
      return 0;
    }

  iterations = GEN_INT (n_iterations);
  iterations_max = GEN_INT (n_iterations_max);

  /* Generate looping insn.  If the pattern FAILs then give up trying
     to modify the loop since there is some aspect the back-end does
     not like.  */
  start_label = gen_label_rtx ();
  doloop_reg = gen_reg_rtx (mode);
  doloop_seq = gen_doloop_end (doloop_reg, iterations, iterations_max,
			       GEN_INT (loop->level), start_label);
  if (! doloop_seq && mode != word_mode)
    {
      PUT_MODE (doloop_reg, word_mode);
      doloop_seq = gen_doloop_end (doloop_reg, iterations, iterations_max,
				   GEN_INT (loop->level), start_label);
    }
  if (! doloop_seq)
    {
      if (loop_dump_stream)
	fprintf (loop_dump_stream,
		 "Doloop: Target unwilling to use doloop pattern!\n");
      return 0;
    }

  /* A raw define_insn may yield a plain pattern.  If a sequence
     was involved, the last must be the jump instruction.  */
  if (GET_CODE (doloop_seq) == SEQUENCE)
    {
      doloop_pat = XVECEXP (doloop_seq, 0, XVECLEN (doloop_seq, 0) - 1);
      if (GET_CODE (doloop_pat) == JUMP_INSN)
	doloop_pat = PATTERN (doloop_pat);
      else
	doloop_pat = NULL_RTX;
    }
  else
    doloop_pat = doloop_seq;

  if (! doloop_pat
      || ! (condition = doloop_condition_get (doloop_pat)))
    {
      if (loop_dump_stream)
	fprintf (loop_dump_stream,
		 "Doloop: Unrecognizable doloop pattern!\n");
      return 0;
    }

  if (n_iterations != 0)
    /* Handle the simpler case, where we know the iteration count at
       compile time.  */
    return doloop_modify (loop, iterations, iterations_max, doloop_seq,
			  start_label, condition);
  else
    /* Handle the harder case, where we must add additional runtime tests.  */
    return doloop_modify_runtime (loop, iterations_max, doloop_seq,
				  start_label, mode, condition);
}

#endif /* HAVE_doloop_end */
