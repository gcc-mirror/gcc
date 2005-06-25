/* Calculate branch probabilities, and basic block execution counts.
   Copyright (C) 1990, 1991, 1992, 1993, 1994, 1996, 1997, 1998, 1999,
   2000, 2001, 2002, 2003, 2005  Free Software Foundation, Inc.
   Contributed by James E. Wilson, UC Berkeley/Cygnus Support;
   based on some ideas from Dain Samples of UC Berkeley.
   Further mangling by Bob Manson, Cygnus Support.

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
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

/* Generate basic block profile instrumentation and auxiliary files.
   RTL-based version.  See profile.c for overview.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "flags.h"
#include "output.h"
#include "regs.h"
#include "expr.h"
#include "function.h"
#include "toplev.h"
#include "coverage.h"
#include "value-prof.h"
#include "tree.h"
#include "ggc.h"

/* Do initialization work for the edge profiler.  */

static void
rtl_init_edge_profiler (void)
{
  /* gen_edge_profiler calls safe_insert_insn_on_edge which needs
     register liveness data to be available.  */
  life_analysis (NULL, 0);
}

/* Output instructions as RTL to increment the edge execution count.  */

static void
rtl_gen_edge_profiler (int edgeno, edge e)
{
  rtx ref = rtl_coverage_counter_ref (GCOV_COUNTER_ARCS, edgeno);
  rtx tmp;
  enum machine_mode mode = GET_MODE (ref);
  rtx sequence;

  start_sequence ();
  ref = validize_mem (ref);

  tmp = expand_simple_binop (mode, PLUS, ref, const1_rtx,
			     ref, 0, OPTAB_WIDEN);

  if (tmp != ref)
    emit_move_insn (copy_rtx (ref), tmp);

  sequence = get_insns ();
  end_sequence ();
  safe_insert_insn_on_edge (sequence, e);
  rebuild_jump_labels (e->insns.r);
}

/* Output instructions as RTL to increment the interval histogram counter.
   VALUE is the expression whose value is profiled.  TAG is the tag of the
   section for counters, BASE is offset of the counter position.  */

static void
rtl_gen_interval_profiler (histogram_value value, unsigned tag, unsigned base)
{
  enum machine_mode mode = mode_for_size (GCOV_TYPE_SIZE, MODE_INT, 0);
  rtx mem_ref, tmp, tmp1, mr, val;
  rtx sequence;
  rtx more_label = gen_label_rtx ();
  rtx less_label = gen_label_rtx ();
  rtx end_of_code_label = gen_label_rtx ();
  int per_counter = GCOV_TYPE_SIZE / BITS_PER_UNIT;
  edge e = split_block (BLOCK_FOR_INSN (value->hvalue.rtl.insn),
		   PREV_INSN (value->hvalue.rtl.insn));

  start_sequence ();

  if (value->hvalue.rtl.seq)
    emit_insn (value->hvalue.rtl.seq);

  mr = gen_reg_rtx (Pmode);

  tmp = rtl_coverage_counter_ref (tag, base);
  tmp = force_reg (Pmode, XEXP (tmp, 0));

  val = expand_simple_binop (value->hvalue.rtl.mode, MINUS,
			     copy_rtx (value->hvalue.rtl.value),
			     GEN_INT (value->hdata.intvl.int_start),
			     NULL_RTX, 0, OPTAB_WIDEN);

    do_compare_rtx_and_jump (copy_rtx (val), GEN_INT (value->hdata.intvl.steps),
			   GE, 0, value->hvalue.rtl.mode, NULL_RTX, NULL_RTX, 
			   more_label);
  do_compare_rtx_and_jump (copy_rtx (val), const0_rtx, LT, 0, 
			   value->hvalue.rtl.mode,
			     NULL_RTX, NULL_RTX, less_label);

  /* We are in range.  */
  tmp1 = expand_simple_binop (value->hvalue.rtl.mode, MULT,
			      copy_rtx (val), GEN_INT (per_counter),
			      NULL_RTX, 0, OPTAB_WIDEN);
  tmp1 = expand_simple_binop (Pmode, PLUS, copy_rtx (tmp), tmp1, mr,
			      0, OPTAB_WIDEN);
  if (tmp1 != mr)
    emit_move_insn (copy_rtx (mr), tmp1);

      emit_jump_insn (gen_jump (end_of_code_label));
      emit_barrier ();

  /* Above the interval.  */
      emit_label (more_label);
      tmp1 = expand_simple_binop (Pmode, PLUS, copy_rtx (tmp),
				  GEN_INT (per_counter * value->hdata.intvl.steps),
				  mr, 0, OPTAB_WIDEN);
      if (tmp1 != mr)
	emit_move_insn (copy_rtx (mr), tmp1);
	  emit_jump_insn (gen_jump (end_of_code_label));
	  emit_barrier ();

  /* Below the interval.  */
      emit_label (less_label);
      tmp1 = expand_simple_binop (Pmode, PLUS, copy_rtx (tmp),
			GEN_INT (per_counter * (value->hdata.intvl.steps +1)),
		mr, 0, OPTAB_WIDEN);
      if (tmp1 != mr)
	emit_move_insn (copy_rtx (mr), tmp1);

    emit_label (end_of_code_label);

  mem_ref = validize_mem (gen_rtx_MEM (mode, mr));

  tmp = expand_simple_binop (mode, PLUS, copy_rtx (mem_ref), const1_rtx,
			     mem_ref, 0, OPTAB_WIDEN);

  if (tmp != mem_ref)
    emit_move_insn (copy_rtx (mem_ref), tmp);

  sequence = get_insns ();
  end_sequence ();
  rebuild_jump_labels (sequence);
  safe_insert_insn_on_edge (sequence, e);
}

/* Output instructions as RTL to increment the power of two histogram counter.
   VALUE is the expression whose value is profiled.  TAG is the tag of the
   section for counters, BASE is offset of the counter position.  */

static void
rtl_gen_pow2_profiler (histogram_value value, unsigned tag, unsigned base)
{
  enum machine_mode mode = mode_for_size (GCOV_TYPE_SIZE, MODE_INT, 0);
  rtx mem_ref, tmp, mr, uval;
  rtx sequence;
  rtx end_of_code_label = gen_label_rtx ();
  int per_counter = GCOV_TYPE_SIZE / BITS_PER_UNIT;
  edge e = split_block (BLOCK_FOR_INSN (value->hvalue.rtl.insn),
			PREV_INSN (value->hvalue.rtl.insn));

  start_sequence ();

  if (value->hvalue.rtl.seq)
    emit_insn (value->hvalue.rtl.seq);

  mr = gen_reg_rtx (Pmode);
  tmp = rtl_coverage_counter_ref (tag, base);
  tmp = force_reg (Pmode, XEXP (tmp, 0));
  emit_move_insn (mr, tmp);

  uval = gen_reg_rtx (value->hvalue.rtl.mode);
  emit_move_insn (uval, copy_rtx (value->hvalue.rtl.value));

  /* Check for non-power of 2.  */
  do_compare_rtx_and_jump (copy_rtx (uval), const0_rtx, LE, 0, value->hvalue.rtl.mode,
			   NULL_RTX, NULL_RTX, end_of_code_label);
  tmp = expand_simple_binop (value->hvalue.rtl.mode, PLUS, copy_rtx (uval),
			     constm1_rtx, NULL_RTX, 0, OPTAB_WIDEN);
  tmp = expand_simple_binop (value->hvalue.rtl.mode, AND, copy_rtx (uval), tmp,
			     NULL_RTX, 0, OPTAB_WIDEN);
  do_compare_rtx_and_jump (tmp, const0_rtx, NE, 0, value->hvalue.rtl.mode, NULL_RTX,
			   NULL_RTX, end_of_code_label);

  tmp = expand_simple_binop (Pmode, PLUS, copy_rtx (mr), GEN_INT (per_counter),
			     mr, 0, OPTAB_WIDEN);
  if (tmp != mr)
    emit_move_insn (copy_rtx (mr), tmp);

  /* Increase the counter.  */
  emit_label (end_of_code_label);

  mem_ref = validize_mem (gen_rtx_MEM (mode, mr));

  tmp = expand_simple_binop (mode, PLUS, copy_rtx (mem_ref), const1_rtx,
			     mem_ref, 0, OPTAB_WIDEN);

  if (tmp != mem_ref)
    emit_move_insn (copy_rtx (mem_ref), tmp);

  sequence = get_insns ();
  end_sequence ();
  rebuild_jump_labels (sequence);
  safe_insert_insn_on_edge (sequence, e);
}

/* Output instructions as RTL for code to find the most common value.
   VALUE is the expression whose value is profiled.  TAG is the tag of the
   section for counters, BASE is offset of the counter position.  */

static rtx
rtl_gen_one_value_profiler_no_edge_manipulation (histogram_value value,
						 unsigned tag, unsigned base)
{
  enum machine_mode mode = mode_for_size (GCOV_TYPE_SIZE, MODE_INT, 0);
  rtx stored_value_ref, counter_ref, all_ref, stored_value, counter, all;
  rtx tmp, uval;
  rtx sequence;
  rtx same_label = gen_label_rtx ();
  rtx zero_label = gen_label_rtx ();
  rtx end_of_code_label = gen_label_rtx ();

  start_sequence ();

  if (value->hvalue.rtl.seq)
    emit_insn (value->hvalue.rtl.seq);

  stored_value_ref = rtl_coverage_counter_ref (tag, base);
  counter_ref = rtl_coverage_counter_ref (tag, base + 1);
  all_ref = rtl_coverage_counter_ref (tag, base + 2);
  stored_value = validize_mem (stored_value_ref);
  counter = validize_mem (counter_ref);
  all = validize_mem (all_ref);

  uval = gen_reg_rtx (mode);
  convert_move (uval, copy_rtx (value->hvalue.rtl.value), 0);

  /* Check if the stored value matches.  */
  do_compare_rtx_and_jump (copy_rtx (uval), copy_rtx (stored_value), EQ,
			   0, mode, NULL_RTX, NULL_RTX, same_label);

  /* Does not match; check whether the counter is zero.  */
  do_compare_rtx_and_jump (copy_rtx (counter), const0_rtx, EQ, 0, mode,
			   NULL_RTX, NULL_RTX, zero_label);

  /* The counter is not zero yet.  */
  tmp = expand_simple_binop (mode, PLUS, copy_rtx (counter), constm1_rtx,
			     counter, 0, OPTAB_WIDEN);

  if (tmp != counter)
    emit_move_insn (copy_rtx (counter), tmp);

  emit_jump_insn (gen_jump (end_of_code_label));
  emit_barrier ();

  emit_label (zero_label);
  /* Set new value.  */
  emit_move_insn (copy_rtx (stored_value), copy_rtx (uval));

  emit_label (same_label);
  /* Increase the counter.  */
  tmp = expand_simple_binop (mode, PLUS, copy_rtx (counter), const1_rtx,
			     counter, 0, OPTAB_WIDEN);

  if (tmp != counter)
    emit_move_insn (copy_rtx (counter), tmp);

  emit_label (end_of_code_label);

  /* Increase the counter of all executions; this seems redundant given
     that ve have counts for edges in cfg, but it may happen that some
     optimization will change the counts for the block (either because
     it is unable to update them correctly, or because it will duplicate
     the block or its part).  */
  tmp = expand_simple_binop (mode, PLUS, copy_rtx (all), const1_rtx,
			     all, 0, OPTAB_WIDEN);

  if (tmp != all)
    emit_move_insn (copy_rtx (all), tmp);
  sequence = get_insns ();
  end_sequence ();
  return sequence;
}

/* Output instructions as RTL for code to find the most common value.
   VALUE is the expression whose value is profiled.  TAG is the tag of the
   section for counters, BASE is offset of the counter position.  */

static void
rtl_gen_one_value_profiler (histogram_value value, unsigned tag, unsigned base)
{
  edge e = split_block (BLOCK_FOR_INSN (value->hvalue.rtl.insn),
		   PREV_INSN (value->hvalue.rtl.insn));
  rtx sequence = rtl_gen_one_value_profiler_no_edge_manipulation (value, 
			tag, base);
  rebuild_jump_labels (sequence);
  safe_insert_insn_on_edge (sequence, e);
}

/* Output instructions as RTL for code to find the most common value of
   a difference between two evaluations of an expression.
   VALUE is the expression whose value is profiled.  TAG is the tag of the
   section for counters, BASE is offset of the counter position.  */

static void
rtl_gen_const_delta_profiler (histogram_value value, unsigned tag, unsigned base)
{
  histogram_value one_value_delta;
  enum machine_mode mode = mode_for_size (GCOV_TYPE_SIZE, MODE_INT, 0);
  rtx stored_value_ref, stored_value, tmp, uval;
  rtx sequence;
  edge e = split_block (BLOCK_FOR_INSN (value->hvalue.rtl.insn),
		   PREV_INSN (value->hvalue.rtl.insn));

  start_sequence ();

  if (value->hvalue.rtl.seq)
    emit_insn (value->hvalue.rtl.seq);

  stored_value_ref = rtl_coverage_counter_ref (tag, base);
  stored_value = validize_mem (stored_value_ref);

  uval = gen_reg_rtx (mode);
  convert_move (uval, copy_rtx (value->hvalue.rtl.value), 0);
  tmp = expand_simple_binop (mode, MINUS,
			     copy_rtx (uval), copy_rtx (stored_value),
			     NULL_RTX, 0, OPTAB_WIDEN);

  one_value_delta = ggc_alloc (sizeof (*one_value_delta));
  one_value_delta->hvalue.rtl.value = tmp;
  one_value_delta->hvalue.rtl.mode = mode;
  one_value_delta->hvalue.rtl.seq = NULL_RTX;
  one_value_delta->hvalue.rtl.insn = value->hvalue.rtl.insn;
  one_value_delta->type = HIST_TYPE_SINGLE_VALUE;
  emit_insn (rtl_gen_one_value_profiler_no_edge_manipulation (one_value_delta,
							      tag, base + 1));
  emit_move_insn (copy_rtx (stored_value), uval);
  sequence = get_insns ();
  end_sequence ();
  rebuild_jump_labels (sequence);
  safe_insert_insn_on_edge (sequence, e);
}

/* Return the file on which profile dump output goes, if any.  */

static FILE *rtl_profile_dump_file (void) {
  return dump_file;
}

struct profile_hooks rtl_profile_hooks =
{
  rtl_init_edge_profiler,
  rtl_gen_edge_profiler,
  rtl_gen_interval_profiler,
  rtl_gen_pow2_profiler,
  rtl_gen_one_value_profiler,
  rtl_gen_const_delta_profiler,
  rtl_profile_dump_file
};
