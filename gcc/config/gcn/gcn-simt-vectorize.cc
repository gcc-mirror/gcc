/* AMD GCN "simt-vect" pass implementation.

   Copyright (C) 2026 Free Software Foundation, Inc.
   Contributed by BayLibre.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* This pass converts a regular function that takes scalar parameters, and
   returns a scalar value, to a function that takes vector parameters and
   returns a vector value.  Every instruction is parallelized such that the
   function now does the same thing 64 times in parallel.

   This is just the instruction-level conversion; the CFG conversion happens in
   gcn-simt-cbranch.cc.

   If a function has SIMT_ENTER and SIMT_EXIT insns then there's no need to do
   any conversion outside those regions because we know those will never be
   executed in parallel.  Otherwise, we must assume the function can be called
   in a scalar context (EXEC == 1), or in a SIMT context (EXEC > 1), and must
   handle both.

   These scalar -> SIMT and SIMT -> scalar transitions must be handled
   carefully.  Values must be "broadcast" to all threads (easy), or reduced to
   one value (not so easy).

   If a value is known to always hold the same value in all threads (i.e. it
   originates from an immediate constant, or from a global variable) then we
   try to keep it that way.  However, if a value is ever written to in a
   conditional context (i.e. the value is only always the same in the threads
   that are enabled at that moment) then we need to use vector values even for
   that.

   Therefore, this algorithm makes multiple passes over the instructions to
   track what depends on what and determine the constraints for each pseudo.

   Once the registers have been classified, the instructions are "vectorized"
   almost one-for-one by converting scalar modes to the corresponding vector
   mode (e.g. DImode -> V64DImode).  There are a number of special cases to
   handle -- especially clobbers and immediate constants.

   We use the "unmasked" variants of the vector instructions (i.e. the variant
   without the "_exec" suffix).  In SIMT mode, all instructions will be
   masked, of course, so this doesn't really add much information, and the
   other compiler passes seem to struggle with all the vec_merge patterns and
   the additional register dependencies.  It doesn't hurt that this choice is
   easier to implement here.
*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "tree-pass.h"
#include "memmodel.h"
#include "rtl.h"
#include "print-rtl.h"
#include "rtl-iter.h"
#include "emit-rtl.h"
#include "regset.h"
#include "insn-config.h"
#include "recog.h"
#include "insn-attr.h"
#include "insn-opinit.h"
#include "df.h"
#include "pretty-print.h"
#include "errors.h"

const pass_data pass_data_gcn_simt_vectorize =
{
  RTL_PASS, // type
  "simt_vect", // name
  OPTGROUP_NONE, // optinfo_flags
  TV_SIMT, // tv_id
  0, // properties_required
  0, // properties_provided
  0, // properties_destroyed
  0, // todo_flags_start
  TODO_df_verify | TODO_df_finish, // todo_flags_finish
};

class pass_gcn_simt_vectorize : public rtl_opt_pass
{
public:
  pass_gcn_simt_vectorize (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_gcn_simt_vectorize, ctxt)
  {}

  // opt_pass methods:
  virtual bool gate (function *);
  virtual unsigned int execute (function *);
};

rtl_opt_pass *
make_pass_gcn_simt_vectorize (gcc::context *ctxt)
{
  return new pass_gcn_simt_vectorize (ctxt);
}

bool
pass_gcn_simt_vectorize::gate (function *)
{
  /* This pass is required for correctness when -msimt is enabled.  */
  return flag_simt_mode;
}


/* Write a register name or number to BUF, for dumps and debug.  */

static char *
regno_to_string (unsigned int regno, char buf[10])
{
  if (regno >= FIRST_PSEUDO_REGISTER)
    snprintf (buf, 10, "r%d", regno);
  else
    snprintf (buf, 10, "%s", reg_names[regno]);
  return buf;
}

/* Emit a slim dump of X (an insn) to the file F, without the notes
   dump_insn_slim includes.  */

static void
dump_insn_no_notes (FILE *f, const rtx_insn *x)
{
  pretty_printer rtl_slim_pp;
  rtl_slim_pp.set_output_stream (f);
  print_insn (&rtl_slim_pp, x, 1);
  pp_flush (&rtl_slim_pp);
  fprintf (f, "\n");
}

/* Emit a slim-style dump of X (an RTX node) to the file F, including the
   mode, but don't recurse into the expression.  */

static void
dump_value_with_mode (FILE *f, const_rtx x)
{
  if (REG_P (x)
      || GET_CODE (x) == SET
      || CONST_INT_P (x)
      || CONST_DOUBLE_P (x)
      || SYMBOL_REF_P (x))
    dump_value_slim (f, x, 1);
  else if (GET_CODE (x) == CLOBBER
	   || GET_CODE (x) == USE)
    {
      dump_value_slim (f, x, 1);
      fprintf (f, " ");
      dump_value_with_mode (f, XEXP (x, 0));
    }
  else
    fprintf (f, "%s:%s", GET_RTX_NAME (GET_CODE (x)),
	     GET_MODE_NAME (GET_MODE (x)));

  if (CONST_VECTOR_P (x))
    {
      fprintf (f, " [");
      dump_value_with_mode (f, XVECEXP (x, 0, 0));
      fprintf (f, " ...]");
    }
}

/* This class represents a SIMT_ENTER/SIMT_EXIT region within a function.
   There may be multiple within a function, but we only support the case where
   the entry and exit are in the same function and properly balanced.

   If the whole function is being converted to SIMT then there will be zero
   instances of this class.

   The values here track the pass state as analysis and conversion progress.
 */

class simt_region
{
public:
  simt_region (basic_block entry, rtx_insn *entry_insn,
	       hash_map<basic_block,rtx_insn*> *exits)
    : entry_block (entry), simt_enter_insn (entry_insn),
      exit_block (NULL), simt_exit_insn (NULL)
  {
    INIT_REG_SET (&live_at_simt_enter);
    INIT_REG_SET (&live_at_simt_exit);
    bitmap_initialize (&simt_bbs, NULL);

    /* Scan the region's blocks.  */
    find_simt_bbs (entry, exits);

    /* We don't support cases where the region entry and exit are in
       different functions.  */
    gcc_assert (exit_block && simt_exit_insn);
  }

  ~simt_region ()
  {
    CLEAR_REG_SET (&live_at_simt_enter);
    CLEAR_REG_SET (&live_at_simt_exit);
    bitmap_clear (&simt_bbs);
  }

  /* The BB and INSN where the SIMT_ENTER is located.
     Populated on construction.  */
  basic_block entry_block;
  rtx_insn *simt_enter_insn;

  /* The BB and INSN where the SIMT_EXIT is located.
     Populated on construction.  */
  basic_block exit_block;
  rtx_insn *simt_exit_insn;

  /* These registers are the ones that will need to be converted to/from
     scalar at the transition points.
     Populated during the analysis passes.  */
  regset_head live_at_simt_enter;
  regset_head live_at_simt_exit;

  /* The set of BBs within the enter/exit region.
     Populated on construction.  */
  bitmap_head simt_bbs;

private:

  /* Find all basic_blocks inside the SIMT SESE region, recursively.
     Called on construction.  */

  void find_simt_bbs (basic_block bb, hash_map<basic_block,rtx_insn*> *exits)
  {
    bitmap_set_bit (&simt_bbs, bb->index);

    rtx_insn **insn = exits->get (bb);
    if (insn)
      {
	gcc_assert (exit_block == NULL);
	exit_block = bb;
	simt_exit_insn = *insn;
	return;
      }

    for (edge e : bb->succs)
      {
	if (!bitmap_bit_p (&simt_bbs, e->dest->index))
	  find_simt_bbs (e->dest, exits);
      }
  }
};

/* Scan the dependency graph to identify what values need to be converted
   to vectors (while leaving the rest as scalars).

   The scan occurs when the object is constructed, and then the transformation
   algorithm can read the findings.

   The goal of the analysis is to classify ("assign") each instruction and
   register as "scalar" or "vector".

   Any value that comes from a vector source (function arguments,
   SIMT_LANE_ID, etc.) should cause all dependent instructions to produce
   vector results.  This then has a knock-on effect through all the
   dependencies of those instructions, and so on, until the end of the
   function.

   This class holds the state recorded during the analysis passes.
   There will be exactly one instance of this class, per function, and it
   only exists for the lifetime of this pass.  It wraps variables that would
   otherwise be global variables, and provides a data pointer to give to
   callbacks.  */

class simt_vect_scan
{
public:
  simt_vect_scan (vec<simt_region*> *simt_regions)
    : simt_regions (simt_regions), rescanning (false), rescan_needed (true)
  {
    INIT_REG_SET (&assigned_vector);
    INIT_REG_SET (&maybe_assigned_vector);
    INIT_REG_SET (&done);
    bitmap_initialize (&unfinished_bbs, NULL);
    bitmap_initialize (&vector_insns, NULL);

    do_function_analysis ();
  }

  ~simt_vect_scan ()
  {
    CLEAR_REG_SET (&assigned_vector);
    CLEAR_REG_SET (&maybe_assigned_vector);
    CLEAR_REG_SET (&done);
    bitmap_clear (&unfinished_bbs);
    bitmap_clear (&vector_insns);
  }

  /* Return true if this register will be duplicated at SIMT_ENTER.  */

  bool scalar_reg_needs_vec_duplicate_p (unsigned regno, basic_block bb)
  {
    for (auto r : simt_regions)
      if (bitmap_bit_p (&r->simt_bbs, bb->index)
	  && REGNO_REG_SET_P (&r->live_at_simt_enter, regno))
	return true;

    return false;
  }

  /* Return true if the instruction was classified as needing vectorization.  */

  bool transform_insn_p (int insn_id)
  {
    return bitmap_bit_p (&vector_insns, insn_id);
  }

  /* Return true if this register was classified as needing vectorization.  */

  bool transform_reg_p (unsigned regno)
  {
    return REGNO_REG_SET_P (&assigned_vector, regno);
  }

private:

  /* Function state.  */
  regset_head assigned_vector;
  regset_head done;
  bitmap_head vector_insns;
  vec<simt_region *> *simt_regions;
  bool rescanning;
  bool rescan_needed;

  /* BB state.  */
  basic_block bb;
  bitmap_head unfinished_bbs;
  regset_head maybe_assigned_vector;
  simt_region *active_simt_region;
  bool entry_found;
  bool exit_found;

  /* Insn state.  */
  bool simt_enabled;
  bool use_vector;
  bool uncertain;

  /* To be called every time scanning starts a new block.
     Updates the state machine.  */

  void reset_bb_context (basic_block bb)
  {
    this->bb = bb;
    bitmap_clear_bit (&unfinished_bbs, bb->index);
    CLEAR_REG_SET (&maybe_assigned_vector);

    active_simt_region = NULL;
    for (auto r : simt_regions)
      {
	if (optimize)
	  {
	    if (r->entry_block == bb)
	      COPY_REG_SET (&r->live_at_simt_enter, DF_LIVE_IN (bb));
	    if (r->exit_block == bb)
	      COPY_REG_SET (&r->live_at_simt_exit, DF_LIVE_IN (bb));
	  }
	if (bitmap_bit_p (&r->simt_bbs, bb->index))
	  active_simt_region = r;
      }

    /* SIMT is not enabled outside the declared regions, if any, and not
       at the start of the entry block (this flag will switch state when
       the entry insn is found).  */
    simt_enabled = (simt_regions->is_empty ()
		    || (active_simt_region
			&& active_simt_region->entry_block != bb));
    entry_found = false;
    exit_found = false;
  }

  /* To be called every time scanning reaches the end of a block.
     Processes any deferred decisions.  */

  void finish_bb ()
  {
    reg_set_iterator iter;
    unsigned int regno;
    EXECUTE_IF_SET_IN_REG_SET (&maybe_assigned_vector, 0, regno, iter)
      {
	if (REGNO_REG_SET_P (&assigned_vector, regno))
	  continue;

	/* Convert each remaining "maybe vector" register to
	   "assigned vector".

	   FIXME: Are there more cases where we can allow scalar values to
	   remain scalar?  Even a single-set, single read, register could be
	   written multiple times in a loop scenario .... but perhaps it's
	   safe when it's set to a constant value (e.g. symbol_ref)?  */
	SET_REGNO_REG_SET (&assigned_vector, regno);
	rescan_needed = true;

	if (dump_file && (dump_flags & TDF_DETAILS))
	  {
	    char buf[10];
	    fprintf (dump_file, "  %s lives beyond bb; must use vector\n",
		     regno_to_string (regno, buf));
	  }
      }
  }

  /* To be called every time scanning moves to the next insn.  */

  void reset_insn_context ()
  {
    use_vector = false;
    uncertain = false;
  }

  /* To be called when the scan encounters a SIMT_ENTER insn.  */

  void found_simt_enter ()
  {
    simt_enabled = true;
    entry_found = true;
  }

  /* To be called when the scan encounters a SIMT_EXIT insn.  */

  void found_simt_exit ()
  {
    simt_enabled = false;
    exit_found = true;
  }

  /* Record that a VGPR hardreg has been found (e.g. an incoming function
     parameter).  This implies that the current insn should be assigned
     for vectorization.  */

  void found_vector_hardreg (unsigned regno)
  {
    SET_REGNO_REG_SET (&done, regno);

    if (simt_enabled)
      {
	SET_REGNO_REG_SET (&assigned_vector, regno);
	use_vector = true;
      }
  }

  /* Record that a scalar hardreg has been found (e.g. stack pointer).
     The current insn might be able to remain scalar.  */

  void found_scalar (unsigned regno)
  {
    SET_REGNO_REG_SET (&done, regno);
  }

  /* Record that a register that does not itself determine the final
     decision has been found (e.g. pseudoreg).  */

  void found_uncertain (unsigned regno)
  {
    if (REGNO_REG_SET_P (&done, regno))
      use_vector |= REGNO_REG_SET_P (&assigned_vector, regno);
    else
      uncertain = true;
  }

  /* Record finding a REG_DEAD REG_NOTE.  These are of interest when they
     help determine if a scalar value is safe to remain scalar.  */

  void found_reg_dead (unsigned regno)
  {
    if (REGNO_REG_SET_P (&maybe_assigned_vector, regno))
      {
	/* A scalar value needs to be made vector if it is assigned to a
	   register in a context where it might only affect some of the
	   threads, but if it is only ever used within the same basic
	   block there is no problem and we can remove the flag.  */
	CLEAR_REGNO_REG_SET (&maybe_assigned_vector, regno);

	if (dump_file && (dump_flags & TDF_DETAILS))
	  {
	    char buf[10];
	    fprintf (dump_file, "      %s dead within bb;"
		     " may remain scalar\n",
		     regno_to_string (regno, buf));
	  }
      }
  }

  /* Record that a vector constant has been found.  The insn will be
     assigned to use vectors.  */

  void found_vector_const ()
  {
    use_vector = true;
  }

  /* This register is written to, in the current insn, so use the values
     discovered from the rest of the insn scan to determine whether to
     assign it to be a vector register, or remain scalar.  */

  void assign_set_regno (unsigned regno)
  {
    if (active_simt_region)
      {
	if (active_simt_region->entry_block == bb && !entry_found)
	  SET_REGNO_REG_SET (&active_simt_region->live_at_simt_enter, regno);
	if (active_simt_region->exit_block == bb && !exit_found)
	  SET_REGNO_REG_SET (&active_simt_region->live_at_simt_exit, regno);
      }

    if (use_vector
	&& !simt_enabled)
      {
	SET_REGNO_REG_SET (&done, regno);

	if (dump_file && (dump_flags & TDF_DETAILS))
	  fprintf (dump_file, "      forced scalar\n");
	return;
      }

    if (!use_vector
	&& !REGNO_REG_SET_P (&assigned_vector, regno)
	&& simt_enabled
	&& single_pred_p (EXIT_BLOCK_PTR_FOR_FN (cfun))
	&& !dominated_by_p (CDI_DOMINATORS,
			    single_pred (EXIT_BLOCK_PTR_FOR_FN (cfun)),
			    bb))
      {
	/* Writing to a scalar in a conditional context is probably not
	   safe; the value will change in threads that aren't supposed to
	   be "doing" anything.

	    FIXME: Is "dominates the function exit" a sufficiently good
	    definition of "conditional context"?  Is it actually safe?
	    Conversely, is it too conservative?  */
	SET_REGNO_REG_SET (&maybe_assigned_vector, regno);

	if (dump_file && (dump_flags & TDF_DETAILS))
	  {
	    char buf[10];
	    fprintf (dump_file,
		     "      %s may need to use vector (conditional context)\n",
		     regno_to_string (regno, buf));
	  }
      }

    if (use_vector
	&& REGNO_REG_SET_P (&done, regno)
	&& !REGNO_REG_SET_P (&assigned_vector, regno)
	&& !SCALAR_HARDREG_P (regno))
      {
	/* We assigned vector state to a register that was previously detected
	   as scalar.  This will need to be backward-propagated to BBs already
	   completed.  */
	rescan_needed = true;

	if (dump_file && (dump_flags & TDF_DETAILS))
	  fprintf (dump_file,
		   "      register mode has changed;"
		   " another pass will be needed\n");
      }

    if (use_vector)
      {
	if (SCALAR_HARDREG_P (regno))
	  /* No-op; stack adjustments etc. need to be allowed.  */;
	else
	  SET_REGNO_REG_SET (&assigned_vector, regno);
	SET_REGNO_REG_SET (&done, regno);
      }
    else if (REGNO_REG_SET_P (&done, regno)
	     && REGNO_REG_SET_P (&assigned_vector, regno))
      use_vector = true;
    else if (!uncertain)
      SET_REGNO_REG_SET (&done, regno);
  }

  /* To be called when the insn has been fully scanned.  Use the data gathered
     to determine whether the insn will be vectorized, or left scalar.  */

  void assign_insn (const rtx_insn *insn)
  {
    if (active_simt_region)
      {
	if (active_simt_region->entry_block == bb && !entry_found)
	  for (rtx note = REG_NOTES (insn); note; note = XEXP (note, 1))
	    if (REG_NOTE_KIND (note) == REG_DEAD
		|| REG_NOTE_KIND (note) == REG_UNUSED)
	      CLEAR_REGNO_REG_SET (&active_simt_region->live_at_simt_enter,
				   REGNO (XEXP (note, 0)));
	if (active_simt_region->exit_block == bb && !exit_found)
	  for (rtx note = REG_NOTES (insn); note; note = XEXP (note, 1))
	    if (REG_NOTE_KIND (note) == REG_DEAD
		|| REG_NOTE_KIND (note) == REG_UNUSED)
	      CLEAR_REGNO_REG_SET (&active_simt_region->live_at_simt_exit,
				   REGNO (XEXP (note, 0)));
      }

    if (use_vector
	&& !simt_enabled)
      {
	if (dump_file)
	  fprintf (dump_file, "!!!!!!!!!!!!!! SIMT NOT ENABLED !!!!!!!!!!!!!\n");
	use_vector = false;
	return;
      }

    if (use_vector)
      bitmap_set_bit (&vector_insns, INSN_UID (insn));
    else if (uncertain)
      bitmap_set_bit (&unfinished_bbs, bb->index);
  }

  /* Return true if BB[INDEX] still has unassigned insns.  */

  bool bb_incomplete_p (unsigned index)
  {
    return (rescanning || bitmap_bit_p (&unfinished_bbs, index));
  }

  /* Return a string representing the decision made by assign_set_regno, for
     printing to dump files.  */

  const char *dump_reg_assignment (unsigned regno)
  {
    if (REGNO_REG_SET_P (&done, regno))
      return (REGNO_REG_SET_P (&assigned_vector, regno)
	      ? "vector"
	      : "scalar");
    else
      return "<unknown>";
  }

  /* Return a string representing the decision made by assign_insn, for
     printing to dump files.  */

  const char *dump_current_insn_assignment ()
  {
    if (use_vector)
      return "vector";
    else if (uncertain)
      return "<unknown>";
    else
      return "scalar";
  }

  /* Call back for note_uses. DATAP holds THIS.  */

  static void
  callback_scan_uses (rtx *x, void *datap)
  {
    simt_vect_scan *data = (simt_vect_scan*)datap;

    subrtx_iterator::array_type array;
    FOR_EACH_SUBRTX (iter, array, *x, NONCONST)
      {
	const_rtx x = *iter;

	if (REG_P (x))
	  {
	    unsigned reg = REGNO (x);
	    if (VGPR_REGNO_P (reg) || AVGPR_REGNO_P (reg))
	      data->found_vector_hardreg (reg);
	    else if (HARD_REGISTER_NUM_P (reg))
	      data->found_scalar (reg);
	    else
	      data->found_uncertain (reg);

	    if (dump_file && (dump_flags & TDF_DETAILS))
	      {
		fprintf (dump_file, "      use: ");
		dump_value_with_mode (dump_file, x);
		fprintf (dump_file, " is %s\n", data->dump_reg_assignment (reg));
	      }
	  }
	else if (CONST_INT_P (x) || CONST_DOUBLE_P (x) || SYMBOL_REF_P (x))
	  {
	    /* No-op; these can be converted any way we need.  */

	    if (dump_file && (dump_flags & TDF_DETAILS))
	      {
		fprintf (dump_file, "      scalar constant: ");
		dump_value_with_mode (dump_file, x);
		fprintf (dump_file, "\n");
	      }
	  }
	else if (CONST_VECTOR_P (x))
	  {
	    data->found_vector_const ();

	    if (dump_file && (dump_flags & TDF_DETAILS))
	      {
		fprintf (dump_file, "      vector constant: ");
		dump_value_with_mode (dump_file, x);
		fprintf (dump_file, "\n");
	      }
	  }

      }
  }

  /* Call back for note_stores. DATAP holds THIS.  */

  static void
  callback_scan_sets (rtx x, const_rtx parent, void *datap)
  {
    simt_vect_scan *data = (simt_vect_scan*)datap;

    if (GET_CODE (parent) == CLOBBER
	&& REG_P (x)
	&& SCALAR_HARDREG_P (REGNO (x)))
      {
	if (dump_file && (dump_flags & TDF_DETAILS))
	  {
	    fprintf (dump_file, "      hardreg clobber: ");
	    dump_value_with_mode (dump_file, x);
	    fprintf (dump_file, " (ignored)\n");
	  }

	return;
      }

    if (REG_P (x))
      {
	unsigned reg = REGNO (x);
	data->assign_set_regno (reg);

	if (dump_file && (dump_flags & TDF_DETAILS))
	  {
	    fprintf (dump_file, "      set: ");
	    dump_value_with_mode (dump_file, x);
	    fprintf (dump_file, " -> %s\n", data->dump_reg_assignment (reg));
	  }
      }
  }

  /* Do one complete pass over every block and insn in the function.  Must
     be called repeatedly until it returns true, indicating nothing left to do.

     First pass: find root vector/scalar requirements, and forward-propagate
     through the dependency chain.  At the end of this pass there may be
     registers that remain unclassified, where there are cross-block
     dependencies.
     Second pass: now every instruction has been visited once, we can rescan
     and identify any registers that started scalar, but converted to vector
     later (basically backward-propagation).
     Third pass, plus: repeat only basic blocks marked unfinished, if any.  */

  bool do_one_scan_pass ()
  {
    rescanning = rescan_needed;
    rescan_needed = false;

    FOR_EACH_BB_FN (bb, cfun)
      {
	if (!bb_incomplete_p (bb->index))
	  continue;

	if (dump_file && (dump_flags & TDF_DETAILS))
	  fprintf (dump_file, "BB %d:\n", bb->index);

	reset_bb_context (bb);

	rtx_insn *insn, *curr;
	FOR_BB_INSNS_SAFE (bb, insn, curr)
	  {
	    if (!NONDEBUG_INSN_P (insn))
	      continue;

	    if (dump_file && (dump_flags & TDF_DETAILS))
	      dump_insn_no_notes (dump_file, insn);

	    if (recog_memoized (insn) == CODE_FOR_omp_simt_exit)
	      {
		if (dump_file && (dump_flags & TDF_DETAILS))
		  fprintf (dump_file, "Exiting SIMT region\n");

		found_simt_exit ();
	      }

	    reset_insn_context ();
	    note_uses (&PATTERN (insn), callback_scan_uses, this);
	    note_stores (insn, callback_scan_sets, this);

	    for (rtx note = REG_NOTES (insn); note; note = XEXP (note, 1))
	      if (REG_NOTE_KIND (note) == REG_DEAD
		  || REG_NOTE_KIND (note) == REG_UNUSED)
	      found_reg_dead (REGNO (XEXP (note, 0)));

	    assign_insn (insn);
	    if (dump_file)
	      fprintf (dump_file, "      assign insn %d -> %s\n",
		       INSN_UID (insn),
		       dump_current_insn_assignment ());

	    if (recog_memoized (insn) == CODE_FOR_omp_simt_enter)
	      {
		if (dump_file && (dump_flags & TDF_DETAILS))
		  fprintf (dump_file, "Entering SIMT region\n");

		found_simt_enter ();
	      }
	  }

	finish_bb ();
      }

    return (!rescan_needed && bitmap_empty_p (&unfinished_bbs));
  }

  /* Repeatedly scan the function insns until all are classified as
     either vector or scalar.  */

  void do_function_analysis ()
  {
    calculate_dominance_info (CDI_DOMINATORS);

    bool finished = false;
    for (int count = 1; !finished; count++)
      {
	gcc_assert (count < 100);
	if (dump_file)
	  fprintf (dump_file,
		   "================= Scan pass #%d =================\n",
		   count);

	finished = do_one_scan_pass ();

	if (dump_file && (dump_flags & TDF_DETAILS))
	  fprintf (dump_file, "\n\n");
      }

    free_dominance_info (CDI_DOMINATORS);
  }
};

/* Helper function to map scalar modes to the equivalent vector mode.  */

static machine_mode
vectorize_mode (machine_mode scalar_mode)
{
  switch (scalar_mode)
    {
    case E_VOIDmode:
      return VOIDmode;
      break;

    /* BImode values map to DImode masks.  */
    case E_BImode:
      return DImode;
      break;

    /* Regular scalars map to vector equivalents.  */
    case E_QImode:
      return V64QImode;
      break;
    case E_HImode:
      return V64HImode;
      break;
    case E_SImode:
      return V64SImode;
      break;
    case E_DImode:
      return V64DImode;
      break;
    case E_TImode:
      return V64TImode;
      break;
    case E_SFmode:
      return V64SFmode;
      break;
    case E_DFmode:
      return V64DFmode;
      break;

    /* Complex modes are not available as vectors, but they're also not used
       anywhere in the scalar instructions, so they should only ever occur on
       call instructions with hardregs.  Just use an equivalent sized
       vector mode.  */
    case CQImode:
    case HCmode:
    case CHImode:
      return V64SImode;
    case SCmode:
    case CSImode:
      return V64DImode;
    case DCmode:
    case CDImode:
      return V64TImode;

    default:
      break;
    }

    gcc_unreachable ();
}

/* Convert scalar register OLDREG to the equivalent vector version, and return
   it.  CACHE ensures that each pseudoreg is always replaced with the
   same new pseudoreg.  */

static rtx
transform_reg (rtx oldreg, vec<rtx> &cache)
{
  rtx newreg;
  /* Pseudoregs normally all share the same underlying rtx
     instance, and therefore we must replace them with a new pseudoreg, or else
     the occurrences outside any SIMT_ENTER/SIMT_EXIT region would need different
     modes to the ones inside, which would not work.  We therefore remember and
     reuse the newly created pseudoregs as we go.  The scalar/vector
     conversions will be inserted later, if needed.

     Hard registers, conversely, do not normally share a single rtx, and are
     not required to have the same mode, but we must keep the same regno
     (except for SCC which is vectorized by replacing it with VCC).  */
  int regno = REGNO (oldreg);
  if (HARD_REGISTER_NUM_P (regno))
    {
      if (regno == SCC_REG)
	regno = VCC_REG;
      newreg = gen_rtx_REG (vectorize_mode (GET_MODE (oldreg)), regno);
      set_reg_attrs_from_value (newreg, oldreg);
    }
  else if (cache[regno])
    newreg = cache[regno];
  else
    {
      machine_mode vmode = vectorize_mode (GET_MODE (oldreg));
      newreg = gen_reg_rtx_and_attrs (oldreg);
      PUT_MODE_RAW (newreg, vmode);
      cache[regno] = newreg;
    }
  return newreg;
}

/* Scan the insns to find SIMT_ENTER/SIMT_EXIT statements.  If present, the
   function enters in a scalar context (we have a kernel function);
   otherwise we assume a vector context throughout (e.g. a library
   function).

   Returns false if the pass should abort (all-scalar function).
   FIXME: Add OACC FORK/JOIN.  */

static bool
find_simt_regions (vec<simt_region*> &simt_regions, function *fn)
{
  basic_block bb;
  hash_map<basic_block,rtx_insn *> simt_entries, simt_exits;
  FOR_EACH_BB_FN (bb, fn)
    {
      rtx_insn *insn;
      FOR_BB_INSNS (bb, insn)
	{
	  if (!NONDEBUG_INSN_P (insn))
	    continue;

	  int insn_code = recog_memoized (insn);
	  switch (insn_code)
	    {
	    default:
	      break;

	    case CODE_FOR_omp_simt_enter:
	      simt_entries.put (bb, insn);
	      break;

	    case CODE_FOR_omp_simt_exit:
	      simt_exits.put (bb, insn);
	      break;
	    }
	}
    }
  gcc_assert (simt_entries.elements () == simt_exits.elements ());
  for (auto e : simt_entries)
    {
      simt_region *r = new simt_region (e.first, e.second, &simt_exits);
      simt_regions.safe_push (r);
    }

  if (!cfun->machine->normal_function
      && simt_regions.is_empty ())
    {
      /* If the function is not "normal" then it's a kernel entry point, which
	 means we're entering in a scalar context.  If it never switches
	 to SIMT mode anywhere we can skip processing the whole function.  */
      if (dump_file)
	fprintf (dump_file,
		 "Kernel entry function; using scalar throughout.\n\n");
      return false;
    }
  if (!simt_regions.is_empty ())
    {
      if (dump_file)
	{
	  for (auto r : simt_regions)
	    {
	      fprintf (dump_file,
		       "Using SIMT region between %d and %d, including ... \n",
		       r->entry_block->index, r->exit_block->index);
	      bitmap_print (dump_file, &r->simt_bbs, "   bb ", "\n");
	    }
	  fprintf (dump_file, "\n");
	}

      if (optimize == 1)
	{
	  df_live_add_problem ();
	  df_live_set_all_dirty ();
	}
      /* DF is disabled at -O0, so attempting to use it ICEs.  In which case,
	 it should be safe to assume no live registers, so skip DF.  */
      if (optimize)
	df_analyze ();
    }
  else if (dump_file)
    fprintf (dump_file, "Using SIMT throughout.\n\n");

  return true;
}

unsigned int
pass_gcn_simt_vectorize::execute (function *fn)
{
  /* Vector modes are enabled, for this function, from this point onwards.  */
  cfun->machine->vectors_enabled = true;

  /* Analyze the blocks and instructions, and classify each instruction and
     register for SIMT conversion, or not.  */
  vec<simt_region*> simt_regions = vNULL;
  if (!find_simt_regions (simt_regions, fn))
    return 0;
  simt_vect_scan scan(&simt_regions);

  /* Initial analysis is now complete.  All instructions and registers have
     been classified as scalar or vector.  */

  if (dump_file)
    fprintf (dump_file, "================= Transform pass =================\n");

  /* Records which vector pseudoreg replaces which scalar pseudo.
     Indexed by regno.  Populated on demand.  */
  auto_vec<rtx> converted_regs;
  converted_regs.safe_grow_cleared (reg_rtx_no, true);

  /* Temporary worklists for deferred transformation tasks.
     Reset for each insn.  */
  auto_vec<rtx*> subreg_writes, subreg_reads, mem_writes, mem_reads;

  /* Temporary storage for rebuilding PARALLEL RTX.
     Reset for each insn.  */
  auto_vec<rtx> parallelvec;

  /* Temporary storage to record RTX elements that do not need conversion.
     Reset for each insn.  */
  hash_set<rtx> skiplist, skiprecursivelist;

  /* Temporary storage to ensure that the same scalar doesn't get
     duplicated to multiple pseudos in the same insn.
     Reset for each insn.  */
  hash_map<int_hash<int, -1, -2>,rtx> vecdup_cache;

  basic_block bb;
  FOR_EACH_BB_FN (bb, fn)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "BB %d:\n", bb->index);

      rtx_insn *insn, *curr;
      FOR_BB_INSNS_SAFE (bb, insn, curr)
	{
	  if (!NONDEBUG_INSN_P (insn))
	    continue;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    dump_insn_no_notes (dump_file, insn);

	  int orig_insn_code = recog_memoized (insn);
	  bool is_asm = (orig_insn_code < 0
			 && asm_noperands (PATTERN (insn)) >= 0);

	  /* Skip unrecognized instructions.  There shouldn't be any of
	     these, but if there are we wouldn't know what to do with them.  */
	  if (orig_insn_code < 0 && !is_asm)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file,
			 "  '%s' -> unrecognized; no transform\n",
			 (orig_insn_code < 0 ? "<unknown>"
			  : get_insn_name (orig_insn_code)));

	      continue;
	    }

	  /* Skip instructions previously identified as remaining scalar.  */
	  if (!scan.transform_insn_p (INSN_UID (insn)))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file,
			 "  '%s' -> scalar only; no transform\n",
			 (orig_insn_code < 0 ? "<unknown>"
			  : get_insn_name (orig_insn_code)));

	      continue;
	    }

	  /* Initialize the mode we will use to convert CONST_INT and
	     CONST_DOUBLE to CONST_VECTOR.  The actual value might come
	     from a special case, and insn attribute, or from the SET
	     destination mode.  */
	  machine_mode insn_mode = VOIDmode;

	  /* Read any "simt_const" attribute from the machine description.
	     This is used to determine a value for INSN_MODE, which is used
	     as the mode for CONST_VECTOR (which must have a proper mode,
	     unlike the CONST_INT or CONST_DOUBLE they're transformed from).  */
	  attr_simt_const const_attr = get_attr_simt_const (insn);
	  if (const_attr)
	    switch (const_attr)
	      {
	      case SIMT_CONST_DEFAULT:
		/* No attribute was set.  */
		break;
	      case SIMT_CONST_SI2:
	      case SIMT_CONST_SEXTDI2:
	      case SIMT_CONST_UEXTDI2:
		/* These will be identified later, as we walk the RTX.  */
		break;
	      case SIMT_CONST_CSTORE:
		{
		  /* A cstore instruction's operand mode is found within the
		     comparison operator.

		     (parallel [(set () (op (reg:MODE)))])
		     or
		     (set () (op (reg:MODE))) */
		  rtx set = PATTERN (insn);
		  if (GET_CODE (set) == PARALLEL)
		    set = XVECEXP (set, 0, 0);
		  gcc_checking_assert (GET_CODE (set) == SET);
		  rtx op = XEXP (set, 1);
		  rtx val = XEXP (op, 0);
		  if (GET_MODE (val) == VOIDmode)
		    val = XEXP (op, 1);

		  insn_mode = vectorize_mode (GET_MODE (val));
		  gcc_checking_assert (insn_mode != VOIDmode);
		}
	      }

	  /* Handle a few special cases early.  */
	  switch (orig_insn_code)
	    {
	    case CODE_FOR_cjump:
		{
		  /* A cjump instruction's operand mode is different to the
		     SET mode.  We need a custom pattern to find INSN_MODE.

		     (set () (if_then_else (op:MODE)))  */
		  rtx set = PATTERN (insn);
		  rtx if_then_else = XEXP (set, 1);
		  rtx op = XEXP (if_then_else, 0);
		  rtx val = XEXP (op, 0);

		  insn_mode = vectorize_mode (GET_MODE (val));
		  gcc_assert (insn_mode != VOIDmode);

		  break;
		}

	    case CODE_FOR_gcn_indirect_call:
	    case CODE_FOR_gcn_call_value_indirect:
		{
		  /* We can only call one function at a time, so we need to
		     insert a loop to call each address, in the case that
		     all the lanes don't call the same destination.  */
		  rtx parallel = PATTERN (insn);
		  rtx call = XVECEXP (parallel, 0, 0);
		  if (GET_CODE (call) == SET)
		    call = XEXP (call, 1);
		  rtx mem = XEXP (call, 0);
		  rtx vectorreg = XEXP (mem, 0);
		  if (!scan.transform_reg_p (REGNO (vectorreg)))
		    /* The address is already scalar, so all is well.  */
		    break;
		  rtx vectoraddr = transform_reg (vectorreg, converted_regs);

		  /* FIXME implement the loop!
		     HACK For now, just take the address from v_firstlane.  */
		  rtx scalarreg = gen_reg_rtx (DImode);
		  rtx extract = gen_extract_first_v64di (scalarreg, vectoraddr);
		  if (dump_file && optimize)
		    fprintf (dump_file, "      df ");
		  rtx_insn *new_insn = emit_insn_before (extract, insn);
		  XEXP (mem, 0) = scalarreg;
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "      emit extract_first:");
		      dump_insn_no_notes (dump_file, new_insn);
		      fprintf (dump_file, "      substitute: ");
		      dump_value_with_mode (dump_file, vectorreg);
		      fprintf (dump_file, " -> ");
		      dump_value_with_mode (dump_file, scalarreg);
		    }

		  break;
		}

	    default:
	      /* No special handling needed.  */
	      ;
	    }

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  '%s' -> transform to vector\n",
		     (orig_insn_code < 0 ? "<unknown>"
		      : get_insn_name (orig_insn_code)));

	  /* Initialize state tracking for rebuilding a PARALLEL.  */
	  parallelvec.truncate (0);
	  int clobber_digit = 1;
	  int parallel_loc = 0;
	  int clobber_attr = get_attr_simt_clobber (insn);
	  bool clobbers_changed = false;

	  /* Initialize state tracking for converting the insn.  */
	  attr_simt_special special_attr = get_attr_simt_special (insn);
	  int set_count = 0;
	  int input_counter = 0;
	  bool pc_write = false;
	  subreg_writes.truncate (0);
	  subreg_reads.truncate (0);
	  mem_writes.truncate (0);
	  mem_reads.truncate (0);
	  skiplist.empty ();
	  skiprecursivelist.empty ();
	  vecdup_cache.empty ();

	  /* Walk the RTX for the entire INSN.
	     This is a top-down walk, but walks PARALLEL entries in reverse,
	     starting with the clobbers, and finishing with the SET.  If that
	     ever changes then this algorithm will break.  */
	  subrtx_ptr_iterator::array_type array;
	  FOR_EACH_SUBRTX_PTR (iter, array, &PATTERN (insn), ALL)
	    {
	      rtx x = **iter;
	      rtx *delayed_substitute = NULL;
	      int clobber_code;
	      bool do_transform = false;
	      bool do_dump = true;
	      machine_mode mode;

	      /* Check if "x" is related to something previously
		 found to not need conversion.  */
	      if (skiplist.contains (x))
		{
		  /* "x" is to be skipped (not edited), but its subrtxes
		     are to be transformed as normal.  */
		  skiplist.remove (x);

		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "      skip: ");
		      dump_value_with_mode (dump_file, x);
		      fprintf (dump_file, "\n");
		    }
		  continue;
		}
	      if (skiprecursivelist.contains (x))
		{
		  /* Both "x" and its subrtxes are to be skipped (not
		     edited).  */
		  iter.skip_subrtxes ();
		  skiprecursivelist.remove (x);

		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "      skip recursively: ");
		      dump_value_with_mode (dump_file, x);
		      fprintf (dump_file, "\n");
		    }
		  continue;
		}

	      switch (GET_CODE (x))
		{
		case SET:
		  /* Handle instructions marked in the machine description
		     (set_attr "simt_special" "set_vcc").  The effect is to
		     leave the mode unchanged on the specific elements.

		     Note that FOR_EACH_SUBRTX walks backwards, so
		     "set_count == 0" means we're at the secondary SET clause.
		   */
		  if (set_count++ == 0
		      && special_attr == SIMT_SPECIAL_SET_VCC)
		    {
		      /* The pattern looks like this:

			   (parallel   <---------- not scanned yet
			     [(set (anything)
				   (anything))
			      (set <---------------------- "x" is here
			           (reg:DI VCC)  <----- skip this
			           (something:DI   <------ and this
					(anything)))]  <----- but not this
			
			 We want to transform "anything", as usual, but
			 the "DI" modes need to remain as-is.  */
		      rtx setdst = XEXP (x, 0);
		      rtx setsrc = XEXP (x, 1);
		      skiprecursivelist.add (setdst);
		      skiplist.add (setsrc);

		      if (dump_file && (dump_flags & TDF_DETAILS))
			fprintf (dump_file, "      special (set_vcc): ");

		      /* The insn_mode will need to be detected from the other
			 SET, but waiting to get there is too late.  */
		      if (insn_mode == VOIDmode)
			{
			  rtx parallel = PATTERN (insn);
			  gcc_checking_assert (GET_CODE (parallel) == PARALLEL);
			  rtx otherset = XVECEXP (parallel, 0, 0);
			  gcc_checking_assert (GET_CODE (otherset) == SET);
			  rtx otherdst = XEXP (otherset, 0);

			  insn_mode = vectorize_mode (GET_MODE (otherdst));
			}

		      break;
		    }

		  /* SET doesn't need converting itself (there's no mode),
		     but it's a good place to probe some properties of the
		     INSN.  */
		  do_dump = false;

		 /* This is the main place we detect the general mode
		    of the INSN (if no special cases apply).  */
		  if (insn_mode == VOIDmode)
		    insn_mode = vectorize_mode (GET_MODE (XEXP (x, 0)));

		  /* When we walk to a MEM we won't know if it's reading
		     or writing, so make a note now, for later.  */
		  if (MEM_P (SET_DEST (x)) && VECTOR_MODE_P (insn_mode))
		    mem_writes.safe_push (&SET_DEST (x));

		  /* When we walk to a SUBREG we won't know if it's reading
		     or writing, so make a note now, for later.
		     Also, a SUBREG can be hiding a MEM.  */
		  if (SUBREG_P (SET_DEST (x)) && VECTOR_MODE_P (insn_mode))
		    {
		      subreg_writes.safe_push (&SET_DEST (x));

		      if (MEM_P (SUBREG_REG (SET_DEST (x))))
			mem_writes.safe_push (&SUBREG_REG (SET_DEST (x)));
		    }

		  /* Special case:
		     Writes to the stack pointer, and other explicit scalar
		     register uses, can't be vectorized, so need to be treated
		     specially.  In most cases all the threads will be trying
		     to make the same adjustment (ahead of a function call that
		     takes stack parameters, for instance) so we can just take
		     the value from the first active lane.
		     FIXME: how to detect exceptions????  */
		  if (REG_P (SET_DEST (x))
		      && SCALAR_HARDREG_P (REGNO (SET_DEST (x))))
		    {
		      rtx dest = SET_DEST (x);
		      machine_mode mode = vectorize_mode (GET_MODE (dest));
		      rtx tmp = gen_reg_rtx (mode);

		      if (dump_file && (dump_flags & TDF_DETAILS))
			{
			  fprintf (dump_file, "      special: ");
			  dump_value_with_mode (dump_file, dest);
			  fprintf (dump_file, " -> ");
			  dump_value_with_mode (dump_file, tmp);
			  fprintf (dump_file, " (scalar hardreg write)\n");
			}

		      if (dump_file && optimize)
			fprintf (dump_file, "      df ");
		      rtx_insn *new_insn =
			emit_insn_after (gen_extract_first (mode, dest, tmp),
					 insn);

		      if (dump_file && (dump_flags & TDF_DETAILS))
			{
			  fprintf (dump_file, "      add SGPR write insn: ");
			  dump_insn_no_notes (dump_file, new_insn);
			}

		      SET_DEST (x) = tmp;
		    }
		  break;

		case PARALLEL:
		  /* PARALLEL doesn't need converting itself (no mode), but it
		     will contain CLOBBER that might need to be deleted, so
		     prepare for that now by taking a copy.  */
		  parallel_loc = XVECLEN (x, 0);
		  parallelvec.safe_grow (XVECLEN (x, 0));
		  for (int i = 0; i < XVECLEN (x, 0); i++)
		    parallelvec[i] = XVECEXP (x, 0, i);

		  do_dump = false;
		  break;

		case CALL:
		  /* Don't transform CALL operators.  */
		  iter.skip_subrtxes ();
		  do_dump = false;
		  break;

		case LSHIFTRT:
		case ASHIFT:
		case ASHIFTRT:
		  /* Handle shift instructions marked up with
		     (set_attr "simt_special" "scalar_shiftop").  */
		  if (special_attr == SIMT_SPECIAL_SCALAR_SHIFTOP)
		    {
		      /* The second parameter (the shift amount) is not to
			 be converted; all lanes will use a scalar value.
			 Note: other shift patterns with vector shift
			 amounts may exist.  */
		      delayed_substitute = &XEXP (x, 0);

		      if (dump_file && (dump_flags & TDF_DETAILS))
			{
			  fprintf (dump_file, "      no-op (scalar_shiftop): ");
			  dump_value_with_mode (dump_file, XEXP (x, 1));
			  fprintf (dump_file, "\n");
			}
		    }

		  do_transform = true;
		  break;

		case CLOBBER:
		  /* Handle the "simt_clobber" insn attribute.
		     See the gcn.md for the encoding details.
		     CLOBBER_DIGIT is increased every time we come here, so
		     each clobber uses a different digit from the attribute.
		     Note: The iterator presents PARALLEL in reverse order,
		     so we start reading the encoding from the LSB.  */
		  clobber_code = (clobber_attr / clobber_digit) % 10;

		  switch (clobber_code)
		    {
		    case 0: /* Unset.  */
		    default:
		      if (!is_asm)
			gcc_unreachable ();
		      /* Fall through.  */

		    case 1: /* Keep clobber as-is.  */
		      if (dump_file && (dump_flags & TDF_DETAILS))
			fprintf (dump_file, "      keep: ");

		      /* Place the clobber in the output PARALLEL unchanged.  */
		      parallel_loc--;
		      iter.skip_subrtxes ();
		      break;

		    case 2: /* Delete clobber.  */
		      if (dump_file && (dump_flags & TDF_DETAILS))
			fprintf (dump_file, "      delete: ");

		      /* Do not place the clobber in the output PARALLEL.  */
		      parallelvec.ordered_remove (--parallel_loc);
		      clobbers_changed = true;

		      iter.skip_subrtxes ();
		      break;

		    case 3: /* Transform scalar clobber to vector.  */
		      **iter = x = copy_rtx_if_shared (x);
		      if (dump_file && (dump_flags & TDF_DETAILS))
			fprintf (dump_file, "      transform: ");

		      /* The clobber will appear in the output PARALLEL, but
			 with the contents transformed (i.e. not skipped).  */
		      parallel_loc--;
		      break;
		    }

		  clobber_digit *= 10;
		  break;

		case USE:
		  /* The USE needs to appear in the output PARALLEL.  */
		  parallel_loc--;
		  break;

		case CONST_INT:
		case CONST_DOUBLE:
		  /* Scalar constants might need to be converted to vector
		     constants, which is a pain because CONST_VECTOR requires
		     a real mode.  The mode recorded in INSN_MODE should be
		     the right thing (if it isn't, a new special case or
		     insn attributes might be needed).  */
		  iter.skip_subrtxes ();
		  input_counter++;

		  if (!VECTOR_MODE_P (insn_mode))
		    {
		      if (dump_file && (dump_flags & TDF_DETAILS))
			fprintf (dump_file, "      no need to convert const: ");

		      break;
		    }

		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "      convert const: ");
		      dump_value_with_mode (dump_file, x);
		      fprintf (dump_file, " -> ");
		    }

		  /* Select the mode for the CONST_VECTOR.  */
		  mode = insn_mode;

		  /* Apply any "simt_const" attribute setting.  */
		  if ((const_attr == SIMT_CONST_SI2
		       || const_attr == SIMT_CONST_SEXTDI2
		       || const_attr == SIMT_CONST_UEXTDI2)
		      /* FIXME: this is fragile!  */
		      && input_counter == 3)
		    mode = V64SImode;

		  /* Duplicate the constant 64x.  */
		  x = gen_const_vec_duplicate (mode, x);

		  /* Handle "simt_const" attributes that add extends.  */
		  if (const_attr == SIMT_CONST_SEXTDI2
		      && input_counter == 3)
		    x = gen_rtx_SIGN_EXTEND (V64DImode, x);
		  else if (const_attr == SIMT_CONST_UEXTDI2
			   && input_counter == 3)
		    x = gen_rtx_ZERO_EXTEND (V64DImode, x);

		  **iter = x;
		  break;

		case SYMBOL_REF:
		case LABEL_REF:
		  /* These only occur in movdi_symbol, or in jumps, so we need
		     to either make them fit movv64di_symbol, or leave them
		     alone.  */
		  iter.skip_subrtxes ();

		  /* Handle (set_attr "simt_special" "symbols_ok").  */
		  if (special_attr != SIMT_SPECIAL_SYMBOLS_OK)
		    {
		      rtx tmpdi = gen_reg_rtx (GET_MODE (x));
		      rtx movdi_symbol = gen_movdi_symbol (tmpdi, x);
		      emit_insn_before (movdi_symbol, insn);

		      rtx tmpv64di = gen_reg_rtx (insn_mode);
		      rtx vecdup = gen_rtx_fmt_e (VEC_DUPLICATE, insn_mode,
						  tmpdi);
		      rtx vecdup_insn = gen_rtx_SET (tmpv64di, vecdup);
		      emit_insn_before (vecdup_insn, insn);

		      **iter = tmpv64di;

		      if (dump_file && (dump_flags & TDF_DETAILS))
			{
			  fprintf (dump_file, "      insert vec_dup around: ");
			  dump_value_with_mode (dump_file, x);
			  fprintf (dump_file, "\n");
			}
		    }

		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "      no-op: ");
		  break;

		case SUBREG:
		  /* This subreg might be a read or a write, but the writes
		     (already found above) will be filtered out later.  */
		  if (VECTOR_MODE_P (insn_mode))
		    subreg_reads.safe_push (*iter);
		  do_transform = true;
		  break;

		case REG:
		  iter.skip_subrtxes ();
		  input_counter++;

		  /* Registers classified to remain scalar within insns
		     classified to be vectorized need VEC_DUPLICATE.
		     Note that special cases exist (e.g. shift amounts),
		     but those will have set skip_subrtxes and we won't have
		     got here.  */
		  if (((!scan.transform_reg_p (REGNO (x))
			&& !scan.scalar_reg_needs_vec_duplicate_p (REGNO (x),
								   bb))
		       || SCALAR_HARDREG_P (REGNO (x)))
		      && !VECTOR_MODE_P (GET_MODE (x)))
		    {
		      rtx *cached_var = vecdup_cache.get (REGNO (x));
		      rtx tmpvar;

		      if (cached_var)
			/* This is not the first time we encountered this
			   register in this insn.  Reuse the same duplicated
			   pseudoreg.  */
			tmpvar = *cached_var;
		      else if (GET_MODE (x) == BImode)
			{
			  /* BImode gets duplicated to DImode, not an actual
			     vector mode.  Only active lanes are selected.  */
			  rtx execreg = gen_rtx_REG (DImode, EXEC_REG);
			  tmpvar = gen_reg_rtx_and_attrs (x);
			  PUT_MODE_RAW (tmpvar, DImode);
			  rtx cselect = gen_cselect (DImode, tmpvar, x,
						     execreg, const0_rtx);
			  vecdup_cache.put (REGNO (x), tmpvar);

			  if (dump_file && optimize)
			    fprintf (dump_file, "      df ");
			  rtx_insn *new_insn = emit_insn_before (cselect, insn);
			  if (dump_file && (dump_flags & TDF_DETAILS))
			    {
			      fprintf (dump_file, "      emit bi>di convert:");
			      dump_insn_no_notes (dump_file, new_insn);
			    }
			}
		      else
			{
			  /* Any other register gets duplicated into a new
			     temporary pseudoreg.  */
			  machine_mode vmode = vectorize_mode (GET_MODE (x));
			  tmpvar = gen_reg_rtx_and_attrs (x);
			  PUT_MODE_RAW (tmpvar, vmode);
			  rtx vecdup = gen_rtx_fmt_e (VEC_DUPLICATE, vmode, x);
			  rtx vecdup_insn = gen_rtx_SET (tmpvar, vecdup);
			  vecdup_cache.put (REGNO (x), tmpvar);

			  if (dump_file && optimize)
			    fprintf (dump_file, "      df ");
			  rtx_insn *new_insn = emit_insn_before (vecdup_insn,
								 insn);
			  if (dump_file && (dump_flags & TDF_DETAILS))
			    {
			      fprintf (dump_file, "      emit vec_dup:");
			      dump_insn_no_notes (dump_file, new_insn);
			    }
			}

		      **iter = tmpvar;
		      if (dump_file && (dump_flags & TDF_DETAILS))
			{
			  fprintf (dump_file, "      substitute: ");
			  dump_value_with_mode (dump_file, x);
			  fprintf (dump_file, " -> ");
			}
		    }
		  else
		    /* Other registers just get the standard transformation
		       that is applied to all non-special RTX codes.  */
		    do_transform = true;
		  break;

		case MEM:
		  /* This MEM might be a read or a write, but the writes
		     (already found above) will be filtered out later.  */
		  if (VECTOR_MODE_P (insn_mode))
		    mem_reads.safe_push (*iter);

		  /* The transformed MEM will represent a vector of
		     independent addresses.  */
		  MEM_INDEPENDENT_P (x) = 1;

		  /* The address space might also need to be vectorized.  */
		  if (MEM_ADDR_SPACE (x) == ADDR_SPACE_SCALAR_FLAT)
		    set_mem_addr_space (x, ADDR_SPACE_FLAT);

		  do_transform = true;
		  break;

		case UNSPEC:
		  /* Some UNSPECs have special handling.  */
		  switch (XINT (x, 1))
		    {
		    case UNSPEC_SIMT_XCHG_BFLY:
		    case UNSPEC_DIV_FMAS:
		      /* The second unspec argument remains scalar, but
			 we do want to recurse into the first argument.  */
		      delayed_substitute = &XVECEXP (x, 0, 0);
		      do_transform = true;

		      if (dump_file && (dump_flags & TDF_DETAILS))
			{
			  fprintf (dump_file, "      no-op: ");
			  dump_value_with_mode (dump_file, XVECEXP (x, 0, 1));
			  fprintf (dump_file, " (special case)\n");
			}
		      break;

		    default:
		      do_transform = true;
		    }
		  break;

		case IF_THEN_ELSE:
		  if (pc_write)
		    {
		      /* This IF_THEN_ELSE occurs in a cbranch, in which case
			 converting the modes would be wrong.  */
		      do_transform = true;
		      break;
		    }

		  /* Scalar IF_THEN_ELSE needs to be translated to
		     vector VEC_MERGE; same idea, different name.  The
		     parameters need to be reordered too:

		       (set (op0)
		            (if_then_else
			      (ne (scc) 0)
			      (op1) (op2)))

		     Becomes:

		       (set (op0)
			    (vec_merge
			      (op1) (op2)
			      (vcc)))
		  */
		    {
		      rtx condop = XEXP (x, 0);
		      rtx opcc = XEXP (condop, 0);
		      rtx optrue = XEXP (x, 1);
		      rtx opfalse = XEXP (x, 2);

		      /* The code below assumes the cselect pattern uses NE
			 in the machine description; check nobody changed
			 that.  */
		      gcc_checking_assert (GET_CODE (condop) == NE);

		      machine_mode vmode = vectorize_mode (GET_MODE (x));
		      rtx vec_merge = gen_rtx_VEC_MERGE (vmode, optrue,
							 opfalse, opcc);
		      **iter = vec_merge;
		      do_transform = false;

		      if (dump_file && (dump_flags & TDF_DETAILS))
			{
			  fprintf (dump_file, "      transform: ");
			  dump_value_with_mode (dump_file, x);
			  fprintf (dump_file, " -> ");
			}
		    }
		  break;

		default:
		  do_transform = true;
		}

	      /* If no special case requested otherwise, we do a standard
		 scalar -> vector mode conversion here.  */
	      if (do_transform
		  && VECTOR_MODE_P (GET_MODE (x)))
		{
		  /* The mode was already vectorized somewhere else.  */
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "      no-op: ");
		}
	      else if (do_transform)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "      transform: ");
		      dump_value_with_mode (dump_file, x);
		      fprintf (dump_file, " -> ");
		    }

		  if (REG_P (x))
		    /* Registers get substituted with a new pseudoreg.  */
		    **iter = transform_reg (x, converted_regs);
		  else if (GET_CODE (x) == PC)
		    /* The program counter is special.  */
		    pc_write = true;
		  else
		    /* Otherwise, just edit the RTL in place.  */
		    PUT_MODE_RAW (x, vectorize_mode (GET_MODE (x)));
		}

	      if (do_dump && dump_file && (dump_flags & TDF_DETAILS))
		{
		  dump_value_with_mode (dump_file, **iter);
		  fprintf (dump_file, "\n");
		}

	      if (delayed_substitute)
		iter.substitute (delayed_substitute);
	    }

	  /* Discard REG_NOTES.  They'll get regenerated later.
	     (Are there any exceptions?)  */
	  REG_NOTES (insn) = NULL;

	  /* Handle any "simt_special" attributes that add new CLOBBERs.  */
	  if (special_attr == SIMT_SPECIAL_ADD_DI_CLOBBER
	      || special_attr == SIMT_SPECIAL_ADD_VDI_CLOBBER
	      || special_attr == SIMT_SPECIAL_ADD_VSI_DI_CLOBBER)
	    {
	      /* The vector version of the instruction has additional
		 clobbers.  Add them now.  */
	      rtx parallel = PATTERN (insn);
	      if (GET_CODE (parallel) != PARALLEL)
		{
		  /* We need to create the surrounding PARALLEL too.  */
		  gcc_checking_assert (parallelvec.length () == 0);
		  parallelvec.safe_push (PATTERN (insn));
		}

	      /* Each attribute type may add multiple clobbers.  These tables
		 encode the modes they will have and the order they will
		 be added.  */
	      static const machine_mode vsidi[] = {V64SImode, DImode, VOIDmode};
	      static const machine_mode di[] = {DImode, VOIDmode};
	      static const machine_mode vdi[] = {V64DImode, VOIDmode};
	      const machine_mode *todo =
		(special_attr == SIMT_SPECIAL_ADD_VSI_DI_CLOBBER ? vsidi
		 : special_attr == SIMT_SPECIAL_ADD_DI_CLOBBER ? di
		 : special_attr == SIMT_SPECIAL_ADD_VDI_CLOBBER ? vdi
		 : NULL);

	      /* Add one CLOBBER for each entry in the todo table.  */
	      for (int i = 0; todo[i] != VOIDmode; i++)
		{
		  machine_mode mode = todo[i];
		  rtx newscratch = gen_rtx_SCRATCH (mode);
		  rtx newclobber = gen_rtx_CLOBBER (VOIDmode, newscratch);
		  parallelvec.safe_push (newclobber);

		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "      add new clobber: ");
		      dump_value_with_mode (dump_file, newclobber);
		      fprintf (dump_file, "\n");
		    }
		}
	      clobbers_changed = true;
	    }

	  /* If any special case changed the list of clobbers, replace the
	     contents of the top-level PARALLEL now. Use what we copied
	     above.  */
	  if (clobbers_changed)
	    {
	      rtx parallel = PATTERN (insn);
	      if (GET_CODE (parallel) != PARALLEL)
		{
		  /* There wasn't a parallel, so we need to add one.  */
		  parallel = gen_rtx_PARALLEL (VOIDmode, NULL);
		  PATTERN (insn) = parallel;

		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "      insert parallel\n");
		}

	      if (parallelvec.length () > 1)
		XVEC (parallel, 0) = gen_rtvec_v (parallelvec.length (),
						  parallelvec.address ());
	      else
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "      trim parallel\n");

		  PATTERN (insn) = parallelvec[0];
		}
	    }

	  /* The CONST_INT inside MEM will have been set to the mode of the
	     whole instruction (because the code above couldn't see the
	     context), but they actually want to be V64DImode.  */
	  for (auto xp : mem_reads)
	    {
	      rtx x = *xp;

	      rtx addr = XEXP (x, 0);
	      if (GET_CODE (addr) != PLUS
		  || !VECTOR_MODE_P (GET_MODE (addr)))
		continue;

	      rtx offset = XEXP (addr, 1);
	      if (GET_CODE (offset) != CONST_VECTOR)
		continue;

	      if (GET_MODE (offset) != V64DImode)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "      fixup mem offset mode\n");

		  PUT_MODE_RAW (offset, V64DImode);
		}
	    }
#if 0
/* This is a partial (possibly slightly bitrotten) implementation of an
   alternative to the "Vector MEM" proposal, already posted upstream.
   It inserts GATHER_LOAD and SCATTER_STORE patterns replacing the existing
   load instruction.  Once that proposal is accepted this code can be
   deleted.  */

	  for (auto xp : mem_writes)
	    {
	      x = *xp;

	      rtx addr = XEXP(x, 0);
	      rtx addrspace = gen_rtx_CONST_INT (VOIDmode, MEM_ADDR_SPACE (x));
	      rtx volatil = gen_rtx_CONST_INT (VOIDmode, MEM_VOLATILE_P (x));
	      machine_mode memmode = GET_MODE (x);
	      rtx tmpreg = gen_reg_rtx (memmode);

	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "      substitute: ");
		  dump_value_with_mode (dump_file, x);
		  fprintf (dump_file, " -> ");
		  dump_value_with_mode (dump_file, tmpreg);
		  fprintf (dump_file, "\n");
		}

	      rtx scatter = gen_scatter_expr (memmode, addr,
					      tmpreg, addrspace, volatil);
	      if (dump_file && optimize)
		fprintf (dump_file, "      df ");
	      rtx_insn *newinsn = emit_insn_after (scatter, insn);
	      *xp = tmpreg;

	      if (dump_file)
		{
		  fprintf (dump_file, "      insert scatter_store: ");
		  dump_insn_no_notes (dump_file, newinsn);
		}
	    }
	  for (auto xp : mem_reads)
	    {
	      x = *xp;

	      if (!MEM_P (x))
		continue; // This was already processed as a write.

	      rtx addr = XEXP(x, 0);
	      rtx addrspace = gen_rtx_CONST_INT (VOIDmode, MEM_ADDR_SPACE (x));
	      rtx volatil = gen_rtx_CONST_INT (VOIDmode, MEM_VOLATILE_P (x));
	      machine_mode memmode = GET_MODE (x);
	      rtx tmpreg = gen_reg_rtx (memmode);

	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "      substitute: ");
		  dump_value_with_mode (dump_file, x);
		  fprintf (dump_file, " -> ");
		  dump_value_with_mode (dump_file, tmpreg);
		  fprintf (dump_file, "\n");
		}

	      rtx gather = gen_gather_expr (memmode, tmpreg, addr,
					    addrspace, volatil);
	      if (dump_file && optimize)
		fprintf (dump_file, "      df ");
	      rtx_insn *newinsn = emit_insn_before (gather, insn);
	      *xp = tmpreg;

	      if (dump_file)
		{
		  fprintf (dump_file, "      insert gather_load: ");
		  dump_insn_no_notes (dump_file, newinsn);
		}
	    }
#endif

	  /* SUBREGs don't translate directly from scalar to vector because
	     a subreg of a vector refers to the register as a whole, not the
	     individual lanes.  We have UNSPEC_VSUBREG_READ and
	     UNSPEC_VSUBREG_WRITE to handle the problem (imperfectly), so
	     substitute those in now.  Writes and reads need different
	     transformations.  */
	  for (auto xp : subreg_writes)
	    {
	      rtx x = *xp;

	      rtx reg = SUBREG_REG (x);
	      machine_mode subregmode = GET_MODE (x);
	      rtx tmpreg = gen_reg_rtx (subregmode);

	      const int bytes_per_lane = 4;
	      int byte = SUBREG_BYTE (x).to_constant ();
	      int partno = byte / bytes_per_lane;
	      rtx part = gen_rtx_CONST_INT (VOIDmode, partno);

	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "      substitute: ");
		  dump_value_with_mode (dump_file, x);
		  fprintf (dump_file, " -> ");
		  dump_value_with_mode (dump_file, tmpreg);
		  fprintf (dump_file, "\n");
		}

	      if (!REG_P (reg))
		{
		  /* This is probably a mem, so emit an additional insn.  */
		  rtx tmp1 = gen_reg_rtx (GET_MODE (reg));
		  rtx newset = gen_rtx_SET (tmp1, reg);

		  if (dump_file && optimize)
		    fprintf (dump_file, "      df ");
		  rtx_insn *newinsn1 = emit_insn_after (newset, insn);
		  reg = tmp1;

		  if (dump_file)
		    {
		      fprintf (dump_file, "      break out subreg insn: ");
		      dump_insn_no_notes (dump_file, newinsn1);
		    }
		}
	      rtx vsubreg_write = gen_vsubreg_write (GET_MODE (reg),
						    subregmode,
						    reg, tmpreg, part);

	      if (dump_file && optimize)
		fprintf (dump_file, "      df ");
	      rtx_insn *newinsn = emit_insn_after (vsubreg_write, insn);
	      *xp = tmpreg;

	      if (dump_file)
		{
		  fprintf (dump_file, "      insert vsubreg_write: ");
		  dump_insn_no_notes (dump_file, newinsn);
		}
	    }
	  for (auto xp : subreg_reads)
	    {
	      rtx x = *xp;

	      /* The SUBREG_READS list also contains the writes, since there
		 was no way to tell them apart, above, but those were already
		 substituted so we can identify them now and skip them.  */
	      if (!SUBREG_P (x))
		continue;

	      rtx reg = SUBREG_REG (x);
	      machine_mode subregmode = GET_MODE (x);
	      rtx tmpreg = gen_reg_rtx (subregmode);

	      const int bytes_per_lane = 4;
	      int byte = SUBREG_BYTE (x).to_constant ();
	      int partno = byte / bytes_per_lane;
	      rtx part = gen_rtx_CONST_INT (VOIDmode, partno);

	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "      substitute: ");
		  dump_value_with_mode (dump_file, x);
		  fprintf (dump_file, " -> ");
		  dump_value_with_mode (dump_file, tmpreg);
		  fprintf (dump_file, "\n");
		}

	      if (!REG_P (reg))
		{
		  /* This is probably a mem, so emit an additional insn.  */
		  rtx tmp1 = gen_reg_rtx (GET_MODE (reg));
		  rtx newset = gen_rtx_SET (tmp1, reg);

		  if (dump_file && optimize)
		    fprintf (dump_file, "      df ");
		  rtx_insn *newinsn1 = emit_insn_before (newset, insn);
		  reg = tmp1;

		  if (dump_file)
		    {
		      fprintf (dump_file, "      break out subreg insn: ");
		      dump_insn_no_notes (dump_file, newinsn1);
		    }
		}
	      rtx vsubreg_read = gen_vsubreg_read (subregmode, GET_MODE (reg),
						  tmpreg, reg, part);

	      if (dump_file && optimize)
		fprintf (dump_file, "      df ");
	      rtx_insn *newinsn = emit_insn_before (vsubreg_read, insn);
	      *xp = tmpreg;

	      if (dump_file)
		{
		  fprintf (dump_file, "      insert vsubreg_read: ");
		  dump_insn_no_notes (dump_file, newinsn);
		}
	    }

	  /* Finally, check we produced a valid insn.  */
	  INSN_CODE (insn) = -1;
	  int new_insn_code = recog_memoized (insn);
	  if (dump_file)
	    fprintf (dump_file, "      new insn %d code: %s\n",
		     INSN_UID (insn),
		     (new_insn_code < 0
		      ? (is_asm ? "ASM" : "ERROR")
		      : get_insn_name (new_insn_code)));
	  if (!is_asm && new_insn_code == -1)
	    fatal_insn_not_found (insn);

	  if (dump_file && optimize)
	    fprintf (dump_file, "      df ");
	  df_insn_rescan (insn);
	}
    }

  /* After all the insns have been converted to SIMT, fix up the registers that
     are affected by the SIMT_ENTER/SIMT_EXIT transitions, if any.  */
  for (auto r : simt_regions)
    {
      /* Insert scalar->vector conversions at SIMT_ENTER.  */
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "\nHandling scalars live at SIMT_ENTER in bb %d....\n",
		 r->entry_block->index);

      reg_set_iterator iter;
      unsigned int regno;
      EXECUTE_IF_SET_IN_REG_SET (&r->live_at_simt_enter, 0, regno, iter)
	{
	  rtx oldreg = regno_reg_rtx[regno];
	  rtx newreg = converted_regs[regno];
	  machine_mode smode = GET_MODE (oldreg);
	  rtx x;

	  /* This can only happen when there are multiple SIMT regions in
	     one function.  */
	  if (VECTOR_MODE_P (smode))
	    continue;

	  if (newreg)
	    {
	      /* This register needs to be vec_duplicated.  */
	      machine_mode vmode = GET_MODE (newreg);

	      /* BImode is extended to DImode.  All other modes are duplicated
		 to all vector lanes.  */
	      if (smode == BImode)
		x = gen_cselect (vmode, newreg, oldreg,
				 gen_rtx_REG (DImode, EXEC_REG), const0_rtx);
	      else
		x = gen_rtx_SET (newreg,
				 gen_rtx_fmt_e (VEC_DUPLICATE, vmode, oldreg));
	    }
	  else
	    {
	      /* This register was never converted (it remains scalar), but
		 we need to ensure that it reads the same in all lanes.  */

	      x = gen_ensure_scalar (smode, oldreg, oldreg);
	    }

	  rtx_insn *new_insn = emit_insn_after (x, r->simt_enter_insn);

	  if (dump_file)
	    {
	      fprintf (dump_file, "Insert scalar -> vector conversion: ");
	      dump_insn_no_notes (dump_file, new_insn);
	    }
	}

      /* Insert vector->scalar conversions at SIMT_EXIT.

	 This reduction is inherently lossy, but it should not happen that
	 actual user variables need reducing.  It's more likely that compiler
	 temporaries (addresses, constants, etc.) are live at these
	 boundaries.  For now, we just take the first active lane and
	 discard the rest.  */
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "\nHandling vectors live at SIMT_EXIT in bb %d....\n",
		 r->exit_block->index);

      EXECUTE_IF_SET_IN_REG_SET (&r->live_at_simt_exit, 0, regno, iter)
	{
	  /* Registers that were never converted (they remained scalar) do
	     not need fixing here, even though they are live at the
	     transition point.  */
	  if (!converted_regs[regno])
	    continue;

	  rtx oldreg = converted_regs[regno];
	  rtx newreg = regno_reg_rtx[regno];
	  machine_mode smode = GET_MODE (newreg);
	  machine_mode vmode = GET_MODE (oldreg);
	  rtx x;

	  /* DImode is reduced to BImode by checking for any true value ...
	     this is probably imperfect.
	     Vectors are reduced to scalar by selecting the first active
	     lane ... this is also problematic.  */
	  if (smode == BImode)
	    x = gen_cstoredi4 (newreg, gen_rtx_NE (BImode, 0, 0), oldreg,
			       const0_rtx);
	  else
	    x = gen_extract_first (vmode, newreg, oldreg);

	  rtx_insn *new_insn = emit_insn_before (x, r->simt_exit_insn);

	  if (dump_file)
	    {
	      fprintf (dump_file, "Insert vector -> scalar conversion: ");
	      dump_insn_no_notes (dump_file, new_insn);
	    }
	}

      /* Free the region object.  */
      delete r;
    }

  return 0;
}
