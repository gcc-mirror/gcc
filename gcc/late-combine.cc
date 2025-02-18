// Late-stage instruction combination pass.
// Copyright (C) 2023-2025 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.
//
// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.
//
// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// The current purpose of this pass is to substitute definitions into
// all uses, so that the definition can be removed.  However, it could
// be extended to handle other combination-related optimizations in future.
//
// The pass can run before or after register allocation.  When running
// before register allocation, it tries to avoid cases that are likely
// to increase register pressure.  For the same reason, it avoids moving
// instructions around, even if doing so would allow an optimization to
// succeed.  These limitations are removed when running after register
// allocation.

#define INCLUDE_ALGORITHM
#define INCLUDE_FUNCTIONAL
#define INCLUDE_ARRAY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "df.h"
#include "rtl-ssa.h"
#include "print-rtl.h"
#include "tree-pass.h"
#include "cfgcleanup.h"
#include "target.h"
#include "dbgcnt.h"

using namespace rtl_ssa;

namespace {
const pass_data pass_data_late_combine =
{
  RTL_PASS, // type
  "late_combine", // name
  OPTGROUP_NONE, // optinfo_flags
  TV_LATE_COMBINE, // tv_id
  0, // properties_required
  0, // properties_provided
  0, // properties_destroyed
  0, // todo_flags_start
  TODO_df_finish, // todo_flags_finish
};

// Represents an attempt to substitute a single-set definition into all
// uses of the definition.
class insn_combination
{
public:
  insn_combination (set_info *, rtx, rtx);
  bool run ();
  array_slice<insn_change *const> use_changes () const;

private:
  use_array get_new_uses (use_info *);
  bool substitute_nondebug_use (use_info *);
  bool substitute_nondebug_uses (set_info *);
  bool try_to_preserve_debug_info (insn_change &, use_info *);
  void substitute_debug_use (use_info *);
  bool substitute_note (insn_info *, rtx, bool);
  void substitute_notes (insn_info *, bool);
  void substitute_note_uses (use_info *);
  void substitute_optional_uses (set_info *);

  // Represents the state of the function's RTL at the start of this
  // combination attempt.
  insn_change_watermark m_rtl_watermark;

  // Represents the rtl-ssa state at the start of this combination attempt.
  obstack_watermark m_attempt;

  // The instruction that contains the definition, and that we're trying
  // to delete.
  insn_info *m_def_insn;

  // The definition itself.
  set_info *m_def;

  // The destination and source of the single set that defines m_def.
  // The destination is known to be a plain REG.
  rtx m_dest;
  rtx m_src;

  // Contains the full list of changes that we want to make, in reverse
  // postorder.
  auto_vec<insn_change *> m_nondebug_changes;
};

// Class that represents one run of the pass.
class late_combine
{
public:
  unsigned int execute (function *);

private:
  rtx optimizable_set (insn_info *);
  bool check_register_pressure (insn_info *, rtx);
  bool check_uses (set_info *, rtx);
  bool combine_into_uses (insn_info *, insn_info *);

  auto_vec<insn_info *> m_worklist;
};

insn_combination::insn_combination (set_info *def, rtx dest, rtx src)
  : m_rtl_watermark (),
    m_attempt (crtl->ssa->new_change_attempt ()),
    m_def_insn (def->insn ()),
    m_def (def),
    m_dest (dest),
    m_src (src),
    m_nondebug_changes ()
{
}

array_slice<insn_change *const>
insn_combination::use_changes () const
{
  return { m_nondebug_changes.address () + 1,
	   m_nondebug_changes.length () - 1 };
}

// USE is a direct or indirect use of m_def.  Return the list of uses
// that would be needed after substituting m_def into the instruction.
// The returned list is marked as invalid if USE's insn and m_def_insn
// use different definitions for the same resource (register or memory).
use_array
insn_combination::get_new_uses (use_info *use)
{
  auto *def = use->def ();
  auto *use_insn = use->insn ();

  use_array new_uses = use_insn->uses ();
  new_uses = remove_uses_of_def (m_attempt, new_uses, def);
  new_uses = merge_access_arrays (m_attempt, m_def_insn->uses (), new_uses);
  if (new_uses.is_valid () && use->ebb () != m_def->ebb ())
    new_uses = crtl->ssa->make_uses_available (m_attempt, new_uses, use->bb (),
					       use_insn->is_debug_insn ());
  return new_uses;
}

// Start the process of trying to replace USE by substitution, given that
// USE occurs in a non-debug instruction.  Check:
//
// - that the substitution can be represented in RTL
//
// - that each use of a resource (register or memory) within the new
//   instruction has a consistent definition
//
// - that the new instruction is a recognized pattern
//
// - that the instruction can be placed somewhere that makes all definitions
//   and uses valid, and that permits any new hard-register clobbers added
//   during the recognition process
//
// Return true on success.
bool
insn_combination::substitute_nondebug_use (use_info *use)
{
  insn_info *use_insn = use->insn ();
  rtx_insn *use_rtl = use_insn->rtl ();

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_insn_slim (dump_file, use->insn ()->rtl ());

  // Reject second and subsequent uses if the target does not allow
  // the defining instruction to be copied.
  if (targetm.cannot_copy_insn_p
      && m_nondebug_changes.length () >= 2
      && targetm.cannot_copy_insn_p (m_def_insn->rtl ()))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "-- The target does not allow multiple"
		 " copies of insn %d\n", m_def_insn->uid ());
      return false;
    }

  // Check that we can change the instruction pattern.  Leave recognition
  // of the result till later.
  insn_propagation prop (use_rtl, m_dest, m_src);
  if (!prop.apply_to_pattern (&PATTERN (use_rtl))
      || prop.num_replacements == 0)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "-- RTL substitution failed\n");
      return false;
    }

  use_array new_uses = get_new_uses (use);
  if (!new_uses.is_valid ())
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "-- could not prove that all sources"
		 " are available\n");
      return false;
    }

  // Create a tentative change for the use.
  auto *where = XOBNEW (m_attempt, insn_change);
  auto *use_change = new (where) insn_change (use_insn);
  m_nondebug_changes.safe_push (use_change);
  use_change->new_uses = new_uses;

  struct local_ignore : ignore_nothing
  {
    local_ignore (const set_info *def, const insn_info *use_insn)
      : m_def (def), m_use_insn (use_insn) {}

    // We don't limit the number of insns per optimization, so ignoring all
    // insns for all insns would lead to quadratic complexity.  Just ignore
    // the use and definition, which should be enough for most purposes.
    bool
    should_ignore_insn (const insn_info *insn)
    {
      return insn == m_def->insn () || insn == m_use_insn;
    }

    // Ignore the definition that we're removing, and all uses of it.
    bool should_ignore_def (const def_info *def) { return def == m_def; }

    const set_info *m_def;
    const insn_info *m_use_insn;
  };

  auto ignore = local_ignore (m_def, use_insn);

  // Moving instructions before register allocation could increase
  // register pressure.  Only try moving them after RA.
  if (reload_completed && can_move_insn_p (use_insn))
    use_change->move_range = { use_insn->bb ()->head_insn (),
			       use_insn->ebb ()->last_bb ()->end_insn () };
  if (!restrict_movement (*use_change, ignore))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "-- cannot satisfy all definitions and uses"
		 " in insn %d\n", INSN_UID (use_insn->rtl ()));
      return false;
    }

  if (!recog (m_attempt, *use_change, ignore))
    return false;

  return true;
}

// Apply substitute_nondebug_use to all direct and indirect uses of DEF.
// There will be at most one level of indirection.
bool
insn_combination::substitute_nondebug_uses (set_info *def)
{
  for (use_info *use : def->nondebug_insn_uses ())
    if (!use->is_live_out_use ()
	&& !use->only_occurs_in_notes ()
	&& !substitute_nondebug_use (use))
      return false;

  for (use_info *use : def->phi_uses ())
    if (!substitute_nondebug_uses (use->phi ()))
      return false;

  return true;
}

// USE_CHANGE.insn () is a debug instruction that uses m_def.  Try to
// substitute the definition into the instruction and try to describe
// the result in USE_CHANGE.  Return true on success.  Failure means that
// the instruction must be reset instead.
bool
insn_combination::try_to_preserve_debug_info (insn_change &use_change,
					      use_info *use)
{
  // Punt on unsimplified subregs of hard registers.  In that case,
  // propagation can succeed and create a wider reg than the one we
  // started with.
  if (HARD_REGISTER_NUM_P (use->regno ())
      && use->includes_subregs ())
    return false;

  insn_info *use_insn = use_change.insn ();
  rtx_insn *use_rtl = use_insn->rtl ();

  use_change.new_uses = get_new_uses (use);
  if (!use_change.new_uses.is_valid ()
      || !restrict_movement (use_change))
    return false;

  insn_propagation prop (use_rtl, m_dest, m_src);
  return prop.apply_to_pattern (&INSN_VAR_LOCATION_LOC (use_rtl));
}

// USE_INSN is a debug instruction that uses m_def.  Update it to reflect
// the fact that m_def is going to disappear.  Try to preserve the source
// value if possible, but reset the instruction if not.
void
insn_combination::substitute_debug_use (use_info *use)
{
  auto *use_insn = use->insn ();
  rtx_insn *use_rtl = use_insn->rtl ();

  auto use_change = insn_change (use_insn);
  if (!try_to_preserve_debug_info (use_change, use))
    {
      use_change.new_uses = {};
      use_change.move_range = use_change.insn ();
      INSN_VAR_LOCATION_LOC (use_rtl) = gen_rtx_UNKNOWN_VAR_LOC ();
    }
  insn_change *changes[] = { &use_change };
  crtl->ssa->change_insns (changes);
}

// NOTE is a reg note of USE_INSN, which previously used m_def.  Update
// the note to reflect the fact that m_def is going to disappear.  Return
// true on success, or false if the note must be deleted.
//
// CAN_PROPAGATE is true if m_dest can be replaced with m_use.
bool
insn_combination::substitute_note (insn_info *use_insn, rtx note,
				   bool can_propagate)
{
  if (REG_NOTE_KIND (note) == REG_EQUAL
      || REG_NOTE_KIND (note) == REG_EQUIV)
    {
      insn_propagation prop (use_insn->rtl (), m_dest, m_src);
      return (prop.apply_to_note (&XEXP (note, 0))
	      && (can_propagate || prop.num_replacements == 0));
    }
  return true;
}

// Update USE_INSN's notes after deciding to go ahead with the optimization.
// CAN_PROPAGATE is true if m_dest can be replaced with m_use.
void
insn_combination::substitute_notes (insn_info *use_insn, bool can_propagate)
{
  rtx_insn *use_rtl = use_insn->rtl ();
  rtx *ptr = &REG_NOTES (use_rtl);
  while (rtx note = *ptr)
    {
      if (substitute_note (use_insn, note, can_propagate))
	ptr = &XEXP (note, 1);
      else
	*ptr = XEXP (note, 1);
    }
}

// We've decided to go ahead with the substitution.  Update all REG_NOTES
// involving USE.
void
insn_combination::substitute_note_uses (use_info *use)
{
  insn_info *use_insn = use->insn ();

  bool can_propagate = true;
  if (use->only_occurs_in_notes ())
    {
      // The only uses are in notes.  Try to keep the note if we can,
      // but removing it is better than aborting the optimization.
      insn_change use_change (use_insn);
      use_change.new_uses = get_new_uses (use);
      if (!use_change.new_uses.is_valid ()
	  || !restrict_movement (use_change))
	{
	  use_change.move_range = use_insn;
	  use_change.new_uses = remove_uses_of_def (m_attempt,
						    use_insn->uses (),
						    use->def ());
	  can_propagate = false;
	}
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "%s notes in:\n",
		   can_propagate ? "updating" : "removing");
	  dump_insn_slim (dump_file, use_insn->rtl ());
	}
      substitute_notes (use_insn, can_propagate);
      insn_change *changes[] = { &use_change };
      crtl->ssa->change_insns (changes);
    }
  else
    // We've already decided to update the insn's pattern and know that m_src
    // will be available at the insn's new location.  Now update its notes.
    substitute_notes (use_insn, can_propagate);
}

// We've decided to go ahead with the substitution and we've dealt with
// all uses that occur in the patterns of non-debug insns.  Update all
// other uses for the fact that m_def is about to disappear.
void
insn_combination::substitute_optional_uses (set_info *def)
{
  if (auto insn_uses = def->all_insn_uses ())
    {
      use_info *use = *insn_uses.begin ();
      while (use)
	{
	  use_info *next_use = use->next_any_insn_use ();
	  if (use->is_in_debug_insn ())
	    substitute_debug_use (use);
	  else if (!use->is_live_out_use ())
	    substitute_note_uses (use);
	  use = next_use;
	}
    }
  for (use_info *use : def->phi_uses ())
    substitute_optional_uses (use->phi ());
}

// Try to perform the substitution.  Return true on success.
bool
insn_combination::run ()
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\ntrying to combine definition of r%d in:\n",
	       m_def->regno ());
      dump_insn_slim (dump_file, m_def_insn->rtl ());
      fprintf (dump_file, "into:\n");
    }

  auto def_change = insn_change::delete_insn (m_def_insn);
  m_nondebug_changes.safe_push (&def_change);

  if (!substitute_nondebug_uses (m_def)
      || !changes_are_worthwhile (m_nondebug_changes)
      || !crtl->ssa->verify_insn_changes (m_nondebug_changes))
    return false;

  // We've now decided that the optimization is valid and profitable.
  // Allow it to be suppressed for bisection purposes.
  if (!dbg_cnt (::late_combine))
    return false;

  substitute_optional_uses (m_def);

  confirm_change_group ();
  crtl->ssa->change_insns (m_nondebug_changes);
  return true;
}

// See whether INSN is a single_set that we can optimize.  Return the
// set if so, otherwise return null.
rtx
late_combine::optimizable_set (insn_info *insn)
{
  if (!insn->can_be_optimized ()
      || insn->is_asm ()
      || insn->is_call ()
      || insn->has_volatile_refs ()
      || insn->has_pre_post_modify ()
      || !can_move_insn_p (insn))
    return NULL_RTX;

  return single_set (insn->rtl ());
}

// Suppose that we can replace all uses of SET_DEST (SET) with SET_SRC (SET),
// where SET occurs in INSN.  Return true if doing so is not likely to
// increase register pressure.
bool
late_combine::check_register_pressure (insn_info *insn, rtx set)
{
  // Plain register-to-register moves do not establish a register class
  // preference and have no well-defined effect on the register allocator.
  // If changes in register class are needed, the register allocator is
  // in the best position to place those changes.  If no change in
  // register class is needed, then the optimization reduces register
  // pressure if SET_SRC (set) was already live at uses, otherwise the
  // optimization is pressure-neutral.
  rtx src = SET_SRC (set);
  if (REG_P (src))
    return true;

  // On the same basis, substituting a SET_SRC that contains a single
  // pseudo register either reduces pressure or is pressure-neutral,
  // subject to the constraints below.  We would need to do more
  // analysis for SET_SRCs that use more than one pseudo register.
  unsigned int nregs = 0;
  for (auto *use : insn->uses ())
    if (use->is_reg ()
	&& !HARD_REGISTER_NUM_P (use->regno ())
	&& !use->only_occurs_in_notes ())
      if (++nregs > 1)
	return false;

  // If there are no pseudo registers in SET_SRC then the optimization
  // should improve register pressure.
  if (nregs == 0)
    return true;

  // We'd be substituting (set (reg R1) SRC) where SRC is known to
  // contain a single pseudo register R2.  Assume for simplicity that
  // each new use of R2 would need to be in the same class C as the
  // current use of R2.  If, for a realistic allocation, C is a
  // non-strict superset of the R1's register class, the effect on
  // register pressure should be positive or neutral.  If instead
  // R1 occupies a different register class from R2, or if R1 has
  // more allocation freedom than R2, then there's a higher risk that
  // the effect on register pressure could be negative.
  //
  // First use constrain_operands to get the most likely choice of
  // alternative.  For simplicity, just handle the case where the
  // output operand is operand 0.
  extract_insn (insn->rtl ());
  rtx dest = SET_DEST (set);
  if (recog_data.n_operands == 0
      || recog_data.operand[0] != dest)
    return false;

  if (!constrain_operands (0, get_enabled_alternatives (insn->rtl ())))
    return false;

  preprocess_constraints (insn->rtl ());
  auto *alt = which_op_alt ();
  auto dest_class = alt[0].cl;

  // Check operands 1 and above.
  auto check_src = [&] (unsigned int i)
    {
      if (recog_data.is_operator[i])
	return true;

      rtx op = recog_data.operand[i];
      if (CONSTANT_P (op))
	return true;

      if (SUBREG_P (op))
	op = SUBREG_REG (op);
      if (REG_P (op))
	{
	  // Ignore hard registers.  We've already rejected uses of non-fixed
	  // hard registers in the SET_SRC.
	  if (HARD_REGISTER_P (op))
	    return true;

	  // Make sure that the source operand's class is at least as
	  // permissive as the destination operand's class.
	  auto src_class = alternative_class (alt, i);
	  if (dest_class != src_class)
	    {
	      auto extra_dest_regs = (reg_class_contents[dest_class]
				      & ~reg_class_contents[src_class]
				      & ~fixed_reg_set);
	      if (!hard_reg_set_empty_p (extra_dest_regs))
		return false;
	    }

	  // Make sure that the source operand occupies no more hard
	  // registers than the destination operand.  This mostly matters
	  // for subregs.
	  if (targetm.class_max_nregs (dest_class, GET_MODE (dest))
	      < targetm.class_max_nregs (src_class, GET_MODE (op)))
	    return false;

	  return true;
	}
      return false;
    };
  for (int i = 1; i < recog_data.n_operands; ++i)
    if (recog_data.operand_type[i] != OP_OUT && !check_src (i))
      return false;

  return true;
}

// Check uses of DEF to see whether there is anything obvious that
// prevents the substitution of SET into uses of DEF.
bool
late_combine::check_uses (set_info *def, rtx set)
{
  use_info *prev_use = nullptr;
  for (use_info *use : def->nondebug_insn_uses ())
    {
      insn_info *use_insn = use->insn ();

      if (use->is_live_out_use ())
	continue;
      if (use->only_occurs_in_notes ())
	continue;

      // We cannot replace all uses if the value is live on exit.
      if (use->is_artificial ())
	return false;

      // Avoid increasing the complexity of instructions that
      // reference allocatable hard registers.
      if (!REG_P (SET_SRC (set))
	  && !reload_completed
	  && (accesses_include_nonfixed_hard_registers (use_insn->uses ())
	      || accesses_include_nonfixed_hard_registers (use_insn->defs ())))
	return false;

      // Don't substitute into a non-local goto, since it can then be
      // treated as a jump to local label, e.g. in shorten_branches.
      // ??? But this shouldn't be necessary.
      if (use_insn->is_jump ()
	  && find_reg_note (use_insn->rtl (), REG_NON_LOCAL_GOTO, NULL_RTX))
	return false;

      // Reject cases where one of the uses is a function argument.
      // The combine attempt should fail anyway, but this is a common
      // case that is easy to check early.
      if (use_insn->is_call ()
	  && HARD_REGISTER_P (SET_DEST (set))
	  && find_reg_fusage (use_insn->rtl (), USE, SET_DEST (set)))
	return false;

      // We'll keep the uses in their original order, even if we move
      // them relative to other instructions.  Make sure that non-final
      // uses do not change any values that occur in the SET_SRC.
      if (prev_use && prev_use->ebb () == use->ebb ())
	{
	  def_info *ultimate_def = look_through_degenerate_phi (def);
	  if (insn_clobbers_resources (prev_use->insn (),
				       ultimate_def->insn ()->uses ()))
	    return false;
	}

      prev_use = use;
    }

  for (use_info *use : def->phi_uses ())
    if (!use->phi ()->is_degenerate ()
	|| !check_uses (use->phi (), set))
      return false;

  return true;
}

// Try to remove INSN by substituting a definition into all uses.
// If the optimization moves any instructions before CURSOR, add those
// instructions to the end of m_worklist.
bool
late_combine::combine_into_uses (insn_info *insn, insn_info *cursor)
{
  // For simplicity, don't try to handle sets of multiple hard registers.
  // And for correctness, don't remove any assignments to the stack or
  // frame pointers, since that would implicitly change the set of valid
  // memory locations between this assignment and the next.
  //
  // Removing assignments to the hard frame pointer would invalidate
  // backtraces.
  set_info *def = single_set_info (insn);
  if (!def
      || !def->is_reg ()
      || def->regno () == STACK_POINTER_REGNUM
      || def->regno () == FRAME_POINTER_REGNUM
      || def->regno () == HARD_FRAME_POINTER_REGNUM)
    return false;

  rtx set = optimizable_set (insn);
  if (!set)
    return false;

  // For simplicity, don't try to handle subreg destinations.
  rtx dest = SET_DEST (set);
  if (!REG_P (dest) || def->regno () != REGNO (dest))
    return false;

  // Don't prolong the live ranges of allocatable hard registers, or put
  // them into more complicated instructions.  Failing to prevent this
  // could lead to spill failures, or at least to worst register allocation.
  if (!reload_completed
      && accesses_include_nonfixed_hard_registers (insn->uses ()))
    return false;

  if (!reload_completed && !check_register_pressure (insn, set))
    return false;

  if (!check_uses (def, set))
    return false;

  insn_combination combination (def, SET_DEST (set), SET_SRC (set));
  if (!combination.run ())
    return false;

  for (auto *use_change : combination.use_changes ())
    if (*use_change->insn () < *cursor)
      m_worklist.safe_push (use_change->insn ());
    else
      break;
  return true;
}

// Run the pass on function FN.
unsigned int
late_combine::execute (function *fn)
{
  // Initialization.
  calculate_dominance_info (CDI_DOMINATORS);
  df_analyze ();
  crtl->ssa = new rtl_ssa::function_info (fn);
  // Don't allow memory_operand to match volatile MEMs.
  init_recog_no_volatile ();

  insn_info *insn = *crtl->ssa->nondebug_insns ().begin ();
  while (insn)
    {
      if (!insn->is_artificial ())
	{
	  insn_info *prev = insn->prev_nondebug_insn ();
	  if (combine_into_uses (insn, prev))
	    {
	      // Any instructions that get added to the worklist were
	      // previously after PREV.  Thus if we were able to move
	      // an instruction X before PREV during one combination,
	      // X cannot depend on any instructions that we move before
	      // PREV during subsequent combinations.  This means that
	      // the worklist should be free of backwards dependencies,
	      // even if it isn't necessarily in RPO.
	      for (unsigned int i = 0; i < m_worklist.length (); ++i)
		combine_into_uses (m_worklist[i], prev);
	      m_worklist.truncate (0);
	      insn = prev;
	    }
	}
      insn = insn->next_nondebug_insn ();
    }

  // Finalization.
  if (crtl->ssa->perform_pending_updates ())
    cleanup_cfg (0);

  delete crtl->ssa;
  crtl->ssa = nullptr;

  // Make the recognizer allow volatile MEMs again.
  init_recog ();
  free_dominance_info (CDI_DOMINATORS);
  return 0;
}

class pass_late_combine : public rtl_opt_pass
{
public:
  pass_late_combine (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_late_combine, ctxt)
  {}

  // opt_pass methods:
  opt_pass *clone () override { return new pass_late_combine (m_ctxt); }
  bool gate (function *) override;
  unsigned int execute (function *) override;
};

bool
pass_late_combine::gate (function *)
{
  return optimize > 0 && flag_late_combine_instructions;
}

unsigned int
pass_late_combine::execute (function *fn)
{
  return late_combine ().execute (fn);
}

} // end namespace

// Create a new CC fusion pass instance.

rtl_opt_pass *
make_pass_late_combine (gcc::context *ctxt)
{
  return new pass_late_combine (ctxt);
}
