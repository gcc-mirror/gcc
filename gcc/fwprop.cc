/* RTL-based forward propagation pass for GNU compiler.
   Copyright (C) 2005-2024 Free Software Foundation, Inc.
   Contributed by Paolo Bonzini and Steven Bosscher.

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

#define INCLUDE_ALGORITHM
#define INCLUDE_FUNCTIONAL
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "rtlanal.h"
#include "df.h"
#include "rtl-ssa.h"

#include "predict.h"
#include "cfgrtl.h"
#include "cfgcleanup.h"
#include "cfgloop.h"
#include "tree-pass.h"
#include "rtl-iter.h"
#include "target.h"

/* This pass does simple forward propagation and simplification when an
   operand of an insn can only come from a single def.  This pass uses
   RTL SSA, so it is global.  However, we only do limited analysis of
   available expressions.

   1) The pass tries to propagate the source of the def into the use,
   and checks if the result is independent of the substituted value.
   For example, the high word of a (zero_extend:DI (reg:SI M)) is always
   zero, independent of the source register.

   In particular, we propagate constants into the use site.  Sometimes
   RTL expansion did not put the constant in the same insn on purpose,
   to satisfy a predicate, and the result will fail to be recognized;
   but this happens rarely and in this case we can still create a
   REG_EQUAL note.  For multi-word operations, this

      (set (subreg:SI (reg:DI 120) 0) (const_int 0))
      (set (subreg:SI (reg:DI 120) 4) (const_int -1))
      (set (subreg:SI (reg:DI 122) 0)
	 (ior:SI (subreg:SI (reg:DI 119) 0) (subreg:SI (reg:DI 120) 0)))
      (set (subreg:SI (reg:DI 122) 4)
	 (ior:SI (subreg:SI (reg:DI 119) 4) (subreg:SI (reg:DI 120) 4)))

   can be simplified to the much simpler

      (set (subreg:SI (reg:DI 122) 0) (subreg:SI (reg:DI 119)))
      (set (subreg:SI (reg:DI 122) 4) (const_int -1))

   This particular propagation is also effective at putting together
   complex addressing modes.  We are more aggressive inside MEMs, in
   that all definitions are propagated if the use is in a MEM; if the
   result is a valid memory address we check address_cost to decide
   whether the substitution is worthwhile.

   2) The pass propagates register copies.  This is not as effective as
   the copy propagation done by CSE's canon_reg, which works by walking
   the instruction chain, it can help the other transformations.

   We should consider removing this optimization, and instead reorder the
   RTL passes, because GCSE does this transformation too.  With some luck,
   the CSE pass at the end of rest_of_handle_gcse could also go away.

   3) The pass looks for paradoxical subregs that are actually unnecessary.
   Things like this:

     (set (reg:QI 120) (subreg:QI (reg:SI 118) 0))
     (set (reg:QI 121) (subreg:QI (reg:SI 119) 0))
     (set (reg:SI 122) (plus:SI (subreg:SI (reg:QI 120) 0)
				(subreg:SI (reg:QI 121) 0)))

   are very common on machines that can only do word-sized operations.
   For each use of a paradoxical subreg (subreg:WIDER (reg:NARROW N) 0),
   if it has a single def and it is (subreg:NARROW (reg:WIDE M) 0),
   we can replace the paradoxical subreg with simply (reg:WIDE M).  The
   above will simplify this to

     (set (reg:QI 120) (subreg:QI (reg:SI 118) 0))
     (set (reg:QI 121) (subreg:QI (reg:SI 119) 0))
     (set (reg:SI 122) (plus:SI (reg:SI 118) (reg:SI 119)))

   where the first two insns are now dead.  */

using namespace rtl_ssa;

static int num_changes;

/* Do not try to replace constant addresses or addresses of local and
   argument slots.  These MEM expressions are made only once and inserted
   in many instructions, as well as being used to control symbol table
   output.  It is not safe to clobber them.

   There are some uncommon cases where the address is already in a register
   for some reason, but we cannot take advantage of that because we have
   no easy way to unshare the MEM.  In addition, looking up all stack
   addresses is costly.  */

static bool
can_simplify_addr (rtx addr)
{
  rtx reg;

  if (CONSTANT_ADDRESS_P (addr))
    return false;

  if (GET_CODE (addr) == PLUS)
    reg = XEXP (addr, 0);
  else
    reg = addr;

  return (!REG_P (reg)
	  || (REGNO (reg) != FRAME_POINTER_REGNUM
	      && REGNO (reg) != HARD_FRAME_POINTER_REGNUM
	      && REGNO (reg) != ARG_POINTER_REGNUM));
}

/* MEM is the result of an address simplification, and temporarily
   undoing changes OLD_NUM_CHANGES onwards restores the original address.
   Return whether it is good to use the new address instead of the
   old one.  INSN is the containing instruction.  */

static bool
should_replace_address (int old_num_changes, rtx mem, rtx_insn *insn)
{
  int gain;

  /* Prefer the new address if it is less expensive.  */
  bool speed = optimize_bb_for_speed_p (BLOCK_FOR_INSN (insn));
  temporarily_undo_changes (old_num_changes);
  gain = address_cost (XEXP (mem, 0), GET_MODE (mem),
		       MEM_ADDR_SPACE (mem), speed);
  redo_changes (old_num_changes);
  gain -= address_cost (XEXP (mem, 0), GET_MODE (mem),
			MEM_ADDR_SPACE (mem), speed);

  /* If the addresses have equivalent cost, prefer the new address
     if it has the highest `set_src_cost'.  That has the potential of
     eliminating the most insns without additional costs, and it
     is the same that cse.cc used to do.  */
  if (gain == 0)
    {
      gain = set_src_cost (XEXP (mem, 0), VOIDmode, speed);
      temporarily_undo_changes (old_num_changes);
      gain -= set_src_cost (XEXP (mem, 0), VOIDmode, speed);
      redo_changes (old_num_changes);
    }

  return (gain > 0);
}


namespace
{
  class fwprop_propagation : public insn_propagation
  {
  public:
    static const uint16_t CHANGED_MEM = FIRST_SPARE_RESULT;
    static const uint16_t CONSTANT = FIRST_SPARE_RESULT << 1;
    static const uint16_t PROFITABLE = FIRST_SPARE_RESULT << 2;

    fwprop_propagation (insn_info *, set_info *, rtx, rtx);

    bool changed_mem_p () const { return result_flags & CHANGED_MEM; }
    bool folded_to_constants_p () const;
    bool likely_profitable_p () const;

    bool check_mem (int, rtx) final override;
    void note_simplification (int, uint16_t, rtx, rtx) final override;
    uint16_t classify_result (rtx, rtx);

  private:
    const bool single_use_p;
    const bool single_ebb_p;
  };
}

/* Prepare to replace FROM with TO in USE_INSN.  */

fwprop_propagation::fwprop_propagation (insn_info *use_insn,
					set_info *def, rtx from, rtx to)
  : insn_propagation (use_insn->rtl (), from, to),
    single_use_p (def->single_nondebug_use ()),
    single_ebb_p (use_insn->ebb () == def->ebb ())
{
  should_check_mems = true;
  should_note_simplifications = true;
}

/* MEM is the result of an address simplification, and temporarily
   undoing changes OLD_NUM_CHANGES onwards restores the original address.
   Return true if the propagation should continue, false if it has failed.  */

bool
fwprop_propagation::check_mem (int old_num_changes, rtx mem)
{
  if (!memory_address_addr_space_p (GET_MODE (mem), XEXP (mem, 0),
				    MEM_ADDR_SPACE (mem)))
    {
      failure_reason = "would create an invalid MEM";
      return false;
    }

  temporarily_undo_changes (old_num_changes);
  bool can_simplify = can_simplify_addr (XEXP (mem, 0));
  redo_changes (old_num_changes);
  if (!can_simplify)
    {
      failure_reason = "would replace a frame address";
      return false;
    }

  /* Copy propagations are always ok.  Otherwise check the costs.  */
  if (!(REG_P (from) && REG_P (to))
      && !should_replace_address (old_num_changes, mem, insn))
    {
      failure_reason = "would increase the cost of a MEM";
      return false;
    }

  result_flags |= CHANGED_MEM;
  return true;
}

/* OLDX has been simplified to NEWX.  Describe the change in terms of
   result_flags.  */

uint16_t
fwprop_propagation::classify_result (rtx old_rtx, rtx new_rtx)
{
  if (CONSTANT_P (new_rtx))
    {
      /* If OLD_RTX is a LO_SUM, then it presumably exists for a reason,
	 and NEW_RTX is likely not a legitimate address.  We want it to
	 disappear if it is invalid.

	 ??? Using the mode of the LO_SUM as the mode of the address
	 seems odd, but it was what the pre-SSA code did.  */
      if (GET_CODE (old_rtx) == LO_SUM
	  && !memory_address_p (GET_MODE (old_rtx), new_rtx))
	return CONSTANT;
      return CONSTANT | PROFITABLE;
    }

  /* Allow replacements that simplify operations on a vector or complex
     value to a component.  The most prominent case is
     (subreg ([vec_]concat ...)).   */
  if (REG_P (new_rtx)
      && !HARD_REGISTER_P (new_rtx)
      && (VECTOR_MODE_P (GET_MODE (from))
	  || COMPLEX_MODE_P (GET_MODE (from)))
      && GET_MODE (new_rtx) == GET_MODE_INNER (GET_MODE (from)))
    return PROFITABLE;

  /* Allow (subreg (mem)) -> (mem) simplifications with the following
     exceptions:
     1) Propagating (mem)s into multiple uses is not profitable.
     2) Propagating (mem)s across EBBs may not be profitable if the source EBB
	runs less frequently.
     3) Propagating (mem)s into paradoxical (subreg)s is not profitable.
     4) Creating new (mem/v)s is not correct, since DCE will not remove the old
	ones.  */
  if (single_use_p
      && single_ebb_p
      && SUBREG_P (old_rtx)
      && !paradoxical_subreg_p (old_rtx)
      && MEM_P (new_rtx)
      && !MEM_VOLATILE_P (new_rtx))
    return PROFITABLE;

  return 0;
}

/* Record that OLD_RTX has been simplified to NEW_RTX.  OLD_NUM_CHANGES
   is the number of unrelated changes that had been made before processing
   OLD_RTX and its subrtxes.  OLD_RESULT_FLAGS is the value that result_flags
   had at that point.  */

void
fwprop_propagation::note_simplification (int old_num_changes,
					 uint16_t old_result_flags,
					 rtx old_rtx, rtx new_rtx)
{
  result_flags &= ~(CONSTANT | PROFITABLE);
  uint16_t new_flags = classify_result (old_rtx, new_rtx);
  if (old_num_changes)
    new_flags &= old_result_flags;
  result_flags |= new_flags;
}

/* Return true if all substitutions eventually folded to constants.  */

bool
fwprop_propagation::folded_to_constants_p () const
{
  /* If we're propagating a HIGH, require it to be folded with a
     partnering LO_SUM.  For example, a REG_EQUAL note with a register
     replaced by an unfolded HIGH is not useful.  */
  if (CONSTANT_P (to) && GET_CODE (to) != HIGH)
    return true;
  return !(result_flags & UNSIMPLIFIED) && (result_flags & CONSTANT);
}


/* Return true if it is worth keeping the result of the propagation,
   false if it would increase the complexity of the pattern too much.  */

bool
fwprop_propagation::likely_profitable_p () const
{
  if (changed_mem_p ())
    return true;

  if (!(result_flags & UNSIMPLIFIED)
      && (result_flags & PROFITABLE))
    return true;

  if (REG_P (to))
    return true;

  if (GET_CODE (to) == SUBREG
      && REG_P (SUBREG_REG (to))
      && !paradoxical_subreg_p (to))
    return true;

  if (CONSTANT_P (to))
    return true;

  return false;
}

/* Check that X has a single def.  */

static bool
reg_single_def_p (rtx x)
{
  return REG_P (x) && crtl->ssa->single_dominating_def (REGNO (x));
}

/* Try to substitute (set DEST SRC), which defines DEF, into note NOTE of
   USE_INSN.  Return the number of substitutions on success, otherwise return
   -1 and leave USE_INSN unchanged.

   If REQUIRE_CONSTANT is true, require all substituted occurrences of SRC
   to fold to a constant, so that the note does not use any more registers
   than it did previously.  If REQUIRE_CONSTANT is false, also allow the
   substitution if it's something we'd normally allow for the main
   instruction pattern.  */

static int
try_fwprop_subst_note (insn_info *use_insn, set_info *def,
		       rtx note, rtx dest, rtx src, bool require_constant)
{
  rtx_insn *use_rtl = use_insn->rtl ();
  insn_info *def_insn = def->insn ();

  insn_change_watermark watermark;
  fwprop_propagation prop (use_insn, def, dest, src);
  if (!prop.apply_to_rvalue (&XEXP (note, 0)))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "cannot propagate from insn %d into"
		 " notes of insn %d: %s\n", def_insn->uid (),
		 use_insn->uid (), prop.failure_reason);
      return -1;
    }

  if (prop.num_replacements == 0)
    return 0;

  if (require_constant)
    {
      if (!prop.folded_to_constants_p ())
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "cannot propagate from insn %d into"
		     " notes of insn %d: %s\n", def_insn->uid (),
		     use_insn->uid (), "wouldn't fold to constants");
	  return -1;
	}
    }
  else
    {
      if (!prop.folded_to_constants_p () && !prop.likely_profitable_p ())
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "cannot propagate from insn %d into"
		     " notes of insn %d: %s\n", def_insn->uid (),
		     use_insn->uid (), "would increase complexity of node");
	  return -1;
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nin notes of insn %d, replacing:\n  ",
	       INSN_UID (use_rtl));
      temporarily_undo_changes (0);
      print_inline_rtx (dump_file, note, 2);
      redo_changes (0);
      fprintf (dump_file, "\n with:\n  ");
      print_inline_rtx (dump_file, note, 2);
      fprintf (dump_file, "\n");
    }
  watermark.keep ();
  return prop.num_replacements;
}

/* Try to substitute (set DEST SRC), which defines DEF, into location LOC of
   USE_INSN's pattern.  Return true on success, otherwise leave USE_INSN
   unchanged.  */

static bool
try_fwprop_subst_pattern (obstack_watermark &attempt, insn_change &use_change,
			  set_info *def, rtx *loc, rtx dest, rtx src)
{
  insn_info *use_insn = use_change.insn ();
  rtx_insn *use_rtl = use_insn->rtl ();
  insn_info *def_insn = def->insn ();

  insn_change_watermark watermark;
  fwprop_propagation prop (use_insn, def, dest, src);
  if (!prop.apply_to_pattern (loc))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "cannot propagate from insn %d into"
		 " insn %d: %s\n", def_insn->uid (), use_insn->uid (),
		 prop.failure_reason);
      return false;
    }

  if (prop.num_replacements == 0)
    return false;

  if (!prop.likely_profitable_p ()
      && (prop.changed_mem_p ()
	  || contains_mem_rtx_p (src)
	  || use_insn->is_asm ()
	  || !single_set (use_rtl)))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "cannot propagate from insn %d into"
		 " insn %d: %s\n", def_insn->uid (), use_insn->uid (),
		 "would increase complexity of pattern");
      return false;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\npropagating insn %d into insn %d, replacing:\n",
	       def_insn->uid (), use_insn->uid ());
      temporarily_undo_changes (0);
      print_rtl_single (dump_file, PATTERN (use_rtl));
      redo_changes (0);
    }

  /* ??? In theory, it should be better to use insn costs rather than
     set_src_costs here.  That would involve replacing this code with
     change_is_worthwhile.  */
  bool ok = recog (attempt, use_change);
  if (ok && !prop.changed_mem_p () && !use_insn->is_asm ())
    if (rtx use_set = single_set (use_rtl))
      {
	bool speed = optimize_bb_for_speed_p (BLOCK_FOR_INSN (use_rtl));
	temporarily_undo_changes (0);
	auto old_cost = set_src_cost (SET_SRC (use_set),
				      GET_MODE (SET_DEST (use_set)), speed);
	redo_changes (0);
	auto new_cost = set_src_cost (SET_SRC (use_set),
				      GET_MODE (SET_DEST (use_set)), speed);
	if (new_cost > old_cost
	    || (new_cost == old_cost && !prop.likely_profitable_p ()))
	  {
	    if (dump_file)
	      fprintf (dump_file, "change not profitable"
		       " (cost %d -> cost %d)\n", old_cost, new_cost);
	    ok = false;
	  }
      }

  if (!ok)
    {
      /* The pattern didn't match, but if all uses of SRC folded to
	 constants, we can add a REG_EQUAL note for the result, if there
	 isn't one already.  */
      if (!prop.folded_to_constants_p ())
	return false;

      /* Test this first to avoid creating an unnecessary copy of SRC.  */
      if (find_reg_note (use_rtl, REG_EQUAL, NULL_RTX))
	return false;

      rtx set = set_for_reg_notes (use_rtl);
      if (!set || !REG_P (SET_DEST (set)))
	return false;

      rtx value = copy_rtx (SET_SRC (set));
      cancel_changes (0);

      /* If there are any paradoxical SUBREGs, drop the REG_EQUAL note,
	 because the bits in there can be anything and so might not
	 match the REG_EQUAL note content.  See PR70574.  */
      if (contains_paradoxical_subreg_p (SET_SRC (set)))
	return false;

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, " Setting REG_EQUAL note\n");

      return set_unique_reg_note (use_rtl, REG_EQUAL, value);
    }

  rtx *note_ptr = &REG_NOTES (use_rtl);
  while (rtx note = *note_ptr)
    {
      if ((REG_NOTE_KIND (note) == REG_EQUAL
	   || REG_NOTE_KIND (note) == REG_EQUIV)
	  && try_fwprop_subst_note (use_insn, def, note, dest, src, false) < 0)
	{
	  *note_ptr = XEXP (note, 1);
	  free_EXPR_LIST_node (note);
	}
      else
	note_ptr = &XEXP (note, 1);
    }

  confirm_change_group ();
  crtl->ssa->change_insn (use_change);
  num_changes++;
  return true;
}

/* Try to substitute (set DEST SRC), which defines DEF, into USE_INSN's notes,
   given that it was not possible to do this for USE_INSN's main pattern.
   Return true on success, otherwise leave USE_INSN unchanged.  */

static bool
try_fwprop_subst_notes (insn_info *use_insn, set_info *def,
			rtx dest, rtx src)
{
  rtx_insn *use_rtl = use_insn->rtl ();
  for (rtx note = REG_NOTES (use_rtl); note; note = XEXP (note, 1))
    if ((REG_NOTE_KIND (note) == REG_EQUAL
	 || REG_NOTE_KIND (note) == REG_EQUIV)
	&& try_fwprop_subst_note (use_insn, def, note, dest, src, true) > 0)
      {
	confirm_change_group ();
	return true;
      }

  return false;
}

/* Check whether we could validly substitute (set DEST SRC), which defines DEF,
   into USE.  If so, first try performing the substitution in location LOC
   of USE->insn ()'s pattern.  If that fails, try instead to substitute
   into the notes.

   Return true on success, otherwise leave USE_INSN unchanged.  */

static bool
try_fwprop_subst (use_info *use, set_info *def,
		  rtx *loc, rtx dest, rtx src)
{
  insn_info *use_insn = use->insn ();
  insn_info *def_insn = def->insn ();

  auto attempt = crtl->ssa->new_change_attempt ();
  use_array src_uses = remove_note_accesses (attempt, def_insn->uses ());

  /* ??? Not really a meaningful test: it means we can propagate arithmetic
     involving hard registers but not bare references to them.  A better
     test would be to iterate over src_uses looking for hard registers
     that are not fixed.  */
  if (REG_P (src) && HARD_REGISTER_P (src))
    return false;

  /* ??? It would be better to make this EBB-based instead.  That would
     involve checking for equal EBBs rather than equal BBs and trying
     to make the uses available at use_insn->ebb ()->first_bb ().  */
  if (def_insn->bb () != use_insn->bb ())
    {
      src_uses = crtl->ssa->make_uses_available (attempt, src_uses,
						 use_insn->bb (),
						 use_insn->is_debug_insn ());
      if (!src_uses.is_valid ())
	return false;
    }

  insn_change use_change (use_insn);
  use_change.new_uses = merge_access_arrays (attempt, use_change.new_uses,
					     src_uses);
  if (!use_change.new_uses.is_valid ())
    return false;

  /* ??? We could allow movement within the EBB by adding:

     use_change.move_range = use_insn->ebb ()->insn_range ();  */
  if (!restrict_movement (use_change))
    return false;

  return (try_fwprop_subst_pattern (attempt, use_change, def, loc, dest, src)
	  || try_fwprop_subst_notes (use_insn, def, dest, src));
}

/* For the given single_set INSN, containing SRC known to be a
   ZERO_EXTEND or SIGN_EXTEND of a register, return true if INSN
   is redundant due to the register being set by a LOAD_EXTEND_OP
   load from memory.  */

static bool
free_load_extend (rtx src, insn_info *insn)
{
  rtx reg = XEXP (src, 0);
  if (load_extend_op (GET_MODE (reg)) != GET_CODE (src))
    return false;

  def_info *def = nullptr;
  for (use_info *use : insn->uses ())
    if (use->regno () == REGNO (reg))
      {
	def = use->def ();
	break;
      }

  if (!def)
    return false;

  insn_info *def_insn = def->insn ();
  if (def_insn->is_artificial ())
    return false;

  rtx_insn *def_rtl = def_insn->rtl ();
  if (NONJUMP_INSN_P (def_rtl))
    {
      rtx patt = PATTERN (def_rtl);

      if (GET_CODE (patt) == SET
	  && GET_CODE (SET_SRC (patt)) == MEM
	  && rtx_equal_p (SET_DEST (patt), reg))
	return true;
    }
  return false;
}

/* Subroutine of forward_propagate_subreg that handles a use of DEST
   in REF.  The other parameters are the same.  */

static bool
forward_propagate_subreg (use_info *use, set_info *def,
			  rtx dest, rtx src, df_ref ref)
{
  scalar_int_mode int_use_mode, src_mode;

  /* Only consider subregs... */
  rtx use_reg = DF_REF_REG (ref);
  machine_mode use_mode = GET_MODE (use_reg);
  if (GET_CODE (use_reg) != SUBREG
      || GET_MODE (SUBREG_REG (use_reg)) != GET_MODE (dest))
    return false;

  /* ??? Replacing throughout the pattern would help for match_dups.  */
  rtx *loc = DF_REF_LOC (ref);
  if (paradoxical_subreg_p (use_reg))
    {
      /* If this is a paradoxical SUBREG, we have no idea what value the
	 extra bits would have.  However, if the operand is equivalent to
	 a SUBREG whose operand is the same as our mode, and all the modes
	 are within a word, we can just use the inner operand because
	 these SUBREGs just say how to treat the register.  */
      if (GET_CODE (src) == SUBREG
	  && REG_P (SUBREG_REG (src))
	  && REGNO (SUBREG_REG (src)) >= FIRST_PSEUDO_REGISTER
	  && GET_MODE (SUBREG_REG (src)) == use_mode
	  && subreg_lowpart_p (src))
	return try_fwprop_subst (use, def, loc, use_reg, SUBREG_REG (src));
    }

  /* If this is a SUBREG of a ZERO_EXTEND or SIGN_EXTEND, and the SUBREG
     is the low part of the reg being extended then just use the inner
     operand.  Don't do this if the ZERO_EXTEND or SIGN_EXTEND insn will
     be removed due to it matching a LOAD_EXTEND_OP load from memory,
     or due to the operation being a no-op when applied to registers.
     For example, if we have:

	 A: (set (reg:DI X) (sign_extend:DI (reg:SI Y)))
	 B: (... (subreg:SI (reg:DI X)) ...)

     and mode_rep_extended says that Y is already sign-extended,
     the backend will typically allow A to be combined with the
     definition of Y or, failing that, allow A to be deleted after
     reload through register tying.  Introducing more uses of Y
     prevents both optimisations.  */
  else if (is_a <scalar_int_mode> (use_mode, &int_use_mode)
	   && subreg_lowpart_p (use_reg))
    {
      if ((GET_CODE (src) == ZERO_EXTEND
	   || GET_CODE (src) == SIGN_EXTEND)
	  && is_a <scalar_int_mode> (GET_MODE (src), &src_mode)
	  && REG_P (XEXP (src, 0))
	  && REGNO (XEXP (src, 0)) >= FIRST_PSEUDO_REGISTER
	  && GET_MODE (XEXP (src, 0)) == use_mode
	  && !free_load_extend (src, def->insn ())
	  && (targetm.mode_rep_extended (int_use_mode, src_mode)
	      != (int) GET_CODE (src)))
	return try_fwprop_subst (use, def, loc, use_reg, XEXP (src, 0));
    }

  return false;
}

/* Try to substitute (set DEST SRC), which defines DEF, into USE and simplify
   the result, handling cases where DEST is used in a subreg and where
   applying that subreg to SRC results in a useful simplification.  */

static bool
forward_propagate_subreg (use_info *use, set_info *def, rtx dest, rtx src)
{
  if (!use->includes_subregs () || !REG_P (dest))
    return false;

  if (GET_CODE (src) != SUBREG
      && GET_CODE (src) != ZERO_EXTEND
      && GET_CODE (src) != SIGN_EXTEND)
    return false;

  rtx_insn *use_rtl = use->insn ()->rtl ();
  df_ref ref;

  FOR_EACH_INSN_USE (ref, use_rtl)
    if (DF_REF_REGNO (ref) == use->regno ()
	&& forward_propagate_subreg (use, def, dest, src, ref))
      return true;

  FOR_EACH_INSN_EQ_USE (ref, use_rtl)
    if (DF_REF_REGNO (ref) == use->regno ()
	&& forward_propagate_subreg (use, def, dest, src, ref))
      return true;

  return false;
}

/* Try to substitute (set DEST SRC), which defines DEF, into USE and
   simplify the result.  */

static bool
forward_propagate_and_simplify (use_info *use, set_info *def,
				rtx dest, rtx src)
{
  insn_info *use_insn = use->insn ();
  rtx_insn *use_rtl = use_insn->rtl ();
  insn_info *def_insn = def->insn ();

  /* ??? This check seems unnecessary.  We should be able to propagate
     into any kind of instruction, regardless of whether it's a single set.
     It seems odd to be more permissive with asms than normal instructions.  */
  bool need_single_set = (!use_insn->is_asm () && !use_insn->is_debug_insn ());
  rtx use_set = single_set (use_rtl);
  if (need_single_set && !use_set)
    return false;

  /* Do not propagate into PC etc.

     ??? This too seems unnecessary.  The current code should work correctly
     without it, including cases where jumps become unconditional.  */
  if (use_set && GET_MODE (SET_DEST (use_set)) == VOIDmode)
    return false;

  /* In __asm don't replace if src might need more registers than
     reg, as that could increase register pressure on the __asm.  */
  if (use_insn->is_asm () && def_insn->uses ().size () > 1)
    return false;

  /* Check if the def is loading something from the constant pool; in this
     case we would undo optimization such as compress_float_constant.
     Still, we can set a REG_EQUAL note.  */
  if (MEM_P (src) && MEM_READONLY_P (src))
    {
      rtx x = avoid_constant_pool_reference (src);
      rtx note_set;
      if (x != src
	  && (note_set = set_for_reg_notes (use_rtl))
	  && REG_P (SET_DEST (note_set))
	  && !contains_paradoxical_subreg_p (SET_SRC (note_set)))
	{
	  rtx note = find_reg_note (use_rtl, REG_EQUAL, NULL_RTX);
	  rtx old_rtx = note ? XEXP (note, 0) : SET_SRC (note_set);
	  rtx new_rtx = simplify_replace_rtx (old_rtx, src, x);
	  if (old_rtx != new_rtx)
	    set_unique_reg_note (use_rtl, REG_EQUAL, copy_rtx (new_rtx));
	}
      return false;
    }

  /* ??? Unconditionally propagating into PATTERN would work better
     for instructions that have match_dups.  */
  rtx *loc = need_single_set ? &use_set : &PATTERN (use_rtl);
  return try_fwprop_subst (use, def, loc, dest, src);
}

/* Given a use USE of an insn, if it has a single reaching
   definition, try to forward propagate it into that insn.
   Return true if something changed.

   REG_PROP_ONLY is true if we should only propagate register copies.  */

static bool
forward_propagate_into (use_info *use, bool reg_prop_only = false)
{
  if (use->includes_read_writes ())
    return false;

  /* Disregard uninitialized uses.  */
  set_info *def = use->def ();
  if (!def)
    return false;

  /* Only consider single-register definitions.  This could be relaxed,
     but it should rarely be needed before RA.  */
  def = look_through_degenerate_phi (def);
  if (def->includes_multiregs ())
    return false;

  /* Only consider uses whose definition comes from a real instruction.  */
  insn_info *def_insn = def->insn ();
  if (def_insn->is_artificial ())
    return false;

  rtx_insn *def_rtl = def_insn->rtl ();
  if (!NONJUMP_INSN_P (def_rtl))
    return false;
  /* ??? This seems an unnecessary restriction.  We can easily tell
     which set the definition comes from.  */
  if (multiple_sets (def_rtl))
    return false;
  rtx def_set = simple_regno_set (PATTERN (def_rtl), def->regno ());
  if (!def_set)
    return false;

  rtx dest = SET_DEST (def_set);
  rtx src = SET_SRC (def_set);
  if (volatile_refs_p (src))
    return false;

  /* Allow propagations into a loop only for reg-to-reg copies, since
     replacing one register by another shouldn't increase the cost.
     Propagations from inner loop to outer loop should also be ok.  */
  struct loop *def_loop = def_insn->bb ()->cfg_bb ()->loop_father;
  struct loop *use_loop = use->bb ()->cfg_bb ()->loop_father;
  if ((reg_prop_only
       || (def_loop != use_loop
	   && !flow_loop_nested_p (use_loop, def_loop)))
      && (!reg_single_def_p (dest) || !reg_single_def_p (src)))
    return false;

  /* Don't substitute into a non-local goto, this confuses CFG.  */
  insn_info *use_insn = use->insn ();
  rtx_insn *use_rtl = use_insn->rtl ();
  if (JUMP_P (use_rtl)
      && find_reg_note (use_rtl, REG_NON_LOCAL_GOTO, NULL_RTX))
    return false;

  if (forward_propagate_and_simplify (use, def, dest, src)
      || forward_propagate_subreg (use, def, dest, src))
    return true;

  return false;
}

static void
fwprop_init (void)
{
  num_changes = 0;
  calculate_dominance_info (CDI_DOMINATORS);

  /* We do not always want to propagate into loops, so we have to find
     loops and be careful about them.  Avoid CFG modifications so that
     we don't have to update dominance information afterwards for
     build_single_def_use_links.  */
  loop_optimizer_init (AVOID_CFG_MODIFICATIONS);

  df_analyze ();
  crtl->ssa = new rtl_ssa::function_info (cfun);
}

static void
fwprop_done (void)
{
  loop_optimizer_finalize ();

  crtl->ssa->perform_pending_updates ();
  free_dominance_info (CDI_DOMINATORS);
  cleanup_cfg (0);

  delete crtl->ssa;
  crtl->ssa = nullptr;

  delete_trivially_dead_insns (get_insns (), max_reg_num ());

  if (dump_file)
    fprintf (dump_file,
	     "\nNumber of successful forward propagations: %d\n\n",
	     num_changes);
}

/* Try to optimize INSN, returning true if something changes.
   FWPROP_ADDR_P is true if we are running fwprop_addr rather than
   the full fwprop.  */

static bool
fwprop_insn (insn_info *insn, bool fwprop_addr_p)
{
  for (use_info *use : insn->uses ())
    {
      if (use->is_mem ())
	continue;
      /* ??? The choices here follow those in the pre-SSA code.  */
      if (!use->includes_address_uses ())
	{
	  if (forward_propagate_into (use, fwprop_addr_p))
	    return true;
	}
      else
	{
	  struct loop *loop = insn->bb ()->cfg_bb ()->loop_father;
	  /* The outermost loop is not really a loop.  */
	  if (loop == NULL || loop_outer (loop) == NULL)
	    {
	      if (forward_propagate_into (use, fwprop_addr_p))
		return true;
	    }
	  else if (fwprop_addr_p)
	    {
	      if (forward_propagate_into (use, false))
		return true;
	    }
	}
    }
  return false;
}

/* Main entry point.  */

static bool
gate_fwprop (void)
{
  return optimize > 0 && flag_forward_propagate;
}

static unsigned int
fwprop (bool fwprop_addr_p)
{
  fwprop_init ();

  /* Go through all the instructions (including debug instructions) looking
     for uses that we could propagate into.

     Do not forward propagate addresses into loops until after unrolling.
     CSE did so because it was able to fix its own mess, but we are not.  */

  insn_info *next;

  /* ??? This code uses a worklist in order to preserve the behavior
     of the pre-SSA implementation.  It would be better to instead
     iterate on each instruction until no more propagations are
     possible, then move on to the next.  */
  auto_vec<insn_info *> worklist;
  for (insn_info *insn = crtl->ssa->first_insn (); insn; insn = next)
    {
      next = insn->next_any_insn ();
      if (insn->can_be_optimized () || insn->is_debug_insn ())
	if (fwprop_insn (insn, fwprop_addr_p))
	  worklist.safe_push (insn);
    }
  for (unsigned int i = 0; i < worklist.length (); ++i)
    {
      insn_info *insn = worklist[i];
      if (fwprop_insn (insn, fwprop_addr_p))
	worklist.safe_push (insn);
    }

  fwprop_done ();
  return 0;
}

namespace {

const pass_data pass_data_rtl_fwprop =
{
  RTL_PASS, /* type */
  "fwprop1", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_FWPROP, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_rtl_fwprop : public rtl_opt_pass
{
public:
  pass_rtl_fwprop (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_rtl_fwprop, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override { return gate_fwprop (); }
  unsigned int execute (function *) final override { return fwprop (false); }

}; // class pass_rtl_fwprop

} // anon namespace

rtl_opt_pass *
make_pass_rtl_fwprop (gcc::context *ctxt)
{
  return new pass_rtl_fwprop (ctxt);
}

namespace {

const pass_data pass_data_rtl_fwprop_addr =
{
  RTL_PASS, /* type */
  "fwprop2", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_FWPROP, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_rtl_fwprop_addr : public rtl_opt_pass
{
public:
  pass_rtl_fwprop_addr (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_rtl_fwprop_addr, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override { return gate_fwprop (); }
  unsigned int execute (function *) final override { return fwprop (true); }

}; // class pass_rtl_fwprop_addr

} // anon namespace

rtl_opt_pass *
make_pass_rtl_fwprop_addr (gcc::context *ctxt)
{
  return new pass_rtl_fwprop_addr (ctxt);
}
