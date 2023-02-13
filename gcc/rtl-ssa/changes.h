// RTL SSA classes related to changing instructions                 -*- C++ -*-
// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

namespace rtl_ssa {

// A class that describes a change that we're considering making to an
// instruction.  There are three choices:
//
// (1) delete the instruction
// (2) replace the instruction with a new instruction in-place
// (3) replace the instruction with a new instruction at a different location
//
// Anything related to the "new instruction" is irrelevant for (1).
//
// The class doesn't actually change anything itself, it simply records
// something that we might do.
class insn_change
{
public:
  enum delete_action { DELETE };

  // Construct a possible change to INSN.
  insn_change (insn_info *insn);

  // Construct a possible deletion of INSN.
  insn_change (insn_info *insn, delete_action);

  // The instruction that we would change.
  insn_info *insn () const { return m_insn; }

  // The rtx_insn of the instruction that we would change.
  rtx_insn *rtl () const { return m_insn->rtl (); }

  // The basic block that contains insn ().
  bb_info *bb () const { return m_insn->bb (); }

  // The extended basic block that contains insn ().
  ebb_info *ebb () const { return m_insn->ebb (); }

  // The uid of the instruction that we would change.
  unsigned int insn_uid () const { return m_insn->uid (); }

  // The list of things that the original instruction defined and used.
  def_array old_defs () const { return m_insn->defs (); }
  use_array old_uses () const { return m_insn->uses (); }

  // The cost of the original instruction, as calculated by the target.
  unsigned int old_cost () const { return m_insn->cost (); }

  // Return true if the original instruction would simply be deleted,
  // rather than being replaced by a new instruction.
  bool is_deletion () const { return m_is_deletion; }

  // Print a description of the change to PP.
  void print (pretty_printer *pp) const;

  // Return an insn_change for deleting INSN.
  static insn_change delete_insn (insn_info *insn) { return { insn, DELETE }; }

private:
  // The value returned by insn ().
  insn_info *m_insn;

public:
  // The list of things that the new instruction would define and use.
  def_array new_defs;
  use_array new_uses;

  // The range of instructions after which the instruction could be placed.
  // The range can include INSN itself: placing the instruction after either
  // INSN or INSN->prev_nondebug_insn () is equivalent to not moving the
  // instruction.
  insn_range_info move_range;

  // The cost that the new instruction would have, as calculated by the target.
  unsigned int new_cost;

private:
  // The value returned by is_deletion ().
  bool m_is_deletion;
};

// A class that represents a closure of the two-argument form of
// insn_is_changing.  See the comment above the one-argument form
// for details.
class insn_is_changing_closure
{
public:
  insn_is_changing_closure (array_slice<insn_change *const> changes);
  bool operator() (const insn_info *) const;

private:
  array_slice<insn_change *const> m_changes;
};

void pp_insn_change (pretty_printer *, const insn_change &);

}

void dump (FILE *, const rtl_ssa::insn_change &);

void DEBUG_FUNCTION debug (const rtl_ssa::insn_change &);
