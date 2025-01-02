// RTL SSA predicate classes                                        -*- C++ -*-
// Copyright (C) 2024-2025 Free Software Foundation, Inc.
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

// Collects predicates that affect a scan over the IR, specifying what
// (if anything) should be ignored.
struct ignore_nothing
{
  // Return true if the scan should ignore the given definition
  // and all uses of the definition.
  bool should_ignore_def (const def_info *) { return false; }

  // Return true if the scan should ignore the given instruction.
  bool should_ignore_insn (const insn_info *) { return false; }
};

// Predicates that ignore the instruction passed to the constructor
// (and nothing else).
class ignore_insn : public ignore_nothing
{
public:
  ignore_insn (const insn_info *insn) : m_insn (insn) {}
  bool should_ignore_insn (const insn_info *insn) { return insn == m_insn; }

private:
  const insn_info *m_insn;
};

// Predicates that ignore all the instructions being changed by a set
// of insn_changes.
class ignore_changing_insns : public ignore_nothing
{
public:
  ignore_changing_insns (array_slice<insn_change *const>);
  bool should_ignore_insn (const insn_info *);

private:
  array_slice<insn_change *const> m_changes;
};

}
