// Instruction-related utilities for RTL SSA                        -*- C++ -*-
// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

// Return whichever of INSN1 and INSN2 occurs earlier in the function's
// reverse postorder.
inline insn_info *
earlier_insn (insn_info *insn1, insn_info *insn2)
{
  return *insn1 < *insn2 ? insn1 : insn2;
}

// Return whichever of INSN1 and INSN2 occurs later in the function's
// reverse postorder.
inline insn_info *
later_insn (insn_info *insn1, insn_info *insn2)
{
  return *insn1 < *insn2 ? insn2 : insn1;
}

// Return a closure of operator== for INSN.  See insn_is_changing for
// the rationale for defining the function this way.
inline insn_is_closure
insn_is (const insn_info *insn)
{
  return insn_is_closure (insn);
}

}
