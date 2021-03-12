// Definition of private classes for RTL SSA                        -*- C++ -*-
// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

// Information about a basic block's phi nodes.  This class is only used when
// constructing the SSA form, it isn't meant to be kept up-to-date.
class function_info::bb_phi_info
{
public:
  // The set of registers that need phi nodes.
  bitmap_head regs;

  // The number of registers in REGS.
  unsigned int num_phis;

  // The number of inputs to each phi node.  Caching the information here
  // is at best a minor optimisation, but it fills a 32-bit hole that would
  // otherwise exist on 64-bit hosts.
  unsigned int num_preds;

  // An array of all the phi inputs for this block.  It lists all inputs
  // from the first incoming edge followed by all inputs for the next
  // incoming edge, and so on.  The inputs for a given edge are sorted
  // by increasing register number.
  set_info **inputs;
};

// Information used while constructing the SSA form and discarded
// afterwards.
class function_info::build_info
{
public:
  build_info (unsigned int, unsigned int);
  ~build_info ();

  set_info *current_reg_value (unsigned int) const;
  set_info *current_mem_value () const;

  void record_reg_def (def_info *);
  void record_mem_def (def_info *);

  // The block that we're currently processing.
  bb_info *current_bb;

  // The EBB that contains CURRENT_BB.
  ebb_info *current_ebb;

  // Except for the local exception noted below:
  //
  // - If register R has been defined in the current EBB, LAST_ACCESS[R + 1]
  //   is the last definition of R in the EBB.
  //
  // - Otherwise, if the current EBB is dominated by a definition of R,
  //   LAST_ACCESS[R + 1] is the nearest dominating definition.
  //
  // - Otherwise, LAST_ACCESS[R + 1] is null.
  //
  // Similarly:
  //
  // - If the current EBB has defined memory, LAST_ACCESS[0] is the last
  //   definition of memory in the EBB.
  //
  // - Otherwise LAST_ACCESS[0] is the value of memory that is live on
  // - entry to the EBB.
  //
  // The exception is that while building instructions, LAST_ACCESS[I]
  // can temporarily be the use of regno I - 1 by that instruction.
  auto_vec<access_info *> last_access;

  // A bitmap used to hold EBB_LIVE_IN_FOR_DEBUG.
  auto_bitmap tmp_ebb_live_in_for_debug;

  // If nonnull, a bitmap of registers that are live on entry to this EBB,
  // with a tree view for quick lookup.  This bitmap is calculated lazily
  // and is only used if MAY_HAVE_DEBUG_INSNS.
  bitmap ebb_live_in_for_debug;

  // The set of registers that might need to have phis associated with them.
  // Registers outside this set are known to have a single definition that
  // dominates all uses.
  //
  // Before RA, about 5% of registers are typically in the set.
  auto_sbitmap potential_phi_regs;

  // A sparse bitmap representation of POTENTIAL_PHI_REGS.  Only used if
  // MAY_HAVE_DEBUG_INSNS.
  auto_bitmap potential_phi_regs_for_debug;

  // The set of registers that have been defined so far in the current EBB.
  auto_bitmap ebb_def_regs;

  // BB_PHIS[B] describes the phis for basic block B.
  auto_vec<bb_phi_info> bb_phis;

  // BB_MEM_LIVE_OUT[B] is the memory value that is live on exit from
  // basic block B.
  auto_vec<set_info *> bb_mem_live_out;

  // BB_TO_RPO[B] gives the position of block B in a reverse postorder
  // of the CFG.  The RPO is a tweaked version of the one normally
  // returned by pre_and_rev_post_order_compute, with all blocks in
  // an EBB having consecutive positions.
  auto_vec<int> bb_to_rpo;

  // This stack is divided into sections, with one section for the
  // current basic block and one section for each dominating block.
  // Each element is a register definition.
  //
  // If the section for block B contains a definition D of a register R,
  // then one of two things is true:
  //
  // - D occurs in B and no definition of R dominates B.
  // - D dominates B and is the nearest dominating definition of R.
  //
  // The two cases are distinguished by the value of D->bb ().
  auto_vec<def_info *> def_stack;

  // The top of this stack records the start of the current block's
  // section in DEF_STACK.
  auto_vec<unsigned int> old_def_stack_limit;
};

}
