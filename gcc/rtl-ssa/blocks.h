// Basic-block-related classes for RTL SSA                          -*- C++ -*-
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

// SSA-related information about a basic block.  Each block contains
// the following, which are conceptually executed in order:
//
// - an artificial "head" insn_info that holds artificial uses and definitions
//   for the start of the block.
//
// - one insn_info for each "real" instruction in the block
//   (i.e. those that have an RTL pattern).
//
// - an artificial "end" insn_info that holds artificial uses and definitions
//   for the end of the block.
//
// Blocks are grouped together into extended basic blocks.  In cases where
// multiple EBBs exist (such as in a full diamond), we try to pick the one
// that's most frequently executed.
//
// Blocks are chained together in reverse postorder.  (Rather than use a
// list, we could instead have stored the index of the block in the overall
// postorder.  However, using lists should make it cheaper to update the
// information after trivial CFG manipulations.)
class bb_info
{
  // Size: 6 LP64 words.
  friend class function_info;

public:
  // Return the previous basic block in reverse postorder, or null if this
  // is the entry block.
  bb_info *prev_bb () const { return m_prev_bb; }

  // Return the next basic block in reverse postorder, or null if this
  // is the exit block.
  bb_info *next_bb () const { return m_next_bb; }

  // Return true if this block is the function's entry block.
  bool is_entry_block () const { return !m_prev_bb; }

  // Return true if this block is the function's exit block.
  bool is_exit_block () const { return !m_next_bb; }

  // Return the underlying basic_block structure.
  basic_block cfg_bb () const { return m_cfg_bb; }

  // Return the unique identifier of the underlying basic_block.  These uids
  // do not follow any particular order.
  unsigned int index () const { return m_cfg_bb->index; }

  // Return the EBB that contains this block.
  ebb_info *ebb () const { return m_ebb; }

  // Return a list of all the instructions in the block, in execution order.
  // The list includes the head and end instructions described above.
  //
  // Iterations over the list will pick up any new instructions that are
  // inserted after the iterator's current instruction.
  iterator_range<any_insn_iterator> all_insns () const;

  // Like all_insns (), except that the instructions are in reverse order.
  //
  // Iterations over the list will pick up any new instructions that are
  // inserted before the iterator's current instruction.
  iterator_range<reverse_any_insn_iterator> reverse_all_insns () const;

  // Like all_insns (), but without the debug instructions.
  iterator_range<nondebug_insn_iterator> nondebug_insns () const;

  // Like reverse_all_insns (), but without the debug instructions.
  iterator_range<reverse_nondebug_insn_iterator>
    reverse_nondebug_insns () const;

  // Like all_insns (), but without the artificial instructions.
  iterator_range<any_insn_iterator> real_insns () const;

  // Like reverse_all_insns (), but without the artificial instructions.
  iterator_range<reverse_any_insn_iterator> reverse_real_insns () const;

  // Like real_insns (), but without the debug instructions.
  iterator_range<nondebug_insn_iterator> real_nondebug_insns () const;

  // Like reverse_real_insns (), but without the debug instructions.
  iterator_range<reverse_nondebug_insn_iterator>
    reverse_real_nondebug_insns () const;

  // Return the instruction that holds the artificial uses and
  // definitions at the head of the block.  The associated RTL insn
  // is the block head note.
  //
  // This instruction always exists, even if it has no uses and definitions.
  insn_info *head_insn () const { return m_head_insn; }

  // Return the instruction that holds the artificial uses and definitions
  // at the end of the block.  There is no associated RTL insn.
  //
  // This instruction always exists, even if it has no uses and definitions.
  insn_info *end_insn () const { return m_end_insn; }

  // Print "bb" + index () to PP.
  void print_identifier (pretty_printer *pp) const;

  // Print a full description of the block to PP.
  void print_full (pretty_printer *) const;

private:
  bb_info (basic_block);

  void set_prev_bb (bb_info *bb) { m_prev_bb = bb; }
  void set_next_bb (bb_info *bb) { m_next_bb = bb; }
  void set_cfg_bb (basic_block cfg_bb) { m_cfg_bb = cfg_bb; }
  void set_ebb (ebb_info *ebb) { m_ebb = ebb; }
  void set_head_insn (insn_info *insn) { m_head_insn = insn; }
  void set_end_insn (insn_info *insn) { m_end_insn = insn; }

  // The values returned by the functions above.
  bb_info *m_prev_bb;
  bb_info *m_next_bb;
  basic_block m_cfg_bb;
  ebb_info *m_ebb;
  insn_info *m_head_insn;
  insn_info *m_end_insn;
};

// Iterators for lists of basic blocks.
using bb_iterator = list_iterator<bb_info, &bb_info::next_bb>;
using reverse_bb_iterator = list_iterator<bb_info, &bb_info::prev_bb>;

// This class collects together instructions for which has_call_clobbers ()
// is true, storing them in a splay tree that follows reverse postorder.
// Instances of the class form a singly-linked list, with one instance
// per predefined_function_abi.
class ebb_call_clobbers_info : public insn_call_clobbers_tree
{
  // Size 3 LP64 words.
  friend class function_info;

public:
  // Return the next group in the list.
  ebb_call_clobbers_info *next () const { return m_next; }

  // Return the function abi used by all the calls in the group.
  const predefined_function_abi *abi () const { return m_abi; }

  // Return true if at least one call in the group should conservatively
  // be assumed to clobber RESOURCE.
  bool clobbers (resource_info) const;

  // Print a summary of what the class describes to PP, without printing
  // the actual instructions.
  void print_summary (pretty_printer *pp) const;

  // Print a full description of the object to PP, including the
  // instructions it contains.
  void print_full (pretty_printer *) const;

private:
  ebb_call_clobbers_info (const predefined_function_abi *);

  // The values returned by the accessors above.
  ebb_call_clobbers_info *m_next;
  const predefined_function_abi *m_abi;
};

// A list of ebb_call_clobbers_infos.
using ebb_call_clobbers_iterator
  = list_iterator<ebb_call_clobbers_info, &ebb_call_clobbers_info::next>;

// Information about an extended basic block.
//
// Each EBB has a list of phi nodes and starts with an artificial phi
// instruction that conceptually "executes" the phi nodes.  The phi
// nodes are independent of one another and so can be executed in any
// order.  The order of the phi nodes in the list is not significant.
//
// Each EBB also maintains a list of ebb_call_clobbers_info structures
// that describe all instructions for which has_call_clobbers () is true.
// See the comment above that class for details.
class ebb_info
{
  // Size: 5 LP64 words.
  friend class function_info;

public:
  // Return the previous EBB in reverse postorder, or null if this EBB
  // contains the entry block.
  ebb_info *prev_ebb () const;

  // Return the next EBB in reverse postorder, or null if this EBB contains
  // the exit block.
  ebb_info *next_ebb () const;

  // Return the instruction that holds the EBB's phi nodes (and does
  // nothing else).  There is no associated RTL insn.
  //
  // This instruction always exists, even if the EBB does not currently
  // need any phi nodes.
  insn_info *phi_insn () const { return m_phi_insn; }

  // Return the first and last blocks in the EBB.
  bb_info *first_bb () const { return m_first_bb; }
  bb_info *last_bb () const { return m_last_bb; }

  // Return the first of the EBB's phi nodes.
  phi_info *first_phi () const { return m_first_phi; }

  // Return the head of the list of ebb_call_clobbers_infos.
  ebb_call_clobbers_info *first_call_clobbers () const;

  // Return the list of ebb_call_clobbers_infos.
  iterator_range<ebb_call_clobbers_iterator> call_clobbers () const;

  // Return a list of the EBB's phi nodes, in arbitrary order.
  iterator_range<phi_iterator> phis () const;

  // Return a list of the blocks in the EBB, in execution order.
  iterator_range<bb_iterator> bbs () const;

  // Return a list of the blocks in the EBB, in reverse execution order.
  iterator_range<reverse_bb_iterator> reverse_bbs () const;

  // Return a list of all the instructions in the EBB, in execution order.
  // The list includes phi_insn (), the head and end of each block,
  // and the real instructions in each block.
  //
  // Iterations over the list will pick up any new instructions that are
  // inserted after the iterator's current instruction.
  iterator_range<any_insn_iterator> all_insns () const;

  // Like all_insns (), except that the instructions are in reverse order.
  //
  // Iterations over the list will pick up any new instructions that are
  // inserted before the iterator's current instruction.
  iterator_range<reverse_any_insn_iterator> reverse_all_insns () const;

  // Like all_insns (), but without the debug instructions.
  iterator_range<nondebug_insn_iterator> nondebug_insns () const;

  // Like reverse_all_insns (), but without the debug instructions.
  iterator_range<reverse_nondebug_insn_iterator>
    reverse_nondebug_insns () const;

  // Return an insn_range that covers the same instructions as all_insns ().
  insn_range_info insn_range () const;

  // Print "ebb" + first_bb ()->index () to PP.
  void print_identifier (pretty_printer *pp) const;

  // Print a full description of the EBB to PP.
  void print_full (pretty_printer *pp) const;

private:
  ebb_info (bb_info *, bb_info *);

  void set_first_phi (phi_info *phi) { m_first_phi = phi; }
  void set_phi_insn (insn_info *insn) { m_phi_insn = insn; }
  void set_first_call_clobbers (ebb_call_clobbers_info *);

  // The values returned by the functions above.
  phi_info *m_first_phi;
  insn_info *m_phi_insn;
  bb_info *m_first_bb;
  bb_info *m_last_bb;
  ebb_call_clobbers_info *m_first_call_clobbers;
};

// Iterators for lists of extended basic blocks.
using ebb_iterator = list_iterator<ebb_info, &ebb_info::next_ebb>;
using reverse_ebb_iterator = list_iterator<ebb_info, &ebb_info::prev_ebb>;

void pp_bb (pretty_printer *, const bb_info *);
void pp_ebb_call_clobbers (pretty_printer *, const ebb_call_clobbers_info *);
void pp_ebb (pretty_printer *, const ebb_info *);

}

void dump (FILE *, const rtl_ssa::bb_info *);
void dump (FILE *, const rtl_ssa::ebb_call_clobbers_info *);
void dump (FILE *, const rtl_ssa::ebb_info *);

void DEBUG_FUNCTION debug (const rtl_ssa::bb_info *);
void DEBUG_FUNCTION debug (const rtl_ssa::ebb_call_clobbers_info *);
void DEBUG_FUNCTION debug (const rtl_ssa::ebb_info *);
