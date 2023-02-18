// Instruction-related RTL SSA classes                              -*- C++ -*-
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

// A fake cost for instructions that we haven't costed yet.
const int UNKNOWN_COST = INT_MAX;

// Enumerates the kinds of note that can be added to an instruction.
// See the comment above insn_info for details.
enum class insn_note_kind : uint8_t
{
  ORDER_NODE,
  CALL_CLOBBERS
};

// The base class for notes that can be added to an instruction.
// See the comment above insn_info for details.
class insn_note
{
  // Size: 2 LP64 words.
  friend class insn_info;
  friend class function_info;

public:
  // Return what kind of note this is.
  insn_note_kind kind () const { return m_kind; }

  // Return the next note in the list, or null if none.
  insn_note *next_note () const { return m_next_note; }

  // Used with T = Derived *, where Derived is derived from insn_note.
  // Convert the note to Derived, asserting that it has the right kind.
  template<typename T>
  T as_a ();

  // Used with T = Derived *, where Derived is derived from insn_note.
  // If the note is a Derived note, return it in that form, otherwise
  // return null.
  template<typename T>
  T dyn_cast ();

protected:
  // Construct a note with the given kind.
  insn_note (insn_note_kind);

private:
  // The next note in the list, or null if none.
  insn_note *m_next_note;

  // The kind of note this is.
  insn_note_kind m_kind : 8;

protected:
  // Fill in the remaining LP64 word with data that derived classes can use.
  unsigned int m_data8 : 8;
  unsigned int m_data16 : 16;
  unsigned int m_data32 : 32;
};

// Instructions have one of these notes if insn_info::has_call_clobbers ()
// is true.  All such instructions in an EBB are first grouped together
// by the predefined_function_abis of the functions that they call.
// Then, for each such predefined ABI, the call_clobbers notes are put
// into a splay tree whose nodes follow execution order.
class insn_call_clobbers_note : public insn_note
{
  friend class function_info;
  friend class default_splay_tree_accessors<insn_call_clobbers_note *>;

public:
  static const insn_note_kind kind = insn_note_kind::CALL_CLOBBERS;

  // Return the identifier of the predefined_function_abi.
  unsigned int abi_id () const { return m_data32; }

  // Return the instruction to which the note is attached.
  insn_info *insn () const { return m_insn; }

protected:
  insn_call_clobbers_note (unsigned int abi_id, insn_info *insn);

  // The splay tree pointers.
  insn_call_clobbers_note *m_children[2];

  // The value returned by insn ().
  insn_info *m_insn;
};

// A splay tree of insn_call_clobbers_notes.
using insn_call_clobbers_tree = default_splay_tree<insn_call_clobbers_note *>;

// SSA-related information about an instruction.  It also represents
// artificial instructions that are added to make the dataflow correct;
// these artificial instructions fall into three categories:
//
// - Instructions that hold the phi nodes for an extended basic block (is_phi).
//
// - Instructions that represent the head of a basic block and that hold
//   all the associated artificial uses and definitions.
//
// - Instructions that represent the end of a basic block and that again
//   hold all the associated artificial uses and definitions.
//
// Dataflow-wise, each instruction goes through three stages:
//
// (1) Use all the values in uses ().
//
// (2) If has_call_clobbers (), clobber the registers indicated by
//     insn_callee_abi.
//
// (3) Define all the values in defs ().
//
// Having stage (2) is a trade-off: it makes processing the instructions
// more complicated, but it saves having to allocate memory for every
// individual call clobber.  Without it, clobbers for calls would often
// make up a large proportion of the total definitions in a function.
//
// All the instructions in a function are chained together in a list
// that follows a reverse postorder traversal of the CFG.  The list
// contains both debug and nondebug instructions, but it is possible
// to hop from one nondebug instruction to the next with constant complexity.
//
// Instructions can have supplemental information attached in the form
// of "notes", a bit like REG_NOTES for the underlying RTL insns.
class insn_info
{
  // Size: 9 LP64 words.
  friend class ebb_info;
  friend class function_info;

public:
  // Compare instructions by their positions in the function list described
  // above.  Thus for two instructions in the same basic block, I1 < I2 if
  // I1 comes before I2 in the block.
  bool operator< (const insn_info &) const;
  bool operator<= (const insn_info &) const;
  bool operator>= (const insn_info &) const;
  bool operator> (const insn_info &) const;

  // Return -1 if this instruction comes before INSN in the reverse
  // postorder, 0 if this instruction is INSN, or 1 if this instruction
  // comes after INSN in the reverse postorder.
  int compare_with (const insn_info *insn) const;

  // Return the previous and next instructions in the list described above,
  // or null if there are no such instructions.
  insn_info *prev_any_insn () const;
  insn_info *next_any_insn () const;

  // Only valid if !is_debug_insn ().  Return the previous and next
  // nondebug instructions in the list described above, skipping over
  // any intervening debug instructions.  These are constant-time operations.
  insn_info *prev_nondebug_insn () const;
  insn_info *next_nondebug_insn () const;

  // Return the underlying RTL insn.  This instruction is null if is_phi ()
  // or is_bb_end () are true.  The instruction is a basic block note if
  // is_bb_head () is true.
  rtx_insn *rtl () const { return m_rtl; }

  // Return true if the instruction is a real insn with an rtl pattern.
  // Return false if it is an artificial instruction that represents the
  // phi nodes in an extended basic block or the head or end of a basic block.
  bool is_real () const { return m_cost_or_uid >= 0; }

  // Return the opposite of is_real ().
  bool is_artificial () const { return m_cost_or_uid < 0; }

  // Return true if the instruction was a real instruction but has now
  // been deleted.  In this case the instruction is no longer part of
  // the SSA information.
  bool has_been_deleted () const { return m_rtl && !INSN_P (m_rtl); }

  // Return true if the instruction is a debug instruction (and thus
  // also a real instruction).
  bool is_debug_insn () const { return m_is_debug_insn; }

  // Return true if the instruction is something that we can optimize.
  // This implies that it is a real instruction that contains an asm
  // or that contains something that matches an .md define_insn pattern.
  bool can_be_optimized () const { return m_can_be_optimized; }

  // Return true if the instruction is a call instruction.
  //
  // ??? We could cache this information, but since most callers would
  // go on to access PATTERN (rtl ()), a cache might not be helpful and
  // could even be counterproductive.
  bool is_call () const { return CALL_P (m_rtl); }

  // Return true if the instruction is a jump instruction.
  //
  // ??? See is_call for the reason we don't cache this.
  bool is_jump () const { return JUMP_P (m_rtl); }

  // Return true if the instruction is real and contains an inline asm.
  bool is_asm () const { return m_is_asm; }

  // Return true if the instruction is real and includes an RTX_AUTOINC
  // operation.
  bool has_pre_post_modify () const { return m_has_pre_post_modify; }

  // Return true if the instruction is real and has volatile references,
  // in the sense of volatile_refs_p.  This includes volatile memory,
  // volatile asms and UNSPEC_VOLATILEs.
  bool has_volatile_refs () const { return m_has_volatile_refs; }

  // Return true if the instruction is aritificial and if its (sole)
  // purpose is to hold the phi nodes in an extended basic block.
  bool is_phi () const;

  // Return true if the instruction is artificial and if it represents
  // the head of a basic block.  If so, the instruction conceptually
  // executes before the real instructions in the block.  The uses
  // and definitions represent the df_get_artificial_uses and
  // df_get_artificial_defs entries for the head of the block.
  bool is_bb_head () const;

  // Return true if the instruction is artificial and if it represents
  // the end of a basic block.  The uses and definitions represent the
  // the df_get_artificial_uses and df_get_artificial_defs entries for
  // the end of the block.
  bool is_bb_end () const;

  // Return the basic block that the instruction is in.
  bb_info *bb () const { return m_bb; }

  // Return the extended basic block that the instruction is in;
  // see bb_info for details.
  ebb_info *ebb () const;

  // If the instruction is real, return the unique identifier of the
  // underlying RTL insn.  If the instruction is artificial, return
  // a unique negative identifier for the instructions.
  //
  // Note that the identifiers are not linear: it can be the case that
  // an instruction with a higher uid comes earlier in a block than an
  // instruction with a lower uid.  The identifiers are however persistent;
  // the identifier remains the same after the instruction has been moved
  // or changed.
  int uid () const;

  // Return the list of things that this instruction uses.  Registers
  // come first, in register number order, followed by memory.
  use_array uses () const;

  // Return true if the instruction is a call and if the clobbers
  // described by insn_callee_abi have been omitted from the list
  // of definitions.
  bool has_call_clobbers () const;

  // Return the list of things that this instruction sets or clobbers.
  // Registers come first, in register number order, followed by memory.
  //
  // If has_call_clobbers () is true, the list omits both the full and
  // partial register clobbers described by insn_callee_abi.
  def_array defs () const;

  // The number of entries in uses ().
  unsigned int num_uses () const { return m_num_uses; }

  // The number of entries in defs ().
  unsigned int num_defs () const { return m_num_defs; }

  // Return the cost of the instruction, as calculated by the target.
  // For performance reasons, the cost is evaluated lazily on first use.
  //
  // Artificial instructions have a cost of 0.
  unsigned int cost () const;

  // Return the first insn_note attached to the instruction, or null
  // if none.
  insn_note *first_note () const { return m_first_note; }

  // See if a note of type T is attached to the instruction.  Return it
  // if so, otherwise return null.
  template<typename T>
  const T *find_note () const;

  // Print "i" + uid () for real instructions and "a" + -uid () for
  // artificial instructions.
  void print_identifier (pretty_printer *) const;

  // Print a short(ish) description of where the instruction is.
  void print_location (pretty_printer *) const;

  // Combine print_identifier and print_location.
  void print_identifier_and_location (pretty_printer *) const;

  // Print a full description of the instruction.
  void print_full (pretty_printer *) const;

private:
  // The first-order way of representing the order between instructions
  // is to assign "program points", with higher point numbers coming
  // later in the reverse postorder than lower point numbers.  However,
  // after a sequence of instruction movements, we may end up in a situation
  // that adjacent instructions have the same program point.
  //
  // When that happens, we put the instructions into a splay tree that
  // records their relative order.  Each node of the splay tree is an
  // order_node note that is attached to its respective instruction.
  // The root of the splay tree is not stored, since the only thing
  // we need the tree for is to compare two nodes.
  class order_node : public insn_note
  {
  public:
    static const insn_note_kind kind = insn_note_kind::ORDER_NODE;

    order_node (int uid);

    // Return the uid of the instruction that this node describes.
    int uid () const { return m_data32; }

    // The splay tree pointers.
    order_node *m_children[2];
    order_node *m_parent;
  };
  using order_splay_tree = default_rootless_splay_tree<order_node *>;

  // prev_insn_or_last_debug_insn represents a choice between two things:
  //
  // (1) A pointer to the previous instruction in the list that has the
  //     same is_debug_insn () value, or null if no such instruction exists.
  //
  // (2) A pointer to the end of a sublist of debug instructions.
  //
  // (2) is used if this instruction is a debug instruction and the
  // previous instruction is not.  (1) is used otherwise.
  //
  // next_nondebug_or_debug_insn points to the next instruction but also
  // records whether that next instruction is a debug instruction or a
  // nondebug instruction.
  //
  // Thus the list is chained as follows:
  //
  //         ---->        ---->     ---->     ---->     ---->
  // NONDEBUG     NONDEBUG     DEBUG     DEBUG     DEBUG     NONDEBUG ...
  //         <----    ^     +--     <----     <----  ^    +--
  //                  |     |                        |    |
  //                  |     +------------------------+    |
  //                  |                                   |
  //                  +-----------------------------------+
  using prev_insn_or_last_debug_insn = pointer_mux<insn_info>;
  using next_nondebug_or_debug_insn = pointer_mux<insn_info>;

  insn_info (bb_info *bb, rtx_insn *rtl, int cost_or_uid);

  static void print_uid (pretty_printer *, int);

  void calculate_cost () const;
  void set_properties (const rtx_properties &);
  void set_accesses (access_info **, unsigned int, unsigned int);
  void copy_accesses (access_array, access_array);
  void set_cost (unsigned int cost) { m_cost_or_uid = cost; }
  void set_bb (bb_info *bb) { m_bb = bb; }

  void add_note (insn_note *note);

  order_node *get_order_node () const;
  order_node *get_known_order_node () const;
  int slow_compare_with (const insn_info &) const;

  insn_info *last_debug_insn () const;

  unsigned int point () const { return m_point; }
  void copy_prev_from (insn_info *);
  void copy_next_from (insn_info *);
  void set_prev_sametype_insn (insn_info *);
  void set_last_debug_insn (insn_info *);
  void set_next_any_insn (insn_info *);
  void set_point (unsigned int point) { m_point = point; }
  void clear_insn_links ();
  bool has_insn_links ();

  // The values returned by the accessors above.
  prev_insn_or_last_debug_insn m_prev_insn_or_last_debug_insn;
  next_nondebug_or_debug_insn m_next_nondebug_or_debug_insn;
  bb_info *m_bb;
  rtx_insn *m_rtl;

  // The list of definitions followed by the list of uses.
  access_info **m_accesses;

  // The number of definitions and the number uses.  FIRST_PSEUDO_REGISTER + 1
  // is the maximum number of accesses to hard registers and memory, and
  // MAX_RECOG_OPERANDS is the maximum number of pseudos that can be
  // defined by an instruction, so the number of definitions in a real
  // instruction should fit easily in 16 bits.  However, there are no
  // limits on the number of definitions in artifical instructions.
  unsigned int m_num_uses;
  unsigned int m_num_defs;

  // Flags returned by the accessors above.
  unsigned int m_is_debug_insn : 1;
  unsigned int m_can_be_optimized : 1;
  unsigned int m_is_asm : 1;
  unsigned int m_has_pre_post_modify : 1;
  unsigned int m_has_volatile_refs : 1;

  // For future expansion.
  unsigned int m_spare : 27;

  // The program point at which the instruction occurs.
  //
  // Note that the values of the program points are influenced by -g
  // and so should not used to make codegen decisions.
  unsigned int m_point;

  // Negative if the instruction is artificial, nonnegative if it is real.
  //
  // For real instructions: the cost of the instruction, or UNKNOWN_COST
  // if we haven't measured it yet.
  //
  // For artificial instructions: the (negative) unique identifier of the
  // instruction.
  mutable int m_cost_or_uid;

  // On LP64 systems, there's a gap here that could be used for future
  // expansion.

  // The list of notes that have been attached to the instruction.
  insn_note *m_first_note;
};

// Iterators for unfiltered lists of instructions.
using any_insn_iterator = list_iterator<insn_info, &insn_info::next_any_insn>;
using reverse_any_insn_iterator
  = list_iterator<insn_info, &insn_info::prev_any_insn>;

// Iterators for nondebug instructions only.
using nondebug_insn_iterator
  = list_iterator<insn_info, &insn_info::next_nondebug_insn>;
using reverse_nondebug_insn_iterator
  = list_iterator<insn_info, &insn_info::prev_nondebug_insn>;

// A class that describes an inclusive range of instructions.
class insn_range_info
{
public:
  insn_range_info () = default;

  // Create a range that contains a singleton instruction.
  insn_range_info (insn_info *insn) : first (insn), last (insn) {}

  // Create a range [FIRST, LAST], given that *FIRST <= *LAST.
  insn_range_info (insn_info *first, insn_info *last);

  // Return true if the range contains at least one instruction.
  explicit operator bool () const { return *first <= *last; }

  bool operator== (const insn_range_info &) const;
  bool operator!= (const insn_range_info &) const;

  // If the range contains a single instruction, return that instruction,
  // otherwise return null.
  insn_info *singleton () const;

  // Return true if the range includes INSN.
  bool includes (insn_info *insn) const;

  // If INSN is inside the range, return INSN, otherwise return the
  // nearest in-range instruction.
  insn_info *clamp_insn_to_range (insn_info *insn) const;

  // Return true if this range is a subrange of OTHER, i.e. if OTHER
  // includes every instruction that this range does.
  bool is_subrange_of (const insn_range_info &other) const;

  // The lower and upper bounds of the range.
  insn_info *first;
  insn_info *last;
};

// A class that represents a closure of operator== for instructions.
// This is used by insn_is; see there for details.
class insn_is_closure
{
public:
  insn_is_closure (const insn_info *insn) : m_insn (insn) {}
  bool operator() (const insn_info *other) const { return m_insn == other; }

private:
  const insn_info *m_insn;
};

void pp_insn (pretty_printer *, const insn_info *);

}

void dump (FILE *, const rtl_ssa::insn_info *);

void DEBUG_FUNCTION debug (const rtl_ssa::insn_info *);
