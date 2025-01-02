// Implementation of public inline member functions for RTL SSA     -*- C++ -*-
// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

// This file contains inline implementations of public member functions that
// are too large to be written in the class definition.  It also contains
// some non-inline template definitions of public member functions.
// See the comments above the function declarations for details.
//
// The file also contains the bare minimum of private and protected inline
// member functions that are needed to make the public functions compile.
namespace rtl_ssa {

inline void
access_array_builder::reserve (unsigned int num_accesses)
{
  obstack_make_room (m_obstack, num_accesses * sizeof (access_info *));
}

inline void
access_array_builder::quick_push (access_info *access)
{
  obstack_ptr_grow_fast (m_obstack, access);
}

inline array_slice<access_info *>
access_array_builder::finish ()
{
  unsigned num_accesses
    = obstack_object_size (m_obstack) / sizeof (access_info *);
  if (num_accesses == 0)
    return {};

  auto **base = static_cast<access_info **> (obstack_finish (m_obstack));
  keep ();
  return { base, num_accesses };
}

inline bool
access_info::is_set_with_nondebug_insn_uses () const
{
  return m_is_set_with_nondebug_insn_uses;
}

inline bool
use_info::is_in_debug_insn () const
{
  return m_insn_or_phi.is_first () && m_is_in_debug_insn_or_phi;
}

inline bb_info *
use_info::bb () const
{
  if (m_insn_or_phi.is_first ())
    return m_insn_or_phi.known_first ()->bb ();
  return m_insn_or_phi.known_second ()->bb ();
}

inline ebb_info *
use_info::ebb () const
{
  return bb ()->ebb ();
}

inline use_info *
use_info::prev_use () const
{
  return m_last_use_or_prev_use.second_or_null ();
}

inline use_info *
use_info::next_use () const
{
  return m_last_nondebug_insn_use_or_next_use.second_or_null ();
}

inline bool
use_info::is_first_use () const
{
  return m_last_use_or_prev_use.is_first ();
}

inline bool
use_info::is_last_use () const
{
  return m_last_nondebug_insn_use_or_next_use.is_first ();
}

inline use_info *
use_info::next_nondebug_insn_use () const
{
  if (m_is_last_nondebug_insn_use)
    return nullptr;
  return m_last_nondebug_insn_use_or_next_use.known_second ();
}

inline use_info *
use_info::next_any_insn_use () const
{
  // This is used less often than next_nondebug_insn_use, so it doesn't
  // seem worth having an m_is_last_nondebug_insn_use-style end marker.
  if (use_info *use = next_use ())
    if (use->is_in_any_insn ())
      return use;
  return nullptr;
}

inline use_info *
use_info::next_debug_insn_use () const
{
  if (auto use = next_use ())
    if (use->is_in_debug_insn ())
      return use;
  return nullptr;
}

inline use_info *
use_info::prev_phi_use () const
{
  // This is used less often than next_nondebug_insn_use, so it doesn't
  // seem worth having an m_is_last_nondebug_insn_use-style end marker.
  if (use_info *use = prev_use ())
    if (use->is_in_phi ())
      return use;
  return nullptr;
}

// Return the last use of any kind in the list.  Only valid when is_first ()
// is true.
inline use_info *
use_info::last_use () const
{
  return m_last_use_or_prev_use.known_first ();
}

// Return the last nondebug insn use in the list, or null if none.  Only valid
// when is_last_use () is true.
inline use_info *
use_info::last_nondebug_insn_use () const
{
  return m_last_nondebug_insn_use_or_next_use.known_first ();
}

inline def_info *
def_info::prev_def () const
{
  return m_last_def_or_prev_def.second_or_null ();
}

inline def_info *
def_info::next_def () const
{
  return m_splay_root_or_next_def.second_or_null ();
}

inline bool
def_info::is_first_def () const
{
  return m_last_def_or_prev_def.is_first ();
}

inline bool
def_info::is_last_def () const
{
  return m_splay_root_or_next_def.is_first ();
}

inline bb_info *
def_info::bb () const
{
  return m_insn->bb ();
}

inline ebb_info *
def_info::ebb () const
{
  return m_insn->ebb ();
}

inline clobber_group *
clobber_info::group () const
{
  if (!m_group || !m_group->has_been_superceded ())
    return m_group;
  return const_cast<clobber_info *> (this)->recompute_group ();
}

inline use_info *
set_info::last_use () const
{
  return m_first_use ? m_first_use->last_use () : nullptr;
}

inline use_info *
set_info::first_nondebug_insn_use () const
{
  if (m_is_set_with_nondebug_insn_uses)
    return m_first_use;
  return nullptr;
}

inline use_info *
set_info::last_nondebug_insn_use () const
{
  if (m_is_set_with_nondebug_insn_uses)
    return m_first_use->last_use ()->last_nondebug_insn_use ();
  return nullptr;
}

inline use_info *
set_info::first_debug_insn_use () const
{
  use_info *use;
  if (has_nondebug_insn_uses ())
    use = last_nondebug_insn_use ()->next_use ();
  else
    use = first_use ();

  if (use && use->is_in_debug_insn ())
    return use;
  return nullptr;
}

inline use_info *
set_info::first_any_insn_use () const
{
  if (m_first_use && m_first_use->is_in_any_insn ())
    return m_first_use;
  return nullptr;
}

inline use_info *
set_info::last_phi_use () const
{
  if (m_first_use)
    {
      use_info *last = m_first_use->last_use ();
      if (last->is_in_phi ())
	return last;
    }
  return nullptr;
}

inline bool
set_info::has_nondebug_uses () const
{
  return has_nondebug_insn_uses () || has_phi_uses ();
}

inline bool
set_info::has_nondebug_insn_uses () const
{
  return m_is_set_with_nondebug_insn_uses;
}

inline bool
set_info::has_phi_uses () const
{
  return m_first_use && m_first_use->last_use ()->is_in_phi ();
}

inline use_info *
set_info::single_nondebug_use () const
{
  if (!has_phi_uses ())
    return single_nondebug_insn_use ();
  if (!has_nondebug_insn_uses ())
    return single_phi_use ();
  return nullptr;
}

inline use_info *
set_info::single_nondebug_insn_use () const
{
  use_info *first = first_nondebug_insn_use ();
  if (first && !first->next_nondebug_insn_use ())
    return first;
  return nullptr;
}

inline use_info *
set_info::single_phi_use () const
{
  use_info *last = last_phi_use ();
  if (last && !last->prev_phi_use ())
    return last;
  return nullptr;
}

inline bool
set_info::is_local_to_ebb () const
{
  if (!m_first_use)
    return true;

  use_info *last = m_first_use->last_use ();
  if (last->is_in_phi ())
    return false;

  last = last->last_nondebug_insn_use ();
  return !last || last->ebb () == ebb ();
}

inline iterator_range<use_iterator>
set_info::all_uses () const
{
  return { m_first_use, nullptr };
}

inline iterator_range<reverse_use_iterator>
set_info::reverse_all_uses () const
{
  return { last_use (), nullptr };
}

inline iterator_range<nondebug_insn_use_iterator>
set_info::nondebug_insn_uses () const
{
  return { first_nondebug_insn_use (), nullptr };
}

inline iterator_range<debug_insn_use_iterator>
set_info::debug_insn_uses () const
{
  return { first_debug_insn_use (), nullptr };
}

inline iterator_range<reverse_use_iterator>
set_info::reverse_nondebug_insn_uses () const
{
  return { last_nondebug_insn_use (), nullptr };
}

inline iterator_range<any_insn_use_iterator>
set_info::all_insn_uses () const
{
  return { first_any_insn_use (), nullptr };
}

inline iterator_range<phi_use_iterator>
set_info::phi_uses () const
{
  return { last_phi_use (), nullptr };
}

inline use_array
phi_info::inputs () const
{
  if (m_num_inputs == 1)
    return use_array (&m_single_input, 1);
  return use_array (m_inputs, m_num_inputs);
}

inline use_info *
phi_info::input_use (unsigned int i) const
{
  if (m_num_inputs == 1)
    return as_a<use_info *> (m_single_input);
  return as_a<use_info *> (m_inputs[i]);
}

inline set_info *
phi_info::input_value (unsigned int i) const
{
  return input_use (i)->def ();
}

inline def_info *
def_node::first_def () const
{
  // This should get optimized into an AND with -2.
  if (m_clobber_or_set.is_first ())
    return m_clobber_or_set.known_first ();
  return m_clobber_or_set.known_second ();
}

inline clobber_info *
clobber_group::first_clobber () const
{
  return m_clobber_or_set.known_first ();
}

inline iterator_range<def_iterator>
clobber_group::clobbers () const
{
  return { first_clobber (), m_last_clobber->next_def () };
}

inline def_info *
def_mux::first_def () const
{
  if (is_first ())
    return known_first ();
  return known_second ()->first_def ();
}

inline def_info *
def_mux::last_def () const
{
  if (is_first ())
    return known_first ();

  def_node *node = known_second ();
  if (auto *clobber = ::dyn_cast<clobber_group *> (node))
    return clobber->last_clobber ();

  return node->first_def ();
}

inline set_info *
def_mux::set () const
{
  if (is_first ())
    return ::safe_dyn_cast<set_info *> (known_first ());
  return ::dyn_cast<set_info *> (known_second ()->first_def ());
}

inline def_info *
def_lookup::last_def_of_prev_group () const
{
  if (!mux)
    return nullptr;

  if (comparison > 0)
    return mux.last_def ();

  return mux.first_def ()->prev_def ();
}

inline def_info *
def_lookup::first_def_of_next_group () const
{
  if (!mux)
    return nullptr;

  if (comparison < 0)
    return mux.first_def ();

  return mux.last_def ()->next_def ();
}

inline set_info *
def_lookup::matching_set () const
{
  if (comparison == 0)
    return mux.set ();
  return nullptr;
}

inline def_info *
def_lookup::matching_set_or_last_def_of_prev_group () const
{
  if (set_info *set = matching_set ())
    return set;
  return last_def_of_prev_group ();
}

inline def_info *
def_lookup::matching_set_or_first_def_of_next_group () const
{
  if (set_info *set = matching_set ())
    return set;
  return first_def_of_next_group ();
}

inline insn_note::insn_note (insn_note_kind kind)
  : m_next_note (nullptr),
    m_kind (kind),
    m_data8 (0),
    m_data16 (0),
    m_data32 (0)
{
}

template<typename T>
inline T
insn_note::as_a ()
{
  using deref_type = decltype (*std::declval<T> ());
  using derived = typename std::remove_reference<deref_type>::type;
  gcc_checking_assert (m_kind == derived::kind);
  return static_cast<T> (this);
}

template<typename T>
inline T
insn_note::dyn_cast ()
{
  using deref_type = decltype (*std::declval<T> ());
  using derived = typename std::remove_reference<deref_type>::type;
  if (m_kind == derived::kind)
    return static_cast<T> (this);
  return nullptr;
}

inline bool
insn_info::operator< (const insn_info &other) const
{
  if (this == &other)
    return false;

  if (LIKELY (m_point != other.m_point))
    return m_point < other.m_point;

  return slow_compare_with (other) < 0;
}

inline bool
insn_info::operator> (const insn_info &other) const
{
  return other < *this;
}

inline bool
insn_info::operator<= (const insn_info &other) const
{
  return !(other < *this);
}

inline bool
insn_info::operator>= (const insn_info &other) const
{
  return !(*this < other);
}

inline int
insn_info::compare_with (const insn_info *other) const
{
  if (this == other)
    return 0;

  if (LIKELY (m_point != other->m_point))
    // Assume that points remain in [0, INT_MAX].
    return m_point - other->m_point;

  return slow_compare_with (*other);
}

inline insn_info *
insn_info::prev_nondebug_insn () const
{
  gcc_checking_assert (!is_debug_insn ());
  return m_prev_sametype_or_last_debug_insn.known_first ();
}

inline insn_info *
insn_info::next_nondebug_insn () const
{
  gcc_checking_assert (!is_debug_insn ());
  const insn_info *from = this;
  if (insn_info *first_debug = m_next_nondebug_or_debug_insn.second_or_null ())
    from = first_debug->last_debug_insn ();
  return from->m_next_nondebug_or_debug_insn.known_first ();
}

inline insn_info *
insn_info::prev_any_insn () const
{
  if (auto *last_debug = m_prev_sametype_or_last_debug_insn.second_or_null ())
    // This instruction is the first in a subsequence of debug instructions.
    // Move to the following nondebug instruction and get the previous one
    // from there.
    return (last_debug->m_next_nondebug_or_debug_insn.known_first ()
	    ->m_prev_sametype_or_last_debug_insn.known_first ());
  auto *prev = m_prev_sametype_or_last_debug_insn.known_first ();
  if (prev)
    {
      auto *next = prev->next_any_insn ();
      if (next != this)
	// This instruction is a non-debug instruction and there are some
	// debug instructions between it and PREV.  NEXT is the first of
	// the debug instructions; get the last.
	return next->m_prev_sametype_or_last_debug_insn.known_second ();
    }
  return prev;
}

inline insn_info *
insn_info::next_any_insn () const
{
  // This should get optimized into an AND with -2.
  if (m_next_nondebug_or_debug_insn.is_first ())
    return m_next_nondebug_or_debug_insn.known_first ();
  return m_next_nondebug_or_debug_insn.known_second ();
}

inline bool
insn_info::is_phi () const
{
  return this == ebb ()->phi_insn ();
}

inline bool
insn_info::is_bb_head () const
{
  return this == m_bb->head_insn ();
}

inline bool
insn_info::is_bb_end () const
{
  return this == m_bb->end_insn ();
}

inline ebb_info *
insn_info::ebb () const
{
  return m_bb->ebb ();
}

inline int
insn_info::uid () const
{
  return m_cost_or_uid < 0 ? m_cost_or_uid : INSN_UID (m_rtl);
}

inline use_array
insn_info::uses () const
{
  return use_array (m_accesses + m_num_defs, m_num_uses);
}

inline bool
insn_info::has_call_clobbers () const
{
  return find_note<insn_call_clobbers_note> ();
}

inline def_array
insn_info::defs () const
{
  return def_array (m_accesses, m_num_defs);
}

inline unsigned int
insn_info::cost () const
{
  if (m_cost_or_uid < 0)
    return 0;
  if (m_cost_or_uid == UNKNOWN_COST)
    calculate_cost ();
  return m_cost_or_uid;
}

template<typename T>
inline const T *
insn_info::find_note () const
{
  // We could break if the note kind is > T::kind, but since the number
  // of notes should be very small, the check is unlikely to pay for itself.
  for (const insn_note *note = first_note (); note; note = note->next_note ())
    if (note->kind () == T::kind)
      return static_cast<const T *> (note);
  return nullptr;
}

// Only valid for debug instructions that come after a nondebug instruction,
// and so start a subsequence of debug instructions.  Return the last debug
// instruction in the subsequence.
inline insn_info *
insn_info::last_debug_insn () const
{
  return m_prev_sametype_or_last_debug_insn.known_second ();
}

inline insn_range_info::insn_range_info (insn_info *first, insn_info *last)
  : first (first), last (last)
{
}

inline bool
insn_range_info::operator== (const insn_range_info &other) const
{
  return first == other.first && last == other.last;
}

inline bool
insn_range_info::operator!= (const insn_range_info &other) const
{
  return first != other.first || last != other.last;
}

inline insn_info *
insn_range_info::singleton () const
{
  return first == last ? last : nullptr;
}

inline bool
insn_range_info::includes (insn_info *insn) const
{
  return *insn >= *first && *insn <= *last;
}

inline insn_info *
insn_range_info::clamp_insn_to_range (insn_info *insn) const
{
  if (*first > *insn)
    return first;
  if (*last < *insn)
    return last;
  return insn;
}

inline bool
insn_range_info::is_subrange_of (const insn_range_info &other) const
{
  return *first >= *other.first && *last <= *other.last;
}

inline iterator_range<any_insn_iterator>
bb_info::all_insns () const
{
  return { m_head_insn, m_end_insn->next_any_insn () };
}

inline iterator_range<reverse_any_insn_iterator>
bb_info::reverse_all_insns () const
{
  return { m_end_insn, m_head_insn->prev_any_insn () };
}

inline iterator_range<nondebug_insn_iterator>
bb_info::nondebug_insns () const
{
  return { m_head_insn, m_end_insn->next_nondebug_insn () };
}

inline iterator_range<reverse_nondebug_insn_iterator>
bb_info::reverse_nondebug_insns () const
{
  return { m_end_insn, m_head_insn->prev_nondebug_insn () };
}

inline iterator_range<any_insn_iterator>
bb_info::real_insns () const
{
  return { m_head_insn->next_any_insn (), m_end_insn };
}

inline iterator_range<reverse_any_insn_iterator>
bb_info::reverse_real_insns () const
{
  return { m_end_insn->prev_any_insn (), m_head_insn };
}

inline iterator_range<nondebug_insn_iterator>
bb_info::real_nondebug_insns () const
{
  return { m_head_insn->next_nondebug_insn (), m_end_insn };
}

inline iterator_range<reverse_nondebug_insn_iterator>
bb_info::reverse_real_nondebug_insns () const
{
  return { m_end_insn->prev_nondebug_insn (), m_head_insn };
}

inline bool
ebb_call_clobbers_info::clobbers (resource_info resource) const
{
  // Only register clobbers are tracked this way.  Other clobbers are
  // recorded explicitly.
  return (resource.is_reg ()
	  && m_abi->clobbers_reg_p (resource.mode, resource.regno));
}

inline ebb_info *
ebb_info::prev_ebb () const
{
  if (bb_info *prev_bb = m_first_bb->prev_bb ())
    return prev_bb->ebb ();
  return nullptr;
}

inline ebb_info *
ebb_info::next_ebb () const
{
  if (bb_info *next_bb = m_last_bb->next_bb ())
    return next_bb->ebb ();
  return nullptr;
}

inline iterator_range<phi_iterator>
ebb_info::phis () const
{
  return { m_first_phi, nullptr };
}

inline iterator_range<bb_iterator>
ebb_info::bbs () const
{
  return { m_first_bb, m_last_bb->next_bb () };
}

inline iterator_range<reverse_bb_iterator>
ebb_info::reverse_bbs () const
{
  return { m_last_bb, m_first_bb->prev_bb () };
}

inline iterator_range<any_insn_iterator>
ebb_info::all_insns () const
{
  return { m_phi_insn, m_last_bb->end_insn ()->next_any_insn () };
}

inline iterator_range<reverse_any_insn_iterator>
ebb_info::reverse_all_insns () const
{
  return { m_last_bb->end_insn (), m_phi_insn->prev_any_insn () };
}

inline iterator_range<nondebug_insn_iterator>
ebb_info::nondebug_insns () const
{
  return { m_phi_insn, m_last_bb->end_insn ()->next_nondebug_insn () };
}

inline iterator_range<reverse_nondebug_insn_iterator>
ebb_info::reverse_nondebug_insns () const
{
  return { m_last_bb->end_insn (), m_phi_insn->prev_nondebug_insn () };
}

inline insn_range_info
ebb_info::insn_range () const
{
  return { m_phi_insn, m_last_bb->end_insn () };
}

inline void
ebb_info::set_first_call_clobbers (ebb_call_clobbers_info *call_clobbers)
{
  m_first_call_clobbers = call_clobbers;
}

inline ebb_call_clobbers_info *
ebb_info::first_call_clobbers () const
{
  return m_first_call_clobbers;
}

inline iterator_range<ebb_call_clobbers_iterator>
ebb_info::call_clobbers () const
{
  return { m_first_call_clobbers, nullptr };
}

inline insn_change::insn_change (insn_info *insn)
  : m_insn (insn),
    new_defs (insn->defs ()),
    new_uses (insn->uses ()),
    move_range (insn),
    new_cost (UNKNOWN_COST),
    m_is_deletion (false)
{
}

inline insn_change::insn_change (insn_info *insn, delete_action)
  : m_insn (insn),
    new_defs (),
    new_uses (),
    move_range (insn),
    new_cost (0),
    m_is_deletion (true)
{
}

inline iterator_range<bb_iterator>
function_info::bbs () const
{
  return { m_first_bb, nullptr };
}

inline iterator_range<reverse_bb_iterator>
function_info::reverse_bbs () const
{
  return { m_last_bb, nullptr };
}

inline iterator_range<ebb_iterator>
function_info::ebbs () const
{
  return { m_first_bb->ebb (), nullptr };
}

inline iterator_range<reverse_ebb_iterator>
function_info::reverse_ebbs () const
{
  return { m_last_bb->ebb (), nullptr };
}

inline iterator_range<any_insn_iterator>
function_info::all_insns () const
{
  return { m_first_insn, nullptr };
}

inline iterator_range<reverse_any_insn_iterator>
function_info::reverse_all_insns () const
{
  return { m_last_insn, nullptr };
}

inline iterator_range<nondebug_insn_iterator>
function_info::nondebug_insns () const
{
  return { m_first_insn, nullptr };
}

inline iterator_range<reverse_nondebug_insn_iterator>
function_info::reverse_nondebug_insns () const
{
  return { m_last_insn, nullptr };
}

inline iterator_range<def_iterator>
function_info::mem_defs () const
{
  return { m_defs[0], nullptr };
}

inline iterator_range<def_iterator>
function_info::reg_defs (unsigned int regno) const
{
  return { m_defs[regno + 1], nullptr };
}

inline bool
function_info::is_single_dominating_def (const set_info *set) const
{
  return (set->is_first_def ()
	  && set->is_last_def ()
	  && (!HARD_REGISTER_NUM_P (set->regno ())
	      || !TEST_HARD_REG_BIT (m_clobbered_by_calls, set->regno ())));
}

inline set_info *
function_info::single_dominating_def (unsigned int regno) const
{
  if (set_info *set = safe_dyn_cast<set_info *> (m_defs[regno + 1]))
    if (is_single_dominating_def (set))
      return set;
  return nullptr;
}

template<typename IgnorePredicates>
bool
function_info::add_regno_clobber (obstack_watermark &watermark,
				  insn_change &change, unsigned int regno,
				  IgnorePredicates ignore)
{
  // Check whether CHANGE already clobbers REGNO.
  if (find_access (change.new_defs, regno))
    return true;

  // Get the closest position to INSN at which the new instruction
  // could be placed.
  insn_info *insn = change.move_range.clamp_insn_to_range (change.insn ());
  def_array new_defs = insert_temp_clobber (watermark, insn, regno,
					    change.new_defs);
  if (!new_defs.is_valid ())
    return false;

  // Find a definition at or neighboring INSN.
  insn_range_info move_range = change.move_range;
  if (!restrict_movement_for_dead_range (move_range, regno, insn, ignore))
    return false;

  change.new_defs = new_defs;
  change.move_range = move_range;
  return true;
}

template<typename T, typename... Ts>
inline T *
function_info::change_alloc (obstack_watermark &wm, Ts... args)
{
  static_assert (std::is_trivially_destructible<T>::value,
		 "destructor won't be called");
  static_assert (alignof (T) <= obstack_alignment,
		 "too much alignment required");
  void *addr = XOBNEW (wm, T);
  return new (addr) T (std::forward<Ts> (args)...);
}

inline
ignore_changing_insns::
ignore_changing_insns (array_slice<insn_change *const> changes)
  : m_changes (changes)
{
}

inline bool
ignore_changing_insns::should_ignore_insn (const insn_info *insn)
{
  for (const insn_change *change : m_changes)
    if (change->insn () == insn)
      return true;
  return false;
}

}
