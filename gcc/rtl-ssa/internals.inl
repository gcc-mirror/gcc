// Implementation of private inline member functions for RTL SSA    -*- C++ -*-
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

// Construct a new access with the given resource () and kind () values.
inline access_info::access_info (resource_info resource, access_kind kind)
  : m_regno (resource.regno),
    m_kind (kind),
    m_is_artificial (false),
    m_is_set_with_nondebug_insn_uses (false),
    m_is_pre_post_modify (false),
    m_is_call_clobber (false),
    m_is_live_out_use (false),
    m_includes_address_uses (false),
    m_includes_read_writes (false),
    m_includes_subregs (false),
    m_includes_multiregs (false),
    m_only_occurs_in_notes (false),
    m_is_last_nondebug_insn_use (false),
    m_is_in_debug_insn_or_phi (false),
    m_has_been_superceded (false),
    m_is_temp (false),
    m_spare (0),
    m_mode (resource.mode)
{
}

// Construct a use of RESOURCE in LOCATION.  The resource's value is provided
// by DEF, or is completely undefined if DEF is null.
inline use_info::use_info (insn_or_phi location, resource_info resource,
			   set_info *definition)
  : access_info (resource, access_kind::USE),
    m_insn_or_phi (location),
    m_last_use_or_prev_use (nullptr),
    m_last_nondebug_insn_use_or_next_use (nullptr),
    m_def (definition)
{
  if (m_insn_or_phi.is_second ())
    {
      m_is_in_debug_insn_or_phi = true;
      m_is_artificial = true;
    }
  else
    {
      insn_info *insn = m_insn_or_phi.known_first ();
      m_is_in_debug_insn_or_phi = insn->is_debug_insn ();
      m_is_artificial = insn->is_artificial ();
    }
}

// Return the correct (uncached) value of m_is_last_nondebug_insn_use.
inline bool
use_info::calculate_is_last_nondebug_insn_use () const
{
  use_info *next = next_use ();
  return is_in_nondebug_insn () && (!next || next->is_in_debug_insn_or_phi ());
}

// Accumulate any properties about REF that are also stored in use_infos.
// IS_FIRST is true if REF is the first access to resource () that we have
// recorded in this way, false if we have already recorded previous
// references.
inline void
use_info::record_reference (rtx_obj_reference ref, bool is_first)
{
  if (is_first)
    {
      m_includes_address_uses = ref.in_address ();
      m_includes_read_writes = ref.is_write ();
      m_includes_subregs = ref.in_subreg ();
      m_includes_multiregs = ref.is_multireg ();
      m_only_occurs_in_notes = ref.in_note ();
    }
  else
    {
      m_includes_address_uses |= ref.in_address ();
      m_includes_read_writes |= ref.is_write ();
      m_includes_subregs |= ref.in_subreg ();
      m_includes_multiregs |= ref.is_multireg ();
      m_only_occurs_in_notes &= ref.in_note ();
    }
}

// Change the value of insn () to INSN.
inline void
use_info::set_insn (insn_info *insn)
{
  m_insn_or_phi = insn;
  m_is_artificial = insn->is_artificial ();
}

// Copy the overloaded prev link from OTHER.
inline void
use_info::copy_prev_from (use_info *other)
{
  m_last_use_or_prev_use = other->m_last_use_or_prev_use;
}

// Copy the overloaded next link from OTHER.
inline void
use_info::copy_next_from (use_info *other)
{
  m_last_nondebug_insn_use_or_next_use
    = other->m_last_nondebug_insn_use_or_next_use;
  m_is_last_nondebug_insn_use = calculate_is_last_nondebug_insn_use ();
}

// Record that this use is the first in the list and that the last use is LAST.
inline void
use_info::set_last_use (use_info *last_use)
{
  m_last_use_or_prev_use.set_first (last_use);
}

// Record that this use is not the first in the list and that the previous
// use is PREV.
inline void
use_info::set_prev_use (use_info *prev_use)
{
  m_last_use_or_prev_use.set_second (prev_use);
}

// Record that this use is the last use in the list.  If USE is nonnull,
// record that USE is the last use in the list by a nondebug instruction,
// otherwise record that there are no uses by nondebug instructions
// in the list.
inline void
use_info::set_last_nondebug_insn_use (use_info *use)
{
  m_last_nondebug_insn_use_or_next_use.set_first (use);
  m_is_last_nondebug_insn_use = (use == this);
}

// Record that this use is not the last in the list and that the next
// use is NEXT_USE.
inline void
use_info::set_next_use (use_info *next_use)
{
  m_last_nondebug_insn_use_or_next_use.set_second (next_use);
  m_is_last_nondebug_insn_use = calculate_is_last_nondebug_insn_use ();
}

// Clear any information relating to the position of the use in its
// definition's list.
inline void
use_info::clear_use_links ()
{
  m_last_use_or_prev_use = nullptr;
  m_last_nondebug_insn_use_or_next_use = nullptr;
  m_is_last_nondebug_insn_use = false;
}

// Return true if the use has any links to other uses.  This is mostly
// for assert checking.
inline bool
use_info::has_use_links ()
{
  return (m_last_use_or_prev_use
	  || m_last_nondebug_insn_use_or_next_use
	  || m_is_last_nondebug_insn_use);
}

// Construct a definition of RESOURCE in INSN, giving it kind KIND.
inline def_info::def_info (insn_info *insn, resource_info resource,
			   access_kind kind)
  : access_info (resource, kind),
    m_insn (insn),
    m_last_def_or_prev_def (nullptr),
    m_splay_root_or_next_def (nullptr)
{
  m_is_artificial = insn->is_artificial ();
}

// Record any properties about REF that are also stored in def_infos.
// IS_FIRST is true if REF is the first access to resource () that we have
// recorded in this way, false if we have already recorded previous
// references.
inline void
def_info::record_reference (rtx_obj_reference ref, bool is_first)
{
  if (is_first)
    {
      m_is_pre_post_modify = ref.is_pre_post_modify ();
      m_includes_read_writes = ref.is_read ();
      m_includes_subregs = ref.in_subreg ();
      m_includes_multiregs = ref.is_multireg ();
    }
  else
    {
      m_is_pre_post_modify |= ref.is_pre_post_modify ();
      m_includes_read_writes |= ref.is_read ();
      m_includes_subregs |= ref.in_subreg ();
      m_includes_multiregs |= ref.is_multireg ();
    }
}

// Return the last definition in the list.  Only valid when is_first ()
// is true.
inline def_info *
def_info::last_def () const
{
  return m_last_def_or_prev_def.known_first ();
}

// Return the root of the splay tree of definitions of resource (),
// or null if no splay tree has been created for this resource.
// Only valid when is_last () is true.
inline def_node *
def_info::splay_root () const
{
  return m_splay_root_or_next_def.known_first ();
}

// Copy the overloaded prev link from OTHER.
inline void
def_info::copy_prev_from (def_info *other)
{
  m_last_def_or_prev_def
    = other->m_last_def_or_prev_def;
}

// Copy the overloaded next link from OTHER.
inline void
def_info::copy_next_from (def_info *other)
{
  m_splay_root_or_next_def = other->m_splay_root_or_next_def;
}

// Record that this definition is the first in the list and that the last
// definition is LAST.
inline void
def_info::set_last_def (def_info *last_def)
{
  m_last_def_or_prev_def.set_first (last_def);
}

// Record that this definition is not the first in the list and that the
// previous definition is PREV.
inline void
def_info::set_prev_def (def_info *prev_def)
{
  m_last_def_or_prev_def.set_second (prev_def);
}

// Record that this definition is the last in the list and that the root
// of the splay tree associated with resource () is ROOT.
inline void
def_info::set_splay_root (def_node *root)
{
  m_splay_root_or_next_def = root;
}

// Record that this definition is not the last in the list and that the
// next definition is NEXT.
inline void
def_info::set_next_def (def_info *next_def)
{
  m_splay_root_or_next_def = next_def;
}

// Clear the prev and next links
inline void
def_info::clear_def_links ()
{
  m_last_def_or_prev_def = nullptr;
  m_splay_root_or_next_def = nullptr;
}

// Return true if the definition has any links to other definitions.
// This is mostly for assert checking.
inline bool
def_info::has_def_links ()
{
  return m_last_def_or_prev_def || m_splay_root_or_next_def;
}

// Construct a clobber of register REGNO in insn INSN.
inline clobber_info::clobber_info (insn_info *insn, unsigned int regno)
  : def_info (insn, { E_BLKmode, regno }, access_kind::CLOBBER),
    m_children (),
    m_parent (nullptr),
    m_group (nullptr)
{
}

// Set the containing group to GROUP, if it isn't already.  The main
// use of this function is to update the new root of GROUP's splay tree.
inline void
clobber_info::update_group (clobber_group *group)
{
  if (__builtin_expect (m_group != group, 0))
    m_group = group;
}

// Cconstruct a set_info for a store to RESOURCE in INSN, giving it
// kind KIND.
inline set_info::set_info (insn_info *insn, resource_info resource,
			   access_kind kind)
  : def_info (insn, resource, kind),
    m_first_use (nullptr)
{
}

// Cconstruct a set_info for a store to RESOURCE in INSN.
inline set_info::set_info (insn_info *insn, resource_info resource)
  : set_info (insn, resource, access_kind::SET)
{
}

// Record that USE is the first use of this definition.
inline void
set_info::set_first_use (use_info *first_use)
{
  m_first_use = first_use;
  m_is_set_with_nondebug_insn_uses
    = (first_use && first_use->is_in_nondebug_insn ());
}

// Construct a phi for RESOURCE in INSN, giving it identifier UID.
inline phi_info::phi_info (insn_info *insn, resource_info resource,
			   unsigned int uid)
  : set_info (insn, resource, access_kind::PHI),
    m_uid (uid),
    m_num_inputs (0),
    m_prev_phi (nullptr),
    m_next_phi (nullptr)
{
}

// Turn the phi into a degenerate phi, with INPUT representing the
// value of the resource on all incoming edges.
inline void
phi_info::make_degenerate (use_info *input)
{
  m_num_inputs = 1;
  m_single_input = input;
}

// Set the inputs of the phi to INPUTS.
inline void
phi_info::set_inputs (use_array inputs)
{
  m_num_inputs = inputs.size ();
  if (inputs.size () == 1)
    m_single_input = inputs[0];
  else
    m_inputs = access_array (inputs).begin ();
}

// Construct a definition splay tree node for FIRST_DEF, which is either
// the first clobber_info in a group or a standalone set_info.
inline def_node::def_node (clobber_or_set first_def)
  : m_clobber_or_set (first_def),
    m_children ()
{
}

// Construct a new group of clobber_infos that initially contains just CLOBBER.
inline clobber_group::clobber_group (clobber_info *clobber)
  : def_node (clobber),
    m_last_clobber (clobber),
    m_clobber_tree (clobber)
{
  clobber->m_group = this;
}

// Construct a node for the instruction with uid UID.
inline insn_info::order_node::order_node (int uid)
  : insn_note (kind),
    m_children (),
    m_parent (nullptr)
{
  m_data32 = uid;
}

// Construct a note for instruction INSN, giving it abi_id () value ABI_ID.
inline insn_call_clobbers_note::insn_call_clobbers_note (unsigned int abi_id,
							 insn_info *insn)
  : insn_note (kind),
    m_children (),
    m_insn (insn)
{
  m_data32 = abi_id;
}

// Construct an instruction with the given bb () and rtl () values.
// If the instruction is real, COST_OR_UID is the value of cost (),
// otherwise it is the value of uid ().
inline insn_info::insn_info (bb_info *bb, rtx_insn *rtl, int cost_or_uid)
  : m_prev_insn_or_last_debug_insn (nullptr),
    m_next_nondebug_or_debug_insn (nullptr),
    m_bb (bb),
    m_rtl (rtl),
    m_accesses (nullptr),
    m_num_uses (0),
    m_num_defs (0),
    m_is_debug_insn (rtl && DEBUG_INSN_P (rtl)),
    m_can_be_optimized (false),
    m_is_asm (false),
    m_has_pre_post_modify (false),
    m_has_volatile_refs (false),
    m_spare (0),
    m_point (0),
    m_cost_or_uid (cost_or_uid),
    m_first_note (nullptr)
{
}

// Copy any insn properties from PROPERTIES that are also stored in an
// insn_info.
inline void
insn_info::set_properties (const rtx_properties &properties)
{
  m_is_asm = properties.has_asm;
  m_has_pre_post_modify = properties.has_pre_post_modify;
  m_has_volatile_refs = properties.has_volatile_refs;
  // Not strictly related to the properties we've been given, but it's
  // a convenient location to do this.
  m_can_be_optimized = (NONDEBUG_INSN_P (m_rtl)
			& (GET_CODE (PATTERN (m_rtl)) != USE)
			& (GET_CODE (PATTERN (m_rtl)) != CLOBBER));
}

// Change the list of instruction accesses to ACCESSES, which contains
// NUM_DEFS definitions followed by NUM_USES uses.
inline void
insn_info::set_accesses (access_info **accesses,
			 unsigned int num_defs, unsigned int num_uses)
{
  m_accesses = accesses;
  m_num_defs = num_defs;
  gcc_assert (num_defs == m_num_defs);
  m_num_uses = num_uses;
}

// Change defs () and uses () to DEFS and USES respectively, given that
// the existing m_accesses array has enough room for them.
inline void
insn_info::copy_accesses (access_array defs, access_array uses)
{
  gcc_assert (defs.size () + uses.size () <= m_num_defs + m_num_uses);
  memcpy (m_accesses, defs.begin (), defs.size_bytes ());
  memcpy (m_accesses + defs.size (), uses.begin (), uses.size_bytes ());
  m_num_defs = defs.size ();
  gcc_assert (m_num_defs == defs.size ());
  m_num_uses = uses.size ();
}

// If the instruction has an insn_info::order_node, return the node,
// otherwise return null.
inline insn_info::order_node *
insn_info::get_order_node () const
{
  // The order_node always comes first.
  if (insn_note *note = first_note ())
    return note->dyn_cast<insn_info::order_node *> ();
  return nullptr;
}

// Like get_order_node (), but the node is known to exist.
inline insn_info::order_node *
insn_info::get_known_order_node () const
{
  // The order_node always comes first.
  return first_note ()->as_a<insn_info::order_node *> ();
}

// Copy the overloaded prev link from OTHER.
inline void
insn_info::copy_prev_from (insn_info *other)
{
  m_prev_insn_or_last_debug_insn = other->m_prev_insn_or_last_debug_insn;
}

// Copy the overloaded next link from OTHER.
inline void
insn_info::copy_next_from (insn_info *other)
{
  m_next_nondebug_or_debug_insn = other->m_next_nondebug_or_debug_insn;
}

// If this is a nondebug instruction, record that the previous nondebug
// instruction is PREV.  (There might be intervening debug instructions.)
//
// If this is a debug instruction, record that the previous instruction
// is debug instruction PREV.
inline void
insn_info::set_prev_sametype_insn (insn_info *prev)
{
  m_prev_insn_or_last_debug_insn.set_first (prev);
}

// Only valid for debug instructions.  Record that this instruction starts
// a subsequence of debug instructions that ends with LAST.
inline void
insn_info::set_last_debug_insn (insn_info *last)
{
  m_prev_insn_or_last_debug_insn.set_second (last);
}

// Record that the next instruction of any kind is NEXT.
inline void
insn_info::set_next_any_insn (insn_info *next)
{
  if (next && next->is_debug_insn ())
    m_next_nondebug_or_debug_insn.set_second (next);
  else
    m_next_nondebug_or_debug_insn.set_first (next);
}

// Clear the list links and point number for this instruction.
inline void
insn_info::clear_insn_links ()
{
  m_prev_insn_or_last_debug_insn = nullptr;
  m_next_nondebug_or_debug_insn = nullptr;
  m_point = 0;
}

// Return true if the instruction contains any list information.
// This is used by assert checking.
inline bool
insn_info::has_insn_links ()
{
  return (m_prev_insn_or_last_debug_insn
	  || m_next_nondebug_or_debug_insn
	  || m_point);
}

// Construct a representation of basic block CFG_BB.
inline bb_info::bb_info (basic_block cfg_bb)
  : m_prev_bb (nullptr),
    m_next_bb (nullptr),
    m_cfg_bb (cfg_bb),
    m_ebb (nullptr),
    m_head_insn (nullptr),
    m_end_insn (nullptr)
{
}

// Construct a tree of call clobbers for the given ABI.
inline ebb_call_clobbers_info::
ebb_call_clobbers_info (const predefined_function_abi *abi)
  : m_next (nullptr),
    m_abi (abi)
{
}

// Construct an EBB whose first block is FIRST_BB and whose last block
// is LAST_BB.
inline ebb_info::ebb_info (bb_info *first_bb, bb_info *last_bb)
  : m_first_phi (nullptr),
    m_phi_insn (nullptr),
    m_first_bb (first_bb),
    m_last_bb (last_bb),
    m_first_call_clobbers (nullptr)
{
}

// Record register definition DEF in last_access, pushing a definition
// to def_stack where appropriate.
inline void
function_info::build_info::record_reg_def (def_info *def)
{
  unsigned int regno = def->regno ();
  auto *prev_dominating_def = safe_as_a<def_info *> (last_access[regno + 1]);
  if (!prev_dominating_def)
    // Indicate that DEF is the first dominating definition of REGNO.
    def_stack.safe_push (def);
  else if (prev_dominating_def->bb () != def->bb ())
    // Record that PREV_DOMINATING_DEF was the dominating definition
    // of REGNO on entry to the current block.
    def_stack.safe_push (prev_dominating_def);
  last_access[regno + 1] = def;
}

// Set the contents of last_access for memory to DEF.
inline void
function_info::build_info::record_mem_def (def_info *def)
{
  last_access[0] = def;
}

// Return the current value of live register REGNO, or null if the register's
// value is completedly undefined.
inline set_info *
function_info::build_info::current_reg_value (unsigned int regno) const
{
  return safe_dyn_cast<set_info *> (last_access[regno + 1]);
}

// Return the current value of memory.
inline set_info *
function_info::build_info::current_mem_value () const
{
  return as_a<set_info *> (last_access[0]);
}

// Allocate a T on the function's main obstack, passing ARGS
// to its constructor.
template<typename T, typename... Ts>
inline T *
function_info::allocate (Ts... args)
{
  static_assert (std::is_trivially_destructible<T>::value,
		 "destructor won't be called");
  static_assert (alignof (T) <= obstack_alignment,
		 "too much alignment required");
  void *addr = obstack_alloc (&m_obstack, sizeof (T));
  return new (addr) T (std::forward<Ts> (args)...);
}

// Allocate a T on the function's temporary obstack, passing ARGS
// to its constructor.
template<typename T, typename... Ts>
inline T *
function_info::allocate_temp (Ts... args)
{
  static_assert (std::is_trivially_destructible<T>::value,
		 "destructor won't be called");
  static_assert (alignof (T) <= obstack_alignment,
		 "too much alignment required");
  void *addr = obstack_alloc (&m_temp_obstack, sizeof (T));
  return new (addr) T (std::forward<Ts> (args)...);
}

// Add INSN to the end of the function's list of instructions.
inline void
function_info::append_insn (insn_info *insn)
{
  gcc_checking_assert (!insn->has_insn_links ());
  if (insn_info *after = m_last_insn)
    add_insn_after (insn, after);
  else
    // The first instruction is for the entry block and is always a nondebug
    // insn
    m_first_insn = m_last_insn = m_last_nondebug_insn = insn;
}

// Start building a new list of uses and definitions for an instruction.
inline void
function_info::start_insn_accesses ()
{
  gcc_checking_assert (m_temp_defs.is_empty ()
		       && m_temp_uses.is_empty ());
}

// Return a mode that encapsulates two distinct references to a register,
// one with mode MODE1 and one with mode MODE2.  Treat BLKmode as a
// "don't know" wildcard.
inline machine_mode
combine_modes (machine_mode mode1, machine_mode mode2)
{
  if (mode1 == E_BLKmode)
    return mode2;

  if (mode2 == E_BLKmode)
    return mode1;

  return wider_subreg_mode (mode1, mode2);
}

// PRINTER (PP, ARGS...) prints ARGS... to a pretty_printer PP.  Use it
// to print ARGS... to FILE.
template<typename Printer, typename... Args>
inline void
dump_using (FILE *file, Printer printer, Args... args)
{
  pretty_printer pp;
  printer (&pp, args...);
  pp_newline (&pp);
  fprintf (file, "%s", pp_formatted_text (&pp));
}

}
