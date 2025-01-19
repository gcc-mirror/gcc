// Access-related classes for RTL SSA                               -*- C++ -*-
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

namespace rtl_ssa {

// Forward declarations.
class bb_info;
class clobber_group;
class def_node;
class ebb_info;
class insn_info;
class phi_info;
class set_info;

// Used as a boolean argunent to certain routines.
enum class ignore_clobbers { NO, YES };

// Represents something that the SSA form tracks: either a register
// or memory.
class resource_info
{
public:
  // Return true if this resource represents memory.
  bool is_mem () const { return regno == MEM_REGNO; }

  // Return true if this resource represents a register.
  bool is_reg () const { return regno != MEM_REGNO; }

  // Print the name of the resource to PP.
  void print_identifier (pretty_printer *pp) const;

  // Possibly print additional information about the resource to PP.
  void print_context (pretty_printer *pp) const;

  // A combination of print_identifier and print_context.
  void print (pretty_printer *pp) const;

  // The mode with which the resource is being defined or used.  This is
  // always BLKmode for memory.  It can also be BLKmode for registers if
  // we don't yet know the real mode, or if the mode is not relevant for
  // some reason.
  machine_mode mode;

  // The pseudo register or single hard register that the resource represents,
  // or MEM_REGNO for memory.
  unsigned int regno;
};

// For simplicity, we treat memory as a single unified entity.
const resource_info memory = { E_BLKmode, MEM_REGNO };

// Flags used when printing access_infos.
//
// Print the location at which the access occurs.  This is redundant
// when the access is being printed as part of the instruction or phi node
// that contains the access.
const unsigned int PP_ACCESS_INCLUDE_LOCATION = 1U << 0;
//
// Print links to other accesses: the definition that defines a use,
// the uses of a definition, and the inputs of a phi node.
const unsigned int PP_ACCESS_INCLUDE_LINKS = 1U << 1;
//
// Print additional properties about the access.
const unsigned int PP_ACCESS_INCLUDE_PROPERTIES = 1U << 2;
//
// The usual flags when printing an access in isolation.
const unsigned int PP_ACCESS_DEFAULT = (PP_ACCESS_INCLUDE_LOCATION
					| PP_ACCESS_INCLUDE_LINKS
					| PP_ACCESS_INCLUDE_PROPERTIES);
//
// The usual flags when printing a def_info from its defining instruction.
const unsigned int PP_ACCESS_SETTER = (PP_ACCESS_INCLUDE_LINKS
				       | PP_ACCESS_INCLUDE_PROPERTIES);
//
// The usual flags when printing a use_info from its user.
const unsigned int PP_ACCESS_USER = PP_ACCESS_INCLUDE_PROPERTIES;

// The various ways of accessing a resource.  The two range checks that
// we need to perform are [SET, PHI] (for set_info) and [SET, CLOBBER]
// (for def_info), so the ordering tries to make those tests as
// efficient as possible.
enum class access_kind : uint8_t
{
  // Set the resource to a useful value.
  SET,

  // A form of SET that collects the possible incoming values of the
  // resource using a phi node; the resource does not actually change value.
  PHI,

  // Set the resource to a value that is both unknown and not useful.
  CLOBBER,

  // Use the current value of the resource.
  USE
};

// A base class that represents an access to a resource.
class access_info
{
  // Size: 1 LP64 word
  friend class function_info;

public:
  // Return the resource that is being accessed.
  resource_info resource () const { return { m_mode, m_regno }; }

  // Return true if the access is to memory.
  bool is_mem () const { return m_regno == MEM_REGNO; }

  // Return true if the access is to a register.
  bool is_reg () const { return m_regno != MEM_REGNO; }

  // If the access is to a register, return the register number,
  // otherwise return MEM_REGNO.
  unsigned int regno () const { return m_regno; }

  // For sets, return the mode of the value to which the resource is being set.
  // For uses, return the mode in which the resource is being used (which for
  // hard registers might be different from the mode in which the resource
  // was set).
  //
  // When accessing memory, the mode is always BLKmode.  When accessing
  // pseudo registers, the mode is always the mode of the pseudo register
  // (and so doesn't, for example, take subregs into account).
  machine_mode mode () const { return m_mode; }

  // Return the kind of access that this is.
  access_kind kind () const { return m_kind; }

  // Return true if the access occurs in a phi node or an "artificial"
  // instruction (see insn_info), false if it occurs in a real instruction.
  bool is_artificial () const { return m_is_artificial; }

  // Return the opposite of is_artificial.
  bool is_real () const { return !m_is_artificial; }

  // Return true if this access is a set_info whose result is used by at least
  // one nondebug instruction.
  bool is_set_with_nondebug_insn_uses () const;

  // Return true if the access describes a set_info and if the value
  // is defined by an RTX_AUTOINC rtx.
  bool is_pre_post_modify () const { return m_is_pre_post_modify; }

  // Return true if the access is a clobber_info that describes the effect
  // of a called function.  This kind of clobber is added for -fipa-ra
  // functions that clobber only a strict subset of the normal ABI set.
  bool is_call_clobber () const { return m_is_call_clobber; }

  // Return true if the access is a use_info that simply marks a point in
  // the live range of a set_info at which the value is live out from
  // the containing EBB.
  bool is_live_out_use () const { return m_is_live_out_use; }

  // Return true if the access is a use_info for an instruction and if
  // at least some of the uses occur within a MEM address.
  //
  // There shouldn't be a need to check whether *all* uses occur within
  // a MEM address, since in principle:
  //
  // A: (set (reg:SI R1) (mem:SI (post_inc:SI (reg:SI R2))))
  //
  // should be semantically equivalent to:
  //
  // B: (parallel [(set (reg:SI R1) (mem:SI (reg:SI R2)))
  //               (set (reg:SI R2) (plus:SI (reg:SI R2) (const_int 4)))])
  //
  // even though R2 occurs only in MEMs for A but occurs outside MEMs for B.
  bool includes_address_uses () const { return m_includes_address_uses; }

  // Return true if the access occurs in an instruction and if at least
  // some accesses to resource () occur in a read-modify-write context.
  // This is equivalent to the DF_REF_READ_WRITE flag.
  bool includes_read_writes () const { return m_includes_read_writes; }

  // Return true if the access occurs in an instruction and if at least
  // some accesses to resource () occur in a subreg context.
  bool includes_subregs () const { return m_includes_subregs; }

  // Return true if the access occurs in an instruction and if at least
  // some accesses to resource () occur in a multi-register REG.
  // This implies that resource () is a hard register.
  bool includes_multiregs () const { return m_includes_multiregs; }

  // Return true if the access occurs in a real nondebug instruction
  // and if all accesses to resource () occur in notes, rather than
  // in the main instruction pattern.
  bool only_occurs_in_notes () const { return m_only_occurs_in_notes; }

  // Return true if this is a temporary access, e.g. one created for
  // an insn that is about to be inserted.
  bool is_temporary () const { return m_is_temp; }

protected:
  access_info (resource_info, access_kind);

  void print_prefix_flags (pretty_printer *) const;
  void print_properties_on_new_lines (pretty_printer *) const;

private:
  void set_mode (machine_mode mode) { m_mode = mode; }

  // The values returned by the accessors above.
  unsigned int m_regno;
  machine_mode m_mode : MACHINE_MODE_BITSIZE;
  access_kind m_kind : 2;

protected:
  // The value returned by the accessors above.
  unsigned int m_is_artificial : 1;
  unsigned int m_is_set_with_nondebug_insn_uses : 1;
  unsigned int m_is_pre_post_modify : 1;
  unsigned int m_is_call_clobber : 1;
  unsigned int m_is_live_out_use : 1;
  unsigned int m_includes_address_uses : 1;
  unsigned int m_includes_read_writes : 1;
  unsigned int m_includes_subregs : 1;
  unsigned int m_includes_multiregs : 1;
  unsigned int m_only_occurs_in_notes : 1;

  // True if this access is a use_insn that occurs in a nondebug instruction,
  // and if there are no following uses by nondebug instructions.  The next use
  // is null, a use_info for a debug instruction, or a use_info for a phi node.
  //
  // Providing this helps to optimize use_info::next_nondebug_insn_use.
  unsigned int m_is_last_nondebug_insn_use : 1;

  // True if this access is a use_info for a debug instruction or
  // a phi node.
  unsigned int m_is_in_debug_insn_or_phi : 1;

private:
  // Used as a flag during various update routines; has no long-lasting
  // meaning.
  unsigned int m_has_been_superceded : 1;

  // Indicates that this access has been allocated on the function_info's
  // temporary obstack and so is not (yet) part of the proper SSA form.
  unsigned int m_is_temp : 1;
};

// A contiguous array of access_info pointers.  Used to represent a
// (mostly small) number of definitions and/or uses.
using access_array = array_slice<access_info *const>;

// A class for building an access_array on an obstack.  It automatically
// frees any in-progress array if the build attempt fails before finish ()
// has been called.
class access_array_builder : public obstack_watermark
{
public:
  using obstack_watermark::obstack_watermark;

  // Make sure that the array has enough for NUM_ACCESSES accesses.
  void reserve (unsigned int num_accesses);

  // Add ACCESS to the end of the array that we're building, given that
  // reserve () has already made room.
  void quick_push (access_info *access);

  // Finish and return the new array.  The array survives the destruction
  // of the builder.
  array_slice<access_info *> finish ();
};

// An access_info that represents the use of a resource in either a phi node
// or an instruction.  It records which set_info (if any) provides the
// resource's value.
class use_info : public access_info
{
  // Overall size: 5 LP64 words.
  friend class set_info;
  friend class function_info;

public:
  // Return true if the access occurs in an instruction rather than a phi node.
  // The instruction might be a debug instruction or a nondebug instruction.
  bool is_in_any_insn () const { return m_insn_or_phi.is_first (); }

  // Return true if the access occurs in a nondebug instruction,
  // false if it occurs in a debug instruction or a phi node.
  bool is_in_nondebug_insn () const { return !m_is_in_debug_insn_or_phi; }

  // Return true if the instruction occurs in a debug instruction.
  bool is_in_debug_insn () const;

  // Return true if the access occurs in a phi node rather than in an
  // instruction.
  bool is_in_phi () const { return m_insn_or_phi.is_second (); }

  // Return true if the access occurs in a debug instruction or a phi node,
  // false if it occurs in a nondebug instruction.
  bool is_in_debug_insn_or_phi () const { return m_is_in_debug_insn_or_phi; }

  // Return the instruction that uses the resource.  Only valid is
  // is_in_any_insn ().
  insn_info *insn () const { return m_insn_or_phi.known_first (); }

  // Return the phi node that uses the resource.  Only valid if is_in_phi ().
  phi_info *phi () const { return m_insn_or_phi.known_second (); }

  // Return the basic block that contains the access.
  bb_info *bb () const;

  // Return the extended basic block that contains the access.
  ebb_info *ebb () const;

  // Return the set_info whose result the access uses, or null if the
  // value of the resource is completely undefined.
  //
  // The value is undefined if the use is completely upwards exposed
  // (i.e. has no preceding definition) or if the preceding definition
  // is a clobber rather than a set.
  //
  // The mode of the definition can be different from the mode of the use;
  // for example, a hard register might be set in DImode and used in SImode.
  set_info *def () const { return m_def; }

  // Return the previous and next uses of the definition.  See set_info
  // for details about the ordering.
  //
  // These routines are only meaningful when def () is nonnull.
  use_info *prev_use () const;
  use_info *next_use () const;

  // Return the next use by a nondebug instruction, or null if none.
  //
  // This is only valid if is_in_nondebug_insn ().  It is equivalent to,
  // but more efficient than:
  //
  //    next_use () && next_use ()->is_in_nondebug_insn ()
  //    ? next_use () : nullptr
  use_info *next_nondebug_insn_use () const;

  // Return the next use by an instruction, or null if none.  The use might
  // be by a debug instruction or a nondebug instruction.
  //
  // This is only valid if is_in_any_insn ().  It is equivalent to:
  //
  //    next_use () && next_use ()->is_in_any_insn () ? next_use () : nullptr
  use_info *next_any_insn_use () const;

  // Return the next use by a debug instruction, or null if none.
  // This is only valid if is_in_debug_insn ().
  use_info *next_debug_insn_use () const;

  // Return the previous use by a phi node in the list, or null if none.
  //
  // This is only valid if is_in_phi ().  It is equivalent to:
  //
  //    prev_use () && prev_use ()->is_in_phi () ? prev_use () : nullptr
  use_info *prev_phi_use () const;

  // Return true if this is the first use of the definition.  See set_info
  // for details about the ordering.
  //
  // This routine is only meaningful when def () is nonnull.
  bool is_first_use () const;

  // Return true if this is the last use of the definition.  See set_info
  // for details about the ordering.
  //
  // This routine is only meaningful when def () is nonnull.
  bool is_last_use () const;

  // Print a description of def () to PP.
  void print_def (pretty_printer *pp) const;

  // Print a description of the location of the use to PP.
  void print_location (pretty_printer *pp) const;

  // Print a description of the use to PP under the control of
  // PP_ACCESS_* flags FLAGS.
  void print (pretty_printer *pp,
	      unsigned int flags = PP_ACCESS_DEFAULT) const;

private:
  // If we only create a set_info splay tree for sets that are used by
  // three instructions or more, then only about 16% of uses need to be in
  // a splay tree.  It is therefore more memory-efficient to use separate
  // nodes for the splay tree, instead of storing the child nodes
  // directly in the use_info.

  // Make insn_info the first (and thus directly-encoded) choice since
  // insn () is read much more often than phi ().
  using insn_or_phi = pointer_mux<insn_info, phi_info>;

  // The use belongs to a list that is partitioned into three sections:
  //
  // (1) all uses in nondebug instructions, in reverse postorder
  //
  // (2) all uses in debug instructions, in reverse postorder
  //
  // (3) all phi nodes, in no particular order.
  //
  // In order to preserve memory:
  //
  // - The set_info just has a pointer to the first use.
  //
  // - The first use's "prev" pointer points to the last use.
  //
  // - The last use's "next" pointer points to the last use in a nondebug
  //   instruction, or null if there are no such uses.
  using last_use_or_prev_use = pointer_mux<use_info>;
  using last_nondebug_insn_use_or_next_use = pointer_mux<use_info>;

  use_info (insn_or_phi, resource_info, set_info *);

  use_info *last_use () const;
  use_info *last_nondebug_insn_use () const;
  bool calculate_is_last_nondebug_insn_use () const;

  void record_reference (rtx_obj_reference, bool);
  void set_insn (insn_info *);
  void set_def (set_info *set) { m_def = set; }
  void set_is_live_out_use (bool value) { m_is_live_out_use = value; }
  void copy_prev_from (use_info *);
  void copy_next_from (use_info *);
  void set_last_use (use_info *);
  void set_prev_use (use_info *);
  void set_last_nondebug_insn_use (use_info *);
  void set_next_use (use_info *);
  void clear_use_links ();
  bool has_use_links ();
  bool check_integrity ();

  // The location of the use.
  insn_or_phi m_insn_or_phi;

  // The overloaded "prev" and "next" pointers, as described above.
  last_use_or_prev_use m_last_use_or_prev_use;
  last_nondebug_insn_use_or_next_use m_last_nondebug_insn_use_or_next_use;

  // The value of def ().
  set_info *m_def;
};

// Iterators for lists of uses.
using use_iterator = list_iterator<use_info, &use_info::next_use>;
using reverse_use_iterator = list_iterator<use_info, &use_info::prev_use>;

// Like use_iterator, but specifically for uses by nondebug instructions,
// uses by any kind of instruction, and uses by phi nodes respectively.
// These iterators allow a nullptr end point even if there are other types
// of use in the same definition.
using nondebug_insn_use_iterator
  = list_iterator<use_info, &use_info::next_nondebug_insn_use>;
using debug_insn_use_iterator
  = list_iterator<use_info, &use_info::next_debug_insn_use>;
using any_insn_use_iterator
  = list_iterator<use_info, &use_info::next_any_insn_use>;
using phi_use_iterator = list_iterator<use_info, &use_info::prev_phi_use>;

// A view of an access_array in which every entry is known to be a use_info.
using use_array = const_derived_container<use_info *, access_array>;

// An access_info that describes a definition of a resource.  The definition
// can be a set or a clobber; the difference is that a set provides a known
// and potentially useful value, while a clobber provides an unknown and
// unusable value.
//
// Every definition is associated with an insn_info.  All definitions of
// a given resource are stored in a linked list, maintained in reverse
// postorder.
class def_info : public access_info
{
  // Overall size: 4 LP64 words
  friend class function_info;
  friend class clobber_group;

public:
  // Return the instruction that contains the definition.
  insn_info *insn () const { return m_insn; }

  // Return the basic block that contains the definition.
  bb_info *bb () const;

  // Return the extended basic block that contains the access.
  ebb_info *ebb () const;

  // Return the previous and next definitions of the same resource,
  // in reverse postorder, or null if no such definition exists.
  def_info *prev_def () const;
  def_info *next_def () const;

  // Return true if this is the first definition in the list.
  bool is_first_def () const;

  // Return true if this is the last definition in the list.
  bool is_last_def () const;

  // Print the location of the definition to PP.
  void print_location (pretty_printer *pp) const;

  // Print a unique identifier for this definition to PP.  The identifier has
  // the form <resource>:<insn uid>.
  void print_identifier (pretty_printer *pp) const;

protected:
  def_info (insn_info *insn, resource_info resource, access_kind kind);

private:
  // In order to preserve memory, the list head only points to the first
  // definition in the list.  The "prev" entry of the first definition
  // then points to the last definition.
  using last_def_or_prev_def = pointer_mux<def_info>;

  // For similar memory-saving reasons, if we want to create a splay tree
  // of accesses to a resource, we hang the root off the "next" entry of
  // the last definition in the list.
  using splay_root_or_next_def = pointer_mux<def_node, def_info>;

  void set_insn (insn_info *insn) { m_insn = insn; }

  def_info *last_def () const;
  def_node *splay_root () const;

  void record_reference (rtx_obj_reference, bool);
  void copy_prev_from (def_info *);
  void copy_next_from (def_info *);
  void set_last_def (def_info *);
  void set_prev_def (def_info *);
  void set_splay_root (def_node *);
  void set_next_def (def_info *);
  void clear_def_links ();
  bool has_def_links ();

  // The location of the definition.
  insn_info *m_insn;

  // The overloaded "prev" and "next" pointers, as described above.
  last_def_or_prev_def m_last_def_or_prev_def;
  splay_root_or_next_def m_splay_root_or_next_def;
};

// Iterators for lists of definitions.
using def_iterator = list_iterator<def_info, &def_info::next_def>;
using reverse_def_iterator = list_iterator<def_info, &def_info::prev_def>;

// A view of an access_array in which every entry is known to be a
// def_info.
using def_array = const_derived_container<def_info *, access_array>;

// A def_info that sets the resource to a value that is both
// unknown and not useful.  This is only ever used for registers,
// since memory always has some useful contents.
//
// Neighboring clobbers are grouped into clobber_groups, so that it's
// possibly to skip over all neighboring clobbers in a single step.
class clobber_info : public def_info
{
  // Overall size: 8 LP64 words
  friend class default_splay_tree_accessors<clobber_info *>;
  friend class default_splay_tree_accessors_with_parent<clobber_info *>;
  friend class function_info;
  friend class clobber_group;

public:
  using splay_tree = default_rootless_splay_tree<clobber_info *>;

  // Return true if the clobber belongs to a clobber_group, false if it
  // is standalone.
  bool is_in_group () const { return m_group; }

  // Return the group that the clobber is in, or null if none.
  //
  // Complexity: amortized O(1), worst case O(N), where N is the number
  // of clobbers in the containing clobber_group.
  clobber_group *group () const;

  // Print a description of the clobber to PP under the control of
  // PP_ACCESS_* flags FLAGS.
  void print (pretty_printer *pp,
	      unsigned int flags = PP_ACCESS_DEFAULT) const;

private:
  // Once normal call clobbers are taken out of the equation by
  // insn_call_clobbers_notes, clobber_infos account for roughly 6% of all
  // def_infos, with the rest being set_infos.  clobber_infos are
  // therefore much less size-sensitive than set_infos are.
  //
  // As noted above, we want to group neighboring clobbers together so that
  // we can quickly step over them to find the previous or next "real" set.
  // We also want to be able to split the group in sublinear time,
  // for example when inserting a set/use pair between two clobbers
  // in a group.
  //
  // So:
  //
  // - Clobbers need to have ready access to their group, so that we
  //   can cheaply skip over the whole group.  This means that they
  //   need a group pointer.
  //
  // - We need to be able to update the group pointer lazily, so that
  //   the cost of updating it is counted against accesses to the clobbers
  //   that need updating.
  //
  // We also want to be able to insert clobbers into a group in
  // amortized logarithmic time.
  //
  // We therefore use a splay tree to represent the clobbers in a group,
  // with the nodes storing their parent node.  It is then possible to
  // perform splay operations without first getting hold of the root.
  // The root of the splay tree always has a valid, up-to-date group,
  // so lazy group updates can get the new group from there.
  //
  // Roughly 90% of clobbers have a neighboring definition in the same
  // block, which means that most need to be stored in a splay tree.
  // We therefore store the splay tree fields directly in the clobber_info
  // rather than using a separate node object.

  clobber_info (insn_info *, unsigned int);

  void set_group (clobber_group *group) { m_group = group; }
  void update_group (clobber_group *);
  clobber_group *recompute_group ();

  // The child and parent nodes in the splay tree.
  clobber_info *m_children[2];
  clobber_info *m_parent;

  // The last known value of group (), which might now be out of date.
  clobber_group *m_group;
};

using clobber_tree = clobber_info::splay_tree::rooted;

// A def_info that sets the resource to a useful value.  It records
// all uses of the value in a linked list.  The list is partitioned
// into three sections:
//
// (1) all uses by nondebug instructions, in reverse postorder, followed by
// (2) all uses by debug instructions, in reverse postorder, followed by
// (3) all uses by phi nodes, in no particular order.
//
// There are two cases:
//
// - If we know in advance that there is a single definition of a resource R
//   and therefore decide not to use phi nodes for R, (1) and (2) contain
//   all uses of R, regardless of which blocks contain the uses.  (3) is
//   then empty.
//
// - Otherwise, (1) only contains uses in the same extended basic block
//   as the definition, and it is terminated by a use that marks the end
//   of the live range for the EBB.  In other words, if the resource dies
//   in the EBB, the last use by a nondebug instruction marks the point at
//   which it dies, otherwise there is a fake live-out use at the end of
//   the EBB.
//
// Since debug instructions should not affect codegen, they opportunisticly
// attach to the same set_info as nondebug instructions where possible.
// If a nondebug instruction would attach to a degenerate phi and if no
// such phi exists, debug instructions instead attach to whichever set_info
// provides the value, regardless of where that set_info is.
class set_info : public def_info
{
  // Overall size: 6 LP64 words.
  friend class function_info;
  using use_splay_tree = splay_tree<use_info *>;

public:
  // Return the first and last uses of the set, or null if the list is empty.
  // See the comment above for details about the order.
  use_info *first_use () const { return m_first_use; }
  use_info *last_use () const;

  // Return the first and last uses of the set by nondebug instructions,
  // or null if there are no such uses.  The uses are in reverse postorder.
  use_info *first_nondebug_insn_use () const;
  use_info *last_nondebug_insn_use () const;

  // Return the first use of the set by debug instructions, or null if
  // there is no such use.
  use_info *first_debug_insn_use () const;

  // Return the first use of the set by any kind of instruction, or null
  // if there are no such uses.  The uses are in the order described above.
  use_info *first_any_insn_use () const;

  // Return the last use of the set by phi inputs, or null if there are no
  // such uses.  The phi input uses are in no particular order.
  use_info *last_phi_use () const;

  // Return true if at least one nondebug instruction or phi node uses
  // the set's result.  This is equivalent to testing whether the set is
  // ever live.
  bool has_nondebug_uses () const;

  // Return true if anything uses the set's result.  Note that this includes
  // uses by debug instructions, so it should not be used for optimization
  // decisions.
  bool has_any_uses () const { return m_first_use; }

  // Return true if at least one nondebug instruction uses the set's result.
  bool has_nondebug_insn_uses () const;

  // Return true if at least one phi node uses the set's result.
  bool has_phi_uses () const;

  // If there is exactly one nondebug use of the set's result, return that use,
  // otherwise return null.  The use might be in an instruction or in a phi
  // node.
  use_info *single_nondebug_use () const;

  // If exactly one nondebug instruction uses the set's result, return the use
  // by that instruction, otherwise return null.
  use_info *single_nondebug_insn_use () const;

  // If exactly one phi node uses the set's result, return the use by that phi
  // node, otherwise return null.
  use_info *single_phi_use () const;

  // Return true if the set and its uses are contained within a single
  // extended basic block, with the set coming first.  This implies
  // that all uses are by instructions rather than phi nodes.
  bool is_local_to_ebb () const;

  // List all the uses of the set, in the order described above.
  iterator_range<use_iterator> all_uses () const;

  // Return uses () in reverse order.
  iterator_range<reverse_use_iterator> reverse_all_uses () const;

  // List the uses of the set by nondebug instructions, in reverse postorder.
  iterator_range<nondebug_insn_use_iterator> nondebug_insn_uses () const;

  // List the uses of the set by debug instructions, in reverse postorder.
  iterator_range<debug_insn_use_iterator> debug_insn_uses () const;

  // Return nondebug_insn_uses () in reverse order.
  iterator_range<reverse_use_iterator> reverse_nondebug_insn_uses () const;

  // List the uses of the set by any kind of instruction.  The list follows
  // the order described above.
  iterator_range<any_insn_use_iterator> all_insn_uses () const;

  // List the uses of the set by phi nodes, in no particular order.
  // There is therefore no reversed equivalent of this list.
  iterator_range<phi_use_iterator> phi_uses () const;

  // Print a description of the set to PP under the control of
  // PP_ACCESS_* flags FLAGS.
  void print (pretty_printer *pp,
	      unsigned int flags = PP_ACCESS_DEFAULT) const;

protected:
  set_info (insn_info *, resource_info, access_kind);

  // Print information about uses () to PP, continuing information printed
  // about the set itself.
  void print_uses_on_new_lines (pretty_printer *pp) const;

private:
  // Sets (including phis) account for about 94% of all definitions

  set_info (insn_info *, resource_info);

  void set_first_use (use_info *);

  // The first use in the list.
  use_info *m_first_use;

  // The root of a splay tree of all uses, built lazily when we first
  // think it's needed.
  use_splay_tree m_use_tree;
};

// A set_info for an on-the-side phi node.  The phi node is attached
// to an extended basic block EBB and has one input for each incoming edge.
// The inputs are represented as an array of use_infos, with input I
// corresponding to EDGE_PRED (EBB->first_bb ()->cfg_bb (), I).
//
// Each phi node has a densely-allocated unique identifier, which is intended
// to be suitable for bitmaps or sbitmaps.
//
// All the phi nodes in an extended basic block are chained together
// into a linked list.  The list has no particular order.
class phi_info : public set_info
{
  // Overall size: 8 LP64 words
  friend class function_info;

public:
  // Return the previous and next phi nodes in the extended basic block's list,
  // or null if none.
  phi_info *prev_phi () const { return m_prev_phi; }
  phi_info *next_phi () const { return m_next_phi; }

  // Return the number of phi inputs.  This is 1 for degenerate phis,
  // otherwise it is equal to the number of incoming edges.
  unsigned int num_inputs () const { return m_num_inputs; }

  // Return true if the phi node is degenerate, i.e. if it has only a
  // single input.
  bool is_degenerate () const { return m_num_inputs == 1; }

  // Return the phi node's unique identifier.
  unsigned int uid () const { return m_uid; }

  // Return the array of inputs.  For degenerate phi nodes, this array contains
  // a single element, otherwise it has one input per incoming edge,
  // with element E corresponding to incoming edge E.
  use_array inputs () const;

  // Return the use_info that describes the phi input for incoming edge E.
  use_info *input_use (unsigned int e) const;

  // Return the value of resource () on incoming edge E, or null if the
  // value is completely undefined for that edge.
  set_info *input_value (unsigned int e) const;

  // Print a description of the phi node to PP under the control of
  // PP_ACCESS_* flags FLAGS.
  void print (pretty_printer *pp,
	      unsigned int flags = PP_ACCESS_DEFAULT) const;

private:
  phi_info (insn_info *insn, resource_info resource, unsigned int uid);

  void make_degenerate (use_info *);
  void set_inputs (use_array inputs);
  void set_prev_phi (phi_info *prev_phi) { m_prev_phi = prev_phi; }
  void set_next_phi (phi_info *next_phi) { m_next_phi = next_phi; }
  void clear_phi_links () { m_prev_phi = m_next_phi = nullptr; }
  bool has_phi_links () { return m_prev_phi || m_next_phi; }

  // The values returned by the accessors above.
  unsigned int m_uid;
  unsigned int m_num_inputs;
  union
  {
    access_info *const *m_inputs;
    access_info *m_single_input;
  };
  phi_info *m_prev_phi;
  phi_info *m_next_phi;
};

// An iterator for lists of phi nodes.
using phi_iterator = list_iterator<phi_info, &phi_info::next_phi>;

// One node in a splay tree of definitions.  This base class represents
// a single def_info, but it is structured to allow derived classes
// to add a range.
class def_node
{
  // Size: 3 LP64 words.
  friend class function_info;
  friend class default_splay_tree_accessors<def_node *>;

public:
  // Return the first definition that the node represents.
  def_info *first_def () const;

  // Return which type of access first_def () is.
  bool contains_clobber () const { return m_clobber_or_set.is_first (); }
  bool contains_set () const { return m_clobber_or_set.is_second (); }

protected:
  // More nodes are clobbers rather than sets, so put clobbers first.
  // Neither choice can be null.
  using clobber_or_set = pointer_mux<clobber_info, set_info>;

  // Construct a node that represents FIRST_DEF (and possibly later
  // definitions too, if called from a derived class).
  def_node (clobber_or_set first_def);

  // The first definition in the node.
  clobber_or_set m_clobber_or_set;

private:
  // The splay tree child nodes.
  def_node *m_children[2];
};

// One node in a splay tree of def_infos, representing a single set_info.
class set_node : public def_node
{
  // Overall size: 3 LP64 words.
  friend class function_info;

public:
  // Return the set that the node contains.
  set_info *set () const { return m_clobber_or_set.known_second (); }

  // Print a description of the node to PP.
  void print (pretty_printer *pp) const;

private:
  // Construct a node for SET.
  set_node (set_info *set) : def_node (set) {}
};

// One node in a splay tree of def_infos.  This class represents
// a list of contiguous clobber_infos, in execution order.
class clobber_group : public def_node
{
  // Overall size: 5 LP64 words.
  friend class function_info;

public:
  // Return the first and last clobbers in the group.  The results are
  // always nonnull.
  clobber_info *first_clobber () const;
  clobber_info *last_clobber () const { return m_last_clobber; }

  // Return the last clobber before INSN in the group, or null if none.
  clobber_info *prev_clobber (insn_info *insn) const;

  // Return the next clobber after INSN in the group, or null if none.
  clobber_info *next_clobber (insn_info *insn) const;

  // Return true if this group has been replaced by new clobber_groups.
  bool has_been_superceded () const { return !m_last_clobber; }

  // Return a list of the clobbers in the group, in execution order.
  iterator_range<def_iterator> clobbers () const;

  // Print a description of the group to PP.
  void print (pretty_printer *pp) const;

private:
  clobber_group (clobber_info *);
  clobber_group (clobber_info *, clobber_info *, clobber_info *);

  // Set the values of first_clobber () and last_clobber ().
  void set_first_clobber (clobber_info *c) { m_clobber_or_set = c; }
  void set_last_clobber (clobber_info *c) { m_last_clobber = c; }

  // The value returned by last_clobber ().
  clobber_info *m_last_clobber;

  // A splay tree that contains all the clobbers in the group.
  // The root of the splay tree always has an up-to-date group
  // pointer, but the other clobbers in the tree might not.
  clobber_tree m_clobber_tree;
};

// A splay tree in which one node represents a standalone set_info or a
// range of consecutive clobber_infos.  The nodes follow execution order
// and maintain the invariant that no two groups of clobber_infos appear
// next to each other (instead, the groups are merged).
using def_splay_tree = default_splay_tree<def_node *>;

// This type represents a choice between:
//
// (1) a single definition of a resource
// (2) a node in a def_splay_tree that represents either a single
//     set or a group of clobbers.
class def_mux : public pointer_mux<def_info, def_node>
{
  using parent = pointer_mux<def_info, def_node>;

  // Provide the same constructors as the pointer_mux.
  using parent::parent;

public:
  // Return the first definition associated with this mux.  If the mux holds
  // a single definition, the result is that definition.  If the mux holds
  // a clobber_group, the result is the first clobber in the group.
  def_info *first_def () const;

  // Return the last definition associated with this mux.  If the mux holds
  // a single definition, the result is that definition.  If the mux holds
  // a clobber_group, the result is the last clobber in the group.
  def_info *last_def () const;

  // If the pointer represents a set_info, return that set_info,
  // otherwise return null.
  set_info *set () const;
};

// This class represents the result of looking up the definition of a
// resource at a particular point, here referred to as point P.
// There are four states:
//
// - MUX is null if there were no definitions to search.
//
// - Otherwise, COMPARISON is 0 if we found a definition at P or a
//   clobber_group that spans P.  MUX then contains this definition
//   or clobber_group.
//
// - Otherwise, COMPARISON is greater than 0 if we found the definition
//   that precedes P or the group of clobbers that precedes P.  MUX then
//   contains this definition or clobber_group.
//
// - Otherwise, COMPARISON is less than zero and we found the definition
//   that follows P, or the group of clobbers that follows P.  MUX then
//   contains this definition or clobber_group.
class def_lookup
{
public:
  // If we found a clobber_group that spans P, return the definition
  // that precedes the start of the group, or null if none.
  //
  // Otherwise, return the last definition that occurs before P,
  // or null if none.
  def_info *last_def_of_prev_group () const;

  // If we found a clobber_group that spans P, return the definition
  // that follows the end of the group, or null if none.
  //
  // Otherwise, return the first definition that occurs after P,
  // or null if none.
  def_info *first_def_of_next_group () const;

  // If we found a set_info at P, return that set_info, otherwise return null.
  set_info *matching_set () const;

  // If we found a set_info at P, return that set_info, otherwise return
  // prev_def ().
  def_info *matching_set_or_last_def_of_prev_group () const;

  // If we found a set_info at P, return that set_info, otherwise return
  // next_def ().
  def_info *matching_set_or_first_def_of_next_group () const;

  // P is the location of INSN.  Return the last definition (of any kind)
  // that occurs before INSN, or null if none.
  def_info *prev_def (insn_info *insn) const;

  // P is the location of INSN.  Return the next definition (of any kind)
  // that occurs after INSN, or null if none.
  def_info *next_def (insn_info *insn) const;

  def_mux mux;
  int comparison;
};

void pp_resource (pretty_printer *, resource_info);
void pp_access (pretty_printer *, const access_info *,
		unsigned int flags = PP_ACCESS_DEFAULT);
void pp_accesses (pretty_printer *, access_array,
		  unsigned int flags = PP_ACCESS_DEFAULT);
void pp_def_node (pretty_printer *, const def_node *);
void pp_def_mux (pretty_printer *, def_mux);
void pp_def_lookup (pretty_printer *, def_lookup);
void pp_def_splay_tree (pretty_printer *, def_splay_tree);

}

void dump (FILE *, rtl_ssa::resource_info);
void dump (FILE *, const rtl_ssa::access_info *,
	   unsigned int flags = rtl_ssa::PP_ACCESS_DEFAULT);
void dump (FILE *, rtl_ssa::access_array,
	   unsigned int flags = rtl_ssa::PP_ACCESS_DEFAULT);
void dump (FILE *, const rtl_ssa::def_node *);
void dump (FILE *, rtl_ssa::def_mux);
void dump (FILE *, rtl_ssa::def_lookup);
void dump (FILE *, rtl_ssa::def_splay_tree);

void DEBUG_FUNCTION debug (const rtl_ssa::resource_info *);
void DEBUG_FUNCTION debug (const rtl_ssa::access_info *);
void DEBUG_FUNCTION debug (const rtl_ssa::access_array);
void DEBUG_FUNCTION debug (const rtl_ssa::def_node *);
void DEBUG_FUNCTION debug (const rtl_ssa::def_mux &);
void DEBUG_FUNCTION debug (const rtl_ssa::def_lookup &);
void DEBUG_FUNCTION debug (const rtl_ssa::def_splay_tree &);
