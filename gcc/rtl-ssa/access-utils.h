// Access-related utilities for RTL SSA                             -*- C++ -*-
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

// Return a referene to the whole of register REGNO.
inline resource_info
full_register (unsigned int regno)
{
  return { GET_MODE (regno_reg_rtx[regno]), regno };
}

// Return true if sorted array ACCESSES includes an access to hard registers.
inline bool
accesses_include_hard_registers (const access_array &accesses)
{
  return accesses.size () && HARD_REGISTER_NUM_P (accesses.front ()->regno ());
}

// Return true if ACCESSES includes a reference to a non-fixed hard register.
inline bool
accesses_include_nonfixed_hard_registers (access_array accesses)
{
  for (access_info *access : accesses)
    {
      if (!HARD_REGISTER_NUM_P (access->regno ()))
	break;
      if (!fixed_regs[access->regno ()])
	return true;
    }
  return false;
}

// Return true if sorted array ACCESSES includes an access to memory.
inline bool
accesses_include_memory (const access_array &accesses)
{
  return accesses.size () && accesses.back ()->is_mem ();
}

// If sorted array ACCESSES includes an access to memory, return the access,
// otherwise return null.
template<typename T>
inline auto
memory_access (T accesses) -> decltype (accesses[0])
{
  if (accesses.size () && accesses.back ()->is_mem ())
    return accesses.back ();
  return nullptr;
}

// If ACCESSES has a memory access, drop it.  Otherwise, return ACCESSES
// unchanged.
template<typename T>
inline T
drop_memory_access (T accesses)
{
  if (!memory_access (accesses))
    return accesses;

  access_array arr (accesses);
  return T (arr.begin (), accesses.size () - 1);
}

// Filter ACCESSES to return an access_array of only those accesses that
// satisfy PREDICATE.  Alocate the new array above WATERMARK.
template<typename T, typename FilterPredicate>
inline T
filter_accesses (obstack_watermark &watermark,
		 T accesses,
		 FilterPredicate predicate)
{
  access_array_builder builder (watermark);
  builder.reserve (accesses.size ());
  for (auto access : accesses)
    if (predicate (access))
      builder.quick_push (access);
  return T (builder.finish ());
}

// Given an array of ACCESSES, remove any access with regno REGNO.
// Allocate the new access array above WM.
template<typename T>
inline T
remove_regno_access (obstack_watermark &watermark,
		     T accesses, unsigned int regno)
{
  using Access = decltype (accesses[0]);
  auto pred = [regno](Access a) { return a->regno () != regno; };
  return filter_accesses (watermark, accesses, pred);
}

// As above, but additionally check that we actually did remove an access.
template<typename T>
inline T
check_remove_regno_access (obstack_watermark &watermark,
			   T accesses, unsigned regno)
{
  auto orig_size = accesses.size ();
  auto result = remove_regno_access (watermark, accesses, regno);
  gcc_assert (result.size () < orig_size);
  return result;
}

// If sorted array ACCESSES includes a reference to REGNO, return the
// access, otherwise return null.
template<typename T>
inline auto
find_access (T accesses, unsigned int regno) -> decltype (accesses[0])
{
  unsigned int start = 0;
  unsigned int end = accesses.size ();
  while (start < end)
    {
      unsigned int mid = (start + end) / 2;
      unsigned int found = accesses[mid]->regno ();
      if (found == regno)
	return accesses[mid];
      if (found < regno)
	start = mid + 1;
      else
	end = mid;
    }
  return nullptr;
}

// If sorted array ACCESSES includes a reference to REGNO, return the
// index of the access, otherwise return -1.
inline int
find_access_index (access_array accesses, unsigned int regno)
{
  unsigned int start = 0;
  unsigned int end = accesses.size ();
  while (start < end)
    {
      unsigned int mid = (start + end) / 2;
      unsigned int found = accesses[mid]->regno ();
      if (found == regno)
	return mid;
      if (found < regno)
	start = mid + 1;
      else
	end = mid;
    }
  return -1;
}

// If ACCESS is a set whose result is used by at least one instruction,
// return the access as a set_info, otherwise return null.
inline const set_info *
set_with_nondebug_insn_uses (const access_info *access)
{
  if (access->is_set_with_nondebug_insn_uses ())
    // No need for as_a; this test is just as definitive.
    return static_cast<const set_info *> (access);
  return nullptr;
}

// A non-const version of the above.
inline set_info *
set_with_nondebug_insn_uses (access_info *access)
{
  if (access->is_set_with_nondebug_insn_uses ())
    return static_cast<set_info *> (access);
  return nullptr;
}

// ACCESS is known to be associated with an instruction rather than
// a phi node.  Return which instruction that is.
inline insn_info *
access_insn (const access_info *access)
{
  // In release builds this function reduces to a single pointer reference.
  if (auto *def = dyn_cast<const def_info *> (access))
    return def->insn ();
  return as_a<const use_info *> (access)->insn ();
}

// If ACCESS records a use, return the value that it uses.  If ACCESS records
// a set, return that set.  If ACCESS records a clobber, return null.
inline const set_info *
access_value (const access_info *access)
{
  if (!access)
    return nullptr;

  if (auto *use = dyn_cast<const use_info *> (access))
    return use->def ();

  return dyn_cast<const set_info *> (access);
}

// A non-const version of the above.
inline set_info *
access_value (access_info *access)
{
  auto *const_access = const_cast<const access_info *> (access);
  return const_cast<set_info *> (access_value (const_access));
}

// If ACCESS is a degenerate phi, return the set_info that defines its input,
// otherwise return ACCESS itself.
template<typename T>
inline const T *
look_through_degenerate_phi (const T *access)
{
  if (auto *phi = dyn_cast<const phi_info *> (access))
    if (phi->is_degenerate ())
      return phi->input_value (0);
  return access;
}

// A non-const version of the above.
template<typename T>
inline T *
look_through_degenerate_phi (T *access)
{
  auto *const_access = const_cast<const T *> (access);
  return const_cast<T *> (look_through_degenerate_phi (const_access));
}

// If CLOBBER is in a group, return the first clobber in the group,
// otherwise return CLOBBER itself.
inline clobber_info *
first_clobber_in_group (clobber_info *clobber)
{
  if (clobber->is_in_group ())
    return clobber->group ()->first_clobber ();
  return clobber;
}

// If CLOBBER is in a group, return the last clobber in the group,
// otherwise return CLOBBER itself.
inline clobber_info *
last_clobber_in_group (clobber_info *clobber)
{
  if (clobber->is_in_group ())
    return clobber->group ()->last_clobber ();
  return clobber;
}

// If DEF is a clobber in a group, return the containing group,
// otherwise return DEF.
inline def_mux
clobber_group_or_single_def (def_info *def)
{
  if (auto *clobber = dyn_cast<clobber_info *> (def))
    if (clobber->is_in_group ())
      return clobber->group ();
  return def;
}

// Return the first definition associated with NODE.  If NODE holds
// a single set, the result is that set.  If NODE holds a clobber_group,
// the result is the first clobber in the group.
inline def_info *
first_def (def_node *node)
{
  return node->first_def ();
}

// Likewise for something that is either a node or a single definition.
inline def_info *
first_def (def_mux mux)
{
  return mux.first_def ();
}

// Return the last definition associated with NODE.  If NODE holds
// a single set, the result is that set.  If NODE holds a clobber_group,
// the result is the last clobber in the group.
inline def_info *
last_def (def_node *node)
{
  if (auto *group = dyn_cast<clobber_group *> (node))
    return group->last_clobber ();
  return node->first_def ();
}

// Likewise for something that is either a node or a single definition.
inline def_info *
last_def (def_mux mux)
{
  return mux.last_def ();
}

// If INSN's definitions contain a single set, return that set, otherwise
// return null.
inline set_info *
single_set_info (insn_info *insn)
{
  set_info *set = nullptr;
  for (auto def : insn->defs ())
    if (auto this_set = dyn_cast<set_info *> (def))
      {
	if (set)
	  return nullptr;
	set = this_set;
      }
  return set;
}

int lookup_use (splay_tree<use_info *> &, insn_info *);
int lookup_def (def_splay_tree &, insn_info *);
int lookup_clobber (clobber_tree &, insn_info *);
int lookup_call_clobbers (insn_call_clobbers_tree &, insn_info *);

// Search backwards from immediately before INSN for the first "relevant"
// instruction recorded in TREE.  IGNORE is an object that provides the same
// interface as ignore_nothing; it defines which insns are "relevant"
// and which should be ignored.
//
// Return null if no such relevant instruction exists.
template<typename IgnorePredicates>
insn_info *
prev_call_clobbers (insn_call_clobbers_tree &tree, insn_info *insn,
		    IgnorePredicates ignore)
{
  if (!tree)
    return nullptr;

  int comparison = lookup_call_clobbers (tree, insn);
  while (comparison <= 0 || ignore.should_ignore_insn (tree->insn ()))
    {
      if (!tree.splay_prev_node ())
	return nullptr;

      comparison = 1;
    }
  return tree->insn ();
}

// Search forwards from immediately after INSN for the first "relevant"
// instruction recorded in TREE.  IGNORE is an object that provides the
// same interface as ignore_nothing; it defines which insns are "relevant"
// and which should be ignored.
//
// Return null if no such relevant instruction exists.
template<typename IgnorePredicates>
insn_info *
next_call_clobbers (insn_call_clobbers_tree &tree, insn_info *insn,
		    IgnorePredicates ignore)
{
  if (!tree)
    return nullptr;

  int comparison = lookup_call_clobbers (tree, insn);
  while (comparison >= 0 || ignore.should_ignore_insn (tree->insn ()))
    {
      if (!tree.splay_next_node ())
	return nullptr;

      comparison = -1;
    }
  return tree->insn ();
}

// Search forwards from immediately after INSN for the first instruction
// recorded in TREE.  Return null if no such instruction exists.
inline insn_info *
next_call_clobbers (insn_call_clobbers_tree &tree, insn_info *insn)
{
  return next_call_clobbers (tree, insn, ignore_nothing ());
}

// If ACCESS is a set, return the first "relevant" use of ACCESS by a
// nondebug insn.  IGNORE is an object that provides the same interface
// as ignore_nothing; it defines which accesses and insns are "relevant"
// and which should be ignored.
//
// Return null if ACCESS is not a set or if no such relevant use exists.
template<typename IgnorePredicates>
inline use_info *
first_nondebug_insn_use (const access_info *access, IgnorePredicates ignore)
{
  if (const set_info *set = set_with_nondebug_insn_uses (access))
    {
      // Written this way to emphasize to the compiler that first_use
      // must be nonnull in this situation.
      use_info *use = set->first_use ();
      do
	{
	  if (!ignore.should_ignore_insn (use->insn ()))
	    return use;
	  use = use->next_nondebug_insn_use ();
	}
      while (use);
    }
  return nullptr;
}

// If ACCESS is a set, return the last "relevant" use of ACCESS by a
// nondebug insn.  IGNORE is an object that provides the same interface
// as ignore_nothing; it defines which accesses and insns are "relevant"
// and which should be ignored.
//
// Return null if ACCESS is not a set or if no such relevant use exists.
template<typename IgnorePredicates>
inline use_info *
last_nondebug_insn_use (const access_info *access, IgnorePredicates ignore)
{
  if (const set_info *set = set_with_nondebug_insn_uses (access))
    {
      // Written this way to emphasize to the compiler that
      // last_nondebug_insn_use must be nonnull in this situation.
      use_info *use = set->last_nondebug_insn_use ();
      do
	{
	  if (!ignore.should_ignore_insn (use->insn ()))
	    return use;
	  use = use->prev_use ();
	}
      while (use);
    }
  return nullptr;
}

// If DEF is null, return null.
//
// Otherwise, search backwards for an access to DEF->resource (), starting at
// the end of DEF's live range.  Ignore clobbers if IGNORE_CLOBBERS_SETTING
// is YES, otherwise treat them like any other access.  Also ignore any
// accesses and insns that IGNORE says should be ignored, where IGNORE
// is an object that provides the same interface as ignore_nothing.
//
// Thus if DEF is a set that is used by nondebug insns, the first access
// that the function considers is the last such use of the set.  Otherwise,
// the first access that the function considers is DEF itself.
//
// Return the access found, or null if there is no access that meets
// the criteria.
//
// Note that this function does not consider separately-recorded call clobbers,
// although such clobbers are only relevant if IGNORE_CLOBBERS_SETTING is NO.
template<typename IgnorePredicates>
access_info *
last_access (def_info *def, ignore_clobbers ignore_clobbers_setting,
	     IgnorePredicates ignore)
{
  while (def)
    {
      auto *clobber = dyn_cast<clobber_info *> (def);
      if (clobber && ignore_clobbers_setting == ignore_clobbers::YES)
	def = first_clobber_in_group (clobber);
      else if (!ignore.should_ignore_def (def))
	{
	  if (use_info *use = last_nondebug_insn_use (def, ignore))
	    return use;
	  if (!ignore.should_ignore_insn (def->insn ()))
	    return def;
	}
      def = def->prev_def ();
    }
  return nullptr;
}

// Search backwards for an access to DEF->resource (), starting
// immediately before the point at which DEF occurs.  Ignore clobbers
// if IGNORE_CLOBBERS_SETTING is YES, otherwise treat them like any other
// access.  Also ignore any accesses and insns that IGNORE says should be
// ignored, where IGNORE is an object that provides the same interface as
// ignore_nothing.
//
// Thus if DEF->insn () uses DEF->resource (), that use is the first access
// that the function considers, since an instruction's uses occur strictly
// before its definitions.
//
// Note that this function does not consider separately-recorded call clobbers,
// although such clobbers are only relevant if IGNORE_CLOBBERS_SETTING is NO.
template<typename IgnorePredicates>
inline access_info *
prev_access (def_info *def, ignore_clobbers ignore_clobbers_setting,
	     IgnorePredicates ignore)
{
  return last_access (def->prev_def (), ignore_clobbers_setting, ignore);
}

// If DEF is null, return null.
//
// Otherwise, search forwards for an access to DEF->resource (),
// starting at DEF itself.  Ignore clobbers if IGNORE_CLOBBERS_SETTING
// is YES, otherwise treat them like any other access.  Also ignore any
// accesses and insns that IGNORE says should be ignored, where IGNORE
// is an object that provides the same interface as ignore_nothing.
//
// Return the definition found, or null if there is no access that meets
// the criteria.
//
// Note that this function does not consider separately-recorded call clobbers,
// although such clobbers are only relevant if IGNORE_CLOBBERS_SETTING is NO.
template<typename IgnorePredicates>
access_info *
first_access (def_info *def, ignore_clobbers ignore_clobbers_setting,
	      IgnorePredicates ignore)
{
  while (def)
    {
      auto *clobber = dyn_cast<clobber_info *> (def);
      if (clobber && ignore_clobbers_setting == ignore_clobbers::YES)
	def = last_clobber_in_group (clobber);
      else if (!ignore.should_ignore_def (def))
	{
	  if (!ignore.should_ignore_insn (def->insn ()))
	    return def;
	  if (use_info *use = first_nondebug_insn_use (def, ignore))
	    return use;
	}
      def = def->next_def ();
    }
  return nullptr;
}

// Search forwards for the next access to DEF->resource (),
// starting immediately after DEF's instruction.  Ignore clobbers if
// IGNORE_CLOBBERS_SETTING is YES, otherwise treat them like any other access.
// Also ignore any accesses and insns that IGNORE says should be ignored,
// where IGNORE is an object that provides the same interface as
// ignore_nothing.
//
// Thus if DEF is a set with uses by nondebug insns, the first access that the
// function considers is the first such use of the set.  Otherwise, the first
// access that the function considers is the definition after DEF.
//
// Return the access found, or null if there is no access that meets the
// criteria.
//
// Note that this function does not consider separately-recorded call clobbers,
// although such clobbers are only relevant if IGNORE_CLOBBERS_SETTING is NO.
template<typename IgnorePredicates>
access_info *
next_access (def_info *def, ignore_clobbers ignore_clobbers_setting,
	     IgnorePredicates ignore)
{
  if (!ignore.should_ignore_def (def))
    if (use_info *use = first_nondebug_insn_use (def, ignore))
      return use;

  return first_access (def->next_def (), ignore_clobbers_setting, ignore);
}

// Return true if ACCESS1 should before ACCESS2 in an access_array.
inline bool
compare_access_infos (const access_info *access1, const access_info *access2)
{
  gcc_checking_assert (access1 == access2
		       || access1->regno () != access2->regno ());
  return access1->regno () < access2->regno ();
}

// Sort [BEGIN, END) into ascending regno order.  The sequence must have
// at most one access to a given a regno.
inline void
sort_accesses (access_info **begin, access_info **end)
{
  auto count = end - begin;
  if (count <= 1)
    return;

  if (count == 2)
    {
      gcc_checking_assert (begin[0]->regno () != begin[1]->regno ());
      if (begin[0]->regno () > begin[1]->regno ())
	std::swap (begin[0], begin[1]);
      return;
    }

  std::sort (begin, end, compare_access_infos);
}

// Sort the accesses in CONTAINER, which contains pointers to access_infos.
template<typename T>
inline void
sort_accesses (T &container)
{
  return sort_accesses (container.begin (), container.end ());
}

// The underlying non-template implementation of merge_access_arrays.
access_array merge_access_arrays_base (obstack_watermark &, access_array,
				       access_array);
// Merge access arrays ACCESSES1 and ACCESSES2, including the allocation
// in the area governed by WATERMARK.  Return an invalid access_array if
// ACCESSES1 and ACCESSES2 contain conflicting accesses to the same resource.
//
// T can be an access_array, a def_array or a use_array.
template<typename T>
inline T
merge_access_arrays (obstack_watermark &watermark, T accesses1, T accesses2)
{
  return T (merge_access_arrays_base (watermark, accesses1, accesses2));
}

// The underlying non-template implementation of insert_access.
access_array insert_access_base (obstack_watermark &, access_info *,
				 access_array);

// Return a new access_array that contains the result of inserting ACCESS1
// into sorted access array ACCESSES2.  Allocate the returned array in the
// area governed by WATERMARK.  Return an invalid access_array if ACCESSES2
// contains a conflicting access to the same resource as ACCESS1.
//
// T can be an access_array, a def_array or a use_array.
template<typename T>
inline T
insert_access (obstack_watermark &watermark,
	       typename T::value_type access1, T accesses2)
{
  return T (insert_access_base (watermark, access1, accesses2));
}

// Return a copy of USES that drops any use of DEF.
use_array remove_uses_of_def (obstack_watermark &, use_array uses,
			      def_info *def);

// The underlying non-template implementation of remove_note_accesses.
access_array remove_note_accesses_base (obstack_watermark &, access_array);

// If ACCESSES contains accesses that only occur in notes, return a new
// array without such accesses, allocating it in the area governed by
// WATERMARK.  Return ACCESSES itself otherwise.
//
// T can be an access_array, a def_array or a use_array.
template<typename T>
inline T
remove_note_accesses (obstack_watermark &watermark, T accesses)
{
  return T (remove_note_accesses_base (watermark, accesses));
}

// Return true if ACCESSES1 and ACCESSES2 have at least one resource in common.
bool accesses_reference_same_resource (access_array accesses1,
				       access_array accesses2);

// Return true if INSN clobbers the value of any resources in ACCESSES.
bool insn_clobbers_resources (insn_info *insn, access_array accesses);

}
