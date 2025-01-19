// Implementation of access-related functions for RTL SSA           -*- C++ -*-
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

#define INCLUDE_ALGORITHM
#define INCLUDE_FUNCTIONAL
#define INCLUDE_ARRAY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "df.h"
#include "rtl-ssa.h"
#include "rtl-ssa/internals.h"
#include "rtl-ssa/internals.inl"

using namespace rtl_ssa;

// This clobber belongs to a clobber_group but m_group appears to be
// out of date.  Update it and return the new (correct) value.
clobber_group *
clobber_info::recompute_group ()
{
  using splay_tree = clobber_info::splay_tree;

  // Splay this clobber to the root of the tree while searching for a node
  // that has the correct group.  The root always has the correct group,
  // so the search always breaks early and does not install this clobber
  // as the root.
  clobber_info *cursor = m_parent;
  auto find_group = [](clobber_info *node, unsigned int)
    {
      return node->m_group->has_been_superceded () ? nullptr : node->m_group;
    };
  clobber_group *group = splay_tree::splay_and_search (this, nullptr,
						       find_group);
  gcc_checking_assert (m_parent);

  // If the previous splay operation did anything, this clobber is now an
  // ancestor of CURSOR, and all the nodes inbetween have a stale group.
  // Since we have visited the nodes, we might as well update them too.
  //
  // If the previous splay operation did nothing, start the update from
  // this clobber instead.  In that case we change at most two clobbers:
  // this clobber and possibly its parent.
  if (cursor == m_parent)
    cursor = this;

  // Walk up the tree from CURSOR updating clobbers that need it.
  // This walk always includes this clobber.
  while (cursor->m_group != group)
    {
      cursor->m_group = group;
      cursor = cursor->m_parent;
    }

  gcc_checking_assert (m_group == group);
  return group;
}

// See the comment above the declaration.
void
resource_info::print_identifier (pretty_printer *pp) const
{
  if (is_mem ())
    pp_string (pp, "mem");
  else
    {
      char tmp[3 * sizeof (regno) + 2];
      snprintf (tmp, sizeof (tmp), "r%d", regno);
      pp_string (pp, tmp);
    }
}

// See the comment above the declaration.
void
resource_info::print_context (pretty_printer *pp) const
{
  if (HARD_REGISTER_NUM_P (regno))
    {
      if (const char *name = reg_names[regno])
	{
	  pp_space (pp);
	  pp_left_paren (pp);
	  pp_string (pp, name);
	  if (mode != E_BLKmode)
	    {
	      pp_colon (pp);
	      pp_string (pp, GET_MODE_NAME (mode));
	    }
	  pp_right_paren (pp);
	}
    }
  else if (is_reg ())
    {
      pp_space (pp);
      pp_left_paren (pp);
      if (mode != E_BLKmode)
	{
	  pp_string (pp, GET_MODE_NAME (mode));
	  pp_space (pp);
	}
      pp_string (pp, "pseudo");
      pp_right_paren (pp);
    }
}

// See the comment above the declaration.
void
resource_info::print (pretty_printer *pp) const
{
  print_identifier (pp);
  print_context (pp);
}

// Some properties can naturally be described using adjectives that attach
// to nouns like "use" or "definition".  Print such adjectives to PP.
void
access_info::print_prefix_flags (pretty_printer *pp) const
{
  if (m_is_temp)
    pp_string (pp, "temporary ");
  if (m_has_been_superceded)
    pp_string (pp, "superceded ");
}

// Print properties not handled by print_prefix_flags to PP, putting
// each property on a new line indented by two extra spaces.
void
access_info::print_properties_on_new_lines (pretty_printer *pp) const
{
  if (m_is_pre_post_modify)
    {
      pp_newline_and_indent (pp, 2);
      pp_string (pp, "set by a pre/post-modify");
      pp_indentation (pp) -= 2;
    }
  if (m_includes_address_uses)
    {
      pp_newline_and_indent (pp, 2);
      pp_string (pp, "appears inside an address");
      pp_indentation (pp) -= 2;
    }
  if (m_includes_read_writes)
    {
      pp_newline_and_indent (pp, 2);
      pp_string (pp, "appears in a read/write context");
      pp_indentation (pp) -= 2;
    }
  if (m_includes_subregs)
    {
      pp_newline_and_indent (pp, 2);
      pp_string (pp, "appears inside a subreg");
      pp_indentation (pp) -= 2;
    }
}

// Return true if there are no known issues with the integrity of the
// link information.
inline bool
use_info::check_integrity ()
{
  auto subsequence_id = [](use_info *use)
    {
      if (use->is_in_nondebug_insn ())
	return 1;
      if (use->is_in_debug_insn ())
	return 2;
      return 3;
    };

  use_info *prev = prev_use ();
  use_info *next = next_use ();

  if (prev && subsequence_id (prev) > subsequence_id (this))
    return false;
  if (next && subsequence_id (next) < subsequence_id (this))
    return false;
  if (m_is_last_nondebug_insn_use != calculate_is_last_nondebug_insn_use ())
    return false;

  if (!prev && last_use ()->next_use ())
    return false;
  if (!next)
    if (use_info *use = last_nondebug_insn_use ())
      if (!use->m_is_last_nondebug_insn_use)
	return false;

  return true;
}

// See the comment above the declaration.
void
use_info::print_location (pretty_printer *pp) const
{
  if (is_in_phi ())
    pp_access (pp, phi (), PP_ACCESS_INCLUDE_LOCATION);
  else
    insn ()->print_identifier_and_location (pp);
}

// See the comment above the declaration.
void
use_info::print_def (pretty_printer *pp) const
{
  if (const set_info *set = def ())
    pp_access (pp, set, 0);
  else
    {
      pp_string (pp, "undefined ");
      resource ().print (pp);
    }
}

// See the comment above the declaration.
void
use_info::print (pretty_printer *pp, unsigned int flags) const
{
  print_prefix_flags (pp);

  const set_info *set = def ();
  if (set && set->mode () != mode ())
    {
      pp_string (pp, GET_MODE_NAME (mode ()));
      pp_space (pp);
    }

  pp_string (pp, "use of ");
  print_def (pp);
  if (flags & PP_ACCESS_INCLUDE_LOCATION)
    {
      pp_string (pp, " by ");
      print_location (pp);
    }
  if (set && (flags & PP_ACCESS_INCLUDE_LINKS))
    {
      pp_newline_and_indent (pp, 2);
      pp_string (pp, "defined in ");
      set->insn ()->print_location (pp);
      pp_indentation (pp) -= 2;
    }
  if (flags & PP_ACCESS_INCLUDE_PROPERTIES)
    print_properties_on_new_lines (pp);
}

// See the comment above the declaration.
void
def_info::print_identifier (pretty_printer *pp) const
{
  resource ().print_identifier (pp);
  pp_colon (pp);
  insn ()->print_identifier (pp);
  resource ().print_context (pp);
}

// See the comment above the declaration.
void
def_info::print_location (pretty_printer *pp) const
{
  insn ()->print_identifier_and_location (pp);
}

// See the comment above the declaration.
void
clobber_info::print (pretty_printer *pp, unsigned int flags) const
{
  print_prefix_flags (pp);
  if (is_call_clobber ())
    pp_string (pp, "call ");
  pp_string (pp, "clobber ");
  print_identifier (pp);
  if (flags & PP_ACCESS_INCLUDE_LOCATION)
    {
      pp_string (pp, " in ");
      insn ()->print_location (pp);
    }
  if (flags & PP_ACCESS_INCLUDE_PROPERTIES)
    print_properties_on_new_lines (pp);
}

// See the comment above the declaration.
void
set_info::print_uses_on_new_lines (pretty_printer *pp) const
{
  for (const use_info *use : all_uses ())
    {
      pp_newline_and_indent (pp, 2);
      if (use->is_live_out_use ())
	{
	  pp_string (pp, "live out from ");
	  use->insn ()->print_location (pp);
	}
      else
	{
	  pp_string (pp, "used by ");
	  use->print_location (pp);
	}
      pp_indentation (pp) -= 2;
    }
  if (m_use_tree)
    {
      pp_newline_and_indent (pp, 2);
      pp_string (pp, "splay tree:");
      pp_newline_and_indent (pp, 2);
      auto print_use = [](pretty_printer *pp,
			  splay_tree_node<use_info *> *node)
	{
	  pp_string (pp, "use by ");
	  node->value ()->print_location (pp);
	};
      m_use_tree.print (pp, m_use_tree.root (), print_use);
      pp_indentation (pp) -= 4;
    }
}

// See the comment above the declaration.
void
set_info::print (pretty_printer *pp, unsigned int flags) const
{
  print_prefix_flags (pp);
  pp_string (pp, "set ");
  print_identifier (pp);
  if (flags & PP_ACCESS_INCLUDE_LOCATION)
    {
      pp_string (pp, " in ");
      insn ()->print_location (pp);
    }
  if (flags & PP_ACCESS_INCLUDE_PROPERTIES)
    print_properties_on_new_lines (pp);
  if (flags & PP_ACCESS_INCLUDE_LINKS)
    print_uses_on_new_lines (pp);
}

// See the comment above the declaration.
void
phi_info::print (pretty_printer *pp, unsigned int flags) const
{
  print_prefix_flags (pp);
  pp_string (pp, "phi node ");
  print_identifier (pp);
  if (flags & PP_ACCESS_INCLUDE_LOCATION)
    {
      pp_string (pp, " in ");
      insn ()->print_location (pp);
    }

  if (flags & PP_ACCESS_INCLUDE_PROPERTIES)
    print_properties_on_new_lines (pp);

  if (flags & PP_ACCESS_INCLUDE_LINKS)
    {
      basic_block cfg_bb = bb ()->cfg_bb ();
      pp_newline_and_indent (pp, 2);
      pp_string (pp, "inputs:");
      unsigned int i = 0;
      for (const use_info *input : inputs ())
	{
	  basic_block pred_cfg_bb = EDGE_PRED (cfg_bb, i)->src;
	  pp_newline_and_indent (pp, 2);
	  pp_string (pp, "bb");
	  pp_decimal_int (pp, pred_cfg_bb->index);
	  pp_colon (pp);
	  pp_space (pp);
	  input->print_def (pp);
	  pp_indentation (pp) -= 2;
	  i += 1;
	}
      pp_indentation (pp) -= 2;

      print_uses_on_new_lines (pp);
    }
}

// See the comment above the declaration.
void
set_node::print (pretty_printer *pp) const
{
  pp_access (pp, first_def ());
}

// See the comment above the declaration.
clobber_info *
clobber_group::prev_clobber (insn_info *insn) const
{
  auto &tree = const_cast<clobber_tree &> (m_clobber_tree);
  int comparison = lookup_clobber (tree, insn);
  if (comparison <= 0)
    return dyn_cast<clobber_info *> (tree.root ()->prev_def ());
  return tree.root ();
}

// See the comment above the declaration.
clobber_info *
clobber_group::next_clobber (insn_info *insn) const
{
  auto &tree = const_cast<clobber_tree &> (m_clobber_tree);
  int comparison = lookup_clobber (tree, insn);
  if (comparison >= 0)
    return dyn_cast<clobber_info *> (tree.root ()->next_def ());
  return tree.root ();
}

// See the comment above the declaration.
void
clobber_group::print (pretty_printer *pp) const
{
  auto print_clobber = [](pretty_printer *pp, const def_info *clobber)
    {
      pp_access (pp, clobber);
    };
  pp_string (pp, "grouped clobber");
  for (const def_info *clobber : clobbers ())
    {
      pp_newline_and_indent (pp, 2);
      print_clobber (pp, clobber);
      pp_indentation (pp) -= 2;
    }
  pp_newline_and_indent (pp, 2);
  pp_string (pp, "splay tree");
  pp_newline_and_indent (pp, 2);
  m_clobber_tree.print (pp, print_clobber);
  pp_indentation (pp) -= 4;
}

// See the comment above the declaration.
def_info *
def_lookup::prev_def (insn_info *insn) const
{
  if (mux && comparison == 0)
    if (auto *node = mux.dyn_cast<def_node *> ())
      if (auto *group = dyn_cast<clobber_group *> (node))
	if (clobber_info *clobber = group->prev_clobber (insn))
	  return clobber;

  return last_def_of_prev_group ();
}

// See the comment above the declaration.
def_info *
def_lookup::next_def (insn_info *insn) const
{
  if (mux && comparison == 0)
    if (auto *node = mux.dyn_cast<def_node *> ())
      if (auto *group = dyn_cast<clobber_group *> (node))
	if (clobber_info *clobber = group->next_clobber (insn))
	  return clobber;

  return first_def_of_next_group ();
}

// Return a clobber_group for CLOBBER, creating one if CLOBBER doesn't
// already belong to a group.
clobber_group *
function_info::need_clobber_group (clobber_info *clobber)
{
  if (clobber->is_in_group ())
    return clobber->group ();
  return allocate<clobber_group> (clobber);
}

// Return a def_node for inserting DEF into the associated resource's
// splay tree.  Use a clobber_group if DEF is a clobber and a set_node
// otherwise.
def_node *
function_info::need_def_node (def_info *def)
{
  if (auto *clobber = dyn_cast<clobber_info *> (def))
    return need_clobber_group (clobber);
  return allocate<set_node> (as_a<set_info *> (def));
}

// LAST is the last thing to define LAST->resource (), and is where any
// splay tree root for LAST->resource () is stored.  Require such a splay tree
// to exist, creating a new one if necessary.  Return the root of the tree.
//
// The caller must call LAST->set_splay_root after it has finished with
// the splay tree.
def_splay_tree
function_info::need_def_splay_tree (def_info *last)
{
  if (def_node *root = last->splay_root ())
    return root;

  // Use a left-spine rooted at the last node.
  def_node *root = need_def_node (last);
  def_node *parent = root;
  while (def_info *prev = first_def (parent)->prev_def ())
    {
      def_node *node = need_def_node (prev);
      def_splay_tree::insert_child (parent, 0, node);
      parent = node;
    }
  return root;
}

// Search TREE for either:
//
// - a set_info at INSN or
// - a clobber_group whose range includes INSN
//
// If such a node exists, install it as the root of TREE and return 0.
// Otherwise arbitrarily choose between:
//
// (1) Installing the closest preceding node as the root and returning 1.
// (2) Installing the closest following node as the root and returning -1.
//
// Note that this routine should not be used to check whether INSN
// itself defines a resource; that can be checked more cheaply using
// find_access_index.
int
rtl_ssa::lookup_def (def_splay_tree &tree, insn_info *insn)
{
  auto go_left = [&](def_node *node)
    {
      return *insn < *first_def (node)->insn ();
    };
  auto go_right = [&](def_node *node)
    {
      return *insn > *last_def (node)->insn ();
    };
  return tree.lookup (go_left, go_right);
}

// Search TREE for a clobber in INSN.  If such a clobber exists, install
// it as the root of TREE and return 0.  Otherwise arbitrarily choose between:
//
// (1) Installing the closest preceding clobber as the root and returning 1.
// (2) Installing the closest following clobber as the root and returning -1.
int
rtl_ssa::lookup_clobber (clobber_tree &tree, insn_info *insn)
{
  auto compare = [&](clobber_info *clobber)
    {
      return insn->compare_with (clobber->insn ());
    };
  return tree.lookup (compare);
}

// Search for a definition of RESOURCE at INSN and return the result of
// the search as a def_lookup.  See the comment above the class for more
// details.
def_lookup
function_info::find_def (resource_info resource, insn_info *insn)
{
  def_info *first = m_defs[resource.regno + 1];
  if (!first)
    // There are no nodes.  The comparison result is pretty meaningless
    // in this case.
    return { nullptr, -1 };

  // See whether the first node matches.
  auto first_result = clobber_group_or_single_def (first);
  if (*insn <= *last_def (first_result)->insn ())
    {
      int comparison = (*insn >= *first->insn () ? 0 : -1);
      return { first_result, comparison };
    }

  // See whether the last node matches.
  def_info *last = first->last_def ();
  auto last_result = clobber_group_or_single_def (last);
  if (*insn >= *first_def (last_result)->insn ())
    {
      int comparison = (*insn <= *last->insn () ? 0 : 1);
      return { last_result, comparison };
    }

  // Resort to using a splay tree to search for the result.
  def_splay_tree tree = need_def_splay_tree (last);
  int comparison = lookup_def (tree, insn);
  last->set_splay_root (tree.root ());
  return { tree.root (), comparison };
}

// Add DEF to the function's list of definitions of DEF->resource (),
// inserting DEF immediately before BEFORE.  DEF is not currently in the list.
void
function_info::insert_def_before (def_info *def, def_info *before)
{
  gcc_checking_assert (!def->has_def_links ()
		       && *before->insn () > *def->insn ());

  def->copy_prev_from (before);
  if (def_info *prev = def->prev_def ())
    {
      gcc_checking_assert (*prev->insn () < *def->insn ());
      prev->set_next_def (def);
    }
  else
    m_defs[def->regno () + 1] = def;

  def->set_next_def (before);
  before->set_prev_def (def);
}

// Add DEF to the function's list of definitions of DEF->resource (),
// inserting DEF immediately after AFTER.  DEF is not currently in the list.
void
function_info::insert_def_after (def_info *def, def_info *after)
{
  gcc_checking_assert (!def->has_def_links ()
		       && *after->insn () < *def->insn ());

  def->copy_next_from (after);
  if (def_info *next = def->next_def ())
    {
      gcc_checking_assert (*next->insn () > *def->insn ());
      next->set_prev_def (def);
    }
  else
    m_defs[def->regno () + 1]->set_last_def (def);

  def->set_prev_def (after);
  after->set_next_def (def);
}

// Remove DEF from the function's list of definitions of DEF->resource ().
void
function_info::remove_def_from_list (def_info *def)
{
  def_info *prev = def->prev_def ();
  def_info *next = def->next_def ();

  if (next)
    next->copy_prev_from (def);
  else
    m_defs[def->regno () + 1]->set_last_def (prev);

  if (prev)
    prev->copy_next_from (def);
  else
    m_defs[def->regno () + 1] = next;

  def->clear_def_links ();
}

// Add CLOBBER to GROUP and insert it into the function's list of
// accesses to CLOBBER->resource ().  CLOBBER is not currently part
// of an active group and is not currently in the list.
void
function_info::add_clobber (clobber_info *clobber, clobber_group *group)
{
  // Search for either the previous or next clobber in the group.
  // The result is less than zero if CLOBBER should come before NEIGHBOR
  // or greater than zero if CLOBBER should come after NEIGHBOR.
  int comparison = lookup_clobber (group->m_clobber_tree, clobber->insn ());
  gcc_checking_assert (comparison != 0);
  clobber_info *neighbor = group->m_clobber_tree.root ();

  // Since HEIGHBOR is now the root of the splay tree, its group needs
  // to be up-to-date.
  neighbor->update_group (group);

  // If CLOBBER comes before NEIGHBOR, insert CLOBBER to NEIGHBOR's left,
  // otherwise insert CLOBBER to NEIGHBOR's right.
  clobber_info::splay_tree::insert_child (neighbor, comparison > 0, clobber);
  clobber->set_group (group);

  // Insert the clobber into the function-wide list and update the
  // bounds of the group.
  if (comparison > 0)
    {
      insert_def_after (clobber, neighbor);
      if (neighbor == group->last_clobber ())
	group->set_last_clobber (clobber);
    }
  else
    {
      insert_def_before (clobber, neighbor);
      if (neighbor == group->first_clobber ())
	group->set_first_clobber (clobber);
    }
}

// Remove CLOBBER from GROUP, given that GROUP contains other clobbers too.
// Also remove CLOBBER from the function's list of accesses to
// CLOBBER->resource ().
void
function_info::remove_clobber (clobber_info *clobber, clobber_group *group)
{
  if (clobber == group->first_clobber ())
    {
      auto *new_first = as_a<clobber_info *> (clobber->next_def ());
      group->set_first_clobber (new_first);
      new_first->update_group (group);
    }
  else if (clobber == group->last_clobber ())
    {
      auto *new_last = as_a<clobber_info *> (clobber->prev_def ());
      group->set_last_clobber (new_last);
      new_last->update_group (group);
    }

  clobber_info *replacement = clobber_info::splay_tree::remove_node (clobber);
  if (clobber == group->m_clobber_tree.root ())
    {
      group->m_clobber_tree = replacement;
      replacement->update_group (group);
    }
  clobber->set_group (nullptr);

  remove_def_from_list (clobber);
}

// Add CLOBBER immediately before the first clobber in GROUP, given that
// CLOBBER is not currently part of any group.
void
function_info::prepend_clobber_to_group (clobber_info *clobber,
					 clobber_group *group)
{
  clobber_info *next = group->first_clobber ();
  clobber_info::splay_tree::insert_child (next, 0, clobber);
  group->set_first_clobber (clobber);
  clobber->set_group (group);
}

// Add CLOBBER immediately after the last clobber in GROUP, given that
// CLOBBER is not currently part of any group.
void
function_info::append_clobber_to_group (clobber_info *clobber,
					clobber_group *group)
{
  clobber_info *prev = group->last_clobber ();
  clobber_info::splay_tree::insert_child (prev, 1, clobber);
  group->set_last_clobber (clobber);
  clobber->set_group (group);
}

// Put CLOBBER1 and CLOBBER2 into the same clobber_group, given that
// CLOBBER1 occurs immediately before CLOBBER2 and that the two clobbers
// are not currently in the same group.  LAST is the last definition of
// the associated resource, and is where any splay tree is stored.
void
function_info::merge_clobber_groups (clobber_info *clobber1,
				     clobber_info *clobber2,
				     def_info *last)
{
  if (clobber1->is_in_group () && clobber2->is_in_group ())
    {
      clobber_group *group1 = clobber1->group ();
      clobber_group *group2 = clobber2->group ();
      gcc_checking_assert (clobber1 == group1->last_clobber ()
			   && clobber2 == group2->first_clobber ());

      if (def_splay_tree tree = last->splay_root ())
	{
	  // Remove GROUP2 from the splay tree.
	  int comparison = lookup_def (tree, clobber2->insn ());
	  gcc_checking_assert (comparison == 0);
	  tree.remove_root ();
	  last->set_splay_root (tree.root ());
	}

      // Splice the trees together.
      group1->m_clobber_tree.splice_next_tree (group2->m_clobber_tree);

      // Bring the two extremes of GROUP2 under GROUP1.  Any other
      // clobbers in the group are updated lazily on demand.
      clobber2->set_group (group1);
      group2->last_clobber ()->set_group (group1);
      group1->set_last_clobber (group2->last_clobber ());

      // Record that GROUP2 is no more.
      group2->set_first_clobber (nullptr);
      group2->set_last_clobber (nullptr);
      group2->m_clobber_tree = nullptr;
    }
  else
    {
      // In this case there can be no active splay tree.
      gcc_assert (!last->splay_root ());
      if (clobber2->is_in_group ())
	prepend_clobber_to_group (clobber1, clobber2->group ());
      else
	append_clobber_to_group (clobber2, need_clobber_group (clobber1));
    }
}

// GROUP spans INSN, and INSN now sets the resource that GROUP clobbers.
// Split GROUP around INSN, to form two new groups.  The first of the
// returned groups comes before INSN and the second comes after INSN.
//
// The caller is responsible for updating the def_splay_tree and chaining
// the defs together.
std::array<clobber_group *, 2>
function_info::split_clobber_group (clobber_group *group, insn_info *insn)
{
  // Search for either the previous or next clobber in the group.
  // The result is less than zero if CLOBBER should come before NEIGHBOR
  // or greater than zero if CLOBBER should come after NEIGHBOR.
  clobber_tree &tree1 = group->m_clobber_tree;
  int comparison = lookup_clobber (tree1, insn);
  gcc_checking_assert (comparison != 0);
  clobber_info *neighbor = tree1.root ();

  clobber_tree tree2;
  clobber_info *prev;
  clobber_info *next;
  if (comparison > 0)
    {
      // NEIGHBOR is the last clobber in what will become the first group.
      tree2 = tree1.split_after_root ();
      prev = neighbor;
      next = as_a<clobber_info *> (prev->next_def ());
    }
  else
    {
      // NEIGHBOR is the first clobber in what will become the second group.
      tree2 = neighbor;
      tree1 = tree2.split_before_root ();
      next = neighbor;
      prev = as_a<clobber_info *> (next->prev_def ());
    }

  // Create a new group for each side of the split.  We need to invalidate
  // the old group so that clobber_info::group can tell whether a lazy
  // update is needed.
  clobber_info *first_clobber = group->first_clobber ();
  clobber_info *last_clobber = group->last_clobber ();
  auto *group1 = allocate<clobber_group> (first_clobber, prev, tree1.root ());
  auto *group2 = allocate<clobber_group> (next, last_clobber, tree2.root ());

  // Invalidate the old group.
  group->set_last_clobber (nullptr);

  return { group1, group2 };
}

// Add DEF to the end of the function's list of definitions of
// DEF->resource ().  There is known to be no associated splay tree yet.
void
function_info::append_def (def_info *def)
{
  gcc_checking_assert (!def->has_def_links ());
  def_info **head = &m_defs[def->regno () + 1];
  def_info *first = *head;
  if (!first)
    {
      // This is the only definition of the resource.
      def->set_last_def (def);
      *head = def;
      return;
    }

  def_info *prev = first->last_def ();
  gcc_checking_assert (!prev->splay_root ());

  // Maintain the invariant that two clobbers must not appear in
  // neighboring nodes of the splay tree.
  auto *clobber = dyn_cast<clobber_info *> (def);
  auto *prev_clobber = dyn_cast<clobber_info *> (prev);
  if (clobber && prev_clobber)
    append_clobber_to_group (clobber, need_clobber_group (prev_clobber));

  prev->set_next_def (def);
  def->set_prev_def (prev);
  first->set_last_def (def);
}

// Add DEF to the function's list of definitions of DEF->resource ().
// Also insert it into the associated splay tree, if there is one.
// DEF is not currently part of the list and is not in the splay tree.
void
function_info::add_def (def_info *def)
{
  gcc_checking_assert (!def->has_def_links ()
		       && !def->m_is_temp
		       && !def->m_has_been_superceded);
  def_info **head = &m_defs[def->regno () + 1];
  def_info *first = *head;
  if (!first)
    {
      // This is the only definition of the resource.
      def->set_last_def (def);
      *head = def;
      return;
    }

  def_info *last = first->last_def ();
  insn_info *insn = def->insn ();

  int comparison;
  def_node *neighbor = nullptr;
  def_info *prev = nullptr;
  def_info *next = nullptr;
  if (*insn > *last->insn ())
    {
      // This definition comes after all other definitions.
      comparison = 1;
      if (def_splay_tree tree = last->splay_root ())
	{
	  tree.splay_max_node ();
	  last->set_splay_root (tree.root ());
	  neighbor = tree.root ();
	}
      prev = last;
    }
  else if (*insn < *first->insn ())
    {
      // This definition comes before all other definitions.
      comparison = -1;
      if (def_splay_tree tree = last->splay_root ())
	{
	  tree.splay_min_node ();
	  last->set_splay_root (tree.root ());
	  neighbor = tree.root ();
	}
      next = first;
    }
  else
    {
      // Search the splay tree for an insertion point.
      def_splay_tree tree = need_def_splay_tree (last);
      comparison = lookup_def (tree, insn);
      last->set_splay_root (tree.root ());
      neighbor = tree.root ();

      // Deal with cases in which we found an overlapping live range.
      if (comparison == 0)
	{
	  auto *group = as_a<clobber_group *> (tree.root ());
	  if (auto *clobber = dyn_cast<clobber_info *> (def))
	    {
	      add_clobber (clobber, group);
	      return;
	    }
	  auto new_groups = split_clobber_group (group, insn);

	  // Insert the two new groups immediately after GROUP.
	  def_splay_tree::insert_child (group, 1, new_groups[1]);
	  def_splay_tree::insert_child (group, 1, new_groups[0]);

	  // Remove GROUP.
	  tree.remove_root ();
	  last->set_splay_root (tree.root ());

	  prev = new_groups[0]->last_clobber ();
	  next = new_groups[1]->first_clobber ();

	  // DEF comes after the first group.  (new_groups[1] and -1 would
	  // also work.)
	  neighbor = new_groups[0];
	  comparison = 1;
	}
      // COMPARISON is < 0 if DEF comes before NEIGHBOR or > 0 if DEF comes
      // after NEIGHBOR.
      else if (comparison < 0)
	{
	  next = first_def (neighbor);
	  prev = next->prev_def ();
	}
      else
	{
	  prev = last_def (neighbor);
	  next = prev->next_def ();
	}
    }

  // See if we should merge CLOBBER with a neighboring clobber.
  auto *clobber = dyn_cast<clobber_info *> (def);
  auto *prev_clobber = safe_dyn_cast<clobber_info *> (prev);
  auto *next_clobber = safe_dyn_cast<clobber_info *> (next);
  // We shouldn't have consecutive clobber_groups.
  gcc_checking_assert (!(clobber && prev_clobber && next_clobber));
  if (clobber && prev_clobber)
    append_clobber_to_group (clobber, need_clobber_group (prev_clobber));
  else if (clobber && next_clobber)
    prepend_clobber_to_group (clobber, need_clobber_group (next_clobber));
  else if (neighbor)
    {
      // If DEF comes before NEIGHBOR, insert DEF to NEIGHBOR's left,
      // otherwise insert DEF to NEIGHBOR's right.
      def_node *node = need_def_node (def);
      def_splay_tree::insert_child (neighbor, comparison >= 0, node);
    }
  if (prev)
    insert_def_after (def, prev);
  else
    insert_def_before (def, next);
}

// Remove DEF from the function's list of definitions of DEF->resource ().
// Also remove DEF from the associated splay tree, if there is one.
void
function_info::remove_def (def_info *def)
{
  def_info **head = &m_defs[def->regno () + 1];
  def_info *first = *head;
  gcc_checking_assert (first);
  if (first->is_last_def ())
    {
      // DEF is the only definition of the resource.
      gcc_checking_assert (first == def);
      *head = nullptr;
      def->clear_def_links ();
      return;
    }

  // If CLOBBER belongs to a clobber_group that contains other clobbers
  // too, then we need to update the clobber_group and the list, but any
  // splay tree that contains the clobber_group is unaffected.
  if (auto *clobber = dyn_cast<clobber_info *> (def))
    if (clobber->is_in_group ())
      {
	clobber_group *group = clobber->group ();
	if (group->first_clobber () != group->last_clobber ())
	  {
	    remove_clobber (clobber, group);
	    return;
	  }
      }

  // If we've created a splay tree for this resource, remove the entry
  // for DEF.
  def_info *last = first->last_def ();
  if (def_splay_tree tree = last->splay_root ())
    {
      int comparison = lookup_def (tree, def->insn ());
      gcc_checking_assert (comparison == 0);
      tree.remove_root ();
      last->set_splay_root (tree.root ());
    }

  // If the definition came between two clobbers, merge them into a single
  // group.
  auto *prev_clobber = safe_dyn_cast<clobber_info *> (def->prev_def ());
  auto *next_clobber = safe_dyn_cast<clobber_info *> (def->next_def ());
  if (prev_clobber && next_clobber)
    merge_clobber_groups (prev_clobber, next_clobber, last);

  remove_def_from_list (def);
}

// Require DEF to have a splay tree that contains all non-phi uses.
void
function_info::need_use_splay_tree (set_info *def)
{
  if (!def->m_use_tree)
    for (use_info *use : def->all_insn_uses ())
      {
	auto *use_node = allocate<splay_tree_node<use_info *>> (use);
	def->m_use_tree.insert_max_node (use_node);
      }
}

// Compare two instructions by their position in a use splay tree.  Return >0
// if INSN1 comes after INSN2, <0 if INSN1 comes before INSN2, or 0 if they are
// the same instruction.
static inline int
compare_use_insns (insn_info *insn1, insn_info *insn2)
{
  // Debug instructions go after nondebug instructions.
  int diff = insn1->is_debug_insn () - insn2->is_debug_insn ();
  if (diff != 0)
    return diff;
  return insn1->compare_with (insn2);
}

// Search TREE for a use in INSN.  If such a use exists, install it as
// the root of TREE and return 0.  Otherwise arbitrarily choose between:
//
// (1) Installing the closest preceding use as the root and returning 1.
// (2) Installing the closest following use as the root and returning -1.
int
rtl_ssa::lookup_use (splay_tree<use_info *> &tree, insn_info *insn)
{
  auto compare = [&](splay_tree_node<use_info *> *node)
    {
      return compare_use_insns (insn, node->value ()->insn ());
    };
  return tree.lookup (compare);
}

// Add USE to USE->def ()'s list of uses. inserting USE immediately before
// BEFORE.  USE is not currently in the list.
//
// This routine should not be used for inserting phi uses.
void
function_info::insert_use_before (use_info *use, use_info *before)
{
  gcc_checking_assert (!use->has_use_links () && use->is_in_any_insn ());

  set_info *def = use->def ();

  use->copy_prev_from (before);
  use->set_next_use (before);

  if (use_info *prev = use->prev_use ())
    prev->set_next_use (use);
  else
    use->def ()->set_first_use (use);

  before->set_prev_use (use);
  if (use->is_in_nondebug_insn () && before->is_in_debug_insn_or_phi ())
    def->last_use ()->set_last_nondebug_insn_use (use);

  gcc_checking_assert (use->check_integrity () && before->check_integrity ());
}

// Add USE to USE->def ()'s list of uses. inserting USE immediately after
// AFTER.  USE is not currently in the list.
//
// This routine should not be used for inserting phi uses.
void
function_info::insert_use_after (use_info *use, use_info *after)
{
  set_info *def = use->def ();
  gcc_checking_assert (after->is_in_any_insn ()
		       && !use->has_use_links ()
		       && use->is_in_any_insn ());

  use->set_prev_use (after);
  use->copy_next_from (after);

  after->set_next_use (use);

  if (use_info *next = use->next_use ())
    {
      // The last node doesn't change, but we might need to update its
      // last_nondebug_insn_use record.
      if (use->is_in_nondebug_insn () && next->is_in_debug_insn_or_phi ())
	def->last_use ()->set_last_nondebug_insn_use (use);
      next->set_prev_use (use);
    }
  else
    {
      // USE is now the last node.
      if (use->is_in_nondebug_insn ())
	use->set_last_nondebug_insn_use (use);
      def->first_use ()->set_last_use (use);
    }

  gcc_checking_assert (use->check_integrity () && after->check_integrity ());
}

// If USE has a known definition, add USE to that definition's list of uses.
// Also update the associated splay tree, if any.
void
function_info::add_use (use_info *use)
{
  gcc_checking_assert (!use->has_use_links ()
		       && !use->m_is_temp
		       && !use->m_has_been_superceded);

  set_info *def = use->def ();
  if (!def)
    return;

  use_info *first = def->first_use ();
  if (!first)
    {
      // This is the only use of the definition.
      use->set_last_use (use);
      if (use->is_in_nondebug_insn ())
	use->set_last_nondebug_insn_use (use);

      def->set_first_use (use);

      gcc_checking_assert (use->check_integrity ());
      return;
    }

  if (use->is_in_phi ())
    {
      // Add USE at the end of the list, as the new first phi.
      use_info *last = first->last_use ();

      use->set_prev_use (last);
      use->copy_next_from (last);

      last->set_next_use (use);
      first->set_last_use (use);

      gcc_checking_assert (use->check_integrity ());
      return;
    }

  // If there is currently no splay tree for this definition, see if can
  // get away with a pure list-based update.
  insn_info *insn = use->insn ();
  auto quick_path = [&]()
    {
      // Check if USE should come before all current uses.
      if (first->is_in_phi () || compare_use_insns (insn, first->insn ()) < 0)
	{
	  insert_use_before (use, first);
	  return true;
	}

      // Check if USE should come after all current uses in the same
      // subsequence (i.e. the list of nondebug insn uses or the list
      // of debug insn uses).
      use_info *last = first->last_use ();
      if (use->is_in_debug_insn ())
	{
	  if (last->is_in_phi ())
	    return false;
	}
      else
	last = last->last_nondebug_insn_use ();

      if (compare_use_insns (insn, last->insn ()) > 0)
	{
	  insert_use_after (use, last);
	  return true;
	}

      return false;
    };
  if (!def->m_use_tree && quick_path ())
    return;

  // Search the splay tree for an insertion point.  COMPARISON is less
  // than zero if USE should come before NEIGHBOR, or greater than zero
  // if USE should come after NEIGHBOR.
  need_use_splay_tree (def);
  int comparison = lookup_use (def->m_use_tree, insn);
  gcc_checking_assert (comparison != 0);
  use_info *neighbor = def->m_use_tree.root ()->value ();

  // If USE comes before NEIGHBOR, insert USE to NEIGHBOR's left,
  // otherwise insert USE to NEIGHBOR's right.
  auto *use_node = allocate<splay_tree_node<use_info *>> (use);
  def->m_use_tree.insert_relative (comparison, use_node);
  if (comparison > 0)
    insert_use_after (use, neighbor);
  else
    insert_use_before (use, neighbor);
}

void
function_info::reparent_use (use_info *use, set_info *new_def)
{
  remove_use (use);
  use->set_def (new_def);
  add_use (use);
}

// If USE has a known definition, remove USE from that definition's list
// of uses.  Also remove if it from the associated splay tree, if any.
void
function_info::remove_use (use_info *use)
{
  set_info *def = use->def ();
  if (!def)
    return;

  // Remove USE from the splay tree.
  if (def->m_use_tree && use->is_in_any_insn ())
    {
      int comparison = lookup_use (def->m_use_tree, use->insn ());
      gcc_checking_assert (comparison == 0);
      def->m_use_tree.remove_root ();
    }

  use_info *prev = use->prev_use ();
  use_info *next = use->next_use ();

  use_info *first = def->first_use ();
  use_info *last = first->last_use ();
  if (last->last_nondebug_insn_use () == use)
    last->set_last_nondebug_insn_use (prev);

  if (next)
    next->copy_prev_from (use);
  else
    first->set_last_use (prev);

  if (prev)
    prev->copy_next_from (use);
  else
    def->set_first_use (next);

  use->clear_use_links ();
  gcc_checking_assert ((!prev || prev->check_integrity ())
		       && (!next || next->check_integrity ()));
}

// Allocate a temporary clobber_info for register REGNO in insn INSN,
// including it in the region of the obstack governed by WATERMARK.
// Return a new def_array that contains OLD_DEFS and the new clobber.
//
// OLD_DEFS is known not to define REGNO.
def_array
function_info::insert_temp_clobber (obstack_watermark &watermark,
				    insn_info *insn, unsigned int regno,
				    def_array old_defs)
{
  gcc_checking_assert (watermark == &m_temp_obstack);
  auto *clobber = allocate_temp<clobber_info> (insn, regno);
  clobber->m_is_temp = true;
  return insert_access (watermark, clobber, old_defs);
}

// See the comment above the declaration.
bool
function_info::remains_available_at_insn (const set_info *set,
					  insn_info *insn)
{
  auto *ebb = set->ebb ();
  gcc_checking_assert (ebb == insn->ebb ());

  def_info *next_def = set->next_def ();
  if (next_def && *next_def->insn () < *insn)
    return false;

  if (HARD_REGISTER_NUM_P (set->regno ())
      && TEST_HARD_REG_BIT (m_clobbered_by_calls, set->regno ()))
    for (ebb_call_clobbers_info *call_group : ebb->call_clobbers ())
      {
	if (!call_group->clobbers (set->resource ()))
	  continue;

	insn_info *call_insn = next_call_clobbers (*call_group, insn);
	if (call_insn && *call_insn < *insn)
	  return false;
      }

  return true;
}

// See the comment above the declaration.
bool
function_info::remains_available_on_exit (const set_info *set, bb_info *bb)
{
  if (HARD_REGISTER_NUM_P (set->regno ())
      && TEST_HARD_REG_BIT (m_clobbered_by_calls, set->regno ()))
    {
      insn_info *search_insn = (set->bb () == bb
				? set->insn ()
				: bb->head_insn ());
      for (ebb_call_clobbers_info *call_group : bb->ebb ()->call_clobbers ())
	{
	  if (!call_group->clobbers (set->resource ()))
	    continue;

	  insn_info *insn = next_call_clobbers (*call_group, search_insn);
	  if (insn && insn->bb () == bb)
	    return false;
	}
    }

  return (set->is_last_def ()
	  || *set->next_def ()->insn () > *bb->end_insn ());
}

// A subroutine of make_uses_available.  Try to make USE's definition
// available at the head of BB.  WILL_BE_DEBUG_USE is true if the
// definition will be used only in debug instructions.
//
// On success:
//
// - If the use would have the same def () as USE, return USE.
//
// - If BB already has a degenerate phi for the same definition,
//   return a temporary use of that phi.
//
// - Otherwise, the use would need a new degenerate phi.  Allocate a
//   temporary phi and return a temporary use of it.
//
// Return null on failure.
use_info *
function_info::make_use_available (use_info *use, bb_info *bb,
				   bool will_be_debug_use)
{
  set_info *def = use->def ();
  if (!def)
    return use;

  if (is_single_dominating_def (def))
    return use;

  if (def->ebb () == bb->ebb ())
    {
      if (remains_available_at_insn (def, bb->head_insn ()))
	return use;
      return nullptr;
    }

  basic_block cfg_bb = bb->cfg_bb ();
  bb_info *use_bb = use->bb ();
  if (single_pred_p (cfg_bb)
      && single_pred (cfg_bb) == use_bb->cfg_bb ()
      && remains_available_on_exit (def, use_bb))
    {
      if (will_be_debug_use)
	return use;

      resource_info resource = use->resource ();
      set_info *ultimate_def = look_through_degenerate_phi (def);

      // See if there is already a (degenerate) phi for DEF.
      insn_info *phi_insn = bb->ebb ()->phi_insn ();
      phi_info *phi;
      def_lookup dl = find_def (resource, phi_insn);
      if (set_info *set = dl.matching_set ())
	{
	  // There is an existing phi.
	  phi = as_a<phi_info *> (set);
	  gcc_checking_assert (phi->input_value (0) == ultimate_def);
	}
      else
	{
	  // Create a temporary placeholder phi.  This will become
	  // permanent if the change is later committed.
	  phi = allocate_temp<phi_info> (phi_insn, resource, 0);
	  auto *input = allocate_temp<use_info> (phi, resource, ultimate_def);
	  input->m_is_temp = true;
	  phi->m_is_temp = true;
	  phi->make_degenerate (input);
	  if (def_info *prev = dl.prev_def (phi_insn))
	    phi->set_prev_def (prev);
	  if (def_info *next = dl.next_def (phi_insn))
	    phi->set_next_def (next);
	}

      // Create a temporary use of the phi at the head of the first
      // block, since we know for sure that it's available there.
      insn_info *use_insn = bb->ebb ()->first_bb ()->head_insn ();
      auto *new_use = allocate_temp<use_info> (use_insn, resource, phi);
      new_use->m_is_temp = true;
      return new_use;
    }
  return nullptr;
}

// See the comment above the declaration.
use_array
function_info::make_uses_available (obstack_watermark &watermark,
				    use_array uses, bb_info *bb,
				    bool will_be_debug_uses)
{
  unsigned int num_uses = uses.size ();
  if (num_uses == 0)
    return uses;

  auto **new_uses = XOBNEWVEC (watermark, access_info *, num_uses);
  for (unsigned int i = 0; i < num_uses; ++i)
    {
      use_info *use = make_use_available (uses[i], bb, will_be_debug_uses);
      if (!use)
	return use_array (access_array::invalid ());
      new_uses[i] = use;
    }
  return use_array (new_uses, num_uses);
}

set_info *
function_info::create_set (obstack_watermark &watermark,
			   insn_info *insn,
			   resource_info resource)
{
  auto set = change_alloc<set_info> (watermark, insn, resource);
  set->m_is_temp = true;
  return set;
}

use_info *
function_info::create_use (obstack_watermark &watermark,
			   insn_info *insn,
			   set_info *set)
{
  auto use = change_alloc<use_info> (watermark, insn, set->resource (), set);
  use->m_is_temp = true;
  return use;
}

// Return true if ACCESS1 can represent ACCESS2 and if ACCESS2 can
// represent ACCESS1.
static bool
can_merge_accesses (access_info *access1, access_info *access2)
{
  if (access1 == access2)
    return true;

  auto *use1 = dyn_cast<use_info *> (access1);
  auto *use2 = dyn_cast<use_info *> (access2);
  return use1 && use2 && use1->def () == use2->def ();
}

// See the comment above the declaration.
access_array
rtl_ssa::merge_access_arrays_base (obstack_watermark &watermark,
				   access_array accesses1,
				   access_array accesses2)
{
  if (accesses1.empty ())
    return accesses2;
  if (accesses2.empty ())
    return accesses1;

  auto i1 = accesses1.begin ();
  auto end1 = accesses1.end ();
  auto i2 = accesses2.begin ();
  auto end2 = accesses2.end ();

  access_array_builder builder (watermark);
  builder.reserve (accesses1.size () + accesses2.size ());

  while (i1 != end1 && i2 != end2)
    {
      access_info *access1 = *i1;
      access_info *access2 = *i2;

      unsigned int regno1 = access1->regno ();
      unsigned int regno2 = access2->regno ();
      if (regno1 == regno2)
	{
	  if (!can_merge_accesses (access1, access2))
	    return access_array::invalid ();

	  builder.quick_push (access1);
	  ++i1;
	  ++i2;
	}
      else if (regno1 < regno2)
	{
	  builder.quick_push (access1);
	  ++i1;
	}
      else
	{
	  builder.quick_push (access2);
	  ++i2;
	}
    }
  for (; i1 != end1; ++i1)
    builder.quick_push (*i1);
  for (; i2 != end2; ++i2)
    builder.quick_push (*i2);

  return builder.finish ();
}

// See the comment above the declaration.
access_array
rtl_ssa::insert_access_base (obstack_watermark &watermark,
			     access_info *access1, access_array accesses2)
{
  access_array_builder builder (watermark);
  builder.reserve (1 + accesses2.size ());

  unsigned int regno1 = access1->regno ();
  auto i2 = accesses2.begin ();
  auto end2 = accesses2.end ();
  while (i2 != end2)
    {
      access_info *access2 = *i2;

      unsigned int regno2 = access2->regno ();
      if (regno1 == regno2)
	{
	  if (!can_merge_accesses (access1, access2))
	    return access_array::invalid ();

	  builder.quick_push (access1);
	  access1 = nullptr;
	  ++i2;
	  break;
	}
      else if (regno1 < regno2)
	{
	  builder.quick_push (access1);
	  access1 = nullptr;
	  break;
	}
      else
	{
	  builder.quick_push (access2);
	  ++i2;
	}
    }
  if (access1)
    builder.quick_push (access1);
  for (; i2 != end2; ++i2)
    builder.quick_push (*i2);

  return builder.finish ();
}

// See the comment above the declaration.
use_array
rtl_ssa::remove_uses_of_def (obstack_watermark &watermark, use_array uses,
			     def_info *def)
{
  access_array_builder uses_builder (watermark);
  uses_builder.reserve (uses.size ());
  for (use_info *use : uses)
    if (use->def () != def)
      uses_builder.quick_push (use);
  return use_array (uses_builder.finish ());
}

// See the comment above the declaration.
access_array
rtl_ssa::remove_note_accesses_base (obstack_watermark &watermark,
				    access_array accesses)
{
  auto predicate = [](access_info *a) {
    return !a->only_occurs_in_notes ();
  };

  for (access_info *access : accesses)
    if (access->only_occurs_in_notes ())
      return filter_accesses (watermark, accesses, predicate);

  return accesses;
}

// See the comment above the declaration.
bool
rtl_ssa::accesses_reference_same_resource (access_array accesses1,
					   access_array accesses2)
{
  auto i1 = accesses1.begin ();
  auto end1 = accesses1.end ();
  auto i2 = accesses2.begin ();
  auto end2 = accesses2.end ();

  while (i1 != end1 && i2 != end2)
    {
      access_info *access1 = *i1;
      access_info *access2 = *i2;

      unsigned int regno1 = access1->regno ();
      unsigned int regno2 = access2->regno ();
      if (regno1 == regno2)
	return true;

      if (regno1 < regno2)
	++i1;
      else
	++i2;
    }
  return false;
}

// See the comment above the declaration.
bool
rtl_ssa::insn_clobbers_resources (insn_info *insn, access_array accesses)
{
  if (accesses_reference_same_resource (insn->defs (), accesses))
    return true;

  if (insn->is_call () && accesses_include_hard_registers (accesses))
    {
      function_abi abi = insn_callee_abi (insn->rtl ());
      for (const access_info *access : accesses)
	{
	  if (!HARD_REGISTER_NUM_P (access->regno ()))
	    break;
	  if (abi.clobbers_reg_p (access->mode (), access->regno ()))
	    return true;
	}
    }

  return false;
}

// Print RESOURCE to PP.
void
rtl_ssa::pp_resource (pretty_printer *pp, resource_info resource)
{
  resource.print (pp);
}

// Print ACCESS to PP.  FLAGS is a bitmask of PP_ACCESS_* flags.
void
rtl_ssa::pp_access (pretty_printer *pp, const access_info *access,
		    unsigned int flags)
{
  if (!access)
    pp_string (pp, "<null>");
  else if (auto *phi = dyn_cast<const phi_info *> (access))
    phi->print (pp, flags);
  else if (auto *set = dyn_cast<const set_info *> (access))
    set->print (pp, flags);
  else if (auto *clobber = dyn_cast<const clobber_info *> (access))
    clobber->print (pp, flags);
  else if (auto *use = dyn_cast<const use_info *> (access))
    use->print (pp, flags);
  else
    pp_string (pp, "??? Unknown access");
}

// Print ACCESSES to PP.  FLAGS is a bitmask of PP_ACCESS_* flags.
void
rtl_ssa::pp_accesses (pretty_printer *pp, access_array accesses,
		      unsigned int flags)
{
  if (accesses.empty ())
    pp_string (pp, "none");
  else
    {
      bool is_first = true;
      for (access_info *access : accesses)
	{
	  if (is_first)
	    is_first = false;
	  else
	    pp_newline_and_indent (pp, 0);
	  pp_access (pp, access, flags);
	}
    }
}

// Print NODE to PP.
void
rtl_ssa::pp_def_node (pretty_printer *pp, const def_node *node)
{
  if (!node)
    pp_string (pp, "<null>");
  else if (auto *group = dyn_cast<const clobber_group *> (node))
    group->print (pp);
  else if (auto *set = dyn_cast<const set_node *> (node))
    set->print (pp);
  else
    pp_string (pp, "??? Unknown def node");
}

// Print MUX to PP.
void
rtl_ssa::pp_def_mux (pretty_printer *pp, def_mux mux)
{
  if (auto *node = mux.dyn_cast<def_node *> ())
    pp_def_node (pp, node);
  else
    pp_access (pp, mux.as_a<def_info *> ());
}

// Print DL to PP.
void
rtl_ssa::pp_def_lookup (pretty_printer *pp, def_lookup dl)
{
  pp_string (pp, "comparison result of ");
  pp_decimal_int (pp, dl.comparison);
  pp_string (pp, " for ");
  pp_newline_and_indent (pp, 0);
  pp_def_mux (pp, dl.mux);
}

// Print TREE to PP.
void
rtl_ssa::pp_def_splay_tree (pretty_printer *pp, def_splay_tree tree)
{
  tree.print (pp, pp_def_node);
}

// Dump RESOURCE to FILE.
void
dump (FILE *file, resource_info resource)
{
  dump_using (file, pp_resource, resource);
}

// Dump ACCESS to FILE.  FLAGS is a bitmask of PP_ACCESS_* flags.
void
dump (FILE *file, const access_info *access, unsigned int flags)
{
  dump_using (file, pp_access, access, flags);
}

// Dump ACCESSES to FILE.  FLAGS is a bitmask of PP_ACCESS_* flags.
void
dump (FILE *file, access_array accesses, unsigned int flags)
{
  dump_using (file, pp_accesses, accesses, flags);
}

// Print NODE to FILE.
void
dump (FILE *file, const def_node *node)
{
  dump_using (file, pp_def_node, node);
}

// Print MUX to FILE.
void
dump (FILE *file, def_mux mux)
{
  dump_using (file, pp_def_mux, mux);
}

// Print RESULT to FILE.
void
dump (FILE *file, def_lookup result)
{
  dump_using (file, pp_def_lookup, result);
}

// Print TREE to FILE.
void
dump (FILE *file, def_splay_tree tree)
{
  dump_using (file, pp_def_splay_tree, tree);
}

// Debug interfaces to the dump routines above.
void debug (const resource_info &x) { dump (stderr, x); }
void debug (const access_info *x) { dump (stderr, x); }
void debug (const access_array &x) { dump (stderr, x); }
void debug (const def_node *x) { dump (stderr, x); }
void debug (const def_mux &x) { dump (stderr, x); }
void debug (const def_lookup &x) { dump (stderr, x); }
void debug (const def_splay_tree &x) { dump (stderr, x); }
