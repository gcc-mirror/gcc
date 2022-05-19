/* Header file for gimple range side effects.
   Copyright (C) 2022 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_GIMPLE_RANGE_SIDE_H
#define GCC_GIMPLE_RANGE_SIDE_H

// This class manages an on-demand summary of side effects for a statement.
// It can be instantiated as required and provides a list of side effects.

// New side effects should added in the constructor of this class.

class stmt_side_effects
{
public:
  stmt_side_effects (gimple *s);
  inline unsigned num () const { return num_args; }
  inline tree name (unsigned index) const
    { gcc_checking_assert (index < num_args); return m_names[index]; }
  inline const irange& range (unsigned index) const
    { gcc_checking_assert (index < num_args); return m_ranges[index]; }
  void add_range (tree name, irange &range);
  void add_nonzero (tree name);
private:
  unsigned num_args;
  static const int size_limit = 10;
  tree m_names[size_limit];
  int_range<3> m_ranges[size_limit];
  inline void bump_index () { if (num_args < size_limit - 1) num_args++; }
};

// This class manages a list of side effect ranges for each basic block.
// As side effects are seen, they can be registered to a block and later
// queried.  WHen constructed with a TRUE flag, immediate uses chains are
// followed the first time a name is referenced and block populated if
// thre are any side effects.

class side_effect_manager
{
public:
  side_effect_manager (bool do_search);
  ~side_effect_manager ();
  void add_range (tree name, basic_block bb, const irange &r);
  void add_nonzero (tree name, basic_block bb);
  bool has_range_p (tree name, basic_block bb);
  bool maybe_adjust_range (irange &r, tree name, basic_block bb);
private:
  class exit_range_head
  {
  public:
    bitmap m_names;		// list of names with an outgoing range.
    class exit_range *head;
    int m_num_ranges;
    exit_range *find_ptr (tree name);
  };
  void register_all_uses (tree name);
  vec <exit_range_head> m_on_exit;
  const irange &get_nonzero (tree name);
  vec <irange *> m_nonzero;
  bitmap m_seen;
  bitmap_obstack m_bitmaps;
  struct obstack m_list_obstack;
  irange_allocator m_range_allocator;
};

#endif // GCC_GIMPLE_RANGE_SIDE_H
