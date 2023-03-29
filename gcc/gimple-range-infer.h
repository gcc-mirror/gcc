/* Header file for gimple range inference.
   Copyright (C) 2022-2023 Free Software Foundation, Inc.
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

// Inferred ranges are ranges which are applied to use operands as a by product
// of executing an operation.

// This class manages an on-demand summary of inferred ranges for a statement.
// It can be instantiated as required and provides a list of inferred ranges.
// New inferred ranges should be added in the constructor of this class.

class gimple_infer_range
{
public:
  gimple_infer_range (gimple *s);
  inline unsigned num () const { return num_args; }
  inline tree name (unsigned index) const
    { gcc_checking_assert (index < num_args); return m_names[index]; }
  inline const vrange& range (unsigned index) const
    { gcc_checking_assert (index < num_args); return m_ranges[index]; }
  void add_range (tree name, vrange &range);
  void add_nonzero (tree name);
private:
  void check_assume_func (gcall *call);
  unsigned num_args;
  static const int size_limit = 10;
  tree m_names[size_limit];
  Value_Range m_ranges[size_limit];
  inline void bump_index () { if (num_args < size_limit - 1) num_args++; }
};

// This class manages a list of inferred ranges for each basic block.
// As inferences are made, they can be registered to a block and later
// queried.  When constructed with a TRUE flag, immediate uses chains are
// followed the first time a name is referenced and block populated if
// there are any inferred ranges.

class infer_range_manager
{
public:
  infer_range_manager (bool do_search);
  ~infer_range_manager ();
  void add_range (tree name, basic_block bb, const vrange &r);
  void add_nonzero (tree name, basic_block bb);
  bool has_range_p (tree name, basic_block bb);
  bool has_range_p (basic_block bb);
  bool maybe_adjust_range (vrange &r, tree name, basic_block bb);
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
  const vrange &get_nonzero (tree name);
  vec <vrange *> m_nonzero;
  bitmap m_seen;
  bitmap_obstack m_bitmaps;
  struct obstack m_list_obstack;
  class obstack_vrange_allocator *m_range_allocator;
};

#endif // GCC_GIMPLE_RANGE_SIDE_H
