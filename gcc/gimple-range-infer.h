/* Header file for gimple range inference.
   Copyright (C) 2022-2024 Free Software Foundation, Inc.
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
//
// There are 2 main instantiations.
// gimple_range_infer (gimple *s, range_query *q, bool use_range_ops)
//   S is the statement being queried.
//   Q is a range-query object which is used to resolve any ranges that
//      might be required.  This defaults to NULL which maps to the
//      global_range_query, which is what most passes will want.
//      Ranger internally will pass the cache's range-query which is a
//      read-only query and prevents any additional lookup.
//   USE_RANGEOPS is a boolean flag which defaults to false.  if TRUE,
//      range-ops is invoked to see if any additional side effects are seen
//      based on the stmt.  ie .x = y * 2 will reigster a side effect for Y
//      which is [-INF/2 , +INF/2].  It is not on by default because it
//      is a relatively expensive operation to do on every statement, and
//      ranger will already incorporate that range for Y via GORI most of the
//      time that it matters.  Individual passes may have use for it however.
//      PR 113879 is an example where this can be of use.
//
// gimple_range_infer (tree name, vrange &r)
// This instantiation simply create an inferred range record directly.
//   NAME is the SSA_NAME to create the record for
//   R is the range for NAME.
//
// Once a gimple_infer_range record has been created, the API is simple:
//   num ()    - The number of inferred ranges in this record.
//   name (i)  - The i'th SSA_NAME in this record.
//   range (i) - The range of the i'th SSA_NAME.

class gimple_infer_range
{
public:
  gimple_infer_range (gimple *s, range_query *q = NULL,
		      bool use_rangeops = false);
  gimple_infer_range (tree name, vrange &r);
  inline unsigned num () const { return num_args; }
  inline tree name (unsigned index) const
    { gcc_checking_assert (index < num_args); return m_names[index]; }
  inline const vrange& range (unsigned index) const
    { gcc_checking_assert (index < num_args); return m_ranges[index]; }
private:
  void add_range (tree name, vrange &range);
  void add_nonzero (tree name);
  void check_assume_func (gcall *call);
  unsigned num_args;
  static const int size_limit = 10;
  tree m_names[size_limit];
  value_range m_ranges[size_limit];
  inline void bump_index () { if (num_args < size_limit - 1) num_args++; }
  friend class non_null_wrapper;
};

// This is the basic infer oracle API.  Default functionaility does nothing.

class infer_range_oracle
{
public:
  infer_range_oracle () { }
  virtual ~infer_range_oracle () { }
  virtual void add_ranges (gimple *, gimple_infer_range &) { }
  virtual bool has_range_p (basic_block, tree = NULL_TREE) { return false; }
  virtual bool maybe_adjust_range (vrange &, tree, basic_block)
      { return false; }
};

// This class manages a list of inferred ranges for each basic block.
// As inferences are made, they can be registered to a block and later
// queried via a DOM search.
// When DO_SEARCH is TRUE,  immediate uses chains are followed the first time
// a name is referenced and block populated if there are any inferred ranges.
// range_query Q is the range_query to use for any range lookups. It defaults
// to NULL which maps to the global_range_query.  This is what most passes
// will want to use. Ranger invokes it with the cache's internal query which
// can provide better ranges during a DOM walk.
//
// add_ranges is used to add inferred range IR assocaited with stmt S.
// has_range_p is used to check if NAME has an inferred range in block BB.
// maybe_adjust_range will adjust the range R to incorporate any inferred
//   range NAME may have in block BB.  If there are on inferred ranges in
//   block BB, then R will be unchanged, otherwise the ranges are intersected.

class infer_range_manager : public infer_range_oracle
{
public:
  infer_range_manager (bool do_search, range_query *q = NULL);
  virtual ~infer_range_manager ();
  virtual void add_ranges (gimple *s, gimple_infer_range &ir);
  virtual bool has_range_p (basic_block bb, tree name = NULL_TREE);
  virtual bool maybe_adjust_range (vrange &r, tree name, basic_block bb);
private:
  void add_range (tree name, gimple *s, const vrange &r);
  void add_nonzero (tree name, gimple *s);
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
  class vrange_allocator *m_range_allocator;
  range_query *m_query;
};
#endif // GCC_GIMPLE_RANGE_SIDE_H
