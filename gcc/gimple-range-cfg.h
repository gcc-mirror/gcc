/* Header file for the gimple_ranger class.
   Copyright (C) 2017-2020 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>
   and Aldy Hernandez <aldyh@redhat.com>.

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

#ifndef GCC_GIMPLE_RANGE_CFG_H
#define GCC_GIMPLE_RANGE_CFG_H

class gimple_ranger : public gori_compute_cache
{
public:
  virtual bool range_of_stmt (irange &r, gimple *s, tree name = NULL_TREE);
  virtual void range_on_edge (irange &r, edge e, tree name);

  virtual void range_on_entry (irange &r, basic_block bb, tree name);
  virtual void range_on_exit (irange &r, basic_block bb, tree name);
  virtual bool range_of_phi (irange &r, gphi *phi);
protected:
  bool range_of_range_op (irange &r, gimple *s);
  bool range_of_call (irange &r, gcall *call);
  bool range_of_cond_expr (irange &r, gassign* cond);
private:
  bool range_of_non_trivial_assignment (irange &r, gimple *s);
  bool range_of_builtin_call (irange &r, gcall *call);
  void range_of_builtin_ubsan_call (irange &r, gcall *call, tree_code code);
};

#endif // GCC_GIMPLE_RANGE_CFG_H
