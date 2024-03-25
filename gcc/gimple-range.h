/* Header file for the GIMPLE range interface.
   Copyright (C) 2019-2024 Free Software Foundation, Inc.
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

#ifndef GCC_GIMPLE_RANGE_H
#define GCC_GIMPLE_RANGE_H

#include "ssa.h"
#include "range.h"
#include "value-query.h"
#include "gimple-range-op.h"
#include "gimple-range-trace.h"
#include "gimple-range-edge.h"
#include "gimple-range-fold.h"
#include "gimple-range-gori.h"
#include "gimple-range-cache.h"

// This is the basic range generator interface.
//
// This base class provides all the API entry points, but only provides
// functionality at the statement level.  Ie, it can calculate ranges on
// statements, but does no additional lookup.
//
// All the range_of_* methods will return a range if the types is
// supported by the range engine.  It may be the full range for the
// type, AKA varying_p or it may be a refined range.  If the range
// type is not supported, then false is returned.  Non-statement
// related methods return whatever the current global value is.

class gimple_ranger : public range_query
{
public:
  gimple_ranger (bool use_imm_uses = true);
  ~gimple_ranger ();
  virtual bool range_of_stmt (vrange &r, gimple *, tree name = NULL) override;
  virtual bool range_of_expr (vrange &r, tree name, gimple * = NULL) override;
  virtual bool range_on_edge (vrange &r, edge e, tree name) override;
  void range_on_entry (vrange &r, basic_block bb, tree name);
  void range_on_exit (vrange &r, basic_block bb, tree name);
  void export_global_ranges ();
  inline gori_compute &gori ()  { return m_cache.m_gori; }
  virtual void dump (FILE *f) override;
  void debug ();
  void dump_bb (FILE *f, basic_block bb);
  auto_edge_flag non_executable_edge_flag;
  bool fold_stmt (gimple_stmt_iterator *gsi, tree (*) (tree));
  void register_inferred_ranges (gimple *s);
  void register_transitive_inferred_ranges (basic_block bb);
  range_query &const_query ();
protected:
  bool fold_range_internal (vrange &r, gimple *s, tree name);
  void prefill_name (vrange &r, tree name);
  void prefill_stmt_dependencies (tree ssa);
  ranger_cache m_cache;
  range_tracer tracer;
  basic_block current_bb;
  vec<tree> m_stmt_list;
  friend class path_range_query;
};

/* Create a new ranger instance and associate it with a function.
   Each call must be paired with a call to disable_ranger to release
   resources.  If USE_IMM_USES is true, pre-calculate side effects like
   non-null uses as required using the immediate use chains.  */
extern gimple_ranger *enable_ranger (struct function *m,
				     bool use_imm_uses = true);
extern void disable_ranger (struct function *);

class assume_query : public range_query
{
public:
  assume_query ();
  bool assume_range_p (vrange &r, tree name);
  virtual bool range_of_expr (vrange &r, tree expr, gimple * = NULL);
  void dump (FILE *f);
protected:
  void calculate_stmt (gimple *s, vrange &lhs_range, fur_source &src);
  void calculate_op (tree op, gimple *s, vrange &lhs, fur_source &src);
  void calculate_phi (gphi *phi, vrange &lhs_range, fur_source &src);
  void check_taken_edge (edge e, fur_source &src);

  ssa_lazy_cache global;
  gori_compute m_gori;
};

// DOM based ranger for fast VRP.
// This must be processed in DOM order, and does only basic range operations.

class dom_ranger : public range_query
{
public:
  dom_ranger ();
  ~dom_ranger ();

  virtual bool range_of_expr (vrange &r, tree expr, gimple *s = NULL) override;
  virtual bool range_on_edge (vrange &r, edge e, tree expr) override;
  virtual bool range_of_stmt (vrange &r, gimple *s, tree name = NULL) override;

  bool edge_range (vrange &r, edge e, tree name);
  void range_in_bb (vrange &r, basic_block bb, tree name);

  void pre_bb (basic_block bb);
  void post_bb (basic_block bb);
protected:
  DISABLE_COPY_AND_ASSIGN (dom_ranger);
  void maybe_push_edge (edge e, bool edge_0);
  ssa_cache m_global;
  gimple_outgoing_range m_out;
  vec<ssa_lazy_cache *> m_freelist;
  vec<ssa_lazy_cache *> m_e0;
  vec<ssa_lazy_cache *> m_e1;
  bitmap m_pop_list;
  range_tracer tracer;
};
#endif // GCC_GIMPLE_RANGE_H
