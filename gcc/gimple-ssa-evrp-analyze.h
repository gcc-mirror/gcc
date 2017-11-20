/* Support routines for Value Range Propagation (VRP).
   Copyright (C) 2016-2017 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_GIMPLE_SSA_EVRP_ANALYZE_H
#define GCC_GIMPLE_SSA_EVRP_ANALYZE_H

class evrp_range_analyzer
{
 public:
  evrp_range_analyzer (void);
  ~evrp_range_analyzer (void) { stack.release (); }

  void enter (basic_block);
  void leave (basic_block);
  void record_ranges_from_stmt (gimple *);

  class vr_values vr_values;

 private:
  DISABLE_COPY_AND_ASSIGN (evrp_range_analyzer);
  void push_value_range (tree var, value_range *vr);
  value_range *pop_value_range (tree var);
  value_range *try_find_new_range (tree, tree op, tree_code code, tree limit);
  void record_ranges_from_incoming_edge (basic_block);
  void record_ranges_from_phis (basic_block);

  /* STACK holds the old VR.  */
  auto_vec<std::pair <tree, value_range*> > stack;

  /* Temporary delegators.  */
  value_range *get_value_range (const_tree op)
    { return vr_values.get_value_range (op); }
  bool update_value_range (const_tree op, value_range *vr)
    { return vr_values.update_value_range (op, vr); }
  void extract_range_from_phi_node (gphi *phi, value_range *vr)
    { vr_values.extract_range_from_phi_node (phi, vr); }
  void adjust_range_with_scev (value_range *vr, struct loop *loop,
                               gimple *stmt, tree var)
    { vr_values.adjust_range_with_scev (vr, loop, stmt, var); }
  void extract_range_from_stmt (gimple *stmt, edge *taken_edge_p,
                                tree *output_p, value_range *vr)
    { vr_values.extract_range_from_stmt (stmt, taken_edge_p, output_p, vr); }
  void set_defs_to_varying (gimple *stmt)
    { return vr_values.set_defs_to_varying (stmt); }
  void set_vr_value (tree name, value_range *vr)
    { vr_values.set_vr_value (name, vr); }
  void extract_range_for_var_from_comparison_expr (tree var,
						   enum tree_code cond_code,
						   tree op, tree limit,
						   value_range *vr_p)
    { vr_values.extract_range_for_var_from_comparison_expr (var, cond_code,
							    op, limit, vr_p); }
};

#endif /* GCC_GIMPLE_SSA_EVRP_ANALYZE_H */
