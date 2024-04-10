/* Cost model declaration of RISC-V 'V' Extension for GNU compiler.
   Copyright (C) 2023-2024 Free Software Foundation, Inc.
   Contributed by Juzhe Zhong (juzhe.zhong@rivai.ai), RiVAI Technologies Ltd.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_RISCV_VECTOR_COST_H
#define GCC_RISCV_VECTOR_COST_H

namespace riscv_vector {

struct stmt_point
{
  /* Program point.  */
  unsigned int point;
  gimple *stmt;
  stmt_vec_info stmt_info;
};

enum cost_type_enum
{
  SCALAR_COST,
  VLA_VECTOR_COST,
  VLS_VECTOR_COST
};

/* Pair typedef used by live range: <start, end>.  */
typedef std::pair<unsigned int, unsigned int> pair;

/* rvv-specific vector costs.  */
class costs : public vector_costs
{
  using vector_costs::vector_costs;

public:
  costs (vec_info *, bool);

  bool better_main_loop_than_p (const vector_costs *other) const override;

private:
  unsigned int add_stmt_cost (int count, vect_cost_for_stmt kind,
			      stmt_vec_info stmt_info, slp_tree node,
			      tree vectype, int misalign,
			      vect_cost_model_location where) override;
  void finish_cost (const vector_costs *) override;

  /* True if we have performed one-time initialization based on the
     vec_info.  */
  bool m_analyzed_vinfo = false;

  /* - If M_COST_TYPE = SCALAR_COST then we're costing the original scalar code.
     - If M_COST_TYPE = VLA_VECTOR_COST is nonzero then we're costing VLA
       partial vectorization codes.
     - If M_COST_TYPE = VLS_VECTOR_COST is nonzero then we're costing VLS
       minimum length vector codes.  */
  enum cost_type_enum m_cost_type;

  /* On some CPUs, VLA and VLS provide the same theoretical vector
     throughput, such as 4x128 VLS vs. 2x256 VLA.  In those
     situations, we try to predict whether an VLS implementation
     of the loop could be completely unrolled and become straight-line code.
     If so, it is generally better to use the VLS version rather
     than length-agnostic VLA, since the VLA loop would execute an unknown
     number of times and so could not be completely unrolled in the same way.

     If we're applying this heuristic, M_UNROLLED_VLS_NITERS is the
     number of VLS loop iterations that would be unrolled and
     M_UNROLLED_VLS_STMTS estimates the total number of statements
     in the unrolled loop.  Both values are zero if we're not applying
     the heuristic.  */
  unsigned HOST_WIDE_INT m_unrolled_vls_niters = 0;
  unsigned HOST_WIDE_INT m_unrolled_vls_stmts = 0;

  tree cst0 = build_int_cst (integer_type_node, 0);

  /* Store the memory references already processed.  */
  typedef pair_hash <tree_operand_hash, tree_operand_hash> tree_pair_hash;
  hash_set <tree_pair_hash> memrefs;

  void analyze_loop_vinfo (loop_vec_info);
  void record_potential_vls_unrolling (loop_vec_info);
  bool prefer_unrolled_loop () const;

  /* Analyze the vectorized program statements and compute the maximum live
     V_REGS live at some program point if we enable dynamic LMUL cost model.

     It's true when LMUL of loop vectorization factor > 1 and has unexpected
     V_REGS spills according to the analysis.  */
  bool m_has_unexpected_spills_p = false;
  void record_potential_unexpected_spills (loop_vec_info);

  void adjust_vect_cost_per_loop (loop_vec_info);
  unsigned adjust_stmt_cost (enum vect_cost_for_stmt kind,
			     loop_vec_info,
			     stmt_vec_info stmt_info, slp_tree,
			     tree vectype, int stmt_cost);
};

} // namespace riscv_vector

#endif // GCC_RISCV_VECTOR_COST_H
