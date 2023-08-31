/* Cost model declaration of RISC-V 'V' Extension for GNU compiler.
   Copyright (C) 2023-2023 Free Software Foundation, Inc.
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

/* rvv-specific vector costs.  */
class costs : public vector_costs
{
  using vector_costs::vector_costs;

public:
  costs (vec_info *, bool);

private:
  unsigned int add_stmt_cost (int count, vect_cost_for_stmt kind,
			      stmt_vec_info stmt_info, slp_tree node,
			      tree vectype, int misalign,
			      vect_cost_model_location where) override;
  void finish_cost (const vector_costs *) override;
};

} // namespace riscv_vector

#endif // GCC_RISCV_VECTOR_COST_H
