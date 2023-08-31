/* Cost model implementation for RISC-V 'V' Extension for GNU compiler.
   Copyright (C) 2023-2023 Free Software Foundation, Inc.
   Contributed by Juzhe Zhong (juzhe.zhong@rivai.ai), RiVAI Technologies Ltd.

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

#define IN_TARGET_CODE 1

#define INCLUDE_STRING
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "target.h"
#include "function.h"
#include "tree.h"
#include "basic-block.h"
#include "rtl.h"
#include "gimple.h"
#include "targhooks.h"
#include "cfgloop.h"
#include "fold-const.h"
#include "tm_p.h"
#include "tree-vectorizer.h"

/* This file should be included last.  */
#include "riscv-vector-costs.h"

namespace riscv_vector {

costs::costs (vec_info *vinfo, bool costing_for_scalar)
  : vector_costs (vinfo, costing_for_scalar)
{}

unsigned
costs::add_stmt_cost (int count, vect_cost_for_stmt kind,
		      stmt_vec_info stmt_info, slp_tree, tree vectype,
		      int misalign, vect_cost_model_location where)
{
  /* TODO: Use default STMT cost model.
	   We will support more accurate STMT cost model later.  */
  int stmt_cost = default_builtin_vectorization_cost (kind, vectype, misalign);
  return record_stmt_cost (stmt_info, where, count * stmt_cost);
}

void
costs::finish_cost (const vector_costs *scalar_costs)
{
  vector_costs::finish_cost (scalar_costs);
}

} // namespace riscv_vector
