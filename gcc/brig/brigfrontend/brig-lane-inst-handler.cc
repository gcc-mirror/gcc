/* brig-lane-inst-handler.cc -- brig lane instruction handling
   Copyright (C) 2016-2017 Free Software Foundation, Inc.
   Contributed by Pekka Jaaskelainen <pekka.jaaskelainen@parmance.com>
   for General Processor Tech.

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

#include "brig-code-entry-handler.h"
#include "errors.h"
#include "diagnostic-core.h"
#include "brig-util.h"

brig_lane_inst_handler::brig_lane_inst_handler (brig_to_generic &parent)
  : brig_code_entry_handler (parent)
{
}

size_t
brig_lane_inst_handler::operator () (const BrigBase *base)
{
  const BrigInstLane &inst = *(const BrigInstLane *) base;
  tree_stl_vec operands = build_operands (inst.base);

  tree expr = NULL_TREE;
  if (inst.base.opcode == BRIG_OPCODE_ACTIVELANECOUNT)
    {
      /* Because we are fixed to single WI per wave, it's enough to
	 just check the src value of the single work item itself.  */
      expr = build2 (NE_EXPR, uint32_type_node,
		     build_zero_cst (uint32_type_node), operands[1]);
    }
  else if (inst.base.opcode == BRIG_OPCODE_ACTIVELANEID)
    {
      expr = build_zero_cst (uint32_type_node);
    }
  else if (inst.base.opcode == BRIG_OPCODE_ACTIVELANEMASK)
    {
      tree u64_type = gccbrig_tree_type_for_hsa_type (BRIG_TYPE_U64);
      tree zero_cst = build_zero_cst (u64_type);
      expr = build2 (NE_EXPR, u64_type, zero_cst, operands[1]);

      tree_stl_vec elements;
      elements.push_back (expr);
      elements.push_back (zero_cst);
      elements.push_back (zero_cst);
      elements.push_back (zero_cst);

      expr = pack (elements);
    }
  else if (inst.base.opcode == BRIG_OPCODE_ACTIVELANEPERMUTE)
    {
      tree src = operands[1];
      tree identity = operands[3];
      tree use_identity = operands[4];

      /* When WAVESIZE is 1, we either select the src of the work-item
	 itself or 'identity' in case use_identity is 1.  */

      tree cmp = build2 (EQ_EXPR, uint32_type_node,
			 build_int_cstu (TREE_TYPE (use_identity), 1),
			 use_identity);

      expr = build3 (COND_EXPR, TREE_TYPE (src), cmp, identity, src);
    }
  else
    gcc_unreachable ();

  build_output_assignment (inst.base, operands[0], expr);

  return base->byteCount;
}
