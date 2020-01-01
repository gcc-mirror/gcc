/* brig-seg-inst-handler.cc -- brig segment related instruction handling
   Copyright (C) 2016-2020 Free Software Foundation, Inc.
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

#include <sstream>

#include "brig-code-entry-handler.h"
#include "brig-util.h"
#include "convert.h"
#include "tree-pretty-print.h"
#include "errors.h"
#include "diagnostic-core.h"

brig_seg_inst_handler::brig_seg_inst_handler (brig_to_generic &parent)
  : brig_code_entry_handler (parent)
{
}

size_t
brig_seg_inst_handler::operator () (const BrigBase *base)
{
  const BrigInstBase &inst_base = *(const BrigInstBase *) base;

  std::vector<tree> operands = build_operands (inst_base);

  tree expr = NULL_TREE;

  if (inst_base.opcode == BRIG_OPCODE_STOF)
    {
      const BrigInstSegCvt &inst = *(const BrigInstSegCvt *) base;

      if (inst.segment == BRIG_SEGMENT_GROUP)
	expr = build2 (PLUS_EXPR, size_type_node,
		       convert_to_integer (size_type_node,
					   m_parent.m_cf->m_group_base_arg),
		       convert_to_integer (size_type_node, operands[1]));
      else if (inst.segment == BRIG_SEGMENT_PRIVATE
	       || inst.segment == BRIG_SEGMENT_SPILL)
	expr = build2 (PLUS_EXPR, size_type_node,
		       convert_to_integer (size_type_node,
					   m_parent.m_cf->m_private_base_arg),
		       convert_to_integer (size_type_node, operands[1]));
      else
       gcc_unreachable ();

      if (!(inst.modifier & BRIG_SEG_CVT_NONULL))
	{
	  /* Need to convert the null value. -1 is used for 32b segments,
	     and 0 for flat/global.  */
	  tree cmp
	    = build2 (EQ_EXPR, uint32_type_node,
		      build_int_cstu (uint32_type_node, -1), operands[1]);

	  tree null_check = build3 (COND_EXPR, size_type_node, cmp,
				    build_int_cstu (size_type_node, 0), expr);

	  expr = null_check;
	}
    }
  else if (inst_base.opcode == BRIG_OPCODE_FTOS)
    {
      const BrigInstSegCvt &inst = *(const BrigInstSegCvt *) base;

      if (inst.segment == BRIG_SEGMENT_GROUP)
	expr = build2 (MINUS_EXPR, size_type_node,
		       convert_to_integer (size_type_node,
					   m_parent.m_cf->m_group_base_arg),
		       convert_to_integer (size_type_node, operands[1]));
      else if (inst.segment == BRIG_SEGMENT_PRIVATE)
	expr = build2 (MINUS_EXPR, size_type_node,
		       convert_to_integer (size_type_node,
					   m_parent.m_cf->m_private_base_arg),
		       convert_to_integer (size_type_node, operands[1]));
      else
	gcc_unreachable ();

      if (!(inst.modifier & BRIG_SEG_CVT_NONULL))
	{
	  /* Need to convert the null value. -1 is used for 32b segments,
	     and 0 for flat/global.  */
	  tree cmp = build2 (EQ_EXPR, size_type_node,
			     build_int_cstu (size_type_node, 0), operands[1]);

	  tree null_check
	    = build3 (COND_EXPR, size_type_node, cmp,
		      build_int_cstu (uint32_type_node, -1), expr);
	  expr = null_check;
	}
    }
  else if (inst_base.opcode == BRIG_OPCODE_NULLPTR)
    {
      const BrigInstSeg &inst = *(const BrigInstSeg *) base;
      if (inst.segment == BRIG_SEGMENT_GLOBAL
	  || inst.segment == BRIG_SEGMENT_FLAT
	  || inst.segment == BRIG_SEGMENT_READONLY)
	expr = build_int_cstu (uint64_type_node, 0);
      else
	expr = build_int_cstu (uint32_type_node, -1);
    }
  else if (inst_base.opcode == BRIG_OPCODE_SEGMENTP)
    {
      const BrigInstSegCvt &inst = *(const BrigInstSegCvt *) base;

      tree builtin = NULL_TREE;
      switch (inst.segment)
	{
	case BRIG_SEGMENT_GLOBAL:
	  builtin = builtin_decl_explicit (BUILT_IN_HSAIL_SEGMENTP_GLOBAL);
	  break;
	case BRIG_SEGMENT_GROUP:
	  builtin = builtin_decl_explicit (BUILT_IN_HSAIL_SEGMENTP_GROUP);
	  break;
	case BRIG_SEGMENT_PRIVATE:
	  builtin = builtin_decl_explicit (BUILT_IN_HSAIL_SEGMENTP_PRIVATE);
	  break;
	default:
	  gcc_unreachable ();
	}

      expr = call_builtin (builtin, 2,
			   uint32_type_node, uint64_type_node, operands[1],
			   ptr_type_node, m_parent.m_cf->m_context_arg);
    }
  else
    gcc_unreachable ();

  build_output_assignment (inst_base, operands[0], expr);
  return base->byteCount;
}
