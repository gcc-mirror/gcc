/* brig-control-handler.cc -- brig control directive handling
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
#include "diagnostic.h"
#include "print-tree.h"

size_t
brig_directive_control_handler::operator () (const BrigBase *base)
{
  const BrigDirectiveControl *inst = (const BrigDirectiveControl *) base;
  const BrigData *operand_entries
    = m_parent.get_brig_data_entry (inst->operands);

  /* Parse the constant integer operands.  */
  std::vector<tree> operands;
  for (size_t i = 0; i < operand_entries->byteCount / 4; ++i)
    {
      uint32_t operand_offset
	= ((const uint32_t *) &operand_entries->bytes)[i];
      const BrigBase *operand_data
	= m_parent.get_brig_operand_entry (operand_offset);

      tree operand_type
	= (inst->control == BRIG_CONTROL_REQUIREDGRIDSIZE
	   || inst->control == BRIG_CONTROL_MAXFLATGRIDSIZE) ?
	uint64_type_node : uint32_type_node;
      operands.push_back
	(build_tree_operand (*(const BrigInstBase*)inst, *operand_data,
			     operand_type));
    }

  switch (inst->control)
    {
    case BRIG_CONTROL_MAXDYNAMICGROUPSIZE:
      {
	m_parent.m_cf->m_descriptor.max_dynamic_group_size
	  = int_constant_value (operands.at (0));
	break;
      }
    case BRIG_CONTROL_MAXFLATGRIDSIZE:
      {
	m_parent.m_cf->m_descriptor.max_flat_grid_size
	  = int_constant_value (operands.at (0));
	break;
      }
    case BRIG_CONTROL_MAXFLATWORKGROUPSIZE:
      {
	m_parent.m_cf->m_descriptor.max_flat_workgroup_size
	  = int_constant_value (operands.at (0));
	break;
      }
    case BRIG_CONTROL_REQUIREDDIM:
      {
	m_parent.m_cf->m_descriptor.required_dim
	  = int_constant_value (operands.at (0));
	break;
      }
    case BRIG_CONTROL_REQUIREDGRIDSIZE:
      {
	m_parent.m_cf->m_descriptor.required_grid_size[0]
	  = int_constant_value (operands.at (0));
	m_parent.m_cf->m_descriptor.required_grid_size[1]
	  = int_constant_value (operands.at (1));
	m_parent.m_cf->m_descriptor.required_grid_size[2]
	  = int_constant_value (operands.at (2));
	break;
      }
    case BRIG_CONTROL_REQUIREDWORKGROUPSIZE:
      {
	m_parent.m_cf->m_descriptor.required_workgroup_size[0]
	  = int_constant_value (operands.at (0));
	m_parent.m_cf->m_descriptor.required_workgroup_size[1]
	  = int_constant_value (operands.at (1));
	m_parent.m_cf->m_descriptor.required_workgroup_size[2]
	  = int_constant_value (operands.at (2));
	break;
      }
    case BRIG_CONTROL_REQUIRENOPARTIALWORKGROUPS:
      /* Performance hint only, ignored for now.  */
      break;
    case BRIG_CONTROL_ENABLEBREAKEXCEPTIONS:
    case BRIG_CONTROL_ENABLEDETECTEXCEPTIONS:
      /* Unimplemented.  */
      break;
    default:
      sorry ("Unsupported control directive %x.\n", inst->control);
    }
  return base->byteCount;
}
