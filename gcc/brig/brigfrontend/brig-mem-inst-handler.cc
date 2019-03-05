/* brig-mem-inst-handler.cc -- brig memory inst handler
   Copyright (C) 2016-2019 Free Software Foundation, Inc.
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
#include "brig-util.h"
#include "gimple-expr.h"
#include "print-tree.h"
#include "tree-pretty-print.h"
#include "convert.h"
#include "diagnostic-core.h"

tree
brig_mem_inst_handler::build_mem_access (const BrigInstBase *brig_inst,
					 tree addr, tree data)
{
  bool is_load = brig_inst->opcode == BRIG_OPCODE_LD;
  bool is_store = brig_inst->opcode == BRIG_OPCODE_ST;

  if (!is_load && !is_store)
    gcc_unreachable ();

  tree instr_type = gccbrig_tree_type_for_hsa_type (brig_inst->type);

  /* In case of {ld,st}_v{2,4}. Note: since 'register' variables may
     be any type, even a vector type, we distinguish the registers
     from operand lists by checking for constructor nodes (which
     operand lists are represented as).  */
  if (VECTOR_TYPE_P (TREE_TYPE (data)) && TREE_CODE (data) == CONSTRUCTOR)
    instr_type = TREE_TYPE (data);

  tree ptype = build_pointer_type (instr_type);

  /* The HSAIL mem instructions are unaligned by default.
     TODO: exploit the align modifier, it should lead to faster code.
  */
  tree unaligned_type = build_aligned_type (instr_type, 8);

  /* Create a mem ref from the previous result, without offset.  */
  tree mem_ref
    = build2 (MEM_REF, unaligned_type, addr, build_int_cst (ptype, 0));

  if (is_load)
    {
      /* Add a temporary variable so there won't be multiple
	 reads in case of vector unpack.  */
      mem_ref = m_parent.m_cf->add_temp_var ("mem_read", mem_ref);
      return build_output_assignment (*brig_inst, data, mem_ref);
    }
  else
    {
      tree stmt = build2 (MODIFY_EXPR, TREE_TYPE (mem_ref), mem_ref, data);
      return m_parent.m_cf->append_statement (stmt);
    }
  return mem_ref;
}

size_t
brig_mem_inst_handler::operator () (const BrigBase *base)
{
  const BrigInstBase *brig_inst
    = (const BrigInstBase *) &((const BrigInstBasic *) base)->base;

  if (brig_inst->opcode == BRIG_OPCODE_ALLOCA)
    {
      tree_stl_vec operands = build_operands (*brig_inst);
      size_t alignment = 1;
      const BrigInstMem *mem_inst = (const BrigInstMem *) brig_inst;
      if (mem_inst->align != BRIG_ALIGNMENT_NONE)
	{
	  alignment = 1 << (mem_inst->align - 1);
	}

      tree align_opr = build_int_cstu (size_type_node, alignment);
      tree_stl_vec inputs;
      inputs.push_back (operands[1]);
      inputs.push_back (align_opr);
      tree builtin_call
	= m_parent.m_cf->expand_or_call_builtin (BRIG_OPCODE_ALLOCA,
						 BRIG_TYPE_U32,
						 uint32_type_node, inputs);
      build_output_assignment (*brig_inst, operands[0], builtin_call);
      m_parent.m_cf->m_has_allocas = true;
      return base->byteCount;
    }

  tree instr_type = gccbrig_tree_type_for_hsa_type (brig_inst->type);

  const BrigData *operand_entries
    = m_parent.get_brig_data_entry (brig_inst->operands);

  uint32_t data_operand_offset;
  memcpy (&data_operand_offset, &operand_entries->bytes, 4);

  const BrigBase *operand
    = m_parent.get_brig_operand_entry (data_operand_offset);

  const BrigData *operandData = NULL;

  bool is_store = brig_inst->opcode == BRIG_OPCODE_ST;

  bool is_three_element_vector_access
    = operand->kind == BRIG_KIND_OPERAND_OPERAND_LIST
      && (operandData = m_parent.get_brig_data_entry
	  (((const BrigOperandOperandList *) operand)->elements))
      && operandData->byteCount / 4 == 3;

  if (is_three_element_vector_access)
    {
      /* We need to scalarize the 3-element vector accesses here
	 because gcc assumes the GENERIC vector datatypes are of two exponent
	 size internally.  */
      size_t bytes = operandData->byteCount;
      const BrigOperandOffset32_t *operand_ptr
	= (const BrigOperandOffset32_t *) operandData->bytes;

      uint32_t addr_operand_offset;
      memcpy (&addr_operand_offset, &operand_entries->bytes + 4, 4);

      const BrigOperandAddress *addr_operand
	= (const BrigOperandAddress *) m_parent.get_brig_operand_entry
	(addr_operand_offset);

      tree address_base = build_address_operand (*brig_inst, *addr_operand);

      uint32_t address_offset = 0;
      while (bytes > 0)
	{
	  BrigOperandOffset32_t offset = *operand_ptr;
	  const BrigBase *operand_element
	    = m_parent.get_brig_operand_entry (offset);
	  tree data
	    = build_tree_operand (*brig_inst, *operand_element, instr_type);

	  tree ptr_offset = build_int_cst (size_type_node, address_offset);
	  tree address = build2 (POINTER_PLUS_EXPR, TREE_TYPE (address_base),
				 address_base, ptr_offset);

	  if (is_store && TREE_TYPE (data) != instr_type)
	    data = build_resize_convert_view (instr_type, data);

	  build_mem_access (brig_inst, address, data);

	  address_offset += int_size_in_bytes (instr_type);
	  ++operand_ptr;
	  bytes -= 4;
	}
    }
  else
    {
      tree_stl_vec operands = build_operands (*brig_inst);

      tree &data = operands.at (0);
      tree &addr = operands.at (1);
      build_mem_access (brig_inst, addr, data);
    }

  return base->byteCount;
}
