/* brig-branch-inst-handler.cc -- brig branch instruction handling
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
#include "brig-util.h"
#include "tree-pretty-print.h"
#include "print-tree.h"
#include "vec.h"
#include "fold-const.h"

size_t
brig_branch_inst_handler::operator () (const BrigBase *base)
{
  const BrigInstBase *brig_inst
    = (const BrigInstBase *) &((const BrigInstBasic *) base)->base;

  if (brig_inst->opcode == BRIG_OPCODE_CALL)
    {
      const BrigData *operand_entries
	= m_parent.get_brig_data_entry (brig_inst->operands);
      tree func_ref = NULL_TREE;
      vec<tree, va_gc> *out_args;
      vec_alloc (out_args, 1);
      vec<tree, va_gc> *in_args;
      vec_alloc (in_args, 4);

      size_t operand_count = operand_entries->byteCount / 4;
      gcc_assert (operand_count < 4);

      for (size_t i = 0; i < operand_count; ++i)
	{
	  uint32_t operand_offset
	    = ((const uint32_t *) &operand_entries->bytes)[i];
	  const BrigBase *operand_data
	    = m_parent.get_brig_operand_entry (operand_offset);
	  if (i == 1)
	    {
	      gcc_assert (operand_data->kind == BRIG_KIND_OPERAND_CODE_REF);
	      func_ref = build_tree_operand (*brig_inst, *operand_data);
	      continue;
	    }
	  gcc_assert (operand_data->kind == BRIG_KIND_OPERAND_CODE_LIST);
	  const BrigOperandCodeList *codelist
	    = (const BrigOperandCodeList *) operand_data;
	  const BrigData *data
	    = m_parent.get_brig_data_entry (codelist->elements);

	  size_t bytes = data->byteCount;
	  const BrigOperandOffset32_t *operand_ptr
	    = (const BrigOperandOffset32_t *) data->bytes;

	  vec<tree, va_gc> *args = i == 0 ? out_args : in_args;

	  while (bytes > 0)
	    {
	      BrigOperandOffset32_t offset = *operand_ptr;
	      const BrigBase *code_element
		= m_parent.get_brig_code_entry (offset);
	      gcc_assert (code_element->kind == BRIG_KIND_DIRECTIVE_VARIABLE);
	      const BrigDirectiveVariable *brig_var
		= (const BrigDirectiveVariable *) code_element;
	      tree var = m_parent.m_cf->arg_variable (brig_var);

	      if (brig_var->type & BRIG_TYPE_ARRAY)
		{
		  /* Array return values are passed as the first argument.  */
		  args = in_args;
		  /* Pass pointer to the element zero and use its element zero
		     as the base address.  */
		  tree etype = TREE_TYPE (TREE_TYPE (var));
		  tree ptype = build_pointer_type (etype);
		  tree element_zero
		    = build4 (ARRAY_REF, etype, var, integer_zero_node,
			      NULL_TREE, NULL_TREE);
		  var = build1 (ADDR_EXPR, ptype, element_zero);
		}

	      gcc_assert (var != NULL_TREE);
	      vec_safe_push (args, var);
	      ++operand_ptr;
	      bytes -= 4;
	    }
	}

      gcc_assert (func_ref != NULL_TREE);
      gcc_assert (out_args->length () == 0 || out_args->length () == 1);

      tree ret_val_type = void_type_node;
      tree ret_val = NULL_TREE;
      if (out_args->length () == 1)
	{
	  ret_val = (*out_args)[0];
	  ret_val_type = TREE_TYPE (ret_val);
	}

      /* Pass the hidden kernel arguments along to the called functions as
	 they might call builtins that need them or access group/private
	 memory.  */

      vec_safe_push (in_args, m_parent.m_cf->m_context_arg);
      vec_safe_push (in_args, m_parent.m_cf->m_group_base_arg);
      vec_safe_push (in_args, m_parent.m_cf->m_private_base_arg);

      tree call = build_call_vec (ret_val_type, build_fold_addr_expr (func_ref),
				  in_args);
      TREE_NOTHROW (func_ref) = 1;
      TREE_NOTHROW (call) = 1;

      if (ret_val != NULL_TREE)
	{
	  TREE_ADDRESSABLE (ret_val) = 1;
	  tree result_assign
	    = build2 (MODIFY_EXPR, TREE_TYPE (ret_val), ret_val, call);
	  m_parent.m_cf->append_statement (result_assign);
	}
      else
	{
	  m_parent.m_cf->append_statement (call);
	}

      m_parent.m_cf->m_has_unexpanded_dp_builtins = false;
      m_parent.m_cf->m_called_functions.push_back (func_ref);

      return base->byteCount;
    }

  tree instr_type = gccbrig_tree_type_for_hsa_type (brig_inst->type);
  tree_stl_vec operands = build_operands (*brig_inst);

  if (brig_inst->opcode == BRIG_OPCODE_BR)
    {
      tree goto_stmt = build1 (GOTO_EXPR, instr_type, operands[0]);
      m_parent.m_cf->append_statement (goto_stmt);
    }
  else if (brig_inst->opcode == BRIG_OPCODE_SBR)
    {
      tree select = operands[0];
      tree cases = operands[1];

      tree switch_expr = build3 (SWITCH_EXPR, TREE_TYPE (select), select,
				 NULL_TREE, NULL_TREE);

      tree default_case
	= build_case_label (NULL_TREE, NULL_TREE,
			    create_artificial_label (UNKNOWN_LOCATION));
      append_to_statement_list (default_case, &SWITCH_BODY (switch_expr));

      tree default_jump
	= build1 (GOTO_EXPR, void_type_node, TREE_VEC_ELT (cases, 0));
      append_to_statement_list (default_jump, &SWITCH_BODY (switch_expr));

      for (int c = 0; c < TREE_VEC_LENGTH (cases); ++c)
	{
	  tree case_label
	    = build_case_label (build_int_cst (integer_type_node, c), NULL_TREE,
				create_artificial_label (UNKNOWN_LOCATION));

	  append_to_statement_list (case_label, &SWITCH_BODY (switch_expr));

	  tree jump
	    = build1 (GOTO_EXPR, void_type_node, TREE_VEC_ELT (cases, c));
	  append_to_statement_list (jump, &SWITCH_BODY (switch_expr));
	}
      m_parent.m_cf->append_statement (switch_expr);
    }
  else if (brig_inst->opcode == BRIG_OPCODE_CBR)
    {
      tree condition = operands[0];
      tree target_goto = build1 (GOTO_EXPR, void_type_node, operands[1]);
      /* Represents the if..else as (condition)?(goto foo):(goto bar).  */
      tree if_stmt
	= build3 (COND_EXPR, void_type_node, condition, target_goto, NULL_TREE);
      m_parent.m_cf->append_statement (if_stmt);
    }
  else if (brig_inst->opcode == BRIG_OPCODE_WAVEBARRIER)
    {
      /* WAVEBARRIER is a NOP when WAVESIZE = 1.  */
    }
  else if (brig_inst->opcode == BRIG_OPCODE_BARRIER)
    {
      m_parent.m_cf->m_has_barriers = true;
      tree_stl_vec call_operands;
      /* FIXME.  We should add attributes (are there suitable ones in gcc?) that
	 ensure the barrier won't be duplicated or moved out of loops etc.
	 Like the 'noduplicate' of LLVM.  Same goes for fbarriers.  */
      m_parent.m_cf->append_statement
	(expand_or_call_builtin (brig_inst->opcode, BRIG_TYPE_NONE, NULL_TREE,
				 call_operands));
    }
  else if (brig_inst->opcode >= BRIG_OPCODE_ARRIVEFBAR
	   && brig_inst->opcode <= BRIG_OPCODE_WAITFBAR)
    {
      m_parent.m_cf->m_has_barriers = true;
      m_parent.m_cf->append_statement
	(expand_or_call_builtin (brig_inst->opcode, BRIG_TYPE_NONE,
				 uint32_type_node, operands));
    }
  else
    gcc_unreachable ();
  return base->byteCount;
}
