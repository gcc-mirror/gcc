/* brig-code-entry-handler.h -- a gccbrig base class
   Copyright (C) 2016-2018 Free Software Foundation, Inc.
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

#ifndef GCC_BRIG_CODE_ENTRY_HANDLER_H
#define GCC_BRIG_CODE_ENTRY_HANDLER_H

#include "brig-to-generic.h"

#include <map>
#include <vector>

class tree_element_unary_visitor;

/* An interface to organize the different types of element handlers
   for the code section.  */

class brig_code_entry_handler : public brig_entry_handler
{
public:
  brig_code_entry_handler (brig_to_generic &parent);

  /* Handles the brig_code data at the given pointer and adds it to the
     currently built tree.  Returns the number of consumed bytes.  */

  virtual size_t operator () (const BrigBase *base) = 0;

  void append_statement (tree stmt);

protected:

  tree get_tree_expr_type_for_hsa_type (BrigType16_t brig_type) const;
  tree get_tree_cst_for_hsa_operand (const BrigOperandConstantBytes *brigConst,
				     tree type) const;
  tree get_comparison_result_type (tree source_type);

  tree build_code_ref (const BrigBase &ref);

  tree build_tree_operand (const BrigInstBase &brig_inst,
			   const BrigBase &operand,
			   tree operand_type = NULL_TREE,
			   bool is_input = false);

  tree build_address_operand (const BrigInstBase &brig_inst,
			      const BrigOperandAddress &addr_operand);

  tree build_tree_operand_from_brig (const BrigInstBase *brig_inst,
				     tree operand_type, size_t operand_index);

  tree build_tree_cst_element (BrigType16_t element_type,
			       const unsigned char *next_data) const;

  bool needs_workitem_context_data (BrigOpcode16_t brig_opcode) const;

  tree add_temp_var (std::string name, tree expr);

  tree build_f2h_conversion (tree source);
  tree build_h2f_conversion (tree source);

  tree_stl_vec build_operands (const BrigInstBase &brig_inst);
  void analyze_operands (const BrigInstBase &brig_inst);
  tree build_output_assignment (const BrigInstBase &brig_inst, tree output,
				tree inst_expr);

  tree apply_to_all_elements (tree_element_unary_visitor &visitor,
			      tree operand);

  HOST_WIDE_INT int_constant_value (tree node);

  tree extend_int (tree input, tree dest_type, tree src_type);

private:

  tree_stl_vec build_or_analyze_operands (const BrigInstBase &brig_inst,
					  bool analyze);
};

/* Implement the Visitor software pattern for performing various actions on
   elements of vector operands.  This enables separating the vector element
   traversal/extraction/packing code from whatever different actions are
   performed to each element.  */

class tree_element_unary_visitor
{
public:
  tree operator () (brig_code_entry_handler &handler, tree operand);

  /* Performs an action to a single element, which can have originally
     been a vector element or a scalar.  */

  virtual tree visit_element (brig_code_entry_handler &handler, tree operand)
    = 0;
};

class tree_element_binary_visitor
{
public:
  tree operator () (brig_code_entry_handler &handler, tree operand0,
		   tree operand1);

  /* Performs an action to a pair of elements, which can have originally
     been a vector element or a scalar.  */

  virtual tree visit_element (brig_code_entry_handler &handler, tree operand0,
			      tree operand1)
    = 0;
};

/* Visitor for flushing float elements to zero.  */

class flush_to_zero : public tree_element_unary_visitor
{
public:
  flush_to_zero (bool fp16) : m_fp16 (fp16)
  {
  }

  virtual tree visit_element (brig_code_entry_handler &caller, tree operand);

private:

  /* True if the value should be flushed according to fp16 limits.  */

  bool m_fp16;
};

/* Visitor for converting F16 elements to F32.  */

class half_to_float : public tree_element_unary_visitor
{
public:
  virtual tree visit_element (brig_code_entry_handler &caller, tree operand);
};

/* Visitor for converting F32 elements to F16.  */

class float_to_half : public tree_element_unary_visitor
{
public:
  virtual tree visit_element (brig_code_entry_handler &caller, tree operand);
};

/* A base class for instruction types that support floating point
   modifiers.

   operator () delegates to subclasses (template method pattern) in
   type specific parts.  */

class brig_inst_mod_handler : public brig_code_entry_handler
{
public:
  brig_inst_mod_handler (brig_to_generic &parent)
    : brig_code_entry_handler (parent)
  {
  }

  virtual size_t generate (const BrigBase *base);
  virtual const BrigAluModifier8_t *modifier (const BrigBase *base) const;
  virtual const BrigRound8_t *round (const BrigBase *base) const;

  size_t operator () (const BrigBase *base);
};

class brig_directive_function_handler : public brig_code_entry_handler
{
public:
  brig_directive_function_handler (brig_to_generic &parent)
    : brig_code_entry_handler (parent)
  {
  }
  size_t operator () (const BrigBase *base);
};

class brig_directive_control_handler : public brig_code_entry_handler
{
public:
  brig_directive_control_handler (brig_to_generic &parent)
    : brig_code_entry_handler (parent)
  {
  }

  size_t operator () (const BrigBase *base);
};

class brig_directive_variable_handler : public brig_code_entry_handler
{
public:
  brig_directive_variable_handler (brig_to_generic &parent)
    : brig_code_entry_handler (parent)
  {
  }

  size_t operator () (const BrigBase *base);

  tree build_variable (const BrigDirectiveVariable *brigVar,
		       tree_code var_decltype = VAR_DECL);

  size_t get_brig_var_alignment (const BrigDirectiveVariable *brigVar);
};

class brig_directive_fbarrier_handler : public brig_code_entry_handler
{
public:
  brig_directive_fbarrier_handler (brig_to_generic &parent)
    : brig_code_entry_handler (parent)
  {
  }

  size_t operator () (const BrigBase *base);
};

class brig_directive_label_handler : public brig_code_entry_handler
{
public:
  brig_directive_label_handler (brig_to_generic &parent)
    : brig_code_entry_handler (parent)
  {
  }

  size_t operator () (const BrigBase *base);
};

class brig_directive_comment_handler : public brig_code_entry_handler
{
public:
  brig_directive_comment_handler (brig_to_generic &parent)
    : brig_code_entry_handler (parent)
  {
  }

  size_t operator () (const BrigBase *base);
};

class brig_directive_arg_block_handler : public brig_code_entry_handler
{
public:
  brig_directive_arg_block_handler (brig_to_generic &parent)
    : brig_code_entry_handler (parent)
  {
  }

  size_t operator () (const BrigBase *base);
};

class brig_basic_inst_handler : public brig_code_entry_handler
{
public:
  brig_basic_inst_handler (brig_to_generic &parent);

  size_t operator () (const BrigBase *base);

private:
  tree build_lower_element_broadcast (tree vec_operand);

  bool must_be_scalarized (const BrigInstBase *brig_inst,
			   tree instr_type) const;

  tree build_inst_expr (BrigOpcode16_t brig_opcode, BrigType16_t brig_type,
			 tree arith_type, tree_stl_vec &operands);

  tree build_shuffle (tree arith_type, tree_stl_vec &operands);
  tree build_unpack (tree_stl_vec &operands);
  tree build_pack (tree_stl_vec &operands);

  tree build_unpack_lo_or_hi (BrigOpcode16_t brig_opcode, tree arith_type,
			      tree_stl_vec &operands);
};

class brig_cvt_inst_handler : public brig_inst_mod_handler
{
public:
  brig_cvt_inst_handler (brig_to_generic &parent)
    : brig_inst_mod_handler (parent)
  {
  }

  virtual size_t generate (const BrigBase *base);
  virtual const BrigAluModifier8_t *modifier (const BrigBase *base) const;
  virtual const BrigRound8_t *round (const BrigBase *base) const;
};

class brig_branch_inst_handler : public brig_code_entry_handler
{
public:
  brig_branch_inst_handler (brig_to_generic &parent)
    : brig_code_entry_handler (parent)
  {
  }

  size_t operator () (const BrigBase *base);
};

class brig_mem_inst_handler : public brig_code_entry_handler
{
public:
  brig_mem_inst_handler (brig_to_generic &parent)
    : brig_code_entry_handler (parent)
  {
  }

  size_t operator () (const BrigBase *base);

private:
  tree build_mem_access (const BrigInstBase *brig_inst, tree addr, tree data);
};

class brig_copy_move_inst_handler : public brig_code_entry_handler
{
public:
  brig_copy_move_inst_handler (brig_to_generic &parent)
    : brig_code_entry_handler (parent)
  {
  }

  size_t operator () (const BrigBase *base);

private:
  size_t handle_lda (const BrigInstBase *base);
};

class brig_atomic_inst_handler : public brig_code_entry_handler
{
private:
  typedef std::map<std::string, tree> atomic_builtins_map;

public:
  brig_atomic_inst_handler (brig_to_generic &parent);

  size_t operator () (const BrigBase *base);

protected:
  size_t generate_tree (const BrigInstBase &inst,
			BrigAtomicOperation8_t atomic_opcode);
};

class brig_signal_inst_handler : public brig_atomic_inst_handler
{
public:
  brig_signal_inst_handler (brig_to_generic &parent)
    : brig_atomic_inst_handler (parent)
  {
  }
  size_t operator () (const BrigBase *base);
};

class brig_cmp_inst_handler : public brig_code_entry_handler
{
public:
  brig_cmp_inst_handler (brig_to_generic &parent)
    : brig_code_entry_handler (parent)
  {
  }

  size_t operator () (const BrigBase *base);
};

class brig_seg_inst_handler : public brig_code_entry_handler
{
public:
  brig_seg_inst_handler (brig_to_generic &parent);

  size_t operator () (const BrigBase *base);
};

class brig_lane_inst_handler : public brig_code_entry_handler
{
public:
  brig_lane_inst_handler (brig_to_generic &parent);

  size_t operator () (const BrigBase *base);
};

class brig_queue_inst_handler : public brig_code_entry_handler
{
public:
  brig_queue_inst_handler (brig_to_generic &parent);

  size_t operator () (const BrigBase *base);
};

class brig_directive_module_handler : public brig_code_entry_handler
{
public:
  brig_directive_module_handler (brig_to_generic &parent)
    : brig_code_entry_handler (parent)
  {
  }

  size_t operator () (const BrigBase *base);
};


#endif
