/* brig-code-entry-handler.cc -- a gccbrig base class
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

#include "stringpool.h"
#include "tree-iterator.h"
#include "toplev.h"
#include "diagnostic.h"
#include "brig-machine.h"
#include "brig-util.h"
#include "errors.h"
#include "real.h"
#include "print-tree.h"
#include "tree-pretty-print.h"
#include "target.h"
#include "langhooks.h"
#include "gimple-expr.h"
#include "convert.h"
#include "brig-util.h"
#include "builtins.h"
#include "phsa.h"
#include "brig-builtins.h"
#include "fold-const.h"

brig_code_entry_handler::brig_code_entry_handler (brig_to_generic &parent)
  : brig_entry_handler (parent)
{
}

/* Build a tree operand which is a reference to a piece of code.  REF is the
   original reference as a BRIG object.  */

tree
brig_code_entry_handler::build_code_ref (const BrigBase &ref)
{
  if (ref.kind == BRIG_KIND_DIRECTIVE_LABEL)
    {
      const BrigDirectiveLabel *brig_label = (const BrigDirectiveLabel *) &ref;

      const BrigData *label_name
	= m_parent.get_brig_data_entry (brig_label->name);

      std::string label_str ((const char *) (label_name->bytes),
			     label_name->byteCount);
      return m_parent.m_cf->label (label_str);
    }
  else if (ref.kind == BRIG_KIND_DIRECTIVE_FUNCTION)
    {
      const BrigDirectiveExecutable *func
       = (const BrigDirectiveExecutable *) &ref;
      return m_parent.function_decl (m_parent.get_mangled_name (func));
    }
  else if (ref.kind == BRIG_KIND_DIRECTIVE_FBARRIER)
    {
      const BrigDirectiveFbarrier* fbar = (const BrigDirectiveFbarrier*)&ref;

      std::string var_name = m_parent.get_mangled_name (fbar);
      uint64_t offset
	= m_parent.m_cf->group_variable_segment_offset (var_name);

      tree local_offset = build_int_cst (uint32_type_node, offset);
      if (m_parent.m_cf->m_local_group_variables.has_variable (var_name))
	local_offset
	  = build2 (PLUS_EXPR, uint64_type_node, local_offset,
		    convert (uint64_type_node,
			     m_parent.m_cf->m_group_local_offset_arg));
      return local_offset;
    }
  else
    gcc_unreachable ();
}

/* Produce a tree operand for the given BRIG_INST and its OPERAND.
   OPERAND_TYPE should be the operand type in case it should not
   be dictated by the BrigBase.  IS_INPUT indicates if the operand
   is an input operand or a result.  */

tree
brig_code_entry_handler::build_tree_operand (const BrigInstBase &brig_inst,
					     const BrigBase &operand,
					     tree operand_type, bool is_input)
{
  switch (operand.kind)
    {
    case BRIG_KIND_OPERAND_OPERAND_LIST:
      {
	vec<constructor_elt, va_gc> *constructor_vals = NULL;
	const BrigOperandOperandList &oplist
	  = (const BrigOperandOperandList &) operand;
	const BrigData *data = m_parent.get_brig_data_entry (oplist.elements);
	size_t bytes = data->byteCount;
	const BrigOperandOffset32_t *operand_ptr
	  = (const BrigOperandOffset32_t *) data->bytes;
	while (bytes > 0)
	  {
	    BrigOperandOffset32_t offset = *operand_ptr;
	    const BrigBase *operand_element
	      = m_parent.get_brig_operand_entry (offset);
	    tree element
	      = build_tree_operand (brig_inst, *operand_element, operand_type);

	    /* In case a vector is used an input, cast the elements to
	       correct size here so we don't need a separate unpack/pack for it.
	       fp16-fp32 conversion is done in build_operands ().  */
	    if (is_input && TREE_TYPE (element) != operand_type)
	      element = build_resize_convert_view (operand_type, element);

	    CONSTRUCTOR_APPEND_ELT (constructor_vals, NULL_TREE, element);
	    ++operand_ptr;
	    bytes -= 4;
	  }
	size_t element_count = data->byteCount / 4;
	tree vec_type = build_vector_type (operand_type, element_count);

	return build_constructor (vec_type, constructor_vals);
      }
    case BRIG_KIND_OPERAND_CODE_LIST:
      {
	/* Build a TREE_VEC of code expressions.  */

	const BrigOperandCodeList &oplist
	  = (const BrigOperandCodeList &) operand;
	const BrigData *data = m_parent.get_brig_data_entry (oplist.elements);
	size_t bytes = data->byteCount;
	const BrigOperandOffset32_t *operand_ptr
	  = (const BrigOperandOffset32_t *) data->bytes;

	size_t case_index = 0;
	size_t element_count = data->byteCount / 4;

	/* Create a TREE_VEC out of the labels in the list.  */
	tree vec = make_tree_vec (element_count);

	while (bytes > 0)
	  {
	    BrigOperandOffset32_t offset = *operand_ptr;
	    const BrigBase *ref = m_parent.get_brig_code_entry (offset);
	    tree element = build_code_ref (*ref);

	    gcc_assert (case_index < element_count);
	    TREE_VEC_ELT (vec, case_index) = element;
	    case_index++;

	    ++operand_ptr;
	    bytes -= 4;
	  }
	return vec;
      }
    case BRIG_KIND_OPERAND_REGISTER:
      {
	const BrigOperandRegister *brig_reg
	  = (const BrigOperandRegister *) &operand;
	return m_parent.m_cf->get_m_var_declfor_reg (brig_reg);
      }
    case BRIG_KIND_OPERAND_CONSTANT_BYTES:
      {
	const BrigOperandConstantBytes *brigConst
	  = (const BrigOperandConstantBytes *) &operand;
	/* The constants can be of different type than the instruction
	   and are implicitly casted to the input operand.  */
	return get_tree_cst_for_hsa_operand (brigConst, NULL_TREE);
      }
    case BRIG_KIND_OPERAND_WAVESIZE:
      {
	if (!INTEGRAL_TYPE_P (operand_type))
	  {
	    gcc_unreachable ();
	    return NULL_TREE;
	  }
	return build_int_cstu (operand_type, gccbrig_get_target_wavesize ());
      }
    case BRIG_KIND_OPERAND_CODE_REF:
      {
	const BrigOperandCodeRef *brig_code_ref
	  = (const BrigOperandCodeRef *) &operand;

	const BrigBase *ref = m_parent.get_brig_code_entry (brig_code_ref->ref);

	return build_code_ref (*ref);
      }
    case BRIG_KIND_OPERAND_ADDRESS:
      {
	return build_address_operand (brig_inst,
				      (const BrigOperandAddress &) operand);
      }
    default:
      gcc_unreachable ();
    }
}

/* Build a tree node representing an address reference from a BRIG_INST and its
   ADDR_OPERAND.  */

tree
brig_code_entry_handler::build_address_operand
  (const BrigInstBase &brig_inst, const BrigOperandAddress &addr_operand)
{
  tree instr_type = gccbrig_tree_type_for_hsa_type (brig_inst.type);

  BrigSegment8_t segment = BRIG_SEGMENT_GLOBAL;
  if (brig_inst.opcode == BRIG_OPCODE_LDA)
    segment = ((const BrigInstAddr &) brig_inst).segment;
  else if (brig_inst.base.kind == BRIG_KIND_INST_MEM)
    segment = ((const BrigInstMem &) brig_inst).segment;
  else if (brig_inst.base.kind == BRIG_KIND_INST_ATOMIC)
    segment = ((const BrigInstAtomic &) brig_inst).segment;

  tree var_offset = NULL_TREE;
  tree const_offset = NULL_TREE;
  tree symbol_base = NULL_TREE;

  if (addr_operand.symbol != 0)
    {
      const BrigDirectiveVariable *arg_symbol
	= (const BrigDirectiveVariable *) m_parent.get_brig_code_entry
	(addr_operand.symbol);

      std::string var_name = m_parent.get_mangled_name (arg_symbol);

      if (segment == BRIG_SEGMENT_KERNARG)
	{
	  /* Find the offset to the kernarg buffer for the given
	     kernel argument variable.  */
	  tree func = m_parent.m_cf->m_func_decl;
	  /* __args is the first parameter in kernel functions.  */
	  symbol_base = DECL_ARGUMENTS (func);
	  uint64_t offset = m_parent.m_cf->kernel_arg_offset (arg_symbol);
	  if (offset > 0)
	    const_offset = build_int_cst (size_type_node, offset);
	}
      else if (segment == BRIG_SEGMENT_GROUP)
	{
	  uint64_t offset
	    = m_parent.m_cf->group_variable_segment_offset (var_name);
	  const_offset = build_int_cst (size_type_node, offset);

	  /* If it's a local group variable reference, substract the local
	     group segment offset to get the group base ptr offset.  */
	  if (m_parent.m_cf->m_local_group_variables.has_variable (var_name))
	    const_offset
	      = build2 (PLUS_EXPR, uint64_type_node, const_offset,
			convert (uint64_type_node,
				 m_parent.m_cf->m_group_local_offset_arg));

	}
      else if (segment == BRIG_SEGMENT_PRIVATE || segment == BRIG_SEGMENT_SPILL)
	{
	  uint32_t offset = m_parent.private_variable_segment_offset (var_name);

	  /* Compute the offset to the work item's copy:

	     single-wi-offset * local_size + wiflatid * varsize

	     This way the work items have the same variable in
	     successive elements to each other in the segment,
	     helping to achieve autovectorization of loads/stores
	     with stride 1.  */

	  tree_stl_vec uint32_0
	    = tree_stl_vec (1, build_int_cst (uint32_type_node, 0));

	  tree_stl_vec uint32_1
	    = tree_stl_vec (1, build_int_cst (uint32_type_node, 1));

	  tree_stl_vec uint32_2
	    = tree_stl_vec (1, build_int_cst (uint32_type_node, 2));

	  tree local_size
	    = build2 (MULT_EXPR, uint32_type_node,
		      m_parent.m_cf->expand_or_call_builtin
		      (BRIG_OPCODE_WORKGROUPSIZE, BRIG_TYPE_U32,
		       uint32_type_node, uint32_0),
		      m_parent.m_cf->expand_or_call_builtin
		      (BRIG_OPCODE_WORKGROUPSIZE, BRIG_TYPE_U32,
		       uint32_type_node, uint32_1));

	  local_size
	    = build2 (MULT_EXPR, uint32_type_node,
		      m_parent.m_cf->expand_or_call_builtin
		      (BRIG_OPCODE_WORKGROUPSIZE, BRIG_TYPE_U32,
		       uint32_type_node, uint32_2),
		      local_size);

	  tree var_region
	    = build2 (MULT_EXPR, uint32_type_node,
		      build_int_cst (uint32_type_node, offset), local_size);

	  tree_stl_vec operands;
	  tree pos
	    = build2 (MULT_EXPR, uint32_type_node,
		      build_int_cst (uint32_type_node,
				     m_parent.private_variable_size (var_name)),
		      m_parent.m_cf->expand_or_call_builtin
		      (BRIG_OPCODE_WORKITEMFLATID, BRIG_TYPE_U32,
		       uint32_type_node, operands));

	  tree var_offset
	    = build2 (PLUS_EXPR, uint32_type_node, var_region, pos);

	  /* In case of LDA this is returned directly as an integer value.
	     For other mem-related instructions, we will convert this segment
	     offset to a flat address by adding it as an offset to a (private
	     or group) base pointer later on.  Same applies to group_var_offset.  */
	  symbol_base
	    = m_parent.m_cf->add_temp_var ("priv_var_offset",
					   convert (size_type_node,
						    var_offset));
	}
      else if (segment == BRIG_SEGMENT_ARG)
	{
	  tree arg_var_decl;
	  if (m_parent.m_cf->m_ret_value_brig_var == arg_symbol)
	    arg_var_decl = m_parent.m_cf->m_ret_temp;
	  else
	    arg_var_decl = m_parent.m_cf->arg_variable (arg_symbol);

	  gcc_assert (arg_var_decl != NULL_TREE);

	  tree ptype = build_pointer_type (instr_type);

	  if (arg_symbol->type & BRIG_TYPE_ARRAY)
	    {

	      /* Two different type of array references in case of arguments
		 depending where they are referred at.  In the caller (argument
		 segment), the reference is to an array object and
		 in the callee, the array object has been passed as a pointer
		 to the array object.  */

	      if (POINTER_TYPE_P (TREE_TYPE (arg_var_decl)))
		symbol_base = build_resize_convert_view (ptype, arg_var_decl);
	      else
		{
		  /* In case we are referring to an array (the argument in
		     call site), use its element zero as the base address.  */
		  tree element_zero
		    = build4 (ARRAY_REF, TREE_TYPE (TREE_TYPE (arg_var_decl)),
			      arg_var_decl, integer_zero_node, NULL_TREE,
			      NULL_TREE);
		  symbol_base = build1 (ADDR_EXPR, ptype, element_zero);
		}
	    }
	  else
	    symbol_base = build1 (ADDR_EXPR, ptype, arg_var_decl);
	}
      else
	{
	  tree global_var_decl = m_parent.global_variable (var_name);

	  /* In case the global variable hasn't been defined (yet),
	     use the host def indirection ptr variable.  */
	  if (global_var_decl == NULL_TREE)
	    {
	      std::string host_ptr_name
		= std::string (PHSA_HOST_DEF_PTR_PREFIX) + var_name;
	      tree host_defined_ptr = m_parent.global_variable (host_ptr_name);
	      gcc_assert (host_defined_ptr != NULL_TREE);
	      symbol_base = host_defined_ptr;
	    }
	  else
	    {
	      gcc_assert (global_var_decl != NULL_TREE);

	      tree ptype = build_pointer_type (instr_type);
	      symbol_base = build1 (ADDR_EXPR, ptype, global_var_decl);
	    }
	}
    }

  if (brig_inst.opcode != BRIG_OPCODE_LDA)
    {
      /* In case of lda_* we want to return the segment address because it's
	 used as a value, perhaps in address computation and later converted
	 explicitly to a flat address.

	 In case of other instructions with memory operands we produce the flat
	 address directly here (assuming the target does not have a separate
	 address space for group/private segments for now).  */
      if (segment == BRIG_SEGMENT_GROUP)
	symbol_base = m_parent.m_cf->m_group_base_arg;
      else if (segment == BRIG_SEGMENT_PRIVATE
	       || segment == BRIG_SEGMENT_SPILL)
	{
	  if (symbol_base != NULL_TREE)
	    symbol_base = build2 (POINTER_PLUS_EXPR, ptr_type_node,
				  m_parent.m_cf->m_private_base_arg,
				  symbol_base);
	  else
	    symbol_base = m_parent.m_cf->m_private_base_arg;
	}
    }

  if (addr_operand.reg != 0)
    {
      const BrigOperandRegister *mem_base_reg
	= (const BrigOperandRegister *) m_parent.get_brig_operand_entry
	(addr_operand.reg);
      tree base_reg_var = m_parent.m_cf->get_m_var_declfor_reg (mem_base_reg);
      tree as_uint = build_reinterpret_to_uint (base_reg_var);
      var_offset = convert_to_pointer (ptr_type_node, as_uint);

      gcc_assert (var_offset != NULL_TREE);
    }
  /* The pointer type we use to access the memory.  Should be of the
     width of the load/store instruction, not the target/data
     register.  */
  tree ptype = build_pointer_type (instr_type);

  gcc_assert (ptype != NULL_TREE);

  tree addr = NULL_TREE;
  if (symbol_base != NULL_TREE && var_offset != NULL_TREE)
    /* The most complex addressing mode: symbol + reg [+ const offset].  */
    addr = build2 (POINTER_PLUS_EXPR, ptr_type_node,
		   convert (ptr_type_node, symbol_base),
		   convert (size_type_node, var_offset));
  else if (var_offset != NULL)
    addr = var_offset;
  else if (symbol_base != NULL)
    addr = symbol_base;

  if (const_offset != NULL_TREE)
    {
      if (addr == NULL_TREE)
	/* At least direct module-scope global group symbol access with LDA
	   has only the const_offset.  Group base ptr is not added as LDA should
	   return the segment address, not the flattened one.  */
	addr = const_offset;
      else
	addr = build2 (POINTER_PLUS_EXPR, ptr_type_node,
		       addr, convert (size_type_node, const_offset));
    }

  /* We might have two const offsets in case of group or private arrays
     which have the first offset to the incoming group/private pointer
     arg, and the second one an offset to it. It's also legal to have
     a reference with a zero constant offset but no symbol.  I've seen
     codes that reference kernarg segment like this.  Thus, if at this
     point there is no address expression at all we assume it's an
     access to offset 0. */
  uint64_t offs = gccbrig_to_uint64_t (addr_operand.offset);
  if (offs > 0 || addr == NULL_TREE)
    {
      /* In large mode, the offset is treated as 32bits unless it's
	 global, readonly or kernarg address space.
	 See:
	 http://www.hsafoundation.com/html_spec111/HSA_Library.htm
	 #PRM/Topics/02_ProgModel/small_and_large_machine_models.htm
	 #table_machine_model_data_sizes */

      int is64b_offset = segment == BRIG_SEGMENT_GLOBAL
	|| segment == BRIG_SEGMENT_READONLY
	|| segment == BRIG_SEGMENT_KERNARG;

      /* The original offset is signed and should be sign
	 extended for the pointer arithmetics.  */
      tree const_offset_2 = is64b_offset
        ? build_int_cst (size_type_node, offs)
        : convert (long_integer_type_node,
                   build_int_cst (integer_type_node, offs));

      if (addr == NULL_TREE)
	addr = const_offset_2;
      else
	addr = build2 (POINTER_PLUS_EXPR, ptr_type_node,
		       /* Addr can be a constant offset in case this is
			  a private array access.  */
		       convert (ptr_type_node, addr),
		       convert (size_type_node, const_offset_2));
    }

  gcc_assert (addr != NULL_TREE);
  return convert_to_pointer (ptype, addr);
}

/* Builds a tree operand with the given OPERAND_INDEX for the given
   BRIG_INST with the desired tree OPERAND_TYPE.  OPERAND_TYPE can
   be NULL in case the type is forced by the BRIG_INST type.  */

tree
brig_code_entry_handler::build_tree_operand_from_brig
  (const BrigInstBase *brig_inst, tree operand_type, size_t operand_index)
{
  const BrigData *operand_entries
    = m_parent.get_brig_data_entry (brig_inst->operands);

  uint32_t operand_offset
    = ((const uint32_t *) &operand_entries->bytes)[operand_index];
  const BrigBase *operand_data
    = m_parent.get_brig_operand_entry (operand_offset);

  bool inputp = !gccbrig_hsa_opcode_op_output_p (brig_inst->opcode,
						 operand_index);
  return build_tree_operand (*brig_inst, *operand_data, operand_type, inputp);
}

/* Builds a single (scalar) constant initialized element of type
   ELEMENT_TYPE from the buffer pointed to by NEXT_DATA.  */

tree
brig_code_entry_handler::build_tree_cst_element
  (BrigType16_t element_type, const unsigned char *next_data) const
{

  tree tree_element_type = gccbrig_tree_type_for_hsa_type (element_type);

  tree cst;
  switch (element_type)
    {
    case BRIG_TYPE_F16:
      {
	HOST_WIDE_INT low = *(const uint16_t *) next_data;
	cst = build_int_cst (uint16_type_node, low);
	break;
      }
    case BRIG_TYPE_F32:
      {
	REAL_VALUE_TYPE val;
	ieee_single_format.decode (&ieee_single_format, &val,
				   (const long *) next_data);
	cst = build_real (tree_element_type, val);
	break;
      }
    case BRIG_TYPE_F64:
      {
	long data[2];
	data[0] = *(const uint32_t *) next_data;
	data[1] = *(const uint32_t *) (next_data + 4);
	REAL_VALUE_TYPE val;
	ieee_double_format.decode (&ieee_double_format, &val, data);
	cst = build_real (tree_element_type, val);
	break;
      }
    case BRIG_TYPE_S8:
    case BRIG_TYPE_S16:
    case BRIG_TYPE_S32:
    case BRIG_TYPE_S64:
      {
	HOST_WIDE_INT low = *(const int64_t *) next_data;
	cst = build_int_cst (tree_element_type, low);
	break;
      }
    case BRIG_TYPE_U8:
    case BRIG_TYPE_U16:
    case BRIG_TYPE_U32:
    case BRIG_TYPE_U64:
      {
	unsigned HOST_WIDE_INT low = *(const uint64_t *) next_data;
	cst = build_int_cstu (tree_element_type, low);
	break;
      }
    case BRIG_TYPE_SIG64:
      {
	unsigned HOST_WIDE_INT low = *(const uint64_t *) next_data;
	cst = build_int_cstu (uint64_type_node, low);
	break;
      }
    case BRIG_TYPE_SIG32:
      {
	unsigned HOST_WIDE_INT low = *(const uint64_t *) next_data;
	cst = build_int_cstu (uint32_type_node, low);
	break;
      }
    default:
      gcc_unreachable ();
      return NULL_TREE;
    }
  return cst;
}

/* Produce a tree constant type for the given BRIG constant (BRIG_CONST).
   TYPE should be the forced instruction type, otherwise the type is
   dictated by the BRIG_CONST.  */

tree
brig_code_entry_handler::get_tree_cst_for_hsa_operand
  (const BrigOperandConstantBytes *brig_const, tree type) const
{
  const BrigData *data = m_parent.get_brig_data_entry (brig_const->bytes);

  tree cst = NULL_TREE;

  if (type == NULL_TREE)
    type = gccbrig_tree_type_for_hsa_type (brig_const->type);

  /* The type of a single (scalar) element inside an array,
     vector or an array of vectors.  */
  BrigType16_t scalar_element_type
    = brig_const->type & BRIG_TYPE_BASE_MASK;
  tree tree_element_type = type;

  vec<constructor_elt, va_gc> *constructor_vals = NULL;

  if (TREE_CODE (type) == ARRAY_TYPE)
    tree_element_type = TREE_TYPE (type);

  size_t bytes_left = data->byteCount;
  const unsigned char *next_data = data->bytes;
  size_t scalar_element_size
    = gccbrig_hsa_type_bit_size (scalar_element_type) / BITS_PER_UNIT;

  while (bytes_left > 0)
    {
      if (VECTOR_TYPE_P (tree_element_type))
	{
	  /* In case of vector type elements (or sole vectors),
	     create a vector ctor.  */
	  size_t element_count
	    = gccbrig_type_vector_subparts (tree_element_type);
	  if (bytes_left < scalar_element_size * element_count)
	    fatal_error (UNKNOWN_LOCATION,
			 "Not enough bytes left for the initializer "
			 "(%lu need %lu).", (unsigned long) bytes_left,
			 (unsigned long) (scalar_element_size
					  * element_count));

	  vec<constructor_elt, va_gc> *vec_els = NULL;
	  for (size_t i = 0; i < element_count; ++i)
	    {
	      tree element
		= build_tree_cst_element (scalar_element_type, next_data);
	      CONSTRUCTOR_APPEND_ELT (vec_els, NULL_TREE, element);
	      bytes_left -= scalar_element_size;
	      next_data += scalar_element_size;
	    }
	  cst = build_vector_from_ctor (tree_element_type, vec_els);
	}
      else
	{
	  if (bytes_left < scalar_element_size)
	    fatal_error (UNKNOWN_LOCATION,
			 "Not enough bytes left for the initializer "
			 "(%lu need %lu).", (unsigned long) bytes_left,
			 (unsigned long) scalar_element_size);
	  cst = build_tree_cst_element (scalar_element_type, next_data);
	  bytes_left -= scalar_element_size;
	  next_data += scalar_element_size;
	}
      CONSTRUCTOR_APPEND_ELT (constructor_vals, NULL_TREE, cst);
    }

  if (TREE_CODE (type) == ARRAY_TYPE)
    return build_constructor (type, constructor_vals);
  else
    return cst;
}

/* Return the matching tree instruction arithmetics type for the
   given BRIG_TYPE.  The aritmethics type is the one with which
   computation is done (in contrast to the storage type).  F16
   arithmetics type is emulated using F32 for now.  */

tree
brig_code_entry_handler::get_tree_expr_type_for_hsa_type
  (BrigType16_t brig_type) const
{
  BrigType16_t brig_inner_type = brig_type & BRIG_TYPE_BASE_MASK;
  if (brig_inner_type == BRIG_TYPE_F16)
    {
      if (brig_inner_type == brig_type)
	return m_parent.s_fp32_type;
      size_t element_count = gccbrig_hsa_type_bit_size (brig_type) / 16;
      return build_vector_type (m_parent.s_fp32_type, element_count);
    }
  else
    return gccbrig_tree_type_for_hsa_type (brig_type);
}

/* Return the correct GENERIC type for storing comparison results
   of operand with the type given in SOURCE_TYPE.  */

tree
brig_code_entry_handler::get_comparison_result_type (tree source_type)
{
  if (VECTOR_TYPE_P (source_type))
    {
      size_t element_size = int_size_in_bytes (TREE_TYPE (source_type));
      return build_vector_type
	(build_nonstandard_boolean_type (element_size * BITS_PER_UNIT),
	 gccbrig_type_vector_subparts (source_type));
    }
  else
    return gccbrig_tree_type_for_hsa_type (BRIG_TYPE_B1);
}

/* Creates a FP32 to FP16 conversion call, assuming the source and destination
   are FP32 type variables.  */

tree
brig_code_entry_handler::build_f2h_conversion (tree source)
{
  return float_to_half () (*this, source);
}

/* Creates a FP16 to FP32 conversion call, assuming the source and destination
   are FP32 type variables.  */

tree
brig_code_entry_handler::build_h2f_conversion (tree source)
{
  return half_to_float () (*this, source);
}

/* Builds and "normalizes" the dest and source operands for the instruction
   execution; converts the input operands to the expected instruction type,
   performs half to float conversions, constant to correct type variable,
   and flush to zero (if applicable).  */

tree_stl_vec
brig_code_entry_handler::build_operands (const BrigInstBase &brig_inst)
{
  return build_or_analyze_operands (brig_inst, false);
}

void
brig_code_entry_handler::analyze_operands (const BrigInstBase &brig_inst)
{
  build_or_analyze_operands (brig_inst, true);
}

/* Implements both the build_operands () and analyze_operands () call
   so changes go in tandem.  Performs build_operands () when ANALYZE
   is false.  Otherwise, only analyze operands and return empty
   list.

   If analyzing record each HSA register operand with the
   corresponding resolved operand tree type to
   brig_to_generic::m_fn_regs_use_index.  */

tree_stl_vec
brig_code_entry_handler::
build_or_analyze_operands (const BrigInstBase &brig_inst, bool analyze)
{
  /* Flush to zero.  */
  bool ftz = false;
  const BrigBase *base = &brig_inst.base;

  if (base->kind == BRIG_KIND_INST_MOD)
    {
      const BrigInstMod *mod = (const BrigInstMod *) base;
      ftz = mod->modifier & BRIG_ALU_FTZ;
    }
  else if (base->kind == BRIG_KIND_INST_CMP)
    {
      const BrigInstCmp *cmp = (const BrigInstCmp *) base;
      ftz = cmp->modifier & BRIG_ALU_FTZ;
    }

  bool is_vec_instr = hsa_type_packed_p (brig_inst.type);

  size_t element_count;
  if (is_vec_instr)
    {
      BrigType16_t brig_element_type = brig_inst.type & BRIG_TYPE_BASE_MASK;
      element_count = gccbrig_hsa_type_bit_size (brig_inst.type)
		      / gccbrig_hsa_type_bit_size (brig_element_type);
    }
  else
    element_count = 1;

  bool is_fp16_arith = false;

  tree src_type;
  tree dest_type;
  if (base->kind == BRIG_KIND_INST_CMP)
    {
      const BrigInstCmp *cmp_inst = (const BrigInstCmp *) base;
      src_type = gccbrig_tree_type_for_hsa_type (cmp_inst->sourceType);
      dest_type = gccbrig_tree_type_for_hsa_type (brig_inst.type);
      is_fp16_arith
	= (cmp_inst->sourceType & BRIG_TYPE_BASE_MASK) == BRIG_TYPE_F16;
    }
  else if (base->kind == BRIG_KIND_INST_SOURCE_TYPE)
    {
      const BrigInstSourceType *src_type_inst
	= (const BrigInstSourceType *) base;
      src_type = gccbrig_tree_type_for_hsa_type (src_type_inst->sourceType);
      dest_type = gccbrig_tree_type_for_hsa_type (brig_inst.type);
      is_fp16_arith
	= (src_type_inst->sourceType & BRIG_TYPE_BASE_MASK) == BRIG_TYPE_F16
	&& !gccbrig_is_bit_operation (brig_inst.opcode);
    }
  else if (base->kind == BRIG_KIND_INST_SEG_CVT)
    {
      const BrigInstSegCvt *seg_cvt_inst = (const BrigInstSegCvt *) base;
      src_type = gccbrig_tree_type_for_hsa_type (seg_cvt_inst->sourceType);
      dest_type = gccbrig_tree_type_for_hsa_type (brig_inst.type);
    }
  else if (base->kind == BRIG_KIND_INST_MEM)
    {
      src_type = gccbrig_tree_type_for_hsa_type (brig_inst.type);
      dest_type = src_type;
      /* With mem instructions we don't want to cast the fp16
	 back and forth between fp32, because the load/stores
	 are not specific to the data type.  */
      is_fp16_arith = false;
    }
  else if (base->kind == BRIG_KIND_INST_CVT)
    {
      const BrigInstCvt *cvt_inst = (const BrigInstCvt *) base;

      src_type = gccbrig_tree_type_for_hsa_type (cvt_inst->sourceType);
      dest_type = gccbrig_tree_type_for_hsa_type (brig_inst.type);
    }
  else
    {
      switch (brig_inst.opcode)
	{
	case BRIG_OPCODE_INITFBAR:
	case BRIG_OPCODE_JOINFBAR:
	case BRIG_OPCODE_WAITFBAR:
	case BRIG_OPCODE_ARRIVEFBAR:
	case BRIG_OPCODE_LEAVEFBAR:
	case BRIG_OPCODE_RELEASEFBAR:
	  src_type = uint32_type_node;
	  break;
	default:
	  src_type = gccbrig_tree_type_for_hsa_type (brig_inst.type);
	  break;
	}
      dest_type = src_type;
      is_fp16_arith
	= !gccbrig_is_bit_operation (brig_inst.opcode)
	&& (brig_inst.type & BRIG_TYPE_BASE_MASK) == BRIG_TYPE_F16;
    }

  /* Halfs are a tricky special case: their "storage format" is u16, but
     scalars are stored in 32b regs while packed f16 are... well packed.  */
  tree half_storage_type = element_count > 1
			     ? gccbrig_tree_type_for_hsa_type (brig_inst.type)
			     : uint32_type_node;

  const BrigData *operand_entries
    = m_parent.get_brig_data_entry (brig_inst.operands);
  std::vector<tree> operands;
  for (size_t i = 0; i < operand_entries->byteCount / 4; ++i)
    {
      uint32_t operand_offset = ((const uint32_t *) &operand_entries->bytes)[i];
      const BrigBase *operand_data
	= m_parent.get_brig_operand_entry (operand_offset);

      const bool is_output
	= gccbrig_hsa_opcode_op_output_p (brig_inst.opcode, i);

      tree operand_type = is_output ? dest_type : src_type;

      bool half_to_float = is_fp16_arith;

      /* Special cases for operand types.  */
      if ((brig_inst.opcode == BRIG_OPCODE_SHL
	   || brig_inst.opcode == BRIG_OPCODE_SHR)
	  && i == 2)
	  /* The shift amount is always a scalar.  */
	operand_type
	  = VECTOR_TYPE_P (src_type) ? TREE_TYPE (src_type) : src_type;
      else if (brig_inst.opcode == BRIG_OPCODE_SHUFFLE)
	{
	  if (i == 3)
	    /* HSAIL shuffle inputs the MASK vector as tightly packed bits
	       while GENERIC VEC_PERM_EXPR expects the mask elements to be
	       of the same size as the elements in the input vectors.  Let's
	       cast to a scalar type here and convert to the VEC_PERM_EXPR
	       format in instruction handling.  There are no arbitrary bit
	       width int types in GENERIC so we cannot use the original
	       vector type.  */
	    operand_type = uint32_type_node;
	  else
	    /* Always treat the element as unsigned ints to avoid
	       sign extensions/negative offsets with masks, which
	       are expected to be of the same element type as the
	       data in VEC_PERM_EXPR.  With shuffles the data type
	       should not matter as it's a "raw operation".  */
	    operand_type = get_unsigned_int_type (operand_type);
	}
      else if (brig_inst.opcode == BRIG_OPCODE_PACK)
	{
	  if (i == 1)
	    operand_type = get_unsigned_int_type (dest_type);
	  else if (i == 2)
	    operand_type = get_unsigned_int_type (TREE_TYPE (dest_type));
	  else if (i == 3)
	    operand_type = uint32_type_node;
	}
      else if (brig_inst.opcode == BRIG_OPCODE_UNPACK && i == 2)
	operand_type = uint32_type_node;
      else if (brig_inst.opcode == BRIG_OPCODE_SAD && i == 3)
	operand_type = uint32_type_node;
      else if (brig_inst.opcode == BRIG_OPCODE_CLASS && i == 2)
	{
	  operand_type = uint32_type_node;
	  half_to_float = false;
	}
      else if (brig_inst.opcode == BRIG_OPCODE_ACTIVELANEPERMUTE && i == 4)
	{
	  operand_type = uint32_type_node;
	}
      else if (half_to_float)
	/* Treat the operands as the storage type at this point.  */
	operand_type = half_storage_type;

      if (analyze)
	{
	  if (operand_data->kind == BRIG_KIND_OPERAND_REGISTER)
	    {
	      const BrigOperandRegister &brig_reg
		= (const BrigOperandRegister &) *operand_data;
	      m_parent.add_reg_used_as_type (brig_reg, operand_type);
	    }
	  continue;
	}

      tree operand = build_tree_operand (brig_inst, *operand_data, operand_type,
					 !is_output);
      gcc_assert (operand);

      /* Cast/convert the inputs to correct types as expected by the GENERIC
	 opcode instruction.  */
      if (!is_output)
	{
	  if (half_to_float)
	    operand = build_h2f_conversion
	      (build_resize_convert_view (half_storage_type, operand));
	  else if (TREE_CODE (operand) != LABEL_DECL
		   && TREE_CODE (operand) != TREE_VEC
		   && operand_data->kind != BRIG_KIND_OPERAND_ADDRESS
		   && operand_data->kind != BRIG_KIND_OPERAND_OPERAND_LIST)
	    {
	      operand = build_resize_convert_view (operand_type, operand);
	    }
	  else if (brig_inst.opcode == BRIG_OPCODE_SHUFFLE)
	    /* Force the operand type to be treated as the raw type.  */
	    operand = build_resize_convert_view (operand_type, operand);

	  if (brig_inst.opcode == BRIG_OPCODE_CMOV && i == 1)
	    {
	      /* gcc expects the lower bit to be 1 (or all ones in case of
		 vectors) while CMOV assumes false iff 0.  Convert the input
		 here to what gcc likes by generating
		 'operand = operand != 0'.  */
	      tree cmp_res_type = get_comparison_result_type (operand_type);
	      operand = build2 (NE_EXPR, cmp_res_type, operand,
				build_zero_cst (TREE_TYPE (operand)));
	    }

	  if (ftz)
	    operand = flush_to_zero (is_fp16_arith) (*this, operand);
	}
      operands.push_back (operand);
    }
  return operands;
}

/* Build the GENERIC for assigning the result of an instruction to the result
   "register" (variable).  BRIG_INST is the original brig instruction,
   OUTPUT the result variable/register, INST_EXPR the one producing the
   result.  Required bitcasts and fp32 to fp16 conversions are added as
   well.  */

tree
brig_code_entry_handler::build_output_assignment (const BrigInstBase &brig_inst,
						  tree output, tree inst_expr)
{
  /* The result/input type might be different from the output register
     variable type (can be any type; see get_m_var_declfor_reg @
     brig-function.cc).  */
  tree output_type = TREE_TYPE (output);
  bool is_fp16 = (brig_inst.type & BRIG_TYPE_BASE_MASK) == BRIG_TYPE_F16
		 && brig_inst.base.kind != BRIG_KIND_INST_MEM
		 && !gccbrig_is_bit_operation (brig_inst.opcode);

  /* Flush to zero.  */
  bool ftz = false;
  const BrigBase *base = &brig_inst.base;

  if (m_parent.m_cf->is_id_val (inst_expr))
    inst_expr = m_parent.m_cf->id_val (inst_expr);

  tree input_type = TREE_TYPE (inst_expr);

  m_parent.m_cf->add_reg_var_update (output, inst_expr);

  if (base->kind == BRIG_KIND_INST_MOD)
    {
      const BrigInstMod *mod = (const BrigInstMod *) base;
      ftz = mod->modifier & BRIG_ALU_FTZ;
    }
  else if (base->kind == BRIG_KIND_INST_CMP)
    {
      const BrigInstCmp *cmp = (const BrigInstCmp *) base;
      ftz = cmp->modifier & BRIG_ALU_FTZ;
    }

  if (TREE_CODE (inst_expr) == CALL_EXPR)
    {
      tree func_decl = TREE_OPERAND (TREE_OPERAND (inst_expr, 1), 0);
      input_type = TREE_TYPE (TREE_TYPE (func_decl));
    }

  if (ftz && (VECTOR_FLOAT_TYPE_P (TREE_TYPE (inst_expr))
	      || SCALAR_FLOAT_TYPE_P (TREE_TYPE (inst_expr)) || is_fp16))
    {
      /* Ensure we don't duplicate the arithmetics to the arguments of the bit
	 field reference operators.  */
      inst_expr = m_parent.m_cf->add_temp_var ("before_ftz", inst_expr);
      inst_expr = flush_to_zero (is_fp16) (*this, inst_expr);
    }

  if (is_fp16)
    {
      inst_expr = m_parent.m_cf->add_temp_var ("before_f2h", inst_expr);
      tree f2h_output = build_f2h_conversion (inst_expr);
      tree conv = build_resize_convert_view (output_type, f2h_output);
      tree assign = build2 (MODIFY_EXPR, output_type, output, conv);
      m_parent.m_cf->append_statement (assign);
      return assign;
    }
  else if (VECTOR_TYPE_P (output_type) && TREE_CODE (output) == CONSTRUCTOR)
    {
      /* Expand/unpack the input value to the given vector elements.  */
      size_t i;
      tree input = inst_expr;
      tree element_type = gccbrig_tree_type_for_hsa_type (brig_inst.type);
      tree element;
      tree last_assign = NULL_TREE;
      FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (output), i, element)
	{
	  tree element_ref
	    = build3 (BIT_FIELD_REF, element_type, input,
		      TYPE_SIZE (element_type),
		      bitsize_int (i * int_size_in_bytes (element_type)
				   *  BITS_PER_UNIT));

	  last_assign
	    = build_output_assignment (brig_inst, element, element_ref);
	}
      return last_assign;
    }
  else
    {
      /* All we do here is to bitcast the result and store it to the
	 'register' (variable).  Mainly need to take care of differing
	 bitwidths.  */
      size_t src_width = int_size_in_bytes (input_type);
      size_t dst_width = int_size_in_bytes (output_type);
      tree input = inst_expr;
      /* Integer results are extended to the target register width, using
	 the same sign as the inst_expr.  */
      if (INTEGRAL_TYPE_P (TREE_TYPE (input)) && src_width != dst_width)
	{
	  bool unsigned_p = TYPE_UNSIGNED (TREE_TYPE (input));
	  tree resized_type
	    = build_nonstandard_integer_type (dst_width * BITS_PER_UNIT,
					      unsigned_p);
	  input = convert_to_integer (resized_type, input);
	}
      input = build_resize_convert_view (output_type, input);
      tree assign = build2 (MODIFY_EXPR, output_type, output, input);
      m_parent.m_cf->append_statement (assign);
      return assign;
    }
  return NULL_TREE;
}

/* Appends a GENERIC statement (STMT) to the currently constructed function.  */

void
brig_code_entry_handler::append_statement (tree stmt)
{
  m_parent.m_cf->append_statement (stmt);
}

/* Visits the element(s) in the OPERAND, calling HANDLER to each of them.  */

tree
tree_element_unary_visitor::operator () (brig_code_entry_handler &handler,
					tree operand)
{
  if (VECTOR_TYPE_P (TREE_TYPE (operand)))
    {
      size_t vec_size = int_size_in_bytes (TREE_TYPE (operand));
      size_t element_size = int_size_in_bytes (TREE_TYPE (TREE_TYPE (operand)));
      size_t element_count = vec_size / element_size;

      tree input_element_type = TREE_TYPE (TREE_TYPE (operand));
      tree output_element_type = NULL_TREE;

      vec<constructor_elt, va_gc> *constructor_vals = NULL;
      for (size_t i = 0; i < element_count; ++i)
	{
	  tree element = build3 (BIT_FIELD_REF, input_element_type, operand,
				 TYPE_SIZE (input_element_type),
				 bitsize_int (i * element_size
					      * BITS_PER_UNIT));

	  tree output = visit_element (handler, element);
	  output_element_type = TREE_TYPE (output);

	  CONSTRUCTOR_APPEND_ELT (constructor_vals, NULL_TREE, output);
	}

      tree vec_type = build_vector_type (output_element_type, element_count);

      /* build_constructor creates a vector type which is not a vector_cst
	 that requires compile time constant elements.  */
      tree vec = build_constructor (vec_type, constructor_vals);

      /* Add a temp variable for readability.  */
      tree tmp_var = create_tmp_var (vec_type, "vec_out");
      tree vec_tmp_assign
	= build2 (MODIFY_EXPR, TREE_TYPE (tmp_var), tmp_var, vec);
      handler.append_statement (vec_tmp_assign);
      return tmp_var;
    }
  else
    return visit_element (handler, operand);
}

/* Visits the element pair(s) in the OPERAND0 and OPERAND1, calling HANDLER
   to each of them.  */

tree
tree_element_binary_visitor::operator () (brig_code_entry_handler &handler,
					 tree operand0, tree operand1)
{
  if (VECTOR_TYPE_P (TREE_TYPE (operand0)))
    {
      gcc_assert (VECTOR_TYPE_P (TREE_TYPE (operand1)));
      size_t vec_size = int_size_in_bytes (TREE_TYPE (operand0));
      size_t element_size
	= int_size_in_bytes (TREE_TYPE (TREE_TYPE (operand0)));
      size_t element_count = vec_size / element_size;

      tree input_element_type = TREE_TYPE (TREE_TYPE (operand0));
      tree output_element_type = NULL_TREE;

      vec<constructor_elt, va_gc> *constructor_vals = NULL;
      for (size_t i = 0; i < element_count; ++i)
	{

	  tree element0 = build3 (BIT_FIELD_REF, input_element_type, operand0,
				  TYPE_SIZE (input_element_type),
				  bitsize_int (i * element_size
					       * BITS_PER_UNIT));

	  tree element1 = build3 (BIT_FIELD_REF, input_element_type, operand1,
				  TYPE_SIZE (input_element_type),
				  bitsize_int (i * element_size
					       * BITS_PER_UNIT));

	  tree output = visit_element (handler, element0, element1);
	  output_element_type = TREE_TYPE (output);

	  CONSTRUCTOR_APPEND_ELT (constructor_vals, NULL_TREE, output);
	}

      tree vec_type = build_vector_type (output_element_type, element_count);

      /* build_constructor creates a vector type which is not a vector_cst
	 that requires compile time constant elements.  */
      tree vec = build_constructor (vec_type, constructor_vals);

      /* Add a temp variable for readability.  */
      tree tmp_var = create_tmp_var (vec_type, "vec_out");
      tree vec_tmp_assign
	= build2 (MODIFY_EXPR, TREE_TYPE (tmp_var), tmp_var, vec);
      handler.append_statement (vec_tmp_assign);
      return tmp_var;
    }
  else
    return visit_element (handler, operand0, operand1);
}

/* Generates GENERIC code that flushes the visited element to zero.  */

tree
flush_to_zero::visit_element (brig_code_entry_handler &, tree operand)
{
  size_t size = int_size_in_bytes (TREE_TYPE (operand));
  if (size == 4)
    {
      tree built_in
	= (m_fp16) ? builtin_decl_explicit (BUILT_IN_HSAIL_FTZ_F32_F16) :
	builtin_decl_explicit (BUILT_IN_HSAIL_FTZ_F32);

      return call_builtin (built_in, 1, float_type_node, float_type_node,
			   operand);
    }
  else if (size == 8)
    {
      return call_builtin (builtin_decl_explicit (BUILT_IN_HSAIL_FTZ_F64), 1,
			   double_type_node, double_type_node, operand);
    }
  else
    gcc_unreachable ();
  return NULL_TREE;
}

/* Generates GENERIC code that converts a single precision float to half
   precision float.  */

tree
float_to_half::visit_element (brig_code_entry_handler &caller, tree operand)
{
  tree built_in = builtin_decl_explicit (BUILT_IN_HSAIL_F32_TO_F16);

  tree casted_operand = build_resize_convert_view (uint32_type_node, operand);

  tree call = call_builtin (built_in, 1, uint16_type_node, uint32_type_node,
			    casted_operand);
  tree output
    = create_tmp_var (TREE_TYPE (TREE_TYPE (built_in)), "fp16out");
  tree assign = build2 (MODIFY_EXPR, TREE_TYPE (output), output, call);
  caller.append_statement (assign);
  return output;
}

/* Generates GENERIC code that converts a half precision float to single
   precision float.  */

tree
half_to_float::visit_element (brig_code_entry_handler &caller, tree operand)
{
  tree built_in = builtin_decl_explicit (BUILT_IN_HSAIL_F16_TO_F32);
  tree truncated_source = convert_to_integer (uint16_type_node, operand);

  tree call
    = call_builtin (built_in, 1, uint32_type_node, uint16_type_node,
		    truncated_source);

  tree const_fp32_type
    = build_type_variant (brig_to_generic::s_fp32_type, 1, 0);

  tree output = create_tmp_var (const_fp32_type, "fp32out");
  tree casted_result
    = build_resize_convert_view (brig_to_generic::s_fp32_type, call);

  tree assign = build2 (MODIFY_EXPR, TREE_TYPE (output), output, casted_result);

  caller.append_statement (assign);

  return output;
}

/* Treats the INPUT as SRC_TYPE and sign or zero extends it to DEST_TYPE.  */

tree
brig_code_entry_handler::extend_int (tree input, tree dest_type, tree src_type)
{
  /* Extend integer conversions according to the destination's
     ext mode.  First we need to clip the input register to
     the possible smaller integer size to ensure the correct sign
     bit is extended.  */
  tree clipped_input = convert_to_integer (src_type, input);
  tree conversion_result;

  if (TYPE_UNSIGNED (src_type))
    conversion_result
      = convert_to_integer (unsigned_type_for (dest_type), clipped_input);
  else
    conversion_result
      = convert_to_integer (signed_type_for (dest_type), clipped_input);

  /* Treat the result as unsigned so we do not sign extend to the
     register width.  For some reason this GENERIC sequence sign
     extends to the s register:

     D.1541 = (signed char) s1;
     D.1542 = (signed short) D.1541;
     s0 = (unsigned int) D.1542
  */

  /* The converted result is then extended to the target register
     width, using the same sign as the destination.  */
  return convert_to_integer (dest_type, conversion_result);
}

/* Returns the integer constant value of the given node.
   If it's a cast, looks into the source of the cast.  */
HOST_WIDE_INT
brig_code_entry_handler::int_constant_value (tree node)
{
  tree n = node;
  if (TREE_CODE (n) == VIEW_CONVERT_EXPR)
    n = TREE_OPERAND (n, 0);
  return int_cst_value (n);
}
