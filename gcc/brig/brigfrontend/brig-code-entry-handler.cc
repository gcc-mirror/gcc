/* brig-code-entry-handler.cc -- a gccbrig base class
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

brig_code_entry_handler::builtin_map brig_code_entry_handler::s_custom_builtins;

brig_code_entry_handler::brig_code_entry_handler (brig_to_generic &parent)
  : brig_entry_handler (parent)
{
  if (s_custom_builtins.size () > 0) return;

  /* Populate the builtin index.  */
#undef DEF_HSAIL_ATOMIC_BUILTIN
#undef DEF_HSAIL_CVT_ZEROI_SAT_BUILTIN
#undef DEF_HSAIL_INTR_BUILTIN
#undef DEF_HSAIL_SAT_BUILTIN
#undef DEF_HSAIL_BUILTIN
#define DEF_HSAIL_BUILTIN(ENUM, HSAIL_OPCODE, HSAIL_TYPE, NAME, TYPE, ATTRS) \
  s_custom_builtins[std::make_pair (HSAIL_OPCODE, HSAIL_TYPE)]		\
    = builtin_decl_explicit (ENUM);

#include "brig-builtins.def"
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

      uint64_t offset = m_parent.group_variable_segment_offset
	(m_parent.get_mangled_name (fbar));

      return build_int_cst (uint32_type_node, offset);
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
	      {
		if (int_size_in_bytes (TREE_TYPE (element))
		    == int_size_in_bytes (operand_type)
		    && !INTEGRAL_TYPE_P (operand_type))
		  element = build1 (VIEW_CONVERT_EXPR, operand_type, element);
		else
		  element = convert (operand_type, element);
	      }

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

	  uint64_t offset = m_parent.group_variable_segment_offset (var_name);
	  const_offset = build_int_cst (size_type_node, offset);
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
		      expand_or_call_builtin (BRIG_OPCODE_WORKGROUPSIZE,
					      BRIG_TYPE_U32,
					      uint32_type_node, uint32_0),
		      expand_or_call_builtin (BRIG_OPCODE_WORKGROUPSIZE,
					      BRIG_TYPE_U32,
					      uint32_type_node, uint32_1));

	  local_size
	    = build2 (MULT_EXPR, uint32_type_node,
		      expand_or_call_builtin (BRIG_OPCODE_WORKGROUPSIZE,
					      BRIG_TYPE_U32,
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
		      expand_or_call_builtin (BRIG_OPCODE_WORKITEMFLATID,
					      BRIG_TYPE_U32,
					      uint32_type_node, operands));

	  tree var_offset
	    = build2 (PLUS_EXPR, uint32_type_node, var_region, pos);

	  /* In case of LDA this is returned directly as an integer value.
	     For other mem-related instructions, we will convert this segment
	     offset to a flat address by adding it as an offset to a (private
	     or group) base pointer later on.  Same applies to group_var_offset.  */
	  symbol_base
	    = add_temp_var ("priv_var_offset",
			    convert (size_type_node, var_offset));
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
		symbol_base = build_reinterpret_cast (ptype, arg_var_decl);
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
      var_offset = convert_to_pointer (ptr_type_node, base_reg_var);

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
      tree const_offset_2 = build_int_cst (size_type_node, offs);
      if (addr == NULL_TREE)
	addr = const_offset_2;
      else
	addr = build2 (POINTER_PLUS_EXPR, ptr_type_node,
		       addr, convert (size_type_node, const_offset_2));

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
  return build_tree_operand (*brig_inst, *operand_data, operand_type);
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
	  size_t element_count = TYPE_VECTOR_SUBPARTS (tree_element_type);
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

/* In case the HSA instruction must be implemented using a builtin,
   this function is called to get the correct builtin function.
   TYPE is the instruction tree type, BRIG_OPCODE the opcode of the
   brig instruction and BRIG_TYPE the brig instruction's type.  */

tree
brig_code_entry_handler::get_builtin_for_hsa_opcode
  (tree type, BrigOpcode16_t brig_opcode, BrigType16_t brig_type) const
{
  tree builtin = NULL_TREE;
  tree builtin_type = type;

  /* For vector types, first find the scalar version of the builtin.  */
  if (type != NULL_TREE && VECTOR_TYPE_P (type))
    builtin_type = TREE_TYPE (type);
  BrigType16_t brig_inner_type = brig_type & BRIG_TYPE_BASE_MASK;

  /* Some BRIG opcodes can use the same builtins for unsigned and
     signed types.  Force these cases to unsigned types.  */

  if (brig_opcode == BRIG_OPCODE_BORROW
      || brig_opcode == BRIG_OPCODE_CARRY
      || brig_opcode == BRIG_OPCODE_LASTBIT
      || brig_opcode == BRIG_OPCODE_BITINSERT)
    {
      if (brig_type == BRIG_TYPE_S32)
	brig_type = BRIG_TYPE_U32;
      else if (brig_type == BRIG_TYPE_S64)
	brig_type = BRIG_TYPE_U64;
    }

  switch (brig_opcode)
    {
    case BRIG_OPCODE_FLOOR:
      builtin = mathfn_built_in (builtin_type, BUILT_IN_FLOOR);
      break;
    case BRIG_OPCODE_CEIL:
      builtin = mathfn_built_in (builtin_type, BUILT_IN_CEIL);
      break;
    case BRIG_OPCODE_SQRT:
    case BRIG_OPCODE_NSQRT:
      builtin = mathfn_built_in (builtin_type, BUILT_IN_SQRT);
      break;
    case BRIG_OPCODE_RINT:
      builtin = mathfn_built_in (builtin_type, BUILT_IN_RINT);
      break;
    case BRIG_OPCODE_TRUNC:
      builtin = mathfn_built_in (builtin_type, BUILT_IN_TRUNC);
      break;
    case BRIG_OPCODE_COPYSIGN:
      builtin = mathfn_built_in (builtin_type, BUILT_IN_COPYSIGN);
      break;
    case BRIG_OPCODE_NSIN:
      builtin = mathfn_built_in (builtin_type, BUILT_IN_SIN);
      break;
    case BRIG_OPCODE_NLOG2:
      builtin = mathfn_built_in (builtin_type, BUILT_IN_LOG2);
      break;
    case BRIG_OPCODE_NEXP2:
      builtin = mathfn_built_in (builtin_type, BUILT_IN_EXP2);
      break;
    case BRIG_OPCODE_NFMA:
      builtin = mathfn_built_in (builtin_type, BUILT_IN_FMA);
      break;
    case BRIG_OPCODE_NCOS:
      builtin = mathfn_built_in (builtin_type, BUILT_IN_COS);
      break;
    case BRIG_OPCODE_POPCOUNT:
      /* Popcount should be typed by its argument type (the return value
	 is always u32).  Let's use a b64 version for also for b32 for now.  */
      return builtin_decl_explicit (BUILT_IN_POPCOUNTL);
    case BRIG_OPCODE_BORROW:
      /* Borrow uses the same builtin for unsigned and signed types.  */
      if (brig_type == BRIG_TYPE_S32 || brig_type == BRIG_TYPE_U32)
	return builtin_decl_explicit (BUILT_IN_HSAIL_BORROW_U32);
      else
	return builtin_decl_explicit (BUILT_IN_HSAIL_BORROW_U64);
    case BRIG_OPCODE_CARRY:
      /* Carry also uses the same builtin for unsigned and signed types.  */
      if (brig_type == BRIG_TYPE_S32 || brig_type == BRIG_TYPE_U32)
	return builtin_decl_explicit (BUILT_IN_HSAIL_CARRY_U32);
      else
	return builtin_decl_explicit (BUILT_IN_HSAIL_CARRY_U64);
    default:

      /* Use our builtin index for finding a proper builtin for the BRIG
	 opcode and BRIG type.  This takes care most of the builtin cases,
	 the special cases are handled in the separate 'case' statements
	 above.  */
      builtin_map::const_iterator i
	= s_custom_builtins.find (std::make_pair (brig_opcode, brig_type));
      if (i != s_custom_builtins.end ())
	return (*i).second;

      if (brig_inner_type != brig_type)
	{
	  /* Try to find a scalar built-in we could use.  */
	  i = s_custom_builtins.find
	    (std::make_pair (brig_opcode, brig_inner_type));
	  if (i != s_custom_builtins.end ())
	    return (*i).second;
	}

      /* In case this is an fp16 operation that is promoted to fp32,
	 try to find a fp32 scalar built-in.  */
      if (brig_inner_type == BRIG_TYPE_F16)
	{
	  i = s_custom_builtins.find
	    (std::make_pair (brig_opcode, BRIG_TYPE_F32));
	  if (i != s_custom_builtins.end ())
	    return (*i).second;
	}
      gcc_unreachable ();
    }

  if (VECTOR_TYPE_P (type) && builtin != NULL_TREE)
    {
      /* Try to find a vectorized version of the built-in.
	 TODO: properly assert that builtin is a mathfn builtin? */
      tree vec_builtin
	= targetm.vectorize.builtin_vectorized_function
	(builtin_mathfn_code (builtin), type, type);
      if (vec_builtin != NULL_TREE)
	return vec_builtin;
      else
	return builtin;
    }
  if (builtin == NULL_TREE)
    gcc_unreachable ();
  return builtin;
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
	 TYPE_VECTOR_SUBPARTS (source_type));
    }
  else
    return gccbrig_tree_type_for_hsa_type (BRIG_TYPE_B1);
}

/* Returns true in case the given opcode needs to know about work-item context
   data.  In such case the context data is passed as a pointer to a work-item
   context object, as the last argument in the builtin call.  */

bool
brig_code_entry_handler::needs_workitem_context_data
  (BrigOpcode16_t brig_opcode) const
{
  switch (brig_opcode)
    {
    case BRIG_OPCODE_WORKITEMABSID:
    case BRIG_OPCODE_WORKITEMFLATABSID:
    case BRIG_OPCODE_WORKITEMFLATID:
    case BRIG_OPCODE_CURRENTWORKITEMFLATID:
    case BRIG_OPCODE_WORKITEMID:
    case BRIG_OPCODE_WORKGROUPID:
    case BRIG_OPCODE_WORKGROUPSIZE:
    case BRIG_OPCODE_CURRENTWORKGROUPSIZE:
    case BRIG_OPCODE_GRIDGROUPS:
    case BRIG_OPCODE_GRIDSIZE:
    case BRIG_OPCODE_DIM:
    case BRIG_OPCODE_PACKETID:
    case BRIG_OPCODE_PACKETCOMPLETIONSIG:
    case BRIG_OPCODE_BARRIER:
    case BRIG_OPCODE_WAVEBARRIER:
    case BRIG_OPCODE_ARRIVEFBAR:
    case BRIG_OPCODE_INITFBAR:
    case BRIG_OPCODE_JOINFBAR:
    case BRIG_OPCODE_LEAVEFBAR:
    case BRIG_OPCODE_RELEASEFBAR:
    case BRIG_OPCODE_WAITFBAR:
    case BRIG_OPCODE_CUID:
    case BRIG_OPCODE_MAXCUID:
    case BRIG_OPCODE_DEBUGTRAP:
    case BRIG_OPCODE_GROUPBASEPTR:
    case BRIG_OPCODE_KERNARGBASEPTR:
    case BRIG_OPCODE_ALLOCA:
      return true;
    default:
      return false;
    };
}

/* Returns true in case the given opcode that would normally be generated
   as a builtin call can be expanded to tree nodes.  */

bool
brig_code_entry_handler::can_expand_builtin (BrigOpcode16_t brig_opcode) const
{
  switch (brig_opcode)
    {
    case BRIG_OPCODE_WORKITEMFLATABSID:
    case BRIG_OPCODE_WORKITEMFLATID:
    case BRIG_OPCODE_WORKITEMABSID:
    case BRIG_OPCODE_WORKGROUPSIZE:
    case BRIG_OPCODE_CURRENTWORKGROUPSIZE:
      /* TODO: expand more builtins.  */
      return true;
    default:
      return false;
    };
}

/* Try to expand the given builtin call to reuse a previously generated
   variable, if possible.  If not, just call the given builtin.
   BRIG_OPCODE and BRIG_TYPE identify the builtin's BRIG opcode/type,
   ARITH_TYPE its GENERIC type, and OPERANDS contains the builtin's
   input operands.  */

tree
brig_code_entry_handler::expand_or_call_builtin (BrigOpcode16_t brig_opcode,
						 BrigType16_t brig_type,
						 tree arith_type,
						 tree_stl_vec &operands)
{
  if (m_parent.m_cf->m_is_kernel && can_expand_builtin (brig_opcode))
    return expand_builtin (brig_opcode, operands);

  tree built_in
    = get_builtin_for_hsa_opcode (arith_type, brig_opcode, brig_type);

  if (!VECTOR_TYPE_P (TREE_TYPE (TREE_TYPE (built_in)))
      && arith_type != NULL_TREE && VECTOR_TYPE_P (arith_type)
      && brig_opcode != BRIG_OPCODE_LERP
      && brig_opcode != BRIG_OPCODE_PACKCVT
      && brig_opcode != BRIG_OPCODE_SAD
      && brig_opcode != BRIG_OPCODE_SADHI)
    {
      /* Call the scalar built-in for all elements in the vector.  */
      tree_stl_vec operand0_elements;
      if (operands.size () > 0)
	unpack (operands[0], operand0_elements);

      tree_stl_vec operand1_elements;
      if (operands.size () > 1)
	unpack (operands[1], operand1_elements);

      tree_stl_vec result_elements;

      for (size_t i = 0; i < TYPE_VECTOR_SUBPARTS (arith_type); ++i)
	{
	  tree_stl_vec call_operands;
	  if (operand0_elements.size () > 0)
	    call_operands.push_back (operand0_elements.at (i));

	  if (operand1_elements.size () > 0)
	    call_operands.push_back (operand1_elements.at (i));

	  result_elements.push_back
	    (expand_or_call_builtin (brig_opcode, brig_type,
				     TREE_TYPE (arith_type),
				     call_operands));
	}
      return pack (result_elements);
    }

  tree_stl_vec call_operands;
  tree_stl_vec operand_types;

  tree arg_type_chain = TYPE_ARG_TYPES (TREE_TYPE (built_in));

  for (size_t i = 0; i < operands.size (); ++i)
    {
      tree operand_type = TREE_VALUE (arg_type_chain);
      call_operands.push_back (convert (operand_type, operands[i]));
      operand_types.push_back (operand_type);
      arg_type_chain = TREE_CHAIN (arg_type_chain);
    }

  if (needs_workitem_context_data (brig_opcode))
    {
      call_operands.push_back (m_parent.m_cf->m_context_arg);
      operand_types.push_back (ptr_type_node);
      m_parent.m_cf->m_has_unexpanded_dp_builtins = true;
    }

  size_t operand_count = call_operands.size ();

  call_operands.resize (4, NULL_TREE);
  operand_types.resize (4, NULL_TREE);
  for (size_t i = 0; i < operand_count; ++i)
    call_operands.at (i) = build_reinterpret_cast (operand_types.at (i),
						   call_operands.at (i));

  tree fnptr = build_fold_addr_expr (built_in);
  return build_call_array (TREE_TYPE (TREE_TYPE (built_in)), fnptr,
			   operand_count, &call_operands[0]);
}

/* Instead of calling a built-in, reuse a previously returned value known to
   be still valid.  This is beneficial especially for the work-item
   identification related builtins as not having them as calls can lead to
   more easily vectorizable parallel loops for multi work-item work-groups.
   BRIG_OPCODE identifies the builtin and OPERANDS store the operands.  */

tree
brig_code_entry_handler::expand_builtin (BrigOpcode16_t brig_opcode,
					 tree_stl_vec &operands)
{
  tree_stl_vec uint32_0 = tree_stl_vec (1, build_int_cst (uint32_type_node, 0));

  tree_stl_vec uint32_1 = tree_stl_vec (1, build_int_cst (uint32_type_node, 1));

  tree_stl_vec uint32_2 = tree_stl_vec (1, build_int_cst (uint32_type_node, 2));

  if (brig_opcode == BRIG_OPCODE_WORKITEMFLATABSID)
    {
      tree id0 = expand_builtin (BRIG_OPCODE_WORKITEMABSID, uint32_0);
      id0 = convert (uint64_type_node, id0);

      tree id1 = expand_builtin (BRIG_OPCODE_WORKITEMABSID, uint32_1);
      id1 = convert (uint64_type_node, id1);

      tree id2 = expand_builtin (BRIG_OPCODE_WORKITEMABSID, uint32_2);
      id2 = convert (uint64_type_node, id2);

      tree max0 = convert (uint64_type_node,
			   m_parent.m_cf->m_grid_size_vars[0]);
      tree max1 = convert (uint64_type_node,
			   m_parent.m_cf->m_grid_size_vars[1]);

      tree id2_x_max0_x_max1 = build2 (MULT_EXPR, uint64_type_node, id2, max0);
      id2_x_max0_x_max1
	= build2 (MULT_EXPR, uint64_type_node, id2_x_max0_x_max1, max1);

      tree id1_x_max0 = build2 (MULT_EXPR, uint64_type_node, id1, max0);

      tree sum = build2 (PLUS_EXPR, uint64_type_node, id0, id1_x_max0);
      sum = build2 (PLUS_EXPR, uint64_type_node, sum, id2_x_max0_x_max1);

      return add_temp_var ("workitemflatabsid", sum);
    }
  else if (brig_opcode == BRIG_OPCODE_WORKITEMABSID)
    {
      HOST_WIDE_INT dim = int_constant_value (operands[0]);

      tree local_id_var = m_parent.m_cf->m_local_id_vars[dim];
      tree wg_id_var = m_parent.m_cf->m_wg_id_vars[dim];
      tree wg_size_var = m_parent.m_cf->m_wg_size_vars[dim];
      tree grid_size_var = m_parent.m_cf->m_grid_size_vars[dim];

      tree wg_id_x_wg_size = build2 (MULT_EXPR, uint32_type_node,
				     convert (uint32_type_node, wg_id_var),
				     convert (uint32_type_node, wg_size_var));
      tree sum
	= build2 (PLUS_EXPR, uint32_type_node, wg_id_x_wg_size, local_id_var);

      /* We need a modulo here because of work-groups which have dimensions
	 larger than the grid size :( TO CHECK: is this really allowed in the
	 specs?  */
      tree modulo
	= build2 (TRUNC_MOD_EXPR, uint32_type_node, sum, grid_size_var);

      return add_temp_var (std::string ("workitemabsid_")
			     + (char) ((int) 'x' + dim),
			   modulo);
    }
  else if (brig_opcode == BRIG_OPCODE_WORKITEMFLATID)
    {
      tree z_x_wgsx_wgsy
	= build2 (MULT_EXPR, uint32_type_node,
		  m_parent.m_cf->m_local_id_vars[2],
		  m_parent.m_cf->m_wg_size_vars[0]);
      z_x_wgsx_wgsy = build2 (MULT_EXPR, uint32_type_node, z_x_wgsx_wgsy,
			      m_parent.m_cf->m_wg_size_vars[1]);

      tree y_x_wgsx
	= build2 (MULT_EXPR, uint32_type_node,
		  m_parent.m_cf->m_local_id_vars[1],
		  m_parent.m_cf->m_wg_size_vars[0]);

      tree sum = build2 (PLUS_EXPR, uint32_type_node, y_x_wgsx, z_x_wgsx_wgsy);
      sum = build2 (PLUS_EXPR, uint32_type_node,
		    m_parent.m_cf->m_local_id_vars[0],
		    sum);
      return add_temp_var ("workitemflatid", sum);
    }
  else if (brig_opcode == BRIG_OPCODE_WORKGROUPSIZE)
    {
      HOST_WIDE_INT dim = int_constant_value (operands[0]);
      return m_parent.m_cf->m_wg_size_vars[dim];
    }
  else if (brig_opcode == BRIG_OPCODE_CURRENTWORKGROUPSIZE)
    {
      HOST_WIDE_INT dim = int_constant_value (operands[0]);
      return m_parent.m_cf->m_cur_wg_size_vars[dim];
    }
  else
    gcc_unreachable ();

  return NULL_TREE;
}

/* Appends and returns a new temp variable and an accompanying assignment
   statement that stores the value of the given EXPR and has the given NAME.  */

tree
brig_code_entry_handler::add_temp_var (std::string name, tree expr)
{
  tree temp_var = create_tmp_var (TREE_TYPE (expr), name.c_str ());
  tree assign = build2 (MODIFY_EXPR, TREE_TYPE (temp_var), temp_var, expr);
  m_parent.m_cf->append_statement (assign);
  return temp_var;
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
      else if (half_to_float)
	/* Treat the operands as the storage type at this point.  */
	operand_type = half_storage_type;

      tree operand = build_tree_operand (brig_inst, *operand_data, operand_type,
					 !is_output);

      gcc_assert (operand);

      /* Cast/convert the inputs to correct types as expected by the GENERIC
	 opcode instruction.  */
      if (!is_output)
	{
	  if (half_to_float)
	    operand = build_h2f_conversion
	      (build_reinterpret_cast (half_storage_type, operand));
	  else if (TREE_CODE (operand) != LABEL_DECL
		   && TREE_CODE (operand) != TREE_VEC
		   && operand_data->kind != BRIG_KIND_OPERAND_ADDRESS
		   && !VECTOR_TYPE_P (TREE_TYPE (operand)))
	    {
	      size_t reg_width = int_size_in_bytes (TREE_TYPE (operand));
	      size_t instr_width = int_size_in_bytes (operand_type);
	      if (reg_width == instr_width)
		operand = build_reinterpret_cast (operand_type, operand);
	      else if (reg_width > instr_width)
		{
		  /* Clip the operand because the instruction's bitwidth
		     is smaller than the HSAIL reg width.  */
		  if (INTEGRAL_TYPE_P (operand_type))
		    operand
		      = convert_to_integer (signed_or_unsigned_type_for
					    (TYPE_UNSIGNED (operand_type),
					     operand_type), operand);
		  else
		    operand = build_reinterpret_cast (operand_type, operand);
		}
	      else if (reg_width < instr_width)
		/* At least shift amount operands can be read from smaller
		   registers than the data operands.  */
		operand = convert (operand_type, operand);
	    }
	  else if (brig_inst.opcode == BRIG_OPCODE_SHUFFLE)
	    /* Force the operand type to be treated as the raw type.  */
	    operand = build_reinterpret_cast (operand_type, operand);

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
  /* The destination type might be different from the output register
     variable type (which is always an unsigned integer type).  */
  tree output_type = TREE_TYPE (output);
  tree input_type = TREE_TYPE (inst_expr);
  bool is_fp16 = (brig_inst.type & BRIG_TYPE_BASE_MASK) == BRIG_TYPE_F16
		 && brig_inst.base.kind != BRIG_KIND_INST_MEM
		 && !gccbrig_is_bit_operation (brig_inst.opcode);

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
      inst_expr = add_temp_var ("before_ftz", inst_expr);
      inst_expr = flush_to_zero (is_fp16) (*this, inst_expr);
    }

  if (is_fp16)
    {
      inst_expr = add_temp_var ("before_f2h", inst_expr);
      tree f2h_output = build_f2h_conversion (inst_expr);
      tree conv_int = convert_to_integer (output_type, f2h_output);
      tree assign = build2 (MODIFY_EXPR, output_type, output, conv_int);
      m_parent.m_cf->append_statement (assign);
      return assign;
    }
  else if (VECTOR_TYPE_P (TREE_TYPE (output)))
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
		      build_int_cst (uint32_type_node,
				     i * int_size_in_bytes (element_type)
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

      if (src_width == dst_width)
	{
	  /* A simple bitcast should do.  */
	  tree bitcast = build_reinterpret_cast (output_type, inst_expr);
	  tree assign = build2 (MODIFY_EXPR, output_type, output, bitcast);
	  m_parent.m_cf->append_statement (assign);
	  return assign;
	}
      else
	{
	  tree conv_int = convert_to_integer (output_type, inst_expr);
	  tree assign = build2 (MODIFY_EXPR, output_type, output, conv_int);
	  m_parent.m_cf->append_statement (assign);
	  return assign;
	}
    }
  return NULL_TREE;
}

/* Appends a GENERIC statement (STMT) to the currently constructed function.  */

void
brig_code_entry_handler::append_statement (tree stmt)
{
  m_parent.m_cf->append_statement (stmt);
}

/* Unpacks the elements of the vector in VALUE to scalars (bit field
   references) in ELEMENTS.  */

void
brig_code_entry_handler::unpack (tree value, tree_stl_vec &elements)
{
  size_t vec_size = int_size_in_bytes (TREE_TYPE (value));
  size_t element_size
    = int_size_in_bytes (TREE_TYPE (TREE_TYPE (value))) * BITS_PER_UNIT;
  size_t element_count
    = vec_size * BITS_PER_UNIT / element_size;

  tree input_element_type = TREE_TYPE (TREE_TYPE (value));

  value = add_temp_var ("unpack_input", value);

  for (size_t i = 0; i < element_count; ++i)
    {
      tree element
	= build3 (BIT_FIELD_REF, input_element_type, value,
		  TYPE_SIZE (input_element_type),
		  build_int_cst (unsigned_char_type_node, i * element_size));

      element = add_temp_var ("scalar", element);
      elements.push_back (element);
    }
}

/* Pack the elements of the scalars in ELEMENTS to the returned vector.  */

tree
brig_code_entry_handler::pack (tree_stl_vec &elements)
{
  size_t element_count = elements.size ();

  gcc_assert (element_count > 1);

  tree output_element_type = TREE_TYPE (elements.at (0));

  vec<constructor_elt, va_gc> *constructor_vals = NULL;
  for (size_t i = 0; i < element_count; ++i)
    CONSTRUCTOR_APPEND_ELT (constructor_vals, NULL_TREE, elements.at (i));

  tree vec_type = build_vector_type (output_element_type, element_count);

  /* build_constructor creates a vector type which is not a vector_cst
     that requires compile time constant elements.  */
  tree vec = build_constructor (vec_type, constructor_vals);

  /* Add a temp variable for readability.  */
  tree tmp_var = create_tmp_var (vec_type, "vec_out");
  tree vec_tmp_assign = build2 (MODIFY_EXPR, TREE_TYPE (tmp_var), tmp_var, vec);
  m_parent.m_cf->append_statement (vec_tmp_assign);
  return tmp_var;
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
				 build_int_cst (unsigned_char_type_node,
						i * element_size
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
				  build_int_cst (unsigned_char_type_node,
						 i * element_size
						 * BITS_PER_UNIT));

	  tree element1 = build3 (BIT_FIELD_REF, input_element_type, operand1,
				  TYPE_SIZE (input_element_type),
				  build_int_cst (unsigned_char_type_node,
						 i * element_size
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

  tree casted_operand = build_reinterpret_cast (uint32_type_node, operand);

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
    = build_reinterpret_cast (brig_to_generic::s_fp32_type, call);

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

