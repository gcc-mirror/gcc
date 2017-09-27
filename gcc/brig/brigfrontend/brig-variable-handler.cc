/* brig-variable-handler.cc -- brig variable directive handling
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
#include "errors.h"
#include "brig-machine.h"
#include "brig-util.h"
#include "print-tree.h"
#include "diagnostic-core.h"

tree
brig_directive_variable_handler::build_variable
  (const BrigDirectiveVariable *brigVar, tree_code var_decltype)
{
  std::string var_name = m_parent.get_mangled_name (brigVar);

  bool is_definition = brigVar->modifier & BRIG_VARIABLE_DEFINITION;

  tree name_identifier = get_identifier (var_name.c_str ());

  tree var_decl;
  tree t;
  if (brigVar->type & BRIG_TYPE_ARRAY)
    {
      tree element_type
	= gccbrig_tree_type_for_hsa_type (brigVar->type & ~BRIG_TYPE_ARRAY);
      uint64_t element_count = gccbrig_to_uint64_t (brigVar->dim);
      if (is_definition && element_count == 0)
	fatal_error (UNKNOWN_LOCATION, "Array definition with zero elements.");
      if (var_decltype == PARM_DECL)
	t = build_pointer_type (element_type);
      else
	t = build_array_type_nelts (element_type, element_count);
    }
  else
    {
      t = gccbrig_tree_type_for_hsa_type (brigVar->type);
    }

  size_t alignment = get_brig_var_alignment (brigVar);

  if (brigVar->segment == BRIG_SEGMENT_READONLY
      || brigVar->segment == BRIG_SEGMENT_KERNARG
      || (brigVar->modifier & BRIG_VARIABLE_CONST))
    TYPE_READONLY (t) = 1;

  TYPE_ADDR_SPACE (t) = gccbrig_get_target_addr_space_id (brigVar->segment);

  var_decl = build_decl (UNKNOWN_LOCATION, var_decltype, name_identifier, t);

  SET_DECL_ALIGN (var_decl, alignment * BITS_PER_UNIT);

  /* Force the HSA alignments.  */
  DECL_USER_ALIGN (var_decl) = 1;

  TREE_USED (var_decl) = 1;

  TREE_PUBLIC (var_decl) = 1;
  if (is_definition)
    DECL_EXTERNAL (var_decl) = 0;
  else
    DECL_EXTERNAL (var_decl) = 1; /* The definition is elsewhere.  */

  if (brigVar->init != 0)
    {
      gcc_assert (brigVar->segment == BRIG_SEGMENT_READONLY
		  || brigVar->segment == BRIG_SEGMENT_GLOBAL);

      const BrigBase *cst_operand_data
	= m_parent.get_brig_operand_entry (brigVar->init);

      tree initializer = NULL_TREE;
      if (cst_operand_data->kind == BRIG_KIND_OPERAND_CONSTANT_BYTES)
	initializer = get_tree_cst_for_hsa_operand
	  ((const BrigOperandConstantBytes *) cst_operand_data, t);
      else
	error ("variable initializers of type %x not implemented",
	       cst_operand_data->kind);
      gcc_assert (initializer != NULL_TREE);
      DECL_INITIAL (var_decl) = initializer;
    }

  if (var_decltype == PARM_DECL)
    {
      DECL_ARG_TYPE (var_decl) = TREE_TYPE (var_decl);
      DECL_EXTERNAL (var_decl) = 0;
      TREE_PUBLIC (var_decl) = 0;
    }

  TREE_ADDRESSABLE (var_decl) = 1;

  TREE_USED (var_decl) = 1;
  DECL_NONLOCAL (var_decl) = 1;
  DECL_ARTIFICIAL (var_decl) = 0;

  return var_decl;
}

size_t
brig_directive_variable_handler::operator () (const BrigBase *base)
{
  const BrigDirectiveVariable *brigVar = (const BrigDirectiveVariable *) base;

  bool is_definition = brigVar->modifier & BRIG_VARIABLE_DEFINITION;

  size_t var_size;
  tree var_type;
  if (brigVar->type & BRIG_TYPE_ARRAY)
    {
      tree element_type
	= gccbrig_tree_type_for_hsa_type (brigVar->type & ~BRIG_TYPE_ARRAY);
      uint64_t element_count = gccbrig_to_uint64_t (brigVar->dim);
      if (is_definition && element_count == 0)
	fatal_error (UNKNOWN_LOCATION, "Array definition with zero elements.");
      var_type = build_array_type_nelts (element_type, element_count);
      size_t element_size = tree_to_uhwi (TYPE_SIZE (element_type));
      var_size = element_size * element_count / 8;
    }
  else
    {
      var_type = gccbrig_tree_type_for_hsa_type (brigVar->type);
      var_size = tree_to_uhwi (TYPE_SIZE (var_type)) / 8;
    }

  size_t alignment = get_brig_var_alignment (brigVar);

  bool function_scope = m_parent.m_cf != NULL;

  if (function_scope)
    m_parent.m_cf->m_function_scope_vars.insert (base);

  std::string var_name = m_parent.get_mangled_name (brigVar);
  if (brigVar->segment == BRIG_SEGMENT_GROUP)
    {
      /* Non-kernel scope group variables have been added at the
	 'analyze' stage.  */
      m_parent.add_group_variable (var_name, var_size, alignment,
				   function_scope);
      return base->byteCount;
    }

  /* During analyze, handle only (module scope) group variables.  */
  if (m_parent.m_analyzing)
    return base->byteCount;

  if (brigVar->segment == BRIG_SEGMENT_KERNARG)
    {
      /* Do not create a real variable, but only a table of
	 offsets to the kernarg segment buffer passed as the
	 single argument by the kernel launcher for later
	 reference.  Ignore kernel declarations.  */
      if (m_parent.m_cf != NULL && m_parent.m_cf->m_func_decl != NULL_TREE)
	m_parent.m_cf->append_kernel_arg (brigVar, var_size, alignment);
      return base->byteCount;
    }
  else if (brigVar->segment == BRIG_SEGMENT_PRIVATE
	   || brigVar->segment == BRIG_SEGMENT_SPILL)
    {
      /* Private variables are handled like group variables,
	 except that their offsets are multiplied by the work-item
	 flat id, when accessed.  */
      if (!m_parent.has_private_variable (var_name))
	m_parent.append_private_variable (var_name, var_size, alignment);
      return base->byteCount;
    }
  else if (brigVar->segment == BRIG_SEGMENT_GLOBAL
	   || brigVar->segment == BRIG_SEGMENT_READONLY)
    {
      tree def = is_definition ? NULL_TREE :
	m_parent.global_variable (var_name);

      if (!is_definition && def != NULL_TREE)
	{
	  /* We have a definition already for this declaration.
	     Use the definition instead of the declaration.  */
	}
      else if (gccbrig_might_be_host_defined_var_p (brigVar))
	{
	  tree var_decl = build_variable (brigVar);
	  m_parent.add_host_def_var_ptr (var_name, var_decl);
	}
      else
	{
	  tree var_decl = build_variable (brigVar);
	  /* Make all global variables program scope for now
	     so we can get their address from the Runtime API.  */
	  DECL_CONTEXT (var_decl) = NULL_TREE;
	  TREE_STATIC (var_decl) = 1;
	  m_parent.add_global_variable (var_name, var_decl);
	}
    }
  else if (brigVar->segment == BRIG_SEGMENT_ARG)
    {

      if (m_parent.m_cf->m_generating_arg_block)
	{
	  tree var_decl = build_variable (brigVar);
	  tree bind_expr = m_parent.m_cf->m_current_bind_expr;

	  DECL_CONTEXT (var_decl) = m_parent.m_cf->m_func_decl;
	  DECL_CHAIN (var_decl) = BIND_EXPR_VARS (bind_expr);
	  BIND_EXPR_VARS (bind_expr) = var_decl;
	  TREE_PUBLIC (var_decl) = 0;

	  m_parent.m_cf->add_arg_variable (brigVar, var_decl);
	}
      else
	{
	  /* Must be an incoming function argument which has
	     been parsed in brig-function-handler.cc.  No
	     need to generate anything here.  */
	}
    }
  else
    gcc_unreachable ();

  return base->byteCount;
}

/* Returns the alignment for the given BRIG variable.  In case the variable
   explicitly defines alignment and its larger than the natural alignment,
   returns it instead of the natural one.  */

size_t
brig_directive_variable_handler::get_brig_var_alignment
(const BrigDirectiveVariable *brigVar)
{

  size_t defined_alignment
    = brigVar->align == BRIG_ALIGNMENT_NONE ? 0 : 1 << (brigVar->align - 1);
  size_t natural_alignment;
  if (brigVar->type & BRIG_TYPE_ARRAY)
    {
      tree element_type
	= gccbrig_tree_type_for_hsa_type (brigVar->type & ~BRIG_TYPE_ARRAY);
      size_t element_size = tree_to_uhwi (TYPE_SIZE (element_type));
      natural_alignment = element_size / BITS_PER_UNIT;
    }
  else
    {
      tree t = gccbrig_tree_type_for_hsa_type (brigVar->type);
      natural_alignment = tree_to_uhwi (TYPE_SIZE (t)) / BITS_PER_UNIT;
    }

  return natural_alignment > defined_alignment
    ? natural_alignment : defined_alignment;
}
