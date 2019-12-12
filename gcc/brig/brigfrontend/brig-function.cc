/* brig-function.cc -- declaration of brig_function class.
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

#include <sstream>
#include <iomanip>

#include "brig-function.h"
#include "stringpool.h"
#include "tree-iterator.h"
#include "toplev.h"
#include "gimplify.h"
#include "gimple-expr.h"
#include "print-tree.h"
#include "hsa-brig-format.h"
#include "stor-layout.h"
#include "diagnostic-core.h"
#include "brig-code-entry-handler.h"
#include "brig-machine.h"
#include "brig-util.h"
#include "phsa.h"
#include "tree-pretty-print.h"
#include "dumpfile.h"
#include "profile-count.h"
#include "tree-cfg.h"
#include "errors.h"
#include "function.h"
#include "brig-to-generic.h"
#include "brig-builtins.h"
#include "options.h"
#include "fold-const.h"
#include "target.h"
#include "builtins.h"

brig_function::builtin_map brig_function::s_custom_builtins;

brig_function::brig_function (const BrigDirectiveExecutable *exec,
			      brig_to_generic *parent)
  : m_brig_def (exec), m_is_kernel (false), m_is_finished (false), m_name (""),
    m_current_bind_expr (NULL_TREE), m_func_decl (NULL_TREE),
    m_context_arg (NULL_TREE), m_group_base_arg (NULL_TREE),
    m_private_base_arg (NULL_TREE), m_ret_value (NULL_TREE),
    m_next_kernarg_offset (0), m_kernarg_max_align (0),
    m_ret_value_brig_var (NULL), m_has_barriers (false), m_has_allocas (false),
    m_has_function_calls_with_barriers (false), m_calls_analyzed (false),
    m_is_wg_function (false), m_has_unexpanded_dp_builtins (false),
    m_generating_arg_block (false), m_parent (parent)
{
  memset (m_regs, 0,
	  BRIG_2_TREE_HSAIL_TOTAL_REG_COUNT * sizeof (BrigOperandRegister *));
  memset (&m_descriptor, 0, sizeof (phsa_descriptor));

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

brig_function::~brig_function ()
{
  for (size_t i = 0; i < BRIG_2_TREE_HSAIL_TOTAL_REG_COUNT; ++i)
    {
      if (m_regs[i] != NULL)
	{
	  delete m_regs[i];
	  m_regs[i] = NULL;
	}
    }
}

/* Returns a GENERIC label with the given name in the given function.
   Creates it, if not yet found.  */

tree
brig_function::label (const std::string &name)
{
  label_index::const_iterator i = m_label_index.find (name);
  if (i == m_label_index.end ())
    {
      tree name_identifier
	= get_identifier_with_length (name.c_str (), name.size ());

      tree label_decl = build_decl (UNKNOWN_LOCATION, LABEL_DECL,
				    name_identifier, void_type_node);

      DECL_CONTEXT (label_decl) = m_func_decl;
      DECL_ARTIFICIAL (label_decl) = 0;

      m_label_index[name] = label_decl;
      return label_decl;
    }
  else
    return (*i).second;
}

/* Record an argument variable for later use.  This includes both local
   variables inside arg blocks and incoming function arguments.  */

void
brig_function::add_arg_variable (const BrigDirectiveVariable *brigVar,
				 tree treeDecl)
{
  m_arg_variables[brigVar] = treeDecl;
}

tree
brig_function::arg_variable (const BrigDirectiveVariable *var) const
{
  variable_index::const_iterator i = m_arg_variables.find (var);
  if (i == m_arg_variables.end ())
    return NULL_TREE;
  else
    return (*i).second;
}

/* Appends a new kernel argument descriptor for the current kernel's
   arg space.  */

void
brig_function::append_kernel_arg (const BrigDirectiveVariable *var, size_t size,
				  size_t alignment)
{
  gcc_assert (m_func_decl != NULL_TREE);
  gcc_assert (m_is_kernel);

  size_t align_padding = m_next_kernarg_offset % alignment == 0 ?
    0 : (alignment - m_next_kernarg_offset % alignment);
  m_next_kernarg_offset += align_padding;
  m_kernarg_offsets[var] = m_next_kernarg_offset;
  m_next_kernarg_offset += size;

  m_kernarg_max_align
    = m_kernarg_max_align < alignment ? alignment : m_kernarg_max_align;
}

size_t
brig_function::kernel_arg_offset (const BrigDirectiveVariable *var) const
{
  var_offset_table::const_iterator i = m_kernarg_offsets.find (var);
  gcc_assert (i != m_kernarg_offsets.end ());
  return (*i).second;
}

/* Add work-item ID variables to the beginning of the kernel function
   which can be used for address computation as kernel dispatch packet
   instructions can be expanded to GENERIC nodes referring to them.  */

void
brig_function::add_id_variables ()
{
  tree bind_expr = m_current_bind_expr;
  tree stmts = BIND_EXPR_BODY (bind_expr);

  /* Initialize the WG limits and local ids.  */
  m_kernel_entry = tsi_start (stmts);

  for (int i = 0; i < 3; ++i)
    {
      char dim_char = (char) ((int) 'x' + i);

      /* The local sizes are limited to 16b values, but let's still use 32b
	 to avoid unnecessary casts (the ID functions are 32b).  */
      m_local_id_vars[i]
	= add_local_variable (std::string ("__local_") + dim_char,
			      long_long_integer_type_node);

      tree workitemid_call
	= call_builtin (builtin_decl_explicit (BUILT_IN_HSAIL_WORKITEMID), 2,
			uint32_type_node, uint32_type_node,
			build_int_cst (uint32_type_node, i), ptr_type_node,
			m_context_arg);

      tree id_init = build2 (MODIFY_EXPR, TREE_TYPE (m_local_id_vars[i]),
			     m_local_id_vars[i],
			     convert (TREE_TYPE (m_local_id_vars[i]),
				      workitemid_call));

      append_statement (id_init);

      m_cur_wg_size_vars[i]
	= add_local_variable (std::string ("__cur_wg_size_") + dim_char,
			      long_long_integer_type_node);

      tree cwgz_call;
      if (flag_assume_phsa)
	{
	  tree_stl_vec operands
	    = tree_stl_vec (1, build_int_cst (uint32_type_node, i));
	  cwgz_call
	    = expand_or_call_builtin (BRIG_OPCODE_CURRENTWORKGROUPSIZE,
				      BRIG_TYPE_U32, uint32_type_node,
				      operands);
	}
      else
	cwgz_call = call_builtin
	  (builtin_decl_explicit (BUILT_IN_HSAIL_CURRENTWORKGROUPSIZE),
	   2, uint32_type_node, uint32_type_node,
	   build_int_cst (uint32_type_node, i), ptr_type_node, m_context_arg);

      tree limit_init = build2 (MODIFY_EXPR, TREE_TYPE (m_cur_wg_size_vars[i]),
				m_cur_wg_size_vars[i],
				convert (TREE_TYPE (m_cur_wg_size_vars[i]),
					 cwgz_call));

      append_statement (limit_init);

      m_wg_id_vars[i]
	= add_local_variable (std::string ("__workgroupid_") + dim_char,
			      uint32_type_node);

      tree wgid_call;
      if (flag_assume_phsa)
	{
	  tree_stl_vec operands
	    = tree_stl_vec (1, build_int_cst (uint32_type_node, i));
	  wgid_call
	    = expand_or_call_builtin (BRIG_OPCODE_WORKGROUPID, BRIG_TYPE_U32,
				      uint32_type_node, operands);
	}
      else
	wgid_call
	  = call_builtin (builtin_decl_explicit (BUILT_IN_HSAIL_WORKGROUPID),
			  2, uint32_type_node, uint32_type_node,
			  build_int_cst (uint32_type_node, i), ptr_type_node,
			  m_context_arg);

      tree wgid_init = build2 (MODIFY_EXPR, TREE_TYPE (m_wg_id_vars[i]),
			       m_wg_id_vars[i], wgid_call);

      append_statement (wgid_init);

      m_wg_size_vars[i]
	= add_local_variable (std::string ("__workgroupsize_") + dim_char,
			      uint32_type_node);

      tree wgsize_call;
      if (flag_assume_phsa)
	{
	  tree_stl_vec operands
	    = tree_stl_vec (1, build_int_cst (uint32_type_node, i));
	  wgsize_call
	    = expand_or_call_builtin (BRIG_OPCODE_WORKGROUPSIZE, BRIG_TYPE_U32,
				      uint32_type_node, operands);
	}
      else
	wgsize_call
	  = call_builtin (builtin_decl_explicit (BUILT_IN_HSAIL_WORKGROUPSIZE),
			  2, uint32_type_node, uint32_type_node,
			  build_int_cst (uint32_type_node, i), ptr_type_node,
			  m_context_arg);

      tree wgsize_init = build2 (MODIFY_EXPR, TREE_TYPE (m_wg_size_vars[i]),
				 m_wg_size_vars[i], wgsize_call);

      append_statement (wgsize_init);

      m_grid_size_vars[i]
	= add_local_variable (std::string ("__gridsize_") + dim_char,
			      uint32_type_node);

      tree gridsize_call
	= call_builtin (builtin_decl_explicit (BUILT_IN_HSAIL_GRIDSIZE), 2,
			uint32_type_node, uint32_type_node,
			build_int_cst (uint32_type_node, i), ptr_type_node,
			m_context_arg);

      tree gridsize_init = build2 (MODIFY_EXPR, TREE_TYPE (m_grid_size_vars[i]),
				   m_grid_size_vars[i], gridsize_call);

      append_statement (gridsize_init);

      m_abs_id_base_vars[i]
	= add_local_variable (std::string ("__abs_id_base_") + dim_char,
			      long_long_integer_type_node);

      m_abs_id_vars[i]
	= add_local_variable (std::string ("__abs_id_") + dim_char,
			      long_long_integer_type_node);

      tree abs_id_base
	= build2 (MULT_EXPR, long_long_integer_type_node,
		  convert (long_long_integer_type_node, m_wg_id_vars[i]),
		  convert (long_long_integer_type_node, m_wg_size_vars[i]));
      tree abs_id
	= build2 (PLUS_EXPR, long_long_integer_type_node, abs_id_base,
		  convert (long_long_integer_type_node, m_local_id_vars[i]));

      tree abs_id_base_init
	= build2 (MODIFY_EXPR, TREE_TYPE (m_abs_id_base_vars[i]),
		  m_abs_id_base_vars[i], abs_id_base);
      append_statement (abs_id_base_init);

      tree abs_id_init = build2 (MODIFY_EXPR,
				 TREE_TYPE (m_abs_id_vars[i]),
				 m_abs_id_vars[i], abs_id);
      append_statement (abs_id_init);
    }
}

/* Creates a new local variable with the given NAME and given GENERIC
   TYPE.  */

tree
brig_function::add_local_variable (std::string name, tree type)
{
  tree name_identifier
    = get_identifier_with_length (name.c_str (), name.size ());
  tree variable
    = build_decl (UNKNOWN_LOCATION, VAR_DECL, name_identifier, type);

  DECL_NONLOCAL (variable) = 0;
  TREE_ADDRESSABLE (variable) = 0;
  TREE_STATIC (variable) = 0;
  TREE_USED (variable) = 1;
  DECL_ARTIFICIAL (variable) = 0;

  tree bind_expr = DECL_SAVED_TREE (m_func_decl);

  DECL_CONTEXT (variable) = m_func_decl;

  DECL_CHAIN (variable) = BIND_EXPR_VARS (bind_expr);
  BIND_EXPR_VARS (bind_expr) = variable;
  return variable;
}

/* Return tree type for an HSA register.

   The tree type can be anything (scalar, vector, int, float, etc.)
   but its size is guaranteed to match the HSA register size.

   HSA registers are untyped but we select a type based on their use
   to reduce (sometimes unoptimizable) VIEW_CONVERT_EXPR nodes (seems
   to occur when use or def reaches over current BB).  */

tree
brig_function::get_tree_type_for_hsa_reg (const BrigOperandRegister *reg) const
{
  size_t reg_size = gccbrig_reg_size (reg);

  /* The default type.  */
  tree type = build_nonstandard_integer_type (reg_size, true);

  if (m_parent->m_fn_regs_use_index.count (m_name) == 0)
    return type;

  const regs_use_index &index = m_parent->m_fn_regs_use_index[m_name];
  size_t reg_id = gccbrig_hsa_reg_id (*reg);
  if (index.count (reg_id) == 0)
    return type;

  const reg_use_info &info = index.find (reg_id)->second;
  std::vector<std::pair<tree, size_t> >::const_iterator it
    = info.m_type_refs.begin ();
  std::vector<std::pair<tree, size_t> >::const_iterator it_end
    = info.m_type_refs.end ();
  size_t max_refs_as_type_count = 0;
  for (; it != it_end; it++)
    {
      size_t type_bit_size = int_size_in_bytes (it->first) * BITS_PER_UNIT;
      if (type_bit_size != reg_size) continue;
      if (it->second > max_refs_as_type_count)
	{
	  type = it->first;
	  max_refs_as_type_count = it->second;
	}
    }

  return type;
}

/* Returns a DECL_VAR for the given HSAIL operand register.
   If it has not been created yet for the function being generated,
   creates it as a type determined by analysis phase.  */

tree
brig_function::get_m_var_declfor_reg (const BrigOperandRegister *reg)
{
  size_t offset = gccbrig_hsa_reg_id (*reg);

  reg_decl_index_entry *regEntry = m_regs[offset];
  if (regEntry == NULL)
    {
      size_t reg_size = gccbrig_reg_size (reg);
      tree type;
      if (reg_size > 1)
	type = get_tree_type_for_hsa_reg (reg);
      else
	type = boolean_type_node;

      /* Drop the const qualifier so we do not end up with a read only
	 register variable which cannot be written to later.  */
      tree nonconst_type = build_type_variant (type, false, false);

      regEntry = new reg_decl_index_entry;

      regEntry->m_var_decl
	= add_local_variable (gccbrig_reg_name (reg), nonconst_type);
      m_regs[offset] = regEntry;
    }
  return regEntry->m_var_decl;
}

/* Builds a work-item do..while loop for a single DIM.  HEADER_ENTRY is
   a statement after which the iteration variables should be initialized and
   the loop body starts.  BRANCH_AFTER is the statement after which the loop
   predicate check and the back edge goto will be appended.  */

void
brig_function::add_wi_loop (int dim, tree_stmt_iterator *header_entry,
			    tree_stmt_iterator *branch_after)
{
  tree ivar = m_local_id_vars[dim];
  tree abs_id_base_var = m_abs_id_base_vars[dim];
  tree abs_id_var = m_abs_id_vars[dim];
  tree ivar_max = m_cur_wg_size_vars[dim];
  tree_stmt_iterator entry = *header_entry;

  /* TODO: this is not a parallel loop as we share the "register variables"
     across work-items.  Should create a copy of them per WI instance.  That
     is, declare temporaries for new definitions inside the loop body, not at
     function scope.  */

  tree ivar_init = build2 (MODIFY_EXPR, TREE_TYPE (ivar), ivar,
			   build_zero_cst (TREE_TYPE (ivar)));
  tsi_link_after (&entry, ivar_init, TSI_NEW_STMT);

  tree abs_id_var_init = build2 (MODIFY_EXPR, TREE_TYPE (abs_id_var),
				 abs_id_var,
				 convert (TREE_TYPE (abs_id_var),
					  abs_id_base_var));
  tsi_link_after (&entry, abs_id_var_init, TSI_NEW_STMT);

  tree loop_body_label
    = label (std::string ("__wi_loop_") + (char) ((int) 'x' + dim));
  tree loop_body_label_stmt = build_stmt (LABEL_EXPR, loop_body_label);

  tsi_link_after (&entry, loop_body_label_stmt, TSI_NEW_STMT);

  if (m_has_unexpanded_dp_builtins)
    {
      if (!flag_assume_phsa)
	{
	  tree id_set_builtin
	    = builtin_decl_explicit (BUILT_IN_HSAIL_SETWORKITEMID);
	  /* Set the local ID to the current wi-loop iteration variable value
	     to ensure the builtins see the correct values.  */
	  tree id_set_call
	    = call_builtin (id_set_builtin, 3,
			    void_type_node, uint32_type_node,
			    build_int_cst (uint32_type_node, dim),
			    uint32_type_node, convert (uint32_type_node, ivar),
			    ptr_type_node, m_context_arg);
	  tsi_link_after (&entry, id_set_call, TSI_NEW_STMT);
	}
      else
	{
	  tree ptr_type = build_pointer_type (uint32_type_node);
	  tree ctx = build2 (MEM_REF, uint32_type_node, m_context_arg,
			     build_int_cst (ptr_type, dim * 4));
	  tree assign = build2 (MODIFY_EXPR, uint32_type_node, ctx,
				convert (uint32_type_node, ivar));

	  tsi_link_after (&entry, assign, TSI_NEW_STMT);
	}
    }

  /* Increment the WI iteration variable.  */
  tree incr = build2 (PREINCREMENT_EXPR, TREE_TYPE (ivar), ivar,
		      build_one_cst (TREE_TYPE (ivar)));

  tsi_link_after (branch_after, incr, TSI_NEW_STMT);

  /* ...and the abs id variable.  */
  tree abs_id_incr = build2 (PREINCREMENT_EXPR, TREE_TYPE (abs_id_var),
			     abs_id_var,
			     build_one_cst (TREE_TYPE (abs_id_var)));

  tsi_link_after (branch_after, abs_id_incr, TSI_NEW_STMT);

  /* Append the predicate check with the back edge goto.  */
  tree condition = build2 (LT_EXPR, TREE_TYPE (ivar), ivar, ivar_max);
  tree target_goto = build1 (GOTO_EXPR, void_type_node, loop_body_label);
  tree if_stmt
    = build3 (COND_EXPR, void_type_node, condition, target_goto, NULL_TREE);
  tsi_link_after (branch_after, if_stmt, TSI_NEW_STMT);
}

/* Recursively analyzes the function and its callees for barrier usage.  */

void
brig_function::analyze_calls ()
{
  if (m_calls_analyzed)
    return;

  /* Set this early to not get stuck in case of recursive call graphs.
     This is safe because if the function calls itself, either the function
     has barrier calls which implies a call to a function with barrier calls,
     or it doesn't in which case the result depends on the later called
     functions.  */
  m_calls_analyzed = true;

  for (size_t i = 0; i < m_called_functions.size (); ++i)
    {
      tree f = m_called_functions[i];
      brig_function *called_f = m_parent->get_finished_function (f);
      if (called_f == NULL)
	{
	  /* Unfinished function (only declaration within the set of BRIGs)
	     found.  Cannot finish the CG analysis.  Have to assume it does have
	     a barrier for safety.  */
	  m_has_function_calls_with_barriers = true;
	  m_has_unexpanded_dp_builtins = true;
	  break;
	}
      called_f->analyze_calls ();
      /* We can assume m_has_barriers has been correctly set during the
	 construction of the function decl.  No need to reanalyze it.  */
      m_has_function_calls_with_barriers |= called_f->m_has_barriers;

      /* If the function or any of its called functions has dispatch
	 packet builtin calls that require the local id, we need to
	 set the local id to the context in the work item loop before
	 the functions are called.  If we analyze the opposite, these
	 function calls can be omitted.  */
      m_has_unexpanded_dp_builtins |= called_f->m_has_unexpanded_dp_builtins;
    }
}

/* Tries to convert the current kernel to a work-group function that executes
   all work-items using loops.  Returns true in case the conversion was
   successful.  */

bool
brig_function::convert_to_wg_function ()
{
  if (!m_calls_analyzed)
    analyze_calls ();

  if (m_has_barriers || m_has_function_calls_with_barriers)
    return false;

  /* The most trivial case: No barriers at all in the kernel.
     We can create one big work-item loop around the whole kernel.  */
  tree bind_expr = m_current_bind_expr;
  tree stmts = BIND_EXPR_BODY (bind_expr);

  for (int i = 0; i < 3; ++i)
    {
      /* The previous loop has added a new label to the end of the function,
	 the next level loop should wrap around it also.  */
      tree_stmt_iterator function_exit = tsi_last (stmts);
      add_wi_loop (i, &m_kernel_entry, &function_exit);
    }

  m_is_wg_function = true;
  return false;
}

/* Emits a kernel description to a special ELF section so it can be
   utilized by an HSA runtime implementation.  The assembly block
   must be emitted to a statement list of an function, which is given
   as an argument.  Returns the assembly block used to emit the section. */

tree
brig_function::emit_metadata (tree stmt_list)
{
  /* Emit an ELF section via an assembly directive that generates a special
     ELF section for each kernel that contains raw bytes of a descriptor
     object.  This is pretty disgusting, but life is never perfect ;)  */

  /* Use the original kernel name without the '_' prefix in the section name.  */
  std::string kern_name = m_is_kernel ? m_name.substr (1) : m_name;

  std::ostringstream strstr;
  strstr << std::endl
	 << ".pushsection " << PHSA_DESC_SECTION_PREFIX << kern_name
	 << std::endl
	 << "\t.p2align 1, 1, 1" << std::endl
	 << "\t.byte ";

  for (size_t i = 0; i < sizeof (phsa_descriptor); ++i)
    {
      strstr << "0x" << std::setw (2) << std::setfill ('0') << std::hex
	     << (unsigned) *((unsigned char *) &m_descriptor + i);
      if (i + 1 < sizeof (phsa_descriptor))
	strstr << ", ";
    }

  strstr << std::endl << ".popsection" << std::endl << std::endl;

  tree metadata_asm
    = build_stmt (ASM_EXPR,
		  build_string (strstr.str ().size (), strstr.str ().c_str ()),
		  NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE);

  append_to_statement_list_force (metadata_asm, &stmt_list);
  return metadata_asm;
}

/* Emits the kernel launcher function.  Also emits the metadata section
   creation statements in it.

   The launcher function calls the device-side runtime
   that runs the kernel for all work-items.  In C:

   void KernelName (void* context, void* group_base_addr)
   {
     __hsail_launch_kernel (_KernelName, context, group_base_addr);
   }

   or, in case of a successful conversion to a work-group function:

   void KernelName (void* context, void* group_base_addr)
   {
     __hsail_launch_wg_function (_KernelName, context, group_base_addr);
   }

   The user/host sees this function as the kernel to call from the
   outside.  The actual kernel generated from HSAIL was named _KernelName.
*/

tree
brig_function::emit_launcher_and_metadata ()
{
  /* The original kernel name without the '_' prefix.  */
  std::string kern_name = m_name.substr (1);

  tree name_identifier
    = get_identifier_with_length (kern_name.c_str (), kern_name.size ());

  tree restrict_void_ptr
    = build_qualified_type (build_pointer_type (void_type_node),
			    TYPE_QUAL_RESTRICT);
  tree restrict_char_ptr
    = build_qualified_type (build_pointer_type (char_type_node),
			    TYPE_QUAL_RESTRICT);
  tree launcher
    = build_decl (UNKNOWN_LOCATION, FUNCTION_DECL, name_identifier,
		  build_function_type_list (void_type_node, restrict_void_ptr,
					    restrict_char_ptr, NULL_TREE));

  TREE_USED (launcher) = 1;
  DECL_ARTIFICIAL (launcher) = 1;

  tree context_arg = build_decl (UNKNOWN_LOCATION, PARM_DECL,
				 get_identifier ("__context"),
				 restrict_void_ptr);

  DECL_ARGUMENTS (launcher) = context_arg;
  DECL_ARG_TYPE (context_arg) = restrict_void_ptr;
  DECL_CONTEXT (context_arg) = launcher;
  TREE_USED (context_arg) = 1;
  DECL_ARTIFICIAL (context_arg) = 1;

  tree group_base_addr_arg
    = build_decl (UNKNOWN_LOCATION, PARM_DECL,
		  get_identifier ("__group_base_addr"), restrict_char_ptr);

  chainon (DECL_ARGUMENTS (launcher), group_base_addr_arg);
  DECL_ARG_TYPE (group_base_addr_arg) = restrict_char_ptr;
  DECL_CONTEXT (group_base_addr_arg) = launcher;
  TREE_USED (group_base_addr_arg) = 1;
  DECL_ARTIFICIAL (group_base_addr_arg) = 1;

  tree resdecl
    = build_decl (UNKNOWN_LOCATION, RESULT_DECL, NULL_TREE, void_type_node);

  DECL_RESULT (launcher) = resdecl;
  DECL_CONTEXT (resdecl) = launcher;

  DECL_INITIAL (launcher) = make_node (BLOCK);
  TREE_USED (DECL_INITIAL (launcher)) = 1;

  tree stmt_list = alloc_stmt_list ();

  tree bind_expr = build3 (BIND_EXPR, void_type_node, NULL, stmt_list, NULL);

  TREE_STATIC (launcher) = 1;
  TREE_PUBLIC (launcher) = 1;

  DECL_SAVED_TREE (launcher) = bind_expr;

  if (DECL_STRUCT_FUNCTION (launcher) == NULL)
    push_struct_function (launcher);
  else
    push_cfun (DECL_STRUCT_FUNCTION (launcher));

  tree kernel_func_ptr = build1 (ADDR_EXPR, ptr_type_node, m_func_decl);

  tree phsail_launch_kernel_call;

  /* Compute the local group segment frame start pointer.  */
  tree group_local_offset_temp
    = create_tmp_var (uint32_type_node, "group_local_offset");
  tree group_local_offset_arg
    = build2 (MODIFY_EXPR, uint32_type_node,
	      group_local_offset_temp,
	      build_int_cst (uint32_type_node,
			     m_parent->m_module_group_variables.size()));

  /* Emit a launcher depending whether we converted the kernel function to
     a work group function or not.  */
  if (m_is_wg_function)
    phsail_launch_kernel_call
      = call_builtin (builtin_decl_explicit (BUILT_IN_HSAIL_LAUNCH_WG_FUNC),
		      4, void_type_node,
		      ptr_type_node, kernel_func_ptr, restrict_void_ptr,
		      context_arg, restrict_char_ptr, group_base_addr_arg,
		      uint32_type_node, group_local_offset_arg);
  else
    phsail_launch_kernel_call
      = call_builtin (builtin_decl_explicit (BUILT_IN_HSAIL_LAUNCH_KERNEL),
		      4, void_type_node,
		      ptr_type_node, kernel_func_ptr, restrict_void_ptr,
		      context_arg, restrict_char_ptr, group_base_addr_arg,
		      uint32_type_node, group_local_offset_arg);

  append_to_statement_list_force (phsail_launch_kernel_call, &stmt_list);

  emit_metadata (stmt_list);

  set_externally_visible (launcher);

  return launcher;
}

tree
brig_function::append_statement (tree stmt)
{
  gcc_assert (m_func_decl != NULL);

  tree bind_expr = m_current_bind_expr;
  tree stmts = BIND_EXPR_BODY (bind_expr);

  append_to_statement_list_force (stmt, &stmts);
  return stmt;
}

/* Creates a new "alloca frame" for the current function by
   injecting an alloca frame push in the beginning of the function
   and an alloca frame pop before all function exit points.  */

void
brig_function::create_alloca_frame ()
{
  tree_stmt_iterator entry;

  /* Adds the alloca push only after the ids have been initialized
     in case of a kernel function.  */
  if (m_is_kernel)
    entry = m_kernel_entry;
  else
    {
      tree bind_expr = m_current_bind_expr;
      tree stmts = BIND_EXPR_BODY (bind_expr);
      entry = tsi_start (stmts);
    }

  tree push_frame_builtin = builtin_decl_explicit (BUILT_IN_HSAIL_PUSH_FRAME);
  tree push_frame_call
    = call_builtin (push_frame_builtin, 1, void_type_node, ptr_type_node,
		    m_context_arg);

  tsi_link_before (&entry, push_frame_call, TSI_NEW_STMT);

  tree pop_frame_builtin = builtin_decl_explicit (BUILT_IN_HSAIL_POP_FRAME);

  do
    {
      tree stmt = tsi_stmt (entry);
      if (TREE_CODE (stmt) == RETURN_EXPR)
	{
	  tree pop_frame_call
	    = call_builtin (pop_frame_builtin, 1, void_type_node,
			    ptr_type_node, m_context_arg);

	  tsi_link_before (&entry, pop_frame_call, TSI_SAME_STMT);
	}
      tsi_next (&entry);
    }
  while (!tsi_end_p (entry));
}

/* Finishes the currently built function.  After calling this, no new
   statements should be appeneded to the function.  */
void
brig_function::finish ()
{
  append_return_stmt ();

  /* Currently assume single alloca frame per WG.  */
  if (m_has_allocas)
    create_alloca_frame ();
}

void
brig_function::finish_kernel ()
{
  /* Kernel functions should have a single exit point.
     Let's create one.  The return instructions should have
     been converted to branches to this label.  */
  append_statement (build_stmt (LABEL_EXPR, m_exit_label));
  /* Attempt to convert the kernel to a work-group function that
     executes all work-items of the WG using a loop.  */
  convert_to_wg_function ();

  append_return_stmt ();

  /* Currently assume single alloca frame per WG.  */
  if (m_has_allocas)
    create_alloca_frame ();
}

void
brig_function::append_return_stmt ()
{
  gcc_assert (m_current_bind_expr != NULL_TREE);
  tree stmts = BIND_EXPR_BODY (m_current_bind_expr);

  if (STATEMENT_LIST_TAIL (stmts) == NULL)
    return; /* Empty function.  */

  tree last_stmt = tsi_stmt (tsi_last (stmts));

  if (TREE_CODE (last_stmt) == RETURN_EXPR)
    return;

  if (m_ret_value != NULL_TREE)
    {
      tree result_assign
	= build2 (MODIFY_EXPR, TREE_TYPE (m_ret_value), m_ret_value,
		  m_ret_temp);

      tree return_expr
	= build1 (RETURN_EXPR, TREE_TYPE (result_assign), result_assign);
      append_to_statement_list_force (return_expr, &stmts);
    }
  else
    {
      tree return_stmt = build_stmt (RETURN_EXPR, NULL);
      append_to_statement_list_force (return_stmt, &stmts);
    }
}

bool
brig_function::has_function_scope_var (const BrigBase* var) const
{
  return m_function_scope_vars.find (var) != m_function_scope_vars.end ();
}

size_t
brig_function::group_variable_segment_offset (const std::string &name) const
{
  if (m_local_group_variables.has_variable (name))
    return m_local_group_variables.segment_offset (name);

  gcc_assert (m_parent->m_module_group_variables.has_variable (name));
  return m_parent->m_module_group_variables.segment_offset (name);
}

/* Try to expand the given builtin call to reuse a previously generated
   variable, if possible.  If not, just call the given builtin.
   BRIG_OPCODE and BRIG_TYPE identify the builtin's BRIG opcode/type,
   ARITH_TYPE its GENERIC type, and OPERANDS contains the builtin's
   input operands.  */

tree
brig_function::expand_or_call_builtin (BrigOpcode16_t brig_opcode,
				       BrigType16_t brig_type,
				       tree arith_type,
				       tree_stl_vec &operands)
{
  if (needs_workitem_context_data (brig_opcode))
    m_has_unexpanded_dp_builtins = true;

  if (can_expand_builtin (brig_opcode))
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

      size_t element_count = gccbrig_type_vector_subparts (arith_type);
      for (size_t i = 0; i < element_count; ++i)
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
      call_operands.push_back (m_context_arg);
      operand_types.push_back (ptr_type_node);
    }

  size_t operand_count = call_operands.size ();

  call_operands.resize (4, NULL_TREE);
  operand_types.resize (4, NULL_TREE);
  for (size_t i = 0; i < operand_count; ++i)
    call_operands.at (i) = build_resize_convert_view (operand_types.at (i),
						      call_operands.at (i));

  tree fnptr = build_fold_addr_expr (built_in);
  return build_call_array (TREE_TYPE (TREE_TYPE (built_in)), fnptr,
			   operand_count, &call_operands[0]);
}

/* Instead of calling a built-in function, use a more efficient mechanism
   such as reuse a previously returned value known to be still valid, or
   access the work-item context struct directly.  This is beneficial especially
   for the work-item identification related builtins as not having them as
   unanalyzable black box calls can lead to more easily vectorizable parallel
   loops for multi work-item work-groups.  BRIG_OPCODE identifies the builtin
   and OPERANDS store the operands.  */

tree
brig_function::expand_builtin (BrigOpcode16_t brig_opcode,
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

      tree max0 = convert (uint64_type_node, m_grid_size_vars[0]);
      tree max1 = convert (uint64_type_node, m_grid_size_vars[1]);

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
      return m_abs_id_vars[dim];
    }
  else if (brig_opcode == BRIG_OPCODE_WORKITEMFLATID)
    {

      tree wg_size_x = expand_builtin (BRIG_OPCODE_WORKGROUPSIZE, uint32_0);
      tree wg_size_y = expand_builtin (BRIG_OPCODE_WORKGROUPSIZE, uint32_1);
      tree z_x_wgsx_wgsy
	= build2 (MULT_EXPR, uint32_type_node,
		  convert (uint32_type_node,
			   expand_builtin (BRIG_OPCODE_WORKITEMID, uint32_2)),
		  wg_size_x);
      z_x_wgsx_wgsy = build2 (MULT_EXPR, uint32_type_node, z_x_wgsx_wgsy,
			      wg_size_y);

      tree y_x_wgsx
	= build2 (MULT_EXPR, uint32_type_node,
		  convert (uint32_type_node,
			   expand_builtin (BRIG_OPCODE_WORKITEMID, uint32_1)),
		  wg_size_x);

      tree sum = build2 (PLUS_EXPR, uint32_type_node, y_x_wgsx, z_x_wgsx_wgsy);
      sum = build2 (PLUS_EXPR, uint32_type_node,
		    convert (uint32_type_node,
			     expand_builtin (BRIG_OPCODE_WORKITEMID, uint32_0)),
		    sum);
      return add_temp_var ("workitemflatid", sum);
    }
  else if (brig_opcode == BRIG_OPCODE_WORKGROUPSIZE)
    {
      HOST_WIDE_INT dim = int_constant_value (operands[0]);
      if (flag_assume_phsa)
	{
	  tree ptr_type = build_pointer_type (uint32_type_node);
	  tree ctx = build2 (MEM_REF, uint32_type_node, m_context_arg,
			     build_int_cst (ptr_type,
					    PHSA_CONTEXT_WG_SIZES
					    + dim * 4));
	  std::string name ("wgsize_x");
	  name [name.length() - 1] += dim;
	  return add_temp_var (name.c_str(), ctx);
	}
      else if (m_is_kernel)
	{
	  /* For kernels without phsa we generate certain temps before
	     the WI loop, which means we don't need to rely on LICM to get
	     them moved out.  */
	  return m_wg_size_vars[dim];
	}
      else
	gcc_unreachable ();
    }
  else if (brig_opcode == BRIG_OPCODE_WORKITEMID)
    {
      HOST_WIDE_INT dim = int_constant_value (operands[0]);
      if (m_is_kernel)
	{
	  return m_local_id_vars [dim];
	}
      else if (flag_assume_phsa)
	{
	  tree ptr_type = build_pointer_type (uint32_type_node);
	  tree ctx = build2 (MEM_REF, uint32_type_node, m_context_arg,
			     build_int_cst (ptr_type,
					    PHSA_CONTEXT_OFFS_WI_IDS
					    + dim * 4));
	  std::string name ("wiid_x");
	  name [name.length() - 1] += dim;
	  return add_temp_var (name.c_str(), ctx);
	}
      else
	gcc_unreachable ();
    }
  else if (brig_opcode == BRIG_OPCODE_WORKGROUPID)
    {
      HOST_WIDE_INT dim = int_constant_value (operands[0]);
      if (flag_assume_phsa)
	{
	  tree ptr_type = build_pointer_type (uint32_type_node);
	  tree ctx = build2 (MEM_REF, uint32_type_node, m_context_arg,
			     build_int_cst (ptr_type,
					    PHSA_CONTEXT_OFFS_WG_IDS
					    + dim * 4));
	  std::string name ("wgid_x");
	  name [name.length() - 1] += dim;
	  return add_temp_var (name.c_str(), ctx);
	} else if (m_is_kernel)
	return m_wg_id_vars [dim];
      else
	gcc_unreachable ();
    }
  else if (brig_opcode == BRIG_OPCODE_CURRENTWORKGROUPSIZE)
    {
      HOST_WIDE_INT dim = int_constant_value (operands[0]);
      if (flag_assume_phsa)
	{
	  tree ptr_type = build_pointer_type (uint32_type_node);
	  tree ctx = build2 (MEM_REF, uint32_type_node, m_context_arg,
			     build_int_cst (ptr_type,
					    PHSA_CONTEXT_CURRENT_WG_SIZES
					    + dim * 4));
	  std::string name ("curwgsize_x");
	  name [name.length() - 1] += dim;
	  return add_temp_var (name.c_str(), ctx);
	} else if (m_is_kernel)
	return m_cur_wg_size_vars[dim];
      else
	gcc_unreachable ();
    }
  else
    gcc_unreachable ();

  return NULL_TREE;
}

/* Returns true in case the given opcode that would normally be generated
   as a builtin call can be expanded to tree nodes.  */

bool
brig_function::can_expand_builtin (BrigOpcode16_t brig_opcode) const
{
  switch (brig_opcode)
    {
    case BRIG_OPCODE_CURRENTWORKGROUPSIZE:
    case BRIG_OPCODE_WORKITEMFLATID:
    case BRIG_OPCODE_WORKITEMID:
    case BRIG_OPCODE_WORKGROUPID:
    case BRIG_OPCODE_WORKGROUPSIZE:
      return m_is_kernel || flag_assume_phsa;
    case BRIG_OPCODE_WORKITEMFLATABSID:
    case BRIG_OPCODE_WORKITEMABSID:
      return m_is_kernel;
    default:
      return false;
    };
}

/* In case the HSA instruction must be implemented using a builtin,
   this function is called to get the correct builtin function.
   TYPE is the instruction tree type, BRIG_OPCODE the opcode of the
   brig instruction and BRIG_TYPE the brig instruction's type.  */

tree
brig_function::get_builtin_for_hsa_opcode
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
    case BRIG_OPCODE_FMA:
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

/* Unpacks the elements of the vector in VALUE to scalars (bit field
   references) in ELEMENTS.  */

void
brig_function::unpack (tree value, tree_stl_vec &elements)
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
		  bitsize_int(i * element_size));

      element = add_temp_var ("scalar", element);
      elements.push_back (element);
    }
}

/* Pack the elements of the scalars in ELEMENTS to the returned vector.  */

tree
brig_function::pack (tree_stl_vec &elements)
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
  append_statement (vec_tmp_assign);
  return tmp_var;
}

/* Returns true in case the given opcode needs to know about work-item context
   data.  In such case the context data is passed as a pointer to a work-item
   context object, as the last argument in the builtin call.  */

bool
brig_function::needs_workitem_context_data
(BrigOpcode16_t brig_opcode)
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

/* Appends and returns a new temp variable and an accompanying assignment
   statement that stores the value of the given EXPR and has the given NAME.  */

tree
brig_function::add_temp_var (std::string name, tree expr)
{
  tree temp_var = create_tmp_var (TREE_TYPE (expr), name.c_str ());
  tree assign = build2 (MODIFY_EXPR, TREE_TYPE (temp_var), temp_var, expr);
  append_statement (assign);
  return temp_var;
}

/* Returns the integer constant value of the given node.
   If it's a cast, looks into the source of the cast.  */

HOST_WIDE_INT
brig_function::int_constant_value (tree node)
{
  tree n = node;
  if (TREE_CODE (n) == VIEW_CONVERT_EXPR)
    n = TREE_OPERAND (n, 0);
  return int_cst_value (n);
}

/* Returns the tree code that should be used to implement the given
   HSA instruction opcode (BRIG_OPCODE) for the given type of instruction
   (BRIG_TYPE).  In case the opcode cannot be mapped to a TREE node directly,
   returns TREE_LIST (if it can be emulated with a simple chain of tree
   nodes) or CALL_EXPR if the opcode should be implemented using a builtin
   call.  */

tree_code
brig_function::get_tree_code_for_hsa_opcode
  (BrigOpcode16_t brig_opcode, BrigType16_t brig_type)
{
  BrigType16_t brig_inner_type = brig_type & BRIG_TYPE_BASE_MASK;
  switch (brig_opcode)
    {
    case BRIG_OPCODE_NOP:
      return NOP_EXPR;
    case BRIG_OPCODE_ADD:
      return PLUS_EXPR;
    case BRIG_OPCODE_CMOV:
      if (brig_inner_type == brig_type)
	return COND_EXPR;
      else
	return VEC_COND_EXPR;
    case BRIG_OPCODE_SUB:
      return MINUS_EXPR;
    case BRIG_OPCODE_MUL:
    case BRIG_OPCODE_MUL24:
      return MULT_EXPR;
    case BRIG_OPCODE_MULHI:
    case BRIG_OPCODE_MUL24HI:
      return MULT_HIGHPART_EXPR;
    case BRIG_OPCODE_DIV:
      if (gccbrig_is_float_type (brig_inner_type))
	return RDIV_EXPR;
      else
	return TRUNC_DIV_EXPR;
    case BRIG_OPCODE_NEG:
      return NEGATE_EXPR;
    case BRIG_OPCODE_MIN:
      if (gccbrig_is_float_type (brig_inner_type))
	return CALL_EXPR;
      else
	return MIN_EXPR;
    case BRIG_OPCODE_MAX:
      if (gccbrig_is_float_type (brig_inner_type))
	return CALL_EXPR;
      else
	return MAX_EXPR;
    case BRIG_OPCODE_ABS:
      return ABS_EXPR;
    case BRIG_OPCODE_SHL:
      return LSHIFT_EXPR;
    case BRIG_OPCODE_SHR:
      return RSHIFT_EXPR;
    case BRIG_OPCODE_OR:
      return BIT_IOR_EXPR;
    case BRIG_OPCODE_XOR:
      return BIT_XOR_EXPR;
    case BRIG_OPCODE_AND:
      return BIT_AND_EXPR;
    case BRIG_OPCODE_NOT:
      return BIT_NOT_EXPR;
    case BRIG_OPCODE_RET:
      return RETURN_EXPR;
    case BRIG_OPCODE_MOV:
    case BRIG_OPCODE_LDF:
      return MODIFY_EXPR;
    case BRIG_OPCODE_LD:
    case BRIG_OPCODE_ST:
      return MEM_REF;
    case BRIG_OPCODE_BR:
      return GOTO_EXPR;
    case BRIG_OPCODE_REM:
      if (brig_type == BRIG_TYPE_U64 || brig_type == BRIG_TYPE_U32)
	return TRUNC_MOD_EXPR;
      else
	return CALL_EXPR;
    case BRIG_OPCODE_NRCP:
    case BRIG_OPCODE_NRSQRT:
      /* Implement as 1/f (x).  gcc should pattern detect that and
	 use a native instruction, if available, for it.  */
      return TREE_LIST;
    case BRIG_OPCODE_FMA:
    case BRIG_OPCODE_FLOOR:
    case BRIG_OPCODE_CEIL:
    case BRIG_OPCODE_SQRT:
    case BRIG_OPCODE_NSQRT:
    case BRIG_OPCODE_RINT:
    case BRIG_OPCODE_TRUNC:
    case BRIG_OPCODE_POPCOUNT:
    case BRIG_OPCODE_COPYSIGN:
    case BRIG_OPCODE_NCOS:
    case BRIG_OPCODE_NSIN:
    case BRIG_OPCODE_NLOG2:
    case BRIG_OPCODE_NEXP2:
    case BRIG_OPCODE_NFMA:
      /* Class has type B1 regardless of the float type, thus
	 the below builtin map search cannot find it.  */
    case BRIG_OPCODE_CLASS:
    case BRIG_OPCODE_WORKITEMABSID:
      return CALL_EXPR;
    default:

      /* Some BRIG opcodes can use the same builtins for unsigned and
	 signed types.  Force these cases to unsigned types.
      */

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


      builtin_map::const_iterator i
	= s_custom_builtins.find (std::make_pair (brig_opcode, brig_type));
      if (i != s_custom_builtins.end ())
	return CALL_EXPR;
      else if (s_custom_builtins.find
	       (std::make_pair (brig_opcode, brig_inner_type))
	       != s_custom_builtins.end ())
	return CALL_EXPR;
      if (brig_inner_type == BRIG_TYPE_F16
	  && s_custom_builtins.find
	  (std::make_pair (brig_opcode, BRIG_TYPE_F32))
	  != s_custom_builtins.end ())
	return CALL_EXPR;
      break;
    }
  return TREE_LIST; /* Emulate using a chain of nodes.  */
}

/* Inform of an update to the REG_VAR.  */

void
brig_function::add_reg_var_update (tree reg_var, tree var)
{
  if (var == m_abs_id_vars[0] || var == m_abs_id_vars[1]
      || var == m_abs_id_vars[2] || var == m_local_id_vars[0]
      || var == m_local_id_vars[1] || var == m_local_id_vars[2])
    m_id_val_defs [reg_var] = var;
  else
    {
      /* Possible overwrite of an ID value.  */

      id_val_map::iterator i = m_id_val_defs.find (reg_var);
      if (i != m_id_val_defs.end())
	m_id_val_defs.erase (i);
    }
}

/* If the REG_VAR is known to contain an ID value at this point in
   the basic block, return true.  */

bool
brig_function::is_id_val (tree reg_var)
{
  id_val_map::iterator i = m_id_val_defs.find (reg_var);
  return i != m_id_val_defs.end();
}

/* Return an ID value for the given REG_VAR if its known to contain
   one at this point in the BB, NULL_TREE otherwise.  */

tree
brig_function::id_val (tree reg_var)
{
  id_val_map::iterator i = m_id_val_defs.find (reg_var);
  if (i != m_id_val_defs.end())
    return (*i).second;
  else
    return NULL_TREE;
}

/* Informs of starting a new basic block.  Called when generating
   a label, a call, a jump, or a return.  */

void
brig_function::start_new_bb ()
{
  m_id_val_defs.clear ();
}
