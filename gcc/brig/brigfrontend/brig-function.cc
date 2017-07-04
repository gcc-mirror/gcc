/* brig-function.cc -- declaration of brig_function class.
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

brig_function::brig_function (const BrigDirectiveExecutable *exec,
			      brig_to_generic *parent)
  : m_brig_def (exec), m_is_kernel (false), m_is_finished (false), m_name (""),
    m_current_bind_expr (NULL_TREE), m_func_decl (NULL_TREE),
    m_context_arg (NULL_TREE), m_group_base_arg (NULL_TREE),
    m_private_base_arg (NULL_TREE), m_ret_value (NULL_TREE),
    m_next_kernarg_offset (0), m_kernarg_max_align (0),
    m_ret_value_brig_var (NULL), m_has_barriers (false),
    m_has_allocas (false), m_has_function_calls_with_barriers (false),
    m_calls_analyzed (false), m_is_wg_function (false),
    m_has_unexpanded_dp_builtins (false), m_generating_arg_block (false),
    m_parent (parent)
{
  memset (m_regs, 0,
	  BRIG_2_TREE_HSAIL_TOTAL_REG_COUNT * sizeof (BrigOperandRegister *));
  memset (&m_descriptor, 0, sizeof (phsa_descriptor));
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

  tree_stmt_iterator entry = tsi_start (stmts);

  for (int i = 0; i < 3; ++i)
    {
      char dim_char = (char) ((int) 'x' + i);

      /* The local sizes are limited to 16b values, but let's still use 32b
	 to avoid unnecessary casts (the ID functions are 32b).  */
      m_local_id_vars[i]
	= add_local_variable (std::string ("__local_") + dim_char,
			      uint32_type_node);

      tree workitemid_call
	= call_builtin (builtin_decl_explicit (BUILT_IN_HSAIL_WORKITEMID), 2,
			uint32_type_node, uint32_type_node,
			build_int_cst (uint32_type_node, i), ptr_type_node,
			m_context_arg);

      tree id_init = build2 (MODIFY_EXPR, TREE_TYPE (m_local_id_vars[i]),
			     m_local_id_vars[i], workitemid_call);

      tsi_link_after (&entry, id_init, TSI_NEW_STMT);

      m_cur_wg_size_vars[i]
	= add_local_variable (std::string ("__cur_wg_size_") + dim_char,
			      uint32_type_node);

      tree cwgz_call
	= call_builtin
	(builtin_decl_explicit (BUILT_IN_HSAIL_CURRENTWORKGROUPSIZE),
	 2, uint32_type_node, uint32_type_node,
	 build_int_cst (uint32_type_node, i), ptr_type_node, m_context_arg);

      tree limit_init = build2 (MODIFY_EXPR, TREE_TYPE (m_cur_wg_size_vars[i]),
				m_cur_wg_size_vars[i], cwgz_call);

      tsi_link_after (&entry, limit_init, TSI_NEW_STMT);

      m_wg_id_vars[i]
	= add_local_variable (std::string ("__workgroupid_") + dim_char,
			      uint32_type_node);

      tree wgid_call
	= call_builtin (builtin_decl_explicit (BUILT_IN_HSAIL_WORKGROUPID),
			2, uint32_type_node, uint32_type_node,
			build_int_cst (uint32_type_node, i), ptr_type_node,
			m_context_arg);

      tree wgid_init = build2 (MODIFY_EXPR, TREE_TYPE (m_wg_id_vars[i]),
			       m_wg_id_vars[i], wgid_call);

      tsi_link_after (&entry, wgid_init, TSI_NEW_STMT);

      m_wg_size_vars[i]
	= add_local_variable (std::string ("__workgroupsize_") + dim_char,
			      uint32_type_node);

      tree wgsize_call
	= call_builtin (builtin_decl_explicit (BUILT_IN_HSAIL_WORKGROUPSIZE),
			2, uint32_type_node, uint32_type_node,
			build_int_cst (uint32_type_node, i), ptr_type_node,
			m_context_arg);

      tree wgsize_init = build2 (MODIFY_EXPR, TREE_TYPE (m_wg_size_vars[i]),
				 m_wg_size_vars[i], wgsize_call);

      tsi_link_after (&entry, wgsize_init, TSI_NEW_STMT);

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

      tsi_link_after (&entry, gridsize_init, TSI_NEW_STMT);
    }

  m_kernel_entry = entry;
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

/* Returns a DECL_VAR for the given HSAIL operand register.
   If it has not been created yet for the function being generated,
   creates it as an unsigned int variable.  */

tree
brig_function::get_m_var_declfor_reg (const BrigOperandRegister *reg)
{
  size_t offset = reg->regNum;
  switch (reg->regKind)
    {
    case BRIG_REGISTER_KIND_QUAD:
      offset
	+= BRIG_2_TREE_HSAIL_D_REG_COUNT + BRIG_2_TREE_HSAIL_S_REG_COUNT +
	BRIG_2_TREE_HSAIL_C_REG_COUNT;
      break;
    case BRIG_REGISTER_KIND_DOUBLE:
      offset += BRIG_2_TREE_HSAIL_S_REG_COUNT + BRIG_2_TREE_HSAIL_C_REG_COUNT;
      break;
    case BRIG_REGISTER_KIND_SINGLE:
      offset += BRIG_2_TREE_HSAIL_C_REG_COUNT;
    case BRIG_REGISTER_KIND_CONTROL:
      break;
    default:
      gcc_unreachable ();
      break;
    }

  reg_decl_index_entry *regEntry = m_regs[offset];
  if (regEntry == NULL)
    {
      size_t reg_size = gccbrig_reg_size (reg);
      tree type;
      if (reg_size > 1)
	type = build_nonstandard_integer_type (reg_size, true);
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
  tree ivar_max = m_cur_wg_size_vars[dim];
  tree_stmt_iterator entry = *header_entry;

  /* TODO: this is not a parallel loop as we share the "register variables"
     across work-items.  Should create a copy of them per WI instance.  That
     is, declare temporaries for new definitions inside the loop body, not at
     function scope.  */

  tree ivar_init = build2 (MODIFY_EXPR, TREE_TYPE (ivar), ivar,
			   build_zero_cst (TREE_TYPE (ivar)));
  tsi_link_after (&entry, ivar_init, TSI_NEW_STMT);

  tree loop_body_label
    = label (std::string ("__wi_loop_") + (char) ((int) 'x' + dim));
  tree loop_body_label_stmt = build_stmt (LABEL_EXPR, loop_body_label);

  tsi_link_after (&entry, loop_body_label_stmt, TSI_NEW_STMT);

  if (m_has_unexpanded_dp_builtins)
    {
      tree id_set_builtin
	= builtin_decl_explicit (BUILT_IN_HSAIL_SETWORKITEMID);
      /* Set the local ID to the current wi-loop iteration variable value to
	 ensure the builtins see the correct values.  */
      tree id_set_call
	= call_builtin (id_set_builtin, 3,
			void_type_node, uint32_type_node,
			build_int_cst (uint32_type_node, dim), uint32_type_node,
			ivar, ptr_type_node, m_context_arg);
      tsi_link_after (&entry, id_set_call, TSI_NEW_STMT);
    }

  /* Increment the WI iteration variable.  */
  tree incr = build2 (PREINCREMENT_EXPR, TREE_TYPE (ivar), ivar,
		      build_one_cst (TREE_TYPE (ivar)));

  tsi_link_after (branch_after, incr, TSI_NEW_STMT);

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

  tree launcher
    = build_decl (UNKNOWN_LOCATION, FUNCTION_DECL, name_identifier,
		  build_function_type_list (void_type_node, ptr_type_node,
					    ptr_type_node, NULL_TREE));

  TREE_USED (launcher) = 1;
  DECL_ARTIFICIAL (launcher) = 1;

  tree context_arg = build_decl (UNKNOWN_LOCATION, PARM_DECL,
				 get_identifier ("__context"), ptr_type_node);

  DECL_ARGUMENTS (launcher) = context_arg;
  DECL_ARG_TYPE (context_arg) = ptr_type_node;
  DECL_CONTEXT (context_arg) = launcher;
  TREE_USED (context_arg) = 1;
  DECL_ARTIFICIAL (context_arg) = 1;

  tree group_base_addr_arg
    = build_decl (UNKNOWN_LOCATION, PARM_DECL,
		  get_identifier ("__group_base_addr"), ptr_type_node);

  chainon (DECL_ARGUMENTS (launcher), group_base_addr_arg);
  DECL_ARG_TYPE (group_base_addr_arg) = ptr_type_node;
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

  TREE_STATIC (launcher) = 0;
  TREE_PUBLIC (launcher) = 1;

  DECL_SAVED_TREE (launcher) = bind_expr;

  if (DECL_STRUCT_FUNCTION (launcher) == NULL)
    push_struct_function (launcher);
  else
    push_cfun (DECL_STRUCT_FUNCTION (launcher));

  tree kernel_func_ptr = build1 (ADDR_EXPR, ptr_type_node, m_func_decl);

  tree phsail_launch_kernel_call;

  /* Emit a launcher depending whether we converted the kernel function to
     a work group function or not.  */
  if (m_is_wg_function)
    phsail_launch_kernel_call
      = call_builtin (builtin_decl_explicit (BUILT_IN_HSAIL_LAUNCH_WG_FUNC),
		      3, void_type_node,
		      ptr_type_node, kernel_func_ptr, ptr_type_node,
		      context_arg, ptr_type_node, group_base_addr_arg);
  else
    phsail_launch_kernel_call
      = call_builtin (builtin_decl_explicit (BUILT_IN_HSAIL_LAUNCH_KERNEL),
		      3, void_type_node,
		      ptr_type_node, kernel_func_ptr, ptr_type_node,
		      context_arg, ptr_type_node, group_base_addr_arg);

  append_to_statement_list_force (phsail_launch_kernel_call, &stmt_list);

  emit_metadata (stmt_list);

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
