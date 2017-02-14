/* brig-code-entry-handler.cc -- brig function directive handling
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

#include "brig-code-entry-handler.h"

#include "brig-machine.h"
#include "stringpool.h"
#include "tree-iterator.h"
#include "gimple-expr.h"
#include "function.h"
#include "phsa.h"

#include "tree-pretty-print.h"
#include "print-tree.h"

extern int gccbrig_verbose;

size_t
brig_directive_function_handler::operator () (const BrigBase *base)
{
  m_parent.finish_function ();

  size_t bytes_consumed = base->byteCount;

  const BrigDirectiveExecutable *exec = (const BrigDirectiveExecutable *) base;

  if (gccbrig_verbose)
    {
      printf ("brig: function name %s\n",
	      m_parent.get_string (exec->name).c_str());
      printf ("brig: inargs %d outargs %d name offset %d\n", exec->inArgCount,
	      exec->outArgCount, exec->name);
    }

  const bool is_definition
    = exec->modifier & BRIG_EXECUTABLE_DEFINITION;

  const bool is_kernel = base->kind == BRIG_KIND_DIRECTIVE_KERNEL;

  /* There doesn't seem to be actual use cases for kernel declarations
     as they cannot be called by the program.  Ignore them until there's
     a reason not to.  */
  if (is_kernel && !is_definition)
    return bytes_consumed;

  m_parent.m_cf = new brig_function (exec, &m_parent);

  std::string func_name = m_parent.get_mangled_name (exec);

  tree fndecl;
  tree ret_value = NULL_TREE;

  tree stmt_list = alloc_stmt_list ();

  /* Add a function scope BIND_EXPR using which we can push local variables that
     represent HSAIL registers.  */
  tree bind_expr = build3 (BIND_EXPR, void_type_node, NULL, stmt_list, NULL);

  if (is_kernel)
    {
      /* The generated kernel function is not the one that should be
	 called by the host.  */
      func_name = std::string ("_") + func_name;

      tree name_identifier
	= get_identifier_with_length (func_name.c_str (), func_name.size ());

      /* The generated kernel functions take the following arguments:

	 1) a char* which is a starting address of the argument segment where
	 the call's arguments are stored by the launcher.
	 2) a void* parameter that points to a phsail-finalizer context object
	 which passes the hsa kernel packet etc.
	 3) a void* parameter that contains the first flat address of the group
	 region allocated to the current work-group.  */

      tree char_ptr_type_node = build_pointer_type (char_type_node);
      fndecl = build_decl (UNKNOWN_LOCATION, FUNCTION_DECL, name_identifier,
			   build_function_type_list (void_type_node,
						     char_ptr_type_node,
						     ptr_type_node,
						     ptr_type_node, NULL_TREE));

      SET_DECL_ASSEMBLER_NAME (fndecl, name_identifier);

      tree resdecl
	= build_decl (UNKNOWN_LOCATION, RESULT_DECL, NULL_TREE, void_type_node);

      tree typelist = TYPE_ARG_TYPES (TREE_TYPE (fndecl));
      tree argtype = TREE_VALUE (typelist);
      TYPE_ADDR_SPACE (argtype)
	= gccbrig_get_target_addr_space_id (BRIG_SEGMENT_KERNARG);

      tree arg_arg = build_decl (UNKNOWN_LOCATION, PARM_DECL,
				 get_identifier ("__args"), char_ptr_type_node);
      DECL_ARGUMENTS (fndecl) = arg_arg;
      DECL_ARG_TYPE (arg_arg) = char_ptr_type_node;
      DECL_CONTEXT (arg_arg) = fndecl;
      DECL_ARTIFICIAL (arg_arg) = 1;
      TREE_READONLY (arg_arg) = 1;
      TREE_USED (arg_arg) = 1;

      DECL_RESULT (fndecl) = resdecl;
      DECL_CONTEXT (resdecl) = fndecl;
      DECL_EXTERNAL (fndecl) = 0;
    }
  else
    {
      /* Build a regular function fingerprint to enable targets to optimize
	 the calling convention as they see fit.  */
      tree name_identifier
	= get_identifier_with_length (func_name.c_str (), func_name.size ());

      m_parent.m_cf->m_arg_variables.clear ();

      brig_directive_variable_handler arg_handler (m_parent);

      vec<tree, va_gc> *args;
      vec_alloc (args, 4);

      tree arg_decls = NULL_TREE;

      tree ret_type = void_type_node;
      if (exec->outArgCount == 1)
	{
	  /* The return value variable should be the first entry after the
	     function directive.  */
	  const BrigBase *retval
	    = (const BrigBase *) ((const char *) base + base->byteCount);
	  gcc_assert (retval->kind == BRIG_KIND_DIRECTIVE_VARIABLE);

	  const BrigDirectiveVariable *brigVar
	    = (const BrigDirectiveVariable *) retval;

	  brig_directive_variable_handler varhandler (m_parent);

	  if (brigVar->type & BRIG_TYPE_ARRAY)
	    {
	      /* Push array output arguments to the beginning of the
		 function argument list instead of regular function
		 return values.  */

	      tree arg_var = varhandler.build_variable (brigVar, PARM_DECL);
	      vec_safe_push (args, TREE_TYPE (arg_var));

	      m_parent.m_cf->add_arg_variable (brigVar, arg_var);

	      if (arg_decls == NULL_TREE)
		arg_decls = arg_var;
	      else
		chainon (arg_decls, arg_var);

	      m_parent.m_cf->add_arg_variable (brigVar, arg_var);

	      ret_value = build_decl (UNKNOWN_LOCATION, RESULT_DECL, NULL_TREE,
				      void_type_node);
	    }
	  else
	    {
	      ret_value = varhandler.build_variable (brigVar, RESULT_DECL);
	      m_parent.m_cf->m_ret_value = ret_value;
	      ret_type = TREE_TYPE (ret_value);
	      m_parent.m_cf->m_ret_value_brig_var = brigVar;
	    }
	  bytes_consumed += retval->byteCount;
	}
      else
	ret_value = build_decl (UNKNOWN_LOCATION, RESULT_DECL, NULL_TREE,
				void_type_node);

      TREE_ADDRESSABLE (ret_value) = 1;

      if (exec->inArgCount > 0)
	{
	  uint32_t arg_offset = exec->firstInArg;
	  for (size_t arg = 0; arg < exec->inArgCount; ++arg)
	    {

	      const BrigDirectiveVariable *brigVar
		= (const BrigDirectiveVariable *) m_parent.get_brig_code_entry
		(arg_offset);

	      gcc_assert (brigVar->base.kind == BRIG_KIND_DIRECTIVE_VARIABLE);

	      /* Delegate to the brig_directive_variable_handler.  */
	      brig_directive_variable_handler varhandler (m_parent);
	      tree arg_var = varhandler.build_variable (brigVar, PARM_DECL);
	      arg_offset += brigVar->base.byteCount;
	      vec_safe_push (args, TREE_TYPE (arg_var));

	      m_parent.m_cf->add_arg_variable (brigVar, arg_var);

	      if (arg_decls == NULL_TREE)
		arg_decls = arg_var;
	      else
		chainon (arg_decls, arg_var);
	    }
	}

      vec_safe_push (args, ptr_type_node);
      vec_safe_push (args, ptr_type_node);

      fndecl = build_decl (UNKNOWN_LOCATION, FUNCTION_DECL, name_identifier,
			   build_function_type_vec (ret_type, args));

      DECL_RESULT (fndecl) = ret_value;
      DECL_CONTEXT (ret_value) = fndecl;
      DECL_EXTERNAL (fndecl) = 0;
      DECL_ARGUMENTS (fndecl) = arg_decls;
    }

  /* All functions need the hidden __context argument passed on
     because they might call WI-specific functions which need
     the context info.  */
  tree context_arg = build_decl (UNKNOWN_LOCATION, PARM_DECL,
				 get_identifier ("__context"), ptr_type_node);
  if (DECL_ARGUMENTS (fndecl) == NULL_TREE)
    DECL_ARGUMENTS (fndecl) = context_arg;
  else
    chainon (DECL_ARGUMENTS (fndecl), context_arg);
  DECL_CONTEXT (context_arg) = fndecl;
  DECL_ARG_TYPE (context_arg) = ptr_type_node;
  DECL_ARTIFICIAL (context_arg) = 1;
  TREE_READONLY (context_arg) = 1;
  TREE_USED (context_arg) = 1;

  /* They can also access group memory, so we need to pass the
     group pointer along too.  */
  tree group_base_arg
    = build_decl (UNKNOWN_LOCATION, PARM_DECL,
		  get_identifier ("__group_base_addr"), ptr_type_node);
  chainon (DECL_ARGUMENTS (fndecl), group_base_arg);
  DECL_ARG_TYPE (group_base_arg) = ptr_type_node;
  DECL_CONTEXT (group_base_arg) = fndecl;
  DECL_ARTIFICIAL (group_base_arg) = 1;
  TREE_READONLY (group_base_arg) = 1;
  TREE_USED (group_base_arg) = 1;

  /* Same for private.  */
  tree private_base_arg
    = build_decl (UNKNOWN_LOCATION, PARM_DECL,
		  get_identifier ("__private_base_addr"), ptr_type_node);
  chainon (DECL_ARGUMENTS (fndecl), private_base_arg);
  DECL_ARG_TYPE (private_base_arg) = ptr_type_node;
  DECL_CONTEXT (private_base_arg) = fndecl;
  DECL_ARTIFICIAL (private_base_arg) = 1;
  TREE_READONLY (private_base_arg) = 1;
  TREE_USED (private_base_arg) = 1;

  DECL_SAVED_TREE (fndecl) = bind_expr;

  /* Try to preserve the functions across IPA.  */
  DECL_PRESERVE_P (fndecl) = 1;
  TREE_SIDE_EFFECTS (fndecl) = 1;

  TREE_ADDRESSABLE (fndecl) = 1;

  if (base->kind == BRIG_KIND_DIRECTIVE_FUNCTION)
    {
      TREE_STATIC (fndecl) = 1;
      TREE_PUBLIC (fndecl) = 1;
    }
  else if (base->kind == BRIG_KIND_DIRECTIVE_KERNEL)
    {
      TREE_STATIC (fndecl) = 1;
      TREE_PUBLIC (fndecl) = 1;
    }
  else if (base->kind == BRIG_KIND_DIRECTIVE_SIGNATURE)
    {
      TREE_STATIC (fndecl) = 0;
      TREE_PUBLIC (fndecl) = 1;
      DECL_EXTERNAL (fndecl) = 1;
    }
  else if (base->kind == BRIG_KIND_DIRECTIVE_INDIRECT_FUNCTION)
    {
      TREE_STATIC (fndecl) = 0;
      TREE_PUBLIC (fndecl) = 1;
    }
  else
    gcc_unreachable ();

  TREE_USED (fndecl) = 1;
  DECL_ARTIFICIAL (fndecl) = 0;

  tree initial_block = make_node (BLOCK);
  DECL_INITIAL (fndecl) = initial_block;
  TREE_USED (DECL_INITIAL (fndecl)) = 1;

  if (ret_value != NULL_TREE && TREE_TYPE (ret_value) != void_type_node)
    {
      DECL_CONTEXT (ret_value) = fndecl;
      DECL_CHAIN (ret_value) = BIND_EXPR_VARS (bind_expr);
      BIND_EXPR_VARS (bind_expr) = ret_value;
    }

  tree arg;
  for (arg = DECL_ARGUMENTS (fndecl); arg != NULL_TREE; arg = TREE_CHAIN (arg))
    {
      DECL_CONTEXT (arg) = fndecl;
      DECL_ARG_TYPE (arg) = TREE_TYPE (arg);
    }

  m_parent.add_function_decl (func_name, fndecl);
  m_parent.append_global (fndecl);

  if (!is_definition)
    return bytes_consumed;

  m_parent.start_function (fndecl);

  m_parent.m_cf->m_name = func_name;
  m_parent.m_cf->m_func_decl = fndecl;
  m_parent.m_cf->m_current_bind_expr = bind_expr;
  m_parent.m_cf->m_is_kernel = is_kernel;
  m_parent.m_cf->m_context_arg = context_arg;
  m_parent.m_cf->m_group_base_arg = group_base_arg;
  m_parent.m_cf->m_private_base_arg = private_base_arg;

  if (ret_value != NULL_TREE && TREE_TYPE (ret_value) != void_type_node)
    {
      /* We cannot assign to <<retval>> directly in gcc trunk.  We need to
	 create a local temporary variable which can be stored to and when
	 returning from the function, we'll copy it to the actual <<retval>>
	 in return statement's argument.  */
      tree temp_var = m_parent.m_cf->m_ret_temp
	= m_parent.m_cf->add_local_variable ("_retvalue_temp",
					     TREE_TYPE (ret_value));
      TREE_ADDRESSABLE (temp_var) = 1;
    }

  if (is_kernel)
    {
      m_parent.m_cf->add_id_variables ();

      /* Create a single entry point in the function.  */
      m_parent.m_cf->m_entry_label_stmt
	= build_stmt (LABEL_EXPR, m_parent.m_cf->label ("__kernel_entry"));
      m_parent.m_cf->append_statement (m_parent.m_cf->m_entry_label_stmt);

      tree bind_expr = m_parent.m_cf->m_current_bind_expr;
      tree stmts = BIND_EXPR_BODY (bind_expr);

      m_parent.m_cf->m_kernel_entry = tsi_last (stmts);

      /* Let's not append the exit label yet, but only after the
	 function has been built.  We need to build it so it can
	 be referred to because returns are converted to gotos to this
	 label.  */
      m_parent.m_cf->m_exit_label = m_parent.m_cf->label ("__kernel_exit");
    }

  return bytes_consumed;
}
