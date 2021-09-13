/* Utility functions for the analyzer.
   Copyright (C) 2019-2021 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "diagnostic.h"
#include "intl.h"
#include "function.h"
#include "analyzer/analyzer.h"

#if ENABLE_ANALYZER

namespace ana {

/* Workaround for missing location information for some stmts,
   which ultimately should be solved by fixing the frontends
   to provide the locations (TODO).  */

location_t
get_stmt_location (const gimple *stmt, function *fun)
{
  if (get_pure_location (stmt->location) == UNKNOWN_LOCATION)
    {
      /* Workaround for missing location information for clobber
	 stmts, which seem to lack location information in the C frontend
	 at least.  Created by gimplify_bind_expr, which uses the
	   BLOCK_SOURCE_END_LOCATION (BIND_EXPR_BLOCK (bind_expr))
	 but this is never set up when the block is created in
	 c_end_compound_stmt's pop_scope.
	 TODO: fix this missing location information.

	 For now, as a hackish workaround, use the location of the end of
	 the function.  */
      if (gimple_clobber_p (stmt) && fun)
	return fun->function_end_locus;
    }

  return stmt->location;
}

static tree
fixup_tree_for_diagnostic_1 (tree expr, hash_set<tree> *visited);

/* Attemp to generate a tree for the LHS of ASSIGN_STMT.
   VISITED must be non-NULL; it is used to ensure termination.  */

static tree
get_diagnostic_tree_for_gassign_1 (const gassign *assign_stmt,
				   hash_set<tree> *visited)
{
  enum tree_code code = gimple_assign_rhs_code (assign_stmt);

  /* Reverse the effect of extract_ops_from_tree during
     gimplification.  */
  switch (get_gimple_rhs_class (code))
    {
    default:
    case GIMPLE_INVALID_RHS:
      gcc_unreachable ();
    case GIMPLE_TERNARY_RHS:
    case GIMPLE_BINARY_RHS:
    case GIMPLE_UNARY_RHS:
      {
	tree t = make_node (code);
	TREE_TYPE (t) = TREE_TYPE (gimple_assign_lhs (assign_stmt));
	unsigned num_rhs_args = gimple_num_ops (assign_stmt) - 1;
	for (unsigned i = 0; i < num_rhs_args; i++)
	  {
	    tree op = gimple_op (assign_stmt, i + 1);
	    if (op)
	      {
		op = fixup_tree_for_diagnostic_1 (op, visited);
		if (op == NULL_TREE)
		  return NULL_TREE;
	      }
	    TREE_OPERAND (t, i) = op;
	  }
	return t;
      }
    case GIMPLE_SINGLE_RHS:
      {
	tree op = gimple_op (assign_stmt, 1);
	op = fixup_tree_for_diagnostic_1 (op, visited);
	return op;
      }
    }
}

/*  Subroutine of fixup_tree_for_diagnostic_1, called on SSA names.
    Attempt to reconstruct a a tree expression for SSA_NAME
    based on its def-stmt.
    SSA_NAME must be non-NULL.
    VISITED must be non-NULL; it is used to ensure termination.

    Return NULL_TREE if there is a problem.  */

static tree
maybe_reconstruct_from_def_stmt (tree ssa_name,
				 hash_set<tree> *visited)
{
  /* Ensure termination.  */
  if (visited->contains (ssa_name))
    return NULL_TREE;
  visited->add (ssa_name);

  gimple *def_stmt = SSA_NAME_DEF_STMT (ssa_name);

  switch (gimple_code (def_stmt))
    {
    default:
      gcc_unreachable ();
    case GIMPLE_ASM:
    case GIMPLE_NOP:
    case GIMPLE_PHI:
      /* Can't handle these.  */
      return NULL_TREE;
    case GIMPLE_ASSIGN:
      return get_diagnostic_tree_for_gassign_1
	(as_a <const gassign *> (def_stmt), visited);
    case GIMPLE_CALL:
      {
	gcall *call_stmt = as_a <gcall *> (def_stmt);
	tree return_type = gimple_call_return_type (call_stmt);
	tree fn = fixup_tree_for_diagnostic_1 (gimple_call_fn (call_stmt),
					       visited);
	if (fn == NULL_TREE)
	  return NULL_TREE;
	unsigned num_args = gimple_call_num_args (call_stmt);
	auto_vec<tree> args (num_args);
	for (unsigned i = 0; i < num_args; i++)
	  {
	    tree arg = gimple_call_arg (call_stmt, i);
	    arg = fixup_tree_for_diagnostic_1 (arg, visited);
	    if (arg == NULL_TREE)
	      return NULL_TREE;
	    args.quick_push (arg);
	  }
	gcc_assert (fn);
	return build_call_array_loc (gimple_location (call_stmt),
				     return_type, fn,
				     num_args, args.address ());
      }
      break;
    }
}

/* Subroutine of fixup_tree_for_diagnostic: attempt to fixup EXPR,
   which can be NULL.
   VISITED must be non-NULL; it is used to ensure termination.  */

static tree
fixup_tree_for_diagnostic_1 (tree expr, hash_set<tree> *visited)
{
  if (expr
      && TREE_CODE (expr) == SSA_NAME
      && (SSA_NAME_VAR (expr) == NULL_TREE
	  || DECL_ARTIFICIAL (SSA_NAME_VAR (expr))))
    {
      if (tree var = SSA_NAME_VAR (expr))
	if (VAR_P (var) && DECL_HAS_DEBUG_EXPR_P (var))
	  return DECL_DEBUG_EXPR (var);
      if (tree expr2 = maybe_reconstruct_from_def_stmt (expr, visited))
	return expr2;
    }
  return expr;
}

/* We don't want to print '<unknown>' in our diagnostics (PR analyzer/99771),
   but sometimes we generate diagnostics involving an ssa name for a
   temporary.

   Work around this by attempting to reconstruct a tree expression for
   such temporaries based on their def-stmts.

   Otherwise return EXPR.

   EXPR can be NULL.  */

tree
fixup_tree_for_diagnostic (tree expr)
{
  hash_set<tree> visited;
  return fixup_tree_for_diagnostic_1 (expr, &visited);
}

/* Attempt to generate a tree for the LHS of ASSIGN_STMT.  */

tree
get_diagnostic_tree_for_gassign (const gassign *assign_stmt)
{
  hash_set<tree> visited;
  return get_diagnostic_tree_for_gassign_1 (assign_stmt, &visited);
}

} // namespace ana

/* Helper function for checkers.  Is the CALL to the given function name,
   and with the given number of arguments?

   This doesn't resolve function pointers via the region model;
   is_named_call_p should be used instead, using a fndecl from
   get_fndecl_for_call; this function should only be used for special cases
   where it's not practical to get at the region model, or for special
   analyzer functions such as __analyzer_dump.  */

bool
is_special_named_call_p (const gcall *call, const char *funcname,
			 unsigned int num_args)
{
  gcc_assert (funcname);

  tree fndecl = gimple_call_fndecl (call);
  if (!fndecl)
    return false;

  return is_named_call_p (fndecl, funcname, call, num_args);
}

/* Helper function for checkers.  Is FNDECL an extern fndecl at file scope
   that has the given FUNCNAME?

   Compare with special_function_p in calls.c.  */

bool
is_named_call_p (const_tree fndecl, const char *funcname)
{
  gcc_assert (fndecl);
  gcc_assert (funcname);

  if (!maybe_special_function_p (fndecl))
    return false;

  tree identifier = DECL_NAME (fndecl);
  const char *name = IDENTIFIER_POINTER (identifier);
  const char *tname = name;

  /* Potentially disregard prefix _ or __ in FNDECL's name, but not if
     FUNCNAME itself has leading underscores (e.g. when looking for
     "__analyzer_eval").  */
  if (funcname[0] != '_' && name[0] == '_')
    {
      if (name[1] == '_')
	tname += 2;
      else
	tname += 1;
    }

  return 0 == strcmp (tname, funcname);
}

/* Return true if FNDECL is within the namespace "std".
   Compare with cp/typeck.c: decl_in_std_namespace_p, but this doesn't
   rely on being the C++ FE (or handle inline namespaces inside of std).  */

static inline bool
is_std_function_p (const_tree fndecl)
{
  tree name_decl = DECL_NAME (fndecl);
  if (!name_decl)
    return false;
  if (!DECL_CONTEXT (fndecl))
    return false;
  if (TREE_CODE (DECL_CONTEXT (fndecl)) != NAMESPACE_DECL)
    return false;
  tree ns = DECL_CONTEXT (fndecl);
  if (!(DECL_CONTEXT (ns) == NULL_TREE
	|| TREE_CODE (DECL_CONTEXT (ns)) == TRANSLATION_UNIT_DECL))
    return false;
  if (!DECL_NAME (ns))
    return false;
  return id_equal ("std", DECL_NAME (ns));
}

/* Like is_named_call_p, but look for std::FUNCNAME.  */

bool
is_std_named_call_p (const_tree fndecl, const char *funcname)
{
  gcc_assert (fndecl);
  gcc_assert (funcname);

  if (!is_std_function_p (fndecl))
    return false;

  tree identifier = DECL_NAME (fndecl);
  const char *name = IDENTIFIER_POINTER (identifier);
  const char *tname = name;

  /* Don't disregard prefix _ or __ in FNDECL's name.  */

  return 0 == strcmp (tname, funcname);
}

/* Helper function for checkers.  Is FNDECL an extern fndecl at file scope
   that has the given FUNCNAME, and does CALL have the given number of
   arguments?  */

bool
is_named_call_p (const_tree fndecl, const char *funcname,
		 const gcall *call, unsigned int num_args)
{
  gcc_assert (fndecl);
  gcc_assert (funcname);

  if (!is_named_call_p (fndecl, funcname))
    return false;

  if (gimple_call_num_args (call) != num_args)
    return false;

  return true;
}

/* Like is_named_call_p, but check for std::FUNCNAME.  */

bool
is_std_named_call_p (const_tree fndecl, const char *funcname,
		     const gcall *call, unsigned int num_args)
{
  gcc_assert (fndecl);
  gcc_assert (funcname);

  if (!is_std_named_call_p (fndecl, funcname))
    return false;

  if (gimple_call_num_args (call) != num_args)
    return false;

  return true;
}

/* Return true if stmt is a setjmp or sigsetjmp call.  */

bool
is_setjmp_call_p (const gcall *call)
{
  if (is_special_named_call_p (call, "setjmp", 1)
      || is_special_named_call_p (call, "sigsetjmp", 2))
    /* region_model::on_setjmp requires a pointer.  */
    if (POINTER_TYPE_P (TREE_TYPE (gimple_call_arg (call, 0))))
      return true;

  return false;
}

/* Return true if stmt is a longjmp or siglongjmp call.  */

bool
is_longjmp_call_p (const gcall *call)
{
  if (is_special_named_call_p (call, "longjmp", 2)
      || is_special_named_call_p (call, "siglongjmp", 2))
    /* exploded_node::on_longjmp requires a pointer for the initial
       argument.  */
    if (POINTER_TYPE_P (TREE_TYPE (gimple_call_arg (call, 0))))
      return true;

  return false;
}

/* For a CALL that matched is_special_named_call_p or is_named_call_p for
   some name, return a name for the called function suitable for use in
   diagnostics (stripping the leading underscores).  */

const char *
get_user_facing_name (const gcall *call)
{
  tree fndecl = gimple_call_fndecl (call);
  gcc_assert (fndecl);

  tree identifier = DECL_NAME (fndecl);
  gcc_assert (identifier);

  const char *name = IDENTIFIER_POINTER (identifier);

  /* Strip prefix _ or __ in FNDECL's name.  */
  if (name[0] == '_')
    {
      if (name[1] == '_')
	return name + 2;
      else
	return name + 1;
    }

  return name;
}

/* Generate a label_text instance by formatting FMT, using a
   temporary clone of the global_dc's printer (thus using its
   formatting callbacks).

   Colorize if the global_dc supports colorization and CAN_COLORIZE is
   true.  */

label_text
make_label_text (bool can_colorize, const char *fmt, ...)
{
  pretty_printer *pp = global_dc->printer->clone ();
  pp_clear_output_area (pp);

  if (!can_colorize)
    pp_show_color (pp) = false;

  text_info ti;
  rich_location rich_loc (line_table, UNKNOWN_LOCATION);

  va_list ap;

  va_start (ap, fmt);

  ti.format_spec = _(fmt);
  ti.args_ptr = &ap;
  ti.err_no = 0;
  ti.x_data = NULL;
  ti.m_richloc = &rich_loc;

  pp_format (pp, &ti);
  pp_output_formatted_text (pp);

  va_end (ap);

  label_text result = label_text::take (xstrdup (pp_formatted_text (pp)));
  delete pp;
  return result;
}

#endif /* #if ENABLE_ANALYZER */
