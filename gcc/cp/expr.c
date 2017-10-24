/* Convert language-specific tree expression to rtl instructions,
   for GNU compiler.
   Copyright (C) 1988-2017 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "cp-tree.h"

/* Expand C++-specific constants.  Currently, this means PTRMEM_CST.  */

tree
cplus_expand_constant (tree cst)
{
  switch (TREE_CODE (cst))
    {
    case PTRMEM_CST:
      {
	tree type = TREE_TYPE (cst);
	tree member;

	/* Find the member.  */
	member = PTRMEM_CST_MEMBER (cst);

	/* We can't lower this until the class is complete.  */
	if (!COMPLETE_TYPE_P (DECL_CONTEXT (member)))
	  return cst;

	if (TREE_CODE (member) == FIELD_DECL)
	  {
	    /* Find the offset for the field.  */
	    cst = byte_position (member);
	    while (!same_type_p (DECL_CONTEXT (member),
				 TYPE_PTRMEM_CLASS_TYPE (type)))
	      {
		/* The MEMBER must have been nestled within an
		   anonymous aggregate contained in TYPE.  Find the
		   anonymous aggregate.  */
		member = lookup_anon_field (TYPE_PTRMEM_CLASS_TYPE (type),
					    DECL_CONTEXT (member));
		cst = size_binop (PLUS_EXPR, cst, byte_position (member));
	      }
	    cst = fold (build_nop (type, cst));
	  }
	else
	  {
	    tree delta;
	    tree pfn;

	    expand_ptrmemfunc_cst (cst, &delta, &pfn);
	    cst = build_ptrmemfunc1 (type, delta, pfn);
	  }
      }
      break;

    case CONSTRUCTOR:
      {
	constructor_elt *elt;
	unsigned HOST_WIDE_INT idx;
	FOR_EACH_VEC_SAFE_ELT (CONSTRUCTOR_ELTS (cst), idx, elt)
	  elt->value = cplus_expand_constant (elt->value);
      }

    default:
      /* There's nothing to do.  */
      break;
    }

  return cst;
}

/* We've seen an actual use of EXPR.  Possibly replace an outer variable
   reference inside with its constant value or a lambda capture.  */

static tree
mark_use (tree expr, bool rvalue_p, bool read_p,
	  location_t loc /* = UNKNOWN_LOCATION */,
	  bool reject_builtin /* = true */)
{
#define RECUR(t) mark_use ((t), rvalue_p, read_p, loc, reject_builtin)

  if (expr == NULL_TREE || expr == error_mark_node)
    return expr;

  if (reject_builtin && reject_gcc_builtin (expr, loc))
    return error_mark_node;

  if (read_p)
    mark_exp_read (expr);

  tree oexpr = expr;
  bool recurse_op[3] = { false, false, false };
  switch (TREE_CODE (expr))
    {
    case VAR_DECL:
    case PARM_DECL:
      if (outer_automatic_var_p (expr)
	  && decl_constant_var_p (expr))
	{
	  if (rvalue_p)
	    {
	      tree t = maybe_constant_value (expr);
	      if (TREE_CONSTANT (t))
		{
		  expr = t;
		  break;
		}
	    }
	  expr = process_outer_var_ref (expr, tf_warning_or_error, true);
	  if (!(TREE_TYPE (oexpr)
		&& TREE_CODE (TREE_TYPE (oexpr)) == REFERENCE_TYPE))
	    expr = convert_from_reference (expr);
	}
      break;
    case COMPONENT_REF:
    case NON_DEPENDENT_EXPR:
      recurse_op[0] = true;
      break;
    case COMPOUND_EXPR:
      recurse_op[1] = true;
      break;
    case COND_EXPR:
      recurse_op[2] = true;
      if (TREE_OPERAND (expr, 1))
	recurse_op[1] = true;
      break;
    case INDIRECT_REF:
      if (REFERENCE_REF_P (expr))
	{
	  /* Try to look through the reference.  */
	  tree ref = TREE_OPERAND (expr, 0);
	  tree r = mark_rvalue_use (ref, loc, reject_builtin);
	  if (r != ref)
	    expr = convert_from_reference (r);
	}
      break;
    default:
      break;
    }

  for (int i = 0; i < 3; ++i)
    if (recurse_op[i])
      {
	tree op = TREE_OPERAND (expr, i);
	op = RECUR (op);
	if (op == error_mark_node)
	  return error_mark_node;
	TREE_OPERAND (expr, i) = op;
      }

  return expr;
#undef RECUR
}

/* Called whenever the expression EXPR is used in an rvalue context.
   When REJECT_BUILTIN is true the expression is checked to make sure
   it doesn't make it possible to obtain the address of a GCC built-in
   function with no library fallback (or any of its bits, such as in
   a conversion to bool).  */

tree
mark_rvalue_use (tree e,
		 location_t loc /* = UNKNOWN_LOCATION */,
		 bool reject_builtin /* = true */)
{
  return mark_use (e, true, true, loc, reject_builtin);
}

/* Called when expr appears as a discarded-value expression.  */

tree
mark_discarded_use (tree expr)
{
  /* The lvalue-to-rvalue conversion (7.1) is applied if and only if the
     expression is a glvalue of volatile-qualified type and it is one of the
     following:
     * ( expression ), where expression is one of these expressions,
     * id-expression (8.1.4),
     * subscripting (8.2.1),
     * class member access (8.2.5),
     * indirection (8.3.1),
     * pointer-to-member operation (8.5),
     * conditional expression (8.16) where both the second and the third
       operands are one of these expressions, or
     * comma expression (8.19) where the right operand is one of these
       expressions.  */
  if (expr == NULL_TREE)
    return expr;

  switch (TREE_CODE (expr))
    {
    case COND_EXPR:
      TREE_OPERAND (expr, 2) = mark_discarded_use (TREE_OPERAND (expr, 2));
      gcc_fallthrough ();
    case COMPOUND_EXPR:
      TREE_OPERAND (expr, 1) = mark_discarded_use (TREE_OPERAND (expr, 1));
      return expr;

    case COMPONENT_REF:
    case ARRAY_REF:
    case INDIRECT_REF:
    case MEMBER_REF:
      break;
    default:
      if (DECL_P (expr))
	break;
      else
	return expr;
    }

  /* Like mark_rvalue_use, but don't reject built-ins.  */
  return mark_use (expr, true, true, input_location, false);
}

/* Called whenever an expression is used in an lvalue context.  */

tree
mark_lvalue_use (tree expr)
{
  return mark_use (expr, false, true, input_location, false);
}

/* As above, but don't consider this use a read.  */

tree
mark_lvalue_use_nonread (tree expr)
{
  return mark_use (expr, false, false, input_location, false);
}

/* Called whenever an expression is used in a type use context.  */

tree
mark_type_use (tree expr)
{
  mark_exp_read (expr);
  return expr;
}

/* Mark EXP as read, not just set, for set but not used -Wunused
   warning purposes.  */

void
mark_exp_read (tree exp)
{
  if (exp == NULL)
    return;

  switch (TREE_CODE (exp))
    {
    case VAR_DECL:
      if (DECL_DECOMPOSITION_P (exp))
	mark_exp_read (DECL_DECOMP_BASE (exp));
      gcc_fallthrough ();
    case PARM_DECL:
      DECL_READ_P (exp) = 1;
      break;
    case ARRAY_REF:
    case COMPONENT_REF:
    case MODIFY_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
    CASE_CONVERT:
    case ADDR_EXPR:
    case INDIRECT_REF:
    case FLOAT_EXPR:
    case NON_DEPENDENT_EXPR:
    case VIEW_CONVERT_EXPR:
      mark_exp_read (TREE_OPERAND (exp, 0));
      break;
    case COMPOUND_EXPR:
      mark_exp_read (TREE_OPERAND (exp, 1));
      break;
    case COND_EXPR:
      if (TREE_OPERAND (exp, 1))
	mark_exp_read (TREE_OPERAND (exp, 1));
      if (TREE_OPERAND (exp, 2))
	mark_exp_read (TREE_OPERAND (exp, 2));
      break;
    default:
      break;
    }
}

