// Copyright (C) 2020-2022 Free Software Foundation, Inc.

// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include "rust-compile-base.h"
#include "fold-const.h"
#include "stringpool.h"

namespace Rust {
namespace Compile {

void
HIRCompileBase::setup_attributes_on_fndecl (
  tree fndecl, bool is_main_entry_point, bool has_visibility,
  const HIR::FunctionQualifiers &qualifiers, const AST::AttrVec &attrs)
{
  // if its the main fn or pub visibility mark its as DECL_PUBLIC
  // please see https://github.com/Rust-GCC/gccrs/pull/137
  if (is_main_entry_point || has_visibility)
    {
      TREE_PUBLIC (fndecl) = 1;
    }

  // is it a const fn
  if (qualifiers.is_const ())
    {
      TREE_READONLY (fndecl) = 1;
    }

  // is it inline?
  for (const auto &attr : attrs)
    {
      bool is_inline = attr.get_path ().as_string ().compare ("inline") == 0;
      if (is_inline)
	{
	  DECL_DECLARED_INLINE_P (fndecl) = 1;

	  // do we want to force inline regardless of optimisation level?
	  // https://gcc.gnu.org/onlinedocs/gcc/Inline.html
	  //
	  // /* Add attribute "always_inline": */
	  // DECL_ATTRIBUTES (fndecl)
	  //   = tree_cons (get_identifier ("always_inline"), NULL,
	  //       	 DECL_ATTRIBUTES (fndecl));
	}
    }
}

void
HIRCompileBase::setup_abi_options (tree fndecl, ABI abi)
{
  switch (abi)
    {
    case Rust::ABI::RUST:
    case Rust::ABI::INTRINSIC:
    case Rust::ABI::C:
    case Rust::ABI::CDECL:
      DECL_ATTRIBUTES (fndecl)
	= tree_cons (get_identifier ("cdecl"), NULL, DECL_ATTRIBUTES (fndecl));
      break;

    case Rust::ABI::STDCALL:
      DECL_ATTRIBUTES (fndecl) = tree_cons (get_identifier ("stdcall"), NULL,
					    DECL_ATTRIBUTES (fndecl));
      break;

    case Rust::ABI::FASTCALL:
      DECL_ATTRIBUTES (fndecl) = tree_cons (get_identifier ("fastcall"), NULL,
					    DECL_ATTRIBUTES (fndecl));

      break;

    default:
      break;
    }
}

// ported from gcc/c/c-typecheck.c
//
// Mark EXP saying that we need to be able to take the
// address of it; it should not be allocated in a register.
// Returns true if successful.  ARRAY_REF_P is true if this
// is for ARRAY_REF construction - in that case we don't want
// to look through VIEW_CONVERT_EXPR from VECTOR_TYPE to ARRAY_TYPE,
// it is fine to use ARRAY_REFs for vector subscripts on vector
// register variables.
bool
HIRCompileBase::mark_addressable (tree exp, Location locus)
{
  tree x = exp;

  while (1)
    switch (TREE_CODE (x))
      {
      case VIEW_CONVERT_EXPR:
	if (TREE_CODE (TREE_TYPE (x)) == ARRAY_TYPE
	    && VECTOR_TYPE_P (TREE_TYPE (TREE_OPERAND (x, 0))))
	  return true;
	x = TREE_OPERAND (x, 0);
	break;

      case COMPONENT_REF:
	// TODO
	// if (DECL_C_BIT_FIELD (TREE_OPERAND (x, 1)))
	//   {
	//     error ("cannot take address of bit-field %qD", TREE_OPERAND (x,
	//     1)); return false;
	//   }

	/* FALLTHRU */
      case ADDR_EXPR:
      case ARRAY_REF:
      case REALPART_EXPR:
      case IMAGPART_EXPR:
	x = TREE_OPERAND (x, 0);
	break;

      case COMPOUND_LITERAL_EXPR:
	TREE_ADDRESSABLE (x) = 1;
	TREE_ADDRESSABLE (COMPOUND_LITERAL_EXPR_DECL (x)) = 1;
	return true;

      case CONSTRUCTOR:
	TREE_ADDRESSABLE (x) = 1;
	return true;

      case VAR_DECL:
      case CONST_DECL:
      case PARM_DECL:
      case RESULT_DECL:
	// (we don't have a concept of a "register" declaration)
	// fallthrough */

	/* FALLTHRU */
      case FUNCTION_DECL:
	TREE_ADDRESSABLE (x) = 1;

	/* FALLTHRU */
      default:
	return true;
      }

  return false;
}

tree
HIRCompileBase::address_expression (tree expr, Location location)
{
  if (expr == error_mark_node)
    return error_mark_node;

  if (!mark_addressable (expr, location))
    return error_mark_node;

  return build_fold_addr_expr_loc (location.gcc_location (), expr);
}

} // namespace Compile
} // namespace Rust
