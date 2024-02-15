// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

#include "rust-compile-context.h"
#include "rust-compile-type.h"

namespace Rust {
namespace Compile {

Context::Context ()
  : resolver (Resolver::Resolver::get ()),
    tyctx (Resolver::TypeCheckContext::get ()),
    mappings (Analysis::Mappings::get ()), mangler (Mangler ())
{
  setup_builtins ();
}

void
Context::setup_builtins ()
{
  auto builtins = resolver->get_builtin_types ();
  for (auto it = builtins.begin (); it != builtins.end (); it++)
    {
      HirId ref;
      bool ok = tyctx->lookup_type_by_node_id ((*it)->get_node_id (), &ref);
      rust_assert (ok);

      TyTy::BaseType *lookup;
      ok = tyctx->lookup_type (ref, &lookup);
      rust_assert (ok);

      TyTyResolveCompile::compile (this, lookup);
    }
}

hashval_t
Context::type_hasher (tree type)
{
  inchash::hash hstate;

  hstate.add_int (TREE_CODE (type));

  if (TYPE_NAME (type))
    {
      hashval_t record_name_hash
	= IDENTIFIER_HASH_VALUE (DECL_NAME (TYPE_NAME (type)));
      hstate.add_object (record_name_hash);
    }

  for (tree t = TYPE_ATTRIBUTES (type); t; t = TREE_CHAIN (t))
    /* Just the identifier is adequate to distinguish.  */
    hstate.add_object (IDENTIFIER_HASH_VALUE (TREE_PURPOSE (t)));

  switch (TREE_CODE (type))
    {
    case METHOD_TYPE:
      hstate.add_object (TYPE_HASH (TYPE_METHOD_BASETYPE (type)));
      /* FALLTHROUGH. */
    case FUNCTION_TYPE:
      for (tree t = TYPE_ARG_TYPES (type); t; t = TREE_CHAIN (t))
	if (TREE_VALUE (t) != error_mark_node)
	  hstate.add_object (TYPE_HASH (TREE_VALUE (t)));
      break;

    case OFFSET_TYPE:
      hstate.add_object (TYPE_HASH (TYPE_OFFSET_BASETYPE (type)));
      break;

      case ARRAY_TYPE: {
	if (TYPE_DOMAIN (type))
	  hstate.add_object (TYPE_HASH (TYPE_DOMAIN (type)));
	if (!AGGREGATE_TYPE_P (TREE_TYPE (type)))
	  {
	    unsigned typeless = TYPE_TYPELESS_STORAGE (type);
	    hstate.add_object (typeless);
	  }
      }
      break;

      case INTEGER_TYPE: {
	tree t = TYPE_MAX_VALUE (type);
	if (!t)
	  t = TYPE_MIN_VALUE (type);
	for (int i = 0; i < TREE_INT_CST_NUNITS (t); i++)
	  hstate.add_object (TREE_INT_CST_ELT (t, i));
	break;
      }

    case REAL_TYPE:
      case FIXED_POINT_TYPE: {
	unsigned prec = TYPE_PRECISION (type);
	hstate.add_object (prec);
	break;
      }

    case VECTOR_TYPE:
      hstate.add_poly_int (TYPE_VECTOR_SUBPARTS (type));
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
      case QUAL_UNION_TYPE: {
	for (tree t = TYPE_FIELDS (type); t; t = TREE_CHAIN (t))
	  {
	    hashval_t name_hash = IDENTIFIER_HASH_VALUE (DECL_NAME (t));
	    hashval_t type_hash = type_hasher (TREE_TYPE (t));
	    hstate.add_object (name_hash);
	    hstate.add_object (type_hash);
	  }
      }
      break;

    case BOOLEAN_TYPE:
      break;

    case REFERENCE_TYPE:
      case POINTER_TYPE: {
	hashval_t type_hash = type_hasher (TREE_TYPE (type));
	hstate.add_object (type_hash);
      }
      break;

    default:
      break;
    }

  return hstate.end ();
}

void
Context::push_closure_context (HirId id)
{
  auto it = closure_bindings.find (id);
  rust_assert (it == closure_bindings.end ());

  closure_bindings.insert ({id, {}});
  closure_scope_bindings.push_back (id);
}

void
Context::pop_closure_context ()
{
  rust_assert (!closure_scope_bindings.empty ());

  HirId ref = closure_scope_bindings.back ();
  closure_scope_bindings.pop_back ();
  closure_bindings.erase (ref);
}

void
Context::insert_closure_binding (HirId id, tree expr)
{
  rust_assert (!closure_scope_bindings.empty ());

  HirId ref = closure_scope_bindings.back ();
  closure_bindings[ref].insert ({id, expr});
}

bool
Context::lookup_closure_binding (HirId id, tree *expr)
{
  if (closure_scope_bindings.empty ())
    return false;

  HirId ref = closure_scope_bindings.back ();
  auto it = closure_bindings.find (ref);
  rust_assert (it != closure_bindings.end ());

  auto iy = it->second.find (id);
  if (iy == it->second.end ())
    return false;

  *expr = iy->second;
  return true;
}

} // namespace Compile
} // namespace Rust
