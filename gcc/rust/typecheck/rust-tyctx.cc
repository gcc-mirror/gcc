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

#include "rust-hir-type-check.h"

namespace Rust {
namespace Resolver {

TypeCheckContext *
TypeCheckContext::get ()
{
  static TypeCheckContext *instance;
  if (instance == nullptr)
    instance = new TypeCheckContext ();

  return instance;
}

TypeCheckContext::TypeCheckContext () {}

TypeCheckContext::~TypeCheckContext () {}

bool
TypeCheckContext::lookup_builtin (NodeId id, TyTy::BaseType **type)
{
  auto ref_it = node_id_refs.find (id);
  if (ref_it == node_id_refs.end ())
    return false;

  auto it = resolved.find (ref_it->second);
  if (it == resolved.end ())
    return false;

  *type = it->second;
  return true;
}

bool
TypeCheckContext::lookup_builtin (std::string name, TyTy::BaseType **type)
{
  for (auto &builtin : builtins)
    {
      if (name.compare (builtin->as_string ()) == 0)
	{
	  *type = builtin.get ();
	  return true;
	}
    }
  return false;
}

void
TypeCheckContext::insert_builtin (HirId id, NodeId ref, TyTy::BaseType *type)
{
  node_id_refs[ref] = id;
  resolved[id] = type;
  builtins.push_back (std::unique_ptr<TyTy::BaseType> (type));
}

void
TypeCheckContext::insert_type (const Analysis::NodeMapping &mappings,
			       TyTy::BaseType *type)
{
  rust_assert (type != nullptr);
  NodeId ref = mappings.get_nodeid ();
  HirId id = mappings.get_hirid ();
  node_id_refs[ref] = id;
  resolved[id] = type;
}

void
TypeCheckContext::insert_implicit_type (TyTy::BaseType *type)
{
  rust_assert (type != nullptr);
  resolved[type->get_ref ()] = type;
}

void
TypeCheckContext::insert_implicit_type (HirId id, TyTy::BaseType *type)
{
  rust_assert (type != nullptr);
  resolved[id] = type;
}

bool
TypeCheckContext::lookup_type (HirId id, TyTy::BaseType **type)
{
  auto it = resolved.find (id);
  if (it == resolved.end ())
    return false;

  *type = it->second;
  return true;
}

void
TypeCheckContext::insert_type_by_node_id (NodeId ref, HirId id)
{
  rust_assert (node_id_refs.find (ref) == node_id_refs.end ());
  node_id_refs[ref] = id;
}

bool
TypeCheckContext::lookup_type_by_node_id (NodeId ref, HirId *id)
{
  auto it = node_id_refs.find (ref);
  if (it == node_id_refs.end ())
    return false;

  *id = it->second;
  return true;
}

TyTy::BaseType *
TypeCheckContext::peek_return_type ()
{
  return return_type_stack.back ().second;
}

void
TypeCheckContext::push_return_type (TypeCheckContextItem item,
				    TyTy::BaseType *return_type)
{
  return_type_stack.push_back ({std::move (item), return_type});
}

void
TypeCheckContext::pop_return_type ()
{
  return_type_stack.pop_back ();
}

TypeCheckContextItem &
TypeCheckContext::peek_context ()
{
  return return_type_stack.back ().first;
}

} // namespace Resolver
} // namespace Rust
