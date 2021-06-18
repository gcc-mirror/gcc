// Copyright (C) 2020 Free Software Foundation, Inc.

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

#ifndef RUST_HIR_TYPE_CHECK
#define RUST_HIR_TYPE_CHECK

#include "rust-hir-full-decls.h"
#include "rust-hir-map.h"
#include "rust-tyty.h"
#include "rust-hir-trait-ref.h"

namespace Rust {
namespace Resolver {

class TypeCheckContext
{
public:
  static TypeCheckContext *get ();

  ~TypeCheckContext ();

  bool lookup_builtin (NodeId id, TyTy::BaseType **type);
  bool lookup_builtin (std::string name, TyTy::BaseType **type);
  void insert_builtin (HirId id, NodeId ref, TyTy::BaseType *type);

  void insert_type (const Analysis::NodeMapping &mappings,
		    TyTy::BaseType *type);
  bool lookup_type (HirId id, TyTy::BaseType **type);

  void insert_type_by_node_id (NodeId ref, HirId id);
  bool lookup_type_by_node_id (NodeId ref, HirId *id);

  TyTy::BaseType *peek_return_type ();
  void push_return_type (TyTy::BaseType *return_type);
  void pop_return_type ();

  void iterate (std::function<bool (HirId, TyTy::BaseType *)> cb)
  {
    for (auto it = resolved.begin (); it != resolved.end (); it++)
      {
	if (!cb (it->first, it->second))
	  return;
      }
  }

  void push_new_loop_context (HirId id)
  {
    TyTy::BaseType *infer_var
      = new TyTy::InferType (id, TyTy::InferType::InferTypeKind::GENERAL);
    loop_type_stack.push_back (infer_var);
  }

  void push_new_while_loop_context (HirId id)
  {
    TyTy::BaseType *infer_var = new TyTy::ErrorType (id);
    loop_type_stack.push_back (infer_var);
  }

  TyTy::BaseType *peek_loop_context () { return loop_type_stack.back (); }

  TyTy::BaseType *pop_loop_context ()
  {
    auto back = peek_loop_context ();
    loop_type_stack.pop_back ();
    return back;
  }

  void swap_head_loop_context (TyTy::BaseType *val)
  {
    loop_type_stack.pop_back ();
    loop_type_stack.push_back (val);
  }

  void insert_trait_reference (DefId id, TraitReference &&ref)
  {
    rust_assert (trait_context.find (id) == trait_context.end ());
    trait_context.emplace (id, std::move (ref));
  }

  bool lookup_trait_reference (DefId id, TraitReference &ref)
  {
    auto it = trait_context.find (id);
    if (it == trait_context.end ())
      return false;

    ref = it->second;
    return true;
  }

private:
  TypeCheckContext ();

  std::map<NodeId, HirId> node_id_refs;
  std::map<HirId, TyTy::BaseType *> resolved;
  std::vector<std::unique_ptr<TyTy::BaseType>> builtins;
  std::vector<TyTy::BaseType *> return_type_stack;
  std::vector<TyTy::BaseType *> loop_type_stack;
  std::map<DefId, TraitReference> trait_context;
};

class TypeResolution
{
public:
  static void Resolve (HIR::Crate &crate);
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK
