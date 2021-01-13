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

namespace Rust {
namespace Resolver {

class TypeCheckContext
{
public:
  static TypeCheckContext *get ();

  ~TypeCheckContext ();

  bool lookup_builtin (NodeId id, TyTy::TyBase **type);
  bool lookup_builtin (std::string name, TyTy::TyBase **type);
  void insert_builtin (HirId id, NodeId ref, TyTy::TyBase *type);

  void insert_type (HirId id, TyTy::TyBase *type);
  bool lookup_type (HirId id, TyTy::TyBase **type);

  void insert_type_by_node_id (NodeId ref, HirId id);
  bool lookup_type_by_node_id (NodeId ref, HirId *id);

  TyTy::TyBase *peek_return_type ();
  void push_return_type (TyTy::TyBase *return_type);
  void pop_return_type ();

private:
  TypeCheckContext ();

  std::map<NodeId, HirId> node_id_refs;
  std::map<HirId, TyTy::TyBase *> resolved;
  std::vector<std::unique_ptr<TyTy::TyBase> > builtins;
  std::vector<TyTy::TyBase *> return_type_stack;
};

class TypeResolution
{
public:
  static void Resolve (HIR::Crate &crate);
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK
