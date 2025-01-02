// Copyright (C) 2020-2025 Free Software Foundation, Inc.

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

#ifndef RUST_VISIBILITY_H
#define RUST_VISIBILITY_H

#include "rust-hir.h"
#include "rust-hir-expr.h"
#include "rust-hir-stmt.h"
#include "rust-hir-item.h"
#include "rust-hir-map.h"
#include "rust-name-resolver.h"
#include "rust-hir-visitor.h"

namespace Rust {
namespace Privacy {

class VisibilityResolver : public HIR::HIRVisItemVisitor
{
public:
  VisibilityResolver (Analysis::Mappings &mappings,
		      Rust::Resolver::Resolver &resolver);

  /**
   * Perform visibility resolving on an entire crate
   */
  void go (HIR::Crate &crate);

  /**
   * Resolve a path to the module it refers
   */
  bool resolve_module_path (const HIR::SimplePath &restriction,
			    DefId &to_resolve);

  /**
   * Resolve the visibility of an item to its ModuleVisibility. This function
   * emits errors if necessary. The contents of the to_resolve parameter will be
   * overwritten on success.
   *
   * @param visibility Visibility of the item to resolve
   * @param to_resolve ModuleVisibility reference to fill on success.
   *
   * @return false on error, true if the resolving was successful.
   */
  bool resolve_visibility (const HIR::Visibility &visibility,
			   ModuleVisibility &to_resolve);

  /**
   * Resolve the visibility of an item and updates it. This is useful for
   * vis-items who need to be resolved but do not care about their module
   * visibility - const items, static items, etc. For items with an impact on
   * their children (enums, traits), this cannot be used
   */
  void resolve_and_update (const HIR::VisItem *item);

  /**
   * Get the DefId of the parent module we are currently visiting.
   *
   * @return UNKNOWN_DEFID if the module stack is empty, a valid `DefId`
   * otherwise
   */
  DefId peek_module ();

  virtual void visit (HIR::Module &mod);
  virtual void visit (HIR::ExternCrate &crate);
  virtual void visit (HIR::UseDeclaration &use_decl);
  virtual void visit (HIR::Function &func);
  virtual void visit (HIR::TypeAlias &type_alias);
  virtual void visit (HIR::StructStruct &struct_item);
  virtual void visit (HIR::TupleStruct &tuple_struct);
  virtual void visit (HIR::Enum &enum_item);
  virtual void visit (HIR::Union &union_item);
  virtual void visit (HIR::ConstantItem &const_item);
  virtual void visit (HIR::StaticItem &static_item);
  virtual void visit (HIR::Trait &trait);
  virtual void visit (HIR::ImplBlock &impl);
  virtual void visit (HIR::ExternBlock &block);

private:
  Analysis::Mappings &mappings;
  Rust::Resolver::Resolver &resolver;
  DefId current_module;
};

} // namespace Privacy
} // namespace Rust

#endif // !RUST_VISIBILITY_H
