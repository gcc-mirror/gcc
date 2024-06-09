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

#ifndef RUST_TOPLEVEL_NAME_RESOLVER_2_0_H
#define RUST_TOPLEVEL_NAME_RESOLVER_2_0_H

#include "rust-ast-visitor.h"
#include "rust-name-resolution-context.h"
#include "rust-default-resolver.h"

namespace Rust {
namespace Resolver2_0 {

/**
 * The `TopLevel` visitor takes care of collecting all the definitions in a
 * crate, and inserting them into the proper namespaces. These definitions can
 * then be accessed by subsequent resolvers, such as `Early` or `Late`.
 */
class TopLevel : public DefaultResolver
{
  using DefaultResolver::visit;

public:
  TopLevel (NameResolutionContext &resolver);

  void go (AST::Crate &crate);

private:
  /**
   * Insert a new definition or error out if a definition with the same name was
   * already present in the same namespace in the same scope.
   *
   * @param identifier The identifier of the definition to add.
   * @param node A reference to the node, so we can get its `NodeId` and
   * location.
   * @param ns The namespace in which to add the definition.
   */
  template <typename T>
  void insert_or_error_out (const Identifier &identifier, const T &node,
			    Namespace ns);
  void insert_or_error_out (const Identifier &identifier,
			    const location_t &locus, const NodeId &id,
			    Namespace ns);

  // FIXME: Do we move these to our mappings?
  std::unordered_map<NodeId, location_t> node_locations;

  void visit (AST::Module &module) override;
  void visit (AST::MacroRulesDefinition &macro) override;
  void visit (AST::Function &function) override;
  void visit (AST::BlockExpr &expr) override;
  void visit (AST::StaticItem &static_item) override;
  void visit (AST::StructStruct &struct_item) override;
  void visit (AST::TupleStruct &tuple_struct) override;
  void visit (AST::EnumItem &variant) override;
  void visit (AST::EnumItemTuple &variant) override;
  void visit (AST::EnumItemStruct &variant) override;
  void visit (AST::EnumItemDiscriminant &variant) override;
  void visit (AST::Enum &enum_item) override;
  void visit (AST::Union &union_item) override;
  void visit (AST::ConstantItem &const_item) override;
  void visit (AST::ExternCrate &crate) override;

  // FIXME: Documentation
  // Call this on all the paths of a UseDec - so each flattened path in a
  // UseTreeList for example
  // FIXME: Should that return `found`?
  bool handle_use_dec (AST::SimplePath path);

  void visit (AST::UseDeclaration &use) override;
};

} // namespace Resolver2_0
} // namespace Rust

#endif // !RUST_TOPLEVEL_NAME_RESOLVER_2_0_H
