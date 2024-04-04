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

#ifndef RUST_TOPLEVEL_NAME_RESOLVER_2_0_H
#define RUST_TOPLEVEL_NAME_RESOLVER_2_0_H

#include "optional.h"
#include "rust-ast-visitor.h"
#include "rust-ast.h"
#include "rust-item.h"
#include "rust-name-resolution-context.h"
#include "rust-default-resolver.h"

namespace Rust {
namespace Resolver2_0 {

class GlobbingVisitor : public AST::DefaultASTVisitor
{
  using AST::DefaultASTVisitor::visit;

public:
  GlobbingVisitor (NameResolutionContext &ctx) : ctx (ctx) {}

  void go (AST::Module *module);
  void visit (AST::Module &module) override;
  void visit (AST::MacroRulesDefinition &macro) override;
  void visit (AST::Function &function) override;
  void visit (AST::StaticItem &static_item) override;
  void visit (AST::StructStruct &struct_item) override;
  void visit (AST::TupleStruct &tuple_struct) override;
  void visit (AST::Enum &enum_item) override;
  void visit (AST::Union &union_item) override;
  void visit (AST::ConstantItem &const_item) override;
  void visit (AST::ExternCrate &crate) override;
  void visit (AST::UseDeclaration &use) override;

private:
  NameResolutionContext &ctx;
};
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

  // Store node forwarding for use declaration, the link between a
  // definition and its new local name.
  std::unordered_map<NodeId, NodeId> node_forwarding;

  // Each import will be transformed into an instance of `ImportKind`, a class
  // representing some of the data we need to resolve in the
  // `EarlyNameResolver`. Basically, for each `UseTree` that we see in
  // `TopLevel`, create one of these. `TopLevel` should build a list of these
  // `ImportKind`s, which `Early` can then resolve to their proper definitions.
  // Then, a final pass will insert the definitions into the `ForeverStack` -
  // `FinalizeImports`.
  //
  // Using this struct should be very simple - each path within a `UseTree`
  // becomes one `ImportKind`. The complex case is glob imports, in which case
  // one glob import will become one `ImportKind` which will later become
  // multiple definitions thanks to the `GlobbingVisitor`.
  struct ImportKind
  {
    enum class Kind
    {
      Glob,
      Simple,
      Rebind,
    } kind;

    static ImportKind Glob (AST::SimplePath &&to_resolve)
    {
      return ImportKind (Kind::Glob, std::move (to_resolve));
    }

    static ImportKind Simple (AST::SimplePath &&to_resolve)
    {
      return ImportKind (Kind::Simple, std::move (to_resolve));
    }

    static ImportKind Rebind (AST::SimplePath &&to_resolve,
			      AST::UseTreeRebind &&rebind)
    {
      return ImportKind (Kind::Rebind, std::move (to_resolve),
			 std::move (rebind));
    }

    // The path for `Early` to resolve.
    AST::SimplePath to_resolve;

    // The path to rebind an import to - only present if kind is Kind::Rebind
    tl::optional<AST::UseTreeRebind> rebind;

  private:
    ImportKind (Kind kind, AST::SimplePath &&to_resolve,
		tl::optional<AST::UseTreeRebind> &&rebind = tl::nullopt)
      : kind (kind), to_resolve (std::move (to_resolve)),
	rebind (std::move (rebind))
    {}
  };

  // One of the outputs of the `TopLevel` visitor - the list of imports that
  // `Early` should take care of resolving
  std::vector<ImportKind> imports_to_resolve;

  void visit (AST::Module &module) override;
  void visit (AST::Trait &trait) override;
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
  bool handle_use_dec (AST::SimplePath &path);
  bool handle_use_glob (AST::SimplePath &glob);
  bool handle_rebind (std::pair<AST::SimplePath, AST::UseTreeRebind> &pair);

  void visit (AST::UseDeclaration &use) override;
};

} // namespace Resolver2_0
} // namespace Rust

#endif // !RUST_TOPLEVEL_NAME_RESOLVER_2_0_H
