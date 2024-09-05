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

#include "rust-ast.h"
#include "rust-expr.h"
#include "rust-name-resolution-context.h"
#include "rust-toplevel-name-resolver-2.0.h"
#include "rust-early-name-resolver-2.0.h"

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

// TODO: Fix documentation
// How do we do that?
//
// We want to resolve in the EarlyNameResolver, but we want to declare in the
// TopLevel Should the TopLevel declare stubs? How does rustc do it? How to do
// that for globbing? Should we do globbing afterwards once we've declared all
// the Uses*?
//
// Basically, for each use declare it in a separate map - in the
// EarlyNameResolver resolve and fix the ForeverStack? Emptying the maps each
// time?
//
// e.g. TopLevel builds a std::vector<NodeId, SimplePath> use_trees_to_resolve;
// Early goes through and resolves the SimplePath, then replaces the NodeId with
// the resolved one? Do we even need to do that?
//
// rustc just creates an empty definition for the use tree.
//
// What about globbing? std::vector<GlobbulesPath> globules;
// Early goes through and visits the module's path and calls the
// GlobbingVisitor?
//
// the file `imports.rs` goes through and *finalizes* imports. So we can
// probably add a FinalizeImport pass after the TopLevel and the Early.
// - TopLevel takes care of declaring these use trees
// - Early takes care of resolving them to definition points
// - Finalize takes care of mapping the use's definition point to the actual
// definition point
// 	- We need to work more on that last bit to know exactly what is being
// inserted, but probably it's going to mutate the ForeverStack - is that okay?
// 	- Oh actually maybe no!
// 	- TopLevel creates a map of UseTrees with paths to resolve. This should
// probably be an ImportKind enum or whatever
// 	- Early resolves them, creates a map of SimplePath with the associated
// definition: Map<ImportKind, ImportData>
// 	- Finalizes visits all UseTrees and inserts the Definitions found for
// each ImportKind - easy!
// 	- yay!

class FinalizeImports : DefaultResolver
{
public:
  FinalizeImports (Early::ImportMappings &&data, TopLevel &toplevel,
		   NameResolutionContext &ctx);

  void go (AST::Crate &crate);

private:
  using AST::DefaultASTVisitor::visit;

  void visit (AST::UseDeclaration &) override;

  Early::ImportMappings data;
  TopLevel &toplevel;
  NameResolutionContext &ctx;
};

} // namespace Resolver2_0
} // namespace Rust
