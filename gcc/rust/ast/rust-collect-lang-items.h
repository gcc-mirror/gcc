// Copyright (C) 2024 Free Software Foundation, Inc.

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

#ifndef RUST_COLLECT_LANG_ITEMS_H
#define RUST_COLLECT_LANG_ITEMS_H

#include "rust-ast-visitor.h"
#include "rust-ast.h"
#include "rust-hir-map.h"
#include "rust-item.h"

namespace Rust {
namespace AST {

// This class collects lang items ahead of lowering, as they are now needed for
// some parts of name resolution
class CollectLangItems : public DefaultASTVisitor
{
public:
  CollectLangItems () : mappings (Analysis::Mappings::get ()){};

  void go (AST::Crate &crate) { DefaultASTVisitor::visit (crate); }

  Analysis::Mappings &mappings;

  // We must implement visitors for all constructs that could be lang items.
  // Lang items can be traits, but also enums, and even enum variants.
  //
  // https://github.com/rust-lang/rust/blob/master/compiler/rustc_hir/src/lang_items.rs

  using DefaultASTVisitor::visit;

  void visit (AST::Trait &item) override;
  void visit (AST::TraitItemType &item) override;
  void visit (AST::Function &item) override;
  void visit (AST::StructStruct &item) override;
  void visit (AST::EnumItem &item) override;

private:
  template <typename T> void maybe_add_lang_item (const T &item);
};

} // namespace AST
} // namespace Rust

#endif // ! RUST_COLLECT_LANG_ITEMS_H
