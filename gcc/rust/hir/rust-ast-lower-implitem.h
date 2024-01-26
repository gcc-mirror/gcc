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

#ifndef RUST_AST_LOWER_IMPLITEM_H
#define RUST_AST_LOWER_IMPLITEM_H

#include "rust-ast-lower-base.h"

namespace Rust {
namespace HIR {

class ASTLowerImplItem : public ASTLoweringBase
{
  using Rust::HIR::ASTLoweringBase::visit;

public:
  static HIR::ImplItem *translate (AST::AssociatedItem *item,
				   HirId parent_impl_id);
  void visit (AST::TypeAlias &alias) override;
  void visit (AST::ConstantItem &constant) override;
  void visit (AST::Function &function) override;

private:
  ASTLowerImplItem () : translated (nullptr), item_cast (nullptr) {}

  HIR::ImplItem *translated;
  HIR::Item *item_cast;
};

class ASTLowerTraitItem : public ASTLoweringBase
{
  using Rust::HIR::ASTLoweringBase::visit;

public:
  static HIR::TraitItem *translate (AST::AssociatedItem *item);
  void visit (AST::Function &func) override;
  void visit (AST::TraitItemConst &constant) override;
  void visit (AST::TraitItemType &type) override;

private:
  ASTLowerTraitItem () : translated (nullptr) {}

  HIR::TraitItem *translated;
};

} // namespace HIR
} // namespace Rust

#endif // RUST_AST_LOWER_IMPLITEM_H
