// Copyright (C) 2020-2026 Free Software Foundation, Inc.

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

#ifndef RUST_FEATURE_GATE_H
#define RUST_FEATURE_GATE_H

#include "rust-ast-visitor.h"
#include "rust-feature.h"

namespace Rust {

class FeatureGate : public AST::DefaultASTVisitor
{
public:
  FeatureGate () {}

  using AST::DefaultASTVisitor::visit;

  void check (AST::Crate &crate);
  void visit (AST::Crate &crate) override;

  void visit (AST::LifetimeParam &lifetime_param) override;
  void visit (AST::ConstGenericParam &const_param) override;
  void visit (AST::BorrowExpr &expr) override;
  void visit (AST::BoxExpr &expr) override;
  void visit (AST::TypeParam &param) override;
  void visit (AST::UseTreeGlob &use_tree) override;
  void visit (AST::Function &function) override;
  void visit (AST::TraitImpl &impl) override;
  void visit (AST::Trait &trait) override;
  void visit (AST::ExternalTypeItem &item) override;
  void visit (AST::ExternBlock &block) override;
  void visit (AST::MacroRulesDefinition &rules_def) override;
  void visit (AST::RangePattern &pattern) override;
  void visit (AST::StructStruct &struct_item) override;
  void visit (AST::TraitItemType &trait_item_type) override;
  void visit (AST::Enum &enum_item) override;
  void visit (AST::EnumItem &enum_variant) override;

private:
  void gate (Feature::Name name, location_t loc, const std::string &error_msg);
  void check_rustc_attri (const std::vector<AST::Attribute> &attributes);
  void
  check_may_dangle_attribute (const std::vector<AST::Attribute> &attributes);
  void
  check_lang_item_attribute (const std::vector<AST::Attribute> &attributes);
  void note_stability_attribute (const std::vector<AST::Attribute> &attributes);

  std::set<Feature::Name> valid_lang_features;
  std::map<std::string, location_t> valid_lib_features;

  enum class Stability
  {
    STABLE,
    UNSTABLE,
  };

  std::map<std::string, Stability> defined_lib_features;
};
} // namespace Rust
#endif
