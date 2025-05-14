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

#ifndef RUST_BIR_BUILDER_PATTERN_H
#define RUST_BIR_BUILDER_PATTERN_H

#include "rust-bir-builder-internal.h"
#include "rust-bir-free-region.h"
#include "rust-tyty-variance-analysis.h"

namespace Rust {
namespace BIR {

/**
 * Compiles binding of values into newly created variables.
 * Used in let, match arm, and function parameter patterns.
 */
class PatternBindingBuilder : protected AbstractBuilder,
			      public HIR::HIRPatternVisitor
{
  /** Value of initialization expression. */
  tl::optional<PlaceId> init;
  tl::optional<TyTy::BaseType *> type_annotation;
  tl::optional<FreeRegions> regions;

  /** Emulates recursive stack saving and restoring inside a visitor. */
  class SavedState
  {
    PatternBindingBuilder *builder;

  public:
    const tl::optional<PlaceId> init;
    const tl::optional<FreeRegions> regions;

  public:
    explicit SavedState (PatternBindingBuilder *builder)
      : builder (builder), init (builder->init), regions (builder->regions)
    {}

    ~SavedState () { builder->init = init; }
  };

public:
  PatternBindingBuilder (BuilderContext &ctx, tl::optional<PlaceId> init,
			 tl::optional<TyTy::BaseType *> type_annotation)
    : AbstractBuilder (ctx), init (init), type_annotation (type_annotation),
      regions (tl::nullopt)
  {}

  void go (HIR::Pattern &pattern) { pattern.accept_vis (*this); }

  void visit_identifier (const Analysis::NodeMapping &node, bool is_ref,
			 location_t location, bool is_mut = false);

  void visit (HIR::IdentifierPattern &pattern) override;

  void visit (HIR::ReferencePattern &pattern) override;

  void visit (HIR::SlicePattern &pattern) override;

  void visit (HIR::AltPattern &pattern) override;

  void visit (HIR::StructPattern &pattern) override;

  void visit_tuple_fields (std::vector<std::unique_ptr<HIR::Pattern>> &fields,
			   SavedState &saved, size_t &index);

  void visit (HIR::TuplePattern &pattern) override;

  void visit (HIR::TupleStructPattern &pattern) override;
  void visit (HIR::WildcardPattern &pattern) override {}

  // Unused for binding.
  void visit (HIR::LiteralPattern &pattern) override {}
  void visit (HIR::PathInExpression &expression) override {}
  void visit (HIR::QualifiedPathInExpression &expression) override {}
  void visit (HIR::RangePattern &pattern) override {}
};

} // namespace BIR
} // namespace Rust

#endif // RUST_BIR_BUILDER_PATTERN_H
