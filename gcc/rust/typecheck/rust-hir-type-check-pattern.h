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

#ifndef RUST_HIR_TYPE_CHECK_PATTERN
#define RUST_HIR_TYPE_CHECK_PATTERN

#include "rust-hir-type-check-base.h"
#include "rust-hir-visitor.h"

namespace Rust {
namespace Resolver {

class TypeCheckPattern : public TypeCheckBase, public HIR::HIRPatternVisitor
{
public:
  static TyTy::BaseType *Resolve (HIR::Pattern &pattern,
				  TyTy::BaseType *parent);

  void visit (HIR::PathInExpression &pattern) override;
  void visit (HIR::StructPattern &pattern) override;
  void visit (HIR::TupleStructPattern &pattern) override;
  void visit (HIR::WildcardPattern &pattern) override;
  void visit (HIR::TuplePattern &pattern) override;
  void visit (HIR::LiteralPattern &pattern) override;
  void visit (HIR::RangePattern &pattern) override;
  void visit (HIR::IdentifierPattern &pattern) override;
  void visit (HIR::QualifiedPathInExpression &pattern) override;
  void visit (HIR::ReferencePattern &pattern) override;
  void visit (HIR::SlicePattern &pattern) override;
  void visit (HIR::AltPattern &pattern) override;

private:
  TypeCheckPattern (TyTy::BaseType *parent);

  TyTy::BaseType *
  typecheck_range_pattern_bound (Rust::HIR::RangePatternBound &bound,
				 Analysis::NodeMapping mappings,
				 location_t locus);

  void emit_pattern_size_error (const HIR::Pattern &pattern,
				size_t expected_field_count,
				size_t got_field_count);

  TyTy::BaseType *parent;
  TyTy::BaseType *infered;
};

class ClosureParamInfer : private TypeCheckBase, private HIR::HIRPatternVisitor
{
public:
  static TyTy::BaseType *Resolve (HIR::Pattern &pattern);

  void visit (HIR::PathInExpression &pattern) override;
  void visit (HIR::StructPattern &pattern) override;
  void visit (HIR::TupleStructPattern &pattern) override;
  void visit (HIR::WildcardPattern &pattern) override;
  void visit (HIR::TuplePattern &pattern) override;
  void visit (HIR::LiteralPattern &pattern) override;
  void visit (HIR::RangePattern &pattern) override;
  void visit (HIR::IdentifierPattern &pattern) override;
  void visit (HIR::QualifiedPathInExpression &pattern) override;
  void visit (HIR::ReferencePattern &pattern) override;
  void visit (HIR::SlicePattern &pattern) override;
  void visit (HIR::AltPattern &pattern) override;

private:
  ClosureParamInfer ();

  TyTy::BaseType *infered;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_PATTERN
