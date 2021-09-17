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

#ifndef RUST_COMPILE_BASE
#define RUST_COMPILE_BASE

#include "rust-compile-context.h"
#include "rust-hir-visitor.h"
#include "rust-hir-full.h"

namespace Rust {
namespace Compile {

class HIRCompileBase : public HIR::HIRVisitor
{
public:
  virtual ~HIRCompileBase () {}

  // rust-ast.h
  virtual void visit (HIR::IdentifierExpr &ident_expr) {}
  virtual void visit (HIR::Lifetime &lifetime) {}
  virtual void visit (HIR::LifetimeParam &lifetime_param) {}

  // rust-path.h
  virtual void visit (HIR::PathInExpression &path) {}
  virtual void visit (HIR::TypePathSegment &segment) {}
  virtual void visit (HIR::TypePathSegmentGeneric &segment) {}
  virtual void visit (HIR::TypePathSegmentFunction &segment) {}
  virtual void visit (HIR::TypePath &path) {}
  virtual void visit (HIR::QualifiedPathInExpression &path) {}
  virtual void visit (HIR::QualifiedPathInType &path) {}

  // rust-expr.h
  virtual void visit (HIR::LiteralExpr &expr) {}
  virtual void visit (HIR::BorrowExpr &expr) {}
  virtual void visit (HIR::DereferenceExpr &expr) {}
  virtual void visit (HIR::ErrorPropagationExpr &expr) {}
  virtual void visit (HIR::NegationExpr &expr) {}
  virtual void visit (HIR::ArithmeticOrLogicalExpr &expr) {}
  virtual void visit (HIR::ComparisonExpr &expr) {}
  virtual void visit (HIR::LazyBooleanExpr &expr) {}
  virtual void visit (HIR::TypeCastExpr &expr) {}
  virtual void visit (HIR::AssignmentExpr &expr) {}
  virtual void visit (HIR::CompoundAssignmentExpr &expr) {}
  virtual void visit (HIR::GroupedExpr &expr) {}
  // virtual void visit(ArrayElems& elems) {}
  virtual void visit (HIR::ArrayElemsValues &elems) {}
  virtual void visit (HIR::ArrayElemsCopied &elems) {}
  virtual void visit (HIR::ArrayExpr &expr) {}
  virtual void visit (HIR::ArrayIndexExpr &expr) {}
  virtual void visit (HIR::TupleExpr &expr) {}
  virtual void visit (HIR::TupleIndexExpr &expr) {}
  virtual void visit (HIR::StructExprStruct &expr) {}
  // virtual void visit(StructExprField& field) {}
  virtual void visit (HIR::StructExprFieldIdentifier &field) {}
  virtual void visit (HIR::StructExprFieldIdentifierValue &field) {}
  virtual void visit (HIR::StructExprFieldIndexValue &field) {}
  virtual void visit (HIR::StructExprStructFields &expr) {}
  virtual void visit (HIR::StructExprStructBase &expr) {}
  virtual void visit (HIR::CallExpr &expr) {}
  virtual void visit (HIR::MethodCallExpr &expr) {}
  virtual void visit (HIR::FieldAccessExpr &expr) {}
  virtual void visit (HIR::ClosureExprInner &expr) {}
  virtual void visit (HIR::BlockExpr &expr) {}
  virtual void visit (HIR::ClosureExprInnerTyped &expr) {}
  virtual void visit (HIR::ContinueExpr &expr) {}
  virtual void visit (HIR::BreakExpr &expr) {}
  virtual void visit (HIR::RangeFromToExpr &expr) {}
  virtual void visit (HIR::RangeFromExpr &expr) {}
  virtual void visit (HIR::RangeToExpr &expr) {}
  virtual void visit (HIR::RangeFullExpr &expr) {}
  virtual void visit (HIR::RangeFromToInclExpr &expr) {}
  virtual void visit (HIR::RangeToInclExpr &expr) {}
  virtual void visit (HIR::ReturnExpr &expr) {}
  virtual void visit (HIR::UnsafeBlockExpr &expr) {}
  virtual void visit (HIR::LoopExpr &expr) {}
  virtual void visit (HIR::WhileLoopExpr &expr) {}
  virtual void visit (HIR::WhileLetLoopExpr &expr) {}
  virtual void visit (HIR::ForLoopExpr &expr) {}
  virtual void visit (HIR::IfExpr &expr) {}
  virtual void visit (HIR::IfExprConseqElse &expr) {}
  virtual void visit (HIR::IfExprConseqIf &expr) {}
  virtual void visit (HIR::IfExprConseqIfLet &expr) {}
  virtual void visit (HIR::IfLetExpr &expr) {}
  virtual void visit (HIR::IfLetExprConseqElse &expr) {}
  virtual void visit (HIR::IfLetExprConseqIf &expr) {}
  virtual void visit (HIR::IfLetExprConseqIfLet &expr) {}
  // virtual void visit(MatchCase& match_case) {}
  // virtual void visit (HIR::MatchCaseBlockExpr &match_case) {}
  // virtual void visit (HIR::MatchCaseExpr &match_case) {}
  virtual void visit (HIR::MatchExpr &expr) {}
  virtual void visit (HIR::AwaitExpr &expr) {}
  virtual void visit (HIR::AsyncBlockExpr &expr) {}

  // rust-item.h
  virtual void visit (HIR::TypeParam &param) {}
  // virtual void visit(WhereClauseItem& item) {}
  virtual void visit (HIR::LifetimeWhereClauseItem &item) {}
  virtual void visit (HIR::TypeBoundWhereClauseItem &item) {}
  virtual void visit (HIR::Module &module) {}
  virtual void visit (HIR::ExternCrate &crate) {}
  // virtual void visit(UseTree& use_tree) {}
  virtual void visit (HIR::UseTreeGlob &use_tree) {}
  virtual void visit (HIR::UseTreeList &use_tree) {}
  virtual void visit (HIR::UseTreeRebind &use_tree) {}
  virtual void visit (HIR::UseDeclaration &use_decl) {}
  virtual void visit (HIR::Function &function) {}
  virtual void visit (HIR::TypeAlias &type_alias) {}
  virtual void visit (HIR::StructStruct &struct_item) {}
  virtual void visit (HIR::TupleStruct &tuple_struct) {}
  virtual void visit (HIR::EnumItem &item) {}
  virtual void visit (HIR::EnumItemTuple &item) {}
  virtual void visit (HIR::EnumItemStruct &item) {}
  virtual void visit (HIR::EnumItemDiscriminant &item) {}
  virtual void visit (HIR::Enum &enum_item) {}
  virtual void visit (HIR::Union &union_item) {}
  virtual void visit (HIR::ConstantItem &const_item) {}
  virtual void visit (HIR::StaticItem &static_item) {}
  virtual void visit (HIR::TraitItemFunc &item) {}
  virtual void visit (HIR::TraitItemConst &item) {}
  virtual void visit (HIR::TraitItemType &item) {}
  virtual void visit (HIR::Trait &trait) {}
  virtual void visit (HIR::ImplBlock &impl) {}

  virtual void visit (HIR::ExternalStaticItem &item) {}
  virtual void visit (HIR::ExternalFunctionItem &item) {}
  virtual void visit (HIR::ExternBlock &block) {}

  // rust-pattern.h
  virtual void visit (HIR::LiteralPattern &pattern) {}
  virtual void visit (HIR::IdentifierPattern &pattern) {}
  virtual void visit (HIR::WildcardPattern &pattern) {}
  // virtual void visit(RangePatternBound& bound) {}
  virtual void visit (HIR::RangePatternBoundLiteral &bound) {}
  virtual void visit (HIR::RangePatternBoundPath &bound) {}
  virtual void visit (HIR::RangePatternBoundQualPath &bound) {}
  virtual void visit (HIR::RangePattern &pattern) {}
  virtual void visit (HIR::ReferencePattern &pattern) {}
  // virtual void visit(StructPatternField& field) {}
  virtual void visit (HIR::StructPatternFieldTuplePat &field) {}
  virtual void visit (HIR::StructPatternFieldIdentPat &field) {}
  virtual void visit (HIR::StructPatternFieldIdent &field) {}
  virtual void visit (HIR::StructPattern &pattern) {}
  // virtual void visit(TupleStructItems& tuple_items) {}
  virtual void visit (HIR::TupleStructItemsNoRange &tuple_items) {}
  virtual void visit (HIR::TupleStructItemsRange &tuple_items) {}
  virtual void visit (HIR::TupleStructPattern &pattern) {}
  // virtual void visit(TuplePatternItems& tuple_items) {}
  virtual void visit (HIR::TuplePatternItemsMultiple &tuple_items) {}
  virtual void visit (HIR::TuplePatternItemsRanged &tuple_items) {}
  virtual void visit (HIR::TuplePattern &pattern) {}
  virtual void visit (HIR::GroupedPattern &pattern) {}
  virtual void visit (HIR::SlicePattern &pattern) {}

  // rust-stmt.h
  virtual void visit (HIR::EmptyStmt &stmt) {}
  virtual void visit (HIR::LetStmt &stmt) {}
  virtual void visit (HIR::ExprStmtWithoutBlock &stmt) {}
  virtual void visit (HIR::ExprStmtWithBlock &stmt) {}

  // rust-type.h
  virtual void visit (HIR::TraitBound &bound) {}
  virtual void visit (HIR::ImplTraitType &type) {}
  virtual void visit (HIR::TraitObjectType &type) {}
  virtual void visit (HIR::ParenthesisedType &type) {}
  virtual void visit (HIR::ImplTraitTypeOneBound &type) {}
  virtual void visit (HIR::TraitObjectTypeOneBound &type) {}
  virtual void visit (HIR::TupleType &type) {}
  virtual void visit (HIR::NeverType &type) {}
  virtual void visit (HIR::RawPointerType &type) {}
  virtual void visit (HIR::ReferenceType &type) {}
  virtual void visit (HIR::ArrayType &type) {}
  virtual void visit (HIR::SliceType &type) {}
  virtual void visit (HIR::InferredType &type) {}
  virtual void visit (HIR::BareFunctionType &type) {}

protected:
  HIRCompileBase (Context *ctx) : ctx (ctx) {}

  Context *ctx;

  Context *get_context () { return ctx; }

  void compile_function_body (Bfunction *fndecl,
			      std::unique_ptr<HIR::BlockExpr> &function_body,
			      bool has_return_type);

  bool compile_locals_for_block (Resolver::Rib &rib, Bfunction *fndecl,
				 std::vector<Bvariable *> &locals);

  Bexpression *coercion_site (Bexpression *compiled_ref, TyTy::BaseType *actual,
			      TyTy::BaseType *expected, Location locus);

  Bexpression *coerce_to_dyn_object (Bexpression *compiled_ref,
				     TyTy::BaseType *actual,
				     TyTy::BaseType *expected,
				     TyTy::DynamicObjectType *ty,
				     Location locus);

  Bexpression *
  compute_address_for_trait_item (const Resolver::TraitItemReference *ref,
				  TyTy::BaseType *receiver);
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_BASE
