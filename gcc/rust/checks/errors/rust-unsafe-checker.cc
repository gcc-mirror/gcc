// Copyright (C) 2020-2022 Free Software Foundation, Inc.

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

#include "rust-unsafe-checker.h"
#include "rust-hir.h"
#include "rust-hir-expr.h"
#include "rust-hir-stmt.h"
#include "rust-hir-item.h"

namespace Rust {
namespace HIR {

UnsafeChecker::UnsafeChecker () : context (*Resolver::TypeCheckContext::get ())
{}

void
UnsafeChecker::go (HIR::Crate &crate)
{
  for (auto &item : crate.items)
    item->accept_vis (*this);
}

void
UnsafeChecker::push_unsafe (HirId id)
{
  unsafe_contexts.emplace_back (id);
}

HirId
UnsafeChecker::pop_unsafe ()
{
  rust_assert (!unsafe_contexts.empty ());

  auto last = unsafe_contexts.back ();
  unsafe_contexts.pop_back ();

  return last;
}

bool
UnsafeChecker::is_unsafe_context ()
{
  return !unsafe_contexts.empty ();
}

void
UnsafeChecker::visit (IdentifierExpr &ident_expr)
{}

void
UnsafeChecker::visit (Lifetime &lifetime)
{}

void
UnsafeChecker::visit (LifetimeParam &lifetime_param)
{}

void
UnsafeChecker::visit (PathInExpression &path)
{}

void
UnsafeChecker::visit (TypePathSegment &segment)
{}

void
UnsafeChecker::visit (TypePathSegmentGeneric &segment)
{}

void
UnsafeChecker::visit (TypePathSegmentFunction &segment)
{}

void
UnsafeChecker::visit (TypePath &path)
{}

void
UnsafeChecker::visit (QualifiedPathInExpression &path)
{}

void
UnsafeChecker::visit (QualifiedPathInType &path)
{}

void
UnsafeChecker::visit (LiteralExpr &expr)
{}

void
UnsafeChecker::visit (BorrowExpr &expr)
{
  expr.get_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (DereferenceExpr &expr)
{
  TyTy::BaseType *to_deref_type;
  auto to_deref = expr.get_expr ()->get_mappings ().get_hirid ();

  rust_assert (context.lookup_type (to_deref, &to_deref_type));

  if (to_deref_type->get_kind () == TyTy::TypeKind::POINTER
      && !is_unsafe_context ())
    rust_error_at (expr.get_locus (), "dereference of raw pointer requires "
				      "unsafe function or block");
}

void
UnsafeChecker::visit (ErrorPropagationExpr &expr)
{
  expr.get_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (NegationExpr &expr)
{
  expr.get_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (ArithmeticOrLogicalExpr &expr)
{
  expr.get_lhs ()->accept_vis (*this);
  expr.get_rhs ()->accept_vis (*this);
}

void
UnsafeChecker::visit (ComparisonExpr &expr)
{
  expr.get_lhs ()->accept_vis (*this);
  expr.get_rhs ()->accept_vis (*this);
}

void
UnsafeChecker::visit (LazyBooleanExpr &expr)
{
  expr.get_lhs ()->accept_vis (*this);
  expr.get_rhs ()->accept_vis (*this);
}

void
UnsafeChecker::visit (TypeCastExpr &expr)
{
  expr.get_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (AssignmentExpr &expr)
{
  expr.get_lhs ()->accept_vis (*this);
  expr.get_rhs ()->accept_vis (*this);
}

void
UnsafeChecker::visit (CompoundAssignmentExpr &expr)
{
  expr.get_left_expr ()->accept_vis (*this);
  expr.get_right_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (GroupedExpr &expr)
{
  expr.get_expr_in_parens ()->accept_vis (*this);
}

void
UnsafeChecker::visit (ArrayElemsValues &elems)
{
  for (auto &elem : elems.get_values ())
    elem->accept_vis (*this);
}

void
UnsafeChecker::visit (ArrayElemsCopied &elems)
{
  elems.get_elem_to_copy ()->accept_vis (*this);
}

void
UnsafeChecker::visit (ArrayExpr &expr)
{
  expr.get_internal_elements ()->accept_vis (*this);
}

void
UnsafeChecker::visit (ArrayIndexExpr &expr)
{
  expr.get_array_expr ()->accept_vis (*this);
  expr.get_index_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (TupleExpr &expr)
{
  for (auto &elem : expr.get_tuple_elems ())
    elem->accept_vis (*this);
}

void
UnsafeChecker::visit (TupleIndexExpr &expr)
{
  expr.get_tuple_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (StructExprStruct &expr)
{}

void
UnsafeChecker::visit (StructExprFieldIdentifier &field)
{}

void
UnsafeChecker::visit (StructExprFieldIdentifierValue &field)
{
  field.get_value ()->accept_vis (*this);
}

void
UnsafeChecker::visit (StructExprFieldIndexValue &field)
{
  field.get_value ()->accept_vis (*this);
}

void
UnsafeChecker::visit (StructExprStructFields &expr)
{
  for (auto &field : expr.get_fields ())
    field->accept_vis (*this);
}

void
UnsafeChecker::visit (StructExprStructBase &expr)
{}

void
UnsafeChecker::visit (CallExpr &expr)
{}

void
UnsafeChecker::visit (MethodCallExpr &expr)
{}

void
UnsafeChecker::visit (FieldAccessExpr &expr)
{
  // FIXME: If the receiver is an union, we need to be in an unsafe context to
  // access it. Make sure to check.

  expr.get_receiver_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (ClosureExprInner &expr)
{}

void
UnsafeChecker::visit (BlockExpr &expr)
{
  for (auto &stmt : expr.get_statements ())
    stmt->accept_vis (*this);

  if (expr.has_expr ())
    expr.get_final_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (ClosureExprInnerTyped &expr)
{}

void
UnsafeChecker::visit (ContinueExpr &expr)
{}

void
UnsafeChecker::visit (BreakExpr &expr)
{
  if (expr.has_break_expr ())
    expr.get_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (RangeFromToExpr &expr)
{
  expr.get_from_expr ()->accept_vis (*this);
  expr.get_to_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (RangeFromExpr &expr)
{
  expr.get_from_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (RangeToExpr &expr)
{
  expr.get_to_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (RangeFullExpr &expr)
{}

void
UnsafeChecker::visit (RangeFromToInclExpr &expr)
{
  expr.get_from_expr ()->accept_vis (*this);
  expr.get_to_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (RangeToInclExpr &expr)
{
  expr.get_to_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (ReturnExpr &expr)
{
  if (expr.has_return_expr ())
    expr.get_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (UnsafeBlockExpr &expr)
{
  push_unsafe (expr.get_mappings ().get_hirid ());

  expr.get_block_expr ()->accept_vis (*this);

  pop_unsafe ();
}

void
UnsafeChecker::visit (LoopExpr &expr)
{
  expr.get_loop_block ()->accept_vis (*this);
}

void
UnsafeChecker::visit (WhileLoopExpr &expr)
{
  expr.get_predicate_expr ()->accept_vis (*this);
  expr.get_loop_block ()->accept_vis (*this);
}

void
UnsafeChecker::visit (WhileLetLoopExpr &expr)
{
  expr.get_cond ()->accept_vis (*this);
  expr.get_loop_block ()->accept_vis (*this);
}

void
UnsafeChecker::visit (ForLoopExpr &expr)
{
  expr.get_iterator_expr ()->accept_vis (*this);
  expr.get_loop_block ()->accept_vis (*this);
}

void
UnsafeChecker::visit (IfExpr &expr)
{
  expr.get_if_condition ()->accept_vis (*this);
  expr.get_if_block ()->accept_vis (*this);
}

void
UnsafeChecker::visit (IfExprConseqElse &expr)
{
  expr.get_if_condition ()->accept_vis (*this);
  expr.get_if_block ()->accept_vis (*this);
  expr.get_else_block ()->accept_vis (*this);
}

void
UnsafeChecker::visit (IfExprConseqIf &expr)
{
  expr.get_if_condition ()->accept_vis (*this);
  expr.get_if_block ()->accept_vis (*this);
  expr.get_conseq_if_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (IfExprConseqIfLet &expr)
{
  expr.get_if_condition ()->accept_vis (*this);
  expr.get_if_block ()->accept_vis (*this);

  // TODO: Visit conseq if let expression
}

void
UnsafeChecker::visit (IfLetExpr &expr)
{
  expr.get_scrutinee_expr ()->accept_vis (*this);
  expr.get_if_block ()->accept_vis (*this);
}

void
UnsafeChecker::visit (IfLetExprConseqElse &expr)
{
  expr.get_scrutinee_expr ()->accept_vis (*this);
  expr.get_if_block ()->accept_vis (*this);

  // TODO: Visit else expression
}

void
UnsafeChecker::visit (IfLetExprConseqIf &expr)
{
  expr.get_scrutinee_expr ()->accept_vis (*this);
  expr.get_if_block ()->accept_vis (*this);
}

void
UnsafeChecker::visit (IfLetExprConseqIfLet &expr)
{
  expr.get_scrutinee_expr ()->accept_vis (*this);
  expr.get_if_block ()->accept_vis (*this);

  // TODO: Visit conseq if let expression
}

void
UnsafeChecker::visit (MatchExpr &expr)
{
  expr.get_scrutinee_expr ()->accept_vis (*this);

  for (auto &match_arm : expr.get_match_cases ())
    match_arm.get_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (AwaitExpr &expr)
{
  // TODO: Visit expression
}

void
UnsafeChecker::visit (AsyncBlockExpr &expr)
{
  // TODO: Visit block expression
}

void
UnsafeChecker::visit (TypeParam &param)
{}

void
UnsafeChecker::visit (ConstGenericParam &param)
{}

void
UnsafeChecker::visit (LifetimeWhereClauseItem &item)
{}

void
UnsafeChecker::visit (TypeBoundWhereClauseItem &item)
{}

void
UnsafeChecker::visit (Module &module)
{
  for (auto &item : module.get_items ())
    item->accept_vis (*this);
}

void
UnsafeChecker::visit (ExternCrate &crate)
{}

void
UnsafeChecker::visit (UseTreeGlob &use_tree)
{}

void
UnsafeChecker::visit (UseTreeList &use_tree)
{}

void
UnsafeChecker::visit (UseTreeRebind &use_tree)
{}

void
UnsafeChecker::visit (UseDeclaration &use_decl)
{}

void
UnsafeChecker::visit (Function &function)
{
  auto is_unsafe_fn = function.get_qualifiers ().is_unsafe ();

  if (is_unsafe_fn)
    push_unsafe (function.get_mappings ().get_hirid ());

  function.get_definition ()->accept_vis (*this);

  if (is_unsafe_fn)
    pop_unsafe ();
}

void
UnsafeChecker::visit (TypeAlias &type_alias)
{
  // FIXME: What do we need to do to handle type aliasing? Is it possible to
  // have unsafe types? Type aliases on unsafe functions?
}

void
UnsafeChecker::visit (StructStruct &struct_item)
{}

void
UnsafeChecker::visit (TupleStruct &tuple_struct)
{}

void
UnsafeChecker::visit (EnumItem &item)
{}

void
UnsafeChecker::visit (EnumItemTuple &item)
{}

void
UnsafeChecker::visit (EnumItemStruct &item)
{}

void
UnsafeChecker::visit (EnumItemDiscriminant &item)
{}

void
UnsafeChecker::visit (Enum &enum_item)
{}

void
UnsafeChecker::visit (Union &union_item)
{}

void
UnsafeChecker::visit (ConstantItem &const_item)
{
  const_item.get_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (StaticItem &static_item)
{
  static_item.get_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (TraitItemFunc &item)
{
  if (item.has_block_defined ())
    item.get_block_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (TraitItemConst &item)
{
  if (item.has_expr ())
    item.get_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (TraitItemType &item)
{}

void
UnsafeChecker::visit (Trait &trait)
{
  // FIXME: Handle unsafe traits
  for (auto &item : trait.get_trait_items ())
    item->accept_vis (*this);
}

void
UnsafeChecker::visit (ImplBlock &impl)
{
  // FIXME: Handle unsafe impls
  for (auto &item : impl.get_impl_items ())
    item->accept_vis (*this);
}

void
UnsafeChecker::visit (ExternalStaticItem &item)
{}

void
UnsafeChecker::visit (ExternalFunctionItem &item)
{}

void
UnsafeChecker::visit (ExternBlock &block)
{
  // FIXME: Do we need to do this?
  for (auto &item : block.get_extern_items ())
    item->accept_vis (*this);
}

void
UnsafeChecker::visit (LiteralPattern &pattern)
{}

void
UnsafeChecker::visit (IdentifierPattern &pattern)
{}

void
UnsafeChecker::visit (WildcardPattern &pattern)
{}

void
UnsafeChecker::visit (RangePatternBoundLiteral &bound)
{}

void
UnsafeChecker::visit (RangePatternBoundPath &bound)
{}

void
UnsafeChecker::visit (RangePatternBoundQualPath &bound)
{}

void
UnsafeChecker::visit (RangePattern &pattern)
{}

void
UnsafeChecker::visit (ReferencePattern &pattern)
{}

void
UnsafeChecker::visit (StructPatternFieldTuplePat &field)
{}

void
UnsafeChecker::visit (StructPatternFieldIdentPat &field)
{}

void
UnsafeChecker::visit (StructPatternFieldIdent &field)
{}

void
UnsafeChecker::visit (StructPattern &pattern)
{}

void
UnsafeChecker::visit (TupleStructItemsNoRange &tuple_items)
{}

void
UnsafeChecker::visit (TupleStructItemsRange &tuple_items)
{}

void
UnsafeChecker::visit (TupleStructPattern &pattern)
{}

void
UnsafeChecker::visit (TuplePatternItemsMultiple &tuple_items)
{}

void
UnsafeChecker::visit (TuplePatternItemsRanged &tuple_items)
{}

void
UnsafeChecker::visit (TuplePattern &pattern)
{}

void
UnsafeChecker::visit (GroupedPattern &pattern)
{}

void
UnsafeChecker::visit (SlicePattern &pattern)
{}

void
UnsafeChecker::visit (EmptyStmt &stmt)
{}

void
UnsafeChecker::visit (LetStmt &stmt)
{
  if (stmt.has_init_expr ())
    stmt.get_init_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (ExprStmtWithoutBlock &stmt)
{
  stmt.get_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (ExprStmtWithBlock &stmt)
{
  stmt.get_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (TraitBound &bound)
{}

void
UnsafeChecker::visit (ImplTraitType &type)
{}

void
UnsafeChecker::visit (TraitObjectType &type)
{}

void
UnsafeChecker::visit (ParenthesisedType &type)
{}

void
UnsafeChecker::visit (ImplTraitTypeOneBound &type)
{}

void
UnsafeChecker::visit (TupleType &type)
{}

void
UnsafeChecker::visit (NeverType &type)
{}

void
UnsafeChecker::visit (RawPointerType &type)
{}

void
UnsafeChecker::visit (ReferenceType &type)
{}

void
UnsafeChecker::visit (ArrayType &type)
{}

void
UnsafeChecker::visit (SliceType &type)
{}

void
UnsafeChecker::visit (InferredType &type)
{}

void
UnsafeChecker::visit (BareFunctionType &type)
{}

} // namespace HIR
} // namespace Rust
