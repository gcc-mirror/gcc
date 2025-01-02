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

#include "rust-unsafe-checker.h"
#include "rust-hir.h"
#include "rust-hir-expr.h"
#include "rust-hir-stmt.h"
#include "rust-hir-item.h"
#include "rust-attribute-values.h"

namespace Rust {
namespace HIR {

UnsafeChecker::UnsafeChecker ()
  : context (*Resolver::TypeCheckContext::get ()),
    resolver (*Resolver::Resolver::get ()),
    mappings (*Analysis::Mappings::get ())
{}

void
UnsafeChecker::go (HIR::Crate &crate)
{
  for (auto &item : crate.get_items ())
    item->accept_vis (*this);
}

static void
check_static_mut (HIR::Item *maybe_static, location_t locus)
{
  if (maybe_static->get_hir_kind () == Node::BaseKind::VIS_ITEM)
    {
      auto item = static_cast<Item *> (maybe_static);
      if (item->get_item_kind () == Item::ItemKind::Static)
	{
	  auto static_item = static_cast<StaticItem *> (item);
	  if (static_item->is_mut ())
	    rust_error_at (
	      locus, "use of mutable static requires unsafe function or block");
	}
    }
}

static void
check_extern_static (HIR::ExternalItem *maybe_static, location_t locus)
{
  if (maybe_static->get_extern_kind () == ExternalItem::ExternKind::Static)
    rust_error_at (locus,
		   "use of extern static requires unsafe function or block");
}

void
UnsafeChecker::check_use_of_static (HirId node_id, location_t locus)
{
  if (unsafe_context.is_in_context ())
    return;

  auto maybe_static_mut = mappings.lookup_hir_item (node_id);

  HirId extern_block;
  auto maybe_extern_static
    = mappings.lookup_hir_extern_item (node_id, &extern_block);

  if (maybe_static_mut)
    check_static_mut (maybe_static_mut, locus);

  if (maybe_extern_static)
    check_extern_static (static_cast<ExternalItem *> (maybe_extern_static),
			 locus);
}

static void
check_unsafe_call (HIR::Function *fn, location_t locus, const std::string &kind)
{
  if (fn->get_qualifiers ().is_unsafe ())
    rust_error_at (locus, ErrorCode::E0133,
		   "call to unsafe %s requires unsafe function or block",
		   kind.c_str ());
}

static bool
is_safe_intrinsic (const std::string &fn_name)
{
  static const std::unordered_set<std::string> safe_intrinsics = {
    "abort",
    "size_of",
    "min_align_of",
    "needs_drop",
    "caller_location",
    "add_with_overflow",
    "sub_with_overflow",
    "mul_with_overflow",
    "wrapping_add",
    "wrapping_sub",
    "wrapping_mul",
    "saturating_add",
    "saturating_sub",
    "rotate_left",
    "rotate_right",
    "ctpop",
    "ctlz",
    "cttz",
    "bswap",
    "bitreverse",
    "discriminant_value",
    "type_id",
    "likely",
    "unlikely",
    "ptr_guaranteed_eq",
    "ptr_guaranteed_ne",
    "minnumf32",
    "minnumf64",
    "maxnumf32",
    "rustc_peek",
    "maxnumf64",
    "type_name",
    "forget",
    "black_box",
    "variant_count",
  };

  return safe_intrinsics.find (fn_name) != safe_intrinsics.end ();
}

static void
check_extern_call (HIR::ExternalItem *maybe_fn, HIR::ExternBlock *parent_block,
		   location_t locus)
{
  // We have multiple operations to perform here
  //     1. Is the item an actual function we're calling
  //     2. Is the block it's defined in an FFI block or an `extern crate` block
  //
  // It is not unsafe to call into other crates, so items defined in an `extern
  // crate` must be callable without being in an unsafe context. On the other
  // hand, any function defined in a block with a specific ABI (even `extern
  // "Rust"` blocks) is unsafe to call

  if (maybe_fn->get_extern_kind () != ExternalItem::ExternKind::Function)
    return;

  // Some intrinsics are safe to call
  if (parent_block->get_abi () == Rust::ABI::INTRINSIC
      && is_safe_intrinsic (maybe_fn->get_item_name ().as_string ()))
    return;

  rust_error_at (locus,
		 "call to extern function requires unsafe function or block");
}

void
UnsafeChecker::check_function_call (HirId node_id, location_t locus)
{
  if (unsafe_context.is_in_context ())
    return;

  HirId parent_extern_block;
  auto maybe_fn = mappings.lookup_hir_item (node_id);
  auto maybe_extern
    = mappings.lookup_hir_extern_item (node_id, &parent_extern_block);

  if (maybe_fn && maybe_fn->get_item_kind () == Item::ItemKind::Function)
    check_unsafe_call (static_cast<Function *> (maybe_fn), locus, "function");

  if (maybe_extern)
    check_extern_call (static_cast<ExternalItem *> (maybe_extern),
		       mappings.lookup_hir_extern_block (parent_extern_block),
		       locus);
}

static void
check_target_attr (HIR::Function *fn, location_t locus)
{
  if (std::any_of (fn->get_outer_attrs ().begin (),
		   fn->get_outer_attrs ().end (),
		   [] (const AST::Attribute &attr) {
		     return attr.get_path ().as_string ()
			    == Values::Attributes::TARGET_FEATURE;
		   }))
    rust_error_at (locus,
		   "call to function with %<#[target_feature]%> requires "
		   "unsafe function or block");
}

void
UnsafeChecker::check_function_attr (HirId node_id, location_t locus)
{
  if (unsafe_context.is_in_context ())
    return;

  auto maybe_fn = mappings.lookup_hir_item (node_id);

  if (maybe_fn && maybe_fn->get_item_kind () == Item::ItemKind::Function)
    check_target_attr (static_cast<Function *> (maybe_fn), locus);
}

void
UnsafeChecker::visit (Lifetime &)
{}

void
UnsafeChecker::visit (LifetimeParam &)
{}

void
UnsafeChecker::visit (PathInExpression &path)
{
  NodeId ast_node_id = path.get_mappings ().get_nodeid ();
  NodeId ref_node_id;
  HirId definition_id;

  if (!resolver.lookup_resolved_name (ast_node_id, &ref_node_id))
    return;

  rust_assert (mappings.lookup_node_to_hir (ref_node_id, &definition_id));

  check_use_of_static (definition_id, path.get_locus ());
}

void
UnsafeChecker::visit (TypePathSegment &)
{}

void
UnsafeChecker::visit (TypePathSegmentGeneric &)
{}

void
UnsafeChecker::visit (TypePathSegmentFunction &)
{}

void
UnsafeChecker::visit (TypePath &)
{}

void
UnsafeChecker::visit (QualifiedPathInExpression &)
{}

void
UnsafeChecker::visit (QualifiedPathInType &)
{}

void
UnsafeChecker::visit (LiteralExpr &)
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
      && !unsafe_context.is_in_context ())
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
  expr.get_lhs ()->accept_vis (*this);
  expr.get_rhs ()->accept_vis (*this);
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
UnsafeChecker::visit (StructExprStruct &)
{}

void
UnsafeChecker::visit (StructExprFieldIdentifier &)
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
UnsafeChecker::visit (StructExprStructBase &)
{}

void
UnsafeChecker::visit (CallExpr &expr)
{
  if (!expr.get_fnexpr ())
    return;

  NodeId ast_node_id = expr.get_fnexpr ()->get_mappings ().get_nodeid ();
  NodeId ref_node_id;
  HirId definition_id;

  // There are no unsafe types, and functions are defined in the name resolver.
  // If we can't find the name, then we're dealing with a type and should return
  // early.
  if (!resolver.lookup_resolved_name (ast_node_id, &ref_node_id))
    return;

  rust_assert (mappings.lookup_node_to_hir (ref_node_id, &definition_id));

  // At this point we have the function's HIR Id. There are three checks we
  // must perform:
  //     1. The function is an unsafe one
  //     2. The function is an extern one
  //     3. The function is marked with a target_feature attribute
  check_function_call (definition_id, expr.get_locus ());
  check_function_attr (definition_id, expr.get_locus ());

  if (expr.has_params ())
    for (auto &arg : expr.get_arguments ())
      arg->accept_vis (*this);
}

void
UnsafeChecker::visit (MethodCallExpr &expr)
{
  TyTy::BaseType *method_type;
  context.lookup_type (expr.get_method_name ().get_mappings ().get_hirid (),
		       &method_type);

  auto fn = *static_cast<TyTy::FnType *> (method_type);
  auto method = mappings.lookup_hir_implitem (fn.get_ref (), nullptr);

  if (!unsafe_context.is_in_context () && method)
    check_unsafe_call (static_cast<Function *> (method), expr.get_locus (),
		       "method");

  expr.get_receiver ()->accept_vis (*this);

  for (auto &arg : expr.get_arguments ())
    arg->accept_vis (*this);
}

void
UnsafeChecker::visit (FieldAccessExpr &expr)
{
  expr.get_receiver_expr ()->accept_vis (*this);

  if (unsafe_context.is_in_context ())
    return;

  TyTy::BaseType *receiver_ty;
  auto ok = context.lookup_type (
    expr.get_receiver_expr ()->get_mappings ().get_hirid (), &receiver_ty);
  rust_assert (ok);

  if (receiver_ty->get_kind () == TyTy::TypeKind::ADT)
    {
      auto maybe_union = static_cast<TyTy::ADTType *> (receiver_ty);
      if (maybe_union->is_union ())
	rust_error_at (
	  expr.get_locus (),
	  "access to union field requires unsafe function or block");
    }
}

void
UnsafeChecker::visit (ClosureExpr &expr)
{
  expr.get_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (BlockExpr &expr)
{
  for (auto &stmt : expr.get_statements ())
    stmt->accept_vis (*this);

  if (expr.has_expr ())
    expr.get_final_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (ContinueExpr &)
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
UnsafeChecker::visit (RangeFullExpr &)
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
  unsafe_context.enter (expr.get_mappings ().get_hirid ());

  expr.get_block_expr ()->accept_vis (*this);

  unsafe_context.exit ();
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
UnsafeChecker::visit (MatchExpr &expr)
{
  expr.get_scrutinee_expr ()->accept_vis (*this);

  for (auto &match_arm : expr.get_match_cases ())
    match_arm.get_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (AwaitExpr &)
{
  // TODO: Visit expression
}

void
UnsafeChecker::visit (AsyncBlockExpr &)
{
  // TODO: Visit block expression
}

void
UnsafeChecker::visit (TypeParam &)
{}

void
UnsafeChecker::visit (ConstGenericParam &)
{}

void
UnsafeChecker::visit (LifetimeWhereClauseItem &)
{}

void
UnsafeChecker::visit (TypeBoundWhereClauseItem &)
{}

void
UnsafeChecker::visit (Module &module)
{
  for (auto &item : module.get_items ())
    item->accept_vis (*this);
}

void
UnsafeChecker::visit (ExternCrate &)
{}

void
UnsafeChecker::visit (UseTreeGlob &)
{}

void
UnsafeChecker::visit (UseTreeList &)
{}

void
UnsafeChecker::visit (UseTreeRebind &)
{}

void
UnsafeChecker::visit (UseDeclaration &)
{}

void
UnsafeChecker::visit (Function &function)
{
  auto is_unsafe_fn = function.get_qualifiers ().is_unsafe ();

  if (is_unsafe_fn)
    unsafe_context.enter (function.get_mappings ().get_hirid ());

  function.get_definition ()->accept_vis (*this);

  if (is_unsafe_fn)
    unsafe_context.exit ();
}

void
UnsafeChecker::visit (TypeAlias &)
{
  // FIXME: What do we need to do to handle type aliasing? Is it possible to
  // have unsafe types? Type aliases on unsafe functions?
}

void
UnsafeChecker::visit (StructStruct &)
{}

void
UnsafeChecker::visit (TupleStruct &)
{}

void
UnsafeChecker::visit (EnumItem &)
{}

void
UnsafeChecker::visit (EnumItemTuple &)
{}

void
UnsafeChecker::visit (EnumItemStruct &)
{}

void
UnsafeChecker::visit (EnumItemDiscriminant &)
{}

void
UnsafeChecker::visit (Enum &)
{}

void
UnsafeChecker::visit (Union &)
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
UnsafeChecker::visit (TraitItemType &)
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
UnsafeChecker::visit (ExternalStaticItem &)
{}

void
UnsafeChecker::visit (ExternalFunctionItem &)
{}

void
UnsafeChecker::visit (ExternalTypeItem &)
{}

void
UnsafeChecker::visit (ExternBlock &block)
{
  // FIXME: Do we need to do this?
  for (auto &item : block.get_extern_items ())
    item->accept_vis (*this);
}

void
UnsafeChecker::visit (LiteralPattern &)
{}

void
UnsafeChecker::visit (IdentifierPattern &)
{}

void
UnsafeChecker::visit (WildcardPattern &)
{}

void
UnsafeChecker::visit (RangePatternBoundLiteral &)
{}

void
UnsafeChecker::visit (RangePatternBoundPath &)
{}

void
UnsafeChecker::visit (RangePatternBoundQualPath &)
{}

void
UnsafeChecker::visit (RangePattern &)
{}

void
UnsafeChecker::visit (ReferencePattern &)
{}

void
UnsafeChecker::visit (StructPatternFieldTuplePat &)
{}

void
UnsafeChecker::visit (StructPatternFieldIdentPat &)
{}

void
UnsafeChecker::visit (StructPatternFieldIdent &)
{}

void
UnsafeChecker::visit (StructPattern &)
{}

void
UnsafeChecker::visit (TupleStructItemsNoRange &)
{}

void
UnsafeChecker::visit (TupleStructItemsRange &)
{}

void
UnsafeChecker::visit (TupleStructPattern &)
{}

void
UnsafeChecker::visit (TuplePatternItemsMultiple &)
{}

void
UnsafeChecker::visit (TuplePatternItemsRanged &)
{}

void
UnsafeChecker::visit (TuplePattern &)
{}

void
UnsafeChecker::visit (SlicePattern &)
{}

void
UnsafeChecker::visit (AltPattern &)
{}

void
UnsafeChecker::visit (EmptyStmt &)
{}

void
UnsafeChecker::visit (LetStmt &stmt)
{
  if (stmt.has_init_expr ())
    stmt.get_init_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (ExprStmt &stmt)
{
  stmt.get_expr ()->accept_vis (*this);
}

void
UnsafeChecker::visit (TraitBound &)
{}

void
UnsafeChecker::visit (ImplTraitType &)
{}

void
UnsafeChecker::visit (TraitObjectType &)
{}

void
UnsafeChecker::visit (ParenthesisedType &)
{}

void
UnsafeChecker::visit (ImplTraitTypeOneBound &)
{}

void
UnsafeChecker::visit (TupleType &)
{}

void
UnsafeChecker::visit (NeverType &)
{}

void
UnsafeChecker::visit (RawPointerType &)
{}

void
UnsafeChecker::visit (ReferenceType &)
{}

void
UnsafeChecker::visit (ArrayType &)
{}

void
UnsafeChecker::visit (SliceType &)
{}

void
UnsafeChecker::visit (InferredType &)
{}

void
UnsafeChecker::visit (BareFunctionType &)
{}

} // namespace HIR
} // namespace Rust
