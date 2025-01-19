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

#include "rust-const-checker.h"
#include "rust-hir.h"
#include "rust-hir-expr.h"
#include "rust-hir-stmt.h"
#include "rust-hir-item.h"

namespace Rust {
namespace HIR {

ConstChecker::ConstChecker ()
  : resolver (*Resolver::Resolver::get ()),
    mappings (*Analysis::Mappings::get ())
{}

void
ConstChecker::go (HIR::Crate &crate)
{
  for (auto &item : crate.get_items ())
    item->accept_vis (*this);
}

bool
ConstChecker::is_const_extern_fn (HIR::ExternalFunctionItem &fn)
{
  // FIXME: Is it really how we want to handle `rustc_const_stable`
  // and `rustc_const_unstable`?
  // TODO: Add these attributes to the attribute check and handle
  // `stable` and `unstable` as well
  return std::any_of (
    fn.get_outer_attrs ().begin (), fn.get_outer_attrs ().end (),
    [] (const AST::Attribute &attr) {
      // `starts_with` in C++11...
      return attr.get_path ().as_string ().rfind ("rustc_const_", 0) == 0;
    });
}

const char *
ConstChecker::ctx_to_str (ConstGenericCtx ctx)
{
  switch (ctx)
    {
    case ConstGenericCtx::Function:
      return "function";
    case ConstGenericCtx::TypeAlias:
      return "type alias";
    case ConstGenericCtx::Struct:
      return "struct";
    case ConstGenericCtx::Enum:
      return "enum";
    case ConstGenericCtx::Union:
      return "union";
    case ConstGenericCtx::Trait:
      return "trait";
    case ConstGenericCtx::Impl:
      return "impl";
    default:
      rust_unreachable ();
    }
}

bool
ConstChecker::ctx_allows_default (ConstGenericCtx ctx)
{
  switch (ctx)
    {
    case ConstGenericCtx::TypeAlias:
    case ConstGenericCtx::Struct:
    case ConstGenericCtx::Enum:
    case ConstGenericCtx::Trait:
      return true;
    default:
      return false;
    }
}

void
ConstChecker::check_default_const_generics (
  std::vector<std::unique_ptr<GenericParam>> &params, ConstGenericCtx context)
{
  if (ctx_allows_default (context))
    return;

  for (auto &param : params)
    {
      if (param->get_kind () == GenericParam::GenericKind::CONST)
	{
	  auto const_param = static_cast<ConstGenericParam *> (param.get ());
	  if (const_param->has_default_expression ())
	    rust_error_at (
	      param->get_locus (),
	      "default values for const generic parameters are not "
	      "allowed in %qs items",
	      ctx_to_str (context));
	}
    }
}

void
ConstChecker::visit (Lifetime &)
{}

void
ConstChecker::visit (LifetimeParam &)
{}

void
ConstChecker::visit (PathInExpression &)
{}

void
ConstChecker::visit (TypePathSegment &)
{}

void
ConstChecker::visit (TypePathSegmentGeneric &)
{}

void
ConstChecker::visit (TypePathSegmentFunction &)
{}

void
ConstChecker::visit (TypePath &)
{}

void
ConstChecker::visit (QualifiedPathInExpression &)
{}

void
ConstChecker::visit (QualifiedPathInType &)
{}

void
ConstChecker::visit (LiteralExpr &)
{}

void
ConstChecker::visit (BorrowExpr &expr)
{
  expr.get_expr ()->accept_vis (*this);
}

void
ConstChecker::visit (DereferenceExpr &expr)
{
  expr.get_expr ()->accept_vis (*this);
}

void
ConstChecker::visit (ErrorPropagationExpr &expr)
{
  expr.get_expr ()->accept_vis (*this);
}

void
ConstChecker::visit (NegationExpr &expr)
{
  expr.get_expr ()->accept_vis (*this);
}

void
ConstChecker::visit (ArithmeticOrLogicalExpr &expr)
{
  expr.get_lhs ()->accept_vis (*this);
  expr.get_rhs ()->accept_vis (*this);
}

void
ConstChecker::visit (ComparisonExpr &expr)
{
  expr.get_lhs ()->accept_vis (*this);
  expr.get_rhs ()->accept_vis (*this);
}

void
ConstChecker::visit (LazyBooleanExpr &expr)
{
  expr.get_lhs ()->accept_vis (*this);
  expr.get_rhs ()->accept_vis (*this);
}

void
ConstChecker::visit (TypeCastExpr &expr)
{
  expr.get_expr ()->accept_vis (*this);
}

void
ConstChecker::visit (AssignmentExpr &expr)
{
  expr.get_lhs ()->accept_vis (*this);
  expr.get_rhs ()->accept_vis (*this);
}

void
ConstChecker::visit (CompoundAssignmentExpr &expr)
{
  expr.get_lhs ()->accept_vis (*this);
  expr.get_rhs ()->accept_vis (*this);
}

void
ConstChecker::visit (GroupedExpr &expr)
{
  expr.get_expr_in_parens ()->accept_vis (*this);
}

void
ConstChecker::visit (ArrayElemsValues &elems)
{
  for (auto &elem : elems.get_values ())
    elem->accept_vis (*this);
}

void
ConstChecker::visit (ArrayElemsCopied &elems)
{
  elems.get_elem_to_copy ()->accept_vis (*this);

  const_context.enter (elems.get_mappings ().get_hirid ());

  elems.get_num_copies_expr ()->accept_vis (*this);

  const_context.exit ();
}

void
ConstChecker::visit (ArrayExpr &expr)
{
  expr.get_internal_elements ()->accept_vis (*this);
}

void
ConstChecker::visit (ArrayIndexExpr &expr)
{
  expr.get_array_expr ()->accept_vis (*this);
  expr.get_index_expr ()->accept_vis (*this);
}

void
ConstChecker::visit (TupleExpr &expr)
{
  for (auto &elem : expr.get_tuple_elems ())
    elem->accept_vis (*this);
}

void
ConstChecker::visit (TupleIndexExpr &expr)
{
  expr.get_tuple_expr ()->accept_vis (*this);
}

void
ConstChecker::visit (StructExprStruct &)
{}

void
ConstChecker::visit (StructExprFieldIdentifier &)
{}

void
ConstChecker::visit (StructExprFieldIdentifierValue &field)
{
  field.get_value ()->accept_vis (*this);
}

void
ConstChecker::visit (StructExprFieldIndexValue &field)
{
  field.get_value ()->accept_vis (*this);
}

void
ConstChecker::visit (StructExprStructFields &expr)
{
  for (auto &field : expr.get_fields ())
    field->accept_vis (*this);
}

void
ConstChecker::visit (StructExprStructBase &)
{}

void
ConstChecker::check_function_call (HirId fn_id, location_t locus)
{
  if (!const_context.is_in_context ())
    return;

  auto maybe_fn = mappings.lookup_hir_item (fn_id);
  if (maybe_fn && maybe_fn->get_item_kind () != Item::ItemKind::Function)
    return;

  // There are const extern functions (intrinsics)
  // TODO: Should we check the ABI is only "rust intrinsics"? Is that handled
  // elsewhere?
  HirId parent_block;
  auto maybe_extern_item
    = mappings.lookup_hir_extern_item (fn_id, &parent_block);
  if (maybe_extern_item
      && maybe_extern_item->get_extern_kind ()
	   != ExternalItem::ExternKind::Function)
    return;

  auto is_error = false;
  if (maybe_fn)
    {
      auto fn = static_cast<Function *> (maybe_fn);
      if (!fn->get_qualifiers ().is_const ())
	is_error = true;
    }

  if (maybe_extern_item)
    {
      {
	auto fn = static_cast<ExternalFunctionItem *> (maybe_extern_item);
	if (!is_const_extern_fn (*fn))
	  is_error = true;
      }
    }

  if (is_error)
    rust_error_at (locus, ErrorCode::E0015,
		   "only functions marked as %<const%> are allowed to be "
		   "called from constant contexts");
}

void
ConstChecker::visit (CallExpr &expr)
{
  if (!expr.get_fnexpr ())
    return;

  NodeId ast_node_id = expr.get_fnexpr ()->get_mappings ().get_nodeid ();
  NodeId ref_node_id;
  HirId definition_id;

  // We don't care about types here
  if (!resolver.lookup_resolved_name (ast_node_id, &ref_node_id))
    return;

  rust_assert (mappings.lookup_node_to_hir (ref_node_id, &definition_id));

  check_function_call (definition_id, expr.get_locus ());

  for (auto &arg : expr.get_arguments ())
    arg->accept_vis (*this);
}

void
ConstChecker::visit (MethodCallExpr &expr)
{
  expr.get_receiver ()->accept_vis (*this);

  for (auto &arg : expr.get_arguments ())
    arg->accept_vis (*this);
}

void
ConstChecker::visit (FieldAccessExpr &expr)
{
  expr.get_receiver_expr ()->accept_vis (*this);
}

void
ConstChecker::visit (ClosureExpr &expr)
{
  expr.get_expr ()->accept_vis (*this);
}

void
ConstChecker::visit (BlockExpr &expr)
{
  for (auto &stmt : expr.get_statements ())
    stmt->accept_vis (*this);

  if (expr.has_expr ())
    expr.get_final_expr ()->accept_vis (*this);
}

void
ConstChecker::visit (ContinueExpr &)
{}

void
ConstChecker::visit (BreakExpr &expr)
{
  if (expr.has_break_expr ())
    expr.get_expr ()->accept_vis (*this);
}

void
ConstChecker::visit (RangeFromToExpr &expr)
{
  expr.get_from_expr ()->accept_vis (*this);
  expr.get_to_expr ()->accept_vis (*this);
}

void
ConstChecker::visit (RangeFromExpr &expr)
{
  expr.get_from_expr ()->accept_vis (*this);
}

void
ConstChecker::visit (RangeToExpr &expr)
{
  expr.get_to_expr ()->accept_vis (*this);
}

void
ConstChecker::visit (RangeFullExpr &)
{}

void
ConstChecker::visit (RangeFromToInclExpr &expr)
{
  expr.get_from_expr ()->accept_vis (*this);
  expr.get_to_expr ()->accept_vis (*this);
}

void
ConstChecker::visit (RangeToInclExpr &)
{
  // FIXME: Visit to_expr
}

void
ConstChecker::visit (ReturnExpr &expr)
{
  if (expr.has_return_expr ())
    expr.get_expr ()->accept_vis (*this);
}

void
ConstChecker::visit (UnsafeBlockExpr &expr)
{
  expr.get_block_expr ()->accept_vis (*this);
}

void
ConstChecker::visit (LoopExpr &expr)
{
  expr.get_loop_block ()->accept_vis (*this);
}

void
ConstChecker::visit (WhileLoopExpr &expr)
{
  expr.get_predicate_expr ()->accept_vis (*this);
  expr.get_loop_block ()->accept_vis (*this);
}

void
ConstChecker::visit (WhileLetLoopExpr &expr)
{
  expr.get_cond ()->accept_vis (*this);
  expr.get_loop_block ()->accept_vis (*this);
}

void
ConstChecker::visit (IfExpr &expr)
{
  expr.get_if_condition ()->accept_vis (*this);
  expr.get_if_block ()->accept_vis (*this);
}

void
ConstChecker::visit (IfExprConseqElse &expr)
{
  expr.get_if_condition ()->accept_vis (*this);
  expr.get_if_block ()->accept_vis (*this);
  expr.get_else_block ()->accept_vis (*this);
}

void
ConstChecker::visit (IfLetExpr &expr)
{
  expr.get_scrutinee_expr ()->accept_vis (*this);
  expr.get_if_block ()->accept_vis (*this);
}

void
ConstChecker::visit (IfLetExprConseqElse &expr)
{
  expr.get_scrutinee_expr ()->accept_vis (*this);
  expr.get_if_block ()->accept_vis (*this);

  // TODO: Visit else expression
}

void
ConstChecker::visit (MatchExpr &expr)
{
  expr.get_scrutinee_expr ()->accept_vis (*this);

  for (auto &match_arm : expr.get_match_cases ())
    match_arm.get_expr ()->accept_vis (*this);
}

void
ConstChecker::visit (AwaitExpr &)
{
  // TODO: Visit expression
}

void
ConstChecker::visit (AsyncBlockExpr &)
{
  // TODO: Visit block expression
}

void
ConstChecker::visit (TypeParam &)
{}

void
ConstChecker::visit (ConstGenericParam &)
{}

void
ConstChecker::visit (LifetimeWhereClauseItem &)
{}

void
ConstChecker::visit (TypeBoundWhereClauseItem &)
{}

void
ConstChecker::visit (Module &module)
{
  for (auto &item : module.get_items ())
    item->accept_vis (*this);
}

void
ConstChecker::visit (ExternCrate &)
{}

void
ConstChecker::visit (UseTreeGlob &)
{}

void
ConstChecker::visit (UseTreeList &)
{}

void
ConstChecker::visit (UseTreeRebind &)
{}

void
ConstChecker::visit (UseDeclaration &)
{}

void
ConstChecker::visit (Function &function)
{
  auto const_fn = function.get_qualifiers ().is_const ();
  if (const_fn)
    const_context.enter (function.get_mappings ().get_hirid ());

  check_default_const_generics (function.get_generic_params (),
				ConstGenericCtx::Function);

  for (auto &param : function.get_function_params ())
    param.get_type ()->accept_vis (*this);

  function.get_definition ()->accept_vis (*this);

  if (const_fn)
    const_context.exit ();
}

void
ConstChecker::visit (TypeAlias &type_alias)
{
  check_default_const_generics (type_alias.get_generic_params (),
				ConstGenericCtx::TypeAlias);
}

void
ConstChecker::visit (StructStruct &struct_item)
{
  check_default_const_generics (struct_item.get_generic_params (),
				ConstGenericCtx::Struct);
}

void
ConstChecker::visit (TupleStruct &tuple_struct)
{
  check_default_const_generics (tuple_struct.get_generic_params (),
				ConstGenericCtx::Struct);
}

void
ConstChecker::visit (EnumItem &)
{}

void
ConstChecker::visit (EnumItemTuple &)
{}

void
ConstChecker::visit (EnumItemStruct &)
{}

void
ConstChecker::visit (EnumItemDiscriminant &item)
{
  const_context.enter (item.get_mappings ().get_hirid ());

  item.get_discriminant_expression ()->accept_vis (*this);

  const_context.exit ();
}

void
ConstChecker::visit (Enum &enum_item)
{
  check_default_const_generics (enum_item.get_generic_params (),
				ConstGenericCtx::Enum);
}

void
ConstChecker::visit (Union &union_item)
{
  check_default_const_generics (union_item.get_generic_params (),
				ConstGenericCtx::Union);
}

void
ConstChecker::visit (ConstantItem &const_item)
{
  const_context.enter (const_item.get_mappings ().get_hirid ());

  const_item.get_expr ()->accept_vis (*this);

  const_context.exit ();
}

void
ConstChecker::visit (StaticItem &static_item)
{
  const_context.enter (static_item.get_mappings ().get_hirid ());

  static_item.get_expr ()->accept_vis (*this);

  const_context.exit ();
}

void
ConstChecker::visit (TraitItemFunc &item)
{
  if (item.has_block_defined ())
    item.get_block_expr ()->accept_vis (*this);
}

void
ConstChecker::visit (TraitItemConst &item)
{
  if (item.has_expr ())
    item.get_expr ()->accept_vis (*this);
}

void
ConstChecker::visit (TraitItemType &)
{}

void
ConstChecker::visit (Trait &trait)
{
  check_default_const_generics (trait.get_generic_params (),
				ConstGenericCtx::Trait);

  for (auto &item : trait.get_trait_items ())
    item->accept_vis (*this);
}

void
ConstChecker::visit (ImplBlock &impl)
{
  check_default_const_generics (impl.get_generic_params (),
				ConstGenericCtx::Impl);

  for (auto &item : impl.get_impl_items ())
    item->accept_vis (*this);
}

void
ConstChecker::visit (ExternalStaticItem &)
{}

void
ConstChecker::visit (ExternalFunctionItem &)
{}

void
ConstChecker::visit (ExternalTypeItem &)
{}

void
ConstChecker::visit (ExternBlock &block)
{
  // FIXME: Do we need to do this?
  for (auto &item : block.get_extern_items ())
    item->accept_vis (*this);
}

void
ConstChecker::visit (LiteralPattern &)
{}

void
ConstChecker::visit (IdentifierPattern &)
{}

void
ConstChecker::visit (WildcardPattern &)
{}

void
ConstChecker::visit (RangePatternBoundLiteral &)
{}

void
ConstChecker::visit (RangePatternBoundPath &)
{}

void
ConstChecker::visit (RangePatternBoundQualPath &)
{}

void
ConstChecker::visit (RangePattern &)
{}

void
ConstChecker::visit (ReferencePattern &)
{}

void
ConstChecker::visit (StructPatternFieldTuplePat &)
{}

void
ConstChecker::visit (StructPatternFieldIdentPat &)
{}

void
ConstChecker::visit (StructPatternFieldIdent &)
{}

void
ConstChecker::visit (StructPattern &)
{}

void
ConstChecker::visit (TupleStructItemsNoRange &)
{}

void
ConstChecker::visit (TupleStructItemsRange &)
{}

void
ConstChecker::visit (TupleStructPattern &)
{}

void
ConstChecker::visit (TuplePatternItemsMultiple &)
{}

void
ConstChecker::visit (TuplePatternItemsRanged &)
{}

void
ConstChecker::visit (TuplePattern &)
{}

void
ConstChecker::visit (SlicePattern &)
{}

void
ConstChecker::visit (AltPattern &)
{}

void
ConstChecker::visit (EmptyStmt &)
{}

void
ConstChecker::visit (LetStmt &stmt)
{
  if (stmt.has_init_expr ())
    stmt.get_init_expr ()->accept_vis (*this);
}

void
ConstChecker::visit (ExprStmt &stmt)
{
  stmt.get_expr ()->accept_vis (*this);
}

void
ConstChecker::visit (TraitBound &)
{}

void
ConstChecker::visit (ImplTraitType &)
{}

void
ConstChecker::visit (TraitObjectType &)
{}

void
ConstChecker::visit (ParenthesisedType &)
{}

void
ConstChecker::visit (ImplTraitTypeOneBound &)
{}

void
ConstChecker::visit (TupleType &)
{}

void
ConstChecker::visit (NeverType &)
{}

void
ConstChecker::visit (RawPointerType &)
{}

void
ConstChecker::visit (ReferenceType &type)
{
  if (const_context.is_in_context () && type.is_mut ())
    rust_error_at (type.get_locus (), ErrorCode::E0658,
		   "mutable references are not allowed in constant functions");
}

void
ConstChecker::visit (ArrayType &type)
{
  const_context.enter (type.get_mappings ().get_hirid ());

  type.get_size_expr ()->accept_vis (*this);

  const_context.exit ();
}

void
ConstChecker::visit (SliceType &)
{}

void
ConstChecker::visit (InferredType &)
{}

void
ConstChecker::visit (BareFunctionType &)
{}

} // namespace HIR
} // namespace Rust
