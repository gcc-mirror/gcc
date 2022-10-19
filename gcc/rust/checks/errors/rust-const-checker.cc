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
  for (auto &item : crate.items)
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
      gcc_unreachable ();
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
ConstChecker::visit (Lifetime &lifetime)
{}

void
ConstChecker::visit (LifetimeParam &lifetime_param)
{}

void
ConstChecker::visit (PathInExpression &path)
{}

void
ConstChecker::visit (TypePathSegment &segment)
{}

void
ConstChecker::visit (TypePathSegmentGeneric &segment)
{}

void
ConstChecker::visit (TypePathSegmentFunction &segment)
{}

void
ConstChecker::visit (TypePath &path)
{}

void
ConstChecker::visit (QualifiedPathInExpression &path)
{}

void
ConstChecker::visit (QualifiedPathInType &path)
{}

void
ConstChecker::visit (LiteralExpr &expr)
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
  expr.get_left_expr ()->accept_vis (*this);
  expr.get_right_expr ()->accept_vis (*this);
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
ConstChecker::visit (StructExprStruct &expr)
{}

void
ConstChecker::visit (StructExprFieldIdentifier &field)
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
ConstChecker::visit (StructExprStructBase &expr)
{}

void
ConstChecker::check_function_call (HirId fn_id, Location locus)
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
    rust_error_at (locus, "only functions marked as %<const%> are allowed to "
			  "be called from constant contexts");
}

void
ConstChecker::visit (CallExpr &expr)
{
  auto fn = expr.get_fnexpr ();
  if (!fn)
    return;

  NodeId ast_node_id = fn->get_mappings ().get_nodeid ();
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
ConstChecker::visit (ContinueExpr &expr)
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
ConstChecker::visit (RangeFullExpr &expr)
{}

void
ConstChecker::visit (RangeFromToInclExpr &expr)
{
  expr.get_from_expr ()->accept_vis (*this);
  expr.get_to_expr ()->accept_vis (*this);
}

void
ConstChecker::visit (RangeToInclExpr &expr)
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
ConstChecker::visit (ForLoopExpr &expr)
{
  expr.get_iterator_expr ()->accept_vis (*this);
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
ConstChecker::visit (IfExprConseqIf &expr)
{
  expr.get_if_condition ()->accept_vis (*this);
  expr.get_if_block ()->accept_vis (*this);
  expr.get_conseq_if_expr ()->accept_vis (*this);
}

void
ConstChecker::visit (IfExprConseqIfLet &expr)
{
  expr.get_if_condition ()->accept_vis (*this);
  expr.get_if_block ()->accept_vis (*this);

  // TODO: Visit conseq if let expression
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
ConstChecker::visit (IfLetExprConseqIf &expr)
{
  expr.get_scrutinee_expr ()->accept_vis (*this);
  expr.get_if_block ()->accept_vis (*this);
}

void
ConstChecker::visit (IfLetExprConseqIfLet &expr)
{
  expr.get_scrutinee_expr ()->accept_vis (*this);
  expr.get_if_block ()->accept_vis (*this);

  // TODO: Visit conseq if let expression
}

void
ConstChecker::visit (MatchExpr &expr)
{
  expr.get_scrutinee_expr ()->accept_vis (*this);

  for (auto &match_arm : expr.get_match_cases ())
    match_arm.get_expr ()->accept_vis (*this);
}

void
ConstChecker::visit (AwaitExpr &expr)
{
  // TODO: Visit expression
}

void
ConstChecker::visit (AsyncBlockExpr &expr)
{
  // TODO: Visit block expression
}

void
ConstChecker::visit (TypeParam &param)
{}

void
ConstChecker::visit (ConstGenericParam &param)
{}

void
ConstChecker::visit (LifetimeWhereClauseItem &item)
{}

void
ConstChecker::visit (TypeBoundWhereClauseItem &item)
{}

void
ConstChecker::visit (Module &module)
{
  for (auto &item : module.get_items ())
    item->accept_vis (*this);
}

void
ConstChecker::visit (ExternCrate &crate)
{}

void
ConstChecker::visit (UseTreeGlob &use_tree)
{}

void
ConstChecker::visit (UseTreeList &use_tree)
{}

void
ConstChecker::visit (UseTreeRebind &use_tree)
{}

void
ConstChecker::visit (UseDeclaration &use_decl)
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
ConstChecker::visit (EnumItem &enum_item)
{}

void
ConstChecker::visit (EnumItemTuple &item)
{}

void
ConstChecker::visit (EnumItemStruct &item)
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
ConstChecker::visit (TraitItemType &item)
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
ConstChecker::visit (ExternalStaticItem &item)
{}

void
ConstChecker::visit (ExternalFunctionItem &item)
{}

void
ConstChecker::visit (ExternBlock &block)
{
  // FIXME: Do we need to do this?
  for (auto &item : block.get_extern_items ())
    item->accept_vis (*this);
}

void
ConstChecker::visit (LiteralPattern &pattern)
{}

void
ConstChecker::visit (IdentifierPattern &pattern)
{}

void
ConstChecker::visit (WildcardPattern &pattern)
{}

void
ConstChecker::visit (RangePatternBoundLiteral &bound)
{}

void
ConstChecker::visit (RangePatternBoundPath &bound)
{}

void
ConstChecker::visit (RangePatternBoundQualPath &bound)
{}

void
ConstChecker::visit (RangePattern &pattern)
{}

void
ConstChecker::visit (ReferencePattern &pattern)
{}

void
ConstChecker::visit (StructPatternFieldTuplePat &field)
{}

void
ConstChecker::visit (StructPatternFieldIdentPat &field)
{}

void
ConstChecker::visit (StructPatternFieldIdent &field)
{}

void
ConstChecker::visit (StructPattern &pattern)
{}

void
ConstChecker::visit (TupleStructItemsNoRange &tuple_items)
{}

void
ConstChecker::visit (TupleStructItemsRange &tuple_items)
{}

void
ConstChecker::visit (TupleStructPattern &pattern)
{}

void
ConstChecker::visit (TuplePatternItemsMultiple &tuple_items)
{}

void
ConstChecker::visit (TuplePatternItemsRanged &tuple_items)
{}

void
ConstChecker::visit (TuplePattern &pattern)
{}

void
ConstChecker::visit (GroupedPattern &pattern)
{}

void
ConstChecker::visit (SlicePattern &pattern)
{}

void
ConstChecker::visit (EmptyStmt &stmt)
{}

void
ConstChecker::visit (LetStmt &stmt)
{
  if (stmt.has_init_expr ())
    stmt.get_init_expr ()->accept_vis (*this);
}

void
ConstChecker::visit (ExprStmtWithoutBlock &stmt)
{
  stmt.get_expr ()->accept_vis (*this);
}

void
ConstChecker::visit (ExprStmtWithBlock &stmt)
{
  stmt.get_expr ()->accept_vis (*this);
}

void
ConstChecker::visit (TraitBound &bound)
{}

void
ConstChecker::visit (ImplTraitType &type)
{}

void
ConstChecker::visit (TraitObjectType &type)
{}

void
ConstChecker::visit (ParenthesisedType &type)
{}

void
ConstChecker::visit (ImplTraitTypeOneBound &type)
{}

void
ConstChecker::visit (TupleType &type)
{}

void
ConstChecker::visit (NeverType &type)
{}

void
ConstChecker::visit (RawPointerType &type)
{}

void
ConstChecker::visit (ReferenceType &type)
{}

void
ConstChecker::visit (ArrayType &type)
{
  const_context.enter (type.get_mappings ().get_hirid ());

  type.get_size_expr ()->accept_vis (*this);

  const_context.exit ();
}

void
ConstChecker::visit (SliceType &type)
{}

void
ConstChecker::visit (InferredType &type)
{}

void
ConstChecker::visit (BareFunctionType &type)
{}

} // namespace HIR
} // namespace Rust
