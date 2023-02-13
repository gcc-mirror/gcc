// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

#include "rust-hir-full.h"
#include "rust-tyty-call.h"
#include "rust-hir-type-check-struct-field.h"
#include "rust-hir-path-probe.h"
#include "rust-substitution-mapper.h"
#include "rust-hir-trait-resolve.h"
#include "rust-hir-type-bounds.h"
#include "rust-hir-dot-operator.h"
#include "rust-hir-type-check-pattern.h"
#include "rust-hir-type-check-expr.h"
#include "rust-hir-type-check-stmt.h"

namespace Rust {
namespace Resolver {

TypeCheckExpr::TypeCheckExpr () : TypeCheckBase (), infered (nullptr) {}

// Perform type checking on expr. Also runs type unification algorithm.
// Returns the unified type of expr
TyTy::BaseType *
TypeCheckExpr::Resolve (HIR::Expr *expr)
{
  TypeCheckExpr resolver;
  expr->accept_vis (resolver);

  if (resolver.infered == nullptr)
    {
      // FIXME
      // this is an internal error message for debugging and should be removed
      // at some point
      rust_error_at (expr->get_locus (), "failed to type resolve expression");
      return new TyTy::ErrorType (expr->get_mappings ().get_hirid ());
    }

  auto ref = expr->get_mappings ().get_hirid ();
  resolver.infered->set_ref (ref);
  resolver.context->insert_type (expr->get_mappings (), resolver.infered);

  return resolver.infered;
}

void
TypeCheckExpr::visit (HIR::TupleIndexExpr &expr)
{
  auto resolved = TypeCheckExpr::Resolve (expr.get_tuple_expr ().get ());
  if (resolved->get_kind () == TyTy::TypeKind::ERROR)
    {
      rust_error_at (expr.get_tuple_expr ()->get_locus (),
		     "failed to resolve TupleIndexExpr receiver");
      return;
    }

  // FIXME does this require autoderef here?
  if (resolved->get_kind () == TyTy::TypeKind::REF)
    {
      TyTy::ReferenceType *r = static_cast<TyTy::ReferenceType *> (resolved);
      resolved = r->get_base ();
    }

  bool is_valid_type = resolved->get_kind () == TyTy::TypeKind::ADT
		       || resolved->get_kind () == TyTy::TypeKind::TUPLE;
  if (!is_valid_type)
    {
      rust_error_at (expr.get_tuple_expr ()->get_locus (),
		     "Expected Tuple or ADT got: %s",
		     resolved->as_string ().c_str ());
      return;
    }

  if (resolved->get_kind () == TyTy::TypeKind::TUPLE)
    {
      TyTy::TupleType *tuple = static_cast<TyTy::TupleType *> (resolved);
      TupleIndex index = expr.get_tuple_index ();
      if ((size_t) index >= tuple->num_fields ())
	{
	  rust_error_at (expr.get_locus (), "unknown field at index %i", index);
	  return;
	}

      auto field_tyty = tuple->get_field ((size_t) index);
      if (field_tyty == nullptr)
	{
	  rust_error_at (expr.get_locus (),
			 "failed to lookup field type at index %i", index);
	  return;
	}

      infered = field_tyty;
      return;
    }

  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (resolved);
  rust_assert (!adt->is_enum ());
  rust_assert (adt->number_of_variants () == 1);

  TyTy::VariantDef *variant = adt->get_variants ().at (0);
  TupleIndex index = expr.get_tuple_index ();
  if ((size_t) index >= variant->num_fields ())
    {
      rust_error_at (expr.get_locus (), "unknown field at index %i", index);
      return;
    }

  auto field_tyty = variant->get_field_at_index ((size_t) index);
  if (field_tyty == nullptr)
    {
      rust_error_at (expr.get_locus (),
		     "failed to lookup field type at index %i", index);
      return;
    }

  infered = field_tyty->get_field_type ();
}

void
TypeCheckExpr::visit (HIR::TupleExpr &expr)
{
  if (expr.is_unit ())
    {
      auto unit_node_id = resolver->get_unit_type_node_id ();
      if (!context->lookup_builtin (unit_node_id, &infered))
	{
	  rust_error_at (expr.get_locus (),
			 "failed to lookup builtin unit type");
	}
      return;
    }

  std::vector<TyTy::TyVar> fields;
  for (auto &elem : expr.get_tuple_elems ())
    {
      auto field_ty = TypeCheckExpr::Resolve (elem.get ());
      fields.push_back (TyTy::TyVar (field_ty->get_ref ()));
    }
  infered = new TyTy::TupleType (expr.get_mappings ().get_hirid (),
				 expr.get_locus (), fields);
}

void
TypeCheckExpr::visit (HIR::ReturnExpr &expr)
{
  auto fn_return_tyty = context->peek_return_type ();
  Location expr_locus = expr.has_return_expr () ? expr.get_expr ()->get_locus ()
						: expr.get_locus ();
  TyTy::BaseType *expr_ty
    = expr.has_return_expr ()
	? TypeCheckExpr::Resolve (expr.get_expr ())
	: TyTy::TupleType::get_unit_type (expr.get_mappings ().get_hirid ());

  infered = unify_site (expr.get_mappings ().get_hirid (),
			TyTy::TyWithLocation (fn_return_tyty),
			TyTy::TyWithLocation (expr_ty, expr_locus),
			expr.get_locus ());

  infered = new TyTy::NeverType (expr.get_mappings ().get_hirid ());
}

void
TypeCheckExpr::visit (HIR::CallExpr &expr)
{
  TyTy::BaseType *function_tyty = TypeCheckExpr::Resolve (expr.get_fnexpr ());

  rust_debug_loc (expr.get_locus (), "resolved_call_expr to: {%s}",
		  function_tyty->get_name ().c_str ());

  TyTy::VariantDef &variant = TyTy::VariantDef::get_error_node ();
  if (function_tyty->get_kind () == TyTy::TypeKind::ADT)
    {
      TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (function_tyty);
      if (adt->is_enum ())
	{
	  // lookup variant id
	  HirId variant_id;
	  bool ok = context->lookup_variant_definition (
	    expr.get_fnexpr ()->get_mappings ().get_hirid (), &variant_id);
	  rust_assert (ok);

	  TyTy::VariantDef *lookup_variant = nullptr;
	  ok = adt->lookup_variant_by_id (variant_id, &lookup_variant);
	  rust_assert (ok);

	  variant = *lookup_variant;
	}
      else
	{
	  rust_assert (adt->number_of_variants () == 1);
	  variant = *adt->get_variants ().at (0);
	}

      infered
	= TyTy::TypeCheckCallExpr::go (function_tyty, expr, variant, context);
      return;
    }

  bool resolved_fn_trait_call
    = resolve_fn_trait_call (expr, function_tyty, &infered);
  if (resolved_fn_trait_call)
    return;

  bool valid_tyty = function_tyty->get_kind () == TyTy::TypeKind::FNDEF
		    || function_tyty->get_kind () == TyTy::TypeKind::FNPTR;
  if (!valid_tyty)
    {
      rust_error_at (expr.get_locus (),
		     "Failed to resolve expression of function call");
      return;
    }

  infered = TyTy::TypeCheckCallExpr::go (function_tyty, expr, variant, context);
}

void
TypeCheckExpr::visit (HIR::AssignmentExpr &expr)
{
  infered = TyTy::TupleType::get_unit_type (expr.get_mappings ().get_hirid ());

  auto lhs = TypeCheckExpr::Resolve (expr.get_lhs ());
  auto rhs = TypeCheckExpr::Resolve (expr.get_rhs ());

  coercion_site (expr.get_mappings ().get_hirid (),
		 TyTy::TyWithLocation (lhs, expr.get_lhs ()->get_locus ()),
		 TyTy::TyWithLocation (rhs, expr.get_rhs ()->get_locus ()),
		 expr.get_locus ());
}

void
TypeCheckExpr::visit (HIR::CompoundAssignmentExpr &expr)
{
  infered = TyTy::TupleType::get_unit_type (expr.get_mappings ().get_hirid ());

  auto lhs = TypeCheckExpr::Resolve (expr.get_left_expr ().get ());
  auto rhs = TypeCheckExpr::Resolve (expr.get_right_expr ().get ());

  // we dont care about the result of the unify from a compound assignment
  // since this is a unit-type expr
  coercion_site (expr.get_mappings ().get_hirid (),
		 TyTy::TyWithLocation (lhs,
				       expr.get_left_expr ()->get_locus ()),
		 TyTy::TyWithLocation (rhs,
				       expr.get_right_expr ()->get_locus ()),
		 expr.get_locus ());

  auto lang_item_type
    = Analysis::RustLangItem::CompoundAssignmentOperatorToLangItem (
      expr.get_expr_type ());
  bool operator_overloaded
    = resolve_operator_overload (lang_item_type, expr, lhs, rhs);
  if (operator_overloaded)
    return;

  bool valid_lhs = validate_arithmetic_type (lhs, expr.get_expr_type ());
  bool valid_rhs = validate_arithmetic_type (rhs, expr.get_expr_type ());
  bool valid = valid_lhs && valid_rhs;
  if (!valid)
    {
      rust_error_at (expr.get_locus (),
		     "cannot apply this operator to types %s and %s",
		     lhs->as_string ().c_str (), rhs->as_string ().c_str ());
      return;
    }
}

void
TypeCheckExpr::visit (HIR::LiteralExpr &expr)
{
  infered = resolve_literal (expr.get_mappings (), expr.get_literal (),
			     expr.get_locus ());
}

void
TypeCheckExpr::visit (HIR::ArithmeticOrLogicalExpr &expr)
{
  auto lhs = TypeCheckExpr::Resolve (expr.get_lhs ());
  auto rhs = TypeCheckExpr::Resolve (expr.get_rhs ());

  auto lang_item_type
    = Analysis::RustLangItem::OperatorToLangItem (expr.get_expr_type ());
  bool operator_overloaded
    = resolve_operator_overload (lang_item_type, expr, lhs, rhs);
  if (operator_overloaded)
    return;

  bool valid_lhs = validate_arithmetic_type (lhs, expr.get_expr_type ());
  bool valid_rhs = validate_arithmetic_type (rhs, expr.get_expr_type ());
  bool valid = valid_lhs && valid_rhs;
  if (!valid)
    {
      rust_error_at (expr.get_locus (),
		     "cannot apply this operator to types %s and %s",
		     lhs->as_string ().c_str (), rhs->as_string ().c_str ());
      return;
    }

  switch (expr.get_expr_type ())
    {
    case ArithmeticOrLogicalOperator::LEFT_SHIFT:
      case ArithmeticOrLogicalOperator::RIGHT_SHIFT: {
	TyTy::TyWithLocation from (rhs, expr.get_rhs ()->get_locus ());
	TyTy::TyWithLocation to (lhs, expr.get_lhs ()->get_locus ());
	infered = cast_site (expr.get_mappings ().get_hirid (), from, to,
			     expr.get_locus ());
      }
      break;

      default: {
	infered = unify_site (
	  expr.get_mappings ().get_hirid (),
	  TyTy::TyWithLocation (lhs, expr.get_lhs ()->get_locus ()),
	  TyTy::TyWithLocation (rhs, expr.get_rhs ()->get_locus ()),
	  expr.get_locus ());
      }
      break;
    }
}

void
TypeCheckExpr::visit (HIR::ComparisonExpr &expr)
{
  auto lhs = TypeCheckExpr::Resolve (expr.get_lhs ());
  auto rhs = TypeCheckExpr::Resolve (expr.get_rhs ());

  unify_site (expr.get_mappings ().get_hirid (),
	      TyTy::TyWithLocation (lhs, expr.get_lhs ()->get_locus ()),
	      TyTy::TyWithLocation (rhs, expr.get_rhs ()->get_locus ()),
	      expr.get_locus ());

  bool ok = context->lookup_builtin ("bool", &infered);
  rust_assert (ok);
}

void
TypeCheckExpr::visit (HIR::LazyBooleanExpr &expr)
{
  auto lhs = TypeCheckExpr::Resolve (expr.get_lhs ());
  auto rhs = TypeCheckExpr::Resolve (expr.get_rhs ());

  // we expect the lhs and rhs must be bools at this point
  TyTy::BaseType *boolean_node = nullptr;
  bool ok = context->lookup_builtin ("bool", &boolean_node);
  rust_assert (ok);

  // verify the lhs and rhs before unifying together
  lhs = unify_site (expr.get_mappings ().get_hirid (),
		    TyTy::TyWithLocation (boolean_node,
					  expr.get_lhs ()->get_locus ()),
		    TyTy::TyWithLocation (lhs, expr.get_lhs ()->get_locus ()),
		    expr.get_locus ());

  rhs = unify_site (expr.get_mappings ().get_hirid (),
		    TyTy::TyWithLocation (boolean_node,
					  expr.get_rhs ()->get_locus ()),
		    TyTy::TyWithLocation (rhs, expr.get_rhs ()->get_locus ()),
		    expr.get_locus ());

  infered
    = unify_site (expr.get_mappings ().get_hirid (),
		  TyTy::TyWithLocation (lhs, expr.get_lhs ()->get_locus ()),
		  TyTy::TyWithLocation (rhs, expr.get_rhs ()->get_locus ()),
		  expr.get_locus ());
}

void
TypeCheckExpr::visit (HIR::NegationExpr &expr)
{
  auto negated_expr_ty = TypeCheckExpr::Resolve (expr.get_expr ().get ());

  // check for operator overload
  auto lang_item_type = Analysis::RustLangItem::NegationOperatorToLangItem (
    expr.get_expr_type ());
  bool operator_overloaded
    = resolve_operator_overload (lang_item_type, expr, negated_expr_ty,
				 nullptr);
  if (operator_overloaded)
    return;

  // https://doc.rust-lang.org/reference/expressions/operator-expr.html#negation-operators
  switch (expr.get_expr_type ())
    {
      case NegationOperator::NEGATE: {
	bool valid
	  = (negated_expr_ty->get_kind () == TyTy::TypeKind::INT)
	    || (negated_expr_ty->get_kind () == TyTy::TypeKind::UINT)
	    || (negated_expr_ty->get_kind () == TyTy::TypeKind::FLOAT)
	    || (negated_expr_ty->get_kind () == TyTy::TypeKind::ISIZE)
	    || (negated_expr_ty->get_kind () == TyTy::TypeKind::USIZE)
	    || (negated_expr_ty->get_kind () == TyTy::TypeKind::INFER
		&& (((TyTy::InferType *) negated_expr_ty)->get_infer_kind ()
		    == TyTy::InferType::INTEGRAL))
	    || (negated_expr_ty->get_kind () == TyTy::TypeKind::INFER
		&& (((TyTy::InferType *) negated_expr_ty)->get_infer_kind ()
		    == TyTy::InferType::FLOAT));
	if (!valid)
	  {
	    rust_error_at (expr.get_locus (), "cannot apply unary - to %s",
			   negated_expr_ty->as_string ().c_str ());
	    return;
	  }
      }
      break;

      case NegationOperator::NOT: {
	bool valid
	  = (negated_expr_ty->get_kind () == TyTy::TypeKind::BOOL)
	    || (negated_expr_ty->get_kind () == TyTy::TypeKind::INT)
	    || (negated_expr_ty->get_kind () == TyTy::TypeKind::UINT)
	    || (negated_expr_ty->get_kind () == TyTy::TypeKind::INFER
		&& (((TyTy::InferType *) negated_expr_ty)->get_infer_kind ()
		    == TyTy::InferType::INTEGRAL));
	if (!valid)
	  {
	    rust_error_at (expr.get_locus (), "cannot apply unary %<!%> to %s",
			   negated_expr_ty->as_string ().c_str ());
	    return;
	  }
      }
      break;
    }

  infered = negated_expr_ty->clone ();
  infered->append_reference (negated_expr_ty->get_ref ());
}

void
TypeCheckExpr::visit (HIR::IfExpr &expr)
{
  TypeCheckExpr::Resolve (expr.get_if_condition ());
  TypeCheckExpr::Resolve (expr.get_if_block ());

  infered = TyTy::TupleType::get_unit_type (expr.get_mappings ().get_hirid ());
}

void
TypeCheckExpr::visit (HIR::IfExprConseqElse &expr)
{
  TypeCheckExpr::Resolve (expr.get_if_condition ());
  auto if_blk_resolved = TypeCheckExpr::Resolve (expr.get_if_block ());
  auto else_blk_resolved = TypeCheckExpr::Resolve (expr.get_else_block ());

  if (if_blk_resolved->get_kind () == TyTy::NEVER)
    infered = else_blk_resolved;
  else if (else_blk_resolved->get_kind () == TyTy::NEVER)
    infered = if_blk_resolved;
  else
    {
      infered = unify_site (
	expr.get_mappings ().get_hirid (),
	TyTy::TyWithLocation (if_blk_resolved,
			      expr.get_if_block ()->get_locus ()),
	TyTy::TyWithLocation (else_blk_resolved,
			      expr.get_else_block ()->get_locus ()),
	expr.get_locus ());
    }
}

void
TypeCheckExpr::visit (HIR::IfExprConseqIf &expr)
{
  TypeCheckExpr::Resolve (expr.get_if_condition ());
  auto if_blk_resolved = TypeCheckExpr::Resolve (expr.get_if_block ());
  auto else_blk_resolved = TypeCheckExpr::Resolve (expr.get_conseq_if_expr ());

  if (if_blk_resolved->get_kind () == TyTy::NEVER)
    infered = else_blk_resolved;
  else if (else_blk_resolved->get_kind () == TyTy::NEVER)
    infered = if_blk_resolved;
  else
    {
      infered = unify_site (
	expr.get_mappings ().get_hirid (),
	TyTy::TyWithLocation (if_blk_resolved,
			      expr.get_if_block ()->get_locus ()),
	TyTy::TyWithLocation (else_blk_resolved,
			      expr.get_conseq_if_expr ()->get_locus ()),
	expr.get_locus ());
    }
}

void
TypeCheckExpr::visit (HIR::IfLetExpr &expr)
{
  // this needs to perform a least upper bound coercion on the blocks and then
  // unify the scruintee and arms
  TyTy::BaseType *scrutinee_tyty
    = TypeCheckExpr::Resolve (expr.get_scrutinee_expr ().get ());

  for (auto &pattern : expr.get_patterns ())
    {
      TyTy::BaseType *kase_arm_ty
	= TypeCheckPattern::Resolve (pattern.get (), scrutinee_tyty);

      unify_site (expr.get_mappings ().get_hirid (),
		  TyTy::TyWithLocation (scrutinee_tyty),
		  TyTy::TyWithLocation (kase_arm_ty, pattern->get_locus ()),
		  expr.get_locus ());
    }

  TypeCheckExpr::Resolve (expr.get_if_block ());

  infered = TyTy::TupleType::get_unit_type (expr.get_mappings ().get_hirid ());
}

void
TypeCheckExpr::visit (HIR::UnsafeBlockExpr &expr)
{
  infered = TypeCheckExpr::Resolve (expr.get_block_expr ().get ());
}

void
TypeCheckExpr::visit (HIR::BlockExpr &expr)
{
  for (auto &s : expr.get_statements ())
    {
      if (!s->is_item ())
	continue;

      TypeCheckStmt::Resolve (s.get ());
    }

  for (auto &s : expr.get_statements ())
    {
      if (s->is_item ())
	continue;

      auto resolved = TypeCheckStmt::Resolve (s.get ());
      if (resolved == nullptr)
	{
	  rust_error_at (s->get_locus (), "failure to resolve type");
	  return;
	}

      if (s->is_unit_check_needed () && !resolved->is_unit ())
	{
	  auto unit
	    = TyTy::TupleType::get_unit_type (s->get_mappings ().get_hirid ());
	  resolved
	    = unify_site (s->get_mappings ().get_hirid (),
			  TyTy::TyWithLocation (unit),
			  TyTy::TyWithLocation (resolved), s->get_locus ());
	}
    }

  if (expr.has_expr ())
    infered = TypeCheckExpr::Resolve (expr.get_final_expr ().get ())->clone ();
  else if (expr.is_tail_reachable ())
    infered
      = TyTy::TupleType::get_unit_type (expr.get_mappings ().get_hirid ());
  else
    {
      // FIXME this seems wrong
      infered = new TyTy::NeverType (expr.get_mappings ().get_hirid ());
    }
}

void
TypeCheckExpr::visit (HIR::RangeFromToExpr &expr)
{
  auto lang_item_type = Analysis::RustLangItem::ItemType::RANGE;

  DefId respective_lang_item_id = UNKNOWN_DEFID;
  bool lang_item_defined
    = mappings->lookup_lang_item (lang_item_type, &respective_lang_item_id);

  // we need to have it maybe
  if (!lang_item_defined)
    {
      rust_internal_error_at (
	expr.get_locus (), "unable to find relevant lang item: %s",
	Analysis::RustLangItem::ToString (lang_item_type).c_str ());
      return;
    }

  // look it up and it _must_ be a struct definition
  HIR::Item *item = mappings->lookup_defid (respective_lang_item_id);
  rust_assert (item != nullptr);

  TyTy::BaseType *item_type = nullptr;
  bool ok
    = context->lookup_type (item->get_mappings ().get_hirid (), &item_type);
  rust_assert (ok);
  rust_assert (item_type->get_kind () == TyTy::TypeKind::ADT);
  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (item_type);

  // this is a single generic item lets assert that
  rust_assert (adt->get_num_substitutions () == 1);

  // resolve the range expressions and these types must unify then we use that
  // type to substitute into the ADT
  TyTy::BaseType *from_ty
    = TypeCheckExpr::Resolve (expr.get_from_expr ().get ());
  TyTy::BaseType *to_ty = TypeCheckExpr::Resolve (expr.get_to_expr ().get ());

  TyTy::BaseType *unified = unify_site (
    expr.get_mappings ().get_hirid (),
    TyTy::TyWithLocation (from_ty, expr.get_from_expr ()->get_locus ()),
    TyTy::TyWithLocation (to_ty, expr.get_to_expr ()->get_locus ()),
    expr.get_locus ());

  // substitute it in
  std::vector<TyTy::SubstitutionArg> subst_mappings;
  const TyTy::SubstitutionParamMapping *param_ref = &adt->get_substs ().at (0);
  subst_mappings.push_back (TyTy::SubstitutionArg (param_ref, unified));

  TyTy::SubstitutionArgumentMappings subst (subst_mappings, {},
					    expr.get_locus ());
  infered = SubstMapperInternal::Resolve (adt, subst);
}

void
TypeCheckExpr::visit (HIR::RangeFromExpr &expr)
{
  auto lang_item_type = Analysis::RustLangItem::ItemType::RANGE_FROM;

  DefId respective_lang_item_id = UNKNOWN_DEFID;
  bool lang_item_defined
    = mappings->lookup_lang_item (lang_item_type, &respective_lang_item_id);

  // we need to have it maybe
  if (!lang_item_defined)
    {
      rust_internal_error_at (
	expr.get_locus (), "unable to find relevant lang item: %s",
	Analysis::RustLangItem::ToString (lang_item_type).c_str ());
      return;
    }

  // look it up and it _must_ be a struct definition
  HIR::Item *item = mappings->lookup_defid (respective_lang_item_id);
  rust_assert (item != nullptr);

  TyTy::BaseType *item_type = nullptr;
  bool ok
    = context->lookup_type (item->get_mappings ().get_hirid (), &item_type);
  rust_assert (ok);
  rust_assert (item_type->get_kind () == TyTy::TypeKind::ADT);
  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (item_type);

  // this is a single generic item lets assert that
  rust_assert (adt->get_num_substitutions () == 1);

  // resolve the range expressions and these types must unify then we use that
  // type to substitute into the ADT
  TyTy::BaseType *from_ty
    = TypeCheckExpr::Resolve (expr.get_from_expr ().get ());

  // substitute it in
  std::vector<TyTy::SubstitutionArg> subst_mappings;
  const TyTy::SubstitutionParamMapping *param_ref = &adt->get_substs ().at (0);
  subst_mappings.push_back (TyTy::SubstitutionArg (param_ref, from_ty));

  TyTy::SubstitutionArgumentMappings subst (subst_mappings, {},
					    expr.get_locus ());
  infered = SubstMapperInternal::Resolve (adt, subst);
}

void
TypeCheckExpr::visit (HIR::RangeToExpr &expr)
{
  auto lang_item_type = Analysis::RustLangItem::ItemType::RANGE_TO;

  DefId respective_lang_item_id = UNKNOWN_DEFID;
  bool lang_item_defined
    = mappings->lookup_lang_item (lang_item_type, &respective_lang_item_id);

  // we need to have it maybe
  if (!lang_item_defined)
    {
      rust_internal_error_at (
	expr.get_locus (), "unable to find relevant lang item: %s",
	Analysis::RustLangItem::ToString (lang_item_type).c_str ());
      return;
    }

  // look it up and it _must_ be a struct definition
  HIR::Item *item = mappings->lookup_defid (respective_lang_item_id);
  rust_assert (item != nullptr);

  TyTy::BaseType *item_type = nullptr;
  bool ok
    = context->lookup_type (item->get_mappings ().get_hirid (), &item_type);
  rust_assert (ok);
  rust_assert (item_type->get_kind () == TyTy::TypeKind::ADT);
  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (item_type);

  // this is a single generic item lets assert that
  rust_assert (adt->get_num_substitutions () == 1);

  // resolve the range expressions and these types must unify then we use that
  // type to substitute into the ADT
  TyTy::BaseType *from_ty = TypeCheckExpr::Resolve (expr.get_to_expr ().get ());

  // substitute it in
  std::vector<TyTy::SubstitutionArg> subst_mappings;
  const TyTy::SubstitutionParamMapping *param_ref = &adt->get_substs ().at (0);
  subst_mappings.push_back (TyTy::SubstitutionArg (param_ref, from_ty));

  TyTy::SubstitutionArgumentMappings subst (subst_mappings, {},
					    expr.get_locus ());
  infered = SubstMapperInternal::Resolve (adt, subst);
}

void
TypeCheckExpr::visit (HIR::RangeFullExpr &expr)
{
  auto lang_item_type = Analysis::RustLangItem::ItemType::RANGE_FULL;

  DefId respective_lang_item_id = UNKNOWN_DEFID;
  bool lang_item_defined
    = mappings->lookup_lang_item (lang_item_type, &respective_lang_item_id);

  // we need to have it maybe
  if (!lang_item_defined)
    {
      rust_internal_error_at (
	expr.get_locus (), "unable to find relevant lang item: %s",
	Analysis::RustLangItem::ToString (lang_item_type).c_str ());
      return;
    }

  // look it up and it _must_ be a struct definition
  HIR::Item *item = mappings->lookup_defid (respective_lang_item_id);
  rust_assert (item != nullptr);

  TyTy::BaseType *item_type = nullptr;
  bool ok
    = context->lookup_type (item->get_mappings ().get_hirid (), &item_type);
  rust_assert (ok);
  rust_assert (item_type->is_unit ());

  infered = item_type;
}

void
TypeCheckExpr::visit (HIR::RangeFromToInclExpr &expr)
{
  auto lang_item_type = Analysis::RustLangItem::ItemType::RANGE_INCLUSIVE;

  DefId respective_lang_item_id = UNKNOWN_DEFID;
  bool lang_item_defined
    = mappings->lookup_lang_item (lang_item_type, &respective_lang_item_id);

  // we need to have it maybe
  if (!lang_item_defined)
    {
      rust_internal_error_at (
	expr.get_locus (), "unable to find relevant lang item: %s",
	Analysis::RustLangItem::ToString (lang_item_type).c_str ());
      return;
    }

  // look it up and it _must_ be a struct definition
  HIR::Item *item = mappings->lookup_defid (respective_lang_item_id);
  rust_assert (item != nullptr);

  TyTy::BaseType *item_type = nullptr;
  bool ok
    = context->lookup_type (item->get_mappings ().get_hirid (), &item_type);
  rust_assert (ok);
  rust_assert (item_type->get_kind () == TyTy::TypeKind::ADT);
  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (item_type);

  // this is a single generic item lets assert that
  rust_assert (adt->get_num_substitutions () == 1);

  // resolve the range expressions and these types must unify then we use that
  // type to substitute into the ADT
  TyTy::BaseType *from_ty
    = TypeCheckExpr::Resolve (expr.get_from_expr ().get ());
  TyTy::BaseType *to_ty = TypeCheckExpr::Resolve (expr.get_to_expr ().get ());
  TyTy::BaseType *unified = unify_site (
    expr.get_mappings ().get_hirid (),
    TyTy::TyWithLocation (from_ty, expr.get_from_expr ()->get_locus ()),
    TyTy::TyWithLocation (to_ty, expr.get_to_expr ()->get_locus ()),
    expr.get_locus ());

  // substitute it in
  std::vector<TyTy::SubstitutionArg> subst_mappings;
  const TyTy::SubstitutionParamMapping *param_ref = &adt->get_substs ().at (0);
  subst_mappings.push_back (TyTy::SubstitutionArg (param_ref, unified));

  TyTy::SubstitutionArgumentMappings subst (subst_mappings, {},
					    expr.get_locus ());
  infered = SubstMapperInternal::Resolve (adt, subst);
}

void
TypeCheckExpr::visit (HIR::ArrayIndexExpr &expr)
{
  auto array_expr_ty = TypeCheckExpr::Resolve (expr.get_array_expr ());
  if (array_expr_ty->get_kind () == TyTy::TypeKind::ERROR)
    return;

  auto index_expr_ty = TypeCheckExpr::Resolve (expr.get_index_expr ());
  if (index_expr_ty->get_kind () == TyTy::TypeKind::ERROR)
    return;

  // first attempt to use direct array index logic
  auto direct_array_expr_ty = array_expr_ty;
  if (direct_array_expr_ty->get_kind () == TyTy::TypeKind::REF)
    {
      // lets try and deref it since rust allows this
      auto ref = static_cast<TyTy::ReferenceType *> (direct_array_expr_ty);
      auto base = ref->get_base ();
      if (base->get_kind () == TyTy::TypeKind::ARRAY)
	direct_array_expr_ty = base;
    }

  TyTy::BaseType *size_ty;
  bool ok = context->lookup_builtin ("usize", &size_ty);
  rust_assert (ok);

  bool maybe_simple_array_access = index_expr_ty->can_eq (size_ty, false);
  if (maybe_simple_array_access
      && direct_array_expr_ty->get_kind () == TyTy::TypeKind::ARRAY)
    {
      unify_site (expr.get_index_expr ()->get_mappings ().get_hirid (),
		  TyTy::TyWithLocation (size_ty),
		  TyTy::TyWithLocation (index_expr_ty,
					expr.get_index_expr ()->get_locus ()),
		  expr.get_locus ());

      TyTy::ArrayType *array_type
	= static_cast<TyTy::ArrayType *> (direct_array_expr_ty);
      infered = array_type->get_element_type ()->clone ();
      return;
    }

  // is this a case of core::ops::index?
  auto lang_item_type = Analysis::RustLangItem::ItemType::INDEX;
  bool operator_overloaded
    = resolve_operator_overload (lang_item_type, expr, array_expr_ty,
				 index_expr_ty);
  if (operator_overloaded)
    {
      // index and index mut always return a reference to the element
      TyTy::BaseType *resolved = infered;
      rust_assert (resolved->get_kind () == TyTy::TypeKind::REF);
      TyTy::ReferenceType *ref = static_cast<TyTy::ReferenceType *> (resolved);

      infered = ref->get_base ()->clone ();
      return;
    }

  // error[E0277]: the type `[{integer}]` cannot be indexed by `u32`
  RichLocation r (expr.get_locus ());
  r.add_range (expr.get_array_expr ()->get_locus ());
  r.add_range (expr.get_index_expr ()->get_locus ());
  rust_error_at (r, "the type %<%s%> cannot be indexed by %<%s%>",
		 array_expr_ty->get_name ().c_str (),
		 index_expr_ty->get_name ().c_str ());
}

void
TypeCheckExpr::visit (HIR::ArrayExpr &expr)
{
  HIR::ArrayElems &elements = *expr.get_internal_elements ();

  HIR::Expr *capacity_expr = nullptr;
  TyTy::BaseType *element_type = nullptr;
  switch (elements.get_array_expr_type ())
    {
      case HIR::ArrayElems::ArrayExprType::COPIED: {
	HIR::ArrayElemsCopied &elems
	  = static_cast<HIR::ArrayElemsCopied &> (elements);
	element_type = TypeCheckExpr::Resolve (elems.get_elem_to_copy ());

	auto capacity_type
	  = TypeCheckExpr::Resolve (elems.get_num_copies_expr ());

	TyTy::BaseType *expected_ty = nullptr;
	bool ok = context->lookup_builtin ("usize", &expected_ty);
	rust_assert (ok);
	context->insert_type (elems.get_num_copies_expr ()->get_mappings (),
			      expected_ty);

	unify_site (
	  expr.get_mappings ().get_hirid (), TyTy::TyWithLocation (expected_ty),
	  TyTy::TyWithLocation (capacity_type,
				elems.get_num_copies_expr ()->get_locus ()),
	  expr.get_locus ());

	capacity_expr = elems.get_num_copies_expr ();
      }
      break;

      case HIR::ArrayElems::ArrayExprType::VALUES: {
	HIR::ArrayElemsValues &elems
	  = static_cast<HIR::ArrayElemsValues &> (elements);

	std::vector<TyTy::BaseType *> types;
	for (auto &elem : elems.get_values ())
	  {
	    types.push_back (TypeCheckExpr::Resolve (elem.get ()));
	  }

	// this is a LUB
	element_type
	  = TyTy::TyVar::get_implicit_infer_var (expr.get_locus ()).get_tyty ();
	for (auto &type : types)
	  {
	    element_type
	      = unify_site (expr.get_mappings ().get_hirid (),
			    TyTy::TyWithLocation (element_type),
			    TyTy::TyWithLocation (type, type->get_locus ()),
			    expr.get_locus ());
	  }

	auto crate_num = mappings->get_current_crate ();
	Analysis::NodeMapping mapping (crate_num, UNKNOWN_NODEID,
				       mappings->get_next_hir_id (crate_num),
				       UNKNOWN_LOCAL_DEFID);
	std::string capacity_str = std::to_string (elems.get_num_elements ());
	capacity_expr = new HIR::LiteralExpr (mapping, capacity_str,
					      HIR::Literal::LitType::INT,
					      PrimitiveCoreType::CORETYPE_USIZE,
					      Location (), {});

	// mark the type for this implicit node
	TyTy::BaseType *expected_ty = nullptr;
	bool ok = context->lookup_builtin ("usize", &expected_ty);
	rust_assert (ok);
	context->insert_type (mapping, expected_ty);
      }
      break;
    }

  infered = new TyTy::ArrayType (expr.get_mappings ().get_hirid (),
				 expr.get_locus (), *capacity_expr,
				 TyTy::TyVar (element_type->get_ref ()));
}

// empty struct
void
TypeCheckExpr::visit (HIR::StructExprStruct &struct_expr)
{
  TyTy::BaseType *struct_path_ty
    = TypeCheckExpr::Resolve (&struct_expr.get_struct_name ());
  if (struct_path_ty->get_kind () != TyTy::TypeKind::ADT)
    {
      rust_error_at (struct_expr.get_struct_name ().get_locus (),
		     "expected an ADT type for constructor");
      return;
    }

  infered = struct_path_ty;
}

void
TypeCheckExpr::visit (HIR::StructExprStructFields &struct_expr)
{
  infered = TypeCheckStructExpr::Resolve (&struct_expr);
}

void
TypeCheckExpr::visit (HIR::GroupedExpr &expr)
{
  infered = TypeCheckExpr::Resolve (expr.get_expr_in_parens ().get ());
}

void
TypeCheckExpr::visit (HIR::FieldAccessExpr &expr)
{
  auto struct_base = TypeCheckExpr::Resolve (expr.get_receiver_expr ().get ());

  // FIXME does this require autoderef here?
  if (struct_base->get_kind () == TyTy::TypeKind::REF)
    {
      TyTy::ReferenceType *r = static_cast<TyTy::ReferenceType *> (struct_base);
      struct_base = r->get_base ();
    }

  bool is_valid_type = struct_base->get_kind () == TyTy::TypeKind::ADT;
  if (!is_valid_type)
    {
      rust_error_at (expr.get_locus (),
		     "expected algebraic data type got: [%s]",
		     struct_base->as_string ().c_str ());
      return;
    }

  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (struct_base);
  rust_assert (!adt->is_enum ());
  rust_assert (adt->number_of_variants () == 1);

  TyTy::VariantDef *vaiant = adt->get_variants ().at (0);

  TyTy::StructFieldType *lookup = nullptr;
  bool found = vaiant->lookup_field (expr.get_field_name (), &lookup, nullptr);
  if (!found)
    {
      rust_error_at (expr.get_locus (), "unknown field [%s] for type [%s]",
		     expr.get_field_name ().c_str (),
		     adt->as_string ().c_str ());
      return;
    }

  infered = lookup->get_field_type ();
}

void
TypeCheckExpr::visit (HIR::MethodCallExpr &expr)
{
  auto receiver_tyty = TypeCheckExpr::Resolve (expr.get_receiver ().get ());
  if (receiver_tyty->get_kind () == TyTy::TypeKind::ERROR)
    {
      rust_error_at (expr.get_receiver ()->get_locus (),
		     "failed to resolve receiver in MethodCallExpr");
      return;
    }

  context->insert_receiver (expr.get_mappings ().get_hirid (), receiver_tyty);

  auto candidates
    = MethodResolver::Probe (receiver_tyty,
			     expr.get_method_name ().get_segment ());
  if (candidates.empty ())
    {
      rust_error_at (
	expr.get_method_name ().get_locus (),
	"failed to resolve method for %<%s%>",
	expr.get_method_name ().get_segment ().as_string ().c_str ());
      return;
    }

  if (candidates.size () > 1)
    {
      RichLocation r (expr.get_method_name ().get_locus ());
      for (auto &c : candidates)
	r.add_range (c.candidate.locus);

      rust_error_at (
	r, "multiple candidates found for method %<%s%>",
	expr.get_method_name ().get_segment ().as_string ().c_str ());
      return;
    }

  auto candidate = *candidates.begin ();
  rust_debug_loc (expr.get_method_name ().get_locus (),
		  "resolved method to: {%u} {%s}",
		  candidate.candidate.ty->get_ref (),
		  candidate.candidate.ty->debug_str ().c_str ());

  // Get the adjusted self
  Adjuster adj (receiver_tyty);
  TyTy::BaseType *adjusted_self = adj.adjust_type (candidate.adjustments);

  // store the adjustments for code-generation to know what to do which must be
  // stored onto the receiver to so as we don't trigger duplicate deref mappings
  // ICE when an argument is a method call
  HirId autoderef_mappings_id
    = expr.get_receiver ()->get_mappings ().get_hirid ();
  context->insert_autoderef_mappings (autoderef_mappings_id,
				      std::move (candidate.adjustments));

  PathProbeCandidate &resolved_candidate = candidate.candidate;
  TyTy::BaseType *lookup_tyty = candidate.candidate.ty;
  NodeId resolved_node_id
    = resolved_candidate.is_impl_candidate ()
	? resolved_candidate.item.impl.impl_item->get_impl_mappings ()
	    .get_nodeid ()
	: resolved_candidate.item.trait.item_ref->get_mappings ().get_nodeid ();

  if (lookup_tyty->get_kind () != TyTy::TypeKind::FNDEF)
    {
      RichLocation r (expr.get_method_name ().get_locus ());
      r.add_range (resolved_candidate.locus);
      rust_error_at (r, "associated impl item is not a method");
      return;
    }

  TyTy::BaseType *lookup = lookup_tyty;
  TyTy::FnType *fn = static_cast<TyTy::FnType *> (lookup);
  if (!fn->is_method ())
    {
      RichLocation r (expr.get_method_name ().get_locus ());
      r.add_range (resolved_candidate.locus);
      rust_error_at (r, "associated function is not a method");
      return;
    }

  fn->prepare_higher_ranked_bounds ();
  auto root = receiver_tyty->get_root ();
  if (root->get_kind () == TyTy::TypeKind::ADT)
    {
      const TyTy::ADTType *adt = static_cast<const TyTy::ADTType *> (root);
      if (adt->has_substitutions () && fn->needs_substitution ())
	{
	  // consider the case where we have:
	  //
	  // struct Foo<X,Y>(X,Y);
	  //
	  // impl<T> Foo<T, i32> {
	  //   fn test<X>(self, a:X) -> (T,X) { (self.0, a) }
	  // }
	  //
	  // In this case we end up with an fn type of:
	  //
	  // fn <T,X> test(self:Foo<T,i32>, a:X) -> (T,X)
	  //
	  // This means the instance or self we are calling this method for
	  // will be substituted such that we can get the inherited type
	  // arguments but then need to use the turbo fish if available or
	  // infer the remaining arguments. Luckily rust does not allow for
	  // default types GenericParams on impl blocks since these must
	  // always be at the end of the list

	  auto s = fn->get_self_type ()->get_root ();
	  rust_assert (s->can_eq (adt, false));
	  rust_assert (s->get_kind () == TyTy::TypeKind::ADT);
	  const TyTy::ADTType *self_adt
	    = static_cast<const TyTy::ADTType *> (s);

	  // we need to grab the Self substitutions as the inherit type
	  // parameters for this
	  if (self_adt->needs_substitution ())
	    {
	      rust_assert (adt->was_substituted ());

	      TyTy::SubstitutionArgumentMappings used_args_in_prev_segment
		= GetUsedSubstArgs::From (adt);

	      TyTy::SubstitutionArgumentMappings inherit_type_args
		= self_adt->solve_mappings_from_receiver_for_self (
		  used_args_in_prev_segment);

	      // there may or may not be inherited type arguments
	      if (!inherit_type_args.is_error ())
		{
		  // need to apply the inherited type arguments to the
		  // function
		  lookup = fn->handle_substitions (inherit_type_args);
		}
	    }
	}
    }

  // apply any remaining generic arguments
  if (expr.get_method_name ().has_generic_args ())
    {
      HIR::GenericArgs &args = expr.get_method_name ().get_generic_args ();
      rust_debug_loc (args.get_locus (),
		      "applying generic arguments to method_call: {%s}",
		      lookup->debug_str ().c_str ());

      lookup
	= SubstMapper::Resolve (lookup, expr.get_method_name ().get_locus (),
				&args);
      if (lookup->get_kind () == TyTy::TypeKind::ERROR)
	return;
    }
  else if (lookup->needs_generic_substitutions ())
    {
      rust_debug ("method needs inference: {%s}",
		  lookup->debug_str ().c_str ());
      lookup = SubstMapper::InferSubst (lookup,
					expr.get_method_name ().get_locus ());
    }

  rust_debug ("type-checking method_call: {%s}", lookup->debug_str ().c_str ());

  TyTy::BaseType *function_ret_tyty
    = TyTy::TypeCheckMethodCallExpr::go (static_cast<TyTy::FnType *> (lookup),
					 expr, adjusted_self, context);
  if (function_ret_tyty == nullptr
      || function_ret_tyty->get_kind () == TyTy::TypeKind::ERROR)
    {
      rust_error_at (expr.get_locus (),
		     "failed to lookup type to MethodCallExpr");
      return;
    }

  // store the expected fntype
  context->insert_type (expr.get_method_name ().get_mappings (), lookup);

  // set up the resolved name on the path
  resolver->insert_resolved_name (expr.get_mappings ().get_nodeid (),
				  resolved_node_id);

  // return the result of the function back
  infered = function_ret_tyty;
}

void
TypeCheckExpr::visit (HIR::LoopExpr &expr)
{
  context->push_new_loop_context (expr.get_mappings ().get_hirid (),
				  expr.get_locus ());
  TyTy::BaseType *block_expr
    = TypeCheckExpr::Resolve (expr.get_loop_block ().get ());
  if (!block_expr->is_unit ())
    {
      rust_error_at (expr.get_loop_block ()->get_locus (),
		     "expected %<()%> got %s",
		     block_expr->as_string ().c_str ());
      return;
    }

  TyTy::BaseType *loop_context_type = context->pop_loop_context ();

  bool loop_context_type_infered
    = (loop_context_type->get_kind () != TyTy::TypeKind::INFER)
      || ((loop_context_type->get_kind () == TyTy::TypeKind::INFER)
	  && (((TyTy::InferType *) loop_context_type)->get_infer_kind ()
	      != TyTy::InferType::GENERAL));

  infered
    = loop_context_type_infered
	? loop_context_type
	: TyTy::TupleType::get_unit_type (expr.get_mappings ().get_hirid ());
}

void
TypeCheckExpr::visit (HIR::WhileLoopExpr &expr)
{
  context->push_new_while_loop_context (expr.get_mappings ().get_hirid ());

  TypeCheckExpr::Resolve (expr.get_predicate_expr ().get ());
  TyTy::BaseType *block_expr
    = TypeCheckExpr::Resolve (expr.get_loop_block ().get ());

  if (!block_expr->is_unit ())
    {
      rust_error_at (expr.get_loop_block ()->get_locus (),
		     "expected %<()%> got %s",
		     block_expr->as_string ().c_str ());
      return;
    }

  context->pop_loop_context ();
  infered = TyTy::TupleType::get_unit_type (expr.get_mappings ().get_hirid ());
}

void
TypeCheckExpr::visit (HIR::BreakExpr &expr)
{
  if (!context->have_loop_context ())
    {
      rust_error_at (expr.get_locus (), "cannot %<break%> outside of a loop");
      return;
    }

  if (expr.has_break_expr ())
    {
      TyTy::BaseType *break_expr_tyty
	= TypeCheckExpr::Resolve (expr.get_expr ().get ());

      TyTy::BaseType *loop_context = context->peek_loop_context ();
      if (loop_context->get_kind () == TyTy::TypeKind::ERROR)
	{
	  rust_error_at (expr.get_locus (),
			 "can only break with a value inside %<loop%>");
	  return;
	}

      TyTy::BaseType *unified_ty
	= unify_site (expr.get_mappings ().get_hirid (),
		      TyTy::TyWithLocation (loop_context),
		      TyTy::TyWithLocation (break_expr_tyty,
					    expr.get_expr ()->get_locus ()),
		      expr.get_locus ());
      context->swap_head_loop_context (unified_ty);
    }

  infered = new TyTy::NeverType (expr.get_mappings ().get_hirid ());
}

void
TypeCheckExpr::visit (HIR::ContinueExpr &expr)
{
  if (!context->have_loop_context ())
    {
      rust_error_at (expr.get_locus (),
		     "cannot %<continue%> outside of a loop");
      return;
    }

  infered = new TyTy::NeverType (expr.get_mappings ().get_hirid ());
}

void
TypeCheckExpr::visit (HIR::BorrowExpr &expr)
{
  TyTy::BaseType *resolved_base
    = TypeCheckExpr::Resolve (expr.get_expr ().get ());

  // In Rust this is valid because of DST's
  //
  // fn test() {
  //     let a:&str = "TEST 1";
  //     let b:&str = &"TEST 2";
  // }
  if (resolved_base->get_kind () == TyTy::TypeKind::REF)
    {
      const TyTy::ReferenceType *ref
	= static_cast<const TyTy::ReferenceType *> (resolved_base);

      // this might end up being a more generic is_dyn object check but lets
      // double check dyn traits type-layout first
      if (ref->is_dyn_str_type ())
	{
	  infered = resolved_base;
	  return;
	}
    }

  infered = new TyTy::ReferenceType (expr.get_mappings ().get_hirid (),
				     TyTy::TyVar (resolved_base->get_ref ()),
				     expr.get_mut ());
}

void
TypeCheckExpr::visit (HIR::DereferenceExpr &expr)
{
  TyTy::BaseType *resolved_base
    = TypeCheckExpr::Resolve (expr.get_expr ().get ());

  auto lang_item_type = Analysis::RustLangItem::ItemType::DEREF;
  bool operator_overloaded
    = resolve_operator_overload (lang_item_type, expr, resolved_base, nullptr);
  if (operator_overloaded)
    {
      // operator overloaded deref always refurns a reference type lets assert
      // this
      rust_assert (infered->get_kind () == TyTy::TypeKind::REF);
      resolved_base = infered;
    }

  bool is_valid_type = resolved_base->get_kind () == TyTy::TypeKind::REF
		       || resolved_base->get_kind () == TyTy::TypeKind::POINTER;
  if (!is_valid_type)
    {
      rust_error_at (expr.get_locus (), "expected reference type got %s",
		     resolved_base->as_string ().c_str ());
      return;
    }

  if (resolved_base->get_kind () == TyTy::TypeKind::REF)
    {
      TyTy::ReferenceType *ref_base
	= static_cast<TyTy::ReferenceType *> (resolved_base);
      infered = ref_base->get_base ()->clone ();
    }
  else
    {
      TyTy::PointerType *ref_base
	= static_cast<TyTy::PointerType *> (resolved_base);
      infered = ref_base->get_base ()->clone ();
    }
}

void
TypeCheckExpr::visit (HIR::TypeCastExpr &expr)
{
  TyTy::BaseType *expr_to_convert
    = TypeCheckExpr::Resolve (expr.get_casted_expr ().get ());
  TyTy::BaseType *tyty_to_convert_to
    = TypeCheckType::Resolve (expr.get_type_to_convert_to ().get ());

  TyTy::TyWithLocation from (expr_to_convert,
			     expr.get_casted_expr ()->get_locus ());
  TyTy::TyWithLocation to (tyty_to_convert_to,
			   expr.get_type_to_convert_to ()->get_locus ());
  infered = cast_site (expr.get_mappings ().get_hirid (), from, to,
		       expr.get_locus ());
}

void
TypeCheckExpr::visit (HIR::MatchExpr &expr)
{
  // this needs to perform a least upper bound coercion on the blocks and then
  // unify the scruintee and arms
  TyTy::BaseType *scrutinee_tyty
    = TypeCheckExpr::Resolve (expr.get_scrutinee_expr ().get ());

  std::vector<TyTy::BaseType *> kase_block_tys;
  for (auto &kase : expr.get_match_cases ())
    {
      // lets check the arms
      HIR::MatchArm &kase_arm = kase.get_arm ();
      for (auto &pattern : kase_arm.get_patterns ())
	{
	  TyTy::BaseType *kase_arm_ty
	    = TypeCheckPattern::Resolve (pattern.get (), scrutinee_tyty);

	  TyTy::BaseType *checked_kase = unify_site (
	    expr.get_mappings ().get_hirid (),
	    TyTy::TyWithLocation (scrutinee_tyty,
				  expr.get_scrutinee_expr ()->get_locus ()),
	    TyTy::TyWithLocation (kase_arm_ty, pattern->get_locus ()),
	    expr.get_locus ());
	  if (checked_kase->get_kind () == TyTy::TypeKind::ERROR)
	    return;
	}

      // check the kase type
      TyTy::BaseType *kase_block_ty
	= TypeCheckExpr::Resolve (kase.get_expr ().get ());
      kase_block_tys.push_back (kase_block_ty);
    }

  if (kase_block_tys.size () == 0)
    {
      infered
	= TyTy::TupleType::get_unit_type (expr.get_mappings ().get_hirid ());
      return;
    }

  // this is a LUB
  infered = kase_block_tys.at (0);
  for (size_t i = 1; i < kase_block_tys.size (); i++)
    {
      TyTy::BaseType *kase_ty = kase_block_tys.at (i);
      infered = unify_site (expr.get_mappings ().get_hirid (),
			    TyTy::TyWithLocation (infered),
			    TyTy::TyWithLocation (kase_ty), expr.get_locus ());
    }
}

void
TypeCheckExpr::visit (HIR::ClosureExpr &expr)
{
  TypeCheckContextItem &current_context = context->peek_context ();
  TyTy::FnType *current_context_fndecl = current_context.get_context_type ();

  HirId ref = expr.get_mappings ().get_hirid ();
  DefId id = expr.get_mappings ().get_defid ();
  RustIdent ident{current_context_fndecl->get_ident ().path, expr.get_locus ()};

  // get from parent context
  std::vector<TyTy::SubstitutionParamMapping> subst_refs
    = current_context_fndecl->clone_substs ();

  std::vector<TyTy::TyVar> parameter_types;
  for (auto &p : expr.get_params ())
    {
      if (p.has_type_given ())
	{
	  TyTy::BaseType *param_tyty
	    = TypeCheckType::Resolve (p.get_type ().get ());
	  TyTy::TyVar param_ty (param_tyty->get_ref ());
	  parameter_types.push_back (param_ty);

	  TypeCheckPattern::Resolve (p.get_pattern ().get (),
				     param_ty.get_tyty ());
	}
      else
	{
	  TyTy::TyVar param_ty
	    = TyTy::TyVar::get_implicit_infer_var (p.get_locus ());
	  parameter_types.push_back (param_ty);

	  TypeCheckPattern::Resolve (p.get_pattern ().get (),
				     param_ty.get_tyty ());
	}
    }

  // we generate an implicit hirid for the closure args
  HirId implicit_args_id = mappings->get_next_hir_id ();
  TyTy::TupleType *closure_args
    = new TyTy::TupleType (implicit_args_id, expr.get_locus (),
			   parameter_types);
  context->insert_implicit_type (closure_args);

  Location result_type_locus = expr.has_return_type ()
				 ? expr.get_return_type ()->get_locus ()
				 : expr.get_locus ();
  TyTy::TyVar result_type
    = expr.has_return_type ()
	? TyTy::TyVar (
	  TypeCheckType::Resolve (expr.get_return_type ().get ())->get_ref ())
	: TyTy::TyVar::get_implicit_infer_var (expr.get_locus ());

  // resolve the block
  Location closure_expr_locus = expr.get_expr ()->get_locus ();
  TyTy::BaseType *closure_expr_ty
    = TypeCheckExpr::Resolve (expr.get_expr ().get ());
  coercion_site (expr.get_mappings ().get_hirid (),
		 TyTy::TyWithLocation (result_type.get_tyty (),
				       result_type_locus),
		 TyTy::TyWithLocation (closure_expr_ty, closure_expr_locus),
		 expr.get_locus ());

  // generate the closure type
  NodeId closure_node_id = expr.get_mappings ().get_nodeid ();
  const std::set<NodeId> &captures = resolver->get_captures (closure_node_id);
  infered = new TyTy::ClosureType (ref, id, ident, closure_args, result_type,
				   subst_refs, captures);

  // FIXME
  // all closures automatically inherit the appropriate fn trait. Lets just
  // assume FnOnce for now. I think this is based on the return type of the
  // closure

  Analysis::RustLangItem::ItemType lang_item_type
    = Analysis::RustLangItem::ItemType::FN_ONCE;
  DefId respective_lang_item_id = UNKNOWN_DEFID;
  bool lang_item_defined
    = mappings->lookup_lang_item (lang_item_type, &respective_lang_item_id);
  if (!lang_item_defined)
    {
      // FIXME
      // we need to have a unified way or error'ing when we are missing lang
      // items that is useful
      rust_fatal_error (
	expr.get_locus (), "unable to find lang item: %<%s%>",
	Analysis::RustLangItem::ToString (lang_item_type).c_str ());
    }
  rust_assert (lang_item_defined);

  // these lang items are always traits
  HIR::Item *item = mappings->lookup_defid (respective_lang_item_id);
  rust_assert (item->get_item_kind () == HIR::Item::ItemKind::Trait);
  HIR::Trait *trait_item = static_cast<HIR::Trait *> (item);

  TraitReference *trait = TraitResolver::Resolve (*trait_item);
  rust_assert (!trait->is_error ());

  TyTy::TypeBoundPredicate predicate (*trait, expr.get_locus ());

  // resolve the trait bound where the <(Args)> are the parameter tuple type
  HIR::GenericArgs args = HIR::GenericArgs::create_empty (expr.get_locus ());

  // lets generate an implicit Type so that it resolves to the implict tuple
  // type we have created
  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_mappings ().get_nodeid (),
				 implicit_args_id, UNKNOWN_LOCAL_DEFID);
  HIR::TupleType *implicit_tuple
    = new HIR::TupleType (mapping,
			  {} // we dont need to fill this out because it will
			     // auto resolve because the hir id's match
			  ,
			  expr.get_locus ());
  args.get_type_args ().push_back (std::unique_ptr<HIR::Type> (implicit_tuple));

  // apply the arguments
  predicate.apply_generic_arguments (&args);

  // finally inherit the trait bound
  infered->inherit_bounds ({predicate});
}

bool
TypeCheckExpr::resolve_operator_overload (
  Analysis::RustLangItem::ItemType lang_item_type, HIR::OperatorExprMeta expr,
  TyTy::BaseType *lhs, TyTy::BaseType *rhs)
{
  // look up lang item for arithmetic type
  std::string associated_item_name
    = Analysis::RustLangItem::ToString (lang_item_type);
  DefId respective_lang_item_id = UNKNOWN_DEFID;
  bool lang_item_defined
    = mappings->lookup_lang_item (lang_item_type, &respective_lang_item_id);

  // probe for the lang-item
  if (!lang_item_defined)
    return false;

  auto segment = HIR::PathIdentSegment (associated_item_name);
  auto candidates
    = MethodResolver::Probe (lhs, HIR::PathIdentSegment (associated_item_name));

  bool have_implementation_for_lang_item = candidates.size () > 0;
  if (!have_implementation_for_lang_item)
    return false;

  if (candidates.size () > 1)
    {
      // mutliple candidates
      RichLocation r (expr.get_locus ());
      for (auto &c : candidates)
	r.add_range (c.candidate.locus);

      rust_error_at (
	r, "multiple candidates found for possible operator overload");

      return false;
    }

  // Get the adjusted self
  auto candidate = *candidates.begin ();
  Adjuster adj (lhs);
  TyTy::BaseType *adjusted_self = adj.adjust_type (candidate.adjustments);

  // is this the case we are recursive
  // handle the case where we are within the impl block for this lang_item
  // otherwise we end up with a recursive operator overload such as the i32
  // operator overload trait
  TypeCheckContextItem &fn_context = context->peek_context ();
  if (fn_context.get_type () == TypeCheckContextItem::ItemType::IMPL_ITEM)
    {
      auto &impl_item = fn_context.get_impl_item ();
      HIR::ImplBlock *parent = impl_item.first;
      HIR::Function *fn = impl_item.second;

      if (parent->has_trait_ref ()
	  && fn->get_function_name ().compare (associated_item_name) == 0)
	{
	  TraitReference *trait_reference
	    = TraitResolver::Lookup (*parent->get_trait_ref ().get ());
	  if (!trait_reference->is_error ())
	    {
	      TyTy::BaseType *lookup = nullptr;
	      bool ok = context->lookup_type (fn->get_mappings ().get_hirid (),
					      &lookup);
	      rust_assert (ok);
	      rust_assert (lookup->get_kind () == TyTy::TypeKind::FNDEF);

	      TyTy::FnType *fntype = static_cast<TyTy::FnType *> (lookup);
	      rust_assert (fntype->is_method ());

	      bool is_lang_item_impl
		= trait_reference->get_mappings ().get_defid ()
		  == respective_lang_item_id;
	      bool self_is_lang_item_self
		= fntype->get_self_type ()->is_equal (*adjusted_self);
	      bool recursive_operator_overload
		= is_lang_item_impl && self_is_lang_item_self;

	      if (recursive_operator_overload)
		return false;
	    }
	}
    }

  // store the adjustments for code-generation to know what to do
  context->insert_autoderef_mappings (expr.get_lvalue_mappings ().get_hirid (),
				      std::move (candidate.adjustments));

  // now its just like a method-call-expr
  context->insert_receiver (expr.get_mappings ().get_hirid (), lhs);

  PathProbeCandidate &resolved_candidate = candidate.candidate;
  TyTy::BaseType *lookup_tyty = candidate.candidate.ty;
  NodeId resolved_node_id
    = resolved_candidate.is_impl_candidate ()
	? resolved_candidate.item.impl.impl_item->get_impl_mappings ()
	    .get_nodeid ()
	: resolved_candidate.item.trait.item_ref->get_mappings ().get_nodeid ();

  rust_assert (lookup_tyty->get_kind () == TyTy::TypeKind::FNDEF);
  TyTy::BaseType *lookup = lookup_tyty;
  TyTy::FnType *fn = static_cast<TyTy::FnType *> (lookup);
  rust_assert (fn->is_method ());

  fn->prepare_higher_ranked_bounds ();
  rust_debug_loc (expr.get_locus (), "resolved operator overload to: {%u} {%s}",
		  candidate.candidate.ty->get_ref (),
		  candidate.candidate.ty->debug_str ().c_str ());

  auto root = lhs->get_root ();
  if (root->get_kind () == TyTy::TypeKind::ADT)
    {
      const TyTy::ADTType *adt = static_cast<const TyTy::ADTType *> (root);
      if (adt->has_substitutions () && fn->needs_substitution ())
	{
	  // consider the case where we have:
	  //
	  // struct Foo<X,Y>(X,Y);
	  //
	  // impl<T> Foo<T, i32> {
	  //   fn test<X>(self, a:X) -> (T,X) { (self.0, a) }
	  // }
	  //
	  // In this case we end up with an fn type of:
	  //
	  // fn <T,X> test(self:Foo<T,i32>, a:X) -> (T,X)
	  //
	  // This means the instance or self we are calling this method for
	  // will be substituted such that we can get the inherited type
	  // arguments but then need to use the turbo fish if available or
	  // infer the remaining arguments. Luckily rust does not allow for
	  // default types GenericParams on impl blocks since these must
	  // always be at the end of the list

	  auto s = fn->get_self_type ()->get_root ();
	  rust_assert (s->can_eq (adt, false));
	  rust_assert (s->get_kind () == TyTy::TypeKind::ADT);
	  const TyTy::ADTType *self_adt
	    = static_cast<const TyTy::ADTType *> (s);

	  // we need to grab the Self substitutions as the inherit type
	  // parameters for this
	  if (self_adt->needs_substitution ())
	    {
	      rust_assert (adt->was_substituted ());

	      TyTy::SubstitutionArgumentMappings used_args_in_prev_segment
		= GetUsedSubstArgs::From (adt);

	      TyTy::SubstitutionArgumentMappings inherit_type_args
		= self_adt->solve_mappings_from_receiver_for_self (
		  used_args_in_prev_segment);

	      // there may or may not be inherited type arguments
	      if (!inherit_type_args.is_error ())
		{
		  // need to apply the inherited type arguments to the
		  // function
		  lookup = fn->handle_substitions (inherit_type_args);
		}
	    }
	}
    }

  // handle generics
  if (lookup->needs_generic_substitutions ())
    lookup = SubstMapper::InferSubst (lookup, expr.get_locus ());

  // type check the arguments if required
  TyTy::FnType *type = static_cast<TyTy::FnType *> (lookup);
  rust_assert (type->num_params () > 0);
  auto fnparam = type->param_at (0);

  // typecheck the self
  unify_site (expr.get_mappings ().get_hirid (),
	      TyTy::TyWithLocation (fnparam.second),
	      TyTy::TyWithLocation (adjusted_self), expr.get_locus ());
  if (rhs == nullptr)
    {
      rust_assert (type->num_params () == 1);
    }
  else
    {
      rust_assert (type->num_params () == 2);
      auto fnparam = type->param_at (1);
      unify_site (expr.get_mappings ().get_hirid (),
		  TyTy::TyWithLocation (fnparam.second),
		  TyTy::TyWithLocation (rhs), expr.get_locus ());
    }

  rust_assert (lookup->get_kind () == TyTy::TypeKind::FNDEF);
  fn = static_cast<TyTy::FnType *> (lookup);
  fn->monomorphize ();

  // get the return type
  TyTy::BaseType *function_ret_tyty
    = type->get_return_type ()->monomorphized_clone ();

  // store the expected fntype
  context->insert_operator_overload (expr.get_mappings ().get_hirid (), type);

  // set up the resolved name on the path
  resolver->insert_resolved_name (expr.get_mappings ().get_nodeid (),
				  resolved_node_id);

  // return the result of the function back
  infered = function_ret_tyty;

  return true;
}

HIR::PathIdentSegment
TypeCheckExpr::resolve_possible_fn_trait_call_method_name (
  const TyTy::BaseType &receiver)
{
  // Question
  // do we need to probe possible bounds here? I think not, i think when we
  // support Fn traits they are explicitly specified

  // FIXME
  // the logic to map the FnTrait to their respective call trait-item is
  // duplicated over in the backend/rust-compile-expr.cc
  for (const auto &bound : receiver.get_specified_bounds ())
    {
      bool found_fn = bound.get_name ().compare ("Fn") == 0;
      bool found_fn_mut = bound.get_name ().compare ("FnMut") == 0;
      bool found_fn_once = bound.get_name ().compare ("FnOnce") == 0;

      if (found_fn)
	{
	  return HIR::PathIdentSegment ("call");
	}
      else if (found_fn_mut)
	{
	  return HIR::PathIdentSegment ("call_mut");
	}
      else if (found_fn_once)
	{
	  return HIR::PathIdentSegment ("call_once");
	}
    }

  // nothing
  return HIR::PathIdentSegment ("");
}

bool
TypeCheckExpr::resolve_fn_trait_call (HIR::CallExpr &expr,
				      TyTy::BaseType *receiver_tyty,
				      TyTy::BaseType **result)
{
  // we turn this into a method call expr
  HIR::PathIdentSegment method_name
    = resolve_possible_fn_trait_call_method_name (*receiver_tyty);
  if (method_name.is_error ())
    return false;

  auto candidates = MethodResolver::Probe (receiver_tyty, method_name);
  if (candidates.empty ())
    return false;

  if (candidates.size () > 1)
    {
      RichLocation r (expr.get_locus ());
      for (auto &c : candidates)
	r.add_range (c.candidate.locus);

      rust_error_at (
	r, "multiple candidates found for function trait method call %<%s%>",
	method_name.as_string ().c_str ());
      return false;
    }

  if (receiver_tyty->get_kind () == TyTy::TypeKind::CLOSURE)
    {
      const TyTy::ClosureType &closure
	= static_cast<TyTy::ClosureType &> (*receiver_tyty);
      closure.setup_fn_once_output ();
    }

  auto candidate = *candidates.begin ();
  rust_debug_loc (expr.get_locus (),
		  "resolved call-expr to fn trait: {%u} {%s}",
		  candidate.candidate.ty->get_ref (),
		  candidate.candidate.ty->debug_str ().c_str ());

  // Get the adjusted self
  Adjuster adj (receiver_tyty);
  TyTy::BaseType *adjusted_self = adj.adjust_type (candidate.adjustments);

  // store the adjustments for code-generation to know what to do which must be
  // stored onto the receiver to so as we don't trigger duplicate deref mappings
  // ICE when an argument is a method call
  HirId autoderef_mappings_id = expr.get_mappings ().get_hirid ();
  context->insert_autoderef_mappings (autoderef_mappings_id,
				      std::move (candidate.adjustments));
  context->insert_receiver (expr.get_mappings ().get_hirid (), receiver_tyty);

  PathProbeCandidate &resolved_candidate = candidate.candidate;
  TyTy::BaseType *lookup_tyty = candidate.candidate.ty;
  NodeId resolved_node_id
    = resolved_candidate.is_impl_candidate ()
	? resolved_candidate.item.impl.impl_item->get_impl_mappings ()
	    .get_nodeid ()
	: resolved_candidate.item.trait.item_ref->get_mappings ().get_nodeid ();

  if (lookup_tyty->get_kind () != TyTy::TypeKind::FNDEF)
    {
      RichLocation r (expr.get_locus ());
      r.add_range (resolved_candidate.locus);
      rust_error_at (r, "associated impl item is not a method");
      return false;
    }

  TyTy::BaseType *lookup = lookup_tyty;
  TyTy::FnType *fn = static_cast<TyTy::FnType *> (lookup);
  if (!fn->is_method ())
    {
      RichLocation r (expr.get_locus ());
      r.add_range (resolved_candidate.locus);
      rust_error_at (r, "associated function is not a method");
      return false;
    }

  // fn traits only support tuple argument passing so we need to implicitly set
  // this up to get the same type checking we get in the rest of the pipeline

  std::vector<TyTy::TyVar> call_args;
  for (auto &arg : expr.get_arguments ())
    {
      TyTy::BaseType *a = TypeCheckExpr::Resolve (arg.get ());
      call_args.push_back (TyTy::TyVar (a->get_ref ()));
    }

  // crate implicit tuple
  HirId implicit_arg_id = mappings->get_next_hir_id ();
  Analysis::NodeMapping mapping (mappings->get_current_crate (), UNKNOWN_NODEID,
				 implicit_arg_id, UNKNOWN_LOCAL_DEFID);

  TyTy::TupleType *tuple
    = new TyTy::TupleType (implicit_arg_id, expr.get_locus (), call_args);
  context->insert_implicit_type (implicit_arg_id, tuple);

  std::vector<TyTy::Argument> args;
  TyTy::Argument a (mapping, tuple,
		    expr.get_locus () /*FIXME is there a better location*/);
  args.push_back (std::move (a));

  TyTy::BaseType *function_ret_tyty
    = TyTy::TypeCheckMethodCallExpr::go (fn, expr.get_mappings (), args,
					 expr.get_locus (), expr.get_locus (),
					 adjusted_self, context);
  if (function_ret_tyty == nullptr
      || function_ret_tyty->get_kind () == TyTy::TypeKind::ERROR)
    {
      rust_error_at (expr.get_locus (),
		     "failed check fn trait call-expr MethodCallExpr");
      return false;
    }

  // store the expected fntype
  context->insert_operator_overload (expr.get_mappings ().get_hirid (), fn);

  // set up the resolved name on the path
  resolver->insert_resolved_name (expr.get_mappings ().get_nodeid (),
				  resolved_node_id);

  // return the result of the function back
  *result = function_ret_tyty;

  return true;
}

bool
TypeCheckExpr::validate_arithmetic_type (
  const TyTy::BaseType *tyty, HIR::ArithmeticOrLogicalExpr::ExprType expr_type)
{
  const TyTy::BaseType *type = tyty->destructure ();

  // https://doc.rust-lang.org/reference/expressions/operator-expr.html#arithmetic-and-logical-binary-operators
  // this will change later when traits are added
  switch (expr_type)
    {
    case ArithmeticOrLogicalOperator::ADD:
    case ArithmeticOrLogicalOperator::SUBTRACT:
    case ArithmeticOrLogicalOperator::MULTIPLY:
    case ArithmeticOrLogicalOperator::DIVIDE:
    case ArithmeticOrLogicalOperator::MODULUS:
      return (type->get_kind () == TyTy::TypeKind::INT)
	     || (type->get_kind () == TyTy::TypeKind::UINT)
	     || (type->get_kind () == TyTy::TypeKind::FLOAT)
	     || (type->get_kind () == TyTy::TypeKind::USIZE)
	     || (type->get_kind () == TyTy::TypeKind::ISIZE)
	     || (type->get_kind () == TyTy::TypeKind::INFER
		 && (((const TyTy::InferType *) type)->get_infer_kind ()
		     == TyTy::InferType::INTEGRAL))
	     || (type->get_kind () == TyTy::TypeKind::INFER
		 && (((const TyTy::InferType *) type)->get_infer_kind ()
		     == TyTy::InferType::FLOAT));

      // integers or bools
    case ArithmeticOrLogicalOperator::BITWISE_AND:
    case ArithmeticOrLogicalOperator::BITWISE_OR:
    case ArithmeticOrLogicalOperator::BITWISE_XOR:
      return (type->get_kind () == TyTy::TypeKind::INT)
	     || (type->get_kind () == TyTy::TypeKind::UINT)
	     || (type->get_kind () == TyTy::TypeKind::USIZE)
	     || (type->get_kind () == TyTy::TypeKind::ISIZE)
	     || (type->get_kind () == TyTy::TypeKind::BOOL)
	     || (type->get_kind () == TyTy::TypeKind::INFER
		 && (((const TyTy::InferType *) type)->get_infer_kind ()
		     == TyTy::InferType::INTEGRAL));

      // integers only
    case ArithmeticOrLogicalOperator::LEFT_SHIFT:
    case ArithmeticOrLogicalOperator::RIGHT_SHIFT:
      return (type->get_kind () == TyTy::TypeKind::INT)
	     || (type->get_kind () == TyTy::TypeKind::UINT)
	     || (type->get_kind () == TyTy::TypeKind::USIZE)
	     || (type->get_kind () == TyTy::TypeKind::ISIZE)
	     || (type->get_kind () == TyTy::TypeKind::INFER
		 && (((const TyTy::InferType *) type)->get_infer_kind ()
		     == TyTy::InferType::INTEGRAL));
    }

  gcc_unreachable ();
  return false;
}

} // namespace Resolver
} // namespace Rust
