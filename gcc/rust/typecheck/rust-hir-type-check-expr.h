// Copyright (C) 2020 Free Software Foundation, Inc.

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

#ifndef RUST_HIR_TYPE_CHECK_EXPR
#define RUST_HIR_TYPE_CHECK_EXPR

#include "rust-hir-type-check-base.h"
#include "rust-hir-full.h"
#include "rust-tyty.h"
#include "rust-tyty-call.h"
#include "rust-hir-type-check-struct-field.h"
#include "rust-hir-method-resolve.h"

namespace Rust {
namespace Resolver {

class TypeCheckExpr : public TypeCheckBase
{
public:
  static TyTy::BaseType *Resolve (HIR::Expr *expr, bool inside_loop)
  {
    TypeCheckExpr resolver (inside_loop);
    expr->accept_vis (resolver);

    if (resolver.infered == nullptr)
      {
	rust_error_at (expr->get_locus_slow (),
		       "failed to type resolve expression");
	return new TyTy::ErrorType (expr->get_mappings ().get_hirid ());
      }

    auto ref = expr->get_mappings ().get_hirid ();
    resolver.infered->set_ref (ref);
    resolver.context->insert_type (expr->get_mappings (), resolver.infered);

    return resolver.infered;
  }

  void visit (HIR::TupleIndexExpr &expr)
  {
    auto resolved
      = TypeCheckExpr::Resolve (expr.get_tuple_expr ().get (), inside_loop);
    if (resolved == nullptr)
      {
	rust_error_at (expr.get_tuple_expr ()->get_locus_slow (),
		       "failed to resolve TupleIndexExpr receiver");
	return;
      }

    bool is_valid_type = resolved->get_kind () == TyTy::TypeKind::ADT
			 || resolved->get_kind () == TyTy::TypeKind::TUPLE;
    if (!is_valid_type)
      {
	rust_error_at (expr.get_tuple_expr ()->get_locus_slow (),
		       "Expected Tuple or ADT got: %s",
		       resolved->as_string ().c_str ());
	return;
      }

    if (resolved->get_kind () == TyTy::TypeKind::TUPLE)
      {
	TyTy::TupleType *tuple = (TyTy::TupleType *) resolved;
	TupleIndex index = expr.get_tuple_index ();
	if ((size_t) index >= tuple->num_fields ())
	  {
	    rust_error_at (expr.get_locus (), "unknown field at index %i",
			   index);
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

    TyTy::ADTType *adt = (TyTy::ADTType *) resolved;
    TupleIndex index = expr.get_tuple_index ();
    if ((size_t) index >= adt->num_fields ())
      {
	rust_error_at (expr.get_locus (), "unknown field at index %i", index);
	return;
      }

    auto field_tyty = adt->get_field ((size_t) index);
    if (field_tyty == nullptr)
      {
	rust_error_at (expr.get_locus (),
		       "failed to lookup field type at index %i", index);
	return;
      }

    infered = field_tyty->get_field_type ();
  }

  void visit (HIR::TupleExpr &expr)
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

    std::vector<HirId> fields;
    for (auto &elem : expr.get_tuple_elems ())
      {
	auto field_ty = TypeCheckExpr::Resolve (elem.get (), false);
	fields.push_back (field_ty->get_ref ());
      }
    infered = new TyTy::TupleType (expr.get_mappings ().get_hirid (), fields);
  }

  void visit (HIR::ReturnExpr &expr)
  {
    if (!expr.has_return_expr ())
      {
	infered = new TyTy::UnitType (expr.get_mappings ().get_hirid ());
	return;
      }

    auto fn_return_tyty = context->peek_return_type ();
    rust_assert (fn_return_tyty != nullptr);

    auto expr_ty = TypeCheckExpr::Resolve (expr.get_expr (), false);
    if (expr_ty == nullptr)
      {
	rust_error_at (expr.get_locus (),
		       "failed to resolve type for ReturnExpr");
	return;
      }

    infered = fn_return_tyty->unify (expr_ty);
    fn_return_tyty->append_reference (expr_ty->get_ref ());
    for (auto &ref : infered->get_combined_refs ())
      fn_return_tyty->append_reference (ref);
  }

  void visit (HIR::CallExpr &expr)
  {
    TyTy::BaseType *function_tyty
      = TypeCheckExpr::Resolve (expr.get_fnexpr (), false);
    if (function_tyty == nullptr)
      return;

    bool valid_tyty = function_tyty->get_kind () == TyTy::TypeKind::ADT
		      || function_tyty->get_kind () == TyTy::TypeKind::FNDEF;
    if (!valid_tyty)
      {
	rust_error_at (expr.get_locus (),
		       "Failed to resolve expression of function call");
	return;
      }

    infered = TyTy::TypeCheckCallExpr::go (function_tyty, expr, context);
    if (infered == nullptr)
      {
	rust_error_at (expr.get_locus (), "failed to lookup type to CallExpr");
	return;
      }

    infered->set_ref (expr.get_mappings ().get_hirid ());
  }

  void visit (HIR::MethodCallExpr &expr)
  {
    auto receiver_tyty
      = TypeCheckExpr::Resolve (expr.get_receiver ().get (), false);
    if (receiver_tyty == nullptr)
      {
	rust_error_at (expr.get_receiver ()->get_locus_slow (),
		       "failed to resolve receiver in MethodCallExpr");
	return;
      }

    // https://doc.rust-lang.org/reference/expressions/method-call-expr.html
    // method resolution is complex in rust once we start handling generics and
    // traits. For now we only support looking up the valid name in impl blocks
    // which is simple. There will need to be adjustments to ensure we can turn
    // the receiver into borrowed references etc

    auto probes
      = MethodResolution::Probe (receiver_tyty, expr.get_method_name ());
    if (probes.size () == 0)
      {
	rust_error_at (expr.get_locus (),
		       "failed to resolve the PathExprSegment to any Method");
	return;
      }
    else if (probes.size () > 1)
      {
	rust_error_at (
	  expr.get_locus (),
	  "Generics and Traits are not implemented yet for MethodCall");
	return;
      }

    auto resolved_method = probes.at (0);
    TyTy::BaseType *lookup;
    if (!context->lookup_type (resolved_method->get_mappings ().get_hirid (),
			       &lookup))
      {
	rust_error_at (resolved_method->get_locus (),
		       "failed to lookup type for CallExpr: %s",
		       expr.as_string ().c_str ());
	return;
      }

    infered = TyTy::TypeCheckMethodCallExpr::go (lookup, expr, context);
    if (infered == nullptr)
      {
	rust_error_at (expr.get_locus (),
		       "failed to lookup type to MethodCallExpr");
	return;
      }

    infered->set_ref (expr.get_mappings ().get_hirid ());

    // set up the resolved name on the path
    resolver->insert_resolved_name (
      expr.get_mappings ().get_nodeid (),
      resolved_method->get_mappings ().get_nodeid ());
  }

  void visit (HIR::AssignmentExpr &expr)
  {
    infered = new TyTy::UnitType (expr.get_mappings ().get_hirid ());

    auto lhs = TypeCheckExpr::Resolve (expr.get_lhs (), false);
    auto rhs = TypeCheckExpr::Resolve (expr.get_rhs (), false);

    auto result = lhs->unify (rhs);
    if (result->get_kind () == TyTy::TypeKind::ERROR)
      {
	rust_error_at (expr.get_locus (),
		       "type resolution failure in AssignmentExpr");
	return;
      }

    // in the case of declare first for an ADT Type:
    //
    // let a;
    // a = Foo{..}
    // let b = a.field;
    //
    // The lhs will have a TyTy of INFER and so when the declaration is
    // referenced it will still have an unknown type so we will fail to resolve
    // FieldAccessExpr

    NodeId ast_node_id = expr.get_lhs ()->get_mappings ().get_nodeid ();
    NodeId ref_node_id;
    if (!resolver->lookup_resolved_name (ast_node_id, &ref_node_id))
      return;

    Definition def;
    if (!resolver->lookup_definition (ref_node_id, &def))
      {
	rust_error_at (expr.get_locus (),
		       "assignment infer - unknown reference");
	return;
      }

    HirId ref;
    if (!mappings->lookup_node_to_hir (expr.get_mappings ().get_crate_num (),
				       def.parent, &ref))
      {
	rust_error_at (expr.get_locus (),
		       "assignment infer - reverse lookup failure");
	return;
      }

    context->insert_type (
      Analysis::NodeMapping (expr.get_lhs ()->get_mappings ().get_crate_num (),
			     ref_node_id, ref, UNKNOWN_LOCAL_DEFID),
      result->clone ());
  }

  void visit (HIR::IdentifierExpr &expr)
  {
    NodeId ast_node_id = expr.get_mappings ().get_nodeid ();

    // then lookup the reference_node_id
    NodeId ref_node_id = UNKNOWN_NODEID;
    if (resolver->lookup_resolved_name (ast_node_id, &ref_node_id))
      {
	// these ref_node_ids will resolve to a pattern declaration but we are
	// interested in the definition that this refers to get the parent id
	Definition def;
	if (!resolver->lookup_definition (ref_node_id, &def))
	  {
	    rust_error_at (expr.get_locus (),
			   "unknown reference for resolved name");
	    return;
	  }
	ref_node_id = def.parent;
      }
    else if (!resolver->lookup_resolved_type (ast_node_id, &ref_node_id))
      {
	rust_error_at (expr.get_locus (),
		       "Failed to lookup type reference for node: %s",
		       expr.as_string ().c_str ());
	return;
      }

    if (ref_node_id == UNKNOWN_NODEID)
      {
	rust_error_at (expr.get_locus (), "unresolved node: %s",
		       expr.as_string ().c_str ());
	return;
      }

    // node back to HIR
    HirId ref;
    if (!mappings->lookup_node_to_hir (expr.get_mappings ().get_crate_num (),
				       ref_node_id, &ref))
      {
	rust_error_at (expr.get_locus (), "reverse lookup failure");
	return;
      }

    // the base reference for this name _must_ have a type set
    TyTy::BaseType *lookup;
    if (!context->lookup_type (ref, &lookup))
      {
	rust_error_at (mappings->lookup_location (ref),
		       "Failed to resolve IdentifierExpr type: %s",
		       expr.as_string ().c_str ());
	return;
      }

    lookup->append_reference (lookup->get_ref ());
    infered = lookup->clone ();
    infered->set_ref (expr.get_mappings ().get_hirid ());
  }

  void visit (HIR::LiteralExpr &expr)
  {
    switch (expr.get_lit_type ())
      {
	case HIR::Literal::LitType::INT: {
	  bool ok = false;

	  switch (expr.get_literal ()->get_type_hint ())
	    {
	    case CORETYPE_I8:
	      ok = context->lookup_builtin ("i8", &infered);
	      break;
	    case CORETYPE_I16:
	      ok = context->lookup_builtin ("i16", &infered);
	      break;
	    case CORETYPE_I32:
	      ok = context->lookup_builtin ("i32", &infered);
	      break;
	    case CORETYPE_I64:
	      ok = context->lookup_builtin ("i64", &infered);
	      break;
	    case CORETYPE_I128:
	      ok = context->lookup_builtin ("i128", &infered);
	      break;

	    case CORETYPE_U8:
	      ok = context->lookup_builtin ("u8", &infered);
	      break;
	    case CORETYPE_U16:
	      ok = context->lookup_builtin ("u16", &infered);
	      break;
	    case CORETYPE_U32:
	      ok = context->lookup_builtin ("u32", &infered);
	      break;
	    case CORETYPE_U64:
	      ok = context->lookup_builtin ("u64", &infered);
	      break;
	    case CORETYPE_U128:
	      ok = context->lookup_builtin ("u128", &infered);
	      break;

	    case CORETYPE_F32:
	      expr.get_literal ()->set_lit_type (HIR::Literal::LitType::FLOAT);
	      ok = context->lookup_builtin ("f32", &infered);
	      break;
	    case CORETYPE_F64:
	      expr.get_literal ()->set_lit_type (HIR::Literal::LitType::FLOAT);
	      ok = context->lookup_builtin ("f64", &infered);
	      break;

	    default:
	      ok = true;
	      infered = new TyTy::InferType (
		expr.get_mappings ().get_hirid (),
		TyTy::InferType::InferTypeKind::INTEGRAL);
	      break;
	    }
	  rust_assert (ok);
	}
	break;

	case HIR::Literal::LitType::FLOAT: {
	  bool ok = false;

	  switch (expr.get_literal ()->get_type_hint ())
	    {
	    case CORETYPE_F32:
	      ok = context->lookup_builtin ("f32", &infered);
	      break;
	    case CORETYPE_F64:
	      ok = context->lookup_builtin ("f64", &infered);
	      break;

	    default:
	      ok = true;
	      infered
		= new TyTy::InferType (expr.get_mappings ().get_hirid (),
				       TyTy::InferType::InferTypeKind::FLOAT);
	      break;
	    }
	  rust_assert (ok);
	}
	break;

	case HIR::Literal::LitType::BOOL: {
	  auto ok = context->lookup_builtin ("bool", &infered);
	  rust_assert (ok);
	}
	break;

	case HIR::Literal::LitType::CHAR: {
	  auto ok = context->lookup_builtin ("char", &infered);
	  rust_assert (ok);
	}
	break;

	case HIR::Literal::LitType::STRING: {
	  TyTy::BaseType *base = nullptr;
	  auto ok = context->lookup_builtin ("str", &base);
	  rust_assert (ok);

	  infered = new TyTy::ReferenceType (expr.get_mappings ().get_hirid (),
					     base->get_ref ());
	}
	break;

      default:
	gcc_unreachable ();
	break;
      }

    infered = infered->clone ();
  }

  void visit (HIR::ArithmeticOrLogicalExpr &expr)
  {
    auto lhs = TypeCheckExpr::Resolve (expr.get_lhs (), false);
    auto rhs = TypeCheckExpr::Resolve (expr.get_rhs (), false);

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

    infered = lhs->unify (rhs);
    infered->append_reference (lhs->get_ref ());
    infered->append_reference (rhs->get_ref ());
  }

  void visit (HIR::ComparisonExpr &expr)
  {
    auto lhs = TypeCheckExpr::Resolve (expr.get_lhs (), false);
    auto rhs = TypeCheckExpr::Resolve (expr.get_rhs (), false);

    auto result = lhs->unify (rhs);
    if (result == nullptr || result->get_kind () == TyTy::TypeKind::ERROR)
      return;

    // we expect this to be
    infered = new TyTy::BoolType (expr.get_mappings ().get_hirid ());
    infered->append_reference (lhs->get_ref ());
    infered->append_reference (rhs->get_ref ());
  }

  void visit (HIR::LazyBooleanExpr &expr)
  {
    auto lhs = TypeCheckExpr::Resolve (expr.get_lhs (), false);
    auto rhs = TypeCheckExpr::Resolve (expr.get_rhs (), false);

    // we expect the lhs and rhs must be bools at this point
    TyTy::BoolType elhs (expr.get_mappings ().get_hirid ());
    lhs = elhs.unify (lhs);
    if (lhs == nullptr || lhs->get_kind () == TyTy::TypeKind::ERROR)
      return;

    TyTy::BoolType rlhs (expr.get_mappings ().get_hirid ());
    rhs = elhs.unify (rhs);
    if (lhs == nullptr || lhs->get_kind () == TyTy::TypeKind::ERROR)
      return;

    infered = lhs->unify (rhs);
    infered->append_reference (lhs->get_ref ());
    infered->append_reference (rhs->get_ref ());
  }

  void visit (HIR::NegationExpr &expr)
  {
    auto negated_expr_ty = TypeCheckExpr::Resolve (expr.get_expr (), false);

    // https://doc.rust-lang.org/reference/expressions/operator-expr.html#negation-operators
    switch (expr.get_expr_type ())
      {
	case NegationOperator::NEGATE: {
	  bool valid
	    = (negated_expr_ty->get_kind () == TyTy::TypeKind::INT)
	      || (negated_expr_ty->get_kind () == TyTy::TypeKind::UINT)
	      || (negated_expr_ty->get_kind () == TyTy::TypeKind::FLOAT)
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
	      rust_error_at (expr.get_locus (), "cannot apply unary ! to %s",
			     negated_expr_ty->as_string ().c_str ());
	      return;
	    }
	}
	break;
      }

    infered = negated_expr_ty->clone ();
    infered->append_reference (negated_expr_ty->get_ref ());
  }

  void visit (HIR::IfExpr &expr)
  {
    TypeCheckExpr::Resolve (expr.get_if_condition (), false);
    TypeCheckExpr::Resolve (expr.get_if_block (), inside_loop);

    infered = new TyTy::UnitType (expr.get_mappings ().get_hirid ());
  }

  void visit (HIR::IfExprConseqElse &expr)
  {
    TypeCheckExpr::Resolve (expr.get_if_condition (), false);
    auto if_blk_resolved
      = TypeCheckExpr::Resolve (expr.get_if_block (), inside_loop);
    auto else_blk_resolved
      = TypeCheckExpr::Resolve (expr.get_else_block (), inside_loop);

    infered = if_blk_resolved->unify (else_blk_resolved);
  }

  void visit (HIR::IfExprConseqIf &expr)
  {
    TypeCheckExpr::Resolve (expr.get_if_condition (), false);
    auto if_blk = TypeCheckExpr::Resolve (expr.get_if_block (), inside_loop);
    auto else_blk
      = TypeCheckExpr::Resolve (expr.get_conseq_if_expr (), inside_loop);

    infered = if_blk->unify (else_blk);
  }

  void visit (HIR::BlockExpr &expr);

  void visit (HIR::ArrayIndexExpr &expr)
  {
    TyTy::BaseType *size_ty;
    if (!context->lookup_builtin ("usize", &size_ty))
      {
	rust_error_at (
	  expr.get_locus (),
	  "Failure looking up size type for index in ArrayIndexExpr");
	return;
      }

    auto resolved_index_expr
      = size_ty->unify (TypeCheckExpr::Resolve (expr.get_index_expr (), false));
    if (resolved_index_expr == nullptr)
      {
	rust_error_at (expr.get_index_expr ()->get_locus_slow (),
		       "Type Resolver failure in Index for ArrayIndexExpr");
	return;
      }
    context->insert_type (expr.get_index_expr ()->get_mappings (),
			  resolved_index_expr);

    // resolve the array reference
    expr.get_array_expr ()->accept_vis (*this);
    if (infered == nullptr)
      {
	rust_error_at (expr.get_index_expr ()->get_locus_slow (),
		       "failed to resolve array reference expression");
	return;
      }
    else if (infered->get_kind () != TyTy::TypeKind::ARRAY)
      {
	rust_error_at (expr.get_index_expr ()->get_locus_slow (),
		       "expected an ArrayType got [%s]",
		       infered->as_string ().c_str ());
	infered = nullptr;
	return;
      }

    TyTy::ArrayType *array_type = (TyTy::ArrayType *) infered;
    infered = array_type->get_type ()->clone ();
  }

  void visit (HIR::ArrayExpr &expr)
  {
    HIR::ArrayElems *elements = expr.get_internal_elements ();
    size_t num_elems = elements->get_num_elements ();

    elements->accept_vis (*this);
    rust_assert (infered_array_elems != nullptr);

    infered = new TyTy::ArrayType (expr.get_mappings ().get_hirid (), num_elems,
				   infered_array_elems);
  }

  void visit (HIR::ArrayElemsValues &elems)
  {
    std::vector<TyTy::BaseType *> types;
    elems.iterate ([&] (HIR::Expr *e) mutable -> bool {
      types.push_back (TypeCheckExpr::Resolve (e, false));
      return true;
    });

    infered_array_elems = types[0];
    for (size_t i = 1; i < types.size (); i++)
      {
	infered_array_elems = infered_array_elems->unify (types.at (i));
      }

    for (auto &elem : types)
      infered_array_elems->append_reference (elem->get_ref ());
  }

  void visit (HIR::ArrayElemsCopied &elems)
  {
    infered_array_elems
      = TypeCheckExpr::Resolve (elems.get_elem_to_copy (), false);
  }

  void visit (HIR::StructExprStructFields &struct_expr)
  {
    infered = TypeCheckStructExpr::Resolve (&struct_expr);
  }

  void visit (HIR::GroupedExpr &expr)
  {
    infered = TypeCheckExpr::Resolve (expr.get_expr_in_parens ().get (), false);
  }

  void visit (HIR::FieldAccessExpr &expr)
  {
    auto struct_base
      = TypeCheckExpr::Resolve (expr.get_receiver_expr ().get (), false);

    bool is_valid_type = struct_base->get_kind () == TyTy::TypeKind::ADT;
    if (!is_valid_type)
      {
	rust_error_at (expr.get_locus (),
		       "expected algebraic data type got: [%s]",
		       struct_base->as_string ().c_str ());
	return;
      }

    TyTy::ADTType *adt = (TyTy::ADTType *) struct_base;
    auto resolved = adt->get_field (expr.get_field_name ());
    if (resolved == nullptr)
      {
	rust_error_at (expr.get_locus (), "unknown field [%s] for type [%s]",
		       expr.get_field_name ().c_str (),
		       adt->as_string ().c_str ());
	return;
      }

    infered = resolved->get_field_type ();
  }

  void visit (HIR::PathInExpression &expr)
  {
    NodeId ast_node_id = expr.get_mappings ().get_nodeid ();

    // then lookup the reference_node_id
    NodeId ref_node_id = UNKNOWN_NODEID;
    if (resolver->lookup_resolved_name (ast_node_id, &ref_node_id))
      {
	// these ref_node_ids will resolve to a pattern declaration but we are
	// interested in the definition that this refers to get the parent id
	Definition def;
	if (!resolver->lookup_definition (ref_node_id, &def))
	  {
	    rust_error_at (expr.get_locus (),
			   "unknown reference for resolved name");
	    return;
	  }
	ref_node_id = def.parent;
      }
    else if (!resolver->lookup_resolved_type (ast_node_id, &ref_node_id))
      {
	rust_error_at (expr.get_locus (),
		       "Failed to lookup type reference for node: %s",
		       expr.as_string ().c_str ());
	return;
      }

    if (ref_node_id == UNKNOWN_NODEID)
      {
	rust_error_at (expr.get_locus (), "unresolved node: %s",
		       expr.as_string ().c_str ());
	return;
      }

    // node back to HIR
    HirId ref;
    if (!mappings->lookup_node_to_hir (expr.get_mappings ().get_crate_num (),
				       ref_node_id, &ref))
      {
	rust_error_at (expr.get_locus (), "reverse lookup failure");
	return;
      }

    if (!context->lookup_type (ref, &infered))
      {
	rust_error_at (expr.get_locus (),
		       "failed to resolve PathInExpression type");
	return;
      }

    HIR::PathExprSegment seg = expr.get_final_segment ();
    if (!infered->supports_substitions () && seg.has_generic_args ())
      {
	rust_error_at (expr.get_locus (),
		       "path does not support substitutions");
	return;
      }

    if (infered->has_subsititions_defined ())
      {
	if (infered->get_kind () != TyTy::TypeKind::ADT)
	  {
	    rust_error_at (expr.get_locus (),
			   "substitutions only support on ADT types so far");
	    return;
	  }

	TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (infered);
	infered = seg.has_generic_args ()
		    ? adt->handle_substitions (seg.get_generic_args ())
		    : adt->infer_substitions ();
      }
  }

  void visit (HIR::LoopExpr &expr)
  {
    context->push_new_loop_context (expr.get_mappings ().get_hirid ());
    TyTy::BaseType *block_expr
      = TypeCheckExpr::Resolve (expr.get_loop_block ().get (), true);
    if (block_expr->get_kind () != TyTy::TypeKind::UNIT)
      {
	rust_error_at (expr.get_loop_block ()->get_locus_slow (),
		       "expected () got %s", block_expr->as_string ().c_str ());
	return;
      }

    TyTy::BaseType *loop_context_type = context->pop_loop_context ();

    bool loop_context_type_infered
      = (loop_context_type->get_kind () != TyTy::TypeKind::INFER)
	|| ((loop_context_type->get_kind () == TyTy::TypeKind::INFER)
	    && (((TyTy::InferType *) loop_context_type)->get_infer_kind ()
		!= TyTy::InferType::GENERAL));

    infered = loop_context_type_infered
		? loop_context_type
		: new TyTy::UnitType (expr.get_mappings ().get_hirid ());
  }

  void visit (HIR::WhileLoopExpr &expr)
  {
    context->push_new_while_loop_context (expr.get_mappings ().get_hirid ());

    TypeCheckExpr::Resolve (expr.get_predicate_expr ().get (), false);
    TyTy::BaseType *block_expr
      = TypeCheckExpr::Resolve (expr.get_loop_block ().get (), true);

    if (block_expr->get_kind () != TyTy::TypeKind::UNIT)
      {
	rust_error_at (expr.get_loop_block ()->get_locus_slow (),
		       "expected () got %s", block_expr->as_string ().c_str ());
	return;
      }

    context->pop_loop_context ();
    infered = new TyTy::UnitType (expr.get_mappings ().get_hirid ());
  }

  void visit (HIR::BreakExpr &expr)
  {
    if (!inside_loop)
      {
	rust_error_at (expr.get_locus (), "cannot `break` outside of a loop");
	return;
      }

    if (expr.has_break_expr ())
      {
	TyTy::BaseType *break_expr_tyty
	  = TypeCheckExpr::Resolve (expr.get_expr ().get (), false);

	TyTy::BaseType *loop_context = context->peek_loop_context ();
	if (loop_context->get_kind () == TyTy::TypeKind::ERROR)
	  {
	    rust_error_at (expr.get_locus (),
			   "can only break with a value inside `loop`");
	    return;
	  }

	TyTy::BaseType *unified_ty = loop_context->unify (break_expr_tyty);
	context->swap_head_loop_context (unified_ty);
      }

    infered = new TyTy::UnitType (expr.get_mappings ().get_hirid ());
  }

  void visit (HIR::ContinueExpr &expr)
  {
    if (!inside_loop)
      {
	rust_error_at (expr.get_locus (),
		       "cannot `continue` outside of a loop");
	return;
      }

    infered = new TyTy::UnitType (expr.get_mappings ().get_hirid ());
  }

  void visit (HIR::BorrowExpr &expr)
  {
    TyTy::BaseType *resolved_base
      = TypeCheckExpr::Resolve (expr.get_expr ().get (), false);

    // FIXME double_reference

    infered = new TyTy::ReferenceType (expr.get_mappings ().get_hirid (),
				       resolved_base->get_ref ());
  }

  void visit (HIR::DereferenceExpr &expr)
  {
    TyTy::BaseType *resolved_base
      = TypeCheckExpr::Resolve (expr.get_expr ().get (), false);
    if (resolved_base->get_kind () != TyTy::TypeKind::REF)
      {
	rust_error_at (expr.get_locus (), "expected reference type got %s",
		       resolved_base->as_string ().c_str ());
	return;
      }

    TyTy::ReferenceType *ref_base = (TyTy::ReferenceType *) resolved_base;
    infered = ref_base->get_base ()->clone ();
  }

private:
  TypeCheckExpr (bool inside_loop)
    : TypeCheckBase (), infered (nullptr), infered_array_elems (nullptr),
      inside_loop (inside_loop)
  {}

  bool
  validate_arithmetic_type (TyTy::BaseType *type,
			    HIR::ArithmeticOrLogicalExpr::ExprType expr_type)
  {
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
	       || (type->get_kind () == TyTy::TypeKind::INFER
		   && (((TyTy::InferType *) type)->get_infer_kind ()
		       == TyTy::InferType::INTEGRAL))
	       || (type->get_kind () == TyTy::TypeKind::INFER
		   && (((TyTy::InferType *) type)->get_infer_kind ()
		       == TyTy::InferType::FLOAT));

	// integers or bools
      case ArithmeticOrLogicalOperator::BITWISE_AND:
      case ArithmeticOrLogicalOperator::BITWISE_OR:
      case ArithmeticOrLogicalOperator::BITWISE_XOR:
	return (type->get_kind () == TyTy::TypeKind::INT)
	       || (type->get_kind () == TyTy::TypeKind::UINT)
	       || (type->get_kind () == TyTy::TypeKind::BOOL)
	       || (type->get_kind () == TyTy::TypeKind::INFER
		   && (((TyTy::InferType *) type)->get_infer_kind ()
		       == TyTy::InferType::INTEGRAL));

	// integers only
      case ArithmeticOrLogicalOperator::LEFT_SHIFT:
      case ArithmeticOrLogicalOperator::RIGHT_SHIFT:
	return (type->get_kind () == TyTy::TypeKind::INT)
	       || (type->get_kind () == TyTy::TypeKind::UINT)
	       || (type->get_kind () == TyTy::TypeKind::INFER
		   && (((TyTy::InferType *) type)->get_infer_kind ()
		       == TyTy::InferType::INTEGRAL));
      }
    gcc_unreachable ();
  }

  TyTy::BaseType *infered;
  TyTy::BaseType *infered_array_elems;

  bool inside_loop;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_EXPR
