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

namespace Rust {
namespace Resolver {

class TypeCheckExpr : public TypeCheckBase
{
public:
  static TyTy::TyBase *Resolve (HIR::Expr *expr, bool is_final_expr = false)
  {
    TypeCheckExpr resolver (is_final_expr);
    expr->accept_vis (resolver);

    if (resolver.infered == nullptr)
      {
	resolver.infered
	  = new TyTy::UnitType (expr->get_mappings ().get_hirid ());
      }

    auto ref = expr->get_mappings ().get_hirid ();
    resolver.infered->set_ref (ref);
    resolver.context->insert_type (expr->get_mappings (), resolver.infered);

    return resolver.infered;
  }

  void visit (HIR::TupleIndexExpr &expr)
  {
    auto resolved = TypeCheckExpr::Resolve (expr.get_tuple_expr ().get ());
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
	auto field_ty = TypeCheckExpr::Resolve (elem.get ());
	fields.push_back (field_ty->get_ref ());
      }
    infered = new TyTy::TupleType (expr.get_mappings ().get_hirid (), fields);
  }

  void visit (HIR::ReturnExpr &expr)
  {
    auto fn_return_tyty = context->peek_return_type ();
    rust_assert (fn_return_tyty != nullptr);

    auto expr_ty = TypeCheckExpr::Resolve (expr.get_expr ());
    if (expr_ty == nullptr)
      {
	rust_error_at (expr.get_locus (),
		       "failed to resolve type for ReturnExpr");
	return;
      }

    infered = fn_return_tyty->combine (expr_ty);
    fn_return_tyty->append_reference (expr_ty->get_ref ());
    for (auto &ref : infered->get_combined_refs ())
      fn_return_tyty->append_reference (ref);
  }

  void visit (HIR::CallExpr &expr)
  {
    auto fn = expr.get_fnexpr ();
    auto fn_node_id = fn->get_mappings ().get_nodeid ();

    // then lookup the reference_node_id
    NodeId ref_node_id = UNKNOWN_NODEID;
    if (resolver->lookup_resolved_name (fn_node_id, &ref_node_id))
      {
	Definition def;
	if (!resolver->lookup_definition (ref_node_id, &def))
	  {
	    rust_error_at (expr.get_locus (),
			   "unknown reference for resolved name");
	    return;
	  }
	ref_node_id = def.parent;
      }
    else if (!resolver->lookup_resolved_type (fn_node_id, &ref_node_id))
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
	rust_error_at (expr.get_locus (), "reverse lookup failure for node %u",
		       ref_node_id);
	return;
      }

    // check if this has a type
    TyTy::TyBase *lookup;
    if (!context->lookup_type (ref, &lookup))
      {
	// FIXME we need to be able to lookup the location info for the
	// reference here
	rust_error_at (expr.get_locus (), "consider giving this a type: %s",
		       expr.as_string ().c_str ());
	return;
      }

    infered = TyTy::TypeCheckCallExpr::go (lookup, expr, context);
    if (infered == nullptr)
      {
	rust_error_at (expr.get_locus (), "failed to lookup type to CallExpr");
	return;
      }

    infered->set_ref (expr.get_mappings ().get_hirid ());
  }

  void visit (HIR::AssignmentExpr &expr)
  {
    auto lhs = TypeCheckExpr::Resolve (expr.get_lhs ());
    auto rhs = TypeCheckExpr::Resolve (expr.get_rhs ());

    infered = lhs->combine (rhs);
    if (infered == nullptr)
      {
	rust_error_at (expr.get_locus (),
		       "failure in TypeInference AssignmentExpr");
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

    // FIXME free the old one
    context->insert_type (
      Analysis::NodeMapping (expr.get_lhs ()->get_mappings ().get_crate_num (),
			     ref_node_id, ref, UNKNOWN_LOCAL_DEFID),
      infered->clone ());
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
    TyTy::TyBase *lookup;
    if (!context->lookup_type (ref, &lookup))
      {
	rust_error_at (mappings->lookup_location (ref),
		       "consider giving this a type: %s",
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

      default:
	gcc_unreachable ();
	break;
      }

    infered = infered->clone ();
  }

  void visit (HIR::ArithmeticOrLogicalExpr &expr)
  {
    auto lhs = TypeCheckExpr::Resolve (expr.get_lhs ());
    auto rhs = TypeCheckExpr::Resolve (expr.get_rhs ());
    auto combined = lhs->combine (rhs);

    // https://doc.rust-lang.org/reference/expressions/operator-expr.html#arithmetic-and-logical-binary-operators
    switch (expr.get_expr_type ())
      {
	// integers or floats
      case HIR::ArithmeticOrLogicalExpr::ADD:
      case HIR::ArithmeticOrLogicalExpr::SUBTRACT:
      case HIR::ArithmeticOrLogicalExpr::MULTIPLY:
      case HIR::ArithmeticOrLogicalExpr::DIVIDE:
	case HIR::ArithmeticOrLogicalExpr::MODULUS: {
	  bool valid = (combined->get_kind () == TyTy::TypeKind::INT)
		       || (combined->get_kind () == TyTy::TypeKind::UINT)
		       || (combined->get_kind () == TyTy::TypeKind::FLOAT)
		       || (combined->get_kind () == TyTy::TypeKind::INFER
			   && (((TyTy::InferType *) combined)->get_infer_kind ()
			       == TyTy::InferType::INTEGRAL))
		       || (combined->get_kind () == TyTy::TypeKind::INFER
			   && (((TyTy::InferType *) combined)->get_infer_kind ()
			       == TyTy::InferType::FLOAT));
	  if (!valid)
	    {
	      rust_error_at (expr.get_locus (), "cannot apply operator to %s",
			     combined->as_string ().c_str ());
	      return;
	    }
	}
	break;

	// integers or bools
      case HIR::ArithmeticOrLogicalExpr::BITWISE_AND:
      case HIR::ArithmeticOrLogicalExpr::BITWISE_OR:
	case HIR::ArithmeticOrLogicalExpr::BITWISE_XOR: {
	  bool valid = (combined->get_kind () == TyTy::TypeKind::INT)
		       || (combined->get_kind () == TyTy::TypeKind::UINT)
		       || (combined->get_kind () == TyTy::TypeKind::BOOL)
		       || (combined->get_kind () == TyTy::TypeKind::INFER
			   && (((TyTy::InferType *) combined)->get_infer_kind ()
			       == TyTy::InferType::INTEGRAL));
	  if (!valid)
	    {
	      rust_error_at (expr.get_locus (), "cannot apply operator to %s",
			     combined->as_string ().c_str ());
	      return;
	    }
	}
	break;

	// integers only
      case HIR::ArithmeticOrLogicalExpr::LEFT_SHIFT:
	case HIR::ArithmeticOrLogicalExpr::RIGHT_SHIFT: {
	  bool valid = (combined->get_kind () == TyTy::TypeKind::INT)
		       || (combined->get_kind () == TyTy::TypeKind::UINT)
		       || (combined->get_kind () == TyTy::TypeKind::INFER
			   && (((TyTy::InferType *) combined)->get_infer_kind ()
			       == TyTy::InferType::INTEGRAL));
	  if (!valid)
	    {
	      rust_error_at (expr.get_locus (), "cannot apply operator to %s",
			     combined->as_string ().c_str ());
	      return;
	    }
	}
	break;
      }

    infered = combined;
    infered->append_reference (lhs->get_ref ());
    infered->append_reference (rhs->get_ref ());
  }

  void visit (HIR::ComparisonExpr &expr)
  {
    auto lhs = TypeCheckExpr::Resolve (expr.get_lhs ());
    auto rhs = TypeCheckExpr::Resolve (expr.get_rhs ());

    auto result = lhs->combine (rhs);
    if (result == nullptr || result->get_kind () == TyTy::TypeKind::ERROR)
      return;

    // we expect this to be
    infered = new TyTy::BoolType (expr.get_mappings ().get_hirid ());
    infered->append_reference (lhs->get_ref ());
    infered->append_reference (rhs->get_ref ());
  }

  void visit (HIR::LazyBooleanExpr &expr)
  {
    auto lhs = TypeCheckExpr::Resolve (expr.get_lhs ());
    auto rhs = TypeCheckExpr::Resolve (expr.get_rhs ());

    // we expect the lhs and rhs must be bools at this point
    TyTy::BoolType elhs (expr.get_mappings ().get_hirid ());
    lhs = elhs.combine (lhs);
    if (lhs == nullptr || lhs->get_kind () == TyTy::TypeKind::ERROR)
      return;

    TyTy::BoolType rlhs (expr.get_mappings ().get_hirid ());
    rhs = elhs.combine (rhs);
    if (lhs == nullptr || lhs->get_kind () == TyTy::TypeKind::ERROR)
      return;

    infered = lhs->combine (rhs);
    infered->append_reference (lhs->get_ref ());
    infered->append_reference (rhs->get_ref ());
  }

  void visit (HIR::NegationExpr &expr)
  {
    auto negated_expr_ty = TypeCheckExpr::Resolve (expr.get_expr ());

    // https://doc.rust-lang.org/reference/expressions/operator-expr.html#negation-operators
    switch (expr.get_negation_type ())
      {
	case HIR::NegationExpr::NegationType::NEGATE: {
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

	case HIR::NegationExpr::NegationType::NOT: {
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
    TypeCheckExpr::Resolve (expr.get_if_condition ());
    auto blk_expr = TypeCheckExpr::Resolve (expr.get_if_block ());

    if (is_final_expr
	&& context->peek_return_type ()->get_kind () != TyTy::TypeKind::UNIT)
      {
	auto expected_ty = context->peek_return_type ();
	infered = expected_ty->combine (blk_expr);
      }
  }

  void visit (HIR::IfExprConseqElse &expr)
  {
    // check and resolve all types in the conditional var
    TypeCheckExpr::Resolve (expr.get_if_condition ());

    auto if_blk_resolved = TypeCheckExpr::Resolve (expr.get_if_block ());
    auto else_blk_resolved = TypeCheckExpr::Resolve (expr.get_else_block ());

    TyTy::TyBase *if_block_tyty = nullptr;
    if (expr.get_if_block ()->has_expr ())
      if_block_tyty
	= TypeCheckExpr::Resolve (expr.get_if_block ()->expr.get ());
    else
      if_block_tyty = if_blk_resolved;

    TyTy::TyBase *else_block_tyty = nullptr;
    if (expr.get_else_block ()->has_expr ())
      else_block_tyty
	= TypeCheckExpr::Resolve (expr.get_else_block ()->expr.get ());
    else
      else_block_tyty = else_blk_resolved;

    if (context->peek_return_type ()->get_kind () != TyTy::TypeKind::UNIT)
      {
	// this must combine to what the type is expected
	// this might be a parameter or the last expr in an if + else in a
	// BlockExpr then it must resolve to fn return type else its a unit-type
	auto expected_ty
	  = is_final_expr
	      ? context->peek_return_type ()
	      : new TyTy::UnitType (expr.get_mappings ().get_hirid ());

	auto if_blk_combined = expected_ty->combine (if_block_tyty);
	auto else_blk_combined = expected_ty->combine (else_block_tyty);

	infered = if_blk_combined->combine (else_blk_combined);
      }
  }

  void visit (HIR::IfExprConseqIf &expr)
  {
    TypeCheckExpr::Resolve (expr.get_if_condition ());
    auto if_blk = TypeCheckExpr::Resolve (expr.get_if_block ());
    auto elif_blk = TypeCheckExpr::Resolve (expr.get_conseq_if_expr ());

    if (is_final_expr
	&& context->peek_return_type ()->get_kind () != TyTy::TypeKind::UNIT)
      {
	auto expected_ty = context->peek_return_type ();

	infered = expected_ty->combine (if_blk);
	infered = infered->combine (elif_blk);
      }
  }

  void visit (HIR::BlockExpr &expr);

  void visit (HIR::ArrayIndexExpr &expr)
  {
    TyTy::TyBase *size_ty;
    if (!context->lookup_builtin ("usize", &size_ty))
      {
	rust_error_at (
	  expr.get_locus (),
	  "Failure looking up size type for index in ArrayIndexExpr");
	return;
      }

    auto resolved_index_expr
      = size_ty->combine (TypeCheckExpr::Resolve (expr.get_index_expr ()));
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
    std::vector<TyTy::TyBase *> types;
    elems.iterate ([&] (HIR::Expr *e) mutable -> bool {
      types.push_back (TypeCheckExpr::Resolve (e));
      return true;
    });

    infered_array_elems = types[0];
    for (size_t i = 1; i < types.size (); i++)
      {
	infered_array_elems = infered_array_elems->combine (types.at (i));
      }

    for (auto &elem : types)
      infered_array_elems->append_reference (elem->get_ref ());
  }

  void visit (HIR::ArrayElemsCopied &elems)
  {
    infered_array_elems = TypeCheckExpr::Resolve (elems.get_elem_to_copy ());
  }

  void visit (HIR::StructExprStructFields &struct_expr)
  {
    infered = TypeCheckStructExpr::Resolve (&struct_expr);
  }

  void visit (HIR::GroupedExpr &expr)
  {
    infered = TypeCheckExpr::Resolve (expr.get_expr_in_parens ().get ());
  }

  void visit (HIR::FieldAccessExpr &expr)
  {
    auto struct_base
      = TypeCheckExpr::Resolve (expr.get_receiver_expr ().get ());

    bool is_valid_type = struct_base->get_kind () == TyTy::TypeKind::ADT;
    if (!is_valid_type)
      {
	rust_error_at (expr.get_locus (),
		       "expected ADT or Tuple Type got: [%s]",
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

private:
  TypeCheckExpr (bool is_final_expr)
    : TypeCheckBase (), infered (nullptr), infered_array_elems (nullptr),
      is_final_expr (is_final_expr)
  {}

  TyTy::TyBase *infered;
  TyTy::TyBase *infered_array_elems;

  bool is_final_expr;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_EXPR
