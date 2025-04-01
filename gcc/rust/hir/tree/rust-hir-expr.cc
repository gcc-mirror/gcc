// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

#include "rust-hir-expr.h"
#include "rust-operators.h"
#include "rust-hir-stmt.h"

namespace Rust {
namespace HIR {

Expr::Expr (Analysis::NodeMapping mappings, AST::AttrVec outer_attribs)
  : outer_attrs (std::move (outer_attribs)), mappings (std::move (mappings))
{}

ExprWithoutBlock::ExprWithoutBlock (Analysis::NodeMapping mappings,
				    AST::AttrVec outer_attribs)
  : Expr (std::move (mappings), std::move (outer_attribs))
{}

LoopLabel::LoopLabel (Analysis::NodeMapping mapping, Lifetime loop_label,
		      location_t locus)
  : label (std::move (loop_label)), locus (locus), mappings (mapping)
{}

ExprWithBlock::ExprWithBlock (Analysis::NodeMapping mappings,
			      AST::AttrVec outer_attrs)
  : Expr (std::move (mappings), std::move (outer_attrs))
{}

LiteralExpr::LiteralExpr (Analysis::NodeMapping mappings,
			  std::string value_as_string, Literal::LitType type,
			  PrimitiveCoreType type_hint, location_t locus,
			  AST::AttrVec outer_attrs)
  : ExprWithoutBlock (std::move (mappings), std::move (outer_attrs)),
    literal (std::move (value_as_string), type, type_hint), locus (locus)
{}

LiteralExpr::LiteralExpr (Analysis::NodeMapping mappings, Literal literal,
			  location_t locus, AST::AttrVec outer_attrs)
  : ExprWithoutBlock (std::move (mappings), std::move (outer_attrs)),
    literal (std::move (literal)), locus (locus)
{}

OperatorExpr::OperatorExpr (Analysis::NodeMapping mappings,
			    std::unique_ptr<Expr> main_or_left_expr,
			    AST::AttrVec outer_attribs, location_t locus)
  : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs)),
    locus (locus), main_or_left_expr (std::move (main_or_left_expr))
{}

OperatorExpr::OperatorExpr (OperatorExpr const &other)
  : ExprWithoutBlock (other), locus (other.locus),
    main_or_left_expr (other.main_or_left_expr->clone_expr ())
{}

OperatorExpr &
OperatorExpr::operator= (OperatorExpr const &other)
{
  ExprWithoutBlock::operator= (other);
  main_or_left_expr = other.main_or_left_expr->clone_expr ();
  locus = other.locus;
  // outer_attrs = other.outer_attrs;

  return *this;
}

BorrowExpr::BorrowExpr (Analysis::NodeMapping mappings,
			std::unique_ptr<Expr> borrow_lvalue, Mutability mut,
			bool raw, AST::AttrVec outer_attribs, location_t locus)
  : OperatorExpr (std::move (mappings), std::move (borrow_lvalue),
		  std::move (outer_attribs), locus),
    mut (mut), raw (raw)
{}

DereferenceExpr::DereferenceExpr (Analysis::NodeMapping mappings,
				  std::unique_ptr<Expr> deref_lvalue,
				  AST::AttrVec outer_attribs, location_t locus)
  : OperatorExpr (std::move (mappings), std::move (deref_lvalue),
		  std::move (outer_attribs), locus)
{}

ErrorPropagationExpr::ErrorPropagationExpr (
  Analysis::NodeMapping mappings, std::unique_ptr<Expr> potential_error_value,
  AST::AttrVec outer_attribs, location_t locus)
  : OperatorExpr (std::move (mappings), std::move (potential_error_value),
		  std::move (outer_attribs), locus)
{}

NegationExpr::NegationExpr (Analysis::NodeMapping mappings,
			    std::unique_ptr<Expr> negated_value,
			    ExprType expr_kind, AST::AttrVec outer_attribs,
			    location_t locus)
  : OperatorExpr (std::move (mappings), std::move (negated_value),
		  std::move (outer_attribs), locus),
    expr_type (expr_kind)
{}

ArithmeticOrLogicalExpr::ArithmeticOrLogicalExpr (
  Analysis::NodeMapping mappings, std::unique_ptr<Expr> left_value,
  std::unique_ptr<Expr> right_value, ExprType expr_kind, location_t locus)
  : OperatorExpr (std::move (mappings), std::move (left_value), AST::AttrVec (),
		  locus),
    expr_type (expr_kind), right_expr (std::move (right_value))
{}

ArithmeticOrLogicalExpr::ArithmeticOrLogicalExpr (
  ArithmeticOrLogicalExpr const &other)
  : OperatorExpr (other), expr_type (other.expr_type),
    right_expr (other.right_expr->clone_expr ())
{}

ArithmeticOrLogicalExpr &
ArithmeticOrLogicalExpr::operator= (ArithmeticOrLogicalExpr const &other)
{
  OperatorExpr::operator= (other);
  // main_or_left_expr = other.main_or_left_expr->clone_expr();
  right_expr = other.right_expr->clone_expr ();
  expr_type = other.expr_type;

  return *this;
}

ComparisonExpr::ComparisonExpr (Analysis::NodeMapping mappings,
				std::unique_ptr<Expr> left_value,
				std::unique_ptr<Expr> right_value,
				ExprType comparison_kind, location_t locus)
  : OperatorExpr (std::move (mappings), std::move (left_value), AST::AttrVec (),
		  locus),
    expr_type (comparison_kind), right_expr (std::move (right_value))
{}

ComparisonExpr::ComparisonExpr (ComparisonExpr const &other)
  : OperatorExpr (other), expr_type (other.expr_type),
    right_expr (other.right_expr->clone_expr ())
{}

ComparisonExpr &
ComparisonExpr::operator= (ComparisonExpr const &other)
{
  OperatorExpr::operator= (other);
  // main_or_left_expr = other.main_or_left_expr->clone_expr();
  right_expr = other.right_expr->clone_expr ();
  expr_type = other.expr_type;
  // outer_attrs = other.outer_attrs;

  return *this;
}

LazyBooleanExpr::LazyBooleanExpr (Analysis::NodeMapping mappings,
				  std::unique_ptr<Expr> left_bool_expr,
				  std::unique_ptr<Expr> right_bool_expr,
				  ExprType expr_kind, location_t locus)
  : OperatorExpr (std::move (mappings), std::move (left_bool_expr),
		  AST::AttrVec (), locus),
    expr_type (expr_kind), right_expr (std::move (right_bool_expr))
{}

LazyBooleanExpr::LazyBooleanExpr (LazyBooleanExpr const &other)
  : OperatorExpr (other), expr_type (other.expr_type),
    right_expr (other.right_expr->clone_expr ())
{}

LazyBooleanExpr &
LazyBooleanExpr::operator= (LazyBooleanExpr const &other)
{
  OperatorExpr::operator= (other);
  // main_or_left_expr = other.main_or_left_expr->clone_expr();
  right_expr = other.right_expr->clone_expr ();
  expr_type = other.expr_type;

  return *this;
}

TypeCastExpr::TypeCastExpr (Analysis::NodeMapping mappings,
			    std::unique_ptr<Expr> expr_to_cast,
			    std::unique_ptr<Type> type_to_cast_to,
			    location_t locus)
  : OperatorExpr (std::move (mappings), std::move (expr_to_cast),
		  AST::AttrVec (), locus),
    type_to_convert_to (std::move (type_to_cast_to))
{}

TypeCastExpr::TypeCastExpr (TypeCastExpr const &other)
  : OperatorExpr (other),
    type_to_convert_to (other.type_to_convert_to->clone_type ())
{}

TypeCastExpr &
TypeCastExpr::operator= (TypeCastExpr const &other)
{
  OperatorExpr::operator= (other);
  // main_or_left_expr = other.main_or_left_expr->clone_expr();
  type_to_convert_to = other.type_to_convert_to->clone_type ();

  return *this;
}

AssignmentExpr::AssignmentExpr (Analysis::NodeMapping mappings,
				std::unique_ptr<Expr> value_to_assign_to,
				std::unique_ptr<Expr> value_to_assign,
				location_t locus)
  : OperatorExpr (std::move (mappings), std::move (value_to_assign_to),
		  AST::AttrVec (), locus),
    right_expr (std::move (value_to_assign))
{}

AssignmentExpr::AssignmentExpr (AssignmentExpr const &other)
  : OperatorExpr (other), right_expr (other.right_expr->clone_expr ())
{}

AssignmentExpr &
AssignmentExpr::operator= (AssignmentExpr const &other)
{
  OperatorExpr::operator= (other);
  // main_or_left_expr = other.main_or_left_expr->clone_expr();
  right_expr = other.right_expr->clone_expr ();
  // outer_attrs = other.outer_attrs;

  return *this;
}

CompoundAssignmentExpr::CompoundAssignmentExpr (
  Analysis::NodeMapping mappings, std::unique_ptr<Expr> value_to_assign_to,
  std::unique_ptr<Expr> value_to_assign, ExprType expr_kind, location_t locus)
  : OperatorExpr (std::move (mappings), std::move (value_to_assign_to),
		  AST::AttrVec (), locus),
    expr_type (expr_kind), right_expr (std::move (value_to_assign))
{}

CompoundAssignmentExpr::CompoundAssignmentExpr (
  CompoundAssignmentExpr const &other)
  : OperatorExpr (other), expr_type (other.expr_type),
    right_expr (other.right_expr->clone_expr ())
{}

CompoundAssignmentExpr &
CompoundAssignmentExpr::operator= (CompoundAssignmentExpr const &other)
{
  OperatorExpr::operator= (other);
  // main_or_left_expr = other.main_or_left_expr->clone_expr();
  right_expr = other.right_expr->clone_expr ();
  expr_type = other.expr_type;
  // outer_attrs = other.outer_attrs;

  return *this;
}

GroupedExpr::GroupedExpr (Analysis::NodeMapping mappings,
			  std::unique_ptr<Expr> parenthesised_expr,
			  AST::AttrVec inner_attribs,
			  AST::AttrVec outer_attribs, location_t locus)
  : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs)),
    WithInnerAttrs (std::move (inner_attribs)),
    expr_in_parens (std::move (parenthesised_expr)), locus (locus)
{}

GroupedExpr::GroupedExpr (GroupedExpr const &other)
  : ExprWithoutBlock (other), WithInnerAttrs (other.inner_attrs),
    expr_in_parens (other.expr_in_parens->clone_expr ()), locus (other.locus)
{}

GroupedExpr &
GroupedExpr::operator= (GroupedExpr const &other)
{
  ExprWithoutBlock::operator= (other);
  inner_attrs = other.inner_attrs;
  expr_in_parens = other.expr_in_parens->clone_expr ();
  locus = other.locus;
  // outer_attrs = other.outer_attrs;

  return *this;
}

ArrayElemsValues::ArrayElemsValues (Analysis::NodeMapping mappings,
				    std::vector<std::unique_ptr<Expr>> elems)
  : ArrayElems (mappings), values (std::move (elems))
{}

ArrayElemsValues::ArrayElemsValues (ArrayElemsValues const &other)
  : ArrayElems (other)
{
  values.reserve (other.values.size ());
  for (const auto &e : other.values)
    values.push_back (e->clone_expr ());
}

ArrayElemsValues &
ArrayElemsValues::operator= (ArrayElemsValues const &other)
{
  values.reserve (other.values.size ());
  for (const auto &e : other.values)
    values.push_back (e->clone_expr ());

  return *this;
}

ArrayElemsCopied::ArrayElemsCopied (Analysis::NodeMapping mappings,
				    std::unique_ptr<Expr> copied_elem,
				    std::unique_ptr<Expr> copy_amount)
  : ArrayElems (mappings), elem_to_copy (std::move (copied_elem)),
    num_copies (std::move (copy_amount))
{}

ArrayElemsCopied::ArrayElemsCopied (ArrayElemsCopied const &other)
  : ArrayElems (other), elem_to_copy (other.elem_to_copy->clone_expr ()),
    num_copies (other.num_copies->clone_expr ())
{}

ArrayElemsCopied &
ArrayElemsCopied::operator= (ArrayElemsCopied const &other)
{
  elem_to_copy = other.elem_to_copy->clone_expr ();
  num_copies = other.num_copies->clone_expr ();

  return *this;
}

ArrayExpr::ArrayExpr (Analysis::NodeMapping mappings,
		      std::unique_ptr<ArrayElems> array_elems,
		      AST::AttrVec inner_attribs, AST::AttrVec outer_attribs,
		      location_t locus)
  : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs)),
    WithInnerAttrs (std::move (inner_attribs)),
    internal_elements (std::move (array_elems)), locus (locus)
{}

ArrayExpr::ArrayExpr (ArrayExpr const &other)
  : ExprWithoutBlock (other), WithInnerAttrs (other.inner_attrs),
    locus (other.locus)
{
  if (other.has_array_elems ())
    internal_elements = other.internal_elements->clone_array_elems ();
}

ArrayExpr &
ArrayExpr::operator= (ArrayExpr const &other)
{
  ExprWithoutBlock::operator= (other);
  inner_attrs = other.inner_attrs;
  if (other.has_array_elems ())
    internal_elements = other.internal_elements->clone_array_elems ();
  locus = other.locus;
  // outer_attrs = other.outer_attrs;

  return *this;
}

ArrayIndexExpr::ArrayIndexExpr (Analysis::NodeMapping mappings,
				std::unique_ptr<Expr> array_expr,
				std::unique_ptr<Expr> array_index_expr,
				AST::AttrVec outer_attribs, location_t locus)
  : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs)),
    array_expr (std::move (array_expr)),
    index_expr (std::move (array_index_expr)), locus (locus)
{}

ArrayIndexExpr::ArrayIndexExpr (ArrayIndexExpr const &other)
  : ExprWithoutBlock (other), array_expr (other.array_expr->clone_expr ()),
    index_expr (other.index_expr->clone_expr ()), locus (other.locus)
{}

ArrayIndexExpr &
ArrayIndexExpr::operator= (ArrayIndexExpr const &other)
{
  ExprWithoutBlock::operator= (other);
  array_expr = other.array_expr->clone_expr ();
  index_expr = other.index_expr->clone_expr ();
  // outer_attrs = other.outer_attrs;
  locus = other.locus;

  return *this;
}

TupleExpr::TupleExpr (Analysis::NodeMapping mappings,
		      std::vector<std::unique_ptr<Expr>> tuple_elements,
		      AST::AttrVec inner_attribs, AST::AttrVec outer_attribs,
		      location_t locus)
  : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs)),
    WithInnerAttrs (std::move (inner_attribs)),
    tuple_elems (std::move (tuple_elements)), locus (locus)
{}

TupleExpr::TupleExpr (TupleExpr const &other)
  : ExprWithoutBlock (other), WithInnerAttrs (other.inner_attrs),
    locus (other.locus)
{
  tuple_elems.reserve (other.tuple_elems.size ());
  for (const auto &e : other.tuple_elems)
    tuple_elems.push_back (e->clone_expr ());
}

TupleExpr &
TupleExpr::operator= (TupleExpr const &other)
{
  ExprWithoutBlock::operator= (other);
  inner_attrs = other.inner_attrs;
  locus = other.locus;

  tuple_elems.reserve (other.tuple_elems.size ());
  for (const auto &e : other.tuple_elems)
    tuple_elems.push_back (e->clone_expr ());

  return *this;
}

TupleIndexExpr::TupleIndexExpr (Analysis::NodeMapping mappings,
				std::unique_ptr<Expr> tuple_expr,
				TupleIndex index, AST::AttrVec outer_attribs,
				location_t locus)
  : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs)),
    tuple_expr (std::move (tuple_expr)), tuple_index (index), locus (locus)
{}

TupleIndexExpr::TupleIndexExpr (TupleIndexExpr const &other)
  : ExprWithoutBlock (other), tuple_expr (other.tuple_expr->clone_expr ()),
    tuple_index (other.tuple_index), locus (other.locus)
{}

TupleIndexExpr &
TupleIndexExpr::operator= (TupleIndexExpr const &other)
{
  ExprWithoutBlock::operator= (other);
  tuple_expr = other.tuple_expr->clone_expr ();
  tuple_index = other.tuple_index;
  locus = other.locus;
  // outer_attrs = other.outer_attrs;

  return *this;
}

StructExpr::StructExpr (Analysis::NodeMapping mappings,
			PathInExpression struct_path,
			AST::AttrVec outer_attribs)
  : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs)),
    struct_name (std::move (struct_path))
{}

StructExprStruct::StructExprStruct (Analysis::NodeMapping mappings,
				    PathInExpression struct_path,
				    AST::AttrVec inner_attribs,
				    AST::AttrVec outer_attribs,
				    location_t locus)
  : StructExpr (std::move (mappings), std::move (struct_path),
		std::move (outer_attribs)),
    WithInnerAttrs (std::move (inner_attribs)), locus (locus)
{}

StructBase::StructBase (std::unique_ptr<Expr> base_struct_ptr)
  : base_struct (std::move (base_struct_ptr))
{}

StructBase::StructBase (StructBase const &other)
{
  /* HACK: gets around base_struct pointer being null (e.g. if no struct base
   * exists) */
  if (other.base_struct != nullptr)
    other.base_struct->clone_expr ();
}

StructBase &
StructBase::operator= (StructBase const &other)
{
  base_struct = other.base_struct->clone_expr ();

  return *this;
}

StructExprField::StructExprField (Analysis::NodeMapping mapping,
				  location_t locus)
  : mappings (mapping), locus (locus)
{}

StructExprFieldIdentifier::StructExprFieldIdentifier (
  Analysis::NodeMapping mapping, Identifier field_identifier, location_t locus)
  : StructExprField (mapping, locus), field_name (std::move (field_identifier))
{}

StructExprFieldWithVal::StructExprFieldWithVal (
  Analysis::NodeMapping mapping, std::unique_ptr<Expr> field_value,
  location_t locus)
  : StructExprField (mapping, locus), value (std::move (field_value))
{}

StructExprFieldWithVal::StructExprFieldWithVal (
  StructExprFieldWithVal const &other)
  : StructExprField (other.mappings, other.locus),
    value (other.value->clone_expr ())
{}

StructExprFieldWithVal &
StructExprFieldWithVal::operator= (StructExprFieldWithVal const &other)
{
  value = other.value->clone_expr ();
  mappings = other.mappings;
  locus = other.locus;

  return *this;
}

StructExprFieldIdentifierValue::StructExprFieldIdentifierValue (
  Analysis::NodeMapping mapping, Identifier field_identifier,
  std::unique_ptr<Expr> field_value, location_t locus)
  : StructExprFieldWithVal (mapping, std::move (field_value), locus),
    field_name (std::move (field_identifier))
{}

StructExprFieldIndexValue::StructExprFieldIndexValue (
  Analysis::NodeMapping mapping, TupleIndex tuple_index,
  std::unique_ptr<Expr> field_value, location_t locus)
  : StructExprFieldWithVal (mapping, std::move (field_value), locus),
    index (tuple_index)
{}

StructExprStructFields::StructExprStructFields (
  Analysis::NodeMapping mappings, PathInExpression struct_path,
  std::vector<std::unique_ptr<StructExprField>> expr_fields, location_t locus,
  tl::optional<std::unique_ptr<StructBase>> base_struct,
  AST::AttrVec inner_attribs = AST::AttrVec (),
  AST::AttrVec outer_attribs = AST::AttrVec ())
  : StructExprStruct (std::move (mappings), std::move (struct_path),
		      std::move (inner_attribs), std::move (outer_attribs),
		      locus),
    fields (std::move (expr_fields)), struct_base (std::move (base_struct))
{}

StructExprStructFields::StructExprStructFields (
  StructExprStructFields const &other)
  : StructExprStruct (other),
    struct_base (other.has_struct_base ()
		   ? tl::optional<std::unique_ptr<StructBase>> (
		     std::make_unique<StructBase> (*other.struct_base.value ()))
		   : tl::nullopt),
    union_index (other.union_index)
{
  fields.reserve (other.fields.size ());
  for (const auto &e : other.fields)
    fields.push_back (e->clone_struct_expr_field ());
}

StructExprStructFields &
StructExprStructFields::operator= (StructExprStructFields const &other)
{
  StructExprStruct::operator= (other);
  struct_base = other.has_struct_base ()
		  ? tl::optional<std::unique_ptr<StructBase>> (
		    std::make_unique<StructBase> (*other.struct_base.value ()))
		  : tl::nullopt;
  union_index = other.union_index;

  fields.reserve (other.fields.size ());
  for (const auto &e : other.fields)
    fields.push_back (e->clone_struct_expr_field ());

  return *this;
}

StructExprStructBase::StructExprStructBase (Analysis::NodeMapping mappings,
					    PathInExpression struct_path,
					    StructBase base_struct,
					    AST::AttrVec inner_attribs,
					    AST::AttrVec outer_attribs,
					    location_t locus)
  : StructExprStruct (std::move (mappings), std::move (struct_path),
		      std::move (inner_attribs), std::move (outer_attribs),
		      locus),
    struct_base (std::move (base_struct))
{}

CallExpr::CallExpr (Analysis::NodeMapping mappings,
		    std::unique_ptr<Expr> function_expr,
		    std::vector<std::unique_ptr<Expr>> function_params,
		    AST::AttrVec outer_attribs, location_t locus)
  : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs)),
    function (std::move (function_expr)), params (std::move (function_params)),
    locus (locus)
{}

CallExpr::CallExpr (CallExpr const &other)
  : ExprWithoutBlock (other), function (other.function->clone_expr ()),
    locus (other.locus)
/*, params(other.params),*/ {
  params.reserve (other.params.size ());
  for (const auto &e : other.params)
    params.push_back (e->clone_expr ());
}

CallExpr &
CallExpr::operator= (CallExpr const &other)
{
  ExprWithoutBlock::operator= (other);
  function = other.function->clone_expr ();
  locus = other.locus;
  // params = other.params;
  // outer_attrs = other.outer_attrs;

  params.reserve (other.params.size ());
  for (const auto &e : other.params)
    params.push_back (e->clone_expr ());

  return *this;
}

MethodCallExpr::MethodCallExpr (
  Analysis::NodeMapping mappings, std::unique_ptr<Expr> call_receiver,
  PathExprSegment method_path, std::vector<std::unique_ptr<Expr>> method_params,
  AST::AttrVec outer_attribs, location_t locus)
  : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs)),
    receiver (std::move (call_receiver)), method_name (std::move (method_path)),
    params (std::move (method_params)), locus (locus)
{}

MethodCallExpr::MethodCallExpr (MethodCallExpr const &other)
  : ExprWithoutBlock (other), receiver (other.receiver->clone_expr ()),
    method_name (other.method_name), locus (other.locus)
/*, params(other.params),*/ {
  params.reserve (other.params.size ());
  for (const auto &e : other.params)
    params.push_back (e->clone_expr ());
}

MethodCallExpr &
MethodCallExpr::operator= (MethodCallExpr const &other)
{
  ExprWithoutBlock::operator= (other);
  receiver = other.receiver->clone_expr ();
  method_name = other.method_name;
  locus = other.locus;
  // params = other.params;
  // outer_attrs = other.outer_attrs;

  params.reserve (other.params.size ());
  for (const auto &e : other.params)
    params.push_back (e->clone_expr ());

  return *this;
}

FieldAccessExpr::FieldAccessExpr (Analysis::NodeMapping mappings,
				  std::unique_ptr<Expr> field_access_receiver,
				  Identifier field_name,
				  AST::AttrVec outer_attribs, location_t locus)
  : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs)),
    receiver (std::move (field_access_receiver)),
    field (std::move (field_name)), locus (locus)
{}

FieldAccessExpr::FieldAccessExpr (FieldAccessExpr const &other)
  : ExprWithoutBlock (other), receiver (other.receiver->clone_expr ()),
    field (other.field), locus (other.locus)
{}

FieldAccessExpr &
FieldAccessExpr::operator= (FieldAccessExpr const &other)
{
  ExprWithoutBlock::operator= (other);
  receiver = other.receiver->clone_expr ();
  field = other.field;
  locus = other.locus;
  // outer_attrs = other.outer_attrs;

  return *this;
}

ClosureParam::ClosureParam (std::unique_ptr<Pattern> param_pattern,
			    location_t locus, std::unique_ptr<Type> param_type,
			    std::vector<AST::Attribute> outer_attrs)
  : outer_attrs (std::move (outer_attrs)), pattern (std::move (param_pattern)),
    type (std::move (param_type)), locus (locus)
{}

ClosureParam::ClosureParam (ClosureParam const &other)
  : pattern (other.pattern->clone_pattern ())
{
  // guard to protect from null pointer dereference
  if (other.pattern != nullptr)
    pattern = other.pattern->clone_pattern ();
  if (other.type != nullptr)
    type = other.type->clone_type ();
}

ClosureParam &
ClosureParam::operator= (ClosureParam const &other)
{
  outer_attrs = other.outer_attrs;

  // guard to protect from null pointer dereference
  if (other.pattern != nullptr)
    pattern = other.pattern->clone_pattern ();
  else
    pattern = nullptr;
  if (other.type != nullptr)
    type = other.type->clone_type ();
  else
    type = nullptr;

  return *this;
}

ClosureExpr::ClosureExpr (Analysis::NodeMapping mappings,
			  std::vector<ClosureParam> closure_params,
			  std::unique_ptr<Type> closure_return_type,
			  std::unique_ptr<Expr> closure_expr, bool has_move,
			  AST::AttrVec outer_attribs, location_t locus)
  : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs)),
    has_move (has_move), params (std::move (closure_params)), locus (locus),
    return_type (std::move (closure_return_type)),
    expr (std::move (closure_expr))
{}

ClosureExpr::ClosureExpr (ClosureExpr const &other)
  : ExprWithoutBlock (other.get_mappings (), other.get_outer_attrs ())
{
  return_type
    = other.has_return_type () ? other.return_type->clone_type () : nullptr;
  expr = other.expr->clone_expr ();
  params = other.params;
  has_move = other.has_move;
}

ClosureExpr &
ClosureExpr::operator= (ClosureExpr const &other)
{
  mappings = other.mappings;
  return_type
    = other.has_return_type () ? other.return_type->clone_type () : nullptr;
  expr = other.expr->clone_expr ();
  params = other.params;
  has_move = other.has_move;

  return *this;
}

BlockExpr::BlockExpr (Analysis::NodeMapping mappings,
		      std::vector<std::unique_ptr<Stmt>> block_statements,
		      std::unique_ptr<Expr> block_expr, bool tail_reachable,
		      AST::AttrVec inner_attribs, AST::AttrVec outer_attribs,
		      tl::optional<LoopLabel> label, location_t start_locus,
		      location_t end_locus)
  : ExprWithBlock (std::move (mappings), std::move (outer_attribs)),
    WithInnerAttrs (std::move (inner_attribs)),
    statements (std::move (block_statements)), expr (std::move (block_expr)),
    tail_reachable (tail_reachable), label (std::move (label)),
    start_locus (start_locus), end_locus (end_locus)
{}

BlockExpr::BlockExpr (BlockExpr const &other)
  : ExprWithBlock (other), /*statements(other.statements),*/
    WithInnerAttrs (other.inner_attrs), label (other.label),
    start_locus (other.start_locus), end_locus (other.end_locus)
{
  // guard to protect from null pointer dereference
  if (other.expr != nullptr)
    expr = other.expr->clone_expr ();

  statements.reserve (other.statements.size ());
  for (const auto &e : other.statements)
    statements.push_back (e->clone_stmt ());
}

BlockExpr &
BlockExpr::operator= (BlockExpr const &other)
{
  ExprWithBlock::operator= (other);
  // statements = other.statements;
  expr = other.expr->clone_expr ();
  inner_attrs = other.inner_attrs;
  start_locus = other.end_locus;
  end_locus = other.end_locus;
  // outer_attrs = other.outer_attrs;

  statements.reserve (other.statements.size ());
  for (const auto &e : other.statements)
    statements.push_back (e->clone_stmt ());

  return *this;
}

ContinueExpr::ContinueExpr (Analysis::NodeMapping mappings, location_t locus,
			    tl::optional<Lifetime> label,
			    AST::AttrVec outer_attribs)
  : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs)),
    label (std::move (label)), locus (locus)
{}

BreakExpr::BreakExpr (Analysis::NodeMapping mappings, location_t locus,
		      tl::optional<Lifetime> break_label,
		      std::unique_ptr<Expr> expr_in_break,
		      AST::AttrVec outer_attribs)
  : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs)),
    label (std::move (break_label)), break_expr (std::move (expr_in_break)),
    locus (locus)
{}

BreakExpr::BreakExpr (BreakExpr const &other)
  : ExprWithoutBlock (other), label (other.label), locus (other.locus)
{
  // guard to protect from null pointer dereference
  if (other.break_expr != nullptr)
    break_expr = other.break_expr->clone_expr ();
}

BreakExpr &
BreakExpr::operator= (BreakExpr const &other)
{
  ExprWithoutBlock::operator= (other);
  label = other.label;
  break_expr = other.break_expr->clone_expr ();
  locus = other.locus;
  // outer_attrs = other.outer_attrs;

  return *this;
}

RangeExpr::RangeExpr (Analysis::NodeMapping mappings, location_t locus)
  : ExprWithoutBlock (std::move (mappings), AST::AttrVec ()), locus (locus)
{}

RangeFromToExpr::RangeFromToExpr (Analysis::NodeMapping mappings,
				  std::unique_ptr<Expr> range_from,
				  std::unique_ptr<Expr> range_to,
				  location_t locus)
  : RangeExpr (std::move (mappings), locus), from (std::move (range_from)),
    to (std::move (range_to))
{}

RangeFromToExpr::RangeFromToExpr (RangeFromToExpr const &other)
  : RangeExpr (other), from (other.from->clone_expr ()),
    to (other.to->clone_expr ())
{}

RangeFromToExpr &
RangeFromToExpr::operator= (RangeFromToExpr const &other)
{
  RangeExpr::operator= (other);
  from = other.from->clone_expr ();
  to = other.to->clone_expr ();

  return *this;
}

RangeFromExpr::RangeFromExpr (Analysis::NodeMapping mappings,
			      std::unique_ptr<Expr> range_from,
			      location_t locus)
  : RangeExpr (std::move (mappings), locus), from (std::move (range_from))
{}

RangeFromExpr::RangeFromExpr (RangeFromExpr const &other)
  : RangeExpr (other), from (other.from->clone_expr ())
{}

RangeFromExpr &
RangeFromExpr::operator= (RangeFromExpr const &other)
{
  RangeExpr::operator= (other);
  from = other.from->clone_expr ();

  return *this;
}

RangeToExpr::RangeToExpr (Analysis::NodeMapping mappings,
			  std::unique_ptr<Expr> range_to, location_t locus)
  : RangeExpr (std::move (mappings), locus), to (std::move (range_to))
{}

RangeToExpr::RangeToExpr (RangeToExpr const &other)
  : RangeExpr (other), to (other.to->clone_expr ())
{}

RangeToExpr &
RangeToExpr::operator= (RangeToExpr const &other)
{
  RangeExpr::operator= (other);
  to = other.to->clone_expr ();

  return *this;
}

RangeFullExpr::RangeFullExpr (Analysis::NodeMapping mappings, location_t locus)
  : RangeExpr (std::move (mappings), locus)
{}

RangeFromToInclExpr::RangeFromToInclExpr (Analysis::NodeMapping mappings,
					  std::unique_ptr<Expr> range_from,
					  std::unique_ptr<Expr> range_to,
					  location_t locus)
  : RangeExpr (std::move (mappings), locus), from (std::move (range_from)),
    to (std::move (range_to))
{}

RangeFromToInclExpr::RangeFromToInclExpr (RangeFromToInclExpr const &other)
  : RangeExpr (other), from (other.from->clone_expr ()),
    to (other.to->clone_expr ())
{}

RangeFromToInclExpr &
RangeFromToInclExpr::operator= (RangeFromToInclExpr const &other)
{
  RangeExpr::operator= (other);
  from = other.from->clone_expr ();
  to = other.to->clone_expr ();

  return *this;
}

RangeToInclExpr::RangeToInclExpr (Analysis::NodeMapping mappings,
				  std::unique_ptr<Expr> range_to,
				  location_t locus)
  : RangeExpr (std::move (mappings), locus), to (std::move (range_to))
{}

RangeToInclExpr::RangeToInclExpr (RangeToInclExpr const &other)
  : RangeExpr (other), to (other.to->clone_expr ())
{}

RangeToInclExpr &
RangeToInclExpr::operator= (RangeToInclExpr const &other)
{
  RangeExpr::operator= (other);
  to = other.to->clone_expr ();

  return *this;
}

ReturnExpr::ReturnExpr (Analysis::NodeMapping mappings, location_t locus,
			std::unique_ptr<Expr> returned_expr,
			AST::AttrVec outer_attribs)
  : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs)),
    return_expr (std::move (returned_expr)), locus (locus)
{}

ReturnExpr::ReturnExpr (ReturnExpr const &other)
  : ExprWithoutBlock (other), locus (other.locus)
{
  // guard to protect from null pointer dereference
  if (other.return_expr != nullptr)
    return_expr = other.return_expr->clone_expr ();
}

ReturnExpr &
ReturnExpr::operator= (ReturnExpr const &other)
{
  ExprWithoutBlock::operator= (other);
  return_expr = other.return_expr->clone_expr ();
  locus = other.locus;
  // outer_attrs = other.outer_attrs;

  return *this;
}

UnsafeBlockExpr::UnsafeBlockExpr (Analysis::NodeMapping mappings,
				  std::unique_ptr<BlockExpr> block_expr,
				  AST::AttrVec outer_attribs, location_t locus)
  : ExprWithBlock (std::move (mappings), std::move (outer_attribs)),
    expr (std::move (block_expr)), locus (locus)
{}

UnsafeBlockExpr::UnsafeBlockExpr (UnsafeBlockExpr const &other)
  : ExprWithBlock (other), expr (other.expr->clone_block_expr ()),
    locus (other.locus)
{}

UnsafeBlockExpr &
UnsafeBlockExpr::operator= (UnsafeBlockExpr const &other)
{
  ExprWithBlock::operator= (other);
  expr = other.expr->clone_block_expr ();
  locus = other.locus;
  // outer_attrs = other.outer_attrs;

  return *this;
}

BaseLoopExpr::BaseLoopExpr (Analysis::NodeMapping mappings,
			    std::unique_ptr<BlockExpr> loop_block,
			    location_t locus,
			    tl::optional<LoopLabel> loop_label,
			    AST::AttrVec outer_attribs)
  : ExprWithBlock (std::move (mappings), std::move (outer_attribs)),
    loop_label (std::move (loop_label)), loop_block (std::move (loop_block)),
    locus (locus)
{}

BaseLoopExpr::BaseLoopExpr (BaseLoopExpr const &other)
  : ExprWithBlock (other), loop_label (other.loop_label),
    loop_block (other.loop_block->clone_block_expr ()), locus (other.locus)
{}

BaseLoopExpr &
BaseLoopExpr::operator= (BaseLoopExpr const &other)
{
  ExprWithBlock::operator= (other);
  loop_block = other.loop_block->clone_block_expr ();
  loop_label = other.loop_label;
  locus = other.locus;
  // outer_attrs = other.outer_attrs;

  return *this;
}

LoopExpr::LoopExpr (Analysis::NodeMapping mappings,
		    std::unique_ptr<BlockExpr> loop_block, location_t locus,
		    tl::optional<LoopLabel> loop_label,
		    AST::AttrVec outer_attribs)
  : BaseLoopExpr (std::move (mappings), std::move (loop_block), locus,
		  std::move (loop_label), std::move (outer_attribs))
{}

WhileLoopExpr::WhileLoopExpr (Analysis::NodeMapping mappings,
			      std::unique_ptr<Expr> loop_condition,
			      std::unique_ptr<BlockExpr> loop_block,
			      location_t locus,
			      tl::optional<LoopLabel> loop_label,
			      AST::AttrVec outer_attribs)
  : BaseLoopExpr (std::move (mappings), std::move (loop_block), locus,
		  std::move (loop_label), std::move (outer_attribs)),
    condition (std::move (loop_condition))
{}

WhileLoopExpr::WhileLoopExpr (WhileLoopExpr const &other)
  : BaseLoopExpr (other), condition (other.condition->clone_expr ())
{}

WhileLoopExpr &
WhileLoopExpr::operator= (WhileLoopExpr const &other)
{
  BaseLoopExpr::operator= (other);
  condition = other.condition->clone_expr ();
  // loop_block = other.loop_block->clone_block_expr();
  // loop_label = other.loop_label;
  // outer_attrs = other.outer_attrs;

  return *this;
}

WhileLetLoopExpr::WhileLetLoopExpr (
  Analysis::NodeMapping mappings,
  std::vector<std::unique_ptr<Pattern>> match_arm_patterns,
  std::unique_ptr<Expr> condition, std::unique_ptr<BlockExpr> loop_block,
  location_t locus, tl::optional<LoopLabel> loop_label,
  AST::AttrVec outer_attribs)
  : BaseLoopExpr (std::move (mappings), std::move (loop_block), locus,
		  std::move (loop_label), std::move (outer_attribs)),
    match_arm_patterns (std::move (match_arm_patterns)),
    condition (std::move (condition))
{}

WhileLetLoopExpr::WhileLetLoopExpr (WhileLetLoopExpr const &other)
  : BaseLoopExpr (other),
    /*match_arm_patterns(other.match_arm_patterns),*/ condition (
      other.condition->clone_expr ())
{
  match_arm_patterns.reserve (other.match_arm_patterns.size ());
  for (const auto &e : other.match_arm_patterns)
    match_arm_patterns.push_back (e->clone_pattern ());
}

WhileLetLoopExpr &
WhileLetLoopExpr::operator= (WhileLetLoopExpr const &other)
{
  BaseLoopExpr::operator= (other);
  // match_arm_patterns = other.match_arm_patterns;
  condition = other.condition->clone_expr ();
  // loop_block = other.loop_block->clone_block_expr();
  // loop_label = other.loop_label;
  // outer_attrs = other.outer_attrs;

  match_arm_patterns.reserve (other.match_arm_patterns.size ());
  for (const auto &e : other.match_arm_patterns)
    match_arm_patterns.push_back (e->clone_pattern ());

  return *this;
}

IfExpr::IfExpr (Analysis::NodeMapping mappings, std::unique_ptr<Expr> condition,
		std::unique_ptr<BlockExpr> if_block, location_t locus)
  : ExprWithBlock (std::move (mappings), AST::AttrVec ()),
    condition (std::move (condition)), if_block (std::move (if_block)),
    locus (locus)
{}

IfExpr::IfExpr (IfExpr const &other)
  : ExprWithBlock (other), condition (other.condition->clone_expr ()),
    if_block (other.if_block->clone_block_expr ()), locus (other.locus)
{}

IfExpr &
IfExpr::operator= (IfExpr const &other)
{
  ExprWithBlock::operator= (other);
  condition = other.condition->clone_expr ();
  if_block = other.if_block->clone_block_expr ();
  locus = other.locus;

  return *this;
}

IfExprConseqElse::IfExprConseqElse (Analysis::NodeMapping mappings,
				    std::unique_ptr<Expr> condition,
				    std::unique_ptr<BlockExpr> if_block,
				    std::unique_ptr<ExprWithBlock> else_block,
				    location_t locus)
  : IfExpr (std::move (mappings), std::move (condition), std::move (if_block),
	    locus),
    else_block (std::move (else_block))
{}

IfExprConseqElse::IfExprConseqElse (IfExprConseqElse const &other)
  : IfExpr (other), else_block (other.else_block->clone_expr_with_block ())
{}

IfExprConseqElse &
IfExprConseqElse::operator= (IfExprConseqElse const &other)
{
  IfExpr::operator= (other);
  // condition = other.condition->clone_expr();
  // if_block = other.if_block->clone_block_expr();
  else_block = other.else_block->clone_expr_with_block ();

  return *this;
}

MatchArm::MatchArm (std::vector<std::unique_ptr<Pattern>> match_arm_patterns,
		    location_t locus, std::unique_ptr<Expr> guard_expr,
		    AST::AttrVec outer_attrs)
  : outer_attrs (std::move (outer_attrs)),
    match_arm_patterns (std::move (match_arm_patterns)),
    guard_expr (std::move (guard_expr)), locus (locus)
{}

MatchArm::MatchArm (MatchArm const &other) : outer_attrs (other.outer_attrs)
{
  // guard to protect from null pointer dereference
  if (other.guard_expr != nullptr)
    guard_expr = other.guard_expr->clone_expr ();

  match_arm_patterns.reserve (other.match_arm_patterns.size ());
  for (const auto &e : other.match_arm_patterns)
    match_arm_patterns.push_back (e->clone_pattern ());

  locus = other.locus;
}

MatchArm &
MatchArm::operator= (MatchArm const &other)
{
  outer_attrs = other.outer_attrs;

  if (other.guard_expr != nullptr)
    guard_expr = other.guard_expr->clone_expr ();

  match_arm_patterns.clear ();
  match_arm_patterns.reserve (other.match_arm_patterns.size ());
  for (const auto &e : other.match_arm_patterns)
    match_arm_patterns.push_back (e->clone_pattern ());

  return *this;
}

MatchCase::MatchCase (Analysis::NodeMapping mappings, MatchArm arm,
		      std::unique_ptr<Expr> expr)
  : mappings (mappings), arm (std::move (arm)), expr (std::move (expr))
{}

MatchCase::MatchCase (const MatchCase &other)
  : mappings (other.mappings), arm (other.arm), expr (other.expr->clone_expr ())
{}

MatchCase &
MatchCase::operator= (const MatchCase &other)
{
  mappings = other.mappings;
  arm = other.arm;
  expr = other.expr->clone_expr ();

  return *this;
}

MatchExpr::MatchExpr (Analysis::NodeMapping mappings,
		      std::unique_ptr<Expr> branch_value,
		      std::vector<MatchCase> match_arms,
		      AST::AttrVec inner_attrs, AST::AttrVec outer_attrs,
		      location_t locus)
  : ExprWithBlock (std::move (mappings), std::move (outer_attrs)),
    WithInnerAttrs (std::move (inner_attrs)),
    branch_value (std::move (branch_value)),
    match_arms (std::move (match_arms)), locus (locus)
{}

MatchExpr::MatchExpr (MatchExpr const &other)
  : ExprWithBlock (other), WithInnerAttrs (other.inner_attrs),
    branch_value (other.branch_value->clone_expr ()),
    match_arms (other.match_arms), locus (other.locus)
{
  /*match_arms.reserve (other.match_arms.size ());
  for (const auto &e : other.match_arms)
    match_arms.push_back (e->clone_match_case ());*/
}

MatchExpr &
MatchExpr::operator= (MatchExpr const &other)
{
  ExprWithBlock::operator= (other);
  branch_value = other.branch_value->clone_expr ();
  inner_attrs = other.inner_attrs;
  match_arms = other.match_arms;
  // outer_attrs = other.outer_attrs;
  locus = other.locus;

  /*match_arms.reserve (other.match_arms.size ());
  for (const auto &e : other.match_arms)
    match_arms.push_back (e->clone_match_case ());*/

  return *this;
}

AwaitExpr::AwaitExpr (Analysis::NodeMapping mappings,
		      std::unique_ptr<Expr> awaited_expr,
		      AST::AttrVec outer_attrs, location_t locus)
  : ExprWithoutBlock (std::move (mappings), std::move (outer_attrs)),
    awaited_expr (std::move (awaited_expr)), locus (locus)
{}

AwaitExpr::AwaitExpr (AwaitExpr const &other)
  : ExprWithoutBlock (other), awaited_expr (other.awaited_expr->clone_expr ()),
    locus (other.locus)
{}

AwaitExpr &
AwaitExpr::operator= (AwaitExpr const &other)
{
  ExprWithoutBlock::operator= (other);
  awaited_expr = other.awaited_expr->clone_expr ();
  locus = other.locus;

  return *this;
}

AsyncBlockExpr::AsyncBlockExpr (Analysis::NodeMapping mappings,
				std::unique_ptr<BlockExpr> block_expr,
				bool has_move, AST::AttrVec outer_attrs,
				location_t locus)
  : ExprWithBlock (std::move (mappings), std::move (outer_attrs)),
    has_move (has_move), block_expr (std::move (block_expr)), locus (locus)
{}

AsyncBlockExpr::AsyncBlockExpr (AsyncBlockExpr const &other)
  : ExprWithBlock (other), has_move (other.has_move),
    block_expr (other.block_expr->clone_block_expr ()), locus (other.locus)
{}

AsyncBlockExpr &
AsyncBlockExpr::operator= (AsyncBlockExpr const &other)
{
  ExprWithBlock::operator= (other);
  has_move = other.has_move;
  block_expr = other.block_expr->clone_block_expr ();
  locus = other.locus;

  return *this;
}

OperatorExprMeta::OperatorExprMeta (HIR::CompoundAssignmentExpr &expr)
  : node_mappings (expr.get_mappings ()),
    lvalue_mappings (expr.get_expr ().get_mappings ()),
    locus (expr.get_locus ())
{}

OperatorExprMeta::OperatorExprMeta (HIR::ArithmeticOrLogicalExpr &expr)
  : node_mappings (expr.get_mappings ()),
    lvalue_mappings (expr.get_expr ().get_mappings ()),
    locus (expr.get_locus ())
{}

OperatorExprMeta::OperatorExprMeta (HIR::NegationExpr &expr)
  : node_mappings (expr.get_mappings ()),
    lvalue_mappings (expr.get_expr ().get_mappings ()),
    locus (expr.get_locus ())
{}

OperatorExprMeta::OperatorExprMeta (HIR::DereferenceExpr &expr)
  : node_mappings (expr.get_mappings ()),
    lvalue_mappings (expr.get_expr ().get_mappings ()),
    locus (expr.get_locus ())
{}

OperatorExprMeta::OperatorExprMeta (HIR::ArrayIndexExpr &expr)
  : node_mappings (expr.get_mappings ()),
    lvalue_mappings (expr.get_array_expr ().get_mappings ()),
    locus (expr.get_locus ())
{}

OperatorExprMeta::OperatorExprMeta (HIR::ComparisonExpr &expr)
  : node_mappings (expr.get_mappings ()),
    lvalue_mappings (expr.get_expr ().get_mappings ()),
    locus (expr.get_locus ())
{}

AnonConst::AnonConst (NodeId id, std::unique_ptr<Expr> expr)
  : id (id), expr (std::move (expr))
{
  rust_assert (this->expr != nullptr);
}

AnonConst::AnonConst (const AnonConst &other)
{
  id = other.id;
  expr = other.expr->clone_expr ();
}

AnonConst
AnonConst::operator= (const AnonConst &other)
{
  id = other.id;
  expr = other.expr->clone_expr ();
  return *this;
}

InlineAsmOperand::In::In (
  const tl::optional<struct AST::InlineAsmRegOrRegClass> &reg,
  std::unique_ptr<Expr> expr)
  : reg (reg), expr (std::move (expr))
{
  rust_assert (this->expr != nullptr);
}

InlineAsmOperand::In::In (const struct In &other)
{
  reg = other.reg;

  expr = other.expr->clone_expr ();
}

InlineAsmOperand::In
InlineAsmOperand::In::operator= (const struct In &other)
{
  reg = other.reg;
  expr = other.expr->clone_expr ();

  return *this;
}

InlineAsmOperand::Out::Out (
  tl::optional<struct AST::InlineAsmRegOrRegClass> &reg, bool late,
  std::unique_ptr<Expr> expr)
  : reg (reg), late (late), expr (std::move (expr))
{
  rust_assert (this->expr != nullptr);
}

InlineAsmOperand::Out::Out (const struct Out &other)
{
  reg = other.reg;
  late = other.late;
  expr = other.expr->clone_expr ();
}

InlineAsmOperand::Out
InlineAsmOperand::Out::operator= (const struct Out &other)
{
  reg = other.reg;
  late = other.late;
  expr = other.expr->clone_expr ();
  return *this;
}

InlineAsmOperand::InOut::InOut (
  tl::optional<struct AST::InlineAsmRegOrRegClass> &reg, bool late,
  std::unique_ptr<Expr> expr)
  : reg (reg), late (late), expr (std::move (expr))
{
  rust_assert (this->expr != nullptr);
}

InlineAsmOperand::InOut::InOut (const struct InOut &other)
{
  reg = other.reg;
  late = other.late;
  expr = other.expr->clone_expr ();
}

InlineAsmOperand::InOut
InlineAsmOperand::InOut::operator= (const struct InOut &other)
{
  reg = other.reg;
  late = other.late;
  expr = other.expr->clone_expr ();

  return *this;
}

InlineAsmOperand::SplitInOut::SplitInOut (
  tl::optional<struct AST::InlineAsmRegOrRegClass> &reg, bool late,
  std::unique_ptr<Expr> in_expr, std::unique_ptr<Expr> out_expr)
  : reg (reg), late (late), in_expr (std::move (in_expr)),
    out_expr (std::move (out_expr))
{
  rust_assert (this->in_expr != nullptr);
  rust_assert (this->out_expr != nullptr);
}

InlineAsmOperand::SplitInOut::SplitInOut (const struct SplitInOut &other)
{
  reg = other.reg;
  late = other.late;
  in_expr = other.in_expr->clone_expr ();
  out_expr = other.out_expr->clone_expr ();
}

InlineAsmOperand::SplitInOut
InlineAsmOperand::SplitInOut::operator= (const struct SplitInOut &other)
{
  reg = other.reg;
  late = other.late;
  in_expr = other.in_expr->clone_expr ();
  out_expr = other.out_expr->clone_expr ();

  return *this;
}

InlineAsmOperand::Sym::Sym (std::unique_ptr<Expr> expr)
  : expr (std::move (expr))
{
  rust_assert (this->expr != nullptr);
}

InlineAsmOperand::Sym::Sym (const struct Sym &other)
{
  expr = std::unique_ptr<Expr> (other.expr->clone_expr ());
}

InlineAsmOperand::Sym
InlineAsmOperand::Sym::operator= (const struct Sym &other)
{
  expr = std::unique_ptr<Expr> (other.expr->clone_expr ());
  return *this;
}

InlineAsmOperand::Label::Label (tl::optional<std::string> label_name,
				std::unique_ptr<Expr> expr)
  : expr (std::move (expr))
{
  rust_assert (this->expr != nullptr);
  if (label_name.has_value ())
    this->label_name = label_name.value ();
}

InlineAsmOperand::Label::Label (const struct Label &other)
{
  expr = std::unique_ptr<Expr> (other.expr->clone_expr ());
}

InlineAsmOperand::Label
InlineAsmOperand::Label::operator= (const struct Label &other)
{
  expr = std::unique_ptr<Expr> (other.expr->clone_expr ());
  return *this;
}

InlineAsm::InlineAsm (location_t locus, bool is_global_asm,
		      std::vector<AST::InlineAsmTemplatePiece> template_,
		      std::vector<AST::TupleTemplateStr> template_strs,
		      std::vector<HIR::InlineAsmOperand> operands,
		      std::vector<AST::TupleClobber> clobber_abi,
		      std::set<AST::InlineAsmOption> options,
		      Analysis::NodeMapping mappings,
		      AST::AttrVec outer_attribs)
  : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs)),
    locus (locus), is_global_asm (is_global_asm),
    template_ (std::move (template_)),
    template_strs (std::move (template_strs)), operands (std::move (operands)),
    clobber_abi (std::move (clobber_abi)), options (std::move (options))
{}

} // namespace HIR
} // namespace Rust
