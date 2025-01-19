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

#include "rust-hir-dump.h"
#include "rust-abi.h"
#include "rust-hir-item.h"
#include "rust-hir-path.h"
#include "rust-hir-type.h"
#include "rust-hir.h"
#include <string>
#include "rust-attribute-values.h"

namespace Rust {
namespace HIR {

// Dump Format for HIR
//
// SomeHIRNode [
//    field: ...
//    field: ...
//    field: ...
// ]
//
// When a field is a collection of other HIR objects:
//   field {
//     SomeHIRNode [ ... ]
//     SomeOtherHIRNode [ ... ]
//   }
//
// If a field is an empty collection:
//  field: empty
//
// If a field is optional and is currently not holding anything:
//   field: none

std::string Dump::delims[2][2] = {
  {std::string ("{"), std::string ("}")},
  {std::string ("["), std::string ("]")},
};

static std::string
BoundPolarityString (BoundPolarity polarity)
{
  switch (polarity)
    {
    case RegularBound:
      return "regular";
    case NegativeBound:
      return "negative";
    case AntiBound:
      return "anti";
    }
  return "unknown";
}

/**
 * Static member used to dump HIR from the debugger to stderr.
 *
 * @param v The HIR node to dump
 */
void
Dump::debug (FullVisitable &v)
{
  Dump dump (std::cerr);
  v.accept_vis (dump);
}

void
Dump::go (HIR::Crate &e)
{
  begin ("Crate");
  do_inner_attrs (e);
  do_mappings (e.get_mappings ());

  visit_collection ("items", e.get_items ());
  end ("Crate");
}

Dump::Dump (std::ostream &stream) : stream (stream) {}

/**
 * Writes TEXT with a final newline if ENDLINE is true.
 * If TEXT is starting the current line, its first line is indented.
 * If TEXT is multiline, all followning lines are also indented.
 *
 * @param text Text to emit
 * @param endline If true, newline is emitted after text
 */
void
Dump::put (std::string text, bool endline)
{
  if (beg_of_line)
    {
      stream << indentation;
      beg_of_line = false;
    }

  // keep multiline string indented
  std::string::size_type pos = 0;
  std::string::size_type prev = 0;
  auto first = true;
  while ((pos = text.find ('\n', prev)) != std::string::npos)
    {
      if (!first)
	stream << std::endl << indentation;
      first = false;

      stream << text.substr (prev, pos - prev);
      prev = pos + 1;
    }

  if (first)
    stream << text;

  if (endline)
    {
      stream << std::endl;
      beg_of_line = endline;
    }
}

/**
 * Called when starting to emit info for an HIR node.
 * Emits NAME and an opening delimiter denoted by D.
 *
 * @param name Name of HIR node
 * @param d Delimiter
 */
void
Dump::begin (std::string name, enum delim d)
{
  if (!beg_of_line)
    put ("");
  put (name + " " + delims[d][0], true);
  indentation.increment ();
}

/**
 * Called when ending the dump info for an HIR node. Emits a C++-style
 * comment with NAME and a closing delimiter denoted by D.
 *
 * @param name Name of HIR node
 * @param d Delimiter
 */
void
Dump::end (std::string name, enum delim d)
{
  indentation.decrement ();
  if (!beg_of_line)
    stream << std::endl;
  put (delims[d][1] + " // " + name);
}

/**
 * Called when starting to emit info for a field within an HIR node.
 * Emits NAME and an opening curly brace
 *
 * @param name HIR field name
 */
void
Dump::begin_field (std::string name)
{
  begin (name, CURLY);
}

/**
 * Called when ending the dump info for a field within an HIR node.
 * Emits a C++-style comment with NAME and a closing curly brace
 *
 * @param name HIR field name
 */
void
Dump::end_field (std::string name)
{
  end (name, CURLY);
}

/**
 * Emits a single field/value pair denoted by NAME and TEXT.
 *
 * @param name Field name
 * @param text Field value
 */
void
Dump::put_field (std::string name, std::string text)
{
  put (name + ": ", false);
  indentation.increment ();
  put (text);
  indentation.decrement ();
}

/**
 * Recursively visits an HIR field NAME with value possibly pointed to by
 * PTR, if PTR is not null. If PTR is null, simply emits FIELD_NAME/NULL pair.
 *
 * @param name Field name
 * @param ptr Pointer to field's value
 */
template <class T>
void
Dump::visit_field (std::string name, std::unique_ptr<T> &ptr)
{
  if (ptr)
    visit_field (name, *ptr);
  else
    put_field (name, "NULL");
}

/**
 * Recursively visits an HIR field NAME with value V.
 *
 * @param name Field name
 * @param v Field value
 */
void
Dump::visit_field (std::string name, FullVisitable &v)
{
  put (name + ": ", false);
  indentation.increment ();
  v.accept_vis (*this);
  indentation.decrement ();
}

/**
 * Recursively visits a collection VEC of HIR node for field NAME.
 * If VEC is empty, simply emits the NAME/empty pair.
 *
 * @param name Field name
 * @param vec Field value as a vector
 */
template <class T>
void
Dump::visit_collection (std::string name, std::vector<std::unique_ptr<T>> &vec)
{
  if (vec.empty ())
    {
      put_field (name, "empty");
      return;
    }

  begin_field (name);
  for (const auto &elt : vec)
    elt->accept_vis (*this);
  end_field (name);
}

/**
 * Recursively visits a collection VEC of HIR node for field NAME.
 * If VEC is empty, simply emits the NAME/empty pair.
 *
 * @param name Field name
 * @param vec Field value as a vector
 */
template <class T>
void
Dump::visit_collection (std::string name, std::vector<T> &vec)
{
  if (vec.empty ())
    {
      put_field (name, "empty");
      return;
    }

  begin_field (name);
  for (auto &elt : vec)
    elt.accept_vis (*this);
  end_field (name);
}

void
Dump::do_traititem (TraitItem &e)
{
  do_mappings (e.get_mappings ());
  auto oa = e.get_outer_attrs ();
  do_outer_attrs (oa);
}

void
Dump::do_vis_item (VisItem &e)
{
  do_item (e);
  std::string str = "none";
  if (e.has_visibility ())
    str = e.get_visibility ().as_string ();
  put_field ("visibility", str);
}

void
Dump::do_functionparam (FunctionParam &e)
{
  begin ("FunctionParam");
  do_mappings (e.get_mappings ());
  visit_field ("param_name", e.get_param_name ());
  visit_field ("type", e.get_type ());
  end ("FunctionParam");
}

void
Dump::do_pathpattern (PathPattern &e)
{
  std::string str = "";

  for (const auto &segment : e.get_segments ())
    str += segment.as_string () + ", ";

  put_field ("segments", str);
}

void
Dump::do_structexprstruct (StructExprStruct &e)
{
  do_expr (e);

  // StructExpr
  visit_field ("struct_name", e.get_struct_name ());

  // StructExprStruct
  do_mappings (e.get_mappings ());
  do_inner_attrs (e);
}

void
Dump::do_ifexpr (IfExpr &e)
{
  do_expr (e);
  visit_field ("condition", e.get_if_condition ());
  visit_field ("if_block", e.get_if_block ());
}

void
Dump::do_expr (Expr &e)
{
  do_mappings (e.get_mappings ());
  auto oa = e.get_outer_attrs ();
  do_outer_attrs (oa);
}

void
Dump::do_pathexpr (PathExpr &e)
{
  do_expr (e);
}

void
Dump::do_typepathsegment (TypePathSegment &e)
{
  do_mappings (e.get_mappings ());
  put_field ("ident_segment", e.get_ident_segment ().as_string ());
}

void
Dump::do_typepathfunction (TypePathFunction &e)
{
  visit_collection ("params", e.get_params ());
  visit_field ("return_type", e.get_return_type ());
}

void
Dump::do_qualifiedpathtype (QualifiedPathType &e)
{
  do_mappings (e.get_mappings ());
  visit_field ("type", e.get_type ());

  visit_field ("trait", e.get_trait ());
}

void
Dump::do_operatorexpr (OperatorExpr &e)
{
  visit_field ("main_or_left_expr", e.get_expr ());
}

void
Dump::do_mappings (const Analysis::NodeMapping &mappings)
{
  put ("mapping: ", false);
  put (mappings.as_string ());
}

void
Dump::do_inner_attrs (WithInnerAttrs &e)
{
  auto attrs = e.get_inner_attrs ();

  if (attrs.empty ())
    {
      put_field ("inner_attrs", "empty");
      return;
    }

  begin_field ("inner_attrs");
  for (auto &elt : attrs)
    visit (elt);
  end_field ("inner_attrs");
}

void
Dump::do_outer_attrs (std::vector<AST::Attribute> &attrs)
{
  if (attrs.empty ())
    put_field ("outer_attributes", "empty");
  else
    {
      begin_field ("outer_attributes");
      for (const auto &attr : attrs)
	put (attr.as_string ());
      end_field ("outer_attributes");
    }
}

void
Dump::do_baseloopexpr (BaseLoopExpr &e)
{
  do_expr (e);

  if (!e.has_loop_label ())
    put_field ("label", "none");
  else
    put_field ("label", e.get_loop_label ().as_string ());

  visit_field ("loop_block", e.get_loop_block ());
}

void
Dump::do_ifletexpr (IfLetExpr &e)
{
  do_expr (e);

  visit_collection ("match_arm_patterns", e.get_patterns ());

  visit_field ("value", e.get_scrutinee_expr ());
  visit_field ("if_block", e.get_if_block ());
}

void
Dump::do_struct (Struct &e)
{
  do_vis_item (e);
  put_field ("struct_name", e.get_identifier ().as_string ());
  visit_collection ("generic_params", e.get_generic_params ());

  if (!e.has_where_clause ())
    put_field ("where_clause", "none");
  else
    put_field ("where clause", e.get_where_clause ().as_string ());
}

void
Dump::do_enumitem (EnumItem &e)
{
  do_item (e);

  put_field ("variant_name", e.get_identifier ().as_string ());

  std::string str;
  switch (e.get_enum_item_kind ())
    {
    case EnumItem::EnumItemKind::Named:
      str = "[Named variant]";
      break;
    case EnumItem::EnumItemKind::Tuple:
      str = "[Tuple variant]";
      break;
    case EnumItem::EnumItemKind::Struct:
      str = "[Struct variant]";
      break;
    case EnumItem::EnumItemKind::Discriminant:
      str = "[Discriminant variant]";
      break;
    }
  put_field ("item_kind", str);
}

void
Dump::do_traitfunctiondecl (TraitFunctionDecl &e)
{
  begin ("TraitFunctionDecl");
  put_field ("qualifiers", e.get_qualifiers ().as_string ());
  put_field ("function_name", e.get_function_name ().as_string ());
  visit_collection ("generic_params", e.get_generic_params ());

  if (!e.get_function_params ().empty ())
    {
      begin_field ("function_params");
      for (auto &item : e.get_function_params ())
	do_functionparam (item);
      end_field ("function_params");
    }
  else
    put_field ("function_params", "empty");

  visit_field ("return_type", e.get_return_type ());

  if (e.has_where_clause ())
    put_field ("where_clause", e.get_where_clause ().as_string ());
  else
    put_field ("where_clause", "none");

  put_field ("self", e.get_self ().as_string ());

  end ("TraitFunctionDecl");
}

void
Dump::do_externalitem (ExternalItem &e)
{
  do_mappings (e.get_mappings ());

  auto oa = e.get_outer_attrs ();
  do_outer_attrs (oa);

  std::string str = "none";
  if (e.has_visibility ())
    str = e.get_visibility ().as_string ();
  put_field ("visibility", str);
  put_field ("item_name", e.get_item_name ().as_string ());
}

void
Dump::do_namefunctionparam (NamedFunctionParam &e)
{
  begin ("NamedFunctionParam");
  do_mappings (e.get_mappings ());
  put_field ("name", e.get_param_name ().as_string ());
  visit_field ("type", e.get_type ());
  end ("NamedFunctionParam");
}

void
Dump::do_stmt (Stmt &e)
{
  do_mappings (e.get_mappings ());
}

void
Dump::do_type (Type &e)
{
  do_mappings (e.get_mappings ());
}

void
Dump::do_item (Item &e)
{
  do_stmt (e);
  auto oa = e.get_outer_attrs ();
  do_outer_attrs (oa);
}

void
Dump::do_tuplefield (TupleField &e)
{
  do_mappings (e.get_mappings ());
  auto oa = e.get_outer_attrs ();
  do_outer_attrs (oa);

  std::string str = "none";
  if (e.has_visibility ())
    str = e.get_visibility ().as_string ();
  put_field ("visibility", str);

  visit_field ("field_type", e.get_field_type ());
}

void
Dump::do_structfield (StructField &e)
{
  do_mappings (e.get_mappings ());
  auto oa = e.get_outer_attrs ();
  do_outer_attrs (oa);

  std::string str = "none";
  if (e.has_visibility ())
    str = e.get_visibility ().as_string ();
  put_field ("visibility", str);
  put_field ("field_name", e.get_field_name ().as_string ());
  visit_field ("field_type", e.get_field_type ());
}

void
Dump::do_genericargs (GenericArgs &e)
{
  visit_collection ("lifetime_args", e.get_lifetime_args ());
  visit_collection ("type_args", e.get_type_args ());

  if (e.get_const_args ().empty ())
    {
      put_field ("binding_args", "empty");
    }
  else
    {
      begin_field ("const_args");
      for (auto &arg : e.get_const_args ())
	{
	  begin ("ConstGenericArg");
	  visit_field ("expression", arg.get_expression ());
	  end ("ConstGenericArg");
	}
      end_field ("const_args");
    }

  if (e.get_binding_args ().empty ())
    {
      put_field ("binding_args", "empty");
    }
  else
    {
      begin_field ("binding_args");
      for (auto &arg : e.get_binding_args ())
	{
	  begin ("GenericArgsBinding");
	  put_field ("identfier", arg.get_identifier ().as_string ());
	  visit_field ("type", arg.get_type ());
	  end ("GenericArgsBinding");
	}
      end_field ("binding_args");
    }
}

void
Dump::do_maybenamedparam (MaybeNamedParam &e)
{
  visit_field ("param_type", e.get_type ());
  put_field ("param_kind", enum_to_str (e.get_param_kind ()));
  put_field ("name", e.get_name ().as_string ());
}

// All visit methods

void
Dump::visit (AST::Attribute &attribute)
{
  // Special, no begin/end as this is called by do_inner_attrs.
  put_field (Values::Attributes::PATH, attribute.get_path ().as_string ());

  std::string str = "none";
  if (attribute.has_attr_input ())
    str = attribute.get_attr_input ().as_string ();
  put_field ("attr_input", str);
}

void
Dump::visit (Lifetime &e)
{
  do_mappings (e.get_mappings ());

  std::string type;
  std::string name = e.get_name ();
  switch (e.get_lifetime_type ())
    {
    case AST::Lifetime::LifetimeType::NAMED:
      type = "[NAMED]";
      break;
    case AST::Lifetime::LifetimeType::STATIC:
      type = "[STATIC]";
      name += " (not applicable for type)";
      break;
    case AST::Lifetime::LifetimeType::WILDCARD:
      type = "[WILDCARD]";
      name += " (not applicable for type)";
      break;
    default:
      rust_assert (false);
      break;
    }
  put_field ("lifetime_type", type);
  put_field ("lifetime_name", name);
}

void
Dump::visit (LifetimeParam &lifetimeparam)
{
  begin ("Lifetimeparam");
  put (lifetimeparam.as_string ());
  end ("Lifetimeparam");
}

void
Dump::visit (PathInExpression &e)
{
  begin ("PathInExpression");
  do_pathpattern (e);
  do_pathexpr (e);

  put_field ("has_opening_scope_resolution",
	     std::to_string (e.opening_scope_resolution ()));
  end ("PathInExpression");
}

void
Dump::visit (TypePathSegment &e)
{
  begin ("TypePathSegment");
  do_typepathsegment (e);
  end ("TypePathSegment");
}

void
Dump::visit (TypePathSegmentGeneric &e)
{
  begin ("TypePathSegmentGeneric");
  do_typepathsegment (e);

  if (e.has_generic_args ())
    {
      begin_field ("generic_args");
      begin ("GenericArgs");
      do_genericargs (e.get_generic_args ());
      end ("GenericArgs");
      end_field ("generic_args");
    }
  else
    {
      put_field ("generic_args", "empty");
    }

  end ("TypePathSegmentGeneric");
}

void
Dump::visit (TypePathSegmentFunction &e)
{
  begin ("TypePathSegmentFunction");
  do_typepathsegment (e);

  begin ("function_path");
  do_typepathfunction (e.get_function_path ());
  end ("function_path");

  end ("TypePathSegmentFunction");
}

void
Dump::visit (TypePath &e)
{
  begin ("TypePath");
  put_field ("has_opening_scope_resolution",
	     std::to_string (e.has_opening_scope_resolution_op ()));

  visit_collection ("segments", e.get_segments ());

  end ("TypePath");
}

void
Dump::visit (QualifiedPathInExpression &e)
{
  begin ("QualifiedPathInExpression");
  do_pathpattern (e);
  do_expr (e);

  begin_field ("path_type");

  begin ("QualifiedPathType");
  do_qualifiedpathtype (e.get_path_type ());
  end ("QualifiedPathType");

  end_field ("path_type");

  end ("QualifiedPathInExpression");
}

void
Dump::visit (QualifiedPathInType &e)
{
  begin ("QualifiedPathInType");

  begin_field ("path_type");
  do_qualifiedpathtype (e.get_path_type ());
  end_field ("path_type");

  begin_field ("associated_segment");
  do_typepathsegment (*e.get_associated_segment ());
  end_field ("associated_segment");

  visit_collection ("segments", e.get_segments ());

  end ("QualifiedPathInType");
}

void
Dump::visit (LiteralExpr &e)
{
  begin ("LiteralExpr");
  do_expr (e);
  put_field ("literal", e.get_literal ().as_string ());
  end ("LiteralExpr");
}

void
Dump::visit (BorrowExpr &e)
{
  begin ("BorrowExpr");
  do_operatorexpr (e);

  put_field ("mut", enum_to_str (e.get_mut ()));

  end ("BorrowExpr");
}

void
Dump::visit (DereferenceExpr &e)
{
  begin ("DereferenceExpr");
  do_operatorexpr (e);
  end ("DereferenceExpr");
}

void
Dump::visit (ErrorPropagationExpr &e)
{
  begin ("ErrorPropagationExpr");
  do_operatorexpr (e);
  end ("ErrorPropagationExpr");
}

void
Dump::visit (NegationExpr &e)
{
  begin ("NegationExpr");
  do_operatorexpr (e);
  std::string str;
  switch (e.get_expr_type ())
    {
    case NegationOperator::NEGATE:
      str = "[NEGATE]";
      break;
    case NegationOperator::NOT:
      str = "[NOT]";
      break;
    default:
      rust_assert (false);
    }
  put_field ("expr_type", str);

  end ("NegationExpr");
}

void
Dump::visit (ArithmeticOrLogicalExpr &e)
{
  begin ("ArithmeticOrLogicalExpr");
  std::string str;

  // which operator
  switch (e.get_expr_type ())
    {
    case ArithmeticOrLogicalOperator::ADD:
      str = "[ADD]";
      break;
    case ArithmeticOrLogicalOperator::SUBTRACT:
      str = "SUBTRACT";
      break;
    case ArithmeticOrLogicalOperator::MULTIPLY:
      str = "MULTIPLY";
      break;
    case ArithmeticOrLogicalOperator::DIVIDE:
      str = "DIVIDE";
      break;
    case ArithmeticOrLogicalOperator::MODULUS:
      str = "MODULUS";
      break;
    case ArithmeticOrLogicalOperator::BITWISE_AND:
      str = "BITWISE";
      break;
    case ArithmeticOrLogicalOperator::BITWISE_OR:
      str = "BITWISE";
      break;
    case ArithmeticOrLogicalOperator::BITWISE_XOR:
      str = "BITWISE";
      break;
    case ArithmeticOrLogicalOperator::LEFT_SHIFT:
      str = "<LEFT";
      break;
    case ArithmeticOrLogicalOperator::RIGHT_SHIFT:
      str = ">RIGHT";
      break;
    default:
      rust_unreachable ();
      break;
    }
  put_field ("expr_type", str);
  do_operatorexpr (e);
  visit_field ("right_expr", *e.get_rhs ());

  end ("ArithmeticOrLogicalExpr");
}

void
Dump::visit (ComparisonExpr &e)
{
  begin ("ComparisonExpr");
  std::string str;
  switch (e.get_expr_type ())
    {
    case ComparisonOperator::EQUAL:
      str = "EQUAL";
      break;
    case ComparisonOperator::NOT_EQUAL:
      str = "NOT_EQUAL";
      break;
    case ComparisonOperator::GREATER_THAN:
      str = "GREATER_THAN";
      break;
    case ComparisonOperator::LESS_THAN:
      str = "LESS_THAN";
      break;
    case ComparisonOperator::GREATER_OR_EQUAL:
      str = "GREATER_OR_EQUAL";
      break;
    case ComparisonOperator::LESS_OR_EQUAL:
      str = "LESS_OR_EQUAL";
      break;
    default:
      rust_assert (false);
    }
  put_field ("expr_type", str);
  do_operatorexpr (e);
  visit_field ("right_expr", *e.get_rhs ());
  end ("ComparisonExpr");
}

void
Dump::visit (LazyBooleanExpr &e)
{
  begin ("LazyBooleanExpr");

  std::string str;
  switch (e.get_expr_type ())
    {
    case LazyBooleanOperator::LOGICAL_OR:
      str = "LOGICAL_OR";
      break;
    case LazyBooleanOperator::LOGICAL_AND:
      str = "LOGICAL_AND";
      break;
    default:
      rust_assert (false);
    }

  do_operatorexpr (e);
  visit_field ("right_expr", *e.get_rhs ());
  end ("LazyBooleanExpr");
}

void
Dump::visit (TypeCastExpr &e)
{
  begin ("TypeCastExpr");
  do_operatorexpr (e);
  visit_field ("type_to_convert_to", e.get_type_to_convert_to ());
  end ("TypeCastExpr");
}

void
Dump::visit (AssignmentExpr &e)
{
  begin ("AssignmentExpr");
  do_operatorexpr (e);
  visit_field ("right_expr", *e.get_rhs ());
  end ("AssignmentExpr");
}

void
Dump::visit (CompoundAssignmentExpr &e)
{
  begin ("CompoundAssignmentExpr");

  do_operatorexpr (e);
  visit_field ("right_expr", *e.get_rhs ());

  std::string str;

  // get operator string
  switch (e.get_expr_type ())
    {
    case ArithmeticOrLogicalOperator::ADD:
      str = "ADD";
      break;
    case ArithmeticOrLogicalOperator::SUBTRACT:
      str = "SUBTRACT";
      break;
    case ArithmeticOrLogicalOperator::MULTIPLY:
      str = "MULTIPLY";
      break;
    case ArithmeticOrLogicalOperator::DIVIDE:
      str = "DIVIDE";
      break;
    case ArithmeticOrLogicalOperator::MODULUS:
      str = "MODULUS";
      break;
    case ArithmeticOrLogicalOperator::BITWISE_AND:
      str = "BITWISE_AND";
      break;
    case ArithmeticOrLogicalOperator::BITWISE_OR:
      str = "BITWISE_OR";
      break;
    case ArithmeticOrLogicalOperator::BITWISE_XOR:
      str = "BITWISE_XOR";
      break;
    case ArithmeticOrLogicalOperator::LEFT_SHIFT:
      str = "LEFT_SHIFT";
      break;
    case ArithmeticOrLogicalOperator::RIGHT_SHIFT:
      str = "RIGHT_SHIFT";
      break;
    default:
      gcc_unreachable ();
      break;
    }
  put_field ("expr_type", str);
  end ("CompoundAssignmentExpr");
}

void
Dump::visit (GroupedExpr &e)
{
  begin ("GroupedExpr");
  do_expr (e);
  do_inner_attrs (e);

  visit_field ("expr_in_parens", e.get_expr_in_parens ());

  end ("GroupedExpr");
}

void
Dump::visit (ArrayElemsValues &e)
{
  begin ("ArrayElemsValues");
  do_mappings (e.get_mappings ());

  visit_collection ("values", e.get_values ());
  end ("ArrayElemsValues");
}

void
Dump::visit (ArrayElemsCopied &e)
{
  begin ("ArrayElemsCopied");
  do_mappings (e.get_mappings ());

  visit_field ("copied_elem", e.get_elem_to_copy ());
  visit_field ("num_copies", e.get_num_copies_expr ());

  end ("ArrayElemsCopied");
}

void
Dump::visit (ArrayExpr &e)
{
  begin ("ArrayExpr");
  do_expr (e);
  do_inner_attrs (e);

  visit_field ("internal_elements", e.get_internal_elements ());

  end ("ArrayExpr");
}

void
Dump::visit (ArrayIndexExpr &e)
{
  begin ("ArrayIndexExpr");
  do_expr (e);

  visit_field ("array_expr", e.get_array_expr ());
  visit_field ("index_expr", e.get_index_expr ());
  end ("ArrayIndexExpr");
}

void
Dump::visit (TupleExpr &e)
{
  begin ("TupleExpr");
  do_expr (e);
  do_inner_attrs (e);

  visit_collection ("tuple_elems", e.get_tuple_elems ());

  end ("TupleExpr");
}

void
Dump::visit (TupleIndexExpr &e)
{
  begin ("TupleIndexExpr");
  do_expr (e);
  visit_field ("tuple_expr", e.get_tuple_expr ());
  put_field ("tuple_index", std::to_string (e.get_tuple_index ()));
  end ("TupleIndexExpr");
}

void
Dump::visit (StructExprStruct &e)
{
  begin ("StructExprStruct");
  do_structexprstruct (e);
  end ("StructExprStruct");
}

void
Dump::visit (StructExprFieldIdentifier &e)
{
  begin ("StructExprFieldIdentifier");
  do_mappings (e.get_mappings ());

  put_field ("field_name", e.get_field_name ().as_string ());
  end ("StructExprFieldIdentifier");
}

void
Dump::visit (StructExprFieldIdentifierValue &e)
{
  begin ("StructExprFieldIdentifierValue");
  do_mappings (e.get_mappings ());

  visit_field ("value", e.get_value ());
  end ("StructExprFieldIdentifierValue");
}

void
Dump::visit (StructExprFieldIndexValue &e)
{
  begin ("StructExprFieldIndexValue");
  do_mappings (e.get_mappings ());

  put_field ("index", std::to_string (e.get_tuple_index ()));
  visit_field ("value", e.get_value ());
  end ("StructExprFieldIndexValue");
}

void
Dump::visit (StructExprStructFields &e)
{
  begin ("StructExprStructFields");
  do_structexprstruct (e);

  visit_collection ("fields", e.get_fields ());

  if (!e.has_struct_base ())
    put_field ("struct_base", "none");
  else
    put_field ("struct_base", e.get_struct_base ()->as_string ());

  end ("StructExprStructFields");
}

void
Dump::visit (StructExprStructBase &e)
{
  begin ("StructExprStructBase");
  do_structexprstruct (e);

  put_field ("struct_base", e.get_struct_base ()->as_string ());

  end ("StructExprStructBase");
}

void
Dump::visit (CallExpr &e)
{
  begin ("CallExpr");
  do_expr (e);
  visit_field ("function", e.get_fnexpr ());

  visit_collection ("params", e.get_arguments ());

  end ("CallExpr");
}

void
Dump::visit (MethodCallExpr &e)
{
  begin ("MethodCallExpr");
  do_expr (e);

  visit_field ("receiver", e.get_receiver ());
  put_field ("method_name", e.get_method_name ().as_string ());
  visit_collection ("params", e.get_arguments ());

  end ("MethodCallExpr");
}

void
Dump::visit (FieldAccessExpr &e)
{
  begin ("FieldAccessExpr");
  do_expr (e);
  visit_field ("receiver", e.get_receiver_expr ());
  put_field ("field", e.get_field_name ().as_string ());
  end ("FieldAccessExpr");
}

void
Dump::visit (ClosureExpr &e)
{
  begin ("ClosureExpr");
  do_expr (e);

  if (!e.has_params ())
    {
      put_field ("params", "none");
    }
  else
    {
      begin_field ("params");
      for (auto &param : e.get_params ())
	{
	  begin ("ClosureParam");
	  auto oa = param.get_outer_attrs ();
	  do_outer_attrs (oa);
	  visit_field ("pattern", param.get_pattern ());
	  visit_field ("type", param.get_type ());
	  end ("ClosureParam");
	}
      end_field ("params");
    }

  visit_field ("return_type", e.get_return_type ());

  visit_field ("expr", e.get_expr ());
  end ("ClosureExpr");
}

void
Dump::visit (BlockExpr &e)
{
  begin ("BlockExpr");
  do_expr (e);
  do_inner_attrs (e);

  visit_collection ("statements", e.get_statements ());

  visit_field ("expr", e.get_final_expr ());

  end ("BlockExpr");
}

void
Dump::visit (ContinueExpr &e)
{
  begin ("ContinueExpr");

  if (e.has_label ())
    put_field ("label", e.get_label ().as_string ());
  else
    put_field ("label", "none");

  end ("ContinueExpr");
}

void
Dump::visit (BreakExpr &e)
{
  begin ("BreakExpr");
  std::string str ("break ");

  if (e.has_label ())
    put_field ("label", e.get_label ().as_string ());
  else
    put_field ("label", "none");

  visit_field ("break_expr ", e.get_expr ());

  end ("BreakExpr");
}

void
Dump::visit (RangeFromToExpr &e)
{
  begin ("RangeFromToExpr");

  visit_field ("from", e.get_from_expr ());
  visit_field ("to", e.get_to_expr ());

  end ("RangeFromToExpr");
}

void
Dump::visit (RangeFromExpr &e)
{
  begin ("RangeFromExpr");

  visit_field ("from", e.get_from_expr ());

  end ("RangeFromExpr");
}

void
Dump::visit (RangeToExpr &e)
{
  begin ("RangeToExpr");

  visit_field ("to", e.get_to_expr ());

  end ("RangeToExpr");
}

void
Dump::visit (RangeFullExpr &e)
{
  begin ("RangeFullExpr");
  end ("RangeFullExpr");
}

void
Dump::visit (RangeFromToInclExpr &e)
{
  begin ("RangeFromToInclExpr");

  visit_field ("from", e.get_from_expr ());
  visit_field ("to", e.get_to_expr ());

  end ("RangeFromToInclExpr");
}

void
Dump::visit (RangeToInclExpr &e)
{
  begin ("RangeToInclExpr");

  visit_field ("to", e.get_to_expr ());

  end ("RangeToInclExpr");
}

void
Dump::visit (ReturnExpr &e)
{
  begin ("ReturnExpr");
  do_mappings (e.get_mappings ());

  visit_field ("return_expr", e.get_expr ());

  end ("ReturnExpr");
}

void
Dump::visit (UnsafeBlockExpr &e)
{
  begin ("UnsafeBlockExpr");
  auto oa = e.get_outer_attrs ();
  do_outer_attrs (oa);

  visit_field ("block_expr", e.get_block_expr ());

  end ("UnsafeBlockExpr");
}

void
Dump::visit (LoopExpr &e)
{
  begin ("LoopExpr");
  do_baseloopexpr (e);
  end ("LoopExpr");
}

void
Dump::visit (WhileLoopExpr &e)
{
  begin ("WhileLoopExpr");
  do_baseloopexpr (e);

  visit_field ("condition", e.get_predicate_expr ());

  end ("WhileLoopExpr");
}

void
Dump::visit (WhileLetLoopExpr &e)
{
  begin ("WhileLetLoopExpr");
  do_baseloopexpr (e);

  visit_collection ("match_arm_patterns", e.get_patterns ());

  visit_field ("condition", e.get_cond ());

  end ("WhileLetLoopExpr");
}

void
Dump::visit (IfExpr &e)
{
  begin ("IfExpr");
  do_ifexpr (e);
  end ("IfExpr");
}

void
Dump::visit (IfExprConseqElse &e)
{
  begin ("IfExprConseqElse");
  do_ifexpr (e);
  visit_field ("else_block", e.get_else_block ());

  end ("IfExprConseqElse");
}

void
Dump::visit (IfLetExpr &e)
{
  begin ("IfLetExpr");
  do_ifletexpr (e);
  end ("IfLetExpr");
}

void
Dump::visit (IfLetExprConseqElse &e)
{
  begin ("IfLetExprConseqElse");
  do_ifletexpr (e);
  visit_field ("else_block", e.get_else_block ());
  end ("IfLetExprConseqElse");
}

void
Dump::visit (MatchExpr &e)
{
  begin ("MatchExpr");
  do_inner_attrs (e);
  do_expr (e);
  visit_field ("branch_value", e.get_scrutinee_expr ());

  std::string str;
  if (e.get_match_cases ().empty ())
    str = "none";
  else
    for (const auto &arm : e.get_match_cases ())
      str += "\n " + arm.as_string ();
  put_field ("match_arms", str);

  end ("MatchExpr");
}

void
Dump::visit (AwaitExpr &e)
{
  begin ("AwaitExpr");
  do_expr (e);
  visit_field ("awaited_expr", e.get_awaited_expr ());
  end ("AwaitExpr");
}

void
Dump::visit (AsyncBlockExpr &e)
{
  begin ("AsyncBlockExpr");
  do_expr (e);

  put_field ("has move", std::to_string (e.get_has_move ()));
  visit_field ("block_expr", e.get_block_expr ());

  end ("AsyncBlockExpr");
}

void
Dump::visit (TypeParam &e)
{
  begin ("TypeParam");
  put_field ("outer_attr", e.get_outer_attribute ().as_string ());

  put_field ("type_representation", e.get_type_representation ().as_string ());

  visit_collection ("type_param_bounds", e.get_type_param_bounds ());

  visit_field ("type", e.get_type ());

  end ("TypeParam");
}

void
Dump::visit (ConstGenericParam &e)
{
  begin ("ConstGenericParam");
  do_mappings (e.get_mappings ());
  put_field ("name", e.get_name ());
  visit_field ("type", e.get_type ());
  visit_field ("default_expression", e.get_default_expression ());
  end ("ConstGenericParam");
}

void
Dump::visit (LifetimeWhereClauseItem &e)
{
  begin ("LifetimeWhereClauseItem");
  do_mappings (e.get_mappings ());

  visit_field ("lifetime", e.get_lifetime ());
  visit_collection ("lifetime_bounds", e.get_lifetime_bounds ());

  end ("LifetimeWhereClauseItem");
}

void
Dump::visit (TypeBoundWhereClauseItem &e)
{
  begin ("TypeBoundWhereClauseItem");
  do_mappings (e.get_mappings ());

  visit_collection ("for_lifetime", e.get_for_lifetimes ());

  visit_field ("bound_type", e.get_bound_type ());
  visit_collection ("type_param_bound", e.get_type_param_bounds ());
  end ("TypeBoundWhereClauseItem");
}

void
Dump::visit (Module &e)
{
  begin ("Module");
  do_inner_attrs (e);
  put_field ("module_name", e.get_module_name ().as_string ());
  visit_collection ("items", e.get_items ());

  end ("Module");
}

void
Dump::visit (ExternCrate &e)
{
  begin ("ExternCrate");
  do_vis_item (e);
  put_field ("referenced_crate", e.get_referenced_crate ());
  put_field ("as_clause_name", e.get_as_clause_name ());

  end ("ExternCrate");
}

void
Dump::visit (UseTreeGlob &e)
{
  begin ("UseTreeGlob");

  std::string glob, path = "not applicable";
  switch (e.get_glob_type ())
    {
    case UseTreeGlob::PathType::NO_PATH:
      glob = "*";
      break;
    case UseTreeGlob::PathType::GLOBAL:
      glob = "::*";
      break;
      case UseTreeGlob::PathType::PATH_PREFIXED: {
	path = e.get_path ().as_string ();
	glob = "::*";
	break;
      }
    default:
      gcc_unreachable ();
    }
  put_field ("glob", glob);
  put_field ("path", path);

  end ("UseTreeGlob");
}

void
Dump::visit (UseTreeList &e)
{
  begin ("UseTreeList");

  std::string path_type, path = "not applicable";
  switch (e.get_path_type ())
    {
    case UseTreeList::PathType::NO_PATH:
      path_type = "*";
      break;
    case UseTreeList::PathType::GLOBAL:
      path_type = "::*";
      break;
      case UseTreeList::PathType::PATH_PREFIXED: {
	path = e.get_path ().as_string ();
	path_type = "::*";
	break;
      }
    default:
      gcc_unreachable ();
    }
  put_field ("path_type", path_type);
  put_field ("path", path);

  visit_collection ("trees", e.get_trees ());

  end ("UseTreeList");
}

void
Dump::visit (UseTreeRebind &e)
{
  begin ("UseTreeRebind");
  put_field ("path", e.get_path ().as_string ());
  put_field ("identifier", e.get_identifier ().as_string ());
  put_field ("bind_type", enum_to_str (e.get_bind_type ()));
  end ("UseTreeRebind");
}

void
Dump::visit (UseDeclaration &e)
{
  begin ("UseDeclaration");
  do_vis_item (e);

  visit_field ("use_tree", e.get_use_tree ());

  end ("UseDeclaration");
}

void
Dump::visit (Function &e)
{
  begin ("Function");
  do_vis_item (e);

  put_field ("function_qualifiers", e.get_qualifiers ().as_string ());

  visit_collection ("generic_params", e.get_generic_params ());

  put_field ("function_name", e.get_function_name ().as_string ());

  if (e.has_function_params ())
    {
      begin_field ("function_params");
      for (auto &item : e.get_function_params ())
	do_functionparam (item);
      end_field ("function_params");
    }
  else
    {
      put_field ("function_params", "empty");
    }

  visit_field ("return_type", e.get_return_type ());

  if (!e.has_where_clause ())
    put_field ("where_clause", "none");
  else
    put_field ("where clause", e.get_where_clause ().as_string ());

  visit_field ("function_body", e.get_definition ());
  put_field ("self", e.get_self_param ().as_string ());

  end ("Function");
}

void
Dump::visit (TypeAlias &e)
{
  begin ("TypeAlias");

  do_vis_item (e);

  put_field ("new_type_name", e.get_new_type_name ().as_string ());

  visit_collection ("generic_params", e.get_generic_params ());

  if (!e.has_where_clause ())
    put_field ("where_clause", "none");
  else
    put_field ("where clause", e.get_where_clause ().as_string ());

  put_field ("type", e.get_type_aliased ()->as_string ());

  end ("TypeAlias");
}

void
Dump::visit (StructStruct &e)
{
  begin ("StructStruct");
  do_struct (e);

  put_field ("is_unit", std::to_string (e.is_unit_struct ()));

  if (e.get_fields ().empty ())
    put_field ("fields", "empty");
  else
    {
      begin_field ("fields");
      for (auto &field : e.get_fields ())
	{
	  begin ("StructField");
	  do_structfield (field);
	  end ("StructField");
	}
      end_field ("fields");
    }

  end ("StructStruct");
}

void
Dump::visit (TupleStruct &e)
{
  begin ("TupleStruct");
  do_struct (e);

  if (e.get_fields ().empty ())
    put_field ("fields", "empty");
  else
    {
      begin_field ("fields");
      for (auto &field : e.get_fields ())
	{
	  begin ("TupleField");
	  do_tuplefield (field);
	  end ("TupleField");
	}
      end_field ("fields");
    }

  end ("TupleStruct");
}

void
Dump::visit (EnumItem &e)
{
  begin ("EnumItem");
  do_enumitem (e);
  end ("EnumItem");
}

void
Dump::visit (EnumItemTuple &e)
{
  begin ("EnumItemTuple");
  do_enumitem (e);

  if (e.has_tuple_fields ())
    {
      begin_field ("tuple_fields");
      for (auto field : e.get_tuple_fields ())
	do_tuplefield (field);
      end_field ("tuple_fields");
    }
  else
    put_field ("tuple_fields", "empty");

  end ("EnumItemTuple");
}

void
Dump::visit (EnumItemStruct &e)
{
  begin ("EnumItemStruct");
  do_enumitem (e);

  if (e.has_struct_fields ())
    {
      begin_field ("struct_fields");
      for (auto field : e.get_struct_fields ())
	do_structfield (field);
      end_field ("struct_fields");
    }
  else
    {
      put_field ("struct_fields", "empty");
    }
  end ("EnumItemStruct");
}

void
Dump::visit (EnumItemDiscriminant &e)
{
  begin ("EnumItemDiscriminant");

  do_enumitem (e);

  visit_field ("discriminant", e.get_discriminant_expression ());

  end ("EnumItemDiscriminant");
}

void
Dump::visit (Enum &e)
{
  begin ("Enum");
  do_vis_item (e);

  put_field ("enum_name", e.get_identifier ().as_string ());

  visit_collection ("generic_params", e.get_generic_params ());

  std::string str = "none";
  if (e.has_where_clause ())
    str = e.get_where_clause ().as_string ();
  put_field ("where clause", str);

  visit_collection ("items", e.get_variants ());

  end ("Enum");
}

void
Dump::visit (Union &e)
{
  begin ("Union");
  do_vis_item (e);

  visit_collection ("generic_params", e.get_generic_params ());

  std::string str;
  if (e.has_where_clause ())
    str = e.get_where_clause ().as_string ();
  else
    str = "none";
  put_field ("where clause", str);

  if (e.get_variants ().empty ())
    {
      put_field ("variants", "empty");
    }
  else
    {
      begin_field ("variants");
      for (auto &elt : e.get_variants ())
	{
	  begin ("StructField");
	  auto oa = e.get_outer_attrs ();
	  do_outer_attrs (oa);

	  std::string str = "none";
	  if (elt.has_visibility ())
	    str = elt.get_visibility ().as_string ();
	  put_field ("visibility", str);
	  put_field ("field_name", elt.get_field_name ().as_string ());
	  visit_field ("field_type", elt.get_field_type ());
	  end ("StructField");
	}
      end_field ("variants");
    }
  end ("Union");
}

void
Dump::visit (ConstantItem &e)
{
  begin ("ConstantItem");
  do_vis_item (e);
  put_field ("identifier", e.get_identifier ().as_string ());
  visit_field ("type", e.get_type ());
  visit_field ("const_expr", e.get_expr ());
  end ("ConstantItem");
}

void
Dump::visit (StaticItem &e)
{
  begin ("StaticItem");
  do_vis_item (e);
  put_field ("mut", std::to_string (e.is_mut ()));
  put_field ("name", e.get_identifier ().as_string ());
  visit_field ("type", e.get_type ());
  visit_field ("expr", e.get_expr ());
  end ("StaticItem");
}

void
Dump::visit (TraitItemFunc &e)
{
  begin ("TraitItemFunc");
  do_traititem (e);

  do_traitfunctiondecl (e.get_decl ());

  visit_field ("block_expr", e.get_block_expr ());

  end ("TraitItemFunc");
}

void
Dump::visit (TraitItemConst &e)
{
  begin ("TraitItemConst");
  do_traititem (e);

  put_field ("name", e.get_name ().as_string ());
  visit_field ("type", e.get_type ());
  visit_field ("expr", e.get_expr ());
  end ("TraitItemConst");
}

void
Dump::visit (TraitItemType &e)
{
  begin ("TraitItemType");
  do_traititem (e);

  put_field ("name", e.get_name ().as_string ());
  visit_collection ("type_param_bounds", e.get_type_param_bounds ());
  end ("TraitItemType");
}

void
Dump::visit (Trait &e)
{
  begin ("Trait");
  do_vis_item (e);
  put_field ("unsafety", std::to_string (e.is_unsafe ()));
  put_field ("name", e.get_name ().as_string ());

  visit_collection ("generic_params", e.get_generic_params ());

  visit_collection ("type_param_bounds", e.get_type_param_bounds ());

  std::string str;
  if (e.has_where_clause ())
    str = e.get_where_clause ().as_string ();
  else
    str = "none";
  put_field ("where clause", str);

  visit_collection ("trait_items", e.get_trait_items ());

  end ("Trait");
}

void
Dump::visit (ImplBlock &e)
{
  begin ("ImplBlock");
  do_vis_item (e);

  visit_collection ("generic_params", e.get_generic_params ());

  visit_field ("impl_type", e.get_type ());

  std::string str;
  if (e.has_where_clause ())
    str = e.get_where_clause ().as_string ();
  else
    str = "none";
  put_field ("where clause", str);

  do_inner_attrs (e);

  visit_collection ("impl_items", e.get_impl_items ());

  end ("ImplBlock");
}

void
Dump::visit (ExternalStaticItem &e)
{
  begin ("ExternalStaticItem");

  // FIXME do_vis_item... but not a VisItem... But could be?
  auto oa = e.get_outer_attrs ();
  do_outer_attrs (oa);

  std::string str = "none";
  if (e.has_visibility ())
    str = e.get_visibility ().as_string ();
  put_field ("visibility", str);
  //

  put_field ("mut", std::to_string (e.is_mut ()));
  put_field ("name", e.get_item_name ().as_string ());
  visit_field ("type", e.get_item_type ());

  end ("ExternalStaticItem");
}

void
Dump::visit (ExternalFunctionItem &e)
{
  begin ("ExternalFunctionItem");
  do_externalitem (e);

  visit_collection ("generic_params", e.get_generic_params ());

  std::string str = "none";
  if (!e.get_function_params ().empty ())
    for (auto &param : e.get_function_params ())
      do_namefunctionparam (param);
  else
    put_field ("function_params", "none");

  put_field ("has_variadics", std::to_string (e.is_variadic ()));

  visit_field ("return_type", e.get_return_type ());

  end ("ExternalFunctionItem");
}

void
Dump::visit (ExternalTypeItem &e)
{
  begin ("ExternalTypeItem");

  do_externalitem (e);

  end ("ExternalTypeItem");
}

void
Dump::visit (ExternBlock &e)
{
  begin ("ExternBlock");
  do_vis_item (e);
  do_inner_attrs (e);

  put_field ("abi", get_string_from_abi (e.get_abi ()));

  visit_collection ("extern_items", e.get_extern_items ());

  end ("ExternBlock");
}

void
Dump::visit (LiteralPattern &e)
{
  begin ("LiteralPattern");
  put_field ("lit", e.get_literal ().as_string ());
  do_mappings (e.get_mappings ());
  end ("LiteralPattern");
}

void
Dump::visit (IdentifierPattern &e)
{
  begin ("IdentifierPattern");
  put_field ("variable_ident", e.get_identifier ().as_string ());
  put_field ("is_ref", std::to_string (e.get_is_ref ()));
  put_field ("mut", std::to_string (e.is_mut ()));

  if (e.has_pattern_to_bind ())
    put_field ("to_bind", e.get_to_bind ()->as_string ());
  else
    put_field ("to_bind", "none");

  end ("IdentifierPattern");
}
void
Dump::visit (WildcardPattern &e)
{
  begin ("WildcardPattern");
  do_mappings (e.get_mappings ());
  end ("WildcardPattern");
}

void
Dump::visit (RangePatternBoundLiteral &e)
{
  begin ("RangePatternBoundLiteral");
  put_field ("literal", e.get_literal ().as_string ());
  put_field ("has_minus", std::to_string (e.get_has_minus ()));
  end ("RangePatternBoundLiteral");
}

void
Dump::visit (RangePatternBoundPath &e)
{
  begin ("RangePatternBoundPath");
  put_field ("path", e.get_path ().as_string ());
  end ("RangePatternBoundPath");
}

void
Dump::visit (RangePatternBoundQualPath &e)
{
  begin ("RangePatternBoundQualPath");
  visit_field ("path", e.get_qualified_path ());
  end ("RangePatternBoundQualPath");
}

void
Dump::visit (RangePattern &e)
{
  begin ("RangePattern");
  do_mappings (e.get_mappings ());
  put_field ("lower", e.get_lower_bound ()->as_string ());
  put_field ("upper", e.get_upper_bound ()->as_string ());
  put_field ("has_ellipsis_syntax",
	     std::to_string (e.get_has_ellipsis_syntax ()));
  end ("RangePattern");
}

void
Dump::visit (ReferencePattern &e)
{
  begin ("ReferencePattern");
  do_mappings (e.get_mappings ());
  put_field ("mut", std::to_string (e.is_mut ()));
  put_field ("pattern", e.get_referenced_pattern ()->as_string ());
  end ("ReferencePattern");
}

void
Dump::visit (StructPatternFieldTuplePat &e)
{
  begin ("StructPatternFieldTuplePat");
  do_mappings (e.get_mappings ());
  auto oa = e.get_outer_attrs ();
  do_outer_attrs (oa);
  put_field ("index", std::to_string (e.get_index ()));
  put_field ("tuple_pattern", e.get_tuple_pattern ()->as_string ());
  end ("StructPatternFieldTuplePat");
}

void
Dump::visit (StructPatternFieldIdentPat &e)
{
  begin ("StructPatternFieldIdentPat");
  auto oa = e.get_outer_attrs ();
  do_outer_attrs (oa);
  put_field ("ident", e.get_identifier ().as_string ());
  put_field ("ident_pattern", e.get_pattern ()->as_string ());
  end ("StructPatternFieldIdentPat");
}

void
Dump::visit (StructPatternFieldIdent &e)
{
  begin ("StructPatternFieldIdent");
  auto oa = e.get_outer_attrs ();
  do_outer_attrs (oa);

  put_field ("has_ref", std::to_string (e.get_has_ref ()));
  put_field ("mut", std::to_string (e.is_mut ()));
  put_field ("ident", e.get_identifier ().as_string ());
  end ("StructPatternFieldIdent");
}

void
Dump::visit (StructPattern &e)
{
  begin ("StructPattern");

  visit_field ("path", e.get_path ());
  put_field ("elems", e.get_struct_pattern_elems ().as_string ());

  end ("StructPattern");
}

void
Dump::visit (TupleStructItemsNoRange &e)
{
  begin ("TupleStructItemsNoRange");
  visit_collection ("patterns", e.get_patterns ());
  end ("TupleStructItemsNoRange");
}

void
Dump::visit (TupleStructItemsRange &e)
{
  begin ("TupleStructItemsRange");
  visit_collection ("lower_patterns", e.get_lower_patterns ());
  visit_collection ("upper_patterns", e.get_upper_patterns ());
  end ("TupleStructItemsRange");
}

void
Dump::visit (TupleStructPattern &e)
{
  begin ("TupleStructPattern");
  do_mappings (e.get_mappings ());

  put_field ("path", e.get_path ().as_string ());

  visit_field ("items", e.get_items ());

  end ("TupleStructPattern");
}

void
Dump::visit (TuplePatternItemsMultiple &e)
{
  begin ("TuplePatternItemsMultiple");
  visit_collection ("patterns", e.get_patterns ());
  end ("TuplePatternItemsMultiple");
}

void
Dump::visit (TuplePatternItemsRanged &e)
{
  begin ("TuplePatternItemsRanged");
  visit_collection ("lower_patterns", e.get_lower_patterns ());
  visit_collection ("upper_patterns", e.get_upper_patterns ());
  end ("TuplePatternItemsRanged");
}

void
Dump::visit (TuplePattern &e)
{
  begin ("TuplePattern");
  do_mappings (e.get_mappings ());
  visit_field ("items", e.get_items ());
  end ("TuplePattern");
}

void
Dump::visit (SlicePattern &e)
{
  begin ("SlicePattern");
  do_mappings (e.get_mappings ());
  visit_collection ("items", e.get_items ());
  end ("SlicePattern");
}

void
Dump::visit (AltPattern &e)
{
  begin ("AltPattern");
  do_mappings (e.get_mappings ());
  visit_collection ("alts", e.get_alts ());
  end ("AltPattern");
}

void
Dump::visit (EmptyStmt &e)
{
  begin ("EmptyStmt");
  do_stmt (e);
  end ("EmptyStmt");
}

void
Dump::visit (LetStmt &e)
{
  begin ("LetStmt");
  do_stmt (e);
  auto oa = e.get_outer_attrs ();
  do_outer_attrs (oa);

  put_field ("variable_pattern", e.get_pattern ()->as_string ());

  visit_field ("type", e.get_type ());
  visit_field ("init_expr", e.get_init_expr ());

  end ("LetStmt");
}

void
Dump::visit (ExprStmt &e)
{
  begin ("ExprStmt");
  do_stmt (e);
  put_field ("must_be_unit", std::to_string (e.is_unit_check_needed ()));
  visit_field ("expr", e.get_expr ());
  end ("ExprStmt");
}

void
Dump::visit (TraitBound &e)
{
  begin ("TraitBound");
  do_mappings (e.get_mappings ());
  put_field ("in_parens", std::to_string (e.get_in_parens ()));
  put_field ("polarity", BoundPolarityString (e.get_polarity ()));

  visit_collection ("for_lifetime", e.get_for_lifetimes ());
  visit_field ("type_path", e.get_path ());

  end ("TraitBound");
}

void
Dump::visit (ImplTraitType &e)
{
  begin ("ImplTraitType");
  do_type (e);

  visit_collection ("type_param_bounds", e.get_type_param_bounds ());

  end ("ImplTraitType");
}

void
Dump::visit (TraitObjectType &e)
{
  begin ("TraitObjectType");
  do_type (e);

  put_field ("has_dyn", std::to_string (e.get_has_dyn ()));

  visit_collection ("type_param_bounds", e.get_type_param_bounds ());

  end ("TraitObjectType");
}

void
Dump::visit (ParenthesisedType &e)
{
  begin ("ParenthesisedType");
  do_type (e);
  put_field ("type_in_parens", e.get_type_in_parens ()->as_string ());
  end ("ParenthesisedType");
}

void
Dump::visit (ImplTraitTypeOneBound &e)
{
  begin ("ImplTraitTypeOneBound");
  do_type (e);
  visit_field ("trait_bound", e.get_trait_bound ());
  end ("ImplTraitTypeOneBound");
}

void
Dump::visit (TupleType &e)
{
  begin ("TupleType");
  do_type (e);
  visit_collection ("elems", e.get_elems ());
  end ("TupleType");
}

void
Dump::visit (NeverType &e)
{
  begin ("NeverType");
  do_type (e);
  end ("NeverType");
}

void
Dump::visit (RawPointerType &e)
{
  begin ("RawPointerType");
  do_type (e);
  put_field ("mut", Rust::enum_to_str (e.get_mut ()));
  put_field ("type", e.get_type ()->as_string ());
  end ("RawPointerType");
}

void
Dump::visit (ReferenceType &e)
{
  begin ("ReferenceType");
  do_type (e);
  put_field ("lifetime", e.get_lifetime ().as_string ());
  put_field ("mut", enum_to_str (e.get_mut ()));
  put_field ("type", e.get_base_type ()->as_string ());
  end ("ReferenceType");
}

void
Dump::visit (ArrayType &e)
{
  begin ("ArrayType");
  do_type (e);
  visit_field ("type", e.get_element_type ());
  visit_field ("size", e.get_size_expr ());
  end ("ArrayType");
}

void
Dump::visit (SliceType &e)
{
  begin ("SliceType");
  do_type (e);
  visit_field ("elem_type", e.get_element_type ());
  end ("SliceType");
}

void
Dump::visit (InferredType &e)
{
  begin ("InferredType");
  do_type (e);
  end ("InferredType");
}

void
Dump::visit (BareFunctionType &e)
{
  begin ("BareFunctionType");
  do_type (e);

  visit_collection ("for_lifetimes", e.get_for_lifetimes ());

  put_field ("function_qualifiers", e.get_function_qualifiers ().as_string ());

  if (e.get_function_params ().empty ())
    {
      put_field ("params", "none");
    }
  else
    {
      begin_field ("params");
      for (auto &param : e.get_function_params ())
	{
	  begin ("MaybeNamedParam");
	  do_maybenamedparam (param);
	  end ("MaybeNamedParam");
	}
      end_field ("params");
    }

  visit_field ("return_type", e.get_return_type ());
  put_field ("is_variadic", std::to_string (e.get_is_variadic ()));
  end ("BareFunctionType");
}

} // namespace HIR
} // namespace Rust

// In the global namespace to make it easier to call from debugger
void
debug (Rust::HIR::FullVisitable &v)
{
  Rust::HIR::Dump::debug (v);
}
