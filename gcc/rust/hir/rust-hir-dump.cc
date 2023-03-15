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

#include "rust-hir-dump.h"

namespace Rust {
namespace HIR {

Dump::Dump (std::ostream &stream) : stream (stream) {}

void
Dump::go (HIR::Crate &crate)
{
  stream << "Crate {" << std::endl;
  // inner attributes
  if (!crate.inner_attrs.empty ())
    {
      indentation.increment ();
      stream << indentation;
      stream << "inner_attrs: [";
      for (auto &attr : crate.inner_attrs)
	stream << attr.as_string ();
      stream << "]," << std::endl;
      indentation.decrement ();
    }

  indentation.increment ();
  stream << indentation;
  //

  stream << "items: [";

  stream << indentation;
  for (const auto &item : crate.items)
    {
      stream << std::endl;
      item->accept_vis (*this);
    }
  stream << std::endl;
  stream << indentation;

  stream << "]," << std::endl;
  indentation.decrement ();
  //

  indentation.increment ();
  stream << indentation;
  stream << "node_mappings: ";
  stream << crate.get_mappings ().as_string ();
  indentation.decrement ();

  stream << "\n}" << std::endl;
}

void
Dump::visit (AST::Attribute &attribute)
{
  std::string path_str = attribute.get_path ().as_string ();
  stream << path_str;
  if (attribute.has_attr_input ())
    stream << attribute.get_attr_input ().as_string ();
}

void
Dump::visit (Lifetime &lifetime)
{
  if (lifetime.is_error ())
    {
      stream << "error lifetime";
      return;
    }

  switch (lifetime.get_lifetime_type ())
    {
    case AST::Lifetime::LifetimeType::NAMED:
      stream << "'" << lifetime.get_name ();
      break;
    case AST::Lifetime::LifetimeType::STATIC:
      stream << "'static";
      break;
    case AST::Lifetime::LifetimeType::WILDCARD:
      stream << "'_";
      break;
    default:
      stream << "ERROR-MARK-STRING: lifetime type failure";
      break;
    }
}
void
Dump::visit (LifetimeParam &)
{}
void
Dump::visit (PathInExpression &)
{}
void
Dump::visit (TypePathSegment &)
{}
void
Dump::visit (TypePathSegmentGeneric &)
{}
void
Dump::visit (TypePathSegmentFunction &)
{}
void
Dump::visit (TypePath &)
{}
void
Dump::visit (QualifiedPathInExpression &)
{}
void
Dump::visit (QualifiedPathInType &)
{}

void
Dump::visit (LiteralExpr &literal_expr)
{
  stream << literal_expr.get_literal ().as_string () << " "
	 << literal_expr.get_mappings ().as_string ();
}
void
Dump::visit (BorrowExpr &)
{}
void
Dump::visit (DereferenceExpr &)
{}
void
Dump::visit (ErrorPropagationExpr &)
{}
void
Dump::visit (NegationExpr &)
{}
void
Dump::visit (ArithmeticOrLogicalExpr &aole)
{
  std::string operator_str;
  operator_str.reserve (1);

  // which operator
  switch (aole.get_expr_type ())
    {
    case ArithmeticOrLogicalOperator::ADD:
      operator_str = "+";
      break;
    case ArithmeticOrLogicalOperator::SUBTRACT:
      operator_str = "-";
      break;
    case ArithmeticOrLogicalOperator::MULTIPLY:
      operator_str = "*";
      break;
    case ArithmeticOrLogicalOperator::DIVIDE:
      operator_str = "/";
      break;
    case ArithmeticOrLogicalOperator::MODULUS:
      operator_str = "%";
      break;
    case ArithmeticOrLogicalOperator::BITWISE_AND:
      operator_str = "&";
      break;
    case ArithmeticOrLogicalOperator::BITWISE_OR:
      operator_str = "|";
      break;
    case ArithmeticOrLogicalOperator::BITWISE_XOR:
      operator_str = "^";
      break;
    case ArithmeticOrLogicalOperator::LEFT_SHIFT:
      operator_str = "<<";
      break;
    case ArithmeticOrLogicalOperator::RIGHT_SHIFT:
      operator_str = ">>";
      break;
    default:
      gcc_unreachable ();
      break;
    }

  aole.visit_lhs (*this);
  stream << "\n";
  stream << indentation;
  stream << operator_str << "\n";
  stream << indentation;
  aole.visit_rhs (*this);
}
void
Dump::visit (ComparisonExpr &)
{}
void
Dump::visit (LazyBooleanExpr &)
{}
void
Dump::visit (TypeCastExpr &)
{}
void
Dump::visit (AssignmentExpr &)
{}
void
Dump::visit (CompoundAssignmentExpr &)
{}
void
Dump::visit (GroupedExpr &)
{}

void
Dump::visit (ArrayElemsValues &)
{}
void
Dump::visit (ArrayElemsCopied &)
{}
void
Dump::visit (ArrayExpr &)
{}
void
Dump::visit (ArrayIndexExpr &)
{}
void
Dump::visit (TupleExpr &)
{}
void
Dump::visit (TupleIndexExpr &)
{}
void
Dump::visit (StructExprStruct &)
{}

void
Dump::visit (StructExprFieldIdentifier &)
{}
void
Dump::visit (StructExprFieldIdentifierValue &)
{}

void
Dump::visit (StructExprFieldIndexValue &)
{}
void
Dump::visit (StructExprStructFields &)
{}
void
Dump::visit (StructExprStructBase &)
{}

void
Dump::visit (CallExpr &)
{}
void
Dump::visit (MethodCallExpr &)
{}
void
Dump::visit (FieldAccessExpr &)
{}
void
Dump::visit (ClosureExpr &)
{}
void
Dump::visit (BlockExpr &block_expr)
{
  stream << "BlockExpr: [\n";

  indentation.increment ();
  // TODO: inner attributes
  if (!block_expr.inner_attrs.empty ())
    {
      stream << indentation << "inner_attrs: [";
      indentation.increment ();
      for (auto &attr : block_expr.inner_attrs)
	{
	  stream << "\n";
	  stream << indentation;
	  visit (attr);
	}
      indentation.decrement ();
      stream << "\n" << indentation << "]\n";
    }

  // statements
  // impl null pointer check

  if (block_expr.has_statements ())
    {
      auto &stmts = block_expr.get_statements ();
      for (auto &stmt : stmts)
	{
	  stream << indentation << "Stmt: {\n";
	  stmt->accept_vis (*this);
	  stream << "\n";
	  stream << indentation << "}\n";
	}
    }

  // final expression
  if (block_expr.has_expr ())
    {
      stream << indentation << "final expression:";
      stream << "\n" << indentation << block_expr.expr->as_string ();
    }

  indentation.decrement ();
  stream << "\n" << indentation << "]";
}

void
Dump::visit (ContinueExpr &)
{}
void
Dump::visit (BreakExpr &)
{}
void
Dump::visit (RangeFromToExpr &)
{}
void
Dump::visit (RangeFromExpr &)
{}
void
Dump::visit (RangeToExpr &)
{}
void
Dump::visit (RangeFullExpr &)
{}
void
Dump::visit (RangeFromToInclExpr &)
{}
void
Dump::visit (RangeToInclExpr &)
{}
void
Dump::visit (ReturnExpr &)
{}
void
Dump::visit (UnsafeBlockExpr &)
{}
void
Dump::visit (LoopExpr &)
{}
void
Dump::visit (WhileLoopExpr &)
{}
void
Dump::visit (WhileLetLoopExpr &)
{}
void
Dump::visit (ForLoopExpr &)
{}
void
Dump::visit (IfExpr &)
{}
void
Dump::visit (IfExprConseqElse &)
{}
void
Dump::visit (IfLetExpr &)
{}
void
Dump::visit (IfLetExprConseqElse &)
{}

void
Dump::visit (MatchExpr &)
{}
void
Dump::visit (AwaitExpr &)
{}
void
Dump::visit (AsyncBlockExpr &)
{}

void
Dump::visit (TypeParam &)
{}

void
Dump::visit (ConstGenericParam &)
{}

void
Dump::visit (LifetimeWhereClauseItem &)
{}
void
Dump::visit (TypeBoundWhereClauseItem &)
{}
void
Dump::visit (Module &)
{}
void
Dump::visit (ExternCrate &)
{}

void
Dump::visit (UseTreeGlob &)
{}
void
Dump::visit (UseTreeList &)
{}
void
Dump::visit (UseTreeRebind &)
{}
void
Dump::visit (UseDeclaration &)
{}
void
Dump::visit (Function &func)
{
  indentation.increment ();
  stream << indentation << "Function {" << std::endl;
  indentation.increment ();

  // function name
  stream << indentation << "func_name: ";
  auto func_name = func.get_function_name ();
  stream << func_name;
  stream << ",\n";

  // return type
  stream << indentation << "return_type: ";
  if (func.has_return_type ())
    {
      auto &ret_type = func.get_return_type ();
      stream << ret_type->as_string ();
      stream << ",\n";
    }
  else
    {
      stream << "void,\n";
    }

  // function params
  if (func.has_function_params ())
    {
      stream << indentation << "params: [\n";
      indentation.increment ();
      auto &func_params = func.get_function_params ();
      for (const auto &item : func_params)
	{
	  stream << indentation << item.as_string () << ",\n";
	}

      // parameter node mappings
      stream << indentation << "node_mappings: [\n";
      for (const auto &item : func_params)
	{
	  auto nmap = item.get_mappings ();
	  indentation.increment ();
	  stream << indentation;
	  auto pname = item.param_name->as_string ();
	  stream << pname << ": ";
	  stream << nmap.as_string () << ",\n";
	  indentation.decrement ();
	}
      stream << indentation << "],";
      indentation.decrement ();
      stream << "\n";
      stream << indentation << "],";
      stream << "\n";
    }

  // function body
  stream << indentation;
  auto &func_body = func.get_definition ();
  func_body->accept_vis (*this);

  // func node mappings
  stream << "\n";
  stream << indentation << "node_mappings: ";
  stream << func.get_impl_mappings ().as_string ();
  indentation.decrement ();
  stream << "\n";
  stream << indentation << "}" << std::endl;
  // TODO: get function definition and visit block

  // stream << std::endl;
  indentation.decrement ();
}
void
Dump::visit (TypeAlias &)
{}
void
Dump::visit (StructStruct &)
{}
void
Dump::visit (TupleStruct &)
{}
void
Dump::visit (EnumItem &)
{}
void
Dump::visit (EnumItemTuple &)
{}
void
Dump::visit (EnumItemStruct &)
{}
void
Dump::visit (EnumItemDiscriminant &)
{}
void
Dump::visit (Enum &)
{}
void
Dump::visit (Union &)
{}
void
Dump::visit (ConstantItem &)
{}
void
Dump::visit (StaticItem &)
{}
void
Dump::visit (TraitItemFunc &)
{}
void
Dump::visit (TraitItemConst &)
{}
void
Dump::visit (TraitItemType &)
{}
void
Dump::visit (Trait &)
{}
void
Dump::visit (ImplBlock &)
{}

void
Dump::visit (ExternalStaticItem &)
{}
void
Dump::visit (ExternalFunctionItem &)
{}
void
Dump::visit (ExternBlock &)
{}

void
Dump::visit (LiteralPattern &)
{}
void
Dump::visit (IdentifierPattern &ident)
{
  auto ident_name = ident.get_identifier ();
  stream << ident_name;
}
void
Dump::visit (WildcardPattern &)
{}

void
Dump::visit (RangePatternBoundLiteral &)
{}
void
Dump::visit (RangePatternBoundPath &)
{}
void
Dump::visit (RangePatternBoundQualPath &)
{}
void
Dump::visit (RangePattern &)
{}
void
Dump::visit (ReferencePattern &)
{}

void
Dump::visit (StructPatternFieldTuplePat &)
{}
void
Dump::visit (StructPatternFieldIdentPat &)
{}
void
Dump::visit (StructPatternFieldIdent &)
{}
void
Dump::visit (StructPattern &)
{}

void
Dump::visit (TupleStructItemsNoRange &)
{}
void
Dump::visit (TupleStructItemsRange &)
{}
void
Dump::visit (TupleStructPattern &)
{}

void
Dump::visit (TuplePatternItemsMultiple &)
{}
void
Dump::visit (TuplePatternItemsRanged &)
{}
void
Dump::visit (TuplePattern &)
{}
void
Dump::visit (SlicePattern &)
{}
void
Dump::visit (AltPattern &)
{}

void
Dump::visit (EmptyStmt &)
{}
void
Dump::visit (LetStmt &let_stmt)
{
  indentation.increment ();
  // TODO: outer attributes
  stream << indentation << "LetStmt: {\n";
  indentation.increment ();
  stream << indentation;

  auto var_pattern = let_stmt.get_pattern ();
  stream << var_pattern->as_string ();
  // return type
  if (let_stmt.has_type ())
    {
      auto ret_type = let_stmt.get_type ();
      stream << ": " << ret_type->as_string ();
    }

  // init expr
  if (let_stmt.has_init_expr ())
    {
      stream << " = Expr: {\n ";
      indentation.increment ();
      stream << indentation;
      auto expr = let_stmt.get_init_expr ();
      expr->accept_vis (*this);
      stream << "\n";
      stream << indentation << "}\n";
      indentation.decrement ();
    }
  indentation.decrement ();
  stream << indentation << "}\n";

  indentation.decrement ();
}
void
Dump::visit (ExprStmtWithoutBlock &expr_stmt)
{
  auto expr = expr_stmt.get_expr ();
  expr->accept_vis (*this);
}
void
Dump::visit (ExprStmtWithBlock &)
{}

void
Dump::visit (TraitBound &)
{}
void
Dump::visit (ImplTraitType &)
{}
void
Dump::visit (TraitObjectType &)
{}
void
Dump::visit (ParenthesisedType &)
{}
void
Dump::visit (ImplTraitTypeOneBound &)
{}
void
Dump::visit (TupleType &)
{}
void
Dump::visit (NeverType &)
{}
void
Dump::visit (RawPointerType &)
{}
void
Dump::visit (ReferenceType &)
{}
void
Dump::visit (ArrayType &)
{}
void
Dump::visit (SliceType &)
{}
void
Dump::visit (InferredType &)
{}
void
Dump::visit (BareFunctionType &)
{}
} // namespace HIR
} // namespace Rust
