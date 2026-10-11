// Copyright (C) 2026 Free Software Foundation, Inc.

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

#include "rust-attribute-checker.h"
#include "rust-attributes.h"
#include "rust-attribute-values.h"
#include "rust-diagnostics.h"
#include "rust-session-manager.h"

namespace Rust {
namespace Analysis {

using Attrs = Values::Attributes;

static bool
is_proc_macro_type (const AST::Attribute &attribute)
{
  auto result_opt = lookup_builtin (attribute);
  if (!result_opt.has_value ())
    return false;
  auto result = result_opt.value ();

  auto name = result.name;
  return name == Attrs::PROC_MACRO || name == Attrs::PROC_MACRO_DERIVE
	 || name == Attrs::PROC_MACRO_ATTRIBUTE;
}

// Emit an error when one encountered attribute is either #[proc_macro],
// #[proc_macro_attribute] or #[proc_macro_derive]
static void
check_proc_macro_non_function (const AST::Attribute &attr)
{
  if (is_proc_macro_type (attr))
    rust_error_at (attr.get_locus (),
		   "the %<#[%s]%> attribute may only be used on bare functions",
		   attr.get_path ().get_segments ()[0].as_string ().c_str ());
}

// Emit an error when one attribute is either proc_macro, proc_macro_attribute
// or proc_macro_derive
static void
check_proc_macro_non_root (const AST::Attribute &attr, location_t loc)
{
  if (is_proc_macro_type (attr))
    {
      rust_error_at (
	loc,
	"functions tagged with %<#[%s]%> must currently "
	"reside in the root of the crate",
	attr.get_path ().get_segments ().at (0).as_string ().c_str ());
    }
}

AttributeChecker::AttributeChecker () {}

void
AttributeChecker::go (AST::Crate &crate)
{
  visit (crate);
}

void
AttributeChecker::visit (AST::Attribute &attribute)
{
  auto &session = Session::get_instance ();
  if (attribute.get_path () == Values::Attributes::CFG_ATTR)
    {
      if (!attribute.is_parsed_to_meta_item ())
	attribute.parse_attr_to_meta_item ();
      if (!attribute.check_cfg_predicate (session))
	return; // Do not emit errors for attribute that'll get stripped.
    }

  AST::DefaultASTVisitor::visit (attribute);
}

void
AttributeChecker::visit (AST::Token &)
{}

void
AttributeChecker::visit (AST::DelimTokenTree &ttree)
{
  // Checking if the DelimTokenTree contains '('
  // or something else. If something else, then error
  const auto &token_stream = ttree.to_token_stream ();
  for (auto &token : token_stream)
    {
      switch (token.get ()->get_id ())
	{
	case TokenId::LEFT_PAREN:
	  // correct case
	  break;
	case TokenId::LEFT_CURLY:
	case TokenId::LEFT_SQUARE:
	  rust_error_at (token.get ()->get_locus (),
			 "wrong meta list delimiters");
	  rust_inform (token.get ()->get_locus (),
		       "the delimiters should be %<(%> and %<)%>");
	  break;
	default:
	  break;
	}
    }
}

void
AttributeChecker::visit (AST::IdentifierExpr &)
{}

void
AttributeChecker::visit (AST::Lifetime &)
{}

void
AttributeChecker::visit (AST::LifetimeParam &)
{}

void
AttributeChecker::visit (AST::ConstGenericParam &)
{}

// rust-path.h
void
AttributeChecker::visit (AST::PathInExpression &)
{}

void
AttributeChecker::visit (AST::TypePathSegment &)
{}

void
AttributeChecker::visit (AST::TypePathSegmentGeneric &)
{}

void
AttributeChecker::visit (AST::TypePathSegmentFunction &)
{}

void
AttributeChecker::visit (AST::TypePath &)
{}

void
AttributeChecker::visit (AST::QualifiedPathInExpression &)
{}

void
AttributeChecker::visit (AST::QualifiedPathInType &)
{}

// rust-expr.h
void
AttributeChecker::visit (AST::LiteralExpr &)
{}

void
AttributeChecker::visit (AST::AttrInputLiteral &)
{}

void
AttributeChecker::visit (AST::MetaItemLitExpr &)
{}

void
AttributeChecker::visit (AST::MetaItemPathExpr &attribute)
{
  if (!attribute.get_expr ().is_literal ())
    {
      rust_error_at (attribute.get_expr ().get_locus (),
		     "malformed %<path%> attribute input");
      rust_inform (attribute.get_expr ().get_locus (),
		   "must be of the form: %<#[path = \"file\"]%>");
    }
}

void
AttributeChecker::visit (AST::BorrowExpr &)
{}

void
AttributeChecker::visit (AST::DereferenceExpr &)
{}

void
AttributeChecker::visit (AST::ErrorPropagationExpr &)
{}

void
AttributeChecker::visit (AST::NegationExpr &)
{}

void
AttributeChecker::visit (AST::ArithmeticOrLogicalExpr &)
{}

void
AttributeChecker::visit (AST::ComparisonExpr &)
{}

void
AttributeChecker::visit (AST::LazyBooleanExpr &)
{}

void
AttributeChecker::visit (AST::TypeCastExpr &)
{}

void
AttributeChecker::visit (AST::AssignmentExpr &)
{}

void
AttributeChecker::visit (AST::CompoundAssignmentExpr &)
{}

void
AttributeChecker::visit (AST::GroupedExpr &)
{}

void
AttributeChecker::visit (AST::ArrayElemsValues &)
{}

void
AttributeChecker::visit (AST::ArrayElemsCopied &)
{}

void
AttributeChecker::visit (AST::ArrayExpr &)
{}

void
AttributeChecker::visit (AST::ArrayIndexExpr &)
{}

void
AttributeChecker::visit (AST::TupleExpr &)
{}

void
AttributeChecker::visit (AST::TupleIndexExpr &)
{}

void
AttributeChecker::visit (AST::StructExprStruct &)
{}

void
AttributeChecker::visit (AST::StructExprFieldIdentifier &)
{}

void
AttributeChecker::visit (AST::StructExprFieldIdentifierValue &)
{}

void
AttributeChecker::visit (AST::StructExprFieldIndexValue &)
{}

void
AttributeChecker::visit (AST::StructExprStructFields &)
{}

void
AttributeChecker::visit (AST::StructExprStructBase &)
{}

void
AttributeChecker::visit (AST::CallExpr &)
{}

void
AttributeChecker::visit (AST::MethodCallExpr &)
{}

void
AttributeChecker::visit (AST::FieldAccessExpr &)
{}

void
AttributeChecker::visit (AST::ClosureExprInner &)
{}

void
AttributeChecker::visit (AST::BlockExpr &expr)
{
  for (auto &stmt : expr.get_statements ())
    {
      if (stmt->get_stmt_kind () == AST::Stmt::Kind::Item)
	{
	  // Non owning pointer, let it go out of scope
	  auto item = static_cast<AST::Item *> (stmt.get ());
	  for (auto &attr : item->get_outer_attrs ())
	    check_proc_macro_non_root (attr, item->get_locus ());
	}
    }
  AST::DefaultASTVisitor::visit (expr);
}

void
AttributeChecker::visit (AST::ClosureExprInnerTyped &)
{}

void
AttributeChecker::visit (AST::ContinueExpr &)
{}

void
AttributeChecker::visit (AST::BreakExpr &)
{}

void
AttributeChecker::visit (AST::RangeFromToExpr &)
{}

void
AttributeChecker::visit (AST::RangeFromExpr &)
{}

void
AttributeChecker::visit (AST::RangeToExpr &)
{}

void
AttributeChecker::visit (AST::RangeFullExpr &)
{}

void
AttributeChecker::visit (AST::RangeFromToInclExpr &)
{}

void
AttributeChecker::visit (AST::RangeToInclExpr &)
{}

void
AttributeChecker::visit (AST::ReturnExpr &)
{}

void
AttributeChecker::visit (AST::LoopExpr &)
{}

void
AttributeChecker::visit (AST::WhileLoopExpr &)
{}

void
AttributeChecker::visit (AST::WhileLetLoopExpr &)
{}

void
AttributeChecker::visit (AST::ForLoopExpr &)
{}

void
AttributeChecker::visit (AST::IfExpr &)
{}

void
AttributeChecker::visit (AST::IfExprConseqElse &)
{}

void
AttributeChecker::visit (AST::IfLetExpr &)
{}

void
AttributeChecker::visit (AST::IfLetExprConseqElse &)
{}

void
AttributeChecker::visit (AST::MatchExpr &)
{}

void
AttributeChecker::visit (AST::AwaitExpr &)
{}

void
AttributeChecker::visit (AST::AsyncBlockExpr &)
{}

// rust-item.h
void
AttributeChecker::visit (AST::TypeParam &)
{}

void
AttributeChecker::visit (AST::LifetimeWhereClauseItem &)
{}

void
AttributeChecker::visit (AST::TypeBoundWhereClauseItem &)
{}

void
AttributeChecker::visit (AST::Module &module)
{
  for (auto &attr : module.get_outer_attrs ())
    check_proc_macro_non_function (attr);

  for (auto &item : module.get_items ())
    for (auto &attr : item->get_outer_attrs ())
      check_proc_macro_non_root (attr, item->get_locus ());

  AST::DefaultASTVisitor::visit (module);
}

void
AttributeChecker::visit (AST::ExternCrate &crate)
{
  for (auto &attr : crate.get_outer_attrs ())
    check_proc_macro_non_function (attr);
}

void
AttributeChecker::visit (AST::UseTreeGlob &)
{}

void
AttributeChecker::visit (AST::UseTreeList &)
{}

void
AttributeChecker::visit (AST::UseTreeRebind &)
{}

void
AttributeChecker::visit (AST::UseDeclaration &declaration)
{
  for (auto &attr : declaration.get_outer_attrs ())
    check_proc_macro_non_function (attr);
}

void
AttributeChecker::visit (AST::Function &fun)
{
  if (fun.has_body ())
    fun.get_definition ().value ()->accept_vis (*this);
}

void
AttributeChecker::visit (AST::TypeAlias &alias)
{
  for (auto &attr : alias.get_outer_attrs ())
    check_proc_macro_non_function (attr);
}

void
AttributeChecker::visit (AST::StructStruct &struct_item)
{
  for (auto &attr : struct_item.get_outer_attrs ())
    {
      check_proc_macro_non_function (attr);
    }

  AST::DefaultASTVisitor::visit (struct_item);
}

void
AttributeChecker::visit (AST::TupleStruct &tuplestruct)
{
  for (auto &attr : tuplestruct.get_outer_attrs ())
    check_proc_macro_non_function (attr);
}

void
AttributeChecker::visit (AST::EnumItem &)
{}

void
AttributeChecker::visit (AST::EnumItemTuple &)
{}

void
AttributeChecker::visit (AST::EnumItemStruct &)
{}

void
AttributeChecker::visit (AST::EnumItemDiscriminant &)
{}

void
AttributeChecker::visit (AST::Enum &enumeration)
{
  for (auto &attr : enumeration.get_outer_attrs ())
    check_proc_macro_non_function (attr);
}

void
AttributeChecker::visit (AST::Union &u)
{
  for (auto &attr : u.get_outer_attrs ())
    check_proc_macro_non_function (attr);
}

void
AttributeChecker::visit (AST::ConstantItem &item)
{
  for (auto &attr : item.get_outer_attrs ())
    check_proc_macro_non_function (attr);
}

void
AttributeChecker::visit (AST::StaticItem &item)
{
  for (auto &attr : item.get_outer_attrs ())
    check_proc_macro_non_function (attr);
}

void
AttributeChecker::visit (AST::TraitItemType &)
{}

void
AttributeChecker::visit (AST::Trait &trait)
{
  for (auto &attr : trait.get_outer_attrs ())
    check_proc_macro_non_function (attr);

  AST::DefaultASTVisitor::visit (trait);
}

void
AttributeChecker::visit (AST::InherentImpl &impl)
{
  for (auto &attr : impl.get_outer_attrs ())
    check_proc_macro_non_function (attr);

  AST::DefaultASTVisitor::visit (impl);
}

void
AttributeChecker::visit (AST::TraitImpl &impl)
{
  for (auto &attr : impl.get_outer_attrs ())
    check_proc_macro_non_function (attr);

  AST::DefaultASTVisitor::visit (impl);
}

void
AttributeChecker::visit (AST::ExternalTypeItem &)
{}

void
AttributeChecker::visit (AST::ExternalStaticItem &)
{}

void
AttributeChecker::visit (AST::ExternBlock &block)
{
  for (auto &attr : block.get_outer_attrs ())
    check_proc_macro_non_function (attr);
}

// rust-macro.h
void
AttributeChecker::visit (AST::MacroMatchFragment &)
{}

void
AttributeChecker::visit (AST::MacroMatchRepetition &)
{}

void
AttributeChecker::visit (AST::MacroMatcher &)
{}

void
AttributeChecker::visit (AST::MacroRulesDefinition &)
{}

void
AttributeChecker::visit (AST::MacroInvocation &)
{}

void
AttributeChecker::visit (AST::MetaItemPath &)
{}

void
AttributeChecker::visit (AST::MetaWord &)
{}

void
AttributeChecker::visit (AST::MetaNameValueStr &)
{}

void
AttributeChecker::visit (AST::MetaListPaths &)
{}

void
AttributeChecker::visit (AST::MetaListNameValueStr &)
{}

// rust-pattern.h
void
AttributeChecker::visit (AST::LiteralPattern &)
{}

void
AttributeChecker::visit (AST::IdentifierPattern &)
{}

void
AttributeChecker::visit (AST::WildcardPattern &)
{}

void
AttributeChecker::visit (AST::RestPattern &)
{}

// void AttributeChecker::visit(RangePatternBound& ){}

void
AttributeChecker::visit (AST::RangePatternBoundLiteral &)
{}

void
AttributeChecker::visit (AST::RangePatternBoundPath &)
{}

void
AttributeChecker::visit (AST::RangePatternBoundQualPath &)
{}

void
AttributeChecker::visit (AST::RangePattern &)
{}

void
AttributeChecker::visit (AST::ReferencePattern &)
{}

// void AttributeChecker::visit(StructPatternField& ){}

void
AttributeChecker::visit (AST::StructPatternFieldTuplePat &)
{}

void
AttributeChecker::visit (AST::StructPatternFieldIdentPat &)
{}

void
AttributeChecker::visit (AST::StructPatternFieldIdent &)
{}

void
AttributeChecker::visit (AST::StructPattern &)
{}

// void AttributeChecker::visit(TupleStructItems& ){}

void
AttributeChecker::visit (AST::TupleStructItemsNoRest &)
{}

void
AttributeChecker::visit (AST::TupleStructItemsHasRest &)
{}

void
AttributeChecker::visit (AST::TupleStructPattern &)
{}

// void AttributeChecker::visit(TuplePatternItems& ){}

void
AttributeChecker::visit (AST::TuplePatternItemsNoRest &)
{}

void
AttributeChecker::visit (AST::TuplePatternItemsHasRest &)
{}

void
AttributeChecker::visit (AST::TuplePattern &)
{}

void
AttributeChecker::visit (AST::GroupedPattern &)
{}

void
AttributeChecker::visit (AST::SlicePattern &)
{}

void
AttributeChecker::visit (AST::AltPattern &)
{}

// rust-stmt.h
void
AttributeChecker::visit (AST::EmptyStmt &)
{}

void
AttributeChecker::visit (AST::LetStmt &)
{}

void
AttributeChecker::visit (AST::ExprStmt &)
{}

// rust-type.h
void
AttributeChecker::visit (AST::TraitBound &)
{}

void
AttributeChecker::visit (AST::ImplTraitType &)
{}

void
AttributeChecker::visit (AST::TraitObjectType &)
{}

void
AttributeChecker::visit (AST::ParenthesisedType &)
{}

void
AttributeChecker::visit (AST::ImplTraitTypeOneBound &)
{}

void
AttributeChecker::visit (AST::TraitObjectTypeOneBound &)
{}

void
AttributeChecker::visit (AST::TupleType &)
{}

void
AttributeChecker::visit (AST::NeverType &)
{}

void
AttributeChecker::visit (AST::RawPointerType &)
{}

void
AttributeChecker::visit (AST::ReferenceType &)
{}

void
AttributeChecker::visit (AST::ArrayType &)
{}

void
AttributeChecker::visit (AST::SliceType &)
{}

void
AttributeChecker::visit (AST::InferredType &)
{}

void
AttributeChecker::visit (AST::BareFunctionType &)
{}

void
AttributeChecker::visit (AST::SelfParam &)
{}

void
AttributeChecker::visit (AST::VariadicParam &)
{}

void
AttributeChecker::visit (AST::FunctionParam &)
{}

} // namespace Analysis
} // namespace Rust
