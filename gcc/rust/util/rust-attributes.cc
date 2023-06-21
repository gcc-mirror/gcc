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

#include "rust-system.h"
#include "rust-attributes.h"
#include "rust-ast.h"
#include "rust-ast-full.h"
#include "rust-diagnostics.h"

namespace Rust {
namespace Analysis {

// https://doc.rust-lang.org/stable/nightly-rustc/src/rustc_feature/builtin_attrs.rs.html#248
static const BuiltinAttrDefinition __definitions[]
  = {{"inline", CODE_GENERATION},
     {"cold", CODE_GENERATION},
     {"cfg", EXPANSION},
     {"cfg_attr", EXPANSION},
     {"deprecated", STATIC_ANALYSIS},
     {"allow", STATIC_ANALYSIS},
     {"doc", HIR_LOWERING},
     {"must_use", STATIC_ANALYSIS},
     {"lang", HIR_LOWERING},
     {"link_section", CODE_GENERATION},
     {"no_mangle", CODE_GENERATION},
     {"repr", CODE_GENERATION},
     {"path", EXPANSION},
     {"macro_use", NAME_RESOLUTION},
     // FIXME: This is not implemented yet, see
     // https://github.com/Rust-GCC/gccrs/issues/1475
     {"target_feature", CODE_GENERATION},
     // From now on, these are reserved by the compiler and gated through
     // #![feature(rustc_attrs)]
     {"rustc_inherit_overflow_checks", CODE_GENERATION}};

BuiltinAttributeMappings *
BuiltinAttributeMappings::get ()
{
  static BuiltinAttributeMappings *instance = nullptr;
  if (instance == nullptr)
    instance = new BuiltinAttributeMappings ();

  return instance;
}

const BuiltinAttrDefinition &
BuiltinAttributeMappings::lookup_builtin (const std::string &attr_name) const
{
  auto it = mappings.find (attr_name);
  if (it == mappings.end ())
    return BuiltinAttrDefinition::error_node ();

  return it->second;
}

BuiltinAttributeMappings::BuiltinAttributeMappings ()
{
  size_t ndefinitions = sizeof (__definitions) / sizeof (BuiltinAttrDefinition);
  for (size_t i = 0; i < ndefinitions; i++)
    {
      const BuiltinAttrDefinition &def = __definitions[i];
      mappings.insert ({def.name, def});
    }
}

AttributeChecker::AttributeChecker () {}

void
AttributeChecker::go (AST::Crate &crate)
{
  check_attributes (crate.get_inner_attrs ());

  for (auto &item : crate.items)
    item->accept_vis (*this);
}

static bool
is_builtin (const AST::Attribute &attribute, BuiltinAttrDefinition &builtin)
{
  auto &segments = attribute.get_path ().get_segments ();

  // Builtin attributes always have a single segment. This avoids us creating
  // strings all over the place and performing a linear search in the builtins
  // map
  if (segments.size () != 1)
    return false;

  builtin = BuiltinAttributeMappings::get ()->lookup_builtin (
    segments.at (0).get_segment_name ());

  return !builtin.is_error ();
}

/**
 * Check that the string given to #[doc(alias = ...)] or #[doc(alias(...))] is
 * valid.
 *
 * This means no whitespace characters other than spaces and no quoting
 * characters.
 */
static void
check_doc_alias (const std::string &alias_input, const Location &locus)
{
  // FIXME: The locus here is for the whole attribute. Can we get the locus
  // of the alias input instead?
  for (auto c : alias_input)
    if ((ISSPACE (c) && c != ' ') || c == '\'' || c == '\"')
      {
	auto to_print = std::string (1, c);
	switch (c)
	  {
	  case '\n':
	    to_print = "\\n";
	    break;
	  case '\t':
	    to_print = "\\t";
	    break;
	  default:
	    break;
	  }
	rust_error_at (locus,
		       "invalid character used in %<#[doc(alias)]%> input: %qs",
		       to_print.c_str ());
      }

  if (alias_input.empty ())
    return;

  if (alias_input.front () == ' ' || alias_input.back () == ' ')
    rust_error_at (locus,
		   "%<#[doc(alias)]%> input cannot start or end with a space");
}

static void
check_doc_attribute (const AST::Attribute &attribute)
{
  if (!attribute.has_attr_input ())
    {
      rust_error_at (
	attribute.get_locus (),
	// FIXME: Improve error message here. Rustc has a very good one
	"%<#[doc]%> cannot be an empty attribute");
      return;
    }

  switch (attribute.get_attr_input ().get_attr_input_type ())
    {
    case AST::AttrInput::LITERAL:
    case AST::AttrInput::META_ITEM:
      break;
      // FIXME: Handle them as well

      case AST::AttrInput::TOKEN_TREE: {
	// FIXME: This doesn't check for #[doc(alias(...))]
	const auto &option = static_cast<const AST::DelimTokenTree &> (
	  attribute.get_attr_input ());
	auto *meta_item = option.parse_to_meta_item ();

	for (auto &item : meta_item->get_items ())
	  {
	    if (item->is_key_value_pair ())
	      {
		auto name_value
		  = static_cast<AST::MetaNameValueStr *> (item.get ())
		      ->get_name_value_pair ();

		// FIXME: Check for other stuff than #[doc(alias = ...)]
		if (name_value.first == "alias")
		  check_doc_alias (name_value.second, attribute.get_locus ());
	      }
	  }
	break;
      }
    }
}

void
AttributeChecker::check_attribute (const AST::Attribute &attribute)
{
  BuiltinAttrDefinition result;

  // This checker does not check non-builtin attributes
  if (!is_builtin (attribute, result))
    return;

  // TODO: Add checks here for each builtin attribute
  // TODO: Have an enum of builtins as well, switching on strings is annoying
  // and costly
  if (result.name == "doc")
    check_doc_attribute (attribute);
}

void
AttributeChecker::check_attributes (const AST::AttrVec &attributes)
{
  for (auto &attr : attributes)
    check_attribute (attr);
}

void
AttributeChecker::visit (AST::Token &)
{}

void
AttributeChecker::visit (AST::DelimTokenTree &)
{}

void
AttributeChecker::visit (AST::AttrInputMetaItemContainer &)
{}

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
AttributeChecker::visit (AST::MetaItemPathLit &)
{}

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
AttributeChecker::visit (AST::BlockExpr &)
{}

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
AttributeChecker::visit (AST::UnsafeBlockExpr &)
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
AttributeChecker::visit (AST::IfExprConseqIf &)
{}

void
AttributeChecker::visit (AST::IfExprConseqIfLet &)
{}

void
AttributeChecker::visit (AST::IfLetExpr &)
{}

void
AttributeChecker::visit (AST::IfLetExprConseqElse &)
{}

void
AttributeChecker::visit (AST::IfLetExprConseqIf &)
{}

void
AttributeChecker::visit (AST::IfLetExprConseqIfLet &)
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
AttributeChecker::visit (AST::Method &)
{}

void
AttributeChecker::visit (AST::Module &)
{}

void
AttributeChecker::visit (AST::ExternCrate &)
{}

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
AttributeChecker::visit (AST::UseDeclaration &)
{}

void
AttributeChecker::visit (AST::Function &)
{}

void
AttributeChecker::visit (AST::TypeAlias &)
{}

void
AttributeChecker::visit (AST::StructStruct &struct_item)
{
  check_attributes (struct_item.get_outer_attrs ());
}

void
AttributeChecker::visit (AST::TupleStruct &)
{}

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
AttributeChecker::visit (AST::Enum &)
{}

void
AttributeChecker::visit (AST::Union &)
{}

void
AttributeChecker::visit (AST::ConstantItem &)
{}

void
AttributeChecker::visit (AST::StaticItem &)
{}

void
AttributeChecker::visit (AST::TraitItemFunc &)
{}

void
AttributeChecker::visit (AST::TraitItemMethod &)
{}

void
AttributeChecker::visit (AST::TraitItemConst &)
{}

void
AttributeChecker::visit (AST::TraitItemType &)
{}

void
AttributeChecker::visit (AST::Trait &)
{}

void
AttributeChecker::visit (AST::InherentImpl &)
{}

void
AttributeChecker::visit (AST::TraitImpl &)
{}

void
AttributeChecker::visit (AST::ExternalStaticItem &)
{}

void
AttributeChecker::visit (AST::ExternalFunctionItem &)
{}

void
AttributeChecker::visit (AST::ExternBlock &)
{}

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
AttributeChecker::visit (AST::MetaItemSeq &)
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
AttributeChecker::visit (AST::TupleStructItemsNoRange &)
{}

void
AttributeChecker::visit (AST::TupleStructItemsRange &)
{}

void
AttributeChecker::visit (AST::TupleStructPattern &)
{}

// void AttributeChecker::visit(TuplePatternItems& ){}

void
AttributeChecker::visit (AST::TuplePatternItemsMultiple &)
{}

void
AttributeChecker::visit (AST::TuplePatternItemsRanged &)
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
AttributeChecker::visit (AST::ExprStmtWithoutBlock &)
{}

void
AttributeChecker::visit (AST::ExprStmtWithBlock &)
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

} // namespace Analysis
} // namespace Rust
