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
AttributeChecker::visit (AST::Token &tok)
{}

void
AttributeChecker::visit (AST::DelimTokenTree &delim_tok_tree)
{}

void
AttributeChecker::visit (AST::AttrInputMetaItemContainer &input)
{}

void
AttributeChecker::visit (AST::IdentifierExpr &ident_expr)
{}

void
AttributeChecker::visit (AST::Lifetime &lifetime)
{}

void
AttributeChecker::visit (AST::LifetimeParam &lifetime_param)
{}

void
AttributeChecker::visit (AST::ConstGenericParam &const_param)
{}

// rust-path.h
void
AttributeChecker::visit (AST::PathInExpression &path)
{}

void
AttributeChecker::visit (AST::TypePathSegment &segment)
{}

void
AttributeChecker::visit (AST::TypePathSegmentGeneric &segment)
{}

void
AttributeChecker::visit (AST::TypePathSegmentFunction &segment)
{}

void
AttributeChecker::visit (AST::TypePath &path)
{}

void
AttributeChecker::visit (AST::QualifiedPathInExpression &path)
{}

void
AttributeChecker::visit (AST::QualifiedPathInType &path)
{}

// rust-expr.h
void
AttributeChecker::visit (AST::LiteralExpr &expr)
{}

void
AttributeChecker::visit (AST::AttrInputLiteral &attr_input)
{}

void
AttributeChecker::visit (AST::MetaItemLitExpr &meta_item)
{}

void
AttributeChecker::visit (AST::MetaItemPathLit &meta_item)
{}

void
AttributeChecker::visit (AST::BorrowExpr &expr)
{}

void
AttributeChecker::visit (AST::DereferenceExpr &expr)
{}

void
AttributeChecker::visit (AST::ErrorPropagationExpr &expr)
{}

void
AttributeChecker::visit (AST::NegationExpr &expr)
{}

void
AttributeChecker::visit (AST::ArithmeticOrLogicalExpr &expr)
{}

void
AttributeChecker::visit (AST::ComparisonExpr &expr)
{}

void
AttributeChecker::visit (AST::LazyBooleanExpr &expr)
{}

void
AttributeChecker::visit (AST::TypeCastExpr &expr)
{}

void
AttributeChecker::visit (AST::AssignmentExpr &expr)
{}

void
AttributeChecker::visit (AST::CompoundAssignmentExpr &expr)
{}

void
AttributeChecker::visit (AST::GroupedExpr &expr)
{}

void
AttributeChecker::visit (AST::ArrayElemsValues &elems)
{}

void
AttributeChecker::visit (AST::ArrayElemsCopied &elems)
{}

void
AttributeChecker::visit (AST::ArrayExpr &expr)
{}

void
AttributeChecker::visit (AST::ArrayIndexExpr &expr)
{}

void
AttributeChecker::visit (AST::TupleExpr &expr)
{}

void
AttributeChecker::visit (AST::TupleIndexExpr &expr)
{}

void
AttributeChecker::visit (AST::StructExprStruct &expr)
{}

void
AttributeChecker::visit (AST::StructExprFieldIdentifier &field)
{}

void
AttributeChecker::visit (AST::StructExprFieldIdentifierValue &field)
{}

void
AttributeChecker::visit (AST::StructExprFieldIndexValue &field)
{}

void
AttributeChecker::visit (AST::StructExprStructFields &expr)
{}

void
AttributeChecker::visit (AST::StructExprStructBase &expr)
{}

void
AttributeChecker::visit (AST::CallExpr &expr)
{}

void
AttributeChecker::visit (AST::MethodCallExpr &expr)
{}

void
AttributeChecker::visit (AST::FieldAccessExpr &expr)
{}

void
AttributeChecker::visit (AST::ClosureExprInner &expr)
{}

void
AttributeChecker::visit (AST::BlockExpr &expr)
{}

void
AttributeChecker::visit (AST::ClosureExprInnerTyped &expr)
{}

void
AttributeChecker::visit (AST::ContinueExpr &expr)
{}

void
AttributeChecker::visit (AST::BreakExpr &expr)
{}

void
AttributeChecker::visit (AST::RangeFromToExpr &expr)
{}

void
AttributeChecker::visit (AST::RangeFromExpr &expr)
{}

void
AttributeChecker::visit (AST::RangeToExpr &expr)
{}

void
AttributeChecker::visit (AST::RangeFullExpr &expr)
{}

void
AttributeChecker::visit (AST::RangeFromToInclExpr &expr)
{}

void
AttributeChecker::visit (AST::RangeToInclExpr &expr)
{}

void
AttributeChecker::visit (AST::ReturnExpr &expr)
{}

void
AttributeChecker::visit (AST::UnsafeBlockExpr &expr)
{}

void
AttributeChecker::visit (AST::LoopExpr &expr)
{}

void
AttributeChecker::visit (AST::WhileLoopExpr &expr)
{}

void
AttributeChecker::visit (AST::WhileLetLoopExpr &expr)
{}

void
AttributeChecker::visit (AST::ForLoopExpr &expr)
{}

void
AttributeChecker::visit (AST::IfExpr &expr)
{}

void
AttributeChecker::visit (AST::IfExprConseqElse &expr)
{}

void
AttributeChecker::visit (AST::IfExprConseqIf &expr)
{}

void
AttributeChecker::visit (AST::IfExprConseqIfLet &expr)
{}

void
AttributeChecker::visit (AST::IfLetExpr &expr)
{}

void
AttributeChecker::visit (AST::IfLetExprConseqElse &expr)
{}

void
AttributeChecker::visit (AST::IfLetExprConseqIf &expr)
{}

void
AttributeChecker::visit (AST::IfLetExprConseqIfLet &expr)
{}

void
AttributeChecker::visit (AST::MatchExpr &expr)
{}

void
AttributeChecker::visit (AST::AwaitExpr &expr)
{}

void
AttributeChecker::visit (AST::AsyncBlockExpr &expr)
{}

// rust-item.h
void
AttributeChecker::visit (AST::TypeParam &param)
{}

void
AttributeChecker::visit (AST::LifetimeWhereClauseItem &item)
{}

void
AttributeChecker::visit (AST::TypeBoundWhereClauseItem &item)
{}

void
AttributeChecker::visit (AST::Method &method)
{}

void
AttributeChecker::visit (AST::Module &module)
{}

void
AttributeChecker::visit (AST::ExternCrate &crate)
{}

void
AttributeChecker::visit (AST::UseTreeGlob &use_tree)
{}

void
AttributeChecker::visit (AST::UseTreeList &use_tree)
{}

void
AttributeChecker::visit (AST::UseTreeRebind &use_tree)
{}

void
AttributeChecker::visit (AST::UseDeclaration &use_decl)
{}

void
AttributeChecker::visit (AST::Function &function)
{}

void
AttributeChecker::visit (AST::TypeAlias &type_alias)
{}

void
AttributeChecker::visit (AST::StructStruct &struct_item)
{
  check_attributes (struct_item.get_outer_attrs ());
}

void
AttributeChecker::visit (AST::TupleStruct &tuple_struct)
{}

void
AttributeChecker::visit (AST::EnumItem &item)
{}

void
AttributeChecker::visit (AST::EnumItemTuple &item)
{}

void
AttributeChecker::visit (AST::EnumItemStruct &item)
{}

void
AttributeChecker::visit (AST::EnumItemDiscriminant &item)
{}

void
AttributeChecker::visit (AST::Enum &enum_item)
{}

void
AttributeChecker::visit (AST::Union &union_item)
{}

void
AttributeChecker::visit (AST::ConstantItem &const_item)
{}

void
AttributeChecker::visit (AST::StaticItem &static_item)
{}

void
AttributeChecker::visit (AST::TraitItemFunc &item)
{}

void
AttributeChecker::visit (AST::TraitItemMethod &item)
{}

void
AttributeChecker::visit (AST::TraitItemConst &item)
{}

void
AttributeChecker::visit (AST::TraitItemType &item)
{}

void
AttributeChecker::visit (AST::Trait &trait)
{}

void
AttributeChecker::visit (AST::InherentImpl &impl)
{}

void
AttributeChecker::visit (AST::TraitImpl &impl)
{}

void
AttributeChecker::visit (AST::ExternalStaticItem &item)
{}

void
AttributeChecker::visit (AST::ExternalFunctionItem &item)
{}

void
AttributeChecker::visit (AST::ExternBlock &block)
{}

// rust-macro.h
void
AttributeChecker::visit (AST::MacroMatchFragment &match)
{}

void
AttributeChecker::visit (AST::MacroMatchRepetition &match)
{}

void
AttributeChecker::visit (AST::MacroMatcher &matcher)
{}

void
AttributeChecker::visit (AST::MacroRulesDefinition &rules_def)
{}

void
AttributeChecker::visit (AST::MacroInvocation &macro_invoc)
{}

void
AttributeChecker::visit (AST::MetaItemPath &meta_item)
{}

void
AttributeChecker::visit (AST::MetaItemSeq &meta_item)
{}

void
AttributeChecker::visit (AST::MetaWord &meta_item)
{}

void
AttributeChecker::visit (AST::MetaNameValueStr &meta_item)
{}

void
AttributeChecker::visit (AST::MetaListPaths &meta_item)
{}

void
AttributeChecker::visit (AST::MetaListNameValueStr &meta_item)
{}

// rust-pattern.h
void
AttributeChecker::visit (AST::LiteralPattern &pattern)
{}

void
AttributeChecker::visit (AST::IdentifierPattern &pattern)
{}

void
AttributeChecker::visit (AST::WildcardPattern &pattern)
{}

// void AttributeChecker::visit(RangePatternBound& bound){}

void
AttributeChecker::visit (AST::RangePatternBoundLiteral &bound)
{}

void
AttributeChecker::visit (AST::RangePatternBoundPath &bound)
{}

void
AttributeChecker::visit (AST::RangePatternBoundQualPath &bound)
{}

void
AttributeChecker::visit (AST::RangePattern &pattern)
{}

void
AttributeChecker::visit (AST::ReferencePattern &pattern)
{}

// void AttributeChecker::visit(StructPatternField& field){}

void
AttributeChecker::visit (AST::StructPatternFieldTuplePat &field)
{}

void
AttributeChecker::visit (AST::StructPatternFieldIdentPat &field)
{}

void
AttributeChecker::visit (AST::StructPatternFieldIdent &field)
{}

void
AttributeChecker::visit (AST::StructPattern &pattern)
{}

// void AttributeChecker::visit(TupleStructItems& tuple_items){}

void
AttributeChecker::visit (AST::TupleStructItemsNoRange &tuple_items)
{}

void
AttributeChecker::visit (AST::TupleStructItemsRange &tuple_items)
{}

void
AttributeChecker::visit (AST::TupleStructPattern &pattern)
{}

// void AttributeChecker::visit(TuplePatternItems& tuple_items){}

void
AttributeChecker::visit (AST::TuplePatternItemsMultiple &tuple_items)
{}

void
AttributeChecker::visit (AST::TuplePatternItemsRanged &tuple_items)
{}

void
AttributeChecker::visit (AST::TuplePattern &pattern)
{}

void
AttributeChecker::visit (AST::GroupedPattern &pattern)
{}

void
AttributeChecker::visit (AST::SlicePattern &pattern)
{}

// rust-stmt.h
void
AttributeChecker::visit (AST::EmptyStmt &stmt)
{}

void
AttributeChecker::visit (AST::LetStmt &stmt)
{}

void
AttributeChecker::visit (AST::ExprStmtWithoutBlock &stmt)
{}

void
AttributeChecker::visit (AST::ExprStmtWithBlock &stmt)
{}

// rust-type.h
void
AttributeChecker::visit (AST::TraitBound &bound)
{}

void
AttributeChecker::visit (AST::ImplTraitType &type)
{}

void
AttributeChecker::visit (AST::TraitObjectType &type)
{}

void
AttributeChecker::visit (AST::ParenthesisedType &type)
{}

void
AttributeChecker::visit (AST::ImplTraitTypeOneBound &type)
{}

void
AttributeChecker::visit (AST::TraitObjectTypeOneBound &type)
{}

void
AttributeChecker::visit (AST::TupleType &type)
{}

void
AttributeChecker::visit (AST::NeverType &type)
{}

void
AttributeChecker::visit (AST::RawPointerType &type)
{}

void
AttributeChecker::visit (AST::ReferenceType &type)
{}

void
AttributeChecker::visit (AST::ArrayType &type)
{}

void
AttributeChecker::visit (AST::SliceType &type)
{}

void
AttributeChecker::visit (AST::InferredType &type)
{}

void
AttributeChecker::visit (AST::BareFunctionType &type)
{}

} // namespace Analysis
} // namespace Rust
