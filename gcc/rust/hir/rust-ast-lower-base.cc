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

#include "rust-ast-lower-base.h"
#include "rust-ast-lower-type.h"
#include "rust-ast-lower-pattern.h"
#include "rust-ast-lower-extern.h"
#include "rust-ast.h"
#include "rust-attribute-values.h"
#include "rust-diagnostics.h"
#include "rust-expr.h"
#include "rust-item.h"
#include "rust-system.h"
#include "rust-attributes.h"

namespace Rust {
namespace HIR {

void
ASTLoweringBase::visit (AST::MacroInvocation &invoc)
{
  rust_fatal_error (invoc.get_locus (), "rogue macro detected during lowering");
  rust_unreachable ();
}

void
ASTLoweringBase::visit (AST::ErrorPropagationExpr &expr)
{
  rust_fatal_error (expr.get_locus (),
		    "missing desugar for question mark operator");
  rust_unreachable ();
}

void
ASTLoweringBase::visit (AST::Token &)
{}
void
ASTLoweringBase::visit (AST::DelimTokenTree &)
{}
void
ASTLoweringBase::visit (AST::AttrInputMetaItemContainer &)
{}
//  void ASTLoweringBase::visit(MetaItemmeta_item) {}
//  void vsit(Stmtstmt) {}
//  void ASTLoweringBase::visit(Exprexpr) {}
void
ASTLoweringBase::visit (AST::IdentifierExpr &)
{}
//  void ASTLoweringBase::visit(Patternpattern) {}
//  void ASTLoweringBase::visit(Typetype) {}
//  void ASTLoweringBase::visit(TypeParamBoundtype_param_bound) {}
void
ASTLoweringBase::visit (AST::Lifetime &)
{}
//  void ASTLoweringBase::visit(GenericParamgeneric_param) {}
void
ASTLoweringBase::visit (AST::LifetimeParam &)
{}
void
ASTLoweringBase::visit (AST::ConstGenericParam &)
{}
//  void ASTLoweringBase::visit(TraitItemtrait_item) {}
//  void ASTLoweringBase::visit(InherentImplIteminherent_impl_item) {}
//  void ASTLoweringBase::visit(TraitImplItemtrait_impl_item) {}

// rust-path.h
void
ASTLoweringBase::visit (AST::PathInExpression &)
{}
void
ASTLoweringBase::visit (AST::TypePathSegment &)
{}
void
ASTLoweringBase::visit (AST::TypePathSegmentGeneric &)
{}
void
ASTLoweringBase::visit (AST::TypePathSegmentFunction &)
{}
void
ASTLoweringBase::visit (AST::TypePath &)
{}
void
ASTLoweringBase::visit (AST::QualifiedPathInExpression &)
{}
void
ASTLoweringBase::visit (AST::QualifiedPathInType &)
{}

// rust-expr.h
void
ASTLoweringBase::visit (AST::LiteralExpr &)
{}
void
ASTLoweringBase::visit (AST::AttrInputLiteral &)
{}
void
ASTLoweringBase::visit (AST::AttrInputMacro &)
{}
void
ASTLoweringBase::visit (AST::MetaItemLitExpr &)
{}
void
ASTLoweringBase::visit (AST::MetaItemPathLit &)
{}
void
ASTLoweringBase::visit (AST::BorrowExpr &)
{}
void
ASTLoweringBase::visit (AST::DereferenceExpr &)
{}
void
ASTLoweringBase::visit (AST::NegationExpr &)
{}
void
ASTLoweringBase::visit (AST::ArithmeticOrLogicalExpr &)
{}
void
ASTLoweringBase::visit (AST::ComparisonExpr &)
{}
void
ASTLoweringBase::visit (AST::LazyBooleanExpr &)
{}
void
ASTLoweringBase::visit (AST::TypeCastExpr &)
{}
void
ASTLoweringBase::visit (AST::AssignmentExpr &)
{}
void
ASTLoweringBase::visit (AST::CompoundAssignmentExpr &)
{}
void
ASTLoweringBase::visit (AST::GroupedExpr &)
{}
//  void ASTLoweringBase::visit(ArrayElemselems) {}
void
ASTLoweringBase::visit (AST::ArrayElemsValues &)
{}
void
ASTLoweringBase::visit (AST::ArrayElemsCopied &)
{}
void
ASTLoweringBase::visit (AST::ArrayExpr &)
{}
void
ASTLoweringBase::visit (AST::ArrayIndexExpr &)
{}
void
ASTLoweringBase::visit (AST::TupleExpr &)
{}
void
ASTLoweringBase::visit (AST::TupleIndexExpr &)
{}
void
ASTLoweringBase::visit (AST::StructExprStruct &)
{}
//  void ASTLoweringBase::visit(StructExprFieldfield) {}
void
ASTLoweringBase::visit (AST::StructExprFieldIdentifier &)
{}
void
ASTLoweringBase::visit (AST::StructExprFieldIdentifierValue &)
{}
void
ASTLoweringBase::visit (AST::StructExprFieldIndexValue &)
{}
void
ASTLoweringBase::visit (AST::StructExprStructFields &)
{}
void
ASTLoweringBase::visit (AST::StructExprStructBase &)
{}
void
ASTLoweringBase::visit (AST::CallExpr &)
{}
void
ASTLoweringBase::visit (AST::MethodCallExpr &)
{}
void
ASTLoweringBase::visit (AST::FieldAccessExpr &)
{}
void
ASTLoweringBase::visit (AST::ClosureExprInner &)
{}
void
ASTLoweringBase::visit (AST::BlockExpr &)
{}
void
ASTLoweringBase::visit (AST::ClosureExprInnerTyped &)
{}
void
ASTLoweringBase::visit (AST::ContinueExpr &)
{}
void
ASTLoweringBase::visit (AST::BreakExpr &)
{}
void
ASTLoweringBase::visit (AST::RangeFromToExpr &)
{}
void
ASTLoweringBase::visit (AST::RangeFromExpr &)
{}
void
ASTLoweringBase::visit (AST::RangeToExpr &)
{}
void
ASTLoweringBase::visit (AST::RangeFullExpr &)
{}
void
ASTLoweringBase::visit (AST::RangeFromToInclExpr &)
{}
void
ASTLoweringBase::visit (AST::RangeToInclExpr &)
{}

void
ASTLoweringBase::visit (AST::BoxExpr &)
{}

void
ASTLoweringBase::visit (AST::ReturnExpr &)
{}
void
ASTLoweringBase::visit (AST::UnsafeBlockExpr &)
{}
void
ASTLoweringBase::visit (AST::LoopExpr &)
{}
void
ASTLoweringBase::visit (AST::WhileLoopExpr &)
{}
void
ASTLoweringBase::visit (AST::WhileLetLoopExpr &)
{}
void
ASTLoweringBase::visit (AST::ForLoopExpr &)
{}
void
ASTLoweringBase::visit (AST::IfExpr &)
{}
void
ASTLoweringBase::visit (AST::IfExprConseqElse &)
{}
void
ASTLoweringBase::visit (AST::IfLetExpr &)
{}
void
ASTLoweringBase::visit (AST::IfLetExprConseqElse &)
{}

void
ASTLoweringBase::visit (AST::InlineAsm &)
{}

//  void ASTLoweringBase::visit(MatchCasematch_case) {}
// void ASTLoweringBase:: (AST::MatchCaseBlockExpr &) {}
// void ASTLoweringBase:: (AST::MatchCaseExpr &) {}
void
ASTLoweringBase::visit (AST::MatchExpr &)
{}
void
ASTLoweringBase::visit (AST::AwaitExpr &)
{}
void
ASTLoweringBase::visit (AST::AsyncBlockExpr &)
{}

// rust-item.h
void
ASTLoweringBase::visit (AST::TypeParam &)
{}
//  void ASTLoweringBase::visit(WhereClauseItemitem) {}
void
ASTLoweringBase::visit (AST::LifetimeWhereClauseItem &)
{}
void
ASTLoweringBase::visit (AST::TypeBoundWhereClauseItem &)
{}
void
ASTLoweringBase::visit (AST::Module &)
{}
void
ASTLoweringBase::visit (AST::ExternCrate &)
{}
//  void ASTLoweringBase::visit(UseTreeuse_tree) {}
void
ASTLoweringBase::visit (AST::UseTreeGlob &)
{}
void
ASTLoweringBase::visit (AST::UseTreeList &)
{}
void
ASTLoweringBase::visit (AST::UseTreeRebind &)
{}
void
ASTLoweringBase::visit (AST::UseDeclaration &)
{}
void
ASTLoweringBase::visit (AST::Function &)
{}
void
ASTLoweringBase::visit (AST::TypeAlias &)
{}
void
ASTLoweringBase::visit (AST::StructStruct &)
{}
void
ASTLoweringBase::visit (AST::TupleStruct &)
{}
void
ASTLoweringBase::visit (AST::EnumItem &)
{}
void
ASTLoweringBase::visit (AST::EnumItemTuple &)
{}
void
ASTLoweringBase::visit (AST::EnumItemStruct &)
{}
void
ASTLoweringBase::visit (AST::EnumItemDiscriminant &)
{}
void
ASTLoweringBase::visit (AST::Enum &)
{}
void
ASTLoweringBase::visit (AST::Union &)
{}
void
ASTLoweringBase::visit (AST::ConstantItem &)
{}
void
ASTLoweringBase::visit (AST::StaticItem &)
{}
void
ASTLoweringBase::visit (AST::TraitItemConst &)
{}
void
ASTLoweringBase::visit (AST::TraitItemType &)
{}
void
ASTLoweringBase::visit (AST::Trait &)
{}
void
ASTLoweringBase::visit (AST::InherentImpl &)
{}
void
ASTLoweringBase::visit (AST::TraitImpl &)
{}
//  void ASTLoweringBase::visit(ExternalItemitem) {}
void
ASTLoweringBase::visit (AST::ExternalTypeItem &)
{}
void
ASTLoweringBase::visit (AST::ExternalStaticItem &)
{}
void
ASTLoweringBase::visit (AST::ExternBlock &)
{}

// rust-macro.h
void
ASTLoweringBase::visit (AST::MacroMatchFragment &)
{}
void
ASTLoweringBase::visit (AST::MacroMatchRepetition &)
{}
void
ASTLoweringBase::visit (AST::MacroMatcher &)
{}
void
ASTLoweringBase::visit (AST::MacroRulesDefinition &)
{}
void
ASTLoweringBase::visit (AST::MetaItemPath &)
{}
void
ASTLoweringBase::visit (AST::MetaItemSeq &)
{}
void
ASTLoweringBase::visit (AST::MetaWord &)
{}
void
ASTLoweringBase::visit (AST::MetaNameValueStr &)
{}
void
ASTLoweringBase::visit (AST::MetaListPaths &)
{}
void
ASTLoweringBase::visit (AST::MetaListNameValueStr &)
{}

// rust-pattern.h
void
ASTLoweringBase::visit (AST::LiteralPattern &)
{}
void
ASTLoweringBase::visit (AST::IdentifierPattern &)
{}
void
ASTLoweringBase::visit (AST::WildcardPattern &)
{}
void
ASTLoweringBase::visit (AST::RestPattern &)
{}
//  void ASTLoweringBase::visit(RangePatternBoundbound) {}
void
ASTLoweringBase::visit (AST::RangePatternBoundLiteral &)
{}
void
ASTLoweringBase::visit (AST::RangePatternBoundPath &)
{}
void
ASTLoweringBase::visit (AST::RangePatternBoundQualPath &)
{}
void
ASTLoweringBase::visit (AST::RangePattern &)
{}
void
ASTLoweringBase::visit (AST::ReferencePattern &)
{}
//  void ASTLoweringBase::visit(StructPatternFieldfield) {}
void
ASTLoweringBase::visit (AST::StructPatternFieldTuplePat &)
{}
void
ASTLoweringBase::visit (AST::StructPatternFieldIdentPat &)
{}
void
ASTLoweringBase::visit (AST::StructPatternFieldIdent &)
{}
void
ASTLoweringBase::visit (AST::StructPattern &)
{}
//  void ASTLoweringBase::visit(TupleStructItemstuple_items) {}
void
ASTLoweringBase::visit (AST::TupleStructItemsNoRange &)
{}
void
ASTLoweringBase::visit (AST::TupleStructItemsRange &)
{}
void
ASTLoweringBase::visit (AST::TupleStructPattern &)
{}
//  void ASTLoweringBase::visit(TuplePatternItemstuple_items) {}
void
ASTLoweringBase::visit (AST::TuplePatternItemsMultiple &)
{}
void
ASTLoweringBase::visit (AST::TuplePatternItemsRanged &)
{}
void
ASTLoweringBase::visit (AST::TuplePattern &)
{}
void
ASTLoweringBase::visit (AST::GroupedPattern &)
{}
void
ASTLoweringBase::visit (AST::SlicePattern &)
{}
void
ASTLoweringBase::visit (AST::AltPattern &)
{}

// rust-stmt.h
void
ASTLoweringBase::visit (AST::EmptyStmt &)
{}
void
ASTLoweringBase::visit (AST::LetStmt &)
{}
void
ASTLoweringBase::visit (AST::ExprStmt &)
{}

// rust-type.h
void
ASTLoweringBase::visit (AST::TraitBound &)
{}
void
ASTLoweringBase::visit (AST::ImplTraitType &)
{}
void
ASTLoweringBase::visit (AST::TraitObjectType &)
{}
void
ASTLoweringBase::visit (AST::ParenthesisedType &)
{}
void
ASTLoweringBase::visit (AST::ImplTraitTypeOneBound &)
{}
void
ASTLoweringBase::visit (AST::TraitObjectTypeOneBound &)
{}
void
ASTLoweringBase::visit (AST::TupleType &)
{}
void
ASTLoweringBase::visit (AST::NeverType &)
{}
void
ASTLoweringBase::visit (AST::RawPointerType &)
{}
void
ASTLoweringBase::visit (AST::ReferenceType &)
{}
void
ASTLoweringBase::visit (AST::ArrayType &)
{}
void
ASTLoweringBase::visit (AST::SliceType &)
{}
void
ASTLoweringBase::visit (AST::InferredType &)
{}
void
ASTLoweringBase::visit (AST::BareFunctionType &)
{}

void
ASTLoweringBase::visit (AST::FunctionParam &param)
{}

void
ASTLoweringBase::visit (AST::VariadicParam &param)
{}

void
ASTLoweringBase::visit (AST::SelfParam &param)
{}

void
ASTLoweringBase::visit (AST::FormatArgs &fmt)
{}

HIR::Lifetime
ASTLoweringBase::lower_lifetime (AST::Lifetime &lifetime,
				 bool default_to_static_lifetime)
{
  auto lifetime_type = lifetime.get_lifetime_type ();
  if (lifetime_type == AST::Lifetime::WILDCARD && default_to_static_lifetime)
    {
      // If compiling in a static context.
      lifetime_type = AST::Lifetime::STATIC;
    }

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, lifetime.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);
  mappings.insert_node_to_hir (mapping.get_nodeid (), mapping.get_hirid ());

  return HIR::Lifetime (mapping, lifetime_type, lifetime.get_lifetime_name (),
			lifetime.get_locus ());
}

HIR::LoopLabel
ASTLoweringBase::lower_loop_label (AST::LoopLabel &loop_label)
{
  HIR::Lifetime life = lower_lifetime (loop_label.get_lifetime ());

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, loop_label.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);
  mappings.insert_node_to_hir (mapping.get_nodeid (), mapping.get_hirid ());

  return HIR::LoopLabel (mapping, std::move (life), loop_label.get_locus ());
}

std::vector<std::unique_ptr<HIR::GenericParam>>
ASTLoweringBase::lower_generic_params (
  std::vector<std::unique_ptr<AST::GenericParam>> &params)
{
  std::vector<std::unique_ptr<HIR::GenericParam>> lowered;
  for (auto &ast_param : params)
    {
      auto hir_param = ASTLowerGenericParam::translate (*ast_param);
      lowered.push_back (std::unique_ptr<HIR::GenericParam> (hir_param));
    }

  return lowered;
}

HIR::PathExprSegment
ASTLoweringBase::lower_path_expr_seg (AST::PathExprSegment &s)
{
  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, s.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  return HIR::PathExprSegment (
    std::move (mapping),
    HIR::PathIdentSegment (s.get_ident_segment ().as_string ()), s.get_locus (),
    s.has_generic_args () ? lower_generic_args (s.get_generic_args ())
			  : HIR::GenericArgs::create_empty ());
}

HIR::GenericArgsBinding
ASTLoweringBase::lower_binding (AST::GenericArgsBinding &binding)
{
  HIR::Type *lowered_type = ASTLoweringType::translate (binding.get_type ());
  return HIR::GenericArgsBinding (binding.get_identifier (),
				  std::unique_ptr<HIR::Type> (lowered_type),
				  binding.get_locus ());
}

HIR::GenericArgs
ASTLoweringBase::lower_generic_args (AST::GenericArgs &args)
{
  std::vector<HIR::GenericArgsBinding> binding_args;
  for (auto &binding : args.get_binding_args ())
    {
      HIR::GenericArgsBinding b = lower_binding (binding);
      binding_args.push_back (std::move (b));
    }

  std::vector<HIR::Lifetime> lifetime_args;
  for (auto &lifetime : args.get_lifetime_args ())
    {
      HIR::Lifetime l = lower_lifetime (lifetime);
      lifetime_args.push_back (std::move (l));
    }

  std::vector<std::unique_ptr<HIR::Type>> type_args;
  std::vector<HIR::ConstGenericArg> const_args;

  for (auto &arg : args.get_generic_args ())
    {
      switch (arg.get_kind ())
	{
	  case AST::GenericArg::Kind::Type: {
	    auto type = ASTLoweringType::translate (arg.get_type ());
	    type_args.emplace_back (std::unique_ptr<HIR::Type> (type));
	    break;
	  }
	  case AST::GenericArg::Kind::Const: {
	    auto expr = ASTLoweringExpr::translate (arg.get_expression ());
	    const_args.emplace_back (
	      HIR::ConstGenericArg (std::unique_ptr<HIR::Expr> (expr),
				    expr->get_locus ()));
	    break;
	  }
	default:
	  rust_unreachable ();
	}
    }

  return HIR::GenericArgs (std::move (lifetime_args), std::move (type_args),
			   std::move (binding_args), std::move (const_args),
			   args.get_locus ());
}

HIR::SelfParam
ASTLoweringBase::lower_self (AST::Param &param)
{
  rust_assert (param.is_self ());

  auto self = static_cast<AST::SelfParam &> (param);
  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, self.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 mappings.get_next_localdef_id (crate_num));
  mappings.insert_location (mapping.get_hirid (), param.get_locus ());

  if (self.has_type ())
    {
      HIR::Type *type = ASTLoweringType::translate (self.get_type ());
      return HIR::SelfParam (mapping, std::unique_ptr<HIR::Type> (type),
			     self.get_is_mut (), self.get_locus ());
    }
  else if (!self.get_has_ref ())
    {
      return HIR::SelfParam (mapping, std::unique_ptr<HIR::Type> (nullptr),
			     self.get_is_mut (), self.get_locus ());
    }

  tl::optional<HIR::Lifetime> lifetime = tl::nullopt;

  if (self.has_lifetime ())
    lifetime = lower_lifetime (self.get_lifetime ());

  return HIR::SelfParam (mapping, lifetime, self.get_is_mut (),
			 self.get_locus ());
}

HIR::Type *
ASTLoweringBase::lower_type_no_bounds (AST::TypeNoBounds &type)
{
  return ASTLoweringType::translate (type);
}

HIR::TypeParamBound *
ASTLoweringBase::lower_bound (AST::TypeParamBound &bound)
{
  return ASTLoweringTypeBounds::translate (bound);
}

/* Checks whether the name of a field already exists.  Returns true
   and produces an error if so.  */
bool
struct_field_name_exists (std::vector<HIR::StructField> &fields,
			  HIR::StructField &new_field)
{
  for (auto &field : fields)
    {
      if (field.get_field_name ().as_string ().compare (
	    new_field.get_field_name ().as_string ())
	  == 0)
	{
	  rich_location r (line_table, new_field.get_locus ());
	  r.add_range (field.get_locus ());
	  rust_error_at (r, ErrorCode::E0124, "field %qs is already declared",
			 field.get_field_name ().as_string ().c_str ());
	  return true;
	}
    }
  return false;
}

HIR::FunctionQualifiers
ASTLoweringBase::lower_qualifiers (const AST::FunctionQualifiers &qualifiers)
{
  Unsafety unsafety
    = qualifiers.is_unsafe () ? Unsafety::Unsafe : Unsafety::Normal;
  bool has_extern = qualifiers.is_extern ();
  ABI abi = has_extern ? ABI::C : ABI::RUST;

  if (qualifiers.has_abi ())
    {
      const std::string &extern_abi = qualifiers.get_extern_abi ();
      abi = get_abi_from_string (extern_abi);
      if (has_extern && abi == ABI::UNKNOWN)
	rust_error_at (qualifiers.get_locus (), ErrorCode::E0703,
		       "invalid ABI: found %qs", extern_abi.c_str ());
    }

  return HIR::FunctionQualifiers (qualifiers.get_async_status (),
				  qualifiers.get_const_status (), unsafety,
				  has_extern, abi);
}

void
ASTLoweringBase::handle_outer_attributes (const ItemWrapper &item)
{
  for (const auto &attr : item.get_outer_attrs ())
    {
      const auto &str_path = attr.get_path ().as_string ();
      if (!Analysis::Attributes::is_known (str_path))
	{
	  rust_error_at (attr.get_locus (), "unknown attribute");
	  continue;
	}

      bool is_lang_item = str_path == Values::Attributes::LANG
			  && attr.has_attr_input ()
			  && attr.get_attr_input ().get_attr_input_type ()
			       == AST::AttrInput::AttrInputType::LITERAL;

      bool is_doc_item = str_path == Values::Attributes::DOC;

      if (is_doc_item)
	handle_doc_item_attribute (item, attr);
      else if (is_lang_item)
	handle_lang_item_attribute (item, attr);
      else if (!attribute_handled_in_another_pass (str_path))
	{
	  rust_error_at (attr.get_locus (), "unhandled attribute: [%s]",
			 attr.get_path ().as_string ().c_str ());
	}
    }
}

void
ASTLoweringBase::handle_doc_item_attribute (const ItemWrapper &,
					    const AST::Attribute &attr)
{
  auto simple_doc_comment = attr.has_attr_input ()
			    && attr.get_attr_input ().get_attr_input_type ()
				 == AST::AttrInput::AttrInputType::LITERAL;
  if (simple_doc_comment)
    return;

  const AST::AttrInput &input = attr.get_attr_input ();
  bool is_token_tree
    = input.get_attr_input_type () == AST::AttrInput::AttrInputType::TOKEN_TREE;
  rust_assert (is_token_tree);
  const auto &option = static_cast<const AST::DelimTokenTree &> (input);
  AST::AttrInputMetaItemContainer *meta_item = option.parse_to_meta_item ();

  // TODO: add actual and complete checks for the doc attributes
  //
  // FIXME: Move this to the AttributeChecker visitor
  rust_assert (meta_item);
}

void
ASTLoweringBase::handle_lang_item_attribute (const ItemWrapper &item,
					     const AST::Attribute &attr)
{
  auto &literal = static_cast<AST::AttrInputLiteral &> (attr.get_attr_input ());
  const auto &lang_item_type_str = literal.get_literal ().as_string ();
  auto lang_item_type = LangItem::Parse (lang_item_type_str);

  if (lang_item_type)
    mappings.insert_lang_item (*lang_item_type,
			       item.get_mappings ().get_defid ());
  else
    rust_error_at (attr.get_locus (), "unknown lang item");
}

bool
ASTLoweringBase::attribute_handled_in_another_pass (
  const std::string &attribute_path) const
{
  const auto &lookup = attr_mappings->lookup_builtin (attribute_path);
  if (lookup.is_error ())
    return false;

  if (lookup.handler == Analysis::CompilerPass::UNKNOWN)
    return false;

  return lookup.handler != Analysis::CompilerPass::HIR_LOWERING;
}

std::unique_ptr<HIR::TuplePatternItems>
ASTLoweringBase::lower_tuple_pattern_multiple (
  AST::TuplePatternItemsMultiple &pattern)
{
  std::vector<std::unique_ptr<HIR::Pattern>> patterns;
  for (auto &p : pattern.get_patterns ())
    {
      HIR::Pattern *translated = ASTLoweringPattern::translate (*p);
      patterns.push_back (std::unique_ptr<HIR::Pattern> (translated));
    }

  return std::unique_ptr<HIR::TuplePatternItems> (
    new HIR::TuplePatternItemsMultiple (std::move (patterns)));
}

std::unique_ptr<TuplePatternItems>
ASTLoweringBase::lower_tuple_pattern_ranged (
  AST::TuplePatternItemsRanged &pattern)
{
  std::vector<std::unique_ptr<HIR::Pattern>> lower_patterns;
  std::vector<std::unique_ptr<HIR::Pattern>> upper_patterns;

  for (auto &p : pattern.get_lower_patterns ())
    {
      HIR::Pattern *translated = ASTLoweringPattern::translate (*p);
      lower_patterns.push_back (std::unique_ptr<HIR::Pattern> (translated));
    }

  for (auto &p : pattern.get_upper_patterns ())
    {
      HIR::Pattern *translated = ASTLoweringPattern::translate (*p);
      upper_patterns.push_back (std::unique_ptr<HIR::Pattern> (translated));
    }

  return std::unique_ptr<HIR::TuplePatternItems> (
    new HIR::TuplePatternItemsRanged (std::move (lower_patterns),
				      std::move (upper_patterns)));
}

std::unique_ptr<HIR::RangePatternBound>
ASTLoweringBase::lower_range_pattern_bound (AST::RangePatternBound &bound)
{
  std::unique_ptr<HIR::RangePatternBound> hir_bound = nullptr;
  switch (bound.get_bound_type ())
    {
      case AST::RangePatternBound::RangePatternBoundType::LITERAL: {
	AST::RangePatternBoundLiteral &ref
	  = static_cast<AST::RangePatternBoundLiteral &> (bound);

	HIR::Literal literal = lower_literal (ref.get_literal ());

	hir_bound = std::unique_ptr<HIR::RangePatternBound> (
	  new HIR::RangePatternBoundLiteral (literal, ref.get_locus (),
					     ref.get_has_minus ()));
      }
      break;
      case AST::RangePatternBound::RangePatternBoundType::PATH: {
	auto &ref = static_cast<AST::RangePatternBoundPath &> (bound);

	HIR::PathInExpression *path
	  = ASTLowerPathInExpression::translate (ref.get_path ());

	hir_bound = std::unique_ptr<HIR::RangePatternBound> (
	  new HIR::RangePatternBoundPath (*path));
      }
      break;
      case AST::RangePatternBound::RangePatternBoundType::QUALPATH: {
	auto &ref = static_cast<AST::RangePatternBoundQualPath &> (bound);

	HIR::QualifiedPathInExpression *qualpath
	  = ASTLowerQualPathInExpression::translate (ref.get_qualified_path ());

	hir_bound = std::unique_ptr<HIR::RangePatternBound> (
	  new HIR::RangePatternBoundQualPath (*qualpath));
      }
      break;
    }

  return hir_bound;
}

HIR::Literal
ASTLoweringBase::lower_literal (const AST::Literal &literal)
{
  HIR::Literal::LitType type = HIR::Literal::LitType::CHAR;
  switch (literal.get_lit_type ())
    {
    case AST::Literal::LitType::CHAR:
      type = HIR::Literal::LitType::CHAR;
      break;
    case AST::Literal::LitType::STRING:
      type = HIR::Literal::LitType::STRING;
      break;
    case AST::Literal::LitType::BYTE:
      type = HIR::Literal::LitType::BYTE;
      break;
    case AST::Literal::LitType::BYTE_STRING:
      type = HIR::Literal::LitType::BYTE_STRING;
      break;
    case AST::Literal::LitType::RAW_STRING:
      type = HIR::Literal::LitType::STRING;
      break;
    case AST::Literal::LitType::INT:
      type = HIR::Literal::LitType::INT;
      break;
    case AST::Literal::LitType::FLOAT:
      type = HIR::Literal::LitType::FLOAT;
      break;
    case AST::Literal::LitType::BOOL:
      type = HIR::Literal::LitType::BOOL;
      break;
    case AST::Literal::LitType::ERROR:
      rust_unreachable ();
      break;
    }

  return HIR::Literal (literal.as_string (), type, literal.get_type_hint ());
}

HIR::ExternBlock *
ASTLoweringBase::lower_extern_block (AST::ExternBlock &extern_block)
{
  HIR::Visibility vis = translate_visibility (extern_block.get_visibility ());

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, extern_block.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 mappings.get_next_localdef_id (crate_num));

  std::vector<std::unique_ptr<HIR::ExternalItem>> extern_items;
  for (auto &item : extern_block.get_extern_items ())
    {
      if (item->is_marked_for_strip ())
	continue;

      HIR::ExternalItem *lowered
	= ASTLoweringExternItem::translate (item.get (), mapping.get_hirid ());
      extern_items.push_back (std::unique_ptr<HIR::ExternalItem> (lowered));
    }

  ABI abi = ABI::C;
  if (extern_block.has_abi ())
    {
      const std::string &extern_abi = extern_block.get_abi ();
      abi = get_abi_from_string (extern_abi);
      if (abi == ABI::UNKNOWN)
	rust_error_at (extern_block.get_locus (), ErrorCode::E0703,
		       "invalid ABI: found %qs", extern_abi.c_str ());
    }

  HIR::ExternBlock *hir_extern_block
    = new HIR::ExternBlock (mapping, abi, std::move (extern_items),
			    std::move (vis), extern_block.get_inner_attrs (),
			    extern_block.get_outer_attrs (),
			    extern_block.get_locus ());

  mappings.insert_hir_extern_block (hir_extern_block);

  return hir_extern_block;
}

void
ASTLoweringBase::lower_macro_definition (AST::MacroRulesDefinition &def)
{
  auto is_export = false;
  for (const auto &attr : def.get_outer_attrs ())
    if (attr.get_path ().as_string () == Values::Attributes::MACRO_EXPORT)
      is_export = true;

  if (is_export)
    {
      mappings.insert_exported_macro (def);
      mappings.insert_ast_item (&def);
      mappings.insert_location (def.get_node_id (), def.get_locus ());
    }
}

} // namespace HIR
} // namespace Rust
