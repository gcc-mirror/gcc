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

#include "rust-ast-lower-base.h"
#include "rust-ast-lower-type.h"
#include "rust-ast-lower-pattern.h"
#include "rust-ast-lower-extern.h"

namespace Rust {
namespace HIR {

void
ASTLoweringBase::visit (AST::Token &tok)
{}
void
ASTLoweringBase::visit (AST::DelimTokenTree &delim_tok_tree)
{}
void
ASTLoweringBase::visit (AST::AttrInputMetaItemContainer &input)
{}
//  void ASTLoweringBase::visit(MetaItem& meta_item) {}
//  void vsit(Stmt& stmt) {}
//  void ASTLoweringBase::visit(Expr& expr) {}
void
ASTLoweringBase::visit (AST::IdentifierExpr &ident_expr)
{}
//  void ASTLoweringBase::visit(Pattern& pattern) {}
//  void ASTLoweringBase::visit(Type& type) {}
//  void ASTLoweringBase::visit(TypeParamBound& type_param_bound) {}
void
ASTLoweringBase::visit (AST::Lifetime &lifetime)
{}
//  void ASTLoweringBase::visit(GenericParam& generic_param) {}
void
ASTLoweringBase::visit (AST::LifetimeParam &lifetime_param)
{}
void
ASTLoweringBase::visit (AST::ConstGenericParam &const_param)
{}
//  void ASTLoweringBase::visit(TraitItem& trait_item) {}
//  void ASTLoweringBase::visit(InherentImplItem& inherent_impl_item) {}
//  void ASTLoweringBase::visit(TraitImplItem& trait_impl_item) {}

// rust-path.h
void
ASTLoweringBase::visit (AST::PathInExpression &path)
{}
void
ASTLoweringBase::visit (AST::TypePathSegment &segment)
{}
void
ASTLoweringBase::visit (AST::TypePathSegmentGeneric &segment)
{}
void
ASTLoweringBase::visit (AST::TypePathSegmentFunction &segment)
{}
void
ASTLoweringBase::visit (AST::TypePath &path)
{}
void
ASTLoweringBase::visit (AST::QualifiedPathInExpression &path)
{}
void
ASTLoweringBase::visit (AST::QualifiedPathInType &path)
{}

// rust-expr.h
void
ASTLoweringBase::visit (AST::LiteralExpr &expr)
{}
void
ASTLoweringBase::visit (AST::AttrInputLiteral &attr_input)
{}
void
ASTLoweringBase::visit (AST::MetaItemLitExpr &meta_item)
{}
void
ASTLoweringBase::visit (AST::MetaItemPathLit &meta_item)
{}
void
ASTLoweringBase::visit (AST::BorrowExpr &expr)
{}
void
ASTLoweringBase::visit (AST::DereferenceExpr &expr)
{}
void
ASTLoweringBase::visit (AST::ErrorPropagationExpr &expr)
{}
void
ASTLoweringBase::visit (AST::NegationExpr &expr)
{}
void
ASTLoweringBase::visit (AST::ArithmeticOrLogicalExpr &expr)
{}
void
ASTLoweringBase::visit (AST::ComparisonExpr &expr)
{}
void
ASTLoweringBase::visit (AST::LazyBooleanExpr &expr)
{}
void
ASTLoweringBase::visit (AST::TypeCastExpr &expr)
{}
void
ASTLoweringBase::visit (AST::AssignmentExpr &expr)
{}
void
ASTLoweringBase::visit (AST::CompoundAssignmentExpr &expr)
{}
void
ASTLoweringBase::visit (AST::GroupedExpr &expr)
{}
//  void ASTLoweringBase::visit(ArrayElems& elems) {}
void
ASTLoweringBase::visit (AST::ArrayElemsValues &elems)
{}
void
ASTLoweringBase::visit (AST::ArrayElemsCopied &elems)
{}
void
ASTLoweringBase::visit (AST::ArrayExpr &expr)
{}
void
ASTLoweringBase::visit (AST::ArrayIndexExpr &expr)
{}
void
ASTLoweringBase::visit (AST::TupleExpr &expr)
{}
void
ASTLoweringBase::visit (AST::TupleIndexExpr &expr)
{}
void
ASTLoweringBase::visit (AST::StructExprStruct &expr)
{}
//  void ASTLoweringBase::visit(StructExprField& field) {}
void
ASTLoweringBase::visit (AST::StructExprFieldIdentifier &field)
{}
void
ASTLoweringBase::visit (AST::StructExprFieldIdentifierValue &field)
{}
void
ASTLoweringBase::visit (AST::StructExprFieldIndexValue &field)
{}
void
ASTLoweringBase::visit (AST::StructExprStructFields &expr)
{}
void
ASTLoweringBase::visit (AST::StructExprStructBase &expr)
{}
void
ASTLoweringBase::visit (AST::CallExpr &expr)
{}
void
ASTLoweringBase::visit (AST::MethodCallExpr &expr)
{}
void
ASTLoweringBase::visit (AST::FieldAccessExpr &expr)
{}
void
ASTLoweringBase::visit (AST::ClosureExprInner &expr)
{}
void
ASTLoweringBase::visit (AST::BlockExpr &expr)
{}
void
ASTLoweringBase::visit (AST::ClosureExprInnerTyped &expr)
{}
void
ASTLoweringBase::visit (AST::ContinueExpr &expr)
{}
void
ASTLoweringBase::visit (AST::BreakExpr &expr)
{}
void
ASTLoweringBase::visit (AST::RangeFromToExpr &expr)
{}
void
ASTLoweringBase::visit (AST::RangeFromExpr &expr)
{}
void
ASTLoweringBase::visit (AST::RangeToExpr &expr)
{}
void
ASTLoweringBase::visit (AST::RangeFullExpr &expr)
{}
void
ASTLoweringBase::visit (AST::RangeFromToInclExpr &expr)
{}
void
ASTLoweringBase::visit (AST::RangeToInclExpr &expr)
{}
void
ASTLoweringBase::visit (AST::ReturnExpr &expr)
{}
void
ASTLoweringBase::visit (AST::UnsafeBlockExpr &expr)
{}
void
ASTLoweringBase::visit (AST::LoopExpr &expr)
{}
void
ASTLoweringBase::visit (AST::WhileLoopExpr &expr)
{}
void
ASTLoweringBase::visit (AST::WhileLetLoopExpr &expr)
{}
void
ASTLoweringBase::visit (AST::ForLoopExpr &expr)
{}
void
ASTLoweringBase::visit (AST::IfExpr &expr)
{}
void
ASTLoweringBase::visit (AST::IfExprConseqElse &expr)
{}
void
ASTLoweringBase::visit (AST::IfExprConseqIf &expr)
{}
void
ASTLoweringBase::visit (AST::IfExprConseqIfLet &expr)
{}
void
ASTLoweringBase::visit (AST::IfLetExpr &expr)
{}
void
ASTLoweringBase::visit (AST::IfLetExprConseqElse &expr)
{}
void
ASTLoweringBase::visit (AST::IfLetExprConseqIf &expr)
{}
void
ASTLoweringBase::visit (AST::IfLetExprConseqIfLet &expr)
{}
//  void ASTLoweringBase::visit(MatchCase& match_case) {}
// void ASTLoweringBase:: (AST::MatchCaseBlockExpr &match_case) {}
// void ASTLoweringBase:: (AST::MatchCaseExpr &match_case) {}
void
ASTLoweringBase::visit (AST::MatchExpr &expr)
{}
void
ASTLoweringBase::visit (AST::AwaitExpr &expr)
{}
void
ASTLoweringBase::visit (AST::AsyncBlockExpr &expr)
{}

// rust-item.h
void
ASTLoweringBase::visit (AST::TypeParam &param)
{}
//  void ASTLoweringBase::visit(WhereClauseItem& item) {}
void
ASTLoweringBase::visit (AST::LifetimeWhereClauseItem &item)
{}
void
ASTLoweringBase::visit (AST::TypeBoundWhereClauseItem &item)
{}
void
ASTLoweringBase::visit (AST::Method &method)
{}
void
ASTLoweringBase::visit (AST::Module &module)
{}
void
ASTLoweringBase::visit (AST::ExternCrate &crate)
{}
//  void ASTLoweringBase::visit(UseTree& use_tree) {}
void
ASTLoweringBase::visit (AST::UseTreeGlob &use_tree)
{}
void
ASTLoweringBase::visit (AST::UseTreeList &use_tree)
{}
void
ASTLoweringBase::visit (AST::UseTreeRebind &use_tree)
{}
void
ASTLoweringBase::visit (AST::UseDeclaration &use_decl)
{}
void
ASTLoweringBase::visit (AST::Function &function)
{}
void
ASTLoweringBase::visit (AST::TypeAlias &type_alias)
{}
void
ASTLoweringBase::visit (AST::StructStruct &struct_item)
{}
void
ASTLoweringBase::visit (AST::TupleStruct &tuple_struct)
{}
void
ASTLoweringBase::visit (AST::EnumItem &item)
{}
void
ASTLoweringBase::visit (AST::EnumItemTuple &item)
{}
void
ASTLoweringBase::visit (AST::EnumItemStruct &item)
{}
void
ASTLoweringBase::visit (AST::EnumItemDiscriminant &item)
{}
void
ASTLoweringBase::visit (AST::Enum &enum_item)
{}
void
ASTLoweringBase::visit (AST::Union &union_item)
{}
void
ASTLoweringBase::visit (AST::ConstantItem &const_item)
{}
void
ASTLoweringBase::visit (AST::StaticItem &static_item)
{}
void
ASTLoweringBase::visit (AST::TraitItemFunc &item)
{}
void
ASTLoweringBase::visit (AST::TraitItemMethod &item)
{}
void
ASTLoweringBase::visit (AST::TraitItemConst &item)
{}
void
ASTLoweringBase::visit (AST::TraitItemType &item)
{}
void
ASTLoweringBase::visit (AST::Trait &trait)
{}
void
ASTLoweringBase::visit (AST::InherentImpl &impl)
{}
void
ASTLoweringBase::visit (AST::TraitImpl &impl)
{}
//  void ASTLoweringBase::visit(ExternalItem& item) {}
void
ASTLoweringBase::visit (AST::ExternalStaticItem &item)
{}
void
ASTLoweringBase::visit (AST::ExternalFunctionItem &item)
{}
void
ASTLoweringBase::visit (AST::ExternBlock &block)
{}

// rust-macro.h
void
ASTLoweringBase::visit (AST::MacroMatchFragment &match)
{}
void
ASTLoweringBase::visit (AST::MacroMatchRepetition &match)
{}
void
ASTLoweringBase::visit (AST::MacroMatcher &matcher)
{}
void
ASTLoweringBase::visit (AST::MacroRulesDefinition &rules_def)
{}
void
ASTLoweringBase::visit (AST::MacroInvocation &macro_invoc)
{}
void
ASTLoweringBase::visit (AST::MetaItemPath &meta_item)
{}
void
ASTLoweringBase::visit (AST::MetaItemSeq &meta_item)
{}
void
ASTLoweringBase::visit (AST::MetaWord &meta_item)
{}
void
ASTLoweringBase::visit (AST::MetaNameValueStr &meta_item)
{}
void
ASTLoweringBase::visit (AST::MetaListPaths &meta_item)
{}
void
ASTLoweringBase::visit (AST::MetaListNameValueStr &meta_item)
{}

// rust-pattern.h
void
ASTLoweringBase::visit (AST::LiteralPattern &pattern)
{}
void
ASTLoweringBase::visit (AST::IdentifierPattern &pattern)
{}
void
ASTLoweringBase::visit (AST::WildcardPattern &pattern)
{}
//  void ASTLoweringBase::visit(RangePatternBound& bound) {}
void
ASTLoweringBase::visit (AST::RangePatternBoundLiteral &bound)
{}
void
ASTLoweringBase::visit (AST::RangePatternBoundPath &bound)
{}
void
ASTLoweringBase::visit (AST::RangePatternBoundQualPath &bound)
{}
void
ASTLoweringBase::visit (AST::RangePattern &pattern)
{}
void
ASTLoweringBase::visit (AST::ReferencePattern &pattern)
{}
//  void ASTLoweringBase::visit(StructPatternField& field) {}
void
ASTLoweringBase::visit (AST::StructPatternFieldTuplePat &field)
{}
void
ASTLoweringBase::visit (AST::StructPatternFieldIdentPat &field)
{}
void
ASTLoweringBase::visit (AST::StructPatternFieldIdent &field)
{}
void
ASTLoweringBase::visit (AST::StructPattern &pattern)
{}
//  void ASTLoweringBase::visit(TupleStructItems& tuple_items) {}
void
ASTLoweringBase::visit (AST::TupleStructItemsNoRange &tuple_items)
{}
void
ASTLoweringBase::visit (AST::TupleStructItemsRange &tuple_items)
{}
void
ASTLoweringBase::visit (AST::TupleStructPattern &pattern)
{}
//  void ASTLoweringBase::visit(TuplePatternItems& tuple_items) {}
void
ASTLoweringBase::visit (AST::TuplePatternItemsMultiple &tuple_items)
{}
void
ASTLoweringBase::visit (AST::TuplePatternItemsRanged &tuple_items)
{}
void
ASTLoweringBase::visit (AST::TuplePattern &pattern)
{}
void
ASTLoweringBase::visit (AST::GroupedPattern &pattern)
{}
void
ASTLoweringBase::visit (AST::SlicePattern &pattern)
{}

// rust-stmt.h
void
ASTLoweringBase::visit (AST::EmptyStmt &stmt)
{}
void
ASTLoweringBase::visit (AST::LetStmt &stmt)
{}
void
ASTLoweringBase::visit (AST::ExprStmtWithoutBlock &stmt)
{}
void
ASTLoweringBase::visit (AST::ExprStmtWithBlock &stmt)
{}

// rust-type.h
void
ASTLoweringBase::visit (AST::TraitBound &bound)
{}
void
ASTLoweringBase::visit (AST::ImplTraitType &type)
{}
void
ASTLoweringBase::visit (AST::TraitObjectType &type)
{}
void
ASTLoweringBase::visit (AST::ParenthesisedType &type)
{}
void
ASTLoweringBase::visit (AST::ImplTraitTypeOneBound &type)
{}
void
ASTLoweringBase::visit (AST::TraitObjectTypeOneBound &type)
{}
void
ASTLoweringBase::visit (AST::TupleType &type)
{}
void
ASTLoweringBase::visit (AST::NeverType &type)
{}
void
ASTLoweringBase::visit (AST::RawPointerType &type)
{}
void
ASTLoweringBase::visit (AST::ReferenceType &type)
{}
void
ASTLoweringBase::visit (AST::ArrayType &type)
{}
void
ASTLoweringBase::visit (AST::SliceType &type)
{}
void
ASTLoweringBase::visit (AST::InferredType &type)
{}
void
ASTLoweringBase::visit (AST::BareFunctionType &type)
{}

HIR::Lifetime
ASTLoweringBase::lower_lifetime (AST::Lifetime &lifetime)
{
  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, lifetime.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);
  mappings->insert_node_to_hir (mapping.get_crate_num (), mapping.get_nodeid (),
				mapping.get_hirid ());

  return HIR::Lifetime (mapping, lifetime.get_lifetime_type (),
			lifetime.get_lifetime_name (), lifetime.get_locus ());
}

HIR::LoopLabel
ASTLoweringBase::lower_loop_label (AST::LoopLabel &loop_label)
{
  HIR::Lifetime life = lower_lifetime (loop_label.get_lifetime ());

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, loop_label.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);
  mappings->insert_node_to_hir (mapping.get_crate_num (), mapping.get_nodeid (),
				mapping.get_hirid ());

  return HIR::LoopLabel (mapping, std::move (life), loop_label.get_locus ());
}

std::vector<std::unique_ptr<HIR::GenericParam>>
ASTLoweringBase::lower_generic_params (
  std::vector<std::unique_ptr<AST::GenericParam>> &params)
{
  std::vector<std::unique_ptr<HIR::GenericParam>> lowered;
  for (auto &ast_param : params)
    {
      auto hir_param = ASTLowerGenericParam::translate (ast_param.get ());
      lowered.push_back (std::unique_ptr<HIR::GenericParam> (hir_param));
    }

  return lowered;
}

HIR::PathExprSegment
ASTLoweringBase::lower_path_expr_seg (AST::PathExprSegment &s)
{
  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, s.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
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
  HIR::Type *lowered_type
    = ASTLoweringType::translate (binding.get_type ().get ());
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
	    auto type = ASTLoweringType::translate (arg.get_type ().get ());
	    type_args.emplace_back (std::unique_ptr<HIR::Type> (type));
	    break;
	  }
	  case AST::GenericArg::Kind::Const: {
	    auto expr
	      = ASTLoweringExpr::translate (arg.get_expression ().get ());
	    const_args.emplace_back (
	      HIR::ConstGenericArg (std::unique_ptr<HIR::Expr> (expr),
				    expr->get_locus ()));
	    break;
	  }
	  // FIXME: Arthur: Other horrible hack waiting for disambiguation
	  case AST::GenericArg::Kind::Either: {
	    arg = arg.disambiguate_to_type ();
	    auto type = ASTLoweringType::translate (arg.get_type ().get ());
	    type_args.emplace_back (std::unique_ptr<HIR::Type> (type));
	    break;
	  }
	default:
	  gcc_unreachable ();
	}
    }

  return HIR::GenericArgs (std::move (lifetime_args), std::move (type_args),
			   std::move (binding_args), std::move (const_args),
			   args.get_locus ());
}

HIR::SelfParam
ASTLoweringBase::lower_self (AST::SelfParam &self)
{
  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, self.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  if (self.has_type ())
    {
      HIR::Type *type = ASTLoweringType::translate (self.get_type ().get ());
      return HIR::SelfParam (mapping, std::unique_ptr<HIR::Type> (type),
			     self.get_is_mut (), self.get_locus ());
    }
  else if (!self.get_has_ref ())
    {
      return HIR::SelfParam (mapping, std::unique_ptr<HIR::Type> (nullptr),
			     self.get_is_mut (), self.get_locus ());
    }

  AST::Lifetime l = self.get_lifetime ();
  return HIR::SelfParam (mapping, lower_lifetime (l), self.get_is_mut (),
			 self.get_locus ());
}

void
ASTLowerTypePath::visit (AST::TypePathSegmentGeneric &segment)
{
  std::vector<HIR::GenericArgsBinding> binding_args; // TODO

  std::string segment_name = segment.get_ident_segment ().as_string ();
  bool has_separating_scope_resolution
    = segment.get_separating_scope_resolution ();

  auto generic_args = lower_generic_args (segment.get_generic_args ());

  auto crate_num = mappings->get_current_crate ();
  auto hirid = mappings->get_next_hir_id (crate_num);
  Analysis::NodeMapping mapping (crate_num, segment.get_node_id (), hirid,
				 UNKNOWN_LOCAL_DEFID);

  translated_segment
    = new HIR::TypePathSegmentGeneric (std::move (mapping), segment_name,
				       has_separating_scope_resolution,
				       generic_args, segment.get_locus ());
}

void
ASTLowerQualifiedPathInType::visit (AST::QualifiedPathInType &path)
{
  auto crate_num = mappings->get_current_crate ();
  auto hirid = mappings->get_next_hir_id (crate_num);
  Analysis::NodeMapping qual_mappings (
    crate_num, path.get_qualified_path_type ().get_node_id (), hirid,
    UNKNOWN_LOCAL_DEFID);

  HIR::Type *qual_type = ASTLoweringType::translate (
    path.get_qualified_path_type ().get_type ().get ());
  HIR::TypePath *qual_trait = ASTLowerTypePath::translate (
    path.get_qualified_path_type ().get_as_type_path ());

  HIR::QualifiedPathType qual_path_type (
    qual_mappings, std::unique_ptr<HIR::Type> (qual_type),
    std::unique_ptr<HIR::TypePath> (qual_trait),
    path.get_qualified_path_type ().get_locus ());

  translated_segment = nullptr;
  path.get_associated_segment ()->accept_vis (*this);
  if (translated_segment == nullptr)
    {
      rust_fatal_error (path.get_associated_segment ()->get_locus (),
			"failed to translate AST TypePathSegment");
      return;
    }
  std::unique_ptr<HIR::TypePathSegment> associated_segment (translated_segment);

  std::vector<std::unique_ptr<HIR::TypePathSegment>> translated_segments;
  for (auto &seg : path.get_segments ())
    {
      translated_segment = nullptr;
      seg->accept_vis (*this);
      if (translated_segment == nullptr)
	{
	  rust_fatal_error (seg->get_locus (),
			    "failed to translte AST TypePathSegment");
	}
      translated_segments.push_back (
	std::unique_ptr<HIR::TypePathSegment> (translated_segment));
    }

  Analysis::NodeMapping mapping (crate_num, path.get_node_id (), hirid,
				 mappings->get_next_localdef_id (crate_num));

  translated = new HIR::QualifiedPathInType (std::move (mapping),
					     std::move (qual_path_type),
					     std::move (associated_segment),
					     std::move (translated_segments),
					     path.get_locus ());
  mappings->insert_hir_type (crate_num, hirid, translated);
}

void
ASTLoweringType::visit (AST::TraitObjectTypeOneBound &type)
{
  std::vector<std::unique_ptr<HIR::TypeParamBound>> bounds;
  HIR::TypeParamBound *translated_bound
    = ASTLoweringTypeBounds::translate (&type.get_trait_bound ());
  bounds.push_back (std::unique_ptr<HIR::TypeParamBound> (translated_bound));

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, type.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  translated = new HIR::TraitObjectType (mapping, std::move (bounds),
					 type.get_locus (), type.is_dyn ());

  mappings->insert_hir_type (mapping.get_crate_num (), mapping.get_hirid (),
			     translated);
}

void
ASTLoweringType::visit (AST::TraitObjectType &type)
{
  std::vector<std::unique_ptr<HIR::TypeParamBound>> bounds;

  for (auto &bound : type.get_type_param_bounds ())
    {
      HIR::TypeParamBound *translated_bound
	= ASTLoweringTypeBounds::translate (bound.get ());
      bounds.push_back (
	std::unique_ptr<HIR::TypeParamBound> (translated_bound));
    }

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, type.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  translated = new HIR::TraitObjectType (mapping, std::move (bounds),
					 type.get_locus (), type.is_dyn ());

  mappings->insert_hir_type (mapping.get_crate_num (), mapping.get_hirid (),
			     translated);
}

HIR::Type *
ASTLoweringBase::lower_type_no_bounds (AST::TypeNoBounds *type)
{
  return ASTLoweringType::translate (type);
}

HIR::TypeParamBound *
ASTLoweringBase::lower_bound (AST::TypeParamBound *bound)
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
      if (field.get_field_name ().compare (new_field.get_field_name ()) == 0)
	{
	  RichLocation r (new_field.get_locus ());
	  r.add_range (field.get_locus ());
	  rust_error_at (r, "duplicate field name %qs",
			 field.get_field_name ().c_str ());
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

  ABI abi = ABI::RUST;
  if (qualifiers.has_abi ())
    {
      const std::string &extern_abi = qualifiers.get_extern_abi ();
      abi = get_abi_from_string (extern_abi);
      if (has_extern && abi == ABI::UNKNOWN)
	rust_error_at (qualifiers.get_locus (), "unknown ABI option");
    }

  return HIR::FunctionQualifiers (qualifiers.get_const_status (), unsafety,
				  has_extern, abi);
}

void
ASTLoweringBase::handle_outer_attributes (const HIR::Item &item)
{
  for (const auto &attr : item.get_outer_attrs ())
    {
      const auto &str_path = attr.get_path ().as_string ();
      if (!is_known_attribute (str_path))
	{
	  rust_error_at (attr.get_locus (), "unknown attribute");
	  continue;
	}

      bool is_lang_item = str_path.compare ("lang") == 0
			  && attr.has_attr_input ()
			  && attr.get_attr_input ().get_attr_input_type ()
			       == AST::AttrInput::AttrInputType::LITERAL;

      bool is_doc_item = str_path.compare ("doc") == 0;

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
ASTLoweringBase::handle_doc_item_attribute (const HIR::Item &item,
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
  rust_assert (meta_item);
}

void
ASTLoweringBase::handle_lang_item_attribute (const HIR::Item &item,
					     const AST::Attribute &attr)
{
  auto &literal = static_cast<AST::AttrInputLiteral &> (attr.get_attr_input ());
  const auto &lang_item_type_str = literal.get_literal ().as_string ();
  auto lang_item_type = Analysis::RustLangItem::Parse (lang_item_type_str);
  if (lang_item_type == Analysis::RustLangItem::ItemType::UNKNOWN)
    {
      rust_error_at (attr.get_locus (), "unknown lang item");
      return;
    }
  mappings->insert_lang_item (lang_item_type,
			      item.get_mappings ().get_defid ());
}

bool
ASTLoweringBase::is_known_attribute (const std::string &attribute_path) const
{
  const auto &lookup = attr_mappings->lookup_builtin (attribute_path);
  return !lookup.is_error ();
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
      HIR::Pattern *translated = ASTLoweringPattern::translate (p.get ());
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
      HIR::Pattern *translated = ASTLoweringPattern::translate (p.get ());
      lower_patterns.push_back (std::unique_ptr<HIR::Pattern> (translated));
    }

  for (auto &p : pattern.get_upper_patterns ())
    {
      HIR::Pattern *translated = ASTLoweringPattern::translate (p.get ());
      upper_patterns.push_back (std::unique_ptr<HIR::Pattern> (translated));
    }

  return std::unique_ptr<HIR::TuplePatternItems> (
    new HIR::TuplePatternItemsRanged (std::move (lower_patterns),
				      std::move (upper_patterns)));
}

std::unique_ptr<HIR::RangePatternBound>
ASTLoweringBase::lower_range_pattern_bound (AST::RangePatternBound *bound)
{
  std::unique_ptr<HIR::RangePatternBound> hir_bound = nullptr;
  switch (bound->get_bound_type ())
    {
      case AST::RangePatternBound::RangePatternBoundType::LITERAL: {
	AST::RangePatternBoundLiteral &ref
	  = *static_cast<AST::RangePatternBoundLiteral *> (bound);

	HIR::Literal literal = lower_literal (ref.get_literal ());

	hir_bound = std::unique_ptr<HIR::RangePatternBound> (
	  new HIR::RangePatternBoundLiteral (literal, ref.get_locus (),
					     ref.get_has_minus ()));
      }
      break;
      case AST::RangePatternBound::RangePatternBoundType::PATH: {
	AST::RangePatternBoundPath &ref
	  = *static_cast<AST::RangePatternBoundPath *> (bound);

	HIR::PathInExpression *path
	  = ASTLowerPathInExpression::translate (&ref.get_path ());

	hir_bound = std::unique_ptr<HIR::RangePatternBound> (
	  new HIR::RangePatternBoundPath (*path));
      }
      break;
      case AST::RangePatternBound::RangePatternBoundType::QUALPATH: {
	AST::RangePatternBoundQualPath &ref
	  = *static_cast<AST::RangePatternBoundQualPath *> (bound);

	HIR::QualifiedPathInExpression *qualpath
	  = ASTLowerQualPathInExpression::translate (
	    &ref.get_qualified_path ());

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
      gcc_unreachable ();
      break;
    }

  return HIR::Literal (literal.as_string (), type, literal.get_type_hint ());
}

HIR::ExternBlock *
ASTLoweringBase::lower_extern_block (AST::ExternBlock &extern_block)
{
  HIR::Visibility vis = translate_visibility (extern_block.get_visibility ());

  std::vector<std::unique_ptr<HIR::ExternalItem>> extern_items;
  for (auto &item : extern_block.get_extern_items ())
    {
      if (item->is_marked_for_strip ())
	continue;

      HIR::ExternalItem *lowered
	= ASTLoweringExternItem::translate (item.get ());
      extern_items.push_back (std::unique_ptr<HIR::ExternalItem> (lowered));
    }

  ABI abi = ABI::RUST;
  if (extern_block.has_abi ())
    {
      const std::string &extern_abi = extern_block.get_abi ();
      abi = get_abi_from_string (extern_abi);
      if (abi == ABI::UNKNOWN)
	rust_error_at (extern_block.get_locus (), "unknown ABI option");
    }

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, extern_block.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  HIR::ExternBlock *hir_extern_block
    = new HIR::ExternBlock (mapping, abi, std::move (extern_items),
			    std::move (vis), extern_block.get_inner_attrs (),
			    extern_block.get_outer_attrs (),
			    extern_block.get_locus ());

  mappings->insert_defid_mapping (mapping.get_defid (), hir_extern_block);
  mappings->insert_hir_item (mapping.get_crate_num (), mapping.get_hirid (),
			     hir_extern_block);
  mappings->insert_location (crate_num, mapping.get_hirid (),
			     extern_block.get_locus ());

  return hir_extern_block;
}

} // namespace HIR
} // namespace Rust
