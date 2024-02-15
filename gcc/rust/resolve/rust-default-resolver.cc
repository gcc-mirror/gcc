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

#include "rust-default-resolver.h"
#include "rust-ast-full.h"
#include "rust-ast-visitor.h"
#include "rust-item.h"

namespace Rust {
namespace Resolver2_0 {

void
DefaultResolver::visit (AST::BlockExpr &expr)
{
  // extracting the lambda from the `scoped` call otherwise the code looks like
  // a hot turd thanks to our .clang-format

  auto inner_fn = [this, &expr] () {
    for (auto &stmt : expr.get_statements ())
      stmt->accept_vis (*this);

    if (expr.has_tail_expr ())
      expr.get_tail_expr ()->accept_vis (*this);
  };

  ctx.scoped (Rib::Kind::Normal, expr.get_node_id (), inner_fn);
}

void
DefaultResolver::visit (AST::Module &module)
{
  auto item_fn = [this, &module] () {
    for (auto &item : module.get_items ())
      item->accept_vis (*this);
  };

  ctx.scoped (Rib::Kind::Module, module.get_node_id (), item_fn,
	      module.get_name ());
}

void
DefaultResolver::visit (AST::Function &function)
{
  auto def_fn = [this, &function] () {
    for (auto &p : function.get_function_params ())
      {
	if (p->is_variadic ())
	  {
	    auto param = static_cast<AST::VariadicParam *> (p.get ());
	    param->get_pattern ()->accept_vis (*this);
	  }
	else if (p->is_self ())
	  {
	    auto param = static_cast<AST::SelfParam *> (p.get ());
	    param->get_type ()->accept_vis (*this);
	    param->get_lifetime ().accept_vis (*this);
	  }
	else
	  {
	    auto param = static_cast<AST::FunctionParam *> (p.get ());
	    param->get_pattern ()->accept_vis (*this);
	    param->get_type ()->accept_vis (*this);
	  }
      }

    if (function.has_body ())
      function.get_definition ().value ()->accept_vis (*this);
  };

  ctx.scoped (Rib::Kind::Function, function.get_node_id (), def_fn);
}

void
DefaultResolver::visit (AST::ForLoopExpr &expr)
{
  ctx.scoped (Rib::Kind::Normal, expr.get_node_id (), [this, &expr] () {
    expr.get_pattern ()->accept_vis (*this);
    expr.get_iterator_expr ()->accept_vis (*this);
    expr.get_loop_block ()->accept_vis (*this);
  });
}

void
DefaultResolver::visit (AST::Trait &trait)
{
  auto inner_fn = [this, &trait] () {
    for (auto &item : trait.get_trait_items ())
      item->accept_vis (*this);
  };

  ctx.scoped (Rib::Kind::TraitOrImpl, trait.get_node_id (), inner_fn,
	      trait.get_identifier () /* FIXME: Is that valid?*/);
}

void
DefaultResolver::visit (AST::InherentImpl &impl)
{
  auto inner_fn = [this, &impl] () {
    for (auto &item : impl.get_impl_items ())
      item->accept_vis (*this);
  };

  ctx.scoped (Rib::Kind::TraitOrImpl, impl.get_node_id (), inner_fn);
}

void
DefaultResolver::visit (AST::TraitImpl &impl)
{
  auto inner_fn = [this, &impl] () {
    for (auto &item : impl.get_impl_items ())
      item->accept_vis (*this);
  };

  ctx.scoped (Rib::Kind::TraitOrImpl, impl.get_node_id (), inner_fn);
}

void
DefaultResolver::visit (AST::ExternBlock &block)
{
  auto inner_fn = [this, &block] () {
    for (auto &item : block.get_extern_items ())
      item->accept_vis (*this);
  };

  ctx.scoped (Rib::Kind::Normal /* FIXME: Correct? */, block.get_node_id (),
	      inner_fn);
}

void
DefaultResolver::visit (AST::StructStruct &type)
{
  // do we need to scope anything here? no, right?

  // we also can't visit `StructField`s by default, so there's nothing to do -
  // correct? or should we do something like

  AST::DefaultASTVisitor::visit (type);

  // FIXME: ???
}

void
DefaultResolver::visit (AST::Enum &type)
{
  // FIXME: Do we need to scope anything by default?

  auto variant_fn = [this, &type] () {
    for (auto &variant : type.get_variants ())
      variant->accept_vis (*this);
  };

  ctx.scoped (Rib::Kind::Item /* FIXME: Correct? */, type.get_node_id (),
	      variant_fn, type.get_identifier ());
}

void
DefaultResolver::visit (AST::StructExprFieldIdentifierValue &)
{}

void
DefaultResolver::visit (AST::StructExprFieldIndexValue &)
{}

void
DefaultResolver::visit (AST::ClosureExprInner &)
{}

void
DefaultResolver::visit (AST::ClosureExprInnerTyped &)
{}

void
DefaultResolver::visit (AST::ContinueExpr &expr)
{}

void
DefaultResolver::visit (AST::RangeFromToExpr &expr)
{}

void
DefaultResolver::visit (AST::RangeFromExpr &expr)
{}

void
DefaultResolver::visit (AST::RangeToExpr &expr)
{}

void
DefaultResolver::visit (AST::RangeFromToInclExpr &expr)
{}

void
DefaultResolver::visit (AST::RangeToInclExpr &expr)
{}

void
DefaultResolver::visit (AST::ReturnExpr &expr)
{}

void
DefaultResolver::visit (AST::UnsafeBlockExpr &expr)
{}

void
DefaultResolver::visit (AST::LoopExpr &expr)
{}

void
DefaultResolver::visit (AST::WhileLoopExpr &expr)
{}

void
DefaultResolver::visit (AST::WhileLetLoopExpr &expr)
{}

void
DefaultResolver::visit (AST::IfExpr &expr)
{}

void
DefaultResolver::visit (AST::IfExprConseqElse &)
{}

void
DefaultResolver::visit (AST::IfLetExpr &expr)
{}

void
DefaultResolver::visit (AST::IfLetExprConseqElse &)
{}

void
DefaultResolver::visit (AST::MatchExpr &expr)
{}

void
DefaultResolver::visit (AST::AwaitExpr &expr)
{}

void
DefaultResolver::visit (AST::AsyncBlockExpr &expr)
{}

void
DefaultResolver::visit (AST::DelimTokenTree &)
{}

void
DefaultResolver::visit (AST::AttrInputMetaItemContainer &)
{}

void
DefaultResolver::visit (AST::IdentifierExpr &expr)
{}

void
DefaultResolver::visit (AST::LifetimeParam &)
{}

void
DefaultResolver::visit (AST::ConstGenericParam &)
{}

void
DefaultResolver::visit (AST::PathInExpression &)
{}

void
DefaultResolver::visit (AST::TypePathSegmentGeneric &)
{}

void
DefaultResolver::visit (AST::TypePathSegmentFunction &)
{}

void
DefaultResolver::visit (AST::TypePath &)
{}

void
DefaultResolver::visit (AST::QualifiedPathInExpression &)
{}

void
DefaultResolver::visit (AST::QualifiedPathInType &)
{}

void
DefaultResolver::visit (AST::LiteralExpr &expr)
{}

void
DefaultResolver::visit (AST::AttrInputLiteral &)
{}

void
DefaultResolver::visit (AST::AttrInputMacro &)
{}

void
DefaultResolver::visit (AST::MetaItemLitExpr &expr)
{}

void
DefaultResolver::visit (AST::MetaItemPathLit &)
{}

void
DefaultResolver::visit (AST::StructExprStruct &)
{}

void
DefaultResolver::visit (AST::StructExprStructFields &)
{}

void
DefaultResolver::visit (AST::StructExprStructBase &)
{}

void
DefaultResolver::visit (AST::TypeParam &)
{}

void
DefaultResolver::visit (AST::LifetimeWhereClauseItem &)
{}

void
DefaultResolver::visit (AST::TypeBoundWhereClauseItem &)
{}

void
DefaultResolver::visit (AST::ExternCrate &)
{}

void
DefaultResolver::visit (AST::UseTreeGlob &)
{}

void
DefaultResolver::visit (AST::UseTreeList &)
{}

void
DefaultResolver::visit (AST::UseTreeRebind &)
{}

void
DefaultResolver::visit (AST::UseDeclaration &)
{}

void
DefaultResolver::visit (AST::TypeAlias &)
{}

void
DefaultResolver::visit (AST::EnumItem &)
{}

void
DefaultResolver::visit (AST::EnumItemTuple &)
{}

void
DefaultResolver::visit (AST::EnumItemStruct &)
{}

void
DefaultResolver::visit (AST::EnumItemDiscriminant &)
{}

void
DefaultResolver::visit (AST::ConstantItem &item)
{
  auto expr_vis = [this, &item] () { item.get_expr ()->accept_vis (*this); };

  // FIXME: Why do we need a Rib here?
  ctx.scoped (Rib::Kind::Item, item.get_node_id (), expr_vis);
}

void
DefaultResolver::visit (AST::StaticItem &item)
{
  auto expr_vis = [this, &item] () { item.get_expr ()->accept_vis (*this); };

  // FIXME: Why do we need a Rib here?
  ctx.scoped (Rib::Kind::ConstantItem, item.get_node_id (), expr_vis);
}

void
DefaultResolver::visit (AST::TraitItemConst &)
{}

void
DefaultResolver::visit (AST::TraitItemType &)
{}

void
DefaultResolver::visit (AST::ExternalTypeItem &)
{}

void
DefaultResolver::visit (AST::ExternalStaticItem &)
{}

void
DefaultResolver::visit (AST::ExternalFunctionItem &)
{}

void
DefaultResolver::visit (AST::MacroMatchRepetition &)
{}

void
DefaultResolver::visit (AST::MacroMatcher &)
{}

void
DefaultResolver::visit (AST::MacroRulesDefinition &)
{}

void
DefaultResolver::visit (AST::MacroInvocation &)
{}

void
DefaultResolver::visit (AST::MetaItemPath &)
{}

void
DefaultResolver::visit (AST::MetaItemSeq &)
{}

void
DefaultResolver::visit (AST::MetaListPaths &)
{}

void
DefaultResolver::visit (AST::MetaListNameValueStr &)
{}

void
DefaultResolver::visit (AST::RangePatternBoundPath &)
{}

void
DefaultResolver::visit (AST::RangePatternBoundQualPath &)
{}

void
DefaultResolver::visit (AST::RangePattern &)
{}

void
DefaultResolver::visit (AST::ReferencePattern &)
{}

void
DefaultResolver::visit (AST::StructPatternFieldTuplePat &)
{}

void
DefaultResolver::visit (AST::StructPatternFieldIdentPat &)
{}

void
DefaultResolver::visit (AST::StructPatternFieldIdent &)
{}

void
DefaultResolver::visit (AST::StructPattern &)
{}

void
DefaultResolver::visit (AST::TupleStructItemsNoRange &)
{}

void
DefaultResolver::visit (AST::TupleStructItemsRange &)
{}

void
DefaultResolver::visit (AST::TupleStructPattern &)
{}

void
DefaultResolver::visit (AST::TuplePatternItemsMultiple &)
{}

void
DefaultResolver::visit (AST::TuplePatternItemsRanged &)
{}

void
DefaultResolver::visit (AST::TuplePattern &)
{}

void
DefaultResolver::visit (AST::GroupedPattern &)
{}

void
DefaultResolver::visit (AST::SlicePattern &)
{}

void
DefaultResolver::visit (AST::AltPattern &)
{}

void
DefaultResolver::visit (AST::EmptyStmt &)
{}

void
DefaultResolver::visit (AST::TraitBound &)
{}

void
DefaultResolver::visit (AST::ImplTraitType &)
{}

void
DefaultResolver::visit (AST::TraitObjectType &)
{}

void
DefaultResolver::visit (AST::ParenthesisedType &)
{}

void
DefaultResolver::visit (AST::ImplTraitTypeOneBound &)
{}

void
DefaultResolver::visit (AST::TraitObjectTypeOneBound &)
{}

void
DefaultResolver::visit (AST::TupleType &)
{}

void
DefaultResolver::visit (AST::RawPointerType &)
{}

void
DefaultResolver::visit (AST::ReferenceType &)
{}

void
DefaultResolver::visit (AST::ArrayType &)
{}

void
DefaultResolver::visit (AST::SliceType &)
{}

void
DefaultResolver::visit (AST::BareFunctionType &)
{}

void
DefaultResolver::visit (AST::SelfParam &)
{}

void
DefaultResolver::visit (AST::FunctionParam &)
{}

void
DefaultResolver::visit (AST::VariadicParam &)
{}

} // namespace Resolver2_0
} // namespace Rust
