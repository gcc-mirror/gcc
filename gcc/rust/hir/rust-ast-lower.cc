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

#include "rust-ast-lower.h"
#include "rust-ast-lower-item.h"
#include "rust-ast-lower-implitem.h"
#include "rust-ast-lower-expr.h"
#include "rust-ast-lower-block.h"
#include "rust-ast-lower-type.h"

namespace Rust {
namespace HIR {

ASTLowering::ASTLowering (AST::Crate &astCrate) : astCrate (astCrate) {}

ASTLowering::~ASTLowering () {}

HIR::Crate
ASTLowering::Resolve (AST::Crate &astCrate)
{
  ASTLowering resolver (astCrate);
  return resolver.go ();
}

HIR::Crate
ASTLowering::go ()
{
  std::vector<std::unique_ptr<HIR::Item> > items;

  for (auto it = astCrate.items.begin (); it != astCrate.items.end (); it++)
    {
      auto translated = ASTLoweringItem::translate (it->get ());
      if (translated != nullptr)
	items.push_back (std::unique_ptr<HIR::Item> (translated));
    }

  auto mappings = Analysis::Mappings::get ();
  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, UNKNOWN_NODEID,
				 mappings->get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  return HIR::Crate (std::move (items), astCrate.get_inner_attrs (), mapping);
}

// rust-ast-lower-block.h
void
ASTLoweringBlock::visit (AST::BlockExpr &expr)
{
  std::vector<std::unique_ptr<HIR::Stmt> > block_stmts;
  bool block_did_terminate = false;
  expr.iterate_stmts ([&] (AST::Stmt *s) mutable -> bool {
    if (block_did_terminate)
      rust_warning_at (s->get_locus (), 0, "unreachable statement");

    bool terminated = false;
    auto translated_stmt = ASTLoweringStmt::translate (s, &terminated);
    block_stmts.push_back (std::unique_ptr<HIR::Stmt> (translated_stmt));
    block_did_terminate |= terminated;
    return true;
  });

  if (expr.has_tail_expr () && block_did_terminate)
    {
      // warning unreachable tail expressions
      rust_warning_at (expr.get_tail_expr ()->get_locus (), 0,
		       "unreachable expression");
    }

  HIR::ExprWithoutBlock *tail_expr = nullptr;
  if (expr.has_tail_expr ())
    {
      bool terminated = false;
      tail_expr = (HIR::ExprWithoutBlock *)
	ASTLoweringExpr::translate (expr.get_tail_expr ().get (), &terminated);
      block_did_terminate |= terminated;
    }

  bool tail_reachable = !block_did_terminate;
  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);
  translated
    = new HIR::BlockExpr (mapping, std::move (block_stmts),
			  std::unique_ptr<HIR::ExprWithoutBlock> (tail_expr),
			  tail_reachable, expr.get_inner_attrs (),
			  expr.get_outer_attrs (), expr.get_locus ());

  terminated = block_did_terminate;
}

void
ASTLoweringIfBlock::visit (AST::IfExpr &expr)
{
  bool ignored_terminated = false;
  HIR::Expr *condition
    = ASTLoweringExpr::translate (expr.get_condition_expr ().get (),
				  &ignored_terminated);
  HIR::BlockExpr *block
    = ASTLoweringBlock::translate (expr.get_if_block ().get (),
				   &ignored_terminated);

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated = new HIR::IfExpr (mapping, std::unique_ptr<HIR::Expr> (condition),
				std::unique_ptr<HIR::BlockExpr> (block),
				expr.get_locus ());
}

void
ASTLoweringIfBlock::visit (AST::IfExprConseqElse &expr)
{
  HIR::Expr *condition
    = ASTLoweringExpr::translate (expr.get_condition_expr ().get ());

  bool if_block_terminated = false;
  bool else_block_termianted = false;

  HIR::BlockExpr *if_block
    = ASTLoweringBlock::translate (expr.get_if_block ().get (),
				   &if_block_terminated);
  HIR::BlockExpr *else_block
    = ASTLoweringBlock::translate (expr.get_else_block ().get (),
				   &else_block_termianted);

  terminated = if_block_terminated && else_block_termianted;

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated
    = new HIR::IfExprConseqElse (mapping,
				 std::unique_ptr<HIR::Expr> (condition),
				 std::unique_ptr<HIR::BlockExpr> (if_block),
				 std::unique_ptr<HIR::BlockExpr> (else_block),
				 expr.get_locus ());
}

void
ASTLoweringIfBlock::visit (AST::IfExprConseqIf &expr)
{
  HIR::Expr *condition
    = ASTLoweringExpr::translate (expr.get_condition_expr ().get ());

  bool ignored_terminated = false;
  HIR::BlockExpr *block
    = ASTLoweringBlock::translate (expr.get_if_block ().get (),
				   &ignored_terminated);
  HIR::IfExpr *conseq_if_expr
    = ASTLoweringIfBlock::translate (expr.get_conseq_if_expr ().get (),
				     &ignored_terminated);

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated
    = new HIR::IfExprConseqIf (mapping, std::unique_ptr<HIR::Expr> (condition),
			       std::unique_ptr<HIR::BlockExpr> (block),
			       std::unique_ptr<HIR::IfExpr> (conseq_if_expr),
			       expr.get_locus ());
}

// rust-ast-lower-struct-field-expr.h

void
ASTLowerStructExprField::visit (AST::StructExprFieldIdentifierValue &field)
{
  HIR::Expr *value = ASTLoweringExpr::translate (field.get_value ().get ());

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, field.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated = new HIR::StructExprFieldIdentifierValue (
    mapping, field.get_field_name (), std::unique_ptr<HIR::Expr> (value),
    field.get_locus ());
}

void
ASTLowerStructExprField::visit (AST::StructExprFieldIndexValue &field)
{
  HIR::Expr *value = ASTLoweringExpr::translate (field.get_value ().get ());

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, field.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated
    = new HIR::StructExprFieldIndexValue (mapping, field.get_index (),
					  std::unique_ptr<HIR::Expr> (value),
					  field.get_locus ());
}

void
ASTLowerStructExprField::visit (AST::StructExprFieldIdentifier &field)
{
  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, field.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated
    = new HIR::StructExprFieldIdentifier (mapping, field.get_field_name (),
					  field.get_locus ());
}

// rust-ast-lower-block.h

void
ASTLoweringExprWithBlock::visit (AST::WhileLoopExpr &expr)
{
  HIR::BlockExpr *loop_block
    = ASTLoweringBlock::translate (expr.get_loop_block ().get (), &terminated);

  HIR::LoopLabel loop_label = lower_loop_label (expr.get_loop_label ());
  HIR::Expr *loop_condition
    = ASTLoweringExpr::translate (expr.get_predicate_expr ().get (),
				  &terminated);

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated
    = new HIR::WhileLoopExpr (mapping,
			      std::unique_ptr<HIR::Expr> (loop_condition),
			      std::unique_ptr<HIR::BlockExpr> (loop_block),
			      expr.get_locus (), std::move (loop_label),
			      expr.get_outer_attrs ());
}

// rust-ast-lower-expr.h

void
ASTLowerPathInExpression::visit (AST::PathInExpression &expr)
{
  std::vector<HIR::PathExprSegment> path_segments;
  expr.iterate_path_segments ([&] (AST::PathExprSegment &s) mutable -> bool {
    path_segments.push_back (lower_path_expr_seg (s));

    // insert the mappings for the segment
    HIR::PathExprSegment *lowered_seg = &path_segments.back ();
    mappings->insert_hir_path_expr_seg (
      lowered_seg->get_mappings ().get_crate_num (),
      lowered_seg->get_mappings ().get_hirid (), lowered_seg);
    return true;
  });

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated = new HIR::PathInExpression (mapping, std::move (path_segments),
					  expr.get_locus (),
					  expr.opening_scope_resolution ());
}

HIR::QualifiedPathType
ASTLoweringBase::lower_qual_path_type (AST::QualifiedPathType &qualified_type)
{
  HIR::Type *type
    = ASTLoweringType::translate (qualified_type.get_type ().get ());
  HIR::TypePath *trait
    = qualified_type.has_as_clause ()
	? ASTLowerTypePath::translate (qualified_type.get_as_type_path ())
	: nullptr;

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, qualified_type.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  return HIR::QualifiedPathType (mapping, std::unique_ptr<HIR::Type> (type),
				 std::unique_ptr<HIR::TypePath> (trait),
				 qualified_type.get_locus ());
}

void
ASTLowerQualPathInExpression::visit (AST::QualifiedPathInExpression &expr)
{
  HIR::QualifiedPathType qual_path_type
    = lower_qual_path_type (expr.get_qualified_path_type ());

  std::vector<HIR::PathExprSegment> path_segments;
  expr.iterate_path_segments ([&] (AST::PathExprSegment &s) mutable -> bool {
    path_segments.push_back (lower_path_expr_seg (s));

    // insert the mappings for the segment
    HIR::PathExprSegment *lowered_seg = &path_segments.back ();
    mappings->insert_hir_path_expr_seg (
      lowered_seg->get_mappings ().get_crate_num (),
      lowered_seg->get_mappings ().get_hirid (), lowered_seg);
    return true;
  });

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated = new HIR::QualifiedPathInExpression (mapping, qual_path_type,
						   std::move (path_segments),
						   expr.get_locus (),
						   expr.get_outer_attrs ());
}

// rust-ast-lower-base.h

std::vector<std::unique_ptr<HIR::GenericParam> >
ASTLoweringBase::lower_generic_params (
  std::vector<std::unique_ptr<AST::GenericParam> > &params)
{
  std::vector<std::unique_ptr<HIR::GenericParam> > lowered;
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

  std::vector<std::unique_ptr<HIR::Type> > type_args;
  for (auto &type : args.get_type_args ())
    {
      HIR::Type *t = ASTLoweringType::translate (type.get ());
      type_args.push_back (std::unique_ptr<HIR::Type> (t));
    }

  return HIR::GenericArgs (std::move (lifetime_args), std::move (type_args),
			   std::move (binding_args), args.get_locus ());
}

HIR::SelfParam
ASTLoweringBase::lower_self (AST::SelfParam &self)
{
  HIR::Type *type = self.has_type ()
		      ? ASTLoweringType::translate (self.get_type ().get ())
		      : nullptr;

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, self.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  return HIR::SelfParam (mapping, std::unique_ptr<HIR::Type> (type),
			 self.get_is_mut (), self.get_locus ());
}

void
ASTLowerTypePath::visit (AST::TypePathSegmentGeneric &segment)
{
  std::vector<HIR::GenericArgsBinding> binding_args; // TODO

  std::string segment_name = segment.get_ident_segment ().as_string ();
  bool has_separating_scope_resolution
    = segment.get_separating_scope_resolution ();

  std::vector<HIR::Lifetime> lifetime_args;
  for (auto &lifetime : segment.get_generic_args ().get_lifetime_args ())
    {
      HIR::Lifetime l = lower_lifetime (lifetime);
      lifetime_args.push_back (std::move (l));
    }

  std::vector<std::unique_ptr<HIR::Type> > type_args;
  for (auto &type : segment.get_generic_args ().get_type_args ())
    {
      HIR::Type *t = ASTLoweringType::translate (type.get ());
      type_args.push_back (std::unique_ptr<HIR::Type> (t));
    }

  auto crate_num = mappings->get_current_crate ();
  auto hirid = mappings->get_next_hir_id (crate_num);
  Analysis::NodeMapping mapping (crate_num, segment.get_node_id (), hirid,
				 UNKNOWN_LOCAL_DEFID);

  translated_segment = new HIR::TypePathSegmentGeneric (
    std::move (mapping), segment_name, has_separating_scope_resolution,
    std::move (lifetime_args), std::move (type_args), std::move (binding_args),
    segment.get_locus ());
}

// rust-ast-lower-base

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

} // namespace HIR
} // namespace Rust
