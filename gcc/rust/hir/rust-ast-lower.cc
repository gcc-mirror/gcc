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

#include "rust-ast-lower.h"
#include "rust-ast-lower-item.h"
#include "rust-ast-lower-stmt.h"
#include "rust-ast-lower-expr.h"
#include "rust-ast-lower-block.h"
#include "rust-ast-lower-type.h"
#include "rust-ast-lower-pattern.h"
#include "rust-ast-lower-struct-field-expr.h"
#include "rust-expr.h"
#include "rust-hir-expr.h"

namespace Rust {
namespace HIR {
using HIR::ClosureParam;

Visibility
translate_visibility (const AST::Visibility &vis)
{
  // FIXME: How do we create a private visibility here? Is it always private if
  // the AST vis is an error?
  // FIXME: We need to add a `create_private()` static function to the
  // AST::Visibility class and use it when the vis is empty in the parser...
  if (vis.is_error ())
    return Visibility::create_error ();

  switch (vis.get_vis_type ())
    {
    case AST::Visibility::PUB:
      return Visibility (Visibility::VisType::PUBLIC);
    case AST::Visibility::PRIV:
    case AST::Visibility::PUB_SELF:
      return Visibility (Visibility::VisType::PRIVATE);
    case AST::Visibility::PUB_CRATE:
    case AST::Visibility::PUB_SUPER:
    case AST::Visibility::PUB_IN_PATH:
      return Visibility (Visibility::VisType::RESTRICTED,
			 ASTLoweringSimplePath::translate (vis.get_path ()),
			 vis.get_locus ());
      break;
    }

  return Visibility::create_error ();
}

ASTLowering::ASTLowering (AST::Crate &astCrate) : astCrate (astCrate) {}

ASTLowering::~ASTLowering () {}

std::unique_ptr<HIR::Crate>
ASTLowering::Resolve (AST::Crate &astCrate)
{
  ASTLowering resolver (astCrate);
  return resolver.go ();
}

std::unique_ptr<HIR::Crate>
ASTLowering::go ()
{
  std::vector<std::unique_ptr<HIR::Item>> items;

  for (auto &item : astCrate.items)
    {
      auto translated = ASTLoweringItem::translate (*item);
      if (translated != nullptr)
	items.push_back (std::unique_ptr<HIR::Item> (translated));
    }

  auto &mappings = Analysis::Mappings::get ();
  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, astCrate.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  return std::unique_ptr<HIR::Crate> (
    new HIR::Crate (std::move (items), astCrate.get_inner_attrs (), mapping));
}

// rust-ast-lower-block.h
void
ASTLoweringBlock::visit (AST::BlockExpr &expr)
{
  tl::optional<HIR::LoopLabel> label;
  if (expr.has_label ())
    label = lower_loop_label (expr.get_label ());
  else
    label = tl::nullopt;

  std::vector<std::unique_ptr<HIR::Stmt>> block_stmts;
  bool block_did_terminate = false;

  for (auto &s : expr.get_statements ())
    {
      // FIXME: We basically need to do that check for *every* single node in
      // the AST. this isn't realistic and this should be turned into an
      // optional, debug-visitor instead, which goes through the entire AST and
      // checks if any of the nodes are macro invocations
      if (s->get_stmt_kind () == AST::Stmt::Kind::MacroInvocation)
	rust_fatal_error (
	  s->get_locus (),
	  "macro invocations should not get lowered to HIR - At "
	  "this point in "
	  "the pipeline, they should all have been expanded");

      if (block_did_terminate)
	rust_warning_at (s->get_locus (), 0, "unreachable statement");

      bool terminated = false;
      auto translated_stmt = ASTLoweringStmt::translate (s.get (), &terminated);
      block_did_terminate |= terminated;

      if (translated_stmt)
	block_stmts.push_back (std::unique_ptr<HIR::Stmt> (translated_stmt));
    }

  if (expr.has_tail_expr () && block_did_terminate)
    {
      // warning unreachable tail expressions
      rust_warning_at (expr.get_tail_expr ().get_locus (), 0,
		       "unreachable expression");
    }

  HIR::ExprWithoutBlock *tail_expr = nullptr;
  if (expr.has_tail_expr ())
    {
      bool terminated = false;
      tail_expr = (HIR::ExprWithoutBlock *)
	ASTLoweringExpr::translate (expr.get_tail_expr (), &terminated);
      block_did_terminate |= terminated;
    }

  bool tail_reachable = !block_did_terminate;
  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);
  translated
    = new HIR::BlockExpr (mapping, std::move (block_stmts),
			  std::unique_ptr<HIR::ExprWithoutBlock> (tail_expr),
			  tail_reachable, expr.get_inner_attrs (),
			  expr.get_outer_attrs (), label,
			  expr.get_start_locus (), expr.get_end_locus ());

  terminated = block_did_terminate;
}

void
ASTLoweringIfBlock::visit (AST::IfExpr &expr)
{
  bool ignored_terminated = false;
  HIR::Expr *condition = ASTLoweringExpr::translate (expr.get_condition_expr (),
						     &ignored_terminated);
  HIR::BlockExpr *block
    = ASTLoweringBlock::translate (expr.get_if_block (), &ignored_terminated);

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated = new HIR::IfExpr (mapping, std::unique_ptr<HIR::Expr> (condition),
				std::unique_ptr<HIR::BlockExpr> (block),
				expr.get_locus ());
}

void
ASTLoweringIfBlock::visit (AST::IfExprConseqElse &expr)
{
  HIR::Expr *condition
    = ASTLoweringExpr::translate (expr.get_condition_expr ());

  bool if_block_terminated = false;
  bool else_block_termianted = false;

  HIR::BlockExpr *if_block
    = ASTLoweringBlock::translate (expr.get_if_block (), &if_block_terminated);
  HIR::ExprWithBlock *else_block
    = ASTLoweringExprWithBlock::translate (expr.get_else_block (),
					   &else_block_termianted);

  terminated = if_block_terminated && else_block_termianted;

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated = new HIR::IfExprConseqElse (
    mapping, std::unique_ptr<HIR::Expr> (condition),
    std::unique_ptr<HIR::BlockExpr> (if_block),
    std::unique_ptr<HIR::ExprWithBlock> (else_block), expr.get_locus ());
}

/**
 *  Lowers the common part "if let 'pattern' = 'expr' { 'if_block' }" of
 *  IfLetExpr[ConseqElse]:
 *  - 'expr' is lowered into *BRANCH_VALUE
 *  - 'pattern' + 'if_block' are lowered and resulting ARM pushed in MATCH_ARMS
 *  - 'KASE_ELSE_EXPR' is the lowered HIR to be used in the else part.
 *
 *  Looks like:
 *
 *  match (expr) {
 *    pattern => {if_block}
 *    _ => kase_else_expr
 *  }
 *
 */
void
ASTLoweringIfLetBlock::desugar_iflet (AST::IfLetExpr &expr,
				      HIR::Expr **branch_value,
				      HIR::Expr *kase_else_expr,
				      std::vector<HIR::MatchCase> &match_arms)
{
  HIR::Expr *kase_expr;
  std::vector<std::unique_ptr<HIR::Pattern>> match_arm_patterns;

  *branch_value = ASTLoweringExpr::translate (expr.get_value_expr ());
  kase_expr = ASTLoweringExpr::translate (expr.get_if_block ());

  // (stable) if let only accepts a single pattern, but (unstable) if let chains
  // need more than one pattern.
  // We don't support if let chains, so only support a single pattern.
  rust_assert (expr.get_patterns ().size () == 1);

  for (auto &pattern : expr.get_patterns ())
    {
      HIR::Pattern *ptrn = ASTLoweringPattern::translate (*pattern);
      match_arm_patterns.push_back (std::unique_ptr<HIR::Pattern> (ptrn));
    }

  // The match arm corresponding to the if let pattern when it matches.
  HIR::MatchArm arm (std::move (match_arm_patterns), expr.get_locus (), nullptr,
		     {});

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  HIR::MatchCase kase (std::move (mapping), std::move (arm),
		       std::unique_ptr<HIR::Expr> (kase_expr));
  match_arms.push_back (std::move (kase));

  // The default match arm when the if let pattern does not match
  std::vector<std::unique_ptr<HIR::Pattern>> match_arm_patterns_wildcard;
  Analysis::NodeMapping mapping_default (crate_num, expr.get_node_id (),
					 mappings.get_next_hir_id (crate_num),
					 UNKNOWN_LOCAL_DEFID);

  std::unique_ptr<HIR::WildcardPattern> wc
    = std::unique_ptr<HIR::WildcardPattern> (
      new HIR::WildcardPattern (mapping_default, expr.get_locus ()));

  match_arm_patterns_wildcard.push_back (std::move (wc));

  HIR::MatchArm arm_default (std::move (match_arm_patterns_wildcard),
			     expr.get_locus (), nullptr, {});

  HIR::MatchCase kase_else (std::move (mapping_default),
			    std::move (arm_default),
			    std::unique_ptr<HIR::Expr> (kase_else_expr));
  match_arms.push_back (std::move (kase_else));
}

void
ASTLoweringIfLetBlock::visit (AST::IfLetExpr &expr)
{
  // Desugar:
  //   if let Some(y) = some_value {
  //     bar();
  //   }
  //
  //   into:
  //
  //   match some_value {
  //     Some(y) => {bar();},
  //     _ => ()
  //   }

  HIR::Expr *branch_value;

  std::vector<HIR::MatchCase> match_arms;
  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  HIR::TupleExpr *unit
    = new HIR::TupleExpr (mapping, {}, {}, {}, expr.get_locus ());

  desugar_iflet (expr, &branch_value, unit, match_arms);

  translated
    = new HIR::MatchExpr (mapping, std::unique_ptr<HIR::Expr> (branch_value),
			  std::move (match_arms), {}, {}, expr.get_locus ());
}

void
ASTLoweringIfLetBlock::visit (AST::IfLetExprConseqElse &expr)
{
  // desugar:
  //   if let Some(y) = some_value {
  //     bar();
  //   } else {
  //     baz();
  //   }
  //
  //   into
  //   match some_value {
  //     Some(y) => {bar();},
  //     _ => {baz();}
  //   }
  //

  HIR::Expr *branch_value;
  std::vector<HIR::MatchCase> match_arms;

  HIR::Expr *kase_else_expr
    = ASTLoweringExpr::translate (expr.get_else_block ());

  desugar_iflet (expr, &branch_value, kase_else_expr, match_arms);

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated
    = new HIR::MatchExpr (mapping, std::unique_ptr<HIR::Expr> (branch_value),
			  std::move (match_arms), {}, {}, expr.get_locus ());
}

// rust-ast-lower-struct-field-expr.h

void
ASTLowerStructExprField::visit (AST::StructExprFieldIdentifierValue &field)
{
  HIR::Expr *value = ASTLoweringExpr::translate (field.get_value ());

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, field.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated = new HIR::StructExprFieldIdentifierValue (
    mapping, field.get_field_name (), std::unique_ptr<HIR::Expr> (value),
    field.get_locus ());
}

void
ASTLowerStructExprField::visit (AST::StructExprFieldIndexValue &field)
{
  HIR::Expr *value = ASTLoweringExpr::translate (field.get_value ());

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, field.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated
    = new HIR::StructExprFieldIndexValue (mapping, field.get_index (),
					  std::unique_ptr<HIR::Expr> (value),
					  field.get_locus ());
}

void
ASTLowerStructExprField::visit (AST::StructExprFieldIdentifier &field)
{
  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, field.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
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
    = ASTLoweringBlock::translate (expr.get_loop_block (), &terminated);

  tl::optional<HIR::LoopLabel> loop_label;
  if (expr.has_loop_label ())
    loop_label = lower_loop_label (expr.get_loop_label ());

  HIR::Expr *loop_condition
    = ASTLoweringExpr::translate (expr.get_predicate_expr (), &terminated);

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated
    = new HIR::WhileLoopExpr (mapping,
			      std::unique_ptr<HIR::Expr> (loop_condition),
			      std::unique_ptr<HIR::BlockExpr> (loop_block),
			      expr.get_locus (), std::move (loop_label),
			      expr.get_outer_attrs ());
}

void
ASTLoweringExprWithBlock::visit (AST::ForLoopExpr &expr)
{
  rust_unreachable ();
}

void
ASTLoweringExprWithBlock::visit (AST::MatchExpr &expr)
{
  HIR::Expr *branch_value
    = ASTLoweringExpr::translate (expr.get_scrutinee_expr ());

  std::vector<HIR::MatchCase> match_arms;
  for (auto &match_case : expr.get_match_cases ())
    {
      HIR::Expr *kase_expr
	= ASTLoweringExpr::translate (match_case.get_expr ());

      HIR::Expr *kase_guard_expr = nullptr;
      if (match_case.get_arm ().has_match_arm_guard ())
	{
	  kase_guard_expr = ASTLoweringExpr::translate (
	    match_case.get_arm ().get_guard_expr ());
	}

      std::vector<std::unique_ptr<HIR::Pattern>> match_arm_patterns;
      for (auto &pattern : match_case.get_arm ().get_patterns ())
	{
	  HIR::Pattern *ptrn = ASTLoweringPattern::translate (*pattern);
	  match_arm_patterns.push_back (std::unique_ptr<HIR::Pattern> (ptrn));
	}

      HIR::MatchArm arm (std::move (match_arm_patterns), expr.get_locus (),
			 std::unique_ptr<HIR::Expr> (kase_guard_expr),
			 match_case.get_arm ().get_outer_attrs ());

      auto crate_num = mappings.get_current_crate ();
      Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				     mappings.get_next_hir_id (crate_num),
				     UNKNOWN_LOCAL_DEFID);

      HIR::MatchCase kase (std::move (mapping), std::move (arm),
			   std::unique_ptr<HIR::Expr> (kase_expr));
      match_arms.push_back (std::move (kase));
    }

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated
    = new HIR::MatchExpr (mapping, std::unique_ptr<HIR::Expr> (branch_value),
			  std::move (match_arms), expr.get_inner_attrs (),
			  expr.get_outer_attrs (), expr.get_locus ());
}

// rust-ast-lower-expr.h

void
ASTLowerPathInExpression::visit (AST::PathInExpression &expr)
{
  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  if (expr.is_lang_item ())
    {
      translated = new HIR::PathInExpression (mapping, expr.get_lang_item (),
					      expr.get_locus (), false);
      return;
    }

  std::vector<HIR::PathExprSegment> path_segments;
  auto &segments = expr.get_segments ();
  for (auto &s : segments)
    {
      path_segments.push_back (lower_path_expr_seg ((s)));

      // insert the mappings for the segment
      HIR::PathExprSegment *lowered_seg = &path_segments.back ();
      mappings.insert_hir_path_expr_seg (lowered_seg);
    }

  translated = new HIR::PathInExpression (mapping, std::move (path_segments),
					  expr.get_locus (),
					  expr.opening_scope_resolution ());
}

HIR::QualifiedPathType
ASTLoweringBase::lower_qual_path_type (AST::QualifiedPathType &qualified_type)
{
  HIR::Type *type = ASTLoweringType::translate (qualified_type.get_type ());
  HIR::TypePath *trait
    = qualified_type.has_as_clause ()
	? ASTLowerTypePath::translate (qualified_type.get_as_type_path ())
	: nullptr;

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, qualified_type.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
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
  auto &segments = expr.get_segments ();
  for (auto &s : segments)
    {
      path_segments.push_back (lower_path_expr_seg ((s)));

      // insert the mappings for the segment
      HIR::PathExprSegment *lowered_seg = &path_segments.back ();
      mappings.insert_hir_path_expr_seg (lowered_seg);
    }

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated = new HIR::QualifiedPathInExpression (mapping, qual_path_type,
						   std::move (path_segments),
						   expr.get_locus (),
						   expr.get_outer_attrs ());
}

ClosureParam
ASTLoweringBase::lower_closure_param (AST::ClosureParam &param)
{
  HIR::Pattern *param_pattern
    = ASTLoweringPattern::translate (param.get_pattern ());

  HIR::Type *param_type = param.has_type_given ()
			    ? ASTLoweringType::translate (param.get_type ())
			    : nullptr;

  return HIR::ClosureParam (std::unique_ptr<HIR::Pattern> (param_pattern),
			    param.get_locus (),
			    param.has_type_given ()
			      ? std::unique_ptr<HIR::Type> (param_type)
			      : nullptr,
			    param.get_outer_attrs ());
}

} // namespace HIR
} // namespace Rust
