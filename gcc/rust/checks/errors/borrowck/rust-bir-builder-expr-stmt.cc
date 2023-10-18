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
// along with GCC; see the file COPYING3.  If
// not see
// <http://www.gnu.org/licenses/>.

#include "rust-bir-builder-expr-stmt.h"
#include "rust-bir-builder-lazyboolexpr.h"
#include "rust-bir-builder-pattern.h"
#include "rust-bir-builder-struct.h"

namespace Rust {
namespace BIR {

void
ExprStmtBuilder::visit (HIR::ClosureExpr &expr)
{
  auto closure_ty = lookup_type (expr)->as<TyTy::ClosureType> ();
  std::vector<PlaceId> captures;
  for (auto &capture : closure_ty->get_captures ())
    {
      captures.push_back (ctx.place_db.lookup_variable (capture));
    }

  // Not a coercion site.
  return_expr (new InitializerExpr (std::move (captures)), lookup_type (expr));
}

void
ExprStmtBuilder::visit (HIR::StructExprStructFields &fields)
{
  auto struct_ty
    = lookup_type (fields)->as<TyTy::ADTType> ()->get_variants ().at (0);
  auto init_values = StructBuilder (ctx, struct_ty).build (fields);
  return_expr (new InitializerExpr (std::move (init_values)),
	       lookup_type (fields));
}

void
ExprStmtBuilder::visit (HIR::StructExprStruct &expr)
{
  // There is no way to modify empty struct, which makes them constant.
  return_place (ctx.place_db.get_constant (lookup_type (expr)));
}

void
ExprStmtBuilder::visit (HIR::LiteralExpr &expr)
{
  // Different literal values of the same type are not distinguished.
  return_place (ctx.place_db.get_constant (lookup_type (expr)));
}

void
ExprStmtBuilder::visit (HIR::BorrowExpr &expr)
{
  auto operand = visit_expr (*expr.get_expr ());
  return_expr (new BorrowExpr (operand), lookup_type (expr));
}

void
ExprStmtBuilder::visit (HIR::DereferenceExpr &expr)
{
  auto operand = visit_expr (*expr.get_expr ());
  return_place (operand);
}

void
ExprStmtBuilder::visit (HIR::ErrorPropagationExpr &expr)
{
  rust_sorry_at (expr.get_locus (), "error propagation is not supported");
}

void
ExprStmtBuilder::visit (HIR::NegationExpr &expr)
{
  PlaceId operand = visit_expr (*expr.get_expr ());
  return_expr (new Operator<1> ({operand}), lookup_type (expr));
}

void
ExprStmtBuilder::visit (HIR::ArithmeticOrLogicalExpr &expr)
{
  PlaceId lhs = visit_expr (*expr.get_lhs ());
  PlaceId rhs = visit_expr (*expr.get_rhs ());
  return_expr (new Operator<2> ({lhs, rhs}), lookup_type (expr));
}

void
ExprStmtBuilder::visit (HIR::ComparisonExpr &expr)
{
  PlaceId lhs = visit_expr (*expr.get_lhs ());
  PlaceId rhs = visit_expr (*expr.get_rhs ());
  return_expr (new Operator<2> ({lhs, rhs}), lookup_type (expr));
}

void
ExprStmtBuilder::visit (HIR::LazyBooleanExpr &expr)
{
  return_place (LazyBooleanExprBuilder (ctx).build (expr));
}

void
ExprStmtBuilder::visit (HIR::TypeCastExpr &expr)
{
  return_place (visit_expr (*expr.get_expr ()));
}

void
ExprStmtBuilder::visit (HIR::AssignmentExpr &expr)
{
  auto lhs = visit_expr (*expr.get_lhs ());
  auto rhs = visit_expr (*expr.get_rhs ());
  push_assignment (lhs, rhs);
}

void
ExprStmtBuilder::visit (HIR::CompoundAssignmentExpr &expr)
{
  auto lhs = visit_expr (*expr.get_lhs ());
  auto rhs = visit_expr (*expr.get_rhs ());
  push_assignment (lhs, new Operator<2> ({lhs, rhs}));
  // TODO: (philip) nicer unit?
  return_place (ctx.place_db.get_constant (lookup_type (expr)));
}

void
ExprStmtBuilder::visit (HIR::GroupedExpr &expr)
{
  // Uses accept_vis directly to avoid creating n new temporary.
  expr.get_expr_in_parens ()->accept_vis (*this);
}

void
ExprStmtBuilder::visit (HIR::ArrayExpr &expr)
{
  switch (expr.get_internal_elements ()->get_array_expr_type ())
    {
      case HIR::ArrayElems::VALUES: {
	auto init_values = visit_list ((static_cast<HIR::ArrayElemsValues *> (
					  expr.get_internal_elements ().get ()))
					 ->get_values ());
	return_expr (new InitializerExpr (std::move (init_values)),
		     lookup_type (expr));
	break;
      }
      case HIR::ArrayElems::COPIED: {
	auto init = visit_expr (*(static_cast<HIR::ArrayElemsCopied *> (
				    expr.get_internal_elements ().get ()))
				   ->get_elem_to_copy ());
	return_expr (new InitializerExpr ({init}), lookup_type (expr));
	break;
      }
    }
}

void
ExprStmtBuilder::visit (HIR::ArrayIndexExpr &expr)
{
  auto lhs = visit_expr (*expr.get_array_expr ());
  auto rhs = visit_expr (*expr.get_index_expr ());
  // The Index is not tracked in BIR.
  (void) rhs;
  return_place (
    ctx.place_db.lookup_or_add_path (Place::INDEX, lookup_type (expr), lhs));
}

void
ExprStmtBuilder::visit (HIR::TupleExpr &expr)
{
  std::vector<PlaceId> init_values = visit_list (expr.get_tuple_elems ());
  if (std::any_of (init_values.begin (), init_values.end (),
		   [this] (PlaceId id) {
		     return ctx.place_db[id].lifetime.has_lifetime ();
		   }))
    {
      ctx.place_db[expr_return_place].lifetime
	= {ctx.lifetime_interner.get_anonymous ()};
    }
  return_expr (new InitializerExpr (std::move (init_values)),
	       lookup_type (expr));
}

void
ExprStmtBuilder::visit (HIR::TupleIndexExpr &expr)
{
  auto tuple = visit_expr (*expr.get_tuple_expr ());
  return_place (ctx.place_db.lookup_or_add_path (Place::FIELD,
						 lookup_type (expr), tuple,
						 expr.get_tuple_index ()));
}

void
ExprStmtBuilder::visit (HIR::CallExpr &expr)
{
  PlaceId fn = visit_expr (*expr.get_fnexpr ());
  std::vector<PlaceId> arguments = visit_list (expr.get_arguments ());

  TyTy::BaseType *call_type = ctx.place_db[fn].tyty;
  if (auto fn_type = call_type->try_as<TyTy::FnType> ())
    {
      for (size_t i = 0; i < fn_type->get_params ().size (); ++i)
	{
	  coercion_site (arguments[i], fn_type->get_params ()[i].second);
	}
    }
  else if (auto fn_ptr_type = call_type->try_as<TyTy::FnPtr> ())
    {
      for (size_t i = 0; i < fn_ptr_type->get_params ().size (); ++i)
	{
	  coercion_site (arguments[i],
			 fn_ptr_type->get_params ()[i].get_tyty ());
	}
    }
  else
    {
      rust_unreachable ();
    }

  return_expr (new CallExpr (fn, std::move (arguments)), lookup_type (expr),
	       true);
}

void
ExprStmtBuilder::visit (HIR::FieldAccessExpr &expr)
{
  auto receiver = visit_expr (*expr.get_receiver_expr ());
  auto type = autoderef (receiver);
  rust_assert (type->get_kind () == TyTy::ADT);
  auto adt = type->as<TyTy::ADTType> ();
  rust_assert (!adt->is_enum ());
  rust_assert (adt->number_of_variants () == 1);
  TyTy::VariantDef *variant = adt->get_variants ().at (0);

  TyTy::StructFieldType *field_ty = nullptr;
  size_t field_index = 0;
  bool ok = variant->lookup_field (expr.get_field_name ().as_string (),
				   &field_ty, &field_index);
  rust_assert (ok);

  return_place (ctx.place_db.lookup_or_add_path (Place::FIELD,
						 field_ty->get_field_type (),
						 receiver, field_index));
}

void
ExprStmtBuilder::visit (HIR::BlockExpr &block)
{
  for (auto &stmt : block.get_statements ())
    {
      stmt->accept_vis (*this);
    }
  if (block.has_expr ())
    {
      return_place (visit_expr (*block.get_final_expr ()));
    }
}

void
ExprStmtBuilder::visit (HIR::ContinueExpr &cont)
{
  //  BuilderContext::LabelledBlockCtx loop_ctx;
  //  NodeId label = UNKNOWN_NODEID;
  //  if (cont.has_label ())
  //    {
  //      if (!resolve_label (cont.get_label (), label))
  //	return;
  //    }
  //
  //  if (!find_block_ctx (label, loop_ctx))
  //    {
  //      rust_error_at (cont.get_locus (), "unresolved loop label");
  //    }
  //
  //  add_jump_to (loop_ctx.continue_bb);
}

void
ExprStmtBuilder::visit (HIR::BreakExpr &brk)
{
  //  BuilderContext::LabelledBlockCtx block_ctx{};
  //  NodeId label = UNKNOWN_NODEID;
  //  if (brk.has_label ())
  //    {
  //      if (!resolve_label (brk.get_label (), label))
  //	return;
  //    }
  //  if (!find_block_ctx (label, block_ctx))
  //    {
  //      rust_error_at (brk.get_locus (), "unresolved labelled block");
  //    }
  //
  //  if (brk.has_break_expr ())
  //    {
  //      brk.get_expr ()->accept_vis (*this);
  //      push_assignment (block_ctx.label_var, new Operator<1> ({translated}));
  //    }
  //
  //  add_jump_to (block_ctx.break_bb);
}

void
ExprStmtBuilder::visit (HIR::RangeFromToExpr &range)
{
  auto from = visit_expr (*range.get_from_expr ());
  auto to = visit_expr (*range.get_to_expr ());
  return_expr (new InitializerExpr ({from, to}), lookup_type (range));
}

void
ExprStmtBuilder::visit (HIR::RangeFromExpr &expr)
{
  auto from = visit_expr (*expr.get_from_expr ());
  return_expr (new InitializerExpr ({from}), lookup_type (expr));
}

void
ExprStmtBuilder::visit (HIR::RangeToExpr &expr)
{
  auto to = visit_expr (*expr.get_to_expr ());
  return_expr (new InitializerExpr ({to}), lookup_type (expr));
}

void
ExprStmtBuilder::visit (HIR::RangeFullExpr &expr)
{
  return_expr (new InitializerExpr ({}), lookup_type (expr));
}

void
ExprStmtBuilder::visit (HIR::RangeFromToInclExpr &expr)
{
  auto from = visit_expr (*expr.get_from_expr ());
  auto to = visit_expr (*expr.get_to_expr ());
  return_expr (new InitializerExpr ({from, to}), lookup_type (expr));
}

void
ExprStmtBuilder::visit (HIR::RangeToInclExpr &expr)
{
  auto to = visit_expr (*expr.get_to_expr ());
  return_expr (new InitializerExpr ({to}), lookup_type (expr));
}

void
ExprStmtBuilder::visit (HIR::ReturnExpr &ret)
{
  if (ret.has_return_expr ())
    {
      push_assignment (RETURN_VALUE_PLACE, visit_expr (*ret.get_expr ()));
    }
  ctx.get_current_bb ().statements.emplace_back (Node::Kind::RETURN);
}

void
ExprStmtBuilder::visit (HIR::UnsafeBlockExpr &expr)
{
  rust_sorry_at (expr.get_locus (), "unsafe blocks are not supported");
}

void
ExprStmtBuilder::visit (HIR::LoopExpr &expr)
{
  //  PlaceId label_var = ctx.place_db.add_temporary (nullptr);
  //  NodeId label;
  //  if (!resolve_label (expr.get_loop_label (), label))
  //    return;
  //  ctx.label_place_map.emplace (label, label_var);
  //
  //  expr.get_loop_block ()->accept_vis (*this);
  //
  //  translated = label_var;
}
void
ExprStmtBuilder::visit (HIR::WhileLoopExpr &expr)
{
  //  // TODO: Desugar in AST->HIR ???
  //  PlaceId label_var = ctx.place_db.add_temporary (nullptr);
  //  NodeId label;
  //  if (!resolve_label (expr.get_loop_label (), label))
  //    return;
  //  ctx.label_place_map.emplace (label, label_var);
  //
  //  expr.get_predicate_expr ()->accept_vis (*this);
  //
  //  expr.get_loop_block ()->accept_vis (*this);
  //
  //  translated = label_var;
}
void
ExprStmtBuilder::visit (HIR::WhileLetLoopExpr &expr)
{
  // TODO: Desugar in AST->HIR
}
void
ExprStmtBuilder::visit (HIR::IfExpr &expr)
{
  // If without else cannot return a non-unit value (see [E0317]).

  push_switch (visit_expr (*expr.get_if_condition ()));
  BasicBlockId if_block = ctx.current_bb;

  ctx.current_bb = new_bb ();
  (void) visit_expr (*expr.get_if_block ());
  BasicBlockId then_block = ctx.current_bb;

  ctx.current_bb = new_bb ();
  BasicBlockId final_block = ctx.current_bb;
  return_unit (expr);

  // Jumps are added at the end to match rustc MIR order for easier comparison.
  add_jump (if_block, then_block);
  add_jump (if_block, final_block);
  add_jump (then_block, final_block);
}

void
ExprStmtBuilder::visit (HIR::IfExprConseqElse &expr)
{
  PlaceId result = ctx.place_db.add_temporary (lookup_type (expr));

  push_switch (visit_expr (*expr.get_if_condition ()));
  BasicBlockId if_block = ctx.current_bb;

  ctx.current_bb = new_bb ();
  auto then_res = visit_expr (*expr.get_if_block ());
  push_assignment (result, then_res);
  BasicBlockId then_block = ctx.current_bb;

  ctx.current_bb = new_bb ();
  auto else_res = visit_expr (*expr.get_else_block ());
  push_assignment (result, else_res);
  BasicBlockId else_block = ctx.current_bb;

  ctx.current_bb = new_bb ();
  BasicBlockId final_block = ctx.current_bb;
  return_place (result);

  // Jumps are added at the end to match rustc MIR order for easier comparison.
  add_jump (if_block, then_block);
  add_jump (if_block, else_block);
  add_jump (then_block, final_block);
  add_jump (else_block, final_block);
}
void
ExprStmtBuilder::visit (HIR::IfLetExpr &expr)
{
  rust_sorry_at (expr.get_locus (), "if let expressions are not supported");
}
void
ExprStmtBuilder::visit (HIR::IfLetExprConseqElse &expr)
{
  rust_sorry_at (expr.get_locus (), "if let expressions are not supported");
}
void
ExprStmtBuilder::visit (HIR::MatchExpr &expr)
{
  //  // TODO
  //  expr.get_scrutinee_expr ()->accept_vis (*this);
  //  PlaceId scrutinee = translated;
  //
  //  BasicBlockId final_bb = new_bb ();
  //
  //  BasicBlockId next_case_bb = new_bb ();
  //  for (auto &match_case : expr.get_match_cases ())
  //    {
  //      BasicBlockId body_bb = new_bb ();
  //
  //      BasicBlockId next_pattern_bb = new_bb ();
  //      for (auto &pat : match_case.get_arm ().get_patterns ())
  //	{
  //	  compile_pattern_validation (*pat, scrutinee);
  //	  push_switch (translated);
  //	  add_jump_to (next_pattern_bb);
  //	  start_new_subsequent_bb ();
  //	  compile_pattern_bindings (*pat, scrutinee);
  //	  add_jump_to (body_bb);
  //
  //	  ctx.current_bb = next_pattern_bb;
  //	  next_pattern_bb = new_bb ();
  //	}
  //      ctx.current_bb = next_pattern_bb;
  //      // No pattern matched, go to the next case.
  //      add_jump_to (next_case_bb);
  //
  //      ctx.current_bb = body_bb;
  //      match_case.get_expr ()->accept_vis (*this);
  //      add_jump_to (final_bb);
  //
  //      ctx.current_bb = next_case_bb;
  //      next_case_bb = new_bb ();
  //    }
  //  add_jump_to (final_bb);
  //
  //  ctx.current_bb = final_bb;
}

void
ExprStmtBuilder::visit (HIR::AwaitExpr &expr)
{
  rust_sorry_at (expr.get_locus (), "await expressions are not supported");
}

void
ExprStmtBuilder::visit (HIR::AsyncBlockExpr &expr)
{
  rust_sorry_at (expr.get_locus (), "async blocks are not supported");
}

void
ExprStmtBuilder::visit (HIR::QualifiedPathInExpression &expr)
{
  PlaceId result;
  // Note: Type is only stored for the expr, not the segment.
  bool ok = resolve_variable (expr.get_final_segment (), result);
  rust_assert (ok);
  return_place (result);
}

void
ExprStmtBuilder::visit (HIR::PathInExpression &expr)
{
  PlaceId result;
  // Note: Type is only stored for the expr, not the segment.
  bool ok = resolve_variable (expr.get_final_segment (), result);
  rust_assert (ok);
  return_place (result);
}

void
ExprStmtBuilder::visit (HIR::LetStmt &stmt)
{
  if (stmt.has_init_expr ())
    {
      auto init = visit_expr (*stmt.get_init_expr ());
      PatternBindingBuilder (ctx, init, stmt.get_type ().get ())
	.go (*stmt.get_pattern ());
    }
  else if (stmt.get_pattern ()->get_pattern_type () == HIR::Pattern::IDENTIFIER)
    {
      auto var = declare_variable (stmt.get_pattern ()->get_mappings ());
      auto &var_place = ctx.place_db[var];
      if (var_place.tyty->get_kind () == TyTy::REF)
	{
	  auto p_type = tl::optional<HIR::ReferenceType *> (
	    static_cast<HIR::ReferenceType *> (stmt.get_type ().get ()));
	  var_place.lifetime = ctx.lookup_lifetime (
	    p_type.map (&HIR::ReferenceType::get_lifetime));
	}
      return;
    }
  else
    {
      // TODO
      rust_sorry_at (stmt.get_locus (), "pattern matching in let statements");
    }
}

void
ExprStmtBuilder::visit (HIR::ExprStmt &stmt)
{
  (void) visit_expr (*stmt.get_expr ());
}

} // namespace BIR
} // namespace Rust