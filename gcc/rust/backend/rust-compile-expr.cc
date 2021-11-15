// Copyright (C) 2020-2021 Free Software Foundation, Inc.

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

#include "rust-compile.h"
#include "rust-compile-item.h"
#include "rust-compile-expr.h"
#include "rust-compile-struct-field-expr.h"
#include "rust-hir-trait-resolve.h"
#include "rust-hir-path-probe.h"
#include "rust-hir-type-bounds.h"
#include "rust-hir-dot-operator.h"

namespace Rust {
namespace Compile {

void
CompileExpr::visit (HIR::ArithmeticOrLogicalExpr &expr)
{
  auto op = expr.get_expr_type ();
  auto lhs = CompileExpr::Compile (expr.get_lhs (), ctx);
  auto rhs = CompileExpr::Compile (expr.get_rhs (), ctx);

  // this might be an operator overload situation lets check
  TyTy::FnType *fntype;
  bool is_op_overload = ctx->get_tyctx ()->lookup_operator_overload (
    expr.get_mappings ().get_hirid (), &fntype);
  if (is_op_overload)
    {
      auto lang_item_type
	= Analysis::RustLangItem::OperatorToLangItem (expr.get_expr_type ());
      translated = resolve_operator_overload (lang_item_type, expr, lhs, rhs,
					      expr.get_lhs (), expr.get_rhs ());
      return;
    }

  translated
    = ctx->get_backend ()->arithmetic_or_logical_expression (op, lhs, rhs,
							     expr.get_locus ());
}

void
CompileExpr::visit (HIR::CompoundAssignmentExpr &expr)
{
  fncontext fn = ctx->peek_fn ();

  auto op = expr.get_expr_type ();
  auto lhs = CompileExpr::Compile (expr.get_left_expr ().get (), ctx);
  auto rhs = CompileExpr::Compile (expr.get_right_expr ().get (), ctx);

  // this might be an operator overload situation lets check
  TyTy::FnType *fntype;
  bool is_op_overload = ctx->get_tyctx ()->lookup_operator_overload (
    expr.get_mappings ().get_hirid (), &fntype);
  if (is_op_overload)
    {
      auto lang_item_type
	= Analysis::RustLangItem::CompoundAssignmentOperatorToLangItem (
	  expr.get_expr_type ());
      auto compound_assignment
	= resolve_operator_overload (lang_item_type, expr, lhs, rhs,
				     expr.get_left_expr ().get (),
				     expr.get_right_expr ().get ());
      auto assignment
	= ctx->get_backend ()->expression_statement (fn.fndecl,
						     compound_assignment);
      ctx->add_statement (assignment);

      return;
    }

  auto operator_expr
    = ctx->get_backend ()->arithmetic_or_logical_expression (op, lhs, rhs,
							     expr.get_locus ());
  Bstatement *assignment
    = ctx->get_backend ()->assignment_statement (fn.fndecl, lhs, operator_expr,
						 expr.get_locus ());
  ctx->add_statement (assignment);
}

void
CompileExpr::visit (HIR::NegationExpr &expr)
{
  auto op = expr.get_expr_type ();
  auto negated_expr = CompileExpr::Compile (expr.get_expr ().get (), ctx);
  auto location = expr.get_locus ();

  // this might be an operator overload situation lets check
  TyTy::FnType *fntype;
  bool is_op_overload = ctx->get_tyctx ()->lookup_operator_overload (
    expr.get_mappings ().get_hirid (), &fntype);
  if (is_op_overload)
    {
      auto lang_item_type
	= Analysis::RustLangItem::NegationOperatorToLangItem (op);
      translated
	= resolve_operator_overload (lang_item_type, expr, negated_expr,
				     nullptr, expr.get_expr ().get (), nullptr);
      return;
    }

  translated
    = ctx->get_backend ()->negation_expression (op, negated_expr, location);
}

tree
CompileExpr::compile_dyn_dispatch_call (const TyTy::DynamicObjectType *dyn,
					TyTy::BaseType *receiver,
					TyTy::FnType *fntype, tree receiver_ref,
					std::vector<HIR::Expr *> &arguments,
					Location expr_locus)
{
  size_t offs = 0;
  const Resolver::TraitItemReference *ref = nullptr;
  for (auto &bound : dyn->get_object_items ())
    {
      const Resolver::TraitItemReference *item = bound.first;
      auto t = item->get_tyty ();
      rust_assert (t->get_kind () == TyTy::TypeKind::FNDEF);
      auto ft = static_cast<TyTy::FnType *> (t);

      if (ft->get_id () == fntype->get_id ())
	{
	  ref = item;
	  break;
	}
      offs++;
    }

  if (ref == nullptr)
    return ctx->get_backend ()->error_expression ();

  // get any indirection sorted out
  if (receiver->get_kind () == TyTy::TypeKind::REF)
    {
      TyTy::ReferenceType *r = static_cast<TyTy::ReferenceType *> (receiver);
      auto indirect_ty = r->get_base ();
      tree indrect_compiled_tyty
	= TyTyResolveCompile::compile (ctx, indirect_ty);

      tree indirect
	= ctx->get_backend ()->indirect_expression (indrect_compiled_tyty,
						    receiver_ref, true,
						    expr_locus);
      receiver_ref = indirect;
    }

  // access the offs + 1 for the fnptr and offs=0 for the reciever obj
  tree self_argument
    = ctx->get_backend ()->struct_field_expression (receiver_ref, 0,
						    expr_locus);

  // access the vtable for the fn
  tree fn_vtable_access
    = ctx->get_backend ()->struct_field_expression (receiver_ref, offs + 1,
						    expr_locus);

  // cast it to the correct fntype
  tree expected_fntype = TyTyResolveCompile::compile (ctx, fntype, true);
  tree fn_convert_expr
    = ctx->get_backend ()->convert_expression (expected_fntype,
					       fn_vtable_access, expr_locus);

  fncontext fnctx = ctx->peek_fn ();
  Bblock *enclosing_scope = ctx->peek_enclosing_scope ();
  bool is_address_taken = false;
  Bstatement *ret_var_stmt = nullptr;
  Bvariable *fn_convert_expr_tmp
    = ctx->get_backend ()->temporary_variable (fnctx.fndecl, enclosing_scope,
					       expected_fntype, fn_convert_expr,
					       is_address_taken, expr_locus,
					       &ret_var_stmt);
  ctx->add_statement (ret_var_stmt);

  std::vector<tree> args;
  args.push_back (self_argument);
  for (auto &argument : arguments)
    {
      tree compiled_expr = CompileExpr::Compile (argument, ctx);
      args.push_back (compiled_expr);
    }

  tree fn_expr
    = ctx->get_backend ()->var_expression (fn_convert_expr_tmp, expr_locus);

  return ctx->get_backend ()->call_expression (fnctx.fndecl, fn_expr, args,
					       nullptr, expr_locus);
}

tree
CompileExpr::resolve_method_address (TyTy::FnType *fntype, HirId ref,
				     TyTy::BaseType *receiver,
				     HIR::PathIdentSegment &segment,
				     Analysis::NodeMapping expr_mappings,
				     Location expr_locus)
{
  // lookup compiled functions since it may have already been compiled
  Bfunction *fn = nullptr;
  if (ctx->lookup_function_decl (fntype->get_ty_ref (), &fn))
    {
      return ctx->get_backend ()->function_code_expression (fn, expr_locus);
    }

  // Now we can try and resolve the address since this might be a forward
  // declared function, generic function which has not be compiled yet or
  // its an not yet trait bound function
  HIR::ImplItem *resolved_item
    = ctx->get_mappings ()->lookup_hir_implitem (expr_mappings.get_crate_num (),
						 ref, nullptr);
  if (resolved_item != nullptr)
    {
      if (!fntype->has_subsititions_defined ())
	return CompileInherentImplItem::Compile (receiver, resolved_item, ctx,
						 true);

      return CompileInherentImplItem::Compile (receiver, resolved_item, ctx,
					       true, fntype);
    }

  // it might be resolved to a trait item
  HIR::TraitItem *trait_item = ctx->get_mappings ()->lookup_hir_trait_item (
    expr_mappings.get_crate_num (), ref);
  HIR::Trait *trait = ctx->get_mappings ()->lookup_trait_item_mapping (
    trait_item->get_mappings ().get_hirid ());

  Resolver::TraitReference *trait_ref
    = &Resolver::TraitReference::error_node ();
  bool ok = ctx->get_tyctx ()->lookup_trait_reference (
    trait->get_mappings ().get_defid (), &trait_ref);
  rust_assert (ok);

  // the type resolver can only resolve type bounds to their trait
  // item so its up to us to figure out if this path should resolve
  // to an trait-impl-block-item or if it can be defaulted to the
  // trait-impl-item's definition

  auto root = receiver->get_root ();
  std::vector<Resolver::PathProbeCandidate> candidates
    = Resolver::PathProbeType::Probe (root, segment, true, false, true);

  if (candidates.size () == 0)
    {
      // this means we are defaulting back to the trait_item if
      // possible
      Resolver::TraitItemReference *trait_item_ref = nullptr;
      bool ok = trait_ref->lookup_hir_trait_item (*trait_item, &trait_item_ref);
      rust_assert (ok);				    // found
      rust_assert (trait_item_ref->is_optional ()); // has definition

      // FIXME Optional means it has a definition and an associated
      // block which can be a default implementation, if it does not
      // contain an implementation we should actually return
      // error_mark_node

      return CompileTraitItem::Compile (receiver,
					trait_item_ref->get_hir_trait_item (),
					ctx, fntype, true, expr_locus);
    }
  else
    {
      std::vector<Resolver::Adjustment> adjustments;
      Resolver::PathProbeCandidate *candidate
	= Resolver::MethodResolution::Select (candidates, root, adjustments);

      // FIXME this will be a case to return error_mark_node, there is
      // an error scenario where a Trait Foo has a method Bar, but this
      // receiver does not implement this trait or has an incompatible
      // implementation and we should just return error_mark_node
      rust_assert (candidate != nullptr);
      rust_assert (candidate->is_impl_candidate ());

      HIR::ImplItem *impl_item = candidate->item.impl.impl_item;
      if (!fntype->has_subsititions_defined ())
	return CompileInherentImplItem::Compile (receiver, impl_item, ctx,
						 true);

      return CompileInherentImplItem::Compile (receiver, impl_item, ctx, true,
					       fntype);
    }
}

tree
CompileExpr::resolve_operator_overload (
  Analysis::RustLangItem::ItemType lang_item_type, HIR::OperatorExpr &expr,
  tree lhs, tree rhs, HIR::Expr *lhs_expr, HIR::Expr *rhs_expr)
{
  TyTy::FnType *fntype;
  bool is_op_overload = ctx->get_tyctx ()->lookup_operator_overload (
    expr.get_mappings ().get_hirid (), &fntype);
  rust_assert (is_op_overload);

  // lookup the resolved name
  NodeId resolved_node_id = UNKNOWN_NODEID;
  bool ok = ctx->get_resolver ()->lookup_resolved_name (
    expr.get_mappings ().get_nodeid (), &resolved_node_id);
  rust_assert (ok);

  // reverse lookup
  HirId ref;
  ok = ctx->get_mappings ()->lookup_node_to_hir (
    expr.get_mappings ().get_crate_num (), resolved_node_id, &ref);
  rust_assert (ok);

  TyTy::BaseType *receiver = nullptr;
  ok = ctx->get_tyctx ()->lookup_receiver (expr.get_mappings ().get_hirid (),
					   &receiver);
  rust_assert (ok);

  bool is_dyn_dispatch
    = receiver->get_root ()->get_kind () == TyTy::TypeKind::DYNAMIC;
  bool is_generic_receiver = receiver->get_kind () == TyTy::TypeKind::PARAM;
  if (is_generic_receiver)
    {
      TyTy::ParamType *p = static_cast<TyTy::ParamType *> (receiver);
      receiver = p->resolve ();
    }

  if (is_dyn_dispatch)
    {
      const TyTy::DynamicObjectType *dyn
	= static_cast<const TyTy::DynamicObjectType *> (receiver->get_root ());

      std::vector<HIR::Expr *> arguments;
      if (rhs_expr != nullptr) // can be null for negation_expr (unary ones)
	arguments.push_back (rhs_expr);

      return compile_dyn_dispatch_call (dyn, receiver, fntype, lhs, arguments,
					expr.get_locus ());
    }

  // lookup compiled functions since it may have already been compiled
  HIR::PathIdentSegment segment_name (
    Analysis::RustLangItem::ToString (lang_item_type));
  tree fn_expr
    = resolve_method_address (fntype, ref, receiver, segment_name,
			      expr.get_mappings (), expr.get_locus ());

  // lookup the autoderef mappings
  std::vector<Resolver::Adjustment> *adjustments = nullptr;
  ok = ctx->get_tyctx ()->lookup_autoderef_mappings (
    expr.get_mappings ().get_hirid (), &adjustments);
  rust_assert (ok);

  // FIXME refactor this out
  tree self = lhs;
  for (auto &adjustment : *adjustments)
    {
      switch (adjustment.get_type ())
	{
	case Resolver::Adjustment::AdjustmentType::IMM_REF:
	case Resolver::Adjustment::AdjustmentType::MUT_REF:
	  self
	    = ctx->get_backend ()->address_expression (self,
						       lhs_expr->get_locus ());
	  break;

	case Resolver::Adjustment::AdjustmentType::DEREF_REF:
	  tree expected_type
	    = TyTyResolveCompile::compile (ctx, adjustment.get_expected ());
	  self
	    = ctx->get_backend ()->indirect_expression (expected_type, self,
							true, /* known_valid*/
							lhs_expr->get_locus ());
	  break;
	}
    }

  std::vector<tree> args;
  args.push_back (self); // adjusted self
  if (rhs != nullptr)	 // can be null for negation_expr (unary ones)
    args.push_back (rhs);

  auto fncontext = ctx->peek_fn ();
  return ctx->get_backend ()->call_expression (fncontext.fndecl, fn_expr, args,
					       nullptr, expr.get_locus ());
}

} // namespace Compile
} // namespace Rust
