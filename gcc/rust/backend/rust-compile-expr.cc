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
  tree assignment
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

void
CompileExpr::visit (HIR::DereferenceExpr &expr)
{
  TyTy::BaseType *tyty = nullptr;
  if (!ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (),
				       &tyty))
    {
      rust_fatal_error (expr.get_locus (),
			"did not resolve type for this TupleExpr");
      return;
    }

  tree main_expr = CompileExpr::Compile (expr.get_expr ().get (), ctx);

  // this might be an operator overload situation lets check
  TyTy::FnType *fntype;
  bool is_op_overload = ctx->get_tyctx ()->lookup_operator_overload (
    expr.get_mappings ().get_hirid (), &fntype);
  if (is_op_overload)
    {
      auto lang_item_type = Analysis::RustLangItem::ItemType::DEREF;
      tree operator_overload_call
	= resolve_operator_overload (lang_item_type, expr, main_expr, nullptr,
				     expr.get_expr ().get (), nullptr);

      // rust deref always returns a reference from this overload then we can
      // actually do the indirection
      main_expr = operator_overload_call;
    }

  tree expected_type = TyTyResolveCompile::compile (ctx, tyty);
  bool known_valid = true;
  translated
    = ctx->get_backend ()->indirect_expression (expected_type, main_expr,
						known_valid, expr.get_locus ());
}

void
CompileExpr::visit (HIR::CallExpr &expr)
{
  TyTy::BaseType *tyty = nullptr;
  if (!ctx->get_tyctx ()->lookup_type (
	expr.get_fnexpr ()->get_mappings ().get_hirid (), &tyty))
    {
      rust_error_at (expr.get_locus (), "unknown type");
      return;
    }

  // must be a tuple constructor
  bool is_fn = tyty->get_kind () == TyTy::TypeKind::FNDEF
	       || tyty->get_kind () == TyTy::TypeKind::FNPTR;
  bool is_adt_ctor = !is_fn;
  if (is_adt_ctor)
    {
      rust_assert (tyty->get_kind () == TyTy::TypeKind::ADT);
      TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (tyty);
      tree compiled_adt_type = TyTyResolveCompile::compile (ctx, tyty);

      // what variant is it?
      int union_disriminator = -1;
      TyTy::VariantDef *variant = nullptr;
      if (!adt->is_enum ())
	{
	  rust_assert (adt->number_of_variants () == 1);
	  variant = adt->get_variants ().at (0);
	}
      else
	{
	  HirId variant_id;
	  bool ok = ctx->get_tyctx ()->lookup_variant_definition (
	    expr.get_fnexpr ()->get_mappings ().get_hirid (), &variant_id);
	  rust_assert (ok);

	  ok = adt->lookup_variant_by_id (variant_id, &variant,
					  &union_disriminator);
	  rust_assert (ok);
	}

      // this assumes all fields are in order from type resolution and if a
      // base struct was specified those fields are filed via accesors
      std::vector<tree> arguments;
      for (size_t i = 0; i < expr.get_arguments ().size (); i++)
	{
	  auto &argument = expr.get_arguments ().at (i);
	  auto rvalue = CompileExpr::Compile (argument.get (), ctx);

	  // assignments are coercion sites so lets convert the rvalue if
	  // necessary
	  auto respective_field = variant->get_field_at_index (i);
	  auto expected = respective_field->get_field_type ();

	  TyTy::BaseType *actual = nullptr;
	  bool ok = ctx->get_tyctx ()->lookup_type (
	    argument->get_mappings ().get_hirid (), &actual);
	  rust_assert (ok);

	  // coerce it if required
	  rvalue = coercion_site (rvalue, actual, expected, expr.get_locus ());

	  // add it to the list
	  arguments.push_back (rvalue);
	}

      // the constructor depends on whether this is actually an enum or not if
      // its an enum we need to setup the discriminator
      std::vector<tree> ctor_arguments;
      if (adt->is_enum ())
	{
	  HirId variant_id = variant->get_id ();
	  mpz_t val;
	  mpz_init_set_ui (val, variant_id);

	  tree t = TyTyResolveCompile::get_implicit_enumeral_node_type (ctx);
	  tree qualifier
	    = double_int_to_tree (t, mpz_get_double_int (t, val, true));
	  ctor_arguments.push_back (qualifier);
	}
      for (auto &arg : arguments)
	ctor_arguments.push_back (arg);

      translated = ctx->get_backend ()->constructor_expression (
	compiled_adt_type, adt->is_enum (), ctor_arguments, union_disriminator,
	expr.get_locus ());

      return;
    }

  auto get_parameter_tyty_at_index
    = [] (const TyTy::BaseType *base, size_t index,
	  TyTy::BaseType **result) -> bool {
    bool is_fn = base->get_kind () == TyTy::TypeKind::FNDEF
		 || base->get_kind () == TyTy::TypeKind::FNPTR;
    rust_assert (is_fn);

    if (base->get_kind () == TyTy::TypeKind::FNPTR)
      {
	const TyTy::FnPtr *fn = static_cast<const TyTy::FnPtr *> (base);
	*result = fn->param_at (index);

	return true;
      }

    const TyTy::FnType *fn = static_cast<const TyTy::FnType *> (base);
    auto param = fn->param_at (index);
    *result = param.second;

    return true;
  };

  bool is_varadic = false;
  if (tyty->get_kind () == TyTy::TypeKind::FNDEF)
    {
      const TyTy::FnType *fn = static_cast<const TyTy::FnType *> (tyty);
      is_varadic = fn->is_varadic ();
    }

  size_t required_num_args;
  if (tyty->get_kind () == TyTy::TypeKind::FNDEF)
    {
      const TyTy::FnType *fn = static_cast<const TyTy::FnType *> (tyty);
      required_num_args = fn->num_params ();
    }
  else
    {
      const TyTy::FnPtr *fn = static_cast<const TyTy::FnPtr *> (tyty);
      required_num_args = fn->num_params ();
    }

  std::vector<tree> args;
  for (size_t i = 0; i < expr.get_arguments ().size (); i++)
    {
      auto &argument = expr.get_arguments ().at (i);
      auto rvalue = CompileExpr::Compile (argument.get (), ctx);

      if (is_varadic && i >= required_num_args)
	{
	  args.push_back (rvalue);
	  continue;
	}

      // assignments are coercion sites so lets convert the rvalue if
      // necessary
      bool ok;
      TyTy::BaseType *expected = nullptr;
      ok = get_parameter_tyty_at_index (tyty, i, &expected);
      rust_assert (ok);

      TyTy::BaseType *actual = nullptr;
      ok = ctx->get_tyctx ()->lookup_type (
	argument->get_mappings ().get_hirid (), &actual);
      rust_assert (ok);

      // coerce it if required
      rvalue = coercion_site (rvalue, actual, expected, expr.get_locus ());

      // add it to the list
      args.push_back (rvalue);
    }

  // must be a call to a function
  auto fn_address = CompileExpr::Compile (expr.get_fnexpr (), ctx);
  auto fncontext = ctx->peek_fn ();
  translated
    = ctx->get_backend ()->call_expression (fncontext.fndecl, fn_address, args,
					    nullptr, expr.get_locus ());
}

void
CompileExpr::visit (HIR::MethodCallExpr &expr)
{
  // method receiver
  tree self = CompileExpr::Compile (expr.get_receiver ().get (), ctx);

  // lookup the resolved name
  NodeId resolved_node_id = UNKNOWN_NODEID;
  if (!ctx->get_resolver ()->lookup_resolved_name (
	expr.get_mappings ().get_nodeid (), &resolved_node_id))
    {
      rust_error_at (expr.get_locus (), "failed to lookup resolved MethodCall");
      return;
    }

  // reverse lookup
  HirId ref;
  if (!ctx->get_mappings ()->lookup_node_to_hir (
	expr.get_mappings ().get_crate_num (), resolved_node_id, &ref))
    {
      rust_fatal_error (expr.get_locus (), "reverse lookup failure");
      return;
    }

  // lookup the expected function type
  TyTy::BaseType *lookup_fntype = nullptr;
  bool ok = ctx->get_tyctx ()->lookup_type (
    expr.get_method_name ().get_mappings ().get_hirid (), &lookup_fntype);
  rust_assert (ok);
  rust_assert (lookup_fntype->get_kind () == TyTy::TypeKind::FNDEF);
  TyTy::FnType *fntype = static_cast<TyTy::FnType *> (lookup_fntype);

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
      for (auto &arg : expr.get_arguments ())
	arguments.push_back (arg.get ());

      translated = compile_dyn_dispatch_call (dyn, receiver, fntype, self,
					      arguments, expr.get_locus ());
      return;
    }

  // lookup compiled functions since it may have already been compiled
  HIR::PathExprSegment method_name = expr.get_method_name ();
  HIR::PathIdentSegment segment_name = method_name.get_segment ();
  tree fn_expr
    = resolve_method_address (fntype, ref, receiver, segment_name,
			      expr.get_mappings (), expr.get_locus ());

  // lookup the autoderef mappings
  std::vector<Resolver::Adjustment> *adjustments = nullptr;
  ok = ctx->get_tyctx ()->lookup_autoderef_mappings (
    expr.get_mappings ().get_hirid (), &adjustments);
  rust_assert (ok);

  for (auto &adjustment : *adjustments)
    {
      switch (adjustment.get_type ())
	{
	case Resolver::Adjustment::AdjustmentType::IMM_REF:
	case Resolver::Adjustment::AdjustmentType::MUT_REF:
	  self = ctx->get_backend ()->address_expression (
	    self, expr.get_receiver ()->get_locus ());
	  break;

	case Resolver::Adjustment::AdjustmentType::DEREF_REF:
	  tree expected_type
	    = TyTyResolveCompile::compile (ctx, adjustment.get_expected ());
	  self = ctx->get_backend ()->indirect_expression (
	    expected_type, self, true, /* known_valid*/
	    expr.get_receiver ()->get_locus ());
	  break;
	}
    }

  std::vector<tree> args;
  args.push_back (self); // adjusted self

  // normal args
  for (size_t i = 0; i < expr.get_arguments ().size (); i++)
    {
      auto &argument = expr.get_arguments ().at (i);
      auto rvalue = CompileExpr::Compile (argument.get (), ctx);

      // assignments are coercion sites so lets convert the rvalue if
      // necessary, offset from the already adjusted implicit self
      bool ok;
      TyTy::BaseType *expected = fntype->param_at (i + 1).second;

      TyTy::BaseType *actual = nullptr;
      ok = ctx->get_tyctx ()->lookup_type (
	argument->get_mappings ().get_hirid (), &actual);
      rust_assert (ok);

      // coerce it if required
      rvalue = coercion_site (rvalue, actual, expected, expr.get_locus ());

      // add it to the list
      args.push_back (rvalue);
    }

  auto fncontext = ctx->peek_fn ();
  translated
    = ctx->get_backend ()->call_expression (fncontext.fndecl, fn_expr, args,
					    nullptr, expr.get_locus ());
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
  tree enclosing_scope = ctx->peek_enclosing_scope ();
  bool is_address_taken = false;
  tree ret_var_stmt = NULL_TREE;
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
  tree fn = NULL_TREE;
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
