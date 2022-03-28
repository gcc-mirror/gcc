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

#include "rust-compile.h"
#include "rust-compile-item.h"
#include "rust-compile-implitem.h"
#include "rust-compile-expr.h"
#include "rust-compile-struct-field-expr.h"
#include "rust-hir-trait-resolve.h"
#include "rust-hir-path-probe.h"
#include "rust-hir-type-bounds.h"
#include "rust-compile-pattern.h"
#include "rust-constexpr.h"

#include "fold-const.h"
#include "realmpfr.h"
#include "convert.h"
#include "print-tree.h"

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
      ctx->add_statement (compound_assignment);

      return;
    }

  auto operator_expr
    = ctx->get_backend ()->arithmetic_or_logical_expression (op, lhs, rhs,
							     expr.get_locus ());
  tree assignment
    = ctx->get_backend ()->assignment_statement (lhs, operator_expr,
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
CompileExpr::visit (HIR::BorrowExpr &expr)
{
  tree main_expr = CompileExpr::Compile (expr.get_expr ().get (), ctx);
  translated = address_expression (main_expr, expr.get_locus ());
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
CompileExpr::visit (HIR::MatchExpr &expr)
{
  // https://gcc.gnu.org/onlinedocs/gccint/Basic-Statements.html#Basic-Statements
  // TODO
  // SWITCH_ALL_CASES_P is true if the switch includes a default label or the
  // case label ranges cover all possible values of the condition expression

  /* Switch expression.

     TREE_TYPE is the original type of the condition, before any
     language required type conversions.  It may be NULL, in which case
     the original type and final types are assumed to be the same.

     Operand 0 is the expression used to perform the branch,
     Operand 1 is the body of the switch, which probably contains
       CASE_LABEL_EXPRs.  It may also be NULL, in which case operand 2
       must not be NULL.  */
  // DEFTREECODE (SWITCH_EXPR, "switch_expr", tcc_statement, 2)

  /* Used to represent a case label.

     Operand 0 is CASE_LOW.  It may be NULL_TREE, in which case the label
       is a 'default' label.
     Operand 1 is CASE_HIGH.  If it is NULL_TREE, the label is a simple
       (one-value) case label.  If it is non-NULL_TREE, the case is a range.
     Operand 2 is CASE_LABEL, which has the corresponding LABEL_DECL.
     Operand 3 is CASE_CHAIN.  This operand is only used in tree-cfg.cc to
       speed up the lookup of case labels which use a particular edge in
       the control flow graph.  */
  // DEFTREECODE (CASE_LABEL_EXPR, "case_label_expr", tcc_statement, 4)

  TyTy::BaseType *scrutinee_expr_tyty = nullptr;
  if (!ctx->get_tyctx ()->lookup_type (
	expr.get_scrutinee_expr ()->get_mappings ().get_hirid (),
	&scrutinee_expr_tyty))
    {
      translated = error_mark_node;
      return;
    }

  rust_assert (scrutinee_expr_tyty->get_kind () == TyTy::TypeKind::ADT);

  // this will need to change but for now the first pass implementation, lets
  // assert this is the case
  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (scrutinee_expr_tyty);
  rust_assert (adt->is_enum ());
  rust_assert (adt->number_of_variants () > 0);

  TyTy::BaseType *expr_tyty = nullptr;
  if (!ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (),
				       &expr_tyty))
    {
      translated = error_mark_node;
      return;
    }

  fncontext fnctx = ctx->peek_fn ();
  Bvariable *tmp = NULL;
  bool needs_temp = !expr_tyty->is_unit ();
  if (needs_temp)
    {
      tree enclosing_scope = ctx->peek_enclosing_scope ();
      tree block_type = TyTyResolveCompile::compile (ctx, expr_tyty);

      bool is_address_taken = false;
      tree ret_var_stmt = nullptr;
      tmp = ctx->get_backend ()->temporary_variable (
	fnctx.fndecl, enclosing_scope, block_type, NULL, is_address_taken,
	expr.get_locus (), &ret_var_stmt);
      ctx->add_statement (ret_var_stmt);
    }

  // lets compile the scrutinee expression
  tree match_scrutinee_expr
    = CompileExpr::Compile (expr.get_scrutinee_expr ().get (), ctx);

  // need to access the qualifier field, if we use QUAL_UNION_TYPE this would be
  // DECL_QUALIFIER i think. For now this will just access the first record
  // field and its respective qualifier because it will always be set because
  // this is all a big special union
  tree scrutinee_first_record_expr
    = ctx->get_backend ()->struct_field_expression (
      match_scrutinee_expr, 0, expr.get_scrutinee_expr ()->get_locus ());
  tree match_scrutinee_expr_qualifier_expr
    = ctx->get_backend ()->struct_field_expression (
      scrutinee_first_record_expr, 0, expr.get_scrutinee_expr ()->get_locus ());

  // setup the end label so the cases can exit properly
  tree fndecl = fnctx.fndecl;
  Location end_label_locus = expr.get_locus (); // FIXME
  tree end_label
    = ctx->get_backend ()->label (fndecl,
				  "" /* empty creates an artificial label */,
				  end_label_locus);
  tree end_label_decl_statement
    = ctx->get_backend ()->label_definition_statement (end_label);

  // setup the switch-body-block
  Location start_location; // FIXME
  Location end_location;   // FIXME
  tree enclosing_scope = ctx->peek_enclosing_scope ();
  tree switch_body_block
    = ctx->get_backend ()->block (fndecl, enclosing_scope, {}, start_location,
				  end_location);
  ctx->push_block (switch_body_block);

  for (auto &kase : expr.get_match_cases ())
    {
      // for now lets just get single pattern's working
      HIR::MatchArm &kase_arm = kase.get_arm ();
      rust_assert (kase_arm.get_patterns ().size () > 0);

      // generate implicit label
      Location arm_locus = kase_arm.get_locus ();
      tree case_label = ctx->get_backend ()->label (
	fndecl, "" /* empty creates an artificial label */, arm_locus);

      // setup the bindings for the block
      for (auto &kase_pattern : kase_arm.get_patterns ())
	{
	  tree switch_kase_expr
	    = CompilePatternCaseLabelExpr::Compile (kase_pattern.get (),
						    case_label, ctx);
	  ctx->add_statement (switch_kase_expr);

	  CompilePatternBindings::Compile (kase_pattern.get (),
					   match_scrutinee_expr, ctx);
	}

      // compile the expr and setup the assignment if required when tmp != NULL
      tree kase_expr_tree = CompileExpr::Compile (kase.get_expr ().get (), ctx);
      if (tmp != NULL)
	{
	  tree result_reference
	    = ctx->get_backend ()->var_expression (tmp, arm_locus);
	  tree assignment
	    = ctx->get_backend ()->assignment_statement (result_reference,
							 kase_expr_tree,
							 arm_locus);
	  ctx->add_statement (assignment);
	}

      // go to end label
      tree goto_end_label = build1_loc (arm_locus.gcc_location (), GOTO_EXPR,
					void_type_node, end_label);
      ctx->add_statement (goto_end_label);
    }

  // setup the switch expression
  tree match_body = ctx->pop_block ();
  tree match_expr_stmt
    = build2_loc (expr.get_locus ().gcc_location (), SWITCH_EXPR,
		  TREE_TYPE (match_scrutinee_expr_qualifier_expr),
		  match_scrutinee_expr_qualifier_expr, match_body);
  ctx->add_statement (match_expr_stmt);
  ctx->add_statement (end_label_decl_statement);

  if (tmp != NULL)
    {
      translated = ctx->get_backend ()->var_expression (tmp, expr.get_locus ());
    }
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
	  Location lvalue_locus
	    = ctx->get_mappings ()->lookup_location (expected->get_ty_ref ());
	  Location rvalue_locus = argument->get_locus ();
	  rvalue = coercion_site (rvalue, actual, expected, lvalue_locus,
				  rvalue_locus);

	  // add it to the list
	  arguments.push_back (rvalue);
	}

      // the constructor depends on whether this is actually an enum or not if
      // its an enum we need to setup the discriminator
      std::vector<tree> ctor_arguments;
      if (adt->is_enum ())
	{
	  HIR::Expr *discrim_expr = variant->get_discriminant ();
	  tree discrim_expr_node = CompileExpr::Compile (discrim_expr, ctx);
	  tree folded_discrim_expr = ConstCtx::fold (discrim_expr_node);
	  tree qualifier = folded_discrim_expr;

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
      Location lvalue_locus
	= ctx->get_mappings ()->lookup_location (expected->get_ty_ref ());
      Location rvalue_locus = argument->get_locus ();
      rvalue
	= coercion_site (rvalue, actual, expected, lvalue_locus, rvalue_locus);

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

  // apply adjustments for the fn call
  self = resolve_adjustements (*adjustments, self,
			       expr.get_receiver ()->get_locus ());

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
      Location lvalue_locus
	= ctx->get_mappings ()->lookup_location (expected->get_ty_ref ());
      Location rvalue_locus = argument->get_locus ();
      rvalue
	= coercion_site (rvalue, actual, expected, lvalue_locus, rvalue_locus);

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
    return error_mark_node;

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
      return address_expression (fn, expr_locus);
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
	return CompileInherentImplItem::Compile (resolved_item, ctx);

      return CompileInherentImplItem::Compile (resolved_item, ctx, fntype);
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

      return CompileTraitItem::Compile (trait_item_ref->get_hir_trait_item (),
					ctx, fntype, true, expr_locus);
    }
  else
    {
      // FIXME this will be a case to return error_mark_node, there is
      // an error scenario where a Trait Foo has a method Bar, but this
      // receiver does not implement this trait or has an incompatible
      // implementation and we should just return error_mark_node

      rust_assert (candidates.size () == 1);
      auto &candidate = candidates.at (0);
      rust_assert (candidate.is_impl_candidate ());

      HIR::ImplItem *impl_item = candidate.item.impl.impl_item;
      if (!fntype->has_subsititions_defined ())
	return CompileInherentImplItem::Compile (impl_item, ctx);

      return CompileInherentImplItem::Compile (impl_item, ctx, fntype);
    }
}

tree
CompileExpr::resolve_operator_overload (
  Analysis::RustLangItem::ItemType lang_item_type, HIR::OperatorExprMeta expr,
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

  // apply adjustments for the fn call
  tree self = resolve_adjustements (*adjustments, lhs, lhs_expr->get_locus ());

  std::vector<tree> args;
  args.push_back (self); // adjusted self
  if (rhs != nullptr)	 // can be null for negation_expr (unary ones)
    args.push_back (rhs);

  auto fncontext = ctx->peek_fn ();
  return ctx->get_backend ()->call_expression (fncontext.fndecl, fn_expr, args,
					       nullptr, expr.get_locus ());
}

tree
CompileExpr::compile_bool_literal (const HIR::LiteralExpr &expr,
				   const TyTy::BaseType *tyty)
{
  rust_assert (expr.get_lit_type () == HIR::Literal::BOOL);

  const auto literal_value = expr.get_literal ();
  bool bval = literal_value.as_string ().compare ("true") == 0;
  return ctx->get_backend ()->boolean_constant_expression (bval);
}

tree
CompileExpr::compile_integer_literal (const HIR::LiteralExpr &expr,
				      const TyTy::BaseType *tyty)
{
  rust_assert (expr.get_lit_type () == HIR::Literal::INT);
  const auto literal_value = expr.get_literal ();

  tree type = TyTyResolveCompile::compile (ctx, tyty);
  rust_assert (TREE_CODE (type) == INTEGER_TYPE);

  mpz_t ival;
  if (mpz_init_set_str (ival, literal_value.as_string ().c_str (), 10) != 0)
    {
      rust_error_at (expr.get_locus (), "bad number in literal");
      return error_mark_node;
    }

  mpz_t type_min;
  mpz_t type_max;
  mpz_init (type_min);
  mpz_init (type_max);
  get_type_static_bounds (type, type_min, type_max);

  if (mpz_cmp (ival, type_min) < 0 || mpz_cmp (ival, type_max) > 0)
    {
      rust_error_at (expr.get_locus (),
		     "integer overflows the respective type %<%s%>",
		     tyty->get_name ().c_str ());
      return error_mark_node;
    }
  return double_int_to_tree (type, mpz_get_double_int (type, ival, true));
}

tree
CompileExpr::compile_float_literal (const HIR::LiteralExpr &expr,
				    const TyTy::BaseType *tyty)
{
  rust_assert (expr.get_lit_type () == HIR::Literal::FLOAT);
  const auto literal_value = expr.get_literal ();

  mpfr_t fval;
  if (mpfr_init_set_str (fval, literal_value.as_string ().c_str (), 10,
			 MPFR_RNDN)
      != 0)
    {
      rust_error_at (expr.get_locus (), "bad number in literal");
      return error_mark_node;
    }

  tree type = TyTyResolveCompile::compile (ctx, tyty);

  // taken from:
  // see go/gofrontend/expressions.cc:check_float_type
  mpfr_exp_t exp = mpfr_get_exp (fval);
  bool real_value_overflow = exp > TYPE_PRECISION (type);

  REAL_VALUE_TYPE r1;
  real_from_mpfr (&r1, fval, type, GMP_RNDN);
  REAL_VALUE_TYPE r2;
  real_convert (&r2, TYPE_MODE (type), &r1);

  tree real_value = build_real (type, r2);
  if (TREE_OVERFLOW (real_value) || real_value_overflow)
    {
      rust_error_at (expr.get_locus (),
		     "decimal overflows the respective type %<%s%>",
		     tyty->get_name ().c_str ());
      return error_mark_node;
    }

  return real_value;
}

tree
CompileExpr::compile_char_literal (const HIR::LiteralExpr &expr,
				   const TyTy::BaseType *tyty)
{
  rust_assert (expr.get_lit_type () == HIR::Literal::CHAR);
  const auto literal_value = expr.get_literal ();

  // FIXME needs wchar_t
  char c = literal_value.as_string ().c_str ()[0];
  return ctx->get_backend ()->wchar_constant_expression (c);
}

tree
CompileExpr::compile_byte_literal (const HIR::LiteralExpr &expr,
				   const TyTy::BaseType *tyty)
{
  rust_assert (expr.get_lit_type () == HIR::Literal::BYTE);
  const auto literal_value = expr.get_literal ();

  tree type = TyTyResolveCompile::compile (ctx, tyty);
  char c = literal_value.as_string ().c_str ()[0];
  return build_int_cst (type, c);
}

tree
CompileExpr::compile_string_literal (const HIR::LiteralExpr &expr,
				     const TyTy::BaseType *tyty)
{
  rust_assert (expr.get_lit_type () == HIR::Literal::STRING);
  const auto literal_value = expr.get_literal ();

  auto base = ctx->get_backend ()->string_constant_expression (
    literal_value.as_string ());
  return address_expression (base, expr.get_locus ());
}

tree
CompileExpr::compile_byte_string_literal (const HIR::LiteralExpr &expr,
					  const TyTy::BaseType *tyty)
{
  rust_assert (expr.get_lit_type () == HIR::Literal::BYTE_STRING);

  // the type here is &[ty; capacity]
  rust_assert (tyty->get_kind () == TyTy::TypeKind::REF);
  const auto ref_tyty = static_cast<const TyTy::ReferenceType *> (tyty);
  auto base_tyty = ref_tyty->get_base ();
  rust_assert (base_tyty->get_kind () == TyTy::TypeKind::ARRAY);
  auto array_tyty = static_cast<TyTy::ArrayType *> (base_tyty);

  std::string value_str = expr.get_literal ().as_string ();
  std::vector<tree> vals;
  std::vector<unsigned long> indexes;
  for (size_t i = 0; i < value_str.size (); i++)
    {
      char b = value_str.at (i);
      tree bb = ctx->get_backend ()->char_constant_expression (b);
      vals.push_back (bb);
      indexes.push_back (i);
    }

  tree array_type = TyTyResolveCompile::compile (ctx, array_tyty);
  tree constructed
    = ctx->get_backend ()->array_constructor_expression (array_type, indexes,
							 vals,
							 expr.get_locus ());

  return address_expression (constructed, expr.get_locus ());
}

tree
CompileExpr::type_cast_expression (tree type_to_cast_to, tree expr_tree,
				   Location location)
{
  if (type_to_cast_to == error_mark_node || expr_tree == error_mark_node
      || TREE_TYPE (expr_tree) == error_mark_node)
    return error_mark_node;

  if (ctx->get_backend ()->type_size (type_to_cast_to) == 0
      || TREE_TYPE (expr_tree) == void_type_node)
    {
      // Do not convert zero-sized types.
      return expr_tree;
    }
  else if (TREE_CODE (type_to_cast_to) == INTEGER_TYPE)
    {
      tree cast = fold (convert_to_integer (type_to_cast_to, expr_tree));
      // FIXME check for TREE_OVERFLOW?
      return cast;
    }
  else if (TREE_CODE (type_to_cast_to) == REAL_TYPE)
    {
      tree cast = fold (convert_to_real (type_to_cast_to, expr_tree));
      // FIXME
      // We might need to check that the tree is MAX val and thusly saturate it
      // to inf. we can get the bounds and check the value if its >= or <= to
      // the min and max bounds
      //
      // https://github.com/Rust-GCC/gccrs/issues/635
      return cast;
    }
  else if (TREE_CODE (type_to_cast_to) == COMPLEX_TYPE)
    {
      return fold (convert_to_complex (type_to_cast_to, expr_tree));
    }
  else if (TREE_CODE (type_to_cast_to) == POINTER_TYPE
	   && TREE_CODE (TREE_TYPE (expr_tree)) == INTEGER_TYPE)
    {
      return fold (convert_to_pointer (type_to_cast_to, expr_tree));
    }
  else if (TREE_CODE (type_to_cast_to) == RECORD_TYPE
	   || TREE_CODE (type_to_cast_to) == ARRAY_TYPE)
    {
      return fold_build1_loc (location.gcc_location (), VIEW_CONVERT_EXPR,
			      type_to_cast_to, expr_tree);
    }

  return fold_convert_loc (location.gcc_location (), type_to_cast_to,
			   expr_tree);
}

void
CompileExpr::visit (HIR::ArrayExpr &expr)
{
  TyTy::BaseType *tyty = nullptr;
  if (!ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (),
				       &tyty))
    {
      rust_fatal_error (expr.get_locus (),
			"did not resolve type for this array expr");
      return;
    }

  tree array_type = TyTyResolveCompile::compile (ctx, tyty);
  if (TREE_CODE (array_type) != ARRAY_TYPE)
    {
      translated = error_mark_node;
      return;
    }

  rust_assert (tyty->get_kind () == TyTy::TypeKind::ARRAY);
  const TyTy::ArrayType &array_tyty
    = static_cast<const TyTy::ArrayType &> (*tyty);

  HIR::ArrayElems &elements = *expr.get_internal_elements ();
  switch (elements.get_array_expr_type ())
    {
      case HIR::ArrayElems::ArrayExprType::VALUES: {
	HIR::ArrayElemsValues &elems
	  = static_cast<HIR::ArrayElemsValues &> (elements);
	translated
	  = array_value_expr (expr.get_locus (), array_tyty, array_type, elems);
      }
      return;

    case HIR::ArrayElems::ArrayExprType::COPIED:
      HIR::ArrayElemsCopied &elems
	= static_cast<HIR::ArrayElemsCopied &> (elements);
      translated
	= array_copied_expr (expr.get_locus (), array_tyty, array_type, elems);
    }
}

tree
CompileExpr::array_value_expr (Location expr_locus,
			       const TyTy::ArrayType &array_tyty,
			       tree array_type, HIR::ArrayElemsValues &elems)
{
  std::vector<unsigned long> indexes;
  std::vector<tree> constructor;
  size_t i = 0;
  for (auto &elem : elems.get_values ())
    {
      tree translated_expr = CompileExpr::Compile (elem.get (), ctx);
      constructor.push_back (translated_expr);
      indexes.push_back (i++);
    }

  return ctx->get_backend ()->array_constructor_expression (array_type, indexes,
							    constructor,
							    expr_locus);
}

tree
CompileExpr::array_copied_expr (Location expr_locus,
				const TyTy::ArrayType &array_tyty,
				tree array_type, HIR::ArrayElemsCopied &elems)
{
  //  see gcc/cp/typeck2.cc:1369-1401
  gcc_assert (TREE_CODE (array_type) == ARRAY_TYPE);
  tree domain = TYPE_DOMAIN (array_type);
  if (!domain)
    return error_mark_node;

  if (!TREE_CONSTANT (TYPE_MAX_VALUE (domain)))
    {
      rust_error_at (expr_locus, "non const capacity domain %qT", array_type);
      return error_mark_node;
    }

  tree capacity_expr = CompileExpr::Compile (elems.get_num_copies_expr (), ctx);
  if (!TREE_CONSTANT (capacity_expr))
    {
      rust_error_at (expr_locus, "non const num copies %qT", array_type);
      return error_mark_node;
    }

  // get the compiled value
  tree translated_expr = CompileExpr::Compile (elems.get_elem_to_copy (), ctx);

  tree max_domain = TYPE_MAX_VALUE (domain);
  tree min_domain = TYPE_MIN_VALUE (domain);

  auto max = wi::to_offset (max_domain);
  auto min = wi::to_offset (min_domain);
  auto precision = TYPE_PRECISION (TREE_TYPE (domain));
  auto sign = TYPE_SIGN (TREE_TYPE (domain));
  unsigned HOST_WIDE_INT len
    = wi::ext (max - min + 1, precision, sign).to_uhwi ();

  // create the constructor
  size_t idx = 0;
  std::vector<unsigned long> indexes;
  std::vector<tree> constructor;
  for (unsigned HOST_WIDE_INT i = 0; i < len; i++)
    {
      constructor.push_back (translated_expr);
      indexes.push_back (idx++);
    }

  return ctx->get_backend ()->array_constructor_expression (array_type, indexes,
							    constructor,
							    expr_locus);
}

tree
HIRCompileBase::resolve_adjustements (
  std::vector<Resolver::Adjustment> &adjustments, tree expression,
  Location locus)
{
  tree e = expression;
  for (auto &adjustment : adjustments)
    {
      switch (adjustment.get_type ())
	{
	case Resolver::Adjustment::AdjustmentType::ERROR:
	  return error_mark_node;

	case Resolver::Adjustment::AdjustmentType::IMM_REF:
	case Resolver::Adjustment::AdjustmentType::MUT_REF:
	  e = address_expression (e, locus);
	  break;

	case Resolver::Adjustment::AdjustmentType::DEREF:
	case Resolver::Adjustment::AdjustmentType::DEREF_MUT:
	  e = resolve_deref_adjustment (adjustment, e, locus);
	  break;

	case Resolver::Adjustment::AdjustmentType::INDIRECTION:
	  e = resolve_indirection_adjustment (adjustment, e, locus);
	  break;

	case Resolver::Adjustment::AdjustmentType::UNSIZE:
	  e = resolve_unsized_adjustment (adjustment, e, locus);
	  break;
	}
    }

  return e;
}

tree
HIRCompileBase::resolve_deref_adjustment (Resolver::Adjustment &adjustment,
					  tree expression, Location locus)
{
  rust_assert (adjustment.is_deref_adjustment ()
	       || adjustment.is_deref_mut_adjustment ());
  rust_assert (adjustment.has_operator_overload ());

  TyTy::FnType *lookup = adjustment.get_deref_operator_fn ();
  HIR::ImplItem *resolved_item = adjustment.get_deref_hir_item ();

  tree fn_address = error_mark_node;
  if (!lookup->has_subsititions_defined ())
    fn_address = CompileInherentImplItem::Compile (resolved_item, ctx, nullptr,
						   true, locus);
  else
    fn_address = CompileInherentImplItem::Compile (resolved_item, ctx, lookup,
						   true, locus);

  // does it need a reference to call
  tree adjusted_argument = expression;
  bool needs_borrow = adjustment.get_deref_adjustment_type ()
		      != Resolver::Adjustment::AdjustmentType::ERROR;
  if (needs_borrow)
    {
      adjusted_argument = address_expression (expression, locus);
    }

  // make the call
  auto fncontext = ctx->peek_fn ();
  return ctx->get_backend ()->call_expression (fncontext.fndecl, fn_address,
					       {adjusted_argument}, nullptr,
					       locus);
}

tree
HIRCompileBase::resolve_indirection_adjustment (
  Resolver::Adjustment &adjustment, tree expression, Location locus)
{
  tree expected_type
    = TyTyResolveCompile::compile (ctx, adjustment.get_expected ());

  return ctx->get_backend ()->indirect_expression (expected_type, expression,
						   true, /* known_valid*/
						   locus);
}

tree
HIRCompileBase::resolve_unsized_adjustment (Resolver::Adjustment &adjustment,
					    tree expression, Location locus)
{
  // assumes this is an array
  tree expr_type = TREE_TYPE (expression);
  rust_assert (TREE_CODE (expr_type) == ARRAY_TYPE);

  // takes an array and returns a fat-pointer so this becomes a constructor
  // expression
  rust_assert (adjustment.get_expected ()->get_kind ()
	       == TyTy::TypeKind::SLICE);
  tree fat_pointer
    = TyTyResolveCompile::compile (ctx, adjustment.get_expected ());

  // make a constructor for this
  tree data = address_expression (expression, locus);

  // fetch the size from the domain
  tree domain = TYPE_DOMAIN (expr_type);
  tree size = TYPE_MAX_VALUE (domain);

  return ctx->get_backend ()->constructor_expression (fat_pointer, false,
						      {data, size}, -1, locus);
}

void
CompileExpr::visit (HIR::IdentifierExpr &expr)
{
  NodeId ast_node_id = expr.get_mappings ().get_nodeid ();

  bool is_value = false;
  NodeId ref_node_id = UNKNOWN_NODEID;
  if (ctx->get_resolver ()->lookup_resolved_name (ast_node_id, &ref_node_id))
    {
      // these ref_node_ids will resolve to a pattern declaration but we are
      // interested in the definition that this refers to get the parent id
      Resolver::Definition def;
      if (!ctx->get_resolver ()->lookup_definition (ref_node_id, &def))
	{
	  rust_error_at (expr.get_locus (),
			 "unknown reference for resolved name");
	  return;
	}
      ref_node_id = def.parent;
      is_value = true;
    }
  else if (!ctx->get_resolver ()->lookup_resolved_type (ast_node_id,
							&ref_node_id))
    {
      rust_error_at (expr.get_locus (),
		     "Failed to lookup type reference for node: %s",
		     expr.as_string ().c_str ());
      return;
    }

  if (ref_node_id == UNKNOWN_NODEID)
    {
      rust_fatal_error (expr.get_locus (), "unresolved IdentifierExpr: %s",
			expr.as_string ().c_str ());
      return;
    }

  // node back to HIR
  HirId ref;
  if (!ctx->get_mappings ()->lookup_node_to_hir (
	expr.get_mappings ().get_crate_num (), ref_node_id, &ref))
    {
      rust_error_at (expr.get_locus (), "reverse lookup failure");
      return;
    }

  TyTy::BaseType *lookup = nullptr;
  if (!ctx->get_tyctx ()->lookup_type (ref, &lookup))
    {
      rust_fatal_error (expr.get_locus (),
			"failed to find type relevant to this context: %s",
			expr.get_mappings ().as_string ().c_str ());
      return;
    }

  bool is_type_ref = !is_value;
  if (is_type_ref)
    {
      // this might be a case for
      //
      // struct S;
      //
      // fn main() {
      //    let s = S;
      // }

      if (lookup->is_unit ())
	{
	  translated = ctx->get_backend ()->unit_expression ();
	  return;
	}

      // rust actually treats like this an fn call or structs with fields but
      // unit structs are just the struct name lets catch it with an is-unit
      // check
      gcc_unreachable ();
    }

  tree fn = NULL_TREE;
  Bvariable *var = nullptr;
  if (ctx->lookup_const_decl (ref, &translated))
    {
      TREE_USED (translated) = 1;
      return;
    }
  else if (ctx->lookup_function_decl (ref, &fn))
    {
      TREE_USED (fn) = 1;
      translated = address_expression (fn, expr.get_locus ());
    }
  else if (ctx->lookup_var_decl (ref, &var))
    {
      // TREE_USED is setup in the gcc abstraction here
      translated = ctx->get_backend ()->var_expression (var, expr.get_locus ());
    }
  else if (ctx->lookup_pattern_binding (ref, &translated))
    {
      TREE_USED (translated) = 1;
      return;
    }
  else
    {
      // lets try and query compile it to an item/impl item
      HIR::Item *resolved_item = ctx->get_mappings ()->lookup_hir_item (
	expr.get_mappings ().get_crate_num (), ref);
      bool is_hir_item = resolved_item != nullptr;
      if (!is_hir_item)
	{
	  translated = error_mark_node;
	  return;
	}

      if (!lookup->has_subsititions_defined ())
	translated = CompileItem::compile (resolved_item, ctx, nullptr, true,
					   expr.get_locus ());
      else
	translated = CompileItem::compile (resolved_item, ctx, lookup, true,
					   expr.get_locus ());

      if (translated != error_mark_node)
	{
	  TREE_USED (translated) = 1;
	}
    }
}

void
CompileExpr::visit (HIR::RangeFromToExpr &expr)
{
  tree from = CompileExpr::Compile (expr.get_from_expr ().get (), ctx);
  tree to = CompileExpr::Compile (expr.get_to_expr ().get (), ctx);
  if (from == error_mark_node || to == error_mark_node)
    {
      translated = error_mark_node;
      return;
    }

  TyTy::BaseType *tyty = nullptr;
  bool ok
    = ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (), &tyty);
  rust_assert (ok);

  tree adt = TyTyResolveCompile::compile (ctx, tyty);

  // make the constructor
  translated
    = ctx->get_backend ()->constructor_expression (adt, false, {from, to}, -1,
						   expr.get_locus ());
}

void
CompileExpr::visit (HIR::RangeFromExpr &expr)
{
  tree from = CompileExpr::Compile (expr.get_from_expr ().get (), ctx);
  if (from == error_mark_node)
    {
      translated = error_mark_node;
      return;
    }

  TyTy::BaseType *tyty = nullptr;
  bool ok
    = ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (), &tyty);
  rust_assert (ok);

  tree adt = TyTyResolveCompile::compile (ctx, tyty);

  // make the constructor
  translated
    = ctx->get_backend ()->constructor_expression (adt, false, {from}, -1,
						   expr.get_locus ());
}

void
CompileExpr::visit (HIR::RangeToExpr &expr)
{
  tree to = CompileExpr::Compile (expr.get_to_expr ().get (), ctx);
  if (to == error_mark_node)
    {
      translated = error_mark_node;
      return;
    }

  TyTy::BaseType *tyty = nullptr;
  bool ok
    = ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (), &tyty);
  rust_assert (ok);

  tree adt = TyTyResolveCompile::compile (ctx, tyty);

  // make the constructor
  translated
    = ctx->get_backend ()->constructor_expression (adt, false, {to}, -1,
						   expr.get_locus ());
}

void
CompileExpr::visit (HIR::RangeFullExpr &expr)
{
  TyTy::BaseType *tyty = nullptr;
  bool ok
    = ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (), &tyty);
  rust_assert (ok);

  tree adt = TyTyResolveCompile::compile (ctx, tyty);
  translated = ctx->get_backend ()->constructor_expression (adt, false, {}, -1,
							    expr.get_locus ());
}

void
CompileExpr::visit (HIR::RangeFromToInclExpr &expr)
{
  tree from = CompileExpr::Compile (expr.get_from_expr ().get (), ctx);
  tree to = CompileExpr::Compile (expr.get_to_expr ().get (), ctx);
  if (from == error_mark_node || to == error_mark_node)
    {
      translated = error_mark_node;
      return;
    }

  TyTy::BaseType *tyty = nullptr;
  bool ok
    = ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (), &tyty);
  rust_assert (ok);

  tree adt = TyTyResolveCompile::compile (ctx, tyty);

  // make the constructor
  translated
    = ctx->get_backend ()->constructor_expression (adt, false, {from, to}, -1,
						   expr.get_locus ());
}

void
CompileExpr::visit (HIR::ArrayIndexExpr &expr)
{
  tree array_reference = CompileExpr::Compile (expr.get_array_expr (), ctx);
  tree index = CompileExpr::Compile (expr.get_index_expr (), ctx);

  // this might be an core::ops::index lang item situation
  TyTy::FnType *fntype;
  bool is_op_overload = ctx->get_tyctx ()->lookup_operator_overload (
    expr.get_mappings ().get_hirid (), &fntype);
  if (is_op_overload)
    {
      auto lang_item_type = Analysis::RustLangItem::ItemType::INDEX;
      tree operator_overload_call
	= resolve_operator_overload (lang_item_type, expr, array_reference,
				     index, expr.get_array_expr (),
				     expr.get_index_expr ());

      // lookup the expected type for this expression
      TyTy::BaseType *tyty = nullptr;
      bool ok
	= ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (),
					  &tyty);
      rust_assert (ok);
      tree expected_type = TyTyResolveCompile::compile (ctx, tyty);

      // rust deref always returns a reference from this overload then we can
      // actually do the indirection
      translated
	= ctx->get_backend ()->indirect_expression (expected_type,
						    operator_overload_call,
						    true, expr.get_locus ());
      return;
    }

  // lets check if the array is a reference type then we can add an
  // indirection if required
  TyTy::BaseType *array_expr_ty = nullptr;
  bool ok = ctx->get_tyctx ()->lookup_type (
    expr.get_array_expr ()->get_mappings ().get_hirid (), &array_expr_ty);
  rust_assert (ok);

  // do we need to add an indirect reference
  if (array_expr_ty->get_kind () == TyTy::TypeKind::REF)
    {
      TyTy::ReferenceType *r
	= static_cast<TyTy::ReferenceType *> (array_expr_ty);
      TyTy::BaseType *tuple_type = r->get_base ();
      tree array_tyty = TyTyResolveCompile::compile (ctx, tuple_type);

      array_reference
	= ctx->get_backend ()->indirect_expression (array_tyty, array_reference,
						    true, expr.get_locus ());
    }

  translated
    = ctx->get_backend ()->array_index_expression (array_reference, index,
						   expr.get_locus ());
}

} // namespace Compile
} // namespace Rust
