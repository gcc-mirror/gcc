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

#include "rust-compile.h"
#include "rust-compile-item.h"
#include "rust-compile-expr.h"
#include "rust-compile-struct-field-expr.h"
#include "rust-hir-trait-resolve.h"
#include "rust-hir-path-probe.h"
#include "rust-hir-dot-operator.h"

namespace Rust {
namespace Compile {

CompileCrate::CompileCrate (HIR::Crate &crate, Context *ctx)
  : crate (crate), ctx (ctx)
{}

CompileCrate::~CompileCrate () {}

void
CompileCrate::Compile (HIR::Crate &crate, Context *ctx)

{
  CompileCrate c (crate, ctx);
  c.go ();
}

void
CompileCrate::go ()
{
  for (auto &item : crate.items)
    CompileItem::compile (item.get (), ctx, false);

  for (auto &item : crate.items)
    CompileItem::compile (item.get (), ctx, true);
}

// rust-compile-expr.h

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
  if (!is_fn)
    {
      Btype *type = TyTyResolveCompile::compile (ctx, tyty);

      // this assumes all fields are in order from type resolution and if a
      // base struct was specified those fields are filed via accesors
      std::vector<Bexpression *> vals;
      expr.iterate_params ([&] (HIR::Expr *argument) mutable -> bool {
	Bexpression *e = CompileExpr::Compile (argument, ctx);
	vals.push_back (e);
	return true;
      });

      translated
	= ctx->get_backend ()->constructor_expression (type, vals, -1,
						       expr.get_locus ());
    }
  else
    {
      // must be a call to a function
      Bexpression *fn = CompileExpr::Compile (expr.get_fnexpr (), ctx);
      rust_assert (fn != nullptr);

      std::vector<Bexpression *> args;
      expr.iterate_params ([&] (HIR::Expr *p) mutable -> bool {
	Bexpression *compiled_expr = CompileExpr::Compile (p, ctx);
	rust_assert (compiled_expr != nullptr);
	args.push_back (compiled_expr);
	return true;
      });

      auto fncontext = ctx->peek_fn ();
      translated
	= ctx->get_backend ()->call_expression (fncontext.fndecl, fn, args,
						nullptr, expr.get_locus ());
    }
}

void
CompileExpr::visit (HIR::MethodCallExpr &expr)
{
  // method receiver
  Bexpression *self = CompileExpr::Compile (expr.get_receiver ().get (), ctx);

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
      TyTy::DynamicObjectType *dyn
	= static_cast<TyTy::DynamicObjectType *> (receiver->get_root ());

      size_t offs = 0;
      const Resolver::TraitItemReference *ref = nullptr;
      for (auto &item : dyn->get_object_items ())
	{
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
	{
	  translated = ctx->get_backend ()->error_expression ();
	  return;
	}

      // get any indirection sorted out
      auto receiver_ref = self;
      if (receiver->get_kind () == TyTy::TypeKind::REF)
	{
	  TyTy::ReferenceType *r
	    = static_cast<TyTy::ReferenceType *> (receiver);
	  auto indirect_ty = r->get_base ();
	  Btype *indrect_compiled_tyty
	    = TyTyResolveCompile::compile (ctx, indirect_ty);

	  Bexpression *indirect
	    = ctx->get_backend ()->indirect_expression (indrect_compiled_tyty,
							receiver_ref, true,
							expr.get_locus ());
	  receiver_ref = indirect;
	}

      // access the offs + 1 for the fnptr and offs=0 for the reciever obj
      Bexpression *self_argument
	= ctx->get_backend ()->struct_field_expression (receiver_ref, 0,
							expr.get_locus ());

      // access the vtable for the fn
      Bexpression *fn_vtable_access
	= ctx->get_backend ()->struct_field_expression (receiver_ref, offs + 1,
							expr.get_locus ());

      // cast it to the correct fntype
      Btype *expected_fntype = TyTyResolveCompile::compile (ctx, fntype, true);
      Bexpression *fn_convert_expr
	= ctx->get_backend ()->convert_expression (expected_fntype,
						   fn_vtable_access,
						   expr.get_locus ());

      fncontext fnctx = ctx->peek_fn ();
      Bblock *enclosing_scope = ctx->peek_enclosing_scope ();
      bool is_address_taken = false;
      Bstatement *ret_var_stmt = nullptr;

      Bvariable *fn_convert_expr_tmp = ctx->get_backend ()->temporary_variable (
	fnctx.fndecl, enclosing_scope, expected_fntype, fn_convert_expr,
	is_address_taken, expr.get_locus (), &ret_var_stmt);
      ctx->add_statement (ret_var_stmt);

      std::vector<Bexpression *> args;
      args.push_back (self_argument);
      expr.iterate_params ([&] (HIR::Expr *p) mutable -> bool {
	Bexpression *compiled_expr = CompileExpr::Compile (p, ctx);
	rust_assert (compiled_expr != nullptr);
	args.push_back (compiled_expr);
	return true;
      });

      Bexpression *fn_expr
	= ctx->get_backend ()->var_expression (fn_convert_expr_tmp,
					       expr.get_locus ());

      translated
	= ctx->get_backend ()->call_expression (fnctx.fndecl, fn_expr, args,
						nullptr, expr.get_locus ());
      return;
    }

  // lookup compiled functions
  Bfunction *fn = nullptr;
  if (!ctx->lookup_function_decl (fntype->get_ty_ref (), &fn))
    {
      // this might fail because its a forward decl so we can attempt to
      // resolve it now
      HIR::ImplItem *resolved_item = ctx->get_mappings ()->lookup_hir_implitem (
	expr.get_mappings ().get_crate_num (), ref, nullptr);
      if (resolved_item == nullptr)
	{
	  // it might be resolved to a trait item
	  HIR::TraitItem *trait_item
	    = ctx->get_mappings ()->lookup_hir_trait_item (
	      expr.get_mappings ().get_crate_num (), ref);
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
	    = Resolver::PathProbeType::Probe (
	      root, expr.get_method_name ().get_segment (), true, false, true);

	  if (candidates.size () == 0)
	    {
	      // this means we are defaulting back to the trait_item if
	      // possible
	      Resolver::TraitItemReference *trait_item_ref = nullptr;
	      bool ok = trait_ref->lookup_hir_trait_item (*trait_item,
							  &trait_item_ref);
	      rust_assert (ok);				    // found
	      rust_assert (trait_item_ref->is_optional ()); // has definition

	      // FIXME Optional means it has a definition and an associated
	      // block which can be a default implementation, if it does not
	      // contain an implementation we should actually return
	      // error_mark_node

	      TyTy::BaseType *self_type = nullptr;
	      if (!ctx->get_tyctx ()->lookup_type (
		    expr.get_receiver ()->get_mappings ().get_hirid (),
		    &self_type))
		{
		  rust_error_at (expr.get_locus (),
				 "failed to resolve type for self param");
		  return;
		}

	      CompileTraitItem::Compile (self_type,
					 trait_item_ref->get_hir_trait_item (),
					 ctx, fntype);
	      if (!ctx->lookup_function_decl (fntype->get_ty_ref (), &fn))
		{
		  translated = ctx->get_backend ()->error_expression ();
		  rust_error_at (expr.get_locus (),
				 "forward declaration was not compiled");
		  return;
		}
	    }
	  else
	    {
	      std::vector<Resolver::Adjustment> adjustments;
	      Resolver::PathProbeCandidate *candidate
		= Resolver::MethodResolution::Select (candidates, root,
						      adjustments);

	      // FIXME this will be a case to return error_mark_node, there is
	      // an error scenario where a Trait Foo has a method Bar, but this
	      // receiver does not implement this trait or has an incompatible
	      // implementation and we should just return error_mark_node
	      rust_assert (candidate != nullptr);
	      rust_assert (candidate->is_impl_candidate ());

	      HIR::ImplItem *impl_item = candidate->item.impl.impl_item;

	      TyTy::BaseType *self_type = nullptr;
	      if (!ctx->get_tyctx ()->lookup_type (
		    expr.get_receiver ()->get_mappings ().get_hirid (),
		    &self_type))
		{
		  rust_error_at (expr.get_locus (),
				 "failed to resolve type for self param");
		  return;
		}

	      if (!fntype->has_subsititions_defined ())
		CompileInherentImplItem::Compile (self_type, impl_item, ctx,
						  true);
	      else
		CompileInherentImplItem::Compile (self_type, impl_item, ctx,
						  true, fntype);

	      if (!ctx->lookup_function_decl (
		    impl_item->get_impl_mappings ().get_hirid (), &fn))
		{
		  translated = ctx->get_backend ()->error_expression ();
		  rust_error_at (expr.get_locus (),
				 "forward declaration was not compiled");
		  return;
		}
	    }
	}
      else
	{
	  TyTy::BaseType *self_type = nullptr;
	  if (!ctx->get_tyctx ()->lookup_type (
		expr.get_receiver ()->get_mappings ().get_hirid (), &self_type))
	    {
	      rust_error_at (expr.get_locus (),
			     "failed to resolve type for self param");
	      return;
	    }

	  if (!fntype->has_subsititions_defined ())
	    CompileInherentImplItem::Compile (self_type, resolved_item, ctx,
					      true);
	  else
	    CompileInherentImplItem::Compile (self_type, resolved_item, ctx,
					      true, fntype);

	  if (!ctx->lookup_function_decl (fntype->get_ty_ref (), &fn))
	    {
	      translated = ctx->get_backend ()->error_expression ();
	      rust_error_at (expr.get_locus (),
			     "forward declaration was not compiled");
	      return;
	    }
	}
    }

  Bexpression *fn_expr
    = ctx->get_backend ()->function_code_expression (fn, expr.get_locus ());

  std::vector<Bexpression *> args;

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
	  Btype *expected_type
	    = TyTyResolveCompile::compile (ctx, adjustment.get_expected ());
	  self = ctx->get_backend ()->indirect_expression (
	    expected_type, self, true, /* known_valid*/
	    expr.get_receiver ()->get_locus ());
	  break;
	}
    }
  args.push_back (self);

  // normal args
  expr.iterate_params ([&] (HIR::Expr *p) mutable -> bool {
    Bexpression *compiled_expr = CompileExpr::Compile (p, ctx);
    rust_assert (compiled_expr != nullptr);
    args.push_back (compiled_expr);
    return true;
  });

  auto fncontext = ctx->peek_fn ();
  translated
    = ctx->get_backend ()->call_expression (fncontext.fndecl, fn_expr, args,
					    nullptr, expr.get_locus ());
}

// rust-compile-block.h

void
CompileBlock::visit (HIR::BlockExpr &expr)
{
  fncontext fnctx = ctx->peek_fn ();
  Bfunction *fndecl = fnctx.fndecl;
  Location start_location = expr.get_locus ();
  Location end_location = expr.get_closing_locus ();
  auto body_mappings = expr.get_mappings ();

  Resolver::Rib *rib = nullptr;
  if (!ctx->get_resolver ()->find_name_rib (body_mappings.get_nodeid (), &rib))
    {
      rust_fatal_error (expr.get_locus (), "failed to setup locals per block");
      return;
    }

  std::vector<Bvariable *> locals;
  bool ok = compile_locals_for_block (*rib, fndecl, locals);
  rust_assert (ok);

  Bblock *enclosing_scope = ctx->peek_enclosing_scope ();
  Bblock *new_block
    = ctx->get_backend ()->block (fndecl, enclosing_scope, locals,
				  start_location, end_location);
  ctx->push_block (new_block);

  for (auto &s : expr.get_statements ())
    {
      auto compiled_expr = CompileStmt::Compile (s.get (), ctx);
      if (compiled_expr != nullptr)
	{
	  Bstatement *compiled_stmt
	    = ctx->get_backend ()->expression_statement (fnctx.fndecl,
							 compiled_expr);
	  ctx->add_statement (compiled_stmt);
	}
    }

  if (expr.has_expr ())
    {
      // the previous passes will ensure this is a valid return or
      // a valid trailing expression
      Bexpression *compiled_expr = CompileExpr::Compile (expr.expr.get (), ctx);
      if (compiled_expr != nullptr)
	{
	  if (result == nullptr)
	    {
	      Bstatement *final_stmt
		= ctx->get_backend ()->expression_statement (fnctx.fndecl,
							     compiled_expr);
	      ctx->add_statement (final_stmt);
	    }
	  else
	    {
	      Bexpression *result_reference
		= ctx->get_backend ()->var_expression (
		  result, expr.get_final_expr ()->get_locus ());

	      Bstatement *assignment
		= ctx->get_backend ()->assignment_statement (fnctx.fndecl,
							     result_reference,
							     compiled_expr,
							     expr.get_locus ());
	      ctx->add_statement (assignment);
	    }
	}
    }

  ctx->pop_block ();
  translated = new_block;
}

void
CompileConditionalBlocks::visit (HIR::IfExpr &expr)
{
  fncontext fnctx = ctx->peek_fn ();
  Bfunction *fndecl = fnctx.fndecl;
  Bexpression *condition_expr
    = CompileExpr::Compile (expr.get_if_condition (), ctx);
  Bblock *then_block
    = CompileBlock::compile (expr.get_if_block (), ctx, result);

  translated
    = ctx->get_backend ()->if_statement (fndecl, condition_expr, then_block,
					 NULL, expr.get_locus ());
}

void
CompileConditionalBlocks::visit (HIR::IfExprConseqElse &expr)
{
  fncontext fnctx = ctx->peek_fn ();
  Bfunction *fndecl = fnctx.fndecl;
  Bexpression *condition_expr
    = CompileExpr::Compile (expr.get_if_condition (), ctx);
  Bblock *then_block
    = CompileBlock::compile (expr.get_if_block (), ctx, result);
  Bblock *else_block
    = CompileBlock::compile (expr.get_else_block (), ctx, result);

  translated
    = ctx->get_backend ()->if_statement (fndecl, condition_expr, then_block,
					 else_block, expr.get_locus ());
}

void
CompileConditionalBlocks::visit (HIR::IfExprConseqIf &expr)
{
  fncontext fnctx = ctx->peek_fn ();
  Bfunction *fndecl = fnctx.fndecl;
  Bexpression *condition_expr
    = CompileExpr::Compile (expr.get_if_condition (), ctx);
  Bblock *then_block
    = CompileBlock::compile (expr.get_if_block (), ctx, result);

  // else block
  std::vector<Bvariable *> locals;
  Location start_location = expr.get_conseq_if_expr ()->get_locus ();
  Location end_location = expr.get_conseq_if_expr ()->get_locus (); // FIXME
  Bblock *enclosing_scope = ctx->peek_enclosing_scope ();
  Bblock *else_block
    = ctx->get_backend ()->block (fndecl, enclosing_scope, locals,
				  start_location, end_location);
  ctx->push_block (else_block);

  Bstatement *else_stmt_decl
    = CompileConditionalBlocks::compile (expr.get_conseq_if_expr (), ctx,
					 result);
  ctx->add_statement (else_stmt_decl);

  ctx->pop_block ();

  translated
    = ctx->get_backend ()->if_statement (fndecl, condition_expr, then_block,
					 else_block, expr.get_locus ());
}

// rust-compile-struct-field-expr.h

void
CompileStructExprField::visit (HIR::StructExprFieldIdentifierValue &field)
{
  translated = CompileExpr::Compile (field.get_value (), ctx);
}

void
CompileStructExprField::visit (HIR::StructExprFieldIndexValue &field)
{
  translated = CompileExpr::Compile (field.get_value (), ctx);
}

void
CompileStructExprField::visit (HIR::StructExprFieldIdentifier &field)
{
  // we can make the field look like an identifier expr to take advantage of
  // existing code
  HIR::IdentifierExpr expr (field.get_mappings (), field.get_field_name (),
			    field.get_locus ());
  translated = CompileExpr::Compile (&expr, ctx);
}

// Shared methods in compilation

void
HIRCompileBase::compile_function_body (
  Bfunction *fndecl, std::unique_ptr<HIR::BlockExpr> &function_body,
  bool has_return_type)
{
  for (auto &s : function_body->get_statements ())
    {
      auto compiled_expr = CompileStmt::Compile (s.get (), ctx);
      if (compiled_expr != nullptr)
	{
	  Bstatement *compiled_stmt
	    = ctx->get_backend ()->expression_statement (fndecl, compiled_expr);
	  ctx->add_statement (compiled_stmt);
	}
    }

  if (function_body->has_expr ())
    {
      // the previous passes will ensure this is a valid return
      // or a valid trailing expression
      Bexpression *compiled_expr
	= CompileExpr::Compile (function_body->expr.get (), ctx);

      if (compiled_expr != nullptr)
	{
	  if (has_return_type)
	    {
	      std::vector<Bexpression *> retstmts;
	      retstmts.push_back (compiled_expr);

	      auto ret = ctx->get_backend ()->return_statement (
		fndecl, retstmts,
		function_body->get_final_expr ()->get_locus ());
	      ctx->add_statement (ret);
	    }
	  else
	    {
	      Bstatement *final_stmt
		= ctx->get_backend ()->expression_statement (fndecl,
							     compiled_expr);
	      ctx->add_statement (final_stmt);
	    }
	}
    }
}

bool
HIRCompileBase::compile_locals_for_block (Resolver::Rib &rib, Bfunction *fndecl,
					  std::vector<Bvariable *> &locals)
{
  rib.iterate_decls ([&] (NodeId n, Location) mutable -> bool {
    Resolver::Definition d;
    bool ok = ctx->get_resolver ()->lookup_definition (n, &d);
    rust_assert (ok);

    HIR::Stmt *decl = nullptr;
    ok = ctx->get_mappings ()->resolve_nodeid_to_stmt (d.parent, &decl);
    rust_assert (ok);

    // if its a function we extract this out side of this fn context
    // and it is not a local to this function
    bool is_item = ctx->get_mappings ()->lookup_hir_item (
		     decl->get_mappings ().get_crate_num (),
		     decl->get_mappings ().get_hirid ())
		   != nullptr;
    if (is_item)
      {
	HIR::Item *item = static_cast<HIR::Item *> (decl);
	CompileItem::compile (item, ctx, true);
	return true;
      }

    Bvariable *compiled = CompileVarDecl::compile (fndecl, decl, ctx);
    locals.push_back (compiled);

    return true;
  });

  return true;
}

Bexpression *
HIRCompileBase::coercion_site (Bexpression *compiled_ref,
			       TyTy::BaseType *actual, TyTy::BaseType *expected,
			       Location locus)
{
  auto root_actual_kind = actual->get_root ()->get_kind ();
  auto root_expected_kind = expected->get_root ()->get_kind ();

  if (root_expected_kind == TyTy::TypeKind::DYNAMIC
      && root_actual_kind != TyTy::TypeKind::DYNAMIC)
    {
      TyTy::DynamicObjectType *dyn
	= static_cast<TyTy::DynamicObjectType *> (expected->get_root ());
      return coerce_to_dyn_object (compiled_ref, actual, expected, dyn, locus);
    }

  return compiled_ref;
}

Bexpression *
HIRCompileBase::coerce_to_dyn_object (Bexpression *compiled_ref,
				      TyTy::BaseType *actual,
				      TyTy::BaseType *expected,
				      TyTy::DynamicObjectType *ty,
				      Location locus)
{
  Btype *dynamic_object = TyTyResolveCompile::compile (ctx, ty);

  //' this assumes ordering and current the structure is
  // __trait_object_ptr
  // [list of function ptrs]

  std::vector<Bexpression *> vals;
  vals.push_back (compiled_ref);
  for (auto &item : ty->get_object_items ())
    {
      // compute the address of each method item
      auto address = compute_address_for_trait_item (item, actual->get_root ());
      vals.push_back (address);
    }

  Bexpression *constructed_trait_object
    = ctx->get_backend ()->constructor_expression (dynamic_object, vals, -1,

						   locus);

  fncontext fnctx = ctx->peek_fn ();
  Bblock *enclosing_scope = ctx->peek_enclosing_scope ();
  bool is_address_taken = false;
  Bstatement *ret_var_stmt = nullptr;

  Bvariable *dyn_tmp = ctx->get_backend ()->temporary_variable (
    fnctx.fndecl, enclosing_scope, dynamic_object, constructed_trait_object,
    is_address_taken, locus, &ret_var_stmt);
  ctx->add_statement (ret_var_stmt);

  // FIXME this needs to be more generic to apply any covariance

  auto e = expected;
  std::vector<Resolver::Adjustment> adjustments;
  while (e->get_kind () == TyTy::TypeKind::REF)
    {
      auto r = static_cast<TyTy::ReferenceType *> (e);
      e = r->get_base ();

      if (r->is_mutable ())
	adjustments.push_back (
	  Resolver::Adjustment (Resolver::Adjustment::AdjustmentType::MUT_REF,
				e));
      else
	adjustments.push_back (
	  Resolver::Adjustment (Resolver::Adjustment::AdjustmentType::IMM_REF,
				e));
    }

  auto resulting_dyn_object_ref
    = ctx->get_backend ()->var_expression (dyn_tmp, locus);
  for (auto it = adjustments.rbegin (); it != adjustments.rend (); it++)
    {
      bool ok
	= it->get_type () == Resolver::Adjustment::AdjustmentType::IMM_REF
	  || it->get_type () == Resolver::Adjustment::AdjustmentType::MUT_REF;
      rust_assert (ok);

      resulting_dyn_object_ref
	= ctx->get_backend ()->address_expression (resulting_dyn_object_ref,
						   locus);
    }
  return resulting_dyn_object_ref;
}

Bexpression *
HIRCompileBase::compute_address_for_trait_item (
  const Resolver::TraitItemReference *trait_item_ref, TyTy::BaseType *receiver)
{
  TyTy::BaseType *item_type = trait_item_ref->get_tyty ();
  rust_assert (item_type->get_kind () == TyTy::TypeKind::FNDEF);
  TyTy::FnType *fntype = static_cast<TyTy::FnType *> (item_type);

  auto root = receiver->get_root ();
  HIR::PathIdentSegment segment_name (trait_item_ref->get_identifier ());
  std::vector<Resolver::PathProbeCandidate> candidates
    = Resolver::PathProbeType::Probe (root, segment_name, true, false, true);

  // FIXME for default trait item resolution
  //
  // if (candidates.size () == 0)
  //   {
  //     rust_assert (trait_item_ref->is_optional ()); // has definition
  //
  //     CompileTraitItem::Compile (self_type,
  //       			 trait_item_ref->get_hir_trait_item (), ctx,
  //       			 fntype);
  //     if (!ctx->lookup_function_decl (fntype->get_ty_ref (), &fn))
  //       {
  //         return ctx->get_backend ()->error_expression ();
  //       }
  //   }

  rust_assert (!candidates.empty ());
  rust_assert (candidates.size () == 1);

  Resolver::PathProbeCandidate *candidate = &candidates.at (0);
  rust_assert (candidate->is_impl_candidate ());

  HIR::ImplItem *impl_item = candidate->item.impl.impl_item;

  return CompileInherentImplItem::Compile (receiver->get_root (), impl_item,
					   ctx, true, fntype, true,
					   Location () /* FIXME */);
}

} // namespace Compile
} // namespace Rust
