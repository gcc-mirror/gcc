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
#include "fnv-hash.h"

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
	  rust_error_at (expr.get_locus (),
			 "failed to lookup forward declaration");
	  return;
	}

      TyTy::BaseType *self_type = nullptr;
      if (!ctx->get_tyctx ()->lookup_type (
	    expr.get_receiver ()->get_mappings ().get_hirid (), &self_type))
	{
	  rust_error_at (expr.get_locus (),
			 "failed to resolve type for self param");
	  return;
	}

      if (!fntype->has_subsititions_defined ())
	CompileInherentImplItem::Compile (self_type, resolved_item, ctx, true);
      else
	CompileInherentImplItem::Compile (self_type, resolved_item, ctx, true,
					  fntype);

      if (!ctx->lookup_function_decl (fntype->get_ty_ref (), &fn))
	{
	  rust_error_at (expr.get_locus (),
			 "forward declaration was not compiled");
	  return;
	}
    }

  Bexpression *fn_expr
    = ctx->get_backend ()->function_code_expression (fn, expr.get_locus ());

  std::vector<Bexpression *> args;

  // method receiver
  Bexpression *self = CompileExpr::Compile (expr.get_receiver ().get (), ctx);
  rust_assert (self != nullptr);
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
		  result, expr.get_final_expr ()->get_locus_slow ());

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
		function_body->get_final_expr ()->get_locus_slow ());
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

// Mr Mangle time

static const std::string kMangledSymbolPrefix = "_ZN";
static const std::string kMangledSymbolDelim = "E";
static const std::string kMangledGenericDelim = "$C$";
static const std::string kMangledSubstBegin = "$LT$";
static const std::string kMangledSubstEnd = "$GT$";

static std::string
mangle_name (const std::string &name)
{
  return std::to_string (name.size ()) + name;
}

// rustc uses a sip128 hash for legacy mangling, but an fnv 128 was quicker to
// implement for now
static std::string
legacy_hash (const std::string &fingerprint)
{
  Hash::FNV128 hasher;
  hasher.write ((const unsigned char *) fingerprint.c_str (),
		fingerprint.size ());

  uint64_t hi, lo;
  hasher.sum (&hi, &lo);

  char hex[16 + 1];
  memset (hex, 0, sizeof hex);
  snprintf (hex, sizeof hex, "%08" PRIx64 "%08" PRIx64, lo, hi);

  return "h" + std::string (hex, sizeof (hex) - 1);
}

static std::string
mangle_self (const TyTy::BaseType *self)
{
  if (self->get_kind () != TyTy::TypeKind::ADT)
    return mangle_name (self->get_name ());

  const TyTy::ADTType *s = static_cast<const TyTy::ADTType *> (self);
  std::string buf = s->get_identifier ();

  if (s->has_subsititions_defined ())
    {
      buf += kMangledSubstBegin;

      const std::vector<TyTy::SubstitutionParamMapping> &params
	= s->get_substs ();
      for (size_t i = 0; i < params.size (); i++)
	{
	  const TyTy::SubstitutionParamMapping &sub = params.at (i);
	  buf += sub.as_string ();

	  if ((i + 1) < params.size ())
	    buf += kMangledGenericDelim;
	}

      buf += kMangledSubstEnd;
    }

  return mangle_name (buf);
}

std::string
Context::mangle_item (const TyTy::BaseType *ty, const std::string &name) const
{
  const std::string &crate_name = mappings->get_current_crate_name ();

  const std::string hash = legacy_hash (ty->as_string ());
  const std::string hash_sig = mangle_name (hash);

  return kMangledSymbolPrefix + mangle_name (crate_name) + mangle_name (name)
	 + hash_sig + kMangledSymbolDelim;
}

std::string
Context::mangle_impl_item (const TyTy::BaseType *self, const TyTy::BaseType *ty,
			   const std::string &name) const
{
  const std::string &crate_name = mappings->get_current_crate_name ();

  const std::string hash = legacy_hash (ty->as_string ());
  const std::string hash_sig = mangle_name (hash);

  return kMangledSymbolPrefix + mangle_name (crate_name) + mangle_self (self)
	 + mangle_name (name) + hash_sig + kMangledSymbolDelim;
}

} // namespace Compile
} // namespace Rust
