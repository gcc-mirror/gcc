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
  // this can be a function call or it can be a constructor for a tuple struct
  Bexpression *fn = ResolvePathRef::Compile (expr.get_fnexpr (), ctx);
  if (fn != nullptr)
    {
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
  else
    {
      Btype *type = ResolvePathType::Compile (expr.get_fnexpr (), ctx);
      if (type == nullptr)
	{
	  rust_fatal_error (expr.get_locus (),
			    "failed to lookup type associated with call");
	  return;
	}

      // this assumes all fields are in order from type resolution and if a base
      // struct was specified those fields are filed via accesors
      std::vector<Bexpression *> vals;
      expr.iterate_params ([&] (HIR::Expr *argument) mutable -> bool {
	Bexpression *e = CompileExpr::Compile (argument, ctx);
	vals.push_back (e);
	return true;
      });

      translated
	= ctx->get_backend ()->constructor_expression (type, vals,
						       expr.get_locus ());
    }
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
  rib->iterate_decls ([&] (NodeId n, Location) mutable -> bool {
    Resolver::Definition d;
    bool ok = ctx->get_resolver ()->lookup_definition (n, &d);
    rust_assert (ok);

    HIR::Stmt *decl = nullptr;
    ok = ctx->get_mappings ()->resolve_nodeid_to_stmt (d.parent, &decl);
    rust_assert (ok);

    Bvariable *compiled = CompileVarDecl::compile (fndecl, decl, ctx);
    locals.push_back (compiled);

    return true;
  });

  Bblock *enclosing_scope = ctx->peek_enclosing_scope ();
  Bblock *new_block
    = ctx->get_backend ()->block (fndecl, enclosing_scope, locals,
				  start_location, end_location);
  ctx->push_block (new_block);

  expr.iterate_stmts ([&] (HIR::Stmt *s) mutable -> bool {
    CompileStmt::Compile (s, ctx);
    return true;
  });

  if (expr.has_expr () && expr.tail_expr_reachable ())
    {
      // the previous passes will ensure this is a valid return
      // dead code elimination should remove any bad trailing expressions
      Bexpression *compiled_expr = CompileExpr::Compile (expr.expr.get (), ctx);
      rust_assert (compiled_expr != nullptr);

      auto fncontext = ctx->peek_fn ();

      std::vector<Bexpression *> retstmts;
      retstmts.push_back (compiled_expr);
      auto s
	= ctx->get_backend ()->return_statement (fncontext.fndecl, retstmts,
						 expr.expr->get_locus_slow ());
      ctx->add_statement (s);
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
  Bblock *then_block = CompileBlock::compile (expr.get_if_block (), ctx);

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
  Bblock *then_block = CompileBlock::compile (expr.get_if_block (), ctx);
  Bblock *else_block = CompileBlock::compile (expr.get_else_block (), ctx);

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
  Bblock *then_block = CompileBlock::compile (expr.get_if_block (), ctx);

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
    = CompileConditionalBlocks::compile (expr.get_conseq_if_expr (), ctx);
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

} // namespace Compile
} // namespace Rust
