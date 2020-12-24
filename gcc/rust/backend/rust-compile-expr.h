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

#ifndef RUST_COMPILE_EXPR
#define RUST_COMPILE_EXPR

#include "rust-compile-base.h"
#include "rust-compile-tyty.h"
#include "rust-compile-resolve-path.h"
#include "rust-compile-block.h"

namespace Rust {
namespace Compile {

class CompileExpr : public HIRCompileBase
{
public:
  static Bexpression *Compile (HIR::Expr *expr, Context *ctx)
  {
    CompileExpr compiler (ctx);
    expr->accept_vis (compiler);
    return compiler.translated;
  }

  virtual ~CompileExpr () {}

  void visit (HIR::ReturnExpr &expr)
  {
    Bexpression *compiled_expr
      = CompileExpr::Compile (expr.return_expr.get (), ctx);
    rust_assert (compiled_expr != nullptr);

    auto fncontext = ctx->peek_fn ();

    std::vector<Bexpression *> retstmts;
    retstmts.push_back (compiled_expr);
    auto s = ctx->get_backend ()->return_statement (fncontext.fndecl, retstmts,
						    expr.get_locus ());
    ctx->add_statement (s);
  }

  void visit (HIR::CallExpr &expr)
  {
    Bexpression *fn = ResolvePath::Compile (expr.get_fnexpr (), ctx);
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

  void visit (HIR::IdentifierExpr &expr)
  {
    // need to look up the reference for this identifier
    NodeId ref_node_id;
    if (!ctx->get_resolver ()->lookup_resolved_name (
	  expr.get_mappings ().get_nodeid (), &ref_node_id))
      {
	rust_fatal_error (expr.get_locus (), "failed to look up resolved name");
	return;
      }

    printf ("have ast node id %u ref %u for expr [%s]\n",
	    expr.get_mappings ().get_nodeid (), ref_node_id,
	    expr.as_string ().c_str ());

    // these ref_node_ids will resolve to a pattern declaration but we are
    // interested in the definition that this refers to get the parent id
    Resolver::Definition def;
    if (!ctx->get_resolver ()->lookup_definition (ref_node_id, &def))
      {
	rust_error_at (expr.get_locus (), "unknown reference");
	return;
      }

    HirId ref;
    if (!ctx->get_mappings ()->lookup_node_to_hir (
	  expr.get_mappings ().get_crate_num (), def.parent, &ref))
      {
	rust_fatal_error (expr.get_locus (), "reverse lookup failure");
	return;
      }

    // this could be a constant reference
    if (ctx->lookup_const_decl (ref, &translated))
      return;

    // must be an identifier
    Bvariable *var = nullptr;
    if (!ctx->lookup_var_decl (ref, &var))
      {
	rust_fatal_error (expr.get_locus (),
			  "failed to lookup compiled variable");
	return;
      }

    translated = ctx->get_backend ()->var_expression (var, expr.get_locus ());
  }

  void visit (HIR::LiteralExpr &expr)
  {
    switch (expr.get_lit_type ())
      {
	case HIR::Literal::BOOL: {
	  bool bval = expr.as_string ().compare ("true") == 0;
	  translated = ctx->get_backend ()->boolean_constant_expression (bval);
	}
	return;

	case HIR::Literal::INT: {
	  mpz_t ival;
	  if (mpz_init_set_str (ival, expr.as_string ().c_str (), 10) != 0)
	    {
	      rust_fatal_error (expr.get_locus (), "bad number in literal");
	      return;
	    }

	  TyTy::TyBase *tyty = nullptr;
	  if (!ctx->get_tyctx ()->lookup_type (
		expr.get_mappings ().get_hirid (), &tyty))
	    {
	      rust_fatal_error (expr.get_locus (),
				"did not resolve type for this literal expr");
	      return;
	    }

	  Btype *type = TyTyResolveCompile::compile (ctx, tyty);
	  translated
	    = ctx->get_backend ()->integer_constant_expression (type, ival);
	}
	return;

      default:
	rust_fatal_error (expr.get_locus (), "unknown literal");
	return;
      }

    gcc_unreachable ();
  }

  void visit (HIR::AssignmentExpr &expr)
  {
    fncontext fn = ctx->peek_fn ();
    auto lhs = CompileExpr::Compile (expr.get_lhs (), ctx);
    auto rhs = CompileExpr::Compile (expr.get_rhs (), ctx);

    Bstatement *assignment
      = ctx->get_backend ()->assignment_statement (fn.fndecl, lhs, rhs,
						   expr.get_locus ());
    ctx->add_statement (assignment);
  }

  void visit (HIR::ArrayIndexExpr &expr)
  {
    Bexpression *array = CompileExpr::Compile (expr.get_array_expr (), ctx);
    Bexpression *index = CompileExpr::Compile (expr.get_index_expr (), ctx);
    translated
      = ctx->get_backend ()->array_index_expression (array, index,
						     expr.get_locus ());
  }

  void visit (HIR::ArrayExpr &expr)
  {
    TyTy::TyBase *tyty = nullptr;
    if (!ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (),
					 &tyty))
      {
	rust_fatal_error (expr.get_locus (),
			  "did not resolve type for this array expr");
	return;
      }

    Btype *array_type = TyTyResolveCompile::compile (ctx, tyty);

    expr.get_internal_elements ()->accept_vis (*this);
    std::vector<unsigned long> indexes;
    for (size_t i = 0; i < constructor.size (); i++)
      indexes.push_back (i);

    translated
      = ctx->get_backend ()->array_constructor_expression (array_type, indexes,
							   constructor,
							   expr.get_locus ());
  }

  void visit (HIR::ArrayElemsValues &elems)
  {
    elems.iterate ([&] (HIR::Expr *e) mutable -> bool {
      Bexpression *translated_expr = CompileExpr::Compile (e, ctx);
      constructor.push_back (translated_expr);
      return true;
    });
  }

  void visit (HIR::ArrayElemsCopied &elems)
  {
    Bexpression *translated_expr
      = CompileExpr::Compile (elems.get_elem_to_copy (), ctx);

    for (size_t i = 0; i < elems.get_num_elements (); ++i)
      constructor.push_back (translated_expr);
  }

  void visit (HIR::ArithmeticOrLogicalExpr &expr)
  {
    Operator op;
    switch (expr.get_expr_type ())
      {
      case HIR::ArithmeticOrLogicalExpr::ADD:
	op = OPERATOR_PLUS;
	break;
      case HIR::ArithmeticOrLogicalExpr::SUBTRACT:
	op = OPERATOR_MINUS;
	break;
      case HIR::ArithmeticOrLogicalExpr::MULTIPLY:
	op = OPERATOR_MULT;
	break;
      case HIR::ArithmeticOrLogicalExpr::DIVIDE:
	op = OPERATOR_DIV;
	break;
      case HIR::ArithmeticOrLogicalExpr::MODULUS:
	op = OPERATOR_MOD;
	break;
      case HIR::ArithmeticOrLogicalExpr::BITWISE_AND:
	op = OPERATOR_AND;
	break;
      case HIR::ArithmeticOrLogicalExpr::BITWISE_OR:
	op = OPERATOR_OR;
	break;
      case HIR::ArithmeticOrLogicalExpr::BITWISE_XOR:
	op = OPERATOR_XOR;
	break;
      case HIR::ArithmeticOrLogicalExpr::LEFT_SHIFT:
	op = OPERATOR_LSHIFT;
	break;
      case HIR::ArithmeticOrLogicalExpr::RIGHT_SHIFT:
	op = OPERATOR_RSHIFT;
	break;
      default:
	rust_fatal_error (expr.get_locus (), "failed to compile operator");
	return;
      }

    auto lhs = CompileExpr::Compile (expr.get_lhs (), ctx);
    auto rhs = CompileExpr::Compile (expr.get_rhs (), ctx);

    translated = ctx->get_backend ()->binary_expression (op, lhs, rhs,
							 expr.get_locus ());
  }

  void visit (HIR::ComparisonExpr &expr)
  {
    Operator op;
    switch (expr.get_expr_type ())
      {
      case HIR::ComparisonExpr::EQUAL:
	op = OPERATOR_EQEQ;
	break;
      case HIR::ComparisonExpr::NOT_EQUAL:
	op = OPERATOR_NOTEQ;
	break;
      case HIR::ComparisonExpr::GREATER_THAN:
	op = OPERATOR_GT;
	break;
      case HIR::ComparisonExpr::LESS_THAN:
	op = OPERATOR_LT;
	break;
      case HIR::ComparisonExpr::GREATER_OR_EQUAL:
	op = OPERATOR_GE;
	break;
      case HIR::ComparisonExpr::LESS_OR_EQUAL:
	op = OPERATOR_LE;
	break;
      default:
	rust_fatal_error (expr.get_locus (), "failed to compile operator");
	return;
      }

    auto lhs = CompileExpr::Compile (expr.get_lhs (), ctx);
    auto rhs = CompileExpr::Compile (expr.get_rhs (), ctx);

    translated = ctx->get_backend ()->binary_expression (op, lhs, rhs,
							 expr.get_locus ());
  }

  void visit (HIR::LazyBooleanExpr &expr)
  {
    Operator op;
    switch (expr.get_expr_type ())
      {
      case HIR::LazyBooleanExpr::LOGICAL_OR:
	op = OPERATOR_OROR;
	break;
      case HIR::LazyBooleanExpr::LOGICAL_AND:
	op = OPERATOR_ANDAND;
	break;
      default:
	rust_fatal_error (expr.get_locus (), "failed to compile operator");
	return;
      }

    auto lhs = CompileExpr::Compile (expr.get_lhs (), ctx);
    auto rhs = CompileExpr::Compile (expr.get_rhs (), ctx);

    translated = ctx->get_backend ()->binary_expression (op, lhs, rhs,
							 expr.get_locus ());
  }

  void visit (HIR::IfExpr &expr)
  {
    auto stmt = CompileConditionalBlocks::compile (&expr, ctx);
    ctx->add_statement (stmt);
  }

  void visit (HIR::IfExprConseqElse &expr)
  {
    auto stmt = CompileConditionalBlocks::compile (&expr, ctx);
    ctx->add_statement (stmt);
  }

  void visit (HIR::IfExprConseqIf &expr)
  {
    auto stmt = CompileConditionalBlocks::compile (&expr, ctx);
    ctx->add_statement (stmt);
  }

  void visit (HIR::BlockExpr &expr)
  {
    auto code_block = CompileBlock::compile (&expr, ctx);
    auto block_stmt = ctx->get_backend ()->block_statement (code_block);
    ctx->add_statement (block_stmt);
  }

private:
  CompileExpr (Context *ctx) : HIRCompileBase (ctx), translated (nullptr) {}

  Bexpression *translated;
  std::vector<Bexpression *> constructor;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_EXPR
