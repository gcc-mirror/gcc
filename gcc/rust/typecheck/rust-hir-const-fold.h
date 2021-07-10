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

#ifndef RUST_HIR_CONST_FOLD_H
#define RUST_HIR_CONST_FOLD_H

#include "rust-hir-const-fold-base.h"
#include "rust-tyty-visitor.h"

namespace Rust {
namespace ConstFold {

class ConstFoldType : public TyTy::TyVisitor
{
public:
  static Btype *fold (TyTy::BaseType *type, ::Backend *backend)
  {
    ConstFoldType folder (backend);
    type->accept_vis (folder);
    return folder.translated;
  };

  void visit (TyTy::ErrorType &) override { gcc_unreachable (); }

  void visit (TyTy::InferType &) override { gcc_unreachable (); }

  void visit (TyTy::ADTType &) override { gcc_unreachable (); }

  void visit (TyTy::ArrayType &) override { gcc_unreachable (); }

  void visit (TyTy::ReferenceType &) override { gcc_unreachable (); }

  void visit (TyTy::ParamType &) override { gcc_unreachable (); }

  void visit (TyTy::FnPtr &) override { gcc_unreachable (); }

  void visit (TyTy::FnType &) override { gcc_unreachable (); }

  void visit (TyTy::PlaceholderType &) override { gcc_unreachable (); }

  void visit (TyTy::TupleType &type) override
  {
    if (type.num_fields () == 0)
      translated = backend->unit_type ();
    else
      gcc_unreachable ();
  }

  void visit (TyTy::BoolType &) override
  {
    translated = backend->named_type ("bool", backend->bool_type (),
				      Linemap::predeclared_location ());
  }

  void visit (TyTy::IntType &type) override
  {
    switch (type.get_int_kind ())
      {
      case TyTy::IntType::I8:
	translated
	  = backend->named_type ("i8", backend->integer_type (false, 8),
				 Linemap::predeclared_location ());
	return;

      case TyTy::IntType::I16:
	translated
	  = backend->named_type ("i16", backend->integer_type (false, 16),
				 Linemap::predeclared_location ());
	return;

      case TyTy::IntType::I32:
	translated
	  = backend->named_type ("i32", backend->integer_type (false, 32),
				 Linemap::predeclared_location ());
	return;

      case TyTy::IntType::I64:
	translated
	  = backend->named_type ("i64", backend->integer_type (false, 64),
				 Linemap::predeclared_location ());
	return;

      case TyTy::IntType::I128:
	translated
	  = backend->named_type ("i128", backend->integer_type (false, 128),
				 Linemap::predeclared_location ());
	return;
      }
    gcc_unreachable ();
  }

  void visit (TyTy::UintType &type) override
  {
    switch (type.get_uint_kind ())
      {
      case TyTy::UintType::U8:
	translated = backend->named_type ("u8", backend->integer_type (true, 8),
					  Linemap::predeclared_location ());
	return;

      case TyTy::UintType::U16:
	translated
	  = backend->named_type ("u16", backend->integer_type (true, 16),
				 Linemap::predeclared_location ());
	return;

      case TyTy::UintType::U32:
	translated
	  = backend->named_type ("u32", backend->integer_type (true, 32),
				 Linemap::predeclared_location ());
	return;

      case TyTy::UintType::U64:
	translated
	  = backend->named_type ("u64", backend->integer_type (true, 64),
				 Linemap::predeclared_location ());
	return;

      case TyTy::UintType::U128:
	translated
	  = backend->named_type ("u128", backend->integer_type (true, 128),
				 Linemap::predeclared_location ());
	return;
      }
    gcc_unreachable ();
  }

  void visit (TyTy::FloatType &type) override
  {
    switch (type.get_float_kind ())
      {
      case TyTy::FloatType::F32:
	translated = backend->named_type ("f32", backend->float_type (32),
					  Linemap::predeclared_location ());
	return;

      case TyTy::FloatType::F64:
	translated = backend->named_type ("f64", backend->float_type (64),
					  Linemap::predeclared_location ());
	return;
      }

    gcc_unreachable ();
  }

  void visit (TyTy::USizeType &) override
  {
    translated = backend->named_type (
      "usize", backend->integer_type (true, backend->get_pointer_size ()),
      Linemap::predeclared_location ());
  }

  void visit (TyTy::ISizeType &) override
  {
    translated = backend->named_type (
      "isize", backend->integer_type (false, backend->get_pointer_size ()),
      Linemap::predeclared_location ());
  }

  void visit (TyTy::CharType &) override
  {
    translated = backend->named_type ("char", backend->wchar_type (),
				      Linemap::predeclared_location ());
  }

  void visit (TyTy::StrType &) override
  {
    Btype *raw_str = backend->raw_str_type ();
    translated
      = backend->named_type ("str", raw_str, Linemap::predeclared_location ());
  }

  void visit (TyTy::NeverType &) override { gcc_unreachable (); }

private:
  ConstFoldType (::Backend *backend)
    : backend (backend), translated (backend->error_type ())
  {}

  ::Backend *backend;
  ::Btype *translated;
};

class ConstFoldItem : public ConstFoldBase
{
  using ConstFoldBase::visit;

public:
  static Bexpression *fold (HIR::Item &item)
  {
    ConstFoldItem folder;
    item.accept_vis (folder);
    if (folder.ctx->get_backend ()->is_error_expression (folder.folded))
      {
	rust_error_at (item.get_locus_slow (), "non const value");
	return nullptr;
      }

    folder.ctx->insert_const (item.get_mappings ().get_hirid (), folder.folded);
    return folder.folded;
  };

  void visit (HIR::ConstantItem &item) override;

private:
  ConstFoldItem ()
    : ConstFoldBase (), folded (ctx->get_backend ()->error_expression ())
  {}

  Bexpression *folded;
};

class ConstFoldExpr : public ConstFoldBase
{
  using ConstFoldBase::visit;

public:
  static Bexpression *fold (HIR::Expr *expr)
  {
    ConstFoldExpr folder;
    expr->accept_vis (folder);
    if (folder.ctx->get_backend ()->is_error_expression (folder.folded))
      {
	rust_error_at (expr->get_locus_slow (), "non const value");
	return nullptr;
      }

    folder.ctx->insert_const (expr->get_mappings ().get_hirid (),
			      folder.folded);
    return folder.folded;
  };

  void visit (HIR::IdentifierExpr &expr) override
  {
    NodeId ast_node_id = expr.get_mappings ().get_nodeid ();

    // then lookup the reference_node_id
    NodeId ref_node_id = UNKNOWN_NODEID;
    if (resolver->lookup_resolved_name (ast_node_id, &ref_node_id))
      {
	// these ref_node_ids will resolve to a pattern declaration but we are
	// interested in the definition that this refers to get the parent id
	Resolver::Definition def;
	if (!resolver->lookup_definition (ref_node_id, &def))
	  {
	    rust_error_at (expr.get_locus (),
			   "unknown reference for resolved name");
	    return;
	  }
	ref_node_id = def.parent;
      }

    if (ref_node_id == UNKNOWN_NODEID)
      {
	rust_error_at (expr.get_locus (), "unresolved node: %s",
		       expr.as_string ().c_str ());
	return;
      }

    // node back to HIR
    HirId ref;
    if (!mappings->lookup_node_to_hir (expr.get_mappings ().get_crate_num (),
				       ref_node_id, &ref))
      {
	rust_error_at (expr.get_locus (), "reverse lookup failure");
	return;
      }

    // lookup constant
    ctx->lookup_const (ref, &folded);
  }

  void visit (HIR::LiteralExpr &expr) override
  {
    auto literal_value = expr.get_literal ();
    switch (expr.get_lit_type ())
      {
	case HIR::Literal::INT: {
	  mpz_t ival;
	  if (mpz_init_set_str (ival, literal_value->as_string ().c_str (), 10)
	      != 0)
	    {
	      rust_fatal_error (expr.get_locus (), "bad number in literal");
	      return;
	    }

	  TyTy::BaseType *tyty = nullptr;
	  if (!tyctx->lookup_type (expr.get_mappings ().get_hirid (), &tyty))
	    {
	      rust_fatal_error (expr.get_locus (),
				"did not resolve type for this literal expr");
	      return;
	    }

	  Btype *type = ConstFoldType::fold (tyty, ctx->get_backend ());
	  folded
	    = ctx->get_backend ()->integer_constant_expression (type, ival);
	}
	return;

	/* handle other literals */

      default:
	gcc_unreachable ();
	return;
      }

    gcc_unreachable ();
  }

  void visit (HIR::ArithmeticOrLogicalExpr &expr) override
  {
    auto lhs = ConstFoldExpr::fold (expr.get_lhs ());
    if (lhs == nullptr)
      return;

    auto rhs = ConstFoldExpr::fold (expr.get_rhs ());
    if (rhs == nullptr)
      return;

    auto op = expr.get_expr_type ();
    auto location = expr.get_locus ();

    folded
      = ctx->get_backend ()->arithmetic_or_logical_expression (op, lhs, rhs,
							       location);
  }

  void visit (HIR::NegationExpr &expr) override
  {
    auto negated_expr = ConstFoldExpr::fold (expr.get_expr ().get ());
    if (negated_expr == nullptr)
      return;

    auto op = expr.get_expr_type ();
    auto location = expr.get_locus ();

    folded
      = ctx->get_backend ()->negation_expression (op, negated_expr, location);
  }

private:
  ConstFoldExpr ()
    : ConstFoldBase (), folded (ctx->get_backend ()->error_expression ())
  {}

  Bexpression *folded;
};

} // namespace ConstFold
} // namespace Rust

#endif // RUST_HIR_CONST_FOLD_H
