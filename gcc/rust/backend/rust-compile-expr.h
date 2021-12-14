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

#ifndef RUST_COMPILE_EXPR
#define RUST_COMPILE_EXPR

#include "rust-compile-base.h"
#include "rust-compile-tyty.h"
#include "rust-compile-resolve-path.h"
#include "rust-compile-block.h"
#include "rust-compile-struct-field-expr.h"

namespace Rust {
namespace Compile {

class CompileExpr : public HIRCompileBase
{
  using Rust::Compile::HIRCompileBase::visit;

public:
  static tree Compile (HIR::Expr *expr, Context *ctx)
  {
    CompileExpr compiler (ctx);
    expr->accept_vis (compiler);
    return compiler.translated;
  }

  void visit (HIR::TupleIndexExpr &expr) override
  {
    HIR::Expr *tuple_expr = expr.get_tuple_expr ().get ();
    TupleIndex index = expr.get_tuple_index ();

    tree receiver_ref = CompileExpr::Compile (tuple_expr, ctx);

    TyTy::BaseType *tuple_expr_ty = nullptr;
    bool ok = ctx->get_tyctx ()->lookup_type (
      tuple_expr->get_mappings ().get_hirid (), &tuple_expr_ty);
    rust_assert (ok);

    // do we need to add an indirect reference
    if (tuple_expr_ty->get_kind () == TyTy::TypeKind::REF)
      {
	TyTy::ReferenceType *r
	  = static_cast<TyTy::ReferenceType *> (tuple_expr_ty);
	TyTy::BaseType *tuple_type = r->get_base ();
	tree tuple_tyty = TyTyResolveCompile::compile (ctx, tuple_type);

	tree indirect
	  = ctx->get_backend ()->indirect_expression (tuple_tyty, receiver_ref,
						      true, expr.get_locus ());
	receiver_ref = indirect;
      }

    translated
      = ctx->get_backend ()->struct_field_expression (receiver_ref, index,
						      expr.get_locus ());
  }

  void visit (HIR::TupleExpr &expr) override
  {
    if (expr.is_unit ())
      {
	translated = ctx->get_backend ()->unit_expression ();
	return;
      }

    TyTy::BaseType *tyty = nullptr;
    if (!ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (),
					 &tyty))
      {
	rust_fatal_error (expr.get_locus (),
			  "did not resolve type for this TupleExpr");
	return;
      }

    tree tuple_type = TyTyResolveCompile::compile (ctx, tyty);
    rust_assert (tuple_type != nullptr);

    // this assumes all fields are in order from type resolution
    std::vector<tree> vals;
    for (auto &elem : expr.get_tuple_elems ())
      {
	auto e = CompileExpr::Compile (elem.get (), ctx);
	vals.push_back (e);
      }

    translated
      = ctx->get_backend ()->constructor_expression (tuple_type, false, vals,
						     -1, expr.get_locus ());
  }

  void visit (HIR::ReturnExpr &expr) override
  {
    auto fncontext = ctx->peek_fn ();

    std::vector<tree> retstmts;
    if (expr.has_return_expr ())
      {
	tree compiled_expr
	  = CompileExpr::Compile (expr.return_expr.get (), ctx);
	rust_assert (compiled_expr != nullptr);

	retstmts.push_back (compiled_expr);
      }

    auto s = ctx->get_backend ()->return_statement (fncontext.fndecl, retstmts,
						    expr.get_locus ());
    ctx->add_statement (s);
  }

  void visit (HIR::CallExpr &expr) override;

  void visit (HIR::MethodCallExpr &expr) override;

  void visit (HIR::IdentifierExpr &expr) override
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
	return;
      }
    else if (ctx->lookup_function_decl (ref, &fn))
      {
	translated
	  = ctx->get_backend ()->function_code_expression (fn,
							   expr.get_locus ());
      }
    else if (ctx->lookup_var_decl (ref, &var))
      {
	translated
	  = ctx->get_backend ()->var_expression (var, expr.get_locus ());
      }
    else
      {
	rust_fatal_error (expr.get_locus (),
			  "failed to lookup compiled reference");
      }
  }

  void visit (HIR::LiteralExpr &expr) override
  {
    auto literal_value = expr.get_literal ();
    switch (expr.get_lit_type ())
      {
	case HIR::Literal::BOOL: {
	  bool bval = literal_value->as_string ().compare ("true") == 0;
	  translated = ctx->get_backend ()->boolean_constant_expression (bval);
	}
	return;

	case HIR::Literal::INT: {
	  mpz_t ival;
	  if (mpz_init_set_str (ival, literal_value->as_string ().c_str (), 10)
	      != 0)
	    {
	      rust_fatal_error (expr.get_locus (), "bad number in literal");
	      return;
	    }

	  TyTy::BaseType *tyty = nullptr;
	  if (!ctx->get_tyctx ()->lookup_type (
		expr.get_mappings ().get_hirid (), &tyty))
	    {
	      rust_fatal_error (
		expr.get_locus (),
		"did not resolve type for this literal expr (HirId %d)",
		expr.get_mappings ().get_hirid ());
	      return;
	    }

	  tree type = TyTyResolveCompile::compile (ctx, tyty);
	  translated
	    = ctx->get_backend ()->integer_constant_expression (type, ival);
	}
	return;

	case HIR::Literal::FLOAT: {
	  mpfr_t fval;
	  if (mpfr_init_set_str (fval, literal_value->as_string ().c_str (), 10,
				 MPFR_RNDN)
	      != 0)
	    {
	      rust_fatal_error (expr.get_locus (),
				"bad floating-point number in literal");
	      return;
	    }

	  TyTy::BaseType *tyty = nullptr;
	  if (!ctx->get_tyctx ()->lookup_type (
		expr.get_mappings ().get_hirid (), &tyty))
	    {
	      rust_fatal_error (expr.get_locus (),
				"did not resolve type for this literal expr");
	      return;
	    }

	  tree type = TyTyResolveCompile::compile (ctx, tyty);
	  translated
	    = ctx->get_backend ()->float_constant_expression (type, fval);
	}
	return;

	case HIR::Literal::CHAR: {
	  // FIXME needs wchar_t
	  char c = literal_value->as_string ().c_str ()[0];
	  translated = ctx->get_backend ()->wchar_constant_expression (c);
	}
	return;

	case HIR::Literal::BYTE: {
	  char c = literal_value->as_string ().c_str ()[0];
	  translated = ctx->get_backend ()->char_constant_expression (c);
	}
	return;

	case HIR::Literal::STRING: {
	  auto base = ctx->get_backend ()->string_constant_expression (
	    literal_value->as_string ());
	  translated
	    = ctx->get_backend ()->address_expression (base, expr.get_locus ());
	}
	return;

	case HIR::Literal::BYTE_STRING: {
	  TyTy::BaseType *tyty = nullptr;
	  if (!ctx->get_tyctx ()->lookup_type (
		expr.get_mappings ().get_hirid (), &tyty))
	    {
	      rust_error_at (expr.get_locus (),
			     "did not resolve type for this byte string");
	      return;
	    }

	  // the type here is &[ty; capacity]
	  rust_assert (tyty->get_kind () == TyTy::TypeKind::REF);
	  auto ref_tyty = static_cast<TyTy::ReferenceType *> (tyty);
	  auto base_tyty = ref_tyty->get_base ();
	  rust_assert (base_tyty->get_kind () == TyTy::TypeKind::ARRAY);
	  auto array_tyty = static_cast<TyTy::ArrayType *> (base_tyty);

	  std::string value_str = expr.get_literal ()->as_string ();
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
	  tree constructed = ctx->get_backend ()->array_constructor_expression (
	    array_type, indexes, vals, expr.get_locus ());

	  translated
	    = ctx->get_backend ()->address_expression (constructed,
						       expr.get_locus ());
	}
	return;

      default:
	rust_fatal_error (expr.get_locus (), "unknown literal");
	return;
      }

    gcc_unreachable ();
  }

  void visit (HIR::AssignmentExpr &expr) override
  {
    fncontext fn = ctx->peek_fn ();
    auto lvalue = CompileExpr::Compile (expr.get_lhs (), ctx);
    auto rvalue = CompileExpr::Compile (expr.get_rhs (), ctx);

    // assignments are coercion sites so lets convert the rvalue if necessary
    TyTy::BaseType *expected = nullptr;
    TyTy::BaseType *actual = nullptr;

    bool ok;
    ok = ctx->get_tyctx ()->lookup_type (
      expr.get_lhs ()->get_mappings ().get_hirid (), &expected);
    rust_assert (ok);

    ok = ctx->get_tyctx ()->lookup_type (
      expr.get_rhs ()->get_mappings ().get_hirid (), &actual);
    rust_assert (ok);

    rvalue = coercion_site (rvalue, actual, expected, expr.get_locus ());

    tree assignment
      = ctx->get_backend ()->assignment_statement (fn.fndecl, lvalue, rvalue,
						   expr.get_locus ());

    ctx->add_statement (assignment);
  }

  void visit (HIR::CompoundAssignmentExpr &expr) override;

  void visit (HIR::ArrayIndexExpr &expr) override
  {
    tree array_reference = CompileExpr::Compile (expr.get_array_expr (), ctx);
    tree index = CompileExpr::Compile (expr.get_index_expr (), ctx);

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
	  = ctx->get_backend ()->indirect_expression (array_tyty,
						      array_reference, true,
						      expr.get_locus ());
      }

    translated
      = ctx->get_backend ()->array_index_expression (array_reference, index,
						     expr.get_locus ());
  }

  void visit (HIR::ArrayExpr &expr) override
  {
    TyTy::BaseType *tyty = nullptr;
    if (!ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (),
					 &tyty))
      {
	rust_fatal_error (expr.get_locus (),
			  "did not resolve type for this array expr");
	return;
      }

    rust_assert (tyty->get_kind () == TyTy::TypeKind::ARRAY);
    TyTy::ArrayType *array_tyty = static_cast<TyTy::ArrayType *> (tyty);
    capacity_expr = array_tyty->get_capacity ();

    tree array_type = TyTyResolveCompile::compile (ctx, array_tyty);
    rust_assert (array_type != nullptr);

    expr.get_internal_elements ()->accept_vis (*this);
    std::vector<unsigned long> indexes;
    for (size_t i = 0; i < constructor.size (); i++)
      indexes.push_back (i);

    translated
      = ctx->get_backend ()->array_constructor_expression (array_type, indexes,
							   constructor,
							   expr.get_locus ());
  }

  void visit (HIR::ArrayElemsValues &elems) override
  {
    for (auto &elem : elems.get_values ())
      {
	tree translated_expr = CompileExpr::Compile (elem.get (), ctx);
	constructor.push_back (translated_expr);
      }
  }

  void visit (HIR::ArrayElemsCopied &elems) override
  {
    tree translated_expr
      = CompileExpr::Compile (elems.get_elem_to_copy (), ctx);

    size_t capacity;
    bool ok = ctx->get_backend ()->const_size_cast (capacity_expr, &capacity);
    rust_assert (ok);

    for (size_t i = 0; i < capacity; ++i)
      constructor.push_back (translated_expr);
  }

  void visit (HIR::ArithmeticOrLogicalExpr &expr) override;

  void visit (HIR::ComparisonExpr &expr) override
  {
    auto op = expr.get_expr_type ();
    auto lhs = CompileExpr::Compile (expr.get_lhs (), ctx);
    auto rhs = CompileExpr::Compile (expr.get_rhs (), ctx);
    auto location = expr.get_locus ();

    translated
      = ctx->get_backend ()->comparison_expression (op, lhs, rhs, location);
  }

  void visit (HIR::LazyBooleanExpr &expr) override
  {
    auto op = expr.get_expr_type ();
    auto lhs = CompileExpr::Compile (expr.get_lhs (), ctx);
    auto rhs = CompileExpr::Compile (expr.get_rhs (), ctx);
    auto location = expr.get_locus ();

    translated
      = ctx->get_backend ()->lazy_boolean_expression (op, lhs, rhs, location);
  }

  void visit (HIR::NegationExpr &expr) override;

  void visit (HIR::TypeCastExpr &expr) override
  {
    TyTy::BaseType *tyty_to_cast_to = nullptr;
    if (!ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (),
					 &tyty_to_cast_to))
      {
	translated = ctx->get_backend ()->error_expression ();
	return;
      }

    auto type_to_cast_to = TyTyResolveCompile::compile (ctx, tyty_to_cast_to);
    auto casted_expr
      = CompileExpr::Compile (expr.get_casted_expr ().get (), ctx);
    translated
      = ctx->get_backend ()->convert_expression (type_to_cast_to, casted_expr,
						 expr.get_locus ());
  }

  void visit (HIR::IfExpr &expr) override
  {
    auto stmt = CompileConditionalBlocks::compile (&expr, ctx, nullptr);
    ctx->add_statement (stmt);
  }

  void visit (HIR::IfExprConseqElse &expr) override
  {
    TyTy::BaseType *if_type = nullptr;
    if (!ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (),
					 &if_type))
      {
	rust_error_at (expr.get_locus (),
		       "failed to lookup type of IfExprConseqElse");
	return;
      }

    Bvariable *tmp = NULL;
    bool needs_temp = !if_type->is_unit ();
    if (needs_temp)
      {
	fncontext fnctx = ctx->peek_fn ();
	tree enclosing_scope = ctx->peek_enclosing_scope ();
	tree block_type = TyTyResolveCompile::compile (ctx, if_type);

	bool is_address_taken = false;
	tree ret_var_stmt = nullptr;
	tmp = ctx->get_backend ()->temporary_variable (
	  fnctx.fndecl, enclosing_scope, block_type, NULL, is_address_taken,
	  expr.get_locus (), &ret_var_stmt);
	ctx->add_statement (ret_var_stmt);
      }

    auto stmt = CompileConditionalBlocks::compile (&expr, ctx, tmp);
    ctx->add_statement (stmt);

    if (tmp != NULL)
      {
	translated
	  = ctx->get_backend ()->var_expression (tmp, expr.get_locus ());
      }
  }

  void visit (HIR::IfExprConseqIf &expr) override
  {
    TyTy::BaseType *if_type = nullptr;
    if (!ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (),
					 &if_type))
      {
	rust_error_at (expr.get_locus (),
		       "failed to lookup type of IfExprConseqElse");
	return;
      }

    Bvariable *tmp = NULL;
    bool needs_temp = !if_type->is_unit ();
    if (needs_temp)
      {
	fncontext fnctx = ctx->peek_fn ();
	tree enclosing_scope = ctx->peek_enclosing_scope ();
	tree block_type = TyTyResolveCompile::compile (ctx, if_type);

	bool is_address_taken = false;
	tree ret_var_stmt = nullptr;
	tmp = ctx->get_backend ()->temporary_variable (
	  fnctx.fndecl, enclosing_scope, block_type, NULL, is_address_taken,
	  expr.get_locus (), &ret_var_stmt);
	ctx->add_statement (ret_var_stmt);
      }

    auto stmt = CompileConditionalBlocks::compile (&expr, ctx, tmp);
    ctx->add_statement (stmt);

    if (tmp != NULL)
      {
	translated
	  = ctx->get_backend ()->var_expression (tmp, expr.get_locus ());
      }
  }

  void visit (HIR::BlockExpr &expr) override
  {
    TyTy::BaseType *block_tyty = nullptr;
    if (!ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (),
					 &block_tyty))
      {
	rust_error_at (expr.get_locus (), "failed to lookup type of BlockExpr");
	return;
      }

    Bvariable *tmp = NULL;
    bool needs_temp = !block_tyty->is_unit ();
    if (needs_temp)
      {
	fncontext fnctx = ctx->peek_fn ();
	tree enclosing_scope = ctx->peek_enclosing_scope ();
	tree block_type = TyTyResolveCompile::compile (ctx, block_tyty);

	bool is_address_taken = false;
	tree ret_var_stmt = nullptr;
	tmp = ctx->get_backend ()->temporary_variable (
	  fnctx.fndecl, enclosing_scope, block_type, NULL, is_address_taken,
	  expr.get_locus (), &ret_var_stmt);
	ctx->add_statement (ret_var_stmt);
      }

    auto code_block = CompileBlock::compile (&expr, ctx, tmp);
    auto block_stmt = ctx->get_backend ()->block_statement (code_block);
    ctx->add_statement (block_stmt);

    if (tmp != NULL)
      {
	translated
	  = ctx->get_backend ()->var_expression (tmp, expr.get_locus ());
      }
  }

  void visit (HIR::UnsafeBlockExpr &expr) override
  {
    expr.get_block_expr ()->accept_vis (*this);
  }

  void visit (HIR::StructExprStruct &struct_expr) override
  {
    TyTy::BaseType *tyty = nullptr;
    if (!ctx->get_tyctx ()->lookup_type (
	  struct_expr.get_mappings ().get_hirid (), &tyty))
      {
	rust_error_at (struct_expr.get_locus (), "unknown type");
	return;
      }

    rust_assert (tyty->is_unit ());
    translated = ctx->get_backend ()->unit_expression ();
  }

  void visit (HIR::StructExprStructFields &struct_expr) override
  {
    TyTy::BaseType *tyty = nullptr;
    if (!ctx->get_tyctx ()->lookup_type (
	  struct_expr.get_mappings ().get_hirid (), &tyty))
      {
	rust_error_at (struct_expr.get_locus (), "unknown type");
	return;
      }

    // it must be an ADT
    rust_assert (tyty->get_kind () == TyTy::TypeKind::ADT);
    TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (tyty);

    // what variant is it?
    int union_disriminator = struct_expr.union_index;
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
	  struct_expr.get_struct_name ().get_mappings ().get_hirid (),
	  &variant_id);
	rust_assert (ok);

	ok = adt->lookup_variant_by_id (variant_id, &variant,
					&union_disriminator);
	rust_assert (ok);
      }

    // compile it
    tree compiled_adt_type = TyTyResolveCompile::compile (ctx, tyty);

    // this assumes all fields are in order from type resolution and if a base
    // struct was specified those fields are filed via accesors
    std::vector<tree> arguments;
    for (size_t i = 0; i < struct_expr.get_fields ().size (); i++)
      {
	auto &argument = struct_expr.get_fields ().at (i);
	auto rvalue = CompileStructExprField::Compile (argument.get (), ctx);

	// assignments are coercion sites so lets convert the rvalue if
	// necessary
	auto respective_field = variant->get_field_at_index (i);
	auto expected = respective_field->get_field_type ();

	TyTy::BaseType *actual = nullptr;
	bool ok = ctx->get_tyctx ()->lookup_type (
	  argument->get_mappings ().get_hirid (), &actual);

	// coerce it if required/possible see
	// compile/torture/struct_base_init_1.rs
	if (ok)
	  {
	    rvalue = coercion_site (rvalue, actual, expected,
				    argument->get_locus ());
	  }

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
      struct_expr.get_locus ());
  }

  void visit (HIR::GroupedExpr &expr) override
  {
    translated = CompileExpr::Compile (expr.get_expr_in_parens ().get (), ctx);
  }

  void visit (HIR::FieldAccessExpr &expr) override
  {
    tree receiver_ref
      = CompileExpr::Compile (expr.get_receiver_expr ().get (), ctx);

    // resolve the receiver back to ADT type
    TyTy::BaseType *receiver = nullptr;
    if (!ctx->get_tyctx ()->lookup_type (
	  expr.get_receiver_expr ()->get_mappings ().get_hirid (), &receiver))
      {
	rust_error_at (expr.get_receiver_expr ()->get_locus (),
		       "unresolved type for receiver");
	return;
      }

    size_t field_index = 0;
    if (receiver->get_kind () == TyTy::TypeKind::ADT)
      {
	TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (receiver);
	rust_assert (!adt->is_enum ());
	rust_assert (adt->number_of_variants () == 1);

	TyTy::VariantDef *variant = adt->get_variants ().at (0);
	bool ok = variant->lookup_field (expr.get_field_name (), nullptr,
					 &field_index);
	rust_assert (ok);
      }
    else if (receiver->get_kind () == TyTy::TypeKind::REF)
      {
	TyTy::ReferenceType *r = static_cast<TyTy::ReferenceType *> (receiver);
	TyTy::BaseType *b = r->get_base ();
	rust_assert (b->get_kind () == TyTy::TypeKind::ADT);

	TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (b);
	rust_assert (!adt->is_enum ());
	rust_assert (adt->number_of_variants () == 1);

	TyTy::VariantDef *variant = adt->get_variants ().at (0);
	bool ok = variant->lookup_field (expr.get_field_name (), nullptr,
					 &field_index);
	rust_assert (ok);

	tree adt_tyty = TyTyResolveCompile::compile (ctx, adt);
	tree indirect
	  = ctx->get_backend ()->indirect_expression (adt_tyty, receiver_ref,
						      true, expr.get_locus ());
	receiver_ref = indirect;
      }

    translated
      = ctx->get_backend ()->struct_field_expression (receiver_ref, field_index,
						      expr.get_locus ());
  }

  void visit (HIR::QualifiedPathInExpression &expr) override
  {
    translated = ResolvePathRef::Compile (expr, ctx);
  }

  void visit (HIR::PathInExpression &expr) override
  {
    translated = ResolvePathRef::Compile (expr, ctx);
  }

  void visit (HIR::LoopExpr &expr) override
  {
    TyTy::BaseType *block_tyty = nullptr;
    if (!ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (),
					 &block_tyty))
      {
	rust_error_at (expr.get_locus (), "failed to lookup type of BlockExpr");
	return;
      }

    fncontext fnctx = ctx->peek_fn ();
    Bvariable *tmp = NULL;
    bool needs_temp = !block_tyty->is_unit ();
    if (needs_temp)
      {
	tree enclosing_scope = ctx->peek_enclosing_scope ();
	tree block_type = TyTyResolveCompile::compile (ctx, block_tyty);

	bool is_address_taken = false;
	tree ret_var_stmt = NULL_TREE;
	tmp = ctx->get_backend ()->temporary_variable (
	  fnctx.fndecl, enclosing_scope, block_type, NULL, is_address_taken,
	  expr.get_locus (), &ret_var_stmt);
	ctx->add_statement (ret_var_stmt);
	ctx->push_loop_context (tmp);
      }

    if (expr.has_loop_label ())
      {
	HIR::LoopLabel &loop_label = expr.get_loop_label ();
	tree label
	  = ctx->get_backend ()->label (fnctx.fndecl,
					loop_label.get_lifetime ().get_name (),
					loop_label.get_locus ());
	tree label_decl
	  = ctx->get_backend ()->label_definition_statement (label);
	ctx->add_statement (label_decl);
	ctx->insert_label_decl (
	  loop_label.get_lifetime ().get_mappings ().get_hirid (), label);
      }

    tree loop_begin_label
      = ctx->get_backend ()->label (fnctx.fndecl, "", expr.get_locus ());
    tree loop_begin_label_decl
      = ctx->get_backend ()->label_definition_statement (loop_begin_label);
    ctx->add_statement (loop_begin_label_decl);
    ctx->push_loop_begin_label (loop_begin_label);

    tree code_block
      = CompileBlock::compile (expr.get_loop_block ().get (), ctx, nullptr);
    tree loop_expr
      = ctx->get_backend ()->loop_expression (code_block, expr.get_locus ());
    tree loop_stmt
      = ctx->get_backend ()->expression_statement (fnctx.fndecl, loop_expr);
    ctx->add_statement (loop_stmt);

    if (tmp != NULL)
      {
	ctx->pop_loop_context ();
	translated
	  = ctx->get_backend ()->var_expression (tmp, expr.get_locus ());
      }
    ctx->pop_loop_begin_label ();
  }

  void visit (HIR::WhileLoopExpr &expr) override
  {
    fncontext fnctx = ctx->peek_fn ();
    if (expr.has_loop_label ())
      {
	HIR::LoopLabel &loop_label = expr.get_loop_label ();
	tree label
	  = ctx->get_backend ()->label (fnctx.fndecl,
					loop_label.get_lifetime ().get_name (),
					loop_label.get_locus ());
	tree label_decl
	  = ctx->get_backend ()->label_definition_statement (label);
	ctx->add_statement (label_decl);
	ctx->insert_label_decl (
	  loop_label.get_lifetime ().get_mappings ().get_hirid (), label);
      }

    std::vector<Bvariable *> locals;
    Location start_location = expr.get_loop_block ()->get_locus ();
    Location end_location = expr.get_loop_block ()->get_locus (); // FIXME

    tree enclosing_scope = ctx->peek_enclosing_scope ();
    tree loop_block
      = ctx->get_backend ()->block (fnctx.fndecl, enclosing_scope, locals,
				    start_location, end_location);
    ctx->push_block (loop_block);

    tree loop_begin_label
      = ctx->get_backend ()->label (fnctx.fndecl, "", expr.get_locus ());
    tree loop_begin_label_decl
      = ctx->get_backend ()->label_definition_statement (loop_begin_label);
    ctx->add_statement (loop_begin_label_decl);
    ctx->push_loop_begin_label (loop_begin_label);

    tree condition
      = CompileExpr::Compile (expr.get_predicate_expr ().get (), ctx);
    tree exit_expr
      = ctx->get_backend ()->exit_expression (condition, expr.get_locus ());
    tree break_stmt
      = ctx->get_backend ()->expression_statement (fnctx.fndecl, exit_expr);
    ctx->add_statement (break_stmt);

    tree code_block
      = CompileBlock::compile (expr.get_loop_block ().get (), ctx, nullptr);
    tree code_block_stmt = ctx->get_backend ()->block_statement (code_block);
    ctx->add_statement (code_block_stmt);

    ctx->pop_loop_begin_label ();
    ctx->pop_block ();

    tree loop_expr
      = ctx->get_backend ()->loop_expression (loop_block, expr.get_locus ());
    tree loop_stmt
      = ctx->get_backend ()->expression_statement (fnctx.fndecl, loop_expr);
    ctx->add_statement (loop_stmt);
  }

  void visit (HIR::BreakExpr &expr) override
  {
    fncontext fnctx = ctx->peek_fn ();
    if (expr.has_break_expr ())
      {
	tree compiled_expr
	  = CompileExpr::Compile (expr.get_expr ().get (), ctx);

	Bvariable *loop_result_holder = ctx->peek_loop_context ();
	tree result_reference = ctx->get_backend ()->var_expression (
	  loop_result_holder, expr.get_expr ()->get_locus ());

	tree assignment = ctx->get_backend ()->assignment_statement (
	  fnctx.fndecl, result_reference, compiled_expr, expr.get_locus ());
	ctx->add_statement (assignment);
      }

    if (expr.has_label ())
      {
	NodeId resolved_node_id = UNKNOWN_NODEID;
	if (!ctx->get_resolver ()->lookup_resolved_label (
	      expr.get_label ().get_mappings ().get_nodeid (),
	      &resolved_node_id))
	  {
	    rust_error_at (
	      expr.get_label ().get_locus (),
	      "failed to resolve compiled label for label %s",
	      expr.get_label ().get_mappings ().as_string ().c_str ());
	    return;
	  }

	HirId ref = UNKNOWN_HIRID;
	if (!ctx->get_mappings ()->lookup_node_to_hir (
	      expr.get_mappings ().get_crate_num (), resolved_node_id, &ref))
	  {
	    rust_fatal_error (expr.get_locus (),
			      "reverse lookup label failure");
	    return;
	  }

	tree label = NULL_TREE;
	if (!ctx->lookup_label_decl (ref, &label))
	  {
	    rust_error_at (expr.get_label ().get_locus (),
			   "failed to lookup compiled label");
	    return;
	  }

	tree goto_label
	  = ctx->get_backend ()->goto_statement (label, expr.get_locus ());
	ctx->add_statement (goto_label);
      }
    else
      {
	tree exit_expr = ctx->get_backend ()->exit_expression (
	  ctx->get_backend ()->boolean_constant_expression (true),
	  expr.get_locus ());
	tree break_stmt
	  = ctx->get_backend ()->expression_statement (fnctx.fndecl, exit_expr);
	ctx->add_statement (break_stmt);
      }
  }

  void visit (HIR::ContinueExpr &expr) override
  {
    tree label = ctx->peek_loop_begin_label ();
    if (expr.has_label ())
      {
	NodeId resolved_node_id = UNKNOWN_NODEID;
	if (!ctx->get_resolver ()->lookup_resolved_label (
	      expr.get_label ().get_mappings ().get_nodeid (),
	      &resolved_node_id))
	  {
	    rust_error_at (
	      expr.get_label ().get_locus (),
	      "failed to resolve compiled label for label %s",
	      expr.get_label ().get_mappings ().as_string ().c_str ());
	    return;
	  }

	HirId ref = UNKNOWN_HIRID;
	if (!ctx->get_mappings ()->lookup_node_to_hir (
	      expr.get_mappings ().get_crate_num (), resolved_node_id, &ref))
	  {
	    rust_fatal_error (expr.get_locus (),
			      "reverse lookup label failure");
	    return;
	  }

	if (!ctx->lookup_label_decl (ref, &label))
	  {
	    rust_error_at (expr.get_label ().get_locus (),
			   "failed to lookup compiled label");
	    return;
	  }
      }

    tree goto_label
      = ctx->get_backend ()->goto_statement (label, expr.get_locus ());
    ctx->add_statement (goto_label);
  }

  void visit (HIR::BorrowExpr &expr) override
  {
    tree main_expr = CompileExpr::Compile (expr.get_expr ().get (), ctx);

    translated
      = ctx->get_backend ()->address_expression (main_expr, expr.get_locus ());
  }

  void visit (HIR::DereferenceExpr &expr) override;

protected:
  tree compile_dyn_dispatch_call (const TyTy::DynamicObjectType *dyn,
				  TyTy::BaseType *receiver,
				  TyTy::FnType *fntype, tree receiver_ref,
				  std::vector<HIR::Expr *> &arguments,
				  Location expr_locus);

  tree resolve_method_address (TyTy::FnType *fntype, HirId ref,
			       TyTy::BaseType *receiver,
			       HIR::PathIdentSegment &segment,
			       Analysis::NodeMapping expr_mappings,
			       Location expr_locus);

  tree
  resolve_operator_overload (Analysis::RustLangItem::ItemType lang_item_type,
			     HIR::OperatorExpr &expr, tree lhs, tree rhs,
			     HIR::Expr *lhs_expr, HIR::Expr *rhs_expr);

private:
  CompileExpr (Context *ctx)
    : HIRCompileBase (ctx), translated (nullptr), capacity_expr (nullptr)
  {}

  tree translated;
  tree capacity_expr;
  std::vector<tree> constructor;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_EXPR
