// Copyright (C) 2020-2025 Free Software Foundation, Inc.

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

#include "rust-hir-type-check-base.h"
#include "rust-hir-type-check-expr.h"
#include "rust-hir-type-check-type.h"
#include "rust-hir-trait-resolve.h"
#include "rust-type-util.h"
#include "rust-attribute-values.h"

namespace Rust {
namespace Resolver {

TypeCheckBase::TypeCheckBase ()
  : mappings (Analysis::Mappings::get ()), resolver (Resolver::get ()),
    context (TypeCheckContext::get ())
{}

void
TypeCheckBase::ResolveGenericParams (
  const std::vector<std::unique_ptr<HIR::GenericParam>> &generic_params,
  std::vector<TyTy::SubstitutionParamMapping> &substitutions, bool is_foreign,
  ABI abi)
{
  TypeCheckBase ctx;
  ctx.resolve_generic_params (generic_params, substitutions, is_foreign, abi);
}

static void
walk_types_to_constrain (std::set<HirId> &constrained_symbols,
			 const TyTy::SubstitutionArgumentMappings &constraints)
{
  for (const auto &c : constraints.get_mappings ())
    {
      const TyTy::BaseType *arg = c.get_tyty ();
      if (arg != nullptr)
	{
	  const TyTy::BaseType *p = arg->get_root ();
	  constrained_symbols.insert (p->get_ty_ref ());
	  if (p->has_substitutions_defined ())
	    {
	      walk_types_to_constrain (constrained_symbols,
				       p->get_subst_argument_mappings ());
	    }
	}
    }
}

bool
TypeCheckBase::check_for_unconstrained (
  const std::vector<TyTy::SubstitutionParamMapping> &params_to_constrain,
  const TyTy::SubstitutionArgumentMappings &constraint_a,
  const TyTy::SubstitutionArgumentMappings &constraint_b,
  const TyTy::BaseType *reference)
{
  bool check_result = false;
  bool check_completed
    = context->have_checked_for_unconstrained (reference->get_ref (),
					       &check_result);
  if (check_completed)
    return check_result;

  std::set<HirId> symbols_to_constrain;
  std::map<HirId, location_t> symbol_to_location;
  for (const auto &p : params_to_constrain)
    {
      HirId ref = p.get_param_ty ()->get_ref ();
      symbols_to_constrain.insert (ref);
      symbol_to_location.insert ({ref, p.get_param_locus ()});

      rust_debug_loc (p.get_param_locus (), "XX constrain THIS");
    }

  // set up the set of constrained symbols
  std::set<HirId> constrained_symbols;
  walk_types_to_constrain (constrained_symbols, constraint_a);
  walk_types_to_constrain (constrained_symbols, constraint_b);

  const auto root = reference->get_root ();
  if (root->get_kind () == TyTy::TypeKind::PARAM)
    {
      const TyTy::ParamType *p = static_cast<const TyTy::ParamType *> (root);
      constrained_symbols.insert (p->get_ty_ref ());
    }

  // check for unconstrained
  bool unconstrained = false;
  for (auto &sym : symbols_to_constrain)
    {
      bool used = constrained_symbols.find (sym) != constrained_symbols.end ();
      if (!used)
	{
	  location_t locus = symbol_to_location.at (sym);
	  rust_error_at (locus, "unconstrained type parameter");
	  unconstrained = true;
	}
    }

  context->insert_unconstrained_check_marker (reference->get_ref (),
					      unconstrained);

  return unconstrained;
}

TyTy::BaseType *
TypeCheckBase::resolve_literal (const Analysis::NodeMapping &expr_mappings,
				HIR::Literal &literal, location_t locus)
{
  TyTy::BaseType *infered = nullptr;
  switch (literal.get_lit_type ())
    {
      case HIR::Literal::LitType::INT: {
	bool ok = false;

	switch (literal.get_type_hint ())
	  {
	  case CORETYPE_I8:
	    ok = context->lookup_builtin ("i8", &infered);
	    break;
	  case CORETYPE_I16:
	    ok = context->lookup_builtin ("i16", &infered);
	    break;
	  case CORETYPE_I32:
	    ok = context->lookup_builtin ("i32", &infered);
	    break;
	  case CORETYPE_I64:
	    ok = context->lookup_builtin ("i64", &infered);
	    break;
	  case CORETYPE_I128:
	    ok = context->lookup_builtin ("i128", &infered);
	    break;

	  case CORETYPE_U8:
	    ok = context->lookup_builtin ("u8", &infered);
	    break;
	  case CORETYPE_U16:
	    ok = context->lookup_builtin ("u16", &infered);
	    break;
	  case CORETYPE_U32:
	    ok = context->lookup_builtin ("u32", &infered);
	    break;
	  case CORETYPE_U64:
	    ok = context->lookup_builtin ("u64", &infered);
	    break;
	  case CORETYPE_U128:
	    ok = context->lookup_builtin ("u128", &infered);
	    break;

	  case CORETYPE_F32:
	    literal.set_lit_type (HIR::Literal::LitType::FLOAT);
	    ok = context->lookup_builtin ("f32", &infered);
	    break;
	  case CORETYPE_F64:
	    literal.set_lit_type (HIR::Literal::LitType::FLOAT);
	    ok = context->lookup_builtin ("f64", &infered);
	    break;

	  case CORETYPE_ISIZE:
	    ok = context->lookup_builtin ("isize", &infered);
	    break;

	  case CORETYPE_USIZE:
	    ok = context->lookup_builtin ("usize", &infered);
	    break;

	  default:
	    ok = true;
	    infered
	      = new TyTy::InferType (expr_mappings.get_hirid (),
				     TyTy::InferType::InferTypeKind::INTEGRAL,
				     TyTy::InferType::TypeHint::Default (),
				     locus);
	    break;
	  }
	rust_assert (ok);
      }
      break;

      case HIR::Literal::LitType::FLOAT: {
	bool ok = false;

	switch (literal.get_type_hint ())
	  {
	  case CORETYPE_F32:
	    ok = context->lookup_builtin ("f32", &infered);
	    break;
	  case CORETYPE_F64:
	    ok = context->lookup_builtin ("f64", &infered);
	    break;

	  default:
	    ok = true;
	    infered
	      = new TyTy::InferType (expr_mappings.get_hirid (),
				     TyTy::InferType::InferTypeKind::FLOAT,
				     TyTy::InferType::TypeHint::Default (),
				     locus);
	    break;
	  }
	rust_assert (ok);
      }
      break;

      case HIR::Literal::LitType::BOOL: {
	auto ok = context->lookup_builtin ("bool", &infered);
	rust_assert (ok);
      }
      break;

      case HIR::Literal::LitType::CHAR: {
	auto ok = context->lookup_builtin ("char", &infered);
	rust_assert (ok);
      }
      break;

      case HIR::Literal::LitType::BYTE: {
	auto ok = context->lookup_builtin ("u8", &infered);
	rust_assert (ok);
      }
      break;

      case HIR::Literal::LitType::STRING: {
	TyTy::BaseType *base = nullptr;
	auto ok = context->lookup_builtin ("str", &base);
	rust_assert (ok);

	infered = new TyTy::ReferenceType (expr_mappings.get_hirid (),
					   TyTy::TyVar (base->get_ref ()),
					   Mutability::Imm,
					   TyTy::Region::make_static ());
      }
      break;

      case HIR::Literal::LitType::BYTE_STRING: {
	/* This is an arraytype of u8 reference (&[u8;size]). It isn't in
	   UTF-8, but really just a byte array. Code to construct the array
	   reference copied from ArrayElemsValues and ArrayType. */
	TyTy::BaseType *u8;
	auto ok = context->lookup_builtin ("u8", &u8);
	rust_assert (ok);

	auto crate_num = mappings.get_current_crate ();
	Analysis::NodeMapping capacity_mapping (crate_num, UNKNOWN_NODEID,
						mappings.get_next_hir_id (
						  crate_num),
						UNKNOWN_LOCAL_DEFID);

	/* Capacity is the size of the string (number of chars).
	   It is a constant, but for fold it to get a tree.  */
	std::string capacity_str
	  = std::to_string (literal.as_string ().size ());
	HIR::LiteralExpr *literal_capacity
	  = new HIR::LiteralExpr (capacity_mapping, capacity_str,
				  HIR::Literal::LitType::INT,
				  PrimitiveCoreType::CORETYPE_USIZE, locus, {});

	// mark the type for this implicit node
	TyTy::BaseType *expected_ty = nullptr;
	ok = context->lookup_builtin ("usize", &expected_ty);
	rust_assert (ok);
	context->insert_type (capacity_mapping, expected_ty);

	Analysis::NodeMapping array_mapping (crate_num, UNKNOWN_NODEID,
					     mappings.get_next_hir_id (
					       crate_num),
					     UNKNOWN_LOCAL_DEFID);

	TyTy::ArrayType *array
	  = new TyTy::ArrayType (array_mapping.get_hirid (), locus,
				 *literal_capacity,
				 TyTy::TyVar (u8->get_ref ()));
	context->insert_type (array_mapping, array);

	infered = new TyTy::ReferenceType (expr_mappings.get_hirid (),
					   TyTy::TyVar (array->get_ref ()),
					   Mutability::Imm,
					   TyTy::Region::make_static ());
      }
      break;

    default:
      rust_unreachable ();
      break;
    }

  return infered;
}

TyTy::ADTType::ReprOptions
TypeCheckBase::parse_repr_options (const AST::AttrVec &attrs, location_t locus)
{
  TyTy::ADTType::ReprOptions repr;
  repr.pack = 0;
  repr.align = 0;

  // Default repr for enums is isize, but we now check for other repr in the
  // attributes.
  bool ok = context->lookup_builtin ("isize", &repr.repr);
  rust_assert (ok);

  for (const auto &attr : attrs)
    {
      bool is_repr = attr.get_path ().as_string () == Values::Attributes::REPR;
      if (is_repr && !attr.has_attr_input ())
	{
	  rust_error_at (attr.get_locus (), "malformed %qs attribute", "repr");
	  continue;
	}

      if (is_repr)
	{
	  const AST::AttrInput &input = attr.get_attr_input ();
	  bool is_token_tree = input.get_attr_input_type ()
			       == AST::AttrInput::AttrInputType::TOKEN_TREE;
	  rust_assert (is_token_tree);
	  const auto &option = static_cast<const AST::DelimTokenTree &> (input);
	  AST::AttrInputMetaItemContainer *meta_items
	    = option.parse_to_meta_item ();

	  if (meta_items == nullptr)
	    {
	      rust_error_at (attr.get_locus (), "malformed %qs attribute",
			     "repr");
	      continue;
	    }

	  auto &items = meta_items->get_items ();
	  if (items.size () == 0)
	    {
	      // nothing to do with this its empty
	      delete meta_items;
	      continue;
	    }

	  const std::string inline_option = items.at (0)->as_string ();

	  // TODO: it would probably be better to make the MetaItems more aware
	  // of constructs with nesting like #[repr(packed(2))] rather than
	  // manually parsing the string "packed(2)" here.

	  size_t oparen = inline_option.find ('(', 0);
	  bool is_pack = false;
	  bool is_align = false;
	  bool is_c = false;
	  bool is_integer = false;
	  unsigned char value = 1;

	  if (oparen == std::string::npos)
	    {
	      is_pack = inline_option.compare ("packed") == 0;
	      is_align = inline_option.compare ("align") == 0;
	      is_c = inline_option.compare ("C") == 0;
	      is_integer = (inline_option.compare ("isize") == 0
			    || inline_option.compare ("i8") == 0
			    || inline_option.compare ("i16") == 0
			    || inline_option.compare ("i32") == 0
			    || inline_option.compare ("i64") == 0
			    || inline_option.compare ("i128") == 0
			    || inline_option.compare ("usize") == 0
			    || inline_option.compare ("u8") == 0
			    || inline_option.compare ("u16") == 0
			    || inline_option.compare ("u32") == 0
			    || inline_option.compare ("u64") == 0
			    || inline_option.compare ("u128") == 0);
	    }

	  else
	    {
	      std::string rep = inline_option.substr (0, oparen);
	      is_pack = rep.compare ("packed") == 0;
	      is_align = rep.compare ("align") == 0;

	      size_t cparen = inline_option.find (')', oparen);
	      if (cparen == std::string::npos)
		{
		  rust_error_at (locus, "malformed attribute");
		}

	      std::string value_str = inline_option.substr (oparen, cparen);
	      value = strtoul (value_str.c_str () + 1, NULL, 10);
	    }

	  if (is_pack)
	    {
	      repr.repr_kind = TyTy::ADTType::ReprKind::PACKED;
	      repr.pack = value;
	    }
	  else if (is_align)
	    {
	      repr.repr_kind = TyTy::ADTType::ReprKind::ALIGN;
	      repr.align = value;
	    }
	  else if (is_c)
	    {
	      repr.repr_kind = TyTy::ADTType::ReprKind::C;
	    }
	  else if (is_integer)
	    {
	      repr.repr_kind = TyTy::ADTType::ReprKind::INT;
	      bool ok = context->lookup_builtin (inline_option, &repr.repr);
	      if (!ok)
		{
		  rust_error_at (attr.get_locus (), "Invalid repr type");
		}
	    }

	  delete meta_items;

	  // Multiple repr options must be specified with e.g. #[repr(C,
	  // packed(2))].
	  break;
	}
    }

  return repr;
}

void
TypeCheckBase::resolve_generic_params (
  const std::vector<std::unique_ptr<HIR::GenericParam>> &generic_params,
  std::vector<TyTy::SubstitutionParamMapping> &substitutions, bool is_foreign,
  ABI abi)
{
  for (auto &generic_param : generic_params)
    {
      switch (generic_param->get_kind ())
	{
	  case HIR::GenericParam::GenericKind::LIFETIME: {
	    auto lifetime_param
	      = static_cast<HIR::LifetimeParam &> (*generic_param);
	    auto lifetime = lifetime_param.get_lifetime ();
	    context->get_lifetime_resolver ().insert_mapping (
	      context->intern_lifetime (lifetime));
	  }
	  break;

	  case HIR::GenericParam::GenericKind::CONST: {
	    if (is_foreign && abi != Rust::ABI::INTRINSIC)
	      {
		rust_error_at (generic_param->get_locus (), ErrorCode::E0044,
			       "foreign items may not have const parameters");
	      }

	    auto &param
	      = static_cast<HIR::ConstGenericParam &> (*generic_param);
	    auto specified_type = TypeCheckType::Resolve (param.get_type ());

	    if (param.has_default_expression ())
	      {
		auto expr_type
		  = TypeCheckExpr::Resolve (param.get_default_expression ());

		coercion_site (param.get_mappings ().get_hirid (),
			       TyTy::TyWithLocation (specified_type),
			       TyTy::TyWithLocation (
				 expr_type,
				 param.get_default_expression ().get_locus ()),
			       param.get_locus ());
	      }

	    context->insert_type (generic_param->get_mappings (),
				  specified_type);
	  }
	  break;

	  case HIR::GenericParam::GenericKind::TYPE: {
	    if (is_foreign && abi != Rust::ABI::INTRINSIC)
	      {
		rust_error_at (generic_param->get_locus (), ErrorCode::E0044,
			       "foreign items may not have type parameters");
	      }

	    auto param_type = TypeResolveGenericParam::Resolve (
	      *generic_param, false /*resolve_trait_bounds*/);
	    context->insert_type (generic_param->get_mappings (), param_type);

	    auto &param = static_cast<HIR::TypeParam &> (*generic_param);
	    TyTy::SubstitutionParamMapping p (param, param_type);
	    substitutions.push_back (p);
	  }
	  break;
	}
    }

  // now walk them to setup any specified type param bounds
  for (auto &subst : substitutions)
    {
      auto pty = subst.get_param_ty ();
      TypeResolveGenericParam::ApplyAnyTraitBounds (subst.get_generic_param (),
						    pty);
    }
}

TyTy::TypeBoundPredicate
TypeCheckBase::get_marker_predicate (LangItem::Kind item_type, location_t locus)
{
  DefId item_id = mappings.get_lang_item (item_type, locus);
  HIR::Item *item = mappings.lookup_defid (item_id).value ();
  rust_assert (item->get_item_kind () == HIR::Item::ItemKind::Trait);

  HIR::Trait &trait = *static_cast<HIR::Trait *> (item);
  TraitReference *ref = TraitResolver::Resolve (trait);
  rust_assert (ref != nullptr);

  return TyTy::TypeBoundPredicate (*ref, BoundPolarity::RegularBound, locus);
}

} // namespace Resolver
} // namespace Rust
