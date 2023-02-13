// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

#include "rust-hir-type-check.h"
#include "rust-hir-full.h"
#include "rust-hir-type-check-item.h"
#include "rust-hir-type-check-expr.h"
#include "rust-hir-type-check-pattern.h"
#include "rust-hir-type-check-struct-field.h"
#include "rust-hir-inherent-impl-overlap.h"

extern bool
saw_errors (void);

namespace Rust {
namespace Resolver {

void
TypeResolution::Resolve (HIR::Crate &crate)
{
  for (auto it = crate.items.begin (); it != crate.items.end (); it++)
    TypeCheckItem::Resolve (*it->get ());

  if (saw_errors ())
    return;

  OverlappingImplItemPass::go ();
  if (saw_errors ())
    return;

  auto mappings = Analysis::Mappings::get ();
  auto context = TypeCheckContext::get ();

  // default inference variables if possible
  context->iterate ([&] (HirId id, TyTy::BaseType *ty) mutable -> bool {
    // nothing to do
    if (ty->get_kind () != TyTy::TypeKind::INFER)
      return true;

    TyTy::InferType *infer_var = static_cast<TyTy::InferType *> (ty);
    TyTy::BaseType *default_type;
    bool ok = infer_var->default_type (&default_type);
    if (!ok)
      {
	rust_error_at (mappings->lookup_location (id),
		       "type annotations needed");
	return true;
      }
    else
      {
	auto result
	  = TypeCheckBase::unify_site (id, TyTy::TyWithLocation (ty),
				       TyTy::TyWithLocation (default_type),
				       Location ());
	rust_assert (result);
	rust_assert (result->get_kind () != TyTy::TypeKind::ERROR);
	result->set_ref (id);
	context->insert_type (
	  Analysis::NodeMapping (mappings->get_current_crate (), 0, id,
				 UNKNOWN_LOCAL_DEFID),
	  result);
      }

    return true;
  });
}

// rust-hir-trait-ref.h

TraitItemReference::TraitItemReference (
  std::string identifier, bool optional, TraitItemType type,
  HIR::TraitItem *hir_trait_item, TyTy::BaseType *self,
  std::vector<TyTy::SubstitutionParamMapping> substitutions, Location locus)
  : identifier (identifier), optional_flag (optional), type (type),
    hir_trait_item (hir_trait_item),
    inherited_substitutions (std::move (substitutions)), locus (locus),
    self (self), context (TypeCheckContext::get ())
{}

TraitItemReference::TraitItemReference (TraitItemReference const &other)
  : identifier (other.identifier), optional_flag (other.optional_flag),
    type (other.type), hir_trait_item (other.hir_trait_item),
    locus (other.locus), self (other.self), context (TypeCheckContext::get ())
{
  inherited_substitutions.clear ();
  inherited_substitutions.reserve (other.inherited_substitutions.size ());
  for (size_t i = 0; i < other.inherited_substitutions.size (); i++)
    inherited_substitutions.push_back (
      other.inherited_substitutions.at (i).clone ());
}

TraitItemReference &
TraitItemReference::operator= (TraitItemReference const &other)
{
  identifier = other.identifier;
  optional_flag = other.optional_flag;
  type = other.type;
  hir_trait_item = other.hir_trait_item;
  self = other.self;
  locus = other.locus;
  context = other.context;

  inherited_substitutions.clear ();
  inherited_substitutions.reserve (other.inherited_substitutions.size ());
  for (size_t i = 0; i < other.inherited_substitutions.size (); i++)
    inherited_substitutions.push_back (
      other.inherited_substitutions.at (i).clone ());

  return *this;
}

TyTy::BaseType *
TraitItemReference::get_type_from_typealias (/*const*/
					     HIR::TraitItemType &type) const
{
  TyTy::TyVar var (get_mappings ().get_hirid ());
  return var.get_tyty ();
}

TyTy::BaseType *
TraitItemReference::get_type_from_constant (
  /*const*/ HIR::TraitItemConst &constant) const
{
  TyTy::BaseType *type = TypeCheckType::Resolve (constant.get_type ().get ());
  if (constant.has_expr ())
    {
      TyTy::BaseType *expr
	= TypeCheckExpr::Resolve (constant.get_expr ().get ());

      return TypeCheckBase::unify_site (constant.get_mappings ().get_hirid (),
					TyTy::TyWithLocation (type),
					TyTy::TyWithLocation (expr),
					constant.get_locus ());
    }
  return type;
}

TyTy::BaseType *
TraitItemReference::get_type_from_fn (/*const*/ HIR::TraitItemFunc &fn) const
{
  std::vector<TyTy::SubstitutionParamMapping> substitutions
    = inherited_substitutions;

  HIR::TraitFunctionDecl &function = fn.get_decl ();
  if (function.has_generics ())
    {
      for (auto &generic_param : function.get_generic_params ())
	{
	  switch (generic_param.get ()->get_kind ())
	    {
	    case HIR::GenericParam::GenericKind::LIFETIME:
	    case HIR::GenericParam::GenericKind::CONST:
	      // FIXME: Skipping Lifetime and Const completely until better
	      // handling.
	      break;

	      case HIR::GenericParam::GenericKind::TYPE: {
		auto param_type
		  = TypeResolveGenericParam::Resolve (generic_param.get ());
		context->insert_type (generic_param->get_mappings (),
				      param_type);

		substitutions.push_back (TyTy::SubstitutionParamMapping (
		  static_cast<HIR::TypeParam &> (*generic_param), param_type));
	      }
	      break;
	    }
	}
    }

  TyTy::BaseType *ret_type = nullptr;
  if (!function.has_return_type ())
    ret_type = TyTy::TupleType::get_unit_type (fn.get_mappings ().get_hirid ());
  else
    {
      auto resolved
	= TypeCheckType::Resolve (function.get_return_type ().get ());
      if (resolved->get_kind () == TyTy::TypeKind::ERROR)
	{
	  rust_error_at (fn.get_locus (), "failed to resolve return type");
	  return get_error ();
	}

      ret_type = resolved->clone ();
      ret_type->set_ref (
	function.get_return_type ()->get_mappings ().get_hirid ());
    }

  std::vector<std::pair<HIR::Pattern *, TyTy::BaseType *> > params;
  if (function.is_method ())
    {
      // these are implicit mappings and not used
      auto mappings = Analysis::Mappings::get ();
      auto crate_num = mappings->get_current_crate ();
      Analysis::NodeMapping mapping (crate_num, mappings->get_next_node_id (),
				     mappings->get_next_hir_id (crate_num),
				     UNKNOWN_LOCAL_DEFID);

      // add the synthetic self param at the front, this is a placeholder
      // for compilation to know parameter names. The types are ignored
      // but we reuse the HIR identifier pattern which requires it
      HIR::SelfParam &self_param = function.get_self ();
      HIR::IdentifierPattern *self_pattern
	= new HIR::IdentifierPattern (mapping, "self", self_param.get_locus (),
				      self_param.is_ref (),
				      self_param.is_mut () ? Mutability::Mut
							   : Mutability::Imm,
				      std::unique_ptr<HIR::Pattern> (nullptr));
      // might have a specified type
      TyTy::BaseType *self_type = nullptr;
      if (self_param.has_type ())
	{
	  std::unique_ptr<HIR::Type> &specified_type = self_param.get_type ();
	  self_type = TypeCheckType::Resolve (specified_type.get ());
	}
      else
	{
	  switch (self_param.get_self_kind ())
	    {
	    case HIR::SelfParam::IMM:
	    case HIR::SelfParam::MUT:
	      self_type = self->clone ();
	      break;

	    case HIR::SelfParam::IMM_REF:
	      self_type = new TyTy::ReferenceType (
		self_param.get_mappings ().get_hirid (),
		TyTy::TyVar (self->get_ref ()), Mutability::Imm);
	      break;

	    case HIR::SelfParam::MUT_REF:
	      self_type = new TyTy::ReferenceType (
		self_param.get_mappings ().get_hirid (),
		TyTy::TyVar (self->get_ref ()), Mutability::Mut);
	      break;

	    default:
	      gcc_unreachable ();
	      return nullptr;
	    }
	}

      context->insert_type (self_param.get_mappings (), self_type);
      params.push_back (
	std::pair<HIR::Pattern *, TyTy::BaseType *> (self_pattern, self_type));
    }

  for (auto &param : function.get_function_params ())
    {
      // get the name as well required for later on
      auto param_tyty = TypeCheckType::Resolve (param.get_type ());
      params.push_back (
	std::pair<HIR::Pattern *, TyTy::BaseType *> (param.get_param_name (),
						     param_tyty));

      context->insert_type (param.get_mappings (), param_tyty);
      TypeCheckPattern::Resolve (param.get_param_name (), param_tyty);
    }

  auto mappings = Analysis::Mappings::get ();
  const CanonicalPath *canonical_path = nullptr;
  bool ok = mappings->lookup_canonical_path (fn.get_mappings ().get_nodeid (),
					     &canonical_path);
  rust_assert (ok);

  RustIdent ident{*canonical_path, fn.get_locus ()};
  auto resolved
    = new TyTy::FnType (fn.get_mappings ().get_hirid (),
			fn.get_mappings ().get_defid (),
			function.get_function_name (), ident,
			function.is_method ()
			  ? TyTy::FnType::FNTYPE_IS_METHOD_FLAG
			  : TyTy::FnType::FNTYPE_DEFAULT_FLAGS,
			ABI::RUST, std::move (params), ret_type, substitutions);

  context->insert_type (fn.get_mappings (), resolved);
  return resolved;
}

} // namespace Resolver
} // namespace Rust
