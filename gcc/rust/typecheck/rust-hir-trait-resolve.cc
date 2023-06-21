// Copyright (C) 2021-2023 Free Software Foundation, Inc.

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

#include "rust-hir-trait-resolve.h"
#include "rust-hir-type-check-expr.h"

namespace Rust {
namespace Resolver {

TraitItemReference
ResolveTraitItemToRef::Resolve (
  HIR::TraitItem &item, TyTy::BaseType *self,
  std::vector<TyTy::SubstitutionParamMapping> substitutions)
{
  ResolveTraitItemToRef resolver (self, std::move (substitutions));
  item.accept_vis (resolver);
  return std::move (resolver.resolved);
}

void
ResolveTraitItemToRef::visit (HIR::TraitItemType &type)
{
  // create trait-item-ref
  Location locus = type.get_locus ();
  bool is_optional = false;
  std::string identifier = type.get_name ();

  resolved = TraitItemReference (identifier, is_optional,
				 TraitItemReference::TraitItemType::TYPE, &type,
				 self, substitutions, locus);
}

void
ResolveTraitItemToRef::visit (HIR::TraitItemConst &cst)
{
  // create trait-item-ref
  Location locus = cst.get_locus ();
  bool is_optional = cst.has_expr ();
  std::string identifier = cst.get_name ();

  resolved = TraitItemReference (identifier, is_optional,
				 TraitItemReference::TraitItemType::CONST, &cst,
				 self, substitutions, locus);
}

void
ResolveTraitItemToRef::visit (HIR::TraitItemFunc &fn)
{
  // create trait-item-ref
  Location locus = fn.get_locus ();
  bool is_optional = fn.has_block_defined ();
  std::string identifier = fn.get_decl ().get_function_name ();

  resolved = TraitItemReference (identifier, is_optional,
				 TraitItemReference::TraitItemType::FN, &fn,
				 self, std::move (substitutions), locus);
}

ResolveTraitItemToRef::ResolveTraitItemToRef (
  TyTy::BaseType *self,
  std::vector<TyTy::SubstitutionParamMapping> &&substitutions)
  : TypeCheckBase (), resolved (TraitItemReference::error ()), self (self),
    substitutions (std::move (substitutions))
{}

// TraitItemReference items

TraitReference *
TraitResolver::Resolve (HIR::TypePath &path)
{
  TraitResolver resolver;
  return resolver.resolve_path (path);
}

TraitReference *
TraitResolver::Resolve (HIR::Trait &trait)
{
  TraitResolver resolver;
  return resolver.resolve_trait (&trait);
}

TraitReference *
TraitResolver::Lookup (HIR::TypePath &path)
{
  TraitResolver resolver;
  return resolver.lookup_path (path);
}

TraitResolver::TraitResolver () : TypeCheckBase () {}

bool
TraitResolver::resolve_path_to_trait (const HIR::TypePath &path,
				      HIR::Trait **resolved) const
{
  NodeId ref;
  if (!resolver->lookup_resolved_type (path.get_mappings ().get_nodeid (),
				       &ref))
    {
      rust_error_at (path.get_locus (), "Failed to resolve path to node-id");
      return false;
    }

  HirId hir_node = UNKNOWN_HIRID;
  if (!mappings->lookup_node_to_hir (ref, &hir_node))
    {
      rust_error_at (path.get_locus (), "Failed to resolve path to hir-id");
      return false;
    }

  HIR::Item *resolved_item = mappings->lookup_hir_item (hir_node);
  rust_assert (resolved_item != nullptr);
  rust_assert (resolved_item->get_item_kind () == HIR::Item::ItemKind::Trait);
  *resolved = static_cast<HIR::Trait *> (resolved_item);

  return true;
}

TraitReference *
TraitResolver::resolve_path (HIR::TypePath &path)
{
  HIR::Trait *resolved_trait_reference;
  bool ok = resolve_path_to_trait (path, &resolved_trait_reference);
  if (!ok)
    return &TraitReference::error_node ();

  return resolve_trait (resolved_trait_reference);
}

TraitReference *
TraitResolver::resolve_trait (HIR::Trait *trait_reference)
{
  TraitReference *tref = &TraitReference::error_node ();
  if (context->lookup_trait_reference (
	trait_reference->get_mappings ().get_defid (), &tref))
    {
      return tref;
    }

  DefId trait_id = trait_reference->get_mappings ().get_defid ();
  if (context->trait_query_in_progress (trait_id))
    {
      rust_error_at (trait_reference->get_locus (), "trait cycle detected");
      return &TraitReference::error_node ();
    }

  TraitQueryGuard guard (trait_id);
  TyTy::BaseType *self = nullptr;
  std::vector<TyTy::SubstitutionParamMapping> substitutions;

  // FIXME
  // this should use the resolve_generic_params like everywhere else
  for (auto &generic_param : trait_reference->get_generic_params ())
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
	    context->insert_type (generic_param->get_mappings (), param_type);

	    auto &typaram = static_cast<HIR::TypeParam &> (*generic_param);
	    substitutions.push_back (
	      TyTy::SubstitutionParamMapping (typaram, param_type));

	    if (typaram.get_type_representation ().compare ("Self") == 0)
	      {
		rust_assert (param_type->get_kind () == TyTy::TypeKind::PARAM);
		TyTy::ParamType *p
		  = static_cast<TyTy::ParamType *> (param_type);
		p->set_implicit_self_trait ();
		self = p;
	      }
	  }
	  break;
	}
    }
  rust_assert (self != nullptr);

  // Check if there is a super-trait, and apply this bound to the Self
  // TypeParam
  std::vector<TyTy::TypeBoundPredicate> specified_bounds;

  // copy the substitition mappings
  std::vector<TyTy::SubstitutionParamMapping> self_subst_copy;
  for (auto &sub : substitutions)
    self_subst_copy.push_back (sub.clone ());

  // They also inherit themselves as a bound this enables a trait item to
  // reference other Self::trait_items
  auto self_hrtb
    = TyTy::TypeBoundPredicate (trait_reference->get_mappings ().get_defid (),
				std::move (self_subst_copy),
				trait_reference->get_locus ());
  specified_bounds.push_back (self_hrtb);

  // look for any
  std::vector<const TraitReference *> super_traits;
  if (trait_reference->has_type_param_bounds ())
    {
      for (auto &bound : trait_reference->get_type_param_bounds ())
	{
	  if (bound->get_bound_type ()
	      == HIR::TypeParamBound::BoundType::TRAITBOUND)
	    {
	      HIR::TraitBound *b
		= static_cast<HIR::TraitBound *> (bound.get ());

	      auto predicate = get_predicate_from_bound (b->get_path ());
	      if (predicate.is_error ())
		return &TraitReference::error_node ();

	      specified_bounds.push_back (predicate);
	      super_traits.push_back (predicate.get ());
	    }
	}
    }
  self->inherit_bounds (specified_bounds);

  std::vector<TraitItemReference> item_refs;
  for (auto &item : trait_reference->get_trait_items ())
    {
      // make a copy of the substs
      std::vector<TyTy::SubstitutionParamMapping> item_subst;
      for (auto &sub : substitutions)
	item_subst.push_back (sub.clone ());

      TraitItemReference trait_item_ref
	= ResolveTraitItemToRef::Resolve (*item.get (), self,
					  std::move (item_subst));
      item_refs.push_back (std::move (trait_item_ref));
    }

  TraitReference trait_object (trait_reference, item_refs,
			       std::move (super_traits),
			       std::move (substitutions));
  context->insert_trait_reference (
    trait_reference->get_mappings ().get_defid (), std::move (trait_object));

  tref = &TraitReference::error_node ();
  bool ok = context->lookup_trait_reference (
    trait_reference->get_mappings ().get_defid (), &tref);
  rust_assert (ok);

  // hook to allow the trait to resolve its optional item blocks, we cant
  // resolve the blocks of functions etc because it can end up in a recursive
  // loop of trying to resolve traits as required by the types
  tref->on_resolved ();

  return tref;
}

TraitReference *
TraitResolver::lookup_path (HIR::TypePath &path)
{
  HIR::Trait *resolved_trait_reference;
  bool ok = resolve_path_to_trait (path, &resolved_trait_reference);
  if (!ok)
    return &TraitReference::error_node ();

  TraitReference *tref = &TraitReference::error_node ();
  if (context->lookup_trait_reference (
	resolved_trait_reference->get_mappings ().get_defid (), &tref))
    {
      return tref;
    }
  return &TraitReference::error_node ();
}

void
TraitItemReference::on_resolved ()
{
  switch (type)
    {
    case CONST:
      resolve_item (static_cast<HIR::TraitItemConst &> (*hir_trait_item));
      break;

    case TYPE:
      resolve_item (static_cast<HIR::TraitItemType &> (*hir_trait_item));
      break;

    case FN:
      resolve_item (static_cast<HIR::TraitItemFunc &> (*hir_trait_item));
      break;

    default:
      break;
    }
}

void
TraitItemReference::resolve_item (HIR::TraitItemType &type)
{
  TyTy::BaseType *ty
    = new TyTy::PlaceholderType (type.get_name (),
				 type.get_mappings ().get_hirid ());
  context->insert_type (type.get_mappings (), ty);
}

void
TraitItemReference::resolve_item (HIR::TraitItemConst &constant)
{
  // TODO
}

void
TraitItemReference::resolve_item (HIR::TraitItemFunc &func)
{
  if (!is_optional ())
    return;

  TyTy::BaseType *item_tyty = get_tyty ();
  if (item_tyty->get_kind () == TyTy::TypeKind::ERROR)
    return;

  // check the block and return types
  rust_assert (item_tyty->get_kind () == TyTy::TypeKind::FNDEF);

  // need to get the return type from this
  TyTy::FnType *resolved_fn_type = static_cast<TyTy::FnType *> (item_tyty);
  auto expected_ret_tyty = resolved_fn_type->get_return_type ();
  context->push_return_type (TypeCheckContextItem (&func), expected_ret_tyty);

  auto block_expr_ty = TypeCheckExpr::Resolve (func.get_block_expr ().get ());

  Location fn_return_locus
    = func.get_decl ().has_return_type ()
	? func.get_decl ().get_return_type ()->get_locus ()
	: func.get_locus ();

  TypeCheckBase::coercion_site (func.get_mappings ().get_hirid (),
				TyTy::TyWithLocation (expected_ret_tyty,
						      fn_return_locus),
				TyTy::TyWithLocation (block_expr_ty),
				func.get_locus ());

  context->pop_return_type ();
}

void
TraitItemReference::associated_type_set (TyTy::BaseType *ty) const
{
  rust_assert (get_trait_item_type () == TraitItemType::TYPE);

  TyTy::BaseType *item_ty = get_tyty ();
  rust_assert (item_ty->get_kind () == TyTy::TypeKind::PLACEHOLDER);
  TyTy::PlaceholderType *placeholder
    = static_cast<TyTy::PlaceholderType *> (item_ty);

  placeholder->set_associated_type (ty->get_ty_ref ());
}

void
TraitItemReference::associated_type_reset (bool only_projections) const
{
  rust_assert (get_trait_item_type () == TraitItemType::TYPE);

  TyTy::BaseType *item_ty = get_tyty ();
  rust_assert (item_ty->get_kind () == TyTy::TypeKind::PLACEHOLDER);
  TyTy::PlaceholderType *placeholder
    = static_cast<TyTy::PlaceholderType *> (item_ty);

  if (!only_projections)
    {
      placeholder->clear_associated_type ();
    }
  else
    {
      if (!placeholder->can_resolve ())
	return;

      const TyTy::BaseType *r = placeholder->resolve ();
      if (r->get_kind () == TyTy::TypeKind::PROJECTION)
	{
	  placeholder->clear_associated_type ();
	}
    }
}

TyTy::BaseType *
AssociatedImplTrait::setup_associated_types (
  const TyTy::BaseType *self, const TyTy::TypeBoundPredicate &bound)
{
  // compute the constrained impl block generic arguments based on self and the
  // higher ranked trait bound
  TyTy::BaseType *receiver = self->clone ();

  // impl<Y> SliceIndex<[Y]> for Range<usize>
  // vs
  // I: SliceIndex<[<integer>]> and Range<<integer>>
  //
  // we need to figure out what Y is

  TyTy::BaseType *associated_self = get_self ();
  rust_assert (associated_self->can_eq (self, false));

  // grab the parameters
  HIR::ImplBlock &impl_block = *get_impl_block ();
  std::vector<TyTy::SubstitutionParamMapping> substitutions;
  for (auto &generic_param : impl_block.get_generic_params ())
    {
      switch (generic_param.get ()->get_kind ())
	{
	case HIR::GenericParam::GenericKind::LIFETIME:
	case HIR::GenericParam::GenericKind::CONST:
	  // FIXME: Skipping Lifetime and Const completely until better
	  // handling.
	  break;

	  case HIR::GenericParam::GenericKind::TYPE: {
	    TyTy::BaseType *l = nullptr;
	    bool ok = context->lookup_type (
	      generic_param->get_mappings ().get_hirid (), &l);
	    if (ok && l->get_kind () == TyTy::TypeKind::PARAM)
	      {
		substitutions.push_back (TyTy::SubstitutionParamMapping (
		  static_cast<HIR::TypeParam &> (*generic_param),
		  static_cast<TyTy::ParamType *> (l)));
	      }
	  }
	  break;
	}
    }

  // generate inference variables for these bound arguments so we can compute
  // their values
  Location locus;
  std::vector<TyTy::SubstitutionArg> args;
  for (auto &p : substitutions)
    {
      if (p.needs_substitution ())
	{
	  TyTy::TyVar infer_var = TyTy::TyVar::get_implicit_infer_var (locus);
	  args.push_back (TyTy::SubstitutionArg (&p, infer_var.get_tyty ()));
	}
      else
	{
	  args.push_back (
	    TyTy::SubstitutionArg (&p, p.get_param_ty ()->resolve ()));
	}
    }

  // this callback gives us the parameters that get substituted so we can
  // compute the constrained type parameters for this impl block
  std::map<std::string, HirId> param_mappings;
  TyTy::ParamSubstCb param_subst_cb
    = [&] (const TyTy::ParamType &p, const TyTy::SubstitutionArg &a) {
	param_mappings[p.get_symbol ()] = a.get_tyty ()->get_ref ();
      };

  TyTy::SubstitutionArgumentMappings infer_arguments (std::move (args), {},
						      locus, param_subst_cb);
  TyTy::BaseType *impl_self_infer
    = (associated_self->needs_generic_substitutions ())
	? SubstMapperInternal::Resolve (associated_self, infer_arguments)
	: associated_self;

  // FIXME this needs to do a lookup for the trait-reference DefId instead of
  // assuming its the first one in the list
  rust_assert (associated_self->num_specified_bounds () > 0);
  TyTy::TypeBoundPredicate &impl_predicate
    = associated_self->get_specified_bounds ().at (0);

  // infer the arguments on the predicate
  std::vector<TyTy::BaseType *> impl_trait_predicate_args;
  for (const auto &arg : impl_predicate.get_substs ())
    {
      const TyTy::ParamType *p = arg.get_param_ty ();
      if (p->get_symbol ().compare ("Self") == 0)
	continue;

      TyTy::BaseType *r = p->resolve ();
      r = SubstMapperInternal::Resolve (r, infer_arguments);
      impl_trait_predicate_args.push_back (r);
    }

  // we need to unify the receiver with the impl-block Self so that we compute
  // the type correctly as our receiver may be generic and we are inferring its
  // generic arguments and this Self might be the concrete version or vice
  // versa.
  auto result = TypeCheckBase::unify_site (
    get_impl_block ()->get_mappings ().get_hirid (),
    TyTy::TyWithLocation (receiver), TyTy::TyWithLocation (impl_self_infer),
    impl_predicate.get_locus ());
  rust_assert (result->get_kind () != TyTy::TypeKind::ERROR);
  TyTy::BaseType *self_result = result;

  // unify the bounds arguments
  std::vector<TyTy::BaseType *> hrtb_bound_arguments;
  for (const auto &arg : bound.get_substs ())
    {
      const TyTy::ParamType *p = arg.get_param_ty ();
      if (p->get_symbol ().compare ("Self") == 0)
	continue;

      TyTy::BaseType *r = p->resolve ();
      hrtb_bound_arguments.push_back (r);
    }

  if (impl_trait_predicate_args.size () != hrtb_bound_arguments.size ())
    return self_result;

  for (size_t i = 0; i < impl_trait_predicate_args.size (); i++)
    {
      TyTy::BaseType *a = impl_trait_predicate_args.at (i);
      TyTy::BaseType *b = hrtb_bound_arguments.at (i);

      result
	= TypeCheckBase::unify_site (a->get_ref (), TyTy::TyWithLocation (a),
				     TyTy::TyWithLocation (b),
				     impl_predicate.get_locus ());
      rust_assert (result->get_kind () != TyTy::TypeKind::ERROR);
    }

  // create the argument list
  std::vector<TyTy::SubstitutionArg> associated_arguments;
  for (auto &p : substitutions)
    {
      std::string symbol = p.get_param_ty ()->get_symbol ();
      auto it = param_mappings.find (symbol);
      rust_assert (it != param_mappings.end ());

      HirId id = it->second;
      TyTy::BaseType *argument = nullptr;
      bool ok = context->lookup_type (id, &argument);
      rust_assert (ok);

      TyTy::SubstitutionArg arg (&p, argument);
      associated_arguments.push_back (arg);
    }

  TyTy::SubstitutionArgumentMappings associated_type_args (
    std::move (associated_arguments), {}, locus);

  ImplTypeIterator iter (*impl, [&] (HIR::TypeAlias &type) {
    TraitItemReference *resolved_trait_item = nullptr;
    bool ok = trait->lookup_trait_item (type.get_new_type_name (),
					&resolved_trait_item);
    if (!ok)
      return;
    if (resolved_trait_item->get_trait_item_type ()
	!= TraitItemReference::TraitItemType::TYPE)
      return;

    TyTy::BaseType *lookup;
    if (!context->lookup_type (type.get_mappings ().get_hirid (), &lookup))
      return;

    // this might be generic
    TyTy::BaseType *substituted
      = SubstMapperInternal::Resolve (lookup, associated_type_args);
    resolved_trait_item->associated_type_set (substituted);
  });
  iter.go ();

  return self_result;
}

void
AssociatedImplTrait::reset_associated_types ()
{
  trait->clear_associated_types ();
}

Analysis::NodeMapping
TraitItemReference::get_parent_trait_mappings () const
{
  auto mappings = Analysis::Mappings::get ();

  HIR::Trait *trait
    = mappings->lookup_trait_item_mapping (get_mappings ().get_hirid ());
  rust_assert (trait != nullptr);

  return trait->get_mappings ();
}

bool
TraitItemReference::is_object_safe () const
{
  // https://doc.rust-lang.org/reference/items/traits.html#object-safety
  switch (get_trait_item_type ())
    {
      case TraitItemReference::TraitItemType::FN: {
	// lets be boring and just check that this is indeed a method will do
	// for now
	const HIR::TraitItem *item = get_hir_trait_item ();
	const HIR::TraitItemFunc *fn
	  = static_cast<const HIR::TraitItemFunc *> (item);
	return fn->get_decl ().is_method ();
      }

      // constants are not available via dyn dispatch and so is not object safe
    case TraitItemReference::TraitItemType::CONST:
      return false;

      // types are object safe since they are not available via dyn dispatch
    case TraitItemReference::TraitItemType::TYPE:
      return true;

      // this is just an error so lets just fail it
    case TraitItemReference::TraitItemType::ERROR:
      return false;
    }
  return false;
}

} // namespace Resolver
} // namespace Rust
