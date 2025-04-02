// Copyright (C) 2021-2025 Free Software Foundation, Inc.

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
#include "rust-substitution-mapper.h"
#include "rust-type-util.h"
#include "rust-immutable-name-resolution-context.h"

// used for flag_name_resolution_2_0
#include "options.h"

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
  location_t locus = type.get_locus ();
  bool is_optional = false;
  std::string identifier = type.get_name ().as_string ();

  resolved = TraitItemReference (identifier, is_optional,
				 TraitItemReference::TraitItemType::TYPE, &type,
				 self, substitutions, locus);
}

void
ResolveTraitItemToRef::visit (HIR::TraitItemConst &cst)
{
  // create trait-item-ref
  location_t locus = cst.get_locus ();
  bool is_optional = cst.has_expr ();
  std::string identifier = cst.get_name ().as_string ();

  resolved = TraitItemReference (identifier, is_optional,
				 TraitItemReference::TraitItemType::CONST, &cst,
				 self, substitutions, locus);
}

void
ResolveTraitItemToRef::visit (HIR::TraitItemFunc &fn)
{
  // create trait-item-ref
  location_t locus = fn.get_locus ();
  bool is_optional = fn.has_definition ();
  std::string identifier = fn.get_decl ().get_function_name ().as_string ();

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

HIR::Trait *
TraitResolver::ResolveHirItem (const HIR::TypePath &path)
{
  TraitResolver resolver;

  HIR::Trait *lookup = nullptr;
  bool ok = resolver.resolve_path_to_trait (path, &lookup);
  return ok ? lookup : nullptr;
}

TraitResolver::TraitResolver () : TypeCheckBase () {}

bool
TraitResolver::resolve_path_to_trait (const HIR::TypePath &path,
				      HIR::Trait **resolved) const
{
  NodeId ref;
  bool ok;
  if (flag_name_resolution_2_0)
    {
      auto &nr_ctx
	= Resolver2_0::ImmutableNameResolutionContext::get ().resolver ();

      auto ref_opt = nr_ctx.lookup (path.get_mappings ().get_nodeid ());

      if ((ok = ref_opt.has_value ()))
	ref = *ref_opt;
    }
  else
    {
      auto path_nodeid = path.get_mappings ().get_nodeid ();
      ok = resolver->lookup_resolved_type (path_nodeid, &ref)
	   || resolver->lookup_resolved_name (path_nodeid, &ref)
	   || resolver->lookup_resolved_macro (path_nodeid, &ref);
    }

  if (!ok)
    {
      rust_error_at (path.get_locus (), "Failed to resolve path to node-id");
      return false;
    }

  auto hid = mappings.lookup_node_to_hir (ref);
  if (!hid)
    {
      rust_error_at (path.get_locus (), "Failed to resolve path to hir-id");
      return false;
    }

  auto resolved_item = mappings.lookup_hir_item (hid.value ());
  if (!resolved_item.has_value ())
    {
      rust_error_at (path.get_locus (),
		     "Failed to resolve trait by looking up hir node");
      return false;
    }

  if (resolved_item.value ()->get_item_kind () != HIR::Item::ItemKind::Trait)
    {
      rich_location r (line_table, path.get_locus ());
      r.add_fixit_replace ("not a trait");
      rust_error_at (r, ErrorCode::E0404, "Expected a trait found %qs",
		     path.as_simple_path ().as_string ().c_str ());
      return false;
    }

  *resolved = static_cast<HIR::Trait *> (*resolved_item);
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
      rust_error_at (
	trait_reference->get_locus (), ErrorCode::E0391,
	"cycle detected when computing the super predicates of %qs",
	trait_reference->get_name ().as_string ().c_str ());
      return &TraitReference::error_node ();
    }

  TraitQueryGuard guard (trait_id);
  TyTy::BaseType *self = nullptr;
  std::vector<TyTy::SubstitutionParamMapping> substitutions;

  // this needs to be special cased for the sized trait to not auto implemented
  // Sized on Self
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
	    auto &typaram = static_cast<HIR::TypeParam &> (*generic_param);
	    bool is_self
	      = typaram.get_type_representation ().as_string ().compare ("Self")
		== 0;

	    // https://doc.rust-lang.org/std/marker/trait.Sized.html
	    // The one exception is the implicit Self type of a trait
	    bool apply_sized = !is_self;
	    auto param_type
	      = TypeResolveGenericParam::Resolve (*generic_param, true,
						  apply_sized);

	    context->insert_type (generic_param->get_mappings (), param_type);
	    substitutions.push_back (
	      TyTy::SubstitutionParamMapping (typaram, param_type));

	    if (is_self)
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
				BoundPolarity::RegularBound,
				trait_reference->get_locus ());
  specified_bounds.push_back (self_hrtb);

  // look for any
  std::vector<TyTy::TypeBoundPredicate> super_traits;
  if (trait_reference->has_type_param_bounds ())
    {
      for (auto &bound : trait_reference->get_type_param_bounds ())
	{
	  if (bound->get_bound_type ()
	      == HIR::TypeParamBound::BoundType::TRAITBOUND)
	    {
	      HIR::TraitBound *b
		= static_cast<HIR::TraitBound *> (bound.get ());

	      auto predicate = get_predicate_from_bound (
		b->get_path (),
		tl::nullopt /*this will setup a PLACEHOLDER for self*/);
	      if (predicate.is_error ())
		return &TraitReference::error_node ();

	      specified_bounds.push_back (predicate);
	      super_traits.push_back (predicate);
	    }
	}
    }
  self->inherit_bounds (specified_bounds);

  context->block_context ().enter (TypeCheckBlockContextItem (trait_reference));
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

  TraitReference trait_object (trait_reference, item_refs, super_traits,
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
  context->block_context ().exit ();

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
    = new TyTy::PlaceholderType (type.get_name ().as_string (),
				 type.get_mappings ().get_defid (),
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
  TyTy::BaseType *item_tyty = get_tyty ();
  if (item_tyty->get_kind () == TyTy::TypeKind::ERROR)
    return;

  if (!is_optional ())
    return;

  // check the block and return types
  rust_assert (item_tyty->get_kind () == TyTy::TypeKind::FNDEF);

  // need to get the return type from this
  TyTy::FnType *resolved_fn_type = static_cast<TyTy::FnType *> (item_tyty);
  auto expected_ret_tyty = resolved_fn_type->get_return_type ();
  context->push_return_type (TypeCheckContextItem (&func), expected_ret_tyty);

  auto block_expr_ty = TypeCheckExpr::Resolve (func.get_block_expr ());

  location_t fn_return_locus
    = func.get_decl ().has_return_type ()
	? func.get_decl ().get_return_type ().get_locus ()
	: func.get_locus ();

  coercion_site (func.get_mappings ().get_hirid (),
		 TyTy::TyWithLocation (expected_ret_tyty, fn_return_locus),
		 TyTy::TyWithLocation (block_expr_ty), func.get_locus ());

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

void
AssociatedImplTrait::setup_raw_associated_types ()
{
  auto &impl_items = impl->get_impl_items ();
  for (auto &impl_item : impl_items)
    {
      bool is_type_alias = impl_item->get_impl_item_type ()
			   == HIR::ImplItem::ImplItemType::TYPE_ALIAS;
      if (!is_type_alias)
	continue;

      HIR::TypeAlias &type = *static_cast<HIR::TypeAlias *> (impl_item.get ());

      TraitItemReference *resolved_trait_item = nullptr;
      bool ok
	= trait->lookup_trait_item (type.get_new_type_name ().as_string (),
				    &resolved_trait_item);
      if (!ok)
	continue;
      if (resolved_trait_item->get_trait_item_type ()
	  != TraitItemReference::TraitItemType::TYPE)
	continue;

      TyTy::BaseType *lookup;
      ok = context->lookup_type (type.get_mappings ().get_hirid (), &lookup);
      rust_assert (ok);

      resolved_trait_item->associated_type_set (lookup);
    }
}

TyTy::BaseType *
AssociatedImplTrait::setup_associated_types (
  const TyTy::BaseType *self, const TyTy::TypeBoundPredicate &bound,
  TyTy::SubstitutionArgumentMappings *args, bool infer)
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

  rust_debug ("setup_associated_types for: %s->%s bound %s",
	      associated_self->debug_str ().c_str (),
	      self->debug_str ().c_str (), bound.as_string ().c_str ());

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

  // this callback gives us the parameters that get substituted so we can
  // compute the constrained type parameters for this impl block
  std::map<std::string, HirId> param_mappings;
  TyTy::ParamSubstCb param_subst_cb
    = [&] (const TyTy::ParamType &p, const TyTy::SubstitutionArg &a) {
	param_mappings[p.get_symbol ()] = a.get_tyty ()->get_ref ();
      };

  // generate inference variables for these bound arguments so we can compute
  // their values
  location_t locus = UNKNOWN_LOCATION;
  std::vector<TyTy::SubstitutionArg> subst_args;
  for (auto &p : substitutions)
    {
      if (p.needs_substitution () && infer)
	{
	  TyTy::TyVar infer_var = TyTy::TyVar::get_implicit_infer_var (locus);
	  subst_args.push_back (
	    TyTy::SubstitutionArg (&p, infer_var.get_tyty ()));
	}
      else
	{
	  TyTy::ParamType *param = p.get_param_ty ();
	  TyTy::BaseType *resolved = param->destructure ();
	  subst_args.push_back (TyTy::SubstitutionArg (&p, resolved));
	  param_mappings[param->get_symbol ()] = resolved->get_ref ();
	}
    }

  TyTy::SubstitutionArgumentMappings infer_arguments (
    std::move (subst_args), {},
    TyTy::SubstitutionArgumentMappings::regions_from_nullable_args (args),
    locus, param_subst_cb);
  TyTy::BaseType *impl_self_infer
    = (!associated_self->is_concrete ())
	? SubstMapperInternal::Resolve (associated_self, infer_arguments)
	: associated_self;

  const TyTy::TypeBoundPredicate &impl_predicate
    = associated_self->lookup_predicate (bound.get_id ());
  rust_assert (!impl_predicate.is_error ());

  // infer the arguments on the predicate
  std::vector<TyTy::BaseType *> impl_trait_predicate_args;

  for (size_t i = 0; i < impl_predicate.get_substs ().size (); i++)
    {
      const auto &arg = impl_predicate.get_substs ().at (i);
      if (i == 0)
	continue;

      const TyTy::ParamType *p = arg.get_param_ty ();
      TyTy::BaseType *r = p->resolve ();
      if (!r->is_concrete ())
	{
	  r = SubstMapperInternal::Resolve (r, infer_arguments);
	}
      impl_trait_predicate_args.push_back (r);
    }

  // unify the bounds arguments
  std::vector<TyTy::BaseType *> hrtb_bound_arguments;
  for (size_t i = 0; i < bound.get_substs ().size (); i++)
    {
      const auto &arg = bound.get_substs ().at (i);
      if (i == 0)
	continue;

      const TyTy::ParamType *p = arg.get_param_ty ();
      TyTy::BaseType *r = p->resolve ();
      if (!r->is_concrete ())
	{
	  r = SubstMapperInternal::Resolve (r, infer_arguments);
	}
      hrtb_bound_arguments.push_back (r);
    }

  rust_assert (impl_trait_predicate_args.size ()
	       == hrtb_bound_arguments.size ());
  for (size_t i = 0; i < impl_trait_predicate_args.size (); i++)
    {
      TyTy::BaseType *a = impl_trait_predicate_args.at (i);
      TyTy::BaseType *b = hrtb_bound_arguments.at (i);

      TyTy::BaseType *result
	= unify_site_and (a->get_ref (), TyTy::TyWithLocation (a),
			  TyTy::TyWithLocation (b), impl_predicate.get_locus (),
			  true /*emit-errors*/, true /*commit-if-ok*/,
			  true /*infer*/, true /*cleanup-on-fail*/);
      rust_assert (result->get_kind () != TyTy::TypeKind::ERROR);
    }

  // we need to unify the receiver with the impl-block Self so that we compute
  // the type correctly as our receiver may be generic and we are inferring its
  // generic arguments and this Self might be the concrete version or vice
  // versa.
  auto result = unify_site_and (get_impl_block ()->get_mappings ().get_hirid (),
				TyTy::TyWithLocation (receiver),
				TyTy::TyWithLocation (impl_self_infer),
				impl_predicate.get_locus (),
				true /*emit-errors*/, true /*commit-if-ok*/,
				true /*infer*/, true /*cleanup-on-fail*/);
  rust_assert (result->get_kind () != TyTy::TypeKind::ERROR);
  TyTy::BaseType *self_result = result;

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
    std::move (associated_arguments), {},
    TyTy::SubstitutionArgumentMappings::regions_from_nullable_args (args),
    locus);

  auto &impl_items = impl->get_impl_items ();
  for (auto &impl_item : impl_items)
    {
      bool is_type_alias = impl_item->get_impl_item_type ()
			   == HIR::ImplItem::ImplItemType::TYPE_ALIAS;
      if (!is_type_alias)
	continue;

      HIR::TypeAlias &type = *static_cast<HIR::TypeAlias *> (impl_item.get ());

      TraitItemReference *resolved_trait_item = nullptr;
      bool ok
	= trait->lookup_trait_item (type.get_new_type_name ().as_string (),
				    &resolved_trait_item);
      if (!ok)
	continue;
      if (resolved_trait_item->get_trait_item_type ()
	  != TraitItemReference::TraitItemType::TYPE)
	continue;

      TyTy::BaseType *lookup;
      if (!context->lookup_type (type.get_mappings ().get_hirid (), &lookup))
	continue;

      // this might be generic
      TyTy::BaseType *substituted
	= SubstMapperInternal::Resolve (lookup, associated_type_args);
      resolved_trait_item->associated_type_set (substituted);
    }

  if (args != nullptr)
    {
      *args = associated_type_args;
    }

  return self_result;
}

void
AssociatedImplTrait::reset_associated_types ()
{
  trait->clear_associated_types ();
}

location_t
AssociatedImplTrait::get_locus () const
{
  return impl->get_locus ();
}

Analysis::NodeMapping
TraitItemReference::get_parent_trait_mappings () const
{
  auto &mappings = Analysis::Mappings::get ();

  HIR::Trait *trait
    = mappings.lookup_trait_item_mapping (get_mappings ().get_hirid ());
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
