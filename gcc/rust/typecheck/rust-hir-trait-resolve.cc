// Copyright (C) 2021-2026 Free Software Foundation, Inc.

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
#include "rich-location.h"
#include "rust-hir-trait-reference.h"
#include "rust-hir-type-check-expr.h"
#include "rust-rib.h"
#include "rust-substitution-mapper.h"
#include "text-range-label.h"
#include "rust-type-util.h"
#include "rust-finalized-name-resolution-context.h"

namespace Rust {
namespace Resolver {

static bool
validate_impl_substitution_bounds (
  const std::vector<TyTy::SubstitutionArg> &resolved_args, location_t locus,
  bool emit_error)
{
  auto &mctx = Analysis::Mappings::get ();

  std::vector<TyTy::SubstitutionArg> args;
  for (const auto &arg : resolved_args)
    args.push_back (arg);

  TyTy::SubstitutionArgumentMappings mappings (std::move (args),
					       {} /*binding_args*/,
					       TyTy::RegionParamList (0),
					       locus);

  for (const auto &arg : resolved_args)
    {
      TyTy::BaseGeneric *param
	= const_cast<TyTy::BaseGeneric *> (arg.get_param_ty ());
      if (param == nullptr)
	continue;

      TyTy::BaseType *resolved_arg = arg.get_tyty ();
      if (resolved_arg->get_kind () == TyTy::TypeKind::PARAM)
	resolved_arg
	  = static_cast<TyTy::ParamType *> (resolved_arg)->resolve ();

      if (resolved_arg->get_kind () == TyTy::TypeKind::PARAM
	  || resolved_arg->get_kind () == TyTy::TypeKind::INFER)
	continue;

      auto arg_type_locus
	= mctx.lookup_location (arg.get_tyty ()->get_ty_ref ());
      for (auto bound : param->get_specified_bounds ())
	{
	  auto bound_locus = bound.get_locus ();
	  auto trait_locus = bound.get ()->get_locus ();
	  bound.apply_argument_mappings (mappings, false /*is_super_trait*/);

	  if (!resolved_arg->satisfies_bound (bound, false /*emit_error*/))
	    {
	      if (emit_error)
		{
		  rich_location r (line_table, locus);

		  std::string arg_label_text = "the trait " + bound.get_name ()
					       + " is not implemented for "
					       + resolved_arg->get_name ();

		  text_range_label arg_label (arg_label_text.c_str ());
		  r.add_range (arg_type_locus, SHOW_RANGE_WITHOUT_CARET,
			       &arg_label);

		  bool ambiguous = false;
		  auto *trait_impl
		    = lookup_associated_impl_block (bound, resolved_arg,
						    &ambiguous);
		  text_range_label trait_label (
		    "this trait has no implementations, consider adding one");
		  if (trait_impl == nullptr)
		    r.add_range (trait_locus, SHOW_RANGE_WITHOUT_CARET,
				 &trait_label);

		  text_range_label bound_label (
		    "unsatisfied trait bound introduced here");
		  r.add_range (bound_locus, SHOW_RANGE_WITHOUT_CARET,
			       &bound_label);

		  rust_error_at (r, ErrorCode::E0277,
				 "the trait bound %<%s: %s%> is not satisfied",
				 resolved_arg->get_name ().c_str (),
				 bound.get_name ().c_str ());
		}
	      return false;
	    }
	}
    }

  return true;
}

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
  auto &nr_ctx = Resolver2_0::FinalizedNameResolutionContext::get ();

  NodeId ref;
  if (auto ref_opt = nr_ctx.lookup (path.get_mappings ().get_nodeid (),
				    Resolver2_0::Namespace::Types))
    {
      ref = *ref_opt;
    }
  else
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

	case HIR::GenericParam::GenericKind::TYPE:
	  {
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
	    substitutions.emplace_back (typaram, param_type);

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
  self_subst_copy.reserve (substitutions.size ());

  for (auto &sub : substitutions)
    self_subst_copy.push_back (sub.clone ());

  // They also inherit themselves as a bound this enables a trait item to
  // reference other Self::trait_items
  specified_bounds.emplace_back (trait_reference->get_mappings ().get_defid (),
				 std::move (self_subst_copy),
				 BoundPolarity::RegularBound,
				 trait_reference->get_locus ());

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
		tl::nullopt /*this will setup a PLACEHOLDER for self*/,
		BoundPolarity::RegularBound, false, true);
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
      item_subst.reserve (substitutions.size ());

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
TraitItemReference::on_resolved (const TraitReference *tref)
{
  switch (type)
    {
    case CONST:
      resolve_item (tref, static_cast<HIR::TraitItemConst &> (*hir_trait_item));
      break;

    case TYPE:
      resolve_item (tref, static_cast<HIR::TraitItemType &> (*hir_trait_item));
      break;

    case FN:
      {
	TyTy::BaseType *fn_type = get_tyty ();
	if (is_optional () && fn_type->get_kind () == TyTy::TypeKind::FNDEF)
	  context->mark_function_body_pending (
	    static_cast<TyTy::FnType *> (fn_type)->get_id ());
      }
      break;

    default:
      break;
    }
}

void
TraitItemReference::resolve_default_function_body (const TraitReference *tref)
{
  if (type != FN || !is_optional ())
    return;

  auto &func = static_cast<HIR::TraitItemFunc &> (*hir_trait_item);
  TyTy::BaseType *item_tyty = get_tyty ();
  if (item_tyty->get_kind () != TyTy::TypeKind::FNDEF)
    return;

  auto fn_type = static_cast<TyTy::FnType *> (item_tyty);
  if (!context->function_body_pending (fn_type->get_id ()))
    return;

  resolve_item (tref, func);
}

void
TraitItemReference::resolve_item (const TraitReference *tref,
				  HIR::TraitItemType &type)
{
  auto substitutions = inherited_substitutions;
  if (type.has_generics ())
    {
      auto binder_pin = context->push_lifetime_binder ();
      TypeCheckBase::ResolveGenericParams (HIR::Item::ItemKind::TypeAlias,
					   type.get_locus (),
					   type.get_generic_params (),
					   substitutions, false, ABI::RUST);
    }

  size_t inherited_count = inherited_substitutions.size ();
  auto projection
    = new TyTy::ProjectionType (type.get_mappings ().get_hirid (), nullptr,
				tref, type.get_mappings ().get_defid (),
				substitutions, self,
				TyTy::SubstitutionArgumentMappings::error (),
				{}, {}, inherited_count);

  // Attach the bounds declared on the associated type itself:
  //
  //     type IntoIter: Iterator<Item = Self::Item>
  //
  // so satisfies_bound finds them in specified_bounds
  if (type.has_type_param_bounds ())
    {
      std::vector<TyTy::TypeBoundPredicate> trait_item_bounds;
      for (auto &bound : type.get_type_param_bounds ())
	{
	  if (bound->get_bound_type ()
	      != HIR::TypeParamBound::BoundType::TRAITBOUND)
	    continue;
	  auto *b = static_cast<HIR::TraitBound *> (bound.get ());
	  auto predicate = TypeCheckBase::ResolvePredicateFromBound (
	    b->get_path (),
	    tl::nullopt /*this will setup a PLACEHOLDER for self*/,
	    BoundPolarity::RegularBound, false, true);
	  if (!predicate.is_error ())
	    trait_item_bounds.push_back (std::move (predicate));
	}
      if (!trait_item_bounds.empty ())
	projection->inherit_bounds (trait_item_bounds);
    }

  context->insert_type (type.get_mappings (), projection);
}

void
TraitItemReference::resolve_item (const TraitReference *tref,
				  HIR::TraitItemConst &constant)
{
  TyTy::BaseType *ty = nullptr;
  if (constant.has_type ())
    ty = TypeCheckType::Resolve (constant.get_type ());

  TyTy::BaseType *expr = nullptr;
  if (constant.has_expr ())
    expr = TypeCheckExpr::Resolve (constant.get_expr ());

  bool have_specified_ty = ty != nullptr && !ty->is<TyTy::ErrorType> ();
  bool have_expr_ty = expr != nullptr && !expr->is<TyTy::ErrorType> ();

  if (have_specified_ty && have_expr_ty)
    {
      coercion_site (constant.get_mappings ().get_hirid (),
		     TyTy::TyWithLocation (ty,
					   constant.get_type ().get_locus ()),
		     TyTy::TyWithLocation (expr,
					   constant.get_expr ().get_locus ()),
		     constant.get_locus ());
    }
}

void
TraitItemReference::resolve_item (const TraitReference *tref,
				  HIR::TraitItemFunc &func)
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
  context->clear_function_body_pending (resolved_fn_type->get_id ());
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

TyTy::SubstitutionArgumentMappings
AssociatedImplTrait::bind_impl_for_projection (TyTy::ProjectionType &proj,
					       location_t locus)
{
  std::vector<TyTy::SubstitutionParamMapping> impl_substitutions;
  for (auto &generic_param : impl->get_generic_params ())
    {
      if (generic_param->get_kind () != HIR::GenericParam::GenericKind::TYPE
	  && generic_param->get_kind ()
	       != HIR::GenericParam::GenericKind::CONST)
	continue;
      TyTy::BaseType *l = nullptr;
      bool ok
	= context->lookup_type (generic_param->get_mappings ().get_hirid (),
				&l);
      if (!ok)
	continue;

      TyTy::BaseGeneric *param = nullptr;
      if (l->get_kind () == TyTy::TypeKind::PARAM)
	param = static_cast<TyTy::ParamType *> (l);
      else if (l->get_kind () == TyTy::TypeKind::CONST
	       && l->as_const_type ()->const_kind ()
		    == TyTy::BaseConstType::ConstKind::Decl)
	param = static_cast<TyTy::ConstParamType *> (l);
      if (param != nullptr)
	impl_substitutions.emplace_back (*generic_param, param);
    }

  // Build infer args for each impl param so we dont mutate the impls own
  // ParamTys when unifying. param_mappings records impl_param_symbol ->
  // hirid_of_fresh_infer_var so we can read out what each param resolved to
  // after unification.
  std::vector<TyTy::SubstitutionArg> infer_arg_vec;
  std::map<std::string, HirId> param_mappings;
  for (auto &p : impl_substitutions)
    {
      const std::string &symbol = p.get_param_ty ()->get_symbol ();
      TyTy::TyVar infer_var
	= p.get_generic_param ().get_kind ()
	      == HIR::GenericParam::GenericKind::CONST
	    ? TyTy::TyVar::get_implicit_const_infer_var (locus)
	    : TyTy::TyVar::get_implicit_infer_var (locus);
      TyTy::BaseType *resolved = infer_var.get_tyty ();
      infer_arg_vec.emplace_back (&p, resolved);
      param_mappings[symbol] = resolved->get_ref ();
    }
  TyTy::SubstitutionArgumentMappings infer_arguments (
    std::move (infer_arg_vec), {} /*binding_args*/,
    TyTy::RegionParamList (0) /*regions*/, locus);

  TyTy::BaseType *impl_self_infer
    = !self->is_concrete ()
	? SubstMapperInternal::Resolve (self->clone (), infer_arguments)
	: self->clone ();

  // sub the impls trait predicate args with infers:
  //   SliceIndex<[Y]> -> SliceIndex<[?Y]>
  const TyTy::TypeBoundPredicate &impl_predicate = predicate;
  std::vector<TyTy::BaseType *> impl_predicate_args;
  for (size_t i = 1; i < impl_predicate.get_substs ().size (); i++)
    {
      auto p = impl_predicate.get_substs ().at (i).get_param_ty ();
      TyTy::BaseType *r = p->resolve ();
      if (!r->is_concrete ())
	r = SubstMapperInternal::Resolve (r, infer_arguments);
      impl_predicate_args.push_back (r);
    }

  // Read the projections own (trait-coord) substs and skip Self at index 0.
  // The projection's substs match by symbol in handle_substitions, so a
  // fn-level substitution that binds T won't have walked into the
  // projections X slot whose resolved value is [T]. Re-apply the
  // projections own used_arguments to each arg so any stale impl-coord
  // params inside (the T inside [T]) are followed through to their
  // current bindings
  TyTy::SubstitutionArgumentMappings proj_used
    = proj.get_substitution_arguments ();
  std::vector<TyTy::BaseType *> proj_args;
  for (size_t i = 1; i < proj.get_substs ().size (); i++)
    {
      auto p = proj.get_substs ().at (i).get_param_ty ();
      TyTy::BaseType *r = p->resolve ();
      if (!r->is_concrete () && !proj_used.is_empty ())
	r = SubstMapperInternal::Resolve (r, proj_used);
      proj_args.push_back (r);
    }

  if (impl_predicate_args.size () != proj_args.size ())
    return TyTy::SubstitutionArgumentMappings::error ();

  // unify trait args [?Y] -> [{integer}]
  for (size_t i = 0; i < proj_args.size (); i++)
    {
      TyTy::BaseType *proj_arg = proj_args[i];
      if (impl_predicate_args[i]->get_kind () == TyTy::TypeKind::SLICE
	  && proj_arg->get_kind () != TyTy::TypeKind::SLICE)
	{
	  auto &mappings = Analysis::Mappings::get ();
	  HirId id = mappings.get_next_hir_id ();
	  auto *wrapped
	    = new TyTy::SliceType (id, proj_arg->get_locus (),
				   TyTy::TyVar (proj_arg->get_ref ()));
	  context->insert_implicit_type (id, wrapped);
	  proj_arg = wrapped;
	}

      auto *res = unify_site_and (0 /*id*/,
				  TyTy::TyWithLocation (impl_predicate_args[i]),
				  TyTy::TyWithLocation (proj_arg), locus,
				  false /*emit_errors*/, true /*commit_if_ok*/,
				  true /*infer*/, true /*cleanup_on_fail*/,
				  false /*check_bounds*/);
      if (res->get_kind () == TyTy::TypeKind::ERROR)
	return TyTy::SubstitutionArgumentMappings::error ();
    }

  // unify self Range<{integer}> -> Range<usize>
  auto *res = unify_site_and (impl->get_mappings ().get_hirid (),
			      TyTy::TyWithLocation (proj.get_self ()),
			      TyTy::TyWithLocation (impl_self_infer), locus,
			      false /*emit_errors*/, true /*commit_if_ok*/,
			      true /*infer*/, true /*cleanup_on_fail*/,
			      false /*check_bounds*/);
  if (res->get_kind () == TyTy::TypeKind::ERROR)
    return TyTy::SubstitutionArgumentMappings::error ();

  std::vector<TyTy::SubstitutionArg> resolved_args;
  for (auto &p : impl_substitutions)
    {
      const std::string &symbol = p.get_param_ty ()->get_symbol ();
      auto it = param_mappings.find (symbol);
      if (it == param_mappings.end ())
	continue;
      TyTy::BaseType *r = nullptr;
      bool ok = context->lookup_type (it->second, &r);
      if (!ok)
	continue;
      resolved_args.emplace_back (&p, r);
    }

  if (!validate_impl_substitution_bounds (resolved_args, locus,
					  false /*emit_error*/))
    return TyTy::SubstitutionArgumentMappings::error ();

  return TyTy::SubstitutionArgumentMappings (std::move (resolved_args),
					     {} /*binding_args*/,
					     TyTy::RegionParamList (0)
					     /*regions*/,
					     locus);
}

TyTy::SubstitutionArgumentMappings
AssociatedImplTrait::bind_impl_for_bound (TyTy::BaseType *receiver,
					  const TyTy::TypeBoundPredicate &bound,
					  location_t locus, bool emit_error)
{
  // Same shape as bind_impl_for_projection but the receiver/trait-args are
  // taken from the (binding, bound) pair instead of a ProjectionType.
  std::vector<TyTy::SubstitutionParamMapping> impl_substitutions;
  for (auto &generic_param : impl->get_generic_params ())
    {
      if (generic_param->get_kind () != HIR::GenericParam::GenericKind::TYPE
	  && generic_param->get_kind ()
	       != HIR::GenericParam::GenericKind::CONST)
	continue;
      TyTy::BaseType *l = nullptr;
      bool ok
	= context->lookup_type (generic_param->get_mappings ().get_hirid (),
				&l);
      if (!ok)
	continue;

      TyTy::BaseGeneric *param = nullptr;
      if (l->get_kind () == TyTy::TypeKind::PARAM)
	param = static_cast<TyTy::ParamType *> (l);
      else if (l->get_kind () == TyTy::TypeKind::CONST
	       && l->as_const_type ()->const_kind ()
		    == TyTy::BaseConstType::ConstKind::Decl)
	param = static_cast<TyTy::ConstParamType *> (l);
      if (param != nullptr)
	impl_substitutions.emplace_back (*generic_param, param);
    }

  std::vector<TyTy::SubstitutionArg> infer_arg_vec;
  std::map<std::string, HirId> param_mappings;
  for (auto &p : impl_substitutions)
    {
      const std::string &symbol = p.get_param_ty ()->get_symbol ();
      TyTy::TyVar infer_var
	= p.get_generic_param ().get_kind ()
	      == HIR::GenericParam::GenericKind::CONST
	    ? TyTy::TyVar::get_implicit_const_infer_var (locus)
	    : TyTy::TyVar::get_implicit_infer_var (locus);
      TyTy::BaseType *resolved = infer_var.get_tyty ();
      infer_arg_vec.emplace_back (&p, resolved);
      param_mappings[symbol] = resolved->get_ref ();
    }
  TyTy::SubstitutionArgumentMappings infer_arguments (
    std::move (infer_arg_vec), {} /*binding_args*/,
    TyTy::RegionParamList (0) /*regions*/, locus);

  TyTy::BaseType *impl_self_infer
    = !self->is_concrete ()
	? SubstMapperInternal::Resolve (self->clone (), infer_arguments)
	: self->clone ();

  const TyTy::TypeBoundPredicate &impl_predicate = predicate;
  std::vector<TyTy::BaseType *> impl_predicate_args;
  for (size_t i = 1; i < impl_predicate.get_substs ().size (); i++)
    {
      auto p = impl_predicate.get_substs ().at (i).get_param_ty ();
      TyTy::BaseType *r = p->resolve ();
      if (!r->is_concrete ())
	r = SubstMapperInternal::Resolve (r, infer_arguments);
      impl_predicate_args.push_back (r);
    }

  // Trait args from the bound predicate the caller passed in (skip Self).
  // Apply the bound's own used_arguments so any stale impl-coord params
  // inside follow through to their current bindings.
  TyTy::SubstitutionArgumentMappings bound_used
    = bound.get_substitution_arguments ();
  std::vector<TyTy::BaseType *> bound_args;
  for (size_t i = 1; i < bound.get_substs ().size (); i++)
    {
      auto p = bound.get_substs ().at (i).get_param_ty ();
      TyTy::BaseType *r = p->resolve ();
      if (!r->is_concrete () && !bound_used.is_empty ())
	r = SubstMapperInternal::Resolve (r, bound_used);
      bound_args.push_back (r);
    }

  if (impl_predicate_args.size () != bound_args.size ())
    return TyTy::SubstitutionArgumentMappings::error ();

  for (size_t i = 0; i < bound_args.size (); i++)
    {
      auto *res = unify_site_and (0 /*id*/,
				  TyTy::TyWithLocation (impl_predicate_args[i]),
				  TyTy::TyWithLocation (bound_args[i]), locus,
				  false /*emit_errors*/, true /*commit_if_ok*/,
				  true /*infer*/, true /*cleanup_on_fail*/,
				  false /*check_bounds*/);
      if (res->get_kind () == TyTy::TypeKind::ERROR)
	return TyTy::SubstitutionArgumentMappings::error ();
    }

  auto *res = unify_site_and (impl->get_mappings ().get_hirid (),
			      TyTy::TyWithLocation (receiver),
			      TyTy::TyWithLocation (impl_self_infer), locus,
			      false /*emit_errors*/, true /*commit_if_ok*/,
			      true /*infer*/, true /*cleanup_on_fail*/,
			      false /*check_bounds*/);
  if (res->get_kind () == TyTy::TypeKind::ERROR)
    return TyTy::SubstitutionArgumentMappings::error ();

  std::vector<TyTy::SubstitutionArg> resolved_args;
  for (auto &p : impl_substitutions)
    {
      const std::string &symbol = p.get_param_ty ()->get_symbol ();
      auto it = param_mappings.find (symbol);
      if (it == param_mappings.end ())
	continue;
      TyTy::BaseType *r = nullptr;
      bool ok = context->lookup_type (it->second, &r);
      if (!ok)
	continue;
      resolved_args.emplace_back (&p, r);
    }

  if (!validate_impl_substitution_bounds (resolved_args, locus, emit_error))
    return TyTy::SubstitutionArgumentMappings::error ();

  return TyTy::SubstitutionArgumentMappings (std::move (resolved_args),
					     {} /*binding_args*/,
					     TyTy::RegionParamList (0)
					     /*regions*/,
					     locus);
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
    case TraitItemReference::TraitItemType::FN:
      {
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
