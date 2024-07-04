// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

#include "rust-hir-type-check-type.h"
#include "rust-hir-trait-resolve.h"
#include "rust-hir-type-check-expr.h"
#include "rust-hir-path-probe.h"
#include "rust-hir-type-bounds.h"
#include "rust-substitution-mapper.h"
#include "rust-type-util.h"

namespace Rust {
namespace Resolver {

HIR::GenericArgs
TypeCheckResolveGenericArguments::resolve (HIR::TypePathSegment *segment)
{
  TypeCheckResolveGenericArguments resolver (segment->get_locus ());
  switch (segment->get_type ())
    {
    case HIR::TypePathSegment::SegmentType::GENERIC:
      resolver.visit (static_cast<HIR::TypePathSegmentGeneric &> (*segment));
      break;

    default:
      break;
    }
  return resolver.args;
}

void
TypeCheckResolveGenericArguments::visit (HIR::TypePathSegmentGeneric &generic)
{
  args = generic.get_generic_args ();
}

TyTy::BaseType *
TypeCheckType::Resolve (HIR::Type *type)
{
  // is it already resolved?
  auto context = TypeCheckContext::get ();
  TyTy::BaseType *resolved = nullptr;
  bool already_resolved
    = context->lookup_type (type->get_mappings ().get_hirid (), &resolved);
  if (already_resolved)
    return resolved;

  TypeCheckType resolver (type->get_mappings ().get_hirid ());
  type->accept_vis (resolver);
  rust_assert (resolver.translated != nullptr);
  resolver.context->insert_type (type->get_mappings (), resolver.translated);
  return resolver.translated;
}

void
TypeCheckType::visit (HIR::BareFunctionType &fntype)
{
  auto binder_pin = context->push_lifetime_binder ();
  for (auto &lifetime_param : fntype.get_for_lifetimes ())
    {
      context->intern_and_insert_lifetime (lifetime_param.get_lifetime ());
    }

  TyTy::BaseType *return_type;
  if (fntype.has_return_type ())
    {
      return_type = TypeCheckType::Resolve (fntype.get_return_type ().get ());
    }
  else
    {
      // needs a new implicit ID
      HirId ref = mappings->get_next_hir_id ();
      return_type = TyTy::TupleType::get_unit_type (ref);
      context->insert_implicit_type (ref, return_type);
    }

  std::vector<TyTy::TyVar> params;
  for (auto &param : fntype.get_function_params ())
    {
      TyTy::BaseType *ptype = TypeCheckType::Resolve (param.get_type ().get ());
      params.push_back (TyTy::TyVar (ptype->get_ref ()));
    }

  translated = new TyTy::FnPtr (fntype.get_mappings ().get_hirid (),
				fntype.get_locus (), std::move (params),
				TyTy::TyVar (return_type->get_ref ()));
}

void
TypeCheckType::visit (HIR::TupleType &tuple)
{
  if (tuple.is_unit_type ())
    {
      auto unit_node_id = resolver->get_unit_type_node_id ();
      if (!context->lookup_builtin (unit_node_id, &translated))
	{
	  rust_error_at (tuple.get_locus (),
			 "failed to lookup builtin unit type");
	}
      return;
    }

  std::vector<TyTy::TyVar> fields;
  for (auto &elem : tuple.get_elems ())
    {
      auto field_ty = TypeCheckType::Resolve (elem.get ());
      fields.push_back (TyTy::TyVar (field_ty->get_ref ()));
    }

  translated = new TyTy::TupleType (tuple.get_mappings ().get_hirid (),
				    tuple.get_locus (), fields);
}

void
TypeCheckType::visit (HIR::TypePath &path)
{
  // this can happen so we need to look up the root then resolve the
  // remaining segments if possible
  size_t offset = 0;
  NodeId resolved_node_id = UNKNOWN_NODEID;
  TyTy::BaseType *root = resolve_root_path (path, &offset, &resolved_node_id);
  if (root->get_kind () == TyTy::TypeKind::ERROR)
    return;

  TyTy::BaseType *path_type = root->clone ();
  path_type->set_ref (path.get_mappings ().get_hirid ());
  context->insert_implicit_type (path.get_mappings ().get_hirid (), path_type);

  bool fully_resolved = offset >= path.get_segments ().size ();
  if (fully_resolved)
    {
      translated = path_type;
      return;
    }

  translated
    = resolve_segments (resolved_node_id, path.get_mappings ().get_hirid (),
			path.get_segments (), offset, path_type,
			path.get_mappings (), path.get_locus ());
}

void
TypeCheckType::visit (HIR::QualifiedPathInType &path)
{
  HIR::QualifiedPathType qual_path_type = path.get_path_type ();
  TyTy::BaseType *root
    = TypeCheckType::Resolve (qual_path_type.get_type ().get ());
  if (root->get_kind () == TyTy::TypeKind::ERROR)
    {
      rust_debug_loc (path.get_locus (), "failed to resolve the root");
      return;
    }

  if (!qual_path_type.has_as_clause ())
    {
      // then this is just a normal path-in-expression
      NodeId root_resolved_node_id = UNKNOWN_NODEID;
      bool ok = resolver->lookup_resolved_type (
	qual_path_type.get_type ()->get_mappings ().get_nodeid (),
	&root_resolved_node_id);
      rust_assert (ok);

      translated = resolve_segments (root_resolved_node_id,
				     path.get_mappings ().get_hirid (),
				     path.get_segments (), 0, translated,
				     path.get_mappings (), path.get_locus ());

      return;
    }

  // Resolve the trait now
  std::unique_ptr<HIR::TypePath> &trait_path_ref = qual_path_type.get_trait ();
  TraitReference *trait_ref = TraitResolver::Resolve (*trait_path_ref.get ());
  if (trait_ref->is_error ())
    return;

  // does this type actually implement this type-bound?
  if (!TypeBoundsProbe::is_bound_satisfied_for_type (root, trait_ref))
    {
      rust_error_at (qual_path_type.get_locus (),
		     "root does not satisfy specified trait-bound");
      return;
    }

  // get the predicate for the bound
  auto specified_bound
    = get_predicate_from_bound (*qual_path_type.get_trait ().get (),
				qual_path_type.get_type ().get ());
  if (specified_bound.is_error ())
    return;

  // inherit the bound
  root->inherit_bounds ({specified_bound});

  // lookup the associated item from the specified bound
  std::unique_ptr<HIR::TypePathSegment> &item_seg
    = path.get_associated_segment ();
  HIR::PathIdentSegment item_seg_identifier = item_seg->get_ident_segment ();
  TyTy::TypeBoundPredicateItem item
    = specified_bound.lookup_associated_item (item_seg_identifier.as_string ());
  if (item.is_error ())
    {
      rust_error_at (item_seg->get_locus (), "unknown associated item");
      return;
    }

  // we try to look for the real impl item if possible
  TyTy::SubstitutionArgumentMappings args
    = TyTy::SubstitutionArgumentMappings::error ();
  HIR::ImplItem *impl_item = nullptr;
  if (root->is_concrete ())
    {
      // lookup the associated impl trait for this if we can (it might be
      // generic)
      AssociatedImplTrait *associated_impl_trait
	= lookup_associated_impl_block (specified_bound, root);
      if (associated_impl_trait != nullptr)
	{
	  associated_impl_trait->setup_associated_types (root, specified_bound,
							 &args);

	  for (auto &i :
	       associated_impl_trait->get_impl_block ()->get_impl_items ())
	    {
	      bool found = i->get_impl_item_name ().compare (
			     item_seg_identifier.as_string ())
			   == 0;
	      if (found)
		{
		  impl_item = i.get ();
		  break;
		}
	    }
	}
    }

  NodeId root_resolved_node_id = UNKNOWN_NODEID;
  if (impl_item == nullptr)
    {
      // this may be valid as there could be a default trait implementation here
      // and we dont need to worry if the trait item is actually implemented or
      // not because this will have already been validated as part of the trait
      // impl block
      translated = item.get_tyty_for_receiver (root);
      root_resolved_node_id
	= item.get_raw_item ()->get_mappings ().get_nodeid ();
    }
  else
    {
      HirId impl_item_id = impl_item->get_impl_mappings ().get_hirid ();
      bool ok = query_type (impl_item_id, &translated);
      if (!ok)
	{
	  // FIXME
	  // I think query_type should error if required here anyway
	  return;
	}

      if (!args.is_error ())
	{
	  // apply the args
	  translated = SubstMapperInternal::Resolve (translated, args);
	}

      root_resolved_node_id = impl_item->get_impl_mappings ().get_nodeid ();
    }

  // turbo-fish segment path::<ty>
  if (item_seg->get_type () == HIR::TypePathSegment::SegmentType::GENERIC)
    {
      HIR::TypePathSegmentGeneric &generic_seg
	= static_cast<HIR::TypePathSegmentGeneric &> (*item_seg.get ());

      // turbo-fish segment path::<ty>
      if (generic_seg.has_generic_args ())
	{
	  if (!translated->has_substitutions_defined ())
	    {
	      rust_error_at (item_seg->get_locus (),
			     "substitutions not supported for %s",
			     translated->as_string ().c_str ());
	      translated
		= new TyTy::ErrorType (path.get_mappings ().get_hirid ());
	      return;
	    }
	  translated
	    = SubstMapper::Resolve (translated, path.get_locus (),
				    &generic_seg.get_generic_args (),
				    context->regions_from_generic_args (
				      generic_seg.get_generic_args ()));
	}
    }

  // continue on as a path-in-expression
  bool fully_resolved = path.get_segments ().empty ();
  if (fully_resolved)
    {
      resolver->insert_resolved_type (path.get_mappings ().get_nodeid (),
				      root_resolved_node_id);
      context->insert_receiver (path.get_mappings ().get_hirid (), root);
      return;
    }

  translated
    = resolve_segments (root_resolved_node_id,
			path.get_mappings ().get_hirid (), path.get_segments (),
			0, translated, path.get_mappings (), path.get_locus ());
}

TyTy::BaseType *
TypeCheckType::resolve_root_path (HIR::TypePath &path, size_t *offset,
				  NodeId *root_resolved_node_id)
{
  TyTy::BaseType *root_tyty = nullptr;
  *offset = 0;
  for (size_t i = 0; i < path.get_num_segments (); i++)
    {
      std::unique_ptr<HIR::TypePathSegment> &seg = path.get_segments ().at (i);

      bool have_more_segments = (path.get_num_segments () - 1 != i);
      bool is_root = *offset == 0;
      NodeId ast_node_id = seg->get_mappings ().get_nodeid ();

      // then lookup the reference_node_id
      NodeId ref_node_id = UNKNOWN_NODEID;
      if (!resolver->lookup_resolved_name (ast_node_id, &ref_node_id))
	{
	  resolver->lookup_resolved_type (ast_node_id, &ref_node_id);
	}

      // ref_node_id is the NodeId that the segments refers to.
      if (ref_node_id == UNKNOWN_NODEID)
	{
	  if (is_root)
	    {
	      rust_error_at (seg->get_locus (),
			     "unknown reference for resolved name: %<%s%>",
			     seg->get_ident_segment ().as_string ().c_str ());
	      return new TyTy::ErrorType (path.get_mappings ().get_hirid ());
	    }
	  return root_tyty;
	}

      // node back to HIR
      HirId ref = UNKNOWN_HIRID;
      if (!mappings->lookup_node_to_hir (ref_node_id, &ref))
	{
	  if (is_root)
	    {
	      rust_error_at (seg->get_locus (), "789 reverse lookup failure");
	      rust_debug_loc (
		seg->get_locus (),
		"failure with [%s] mappings [%s] ref_node_id [%u]",
		seg->as_string ().c_str (),
		seg->get_mappings ().as_string ().c_str (), ref_node_id);

	      return new TyTy::ErrorType (path.get_mappings ().get_hirid ());
	    }

	  return root_tyty;
	}

      auto seg_is_module = (nullptr != mappings->lookup_module (ref));
      auto seg_is_crate = mappings->is_local_hirid_crate (ref);
      if (seg_is_module || seg_is_crate)
	{
	  // A::B::C::this_is_a_module::D::E::F
	  //          ^^^^^^^^^^^^^^^^
	  //          Currently handling this.
	  if (have_more_segments)
	    {
	      (*offset)++;
	      continue;
	    }

	  // In the case of :
	  // A::B::C::this_is_a_module
	  //          ^^^^^^^^^^^^^^^^
	  // This is an error, we are not expecting a module.
	  rust_error_at (seg->get_locus (), "expected value");
	  return new TyTy::ErrorType (path.get_mappings ().get_hirid ());
	}

      TyTy::BaseType *lookup = nullptr;
      if (!query_type (ref, &lookup))
	{
	  if (is_root)
	    {
	      rust_error_at (seg->get_locus (),
			     "failed to resolve root segment");
	      return new TyTy::ErrorType (path.get_mappings ().get_hirid ());
	    }
	  return root_tyty;
	}

      // if we have a previous segment type
      if (root_tyty != nullptr)
	{
	  // if this next segment needs substitution we must apply the
	  // previous type arguments
	  //
	  // such as: GenericStruct::<_>::new(123, 456)
	  if (lookup->needs_generic_substitutions ())
	    {
	      if (!root_tyty->needs_generic_substitutions ())
		{
		  auto used_args_in_prev_segment
		    = GetUsedSubstArgs::From (root_tyty);
		  lookup
		    = SubstMapperInternal::Resolve (lookup,
						    used_args_in_prev_segment);
		}
	    }
	}

      // turbo-fish segment path::<ty>
      if (seg->is_generic_segment ())
	{
	  HIR::TypePathSegmentGeneric *generic_segment
	    = static_cast<HIR::TypePathSegmentGeneric *> (seg.get ());

	  auto regions = context->regions_from_generic_args (
	    generic_segment->get_generic_args ());
	  lookup = SubstMapper::Resolve (lookup, path.get_locus (),
					 &generic_segment->get_generic_args (),
					 regions);
	  if (lookup->get_kind () == TyTy::TypeKind::ERROR)
	    return new TyTy::ErrorType (seg->get_mappings ().get_hirid ());
	}
      else if (lookup->needs_generic_substitutions ())
	{
	  HIR::GenericArgs empty
	    = HIR::GenericArgs::create_empty (path.get_locus ());
	  lookup
	    = SubstMapper::Resolve (lookup, path.get_locus (), &empty,
				    context->regions_from_generic_args (empty));
	}

      *root_resolved_node_id = ref_node_id;
      *offset = *offset + 1;
      root_tyty = lookup;

      // this enforces the proper get_segments checks to take place
      auto *maybe_adt = root_tyty->try_as<const TyTy::ADTType> ();
      if (maybe_adt && maybe_adt->is_enum ())
	return root_tyty;
    }

  return root_tyty;
}

TyTy::BaseType *
TypeCheckType::resolve_segments (
  NodeId root_resolved_node_id, HirId expr_id,
  std::vector<std::unique_ptr<HIR::TypePathSegment>> &segments, size_t offset,
  TyTy::BaseType *tyseg, const Analysis::NodeMapping &expr_mappings,
  location_t expr_locus)
{
  NodeId resolved_node_id = root_resolved_node_id;
  TyTy::BaseType *prev_segment = tyseg;
  for (size_t i = offset; i < segments.size (); i++)
    {
      std::unique_ptr<HIR::TypePathSegment> &seg = segments.at (i);

      bool reciever_is_generic
	= prev_segment->get_kind () == TyTy::TypeKind::PARAM;
      bool probe_bounds = true;
      bool probe_impls = !reciever_is_generic;
      bool ignore_mandatory_trait_items = !reciever_is_generic;

      // probe the path is done in two parts one where we search impls if no
      // candidate is found then we search extensions from traits
      auto candidates
	= PathProbeType::Probe (prev_segment, seg->get_ident_segment (),
				probe_impls, false,
				ignore_mandatory_trait_items);
      if (candidates.size () == 0)
	{
	  candidates
	    = PathProbeType::Probe (prev_segment, seg->get_ident_segment (),
				    false, probe_bounds,
				    ignore_mandatory_trait_items);

	  if (candidates.size () == 0)
	    {
	      rust_error_at (
		seg->get_locus (),
		"failed to resolve path segment using an impl Probe");
	      return new TyTy::ErrorType (expr_id);
	    }
	}

      if (candidates.size () > 1)
	{
	  ReportMultipleCandidateError::Report (candidates,
						seg->get_ident_segment (),
						seg->get_locus ());
	  return new TyTy::ErrorType (expr_id);
	}

      auto &candidate = *candidates.begin ();
      prev_segment = tyseg;
      tyseg = candidate.ty;

      if (candidate.is_enum_candidate ())
	{
	  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (tyseg);
	  auto last_variant = adt->get_variants ();
	  TyTy::VariantDef *variant = last_variant.back ();

	  rich_location richloc (line_table, seg->get_locus ());
	  richloc.add_fixit_replace ("not a type");

	  rust_error_at (richloc, ErrorCode::E0573,
			 "expected type, found variant of %<%s::%s%>",
			 adt->get_name ().c_str (),
			 variant->get_identifier ().c_str ());
	  return new TyTy::ErrorType (expr_id);
	}

      if (candidate.is_impl_candidate ())
	{
	  resolved_node_id
	    = candidate.item.impl.impl_item->get_impl_mappings ().get_nodeid ();
	}
      else
	{
	  resolved_node_id
	    = candidate.item.trait.item_ref->get_mappings ().get_nodeid ();
	}

      if (seg->is_generic_segment ())
	{
	  auto *generic_segment
	    = static_cast<HIR::TypePathSegmentGeneric *> (seg.get ());

	  std::vector<TyTy::Region> regions;
	  for (auto &lifetime :
	       generic_segment->get_generic_args ().get_lifetime_args ())
	    {
	      auto region = context->lookup_and_resolve_lifetime (lifetime);
	      if (!region.has_value ())
		{
		  rust_error_at (lifetime.get_locus (),
				 "failed to resolve lifetime");
		  return new TyTy::ErrorType (expr_id);
		}
	      regions.push_back (region.value ());
	    }

	  tyseg = SubstMapper::Resolve (tyseg, expr_locus,
					&generic_segment->get_generic_args (),
					regions);
	  if (tyseg->get_kind () == TyTy::TypeKind::ERROR)
	    return new TyTy::ErrorType (expr_id);
	}
    }

  context->insert_receiver (expr_mappings.get_hirid (), prev_segment);
  rust_assert (resolved_node_id != UNKNOWN_NODEID);

  // lookup if the name resolver was able to canonically resolve this or not
  NodeId path_resolved_id = UNKNOWN_NODEID;
  if (resolver->lookup_resolved_name (expr_mappings.get_nodeid (),
				      &path_resolved_id))
    {
      rust_assert (path_resolved_id == resolved_node_id);
    }
  // check the type scope
  else if (resolver->lookup_resolved_type (expr_mappings.get_nodeid (),
					   &path_resolved_id))
    {
      rust_assert (path_resolved_id == resolved_node_id);
    }
  else
    {
      // name scope first
      if (resolver->get_name_scope ().decl_was_declared_here (resolved_node_id))
	{
	  resolver->insert_resolved_name (expr_mappings.get_nodeid (),
					  resolved_node_id);
	}
      // check the type scope
      else if (resolver->get_type_scope ().decl_was_declared_here (
		 resolved_node_id))
	{
	  resolver->insert_resolved_type (expr_mappings.get_nodeid (),
					  resolved_node_id);
	}
    }

  return tyseg;
}

void
TypeCheckType::visit (HIR::TraitObjectType &type)
{
  std::vector<TyTy::TypeBoundPredicate> specified_bounds;
  for (auto &bound : type.get_type_param_bounds ())
    {
      if (bound->get_bound_type ()
	  != HIR::TypeParamBound::BoundType::TRAITBOUND)
	continue;

      HIR::TypeParamBound &b = *bound.get ();
      HIR::TraitBound &trait_bound = static_cast<HIR::TraitBound &> (b);

      auto binder_pin = context->push_lifetime_binder ();
      for (auto &lifetime_param : trait_bound.get_for_lifetimes ())
	{
	  context->intern_and_insert_lifetime (lifetime_param.get_lifetime ());
	}

      TyTy::TypeBoundPredicate predicate = get_predicate_from_bound (
	trait_bound.get_path (),
	nullptr /*this will setup a PLACEHOLDER for self*/);

      if (!predicate.is_error ()
	  && predicate.is_object_safe (true, type.get_locus ()))
	specified_bounds.push_back (std::move (predicate));
    }

  RustIdent ident{CanonicalPath::create_empty (), type.get_locus ()};
  translated
    = new TyTy::DynamicObjectType (type.get_mappings ().get_hirid (), ident,
				   std::move (specified_bounds));
}

void
TypeCheckType::visit (HIR::ArrayType &type)
{
  auto capacity_type = TypeCheckExpr::Resolve (type.get_size_expr ().get ());
  if (capacity_type->get_kind () == TyTy::TypeKind::ERROR)
    return;

  TyTy::BaseType *expected_ty = nullptr;
  bool ok = context->lookup_builtin ("usize", &expected_ty);
  rust_assert (ok);
  context->insert_type (type.get_size_expr ()->get_mappings (), expected_ty);

  unify_site (type.get_size_expr ()->get_mappings ().get_hirid (),
	      TyTy::TyWithLocation (expected_ty),
	      TyTy::TyWithLocation (capacity_type,
				    type.get_size_expr ()->get_locus ()),
	      type.get_size_expr ()->get_locus ());

  TyTy::BaseType *base
    = TypeCheckType::Resolve (type.get_element_type ().get ());
  translated = new TyTy::ArrayType (type.get_mappings ().get_hirid (),
				    type.get_locus (), *type.get_size_expr (),
				    TyTy::TyVar (base->get_ref ()));
}

void
TypeCheckType::visit (HIR::SliceType &type)
{
  TyTy::BaseType *base
    = TypeCheckType::Resolve (type.get_element_type ().get ());
  translated
    = new TyTy::SliceType (type.get_mappings ().get_hirid (), type.get_locus (),
			   TyTy::TyVar (base->get_ref ()));
}
void
TypeCheckType::visit (HIR::ReferenceType &type)
{
  TyTy::BaseType *base = TypeCheckType::Resolve (type.get_base_type ().get ());
  rust_assert (type.has_lifetime ());
  auto region = context->lookup_and_resolve_lifetime (type.get_lifetime ());
  if (!region.has_value ())
    {
      rust_error_at (type.get_locus (), "failed to resolve lifetime");
      translated = new TyTy::ErrorType (type.get_mappings ().get_hirid ());
      return;
    }
  translated = new TyTy::ReferenceType (type.get_mappings ().get_hirid (),
					TyTy::TyVar (base->get_ref ()),
					type.get_mut (), region.value ());
} // namespace Resolver

void
TypeCheckType::visit (HIR::RawPointerType &type)
{
  TyTy::BaseType *base = TypeCheckType::Resolve (type.get_base_type ().get ());
  translated
    = new TyTy::PointerType (type.get_mappings ().get_hirid (),
			     TyTy::TyVar (base->get_ref ()), type.get_mut ());
}

void
TypeCheckType::visit (HIR::InferredType &type)
{
  translated = new TyTy::InferType (type.get_mappings ().get_hirid (),
				    TyTy::InferType::InferTypeKind::GENERAL,
				    TyTy::InferType::TypeHint::Default (),
				    type.get_locus ());
}

void
TypeCheckType::visit (HIR::NeverType &type)
{
  TyTy::BaseType *lookup = nullptr;
  bool ok = context->lookup_builtin ("!", &lookup);
  rust_assert (ok);

  translated = lookup->clone ();
}

TyTy::ParamType *
TypeResolveGenericParam::Resolve (HIR::GenericParam *param, bool apply_sized)
{
  TypeResolveGenericParam resolver (apply_sized);
  switch (param->get_kind ())
    {
    case HIR::GenericParam::GenericKind::TYPE:
      resolver.visit (static_cast<HIR::TypeParam &> (*param));
      break;

    case HIR::GenericParam::GenericKind::CONST:
      resolver.visit (static_cast<HIR::ConstGenericParam &> (*param));
      break;

    case HIR::GenericParam::GenericKind::LIFETIME:
      resolver.visit (static_cast<HIR::LifetimeParam &> (*param));
      break;
    }
  return resolver.resolved;
}

void
TypeResolveGenericParam::visit (HIR::LifetimeParam &param)
{
  // nothing to do
}

void
TypeResolveGenericParam::visit (HIR::ConstGenericParam &param)
{
  // TODO
}

void
TypeResolveGenericParam::visit (HIR::TypeParam &param)
{
  if (param.has_type ())
    TypeCheckType::Resolve (param.get_type ().get ());

  HIR::Type *implicit_self_bound = nullptr;
  if (param.has_type_param_bounds ())
    {
      // We need two possible parameter types. One with no Bounds and one with
      // the bounds. the Self type for the bounds cannot itself contain the
      // bounds otherwise it will be a trait cycle
      HirId implicit_id = mappings->get_next_hir_id ();
      TyTy::ParamType *p
	= new TyTy::ParamType (param.get_type_representation ().as_string (),
			       param.get_locus (), implicit_id, param,
			       {} /*empty specified bounds*/);
      context->insert_implicit_type (implicit_id, p);

      // generate an implicit HIR Type we can apply to the predicate
      Analysis::NodeMapping mappings (param.get_mappings ().get_crate_num (),
				      param.get_mappings ().get_nodeid (),
				      implicit_id,
				      param.get_mappings ().get_local_defid ());
      implicit_self_bound
	= new HIR::TypePath (mappings, {}, BUILTINS_LOCATION, false);
    }

  std::map<DefId, std::vector<TyTy::TypeBoundPredicate>> predicates;

  // https://doc.rust-lang.org/std/marker/trait.Sized.html
  // All type parameters have an implicit bound of Sized. The special syntax
  // ?Sized can be used to remove this bound if it’s not appropriate.
  //
  // We can only do this when we are not resolving the implicit Self for Sized
  // itself
  rust_debug_loc (param.get_locus (), "apply_sized: %s",
		  apply_sized ? "true" : "false");
  if (apply_sized)
    {
      TyTy::TypeBoundPredicate sized_predicate
	= get_marker_predicate (Analysis::RustLangItem::ItemType::SIZED,
				param.get_locus ());

      predicates[sized_predicate.get_id ()] = {sized_predicate};
    }

  // resolve the bounds
  if (param.has_type_param_bounds ())
    {
      for (auto &bound : param.get_type_param_bounds ())
	{
	  switch (bound->get_bound_type ())
	    {
	      case HIR::TypeParamBound::BoundType::TRAITBOUND: {
		HIR::TraitBound *b
		  = static_cast<HIR::TraitBound *> (bound.get ());

		TyTy::TypeBoundPredicate predicate
		  = get_predicate_from_bound (b->get_path (),
					      implicit_self_bound,
					      b->get_polarity ());
		if (!predicate.is_error ())
		  {
		    switch (predicate.get_polarity ())
		      {
			case BoundPolarity::AntiBound: {
			  bool found = predicates.find (predicate.get_id ())
				       != predicates.end ();
			  if (found)
			    predicates.erase (predicate.get_id ());
			  else
			    {
			      // emit error message
			      rich_location r (line_table, b->get_locus ());
			      r.add_range (predicate.get ()->get_locus ());
			      rust_error_at (
				r, "antibound for %s is not applied here",
				predicate.get ()->get_name ().c_str ());
			    }
			}
			break;

			default: {
			  if (predicates.find (predicate.get_id ())
			      == predicates.end ())
			    {
			      predicates[predicate.get_id ()] = {};
			    }
			  predicates[predicate.get_id ()].push_back (predicate);
			}
			break;
		      }
		  }
	      }
	      break;

	    default:
	      break;
	    }
	}
    }

  // now to flat map the specified_bounds into the raw specified predicates
  std::vector<TyTy::TypeBoundPredicate> specified_bounds;
  for (auto it = predicates.begin (); it != predicates.end (); it++)
    {
      for (const auto &predicate : it->second)
	{
	  specified_bounds.push_back (predicate);
	}
    }

  resolved = new TyTy::ParamType (param.get_type_representation ().as_string (),
				  param.get_locus (),
				  param.get_mappings ().get_hirid (), param,
				  specified_bounds);
}

void
ResolveWhereClauseItem::Resolve (HIR::WhereClauseItem &item,
				 TyTy::RegionConstraints &region_constraints)
{
  ResolveWhereClauseItem resolver (region_constraints);

  auto binder_pin = resolver.context->push_lifetime_binder ();

  switch (item.get_item_type ())
    {
    case HIR::WhereClauseItem::LIFETIME:
      resolver.visit (static_cast<HIR::LifetimeWhereClauseItem &> (item));
      break;

    case HIR::WhereClauseItem::TYPE_BOUND:
      resolver.visit (static_cast<HIR::TypeBoundWhereClauseItem &> (item));
      break;
    }
}

void
ResolveWhereClauseItem::visit (HIR::LifetimeWhereClauseItem &item)
{
  auto lhs = context->lookup_and_resolve_lifetime (item.get_lifetime ());
  if (!lhs.has_value ())
    {
      rust_error_at (UNKNOWN_LOCATION, "failed to resolve lifetime");
    }
  for (auto &lifetime : item.get_lifetime_bounds ())
    {
      auto rhs_i = context->lookup_and_resolve_lifetime (lifetime);
      if (!rhs_i.has_value ())
	{
	  rust_error_at (UNKNOWN_LOCATION, "failed to resolve lifetime");
	}
      region_constraints.region_region.emplace_back (lhs.value (),
						     rhs_i.value ());
    }
}

void
ResolveWhereClauseItem::visit (HIR::TypeBoundWhereClauseItem &item)
{
  auto binder_pin = context->push_lifetime_binder ();
  if (item.has_for_lifetimes ())
    {
      for (auto &lifetime_param : item.get_for_lifetimes ())
	{
	  context->intern_and_insert_lifetime (lifetime_param.get_lifetime ());
	}
    }

  auto &binding_type_path = item.get_bound_type ();
  TyTy::BaseType *binding = TypeCheckType::Resolve (binding_type_path.get ());

  // FIXME double check there might be a trait cycle here see TypeParam handling

  std::vector<TyTy::TypeBoundPredicate> specified_bounds;
  for (auto &bound : item.get_type_param_bounds ())
    {
      switch (bound->get_bound_type ())
	{
	  case HIR::TypeParamBound::BoundType::TRAITBOUND: {
	    auto *b = static_cast<HIR::TraitBound *> (bound.get ());

	    TyTy::TypeBoundPredicate predicate
	      = get_predicate_from_bound (b->get_path (),
					  binding_type_path.get ());
	    if (!predicate.is_error ())
	      specified_bounds.push_back (std::move (predicate));
	  }
	  break;
	  case HIR::TypeParamBound::BoundType::LIFETIME: {
	    if (auto param = binding->try_as<TyTy::ParamType> ())
	      {
		auto *b = static_cast<HIR::Lifetime *> (bound.get ());
		auto region = context->lookup_and_resolve_lifetime (*b);
		if (!region.has_value ())
		  {
		    rust_error_at (UNKNOWN_LOCATION,
				   "failed to resolve lifetime");
		  }
		region_constraints.type_region.emplace_back (param,
							     region.value ());
	      }
	  }
	  break;

	default:
	  break;
	}
    }
  binding->inherit_bounds (specified_bounds);

  // When we apply these bounds we must lookup which type this binding
  // resolves to, as this is the type which will be used during resolution
  // of the block.
  NodeId ast_node_id = binding_type_path->get_mappings ().get_nodeid ();

  // then lookup the reference_node_id
  NodeId ref_node_id = UNKNOWN_NODEID;
  if (!resolver->lookup_resolved_type (ast_node_id, &ref_node_id))
    {
      // FIXME
      rust_error_at (UNDEF_LOCATION,
		     "Failed to lookup type reference for node: %s",
		     binding_type_path->as_string ().c_str ());
      return;
    }

  // node back to HIR
  HirId ref;
  if (!mappings->lookup_node_to_hir (ref_node_id, &ref))
    {
      // FIXME
      rust_error_at (UNDEF_LOCATION, "where-clause reverse lookup failure");
      return;
    }

  // the base reference for this name _must_ have a type set
  TyTy::BaseType *lookup;
  if (!context->lookup_type (ref, &lookup))
    {
      rust_error_at (mappings->lookup_location (ref),
		     "Failed to resolve where-clause binding type: %s",
		     binding_type_path->as_string ().c_str ());
      return;
    }

  // FIXME
  // rust_assert (binding->is_equal (*lookup));
  lookup->inherit_bounds (specified_bounds);
}

} // namespace Resolver
} // namespace Rust
