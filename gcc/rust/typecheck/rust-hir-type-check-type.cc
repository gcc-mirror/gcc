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

#include "rust-hir-type-check-type.h"
#include "rust-hir-trait-resolve.h"

namespace Rust {
namespace Resolver {

void
TypeCheckType::visit (HIR::TypePath &path)
{
  // lookup the Node this resolves to
  NodeId ref;
  auto nid = path.get_mappings ().get_nodeid ();
  if (!resolver->lookup_resolved_type (nid, &ref))
    {
      rust_fatal_error (path.get_locus (), "failed to resolve node '%d' to HIR",
			nid);
      return;
    }

  HirId hir_lookup;
  if (!context->lookup_type_by_node_id (ref, &hir_lookup))
    {
      rust_error_at (path.get_locus (), "failed to lookup HIR %d for node '%s'",
		     ref, path.as_string ().c_str ());
      return;
    }

  TyTy::BaseType *lookup = nullptr;
  if (!context->lookup_type (hir_lookup, &lookup))
    {
      rust_error_at (path.get_locus (), "failed to lookup HIR TyTy");
      return;
    }

  TyTy::BaseType *path_type = lookup->clone ();
  path_type->set_ref (path.get_mappings ().get_hirid ());

  HIR::TypePathSegment *final_seg = path.get_final_segment ().get ();
  HIR::GenericArgs args = TypeCheckResolveGenericArguments::resolve (final_seg);

  bool is_big_self = final_seg->is_ident_only ()
		     && (final_seg->as_string ().compare ("Self") == 0);

  if (path_type->needs_generic_substitutions ())
    {
      if (is_big_self)
	{
	  translated = path_type;
	  return;
	}

      translated = SubstMapper::Resolve (path_type, path.get_locus (), &args);
      if (translated->get_kind () != TyTy::TypeKind::ERROR
	  && mappings != nullptr)
	{
	  check_for_unconstrained (args.get_type_args ());
	}
    }
  else if (!args.is_empty ())
    {
      rust_error_at (path.get_locus (),
		     "TypePath %s declares generic arguments but "
		     "the type %s does not have any",
		     path.as_string ().c_str (),
		     translated->as_string ().c_str ());
    }
  else
    {
      translated = path_type;
    }
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

      resolve_segments (root_resolved_node_id, path.get_segments (), 0,
			translated, path.get_mappings (), path.get_locus ());
    }

  // Resolve the trait now
  TraitReference *trait_ref
    = TraitResolver::Resolve (*qual_path_type.get_trait ().get ());
  if (trait_ref->is_error ())
    return;

  // does this type actually implement this type-bound?
  if (!TypeBoundsProbe::is_bound_satisfied_for_type (root, trait_ref))
    return;

  // we need resolve to the impl block
  NodeId impl_resolved_id = UNKNOWN_NODEID;
  bool ok = resolver->lookup_resolved_name (
    qual_path_type.get_mappings ().get_nodeid (), &impl_resolved_id);
  rust_assert (ok);

  HirId impl_block_id;
  ok = mappings->lookup_node_to_hir (path.get_mappings ().get_crate_num (),
				     impl_resolved_id, &impl_block_id);
  rust_assert (ok);

  AssociatedImplTrait *lookup_associated = nullptr;
  bool found_impl_trait
    = context->lookup_associated_trait_impl (impl_block_id, &lookup_associated);
  rust_assert (found_impl_trait);

  std::unique_ptr<HIR::TypePathSegment> &item_seg
    = path.get_associated_segment ();

  const TraitItemReference *trait_item_ref = nullptr;
  ok
    = trait_ref->lookup_trait_item (item_seg->get_ident_segment ().as_string (),
				    &trait_item_ref);
  if (!ok)
    {
      rust_error_at (item_seg->get_locus (), "unknown associated item");
      return;
    }

  // project
  lookup_associated->setup_associated_types ();

  HIR::GenericArgs trait_generics = qual_path_type.trait_has_generic_args ()
				      ? qual_path_type.get_trait_generic_args ()
				      : HIR::GenericArgs::create_empty ();

  translated = lookup_associated->get_projected_type (
    trait_item_ref, root, item_seg->get_mappings ().get_hirid (),
    trait_generics, item_seg->get_locus ());

  if (translated->get_kind () == TyTy::TypeKind::PLACEHOLDER)
    {
      // lets grab the actual projection type
      TyTy::PlaceholderType *p
	= static_cast<TyTy::PlaceholderType *> (translated);
      if (p->can_resolve ())
	{
	  translated = p->resolve ();
	}
    }

  if (item_seg->get_type () == HIR::TypePathSegment::SegmentType::GENERIC)
    {
      HIR::TypePathSegmentGeneric &generic_seg
	= static_cast<HIR::TypePathSegmentGeneric &> (*item_seg.get ());

      // turbo-fish segment path::<ty>
      if (generic_seg.has_generic_args ())
	{
	  if (!translated->can_substitute ())
	    {
	      rust_error_at (item_seg->get_locus (),
			     "substitutions not supported for %s",
			     translated->as_string ().c_str ());
	      translated
		= new TyTy::ErrorType (path.get_mappings ().get_hirid ());
	      return;
	    }
	  translated = SubstMapper::Resolve (translated, path.get_locus (),
					     &generic_seg.get_generic_args ());
	}
    }

  // continue on as a path-in-expression
  NodeId root_resolved_node_id = trait_item_ref->get_mappings ().get_nodeid ();
  bool fully_resolved = path.get_segments ().empty ();
  if (fully_resolved)
    {
      resolver->insert_resolved_name (path.get_mappings ().get_nodeid (),
				      root_resolved_node_id);
      context->insert_receiver (path.get_mappings ().get_hirid (), root);
      return;
    }

  resolve_segments (root_resolved_node_id, path.get_segments (), 0, translated,
		    path.get_mappings (), path.get_locus ());
}

void
TypeCheckType::resolve_segments (
  NodeId root_resolved_node_id,
  std::vector<std::unique_ptr<HIR::TypePathSegment>> &segments, size_t offset,
  TyTy::BaseType *tyseg, const Analysis::NodeMapping &expr_mappings,
  Location expr_locus)
{
  gcc_unreachable ();
}

void
TypeCheckType::visit (HIR::TraitObjectTypeOneBound &type)
{
  std::vector<TyTy::TypeBoundPredicate> specified_bounds;

  HIR::TraitBound &trait_bound = type.get_trait_bound ();
  TraitReference *trait = resolve_trait_path (trait_bound.get_path ());
  TyTy::TypeBoundPredicate predicate (trait->get_mappings ().get_defid (),
				      trait_bound.get_locus ());

  specified_bounds.push_back (std::move (predicate));

  translated = new TyTy::DynamicObjectType (type.get_mappings ().get_hirid (),
					    std::move (specified_bounds));
}

} // namespace Resolver
} // namespace Rust
