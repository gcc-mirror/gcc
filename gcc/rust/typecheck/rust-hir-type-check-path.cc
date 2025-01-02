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

#include "rust-hir-type-check-expr.h"
#include "rust-hir-type-check-type.h"
#include "rust-hir-type-check-item.h"
#include "rust-hir-trait-resolve.h"
#include "rust-substitution-mapper.h"
#include "rust-hir-path-probe.h"
#include "rust-type-util.h"
#include "rust-hir-type-bounds.h"
#include "rust-session-manager.h"
#include "rust-immutable-name-resolution-context.h"

namespace Rust {
namespace Resolver {

void
TypeCheckExpr::visit (HIR::QualifiedPathInExpression &expr)
{
  HIR::QualifiedPathType qual_path_type = expr.get_path_type ();
  TyTy::BaseType *root
    = TypeCheckType::Resolve (qual_path_type.get_type ().get ());
  if (root->get_kind () == TyTy::TypeKind::ERROR)
    return;

  if (!qual_path_type.has_as_clause ())
    {
      NodeId root_resolved_node_id = UNKNOWN_NODEID;
      resolve_segments (root_resolved_node_id, expr.get_segments (), 0, root,
			expr.get_mappings (), expr.get_locus ());
      return;
    }

  // Resolve the trait now
  std::unique_ptr<HIR::TypePath> &trait_path_ref = qual_path_type.get_trait ();
  TraitReference *trait_ref = TraitResolver::Resolve (*trait_path_ref.get ());
  if (trait_ref->is_error ())
    return;

  // does this type actually implement this type-bound?
  if (!TypeBoundsProbe::is_bound_satisfied_for_type (root, trait_ref))
    return;

  // then we need to look at the next segment to create perform the correct
  // projection type
  if (expr.get_segments ().empty ())
    return;

  // get the predicate for the bound
  auto specified_bound
    = get_predicate_from_bound (*trait_path_ref.get (),
				qual_path_type.get_type ().get ());
  if (specified_bound.is_error ())
    return;

  // inherit the bound
  root->inherit_bounds ({specified_bound});

  // lookup the associated item from the specified bound
  HIR::PathExprSegment &item_seg = expr.get_segments ().at (0);
  HIR::PathIdentSegment item_seg_identifier = item_seg.get_segment ();
  TyTy::TypeBoundPredicateItem item
    = specified_bound.lookup_associated_item (item_seg_identifier.as_string ());
  if (item.is_error ())
    {
      rust_error_at (item_seg.get_locus (), "unknown associated item");
      return;
    }

  // we try to look for the real impl item if possible
  HIR::ImplItem *impl_item = nullptr;

  // lookup the associated impl trait for this if we can (it might be generic)
  AssociatedImplTrait *associated_impl_trait
    = lookup_associated_impl_block (specified_bound, root);
  if (associated_impl_trait != nullptr)
    {
      associated_impl_trait->setup_associated_types (root, specified_bound);

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

  NodeId root_resolved_node_id = UNKNOWN_NODEID;
  if (impl_item == nullptr)
    {
      // this may be valid as there could be a default trait implementation here
      // and we dont need to worry if the trait item is actually implemented or
      // not because this will have already been validated as part of the trait
      // impl block
      infered = item.get_tyty_for_receiver (root);
      root_resolved_node_id
	= item.get_raw_item ()->get_mappings ().get_nodeid ();
    }
  else
    {
      HirId impl_item_id = impl_item->get_impl_mappings ().get_hirid ();
      bool ok = query_type (impl_item_id, &infered);
      if (!ok)
	{
	  // FIXME
	  // I think query_type should error if required here anyway
	  return;
	}

      root_resolved_node_id = impl_item->get_impl_mappings ().get_nodeid ();
    }

  // turbo-fish segment path::<ty>
  if (item_seg.has_generic_args ())
    {
      if (!infered->has_substitutions_defined ())
	{
	  rust_error_at (item_seg.get_locus (),
			 "substitutions not supported for %s",
			 infered->as_string ().c_str ());
	  infered = new TyTy::ErrorType (expr.get_mappings ().get_hirid ());
	  return;
	}
      std::vector<TyTy::Region> regions;

      infered = SubstMapper::Resolve (infered, expr.get_locus (),
				      &item_seg.get_generic_args (),
				      context->regions_from_generic_args (
					item_seg.get_generic_args ()));
    }

  // continue on as a path-in-expression
  bool fully_resolved = expr.get_segments ().size () <= 1;
  if (fully_resolved)
    {
      resolver->insert_resolved_name (expr.get_mappings ().get_nodeid (),
				      root_resolved_node_id);
      context->insert_receiver (expr.get_mappings ().get_hirid (), root);
      return;
    }

  resolve_segments (root_resolved_node_id, expr.get_segments (), 1, infered,
		    expr.get_mappings (), expr.get_locus ());
}

void
TypeCheckExpr::visit (HIR::PathInExpression &expr)
{
  NodeId resolved_node_id = UNKNOWN_NODEID;
  size_t offset = -1;
  TyTy::BaseType *tyseg = resolve_root_path (expr, &offset, &resolved_node_id);
  if (tyseg->get_kind () == TyTy::TypeKind::ERROR)
    return;

  bool fully_resolved = offset == expr.get_segments ().size ();
  if (fully_resolved)
    {
      infered = tyseg;
      return;
    }

  resolve_segments (resolved_node_id, expr.get_segments (), offset, tyseg,
		    expr.get_mappings (), expr.get_locus ());
}

TyTy::BaseType *
TypeCheckExpr::resolve_root_path (HIR::PathInExpression &expr, size_t *offset,
				  NodeId *root_resolved_node_id)
{
  TyTy::BaseType *root_tyty = nullptr;
  *offset = 0;
  for (size_t i = 0; i < expr.get_num_segments (); i++)
    {
      HIR::PathExprSegment &seg = expr.get_segments ().at (i);

      bool have_more_segments = (expr.get_num_segments () - 1 != i);
      bool is_root = *offset == 0;
      NodeId ast_node_id = seg.get_mappings ().get_nodeid ();

      // then lookup the reference_node_id
      NodeId ref_node_id = UNKNOWN_NODEID;

      if (flag_name_resolution_2_0)
	{
	  auto nr_ctx
	    = Resolver2_0::ImmutableNameResolutionContext::get ().resolver ();

	  // assign the ref_node_id if we've found something
	  nr_ctx.lookup (expr.get_mappings ().get_nodeid ())
	    .map ([&ref_node_id] (NodeId resolved) { ref_node_id = resolved; });
	}
      else if (!resolver->lookup_resolved_name (ast_node_id, &ref_node_id))
	resolver->lookup_resolved_type (ast_node_id, &ref_node_id);

      // ref_node_id is the NodeId that the segments refers to.
      if (ref_node_id == UNKNOWN_NODEID)
	{
	  if (root_tyty != nullptr && *offset > 0)
	    {
	      // then we can let the impl path probe take over now
	      return root_tyty;
	    }

	  rust_error_at (seg.get_locus (),
			 "failed to type resolve root segment");
	  return new TyTy::ErrorType (expr.get_mappings ().get_hirid ());
	}

      // node back to HIR
      HirId ref;
      if (!mappings->lookup_node_to_hir (ref_node_id, &ref))
	{
	  rust_error_at (seg.get_locus (), "456 reverse lookup failure");
	  rust_debug_loc (seg.get_locus (),
			  "failure with [%s] mappings [%s] ref_node_id [%u]",
			  seg.as_string ().c_str (),
			  seg.get_mappings ().as_string ().c_str (),
			  ref_node_id);

	  return new TyTy::ErrorType (expr.get_mappings ().get_hirid ());
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
	  rust_error_at (seg.get_locus (), "expected value");
	  return new TyTy::ErrorType (expr.get_mappings ().get_hirid ());
	}

      TyTy::BaseType *lookup = nullptr;
      if (!query_type (ref, &lookup))
	{
	  if (is_root)
	    {
	      rust_error_at (expr.get_locus (), ErrorCode::E0425,
			     "cannot find value %qs in this scope",
			     expr.as_simple_path ().as_string ().c_str ());

	      return new TyTy::ErrorType (expr.get_mappings ().get_hirid ());
	    }
	  return root_tyty;
	}

      // is it an enum item?
      std::pair<HIR::Enum *, HIR::EnumItem *> enum_item_lookup
	= mappings->lookup_hir_enumitem (ref);
      bool is_enum_item = enum_item_lookup.first != nullptr
			  && enum_item_lookup.second != nullptr;
      if (is_enum_item)
	{
	  HirId expr_id = expr.get_mappings ().get_hirid ();
	  HirId variant_id
	    = enum_item_lookup.second->get_mappings ().get_hirid ();
	  context->insert_variant_definition (expr_id, variant_id);
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
      if (seg.has_generic_args ())
	{
	  lookup = SubstMapper::Resolve (lookup, expr.get_locus (),
					 &seg.get_generic_args (),
					 context->regions_from_generic_args (
					   seg.get_generic_args ()));
	  if (lookup->get_kind () == TyTy::TypeKind::ERROR)
	    return new TyTy::ErrorType (expr.get_mappings ().get_hirid ());
	}
      else if (lookup->needs_generic_substitutions ())
	{
	  lookup = SubstMapper::InferSubst (lookup, expr.get_locus ());
	}

      *root_resolved_node_id = ref_node_id;
      *offset = *offset + 1;
      root_tyty = lookup;
    }

  return root_tyty;
}

void
TypeCheckExpr::resolve_segments (NodeId root_resolved_node_id,
				 std::vector<HIR::PathExprSegment> &segments,
				 size_t offset, TyTy::BaseType *tyseg,
				 const Analysis::NodeMapping &expr_mappings,
				 location_t expr_locus)
{
  NodeId resolved_node_id = root_resolved_node_id;
  TyTy::BaseType *prev_segment = tyseg;
  bool reciever_is_generic = prev_segment->get_kind () == TyTy::TypeKind::PARAM;

  for (size_t i = offset; i < segments.size (); i++)
    {
      HIR::PathExprSegment &seg = segments.at (i);
      bool probe_impls = !reciever_is_generic;

      // probe the path is done in two parts one where we search impls if no
      // candidate is found then we search extensions from traits
      auto candidates
	= PathProbeType::Probe (prev_segment, seg.get_segment (), probe_impls,
				false /*probe_bounds*/,
				true /*ignore_mandatory_trait_items*/);
      if (candidates.size () == 0)
	{
	  candidates
	    = PathProbeType::Probe (prev_segment, seg.get_segment (), false,
				    true /*probe_bounds*/,
				    false /*ignore_mandatory_trait_items*/);

	  if (candidates.size () == 0)
	    {
	      rust_error_at (
		seg.get_locus (),
		"failed to resolve path segment using an impl Probe");
	      return;
	    }
	}

      if (candidates.size () > 1)
	{
	  ReportMultipleCandidateError::Report (candidates, seg.get_segment (),
						seg.get_locus ());
	  return;
	}

      auto &candidate = *candidates.begin ();
      prev_segment = tyseg;
      tyseg = candidate.ty;

      HIR::ImplBlock *associated_impl_block = nullptr;
      if (candidate.is_enum_candidate ())
	{
	  const TyTy::VariantDef *variant = candidate.item.enum_field.variant;

	  HirId variant_id = variant->get_id ();
	  std::pair<HIR::Enum *, HIR::EnumItem *> enum_item_lookup
	    = mappings->lookup_hir_enumitem (variant_id);
	  bool enum_item_ok = enum_item_lookup.first != nullptr
			      && enum_item_lookup.second != nullptr;
	  rust_assert (enum_item_ok);

	  HIR::EnumItem *enum_item = enum_item_lookup.second;
	  resolved_node_id = enum_item->get_mappings ().get_nodeid ();

	  // insert the id of the variant we are resolved to
	  context->insert_variant_definition (expr_mappings.get_hirid (),
					      variant_id);
	}
      else if (candidate.is_impl_candidate ())
	{
	  resolved_node_id
	    = candidate.item.impl.impl_item->get_impl_mappings ().get_nodeid ();

	  associated_impl_block = candidate.item.impl.parent;
	}
      else
	{
	  resolved_node_id
	    = candidate.item.trait.item_ref->get_mappings ().get_nodeid ();

	  // lookup the associated-impl-trait
	  HIR::ImplBlock *impl = candidate.item.trait.impl;
	  if (impl != nullptr)
	    {
	      // get the associated impl block
	      associated_impl_block = impl;
	    }
	}

      if (associated_impl_block != nullptr)
	{
	  // associated types
	  HirId impl_block_id
	    = associated_impl_block->get_mappings ().get_hirid ();

	  AssociatedImplTrait *associated = nullptr;
	  bool found_impl_trait
	    = context->lookup_associated_trait_impl (impl_block_id,
						     &associated);

	  auto mappings = TyTy::SubstitutionArgumentMappings::error ();
	  TyTy::BaseType *impl_block_ty
	    = TypeCheckItem::ResolveImplBlockSelfWithInference (
	      *associated_impl_block, seg.get_locus (), &mappings);

	  // we need to apply the arguments to the segment type so they get
	  // unified properly
	  if (!mappings.is_error ())
	    tyseg = SubstMapperInternal::Resolve (tyseg, mappings);

	  prev_segment = unify_site (seg.get_mappings ().get_hirid (),
				     TyTy::TyWithLocation (prev_segment),
				     TyTy::TyWithLocation (impl_block_ty),
				     seg.get_locus ());
	  bool ok = prev_segment->get_kind () != TyTy::TypeKind::ERROR;
	  if (!ok)
	    return;

	  if (found_impl_trait)
	    {
	      // we need to setup with apropriate bounds
	      HIR::TypePath &bound_path
		= *associated->get_impl_block ()->get_trait_ref ().get ();
	      const auto &trait_ref = *TraitResolver::Resolve (bound_path);
	      rust_assert (!trait_ref.is_error ());

	      const auto &predicate
		= impl_block_ty->lookup_predicate (trait_ref.get_defid ());
	      if (!predicate.is_error ())
		impl_block_ty
		  = associated->setup_associated_types (prev_segment,
							predicate);
	    }
	}

      if (seg.has_generic_args ())
	{
	  rust_debug_loc (seg.get_locus (), "applying segment generics: %s",
			  tyseg->as_string ().c_str ());
	  tyseg
	    = SubstMapper::Resolve (tyseg, expr_locus, &seg.get_generic_args (),
				    context->regions_from_generic_args (
				      seg.get_generic_args ()));
	  if (tyseg->get_kind () == TyTy::TypeKind::ERROR)
	    return;
	}
      else if (tyseg->needs_generic_substitutions () && !reciever_is_generic)
	{
	  location_t locus = seg.get_locus ();
	  tyseg = SubstMapper::InferSubst (tyseg, locus);
	  if (tyseg->get_kind () == TyTy::TypeKind::ERROR)
	    return;
	}
    }

  rust_assert (resolved_node_id != UNKNOWN_NODEID);
  if (tyseg->needs_generic_substitutions () && !reciever_is_generic)
    {
      location_t locus = segments.back ().get_locus ();
      tyseg = SubstMapper::InferSubst (tyseg, locus);
      if (tyseg->get_kind () == TyTy::TypeKind::ERROR)
	return;
    }

  context->insert_receiver (expr_mappings.get_hirid (), prev_segment);

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
  else
    {
      resolver->insert_resolved_misc (expr_mappings.get_nodeid (),
				      resolved_node_id);
    }

  infered = tyseg;
}

} // namespace Resolver
} // namespace Rust
