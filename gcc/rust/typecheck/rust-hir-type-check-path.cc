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

#include "rust-hir-type-check-expr.h"

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
      // then this is just a normal path-in-expression
      NodeId root_resolved_node_id = UNKNOWN_NODEID;
      bool ok = resolver->lookup_resolved_type (
	qual_path_type.get_type ()->get_mappings ().get_nodeid (),
	&root_resolved_node_id);
      rust_assert (ok);

      resolve_segments (root_resolved_node_id, expr.get_segments (), 0, root,
			expr.get_mappings (), expr.get_locus ());
    }

  // Resolve the trait now
  TraitReference *trait_ref
    = TraitResolver::Resolve (*qual_path_type.get_trait ().get ());
  if (trait_ref->is_error ())
    return;

  // does this type actually implement this type-bound?
  if (!TypeBoundsProbe::is_bound_satisfied_for_type (root, trait_ref))
    return;

  // then we need to look at the next segment to create perform the correct
  // projection type
  if (expr.get_segments ().empty ())
    return;

  // we need resolve to the impl block
  NodeId impl_resolved_id = UNKNOWN_NODEID;
  bool ok = resolver->lookup_resolved_name (
    qual_path_type.get_mappings ().get_nodeid (), &impl_resolved_id);
  rust_assert (ok);

  HirId impl_block_id;
  ok = mappings->lookup_node_to_hir (expr.get_mappings ().get_crate_num (),
				     impl_resolved_id, &impl_block_id);
  rust_assert (ok);

  AssociatedImplTrait *lookup_associated = nullptr;
  bool found_impl_trait
    = context->lookup_associated_trait_impl (impl_block_id, &lookup_associated);
  rust_assert (found_impl_trait);

  DefId resolved_item_id = UNKNOWN_DEFID;
  HIR::PathExprSegment &item_seg = expr.get_segments ().at (0);

  const TraitItemReference *trait_item_ref = nullptr;
  ok = trait_ref->lookup_trait_item (item_seg.get_segment ().as_string (),
				     &trait_item_ref);
  if (!ok)
    {
      rust_error_at (item_seg.get_locus (), "unknown associated item");
      return;
    }
  resolved_item_id = trait_item_ref->get_mappings ().get_defid ();

  infered = lookup_associated->get_projected_type (
    trait_item_ref, root, item_seg.get_mappings ().get_hirid (),
    item_seg.get_locus ());

  // turbo-fish segment path::<ty>
  if (item_seg.has_generic_args ())
    {
      if (!infered->can_substitute ())
	{
	  rust_error_at (item_seg.get_locus (),
			 "substitutions not supported for %s",
			 infered->as_string ().c_str ());
	  infered = new TyTy::ErrorType (expr.get_mappings ().get_hirid ());
	  return;
	}
      infered = SubstMapper::Resolve (infered, expr.get_locus (),
				      &item_seg.get_generic_args ());
    }

  TyTy::ProjectionType *projection
    = new TyTy::ProjectionType (qual_path_type.get_mappings ().get_hirid (),
				TyTy::TyVar (root->get_ref ()), trait_ref,
				resolved_item_id, lookup_associated);
  context->insert_type (qual_path_type.get_mappings (), projection);

  // continue on as a path-in-expression
  NodeId root_resolved_node_id = trait_item_ref->get_mappings ().get_nodeid ();
  bool fully_resolved = expr.get_segments ().size () <= 1;

  if (fully_resolved)
    {
      // lookup if the name resolver was able to canonically resolve this or
      // not
      NodeId path_resolved_id = UNKNOWN_NODEID;
      if (resolver->lookup_resolved_name (expr.get_mappings ().get_nodeid (),
					  &path_resolved_id))
	{
	  rust_assert (path_resolved_id == root_resolved_node_id);
	}
      // check the type scope
      else if (resolver->lookup_resolved_type (
		 expr.get_mappings ().get_nodeid (), &path_resolved_id))
	{
	  rust_assert (path_resolved_id == root_resolved_node_id);
	}
      else
	{
	  resolver->insert_resolved_name (expr.get_mappings ().get_nodeid (),
					  root_resolved_node_id);
	}

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

  if (tyseg == nullptr)
    {
      rust_debug_loc (expr.get_locus (), "failed to resolve root_seg");
    }
  rust_assert (tyseg != nullptr);

  if (tyseg->get_kind () == TyTy::TypeKind::ERROR)
    return;

  if (expr.get_num_segments () == 1)
    {
      Location locus = expr.get_segments ().back ().get_locus ();

      bool is_big_self
	= expr.get_segments ().front ().get_segment ().as_string ().compare (
	    "Self")
	  == 0;
      if (!is_big_self && tyseg->needs_generic_substitutions ())
	{
	  tyseg = SubstMapper::InferSubst (tyseg, locus);
	}

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
      if (resolver->lookup_resolved_name (ast_node_id, &ref_node_id))
	{
	  // these ref_node_ids will resolve to a pattern declaration but we
	  // are interested in the definition that this refers to get the
	  // parent id
	  Definition def;
	  if (!resolver->lookup_definition (ref_node_id, &def))
	    {
	      rust_error_at (expr.get_locus (),
			     "unknown reference for resolved name");
	      return new TyTy::ErrorType (expr.get_mappings ().get_hirid ());
	    }
	  ref_node_id = def.parent;
	}
      else
	{
	  resolver->lookup_resolved_type (ast_node_id, &ref_node_id);
	}

      // ref_node_id is the NodeId that the segments refers to.
      if (ref_node_id == UNKNOWN_NODEID)
	{
	  if (is_root)
	    {
	      rust_error_at (seg.get_locus (),
			     "failed to type resolve root segment");
	      return new TyTy::ErrorType (expr.get_mappings ().get_hirid ());
	    }
	  return root_tyty;
	}

      // node back to HIR
      HirId ref;
      if (!mappings->lookup_node_to_hir (expr.get_mappings ().get_crate_num (),
					 ref_node_id, &ref))
	{
	  if (is_root)
	    {
	      rust_error_at (seg.get_locus (), "456 reverse lookup failure");
	      rust_debug_loc (
		seg.get_locus (),
		"failure with [%s] mappings [%s] ref_node_id [%u]",
		seg.as_string ().c_str (),
		seg.get_mappings ().as_string ().c_str (), ref_node_id);

	      return new TyTy::ErrorType (expr.get_mappings ().get_hirid ());
	    }

	  return root_tyty;
	}

      auto seg_is_module
	= (nullptr
	   != mappings->lookup_module (expr.get_mappings ().get_crate_num (),
				       ref));

      if (seg_is_module)
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
      if (!context->lookup_type (ref, &lookup))
	{
	  if (is_root)
	    {
	      rust_error_at (seg.get_locus (),
			     "failed to resolve root segment");
	      return new TyTy::ErrorType (expr.get_mappings ().get_hirid ());
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
      if (seg.has_generic_args ())
	{
	  if (!lookup->can_substitute ())
	    {
	      rust_error_at (seg.get_locus (),
			     "substitutions not supported for %s",
			     lookup->as_string ().c_str ());
	      return new TyTy::ErrorType (lookup->get_ref ());
	    }
	  lookup = SubstMapper::Resolve (lookup, expr.get_locus (),
					 &seg.get_generic_args ());
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
				 Location expr_locus)
{
  NodeId resolved_node_id = root_resolved_node_id;
  TyTy::BaseType *prev_segment = tyseg;
  for (size_t i = offset; i < segments.size (); i++)
    {
      HIR::PathExprSegment &seg = segments.at (i);

      bool reciever_is_generic
	= prev_segment->get_kind () == TyTy::TypeKind::PARAM;
      bool probe_bounds = true;
      bool probe_impls = !reciever_is_generic;
      bool ignore_mandatory_trait_items = !reciever_is_generic;

      // probe the path
      auto candidates
	= PathProbeType::Probe (prev_segment, seg.get_segment (), probe_impls,
				probe_bounds, ignore_mandatory_trait_items);
      if (candidates.size () == 0)
	{
	  rust_error_at (seg.get_locus (),
			 "failed to resolve path segment using an impl Probe");
	  return;
	}
      else if (candidates.size () > 1)
	{
	  ReportMultipleCandidateError::Report (candidates, seg.get_segment (),
						seg.get_locus ());
	  return;
	}

      auto &candidate = candidates.at (0);
      prev_segment = tyseg;
      tyseg = candidate.ty;

      if (candidate.is_impl_candidate ())
	{
	  resolved_node_id
	    = candidate.item.impl.impl_item->get_impl_mappings ().get_nodeid ();
	}
      else
	{
	  resolved_node_id
	    = candidate.item.trait.item_ref->get_mappings ().get_nodeid ();

	  // lookup the associated-impl-trait
	  HIR::ImplBlock *impl = candidate.item.trait.impl;
	  if (impl != nullptr)
	    {
	      AssociatedImplTrait *lookup_associated = nullptr;
	      bool found_impl_trait = context->lookup_associated_trait_impl (
		impl->get_mappings ().get_hirid (), &lookup_associated);
	      rust_assert (found_impl_trait);

	      lookup_associated->setup_associated_types ();

	      // we need a new ty_ref_id for this trait item
	      tyseg = tyseg->clone ();
	      tyseg->set_ty_ref (mappings->get_next_hir_id ());
	    }
	}

      if (seg.has_generic_args ())
	{
	  if (!tyseg->can_substitute ())
	    {
	      rust_error_at (expr_locus, "substitutions not supported for %s",
			     tyseg->as_string ().c_str ());
	      return;
	    }

	  tyseg = SubstMapper::Resolve (tyseg, expr_locus,
					&seg.get_generic_args ());
	  if (tyseg->get_kind () == TyTy::TypeKind::ERROR)
	    return;
	}
    }

  context->insert_receiver (expr_mappings.get_hirid (), prev_segment);
  if (tyseg->needs_generic_substitutions ())
    {
      Location locus = segments.back ().get_locus ();
      if (!prev_segment->needs_generic_substitutions ())
	{
	  auto used_args_in_prev_segment
	    = GetUsedSubstArgs::From (prev_segment);
	  if (!used_args_in_prev_segment.is_error ())
	    tyseg
	      = SubstMapperInternal::Resolve (tyseg, used_args_in_prev_segment);
	}
      else
	{
	  tyseg = SubstMapper::InferSubst (tyseg, locus);
	}

      if (tyseg->get_kind () == TyTy::TypeKind::ERROR)
	return;
    }

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
      resolver->insert_resolved_name (expr_mappings.get_nodeid (),
				      resolved_node_id);
    }

  infered = tyseg;
}

} // namespace Resolver
} // namespace Rust
