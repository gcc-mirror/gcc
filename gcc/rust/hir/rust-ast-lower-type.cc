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

#include "rust-ast-lower-type.h"

namespace Rust {
namespace HIR {

HIR::TypePath *
ASTLowerTypePath::translate (AST::TypePath &type)
{
  ASTLowerTypePath resolver;
  type.accept_vis (resolver);
  rust_assert (resolver.translated != nullptr);
  return resolver.translated;
}

void
ASTLowerTypePath::visit (AST::TypePathSegmentFunction &segment)
{
  auto crate_num = mappings->get_current_crate ();
  auto hirid = mappings->get_next_hir_id (crate_num);
  Analysis::NodeMapping mapping (crate_num, segment.get_node_id (), hirid,
				 UNKNOWN_LOCAL_DEFID);

  HIR::PathIdentSegment ident (segment.get_ident_segment ().as_string ());

  AST::TypePathFunction &fn = segment.get_type_path_function ();
  std::vector<std::unique_ptr<HIR::Type>> inputs;
  for (auto &param : fn.get_params ())
    {
      HIR::Type *hir_type = ASTLoweringType::translate (param.get ());
      inputs.push_back (std::unique_ptr<HIR::Type> (hir_type));
    }

  HIR::Type *result_type
    = fn.has_return_type ()
	? ASTLoweringType::translate (fn.get_return_type ().get ())
	: nullptr;

  HIR::TypePathFunction function_path (std::move (inputs),
				       std::unique_ptr<HIR::Type> (
					 result_type));

  translated_segment = new HIR::TypePathSegmentFunction (
    mapping, std::move (ident), segment.get_separating_scope_resolution (),
    std::move (function_path), segment.get_locus ());
}

void
ASTLowerTypePath::visit (AST::TypePathSegment &segment)
{
  auto crate_num = mappings->get_current_crate ();
  auto hirid = mappings->get_next_hir_id (crate_num);
  Analysis::NodeMapping mapping (crate_num, segment.get_node_id (), hirid,
				 UNKNOWN_LOCAL_DEFID);

  HIR::PathIdentSegment ident (segment.get_ident_segment ().as_string ());
  translated_segment
    = new HIR::TypePathSegment (std::move (mapping), ident,
				segment.get_separating_scope_resolution (),
				segment.get_locus ());
}

void
ASTLowerTypePath::visit (AST::TypePathSegmentGeneric &segment)
{
  std::vector<HIR::GenericArgsBinding> binding_args; // TODO

  std::string segment_name = segment.get_ident_segment ().as_string ();
  bool has_separating_scope_resolution
    = segment.get_separating_scope_resolution ();

  auto generic_args = lower_generic_args (segment.get_generic_args ());

  auto crate_num = mappings->get_current_crate ();
  auto hirid = mappings->get_next_hir_id (crate_num);
  Analysis::NodeMapping mapping (crate_num, segment.get_node_id (), hirid,
				 UNKNOWN_LOCAL_DEFID);

  translated_segment
    = new HIR::TypePathSegmentGeneric (std::move (mapping), segment_name,
				       has_separating_scope_resolution,
				       generic_args, segment.get_locus ());
}

void
ASTLowerTypePath::visit (AST::TypePath &path)
{
  std::vector<std::unique_ptr<HIR::TypePathSegment>> translated_segments;

  for (auto &seg : path.get_segments ())
    {
      translated_segment = nullptr;
      seg->accept_vis (*this);
      if (translated_segment == nullptr)
	{
	  rust_fatal_error (seg->get_locus (),
			    "failed to translate AST TypePathSegment");
	}
      translated_segments.push_back (
	std::unique_ptr<HIR::TypePathSegment> (translated_segment));
    }

  auto crate_num = mappings->get_current_crate ();
  auto hirid = mappings->get_next_hir_id (crate_num);
  Analysis::NodeMapping mapping (crate_num, path.get_node_id (), hirid,
				 mappings->get_next_localdef_id (crate_num));

  translated
    = new HIR::TypePath (std::move (mapping), std::move (translated_segments),
			 path.get_locus (),
			 path.has_opening_scope_resolution_op ());
}

HIR::QualifiedPathInType *
ASTLowerQualifiedPathInType::translate (AST::QualifiedPathInType &type)
{
  ASTLowerQualifiedPathInType resolver;
  type.accept_vis (resolver);
  rust_assert (resolver.translated != nullptr);
  return resolver.translated;
}

void
ASTLowerQualifiedPathInType::visit (AST::QualifiedPathInType &path)
{
  auto crate_num = mappings->get_current_crate ();
  auto hirid = mappings->get_next_hir_id (crate_num);
  Analysis::NodeMapping qual_mappings (
    crate_num, path.get_qualified_path_type ().get_node_id (), hirid,
    UNKNOWN_LOCAL_DEFID);

  HIR::Type *qual_type = ASTLoweringType::translate (
    path.get_qualified_path_type ().get_type ().get ());
  HIR::TypePath *qual_trait = ASTLowerTypePath::translate (
    path.get_qualified_path_type ().get_as_type_path ());

  HIR::QualifiedPathType qual_path_type (
    qual_mappings, std::unique_ptr<HIR::Type> (qual_type),
    std::unique_ptr<HIR::TypePath> (qual_trait),
    path.get_qualified_path_type ().get_locus ());

  translated_segment = nullptr;
  path.get_associated_segment ()->accept_vis (*this);
  if (translated_segment == nullptr)
    {
      rust_fatal_error (path.get_associated_segment ()->get_locus (),
			"failed to translate AST TypePathSegment");
      return;
    }
  std::unique_ptr<HIR::TypePathSegment> associated_segment (translated_segment);

  std::vector<std::unique_ptr<HIR::TypePathSegment>> translated_segments;
  for (auto &seg : path.get_segments ())
    {
      translated_segment = nullptr;
      seg->accept_vis (*this);
      if (translated_segment == nullptr)
	{
	  rust_fatal_error (seg->get_locus (),
			    "failed to translte AST TypePathSegment");
	}
      translated_segments.push_back (
	std::unique_ptr<HIR::TypePathSegment> (translated_segment));
    }

  Analysis::NodeMapping mapping (crate_num, path.get_node_id (), hirid,
				 mappings->get_next_localdef_id (crate_num));
  translated = new HIR::QualifiedPathInType (std::move (mapping),
					     std::move (qual_path_type),
					     std::move (associated_segment),
					     std::move (translated_segments),
					     path.get_locus ());
}

void
ASTLoweringType::visit (AST::TraitObjectTypeOneBound &type)
{
  std::vector<std::unique_ptr<HIR::TypeParamBound>> bounds;
  HIR::TypeParamBound *translated_bound
    = ASTLoweringTypeBounds::translate (&type.get_trait_bound ());
  bounds.push_back (std::unique_ptr<HIR::TypeParamBound> (translated_bound));

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, type.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  translated = new HIR::TraitObjectType (mapping, std::move (bounds),
					 type.get_locus (), type.is_dyn ());
}

void
ASTLoweringType::visit (AST::TraitObjectType &type)
{
  std::vector<std::unique_ptr<HIR::TypeParamBound>> bounds;

  for (auto &bound : type.get_type_param_bounds ())
    {
      HIR::TypeParamBound *translated_bound
	= ASTLoweringTypeBounds::translate (bound.get ());
      bounds.push_back (
	std::unique_ptr<HIR::TypeParamBound> (translated_bound));
    }

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, type.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  translated = new HIR::TraitObjectType (mapping, std::move (bounds),
					 type.get_locus (), type.is_dyn ());
}

} // namespace HIR
} // namespace Rust
