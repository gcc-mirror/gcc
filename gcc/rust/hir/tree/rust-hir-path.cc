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

#include "rust-hir-path.h"
#include "optional.h"
#include "rust-hir-bound.h"

namespace Rust {
namespace HIR {

GenericArgsBinding::GenericArgsBinding (Identifier ident,
					std::unique_ptr<Type> type_ptr,
					location_t locus)
  : identifier (std::move (ident)), type (std::move (type_ptr)), locus (locus)
{}

GenericArgsBinding::GenericArgsBinding (GenericArgsBinding const &other)
  : identifier (other.identifier), type (other.type->clone_type ()),
    locus (other.locus)
{}

GenericArgsBinding &
GenericArgsBinding::operator= (GenericArgsBinding const &other)
{
  identifier = other.identifier;
  type = other.type->clone_type ();
  locus = other.locus;
  return *this;
}

ConstGenericArg::ConstGenericArg (std::unique_ptr<Expr> expression,
				  location_t locus)
  : expression (std::move (expression)), locus (locus)
{}

ConstGenericArg::ConstGenericArg (const ConstGenericArg &other)
  : locus (other.locus)
{
  expression = other.expression->clone_expr ();
}

ConstGenericArg
ConstGenericArg::operator= (const ConstGenericArg &other)
{
  expression = other.expression->clone_expr ();
  locus = other.locus;

  return *this;
}

GenericArgs &
GenericArgs::operator= (GenericArgs const &other)
{
  lifetime_args = other.lifetime_args;
  binding_args = other.binding_args;
  const_args = other.const_args;
  locus = other.locus;

  type_args.clear ();
  type_args.reserve (other.type_args.size ());
  for (const auto &e : other.type_args)
    type_args.push_back (e->clone_type ());

  return *this;
}

GenericArgs::GenericArgs (std::vector<Lifetime> lifetime_args,
			  std::vector<std::unique_ptr<Type> > type_args,
			  std::vector<GenericArgsBinding> binding_args,
			  std::vector<ConstGenericArg> const_args,
			  location_t locus)
  : lifetime_args (std::move (lifetime_args)),
    type_args (std::move (type_args)), binding_args (std::move (binding_args)),
    const_args (std::move (const_args)), locus (locus)
{}

GenericArgs::GenericArgs (GenericArgs const &other)
  : lifetime_args (other.lifetime_args), binding_args (other.binding_args),
    const_args (other.const_args), locus (other.locus)
{
  type_args.clear ();
  type_args.reserve (other.type_args.size ());

  for (const auto &e : other.type_args)
    type_args.push_back (e->clone_type ());
}

bool
GenericArgs::is_empty () const
{
  return lifetime_args.size () == 0 && type_args.size () == 0
	 && binding_args.size () == 0;
}

PathExprSegment::PathExprSegment (Analysis::NodeMapping mappings,
				  PathIdentSegment segment_name,
				  location_t locus, GenericArgs generic_args)
  : mappings (std::move (mappings)), segment_name (std::move (segment_name)),
    generic_args (std::move (generic_args)), locus (locus)
{}

PathExprSegment::PathExprSegment (PathExprSegment const &other)
  : mappings (other.mappings), segment_name (other.segment_name),
    generic_args (other.generic_args), locus (other.locus)
{}

PathExprSegment &
PathExprSegment::operator= (PathExprSegment const &other)
{
  mappings = other.mappings;
  segment_name = other.segment_name;
  generic_args = other.generic_args;
  locus = other.locus;

  return *this;
}

void
PathPattern::iterate_path_segments (std::function<bool (PathExprSegment &)> cb)
{
  rust_assert (kind == Kind::Segmented);

  for (auto it = segments.begin (); it != segments.end (); it++)
    {
      if (!cb (*it))
	return;
    }
}

PathInExpression::PathInExpression (Analysis::NodeMapping mappings,
				    std::vector<PathExprSegment> path_segments,
				    location_t locus,
				    bool has_opening_scope_resolution,
				    std::vector<AST::Attribute> outer_attrs)
  : PathPattern (std::move (path_segments)),
    PathExpr (std::move (mappings), std::move (outer_attrs)),
    has_opening_scope_resolution (has_opening_scope_resolution), locus (locus)
{}

PathInExpression::PathInExpression (Analysis::NodeMapping mappings,
				    LangItem::Kind lang_item, location_t locus,
				    bool has_opening_scope_resolution,
				    std::vector<AST::Attribute> outer_attrs)
  : PathPattern (lang_item),
    PathExpr (std::move (mappings), std::move (outer_attrs)),
    has_opening_scope_resolution (has_opening_scope_resolution), locus (locus)
{}

bool
PathInExpression::is_self () const

{
  if (!is_single_segment ())
    return false;

  return get_final_segment ().get_segment ().as_string ().compare ("self") == 0;
}

TypePathSegment::TypePathSegment (Analysis::NodeMapping mappings,
				  PathIdentSegment ident_segment,
				  bool has_separating_scope_resolution,
				  location_t locus)
  : mappings (std::move (mappings)), ident_segment (std::move (ident_segment)),
    lang_item (tl::nullopt), locus (locus),
    has_separating_scope_resolution (has_separating_scope_resolution),
    type (SegmentType::REG)
{}

TypePathSegment::TypePathSegment (Analysis::NodeMapping mappings,
				  LangItem::Kind lang_item, location_t locus)
  : mappings (std::move (mappings)), ident_segment (tl::nullopt),
    lang_item (lang_item), locus (locus),
    has_separating_scope_resolution (false), type (SegmentType::REG)
{}

TypePathSegment::TypePathSegment (Analysis::NodeMapping mappings,
				  std::string segment_name,
				  bool has_separating_scope_resolution,
				  location_t locus)
  : mappings (std::move (mappings)),
    ident_segment (PathIdentSegment (std::move (segment_name))),
    lang_item (tl::nullopt), locus (locus),
    has_separating_scope_resolution (has_separating_scope_resolution),
    type (SegmentType::REG)
{}

TypePathSegmentGeneric::TypePathSegmentGeneric (
  Analysis::NodeMapping mappings, PathIdentSegment ident_segment,
  bool has_separating_scope_resolution, GenericArgs generic_args,
  location_t locus)
  : TypePathSegment (std::move (mappings), std::move (ident_segment),
		     has_separating_scope_resolution, locus),
    generic_args (std::move (generic_args))
{}

TypePathSegmentGeneric::TypePathSegmentGeneric (Analysis::NodeMapping mappings,
						LangItem::Kind lang_item,
						GenericArgs generic_args,
						location_t locus)
  : TypePathSegment (std::move (mappings), lang_item, locus),
    generic_args (std::move (generic_args))
{}

TypePathSegmentGeneric::TypePathSegmentGeneric (
  Analysis::NodeMapping mappings, std::string segment_name,
  bool has_separating_scope_resolution, std::vector<Lifetime> lifetime_args,
  std::vector<std::unique_ptr<Type> > type_args,
  std::vector<GenericArgsBinding> binding_args,
  std::vector<ConstGenericArg> const_args, location_t locus)
  : TypePathSegment (std::move (mappings), std::move (segment_name),
		     has_separating_scope_resolution, locus),
    generic_args (GenericArgs (std::move (lifetime_args), std::move (type_args),
			       std::move (binding_args), std::move (const_args),
			       locus))
{}

TypePathFunction::TypePathFunction (std::vector<std::unique_ptr<Type> > inputs,
				    std::unique_ptr<Type> type)
  : inputs (std::move (inputs)), return_type (std::move (type))
{}

TypePathFunction::TypePathFunction (TypePathFunction const &other)
{
  return_type = other.has_return_type ()
		  ? other.get_return_type ().clone_type ()
		  : nullptr;

  inputs.reserve (other.inputs.size ());
  for (const auto &e : other.inputs)
    inputs.push_back (e->clone_type ());
}

TypePathFunction &
TypePathFunction::operator= (TypePathFunction const &other)
{
  return_type = other.has_return_type ()
		  ? other.get_return_type ().clone_type ()
		  : nullptr;

  inputs.reserve (other.inputs.size ());
  for (const auto &e : other.inputs)
    inputs.push_back (e->clone_type ());

  return *this;
}

TypePathSegmentFunction::TypePathSegmentFunction (
  Analysis::NodeMapping mappings, PathIdentSegment ident_segment,
  bool has_separating_scope_resolution, TypePathFunction function_path,
  location_t locus)
  : TypePathSegment (std::move (mappings), std::move (ident_segment),
		     has_separating_scope_resolution, locus),
    function_path (std::move (function_path))
{}

TypePathSegmentFunction::TypePathSegmentFunction (
  Analysis::NodeMapping mappings, std::string segment_name,
  bool has_separating_scope_resolution, TypePathFunction function_path,
  location_t locus)
  : TypePathSegment (std::move (mappings), std::move (segment_name),
		     has_separating_scope_resolution, locus),
    function_path (std::move (function_path))
{}

TypePath::TypePath (Analysis::NodeMapping mappings,
		    std::vector<std::unique_ptr<TypePathSegment> > segments,
		    location_t locus, bool has_opening_scope_resolution)
  : TypeNoBounds (mappings, locus),
    has_opening_scope_resolution (has_opening_scope_resolution),
    segments (std::move (segments))
{}

TypePath::TypePath (TypePath const &other)
  : TypeNoBounds (other.mappings, other.locus),
    has_opening_scope_resolution (other.has_opening_scope_resolution)
{
  segments.reserve (other.segments.size ());
  for (const auto &e : other.segments)
    segments.push_back (e->clone_type_path_segment ());
}

TypePath &
TypePath::operator= (TypePath const &other)
{
  has_opening_scope_resolution = other.has_opening_scope_resolution;
  locus = other.locus;
  mappings = other.mappings;

  segments.reserve (other.segments.size ());
  for (const auto &e : other.segments)
    segments.push_back (e->clone_type_path_segment ());

  return *this;
}

QualifiedPathType::QualifiedPathType (Analysis::NodeMapping mappings,
				      std::unique_ptr<Type> type,
				      std::unique_ptr<TypePath> trait,
				      location_t locus)
  : type (std::move (type)), trait (std::move (trait)), locus (locus),
    mappings (mappings)
{}

QualifiedPathType::QualifiedPathType (QualifiedPathType const &other)
  : type (other.type->clone_type ()),
    trait (other.has_as_clause ()
	     ? std::unique_ptr<HIR::TypePath> (new HIR::TypePath (*other.trait))
	     : nullptr),
    locus (other.locus), mappings (other.mappings)
{}

QualifiedPathType &
QualifiedPathType::operator= (QualifiedPathType const &other)
{
  type = other.type->clone_type ();
  locus = other.locus;
  mappings = other.mappings;
  trait = other.has_as_clause ()
	    ? std::unique_ptr<HIR::TypePath> (new HIR::TypePath (*other.trait))
	    : nullptr;

  return *this;
}

bool
QualifiedPathType::trait_has_generic_args () const
{
  rust_assert (has_as_clause ());
  bool is_generic_seg = trait->get_final_segment ().get_type ()
			== TypePathSegment::SegmentType::GENERIC;
  if (!is_generic_seg)
    return false;

  auto &seg
    = static_cast<TypePathSegmentGeneric &> (trait->get_final_segment ());
  return seg.has_generic_args ();
}

GenericArgs &
QualifiedPathType::get_trait_generic_args ()
{
  rust_assert (trait_has_generic_args ());
  auto &seg
    = static_cast<TypePathSegmentGeneric &> (trait->get_final_segment ());
  return seg.get_generic_args ();
}

QualifiedPathInExpression::QualifiedPathInExpression (
  Analysis::NodeMapping mappings, QualifiedPathType qual_path_type,
  std::vector<PathExprSegment> path_segments, location_t locus,
  std::vector<AST::Attribute> outer_attrs)
  : PathPattern (std::move (path_segments)),
    PathExpr (std::move (mappings), std::move (outer_attrs)),
    path_type (std::move (qual_path_type)), locus (locus)
{}

QualifiedPathInExpression::QualifiedPathInExpression (
  Analysis::NodeMapping mappings, QualifiedPathType qual_path_type,
  LangItem::Kind lang_item, location_t locus,
  std::vector<AST::Attribute> outer_attrs)
  : PathPattern (lang_item),
    PathExpr (std::move (mappings), std::move (outer_attrs)),
    path_type (std::move (qual_path_type)), locus (locus)
{}

QualifiedPathInType::QualifiedPathInType (
  Analysis::NodeMapping mappings, QualifiedPathType qual_path_type,
  std::unique_ptr<TypePathSegment> associated_segment,
  std::vector<std::unique_ptr<TypePathSegment> > path_segments,
  location_t locus)
  : TypeNoBounds (mappings, locus), path_type (std::move (qual_path_type)),
    associated_segment (std::move (associated_segment)),
    segments (std::move (path_segments))
{}

QualifiedPathInType::QualifiedPathInType (QualifiedPathInType const &other)
  : TypeNoBounds (other.mappings, other.locus), path_type (other.path_type)
{
  auto seg = other.associated_segment->clone_type_path_segment_impl ();
  associated_segment = std::unique_ptr<TypePathSegment> (seg);

  segments.reserve (other.segments.size ());
  for (const auto &e : other.segments)
    segments.push_back (e->clone_type_path_segment ());
}

QualifiedPathInType &
QualifiedPathInType::operator= (QualifiedPathInType const &other)
{
  auto seg = other.associated_segment->clone_type_path_segment_impl ();
  associated_segment = std::unique_ptr<TypePathSegment> (seg);

  path_type = other.path_type;
  locus = other.locus;
  mappings = other.mappings;

  segments.reserve (other.segments.size ());
  for (const auto &e : other.segments)
    segments.push_back (e->clone_type_path_segment ());

  return *this;
}

} // namespace HIR
} // namespace Rust
