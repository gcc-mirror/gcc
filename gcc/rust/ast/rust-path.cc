/* General AST-related method implementations for Rust frontend.
   Copyright (C) 2009-2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "rust-system.h"
#include "rust-ast-full.h"
#include "rust-diagnostics.h"
#include "rust-ast-visitor.h"
#include "rust-macro.h"
#include "rust-session-manager.h"
#include "rust-lex.h"
#include "rust-parse.h"
#include "rust-operators.h"

namespace Rust {
namespace AST {

std::string
GenericArgs::as_string () const
{
  std::string args;

  // lifetime args
  if (!lifetime_args.empty ())
    {
      auto i = lifetime_args.begin ();
      auto e = lifetime_args.end ();

      for (; i != e; i++)
	{
	  args += (*i).as_string ();
	  if (e != i + 1)
	    args += ", ";
	}
    }

  // type args
  if (!generic_args.empty ())
    {
      auto i = generic_args.begin ();
      auto e = generic_args.end ();

      for (; i != e; i++)
	{
	  args += (*i).as_string ();
	  if (e != i + 1)
	    args += ", ";
	}
    }

  // binding args
  if (!binding_args.empty ())
    {
      auto i = binding_args.begin ();
      auto e = binding_args.end ();

      for (; i != e; i++)
	{
	  args += (*i).as_string ();
	  if (e != i + 1)
	    args += ", ";
	}
    }

  return args;
}

GenericArg
GenericArg::disambiguate_to_const () const
{
  rust_assert (get_kind () == Kind::Either);

  // FIXME: is it fine to have no outer attributes?
  return GenericArg::create_const (
    std::unique_ptr<Expr> (new IdentifierExpr (path, {}, locus)));
}

GenericArg
GenericArg::disambiguate_to_type () const
{
  rust_assert (get_kind () == Kind::Either);

  auto segment = std::unique_ptr<TypePathSegment> (
    new TypePathSegment (path.as_string (), false, locus));
  auto segments = std::vector<std::unique_ptr<TypePathSegment>> ();
  segments.emplace_back (std::move (segment));

  return GenericArg::create_type (
    std::unique_ptr<Type> (new TypePath (std::move (segments), locus)));
}

std::string
GenericArgsBinding::as_string () const
{
  // TODO: rewrite to work with non-literalisable types
  return identifier.as_string () + " = " + type->as_string ();
}

std::string
ConstGenericParam::as_string () const
{
  std::string str ("ConstGenericParam: ");
  str += "const " + name.as_string () + ": " + type->as_string ();

  if (has_default_value ())
    str += " = " + get_default_value ().as_string ();

  return str;
}

std::string
PathExprSegment::as_string () const
{
  // TODO: rewrite dump to work with non-literalisable types
  std::string ident_str = segment_name.as_string ();
  if (has_generic_args ())
    ident_str += "::<" + generic_args.as_string () + ">";

  return ident_str;
}

std::string
PathPattern::as_string () const
{
  std::string str;

  for (const auto &segment : segments)
    str += segment.as_string () + "::";

  // basically a hack - remove last two characters of string (remove final ::)
  str.erase (str.length () - 2);

  return str;
}

SimplePath
PathPattern::convert_to_simple_path (bool with_opening_scope_resolution) const
{
  if (!has_segments ())
    return SimplePath::create_empty ();

  // create vector of reserved size (to minimise reallocations)
  std::vector<SimplePathSegment> simple_segments;
  simple_segments.reserve (segments.size ());

  for (const auto &segment : segments)
    {
      // return empty path if doesn't meet simple path segment requirements
      if (segment.is_error () || segment.has_generic_args ()
	  || segment.as_string () == "Self")
	return SimplePath::create_empty ();

      // create segment and add to vector
      std::string segment_str = segment.as_string ();
      simple_segments.push_back (
	SimplePathSegment (std::move (segment_str), segment.get_locus ()));
    }

  // kind of a HACK to get locus depending on opening scope resolution
  location_t locus = UNKNOWN_LOCATION;
  if (with_opening_scope_resolution)
    locus = simple_segments[0].get_locus () - 2; // minus 2 chars for ::
  else
    locus = simple_segments[0].get_locus ();
  // FIXME: this hack probably doesn't actually work

  return SimplePath (std::move (simple_segments), with_opening_scope_resolution,
		     locus);
}

void
PathInExpression::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

std::string
PathInExpression::as_string () const
{
  std::string str;

  if (has_opening_scope_resolution)
    str = "::";

  return str + PathPattern::as_string ();
}

std::string
TypePathSegmentGeneric::as_string () const
{
  // TODO: rewrite to work with non-linearisable types
  return TypePathSegment::as_string () + "<" + generic_args.as_string () + ">";
}

std::string
TypePathSegmentFunction::as_string () const
{
  // TODO: rewrite to work with non-linearisable types
  return TypePathSegment::as_string () + function_path.as_string ();
}

std::string
TypePath::as_string () const
{
  /* TODO: this may need to be rewritten if a segment (e.g. function) can't be
   * literalised */
  std::string str;

  if (has_opening_scope_resolution)
    str = "::";

  for (const auto &segment : segments)
    str += segment->as_string () + "::";

  // kinda hack - remove last 2 '::' characters
  str.erase (str.length () - 2);

  return str;
}

SimplePath
TypePath::as_simple_path () const
{
  if (segments.empty ())
    return SimplePath::create_empty ();

  // create vector of reserved size (to minimise reallocations)
  std::vector<SimplePathSegment> simple_segments;
  simple_segments.reserve (segments.size ());

  for (const auto &segment : segments)
    {
      // return empty path if doesn't meet simple path segment requirements
      if (segment == nullptr || segment->is_error ()
	  || !segment->is_ident_only () || segment->as_string () == "Self")
	return SimplePath::create_empty ();

      // create segment and add to vector
      std::string segment_str = segment->as_string ();
      simple_segments.push_back (
	SimplePathSegment (std::move (segment_str), segment->get_locus ()));
    }

  return SimplePath (std::move (simple_segments), has_opening_scope_resolution,
		     locus);
}

// hopefully definition here will prevent circular dependency issue
TraitBound *
TypePath::to_trait_bound (bool in_parens) const
{
  return new TraitBound (TypePath (*this), get_locus (), in_parens);
}

std::string
TypePathFunction::as_string () const
{
  // TODO: rewrite to work with non-linearisable types
  std::string str ("(");

  if (has_inputs ())
    {
      auto i = inputs.begin ();
      auto e = inputs.end ();

      for (; i != e; i++)
	{
	  str += (*i)->as_string ();
	  if (e != i + 1)
	    str += ", ";
	}
    }

  str += ")";

  if (has_return_type ())
    str += " -> " + return_type->as_string ();

  return str;
}

std::string
QualifiedPathInExpression::as_string () const
{
  return path_type.as_string () + "::" + PathPattern::as_string ();
}

std::string
QualifiedPathInType::as_string () const
{
  /* TODO: this may need adjusting if segments (e.g. with functions) can't be
   * literalised */
  std::string str = path_type.as_string ();

  str += "::" + associated_segment->as_string ();
  for (const auto &segment : segments)
    str += "::" + segment->as_string ();

  return str;
}

void
ConstGenericParam::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
TypePathSegment::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
TypePathSegmentGeneric::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
TypePathSegmentFunction::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
TypePath::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
QualifiedPathInExpression::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
QualifiedPathInType::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

} // namespace AST
} // namespace Rust
