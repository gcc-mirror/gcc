
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

#include "rust-hir-type.h"

namespace Rust {
namespace HIR {

ImplTraitType::ImplTraitType (
  Analysis::NodeMapping mappings,
  std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds,
  location_t locus)
  : Type (mappings, locus), type_param_bounds (std::move (type_param_bounds))
{}

ImplTraitType::ImplTraitType (ImplTraitType const &other)
  : Type (other.mappings, other.locus)
{
  type_param_bounds.reserve (other.type_param_bounds.size ());
  for (const auto &e : other.type_param_bounds)
    type_param_bounds.push_back (e->clone_type_param_bound ());
}

ImplTraitType &
ImplTraitType::operator= (ImplTraitType const &other)
{
  locus = other.locus;
  mappings = other.mappings;

  type_param_bounds.reserve (other.type_param_bounds.size ());
  for (const auto &e : other.type_param_bounds)
    type_param_bounds.push_back (e->clone_type_param_bound ());

  return *this;
}

TraitObjectType::TraitObjectType (
  Analysis::NodeMapping mappings,
  std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds,
  location_t locus, bool is_dyn_dispatch)
  : Type (mappings, locus), has_dyn (is_dyn_dispatch),
    type_param_bounds (std::move (type_param_bounds))
{}

TraitObjectType::TraitObjectType (TraitObjectType const &other)
  : Type (other.mappings, other.locus), has_dyn (other.has_dyn)
{
  type_param_bounds.reserve (other.type_param_bounds.size ());
  for (const auto &e : other.type_param_bounds)
    type_param_bounds.push_back (e->clone_type_param_bound ());
}

TraitObjectType &
TraitObjectType::operator= (TraitObjectType const &other)
{
  mappings = other.mappings;
  has_dyn = other.has_dyn;
  locus = other.locus;
  type_param_bounds.reserve (other.type_param_bounds.size ());
  for (const auto &e : other.type_param_bounds)
    type_param_bounds.push_back (e->clone_type_param_bound ());

  return *this;
}

ParenthesisedType::ParenthesisedType (Analysis::NodeMapping mappings,
				      std::unique_ptr<Type> type_inside_parens,
				      location_t locus)
  : TypeNoBounds (mappings, locus),
    type_in_parens (std::move (type_inside_parens))
{}

ParenthesisedType::ParenthesisedType (ParenthesisedType const &other)
  : TypeNoBounds (other.mappings, other.locus),
    type_in_parens (other.type_in_parens->clone_type ())
{}

ParenthesisedType &
ParenthesisedType::operator= (ParenthesisedType const &other)
{
  mappings = other.mappings;
  type_in_parens = other.type_in_parens->clone_type ();
  locus = other.locus;
  return *this;
}

std::unique_ptr<TraitBound>
ParenthesisedType::to_trait_bound (bool in_parens ATTRIBUTE_UNUSED) const
{
  /* NOTE: obviously it is unknown whether the internal type is a trait bound
   * due to polymorphism, so just let the internal type handle it. As
   * parenthesised type, it must be in parentheses. */
  return type_in_parens->to_trait_bound (true);
}

TupleType::TupleType (Analysis::NodeMapping mappings,
		      std::vector<std::unique_ptr<Type>> elems,
		      location_t locus)
  : TypeNoBounds (mappings, locus), elems (std::move (elems))
{}

TupleType::TupleType (TupleType const &other)
  : TypeNoBounds (other.mappings, other.locus)
{
  mappings = other.mappings;
  elems.reserve (other.elems.size ());
  for (const auto &e : other.elems)
    elems.push_back (e->clone_type ());
}

TupleType &
TupleType::operator= (TupleType const &other)
{
  locus = other.locus;

  elems.reserve (other.elems.size ());
  for (const auto &e : other.elems)
    elems.push_back (e->clone_type ());

  return *this;
}

NeverType::NeverType (Analysis::NodeMapping mappings, location_t locus)
  : TypeNoBounds (mappings, locus)
{}

RawPointerType::RawPointerType (Analysis::NodeMapping mappings, Mutability mut,
				std::unique_ptr<Type> type, location_t locus)
  : TypeNoBounds (mappings, locus), mut (mut), type (std::move (type))
{}

RawPointerType::RawPointerType (RawPointerType const &other)
  : TypeNoBounds (other.mappings, other.locus), mut (other.mut),
    type (other.type->clone_type ())
{}

RawPointerType &
RawPointerType::operator= (RawPointerType const &other)
{
  mappings = other.mappings;
  mut = other.mut;
  type = other.type->clone_type ();
  locus = other.locus;
  return *this;
}

ReferenceType::ReferenceType (Analysis::NodeMapping mappings, Mutability mut,
			      std::unique_ptr<Type> type_no_bounds,
			      location_t locus, tl::optional<Lifetime> lifetime)
  : TypeNoBounds (mappings, locus), lifetime (std::move (lifetime)), mut (mut),
    type (std::move (type_no_bounds))
{}

ReferenceType::ReferenceType (ReferenceType const &other)
  : TypeNoBounds (other.mappings, other.locus), lifetime (other.lifetime),
    mut (other.mut), type (other.type->clone_type ())
{}

ReferenceType &
ReferenceType::operator= (ReferenceType const &other)
{
  mappings = other.mappings;
  lifetime = other.lifetime;
  mut = other.mut;
  type = other.type->clone_type ();
  locus = other.locus;

  return *this;
}

ArrayType::ArrayType (Analysis::NodeMapping mappings,
		      std::unique_ptr<Type> type,
		      std::unique_ptr<Expr> array_size, location_t locus)
  : TypeNoBounds (mappings, locus), elem_type (std::move (type)),
    size (std::move (array_size))
{}

ArrayType::ArrayType (ArrayType const &other)
  : TypeNoBounds (other.mappings, other.locus),
    elem_type (other.elem_type->clone_type ()), size (other.size->clone_expr ())
{}

ArrayType &
ArrayType::operator= (ArrayType const &other)
{
  mappings = other.mappings;
  elem_type = other.elem_type->clone_type ();
  size = other.size->clone_expr ();
  locus = other.locus;
  return *this;
}

SliceType::SliceType (Analysis::NodeMapping mappings,
		      std::unique_ptr<Type> type, location_t locus)
  : TypeNoBounds (mappings, locus), elem_type (std::move (type))
{}

SliceType::SliceType (SliceType const &other)
  : TypeNoBounds (other.mappings, other.locus),
    elem_type (other.elem_type->clone_type ())
{}

SliceType &
SliceType::operator= (SliceType const &other)
{
  mappings = other.mappings;
  elem_type = other.elem_type->clone_type ();
  locus = other.locus;

  return *this;
}

InferredType::InferredType (Analysis::NodeMapping mappings, location_t locus)
  : TypeNoBounds (mappings, locus)
{}

MaybeNamedParam::MaybeNamedParam (Identifier name, ParamKind param_kind,
				  std::unique_ptr<Type> param_type,
				  location_t locus)
  : param_type (std::move (param_type)), param_kind (param_kind),
    name (std::move (name)), locus (locus)
{}

MaybeNamedParam::MaybeNamedParam (MaybeNamedParam const &other)
  : param_type (other.param_type->clone_type ()), param_kind (other.param_kind),
    name (other.name), locus (other.locus)
{}

MaybeNamedParam &
MaybeNamedParam::operator= (MaybeNamedParam const &other)
{
  name = other.name;
  param_kind = other.param_kind;
  param_type = other.param_type->clone_type ();
  locus = other.locus;

  return *this;
}

BareFunctionType::BareFunctionType (
  Analysis::NodeMapping mappings, std::vector<LifetimeParam> lifetime_params,
  FunctionQualifiers qualifiers, std::vector<MaybeNamedParam> named_params,
  bool is_variadic, std::unique_ptr<Type> type, location_t locus)
  : TypeNoBounds (mappings, locus), for_lifetimes (std::move (lifetime_params)),
    function_qualifiers (std::move (qualifiers)),
    params (std::move (named_params)), is_variadic (is_variadic),
    return_type (std::move (type))
{}

BareFunctionType::BareFunctionType (BareFunctionType const &other)
  : TypeNoBounds (other.mappings, other.locus),
    for_lifetimes (other.for_lifetimes),
    function_qualifiers (other.function_qualifiers), params (other.params),
    is_variadic (other.is_variadic),
    return_type (other.has_return_type () ? other.return_type->clone_type ()
					  : nullptr)
{}

BareFunctionType &
BareFunctionType::operator= (BareFunctionType const &other)
{
  mappings = other.mappings;
  for_lifetimes = other.for_lifetimes;
  function_qualifiers = other.function_qualifiers;
  params = other.params;
  is_variadic = other.is_variadic;
  return_type = other.return_type->clone_type ();
  locus = other.locus;

  return *this;
}

} // namespace HIR
} // namespace Rust
