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

#include "rust-substitution-mapper.h"
#include "rust-hir-type-check.h"

namespace Rust {
namespace Resolver {

SubstMapper::SubstMapper (HirId ref, HIR::GenericArgs *generics,
			  const std::vector<TyTy::Region> &regions,
			  location_t locus)
  : resolved (new TyTy::ErrorType (ref)), generics (generics),
    regions (regions), locus (locus)
{}

TyTy::BaseType *
SubstMapper::Resolve (TyTy::BaseType *base, location_t locus,
		      HIR::GenericArgs *generics,
		      const std::vector<TyTy::Region> &regions)
{
  if (!valid_type (base))
    {
      rich_location r (line_table, locus);
      r.add_fixit_remove (generics->get_locus ());
      rust_error_at (r, ErrorCode::E0109,
		     "generic arguments are not allowed for this type");
      return base;
    }

  SubstMapper mapper (base->get_ref (), generics, regions, locus);
  base->accept_vis (mapper);
  rust_assert (mapper.resolved != nullptr);
  return mapper.resolved;
}

TyTy::BaseType *
SubstMapper::InferSubst (TyTy::BaseType *base, location_t locus)
{
  return SubstMapper::Resolve (base, locus, nullptr, {});
}

bool
SubstMapper::valid_type (TyTy::BaseType *base)
{
  bool is_fn = base->is<TyTy::FnType> ();
  bool is_adt = base->is<TyTy::ADTType> ();
  bool is_placeholder = base->is<TyTy::PlaceholderType> ();
  bool is_projection = base->is<TyTy::ProjectionType> ();

  return is_fn || is_adt || is_placeholder || is_projection;
}

bool
SubstMapper::have_generic_args () const
{
  return generics != nullptr;
}

void
SubstMapper::visit (TyTy::FnType &type)
{
  TyTy::FnType *concrete = nullptr;
  if (!have_generic_args ())
    {
      TyTy::BaseType *substs = type.infer_substitions (locus);
      rust_assert (substs->get_kind () == TyTy::TypeKind::FNDEF);
      concrete = static_cast<TyTy::FnType *> (substs);
    }
  else
    {
      TyTy::SubstitutionArgumentMappings mappings
	= type.get_mappings_from_generic_args (*generics, regions);
      if (mappings.is_error ())
	return;

      concrete = type.handle_substitions (mappings);
    }

  if (concrete != nullptr)
    resolved = concrete;
}

void
SubstMapper::visit (TyTy::ADTType &type)
{
  TyTy::ADTType *concrete = nullptr;
  if (!have_generic_args ())
    {
      TyTy::BaseType *substs = type.infer_substitions (locus);
      rust_assert (substs->get_kind () == TyTy::TypeKind::ADT);
      concrete = static_cast<TyTy::ADTType *> (substs);
    }
  else
    {
      TyTy::SubstitutionArgumentMappings mappings
	= type.get_mappings_from_generic_args (*generics, regions);
      if (mappings.is_error ())
	return;

      concrete = type.handle_substitions (mappings);
    }

  if (concrete != nullptr)
    resolved = concrete;
}

void
SubstMapper::visit (TyTy::PlaceholderType &type)
{
  if (!type.can_resolve ())
    {
      resolved = &type;
      return;
    }

  resolved = SubstMapper::Resolve (type.resolve (), locus, generics, regions);
}

void
SubstMapper::visit (TyTy::ProjectionType &type)
{
  TyTy::ProjectionType *concrete = nullptr;
  if (!have_generic_args ())
    {
      TyTy::BaseType *substs = type.infer_substitions (locus);
      rust_assert (substs->get_kind () == TyTy::TypeKind::PROJECTION);
      concrete = static_cast<TyTy::ProjectionType *> (substs);
    }
  else
    {
      TyTy::SubstitutionArgumentMappings mappings
	= type.get_mappings_from_generic_args (*generics, regions);
      if (mappings.is_error ())
	return;

      concrete = type.handle_substitions (mappings);
    }

  if (concrete != nullptr)
    resolved = concrete;
}

SubstMapperInternal::SubstMapperInternal (
  HirId ref, TyTy::SubstitutionArgumentMappings &mappings)
  : resolved (new TyTy::ErrorType (ref)), mappings (mappings)
{}

TyTy::BaseType *
SubstMapperInternal::Resolve (TyTy::BaseType *base,
			      TyTy::SubstitutionArgumentMappings &mappings)
{
  auto context = TypeCheckContext::get ();

  SubstMapperInternal mapper (base->get_ref (), mappings);
  base->accept_vis (mapper);
  rust_assert (mapper.resolved != nullptr);

  // insert these new implict types into the context
  TyTy::BaseType *unused = nullptr;
  bool is_ty_available
    = context->lookup_type (mapper.resolved->get_ty_ref (), &unused);
  if (!is_ty_available)
    {
      context->insert_type (
	Analysis::NodeMapping (0, 0, mapper.resolved->get_ty_ref (), 0),
	mapper.resolved);
    }
  bool is_ref_available
    = context->lookup_type (mapper.resolved->get_ref (), &unused);
  if (!is_ref_available)
    {
      context->insert_type (Analysis::NodeMapping (0, 0,
						   mapper.resolved->get_ref (),
						   0),
			    mapper.resolved);
    }

  return mapper.resolved;
}

bool
SubstMapperInternal::mappings_are_bound (
  TyTy::BaseType *tyseg, TyTy::SubstitutionArgumentMappings &mappings)
{
  if (tyseg->get_kind () == TyTy::TypeKind::ADT)
    {
      TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (tyseg);
      return adt->are_mappings_bound (mappings);
    }
  else if (tyseg->get_kind () == TyTy::TypeKind::FNDEF)
    {
      TyTy::FnType *fn = static_cast<TyTy::FnType *> (tyseg);
      return fn->are_mappings_bound (mappings);
    }

  return false;
}

void
SubstMapperInternal::visit (TyTy::FnType &type)
{
  TyTy::SubstitutionArgumentMappings adjusted
    = type.adjust_mappings_for_this (mappings);
  if (adjusted.is_error () && !mappings.trait_item_mode ())
    return;
  if (adjusted.is_error () && mappings.trait_item_mode ())
    adjusted = mappings;

  TyTy::BaseType *concrete = type.handle_substitions (adjusted);
  if (concrete != nullptr)
    resolved = concrete;
}

void
SubstMapperInternal::visit (TyTy::ADTType &type)
{
  TyTy::SubstitutionArgumentMappings adjusted
    = type.adjust_mappings_for_this (mappings);
  if (adjusted.is_error () && !mappings.trait_item_mode ())
    return;
  if (adjusted.is_error () && mappings.trait_item_mode ())
    adjusted = mappings;

  TyTy::BaseType *concrete = type.handle_substitions (adjusted);
  if (concrete != nullptr)
    resolved = concrete;
}

// these don't support generic arguments but might contain a type param
void
SubstMapperInternal::visit (TyTy::TupleType &type)
{
  resolved = type.handle_substitions (mappings);
}

void
SubstMapperInternal::visit (TyTy::ReferenceType &type)
{
  resolved = type.handle_substitions (mappings);
}

void
SubstMapperInternal::visit (TyTy::PointerType &type)
{
  resolved = type.handle_substitions (mappings);
}

void
SubstMapperInternal::visit (TyTy::ParamType &type)
{
  resolved = type.handle_substitions (mappings);
}

void
SubstMapperInternal::visit (TyTy::PlaceholderType &type)
{
  rust_assert (type.can_resolve ());
  if (mappings.trait_item_mode ())
    {
      resolved = type.resolve ();
    }
  else
    {
      resolved = SubstMapperInternal::Resolve (type.resolve (), mappings);
    }
}

void
SubstMapperInternal::visit (TyTy::ProjectionType &type)
{
  resolved = type.handle_substitions (mappings);
}

void
SubstMapperInternal::visit (TyTy::ClosureType &type)
{
  resolved = type.handle_substitions (mappings);
}

void
SubstMapperInternal::visit (TyTy::ArrayType &type)
{
  resolved = type.handle_substitions (mappings);
}

void
SubstMapperInternal::visit (TyTy::SliceType &type)
{
  resolved = type.handle_substitions (mappings);
}

// nothing to do for these
void
SubstMapperInternal::visit (TyTy::InferType &type)
{
  resolved = type.clone ();
}
void
SubstMapperInternal::visit (TyTy::FnPtr &type)
{
  resolved = type.clone ();
}
void
SubstMapperInternal::visit (TyTy::BoolType &type)
{
  resolved = type.clone ();
}
void
SubstMapperInternal::visit (TyTy::IntType &type)
{
  resolved = type.clone ();
}
void
SubstMapperInternal::visit (TyTy::UintType &type)
{
  resolved = type.clone ();
}
void
SubstMapperInternal::visit (TyTy::FloatType &type)
{
  resolved = type.clone ();
}
void
SubstMapperInternal::visit (TyTy::USizeType &type)
{
  resolved = type.clone ();
}
void
SubstMapperInternal::visit (TyTy::ISizeType &type)
{
  resolved = type.clone ();
}
void
SubstMapperInternal::visit (TyTy::ErrorType &type)
{
  resolved = type.clone ();
}
void
SubstMapperInternal::visit (TyTy::CharType &type)
{
  resolved = type.clone ();
}
void
SubstMapperInternal::visit (TyTy::StrType &type)
{
  resolved = type.clone ();
}
void
SubstMapperInternal::visit (TyTy::NeverType &type)
{
  resolved = type.clone ();
}
void
SubstMapperInternal::visit (TyTy::DynamicObjectType &type)
{
  resolved = type.clone ();
}
void
SubstMapperInternal::visit (TyTy::OpaqueType &type)
{
  resolved = type.handle_substitions (mappings);
}

// SubstMapperFromExisting

SubstMapperFromExisting::SubstMapperFromExisting (TyTy::BaseType *concrete,
						  TyTy::BaseType *receiver)
  : concrete (concrete), receiver (receiver), resolved (nullptr)
{}

TyTy::BaseType *
SubstMapperFromExisting::Resolve (TyTy::BaseType *concrete,
				  TyTy::BaseType *receiver)
{
  rust_assert (concrete->get_kind () == receiver->get_kind ());

  SubstMapperFromExisting mapper (concrete, receiver);
  concrete->accept_vis (mapper);
  return mapper.resolved;
}

void
SubstMapperFromExisting::visit (TyTy::FnType &type)
{
  rust_assert (type.was_substituted ());

  TyTy::FnType *to_sub = static_cast<TyTy::FnType *> (receiver);
  resolved = to_sub->handle_substitions (type.get_substitution_arguments ());
}

void
SubstMapperFromExisting::visit (TyTy::ADTType &type)
{
  rust_assert (type.was_substituted ());

  TyTy::ADTType *to_sub = static_cast<TyTy::ADTType *> (receiver);
  resolved = to_sub->handle_substitions (type.get_substitution_arguments ());
}

void
SubstMapperFromExisting::visit (TyTy::ClosureType &type)
{
  rust_assert (type.was_substituted ());

  TyTy::ClosureType *to_sub = static_cast<TyTy::ClosureType *> (receiver);
  resolved = to_sub->handle_substitions (type.get_substitution_arguments ());
}

// GetUsedSubstArgs

GetUsedSubstArgs::GetUsedSubstArgs ()
  : args (TyTy::SubstitutionArgumentMappings::error ())
{}

TyTy::SubstitutionArgumentMappings
GetUsedSubstArgs::From (const TyTy::BaseType *from)
{
  GetUsedSubstArgs mapper;
  from->accept_vis (mapper);
  return mapper.args;
}

void
GetUsedSubstArgs::visit (const TyTy::FnType &type)
{
  args = type.get_substitution_arguments ();
}

void
GetUsedSubstArgs::visit (const TyTy::ADTType &type)
{
  args = type.get_substitution_arguments ();
}

void
GetUsedSubstArgs::visit (const TyTy::ClosureType &type)
{
  args = type.get_substitution_arguments ();
}

} // namespace Resolver
} // namespace Rust
