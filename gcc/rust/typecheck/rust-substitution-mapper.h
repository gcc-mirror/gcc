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

#ifndef RUST_SUBSTITUTION_MAPPER_H
#define RUST_SUBSTITUTION_MAPPER_H

#include "rust-tyty.h"
#include "rust-tyty-visitor.h"

namespace Rust {
namespace Resolver {

class SubstMapper : public TyTy::TyVisitor
{
public:
  static TyTy::BaseType *Resolve (TyTy::BaseType *base, Location locus,
				  HIR::GenericArgs *generics = nullptr)
  {
    SubstMapper mapper (base->get_ref (), generics, locus);
    base->accept_vis (mapper);
    rust_assert (mapper.resolved != nullptr);
    return mapper.resolved;
  }

  static TyTy::BaseType *InferSubst (TyTy::BaseType *base, Location locus)
  {
    return SubstMapper::Resolve (base, locus, nullptr);
  }

  bool have_generic_args () const { return generics != nullptr; }

  void visit (TyTy::FnType &type) override
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
	  = type.get_mappings_from_generic_args (*generics);
	if (mappings.is_error ())
	  return;

	concrete = type.handle_substitions (mappings);
      }

    if (concrete != nullptr)
      resolved = concrete;
  }

  void visit (TyTy::ADTType &type) override
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
	  = type.get_mappings_from_generic_args (*generics);
	if (mappings.is_error ())
	  return;

	concrete = type.handle_substitions (mappings);
      }

    if (concrete != nullptr)
      resolved = concrete;
  }

  void visit (TyTy::PlaceholderType &type) override
  {
    rust_assert (type.can_resolve ());
    resolved = SubstMapper::Resolve (type.resolve (), locus, generics);
  }

  void visit (TyTy::ProjectionType &type) override
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
	  = type.get_mappings_from_generic_args (*generics);
	if (mappings.is_error ())
	  return;

	concrete = type.handle_substitions (mappings);
      }

    if (concrete != nullptr)
      resolved = concrete;
  }

  // nothing to do for these
  void visit (TyTy::InferType &) override { gcc_unreachable (); }
  void visit (TyTy::TupleType &) override { gcc_unreachable (); }
  void visit (TyTy::FnPtr &) override { gcc_unreachable (); }
  void visit (TyTy::ArrayType &) override { gcc_unreachable (); }
  void visit (TyTy::SliceType &) override { gcc_unreachable (); }
  void visit (TyTy::BoolType &) override { gcc_unreachable (); }
  void visit (TyTy::IntType &) override { gcc_unreachable (); }
  void visit (TyTy::UintType &) override { gcc_unreachable (); }
  void visit (TyTy::FloatType &) override { gcc_unreachable (); }
  void visit (TyTy::USizeType &) override { gcc_unreachable (); }
  void visit (TyTy::ISizeType &) override { gcc_unreachable (); }
  void visit (TyTy::ErrorType &) override { gcc_unreachable (); }
  void visit (TyTy::CharType &) override { gcc_unreachable (); }
  void visit (TyTy::ReferenceType &) override { gcc_unreachable (); }
  void visit (TyTy::PointerType &) override { gcc_unreachable (); }
  void visit (TyTy::ParamType &) override { gcc_unreachable (); }
  void visit (TyTy::StrType &) override { gcc_unreachable (); }
  void visit (TyTy::NeverType &) override { gcc_unreachable (); }
  void visit (TyTy::DynamicObjectType &) override { gcc_unreachable (); }
  void visit (TyTy::ClosureType &) override { gcc_unreachable (); }

private:
  SubstMapper (HirId ref, HIR::GenericArgs *generics, Location locus)
    : resolved (new TyTy::ErrorType (ref)), generics (generics), locus (locus)
  {}

  TyTy::BaseType *resolved;
  HIR::GenericArgs *generics;
  Location locus;
};

class SubstMapperInternal : public TyTy::TyVisitor
{
public:
  static TyTy::BaseType *Resolve (TyTy::BaseType *base,
				  TyTy::SubstitutionArgumentMappings &mappings);

  static bool mappings_are_bound (TyTy::BaseType *ty,
				  TyTy::SubstitutionArgumentMappings &mappings);

  void visit (TyTy::FnType &type) override
  {
    TyTy::SubstitutionArgumentMappings adjusted
      = type.adjust_mappings_for_this (mappings);
    if (adjusted.is_error ())
      return;

    TyTy::BaseType *concrete = type.handle_substitions (adjusted);
    if (concrete != nullptr)
      resolved = concrete;
  }

  void visit (TyTy::ADTType &type) override
  {
    TyTy::SubstitutionArgumentMappings adjusted
      = type.adjust_mappings_for_this (mappings);
    if (adjusted.is_error ())
      return;

    TyTy::BaseType *concrete = type.handle_substitions (adjusted);
    if (concrete != nullptr)
      resolved = concrete;
  }

  // these don't support generic arguments but might contain a type param
  void visit (TyTy::TupleType &type) override
  {
    resolved = type.handle_substitions (mappings);
  }

  void visit (TyTy::ReferenceType &type) override
  {
    resolved = type.handle_substitions (mappings);
  }

  void visit (TyTy::PointerType &type) override
  {
    resolved = type.handle_substitions (mappings);
  }

  void visit (TyTy::ParamType &type) override
  {
    resolved = type.handle_substitions (mappings);
  }

  void visit (TyTy::PlaceholderType &type) override
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

  void visit (TyTy::ProjectionType &type) override
  {
    resolved = type.handle_substitions (mappings);
  }

  void visit (TyTy::ClosureType &type) override
  {
    resolved = type.handle_substitions (mappings);
  }

  void visit (TyTy::ArrayType &type) override
  {
    resolved = type.handle_substitions (mappings);
  }

  void visit (TyTy::SliceType &type) override
  {
    resolved = type.handle_substitions (mappings);
  }

  // nothing to do for these
  void visit (TyTy::InferType &type) override { resolved = type.clone (); }
  void visit (TyTy::FnPtr &type) override { resolved = type.clone (); }
  void visit (TyTy::BoolType &type) override { resolved = type.clone (); }
  void visit (TyTy::IntType &type) override { resolved = type.clone (); }
  void visit (TyTy::UintType &type) override { resolved = type.clone (); }
  void visit (TyTy::FloatType &type) override { resolved = type.clone (); }
  void visit (TyTy::USizeType &type) override { resolved = type.clone (); }
  void visit (TyTy::ISizeType &type) override { resolved = type.clone (); }
  void visit (TyTy::ErrorType &type) override { resolved = type.clone (); }
  void visit (TyTy::CharType &type) override { resolved = type.clone (); }
  void visit (TyTy::StrType &type) override { resolved = type.clone (); }
  void visit (TyTy::NeverType &type) override { resolved = type.clone (); }
  void visit (TyTy::DynamicObjectType &type) override
  {
    resolved = type.clone ();
  }

private:
  SubstMapperInternal (HirId ref, TyTy::SubstitutionArgumentMappings &mappings)
    : resolved (new TyTy::ErrorType (ref)), mappings (mappings)
  {}

  TyTy::BaseType *resolved;
  TyTy::SubstitutionArgumentMappings &mappings;
};

class SubstMapperFromExisting : public TyTy::TyVisitor
{
public:
  static TyTy::BaseType *Resolve (TyTy::BaseType *concrete,
				  TyTy::BaseType *receiver)
  {
    rust_assert (concrete->get_kind () == receiver->get_kind ());

    SubstMapperFromExisting mapper (concrete, receiver);
    concrete->accept_vis (mapper);
    return mapper.resolved;
  }

  void visit (TyTy::FnType &type) override
  {
    rust_assert (type.was_substituted ());

    TyTy::FnType *to_sub = static_cast<TyTy::FnType *> (receiver);
    resolved = to_sub->handle_substitions (type.get_substitution_arguments ());
  }

  void visit (TyTy::ADTType &type) override
  {
    rust_assert (type.was_substituted ());

    TyTy::ADTType *to_sub = static_cast<TyTy::ADTType *> (receiver);
    resolved = to_sub->handle_substitions (type.get_substitution_arguments ());
  }

  void visit (TyTy::ClosureType &type) override
  {
    rust_assert (type.was_substituted ());

    TyTy::ClosureType *to_sub = static_cast<TyTy::ClosureType *> (receiver);
    resolved = to_sub->handle_substitions (type.get_substitution_arguments ());
  }

  void visit (TyTy::InferType &) override { gcc_unreachable (); }
  void visit (TyTy::TupleType &) override { gcc_unreachable (); }
  void visit (TyTy::FnPtr &) override { gcc_unreachable (); }
  void visit (TyTy::ArrayType &) override { gcc_unreachable (); }
  void visit (TyTy::SliceType &) override { gcc_unreachable (); }
  void visit (TyTy::BoolType &) override { gcc_unreachable (); }
  void visit (TyTy::IntType &) override { gcc_unreachable (); }
  void visit (TyTy::UintType &) override { gcc_unreachable (); }
  void visit (TyTy::FloatType &) override { gcc_unreachable (); }
  void visit (TyTy::USizeType &) override { gcc_unreachable (); }
  void visit (TyTy::ISizeType &) override { gcc_unreachable (); }
  void visit (TyTy::ErrorType &) override { gcc_unreachable (); }
  void visit (TyTy::CharType &) override { gcc_unreachable (); }
  void visit (TyTy::ReferenceType &) override { gcc_unreachable (); }
  void visit (TyTy::PointerType &) override { gcc_unreachable (); }
  void visit (TyTy::ParamType &) override { gcc_unreachable (); }
  void visit (TyTy::StrType &) override { gcc_unreachable (); }
  void visit (TyTy::NeverType &) override { gcc_unreachable (); }
  void visit (TyTy::PlaceholderType &) override { gcc_unreachable (); }
  void visit (TyTy::ProjectionType &) override { gcc_unreachable (); }
  void visit (TyTy::DynamicObjectType &) override { gcc_unreachable (); }

private:
  SubstMapperFromExisting (TyTy::BaseType *concrete, TyTy::BaseType *receiver)
    : concrete (concrete), receiver (receiver), resolved (nullptr)
  {}

  TyTy::BaseType *concrete;
  TyTy::BaseType *receiver;

  TyTy::BaseType *resolved;
};

class GetUsedSubstArgs : public TyTy::TyConstVisitor
{
public:
  static TyTy::SubstitutionArgumentMappings From (const TyTy::BaseType *from)
  {
    GetUsedSubstArgs mapper;
    from->accept_vis (mapper);
    return mapper.args;
  }

  void visit (const TyTy::FnType &type) override
  {
    args = type.get_substitution_arguments ();
  }

  void visit (const TyTy::ADTType &type) override
  {
    args = type.get_substitution_arguments ();
  }

  void visit (const TyTy::ClosureType &type) override
  {
    args = type.get_substitution_arguments ();
  }

  void visit (const TyTy::InferType &) override {}
  void visit (const TyTy::TupleType &) override {}
  void visit (const TyTy::FnPtr &) override {}
  void visit (const TyTy::ArrayType &) override {}
  void visit (const TyTy::SliceType &) override {}
  void visit (const TyTy::BoolType &) override {}
  void visit (const TyTy::IntType &) override {}
  void visit (const TyTy::UintType &) override {}
  void visit (const TyTy::FloatType &) override {}
  void visit (const TyTy::USizeType &) override {}
  void visit (const TyTy::ISizeType &) override {}
  void visit (const TyTy::ErrorType &) override {}
  void visit (const TyTy::CharType &) override {}
  void visit (const TyTy::ReferenceType &) override {}
  void visit (const TyTy::PointerType &) override {}
  void visit (const TyTy::ParamType &) override {}
  void visit (const TyTy::StrType &) override {}
  void visit (const TyTy::NeverType &) override {}
  void visit (const TyTy::PlaceholderType &) override {}
  void visit (const TyTy::ProjectionType &) override {}
  void visit (const TyTy::DynamicObjectType &) override {}

private:
  GetUsedSubstArgs () : args (TyTy::SubstitutionArgumentMappings::error ()) {}

  TyTy::SubstitutionArgumentMappings args;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_SUBSTITUTION_MAPPER_H
