// Copyright (C) 2026 Free Software Foundation, Inc.

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

#include "rust-attribute-values.h"
#include "rust-derive-coerce-pointee.h"
#include "rust-feature-store.h"
#include "rust-feature.h"
#include "rust-session-manager.h"

namespace Rust {
namespace AST {

DeriveCoercePointee::DeriveCoercePointee (location_t loc,
					  Builder::Source item_source)
  : DeriveVisitor (loc, item_source)
{}

std::unique_ptr<AST::Item>
DeriveCoercePointee::go (Item &item)
{
  item.accept_vis (*this);
  Features::EarlyFeatureGateStore::get ().add (
    Feature::Name::DERIVE_COERCE_POINTEE,
    Error (loc, "use of unstable library feature %<derive_coerce_pointee%>"));

  // NOTE: This is technically a library feature, so I don't think this is
  // how it should be gated. But as far as I can tell at the moment we have
  // no mechanisms for handling these, and furthermore this will go through
  // our compatibility layer.
  //
  // What this means is that the `CoercePointee` macro will *not* be defined
  // in the `core` that we will be using to compile the kernel at first.
  //
  // The basic example for `CoercePointee` is something like this:
  //
  // ```rust
  // use std::marker::CoercePointee;
  //
  // #[derive(CoercePointee)]
  // struct Flip<Flop>(*const Flop);
  // ```
  //
  // but the marker will not be present in the `core` we will be using at
  // first, so we will need to fake its existence with
  // -frust-compat-version, and then the compiler can assume that it is a
  // built-in derive. At least that's my expectation.

  if (!Session::get_instance ().should_support_coerce_pointee ())
    {
      rust_error_at (loc, "derive(CoercePointee) requires a compatibility mode "
			  "greater or equal to 1.84");
      return {};
    }

  rust_warning_at (
    loc, 0,
    "derive(CoercePointee) is currently unimplemented and has no effect");

  return {};
}

// CoercePointee requires the Struct or Tuple to have transparent representation
void
DeriveCoercePointee::validate_repr_transparent (const Item &item)
{
  const auto &attrs = item.get_outer_attrs ();
  for (const auto &attr : attrs)
    {
      if (attr.get_path ().as_string () != Values::Attributes::REPR)
	continue;
      rust_assert (!attr.empty_input ());

      const auto &attr_input = attr.get_attr_input ();
      rust_assert (attr_input.get_attr_input_type ()
		   == AST::AttrInput::AttrInputType::TOKEN_TREE);

      auto meta_item = static_cast<const AST::DelimTokenTree &> (attr_input)
			 .parse_to_meta_item ();
      rust_assert (meta_item);

      for (const auto &item : meta_item->get_items ())
	if (item->as_string () == "transparent")
	  return;
    }
  rust_error_at (item.get_locus (), ErrorCode::E0802,
		 "%<CoercePointee%> is only applicable to struct/tuple with "
		 "repr(transparent) layout");
}

// Structs or Tuples with CoercePointee must have minimum of one field else
// compile time error
void
DeriveCoercePointee::validate_number_of_fields (const Item &item)
{
  rust_assert (item.get_item_kind () == Item::Kind::Struct);

  auto &base_struct = static_cast<const Struct &> (item);
  size_t field_count = base_struct.get_field_count ();

  if (field_count == 0)
    rust_error_at (item.get_locus (), ErrorCode::E0802,
		   "%<CoercePointee%> can only be derived on a struct "
		   "with at least one field");
}

// #[pointee] needs to be on a generic type parameter, and if there are 2 or
// more generic parameters (not traits), one of them must have the attribute
void
DeriveCoercePointee::validate_non_generic_pointee (const Rust::AST::Item &item)
{
  rust_assert (item.get_item_kind () == Item::Kind::Struct);
  const StructStruct &struct_item = static_cast<const StructStruct &> (item);
  const std::vector<std::unique_ptr<GenericParam>> &generic_params
    = struct_item.get_generic_params ();

  // Implicitly it becomes the pointee
  if (generic_params.size () == 1)
    return;

  size_t pointee_count = 0;

  for (size_t i = 0; i < generic_params.size (); ++i)
    {
      if (generic_params[i].get ()->get_kind () != GenericParam::Kind::Const
	  && (generic_params)[i].get ()->get_kind ()
	       != GenericParam::Kind::Lifetime)
	{
	  auto &type_param
	    = static_cast<TypeParam &> (*(generic_params)[i].get ());

	  for (const auto &attr : type_param.get_outer_attrs ())
	    {
	      if (attr.get_path ().as_string () == Values::Attributes::POINTEE)
		pointee_count += 1;
	    }
	}
    }

  if (pointee_count == 0 || pointee_count > 1)
    rust_error_at (item.get_locus (), ErrorCode::E0802,
		   "exactly one generic type parameter must be marked "
		   "as %<#[pointee]%> to derive %<CoercePointee%> traits");
}

// validates all the requirements for a type to be derived as CoercePointee
void
DeriveCoercePointee::validate (const Item &item)
{
  validate_repr_transparent (item);
  validate_number_of_fields (item);
  validate_non_generic_pointee (item);
}

void
DeriveCoercePointee::visit_struct (StructStruct &item)
{
  validate (item);
}

void
DeriveCoercePointee::visit_tuple (TupleStruct &item)
{
  validate (item);
}

void
DeriveCoercePointee::visit_enum (Enum &item)
{
  error_for_non_supported_objects (item);
}

void
DeriveCoercePointee::visit_union (Union &item)
{
  error_for_non_supported_objects (item);
}

void
DeriveCoercePointee::error_for_non_supported_objects (const Item &item)
{
  rust_error_at (item.get_locus (), ErrorCode::E0802,
		 "%<CoercePointee%> can only be derived on structs with "
		 "%<#[repr(transparent)]%>");
}
} // namespace AST
} // namespace Rust
