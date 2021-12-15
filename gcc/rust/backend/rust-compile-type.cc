// Copyright (C) 2020-2021 Free Software Foundation, Inc.

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

#include "rust-compile-type.h"

namespace Rust {
namespace Compile {

void
TyTyResolveCompile::visit (const TyTy::ErrorType &)
{
  gcc_unreachable ();
}
void
TyTyResolveCompile::visit (const TyTy::InferType &)
{
  gcc_unreachable ();
}
void
TyTyResolveCompile::visit (const TyTy::ClosureType &)
{
  gcc_unreachable ();
}

void
TyTyResolveCompile::visit (const TyTy::ProjectionType &type)
{
  type.get ()->accept_vis (*this);
}

void
TyTyResolveCompile::visit (const TyTy::PlaceholderType &type)
{
  type.resolve ()->accept_vis (*this);
}

void
TyTyResolveCompile::visit (const TyTy::ParamType &param)
{
  recursion_count++;
  rust_assert (recursion_count < kDefaultRecusionLimit);

  param.resolve ()->accept_vis (*this);
}

void
TyTyResolveCompile::visit (const TyTy::FnType &type)
{
  Backend::typed_identifier receiver;
  std::vector<Backend::typed_identifier> parameters;
  std::vector<Backend::typed_identifier> results;

  if (!type.get_return_type ()->is_unit ())
    {
      auto hir_type = type.get_return_type ();
      auto ret = TyTyResolveCompile::compile (ctx, hir_type, trait_object_mode);
      results.push_back (Backend::typed_identifier (
	"_", ret,
	ctx->get_mappings ()->lookup_location (hir_type->get_ref ())));
    }

  for (auto &param_pair : type.get_params ())
    {
      auto param_tyty = param_pair.second;
      auto compiled_param_type
	= TyTyResolveCompile::compile (ctx, param_tyty, trait_object_mode);

      auto compiled_param = Backend::typed_identifier (
	param_pair.first->as_string (), compiled_param_type,
	ctx->get_mappings ()->lookup_location (param_tyty->get_ref ()));

      parameters.push_back (compiled_param);
    }

  if (!type.is_varadic ())
    translated = ctx->get_backend ()->function_type (
      receiver, parameters, results, NULL,
      ctx->get_mappings ()->lookup_location (type.get_ref ()));
  else
    translated = ctx->get_backend ()->function_type_varadic (
      receiver, parameters, results, NULL,
      ctx->get_mappings ()->lookup_location (type.get_ref ()));
}

void
TyTyResolveCompile::visit (const TyTy::FnPtr &type)
{
  tree result_type = TyTyResolveCompile::compile (ctx, type.get_return_type ());

  std::vector<tree> parameters;
  type.iterate_params ([&] (TyTy::BaseType *p) mutable -> bool {
    tree pty = TyTyResolveCompile::compile (ctx, p);
    parameters.push_back (pty);
    return true;
  });

  translated = ctx->get_backend ()->function_ptr_type (
    result_type, parameters,
    ctx->get_mappings ()->lookup_location (type.get_ref ()));
}

void
TyTyResolveCompile::visit (const TyTy::ADTType &type)
{
  if (ctx->lookup_compiled_types (type.get_ty_ref (), &translated, &type))
    return;

  // we dont support enums yet
  rust_assert (!type.is_enum ());
  rust_assert (type.number_of_variants () == 1);

  TyTy::VariantDef &variant = *type.get_variants ().at (0);
  std::vector<Backend::typed_identifier> fields;
  for (size_t i = 0; i < variant.num_fields (); i++)
    {
      const TyTy::StructFieldType *field = variant.get_field_at_index (i);
      tree compiled_field_ty
	= TyTyResolveCompile::compile (ctx, field->get_field_type ());

      Backend::typed_identifier f (field->get_name (), compiled_field_ty,
				   ctx->get_mappings ()->lookup_location (
				     type.get_ty_ref ()));
      fields.push_back (std::move (f));
    }

  tree type_record;
  if (type.is_union ())
    type_record = ctx->get_backend ()->union_type (fields);
  else
    type_record = ctx->get_backend ()->struct_type (fields);
  tree named_struct
    = ctx->get_backend ()->named_type (type.get_name (), type_record,
				       ctx->get_mappings ()->lookup_location (
					 type.get_ty_ref ()));

  ctx->push_type (named_struct);
  translated = named_struct;

  ctx->insert_compiled_type (type.get_ty_ref (), named_struct, &type);
}

void
TyTyResolveCompile::visit (const TyTy::TupleType &type)
{
  if (type.num_fields () == 0)
    {
      translated = ctx->get_backend ()->unit_type ();
      return;
    }

  bool ok = ctx->lookup_compiled_types (type.get_ty_ref (), &translated, &type);
  if (ok)
    return;

  // create implicit struct
  std::vector<Backend::typed_identifier> fields;
  for (size_t i = 0; i < type.num_fields (); i++)
    {
      TyTy::BaseType *field = type.get_field (i);
      tree compiled_field_ty = TyTyResolveCompile::compile (ctx, field);

      // rustc uses the convention __N, where N is an integer, to
      // name the fields of a tuple.  We follow this as well,
      // because this is used by GDB.  One further reason to prefer
      // this, rather than simply emitting the integer, is that this
      // approach makes it simpler to use a C-only debugger, or
      // GDB's C mode, when debugging Rust.
      Backend::typed_identifier f ("__" + std::to_string (i), compiled_field_ty,
				   ctx->get_mappings ()->lookup_location (
				     type.get_ty_ref ()));
      fields.push_back (std::move (f));
    }

  tree struct_type_record = ctx->get_backend ()->struct_type (fields);
  tree named_struct
    = ctx->get_backend ()->named_type (type.as_string (), struct_type_record,
				       ctx->get_mappings ()->lookup_location (
					 type.get_ty_ref ()));

  ctx->push_type (named_struct);
  ctx->insert_compiled_type (type.get_ty_ref (), named_struct, &type);
  translated = named_struct;
}

void
TyTyResolveCompile::visit (const TyTy::ArrayType &type)
{
  tree element_type
    = TyTyResolveCompile::compile (ctx, type.get_element_type ());
  translated
    = ctx->get_backend ()->array_type (element_type, type.get_capacity ());
}

void
TyTyResolveCompile::visit (const TyTy::BoolType &type)
{
  tree compiled_type = nullptr;
  bool ok = ctx->lookup_compiled_types (type.get_ty_ref (), &compiled_type);
  rust_assert (ok);
  translated = compiled_type;
}

void
TyTyResolveCompile::visit (const TyTy::IntType &type)
{
  tree compiled_type = nullptr;
  bool ok = ctx->lookup_compiled_types (type.get_ty_ref (), &compiled_type);
  rust_assert (ok);
  translated = compiled_type;
}

void
TyTyResolveCompile::visit (const TyTy::UintType &type)
{
  tree compiled_type = nullptr;
  bool ok = ctx->lookup_compiled_types (type.get_ty_ref (), &compiled_type);
  rust_assert (ok);
  translated = compiled_type;
}

void
TyTyResolveCompile::visit (const TyTy::FloatType &type)
{
  tree compiled_type = nullptr;
  bool ok = ctx->lookup_compiled_types (type.get_ty_ref (), &compiled_type);
  rust_assert (ok);
  translated = compiled_type;
}

void
TyTyResolveCompile::visit (const TyTy::USizeType &type)
{
  tree compiled_type = nullptr;
  bool ok = ctx->lookup_compiled_types (type.get_ty_ref (), &compiled_type);
  rust_assert (ok);
  translated = compiled_type;
}

void
TyTyResolveCompile::visit (const TyTy::ISizeType &type)
{
  tree compiled_type = nullptr;
  bool ok = ctx->lookup_compiled_types (type.get_ty_ref (), &compiled_type);
  rust_assert (ok);
  translated = compiled_type;
}

void
TyTyResolveCompile::visit (const TyTy::CharType &type)
{
  tree compiled_type = nullptr;
  bool ok = ctx->lookup_compiled_types (type.get_ty_ref (), &compiled_type);
  rust_assert (ok);
  translated = compiled_type;
}

void
TyTyResolveCompile::visit (const TyTy::ReferenceType &type)
{
  tree base_compiled_type
    = TyTyResolveCompile::compile (ctx, type.get_base (), trait_object_mode);
  if (type.is_mutable ())
    {
      translated = ctx->get_backend ()->reference_type (base_compiled_type);
    }
  else
    {
      auto base = ctx->get_backend ()->immutable_type (base_compiled_type);
      translated = ctx->get_backend ()->reference_type (base);
    }
}

void
TyTyResolveCompile::visit (const TyTy::PointerType &type)
{
  tree base_compiled_type
    = TyTyResolveCompile::compile (ctx, type.get_base (), trait_object_mode);
  if (type.is_mutable ())
    {
      translated = ctx->get_backend ()->pointer_type (base_compiled_type);
    }
  else
    {
      auto base = ctx->get_backend ()->immutable_type (base_compiled_type);
      translated = ctx->get_backend ()->pointer_type (base);
    }
}

void
TyTyResolveCompile::visit (const TyTy::StrType &type)
{
  tree compiled_type = nullptr;
  bool ok = ctx->lookup_compiled_types (type.get_ty_ref (), &compiled_type);
  rust_assert (ok);
  translated = compiled_type;
}

void
TyTyResolveCompile::visit (const TyTy::NeverType &)
{
  translated = ctx->get_backend ()->unit_type ();
}

void
TyTyResolveCompile::visit (const TyTy::DynamicObjectType &type)
{
  if (trait_object_mode)
    {
      translated = ctx->get_backend ()->integer_type (
	true, ctx->get_backend ()->get_pointer_size ());
      return;
    }

  if (ctx->lookup_compiled_types (type.get_ty_ref (), &translated, &type))
    return;

  // create implicit struct
  auto items = type.get_object_items ();
  std::vector<Backend::typed_identifier> fields;

  tree uint = ctx->get_backend ()->integer_type (
    true, ctx->get_backend ()->get_pointer_size ());
  tree uintptr_ty = ctx->get_backend ()->pointer_type (uint);

  Backend::typed_identifier f ("__receiver_trait_obj_ptr", uintptr_ty,
			       ctx->get_mappings ()->lookup_location (
				 type.get_ty_ref ()));
  fields.push_back (std::move (f));

  for (size_t i = 0; i < items.size (); i++)
    {
      // mrustc seems to make a vtable consisting of uintptr's
      tree uint = ctx->get_backend ()->integer_type (
	true, ctx->get_backend ()->get_pointer_size ());
      tree uintptr_ty = ctx->get_backend ()->pointer_type (uint);

      Backend::typed_identifier f ("__" + std::to_string (i), uintptr_ty,
				   ctx->get_mappings ()->lookup_location (
				     type.get_ty_ref ()));
      fields.push_back (std::move (f));
    }

  tree type_record = ctx->get_backend ()->struct_type (fields);
  tree named_struct
    = ctx->get_backend ()->named_type (type.get_name (), type_record,
				       ctx->get_mappings ()->lookup_location (
					 type.get_ty_ref ()));

  ctx->push_type (named_struct);
  translated = named_struct;

  ctx->insert_compiled_type (type.get_ty_ref (), named_struct, &type);
}

} // namespace Compile
} // namespace Rust
