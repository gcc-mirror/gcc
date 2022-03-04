// Copyright (C) 2020-2022 Free Software Foundation, Inc.

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
#include "rust-compile-expr.h"
#include "rust-constexpr.h"

#include "tree.h"
#include "print-tree.h"

namespace Rust {
namespace Compile {

static const std::string RUST_ENUM_DISR_FIELD_NAME = "RUST$ENUM$DISR";

// see: gcc/c/c-decl.cc:8230-8241
// https://github.com/Rust-GCC/gccrs/blob/0024bc2f028369b871a65ceb11b2fddfb0f9c3aa/gcc/c/c-decl.c#L8229-L8241
tree
TyTyResolveCompile::get_implicit_enumeral_node_type (Context *ctx)
{
  // static tree enum_node = NULL_TREE;
  // if (enum_node == NULL_TREE)
  //   {
  //     enum_node = make_node (ENUMERAL_TYPE);
  //     SET_TYPE_MODE (enum_node, TYPE_MODE (unsigned_type_node));
  //     SET_TYPE_ALIGN (enum_node, TYPE_ALIGN (unsigned_type_node));
  //     TYPE_USER_ALIGN (enum_node) = 0;
  //     TYPE_UNSIGNED (enum_node) = 1;
  //     TYPE_PRECISION (enum_node) = TYPE_PRECISION (unsigned_type_node);
  //     TYPE_MIN_VALUE (enum_node) = TYPE_MIN_VALUE (unsigned_type_node);
  //     TYPE_MAX_VALUE (enum_node) = TYPE_MAX_VALUE (unsigned_type_node);

  //     // tree identifier = ctx->get_backend ()->get_identifier_node
  //     // ("enumeral"); tree enum_decl
  //     //   = build_decl (BUILTINS_LOCATION, TYPE_DECL, identifier,
  //     enum_node);
  //     // TYPE_NAME (enum_node) = enum_decl;
  //   }
  // return enum_node;

  static tree enum_node = NULL_TREE;
  if (enum_node == NULL_TREE)
    {
      enum_node = ctx->get_backend ()->named_type (
	"enumeral", ctx->get_backend ()->integer_type (false, 64),
	Linemap::predeclared_location ());
    }
  return enum_node;
}

void
TyTyResolveCompile::visit (const TyTy::ErrorType &)
{
  translated = error_mark_node;
}

void
TyTyResolveCompile::visit (const TyTy::InferType &)
{
  translated = error_mark_node;
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
    translated
      = ctx->get_backend ()->function_type (receiver, parameters, results, NULL,
					    type.get_ident ().locus);
  else
    translated
      = ctx->get_backend ()->function_type_varadic (receiver, parameters,
						    results, NULL,
						    type.get_ident ().locus);
}

void
TyTyResolveCompile::visit (const TyTy::FnPtr &type)
{
  tree result_type = TyTyResolveCompile::compile (ctx, type.get_return_type ());

  std::vector<tree> parameters;

  auto &params = type.get_params ();
  for (auto &p : params)
    {
      tree pty = TyTyResolveCompile::compile (ctx, p.get_tyty ());
      parameters.push_back (pty);
    }

  translated = ctx->get_backend ()->function_ptr_type (result_type, parameters,
						       type.get_ident ().locus);
}

void
TyTyResolveCompile::visit (const TyTy::ADTType &type)
{
  if (ctx->lookup_compiled_types (type.get_ty_ref (), &translated, &type))
    return;

  tree type_record = error_mark_node;
  if (!type.is_enum ())
    {
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

      type_record = type.is_union ()
		      ? ctx->get_backend ()->union_type (fields)
		      : ctx->get_backend ()->struct_type (fields);
    }
  else
    {
      // see:
      // https://github.com/bminor/binutils-gdb/blob/527b8861cd472385fa9160a91dd6d65a25c41987/gdb/dwarf2/read.c#L9010-L9241
      //
      // enums are actually a big union so for example the rust enum:
      //
      // enum AnEnum {
      //   A,
      //   B,
      //   C (char),
      //   D { x: i64, y: i64 },
      // }
      //
      // we actually turn this into
      //
      // union {
      //   struct A { int RUST$ENUM$DISR; }; <- this is a data-less variant
      //   struct B { int RUST$ENUM$DISR; }; <- this is a data-less variant
      //   struct C { int RUST$ENUM$DISR; char __0; };
      //   struct D { int RUST$ENUM$DISR; i64 x; i64 y; };
      // }
      //
      // Ada, qual_union_types might still work for this but I am not 100% sure.
      // I ran into some issues lets reuse our normal union and ask Ada people
      // about it.

      std::vector<tree> variant_records;
      for (auto &variant : type.get_variants ())
	{
	  std::vector<Backend::typed_identifier> fields;

	  // add in the qualifier field for the variant
	  tree enumeral_type
	    = TyTyResolveCompile::get_implicit_enumeral_node_type (ctx);
	  Backend::typed_identifier f (RUST_ENUM_DISR_FIELD_NAME, enumeral_type,
				       ctx->get_mappings ()->lookup_location (
					 variant->get_id ()));
	  fields.push_back (std::move (f));

	  // compile the rest of the fields
	  for (size_t i = 0; i < variant->num_fields (); i++)
	    {
	      const TyTy::StructFieldType *field
		= variant->get_field_at_index (i);
	      tree compiled_field_ty
		= TyTyResolveCompile::compile (ctx, field->get_field_type ());

	      std::string field_name = field->get_name ();
	      if (variant->get_variant_type ()
		  == TyTy::VariantDef::VariantType::TUPLE)
		field_name = "__" + field->get_name ();

	      Backend::typed_identifier f (
		field_name, compiled_field_ty,
		ctx->get_mappings ()->lookup_location (type.get_ty_ref ()));
	      fields.push_back (std::move (f));
	    }

	  tree variant_record = ctx->get_backend ()->struct_type (fields);
	  tree named_variant_record = ctx->get_backend ()->named_type (
	    variant->get_ident ().path.get (), variant_record,
	    variant->get_ident ().locus);

	  // set the qualifier to be a builtin
	  DECL_ARTIFICIAL (TYPE_FIELDS (variant_record)) = 1;

	  // add them to the list
	  variant_records.push_back (named_variant_record);
	}

      // now we need to make the actual union, but first we need to make
      // named_type TYPE_DECL's out of the variants

      size_t i = 0;
      std::vector<Backend::typed_identifier> enum_fields;
      for (auto &variant_record : variant_records)
	{
	  TyTy::VariantDef *variant = type.get_variants ().at (i++);
	  std::string implicit_variant_name = variant->get_identifier ();

	  Backend::typed_identifier f (implicit_variant_name, variant_record,
				       ctx->get_mappings ()->lookup_location (
					 type.get_ty_ref ()));
	  enum_fields.push_back (std::move (f));
	}

      // finally make the union or the enum
      type_record = ctx->get_backend ()->union_type (enum_fields);
    }

  std::string named_struct_str
    = type.get_ident ().path.get () + type.subst_as_string ();
  tree named_struct
    = ctx->get_backend ()->named_type (named_struct_str, type_record,
				       type.get_ident ().locus);

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
				       type.get_ident ().locus);

  ctx->push_type (named_struct);
  ctx->insert_compiled_type (type.get_ty_ref (), named_struct, &type);
  translated = named_struct;
}

void
TyTyResolveCompile::visit (const TyTy::ArrayType &type)
{
  tree element_type
    = TyTyResolveCompile::compile (ctx, type.get_element_type ());
  tree capacity_expr = CompileExpr::Compile (&type.get_capacity_expr (), ctx);
  tree folded_capacity_expr = ConstCtx::fold (capacity_expr);

  translated
    = ctx->get_backend ()->array_type (element_type, folded_capacity_expr);
}

void
TyTyResolveCompile::visit (const TyTy::SliceType &type)
{
  // TODO
  gcc_unreachable ();
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
				       type.get_ident ().locus);

  ctx->push_type (named_struct);
  translated = named_struct;

  ctx->insert_compiled_type (type.get_ty_ref (), named_struct, &type);
}

} // namespace Compile
} // namespace Rust
