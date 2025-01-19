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

#include "rust-compile-type.h"
#include "rust-compile-expr.h"
#include "rust-constexpr.h"
#include "rust-gcc.h"

#include "tree.h"

namespace Rust {
namespace Compile {

static const std::string RUST_ENUM_DISR_FIELD_NAME = "RUST$ENUM$DISR";

TyTyResolveCompile::TyTyResolveCompile (Context *ctx, bool trait_object_mode)
  : ctx (ctx), trait_object_mode (trait_object_mode),
    translated (error_mark_node)
{}

tree
TyTyResolveCompile::compile (Context *ctx, const TyTy::BaseType *ty,
			     bool trait_object_mode)
{
  TyTyResolveCompile compiler (ctx, trait_object_mode);
  const TyTy::BaseType *destructured = ty->destructure ();
  destructured->accept_vis (compiler);

  if (compiler.translated != error_mark_node
      && TYPE_NAME (compiler.translated) != NULL)
    {
      // canonicalize the type
      compiler.translated = ctx->insert_compiled_type (compiler.translated);
    }

  return compiler.translated;
}

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
      // equivalent to isize
      enum_node = Backend::named_type (
	"enumeral", Backend::integer_type (false, Backend::get_pointer_size ()),
	BUILTINS_LOCATION);
    }
  return enum_node;
}

tree
TyTyResolveCompile::get_unit_type (Context *ctx)
{
  static tree unit_type;
  if (unit_type == nullptr)
    {
      auto unit_type_node = Backend::struct_type ({});
      unit_type = Backend::named_type ("()", unit_type_node, BUILTINS_LOCATION);
    }
  return unit_type;
}

void
TyTyResolveCompile::visit (const TyTy::ErrorType &)
{
  translated = error_mark_node;
}

void
TyTyResolveCompile::visit (const TyTy::InferType &type)
{
  const TyTy::BaseType *orig = &type;
  TyTy::BaseType *lookup = nullptr;
  bool ok = ctx->get_tyctx ()->lookup_type (type.get_ref (), &lookup);
  if (!ok)
    {
      translated = error_mark_node;
      return;
    }

  if (orig == lookup)
    {
      translated = error_mark_node;
      return;
    }

  translated = TyTyResolveCompile::compile (ctx, lookup);
}

void
TyTyResolveCompile::visit (const TyTy::ParamType &)
{
  translated = error_mark_node;
}

void
TyTyResolveCompile::visit (const TyTy::ProjectionType &type)
{
  translated = error_mark_node;
}

void
TyTyResolveCompile::visit (const TyTy::PlaceholderType &type)
{
  translated = error_mark_node;
}

void
TyTyResolveCompile::visit (const TyTy::ClosureType &type)
{
  auto mappings = ctx->get_mappings ();

  std::vector<Backend::typed_identifier> fields;

  size_t i = 0;
  for (const auto &capture : type.get_captures ())
    {
      // lookup the HirId
      HirId ref = UNKNOWN_HIRID;
      bool ok = mappings->lookup_node_to_hir (capture, &ref);
      rust_assert (ok);

      // lookup the var decl type
      TyTy::BaseType *lookup = nullptr;
      bool found = ctx->get_tyctx ()->lookup_type (ref, &lookup);
      rust_assert (found);

      // FIXME get the var pattern name
      std::string mappings_name = "capture_" + std::to_string (i);

      // FIXME
      // this should be based on the closure move-ability
      tree decl_type = TyTyResolveCompile::compile (ctx, lookup);
      tree capture_type = build_reference_type (decl_type);
      fields.push_back (Backend::typed_identifier (mappings_name, capture_type,
						   type.get_ident ().locus));
    }

  tree type_record = Backend::struct_type (fields);
  RS_CLOSURE_FLAG (type_record) = 1;

  std::string named_struct_str
    = type.get_ident ().path.get () + "::{{closure}}";
  translated = Backend::named_type (named_struct_str, type_record,
				    type.get_ident ().locus);
}

void
TyTyResolveCompile::visit (const TyTy::FnType &type)
{
  Backend::typed_identifier receiver;
  std::vector<Backend::typed_identifier> parameters;
  std::vector<Backend::typed_identifier> results;

  // we can only return unit-type if its not the C ABI because it will expect
  // void
  auto hir_type = type.get_return_type ()->destructure ();
  bool return_is_unit = hir_type->is_unit ();
  bool is_c_abi = type.get_abi () == ABI::C;
  bool should_be_void = is_c_abi && return_is_unit;
  if (!should_be_void)
    {
      auto ret = TyTyResolveCompile::compile (ctx, hir_type, trait_object_mode);
      location_t return_type_locus
	= ctx->get_mappings ()->lookup_location (hir_type->get_ref ());
      results.push_back (
	Backend::typed_identifier ("_", ret, return_type_locus));
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

  if (!type.is_variadic ())
    translated = Backend::function_type (receiver, parameters, results, NULL,
					 type.get_ident ().locus);
  else
    translated
      = Backend::function_type_variadic (receiver, parameters, results, NULL,
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

  translated = Backend::function_ptr_type (result_type, parameters,
					   type.get_ident ().locus);
}

void
TyTyResolveCompile::visit (const TyTy::ADTType &type)
{
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

      type_record = type.is_union () ? Backend::union_type (fields)
				     : Backend::struct_type (fields);
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

	  tree variant_record = Backend::struct_type (fields);
	  tree named_variant_record
	    = Backend::named_type (variant->get_ident ().path.get (),
				   variant_record, variant->get_ident ().locus);

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
      type_record = Backend::union_type (enum_fields);
    }

  // Handle repr options
  // TODO: "packed" should only narrow type alignment and "align" should only
  // widen it. Do we need to check and enforce this here, or is it taken care of
  // later on in the gcc middle-end?
  TyTy::ADTType::ReprOptions repr = type.get_repr_options ();
  if (repr.pack)
    {
      TYPE_PACKED (type_record) = 1;
      if (repr.pack > 1)
	{
	  SET_TYPE_ALIGN (type_record, repr.pack * 8);
	  TYPE_USER_ALIGN (type_record) = 1;
	}
    }
  else if (repr.align)
    {
      SET_TYPE_ALIGN (type_record, repr.align * 8);
      TYPE_USER_ALIGN (type_record) = 1;
    }

  std::string named_struct_str
    = type.get_ident ().path.get () + type.subst_as_string ();
  translated = Backend::named_type (named_struct_str, type_record,
				    type.get_ident ().locus);
}

void
TyTyResolveCompile::visit (const TyTy::TupleType &type)
{
  if (type.num_fields () == 0)
    {
      translated = get_unit_type (ctx);
      return;
    }

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

  tree struct_type_record = Backend::struct_type (fields);
  translated = Backend::named_type (type.as_string (), struct_type_record,
				    type.get_ident ().locus);
}

void
TyTyResolveCompile::visit (const TyTy::ArrayType &type)
{
  tree element_type
    = TyTyResolveCompile::compile (ctx, type.get_element_type ());

  ctx->push_const_context ();
  tree capacity_expr = CompileExpr::Compile (&type.get_capacity_expr (), ctx);
  ctx->pop_const_context ();

  tree folded_capacity_expr = fold_expr (capacity_expr);

  translated = Backend::array_type (element_type, folded_capacity_expr);
}

void
TyTyResolveCompile::visit (const TyTy::SliceType &type)
{
  tree type_record = create_slice_type_record (type);

  std::string named_struct_str
    = std::string ("[") + type.get_element_type ()->get_name () + "]";
  translated = Backend::named_type (named_struct_str, type_record,
				    type.get_ident ().locus);
}

void
TyTyResolveCompile::visit (const TyTy::BoolType &)
{
  translated
    = Backend::named_type ("bool", boolean_type_node, BUILTINS_LOCATION);
}

void
TyTyResolveCompile::visit (const TyTy::IntType &type)
{
  switch (type.get_int_kind ())
    {
    case TyTy::IntType::I8:
      translated = Backend::named_type ("i8", Backend::integer_type (false, 8),
					BUILTINS_LOCATION);
      return;

    case TyTy::IntType::I16:
      translated
	= Backend::named_type ("i16", Backend::integer_type (false, 16),
			       BUILTINS_LOCATION);
      return;

    case TyTy::IntType::I32:
      translated
	= Backend::named_type ("i32", Backend::integer_type (false, 32),
			       BUILTINS_LOCATION);
      return;

    case TyTy::IntType::I64:
      translated
	= Backend::named_type ("i64", Backend::integer_type (false, 64),
			       BUILTINS_LOCATION);
      return;

    case TyTy::IntType::I128:
      translated
	= Backend::named_type ("i128", Backend::integer_type (false, 128),
			       BUILTINS_LOCATION);
      return;
    }
}

void
TyTyResolveCompile::visit (const TyTy::UintType &type)
{
  switch (type.get_uint_kind ())
    {
    case TyTy::UintType::U8:
      translated = Backend::named_type ("u8", Backend::integer_type (true, 8),
					BUILTINS_LOCATION);
      return;

    case TyTy::UintType::U16:
      translated = Backend::named_type ("u16", Backend::integer_type (true, 16),
					BUILTINS_LOCATION);
      return;

    case TyTy::UintType::U32:
      translated = Backend::named_type ("u32", Backend::integer_type (true, 32),
					BUILTINS_LOCATION);
      return;

    case TyTy::UintType::U64:
      translated = Backend::named_type ("u64", Backend::integer_type (true, 64),
					BUILTINS_LOCATION);
      return;

    case TyTy::UintType::U128:
      translated
	= Backend::named_type ("u128", Backend::integer_type (true, 128),
			       BUILTINS_LOCATION);
      return;
    }
}

void
TyTyResolveCompile::visit (const TyTy::FloatType &type)
{
  switch (type.get_float_kind ())
    {
    case TyTy::FloatType::F32:
      translated = Backend::named_type ("f32", Backend::float_type (32),
					BUILTINS_LOCATION);
      return;

    case TyTy::FloatType::F64:
      translated = Backend::named_type ("f64", Backend::float_type (64),
					BUILTINS_LOCATION);
      return;
    }
}

void
TyTyResolveCompile::visit (const TyTy::USizeType &)
{
  translated
    = Backend::named_type ("usize",
			   Backend::integer_type (true,
						  Backend::get_pointer_size ()),
			   BUILTINS_LOCATION);
}

void
TyTyResolveCompile::visit (const TyTy::ISizeType &)
{
  translated
    = Backend::named_type ("isize",
			   Backend::integer_type (false,
						  Backend::get_pointer_size ()),
			   BUILTINS_LOCATION);
}

void
TyTyResolveCompile::visit (const TyTy::CharType &)
{
  translated
    = Backend::named_type ("char", Backend::wchar_type (), BUILTINS_LOCATION);
}

void
TyTyResolveCompile::visit (const TyTy::ReferenceType &type)
{
  const TyTy::SliceType *slice = nullptr;
  const TyTy::StrType *str = nullptr;
  const TyTy::DynamicObjectType *dyn = nullptr;
  if (type.is_dyn_slice_type (&slice))
    {
      tree type_record = create_slice_type_record (*slice);
      std::string dyn_slice_type_str
	= std::string (type.is_mutable () ? "&mut " : "&") + "["
	  + slice->get_element_type ()->get_name () + "]";

      translated = Backend::named_type (dyn_slice_type_str, type_record,
					slice->get_locus ());

      return;
    }
  else if (type.is_dyn_str_type (&str))
    {
      tree type_record = create_str_type_record (*str);
      std::string dyn_str_type_str
	= std::string (type.is_mutable () ? "&mut " : "&") + "str";

      translated = Backend::named_type (dyn_str_type_str, type_record,
					str->get_locus ());

      return;
    }
  else if (type.is_dyn_obj_type (&dyn))
    {
      tree type_record = create_dyn_obj_record (*dyn);
      std::string dyn_str_type_str
	= std::string (type.is_mutable () ? "&mut " : "& ") + dyn->get_name ();

      translated = Backend::named_type (dyn_str_type_str, type_record,
					dyn->get_locus ());

      return;
    }

  tree base_compiled_type
    = TyTyResolveCompile::compile (ctx, type.get_base (), trait_object_mode);
  if (type.is_mutable ())
    {
      translated = Backend::reference_type (base_compiled_type);
    }
  else
    {
      auto base = Backend::immutable_type (base_compiled_type);
      translated = Backend::reference_type (base);
    }
}

void
TyTyResolveCompile::visit (const TyTy::PointerType &type)
{
  const TyTy::SliceType *slice = nullptr;
  const TyTy::StrType *str = nullptr;
  const TyTy::DynamicObjectType *dyn = nullptr;
  if (type.is_dyn_slice_type (&slice))
    {
      tree type_record = create_slice_type_record (*slice);
      std::string dyn_slice_type_str
	= std::string (type.is_mutable () ? "*mut " : "*const ") + "["
	  + slice->get_element_type ()->get_name () + "]";

      translated = Backend::named_type (dyn_slice_type_str, type_record,
					slice->get_locus ());

      return;
    }
  else if (type.is_dyn_str_type (&str))
    {
      tree type_record = create_str_type_record (*str);
      std::string dyn_str_type_str
	= std::string (type.is_mutable () ? "*mut " : "*const ") + "str";

      translated = Backend::named_type (dyn_str_type_str, type_record,
					str->get_locus ());

      return;
    }
  else if (type.is_dyn_obj_type (&dyn))
    {
      tree type_record = create_dyn_obj_record (*dyn);
      std::string dyn_str_type_str
	= std::string (type.is_mutable () ? "*mut " : "*const ")
	  + dyn->get_name ();

      translated = Backend::named_type (dyn_str_type_str, type_record,
					dyn->get_locus ());

      return;
    }

  tree base_compiled_type
    = TyTyResolveCompile::compile (ctx, type.get_base (), trait_object_mode);
  if (type.is_mutable ())
    {
      translated = Backend::pointer_type (base_compiled_type);
    }
  else
    {
      auto base = Backend::immutable_type (base_compiled_type);
      translated = Backend::pointer_type (base);
    }
}

void
TyTyResolveCompile::visit (const TyTy::StrType &type)
{
  tree raw_str = create_str_type_record (type);
  translated = Backend::named_type ("str", raw_str, BUILTINS_LOCATION);
}

void
TyTyResolveCompile::visit (const TyTy::NeverType &)
{
  translated = get_unit_type (ctx);
}

void
TyTyResolveCompile::visit (const TyTy::DynamicObjectType &type)
{
  if (trait_object_mode)
    {
      translated = Backend::integer_type (true, Backend::get_pointer_size ());
      return;
    }

  tree type_record = create_dyn_obj_record (type);
  translated = Backend::named_type (type.get_name (), type_record,
				    type.get_ident ().locus);
}

tree
TyTyResolveCompile::create_dyn_obj_record (const TyTy::DynamicObjectType &type)
{
  // create implicit struct
  auto items = type.get_object_items ();
  std::vector<Backend::typed_identifier> fields;

  tree uint = Backend::integer_type (true, Backend::get_pointer_size ());
  tree uintptr_ty = build_pointer_type (uint);

  Backend::typed_identifier f ("pointer", uintptr_ty,
			       ctx->get_mappings ()->lookup_location (
				 type.get_ty_ref ()));
  fields.push_back (std::move (f));

  tree vtable_size = build_int_cst (size_type_node, items.size ());
  tree vtable_type = Backend::array_type (uintptr_ty, vtable_size);
  Backend::typed_identifier vtf ("vtable", vtable_type,
				 ctx->get_mappings ()->lookup_location (
				   type.get_ty_ref ()));
  fields.push_back (std::move (vtf));

  tree record = Backend::struct_type (fields);
  RS_DST_FLAG (record) = 1;
  TYPE_MAIN_VARIANT (record) = ctx->insert_main_variant (record);

  return record;
}

tree
TyTyResolveCompile::create_slice_type_record (const TyTy::SliceType &type)
{
  // lookup usize
  TyTy::BaseType *usize = nullptr;
  bool ok = ctx->get_tyctx ()->lookup_builtin ("usize", &usize);
  rust_assert (ok);

  tree element_type
    = TyTyResolveCompile::compile (ctx, type.get_element_type ());
  tree data_field_ty = build_pointer_type (element_type);
  Backend::typed_identifier data_field ("data", data_field_ty,
					type.get_locus ());

  tree len_field_ty = TyTyResolveCompile::compile (ctx, usize);
  Backend::typed_identifier len_field ("len", len_field_ty, type.get_locus ());

  tree record = Backend::struct_type ({data_field, len_field});
  RS_DST_FLAG (record) = 1;
  TYPE_MAIN_VARIANT (record) = ctx->insert_main_variant (record);

  return record;
}

tree
TyTyResolveCompile::create_str_type_record (const TyTy::StrType &type)
{
  // lookup usize
  TyTy::BaseType *usize = nullptr;
  bool ok = ctx->get_tyctx ()->lookup_builtin ("usize", &usize);
  rust_assert (ok);

  tree char_ptr = build_pointer_type (char_type_node);
  tree const_char_type = build_qualified_type (char_ptr, TYPE_QUAL_CONST);

  tree element_type = const_char_type;
  tree data_field_ty = build_pointer_type (element_type);
  Backend::typed_identifier data_field ("data", data_field_ty,
					type.get_locus ());

  tree len_field_ty = TyTyResolveCompile::compile (ctx, usize);
  Backend::typed_identifier len_field ("len", len_field_ty, type.get_locus ());

  tree record = Backend::struct_type ({data_field, len_field});
  RS_DST_FLAG (record) = 1;
  TYPE_MAIN_VARIANT (record) = ctx->insert_main_variant (record);

  return record;
}

} // namespace Compile
} // namespace Rust
