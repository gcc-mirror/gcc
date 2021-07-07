// Copyright (C) 2020 Free Software Foundation, Inc.

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

#ifndef RUST_COMPILE_TYTY
#define RUST_COMPILE_TYTY

#include "rust-system.h"
#include "rust-location.h"
#include "rust-diagnostics.h"
#include "rust-backend.h"
#include "rust-tyty.h"
#include "rust-tyty-visitor.h"
#include "rust-hir-map.h"
#include "rust-hir-full.h"

namespace Rust {
namespace Compile {

class TyTyCompile : public TyTy::TyVisitor
{
public:
  static ::Btype *compile (::Backend *backend, TyTy::BaseType *ty)
  {
    TyTyCompile compiler (backend);
    ty->accept_vis (compiler);
    rust_assert (compiler.translated != nullptr);
    return compiler.translated;
  }

  void visit (TyTy::ErrorType &) override { gcc_unreachable (); }

  void visit (TyTy::InferType &) override { gcc_unreachable (); }

  void visit (TyTy::ADTType &) override { gcc_unreachable (); }

  void visit (TyTy::PlaceholderType &) override { gcc_unreachable (); }

  void visit (TyTy::TupleType &type) override
  {
    if (type.num_fields () == 0)
      translated = backend->unit_type ();
    else
      gcc_unreachable ();
  }

  void visit (TyTy::ArrayType &) override { gcc_unreachable (); }

  void visit (TyTy::ReferenceType &) override { gcc_unreachable (); }

  void visit (TyTy::ParamType &) override { gcc_unreachable (); }

  void visit (TyTy::FnPtr &) override { gcc_unreachable (); }

  void visit (TyTy::FnType &type) override
  {
    Backend::Btyped_identifier receiver;
    std::vector<Backend::Btyped_identifier> parameters;
    std::vector<Backend::Btyped_identifier> results;

    if (!type.get_return_type ()->is_unit ())
      {
	auto hir_type = type.get_return_type ();
	auto ret = TyTyCompile::compile (backend, hir_type);
	results.push_back (Backend::Btyped_identifier (
	  "_", ret, mappings->lookup_location (hir_type->get_ref ())));
      }

    for (auto &params : type.get_params ())
      {
	auto param_pattern = params.first;
	auto param_tyty = params.second;
	auto compiled_param_type = TyTyCompile::compile (backend, param_tyty);

	auto compiled_param = Backend::Btyped_identifier (
	  param_pattern->as_string (), compiled_param_type,
	  mappings->lookup_location (param_tyty->get_ref ()));

	parameters.push_back (compiled_param);
      }

    translated
      = backend->function_type (receiver, parameters, results, NULL,
				mappings->lookup_location (type.get_ref ()));
  }

  void visit (TyTy::BoolType &) override
  {
    translated = backend->named_type ("bool", backend->bool_type (),
				      Linemap::predeclared_location ());
  }

  void visit (TyTy::IntType &type) override
  {
    switch (type.get_int_kind ())
      {
      case TyTy::IntType::I8:
	translated
	  = backend->named_type ("i8", backend->integer_type (false, 8),
				 Linemap::predeclared_location ());
	return;

      case TyTy::IntType::I16:
	translated
	  = backend->named_type ("i16", backend->integer_type (false, 16),
				 Linemap::predeclared_location ());
	return;

      case TyTy::IntType::I32:
	translated
	  = backend->named_type ("i32", backend->integer_type (false, 32),
				 Linemap::predeclared_location ());
	return;

      case TyTy::IntType::I64:
	translated
	  = backend->named_type ("i64", backend->integer_type (false, 64),
				 Linemap::predeclared_location ());
	return;

      case TyTy::IntType::I128:
	translated
	  = backend->named_type ("i128", backend->integer_type (false, 128),
				 Linemap::predeclared_location ());
	return;
      }
    gcc_unreachable ();
  }

  void visit (TyTy::UintType &type) override
  {
    switch (type.get_uint_kind ())
      {
      case TyTy::UintType::U8:
	translated = backend->named_type ("u8", backend->integer_type (true, 8),
					  Linemap::predeclared_location ());
	return;

      case TyTy::UintType::U16:
	translated
	  = backend->named_type ("u16", backend->integer_type (true, 16),
				 Linemap::predeclared_location ());
	return;

      case TyTy::UintType::U32:
	translated
	  = backend->named_type ("u32", backend->integer_type (true, 32),
				 Linemap::predeclared_location ());
	return;

      case TyTy::UintType::U64:
	translated
	  = backend->named_type ("u64", backend->integer_type (true, 64),
				 Linemap::predeclared_location ());
	return;

      case TyTy::UintType::U128:
	translated
	  = backend->named_type ("u128", backend->integer_type (true, 128),
				 Linemap::predeclared_location ());
	return;
      }
    gcc_unreachable ();
  }

  void visit (TyTy::FloatType &type) override
  {
    switch (type.get_float_kind ())
      {
      case TyTy::FloatType::F32:
	translated = backend->named_type ("f32", backend->float_type (32),
					  Linemap::predeclared_location ());
	return;

      case TyTy::FloatType::F64:
	translated = backend->named_type ("f64", backend->float_type (64),
					  Linemap::predeclared_location ());
	return;
      }

    gcc_unreachable ();
  }

  void visit (TyTy::USizeType &) override
  {
    translated = backend->named_type (
      "usize", backend->integer_type (true, backend->get_pointer_size ()),
      Linemap::predeclared_location ());
  }

  void visit (TyTy::ISizeType &) override
  {
    translated = backend->named_type (
      "isize", backend->integer_type (false, backend->get_pointer_size ()),
      Linemap::predeclared_location ());
  }

  void visit (TyTy::CharType &) override
  {
    translated = backend->named_type ("char", backend->wchar_type (),
				      Linemap::predeclared_location ());
  }

  void visit (TyTy::StrType &) override
  {
    Btype *raw_str = backend->raw_str_type ();
    translated
      = backend->named_type ("str", raw_str, Linemap::predeclared_location ());
  }

  void visit (TyTy::NeverType &) override
  {
    translated = backend->unit_type ();
  }

private:
  TyTyCompile (::Backend *backend)
    : backend (backend), translated (nullptr),
      mappings (Analysis::Mappings::get ())
  {}

  ::Backend *backend;
  ::Btype *translated;
  Analysis::Mappings *mappings;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_TYTY
