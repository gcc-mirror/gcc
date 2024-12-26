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

#include "rust-derive-copy.h"
#include "rust-ast-full.h"
#include "rust-hir-map.h"
#include "rust-mapping-common.h"
#include "rust-path.h"

namespace Rust {
namespace AST {

DeriveCopy::DeriveCopy (location_t loc)
  : DeriveVisitor (loc), expanded (nullptr)
{}

std::unique_ptr<AST::Item>
DeriveCopy::go (Item &item)
{
  item.accept_vis (*this);

  rust_assert (expanded);

  return std::move (expanded);
}

std::unique_ptr<Item>
DeriveCopy::copy_impl (
  std::string name,
  const std::vector<std::unique_ptr<GenericParam>> &type_generics)
{
  auto copy = builder.type_path (LangItem::Kind::COPY);

  // we need to build up the generics for this impl block which will be just a
  // clone of the types specified ones
  //
  // for example:
  //
  // #[derive(Copy)]
  // struct Be<T: Copy> { ... }
  //
  // we need to generate the impl block:
  //
  // impl<T: Copy> Clone for Be<T>

  std::vector<Lifetime> lifetime_args;
  std::vector<GenericArg> generic_args;
  std::vector<std::unique_ptr<GenericParam>> impl_generics;
  for (const auto &generic : type_generics)
    {
      switch (generic->get_kind ())
	{
	  case GenericParam::Kind::Lifetime: {
	    LifetimeParam &lifetime_param = (LifetimeParam &) *generic.get ();

	    Lifetime l = builder.new_lifetime (lifetime_param.get_lifetime ());
	    lifetime_args.push_back (std::move (l));

	    auto impl_lifetime_param
	      = builder.new_lifetime_param (lifetime_param);
	    impl_generics.push_back (std::move (impl_lifetime_param));
	  }
	  break;

	  case GenericParam::Kind::Type: {
	    TypeParam &type_param = (TypeParam &) *generic.get ();

	    std::unique_ptr<Type> associated_type = builder.single_type_path (
	      type_param.get_type_representation ().as_string ());

	    GenericArg type_arg
	      = GenericArg::create_type (std::move (associated_type));
	    generic_args.push_back (std::move (type_arg));

	    auto impl_type_param = builder.new_type_param (type_param);
	    impl_generics.push_back (std::move (impl_type_param));
	  }
	  break;

	  case GenericParam::Kind::Const: {
	    rust_unreachable ();

	    // TODO
	    // const ConstGenericParam *const_param
	    //   = (const ConstGenericParam *) generic.get ();
	    // std::unique_ptr<Expr> const_expr = nullptr;

	    // GenericArg type_arg
	    //   = GenericArg::create_const (std::move (const_expr));
	    // generic_args.push_back (std::move (type_arg));
	  }
	  break;
	}
    }

  GenericArgs generic_args_for_self (lifetime_args, generic_args,
				     {} /*binding args*/, loc);
  std::unique_ptr<Type> self_type_path
    = impl_generics.empty ()
	? builder.single_type_path (name)
	: builder.single_generic_type_path (name, generic_args_for_self);

  return std::unique_ptr<Item> (
    new TraitImpl (std::move (copy), /* unsafe */ false,
		   /* exclam */ false, /* trait items */ {},
		   std::move (impl_generics), std::move (self_type_path),
		   WhereClause::create_empty (), Visibility::create_private (),
		   {}, {}, loc));
}

void
DeriveCopy::visit_struct (StructStruct &item)
{
  expanded = copy_impl (item.get_struct_name ().as_string (),
			item.get_generic_params ());
}

void
DeriveCopy::visit_tuple (TupleStruct &item)
{
  expanded = copy_impl (item.get_struct_name ().as_string (),
			item.get_generic_params ());
}

void
DeriveCopy::visit_enum (Enum &item)
{
  expanded = copy_impl (item.get_identifier ().as_string (),
			item.get_generic_params ());
}

void
DeriveCopy::visit_union (Union &item)
{
  expanded = copy_impl (item.get_identifier ().as_string (),
			item.get_generic_params ());
}

} // namespace AST
} // namespace Rust
