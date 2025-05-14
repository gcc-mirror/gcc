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

#include "rust-derive.h"
#include "rust-derive-clone.h"
#include "rust-derive-copy.h"
#include "rust-derive-debug.h"
#include "rust-derive-default.h"
#include "rust-derive-eq.h"
#include "rust-derive-partial-eq.h"
#include "rust-derive-hash.h"

namespace Rust {
namespace AST {

DeriveVisitor::DeriveVisitor (location_t loc)
  : loc (loc), builder (Builder (loc))
{}

std::vector<std::unique_ptr<Item>>
DeriveVisitor::derive (Item &item, const Attribute &attr,
		       BuiltinMacro to_derive)
{
  auto loc = attr.get_locus ();

  switch (to_derive)
    {
    case BuiltinMacro::Clone:
      return vec (DeriveClone (loc).go (item));
    case BuiltinMacro::Copy:
      return vec (DeriveCopy (loc).go (item));
    case BuiltinMacro::Debug:
      rust_warning_at (
	loc, 0,
	"derive(Debug) is not fully implemented yet and has no effect - only a "
	"stub implementation will be generated");
      return vec (DeriveDebug (loc).go (item));
    case BuiltinMacro::Default:
      return vec (DeriveDefault (loc).go (item));
    case BuiltinMacro::Eq:
      return DeriveEq (loc).go (item);
    case BuiltinMacro::PartialEq:
      return DerivePartialEq (loc).go (item);
    case BuiltinMacro::Hash:
      return vec (DeriveHash (loc).go (item));
    case BuiltinMacro::Ord:
    case BuiltinMacro::PartialOrd:
    default:
      rust_sorry_at (loc, "unimplemented builtin derive macro");
      return {};
    };
}

DeriveVisitor::ImplGenerics
DeriveVisitor::setup_impl_generics (
  const std::string &type_name,
  const std::vector<std::unique_ptr<GenericParam>> &type_generics,
  tl::optional<std::unique_ptr<TypeParamBound>> &&extra_bound) const
{
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

	    std::vector<std::unique_ptr<TypeParamBound>> extra_bounds;

	    if (extra_bound)
	      extra_bounds.emplace_back (std::move (*extra_bound));

	    auto impl_type_param
	      = builder.new_type_param (type_param, std::move (extra_bounds));

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

  auto generic_args_for_self
    = GenericArgs (lifetime_args, generic_args, {} /*binding args*/, loc);

  std::unique_ptr<Type> self_type_path
    = impl_generics.empty ()
	? builder.single_type_path (type_name)
	: builder.single_generic_type_path (type_name, generic_args_for_self);

  return ImplGenerics{std::move (self_type_path), std::move (impl_generics)};
}

} // namespace AST
} // namespace Rust
