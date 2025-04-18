// Copyright (C) 2025 Free Software Foundation, Inc.

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

#include "rust-derive-ord.h"
#include "rust-ast-dump.h"
#include "rust-ast.h"
#include "rust-derive.h"
#include "rust-item.h"

namespace Rust {
namespace AST {

DeriveOrd::DeriveOrd (Ordering ordering, location_t loc)
  : DeriveVisitor (loc), ordering (ordering)
{}

std::unique_ptr<Item>
DeriveOrd::go (Item &item)
{
  item.accept_vis (*this);

  AST::Dump::debug (*expanded);

  return std::move (expanded);
}

std::unique_ptr<Item>
DeriveOrd::cmp_impl (
  std::unique_ptr<BlockExpr> &&fn_block, Identifier type_name,
  const std::vector<std::unique_ptr<GenericParam>> &type_generics)
{
  auto fn = cmp_fn (std::move (fn_block), type_name);

  auto trait = ordering == Ordering::Partial ? "PartialOrd" : "Ord";
  auto trait_path = builder.type_path ({"core", "cmp", trait}, true);

  auto trait_bound
    = builder.trait_bound (builder.type_path ({"core", "cmp", trait}, true));

  auto trait_items = vec (std::move (fn));

  auto cmp_generics
    = setup_impl_generics (type_name.as_string (), type_generics,
			   std::move (trait_bound));

  return builder.trait_impl (trait_path, std::move (cmp_generics.self_type),
			     std::move (trait_items),
			     std::move (cmp_generics.impl));
}

std::unique_ptr<AssociatedItem>
DeriveOrd::cmp_fn (std::unique_ptr<BlockExpr> &&block, Identifier type_name)
{
  // Ordering
  auto return_type = builder.type_path ({"core", "cmp", "Ordering"}, true);

  // In the case of PartialOrd, we return an Option<Ordering>
  if (ordering == Ordering::Partial)
    {
      auto generic = GenericArg::create_type (ptrify (return_type));

      auto generic_seg = builder.type_path_segment_generic (
	"Option", GenericArgs ({}, {generic}, {}, loc));
      auto core = builder.type_path_segment ("core");
      auto option = builder.type_path_segment ("option");

      return_type
	= builder.type_path (vec (std::move (core), std::move (option),
				  std::move (generic_seg)),
			     true);
    }

  // &self, other: &Self
  auto params = vec (
    builder.self_ref_param (),
    builder.function_param (builder.identifier_pattern ("other"),
			    builder.reference_type (ptrify (
			      builder.type_path (type_name.as_string ())))));

  auto function_name = fn (ordering);

  return builder.function (function_name, std::move (params),
			   ptrify (return_type), std::move (block));
}

std::pair<MatchArm, MatchArm>
DeriveOrd::make_cmp_arms ()
{
  // All comparison results other than Ordering::Equal
  auto non_equal = builder.identifier_pattern (DeriveOrd::not_equal);

  std::unique_ptr<Pattern> equal = ptrify (
    builder.path_in_expression ({"core", "cmp", "Ordering", "Equal"}, true));

  // We need to wrap the pattern in Option::Some if we are doing total ordering
  if (ordering == Ordering::Total)
    {
      auto pattern_items = std::unique_ptr<TupleStructItems> (
	new TupleStructItemsNoRange (vec (std::move (equal))));

      equal
	= std::make_unique<TupleStructPattern> (builder.path_in_expression (
						  LangItem::Kind::OPTION_SOME),
						std::move (pattern_items));
    }

  return {builder.match_arm (std::move (equal)),
	  builder.match_arm (std::move (non_equal))};
}

template <typename T>
inline bool
is_last (T &elt, std::vector<T> &vec)
{
  rust_assert (!vec.empty ());

  return &elt == &vec.back ();
}

std::unique_ptr<Expr>
DeriveOrd::recursive_match (std::vector<SelfOther> &&members)
{
  std::unique_ptr<Expr> final_expr = nullptr;

  for (auto it = members.rbegin (); it != members.rend (); it++)
    {
      auto &member = *it;

      auto cmp_fn_path = builder.path_in_expression (
	{"core", "cmp", trait (ordering), fn (ordering)}, true);

      auto cmp_call = builder.call (ptrify (cmp_fn_path),
				    vec (std::move (member.self_expr),
					 std::move (member.other_expr)));

      // For the last member (so the first iterator), we just create a call
      // expression
      if (it == members.rbegin ())
	{
	  final_expr = std::move (cmp_call);
	  continue;
	}

      // If we aren't dealing with the last member, then we need to wrap all of
      // that in a big match expression and keep going
      auto match_arms = make_cmp_arms ();

      auto match_cases
	= {builder.match_case (std::move (match_arms.first),
			       std::move (final_expr)),
	   builder.match_case (std::move (match_arms.second),
			       builder.identifier (DeriveOrd::not_equal))};

      final_expr
	= builder.match (std::move (cmp_call), std::move (match_cases));
    }

  return final_expr;
}

// we need to do a recursive match expression for all of the fields used in a
// struct so for something like struct Foo { a: i32, b: i32, c: i32 } we must
// first compare each `a` field, then `b`, then `c`, like this:
//
// match cmp_fn(self.<field>, other.<field>) {
//     Ordering::Equal => <recurse>,
//     cmp => cmp,
// }
//
// and the recurse will be the exact same expression, on the next field. so that
// our result looks like this:
//
// match cmp_fn(self.a, other.a) {
//     Ordering::Equal => match cmp_fn(self.b, other.b) {
//         Ordering::Equal =>cmp_fn(self.c, other.c),
//         cmp => cmp,
//     }
//     cmp => cmp,
// }
//
// the last field comparison needs not to be a match but just the function call.
// this is going to be annoying lol
void
DeriveOrd::visit_struct (StructStruct &item)
{
  auto fields = SelfOther::fields (builder, item.get_fields ());

  auto match_expr = recursive_match (std::move (fields));

  expanded = cmp_impl (builder.block (std::move (match_expr)),
		       item.get_identifier (), item.get_generic_params ());
}

// same as structs, but for each field index instead of each field name -
// straightforward once we have `visit_struct` working
void
DeriveOrd::visit_tuple (TupleStruct &item)
{}

// for enums, we need to generate a match for each of the enum's variant that
// contains data and then do the same thing as visit_struct or visit_enum. if
// the two aren't the same variant, then compare the two discriminant values for
// all the dataless enum variants and in the general case.
//
// so for enum Foo { A(i32, i32), B, C } we need to do the following
//
// match (self, other) {
//     (A(self_0, self_1), A(other_0, other_1)) => {
//         match cmp_fn(self_0, other_0) {
//             Ordering::Equal => cmp_fn(self_1, other_1),
//             cmp => cmp,
//         },
//     _ => cmp_fn(discr_value(self), discr_value(other))
// }
void
DeriveOrd::visit_enum (Enum &item)
{}

void
DeriveOrd::visit_union (Union &item)
{
  auto trait_name = trait (ordering);

  rust_error_at (item.get_locus (), "derive(%s) cannot be used on unions",
		 trait_name.c_str ());
}

} // namespace AST
} // namespace Rust
