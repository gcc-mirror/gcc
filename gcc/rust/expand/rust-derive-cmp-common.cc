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

#include "rust-derive-cmp-common.h"
#include "rust-ast-builder.h"
#include "rust-item.h"

namespace Rust {
namespace AST {

SelfOther
SelfOther::index (Builder builder, int idx)
{
  return SelfOther{
    builder.tuple_idx ("self", idx),
    builder.tuple_idx ("other", idx),
  };
}

std::vector<SelfOther>
SelfOther::indexes (Builder builder, const std::vector<TupleField> &fields)
{
  std::vector<SelfOther> vec;

  for (size_t i = 0; i < fields.size (); i++)
    vec.emplace_back (SelfOther::index (builder, i));

  return vec;
}

SelfOther
SelfOther::field (Builder builder, const std::string &field_name)
{
  return SelfOther{
    builder.field_access (builder.identifier ("self"), field_name),
    builder.field_access (builder.identifier ("other"), field_name),
  };
}

std::vector<SelfOther>
SelfOther::fields (Builder builder, const std::vector<StructField> &fields)
{
  std::vector<SelfOther> vec;

  for (const auto &field : fields)
    vec.emplace_back (
      SelfOther::field (builder, field.get_field_name ().as_string ()));

  return vec;
}

MatchCase
EnumMatchBuilder::tuple (EnumItem &variant_raw)
{
  auto &variant = static_cast<EnumItemTuple &> (variant_raw);

  auto self_patterns = std::vector<std::unique_ptr<Pattern>> ();
  auto other_patterns = std::vector<std::unique_ptr<Pattern>> ();

  auto self_other_exprs = std::vector<SelfOther> ();

  for (size_t i = 0; i < variant.get_tuple_fields ().size (); i++)
    {
      // The patterns we're creating for each field are `self_<i>` and
      // `other_<i>` where `i` is the index of the field. It doesn't actually
      // matter what we use, as long as it's ordered, unique, and that we can
      // reuse it in the match case's return expression to check that they are
      // equal.

      auto self_pattern_str = "__self_" + std::to_string (i);
      auto other_pattern_str = "__other_" + std::to_string (i);

      self_patterns.emplace_back (
	builder.identifier_pattern (self_pattern_str));
      other_patterns.emplace_back (
	builder.identifier_pattern (other_pattern_str));

      self_other_exprs.emplace_back (SelfOther{
	builder.identifier (self_pattern_str),
	builder.identifier (other_pattern_str),
      });
    }

  auto self_pattern_items = std::unique_ptr<TupleStructItems> (
    new TupleStructItemsNoRange (std::move (self_patterns)));
  auto other_pattern_items = std::unique_ptr<TupleStructItems> (
    new TupleStructItemsNoRange (std::move (other_patterns)));

  auto self_pattern = std::unique_ptr<Pattern> (
    new ReferencePattern (std::unique_ptr<Pattern> (new TupleStructPattern (
			    variant_path, std::move (self_pattern_items))),
			  false, false, builder.loc));
  auto other_pattern = std::unique_ptr<Pattern> (
    new ReferencePattern (std::unique_ptr<Pattern> (new TupleStructPattern (
			    variant_path, std::move (other_pattern_items))),
			  false, false, builder.loc));

  auto tuple_items = std::make_unique<TuplePatternItemsMultiple> (
    vec (std::move (self_pattern), std::move (other_pattern)));

  auto pattern
    = std::make_unique<TuplePattern> (std::move (tuple_items), builder.loc);

  auto expr = fn (std::move (self_other_exprs));

  return builder.match_case (std::move (pattern), std::move (expr));
}

MatchCase
EnumMatchBuilder::strukt (EnumItem &variant_raw)
{
  auto &variant = static_cast<EnumItemStruct &> (variant_raw);

  auto self_fields = std::vector<std::unique_ptr<StructPatternField>> ();
  auto other_fields = std::vector<std::unique_ptr<StructPatternField>> ();

  auto self_other_exprs = std::vector<SelfOther> ();

  for (auto &field : variant.get_struct_fields ())
    {
      // The patterns we're creating for each field are `self_<field>` and
      // `other_<field>` where `field` is the name of the field. It doesn't
      // actually matter what we use, as long as it's ordered, unique, and that
      // we can reuse it in the match case's return expression to check that
      // they are equal.

      auto field_name = field.get_field_name ().as_string ();

      auto self_pattern_str = "__self_" + field_name;
      auto other_pattern_str = "__other_" + field_name;

      self_fields.emplace_back (builder.struct_pattern_ident_pattern (
	field_name, builder.identifier_pattern (self_pattern_str)));
      other_fields.emplace_back (builder.struct_pattern_ident_pattern (
	field_name, builder.identifier_pattern (other_pattern_str)));

      self_other_exprs.emplace_back (SelfOther{
	builder.identifier (self_pattern_str),
	builder.identifier (other_pattern_str),
      });
    }

  auto self_elts = StructPatternElements (std::move (self_fields));
  auto other_elts = StructPatternElements (std::move (other_fields));

  auto self_pattern = std::unique_ptr<Pattern> (
    new ReferencePattern (std::unique_ptr<Pattern> (new StructPattern (
			    variant_path, builder.loc, std::move (self_elts))),
			  false, false, builder.loc));
  auto other_pattern = std::unique_ptr<Pattern> (
    new ReferencePattern (std::unique_ptr<Pattern> (new StructPattern (
			    variant_path, builder.loc, std::move (other_elts))),
			  false, false, builder.loc));

  auto tuple_items = std::make_unique<TuplePatternItemsMultiple> (
    vec (std::move (self_pattern), std::move (other_pattern)));

  auto pattern
    = std::make_unique<TuplePattern> (std::move (tuple_items), builder.loc);

  auto expr = fn (std::move (self_other_exprs));

  return builder.match_case (std::move (pattern), std::move (expr));
}

} // namespace AST
} // namespace Rust
