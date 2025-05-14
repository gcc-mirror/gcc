// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

#include "rust-hir-generic-param.h"

namespace Rust {
namespace HIR {

GenericParam::GenericParam (Analysis::NodeMapping mapping,
			    enum GenericKind kind)
  : mappings (mapping), kind (kind)
{}

LifetimeParam::LifetimeParam (Analysis::NodeMapping mappings, Lifetime lifetime,
			      location_t locus,
			      std::vector<Lifetime> lifetime_bounds,
			      AST::AttrVec outer_attrs)
  : GenericParam (mappings, GenericKind::LIFETIME),
    lifetime (std::move (lifetime)),
    lifetime_bounds (std::move (lifetime_bounds)),
    outer_attrs (std::move (outer_attrs)), locus (locus)
{}

LifetimeParam::LifetimeParam (LifetimeParam const &other)
  : GenericParam (other.mappings, GenericKind::LIFETIME),
    lifetime (other.lifetime), lifetime_bounds (other.lifetime_bounds),
    outer_attrs (other.outer_attrs), locus (other.locus)
{}

LifetimeParam &
LifetimeParam::operator= (LifetimeParam const &other)
{
  lifetime = other.lifetime;
  lifetime_bounds = other.lifetime_bounds;
  outer_attrs = other.outer_attrs;
  locus = other.locus;
  mappings = other.mappings;

  return *this;
}

ConstGenericParam::ConstGenericParam (std::string name,
				      std::unique_ptr<Type> type,
				      std::unique_ptr<Expr> default_expression,
				      Analysis::NodeMapping mapping,
				      location_t locus)
  : GenericParam (mapping, GenericKind::CONST), name (std::move (name)),
    type (std::move (type)),
    default_expression (std::move (default_expression)), locus (locus)
{}

ConstGenericParam::ConstGenericParam (const ConstGenericParam &other)
  : GenericParam (other)
{
  name = other.name;
  locus = other.locus;

  if (other.type)
    type = other.type->clone_type ();
  if (other.default_expression)
    default_expression = other.default_expression->clone_expr ();
}

std::string
ConstGenericParam::as_string () const
{
  auto result = "ConstGenericParam: " + name + " : " + type->as_string ();

  if (default_expression)
    result += " = " + default_expression->as_string ();

  return result;
}

void
ConstGenericParam::accept_vis (HIRFullVisitor &)
{}

} // namespace HIR
} // namespace Rust
