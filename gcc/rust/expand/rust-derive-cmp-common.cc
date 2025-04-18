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
SelfOther::indexes (Builder builder, int limit)
{
  std::vector<SelfOther> vec;

  for (int i = 0; i < limit; i++)
    {
      vec.emplace_back (SelfOther::index (builder, i));
    }

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

  for (auto &field : fields)
    vec.emplace_back (
      SelfOther::field (builder, field.get_field_name ().as_string ()));

  return vec;
}

} // namespace AST
} // namespace Rust
