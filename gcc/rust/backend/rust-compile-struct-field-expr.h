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

#ifndef RUST_COMPILE_STRUCT_FIELD_EXPR
#define RUST_COMPILE_STRUCT_FIELD_EXPR

#include "rust-compile-base.h"

namespace Rust {
namespace Compile {

class CompileStructExprField : private HIRCompileBase
{
public:
  static tree Compile (HIR::StructExprField *field, Context *ctx);

protected:
  void visit (HIR::StructExprFieldIdentifierValue &field);
  void visit (HIR::StructExprFieldIndexValue &field);
  void visit (HIR::StructExprFieldIdentifier &field);

private:
  CompileStructExprField (Context *ctx);

  tree translated;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_STRUCT_FIELD_EXPR
