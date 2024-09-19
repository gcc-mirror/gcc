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

#ifndef RUST_AST_BUILDER_TYPE
#define RUST_AST_BUILDER_TYPE

#include "rust-ast-visitor.h"

namespace Rust {
namespace AST {

class ASTTypeBuilder : public DefaultASTVisitor
{
protected:
  using DefaultASTVisitor::visit;

public:
  static Type *build (Type &type);

  void visit (BareFunctionType &fntype) override;
  void visit (TupleType &tuple) override;
  void visit (TypePath &path) override;
  void visit (QualifiedPathInType &path) override;
  void visit (ArrayType &type) override;
  void visit (ReferenceType &type) override;
  void visit (RawPointerType &type) override;
  void visit (SliceType &type) override;
  void visit (InferredType &type) override;
  void visit (NeverType &type) override;
  void visit (TraitObjectTypeOneBound &type) override;
  void visit (TraitObjectType &type) override;

private:
  ASTTypeBuilder ();

  Type *translated;
};

} // namespace AST
} // namespace Rust

#endif // RUST_AST_BUILDER_TYPE
