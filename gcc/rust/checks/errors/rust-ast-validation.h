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

#ifndef RUST_AST_VALIDATION_H
#define RUST_AST_VALIDATION_H

#include "rust-ast-visitor.h"
#include "rust-ast-full.h"
#include "rust-item.h"

namespace Rust {

class ASTValidation : public AST::ContextualASTVisitor
{
public:
  ASTValidation () {}

  using AST::ContextualASTVisitor::visit;

  void check (AST::Crate &crate) { AST::ContextualASTVisitor::visit (crate); }

  virtual void visit (AST::Module &module);
  virtual void visit (AST::ConstantItem &const_item);
  virtual void visit (AST::Lifetime &lifetime);
  virtual void visit (AST::LoopLabel &label);
  virtual void visit (AST::Union &item);
  virtual void visit (AST::Function &function);
  virtual void visit (AST::Trait &trait);
};

} // namespace Rust

#endif /* ! RUST_AST_VALIDATION_H */
