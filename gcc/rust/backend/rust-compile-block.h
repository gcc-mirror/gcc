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

#ifndef RUST_COMPILE_BLOCK
#define RUST_COMPILE_BLOCK

#include "rust-compile-base.h"
#include "rust-compile-tyty.h"

namespace Rust {
namespace Compile {

class CompileBlock : public HIRCompileBase
{
public:
  static Bblock *compile (HIR::BlockExpr *expr, Context *ctx)
  {
    CompileBlock compiler (ctx);
    expr->accept_vis (compiler);
    return compiler.translated;
  }

  ~CompileBlock () {}

  void visit (HIR::BlockExpr &expr);

private:
  CompileBlock (Context *ctx) : HIRCompileBase (ctx), translated (nullptr) {}

  Bblock *translated;
};

class CompileConditionalBlocks : public HIRCompileBase
{
public:
  static Bstatement *compile (HIR::IfExpr *expr, Context *ctx)
  {
    CompileConditionalBlocks resolver (ctx);
    expr->accept_vis (resolver);
    return resolver.translated;
  }

  ~CompileConditionalBlocks () {}

  void visit (HIR::IfExpr &expr);

  void visit (HIR::IfExprConseqElse &expr);

  void visit (HIR::IfExprConseqIf &expr);

private:
  CompileConditionalBlocks (Context *ctx)
    : HIRCompileBase (ctx), translated (nullptr)
  {}

  Bstatement *translated;
};

class CompileExprWithBlock : public HIRCompileBase
{
public:
  static Bstatement *compile (HIR::ExprWithBlock *expr, Context *ctx)
  {
    CompileExprWithBlock resolver (ctx);
    expr->accept_vis (resolver);
    return resolver.translated;
  }

  ~CompileExprWithBlock () {}

  void visit (HIR::IfExpr &expr)
  {
    translated = CompileConditionalBlocks::compile (&expr, ctx);
  }

  void visit (HIR::IfExprConseqElse &expr)
  {
    translated = CompileConditionalBlocks::compile (&expr, ctx);
  }

  void visit (HIR::IfExprConseqIf &expr)
  {
    translated = CompileConditionalBlocks::compile (&expr, ctx);
  }

private:
  CompileExprWithBlock (Context *ctx)
    : HIRCompileBase (ctx), translated (nullptr)
  {}

  Bstatement *translated;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_BLOCK
