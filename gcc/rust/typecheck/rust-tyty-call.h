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

#ifndef RUST_TYTY_CALL
#define RUST_TYTY_CALL

#include "rust-diagnostics.h"
#include "rust-hir-full.h"
#include "rust-tyty-visitor.h"
#include "rust-tyty.h"
#include "rust-hir-type-check.h"

namespace Rust {
namespace TyTy {

class TypeCheckCallExpr : private TyVisitor
{
public:
  static BaseType *go (BaseType *ref, HIR::CallExpr &call,
		       Resolver::TypeCheckContext *context)
  {
    TypeCheckCallExpr checker (call, context);
    ref->accept_vis (checker);
    return checker.resolved;
  }

  void visit (InferType &type) override { gcc_unreachable (); }
  void visit (TupleType &type) override { gcc_unreachable (); }
  void visit (ArrayType &type) override { gcc_unreachable (); }
  void visit (BoolType &type) override { gcc_unreachable (); }
  void visit (IntType &type) override { gcc_unreachable (); }
  void visit (UintType &type) override { gcc_unreachable (); }
  void visit (FloatType &type) override { gcc_unreachable (); }
  void visit (USizeType &type) override { gcc_unreachable (); }
  void visit (ISizeType &type) override { gcc_unreachable (); }
  void visit (ErrorType &type) override { gcc_unreachable (); }
  void visit (CharType &type) override { gcc_unreachable (); }
  void visit (ReferenceType &type) override { gcc_unreachable (); }
  void visit (PointerType &type) override { gcc_unreachable (); }
  void visit (ParamType &) override { gcc_unreachable (); }
  void visit (StrType &) override { gcc_unreachable (); }
  void visit (NeverType &) override { gcc_unreachable (); }
  void visit (PlaceholderType &) override { gcc_unreachable (); }
  void visit (ProjectionType &) override { gcc_unreachable (); }

  // tuple-structs
  void visit (ADTType &type) override;

  // call fns
  void visit (FnType &type) override;
  void visit (FnPtr &type) override;

private:
  TypeCheckCallExpr (HIR::CallExpr &c, Resolver::TypeCheckContext *context)
    : resolved (nullptr), call (c), context (context),
      mappings (Analysis::Mappings::get ())
  {}

  BaseType *resolved;
  HIR::CallExpr &call;
  Resolver::TypeCheckContext *context;
  Analysis::Mappings *mappings;
};

class TypeCheckMethodCallExpr : private TyVisitor
{
public:
  // Resolve the Method parameters and return back the return type
  static BaseType *go (BaseType *ref, HIR::MethodCallExpr &call,
		       Resolver::TypeCheckContext *context)
  {
    TypeCheckMethodCallExpr checker (call, context);
    ref->accept_vis (checker);
    return checker.resolved;
  }

  void visit (InferType &type) override { gcc_unreachable (); }
  void visit (TupleType &type) override { gcc_unreachable (); }
  void visit (ArrayType &type) override { gcc_unreachable (); }
  void visit (BoolType &type) override { gcc_unreachable (); }
  void visit (IntType &type) override { gcc_unreachable (); }
  void visit (UintType &type) override { gcc_unreachable (); }
  void visit (FloatType &type) override { gcc_unreachable (); }
  void visit (USizeType &type) override { gcc_unreachable (); }
  void visit (ISizeType &type) override { gcc_unreachable (); }
  void visit (ErrorType &type) override { gcc_unreachable (); }
  void visit (ADTType &type) override { gcc_unreachable (); };
  void visit (CharType &type) override { gcc_unreachable (); }
  void visit (ReferenceType &type) override { gcc_unreachable (); }
  void visit (PointerType &type) override { gcc_unreachable (); }
  void visit (ParamType &) override { gcc_unreachable (); }
  void visit (StrType &) override { gcc_unreachable (); }
  void visit (NeverType &) override { gcc_unreachable (); }
  void visit (PlaceholderType &) override { gcc_unreachable (); }
  void visit (ProjectionType &) override { gcc_unreachable (); }

  // FIXME
  void visit (FnPtr &type) override { gcc_unreachable (); }

  // call fns
  void visit (FnType &type) override;

private:
  TypeCheckMethodCallExpr (HIR::MethodCallExpr &c,
			   Resolver::TypeCheckContext *context)
    : resolved (nullptr), call (c), context (context),
      mappings (Analysis::Mappings::get ())
  {}

  BaseType *resolved;
  HIR::MethodCallExpr &call;
  Resolver::TypeCheckContext *context;
  Analysis::Mappings *mappings;
};

} // namespace TyTy
} // namespace Rust

#endif // RUST_TYTY_CALL
