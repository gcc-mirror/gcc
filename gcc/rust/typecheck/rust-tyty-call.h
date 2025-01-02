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
		       TyTy::VariantDef &variant,
		       Resolver::TypeCheckContext *context)
  {
    TypeCheckCallExpr checker (call, variant, context);
    ref->accept_vis (checker);
    return checker.resolved;
  }

  void visit (InferType &) override { rust_unreachable (); }
  void visit (TupleType &) override { rust_unreachable (); }
  void visit (ArrayType &) override { rust_unreachable (); }
  void visit (SliceType &) override { rust_unreachable (); }
  void visit (BoolType &) override { rust_unreachable (); }
  void visit (IntType &) override { rust_unreachable (); }
  void visit (UintType &) override { rust_unreachable (); }
  void visit (FloatType &) override { rust_unreachable (); }
  void visit (USizeType &) override { rust_unreachable (); }
  void visit (ISizeType &) override { rust_unreachable (); }
  void visit (ErrorType &) override { rust_unreachable (); }
  void visit (CharType &) override { rust_unreachable (); }
  void visit (ReferenceType &) override { rust_unreachable (); }
  void visit (PointerType &) override { rust_unreachable (); }
  void visit (ParamType &) override { rust_unreachable (); }
  void visit (StrType &) override { rust_unreachable (); }
  void visit (NeverType &) override { rust_unreachable (); }
  void visit (PlaceholderType &) override { rust_unreachable (); }
  void visit (ProjectionType &) override { rust_unreachable (); }
  void visit (DynamicObjectType &) override { rust_unreachable (); }
  void visit (ClosureType &type) override { rust_unreachable (); }

  // tuple-structs
  void visit (ADTType &type) override;

  // call fns
  void visit (FnType &type) override;
  void visit (FnPtr &type) override;

private:
  TypeCheckCallExpr (HIR::CallExpr &c, TyTy::VariantDef &variant,
		     Resolver::TypeCheckContext *context)
    : resolved (new TyTy::ErrorType (c.get_mappings ().get_hirid ())), call (c),
      variant (variant), context (context),
      mappings (Analysis::Mappings::get ())
  {}

  BaseType *resolved;
  HIR::CallExpr &call;
  TyTy::VariantDef &variant;
  Resolver::TypeCheckContext *context;
  Analysis::Mappings *mappings;
};

class Argument
{
public:
  Argument (Analysis::NodeMapping mapping, BaseType *argument_type,
	    location_t locus)
    : mapping (mapping), argument_type (argument_type), locus (locus)
  {}

  location_t get_locus () const { return locus; }

  BaseType *get_argument_type () { return argument_type; }

  Analysis::NodeMapping get_mappings () const { return mapping; }

private:
  Analysis::NodeMapping mapping;
  BaseType *argument_type;
  location_t locus;
};

class TypeCheckMethodCallExpr
{
public:
  static BaseType *go (FnType *ref, HIR::MethodCallExpr &call,
		       TyTy::BaseType *adjusted_self,
		       Resolver::TypeCheckContext *context);

  static BaseType *go (FnType *ref, Analysis::NodeMapping call_mappings,
		       std::vector<Argument> &args, location_t call_locus,
		       location_t receiver_locus, TyTy::BaseType *adjusted_self,
		       Resolver::TypeCheckContext *context);

protected:
  BaseType *check (FnType &type);

  TypeCheckMethodCallExpr (Analysis::NodeMapping call_mappings,
			   std::vector<Argument> &args, location_t call_locus,
			   location_t receiver_locus,
			   TyTy::BaseType *adjusted_self,
			   Resolver::TypeCheckContext *context);

  Analysis::NodeMapping call_mappings;
  std::vector<Argument> &arguments;
  location_t call_locus;
  location_t receiver_locus;
  TyTy::BaseType *adjusted_self;
  Resolver::TypeCheckContext *context;
  Analysis::Mappings *mappings;
};

} // namespace TyTy
} // namespace Rust

#endif // RUST_TYTY_CALL
