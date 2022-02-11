// Copyright (C) 2020-2022 Free Software Foundation, Inc.

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

#ifndef RUST_COMPILE_ITEM
#define RUST_COMPILE_ITEM

#include "rust-compile-base.h"

namespace Rust {
namespace Compile {

class CompileItem : public HIRCompileBase
{
protected:
  using Rust::Compile::HIRCompileBase::visit;

public:
  static tree compile (HIR::Item *item, Context *ctx,
		       TyTy::BaseType *concrete = nullptr,
		       bool is_query_mode = false,
		       Location ref_locus = Location ())
  {
    CompileItem compiler (ctx, concrete, ref_locus);
    item->accept_vis (compiler);

    if (is_query_mode && compiler.reference == error_mark_node)
      rust_internal_error_at (ref_locus, "failed to compile item: %s",
			      item->as_string ().c_str ());

    return compiler.reference;
  }

  void visit (HIR::StaticItem &var) override;

  void visit (HIR::ConstantItem &constant) override;

  void visit (HIR::Function &function) override;

  void visit (HIR::ImplBlock &impl_block) override;

  void visit (HIR::ExternBlock &extern_block) override;

  void visit (HIR::Module &module) override;

protected:
  CompileItem (Context *ctx, TyTy::BaseType *concrete, Location ref_locus)
    : HIRCompileBase (ctx), concrete (concrete), reference (error_mark_node),
      ref_locus (ref_locus)
  {}

  TyTy::BaseType *concrete;
  tree reference;
  Location ref_locus;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_ITEM
