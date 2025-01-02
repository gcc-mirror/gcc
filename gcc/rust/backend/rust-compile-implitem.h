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

#ifndef RUST_COMPILE_IMPLITEM_H
#define RUST_COMPILE_IMPLITEM_H

#include "rust-compile-item.h"

namespace Rust {
namespace Compile {

// this is a proxy for HIR::ImplItem's back to use the normal HIR::Item path
class CompileInherentImplItem : public CompileItem
{
public:
  static tree Compile (HIR::ImplItem *item, Context *ctx,
		       TyTy::BaseType *concrete = nullptr,
		       bool is_query_mode = false,
		       location_t ref_locus = UNDEF_LOCATION)
  {
    CompileInherentImplItem compiler (ctx, concrete, ref_locus);
    item->accept_vis (compiler);

    if (is_query_mode && compiler.reference == error_mark_node)
      rust_internal_error_at (ref_locus, "failed to compile impl item: %s",
			      item->as_string ().c_str ());

    return compiler.reference;
  }

private:
  CompileInherentImplItem (Context *ctx, TyTy::BaseType *concrete,
			   location_t ref_locus)
    : CompileItem (ctx, concrete, ref_locus)
  {}
};

class CompileTraitItem : public HIRCompileBase, public HIR::HIRTraitItemVisitor
{
public:
  static tree Compile (HIR::TraitItem *item, Context *ctx,
		       TyTy::BaseType *concrete, bool is_query_mode = false,
		       location_t ref_locus = UNDEF_LOCATION)
  {
    CompileTraitItem compiler (ctx, concrete, ref_locus);
    item->accept_vis (compiler);

    if (is_query_mode && compiler.reference == error_mark_node)
      rust_internal_error_at (ref_locus, "failed to compile trait item: %s",
			      item->as_string ().c_str ());

    return compiler.reference;
  }

  void visit (HIR::TraitItemConst &constant) override;
  void visit (HIR::TraitItemFunc &func) override;

  void visit (HIR::TraitItemType &typ) override {}

private:
  CompileTraitItem (Context *ctx, TyTy::BaseType *concrete,
		    location_t ref_locus)
    : HIRCompileBase (ctx), concrete (concrete), reference (error_mark_node),
      ref_locus (ref_locus)
  {}

  TyTy::BaseType *concrete;
  tree reference;
  location_t ref_locus;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_IMPLITEM_H
