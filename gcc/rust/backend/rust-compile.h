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

#ifndef RUST_COMPILE_H
#define RUST_COMPILE_H

#include "rust-system.h"
#include "rust-hir-map.h"
#include "rust-name-resolver.h"
#include "rust-hir-type-check.h"
#include "rust-linemap.h"
#include "rust-backend.h"

namespace Rust {
namespace Compile {

class Context
{
public:
  Context (::Backend *backend)
    : backend (backend), resolver (Resolver::Resolver::get ()),
      tyctx (Resolver::TypeCheckContext::get ()),
      mappings (Analysis::Mappings::get ())
  {}

  ~Context () {}

  ::Backend *get_backend () { return backend; }
  Resolver::Resolver *get_resolver () { return resolver; }
  Resolver::TypeCheckContext *get_tyctx () { return tyctx; }
  Analysis::Mappings *get_mappings () { return mappings; }

  void push_type (::Btype *t) { type_decls.push_back (t); }
  void push_var (::Bvariable *v) { var_decls.push_back (v); }
  void push_const (::Bexpression *c) { const_decls.push_back (c); }
  void push_function (::Bfunction *f) { func_decls.push_back (f); }

  void write_to_backend ()
  {
    backend->write_global_definitions (type_decls, const_decls, func_decls,
				       var_decls);
  }

private:
  ::Backend *backend;
  Resolver::Resolver *resolver;
  Resolver::TypeCheckContext *tyctx;
  Analysis::Mappings *mappings;

  // To GCC middle-end
  std::vector< ::Btype *> type_decls;
  std::vector< ::Bvariable *> var_decls;
  std::vector< ::Bexpression *> const_decls;
  std::vector< ::Bfunction *> func_decls;
};

class CompileCrate
{
public:
  static void Compile (HIR::Crate &crate, Context *ctx);

  ~CompileCrate ();

private:
  CompileCrate (HIR::Crate &crate, Context *ctx);

  HIR::Crate &crate;
  Context *ctx;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_H
