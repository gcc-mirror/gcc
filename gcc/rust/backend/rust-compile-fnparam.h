// Copyright (C) 2020-2021 Free Software Foundation, Inc.

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

#ifndef RUST_COMPILE_FNPARAM
#define RUST_COMPILE_FNPARAM

#include "rust-compile-base.h"
#include "rust-hir-address-taken.h"

namespace Rust {
namespace Compile {

class CompileFnParam : public HIRCompileBase
{
  using Rust::Compile::HIRCompileBase::visit;

public:
  static Bvariable *compile (Context *ctx, tree fndecl,
			     HIR::FunctionParam *param, tree decl_type,
			     Location locus)
  {
    CompileFnParam compiler (ctx, fndecl, decl_type, locus, *param);
    param->get_param_name ()->accept_vis (compiler);
    return compiler.compiled_param;
  }

  void visit (HIR::IdentifierPattern &pattern) override
  {
    if (!pattern.is_mut ())
      decl_type = ctx->get_backend ()->immutable_type (decl_type);

    bool address_taken = false;
    address_taken_context->lookup_addess_taken (
      param.get_mappings ().get_hirid (), &address_taken);

    compiled_param = ctx->get_backend ()->parameter_variable (
      fndecl, pattern.get_identifier (), decl_type, address_taken, locus);
  }

private:
  CompileFnParam (Context *ctx, tree fndecl, tree decl_type, Location locus,
		  const HIR::FunctionParam &param)
    : HIRCompileBase (ctx), fndecl (fndecl), decl_type (decl_type),
      locus (locus), param (param),
      compiled_param (ctx->get_backend ()->error_variable ()),
      address_taken_context (Resolver::AddressTakenContext::get ())
  {}

  tree fndecl;
  tree decl_type;
  Location locus;
  const HIR::FunctionParam &param;
  Bvariable *compiled_param;
  const Resolver::AddressTakenContext *address_taken_context;
};

class CompileSelfParam : public HIRCompileBase
{
public:
  static Bvariable *compile (Context *ctx, tree fndecl, HIR::SelfParam &self,
			     tree decl_type, Location locus)
  {
    bool is_immutable
      = self.get_self_kind () == HIR::SelfParam::ImplicitSelfKind::IMM
	|| self.get_self_kind () == HIR::SelfParam::ImplicitSelfKind::IMM_REF;
    if (is_immutable)
      decl_type = ctx->get_backend ()->immutable_type (decl_type);

    const auto &address_taken_context = Resolver::AddressTakenContext::get ();
    bool address_taken = false;
    address_taken_context->lookup_addess_taken (
      self.get_mappings ().get_hirid (), &address_taken);

    return ctx->get_backend ()->parameter_variable (fndecl, "self", decl_type,
						    address_taken, locus);
  }
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_FNPARAM
