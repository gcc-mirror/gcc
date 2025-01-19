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

#include "rust-compile-fnparam.h"
#include "rust-compile-pattern.h"

#include "gimple-expr.h"

namespace Rust {
namespace Compile {

CompileFnParam::CompileFnParam (Context *ctx, tree fndecl, tree decl_type,
				location_t locus)
  : HIRCompileBase (ctx), fndecl (fndecl), decl_type (decl_type), locus (locus),
    compiled_param (Bvariable::error_variable ())
{}

Bvariable *
CompileFnParam::compile (Context *ctx, tree fndecl, HIR::FunctionParam *param,
			 tree decl_type, location_t locus)
{
  CompileFnParam compiler (ctx, fndecl, decl_type, locus);
  param->get_param_name ()->accept_vis (compiler);
  return compiler.compiled_param;
}

Bvariable *
CompileFnParam::compile (Context *ctx, tree fndecl, HIR::Pattern *param,
			 tree decl_type, location_t locus)
{
  CompileFnParam compiler (ctx, fndecl, decl_type, locus);
  param->accept_vis (compiler);
  return compiler.compiled_param;
}

void
CompileFnParam::visit (HIR::IdentifierPattern &pattern)
{
  if (!pattern.is_mut ())
    decl_type = Backend::immutable_type (decl_type);

  compiled_param
    = Backend::parameter_variable (fndecl,
				   pattern.get_identifier ().as_string (),
				   decl_type, locus);
}

void
CompileFnParam::visit (HIR::WildcardPattern &pattern)
{
  decl_type = Backend::immutable_type (decl_type);

  compiled_param = Backend::parameter_variable (fndecl, "_", decl_type, locus);
}

void
CompileFnParam::visit (HIR::StructPattern &pattern)
{
  tree tmp_param_var = create_tmp_param_var (decl_type);
  CompilePatternBindings::Compile (&pattern, tmp_param_var, ctx);
}

void
CompileFnParam::visit (HIR::TupleStructPattern &pattern)
{
  tree tmp_param_var = create_tmp_param_var (decl_type);
  CompilePatternBindings::Compile (&pattern, tmp_param_var, ctx);
}

void
CompileFnParam::visit (HIR::ReferencePattern &pattern)
{
  tree tmp_param_var = create_tmp_param_var (decl_type);
  CompilePatternBindings::Compile (&pattern, tmp_param_var, ctx);
}

Bvariable *
CompileSelfParam::compile (Context *ctx, tree fndecl, HIR::SelfParam &self,
			   tree decl_type, location_t locus)
{
  bool is_immutable
    = self.get_self_kind () == HIR::SelfParam::ImplicitSelfKind::IMM
      || self.get_self_kind () == HIR::SelfParam::ImplicitSelfKind::IMM_REF;
  if (is_immutable)
    decl_type = Backend::immutable_type (decl_type);

  return Backend::parameter_variable (fndecl, "self", decl_type, locus);
}

tree
CompileFnParam::create_tmp_param_var (tree decl_type)
{
  // generate the anon param
  tree tmp_ident = create_tmp_var_name ("RSTPRM");
  std::string cpp_str_identifier = std::string (IDENTIFIER_POINTER (tmp_ident));

  decl_type = Backend::immutable_type (decl_type);
  compiled_param = Backend::parameter_variable (fndecl, cpp_str_identifier,
						decl_type, locus);

  return Backend::var_expression (compiled_param, locus);
}

} // namespace Compile
} // namespace Rust
