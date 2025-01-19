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

#include "rust-ast-validation.h"
#include "rust-common.h"
#include "rust-diagnostics.h"
#include "rust-item.h"
#include "rust-keyword-values.h"

namespace Rust {

void
ASTValidation::visit (AST::Lifetime &lifetime)
{
  auto name = lifetime.get_lifetime_name ();
  auto valid = std::set<std::string>{"static", "_"};
  auto &keywords = Values::Keywords::keywords;

  if (valid.find (name) == valid.end ()
      && keywords.find (name) != keywords.end ())
    rust_error_at (lifetime.get_locus (), "lifetimes cannot use keyword names");

  AST::ContextualASTVisitor::visit (lifetime);
}

void
ASTValidation::visit (AST::LoopLabel &label)
{
  auto name = label.get_lifetime ().get_lifetime_name ();
  auto lifetime_name = '\'' + name;
  auto &keywords = Values::Keywords::keywords;
  if (keywords.find (name) != keywords.end ())
    rust_error_at (label.get_lifetime ().get_locus (), "invalid label name %qs",
		   lifetime_name.c_str ());

  // WARNING: Do not call ContextualASTVisitor, we don't want to visit the
  // lifetime
  // Maybe we should refactor LoopLabel instead ?
}

void
ASTValidation::visit (AST::ConstantItem &const_item)
{
  if (!const_item.has_expr () && context.back () != Context::TRAIT_IMPL)
    {
      rust_error_at (const_item.get_locus (),
		     "associated constant in %<impl%> without body");
    }
  AST::ContextualASTVisitor::visit (const_item);
}

void
ASTValidation::visit (AST::Union &item)
{
  if (item.get_variants ().empty ())
    rust_error_at (item.get_locus (), "unions cannot have zero fields");

  AST::ContextualASTVisitor::visit (item);
}

void
ASTValidation::visit (AST::Function &function)
{
  const auto &qualifiers = function.get_qualifiers ();
  if (qualifiers.is_async () && qualifiers.is_const ())
    rust_error_at (function.get_locus (),
		   "functions cannot be both %<const%> and %<async%>");

  if (qualifiers.is_const ()
      && (context.back () == Context::TRAIT_IMPL
	  || context.back () == Context::TRAIT))
    rust_error_at (function.get_locus (), ErrorCode::E0379,
		   "functions in traits cannot be declared %<const%>");

  // may change soon
  if (qualifiers.is_async ()
      && (context.back () == Context::TRAIT_IMPL
	  || context.back () == Context::TRAIT))
    rust_error_at (function.get_locus (), ErrorCode::E0706,
		   "functions in traits cannot be declared %<async%>");

  // if not an associated function but has a self parameter
  if (context.back () != Context::TRAIT
      && context.back () != Context::TRAIT_IMPL
      && context.back () != Context::INHERENT_IMPL
      && function.has_self_param ())
    rust_error_at (
      function.get_self_param ().get_locus (),
      "%<self%> parameter is only allowed in associated functions");

  if (function.is_external ())
    {
      if (function.has_body ())
	rust_error_at (function.get_locus (), "cannot have a body");

      auto &params = function.get_function_params ();

      if (params.size () == 1 && function.is_variadic ())
	rust_error_at (function.get_locus (),
		       "C-variadic function must be declared with at least one "
		       "named argument");

      for (auto it = params.begin (); it != params.end (); it++)
	{
	  if (it->get ()->is_variadic () && it + 1 != params.end ())
	    rust_error_at (
	      it->get ()->get_locus (),
	      "%<...%> must be the last argument of a C-variadic function");

	  // if functional parameter
	  if (!it->get ()->is_self () && !it->get ()->is_variadic ())
	    {
	      auto &param = static_cast<AST::FunctionParam &> (**it);
	      auto kind = param.get_pattern ().get_pattern_kind ();

	      if (kind != AST::Pattern::Kind::Identifier
		  && kind != AST::Pattern::Kind::Wildcard)
		rust_error_at (it->get ()->get_locus (), ErrorCode::E0130,
			       "pattern not allowed in foreign function");
	    }
	}
    }

  else
    {
      if (!function.has_body ())
	{
	  if (context.back () == Context::INHERENT_IMPL
	      || context.back () == Context::TRAIT_IMPL)
	    rust_error_at (function.get_locus (),
			   "associated function in %<impl%> without body");
	  else if (context.back () != Context::TRAIT)
	    rust_error_at (function.get_locus (),
			   "free function without a body");
	}
      auto &function_params = function.get_function_params ();
      for (auto it = function_params.begin (); it != function_params.end ();
	   it++)
	{
	  if (it->get ()->is_variadic ())
	    rust_error_at (
	      it->get ()->get_locus (),
	      "only foreign or %<unsafe extern \"C\"%> functions may "
	      "be C-variadic");
	}
    }

  AST::ContextualASTVisitor::visit (function);
}

void
ASTValidation::visit (AST::Trait &trait)
{
  if (trait.is_auto ())
    {
      if (trait.has_generics ())
	rust_error_at (trait.get_generic_params ()[0]->get_locus (),
		       ErrorCode::E0567,
		       "auto traits cannot have generic parameters");
      if (trait.has_type_param_bounds ())
	rust_error_at (trait.get_type_param_bounds ()[0]->get_locus (),
		       ErrorCode::E0568,
		       "auto traits cannot have super traits");
      if (trait.has_trait_items ())
	{
	  rust_error_at (trait.get_identifier ().get_locus (), ErrorCode::E0380,
			 "auto traits cannot have methods or associated items");
	  for (const auto &item : trait.get_trait_items ())
	    Error::Hint (item->get_locus (), "remove this item").emit ();
	}
    }

  AST::ContextualASTVisitor::visit (trait);
}

void
ASTValidation::visit (AST::Module &module)
{
  if (module.get_unsafety () == Unsafety::Unsafe)
    rust_error_at (module.get_locus (), "module cannot be declared unsafe");

  AST::ContextualASTVisitor::visit (module);
}

} // namespace Rust
