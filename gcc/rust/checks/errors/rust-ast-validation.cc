// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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
#include "rust-diagnostics.h"
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

} // namespace Rust
