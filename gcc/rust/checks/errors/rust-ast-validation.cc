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

namespace Rust {

namespace {
// TODO: make constexpr when update to c++20
const std::map<std::string, TokenId> keywords = {
#define RS_TOKEN(x, y)
#define RS_TOKEN_KEYWORD(tok, key) {key, tok},
  RS_TOKEN_LIST
#undef RS_TOKEN_KEYWORD
#undef RS_TOKEN
};
} // namespace

void
ASTValidation::visit (AST::Lifetime &lifetime)
{
  auto name = lifetime.get_lifetime_name ();
  auto valid = std::set<std::string>{"static", "_"};
  if (valid.find (name) == valid.end ()
      && keywords.find (name) != keywords.end ())
    rust_error_at (lifetime.get_locus (), "lifetimes cannot use keyword names");

  AST::ContextualASTVisitor::visit (lifetime);
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
