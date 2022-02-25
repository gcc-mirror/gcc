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

#include "rust-macro-builtins.h"
#include "rust-diagnostics.h"
#include "rust-expr.h"
#include "rust-session-manager.h"

namespace Rust {
namespace {
std::unique_ptr<AST::Expr>
make_string (Location locus, std::string value)
{
  return std::unique_ptr<AST::Expr> (
    new AST::LiteralExpr (value, AST::Literal::STRING,
			  PrimitiveCoreType::CORETYPE_STR, {}, locus));
}
} // namespace

AST::ASTFragment
MacroBuiltin::assert (Location invoc_locus, AST::MacroInvocData &invoc)
{
  rust_debug ("assert!() called");

  return AST::ASTFragment::create_empty ();
}

AST::ASTFragment
MacroBuiltin::file (Location invoc_locus, AST::MacroInvocData &invoc)
{
  auto current_file
    = Session::get_instance ().linemap->location_file (invoc_locus);
  auto file_str = AST::SingleASTNode (make_string (invoc_locus, current_file));

  return AST::ASTFragment ({file_str});
}
} // namespace Rust
