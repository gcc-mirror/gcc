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

#include "rust-macro-builtins.h"
#include "rust-macro-builtins-helpers.h"

namespace Rust {
tl::optional<AST::Fragment>
MacroBuiltin::file_handler (location_t invoc_locus, AST::MacroInvocData &)
{
  auto current_file = LOCATION_FILE (invoc_locus);
  auto file_str = AST::SingleASTNode (make_string (invoc_locus, current_file));
  auto str_token
    = make_token (Token::make_string (invoc_locus, std::move (current_file)));

  return AST::Fragment ({file_str}, std::move (str_token));
}

tl::optional<AST::Fragment>
MacroBuiltin::column_handler (location_t invoc_locus, AST::MacroInvocData &)
{
  auto current_column = LOCATION_COLUMN (invoc_locus);

  auto column_tok = make_token (
    Token::make_int (invoc_locus, std::to_string (current_column)));
  auto column_no = AST::SingleASTNode (std::unique_ptr<AST::Expr> (
    new AST::LiteralExpr (std::to_string (current_column), AST::Literal::INT,
			  PrimitiveCoreType::CORETYPE_U32, {}, invoc_locus)));

  return AST::Fragment ({column_no}, std::move (column_tok));
}

tl::optional<AST::Fragment>
MacroBuiltin::line_handler (location_t invoc_locus, AST::MacroInvocData &)
{
  auto current_line = LOCATION_LINE (invoc_locus);

  auto line_no = AST::SingleASTNode (std::unique_ptr<AST::Expr> (
    new AST::LiteralExpr (std::to_string (current_line), AST::Literal::INT,
			  PrimitiveCoreType::CORETYPE_U32, {}, invoc_locus)));
  auto tok
    = make_token (Token::make_int (invoc_locus, std::to_string (current_line)));

  return AST::Fragment ({line_no}, std::move (tok));
}
} // namespace Rust
