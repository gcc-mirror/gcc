// Copyright (C) 2020-2026 Free Software Foundation, Inc.

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

#include "rust-ast-fragment.h"
#include "rust-macro-builtins.h"
#include "rust-macro-builtins-helpers.h"
#include "rust-ast-builder.h"
#include "optional.h"
#include "rust-ast-collector.h"

namespace Rust {
tl::optional<AST::Fragment>
MacroBuiltin::assert_handler (location_t invoc_locus,
			      AST::MacroInvocData &invoc,
			      AST::InvocKind semicolon)
{
  rust_debug ("assert!() called");
  auto tt = invoc.get_delim_tok_tree ();
  MacroInvocLexer lex (tt.to_token_stream ());
  Parser<MacroInvocLexer> parser (lex);

  auto last_token_id = macro_end_token (tt, parser);

  // TODO: less args?
  auto expr_to_assert = parser.parse_expr ();
  if (!expr_to_assert.has_value ())
    {
      rust_error_at (invoc_locus,
		     "macro requires a boolean expression as an argument");
      return AST::Fragment::create_error ();
    }

  // don't need to expand macros -- panic! will handle it

  AST::Builder b (invoc_locus, AST::Builder::Source::Any);

  std::vector<std::unique_ptr<AST::TokenTree>> panic_tree;
  const_TokenPtr open = Token::make (TokenId::LEFT_PAREN, invoc_locus);
  panic_tree.push_back (std::make_unique<AST::Token> (std::move (open)));

  if (parser.maybe_skip_token (COMMA))
    {
      bool needs_stringify = true;
      while (parser.peek_current_token ()->get_id () != last_token_id
	     && parser.peek_current_token ()->get_id () != END_OF_FILE)
	{
	  needs_stringify = false;
	  auto token_tree = parser.parse_token_tree ();
	  if (!token_tree.has_value ())
	    {
	      // error already emitted (?)
	      return AST::Fragment::create_error ();
	    }
	  panic_tree.push_back (std::move (token_tree.value ()));
	}
      if (needs_stringify)
	{
	  // TODO: insert stringify invocation
	  (void) 0;
	}
    }

  const_TokenPtr close = Token::make (TokenId::RIGHT_PAREN, invoc_locus);
  panic_tree.push_back (std::make_unique<AST::Token> (std::move (close)));

  auto panic = AST::MacroInvocation::Regular (
    AST::MacroInvocData (AST::SimplePath (Identifier ("panic")),
			 AST::DelimTokenTree (AST::DelimType::PARENS,
					      std::move (panic_tree),
					      invoc_locus)),
    {} /* outer attributes */, invoc_locus, true /* semicoloned */);
  auto stmt = b.statementify (std::move (panic));
  std::vector<std::unique_ptr<AST::Stmt>> stmts;
  stmts.push_back (std::move (stmt));
  auto block = b.block (std::move (stmts));
  auto negated_condition = std::unique_ptr<AST::NegationExpr> (
    new AST::NegationExpr (std::move (expr_to_assert.value ()),
			   AST::NegationExpr::ExprType::NOT, {}, invoc_locus));

  auto if_expr = std::make_unique<AST::IfExpr> (
    std::move (negated_condition) /* condition*/, std::move (block),
    std::vector<AST::Attribute>{}, invoc_locus);

  auto node = AST::SingleASTNode (std::move (if_expr));

  AST::TokenCollector collector;
  collector.visit (node);
  std::vector<std::unique_ptr<AST::Token>> tokens;
  for (auto &&token : collector.collect_tokens ())
    tokens.push_back (std::make_unique<AST::Token> (token));

  return AST::Fragment ({node}, std::move (tokens));

  return AST::Fragment::create_error ();
}
} // namespace Rust
