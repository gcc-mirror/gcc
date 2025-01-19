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

#include "rust-macro-builtins-helpers.h"

namespace Rust {

std::string
make_macro_path_str (BuiltinMacro kind)
{
  auto str = MacroBuiltin::builtins.lookup (kind);
  rust_assert (str.has_value ());

  return str.value ();
}

std::vector<std::unique_ptr<AST::MacroInvocation>>
check_for_eager_invocations (
  std::vector<std::unique_ptr<AST::Expr>> &expressions)
{
  std::vector<std::unique_ptr<AST::MacroInvocation>> pending;

  for (auto &expr : expressions)
    if (expr->get_ast_kind () == AST::Kind::MACRO_INVOCATION)
      pending.emplace_back (std::unique_ptr<AST::MacroInvocation> (
	static_cast<AST::MacroInvocation *> (expr->clone_expr ().release ())));

  return pending;
}

//
// Shorthand function for creating unique_ptr tokens
//
std::unique_ptr<AST::Token>
make_token (const TokenPtr tok)
{
  return std::unique_ptr<AST::Token> (new AST::Token (tok));
}

std::unique_ptr<AST::Expr>
make_string (location_t locus, std::string value)
{
  return std::unique_ptr<AST::Expr> (
    new AST::LiteralExpr (value, AST::Literal::STRING,
			  PrimitiveCoreType::CORETYPE_STR, {}, locus));
}

// TODO: Is this correct?
AST::Fragment
make_eager_builtin_invocation (
  BuiltinMacro kind, location_t locus, AST::DelimTokenTree arguments,
  std::vector<std::unique_ptr<AST::MacroInvocation>> &&pending_invocations)
{
  auto path_str = make_macro_path_str (kind);

  std::unique_ptr<AST::Expr> node = AST::MacroInvocation::Builtin (
    kind,
    AST::MacroInvocData (AST::SimplePath (
			   {AST::SimplePathSegment (path_str, locus)}),
			 std::move (arguments)),
    {}, locus, std::move (pending_invocations));

  return AST::Fragment ({AST::SingleASTNode (std::move (node))},
			arguments.to_token_stream ());
}

/* Match the end token of a macro given the start delimiter of the macro */
TokenId
macro_end_token (AST::DelimTokenTree &invoc_token_tree,
		 Parser<MacroInvocLexer> &parser)
{
  auto last_token_id = TokenId::RIGHT_CURLY;
  switch (invoc_token_tree.get_delim_type ())
    {
    case AST::DelimType::PARENS:
      last_token_id = TokenId::RIGHT_PAREN;
      rust_assert (parser.skip_token (LEFT_PAREN));
      break;

    case AST::DelimType::CURLY:
      rust_assert (parser.skip_token (LEFT_CURLY));
      break;

    case AST::DelimType::SQUARE:
      last_token_id = TokenId::RIGHT_SQUARE;
      rust_assert (parser.skip_token (LEFT_SQUARE));
      break;
    }

  return last_token_id;
}

// Expand and then extract a string literal from the macro
std::unique_ptr<AST::LiteralExpr>
try_extract_string_literal_from_fragment (const location_t &parent_locus,
					  std::unique_ptr<AST::Expr> &node)
{
  auto maybe_lit = static_cast<AST::LiteralExpr *> (node.get ());
  if (!node || !node->is_literal ()
      || maybe_lit->get_lit_type () != AST::Literal::STRING)
    {
      rust_error_at (parent_locus, "argument must be a string literal");
      if (node)
	rust_inform (node->get_locus (), "expanded from here");
      return nullptr;
    }
  return std::unique_ptr<AST::LiteralExpr> (
    static_cast<AST::LiteralExpr *> (node->clone_expr ().release ()));
}

std::vector<std::unique_ptr<AST::Expr>>
try_expand_many_expr (Parser<MacroInvocLexer> &parser,
		      const TokenId last_token_id, MacroExpander *expander,
		      bool &has_error)
{
  auto restrictions = Rust::ParseRestrictions ();
  // stop parsing when encountered a braces/brackets
  restrictions.expr_can_be_null = true;
  // we can't use std::optional, so...
  auto result = std::vector<std::unique_ptr<AST::Expr>> ();
  auto empty_expr = std::vector<std::unique_ptr<AST::Expr>> ();

  auto first_token = parser.peek_current_token ()->get_id ();
  if (first_token == COMMA)
    {
      rust_error_at (parser.peek_current_token ()->get_locus (),
		     "expected expression, found %<,%>");
      has_error = true;
      return empty_expr;
    }

  while (parser.peek_current_token ()->get_id () != last_token_id
	 && parser.peek_current_token ()->get_id () != END_OF_FILE)
    {
      auto expr = parser.parse_expr (AST::AttrVec (), restrictions);
      // something must be so wrong that the expression could not be parsed
      rust_assert (expr);
      result.push_back (std::move (expr));

      auto next_token = parser.peek_current_token ();
      if (!parser.skip_token (COMMA) && next_token->get_id () != last_token_id)
	{
	  rust_error_at (next_token->get_locus (), "expected token: %<,%>");
	  // TODO: is this recoverable? to avoid crashing the parser in the next
	  // fragment we have to exit early here
	  has_error = true;
	  return empty_expr;
	}
    }

  return result;
}

// Parse a single string literal from the given delimited token tree,
// and return the LiteralExpr for it. Allow for an optional trailing comma,
// but otherwise enforce that these are the only tokens.
// FIXME(Arthur): This function needs a rework - it should not emit errors, it
// should probably be smaller
std::unique_ptr<AST::Expr>
parse_single_string_literal (BuiltinMacro kind,
			     AST::DelimTokenTree &invoc_token_tree,
			     location_t invoc_locus, MacroExpander *expander)
{
  MacroInvocLexer lex (invoc_token_tree.to_token_stream ());
  Parser<MacroInvocLexer> parser (lex);

  auto last_token_id = macro_end_token (invoc_token_tree, parser);

  std::unique_ptr<AST::LiteralExpr> lit_expr = nullptr;
  std::unique_ptr<AST::MacroInvocation> macro_invoc = nullptr;

  if (parser.peek_current_token ()->get_id () == STRING_LITERAL)
    {
      lit_expr = parser.parse_literal_expr ();
      parser.maybe_skip_token (COMMA);
      if (parser.peek_current_token ()->get_id () != last_token_id)
	{
	  lit_expr = nullptr;
	  rust_error_at (invoc_locus, "macro takes 1 argument");
	}
    }
  else if (parser.peek_current_token ()->get_id () == last_token_id)
    rust_error_at (invoc_locus, "macro takes 1 argument");
  else
    {
      macro_invoc = parser.parse_macro_invocation (AST::AttrVec ());

      parser.maybe_skip_token (COMMA);
      if (parser.peek_current_token ()->get_id () != last_token_id)
	{
	  lit_expr = nullptr;
	  rust_error_at (invoc_locus, "macro takes 1 argument");
	}

      if (macro_invoc != nullptr)
	{
	  auto path_str = make_macro_path_str (kind);

	  auto pending_invocations
	    = std::vector<std::unique_ptr<AST::MacroInvocation>> ();

	  pending_invocations.push_back (std::move (macro_invoc));

	  return AST::MacroInvocation::Builtin (
	    kind,
	    AST::MacroInvocData (AST::SimplePath ({AST::SimplePathSegment (
				   path_str, invoc_locus)}),
				 std::move (invoc_token_tree)),
	    {}, invoc_locus, std::move (pending_invocations));
	}
      else
	{
	  rust_error_at (invoc_locus, "argument must be a string literal or a "
				      "macro which expands to a string");
	}
    }

  parser.skip_token (last_token_id);

  return std::unique_ptr<AST::Expr> (std::move (lit_expr));
}

/* Treat PATH as a path relative to the source file currently being
   compiled, and return the absolute path for it.  */
std::string
source_relative_path (std::string path, location_t locus)
{
  std::string compile_fname = LOCATION_FILE (locus);

  auto dir_separator_pos = compile_fname.rfind (file_separator);

  /* If there is no file_separator in the path, use current dir ('.').  */
  std::string dirname;
  if (dir_separator_pos == std::string::npos)
    dirname = std::string (".") + file_separator;
  else
    dirname = compile_fname.substr (0, dir_separator_pos) + file_separator;

  return dirname + path;
}

/* Read the full contents of the file FILENAME and return them in a vector.
   FIXME: platform specific.  */
tl::optional<std::vector<uint8_t>>
load_file_bytes (location_t invoc_locus, const char *filename)
{
  RAIIFile file_wrap (filename);
  if (file_wrap.get_raw () == nullptr)
    {
      rust_error_at (invoc_locus, "cannot open filename %s: %m", filename);
      return tl::nullopt;
    }

  FILE *f = file_wrap.get_raw ();
  fseek (f, 0L, SEEK_END);
  long fsize = ftell (f);
  fseek (f, 0L, SEEK_SET);

  std::vector<uint8_t> buf (fsize);

  if (fsize > 0 && fread (&buf[0], fsize, 1, f) != 1)
    {
      rust_error_at (invoc_locus, "error reading file %s: %m", filename);
      return std::vector<uint8_t> ();
    }

  return buf;
}
} // namespace Rust