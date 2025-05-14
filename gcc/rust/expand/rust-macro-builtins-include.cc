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

#include "rust-ast-fragment.h"
#include "rust-common.h"
#include "rust-macro-builtins.h"
#include "rust-macro-builtins-helpers.h"
#include "rust-session-manager.h"
#include "optional.h"
namespace Rust {
/* Expand builtin macro include_bytes!("filename"), which includes the contents
of the given file as reference to a byte array. Yields an expression of type
&'static [u8; N].  */

tl::optional<AST::Fragment>
MacroBuiltin::include_bytes_handler (location_t invoc_locus,
				     AST::MacroInvocData &invoc,
				     AST::InvocKind semicolon)
{
  /* Get target filename from the macro invocation, which is treated as a path
     relative to the include!-ing file (currently being compiled).  */
  auto lit_expr
    = parse_single_string_literal (BuiltinMacro::IncludeBytes,
				   invoc.get_delim_tok_tree (), invoc_locus,
				   invoc.get_expander ());
  if (lit_expr == nullptr)
    return AST::Fragment::create_error ();

  if (!lit_expr->is_literal ())
    {
      auto token_tree = invoc.get_delim_tok_tree ();
      return AST::Fragment ({AST::SingleASTNode (std::move (lit_expr))},
			    token_tree.to_token_stream ());
    }

  std::string target_filename
    = source_relative_path (lit_expr->as_string (), invoc_locus);

  auto maybe_bytes = load_file_bytes (invoc_locus, target_filename.c_str ());

  if (!maybe_bytes.has_value ())
    return AST::Fragment::create_error ();

  std::vector<uint8_t> bytes = maybe_bytes.value ();

  /* Is there a more efficient way to do this?  */
  std::vector<std::unique_ptr<AST::Expr>> elts;

  // We create the tokens for a borrow expression of a byte array, so
  // & [ <byte0>, <byte1>, ... ]
  std::vector<std::unique_ptr<AST::Token>> toks;
  toks.emplace_back (make_token (Token::make (AMP, invoc_locus)));
  toks.emplace_back (make_token (Token::make (LEFT_SQUARE, invoc_locus)));

  for (uint8_t b : bytes)
    {
      elts.emplace_back (
	new AST::LiteralExpr (std::string (1, (char) b), AST::Literal::BYTE,
			      PrimitiveCoreType::CORETYPE_U8,
			      {} /* outer_attrs */, invoc_locus));
      toks.emplace_back (make_token (Token::make_byte_char (invoc_locus, b)));
      toks.emplace_back (make_token (Token::make (COMMA, invoc_locus)));
    }

  toks.emplace_back (make_token (Token::make (RIGHT_SQUARE, invoc_locus)));

  auto elems = std::unique_ptr<AST::ArrayElems> (
    new AST::ArrayElemsValues (std::move (elts), invoc_locus));

  auto array = std::unique_ptr<AST::Expr> (
    new AST::ArrayExpr (std::move (elems), {}, {}, invoc_locus));

  auto borrow = std::unique_ptr<AST::Expr> (
    new AST::BorrowExpr (std::move (array), Mutability::Imm,
			 /* raw borrow */ false,
			 /* double borrow */ false, {}, invoc_locus));

  auto node = AST::SingleASTNode (std::move (borrow));

  return AST::Fragment ({node}, std::move (toks));
}

/* Expand builtin macro include_str!("filename"), which includes the contents
   of the given file as a string. The file must be UTF-8 encoded. Yields an
   expression of type &'static str.  */

tl::optional<AST::Fragment>
MacroBuiltin::include_str_handler (location_t invoc_locus,
				   AST::MacroInvocData &invoc,
				   AST::InvocKind semicolon)
{
  /* Get target filename from the macro invocation, which is treated as a path
     relative to the include!-ing file (currently being compiled).  */
  auto lit_expr
    = parse_single_string_literal (BuiltinMacro::IncludeStr,
				   invoc.get_delim_tok_tree (), invoc_locus,
				   invoc.get_expander ());
  if (lit_expr == nullptr)
    return AST::Fragment::create_error ();

  if (!lit_expr->is_literal ())
    {
      auto token_tree = invoc.get_delim_tok_tree ();
      return AST::Fragment ({AST::SingleASTNode (std::move (lit_expr))},
			    token_tree.to_token_stream ());
    }

  std::string target_filename
    = source_relative_path (lit_expr->as_string (), invoc_locus);

  auto maybe_bytes = load_file_bytes (invoc_locus, target_filename.c_str ());

  if (!maybe_bytes.has_value ())
    return AST::Fragment::create_error ();

  std::vector<uint8_t> bytes = maybe_bytes.value ();

  /* FIXME: reuse lexer */
  int expect_single = 0;
  for (uint8_t b : bytes)
    {
      if (expect_single)
	{
	  if ((b & 0xC0) != 0x80)
	    /* character was truncated, exit with expect_single != 0 */
	    break;
	  expect_single--;
	}
      else if (b & 0x80)
	{
	  if (b >= 0xF8)
	    {
	      /* more than 4 leading 1s */
	      expect_single = 1;
	      break;
	    }
	  else if (b >= 0xF0)
	    {
	      /* 4 leading 1s */
	      expect_single = 3;
	    }
	  else if (b >= 0xE0)
	    {
	      /* 3 leading 1s */
	      expect_single = 2;
	    }
	  else if (b >= 0xC0)
	    {
	      /* 2 leading 1s */
	      expect_single = 1;
	    }
	  else
	    {
	      /* only 1 leading 1 */
	      expect_single = 1;
	      break;
	    }
	}
    }

  std::string str;
  if (expect_single)
    rust_error_at (invoc_locus, "%s was not a valid utf-8 file",
		   target_filename.c_str ());
  else
    str = std::string ((const char *) bytes.data (), bytes.size ());

  auto node = AST::SingleASTNode (make_string (invoc_locus, str));
  auto str_tok = make_token (Token::make_string (invoc_locus, std::move (str)));

  return AST::Fragment ({node}, std::move (str_tok));
}

/* Expand builtin macro include!(), which includes a source file at the current
scope compile time. */

tl::optional<AST::Fragment>
MacroBuiltin::include_handler (location_t invoc_locus,
			       AST::MacroInvocData &invoc,
			       AST::InvocKind semicolon)
{
  bool is_semicoloned = semicolon == AST::InvocKind::Semicoloned;
  /* Get target filename from the macro invocation, which is treated as a path
     relative to the include!-ing file (currently being compiled).  */
  std::unique_ptr<AST::Expr> lit_expr
    = parse_single_string_literal (BuiltinMacro::Include,
				   invoc.get_delim_tok_tree (), invoc_locus,
				   invoc.get_expander (), is_semicoloned);
  if (lit_expr == nullptr)
    return AST::Fragment::create_error ();

  if (!lit_expr->is_literal ())
    {
      // We have to expand an inner macro eagerly
      auto token_tree = invoc.get_delim_tok_tree ();

      // parse_single_string_literal returned an AST::MacroInvocation, which
      // can either be an AST::Item or AST::Expr. Depending on the context the
      // original macro was invoked in, we will set AST::Item or AST::Expr
      // appropriately.
      if (is_semicoloned)
	{
	  std::unique_ptr<AST::Item> lit_item = std::unique_ptr<AST::Item> (
	    static_cast<AST::MacroInvocation *> (lit_expr.release ()));
	  return AST::Fragment ({AST::SingleASTNode (std::move (lit_item))},
				token_tree.to_token_stream ());
	}
      else
	return AST::Fragment ({AST::SingleASTNode (std::move (lit_expr))},
			      token_tree.to_token_stream ());
    }

  std::string filename
    = source_relative_path (lit_expr->as_string (), invoc_locus);
  auto target_filename
    = Rust::Session::get_instance ().include_extra_file (std::move (filename));

  RAIIFile target_file (target_filename);
  Linemap *linemap = Session::get_instance ().linemap;

  if (!target_file.ok ())
    {
      rust_error_at (lit_expr->get_locus (),
		     "cannot open included file %qs: %m", target_filename);
      return AST::Fragment::create_error ();
    }

  rust_debug ("Attempting to parse included file %s", target_filename);

  Lexer lex (target_filename, std::move (target_file), linemap);
  Parser<Lexer> parser (lex);
  std::unique_ptr<AST::Expr> parsed_expr = nullptr;
  std::vector<std::unique_ptr<AST::Item>> parsed_items{};

  if (is_semicoloned)
    parsed_items = parser.parse_items ();
  else
    parsed_expr = parser.parse_expr ();

  bool has_error = !parser.get_errors ().empty ();

  for (const auto &error : parser.get_errors ())
    error.emit ();

  if (has_error)
    {
      // inform the user that the errors above are from a included file
      rust_inform (invoc_locus, "included from here");
      return AST::Fragment::create_error ();
    }

  std::vector<AST::SingleASTNode> nodes{};
  if (is_semicoloned)
    for (auto &item : parsed_items)
      {
	AST::SingleASTNode node (std::move (item));
	nodes.push_back (node);
      }
  else
    {
      AST::SingleASTNode node (std::move (parsed_expr));
      nodes.push_back (node);
    }
  // FIXME: This returns an empty vector of tokens and works fine, but is that
  // the expected behavior? `include` macros are a bit harder to reason about
  // since they include tokens. Furthermore, our lexer has no easy way to return
  // a slice of tokens like the MacroInvocLexer. So it gets even harder to
  // extract tokens from here. For now, let's keep it that way and see if it
  // eventually breaks, but I don't expect it to cause many issues since the
  // list of tokens is only used when a macro invocation mixes eager
  // macro invocations and already expanded tokens. Think
  // `concat!(a!(), 15, b!())`. We need to be able to expand a!(), expand b!(),
  // and then insert the `15` token in between. In the case of `include!()`, we
  // only have one argument. So it's either going to be a macro invocation or a
  // string literal.
  return AST::Fragment (nodes, std::vector<std::unique_ptr<AST::Token>> ());
}
} // namespace Rust
