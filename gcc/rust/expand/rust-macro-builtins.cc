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

#include "rust-macro-builtins.h"
#include "rust-ast.h"
#include "rust-diagnostics.h"
#include "rust-expr.h"
#include "rust-session-manager.h"
#include "rust-macro-invoc-lexer.h"
#include "rust-lex.h"
#include "rust-parse.h"
#include "rust-early-name-resolver.h"
#include "rust-attribute-visitor.h"

namespace Rust {
namespace {

/**
 * Shorthand function for creating unique_ptr tokens
 */
static std::unique_ptr<AST::Token>
make_token (const TokenPtr tok)
{
  return std::unique_ptr<AST::Token> (new AST::Token (tok));
}

std::unique_ptr<AST::Expr>
make_string (Location locus, std::string value)
{
  return std::unique_ptr<AST::Expr> (
    new AST::LiteralExpr (value, AST::Literal::STRING,
			  PrimitiveCoreType::CORETYPE_STR, {}, locus));
}

// TODO: Is this correct?
static AST::Fragment
make_eager_builtin_invocation (
  AST::BuiltinMacro kind, Location locus, AST::DelimTokenTree arguments,
  std::vector<std::unique_ptr<AST::MacroInvocation>> &&pending_invocations)
{
  std::string path_str;

  switch (kind)
    {
    // TODO: Should this be a table lookup?
    case AST::BuiltinMacro::Assert:
      path_str = "assert";
      break;
    case AST::BuiltinMacro::File:
      path_str = "file";
      break;
    case AST::BuiltinMacro::Line:
      path_str = "line";
      break;
    case AST::BuiltinMacro::Column:
      path_str = "column";
      break;
    case AST::BuiltinMacro::IncludeBytes:
      path_str = "include_bytes";
      break;
    case AST::BuiltinMacro::IncludeStr:
      path_str = "include_str";
      break;
    case AST::BuiltinMacro::CompileError:
      path_str = "compile_error";
      break;
    case AST::BuiltinMacro::Concat:
      path_str = "concat";
      break;
    case AST::BuiltinMacro::Env:
      path_str = "env";
      break;
    case AST::BuiltinMacro::Cfg:
      path_str = "cfg";
      break;
    case AST::BuiltinMacro::Include:
      path_str = "include";
      break;
    }

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
static inline TokenId
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

/* Expand and then extract a string literal from the macro */
static std::unique_ptr<AST::LiteralExpr>
try_extract_string_literal_from_fragment (const Location &parent_locus,
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

static std::vector<std::unique_ptr<AST::Expr>>
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

/* Parse a single string literal from the given delimited token tree,
   and return the LiteralExpr for it. Allow for an optional trailing comma,
   but otherwise enforce that these are the only tokens.  */

std::unique_ptr<AST::LiteralExpr>
parse_single_string_literal (AST::DelimTokenTree &invoc_token_tree,
			     Location invoc_locus, MacroExpander *expander)
{
  MacroInvocLexer lex (invoc_token_tree.to_token_stream ());
  Parser<MacroInvocLexer> parser (lex);

  auto last_token_id = macro_end_token (invoc_token_tree, parser);

  std::unique_ptr<AST::LiteralExpr> lit_expr = nullptr;

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
    rust_error_at (invoc_locus, "argument must be a string literal");

  parser.skip_token (last_token_id);

  return lit_expr;
}

/* Treat PATH as a path relative to the source file currently being
   compiled, and return the absolute path for it.  */

std::string
source_relative_path (std::string path, Location locus)
{
  std::string compile_fname
    = Session::get_instance ().linemap->location_file (locus);

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

std::vector<uint8_t>
load_file_bytes (const char *filename)
{
  RAIIFile file_wrap (filename);
  if (file_wrap.get_raw () == nullptr)
    {
      rust_error_at (Location (), "cannot open filename %s: %m", filename);
      return std::vector<uint8_t> ();
    }

  FILE *f = file_wrap.get_raw ();
  fseek (f, 0L, SEEK_END);
  long fsize = ftell (f);
  fseek (f, 0L, SEEK_SET);

  std::vector<uint8_t> buf (fsize);

  if (fread (&buf[0], fsize, 1, f) != 1)
    {
      rust_error_at (Location (), "error reading file %s: %m", filename);
      return std::vector<uint8_t> ();
    }

  return buf;
}
} // namespace

AST::Fragment
MacroBuiltin::assert_handler (Location, AST::MacroInvocData &)
{
  rust_debug ("assert!() called");

  return AST::Fragment::create_error ();
}

AST::Fragment
MacroBuiltin::file_handler (Location invoc_locus, AST::MacroInvocData &)
{
  auto current_file
    = Session::get_instance ().linemap->location_file (invoc_locus);
  auto file_str = AST::SingleASTNode (make_string (invoc_locus, current_file));
  auto str_token
    = make_token (Token::make_string (invoc_locus, std::move (current_file)));

  return AST::Fragment ({file_str}, std::move (str_token));
}

AST::Fragment
MacroBuiltin::column_handler (Location invoc_locus, AST::MacroInvocData &)
{
  auto current_column
    = Session::get_instance ().linemap->location_to_column (invoc_locus);

  auto column_tok = make_token (
    Token::make_int (invoc_locus, std::to_string (current_column)));
  auto column_no = AST::SingleASTNode (std::unique_ptr<AST::Expr> (
    new AST::LiteralExpr (std::to_string (current_column), AST::Literal::INT,
			  PrimitiveCoreType::CORETYPE_U32, {}, invoc_locus)));

  return AST::Fragment ({column_no}, std::move (column_tok));
}

/* Expand builtin macro include_bytes!("filename"), which includes the contents
   of the given file as reference to a byte array. Yields an expression of type
   &'static [u8; N].  */

AST::Fragment
MacroBuiltin::include_bytes_handler (Location invoc_locus,
				     AST::MacroInvocData &invoc)
{
  /* Get target filename from the macro invocation, which is treated as a path
     relative to the include!-ing file (currently being compiled).  */
  auto lit_expr
    = parse_single_string_literal (invoc.get_delim_tok_tree (), invoc_locus,
				   invoc.get_expander ());
  if (lit_expr == nullptr)
    return AST::Fragment::create_error ();

  std::string target_filename
    = source_relative_path (lit_expr->as_string (), invoc_locus);

  std::vector<uint8_t> bytes = load_file_bytes (target_filename.c_str ());

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
    new AST::BorrowExpr (std::move (array), false, false, {}, invoc_locus));

  auto node = AST::SingleASTNode (std::move (borrow));

  return AST::Fragment ({node}, std::move (toks));
} // namespace Rust

/* Expand builtin macro include_str!("filename"), which includes the contents
   of the given file as a string. The file must be UTF-8 encoded. Yields an
   expression of type &'static str.  */

AST::Fragment
MacroBuiltin::include_str_handler (Location invoc_locus,
				   AST::MacroInvocData &invoc)
{
  /* Get target filename from the macro invocation, which is treated as a path
     relative to the include!-ing file (currently being compiled).  */
  auto lit_expr
    = parse_single_string_literal (invoc.get_delim_tok_tree (), invoc_locus,
				   invoc.get_expander ());
  if (lit_expr == nullptr)
    return AST::Fragment::create_error ();

  std::string target_filename
    = source_relative_path (lit_expr->as_string (), invoc_locus);

  std::vector<uint8_t> bytes = load_file_bytes (target_filename.c_str ());

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
    str = std::string ((const char *) &bytes[0], bytes.size ());

  auto node = AST::SingleASTNode (make_string (invoc_locus, str));
  auto str_tok = make_token (Token::make_string (invoc_locus, std::move (str)));

  // FIXME: Do not return an empty token vector here
  return AST::Fragment ({node}, std::move (str_tok));
}

/* Expand builtin macro compile_error!("error"), which forces a compile error
   during the compile time. */
AST::Fragment
MacroBuiltin::compile_error_handler (Location invoc_locus,
				     AST::MacroInvocData &invoc)
{
  auto lit_expr
    = parse_single_string_literal (invoc.get_delim_tok_tree (), invoc_locus,
				   invoc.get_expander ());
  if (lit_expr == nullptr)
    return AST::Fragment::create_error ();

  std::string error_string = lit_expr->as_string ();
  rust_error_at (invoc_locus, "%s", error_string.c_str ());

  return AST::Fragment::create_error ();
}

static std::vector<std::unique_ptr<AST::MacroInvocation>>
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

/* Expand builtin macro concat!(), which joins all the literal parameters
   into a string with no delimiter. */

// This is a weird one. We want to do something where, if something cannot be
// expanded yet (i.e. macro invocation?) we return the whole MacroInvocation
// node again but expanded as much as possible.
// Is that possible? How do we do that?
//
// Let's take a few examples:
//
// 1. concat!(1, 2, true);
// 2. concat!(a!(), 2, true);
// 3. concat!(concat!(1, false), 2, true);
// 4. concat!(concat!(1, a!()), 2, true);
//
// 1. We simply want to return the new fragment: "12true"
// 2. We want to return `concat!(a_expanded, 2, true)` as a fragment
// 3. We want to return `concat!(1, false, 2, true)`
// 4. We want to return `concat!(concat!(1, a_expanded), 2, true);
//
// How do we do that?
//
// For each (un)expanded fragment: we check if it is expanded fully
//
// 1. What is expanded fully?
// 2. How to check?
//
// If it is expanded fully and not a literal, then we error out.
// Otherwise we simply emplace it back and keep going.
//
// In the second case, we must mark that this concat invocation still has some
// expansion to do: This allows us to return a `MacroInvocation { ... }` as an
// AST fragment, instead of a completed string.
//
// This means that we must change all the `try_expand_many_*` APIs and so on to
// return some sort of index or way to signify that we might want to reuse some
// bits and pieces of the original token tree.
//
// Now, before that: How do we resolve the names used in a builtin macro
// invocation?
// Do we split the two passes of parsing the token tree and then expanding it?
// Can we do that easily?
AST::Fragment
MacroBuiltin::concat_handler (Location invoc_locus, AST::MacroInvocData &invoc)
{
  auto invoc_token_tree = invoc.get_delim_tok_tree ();
  MacroInvocLexer lex (invoc_token_tree.to_token_stream ());
  Parser<MacroInvocLexer> parser (lex);

  auto str = std::string ();
  bool has_error = false;

  auto last_token_id = macro_end_token (invoc_token_tree, parser);

  auto start = lex.get_offs ();
  /* NOTE: concat! could accept no argument, so we don't have any checks here */
  auto expanded_expr = try_expand_many_expr (parser, last_token_id,
					     invoc.get_expander (), has_error);
  auto end = lex.get_offs ();

  auto tokens = lex.get_token_slice (start, end);

  auto pending_invocations = check_for_eager_invocations (expanded_expr);
  if (!pending_invocations.empty ())
    return make_eager_builtin_invocation (AST::BuiltinMacro::Concat,
					  invoc_locus,
					  invoc.get_delim_tok_tree (),
					  std::move (pending_invocations));

  for (auto &expr : expanded_expr)
    {
      if (!expr->is_literal ()
	  && expr->get_ast_kind () != AST::MACRO_INVOCATION)
	{
	  has_error = true;
	  rust_error_at (expr->get_locus (), "expected a literal");
	  // diagnostics copied from rustc
	  rust_inform (expr->get_locus (),
		       "only literals (like %<\"foo\"%>, %<42%> and "
		       "%<3.14%>) can be passed to %<concat!()%>");
	  continue;
	}
      auto *literal = static_cast<AST::LiteralExpr *> (expr.get ());
      if (literal->get_lit_type () == AST::Literal::BYTE
	  || literal->get_lit_type () == AST::Literal::BYTE_STRING)
	{
	  has_error = true;
	  rust_error_at (expr->get_locus (),
			 "cannot concatenate a byte string literal");
	  continue;
	}
      str += literal->as_string ();
    }

  parser.skip_token (last_token_id);

  if (has_error)
    return AST::Fragment::create_error ();

  auto node = AST::SingleASTNode (make_string (invoc_locus, str));
  auto str_tok = make_token (Token::make_string (invoc_locus, std::move (str)));

  return AST::Fragment ({node}, std::move (str_tok));
}

/* Expand builtin macro env!(), which inspects an environment variable at
   compile time. */
AST::Fragment
MacroBuiltin::env_handler (Location invoc_locus, AST::MacroInvocData &invoc)
{
  auto invoc_token_tree = invoc.get_delim_tok_tree ();
  MacroInvocLexer lex (invoc_token_tree.to_token_stream ());
  Parser<MacroInvocLexer> parser (lex);

  auto last_token_id = macro_end_token (invoc_token_tree, parser);
  std::unique_ptr<AST::LiteralExpr> error_expr = nullptr;
  std::unique_ptr<AST::LiteralExpr> lit_expr = nullptr;
  bool has_error = false;

  auto start = lex.get_offs ();
  auto expanded_expr = try_expand_many_expr (parser, last_token_id,
					     invoc.get_expander (), has_error);
  auto end = lex.get_offs ();

  auto tokens = lex.get_token_slice (start, end);

  if (has_error)
    return AST::Fragment::create_error ();

  auto pending = check_for_eager_invocations (expanded_expr);
  if (!pending.empty ())
    return make_eager_builtin_invocation (AST::BuiltinMacro::Env, invoc_locus,
					  invoc_token_tree,
					  std::move (pending));

  if (expanded_expr.size () < 1 || expanded_expr.size () > 2)
    {
      rust_error_at (invoc_locus, "env! takes 1 or 2 arguments");
      return AST::Fragment::create_error ();
    }
  if (expanded_expr.size () > 0)
    {
      if (!(lit_expr
	    = try_extract_string_literal_from_fragment (invoc_locus,
							expanded_expr[0])))
	{
	  return AST::Fragment::create_error ();
	}
    }
  if (expanded_expr.size () > 1)
    {
      if (!(error_expr
	    = try_extract_string_literal_from_fragment (invoc_locus,
							expanded_expr[1])))
	{
	  return AST::Fragment::create_error ();
	}
    }

  parser.skip_token (last_token_id);

  auto env_value = getenv (lit_expr->as_string ().c_str ());

  if (env_value == nullptr)
    {
      if (error_expr == nullptr)
	rust_error_at (invoc_locus, "environment variable %qs not defined",
		       lit_expr->as_string ().c_str ());
      else
	rust_error_at (invoc_locus, "%s", error_expr->as_string ().c_str ());
      return AST::Fragment::create_error ();
    }

  auto node = AST::SingleASTNode (make_string (invoc_locus, env_value));
  auto tok
    = make_token (Token::make_string (invoc_locus, std::move (env_value)));

  // FIXME: Do not return an empty token vector here
  return AST::Fragment ({node}, std::move (tok));
}

AST::Fragment
MacroBuiltin::cfg_handler (Location invoc_locus, AST::MacroInvocData &invoc)
{
  // only parse if not already parsed
  if (!invoc.is_parsed ())
    {
      std::unique_ptr<AST::AttrInputMetaItemContainer> converted_input (
	invoc.get_delim_tok_tree ().parse_to_meta_item ());

      if (converted_input == nullptr)
	{
	  rust_debug ("DEBUG: failed to parse macro to meta item");
	  // TODO: do something now? is this an actual error?
	}
      else
	{
	  std::vector<std::unique_ptr<AST::MetaItemInner>> meta_items (
	    std::move (converted_input->get_items ()));
	  invoc.set_meta_item_output (std::move (meta_items));
	}
    }

  /* TODO: assuming that cfg! macros can only have one meta item inner, like cfg
   * attributes */
  if (invoc.get_meta_items ().size () != 1)
    return AST::Fragment::create_error ();

  bool result = invoc.get_meta_items ()[0]->check_cfg_predicate (
    Session::get_instance ());
  auto literal_exp = AST::SingleASTNode (std::unique_ptr<AST::Expr> (
    new AST::LiteralExpr (result ? "true" : "false", AST::Literal::BOOL,
			  PrimitiveCoreType::CORETYPE_BOOL, {}, invoc_locus)));
  auto tok = make_token (
    Token::make (result ? TRUE_LITERAL : FALSE_LITERAL, invoc_locus));

  // FIXME: Do not return an empty token vector here
  return AST::Fragment ({literal_exp}, std::move (tok));
}

/* Expand builtin macro include!(), which includes a source file at the current
 scope compile time. */

AST::Fragment
MacroBuiltin::include_handler (Location invoc_locus, AST::MacroInvocData &invoc)
{
  /* Get target filename from the macro invocation, which is treated as a path
     relative to the include!-ing file (currently being compiled).  */
  auto lit_expr
    = parse_single_string_literal (invoc.get_delim_tok_tree (), invoc_locus,
				   invoc.get_expander ());
  if (lit_expr == nullptr)
    return AST::Fragment::create_error ();

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

  auto parsed_items = parser.parse_items ();
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
  for (auto &item : parsed_items)
    {
      AST::SingleASTNode node (std::move (item));
      nodes.push_back (node);
    }

  // FIXME: This returns an empty vector of tokens and works fine, but is that
  // the expected behavior? `include` macros are a bit harder to reason about
  // since they include tokens. Furthermore, our lexer has no easy way to return
  // a slice of tokens like the MacroInvocLexer. So it gets even harder to
  // extrac tokens from here. For now, let's keep it that way and see if it
  // eventually breaks, but I don't expect it to cause many issues since the
  // list of tokens is only used when a macro invocation mixes eager
  // macro invocations and already expanded tokens. Think
  // `concat!(a!(), 15, b!())`. We need to be able to expand a!(), expand b!(),
  // and then insert the `15` token in between. In the case of `include!()`, we
  // only have one argument. So it's either going to be a macro invocation or a
  // string literal.
  return AST::Fragment (nodes, std::vector<std::unique_ptr<AST::Token>> ());
}

AST::Fragment
MacroBuiltin::line_handler (Location invoc_locus, AST::MacroInvocData &)
{
  auto current_line
    = Session::get_instance ().linemap->location_to_line (invoc_locus);

  auto line_no = AST::SingleASTNode (std::unique_ptr<AST::Expr> (
    new AST::LiteralExpr (std::to_string (current_line), AST::Literal::INT,
			  PrimitiveCoreType::CORETYPE_U32, {}, invoc_locus)));
  auto tok
    = make_token (Token::make_int (invoc_locus, std::to_string (current_line)));

  // FIXME: Do not return an empty token vector here
  return AST::Fragment ({line_no}, std::move (tok));
}

} // namespace Rust
