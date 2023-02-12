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
std::unique_ptr<AST::Expr>
make_string (Location locus, std::string value)
{
  return std::unique_ptr<AST::Expr> (
    new AST::LiteralExpr (value, AST::Literal::STRING,
			  PrimitiveCoreType::CORETYPE_STR, {}, locus));
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

/* Expand and extract an expression from the macro */

static inline AST::Fragment
try_expand_macro_expression (AST::Expr *expr, MacroExpander *expander)
{
  rust_assert (expander);

  auto attr_visitor = Rust::AttrVisitor (*expander);
  auto early_name_resolver = Resolver::EarlyNameResolver ();
  expr->accept_vis (early_name_resolver);
  expr->accept_vis (attr_visitor);
  return expander->take_expanded_fragment (attr_visitor);
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

static std::unique_ptr<AST::LiteralExpr>
try_expand_single_string_literal (AST::Expr *input_expr,
				  MacroExpander *expander)
{
  auto nodes = try_expand_macro_expression (input_expr, expander);
  if (nodes.is_error () || nodes.is_expression_fragment ())
    {
      rust_error_at (input_expr->get_locus (),
		     "argument must be a string literal");
      return nullptr;
    }
  auto expr = nodes.take_expression_fragment ();
  return try_extract_string_literal_from_fragment (input_expr->get_locus (),
						   expr);
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
      auto nodes = try_expand_macro_expression (expr.get (), expander);
      if (nodes.is_error ())
	{
	  // not macro
	  result.push_back (std::move (expr));
	}
      else if (!nodes.is_expression_fragment ())
	{
	  rust_error_at (expr->get_locus (), "expected expression");
	  has_error = true;
	  return empty_expr;
	}
      else
	{
	  result.push_back (nodes.take_expression_fragment ());
	}

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
    {
      // when the expression does not seem to be a string literal, we then try
      // to parse/expand it as macro to see if it expands to a string literal
      auto expr = parser.parse_expr ();
      lit_expr = try_expand_single_string_literal (expr.get (), expander);
    }

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

  return AST::Fragment::complete ({file_str});
}

AST::Fragment
MacroBuiltin::column_handler (Location invoc_locus, AST::MacroInvocData &)
{
  auto current_column
    = Session::get_instance ().linemap->location_to_column (invoc_locus);

  auto column_no = AST::SingleASTNode (std::unique_ptr<AST::Expr> (
    new AST::LiteralExpr (std::to_string (current_column), AST::Literal::INT,
			  PrimitiveCoreType::CORETYPE_U32, {}, invoc_locus)));

  return AST::Fragment::complete ({column_no});
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
  for (uint8_t b : bytes)
    {
      elts.emplace_back (
	new AST::LiteralExpr (std::string (1, (char) b), AST::Literal::BYTE,
			      PrimitiveCoreType::CORETYPE_U8,
			      {} /* outer_attrs */, invoc_locus));
    }

  auto elems = std::unique_ptr<AST::ArrayElems> (
    new AST::ArrayElemsValues (std::move (elts), invoc_locus));

  auto array = std::unique_ptr<AST::Expr> (
    new AST::ArrayExpr (std::move (elems), {}, {}, invoc_locus));

  auto borrow = std::unique_ptr<AST::Expr> (
    new AST::BorrowExpr (std::move (array), false, false, {}, invoc_locus));

  auto node = AST::SingleASTNode (std::move (borrow));
  return AST::Fragment::complete ({node});
}

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

  /* FIXME: Enforce that the file contents are valid UTF-8.  */
  std::string str ((const char *) &bytes[0], bytes.size ());

  auto node = AST::SingleASTNode (make_string (invoc_locus, str));
  return AST::Fragment::complete ({node});
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

/* Expand builtin macro concat!(), which joins all the literal parameters
   into a string with no delimiter. */

AST::Fragment
MacroBuiltin::concat_handler (Location invoc_locus, AST::MacroInvocData &invoc)
{
  auto invoc_token_tree = invoc.get_delim_tok_tree ();
  MacroInvocLexer lex (invoc_token_tree.to_token_stream ());
  Parser<MacroInvocLexer> parser (lex);

  auto str = std::string ();
  bool has_error = false;

  auto last_token_id = macro_end_token (invoc_token_tree, parser);

  /* NOTE: concat! could accept no argument, so we don't have any checks here */
  auto expanded_expr = try_expand_many_expr (parser, last_token_id,
					     invoc.get_expander (), has_error);
  for (auto &expr : expanded_expr)
    {
      if (!expr->is_literal ())
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
  return AST::Fragment::complete ({node});
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

  auto expanded_expr = try_expand_many_expr (parser, last_token_id,
					     invoc.get_expander (), has_error);
  if (has_error)
    return AST::Fragment::create_error ();
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
  return AST::Fragment::complete ({node});
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

  return AST::Fragment::complete ({literal_exp});
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
    error.emit_error ();

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

  return AST::Fragment::complete (nodes);
}

AST::Fragment
MacroBuiltin::line_handler (Location invoc_locus, AST::MacroInvocData &)
{
  auto current_line
    = Session::get_instance ().linemap->location_to_line (invoc_locus);

  auto line_no = AST::SingleASTNode (std::unique_ptr<AST::Expr> (
    new AST::LiteralExpr (std::to_string (current_line), AST::Literal::INT,
			  PrimitiveCoreType::CORETYPE_U32, {}, invoc_locus)));

  return AST::Fragment::complete ({line_no});
}

} // namespace Rust
