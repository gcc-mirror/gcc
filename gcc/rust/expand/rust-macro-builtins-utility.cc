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

#include "rust-fmt.h"
#include "rust-macro-builtins.h"
#include "rust-macro-builtins-helpers.h"

namespace Rust {

/* Expand builtin macro compile_error!("error"), which forces a compile error
   during the compile time. */
tl::optional<AST::Fragment>
MacroBuiltin::compile_error_handler (location_t invoc_locus,
				     AST::MacroInvocData &invoc)
{
  auto lit_expr
    = parse_single_string_literal (BuiltinMacro::CompileError,
				   invoc.get_delim_tok_tree (), invoc_locus,
				   invoc.get_expander ());
  if (lit_expr == nullptr)
    return AST::Fragment::create_error ();

  rust_assert (lit_expr->is_literal ());

  std::string error_string = lit_expr->as_string ();
  rust_error_at (invoc_locus, "%s", error_string.c_str ());

  return AST::Fragment::create_error ();
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
tl::optional<AST::Fragment>
MacroBuiltin::concat_handler (location_t invoc_locus,
			      AST::MacroInvocData &invoc)
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
    return make_eager_builtin_invocation (BuiltinMacro::Concat, invoc_locus,
					  invoc.get_delim_tok_tree (),
					  std::move (pending_invocations));

  for (auto &expr : expanded_expr)
    {
      if (!expr->is_literal ()
	  && expr->get_ast_kind () != AST::Kind::MACRO_INVOCATION)
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
tl::optional<AST::Fragment>
MacroBuiltin::env_handler (location_t invoc_locus, AST::MacroInvocData &invoc)
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
    return make_eager_builtin_invocation (BuiltinMacro::Env, invoc_locus,
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

  return AST::Fragment ({node}, std::move (tok));
}

tl::optional<AST::Fragment>
MacroBuiltin::cfg_handler (location_t invoc_locus, AST::MacroInvocData &invoc)
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

  return AST::Fragment ({literal_exp}, std::move (tok));
}

tl::optional<AST::Fragment>
MacroBuiltin::stringify_handler (location_t invoc_locus,
				 AST::MacroInvocData &invoc)
{
  std::string content;
  auto invoc_token_tree = invoc.get_delim_tok_tree ();
  auto tokens = invoc_token_tree.to_token_stream ();

  // Tokens stream includes the first and last delimiter
  // which we need to skip.
  for (auto token = tokens.cbegin () + 1; token < tokens.cend () - 1; token++)
    {
      // Rust stringify format has no garantees but the reference compiler
      // removes spaces before some tokens depending on the lexer's behavior,
      // let's mimick some of those behaviors.
      auto token_id = (*token)->get_id ();
      if (token_id != RIGHT_PAREN && token_id != EXCLAM
	  && token != tokens.cbegin () + 1)
	{
	  content.push_back (' ');
	}
      content += (*token)->as_string ();
    }

  auto node = AST::SingleASTNode (make_string (invoc_locus, content));
  auto token
    = make_token (Token::make_string (invoc_locus, std::move (content)));
  return AST::Fragment ({node}, std::move (token));
}

} // namespace Rust