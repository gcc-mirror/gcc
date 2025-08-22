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
#include "rust-fmt.h"
#include "rust-macro-builtins-helpers.h"
#include "rust-expand-format-args.h"

namespace Rust {

struct FormatArgsInput
{
  std::string format_str;
  AST::FormatArguments args;
  // bool is_literal?
};

struct FormatArgsParseError
{
  enum class Kind
  {
    MissingArguments
  } kind;
};

static inline tl::expected<std::string, AST::Fragment>
format_args_parse_expr (location_t invoc_locus, AST::MacroInvocData &invoc,
			Parser<MacroInvocLexer> &parser,
			BuiltinMacro macro_kind)
{
  std::unique_ptr<AST::Expr> format_expr = parser.parse_expr ();
  rust_assert (format_expr);

  if (format_expr->get_expr_kind () == AST::Expr::Kind::MacroInvocation)
    {
      std::vector<std::unique_ptr<AST::MacroInvocation>> pending;
      pending.emplace_back (
	static_cast<AST::MacroInvocation *> (format_expr.release ()));
      return tl::unexpected<AST::Fragment> (
	make_eager_builtin_invocation (macro_kind, invoc_locus,
				       invoc.get_delim_tok_tree (),
				       std::move (pending)));
    }

  // TODO(Arthur): Clean this up - if we haven't parsed a string literal but a
  // macro invocation, what do we do here? return a tl::unexpected?
  rust_assert (format_expr->is_literal ());
  return static_cast<AST::LiteralExpr &> (*format_expr)
    .get_literal ()
    .as_string ();
}

static inline tl::expected<AST::FormatArguments, FormatArgsParseError>
format_args_parse_arguments (AST::MacroInvocData &invoc,
			     Parser<MacroInvocLexer> &parser,
			     TokenId last_token_id)
{
  // TODO: check if EOF - return that format_args!() requires at least one
  // argument

  auto args = AST::FormatArguments ();
  // TODO: Allow implicit captures ONLY if the the first arg is a string literal
  // and not a macro invocation

  // TODO: How to consume all of the arguments until the delimiter?

  // TODO: What we then want to do is as follows:
  // for each token, check if it is an identifier
  //     yes? is the next token an equal sign (=)
  //          yes?
  //              -> if that identifier is already present in our map, error
  //              out
  //              -> parse an expression, return a FormatArgument::Named
  //     no?
  //         -> if there have been named arguments before, error out
  //         (positional after named error)
  //         -> parse an expression, return a FormatArgument::Normal
  while (parser.peek_current_token ()->get_id () != last_token_id)
    {
      parser.skip_token (COMMA);

      // Check in case of an extraneous comma in the args list, which is
      // allowed - format_args!("fmt", arg, arg2,)
      if (parser.peek_current_token ()->get_id () == last_token_id)
	break;

      if (parser.peek_current_token ()->get_id () == IDENTIFIER
	  && parser.peek (1)->get_id () == EQUAL)
	{
	  // FIXME: This is ugly - just add a parser.parse_identifier()?
	  auto ident_tok = parser.peek_current_token ();
	  auto ident = Identifier (ident_tok);

	  parser.skip_token (IDENTIFIER);
	  parser.skip_token (EQUAL);

	  auto expr = parser.parse_expr ();

	  // TODO: Handle graciously
	  if (!expr)
	    rust_unreachable ();

	  args.push (AST::FormatArgument::named (ident, std::move (expr)));
	}
      else
	{
	  auto expr = parser.parse_expr ();

	  // TODO: Handle graciously
	  if (!expr)
	    rust_unreachable ();

	  args.push (AST::FormatArgument::normal (std::move (expr)));
	}
      // we need to skip commas, don't we?
    }

  return args;
}

tl::optional<AST::Fragment>
MacroBuiltin::format_args_handler (location_t invoc_locus,
				   AST::MacroInvocData &invoc,
				   AST::InvocKind semicolon,
				   AST::FormatArgs::Newline nl)
{
  MacroInvocLexer lex (invoc.get_delim_tok_tree ().to_token_stream ());
  Parser<MacroInvocLexer> parser (lex);

  auto last_token_id = macro_end_token (invoc.get_delim_tok_tree (), parser);

  auto format_str = format_args_parse_expr (invoc_locus, invoc, parser,
					    nl == AST::FormatArgs::Newline::Yes
					      ? BuiltinMacro::FormatArgsNl
					      : BuiltinMacro::FormatArgs);

  if (!format_str)
    {
      return std::move (format_str.error ());
    }

  auto args = format_args_parse_arguments (invoc, parser, last_token_id);

  if (!args)
    {
      rust_error_at (invoc_locus,
		     "could not parse arguments to %<format_args!()%>");
      return tl::nullopt;
    }

  // TODO(Arthur): We need to handle this
  // // if it is not a literal, it's an eager macro invocation - return it
  // if (!fmt_expr->is_literal ())
  //   {
  //     auto token_tree = invoc.get_delim_tok_tree ();
  //     return AST::Fragment ({AST::SingleASTNode (std::move (fmt_expr))},
  // 	    token_tree.to_token_stream ());
  //   }

  // TODO(Arthur): Handle this as well - raw strings are special for the
  // format_args parser auto fmt_str = static_cast<AST::LiteralExpr &>
  // (*fmt_arg.get ()); Switch on the format string to know if the string is raw
  // or cooked switch (fmt_str.get_lit_type ())
  //   {
  //   // case AST::Literal::RAW_STRING:
  //   case AST::Literal::STRING:
  //     break;
  //   case AST::Literal::CHAR:
  //   case AST::Literal::BYTE:
  //   case AST::Literal::BYTE_STRING:
  //   case AST::Literal::INT:
  //   case AST::Literal::FLOAT:
  //   case AST::Literal::BOOL:
  //   case AST::Literal::ERROR:
  //     rust_unreachable ();
  //   }

  bool append_newline = nl == AST::FormatArgs::Newline::Yes;

  auto fmt_str = std::move (format_str.value ());
  if (append_newline)
    fmt_str += '\n';

  auto pieces = Fmt::Pieces::collect (fmt_str, append_newline,
				      Fmt::ffi::ParseMode::Format);

  // TODO:
  // do the transformation into an AST::FormatArgs node
  // return that
  // expand it during lowering

  // TODO: we now need to take care of creating `unfinished_literal`? this is
  // for creating the `template`

  auto fmt_args_node = AST::FormatArgs (invoc_locus, std::move (pieces),
					std::move (args.value ()));

  auto expanded
    = Fmt::expand_format_args (fmt_args_node,
			       invoc.get_delim_tok_tree ().to_token_stream ());

  if (!expanded.has_value ())
    return AST::Fragment::create_error ();

  return *expanded;

  // auto node = std::unique_ptr<AST::Expr> (fmt_args_node);
  // auto single_node = AST::SingleASTNode (std::move (node));

  // return AST::Fragment ({std::move (single_node)},
  // 	invoc.get_delim_tok_tree ().to_token_stream ());
}

} // namespace Rust
