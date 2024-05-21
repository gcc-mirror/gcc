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

#include "rust-macro-builtins-asm.h"

namespace Rust {

tl::optional<InlineAsmDirSpec>
parseDirSpec (Parser<MacroInvocLexer> &parser, TokenId last_token_id)
{
  return tl::nullopt;
}

int
parse_clobber_abi (Parser<MacroInvocLexer> &parser, TokenId last_token_id,
		   AsmArg &args)
{
  // clobber_abi := "clobber_abi(" <abi> *("," <abi>) [","] ")"

  // PARSE EVERYTHING COMMITTEDLY IN THIS FUNCTION, WE CONFIRMED VIA clobber_abi
  // identifier keyword

  if (!parser.skip_token (LEFT_PAREN))
    {
      // TODO: Raise error exactly like rustc if left parenthesis is not
      // encountered.
      return -1;
    }

  if (parser.skip_token (RIGHT_PAREN))
    {
      // TODO: We encountered a "clobber_abi()", which should be illegal?
      // https://github.com/rust-lang/rust/blob/c00957a3e269219413041a4e3565f33b1f9d0779/compiler/rustc_builtin_macros/src/asm.rs#L381
      return -1;
    }

  ClobberAbis new_abis;

  auto token = parser.peek_current_token ();

  while (token->get_id () != last_token_id && token->get_id () != RIGHT_PAREN)
    {
      // Check if it is a string literal or not, codename: <ABI> in ABNF
      if (token->get_id () == STRING_LITERAL)
	{
	  // TODO: Caring for span in here.
	  new_abis.push_back (token->as_string ());
	}
      else
	{
	  // TODO: We encountered something that is not string literal, which
	  // should be illegal, please emit the correct error
	  // https://github.com/rust-lang/rust/blob/b92758a9aef1cef7b79e2b72c3d8ba113e547f89/compiler/rustc_builtin_macros/src/asm.rs#L387
	}

      if (parser.skip_token (RIGHT_PAREN))
	{
	  break;
	}

      if (!parser.skip_token (COMMA))
	{
	  // TODO: If the skip of comma is unsuccessful, which should be
	  // illegal, pleaes emit the correct error.
	  return -1;
	}
    }

  // Done processing the local clobber abis, push that to the main Args in
  // argument

  for (auto abi : new_abis)
    {
      args.clobber_abis.push_back (abi);
    }

  return 0;
}

int
parse_options (Parser<MacroInvocLexer> &parser, TokenId last_token_id,
	       AsmArg &args, bool is_global_asm)
{
  // Parse everything commitedly
  if (!p.skip_token (LEFT_PAREN))
    {
      // We have shifted `options` to search for the left parenthesis next, we
      // should error out if this is not possible.
      // TODO: report some error.
      return -1;
    }

  auto token = parser.peek_current_token ();
  while (token->get_id () != last_token_id && token->get_id () != RIGHT_PAREN)
    {
      parser.skip_token ();
      token = parser.peek_current_token ();
    }
}
bool
check_identifier (Parser<MacroInvocLexer> &p, std::string ident)
{
  auto token = p.peek_current_token ();

  if (token->get_id () == IDENTIFIER
      && (token->as_string () == ident || ident == ""))
    {
      p.skip_token ();
      return true;
    }
  else
    {
      return false;
    }
}
tl::optional<std::string>
parse_format_string (Parser<MacroInvocLexer> &parser, TokenId last_token_id)
{
  auto token = parser.peek_current_token ();

  if (token->get_id () != last_token_id && token->get_id () == STRING_LITERAL)
    {
      // very nice, we got a supposedly formatted string.
      std::cout << token->get_token_description () << std::endl;
      parser.skip_token ();
      return "formatted string";
    }
  else
    {
      parser.skip_token ();
      std::cout << token->get_token_description () << std::endl;

      rust_error_at (token->get_locus (),
		     "asm template must be a string literal");
      return tl::nullopt;
    }
}

tl::optional<AST::Fragment>
MacroBuiltin::global_asm_handler (location_t invoc_locus,
				  AST::MacroInvocData &invoc)
{
  // Just to clarify the code
  bool is_global_asm = true;
  return parse_asm (invoc_locus, invoc, is_global_asm);
}

tl::optional<AST::Fragment>
MacroBuiltin::nonglobal_asm_handler (location_t invoc_locus,
				     AST::MacroInvocData &invoc)
{
  // Just to clarify the code
  bool is_global_asm = false;
  return parse_asm (invoc_locus, invoc, is_global_asm);
}

tl::optional<AsmArg>
parseAsmArg (Parser<MacroInvocLexer> &parser, TokenId last_token_id,
	     bool is_global_asm)
{
  auto token = parser.peek_current_token ();
  AsmArg arg;
  tl::optional<std::string> fm_string;
  while (token->get_id () != last_token_id)
    {
      std::cout << token->get_token_description () << std::endl;

      token = parser.peek_current_token ();

      // We accept a comma token here.
      if (token->get_id () != COMMA)
	{
	  break;
	}
      parser.skip_token ();

      // And if that token comma is also the trailing comma, we break
      // TODO: Check with mentor see what last_token_id means
      token = parser.peek_current_token ();
      if (token->get_id () == COMMA && token->get_id () == last_token_id)
	{
	  parser.skip_token ();
	  break;
	}

      // Ok after the left paren is good, we better be parsing correctly
      // everything in here, which is operand in ABNF

      // TODO: Parse clobber abi, eat the identifier named "clobber_abi" if true
      if (check_identifier (parser, "clobber_abi"))
	{
	  std::cout << "Clobber abi tee hee" << std::endl;
	  continue;
	}

      // TODO: Parse options
      if (check_identifier (parser, "options"))
	{
	  std::cout << "Parse optoins" << std::endl;
	  continue;
	}

      // Ok after we have check that neither clobber_abi nor options works, the
      // only other logical choice is reg_operand
      fm_string = parse_format_string (parser, last_token_id);
    }
  return tl::nullopt;
}

static tl::optional<AST::Fragment>
parse_asm (location_t invoc_locus, AST::MacroInvocData &invoc,
	   bool is_global_asm)
{
  // From the rule of asm.
  // We first peek and see if it is a format string or not.
  // If yes, we process the first ever format string, and move on to the
  // recurrent of format string Else we exit out

  // After that, we peek and see if it is a reoccuring stream of format string
  // or not. If it is, keep on going to do this format string. Else, move on

  // After that, we peek and see if it is a reoccuring stream of operands or not
  // If it is, keep on going to do this operand thingy.
  // Else, move on

  // We check if there is an optional "," at the end, per ABNF spec.
  // If it is, consume it.

  // Done
  MacroInvocLexer lex (invoc.get_delim_tok_tree ().to_token_stream ());
  Parser<MacroInvocLexer> parser (lex);
  auto last_token_id = macro_end_token (invoc.get_delim_tok_tree (), parser);

  // Parse the first ever formatted string, success or not, will skip 1 token
  auto fm_string = parse_format_string (parser, last_token_id);
  if (fm_string == tl::nullopt)
    return tl::nullopt;

  // formatted string stream
  auto token = parser.peek_current_token ();
  while (token->get_id () != last_token_id)
    {
      std::cout << token->get_token_description () << std::endl;
      token = parser.peek_current_token ();
      if (token->get_id () != COMMA)
	{
	  break;
	}
      parser.skip_token ();
      // Ok after the comma is good, we better be parsing correctly everything
      // in here, which is formatted string in ABNF

      fm_string = parse_format_string (parser, last_token_id);
    }

  // operands stream, also handles the optional ","
  parseAsmArg (parser, last_token_id, is_global_asm);

  return tl::nullopt;
}

} // namespace Rust
