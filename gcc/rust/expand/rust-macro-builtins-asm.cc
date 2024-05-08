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
#include "rust-macro-invoc-lexer.h"

namespace Rust {

struct AsmParseError
{
};

// This is just an enum to hold some operands right now.
enum InlineAsmDirSpec
{
  In,
  Out,
  InOut,
  SplitInOut,
  Const,
  Sym,
  Label,
};

enum InlineAsmOptions
{

};

typedef std::string symbol_name;
typedef std::vector<AST::Expr> Templates;
typedef std::vector<InlineAsmDirSpec> Operands;
typedef std::map<std::string, int> RegisterArgs;
typedef std::map<symbol_name, int> ClobberAbis;
typedef std::map<symbol_name, int> NamedValues;

struct AsmArg
{
  Templates templates;
  Operands operands;
  std::map<symbol_name, int> named_values;
  RegisterArgs register_arguments;
  ClobberAbis clobber_abis;
  InlineAsmOptions options;
  std::vector<InlineAsmOptions>
    options_span; // TODO: @badumbatish @jjasmine I have no idea what span do, i
		  // copied it out of rustc_builtin_macros/src/asm.rs
};

tl::optional<AsmArg>
parseAsmArg (Parser<MacroInvocLexer> &p, bool is_global_asm);
static tl::optional<AST::Fragment>
parse_global_asm (location_t invoc_locus, AST::MacroInvocData &invoc);
static tl::optional<AST::Fragment>
parse_nonglobal_asm (location_t invoc_locus, AST::MacroInvocData &invoc);
static tl::optional<AST::Fragment>
parse_asm (location_t invoc_locus, AST::MacroInvocData &invoc,
	   bool is_global_asm);

tl::optional<InlineAsmDirSpec>
parseDirSpec (Parser<MacroInvocLexer> &p, TokenId last_token_id)
{
  return tl::nullopt;
}
tl::optional<AsmArg>
parseAsmArg (Parser<MacroInvocLexer> &p, bool is_global_asm)
{
  return tl::nullopt;
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

  // operands stream
  token = parser.peek_current_token ();
  while (token->get_id () != last_token_id)
    {
      std::cout << token->get_token_description () << std::endl;

      token = parser.peek_current_token ();
      if (token->get_id () != COMMA)
	{
	  break;
	}
      parser.skip_token ();
      // Ok after the left paren is good, we better be parsing correctly
      // everything in here, which is operand in ABNF

      // TODO: Work on parsing operands
      fm_string = parse_format_string (parser, last_token_id);
    }

  // TODO: Handle the optional ","

  return tl::nullopt;
}

} // namespace Rust
