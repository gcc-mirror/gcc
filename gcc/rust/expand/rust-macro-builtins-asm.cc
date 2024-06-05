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
std::map<AST::InlineAsmOption, std::string> InlineAsmOptionMap{
  {AST::InlineAsmOption::PURE, "pure"},
  {AST::InlineAsmOption::NOMEM, "nomem"},
  {AST::InlineAsmOption::READONLY, "readonly"},
  {AST::InlineAsmOption::PRESERVES_FLAGS, "preserves_flags"},
  {AST::InlineAsmOption::NORETURN, "noreturn"},
  {AST::InlineAsmOption::NOSTACK, "nostack"},
  {AST::InlineAsmOption::MAY_UNWIND, "may_unwind"},
  {AST::InlineAsmOption::ATT_SYNTAX, "att_syntax"},
  {AST::InlineAsmOption::RAW, "raw"},
};

int
parseDirSpec (Parser<MacroInvocLexer> &parser, TokenId last_token_id)
{
  return 0;
}

int
parse_clobber_abi (Parser<MacroInvocLexer> &parser, TokenId last_token_id,
		   InlineAsmContext &inline_asm_ctx)
{
  // clobber_abi := "clobber_abi(" <abi> *("," <abi>) [","] ")"
  // PARSE EVERYTHING COMMITTEDLY IN THIS FUNCTION, WE CONFIRMED VIA clobber_abi
  // identifier keyword
  auto &inline_asm = inline_asm_ctx.inline_asm;
  auto token = parser.peek_current_token ();
  if (!parser.skip_token (LEFT_PAREN))
    {
      // TODO: Raise error exactly like rustc if left parenthesis is not
      // encountered.
      token = parser.peek_current_token ();

      // TODO: Error reporting shifted to the left 1 character, I'm not sure
      // why.
      if (token->get_id () == last_token_id)
	{
	  rust_error_at (token->get_locus (),
			 "expected %<(%>, found end of macro arguments");
	  return -1;
	}
      else
	{
	  rust_error_at (token->get_locus (), "expected %<(%>, found %qs",
			 token->get_token_description ());
	}
      return -1;
    }

  if (parser.skip_token (RIGHT_PAREN))
    {
      // TODO: We encountered a "clobber_abi()", which should be illegal?
      // https://github.com/rust-lang/rust/blob/c00957a3e269219413041a4e3565f33b1f9d0779/compiler/rustc_builtin_macros/src/asm.rs#L381
      rust_error_at (
	parser.peek_current_token ()->get_locus (),
	"at least one abi must be provided as an argument to %<clobber_abi%>");
      return -1;
    }

  std::vector<AST::TupleClobber> new_abis;

  token = parser.peek_current_token ();

  while (token->get_id () != last_token_id && token->get_id () != RIGHT_PAREN)
    {
      // Check if it is a string literal or not, codename: <ABI> in ABNF
      if (token->get_id () == STRING_LITERAL)
	{
	  // TODO: Caring for span in here.
	  new_abis.push_back ({token->as_string (), token->get_locus ()});
	}
      else
	{
	  // TODO: We encountered something that is not string literal, which
	  // should be illegal, please emit the correct error
	  // https://github.com/rust-lang/rust/blob/b92758a9aef1cef7b79e2b72c3d8ba113e547f89/compiler/rustc_builtin_macros/src/asm.rs#L387
	  rust_unreachable ();
	}

      if (parser.skip_token (RIGHT_PAREN))
	{
	  break;
	}

      if (!parser.skip_token (COMMA))
	{
	  // TODO: If the skip of comma is unsuccessful, which should be
	  // illegal, pleaes emit the correct error.
	  rust_unreachable ();
	  return -1;
	}

      token = parser.peek_current_token ();
    }

  // Done processing the local clobber abis, push that to the main Args in
  // argument

  for (auto abi : new_abis)
    {
      inline_asm.clobber_abi.push_back (abi);
    }

  return 0;
}

tl::optional<AST::InlineAsmRegOrRegClass>
parse_reg (Parser<MacroInvocLexer> &parser, TokenId last_token_id,
	   InlineAsmContext &inline_asm_ctx)
{
  using RegType = AST::InlineAsmRegOrRegClass::Type;
  if (!parser.skip_token (LEFT_PAREN))
    {
      // TODO: we expect a left parenthesis here, please return the correct
      // error.
      rust_unreachable ();
      return tl::nullopt;
    }

  // after successful left parenthesis parsing, we should return ast of
  // InlineAsmRegOrRegClass of reg or reg class
  auto token = parser.peek_current_token ();
  auto tok_id = token->get_id ();
  AST::InlineAsmRegOrRegClass reg_class;
  if (parser.skip_token (IDENTIFIER))
    {
      // construct a InlineAsmRegOrRegClass
      reg_class.type = RegType::RegClass;
      reg_class.reg_class.Symbol = token->as_string ();
    }
  else if (tok_id == STRING_LITERAL)
    {
      // TODO: there is STRING_LITERAL, and BYTE_STRING_LITERAL, should we check
      // for both?

      // construct a InlineAsmRegOrRegClass
      // parse_format_string
      reg_class.type = RegType::Reg;
      inline_asm_ctx.is_explicit = true;
      reg_class.reg_class.Symbol = token->as_string ();
    }
  else
    {
      // TODO: This should emit error
      //  return
      //  Err(p.dcx().create_err(errors::ExpectedRegisterClassOrExplicitRegister
      //  {
      //           span: p.token.span,
      //       }));
      rust_unreachable ();
    }
  if (!parser.skip_token (RIGHT_PAREN))
    {
      // TODO: we expect a left parenthesis here, please return the correct
      // error.
      rust_unreachable ();
      return tl::nullopt;
    }

  return reg_class;
}

int
parse_operand (Parser<MacroInvocLexer> &parser, TokenId last_token_id,
	       InlineAsmContext &inline_asm_ctx)
{
  return 0;
}

// From rustc
tl::optional<AST::InlineAsmOperand>
parse_reg_operand (Parser<MacroInvocLexer> &parser, TokenId last_token_id,
		   InlineAsmContext &inline_asm_ctx)
{
  // let name = if p.token.is_ident() && p.look_ahead(1, |t| *t == token::Eq) {
  //           let (ident, _) = p.token.ident().unwrap();
  //           p.bump();
  //           p.expect(&token::Eq)?;
  //           allow_templates = false;
  //           Some(ident.name)
  //       } else {
  //           None
  //       };

  AST::InlineAsmOperand reg_operand;
  rust_debug("Enter parse_reg_operand");
  auto token = parser.peek_current_token ();
  auto iden_token = parser.peek_current_token ();
  auto &inline_asm = inline_asm_ctx.inline_asm;
  if (check_identifier (parser, ""))
    {
    
      rust_debug("Didn't get passed identifier checking, %s", token->as_string().c_str());

      auto equal_token = parser.peek_current_token ();
      if (!parser.skip_token (EQUAL))
	{
	  // TODO: Please handle this.
	  rust_unreachable ();
	}
    }

  token = parser.peek_current_token ();
  rust_debug_loc(token->get_locus(), "Got pass identifier checking with %s",  token->as_string().c_str());


  bool is_global_asm = inline_asm.is_global_asm;

  // For the keyword IN, currently we count it as a seperate keyword called Rust::IN
  // search for #define RS_TOKEN_LIST in code base.
  if (!is_global_asm && parser.skip_token(IN))
    {
      rust_debug("Enter parse_reg_operand in");

      auto reg = parse_reg(parser, last_token_id, inline_asm_ctx);

      if (parser.skip_token(UNDERSCORE)) {
        // We are sure to be failing a test here, based on asm.rs 
        // https://github.com/rust-lang/rust/blob/a330e49593ee890f9197727a3a558b6e6b37f843/compiler/rustc_builtin_macros/src/asm.rs#L112
        rust_unreachable();
      }

      auto expr = parse_format_string(parser, last_token_id, inline_asm_ctx) ;
      reg_operand.register_type = AST::InlineAsmOperand::RegisterType::In;
      
      // Since reg is of type optional<T>, we need to check if it is not optional first.
      // TODO: We don't throw any errors since we should have throw any encountered parsing error in parse_reg
      if (reg) {
        reg_operand.in.reg = reg.value();
      }
      
      // Only clone_expr() if we know that we have parse an expression successfully
      // if (expr) {
      //   reg_operand.in.expr = expr->clone_expr();
      // }

      return reg_operand;
    }
  else if (!is_global_asm && check_identifier (parser, "out"))
    {
      rust_unreachable ();
      return tl::nullopt;
    }
  else if (!is_global_asm && check_identifier (parser, "lateout"))
    {
      rust_unreachable ();
      return tl::nullopt;
    }
  else if (!is_global_asm && check_identifier (parser, "inout"))
    {
      rust_debug("Enter parse_reg_operand inout");
      
      auto reg = parse_reg(parser, last_token_id, inline_asm_ctx);

      if (parser.skip_token(UNDERSCORE)) {
        // We are sure to be failing a test here, based on asm.rs 
        // https://github.com/rust-lang/rust/blob/a330e49593ee890f9197727a3a558b6e6b37f843/compiler/rustc_builtin_macros/src/asm.rs#L112
        rust_unreachable();
      }

      // TODO: Is error propogation our top priority, the ? in rust's asm.rs is doing a lot of work.
      // TODO: Not sure how to use parse_expr
      auto expr = parse_format_string(parser, last_token_id, inline_asm_ctx) ;

      std::unique_ptr<AST::Expr> out_expr;

      if (parser.skip_token(MATCH_ARROW)) {
        rust_debug("Matched MATCH_ARROW");
        if (!parser.skip_token(UNDERSCORE)) {
          
          parse_format_string(parser, last_token_id, inline_asm_ctx) ;
          //out_expr = parser.parse_expr();
        }

        reg_operand.register_type = AST::InlineAsmOperand::RegisterType::SplitInOut;
        // reg_operand.split_in_out.in_expr = expr->clone_expr();
        // reg_operand.split_in_out.out_expr = out_expr->clone_expr();
        // reg_operand.split_in_out.late = false;
        return reg_operand;

      } else {
        reg_operand.register_type = AST::InlineAsmOperand::RegisterType::InOut;
        // reg_operand.in_out.expr = expr->clone_expr();
        // reg_operand.in_out.late = false;
        return reg_operand;
      }
    //  if p.eat(&token::FatArrow) {
    //             let out_expr =
    //                 if p.eat_keyword(kw::Underscore) { None } else { Some(p.parse_expr()?) };
    //             ast::InlineAsmOperand::SplitInOut { reg, in_expr: expr, out_expr, late: false }
    //         } else {
    //             ast::InlineAsmOperand::InOut { reg, expr, late: false }
    //         }
      return tl::nullopt;

    }
  else if (!is_global_asm && check_identifier (parser, "inlateout"))
    {
      rust_unreachable ();
      return tl::nullopt;
    }
  else if (parser.peek_current_token ()->get_id () == CONST)
    {
      // TODO: Please handle const
      rust_unreachable ();
      return tl::nullopt;
    }
  else if (false && check_identifier (parser, "sym"))
    {
      // TODO: Please handle sym
      rust_unreachable ();
      return tl::nullopt;
    }
  else if (false && check_identifier (parser, "label"))
    {
      // TODO: Please handle label
      rust_unreachable ();
      return tl::nullopt;
    }
  else
    {
      // TODO: It is  weird that we can't seem to match any identifier,
      // something must be wrong. consult compiler code in asm.rs or rust online
      // compiler.
      rust_unreachable ();
      return tl::nullopt;
    }
  return reg_operand;
}
void
check_and_set (Parser<MacroInvocLexer> &parser,
	       InlineAsmContext &inline_asm_ctx, AST::InlineAsmOption option)
{
  auto &inline_asm = inline_asm_ctx.inline_asm;
  if (inline_asm.options.count (option) != 0)
    {
      // TODO: report an error of duplication
      rust_error_at (parser.peek_current_token ()->get_locus (),
		     "the %qs option was already provided",
		     InlineAsmOptionMap[option].c_str ());
      return;
    }
  else
    {
      inline_asm.options.insert (option);
    }
}
int
parse_options (Parser<MacroInvocLexer> &parser, TokenId last_token_id,
	       InlineAsmContext &inline_asm_ctx)
{
  bool is_global_asm = inline_asm_ctx.inline_asm.is_global_asm;
  // Parse everything commitedly
  if (!parser.skip_token (LEFT_PAREN))
    {
      // We have shifted `options` to search for the left parenthesis next, we
      // should error out if this is not possible.
      // TODO: report some error.
      return -1;
    }

  auto token = parser.peek_current_token ();
  while (token->get_id () != last_token_id && token->get_id () != RIGHT_PAREN)
    {
      if (!is_global_asm && check_identifier (parser, "pure"))
	{
	  check_and_set (parser, inline_asm_ctx, AST::InlineAsmOption::PURE);
	}
      else if (!is_global_asm && check_identifier (parser, "nomem"))
	{
	  check_and_set (parser, inline_asm_ctx, AST::InlineAsmOption::NOMEM);
	}
      else if (!is_global_asm && check_identifier (parser, "readonly"))
	{
	  check_and_set (parser, inline_asm_ctx,
			 AST::InlineAsmOption::READONLY);
	}
      else if (!is_global_asm && check_identifier (parser, "preserves_flags"))
	{
	  check_and_set (parser, inline_asm_ctx,
			 AST::InlineAsmOption::PRESERVES_FLAGS);
	}
      else if (!is_global_asm && check_identifier (parser, "noreturn"))
	{
	  check_and_set (parser, inline_asm_ctx,
			 AST::InlineAsmOption::NORETURN);
	}
      else if (!is_global_asm && check_identifier (parser, "nostack"))
	{
	  check_and_set (parser, inline_asm_ctx, AST::InlineAsmOption::NOSTACK);
	}
      else if (!is_global_asm && check_identifier (parser, "may_unwind"))
	{
	  check_and_set (parser, inline_asm_ctx,
			 AST::InlineAsmOption::MAY_UNWIND);
	}
      else if (check_identifier (parser, "att_syntax"))
	{
	  check_and_set (parser, inline_asm_ctx,
			 AST::InlineAsmOption::ATT_SYNTAX);
	}
      else if (check_identifier (parser, "raw"))
	{
	  check_and_set (parser, inline_asm_ctx, AST::InlineAsmOption::RAW);
	}
      else
	{
	  // TODO: Unexpected error, please return the correct error
	  rust_error_at (token->get_locus (),
			 "expected one of %qs, %qs, %qs, %qs, %qs, %qs, %qs, "
			 "%qs, %qs, or %qs, found %qs",
			 ")", "att_syntax", "may_unwind", "nomem", "noreturn",
			 "nostack", "preserves_flags", "pure", "raw",
			 "readonly", token->as_string ().c_str ());
	  return -1;
	}
      if (parser.skip_token (RIGHT_PAREN))
	{
	  break;
	}

      // Parse comma as optional
      if (parser.skip_token (COMMA))
	{
	  continue;
	}
      else
	{
	  rust_unreachable ();
	  token = parser.peek_current_token ();
	  return -1;
	}
    }

  // TODO: Per rust asm.rs regarding options_spans
  // I'm guessing this has to do with some error reporting.
  // let new_span = span_start.to(p.prev_token.span);
  // args.options_spans.push(new_span);

  return 0;
}
bool
check_identifier (Parser<MacroInvocLexer> &p, std::string ident)
{
  auto token = p.peek_current_token ();
  
  if (token->get_id () == IDENTIFIER) {
    auto str = token->as_string();

    // For non-promoted keywords, we need to also check for them.

    if (str == ident) {
      p.skip_token ();
      return true;
    }
    if (ident == "") {
      if (str != "in" && str != "out" && str != "lateout" && str != "inout" && str != "inlateout" && str != "const" && str != "sym" && str != "label")
      {
        p.skip_token ();
        return true;
      }
      return false;
    }
  }

  return false;

}

tl::optional<std::string>
parse_format_string (Parser<MacroInvocLexer> &parser, TokenId last_token_id,
		     InlineAsmContext &inline_asm_ctx)
{
  auto token = parser.peek_current_token ();

  if (token->get_id () != last_token_id && token->get_id () == STRING_LITERAL)
    {
      // very nice, we got a supposedly formatted string.
      parser.skip_token ();
      return token->as_string ();
    }
  else
    {
      return tl::nullopt;
    }
}

tl::optional<AST::Fragment>
MacroBuiltin::asm_handler (location_t invoc_locus, AST::MacroInvocData &invoc,
			   bool is_global_asm)
{
  return parse_asm (invoc_locus, invoc, is_global_asm);
}

int
parse_asm_arg (Parser<MacroInvocLexer> &parser, TokenId last_token_id,
	       InlineAsmContext &inline_asm_ctx)
{
  auto token = parser.peek_current_token ();
  tl::optional<std::string> fm_string;
  while (token->get_id () != last_token_id)
    {
      token = parser.peek_current_token ();

      // We accept a comma token here.
      if (token->get_id () != COMMA
	  && inline_asm_ctx.consumed_comma_without_formatted_string)
	{
	  // if it is not a comma, but we consumed it previously, this is fine
	  // but we have to set it to false tho.
	  inline_asm_ctx.consumed_comma_without_formatted_string = false;
	}
      else if (token->get_id () == COMMA
	       && !inline_asm_ctx.consumed_comma_without_formatted_string)
	{
	  inline_asm_ctx.consumed_comma_without_formatted_string = false;
	  parser.skip_token ();
	}
      else
	{
	  // TODO: we consumed comma, and there happens to also be a comma
	  // error should be: expected expression, found `,`
	  break;
	}

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
	  parse_clobber_abi (parser, last_token_id, inline_asm_ctx);
	  continue;
	}

      // TODO: Parse options
      if (check_identifier (parser, "options"))
	{
	  parse_options (parser, last_token_id, inline_asm_ctx);
	  continue;
	}

      // Ok after we have check that neither clobber_abi nor options works, the
      // only other logical choice is reg_operand
      // std::cout << "reg_operand" << std::endl;
      auto operand = parse_reg_operand (parser, last_token_id, inline_asm_ctx);
    }
  return 0;
}

tl::optional<AST::Fragment>
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

  AST::InlineAsm inline_asm (invoc_locus, is_global_asm);
  auto inline_asm_ctx = InlineAsmContext (inline_asm);

  // Parse the first ever formatted string, success or not, will skip 1 token
  auto fm_string = parse_format_string (parser, last_token_id, inline_asm_ctx);

  if (fm_string == tl::nullopt)
    {
      rust_error_at (parser.peek_current_token ()->get_locus (),
		     "%s template must be a string literal", "asm");
      return tl::nullopt;
    }

  // formatted string stream

  while (parser.peek_current_token ()->get_id () != last_token_id)
    {
      if (!parser.skip_token (COMMA))
	{
	  break;
	}
      // Ok after the comma is good, we better be parsing correctly everything
      // in here, which is formatted string in ABNF
      inline_asm_ctx.consumed_comma_without_formatted_string = false;

      fm_string = parse_format_string (parser, last_token_id, inline_asm_ctx);
      if (fm_string == tl::nullopt)
	{
	  inline_asm_ctx.consumed_comma_without_formatted_string = true;
	  break;
	}
    }

  // operands stream, also handles the optional ","
  parse_asm_arg (parser, last_token_id, inline_asm_ctx);

  AST::SingleASTNode single = AST::SingleASTNode (
    inline_asm_ctx.inline_asm.clone_expr_without_block ());
  std::vector<AST::SingleASTNode> single_vec = {single};

  AST::Fragment fragment_ast
    = AST::Fragment (single_vec, std::vector<std::unique_ptr<AST::Token>> ());
  return fragment_ast;
}

} // namespace Rust
