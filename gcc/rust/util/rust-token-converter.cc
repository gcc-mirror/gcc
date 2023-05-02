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

#include "rust-token-converter.h"
#include "libproc_macro/proc_macro.h"

namespace Rust {

static void
pop_group (std::vector<ProcMacro::TokenStream> &streams,
	   ProcMacro::Delimiter delim)
{
  auto g = ProcMacro::Group::make_group (streams.back (), delim);
  streams.pop_back ();
  auto tt = ProcMacro::TokenTree::make_tokentree (g);

  streams.back ().push (tt);
}

static void
dispatch_float_literals (ProcMacro::TokenStream &ts, TokenPtr &token)
{
  std::string::size_type sz;
  auto str = token->as_string ();
  switch (token->get_type_hint ())
    {
      case CORETYPE_F32: {
	auto value = std::stof (str, &sz);
	ts.push (ProcMacro::TokenTree::make_tokentree (
	  ProcMacro::Literal::make_f32 (value, sz == str.length ())));
      }
      break;
      case CORETYPE_F64: {
	auto value = std::stod (str, &sz);
	ts.push (ProcMacro::TokenTree::make_tokentree (
	  ProcMacro::Literal::make_f64 (value, sz == str.length ())));
      }
      break;
    default:
      gcc_unreachable ();
    }
}

static void
dispatch_integer_literals (ProcMacro::TokenStream &ts, TokenPtr &token)
{
  std::string::size_type sz;
  auto str = token->as_string ();
  unsigned long long uvalue;
  long long svalue;

  switch (token->get_type_hint ())
    {
    case CORETYPE_U8:
      uvalue = std::stoull (str, &sz);
      ts.push (ProcMacro::TokenTree::make_tokentree (
	ProcMacro::Literal::make_u8 (uvalue, sz == str.length ())));
      break;
    case CORETYPE_U16:
      uvalue = std::stoull (str, &sz);
      ts.push (ProcMacro::TokenTree::make_tokentree (
	ProcMacro::Literal::make_u16 (uvalue, sz == str.length ())));
      break;
    case CORETYPE_U32:
      uvalue = std::stoull (str, &sz);
      ts.push (ProcMacro::TokenTree::make_tokentree (
	ProcMacro::Literal::make_u32 (uvalue, sz == str.length ())));
      break;
    case CORETYPE_U64:
      uvalue = std::stoull (str, &sz);
      ts.push (ProcMacro::TokenTree::make_tokentree (
	ProcMacro::Literal::make_u32 (uvalue, sz == str.length ())));
      break;
    case CORETYPE_I8:
      svalue = std::stoll (str, &sz);
      ts.push (ProcMacro::TokenTree::make_tokentree (
	ProcMacro::Literal::make_i8 (svalue, sz == str.length ())));
      break;
    case CORETYPE_I16:
      svalue = std::stoll (str, &sz);
      ts.push (ProcMacro::TokenTree::make_tokentree (
	ProcMacro::Literal::make_i16 (svalue, sz == str.length ())));
      break;
    case CORETYPE_I32:
      svalue = std::stoll (str, &sz);
      ts.push (ProcMacro::TokenTree::make_tokentree (
	ProcMacro::Literal::make_i32 (svalue, sz == str.length ())));
      break;
    case CORETYPE_I64:
      svalue = std::stoll (str, &sz);
      ts.push (ProcMacro::TokenTree::make_tokentree (
	ProcMacro::Literal::make_i32 (svalue, sz == str.length ())));
      break;
    case CORETYPE_INT:
      svalue = std::stoll (str, &sz);
      ts.push (ProcMacro::TokenTree::make_tokentree (
	ProcMacro::Literal::make_isize (svalue, sz == str.length ())));
      break;
    case CORETYPE_UINT:
      uvalue = std::stoull (str, &sz);
      ts.push (ProcMacro::TokenTree::make_tokentree (
	ProcMacro::Literal::make_usize (uvalue, sz == str.length ())));
      break;
    case CORETYPE_UNKNOWN:
    default:
      gcc_unreachable ();
      break;
    }
}

ProcMacro::TokenStream
to_tokenstream (std::vector<TokenPtr> tokens)
{
  std::vector<ProcMacro::TokenStream> trees;
  trees.push_back (ProcMacro::TokenStream::make_tokenstream ());
  for (auto &token : tokens)
    {
      switch (token->get_id ())
	{
	// Literals
	case FLOAT_LITERAL:
	  dispatch_float_literals (trees.back (), token);
	  break;
	case INT_LITERAL:
	  dispatch_integer_literals (trees.back (), token);
	  break;
	// FIXME: Why does BYTE_CHAR_LITERAL is not handled by rustc ?
	case CHAR_LITERAL: // TODO: UTF-8 handling
	  trees.back ().push (ProcMacro::TokenTree::make_tokentree (
	    ProcMacro::Literal::make_char (token->as_string ()[0])));
	  break;
	case STRING_LITERAL:
	  trees.back ().push (ProcMacro::TokenTree::make_tokentree (
	    ProcMacro::Literal::make_string (token->as_string ())));
	  break;
	  case BYTE_STRING_LITERAL: {
	    auto str = token->as_string ();
	    std::vector<uint8_t> data (str.begin (), str.end ());
	    trees.back ().push (ProcMacro::TokenTree::make_tokentree (
	      ProcMacro::Literal::make_byte_string (data)));
	  }
	  break;
	// Ident
	case IDENTIFIER:
	case ABSTRACT:
	case AS:
	case ASYNC:
	case AUTO:
	case BECOME:
	case BOX:
	case BREAK:
	case CONST:
	case CONTINUE:
	case CRATE:
	case DO:
	case DYN:
	case ELSE:
	case ENUM_TOK:
	case EXTERN_TOK:
	case FINAL_TOK:
	case FN_TOK:
	case FOR:
	case IF:
	case IMPL:
	case IN:
	case LET:
	case LOOP:
	case MACRO:
	case MATCH_TOK:
	case MOD:
	case MOVE:
	case MUT:
	case OVERRIDE_TOK:
	case PRIV:
	case PUB:
	case REF:
	case RETURN_TOK:
	case SELF_ALIAS:
	case SELF:
	case STATIC_TOK:
	case STRUCT_TOK:
	case SUPER:
	case TRAIT:
	case TRY:
	case TYPE:
	case TYPEOF:
	case UNSAFE:
	case UNSIZED:
	case USE:
	case VIRTUAL:
	case WHERE:
	case WHILE:
	case YIELD:
	// Underscore is not a Punct, considered as an Ident
	case UNDERSCORE:
	// True and false are idents, not literals
	// (https://doc.rust-lang.org/proc_macro/struct.Literal.html)
	case FALSE_LITERAL:
	case TRUE_LITERAL:
	  trees.back ().push (ProcMacro::TokenTree::make_tokentree (
	    ProcMacro::Ident::make_ident (token->as_string ())));
	  break;
	// Joint punct
	case OR:
	case PIPE_EQ:
	case CARET_EQ:
	case RIGHT_SHIFT_EQ:
	case RIGHT_SHIFT:
	case GREATER_OR_EQUAL:
	case MATCH_ARROW:
	case LESS_OR_EQUAL:
	case LEFT_SHIFT_EQ:
	case LEFT_SHIFT:
	case DIV_EQ:
	case ELLIPSIS:
	case DOT_DOT_EQ:
	case DOT_DOT:
	case RETURN_TYPE:
	case MINUS_EQ:
	case PLUS_EQ:
	case ASTERISK_EQ:
	case LOGICAL_AND:
	case AMP_EQ:
	case PERCENT_EQ:
	case SCOPE_RESOLUTION:
	case NOT_EQUAL:
	  case EQUAL_EQUAL: {
	    auto str = token->as_string ();
	    auto it = str.cbegin ();
	    for (; it != str.cend () - 1; it++)
	      {
		trees.back ().push (ProcMacro::TokenTree::make_tokentree (
		  ProcMacro::Punct::make_punct (*it, ProcMacro::JOINT)));
	      }
	    trees.back ().push (ProcMacro::TokenTree::make_tokentree (
	      ProcMacro::Punct::make_punct (*it, ProcMacro::ALONE)));
	  }
	  break;
	// Alone punct tokens
	case EQUAL:
	case RIGHT_ANGLE:
	case LEFT_ANGLE:
	case EXCLAM:
	case TILDE:
	case PLUS:
	case MINUS:
	case ASTERISK:
	case DIV:
	case PERCENT:
	case CARET:
	case AMP:
	case PIPE:
	case PATTERN_BIND:
	case DOT:
	case COMMA:
	case SEMICOLON:
	case COLON:
	case HASH:
	case DOLLAR_SIGN:
	case QUESTION_MARK:
	case SINGLE_QUOTE:
	  trees.back ().push (ProcMacro::TokenTree::make_tokentree (
	    ProcMacro::Punct::make_punct (token->as_string ()[0],
					  ProcMacro::ALONE)));
	  break;
	case RIGHT_PAREN:
	  pop_group (trees, ProcMacro::PARENTHESIS);
	  break;
	case RIGHT_CURLY:
	  pop_group (trees, ProcMacro::BRACE);
	  break;
	case RIGHT_SQUARE:
	  pop_group (trees, ProcMacro::BRACKET);
	  break;
	case LEFT_SQUARE:
	case LEFT_CURLY:
	case LEFT_PAREN:
	  trees.push_back (ProcMacro::TokenStream::make_tokenstream ());
	  break;
	default:
	  gcc_unreachable ();
	}
    }
  return trees.back ();
}

} // namespace Rust
