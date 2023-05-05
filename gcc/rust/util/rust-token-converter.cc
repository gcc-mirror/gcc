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

#include "rust-lex.h"
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
dispatch_float_literals (ProcMacro::TokenStream &ts,
			 const const_TokenPtr &token)
{
  std::string::size_type sz;
  auto str = token->as_string ();
  switch (token->get_type_hint ())
    {
      case CORETYPE_F32: {
	auto value = std::stof (str, &sz);
	ts.push (ProcMacro::TokenTree::make_tokentree (
	  ProcMacro::Literal::make_f32 (value, sz != str.length ())));
      }
      break;
      case CORETYPE_F64: {
	auto value = std::stod (str, &sz);
	ts.push (ProcMacro::TokenTree::make_tokentree (
	  ProcMacro::Literal::make_f64 (value, sz != str.length ())));
      }
      break;
    default:
      gcc_unreachable ();
    }
}

static void
dispatch_integer_literals (ProcMacro::TokenStream &ts,
			   const const_TokenPtr &token)
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
	ProcMacro::Literal::make_u8 (uvalue, sz != str.length ())));
      break;
    case CORETYPE_U16:
      uvalue = std::stoull (str, &sz);
      ts.push (ProcMacro::TokenTree::make_tokentree (
	ProcMacro::Literal::make_u16 (uvalue, sz != str.length ())));
      break;
    case CORETYPE_U32:
      uvalue = std::stoull (str, &sz);
      ts.push (ProcMacro::TokenTree::make_tokentree (
	ProcMacro::Literal::make_u32 (uvalue, sz != str.length ())));
      break;
    case CORETYPE_U64:
      uvalue = std::stoull (str, &sz);
      ts.push (ProcMacro::TokenTree::make_tokentree (
	ProcMacro::Literal::make_u32 (uvalue, sz != str.length ())));
      break;
    case CORETYPE_I8:
      svalue = std::stoll (str, &sz);
      ts.push (ProcMacro::TokenTree::make_tokentree (
	ProcMacro::Literal::make_i8 (svalue, sz != str.length ())));
      break;
    case CORETYPE_I16:
      svalue = std::stoll (str, &sz);
      ts.push (ProcMacro::TokenTree::make_tokentree (
	ProcMacro::Literal::make_i16 (svalue, sz != str.length ())));
      break;
    case CORETYPE_I32:
      svalue = std::stoll (str, &sz);
      ts.push (ProcMacro::TokenTree::make_tokentree (
	ProcMacro::Literal::make_i32 (svalue, sz != str.length ())));
      break;
    case CORETYPE_I64:
      svalue = std::stoll (str, &sz);
      ts.push (ProcMacro::TokenTree::make_tokentree (
	ProcMacro::Literal::make_i32 (svalue, sz != str.length ())));
      break;
    case CORETYPE_INT:
      svalue = std::stoll (str, &sz);
      ts.push (ProcMacro::TokenTree::make_tokentree (
	ProcMacro::Literal::make_isize (svalue, sz != str.length ())));
      break;
    case CORETYPE_UINT:
      uvalue = std::stoull (str, &sz);
      ts.push (ProcMacro::TokenTree::make_tokentree (
	ProcMacro::Literal::make_usize (uvalue, sz != str.length ())));
      break;
    case CORETYPE_UNKNOWN:
    default:
      gcc_unreachable ();
      break;
    }
}

ProcMacro::TokenStream
convert (const std::vector<const_TokenPtr> &tokens)
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
	      trees.back ().push (ProcMacro::TokenTree::make_tokentree (
		ProcMacro::Punct::make_punct (*it, ProcMacro::JOINT)));
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

static void
from_tokenstream (const ProcMacro::TokenStream &ts,
		  std::vector<const_TokenPtr> &result);

/**
 * Append the token corresponding to a given Ident to a vector.
 *
 * @param literal Reference to the Ident to convert.
 * @param result Reference to the output vector.
 */
static void
from_ident (const ProcMacro::Ident &ident, std::vector<const_TokenPtr> &result)
{
  std::string value (reinterpret_cast<const char *> (ident.val), ident.len);
  if (ident.is_raw)
    value = "r#" + value;

  // TODO: Inject span -> for now spans are not stored in Ident, once changed
  // the span should be injected in the built token below.
  Lexer lexer (value);
  result.push_back (lexer.peek_token ());
}

static void
string_literal (const ProcMacro::StringPayload &payload,
		std::vector<const_TokenPtr> &result)
{
  // TODO: UTF-8 string
  result.push_back (Token::make_string (
    Location (),
    std::string (reinterpret_cast<const char *> (payload.data), payload.len)));
}

static void
byte_string_literal (const ProcMacro::ByteStringPayload &payload,
		     std::vector<const_TokenPtr> &result)
{
  result.push_back (Token::make_byte_string (
    Location (),
    std::string (reinterpret_cast<const char *> (payload.data), payload.size)));
}

static void
unsigned_literal (const ProcMacro::Unsigned &lit,
		  std::vector<const_TokenPtr> &result)
{
  switch (lit.tag)
    {
    case ProcMacro::UNSIGNED_8:
      result.push_back (Token::make_int (Location (),
					 std::to_string (lit.payload.unsigned8),
					 CORETYPE_U8));
      break;
    case ProcMacro::UNSIGNED_16:
      result.push_back (
	Token::make_int (Location (), std::to_string (lit.payload.unsigned16),
			 CORETYPE_U16));
      break;
    case ProcMacro::UNSIGNED_32:
      result.push_back (
	Token::make_int (Location (), std::to_string (lit.payload.unsigned32),
			 CORETYPE_U32));
      break;
    case ProcMacro::UNSIGNED_64:
      result.push_back (
	Token::make_int (Location (), std::to_string (lit.payload.unsigned64),
			 CORETYPE_U64));
      break;
    case ProcMacro::UNSIGNED_128:
      // TODO: Handle 128 bits
    default:
      gcc_unreachable ();
    }
}

static void
signed_literal (const ProcMacro::Signed &lit,
		std::vector<const_TokenPtr> &result)
{
  switch (lit.tag)
    {
    case ProcMacro::SIGNED_8:
      result.push_back (Token::make_int (Location (),
					 std::to_string (lit.payload.signed8),
					 CORETYPE_I8));
      break;
    case ProcMacro::SIGNED_16:
      result.push_back (Token::make_int (Location (),
					 std::to_string (lit.payload.signed16),
					 CORETYPE_I16));
      break;
    case ProcMacro::SIGNED_32:
      result.push_back (Token::make_int (Location (),
					 std::to_string (lit.payload.signed32),
					 CORETYPE_I32));
      break;
    case ProcMacro::SIGNED_64:
      result.push_back (Token::make_int (Location (),
					 std::to_string (lit.payload.signed64),
					 CORETYPE_I64));
      break;
    case ProcMacro::SIGNED_128:
      // TODO: Handle 128 bits
    default:
      gcc_unreachable ();
    }
}

/**
 * Append the token corresponding to a given Literal to a vector.
 *
 * @param literal Reference to the Literal to convert.
 * @param result Reference to the vector tokens should be appended to.
 */
static void
from_literal (const ProcMacro::Literal &literal,
	      std::vector<const_TokenPtr> &result)
{
  switch (literal.tag)
    {
    case ProcMacro::STRING:
      string_literal (literal.payload.string_payload, result);
      break;
    case ProcMacro::BYTE_STRING:
      byte_string_literal (literal.payload.byte_string_payload, result);
      break;
    case ProcMacro::CHAR:
      result.push_back (
	Token::make_char (Location (), literal.payload.char_payload));
      break;
    case ProcMacro::UNSIGNED:
      unsigned_literal (literal.payload.unsigned_payload.value, result);
      break;
    case ProcMacro::SIGNED:
      signed_literal (literal.payload.signed_payload.value, result);
      break;
    case ProcMacro::USIZE:
      result.push_back (
	Token::make_int (Location (),
			 std::to_string (literal.payload.usize_payload.value),
			 CORETYPE_USIZE));
      break;
    case ProcMacro::ISIZE:
      result.push_back (
	Token::make_int (Location (),
			 std::to_string (literal.payload.isize_payload.value),
			 CORETYPE_ISIZE));
      break;
    case ProcMacro::FLOAT32:
      result.push_back (Token::make_float (
	Location (), std::to_string (literal.payload.float32_payload.value),
	CORETYPE_F32));
      break;
    case ProcMacro::FLOAT64:
      result.push_back (Token::make_float (
	Location (), std::to_string (literal.payload.float64_payload.value),
	CORETYPE_F64));
      break;
    default:
      gcc_unreachable ();
    }
}

/**
 * Accumulate through successive calls multiple Punct until one is tagged
 * "Alone", then append the formed token to a given result vector.
 *
 * @param punct Reference to the Punct to convert.
 * @param acc Reference to an accumulator for joined Punct.
 * @param result Reference to the output token vector.
 */
static void
from_punct (const ProcMacro::Punct &punct, std::vector<std::uint32_t> &acc,
	    std::vector<const_TokenPtr> &result)
{
  acc.push_back (punct.ch);
  if (ProcMacro::ALONE == punct.spacing) /* Last punct of a chain */
    {
      // TODO: UTF-8 string
      std::string whole (acc.begin (), acc.end ());
      auto lexer = Lexer (whole);
      result.push_back (lexer.peek_token ());
      acc.clear ();
    }
}

/**
 * Iterate over a Group and append all inner tokens to a vector enclosed by its
 * delimiters.
 *
 * @param g Reference to the Group to convert.
 * @param result Reference to the vector tokens should be appended to.
 */
static void
from_group (const ProcMacro::Group &g, std::vector<const_TokenPtr> &result)
{
  switch (g.delimiter)
    {
    case ProcMacro::PARENTHESIS:
      result.push_back (Token::make (LEFT_PAREN, Location ()));
      from_tokenstream (g.stream, result);
      result.push_back (Token::make (RIGHT_PAREN, Location ()));
      break;
    case ProcMacro::BRACE:
      result.push_back (Token::make (LEFT_CURLY, Location ()));
      from_tokenstream (g.stream, result);
      result.push_back (Token::make (RIGHT_CURLY, Location ()));
      break;
    case ProcMacro::BRACKET:
      result.push_back (Token::make (LEFT_SQUARE, Location ()));
      from_tokenstream (g.stream, result);
      result.push_back (Token::make (RIGHT_SQUARE, Location ()));
      break;
    case ProcMacro::NONE:
      from_tokenstream (g.stream, result);
      break;
    default:
      gcc_unreachable ();
    }
}

/**
 * Dispatch TokenTree's conversion to its inner type depending on its tag.
 *
 * @param tt Reference to the TokenTree.
 * @param punct_accumulator Reference to an accumulator for joined Punct.
 * @param result Reference to the vector tokens should be appended to.
 */
static void
from_tokentree (const ProcMacro::TokenTree &tt,
		std::vector<std::uint32_t> &punct_accumulator,
		std::vector<const_TokenPtr> &result)
{
  switch (tt.tag)
    {
    case ProcMacro::GROUP:
      from_group (tt.payload.group, result);
      break;
    case ProcMacro::IDENT:
      from_ident (tt.payload.ident, result);
      break;
    case ProcMacro::PUNCT:
      from_punct (tt.payload.punct, punct_accumulator, result);
      break;
    case ProcMacro::LITERAL:
      from_literal (tt.payload.literal, result);
      break;
    default:
      gcc_unreachable ();
    }
}

/**
 * Iterate over a TokenStream and append all inner tokens to a vector.
 *
 * @param ts Reference to the TokenStream.
 * @param result Reference to the vector tokens should be appended to.
 */
static void
from_tokenstream (const ProcMacro::TokenStream &ts,
		  std::vector<const_TokenPtr> &result)
{
  std::vector<std::uint32_t> punct_accumulator;
  for (std::uint64_t i = 0; i < ts.size; i++)
    {
      from_tokentree (ts.data[i], punct_accumulator, result);
    }
}

std::vector<const_TokenPtr>
convert (const ProcMacro::TokenStream &ts)
{
  std::vector<const_TokenPtr> result;
  from_tokenstream (ts, result);
  return result;
}

} // namespace Rust
