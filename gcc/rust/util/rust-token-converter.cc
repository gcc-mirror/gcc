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
#include "bi-map.h"
#include "line-map.h"

#include <string>

namespace Rust {

static const BiMap<PrimitiveCoreType, std::string> suffixes
  = {{{CORETYPE_F32, "f32"},
      {CORETYPE_F64, "f64"},
      {CORETYPE_U8, "u8"},
      {CORETYPE_U16, "u16"},
      {CORETYPE_U32, "u32"},
      {CORETYPE_U64, "u64"},
      {CORETYPE_U128, "u128"},
      {CORETYPE_I8, "i8"},
      {CORETYPE_I16, "i16"},
      {CORETYPE_I32, "i32"},
      {CORETYPE_I64, "i64"},
      {CORETYPE_I128, "i128"},
      {CORETYPE_ISIZE, "isize"},
      {CORETYPE_USIZE, "usize"}}};

static void
pop_group (std::vector<ProcMacro::TokenStream> &streams,
	   ProcMacro::Delimiter delim)
{
  auto g = ProcMacro::Group::make_group (streams.back (), delim);
  streams.pop_back ();
  auto tt = ProcMacro::TokenTree::make_tokentree (g);

  streams.back ().push (tt);
}

static ProcMacro::Span
convert (location_t location)
{
  return ProcMacro::Span::make_span (location, 0);
}

static location_t
convert (ProcMacro::Span span)
{
  return span.start;
}

static ProcMacro::Literal
handle_suffix (const const_TokenPtr &token, ProcMacro::LitKind kind)
{
  auto str = token->as_string ();
  auto lookup = suffixes.lookup (token->get_type_hint ());
  auto suffix = suffixes.is_iter_ok (lookup) ? lookup->second : "";
  return ProcMacro::Literal::make_literal (kind, convert (token->get_locus ()),
					   str, suffix);
}

ProcMacro::Literal
convert_literal (const_TokenPtr lit)
{
  auto loc = convert (lit->get_locus ());
  switch (lit->get_id ())
    {
    case FLOAT_LITERAL:
      return handle_suffix (lit, ProcMacro::LitKind::make_float ());
    case INT_LITERAL:
      return handle_suffix (lit, ProcMacro::LitKind::make_integer ());
    case CHAR_LITERAL:
      return ProcMacro::Literal::make_literal (ProcMacro::LitKind::make_char (),
					       loc, lit->as_string ());
    case STRING_LITERAL:
      return ProcMacro::Literal::make_literal (ProcMacro::LitKind::make_str (),
					       loc, lit->as_string ());
    case BYTE_CHAR_LITERAL:
      return ProcMacro::Literal::make_literal (ProcMacro::LitKind::make_byte (),
					       loc, lit->as_string ());
    case BYTE_STRING_LITERAL:
      return ProcMacro::Literal::make_literal (
	ProcMacro::LitKind::make_byte_str (), loc, lit->as_string ());
    default:
      rust_unreachable ();
    }
}

ProcMacro::TokenStream
convert (const std::vector<const_TokenPtr> &tokens)
{
  std::vector<ProcMacro::TokenStream> trees;
  trees.push_back (ProcMacro::TokenStream::make_tokenstream ());
  for (auto &token : tokens)
    {
      auto loc = convert (token->get_locus ());
      switch (token->get_id ())
	{
	// Literals
	case FLOAT_LITERAL:
	case INT_LITERAL:
	case CHAR_LITERAL:
	case STRING_LITERAL:
	case BYTE_CHAR_LITERAL:
	case BYTE_STRING_LITERAL:
	  trees.back ().push (
	    ProcMacro::TokenTree::make_tokentree (convert_literal (token)));
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
	case ENUM_KW:
	case EXTERN_KW:
	case FINAL_KW:
	case FN_KW:
	case FOR:
	case IF:
	case IMPL:
	case IN:
	case LET:
	case LOOP:
	case MACRO:
	case MATCH_KW:
	case MOD:
	case MOVE:
	case MUT:
	case OVERRIDE_KW:
	case PRIV:
	case PUB:
	case REF:
	case RETURN_KW:
	case SELF_ALIAS:
	case SELF:
	case STATIC_KW:
	case STRUCT_KW:
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
	    ProcMacro::Ident::make_ident (token->as_string (), loc)));
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
		ProcMacro::Punct::make_punct (*it, loc, ProcMacro::JOINT)));
	    trees.back ().push (ProcMacro::TokenTree::make_tokentree (
	      ProcMacro::Punct::make_punct (*it, loc, ProcMacro::ALONE)));
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
	    ProcMacro::Punct::make_punct (token->as_string ()[0], loc,
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
	  rust_unreachable ();
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
  std::string value (ident.value.to_string ());
  if (ident.is_raw)
    value = "r#" + value;

  Lexer lexer (value, nullptr);
  auto token = lexer.build_token ();
  token->set_locus (convert (ident.span));
  result.push_back (token);
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
  auto lookup = suffixes.lookup (literal.suffix.to_string ());
  auto loc = convert (literal.span);
  auto suffix
    = suffixes.is_iter_ok (lookup) ? lookup->second : CORETYPE_UNKNOWN;
  // FIXME: Add spans instead of empty locations
  switch (literal.kind.tag)
    {
    case ProcMacro::BYTE:
      result.push_back (
	Token::make_byte_char (loc, literal.text.to_string ()[0]));
      break;
    case ProcMacro::CHAR:
      result.push_back (Token::make_char (loc, literal.text.to_string ()[0]));
      break;
    case ProcMacro::INTEGER:
      result.push_back (
	Token::make_int (loc, literal.text.to_string (), suffix));
      break;
    case ProcMacro::FLOAT:
      result.push_back (
	Token::make_float (loc, literal.text.to_string (), suffix));
      break;
    case ProcMacro::STR:
      result.push_back (Token::make_string (loc, literal.text.to_string ()));
      break;
    case ProcMacro::BYTE_STR:
      result.push_back (
	Token::make_byte_string (loc, literal.text.to_string ()));
      break;
    // FIXME: Handle raw string
    case ProcMacro::STR_RAW:
    case ProcMacro::BYTE_STR_RAW:
    default:
      rust_unreachable ();
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
      auto lexer = Lexer (whole, nullptr);
      auto token = lexer.build_token ();
      token->set_locus (convert (punct.span));
      result.push_back (token);
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
  auto loc = convert (g.span);
  switch (g.delimiter)
    {
    case ProcMacro::PARENTHESIS:
      result.push_back (Token::make (LEFT_PAREN, loc));
      from_tokenstream (g.stream, result);
      result.push_back (Token::make (RIGHT_PAREN, loc));
      break;
    case ProcMacro::BRACE:
      result.push_back (Token::make (LEFT_CURLY, loc));
      from_tokenstream (g.stream, result);
      result.push_back (Token::make (RIGHT_CURLY, loc));
      break;
    case ProcMacro::BRACKET:
      result.push_back (Token::make (LEFT_SQUARE, loc));
      from_tokenstream (g.stream, result);
      result.push_back (Token::make (RIGHT_SQUARE, loc));
      break;
    case ProcMacro::NONE:
      from_tokenstream (g.stream, result);
      break;
    default:
      rust_unreachable ();
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
      rust_unreachable ();
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
