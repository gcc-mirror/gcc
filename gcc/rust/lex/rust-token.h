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

#ifndef RUST_TOKEN_H
#define RUST_TOKEN_H

#include "rust-system.h"
#include "rust-linemap.h"
#include "rust-codepoint.h"

namespace Rust {
// "Primitive core types" in Rust - the different int and float types, as well
// as some others
enum PrimitiveCoreType
{
  CORETYPE_UNKNOWN,
  // named primitives
  CORETYPE_BOOL,
  CORETYPE_CHAR,
  CORETYPE_STR,
  // okay technically int and uint are arch-dependent (pointer size)
  CORETYPE_INT,
  CORETYPE_UINT,
  // numbered number primitives
  CORETYPE_F32,
  CORETYPE_F64,
  CORETYPE_I8,
  CORETYPE_I16,
  CORETYPE_I32,
  CORETYPE_I64,
  CORETYPE_I128,
  CORETYPE_U8,
  CORETYPE_U16,
  CORETYPE_U32,
  CORETYPE_U64,
  CORETYPE_U128,
  // Pure decimals are used for tuple index.
  // Also means there is no type hint.
  CORETYPE_PURE_DECIMAL,
  // arch-dependent pointer sizes
  CORETYPE_ISIZE = CORETYPE_INT,
  CORETYPE_USIZE = CORETYPE_UINT
};

// RS_TOKEN(name, description)
// RS_TOKEN_KEYWORD(name, identifier)
//
// Keep RS_TOKEN_KEYWORD sorted

/* note that abstract, async, become, box, do, final, macro, override, priv,
 * try, typeof, unsized, virtual, and yield are unused */
#define RS_TOKEN_LIST                                                          \
  RS_TOKEN (FIRST_TOKEN, "<first-token-marker>")                               \
  RS_TOKEN (END_OF_FILE, "end of file")                                        \
  RS_TOKEN (EXCLAM, "!")                                                       \
  RS_TOKEN (NOT_EQUAL, "!=")                                                   \
  RS_TOKEN (PERCENT, "%")                                                      \
  RS_TOKEN (PERCENT_EQ, "%=")                                                  \
  RS_TOKEN (AMP, "&")                                                          \
  RS_TOKEN (AMP_EQ, "&=")                                                      \
  RS_TOKEN (LOGICAL_AND, "&&")                                                 \
  RS_TOKEN (ASTERISK, "*")                                                     \
  RS_TOKEN (ASTERISK_EQ, "*=")                                                 \
  RS_TOKEN (PLUS, "+")                                                         \
  RS_TOKEN (PLUS_EQ, "+=")                                                     \
  RS_TOKEN (COMMA, ",")                                                        \
  RS_TOKEN (MINUS, "-")                                                        \
  RS_TOKEN (MINUS_EQ, "-=")                                                    \
  RS_TOKEN (RETURN_TYPE, "->")                                                 \
  RS_TOKEN (DOT, ".")                                                          \
  RS_TOKEN (DOT_DOT, "..")                                                     \
  RS_TOKEN (DOT_DOT_EQ, "..=")                                                 \
  RS_TOKEN (ELLIPSIS, "...")                                                   \
  RS_TOKEN (DIV, "/")                                                          \
  RS_TOKEN (DIV_EQ, "/=")                                                      \
  RS_TOKEN (COLON, ":")                                                        \
  RS_TOKEN (SEMICOLON, ";")                                                    \
  RS_TOKEN (LEFT_SHIFT, "<<")                                                  \
  RS_TOKEN (LEFT_SHIFT_EQ, "<<=")                                              \
  RS_TOKEN (LEFT_ANGLE, "<")                                                   \
  RS_TOKEN (LESS_OR_EQUAL, "<=")                                               \
  RS_TOKEN (EQUAL, "=")                                                        \
  RS_TOKEN (EQUAL_EQUAL, "==")                                                 \
  RS_TOKEN (MATCH_ARROW, "=>")                                                 \
  RS_TOKEN (RIGHT_ANGLE, ">")                                                  \
  RS_TOKEN (GREATER_OR_EQUAL, ">=")                                            \
  RS_TOKEN (RIGHT_SHIFT, ">>")                                                 \
  RS_TOKEN (RIGHT_SHIFT_EQ, ">>=")                                             \
  RS_TOKEN (PATTERN_BIND, "@")                                                 \
  RS_TOKEN (TILDE, "~")                                                        \
  RS_TOKEN (BACKSLASH, "\\")                                                   \
  RS_TOKEN (BACKTICK, "`")                                                     \
  RS_TOKEN (CARET, "^")                                                        \
  RS_TOKEN (CARET_EQ, "^=")                                                    \
  RS_TOKEN (PIPE, "|")                                                         \
  RS_TOKEN (PIPE_EQ, "|=")                                                     \
  RS_TOKEN (OR, "||")                                                          \
  RS_TOKEN (QUESTION_MARK, "?")                                                \
  RS_TOKEN (HASH, "#")                                                         \
  /* from here on, dodgy and may not be correct. not operators and may be      \
   * symbols */                                                                \
  /* RS_TOKEN(SPACE, " ") probably too dodgy */                                \
  /* RS_TOKEN(NEWLINE, "\n")*/                                                 \
  RS_TOKEN (SCOPE_RESOLUTION, "::") /* dodgy */                                \
  RS_TOKEN (SINGLE_QUOTE, "'") /* should i differentiate from lifetime? */     \
  RS_TOKEN (DOUBLE_QUOTE, "\"")                                                \
  RS_TOKEN (UNDERSCORE,                                                        \
	    "_") /* TODO: treat as reserved word like mrustc instead? */       \
  RS_TOKEN (IDENTIFIER, "identifier")                                          \
  RS_TOKEN (INT_LITERAL,                                                       \
	    "integer literal") /* do different int and float types need        \
				  different literal types? */                  \
  RS_TOKEN (FLOAT_LITERAL, "float literal")                                    \
  RS_TOKEN (STRING_LITERAL, "string literal")                                  \
  RS_TOKEN (CHAR_LITERAL, "character literal")                                 \
  RS_TOKEN (BYTE_STRING_LITERAL, "byte string literal")                        \
  RS_TOKEN (BYTE_CHAR_LITERAL, "byte character literal")                       \
  RS_TOKEN (LIFETIME, "lifetime") /* TODO: improve token type */               \
  /* Have "interpolated" tokens (whatever that means)? identifer, path, type,  \
   * pattern, */                                                               \
  /* expression, statement, block, meta, item in mrustc (but not directly in   \
   * lexer). */                                                                \
  RS_TOKEN (LEFT_PAREN, "(")                                                   \
  RS_TOKEN (RIGHT_PAREN, ")")                                                  \
  RS_TOKEN (LEFT_CURLY, "{")                                                   \
  RS_TOKEN (RIGHT_CURLY, "}")                                                  \
  RS_TOKEN (LEFT_SQUARE, "[")                                                  \
  RS_TOKEN (RIGHT_SQUARE, "]")                                                 \
  /* Macros */                                                                 \
  RS_TOKEN (DOLLAR_SIGN, "$")                                                  \
  /* Doc Comments */                                                           \
  RS_TOKEN (INNER_DOC_COMMENT, "#![doc]")                                      \
  RS_TOKEN (OUTER_DOC_COMMENT, "#[doc]")                                       \
  /* have "weak" union and 'static keywords? */                                \
  RS_TOKEN_KEYWORD (ABSTRACT, "abstract") /* unused */                         \
  RS_TOKEN_KEYWORD (AS, "as")                                                  \
  RS_TOKEN_KEYWORD (ASYNC, "async")   /* unused */                             \
  RS_TOKEN_KEYWORD (BECOME, "become") /* unused */                             \
  RS_TOKEN_KEYWORD (BOX, "box")	      /* unused */                             \
  RS_TOKEN_KEYWORD (BREAK, "break")                                            \
  RS_TOKEN_KEYWORD (CONST, "const")                                            \
  RS_TOKEN_KEYWORD (CONTINUE, "continue")                                      \
  RS_TOKEN_KEYWORD (CRATE, "crate")                                            \
  /* FIXME: Do we need to add $crate (DOLLAR_CRATE) as a reserved kw? */       \
  RS_TOKEN_KEYWORD (DO, "do") /* unused */                                     \
  RS_TOKEN_KEYWORD (DYN, "dyn")                                                \
  RS_TOKEN_KEYWORD (ELSE, "else")                                              \
  RS_TOKEN_KEYWORD (ENUM_TOK, "enum")                                          \
  RS_TOKEN_KEYWORD (EXTERN_TOK, "extern")                                      \
  RS_TOKEN_KEYWORD (FALSE_LITERAL, "false")                                    \
  RS_TOKEN_KEYWORD (FINAL_TOK, "final") /* unused */                           \
  RS_TOKEN_KEYWORD (FN_TOK, "fn")                                              \
  RS_TOKEN_KEYWORD (FOR, "for")                                                \
  RS_TOKEN_KEYWORD (IF, "if")                                                  \
  RS_TOKEN_KEYWORD (IMPL, "impl")                                              \
  RS_TOKEN_KEYWORD (IN, "in")                                                  \
  RS_TOKEN_KEYWORD (LET, "let")                                                \
  RS_TOKEN_KEYWORD (LOOP, "loop")                                              \
  RS_TOKEN_KEYWORD (MACRO, "macro")                                            \
  RS_TOKEN_KEYWORD (MATCH_TOK, "match")                                        \
  RS_TOKEN_KEYWORD (MOD, "mod")                                                \
  RS_TOKEN_KEYWORD (MOVE, "move")                                              \
  RS_TOKEN_KEYWORD (MUT, "mut")                                                \
  RS_TOKEN_KEYWORD (OVERRIDE_TOK, "override") /* unused */                     \
  RS_TOKEN_KEYWORD (PRIV, "priv")	      /* unused */                     \
  RS_TOKEN_KEYWORD (PUB, "pub")                                                \
  RS_TOKEN_KEYWORD (REF, "ref")                                                \
  RS_TOKEN_KEYWORD (RETURN_TOK, "return")                                      \
  RS_TOKEN_KEYWORD (SELF_ALIAS,                                                \
		    "Self") /* mrustc does not treat this as a reserved word*/ \
  RS_TOKEN_KEYWORD (SELF, "self")                                              \
  RS_TOKEN_KEYWORD (STATIC_TOK, "static")                                      \
  RS_TOKEN_KEYWORD (STRUCT_TOK, "struct")                                      \
  RS_TOKEN_KEYWORD (SUPER, "super")                                            \
  RS_TOKEN_KEYWORD (TRAIT, "trait")                                            \
  RS_TOKEN_KEYWORD (TRUE_LITERAL, "true")                                      \
  RS_TOKEN_KEYWORD (TRY, "try") /* unused */                                   \
  RS_TOKEN_KEYWORD (TYPE, "type")                                              \
  RS_TOKEN_KEYWORD (TYPEOF, "typeof") /* unused */                             \
  RS_TOKEN_KEYWORD (UNSAFE, "unsafe")                                          \
  RS_TOKEN_KEYWORD (UNSIZED, "unsized") /* unused */                           \
  RS_TOKEN_KEYWORD (USE, "use")                                                \
  RS_TOKEN_KEYWORD (VIRTUAL, "virtual") /* unused */                           \
  RS_TOKEN_KEYWORD (WHERE, "where")                                            \
  RS_TOKEN_KEYWORD (WHILE, "while")                                            \
  RS_TOKEN_KEYWORD (YIELD, "yield") /* unused */                               \
  RS_TOKEN (LAST_TOKEN, "<last-token-marker>")

// Contains all token types. Crappy implementation via x-macros.
enum TokenId
{
#define RS_TOKEN(name, _) name,
#define RS_TOKEN_KEYWORD(x, y) RS_TOKEN (x, y)
  RS_TOKEN_LIST
#undef RS_TOKEN_KEYWORD
#undef RS_TOKEN
};

// dodgy "TokenPtr" declaration with Token forward declaration
class Token;
// A smart pointer (shared_ptr) to Token.
typedef std::shared_ptr<Token> TokenPtr;
// A smart pointer (shared_ptr) to a constant Token.
typedef std::shared_ptr<const Token> const_TokenPtr;

// Hackily defined way to get token description for enum value using x-macros
const char *
get_token_description (TokenId id);
/* Hackily defined way to get token description as a string for enum value using
 * x-macros */
const char *
token_id_to_str (TokenId id);
// Get type hint description as a string.
const char *
get_type_hint_string (PrimitiveCoreType type);

// Represents a single token. Create using factory static methods.
class Token
{
private:
  // Token kind.
  TokenId token_id;
  // Token location.
  Location locus;
  // Associated text (if any) of token.
  std::unique_ptr<std::string> str;
  // TODO: maybe remove issues and just store std::string as value?
  /* Type hint for token based on lexer data (e.g. type suffix). Does not exist
   * for most tokens. */
  PrimitiveCoreType type_hint;

  // Token constructor from token id and location. Has a null string.
  Token (TokenId token_id, Location location)
    : token_id (token_id), locus (location), str (nullptr),
      type_hint (CORETYPE_UNKNOWN)
  {}

  // Token constructor from token id, location, and a string.
  Token (TokenId token_id, Location location, std::string &&paramStr)
    : token_id (token_id), locus (location),
      str (new std::string (std::move (paramStr))), type_hint (CORETYPE_UNKNOWN)
  {}

  // Token constructor from token id, location, and a char.
  Token (TokenId token_id, Location location, char paramChar)
    : token_id (token_id), locus (location),
      str (new std::string (1, paramChar)), type_hint (CORETYPE_UNKNOWN)
  {}

  // Token constructor from token id, location, and a "codepoint".
  Token (TokenId token_id, Location location, Codepoint paramCodepoint)
    : token_id (token_id), locus (location),
      str (new std::string (paramCodepoint.as_string ())),
      type_hint (CORETYPE_UNKNOWN)
  {}

  // Token constructor from token id, location, a string, and type hint.
  Token (TokenId token_id, Location location, std::string &&paramStr,
	 PrimitiveCoreType parType)
    : token_id (token_id), locus (location),
      str (new std::string (std::move (paramStr))), type_hint (parType)
  {}

public:
  // No default constructor.
  Token () = delete;
  // Do not copy/assign tokens.
  Token (const Token &) = delete;
  Token &operator= (const Token &) = delete;

  // Allow moving tokens.
  Token (Token &&other) = default;
  Token &operator= (Token &&other) = default;

  ~Token () = default;

  /* TODO: make_shared (which saves a heap allocation) does not work with the
   * private constructor */

  // Makes and returns a new TokenPtr (with null string).
  static TokenPtr make (TokenId token_id, Location locus)
  {
    // return std::make_shared<Token> (token_id, locus);
    return TokenPtr (new Token (token_id, locus));
  }

  // Makes and returns a new TokenPtr of type IDENTIFIER.
  static TokenPtr make_identifier (Location locus, std::string &&str)
  {
    // return std::make_shared<Token> (IDENTIFIER, locus, str);
    return TokenPtr (new Token (IDENTIFIER, locus, std::move (str)));
  }

  // Makes and returns a new TokenPtr of type INT_LITERAL.
  static TokenPtr make_int (Location locus, std::string &&str,
			    PrimitiveCoreType type_hint = CORETYPE_UNKNOWN)
  {
    // return std::make_shared<Token> (INT_LITERAL, locus, str, type_hint);
    return TokenPtr (
      new Token (INT_LITERAL, locus, std::move (str), type_hint));
  }

  // Makes and returns a new TokenPtr of type FLOAT_LITERAL.
  static TokenPtr make_float (Location locus, std::string &&str,
			      PrimitiveCoreType type_hint = CORETYPE_UNKNOWN)
  {
    // return std::make_shared<Token> (FLOAT_LITERAL, locus, str, type_hint);
    return TokenPtr (
      new Token (FLOAT_LITERAL, locus, std::move (str), type_hint));
  }

  // Makes and returns a new TokenPtr of type STRING_LITERAL.
  static TokenPtr make_string (Location locus, std::string &&str)
  {
    // return std::make_shared<Token> (STRING_LITERAL, locus, str,
    // CORETYPE_STR);
    return TokenPtr (
      new Token (STRING_LITERAL, locus, std::move (str), CORETYPE_STR));
  }

  // Makes and returns a new TokenPtr of type CHAR_LITERAL.
  static TokenPtr make_char (Location locus, Codepoint char_lit)
  {
    // return std::make_shared<Token> (CHAR_LITERAL, locus, char_lit);
    return TokenPtr (new Token (CHAR_LITERAL, locus, char_lit));
  }

  // Makes and returns a new TokenPtr of type BYTE_CHAR_LITERAL.
  static TokenPtr make_byte_char (Location locus, char byte_char)
  {
    // return std::make_shared<Token> (BYTE_CHAR_LITERAL, locus, byte_char);
    return TokenPtr (new Token (BYTE_CHAR_LITERAL, locus, byte_char));
  }

  // Makes and returns a new TokenPtr of type BYTE_STRING_LITERAL (fix).
  static TokenPtr make_byte_string (Location locus, std::string &&str)
  {
    // return std::make_shared<Token> (BYTE_STRING_LITERAL, locus, str);
    return TokenPtr (new Token (BYTE_STRING_LITERAL, locus, std::move (str)));
  }

  // Makes and returns a new TokenPtr of type INNER_DOC_COMMENT.
  static TokenPtr make_inner_doc_comment (Location locus, std::string &&str)
  {
    return TokenPtr (new Token (INNER_DOC_COMMENT, locus, std::move (str)));
  }

  // Makes and returns a new TokenPtr of type OUTER_DOC_COMMENT.
  static TokenPtr make_outer_doc_comment (Location locus, std::string &&str)
  {
    return TokenPtr (new Token (OUTER_DOC_COMMENT, locus, std::move (str)));
  }

  // Makes and returns a new TokenPtr of type LIFETIME.
  static TokenPtr make_lifetime (Location locus, std::string &&str)
  {
    // return std::make_shared<Token> (LIFETIME, locus, str);
    return TokenPtr (new Token (LIFETIME, locus, std::move (str)));
  }

  // Gets id of the token.
  TokenId get_id () const { return token_id; }

  // Gets location of the token.
  Location get_locus () const { return locus; }

  // Gets string description of the token.
  const std::string &
  get_str () const; /*{
// FIXME: put in header again when fix null problem
//gcc_assert(str != nullptr);
if (str == nullptr) {
error_at(get_locus(), "attempted to get string for '%s', which has no string.
returning empty string instead.", get_token_description()); return "";
}
return *str;
}*/

  // Gets token's type hint info.
  PrimitiveCoreType get_type_hint () const
  {
    return type_hint == CORETYPE_PURE_DECIMAL ? CORETYPE_UNKNOWN : type_hint;
  }

  // diagnostics (error reporting)
  const char *get_token_description () const
  {
    return Rust::get_token_description (token_id);
  }

  // debugging
  const char *token_id_to_str () const
  {
    return Rust::token_id_to_str (token_id);
  }

  // debugging
  const char *get_type_hint_str () const;

  /* Returns whether the token is a literal of any type (int, float, char,
   * string, byte char, byte string). */
  bool is_literal () const
  {
    switch (token_id)
      {
      case INT_LITERAL:
      case FLOAT_LITERAL:
      case CHAR_LITERAL:
      case STRING_LITERAL:
      case BYTE_CHAR_LITERAL:
      case BYTE_STRING_LITERAL:
	return true;
      default:
	return false;
      }
  }

  /* Returns whether the token actually has a string (regardless of whether it
   * should or not). */
  bool has_str () const { return str != nullptr; }

  // Returns whether the token should have a string.
  bool should_have_str () const
  {
    return is_literal () || token_id == IDENTIFIER || token_id == LIFETIME;
  }

  // Returns whether the token is a pure decimal int literal
  bool is_pure_decimal () const { return type_hint == CORETYPE_PURE_DECIMAL; }
};
} // namespace Rust

#endif
