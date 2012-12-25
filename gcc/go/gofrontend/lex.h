// lex.h -- Go frontend lexer.     -*- C++ -*-

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef GO_LEX_H
#define GO_LEX_H

#include <mpfr.h>

#include "operator.h"
#include "go-linemap.h"

struct Unicode_range;

// The keywords.  These must be in sorted order, other than
// KEYWORD_INVALID.  They must match the Keywords::mapping_ array in
// lex.cc.

enum Keyword
{
  KEYWORD_INVALID,	// Not a keyword.
  KEYWORD_ASM,
  KEYWORD_BREAK,
  KEYWORD_CASE,
  KEYWORD_CHAN,
  KEYWORD_CONST,
  KEYWORD_CONTINUE,
  KEYWORD_DEFAULT,
  KEYWORD_DEFER,
  KEYWORD_ELSE,
  KEYWORD_FALLTHROUGH,
  KEYWORD_FOR,
  KEYWORD_FUNC,
  KEYWORD_GO,
  KEYWORD_GOTO,
  KEYWORD_IF,
  KEYWORD_IMPORT,
  KEYWORD_INTERFACE,
  KEYWORD_MAP,
  KEYWORD_PACKAGE,
  KEYWORD_RANGE,
  KEYWORD_RETURN,
  KEYWORD_SELECT,
  KEYWORD_STRUCT,
  KEYWORD_SWITCH,
  KEYWORD_TYPE,
  KEYWORD_VAR
};

// A token returned from the lexer.

class Token
{
 public:
  // Token classification.
  enum Classification
  {
    // Token is invalid.
    TOKEN_INVALID,
    // Token indicates end of input.
    TOKEN_EOF,
    // Token is a keyword.
    TOKEN_KEYWORD,
    // Token is an identifier.
    TOKEN_IDENTIFIER,
    // Token is a string of characters.
    TOKEN_STRING,
    // Token is an operator.
    TOKEN_OPERATOR,
    // Token is a character constant.
    TOKEN_CHARACTER,
    // Token is an integer.
    TOKEN_INTEGER,
    // Token is a floating point number.
    TOKEN_FLOAT,
    // Token is an imaginary number.
    TOKEN_IMAGINARY
  };

  ~Token();
  Token(const Token&);
  Token& operator=(const Token&);

  // Get token classification.
  Classification
  classification() const
  { return this->classification_; }

  // Make a token for an invalid value.
  static Token
  make_invalid_token(Location location)
  { return Token(TOKEN_INVALID, location); }

  // Make a token representing end of file.
  static Token
  make_eof_token(Location location)
  { return Token(TOKEN_EOF, location); }

  // Make a keyword token.
  static Token
  make_keyword_token(Keyword keyword, Location location)
  {
    Token tok(TOKEN_KEYWORD, location);
    tok.u_.keyword = keyword;
    return tok;
  }

  // Make an identifier token.
  static Token
  make_identifier_token(const std::string& value, bool is_exported,
			Location location)
  {
    Token tok(TOKEN_IDENTIFIER, location);
    tok.u_.identifier_value.name = new std::string(value);
    tok.u_.identifier_value.is_exported = is_exported;
    return tok;
  }

  // Make a quoted string token.
  static Token
  make_string_token(const std::string& value, Location location)
  {
    Token tok(TOKEN_STRING, location);
    tok.u_.string_value = new std::string(value);
    return tok;
  }

  // Make an operator token.
  static Token
  make_operator_token(Operator op, Location location)
  {
    Token tok(TOKEN_OPERATOR, location);
    tok.u_.op = op;
    return tok;
  }

  // Make a character constant token.
  static Token
  make_character_token(mpz_t val, Location location)
  {
    Token tok(TOKEN_CHARACTER, location);
    mpz_init(tok.u_.integer_value);
    mpz_swap(tok.u_.integer_value, val);
    return tok;
  }

  // Make an integer token.
  static Token
  make_integer_token(mpz_t val, Location location)
  {
    Token tok(TOKEN_INTEGER, location);
    mpz_init(tok.u_.integer_value);
    mpz_swap(tok.u_.integer_value, val);
    return tok;
  }

  // Make a float token.
  static Token
  make_float_token(mpfr_t val, Location location)
  {
    Token tok(TOKEN_FLOAT, location);
    mpfr_init(tok.u_.float_value);
    mpfr_swap(tok.u_.float_value, val);
    return tok;
  }

  // Make a token for an imaginary number.
  static Token
  make_imaginary_token(mpfr_t val, Location location)
  {
    Token tok(TOKEN_IMAGINARY, location);
    mpfr_init(tok.u_.float_value);
    mpfr_swap(tok.u_.float_value, val);
    return tok;
  }

  // Get the location of the token.
  Location
  location() const
  { return this->location_; }

  // Return whether this is an invalid token.
  bool
  is_invalid() const
  { return this->classification_ == TOKEN_INVALID; }

  // Return whether this is the EOF token.
  bool
  is_eof() const
  { return this->classification_ == TOKEN_EOF; }

  // Return the keyword value for a keyword token.
  Keyword
  keyword() const
  {
    go_assert(this->classification_ == TOKEN_KEYWORD);
    return this->u_.keyword;
  }

  // Return whether this is an identifier.
  bool
  is_identifier() const
  { return this->classification_ == TOKEN_IDENTIFIER; }

  // Return the identifier.
  const std::string&
  identifier() const
  {
    go_assert(this->classification_ == TOKEN_IDENTIFIER);
    return *this->u_.identifier_value.name;
  }

  // Return whether the identifier is exported.
  bool
  is_identifier_exported() const
  {
    go_assert(this->classification_ == TOKEN_IDENTIFIER);
    return this->u_.identifier_value.is_exported;
  }

  // Return whether this is a string.
  bool
  is_string() const
  {
    return this->classification_ == TOKEN_STRING;
  }

  // Return the value of a string.  The returned value is a string of
  // UTF-8 characters.
  std::string
  string_value() const
  {
    go_assert(this->classification_ == TOKEN_STRING);
    return *this->u_.string_value;
  }

  // Return the value of a character constant.
  const mpz_t*
  character_value() const
  {
    go_assert(this->classification_ == TOKEN_CHARACTER);
    return &this->u_.integer_value;
  }

  // Return the value of an integer.
  const mpz_t*
  integer_value() const
  {
    go_assert(this->classification_ == TOKEN_INTEGER);
    return &this->u_.integer_value;
  }

  // Return the value of a float.
  const mpfr_t*
  float_value() const
  {
    go_assert(this->classification_ == TOKEN_FLOAT);
    return &this->u_.float_value;
  }

  // Return the value of an imaginary number.
  const mpfr_t*
  imaginary_value() const
  {
    go_assert(this->classification_ == TOKEN_IMAGINARY);
    return &this->u_.float_value;
  }

  // Return the operator value for an operator token.
  Operator
  op() const
  {
    go_assert(this->classification_ == TOKEN_OPERATOR);
    return this->u_.op;
  }

  // Return whether this token is KEYWORD.
  bool
  is_keyword(Keyword keyword) const
  {
    return (this->classification_ == TOKEN_KEYWORD
	    && this->u_.keyword == keyword);
  }

  // Return whether this token is OP.
  bool
  is_op(Operator op) const
  { return this->classification_ == TOKEN_OPERATOR && this->u_.op == op; }

  // Print the token for debugging.
  void
  print(FILE*) const;

 private:
  // Private constructor used by make_..._token functions above.
  Token(Classification, Location);

  // Clear the token.
  void
  clear();

  // The token classification.
  Classification classification_;
  union
  {
    // The keyword value for TOKEN_KEYWORD.
    Keyword keyword;
    // The token value for TOKEN_IDENTIFIER.
    struct
    {
      // The name of the identifier.  This has been mangled to only
      // include ASCII characters.
      std::string* name;
      // Whether this name should be exported.  This is true if the
      // first letter in the name is upper case.
      bool is_exported;
    } identifier_value;
    // The string value for TOKEN_STRING.
    std::string* string_value;
    // The token value for TOKEN_CHARACTER or TOKEN_INTEGER.
    mpz_t integer_value;
    // The token value for TOKEN_FLOAT or TOKEN_IMAGINARY.
    mpfr_t float_value;
    // The token value for TOKEN_OPERATOR or the keyword value
    Operator op;
  } u_;
  // The source location.
  Location location_;
};

// The lexer itself.

class Lex
{
 public:
  Lex(const char* input_file_name, FILE* input_file, Linemap *linemap);

  ~Lex();

  // Return the next token.
  Token
  next_token();

  // Return the contents of any current //extern comment.
  const std::string&
  extern_name() const
  { return this->extern_; }

  // Return whether we have seen a //go:nointerface comment, clearing
  // the flag.
  bool
  get_and_clear_nointerface()
  {
    bool ret = this->saw_nointerface_;
    this->saw_nointerface_ = false;
    return ret;
  }

  // Return whether the identifier NAME should be exported.  NAME is a
  // mangled name which includes only ASCII characters.
  static bool
  is_exported_name(const std::string& name);

  // Return whether the identifier NAME is invalid.  When we see an
  // invalid character we still build an identifier, but we use a
  // magic string to indicate that the identifier is invalid.  We then
  // use this to avoid knockon errors.
  static bool
  is_invalid_identifier(const std::string& name);

  // A helper function.  Append V to STR.  IS_CHARACTER is true if V
  // is a Unicode character which should be converted into UTF-8,
  // false if it is a byte value to be appended directly.  The
  // location is used to warn about an out of range character.
  static void
  append_char(unsigned int v, bool is_charater, std::string* str,
	      Location);

  // A helper function.  Fetch a UTF-8 character from STR and store it
  // in *VALUE.  Return the number of bytes read from STR.  Return 0
  // if STR does not point to a valid UTF-8 character.
  static int
  fetch_char(const char* str, unsigned int *value);

  // Return whether C is a Unicode or "C" locale space character.
  static bool
  is_unicode_space(unsigned int c);

 private:
  ssize_t
  get_line();

  bool
  require_line();

  // The current location.
  Location
  location() const;

  // A position CHARS column positions before the current location.
  Location
  earlier_location(int chars) const;

  static bool
  is_hex_digit(char);

  static unsigned char
  octal_value(char c)
  { return c - '0'; }

  Token
  make_invalid_token()
  { return Token::make_invalid_token(this->location()); }

  Token
  make_eof_token()
  { return Token::make_eof_token(this->location()); }

  Token
  make_operator(Operator op, int chars)
  { return Token::make_operator_token(op, this->earlier_location(chars)); }

  Token
  gather_identifier();

  static bool
  could_be_exponent(const char*, const char*);

  Token
  gather_number();

  Token
  gather_character();

  Token
  gather_string();

  Token
  gather_raw_string();

  const char*
  advance_one_utf8_char(const char*, unsigned int*, bool*);

  const char*
  advance_one_char(const char*, bool, unsigned int*, bool*);

  static bool
  is_unicode_digit(unsigned int c);

  static bool
  is_unicode_letter(unsigned int c);

  static bool
  is_unicode_uppercase(unsigned int c);

  static bool
  is_in_unicode_range(unsigned int C, const Unicode_range* ranges,
		      size_t range_size);

  Operator
  three_character_operator(char, char, char);

  Operator
  two_character_operator(char, char);

  Operator
  one_character_operator(char);

  bool
  skip_c_comment();

  void
  skip_cpp_comment();

  // The input file name.
  const char* input_file_name_;
  // The input file.
  FILE* input_file_;
  // The object used to keep track of file names and line numbers.
  Linemap* linemap_;
  // The line buffer.  This holds the current line.
  char* linebuf_;
  // The size of the line buffer.
  size_t linebufsize_;
  // The nmber of characters in the current line.
  size_t linesize_;
  // The current offset in linebuf_.
  size_t lineoff_;
  // The current line number.
  size_t lineno_;
  // Whether to add a semicolon if we see a newline now.
  bool add_semi_at_eol_;
  // Whether we just saw a magic go:nointerface comment.
  bool saw_nointerface_;
  // The external name to use for a function declaration, from a magic
  // //extern comment.
  std::string extern_;
};

#endif // !defined(GO_LEX_H)
