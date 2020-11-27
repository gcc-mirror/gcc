// rs-token.h -- Rust frontend token thing idk.     -*- C++ -*-

// Copyright etc

#ifndef RUST_TOKEN_H
#define RUST_TOKEN_H

#include <string>

enum eTokenType {
#define _(t) t,
#include "eTokenType.enum.h"
#undef _
};

// Location of a token
class Location {
  public:
    std::string filename;
    unsigned int line;
    unsigned int char_num;

    Location()
      : filename("")
      , line(0)
      , char_num(0) {}

    Location(std::string filename, unsigned int line, unsigned int ofs)
      : filename(filename)
      , line(line)
      , char_num(ofs) {
    }
};

// A token returned from the lexer.

class Token {
  public:
    // Token classification. -- going to replace with eTokenType, probably
    /* enum Classification {
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
    };*/

    ~Token();
    Token(const Token&);
    Token& operator=(const Token&);

    // Get token classification.
    Classification
    classification() const { return this->classification_; }

    // Make a token for an invalid value.
    static Token
    make_invalid_token(Location location) { return Token(TOKEN_INVALID, location); }

    // Make a token representing end of file.
    static Token
    make_eof_token(Location location) { return Token(TOKEN_EOF, location); }

    // Make a keyword token.
    static Token
    make_keyword_token(Keyword keyword, Location location) {
        Token tok(TOKEN_KEYWORD, location);
        tok.u_.keyword = keyword;
        return tok;
    }

    // Make an identifier token.
    static Token
    make_identifier_token(const std::string& value, bool is_exported, Location location) {
        Token tok(TOKEN_IDENTIFIER, location);
        tok.u_.identifier_value.name = new std::string(value);
        tok.u_.identifier_value.is_exported = is_exported;
        return tok;
    }

    // Make a quoted string token.
    static Token
    make_string_token(const std::string& value, Location location) {
        Token tok(TOKEN_STRING, location);
        tok.u_.string_value = new std::string(value);
        return tok;
    }

    // Make an operator token.
    static Token
    make_operator_token(Operator op, Location location) {
        Token tok(TOKEN_OPERATOR, location);
        tok.u_.op = op;
        return tok;
    }

    // Make a character constant token.
    static Token
    make_character_token(mpz_t val, Location location) {
        Token tok(TOKEN_CHARACTER, location);
        mpz_init(tok.u_.integer_value);
        mpz_swap(tok.u_.integer_value, val);
        return tok;
    }

    // Make an integer token.
    static Token
    make_integer_token(mpz_t val, Location location) {
        Token tok(TOKEN_INTEGER, location);
        mpz_init(tok.u_.integer_value);
        mpz_swap(tok.u_.integer_value, val);
        return tok;
    }

    // Make a float token.
    static Token
    make_float_token(mpfr_t val, Location location) {
        Token tok(TOKEN_FLOAT, location);
        mpfr_init(tok.u_.float_value);
        mpfr_swap(tok.u_.float_value, val);
        return tok;
    }

    // Make a token for an imaginary number.
    static Token
    make_imaginary_token(mpfr_t val, Location location) {
        Token tok(TOKEN_IMAGINARY, location);
        mpfr_init(tok.u_.float_value);
        mpfr_swap(tok.u_.float_value, val);
        return tok;
    }

    // Get the location of the token.
    Location
    location() const { return this->location_; }

    // Return whether this is an invalid token.
    bool
    is_invalid() const { return this->classification_ == TOKEN_INVALID; }

    // Return whether this is the EOF token.
    bool
    is_eof() const { return this->classification_ == TOKEN_EOF; }

    // Return the keyword value for a keyword token.
    Keyword
    keyword() const {
        gcc_assert(this->classification_ == TOKEN_KEYWORD);
        return this->u_.keyword;
    }

    // Return whether this is an identifier.
    bool
    is_identifier() const { return this->classification_ == TOKEN_IDENTIFIER; }

    // Return the identifier.
    const std::string&
    identifier() const {
        gcc_assert(this->classification_ == TOKEN_IDENTIFIER);
        return *this->u_.identifier_value.name;
    }

    // Return whether the identifier is exported.
    bool
    is_identifier_exported() const {
        gcc_assert(this->classification_ == TOKEN_IDENTIFIER);
        return this->u_.identifier_value.is_exported;
    }

    // Return whether this is a string.
    bool
    is_string() const {
        return this->classification_ == TOKEN_STRING;
    }

    // Return the value of a string.  The returned value is a string of
    // UTF-8 characters.
    std::string
    string_value() const {
        gcc_assert(this->classification_ == TOKEN_STRING);
        return *this->u_.string_value;
    }

    // Return the value of a character constant.
    const mpz_t*
    character_value() const {
        gcc_assert(this->classification_ == TOKEN_CHARACTER);
        return &this->u_.integer_value;
    }

    // Return the value of an integer.
    const mpz_t*
    integer_value() const {
        gcc_assert(this->classification_ == TOKEN_INTEGER);
        return &this->u_.integer_value;
    }

    // Return the value of a float.
    const mpfr_t*
    float_value() const {
        gcc_assert(this->classification_ == TOKEN_FLOAT);
        return &this->u_.float_value;
    }

    // Return the value of an imaginary number.
    const mpfr_t*
    imaginary_value() const {
        gcc_assert(this->classification_ == TOKEN_IMAGINARY);
        return &this->u_.float_value;
    }

    // Return the operator value for an operator token.
    Operator
    op() const {
        gcc_assert(this->classification_ == TOKEN_OPERATOR);
        return this->u_.op;
    }

    // Return whether this token is KEYWORD.
    bool
    is_keyword(Keyword keyword) const {
        return (this->classification_ == TOKEN_KEYWORD && this->u_.keyword == keyword);
    }

    // Return whether this token is OP.
    bool
    is_op(Operator op) const { return this->classification_ == TOKEN_OPERATOR && this->u_.op == op; }

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
    union {
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

#endif // RUST_TOKEN_H