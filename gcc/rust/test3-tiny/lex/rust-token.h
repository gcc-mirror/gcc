#ifndef RUST_TOKEN_H
#define RUST_TOKEN_H

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "input.h"
// order: config, system, coretypes, input

#include <string>
#include <tr1/memory> // as shared_ptr is not available in std memory in c++03

namespace Rust {
// RS_TOKEN(name, description)
// RS_TOKEN_KEYWORD(name, identifier)
//
// Keep RS_TOKEN_KEYWORD sorted

// TODO: change to actual rust tokens
#define RS_TOKEN_LIST                             \
    RS_TOKEN(FIRST_TOKEN, "<first-token-marker>") \
    RS_TOKEN(END_OF_FILE, "end of file")          \
    RS_TOKEN(ASSIG, ":=")                         \
    RS_TOKEN(ASTERISK, "*")                       \
    RS_TOKEN(COLON, ":")                          \
    RS_TOKEN(DIFFERENT, "!=")                     \
    RS_TOKEN(EQUAL, "=")                          \
    RS_TOKEN(LEFT_PAREN, "(")                     \
    RS_TOKEN(MINUS, "-")                          \
    RS_TOKEN(PLUS, "+")                           \
    RS_TOKEN(RIGHT_PAREN, ")")                    \
    RS_TOKEN(SEMICOLON, ";")                      \
    RS_TOKEN(SLASH, "/")                          \
    RS_TOKEN(PERCENT, "%")                        \
    RS_TOKEN(GREATER, ">")                        \
    RS_TOKEN(GREATER_OR_EQUAL, ">=")              \
    RS_TOKEN(SMALLER, "<")                        \
    RS_TOKEN(SMALLER_OR_EQUAL, "<=")              \
    RS_TOKEN(IDENTIFIER, "identifier")            \
    RS_TOKEN(INT_LITERAL, "integer literal")      \
    RS_TOKEN(FLOAT_LITERAL, "float literal")      \
    RS_TOKEN(STRING_LITERAL, "string literal")    \
    RS_TOKEN(LEFT_SQUARE, "[")                    \
    RS_TOKEN(RIGHT_SQUARE, "]")                   \
    RS_TOKEN(DOT, ".")                            \
                                                  \
    RS_TOKEN_KEYWORD(AND, "and")                  \
    RS_TOKEN_KEYWORD(BOOL, "bool")                \
    RS_TOKEN_KEYWORD(DO, "do")                    \
    RS_TOKEN_KEYWORD(ELSE, "else")                \
    RS_TOKEN_KEYWORD(END, "end")                  \
    RS_TOKEN_KEYWORD(FALSE_LITERAL, "false")      \
    RS_TOKEN_KEYWORD(FLOAT, "float")              \
    RS_TOKEN_KEYWORD(FOR, "for")                  \
    RS_TOKEN_KEYWORD(IF, "if")                    \
    RS_TOKEN_KEYWORD(INT, "int")                  \
    RS_TOKEN_KEYWORD(NOT, "not")                  \
    RS_TOKEN_KEYWORD(OR, "or")                    \
    RS_TOKEN_KEYWORD(READ, "read")                \
    RS_TOKEN_KEYWORD(RECORD, "record")            \
    RS_TOKEN_KEYWORD(THEN, "then")                \
    RS_TOKEN_KEYWORD(TO, "to")                    \
    RS_TOKEN_KEYWORD(TRUE_LITERAL, "true")        \
    RS_TOKEN_KEYWORD(TYPE, "type")                \
    RS_TOKEN_KEYWORD(VAR, "var")                  \
    RS_TOKEN_KEYWORD(WHILE, "while")              \
    RS_TOKEN_KEYWORD(WRITE, "write")              \
                                                  \
    RS_TOKEN(LAST_TOKEN, "<last-token-marker>")

    // Contains all token types. Crappy implementation via x-macros.
    enum TokenId {
#define RS_TOKEN(name, _) name,
#define RS_TOKEN_KEYWORD(x, y) RS_TOKEN(x, y)
        RS_TOKEN_LIST
#undef RS_TOKEN_KEYWORD
#undef RS_TOKEN
    };

    // dodgy "TokenPtr" declaration with Token forward declaration
    class Token;
    // A smart pointer (shared_ptr) to Token.
    typedef std::tr1::shared_ptr<Token> TokenPtr;
    // A smart pointer (shared_ptr) to a constant Token.
    typedef std::tr1::shared_ptr<const Token> const_TokenPtr;

    // Hackily defined way to get token description for enum value using x-macros
    const char* get_token_description(TokenId id);
    // Hackily defined way to get token description as a string for enum value using x-macros
    const char* token_id_to_str(TokenId id);

    // Represents a single token. Create using factory static methods.
    class Token {
      private:
        // Token kind.
        TokenId token_id;
        // Token location.
        location_t locus;
        // Associated text (if any) of token.
        std::string* str;

        // Token constructor from token id and location. Has a null string.
        Token(TokenId token_id, location_t location) :
          token_id(token_id), locus(location), str(NULL) {}

        // Token constructor from token id, location, and a string.
        Token(TokenId token_id, location_t location, const std::string& paramStr) :
          token_id(token_id), locus(location), str(new std::string(paramStr)) {}

        // No default initialiser.
        Token();
        // Do not copy/assign tokens.
        Token(const Token&);
        Token& operator=(const Token&);

      public:
        ~Token() {
            delete str;
        }

        // Makes and returns a new TokenPtr (with null string).
        static TokenPtr make(TokenId token_id, location_t locus) {
            return TokenPtr(new Token(token_id, locus));
        }

        // Makes and returns a new TokenPtr of type IDENTIFIER.
        static TokenPtr make_identifier(location_t locus, const std::string& str) {
            return TokenPtr(new Token(IDENTIFIER, locus, str));
        }

        // Makes and returns a new TokenPtr of type INT_LITERAL.
        static TokenPtr make_int(location_t locus, const std::string& str) {
            return TokenPtr(new Token(INT_LITERAL, locus, str));
        }

        // Makes and returns a new TokenPtr of type FLOAT_LITERAL.
        static TokenPtr make_float(location_t locus, const std::string& str) {
            return TokenPtr(new Token(FLOAT_LITERAL, locus, str));
        }

        static TokenPtr make_string(location_t locus, const std::string& str) {
            return TokenPtr(new Token(STRING_LITERAL, locus, str));
        }

        // Gets id of the token.
        TokenId get_id() const {
            return token_id;
        }

        // Gets location of the token.
        location_t get_locus() const {
            return locus;
        }

        // Gets string description of the token.
        const std::string& get_str() const {
            gcc_assert(str != NULL);
            return *str;
        }

        // diagnostics
        const char* get_token_description() const {
            return Rust::get_token_description(token_id);
        }

        // debugging
        const char* token_id_to_str() const {
            return Rust::token_id_to_str(token_id);
        }
    };
}

#endif