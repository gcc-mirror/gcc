#ifndef RUST_TOKEN_H
#define RUST_TOKEN_H

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "input.h"
// order: config, system, coretypes, input

#include <string>
//#include <tr1/memory> // as shared_ptr is not available in std memory in c++03
// replace with proper std::memory in c++11
#include <memory>

#include "rust-codepoint.h"

namespace Rust {
    // "Primitive core types" in Rust - the different int and float types, as well as some others
    enum PrimitiveCoreType {
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
        // arch-dependent pointer sizes
        CORETYPE_ISIZE = CORETYPE_INT,
        CORETYPE_USIZE = CORETYPE_UINT
    };

// RS_TOKEN(name, description)
// RS_TOKEN_KEYWORD(name, identifier)
//
// Keep RS_TOKEN_KEYWORD sorted

// note that abstract, async, become, box, do, final, macro, override, priv, try, typeof, unsized,
// virtual, and yield are unused
// TODO finish converting to rust keywords
#define RS_TOKEN_LIST                                                                         \
    RS_TOKEN(FIRST_TOKEN, "<first-token-marker>")                                             \
    RS_TOKEN(END_OF_FILE, "end of file")                                                      \
    RS_TOKEN(EXCLAM, "!")                                                                     \
    RS_TOKEN(NOT_EQUAL, "!=")                                                                 \
    RS_TOKEN(PERCENT, "%")                                                                    \
    RS_TOKEN(PERCENT_EQ, "%=")                                                                \
    RS_TOKEN(AMP, "&")                                                                        \
    RS_TOKEN(AMP_EQ, "&=")                                                                    \
    RS_TOKEN(LOGICAL_AND, "&&")                                                               \
    RS_TOKEN(ASTERISK, "*")                                                                   \
    RS_TOKEN(ASTERISK_EQ, "*=")                                                               \
    RS_TOKEN(PLUS, "+")                                                                       \
    RS_TOKEN(PLUS_EQ, "+=")                                                                   \
    RS_TOKEN(COMMA, ",")                                                                      \
    RS_TOKEN(MINUS, "-")                                                                      \
    RS_TOKEN(MINUS_EQ, "-=")                                                                  \
    RS_TOKEN(RETURN_TYPE, "->")                                                               \
    RS_TOKEN(DOT, ".")                                                                        \
    RS_TOKEN(DOT_DOT, "..")                                                                   \
    RS_TOKEN(DOT_DOT_EQ, "..=")                                                               \
    RS_TOKEN(ELLIPSIS, "...")                                                                 \
    RS_TOKEN(DIV, "/")                                                                        \
    RS_TOKEN(DIV_EQ, "/=")                                                                    \
    RS_TOKEN(COLON, ":")                                                                      \
    RS_TOKEN(SEMICOLON, ";")                                                                  \
    RS_TOKEN(LEFT_SHIFT, "<<")                                                                \
    RS_TOKEN(LEFT_SHIFT_EQ, "<<=")                                                            \
    RS_TOKEN(LEFT_ANGLE, "<")                                                                 \
    RS_TOKEN(LESS_OR_EQUAL, "<=")                                                             \
    RS_TOKEN(EQUAL, "=")                                                                      \
    RS_TOKEN(EQUAL_EQUAL, "==")                                                               \
    RS_TOKEN(MATCH_ARROW, "=>")                                                               \
    RS_TOKEN(RIGHT_ANGLE, ">")                                                                \
    RS_TOKEN(GREATER_OR_EQUAL, ">=")                                                          \
    RS_TOKEN(RIGHT_SHIFT, ">>")                                                               \
    RS_TOKEN(RIGHT_SHIFT_EQ, ">>=")                                                           \
    RS_TOKEN(PATTERN_BIND, "@")                                                               \
    RS_TOKEN(TILDE, "~")                                                                      \
    RS_TOKEN(BACKSLASH, "\\")                                                                 \
    RS_TOKEN(BACKTICK, "`")                                                                   \
    RS_TOKEN(CARET, "^")                                                                      \
    RS_TOKEN(CARET_EQ, "^=")                                                                  \
    RS_TOKEN(PIPE, "|")                                                                       \
    RS_TOKEN(PIPE_EQ, "|=")                                                                   \
    RS_TOKEN(OR, "||")                                                                        \
    RS_TOKEN(QUESTION_MARK, "?")                                                              \
    RS_TOKEN(HASH, "#")                                                                       \
    /* from here on, dodgy and may not be correct. not operators and may be symbols */        \
    /* RS_TOKEN(SPACE, " ") probably too dodgy */                                             \
    /* RS_TOKEN(NEWLINE, "\n")*/                                                              \
    RS_TOKEN(SCOPE_RESOLUTION, "::") /* dodgy */                                              \
    RS_TOKEN(SINGLE_QUOTE, "'")      /* should i differentiate from lifetime? */              \
    RS_TOKEN(DOUBLE_QUOTE, "\"")                                                              \
    RS_TOKEN(UNDERSCORE, "_") /* TODO: treat as reserved word like mrustc instead? */         \
    RS_TOKEN(IDENTIFIER, "identifier")                                                        \
    RS_TOKEN(INT_LITERAL,                                                                     \
      "integer literal") /* do different int and float types need different literal types? */ \
    RS_TOKEN(FLOAT_LITERAL, "float literal")                                                  \
    RS_TOKEN(STRING_LITERAL, "string literal")                                                \
    RS_TOKEN(CHAR_LITERAL, "character literal")                                               \
    RS_TOKEN(BYTE_STRING_LITERAL, "byte string literal")                                      \
    RS_TOKEN(BYTE_CHAR_LITERAL, "byte character literal")                                     \
    RS_TOKEN(LIFETIME, "lifetime") /* TODO: improve token type */                             \
    /* Have "interpolated" tokens (whatever that means)? identifer, path, type, pattern, */   \
    /* expression, statement, block, meta, item in mrustc (but not directly in lexer). */     \
    RS_TOKEN(LEFT_PAREN, "(")                                                                 \
    RS_TOKEN(RIGHT_PAREN, ")")                                                                \
    RS_TOKEN(LEFT_CURLY, "{")                                                                 \
    RS_TOKEN(RIGHT_CURLY, "}")                                                                \
    RS_TOKEN(LEFT_SQUARE, "[")                                                                \
    RS_TOKEN(RIGHT_SQUARE, "]")                                                               \
    /* Macros */                                                                              \
    RS_TOKEN(DOLLAR_SIGN, "$")                                                                \
    /* Comments */                                                                            \
    RS_TOKEN(LINE_COMMENT, "//")                                                              \
    RS_TOKEN(INNER_LINE_DOC, "//!")                                                           \
    RS_TOKEN(OUTER_LINE_DOC, "///")                                                           \
    RS_TOKEN(BLOCK_COMMENT_START, "/*")                                                       \
    RS_TOKEN(BLOCK_COMMENT_END, "*/")                                                         \
    RS_TOKEN(INNER_BLOCK_DOC_START, "/*!")                                                    \
    RS_TOKEN(OUTER_BLOCK_DOC_START, "/**")  /* have "weak" union and 'static keywords? */     \
                                                                                              \
    RS_TOKEN_KEYWORD(ABSTRACT, "abstract") /* unused */                                       \
    RS_TOKEN_KEYWORD(AS, "as")                                                                \
    RS_TOKEN_KEYWORD(ASYNC, "async")   /* unused */                                           \
    RS_TOKEN_KEYWORD(BECOME, "become") /* unused */                                           \
    RS_TOKEN_KEYWORD(BOX, "box")       /* unused */                                           \
    RS_TOKEN_KEYWORD(BREAK, "break")                                                          \
    RS_TOKEN_KEYWORD(CONST, "const")                                                          \
    RS_TOKEN_KEYWORD(CONTINUE, "continue")                                                    \
    RS_TOKEN_KEYWORD(CRATE, "crate")                                                          \
    RS_TOKEN_KEYWORD(DO, "do") /* unused */                                                   \
    RS_TOKEN_KEYWORD(DYN, "dyn")                                                              \
    RS_TOKEN_KEYWORD(ELSE, "else")                                                            \
    RS_TOKEN_KEYWORD(ENUM_TOK, "enum")                                                        \
    RS_TOKEN_KEYWORD(EXTERN_TOK, "extern")                                                    \
    RS_TOKEN_KEYWORD(FALSE_LITERAL, "false")                                                  \
    RS_TOKEN_KEYWORD(FINAL_TOK, "final") /* unused */                                         \
    RS_TOKEN_KEYWORD(FN_TOK, "fn")                                                            \
    RS_TOKEN_KEYWORD(FOR, "for")                                                              \
    RS_TOKEN_KEYWORD(IF, "if")                                                                \
    RS_TOKEN_KEYWORD(IMPL, "impl")                                                            \
    RS_TOKEN_KEYWORD(IN, "in")                                                                \
    RS_TOKEN_KEYWORD(LET, "let")                                                              \
    RS_TOKEN_KEYWORD(LOOP, "loop")                                                            \
    RS_TOKEN_KEYWORD(MACRO, "macro") /* unused */                                             \
    RS_TOKEN_KEYWORD(MATCH_TOK, "match")                                                      \
    RS_TOKEN_KEYWORD(MOD, "mod")                                                              \
    RS_TOKEN_KEYWORD(MOVE, "move")                                                            \
    RS_TOKEN_KEYWORD(MUT, "mut")                                                              \
    RS_TOKEN_KEYWORD(OVERRIDE_TOK, "override") /* unused */                                   \
    RS_TOKEN_KEYWORD(PRIV, "priv")             /* unused */                                   \
    RS_TOKEN_KEYWORD(PUB, "pub")                                                              \
    RS_TOKEN_KEYWORD(REF, "ref")                                                              \
    RS_TOKEN_KEYWORD(RETURN_TOK, "return")                                                    \
    RS_TOKEN_KEYWORD(SELF_ALIAS, "Self") /* mrustc does not treat this as a reserved word*/   \
    RS_TOKEN_KEYWORD(SELF, "self")                                                            \
    RS_TOKEN_KEYWORD(STATIC_TOK, "static")                                                    \
    RS_TOKEN_KEYWORD(STRUCT_TOK, "struct")                                                    \
    RS_TOKEN_KEYWORD(SUPER, "super")                                                          \
    RS_TOKEN_KEYWORD(TRAIT, "trait")                                                          \
    RS_TOKEN_KEYWORD(TRUE_LITERAL, "true")                                                    \
    RS_TOKEN_KEYWORD(TRY, "try") /* unused */                                                 \
    RS_TOKEN_KEYWORD(TYPE, "type")                                                            \
    RS_TOKEN_KEYWORD(TYPEOF, "typeof") /* unused */                                           \
    RS_TOKEN_KEYWORD(UNSAFE, "unsafe")                                                        \
    RS_TOKEN_KEYWORD(UNSIZED, "unsized") /* unused */                                         \
    RS_TOKEN_KEYWORD(USE, "use")                                                              \
    RS_TOKEN_KEYWORD(VIRTUAL, "virtual") /* unused */                                         \
    RS_TOKEN_KEYWORD(WHERE, "where")                                                          \
    RS_TOKEN_KEYWORD(WHILE, "while")                                                          \
    RS_TOKEN_KEYWORD(YIELD, "yield") /* unused */                                             \
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
    typedef ::std::shared_ptr<Token> TokenPtr;
    // A smart pointer (shared_ptr) to a constant Token.
    typedef ::std::shared_ptr<const Token> const_TokenPtr;

    // Hackily defined way to get token description for enum value using x-macros
    const char* get_token_description(TokenId id);
    // Hackily defined way to get token description as a string for enum value using x-macros
    const char* token_id_to_str(TokenId id);
    // Get type hint description as a string.
    const char* get_type_hint_string(PrimitiveCoreType type);

    // Represents a single token. Create using factory static methods.
    class Token {
      private:
        // Token kind.
        TokenId token_id;
        // Token location.
        location_t locus;
        // Associated text (if any) of token.
        ::std::string* str;
        // Type hint for token based on lexer data (e.g. type suffix). Does not exist for most tokens.
        PrimitiveCoreType type_hint;

        // Token constructor from token id and location. Has a null string.
        Token(TokenId token_id, location_t location) :
          token_id(token_id), locus(location), str(NULL), type_hint(CORETYPE_UNKNOWN) {}

        // Token constructor from token id, location, and a string.
        Token(TokenId token_id, location_t location, const ::std::string& paramStr) :
          token_id(token_id), locus(location), str(new ::std::string(paramStr)),
          type_hint(CORETYPE_UNKNOWN) {}

        // Token constructor from token id, location, and a char.
        Token(TokenId token_id, location_t location, char paramChar) :
          token_id(token_id), locus(location), str(new ::std::string(1, paramChar)),
          type_hint(CORETYPE_UNKNOWN) {}

        // Token constructor from token id, location, and a "codepoint".
        Token(TokenId token_id, location_t location, Codepoint paramCodepoint) :
          token_id(token_id), locus(location), str(new ::std::string(paramCodepoint.as_string())),
          type_hint(CORETYPE_UNKNOWN) {}

        // Token constructor from token id, location, a string, and type hint.
        Token(TokenId token_id, location_t location, const ::std::string& paramStr,
          PrimitiveCoreType parType) :
          token_id(token_id),
          locus(location), str(new ::std::string(paramStr)), type_hint(parType) {}

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
        static TokenPtr make_identifier(location_t locus, const ::std::string& str) {
            return TokenPtr(new Token(IDENTIFIER, locus, str));
        }

        // Makes and returns a new TokenPtr of type INT_LITERAL.
        static TokenPtr make_int(location_t locus, const ::std::string& str) {
            return TokenPtr(new Token(INT_LITERAL, locus, str));
        }

        // Makes and returns a new TokenPtr of type INT_LITERAL.
        static TokenPtr make_int(
          location_t locus, const ::std::string& str, PrimitiveCoreType type_hint) {
            return TokenPtr(new Token(INT_LITERAL, locus, str, type_hint));
        }

        // Makes and returns a new TokenPtr of type FLOAT_LITERAL.
        static TokenPtr make_float(location_t locus, const ::std::string& str) {
            return TokenPtr(new Token(FLOAT_LITERAL, locus, str));
        }

        // Makes and returns a new TokenPtr of type FLOAT_LITERAL.
        static TokenPtr make_float(
          location_t locus, const ::std::string& str, PrimitiveCoreType type_hint) {
            return TokenPtr(new Token(FLOAT_LITERAL, locus, str, type_hint));
        }

        // Makes and returns a new TokenPtr of type STRING_LITERAL.
        static TokenPtr make_string(location_t locus, const ::std::string& str) {
            return TokenPtr(new Token(STRING_LITERAL, locus, str, CORETYPE_STR));
        }

        // Makes and returns a new TokenPtr of type CHAR_LITERAL (fix).
        static TokenPtr make_char(location_t locus, Codepoint char_lit) {
            return TokenPtr(new Token(CHAR_LITERAL, locus, char_lit));
        }

        // Makes and returns a new TokenPtr of type BYTE_CHAR_LITERAL (fix).
        static TokenPtr make_byte_char(location_t locus, char byte_char) {
            return TokenPtr(new Token(BYTE_CHAR_LITERAL, locus, byte_char));
        }

        // Makes and returns a new TokenPtr of type BYTE_STRING_LITERAL (fix).
        static TokenPtr make_byte_string(location_t locus, const ::std::string& str) {
            return TokenPtr(new Token(BYTE_STRING_LITERAL, locus, str));
        }

        // Makes and returns a new TokenPtr of type LIFETIME.
        static TokenPtr make_lifetime(location_t locus, const ::std::string& str) {
            return TokenPtr(new Token(LIFETIME, locus, str));
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
        const ::std::string& get_str() const; /*{
            // FIXME: put in header again when fix null problem
            //gcc_assert(str != NULL);
            if (str == NULL) {
                error_at(get_locus(), "attempted to get string for '%s', which has no string. returning empty string instead.", get_token_description());
                return "";
            }
            return *str;
        }*/

        // Gets token's type hint info.
        PrimitiveCoreType get_type_hint() const {
            return type_hint;
        }

        // diagnostics (error reporting)
        const char* get_token_description() const {
            return Rust::get_token_description(token_id);
        }

        // debugging
        const char* token_id_to_str() const {
            return Rust::token_id_to_str(token_id);
        }

        // debugging
        const char* get_type_hint_str() const;

        /* Returns whether the token is a literal of any type (int, float, char, string, byte char, 
         * byte string). */
        inline bool is_literal() const {
            switch (token_id) {
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

        // Returns whether the token actually has a string (regardless of whether it should or not).
        inline bool has_str() const {
            return str != NULL;
        }

        // Returns whether the token should have a string.
        inline bool should_have_str() const {
            return is_literal() || token_id == IDENTIFIER || token_id == LIFETIME;
        }
    };
}

#endif