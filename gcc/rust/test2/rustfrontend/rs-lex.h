// rs-lex.h -- Rust frontend lexer.     -*- C++ -*-

// Copyright etc

#ifndef RUST_LEX_H
#define RUST_LEX_H

#include <mpfr.h>

#include "rs-token.h"
//#include "go-linemap.h"

// Operators
enum Operator {
    OPERATOR_INVALID,
    OPERATOR_DOUBLE_PIPE,   // ||
    OPERATOR_DOUBLE_AMP,    // &&
    OPERATOR_DOUBLE_EQ,     // ==
    OPERATOR_EXCLAM_EQ,     // !=
    OPERATOR_LT,            // <
    OPERATOR_LE,            // <=
    OPERATOR_GT,            // >
    OPERATOR_GE,            // >=
    OPERATOR_PLUS,          // +
    OPERATOR_MINUS,         // -
    OPERATOR_PIPE,          // |
    OPERATOR_CARET,         // ^
    OPERATOR_MULT,          // *
    OPERATOR_DIV,           // /
    OPERATOR_PERCENT,       // %
    OPERATOR_LSHIFT,        // <<
    OPERATOR_RSHIFT,        // >>
    OPERATOR_AMP,           // &
    OPERATOR_EXCLAM,        // !
    //OPERATOR_BITCLEAR,    // &^
    OPERATOR_LEFT_ARROW,    // <-
    OPERATOR_RIGHT_ARROW,   // ->

    OPERATOR_EQ,            // =
    OPERATOR_PLUS_EQ,       // +=
    OPERATOR_MINUS_EQ,      // -=
    OPERATOR_PIPE_EQ,       // |=
    OPERATOR_CARET_EQ,      // ^=
    OPERATOR_MULT_EQ,       // *=
    OPERATOR_DIV_EQ,        // /=
    OPERATOR_PERCENT_EQ,    // %=
    OPERATOR_LSHIFTEQ,      // <<=
    OPERATOR_RSHIFTEQ,      // >>=
    OPERATOR_AMP_EQ,        // &=
    //OPERATOR_BITCLEAREQ,  // &^=
    //OPERATOR_PLUSPLUS,    // ++
    //OPERATOR_MINUSMINUS,  // --

    OPERATOR_COLON,         // :
    OPERATOR_DOUBLE_COLON,  // ::
    //OPERATOR_COLONEQ,     // :=
    OPERATOR_SEMICOLON,     // ;
    OPERATOR_DOT,           // .
    OPERATOR_DOUBLE_DOT,    // ..
    OPERATOR_ELLIPSIS,      // ...
    OPERATOR_COMMA,         // ,
    OPERATOR_LPAREN,        // (
    OPERATOR_RPAREN,        // )
    OPERATOR_LCURLY,        // {
    OPERATOR_RCURLY,        // }
    OPERATOR_LSQUARE,       // [
    OPERATOR_RSQUARE,       // ]
    OPERATOR_TILDE,         // ~
    OPERATOR_HASH,          // #
    OPERATOR_DOUBLE_QUOTE,  // "
    OPERATOR_SINGLE_QUOTE,  // '
    OPERATOR_DOLLAR,        // $
    OPERATOR_FSLASH,        // /
    OPERATOR_BACKSLASH,     /* \ */
    OPERATOR_BLOCK_COMMENT, // /*
    OPERATOR_LINE_COMMENT,  // //
    OPERATOR_QMARK,         // ?
    OPERATOR_AT,            // @
    OPERATOR_BACKTICK       // `
};

struct Unicode_range;

// The keywords.  These must be in sorted order, other than
// KEYWORD_INVALID.  They must match the Keywords::mapping_ array in
// lex.cc.

enum Keyword {
    KEYWORD_INVALID,    // Not a keyword.
    KEYWORD_UNDERSCORE, // _
    KEYWORD_ABSTRACT,   // abstract
    KEYWORD_ALIGNOF,    // alignof
    KEYWORD_AS,         // as
    KEYWORD_BE,         // be
    KEYWORD_BOX,        // box
    KEYWORD_BREAK,      // break
    KEYWORD_CONST,      // const
    KEYWORD_CONTINUE,   // continue
    KEYWORD_CRATE,      // crate
    KEYWORD_DO,         // do
    KEYWORD_ELSE,       // else
    KEYWORD_ENUM,       // enum
    KEYWORD_EXTERN,     // extern
    KEYWORD_FALSE,      // false
    KEYWORD_FINAL,      // final
    KEYWORD_FN,         // fn
    KEYWORD_FOR,        // for
    KEYWORD_IF,         // if
    KEYWORD_IMPL,       // impl
    KEYWORD_IN,         // in
    KEYWORD_LET,        // let
    KEYWORD_LOOP,       // loop
    KEYWORD_MATCH,      // match
    KEYWORD_MOD,        // mod
    KEYWORD_MOVE,       // move
    KEYWORD_MUT,        // mut
    KEYWORD_OFFSETOF,   // offsetof
    KEYWORD_OVERRIDE,   // override
    KEYWORD_PRIV,       // priv
    KEYWORD_PROC,       // proc
    KEYWORD_PUB,        // pub
    KEYWORD_PURE,        // pure
    KEYWORD_REF,        // ref
    KEYWORD_RETURN,     // return
    KEYWORD_SELF,       // self
    KEYWORD_SIZEOF,     // sizeof
    KEYWORD_STATIC,     // static
    KEYWORD_STRUCT,     // struct
    KEYWORD_SUPER,      // super
    KEYWORD_TRAIT,      // trait
    KEYWORD_TRUE,       // true
    KEYWORD_TYPE,       // type
    KEYWORD_TYPEOF,     // typeof
    KEYWORD_UNSAFE,     // unsafe
    KEYWORD_UNSIZED,    // unsized
    KEYWORD_USE,        // use
    KEYWORD_VIRTUAL,    // virtual
    KEYWORD_WHERE,      // where
    KEYWORD_WHILE,      // while
    KEYWORD_YIELD       // yield
};

// Pragmas built from magic comments and recorded for functions.
// These are used as bits in a bitmask.
// The set of values is intended to be the same as the gc compiler.

// I doubt these are in rust
/* enum RustPragma {
    RUSTPRAGMA_NOINTERFACE = 1 << 0,        // Method not in type descriptor.
    RUSTPRAGMA_NOESCAPE = 1 << 1,           // Args do not escape.
    RUSTPRAGMA_NORACE = 1 << 2,             // No race detector.
    RUSTPRAGMA_NOSPLIT = 1 << 3,            // Do not split stack.
    RUSTPRAGMA_NOINLINE = 1 << 4,           // Do not inline.
    RUSTPRAGMA_SYSTEMSTACK = 1 << 5,        // Must run on system stack.
    RUSTPRAGMA_NOWRITEBARRIER = 1 << 6,     // No write barriers.
    RUSTPRAGMA_NOWRITEBARRIERREC = 1 << 7,  // No write barriers here or callees.
    RUSTPRAGMA_YESWRITEBARRIERREC = 1 << 8, // Stops nowritebarrierrec.
    RUSTPRAGMA_MARK = 1 << 9,               // Marker for nowritebarrierrec.
    RUSTPRAGMA_CGOUNSAFEARGS = 1 << 10,     // Pointer to arg is pointer to all.
    RUSTPRAGMA_UINTPTRESCAPES = 1 << 11,    // uintptr(p) escapes.
    RUSTPRAGMA_NOTINHEAP = 1 << 12          // type is not in heap.
};*/

// The lexer itself.

class Lex {
  public:
    Lex(const char* input_file_name, FILE* input_file, Linemap* linemap);

    ~Lex();

    // Return the next token.
    Token
    next_token();

    // Return the contents of any current //extern comment.
    const std::string&
    extern_name() const { return this->extern_; }

    // Return the current set of pragmas, and clear them.
    unsigned int
    get_and_clear_pragmas() {
        unsigned int ret = this->pragmas_;
        this->pragmas_ = 0;
        return ret;
    }

    struct Linkname {
        std::string ext_name; // External name.
        bool is_exported;     // Whether the internal name is exported.
        Location loc;         // Location of go:linkname directive.

        Linkname()
          : ext_name()
          , is_exported(false)
          , loc() {}

        Linkname(const std::string& ext_name_a, bool is_exported_a, Location loc_a)
          : ext_name(ext_name_a)
          , is_exported(is_exported_a)
          , loc(loc_a) {}
    };

    typedef std::map<std::string, Linkname> Linknames;

    // Return the linknames seen so far, or NULL if none, and clear the
    // set.  These are from go:linkname compiler directives.
    Linknames*
    get_and_clear_linknames() {
        Linknames* ret = this->linknames_;
        this->linknames_ = NULL;
        return ret;
    }

    // Return whether the identifier NAME should be exported.  NAME is a
    // mangled name which includes only ASCII characters.
    static bool
    is_exported_mangled_name(const std::string& name);

    // Return whether the identifier NAME should be exported.  NAME is
    // an unmangled utf-8 string and may contain non-ASCII characters.
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
    append_char(unsigned int v, bool is_charater, std::string* str, Location);

    // A helper function.  Fetch a UTF-8 character from STR and store it
    // in *VALUE.  Return the number of bytes read from STR.  Return 0
    // if STR does not point to a valid UTF-8 character.
    static int
    fetch_char(const char* str, unsigned int* value);

    // Return whether C is a Unicode or "C" locale space character.
    static bool
    is_unicode_space(unsigned int c);

    // Convert the specified hex char into an unsigned integer value.
    static unsigned
    hex_val(char c);

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
    octal_value(char c) { return c - '0'; }

    Token
    make_invalid_token() { return Token::make_invalid_token(this->location()); }

    Token
    make_eof_token() { return Token::make_eof_token(this->location()); }

    Token
    make_operator(Operator op, int chars) { return Token::make_operator_token(op, this->earlier_location(chars)); }

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
    is_in_unicode_range(unsigned int C, const Unicode_range* ranges, size_t range_size);

    Operator
    three_character_operator(char, char, char);

    Operator
    two_character_operator(char, char);

    Operator
    one_character_operator(char);

    bool
    skip_c_comment(bool* found_newline);

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
    // Pragmas for the next function, from magic comments.
    unsigned int pragmas_;
    // The external name to use for a function declaration, from a magic
    // //extern comment.
    std::string extern_;
    // The list of //go:linkname comments, if any.
    Linknames* linknames_;
};

#endif // !defined(GO_LEX_H)
