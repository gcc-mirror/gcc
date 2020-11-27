#include "rust-lex.h"

#include "diagnostic.h" // for error_at
#include "safe-ctype.h"

#include <sstream> // for ostringstream

namespace Rust {
    // TODO: move to separate compilation unit?
    // overload += for uint32_t to allow 32-bit encoded utf-8 to be added
    ::std::string& operator+=(::std::string& str, Codepoint char32) {
        if (char32.value < 0x80) {
            str += static_cast<char>(char32.value);
        } else if (char32.value < (0x1F + 1) << (1 * 6)) {
            str += static_cast<char>(0xC0 | ((char32.value >> 6) & 0x1F));
            str += static_cast<char>(0x80 | ((char32.value >> 0) & 0x3F));
        } else if (char32.value < (0x0F + 1) << (2 * 6)) {
            str += static_cast<char>(0xE0 | ((char32.value >> 12) & 0x0F));
            str += static_cast<char>(0x80 | ((char32.value >> 6) & 0x3F));
            str += static_cast<char>(0x80 | ((char32.value >> 0) & 0x3F));
        } else if (char32.value < (0x07 + 1) << (3 * 6)) {
            str += static_cast<char>(0xF0 | ((char32.value >> 18) & 0x07));
            str += static_cast<char>(0x80 | ((char32.value >> 12) & 0x3F));
            str += static_cast<char>(0x80 | ((char32.value >> 6) & 0x3F));
            str += static_cast<char>(0x80 | ((char32.value >> 0) & 0x3F));
        } else {
            fprintf(stderr, "Invalid unicode codepoint found: '%u' \n", char32.value);
            // error_at(get_current_location(), "Invalid unicode codepoint found: '%u'",
            // char32.value);
        }
        return str;
    }

    ::std::string Codepoint::as_string() {
        std::string str;

        // do i need to do this? or can i just do str += value due to op overloading?

        // ok can't figure out how to just convert to codepoint or use "this" so create new one
        str += Codepoint(value);

        /*if (value < 0x80) {
            str += static_cast<char>(value);
        } else if (value < (0x1F + 1) << (1 * 6)) {
            str += static_cast<char>(0xC0 | ((value >> 6) & 0x1F));
            str += static_cast<char>(0x80 | ((value >> 0) & 0x3F));
        } else if (value < (0x0F + 1) << (2 * 6)) {
            str += static_cast<char>(0xE0 | ((value >> 12) & 0x0F));
            str += static_cast<char>(0x80 | ((value >> 6) & 0x3F));
            str += static_cast<char>(0x80 | ((value >> 0) & 0x3F));
        } else if (value < (0x07 + 1) << (3 * 6)) {
            str += static_cast<char>(0xF0 | ((value >> 18) & 0x07));
            str += static_cast<char>(0x80 | ((value >> 12) & 0x3F));
            str += static_cast<char>(0x80 | ((value >> 6) & 0x3F));
            str += static_cast<char>(0x80 | ((value >> 0) & 0x3F));
        } else {
            error_at(get_current_location(), "Invalid unicode codepoint found: '%u'", value);
        }*/
        return str;
    }

    // Includes all allowable float digits EXCEPT _ and . as that needs lookahead for handling.
    inline bool is_float_digit(char number) {
        return ISDIGIT(number) || number == 'E' || number == 'e';
    }

    // Basically ISXDIGIT from safe-ctype but may change if Rust's encoding or whatever is different
    inline bool is_x_digit(char number) {
        return ISXDIGIT(number);
    }

    inline bool is_octal_digit(char number) {
        return number >= '0' && number <= '7';
    }

    inline bool is_bin_digit(char number) {
        return number == '0' || number == '1';
    }

    inline bool check_valid_float_dot_end(char character) {
        return character != '.' && character != '_' && !ISALPHA(character);
    }

    // ISSPACE from safe-ctype but may change in future
    inline bool is_whitespace(char character) {
        return ISSPACE(character);
    }

    Lexer::Lexer(const char* filename, FILE* input) :
      input(input), current_line(1), current_column(1), line_map(0), input_source(input),
      input_queue(input_source), token_source(this), token_queue(token_source) {
        // inform line_table that file is being entered and is in line 1
        line_map
          = ::linemap_add(::line_table, ::LC_ENTER, /* sysp */ 0, filename, /* current_line */ 1);
    }

    Lexer::~Lexer() {
        ::linemap_add(::line_table, ::LC_LEAVE, /* sysp */ 0, /* filename */ NULL, /* to_line */ 0);
    }

    location_t Lexer::get_current_location() {
        return ::linemap_position_for_column(::line_table, current_column);
    }

    int Lexer::peek_input(int n) {
        return input_queue.peek(n);
    }

    int Lexer::peek_input() {
        return peek_input(0);
    }

    void Lexer::skip_input(int n) {
        input_queue.skip(n);
    }

    void Lexer::skip_input() {
        skip_input(0);
    }

    const_TokenPtr Lexer::peek_token(int n) {
        return token_queue.peek(n);
    }

    const_TokenPtr Lexer::peek_token() {
        return peek_token(0);
    }

    void Lexer::skip_token(int n) {
        token_queue.skip(n);
    }

    void Lexer::skip_token() {
        skip_token(0);
    }

    /* shitty anonymous namespace that can only be accessed inside the compilation unit - used for
     * classify_keyword
     * Binary search in sorted array of keywords created with x-macros. */
    namespace {
        const std::string keyword_index[] = {
#define RS_TOKEN(x, y)
#define RS_TOKEN_KEYWORD(name, keyword) keyword,
            RS_TOKEN_LIST
#undef RS_TOKEN_KEYWORD
#undef RS_TOKEN
        };

        TokenId keyword_keys[] = {
#define RS_TOKEN(x, y)
#define RS_TOKEN_KEYWORD(name, keyword) name,
            RS_TOKEN_LIST
#undef RS_TOKEN_KEYWORD
#undef RS_TOKEN
        };

        const int num_keywords = sizeof(keyword_index) / sizeof(*keyword_index);
    }

    /* Determines whether the string passed in is a keyword or not. If it is, it returns the keyword
     * name.  */
    TokenId Lexer::classify_keyword(const std::string& str) {
        const std::string* last = keyword_index + num_keywords;
        const std::string* idx = std::lower_bound(keyword_index, last, str);

        if (idx == last || str != *idx) {
            return IDENTIFIER;
        } else {
            return keyword_keys[idx - keyword_index];
        }
    }

    TokenPtr Lexer::build_token() {
        // loop to go through multiple characters to build a single token
        while (true) {
            location_t loc = get_current_location();
            /*int */ current_char = peek_input();
            skip_input();

            // return end of file token if end of file
            if (current_char == EOF) {
                return Token::make(END_OF_FILE, loc);
            }

            // detect shebang
            if (loc == 1 && current_line == 1 && current_char == '#') {
                current_char = peek_input();

                if (current_char == '!') {
                    skip_input();
                    current_char = peek_input();

                    switch (current_char) {
                        case '/':
                            // shebang

                            skip_input();

                            // ignore rest of line
                            while (current_char != '\n') {
                                current_char = peek_input();
                                skip_input();
                            }

                            // newline
                            current_line++;
                            current_column = 1;
                            // tell line_table that new line starts
                            linemap_line_start(::line_table, current_line, max_column_hint);
                            continue;
                    }
                }
            }

            // if not end of file, start tokenising
            switch (current_char) {
                // ignore whitespace characters for tokens but continue updating location
                case '\n': // newline
                    current_line++;
                    current_column = 1;
                    // tell line_table that new line starts
                    linemap_line_start(::line_table, current_line, max_column_hint);
                    continue;
                case ' ': // space
                    current_column++;
                    continue;
                case '\t': // tab
                    // width of a tab is not well-defined, assume 8 spaces
                    current_column += 8;
                    continue;

                // punctuation - actual tokens
                case '=':
                    if (peek_input() == '>') {
                        // match arm arrow
                        skip_input();
                        current_column += 2;

                        return Token::make(MATCH_ARROW, loc);
                    } else if (peek_input() == '=') {
                        // equality operator
                        skip_input();
                        current_column += 2;

                        return Token::make(EQUAL_EQUAL, loc);
                    } else {
                        // assignment operator
                        current_column++;
                        return Token::make(EQUAL, loc);
                    }
                case '(':
                    current_column++;
                    return Token::make(LEFT_PAREN, loc);
                case '-':
                    if (peek_input() == '>') {
                        // return type specifier
                        skip_input();
                        current_column += 2;

                        return Token::make(RETURN_TYPE, loc);
                    } else if (peek_input() == '=') {
                        // minus-assign
                        skip_input();
                        current_column += 2;

                        return Token::make(MINUS_EQ, loc);
                    } else {
                        // minus
                        current_column++;
                        return Token::make(MINUS, loc);
                    }
                case '+':
                    if (peek_input() == '=') {
                        // add-assign
                        skip_input();
                        current_column += 2;

                        return Token::make(PLUS_EQ, loc);
                    } else {
                        // add
                        current_column++;
                        return Token::make(PLUS, loc);
                    }
                case ')':
                    current_column++;
                    return Token::make(RIGHT_PAREN, loc);
                case ';':
                    current_column++;
                    return Token::make(SEMICOLON, loc);
                case '*':
                    if (peek_input() == '=') {
                        // multiplication-assign
                        skip_input();
                        current_column += 2;

                        return Token::make(ASTERISK_EQ, loc);
                    } else {
                        // multiplication
                        current_column++;
                        return Token::make(ASTERISK, loc);
                    }
                case ',':
                    current_column++;
                    return Token::make(COMMA, loc);
                case '/':
                    if (peek_input() == '=') {
                        // division-assign
                        skip_input();
                        current_column += 2;

                        return Token::make(DIV_EQ, loc);
                    } else if (peek_input() == '/') {
                        // TODO: single-line doc comments

                        // single line comment
                        skip_input();
                        current_column += 2;

                        // basically ignore until line finishes
                        while (current_char != '\n' && current_char != EOF) {
                            skip_input();
                            current_column++; // not used
                            current_char = peek_input();
                        }
                        continue;
                        break;
                    } else if (peek_input() == '*') {
                        // block comment
                        skip_input();
                        current_column += 2;

                        // TODO: block doc comments

                        current_char = peek_input();

                        int level = 1;
                        while (level > 0) {
                            skip_input();
                            current_column++; // for error-handling
                            current_char = peek_input();

                            // if /* found
                            if (current_char == '/') {
                                if (peek_input(1) == '*') {
                                    // skip /* characters
                                    skip_input(1);

                                    current_column += 2;

                                    level += 1;
                                }
                            }

                            // ignore until */ is found
                            if (current_char == '*') {
                                if (peek_input(1) == '/') {
                                    // skip */ characters
                                    skip_input(1);

                                    current_column += 2;
                                    // should only break inner loop here - seems to do so
                                    // break;

                                    level -= 1;
                                }
                            }
                        }

                        // refresh new token
                        continue;
                        break;
                    } else {
                        // division
                        current_column++;
                        return Token::make(DIV, loc);
                    }
                case '%':
                    if (peek_input() == '=') {
                        // modulo-assign
                        current_column += 2;
                        return Token::make(PERCENT_EQ, loc);
                    } else {
                        // modulo
                        current_column++;
                        return Token::make(PERCENT, loc);
                    }
                case '^':
                    if (peek_input() == '=') {
                        // xor-assign?
                        current_column += 2;
                        return Token::make(CARET_EQ, loc);
                    } else {
                        // xor?
                        current_column++;
                        return Token::make(CARET, loc);
                    }
                case '<':
                    if (peek_input() == '<') {
                        if (peek_input(1) == '=') {
                            // left-shift assign
                            skip_input(1);
                            current_column += 3;

                            return Token::make(LEFT_SHIFT_EQ, loc);
                        } else {
                            // left-shift
                            skip_input();
                            current_column += 2;

                            return Token::make(LEFT_SHIFT, loc);
                        }
                    } else if (peek_input() == '=') {
                        // smaller than or equal to
                        skip_input();
                        current_column += 2;

                        return Token::make(LESS_OR_EQUAL, loc);
                    } else {
                        // smaller than
                        current_column++;
                        return Token::make(LEFT_ANGLE, loc);
                    }
                    break;
                case '>':
                    if (peek_input() == '>') {
                        if (peek_input(1) == '=') {
                            // right-shift-assign
                            skip_input(1);
                            current_column += 3;

                            return Token::make(RIGHT_SHIFT_EQ, loc);
                        } else {
                            // right-shift
                            skip_input();
                            current_column += 2;

                            return Token::make(RIGHT_SHIFT, loc);
                        }
                    } else if (peek_input() == '=') {
                        // larger than or equal to
                        skip_input();
                        current_column += 2;

                        return Token::make(GREATER_OR_EQUAL, loc);
                    } else {
                        // larger than
                        current_column++;
                        return Token::make(RIGHT_ANGLE, loc);
                    }
                case ':':
                    if (peek_input() == ':') {
                        // scope resolution ::
                        skip_input();
                        current_column += 2;

                        return Token::make(SCOPE_RESOLUTION, loc);
                    } else {
                        // single colon :
                        current_column++;
                        return Token::make(COLON, loc);
                    }
                case '!':
                    // no special handling for macros in lexer?
                    if (peek_input() == '=') {
                        // not equal boolean operator
                        skip_input();
                        current_column += 2;

                        return Token::make(NOT_EQUAL, loc);
                    } else {
                        // not equal unary operator
                        current_column++;

                        return Token::make(EXCLAM, loc);
                    }
                case '?':
                    current_column++;
                    return Token::make(QUESTION_MARK, loc);
                case '#':
                    current_column++;
                    return Token::make(HASH, loc);
                case '[':
                    current_column++;
                    return Token::make(LEFT_SQUARE, loc);
                case ']':
                    current_column++;
                    return Token::make(RIGHT_SQUARE, loc);
                case '{':
                    current_column++;
                    return Token::make(LEFT_CURLY, loc);
                case '}':
                    current_column++;
                    return Token::make(RIGHT_CURLY, loc);
                case '@':
                    // TODO: i don't know what this does, does it need special handling?
                    current_column++;
                    return Token::make(PATTERN_BIND, loc);
                case '$':
                    // TODO: i don't know what this does, does it need special handling?
                    current_column++;
                    return Token::make(DOLLAR_SIGN, loc);
                case '~':
                    // TODO: i don't know what this does, does it need special handling?
                    current_column++;
                    return Token::make(TILDE, loc);
                case '\\':
                    // TODO: i don't know what this does, does it need special handling?
                    current_column++;
                    return Token::make(BACKSLASH, loc);
                case '`':
                    // TODO: i don't know what this does, does it need special handling?
                    current_column++;
                    return Token::make(BACKTICK, loc);
                case '|':
                    if (peek_input() == '=') {
                        // bitwise or-assign?
                        skip_input();
                        current_column += 2;

                        return Token::make(PIPE_EQ, loc);
                    } else if (peek_input() == '|') {
                        // logical or
                        skip_input();
                        current_column += 2;

                        return Token::make(OR, loc);
                    } else {
                        // bitwise or
                        current_column++;

                        return Token::make(PIPE, loc);
                    }
                case '&':
                    if (peek_input() == '=') {
                        // bitwise and-assign?
                        skip_input();
                        current_column += 2;

                        return Token::make(AMP_EQ, loc);
                    } else if (peek_input() == '&') {
                        // logical and
                        skip_input();
                        current_column += 2;

                        return Token::make(LOGICAL_AND, loc);
                    } else {
                        // bitwise and/reference
                        current_column++;

                        return Token::make(AMP, loc);
                    }
                case '.':
                    if (peek_input() == '.') {
                        if (peek_input(1) == '.') {
                            // ellipsis
                            skip_input(1);
                            current_column += 3;

                            return Token::make(ELLIPSIS, loc);
                        } else if (peek_input(1) == '=') {
                            // ..=
                            skip_input(1);
                            current_column += 3;

                            return Token::make(DOT_DOT_EQ, loc);
                        } else {
                            // ..
                            skip_input();
                            current_column += 2;

                            return Token::make(DOT_DOT, loc);
                        }
                    } else if (!ISDIGIT(peek_input())) {
                        // single dot .
                        // Only if followed by a non-number
                        current_column++;
                        return Token::make(DOT, loc);
                    }
            }
            // TODO: special handling of _ in the lexer? instead of being identifier

            // byte and byte string test
            if (current_char == 'b') {
                if (peek_input() == '\'') {
                    // byte - allows any ascii or escapes
                    // would also have to take into account escapes: \x hex_digit hex_digit,
                    // \n, \r, \t, \\, \0

                    int length = 1;

                    // char to save
                    char byte_char;

                    skip_input();
                    // make current char the next character
                    current_char = peek_input();

                    // detect escapes
                    if (current_char == '\\') {
                        /*skip_input();

                        // make current_char next character (letter)
                        current_char = peek_input();*/

                        parse_escape(length, byte_char, '\'');

                        if (byte_char > 127) {
                            error_at(
                              get_current_location(), "byte char '%c' out of range", byte_char);
                            byte_char = 0;
                        }

                        // skip_input();
                        current_char = peek_input();
                        length++;

                        if (current_char != '\'') {
                            error_at(get_current_location(), "unclosed byte char");
                        }

                        // TODO: ensure skipping is needed here
                        skip_input();
                        current_char = peek_input();
                        length++; // go to next char
                    } else if (current_char != '\'') {
                        // otherwise, get character from direct input character
                        byte_char = current_char;

                        skip_input();
                        current_char = peek_input();

                        if (current_char != '\'') {
                            error_at(get_current_location(), "unclosed byte char");
                        }

                        // TODO: ensure skipping is needed here
                        skip_input();
                        current_char = peek_input();
                        length++; // go to next char
                    } else {
                        error_at(get_current_location(), "no character inside '' for byte char");
                    }

                    current_column += length;

                    return Token::make_byte_char(loc, byte_char);
                } else if (peek_input() == '"') {
                    // byte string

                    // skip quote character
                    skip_input();

                    std::string str;
                    str.reserve(16); // some sensible default

                    int length = 1;
                    current_char = peek_input();
                    // TODO: handle escapes properly

                    while (current_char != '"' && current_char != '\n') {
                        if (current_char == '\\') {
                            char output_char = 0;
                            parse_escape(length, output_char, '"');

                            if (output_char > 127) {
                                error_at(get_current_location(),
                                  "char '%c' in byte string out of range", output_char);
                                output_char = 0;
                            }

                            str += output_char;

                            continue;
                        }

                        length++;

                        str += current_char;
                        skip_input();
                        current_char = peek_input();
                    }

                    current_column += length;

                    if (current_char == '\n') {
                        error_at(get_current_location(), "unended byte string literal");
                    } else if (current_char == '"') {
                        skip_input();
                        current_char = peek_input();
                    } else {
                        gcc_unreachable();
                    }

                    return Token::make_byte_string(loc, str);
                    // TODO: ensure escapes and string continue work properly
                } else if (peek_input() == 'r' && (peek_input(1) == '#' || peek_input(1) == '"')) {
                    // raw byte string literals
                    std::string str;
                    str.reserve(16); // some sensible default

                    int length = 1;
                    int hash_count = 0;

                    // get hash count at beginnning
                    skip_input();
                    current_char = peek_input();
                    while (current_char == '#') {
                        hash_count++;
                        length++;

                        skip_input();
                        current_char = peek_input();
                    }

                    if (current_char != '"') {
                        error_at(get_current_location(), "raw byte string has no opening '\"'");
                    }

                    skip_input();
                    current_char = peek_input();

                    while (true) {
                        if (current_char == '"') {
                            bool enough_hashes = true;

                            for (int i = 0; i < hash_count; i++) {
                                if (peek_input(i + 1) != '#') {
                                    enough_hashes
                                      = false; // could continue here - improve performance
                                }
                            }

                            if (enough_hashes) {
                                // skip enough input and peek enough input
                                skip_input(hash_count); // is this enough?
                                current_char = peek_input();
                                length += hash_count + 1;
                                break;
                            }
                        }

                        length++;

                        str += current_char;
                        skip_input();
                        current_char = peek_input();
                    }

                    current_column += length;

                    return Token::make_byte_string(loc, str); // TODO: does this work properly
                }
            }

            // raw stuff
            if (current_char == 'r') {
                int peek = peek_input();
                int peek1 = peek_input(1);

                if (peek == '#' && (ISALPHA(peek1) || peek1 == '_')) {
                    // raw identifier
                    std::string str;
                    str.reserve(16); // default

                    skip_input();
                    current_char = peek_input();

                    current_column += 2;

                    str += current_char;

                    bool first_is_underscore = current_char == '_';

                    int length = 1;
                    current_char = peek_input();
                    // loop through entire name
                    while (ISALPHA(current_char) || ISDIGIT(current_char) || current_char == '_') {
                        length++;

                        str += current_char;
                        skip_input();
                        current_char = peek_input();
                    }

                    current_column += length;

                    // if just a single underscore, not an identifier
                    if (first_is_underscore && length == 1) {
                        error_at(get_current_location(), "'_' is not a valid raw identifier");
                    }

                    if (str == "crate" || str == "extern" || str == "self" || str == "super"
                        || str == "Self") {
                        error_at(
                          get_current_location(), "'%s' is a forbidden raw identifier", str.c_str());
                    } else {
                        return Token::make_identifier(loc, str);
                    }
                } else if (peek == '"' || (peek == '#' && (ISALPHA(peek1) || peek1 == '_'))) {
                    // raw string literals
                    std::string str;
                    str.reserve(16); // some sensible default

                    int length = 1;
                    int hash_count = 0;

                    // get hash count at beginnning
                    current_char = peek;
                    while (current_char == '#') {
                        hash_count++;
                        length++;

                        skip_input();
                        current_char = peek_input();
                    }

                    if (current_char != '"') {
                        error_at(get_current_location(), "raw string has no opening '\"'");
                    }

                    skip_input();
                    Codepoint current_char32 = test_peek_codepoint_input();

                    while (true) {
                        if (current_char32.value == '"') {
                            bool enough_hashes = true;

                            for (int i = 0; i < hash_count; i++) {
                                // if (test_peek_codepoint_input(i + 1) != '#') {
                                // TODO: ensure this is a good enough replacement
                                if (peek_input(i + 1) != '#') {
                                    enough_hashes
                                      = false; // could continue here - improve performance
                                }
                            }

                            if (enough_hashes) {
                                // skip enough input and peek enough input
                                skip_input(hash_count); // is this enough?
                                current_char = peek_input();
                                length += hash_count + 1;
                                break;
                            }
                        }

                        length++;

                        str += current_char32;
                        test_skip_codepoint_input();
                        current_char32 = test_peek_codepoint_input();
                    }

                    current_column += length;

                    return Token::make_string(loc, str); // TODO: does this work properly
                }
            }

            // find identifiers and keywords
            if (ISALPHA(current_char) || current_char == '_') {
                std::string str;
                str.reserve(16); // default
                str += current_char;

                bool first_is_underscore = current_char == '_';

                int length = 1;
                current_char = peek_input();
                // loop through entire name
                while (ISALPHA(current_char) || ISDIGIT(current_char) || current_char == '_') {
                    length++;

                    str += current_char;
                    skip_input();
                    current_char = peek_input();
                }

                current_column += length;

                // if just a single underscore, not an identifier
                if (first_is_underscore && length == 1) {
                    return Token::make(UNDERSCORE, loc);
                }

                TokenId keyword = classify_keyword(str);
                if (keyword == IDENTIFIER) {
                    return Token::make_identifier(loc, str);
                } else {
                    return Token::make(keyword, loc);
                }
            }

            // identify literals
            // int or float literals - not processed properly
            if (ISDIGIT(current_char) || current_char == '.') { //  _ not allowed as first char
                std::string str;
                str.reserve(16); // some sensible default
                str += current_char;

                PrimitiveCoreType type_hint = CORETYPE_UNKNOWN;

                bool is_real = (current_char == '.');

                int length = 1;

                // handle binary, octal, hex literals
                if (current_char == '0' && !ISDIGIT(peek_input())) {
                    current_char = peek_input();

                    if (current_char == 'x') {
                        // hex (integer only)

                        skip_input();
                        current_char = peek_input();

                        length++;

                        // add 'x' to string after 0 so it is 0xFFAA or whatever
                        str += 'x';

                        // loop through to add entire hex number to string
                        while (is_x_digit(current_char) || current_char == '_') {
                            if (current_char == '_') {
                                // don't add _ to number
                                skip_input();
                                current_char = peek_input();

                                length++;

                                continue;
                            }

                            length++;

                            // add raw hex numbers
                            str += current_char;
                            skip_input();
                            current_char = peek_input();
                        }

                        current_column += length;

                        // convert hex value to decimal representation
                        long hex_num = ::std::strtol(str.c_str(), NULL, 16);

                        // create output string stream for hex value to be converted to string
                        // again
                        // TODO: if too slow, use sprintf
                        ::std::ostringstream ostr;
                        ostr << hex_num;

                        // reassign string representation to converted value
                        str = ostr.str();

                        // parse in type suffix if it exists
                        parse_in_type_suffix(/*current_char, */ type_hint, length);

                        if (type_hint == CORETYPE_F32 || type_hint == CORETYPE_F64) {
                            error_at(get_current_location(),
                              "invalid type suffix '%s' for integer (hex) literal",
                              get_type_hint_string(type_hint));
                        }
                    } else if (current_char == 'o') {
                        // octal (integer only)

                        skip_input();
                        current_char = peek_input();

                        length++;

                        // don't add any characters as C octals are just 0124 or whatever

                        // loop through to add entire octal number to string
                        while (is_octal_digit(current_char) || current_char == '_') {
                            if (current_char == '_') {
                                // don't add _ to number
                                skip_input();
                                current_char = peek_input();

                                length++;

                                continue;
                            }

                            length++;

                            // add raw octal numbers
                            str += current_char;
                            skip_input();
                            current_char = peek_input();
                        }

                        current_column += length;

                        // convert octal value to decimal representation
                        long octal_num = ::std::strtol(str.c_str(), NULL, 8);

                        // create output string stream for octal value to be converted to
                        // string again
                        // TODO: if too slow, use sprintf
                        ::std::ostringstream ostr;
                        ostr << octal_num;

                        // reassign string representation to converted value
                        str = ostr.str();

                        // parse in type suffix if it exists
                        parse_in_type_suffix(/*current_char, */ type_hint, length);

                        if (type_hint == CORETYPE_F32 || type_hint == CORETYPE_F64) {
                            error_at(get_current_location(),
                              "invalid type suffix '%s' for integer (octal) literal",
                              get_type_hint_string(type_hint));
                        }
                    } else if (current_char == 'b') {
                        // binary (integer only)

                        skip_input();
                        current_char = peek_input();

                        length++;

                        // don't add any characters as C binary numbers are not really
                        // supported

                        // loop through to add entire binary number to string
                        while (is_bin_digit(current_char) || current_char == '_') {
                            if (current_char == '_') {
                                // don't add _ to number
                                skip_input();
                                current_char = peek_input();

                                length++;

                                continue;
                            }

                            length++;

                            // add raw binary numbers
                            str += current_char;
                            skip_input();
                            current_char = peek_input();
                        }

                        current_column += length;

                        // convert binary value to decimal representation
                        long bin_num = ::std::strtol(str.c_str(), NULL, 2);

                        // create output string stream for binary value to be converted to
                        // string again
                        // TODO: if too slow, use sprintf
                        ::std::ostringstream ostr;
                        ostr << bin_num;

                        // reassign string representation to converted value
                        str = ostr.str();

                        // parse in type suffix if it exists
                        parse_in_type_suffix(/*current_char, */ type_hint, length);

                        if (type_hint == CORETYPE_F32 || type_hint == CORETYPE_F64) {
                            error_at(get_current_location(),
                              "invalid type suffix '%s' for integer (binary) literal",
                              get_type_hint_string(type_hint));
                        }
                    }
                } else {
                    // handle decimals (integer or float)

                    current_char = peek_input();

                    // parse initial decimal literal - assuming integer
                    // TODO: test if works
                    parse_in_decimal(/*current_char, */ str, length);

                    // detect float literal - TODO: fix: "242." is not recognised as a float literal
                    if (current_char == '.' && is_float_digit(peek_input(1))) {
                        // float with a '.', parse another decimal into it

                        is_real = true;

                        // add . to str
                        str += current_char;
                        skip_input();
                        current_char = peek_input();

                        length++;

                        // parse another decimal number for float
                        // TODO: test if works
                        parse_in_decimal(/*current_char, */ str, length);

                        // parse in exponent part if it exists
                        // test to see if this works:
                        parse_in_exponent_part(/*current_char, */ str, length);

                        // parse in type suffix if it exists
                        // TODO: see if works:
                        parse_in_type_suffix(/*current_char, */ type_hint, length);

                        if (type_hint != CORETYPE_F32 && type_hint != CORETYPE_F64
                            && type_hint != CORETYPE_UNKNOWN) {
                            error_at(get_current_location(),
                              "invalid type suffix '%s' for float literal",
                              get_type_hint_string(type_hint));
                        }

                    } else if (current_char == '.' && check_valid_float_dot_end(peek_input(1))) {
                        is_real = true;

                        // add . to str
                        str += current_char;
                        skip_input();
                        current_char = peek_input();
                        length++;

                        // add a '0' after the . to stop ambiguity
                        str += '0';

                        // don't parse another decimal number for float

                        // parse in exponent part if it exists - shouldn't exist?
                        // parse_in_exponent_part(/*current_char, */ str, length);

                        // parse in type suffix if it exists - shouldn't exist?
                        // TODO: see if works:
                        // parse_in_type_suffix(/*current_char, */ type_hint, length);

                        if (type_hint != CORETYPE_F32 && type_hint != CORETYPE_F64
                            && type_hint != CORETYPE_UNKNOWN) {
                            error_at(get_current_location(),
                              "invalid type suffix '%s' for float literal",
                              get_type_hint_string(type_hint));
                        }
                    } else if (current_char == 'E' || current_char == 'e') {
                        is_real = true;

                        // parse exponent part
                        parse_in_exponent_part(/*current_char, */ str, length);

                        // parse in type suffix if it exists
                        parse_in_type_suffix(/*current_char, */ type_hint, length);

                        if (type_hint != CORETYPE_F32 && type_hint != CORETYPE_F64
                            && type_hint != CORETYPE_UNKNOWN) {
                            error_at(get_current_location(),
                              "invalid type suffix '%s' for float literal",
                              get_type_hint_string(type_hint));
                        }
                    } else {
                        // is an integer

                        // parse in type suffix if it exists
                        parse_in_type_suffix(/*current_char, */ type_hint, length);

                        if (type_hint == CORETYPE_F32 || type_hint == CORETYPE_F64) {
                            error_at(get_current_location(),
                              "invalid type suffix '%s' for integer (decimal) literal",
                              get_type_hint_string(type_hint));
                        }
                    }

                    current_column += length;
                }

                // actually make the tokens
                if (is_real) {
                    return Token::make_float(loc, str, type_hint);
                } else {
                    return Token::make_int(loc, str, type_hint);
                }
            }

            // string literals - not processed properly
            if (current_char == '"') {
                Codepoint current_char32;

                std::string str;
                str.reserve(16); // some sensible default

                int length = 1;
                current_char32 = test_peek_codepoint_input();

                // ok initial peek_codepoint seems to work without "too long"

                while (current_char32.value != '\n' && current_char32.value != '"') {
                    // TODO: handle escapes and string continue
                    if (current_char32.value == '\\') {
                        // parse escape
                        parse_utf8_escape(length, current_char32, '\'');

                        // TODO: find a way to parse additional characters after the escape?
                        // return after parsing escape?

                        str += current_char32;

                        // required as parsing utf8 escape only changes current_char or something
                        current_char32 = test_peek_codepoint_input();

                        continue;
                    }

                    length += test_get_input_codepoint_length();

                    // does this work? not technically a char. maybe have to convert to char series
                    str += current_char32;
                    test_skip_codepoint_input();
                    current_char32 = test_peek_codepoint_input();
                }

                current_column += length;

                if (current_char32.value == '\n') {
                    error_at(get_current_location(), "unended string literal");
                } else if (current_char32.value == '"') {
                    skip_input();

                    current_char = peek_input();
                } else {
                    gcc_unreachable();
                }

                return Token::make_string(loc, str);
                // TODO: account for escapes and string continue
                // also, in rust a string is a series of unicode characters (4 bytes)
            }

            // char literal attempt
            if (current_char == '\'') {
                // rust chars are 4 bytes and have some weird unicode representation thing
                Codepoint current_char32;

                int length = 1;

                current_char32 = test_peek_codepoint_input();

                // parse escaped char literal
                if (current_char32.value == '\\') {
                    // parse escape
                    parse_utf8_escape(length, current_char32, '\'');

                    // TODO - this skip may not be needed?
                    // test_skip_codepoint_input();

                    if (test_peek_codepoint_input().value != '\'') {
                        error_at(get_current_location(), "unended char literal");
                    } else {
                        test_skip_codepoint_input();
                        current_char = peek_input();
                        length++;
                    }

                    current_column += length;

                    // TODO: FIX - char is actually 4 bytes in Rust (uint32) due to unicode
                    return Token::make_char(loc, current_char32);
                } else {
                    // current_char32 = test_peek_codepoint_input();
                    test_skip_codepoint_input();

                    if (test_peek_codepoint_input().value == '\'') {
                        // parse normal char literal
                        // TODO: FIX - char is actually 4 bytes in Rust (uint32) due to unicode

                        // skip the ' character
                        skip_input();
                        current_char = peek_input();

                        // TODO fix due to different widths of utf-8 chars
                        current_column += 3;

                        return Token::make_char(loc, current_char32);
                    } else if (ISDIGIT(current_char32.value) || ISALPHA(current_char32.value)
                               || current_char32.value == '_') {
                        // parse lifetime name
                        ::std::string str;
                        // TODO: does this work properly?
                        str += current_char32;

                        // TODO: fix lifetime name thing - actually, why am I even using utf-8 here?

                        int length = 1;

                        current_char32 = test_peek_codepoint_input();

                        while (ISDIGIT(current_char32.value) || ISALPHA(current_char32.value)
                               || current_char32.value == '_') {
                            length += test_get_input_codepoint_length();

                            str += current_char32;
                            test_skip_codepoint_input();
                            current_char32 = test_peek_codepoint_input();
                        }

                        current_column += length;

                        return Token::make_lifetime(loc, str);
                    } else {
                        error_at(get_current_location(), "expected ' after character constant");
                    }
                }
            }

            // didn't match anything so error
            error_at(loc, "unexpected character '%x'", current_char);
            current_column++;
        }
    }

    // Shitty pass-by-reference way of parsing in type suffix.
    bool Lexer::parse_in_type_suffix(
      /*char& current_char, */ PrimitiveCoreType& type_hint, int& length) {
        ::std::string suffix;
        suffix.reserve(5);

        // get suffix
        while (ISALPHA(current_char) || ISDIGIT(current_char) || current_char == '_') {
            if (current_char == '_') {
                // don't add _ to suffix
                skip_input();
                current_char = peek_input();

                length++;

                continue;
            }

            length++;

            suffix += current_char;
            skip_input();
            current_char = peek_input();
        }

        if (suffix.empty()) {
            // no type suffix: do nothing but also no error
            return false;
        } else if (suffix == "f32") {
            type_hint = CORETYPE_F32;
        } else if (suffix == "f64") {
            type_hint = CORETYPE_F64;
        } else if (suffix == "i8") {
            type_hint = CORETYPE_I8;
        } else if (suffix == "i16") {
            type_hint = CORETYPE_I16;
        } else if (suffix == "i32") {
            type_hint = CORETYPE_I32;
        } else if (suffix == "i64") {
            type_hint = CORETYPE_I64;
        } else if (suffix == "i128") {
            type_hint = CORETYPE_I128;
        } else if (suffix == "isize") {
            type_hint = CORETYPE_ISIZE;
        } else if (suffix == "u8") {
            type_hint = CORETYPE_U8;
        } else if (suffix == "u16") {
            type_hint = CORETYPE_U16;
        } else if (suffix == "u32") {
            type_hint = CORETYPE_U32;
        } else if (suffix == "u64") {
            type_hint = CORETYPE_U64;
        } else if (suffix == "u128") {
            type_hint = CORETYPE_U128;
        } else if (suffix == "usize") {
            type_hint = CORETYPE_USIZE;
        } else {
            error_at(get_current_location(), "unknown number suffix '%s'", suffix.c_str());

            return false;
        }

        return true;
    }

    void Lexer::parse_in_exponent_part(/*char& current_char, */ std::string& str, int& length) {
        if (current_char == 'E' || current_char == 'e') {
            // add exponent to string as strtod works with it
            str += current_char;
            skip_input();
            current_char = peek_input();

            length++;

            // special - and + handling
            if (current_char == '-') {
                str += '-';

                skip_input();
                current_char = peek_input();

                length++;
            } else if (current_char == '+') {
                // don't add + but still skip input
                skip_input();
                current_char = peek_input();

                length++;
            }

            // parse another decimal number for exponent
            parse_in_decimal(/*current_char, */ str, length);
        }
    }

    void Lexer::parse_in_decimal(/*char& current_char, */ std::string& str, int& length) {
        while (ISDIGIT(current_char) || current_char == '_') {
            if (current_char == '_') {
                // don't add _ to number
                skip_input();
                current_char = peek_input();

                length++;

                continue;
            }

            length++;

            str += current_char;
            skip_input();
            current_char = peek_input();
        }
    }

    // Replace all assorted parse_x_escape with this? Avoids the backwards/peek issue.
    bool Lexer::parse_escape(int& length, char& output_char, char opening_char) {
        // skip to actual letter
        skip_input();
        current_char = peek_input();
        length++;

        switch (current_char) {
            case 'x': {
                // hex char string (null-terminated)
                char hexNum[3] = { 0, 0, 0 };

                // first hex char
                skip_input();
                current_char = peek_input();
                length++;

                if (!ISXDIGIT(current_char)) {
                    error_at(get_current_location(), "invalid character '\\x%c' in \\x sequence",
                      current_char);
                }
                hexNum[0] = current_char;

                // second hex char
                skip_input();
                current_char = peek_input();
                length++;

                if (!ISXDIGIT(current_char)) {
                    error_at(get_current_location(), "invalid character '\\x%c' in \\x sequence",
                      current_char);
                }
                hexNum[1] = current_char;

                long hexLong = ::std::strtol(hexNum, NULL, 16);

                if (hexLong > 127)
                    error_at(get_current_location(),
                      "ascii \\x escape '\\x%s' out of range - allows up to '\\x7F'", hexNum);
                // gcc_assert(hexLong < 128); // as ascii
                char hexChar = static_cast<char>(hexLong);

                // TODO: fix - does this actually give the right character?
                output_char = hexChar;
            } break;
            case 'n':
                output_char = '\n';
                break;
            case 'r':
                output_char = '\r';
                break;
            case 't':
                output_char = '\t';
                break;
            case '\\':
                output_char = '\\';
                break;
            case '0':
                output_char = '\0';
                break;
            case '\'':
                output_char = '\'';
                break;
            case '"':
                output_char = '"';
                break;
            case 'u': {
                // TODO: shouldn't be used with this - use parse_utf8_escape

                skip_input();
                current_char = peek_input();
                length++;

                bool need_close_brace = false;

                // TODO: rustc lexer doesn't seem to allow not having { but mrustc lexer does? look at
                // spec?
                if (current_char == '{') {
                    need_close_brace = true;

                    skip_input();
                    current_char = peek_input();
                    length++;
                }

                // parse unicode escape
                // 1-6 hex digits?
                ::std::string num_str;
                num_str.reserve(6);

                // test adding number directly
                uint32_t test_val;

                // loop through to add entire hex number to string
                while (is_x_digit(current_char) || current_char == '_') {
                    if (current_char == '_') {
                        // don't add _ to number
                        skip_input();
                        current_char = peek_input();

                        length++;

                        continue;
                    }

                    length++;

                    // add raw hex numbers
                    num_str += current_char;

                    // test adding number directly
                    char tmp[2] = { current_char, 0 };
                    test_val *= 16;
                    test_val += ::std::strtol(tmp, NULL, 16);

                    skip_input();
                    current_char = peek_input();
                }

                // ensure closing brace
                if (need_close_brace && current_char != '}') {
                    // actually an error
                    error_at(get_current_location(), "expected terminating '}' in unicode escape");
                    return false;
                }

                // ensure 1-6 hex characters
                if (num_str.length() > 6 || num_str.length() < 1) {
                    error_at(get_current_location(),
                      "unicode escape should be between 1 and 6 hex characters; it is %lu",
                      num_str.length());
                    return false;
                }

                long hex_num = ::std::strtol(num_str.c_str(), NULL, 16);

                // as debug, check hex_num = test_val
                if (hex_num > 255) {
                    error_at(
                      get_current_location(), "non-ascii chars not implemented yet, defaulting to 0");
                    hex_num = 0;
                }

                // make output_char the value - UTF-8?
                // TODO: actually make this work - output char must be 4 bytes, do I need a string for
                // this?
                output_char = static_cast</*uint32_t*/ char>(hex_num);

                return true;
            } break;
            case '\r':
            case '\n':
                // string continue
                while (is_whitespace(current_char)) {
                    if (current_char == '\n') {
                        current_line++;
                        current_column = 1;
                        // tell line_table that new line starts
                        linemap_line_start(::line_table, current_line, max_column_hint);

                        // reset "length"
                        length = 1;

                        // get next char
                        skip_input();
                        current_char = peek_input();

                        continue;
                    }

                    skip_input();
                    current_char = peek_input();
                    length++;
                }

                if (current_char == '\\') {
                    parse_escape(length, output_char, opening_char);
                    return true;
                } else if (current_char == opening_char) {
                    // TODO: does this skip the ' or " character? It shouldn't.
                    output_char = 0;
                    return true;
                } else {
                    output_char = current_char;

                    // TODO: test has right result
                    /*skip_input();
                    current_char = peek_input();*/

                    return true;
                }
            default:
                error_at(get_current_location(), "unknown escape sequence '\\%c'", current_char);
                // returns false if no parsing could be done
                return false;
                break;
        }
        // all non-special cases (unicode, string continue) should skip their used char
        skip_input();
        current_char = peek_input();
        length++;

        // returns true if parsing was successful
        return true;
    }

    bool Lexer::parse_utf8_escape(int& length, Codepoint& output_char, char opening_char) {
        // skip to actual letter
        skip_input();
        current_char = peek_input();
        length++;

        switch (current_char) {
            case 'x': {
                // hex char string (null-terminated)
                char hexNum[3] = { 0, 0, 0 };

                // first hex char
                skip_input();
                current_char = peek_input();
                length++;

                if (!ISXDIGIT(current_char)) {
                    error_at(get_current_location(), "invalid character '\\x%c' in \\x sequence",
                      current_char);
                }
                hexNum[0] = current_char;

                // second hex char
                skip_input();
                current_char = peek_input();
                length++;

                if (!ISXDIGIT(current_char)) {
                    error_at(get_current_location(), "invalid character '\\x%c' in \\x sequence",
                      current_char);
                }
                hexNum[1] = current_char;

                long hexLong = ::std::strtol(hexNum, NULL, 16);

                if (hexLong > 127)
                    error_at(get_current_location(),
                      "ascii \\x escape '\\x%s' out of range - allows up to '\\x7F'", hexNum);
                // gcc_assert(hexLong < 128); // as ascii
                char hexChar = static_cast<char>(hexLong);

                // TODO: fix - does this actually give the right character?
                output_char = hexChar;
            } break;
            case 'n':
                output_char = '\n';
                break;
            case 'r':
                output_char = '\r';
                break;
            case 't':
                output_char = '\t';
                break;
            case '\\':
                output_char = '\\';
                break;
            case '0':
                output_char = '\0';
                break;
            case '\'':
                output_char = '\'';
                break;
            case '"':
                output_char = '"';
                break;
            case 'u': {
                skip_input();
                current_char = peek_input();
                length++;

                bool need_close_brace = false;

                // TODO: rustc lexer doesn't seem to allow not having { but mrustc lexer does? look at
                // spec?
                if (current_char == '{') {
                    need_close_brace = true;

                    skip_input();
                    current_char = peek_input();
                    length++;
                }

                // parse unicode escape
                // 1-6 hex digits?
                ::std::string num_str;
                num_str.reserve(6);

                // test adding number directly
                uint32_t test_val;

                // loop through to add entire hex number to string
                while (is_x_digit(current_char) || current_char == '_') {
                    if (current_char == '_') {
                        // don't add _ to number
                        skip_input();
                        current_char = peek_input();

                        length++;

                        continue;
                    }

                    length++;

                    // add raw hex numbers
                    num_str += current_char;

                    // test adding number directly
                    char tmp[2] = { current_char, 0 };
                    test_val *= 16;
                    test_val += ::std::strtol(tmp, NULL, 16);

                    skip_input();
                    current_char = peek_input();
                }

                // ensure closing brace if required
                if (need_close_brace) {
                    if (current_char == '}') {
                        skip_input();
                        current_char = peek_input();
                        length++;
                    } else {
                        // actually an error
                        error_at(
                          get_current_location(), "expected terminating '}' in unicode escape");
                        return false;
                    }
                }

                // ensure 1-6 hex characters
                if (num_str.length() > 6 || num_str.length() < 1) {
                    error_at(get_current_location(),
                      "unicode escape should be between 1 and 6 hex characters; it is %lu",
                      num_str.length());
                    return false;
                }

                long hex_num = ::std::strtol(num_str.c_str(), NULL, 16);

                // assert fits a uint32_t
                gcc_assert(hex_num < 4294967296);

                // ok can't figure out how to just convert to codepoint or use "this" so create new
                // one
                output_char = Codepoint(static_cast<uint32_t>(hex_num));

                // TODO: what is being outputted? the escape code for the unicode char (unicode
                // number) or the character number?

                return true;
            } break;
            case '\r':
            case '\n':
                // string continue
                while (is_whitespace(current_char)) {
                    if (current_char == '\n') {
                        current_line++;
                        current_column = 1;
                        // tell line_table that new line starts
                        linemap_line_start(::line_table, current_line, max_column_hint);

                        // reset "length"
                        length = 1;

                        // get next char
                        skip_input();
                        current_char = peek_input();

                        continue;
                    }

                    skip_input();
                    current_char = peek_input();
                    length++;
                }

                if (current_char == '\\') {
                    parse_utf8_escape(length, output_char, opening_char);
                    return true;
                } else if (current_char == opening_char) {
                    // TODO: does this skip the ' or " character? It shouldn't.
                    output_char = 0;
                    return true;
                } else {
                    output_char = current_char;

                    // TODO: test has right result
                    /*skip_input();
                    current_char = peek_input();*/

                    return true;
                }
            default:
                error_at(get_current_location(), "unknown escape sequence '\\%c'", current_char);
                // returns false if no parsing could be done
                return false;
                break;
        }
        // all non-special cases (unicode, string continue) should skip their used char
        skip_input();
        current_char = peek_input();
        length++;

        // returns true if parsing was successful
        return true;
    }

#if 0
    bool Lexer::parse_ascii_escape(/*char& current_char, */ int& length, char& output_char) {
        // skip to actual letter
        skip_input();
        current_char = peek_input();
        length++;

        switch (current_char) {
            case 'x': {
                // hex char string (null-terminated)
                char hexNum[3] = { 0, 0, 0 };

                // first hex char
                skip_input();
                current_char = peek_input();
                length++;

                if (!ISXDIGIT(current_char)) {
                    error_at(get_current_location(), "invalid character '\\x%c' in \\x sequence",
                      current_char);
                }
                hexNum[0] = current_char;

                // second hex char
                skip_input();
                current_char = peek_input();
                length++;

                if (!ISXDIGIT(current_char)) {
                    error_at(get_current_location(), "invalid character '\\x%c' in \\x sequence",
                      current_char);
                }
                hexNum[1] = current_char;

                long hexLong = ::std::strtol(hexNum, NULL, 16);

                if (hexLong > 127)
                    error_at(get_current_location(),
                      "ascii \\x escape '\\x%s' out of range - allows up to '\\x7F'", hexNum);
                // gcc_assert(hexLong < 128); // as ascii
                char hexChar = static_cast<char>(hexLong);

                // TODO: fix - does this actually give the right character?
                output_char = hexChar;
            } break;
            case 'n':
                output_char = '\n';
                break;
            case 'r':
                output_char = '\r';
                break;
            case 't':
                output_char = '\t';
                break;
            case '\\':
                output_char = '\\';
                break;
            case '0':
                output_char = '\0';
                break;
            default:
                // error_at(get_current_location(), "unknown escape sequence '\\%c'", current_char);
                // returns false if no parsing could be done
                return false;
                break;
        }
        // returns true if parsing was successful
        return true;
    }

    bool Lexer::parse_quote_escape(/*char& current_char, */ int& length, char& output_char) {
        // skip to actual letter
        skip_input();
        current_char = peek_input();
        length++;

        switch (current_char) {
            case '\'':
                output_char = '\'';
                break;
            case '"':
                output_char = '"';
                break;
            default:
                return false;
                break;
        }
        return true;
    }

    bool Lexer::parse_unicode_escape(
      /*char& current_char, */ int& length, /*char*/ uint32_t& output_char) {
        // skip to actual letter
        skip_input();
        current_char = peek_input();
        length++;

        if (current_char != 'u') {
            // not a unicode escape, but not necessarily an error
            return false;
        }

        skip_input();
        current_char = peek_input();
        length++;

        bool need_close_brace = false;

        // TODO: rustc lexer doesn't seem to allow not having { but mrustc lexer does? look at spec?
        if (current_char == '{') {
            need_close_brace = true;

            skip_input();
            current_char = peek_input();
            length++;
        }

        // parse unicode escape
        // 1-6 hex digits?
        ::std::string num_str;
        num_str.reserve(6);

        // test adding number directly
        uint32_t test_val;

        // loop through to add entire hex number to string
        while (is_x_digit(current_char) || current_char == '_') {
            if (current_char == '_') {
                // don't add _ to number
                skip_input();
                current_char = peek_input();

                length++;

                continue;
            }

            length++;

            // add raw hex numbers
            num_str += current_char;

            // test adding number directly
            char tmp[2] = { current_char, 0 };
            test_val *= 16;
            test_val += ::std::strtol(tmp, NULL, 16);

            skip_input();
            current_char = peek_input();
        }

        // ensure closing brace
        if (need_close_brace && current_char != '}') {
            // actually an error
            error_at(get_current_location(), "expected terminating '}' in unicode escape");
            return false;
        }

        // ensure 1-6 hex characters
        if (num_str.length() > 6 || num_str.length() < 1) {
            error_at(get_current_location(),
              "unicode escape should be between 1 and 6 hex characters; it is %lu", num_str.length());
            return false;
        }

        long hex_num = ::std::strtol(num_str.c_str(), NULL, 16);

        // as debug, check hex_num = test_val

        // make output_char the value - UTF-8?
        // TODO: actually make this work - output char must be 4 bytes, do I need a string for this?
        output_char = static_cast<uint32_t>(hex_num);

        return true;
    }

    bool Lexer::parse_byte_escape(/*char& current_char, */ int& length, char& output_char) {
        // skip to actual letter
        skip_input();
        current_char = peek_input();
        length++;

        switch (current_char) {
            case 'x': {
                // hex char string (null-terminated)
                char hexNum[3] = { 0, 0, 0 };

                // first hex char
                skip_input();
                current_char = peek_input();
                length++;

                if (!ISXDIGIT(current_char)) {
                    error_at(get_current_location(), "invalid character '\\x%c' in \\x sequence",
                      current_char);
                }
                hexNum[0] = current_char;

                // second hex char
                skip_input();
                current_char = peek_input();
                length++;

                if (!ISXDIGIT(current_char)) {
                    error_at(get_current_location(), "invalid character '\\x%c' in \\x sequence",
                      current_char);
                }
                hexNum[1] = current_char;

                long hexLong = ::std::strtol(hexNum, NULL, 16);

                if (hexLong > 255)
                    error_at(get_current_location(),
                      "ascii \\x escape '\\x%s' out of range - allows up to '\\xFF'", hexNum);
                // gcc_assert(hexLong < 128); // as ascii
                char hexChar = static_cast<char>(hexLong);

                // TODO: fix - does this actually give the right character?
                output_char = hexChar;
            } break;
            case 'n':
                output_char = '\n';
                break;
            case 'r':
                output_char = '\r';
                break;
            case 't':
                output_char = '\t';
                break;
            case '\\':
                output_char = '\\';
                break;
            case '0':
                output_char = '\0';
                break;
            default:
                // error_at(get_current_location(), "unknown escape sequence '\\%c'", current_char);
                // returns false if no parsing could be done
                return false;
                break;
        }
        // returns true if parsing was successful
        return true;
    }
#endif

    int Lexer::test_get_input_codepoint_length() {
        uint8_t input = peek_input();

        if (input < 128) {
            // ascii -- 1 byte
            // return input;

            return 1;
        } else if ((input & 0xC0) == 0x80) {
            // invalid (continuation; can't be first char)
            // return 0xFFFE;

            return 0;
        } else if ((input & 0xE0) == 0xC0) {
            // 2 bytes
            uint8_t input2 = peek_input(1);
            if ((input2 & 0xC0) != 0x80)
                return 0;
            // return 0xFFFE;

            // uint32_t output = ((input & 0x1F) << 6) | ((input2 & 0x3F) << 0);
            // return output;
            return 2;
        } else if ((input & 0xF0) == 0xE0) {
            // 3 bytes
            uint8_t input2 = peek_input(1);
            if ((input2 & 0xC0) != 0x80)
                return 0;
            // return 0xFFFE;

            uint8_t input3 = peek_input(2);
            if ((input3 & 0xC0) != 0x80)
                return 0;
            // return 0xFFFE;

            /*uint32_t output
              = ((input & 0x0F) << 12) | ((input2 & 0x3F) << 6) | ((input3 & 0x3F) << 0);
            return output;*/
            return 3;
        } else if ((input & 0xF8) == 0xF0) {
            // 4 bytes
            uint8_t input2 = peek_input(1);
            if ((input2 & 0xC0) != 0x80)
                return 0;
            // return 0xFFFE;

            uint8_t input3 = peek_input(2);
            if ((input3 & 0xC0) != 0x80)
                return 0;
            // return 0xFFFE;

            uint8_t input4 = peek_input(3);
            if ((input4 & 0xC0) != 0x80)
                return 0;
            // return 0xFFFE;

            /*uint32_t output = ((input & 0x07) << 18) | ((input2 & 0x3F) << 12)
                              | ((input3 & 0x3F) << 6) | ((input4 & 0x3F) << 0);
            return output;*/
            return 4;
        } else {
            error_at(get_current_location(), "invalid UTF-8 (too long)");
            return 0;
        }
    }

    // TODO: rewrite lexing system to use utf-8 "codepoints" rather than bytes?
    Codepoint Lexer::test_peek_codepoint_input() {
        uint8_t input = peek_input();

        if (input < 128) {
            // ascii -- 1 byte
            return { input };
        } else if ((input & 0xC0) == 0x80) {
            // invalid (continuation; can't be first char)
            return { 0xFFFE };
        } else if ((input & 0xE0) == 0xC0) {
            // 2 bytes
            uint8_t input2 = peek_input(1);
            if ((input2 & 0xC0) != 0x80)
                return { 0xFFFE };

            uint32_t output = ((input & 0x1F) << 6) | ((input2 & 0x3F) << 0);
            return { output };
        } else if ((input & 0xF0) == 0xE0) {
            // 3 bytes
            uint8_t input2 = peek_input(1);
            if ((input2 & 0xC0) != 0x80)
                return { 0xFFFE };

            uint8_t input3 = peek_input(2);
            if ((input3 & 0xC0) != 0x80)
                return { 0xFFFE };

            uint32_t output
              = ((input & 0x0F) << 12) | ((input2 & 0x3F) << 6) | ((input3 & 0x3F) << 0);
            return { output };
        } else if ((input & 0xF8) == 0xF0) {
            // 4 bytes
            uint8_t input2 = peek_input(1);
            if ((input2 & 0xC0) != 0x80)
                return { 0xFFFE };

            uint8_t input3 = peek_input(2);
            if ((input3 & 0xC0) != 0x80)
                return { 0xFFFE };

            uint8_t input4 = peek_input(3);
            if ((input4 & 0xC0) != 0x80)
                return { 0xFFFE };

            uint32_t output = ((input & 0x07) << 18) | ((input2 & 0x3F) << 12)
                              | ((input3 & 0x3F) << 6) | ((input4 & 0x3F) << 0);
            return { output };
        } else {
            error_at(get_current_location(), "invalid UTF-8 (too long)");
            return { 0xFFFE };
        }
    }

    void Lexer::test_skip_codepoint_input() {
        int toSkip = test_get_input_codepoint_length();
        gcc_assert(toSkip >= 1);

        skip_input(toSkip - 1);
    }

    int Lexer::test_get_input_codepoint_n_length(int n_start_offset) {
        uint8_t input = peek_input(n_start_offset);

        if (input < 128) {
            // ascii -- 1 byte
            // return input;
            return 1;
        } else if ((input & 0xC0) == 0x80) {
            // invalid (continuation; can't be first char)
            // return 0xFFFE;
            return 0;
        } else if ((input & 0xE0) == 0xC0) {
            // 2 bytes
            uint8_t input2 = peek_input(n_start_offset + 1);
            if ((input2 & 0xC0) != 0x80)
                // return 0xFFFE;
                return 0;

            // uint32_t output = ((input & 0x1F) << 6) | ((input2 & 0x3F) << 0);
            // return output;
            return 2;
        } else if ((input & 0xF0) == 0xE0) {
            // 3 bytes
            uint8_t input2 = peek_input(n_start_offset + 1);
            if ((input2 & 0xC0) != 0x80)
                // return 0xFFFE;
                return 0;

            uint8_t input3 = peek_input(n_start_offset + 2);
            if ((input3 & 0xC0) != 0x80)
                // return 0xFFFE;
                return 0;

            /*uint32_t output
              = ((input & 0x0F) << 12) | ((input2 & 0x3F) << 6) | ((input3 & 0x3F) << 0);
            return output;*/
            return 3;
        } else if ((input & 0xF8) == 0xF0) {
            // 4 bytes
            uint8_t input2 = peek_input(n_start_offset + 1);
            if ((input2 & 0xC0) != 0x80)
                // return 0xFFFE;
                return 0;

            uint8_t input3 = peek_input(n_start_offset + 2);
            if ((input3 & 0xC0) != 0x80)
                // return 0xFFFE;
                return 0;

            uint8_t input4 = peek_input(n_start_offset + 3);
            if ((input4 & 0xC0) != 0x80)
                // return 0xFFFE;
                return 0;

            /*uint32_t output = ((input & 0x07) << 18) | ((input2 & 0x3F) << 12)
                              | ((input3 & 0x3F) << 6) | ((input4 & 0x3F) << 0);
            return output;*/
            return 4;
        } else {
            error_at(get_current_location(), "invalid UTF-8 (too long)");
            return 0;
        }
    }

    // peeks the codepoint input at n codepoints ahead of current codepoint - try not to use
    Codepoint Lexer::test_peek_codepoint_input(int n) {
        int totalOffset = 0;

        // add up all offsets into total offset? does this do what I want?
        for (int i = 0; i < n; i++) {
            totalOffset += test_get_input_codepoint_n_length(totalOffset);
        }
        // issues: this would have (at least) O(n) lookup time, not O(1) like the rest?

        // TODO: implement if still needed

        // error out of function as it is not implemented
        gcc_assert(1 == 0);
        return { 0 };
        /*
                uint8_t input = peek_input();

                if (input < 128) {
                    // ascii -- 1 byte
                    return input;
                } else if ((input & 0xC0) == 0x80) {
                    // invalid (continuation; can't be first char)
                    return 0xFFFE;
                } else if ((input & 0xE0) == 0xC0) {
                    // 2 bytes
                    uint8_t input2 = peek_input(1);
                    if ((input2 & 0xC0) != 0x80)
                        return 0xFFFE;

                    uint32_t output = ((input & 0x1F) << 6) | ((input2 & 0x3F) << 0);
                    return output;
                } else if ((input & 0xF0) == 0xE0) {
                    // 3 bytes
                    uint8_t input2 = peek_input(1);
                    if ((input2 & 0xC0) != 0x80)
                        return 0xFFFE;

                    uint8_t input3 = peek_input(2);
                    if ((input3 & 0xC0) != 0x80)
                        return 0xFFFE;

                    uint32_t output
                      = ((input & 0x0F) << 12) | ((input2 & 0x3F) << 6) | ((input3 & 0x3F) << 0);
                    return output;
                } else if ((input & 0xF8) == 0xF0) {
                    // 4 bytes
                    uint8_t input2 = peek_input(1);
                    if ((input2 & 0xC0) != 0x80)
                        return 0xFFFE;

                    uint8_t input3 = peek_input(2);
                    if ((input3 & 0xC0) != 0x80)
                        return 0xFFFE;

                    uint8_t input4 = peek_input(3);
                    if ((input4 & 0xC0) != 0x80)
                        return 0xFFFE;

                    uint32_t output = ((input & 0x07) << 18) | ((input2 & 0x3F) << 12)
                                      | ((input3 & 0x3F) << 6) | ((input4 & 0x3F) << 0);
                    return output;
                } else {
                    error_at(get_current_location(), "invalid UTF-8 (too long)");
                    return 0xFFFE;
                }*/
    }
}