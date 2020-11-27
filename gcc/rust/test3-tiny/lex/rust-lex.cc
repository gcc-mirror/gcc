#include "rust-lex.h"

#include "diagnostic.h" // for error_at
#include "safe-ctype.h"

namespace Rust {
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
            int current_char = peek_input();
            skip_input();

            // return end of file token if end of file
            if (current_char == EOF) {
                return Token::make(END_OF_FILE, loc);
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
                    current_column++;
                    return Token::make(EQUAL, loc);
                case '(':
                    current_column++;
                    return Token::make(LEFT_PAREN, loc);
                case '-':
                    current_column++;
                    return Token::make(MINUS, loc);
                case '+':
                    current_column++;
                    return Token::make(PLUS, loc);
                case ')':
                    current_column++;
                    return Token::make(RIGHT_PAREN, loc);
                case ';':
                    current_column++;
                    return Token::make(SEMICOLON, loc);
                case '*':
                    current_column++;
                    return Token::make(ASTERISK, loc);
                /*case ',':
                    current_column++;
                    return Token::make(COMMA, loc);*/
                case '/':
                    current_column++;
                    return Token::make(SLASH, loc);
                case '%':
                    current_column++;
                    return Token::make(PERCENT, loc);
                /*case '[':
                    current_column++;
                    return Token::make(LEFT_SQUARE, loc);
                case ']':
                    current_column++;
                    return Token::make(RIGHT_SQUARE, loc);*/
                // dodgily implemented peeking for some operators
                case '<':
                    if (peek_input() == '=') {
                        skip_input();
                        current_column += 2;

                        return Token::make(SMALLER_OR_EQUAL, loc);
                    } else {
                        current_column++;
                        return Token::make(SMALLER, loc);
                    }
                    break;
                case '>':
                    if (peek_input() == '=') {
                        skip_input();
                        current_column += 2;

                        return Token::make(GREATER_OR_EQUAL, loc);
                    } else {
                        current_column++;
                        return Token::make(GREATER, loc);
                    }
                case ':':
                    if (peek_input() == '=') {
                        skip_input();
                        current_column += 2;

                        return Token::make(ASSIG, loc);
                    } else {
                        current_column++;
                        return Token::make(COLON, loc);
                    }
                case '!':
                    if (peek_input() == '=') {
                        skip_input();
                        current_column += 2;

                        return Token::make(DIFFERENT, loc);
                    }

                // bad comment implementation
                case '#': // single-line comment
                    current_column++;
                    current_char = peek_input();
                    // basically ignore until line finishes
                    while (current_char != '\n') {
                        skip_input();
                        current_column++; // not used
                        current_char = peek_input();
                    }

                    continue;
                    break;
                
                case '[':
                    current_column++;
                    return Token::make(LEFT_SQUARE, loc);
                case ']':
                    current_column++;
                    return Token::make(RIGHT_SQUARE, loc);
                case '.':
                    if (!ISDIGIT(peek_input())) {
                        // Only if followed by a non-number
                        current_column++;
                        return Token::make(DOT, loc);
                    }
            }

            // find identifiers and keywords
            if (ISALPHA(current_char)
                || current_char == '_') { // is alphanumeric or _ (maybe just letters)
                std::string str;
                str.reserve(16); // default
                str += current_char;

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

                TokenId keyword = classify_keyword(str);
                if (keyword == IDENTIFIER) {
                    return Token::make_identifier(loc, str);
                } else {
                    return Token::make(keyword, loc);
                }
            }

            // identify literals
            // int or float literals
            if (ISDIGIT(current_char) || current_char == '.') {
                std::string str;
                str.reserve(16); // some sensible default
                str += current_char;

                bool is_real = (current_char == '.');

                int length = 1;
                current_char = peek_input();
                while (ISDIGIT(current_char) || (!is_real && current_char == '.')) {
                    length++;

                    is_real = is_real || (current_char == '.');

                    str += current_char;
                    skip_input();
                    current_char = peek_input();
                }

                current_column += length;

                if (is_real) {
                    return Token::make_float(loc, str);
                } else {
                    return Token::make_int(loc, str);
                }
            }

            // string literals
            if (current_char == '"') {
                std::string str;
                str.reserve(16); // some sensible default

                int length = 1;
                current_char = peek_input();
                while (current_char != '\n' && current_char != '"') {
                    length++;

                    str += current_char;
                    skip_input();
                    current_char = peek_input();
                }

                current_column += length;

                if (current_char == '\n') {
                    error_at(get_current_location(), "unended string literal");
                } else if (current_char == '"') {
                    skip_input();
                } else {
                    gcc_unreachable();
                }

                return Token::make_string(loc, str);
            }

            // didn't match anything so error
            error_at(loc, "unexpected character '%x'", current_char);
            current_column++;
        }
    }
}