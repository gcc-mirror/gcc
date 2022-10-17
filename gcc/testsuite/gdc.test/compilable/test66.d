// PERMUTE_ARGS:
// EXTRA_FILES: imports/test66a.d
import imports.test66a;

alias int TOK;

enum
{
	TOKmax
};

struct Token
{
    static char[][TOKmax] tochars;
}

class Lexer
{
    Token token;
}
