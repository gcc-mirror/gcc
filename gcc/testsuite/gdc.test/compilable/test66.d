// PERMUTE_ARGS:

import imports.test66a;

alias int TOK;

enum
{
	TOKmax
};

struct Token
{
    static char[] tochars[TOKmax];
}

class Lexer
{
    Token token;
}

