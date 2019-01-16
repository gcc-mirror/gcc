/*
TEST_OUTPUT:
---
fail_compilation/lexer2.d(14): Error: odd number (3) of hex characters in hex string
fail_compilation/lexer2.d(15): Error: non-hex character 'G' in hex string
fail_compilation/lexer2.d(16): Error: heredoc rest of line should be blank
fail_compilation/lexer2.d(18): Error: unterminated delimited string constant starting at fail_compilation/lexer2.d(18)
fail_compilation/lexer2.d(20): Error: semicolon expected following auto declaration, not `EOF`
---
*/

// https://dlang.dawg.eu/coverage/src/lexer.c.gcov.html

static s1 = x"123";
static s2 = x"123G";
static s4 = q"here notblank
here";
static s5 = q"here
";
