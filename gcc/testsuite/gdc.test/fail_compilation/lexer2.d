/*
TEST_OUTPUT:
---
fail_compilation/lexer2.d(16): Error: semicolon expected following auto declaration, not `"123"`
fail_compilation/lexer2.d(16): Error: declaration expected, not `"123"`
fail_compilation/lexer2.d(17): Error: semicolon expected following auto declaration, not `"123G"`
fail_compilation/lexer2.d(17): Error: declaration expected, not `"123G"`
fail_compilation/lexer2.d(18): Error: heredoc rest of line should be blank
fail_compilation/lexer2.d(20): Error: unterminated delimited string constant starting at fail_compilation/lexer2.d(20)
fail_compilation/lexer2.d(22): Error: semicolon expected following auto declaration, not `End of File`
---
*/

// https://dlang.dawg.eu/coverage/src/lexer.c.gcov.html

static s1 = x"123";
static s2 = x"123G";
static s4 = q"here notblank
here";
static s5 = q"here
";
