/*
TEST_OUTPUT:
---
fail_compilation/lexer2.d(16): Error: odd number (3) of hex characters in hex string
fail_compilation/lexer2.d(16): Error: Built-in hex string literals are obsolete, use `std.conv.hexString!"123"` instead.
fail_compilation/lexer2.d(17): Error: non-hex character 'G' in hex string
fail_compilation/lexer2.d(17): Error: Built-in hex string literals are obsolete, use `std.conv.hexString!"123G"` instead.
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
