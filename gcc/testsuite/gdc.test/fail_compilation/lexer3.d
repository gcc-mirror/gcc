/*
TEST_OUTPUT:
---
fail_compilation/lexer3.d(9): Error: unterminated token string constant starting at fail_compilation/lexer3.d(9)
fail_compilation/lexer3.d(10): Error: semicolon expected following auto declaration, not `EOF`
---
*/

static s1 = q{ asef;
