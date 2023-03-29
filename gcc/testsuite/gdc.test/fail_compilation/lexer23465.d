/*
TEST_OUTPUT:
---
fail_compilation/lexer23465.d(19): Error: char 0x1f37a not allowed in identifier
fail_compilation/lexer23465.d(19): Error: character 0x1f37a is not a valid token
fail_compilation/lexer23465.d(20): Error: character '\' is not a valid token
fail_compilation/lexer23465.d(21): Error: unterminated /+ +/ comment
fail_compilation/lexer23465.d(22): Error: found `End of File` instead of array initializer
fail_compilation/lexer23465.d(22): Error: semicolon needed to end declaration of `arr`, instead of `End of File`
fail_compilation/lexer23465.d(17):        `arr` declared here
---
*/

// https://issues.dlang.org/show_bug.cgi?id=23465
// Invalid token error points to wrong line

int[] arr = [
	0,
    x🍺,
    3\,
    5, /+
