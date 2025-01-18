/*
TEST_OUTPUT:
---
fail_compilation/lexer23465.d(22): Error: character 0x1f37a is not allowed as a continue character in an identifier
fail_compilation/lexer23465.d(22): Error: character 0x1f37a is not a valid token
fail_compilation/lexer23465.d(23): Error: character '\' is not a valid token
fail_compilation/lexer23465.d(24): Error: octal digit expected, not `9`
fail_compilation/lexer23465.d(24): Error: octal literals larger than 7 are no longer supported
fail_compilation/lexer23465.d(25): Error: integer overflow
fail_compilation/lexer23465.d(26): Error: unterminated /+ +/ comment
fail_compilation/lexer23465.d(27): Error: found `End of File` instead of array initializer
fail_compilation/lexer23465.d(27): Error: semicolon needed to end declaration of `arr`, instead of `End of File`
fail_compilation/lexer23465.d(20):        `arr` declared here
---
*/

// https://issues.dlang.org/show_bug.cgi?id=23465
// Invalid token error points to wrong line

int[] arr = [
	0,
    x🍺,
    3\,
    09,
    9999999999999999999999,
    5, /+
