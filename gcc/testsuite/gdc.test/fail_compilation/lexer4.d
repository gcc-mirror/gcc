/*
TEST_OUTPUT:
---
fail_compilation/lexer4.d(22): Error: unterminated character constant
fail_compilation/lexer4.d(24): Error: unterminated character constant
fail_compilation/lexer4.d(25): Error: unterminated character constant
fail_compilation/lexer4.d(26): Error: binary digit expected, not `2`
fail_compilation/lexer4.d(27): Error: octal digit expected, not `8`
fail_compilation/lexer4.d(27): Error: octal literals larger than 7 are no longer supported
fail_compilation/lexer4.d(28): Error: decimal digit expected, not `a`
fail_compilation/lexer4.d(29): Error: repeated integer suffix `U`
fail_compilation/lexer4.d(30): Error: exponent required for hex float
fail_compilation/lexer4.d(31): Error: lower case integer suffix 'l' is not allowed. Please use 'L' instead
fail_compilation/lexer4.d(32): Error: use 'i' suffix instead of 'I'
fail_compilation/lexer4.d(34): Error: line number `1234567891234567879` out of range
fail_compilation/lexer4.d(36): Error: positive integer argument expected following `#line`
fail_compilation/lexer4.d(19): Error: found `"file"` when expecting new line following `#line` directive
---
*/


static c1 = '
;
static c2 = '';
static c3 = 'a;
int i = 0b12;
int j = 0128;
int k = 12a;
int l = 12UU;
int f = 0x1234.0;
int m = 12l;
static n = 12.1I;

#line 1234567891234567879

#line whatever

#line 18 __FILE__

#line 20 "file" "file"

/** asdf *//** asdf2 */
int o;
