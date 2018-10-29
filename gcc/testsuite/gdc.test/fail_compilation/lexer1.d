/*
TEST_OUTPUT:
---
fail_compilation/lexer1.d(30): Error: declaration expected, not `x"01 02 03"w`
fail_compilation/lexer1.d(31): Error: declaration expected, not `2147483649U`
fail_compilation/lexer1.d(32): Error: declaration expected, not `0.1`
fail_compilation/lexer1.d(33): Error: declaration expected, not `0.1f`
fail_compilation/lexer1.d(34): Error: declaration expected, not `0.1L`
fail_compilation/lexer1.d(35): Error: declaration expected, not `0.1i`
fail_compilation/lexer1.d(36): Error: declaration expected, not `0.1fi`
fail_compilation/lexer1.d(37): Error: declaration expected, not `0.1Li`
fail_compilation/lexer1.d(38): Error: declaration expected, not `32U`
fail_compilation/lexer1.d(39): Error: declaration expected, not `55295U`
fail_compilation/lexer1.d(40): Error: declaration expected, not `65536U`
fail_compilation/lexer1.d(41): Error: declaration expected, not `"ab\\c\"\u1234a\U00011100a"d`
fail_compilation/lexer1.d(43): Error: declaration expected, not `module`
fail_compilation/lexer1.d(45): Error: escape hex sequence has 1 hex digits instead of 2
fail_compilation/lexer1.d(46): Error: undefined escape hex sequence \G
fail_compilation/lexer1.d(47): Error: unnamed character entity &unnamedentity;
fail_compilation/lexer1.d(48): Error: unterminated named entity &1;
fail_compilation/lexer1.d(49): Error: unterminated named entity &*;
fail_compilation/lexer1.d(50): Error: unterminated named entity &s1";
fail_compilation/lexer1.d(51): Error: unterminated named entity &2;
fail_compilation/lexer1.d(52): Error: escape octal sequence \400 is larger than \377
---
*/

// https://dlang.dawg.eu/coverage/src/lexer.c.gcov.html

x"01 02 03"w;
0x80000001;
0.1;
0.1f;
0.1L;
0.1i;
0.1fi;
0.1Li;
' ';
'\uD7FF';
'\U00010000';
"ab\\c\"\u1234a\U00011100a\000ab"d;

module x;

static s1 = "\x1G";
static s2 = "\xGG";
static s3 = "\&unnamedentity;";
static s4 = "\&1";
static s5 = "\&*";
static s6 = "\&s1";
static s7 = "\&2;";
static s7 = "\400;";
