/*
REQUIRED_ARGS: -verrors=0
TEST_OUTPUT:
---
fail_compilation/misc_parser_err_cov1.d(29): Error: basic type expected, not `)`
fail_compilation/misc_parser_err_cov1.d(30): Error: basic type expected, not `)`
fail_compilation/misc_parser_err_cov1.d(31): Error: `__traits(identifier, args...)` expected
fail_compilation/misc_parser_err_cov1.d(31): Error: semicolon expected following auto declaration, not `o`
fail_compilation/misc_parser_err_cov1.d(31): Error: expression expected, not `)`
fail_compilation/misc_parser_err_cov1.d(32): Error: expected `(` following `is`, not `;`
fail_compilation/misc_parser_err_cov1.d(33): Error: semicolon expected following auto declaration, not `auto`
fail_compilation/misc_parser_err_cov1.d(33): Error: found `+` when expecting `(` following `mixin`
fail_compilation/misc_parser_err_cov1.d(35): Error: `key:value` expected for associative array literal
fail_compilation/misc_parser_err_cov1.d(36): Error: basic type expected, not `;`
fail_compilation/misc_parser_err_cov1.d(36): Error: `{ members }` expected for anonymous class
fail_compilation/misc_parser_err_cov1.d(38): Error: template argument expected following `!`
fail_compilation/misc_parser_err_cov1.d(38): Error: missing closing `)` after `if (parseShift!()`
fail_compilation/misc_parser_err_cov1.d(38): Error: found `)` when expecting `(`
fail_compilation/misc_parser_err_cov1.d(39): Error: missing closing `)` after `if (`
fail_compilation/misc_parser_err_cov1.d(39): Error: identifier expected following `immutable(int).`, not `+`
fail_compilation/misc_parser_err_cov1.d(39): Error: expression expected, not `;`
fail_compilation/misc_parser_err_cov1.d(40): Error: semicolon expected following auto declaration, not `auto`
fail_compilation/misc_parser_err_cov1.d(40): Error: identifier or `new` expected following `.`, not `+`
fail_compilation/misc_parser_err_cov1.d(41): Error: identifier or new keyword expected following `(...)`.
fail_compilation/misc_parser_err_cov1.d(41): Error: expression expected, not `;`
fail_compilation/misc_parser_err_cov1.d(42): Error: found `}` when expecting `;` following expression
fail_compilation/misc_parser_err_cov1.d(41):        expression: `(__error) + 0`
fail_compilation/misc_parser_err_cov1.d(43): Error: matching `}` expected following compound statement, not `End of File`
fail_compilation/misc_parser_err_cov1.d(33):        unmatched `{`
---
*/
module misc_parser_err_cov1;


//https://issues.dlang.org/show_bug.cgi?id=19995
#line 29
void foo(in);
void bar(int, const @tation);

void main()
{
    // cover errors from line 7930 to EOF
    #line  31
    auto tt = __traits(<o<);
    auto b = is ;
    auto mx1 = mixin +);

    aa +=  [key:value, key];
    auto anon1 = new class;
    auto anon2 = new class {};
    if (parseShift !if){}
    auto unaryExParseError = immutable(int).+;
    auto postFixParseError = int.max.+;
    (int).+;
}
