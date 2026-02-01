/*
TEST_OUTPUT:
---
fail_compilation/ice8795.d-mixin-16(16): Error: found `End of File` when expecting `(`
fail_compilation/ice8795.d-mixin-16(16): Error: expression expected, not `End of File`
fail_compilation/ice8795.d-mixin-16(16): Error: missing closing `)` after `switch (__error`
fail_compilation/ice8795.d-mixin-16(16): Error: found `End of File` instead of statement
fail_compilation/ice8795.d-mixin-16(16):        while parsing string mixin statement
fail_compilation/ice8795.d-mixin-17(17): Error: { } expected following `interface` declaration
fail_compilation/ice8795.d-mixin-17(17): Error: anonymous interfaces not allowed
fail_compilation/ice8795.d-mixin-17(17):        while parsing string mixin statement
---
*/
void main()
{
    mixin("switch");
    mixin("interface;");
}
