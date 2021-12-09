/*
TEST_OUTPUT:
---
fail_compilation/ice8795.d-mixin-14(14): Error: found `End of File` when expecting `(`
fail_compilation/ice8795.d-mixin-14(14): Error: expression expected, not `End of File`
fail_compilation/ice8795.d-mixin-14(14): Error: found `End of File` when expecting `)`
fail_compilation/ice8795.d-mixin-14(14): Error: found `End of File` instead of statement
fail_compilation/ice8795.d-mixin-15(15): Error: { } expected following `interface` declaration
fail_compilation/ice8795.d-mixin-15(15): Error: anonymous interfaces not allowed
---
*/
void main()
{
    mixin("switch");
    mixin("interface;");
}
