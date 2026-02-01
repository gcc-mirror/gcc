/*
TEST_OUTPUT:
---
fail_compilation/fail246.d-mixin-12(12): Error: identifier expected, not `End of File`
fail_compilation/fail246.d-mixin-12(12): Error: `;` expected after `mixin`
fail_compilation/fail246.d-mixin-12(12):        while parsing string mixin statement
---
*/

void a()
{
    mixin(`mixin`);
}
