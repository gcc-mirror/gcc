/*
TEST_OUTPUT:
---
fail_compilation/fail246.d-mixin-11(11): Error: identifier expected, not `EOF`
fail_compilation/fail246.d-mixin-11(11): Error: `;` expected after mixin
---
*/

void a()
{
    mixin(`mixin`);
}
