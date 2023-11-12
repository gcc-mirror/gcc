/*
REQUIRED_ARGS: -m64
TEST_OUTPUT:
---
fail_compilation/test16381.d(15): Error: cannot take address of expression `foo()` because it is not an lvalue
---
*/

// https://issues.dlang.org/show_bug.cgi?id=16381

__vector(float[4]) foo();

void bar()
{
    float g = foo().ptr[0];
}
