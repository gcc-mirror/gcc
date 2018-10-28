/*
REQUIRED_ARGS: -m64
PERMUTE_ARGS:
TEST_OUTPUT:
---
fail_compilation/test16381.d(16): Error: foo() is not an lvalue
---
*/

// https://issues.dlang.org/show_bug.cgi?id=16381

__vector(float[4]) foo();

void bar()
{
    float g = foo().ptr[0];
}

