/*
DISABLED: freebsd32 linux32 osx32 win32
TEST_OUTPUT:
---
fail_compilation/ice20264.d(12): Error: `cast(__vector(float[4]))a` is not an lvalue and cannot be modified
---
*/

void foo(float *a)
{
    alias float4 = __vector(float[4]);
    cast(float4)(a) = 1.0f;
}
