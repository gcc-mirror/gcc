/*
TEST_OUTPUT:
---
fail_compilation/fail263.d(19): Error: function `fail263.f(byte* p)` is not callable using argument types `(const(byte)*)`
fail_compilation/fail263.d(19):        cannot pass argument `cast(const(byte)*)A` of type `const(byte)*` to parameter `byte* p`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=2766
// DMD hangs with 0%cpu
const byte[] A = [ cast(byte)0 ];

void f(byte* p)
{
}

void func()
{
    f(A.ptr);
}
