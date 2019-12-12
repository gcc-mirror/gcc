/*
TEST_OUTPUT:
---
fail_compilation/fail263.d(18): Error: function fail263.f (byte* p) is not callable using argument types (const(byte)*)
---
*/

// Issue 2766 - DMD hangs with 0%cpu

const byte[] A = [ cast(byte)0 ];

void f(byte* p)
{
}

void func()
{
    f(A.ptr);
}
