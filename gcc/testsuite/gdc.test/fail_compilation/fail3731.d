/*
TEST_OUTPUT:
---
fail_compilation/fail3731.d(13): Error: cannot implicitly convert expression `x` of type `immutable(D)` to `fail3731.main.C`
---
*/

void main()
{
    class C {}
    class D : C {}
    auto x = new immutable(D);
    C y = x;
}
