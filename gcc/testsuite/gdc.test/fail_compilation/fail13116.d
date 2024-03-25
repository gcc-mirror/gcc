/*
TEST_OUTPUT:
---
fail_compilation/fail13116.d(14): Error: cannot `ref` return expression `this` because it is not an lvalue
fail_compilation/fail13116.d(23): Error: cannot `ref` return expression `super` because it is not an lvalue
---
*/
struct S
{
    ref S notEvil() return { return this; } // this should be accepted
}
class C
{
    ref C evil() { return this; } // this should be rejected
}
void main()
{
}

class Base { }
class Derived : Base
{
    ref Base evil() { return super; } // should be rejected
}
