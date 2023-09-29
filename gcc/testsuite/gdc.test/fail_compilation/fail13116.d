/*
TEST_OUTPUT:
---
fail_compilation/fail13116.d(14): Error: returning `this` escapes a reference to parameter `this`
fail_compilation/fail13116.d(23): Error: `super` is not an lvalue and cannot be modified
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
