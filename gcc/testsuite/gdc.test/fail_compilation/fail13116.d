/*
TEST_OUTPUT:
---
fail_compilation/fail13116.d(13): Error: this is not an lvalue
---
*/
struct S
{
    ref S notEvil() { return this; } // this should be accepted
}
class C
{
    ref C evil() { return this; } // this should be rejected
}
void main()
{
}

/*
TEST_OUTPUT:
---
fail_compilation/fail13116.d(28): Error: super is not an lvalue
---
*/
class Base { }
class Derived : Base
{
    ref Base evil() { return super; } // should be rejected
}
