/*
TEST_OUTPUT:
---
fail_compilation/ice9273b.d(14): Error: constructor `ice9273b.B.this` no match for implicit `super()` call in constructor
---
*/

class A
{
    this(T)() {}
}
class B : A
{
    this() {}
}
