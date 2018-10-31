/*
TEST_OUTPUT:
---
fail_compilation/fail12636.d(13): Error: C++ class 'fail12636.C' cannot implement D interface 'fail12636.D'
---
*/

interface D
{
    void foo();
}

extern(C++) class C : D
{
    extern(D) override void foo() { }
}

void main()
{
    auto c = new C;
    c.foo(); // works
    D d = c;
    d.foo(); // segfault
}
