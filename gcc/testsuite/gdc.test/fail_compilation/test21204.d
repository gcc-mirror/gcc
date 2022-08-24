// https://issues.dlang.org/show_bug.cgi?id=21204
/*
TEST_OUTPUT:
---
fail_compilation/test21204.d(22): Error: generating an `inout` copy constructor for `struct test21204.B` failed, therefore instances of it are uncopyable
---
*/

struct A
{
    this(ref A other) {}
}

struct B
{
    A a;
}

void example()
{
    B b1;
    B b2 = b1;
}
