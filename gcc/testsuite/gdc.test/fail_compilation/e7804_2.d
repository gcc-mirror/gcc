/*
TEST_OUTPUT:
---
fail_compilation/e7804_2.d(17): Error: `__traits(getMember, Foo, "func")` does not give a valid type
---
*/

module e7804_2;

class Foo
{
     void func(){}
}

void test()
{
    __traits(getMember, Foo, "func") var;
    auto a = cast(__traits(getMember, Foo, "func")) 0;
}
