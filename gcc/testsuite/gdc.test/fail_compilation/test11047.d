/*
TEST_OUTPUT:
---
fail_compilation/test11047.d(11): Error: value of x is not known at compile time
fail_compilation/test11047.d(11): Error: value of x is not known at compile time
---
*/
// https://issues.dlang.org/show_bug.cgi?id=11047

int x;
@(++x, ++x) void foo(){}

@safe pure void test()
{
    __traits(getAttributes, foo);
    __traits(getAttributes, foo)[0];
}


