/*
TEST_OUTPUT:
---
fail_compilation/test16365.d(20): Error: taking address of member `f1` without `this` reference is not allowed in a `@safe` function
fail_compilation/test16365.d(22): Error: cannot implicitly convert expression `&f2` of type `void delegate() pure nothrow @nogc @safe` to `void function() @safe`
fail_compilation/test16365.d(27): Error: using `dg.funcptr` is not allowed in a `@safe` function
---
*/

// https://issues.dlang.org/show_bug.cgi?id=16365

struct S
{
    void f1() @safe { }
}

void main() @safe
{
    void function() @safe f;
    f = &S.f1;
    void f2() @safe { }
    f = &f2;

    void delegate() @safe dg;
    S s;
    dg = &s.f1;
    f = dg.funcptr;
}
