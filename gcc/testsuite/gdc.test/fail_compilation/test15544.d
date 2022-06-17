/*
REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test15544.d(20): Error: reference to local `this` assigned to non-scope `_del` in @safe code
fail_compilation/test15544.d(22): Error: reference to local `this` assigned to non-scope `_del` in @safe code
---
*/

// https://issues.dlang.org/show_bug.cgi?id=15544

void delegate() @safe _del;

struct S {
    int x = 42;

    @safe void test()
    {
        void foo() { assert(x == 42); }
        _del = &foo;

        _del = { assert(x == 42); };
    }
}

/*
TEST_OUTPUT:
---
fail_compilation/test15544.d(46): Error: reference to local `y` assigned to non-scope `dg` in @safe code
---
*/

int delegate() dg;

void testClosure1()
{
    int* x;
    int bar() { return *x; }
    dg = &bar;
}

@safe void testClosure2()
{
    scope int* y;
    int bar() { return *y; }
    dg = &bar;               // Error
    auto dg2 = &bar;
}
