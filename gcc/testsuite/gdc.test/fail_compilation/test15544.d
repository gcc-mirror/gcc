/*
REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test15544.d(20): Error: assigning reference to local `this` to non-scope `_del` is not allowed in a `@safe` function
fail_compilation/test15544.d(22): Error: assigning reference to local `this` to non-scope `_del` is not allowed in a `@safe` function
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
fail_compilation/test15544.d(46): Error: assigning reference to local `y` to non-scope `dg` is not allowed in a `@safe` function
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
