/*
REQUIRED_ARGS:
TEST_OUTPUT:
---
fail_compilation/test20809.d(114): Error: escaping a reference to parameter `this` by returning `this.a` is not allowed in a `@safe` function
fail_compilation/test20809.d(112):        perhaps annotate the function with `return`
---
 */

// https://issues.dlang.org/show_bug.cgi?id=20809

#line 100

@safe:

struct S
{
    @safe:
    int a;
    ~this()
    {
        a = 0;
    }

    ref int val()
    {
        return a;
    }
}

S bar()
{
    return S(2);
}

int foo()
{
    return bar.val;
}

void test()
{
    assert(foo() == 2);
}
