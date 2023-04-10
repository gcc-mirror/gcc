/*
REQUIRED_ARGS:
TEST_OUTPUT:
---
fail_compilation/test20809.d(114): Error: returning `this.a` escapes a reference to parameter `this`
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
