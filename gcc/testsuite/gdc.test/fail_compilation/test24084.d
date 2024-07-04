/* REQUIRED_ARGS: -nothrow
 * TEST_OUTPUT:
---
fail_compilation/test24084.d(110): Error: cannot use `throw` statements with -nothrow
fail_compilation/test24084.d(112): Error: cannot use try-catch statements with -nothrow
---
 */

// https://issues.dlang.org/show_bug.cgi?id=24084

#line 100

struct S
{
    int x;
    ~this() { }
}

void xyzzy()
{
    S s;
    throw new Exception("xx");

    try
    {
        int y;
    } catch (Exception) { }
}
