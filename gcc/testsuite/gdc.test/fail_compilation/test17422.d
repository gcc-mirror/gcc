/*
REQUIRED_ARGS: -dip1000
TEST_OUTPUT:
---
fail_compilation/test17422.d(23): Error: scope variable `p` may not be returned
---
*/
struct RC
{
    Object get() return scope @trusted
    {
        return cast(Object) &store[0];
    }

private:
    ubyte[__traits(classInstanceSize, Object)] store;
}

Object test() @safe
{
    RC rc;
    auto p = rc.get; // p must be inferred as scope variable, works for int*
    return p;
}
