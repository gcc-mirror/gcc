// REQUIRED_ARGS: -w
/*
TEST_OUTPUT:
---
fail_compilation/fail8724.d(14): Error: `object.Exception` is thrown but not caught
fail_compilation/fail8724.d(12): Error: constructor `fail8724.Foo.this` may throw but is marked as `nothrow`
---
*/

struct Foo
{
    this(int) nothrow
    {
        throw new Exception("something");
    }
}
