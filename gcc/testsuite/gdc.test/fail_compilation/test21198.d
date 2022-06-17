// https://issues.dlang.org/show_bug.cgi?id=21198

/*
TEST_OUTPUT:
---
fail_compilation/test21198.d(23): Error: generating an `inout` copy constructor for `struct test21198.U` failed, therefore instances of it are uncopyable
---
*/

struct S
{
    this(ref inout(S) other) inout {}
}

union U
{
    S s;
}

void fun()
{
    U original;
    U copy = original;
}
