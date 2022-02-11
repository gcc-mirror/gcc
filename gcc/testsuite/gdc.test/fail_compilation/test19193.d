/*
REQUIRED_ARGS: -de
TEST_OUTPUT
---
fail_compilation/test19193.d(13): Deprecation: enum member `test19193.T19193!int.A.b` is deprecated
---
*/

// https://issues.dlang.org/show_bug.cgi?id=19193

void main ()
{
    cast(void)T19193!int.A.b;
}

template T19193(T)
{
    enum A
    {
        deprecated b
    }
}
