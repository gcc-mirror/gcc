/*
TEST_OUTPUT:
---
fail_compilation/fail18985.d(16): Error: `foo` is not a scalar, it is a `object.Object`
fail_compilation/fail18985.d(17): Error: `bar` is not a scalar, it is a `shared(Object)`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=18985

Object foo;
shared Object bar;

void main()
{
    foo += 1;
    bar += 1;
}
