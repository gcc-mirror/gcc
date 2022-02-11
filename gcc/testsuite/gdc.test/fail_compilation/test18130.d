/*
TEST_OUTPUT:
---
fail_compilation/test18130.d(8): Error: variable `test18130.foo.v` Zero-length `out` parameters are not allowed.
---
*/
// https://issues.dlang.org/show_bug.cgi?id=18130
void foo(out byte[0] v)
{
}
