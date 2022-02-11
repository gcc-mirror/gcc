/* REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/fail20461.d(106): Error: reference to local variable `buffer` assigned to non-scope parameter calling `assert()`
---
*/

#line 100

// https://issues.dlang.org/show_bug.cgi?id=20461

void test() @safe
{
    char[10] buffer = "0123456789";
    assert(false, buffer[]);
}
