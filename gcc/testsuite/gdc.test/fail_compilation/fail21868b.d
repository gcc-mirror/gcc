/* REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/fail21868b.d(19): Error: returning `&s.x` escapes a reference to parameter `s`
fail_compilation/fail21868b.d(19):        perhaps remove `scope` parameter annotation so `return` applies to `ref`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=21868

struct S
{
    int x;
    int* y;
}

int* test(ref return scope S s)
{
    return &s.x;
}


