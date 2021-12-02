/* REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test18484.d(19): Error: returning `x.bar()` escapes a reference to local variable `x`
fail_compilation/test18484.d(24): Error: escaping reference to stack allocated value returned by `S(0)`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=18484

struct S
{
    int* bar() return;
    int i;
}

int* test1()
{
    auto x = S(); return x.bar();  // error
}

int* test2()
{
    return S().bar();  // error
}

