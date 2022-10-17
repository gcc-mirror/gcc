/*
REQUIRED_ARGS: -de
*/

/*
TEST_OUTPUT:
---
fail_compilation/retscope4.d(3007): Deprecation: slice of static array temporary returned by `func()` assigned to longer lived variable `a`
---
*/

#line 3000

// https://issues.dlang.org/show_bug.cgi?id=12625

int[16] func();

void foo()
{
    int[] a = func();
}
