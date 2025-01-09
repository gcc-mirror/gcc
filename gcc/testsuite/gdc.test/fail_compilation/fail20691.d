/* REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/fail20691.d(106): Error: taking address of `scope` variable `sa` with pointers is not allowed in a `@safe` function
fail_compilation/fail20691.d(107): Error: taking address of `scope` variable `sa` with pointers is not allowed in a `@safe` function
fail_compilation/fail20691.d(108): Error: taking address of `scope` variable `sa` with pointers is not allowed in a `@safe` function
---
*/

#line 100

// https://issues.dlang.org/show_bug.cgi?id=20691

void bar() @safe
{
    scope char[][2] sa;
    scope char[][] da = cast(char[][])sa;
    scope char[][] ca = sa;
    foo(sa);
}

void foo(scope char[][] a) @safe;
