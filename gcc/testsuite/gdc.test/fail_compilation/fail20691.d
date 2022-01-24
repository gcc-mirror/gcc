/* REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/fail20691.d(106): Error: cannot take address of `scope` local `sa` in `@safe` function `bar`
fail_compilation/fail20691.d(106): Error: cannot cast expression `sa` of type `char[][2]` to `char[][]`
fail_compilation/fail20691.d(107): Error: cannot take address of `scope` local `sa` in `@safe` function `bar`
fail_compilation/fail20691.d(107): Error: cannot cast expression `sa` of type `char[][2]` to `char[][]`
fail_compilation/fail20691.d(108): Error: cannot take address of `scope` local `sa` in `@safe` function `bar`
fail_compilation/fail20691.d(108): Error: cannot cast expression `sa` of type `char[][2]` to `char[][]`
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
