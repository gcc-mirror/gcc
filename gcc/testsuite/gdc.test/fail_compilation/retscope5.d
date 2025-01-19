/*
REQUIRED_ARGS: -preview=dip1000
*/

/*
TEST_OUTPUT:
---
fail_compilation/retscope5.d(5010): Error: assigning address of variable `t` to `p` with longer lifetime is not allowed in a `@safe` function
---
*/

#line 5000

// https://issues.dlang.org/show_bug.cgi?id=17725

void test() @safe
{
    int* p;
    struct T {
            int a;
    }
    void escape(ref T t) @safe {
            p = &t.a; // should not compile
    }
}
