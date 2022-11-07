/* TEST_OUTPUT:
REQUIRED_ARGS: -preview=dip1000
---
fail_compilation/test22145.d(115): Error: scope variable `x` assigned to global variable `global`
---
 */

// issues.dlang.org/show_bug.cgi?id=22145

#line 100

struct S
{
    int opApply (scope int delegate (scope int* ptr) @safe dg) @safe
    {
        return 0;
    }
}

void test() @safe
{
    static int* global;
    S s;
    foreach (scope int* x; s)
    {
        global = x;
    }
}
