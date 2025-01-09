/* REQUIRED_ARGS: -preview=dip1000
   TEST_OUTPUT:
---
fail_compilation/test20569.d(19): Error: taking address of `scope` variable `s1` with pointers is not allowed in a `@safe` function
fail_compilation/test20569.d(23): Error: taking address of `scope` variable `s2` with pointers is not allowed in a `@safe` function
---
 */

// https://issues.dlang.org/show_bug.cgi?id=20569

struct S
{
    int* pointer;
}

void main() @safe
{
    scope S s1;
    scope int** p1 = &s1.pointer;

    int x;
    S s2 = S(&x);
    auto p2 = &s2.pointer;
}
