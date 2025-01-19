/* REQUIRED_ARGS: -preview=dip1000
 * TEST_OUTPUT:
---
fail_compilation/test17764.d(109): Error: assigning scope variable `c` to global variable `global` is not allowed in a `@safe` function
---
 */

// https://issues.dlang.org/show_bug.cgi?id=17764

#line 100

int** global;

struct S { int** str; }

void f() @safe
{
    int* buf;
    S[1] c = S(&buf);
    global = c[0].str; /* This should be rejected. */
}
