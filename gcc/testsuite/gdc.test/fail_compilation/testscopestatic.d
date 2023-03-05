
/* TEST_OUTPUT:
---
fail_compilation/testscopestatic.d(15): Error: variable `testscopestatic.foo.p` cannot be `scope` and `static`
fail_compilation/testscopestatic.d(16): Error: variable `testscopestatic.foo.b` cannot be `scope` and `extern`
fail_compilation/testscopestatic.d(17): Error: variable `testscopestatic.foo.c` cannot be `scope` and `__gshared`
fail_compilation/testscopestatic.d(21): Error: field `x` cannot be `scope`
---
*/


void foo()
{
    scope int a;
    static scope int* p;
    extern scope int b;
    scope __gshared int c;

    struct S
    {
        scope int x;
    }
}
