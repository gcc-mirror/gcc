/* REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/fail20183.d(1016): Error: function `addr` is not callable using argument types `(int)`
fail_compilation/fail20183.d(1016):        cannot pass rvalue argument `S(0).i` of type `int` to parameter `return ref int b`
fail_compilation/fail20183.d(1005):        `fail20183.addr(return ref int b)` declared here
fail_compilation/fail20183.d(1017): Error: address of struct temporary returned by `s()` assigned to longer lived variable `q`
---
 */

#line 1000

// https://issues.dlang.org/show_bug.cgi?id=20183

@safe:

int* addr(return ref int b) { return &b; }

struct S
{
    int i;
}

S s() { return S(); }

void test()
{
    int* p = addr(S().i);  // struct literal
    int* q = addr(s().i);  // struct temporary
}

/*
TEST_OUTPUT:
---
fail_compilation/fail20183.d(1107): Error: address of struct temporary returned by `s()` assigned to longer lived variable `this.ptr`
---
 */
#line 1100

class Foo
{
    int* ptr;

    this() @safe
    {
        ptr = addr(s().i);  // struct literal
    }
}
