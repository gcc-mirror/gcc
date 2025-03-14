/* TEST_OUTPUT:
---
fail_compilation/opapplyscope.d(113): Error: function `opapplyscope.S.opApply(scope int delegate(scope int* ptr) @safe dg)` is not callable using argument types `(int delegate(int* x) nothrow @nogc @safe)`
fail_compilation/opapplyscope.d(113):        cannot pass argument `int(int* x) => 0` of type `int delegate(int* x) nothrow @nogc @safe` to parameter `scope int delegate(scope int* ptr) @safe dg`
---
 */

#line 100

struct S
{
    int opApply(scope int delegate (scope int* ptr) @safe dg) @safe
    {
        return 0;
    }
}

void test() @safe
{
    static int* global;
    S s;
    foreach (/*scope*/ int* x; s)
    {
        global = x;
    }
}
