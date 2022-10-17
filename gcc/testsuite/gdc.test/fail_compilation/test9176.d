/*
TEST_OUTPUT:
---
fail_compilation/test9176.d(14): Error: forward reference to inferred return type of function call `get()`
fail_compilation/test9176.d(10):        while evaluating: `static assert(!is(typeof(foo(S()))))`
---
*/

void foo(int x) {}
static assert(!is(typeof(foo(S()))));

struct S
{
    auto get() { return get(); }
    alias get this;
}

void main(){}
