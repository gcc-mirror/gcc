/*
TEST_OUTPUT:
---
fail_compilation/ice14923.d(22): Error: function `ice14923.parse(C a)` is not callable using argument types `(A)`
fail_compilation/ice14923.d(22):        cannot pass argument `b` of type `ice14923.A` to parameter `C a`
fail_compilation/ice14923.d(22):        instantiated from here: `bar!((b) => parse(b))`
---
*/

auto bar(alias fun)()
{
    size_t counter;
    scope(exit) counter++;

    Object a2;
    if (auto ai = cast(A)a2) return fun(ai);
    if (auto ai = cast(B)a2) return fun(ai);
}

void parse(C a)
{
    bar!(b => parse(b))();
}

class A {}

class C {}

class B : C {}
