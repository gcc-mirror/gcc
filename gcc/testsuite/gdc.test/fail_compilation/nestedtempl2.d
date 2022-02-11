/*
TEST_OUTPUT:
---
fail_compilation/nestedtempl2.d(22): Deprecation: function `nestedtempl2.B.func!(n).func` function requires a dual-context, which is deprecated
fail_compilation/nestedtempl2.d(34):        instantiated from here: `func!(n)`
fail_compilation/nestedtempl2.d(34): Error: `this` is only defined in non-static member functions, not `test`
fail_compilation/nestedtempl2.d(34): Error: need `this` of type `B` to call function `func`
fail_compilation/nestedtempl2.d(35): Error: `this` is only defined in non-static member functions, not `test`
fail_compilation/nestedtempl2.d(35): Error: need `this` of type `B` to make delegate from function `func`
fail_compilation/nestedtempl2.d(37): Error: `this` is only defined in non-static member functions, not `test`
fail_compilation/nestedtempl2.d(37): Error: need `this` of type `B` needed to `new` nested class `N`
---
*/

class B
{
    int n;
}

void test()
{
    auto func(alias a)()
    {
        return a;
    }

    class N(alias a)
    {
    }

    auto b = new B();
    b.n = 1;

    func!(b.n)();
    auto dg = &func!(b.n);

    new N!(b.n)();
}
