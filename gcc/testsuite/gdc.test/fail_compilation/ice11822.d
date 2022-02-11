

// REQUIRED_ARGS: -de
/*
TEST_OUTPUT:
---
fail_compilation/ice11822.d(33): Deprecation: function `ice11822.d` is deprecated
fail_compilation/ice11822.d(16):        instantiated from here: `__lambda2!int`
fail_compilation/ice11822.d(22):        instantiated from here: `S!(__lambda2)`
fail_compilation/ice11822.d(33):        instantiated from here: `g!((n) => d(i))`
---
*/

struct S(alias pred)
{
    this(int) { pred(1); }
    void f()  { pred(2); }
}

auto g(alias pred)()
{
    return S!pred(3);
}

deprecated bool d(int)
{
    return true;
}

auto h()
{
    int i;
    return g!(n => d(i))();
}
