

// REQUIRED_ARGS: -de
/*
TEST_OUTPUT:
---
fail_compilation/ice11822.d(32): Deprecation: function ice11822.d is deprecated
fail_compilation/ice11822.d(21):        instantiated from here: S!(__lambda1)
fail_compilation/ice11822.d(32):        instantiated from here: g!((n) => d(i))
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
