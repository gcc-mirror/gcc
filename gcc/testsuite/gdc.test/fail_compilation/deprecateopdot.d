/*
REQUIRED_ARGS: -de
TEST_OUTPUT:
---
fail_compilation/deprecateopdot.d(27): Deprecation: `opDot` is deprecated. Use `alias this`
fail_compilation/deprecateopdot.d(28): Deprecation: `opDot` is deprecated. Use `alias this`
fail_compilation/deprecateopdot.d(29): Deprecation: `opDot` is deprecated. Use `alias this`
---
*/
struct S6
{
    int a, b;
}
struct T6
{
    S6 s;

    S6* opDot() return
    {
        return &s;
    }
}

void test6()
{
    T6 t;
    t.a = 4;
    assert(t.a == 4);
    t.b = 5;
}
