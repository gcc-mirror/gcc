/*
REQUIRED_ARGS: -de
TEST_OUTPUT:
---
fail_compilation/deprecateopdot.d(27): Error: `opDot` is obsolete. Use `alias this`
fail_compilation/deprecateopdot.d(28): Error: `opDot` is obsolete. Use `alias this`
fail_compilation/deprecateopdot.d(29): Error: `opDot` is obsolete. Use `alias this`
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
