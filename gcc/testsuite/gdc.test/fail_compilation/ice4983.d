/*
TEST_OUTPUT:
---
fail_compilation/ice4983.d(14): Error: circular reference to 'ice4983.Foo.dg'
---
*/

struct Foo
{
    void bar()
    {
    }

    void delegate() dg = &Foo.init.bar;
}
