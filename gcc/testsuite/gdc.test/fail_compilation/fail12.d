/*
TEST_OUTPUT:
---
fail_compilation/fail12.d(17): Error: function `fail12.main.Foo!(y).abc` at fail_compilation/fail12.d(9) conflicts with function `fail12.main.Foo!(y).abc` at fail_compilation/fail12.d(9)
---
*/
template Foo(alias b)
{
    int abc() { return b; }
}

void main()
{
    int y = 8;
    mixin Foo!(y);
    mixin Foo!(y);
    assert(abc() == 8);
}

